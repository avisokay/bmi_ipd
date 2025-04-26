library(knitr)
library(dplyr)
library(ggplot2)
library(ggstats)
library(nnet)
library(data.table)
library(devtools)
library(gt)
library(broom)
library(haven)
library(survey)
library(caret)
library(MASS)
library(progress)
library(ipd)

# set number of digits to 2 for tidy outputs later on
options(digits=2)

# ----- IN-SAMPLE ESTIMATES -----
# this contains the code used to perform regression analyses with IPD
# correction for the paper "BMI can predict adiposity, but not well 
# enough to learn about obesity trends.
# See acquire_data.R and clean_data.R for reproducing the data collection, 
# feature engineering, and survey weighting. 

# load data
df = readRDS("../data/nhanes_cleaned.rds")
df_2011 = df[!is.na(df$year) & df$year %in% c(2011:2018), ]
df_2021 = df[!is.na(df$year) & df$year > 2018, ]

# function that takes a dataframe, y_vars, X_vars and estimates a univariate 
# regression for each permutation of y_var and X_var on a subset of the 
# dataframe that has been divided into "labeled" and "unlabeled". 

# ----- NAIVE -----
insample_naive_regressions = function(df, y_vars, X_vars, unlab_proportion = 0.3, seed = 123) {
  # Load required packages
  require(MASS)
  require(progress)
  
  # Create an empty list to store results
  all_results = list()
  
  # Calculate total number of iterations for progress bar
  total_iterations = length(y_vars) * length(X_vars)
  
  # Create a progress bar
  pb = progress::progress_bar$new(
    format = "Running regressions [:bar] :percent :current/:total (:eta remaining)",
    total = total_iterations,
    clear = FALSE,
    width = 80
  )
  
  # Counter for progress bar
  counter = 0
  
  # Loop through each y variable
  for (y_var in y_vars) {
    # Loop through each X variable
    for (X_var in X_vars) {
      # Update progress bar
      counter = counter + 1
      pb$update(counter / total_iterations)
      
      # Create a subset of data where both y_var and X_var are not NA
      subset_df = df[!is.na(df[["obese_dxa"]]) & !is.na(df[[y_var]]) & !is.na(df[[X_var]]), ]
      
      # Set seed for reproducibility
      set.seed(seed)
      
      # Create labeled/unlabeled split on the subset
      n = nrow(subset_df)
      subset_df$set = "labeled"
      unlab_index = sample(seq_len(n), unlab_proportion * n)
      subset_df[unlab_index, "set"] = "unlabeled"
      
      # Construct formula
      formula_str = paste(y_var, "~", X_var)
      formula_obj = as.formula(formula_str)
      
      # create survey design object
      nhanes_design <- svydesign(
        id = ~SDMVPSU,           # Primary Sampling Unit
        strata = ~SDMVSTRA,      # Stratification variable
        weights = ~WTMEC10YR,    # Your calculated 10-year weights
        nest = TRUE,
        data = subset_df[subset_df$set == "labeled", ]
      )

      # Fit logistic regression model on labeled data only
      tryCatch({
        model =
          svyglm(
            formula = formula_obj,
            design = nhanes_design,
            family = quasibinomial()  # Use quasibinomial for survey-weighted logistic regression
          )
      
      # # Fit logistic regression model on labeled data only
      # tryCatch({
      #   model = glm(formula = formula_obj, 
      #               family = "binomial",
      #               weights = WTMEC2YR,
      #               data = subset_df[subset_df$set == "labeled", ])
        
        # Get coefficients with confidence intervals and exponentiate
        model_coef = exp(cbind(coef(model), confint(model)))
        
        # Convert to a data frame
        results = data.frame(
          y_variable = y_var,
          x_variable = X_var,
          Term = rownames(model_coef),
          Estimate = model_coef[, 1],
          Lower = model_coef[, 2],
          Upper = model_coef[, 3],
          stringsAsFactors = FALSE
        )
        
        # Add to list of results
        all_results[[length(all_results) + 1]] = results
        
      }, error = function(e) {
        # Handle any errors
        warning(paste("Error in model with", y_var, "and", X_var, ":", e$message))
      })
    }
  }
  
  # Combine all results into a single data frame
  if (length(all_results) > 0) {
    combined_results = do.call(rbind, all_results)
    rownames(combined_results) = NULL
    return(combined_results)
  } else {
    warning("No models were successfully fit.")
    return(NULL)
  }
}

# ----- IPD -----
insample_ipd_regressions = function(df, y_vars, X_vars, unlab_proportion = 0.3, seed = 123) {
  # Load required packages
  require(MASS)
  require(ipd)
  require(data.table)
  require(progress)
  
  # Create an empty list to store results
  all_results = list()
  
  # Calculate total number of iterations for progress bar
  total_iterations = length(y_vars) * length(X_vars)
  
  # Create a progress bar
  pb = progress::progress_bar$new(
    format = "Running IPD models [:bar] :percent :current/:total (:eta remaining)",
    total = total_iterations,
    clear = FALSE,
    width = 80
  )
  
  # Counter for progress bar
  counter = 0
  
  # Loop through each y variable
  for (y_var in y_vars) {
    # Loop through each X variable
    for (X_var in X_vars) {
      # Update progress bar
      counter = counter + 1
      pb$update(counter / total_iterations)
      
      # Create a subset of data where both y_var and X_var are not NA
      subset_df = df[!is.na(df[["obese_dxa"]]) & !is.na(df[[y_var]]) & !is.na(df[[X_var]]), ]
      
      # Set seed for reproducibility
      set.seed(seed)
      
      # Create labeled/unlabeled split on the subset
      n = nrow(subset_df)
      subset_df$set = "labeled"
      unlab_index = sample(seq_len(n), unlab_proportion * n)
      subset_df[unlab_index, "set"] = "unlabeled"
      
      # Construct IPD formula
      formula_str = paste0("obese_dxa - ", y_var, " ~ ", X_var)
      formula_ipd = as.formula(formula_str)
      
      weights_u = subset_df[subset_df$set == "unlabeled",  "WTMEC10YR"]
      weights_l = subset_df[subset_df$set == "labeled",  "WTMEC10YR"]
      
      # Fit IPD model
      tryCatch({
        ppi_p_model = ipd::ipd(
          formula = formula_ipd,
          method = "ppi_plusplus",
          model = "logistic",
          w_u = weights_u,
          w_l = weights_l,
          data = subset_df,
          label = "set"
        )
        
        # Get coefficients with confidence intervals and exponentiate
        ppi_p_coef = data.table(exp(cbind(ppi_p_model$coefficients, ppi_p_model$ci)))
        rownames(ppi_p_coef) = names(ppi_p_model$coefficients)
        
        # Convert to a data frame
        results = data.frame(
          y_variable = paste0("ipd_", y_var),
          x_variable = X_var,
          Term = rownames(ppi_p_coef),
          Estimate = ppi_p_coef[, 1],
          Lower = ppi_p_coef[, 2],
          Upper = ppi_p_coef[, 3],
          stringsAsFactors = FALSE
        )
        
        colnames(results) = c("y_variable", "x_variable", 
                              "Term", "Estimate", "Lower", "Upper")
        
        # Add to list of results
        all_results[[length(all_results) + 1]] = results
        
      }, error = function(e) {
        # Handle any errors
        warning(paste("Error in IPD model with", y_var, "and", X_var, ":", e$message))
      })
    }
  }
  
  # Combine all results into a single data frame
  if (length(all_results) > 0) {
    combined_results = do.call(rbind, all_results)
    rownames(combined_results) = NULL
    return(combined_results)
  } else {
    warning("No IPD models were successfully fit.")
    return(NULL)
  }
}

# estimate naive results
insample_naive_results = insample_naive_regressions(
  df = df_2011,
  y_vars = c("obese_bmi", "obese_wc", "obese_dxa"),
  X_vars = c("race", "sex", "age", "smoke", "edu", "income")
)

# estimate ipd results
insample_ipd_results = insample_ipd_regressions(
  df = df_2011,
  y_vars = c("obese_bmi", "obese_wc", "obese_dxa"),
  X_vars = c("race", "sex", "age", "smoke", "edu", "income")
)

# combine results
insample_results = rbind(insample_naive_results, insample_ipd_results)

# save outputs to csv
write.csv(insample_results, "../results/insample_estimates.csv",
          row.names = FALSE)

# ----- OUT-SAMPLE ESTIMATES -----

# ----- NAIVE -----
outsample_naive_regressions = function(df, y_vars, X_vars) {
  # Load required packages
  require(MASS)
  require(progress)
  
  # Create an empty list to store results
  all_results = list()
  
  # Calculate total number of iterations for progress bar
  total_iterations = length(y_vars) * length(X_vars)
  
  # Create a progress bar
  pb = progress::progress_bar$new(
    format = "Running regressions [:bar] :percent :current/:total (:eta remaining)",
    total = total_iterations,
    clear = FALSE,
    width = 80
  )
  
  # Counter for progress bar
  counter = 0
  
  # Loop through each y variable
  for (y_var in y_vars) {
    # Loop through each X variable
    for (X_var in X_vars) {
      # Update progress bar
      counter = counter + 1
      pb$update(counter / total_iterations)
      
      # Create a subset of data where both y_var and X_var are not NA
      subset_df = df[!is.na(df[[y_var]]) & !is.na(df[[X_var]]), ]
      
      # Construct formula
      formula_str = paste(y_var, "~", X_var)
      formula_obj = as.formula(formula_str)
      
      # create survey design object
      nhanes_design <- svydesign(
        id = ~SDMVPSU,           # Primary Sampling Unit
        strata = ~SDMVSTRA,      # Stratification variable
        weights = ~WTMEC10YR,    # Your calculated 10-year weights
        nest = TRUE,
        data = subset_df
      )

      # Fit logistic regression model on labeled data only
      tryCatch({
        model =
          svyglm(
            formula = formula_obj,
            design = nhanes_design,
            family = quasibinomial()  # Use quasibinomial for survey-weighted logistic regression
          )
        
        # Get coefficients with confidence intervals and exponentiate
        model_coef = exp(cbind(coef(model), confint(model)))
        
        # Convert to a data frame
        results = data.frame(
          y_variable = y_var,
          x_variable = X_var,
          Term = rownames(model_coef),
          Estimate = model_coef[, 1],
          Lower = model_coef[, 2],
          Upper = model_coef[, 3],
          stringsAsFactors = FALSE
        )
        
        # Add to list of results
        all_results[[length(all_results) + 1]] = results
        
      }, error = function(e) {
        # Handle any errors
        warning(paste("Error in model with", y_var, "and", X_var, ":", e$message))
      })
    }
  }
  
  # Combine all results into a single data frame
  if (length(all_results) > 0) {
    combined_results = do.call(rbind, all_results)
    rownames(combined_results) = NULL
    return(combined_results)
  } else {
    warning("No models were successfully fit.")
    return(NULL)
  }
}

# estimate naive out-sample results
outsample_naive_results = outsample_naive_regressions(
  df = df_2021,
  y_vars = c("obese_bmi", "obese_wc"),
  X_vars = c("race", "sex", "age", "edu")
)

# ----- IPD -----

outsample_ipd_regressions = function(df_lab, df_unlab, y_vars, X_vars, seed = 123) {
  # Load required packages
  require(MASS)
  require(ipd)
  require(data.table)
  require(progress)
  
  # Create an empty list to store results
  all_results = list()
  
  # Calculate total number of iterations for progress bar
  total_iterations = length(y_vars) * length(X_vars)
  
  # Create a progress bar
  pb = progress::progress_bar$new(
    format = "Running IPD models [:bar] :percent :current/:total (:eta remaining)",
    total = total_iterations,
    clear = FALSE,
    width = 80
  )
  
  # Counter for progress bar
  counter = 0
  
  # Loop through each y variable
  for (y_var in y_vars) {
    # Loop through each X variable
    for (X_var in X_vars) {
      # Update progress bar
      counter = counter + 1
      pb$update(counter / total_iterations)
      
      # Select only necessary columns and filter rows where variables are not NA
      needed_cols = c(y_var, "obese_dxa", X_var, "WTMEC10YR", "set")
      
      # Subset labeled data to include only rows and columns needed
      subset_lab = df_lab[!is.na(df_lab[["obese_dxa"]]) & !is.na(df_lab[[y_var]]) & !is.na(df_lab[[X_var]]), 
                          c(y_var, "obese_dxa", X_var, "WTMEC10YR")]
      
      # Subset unlabeled data to include only rows and columns needed
      subset_unlab = df_unlab[!is.na(df_unlab[[y_var]]) & !is.na(df_unlab[[X_var]]), 
                              c(y_var, X_var, "WTMEC10YR")]
      
      # Add obese_dxa column to unlabeled data (will be NA but needed for formula)
      if (!"obese_dxa" %in% colnames(subset_unlab)) {
        subset_unlab$obese_dxa = NA
      }
      
      # Set the 'set' variable for each dataset
      subset_lab$set = "labeled"
      subset_unlab$set = "unlabeled"
      
      # Stack the datasets
      subset_df = rbind(subset_lab, subset_unlab)
      
      # Set seed for reproducibility
      set.seed(seed)
      
      # Construct IPD formula
      formula_str = paste0("obese_dxa - ", y_var, " ~ ", X_var)
      formula_ipd = as.formula(formula_str)
      
      # Extract weights
      weights_u = subset_df[subset_df$set == "unlabeled", "WTMEC10YR"]
      weights_l = subset_df[subset_df$set == "labeled", "WTMEC10YR"]
      
      # Fit IPD model
      tryCatch({
        ppi_p_model = ipd::ipd(
          formula = formula_ipd,
          method = "ppi_plusplus",
          model = "logistic",
          w_u = weights_u,
          w_l = weights_l,
          data = subset_df,
          label = "set"
        )
        
        # Get coefficients with confidence intervals and exponentiate
        ppi_p_coef = data.table(exp(cbind(ppi_p_model$coefficients, ppi_p_model$ci)))
        rownames(ppi_p_coef) = names(ppi_p_model$coefficients)
        
        # Convert to a data frame
        results = data.frame(
          y_variable = paste0("ipd_", y_var),
          x_variable = X_var,
          Term = rownames(ppi_p_coef),
          Estimate = ppi_p_coef[, 1],
          Lower = ppi_p_coef[, 2],
          Upper = ppi_p_coef[, 3],
          stringsAsFactors = FALSE
        )
        
        colnames(results) = c("y_variable", "x_variable", 
                              "Term", "Estimate", "Lower", "Upper")
        
        # Add to list of results
        all_results[[length(all_results) + 1]] = results
        
      }, error = function(e) {
        # Handle any errors
        warning(paste("Error in IPD model with", y_var, "and", X_var, ":", e$message))
      })
    }
  }
  
  # Combine all results into a single data frame
  if (length(all_results) > 0) {
    combined_results = do.call(rbind, all_results)
    rownames(combined_results) = NULL
    return(combined_results)
  } else {
    warning("No IPD models were successfully fit.")
    return(NULL)
  }
}

# estimate ipd results
outsample_ipd_results = outsample_ipd_regressions(
  df_lab = df_2011,
  df_unlab = df_2021,
  y_vars = c("obese_bmi", "obese_wc"),
  X_vars = c("race", "sex", "age", "edu")
)

# combine results
outsample_results = rbind(outsample_naive_results, outsample_ipd_results)

# save outputs to csv
write.csv(outsample_results, "../results/outsample_estimates.csv",
          row.names = FALSE)





