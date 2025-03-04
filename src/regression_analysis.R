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
library(caret)
library(MASS)
library(progress)
if (!require("ipd")) devtools::install_github("ipd-tools/ipd")
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
      
      # Fit logistic regression model on labeled data only
      tryCatch({
        model = glm(formula = formula_obj, 
                     family = "binomial",
                     data = subset_df[subset_df$set == "labeled", ])
        
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
      
      # Fit IPD model
      tryCatch({
        ppi_p_model = ipd::ipd(
          formula = formula_ipd,
          method = "ppi_plusplus",
          model = "logistic",
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
      
      # Fit logistic regression model on labeled data only
      tryCatch({
        model = glm(formula = formula_obj, 
                     family = "binomial",
                     data = subset_df)
        
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
  X_vars = c("race", "sex", "age", "smoke", "edu", "income")
)

# ----- IPD -----

y_var = "obese_bmi"
X_var = "race"

# Create a subset of data where both y_var and X_var are not NA
subset_df = df[!is.na(df[[y_var]]) & !is.na(df[[X_var]]), ]

# labeled 2011-2017 and unlabeled 2021
subset_df$set = ifelse(subset_df$year < 2019, "labeled", "unlabeled")

# Construct IPD formula
formula_str = paste0("obese_dxa - ", y_var, " ~ ", X_var)
formula_ipd = as.formula(formula_str)

ppi_p_model = ipd::ipd(
  formula = formula_ipd,
  method = "ppi_plusplus",
  model = "logistic",
  data = subset_df,
  label = "set"
)


outsample_ipd_regressions = function(df, y_vars, X_vars) {
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
  
  # Loop through each y variable
  for (y_var in y_vars) {
    # Loop through each X variable
    for (X_var in X_vars) {
      
      # Create a subset of data where both y_var and X_var are not NA
      subset_df = df[!is.na(df[[y_var]]) & !is.na(df[[X_var]]), ]
      
      
      
      # Fit IPD model
      tryCatch({
        ppi_p_model = ipd::ipd(
          formula = formula_ipd,
          method = "ppi_plusplus",
          model = "logistic",
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
        
        colnames(ppi_p_results) = c("Term", "Estimate", "Lower", "Upper")
        
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
  df = df,
  y_vars = c("obese_bmi"),
  X_vars = c("age")
)




