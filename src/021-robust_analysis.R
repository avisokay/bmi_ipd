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
if (!require("ipd")) devtools::install_github("ipd-tools/ipd")
library(ipd)

# set number of digits to 2 for tidy outputs later on
options(digits=2)

# ----- ROBUSTNESS CHECKS -----
# Here I check to see if the regression results vary much if you change where the 
# obesity thresholds are, or if you regress on continuous measures instead of binary
# categories. 

# load data
df = readRDS("../data/nhanes_cleaned.rds")
df_2011 = df[!is.na(df$year) & df$year %in% c(2011:2018), ]
df_2021 = df[!is.na(df$year) & df$year > 2018, ]

# Before running regressions, standardize the continuous variables
df_2011$BMXBMI_z = scale(df_2011$BMXBMI)
df_2011$BMXWAIST_z = scale(df_2011$BMXWAIST)
df_2011$DXDTOPF_z = scale(df_2011$DXDTOPF)

# function that takes a dataframe, y_vars, X_vars and estimates a univariate 
# regression for each permutation of y_var and X_var on a subset of the 
# dataframe that has been divided into "labeled" and "unlabeled". 

# ----- REGRESSION FUNCTION -----
insample_continuous_regressions = function(df, y_vars, X_vars, unlab_proportion = 0.3, seed = 123) {
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
      # Still include obese_dxa for data consistency with the split
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
      
      # Fit linear regression model on labeled data only using svyglm with gaussian family
      tryCatch({
        model = svyglm(
          formula = formula_obj,
          design = nhanes_design,
          family = gaussian()  # Change to gaussian for continuous outcomes
        )
        
        # Get coefficients with confidence intervals (no need to exponentiate for linear models)
        model_coef = cbind(coef(model), confint(model))
        
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

# Then use these standardized variables in your regression
insample_continuous_results = insample_continuous_regressions(
  df = df_2011,
  y_vars = c("BMXBMI_z", "BMXWAIST_z", "DXDTOPF_z"),
  X_vars = c("race", "sex", "age", "smoke", "edu", "income")
)

# ----- ESTIMATE CONTINUOUS -----

# Estimate continuous results
insample_continuous_results = insample_continuous_regressions(
  df = df_2011,
  y_vars = c("BMXBMI", "BMXWAIST", "DXDTOPF"),  # Changed to continuous variables
  X_vars = c("race", "sex", "age", "smoke", "edu", "income")
)

# Standardize the variables while preserving their distributions
df_2011$BMXBMI_z = scale(df_2011$BMXBMI)
df_2011$BMXWAIST_z = scale(df_2011$BMXWAIST)
df_2011$DXDTOPF_z = scale(df_2011$DXDTOPF)

# Run regressions on standardized outcomes
insample_standardized_results = insample_continuous_regressions(
  df = df_2011,
  y_vars = c("BMXBMI_z", "BMXWAIST_z", "DXDTOPF_z"),
  X_vars = c("race", "sex", "age", "smoke", "edu", "income")
)

# Save outputs to csv
write.csv(insample_continuous_results, "../results/robust/robust_continuous.csv",
          row.names = FALSE)

# Save outputs
write.csv(insample_standardized_results, "../results/robust/robust_standardized.csv", 
          row.names = FALSE)

# ----- BINARY REGRESSION WITH DIFFERENT CUTOFFS -----

# BMI CATEGORIES FROM 1998
df_2011$obese_bmi_1998 = NA_integer_
df_2011$obese_bmi_1998 = ifelse(df_2011$sex=="Male" & df_2011$BMXBMI > 27.8, 1, 0)
df_2011$obese_bmi_1998 = ifelse(df_2011$sex=="Female" & df_2011$BMXBMI > 27.3, 1, 0)

# ETHNICITY-ADJUSTED BMI CATEGORIES 

# Since the article doesn't mention Hispanic populations, 
# I'll use the standard cutoff of 30.0 kg/m² for this group. 
df_2011$obese_bmi_eth = ifelse(df_2011$race == "White", 
                          ifelse(df_2011$BMXBMI >= 30.0, 1, 0),
                          ifelse(df_2011$race == "Black", 
                                 ifelse(df_2011$BMXBMI >= 28.1, 1, 0),
                                 ifelse(df_2011$race == "Asian", 
                                        ifelse(df_2011$BMXBMI >= 23.9, 1, 0),
                                        ifelse(df_2011$race == "Hispanic", 
                                               ifelse(df_2011$BMXBMI >= 30.0, 1, 0),
                                               NA))))

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
      # Modified this line to be more general - not hardcoded to obese_dxa
      subset_df = df[!is.na(df[[y_var]]) & !is.na(df[[X_var]]), ]
      
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

# estimate robust results
alt_cutoff_results = insample_naive_regressions(
  df = df_2011,
  y_vars = c("obese_dxa", "obese_bmi", "obese_bmi_1998", "obese_bmi_eth"),
  X_vars = c("race", "sex", "age", "smoke", "edu", "income")
)

# Save outputs to csv
write.csv(alt_cutoff_results, "../results/robust/alt_cutoff_results.csv",
          row.names = FALSE)









