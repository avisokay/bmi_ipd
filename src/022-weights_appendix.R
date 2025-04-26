library(tidyr)

## This script looks at how much data is dropped by subsetting to rows with shared observations
## for each obesity measure y and covariate X. It looks separately at the 2011-2017 and 2021-2023
## data.

# # create survey design object
# nhanes_design <- svydesign(
#   id = ~SDMVPSU,           # Primary Sampling Unit
#   strata = ~SDMVSTRA,      # Stratification variable
#   weights = ~WTMEC10YR,    # Your calculated 10-year weights
#   nest = TRUE,
#   data = subset_df[subset_df$set == "labeled", ]
# )
# 
# # Fit logistic regression model on labeled data only
# tryCatch({
#   model = 
#     svyglm(
#       formula = formula_obj,
#       design = nhanes_design,
#       family = quasibinomial()  # Use quasibinomial for survey-weighted logistic regression
#     )



# load data
df = readRDS("../data/nhanes_cleaned.rds")
df_2011 = df[!is.na(df$year) & df$year %in% c(2011:2018), ]
df_2021 = df[!is.na(df$year) & df$year > 2018, ]

y_vars = c("obese_bmi", "obese_wc", "obese_dxa")
X_vars = c("race", "sex", "age", "smoke", "edu", "income")

calculate_missing_percentage <- function(df, y_vars, X_vars) {
  # First get the baseline rows with all obesity measures present
  baseline_df <- df[!is.na(df[["obese_dxa"]]) & !is.na(df[["obese_bmi"]]) & !is.na(df[["obese_wc"]]), ]
  baseline_rows <- nrow(baseline_df)
  
  # Loop through each y variable
  for (y_var in y_vars) {
    # Loop through each X variable
    for (X_var in X_vars) {
      
      # Create a subset of data where both y_var, X_var, and all obesity variables are not NA
      subset_df <- baseline_df[!is.na(baseline_df[[X_var]]), ]
      
      # Remaining rows
      remaining_rows <- nrow(subset_df)
      
      # Calculate percentage dropped
      percent_dropped <- ((baseline_rows - remaining_rows) / baseline_rows) * 100
      
      # Print the result
      cat(sprintf("For %s ~ %s: %.2f%% of data dropped (%d out of %d rows)\n", 
                  y_var, X_var, percent_dropped, baseline_rows - remaining_rows, baseline_rows))
    }
    # Add a blank line between y variables for better readability
    cat("\n")
  }
}

plot_missing_percentage <- function(df, y_vars, X_vars) {
  # Create a dataframe to store the results
  result_df <- data.frame(
    y_var = character(),
    X_var = character(),
    percent_available = numeric(),
    stringsAsFactors = FALSE
  )
  
  # First get the baseline rows with all obesity measures present
  baseline_df <- df[!is.na(df[["obese_dxa"]]) & !is.na(df[["obese_bmi"]]) & !is.na(df[["obese_wc"]]), ]
  baseline_rows <- nrow(baseline_df)
  
  # Loop through each y variable
  for (y_var in y_vars) {
    # Loop through each X variable
    for (X_var in X_vars) {
      
      # Create a subset of data where both y_var, X_var, and all obesity variables are not NA
      subset_df <- baseline_df[!is.na(baseline_df[[X_var]]), ]
      
      # Remaining rows
      remaining_rows <- nrow(subset_df)
      
      # Calculate percentage available (inverse of dropped)
      percent_available <- (remaining_rows / baseline_rows) * 100
      
      # Add to results dataframe
      result_df <- rbind(result_df, data.frame(
        y_var = y_var,
        X_var = X_var,
        percent_available = percent_available
      ))
    }
  }
  
  # Load ggplot2 for plotting
  library(ggplot2)
  
  # Create the plot with much larger text elements
  p <- ggplot(result_df, aes(x = X_var, y = percent_available, fill = y_var)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(
      title = "Percentage of Data Available by Variable Combinations",
      x = "Predictor Variables",
      y = "Percentage Available (%)",
      fill = "Obesity Measure"
    ) +
    theme_minimal() +
    theme(
      # Much larger axis text
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      
      # Much larger axis titles
      axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 20)),
      
      # Much larger plot title
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(b = 20)),
      
      # Much larger legend text
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 16),
      legend.position = "top",
      
      # Increase plot margins
      plot.margin = margin(20, 20, 20, 20)
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    geom_hline(yintercept = 70, linetype = "dashed", color = "red", size = 1.2)
  
  # Print summary data
  print(result_df)
  
  # Return the plot
  return(p)
}

# ----- CALL THE FUNCTIONS ------

calculate_missing_percentage(df_2011, y_vars, X_vars)

# Generate the plot and save it to a variable

p = plot_missing_percentage(df, y_vars, X_vars)
p

# Save the plot as a high-quality PDF
ggsave(
  filename = "../plots/missing_data_plot.pdf",
  plot = p,
  device = "pdf",
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)




