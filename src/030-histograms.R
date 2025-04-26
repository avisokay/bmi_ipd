library(knitr)
library(dplyr)
library(ggplot2)
library(data.table)
library(ipd)
library(gridExtra)
library(grid)

# ----- NHANES OBESITY EXPLORATORY DATA ANALYSIS -----

# This contains the code used to perform EDA for the NHANES obesity data.
# Plotting distribution of variables and obesity distribution plots.
# See acquire_data.R and clean_data.R for reproducing the data collection, 
# feature engineering, and survey weighting. 

## percentiles, not raw units, relative to obesity threshhold.
## align on obesity threshhold as 0.

# ----- VARIABLE DIST PLOTS -----

# load data
df = readRDS("../data/nhanes_cleaned.rds")

# Define the columns to check for non-missing values
columns_to_check_2011 = c("obese_bmi", "obese_wc", "obese_dxa",
                          "year", "race", "sex", "age", "smoke", 
                          "edu", "income")

# Define the columns to check for non-missing values
columns_to_check_2021 = c("obese_bmi", "obese_wc",
                          "year", "race", "sex", "age", "smoke", 
                          "edu")

# Subset the dataframes
df_2011 = df[complete.cases(df[, columns_to_check_2011]) & df$year %in% c(2011:2018), ]
df_2021 = df[complete.cases(df[, columns_to_check_2021]) & df$year > 2018, ]

# standardize by dividing by obesity thresholds
df_2011$bmi_standard = df_2011$BMXBMI/30
df_2011$wc_standard = ifelse(df_2011$sex == "Male", df_2011$BMXWAIST/102, df_2011$BMXWAIST/88)
df_2011$dxa_standard = ifelse(df_2011$sex == "Male", df_2011$DXDTOPF/30, df_2011$DXDTOPF/42)
df_2021$bmi_standard = df_2021$BMXBMI/30
df_2021$wc_standard = ifelse(df_2021$sex == "Male", df_2021$BMXWAIST/102, df_2021$BMXWAIST/88)
df_2021$dxa_standard = ifelse(df_2021$sex == "Male", df_2021$DXDTOPF/30, df_2021$DXDTOPF/42)
df$bmi_standard = df$BMXBMI/30
df$wc_standard = ifelse(df$sex == "Male", df$BMXWAIST/102, df$BMXWAIST/88)
df$dxa_standard = ifelse(df$sex == "Male", df$DXDTOPF/30, df$DXDTOPF/42)




# plot function to see distribution for each variable
plot_distribution = function(data, var_name, title = NULL, continuous = FALSE, 
                             weighted = TRUE, binwidth = NULL, fill_color = "#4682B4") {
  
  # If no title provided, create one
  if (is.null(title)) {
    title = paste("Distribution of", var_name)
  }
  
  # Check if variable exists in the data
  if (!var_name %in% names(data)) {
    stop(paste("Variable", var_name, "not found in the data"))
  }
  
  # Remove missing values
  plot_data = data[!is.na(data[[var_name]]), ]
  
  if (weighted) {
    # For weighted plots
    if (!"WTMEC2YR" %in% names(data)) {
      warning("Weight variable WTMEC2YR not found, using unweighted plot")
      weighted = FALSE
    } else {
      plot_data = plot_data[!is.na(plot_data$WTMEC2YR), ]
    }
  }
  
  if (continuous) {
    # For continuous variables (like age, BMI)
    p = ggplot(plot_data, aes(x = .data[[var_name]])) +
      geom_histogram(aes(y = ..density.., 
                         weight = if(weighted) WTMEC2YR else NULL),
                     binwidth = binwidth, 
                     fill = fill_color, 
                     color = "white", 
                     alpha = 0.8) +
      geom_density(alpha = 0.2, fill = "gray") +
      labs(title = title,
           x = var_name,
           y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(face = "bold"))
    
  } else {
    # For categorical variables (like race, education)
    if (weighted) {
      # Calculate weighted frequencies
      cat_counts = plot_data %>%
        group_by(.data[[var_name]]) %>%
        summarize(count = sum(WTMEC2YR)) %>%
        mutate(prop = count / sum(count))
      
      p = ggplot(cat_counts, aes(x = .data[[var_name]], y = prop)) +
        geom_bar(stat = "identity", fill = fill_color, width = 0.7) +
        labs(title = title,
             x = var_name,
             y = "Proportion") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.title = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      # Unweighted categorical plot
      p = ggplot(plot_data, aes(x = .data[[var_name]])) +
        geom_bar(aes(y = ..prop..), fill = fill_color, width = 0.7) +
        labs(title = title,
             x = var_name,
             y = "Proportion") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.title = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1))
    }
  }
  
  return(p)
}

# 2011 
vars = colnames(df)[189:197]
plot_list = list()
for (var in vars) {
  plot = plot_distribution(data = df_2011, var_name = var, continuous = FALSE)
  plot_list[[var]] = plot
}

# Add a title to the combined plot
combined_plot = grid.arrange(
  grobs = plot_list,
  ncol = 2,
  top = textGrob(
    "Variable Distributions for 2011-2017 Data",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)

# Save the combined plot as pdf
ggsave("../plots/sex/2011_2017_vardist.pdf", 
       combined_plot, 
       width = 12, 
       height = 16, 
       dpi = 600)

# 2021
plot_list = list()
for (var in vars) {
  plot = plot_distribution(data = df_2021, var_name = var, continuous = FALSE)
  plot_list[[var]] = plot
}

# Add a title to the combined plot
combined_plot = grid.arrange(
  grobs = plot_list,
  ncol = 2,
  top = textGrob(
    "Variable Distributions for 2021-2023 Data",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)

# Save the combined plot as pdf
ggsave("../plots/sex/2021_2023_vardist.pdf", 
       combined_plot, 
       width = 12, 
       height = 16, 
       dpi = 600)

# ALL YEARS
plot_list = list()
for (var in vars) {
  plot = plot_distribution(data = df, var_name = var, continuous = FALSE)
  plot_list[[var]] = plot
}

# Add a title to the combined plot
combined_plot = grid.arrange(
  grobs = plot_list,
  ncol = 2,
  top = textGrob(
    "Variable Distributions for 2011-2023 Data",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)

# Save the combined plot as pdf
ggsave("../plots/sex/2011_2023_vardist.pdf", 
       combined_plot, 
       width = 12, 
       height = 16, 
       dpi = 600)

# ----- OVERLAPPING DIST PLOTS ------

# ----- SEX -----
plot_obesity_measures = function(df, var1, var2, var1_name = NULL, var2_name = NULL, 
                                 var1_cutoffs = c(male = 30, female = 30), 
                                 var2_cutoffs = c(male = 30, female = 42),
                                 binwidth = 1, save_plots = FALSE,
                                 year = NULL) {
  
  # Set default names if not provided
  if (is.null(var1_name)) var1_name = var1
  if (is.null(var2_name)) var2_name = var2
  
  # Prepare year text for title
  year_text = ifelse(!is.null(year), paste(" (", year, ")", sep = ""), "")
  
  # Set x-axis label based on variables
  x_label = NULL
  
  # Define units for common variables
  if (var1 == "BMXBMI" && var2 == "DXDTOPF") {
    x_label = expression(paste("BMI: (", kg/m^2, ") , DXA: (Total Body Fat %)"))
  } else if (var1 == "BMXBMI" && var2 == "BMXWAIST") {
    x_label = expression(paste("BMI: (", kg/m^2, ") , WC: (Waist Circumference (cm))"))
  } else if (var1 == "DXDTOPF" && var2 == "BMXWAIST") {
    x_label = expression(paste("DXA: (Total Body Fat %) , WC: (Waist Circumference (cm))"))
  } else {
    # Default label for other variable combinations
    x_label = paste(var1_name, "and", var2_name, "Values")
  }
  
  # Set line types based on variables
  var1_linetype = "solid"
  var2_linetype = "dashed"
  
  # Special case for BMI and Waist Circumference where both should be dashed
  if (var1 == "BMXBMI" && var2 == "BMXWAIST") {
    var1_linetype = "solid"
    var2_linetype = "dashed"
    
    # For BMI+WC, use specific WC cutoffs if not explicitly provided
    if (identical(var2_cutoffs, c(male = 30, female = 42))) {
      var2_cutoffs = c(male = 102, female = 88)
    }
  }
  
  # Ensure the gender variable is available
  if (!"sex" %in% names(df) && "RIAGENDR" %in% names(df)) {
    df$sex = ifelse(df$RIAGENDR == 1, "Male", "Female")
  } else if (!any(c("sex", "RIAGENDR") %in% names(df))) {
    stop("Neither 'sex' nor 'RIAGENDR' found in dataframe")
  }
  
  # Filter out NA values for both variables
  df_clean = df[!is.na(df[[var1]]) & !is.na(df[[var2]]), ]
  
  # Create plot for males
  male_data = df_clean[df_clean$sex == "Male", ]
  male_hist = ggplot(data = male_data) +
    geom_histogram(aes(x = .data[[var1]], fill = var1_name), 
                   binwidth = binwidth, alpha = 0.4, color = "black") +
    geom_histogram(aes(x = .data[[var2]], fill = var2_name), 
                   binwidth = binwidth, alpha = 0.4, color = "black") +
    scale_fill_manual(values = c("tomato", "darkblue"), 
                      labels = c(var1_name, var2_name),
                      name = "Measurement") +
    geom_vline(xintercept = var1_cutoffs["male"], color = "tomato", 
               linetype = var1_linetype, size = 2) +
    geom_vline(xintercept = var2_cutoffs["male"], color = "darkblue", 
               linetype = var2_linetype, size = 2) +
    labs(title = paste("Distribution for Males", year_text, " (n =", nrow(male_data), ")"),
         x = x_label,
         y = "Frequency") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),      
          axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = c(0.8, 0.7))
  
  # Create plot for females
  female_data = df_clean[df_clean$sex == "Female", ]
  female_hist = ggplot(data = female_data) +
    geom_histogram(aes(x = .data[[var1]], fill = var1_name), 
                   binwidth = binwidth, alpha = 0.4, color = "black") +
    geom_histogram(aes(x = .data[[var2]], fill = var2_name), 
                   binwidth = binwidth, alpha = 0.4, color = "black") +
    scale_fill_manual(values = c("tomato", "darkblue"), 
                      labels = c(var1_name, var2_name),
                      name = "Measurement") +
    geom_vline(xintercept = var1_cutoffs["female"], color = "tomato", 
               linetype = var1_linetype, size = 2) +
    geom_vline(xintercept = var2_cutoffs["female"], color = "darkblue", 
               linetype = var2_linetype, size = 2) +
    labs(title = paste("Distribution for Females", year_text, " (n =", nrow(female_data), ")"),
         x = x_label,
         y = "Frequency") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),      
          axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = c(0.8, 0.7))
  
  # Save plots if requested
  if (save_plots) {
    ggsave(paste0("male_", var1_name, "_", var2_name, year_text, ".png"), 
           male_hist, width = 10, height = 7, dpi = 300)
    ggsave(paste0("female_", var1_name, "_", var2_name, year_text, ".png"), 
           female_hist, width = 10, height = 7, dpi = 300)
  }
  
  # Return both plots in a list
  return(list(male = male_hist, female = female_hist))
}

# Define function to create plots for all combinations of measures
create_all_measure_plots = function(df, year_label) {
  # Define all combinations to plot
  combinations = list(
    list(var1 = "BMXBMI", var2 = "DXDTOPF", var1_name = "BMI", var2_name = "DXA", 
         var1_cutoffs = c(male = 30, female = 30), var2_cutoffs = c(male = 30, female = 42)),
    list(var1 = "BMXBMI", var2 = "BMXWAIST", var1_name = "BMI", var2_name = "WC", 
         var1_cutoffs = c(male = 30, female = 30), var2_cutoffs = c(male = 102, female = 88)),
    list(var1 = "DXDTOPF", var2 = "BMXWAIST", var1_name = "DXA", var2_name = "WC", 
         var1_cutoffs = c(male = 30, female = 42), var2_cutoffs = c(male = 102, female = 88))
  )
  
  # Generate all plots
  all_plots = list()
  
  for (combo in combinations) {
    # Generate plots using our plot_obesity_measures function
    plots = plot_obesity_measures(
      df = df,
      var1 = combo$var1,
      var2 = combo$var2,
      var1_name = combo$var1_name,
      var2_name = combo$var2_name,
      var1_cutoffs = combo$var1_cutoffs,
      var2_cutoffs = combo$var2_cutoffs,
      year = year_label
    )
    
    # Add plots to our list
    all_plots[[paste0(combo$var1_name, "_", combo$var2_name, "_male")]] = plots$male
    all_plots[[paste0(combo$var1_name, "_", combo$var2_name, "_female")]] = plots$female
  }
  
  # Create combined plot with title
  combined_plot = grid.arrange(
    grobs = all_plots,
    ncol = 2,
    top = textGrob(
      paste0("Obesity Measure Distributions for ", year_label, " Data"),
      gp = gpar(fontsize = 16, fontface = "bold")
    )
  )
  
  # Save the combined plot as PDF
  output_file = paste0("../plots/sex/", gsub("-", "_", year_label), "_obesity.pdf")
  ggsave(output_file, combined_plot, width = 12, height = 16, dpi = 600)
  
  return(combined_plot)
}

# Run for each dataset
# Assuming your datasets are named df_2011, df_2021, and df_all
plot_2011 = create_all_measure_plots(df_2011, "2011-2017")
plot_2021 = create_all_measure_plots(df_2021, "2021-2023")
plot_all = create_all_measure_plots(df, "2011-2023")

# Define function to create plots for demographic subgroups by measure combinations
create_demographic_measure_plots = function(df, year_label, demographic_var) {
  # Define all combinations to plot
  combinations = list(
    list(var1 = "BMXBMI", var2 = "DXDTOPF", var1_name = "BMI", var2_name = "DXA", 
         var1_cutoffs = c(male = 30, female = 30), var2_cutoffs = c(male = 30, female = 42)),
    list(var1 = "BMXBMI", var2 = "BMXWAIST", var1_name = "BMI", var2_name = "WC", 
         var1_cutoffs = c(male = 30, female = 30), var2_cutoffs = c(male = 102, female = 88)),
    list(var1 = "DXDTOPF", var2 = "BMXWAIST", var1_name = "DXA", var2_name = "WC", 
         var1_cutoffs = c(male = 30, female = 42), var2_cutoffs = c(male = 102, female = 88))
  )
  
  # Get unique values for the selected demographic variable
  demo_values = unique(df[[demographic_var]])
  demo_values = demo_values[!is.na(demo_values)] # Remove NA values
  
  # Create the directory if it doesn't exist
  dir_path = paste0("../plots/", demographic_var, "/")
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # For each demographic value, create plots by sex and measure combination
  for (demo_val in demo_values) {
    # Filter data for current demographic value
    demo_df = df[df[[demographic_var]] == demo_val, ]
    
    # Generate all plots for this demographic group
    demo_plots = list()
    
    for (combo in combinations) {
      # Generate male plot for this demographic group
      male_df = demo_df[demo_df$sex == "Male", ]
      if (nrow(male_df) > 0) { # Only create plot if we have data
        male_plot = plot_obesity_measures(
          df = male_df,
          var1 = combo$var1,
          var2 = combo$var2,
          var1_name = combo$var1_name,
          var2_name = combo$var2_name,
          var1_cutoffs = combo$var1_cutoffs,
          var2_cutoffs = combo$var2_cutoffs,
          year = year_label
        )
        
        # Add to plots list
        demo_plots[[paste0(combo$var1_name, "_", combo$var2_name, "_male")]] = male_plot$male
      }
      
      # Generate female plot for this demographic group
      female_df = demo_df[demo_df$sex == "Female", ]
      if (nrow(female_df) > 0) { # Only create plot if we have data
        female_plot = plot_obesity_measures(
          df = female_df,
          var1 = combo$var1,
          var2 = combo$var2,
          var1_name = combo$var1_name,
          var2_name = combo$var2_name,
          var1_cutoffs = combo$var1_cutoffs,
          var2_cutoffs = combo$var2_cutoffs,
          year = year_label
        )
        
        # Add to plots list
        demo_plots[[paste0(combo$var1_name, "_", combo$var2_name, "_female")]] = female_plot$female
      }
    }
    
    # Create combined plot with title for this demographic group
    if (length(demo_plots) > 0) {
      # Create a readable title from the variable name and value
      if (demographic_var == "race") {
        title_text = paste0(demo_val, " Obesity Measure Distributions (", year_label, ")")
      } else if (demographic_var == "income") {
        title_text = paste0("Income: ", demo_val, " - Obesity Measures (", year_label, ")")
      } else if (demographic_var == "smoke") {
        title_text = paste0("Smoking Status: ", demo_val, " - Obesity Measures (", year_label, ")")
      } else if (demographic_var == "edu") {
        title_text = paste0("Education: ", demo_val, " - Obesity Measures (", year_label, ")")
      } else {
        title_text = paste0(demographic_var, ": ", demo_val, " - Obesity Measures (", year_label, ")")
      }
      
      demo_combined_plot = grid.arrange(
        grobs = demo_plots,
        ncol = 2,
        top = textGrob(
          title_text,
          gp = gpar(fontsize = 16, fontface = "bold")
        )
      )
      
      # Create safe filename from demographic value
      safe_demo_val = gsub("[^a-zA-Z0-9]", "_", demo_val)
      
      # Save the combined plot as PDF in the variable-specific folder
      output_file = paste0(dir_path, gsub("-", "_", year_label), "_", 
                           safe_demo_val, "_obesity.pdf")
      ggsave(output_file, demo_combined_plot, width = 12, height = 16, dpi = 600)
    }
  }
}


# ----- SEX and RACE -----
create_demographic_measure_plots(df_2011, "2011-2017", "race")
# create_demographic_measure_plots(df_2021, "2021-2023", "race")
# create_demographic_measure_plots(df, "2011-2023", "race")

# ----- SEX and INCOME -----
create_demographic_measure_plots(df_2011, "2011-2017", "income")
# create_demographic_measure_plots(df_2021, "2021-2023", "income")
# create_demographic_measure_plots(df, "2011-2023", "income")

# ----- SEX and SMOKING -----
create_demographic_measure_plots(df_2011, "2011-2017", "smoke")
# create_demographic_measure_plots(df_2021, "2021-2023", "smoke")
# create_demographic_measure_plots(df, "2011-2023", "smoke")

# ----- SEX and EDUCATION -----
create_demographic_measure_plots(df_2011, "2011-2017", "edu")
# create_demographic_measure_plots(df_2021, "2021-2023", "edu")
# create_demographic_measure_plots(df, "2011-2023", "edu")

# ----- SEX and AGE -----
create_demographic_measure_plots(df_2011, "2011-2017", "age")
# create_demographic_measure_plots(df_2021, "2021-2023", "age")
# create_demographic_measure_plots(df, "2011-2023", "age")


# ------ OVERLAPPING STANDARDIZED PLOTS ------

plot_standardized_obesity_measures = function(df, var1, var2, var1_name = NULL, var2_name = NULL, 
                                              binwidth = 0.05, save_plots = FALSE,
                                              year = NULL) {
  
  # Set default names if not provided
  if (is.null(var1_name)) var1_name = var1
  if (is.null(var2_name)) var2_name = var2
  
  # Prepare year text for title
  year_text = ifelse(!is.null(year), paste(" (", year, ")", sep = ""), "")
  
  # Set x-axis label based on variables
  x_label = "Standardized Values (1.0 = Obesity Threshold)"
  
  # Set line types based on variables
  var1_linetype = "solid"
  var2_linetype = "dashed"
  
  # Ensure the gender variable is available
  if (!"sex" %in% names(df) && "RIAGENDR" %in% names(df)) {
    df$sex = ifelse(df$RIAGENDR == 1, "Male", "Female")
  } else if (!any(c("sex", "RIAGENDR") %in% names(df))) {
    stop("Neither 'sex' nor 'RIAGENDR' found in dataframe")
  }
  
  # Filter out NA values for both variables
  df_clean = df[!is.na(df[[var1]]) & !is.na(df[[var2]]), ]
  
  # Create plot for males
  male_data = df_clean[df_clean$sex == "Male", ]
  male_hist = ggplot(data = male_data) +
    geom_histogram(aes(x = .data[[var1]], fill = var1_name), 
                   binwidth = binwidth, alpha = 0.4, color = "black") +
    geom_histogram(aes(x = .data[[var2]], fill = var2_name), 
                   binwidth = binwidth, alpha = 0.4, color = "black") +
    scale_fill_manual(values = c("tomato", "darkblue"), 
                      labels = c(var1_name, var2_name),
                      name = "Measurement") +
    geom_vline(xintercept = 1.0, color = "black", 
               linetype = "longdash", size = 2) +
    labs(title = paste("Standardized Distribution for Males", year_text, " (n =", nrow(male_data), ")"),
         x = x_label,
         y = "Frequency") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),      
          axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = c(0.8, 0.7))
  
  # Create plot for females
  female_data = df_clean[df_clean$sex == "Female", ]
  female_hist = ggplot(data = female_data) +
    geom_histogram(aes(x = .data[[var1]], fill = var1_name), 
                   binwidth = binwidth, alpha = 0.4, color = "black") +
    geom_histogram(aes(x = .data[[var2]], fill = var2_name), 
                   binwidth = binwidth, alpha = 0.4, color = "black") +
    scale_fill_manual(values = c("tomato", "darkblue"), 
                      labels = c(var1_name, var2_name),
                      name = "Measurement") +
    geom_vline(xintercept = 1.0, color = "black", 
               linetype = "longdash", size = 2) +
    labs(title = paste("Standardized Distribution for Females", year_text, " (n =", nrow(female_data), ")"),
         x = x_label,
         y = "Frequency") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),      
          axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = c(0.8, 0.7))
  
  # Save plots if requested
  if (save_plots) {
    ggsave(paste0("male_", var1_name, "_", var2_name, year_text, "_standard.png"), 
           male_hist, width = 10, height = 7, dpi = 300)
    ggsave(paste0("female_", var1_name, "_", var2_name, year_text, "_standard.png"), 
           female_hist, width = 10, height = 7, dpi = 300)
  }
  
  # Return both plots in a list
  return(list(male = male_hist, female = female_hist))
}

# Define function to create standardized plots for all combinations of measures
create_all_standardized_measure_plots = function(df, year_label) {
  # Define all combinations to plot
  combinations = list(
    list(var1 = "bmi_standard", var2 = "dxa_standard", var1_name = "BMI", var2_name = "DXA"),
    list(var1 = "bmi_standard", var2 = "wc_standard", var1_name = "BMI", var2_name = "WC"),
    list(var1 = "dxa_standard", var2 = "wc_standard", var1_name = "DXA", var2_name = "WC")
  )
  
  # Generate all plots
  all_plots = list()
  
  for (combo in combinations) {
    # Generate plots using our standardized plot function
    plots = plot_standardized_obesity_measures(
      df = df,
      var1 = combo$var1,
      var2 = combo$var2,
      var1_name = combo$var1_name,
      var2_name = combo$var2_name,
      year = year_label
    )
    
    # Add plots to our list
    all_plots[[paste0(combo$var1_name, "_", combo$var2_name, "_male")]] = plots$male
    all_plots[[paste0(combo$var1_name, "_", combo$var2_name, "_female")]] = plots$female
  }
  
  # Create combined plot with title
  combined_plot = grid.arrange(
    grobs = all_plots,
    ncol = 2,
    top = textGrob(
      paste0("Standardized Obesity Measure Distributions for ", year_label, " Data"),
      gp = gpar(fontsize = 16, fontface = "bold")
    )
  )
  
  # Save the combined plot as PDF
  output_file = paste0("../plots/sex/", gsub("-", "_", year_label), "_obesity_standard.pdf")
  ggsave(output_file, combined_plot, width = 12, height = 16, dpi = 600)
  
  return(combined_plot)
}

# Define function to create standardized plots for demographic subgroups
create_demographic_standardized_measure_plots = function(df, year_label, demographic_var) {
  # Define all combinations to plot
  combinations = list(
    list(var1 = "bmi_standard", var2 = "dxa_standard", var1_name = "BMI", var2_name = "DXA"),
    list(var1 = "bmi_standard", var2 = "wc_standard", var1_name = "BMI", var2_name = "WC"),
    list(var1 = "dxa_standard", var2 = "wc_standard", var1_name = "DXA", var2_name = "WC")
  )
  
  # Get unique values for the selected demographic variable
  demo_values = unique(df[[demographic_var]])
  demo_values = demo_values[!is.na(demo_values)] # Remove NA values
  
  # Create the directory if it doesn't exist
  dir_path = paste0("../plots/", demographic_var, "/")
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # For each demographic value, create plots by sex and measure combination
  for (demo_val in demo_values) {
    # Filter data for current demographic value
    demo_df = df[df[[demographic_var]] == demo_val, ]
    
    # Generate all plots for this demographic group
    demo_plots = list()
    
    for (combo in combinations) {
      # Generate male plot for this demographic group
      male_df = demo_df[demo_df$sex == "Male", ]
      if (nrow(male_df) > 0) { # Only create plot if we have data
        male_plot = plot_standardized_obesity_measures(
          df = male_df,
          var1 = combo$var1,
          var2 = combo$var2,
          var1_name = combo$var1_name,
          var2_name = combo$var2_name,
          year = year_label
        )
        
        # Add to plots list
        demo_plots[[paste0(combo$var1_name, "_", combo$var2_name, "_male")]] = male_plot$male
      }
      
      # Generate female plot for this demographic group
      female_df = demo_df[demo_df$sex == "Female", ]
      if (nrow(female_df) > 0) { # Only create plot if we have data
        female_plot = plot_standardized_obesity_measures(
          df = female_df,
          var1 = combo$var1,
          var2 = combo$var2,
          var1_name = combo$var1_name,
          var2_name = combo$var2_name,
          year = year_label
        )
        
        # Add to plots list
        demo_plots[[paste0(combo$var1_name, "_", combo$var2_name, "_female")]] = female_plot$female
      }
    }
    
    # Create combined plot with title for this demographic group
    if (length(demo_plots) > 0) {
      # Create a readable title from the variable name and value
      if (demographic_var == "race") {
        title_text = paste0(demo_val, " Standardized Obesity Measures (", year_label, ")")
      } else if (demographic_var == "income") {
        title_text = paste0("Income: ", demo_val, " - Standardized Obesity Measures (", year_label, ")")
      } else if (demographic_var == "smoke") {
        title_text = paste0("Smoking Status: ", demo_val, " - Standardized Obesity Measures (", year_label, ")")
      } else if (demographic_var == "edu") {
        title_text = paste0("Education: ", demo_val, " - Standardized Obesity Measures (", year_label, ")")
      } else {
        title_text = paste0(demographic_var, ": ", demo_val, " - Standardized Obesity Measures (", year_label, ")")
      }
      
      demo_combined_plot = grid.arrange(
        grobs = demo_plots,
        ncol = 2,
        top = textGrob(
          title_text,
          gp = gpar(fontsize = 16, fontface = "bold")
        )
      )
      
      # Create safe filename from demographic value
      safe_demo_val = gsub("[^a-zA-Z0-9]", "_", demo_val)
      
      # Save the combined plot as PDF in the variable-specific folder with _standard suffix
      output_file = paste0(dir_path, gsub("-", "_", year_label), "_", 
                           safe_demo_val, "_obesity_standard.pdf")
      ggsave(output_file, demo_combined_plot, width = 12, height = 16, dpi = 600)
    }
  }
}

# First, standardize the variables for all datasets
# For dataset df_2011
df_2011$bmi_standard = df_2011$BMXBMI/30
df_2011$wc_standard = ifelse(df_2011$sex == "Male", df_2011$BMXWAIST/102, df_2011$BMXWAIST/88)
df_2011$dxa_standard = ifelse(df_2011$sex == "Male", df_2011$DXDTOPF/30, df_2011$DXDTOPF/42)

# For dataset df_2021
df_2021$bmi_standard = df_2021$BMXBMI/30
df_2021$wc_standard = ifelse(df_2021$sex == "Male", df_2021$BMXWAIST/102, df_2021$BMXWAIST/88)
df_2021$dxa_standard = ifelse(df_2021$sex == "Male", df_2021$DXDTOPF/30, df_2021$DXDTOPF/42)

# Run standardized plots for each dataset
# For all obesity measures
plot_2011_std = create_all_standardized_measure_plots(df_2011, "2011-2017")
# plot_2021_std = create_all_standardized_measure_plots(df_2021, "2021-2023")
# plot_all_std = create_all_standardized_measure_plots(df, "2011-2023")

# For demographic subgroups
# Race
create_demographic_standardized_measure_plots(df_2011, "2011-2017", "race")
# create_demographic_standardized_measure_plots(df_2021, "2021-2023", "race")
# create_demographic_standardized_measure_plots(df, "2011-2023", "race")

# Income
create_demographic_standardized_measure_plots(df_2011, "2011-2017", "income")
# create_demographic_standardized_measure_plots(df_2021, "2021-2023", "income")
# create_demographic_standardized_measure_plots(df, "2011-2023", "income")

# Smoking
create_demographic_standardized_measure_plots(df_2011, "2011-2017", "smoke")
# create_demographic_standardized_measure_plots(df_2021, "2021-2023", "smoke")
# create_demographic_standardized_measure_plots(df, "2011-2023", "smoke")

# Education
create_demographic_standardized_measure_plots(df_2011, "2011-2017", "edu")
# create_demographic_standardized_measure_plots(df_2021, "2021-2023", "edu")
# create_demographic_standardized_measure_plots(df, "2011-2023", "edu")

# Age
create_demographic_standardized_measure_plots(df_2011, "2011-2017", "age")
# create_demographic_standardized_measure_plots(df_2021, "2021-2023", "age")
# create_demographic_standardized_measure_plots(df, "2011-2023", "age")

# ----- KERNEL DENSITY PLOTS -----

plot_histogram_with_density = function(df, year_label) {
  # Prepare data by sex - keep all available data for each measure
  male_bmi = df[df$sex == "Male" & !is.na(df$bmi_standard), ]
  male_wc = df[df$sex == "Male" & !is.na(df$wc_standard), ]
  male_dxa = df[df$sex == "Male" & !is.na(df$dxa_standard), ]
  
  female_bmi = df[df$sex == "Female" & !is.na(df$bmi_standard), ]
  female_wc = df[df$sex == "Female" & !is.na(df$wc_standard), ]
  female_dxa = df[df$sex == "Female" & !is.na(df$dxa_standard), ]
  
  # Setting common plotting parameters
  x_limits = c(0, 2)  # Covering 0 to 2x the obesity threshold
  binwidth = 0.05
  line_size = 2.5    # Thicker line size
  legend_position = c(0.8, 0.8)
  
  # Create male plot with histograms and density lines
  male_plot = ggplot() +
    # BMI histogram
    geom_histogram(data = male_bmi, aes(x = bmi_standard, fill = "BMI", y = ..count..), 
                   binwidth = binwidth, alpha = 0.4, color = "black", position = "identity") +
    # WC histogram
    geom_histogram(data = male_wc, aes(x = wc_standard, fill = "WC", y = ..count..), 
                   binwidth = binwidth, alpha = 0.4, color = "black", position = "identity") +
    # DXA histogram
    geom_histogram(data = male_dxa, aes(x = dxa_standard, fill = "DXA", y = ..count..), 
                   binwidth = binwidth, alpha = 0.4, color = "black", position = "identity") +
    # Add density lines with different line types AND colors
    geom_density(data = male_bmi, aes(x = bmi_standard, linetype = "BMI", y = ..count..), 
                 size = line_size, color = "darkred") +
    geom_density(data = male_wc, aes(x = wc_standard, linetype = "WC", y = ..count..), 
                 size = line_size, color = "darkblue") +
    geom_density(data = male_dxa, aes(x = dxa_standard, linetype = "DXA", y = ..count..), 
                 size = line_size, color = "darkgreen") +
    # Reference line for obesity threshold
    geom_vline(xintercept = 1, linetype = "solid", color = "black", size = 1.5) +
    # Use grayscale for histograms but hide from legend
    scale_fill_manual(name = "Measure", 
                      values = c("BMI" = "gray90", "WC" = "gray70", "DXA" = "gray50"),
                      labels = c(paste0("BMI (n=", nrow(male_bmi), ")"),
                                 paste0("WC (n=", nrow(male_wc), ")"),
                                 paste0("DXA (n=", nrow(male_dxa), ")")),
                      guide = "none") +
    # Use more distinct line types for density curves
    scale_linetype_manual(name = "Measure", 
                          values = c("BMI" = "solid", "WC" = "dashed", "DXA" = "dotdash"),
                          labels = c(paste0("BMI (n=", nrow(male_bmi), ")"),
                                     paste0("WC (n=", nrow(male_wc), ")"),
                                     paste0("DXA (n=", nrow(male_dxa), ")")),
                          guide = guide_legend(
                            override.aes = list(
                              color = c("darkred", "darkblue", "darkgreen"),
                              size = line_size
                            )
                          )) +
    # Labels and title
    labs(title = paste("Males:", year_label),
         x = "Standardized Values (1.0 = Obesity Threshold)",
         y = NULL) +
    # Set x-axis limits
    xlim(x_limits) +
    # Theme customization
    theme_bw() +
    theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
          axis.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_blank(),    # Remove y-axis text
          axis.ticks.y = element_blank(),   # Remove y-axis ticks
          legend.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 16),
          legend.position = legend_position,
          legend.background = element_rect(fill = "white", color = "black"),
          legend.key.size = unit(1.2, "cm"),
          legend.key.height = unit(0.7, "cm"),   # Shorter key height for line display
          legend.key.width = unit(2, "cm"))      # Wider key width for better line display
  
  # Create female plot with histograms and density lines
  female_plot = ggplot() +
    # BMI histogram
    geom_histogram(data = female_bmi, aes(x = bmi_standard, fill = "BMI", y = ..count..), 
                   binwidth = binwidth, alpha = 0.4, color = "black", position = "identity") +
    # WC histogram
    geom_histogram(data = female_wc, aes(x = wc_standard, fill = "WC", y = ..count..), 
                   binwidth = binwidth, alpha = 0.4, color = "black", position = "identity") +
    # DXA histogram
    geom_histogram(data = female_dxa, aes(x = dxa_standard, fill = "DXA", y = ..count..), 
                   binwidth = binwidth, alpha = 0.4, color = "black", position = "identity") +
    # Add density lines with different line types AND colors
    geom_density(data = female_bmi, aes(x = bmi_standard, linetype = "BMI", y = ..count..), 
                 size = line_size, color = "darkred") +
    geom_density(data = female_wc, aes(x = wc_standard, linetype = "WC", y = ..count..), 
                 size = line_size, color = "darkblue") +
    geom_density(data = female_dxa, aes(x = dxa_standard, linetype = "DXA", y = ..count..), 
                 size = line_size, color = "darkgreen") +
    # Reference line for obesity threshold
    geom_vline(xintercept = 1, linetype = "solid", color = "black", size = 1.5) +
    # Use grayscale for histograms but hide from legend
    scale_fill_manual(name = "Measure", 
                      values = c("BMI" = "gray90", "WC" = "gray70", "DXA" = "gray50"),
                      labels = c(paste0("BMI (n=", nrow(female_bmi), ")"),
                                 paste0("WC (n=", nrow(female_wc), ")"),
                                 paste0("DXA (n=", nrow(female_dxa), ")")),
                      guide = "none") +
    # Use more distinct line types for density curves
    scale_linetype_manual(name = "Measure", 
                          values = c("BMI" = "solid", "WC" = "dashed", "DXA" = "dotdash"),
                          labels = c(paste0("BMI (n=", nrow(female_bmi), ")"),
                                     paste0("WC (n=", nrow(female_wc), ")"),
                                     paste0("DXA (n=", nrow(female_dxa), ")")),
                          guide = guide_legend(
                            override.aes = list(
                              color = c("darkred", "darkblue", "darkgreen"),
                              size = line_size
                            )
                          )) +
    # Labels and title
    labs(title = paste("Females:", year_label),
         x = "Standardized Values (1.0 = Obesity Threshold)",
         y = NULL) +
    # Set x-axis limits
    xlim(x_limits) +
    # Theme customization
    theme_bw() +
    theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
          axis.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_blank(),    # Remove y-axis text
          axis.ticks.y = element_blank(),   # Remove y-axis ticks
          legend.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 16),
          legend.position = legend_position,
          legend.background = element_rect(fill = "white", color = "black"),
          legend.key.size = unit(1.2, "cm"),
          legend.key.height = unit(0.7, "cm"),   # Shorter key height for line display
          legend.key.width = unit(2, "cm"))      # Wider key width for better line display
  
  # Combine plots into a single figure - NOW SIDE BY SIDE (landscape)
  combined_plot = grid.arrange(
    male_plot, female_plot,
    ncol = 2, # Changed from ncol = 1 to ncol = 2 for side-by-side layout
    top = textGrob(
      paste0("Standardized Obesity Measures (", year_label, ")"),
      gp = gpar(fontsize = 28, fontface = "bold")
    )
  )
  
  # Save the combined plot - WIDTH AND HEIGHT SWAPPED FOR LANDSCAPE
  output_file = paste0("../plots/sex/", gsub("-", "_", year_label), "_hist_density_standard_bw.pdf")
  ggsave(output_file, combined_plot, width = 20, height = 8, dpi = 600) # Changed from 16x20 to 20x16
  
  return(combined_plot)
}

# Function for demographic subgroups with histograms and density lines
create_demographic_hist_density_plots = function(df, year_label, demographic_var) {
  # Get unique values for the selected demographic variable
  demo_values = unique(df[[demographic_var]])
  demo_values = demo_values[!is.na(demo_values)] # Remove NA values
  
  # Create the directory if it doesn't exist
  dir_path = paste0("../plots/", demographic_var, "/")
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Setting common plotting parameters
  x_limits = c(0, 2)  # Covering 0 to 2x the obesity threshold
  binwidth = 0.05
  line_size = 2.5    # Thicker line size
  legend_position = c(0.8, 0.8)
  
  # For each demographic value, create plots
  for (demo_val in demo_values) {
    # Filter data for current demographic value
    demo_df = df[df[[demographic_var]] == demo_val, ]
    
    # Prepare data by sex and measure - keep all available data for each
    male_bmi = demo_df[demo_df$sex == "Male" & !is.na(demo_df$bmi_standard), ]
    male_wc = demo_df[demo_df$sex == "Male" & !is.na(demo_df$wc_standard), ]
    male_dxa = demo_df[demo_df$sex == "Male" & !is.na(demo_df$dxa_standard), ]
    
    female_bmi = demo_df[demo_df$sex == "Female" & !is.na(demo_df$bmi_standard), ]
    female_wc = demo_df[demo_df$sex == "Female" & !is.na(demo_df$wc_standard), ]
    female_dxa = demo_df[demo_df$sex == "Female" & !is.na(demo_df$dxa_standard), ]
    
    # Check if we have data for both sexes (at least one measure)
    has_male_data = nrow(male_bmi) > 0 || nrow(male_wc) > 0 || nrow(male_dxa) > 0
    has_female_data = nrow(female_bmi) > 0 || nrow(female_wc) > 0 || nrow(female_dxa) > 0
    
    if (has_male_data && has_female_data) {
      # Create male plot
      male_plot = ggplot() +
        # Add histograms and density lines only for measures with data
        {if(nrow(male_bmi) > 0) geom_histogram(data = male_bmi, 
                                               aes(x = bmi_standard, fill = "BMI", y = ..count..), 
                                               binwidth = binwidth, alpha = 0.4, color = "black", position = "identity")} +
        {if(nrow(male_wc) > 0) geom_histogram(data = male_wc, 
                                              aes(x = wc_standard, fill = "WC", y = ..count..), 
                                              binwidth = binwidth, alpha = 0.4, color = "black", position = "identity")} +
        {if(nrow(male_dxa) > 0) geom_histogram(data = male_dxa, 
                                               aes(x = dxa_standard, fill = "DXA", y = ..count..), 
                                               binwidth = binwidth, alpha = 0.4, color = "black", position = "identity")} +
        
        # Add density lines with different line types AND colors
        {if(nrow(male_bmi) > 0) geom_density(data = male_bmi, 
                                             aes(x = bmi_standard, linetype = "BMI", y = ..count..), 
                                             size = line_size, color = "darkred")} +
        {if(nrow(male_wc) > 0) geom_density(data = male_wc, 
                                            aes(x = wc_standard, linetype = "WC", y = ..count..), 
                                            size = line_size, color = "darkblue")} +
        {if(nrow(male_dxa) > 0) geom_density(data = male_dxa, 
                                             aes(x = dxa_standard, linetype = "DXA", y = ..count..), 
                                             size = line_size, color = "darkgreen")} +
        
        # Reference line for obesity threshold
        geom_vline(xintercept = 1, linetype = "solid", color = "black", size = 1.5) +
        
        # Use grayscale for histograms but hide from legend
        scale_fill_manual(name = "Measure", 
                          values = c("BMI" = "gray90", "WC" = "gray70", "DXA" = "gray50"),
                          labels = c(paste0("BMI (n=", nrow(male_bmi), ")"),
                                     paste0("WC (n=", nrow(male_wc), ")"),
                                     paste0("DXA (n=", nrow(male_dxa), ")")),
                          guide = "none") +
        # Use more distinct line types for density curves
        scale_linetype_manual(name = "Measure", 
                              values = c("BMI" = "solid", "WC" = "dashed", "DXA" = "dotdash"),
                              labels = c(paste0("BMI (n=", nrow(male_bmi), ")"),
                                         paste0("WC (n=", nrow(male_wc), ")"),
                                         paste0("DXA (n=", nrow(male_dxa), ")")),
                              guide = guide_legend(
                                override.aes = list(
                                  color = c("darkred", "darkblue", "darkgreen"),
                                  size = line_size
                                )
                              )) +
        
        # Labels and title
        labs(title = paste("Males:", demo_val),
             x = "Standardized Values (1.0 = Obesity Threshold)",
             y = NULL) +
        
        # Set x-axis limits
        xlim(x_limits) +
        
        # Theme customization
        theme_bw() +
        theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 20, face = "bold"),
              axis.text.x = element_text(size = 16),
              axis.text.y = element_blank(),    # Remove y-axis text
              axis.ticks.y = element_blank(),   # Remove y-axis ticks
              legend.title = element_text(size = 18, face = "bold"),
              legend.text = element_text(size = 16),
              legend.position = legend_position,
              legend.background = element_rect(fill = "white", color = "black"),
              legend.key.size = unit(1.2, "cm"),
              legend.key.height = unit(0.7, "cm"),   # Shorter key height for line display
              legend.key.width = unit(2, "cm"))      # Wider key width for better line display
      
      # Create female plot
      female_plot = ggplot() +
        # Add histograms and density lines only for measures with data
        {if(nrow(female_bmi) > 0) geom_histogram(data = female_bmi, 
                                                 aes(x = bmi_standard, fill = "BMI", y = ..count..), 
                                                 binwidth = binwidth, alpha = 0.4, color = "black", position = "identity")} +
        {if(nrow(female_wc) > 0) geom_histogram(data = female_wc, 
                                                aes(x = wc_standard, fill = "WC", y = ..count..), 
                                                binwidth = binwidth, alpha = 0.4, color = "black", position = "identity")} +
        {if(nrow(female_dxa) > 0) geom_histogram(data = female_dxa, 
                                                 aes(x = dxa_standard, fill = "DXA", y = ..count..), 
                                                 binwidth = binwidth, alpha = 0.4, color = "black", position = "identity")} +
        
        # Add density lines with different line types AND colors
        {if(nrow(female_bmi) > 0) geom_density(data = female_bmi, 
                                               aes(x = bmi_standard, linetype = "BMI", y = ..count..), 
                                               size = line_size, color = "darkred")} +
        {if(nrow(female_wc) > 0) geom_density(data = female_wc, 
                                              aes(x = wc_standard, linetype = "WC", y = ..count..), 
                                              size = line_size, color = "darkblue")} +
        {if(nrow(female_dxa) > 0) geom_density(data = female_dxa, 
                                               aes(x = dxa_standard, linetype = "DXA", y = ..count..), 
                                               size = line_size, color = "darkgreen")} +
        
        # Reference line for obesity threshold
        geom_vline(xintercept = 1, linetype = "solid", color = "black", size = 1.5) +
        
        # Use grayscale for histograms but hide from legend
        scale_fill_manual(name = "Measure", 
                          values = c("BMI" = "gray90", "WC" = "gray70", "DXA" = "gray50"),
                          labels = c(paste0("BMI (n=", nrow(female_bmi), ")"),
                                     paste0("WC (n=", nrow(female_wc), ")"),
                                     paste0("DXA (n=", nrow(female_dxa), ")")),
                          guide = "none") +
        # Use more distinct line types for density curves
        scale_linetype_manual(name = "Measure", 
                              values = c("BMI" = "solid", "WC" = "dashed", "DXA" = "dotdash"),
                              labels = c(paste0("BMI (n=", nrow(female_bmi), ")"),
                                         paste0("WC (n=", nrow(female_wc), ")"),
                                         paste0("DXA (n=", nrow(female_dxa), ")")),
                              guide = guide_legend(
                                override.aes = list(
                                  color = c("darkred", "darkblue", "darkgreen"),
                                  size = line_size
                                )
                              )) +
        
        # Labels and title
        labs(title = paste("Females:", demo_val),
             x = "Standardized Values (1.0 = Obesity Threshold)",
             y = NULL) +
        
        # Set x-axis limits
        xlim(x_limits) +
        
        # Theme customization
        theme_bw() +
        theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 20, face = "bold"),
              axis.text.x = element_text(size = 16),
              axis.text.y = element_blank(),    # Remove y-axis text
              axis.ticks.y = element_blank(),   # Remove y-axis ticks
              legend.title = element_text(size = 18, face = "bold"),
              legend.text = element_text(size = 16),
              legend.position = legend_position,
              legend.background = element_rect(fill = "white", color = "black"),
              legend.key.size = unit(1.2, "cm"),
              legend.key.height = unit(0.7, "cm"),   # Shorter key height for line display
              legend.key.width = unit(2, "cm"))      # Wider key width for better line display
      
      # Create title text based on demographic variable
      if (demographic_var == "race") {
        title_text = paste0(demo_val, " - Standardized Obesity Measures (", year_label, ")")
      } else if (demographic_var == "income") {
        title_text = paste0("Income: ", demo_val, " - Standardized Obesity Measures (", year_label, ")")
      } else if (demographic_var == "smoke") {
        title_text = paste0("Smoking Status: ", demo_val, " - Standardized Obesity Measures (", year_label, ")")
      } else if (demographic_var == "edu") {
        title_text = paste0("Education: ", demo_val, " - Standardized Obesity Measures (", year_label, ")")
      } else {
        title_text = paste0(demographic_var, ": ", demo_val, " - Standardized Obesity Measures (", year_label, ")")
      }
      
      # Combine plots - NOW SIDE BY SIDE (landscape)
      combined_plot = grid.arrange(
        male_plot, female_plot,
        ncol = 2, # Changed from ncol = 1 to ncol = 2 for side-by-side layout
        top = textGrob(
          title_text,
          gp = gpar(fontsize = 28, fontface = "bold")
        )
      )
      
      # Create safe filename
      safe_demo_val = gsub("[^a-zA-Z0-9]", "_", demo_val)
      
      # Save the combined plot - WIDTH AND HEIGHT SWAPPED FOR LANDSCAPE
      output_file = paste0(dir_path, gsub("-", "_", year_label), "_", 
                           safe_demo_val, "_hist_density_standard_bw.pdf")
      ggsave(output_file, combined_plot, width = 20, height = 8, dpi = 600) # Changed from 16x20 to 20x16
    }
  }
}

# Generate histogram+density plots for each dataset
# Main datasets
hist_density_2011_bw = plot_histogram_with_density(df_2011, "2011-2017")
# hist_density_2021_bw = plot_histogram_with_density(df_2021, "2021-2023")
# hist_density_all_bw = plot_histogram_with_density(df, "2011-2023")

# Demographic subgroups
# Race
create_demographic_hist_density_plots(df_2011, "2011-2017", "race")
# create_demographic_hist_density_plots(df_2021, "2021-2023", "race")
# create_demographic_hist_density_plots(df, "2011-2023", "race")

# Income
create_demographic_hist_density_plots(df_2011, "2011-2017", "income") 
# create_demographic_hist_density_plots(df_2021, "2021-2023", "income")
# create_demographic_hist_density_plots(df, "2011-2023", "income")

# Smoking
create_demographic_hist_density_plots(df_2011, "2011-2017", "smoke")
# create_demographic_hist_density_plots(df_2021, "2021-2023", "smoke")
# create_demographic_hist_density_plots(df, "2011-2023", "smoke")

# Education
create_demographic_hist_density_plots(df_2011, "2011-2017", "edu")
# create_demographic_hist_density_plots(df_2021, "2021-2023", "edu")
# create_demographic_hist_density_plots(df, "2011-2023", "edu")

# Age
create_demographic_hist_density_plots(df_2011, "2011-2017", "age")
# create_demographic_hist_density_plots(df_2021, "2021-2023", "age")
# create_demographic_hist_density_plots(df, "2011-2023", "age")



