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
create_demographic_measure_plots(df_2021, "2021-2023", "race")
create_demographic_measure_plots(df, "2011-2023", "race")

# ----- SEX and INCOME -----
create_demographic_measure_plots(df_2011, "2011-2017", "income")
create_demographic_measure_plots(df_2021, "2021-2023", "income")
create_demographic_measure_plots(df, "2011-2023", "income")

# ----- SEX and SMOKING -----
create_demographic_measure_plots(df_2011, "2011-2017", "smoke")
create_demographic_measure_plots(df_2021, "2021-2023", "smoke")
create_demographic_measure_plots(df, "2011-2023", "smoke")

# ----- SEX and EDUCATION -----
create_demographic_measure_plots(df_2011, "2011-2017", "edu")
create_demographic_measure_plots(df_2021, "2021-2023", "edu")
create_demographic_measure_plots(df, "2011-2023", "edu")

# ----- SEX and AGE -----
create_demographic_measure_plots(df_2011, "2011-2017", "age")
create_demographic_measure_plots(df_2021, "2021-2023", "age")
create_demographic_measure_plots(df, "2011-2023", "age")

# ----- CONFUSION MATRICES -----
# is it worth doing these? 







