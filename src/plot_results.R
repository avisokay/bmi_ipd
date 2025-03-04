library(dplyr)
library(ggplot2)
library(gridExtra)

# ----- PLOT RESULTS OF REGRESSION ANALYSIS -----

df = read.csv("../results/insample_estimates.csv")

# Define function to save high-resolution PDFs
save_high_res_plot = function(plot, filename, width = 16, height = 12, dpi = 600) {
  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    device = cairo_pdf
  )
}

# Function to save all plots in a single multi-page PDF
save_all_plots_pdf = function(plot_list, filename = "all_plots.pdf", width = 8.5, height = 11) {
  pdf(filename, width = width, height = height, useDingbats = FALSE)
  for (p in plot_list) {
    print(p)
  }
  dev.off()
  cat("All plots saved to", filename, "\n")
}

# Create a function to generate a dotplot for a specific x_variable
create_dotplot = function(data, x_var) {
  # Filter data for the specified x_variable
  filtered_data = data %>%
    filter(x_variable == x_var)
  
  # Define label mapping for the y_variable (models)
  model_labels = c(
    "obese_bmi" = "BMI",
    "obese_wc" = "WC",
    "obese_dxa" = "DXA",
    "ipd_obese_bmi" = "IPD_BMI",
    "ipd_obese_wc" = "IPD_WC"
  )
  
  # Add readable model labels
  filtered_data$model = model_labels[filtered_data$y_variable]
  
  # Set factor levels to ensure consistent ordering
  filtered_data$model = factor(filtered_data$model, 
                                levels = c("IPD_BMI", "IPD_WC", "DXA", "BMI", "WC"))
  
  # Filter out the intercept
  filtered_data = filtered_data %>%
    filter(Term != "(Intercept)")
  
  # Clean term labels by removing the prefix of the x_variable name
  filtered_data$term_clean = gsub(paste0(x_var), "", filtered_data$Term)
  
  # Define reference categories
  reference_categories = list(
    race = "White",
    age = "Under20",
    income = "Under35k",
    edu = "NoGrad",
    sex = "Male",
    smoke = "Never"
  )
  
  # Create reference data points
  ref_category = reference_categories[[x_var]]
  if (!is.null(ref_category)) {
    ref_data = data.frame()
    for (model_name in unique(filtered_data$y_variable)) {
      ref_row = data.frame(
        y_variable = model_name,
        x_variable = x_var,
        Term = paste0(x_var, ref_category),
        Estimate = 1,
        Lower = 1,
        Upper = 1,
        model = model_labels[model_name],
        term_clean = ref_category
      )
      ref_data = rbind(ref_data, ref_row)
    }
    
    # Combine with filtered data
    filtered_data = rbind(filtered_data, ref_data)
    
    # Get all unique term levels
    all_terms = unique(filtered_data$term_clean)
    # Remove the reference category from the list
    other_terms = setdiff(all_terms, ref_category)
    # Create a new level order with reference at the end (which will appear at the top in the plot)
    new_levels = c(ref_category, other_terms)
    
    # Set the factor levels in reverse order so reference appears at the top
    filtered_data$term_clean = factor(filtered_data$term_clean, 
                                       levels = rev(new_levels))
  }
  
  # Create the plot with alternating backgrounds that will definitely show up
  p = ggplot() +
    # First add the alternating background rectangles
    # This ensures they're behind everything else
    geom_rect(data = data.frame(
      y = seq_along(unique(filtered_data$term_clean)),
      term = unique(filtered_data$term_clean)
    ),
    aes(xmin = -Inf, xmax = Inf, 
        ymin = y - 0.5, ymax = y + 0.5,
        fill = factor(y %% 2)),
    alpha = 0.4, show.legend = FALSE) +
    scale_fill_manual(values = c("white", "gray90")) +
    
    # Then add the actual data points and error bars
    geom_point(data = filtered_data,
               aes(x = Estimate, y = term_clean, shape = model),
               position = position_dodge(width = 0.5), size = 3) +
    geom_errorbarh(data = filtered_data,
                   aes(x = Estimate, y = term_clean, xmin = Lower, xmax = Upper, linetype = model),
                   position = position_dodge(width = 0.5),
                   height = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "darkgray", alpha = 0.7) +
    
    # Apply styling
    scale_shape_manual(values = c(16, 17, 15, 18, 8)) +  # Different point shapes
    scale_linetype_manual(values = c(1, 2, 3, 4, 5)) +   # Different line types
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      # Increase font size for axis text
      axis.text = element_text(size = 12),
      # Increase font size for axis titles
      axis.title = element_text(face = "bold", size = 14),
      # Improve legend appearance
      legend.title = element_blank(),
      legend.text = element_text(size = 11),
      legend.key.size = unit(1, "cm"),
      # Enhance title appearance
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      # Add margin 
      plot.margin = margin(20, 20, 20, 20),
      # Remove panel border
      panel.border = element_blank(),
      # Add axis lines
      axis.line = element_line(color = "black")
    ) +
    labs(
      title = paste("In-sample estimates for", x_var),
      x = "Odds Ratio (with 95% CI)",
      y = NULL
    )
  
  # Improve the legend layout
  p = p + guides(shape = guide_legend(nrow = 1),
                  linetype = guide_legend(nrow = 1))
  
  # Optional: If you want to increase the plot size when saving
  # ggsave("plot.png", p, width = 10, height = 8, dpi = 300)
  
  return(p)
}

# Generate all plots
race_plot = create_dotplot(df, "race")
sex_plot = create_dotplot(df, "sex")
age_plot = create_dotplot(df, "age")
smoke_plot = create_dotplot(df, "smoke")
edu_plot = create_dotplot(df, "edu")
income_plot = create_dotplot(df, "income")

# Save individual plots
# save_high_res_plot(race_plot, "../results/race_plot.pdf", width = 8, height = 6)
# save_high_res_plot(sex_plot, "../results/sex_plot.pdf", width = 8, height = 4)
# save_high_res_plot(age_plot, "../results/age_plot.pdf", width = 8, height = 5)
# save_high_res_plot(smoke_plot, "../results/smoke_plot.pdf", width = 8, height = 5)
# save_high_res_plot(edu_plot, "../results/edu_plot.pdf", width = 8, height = 6)
# save_high_res_plot(income_plot, "../results/income_plot.pdf", width = 8, height = 5)

# Save all plots in a single file
all_plots = list(race_plot, sex_plot, age_plot, smoke_plot, edu_plot, income_plot)
save_all_plots_pdf(all_plots, "../results/obesity_model_comparison.pdf")