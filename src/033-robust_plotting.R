library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(viridis)
library(wordcloud)
library(viridisLite)
library(RColorBrewer)
library(tm)

# ----- CONTINUOUS OUTCOMES -----

continuous_df = read.csv("../results/robust/robust_continuous.csv")

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
# Create a function to generate a dotplot for a specific x_variable with continuous outcomes
create_dotplot_continuous = function(data, x_var) {
  # Filter data for the specified x_variable
  filtered_data = data %>%
    filter(x_variable == x_var)
  
  # Define label mapping for the y_variable (models)
  model_labels = c(
    "BMXBMI" = "BMI",
    "BMXWAIST" = "WC",
    "DXDTOPF" = "DXA"
    # No IPD models for continuous outcomes
  )
  
  # Add readable model labels
  filtered_data$model = model_labels[filtered_data$y_variable]
  
  # Set factor levels to ensure consistent ordering
  filtered_data$model = factor(filtered_data$model, 
                               levels = c("BMI", "WC", "DXA"))
  
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
  
  # Create reference data points at zero (not 1 as with odds ratios)
  ref_category = reference_categories[[x_var]]
  if (!is.null(ref_category)) {
    ref_data = data.frame()
    for (model_name in unique(filtered_data$y_variable)) {
      ref_row = data.frame(
        y_variable = model_name,
        x_variable = x_var,
        Term = paste0(x_var, ref_category),
        Estimate = 0,  # Reference is 0 for continuous outcomes
        Lower = 0,
        Upper = 0,
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
  
  # Create the plot with alternating backgrounds
  p = ggplot() +
    # First add the alternating background rectangles
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
               aes(x = Estimate, y = term_clean, shape = model, color = model),
               position = position_dodge(width = 0.8), size = 12) +
    geom_errorbarh(data = filtered_data,
                   aes(x = Estimate, y = term_clean, xmin = Lower, xmax = Upper, linetype = model, color = model),
                   position = position_dodge(width = 0.8),
                   height = 0.4, size = 3) +
    # Reference line at 0 (not 1) for continuous outcomes
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray", alpha = 0.7, size = 2) +
    
    # Apply styling
    scale_shape_manual(values = c(18, 8, 15)) +  # Adjusted for 3 models
    scale_linetype_manual(values = c(1, 2, 3)) +  # Adjusted for 3 models
    # Using same colors as in density plots
    scale_color_manual(values = c("darkred", "darkblue", "darkgreen")) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 55, face = "bold"),
      axis.text.x = element_text(size = 55, face = "bold"),
      axis.title = element_text(face = "bold", size = 55),
      legend.title = element_blank(),
      legend.text = element_text(size = 55, face = "bold"),
      legend.key.size = unit(3, "cm"),
      legend.key.width = unit(4, "cm"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 55),
      plot.margin = margin(60, 60, 60, 60),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 2)
    ) +
    labs(
      title = paste("Continuous Outcomes by", x_var),
      x = "Change in Outcome Units (with 95% CI)",  # Changed from "Odds Ratio" to reflect continuous outcomes
      y = NULL
    )
  
  # Improve the legend layout with larger spacing
  p = p + guides(
    shape = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 12)),
    linetype = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 4)),
    color = guide_legend(nrow = 1, byrow = TRUE)
  )
  
  return(p)
}

# Generate all plots using the continuous plot function
race_plot = create_dotplot_continuous(continuous_df, "race")
sex_plot = create_dotplot_continuous(continuous_df, "sex")
age_plot = create_dotplot_continuous(continuous_df, "age")
smoke_plot = create_dotplot_continuous(continuous_df, "smoke")
edu_plot = create_dotplot_continuous(continuous_df, "edu")
income_plot = create_dotplot_continuous(continuous_df, "income")

# Save all plots in a single file with much larger dimensions
all_plots = list(race_plot, sex_plot, age_plot, smoke_plot, edu_plot, income_plot)

save_all_plots_pdf(all_plots, "../results/robust/continuous_outcomes_comparison.pdf", width = 24, height = 28)

# ----- CONTINUOUS STANDARDIZED OUTCOMES -----

# Load the standardized results
standardized_df = read.csv("../results/robust/robust_standardized.csv")

# Create a function to generate a dotplot for standardized continuous outcomes
create_dotplot_standardized = function(data, x_var) {
  # Filter data for the specified x_variable
  filtered_data = data %>%
    filter(x_variable == x_var)
  
  # Define label mapping for the y_variable (models)
  model_labels = c(
    "BMXBMI_z" = "BMI",
    "BMXWAIST_z" = "WC", 
    "DXDTOPF_z" = "DXA"
  )
  
  # Add readable model labels
  filtered_data$model = model_labels[filtered_data$y_variable]
  
  # Set factor levels to ensure consistent ordering
  filtered_data$model = factor(filtered_data$model, 
                               levels = c("BMI", "WC", "DXA"))
  
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
  
  # Create reference data points at zero
  ref_category = reference_categories[[x_var]]
  if (!is.null(ref_category)) {
    ref_data = data.frame()
    for (model_name in unique(filtered_data$y_variable)) {
      ref_row = data.frame(
        y_variable = model_name,
        x_variable = x_var,
        Term = paste0(x_var, ref_category),
        Estimate = 0,  
        Lower = 0,
        Upper = 0,
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
  
  # Create the plot with alternating backgrounds
  p = ggplot() +
    # First add the alternating background rectangles
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
               aes(x = Estimate, y = term_clean, shape = model, color = model),
               position = position_dodge(width = 0.8), size = 12) +
    geom_errorbarh(data = filtered_data,
                   aes(x = Estimate, y = term_clean, xmin = Lower, xmax = Upper, linetype = model, color = model),
                   position = position_dodge(width = 0.8),
                   height = 0.4, size = 3) +
    # Reference line at 0 for standardized continuous outcomes
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray", alpha = 0.7, size = 2) +
    
    # Apply styling
    scale_shape_manual(values = c(18, 8, 15)) +
    scale_linetype_manual(values = c(1, 2, 3)) +
    # Using same colors as in density plots
    scale_color_manual(values = c("darkred", "darkblue", "darkgreen")) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 55, face = "bold"),
      axis.text.x = element_text(size = 55, face = "bold"),
      axis.title = element_text(face = "bold", size = 55),
      legend.title = element_blank(),
      legend.text = element_text(size = 55, face = "bold"),
      legend.key.size = unit(3, "cm"),
      legend.key.width = unit(4, "cm"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 55),
      plot.margin = margin(60, 60, 60, 60),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 2)
    ) +
    labs(
      title = paste("Continuous (standardized) Outcomes by", x_var),
      x = "Change in Standard Deviations (with 95% CI)",
      y = NULL
    )
  
  # Improve the legend layout with larger spacing
  p = p + guides(
    shape = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 12)),
    linetype = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 4)),
    color = guide_legend(nrow = 1, byrow = TRUE)
  )
  
  return(p)
}

# Generate all plots using the standardized plot function
race_plot = create_dotplot_standardized(standardized_df, "race")
sex_plot = create_dotplot_standardized(standardized_df, "sex")
age_plot = create_dotplot_standardized(standardized_df, "age")
smoke_plot = create_dotplot_standardized(standardized_df, "smoke")
edu_plot = create_dotplot_standardized(standardized_df, "edu")
income_plot = create_dotplot_standardized(standardized_df, "income")

# Save all plots in a single file
all_plots = list(race_plot, sex_plot, age_plot, smoke_plot, edu_plot, income_plot)

save_all_plots_pdf(all_plots, "../results/robust/standardized_outcomes_comparison.pdf", width = 24, height = 28)

# # Save individual plots
# for(plot_name in c("race", "sex", "age", "smoke", "edu", "income")) {
#   file_path = paste0("../results/standardized_", plot_name, ".pdf")
#   
#   ggsave(
#     filename = file_path,
#     plot = get(paste0(plot_name, "_plot")),
#     width = 24,
#     height = 16,
#     dpi = 600,
#     device = "pdf"
#   )
#   
#   cat("Saved", file_path, "\n")
# }

# ----- ALT CUTOFF PLOTS -----

df = read.csv("../results/robust/alt_cutoff_results.csv")

# Create a function to generate a dotplot for a specific x_variable
create_dotplot = function(data, x_var) {
  # Filter data for the specified x_variable
  filtered_data = data %>%
    filter(x_variable == x_var)
  
  # Define label mapping for the y_variable (models)
  model_labels = c(
    "obese_dxa" = "DXA",
    "obese_bmi" = "BMI Standard",
    "obese_bmi_1998" = "BMI 1998",
    "obese_bmi_eth" = "BMI Ethnic"
  )
  
  # Add readable model labels
  filtered_data$model = model_labels[filtered_data$y_variable]
  
  # Set factor levels to ensure consistent ordering
  filtered_data$model = factor(filtered_data$model, 
                               levels = c("BMI Standard", "BMI 1998", "BMI Ethnic", "DXA"))
  
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
  
  # Create the plot with alternating backgrounds
  p = ggplot() +
    # First add the alternating background rectangles
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
               aes(x = Estimate, y = term_clean, shape = model, color = model),
               position = position_dodge(width = 0.8), size = 12) +
    geom_errorbarh(data = filtered_data,
                   aes(x = Estimate, y = term_clean, xmin = Lower, xmax = Upper, linetype = model, color = model),
                   position = position_dodge(width = 0.8),
                   height = 0.4, size = 3) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "darkgray", alpha = 0.7, size = 2) +
    
    # Apply styling for 3 models (not 5)
    scale_shape_manual(values = c(16, 17, 15, 8)) +
    scale_linetype_manual(values = c(1, 2, 3, 5)) +
    # Add color scale
    scale_color_manual(values = c("darkred", "darkblue", "darkgreen", "black")) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 55, face = "bold"),
      axis.text.x = element_text(size = 55, face = "bold"),
      axis.title = element_text(face = "bold", size = 55),
      legend.title = element_blank(),
      legend.text = element_text(size = 55, face = "bold"),
      legend.key.size = unit(3, "cm"),
      legend.key.width = unit(4, "cm"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 55),
      plot.margin = margin(60, 60, 60, 60),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 2)
    ) +
    labs(
      title = paste("Alternative BMI Cutoffs for Obesity by ", x_var),
      x = "Odds Ratio (with 95% CI)",
      y = NULL
    )
  
  # Improve the legend layout with larger spacing
  p = p + guides(
    shape = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 12)),
    linetype = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 4)),
    color = guide_legend(nrow = 1, byrow = TRUE)
  )
  
  return(p)
}

# Generate all plots using the updated function
race_plot = create_dotplot(df, "race")
sex_plot = create_dotplot(df, "sex")
age_plot = create_dotplot(df, "age")
smoke_plot = create_dotplot(df, "smoke")
edu_plot = create_dotplot(df, "edu")
income_plot = create_dotplot(df, "income")

# Save all plots in a single file
all_plots = list(race_plot, sex_plot, age_plot, smoke_plot, edu_plot, income_plot)

save_all_plots_pdf(all_plots, "../results/robust/alt_cutoffs.pdf", width = 24, height = 28)

# ----- MISC PLOTS -----

# BMI Chart
# Create a data frame with height-weight combinations
heights_m <- seq(1.5, 2.0, length.out = 300)  # Increased resolution for smoother transitions
weights_kg <- seq(40, 120, length.out = 300)  # Increased resolution for smoother transitions
# Create a grid of height and weight combinations
grid <- expand.grid(height_m = heights_m, weight_kg = weights_kg)
# Calculate BMI for each height-weight combination
grid$bmi <- grid$weight_kg / (grid$height_m^2)
# Classify BMI into categories
grid$category <- cut(grid$bmi, 
                     breaks = c(0, 18.5, 25, 30, Inf),
                     labels = c("Underweight", "Normal", "Overweight", "Obese"))
# Create the BMI chart with filled colors and alpha transparency
p <- ggplot(grid, aes(x = height_m, y = weight_kg, fill = category)) +
  geom_raster(interpolate = TRUE, alpha = 0.7) +  # Added alpha = 0.7 to reduce color intensity
  scale_fill_viridis_d(option = "viridis", alpha = 0.7) +  # Apply alpha to the color scale
  scale_x_continuous(breaks = c(1.6, 1.7, 1.8, 1.9, 2.0)) +
  scale_y_continuous(breaks = seq(40, 120, by = 10)) + 
  labs(x = "Height [m]",
       y = "Weight [kg]",
       fill = "BMI Category") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title = element_text(size = 40),
    axis.text = element_text(size = 40),  # Much larger axis text
    legend.position = c(0.05, 0.95),  # Position legend in upper left
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),
    legend.key.size = unit(0.7, "cm"),  # Larger legend keys
    legend.title = element_text(size = 20),  # Larger legend text
    legend.text = element_text(size = 40),  # Larger legend text
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    panel.border = element_rect(color = NA, fill = NA),
    plot.margin = margin(15, 20, 15, 15)  # Increased margins to accommodate labels while keeping full color fill
  )
# Add white grid lines at nice intervals and smoother boundaries between categories
p <- p + 
  geom_hline(yintercept = seq(40, 120, by = 10), color = "white", size = 0.5, alpha = 0.7) +
  geom_vline(xintercept = seq(1.5, 2.0, by = 0.1), color = "white", size = 0.5, alpha = 0.7) +
  # Use stat_contour instead of geom_contour for smoother boundaries
  stat_contour(aes(z = as.numeric(category)), 
               color = "white", 
               size = 0.3,     # Reduced size for more subtle boundaries
               alpha = 0.7,    # Added transparency to the contour lines
               bins = 3)       # Explicitly set number of bins to match category breaks
# Adjust plot boundaries for better appearance - using expand to create space for axis labels
# Use expand = FALSE to make colors fill entire area, but adjust clip to ensure labels are visible
p <- p + coord_cartesian(expand = FALSE, clip = "off")
# Print the plot
print(p)
# Save the plot as a PDF file
ggsave("../plots/bmi_chart.pdf", p, width = 10, height = 8, dpi = 300)

# ----- WORD CLOUD ------

words = c(
  # Most common measures
  rep('Body Mass Index', 60),
  rep('BMI', 45),
  rep('Waist Circumference', 40),
  rep('Body Fat Percentage', 35),
  rep('Waist-to-Hip Ratio', 30),
  
  # Secondary common measures
  rep('DEXA Scan', 25),
  rep('DXA', 18),
  rep('Dual-Energy X-ray Absorptiometry', 15),
  rep('Skinfold Thickness', 22),
  rep('Bioelectrical Impedance', 20),
  rep('BIA', 12),
  
  # Clinical imaging methods
  rep('MRI', 14),
  rep('CT Scan', 13),
  rep('Ultrasound', 10),
  
  # Research methods
  rep('Hydrostatic Weighing', 8),
  rep('Underwater Weighing', 6),
  rep('Air Displacement Plethysmography', 9),
  rep('Bod Pod', 7),
  
  # Specialized adiposity indices
  rep('Visceral Adipose Tissue', 16),
  rep('VAT', 10),
  rep('Subcutaneous Adipose Tissue', 12),
  rep('SAT', 8),
  rep('Waist-to-Height Ratio', 18),
  rep('Body Adiposity Index', 15),
  rep('BAI', 7),
  rep('A Body Shape Index', 9),
  rep('ABSI', 5),
  rep('Relative Fat Mass', 11),
  rep('RFM', 6),
  
  # Less common measures
  rep('Anthropometric Measurements', 12),
  rep('Neck Circumference', 10),
  rep('3D Body Scanning', 8),
  rep('Sagittal Abdominal Diameter', 6),
  rep('Near-Infrared Interactance', 4),
  rep('Electrical Conductivity', 3),
  rep('TOBEC', 2),
  
  # Additional measures for comprehensiveness
  rep('Fat Mass Index', 7),
  rep('FMI', 4),
  rep('Fat-Free Mass Index', 6),
  rep('FFMI', 3),
  rep('Conicity Index', 5),
  rep('Intra-abdominal Fat', 9),
  rep('Total Body Water', 7),
  rep('Multi-frequency BIA', 5),
  rep('Digital Image Analysis', 4),
  rep('Deuterium Dilution', 3),
  rep('Trunk-to-Leg Fat Ratio', 4),
  rep('Ponderal Index', 3),
  rep('Corpulence Index', 2),
  rep('Obesity Index', 4)
)

# Count word frequencies
word_freq <- table(words)
word_df <- data.frame(word = names(word_freq), freq = as.numeric(word_freq))

# Create directory if it doesn't exist
dir.create("../plots", showWarnings = FALSE)

# Set up a high-resolution PNG device for output
png("../plots/adiposity_wordcloud1.png", width = 3000, height = 3000, res = 300)

# Create a color palette with viridis - creating varying opacity by modifying colors
n_colors <- 200  # More colors for smooth gradient
base_viridis <- viridis(100, option = "viridis")

# Create varied opacity by manipulating colors
viridis_palette <- c()
for(i in 1:100) {
  # Add the base color
  viridis_palette <- c(viridis_palette, base_viridis[i])
  # Add a slightly more transparent version
  col_rgb <- col2rgb(base_viridis[i])/255
  viridis_palette <- c(viridis_palette, rgb(col_rgb[1], col_rgb[2], col_rgb[3], 0.7))
}

# Create the wordcloud with modified parameters for more cohesive appearance
set.seed(1234)  # For reproducibility
wordcloud(
  words = word_df$word, 
  freq = word_df$freq, 
  min.freq = 1,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.4,        # More rotation for variety
  colors = viridis_palette,
  scale = c(4, 0.2),    # More dramatic scaling between sizes
  random.color = TRUE,  # Random color assignment helps with variety
  ordered.colors = FALSE,
  fixed.asp = TRUE,     # Maintain aspect ratio
  use.r.layout = FALSE, # Use the C++ layout algorithm which is more compact
  family = "sans"       # Font family
)

# Close the PDF device
dev.off()








