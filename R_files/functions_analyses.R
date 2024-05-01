# Function to create linear model formula
create_lm_formula <- function(dependent_var, independent_vars) {
  # Create the formula string
  # Paste together the dependent variable, the tilde (~), and the independent variables joined by "+"
  formula_str <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "))
  
  # Convert the string to a formula object
  formula_obj <- as.formula(formula_str)
  
  return(formula_obj)
}


# Function to create and save Q-Q plots
create_qq_plot <- function(model, index) {
  # Extract residuals from the model
  residuals <- resid(model)
  
  # Create a Q-Q plot
  qq_plot <- ggplot() + 
    stat_qq(aes(sample = residuals)) + 
    geom_abline(slope = 1, intercept = 0) + 
    theme_grey() +
    ggtitle(paste("Q-Q Plot of Model Residuals - Model", index)) + 
    xlab("Theoretical Quantiles") + 
    ylab("Sample Quantiles")
  
  # Display the Q-Q plot
  print(qq_plot)
  
  # Save the Q-Q plot
  ggsave(filename = paste("outputs/analyses/qq_plot_model_", index, ".png", sep = ""), 
         plot = qq_plot, width = 20, height = 16, units = "cm")
}

# Function to create general linear model formula
create_glm_formula <- function(dependent_var, independent_vars) {
  # Create the formula string
  # Paste together the dependent variable, the tilde (~), and the independent variables joined by "+"
  formula_str <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "))
  
  # Convert the string to a formula object
  formula_obj <- as.formula(formula_str)
  
  return(formula_obj)
}

create_glm_df <- function(df, dep_var, indep_vars) {
  # Combine the dependent variable with the independent variables into a single vector
  glm_vars <- c(dep_var, indep_vars)
  
  # Select the relevant variables and drop rows with any missing values
  glm_df <- df %>%
    select(all_of(glm_vars)) %>%
    drop_na()
  
  return(glm_df)
}


# Function to create calibration plot
create_calibration_plot <- function(data, dep_var, output_file) {
  # Convert dep_var to actual data column in the dataframe
  data$dep_var_actual <- data[[dep_var]]
  
  # Create probability bins and calculate metrics
  calibration_data <- data %>%
    mutate(prob_bin = cut(predicted_prob, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE, right = FALSE)) %>%
    group_by(prob_bin) %>%
    summarise(
      Avg_Predicted = mean(predicted_prob),
      Avg_Actual = mean(dep_var_actual, na.rm = TRUE),  # Ensure numeric calculation
      N = n(),
      SE = sqrt(Avg_Actual * (1 - Avg_Actual) / N),
      Lower_CI = Avg_Actual - 1.96 * SE,
      Upper_CI = Avg_Actual + 1.96 * SE
    ) %>%
    ggplot(aes(x = Avg_Predicted, y = Avg_Actual)) +
    geom_point() +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.02) +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(x = "Average Predicted Probability", y = "Average Observed Outcome", title = "Calibration Plot") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    theme_gray()
  
  # Save the plot
  ggsave(filename = output_file, plot = calibration_data, width = 20, height = 16, units = "cm")
}
}