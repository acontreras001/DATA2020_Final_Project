
library(tidyverse)



# Find columns that only have missing data
find_missing_data_columns <- function(df) {
  columns_with_only_missing_data <- names(df)[colSums(!is.na(df)) == 0]
  return(columns_with_only_missing_data)
}

# Find columns where the entire column is a homogeneous value
find_homogeneous_value_columns <- function(df) {
  
  columns_with_homogeneous_value <- names(df)[apply(df, 2, function(x) length(unique(x)) == 1)]
  
  return(columns_with_homogeneous_value)
}

# Remove columns that only have missing data
remove_missing_data_columns <- function(df) {
  columns_with_only_missing_data <- names(df)[colSums(!is.na(df)) == 0]
  
  df_filtered <- df %>% select(-all_of(columns_with_only_missing_data))

  return(df_filtered)
}

# Remove columns where the entire column is a homogeneous value
remove_homogeneous_value_columns <- function(df) {
  columns_with_homogeneous_value <- names(df)[apply(df, 2, function(x) length(unique(x)) == 1)]
  
  df_filtered <- df %>% select(-all_of(columns_with_homogeneous_value))
  
  return(df_filtered)
}

###

# Normalization - set range between 0 and 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Standardization - mean 0, standard deviation 1
standardize <- function(x) {
  return ((x - mean(x)) / sd(x))
}

# Logarithmic Transformation
log_transform <- function(x) {
  return (log(x))
}

# Min-Max Scaling
min_max_scaling <- function(x, min_range = 0, max_range = 1) {
  return ((x - min(x)) / (max(x) - min(x))) * (max_range - min_range) + min_range
}

###








