library(data.table)
library(nhanesA)
library(dplyr)
library(progress)

# ----- NHANES DATA PREPARATION -----
## This script will download Demographic (DEMO) and
## Body Measures (BMX) data from NHANES for survey
## years 1999-2023 and DXA data for survey years 1999-2005
## and 2011-2017. The output is saved as a .csv to the ../data folder.

# Function to load NHANES data and add a year column
load_nhanes_data = function(prefix, suffix, year, var) {
  if (var == "DXX" && year < 2011) {
    data = nhanesDXA(year)
  } else if (suffix == "") {
    data = nhanes(var)
  } else {
    data = nhanes(paste0(prefix, suffix))
  }
  data$year = year
  return(data)
}

## ----- DEMOGRAPHIC 1999-2023 ----- 
demo_years = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2021)
demo_suffixes = c("", LETTERS[2:10], LETTERS[12])

# Create a progress bar
pb = progress_bar$new(total = length(demo_years), format = "Loading demo data [:bar] :percent :elapsed")

for (i in seq_along(demo_years)) {
  year = demo_years[i]
  suffix = demo_suffixes[i]
  tryCatch({
    assign(paste0("demo_", suffix), load_nhanes_data("DEMO_", suffix, year, var="DEMO"))
    pb$tick()
  }, error = function(e) {
    message(sprintf("Error loading data for year %s: %s", year, e$message))
  })
}

# Combine DEMO data

# Function to convert all columns to character when shared columns are different types
convert_to_numeric = function(df) {
  df[] = lapply(df, as.numeric)
  return(df)
}

# Get the list of dataframe names
df_list = ls(pattern = "^demo")

# Convert the list of names to a list of dataframes
dfs = lapply(df_list, function(x) {
  obj = get(x)
  if (is.data.frame(obj)) return(obj) else return(as.data.frame(obj))
}
)

# Apply the conversion function to each dataframe
dfs = lapply(dfs, convert_to_numeric)

# Combine the dataframes
combined_demo = bind_rows(dfs)

# ----- BMX 1999-2023 ----- 
bmx_years = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2021)
bmx_suffixes = c("", LETTERS[2:10], LETTERS[12])

# Create a progress bar
pb = progress_bar$new(total = length(bmx_years), format = "Loading BMX data [:bar] :percent :elapsed")

for (i in seq_along(bmx_years)) {
  year = bmx_years[i]
  suffix = bmx_suffixes[i]
  tryCatch({
    assign(paste0("bmx_", suffix), load_nhanes_data("BMX_", suffix, year, var="BMX"))
    pb$tick()
  }, error = function(e) {
    message(sprintf("Error loading data for year %s: %s", year, e$message))
  })
}

# Combine BMX data 

# Get the list of dataframe names
df_list = ls(pattern = "^bmx")

# Convert the list of names to a list of dataframes
dfs = lapply(df_list, function(x) {
  obj = get(x)
  if (is.data.frame(obj)) return(obj) else return(as.data.frame(obj))
}
)

# Apply the conversion function to each dataframe
dfs = lapply(dfs, convert_to_numeric)

# Combine the dataframes
combined_bmx = bind_rows(dfs)

# ----- DXA 1999-2017 ----- 
dxa_years = c(2011, 2013, 2015, 2017)
dxa_suffixes = LETTERS[7:10]

# Create a progress bar
pb = progress_bar$new(total = length(dxa_years), format = "Loading DXA data [:bar] :percent :elapsed")

for (i in seq_along(dxa_years)) {
  year = dxa_years[i]
  suffix = dxa_suffixes[i]
  tryCatch({
    assign(paste0("dxx_", suffix), load_nhanes_data("DXX_", suffix, year, var="DXX"))
    pb$tick()
  }, error = function(e) {
    message(sprintf("Error loading data for year %s: %s", year, e$message))
  })
}

# Combine DXX data

# Get the list of dataframe names
df_list = ls(pattern = "^dxx")

# Convert the list of names to a list of dataframes
dfs = lapply(df_list, function(x) {
  obj = get(x)
  if (is.data.frame(obj)) return(obj) else return(as.data.frame(obj))
}
)

# Apply the conversion function to each dataframe
dfs = lapply(dfs, convert_to_numeric)

# Combine the dataframes
combined_dxx = bind_rows(dfs)

# ----- SMQ 1999-2023 ----- 
# cigarette smoking
smoke_years = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2021)
smoke_suffixes = c("", LETTERS[2:10], LETTERS[12])

# Create a progress bar
pb = progress_bar$new(total = length(smoke_years), format = "Loading demo data [:bar] :percent :elapsed")

for (i in seq_along(smoke_years)) {
  year = smoke_years[i]
  suffix = smoke_suffixes[i]
  tryCatch({
    assign(paste0("smq_", suffix), load_nhanes_data("SMQ_", suffix, year, var="DEMO"))
    pb$tick()
  }, error = function(e) {
    message(sprintf("Error loading data for year %s: %s", year, e$message))
  })
}

# Combine SMQ data

# Get the list of dataframe names
df_list = ls(pattern = "^smq")

# Convert the list of names to a list of dataframes
dfs = lapply(df_list, function(x) {
  obj = get(x)
  if (is.data.frame(obj)) return(obj) else return(as.data.frame(obj))
}
)

# Apply the conversion function to each dataframe
dfs = lapply(dfs, convert_to_numeric)

# Combine the dataframes
combined_smq = bind_rows(dfs)

# Create a progress bar
pb = progress_bar$new(total = length(smoke_years), format = "Loading demo data [:bar] :percent :elapsed")

for (i in seq_along(smoke_years)) {
  year = smoke_years[i]
  suffix = smoke_suffixes[i]
  tryCatch({
    assign(paste0("smq_", suffix), load_nhanes_data("SMQ_", suffix, year, var="DEMO"))
    pb$tick()
  }, error = function(e) {
    message(sprintf("Error loading data for year %s: %s", year, e$message))
  })
}

# Combine SMQ data

# Get the list of dataframe names
df_list = ls(pattern = "^smq")

# Convert the list of names to a list of dataframes
dfs = lapply(df_list, function(x) {
  obj = get(x)
  if (is.data.frame(obj)) return(obj) else return(as.data.frame(obj))
}
)

# Apply the conversion function to each dataframe
dfs = lapply(dfs, convert_to_numeric)

# Combine the dataframes
combined_smq = bind_rows(dfs)

# ---- MERGE ALL DATA ----

# Identify which dataframe has 'year' and rename it uniquely
names(combined_demo)[names(combined_demo) == "year"] = "year_demo"
names(combined_bmx)[names(combined_bmx) == "year"] = "year_bmx"
names(combined_dxx)[names(combined_dxx) == "year"] = "year_dxx"
names(combined_smq)[names(combined_smq) == "year"] = "year_smq"

# Then merge as before
merged_df1 = merge(combined_demo, 
                    combined_bmx[, c("SEQN", "BMXBMI", "BMXWAIST", "year_bmx")], 
                    by = "SEQN", all = TRUE)
merged_df2 = merge(merged_df1, 
                    combined_dxx[, c("SEQN", "DXDTOPF", "year_dxx")], 
                    by = "SEQN", all = TRUE)
final_combined = merge(merged_df2,
                        combined_smq[, c("SEQN", "SMQ040", "SMQ621", "SMQ050Q", "year_smq")], 
                        by = "SEQN", all = TRUE)

# After completing the merge with the renamed year columns:
final_combined$year = NA  # Initialize new year column

# Fill with values from each year column in order of priority (adjust priority as needed)
# This prioritizes demo, then bmx, then dxx, then smq
final_combined$year = final_combined$year_demo
final_combined$year[is.na(final_combined$year)] = final_combined$year_bmx[is.na(final_combined$year)]
final_combined$year[is.na(final_combined$year)] = final_combined$year_smq[is.na(final_combined$year)]
final_combined$year[is.na(final_combined$year)] = final_combined$year_dxx[is.na(final_combined$year)]

# Check rows with non-NA values for BMXBMI by year
bmi_not_na = final_combined %>%
  filter(!is.na(BMXBMI)) %>%
  group_by(year) %>%
  summarise(count = n())

# Check rows with non-NA values for DXDTOPF by year
dxd_not_na = final_combined %>%
  filter(!is.na(DXDTOPF)) %>%
  group_by(year) %>%
  summarise(count = n())

# Check rows with non-NA values for BMXWAIST by year
wc_not_na = final_combined %>%
  filter(!is.na(BMXWAIST)) %>%
  group_by(year) %>%
  summarise(count = n())

# Check rows with non-NA values for both BMXBMI and DXDTOPF by year
all_not_na = final_combined %>%
  filter(!is.na(BMXBMI) & !is.na(DXDTOPF) & !is.na(BMXWAIST)) %>%
  group_by(year) %>%
  summarise(count = n())

# Print the results
print("Rows with non-NA values for BMXBMI by year:")
print(bmi_not_na)

print("Rows with non-NA values for DXDTOPF by year:")
print(dxd_not_na)

print("Rows with non-NA values for BMXWAIST by year:")
print(wc_not_na)

print("Rows with non-NA values for BMXBMI, BMXWAIST and DXDTOPF by year:")
print(all_not_na)

# remove rows where BMI or WAIST are missing
final_combined = final_combined[!is.na(final_combined$BMXBMI) &!is.na(final_combined$BMXWAIST), ]

# output csv
write.csv(x = final_combined, file = "../data/nhanes_raw.csv", row.names = FALSE)

