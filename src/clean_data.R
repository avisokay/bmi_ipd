library(data.table)
library(dplyr)
library(tidyr)
library(survey)
library(ggplot2)

# ----- DATA CLEANING -----

# load raw data
df = read.csv("../data/nhanes_raw.csv")

# ----- 1. OBESITY -----
# construct obesity variables based on BMI, WC, DXA
df$obese_bmi = ifelse(df$BMXBMI>30, 1, 0) # bmi score of 30 for both
df$obese_wc = ifelse(df$RIAGENDR == 1 & df$BMXWAIST > 102, 1, # 102 cm for males
                      ifelse(df$RIAGENDR == 2 & df$BMXWAIST > 88, 1, 0)) # 88 cm for females
df$obese_dxa = ifelse(df$RIAGENDR == 1 & df$DXDTOPF > 29, 1, # 30% for males
                      ifelse(df$RIAGENDR == 2 & df$DXDTOPF > 41, 1, 0)) # 42% for females

# ----- 2. RACE/ETHNICITY -----

# Create a new race variable based on RIDRETH3 as character
df$race = NA_character_  # Initialize with NA character type

# Recode based on the values
df$race[df$RIDRETH3 %in% c(1,2)] = "Hispanic"
df$race[df$RIDRETH3 == 3] = "White"
df$race[df$RIDRETH3 == 4] = "Black"
df$race[df$RIDRETH3 == 5] = "Asian"

df$race = factor(df$race, levels = c("White", "Black", "Asian", "Hispanic"))

# ----- 3. SEX -----

# Create a new race variable based on RIAGENDR as character
df$sex = NA_character_  # Initialize with NA character type

# Recode based on the values
df$sex[df$RIAGENDR == 1] = "Male"
df$sex[df$RIAGENDR == 2] = "Female"

df$sex = factor(df$sex, levels = c("Male", "Female"))

# ----- 4. AGE -----
df$age = NA_character_

# Recode with one-word names
df$age[df$RIDAGEYR < 20] = "Under20"
df$age[df$RIDAGEYR %in% 20:39] = "20_39"
df$age[df$RIDAGEYR %in% 40:59] = "40_59"
df$age[df$RIDAGEYR > 59] = "60+"

df$age = factor(df$age, levels = c("Under20", "20_39", "40_59", "60+"))

# ----- 5. SMOKING -----
# smoking categories
df$smoke = NA_character_

df$smoke[df$SMQ040 %in% c(1,2)] = "Smoker"
df$smoke[df$SMQ621 %in% c(1, 2)] = "Never"
df$smoke[df$SMQ050Q %in% c(1:193)] = "Past"

# set factors
df$smoke = factor(df$smoke, levels = c("Never", "Past", "Smoker"))

# ----- 6. EDUCATION -----
df$edu = NA_character_

# Recode with one-word names
df$edu[df$DMDEDUC2 %in% c(1,2)] = "NoGrad"
df$edu[df$DMDEDUC2 == 3] = "HighSchool"
df$edu[df$DMDEDUC2 == 4] = "SomeCollege"
df$edu[df$DMDEDUC2 == 5] = "CollegePlus"

# set factors
df$edu = factor(df$edu, levels = c("NoGrad", "HighSchool", "SomeCollege",
                                    "CollegePlus"))

# ----- 7. INCOME -----
df$income = NA_character_

# Recode with simplified names
df$income[df$INDHHIN2 %in% 1:6] = "Under35k"
df$income[df$INDHHIN2 %in% 7:10] = "35k_75k"
df$income[df$INDHHIN2 %in% 14:15] = "75k+"

# Convert to factor to maintain income level order
df$income = factor(df$income, 
                    levels = c("Under35k", "35k_75k", "75k+"))

# ----- SURVEY WEIGHTS ------
# 
# # First construct 8-year weights for 2011-2017
# # https://wwwn.cdc.gov/nchs/nhanes/tutorials/weighting.aspx
# df$MEC8YR = NA  # Initialize with NA
# df$MEC8YR[df$SDDSRVYR %in% c(7, 8, 9, 10)] = df$WTMEC2YR[df$SDDSRVYR %in% c(7, 8, 9, 10)] / 4
# 
# # Create a survey design object with the new 8-year weights
# nhanes_design = svydesign(
#   id = ~SDMVPSU,           # Masked variance pseudo-Primary Sampling Unit (PSU)
#   strata = ~SDMVSTRA,      # Masked variance stratification variable
#   weights = ~MEC8YR,       # Your new 8-year weights
#   nest = TRUE,             # Nested PSUs within strata
#   data = df
# )
# 
# # Now you can use this design object for analyses with your transformed variables
# # For example, to get weighted proportions for your new race variable:
# race_prop = svymean(~race, nhanes_design, na.rm = TRUE)
# print(race_prop)
# 
# # For your smoking variable:
# smoke_prop = svymean(~smoke, nhanes_design, na.rm = TRUE)
# print(smoke_prop)
# 
# # For cross-tabulations, e.g., smoking by race:
# svyby(~smoke, ~race, nhanes_design, svymean, na.rm = TRUE)
# 
# # Create a survey design object
# nhanes_design = svydesign(
#   id = ~1,  # Assuming no clustering
#   strata = NULL,  # Assuming no stratification
#   weights = ~WTMEC2YR,  # Use the 2-year survey weights
#   data = df,
#   nest = TRUE
# )

# save output as .rds file
saveRDS(df, "../data/nhanes_cleaned.rds")



