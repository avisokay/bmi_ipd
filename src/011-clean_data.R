library(data.table)
library(dplyr)
library(tidyr)
library(survey)
library(ggplot2)

# ----- DATA CLEANING -----

# load raw data
df = read.csv("../data/nhanes_raw.csv")

# ----- 10 YEAR MEC WEIGHTS -----

# First construct 10-year weights for 2011-2021
# https://wwwn.cdc.gov/nchs/nhanes/tutorials/weighting.aspx
weight_years = c(2011, 2013, 2015, 2017, 2021)

df$WTMEC10YR = NA  # Initialize with NA
df$WTMEC10YR = ifelse(df$year %in% weight_years, 1/5 * df$WTMEC2YR, NA)

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

# save output as .rds file
saveRDS(df, "../data/nhanes_cleaned.rds")


