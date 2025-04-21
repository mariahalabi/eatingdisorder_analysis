
# Author: Maria
# Purpose: Pre-processing for Dissertation 
# Version: Script Pre-processing 1

# ========================= Load packages ====================
library(psych)
library(pastecs)
library(dplyr)
library(tidyverse)

# ========================= Define functions ====================
# Create an empty dataframe to track participant counts
participant_tracking <- data.frame(
  Step = character(),
  Cohort = character(),
  Variable = character(),
  Unique_Participants = integer(),
  stringsAsFactors = FALSE
)

## Row sums
my_rowSums <- function(x) {
  if (is.data.frame(x)) x <- as.matrix(x)
  z <- base::rowSums(x, na.rm = TRUE)
  z[!base::rowSums(!is.na(x))] <- NA
  z
}

## Duplicates
remove_duplicates <- function(data, ID_col, date_col = "endDate") {
  
  # require dependencies
  require(sjlabelled)
  
  # not in operator
  `%nin%` <- Negate(`%in%`)
  
  # Error for incorrect ID_col
  if (ID_col %nin% colnames(data)){
    stop("ID_col is incorrectly specified")
  }
  
  # Error for incorrect data_col
  if (date_col %nin% colnames(data)){
    stop("date_col is incorrectly specified")
  }
  
  # take labels for data
  data_labels <- get_labels(data, value = TRUE)
  data_label <- get_label(data)
  
  # Remove rows with NA in ID_col
  data <- data[!is.na(data[[ID_col]]), ]
  
  # Get the first few duplicated row indices (excluding the last)
  first <- which(duplicated(data[[ID_col]], fromLast = TRUE))
  
  # Get the last few duplicated row indices (excluding the first)
  second <- which(duplicated(data[[ID_col]]))
  
  # Get all the duplicates indices
  dupes <- union(first, second)
  
  # No duplicates, return straight away
  if (is_empty(dupes)) {
    out <- data
  }
  else{
    # Process all the duplicated rows
    data_dupe <- data[dupes, ] %>%
      # Process each duplicated ID separately
      split(.[[ID_col]]) %>%
      # Create one row for each duplicated ID
      map_df(function(ID_data) {
        map_df(ID_data[!colnames(ID_data) %in% date_col], function(col) {
          if (sum(!is.na(col)) == 1) {
            # Use the non-NA value
            col[!is.na(col)]
          } else {
            # Use value with latest EndDate
            # Note there could be multiple latest EndDate
            col[ID_data[[date_col]] == max(ID_data[[date_col]])][1]
          }
        })
      }) %>%
      bind_rows()
    
    # Remove the original duplicates and bind with the modified version
    out <- bind_rows(data[-dupes, ], data_dupe)
  }
  
  # add labels to dataframe
  for (i in 1:ncol(out)) {
    labels <- data_labels[[i]]
    if (!is.null(names(labels))) {
      out[i] <- set_labels(out[i], labels = data_labels[[i]])
      out[i] <- set_label(out[i], data_label[i])
    }
  }
  
  # return de-duplicated dataframe
  return(out)
}
# ========================= Set wd ====================
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-King'sCollegeLondon/MT-BioResource data - Agnieszka_Gidziela - Agnieszka_Gidziela")
getwd()

# ================================================== COPING data ====================
                                      # ========== COHORT 1: GLAD  ==========
# ================= Clean demographics (no edu) ==========
# ========== Read clean dem data and inspect ==========
coping_glad_dem_age <- readRDS("data/demographics - coping_glad_edgi_nbr/age_coping_glad_edgi_nbr_clean.rds")
coping_glad_dem_ethnicity <- readRDS("data/demographics - glad_edgi/ethnicity_glad_edgi_clean.rds")
coping_glad_dem_anthropo <- readRDS("data/demographics - coping_glad_edgi_nbr/signup_bmi_height_weight_coping_glad_edgi_nbr_clean.rds")
coping_glad_dem_gender <- readRDS("data/demographics - coping_glad_edgi_nbr/sex_gender_sexuality_coping_glad_edgi_nbr_clean.rds")

names(coping_glad_dem_age)
names(coping_glad_dem_ethnicity)
names(coping_glad_dem_anthropo)
names(coping_glad_dem_gender)

## Cohort1. Demographic Measures
#Participants provided information on demographic variables, including age, sex, gender identity, ethnicity, and anthropometric measures (e.g., height, weight, BMI).
#Apparatus: Self-reported questionnaires completed online via the study platform (?specific platform used, e.g., Qualtrics, REDCap?).
#Details:
#Age: Age was calculated based on self-reported date of birth.
#Gender Identity: Participants were asked to self-identify their gender using predefined options and an open-text field for additional responses.
#Ethnicity: Participants selected their ethnic origin from a predefined list aligned with government census categories (specify the exact options provided, if possible).
#Anthropometric Measures: Participants self-reported height and weight, from which BMI was calculated using the standard formula: BMI = weight (kg) / height (m)^2.

coping_glad_dem_age <- coping_glad_dem_age[coping_glad_dem_age$sample == "GLAD",]
coping_glad_dem_ethnicity <- coping_glad_dem_ethnicity[coping_glad_dem_ethnicity$sample == "GLAD",]
coping_glad_dem_anthropo <- coping_glad_dem_anthropo[coping_glad_dem_anthropo$sample == "GLAD",]
coping_glad_dem_gender <- coping_glad_dem_gender[coping_glad_dem_gender$sample == "GLAD",]

## Items
coping_glad_dem_age <- coping_glad_dem_age[, c("ID", 
                                               "dem.dob_age_cop")]

coping_glad_dem_ethnicity <- coping_glad_dem_ethnicity[, c("ID",
                                                           "dem.what_is_your_ethnic_origin")]

coping_glad_dem_anthropo <- coping_glad_dem_anthropo[, c("ID", 
                                                         "dem.height_signup_cm_cop", 
                                                         "dem.weight_signup_kg_cop", 
                                                         "dem.bmi_signup_cop")]

coping_glad_dem_gender <- coping_glad_dem_gender[, c("ID", 
                                                     "dem.sex_cop", 
                                                     "dem.which_gender_do_you_identify_with_cop", 
                                                     "dem.sex_cop_numeric", 
                                                     "dem.which_gender_do_you_identify_with_cop_numeric", 
                                                     "dem.do_you_identify_as_transgender_cop", 
                                                     "dem.what_is_your_sexual_orientation_cop", 
                                                     "dem.do_you_identify_as_transgender_cop_numeric", 
                                                     "dem.what_is_your_sexual_orientation_cop_numeric")]


# ========== Combine and add cohort name ==========
## Merge data
coping_glad_clean_dem <- merge(merge(merge(coping_glad_dem_age, coping_glad_dem_anthropo, by = "ID", all = TRUE), coping_glad_dem_ethnicity, by = "ID", all.x = TRUE), coping_glad_dem_gender, by = "ID", all = TRUE)

## Add cohort
coping_glad_clean_dem$cohort <- "coping_glad"

summary(coping_glad_clean_dem)


# ========== Unify variables and variable names across datasets: match to var names in glad ==========
coping_glad_clean_dem <- coping_glad_clean_dem[, c("ID", 
                                                   "dem.dob_age_cop", 
                                                   "dem.height_signup_cm_cop", 
                                                   "dem.weight_signup_kg_cop", 
                                                   "dem.bmi_signup_cop", 
                                                   "dem.what_is_your_ethnic_origin", 
                                                   "dem.sex_cop", 
                                                   "dem.sex_cop_numeric",
                                                   "cohort")]; names(coping_glad_clean_dem) <- c("IID", 
                                                                                                 "dem.dob_age_cop", 
                                                                                                 "dem.height_signup_cm_cop", 
                                                                                                 "dem.weight_signup_kg_cop", 
                                                                                                 "dem.bmi_signup_cop", 
                                                                                                 "dem.what_is_your_ethnic_origin", 
                                                                                                 "dem.sex_cop", 
                                                                                                 "dem.sex_cop_numeric","cohort")


# ================= INDEPENDENT VARIABLE: EDs ==========
# ========== Read ED data and inspect ==========
coping_glad_an <- readRDS("data_raw/EDs/an_coping_glad.rds")
coping_glad_bn <- readRDS("data_raw/EDs/icb_coping_glad.rds")
coping_glad_bed <- readRDS("data_raw/EDs/be_coping_glad.rds")

names(coping_glad_an)
names(coping_glad_bn)
names(coping_glad_bed)

# ========== Identify duplicates ==========
dup_coping_glad_an <- duplicated(coping_glad_an$externalDataReference); table(dup_coping_glad_an) #765 
dup_coping_glad_bn <- duplicated(coping_glad_bn$externalDataReference); table(dup_coping_glad_bn) #765
dup_coping_glad_bed <- duplicated(coping_glad_bed$externalDataReference); table(dup_coping_glad_bed) #765

# Function to log participant counts
log_participants <- function(df, cohort, variable, step) {
  count <- length(unique(df$IID))  # Count unique participant IDs
  new_entry <- data.frame(Step = step, Cohort = cohort, Variable = variable, Unique_Participants = count)
  assign("participant_tracking", rbind(participant_tracking, new_entry), envir = .GlobalEnv)
}

# Track original participant counts
log_participants(coping_glad_an, "GLAD", "Anorexia", "Before Removing Duplicates")
log_participants(coping_glad_bn, "GLAD", "Bulimia", "Before Removing Duplicates")
log_participants(coping_glad_bed, "GLAD", "Binge Eating", "Before Removing Duplicates")

# ========== Remove duplicated and incomplete IDs ==========
coping_glad_an <- remove_duplicates(coping_glad_an, "externalDataReference", date_col = "endDate"); table(duplicated(coping_glad_an$externalDataReference))
coping_glad_bn <- remove_duplicates(coping_glad_bn, "externalDataReference", date_col = "endDate"); table(duplicated(coping_glad_bn$externalDataReference))
coping_glad_bed <- remove_duplicates(coping_glad_bed, "externalDataReference", date_col = "endDate"); table(duplicated(coping_glad_bed$externalDataReference))

# ========== Select items ==========
## AN
coping_glad_an <- coping_glad_an[, c("externalDataReference", 
                                     "an.1.lowest_weight_weigh_weighed", 
                                     "an.1.gain_weight_low_weight", 
                                     "an.1.not_at_all_dependentcompletely_dependent", 
                                     "an.1.low_weight_health_negative", 
                                     "an.1.feel_fat_low_weight",
                                     "an.1.body_larger_low_weight")]

## BN
coping_glad_bn <- coping_glad_bn[, c("externalDataReference", 
                                     "icb.body_shape_control_weight.fasted_or_did_not_eat_for_8_waking_hours_or_more", 
                                     "icb.body_shape_control_weight.used_diet_pills_over_the_counter_or_prescription", 
                                     "icb.body_shape_control_weight.exercised_excessively__e.g._felt_compelled_to_exercise_felt_uneasy_or_distressed_if_unable_to_exercise", 
                                     "icb.body_shape_control_weight.made_yourself_vomit", 
                                     "icb.body_shape_control_weight.used_laxatives_including_pills_or_liquids_meant_to_stimulate_bowel_movements", 
                                     "icb.body_shape_control_weight.used_diuretics_water_pills", 
                                     "icb.body_shape_felt_compelled", 
                                     "icb.felt_uneasy_unable_distressed", 
                                     "icb.order_friends_exercise_times", 
                                     "icb.prevented_injury_illness_exercised", 
                                     "icb.making_yourself_vomit", 
                                     "icb.laxatives", 
                                     "icb.diuretics", 
                                     "icb.weight_loss_pills", 
                                     "icb.excessive_exercise", 
                                     "icb.fasting", 
                                     "icb.other_methods", 
                                     "icb.none", 
                                     "icb.making_yourself_vomit.2", 
                                     "icb.laxatives.2",
                                     "icb.diuretics_", 
                                     "icb.weight_loss_pills.2", 
                                     "icb.excessive_exercise.1",
                                     "icb.fasting.2", 
                                     "icb.other_methods.2", 
                                     "icb.none_of_the_above",
                                     "icb.modified_reason_unable_exercise")]

## BED
coping_glad_bed <- coping_glad_bed[, c("externalDataReference", 
                                       "be.ate_regard_short_period", 
                                       "be.regularly_occurring_episodes_binge", 
                                       "be.feel_distressed_overeating_episodes", 
                                       "be.not_at_all_dependentcompletely_dependent", 
                                       "be.regularly_occurring_overeating_episodes", 
                                       "be.binge_eating_distressed_make", 
                                       "be.during_eating_binges_did_you__.eat_much_more_rapidly_than_usual", 
                                       "be.during_eating_binges_did_you__.eat_until_you_felt_uncomfortably_full", 
                                       "be.during_eating_binges_did_you__.eat_large_amounts_of_food_when_you_didnt_feel_physically_hungry", 
                                       "be.during_eating_binges_did_you__.eat_alone_because_you_were_embarrassed_by_whathow_much_you_were_eating", 
                                       "be.during_eating_binges_did_you__.feel_ashameddisgusted_with_yourself_depressed_or_very_guilty_after_overeating", 
                                       "be.during_eating_binges_did_you__.feel_like_you_had_no_control_over_your_eating_e.g._not_being_able_to_stop_eating_feeling_compelled_to_eat_or_going_back_and_forth_for_more_food", 
                                       "be.during_eating_binges_did_you__.make_yourself_vomit_as_a_means_to_control_your_weight_and_shape")]

# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
coping_glad_an <- coping_glad_an %>%
  mutate_all(~ ifelse(. < 0, NA, .))

coping_glad_bn <- coping_glad_bn %>%
  mutate_all(~ ifelse(. < 0, NA, .))

coping_glad_bed <- coping_glad_bed %>%
  mutate_all(~ ifelse(. < 0, NA, .))

# ========== Create symptom sub-scales ==========
## Variables
# BN
coping_glad_bn$icb.weight_control <- my_rowSums(coping_glad_bn[, 2:7])
coping_glad_bn$icb.lowest_weight_control_shape <- my_rowSums(coping_glad_bn[, 12:19])
coping_glad_bn$icb.compensate <- my_rowSums(coping_glad_bn[, 20:27])
coping_glad_bn$icb.exercise <- my_rowSums(coping_glad_bn[, c(8:11, 28)])

# BED
coping_glad_bed$be.during_binges <- my_rowSums(coping_glad_bed[, 8:14])

# ========== Create overall symptom scales ==========
## Flip AN item: an.lowest_weight_people_thought
coping_glad_an$an.1.lowest_weight_weigh_weighed <- coping_glad_an$an.1.lowest_weight_weigh_weighed * -1

## Variables
# AN
coping_glad_an$an.total_score <- my_rowSums(coping_glad_an[,2:7])

# BN
coping_glad_bn$bn.total_score <- my_rowSums(coping_glad_bn[,2:28])

# BED
coping_glad_bed$bed.total_score <- my_rowSums(coping_glad_bed[,2:14])

# ========== Unify variables and variable names across datasets: match to var names in glad ==========
coping_glad_an <- coping_glad_an[, c("externalDataReference", 
                                     "an.1.lowest_weight_weigh_weighed", 
                                     "an.1.gain_weight_low_weight", 
                                     "an.1.not_at_all_dependentcompletely_dependent", 
                                     "an.1.low_weight_health_negative", 
                                     "an.1.feel_fat_low_weight", 
                                     "an.1.body_larger_low_weight",
                                     "an.total_score")]; names(coping_glad_an) <- c("IID", 
                                                                                    "an.lowest_weight_people_thought", 
                                                                                    "an.gain_weight_afraid_fat", 
                                                                                    "an.not_at_all_dependentcompletely_dependent", 
                                                                                    "an.health_low_weightbmi_negative", 
                                                                                    "an.feel_fat_time_low",
                                                                                    "an.people_thought_larger_parts",
                                                                                    "an.total_score")

coping_glad_bn <- coping_glad_bn[, c("externalDataReference", "icb.weight_control", 
                                     "icb.lowest_weight_control_shape", 
                                     "icb.compensate", 
                                     "icb.exercise",
                                     "bn.total_score")]; names(coping_glad_bn) <- c("IID",
                                                                                    "icb.weight_control", 
                                                                                    "icb.lowest_weight_control_shape", 
                                                                                    "icb.compensate", 
                                                                                    "icb.exercise",
                                                                                    "bn.total_score")


coping_glad_bed <- coping_glad_bed[, c("externalDataReference", 
                                       "be.ate_regard_short_period", 
                                       "be.regularly_occurring_episodes_binge", 
                                       "be.feel_distressed_overeating_episodes", 
                                       "be.not_at_all_dependentcompletely_dependent", 
                                       "be.regularly_occurring_overeating_episodes", 
                                       "be.binge_eating_distressed_make", 
                                       "be.during_binges",
                                       "bed.total_score")]; names(coping_glad_bed) <- c("IID",
                                                                                        "be.ate_regard_short_period", 
                                                                                        "be.regularly_occurring_episodes_binge", 
                                                                                        "be.feel_distressed_overeating_episodes", 
                                                                                        "be.not_at_all_dependentcompletely_dependent", 
                                                                                        "be.regularly_occurring_overeating_episodes", 
                                                                                        "be.binge_eating_distressed_make", 
                                                                                        "be.during_binges",
                                                                                        "bed.total_score")


# let's observe our data #
# stats for an 
summary(coping_glad_an$an.total_score)
ggplot(coping_glad_an, aes(x = "", y = an.total_score)) + 
  geom_boxplot(fill = "lightblue", outlier.colour = "red") + 
  labs(title = "Boxplot of AN Total Score")
ggplot(coping_glad_an, aes(x = an.total_score)) + 
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) + 
  labs(title = "Histogram of AN Total Score")

# stats for bn 
summary(coping_glad_bn$bn.total_score)
ggplot(coping_glad_bn, aes(x = "", y = bn.total_score)) + 
  geom_boxplot(fill = "lightblue", outlier.colour = "red") + 
  labs(title = "Boxplot of BN Total Score")
ggplot(coping_glad_bn, aes(x = bn.total_score)) + 
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) + 
  labs(title = "Histogram of BN Total Score")

# stats for bed 
summary(coping_glad_bed$bed.total_score)
ggplot(coping_glad_bed, aes(x = "", y = bed.total_score)) + 
  geom_boxplot(fill = "lightblue", outlier.colour = "red") + 
  labs(title = "Boxplot of BED Total Score")
ggplot(coping_glad_bed, aes(x = bed.total_score)) + 
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) + 
  labs(title = "Histogram of BED Total Score")

## ADDED! Removing outliers ## 
remove_outliers_iqr_count <- function(df, column) {
  # Compute IQR bounds
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  upper_bound <- Q3 + (1.5 * IQR_value)
  lower_bound <- Q1 - (1.5 * IQR_value)  # Generally not an issue for positive scores
  
  # Identify outliers
  outlier_count <- sum(df[[column]] < lower_bound | df[[column]] > upper_bound, na.rm = TRUE)
  
  # Print cut-off values and outliers removed
  print(paste("For", column, "- Lower Bound:", round(lower_bound, 2), "Upper Bound:", round(upper_bound, 2)))
  print(paste("Outliers removed for", column, ":", outlier_count))
  
  # Filter the dataset to keep only inliers
  df_clean <- df %>% filter(df[[column]] >= lower_bound & df[[column]] <= upper_bound)
  
  # Return cleaned dataset and count of removed outliers
  return(list(clean_data = df_clean, removed_outliers = outlier_count))
}

# Apply outlier removal and count removed values for AN total score
an_outlier_result <- remove_outliers_iqr_count(coping_glad_an, "an.total_score")
coping_glad_an <- an_outlier_result$clean_data
an_outliers_removed <- an_outlier_result$removed_outliers

# Apply outlier removal and count removed values for BN total score
bn_outlier_result <- remove_outliers_iqr_count(coping_glad_bn, "bn.total_score")
coping_glad_bn <- bn_outlier_result$clean_data
bn_outliers_removed <- bn_outlier_result$removed_outliers

# Apply outlier removal and count removed values for BED total score
bed_outlier_result <- remove_outliers_iqr_count(coping_glad_bed, "bed.total_score")
coping_glad_bed <- bed_outlier_result$clean_data
bed_outliers_removed <- bed_outlier_result$removed_outliers

# Create a summary table
outliers_summary <- data.frame(
  Variable = c("AN Total Score", "BN Total Score", "BED Total Score"),
  Outliers_Removed = c(an_outliers_removed, bn_outliers_removed, bed_outliers_removed)
)
print(outliers_summary)

# ========== Combine and add cohort name ==========
## Merge data
coping_glad_EDs <- merge(merge(coping_glad_an, coping_glad_bn, by = "IID", all = TRUE), coping_glad_bed, by = "IID", all = TRUE)

## Add cohort
coping_glad_EDs$cohort <- "coping_glad"


# Count total valid (non-missing) observations for each ED subtype
total_an <- sum(!is.na(coping_glad_an$an.total_score))
total_bn <- sum(!is.na(coping_glad_bn$bn.total_score))
total_bed <- sum(!is.na(coping_glad_bed$bed.total_score))

# Print total valid cases for each subtype
cat("Total valid cases per eating disorder subtype (after duplicate removal):\n")
cat("Anorexia Nervosa (AN):", total_an, "out of", 18360, "\n")
cat("Bulimia Nervosa (BN):", total_bn, "out of", 18360, "\n")
cat("Binge Eating Disorder (BED):", total_bed, "out of", 18360, "\n")

# ================= VARIABLE 2: TAF GLAD ==========
# ========== Read TAF data and inspect ==========
coping_glad_taf <- readRDS("data_raw/SH SUI/taf_coping_glad.rds")
names(coping_glad_taf)

# ========== Identify duplicates ==========
dup_coping_glad_taf <- duplicated(coping_glad_taf$externalDataReference); table(dup_coping_glad_taf)

# ========== Remove duplicated and incomplete IDs ==========
coping_glad_taf <- remove_duplicates(coping_glad_taf, "externalDataReference", date_col = "endDate"); table(duplicated(coping_glad_taf$externalDataReference))

# ========== Select items (ADDED and REMOVED!!) ==========
## TAF
coping_glad_taf <- coping_glad_taf[, c("externalDataReference", 
                                       "taf.worth_living_life_thoughts",
                                       "taf.past_felt_weeks", 
                                       "taf.past_felt_weeks.1",
                                       "taf.have_you_contemplated_harming_yourself_",
                                       "taf.meant_end_life_weeks")]


# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
coping_glad_taf <- coping_glad_taf %>%
  mutate_all(~ ifelse(. < 0, NA, .))

# ========== Unify variables and variable names across datasets: match to var names in glad ==========
coping_glad_taf <- coping_glad_taf[, c("externalDataReference", 
                                       "taf.worth_living_life_thoughts",
                                       "taf.past_felt_weeks", 
                                       "taf.past_felt_weeks.1",
                                       "taf.have_you_contemplated_harming_yourself_",
                                       "taf.meant_end_life_weeks")]

# Rename variables for consistency
names(coping_glad_taf) <- c("IID", 
                            "taf.worth_living_life_thoughts",
                            "taf.past_felt_weeks", 
                            "taf.past_felt_weeks.1",
                            "taf.have_you_contemplated_harming_yourself_",
                            "taf.meant_end_life_weeks")

# ========== Add cohort name ==========
coping_glad_taf$cohort <- "coping_glad"


# ================= VARIABLE 3: PANWORRY GLAD==========
# ========== Read PANWORRY data and inspect ==========
coping_glad_panworry <- readRDS("data_raw/Anx/panworry_coping_glad.rds") 

names(coping_glad_panworry)
# ========== Identify duplicates ==========
dup_coping_glad_panworry <- duplicated(coping_glad_panworry$externalDataReference)
table(dup_coping_glad_panworry) #there are 765 duplicated entries 

# ========== Remove duplicated and incomplete IDs ==========
coping_glad_panworry <- remove_duplicates(coping_glad_panworry, "externalDataReference", date_col = "endDate")
table(duplicated(coping_glad_panworry$externalDataReference))  # Checking if duplicates are removed

# ========== Select items (ADDED and REMOVED!!) ==========
## Panworry
coping_glad_panworry <- coping_glad_panworry[, c("externalDataReference", 
                                                 "panworry.impact_on_your_employment_status",              
                                                 "panworry.household_employment_status_key",               
                                                 "panworry.impact_on_your_education_or_exams",             
                                                 "panworry.exams_education_impact_children",              
                                                 "panworry.financial_impact",                              
                                                 "panworry.contracting_the_virus",                         
                                                 "panworry.people_you_know_contracting_the_virus",         
                                                 "panworry.people_you_dont_know_contracting_the_virus",    
                                                 "panworry.wellbeing_impact_mental_health",                
                                                 "panworry.wellbeing_childrens_mental_health",             
                                                 "panworry.relatives_impact_mental_health",                
                                                 "panworry.being_socially_isolated",                       
                                                 "panworry.people_you_know_being_socially_isolated",       
                                                 "panworry.shortage_of_essential_supplies_",              
                                                 "panworry.shortage_of_medication_or_access_to_healthcare",
                                                 "panworry.healthcare_shortage_people_essential",          
                                                 "panworry.virus_government_information_accuracy",         
                                                 "panworry.the_governments_response_to_the_pandemic",      
                                                 "panworry.separation_family_members",                     
                                                 "panworry.global_recession_economy_longstanding")]

# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
coping_glad_panworry <- coping_glad_panworry %>%
  mutate_all(~ ifelse(. < 0, NA, .))


# ========== Unify variables and variable names across datasets: match to var names in glad ==========
coping_glad_panworry <- coping_glad_panworry[, c("externalDataReference", 
                                                 "panworry.impact_on_your_employment_status",              
                                                 "panworry.household_employment_status_key",               
                                                 "panworry.impact_on_your_education_or_exams",             
                                                 "panworry.exams_education_impact_children",              
                                                 "panworry.financial_impact",                              
                                                 "panworry.contracting_the_virus",                         
                                                 "panworry.people_you_know_contracting_the_virus",         
                                                 "panworry.people_you_dont_know_contracting_the_virus",    
                                                 "panworry.wellbeing_impact_mental_health",                
                                                 "panworry.wellbeing_childrens_mental_health",             
                                                 "panworry.relatives_impact_mental_health",                
                                                 "panworry.being_socially_isolated",                       
                                                 "panworry.people_you_know_being_socially_isolated",       
                                                 "panworry.shortage_of_essential_supplies_",              
                                                 "panworry.shortage_of_medication_or_access_to_healthcare",
                                                 "panworry.healthcare_shortage_people_essential",          
                                                 "panworry.virus_government_information_accuracy",         
                                                 "panworry.the_governments_response_to_the_pandemic",      
                                                 "panworry.separation_family_members",                     
                                                 "panworry.global_recession_economy_longstanding")]


# Rename variables for consistency
names(coping_glad_panworry) <- c("IID", 
                                 "panworry.impact_on_your_employment_status",              
                                 "panworry.employment_status_household_key",               
                                 "panworry.impact_on_your_education_or_exams",             
                                 "panworry.exams_education_impact_children",               
                                 "panworry.financial_impact",                              
                                 "panworry.contracting_the_virus" ,                        
                                 "panworry.people_you_know_contracting_the_virus" ,        
                                 "panworry.people_you_dont_know_contracting_the_virus"    ,
                                 "panworry.wellbeing_mental_health_impact",                
                                 "panworry.wellbeing_childrens_mental_health",             
                                 "panworry.relatives_mental_health_impact",                
                                 "panworry.being_socially_isolated"  ,                     
                                 "panworry.people_you_know_being_socially_isolated" ,      
                                 "panworry.shortage_of_essential_supplies_" ,              
                                 "panworry.shortage_of_medication_or_access_to_healthcare",
                                 "panworry.healthcare_shortage_people_essential" ,         
                                 "panworry.virus_government_information_accuracy"  ,       
                                 "panworry.the_governments_response_to_the_pandemic"  ,    
                                 "panworry.family_members_separation"      ,               
                                 "panworry.global_recession_economy_longstanding")


# ========== Add cohort name ==========
coping_glad_panworry$cohort <- "coping_glad"





# ================= VARIABLE 5: Psychopathology GLAD ==========
# ========== Read psychopathology data and inspect ==========
coping_glad_phq9 <- readRDS("data_raw/Depr/phq_coping_glad.rds") 
coping_glad_gad7 <- readRDS("data_raw/Anx/gad7_coping_glad.rds") 

names(coping_glad_phq9)
names(coping_glad_gad7)

# ========== Identify duplicates ==========
dup_coping_glad_phq9 <- duplicated(coping_glad_phq9$externalDataReference); table(dup_coping_glad_phq9)
dup_coping_glad_gad7 <- duplicated(coping_glad_gad7$externalDataReference); table(dup_coping_glad_gad7)

# ========== Remove duplicated and incomplete IDs ==========
coping_glad_phq9 <- remove_duplicates(coping_glad_phq9, "externalDataReference", date_col = "endDate"); table(duplicated(coping_glad_phq9$externalDataReference))
coping_glad_gad7 <- remove_duplicates(coping_glad_gad7, "externalDataReference", date_col = "endDate"); table(duplicated(coping_glad_gad7$externalDataReference))

# ========== Select items ==========
## Depression (ADDED!)
coping_glad_phq9 <- coping_glad_phq9[, c("externalDataReference", 
                                         "phq9.little_interest_or_pleasure_in_doing_things",
                                         "phq9.feeling_down_depressed_or_hopeless",        
                                         "phq9.staying_asleep_sleeping_trouble",            
                                         "phq9.feeling_tired_or_having_little_energy",     
                                         "phq9.poor_appetite_or_overeating",               
                                         "phq9.feeling_bad_failure_family",               
                                         "phq9.trouble_concentrating_newspaper_reading",   
                                         "phq9.moving_fidgety_opposite_slowly",          
                                         "phq9.dead_hurting_thoughts",                  
                                         "phq9.problems_made_difficult_care")]

## Anxiety (ADDED!)
coping_glad_gad7 <- coping_glad_gad7[, c("externalDataReference", 
                                         "gad7.feeling_nervous_anxious_or_on_edge", 
                                         "gad7.control_worrying_stop", 
                                         "gad7.worrying_too_much_about_different_things", 
                                         "gad7.trouble_relaxing", 
                                         "gad7.sit_restless_hard", 
                                         "gad7.becoming_easily_annoyed_or_irritable",
                                         "gad7.awful_feeling_afraid_happen",
                                         "gad7.problems_made_difficult_care",
                                         "gad7.pandemic_feelings_felt",
                                         "gad7.feeling_nervous_anxious_or_on_edge.1",
                                         "gad7.control_worrying_stop.1",
                                         "gad7.worrying_too_much_about_different_things.1",
                                         "gad7.trouble_relaxing.1",
                                         "gad7.sit_restless_hard.1",
                                         "gad7.becoming_easily_annoyed_or_irritable.1",
                                         "gad7.awful_feeling_afraid_happen.1")]

# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
coping_glad_phq9 <- coping_glad_phq9 %>%
  mutate_all(~ ifelse(. < 0, NA, .))

coping_glad_gad7 <- coping_glad_gad7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))

# ========== Create overall symptom scales ==========
## PHQ9 Total Score (Summed Symptoms)
coping_glad_phq9$phq9.total_score <- my_rowSums(coping_glad_phq9[,2:11])

## GAD7 Total Score (Summed Symptoms)
coping_glad_gad7$gad7.total_score <- my_rowSums(coping_glad_gad7[,2:9])

# ========== Standardize Variable Naming for Merging ==========
coping_glad_phq9 <- coping_glad_phq9[, c("externalDataReference", 
                                         "phq9.little_interest_or_pleasure_in_doing_things", 
                                         "phq9.feeling_down_depressed_or_hopeless", 
                                         "phq9.staying_asleep_sleeping_trouble", 
                                         "phq9.feeling_tired_or_having_little_energy", 
                                         "phq9.poor_appetite_or_overeating", 
                                         "phq9.feeling_bad_failure_family", 
                                         "phq9.trouble_concentrating_newspaper_reading", 
                                         "phq9.moving_fidgety_opposite_slowly", 
                                         "phq9.dead_hurting_thoughts",
                                         "phq9.problems_made_difficult_care",
                                         "phq9.total_score")]; names(coping_glad_phq9) <- c("IID",
                                                                                            "phq9.little_interest_or_pleasure_in_doing_things", 
                                                                                            "phq9.feeling_down_depressed_or_hopeless", 
                                                                                            "phq9.staying_asleep_sleeping_trouble", 
                                                                                            "phq9.feeling_tired_or_having_little_energy", 
                                                                                            "phq9.poor_appetite_or_overeating", 
                                                                                            "phq9.feeling_bad_failure_family", 
                                                                                            "phq9.trouble_concentrating_newspaper_reading", 
                                                                                            "phq9.moving_fidgety_opposite_slowly", 
                                                                                            "phq9.dead_hurting_thoughts",
                                                                                            "phq9.problems_made_difficult_care",
                                                                                            "phq9.total_score")


coping_glad_gad7 <- coping_glad_gad7[, c("externalDataReference", 
                                         "gad7.feeling_nervous_anxious_or_on_edge", 
                                         "gad7.control_worrying_stop", 
                                         "gad7.worrying_too_much_about_different_things", 
                                         "gad7.trouble_relaxing", 
                                         "gad7.sit_restless_hard", 
                                         "gad7.becoming_easily_annoyed_or_irritable",
                                         "gad7.awful_feeling_afraid_happen",
                                         "gad7.problems_made_difficult_care",
                                         "gad7.total_score")]; names(coping_glad_gad7) <- c("IID",
                                                                                            "gad7.feeling_nervous_anxious_or_on_edge", 
                                                                                            "gad7.control_worrying_stop", 
                                                                                            "gad7.worrying_too_much_about_different_things", 
                                                                                            "gad7.trouble_relaxing", 
                                                                                            "gad7.sit_restless_hard", 
                                                                                            "gad7.becoming_easily_annoyed_or_irritable",
                                                                                            "gad7.awful_feeling_afraid_happen",
                                                                                            "gad7.problems_made_difficult_care",
                                                                                            "gad7.total_score")



# ========== Merge PHQ9 and GAD7 Data ==========
coping_glad_psychopathology <- merge(coping_glad_phq9, coping_glad_gad7, by = "IID", all = TRUE)

# ========== Add Cohort Name ==========
coping_glad_psychopathology$cohort <- "coping_glad"



                              # ========== COHORT 2: EDGI  ==========

## Cohort2. Demographic Measures
# ========== Read clean dem data and inspect ==========
coping_edgi_dem_age <- readRDS("data/demographics - coping_glad_edgi_nbr/age_coping_glad_edgi_nbr_clean.rds")
coping_edgi_dem_ethnicity <- readRDS("data/demographics - glad_edgi/ethnicity_glad_edgi_clean.rds")
coping_edgi_dem_anthropo <- readRDS("data/demographics - coping_glad_edgi_nbr/signup_bmi_height_weight_coping_glad_edgi_nbr_clean.rds")
coping_edgi_dem_gender <- readRDS("data/demographics - coping_glad_edgi_nbr/sex_gender_sexuality_coping_glad_edgi_nbr_clean.rds")

names(coping_edgi_dem_age)
names(coping_edgi_dem_ethnicity)
names(coping_edgi_dem_anthropo)
names(coping_edgi_dem_gender)

# ========== Select cohort and items ==========
## Cohort
coping_edgi_dem_age <- coping_edgi_dem_age[coping_edgi_dem_age$sample == "EDGI",]
coping_edgi_dem_ethnicity <- coping_edgi_dem_ethnicity[coping_edgi_dem_ethnicity$sample == "EDGI",]
coping_edgi_dem_anthropo <- coping_edgi_dem_anthropo[coping_edgi_dem_anthropo$sample == "EDGI",]
coping_edgi_dem_gender <- coping_edgi_dem_gender[coping_edgi_dem_gender$sample == "EDGI",]

## Items
coping_edgi_dem_age <- coping_edgi_dem_age[, c("ID", 
                                               "dem.dob_age_cop")]

coping_edgi_dem_ethnicity <- coping_edgi_dem_ethnicity[, c("ID",
                                                           "dem.what_is_your_ethnic_origin")]

coping_edgi_dem_anthropo <- coping_edgi_dem_anthropo[, c("ID", 
                                                         "dem.height_signup_cm_cop", 
                                                         "dem.weight_signup_kg_cop", 
                                                         "dem.bmi_signup_cop")]

coping_edgi_dem_gender <- coping_edgi_dem_gender[, c("ID", 
                                                     "dem.sex_cop", 
                                                     "dem.which_gender_do_you_identify_with_cop", 
                                                     "dem.sex_cop_numeric", 
                                                     "dem.which_gender_do_you_identify_with_cop_numeric", 
                                                     "dem.do_you_identify_as_transgender_cop", 
                                                     "dem.what_is_your_sexual_orientation_cop", 
                                                     "dem.do_you_identify_as_transgender_cop_numeric", 
                                                     "dem.what_is_your_sexual_orientation_cop_numeric")]

# ========== Combine and add cohort name ==========
## Merge data
coping_edgi_clean_dem <- merge(merge(merge(coping_edgi_dem_age, coping_edgi_dem_anthropo, by = "ID", all = TRUE), coping_edgi_dem_ethnicity, by = "ID", all.x = TRUE), coping_edgi_dem_gender, by = "ID", all = TRUE)

## Add cohort
coping_edgi_clean_dem$cohort <- "coping_edgi"

# ========== Unify variables and variable names across datasets: match to var names in glad ==========
coping_edgi_clean_dem <- coping_edgi_clean_dem[, c("ID", 
                                                   "dem.dob_age_cop", 
                                                   "dem.height_signup_cm_cop", 
                                                   "dem.weight_signup_kg_cop", 
                                                   "dem.bmi_signup_cop", 
                                                   "dem.what_is_your_ethnic_origin", 
                                                   "dem.sex_cop", 
                                                   "dem.sex_cop_numeric",
                                                   "cohort")]; names(coping_edgi_clean_dem) <- c("IID", 
                                                                                                 "dem.dob_age_cop", 
                                                                                                 "dem.height_signup_cm_cop", 
                                                                                                 "dem.weight_signup_kg_cop", 
                                                                                                 "dem.bmi_signup_cop", 
                                                                                                 "dem.what_is_your_ethnic_origin", 
                                                                                                 "dem.sex_cop", 
                                                                                                 "dem.sex_cop_numeric",
                                                                                                 "cohort")
# ================= INDEPENDENT VARIABLE: EDs EDGI  ==========
edgi_an <- readRDS("data_raw/EDs/an_edgi.rds")
edgi_bn <- readRDS("data_raw/EDs/icb_edgi.rds")
edgi_bed <- readRDS("data_raw/EDs/be_edgi.rds")

names(edgi_an)
names(edgi_bn)
names(edgi_bed)

# ========== Identify duplicates ==========
dup_edgi_an <- duplicated(edgi_an$externalDataReference); table(dup_edgi_an) #81 duplicated
dup_edgi_bn <- duplicated(edgi_bn$externalDataReference); table(dup_edgi_bn)
dup_edgi_bed <- duplicated(edgi_bed$externalDataReference); table(dup_edgi_bed)

# ========== Remove duplicated and incomplete IDs ==========
edgi_an <- remove_duplicates(edgi_an, "externalDataReference", date_col = "endDate"); table(duplicated(edgi_an$externalDataReference))
edgi_bn <- remove_duplicates(edgi_bn, "externalDataReference", date_col = "endDate"); table(duplicated(edgi_bn$externalDataReference))
edgi_bed <- remove_duplicates(edgi_bed, "externalDataReference", date_col = "endDate"); table(duplicated(edgi_bed$externalDataReference))

# ========== Select items ==========
## AN
edgi_an <- edgi_an[, c("externalDataReference", 
                       "an.lowest_weight_people_thought", 
                       "an.gain_weight_fat_afraid", 
                       "an.not_at_all_dependentcompletely_dependent", 
                       "an.health_low_weightbmi_negative", 
                       "an.feel_fat_time_low", 
                       "an.people_thought_larger_body")] 

## BN
edgi_bn <- edgi_bn[, c("externalDataReference", 
                       "icb.body_shape_control_weight.fasted_or_did_not_eat_for_8_waking_hours_or_more", 
                       "icb.body_shape_control_weight.used_diet_pills_over_the_counter_or_prescription", 
                       "icb.body_shape_control_weight.exercised_excessively__e.g._felt_compelled_to_exercise_felt_uneasy_or_distressed_if_unable_to_exercise", 
                       "icb.body_shape_control_weight.made_yourself_vomit", 
                       "icb.body_shape_control_weight.used_laxatives_including_pills_or_liquids_meant_to_stimulate_bowel_movements", 
                       "icb.body_shape_control_weight.used_diuretics_water_pills", 
                       "icb.body_shape_felt_compelled", 
                       "icb.felt_uneasy_unable_distressed", 
                       "icb.order_friends_exercise_times", 
                       "icb.injury_prevented_illness_exercised", 
                       "icb.making_yourself_vomit", 
                       "icb.laxatives", 
                       "icb.diuretics", 
                       "icb.weight_loss_pills", 
                       "icb.excessive_exercise_", 
                       "icb.fasting", 
                       "icb.other_methods", 
                       "icb.none", 
                       "icb.making_yourself_vomit.1", 
                       "icb.laxatives.1", 
                       "icb.diuretics.1", 
                       "icb.weight_loss_pills.1", 
                       "icb.excessive_exercise", 
                       "icb.fasting.1", 
                       "icb.other_methods.1", 
                       "icb.none.1", 
                       "icb.modified_reason_unable_exercise")]
## BED
edgi_bed <- edgi_bed[, c("externalDataReference", 
                         "be.short_period_ate_regard", 
                         "be.stop_eating_binge_eating", 
                         "be.feel_distressed_overeating_episodes", 
                         "be.not_at_all_dependentcompletely_dependent", 
                         "be.experience_regular_episodes_lowest",
                         "be.binge_eating_make_distressed", 
                         "be.during_eating_binges_did_you__.a_eat_much_more_rapidly_than_usual", 
                         "be.during_eating_binges_did_you__.b_eat_until_you_felt_uncomfortably_full", 
                         "be.during_eating_binges_did_you__.c_eat_large_amounts_of_food_when_you_didnt_feel_physically_hungry", 
                         "be.during_eating_binges_did_you__.d_eat_alone_because_you_were_embarrassed_by_whathow_much_you_were_eating", 
                         "be.during_eating_binges_did_you__.e_feel_ashameddisgusted_with_yourself_depressed_or_very_guilty_after_overeating", 
                         "be.during_eating_binges_did_you__.f_feel_like_you_had_no_control_over_your_eating_e.g._not_being_able_to_stop_eating_feeling_compelled_to_eat_or_going_back_and_forth_for_more_food", 
                         "be.during_eating_binges_did_you__.g_make_yourself_vomit_as_a_means_to_control_your_weight_and_shape")]

# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
edgi_an <- edgi_an %>%
  mutate_all(~ ifelse(. < 0, NA, .))

edgi_bn <- edgi_bn %>%
  mutate_all(~ ifelse(. < 0, NA, .))

edgi_bed <- edgi_bed %>%
  mutate_all(~ ifelse(. < 0, NA, .))

# ========== Create symptom sub-scales ==========
## Variables
# BN
edgi_bn$icb.weight_control <- my_rowSums(edgi_bn[, 2:7])
edgi_bn$icb.lowest_weight_control_shape <- my_rowSums(edgi_bn[, 12:19])
edgi_bn$icb.compensate <- my_rowSums(edgi_bn[, 20:27])
edgi_bn$icb.exercise <- my_rowSums(edgi_bn[, c(8:11, 28)])

# BED
edgi_bed$be.during_binges <- my_rowSums(edgi_bed[, 8:14])

# ========== Create overall symptom scales ==========
## Flip AN item: an.lowest_weight_people_thought
edgi_an$an.lowest_weight_people_thought <- edgi_an$an.lowest_weight_people_thought * -1

## Variables
# AN
edgi_an$an.total_score <- my_rowSums(edgi_an[,2:7])

# BN
edgi_bn$bn.total_score <- my_rowSums(edgi_bn[,2:28])

# BED
edgi_bed$bed.total_score <- my_rowSums(edgi_bed[,2:14])

# ========== Unify variables and variable names across datasets: match to var names in glad ==========
edgi_an <- edgi_an[, c("externalDataReference", 
                       "an.lowest_weight_people_thought", 
                       "an.gain_weight_fat_afraid", 
                       "an.not_at_all_dependentcompletely_dependent", 
                       "an.health_low_weightbmi_negative", 
                       "an.feel_fat_time_low", 
                       "an.people_thought_larger_body",
                       "an.total_score")]; names(edgi_an) <- c("IID", 
                                                               "an.lowest_weight_people_thought", 
                                                               "an.gain_weight_afraid_fat", 
                                                               "an.not_at_all_dependentcompletely_dependent", 
                                                               "an.health_low_weightbmi_negative", 
                                                               "an.feel_fat_time_low",
                                                               "an.people_thought_larger_parts",
                                                               "an.total_score")

edgi_bn <- edgi_bn[, c("externalDataReference", 
                       "icb.weight_control", 
                       "icb.lowest_weight_control_shape", 
                       "icb.compensate", 
                       "icb.exercise",
                       "bn.total_score")]; names(edgi_bn) <- c("IID",
                                                               "icb.weight_control", 
                                                               "icb.lowest_weight_control_shape", 
                                                               "icb.compensate", 
                                                               "icb.exercise",
                                                               "bn.total_score")


edgi_bed <- edgi_bed[, c("externalDataReference", 
                         "be.short_period_ate_regard", 
                         "be.stop_eating_binge_eating", 
                         "be.feel_distressed_overeating_episodes", 
                         "be.not_at_all_dependentcompletely_dependent", 
                         "be.experience_regular_episodes_lowest",
                         "be.binge_eating_make_distressed", 
                         "be.during_binges",
                         "bed.total_score")]; names(edgi_bed) <- c("IID",
                                                                   "be.ate_regard_short_period", 
                                                                   "be.regularly_occurring_episodes_binge", 
                                                                   "be.feel_distressed_overeating_episodes", 
                                                                   "be.not_at_all_dependentcompletely_dependent", 
                                                                   "be.regularly_occurring_overeating_episodes", 
                                                                   "be.binge_eating_distressed_make", 
                                                                   "be.during_binges",
                                                                   "bed.total_score")



# let's observe our data #
# stats for an 
summary(edgi_an$an.total_score)
ggplot(edgi_an, aes(x = "", y = an.total_score)) + 
  geom_boxplot(fill = "lightblue", outlier.colour = "red") + 
  labs(title = "Boxplot of AN Total Score")
ggplot(edgi_an, aes(x = an.total_score)) + 
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) + 
  labs(title = "Histogram of AN Total Score")

# stats for bn 
summary(edgi_bn$bn.total_score)
ggplot(edgi_bn, aes(x = "", y = bn.total_score)) + 
  geom_boxplot(fill = "lightblue", outlier.colour = "red") + 
  labs(title = "Boxplot of BN Total Score")
ggplot(edgi_bn, aes(x = bn.total_score)) + 
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) + 
  labs(title = "Histogram of BN Total Score")

# stats for bed 
summary(edgi_bed$bed.total_score)
ggplot(edgi_bed, aes(x = "", y = bed.total_score)) + 
  geom_boxplot(fill = "lightblue", outlier.colour = "red") + 
  labs(title = "Boxplot of BED Total Score")
ggplot(edgi_bed, aes(x = bed.total_score)) + 
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) + 
  labs(title = "Histogram of BED Total Score")

## ADDED! Removing outliers ## 
remove_outliers_iqr_count <- function(df, column) {
  # Compute IQR bounds
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  upper_bound <- Q3 + (1.5 * IQR_value)
  lower_bound <- Q1 - (1.5 * IQR_value)  # Generally not an issue for positive scores
  
  # Identify outliers
  outlier_count <- sum(df[[column]] < lower_bound | df[[column]] > upper_bound, na.rm = TRUE)
  
  # Print cut-off values and outliers removed
  print(paste("For", column, "- Lower Bound:", round(lower_bound, 2), "Upper Bound:", round(upper_bound, 2)))
  print(paste("Outliers removed for", column, ":", outlier_count))
  
  # Filter the dataset to keep only inliers
  df_clean <- df %>% filter(df[[column]] >= lower_bound & df[[column]] <= upper_bound)
  
  # Return cleaned dataset and count of removed outliers
  return(list(clean_data = df_clean, removed_outliers = outlier_count))
}

# Apply outlier removal and count removed values for AN total score
an_outlier_result <- remove_outliers_iqr_count(edgi_an, "an.total_score")
edgi_an <- an_outlier_result$clean_data
an_outliers_removed <- an_outlier_result$removed_outliers

# Apply outlier removal and count removed values for BN total score
bn_outlier_result <- remove_outliers_iqr_count(edgi_bn, "bn.total_score")
edgi_bn <- bn_outlier_result$clean_data
bn_outliers_removed <- bn_outlier_result$removed_outliers

# Apply outlier removal and count removed values for BED total score
bed_outlier_result <- remove_outliers_iqr_count(edgi_bed, "bed.total_score")
edgi_bed <- bed_outlier_result$clean_data
bed_outliers_removed <- bed_outlier_result$removed_outliers

# Create a summary table
outliers_summary <- data.frame(
  Variable = c("AN Total Score", "BN Total Score", "BED Total Score"),
  Outliers_Removed = c(an_outliers_removed, bn_outliers_removed, bed_outliers_removed)
)
print(outliers_summary)

# ========== Combine and add cohort name ==========
## Merge data
edgi_EDs <- merge(merge(edgi_an, edgi_bn, by = "IID", all = TRUE), edgi_bed, by = "IID", all = TRUE)

## Add cohort
edgi_EDs$cohort <- "coping_edgi" 


# ================= VARIABLE 2: TAF EDGI ==========
# ========== Read TAF data and inspect ==========
coping_edgi_taf <- readRDS("data_raw/SH SUI/taf_coping_edgi.rds")
names(coping_edgi_taf)

# ========== Identify duplicates ==========
dup_coping_edgi_taf <- duplicated(coping_edgi_taf$externalDataReference); table(dup_coping_edgi_taf)

# ========== Remove duplicated and incomplete IDs ==========
coping_edgi_taf <- remove_duplicates(coping_edgi_taf, "externalDataReference", date_col = "endDate"); table(duplicated(coping_edgi_taf$externalDataReference))

# ========== Select items (ADDED and REMOVED!!) ==========
## TAF
coping_edgi_taf <- coping_edgi_taf[, c("externalDataReference", 
                                       "taf.worth_living_thoughts_life",
                                       "taf.past_weeks_felt", 
                                       "taf.past_weeks_felt.1",
                                       "taf.contemplated_harming",
                                       "taf.meant_end_weeks_life")]


# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
coping_edgi_taf <- coping_edgi_taf %>%
  mutate_all(~ ifelse(. < 0, NA, .))

# ========== Unify variables and variable names across datasets: match to var names in glad ==========
coping_edgi_taf <- coping_edgi_taf[, c("externalDataReference", 
                                       "taf.worth_living_thoughts_life",
                                       "taf.past_weeks_felt", 
                                       "taf.past_weeks_felt.1",
                                       "taf.contemplated_harming",
                                       "taf.meant_end_weeks_life")]


# Rename variables for consistency
names(coping_edgi_taf) <- c("IID", 
                            "taf.worth_living_life_thoughts",
                            "taf.past_felt_weeks", 
                            "taf.past_felt_weeks.1",
                            "taf.have_you_contemplated_harming_yourself_",
                            "taf.meant_end_life_weeks")

# ========== Add cohort name ==========
coping_edgi_taf$cohort <- "coping_edgi"


# ================= VARIABLE 3: PANWORRY EDGI==========
# ========== Read PANWORRY data and inspect ==========
coping_edgi_panworry <- readRDS("data_raw/Anx/panworry_coping_edgi.rds") #CHANGE THIS 

names(coping_edgi_panworry)
# ========== Identify duplicates ==========
dup_coping_edgi_panworry <- duplicated(coping_edgi_panworry$externalDataReference)
table(dup_coping_edgi_panworry)

# ========== Remove duplicated and incomplete IDs ==========
coping_edgi_panworry <- remove_duplicates(coping_edgi_panworry, "externalDataReference", date_col = "endDate")
table(duplicated(coping_edgi_panworry$externalDataReference))  # Check if duplicates are removed

# ========== Select items (ADDED and REMOVED!!) ==========
## Panworry
coping_edgi_panworry <- coping_edgi_panworry[, c("externalDataReference", 
                                                 "panworry.impact_on_your_employment_status",             
                                                 "panworry.employment_status_household_key",              
                                                 "panworry.impact_on_your_education_or_exams",            
                                                 "panworry.exams_education_impact_children",               
                                                 "panworry.financial_impact",                              
                                                 "panworry.contracting_the_virus",                         
                                                 "panworry.people_you_know_contracting_the_virus",         
                                                 "panworry.people_you_dont_know_contracting_the_virus",    
                                                 "panworry.wellbeing_mental_health_impact",                
                                                 "panworry.wellbeing_childrens_mental_health",             
                                                 "panworry.relatives_mental_health_impact",                
                                                 "panworry.being_socially_isolated",                       
                                                 "panworry.people_you_know_being_socially_isolated",       
                                                 "panworry.shortage_of_essential_supplies_",               
                                                 "panworry.shortage_of_medication_or_access_to_healthcare",
                                                 "panworry.healthcare_shortage_people_essential",          
                                                 "panworry.virus_government_information_accuracy",         
                                                 "panworry.the_governments_response_to_the_pandemic",      
                                                 "panworry.family_members_separation",                     
                                                 "panworry.global_recession_economy_longstanding")]


# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
coping_edgi_panworry <- coping_edgi_panworry %>%
  mutate_all(~ ifelse(. < 0, NA, .))

# ========== Unify variables and variable names across datasets: match to var names in glad ==========
coping_edgi_panworry <- coping_edgi_panworry[, c("externalDataReference", 
                                                 "panworry.impact_on_your_employment_status",             
                                                 "panworry.employment_status_household_key",              
                                                 "panworry.impact_on_your_education_or_exams",            
                                                 "panworry.exams_education_impact_children",               
                                                 "panworry.financial_impact",                              
                                                 "panworry.contracting_the_virus",                         
                                                 "panworry.people_you_know_contracting_the_virus",         
                                                 "panworry.people_you_dont_know_contracting_the_virus",    
                                                 "panworry.wellbeing_mental_health_impact",                
                                                 "panworry.wellbeing_childrens_mental_health",             
                                                 "panworry.relatives_mental_health_impact",                
                                                 "panworry.being_socially_isolated",                       
                                                 "panworry.people_you_know_being_socially_isolated",       
                                                 "panworry.shortage_of_essential_supplies_",               
                                                 "panworry.shortage_of_medication_or_access_to_healthcare",
                                                 "panworry.healthcare_shortage_people_essential",          
                                                 "panworry.virus_government_information_accuracy",         
                                                 "panworry.the_governments_response_to_the_pandemic",      
                                                 "panworry.family_members_separation",                     
                                                 "panworry.global_recession_economy_longstanding")]


# Rename variables for consistency
names(coping_edgi_panworry) <- c("IID", 
                                 "panworry.impact_on_your_employment_status",             
                                 "panworry.employment_status_household_key",              
                                 "panworry.impact_on_your_education_or_exams",            
                                 "panworry.exams_education_impact_children",               
                                 "panworry.financial_impact",                              
                                 "panworry.contracting_the_virus",                         
                                 "panworry.people_you_know_contracting_the_virus",         
                                 "panworry.people_you_dont_know_contracting_the_virus",    
                                 "panworry.wellbeing_mental_health_impact",                
                                 "panworry.wellbeing_childrens_mental_health",             
                                 "panworry.relatives_mental_health_impact",                
                                 "panworry.being_socially_isolated",                       
                                 "panworry.people_you_know_being_socially_isolated",       
                                 "panworry.shortage_of_essential_supplies_",               
                                 "panworry.shortage_of_medication_or_access_to_healthcare",
                                 "panworry.healthcare_shortage_people_essential",          
                                 "panworry.virus_government_information_accuracy",         
                                 "panworry.the_governments_response_to_the_pandemic",      
                                 "panworry.family_members_separation",                     
                                 "panworry.global_recession_economy_longstanding")




# ========== Add cohort name ==========
coping_edgi_panworry$cohort <- "coping_edgi"


# ================= VARIABLE 4 + 5: Psychopathology EDGI ==========
# ========== Read psychopathology data and inspect ==========
coping_edgi_phq9 <- readRDS("data_raw/Depr/phq_coping_edgi.rds") 
coping_edgi_gad7 <- readRDS("data_raw/Anx/gad7_coping_edgi.rds") 

names(coping_edgi_phq9)
names(coping_edgi_gad7)

# ========== Identify duplicates ==========
dup_coping_edgi_phq9 <- duplicated(coping_edgi_phq9$externalDataReference); table(dup_coping_edgi_phq9)
dup_coping_edgi_gad7 <- duplicated(coping_edgi_gad7$externalDataReference); table(dup_coping_edgi_gad7)

# ========== Remove duplicated and incomplete IDs ==========
coping_edgi_phq9 <- remove_duplicates(coping_edgi_phq9, "externalDataReference", date_col = "endDate"); table(duplicated(coping_edgi_phq9$externalDataReference))
coping_edgi_gad7 <- remove_duplicates(coping_edgi_gad7, "externalDataReference", date_col = "endDate"); table(duplicated(coping_edgi_gad7$externalDataReference))

# ========== Select items ==========
## Depression (ADDED!)
coping_edgi_phq9 <- coping_edgi_phq9[, c("externalDataReference", 
                                         "phq9.little_interest_or_pleasure_in_doing_things",
                                         "phq9.feeling_down_depressed_or_hopeless",        
                                         "phq9.staying_asleep_sleeping_trouble",            
                                         "phq9.feeling_tired_or_having_little_energy",     
                                         "phq9.poor_appetite_or_overeating",               
                                         "phq9.feeling_bad_failure_family",               
                                         "phq9.trouble_concentrating_reading_newspaper",   
                                         "phq9.moving_fidgety_noticed_opposite",          
                                         "phq9.dead_hurting_thoughts",                  
                                         "phq9.care_difficult_home_things")]

## Anxiety (ADDED!)
coping_edgi_gad7 <- coping_edgi_gad7[, c("externalDataReference", 
                                         "gad7.feeling_nervous_anxious_or_on_edge",
                                         "gad7.control_worrying_stop",                     
                                         "gad7.worrying_too_much_about_different_things",  
                                         "gad7.trouble_relaxing",                          
                                         "gad7.sit_restless_hard",                       
                                         "gad7.becoming_easily_annoyed_or_irritable",      
                                         "gad7.awful_feeling_afraid_happen",               
                                         "gad7.care_difficult_home_things")]

# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
coping_edgi_phq9 <- coping_edgi_phq9 %>%
  mutate_all(~ ifelse(. < 0, NA, .))

coping_edgi_gad7 <- coping_edgi_gad7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))

# ========== Create overall symptom scales ==========
## PHQ9 Total Score (Summed Symptoms)
coping_edgi_phq9$phq9.total_score <- my_rowSums(coping_edgi_phq9[,2:11])

## GAD7 Total Score (Summed Symptoms)
coping_edgi_gad7$gad7.total_score <- my_rowSums(coping_edgi_gad7[,2:9])

# ========== Standardize Variable Naming for Merging ==========
coping_edgi_phq9 <- coping_edgi_phq9[, c("externalDataReference", 
                                         "phq9.little_interest_or_pleasure_in_doing_things",
                                         "phq9.feeling_down_depressed_or_hopeless",        
                                         "phq9.staying_asleep_sleeping_trouble",            
                                         "phq9.feeling_tired_or_having_little_energy",     
                                         "phq9.poor_appetite_or_overeating",               
                                         "phq9.feeling_bad_failure_family",               
                                         "phq9.trouble_concentrating_reading_newspaper",   
                                         "phq9.moving_fidgety_noticed_opposite",          
                                         "phq9.dead_hurting_thoughts",                  
                                         "phq9.care_difficult_home_things",
                                         "phq9.total_score")]; names(coping_edgi_phq9) <- c("IID",
                                                                                            "phq9.little_interest_or_pleasure_in_doing_things", 
                                                                                            "phq9.feeling_down_depressed_or_hopeless", 
                                                                                            "phq9.staying_asleep_sleeping_trouble", 
                                                                                            "phq9.feeling_tired_or_having_little_energy", 
                                                                                            "phq9.poor_appetite_or_overeating", 
                                                                                            "phq9.feeling_bad_failure_family", 
                                                                                            "phq9.trouble_concentrating_newspaper_reading", 
                                                                                            "phq9.moving_fidgety_opposite_slowly", 
                                                                                            "phq9.dead_hurting_thoughts",
                                                                                            "phq9.problems_made_difficult_care",
                                                                                            "phq9.total_score")


coping_edgi_gad7 <- coping_edgi_gad7[, c("externalDataReference", 
                                         "gad7.feeling_nervous_anxious_or_on_edge",
                                         "gad7.control_worrying_stop",                     
                                         "gad7.worrying_too_much_about_different_things",  
                                         "gad7.trouble_relaxing",                          
                                         "gad7.sit_restless_hard",                       
                                         "gad7.becoming_easily_annoyed_or_irritable",      
                                         "gad7.awful_feeling_afraid_happen",               
                                         "gad7.care_difficult_home_things",
                                         "gad7.total_score")]; names(coping_edgi_gad7) <- c("IID",
                                                                                            "gad7.feeling_nervous_anxious_or_on_edge", 
                                                                                            "gad7.control_worrying_stop", 
                                                                                            "gad7.worrying_too_much_about_different_things", 
                                                                                            "gad7.trouble_relaxing", 
                                                                                            "gad7.sit_restless_hard", 
                                                                                            "gad7.becoming_easily_annoyed_or_irritable",
                                                                                            "gad7.awful_feeling_afraid_happen",
                                                                                            "gad7.problems_made_difficult_care",
                                                                                            "gad7.total_score")

# ========== Merge PHQ9 and GAD7 Data ==========
coping_edgi_psychopathology <- merge(coping_edgi_phq9, coping_edgi_gad7, by = "IID", all = TRUE)

# ========== Add Cohort Name ==========
coping_edgi_psychopathology$cohort <- "coping_edgi"







                                    ######## COHORT 3: NBR ####### 
# ========== Read clean dem data and inspect ==========
coping_nbr_dem_age <- readRDS("data/demographics - coping_glad_edgi_nbr/age_coping_glad_edgi_nbr_clean.rds")
coping_nbr_dem_ethnicity <- readRDS("data/ethnicity_coping_nbr_clean.rds")
coping_nbr_dem_anthropo <- readRDS("data/demographics - coping_glad_edgi_nbr/signup_bmi_height_weight_coping_glad_edgi_nbr_clean.rds")
coping_nbr_dem_gender <- readRDS("data/demographics - coping_glad_edgi_nbr/sex_gender_sexuality_coping_glad_edgi_nbr_clean.rds")

names(coping_nbr_dem_age)
names(coping_nbr_dem_ethnicity)
names(coping_nbr_dem_anthropo)
names(coping_nbr_dem_gender)

# ========== Select cohort and items ==========
## Cohort
coping_nbr_dem_age <- coping_nbr_dem_age[coping_nbr_dem_age$sample == "NBR",]
coping_nbr_dem_ethnicity <- coping_nbr_dem_ethnicity[coping_nbr_dem_ethnicity$sample == "NBR",]
coping_nbr_dem_anthropo <- coping_nbr_dem_anthropo[coping_nbr_dem_anthropo$sample == "NBR",]
coping_nbr_dem_gender <- coping_nbr_dem_gender[coping_nbr_dem_gender$sample == "NBR",]

## Items
coping_nbr_dem_age <- coping_nbr_dem_age[, c("ID", 
                                             "dem.dob_age_cop")]

coping_nbr_dem_ethnicity <- coping_nbr_dem_ethnicity[, c("ID",
                                                         "dem.what_is_your_ethnic_origin_cop")]

coping_nbr_dem_anthropo <- coping_nbr_dem_anthropo[, c("ID", 
                                                       "dem.height_signup_cm_cop", 
                                                       "dem.weight_signup_kg_cop", 
                                                       "dem.bmi_signup_cop")]

coping_nbr_dem_gender <- coping_nbr_dem_gender[, c("ID", 
                                                   "dem.sex_cop", 
                                                   "dem.which_gender_do_you_identify_with_cop", 
                                                   "dem.sex_cop_numeric", 
                                                   "dem.which_gender_do_you_identify_with_cop_numeric", 
                                                   "dem.do_you_identify_as_transgender_cop", 
                                                   "dem.what_is_your_sexual_orientation_cop", 
                                                   "dem.do_you_identify_as_transgender_cop_numeric", 
                                                   "dem.what_is_your_sexual_orientation_cop_numeric")]

# ========== Combine and add cohort name ==========
## Merge data
coping_nbr_clean_dem <- merge(merge(merge(coping_nbr_dem_age, coping_nbr_dem_anthropo, by = "ID", all = TRUE), coping_nbr_dem_ethnicity, by = "ID", all.x = TRUE), coping_nbr_dem_gender, by = "ID", all = TRUE)

## Add cohort
coping_nbr_clean_dem$cohort <- "coping_nbr"

# ========== Unify variables and variable names across datasets: match to var names in glad ==========
coping_nbr_clean_dem <- coping_nbr_clean_dem[, c("ID", 
                                                 "dem.dob_age_cop", 
                                                 "dem.height_signup_cm_cop", 
                                                 "dem.weight_signup_kg_cop", 
                                                 "dem.bmi_signup_cop", 
                                                 "dem.what_is_your_ethnic_origin_cop", 
                                                 "dem.sex_cop", 
                                                 "dem.sex_cop_numeric",
                                                 "cohort")]; names(coping_nbr_clean_dem) <- c("IID", 
                                                                                              "dem.dob_age_cop", 
                                                                                              "dem.height_signup_cm_cop", 
                                                                                              "dem.weight_signup_kg_cop", 
                                                                                              "dem.bmi_signup_cop", 
                                                                                              "dem.what_is_your_ethnic_origin", 
                                                                                              "dem.sex_cop", 
                                                                                              "dem.sex_cop_numeric",
                                                                                              "cohort")

# ================= EDs ==========
# ========== Read ED data and inspect ==========
coping_nbr_an <- readRDS("data_raw/EDs/an_coping_nbr.rds")
coping_nbr_bn <- readRDS("data_raw/EDs/icb_coping_nbr.rds")
coping_nbr_bed <- readRDS("data_raw/EDs/be_coping_nbr.rds")

names(coping_nbr_an)
names(coping_nbr_bn)
names(coping_nbr_bed)

# ========== Identify duplicates ==========
dup_coping_nbr_an <- duplicated(coping_nbr_an$subjectid); table(dup_coping_nbr_an)
dup_coping_nbr_bn <- duplicated(coping_nbr_bn$subjectid); table(dup_coping_nbr_bn)
dup_coping_nbr_bed <- duplicated(coping_nbr_bed$subjectid); table(dup_coping_nbr_bed)

# ========== Remove duplicated and incomplete IDs ==========
coping_nbr_an <- remove_duplicates(coping_nbr_an, "subjectid", date_col = "endDate"); table(duplicated(coping_nbr_an$subjectid))
coping_nbr_bn <- remove_duplicates(coping_nbr_bn, "subjectid", date_col = "endDate"); table(duplicated(coping_nbr_bn$subjectid))
coping_nbr_bed <- remove_duplicates(coping_nbr_bed, "subjectid", date_col = "endDate"); table(duplicated(coping_nbr_bed$subjectid))

# ========== Select items ==========
## AN
coping_nbr_an <- coping_nbr_an[, c("subjectid", 
                                   "an.1.lowest_weight_weigh_weighed", 
                                   "an.1.gain_weight_low_weight", 
                                   "an.1.not_at_all_dependentcompletely_dependent", 
                                   "an.1.low_weight_health_negative", 
                                   "an.1.feel_fat_low_weight",
                                   "an.1.body_larger_people_thought")]

## BN
coping_nbr_bn <- coping_nbr_bn[, c("subjectid", 
                                   "icb.body_shape_control_weight.fasted_or_did_not_eat_for_8_waking_hours_or_more", 
                                   "icb.body_shape_control_weight.used_diet_pills_over_the_counter_or_prescription", 
                                   "icb.body_shape_control_weight.exercised_excessively__e.g._felt_compelled_to_exercise_felt_uneasy_or_distressed_if_unable_to_exercise", 
                                   "icb.body_shape_control_weight.made_yourself_vomit", 
                                   "icb.body_shape_control_weight.used_laxatives_including_pills_or_liquids_meant_to_stimulate_bowel_movements", 
                                   "icb.body_shape_control_weight.used_diuretics_water_pills", 
                                   "icb.body_shape_felt_compelled", 
                                   "icb.felt_uneasy_unable_distressed", 
                                   "icb.order_friends_exercise_times", 
                                   "icb.prevented_injury_illness_exercised", 
                                   "icb.making_yourself_vomit", 
                                   "icb.laxatives", 
                                   "icb.diuretics", 
                                   "icb.weight_loss_pills", 
                                   "icb.excessive_exercise", 
                                   "icb.fasting", 
                                   "icb.other_methods", 
                                   "icb.none", 
                                   "icb.making_yourself_vomit.2", 
                                   "icb.laxatives.2",
                                   "icb.diuretics_", 
                                   "icb.weight_loss_pills.2", 
                                   "icb.excessive_exercise.1",
                                   "icb.fasting.2", 
                                   "icb.other_methods.2", 
                                   "icb.none_of_the_above",
                                   "icb.modified_reason_unable_exercise")]

## BED
coping_nbr_bed <- coping_nbr_bed[, c("subjectid", 
                                     "be.ate_regard_short_period", 
                                     "be.regularly_occurring_episodes_binge", 
                                     "be.overeating_feel_distressed_episodes", 
                                     "be.not_at_all_dependentcompletely_dependent", 
                                     "be.regularly_occurring_overeating_episodes", 
                                     "be.binge_eating_distressed_make", 
                                     "be.during_eating_binges_did_you__.eat_much_more_rapidly_than_usual", 
                                     "be.during_eating_binges_did_you__.eat_until_you_felt_uncomfortably_full", 
                                     "be.during_eating_binges_did_you__.eat_large_amounts_of_food_when_you_didnt_feel_physically_hungry", 
                                     "be.during_eating_binges_did_you__.eat_alone_because_you_were_embarrassed_by_whathow_much_you_were_eating", 
                                     "be.during_eating_binges_did_you__.feel_ashameddisgusted_with_yourself_depressed_or_very_guilty_after_overeating", 
                                     "be.during_eating_binges_did_you__.feel_like_you_had_no_control_over_your_eating_e.g._not_being_able_to_stop_eating_feeling_compelled_to_eat_or_going_back_and_forth_for_more_food", 
                                     "be.during_eating_binges_did_you__.make_yourself_vomit_as_a_means_to_control_your_weight_and_shape")]

# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
coping_nbr_an <- coping_nbr_an %>%
  mutate_all(~ ifelse(. < 0, NA, .))

coping_nbr_bn <- coping_nbr_bn %>%
  mutate_all(~ ifelse(. < 0, NA, .))

coping_nbr_bed <- coping_nbr_bed %>%
  mutate_all(~ ifelse(. < 0, NA, .))

# ========== Create symptom sub-scales ==========
## Variables
# BN
coping_nbr_bn$icb.weight_control <- my_rowSums(coping_nbr_bn[, 2:7])
coping_nbr_bn$icb.lowest_weight_control_shape <- my_rowSums(coping_nbr_bn[, 12:19])
coping_nbr_bn$icb.compensate <- my_rowSums(coping_nbr_bn[, 20:27])
coping_nbr_bn$icb.exercise <- my_rowSums(coping_nbr_bn[, c(8:11, 28)])

# BED
coping_nbr_bed$be.during_binges <- my_rowSums(coping_nbr_bed[, 8:14])

# ========== Create overall symptom scales ==========
## Flip AN item: an.lowest_weight_people_thought
coping_nbr_an$an.1.lowest_weight_weigh_weighed <- coping_nbr_an$an.1.lowest_weight_weigh_weighed * -1

## Variables
# AN
coping_nbr_an$an.total_score <- my_rowSums(coping_nbr_an[,2:7])

# BN
coping_nbr_bn$bn.total_score <- my_rowSums(coping_nbr_bn[,2:28])

# BED
coping_nbr_bed$bed.total_score <- my_rowSums(coping_nbr_bed[,2:14])

# ========== Unify variables and variable names across datasets: match to var names in glad ==========
coping_nbr_an <- coping_nbr_an[, c("subjectid", 
                                   "an.1.lowest_weight_weigh_weighed", 
                                   "an.1.gain_weight_low_weight", 
                                   "an.1.not_at_all_dependentcompletely_dependent", 
                                   "an.1.low_weight_health_negative", 
                                   "an.1.feel_fat_low_weight",
                                   "an.1.body_larger_people_thought",
                                   "an.total_score")]; names(coping_nbr_an) <- c("IID", 
                                                                                 "an.lowest_weight_people_thought", 
                                                                                 "an.gain_weight_afraid_fat", 
                                                                                 "an.not_at_all_dependentcompletely_dependent", 
                                                                                 "an.health_low_weightbmi_negative", 
                                                                                 "an.feel_fat_time_low",
                                                                                 "an.people_thought_larger_parts",
                                                                                 "an.total_score")

coping_nbr_bn <- coping_nbr_bn[, c("subjectid", "icb.weight_control", 
                                   "icb.lowest_weight_control_shape", 
                                   "icb.compensate", 
                                   "icb.exercise",
                                   "bn.total_score")]; names(coping_nbr_bn) <- c("IID",
                                                                                 "icb.weight_control", 
                                                                                 "icb.lowest_weight_control_shape", 
                                                                                 "icb.compensate", 
                                                                                 "icb.exercise",
                                                                                 "bn.total_score")


coping_nbr_bed <- coping_nbr_bed[, c("subjectid", 
                                     "be.ate_regard_short_period", 
                                     "be.regularly_occurring_episodes_binge", 
                                     "be.overeating_feel_distressed_episodes", 
                                     "be.not_at_all_dependentcompletely_dependent", 
                                     "be.regularly_occurring_overeating_episodes", 
                                     "be.binge_eating_distressed_make", 
                                     "be.during_binges",
                                     "bed.total_score")]; names(coping_nbr_bed) <- c("IID",
                                                                                     "be.ate_regard_short_period", 
                                                                                     "be.regularly_occurring_episodes_binge", 
                                                                                     "be.feel_distressed_overeating_episodes", 
                                                                                     "be.not_at_all_dependentcompletely_dependent", 
                                                                                     "be.regularly_occurring_overeating_episodes", 
                                                                                     "be.binge_eating_distressed_make", 
                                                                                     "be.during_binges",
                                                                                     "bed.total_score")



## ADDED! Removing outliers ## 
remove_outliers_iqr_count <- function(df, column) {
  # Compute IQR bounds
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  upper_bound <- Q3 + (1.5 * IQR_value)
  lower_bound <- Q1 - (1.5 * IQR_value)  # Generally not an issue for positive scores
  
  # Identify outliers
  outlier_count <- sum(df[[column]] < lower_bound | df[[column]] > upper_bound, na.rm = TRUE)
  
  # Print cut-off values and outliers removed
  print(paste("For", column, "- Lower Bound:", round(lower_bound, 2), "Upper Bound:", round(upper_bound, 2)))
  print(paste("Outliers removed for", column, ":", outlier_count))
  
  # Filter the dataset to keep only inliers
  df_clean <- df %>% filter(df[[column]] >= lower_bound & df[[column]] <= upper_bound)
  
  # Return cleaned dataset and count of removed outliers
  return(list(clean_data = df_clean, removed_outliers = outlier_count))
}

# Apply outlier removal and count removed values for AN total score
an_outlier_result <- remove_outliers_iqr_count(edgi_an, "an.total_score")
edgi_an <- an_outlier_result$clean_data
an_outliers_removed <- an_outlier_result$removed_outliers

# Apply outlier removal and count removed values for BN total score
bn_outlier_result <- remove_outliers_iqr_count(edgi_bn, "bn.total_score")
edgi_bn <- bn_outlier_result$clean_data
bn_outliers_removed <- bn_outlier_result$removed_outliers

# Apply outlier removal and count removed values for BED total score
bed_outlier_result <- remove_outliers_iqr_count(edgi_bed, "bed.total_score")
edgi_bed <- bed_outlier_result$clean_data
bed_outliers_removed <- bed_outlier_result$removed_outliers

# Create a summary table
outliers_summary <- data.frame(
  Variable = c("AN Total Score", "BN Total Score", "BED Total Score"),
  Outliers_Removed = c(an_outliers_removed, bn_outliers_removed, bed_outliers_removed)
)
print(outliers_summary)


# ========== Combine and add cohort name ==========
## Merge data
coping_nbr_EDs <- merge(merge(coping_nbr_an, coping_nbr_bn, by = "IID", all = TRUE), coping_nbr_bed, by = "IID", all = TRUE)

## Add cohort
coping_nbr_EDs$cohort <- "coping_nbr"



# ================= VARIABLE 2: TAF NBR ==========
# ========== Read TAF data and inspect ==========
coping_nbr_taf <- readRDS("data_raw/SH SUI/taf_coping_nbr.rds")

names(coping_nbr_taf)

# ========== Identify duplicates ==========
dup_coping_nbr_taf <- duplicated(coping_nbr_taf$subjectid); table(dup_coping_nbr_taf)

# ========== Remove duplicated and incomplete IDs ==========
coping_nbr_taf <- remove_duplicates(coping_nbr_taf, "subjectid", date_col = "endDate"); table(duplicated(coping_nbr_taf$subjectid))

# ========== Select items ==========
## TAF
coping_nbr_taf <- coping_nbr_taf[, c("subjectid", 
                                       "taf.worth_living_thoughts_life",
                                       "taf.past_felt_weeks", 
                                       "taf.past_felt_weeks.1",
                                       "taf.have_you_contemplated_harming_yourself_",
                                       "taf.meant_end_life_weeks")]


# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
coping_nbr_taf <- coping_nbr_taf %>%
  mutate_all(~ ifelse(. < 0, NA, .))

# ========== Unify variables and variable names across datasets: match to var names in glad ==========
coping_nbr_taf <- coping_nbr_taf[, c("subjectid", 
                                     "taf.worth_living_thoughts_life",
                                     "taf.past_felt_weeks", 
                                     "taf.past_felt_weeks.1",
                                     "taf.have_you_contemplated_harming_yourself_",
                                     "taf.meant_end_life_weeks")]


# Rename variables for consistency
names(coping_nbr_taf) <- c("IID", 
                            "taf.worth_living_life_thoughts",
                            "taf.past_felt_weeks", 
                            "taf.past_felt_weeks.1",
                            "taf.have_you_contemplated_harming_yourself_",
                            "taf.meant_end_life_weeks")

# ========== Add cohort name ==========
## Add cohort
coping_nbr_taf$cohort <- "coping_nbr"


# ================= VARIABLE 3: PANWORRY NBR ==========
# ========== Read PANWORRY data and inspect ==========
coping_nbr_panworry <- readRDS("data_raw/Anx/panworry_coping_nbr.rds") 

names(coping_nbr_panworry)
# ========== Identify duplicates ==========
dup_coping_nbr_panworry <- duplicated(coping_nbr_panworry$subjectid)
table(dup_coping_nbr_panworry)

# ========== Remove duplicated and incomplete IDs ==========
coping_nbr_panworry <- remove_duplicates(coping_nbr_panworry, "subjectid", date_col = "endDate")
table(duplicated(coping_nbr_panworry$subjectid))  # Check if duplicates are removed

# ========== Select items (ADDED and REMOVED!!) ==========
## Panworry
coping_nbr_panworry <- coping_nbr_panworry[, c("subjectid", 
                                              "panworry.impact_on_your_employment_status",              
                                               "panworry.employment_status_household_key",               
                                               "panworry.impact_on_your_education_or_exams",             
                                               "panworry.exams_education_impact_children",               
                                               "panworry.financial_impact",                              
                                               "panworry.contracting_the_virus" ,                        
                                               "panworry.people_you_know_contracting_the_virus",         
                                               "panworry.people_you_dont_know_contracting_the_virus",    
                                               "panworry.wellbeing_impact_mental_health",                
                                               "panworry.wellbeing_childrens_mental_health",             
                                               "panworry.relatives_impact_mental_health",                
                                               "panworry.being_socially_isolated",                       
                                               "panworry.people_you_know_being_socially_isolated",       
                                               "panworry.shortage_of_essential_supplies_",               
                                               "panworry.shortage_of_medication_or_access_to_healthcare",
                                               "panworry.healthcare_shortage_people_essential",          
                                               "panworry.virus_government_information_accuracy",         
                                               "panworry.the_governments_response_to_the_pandemic",      
                                               "panworry.family_members_separation",                     
                                               "panworry.global_recession_economy_longstanding")]


# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
coping_nbr_panworry <- coping_nbr_panworry %>%
  mutate_all(~ ifelse(. < 0, NA, .))

# ========== Unify variables and variable names across datasets: match to var names in glad ==========
coping_nbr_panworry <- coping_nbr_panworry[, c("subjectid", 
                                               "panworry.impact_on_your_employment_status",              
                                               "panworry.employment_status_household_key",               
                                               "panworry.impact_on_your_education_or_exams",             
                                               "panworry.exams_education_impact_children",               
                                               "panworry.financial_impact",                              
                                               "panworry.contracting_the_virus" ,                        
                                               "panworry.people_you_know_contracting_the_virus",         
                                               "panworry.people_you_dont_know_contracting_the_virus",    
                                               "panworry.wellbeing_impact_mental_health",                
                                               "panworry.wellbeing_childrens_mental_health",             
                                               "panworry.relatives_impact_mental_health",                
                                               "panworry.being_socially_isolated",                       
                                               "panworry.people_you_know_being_socially_isolated",       
                                               "panworry.shortage_of_essential_supplies_",               
                                               "panworry.shortage_of_medication_or_access_to_healthcare",
                                               "panworry.healthcare_shortage_people_essential",          
                                               "panworry.virus_government_information_accuracy",         
                                               "panworry.the_governments_response_to_the_pandemic",      
                                               "panworry.family_members_separation",                     
                                               "panworry.global_recession_economy_longstanding")]


# Rename variables for consistency
names(coping_nbr_panworry) <- c("IID", 
                                 "panworry.impact_on_your_employment_status",             
                                 "panworry.employment_status_household_key",              
                                 "panworry.impact_on_your_education_or_exams",            
                                 "panworry.exams_education_impact_children",               
                                 "panworry.financial_impact",                              
                                 "panworry.contracting_the_virus",                         
                                 "panworry.people_you_know_contracting_the_virus",         
                                 "panworry.people_you_dont_know_contracting_the_virus",    
                                 "panworry.wellbeing_mental_health_impact",                
                                 "panworry.wellbeing_childrens_mental_health",             
                                 "panworry.relatives_mental_health_impact",                
                                 "panworry.being_socially_isolated",                       
                                 "panworry.people_you_know_being_socially_isolated",       
                                 "panworry.shortage_of_essential_supplies_",               
                                 "panworry.shortage_of_medication_or_access_to_healthcare",
                                 "panworry.healthcare_shortage_people_essential",          
                                 "panworry.virus_government_information_accuracy",         
                                 "panworry.the_governments_response_to_the_pandemic",      
                                 "panworry.family_members_separation",                     
                                 "panworry.global_recession_economy_longstanding")


# ========== Add cohort name ==========
coping_nbr_panworry$cohort <- "coping_nbr"



# ================= VARIABLE 4 + 5: Psychopathology NBR ==========
# ========== Read psychopathology data and inspect ==========
coping_nbr_phq9 <- readRDS("data_raw/Depr/phq9_coping_nbr.rds") 
coping_nbr_gad7 <- readRDS("data_raw/Anx/gad7_coping_nbr.rds") 

names(coping_nbr_phq9)
names(coping_nbr_gad7)

# ========== Identify duplicates ==========
dup_coping_nbr_phq9 <- duplicated(coping_nbr_phq9$subjectid); table(dup_coping_nbr_phq9)
dup_coping_nbr_gad7 <- duplicated(coping_nbr_gad7$subjectid); table(dup_coping_nbr_gad7)

# ========== Remove duplicated and incomplete IDs ==========
coping_nbr_phq9 <- remove_duplicates(coping_nbr_phq9, "subjectid", date_col = "endDate"); table(duplicated(coping_nbr_phq9$subjectid))
coping_nbr_gad7 <- remove_duplicates(coping_nbr_gad7, "subjectid", date_col = "endDate"); table(duplicated(coping_nbr_gad7$subjectid))

# ========== Select items ==========
## Depression (ADDED!)
coping_nbr_phq9 <- coping_nbr_phq9[, c("subjectid", 
                                       "phq9.little_interest_or_pleasure_in_doing_things",  
                                       "phq9.feeling_down_depressed_or_hopeless",           
                                       "phq9.staying_asleep_sleeping_trouble" ,             
                                       "phq9.feeling_tired_or_having_little_energy",        
                                       "phq9.poor_appetite_or_overeating" ,                 
                                       "phq9.feeling_bad_failure_family",                   
                                       "phq9.trouble_concentrating_reading_newspaper" ,     
                                       "phq9.moving_fidgety_noticed_opposite"  ,            
                                       "phq9.dead_hurting_thoughts" ,                       
                                       "phq9.problems_made_care_difficult")]

## Anxiety (ADDED!)
coping_nbr_gad7 <- coping_nbr_gad7[, c("subjectid", 
                                       "gad7.feeling_nervous_anxious_or_on_edge" ,       
                                       "gad7.control_worrying_stop"          ,           
                                       "gad7.worrying_too_much_about_different_things" , 
                                       "gad7.trouble_relaxing"  ,                        
                                       "gad7.sit_restless_hard" ,                        
                                       "gad7.becoming_easily_annoyed_or_irritable" ,     
                                       "gad7.awful_feeling_afraid_happen"  ,             
                                       "gad7.problems_made_care_difficult")]

# ========== Data properties ==========
## Structure and recode items, discarding responses like "Don't know"/"Prefer not to say", which are coded -777/-88
# Filter data to retain only columns with values >= 0
coping_nbr_phq9 <- coping_nbr_phq9 %>%
  mutate_all(~ ifelse(. < 0, NA, .))

coping_nbr_gad7 <- coping_nbr_gad7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))


# ========== Create overall symptom scales ==========
## PHQ9 Total Score (Summed Symptoms)
coping_nbr_phq9$phq9.total_score <- my_rowSums(coping_nbr_phq9[,2:11])

## GAD7 Total Score (Summed Symptoms)
coping_nbr_gad7$gad7.total_score <- my_rowSums(coping_nbr_gad7[,2:9])

# ========== Standardize Variable Naming for Merging ==========
coping_nbr_phq9 <- coping_nbr_phq9[, c("subjectid", 
                                       "phq9.little_interest_or_pleasure_in_doing_things",  
                                       "phq9.feeling_down_depressed_or_hopeless",           
                                       "phq9.staying_asleep_sleeping_trouble" ,             
                                       "phq9.feeling_tired_or_having_little_energy",        
                                       "phq9.poor_appetite_or_overeating" ,                 
                                       "phq9.feeling_bad_failure_family",                   
                                       "phq9.trouble_concentrating_reading_newspaper" ,     
                                       "phq9.moving_fidgety_noticed_opposite"  ,            
                                       "phq9.dead_hurting_thoughts" ,                       
                                       "phq9.problems_made_care_difficult",
                                         "phq9.total_score")]; names(coping_nbr_phq9) <- c("IID",
                                                                                            "phq9.little_interest_or_pleasure_in_doing_things", 
                                                                                            "phq9.feeling_down_depressed_or_hopeless", 
                                                                                            "phq9.staying_asleep_sleeping_trouble", 
                                                                                            "phq9.feeling_tired_or_having_little_energy", 
                                                                                            "phq9.poor_appetite_or_overeating", 
                                                                                            "phq9.feeling_bad_failure_family", 
                                                                                            "phq9.trouble_concentrating_newspaper_reading", 
                                                                                            "phq9.moving_fidgety_opposite_slowly", 
                                                                                            "phq9.dead_hurting_thoughts",
                                                                                            "phq9.problems_made_difficult_care",
                                                                                            "phq9.total_score")


coping_nbr_gad7 <- coping_nbr_gad7[, c( "subjectid", 
                                         "gad7.feeling_nervous_anxious_or_on_edge" ,       
                                         "gad7.control_worrying_stop"          ,           
                                         "gad7.worrying_too_much_about_different_things" , 
                                         "gad7.trouble_relaxing"  ,                        
                                         "gad7.sit_restless_hard" ,                        
                                         "gad7.becoming_easily_annoyed_or_irritable" ,     
                                         "gad7.awful_feeling_afraid_happen"  ,             
                                         "gad7.problems_made_care_difficult",
                                         "gad7.total_score")]; names(coping_nbr_gad7) <- c("IID",
                                                                                            "gad7.feeling_nervous_anxious_or_on_edge", 
                                                                                            "gad7.control_worrying_stop", 
                                                                                            "gad7.worrying_too_much_about_different_things", 
                                                                                            "gad7.trouble_relaxing", 
                                                                                            "gad7.sit_restless_hard", 
                                                                                            "gad7.becoming_easily_annoyed_or_irritable",
                                                                                            "gad7.awful_feeling_afraid_happen",
                                                                                            "gad7.problems_made_difficult_care",
                                                                                            "gad7.total_score")

# ========== Merge PHQ9 and GAD7 Data ==========
coping_nbr_psychopathology <- merge(coping_nbr_phq9, coping_nbr_gad7, by = "IID", all = TRUE)

# ========== Add Cohort Name ==========
coping_nbr_psychopathology$cohort <- "coping_nbr"

# ========================= Combine sub-datasets for COPING and sub-studies ==========
# ================= Clean demographics ========
## Combine rows
coping_all_clean_dem <- rbind(coping_glad_clean_dem, coping_edgi_clean_dem, coping_nbr_clean_dem)
dim(coping_all_clean_dem)

## Identify duplicates
table(duplicated(coping_all_clean_dem$IID))


# ================= EDs ========
## Combine rows
coping_all_EDs <- rbind(coping_glad_EDs, edgi_EDs, coping_nbr_EDs)
dim(coping_all_EDs)

## Identify duplicates
table(duplicated(coping_all_EDs$IID))

# ================= Psychopathology ========
## Combine rows 
coping_all_psychopathology <- rbind(coping_glad_psychopathology, coping_edgi_psychopathology, coping_nbr_psychopathology)
dim(coping_all_psychopathology)

## Identify duplicates
table(duplicated(coping_all_psychopathology$IID))

# ================= TAF ========
## Combine rows
coping_all_taf <- rbind(coping_glad_taf, coping_edgi_taf, coping_nbr_taf)
dim(coping_all_taf)

## Identify duplicates
table(duplicated(coping_all_taf$IID))


# ================= PANWORRY (COVID Stressors) ========
## Combine rows
coping_all_panworry <- rbind(coping_glad_panworry, coping_edgi_panworry, coping_nbr_panworry)
dim(coping_all_panworry)

## Identify duplicates
table(duplicated(coping_all_panworry$IID))


# ========================= Master dataset creation ==========
## Merge data retaining only individuals present in coping sub-studies
master_data <- Reduce(function(x, y) merge(x, y, by = c("IID", "cohort"), all = TRUE),
                      list(coping_all_clean_dem, coping_all_EDs, coping_all_psychopathology,
                           coping_all_taf, coping_all_panworry))

## Identify duplicates
table(duplicated(master_data$IID))


# ========================= Create ED and SU row sums ==========
## ED Total Score Sum (Anorexia + Bulimia + BED)
master_data$ED_sums <- my_rowSums(master_data[, c("an.total_score", "bn.total_score", "bed.total_score")])

## SU Total Score Sum (Suicidality from TAF)
master_data$SU_sums <- my_rowSums(master_data[, c("taf.worth_living_life_thoughts",
                                                  "taf.past_felt_weeks", 
                                                  "taf.past_felt_weeks.1",
                                                  "taf.have_you_contemplated_harming_yourself_",
                                                  "taf.meant_end_life_weeks")])


## Panworry Total Score Sum (covid stressors)
master_data$PW_sums <- my_rowSums(master_data[, c("panworry.impact_on_your_employment_status",              
                                                  "panworry.employment_status_household_key",               
                                                  "panworry.impact_on_your_education_or_exams",             
                                                  "panworry.exams_education_impact_children",               
                                                  "panworry.financial_impact",                              
                                                  "panworry.contracting_the_virus" ,                        
                                                  "panworry.people_you_know_contracting_the_virus" ,        
                                                  "panworry.people_you_dont_know_contracting_the_virus"    ,
                                                  "panworry.wellbeing_mental_health_impact",                
                                                  "panworry.wellbeing_childrens_mental_health",             
                                                  "panworry.relatives_mental_health_impact",                
                                                  "panworry.being_socially_isolated"  ,                     
                                                  "panworry.people_you_know_being_socially_isolated" ,      
                                                  "panworry.shortage_of_essential_supplies_" ,              
                                                  "panworry.shortage_of_medication_or_access_to_healthcare",
                                                  "panworry.healthcare_shortage_people_essential" ,         
                                                  "panworry.virus_government_information_accuracy"  ,       
                                                  "panworry.the_governments_response_to_the_pandemic"  ,    
                                                  "panworry.family_members_separation"      ,               
                                                  "panworry.global_recession_economy_longstanding")])

# ========================= Scale numeric variables ==========
## Copy master_data before scaling
master_data_scaled <- master_data

## Scale all relevant numerical variables (excluding categorical ones)
scale_cols <- c("an.total_score", "bn.total_score", "bed.total_score", 
                "taf.worth_living_life_thoughts",
                "taf.past_felt_weeks", 
                "taf.past_felt_weeks.1",
                "taf.have_you_contemplated_harming_yourself_",
                "taf.meant_end_life_weeks", "ED_sums", "SU_sums", "PW_sums")

master_data_scaled[, scale_cols] <- scale(master_data_scaled[, scale_cols])


# ========================= Write out master dataset ==========
saveRDS(master_data, "~/Desktop/ResearchProjectData/master_data.rds")

