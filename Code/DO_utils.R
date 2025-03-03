#Some variable definitions

CalCols <- c("seashell4", "skyblue", "royalblue4", "orange", "firebrick")
DietNames <-  c("AL", "1D", "2D", "20", "40")
names(CalCols) <- DietNames

meta_cols <- c("NetworkFilename", "PoseFilename", "Batch", "Tester", "AgeGroup", "MouseID", "Strain", "FIs", "FIdate", "NFI", "AgeatFrailty", "Longitudinal", "Number")

covars <- c("Diet", "Weight", "Sex") #Covariates

y_vars <- c("AgeW", "AgeAtVid", "CFI_norm", "FLL","score", "Survival", "logitPLL", "Status", "CFI") #Response variables

#####Video features
features_gait <- c("median_angular_velocity", "median_base_tail_lateral_displacement", "median_limb_duty_factor", "median_nose_lateral_displacement", "median_speed_cm_per_sec", "median_step_length1", "median_step_width", "median_stride_length", "median_tip_tail_lateral_displacement", "stride_count", "angular_velocity_iqr", "base_tail_lateral_displacement_iqr", "limb_duty_factor_iqr", "nose_lateral_displacement_iqr", "speed_cm_per_sec_iqr", "step_length1_iqr", "step_width_iqr", "stride_length_iqr", "tip_tail_lateral_displacement_iqr") 
features_engineered <- c("median_length", "median_width", "dAC_mean", "dAC_stdev", "dAC_min", "dAC_median", "dB_mean", "dB_stdev", "dB_max", "dB_median", "aABC_mean", "aABC_stdev", "aABC_min", "aABC_median", "dAC_nongait_mean", "dAC_nongait_stdev","dAC_nongait_median", "dB_nongait_max", "dB_nongait_median", "aABC_nongait_mean", "aABC_nongait_stdev", "aABC_nongait_median")
features_of <- c("distance_cm", "center_time_secs", "periphery_time_secs", "corner_time_secs", "center_distance_cm", "periphery_distance_cm", "corner_distance_cm", "grooming_number_bouts", "grooming_duration_secs")
features_rearing <- c("Rearing_supported_T5", "Rearing_supported_T20", "Rearing_supported_T55", "Rearing_unsupported_T5", "Rearing_unsupported_T20", "Rearing_unsupported_T55")
features_grooming <- c("Grooming_T5", "Grooming_T20", "Grooming_T55")
video_features <- c(features_gait, features_engineered, features_of, features_rearing, features_grooming)
#####

#####Frailty features
old_frailty_features <- c("alopecia", "loss_of_fur_color", "dermatitis", "loss_of_whiskers", "coat_condition", "piloerection", "cataracts", "eye_discharge", "microphthalmia", "corneal_opacity", "nasal_discharge", "rectal_prolapse", "vaginal_uterine_prolapse", "diarrhea", "vestibular_disturbance", "vision_loss_visual_placing", "menace_reflex",  "tail_stiffening", "gait_disorders", "tremor", "tumors", "distended_abdomen", "kyphosis", "body_condition", "breathing_rate_depth", "malocclusions", "righting_reflex", "loss_of_fur_colour", "eye_discharge_swelling", "vaginal_uterine", "tumours")
general_frailty_features <- c("breathing_rate_depth", "coat_condition", "piloerection", "dermatitis", "pallor_or_cyanosis", "kyphosis", "hunched", "body_condition", "eye_discharge", "changes_to_the_globe", "response_to_analgesic", "peri_retro_orbital_swelling", "nasal_discharge", "rectal_prolapse", "vaginal_uterine_prolapse", "diarrhea", "head_piloerection", "thoracic_mass")
physical_frailty_features <- c("tail_stiffening", "vestibular_disturbance", "gait_disorders", "tremor", "activity", "response_to_external_stimuli", "paralysis", "tumors", "distended_abdomen", "urine", "dehydration_reduced_skin_turgor","malocclusions")
other_frailty_features <- c("temperature", "body_weight")
frailty_features <- c(old_frailty_features, general_frailty_features, physical_frailty_features)
#####
