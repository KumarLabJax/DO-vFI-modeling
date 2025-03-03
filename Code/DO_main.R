libs <- c('dplyr', 'ggplot2', 'readr', 'lme4', 'tidymodels','purrr', 'rlang', 'ordinal', 'tidyr', 'wesanderson', 'gamboostLSS', 'gamlss', 'RLRsim', 'rlang','betareg','reshape2','RColorBrewer','janitor','circlize','ComplexHeatmap','ranger')
sapply(libs, require, character.only = TRUE)

#setwd("DO-vFI-modeling/")
#source("Code/vFI_functions.R")
source("Code/DO_functions.R")
#source("Code/DO_expts.R")
source("Code/DO_utils.R")

dfvideo <- read_csv("Data/B6DO_video.csv") #video features for B6 and DO
dfB6 <- dfvideo |> filter(Strain == "B6") |> select(any_of(c(meta_cols, covars, y_vars, video_features))) |> select(!FLL) #B6
dfDO <- dfvideo |> filter(Strain == "DO") |> select(any_of(c(meta_cols, covars, y_vars, video_features)))

#Manual frailty data
B6FI <- read_csv("Data/B6_frailty.csv") 
DOFI <- read_csv("Data/DO_frailty.csv")

#B6 and DO data (merged video and frailty features)
masterdf <- read_csv("Data/B6DO_frailty_video.csv")
