#I: How well do B6-vFRIGHT and B6-vFI clocks predict the chronological age and biological age (frailty) of DO mice?

resI <- predict_out_of_strain(train = dfB6, test = dfDO, yname = "AgeAtVid", nfolds = 10, seed = 20240311)

resII <- predict_out_of_strain(train = dfB6, test = dfDO, yname = "score", nfolds = 10, seed = 20240311)

#II: How well do DO-vFRIGHT and DO-vFI clocks predict the chronological age and biological age (frailty) of B6 mice?

resIII <- predict_out_of_strain(train = dfDO, test = dfB6, yname = "AgeAtVid", nfolds = 10, seed = 20240311)

resIV <- predict_out_of_strain(train = dfDO, test = dfB6, yname = "score", nfolds = 10, seed = 20240311)

#IV: Build vFRIGHT clocks for B6, DO, and combined B6-DO datasets for comparison (Do this on HPC)

DO_resIV <- model_selection(data = dfDO, yname = "AgeAtVid" ,models = c("lm", "rf", "xgb"), seed = 1, strain = "DO", feature_type = "video")
B6_resIV <- model_selection(data = dfB6, yname = "AgeAtVid",models = c("lm","rf","xgb"), seed = 1)
B6DO_resIV <- model_selection(data = dfvideo, yname = "AgeAtVid",models = c("lm","rf","xgb"), seed = 1)

plot_compare_data(B6_results = B6_model_selection_age, DO_results = DO_model_selection_age, combined_results = B6DO_model_selection_age, metric = "MAE", yname = "AgeAtVid")

#V: Buiding vFI clocks for B6, DO, and combined B6-DO datasets for comparison (Do this on HPC)

DO_resV <- model_selection(data = dfDO, yname = "score",models = c("lm","rf","xgb"), seed = 1)
B6_resV <- model_selection(data = dfB6, yname = "score",models = c("lm","rf","xgb"), seed = 1)
B6DO_resV <- model_selection(data = dfvideo, yname = "score", models = c("lm","rf","xgb"), seed = 1)

plot_compare_data(B6_results = B6_model_selection_score, DO_results = DO_model_selection_score, combined_results = B6DO_model_selection_score, metric = "RMSE", yname = "score")

#VI: Comparing age prediction using video features to age prediction using manual frailty items (Do this on HPC)

video_age <- model_selection(data = masterdf, yname = "AgeAtVid", models = c("lm", "rf", "xgb"), seed = 240304, strain = c("B6", "DO"), feature_type = "video")
frailty_age <- model_selection(data = masterdf, yname = "AgeAtVid", models = c("lm", "rf", "xgb"), seed = 240304, strain = c("B6", "DO"), feature_type = "frailty")

all_video_age$metrics |> group_by(Model) |> summarize(mean_rmse = mean(RMSE), mean_mae = mean(MAE), mean_r2 = mean(R2), sd_rmse = sd(RMSE), sd_mae = sd(MAE), sd_r2 = sd(R2))
all_frailty_age$metrics |> group_by(Model) |> summarize(mean_rmse = mean(RMSE), mean_mae = mean(MAE), mean_r2 = mean(R2), sd_rmse = sd(RMSE), sd_mae = sd(MAE), sd_r2 = sd(R2))

#Pick the best model based on the results above
best_model <- "rf" #"xgb"

dfres <- bind_rows(all_video_age$metrics |> filter(Model == best_model) |> mutate(Strain = "DO", Feature = "Video"), all_frailty_age$metrics |> filter(Model == best_model) |> mutate(Strain = "DO", Feature = "Frailty"),all_video_age$metrics |> filter(Model == best_model) |> mutate(Strain = "All", Feature = "Video"), all_frailty_age$metrics |> filter(Model == best_model) |> mutate(Strain = "All", Feature = "Frailty")) |> mutate(Strain =factor(Strain, levels = c("DO", "All")))

dfres |> ggplot(aes(x = Strain, y = MAE, fill = Feature)) + geom_boxplot(position = position_dodge(width = 0.8)) + geom_point(position = position_dodge(width = 0.8)) + theme_classic(base_size = 14) + theme(axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13)) + labs(y = "MAE (weeks)") + scale_fill_manual(values = wes_palette("Royal1")) 


#VII: Predict proportion of life lived (PLL)
resVII <- predict_PLL_video_vs_frailty(data = dataFLL, nfolds = 50, seed = 123, yname = "FLL")

ggplot(resultsVII$results, aes(x = Model, y = value, fill = Features)) + geom_boxplot(alpha = 0.8) + theme_classic(base_size = 14) + theme(axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13)) + scale_fill_brewer(palette = "Dark2") + labs(y = "Metric") + facet_wrap(~Metric, scales = "free_y", labeller = label_parsed) + scale_fill_manual(values = wes_palette("Royal1"), name = "Feature Set", labels = c("Frailty", "Video", "Video+Frailty")) + guides(fill = guide_legend(title = "Feature Set"), size = 2) + theme(legend.position = "top")