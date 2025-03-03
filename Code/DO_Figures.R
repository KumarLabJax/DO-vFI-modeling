source("Code/DO_functions.R")
source("Code/DO_utils.R")

#Figure 1
#F1C
df |> ggplot(aes(x = Strain)) + geom_bar(width = 0.5, aes(fill = Diet)) + theme_classic(base_size = 16) + labs(x = "Strain", y = "Count") + scale_fill_manual(values = CalCols) + scale_y_continuous(expand = c(0,0)) + scale_x_discrete(labels = c("B6" = "B6J", "DO" = "DO")) 

#F1D
df <- df |> mutate(AgeAtVid = round(AgeAtVid))
df <- df |> mutate(AgeG = as.numeric(case_when(AgeAtVid <= 25 ~ 25, AgeAtVid > 25 & AgeAtVid <= 75 ~ 75, AgeAtVid > 75 & AgeAtVid <= 125 ~ 125, AgeAtVid > 125 & AgeAtVid <= 165 ~ 165, AgeAtVid > 165 ~ 200))) 
df_AgeG <- df |> group_by(AgeG, Strain) |> summarize(score_mean = mean(score), score_se = sd(score), n = n())

df |> ggplot() + geom_jitter(data = df, aes(x = AgeAtVid, y = score, color = Diet, shape = Strain),size = 3, alpha = 0.8,width = 20) + geom_pointrange(data = df_AgeG, aes(x = AgeG, y = score_mean, ymin = score_mean - score_se, ymax = score_mean + score_se, shape = Strain),size = 1) +
theme_classic(base_size = 16) + scale_color_manual(values = CalCols, guide = "none") + labs(x = "Age (in weeks)", y = "Score (CFI)") + geom_line(data = df_AgeG, aes(x = AgeG, y = score_mean, group = Strain),size = 1) + scale_shape_discrete(labels = c("B6J", "DO")) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + expand_limits(x = 0, y = 0)


#F1E
df |> ggplot(aes(x = score, color = Strain)) + geom_density(lwd = 1) + theme_classic(base_size = 16) + labs(x = "Score (CFI)", y = "Density") + ggsci::scale_color_jama() + scale_y_continuous(expand = c(0,0)) + ylim(0,0.15) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))


#F1F
dfDOFI <- read_csv("Data/df_DO_frailty_2024_03_14.csv") |> mutate(Strain = as.factor("DO")) |> rename("Weight" = "body_weight") 
dfB6FI <- read_csv("Data/df_frailty_parameters.csv") |> mutate(Strain = as.factor("B6"), Diet = as.factor("AL")) |> janitor::clean_names() |> rename("MouseID" = "mouse_id", "Batch" = "batch", "Sex" = "sex", "Tester" = "tester", "AgeAtTest" = "age_at_test", "Weight" = "weight", "loss_of_fur_color" = "loss_of_fur_colour", "vaginal_uterine_prolapse" = "vaginal_uterine", "eye_discharge" = "eye_discharge_swelling", "tumors" = "tumours", "CFI" = "overall_score", "Strain" = "strain", "Diet" = "diet") |> mutate(Tester = as.factor(Tester)) |> mutate(Strain = as.factor("B6J"))

tmp <- dfDOFI |> select(any_of(c("Diet",frailty_features))) |> gather(variable, value, -Diet) |> mutate(variable = as.factor(variable), value = as.factor(value))
common_frailty_features <- intersect(intersect(names(dfDOFI), names(dfB6FI)), frailty_features)
dfFI <- bind_rows(dfDOFI, dfB6FI) |> select(any_of(c("MouseID", "Strain", "Diet", "Weight", common_frailty_features))) |> mutate_at(common_frailty_features, factor)
dfFImelt <- dfFI |> select(any_of(c("Diet", "Strain", common_frailty_features))) |> gather(variable, value, -Diet, -Strain) |> mutate(variable = as.factor(variable), value = as.factor(value)) 
levels(dfFImelt$variable)[levels(dfFImelt$variable) %in% c("breathing_rate_depth", "distended_abdomen", "vaginal_uterine_prolapse", "vestibular_disturbance", "vision_loss_visual_placing")] <- c("breathing_rate", "dist_abdomen", "vag_prolapse", "vestib_dist", "vision_loss")
dfFImelt$Strain <- factor(dfFImelt$Strain, levels = c("B6J", "DO"))
dfFImelt$value[dfFImelt$value == 0.6] <- 0.5 #correcting the incorrect entries
dfFImelt$value[dfFImelt$value == 0.25] <- 0.5 #correcting the incorrect entries

dfFImelt |> drop_na() |> ggplot(aes(x = Strain, fill = factor(Strain), alpha = value)) + geom_bar(position = "fill") + theme_classic(base_size = 14) + ggsci::scale_fill_jama(name = "Strain") + labs(x = "Strain", y = "Proportion") + theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 14), strip.text = element_text(size=14)) + scale_y_continuous(expand = expansion(mult = c(0, .1)))

#F1G
dfFImelt |> drop_na() |> ggplot(aes(x = interaction(Strain, variable, lex.order = TRUE), fill = factor(Strain), alpha = value)) + geom_bar(position = "fill") + theme_classic(base_size = 14) + ggsci::scale_fill_jama(name = "Strain") + labs(x = "Frailty Feature", y = "Proportion") + facet_grid(~variable, switch = "x", scales = "free_x", space = "free_x") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4, size = 13), axis.text.y = element_text(size = 13), panel.spacing = unit(0, "lines"), strip.background = element_blank(), strip.placement = "outside") + scale_alpha_discrete()

#We made further aesthetic edits to plots (F1 F,G) above using the Inkscape software to make the final version in the paper. 

#Figure 2

#F2B
tmpDO <- preprocess_data(df = dfDO, type = "DO")
tmpDO$p_scorer

#F2C
tmpB6 <- preprocess_data(df = dfB6, type = "B6")
tmpB6$p_scorer

#F2D
tmp <- preprocess_data(df = dfvideo, type = "combined")
tmp$p_scorer

#F2E
tmpDO <- preprocess_data(df = dfDO, type = "DO")
tmpDO$p_diet

#F2A
Reduce(rbind, list(tmpB6$var_percent, tmpDO$var_percent, tmp$var_percent)) |> mutate(Strain = c(rep("B6J", nrow(tmpB6$var_percent)), rep("DO", nrow(tmpDO$var_percent)), rep("B6J-DO", nrow(tmp$var_percent))), percent = 100*percent) |> filter(!grp %in% 'Residual') |> ggplot(aes(x = Strain, y = percent, fill = grp)) + geom_bar(stat = "identity") + theme_classic(base_size = 14) + labs(x = "Strain", y = "Variance explained (%)") + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) + scale_y_continuous(expand = c(0,0)) + ggsci::scale_fill_bmj()



gather(variable, value, -Strain) |> mutate(variable = factor(variable, levels = c("AgeAtVid", "score", "CFI_norm", "FLL", "Survival", "logitPLL", "Status", "CFI"))) |> mutate(value = round(value, 2)) |> mutate(value = paste0(value * 100, "%"))



|> mutate(Strain = factor(Strain, levels = c("B6", "DO", "B6-DO"))) |> gather(variable, value, -Strain) |> mutate(variable = factor(variable, levels = c("AgeAtVid", "score", "CFI_norm", "FLL", "Survival", "logitPLL", "Status", "CFI"))) |> mutate(value = round(value, 2)) |> mutate(value = paste0(value * 100, "%"))


#Figure 3
#F3A
corrs <- df |> filter(Strain == "DO") |> select(all_of(video_features)) |> correlation::correlation() |> summary(redundant = TRUE)
corr_mat <- as.matrix(corrs |> select(!Parameter))
colnames(corr_mat) <- NULL

col_fun <- colorRamp2(c(min(corr_mat, na.rm = TRUE),0,0,max(corr_mat, na.rm = TRUE)),c("#045a8d","#f7f7f7","#f7f7f7","#cb181d"))
ht.z <- ComplexHeatmap::Heatmap(t(as.matrix(corr_mat)), row_names_gp = gpar(fontsize = 10),row_names_side = "left", column_names_gp = gpar(fontsize = 10), column_names_side = "bottom", heatmap_legend_param = list(at = c(-1,0,1),title = latex2exp::TeX("Pearson correlation"), title_position = "leftcenter-rot", border = "black",legend_height = unit(4, "cm"), labels_gp = gpar(fontsize = 14),just = c("right", "top")), col = col_fun, cluster_rows = FALSE, cluster_columns = FALSE, border = TRUE,cell_fun = function(j, i, x, y, width, height, fill) {grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey"))})
ht.z

col_fun2 <- colorRamp2(c(1,2,3),c("#D8B70A", "#02401B", "#A2A475"), transparency = 0)
my_features <- data.frame(Type = ifelse(video_features %in% features_gait, 1, ifelse(video_features %in% features_engineered, 2, 3)))
rownames(my_features) <- video_features
colnames(my_features) <- ""

ht.cluster <- Heatmap(t(as.matrix(my_features)),cluster_rows = FALSE, cluster_columns = FALSE,col = col_fun2, show_heatmap_legend = FALSE,column_names_gp = gpar(fontsize = 10),row_names_gp = gpar(fontsize = 10),row_names_side = "left")
ht.z %v% ht.cluster

ht.cluster_row <- Heatmap((as.matrix(my_features)),cluster_rows = FALSE, cluster_columns = FALSE,col = col_fun2, show_heatmap_legend = FALSE,column_names_gp = gpar(fontsize = 10),row_names_gp = gpar(fontsize = 0),row_names_side = "left")

#F3B

tmp <- purrr::map(video_features, function(x) df |> select(all_of(c("Batch", "Diet", "score", "Sex", x))) |> correlation::correlation(partial = TRUE, multilevel = FALSE) |> filter(Parameter1 == "score" & Parameter2 == paste0(x)) |> select(r,p)) 
corr_all <- do.call(rbind, tmp) |> as_tibble() |> mutate(Feature = video_features, .before = "r") |> mutate(p = p.adjust(p, method = "fdr")) |> rename(All = r, p_all = p) |> mutate(All = ifelse(p_all > 0.05, 0, All))

tmp <- purrr::map(video_features, function(x) df |> filter(Strain == "B6") |> select(all_of(c("Batch", "score", "Sex", x))) |> correlation::correlation(partial = TRUE, multilevel = FALSE) |> filter(Parameter1 == "score" & Parameter2 == paste0(x)) |> select(r,p)) 
corr_B6 <- do.call(rbind, tmp) |> as_tibble() |> mutate(p = p.adjust(p, method = "fdr")) |> rename(B6 = r, p_B6 = p) |> mutate(B6 = ifelse(p_B6 > 0.05, 0, B6))

tmp <- purrr::map(video_features, function(x) df |> filter(Strain == "DO") |> select(all_of(c("Batch", "Diet", "score", "Sex", x))) |> correlation::correlation(partial = TRUE, multilevel = FALSE) |> filter(Parameter1 == "score" & Parameter2 == paste0(x)) |> select(r,p)) 
corr_DO <- do.call(rbind, tmp) |> as_tibble() |> mutate(p = p.adjust(p, method = "fdr")) |> rename(DO = r, p_DO = p) |> mutate(DO = ifelse(p_DO > 0.05, 0, DO))

df_cor <- cbind(corr_DO, corr_B6, corr_all) |> column_to_rownames("Feature") |> select(all_of(c("B6", "DO", "All")))

feature <- df_cor |> rownames_to_column("Feature") |> mutate(Type = ifelse(video_features %in% features_gait, "Gait", ifelse(video_features %in% features_engineered, "Engineered", "Open Field")), DO = abs(DO), B6 = abs(B6)) |> filter(DO != 0 | B6 != 0) |> filter(Type == "Gait") |> arrange(desc(B6)) |> head(1) |> pull(Feature)

p1 <- df |> filter(!!rlang::sym(feature) < 0.60) |> ggplot(aes(x = !!rlang::sym(feature), y = score)) + geom_point(aes(shape = Strain), alpha = 0.8, color = "#D8B70A") + stat_smooth(method = "lm", color = "black") + theme_classic(base_size = 10) + stat_smooth(method = "lm", aes(color = Strain)) + labs(y = "Score (CFI)", x = "Tip Tail LD") + theme(legend.position = "none") + ggsci::scale_color_d3()

#Find the gait features that are most correlated with yname for DO
feature <- df_cor |> rownames_to_column("Feature") |> mutate(Type = ifelse(video_features %in% features_gait, "Gait", ifelse(video_features %in% features_engineered, "Engineered", "Open Field")), DO = abs(DO), B6 = abs(B6)) |> filter(DO != 0 | B6 != 0) |> filter(Type == "Gait") |> arrange(desc(DO)) |> head(1) |> pull(Feature)

p2 <- df |> filter(!!rlang::sym(feature) < 0.60) |> ggplot(aes(x = !!rlang::sym(feature), y = score)) + geom_point(aes(shape = Strain), alpha = 0.8, color = "#D8B70A") + stat_smooth(method = "lm", color = "black") + theme_classic(base_size = 10) + stat_smooth(method = "lm", aes(color = Strain)) + labs(y = "Score (CFI)", x = "Step Width") + theme(legend.position = "none") + ggsci::scale_color_d3()

#Find the gait features that are most correlated with yname for all
feature <- df_cor |> rownames_to_column("Feature") |> mutate(Type = ifelse(video_features %in% features_gait, "Gait", ifelse(video_features %in% features_engineered, "Engineered", "Open Field")), DO = abs(DO), B6 = abs(B6)) |> filter(DO != 0 | B6 != 0) |> filter(Type == "Gait") |> arrange(desc(All)) |> head(1) |> pull(Feature)

p3 <- df |> filter(!!rlang::sym(feature) < 0.6) |> ggplot(aes(x = !!rlang::sym(feature), y = score)) + geom_point(aes(shape = Strain), alpha = 0.8, color = "#D8B70A") + stat_smooth(method = "lm", color = "black") + theme_classic(base_size = 10) + stat_smooth(method = "lm", aes(color = Strain)) + labs(y = "Score (CFI)", x = "Angular Velocity (IQR)") + theme(legend.position = "none") + ggsci::scale_color_d3()

#Find the engineered features that are most correlated with yname for B6

feature <- df_cor |> rownames_to_column("Feature") |> mutate(Type = ifelse(video_features %in% features_gait, "Gait", ifelse(video_features %in% features_engineered, "Engineered", "Open Field")), DO = abs(DO), B6 = abs(B6)) |> filter(DO != 0 | B6 != 0) |> filter(Type == "Engineered") |> arrange(desc(B6)) |> head(1) |> pull(Feature)

p4 <- df |> filter(!!rlang::sym(feature) > 25) |> ggplot(aes(x = !!rlang::sym(feature), y = score)) + geom_point(aes(shape = Strain), alpha = 0.8, color = "#02401B") + stat_smooth(method = "lm", color = "black") + theme_classic(base_size = 10) + stat_smooth(method = "lm", aes(color = Strain)) + labs(y = "Score (CFI)", x = "Median width") + theme(legend.position = "none") + ggsci::scale_color_d3()

#Find the engineered features that are most correlated with yname for DO

feature <- df_cor |> rownames_to_column("Feature") |> mutate(Type = ifelse(video_features %in% features_gait, "Gait", ifelse(video_features %in% features_engineered, "Engineered", "Open Field")), DO = abs(DO), B6 = abs(B6)) |> filter(DO != 0 | B6 != 0) |> filter(Type == "Engineered") |> arrange(desc(DO)) |> head(1) |> pull(Feature)

p5 <- df |> ggplot(aes(x = !!rlang::sym(feature), y = score)) + geom_point(aes(shape = Strain), alpha = 0.8, color = "#02401B") + stat_smooth(method = "lm", color = "black") + theme_classic(base_size = 10) + stat_smooth(method = "lm", aes(color = Strain)) + labs(y = "Score (CFI)", x = "dB_nongait") + theme(legend.position = "none") + ggsci::scale_color_d3() + scale_y_continuous(expand = c(0,0))

#Find the engineered features that are most correlated with yname for all

feature <- df_cor |> rownames_to_column("Feature") |> mutate(Type = ifelse(video_features %in% features_gait, "Gait", ifelse(video_features %in% features_engineered, "Engineered", "Open Field")), DO = abs(DO), B6 = abs(B6)) |> filter(DO != 0 | B6 != 0) |> filter(Type == "Engineered") |> arrange(desc(All)) |> head(1) |> pull(Feature)

p6 <- df |> ggplot(aes(x = !!rlang::sym(feature), y = score)) + geom_point(aes(shape = Strain), alpha = 0.8, color = "#02401B") + stat_smooth(method = "lm", color = "black") + theme_classic(base_size = 10) + stat_smooth(method = "lm", aes(color = Strain)) + labs(y = "Score (CFI)", x = "dAC_min") + theme(legend.position = "none") + ggsci::scale_color_d3()

#Find the open field features that are most correlated with yname for B6

feature <- df_cor |> rownames_to_column("Feature") |> mutate(Type = ifelse(video_features %in% features_gait, "Gait", ifelse(video_features %in% features_engineered, "Engineered", "Open Field")), DO = abs(DO), B6 = abs(B6)) |> filter(DO != 0 | B6 != 0) |> filter(Type == "Open Field") |> arrange(desc(B6)) |> head(1) |> pull(Feature)

p7 <- df |> filter(!!rlang::sym(feature) < 0.2) |> ggplot(aes(x = !!rlang::sym(feature), y = score)) + geom_point(aes(shape = Strain), alpha = 0.8, color = "#A2A475") + stat_smooth(method = "lm", color = "black") + theme_classic(base_size = 10) + stat_smooth(method = "lm", aes(color = Strain)) + labs(y = "Score (CFI)", x = "Rearing_supported_T5") + theme(legend.position = "none") + ggsci::scale_color_d3()

#Find the open field features that are most correlated with yname for DO

feature <- df_cor |> rownames_to_column("Feature") |> mutate(Type = ifelse(video_features %in% features_gait, "Gait", ifelse(video_features %in% features_engineered, "Engineered", "Open Field")), DO = abs(DO), B6 = abs(B6)) |> filter(DO != 0 | B6 != 0) |> filter(Type == "Open Field") |> arrange(desc(DO)) |> head(1) |> pull(Feature)

p8 <- df |> ggplot(aes(x = !!rlang::sym(feature), y = score)) + geom_point(aes(shape = Strain), alpha = 0.8, color = "#A2A475") + stat_smooth(method = "lm", color = "black") + theme_classic(base_size = 10) + stat_smooth(method = "lm", aes(color = Strain)) + labs(y = "Score (CFI)", x = "Grooming_T20") + theme(legend.position = "none") + ggsci::scale_color_d3()

#Find the open field features that are most correlated with yname for all

feature <- df_cor |> rownames_to_column("Feature") |> mutate(Type = ifelse(video_features %in% features_gait, "Gait", ifelse(video_features %in% features_engineered, "Engineered", "Open Field")), DO = abs(DO), B6 = abs(B6)) |> filter(DO != 0 | B6 != 0) |> filter(Type == "Open Field") |> arrange(desc(All)) |> head(1) |> pull(Feature)

p9 <- df |> ggplot(aes(x = !!rlang::sym(feature), y = score)) + geom_point(aes(shape = Strain), alpha = 0.8, color = "#A2A475") + stat_smooth(method = "lm", color = "black") + theme_classic(base_size = 10) + stat_smooth(method = "lm", aes(color = Strain)) + labs(y = "Score (CFI)", x = "Grooming_T20") + theme(legend.position = "none") + ggsci::scale_color_d3()

(p1 | p2 | p3) / (p4 | p5 | p6) / (p7 | p8 | p9)

mypal <- c(ggsci::pal_d3("category10", alpha = 1)(2), "#000000")
dftmp <- data.frame(x = rnorm(3), y = rnorm(3), Strain = factor(c("B6J", "DO", "B6J-DO")))
dftmp$Strain <- factor(dftmp$Strain, level = c("B6J", "DO", "B6J-DO"))
ptmp <- ggplot(dftmp, aes(x = x, y = y, color = Strain)) + geom_point() + theme_bw(base_size = 14) + scale_color_manual(values = mypal) + theme(legend.position = "top") + guides(colour = guide_legend(override.aes = list(size=10))) + guides(colour = guide_legend(override.aes = list(size=6)))
legend <- cowplot::get_plot_component(ptmp, 'guide-box-top', return_all = TRUE)
cowplot::ggdraw(legend)

#F3C
#Change score to AgeW to make similar plots  
corr_DO <- correlation::correlation(df |> filter(Strain == "DO") |> select(all_of(c("score", video_features)))) |> filter(Parameter1 == "score") |> mutate(Type = case_when(Parameter2 %in% features_gait ~ "Gait", Parameter2 %in% features_engineered ~ "Engineered", Parameter2 %in% c(features_rearing, features_of, features_grooming) ~ "Open Field")) |> select(all_of(c("Parameter2", "r", "p", "Type")))

corr_B6 <- correlation::correlation(df |> filter(Strain == "B6") |> select(all_of(c("score", video_features)))) |> filter(Parameter1 == "score") |> mutate(Type = case_when(Parameter2 %in% features_gait ~ "Gait", Parameter2 %in% features_engineered ~ "Engineered", Parameter2 %in% c(features_rearing, features_of, features_grooming) ~ "Open Field")) |> select(all_of(c("Parameter2", "r", "p", "Type")))

corr_all <- correlation::correlation(df |> select(all_of(c("score", video_features)))) |> filter(Parameter1 == "score") |> mutate(Type = case_when(Parameter2 %in% features_gait ~ "Gait", Parameter2 %in% features_engineered ~ "Engineered", Parameter2 %in% c(features_rearing, features_of, features_grooming) ~ "Open Field")) |> select(all_of(c("Parameter2", "r", "p", "Type")))

corr_DO <- corr_DO |> mutate(r = ifelse(p > 0.05, 0, r)) |> select(Parameter2, r) |> rename(Features = Parameter2, DO = r)
corr_B6 <- corr_B6 |> mutate(r = ifelse(p > 0.05, 0, r)) |> select(r) |> rename(B6 = r)
corr_all <- corr_all |> mutate(r = ifelse(p > 0.05, 0, r)) |> select(r) |> rename(All = r)

df_cor <- cbind(corr_DO, corr_B6, corr_all) |> column_to_rownames("Features")
col_fun <- colorRamp2(c(min(df_cor, na.rm = TRUE),0,0,max(df_cor, na.rm = TRUE)),c("#045a8d","#f7f7f7","#f7f7f7","#cb181d"))
ht.z <- ComplexHeatmap::Heatmap(t(as.matrix(df_cor)), row_names_gp = gpar(fontsize = 10),row_names_side = "left", column_names_gp = gpar(fontsize = 10), column_names_side = "bottom", heatmap_legend_param = list(at = c(-1,0,1),title = latex2exp::TeX("Pearson correlation"), title_position = "leftcenter-rot", border = "black",legend_height = unit(4, "cm"), labels_gp = gpar(fontsize = 14),just = c("right", "top")), col = col_fun, cluster_rows = FALSE, cluster_columns = FALSE, border = TRUE,cell_fun = function(j, i, x, y, width, height, fill) {grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey"))})
ht.z

col_fun2 <- colorRamp2(c(1,2,3),c("#D8B70A", "#02401B", "#A2A475"), transparency = 0)
my_features <- data.frame(Type = ifelse(video_features %in% features_gait, 1, ifelse(video_features %in% features_engineered, 2, 3)))
rownames(my_features) <- video_features
colnames(my_features) <- ""

ht.cluster <- Heatmap(t(as.matrix(my_features)),cluster_rows = FALSE, cluster_columns = FALSE,col = col_fun2, show_heatmap_legend = FALSE,column_names_gp = gpar(fontsize = 10),row_names_gp = gpar(fontsize = 10),row_names_side = "left")

ht.z %v% ht.cluster

#F3D, F3E
p1 <- df_cor |> rownames_to_column("Feature") |> mutate(Type = ifelse(video_features %in% features_gait, "Gait", ifelse(video_features %in% features_engineered, "Engineered", "Open Field")), DO = abs(DO), B6 = abs(B6)) |> filter(DO != 0 | B6 != 0) |> ggplot(aes(x = B6, y = DO, color = Type)) + geom_point(size = 3) + scale_color_manual(values = c("Gait" = "#D8B70A", "Engineered" = "#02401B", "Open Field" = "#A2A475")) + theme_classic(base_size = 14) + expand_limits(x = 0, y = 0) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), legend.position = "none") + labs(x = expression("|r|"["CFI, feature"] ~ "(B6J)"), y = expression("| r |"["CFI, feature"] ~ "(DO)")) + geom_abline(linetype = "dashed", color = "grey 30")


p2 <- df_cor |> rownames_to_column("Feature") |> mutate(Type = ifelse(video_features %in% features_gait, "Gait", ifelse(video_features %in% features_engineered, "Engineered", "Open Field")), DO = abs(DO), B6 = abs(B6)) |> filter(DO != 0 | B6 != 0) |> ggplot(aes(x = B6, y = DO, color = Type)) + geom_point(size = 3) + scale_color_manual(values = c("Gait" = "#D8B70A", "Engineered" = "#02401B", "Open Field" = "#A2A475")) + theme_classic(base_size = 14) + expand_limits(x = 0, y = 0) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), legend.position = "none") + labs(x = expression("| r |"["Age, feature"] ~ "(B6J)"), y = expression("| r |"["Age, feature"] ~ "(DO)")) + geom_abline(linetype = "dashed", color = "grey 30")

p1 | p2


#For reproducing the remaining figures in the manuscript, please run the code in DO_expts.R

