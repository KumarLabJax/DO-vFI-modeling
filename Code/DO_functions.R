source("Code/DO_utils.R")

preprocess_data <- function(df, type){

	#'''
	#The preprocess_data function adjusts frailty scores for tester effect in the raw dataset
	#'''

	if (type == "frailty"){

		df <- df[,-which(names(df) %in% c("Tester","Sex","Weight","Batch","NetworkFilename"))]
		return(df)


	} else if (type == "B6") {
		mod.lmm <- lmer(score ~ AgeW + Weight + Sex + (1|Tester) + (1|Batch), data = df[!duplicated(df$MouseID),]) #mA
		mod.lmm1 <- update(mod.lmm, . ~ . - (1|Tester)) #m0
		m.tester <- update(mod.lmm, . ~ . - (1|Batch))
		RLRT <- c(RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$statistic,RLRT_stats <- RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$p.value)
		df$score <- ifelse(df$Tester == "Amanda", df$score - ranef(mod.lmm)$Tester["Amanda",],ifelse(df$Tester == "Gaven", df$score - ranef(mod.lmm)$Tester["Gaven",], ifelse(df$Tester == "Hannah", df$score - ranef(mod.lmm)$Tester["Hannah",], df$score - ranef(mod.lmm)$Tester["Mackenzie",])))
		df$score <- ifelse(df$Batch == 'Batch1', df$score - ranef(mod.lmm)$Batch["Batch1",], ifelse(df$Batch == "Batch2", df$score - ranef(mod.lmm)$Batch["Batch2",], df$score - ranef(mod.lmm)$Batch["Batch3",]))
		df$score[df$score < 0] <- 0
		#df <- df[,-which(names(df) %in% c("Tester","Sex","Weight","Batch","NetworkFilename"))]
		df_scorer <- data.frame(Scorer = row.names(ranef(mod.lmm)$Tester), RE = ranef(mod.lmm)$Tester[,1]) 
		df_scorer$Scorer <- c("Scorer1", "Scorer2", "Scorer3", "Scorer4")
		p_scorer <- ggplot(df_scorer, aes(x = Scorer, y = RE, fill = Scorer)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 16) + theme(legend.position = "none") + scale_fill_manual(values = c("Scorer1" = "#800000FF", "Scorer2" = "#D6D6CEFF", "Scorer3" = "#FFB547FF", "Scorer4" = "#ADB17DFF", "Scorer5" = "#5B8FA8FF", "Scorer6" = "#D49464FF", "Scorer7" = "#B1746FFF", "Scorer8" = "#8A8B79FF")) + xlab("Scorer") + ylab("Estimated RE") + scale_x_discrete("Scorer", labels = c(expression(S[1]), expression(S[2]), expression(S[3]), expression(S[4]))) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
		df_batch <- data.frame(Batch = row.names(ranef(mod.lmm)$Batch), RE = ranef(mod.lmm)$Batch[,1])
		p_batch <- ggplot(df_batch, aes(x = Batch, y = RE, fill = Batch)) + geom_bar(stat = "identity", color = "black") + theme_bw(base_size = 16) + theme(legend.position = "none") + scale_fill_brewer(palette = "Set1") + xlab("Batch") + ylab("Estimated RE")
		return(list(df = df, p_scorer = p_scorer, RLRT = RLRT, p_batch = p_batch, scorer_effects = ranef(mod.lmm)$Tester, batch_effects = ranef(mod.lmm)$Batch, var_percent = summary(mod.lmm)$varcor |> as_tibble() |> mutate(percent = sdcor/sum(sdcor)) |> select(grp, percent)))
	} else if (type == "combined"){
        df$Diet <- factor(df$Diet, levels = c("AL", "20", "40", "1D", "2D"))
        mod.lmm <- lmer(score ~ AgeW + Sex + (1|Diet) + (1|Tester) + (1|Batch), data = df[!duplicated(df$MouseID),]) #mA #Need to sex and add once I get the metadata for DO
		mod.lmm1 <- update(mod.lmm, . ~ . - (1|Tester)) #m0
        mod.lmm2 <- update(mod.lmm, . ~ . - (1|Diet))
		m.tester <- update(mod.lmm, . ~ . - (1|Batch))
		RLRT <- c(RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$statistic,RLRT_stats <- RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$p.value)
		df$score <- ifelse(df$Tester == "Amanda", df$score - ranef(mod.lmm)$Tester["Amanda",],ifelse(df$Tester == "Gaven", df$score - ranef(mod.lmm)$Tester["Gaven",],ifelse(df$Tester == "Hannah", df$score - ranef(mod.lmm)$Tester["Hannah",], ifelse(df$Tester == "Mackenzie", df$score - ranef(mod.lmm)$Tester["Mackenzie",], ifelse(df$Tester == "Farrar Cobb", df$score - ranef(mod.lmm)$Tester["Farrar Cobb",], ifelse(df$Tester == "Sean Deats", df$score - ranef(mod.lmm)$Tester["Sean Deats",], df$score - ranef(mod.lmm)$Tester["TesterNA",]))))))
		df$score <- ifelse(df$Batch == 'Batch1', df$score - ranef(mod.lmm)$Batch["Batch1",], ifelse(df$Batch == 'Batch2', df$score - ranef(mod.lmm)$Batch["Batch2",], ifelse(df$Batch == 'Batch3', df$score - ranef(mod.lmm)$Batch["Batch3",], ifelse(df$Batch == 'New', df$score - ranef(mod.lmm)$Batch["New",], df$score - ranef(mod.lmm)$Batch["Old",]))))
        df$score <- ifelse(df$Diet == 'AL', df$score - ranef(mod.lmm)$Diet["AL",], ifelse(df$Diet == '20', df$score - ranef(mod.lmm)$Diet["20",], ifelse(df$Diet == '40', df$score - ranef(mod.lmm)$Diet["40",], ifelse(df$Diet == '1D', df$score - ranef(mod.lmm)$Diet["1D",], df$score - ranef(mod.lmm)$Diet["2D",]))))
		df$score[df$score < 0] <- 0
		#df <- df[,-which(names(df) %in% c("Tester","Sex","Weight","Batch","NetworkFilename"))]
		df_score <- data.frame(Scorer = row.names(ranef(mod.lmm)$Tester), RE = ranef(mod.lmm)$Tester[,1])
		df_score$Scorer <- c("Scorer1", "Scorer2", "Scorer3", "Scorer4", "Scorer5", "Scorer6", "Scorer7")
		p_scorer <- ggplot(df_score, aes(x = Scorer, y = RE, fill = Scorer)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 16) + theme(legend.position = "none") + scale_fill_manual(values = c("Scorer1" = "#800000FF", "Scorer2" = "#D6D6CEFF", "Scorer3" = "#FFB547FF", "Scorer4" = "#ADB17DFF", "Scorer5" = "#5B8FA8FF", "Scorer6" = "#D49464FF", "Scorer7" = "#B1746FFF", "Scorer8" = "#8A8B79FF")) + xlab("Scorer") + ylab("Estimated RE") + scale_x_discrete("Scorer", labels = c(expression(S[1]), expression(S[2]), expression(S[3]), expression(S[4]), expression(S[5]), expression(S[6]), expression(S[7]))) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
		df_diet <- data.frame(Diet = row.names(ranef(mod.lmm)$Diet), RE = ranef(mod.lmm)$Diet[,1])
        p_diet <- ggplot(df_diet, aes(x = Diet, y = RE, fill = Diet)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 16) + theme(legend.position = "none") + scale_fill_manual(values = CalicoColors) + xlab("Diet") + ylab("Estimated RE") + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
		df_batch <- data.frame(Batch = row.names(ranef(mod.lmm)$Batch), RE = ranef(mod.lmm)$Batch[,1])
		p_batch <- ggplot(df_batch, aes(x = Batch, y = RE, fill = Batch)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 16) + theme(legend.position = "none") + scale_fill_brewer(palette = "Set1") + xlab("Batch") + ylab("Estimated RE") + scale_x_discrete("Batch", labels = c(expression(B[1]), expression(B[2]), expression(B[3]), expression(B[4]), expression(B[5]))) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
		return(list(df = df, p_scorer = p_scorer, p_batch = p_batch, p_diet = p_diet, RLRT = RLRT, scorer_effects = ranef(mod.lmm)$Tester, batch_effects = ranef(mod.lmm)$Batch, diet_effects = ranef(mod.lmm)$Diet, var_percent = summary(mod.lmm)$varcor |> as_tibble() |> mutate(percent = sdcor/sum(sdcor)) |> select(grp, percent)))
    }
    else {

		df$Diet <- factor(df$Diet, levels = c("AL", "20", "40", "1D", "2D"))
        mod.lmm <- lmer(score ~ AgeW + Sex + (1|Diet) + (1|Tester) + (1|Batch), data = df[!duplicated(df$MouseID),]) #mA #Need to sex and add once I get the metadata for DO
		mod.lmm1 <- update(mod.lmm, . ~ . - (1|Tester)) #m0
        mod.lmm2 <- update(mod.lmm, . ~ . - (1|Diet))
		m.tester <- update(mod.lmm, . ~ . - (1|Batch))
		RLRT <- c(RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$statistic,RLRT_stats <- RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$p.value)
		df$score <- ifelse(df$Tester == "Amanda", df$score - ranef(mod.lmm)$Tester["Amanda",],ifelse(df$Tester == "Gaven", df$score - ranef(mod.lmm)$Tester["Gaven",],ifelse(df$Tester == "Hannah", df$score - ranef(mod.lmm)$Tester["Hannah",], ifelse(df$Tester == "Mackenzie", df$score - ranef(mod.lmm)$Tester["Mackenzie",], ifelse(df$Tester == "Farrar Cobb", df$score - ranef(mod.lmm)$Tester["Farrar Cobb",], ifelse(df$Tester == "Sean Deats", df$score - ranef(mod.lmm)$Tester["Sean Deats",], df$score - ranef(mod.lmm)$Tester["TesterNA",]))))))
		df$score <- ifelse(df$Batch == 'New', df$score - ranef(mod.lmm)$Batch["New",], df$score - ranef(mod.lmm)$Batch["Old",])
        df$score <- ifelse(df$Diet == 'AL', df$score - ranef(mod.lmm)$Diet["AL",], ifelse(df$Diet == '20', df$score - ranef(mod.lmm)$Diet["20",], ifelse(df$Diet == '40', df$score - ranef(mod.lmm)$Diet["40",], ifelse(df$Diet == '1D', df$score - ranef(mod.lmm)$Diet["1D",], df$score - ranef(mod.lmm)$Diet["2D",]))))
		df$score[df$score < 0] <- 0
		#df <- df[,-which(names(df) %in% c("Tester","Sex","Weight","Batch","NetworkFilename"))]
		df_score <- data.frame(Scorer = row.names(ranef(mod.lmm)$Tester), RE = ranef(mod.lmm)$Tester[,1])
		df_score$Scorer <- c("Scorer1", "Scorer2", "Scorer5", "Scorer6","Scorer7")
		p_scorer <- ggplot(df_score, aes(x = Scorer, y = RE, fill = Scorer)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 16) + theme(legend.position = "none") + scale_fill_manual(values = c("Scorer1" = "#800000FF", "Scorer2" = "#D6D6CEFF", "Scorer3" = "#FFB547FF", "Scorer4" = "#ADB17DFF", "Scorer5" = "#5B8FA8FF", "Scorer6" = "#D49464FF", "Scorer7" = "#B1746FFF", "Scorer8" = "#8A8B79FF")) + xlab("Scorer") + ylab("Estimated RE") + scale_x_discrete("Scorer", labels = c(expression(S[1]), expression(S[2]), expression(S[5]), expression(S[6]), expression(S[7]))) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
		df_diet <- data.frame(Diet = row.names(ranef(mod.lmm)$Diet), RE = ranef(mod.lmm)$Diet[,1])
        p_diet <- ggplot(df_diet, aes(x = Diet, y = RE, fill = Diet)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 14) + theme(legend.position = "none") + scale_fill_manual(values = CalicoColors) + xlab("Diet") + ylab("Estimated RE") + theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
		df_batch <- data.frame(Batch = row.names(ranef(mod.lmm)$Batch), RE = ranef(mod.lmm)$Batch[,1])
		p_batch <- ggplot(df_batch, aes(x = Batch, y = RE, fill = Batch)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 14) + theme(legend.position = "none") + scale_fill_brewer(palette = "Set1") + xlab("Batch") + ylab("Estimated RE") + theme(axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14))
		return(list(df = df, p_scorer = p_scorer, p_batch = p_batch, p_diet = p_diet, RLRT = RLRT, scorer_effects = ranef(mod.lmm)$Tester, batch_effects = ranef(mod.lmm)$Batch, diet_effects = ranef(mod.lmm)$Diet, var_percent = summary(mod.lmm)$varcor |> as_tibble() |> mutate(percent = sdcor/sum(sdcor)) |> select(grp, percent)))

    }

}



evaluate_rf <- function(object, yname, mtry, min_n, trees, feature_type){

    #Model evaluation
    #' Evaluate Random Forest Model
    #'
    #' This function evaluates a random forest model using the specified parameters.
    #'
    #' @param object The random forest model object.
    #' @param yname The name of the response variable.
    #' @param mtry The number of variables randomly sampled as candidates at each split.
    #' @param min_n The minimum number of samples required to split a node.
    #' @param trees The number of trees to grow.
    #' @param feature_type The type of feature selection to be used.
    #'
    #' @return The evaluation results.
    #'

    y_complement <- setdiff(y_vars, yname)
    formula <- as.formula(paste(yname, "~ ."))

    if (feature_type == "video"){
        features_to_remove <- frailty_features
    } else if (feature_type == "frailty"){
        features_to_remove <- video_features
    } else {
        features_to_remove <- NULL
    }

    n_strains <- length(unique(analysis(object)$Strain))

    if (n_strains == 1){
        
        model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(meta_cols, covars, y_complement, features_to_remove)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    } else {

        model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(setdiff(meta_cols, "Strain"), covars, y_complement, features_to_remove)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())
    }

    model_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = trees) |> set_engine("randomForest") |> set_mode("regression")

    model_wf <- workflow() |> add_recipe(model_recipe) |> add_model(model_spec) |> fit(data = analysis(object))

    holdout_pred <- predict(model_wf, new_data = assessment(object)) |> bind_cols(assessment(object))

    results <- holdout_pred |> metrics(truth = yname, estimate = .pred) |> select(.metric, .estimate) |> select(.estimate) |> pull()

    return(results)
}

evaluate_xgb <- function(object, yname, tree_depth, learn_rate, loss_reduction, min_n, sample_size, trees, feature_type){

    y_complement <- setdiff(y_vars, yname)
    formula <- as.formula(paste(yname, "~ ."))

    if (feature_type == "video"){
        features_to_remove <- frailty_features
    } else if (feature_type == "frailty"){
        features_to_remove <- video_features
    } else {
        features_to_remove <- NULL
    }


    n_strains <- length(unique(analysis(object)$Strain))

    if (n_strains == 1){

        model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(meta_cols, covars, y_complement, features_to_remove)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    } else {

        model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(setdiff(meta_cols, "Strain"), covars, y_complement, features_to_remove)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())
    }

    model_spec <- boost_tree(tree_depth = tree_depth, learn_rate = learn_rate, loss_reduction = loss_reduction, min_n = min_n, sample_size = sample_size, trees = trees) |> set_engine("xgboost") |> set_mode("regression")

    model_wf <- workflow() |> add_recipe(model_recipe) |> add_model(model_spec) |> fit(data = analysis(object))

    holdout_pred <- predict(model_wf, new_data = assessment(object)) |> bind_cols(assessment(object))

    results <- holdout_pred |> metrics(truth = yname, estimate = .pred) |> select(.metric, .estimate) |> select(.estimate) |> pull()

    return(results)
}

evaluate_lm <- function(object, yname, penalty, mixture, feature_type){

    y_complement <- setdiff(y_vars, yname)
    formula <- as.formula(paste(yname, "~ ."))

    if (feature_type == "video"){
        features_to_remove <- frailty_features
    } else if (feature_type == "frailty"){
        features_to_remove <- video_features
    } else {
        features_to_remove <- NULL
    }
    
    n_strains <- length(unique(analysis(object)$Strain))

    if (n_strains == 1){

        model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(meta_cols, covars, y_complement, features_to_remove)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    } else {

        model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(setdiff(meta_cols, "Strain"), covars, y_complement, features_to_remove)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())
    }

    model_spec <- linear_reg(penalty = penalty, mixture = mixture) |> set_engine("glmnet") |> set_mode("regression")

    model_wf <- workflow() |> add_recipe(model_recipe) |> add_model(model_spec) |> fit(data = analysis(object))

    holdout_pred <- predict(model_wf, new_data = assessment(object)) |> bind_cols(assessment(object))

    results <- holdout_pred |> metrics(truth = yname, estimate = .pred) |> select(.metric, .estimate) |> select(.estimate) |> pull()

    return(results)

}

evaluate_mlp <- function(object, yname, hidden_units, penalty, epochs, activation = "relu", learn_rate, optimizer = "SGD"){

    y_complement <- setdiff(y_vars, yname)
    formula <- as.formula(paste(yname, "~ ."))

    n_strains <- length(unique(analysis(object)$Strain))

    if (n_strains == 1){

        model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(meta_cols, covars, y_complement, frailty_features, other_frailty_features)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    } else {

        model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(setdiff(meta_cols, "Strain"), covars, y_complement, frailty_features, other_frailty_features)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())
    }

    model_spec <- mlp(hidden_units = hidden_units, penalty = penalty, epochs = epochs, activation = activation, learn_rate = learn_rate) |> set_engine("brulee") |> set_mode("regression")

    model_wf <- workflow() |> add_recipe(model_recipe) |> add_model(model_spec) |> fit(data = analysis(object))

    holdout_pred <- predict(model_wf, new_data = assessment(object)) |> bind_cols(assessment(object))

    results <- holdout_pred |> metrics(truth = yname, estimate = .pred) |> select(.metric, .estimate) |> select(.estimate) |> pull()

    return(results)

}

#Tuning setup
tune_hyperparams <- function(object, yname, model, feature_type){
    if (model == "rf"){

        if(feature_type == "video"){
            features <- video_features
        } else if (feature_type == "frailty"){
            features <- frailty_features
        } else {
            features <- c(video_features, frailty_features)
        }

        model_spec <- rand_forest(mtry = tune(), min_n = tune(), trees=tune()) |> set_engine("randomForest") |> set_mode("regression")

        #rf_params <- extract_parameter_set_dials(model_spec) |> finalize(analysis(object) |> select(any_of(video_features))) 
        rf_params <- extract_parameter_set_dials(model_spec) |> finalize(analysis(object) |> select(any_of(features)))       

        grid_spec <- rf_params |> grid_latin_hypercube(size = 10)

        tune_results <- grid_spec |> pmap(function(mtry, min_n, trees){
            evaluate_rf(object, yname, mtry, min_n, trees, feature_type)
        })

        results <- do.call(rbind, tune_results)
        colnames(results) <- c("RMSE", "R2", "MAE")

        return(bind_cols(grid_spec, results))
    }

    else if (model == "xgb"){

        model_spec <- boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), min_n = tune(), sample_size = tune(), trees = tune()) |> set_engine("xgboost") |> set_mode("regression")

        xgb_params <- extract_parameter_set_dials(model_spec) |> finalize(analysis(object) |> select(all_of(features)))

        grid_spec <- xgb_params |> grid_latin_hypercube(size = 10)

        tune_results <- grid_spec |> pmap(function(tree_depth, learn_rate, loss_reduction, min_n, sample_size, trees){
            evaluate_xgb(object, yname, tree_depth, learn_rate, loss_reduction, min_n, sample_size, trees, feature_type)
        })

        results <- do.call(rbind, tune_results)
        colnames(results) <- c("RMSE", "R2", "MAE")

        return(bind_cols(grid_spec, results))
    }

    else if (model == "lm"){

        model_spec <- linear_reg(penalty = tune(), mixture = tune()) |> set_engine("glmnet") |> set_mode("regression")

        lm_params <- extract_parameter_set_dials(model_spec) |> finalize(analysis(object) |> select(all_of(features)))

        grid_spec <- lm_params |> grid_latin_hypercube(size = 2)

        tune_results <- grid_spec |> pmap(function(penalty, mixture){
            evaluate_lm(object, yname, penalty, mixture, feature_type)
        })

        results <- do.call(rbind, tune_results)
        colnames(results) <- c("RMSE", "R2", "MAE")

        return(bind_cols(grid_spec, results))
    
    }

    else if (model == "nnet"){

        model_spec <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune(), activation = "relu", learn_rate = tune()) |> set_engine("brulee") |> set_mode("regression")

        nnet_params <- extract_parameter_set_dials(model_spec) |> finalize(analysis(object) |> select(all_of(video_features)))

        grid_spec <- nnet_params |> grid_latin_hypercube(size = 2)

        tune_results <- grid_spec |> pmap(function(hidden_units, penalty, epochs, activation, learn_rate){
            evaluate_mlp(object, yname, hidden_units, penalty, epochs, activation = "relu", learn_rate)
        })

        results <- do.call(rbind, tune_results)
        colnames(results) <- c("RMSE", "R2", "MAE")

        return(bind_cols(grid_spec, results))
    }
}

#Summarize tuning results
tune_summary <- function(object, yname, model, feature_type){
    if (model == "rf"){
        map_df(object$splits, ~tune_hyperparams(.x, yname, "rf", feature_type)) |> group_by(mtry, min_n, trees) |> summarize_all(mean, .groups = "drop")
    } else if (model == "xgb"){
        map_df(object$splits, ~tune_hyperparams(.x, yname, "xgb", feature_type)) |> group_by(tree_depth, learn_rate, loss_reduction, min_n, sample_size, trees) |> summarize_all(mean, .groups = "drop")
    } else if (model == "lm"){
        map_df(object$splits, ~tune_hyperparams(.x, yname, "lm", feature_type)) |> group_by(penalty, mixture) |> summarize_all(mean, .groups = "drop")
    } else if (model == "nnet"){
        map_df(object$splits, ~tune_hyperparams(.x, yname, "nnet")) |> group_by(hidden_units, penalty, epochs, activation = "relu", learn_rate) |> summarize_all(mean, .groups = "drop")
    }
}

#A function to calculate metrics on the test set in the outer fold using the best hyperparameters obtained from the inner fold
rf_metrics <- function(object, yname, mtry, min_n, trees, feature_type){

    y_complement <- setdiff(y_vars, yname)
    formula <- as.formula(paste(yname, "~ ."))
    
    if (feature_type == "video"){
        features_to_remove <- frailty_features
    } else if (feature_type == "frailty"){
        features_to_remove <- video_features
    } else {
        features_to_remove <- NULL
    }

    model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(meta_cols, covars, y_complement, features_to_remove)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    model_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = trees) |> set_engine("randomForest") |> set_mode("regression") 
    
    model_wf <- workflow() |> add_recipe(model_recipe) |> add_model(model_spec) |> fit(data = analysis(object))

    holdout_pred <- predict(model_wf, assessment(object)) |> bind_cols(assessment(object) |> select(yname))

    fold_metrics <- c(rmse(holdout_pred, truth = yname, estimate = .pred)$.estimate, mae(holdout_pred, truth = yname, estimate = .pred)$.estimate, rsq(holdout_pred, truth = yname, estimate = .pred)$.estimate)

    fold_residuals <- holdout_pred |> mutate(residuals = .pred - !!rlang::sym(yname))

    fold_vimp <- vi(model_wf |> extract_fit_parsnip(), method = "model") |> as_tibble() 

    return(list(metrics = fold_metrics, residuals = fold_residuals, vimp = fold_vimp))

}

xgb_metrics <- function(object, yname, tree_depth, learn_rate, loss_reduction, min_n, sample_size, trees, feature_type){

    y_complement <- setdiff(y_vars, yname)
    formula <- as.formula(paste(yname, "~ ."))
    
    if (feature_type == "video"){
        features_to_remove <- frailty_features
    } else if (feature_type == "frailty"){
        features_to_remove <- video_features
    } else {
        features_to_remove <- NULL
    }

    model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(meta_cols, covars, y_complement, features_to_remove)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    model_spec <- boost_tree(tree_depth = tree_depth, learn_rate = learn_rate, loss_reduction = loss_reduction, min_n = min_n, sample_size = sample_size, trees = trees) |> set_engine("xgboost") |> set_mode("regression")
    
    model_wf <- workflow() |> add_recipe(model_recipe) |> add_model(model_spec) |> fit(data = analysis(object))

    holdout_pred <- predict(model_wf, assessment(object) |> select(-yname)) |> bind_cols(assessment(object) |> select(yname))
    
    fold_metrics <- c(rmse(holdout_pred, truth = yname, estimate = .pred)$.estimate, mae(holdout_pred, truth = yname, estimate = .pred)$.estimate, rsq(holdout_pred, truth = yname, estimate = .pred)$.estimate)

    fold_residuals <- holdout_pred |> mutate(residuals = .pred - !!rlang::sym(yname))

    fold_vimp <- vi(model_wf |> extract_fit_parsnip(), method = "model") |> as_tibble() 

    return(list(metrics = fold_metrics, residuals = fold_residuals, vimp = fold_vimp))

}

lm_metrics <- function(object, yname, penalty, mixture, feature_type){

    y_complement <- setdiff(y_vars, yname)
    formula <- as.formula(paste(yname, "~ ."))

    if (feature_type == "video"){
        features_to_remove <- frailty_features
    } else if (feature_type == "frailty"){
        features_to_remove <- video_features
    } else {
        features_to_remove <- NULL
    }

    model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(meta_cols, covars, y_complement, features_to_remove)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    model_spec <- linear_reg(penalty = penalty, mixture = mixture) |> set_engine("glmnet") |> set_mode("regression")

    model_wf <- workflow() |> add_recipe(model_recipe) |> add_model(model_spec) |> fit(data = analysis(object))

    holdout_pred <- predict(model_wf, assessment(object) |> select(-yname)) |> bind_cols(assessment(object) |> select(yname))

    fold_metrics <- c(rmse(holdout_pred, truth = yname, estimate = .pred)$.estimate, mae(holdout_pred, truth = yname, estimate = .pred)$.estimate, rsq(holdout_pred, truth = yname, estimate = .pred)$.estimate)

    fold_residuals <- holdout_pred |> mutate(residuals = .pred - !!rlang::sym(yname))

    #For feature importances, extract model coefficients 
    fold_vimp <- model_wf |> extract_fit_parsnip() |> tidy() |> filter(term != "(Intercept)") |> select(!penalty) |> rename("Importance" = estimate, "Variable" = term)

    return(list(metrics = fold_metrics, residuals = fold_residuals, vimp = fold_vimp))

}

nn_metrics <- function(object, yname, hidden_units, penalty, epochs, learn_rate){

    y_complement <- setdiff(y_vars, yname)
    formula <- as.formula(paste(yname, "~ ."))
    
    if (feature_type == "video"){
        features_to_remove <- frailty_features
    } else if (feature_type == "frailty"){
        features_to_remove <- video_features
    } else {
        features_to_remove <- NULL
    }

    model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c(meta_cols, covars, y_complement, frailty_features, other_frailty_features)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    model_spec <- mlp(hidden_units = hidden_units, penalty = penalty, epochs = epochs, activation = "relu", learn_rate = learn_rate) |> set_engine("brulee") |> set_mode("regression")

    model_wf <- workflow() |> add_recipe(model_recipe) |> add_model(model_spec) |> fit(data = analysis(object))

    holdout_pred <- predict(model_wf, assessment(object) |> select(-yname)) |> bind_cols(assessment(object) |> select(yname))

    fold_metrics <- c(rmse(holdout_pred, truth = yname, estimate = .pred)$.estimate, mae(holdout_pred, truth = yname, estimate = .pred)$.estimate, rsq(holdout_pred, truth = yname, estimate = .pred)$.estimate)

    fold_residuals <- holdout_pred |> mutate(residuals = .pred - !!rlang::sym(yname))

    #fold_vimp <- vi(model_wf |> extract_fit_parsnip(), method = "model") |> as_tibble()

    return(list(metrics = fold_metrics, residuals = fold_residuals, vimp = tibble()))

}

best_cost <- function(results, metric){
    results |> arrange(results[[metric]]) |> ungroup() |> dplyr::slice(1)
    } #Find the best hyperparameters based on the lowest cost metric


compare_age_vs_video <- function(data, seed, nfolds, nrepeats=1){

    set.seed(seed)
    folds <- group_vfold_cv(data, v = nfolds, group = "MouseID", repeats = nrepeats)

    y_complement <- setdiff(y_vars, c("score"))
    formula <- as.formula(paste("score", "~ ."))
    video_recipe <- recipe(formula, data = data) |> update_role(any_of(c("NetworkFilename", "PoseFilename", "Batch", "Tester", "AgeGroup", "MouseID", "Strain", "Diet", "Weight", "Sex", y_complement)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors()) #exclude AgeW

    age_recipe <- recipe(score ~ AgeAtVid, data = data) |> step_naomit(all_predictors()) |> step_normalize(all_numeric_predictors())

    y_complement <- setdiff(y_vars, c("score","AgeAtVid"))
    age_video_recipe <- recipe(score ~ ., data = data) |> update_role(any_of(c("NetworkFilename", "PoseFilename", "Batch", "Tester", "AgeGroup", "MouseID", "Strain", "Diet", "Weight", "Sex", y_complement)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors()) #includes AgeAtVid
    
    lm_spec <- linear_reg(penalty = NULL, mixture = NULL) |> set_engine("lm") |> set_mode("regression")

    gam_spec <- gen_additive_mod(select_features = TRUE, engine = "mgcv") |> set_engine("mgcv") |> set_mode("regression")

    rf_spec <- rand_forest(mtry = NULL, min_n = NULL, trees = NULL) |> set_engine("randomForest") |> set_mode("regression")

    keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

    results <- workflow_set(preproc = list(Age = age_recipe, Age = age_recipe, Video = video_recipe, AgeVideo = age_video_recipe), models = list(lm = lm_spec, gam = gam_spec, rf = rf_spec, rf = rf_spec), cross = FALSE) |> update_workflow_model(id = "Age_gam", spec = gam_spec, formula = as.formula("score ~ AgeAtVid")) |> workflow_map("fit_resamples", resamples = folds, control = keep_pred, metrics = metric_set(rmse, rsq, mae))

    df_results <- collect_metrics(results, summarize = FALSE) |> mutate(wflow_id = as.factor(wflow_id), .metric = as.factor(.metric))
    levels(df_results$.metric) <- c("MAE","RMSE", "R^2")
    levels(df_results$wflow_id) <- c("GAM", "Linear", "RF[both]", "RF")
    df_results$wflow_id <- factor(df_results$wflow_id, levels = c("Linear", "GAM", "RF", "RF[both]"))
    df_results <- df_results |> mutate(Featureset = as.factor(rep(c("Age", "Age", "Video", "AgeVideo"), each = 10*nfolds*3)))

    p <- ggplot(df_results, aes(x = wflow_id, y = .estimate, fill = Featureset)) + geom_boxplot(alpha = 0.8) + facet_wrap(~.metric, scales = "free_y", labeller = label_parsed) + theme_classic(base_size = 13) + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 13)) + labs(x = "Model", y = "Metric", title = "Age vs Video Features") + scale_fill_manual(values = wesanderson::wes_palette("Chevalier1")) + scale_x_discrete(label = parse(text = (levels(df_results$wflow_id)))) + guides(fill = guide_legend(title = "Feature Set"), size = 2)

    return(list(results = df_results, plot = p))

}


predict_out_of_strain <- function(train, test, yname, nfolds, seed){

    #Input: 
    #@train - a dataframe containing the training data
    #@test - a dataframe containing the test data
    #@yname: the name of the response variable
    #@seed: the random seed to use for reproducibility

    #This function trains multiple models (linear regression, random forest, and XGBoost) using cross-validation and hyperparameter tuning. The best model is selected based on the mean absolute error (MAE) metric. The selected model is then used to make predictions on a test dataset, and various evaluation metrics (RMSE, R-squared, MAE) are calculated. The function returns the evaluation results on the test set and the name of the best model.

    set.seed(seed)

    strain <- test |> select(Strain) |> distinct() |> pull() |> as.factor()

    if (!"FLL" %in% colnames(test)){
        test <- test |> mutate(FLL = rep(NA, nrow(test))) #Add a column of NAs to the test set if it doesn't already exist
    }

    folds <- group_vfold_cv(train, v = nfolds, group = "MouseID", repeats = 1)

    y_complement <- setdiff(y_vars, yname)
    formula <- as.formula(paste(yname, "~ ."))
    model_recipe <- recipe(formula, data = train) |> update_role(any_of(c("NetworkFilename", "PoseFilename", "Batch", "Tester", "AgeGroup", "MouseID", "Strain", "Diet", "Weight", "Sex", y_complement)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    lm_spec <- linear_reg(penalty = tune(), mixture = tune()) |> set_engine("glmnet") |> set_mode("regression")

    rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = tune()) |> set_engine("randomForest") |> set_mode("regression")

    xgb_spec <- boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), min_n = tune(), sample_size = tune(), trees = tune()) |> set_engine("xgboost") |> set_mode("regression")

    model_wf <- workflow_set(preproc = list(base = model_recipe), models = list(lm = lm_spec, rf = rf_spec, xgb = xgb_spec))

    keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

    tuned_models <- model_wf |> workflow_map("tune_grid", resamples = folds, grid = 5, control = keep_pred, metrics = metric_set(rmse, rsq, mae))

    #tmp <- map(tuned_models[["result"]], function(x) collect_metrics(x))

    tuned_models_mae <- map_dbl(tuned_models[["result"]], function(x) x |> show_best(metric = "mae") |> summarize(mae = mean(mean)) |> pull(mae))

    best_model <- ifelse(which.min(tuned_models_mae) == 1, "lm", ifelse(which.min(tuned_models_mae) == 2, "rf", "xgb"))

    best_results <- tuned_models |> extract_workflow_set_result(paste0("base_", best_model)) |> select_best(metric = "mae")

    final_model <- tuned_models |> extract_workflow(id = paste0("base_", best_model)) |> finalize_workflow(best_results) |> fit(train) 

    holdout_pred <- predict(final_model, new_data = test) |> bind_cols(test |> select(!!yname))

    test_metrics <- metric_set(rmse, rsq, mae)

    test_results <- test_metrics(holdout_pred, truth = !!yname, estimate = .pred)

    return(list(test_results = test_results, best_model = best_model))

}


predict_PLL <- function(data, nfolds, seed){

    set.seed(seed)

    folds <- group_vfold_cv(data, v = nfolds, group = "MouseID", repeats = 1)

    cv_metrics <- list()
    cv_metrics <- lapply(seq(4), function(x) {cv_metrics[[x]] <- data.frame(matrix(0, nfolds, 3)); colnames(cv_metrics[[x]]) <- c("MAE", "RMSE", "R2"); return(cv_metrics[[x]])})

    for (fold in 1:nfolds){

        cat("Fold = ", fold, "\n")
        train <- folds$splits[[fold]] |> analysis() |> select(-all_of(c(meta_cols, covars, "AgeW", "CFI_norm","score")))
        test <- folds$splits[[fold]] |> assessment() |> select(-all_of(c(meta_cols, covars, "AgeW", "CFI_norm","score")))

        base_recipe <- recipe(FLL ~ ., data = train) |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

        train <- base_recipe |> prep() |> bake(new_data = train)
        test <- base_recipe |> prep() |> bake(new_data = test)

        lm_mod <- betareg::betareg(FLL ~ ., data = train, control = betareg::betareg.control(fstol =1e-7))
        rf_mod <- ranger::ranger(FLL ~ ., data = train, splitrule = "beta")
        gamlss_mod <- gamlss::gamlss(FLL ~ ., data = train, family = BE) 
        gamboost_mod <- gamboostLSS::gamboostLSS(FLL ~ ., data = train, families = gamboostLSS::BetaLSS())

        lm_pred <- predict(lm_mod, newdata = test) |> as_tibble_col(column_name = ".pred") |> bind_cols(test |> select(FLL))

        gamlss_pred <- predict(gamlss_mod, newdata = test, what = "mu", type = "response") |> as_tibble_col(column_name = ".pred") |> bind_cols(test |> select(FLL))
        
        rf_pred <- predict(rf_mod, data = test)$predictions |> as_tibble_col(column_name = ".pred") |> bind_cols(test |> select(FLL))
        
        gamboost_pred <- as.numeric(predict(gamboost_mod, newdata = test, type = "response")$mu) |> as_tibble_col(column_name = ".pred") |> bind_cols(test |> select(FLL))

        test_metrics <- metric_set(rmse, rsq, mae)

        cv_metrics[[1]][fold,] <- test_metrics(lm_pred, truth = "FLL", estimate = .pred) |> select(.estimate) |> pull()
        cv_metrics[[2]][fold,] <- test_metrics(gamlss_pred, truth = "FLL", estimate = .pred) |> select(.estimate) |> pull()
        cv_metrics[[3]][fold,] <- test_metrics(rf_pred, truth = "FLL", estimate = .pred) |> select(.estimate) |> pull()
        cv_metrics[[4]][fold,] <- test_metrics(gamboost_pred, truth = "FLL", estimate = .pred) |> select(.estimate) |> pull()

    }

    results <- bind_rows(reshape2::melt(cv_metrics[[1]]), reshape2::melt(cv_metrics[[2]]), reshape2::melt(cv_metrics[[3]]), reshape2::melt(cv_metrics[[4]])) |> bind_cols(Model = rep(c("Beta Regression", "GAMLSS", "Random Forest", "GAMBoost"), each = 3*nfolds)) |> rename("Metric" = "variable")

    p <- ggplot(results, aes(x = Model, y = value, fill = Model)) + geom_boxplot() + geom_point() + theme_classic(base_size = 12) + theme(axis.text.x = element_text(angle = 90, size = 14, hjust = 1, vjust = 0.6), axis.text.y = element_text(size = 14)) + scale_fill_manual(values = c("#fb6a4a","#ef3b2c", "#a50f15", "#67000d")) + labs(y = "Metric") + facet_wrap(~Metric, scales = "free_y") + theme(legend.position = "none")

    return(list(results, p))

}

extract_median_error_splits <- function(data, results, seed){

    set.seed(seed) #Helps replicate the folds that were used to train and test models in the model_selection function
    folds <- nested_cv(data, outside = group_vfold_cv(v = 10, group = "MouseID", repeats = 5), inside = group_vfold_cv(v = 2, group = "MouseID", repeats = 1)) #Nested cross-validation with group specified as MouseID to include repeat measurements from the same mouse in the same fold

    strain <- data |> dplyr::select(Strain) |> distinct() |> pull() |> as.factor()

    if (length(strain) > 1){
        strain <- as.factor("All")
    }

    model <- results[[1]] |> filter(Strain == paste0(strain)) |> group_by(Model) |> summarize_at(c("MAE"), mean) |> dplyr::slice(which.min(MAE)) |> dplyr::select(Model) |> pull()

    train_test_split <- results[[1]] |> filter(Strain == paste0(strain) & Model == paste0(model)) |> arrange(MAE) |> dplyr::slice(0.5*length(row_number())) |> dplyr::select(splits) |> pluck(1) |> pluck(1) #Select the splits used to train and test the model with the median MAE 

    train <- analysis(train_test_split)
    test <- assessment(train_test_split)

    outer_folds <- bind_cols(results[[1]] |> filter(Strain == paste0(strain) & Model == paste0(model)), results$hyperparams[[paste0(model)]])

    hyperparams <- outer_folds |> arrange(MAE) |> dplyr::slice(0.5*length(row_number())) |> dplyr::select(-splits, -id, -inner_resamples, -RMSE, -MAE, -R2, -Model, -Strain) #Select the best hyperparameters for the model with the median MAE


    return(list(split = train_test_split, hyperparams = hyperparams, model = model))

}



plot_fold_residuals <- function(object, yname, hyperparams, model){

    strain <- analysis(object) |> select(Strain) |> distinct() |> pull() |> as.character()

    if (length(strain) > 1){
        strain <- "All"
    }

    y_complement <- setdiff(y_vars, yname)
    formula <- as.formula(paste(yname, "~ ."))
    model_recipe <- recipe(formula, data = analysis(object)) |> update_role(any_of(c("NetworkFilename", "PoseFilename", "Batch", "Tester", "AgeGroup", "MouseID", "Strain", "Diet", "Weight", "Sex", y_complement)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    if (model == "rf"){
        model_spec <- rand_forest(mtry = hyperparams$mtry, min_n = hyperparams$min_n, trees = hyperparams$trees) |> set_engine("randomForest") |> set_mode("regression")
    } else if (model == "xgb"){
        model_spec <- boost_tree(tree_depth = tree_depth, learn_rate = learn_rate, loss_reduction = loss_reduction, min_n = min_n, sample_size = sample_size, trees = trees) |> set_engine("xgboost") |> set_mode("regression")
    }
    
    model_wf <- workflow() |> add_recipe(model_recipe) |> add_model(model_spec) |> fit(data = analysis(object))

    insample_pred <- predict(model_wf, new_data = analysis(object)) |> bind_cols(analysis(object) |> select(yname)) |> mutate(residual = .pred - !!rlang::sym(yname))

    holdout_pred <- predict(model_wf, assessment(object)) |> bind_cols(assessment(object) |> select(yname)) |> mutate(residual = .pred - !!rlang::sym(yname))

    p1_insample <- ggplot(insample_pred, aes(x = 1:nrow(insample_pred), y = .pred)) + geom_point(color = ifelse(yname == "score", "#FC4E2A", "#045A8D")) + geom_smooth(method = "loess") + labs(title = "Residuals vs Index", x = "Index", y = "Residuals") + theme_classic() + theme_classic(base_size = 12) #todo

    p2_insample <- ggplot(insample_pred, aes(y = .pred, x = !!rlang::sym(yname))) + geom_point(shape = 19, color = ifelse(yname == "score", "#FC4E2A", "#045A8D"), size = 3, alpha = 0.8) + geom_abline(intercept = 0, lwd = 1, color = "black") + stat_smooth(method = "lm", color = "grey80", se = FALSE) + labs(title = "Predicted vs True", x = "FI (True)", y = "vFI (predicted)") + theme_classic(base_size = 12) + scale_x_continuous(breaks = seq(0,14,2)) + scale_y_continuous(breaks = seq(0,14,2)) + expand_limits(x = 0, y = 0) 

    p1_holdout <- ggplot(holdout_pred, aes(x = 1:nrow(holdout_pred), y = .pred)) + geom_point(color = ifelse(yname == "score", "#FC4E2A", "#045A8D")) + geom_smooth(method = "loess") + labs(title = "Residuals vs Index", x = "Index", y = "Residuals") + theme_classic() + theme_classic(base_size = 12) #todo

    p2_holdout <- ggplot(holdout_pred, aes(y = .pred, x = !!rlang::sym(yname))) + geom_point(shape = 19, color = ifelse(yname == "score", "#FC4E2A", "#045A8D"), size = 3, alpha = 0.8) + geom_abline(intercept = 0, lwd = 1, color = "black") + stat_smooth(method = "lm", color = "grey80", se = FALSE) + labs(title = "Predicted vs True", x = "FI (True)", y = "vFI (predicted)") + theme_classic(base_size = 12) + scale_x_continuous(breaks = seq(0,14,2)) + scale_y_continuous(breaks = seq(0,14,2)) + expand_limits(x = 0, y = 0)

    return(list(p1_insample, p2_insample, p1_holdout, p2_holdout))

}

plot_cv_results <- function(results, metric, yname){

    best_model <- results[["metrics"]] |> group_by(Model) |> summarize_at(c("RMSE","MAE", "R2"), mean) |> arrange(!!sym(metric)) |> dplyr::slice(1) |> select(Model) |> pull()

    df_metrics <- results[["metrics"]]
    df_residuals <- results[["residuals"]]
    df_vimp <- results[["vimp"]]
    
    nfolds <- df_vimp |> select(Folds) |> distinct() |> nrow()

    strain <- df_metrics |> select(Strain) |> distinct() |> pull() |> as.character()

    if(strain == "B6"){strain <- "B6J"}

    df_residuals <- df_residuals |> filter(df_residuals$.pred < 1.5*max(df_residuals |> select(!!rlang::sym(yname)))) |> filter(.pred > 0) #Remove outliers

    color_variable <- ifelse(rep(yname == "score", 3), RColorBrewer::brewer.pal(9, "YlOrRd")[seq(4,9,2)], RColorBrewer::brewer.pal(9, "PuBu")[seq(4,9,2)])
    color_variable <- ifelse(best_model == "PLM", color_variable[1], ifelse(best_model == "RF", color_variable[2], color_variable[3]))

    shape_variable <- ifelse(strain %in% c("DO","All"), 19, 17)

    if (strain == "All"){

        if (yname == "score"){

            p_pred <- df_residuals |> ggplot(aes(y = .pred, x = !!sym(yname))) + geom_point(shape = shape_variable, color = paste0(color_variable), size = 3, alpha = 0.8) + geom_abline(intercept = 0, lwd = 1, color = "black") + stat_smooth(method = "lm", color = "grey60", se = FALSE, aes(group = Folds)) + labs(title = paste0(strain), x = "FI (True)", y = "vFI (predicted)") + theme_classic(base_size = 12) + scale_x_continuous(breaks = seq(0,ceiling(max(df_residuals |> select(!!rlang::sym(yname)))),2)) + scale_y_continuous(breaks = seq(0,ceiling(max(df_residuals$.pred)),2)) + expand_limits(x = 0, y = 0) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))

        } else {

            max_lim <- max(ceiling(max(df_residuals |> select(!!rlang::sym(yname)))), ceiling(max(df_residuals$.pred)))

            p_pred <- df_residuals |> ggplot(aes(y = .pred, x = !!sym(yname))) + geom_point(shape = shape_variable, color = paste0(color_variable), size = 3, alpha = 0.8) + geom_abline(intercept = 0, lwd = 1, color = "black") + stat_smooth(method = "lm", color = "grey60", se = FALSE, aes(group = Folds)) + labs(title = paste0(strain), x = "True Age (in weeks)", y = "Predicted Age (in weeks)") + theme_classic(base_size = 12) + scale_x_continuous(breaks = seq(0,max_lim,50)) + scale_y_continuous(breaks = seq(0,max_lim,50)) + expand_limits(x = 0, y = 0) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))

        }
    }

    else {

        if (yname == "score"){

            p_pred <- df_residuals |> ggplot(aes(y = .pred, x = !!sym(yname))) + geom_point(shape = shape_variable, color = paste0(color_variable), size = 3, alpha = 0.8) + geom_abline(intercept = 0, lwd = 1, color = "black") + stat_smooth(method = "lm", color = "grey60", se = FALSE, aes(group = Folds)) + labs(title = paste0(strain), x = "FI (True)", y = "vFI (predicted)") + theme_classic(base_size = 12) + scale_x_continuous(breaks = seq(0,ceiling(max(df_residuals |> select(!!rlang::sym(yname)))),2)) + scale_y_continuous(breaks = seq(0,ceiling(max(df_residuals$.pred)),2)) + expand_limits(x = 0, y = 0) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))

        } else {

            max_lim <- max(ceiling(max(df_residuals |> select(!!rlang::sym(yname)))), ceiling(max(df_residuals$.pred)))

            p_pred <- df_residuals |> ggplot(aes(y = .pred, x = !!sym(yname))) + geom_jitter(shape = shape_variable, color = paste0(color_variable), size = 3, alpha = 0.8, width = 1) + geom_abline(intercept = 0, lwd = 1, color = "black", aes(group = Folds)) + stat_smooth(method = "lm", color = "grey60", se = FALSE, aes(group = Folds)) + labs(title = paste0(strain), x = "True Age (in weeks)", y = "Predicted Age (in weeks)") + theme_classic(base_size = 12) + scale_x_continuous(breaks = seq(0,max_lim,50)) + scale_y_continuous(breaks = seq(0,max_lim,50)) + expand_limits(x = 0, y = 0) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))

        }

    
    }


    p_vimp <- df_vimp |> filter(Model == best_model) |> group_by(Variable) |> summarize(mean = mean(Importance), se = sd(Importance)/sqrt(nfolds)) |> ggplot(aes(x = Variable, y = mean)) + geom_bar(stat = "identity", fill = color_variable) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.4) + labs(title = paste0(strain, " Feature Importance"), x = "Feature", y = "Importance") + theme_classic(base_size = 12) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text.y = element_text(size = 14))

    video_features <- df_vimp |> filter(Model == best_model) |> distinct(Variable) |> select(Variable) |> pull() |> sort()

    col_fun2 <- colorRamp2(c(1,2,3),c("#1B9E77", "#D95F02", "#E7298A"), transparency = 0)
    my_features <- data.frame(Type = ifelse(video_features %in% features_gait, 1, ifelse(video_features %in% features_engineered, 2, 3)))
    colnames(my_features) <- ""

    ht.cluster <- Heatmap(t(as.matrix(my_features)),cluster_rows = FALSE, cluster_columns = FALSE,col = col_fun2, show_heatmap_legend = FALSE,column_names_gp = gpar(fontsize = 10),row_names_gp = gpar(fontsize = 10),row_names_side = "left")

    return(list(pred_vs_true = p_pred, vimp = p_vimp, ht = ht.cluster))


}

plot_compare_data <- function(B6_results, DO_results, combined_results, metric, yname){

    model_selection_results <- bind_rows(B6_results[["metrics"]] |> mutate(Strain = as.factor("B6J")), DO_results[["metrics"]], combined_results[["metrics"]])
    levels(model_selection_results$Model) <- c("LM[alpha]", "RF", "XGB")

    color_values <- c(ifelse(rep(yname == "AgeAtVid",3), RColorBrewer::brewer.pal(9, "PuBu")[seq(4,9,2)], RColorBrewer::brewer.pal(9, "YlOrRd")[seq(4,9,2)]))

    y_lab <- ifelse(metric == "R2", expression(R^{2}), paste0(metric, " (weeks)"))

    p <- model_selection_results |> ggplot(aes(x = Strain, y = !!rlang::sym(metric), fill = Model)) + geom_boxplot() + theme_classic(base_size = 14) + labs(title = ifelse(yname == "score", "vFI clock", "vFRIGHT clock"), x = "Strain", y = y_lab) + scale_fill_manual(values = color_values,labels = scales::parse_format()) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))

    return(p)

}


plot_feature_importance <- function(B6_results, DO_results, combined_results, metric, yname){

    B6_best_model <- B6_results[["metrics"]] |> group_by(Model) |> summarize_at(c("RMSE","MAE", "R2"), mean) |> arrange(!!sym(metric)) |> dplyr::slice(1) |> select(Model) |> pull()

    DO_best_model <- DO_results[["metrics"]] |> group_by(Model) |> summarize_at(c("RMSE","MAE", "R2"), mean) |> arrange(!!sym(metric)) |> dplyr::slice(1) |> select(Model) |> pull()

    combined_best_model <- combined_results[["metrics"]] |> group_by(Model) |> summarize_at(c("RMSE","MAE", "R2"), mean) |> arrange(!!sym(metric)) |> dplyr::slice(1) |> select(Model) |> pull()

    best_models <- c(B6_best_model, DO_best_model, combined_best_model)

    best_model <- names(which.max(table(best_models)))

    B6_feature_importance <- B6_results[["vimp"]] |> filter(Model == best_model)
    DO_feature_importance <- DO_results[["vimp"]] |> filter(Model == best_model)
    combined_feature_importance <- combined_results[["vimp"]] |> filter(Model == best_model)

    df_vimp <- bind_rows(B6_feature_importance |> mutate(Strain = as.factor("B6J")), DO_feature_importance |> mutate(Strain = as.factor("DO")))


    p_vimp <- df_vimp |> group_by(Strain, Variable) |> summarize(mean = mean(Importance), se = sd(Importance)/sqrt(nfolds)) |> ggplot(aes(x = Variable, y = (mean), fill = Strain)) + geom_bar(position = position_dodge(width = 1), stat = "identity") + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.4, position = position_dodge(width = 1)) + labs(title = paste0("Feature Importance", ifelse(yname == "score", " (vFI)", " (vFRIGHT)")), x = "Feature", y = "Importance") + theme_classic(base_size = 12) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text.y = element_text(size = 14)) + scale_fill_brewer(palette = "Set1")

    video_features <- df_vimp |> filter(Model == best_model) |> distinct(Variable) |> select(Variable) |> pull() |> sort()

    col_fun2 <- colorRamp2(c(1,2,3),c("#1B9E77", "#D95F02", "#E7298A"), transparency = 0)
    my_features <- data.frame(Type = ifelse(video_features %in% features_gait, 1, ifelse(video_features %in% features_engineered, 2, 3)))
    colnames(my_features) <- ""

    ht.cluster <- Heatmap(t(as.matrix(my_features)),cluster_rows = FALSE, cluster_columns = FALSE,col = col_fun2, show_heatmap_legend = FALSE,column_names_gp = gpar(fontsize = 10),row_names_gp = gpar(fontsize = 10),row_names_side = "left")

    return(list(p_vimp, ht.cluster))

}


covariate_effect_manualFI <- function(B6FI, DOFI){

    #Manual frailty data
    dfDOFI <- DOFI |> mutate(Strain = as.factor("DO")) |> rename("Weight" = "body_weight")

    dfB6FI <- B6FI |> mutate(Strain = as.factor("B6"), Diet = as.factor("AL")) |> janitor::clean_names() |> rename("MouseID" = "mouse_id", "Batch" = "batch", "Sex" = "sex", "Tester" = "tester", "AgeAtTest" = "age_at_test", "Weight" = "weight", "Strain" = "strain", "Diet" = "diet")

    levels(dfB6FI$Tester) <- list(Amanda = "Scorer1", Gaven = "Scorer2", Hannah = "Scorer3", Mackenzie = "Scorer4")

    tmp <- dfDOFI |> select(any_of(c("Diet",frailty_features))) |> gather(variable, value, -Diet) |> mutate(variable = as.factor(variable), value = as.factor(value))

    common_frailty_features <- intersect(intersect(names(dfDOFI), names(dfB6FI)), frailty_features)

    dfFI <- bind_rows(dfDOFI, dfB6FI) |> select(all_of(c("MouseID", "Strain", "Diet", "Weight", "Sex", "Tester", common_frailty_features))) |> mutate_at(c(common_frailty_features, "Diet"), factor) 

    dfFI$Tester <- as.character(dfFI$Tester)
    dfFI$Tester <- as.factor(ifelse(is.na(dfFI$Tester), ("Tester"), dfFI$Tester))

    zero_variance_features <- names(which(apply(dfFI[,common_frailty_features], 2, function(x) var(x, na.rm = TRUE)) == 0))

    zero_variance_features <- c(zero_variance_features, "cataracts", "corneal_opacity", "diarrhea", "malocclusions", "rectal_prolapse")

    dfFI$Diet <- relevel(dfFI$Diet, ref = "AL")
    dfFI$Strain <- relevel(dfFI$Strain, ref = "B6")

    fct_to_numeric <- function(x) as.numeric(levels(dfFI |> pull(.x)))[dfFI |> pull(.x)]



    df_results <- tibble(Feature = common_frailty_features) |> arrange(Feature)

    df_strain_effects <- map(setdiff(common_frailty_features, zero_variance_features), ~(clm(as.formula(paste0(.x, " ~ Weight + Diet + Strain")), data = dfFI))) |> map(tidy) |> map(~filter(., term == "StrainDO")) |> map(~select(., statistic, p.value)) |> do.call(what = "rbind") |> mutate(Feature = setdiff(common_frailty_features, zero_variance_features), .before = "statistic") |> arrange(Feature) |> mutate(p.value = p.adjust(p.value, method = "fdr"))

    df_results <- left_join(df_results, df_strain_effects, by = "Feature")

    df_results <- df_results |> mutate(pvalSignif = symnum(p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", "+", " ")))

    pvalSignif <- df_results |> pull(pvalSignif)

    col_fun <- circlize::colorRamp2(c(min(df_results$statistic, na.rm = TRUE),0,0,max(df_results$statistic, na.rm = TRUE)),c("#0072B5FF","#f7f7f7","#f7f7f7","#BC3C29FF"))
    

    p <- ComplexHeatmap::Heatmap(t(as.matrix(df_results |> select(statistic))), row_names_gp = gpar(fontsize = 10),row_names_side = "left", column_names_gp = gpar(fontsize = 10), column_names_side = "bottom", heatmap_legend_param = list(at = c(round(min(df_results$statistic, na.rm = TRUE),2),0,round(max(df_results$statistic, na.rm = TRUE),2)),title = latex2exp::TeX("Coefficient"), title_gp = gpar(fontsize = 12),title_position = "lefttop", border = "black",legend_height = unit(4, "cm"), labels_gp = gpar(fontsize = 12),just = c("right", "top"), legend_direction = "horizontal"), col = col_fun, cluster_rows = FALSE, cluster_columns = FALSE, border = TRUE,cell_fun = function(j, i, x, y, width, height, fill) {grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "grey"))
     grid.text((pvalSignif[j]),x, gp = gpar(fontsize = 8))})



    #p2 <- df_strain_effects |> filter(p.value < 0.05) |> ggplot(aes(x = Feature, y = statistic)) + geom_bar(stat = "identity") + theme_classic(base_size = 14) + labs(x = "Frailty Feature", y = "Coefficient") + coord_flip()

    return(list(results = df_strain_effects, plot = p))


}

estimate_frailty_re <- function(B6FI, DOFI){
    
    #Manual frailty data
    dfDOFI <- DOFI |> mutate(Strain = as.factor("DO")) |> rename("Weight" = "body_weight")
    dfB6FI <- B6FI |> mutate(Strain = as.factor("B6"), Diet = as.factor("AL")) |> janitor::clean_names() |> rename("MouseID" = "mouse_id", "Batch" = "batch", "Sex" = "sex", "Tester" = "tester", "AgeAtTest" = "age_at_test", "Weight" = "weight", "loss_of_fur_color" = "loss_of_fur_colour", "vaginal_uterine_prolapse" = "vaginal_uterine", "eye_discharge" = "eye_discharge_swelling", "tumors" = "tumours", "CFI" = "overall_score", "Strain" = "strain", "Diet" = "diet") 

    df_results <- tibble(Feature = common_frailty_features) |> arrange(Feature)

    df_strain_effects <- map(setdiff(common_frailty_features, zero_variance_features), ~clm(as.formula(paste0(.x, " ~ Weight + Sex + Diet + Strain")), data = dfFI)) |> map(tidy) |> map(~filter(., term == "StrainDO")) |> map(~select(., estimate, p.value)) |> do.call(what = "rbind") |> mutate(Feature = setdiff(common_frailty_features, zero_variance_features), .before = "estimate") |> arrange(Feature) |> mutate(p.value = p.adjust(p.value, method = "fdr"))

    mod.lmm <- lmer(score ~ AgeW + Sex + (1|Diet) + (1|Tester) + (1|Batch), data = df[!duplicated(df$MouseID),])

    return(list(results = df_strain_effects, plot = p))

}



predict_PLL_video_vs_frailty <- function(data, yname, nfolds, seed, endpoint = NULL){
    
    set.seed(seed)

    frailty_features_missing_atleast_100 <- data |> summarize(across(everything(), ~ sum(is.na(.x)))) |> select_if(~any(. > 100)) |> names()

    frailty_features_zero_variance <- data |> summarize(across(where(is.numeric), ~ sd((.x)))) |> select_if(~any(. == 0)) |> names()

    frailty_features_to_remove <- intersect(frailty_features, c(frailty_features_missing_atleast_100, frailty_features_zero_variance))

    longitudinal_mIDs <- names(table(data$MouseID)[table(data$MouseID) > 1])
    unique_mIDs <- setdiff(data$MouseID, longitudinal_mIDs)

    dftmp <- data |> filter(MouseID %in% unique_mIDs)

    if (is.numeric(endpoint)){     
        dftmp2 <- data |> filter(MouseID %in% longitudinal_mIDs) |> group_by(MouseID) |> arrange(MouseID, desc(AgeAtVid)) |> filter(row_number() %in% c(endpoint), .keep_all = TRUE) |> ungroup()
        data <- bind_rows(dftmp, dftmp2)
    } else if (is.character(endpoint)){
        dftmp2 <- data |> filter(MouseID %in% longitudinal_mIDs) |> group_by(MouseID) |> arrange(MouseID, desc(AgeAtVid)) |> sample_n(1, replace = FALSE) |> ungroup()
        data <- bind_rows(dftmp, dftmp2)
    } else {
        dftmp2 <- data |> filter(MouseID %in% longitudinal_mIDs) |> group_by(MouseID) |> arrange(MouseID, desc(AgeAtVid)) |> ungroup()
        data <- bind_rows(dftmp, dftmp2)
    }

    data$MouseID <- droplevels(data$MouseID)
    cat("Number of unique mice = ", length(unique(data$MouseID)), "\n")

    data <- data |> mutate(Longitudinal = as.factor(ifelse(MouseID %in% longitudinal_mIDs, "Yes", "No")))

    data <- data |> group_by(MouseID) |> mutate(Number = as.factor(seq_along(MouseID))) |> ungroup()

    folds <- group_vfold_cv(data, v = nfolds, group = "MouseID", repeats = 1)

    cv_metrics <- list()
    cv_metrics <- lapply(seq(6), function(x) {cv_metrics[[x]] <- data.frame(matrix(0, nfolds, 3)); colnames(cv_metrics[[x]]) <- c("RMSE", "R2", "MAE"); return(cv_metrics[[x]])})

    cv_residuals <- list()

    y_complement <- setdiff(y_vars, yname)

    for (fold in 1:nfolds){

        cat("Fold = ", fold, "\n")

        train <- folds$splits[[fold]] |> analysis() |> select(-any_of(c(meta_cols, covars, y_complement, frailty_features_to_remove)))
        
        test <- folds$splits[[fold]] |> assessment() |> select(-any_of(c(meta_cols, covars, y_complement, frailty_features_to_remove)))

        base_recipe <- recipe(FLL ~ ., data = train) |> update_role(any_of(c("CFI_norm", "AgeW")), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

        train <- base_recipe |> prep() |> bake(new_data = train)
        test <- base_recipe |> prep() |> bake(new_data = test)

        frailty_lm <- betareg::betareg(FLL ~ ., data = train |> select(any_of(c("FLL", frailty_features))), control = betareg::betareg.control(fstol =1e-5))

        video_lm <- betareg::betareg(FLL ~ ., data = train |> select(any_of(c("FLL", video_features))), control = betareg::betareg.control(fstol =1e-5))

        frailty_rf <- ranger::ranger(FLL ~ ., data = train |> select(any_of(c("FLL", frailty_features))), splitrule = "beta")

        video_rf <- ranger::ranger(FLL ~ ., data = train |> select(any_of(c("FLL", video_features))), splitrule = "beta")

        frailty_video_lm <- betareg::betareg(FLL ~ ., data = train |> select(any_of(c("FLL", frailty_features, video_features))), control = betareg::betareg.control(fstol =1e-5))

        frailty_video_rf <- ranger::ranger(FLL ~ ., data = train |> select(any_of(c("FLL", frailty_features, video_features))), splitrule = "beta")

        frailty_lm_pred <- predict(frailty_lm, test) |> as_tibble_col(column_name = ".pred") |> bind_cols(test |> select(FLL))

        video_lm_pred <- predict(video_lm, test) |> as_tibble_col(column_name = ".pred") |> bind_cols(test |> select(FLL))

        frailty_rf_pred <- predict(frailty_rf, data = test)$predictions |> as_tibble_col(column_name = ".pred") |> bind_cols(test |> select(FLL))

        video_rf_pred <- predict(video_rf, data = test)$predictions |> as_tibble_col(column_name = ".pred") |> bind_cols(test |> select(FLL))

        frailty_video_lm_pred <- predict(frailty_video_lm, test) |> as_tibble_col(column_name = ".pred") |> bind_cols(test |> select(FLL))

        frailty_video_rf_pred <- predict(frailty_video_rf, data = test)$predictions |> as_tibble_col(column_name = ".pred") |> bind_cols(test |> select(FLL))

        cv_residuals[[fold]] <- video_rf_pred |> mutate(Fold = paste0("Fold", fold))

        test_metrics <- metric_set(rmse, rsq, mae)

        cv_metrics[[1]][fold,] <- test_metrics(frailty_lm_pred, truth = "FLL", estimate = .pred) |> select(.estimate) |> pull()

        cv_metrics[[2]][fold,] <- test_metrics(video_lm_pred, truth = "FLL", estimate = .pred) |> select(.estimate) |> pull()

        cv_metrics[[3]][fold,] <- test_metrics(frailty_video_lm_pred, truth = "FLL", estimate = .pred) |> select(.estimate) |> pull()

        cv_metrics[[4]][fold,] <- test_metrics(frailty_rf_pred, truth = "FLL", estimate = .pred) |> select(.estimate) |> pull()

        cv_metrics[[5]][fold,] <- test_metrics(video_rf_pred, truth = "FLL", estimate = .pred) |> select(.estimate) |> pull()

        cv_metrics[[6]][fold,] <- test_metrics(frailty_video_rf_pred, truth = "FLL", estimate = .pred) |> select(.estimate) |> pull()

    }

    results <- bind_rows(reshape2::melt(cv_metrics[[1]]), reshape2::melt(cv_metrics[[2]]), reshape2::melt(cv_metrics[[3]]), reshape2::melt(cv_metrics[[4]]), reshape2::melt(cv_metrics[[5]]), reshape2::melt(cv_metrics[[6]])) |> bind_cols(Model = rep(c("LM", "LM", "LM", "RF", "RF", "RF"), each = 3*nfolds)) |> rename("Metric" = "variable") |> bind_cols(Features = rep(c("Frailty", "Video", "Video_Frailty", "Frailty", "Video", "Video_Frailty"), each = 3*nfolds))

    levels(results$Metric) <- c('RMSE','R^2', 'MAE')
    results$Metric <- factor(results$Metric, levels = c("MAE", "RMSE", "R^2"))
    #results$Features <- as.factor(results$Features)

    p <- ggplot(results, aes(x = Model, y = value, fill = Features)) + geom_boxplot(alpha = 0.8) + theme_classic(base_size = 14) + theme(axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13)) + scale_fill_brewer(palette = "Dark2") + labs(y = "Metric") + facet_wrap(~Metric, scales = "free_y", labeller = label_parsed) + scale_fill_manual(values = wes_palette("Royal1"), name = "Feature Set", labels = c("Frailty", "Video", "Video+Frailty")) + guides(fill = guide_legend(title = "Feature Set"), size = 2) 

    plot_data <- ggplot(data, aes(x = AgeAtVid, y = score, color = Longitudinal, shape = Number)) + geom_jitter(width = 5, size = 3) + theme_classic(base_size = 13) + labs(x = "Age (in weeks)", y = "CFI")+ scale_color_brewer(palette = "Set1") + theme(axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13)) 

    plot_scatter <- ggplot(data, aes(y = score, x = FLL)) + geom_point() + theme_classic(base_size = 13) + stat_smooth(method = "lm") + labs(x = "Proportion of Life Lived (PLL)", y = "CFI")+ scale_color_brewer(palette = "Set1") + theme(axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13)) 

    plot_density <- ggplot(data, aes(x = FLL)) + geom_density() + theme_classic(base_size = 13) + labs(x = "Proportion of Life Lived (PLL)", y = "Density") + theme(axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13))

    df_residuals <- do.call(rbind, cv_residuals)

    p_residuals <- df_residuals |> ggplot(aes(y = .pred, x = !!sym(yname))) + geom_point(size = 3, alpha = 0.8) + geom_abline(intercept = 0, lwd = 1, color = "black") + stat_smooth(method = "lm", color = "grey60", se = FALSE, aes(group = Fold)) + labs(x = "PLL (True)", y = "vPLL (predicted)") + theme_classic(base_size = 12)  + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))


    return(list(results = results, plot = p, plot_data = plot_data, plot_scatter = plot_scatter, plot_density = plot_density, plot_residuals = p_residuals))

}



predict_age_frailty_vs_video <- function(data, nfolds, seed, yname){

    set.seed(seed)

    frailty_features_missing_atleast_100 <- data |> summarize(across(everything(), ~ sum(is.na(.x)))) |> select_if(~any(. > 100)) |> names()

    frailty_features_zero_variance <- data |> summarize(across(where(is.numeric), ~ sd((.x)))) |> select_if(~any(. == 0)) |> names()

    frailty_features_to_remove <- intersect(frailty_features, c(frailty_features_missing_atleast_100, frailty_features_zero_variance))

    folds <- group_vfold_cv(data, v = nfolds, group = "MouseID", repeats = 1)

    y_complement <- setdiff(y_vars, yname)
    formula <- as.formula(paste(yname, "~ ."))


    video_recipe <- recipe(formula, data = data) |> update_role(any_of(c(meta_cols, covars, y_complement, frailty_features, other_frailty_features)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    frailty_recipe <- recipe(formula, data = data) |> update_role(any_of(c(meta_cols, covars, y_complement, frailty_features_to_remove, video_features)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    video_frailty_recipe <- recipe(formula, data) |> update_role(any_of(c(meta_cols, covars, y_complement, frailty_features_to_remove)), new_role = "ID") |> step_naomit(all_predictors()) |> step_dummy(all_nominal_predictors()) |> step_zv(all_numeric_predictors()) |> step_normalize(all_numeric_predictors())

    lm_spec <- linear_reg(penalty = tune(), mixture = tune()) |> set_engine("glmnet") |> set_mode("regression")

    rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = tune()) |> set_engine("randomForest") |> set_mode("regression")

    xgb_spec <- boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), min_n = tune(), sample_size = tune(), trees = tune()) |> set_engine("xgboost") |> set_mode("regression")

    model_wf <- workflow_set(preproc = list(frailty = frailty_recipe, video = video_recipe, video_fraitly = video_frailty_recipe), models = list(rf = rf_spec, xgb = xgb_spec), cross = TRUE)

    keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

    results <- workflow_set(preproc = list(frailty = frailty_recipe, video = video_recipe, video_frailty = video_frailty_recipe), models = list(rf = rf_spec, xgb = xgb_spec), cross = TRUE) |> workflow_map("tune_grid", resamples = folds, control = keep_pred, metrics = metric_set(rmse, rsq, mae))

    df_results <- collect_metrics(results, summarize = FALSE) |> mutate(wflow_id = as.factor(wflow_id), .metric = as.factor(.metric))

    levels(df_results$.metric) <- c("MAE","RMSE", "R2")
    levels(df_results$wflow_id) <- c("Frailty_RF", "Frailty_XGB", "Video_RF", "Video_XGB", "Video_Frailty_RF", "Video_Frailty_XGB")
    df_results$wflow_id <- factor(df_results$wflow_id, c("Frailty_RF", "Frailty_XGB", "Video_RF", "Video_XGB", "Video_Frailty_RF", "Video_Frailty_XGB"))

    df_results <- df_results |> mutate(Type = rep(c("Frailty", "Video", "VideoFrailty"), each = 60*nfolds))


    levels(df_results$.metric) <- c('MAE','RMSE','R^2')

    p <- ggplot(df_results, aes(x = wflow_id, y = .estimate, fill = Type)) + geom_boxplot(alpha = 0.8) + geom_point() + facet_wrap(~ .metric, scales = "free_y", labeller = label_parsed) + theme_classic(base_size = 13) + theme(axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13)) + labs(x = "Model", y = "Metric (weeks)")  + scale_fill_manual(values = wes_palette("Royal1")) + guides(fill = guide_legend(title = "Feature Set"), size = 2) + scale_x_discrete(labels = c("Frailty_RF" = "RF", "Frailty_XGB" = "XGB", "Video_RF" = "RF", "Video_XGB" = "XGB", "Video_Frailty_RF" = "RF", "Video_Frailty_XGB" = "XGB"))

    p <- ggplot(df_results |> filter(.metric == "MAE"), aes(x = wflow_id, y = .estimate, fill = Type)) + geom_boxplot(alpha = 0.8) + geom_point() + theme_classic(base_size = 13) + theme(axis.text.x = element_text(size = 13), axis.text.y = element_text(size = 13)) + labs(x = "Model", y = "Metric (weeks)")  + scale_fill_manual(values = wes_palette("Royal1")) + guides(fill = guide_legend(title = "Feature Set"), size = 2) + scale_x_discrete(labels = c("Frailty_RF" = "RF", "Frailty_XGB" = "XGB", "Video_RF" = "RF", "Video_XGB" = "XGB", "Video_Frailty_RF" = "RF", "Video_Frailty_XGB" = "XGB"))

    return(list(results = df_results, plot = p))

}

model_selection <- function(data, yname, models, seed, strain, feature_type){
    
    set.seed(seed)

    if(feature_type == "video"){
        data <- data |> select(any_of(c(meta_cols, covars, y_vars, video_features)))
    } else if (feature_type == "frailty"){
        
        frailty_features_missing_atleast_100 <- data |> summarize(across(everything(), ~ sum(is.na(.x)))) |> select_if(~any(. > 100)) |> names()

        frailty_features_zero_variance <- data |> summarize(across(where(is.numeric), ~ sd((.x)))) |> select_if(~any(. == 0)) |> names()

        frailty_features_to_keep <- setdiff(frailty_features, c(frailty_features_missing_atleast_100, frailty_features_zero_variance))
        
        data <- data |> select(any_of(c(meta_cols, covars, y_vars, frailty_features_to_keep)))
    } else {
        
        frailty_features_missing_atleast_100 <- data |> summarize(across(everything(), ~ sum(is.na(.x)))) |> select_if(~any(. > 100)) |> names()

        frailty_features_zero_variance <- data |> summarize(across(where(is.numeric), ~ sd((.x)))) |> select_if(~any(. == 0)) |> names()

        frailty_features_to_keep <- setdiff(frailty_features, c(frailty_features_missing_atleast_100, frailty_features_zero_variance))

        data <- data |> select(any_of(c(meta_cols, covars, y_vars, video_features, frailty_features_to_keep)))
    }

    data <- data |> filter(Strain %in% strain)
    
    folds <- nested_cv(data, outside = group_vfold_cv(v = 2, group = "MouseID", repeats = 1), inside = group_vfold_cv(v = 2, group = "MouseID", repeats = 1)) #Nested cross-validation with group specified as MouseID to include repeat measurements from the same mouse in the same fold
    
    models <- models
    model_results <- list()
    model_residuals <- list()
    model_vimp <- list()
    hyperparams <- list()

    for (model in models){

        cat("Tuning & Evaluating", model, "model\n")
    
        results_inner <- map(folds$inner_resamples, ~tune_summary(.x, yname, model, feature_type)) #Tune model hyperparameters using inner folds 
 
        cost_vals <- results_inner |> map_df(best_cost, "MAE") #Gives the cost values for the best hyperparameters among the inner folds for each outer fold

        best_hyperparams <- results_inner |> map_df(best_cost, "MAE") |> select(-RMSE, -R2, -MAE)#Gives the best hyperparameters among the inner folds for each outer fold

        hyperparams[[model]] <- best_hyperparams

        outer_folds <- bind_cols(folds, best_hyperparams) 

        #Unable to find an elegant way to pass the best hyperparameters to the rf_metrics function
        tmp_metrics <- list()
        tmp_residuals <- list()
        tmp_vimp <- list()

        if (model == "rf"){
            for (f in 1:nrow(outer_folds)) {
                tmp <- rf_metrics(outer_folds$splits[[f]], yname, outer_folds$mtry[f], outer_folds$min_n[f], outer_folds$trees[f], feature_type)
                tmp_metrics[[f]] <- tmp[["metrics"]]
                tmp_residuals[[f]] <- tmp[["residuals"]]
                tmp_vimp[[f]] <- tmp[["vimp"]]
            }}
        else if (model == "xgb"){
            for (f in 1:nrow(outer_folds)) {
                tmp <- xgb_metrics(outer_folds$splits[[f]], yname, outer_folds$tree_depth[f], outer_folds$learn_rate[f], outer_folds$loss_reduction[f], outer_folds$min_n[f], outer_folds$sample_size[f], outer_folds$trees[f], feature_type)
                tmp_metrics[[f]] <- tmp[["metrics"]]
                tmp_residuals[[f]] <- tmp[["residuals"]]
                tmp_vimp[[f]] <- tmp[["vimp"]]
                }
        }
        else if (model == "lm"){
            for (f in 1:nrow(outer_folds)) {
                tmp <- lm_metrics(outer_folds$splits[[f]], yname, outer_folds$penalty[f], outer_folds$mixture[f], feature_type)
                tmp_metrics[[f]] <- tmp[["metrics"]]
                tmp_residuals[[f]] <- tmp[["residuals"]]
                tmp_vimp[[f]] <- tmp[["vimp"]]
                }
        } else if (model == "nnet"){
            for (f in 1:nrow(outer_folds)) {
                tmp <- nn_metrics(outer_folds$splits[[f]], yname, outer_folds$hidden_units[f], outer_folds$penalty[f], outer_folds$epochs[f], outer_folds$learn_rate[f])
                tmp_metrics[[f]] <- tmp[["metrics"]]
                tmp_residuals[[f]] <- tmp[["residuals"]]
                tmp_vimp[[f]] <- tmp[["vimp"]]
                }
        }
        
        if (length(strain) > 1){
            Strain <- "All"
        } else {
            Strain <- strain
        }

        results <- tmp_metrics |> do.call(what = "rbind") |> as_tibble(.name_repair = "unique") |> setNames(c("RMSE", "MAE", "R2")) #Calculate metrics on the test set in the outer fold using the best hyperparameters obtained from the inner fold

        outer_folds <- outer_folds |> bind_cols(results) |> mutate(Model = as.factor(model), Strain = as.factor(Strain)) #Combine the best hyperparameters and the metrics

        model_results[[model]] <- outer_folds |> select(splits, id, inner_resamples, RMSE, MAE, R2, Model, Strain) 

        model_residuals[[model]] <- map(seq(lengths(tmp_residuals)), ~ tmp_residuals[[.x]] |> mutate(Folds = as.factor(paste0("Fold_", .x)))) |> do.call(what = "rbind") |> mutate(Model = as.factor(model), Strain = as.factor(Strain))

        model_vimp[[model]] <- map(seq(lengths(tmp_vimp)), ~ tmp_vimp[[.x]] |> mutate(Folds = as.factor(paste0("Fold_", .x)))) |> do.call(what = "rbind") |> mutate(Model = as.factor(model), Strain = as.factor(Strain))

    }

    results <- model_results |> do.call(what = "rbind")
    residuals <- model_residuals |> do.call(what = "rbind")
    vimp <- model_vimp |> do.call(what = "rbind")

    return(list(metrics = results, residuals = residuals, vimp = vimp, hyperparams = hyperparams))
    
}





