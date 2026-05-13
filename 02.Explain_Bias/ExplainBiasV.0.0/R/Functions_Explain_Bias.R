# ==============================================================================
# =========================== F_Is_decimal =====================================
# ==============================================================================
#' F_Is_decimal
#'
#' Function to validate if a string represents a decimal number
#'
#' @param
#'        a string
#' @return
#'        TRUE: If the complete string matches the regular expression
#'        FALSE: If the string does not match the regular expression
#' @examples
#'        number <- F_Is_decimal(string)
#' @export
F_Is_decimal <- function(string)
{
  grepl("^[0-9]+\\.[0-9]+$", string)
}
# ==============================================================================
# =========================== F_Is_integer =====================================
# ==============================================================================
#' F_Is_integer
#'
#' Function to validate if a string represents an integer number
#'
#' @param
#'        a string
#' @return
#'        TRUE: If the complete string matches the regular expression
#'        FALSE: If the string does not match the regular expression
#' @examples
#'        number <- F_Is_decimal(string)
#' @export
F_Is_integer <- function(string)
{
  grepl("^[0-9]+$", string)
}
# ==============================================================================
# ======================= F_Get_and_validate_numerical_input ===================
# ==============================================================================
#' F_Get_and_validate_numerical_input
#'
#' Function to obtain and validate the input (integer or decimal) of the user
#'
#' @param
#'        prompt: message displayed to user
#'        type  : a string "d" for decimals or "i" for integers
#'        input_mode :
#' @return
#'        validated values to numeric format
#' @examples
#'        entry <- F_Get_and_validate_numerical_input("Enter a decimal value","d")
#' @export
F_Get_and_validate_numerical_input <- function(prompt, type, input_mode)
{
  valid_value <- FALSE
  input <- ""
  while (!valid_value)
  {
    input <- readline(prompt = prompt)
    values <- unlist(strsplit(input, ","))
    values <- trimws(values)

    if (input_mode == 1)
    {
      if (length(values) == 1)
      {
        validation <- switch(type,
                             "d" = F_Is_decimal(values),
                             "i" = F_Is_integer(values),
                             FALSE)
        if (validation)
        {
          valid_value <- TRUE
        }
        else
        {
          cat("Error: Please enter a single valid", ifelse(type == "d", "decimal", "integer"), "value.\n")
        }
      } else {
        cat("Error: Please enter only one value.\n")
      }
    }
    else if (input_mode == 2)
    {
      validation <- switch(type,
                           "d" = all(sapply(values, F_Is_decimal)),
                           "i" = all(sapply(values, F_Is_integer)),
                           FALSE)
      if (validation) {
        valid_value <- TRUE
      }
      else
      {
        cat("Error: Please enter valid", ifelse(type == "d", "decimals", "integers"), "separated by commas.\n")
      }
    }
    else
    {
      cat("Error: Invalid input mode. Please use 1 for single value or 2 for multiple values.\n")
      valid_value <- TRUE # Para evitar un bucle infinito si el modo es inválido
    }
  }
  if (input_mode == 1) {
    return(as.numeric(values))
  } else if (input_mode == 2) {
    return(as.numeric(values))
  } else {
    return(invisible(NULL))
  }
}
# ==============================================================================
# ======================== F_Get_train_proportion ==============================
# ==============================================================================
#' F_Get_train_proportion
#'
#' This function get the train proportion by the user
#'
#' @param
#'        none
#' @return
#'        training sample proportion
#' @examples
#'        train_sample_prop <- F_Get_train_proportion()
#' @export
F_Get_train_proportion <- function()
{
  while (TRUE) {
    print("=========================================================================")
    print("Validation method: Hold - Out")
    print("=========================================================================")
    train_sample_prop <- readline(prompt = "Enter training sample proportion (0 < x < 1) Example: 0.7:")
    if (grepl("^0\\.[0-9]+$", train_sample_prop)) {
      train_sample_prop <- as.numeric(train_sample_prop)
      print("=========================================================================")
      break
    } else {
      print("=========================================================================")
      cat("Invalid option. Please enter an valid decimal number (0 < x < 1).\n")
    }
  }
  return(train_sample_prop)
}
# ==============================================================================
# ======================== F_Get_hypergrid =====================================
# ==============================================================================
#' F_Get_hypergrid
#'
#' This function extracts the hypergrid values by default
#'
#' @param
#'        mode : an integer value (1 or 2)
#' @return
#'        it depends on mode:
#'        mode = 1 : returns values for test (32 combinations)
#'        mode = 2 : returns values for complete experiment (5184 combinations)
#' @examples
#'        hypergrid <- F_Get_hypergrid(1)
#' @export
F_Get_hypergrid <- function(mode)
{
  if (mode == 1)
  {
    return(
      hyper_grid <- expand.grid(
        #nrounds         = c(50, 100, 150),   # Number of boosting rounds
        eta              = c(.01, .05),       # Learning rate
        #gamma           = c(0, 1, 5),        # Minimum loss reduction
        max_depth        = c(1, 3),           # Maximum depth of a tree
        min_child_weight = c(1),              # Minimum sum of instance weight
        subsample        = c(.65, .8),        # Fraction of rows per tree
        colsample_bytree = c(.8),             # Fraction of columns per tree
        lambda           = c(0, 0.5),         # L2 regularization term (Ridge)
        alpha            = c(0, 0.5),         # L1 regularization term (LASSO)
        optimal_trees    = 0,                 # a place to dump results
        min_RMSE         = 0                  # a place to dump results
      ))
  }
  if (mode == 2)
  {
    return(
      hyper_grid <- expand.grid(
        #nrounds         = c(50, 100, 150),      # Number of boosting rounds
        eta              = c(.01, .05, .1, .3),  # Learning rate
        #gamma           = c(0, 1, 5),           # Minimum loss reduction
        max_depth        = c(1, 3, 5, 7),        # Maximum depth of a tree
        min_child_weight = c(1, 3, 5, 7),        # Minimum sum of instance weight
        subsample        = c(.65, .8, 1),        # Fraction of rows per tree
        colsample_bytree = c(.8, .9, 1),         # Fraction of columns per tree
        lambda           = c(0, 0.5, 1),         # L2 regularization term (Ridge)
        alpha            = c(0, 0.5, 1),         # L1 regularization term (LASSO)
        optimal_trees    = 0,                    # a place to dump results
        min_RMSE         = 0                     # a place to dump results
      )
    )
  }
}
# ==============================================================================
# ======================== F_Create_hypergrid =====================================
# ==============================================================================
#' F_Create_hypergrid
#'
#' This function create the hypergrid values by user
#'
#' @param
#'        none
#' @return
#'        hypergrid
#' @examples
#'        hypergrid <- F_Create_hypergrid()
#' @export
F_Create_hypergrid <- function()
{
  # Request values from the user
  print("Enter values separated by commas (e.g. 0.01, 0.05, 0.1):")
  # Get and validate inputs
  values_eta              <- F_Get_and_validate_numerical_input("===============eta(d): ", "d", 2)
  values_max_depth        <- F_Get_and_validate_numerical_input("=========max depth(i): ", "i", 2)
  values_min_child_weight <- F_Get_and_validate_numerical_input("==min child weight(i): ", "i", 2)
  values_subsample        <- F_Get_and_validate_numerical_input("=========subsample(d): ", "d", 2)
  values_colsample_bytree <- F_Get_and_validate_numerical_input("==colsample bytree(d): ", "d", 2)
  values_lambda           <- F_Get_and_validate_numerical_input("============lambda(d): ", "d", 2)
  values_alpha            <- F_Get_and_validate_numerical_input("=============alpha(d): ", "d", 2)

  # Assign values
  return(
    hyper_grid <- expand.grid(
      #nrounds         = c(50, 100, 150),            # Number of boosting rounds
      eta              = values_eta ,                # Learning rate
      #gamma           = c(0, 1, 5),                 # Minimum loss reduction
      max_depth        = values_max_depth,           # Maximum depth of a tree
      min_child_weight = values_min_child_weight,    # Minimum sum of instance weight
      subsample        = values_subsample,           # Fraction of rows per tree
      colsample_bytree = values_colsample_bytree,    # Fraction of columns per tree
      lambda           = values_lambda,              # L2 regularization term (Ridge)
      alpha            = values_alpha,               # L1 regularization term (LASSO)
      optimal_trees    = 0,                          # a place to dump results
      min_RMSE         = 0                           # a place to dump results
    ))
}
# ==============================================================================
# ======================== F_Get_numerical_values_user =========================
# ==============================================================================
#' F_Get_numerical_values_user
#'
#' Function to request a value from the user and validate that it is numeric
#'
#' @param
#'        prompt : Text requesting user input
#' @return
#'        a numeric value
#' @examples
#'        alpha_value <- F_Get_numerical_values_user(prompt)
#' @export
F_Get_numerical_values_user <- function(prompt)
{
  value <- readline(prompt = paste0(prompt, ": "))
  # while loop to ensure that the input is numeric
  while (is.na(as.numeric(value))) {
    cat("Invalid entry. Please enter a numeric value.\n")
    value <- readline(prompt = paste0(prompt, ": "))
  }
  return(as.numeric(value))
}
# ==============================================================================
# ======================== F_Create_optimal_params =============================
# ==============================================================================
#' F_Create_optimal_params
#'
#' This function creates the optimal parameters with data
#' entered manually by the user.
#'
#' @param
#'        none
#' @return
#'        the optimal parameters
#' @examples
#'        optimal_params <- F_Create_optimal_params()
#' @export
F_Create_optimal_params <- function()
{
  # Request values from the user
  print("Enter values:")
  values_eta              <- F_Get_and_validate_numerical_input("===============eta(d): ", "d", 1)
  values_max_depth        <- F_Get_and_validate_numerical_input("=========max depth(i): ", "i", 1)
  values_min_child_weight <- F_Get_and_validate_numerical_input("==min child weight(i): ", "i", 1)
  values_subsample        <- F_Get_and_validate_numerical_input("=========subsample(d): ", "d", 1)
  values_colsample_bytree <- F_Get_and_validate_numerical_input("==colsample bytree(d): ", "d", 1)
  values_lambda           <- F_Get_and_validate_numerical_input("============lambda(d): ", "d", 1)
  values_alpha            <- F_Get_and_validate_numerical_input("=============alpha(d): ", "d", 1)

  # Assign values
  return(
    optimal_params <- data.frame(
      eta              = values_eta,                 # Learning rate
      max_depth        = values_max_depth,           # Maximum depth of a tree
      min_child_weight = values_min_child_weight,    # Minimum sum of instance weight
      subsample        = values_subsample,           # Fraction of rows per tree
      colsample_bytree = values_colsample_bytree,    # Fraction of columns per tree
      lambda           = values_lambda,              # L2 regularization term (Ridge)
      alpha            = values_alpha                # L1 regularization term (LASSO)
    ))
}
# ==============================================================================
# ======================== F_Print_hypergrid_values =============================
# ==============================================================================
#' F_Print_hypergrid_values
#'
#' This function prints the parameters of the hypergrid.
#'
#' @param
#'        hypergrid
#' @return
#'        none
#' @examples
#'        parameters <- F_Print_hypergrid_values(hyper_grid)
#' @export
F_Print_hypergrid_values <- function(hyper_grid)
{
  parameter_names <- names(hyper_grid)
  parameter_values_string <- character(length(parameter_names))

  for (i in seq_along(parameter_names)) {
    name <- parameter_names[i]
    unique_values <- unique(hyper_grid[[name]])
    parameter_values_string[i] <- paste(unique_values, collapse = ", ")
  }

  parameter_df <- data.frame(Parameter = parameter_names,
                             Values = parameter_values_string)
  print(parameter_df)
}
# ==============================================================================
# ======================== F_Extract_centroids =================================
# ==============================================================================
#' F_Extract_centroids
#'
#' This function extracts the centroids of a file with geometry of
#' municipalities or geographic areas.
#'
#' @param
#'        lda_sdf:A dataframe with geometry data, it must contain a column with the area code.
#' @return
#'        A dataframe with latitude and longitude variables representing the centroids.
#' @examples
#'        data_centroids <- F_Extract_centroids(dataframe)
#' @export
F_Extract_centroids<-function(dataframe)
{
  # extract centroids
  centroids <- suppressWarnings({
    st_centroid(dataframe) %>%
      st_coordinates() %>%
      as.data.frame() %>%
      rowid_to_column("row_id")
  })

  # transfer area ids
  centroids_with_ids <- dataframe %>%
    st_set_geometry(NULL) %>%       # Remove geometry to avoid issues with join
    rowid_to_column("row_id") %>%   # Create a temporary row_id for merging
    cbind(centroids)

  # remove row_id and rename x,y
  centroids_with_ids <- centroids_with_ids %>%
    select(-c(row_id)) %>%
    rename(
      longitude = "X",
      latitude = "Y"
    )
  return(centroids_with_ids)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Standardize =======================================
# ==============================================================================
#' F_Standardize
#'
#' This function standardizes the numeric attributes of a dataframe.
#'
#' @param
#'        dataframe:A dataframe with mixed attributes (numerical and categorical)
#' @return
#'        A dataframe with standardized numeric attributes
#' @examples
#'        standardized_data <- F_Standardize(dataframe)
#' @export
F_Standardize <- function(dataframe)
{
  df_standardised <- dataframe %>%
    dplyr::select(-c(Area_code)) %>%
    mutate(across(where(is.numeric), ~ (. - mean(.)) / sd(.))) %>%
    cbind(dataframe %>% dplyr::select(Area_code))

  return(df_standardised)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Missing_values ====================================
# ==============================================================================
#' F_Missing_values
#'
#' This function implements the listwise deletion approach as a
#' treatment for missing values and returns a dataframe without missing values.
#'
#' @param
#'        dataframe:A dataframe with or without missing values
#' @return
#'        A dataframe without missing values
#' @examples
#'        complete_data <- F_Missing_values(dataframe)
#' @export
F_Missing_values <- function(dataframe)
{
  dataframe <- dataframe %>% drop_na()
  return(dataframe)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Treatment_plan ====================================
# ==============================================================================
#' F_Treatment_plan
#'
#' This function implements a treatment plan for the data. It performs
#' transformations to both numerical and categorical variables to optimize
#' their use in the model.
#'
#' @param
#'        features     :list of attributes or independent variables
#'        train_sample :dataframe training set
#'        test_sample  :dataframe test set
#'        partition    :Processed partition to be returned (1 = training set, 2 = test set)
#' @return
#'        A Dataframe with optimized attributes or variables
#' @examples
#'        optimized_data <- F_Treatment_plan(independent_variables,training_set,test_set,1)
#' @export
F_Treatment_plan <- function(features,train_sample,test_sample,partition)
{
  # Create the treatment plan from the training data
  treatplan <- vtreat::designTreatmentsZ(train_sample,
                                         features,
                                         verbose = FALSE)

  # Get the "clean" variable names from the scoreFrame
  new_vars <- treatplan %>%
    magrittr::use_series(scoreFrame) %>%
    dplyr::filter(code %in% c("clean", "lev")) %>%
    magrittr::use_series(varName)

  if(partition == 1)
  {
    features_sample <- vtreat::prepare(treatplan,train_sample,
                                       varRestriction = new_vars) %>% as.matrix()
  }
  if(partition == 2)
  {
    features_sample <- vtreat::prepare(treatplan,test_sample,
                                       varRestriction = new_vars) %>% as.matrix()
  }
  return(features_sample)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Training ==========================================
# ==============================================================================
#' F_Training
#'
#' This function trains a Gradient Boosting model using cross-validation,
#' finds the optimal number of trees, and visualizes the performance of the model
#' as the number of trees increases. This allows the model to be evaluated and optimized.
#'
#' @param
#'        features_train :independent variables
#'        response_train :dependent variable
#' @return
#'        Plot of model performance as the number of trees increases
#' @examples
#'        F_Training(features_train,response_train)
#' @export
F_Training <- function(X_train,y_train)
{
  # reproducibility
  set.seed(123)

  xgb.fit1 <- xgb.cv(
    data = X_train,
    label = y_train,
    nrounds = 1000,
    nfold = 10,
    objective = "reg:squarederror",  # for regression models
    verbose = 0,               # silent,
    #  early_stopping_rounds = 10
  )

  # get number of trees that minimize error
  xgb.fit1$evaluation_log %>%
    dplyr::summarise(
      ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
      rmse.train   = min(train_rmse_mean),
      ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
      rmse.test    = min(test_rmse_mean),
    )

  # plot error vs number trees
  ggplot(xgb.fit1$evaluation_log) +
    geom_line(aes(iter, train_rmse_mean), color = "red") +
    geom_line(aes(iter, test_rmse_mean), color = "blue")
}
# ==============================================================================

# ==============================================================================
# ======================== F_Parallelised_code =================================
# ==============================================================================
#' F_Parallelised_code
#'
#' This function performs a parallel grid search to find the best combination
#' of hyperparameters for an xgboost regression model, using cross-validation
#' to evaluate performance. The code measures run time and returns a dataframe
#' with the results for each hyperparameter combination.
#'
#' @param
#'        hyper_grid     :data with the different combinations of parameters of the XGBoost model
#'        features_train :independent variables
#'        response_train :dependent variable
#' @return
#'        a dataframe with the results for each hyperparameter
#' @examples
#'        results <- F_Parallelised_code(hyper_grid,features_train,response_train)
#' @export
F_Parallelised_code <- function(hyper_grid,X_train,y_train)
{
  # Measure the time taken for parallel grid search
  time_taken <- system.time({

    # Set up parallel backend with the number of cores (adjust according to your system)
    num_cores <- detectCores() - 1  # to leave one core free
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)

    # Parallelized grid search
    results <- foreach(i = 1:nrow(hyper_grid), .combine = rbind, .packages = "xgboost") %dopar% {
      # create parameter list
      params <- list(
        eta                 = hyper_grid$eta[i],
        #gamma              = hyper_grid$gamma[i],
        max_depth           = hyper_grid$max_depth[i],
        min_child_weight    = hyper_grid$min_child_weight[i],
        subsample           = hyper_grid$subsample[i],
        colsample_bytree    = hyper_grid$colsample_bytree[i],
        lambda              = hyper_grid$lambda[i],
        alpha               = hyper_grid$alpha[i]
      )

      # reproducibility
      set.seed(123)

      # train model
      xgb.tune <- xgb.cv(
        params    = params,
        data      = X_train,
        label     = y_train,
        nrounds   = 5000,
        nfold     = 10,
        objective = "reg:squarederror",  # for regression models
        verbose   = 0,                    # silent,
        early_stopping_rounds = 10       # stop if no improvement for 10 consecutive trees
      )

      # collect the optimal number of trees and minimum RMSE
      optimal_trees <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
      min_RMSE <- min(xgb.tune$evaluation_log$test_rmse_mean)

      # return as a row (with hyperparameters and results)
      return(data.frame(
        eta              = hyper_grid$eta[i],
        #gamma           = hyper_grid$gamma[i],
        max_depth        = hyper_grid$max_depth[i],
        min_child_weight = hyper_grid$min_child_weight[i],
        subsample        = hyper_grid$subsample[i],
        colsample_bytree = hyper_grid$colsample_bytree[i],
        lambda           = hyper_grid$lambda[i],
        alpha            = hyper_grid$alpha[i],
        optimal_trees    = optimal_trees,
        min_RMSE         = min_RMSE
      ))
    }

    # Convert to a data frame if not already (should be)
    results <- as.data.frame(results)

    # Stop the cluster after processing
    stopCluster(cl)
    registerDoSEQ()  # Reset back to sequential processing

  })
  # Print the time taken
  print(time_taken)

  # Display the results
  return(results)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Fit_final_model ===================================
# ==============================================================================
#' F_Fit_final_model
#'
#' This function takes the optimal hyperparameters previously found,
#' trains a final xgboost model using those hyperparameters and returns
#' the trained model. The trained model is then ready to make predictions.
#'
#' @param
#'        optimal_pars: optimal parameters
#' @return
#'        a trained model which is ready to make predictions
#' @examples
#'        trained_model <- F_Fit_final_model(optimal_pars)
#' @export
F_Fit_final_model <- function(optimal_pars,X_train,y_train)
{
  # parameter list
  params <- list(
    eta              = optimal_pars$eta,
    max_depth        = optimal_pars$max_depth,
    min_child_weight = optimal_pars$min_child_weight,
    subsample        = optimal_pars$subsample,
    colsample_bytree = optimal_pars$colsample_bytree,
    lambda           = optimal_pars$lambda,
    alpha            = optimal_pars$alpha
  )

  # train final model
  xgb.fit.final <- xgboost(
    params    = params,
    data      = X_train,
    label     = y_train,
    nrounds   = 2000,
    objective = "reg:squarederror",
    verbose   = 0,
    early_stopping_rounds = 15)
  return(xgb.fit.final)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Predictions ===================================
# ==============================================================================
#' F_Predictions
#'
#' This function uses the model, the data set and makes predictions.
#'
#' @param
#'        xgb.fit.final   : trained model
#'        sample          : training set or test set
#'        bias_or_metrics : 1 returns sample with predictions / 2 returns prediction's metrics
#' @return
#'        depends on the parameter bias_or_metrics:
#'        bias_or_metrics = 1 : returns the data set with an extra column named "bias_prediction" with the predictions
#'        bias_or_metrics = 2 : returns a vector with the columns: rmse,pearson correlation,spearmam correlation,standard deviation
#' @examples
#'        predictions <- F_Predictions(model,test_data,1)
#' @export
F_Predictions <- function(xgb.fit.final,sample,bias_or_metrics)
{
  # set names
  aux_train_sample <- setNames(sample[, 3:(ncol(sample) - 1)], xgb.fit.final$feature_names)
  # Drop NA names
  aux_train_sample <- aux_train_sample[, !is.na(names(aux_train_sample))]
  # Prediction in training data
  sample$bias_prediction <- predict(xgb.fit.final, as.matrix(aux_train_sample))

  real_values      <- sample$bias
  predicted_values <- sample$bias_prediction

  ## RMSE
  RMSE0 <- sqrt(sum((real_values - predicted_values)^2 ) / ncol(sample))
  RMSE  <- sqrt(sum((real_values - predicted_values)^2 ) / nrow(sample))

  # Library Metrics
  RMSE_metrics  <- Metrics::rmse(real_values, predicted_values)
  MSE_metrics   <- Metrics::mse(real_values, predicted_values)
  MAE_metrics   <- Metrics::mae(real_values, predicted_values)
  MAPE_metrics  <- Metrics::mape(real_values, predicted_values)
  RMSLE_metrics <- Metrics::rmsle(real_values, predicted_values)
  #R2_metrics    <- Metrics::rsq(real_values, predicted_values)

  # Library caret
  Metrics_caret <- postResample(pred = predicted_values, obs = real_values)
  RMSE_caret <- Metrics_caret["RMSE"]
  MAE_caret  <- Metrics_caret["MAE"]
  R2_caret   <- Metrics_caret["Rsquared"]

  # Manual R2
  ss_res <- sum((real_values - predicted_values)^2, na.rm = TRUE)
  ss_tot <- sum((real_values - mean(real_values, na.rm = TRUE))^2, na.rm = TRUE)
  R2 <- (1 - (ss_res/ss_tot))

  # Library yardstick
  # Tibble
  data_for_rsq <- tibble(
    truth    = real_values,
    estimate = predicted_values
  )
  # R2
  r_squared_yardstick <- data_for_rsq %>%
    rsq(truth = truth, estimate = estimate)
  R2_yardstick <- r_squared_yardstick$.estimate
  # RMSE
  rmse_yardstick <- data_for_rsq %>%
    rmse(truth = truth, estimate = estimate)
  RMSE_yardstick <- rmse_yardstick$.estimate


  ## Pearson correlation and p-value
  #sample_pearson_cor <- cor(sample$bias, sample$bias_prediction, method = "pearson")
  pearson_test <- cor.test(real_values, predicted_values, method = "pearson")
  sample_pearson_cor <- pearson_test$estimate
  sample_pearson_p_value <- pearson_test$p.value

  #sample_pearson_p_value <- formatC(sample_pearson_p_value, format = "f", digits = 25)

  ## Spearman correlation and p-value
  #sample_spearman_cor <- cor(sample$bias, sample$bias_prediction, method = "spearman")
  spearman_test <- cor.test(real_values, predicted_values, method = "spearman", exact = FALSE)
  sample_spearman_cor <- spearman_test$estimate
  sample_spearman_p_value <- spearman_test$p.value

  #sample_spearman_p_value <- formatC(sample_spearman_p_value, format = "f", digits = 25)

  ## standard deviation
  sample_sd_error <- sd(real_values - predicted_values)

  # Return
  if(bias_or_metrics == 1)
    return(sample)
  if(bias_or_metrics == 2)
  {
    sample_metrics <- c(
      RMSE0,
      RMSE,
      RMSE_metrics,
      RMSE_caret,
      MSE_metrics,
      MAE_metrics,
      MAE_caret,
      MAPE_metrics,
      RMSLE_metrics,
      R2,
      #R2_metrics,
      R2_caret,
      R2_yardstick,
      RMSE_yardstick,
      sample_pearson_cor,
      sample_pearson_p_value,
      sample_spearman_cor,
      sample_spearman_p_value,
      sample_sd_error) %>%
      round(5)
    return(sample_metrics)
  }
}
# ==============================================================================

# ==============================================================================
# ======================== F_Metrics_table =====================================
# ==============================================================================
#' F_Metrics_table
#'
#' This function creates the results table of performance metrics and correlations
#'
#' @param
#'        train_sample_metrics   : metrics obtained from the predictions of the F_Predictions function
#'        test_sample_metrics    : same as train_sample_metrics
#' @return
#'        a summary table with metrics and correlations
#' @examples
#'        summary_table <- F_Metrics_table(train_sample_metrics,test_sample_metrics)
#' @export
F_Metrics_table <- function(train_metrics,test_metrics)
{
  metric <- c("RMSE_0",
              "RMSE",
              "RMSE_Metrics",
              "RMSE_caret",
              "MSE_Metrics",
              "MAE_Metrics",
              "MAE_caret",
              "MAPE_Metrics",
              "RMSLE_Metrics",
              "R2",
              #"R2_Metrics",
              "R2_caret",
              "R2_yardstick",
              "RMSE_yardstick",
              "Pearson_Cor",
              "Pearson_P_Value",
              "Spearman_Cor",
              "Spearman_P_Value",
              "SD_Error")
  summary_table <- data.frame(metric, train_metrics, test_metrics)
  names(summary_table) <- c("Metric", "Training", "Test")
  return(summary_table)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Bar_plot_feature_importance =======================
# ==============================================================================
#' F_Bar_plot_feature_importance
#'
#' This function generates a bar chart showing the importance of the 30
#' most important features of a machine learning model,
#' according to the calculated SHAP values.
#'
#' @param
#'        shp = a shapviz object encapsulating the SHAP values
#' @return
#'        a PNG bar plot of feature importance
#' @examples
#'        plot <- F_Bar_plot_feature_importance(shp)
#' @export
F_Bar_plot_feature_importance <- function(shp)
{
  # Preparing the feature labels
  feature_lbl <- sv_importance(shp, kind = "bee",
                               max_display = 30L) %>% .$data %>%
    distinct(.$feature) %>%
    rename(labels = ".$feature")

  # Generating the feature importance bar plot
  return(
    sv_importance(shp, kind = "bar",
                  viridis_args = list(option = "viridis"),
                  max_display = 30L) +
      scale_y_discrete( labels = rev(feature_lbl$labels)) +
      labs(
        x = "Mean SHAP value",
        y = "Features"
      ) +
      theme_plot_tufte() +
      theme(axis.title = element_text(size = 36),
            axis.text.y = element_text(size = 40),
            axis.text.x = element_text(size = 30))
  )
}
# ==============================================================================

# ==============================================================================
# ======================== F_Feature_importance_value_rankings =================
# ==============================================================================
#' F_Feature_importance_value_rankings
#'
#' This function generates a feature importance bar chart,
#' extracts the underlying data, calculates the ranking of the features
#' based on their importance values.
#'
#' @param
#'        shp = a shapviz object encapsulating the SHAP values
#' @return
#'        a dataframe containing the feature names, their importance values and their ranking
#' @examples
#'        ranks <- F_Feature_importance_value_rankings(shp)
#' @export
F_Feature_importance_value_rankings <- function(shp)
{
  # Generate the graph and capture the ggplot object
  importance_plot <- sv_importance(shp, kind = "bar",
                                   viridis_args = list(option = "viridis"),
                                   max_display = 30L)

  # Extract the underlying data
  importance_data <- importance_plot$data

  # Extract SHAP values and feature labels
  importance_values <- importance_data$value
  feature_labels <- importance_data$feature

  # Calculate the ranking of the features
  feature_rank <- rank(-abs(importance_values), ties.method = "min")

  # Create a data frame with the values, labels, and ranking
  shap_df <- data.frame(feature = feature_labels,
                        importance_value = importance_values,
                        rank = feature_rank)

  # Return the resulting data frame
  return(shap_df)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Shap_plot_bias ====================================
# ==============================================================================
#' F_Shap_plot_bias
#'
#' This function generates a swarm plot showing the distribution of SHAP values
#' for the 20 most important features of a model.
#'
#' @param
#'        shp = a shapviz object encapsulating the SHAP values
#' @return
#'        a PNG swarm plot of SHAP values
#' @examples
#'        plot <- F_Shap_plot_bias(shp)
#' @export
F_Shap_plot_bias <- function(shp)
{
  feature_lbl <- sv_importance(shp, kind = "bee",
                               max_display = 20L) %>% .$data %>%
    distinct(.$feature) %>%
    rename(labels = ".$feature")

  return(
    sv_importance(shp,
                  kind = "bee",
                  max_display = 20L,
                  viridis_args = list(option = "viridis") ) +
      scale_y_discrete( labels = rev(feature_lbl$labels)) +
      theme_plot_tufte() +
      theme(
        axis.title = element_text(size = 36),
        axis.text.y = element_text(size = 40),
        axis.text.x = element_text(size = 40),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 35))
  )
}
# ==============================================================================

# ==============================================================================
# ======================== F_Partial_dependence_plots ==========================
# ==============================================================================
#' F_Partial_dependence_plots
#'
#' This function generates a grid of scatter plots showing the relationship
#' between the values of the 8 most important features and their SHAP values,
#' providing information on how each feature influences the model predictions.
#'
#' @param
#'        shp             = a shapviz object encapsulating the SHAP values
#'        xgb.fit.final   = the trained model
#'        features_train  = the independent variables of the training set
#'        shap_values     = The shap values
#' @return
#'        a PNG grid of scatter plots
#' @examples
#'        plot <- F_Partial_dependence_plots(shp,xgb.fit.final,features_train,shap_values)
#' @export
F_Partial_dependence_plots <- function(shp,xgb.fit.final,features_train,shap_values)
{
  feature_lbl <- sv_importance(shp, kind = "bee",
                               max_display = 20L) %>% .$data %>%
    distinct(.$feature) %>%
    rename(labels = ".$feature")

  # Prepare the long-format data:
  shap_long <- shap.prep(xgb_model = xgb.fit.final, X_train = features_train)
  # # is the same as: using given shap_contrib
  shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = features_train)

  Head <- head(feature_lbl, n = 8)

  # select variables
  selected_data <- inner_join(shap_long, head(feature_lbl, n = 8),
                              join_by(variable == labels) )

  # select variables for plot
  selected_variables <- unique(selected_data$variable)

  # create a list to store individual plots
  plot_list <- list()

  # loop over each selected variable and create an individual plot
  for (var in selected_variables) {
    # Filter data for the current variable
    data_subset <- selected_data %>% filter(variable == var)

    # create a scatter plot for the current variable
    p <- ggplot(data_subset, aes(x = rfvalue, y = value, color = rfvalue)) +
      geom_smooth(color = "gray95", span = 0.3, method = "gam", formula = y ~ s(x, bs = "cs")) +
      geom_point(alpha = 0.7, size = 3) +
      scale_colour_viridis_c(option = "D",
                             labels = label_number(scale_cut = cut_short_scale()),
                             guide = guide_colorbar(title.position="top")) +
      scale_y_continuous(
        labels = label_number(scale_cut = cut_short_scale(), accuracy = 0.001)
      ) +
      labs(title = var, x = "Feature value", y = "Shape value") +
      theme_plot_tufte() +
      theme(
        title = element_text(size = 35),
        legend.position = "none",
        text = element_text(size = 15),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30)
      )

    # add the plot to the list
    plot_list[[var]] <- p
  }

  # combine all plots into a single layout
  combined_plot <- wrap_plots(plot_list, ncol = 2)

  # return combined plot
  return(combined_plot)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Df_with_shap_values ===============================
# ==============================================================================
#' F_Df_with_shap_values
#'
#' This function trains an xgboost model with optimized hyperparameters,
#' calculates SHAP values and then combines these values with the original data
#' in a long format that is useful for further analysis and visualization.
#'
#' @param
#'        optimal_pars = the optimal parameter for the model
#'        dataframe     = the standardize dataframe with the dependent variable (bias)
#' @return
#'        a dataframe of the original data in a long format
#' @examples
#'        long_dataframe <- F_Df_with_shap_values(optimal_pars,dataframe)
#' @export
F_Df_with_shap_values <- function(optimal_pars,dataframe)
{
  # parameter list
  params <- list(
    eta = optimal_pars$eta,
    max_depth = optimal_pars$max_depth,
    min_child_weight = optimal_pars$min_child_weight,
    subsample = optimal_pars$subsample,
    colsample_bytree = optimal_pars$colsample_bytree,
    lambda = optimal_pars$lambda,
    alpha = optimal_pars$alpha
  )

  # MODEL
  xgb.fit.final_all <- xgboost(
    params = params,
    data = as.matrix(dataframe[, 4:ncol(dataframe)-1]),
    label = dataframe$bias,
    nrounds = 2000,
    objective = "reg:squarederror",
    verbose = 0,
    early_stopping_rounds = 15)

  # VALUES
  # Return the SHAP values and ranked features by mean|SHAP|
  shap_values_full <- shap.values(xgb_model = xgb.fit.final_all,
                                  X_train = as.matrix(dataframe[, 4:ncol(dataframe)-1]))

  aux_full <- as.data.frame(shap_values_full$shap_score)

  input_df_with_shap_values <- cbind(dataframe[,1:2], aux_full) %>%
    pivot_longer(
      cols = c(3:length(.)),
      names_to = "feature",
      values_to = "shap_value"
    )
  return(input_df_with_shap_values)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Get_selected_areas ================================
# ==============================================================================
#' F_Get_selected_areas
#'
#' This function identifies the 10 areas or municipalities with the highest and
#' lowest population, combines them, sorts them by descending population
#' and returns a vector containing the names of those municipalities.
#'
#' @param
#'        dataframe = the standardize dataframe with the dependent variable (bias)
#' @return
#'        a vector containing the names of the 20 areas or municipalities
#' @examples
#'        areas <- F_Get_selected_areas(dataframe)
#' @export
F_Get_selected_areas <- function(dataframe)
{
  # Get the 10 rows with the highest Total_Population values
  top_10 <- dataframe %>%
    top_n(10, Total_Population) %>%
    select(Area_name,Total_Population)

  # Get the 10 rows with the lowest Total_Population values
  bottom_10 <- dataframe %>%
    top_n(-10, Total_Population) %>%
    select(Area_name,Total_Population)

  # Combine the results into a single dataframe
  selected_areas <- bind_rows(top_10, bottom_10)
  selected_areas <- selected_areas %>% arrange(desc(Total_Population))
  selected_areas <- selected_areas$Area_name
  return(selected_areas)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Heatmap_sorted_alphabetically =====================
# ==============================================================================
#' F_Heatmap_sorted_alphabetically
#'
#' This function generates a heat map showing the SHAP values for the selected
#' municipalities and the corresponding characteristics sorted alphabetically.
#'
#' @param
#'        input_df_with_shap_values = a dataframe of the original data in a long format
#'        (returned by the F_Df_with_shap_values function)
#'        selected_areas = a vector of selected municipalities or areas
#' @return
#'        a PNG heat map showing the SHAP values sorted alphabetically for the selected municipalities
#' @examples
#'        plot <- F_Heatmap_sorted_alphabetically(input_df_with_shap_values,selected_areas)
#' @export
F_Heatmap_sorted_alphabetically <- function(input_df_with_shap_values,selected_areas)
{
  return(
    input_df_with_shap_values %>%
      filter(Area_name %in% selected_areas) %>%
      mutate(Area_name = factor(Area_name, levels = selected_areas)) %>% # Convertir a factor
      tidyplot(x = Area_name,
               y = feature,
               color = shap_value) %>%
      add_heatmap(rotate_labels = 90) %>%
      adjust_size(height = 100) %>%
      adjust_x_axis_title("Areas") %>%
      adjust_y_axis_title("Features") %>%
      add(ggplot2::theme(
        legend.position = "bottom",
        legend.title = element_text(size=25, face = "plain", hjust=0.5, lineheight=0.45,
                                    color="black"),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.70, "cm"),
        legend.text = element_text(size = 25),
        axis.title.x = element_text(size = 48),
        axis.title.y = element_text(size = 48),
        axis.text.x = element_text(size = 28),  # Aumenta el tamaño del texto del eje X (Municipios)
        axis.text.y = element_text(size = 28)   # Aumenta el tamaño del texto del eje Y (Features)
      )) %>%
      add(ggplot2::labs(fill = "Shap value"))
  )
}
# ==============================================================================

# ==============================================================================
# ======================== F_Heatmap_sorted_by_feature_importance ==============
# ==============================================================================
#' F_Heatmap_sorted_by_feature_importance
#'
#' This function generates a heat map showing the SHAP values for the selected
#' municipalities and the corresponding characteristics sorted by feature
#' importance.
#'
#' @param
#'        input_df_with_shap_values = a dataframe of the original data in a long format
#'        (returned by the F_Df_with_shap_values function)
#'        selected_areas = a vector of selected municipalities or areas
#' @return
#'        a PNG heat map showing the SHAP values sorted by feature importnace for the selected municipalities
#' @examples
#'        plot <- F_Heatmap_sorted_by_feature_importance(input_df_with_shap_values,selected_areas)
#' @export
F_Heatmap_sorted_by_feature_importance <- function(shp,input_df_with_shap_values,selected_areas)
{
  # Generar el gráfico y capturar el objeto ggplot
  importance_plot <- sv_importance(shp, kind = "bar",
                                   viridis_args = list(option = "viridis"),
                                   max_display = 30L)
  importance_data <- importance_plot$data
  feature_order <- importance_data$feature

  return(
    input_df_with_shap_values %>%
      filter(Area_name %in% selected_areas) %>%
      mutate(Area_name = factor(Area_name, levels = selected_areas),
             feature = factor(feature, levels = rev(feature_order))) %>% # 2. Convertir feature a factor con el orden calculado
      tidyplot(x = Area_name,
               y = feature,
               color = shap_value) %>%
      add_heatmap(rotate_labels = 90) %>%
      adjust_size(height = 100) %>%
      adjust_x_axis_title("Areas") %>%
      adjust_y_axis_title("Features") %>%
      add(ggplot2::theme(
        legend.position = "bottom",
        legend.title = element_text(size=25, face = "plain", hjust=0.5, lineheight=0.45,
                                    color="black"),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.70, "cm"),
        legend.text = element_text(size = 25),
        axis.title.x = element_text(size = 48),
        axis.title.y = element_text(size = 48),
        axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28)
      )) %>%
      add(ggplot2::labs(fill = "Shap value"))
  )
}
# ==============================================================================

# ==============================================================================
# ======================== theme_map_tufte =====================================
# ==============================================================================
#' theme_map_tufte
#'
#' This function applies a custom theme to the maps.
#'
#' @param
#'        none
#' @return
#'        A ggplot2 theme object.
#' @examples
#'        theme_map_tufte()
#' @export
theme_map_tufte <- function(...)
{
  theme_tufte() +
    theme(
      text = element_text(family = "robotocondensed"),
      # remove all axes
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
}
# ==============================================================================

# ==============================================================================
# ======================== theme_plot_tufte =====================================
# ==============================================================================
#' theme_plot_tufte
#'
#' This function applies a custom theme to the plots.
#'
#' @param
#'        none
#' @return
#'        A ggplot2 theme object.
#' @examples
#'        theme_plot_tufte()
#' @export
theme_plot_tufte <- function(...)
{
  theme_tufte() +
    theme(
      text = element_text(family = "robotocondensed")
    ) +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.title=element_text(size=14)
    )
}
# ==============================================================================

# ==============================================================================
# ======================== F_Load_dependences ==================================
# ==============================================================================
#' F_Load_dependences
#'
#' This function load libraries
#'
#' @param
#'        none
#' @return
#'        none.
#' @examples
#'        F_Load_dependences()
#' @export
F_Load_dependences<-function()
{
  # Clear workspace
  rm(list=ls())

  # Load libraries
  library(xgboost)
  library(parallel)
  library(recommenderlab)
  library(doParallel)
  library(rsample)
  library(tidyverse)
  library(sf)
  library(moments)
  library(SHAPforxgboost)
  library(shapviz)
  library(pdp)
  library(rsample)
  library(foreach)
  library(patchwork)
  library(viridis)
  library(scales)
  library(tidyplots)
  library(Ckmeans.1d.dp)
  library(vtreat)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(ggthemes)
  library(Metrics)
  library(caret)
  library(yardstick)
  library(showtext)
  library(sysfonts)
}
# ==============================================================================
