# ==============================================================================
# ================================= exe() ======================================
# ==============================================================================
#' exe()
#'
#' This function performs complete data analysis. This function coordinates
#' the loading, cleaning, analysis and visualization of the data.
#'
#' @param
#'        none
#' @return
#'        A folder with analysis results
#' @examples
#'        exe()
#' @export
exe <- function()
{
# E X P L A I N      B I A S
rm(list=ls())
F_Load_dependences()
sysfonts::font_add_google("Roboto Condensed", "robotocondensed")
showtext::showtext_auto()
while (TRUE)
{
  # ============================================================================== 0.MENU
  print("=========================================================================")
  print("================ M A I N    E X P L A I N   B I A S =====================")
  print("=========================================================================")
  print("0.Exit")
  print("1.Test (to verify correct operation)")
  print("2.Complete without parameters (Search for optimal parameters)")
  print("3.Complete with parameters (Optimal parameters are provided by the user)")
  print("=========================================================================")
  while (TRUE){
    entry <- readline(prompt = "Enter the option: ")
    if (grepl("^[0-3]+$", entry)) {
      entry <- as.integer(entry)
      break
    } else {
      cat("Invalid option. Please enter an option[0 - 3]..\n")
    }
  }
  if(entry == 0)
  {
    break
  }
  print("================================================================ 2.PARAMETERS")
  if(entry == 1)
  {
      print("=========================================================================")
      print("============= 1.Test (to verify correct operation) ======================")
      print("=========================================================================")
      Outputs <- readline(prompt = "Enter a name for the results folder:")
      train_proportion <- 0.7
      hyper_grid <- F_Get_hypergrid(1)
      print("=========================================================================")
      print("*** Please verify parameters ***")
      print("=========================================================================")
      print(paste("Outputs:",Outputs))
      print(paste("training sample proportion (by default):", train_proportion))
      print(paste("test     sample proportion             :", (1-train_proportion)))
      print(paste("hypergrid combinations (by default)    :",nrow(hyper_grid)))
      F_Print_hypergrid_values(hyper_grid)
      print("=========================================================================")
      continue <- readline(prompt = "Press ENTER to continue or 0 to return to the main menu: ")
      if (continue == "0")
        next
  }
  if(entry == 2)
  {
      print("=========================================================================")
      print("==== 2.Complete without parameters (Search for optimal parameters))======")
      print("=========================================================================")
      print("1.Default initial parameters")
      print("2.Initial parameters by user")
      print("=========================================================================")
      while (TRUE) {
        subentry <- readline(prompt = "Enter the option: ")
        if (grepl("^[1-2]+$", subentry)) {
          subentry <- as.integer(subentry)
          break
        } else {
          cat("Invalid option. Please enter an option[1 - 2]..\n")
        }
      }
      Outputs <- readline(prompt = "Enter a name for the results folder:")
      train_proportion <- F_Get_train_proportion()
      if(subentry == 1){
        hyper_grid <- F_Get_hypergrid(2)
        complete <- "(by default)"
      }
      if(subentry == 2){
        hyper_grid <- F_Create_hypergrid()
        complete <- "(by user)"
      }
      print("=========================================================================")
      print("*** Please verify parameters ***")
      print("=========================================================================")
      print(paste("Outputs:",Outputs))
      print(paste("training sample proportion (by user):", train_proportion))
      print(paste("test     sample proportion          :", (1-train_proportion)))
      print(paste("hypergrid combinations",complete,":",nrow(hyper_grid)))
      F_Print_hypergrid_values(hyper_grid)
      print("=========================================================================")
      continue <- readline(prompt = "Press ENTER to continue or 0 to return to the main menu: ")
      if (continue == "0")
        next
  }
  if(entry == 3)
  {
      print("==========================================================================")
      print("== 3.Complete with parameters (Optimal parameters are provided by the user)")
      print("==========================================================================")
      print("1.Manually entered optimal parameters")
      print("2.Optimal parameters loaded from a file")
      print("==========================================================================")
      while (TRUE) {
        subentry <- readline(prompt = "Enter the option: ")
        if (grepl("^[1-2]+$", subentry)) {
          subentry <- as.integer(subentry)
          break
        } else {
          cat("Invalid option. Please enter an option[1 - 2]..\n")
        }
      }
      if(subentry == 1){
        optimal_pars <- F_Create_optimal_params()
      }
      if(subentry == 2){
        optimal_pars <- read.csv(file.choose(), fileEncoding = "latin1")
      }
      Outputs <- readline(prompt = "Enter a name for the results folder:")
      train_proportion <- F_Get_train_proportion()
      print("=========================================================================")
      print("*** Please verify parameters ***")
      print("=========================================================================")
      print(paste("Outputs:",Outputs))
      print(paste("training sample proportion (by user):", train_proportion))
      print(paste("test     sample proportion          :", (1-train_proportion)))
      print("optimal_pars (by user):")
      optimal_pars_trans <- as.data.frame(t(optimal_pars))
      colnames(optimal_pars_trans) <- "Value"
      optimal_pars_trans$Value <- round(optimal_pars_trans$Value, digits = 2)
      print(optimal_pars_trans)
      print("=========================================================================")
      continue <- readline(prompt = "Press ENTER to continue or 0 to return to the main menu: ")
      if (continue == "0")
        next
  }

  print("======================================================================= 3.DATA")
  print("***The following data are required: ***")
  print("==============================================================================")
  print("1.Population data    (CSV)")
  print("2.Geographical areas (SHP)")
  print("3.Bias data          (CSV)")
  print("==============================================================================")
  print("================================================================================= Population data ")
  continue <- readline(prompt = "Press ENTER to load POPULATION DATA or 0 to return to the main menu: ")
  if (continue == "0")
    next
  Population_data<- read.csv(file.choose(), fileEncoding = "latin1")
  colnames(Population_data)[1] <- "Area_code"
  colnames(Population_data)[2] <- "Area_name"
  colnames(Population_data)[3] <- "Total_Population"
  print(head(Population_data,n=5))
  print("================================================================================= Geographical areas")
  continue <- readline(prompt = "Press ENTER to load GEOGRAPHICAL AREAS DATA (.shp) or 0 to return to the main menu: ")
  if (continue == "0")
    next
  Geographical_areas <- sf::st_read(file.choose())
  colnames(Geographical_areas)[1] <- "Area_code"
  Geographical_areas <-  Geographical_areas %>%
    select(Area_code, geometry)
  print(head(Geographical_areas,n=5))
  print("================================================================================= Bias data")
  continue <- readline(prompt = "Press ENTER to load BIAS DATA or 0 to return to the main menu: ")
  if (continue == "0")
    next
  Bias_data <- read.csv(file.choose(), fileEncoding = "latin1")
  colnames(Bias_data)[1] <- "Area_code"
  print(head(Bias_data,n=5))
  print("=========================================================================")
  continue <- readline(prompt = "Press ENTER to continue or 0 to return to the main menu: ")
  if (continue == "0")
    next

  print("================================================================ 4.DATA WRANGLING")
  print("================================================================================= Extract centroids")
  centroids_with_ids<-F_Extract_centroids(Geographical_areas)
  # Casting "Area_code" as integer in order to use left_join
  centroids_with_ids <- centroids_with_ids %>%
    mutate(Area_code = as.integer(Area_code))
  df_extended <- full_join(Population_data, centroids_with_ids, by = join_by(Area_code == Area_code))
  print("================================================================================= Add bias_w")
  if ("bias_w" %in% names(Bias_data)) {
    df_extended <- merge(df_extended, Bias_data[, c("Area_code", "bias_w")], by = "Area_code")
  }
  print("================================================================================= Standardization")
  df_standardised <- F_Standardize(df_extended)
  print("================================================================================= Add bias")
  input_df <- merge(df_standardised, Bias_data[, c("Area_code", "bias")], by = "Area_code")
  print("================================================================================= Missing values")
  input_df <- F_Missing_values(input_df)
  print("================================================================================= Removing unnecessary objects")
  rm(Geographical_areas, df_standardised,df_extended)

  print("============================================================== 5.SETTING UP MODEL")
  cut <- "random"
  # Independent variables
  xvars <- colnames(input_df[, 4:ncol(input_df)-1])
  x <- c(xvars)
  # Dependent variable
  y <- "bias"
  print("================================================================================= Defining validation method")
  set.seed(123)
  split_df <- initial_split(input_df, prop = train_proportion)
  train_sample <- training(split_df)
  test_sample  <- testing(split_df)
  print("================================================================================= Treatment plan")
  # Prepare the training data
  features_train <- F_Treatment_plan(x,train_sample,test_sample,1)
  response_train <- train_sample[,y]

  # Prepare the test data
  features_test <- F_Treatment_plan(x,train_sample,test_sample,2)
  response_test <- test_sample[, y]

  # dimensions of one-hot encoded data
  print("Dimensions of features train one-hot encoded data:")
  print(dim(features_train))
  print("Dimensions of features test  one-hot encoded data:")
  print(dim(features_test))
  print("================================================================================= Training")
  F_Training(features_train,response_train)
  print("================================================================================= Create output folder")
  if (!dir.exists(Outputs)){
    dir.create(Outputs)
  }
  if(entry == 1 || entry == 2)
  {
    print("================================================================================= Saving hypergrid (CSV)")
    write_csv(hyper_grid, file.path(Outputs, "Hypergrid.csv"))
    print("================================================================================= Parallelised code")
    results <- F_Parallelised_code(hyper_grid,features_train,response_train)
    print("================================================================================= Saving results hypergrid (CSV)")
    write_csv(results, file.path(Outputs, "Results_hypergrid.csv"))
    print("================================================================================= Selecting optimal parameters")
    results <- read_csv(file.path(Outputs, "Results_hypergrid.csv"), show_col_types = FALSE)
    print("================================================================================= Extract parameters used for the final model")
    optimal_pars <- results %>%
      slice_min(order_by = min_RMSE)
    print("================================================================================= Saving optimal parameters (CSV)")
    write_csv(optimal_pars[1, ], file.path(Outputs, "Optimal_parameters.csv"))
  }
  if(entry == 3)
  {
    print("================================================================================= Saving optimal parameters (CSV)")
    write_csv(optimal_pars, file.path(Outputs, "Optimal_parameters.csv"))
  }

  print("=================================================================== 6.FINAL MODEL")
  print("================================================================================= Optimal parameters")
  optimal_pars_trans <- as.data.frame(t(optimal_pars))
  colnames(optimal_pars_trans) <- "Value"
  optimal_pars_trans$Value <- round(optimal_pars_trans$Value, digits = 2)
  print(optimal_pars_trans)
  print("================================================================================= Fit final model")
  xgb.fit.final <- F_Fit_final_model(optimal_pars,features_train,response_train)
  print("================================================================================= Saving final model (RDS)")
  saveRDS(xgb.fit.final, file.path(Outputs, "xgb-fit-final-model.rds"))
  print("================================================================================= Predictions")
  train_sample_predictions <- F_Predictions(xgb.fit.final,train_sample,1)
  test_sample_predictions  <- F_Predictions(xgb.fit.final,test_sample,1)
  print("================================================================================= Metrics table")
  train_sample_metrics <- F_Predictions(xgb.fit.final,train_sample,2)
  test_sample_metrics  <- F_Predictions(xgb.fit.final,test_sample,2)
  summary_table <- F_Metrics_table(train_sample_metrics,test_sample_metrics)
  print(summary_table)
  print("================================================================================= Saving model performance (CSV)")
  write_csv(summary_table, file.path(Outputs, "Model-performance.csv"))

  print("====================================================================== 7.GRAPHICS")
  print("================================================================================= Get SHAP values")
  shap_values <- shap.values(xgb_model = xgb.fit.final, X_train = features_train)
  shp <- shapviz(xgb.fit.final, X_pred = features_train, X = features_train)
  print("================================================================================= Bar plot - feature importance")
  feature_importance_png <- F_Bar_plot_feature_importance(shp)
  print("================================================================================= Saving feature importance (PNG)")
  ggsave(file.path(Outputs, "Feature-importance.png"), plot = feature_importance_png, width = 7, height = 7, units = "in", bg = "white")
  print("================================================================================= Feature importance: value & rankings")
  shap_df <- F_Feature_importance_value_rankings(shp)
  print("================================================================================= Saving feature importance (CSV)")
  write_csv(shap_df,file.path(Outputs, "Feature-importance.csv"))
  print("================================================================================= SHAP plot bias")
  Shap_plot_bias <- F_Shap_plot_bias(shp)
  print("================================================================================= Saving shap plot bias (PNG)")
  ggsave(file.path(Outputs, "Shap-plot-bias.png"), plot = Shap_plot_bias, width = 7, height = 7, units = "in", bg = "white")
  print("================================================================================= Partial dependence plots (SHAP dependence)")
  Shap_dependence <- F_Partial_dependence_plots(shp,xgb.fit.final,features_train,shap_values)
  print("================================================================================= Saving shap dependence (PNG)")
  ggsave(file.path(Outputs, "Shap-dependence.png"), plot = Shap_dependence, width = 7, height = 7, units = "in", bg = "white")
  print("================================================================================= Heatmap of municipalities based on feature importance")
  input_df_with_shap_values <- F_Df_with_shap_values(optimal_pars,input_df)
  print("================================================================================= Get 10 largest and 10 smallest areas (selected areas)")
  selected_areas <- F_Get_selected_areas(input_df)
  print("================================================================================= Selected areas sorted alphabetically by feature)")
  heatmap_xxldas <- F_Heatmap_sorted_alphabetically(input_df_with_shap_values,selected_areas)
  print("================================================================================= Saving heatmap features alphabetically (PNG)")
  ggsave(file.path(Outputs, "Heatmap_feature_alphabetically.png"), plot = heatmap_xxldas, width = 7, height = 7, units = "in", bg = "white")
  print("================================================================================= Selected areas sorted by feature importance")
  heatmap_xxldas_fi <- F_Heatmap_sorted_by_feature_importance(shp,input_df_with_shap_values,selected_areas)
  print("================================================================================= Saving heatmap feature importance (PNG)")
  ggsave(file.path(Outputs, "Heatmap_feature_importance.png"), plot = heatmap_xxldas_fi, width = 7, height = 7, units = "in", bg = "white")
  print("=================================================================================")
  print("======================= END OF EXPLAIN BIAS =====================================")
  print("=================================================================================")
  print(paste("============= Please check the ",Outputs," folder to see results ====================="))
  print("=================================================================================")
  continue <- readline(prompt = "Press ENTER to continue or 0 to return to the main menu: ")
  if (continue == "0")
    next
}
print("============================================================================== THANK YOU FOR USING THE PROGRAM")
print("============================================================================== SEE YOU NEXT TIME")
} # End of the function exe()
