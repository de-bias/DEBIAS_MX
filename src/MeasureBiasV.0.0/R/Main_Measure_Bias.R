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
# M E A S U R E      B I A S
rm(list=ls())
F_Load_dependences()
sysfonts::font_add_google("Roboto Condensed", "robotocondensed")
showtext::showtext_auto()
while (TRUE)
{
  # ============================================================================== 0.MENU
  print("=========================================================================")
  print("================ M A I N    M E A S U R E   B I A S =====================")
  print("=========================================================================")
  print("0.Exit")
  print("1.Get all the bias files:")
  print("   -Nonspatial")
  print("   -Queen")
  print("   -Fbw")
  print("   -Knn")
  print("   -Db")
  print("=========================================================================")
  while (TRUE){
    entry <- readline(prompt = "Enter the option: ")
    if (grepl("^[0-1]+$", entry)) {
      entry <- as.integer(entry)
      break
    } else {
      cat("Invalid option. Please enter an option[0 - 1]..\n")
    }
  }
  if(entry == 0)
  {
    break
  }
  print("================================================================ 1.PARAMETERS")
  if(entry == 1)
  {
    print("=========================================================================")
    print("======================= 1.Get all the bias files ========================")
    print("=========================================================================")
    Outputs <- readline(prompt = "Enter a name for the results folder:")
    num_classes <- NULL
    while(TRUE){
      num_classes_input <- readline(prompt = "Enter the number of classes for plots [2 - 10] Example: 8: ")
      if (grepl("^(?:[2-9]|10)$", num_classes_input)){
        num_classes <- as.integer(num_classes_input)
        break
      } else {
        cat("Invalid input. Please enter a number between 2 and 10 (e.g., 2, 5, 10). Letters are not allowed.\n")
      }
    }
    w_schemes <- c("queen", "fbw", "knn", "db")
    print("=========================================================================")
    print("*** Please verify parameters ***")
    print("=========================================================================")
    print(paste("Outputs:",Outputs))
    print(paste("Number of classes for plots:",num_classes))
    print("=========================================================================")
    continue <- readline(prompt = "Press ENTER to continue or 0 to return to the main menu: ")
    if (continue == "0")
      next
  }
  print("======================================================================= 2.DATA")
  print("***The following data are required: ***")
  print("==============================================================================")
  print("1.Population data    (CSV)")
  print("2.Geographical areas (SHP)")
  print("3.Active population  (CSV)")
  print("==============================================================================")
  print("================================================================================= Population data")
  continue <- readline(prompt = "Press ENTER to load POPULATION DATA or 0 to return to the main menu: ")
  if (continue == "0")
    next
  Population_data <- read.csv(file.choose(), fileEncoding = "latin1")
  colnames(Population_data)[1] <- "Area_code"
  colnames(Population_data)[2] <- "Area_name"
  colnames(Population_data)[3] <- "Total_Population"
  Population_data <- Population_data %>% select(Area_code, Area_name, Total_Population)
  # Verify data
  print(head(Population_data,n=5))
  # Verify dimensions
  print("Dimensions:")
  print(dim(Population_data))
  # Verify missing values
  print("Missing values:")
  print(t(t(colSums(is.na(Population_data)))))

  print("================================================================================= Geographical areas")
  continue <- readline(prompt = "Press ENTER to load GEOGRAPHICAL AREAS DATA (.shp) or 0 to return to the main menu: ")
  if (continue == "0")
    next
  Geographical_areas <- sf::st_read(file.choose())
  colnames(Geographical_areas)[1] <- "Area_code"
  Boundaries <- Geographical_areas

  Geographical_areas <-  Geographical_areas %>%
    select(Area_code, geometry)

  # Verify data
  print(head(Geographical_areas,n=5))
  # Verify dimensions
  print("Dimensions:")
  print(dim(Geographical_areas))
  # Verify missing values
  print("Missing values:")
  print(t(t(colSums(is.na(Geographical_areas)))))
  # =================================================================================
  # Simplifies the complexity of spatial geometries
  Boundaries <- Boundaries %>%st_simplify(preserveTopology = TRUE, dTolerance = 1000)
  # Transforms the coordinate system of spatial geometries
  Boundaries <- st_transform(Boundaries,  crs = 4326)

  # Verify data
  print(head(Boundaries,n=5))
  # Verify dimensions
  print("Dimensions:")
  print(dim(Boundaries))
  # Verify missing values
  print("Missing values:")
  print(t(t(colSums(is.na(Boundaries)))))

  print("================================================================================= Active population")
  continue <- readline(prompt = "Press ENTER to load ACTIVE POPULATION or 0 to return to the main menu: ")
  if (continue == "0")
    next
  Active_population <- read.csv(file.choose(), fileEncoding = "latin1")
  colnames(Active_population)[1] <- "Area_code"
  colnames(Active_population)[2] <- "Area_name"
  colnames(Active_population)[3] <- "Total_active_population"

  Active_population <- Active_population %>% select(Area_code, Area_name, Total_active_population) %>%
    mutate(Area_code = as.integer(Area_code))

  # Verify data
  print(head(Active_population,n=5))
  # Verify dimensions
  print("Dimensions:")
  print(dim(Active_population))
  # Verify missing values
  print("Missing values:")
  print(t(t(colSums(is.na(Active_population)))))
  print("=========================================================================")
  continue <- readline(prompt = "Press ENTER to continue or 0 to return to the main menu: ")
  if (continue == "0")
    next
  print("================================================================ 3.MEASURE BIAS")
  Bias_measurement <- F_Measure_bias(Population_data,Active_population)
  print("Dimensions:")
  print(dim(Bias_measurement))
  print(head(Bias_measurement,n=5))
  # Copy dataframe
  Bias_measurement_ORI <- Bias_measurement
  print("================================================================ 3.1.Missing Values - Listwise deletion")
  # Check for missing values
  Missing_values <- sum(is.na(Bias_measurement))
  print(paste0("   Missing values: ",Missing_values))
  print("========================")

  if(Missing_values > 0)
  {
    Data <- F_Listwise_deletion(Bias_measurement)
    Bias_measurement    <- Data$Dataframe
    Bias_measurement_NA <- Data$Dataframe_NA
  }else{
    Bias_measurement_NA <- data.frame()
  }
  print("================================================================ 3.2.Dropping Negative Bias")
  # Check for negative bias
  Negative_bias <- sum(Bias_measurement$bias < 0)
  print(paste0("   Negative values: ",Negative_bias))
  print("========================")

  if(Negative_bias > 0)
  {
    Data <- F_Drop_negative_bias(Bias_measurement)
    Bias_measurement     <- Data$Dataframe
    Bias_measurement_NEG <- Data$Dataframe_NEG
    Negative_bias_       <- Data$Negative_bias_
  }else{
    Bias_measurement_NEG <- data.frame()
  }
  print("================================================================ 3.3.Data information")
  Data_information <- F_Data_information(Bias_measurement,Population_data,Active_population,Boundaries,Bias_measurement_NA,Bias_measurement_NEG)
  print(Data_information)
  print("================================================================ 3.4. Correlation graph")
  correlation_graph <- F_Correlation_graph(Bias_measurement)
  print("================================================================ 3.5.Create folders for outputs and saving")
  F_Create_outputs_folder(Outputs)
  print("================================================================ 3.5.1.Saving data information")
  File_name <- "Data_information.csv"
  F_Saving_csv(Outputs,File_name,Data_information)
  print("================================================================ 3.5.2.Saving correlation graph")
  File_name <- "Correlation_graph.png"
  F_Saving_png(Outputs,File_name,correlation_graph)
  print("================================================================ 3.5.3.Saving Measure bias")
  File_name <- "Active_population_bias_nonspatial.csv"
  F_Saving_csv(Outputs,File_name,Bias_measurement)
  # MISSING VALUES
  if(Missing_values > 0)
  {
    print("================================================================ 3.5.4.Saving Missing values")
    File_name <- "Active_population_bias_NA.csv"
    F_Saving_csv(Outputs,File_name,Bias_measurement_NA)
    #rm(Bias_measurement_NA)
  }
  # NEGATIVE BIAS
  if(Negative_bias > 0)
  {
    print("================================================================ 3.5.5.Saving Negative bias")
    File_name <- "Active_population_bias_NEG.csv"
    F_Saving_csv(Outputs,File_name,Bias_measurement_NEG)
    #rm(Bias_measurement_NEG)
  }
  # ORIGINAL DATA WITH MISSING VALUES AND/OR NEGATIVE BIAS
  if(Missing_values > 0 || Negative_bias > 0)
  {
    print("================================================================ 3.5.6.Saving original data")
    File_name <- "Active_population_bias_ORI.csv"
    F_Saving_csv(Outputs,File_name,Bias_measurement_ORI)
    #rm(Bias_measurement_ORI)
  }
  print("================================================================ 4.SCATTER PLOT")
  Scatter_plot <- F_Scatter_plot(Bias_measurement,num_classes)
  print("================================================================ 4.1.Saving scatter plot")
  File_name <- "/Scatter/Scatter.png"
  F_Saving_png(Outputs,File_name,Scatter_plot)
  print("================================================================ 5.HISTOGRAM BIAS")
  Histograms <- F_Histograms(Bias_measurement,num_classes)
  print("================================================================ 5.1.Labelled histogram")
  Labelled_histogram <- Histograms$Labelled_histogram
  print("================================================================ 5.1.1Saving labelled histogram")
  File_name <- "/Histogram/Labelled_histogram.png"
  F_Saving_png(Outputs,File_name,Labelled_histogram)
  print("================================================================ 5.2.Unlabelled histogram")
  Unlabelled_histogram <- Histograms$Unlabelled_histogram
  print("================================================================ 5.2.1.Saving unlabelled histogram")
  File_name <- "/Histogram/Unlabelled_histogram.png"
  F_Saving_png(Outputs,File_name,Unlabelled_histogram)
  print("================================================================ 5.3.Adjusted unlabelled histogram")
  hist_data <- data.frame(value = Bias_measurement$bias*100)
  minimum_x <- sort(hist_data$value)[1]
  print(paste0("Minimum x:",minimum_x))
  # Adjusted plot only if the minimum value is < 50
  if (minimum_x < 50)
  {
    Adjusted_unlabelled_histogram <- F_Adjusted_histogram(Bias_measurement,num_classes)
    print("================================================================ 5.3.1.Saving adjusted unlabelled histogram")
    File_name <- "/Histogram/Adjusted_unlabelled_histogram_50.png"
    F_Saving_png(Outputs,File_name,Adjusted_unlabelled_histogram)
  }
  print("================================================================ 6.SPATIAL AUTOCORRELATION")
  # Casting "Area_code" as integer in order to use left_join
  Boundaries <- Boundaries %>%
    mutate(Area_code = as.integer(Area_code))
  Bias_map <- Boundaries %>% left_join(Bias_measurement, by = "Area_code") %>% st_as_sf()
  # Verify data
  print(head(Bias_map,n=5))
  print("================================================================ Data missing treatment")
  # Check for missing values
  print(t(t(colSums(is.na(Bias_map)))))
  print(dim(Bias_map))
  if(sum(is.na(Bias_map))>0)
  {
    Bias_map <- na.omit(Bias_map)
    print(t(t(colSums(is.na(Bias_map)))))
    print(dim(Bias_map))
  }
  print("================================================================ 6.1.Option 1: Queen")
  Structure_queen <- F_Spatial_queen(Bias_map)
  print("================================================================ 6.2.Option 2: Finding optimal fixed band width")
  Structure_fbw <- F_Spatial_fbw(Bias_map)
  print("================================================================ 6.3.Option 3: Neighbourhoods based on k-nearest neighbours")
  {
    print("================================================================ 6.3.1.Searching for the optimum k and the lowest number of subgraphs")
    Analysis_knn    <- F_Searching_optimum_k(Bias_map)
    best_k          <- Analysis_knn$best_k
    knn_figure      <- Analysis_knn$knn_figure
    moran_results_df<- Analysis_knn$moran_results_df
    print("================================================================ 6.3.2.Saving knn analysis")
    File_name <- paste0("Knn_Analysis_",best_k, ".csv")
    F_Saving_csv(Outputs,File_name,moran_results_df)
    File_name <- "Knn_plots.png"
    F_Saving_png(Outputs,File_name,knn_figure,1)
  }
  Structure_knn <- F_Spatial_knn(Bias_map,best_k)
  print("================================================================ 6.4.Option 4: Neighbourhoods based on distance band")
  Structure_db <- F_Spatial_db(Bias_map)
  print("================================================================ 6.5.Save data with spatial lag")
  for (w_scheme in w_schemes)
  {
    File_name <- paste0("Active_population_bias_",w_scheme,".csv")
    structure_name <- paste0("Structure_", w_scheme)
    Current_structure <- get(structure_name)
    Current_bias_map <- Current_structure$Bias_map
    Current_bias_map <- st_drop_geometry(Current_bias_map)
    F_Saving_csv(Outputs,File_name,Current_bias_map)
  }
  print("================================================================ 6.6.Moran's I, p_value & Z-score (only for spatial cases)")
  Morans_results <- F_Moran_results(w_schemes,Structure_queen,Structure_fbw,Structure_knn,Structure_db)
  cat("\n--- Data.frame of all Moran's I results\n")
  print(Morans_results)
  print("================================================================ 6.6.1.Saving Moran's analysis")
  File_name <- paste0("All_Moran_Pvalues.csv")
  F_Saving_csv(Outputs,File_name,Morans_results)
  print("================================================================ 7.Map bias")
  Map <- F_Map(Bias_measurement,Bias_map,num_classes)
  File_name <- "/Map/Map_nonspatial.png"
  F_Saving_png(Outputs,File_name,Map)

  for (w_scheme in w_schemes){
    structure_name <- paste0("Structure_", w_scheme)
    Current_structure <- get(structure_name)
    current_Bias_map  <- Current_structure$Bias_map

    current_moran_stats <- Morans_results[Morans_results$Scheme == w_scheme, ]
    # Checks if data was found for the current scheme
    if (nrow(current_moran_stats) == 0){
      cat(paste0("Warning: No Moran statistics were found for the scheme ", w_scheme, ". Map generation is omitted.\n"))
      next # Goes to the next iteration of the loop
    }
    # Extracts the values of mI and p_value_mI from the filtered row
    mI <- as.numeric(current_moran_stats$Moran_I_mc)
    p_value_mI <- as.numeric(current_moran_stats$P_value_mc) # Using P_value from moran.mc

    cat(paste0("\n--- Generating map for the scheme:: ", w_scheme, " ---\n"))
    Map <- F_Map(Bias_measurement,current_Bias_map,num_classes,1,mI,p_value_mI)
    File_name <- paste0("/Map/Map_",w_scheme,".png")
    F_Saving_png(Outputs,File_name,Map)
  }
  print("================================================================ 8.Final figure")
  {
    # Paths
    map_path <- paste0(Outputs, "/Map/Map_nonspatial.png")
    unlabelled_histogram_path <- paste0(Outputs, "/Histogram/Unlabelled_histogram.png")
    adjusted_histogram_path <- paste0(Outputs, "/Histogram/Adjusted_unlabelled_histogram_50.png")
    scatter_path <- paste0(Outputs,"/Scatter/Scatter.png")

    Final_figure <- F_Load_join_images(map_path,unlabelled_histogram_path,adjusted_histogram_path,scatter_path)
  }
  print("================================================================ 8.1.Saving Final figure")
  print("=================================================================================")
  print("======================= END OF MEASURE BIAS =====================================")
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
