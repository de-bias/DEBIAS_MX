# ==============================================================================
# ======================== F_Measure_bias ======================================
# ==============================================================================
#' F_Measure_bias
#'
#' Function to obtain the bias between 2 datasets
#'
#' @param
#'        Population_data:   data from population like data census
#'        Active_population: data from other source like facebook or phone
#' @return
#'        A dataset with bias measurement
#' @examples
#'        Bias_data <- F_Measure_bias(Dataframe01, Dataframe02)
#' @export
F_Measure_bias <- function(Population_data,Active_population)
{
  # Join dataframes
  Bias_data <- Population_data %>% left_join(Active_population, by = "Area_code")
  # Create Bias column
  Bias_data$bias <- 1 - (Bias_data$Total_active_population)/(Bias_data$Total_Population)
  # Create Map_bias column
  Bias_data$Map_bias <- (Bias_data$bias * 100)
  # Select columns
  Bias_data<- Bias_data %>%
    select(Area_code,
           Area_name.x,
           Total_Population,
           Total_active_population,
           bias,
           Map_bias)%>%
    rename("Area_name" = "Area_name.x")
  return(Bias_data)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Listwise_deletion =================================
# ==============================================================================
#' F_Listwise_deletion
#'
#' Function for handling missing values using listwise deletion method
#'
#' @param
#'        Data: dataframe with missing values
#' @return
#'        A dataframe without missing values and a dataframe only with
#'        missing values
#' @examples
#'        Data <- F_Listwise_deletion(Dataframe)
#' @export
F_Listwise_deletion <- function(Dataframe)
{
  print("Data before listwise deletion")
  print("========================")
  print(t(t(colSums(is.na(Dataframe)))))
  print("========================")
  print("Dimension")
  print(dim(Dataframe))
  print("========================")

  # 1.Identify the rows with NA
  rows_na <- which(rowSums(is.na(Dataframe)) > 0)

  # 2.Create a new dataframe with the rows with NA named Dataframe_NA
  Dataframe_NA <- Dataframe[rows_na, ]

  # 3.Dropping missing values from Dataframe
  Dataframe <- na.omit(Dataframe)

  # Check for missing values
  print("========================")
  print("Data after listwise deletion")
  print("========================")
  print(t(t(colSums(is.na(Dataframe)))))
  print("========================")
  print("Dimension")
  print(dim(Dataframe))
  print("========================")
  print("Dimension missing values")
  print(dim(Dataframe_NA))
  print("========================")

  return_structure <- list(
    Dataframe     = Dataframe,
    Dataframe_NA = Dataframe_NA)
  return(return_structure)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Drop_negative_bias ================================
# ==============================================================================
#' F_Drop_negative_bias
#'
#' Function to eliminate negative biases
#'
#' @param
#'        Data: dataframe without negative biases
#' @return
#'        A dataframe without negative biases and a dataframe only with
#'        negative biases
#' @examples
#'        Data <- F_Drop_negative_bias(Dataframe)
#' @export
F_Drop_negative_bias <- function(Dataframe)
{
  # 1.Create a new dataframe with the rows with negative bias named Dataframe_NEG
  Dataframe_NEG <- Dataframe %>% filter(bias < 0)
  print("Dimension negative values")
  print(dim(Dataframe_NEG))
  print("========================")

  # 2.Dropping negative values from Dataframe
  Dataframe <- Dataframe %>% filter(bias > 0)
  print("Dimension positive values")
  print(dim(Dataframe))
  print("========================")

  # Check for negative bias
  Negative_bias_ <- sum(Dataframe$bias < 0)
  print(paste0("   Negative values: ",Negative_bias_))
  print("========================")

  return_structure <- list(
    Dataframe     = Dataframe,
    Dataframe_NEG = Dataframe_NEG,
    Negative_bias_= Negative_bias_
    )
  return(return_structure)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Data_information ================================
# ==============================================================================
#' F_Data_information
#'
#' Function to summarise dataset information
#'
#' @param
#'        Bias_data        : data with bias
#'        Population_data  : data from population like data census
#'        Active_population: data from other source like facebook or phone
#'        Boundaries       : data from shp file
#'        Bias_NA          : dataframe with only NA values
#'        Bias_NEG         : dataframe with only Negative bias
 #' @return
#'        A dataframe with the dataset information
#' @examples
#'        Data <- F_Drop_negative_bias(Bias,Population,Active,Boundaries,NA,Negatives)
#' @export
F_Data_information <- function(Bias_data,Population_data,Active_population,Boundaries,Bias_NA,Bias_NEG)
{
  total_population  <- sum(Population_data$Total_Population)                    # Total Population
  active_population <- sum(Active_population$Total_active_population)           # Active Population
  coverage          <- (active_population/total_population)*100                 # Coverage
  total_areas       <- dim(Boundaries)[1]                                       # Total areas from SHP file
  areas_from_total_population <- dim(Population_data)[1]                        # Areas from total population
  areas_from_active_population <- dim(Active_population)[1]                     # Areas from active population
  missing_values_areas <- dim(Bias_NA)[1]                                       # Missing values areas
  areas_after_missing <- (areas_from_total_population - missing_values_areas)   # Areas after missing values treatment
  negative_bias_areas <- dim(Bias_NEG)[1]                                       # Negative bias areas
  areas_after_negative <- dim(Bias_data)[1]                                     # Areas after negative bias treatment
  pearson_correlation <- cor(Bias_data$Total_Population, Bias_data$Total_active_population) # Pearson correlation

  # Create dataframe
  data_information <- data.frame(
    Metric = c("Total population",
               "Active population",
               "Coverage",
               "Total areas from SHP file",
               "Areas from total population",
               "Areas from active population",
               "Missing values areas",
               "Areas after missing values treatment",
               "Negative bias areas",
               "Areas after negative bias treatment",
               "Pearson correlation"),
    Value = c(total_population,
              active_population,
              coverage,
              total_areas,
              areas_from_total_population,
              areas_from_active_population,
              missing_values_areas,
              areas_after_missing,
              negative_bias_areas,
              areas_after_negative,
              pearson_correlation)
  )

  # Format columns
  data_information$Value <- ifelse(data_information$Metric == "Coverage",
                                   sprintf("%.2f", data_information$Value),
                                   ifelse(data_information$Metric == "Pearson correlation",
                                          sprintf("%.2f", data_information$Value),
                                          sprintf("%.0f", data_information$Value)))
  return(data_information)
}
# ==============================================================================

# ==============================================================================
# ======================== F_Correlation_graph =================================
# ==============================================================================
#' F_Correlation_graph
#'
#' Function to create a correlation graph between two variables in a given format.
#'
#' @param
#'        A dataframe with the 2 variables of interest
#' @return
#'        A PGN correlation graph
#' @examples
#'        plot <- F_Correlation_graph(Dataframe)
#' @export
F_Correlation_graph <- function(Dataframe)
{
  # Create the correlation graph
  return(ggplot(Dataframe, aes(x = Total_Population, y = Total_active_population)) +
           geom_point() +
           labs(title = "CORRELATION GRAPH",
                x = "Total Population",
                y = "Total Active Population") +
           theme_bw() +
           #theme_plot_tufte() +
           theme(axis.text.x = element_text(size = 40),
                 axis.text.y = element_text(size = 40),
                 axis.title.x = element_text(size = 40),
                 axis.title.y = element_text(size = 40),
                 plot.title = element_text(size = 40)))
}
# ==============================================================================

# ==============================================================================
# ======================== F_Create_outputs_folder =============================
# ==============================================================================
#' F_Create_outputs_folder
#'
#' Function to create folder and subfolder for outputs
#'
#' @param
#'        Outputs: A name for the output folder
#' @return
#'        None
#' @examples
#'        F_Create_outputs_folder(Outputs)
#' @export
F_Create_outputs_folder <- function(Outputs)
{
  # 1. Create the main folder if it does not exist
  if (!dir.exists(Outputs)) {
    dir.create(Outputs)
    cat(paste0("Main Folder Output '", Outputs, "' successfully created.\n"))
  } else {
    cat(paste0("Main Folder Output '", Outputs, "' already exists.\n"))
  }

  # 2. Create the subfolders inside the main folder.
  Subfolders <- c("Scatter", "Histogram", "Map")
  for (Subfolder in Subfolders) {
    path_subfolder <- file.path(Outputs, Subfolder)
    if (!dir.exists(path_subfolder)) {
      dir.create(path_subfolder)
      cat(paste0("Subfolder '", Subfolder, "' created in '", Outputs, "' successfully.\n"))
    } else {
      cat(paste0("Subfolder '", Subfolder, "' already exists in '", Outputs, "'.\n"))
    }
  }
  cat("Folder creation process completed.\n")
}
# ==============================================================================

# ==============================================================================
# ============================== F_Saving_csv ==================================
# ==============================================================================
#' F_Saving_csv
#'
#' Function to save files in csv format
#'
#' @param
#'        Outputs: Folder and path where the file will be stored
#'        Name   : Name under which the file will be stored
#'        Data   : Data or file to be saved
#' @return
#'        None
#' @examples
#'        F_Saving_csv(Outputs,Name,Data)
#' @export
F_Saving_csv <- function(Outputs,Name,Data)
{
  # Try to write the CSV file and save the result.
  write_output <- try(write.csv(Data, file.path(Outputs, Name), fileEncoding = "latin1", row.names = FALSE))
  # Check if there were any errors during writing
  if (inherits(write_output, "try-error")) {
    cat(paste0("Error saving: ",Name," in '", Outputs, "'.\n"))
    cat("Error message: ", as.character(write_output), "\n")
  } else {
    cat(paste0(Name," successfully saved in '", Outputs, "'.\n"))
  }
}
# ==============================================================================

# ==============================================================================
# =============================== F_Saving_png =================================
# ==============================================================================
#' F_Saving_png
#'
#' Function to save files in png format
#'
#' @param
#'        Outputs: Folder and path where the file will be stored
#'        Name   : Name under which the file will be stored
#'        Plot   : Plot to be saved
#'        Option : 0 - default image measurements / other number - predefined image measurements
#' @return
#'        None
#' @examples
#'        F_Saving_csv(Outputs,Name,Plot)
#' @export
F_Saving_png <- function(Outputs,Name,Plot,Option = 0)
{
  # Saving correlation graph
  if(Option == 0)
    write_output <- try(ggsave(file.path(Outputs,Name), plot = Plot, bg = "white"))
  else
    write_output <- try(ggsave(file.path(Outputs,Name), plot = Plot, bg = "white",width = 18, height = 6, units = "in"))
  # Check if there were any errors during saving
  if (inherits(write_output, "try-error")) {
    cat(paste0("Error saving: ",Name," in '", Outputs, "'.\n"))
    cat("Error message: ", as.character(write_output), "\n")
  } else {
    cat(paste0(Name," successfully saved in '", Outputs, "'.\n"))
  }
}
# ==============================================================================

# ==============================================================================
# ============================== F_Scatter_plot ================================
# ==============================================================================
#' F_Scatter_plot
#'
#' Function to create a scatter plot
#'
#' @param
#'        Dataframe   : Data to generate the plot
#'        num_classes : Number of classes or partitions for the plot
#' @return
#'        Scatter plot
#' @examples
#'        Plot <- F_Scatter_plot(Dataframe,num_classes)
#' @export
F_Scatter_plot <- function(Dataframe,num_classes)
{
  # Dividing data into intervals
  jenks_breaks <- classIntervals(Dataframe$bias*100, n = num_classes, style = "jenks")$brks

  # Assigns unique intervals
  jenks_breaks <- unique(jenks_breaks)
  print("Breaks:")
  print(jenks_breaks)

  # Create a factor variable for color breaks
  Dataframe$jenks_bins <- cut(Dataframe$bias*100, breaks = jenks_breaks, include.lowest = TRUE)

  # Create a custom color palette from viridis
  jenks_colors <- viridis(length(jenks_breaks) - 1)

  # Manually format the legend labels to avoid scientific notation
  formatted_labels <- sapply(1:(length(jenks_breaks) - 1), function(i) {
    paste0("[",
           sprintf("%.1f", jenks_breaks[i]), ", ",
           sprintf("%.1f", jenks_breaks[i + 1]), ")")
  })
  print("Labels:")
  print(formatted_labels)

  # Perform the Pearson correlation test, removing NA values
  correlation_test <- cor.test(Dataframe$Total_Population, Dataframe$Total_active_population, use = "complete.obs", method = "pearson")
  correlation_coefficient <- round(correlation_test$estimate, 2)
  p_value <- correlation_test$p.value
  p_annotation <- ifelse(p_value < 0.05, "p < 0.05", paste("p =", round(p_value, 2)))
  annotation <- data.frame(
    x = c(max(Dataframe$Total_Population * 0.6, na.rm = TRUE),
          max(Dataframe$Total_Population * 0.6, na.rm = TRUE)),
    y = c(max(Dataframe$Total_active_population * 0.95, na.rm = TRUE),
          max(Dataframe$Total_active_population * 0.85, na.rm = TRUE)),
    label = c(paste0("r = ", correlation_coefficient), p_annotation)
  )

  annotation

  # Create the plot
  Scatter_plot <- ggplot(Dataframe, aes(x = Total_Population, y = Total_active_population, color = jenks_bins)) +
    geom_point(size = 5, alpha = 0.6, stroke = 0.7) +
    scale_color_viridis_d(
      na.translate = TRUE,
      na.value = "gray"  # Color for NA values
    ) +
    labs(
      x = "Total population",
      y = "Total active population"
    ) +
    geom_text(data=annotation, aes(x=x, y=y, label=label),
              color="black",
              size=25,
              angle=0,
              fontface="bold",
              family = "robotocondensed") +
    scale_x_continuous(labels = label_number(scale = 0.001, suffix = " k")) +
    scale_y_continuous(labels = label_number(scale = 0.001, suffix = " k")) +
    theme_plot_tufte() +
    theme(axis.title.x = element_text(size = 60),  # Set font size for X-axis title
          axis.title.y = element_text(size = 60),   # Set font size for Y-axis title
          axis.text.x = element_text(size = 50),     # Font size for X-axis ticks
          axis.text.y = element_text(size = 50),
          legend.position = "none",
          plot.margin = unit(c(1, 2, 1, 1), "lines")
    )
  return(Scatter_plot)
}
# ==============================================================================

# ==============================================================================
# ============================= F_Histograms ===================================
# ==============================================================================
#' F_Histograms
#'
#' Function to create histograms
#'
#' @param
#'        Dataframe   : Data to generate the plot
#'        num_classes : Number of classes or partitions for the plot
#' @return
#'        2 histograms:Labelled_histogram and Unlabelled_histogram
#' @examples
#'        Plots <- F_Histograms(Dataframe,num_classes)
#' @export
F_Histograms  <- function(Dataframe,num_classes)
{
  # Dividing data into intervals
  jenks_breaks <- classIntervals(Dataframe$bias*100, n = num_classes, style = "jenks")$brks

  # Assigns unique intervals
  jenks_breaks <- unique(jenks_breaks)

  # Create a data frame with histogram information
  hist_data <- data.frame(value = Dataframe$bias*100)

  # Create a factor variable for color breaks
  hist_data$jenks_bins <- cut(hist_data$value, breaks = jenks_breaks, include.lowest = TRUE)

  # Create a custom color palette from viridis
  jenks_colors <- viridis(length(jenks_breaks) - 1)

  # Manually format the legend labels to avoid scientific notation
  formatted_labels <- sapply(1:(length(jenks_breaks) - 1), function(i) {
    paste0("[",
           sprintf("%.1f", jenks_breaks[i]), ", ",
           sprintf("%.1f", jenks_breaks[i + 1]), ")")
  })

  minimum_x <- sort(hist_data$value)[1]
  binwidth <- .16

  # Plot the labelled histogram with ggplot
  Labelled_histogram <- ggplot(hist_data, aes(x = value, fill = jenks_bins)) +
    xlim(minimum_x, 100) +
    geom_histogram(binwidth=binwidth, color = "black", linewidth = 0.3) +
    scale_fill_viridis_d(
      name = "Size of bias",
      labels = formatted_labels,
      na.translate = TRUE,
      na.value = "gray"  # Set color for NA values
    ) +
    labs(x = "Size of bias", y = "Frequency") +
    theme_plot_tufte() +
    theme(
      legend.text = element_text(size = 60),    # Increase legend text size
      legend.title = element_text(size = 60),   # Increase legend title size
      legend.position = "right",                # Optional: adjust legend position
      axis.title.x = element_text(size = 63),   # Set font size for X-axis title
      axis.title.y = element_text(size = 63),   # Set font size for Y-axis title
      axis.text.x = element_text(size = 50),    # Font size for X-axis ticks
      axis.text.y = element_text(size = 50)     # Font size for Y-axis ticks
    )

    # Plot the unlabelled histogram with ggplot
    Unlabelled_histogram <- ggplot(hist_data, aes(x = value, fill = jenks_bins)) +
      xlim(minimum_x ,100) +
      geom_histogram(binwidth=binwidth, color = "black", linewidth = 0.3) +
      scale_fill_viridis_d(
        name = "Size of bias",
        labels = formatted_labels,
        na.translate = TRUE,
        na.value = "gray",
        guide = "none"
      ) +
      labs(x = "Size of bias", y = "Frequency") +
      theme_plot_tufte() +
      theme(
        axis.title.x = element_text(size = 63),
        axis.title.y = element_text(size = 63),
        axis.text.x = element_text(size = 50),
        axis.text.y = element_text(size = 50),
        legend.position = "none"
      )

    return_structure <- list(
      Labelled_histogram   = Labelled_histogram,
      Unlabelled_histogram = Unlabelled_histogram
    )
    return(return_structure)
}
# ==============================================================================

# ==============================================================================
# =========================== F_Adjusted_histogram =============================
# ==============================================================================
#' F_Adjusted_histogram
#'
#' Function to create adjusted histogram
#'
#' @param
#'        Dataframe   : Data to generate the plot
#'        num_classes : Number of classes or partitions for the plot
#' @return
#'        Adjusted scatter plot (range 50 - 100)
#' @examples
#'        Plots <- F_Adjusted_histogram(Dataframe,num_classes)
#' @export
F_Adjusted_histogram  <- function(Dataframe,num_classes)
{
  # min_x_values <- c(50, 60, 70, 80, 90)
  min_x <- 50

  # Dividing data into intervals
  jenks_breaks <- classIntervals(Dataframe$bias*100, n = num_classes, style = "jenks")$brks

  # Assigns unique intervals
  jenks_breaks <- unique(jenks_breaks)

  # Create a data frame with histogram information
  hist_data <- data.frame(value = Dataframe$bias*100)

  # Create a factor variable for color breaks
  hist_data$jenks_bins <- cut(hist_data$value, breaks = jenks_breaks, include.lowest = TRUE)

  # Crear la paleta de colores viridis con el número de breaks - 1
  jenks_colors <- viridis(length(jenks_breaks) - 1)

  # Obtener los niveles únicos de jenks_bins (en el orden en que aparecen)
  unique_bins_ordered <- levels(hist_data$jenks_bins)

  # Asegurarse de que el número de colores coincida con el número de niveles únicos
  if (length(jenks_colors) >= length(unique_bins_ordered)) {
    # Asignar los colores a los niveles únicos
    names(jenks_colors) <- unique_bins_ordered
  } else {
    warning("El número de colores en 'jenks_colors' es menor que el número de intervalos únicos.")
  }

  # Manually format the legend labels
  formatted_labels <- sapply(1:(length(jenks_breaks) - 1), function(i) {
    paste0("[",
           sprintf("%.1f", jenks_breaks[i]), ", ",
           sprintf("%.1f", jenks_breaks[i + 1]), ")")
  })

  binwidth <- .16
  Adjusted_unlabelled_histogram <- ggplot(hist_data, aes(x = value, fill = jenks_bins)) +
    xlim(min_x, 100) + # Establecer el límite inferior dinámicamente
    geom_histogram(binwidth = binwidth, color = "black", linewidth = 0.3) +
    scale_fill_manual(
      name = "Size of bias",
      values = jenks_colors,
      labels = formatted_labels
    ) +
    labs(x = "Size of bias", y = "Frequency") +
    theme_plot_tufte() +
    theme(
      axis.title.x = element_text(size = 63),
      axis.title.y = element_text(size = 63),
      axis.text.x = element_text(size = 50),
      axis.text.y = element_text(size = 50),
      legend.position = "none"
    )
  return(Adjusted_unlabelled_histogram)
}
# ==============================================================================

# ==============================================================================
# =========================== F_Spatial_queen ==================================
# ==============================================================================
#' F_Spatial_queen
#'
#' Function to Compute spatial lag with queen method
#'
#' @param
#'        Bias_map   : Geographical data with bias
#' @return
#'        A structure with spatial lag, number of subgraphs and weight matrix
#' @examples
#'        Structure <- F_Spatial_queen(Bias_map)
#' @export
F_Spatial_queen  <- function(Bias_map)
{
  if ("bias_w" %in% colnames(Bias_map)) {
    Bias_map <- Bias_map[, !names(Bias_map) %in% "bias_w"]
  }
  suffix <- '_w_queen'

  # Extract the coordinates of the centroids (or your points)
  coords <- st_coordinates(st_centroid(Bias_map))

  # Create queen neighbours list
  nb_q <- poly2nb(Bias_map, queen = TRUE)
  subgraphs <- n.comp.nb(nb_q)$nc

  # Create a row-standardised spatial weights matrix
  w_matrix <- nb2listw(nb_q, style = "W", zero.policy = TRUE)

  # Compute spatial lag
  bias_w <- lag.listw(w_matrix, Bias_map$bias, NAOK = TRUE)

  # Save spatial lag in additional column
  Bias_map$bias_w <- bias_w

  cat("The number of disconnected subgraphs for queen is: ", subgraphs, "\n")

  return_structure <- list(
    Bias_map  = Bias_map,
    subgraphs = subgraphs,
    w_matrix  = w_matrix
  )
  return(return_structure)
}
# ==============================================================================

# ==============================================================================
# =========================== F_Spatial_fbw =====================================
# ==============================================================================
#' F_Spatial_fbw
#'
#' Function to Compute spatial lag with fbw method
#'
#' @param
#'        Bias_map   : Geographical data with bias
#' @return
#'        A structure with spatial lag, number of subgraphs and weight matrix
#' @examples
#'        Structure <- F_Spatial_queen(Bias_map)
#' @export
F_Spatial_fbw  <- function(Bias_map)
{
  if ("bias_w" %in% colnames(Bias_map)) {
    Bias_map <- Bias_map[, !names(Bias_map) %in% "bias_w"]
  }
  suffix <- '_w_fbw'
  coords <- st_coordinates(st_centroid(Bias_map))
  # find optimal kernel bandwidth using cross validation
  fbw <- gwr.sel(Map_bias ~ 1,
                 data = Bias_map,
                 coords=cbind( coords[, "X"], coords[, "Y"]),
                 longlat = TRUE,
                 adapt = FALSE,
                 gweight = gwr.Gauss,
                 verbose = FALSE)
  # Create a distance-based neighbors list with a minimum distance of 0 and maximum   distance given by fbw
  nb_d <- dnearneigh(st_coordinates(st_centroid(Bias_map)), d1=0, d2=fbw)
  # Subgraphs
  subgraphs <- n.comp.nb(nb_d)$nc
  # Create a row-standardised spatial weights matrix using distance-based neighbors
  w_matrix <- nb2listw(nb_d, style = "W", zero.policy=TRUE)
  # WARNING!!!!! This weights matrix leaves too many observations disconnected. Below we print the percentage of observations with no neighbors:
  # Find the number of observations with zero neighbors
  zero_neighbors_count <- sum(unlist(lapply(w_matrix$weights, length)) == 0)
  zero_neighbors_count <- (zero_neighbors_count/nrow(Bias_map)*100)
  zero_neighbors_count
  # Compute spatial lag
  bias_w <- lag.listw(w_matrix, Bias_map$bias, NAOK=TRUE)
  # Save spatial lag in additional column
  Bias_map$bias_w <- bias_w

  cat("The number of disconnected subgraphs for fbw is: ", subgraphs, "\n")

  return_structure <- list(
    Bias_map  = Bias_map,
    subgraphs = subgraphs,
    w_matrix  = w_matrix
  )
  return(return_structure)
}
# ==============================================================================

# ==============================================================================
# =========================== F_Searching_optimum_k ============================
# ==============================================================================
#' F_Searching_optimum_k
#'
#' Function to search the optimum k for knn
#'
#' @param
#'        Bias_map   : Geographical data with bias
#' @return
#'        best_k            :optimal k from the analysis
#'        knn_figure        :plots of the results of the analysis
#'        moran_results_df  :file of the result of the analysis
#' @examples
#'        Knn_analysis <- F_Searching_optimum_k(Bias_map)
#' @export
F_Searching_optimum_k  <- function(Bias_map)
{
  # === Extract the coordinates of the centroids (or your points)
  coords <- st_coordinates(st_centroid(Bias_map))

  # === Range of k values to explore
  k_values <- 1:20

  # === List for storing results
  moran_results_list <- list()

  # === Variables for storing the best k and its weight matrix
  best_k_combined <- NA
  best_w_matrix_combined <- NULL
  max_moran_abs_connected <- -Inf
  min_components <- Inf

  # === Loop to iterate over the values of k
  for (k in k_values)
  {
    # Create k-Nearest Neighbors list with k = k
    nb_knn <- knearneigh(coords, k = k)

    # Convert the k-neighbour list to neighbourhood list
    w_knn <- knn2nb(nb_knn)

    # Compute distances from observation to neighbours
    k.distances <- nbdists(w_knn, coords)

    # Calculate weights as the inverse of the distance
    invd_list <- lapply(k.distances, function(x) if (length(x) > 0) (1/(x/100)) else numeric(0))

    # Create the normalised spatial weights matrix
    w_matrix <- nb2listw(w_knn, glist = invd_list, style = "W")

    # Calculate Moran's Index I (analytical test)
    moran_test <- moran.test(Bias_map$bias, w_matrix)
    current_moran_abs <- abs(moran_test$estimate[["Moran I statistic"]])

    # Calculate the number of disconnected subgraphs
    num_components <- n.comp.nb(w_knn)$nc

    # Store the results in the list
    moran_results_list[[as.character(k)]] <- data.frame(
      k = k,
      Moran.I = moran_test$estimate[["Moran I statistic"]],
      p.value = moran_test$p.value,
      Subgraphs = num_components
    )

    # Updating the best k based on the combined criteria
    if (num_components < min_components) {
      min_components <- num_components
      max_moran_abs_connected <- current_moran_abs
      best_k_combined <- k
      best_w_matrix_combined <- w_matrix
    } else if (num_components == min_components && current_moran_abs > max_moran_abs_connected) {
      max_moran_abs_connected <- current_moran_abs
      best_k_combined <- k
      best_w_matrix_combined <- w_matrix
    }

    # Print the results for each k
    cat(paste("k =", k, ", Moran's I =", round(moran_test$estimate[["Moran I statistic"]], 4), ", p-value =", round(moran_test$p.value, 4), ", Subgraphs =", num_components, "\n"))
  }

  # Create the results dataframe
  moran_results_df <- do.call(rbind, moran_results_list)

  # Show he results dataframe
  moran_results_df$p.value <- sprintf("%.10f", moran_results_df$p.value)
  print("\nResults of the iterations:")
  print(moran_results_df)

  # Print the optimal value of k (combined criteria)
  cat(paste("\nThe optimal k-value (based on lower number of subgraphs and higher absolute value of Moran's I-index) is:", best_k_combined, "\n"))

  # --- Calculate and display Moran's I-index and the p-value for the combined optimal k ---
  if (!is.null(best_w_matrix_combined)) {
    best_moran_test_combined <- moran.test(Bias_map$bias, best_w_matrix_combined)
    cat(paste("Índice I de Moran para k =", best_k_combined, ":", round(best_moran_test_combined$estimate[["Moran I statistic"]], 4), "\n"))
    cat(paste("p-value para k =", best_k_combined, ":", round(best_moran_test_combined$p.value, 4), "\n"))

    # Monte Carlo test for the combined optimum k
    mc_best_k_combined <- moran.mc(Bias_map$bias, best_w_matrix_combined, nsim = 999, alternative = "two.sided")
    print(paste("\nMonte Carlo test for k =", best_k_combined, ":"))
    print(mc_best_k_combined)
    print(paste("P-value (Monte Carlo) for k =", best_k_combined, ":", mc_best_k_combined$p.value))

    # Calculate the spatial lag using the optimal weighting matrix (combined criteria)
    bias_lag_optimal_k_combined <- lag.listw(best_w_matrix_combined, Bias_map$bias, NAOK=TRUE)
    print("\nSpatial Lag of bias using the optimal weighting matrix (combined criteria):")
    print(head(bias_lag_optimal_k_combined)) # Display the first rows of the spatial lag
  } else {
    cat("No valid values for k.\n")
  }

  # --- Display of the results ---
  # Function to apply common themes and improve readability
  my_theme <- function() {
    theme_bw() +
      theme(
        plot.title = element_text(size = 28, face = "bold"),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 24)
        # legend.title = element_text(size = 14),
        # legend.text = element_text(size = 12)
      )
  }
  results_df <- do.call(rbind, lapply(moran_results_list, as.data.frame))

  img_morans <- ggplot(results_df, aes(x = k, y = Moran.I)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Moran's I-index for different values of k",
         x = "Number of neighbours (k)",
         y = "Moran Index I") +
    my_theme()

  img_pvalue <- ggplot(results_df, aes(x = k, y = p.value)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue") +
    labs(title = "P-value of the Moran I-index for different values of k",
         x = "Number of neighbours (k)",
         y = "P-value") +
    my_theme()

  img_subgraphs <- ggplot(results_df, aes(x = k, y = Subgraphs)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
    labs(title = "Number of Disconnected Subgraphs per value of k",
         x = "Number of neighbours (k)",
         y = "Number of Subgraphs") +
    my_theme() +
    ylim(floor(min(results_df$Subgraphs)) - 0.5, ceiling(max(results_df$Subgraphs)) + 0.5)

  best_k_combined
  knn_figure <- img_morans + img_pvalue + img_subgraphs

  return_structure <- list(
    best_k           = best_k_combined,
    knn_figure       = knn_figure,
    moran_results_df = moran_results_df
  )
  return(return_structure)
}
# ==============================================================================

# ==============================================================================
# =========================== F_Spatial_knn ====================================
# ==============================================================================
#' F_Spatial_knn
#'
#' Function to Compute spatial lag with knn method
#'
#' @param
#'        Bias_map  : Geographical data with bias
#'        k         : Optimal value of k
#' @return
#'        A structure with spatial lag, number of subgraphs and weight matrix
#' @examples
#'        Structure <- F_Spatial_knn(Bias_map,k)
#' @export
F_Spatial_knn  <- function(Bias_map,k)
{
  if ("bias_w" %in% colnames(Bias_map)) {
    Bias_map <- Bias_map[, !names(Bias_map) %in% "bias_w"]
  }
  suffix <- '_w_knn'
  # Extract the coordinates of the centroids (or your points)
  coords <- st_coordinates(st_centroid(Bias_map))
  # Create k-Nearest Neighbors list(using centroids of a spatial object)
  nb_knn <- knearneigh(coords, k = k)
  # Create weights matrix with equal weights
  w_knn <- knn2nb(nb_knn)
  # Subgraphs
  subgraphs <- n.comp.nb(w_knn)$nc
  # Compute distances from observation to neighbours
  k.distances <- nbdists(w_knn, coords)
  # Use these distances as weights
  invd2a <- lapply(k.distances, function(x) (1/(x/100)))
  # Create weights matrix with weights as 1/d, normalised
  w_matrix <- nb2listw(w_knn, glist = invd2a, style = "W")
  # Compute spatial lag
  bias_w <- lag.listw(w_matrix, Bias_map$bias, NAOK=TRUE)
  # Save spatial lag in additional column
  Bias_map$Bias_w <- bias_w

  cat("The number of disconnected subgraphs for the optimal k (", k, ") is: ", subgraphs, "\n")

  return_structure <- list(
    Bias_map  = Bias_map,
    subgraphs = subgraphs,
    w_matrix  = w_matrix
  )
  return(return_structure)
}
# ==============================================================================

# ==============================================================================
# =========================== F_Spatial_db =====================================
# ==============================================================================
#' F_Spatial_db
#'
#' Function to Compute spatial lag with db method
#'
#' @param
#'        Bias_map  : Geographical data with bias
#' @return
#'        A structure with spatial lag, number of subgraphs and weight matrix
#' @examples
#'        Structure <- F_Spatial_db(Bias_map)
#' @export
F_Spatial_db  <- function(Bias_map)
{
  if ("bias_w" %in% colnames(Bias_map)) {
    Bias_map <- Bias_map[, !names(Bias_map) %in% "bias_w"]
  }
  suffix <- '_w_db'
  # Extract the coordinates of the centroids (or your points)
  coords <- st_coordinates(st_centroid(Bias_map))
  # Compute distance band to ensure that all the observations have neighbours
  k1 <- knn2nb(knearneigh(coords))
  critical.threshold <- max(unlist(nbdists(k1, coords)))
  # Compute lists of neighbours
  nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
  # Subgraphs
  subgraphs <- n.comp.nb(nb.dist.band)$nc
  # Compute distances
  distances <- nbdists(nb.dist.band, coords)
  # Compute inverse distances
  invd1a <- lapply(distances, function(x) (1/(x/100)))
  # Compute spatial weights matrix, normalised
  w_matrix <- nb2listw(nb.dist.band, glist = invd1a, style = "W", zero.policy=TRUE)
  # Compute spatial lag
  bias_w <- lag.listw(w_matrix, Bias_map$bias, NAOK=TRUE)
  # Save spatial lag in additional column
  Bias_map$bias_w <- bias_w

  cat("The number of disconnected subgraphs for db is: ", subgraphs, "\n")

  return_structure <- list(
    Bias_map  = Bias_map,
    subgraphs = subgraphs,
    w_matrix  = w_matrix
  )
  return(return_structure)
}
# ==============================================================================

# ==============================================================================
# =========================== F_Moran_results ==================================
# ==============================================================================
#' F_Moran_results
#'
#' Function to Compute Moran's I, p_value & Z-score for spatial cases
#'
#' @param
#'        Bias_map  : Geographical data with bias
#'        Structure_queen
#'        Structure_fbw
#'        Structure_knn
#'        Structure_db
#' @return
#'        A dataframe with Moran values, p-values, z-score and others
#' @examples
#'        Data <- F_Moran_results(Bias_map)
#' @export
F_Moran_results  <- function(w_schemes,Structure_queen,Structure_fbw,Structure_knn,Structure_db)
{
  all_moran_results <- data.frame(
    Scheme           = character(),
    Moran_I_mc       = numeric(),
    P_value_mc       = numeric(),
    Subgraphs        = integer(),
    Moran_I_test     = numeric(),
    Expected_Moran_I = numeric(),
    Variance_Moran_I = numeric(),
    P_value_test     = numeric(),
    Z_score          = numeric(),
    stringsAsFactors = FALSE
  )
  for (w_scheme in w_schemes) {
    structure_name <- paste0("Structure_", w_scheme)
    Current_structure <- get(structure_name)
    current_Bias_map  <- Current_structure$Bias_map
    current_w_matrix  <- Current_structure$w_matrix
    current_subgraphs <- Current_structure$subgraphs

    cat(paste0("\n--- Processing scheme: ", w_scheme, " ---\n"))

    # Moran's I Monte Carlo test
    result <- moran.mc(current_Bias_map$bias, current_w_matrix, nsim = 1000, alternative = "two.sided", zero.policy = TRUE)

    # Extract Moran's I and p-value
    mI <- as.numeric(result$statistic)
    p_value_mI <- as.numeric(result$p.value)
    print(paste("P_value (moran.mc) =", p_value_mI))
    print(paste("mI (moran.mc)      =", mI))

    # Execute moran.test()
    moran_test_result <- moran.test(current_Bias_map$bias, current_w_matrix, alternative = "two.sided", zero.policy = TRUE)

    # Extracts the Z-score
    z_score_mI <- as.numeric(moran_test_result$statistic)
    expected_mI <- as.numeric(moran_test_result$estimate["Expectation"])
    variance_mI <- as.numeric(moran_test_result$estimate["Variance"])

    # Extracts Moran's I observed and p-value
    observed_mI_test <- as.numeric(moran_test_result$estimate["Moran I statistic"])
    p_value_test <- as.numeric(moran_test_result$p.value)

    # Collect the results in one row for the final data.frame
    # Create a row with the results for the current scheme
    current_row <- data.frame(
      Scheme           = w_scheme,
      Moran_I_mc       = mI,
      P_value_mc       = p_value_mI,
      Subgraphs        = as.integer(current_subgraphs),
      Moran_I_test     = observed_mI_test,
      Expected_Moran_I = expected_mI,
      Variance_Moran_I = variance_mI,
      P_value_test     = p_value_test,
      Z_score          = z_score_mI,
      stringsAsFactors = FALSE
    )
    # Add this row to the main data.frame
    all_moran_results <- rbind(all_moran_results, current_row)
  }
  cols_to_format <- c("Moran_I_mc",
                      "P_value_mc",
                      "Moran_I_test",
                      "Expected_Moran_I",
                      "Variance_Moran_I",
                      "P_value_test",
                      "Z_score")
  for (col_name in cols_to_format) {
    if (col_name %in% names(all_moran_results)) {
      # Redondea numéricamente a 10 decimales
      all_moran_results[[col_name]] <- round(all_moran_results[[col_name]], digits = 10)
    }
  }
  return(all_moran_results)
}
# ==============================================================================

# ==============================================================================
# ================================ F_Map =======================================
# ==============================================================================
#' F_Map
#'
#' Function to create maps
#'
#' @param
#'        Bias_measurement :data with bias
#'        Bias_map         :data with spatial correlation
#'        num_classes      :number of classes or partitions
#'        spatial          : 0 for nonspatial 1 for spatial
#'        mI               :Moran's Indicator
#'        mp_value_mI      :p-value
#' @return
#'        Map with bias
#' @examples
#'        Map <- F_Map(Bias_measurement,Bias_map,num_classes)
#'        Map <- F_Map(Bias_measurement,Bias_map,num_classes,1, mI, mp_value_mI)
#' @export
F_Map  <- function(Bias_measurement,Bias_map,num_classes,spatial = 0, mI = 0, p_value_mI = 0)
{
  jenks_breaks <- classIntervals(Bias_measurement$bias*100, n = num_classes, style = "jenks")$brks
  jenks_breaks <- unique(jenks_breaks)
  Bias_map$jenks_bins <- cut(Bias_map$Map_bias, jenks_breaks, include.lowest = TRUE)
  jenks_colors <- viridis(length(jenks_breaks) - 1)

  # Manually format the legend labels to avoid scientific notation
  formatted_labels <- sapply(1:(length(jenks_breaks) - 1), function(i) {
    paste0("[",
           sprintf("%.1f", jenks_breaks[i]), ", ",
           sprintf("%.1f", jenks_breaks[i + 1]), ")")
  })
  # Map
  if(spatial == 0)
  {
    Map <- ggplot(data = Bias_map) +
      geom_sf(aes(fill = jenks_bins)) +
      scale_fill_viridis_d(name = "Size of bias",
                           labels = formatted_labels,
                           na.translate = TRUE,
                           na.value = "gray") +  # Set color for NA values
      labs(title = " ") +
      theme_map_tufte() +
      theme(
        legend.text = element_text(size = 60),
        legend.title = element_text(size = 60),
        legend.position = "right"
      )
  }
  else
  {
    # Prepare the annotation for the map
    p_annotation <- ifelse(p_value_mI < 0.05, "p < 0.05", paste("p =", round(p_value_mI, 2)))
    bbox <- st_bbox(Bias_map)
    xmin <- bbox["xmin"]
    xmax <- bbox["xmax"]
    ymin <- bbox["ymin"]
    ymax <- bbox["ymax"]
    annotation <- data.frame(
      x = c(xmin + 0.5*(xmax-xmin), xmin + 0.5*(xmax-xmin)),
      y = c(ymin + 1.25*(ymax-ymin), ymin + 1.15*(ymax-ymin)),
      label = c(paste0("I = ", round(mI,2)), p_annotation)
    )
    # Build the map graphic
    Map <- ggplot(data = Bias_map) +
      geom_sf(aes(fill = jenks_bins)) +
      scale_fill_viridis_d(name = "Size of bias",
                           labels = formatted_labels,
                           na.translate = FALSE,
                           na.value = "gray") +  # Set color for NA values
      labs(title = " ") +
      geom_text(data=annotation, aes(x=x, y=y, label=label),
                color="black",
                size=23,
                angle=0,
                fontface="bold",
                family = "robotocondensed") +
      theme_map_tufte() +
      theme(
        legend.text = element_text(size = 55),
        legend.title = element_text(size = 55),
        legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
  }
  return(Map)
}
# ==============================================================================

# ==============================================================================
# ========================== F_Load_join_images ================================
# ==============================================================================
#' F_Load_join_images
#'
#' Function to load and join images
#'
#' @param
#'        path_01 : map_path
#'        path_02A: unlabelled_histogram_path
#'        path_02B: adjusted_histogram_path
#'        path_03 : scatter_path
#' @return
#'        an image with 3 plots
#' @examples
#'        Final_plot <- F_Load_join_images(path_01,path_02A,path_02B,path_03)
#' @export
F_Load_join_images  <- function(path_01,path_02A,path_02B,path_03)
{
  # LOAD IMAGES
  # ============================================================== img1 (map)
  if (file.exists(path_01)){
    img1 <- image_read(path_01)
    message("File uploaded: Map_nonspatial.png")
  } else {
    message("Map_nonspatial.png not found.")
  }
  # ============================================================== img2 (histogram)
  if (file.exists(path_02B)) {
    img2 <- image_read(path_02B)
    message("File uploaded: Adjusted_unlabelled_histogram_50.png")
  } else {
    if (file.exists(path_02A)) {
      img2 <- image_read(path_02A)
      message("File uploaded: Unlabelled_histogram.png")
    } else {
      stop("Not Adjusted_unlabelled_histogram_50.png nor Unlabelled_histogram.png were found in the path.")
    }
  }
  # =================================================================img3 (scatter)
  if (file.exists(path_03)){
    img3 <- image_read(path_03)
    message("File uploaded: Scatter.png")
  } else {
    message("Scatter.png not found.")
  }
  # ================================================================= JOIN
  # Join images
  return(image_append(c(img1, img2, img3)))
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
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(viridis)
  library(spdep)
  library(sysfonts)
  library(showtextdb)
  library(classInt)
  library(scales)
  library(forcats)
  library(spgwr)
  library(ggthemes)
  library(tmap)
  library(tidyverse)
  library(readr)
  library(magrittr)
  library(gridExtra)
  library(magick)
  library(patchwork)
  library(showtext)
  library(sysfonts)
}
# ==============================================================================
