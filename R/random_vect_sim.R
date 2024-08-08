#' Function to identify and tag edges using the terra package
#'
#' @param raster The SpatRaster object representing the USDA's Crop Land Data
#'   Layer.
#' @param edge_depth The number of cells away from the patch edge to be
#'   considered a core cell.
#' @returns A raster where negative numbers represent the edge pixels.
#' @import landscapemetrics
#' @export
tag_edges <- function(raster, edge_depth = 1) {

  # Identify core areas, setting classes as all values that are not 0
  core_areas <- show_cores(raster, edge_depth = edge_depth, class = unique(values(raster)[!is.na(values(raster)) & values(raster) != 0]))

  # Convert core_areas data to a data frame and filter for classes not equal to 0
  core_data <- as.data.frame(core_areas$layer_1$data)

  # Initialize a new raster to tag edges and cores
  tagged_raster <- raster

  # Get cell indices for the edge coordinates
  edge_coords <- core_data[core_data$values == 0, c("x", "y")]
  edge_cells <- terra::cellFromXY(raster, edge_coords)

  # Retrieve the original values for edge cells
  original_values <- terra::values(raster)[edge_cells]

  # Tag edges with the negative version of their original values
  terra::values(tagged_raster)[edge_cells] <- -original_values

  return(tagged_raster)
}



# Function to transition pixel values
transition_pixels <- function(pixel_values, confusion_matrix) {
  unique_values <- as.numeric(rownames(confusion_matrix))

  new_pixel_values <- pixel_values

  for (current_class in unique_values) {
    current_class_indices <- which(pixel_values == current_class)
    n <- length(current_class_indices)

    if (n > 0) {
      transition_probs <- confusion_matrix[as.character(current_class), ]

      new_classes <- sample(
        x = as.numeric(colnames(confusion_matrix)),
        size = n,
        replace = TRUE,
        prob = as.numeric(transition_probs)
      )

      new_pixel_values[current_class_indices] <- new_classes
    }
  }

  return(new_pixel_values)
}

# Function to process a single raster layer
process_single_layer <- function(layer, confusion_matrix) {
  new_values <- transition_pixels(values(layer), confusion_matrix)
  values(layer) <- new_values
  return(layer)
}

# Function to tag edges and apply transitions
tag_and_transition <- function(input_raster, edge_depth, confusion_matrix) {
  tagged_raster <- tag_edges(input_raster, edge_depth = edge_depth)
  updated_pixel_values <- transition_pixels(values(tagged_raster), confusion_matrix)
  values(tagged_raster) <- updated_pixel_values
  return(tagged_raster)
}

# Function to simulate raster changes over multiple simulations
simulate_raster_R <- function(input_raster, edge_depth = 1, n_simulations = 100, confusion_matrix) {
  # Initialize a list to store the simulated rasters
  simulated_rasters <- vector("list", n_simulations)

  for (i in 1:n_simulations) {
    # Set a different seed for each simulation to ensure randomness
    set.seed(Sys.time() + i)
    simulated_raster <- tag_and_transition(input_raster, edge_depth, confusion_matrix)
    simulated_rasters[[i]] <- simulated_raster
  }

  # Combine the simulated rasters into a single SpatRaster
  stacked_rasters <- rast(simulated_rasters)
  return(stacked_rasters)
}
