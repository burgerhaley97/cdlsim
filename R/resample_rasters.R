#' Function to identify and tag edges using the terra package
#'
#' @param raster The SpatRaster object representing the USDA's Crop Land Data
#'   Layer.
#' @param edge_depth The number of cells away from the patch edge to be
#'   considered a core cell.
#' @returns A raster where negative numbers represent the edge pixels.
#' @import terra
#' @import landscapemetrics
#' @export
tag_edges <- function(raster, edge_depth = 1) {

  # Identify core areas, setting classes as all values that are not 0
  core_areas <- show_cores(raster, edge_depth = edge_depth, class = c(1,2))
#unique(values(raster)[!is.na(values(raster)) & values(raster) != 0])
  # Convert core_areas data to a data frame and filter for classes not equal to 0
  core_data <- as.data.frame(core_areas$layer_1$data)

  # Initialize a new raster to tag edges and cores
  tagged_raster <- raster

  # Get cell indices for the edge coordinates
  edge_coords <- core_data[core_data$values == 0, c("x", "y")]
  edge_cells <- cellFromXY(raster, edge_coords)

  # Retrieve the original values for edge cells
  original_values <- values(raster)[edge_cells]

  # Tag edges with the negative version of their original values
  values(tagged_raster)[edge_cells] <- -original_values

  return(tagged_raster)
}


#' Function to transition values of "edge" pixels based on confusion matrices
#'
#' @param pixel_values The pixel values from the tagged SpatRaster.
#' @param confusion_matrix A confusion matrix created from get_trans_mat() that
#'   represents the transition probabilities.
#' @returns New pixel values for the edge pixels.
transition_pixels <- function(pixel_values, confusion_matrix) {
  # Filter out NA values from pixel_values
  valid_indices <- !is.na(pixel_values)
  pixel_values <- pixel_values[valid_indices]

  unique_values <- as.numeric(rownames(confusion_matrix))

  # Create a vector to store updated pixel values
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

# Vectorized function to transition values of pixels based on confusion matrices.
transition_pixels <- function(pixel_values, confusion_matrix) {
  # Flatten pixel_values to ensure it's a vector
  pixel_values <- as.vector(pixel_values)

  # Create a vector to store updated pixel values
  new_pixel_values <- pixel_values

  # Get the unique classes and their frequencies
  unique_classes <- unique(pixel_values)
  class_counts <- table(pixel_values)

  # Generate new pixel values for each class
  for (class in unique_classes) {
    # Skip classes that do not have transition probabilities defined
    if (!as.character(class) %in% rownames(confusion_matrix)) {
      next
    }

    n <- class_counts[as.character(class)]
    transition_probs <- confusion_matrix[as.character(class), ]

    # Check for NA values in transition probabilities and handle them
    if (any(is.na(transition_probs))) {
      stop("Error: NA values found in transition probabilities for class ", class)
    }

    # Calculate expected counts based on probabilities and round down
    expected_counts <- floor(n * transition_probs)

    # Adjust counts to ensure they sum to n
    difference <- n - sum(expected_counts)
    if (!is.na(difference) && difference > 0) {
      adjustment_indices <- sample(seq_along(expected_counts), difference, replace = TRUE)
      expected_counts[adjustment_indices] <- expected_counts[adjustment_indices] + 1
    }

    # Create a vector of new classes based on expected counts
    new_classes <- rep(as.numeric(colnames(confusion_matrix)), expected_counts)

    # Shuffle the new classes to ensure randomness
    new_classes <- sample(new_classes)

    # Assign the new classes to the appropriate pixels
    new_pixel_values[pixel_values == class] <- new_classes
  }

  return(new_pixel_values)
}

#' Function to help process SpatRasters by layer
#'
#' @param layer The pixel values from each layer of the tagged SpatRaster.
#' @param confusion_matrix A confusion matrix created from get_trans_mat() that
#'   represents the transition probabilities.
#' @returns New pixel values for the edge pixels.
process_single_layer <- function(layer, confusion_matrix) {
  new_values <- transition_pixels(values(layer), confusion_matrix)
  values(layer) <- new_values
  return(layer)
}

#' Function to tag edges and apply transitions.
#'
#' @param input_raster Original reclassified raster.
#' @param confusion_matrix A confusion matrix created from get_trans_mat() that
#'   represents the transition probabilities.
#' @param edge_depth The number of cells away from the patch edge to be
#'   considered a core cell.
tag_and_transition <- function(input_raster, edge_depth, confusion_matrix) {
  tagged_raster <- tag_edges(input_raster, edge_depth = edge_depth)
  updated_pixel_values <- transition_pixels(values(tagged_raster), confusion_matrix)
  values(tagged_raster) <- updated_pixel_values
  return(tagged_raster)
}

#' Function to simulate raster pixels on edges
#'
#' @param input_raster Original reclassified raster.
#' @param edge_depth The number of cells away from the patch edge to be
#'   considered a core cell.
#' @param n_simulations The number of simulation you want to run.
#' @param confusion_matrix A confusion matrix created from get_trans_mat() that
#'   represents the transition probabilities.
#' @returns The simulated CDL SpatRaster objects.
#' @export
simulate_raster_changes <- function(input_raster, edge_depth = 1, n_simulations = 10, confusion_matrix) {
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
