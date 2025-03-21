#' Function to resize patches in a SpatRaster using the terra and landscapemetrics packages
#'
#' @param raster The SpatRaster object representing the categorical data.
#' @returns A SpatRaster where each patch is defined as a unique value and each
#'    class has its own layer in the spatraster.
#' @import landscapemetrics
#' @export
find_patches <- function(raster) {

  # Get patches using landscapemetrics::get_patches
  patches_list <- landscapemetrics::get_patches(raster)
  result_rast <- terra::rast(patches_list[[1]])

  # name the layers based on their associated class
  # Extract numeric value
  og_names <- names(result_rast)
  number <- as.numeric(gsub("class_", "", og_names))
  class_names <- c(number)
  names(result_rast) <- class_names

  return(result_rast)

}

# update this function to allow transition matrices that don't have col names
# i.e. the class or col name would then be = (column index - 1)

#' Function to generate transition vectors for negative cell values
#'
#' @param r The SpatRaster object from get_patches with one layer for each
#'    class.
#' @param transition_matrix The transition matrix you want to use.
#' @param iterations The number of iterations desired.
#' @returns A matrix of vector for each patch in each layer.
generate_transition_vectors <- function(r, transition_matrix, iterations = 10) {
  # Initialize a list to store the transition vectors for each unique patch ID
  simulated_values <- list()

  # Extract unique non-NA values for each layer and store in a list
  unique_values_list <- lapply(1:terra::nlyr(r), function(i) {
    as.vector(na.omit(unique(terra::values(r[[i]]))))
  })

  # Set the names of the list elements to match the layer names
  names(unique_values_list) <- names(r)

  # Loop through each list element in unique_values_list
  for (layer_name in names(unique_values_list)) {
    neg_values <- unique_values_list[[layer_name]]

    # Initialize an empty matrix to store the transition vectors for this layer
    layer_matrix <- matrix(nrow = length(neg_values), ncol = iterations)
    rownames(layer_matrix) <- neg_values

    # Loop through each unique value in the current list element
    for (i in seq_along(neg_values)) {
      neg_val <- neg_values[i]

      # Get the transition probabilities for the current layer name from confusion_matrix
      probabilities <- transition_matrix[which(rownames(transition_matrix) == layer_name), ]

      #Get col names to sample from transition matrix
      col_names <- as.numeric(colnames(transition_matrix))

      # Generate the transition vector by sampling with replacement
      new_values <- sample(col_names, size = iterations, replace = TRUE, prob = probabilities)

      # Store the transition vector in the corresponding row of the matrix
      layer_matrix[i, ] <- new_values
    }

    # Store the matrix in the list, with the layer name as the key
    simulated_values[[layer_name]] <- layer_matrix
  }

  return(simulated_values)
}

# might need to change this function since we will alwyays have col/row names

#' Function to modify transition matrix based on user input.
#'
#' @param mat The transition matrix the you want to update.
#' @param indices The vector of classes you want to remain unsimulated.
#' @returns A transition matrix modified to not transition some classes.
modify_matrix <- function(mat, indices = 0) {
  # Check if row and column names exist
  if (!is.null(rownames(mat)) && !is.null(colnames(mat)))  {

    # makes sure the input classes are characters
    indices = as.character(indices)

    # Create a copy of the matrix
    modified_mat <- mat

    # Loop through each name
    for (index in indices) {
      # Find the corresponding row and column
      row_idx <- which(rownames(mat) == index)
      col_idx <- which(colnames(mat) == index)

      # Set the diagonal element to 1
      modified_mat[row_idx, col_idx] <- 1

      # Set other elements in the row and column to 0
      modified_mat[row_idx, -col_idx] <- 0
      modified_mat[-row_idx, col_idx] <- 0
    }


  }else{
    # Create a copy of the input matrix
    modified_mat <- mat

    # Loop through each index in the vector
    for (index in indices) {
      # Set the main diagonal value to 1 for the specified index
      modified_mat[(index+1), (index+1)] <- 1

      # Set the corresponding row and column values to 0, except the diagonal
      modified_mat[(index+1), -(index+1)] <- 0
      modified_mat[-(index+1), (index+1)] <- 0
    }


  }

  return(modified_mat)
}

#' Function to resize patches in a SpatRaster using the terra and landscapemetrics packages
#'
#' @param patched_raster The SpatRaster object representing the categorical data.
#' @param og_raster The values of the classes of interest, default is "all".
#' @returns A SpatRaster where each patch is defined as a unique value and each
#'    class has its own layer in the spatraster.
collapse_and_combined <- function(patched_raster, og_raster) {

  # collapse the patched raster to the first cell value that is not NA
  collapsed_layer <- terra::app(patched_raster, fun = function(x) {
    return(x[which(!is.na(x))[1]])
  })

  # make two layer raster with og and collapsed layers
  combo <- c(og_raster, collapsed_layer)

  return(combo)
}

#' Function to simulate tagged patch values in a single layer spatraster.
#'
#' @param original_raster The SpatRaster object representing the categorical data.
#' @param transition_matrix Transition matrix values that define transitions
#'    based on class values.
#' @param iterations The number of simulations to be performed.
#' @param non_trans Class(es) that the user would like to remain unsimulated.
#'    Defualt is class 0 since this is the background class.
#' @returns A SpatRaster where the negative value patches are set to transition.
#' @import landscapemetrics
#' @export
simulate_raster_patch <- function(original_raster,
                                  transition_matrix,
                                  non_trans = 0,
                                  iterations = 10) {

  # Ensure objects are the correct class
  transition_matrix <- as.matrix(transition_matrix)
  iters <- as.integer(iterations)

  # Update any classes that are set to not transition (still need to check with simulation)
  transition_matrix <- modify_matrix(mat = transition_matrix,
                                     indices = non_trans)

  # Find the patches in the og layer
  patched_raster <- find_patches(original_raster)

  # generate transition vectors
  vecs <- generate_transition_vectors(r = patched_raster, transition_matrix = transition_matrix, iterations = iters)

  # Collapse the patched raster to a single layer and combined with input
  tagged_raster <- collapse_and_combined(patched_raster, og_raster = original_raster)

  # Initialize the simulation with the original raster
  current_raster <- original_raster

  # create the new simulated raster
  current_raster <- terra::app(tagged_raster, fun = transition_patches_random,
                               trans_vecs = vecs,
                               iterations = iters)

  return(current_raster)

}
