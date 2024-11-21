# Random simulation using terra app function

### outline
# A function that takes in the original raster and tags the edges, output should be spateraster
# this will be the already existing tag edges function, with the addition of combining
# the original raster and tagged raster into a two layer spat raster.

#' Function to identify and tag edges using the terra package
#'
#' @param raster The SpatRaster object representing the USDA's Cropland Data
#'   Layer.
#' @param edge_depth The number of cells away from the patch edge to be
#'   considered a core cell.
#' @returns A  2 layers raster where negative numbers represent the edge pixels.
#' @import landscapemetrics
#' @export
tag_edges_2layer <- function(raster, edge_depth = 1) {

  # Identify core areas, setting classes as all values that are not 0
  core_areas <- landscapemetrics::show_cores(raster, edge_depth = edge_depth)

  # class = unique(values(raster)[!is.na(values(raster)) & values(raster) != 0]

  # Convert core_areas data to a data frame
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

  return(c(raster, tagged_raster))
}


# A function that takes in a vector of length two (a pixel value from each layer)
# and if both pixels are positive do nothing else transition the first value of the second
# element to a new value based on the transition matrix provided.

# Transition a vector of length one based on its sign
transition_function_terra <- function(cell_values, transition_matrix) {
  # Check if both cell values are positive
  if (all(cell_values > 0)) {
    return(cell_values[1])  # Return the first cell value unchanged
  } else {
    # Get the row name corresponding to the second element's value
    row_name <- as.character(cell_values[2])

    # Use the row name to get the probabilities from the transition matrix
    probabilities <- transition_matrix[row_name, ]

    # Sample a new value based on the probabilities in the transition matrix
    new_value <- sample(as.numeric(colnames(transition_matrix)), size = 1, prob = probabilities)
    return(new_value)
  }
}


# Define the simulation function with layered output
simulate_raster_terra <- function(original_raster, transition_matrix, iterations = 10, edge_depth = 1) {

  # tag the original raster
  tagged_raster <- tag_edges_2layer(original_raster, edge_depth)

  # Create a list to hold each iteration's raster
  layers <- vector("list", iterations)

  # Initialize the simulation with the original raster
  current_raster <- original_raster

  # Run the simulation for the specified number of iterations
  for (i in 1:iterations) {
    # Apply the transition function to each vector of length two
    current_raster <- terra::app(tagged_raster,
                          fun = function(x) transition_function_terra(x, transition_matrix))

    # Store the current state of the raster as a layer
    layers[[i]] <- current_raster
  }

  # Combine all layers into a single SpatRaster object
  result_raster <- terra::rast(layers)

  return(result_raster)
}

# simulate using the terra app approach but also use the Rcpp transition_function
simulate_raster_rcpp <- function(original_raster, transition_matrix, iterations = 10, edge_depth = 1,
                                 background_value = 0, background_trans = FALSE) {

  # Ensure transition_matrix is a matrix
  transition_matrix <- as.matrix(transition_matrix)

  if (!background_trans) {
    if (background_value != 0) {
      # Find the position of background_value in the matrix
      row_idx <- which(as.integer(rownames(transition_matrix)) == background_value)
      col_idx <- which(as.integer(colnames(transition_matrix)) == background_value)

      if (length(row_idx) > 0 && length(col_idx) > 0) {
        # Insert a row of 1 at the position of background_value
        new_row <- rep(0, ncol(transition_matrix) + 1)
        new_row[1] <- 1
        transition_matrix <- rbind(transition_matrix[1:(row_idx - 1), ], new_row, transition_matrix[row_idx:nrow(transition_matrix), ])

        # Insert a column of 0s at the position of background_value
        new_col <- rep(0, nrow(transition_matrix))
        transition_matrix <- cbind(transition_matrix[, 1:(col_idx - 1)], new_col, transition_matrix[, col_idx:ncol(transition_matrix)])

        # Set the row and column names
        rownames(transition_matrix) <- c(rownames(transition_matrix)[1:(row_idx - 1)], background_value, rownames(transition_matrix)[row_idx:(nrow(transition_matrix) - 1)])
        colnames(transition_matrix) <- c(colnames(transition_matrix)[1:(col_idx - 1)], background_value, colnames(transition_matrix)[col_idx:(ncol(transition_matrix) - 1)])
      }
    } else {
      # If background_value is 0, retain current behavior
      transition_matrix <- rbind(0, transition_matrix)
      transition_matrix <- cbind(0, transition_matrix)

      rownames(transition_matrix) <- c(0, rownames(transition_matrix)[-1])
      colnames(transition_matrix) <- c(0, colnames(transition_matrix)[-1])
    }
  }

  # Tag the original raster
  tagged_raster <- tag_edges_2layer(original_raster, edge_depth)

  # Extract row and column names from the transition matrix
  # Define vars for use in the app function
  row_names <- as.integer(rownames(transition_matrix))
  col_names <- as.integer(colnames(transition_matrix))
  iterations <- as.integer(iterations)


  # Initialize the simulation with the original raster
  current_raster <- original_raster

  # create the raster with correct number of iterations directly
  current_raster <- terra::app(tagged_raster,fun = transition_function,
                                   transition_matrix = transition_matrix,
                                    row_names = row_names,
                                   col_names= col_names,
                                   iterations = iterations)

  return(current_raster)
}

