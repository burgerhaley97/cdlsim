# Create an empty raster with dimensions 100 by 100
r <- rast(nrows=100, ncols=100, xmin=0, xmax=100, ymin=0, ymax=100)

# Initialize all cells with class 1
values(r) <- 1

# Define coordinates for the first 30x30 square of class 2
# First square (top-left corner at (10, 10))
for (i in 10:39) {  # 10 to 39 makes a 30x30 square
  for (j in 10:39) {
    r[i, j] <- 2
  }
}

# Define coordinates for the second 30x30 square of class 3
# Second square (top-left corner at (50, 50))
for (i in 50:79) {  # 50 to 79 makes a 30x30 square
  for (j in 50:79) {
    r[i, j] <- 3
  }
}

plot(r)

library(terra)

# Create an empty raster with dimensions 100 by 100
r <- rast(nrows=100, ncols=100, xmin=0, xmax=100, ymin=0, ymax=100)

# Initialize all cells with class 1
values(r) <- 1

# Define coordinates for the first 30x30 square of class 2
# First square (top-left corner at (10, 10))
for (i in 10:39) {  # 10 to 39 makes a 30x30 square
  for (j in 10:39) {
    r[i, j] <- 2
  }
}

# Define coordinates for the second 30x30 square of class 3
# Second square (top-left corner at (50, 50))
for (i in 50:79) {  # 50 to 79 makes a 30x30 square
  for (j in 50:79) {
    r[i, j] <- 3
  }
}

# Add three small patches for class 2
# First small patch of class 2 at (20, 20), 5x5
for (i in 20:24) {
  for (j in 20:24) {
    r[i, j] <- 2
  }
}

# Second small patch of class 2 at (60, 10), 5x5
for (i in 60:64) {
  for (j in 10:14) {
    r[i, j] <- 2
  }
}

# Third small patch of class 2 at (80, 80), 5x5
for (i in 80:84) {
  for (j in 80:84) {
    r[i, j] <- 2
  }
}

# Add three small patches for class 3
# First small patch of class 3 at (30, 70), 5x5
for (i in 30:34) {
  for (j in 70:74) {
    r[i, j] <- 3
  }
}

# Second small patch of class 3 at (70, 30), 5x5
for (i in 70:74) {
  for (j in 30:34) {
    r[i, j] <- 3
  }
}

# Third small patch of class 3 at (90, 10), 5x5
for (i in 90:94) {
  for (j in 10:14) {
    r[i, j] <- 3
  }
}

# Plot the updated raster
plot(r)



patches <- landscapemetrics::get_patches(r)
length(values((patches$layer_1$class_2)))
unique(values(!is.na(patches$layer_1$class_2)))
plot(patches$layer_1$class_2)
plot(patches$layer_1$class_3)
length(unique(values(patches$layer_1$class_3)))

#' Function to resize patches in a SpatRaster using the terra and landscapemetrics packages
#'
#' @param raster The SpatRaster object representing the categorical data.
#' @param class_values The values of the classes you want to resize patches for; default is all unique classes.
#' @returns A 2-layer SpatRaster where the second layer contains resized patches.
#' @import landscapemetrics
#' @export
resize_patches_2layer <- function(raster, class_values = NULL) {

  # If no class values are provided, use all unique non-NA, non-zero values
  if (is.null(class_values)) {
    class_values <- unique(values(raster)[!is.na(values(raster)) & values(raster) != 0])
  }

  # Get patches using landscapemetrics
  patches <- landscapemetrics::get_patches(raster, class = class_values)

  # Initialize a new raster for resized patches, starting with NA values
  resized_raster <- rast(raster)
  values(resized_raster) <- NA

  # Loop over each class to extract and resize patches
  for (class_id in class_values) {
    layer_name <- paste0("layer_1$class_", class_id)

    # Extract the raster layers for the current class
    class_raster <- eval(parse(text = paste0("patches$", layer_name)))
    print(class_raster)

    class_total <- length(which(!is.na(unique(values(class_raster)))))

    # Loop over each patch in the class raster
    for (patch_idx in 1:length(class_total)) {
      patch_layer <- class_raster[[patch_idx]]

      # Identify the cells that belong to this patch
      patch_cells <- which(!is.na(values(patch_layer)))

      # Determine the current size of the patch
      current_size <- length(patch_cells)

      # Calculate the 5% increase or decrease in the patch size
      adjustment_size <- round(current_size * 0.05)

      # Randomly decide whether to increase or decrease the patch size
      if (runif(1) > 0.5) {
        # Increase patch size
        for (k in 1:adjustment_size) {
          # Find neighboring cells that are NA in the resized raster
          neighbors <- terra::adjacent(resized_raster, patch_cells, directions = 8, pairs = FALSE)
          valid_neighbors <- neighbors[is.na(values(resized_raster)[neighbors])]

          if (length(valid_neighbors) > 0) {
            # Randomly select one neighbor cell to include in the patch
            selected_cell <- sample(valid_neighbors, 1)
            values(resized_raster)[selected_cell] <- class_id
          }
        }
      } else {
        # Decrease patch size
        if (current_size > adjustment_size) {
          # Randomly remove the specified number of cells from the patch
          cells_to_remove <- sample(patch_cells, adjustment_size)
          values(resized_raster)[cells_to_remove] <- NA
        }
      }

      # Assign the remaining original patch cells to the resized raster
      patch_cells <- which(!is.na(values(patch_layer)))
      values(resized_raster)[patch_cells] <- class_id
    }
  }

  # Combine the original and resized rasters into a two-layer SpatRaster
  combined_raster <- c(raster, resized_raster)

  return(combined_raster)
}

test_tag <- resize_patches_2layer(r)

plot(test_tag)


tag_patches_2layer <- function(raster, class_values = NULL, tag_percentage = 33) {

  # If no class values are provided, use all unique non-NA, non-zero values
  if (is.null(class_values)) {
    class_values <- unique(values(raster)[!is.na(values(raster)) & values(raster) != 0])
  }

  # Get patches using landscapemetrics
  patches <- landscapemetrics::get_patches(raster, class = class_values)

  # Initialize a new raster to tag patches, starting with NA values
  tagged_raster <- rast(raster)
  values(tagged_raster) <- NA

  # Loop over each class to extract and tag patches
  for (class_id in class_values) {
    layer_name <- paste0("layer_1$class_", class_id)

    # Extract the raster for the current class
    class_raster <- eval(parse(text = paste0("patches$", layer_name)))

    # Calculate the number of patches to tag based on the percentage
    num_patches <- length(class_raster)
    num_to_tag <- ceiling(num_patches * (tag_percentage / 100))

    # Randomly select patches to tag
    selected_patches <- sample(1:num_patches, num_to_tag)

    # Loop over each selected patch and tag with negative values
    for (patch in selected_patches) {
      patch_layer <- class_raster[[patch]]

      # Find cell indices where the patch exists
      patch_cells <- which(!is.na(values(patch_layer)))

      # Tag these cells in the tagged raster with negative of the class value
      values(tagged_raster)[patch_cells] <- -class_id
    }
  }

  # Combine the original and tagged rasters into a two-layer SpatRaster
  combined_raster <- c(raster, tagged_raster)

  return(combined_raster)
}

#' Function to resize patches in a SpatRaster using the terra and landscapemetrics packages
#'
#' @param raster The SpatRaster object representing the categorical data.
#' @param class_values The values of the classes you want to resize patches for; default is all unique classes.
#' @param tag_percentage The percentage of patches in each class to tag.
#' @returns A SpatRaster where the negative value patches are set to transition.
#' @import landscapemetrics
#' @export
tag_patches_2layer <- function(raster, class_values = NULL, tag_percentage = 20) {

  # Use provided class values or calculate them if NULL
  if (is.null(class_values)) {
    class_values <- unique(values(raster)[!is.na(values(raster)) & values(raster) != 0])
  }

  # Get patches using landscapemetrics::get_patches
  patches_list <- landscapemetrics::get_patches(raster, class = class_values)

  # Initialize a new raster to hold the results, copying the original values
  result_raster <- rast(raster)
  values(result_raster) <- values(raster)

  # Loop over each class in the patches_list
  for (class_index in seq_along(patches_list[[1]])) {

    # Extract the SpatRaster for the current class
    class_patches <- patches_list[[1]][[class_index]]

    # Identify all unique patch values in this class layer, excluding NA
    unique_patch_values <- unique(terra::values(class_patches)[!is.na(values(class_patches))])

    # Calculate number of patches to tag based on the percentage
    num_patches <- length(unique_patch_values)
    num_to_tag <- floor(num_patches * (tag_percentage / 100))
    print(num_to_tag)

    # Randomly select the specified percentage of unique patch values
    selected_patch_values <- sample(unique_patch_values, num_to_tag)

    # Loop over each selected patch value and tag cells with the negative class index
    for (patch_value in selected_patch_values) {
      # Identify all cells belonging to this selected patch value
      patch_cells <- which(values(class_patches) == patch_value)

      # Tag these cells in the result raster with the negative class index
      terra::values(result_raster)[patch_cells] <- -class_index
    }
  }

  return(result_raster)
}

