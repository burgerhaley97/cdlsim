#' Function to resize patches in a SpatRaster using the terra and landscapemetrics packages
#'
#' @param raster The SpatRaster object representing the categorical data.
#' @param class_values The values of the classes you want to resize patches for; default is all unique classes.
#' @param tag_percentage The percentage of patches in each class to tag.
#' @returns A SpatRaster where the negative value patches are set to transition.
#' @import landscapemetrics
#' @export
#' tag_patches_2layer <- function(raster, class_values = NULL, tag_percentage = 25) {
#'
#'   # Use provided class values or calculate them if NULL
if (is.null(class_values)) {
  #'     class_values <- unique(terra::values(raster)[!is.na(terra::values(raster)) & terra::values(raster) != 0])
  #'   }
  #'
  #'   # Get patches using landscapemetrics::get_patches
  #'   patches_list <- landscapemetrics::get_patches(raster, class = class_values)
  #'
  #'   # Initialize a new raster to hold the results, copying the original values
  #'   result_raster <- terra::rast(raster)
  #'   terra::values(result_raster) <- terra::values(raster)
  #'
  #'   # Loop over each class in the patches_list
  #'   for (class_index in seq_along(patches_list[[1]])) {
  #'
  #'     # Extract the SpatRaster for the current class
  #'     class_patches <- patches_list[[1]][[class_index]]
  #'
  #'     # Identify all unique patch values in this class layer, excluding NA
  #'     unique_patch_values <- unique(terra::values(class_patches)[!is.na(terra::values(class_patches))])
  #'
  #'     # Calculate number of patches to tag based on the percentage
  #'     num_patches <- length(unique_patch_values)
  #'     num_to_tag <- floor(num_patches * (tag_percentage / 100))
  #'
  #'     # Randomly select the specified percentage of unique patch values
  #'     selected_patch_values <- sample(unique_patch_values, num_to_tag)
  #'
  #'     # Loop over each selected patch value and tag cells with the negative class index
  #'     for (patch_value in selected_patch_values) {
  #'       # Identify all cells belonging to this selected patch value
  #'       patch_cells <- which(values(class_patches) == patch_value)
  #'
  #'       # Tag these cells in the result raster with the negative class index
  #'       terra::values(result_raster)[patch_cells] <- -class_index
  #'     }
  #'   }
  #'
  #'   return(c(raster, result_raster))
  #' }
