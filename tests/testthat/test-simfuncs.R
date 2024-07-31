test_that("all values are positive", {
  # test the function that transitions the edge pixels doesn't leave any
  # negative (edge) values

  # Create a sample raster using the terra package for demonstration
  set.seed(135799)
  r <- terra::rast(ncol=10, nrow=10)
  terra::values(r) <- sample(1:4, terra::ncell(r), replace=TRUE)

  # Create a confusion matrix with transition probabilities
  confusion_matrix <- matrix(
    c(0.1, 0.2, 0.3, 0.4,  # Probabilities for transition from -1
      0.3, 0.2, 0.4, 0.1,  # Probabilities for transition from -2
      0.25, 0.25, 0.25, 0.25,  # Probabilities for transition from -3
      0.4, 0.3, 0.2, 0.1),  # Probabilities for transition from -4
    nrow = 4,
    byrow = TRUE
  )

  # Assign row and column names
  rownames(confusion_matrix) <- c("-1", "-2", "-3", "-4")
  colnames(confusion_matrix) <- c("1", "2", "3", "4")

  # Convert to a data frame for better readability
  transition_matrix <- as.data.frame(confusion_matrix)

  # Call the function with the raster and the edge mapping
  tagged_raster <- tag_edges(r, edge_depth = 1)

  # Apply the transition probabilities
  updated_pixel_values <- transition_pixels(values(tagged_raster), transition_matrix)

  # Update the raster with new pixel values
  values(tagged_raster) <- updated_pixel_values

  # expect that no values in the tagged raster are negative
  expect_true(all(values(tagged_raster) > 0))
})

test_that("probability is deterministic", {
  # test that the simulated rasters have the correct number cells in a class.

  # Create an empty raster with dimensions 100 by 100
  r <- terra::rast(nrows=100, ncols=100, xmin=0, xmax=100, ymin=0, ymax=100)

  # Initialize all cells with class 1
  values(r) <- 1

  # Define coordinates for the two 30x30 squares of class 2
  # First square (top-left corner at (10, 10))
  for (i in 10:39) {  # 10 to 39 makes a 30x30 square
    for (j in 10:39) {
      r[i, j] <- 2
    }
  }

  # Second square (top-left corner at (50, 50))
  for (i in 50:79) {  # 50 to 79 makes a 30x30 square
    for (j in 50:79) {
      r[i, j] <- 2
    }
  }

  confusion_matrix <- matrix(
    c(1, 0,  # Probabilities for transition from -1
      0.5,0.5),  # Probabilities for transition from -2
    nrow = 2,
    byrow = TRUE
  )
  # Assign row and column names
  rownames(confusion_matrix) <- c("-1", "-2")
  colnames(confusion_matrix) <- c("1", "2")

  # Convert to a data frame for better readability
  transition_matrix <- as.data.frame(confusion_matrix)

  simple_rasters <- simulate_raster_changes(r, n_simulations = 5, confusion_matrix = transition_matrix)
  simple_counts <- freq(simple_rasters)
  class <- simple_counts[simple_counts$value == 2, ]

  # vector of lengths
  expected_counts <- rep(1684,times =5)

  # expect that no values in the tagged raster are negative
  expect_equal(class$count, expected_counts)
})

