test_that("sim diversity does not exceed max of input raster", {
  # test that in the case where the input raster is at max diversity that the
  # the simulation's max diversity doesn't exceed the input.

  #### Make the large max diversity raster
  # Set parameters
  n_classes <- 3        # Number of classes
  square_size <- 30     # Each square is 30x30 pixels
  n_squares <- 9        # 9x9 grid of squares

  # Calculate raster dimensions
  ncol <- square_size * n_squares   # Width to fit 9 squares across
  nrow <- square_size * n_squares   # Height to fit 9 squares down

  # Create an empty raster
  rbig <- terra::rast(ncol = ncol, nrow = nrow)
  terra::values(rbig) <- NA  # Initialize with NA values

  # Assign alternating class values to each square in a checkerboard pattern
  for (i in 1:n_squares) {
    for (j in 1:n_squares) {
      # Define square boundaries
      row_start <- (i - 1) * square_size + 1
      row_end <- i * square_size
      col_start <- (j - 1) * square_size + 1
      col_end <- j * square_size

      # Calculate class value based on row and column position, alternating classes
      class_value <- ((i + j - 2) %% n_classes) + 1  # Alternates classes 1, 2, and 3 in a checkerboard pattern

      # Assign values to the defined square region
      rbig[row_start:row_end, col_start:col_end] <- class_value
    }
  }

  # Define confusion matrix, background not transitioning
  conf_mat <- matrix(1/3, nrow = 3, ncol = 3)

  # Assign row and column names
  rownames(conf_mat) <- c( "1", "2", "3")
  colnames(conf_mat) <- c( "1", "2", "3")

  # create 5 simulations with class not transitioning
  div_sim <- simulate_raster_patch(original_raster = rbig,
                                     transition_matrix = conf_mat,
                                     iterations = 10)

  # calculate Shannon's diversity using the landscapemetrics package
  shdi_sim <- landscapemetrics::lsm_l_sidi(div_sim)
  interval <- quantile(shdi_sim$value, c(0.025, 0.5, 0.975))
  shdi_sim_int <- dplyr::as_tibble(interval) |>
    dplyr::mutate(quantile = c(2.5, 50, 97.5)) |>
    dplyr::filter(quantile == 97.5) |>
    dplyr::pull(value)

  shdi_og <- landscapemetrics::lsm_l_shdi(rbig) |>
    dplyr::pull(value)

  # expect that the og div value is always bigger than the max sim value
  expect_true(shdi_og > shdi_sim_int)
})

test_that("Non-transitions maintain pixel count", {
  # test that the simulated rasters have the correct number cells in a class.

  #### Make the smaller max diversity raster
  # Define raster dimensions and classes
  ncol <- 90
  nrow <- 90
  classes <- 3

  # Initialize a SpatRaster with the desired dimensions
  r <- terra::rast(ncol = ncol, nrow = nrow, vals = NA)

  # Assign values in 30x30 blocks for each class
  r[1:30, 1:30] <- 1  # Top-left 30x30 block
  r[1:30, 31:60] <- 2  # Top-center 30x30 block
  r[1:30, 61:90] <- 3  # Top-right 30x30 block

  r[31:60, 1:30] <- 2  # Middle-left 30x30 block
  r[31:60, 31:60] <- 3  # Middle-center 30x30 block
  r[31:60, 61:90] <- 1  # Middle-right 30x30 block

  r[61:90, 1:30] <- 3  # Bottom-left 30x30 block
  r[61:90, 31:60] <- 1  # Bottom-center 30x30 block
  r[61:90, 61:90] <- 2  # Bottom-right 30x30 block

  # Define confusion matrix, background not transitioning
  conf_mat <- matrix(1/3, nrow = 3, ncol = 3)

  # Assign row and column names
  rownames(conf_mat) <- c( "1", "2", "3")
  colnames(conf_mat) <- c( "1", "2", "3")
  conf_mat[1, ] <- 0
  conf_mat[ , 1] <- 0
  conf_mat[1, 1] <- 1


  # create 5 simulations with class not transitioning
  class_sim <- simulate_raster_patch(original_raster = r,
                                     transition_matrix = conf_mat,
                                     iterations = 1)

  # check that freq of input out sims are the same for class 1
  og_counts <- terra::freq(r)
  class_1_og <- og_counts[og_counts$value == 1, 3]

  sim_counts <- terra::freq(class_sim)
  class_1_sim <- sim_counts[sim_counts$value == 1, 3]

  # expect that no values in the tagged raster are negative
  expect_equal(class_1_og, class_1_sim)
})

test_that("no pixels are dropped", {
  # Still need to check this test
  # Create an empty raster with dimensions 100 by 100
  r <- terra::rast(nrows=100, ncols=100, xmin=0, xmax=100, ymin=0, ymax=100)

  # Initialize all cells with class 1
  terra::values(r) <- 1

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

  # Make conf mat with 1s on diagonal
  n <- 3
  conf_mat <- matrix(0, nrow = n, ncol = n)
  diag(conf_mat) <- 1

  # Assign row and column names
  rownames(conf_mat) <- c("1", "2", "3")
  colnames(conf_mat) <- c("1", "2", "3")
  conf_mat

  patch_sim <- simulate_raster_patch(original_raster = r,
                                       transition_matrix = conf_mat,
                                       iterations = 1)


  mismatch_mask <- r != patch_sim


  # expect that no values in the tagged raster are negative
  expect_equal(sum(terra::values(mismatch_mask)), 0)
})

