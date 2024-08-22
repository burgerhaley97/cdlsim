######################################################################
# simulation of CDL data workflow example from 2008
######################################################################

#### STEP 1: Load in the 90 meter cdl example data
bl_cdl <- terra::rast("exec/extdata/bl_cdl_90m.tif")
values(bl_cdl)

#### STEP 2: reclassify the raster
# Non-ag values
ag_mask <- c(61:65, 81:83, 87:88, 92, 111:112, 121:124,
             131, 141:143, 152, 176, 181, 190, 195)

# alfalfa and other hay
alfalfa <- c(36:37)

# corn, soy, cotton, wheat (and mixed categories)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)

# reclassify into background = 0, non-ag = 1, ag = 2, alfalfa = 3, major_ag = 4
ag_raster <- terra::app(bl_cdl, fun = function(x) {
  ifelse(is.nan(x), 0,
         ifelse(x %in% major_ag, 4,
                ifelse(x %in% alfalfa, 3,
                       ifelse(x %in% ag_mask, 1, 2))))
})

#### STEP 3: Get the confusion matrices for your states of interest
# Process the raw data downloaded from the zip file web address
download_cdl_mat_files(years = 2008, temp_dir = "extracted_files")
all_data_ut <- get_mat_data_ut()
all_data_id <- get_mat_data_id()

# Combine the lists for each state
combined_data <- Map(list, all_data_ut, all_data_id)

# Define the values that represent our classes of interest
non_ag <- c(61:65, 81:83, 87:88, 92, 111:112, 121:124, 131, 141:143, 152, 176, 181, 190, 195)
alfalfa <- c(36:37)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)
all_numbers <- 1:256
ag <- setdiff(all_numbers, c(non_ag, alfalfa, major_ag))

# List of categories with their corresponding vectors
categories <- list(non_ag = non_ag, ag = ag, alfalfa = alfalfa, major_ag = major_ag)

# get the confusion matrix for just 2008
result <- get_trans_mat(combined_data, categories)
transition_matrix <- result[[1]]

#### STEP 4: Run the simulation
# Simulate raster changes over 10 simulations
output_rasters <- simulate_raster_changes(ag_raster, n_simulations = 5, confusion_matrix = transition_matrix)

# Plot the first two rasters side by side
par(mfrow = c(1, 2))
plot(output_rasters[[1]], main = "Simulation 1")
plot(output_rasters[[2]], main = "Simulation 2")

plot(ag_raster)

######################################################################
#  Example using simple two category raster (terra sim)
######################################################################

# make sure simulation is working
# Create an empty raster with dimensions 100 by 100
r <- rast(nrows=100, ncols=100, xmin=0, xmax=100, ymin=0, ymax=100)

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

# transition matrix
confusion_matrix <- matrix(
  c(0.5, 0.5,  # Probabilities for transition from -1
    0.5,0.5),  # Probabilities for transition from -2
  nrow = 2,
  byrow = TRUE
)
# Assign row and column names
rownames(confusion_matrix) <- c("-1", "-2")
colnames(confusion_matrix) <- c("1", "2")

# Convert to a data frame for better readability
transition_matrix <- as.data.frame(confusion_matrix)
transition_matrix

# tag the edges
r_tagged <- tag_edges_2layer(r)
plot(r_tagged)

# Apply the transition function to each cell pair
result_raster <- app(r_tagged, fun = transition_function, transition_matrix = transition_matrix)

# test that the simulation works
test_two <- simulate_raster_terra(r, transition_matrix, 5, edge_depth = 3)
plot(test_two, main = "edge depth = 3")

# hexegon like shape?
test_two <- simulate_raster_terra(r, transition_matrix, 5, edge_depth = 20)
plot(test_two, main = "edge depth = 20")

test_two <- simulate_raster_terra(r, transition_matrix, 5, edge_depth = 30)
plot(test_two, main = "edge depth = 30")

test_two <- simulate_raster_terra(r, transition_matrix, 5, edge_depth = 100)
plot(test_two, main = "edge depth = 100")


test_two <- simulate_raster_terra(r, transition_matrix, 5, edge_depth = 120)
plot(test_two, main = "edge depth = 120")
test_freq <- freq(test_two)

#### old deterministic way
simple_rasters <- simulate_raster_changes(r, n_simulations = 5, confusion_matrix = transition_matrix)
plot(simple_rasters)

simple_counts <- freq(simple_rasters)
simple_counts


simple_counts[simple_counts$value == 2, ]

r_counts <- freq(sim)
r_counts

# Call the function with the raster and the edge mapping
tagged_r <- tag_edges(r, edge_depth = 1)
tagged_r

# try highlighting the tagged edges to see change
negative_cells <- tagged_r < 0
plot(r, main="Raster with Edges in Red", colNA="transparent")
plot(negative_cells, add=TRUE, col=c("transparent", "red"), legend=FALSE)

######################################################################
#  Example using simple three category raster (terra sim)
#  edge depth comparison
######################################################################
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


# transition matrix
confusion_matrix <- matrix(
  c(1/3, 1/3, 1/3,  # Probabilities for transition from -1
    1/3, 1/3, 1/3,
    1/3, 1/3, 1/3),  # Probabilities for transition from -2
  nrow = 3,
  byrow = TRUE
)
# Assign row and column names
rownames(confusion_matrix) <- c("-1", "-2", "-3")
colnames(confusion_matrix) <- c("1", "2", "3")

# Convert to a data frame for better readability
transition_matrix <- as.data.frame(confusion_matrix)
transition_matrix

# tag the edges
r_tagged <- tag_edges_2layer(r)
plot(r_tagged)

# Apply the transition function to each cell pair
result_raster <- app(r_tagged, fun = transition_function, transition_matrix = transition_matrix)

# test that the simulation works
test_two <- simulate_raster_terra(r, transition_matrix, 5, edge_depth = 3)
plot(test_two, main = "edge depth = 3")

# hexegon like shape?
test_two <- simulate_raster_terra(r, transition_matrix, 5, edge_depth = 20)
plot(test_two, main = "edge depth = 20")

test_two <- simulate_raster_terra(r, transition_matrix, 5, edge_depth = 120)
plot(test_two, main = "edge depth = 120")

# transition matrix
confusion_matrix <- matrix(
  c(1, 0, 0,  # Probabilities for transition from -1
    0.5, 0.5, 0,
    0.5, 0, 0.5),  # Probabilities for transition from -2
  nrow = 3,
  byrow = TRUE
)
# Assign row and column names
rownames(confusion_matrix) <- c("-1", "-2", "-3")
colnames(confusion_matrix) <- c("1", "2", "3")

# Convert to a data frame for better readability
transition_matrix <- as.data.frame(confusion_matrix)
transition_matrix

test_two <- simulate_raster_terra(r, transition_matrix, 5, edge_depth = 3)
plot(test_two, main = "edge depth = 3")

test_two <- simulate_raster_terra(r, transition_matrix, 5, edge_depth = 12)
plot(test_two, main = "edge depth = 20")


test_two <- simulate_raster_terra(r, transition_matrix, 5, edge_depth = 120)
plot(test_two, main = "edge depth = 120")



######################################################################
#
######################################################################
# make this into a test for use later
# Create a sample raster using the terra package for demonstration
r <- terra::rast(ncol=100, nrow=100, nlyrs = 1)
values(r) <- sample(0:4, ncell(r), replace=TRUE)
plot(r[[2]])

# Create a confusion matrix with transition probabilities
confusion_matrix <- matrix(
  c(0.1, 0.8, 0.05, 0.05,  # Probabilities for transition from -1
    0.1, 0.1, 0.1, 0.7,  # Probabilities for transition from -2
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

class_counts_og <- freq(ag_raster)
class_counts

# Call the function with the raster and the edge mapping
tagged_raster1 <- tag_edges(ag_raster, edge_depth = 1)


# Apply the transition probabilities
updated_pixel_values <- transition_pixels(values(tagged_raster1), transition_matrix)

# Update the raster with new pixel values
values(tagged_raster1) <- updated_pixel_values

# Print the result raster
class_counts1 <- freq(tagged_raster1)
class_counts1

# Call the function with the raster and the edge mapping
tagged_raster2 <- tag_edges(ag_raster, edge_depth = 1)

# Apply the transition probabilities
updated_pixel_values <- transition_pixels(values(tagged_raster2), transition_matrix)

# Update the raster with new pixel values
values(tagged_raster2) <- updated_pixel_values

# Print the result raster
class_counts2 <- freq(tagged_raster2)
class_counts2

# plot the two rasters side by side
# Set up the plotting area
par(mfrow = c(1, 2))  # 1 row, 2 columns

# Plot the first raster
plot(tagged_raster1, main = "Raster-1")

# Plot the second raster
plot(tagged_raster2, main = "Raster-2")


sim_test_one <- simulate_raster_changes(ag_raster, n_simulations = 5, confusion_matrix = transition_matrix)

# try highlighting the tagged edges to see change
negative_cells <- tagged_raster < 0
plot(tagged_raster, main="Raster with Negative Values Highlighted", colNA="transparent")
plot(negative_cells, add=TRUE, col=c("transparent", "red"), legend=FALSE)



###############################################################################
#                  Example workflow
###############################################################################

# Process the raw data downloaded from the zip file web address
all_data_ut <- get_mat_data_ut()
all_data_id <- get_mat_data_id()

# Combine the lists for each state
combined_data <- Map(list, all_data_ut, all_data_id)

# Define the values that represent our classes of interest
non_ag <- c(61:65, 81:83, 87:88, 92, 111:112, 121:124, 131, 141:143, 152, 176, 181, 190, 195)
alfalfa <- c(36:37)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)
all_numbers <- 1:256
other_ag <- setdiff(all_numbers, c(non_ag, alfalfa, major_ag))

# List of categories with their corresponding vectors
categories <- list(non_ag = non_ag, alfalfa = alfalfa, major_ag = major_ag, other_ag = other_ag)

# get the conf mat for each year based on both states
result <- get_trans_mat(combined_data, categories)


###############################################################################
#                  Example Using Iowa Data
###############################################################################
# aggregate to 90m (clipped raster in download file)
iowa_90 <- terra::aggregate(Iowan_test, fact = 3, fun = "modal")

# define major ag category
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)

# reclassify into background = 0, non-ag = 1, ag = 2, alfalfa = 3, major_ag = 4
ag_raster <- terra::app(iowa_90, fun = function(x) {
  ifelse(is.na(x), 0,
         ifelse(x %in% major_ag, 2, 1))
})


# Define 50/50 confusion matrix
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

# Run simulation with 5 iterations
sim_test_one <- simulate_raster_changes(ag_raster, n_simulations = 5, confusion_matrix = transition_matrix)
plot(sim_test_one, main = "Iowa Test")

# Print the result raster
counts <- freq(sim_test_one)
counts


show_cores(ag_raster, class = c(1,2))


###############################################################################
#                  Example Using Iowa Data (determinisitc)
###############################################################################
# aggregate to 90m (clipped raster in download file)
iowa_90 <- terra::aggregate(Iowan_test, fact = 3, fun = "modal")

# define major ag category
corn <- 1
soy <- 5

# reclassify into background = 0, non-ag = 1, ag = 2, alfalfa = 3, major_ag = 4
corn_soy_raster <- terra::app(iowa_90, fun = function(x) {
  ifelse(is.na(x), 0,
         ifelse(x %in% corn, 1,
         ifelse(x %in% soy, 2, 3)))
})

plot(corn_soy_raster, main = "corn = 1, soy = 2, other = 3")

# Define 50/50 confusion matrix
confusion_matrix <- matrix(
  c(1, 0, 0, 0,  # Probabilities for transition from -1
    0, 0.5, 0.5, 0,
    0, 0.5, 0.5, 0,
    0, 0, 0, 1),  # Probabilities for transition from -2
  nrow = 4,
  byrow = TRUE
)
# Assign row and column names
rownames(confusion_matrix) <- c("0", "-1", "-2", "-3")
colnames(confusion_matrix) <- c("0", "1", "2", "3")

# Convert to a data frame for better readability
transition_matrix <- as.data.frame(confusion_matrix)
transition_matrix

# Run simulation with 5 iterations
sim_D <- simulate_raster_D(corn_soy_raster, confusion_matrix = transition_matrix)
plot(sim_D, main = "Iowa Test Simulation deterministic")

# Print the result raster
counts <- freq(sim_test_one)
counts


###############################################################################
#                  Example Using Iowa Data (random sim)
###############################################################################
# Run simulation with 5 iterations
sim_D <- simulate_raster_D(r, n_simulations = 5, confusion_matrix = transition_matrix)
plot(sim_D, main = "Iowa Test Simulation determin")
counts <- freq(sim_D)
counts

###############################################################################
#                  Example Using Iowa Data (terra sim)
###############################################################################
# Run simulation with 5 iterations
sim_terra <- simulate_raster_terra(r, iterations = 5, transition_matrix = transition_matrix)
plot(sim_terra, main = "Iowa Test Simulation  terra")
counts <- freq(sim_terra)
counts

sim_terra_corn <- simulate_raster_terra(corn_soy_raster, iterations = 5, transition_matrix = transition_matrix)
plot(sim_terra_corn)
corn_soy <- freq(corn_soy_raster)
corn_soy
corn_soy <- freq(sim_terra_corn)
corn_soy
###############################################################################
#                  Example Using Iowa Data (random sim)
###############################################################################
# Run simulation with 5 iterations
sim_R <- simulate_raster_R(r, n_simulations = 5, confusion_matrix = transition_matrix)
plot(sim_R, main = "Iowa Test Simulation random")
counts <- freq(sim_R)
counts
plot(corn_soy_raster)

###############################################################################
#                  Test Rcpp Version of transition_function
###############################################################################
# make sure simulation is working
# Create an empty raster with dimensions 100 by 100
r <- rast(nrows=100, ncols=100, xmin=0, xmax=100, ymin=0, ymax=100)

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

plot(r)

# transition matrix
confusion_matrix <- matrix(
  c(0.9, 0.1,  # Probabilities for transition from -1
    0.1,0.9),  # Probabilities for transition from -2
  nrow = 2,
  byrow = TRUE
)
# Assign row and column names
rownames(confusion_matrix) <- c("-1", "-2")
colnames(confusion_matrix) <- c("1", "2")

# Convert to a data frame for better readability
transition_matrix <- as.data.frame(confusion_matrix)
transition_matrix

# test sim
sim_terra <- simulate_raster_rcpp(r, iterations = 5, transition_matrix = transition_matrix)
plot(sim_terra)

###############################################################################
#                  Compare Speed
###############################################################################
library(microbenchmark)

# Perform the benchmark
benchmark_results <- microbenchmark(
  sim_D = simulate_raster_D(r, n_simulations = 5, confusion_matrix = transition_matrix),
  sim_R = simulate_raster_R(r, n_simulations = 5, confusion_matrix = transition_matrix),
  sim_terra = simulate_raster_terra(r, iterations = 5, transition_matrix = transition_matrix),
  sim_terra_cpp = simulate_raster_rcpp(r, iterations = 5, transition_matrix = transition_matrix),
  times = 10  # Number of iterations
)

benchmark_results_corn_soy <- microbenchmark(
  sim_D = simulate_raster_D(corn_soy_raster, n_simulations = 5, confusion_matrix = transition_matrix),
  sim_R = simulate_raster_R(corn_soy_raster, n_simulations = 5, confusion_matrix = transition_matrix),
  sim_terra = simulate_raster_terra(corn_soy_raster, iterations = 5, transition_matrix = transition_matrix),
  sim_terra_cpp = simulate_raster_rcpp(corn_soy_raster, iterations = 5, transition_matrix = transition_matrix),
  times = 10  # Number of iterations
)

# Define 50/50 confusion matrix
confusion_matrix <- matrix(
  c(1, 0, 0, 0, 0, # Probabilities for transition from -1
    0, 0.5, 0.5, 0, 0,
    0, 0.5, 0.5, 0, 0,
    0, 0, 0, 1, 0,
    0, 0.25, 0.25, 0.25, 0.25),  # Probabilities for transition from -2
  nrow = 5,
  byrow = TRUE
)
# Assign row and column names
rownames(confusion_matrix) <- c("0", "-1", "-2", "-3", "-4")
colnames(confusion_matrix) <- c("0", "1", "2", "3", "4")
# Convert to a data frame for better readability
transition_matrix <- as.data.frame(confusion_matrix)
transition_matrix


# sim_D = simulate_raster_D(ag_raster, n_simulations = 5, confusion_matrix = transition_matrix),

# Perform the benchmark
benchmark_results_watershed <- microbenchmark(
  sim_R = simulate_raster_R(ag_raster, n_simulations = 5, confusion_matrix = transition_matrix),
  sim_terra = simulate_raster_terra(ag_raster, iterations = 5, transition_matrix = transition_matrix),
  sim_terra_cpp = simulate_raster_rcpp(ag_raster, iterations = 5, transition_matrix = transition_matrix),
  times = 2  # Number of iterations
)

# Perform the benchmark
benchmark_results_vector_cpp <- microbenchmark(
  sim_R = simulate_raster_R(ag_raster, n_simulations = 5, confusion_matrix = transition_matrix),
  sim_terra = simulate_raster_terra(ag_raster, iterations = 5, transition_matrix = transition_matrix),
  sim_terra_cpp = simulate_raster_rcpp(ag_raster, iterations = 5, transition_matrix = transition_matrix),
  times = 2  # Number of iterations
)

plot(ag_raster)

sim_D <- simulate_raster_D(ag_raster, n_simulations = 5, confusion_matrix = transition_matrix)
sim_terra <- simulate_raster_terra(corn_soy_raster, iterations = 5, transition_matrix = transition_matrix)
sim_terra_cpp_100 <- simulate_raster_rcpp(ag_raster, iterations = 100, transition_matrix = transition_matrix)
plot(sim_terra_cpp)
counts <- freq(sim_terra_cpp_100)
plot(counts$count)

# Create a separate plot for each value
ggplot(counts, aes(x = as.factor(layer), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  facet_wrap(~ value, scales = "free_y") +
  labs(x = "Layer", y = "Count", title = "Histograms of Count by Layer for Each Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##############################################################################
#                 Try simulating landscape metrics
##############################################################################
library(landscapemetrics)
library(readxl)
library(dplyr)
library(broom)
library(ggplot2)


#                          1 -- Prepare CDL Data
###############################################################################

#### STEP 1: Starting from raw spatrasters
bl_cdl_all <- terra::rast("C:/Users/A02425259/Git/LandUseBL/Intermediate_rasters/bl_crop.tif")
plot(bl_cdl_all[[1:5]])

# aggregate to 90m (clipped raster in download file)
bl_cdl_all_90 <- terra::aggregate(bl_cdl_all, fact = 3, fun = "modal")

# reclassify based on ag categories
#### STEP 2: reclassify the raster
# Non-ag values
ag_mask <- c(61:65, 81:83, 87:88, 92, 111:112, 121:124,
             131, 141:143, 152, 176, 181, 190, 195)

# alfalfa and other hay
alfalfa <- c(36:37)

# corn, soy, cotton, wheat (and mixed categories)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)

# reclassify into background = 0, non-ag = 1, ag = 2, alfalfa = 3, major_ag = 4
ag_raster_all <- terra::app(bl_cdl_all_90, fun = function(x) {
  ifelse(is.nan(x), 0,
         ifelse(x %in% major_ag, 4,
                ifelse(x %in% alfalfa, 3,
                       ifelse(x %in% ag_mask, 1, 2))))
})


#                      2 -- Prepare Transition Matrices
##############################################################################
#### Get transition matrices for states around Bear Lake watershed
UT <- get_mat_data("UT")
ID <- get_mat_data("ID")
WY <- get_mat_data("WY")

# Combine the lists for each state
combined_data <- Map(list, UT, ID, WY)

# Define the values that represent our classes of interest
non_ag <- c(61:65, 81:83, 87:88, 92, 111:112, 121:124, 131, 141:143, 152, 176, 181, 190, 195)
alfalfa <- c(36:37)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)
all_numbers <- 1:256
ag <- setdiff(all_numbers, c(non_ag, alfalfa, major_ag))

# List of categories with their corresponding vectors
cats <- list(non_ag = non_ag, ag = ag, alfalfa = alfalfa, major_ag = major_ag)

tran_mat <- get_trans_mat_gen(combined_data, categories = cats)
trans_mat_2008 <- as.matrix(tran_mat[[1]])


#### Simulate the data for just one year
sim_terra_cpp_10 <- simulate_raster_rcpp(ag_raster, iterations = 10, transition_matrix = trans_mat_2008)
plot(sim_terra_cpp_10)

sim_terra_cpp_100 <- simulate_raster_rcpp(ag_raster, iterations = 100, transition_matrix = trans_mat_2008)
plot(sim_terra_cpp_100[[1:10]])


#                          3 -- Simulate the SpatRasters
##############################################################################

# simulate all years
simulate_multiple_rasters <- function(ag_rasters, transition_matrices, iterations = 100) {
  # Get the number of layers in the SpatRaster
  num_layers <- nlyr(ag_rasters)

  # Initialize a list to store the results for each layer
  simulation_results <- vector("list", num_layers)

  # Loop through each layer of the SpatRaster
  for (i in 1:num_layers) {
    ag_raster <- ag_rasters[[i]]
    transition_matrix <- transition_matrices[[i]]

    # Perform the simulations
    simulation_results[[i]] <- simulate_raster_rcpp(ag_raster, iterations = iterations, transition_matrix = transition_matrix)
  }

  return(simulation_results)
}


sim_all_years <- simulate_multiple_rasters(ag_rasters = ag_raster_all[[1:3]], transition_matrices = tran_mat[1:3], iterations = 2)
plot(sim_all_years[[1]])
class(sim_all_years)


#                  4 -- Calculate Metrics for Each Simulation
##############################################################################

# function to apply the landscapemetrics functions to our sim_results
apply_function_to_simulations <- function(simulation_results, func) {
  # Initialize a list to store the results
  lsm_results <- list()

  # Loop through each simulated raster in the simulation_results list
  for (i in seq_along(simulation_results)) {
    sim_raster <- simulation_results[[i]]

    # Apply the provided function
    result <- func(sim_raster)

    # Store the result
    lsm_results[[i]] <- result
  }

  return(lsm_results)
}

# test function
mp_area_all <- apply_function_to_simulations(sim_all_years, func = lsm_c_area_mn)
print(mp_area_all)

# mean patch area (in hectares)
# The metric summarizes each class as the mean of all patch areas belonging to class i.
mean_patch_area <- lsm_c_area_mn(sim_terra_cpp_100)


# Assuming your tibble is named `df`
# Calculate 95% confidence intervals by class
confidence_intervals <- mean_patch_area %>%
  group_by(class) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    # CI = mean +/- the t-crit value * standard error
    lower_ci = mean_value - qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n()),
    upper_ci = mean_value + qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n())
  )

print(confidence_intervals)


# filter out background
df_filtered <- mean_patch_area %>%
  filter(class != 0)

ggplot(df_filtered, aes(x = as.factor(class), y = value)) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  stat_summary(fun.data = function(y) {
    mean_y <- mean(y)
    lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
    upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
    return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
  }, geom = "errorbar", width = 0.2, color = "blue") +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Values with 95% Confidence Intervals"
  ) +
  theme_minimal()

# Create a scatter plot for each class
ggplot(mean_patch_area, aes(x = layer, y = value, color = as.factor(class))) +
  geom_point() +
  facet_wrap(~ class, scales = "free") +
  labs(
    x = "Layer",
    y = "Value",
    color = "Class",
    title = "Scatter Plot of Value for Each Class"
  ) +
  theme_minimal()

# filter out background
df <- mean_patch_area %>%
  filter(class == 2)
area <- df$value

# Perform a t-test to get the 95% CI
t_test_result <- t.test(area)

# Extract the 95% confidence interval
ci <- t_test_result$conf.int

print(ci)


# Total class area (in hectares)
# The total (class) area sums the area of all patches belonging to class i.
total_patch_area <- lsm_c_ca(ag_raster)

# patch density
lsm_c_pd(landscape, directions = 8)

# Interspersion and Juxtaposition index (describes the intermixing of classes)
lsm_c_iji(landscape, verbose = TRUE)

# Patch cohesion
# percent where higher = more aggregation of patches in same class
# COHESION is an 'Aggregation metric'. It characterizes the connectedness of patches belonging to class i.
lsm_c_cohesion(landscape, directions = 8)

# percent land cover by class
# percent, Approches PLAND = 0 when the proportional class area is decreasing. Equals PLAND = 100 when only one patch is present.
lsm_c_pland(landscape, directions = 8)

# Largest patch index (Area and Edge metric)
# percentage of the landscape covered by the largest patch in the landscape. It is a simple measure of dominance.
# Approaches LPI = 0 when the largest patch is becoming small and equals LPI = 100 when only one patch is present
lsm_l_lpi(landscape, directions = 8)

# Shannon's diversity index (Diversity metric)
lsm_l_shdi(landscape)

# Simpson's diversity index (Diversity metric)
lsm_l_sidi(landscape, directions = 8)
