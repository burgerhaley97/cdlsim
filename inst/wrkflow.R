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

plot(r)
# transition matrix
confusion_matrix <- matrix(
  c(1/3, 1/3, 1/3,  # Probabilities for transition from -1
    1/3, 1/3, 1/3,
    1/3, 1/3, 1/3),  # Probabilities for transition from -2
  nrow = 3,
  byrow = TRUE
)

# transition matrix
confusion_matrix <- matrix(
  c(1, 0, 0,  # Probabilities for transition from -1
    0, .5, .5,
    0, .5, .5),  # Probabilities for transition from -2
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





# aggregate to 90m (clipped raster in download file)
bl_cdl_all_90 <- terra::aggregate(bl_cdl_all, fact = 3, fun = "modal")
unique(bl_cdl_all_90[[1]])

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
library(readxl)
library(dplyr)
library(terra)
library(ggplot2)
library(landscapemetrics)
UT <- get_mat_data("UT")
ID <- get_mat_data("ID")
WY <- get_mat_data("WY")

# Combine the lists for each state
combined_data <- Map(list, UT, ID, WY)
combined_data[[1]]

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

ag_raster <- ag_raster_all[[1]]

#### Simulate the data for just one year
sim_terra_cpp_10 <- simulate_raster_rcpp(ag_raster, iterations = 10, transition_matrix = trans_mat_2008)

sim_terra_cpp_100 <- simulate_raster_rcpp(ag_raster, iterations = 100, transition_matrix = trans_mat_2008)

sim_terra_cpp_1000 <- simulate_raster_rcpp(ag_raster, iterations = 1000, transition_matrix = trans_mat_2008)

# calculate og and simulated metrics
c_area_og <- lsm_c_area_mn(ag_raster)
c_area_10 <- lsm_c_area_mn(sim_terra_cpp_10)
c_area_100 <- lsm_c_area_mn(sim_terra_cpp_100)
c_area_1000 <- lsm_c_area_mn(sim_terra_cpp_1000)

CI_10 <- calculate_confidence_intervals(c_area_10)
CI_100 <- calculate_confidence_intervals(c_area_100)
CI_1000 <- calculate_confidence_intervals(c_area_1000)


func_list <- list(lsm_c_cohesion, lsm_c_core_mn, lsm_c_te, lsm_l_area_mn, lsm_c_pland, lsm_c_clumpy, lsm_c_mesh)
func_list_2 <- list(lsm_l_tca, lsm_l_sidi, lsm_l_sidi, lsm_l_pd, lsm_l_np, lsm_l_lpi, lsm_l_core_mn, lsm_l_cohesion, lsm_l_area_mn,
                    lsm_c_lpi, lsm_c_contig_mn)
funcs_results_10 <- apply_functions_to_simulations(sim_terra_cpp_10, func_list)
apply_functions_to_simulations



results_CI_10 <- calculate_confidence_intervals_list(funcs_results_10)
results_CI_10


mesh_og <- lsm_c_mesh(ag_raster)
te_og <- lsm_c_te(ag_raster)


CI_10 <- CI_10 %>% mutate(source = "CI_10")
CI_100 <- CI_100 %>% mutate(source = "CI_100")
c_area_og <- c_area_og %>% mutate(source = "Original")

# Combine the data frames
combined_CI <- bind_rows(CI_10, CI_100)

# Plot CI_10 and CI_100 with confidence intervals
p <- ggplot(combined_CI, aes(x = class, y = mean_value, color = source)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +  # Plot the mean values
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.2), width = 0.1) +  # Plot the error bars
  labs(title = "Comparison of 95% Confidence Intervals by Class",
       x = "Class", y = "Mean Value") +
  theme_minimal() +
  scale_color_manual(values = c("CI_10" = "blue", "CI_100" = "red", "Original" = "black"))  # Customize colors

# Add the original data as points without error bars
p <- p + geom_point(data = c_area_og, aes(x = class, y = value),
                    color = "black", size = 4, shape = 17)  # Plot as triangles or use another shape

# Print the plot
print(p)



c_mesh_og <- c_area_og %>% mutate(source = "Original")

# Combine the data frames
combined_CI <- results_CI_10$mesh

# Plot CI_10 and CI_100 with confidence intervals
p <- ggplot(combined_CI, aes(x = class, y = mean_value)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +  # Plot the mean values
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.2), width = 0.1) +  # Plot the error bars
  labs(title = "Comparison of 95% Confidence Intervals by Class",
       x = "Class", y = "Mean Value") +
  theme_minimal() +
  scale_color_manual(values = c("CI_10" = "blue", "CI_100" = "red", "Original" = "black"))  # Customize colors

# Add the original data as points without error bars
p <- p + geom_point(data = c_area_og, aes(x = class, y = value),
                    color = "black", size = 4, shape = 17)  # Plot as triangles or use another shape

# Print the plot
print(p)
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

# simulate the first three years of cdl data 10, 100, and 1000 times
sim_all_years_10 <- simulate_multiple_rasters(ag_rasters = ag_raster_all[[1:3]], transition_matrices = tran_mat[1:3], iterations = 10)
sim_all_years_100 <- simulate_multiple_rasters(ag_rasters = ag_raster_all[[1:3]], transition_matrices = tran_mat[1:3], iterations = 100)
sim_all_years_1000 <- simulate_multiple_rasters(ag_rasters = ag_raster_all[[1:3]], transition_matrices = tran_mat[1:3], iterations = 1000)

# Plot
ggplot(df_filtered, aes(x = class, y = mean_value, group = class)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Dots for means
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.5), width = 0.2) +  # Error bars
  labs(title = "Mean Value by Class with 95% Confidence Intervals",
       x = "Class", y = "Mean Value") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  geom_point(c_area_og, x = class, y = value)




#                  4 -- Calculate Metrics for Each Simulation
##############################################################################

# function to apply the landscapemetrics functions to our sim_results
apply_function_to_simulations <- function(simulation_results, func) {
  # Initialize a list to store the results
  lsm_results <- list()

  # Loop through each simulated raster in the simulation_results list
  for (i in terra:nlyr(simulation_results)) {
    sim_raster <- simulation_results[[i]]

    # Apply the provided function
    result <- func(sim_raster)

    # Store the result
    lsm_results[[i]] <- result
  }

return(lsm_results)

}

apply_functions_to_simulations <- function(simulation_results, func_list) {
  # Initialize a list to store the results
  lsm_results <- list()

  # Loop through each simulated raster in the simulation_results list
  for (i in seq_along(simulation_results)) {
    sim_raster <- simulation_results[[i]]

    # Initialize a list to store results for this raster
    raster_results <- list()

    # Loop through each function in func_list and apply it
    for (j in seq_along(func_list)) {
      func <- func_list[[j]]
      result <- func(sim_raster)

      # Store the result with the function's name
      raster_results[[j]] <- result
    }

    # Store the raster's results in the main results list
    lsm_results[[i]] <- raster_results
  }

  return(lsm_results)
}

apply_functions_to_simulations <- function(simulation_results, func_list) {
  # Initialize a list to store the results
  lsm_results <- list()

  # Loop through each layer in the SpatRaster object
  for (i in seq_len(terra::nlyr(simulation_results))) {
    # Extract the specific layer
    sim_raster <- terra::subset(simulation_results, i)

    # Initialize a list to store results for this layer
    layer_results <- list()

    # Loop through each function in func_list and apply it
    for (j in seq_along(func_list)) {
      func <- func_list[[j]]
      result <- func(sim_raster)

      # Store the result with the function's name
      layer_results[[j]] <- result
    }

    # Store the layer's results in the main results list, with the layer index as the key
    lsm_results[[i]] <- layer_results
  }

  return(lsm_results)
}
# Combine the data frames with an identifier
combined_df <- bind_rows(
  lapply(seq_along(class_area_CI), function(i) {
    class_area_CI[[i]] %>% mutate(frame = paste0("Frame ", i))
  })
)

# filter out background
df_filtered <- combined_df %>%
  filter(class != 0)

# filter out background
df_filtered <- combined_df %>%
  filter(class != 0 & class != 1)

# Plot
ggplot(df_filtered, aes(x = class, y = mean_value, color = frame, group = frame)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Dots for means
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.5), width = 0.2) +  # Error bars
  labs(title = "Mean Value by Class with 95% Confidence Intervals",
       x = "Class", y = "Mean Value") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Nice color scheme

#                    5 -- Calculate 95% CIs
##############################################################################
calculate_confidence_intervals <- function(df) {
  df %>%
    group_by(class) %>%
    summarize(
      mean_value = mean(value, na.rm = TRUE),
      # CI = mean +/- the t-crit value * standard error
      lower_ci = mean_value - qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n()),
      upper_ci = mean_value + qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n())
    )
}


# function to calculate the 95% CIs from a list of data frames
calculate_confidence_intervals_list <- function(df_list) {
  lapply(df_list, function(df) {
    df %>%
      group_by(class) %>%
      summarize(
        mean_value = mean(value, na.rm = TRUE),
        # CI = mean +/- the t-crit value * standard error
        lower_ci = mean_value - qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n()),
        upper_ci = mean_value + qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n())
      )
  })

}

calculate_confidence_intervals_list <- function(df_list_of_lists) {
  # Determine the number of data frames in each sublist
  num_elements <- length(df_list_of_lists[[1]])

  # Initialize a named list to store the results
  results <- list()

  # Loop through each position across all sublists
  for (i in seq_len(num_elements)) {
    # Extract the data frames at the ith position from each sublist
    combined_df <- bind_rows(lapply(df_list_of_lists, `[[`, i))

    # Calculate the confidence intervals for the combined data frames
    ci_df <- combined_df %>%
      group_by(class) %>%
      summarize(
        mean_value = mean(value, na.rm = TRUE),
        lower_ci = mean_value - qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n()),
        upper_ci = mean_value + qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n())
      )

    # Use the value of the first cell in the metric column as the name
    metric_name <- combined_df$metric[1]

    # Store the result with the metric name as the key
    results[[metric_name]] <- ci_df
  }

  return(results)
}

calculate_confidence_intervals <- function(df_list_of_lists) {
  # Initialize a list to store the results
  results <- list()

  # Loop through each list of data frames
  for (list_index in seq_along(df_list_of_lists)) {
    df_list <- df_list_of_lists[[list_index]]

    # Initialize a list to store the results for this specific list
    list_results <- list()

    for (df in df_list) {
      # Extract the name based on the first entry in the level and metric columns
      name <- paste(df$level[1], df$metric[1], sep = " ")

      # Calculate the confidence intervals
      ci_df <- df %>%
        group_by(class) %>%
        summarize(
          mean_value = mean(value, na.rm = TRUE),
          # CI = mean +/- the t-crit value * standard error
          lower_ci = mean_value - qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n()),
          upper_ci = mean_value + qt(0.975, df = n() - 1) * sd(value, na.rm = TRUE) / sqrt(n())
        )

      # Store the result with the generated name in the current list
      list_results[[name]] <- ci_df
    }

    # Store the current list's results in the main results list with the original list number as the name
    results[[as.character(list_index)]] <- list_results
  }

  return(results)
}

# Combine the data frames with an identifier
combined_df <- bind_rows(
  lapply(seq_along(class_area_CI), function(i) {
    class_area_CI[[i]] %>% mutate(frame = paste0("Frame ", i))
  })
)

# filter out background
df_filtered <- combined_df %>%
  filter(class != 0)

# filter out background
df_filtered <- combined_df %>%
  filter(class != 0 & class != 1)

# Plot
ggplot(df_filtered, aes(x = class, y = mean_value, color = frame, group = frame)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Dots for means
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.5), width = 0.2) +  # Error bars
  labs(title = "Mean Value by Class with 95% Confidence Intervals",
       x = "Class", y = "Mean Value") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Nice color scheme


# example metrics to be calculated
################################################################################
# mean patch area (in hectares)
# The metric summarizes each class as the mean of all patch areas belonging to class i.
mean_patch_area <- lsm_c_area_mn(sim_terra_cpp_100)


# Assuming your tibble is named `df`
# Calculate 95% confidence intervals by class
confidence_intervals <- mp_area_all %>%
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

og_mean_area <- lsm_c_area_mn(ag_raster_all[[1]])
og_mean_area

ggplot(mean_patch_area, aes(x = layer, y = value, color = as.factor(class))) +
  geom_point() +
  facet_wrap(~ class, scales = "free") +
  labs(
    x = "Layer",
    y = "Value",
    color = "Class",
    title = "Scatter Plot of Value for Each Class"
  ) +
  theme_minimal() +
  # Add horizontal lines from 'og_mean_area'
  geom_hline(data = og_mean_area,
             aes(yintercept = value),
             color = "black", linetype = "dashed", size = 1)

# filter out background
df <- mean_patch_area %>%
  filter(class == 2)
area <- df$value

# Perform a t-test to get the 95% CI
t_test_result <- t.test(area)

# Extract the 95% confidence interval
ci <- t_test_result$conf.int

print(ci)


###############################################################################
#                       Test for sensitivity 10, 100, 1000
###############################################################################

# simulate the first three years of cdl data 10, 100, and 1000 times
sim_all_years_10 <- simulate_multiple_rasters(ag_rasters = ag_raster_all[[1:3]], transition_matrices = tran_mat[1:3], iterations = 10)
sim_all_years_100 <- simulate_multiple_rasters(ag_rasters = ag_raster_all[[1:3]], transition_matrices = tran_mat[1:3], iterations = 100)
sim_all_years_1000 <- simulate_multiple_rasters(ag_rasters = ag_raster_all[[1:3]], transition_matrices = tran_mat[1:3], iterations = 1000)

func_list <- list(lsm_c_area_mn, lsm_c_cohesion, lsm_c_core_mn, lsm_c_te, lsm_l_area_mn)

### try some class functions
# look at different metrics
# List of functions to apply
func_list <- list(lsm_c_area_mn, lsm_c_pland)
metrics_10 <- apply_functions_to_simulations(sim_all_years_10, func_list)
metrics_100 <- apply_functions_to_simulations(sim_all_years_100, func_list)
metrics_1000 <- apply_functions_to_simulations(sim_all_years_1000, func_list)

# List of simulation results and corresponding names for saving
sim_results_list <- list(sim_all_years_10, sim_all_years_100, sim_all_years_1000)
save_names <- c("r_10", "r_100", "r_1000")

# List of functions to apply
func_list <- list(lsm_c_area_mn, lsm_c_pland)

# Loop through the simulation results
for (i in seq_along(sim_results_list)) {
  # Apply the list of functions to the current simulation results
  metrics <- apply_functions_to_simulations(sim_results_list[[i]], func_list)

  # Assign the result to a dynamically named variable
  assign(save_names[i], metrics)
}


#### calculate the 95% CIs
mn_area_10 <- calculate_confidence_intervals(r_10)
mn_area_100 <- calculate_confidence_intervals(r_100)
mn_area_1000 <- calculate_confidence_intervals(r_1000)

# Combine the data frames with an identifier "Year"
combined_df <- bind_rows(
  lapply(seq_along(mn_area_10), function(i) {
    mn_area_10[[i]] %>% mutate(frame = paste0("Year ", i))
  })
)

# filter out background
df_filtered <- combined_df %>%
  filter(class != 0)

# Plot each variation 10, 100, 1000 by frame (year)
ggplot(df_filtered, aes(x = class, y = mean_value, color = frame, group = Year)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Dots for means
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.5), width = 0.2) +  # Error bars
  labs(title = "Mean Value by Class with 95% Confidence Intervals",
       x = "Class", y = "Mean Value") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")









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


###############################################################################
#                10/24:   Calculate 95% CIs for Patch Sim
###############################################################################
library(dplyr)
library(terra)
library(ggplot2)

## load 2008 example data
cdl_2008 <- terra::rast("inst/extdata/bl_cdl_90m.tif")

# first crop data to smaller watershed unit around Bear Lake
# load in the watershed polygon
bl_watershed <- sf::st_read("C:/Users/A02425259/Git/LandUseBL/shapefiles")
bl_watershed_trans <- sf::st_transform(bl_watershed, crs = sf::st_crs(cdl_2008))

# subset sbu watershed to just include Bear Lake polygon
bl_sub <- bl_watershed_trans %>%
  dplyr::filter(name == "Bear Lake")

# transform to the correct CRS and crop the CDL data to this area
bl_sub_raster <- mask(cdl_2008, bl_sub)
bl_sub_raster <- crop(bl_sub_raster, bl_sub)

#### Reclassify the rasters to major ag categories
# Non-ag values
ag_mask <- c(
  61:65, 81:83, 87:88, 92, 111:112, 121:124,
  131, 141:143, 152, 176, 181, 190, 195
)
# alfalfa and other hay
alfalfa <- c(36:37)
# corn, soy, cotton, wheat (and mixed categories)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)

# reclassify into background = 0, major-ag = 1, alfalfa = 2, non-ag = 3, gen_ag = 4
cdl_reclass <- terra::app(bl_sub_raster, fun = function(x) {
  ifelse(is.na(x), 0,
         ifelse(x %in% major_ag, 1,
                ifelse(x %in% alfalfa, 2,
                       ifelse(x %in% ag_mask, 3, 4)
                )
         )
  )
})

plot(cdl_reclass)

# Define confusion matrix, background not transitioning
conf_mat <- matrix(1/5, nrow = 5, ncol = 5)
conf_mat[1, ] <- c(1, 0, 0, 0, 0)
conf_mat[, 1] <- c(1, 0, 0, 0, 0)

# Assign row and column names
rownames(conf_mat) <- c("0", "1", "2", "3", "4")
colnames(conf_mat) <- c("0", "1", "2", "3", "4")
conf_mat


################################################################################
#               Create Simulations with 10, 100, 1000 iterations
################################################################################

bench::mark(
patch_sim_10 <- simulate_raster_patch(original_raster = cdl_reclass,
                                   transition_matrix = conf_mat,
                                   iterations = 10),
iterations = 1)

patch_sim_100 <- simulate_raster_patch(original_raster = cdl_reclass,
                                   transition_matrix = conf_mat,
                                   iterations = 100)

patch_sim_1000 <- simulate_raster_patch(original_raster = cdl_reclass,
                                       transition_matrix = conf_mat,
                                       iterations = 1000)

################################################################################
#                  Calculate Mean Patch Area
################################################################################
### Try with 10 iterations
mean_patch <- landscapemetrics::lsm_c_area_mn(patch_sim_10)

mean_patch_sim <- mean_patch |>
  mutate(type = "sim") |>
  filter(class > 0)

mean_patch_og <- landscapemetrics::lsm_c_area_mn(cdl_reclass)

mean_patch_og <- mean_patch_og |>
  mutate(type = "og") |>
  filter(class > 0)

mean_patch_combo <- bind_rows(mean_patch_sim, mean_patch_og)

ggplot(mean_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  # Plot the mean for type == "og"
  geom_point(data = mean_patch_combo[mean_patch_combo$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  # Plot the mean and confidence intervals for type == "sim"
  stat_summary(data = mean_patch_combo[mean_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = mean_patch_combo[mean_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Patch Area with 95% CIs (10)"
  ) +
  theme_minimal()

### Try with 100 iterations
mean_patch <- landscapemetrics::lsm_c_area_mn(patch_sim_100)

mean_patch_sim <- mean_patch |>
  mutate(type = "sim") |>
  filter(class > 0)

mean_patch_og <- landscapemetrics::lsm_c_area_mn(cdl_reclass)

mean_patch_og <- mean_patch_og |>
  mutate(type = "og") |>
  filter(class > 0)

mean_patch_combo <- bind_rows(mean_patch_sim, mean_patch_og)

ggplot(mean_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  # Plot the mean for type == "og"
  geom_point(data = mean_patch_combo[mean_patch_combo$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  # Plot the mean and confidence intervals for type == "sim"
  stat_summary(data = mean_patch_combo[mean_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = mean_patch_combo[mean_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Patch Area with 95% CIs (100)"
  ) +
  theme_minimal()

### Try with 1000 iterations
mean_patch <- landscapemetrics::lsm_c_area_mn(patch_sim_1000)

mean_patch_sim <- mean_patch |>
  mutate(type = "sim") |>
  filter(class > 0)

mean_patch_og <- landscapemetrics::lsm_c_area_mn(cdl_reclass)

mean_patch_og <- mean_patch_og |>
  mutate(type = "og") |>
  filter(class > 0)

mean_patch_combo <- bind_rows(mean_patch_sim, mean_patch_og)

ggplot(mean_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  # Plot the mean for type == "og"
  geom_point(data = mean_patch_combo[mean_patch_combo$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  # Plot the mean and confidence intervals for type == "sim"
  stat_summary(data = mean_patch_combo[mean_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = mean_patch_combo[mean_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Patch Area with 95% CIs (1000)"
  ) +
  theme_minimal()


ggplot(mean_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  geom_point(size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Patch Area by Class (1000)"
  ) +
  theme_minimal()
################################################################################
#                  Calculate Pland by Class
################################################################################
pland_patch <- landscapemetrics::lsm_c_pland(patch_sim_10)

pland_patch_sim <- pland_patch |>
  mutate(type = "sim") |>
  filter(class > 0)

pland_patch_og <- landscapemetrics::lsm_c_pland(cdl_reclass)

pland_patch_og <- pland_patch_og |>
  mutate(type = "og") |>
  filter(class > 0)

pland_patch_combo <- bind_rows(pland_patch_sim, pland_patch_og)

ggplot(pland_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  # Plot the mean for type == "og"
  geom_point(data = pland_patch_combo[pland_patch_combo$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  # Plot the mean and confidence intervals for type == "sim"
  stat_summary(data = pland_patch_combo[pland_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = pland_patch_combo[pland_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean PLAND with 95% CIs (10)"
  ) +
  theme_minimal()

pland_patch <- landscapemetrics::lsm_c_pland(patch_sim_100)

pland_patch_sim <- pland_patch |>
  mutate(type = "sim") |>
  filter(class > 0)

pland_patch_og <- landscapemetrics::lsm_c_pland(cdl_reclass)

pland_patch_og <- pland_patch_og |>
  mutate(type = "og") |>
  filter(class > 0)

pland_patch_combo <- bind_rows(pland_patch_sim, pland_patch_og)

ggplot(pland_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  # Plot the mean for type == "og"
  geom_point(data = pland_patch_combo[pland_patch_combo$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  # Plot the mean and confidence intervals for type == "sim"
  stat_summary(data = pland_patch_combo[pland_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = pland_patch_combo[pland_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Proportional Land (PLAND) with 95% CIs (100)"
  ) +
  theme_minimal()

pland_patch <- landscapemetrics::lsm_c_pland(patch_sim_1000)

pland_patch_sim <- pland_patch |>
  mutate(type = "sim") |>
  filter(class > 0)

pland_patch_og <- landscapemetrics::lsm_c_pland(cdl_reclass)

pland_patch_og <- pland_patch_og |>
  mutate(type = "og") |>
  filter(class > 0)

pland_patch_combo <- bind_rows(pland_patch_sim, pland_patch_og)

ggplot(pland_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  # Plot the mean for type == "og"
  geom_point(data = pland_patch_combo[pland_patch_combo$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  # Plot the mean and confidence intervals for type == "sim"
  stat_summary(data = pland_patch_combo[pland_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = pland_patch_combo[pland_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Proportional Land (PLAND) with 95% CIs (1000)"
  ) +
  theme_minimal()


################################################################################
#                  Calculate Shannon's Diversity Index
################################################################################
shdi_patch <- landscapemetrics::lsm_l_shdi(patch_sim_10)

shdi_patch_sim <- shdi_patch |>
  mutate(type = "sim")

shdi_patch_og <- landscapemetrics::lsm_l_shdi(cdl_reclass)

shdi_patch_og <- shdi_patch_og |>
  mutate(type = "og")

shdi_patch_combo <- bind_rows(shdi_patch_sim, shdi_patch_og)

ggplot(shdi_patch_combo, aes(x = as.factor(level), y = value, color = type)) +
  # Plot the mean for type == "og"
  geom_point(data = shdi_patch_combo[shdi_patch_combo$type == "og", ],
             aes(x = as.factor(level), y = value), color = "red", size = 3) +
  # Plot the mean and confidence intervals for type == "sim"
  stat_summary(data = shdi_patch_combo[shdi_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = shdi_patch_combo[shdi_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  scale_y_continuous(limits = c(0.85, 1.0))+
  labs(
    x = "Class",
    y = "Value",
    title = "Mean SHDI with 95% CIs (10)"
  ) +
  theme_minimal()

shdi_patch <- landscapemetrics::lsm_l_shdi(patch_sim_100)

shdi_patch_sim <- shdi_patch |>
  mutate(type = "sim")

shdi_patch_og <- landscapemetrics::lsm_l_shdi(cdl_reclass)

shdi_patch_og <- shdi_patch_og |>
  mutate(type = "og")

shdi_patch_combo <- bind_rows(shdi_patch_sim, shdi_patch_og)

ggplot(shdi_patch_combo, aes(x = as.factor(level), y = value, color = type)) +
  # Plot the mean for type == "og"
  geom_point(data = shdi_patch_combo[shdi_patch_combo$type == "og", ],
             aes(x = as.factor(level), y = value), color = "red", size = 3) +
  # Plot the mean and confidence intervals for type == "sim"
  stat_summary(data = shdi_patch_combo[shdi_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = shdi_patch_combo[shdi_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  scale_y_continuous(limits = c(0.85, 1.0))+
  labs(
    x = "Class",
    y = "Value",
    title = "Mean SHDI with 95% CIs (100)"
  ) +
  theme_minimal()

shdi_patch <- landscapemetrics::lsm_l_shdi(patch_sim_1000)

shdi_patch_sim <- shdi_patch |>
  mutate(type = "sim") |>
  filter(class > 0)

shdi_patch_og <- landscapemetrics::lsm_l_shdi(cdl_reclass)

shdi_patch_og <- shdi_patch_og |>
  mutate(type = "og") |>
  filter(class > 0)

shdi_patch_combo <- bind_rows(shdi_patch_sim, shdi_patch_og)

ggplot(shdi_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  # Plot the mean for type == "og"
  geom_point(data = shdi_patch_combo[shdi_patch_combo$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  # Plot the mean and confidence intervals for type == "sim"
  stat_summary(data = shdi_patch_combo[shdi_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = shdi_patch_combo[shdi_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean SHDI with 95% CIs (1000)"
  ) +
  theme_minimal()


################################################################################
#                  Calculate Landscape Cohesion Index
################################################################################
coh_patch <- landscapemetrics::lsm_l_cohesion(patch_sim_10)

coh_patch_sim <- coh_patch |>
  mutate(type = "sim")

coh_patch_og <- landscapemetrics::lsm_l_cohesion(cdl_reclass)

coh_patch_og <- coh_patch_og |>
  mutate(type = "og")

coh_patch_combo <- bind_rows(coh_patch_sim, coh_patch_og)

ggplot(coh_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  geom_point(data = coh_patch_combo[coh_patch_combo$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  stat_summary(data = coh_patch_combo[coh_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = coh_patch_combo[coh_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Cohesion with 95% CIs (10)"
  ) +
  theme_minimal()

coh_patch <- landscapemetrics::lsm_l_cohesion(patch_sim_100)

coh_patch_sim <- coh_patch |>
  mutate(type = "sim")

coh_patch_og <- landscapemetrics::lsm_l_cohesion(cdl_reclass)

coh_patch_og <- coh_patch_og |>
  mutate(type = "og")

coh_patch_combo <- bind_rows(coh_patch_sim, coh_patch_og)

ggplot(coh_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  geom_point(data = coh_patch_combo[coh_patch_combo$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  stat_summary(data = coh_patch_combo[coh_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = coh_patch_combo[coh_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Cohesion with 95% CIs (10)"
  ) +
  theme_minimal()

coh_patch <- landscapemetrics::lsm_l_cohesion(patch_sim_1000)

coh_patch_sim <- coh_patch |>
  mutate(type = "sim") |>
  filter(class > 0)

coh_patch_og <- landscapemetrics::lsm_l_cohesion(cdl_reclass)

coh_patch_og <- coh_patch_og |>
  mutate(type = "og") |>
  filter(class > 0)

coh_patch_combo_1000 <- bind_rows(coh_patch_sim, coh_patch_og)

ggplot(coh_patch_combo_1000, aes(x = as.factor(class), y = value, color = type)) +
  geom_point(data = coh_patch_combo_1000[coh_patch_combo_1000$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Cohesion (1000)"
  ) +
  theme_minimal()

################################################################################
#                  Total Class Area
################################################################################
lsm_c_ca(landscape, directions = 8)


################################################################################
#                 Mean of all patches in the Landscape
################################################################################
l_area_patch <- landscapemetrics::lsm_l_area_mn(patch_sim_1000)

l_area_sim <- l_area_patch |>
  mutate(type = "sim")

l_area_og <- landscapemetrics::lsm_l_area_mn(cdl_reclass)

l_area_og <- l_area_og |>
  mutate(type = "og")

l_area_combo_10 <- bind_rows(l_area_sim, l_area_og)

ggplot(l_area_combo_10, aes(x = as.factor(class), y = value, color = type)) +
  geom_point(data = l_area_combo_10[l_area_combo_10$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  stat_summary(data = l_area_combo_10[l_area_combo_10$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = l_area_combo_10[l_area_combo_10$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Patch Area (AREA_MN) with 95% CIs (1000)"
  ) +
  theme_minimal()


shdi_patch <- landscapemetrics::lsm_l_area_mn(patch_sim_10)

shdi_patch_sim <- shdi_patch |>
  mutate(type = "sim") |>
  filter(class > 0)

shdi_patch_og <- landscapemetrics::lsm_l_area_mn(cdl_reclass)

shdi_patch_og <- shdi_patch_og |>
  mutate(type = "og") |>
  filter(class > 0)

shdi_patch_combo <- bind_rows(shdi_patch_sim, shdi_patch_og)

ggplot(shdi_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  # Plot the mean for type == "og"
  geom_point(data = shdi_patch_combo[shdi_patch_combo$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  # Plot the mean and confidence intervals for type == "sim"
  stat_summary(data = shdi_patch_combo[shdi_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = shdi_patch_combo[shdi_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Patch Area (AREA_MN) with 95% CIs (100)"
  ) +
  theme_minimal()

shdi_patch <- landscapemetrics::lsm_l_area_mn(patch_sim_1000)

shdi_patch_sim <- shdi_patch |>
  mutate(type = "sim") |>
  filter(class > 0)

shdi_patch_og <- landscapemetrics::lsm_l_area_mn(cdl_reclass)

shdi_patch_og <- shdi_patch_og |>
  mutate(type = "og") |>
  filter(class > 0)

shdi_patch_combo <- bind_rows(shdi_patch_sim, shdi_patch_og)

ggplot(shdi_patch_combo, aes(x = as.factor(class), y = value, color = type)) +
  # Plot the mean for type == "og"
  geom_point(data = shdi_patch_combo[shdi_patch_combo$type == "og", ],
             aes(x = as.factor(class), y = value), color = "red", size = 3) +
  # Plot the mean and confidence intervals for type == "sim"
  stat_summary(data = shdi_patch_combo[shdi_patch_combo$type == "sim", ],
               fun.data = function(y) {
                 mean_y <- mean(y)
                 lower_ci <- mean_y - qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 upper_ci <- mean_y + qt(0.975, df = length(y) - 1) * sd(y) / sqrt(length(y))
                 return(c(ymin = lower_ci, ymax = upper_ci, y = mean_y))
               }, geom = "errorbar", width = 0.2, color = "blue") +
  stat_summary(data = shdi_patch_combo[shdi_patch_combo$type == "sim", ],
               fun = mean, geom = "point", color = "blue", size = 3) +
  labs(
    x = "Class",
    y = "Value",
    title = "Mean Patch Area (AREA_MN) with 95% CIs (1000)"
  ) +
  theme_minimal()


################################################################################
#               See if any pixels are getting dropped
################################################################################

# get  a patched raster

patch_test <- find_patches(cdl_reclass)

patch_2_layers <- collapse_and_combined(patched_raster = patch_test,
                                        og_raster = cdl_reclass)




################################################################################
#               Logical Mask to Compare Rasters
################################################################################

# Make conf mat with 1s on diagonal
n <- 5
conf_mat <- matrix(0, nrow = n, ncol = n)
diag(conf_mat) <- 1

# Assign row and column names
rownames(conf_mat) <- c("0", "1", "2", "3", "4")
colnames(conf_mat) <- c("0", "1", "2", "3", "4")
conf_mat

patch_sim_1 <- simulate_raster_patch(original_raster = cdl_reclass,
                                        transition_matrix = conf_mat,
                                        iterations = 1)


mismatch_mask <- cdl_reclass != patch_sim_1
sum(values(mismatch_mask))

mismatch_raster <- classify(mismatch_mask, cbind(NA, 1))  # Set mismatches to 1 and others to 0
plot(mismatch_raster, main = "Mismatched Pixels")


mean_area_reclass <-lsm_c_area_mn(cdl_reclass)
mean_area_patched <- lsm_c_area_mn(patch_sim_1)

mean_original <- mean_area_reclass |>
  arrange(value, ascending = TRUE)

mean_simulated <- mean_area_patched |>
  arrange(value, ascending = TRUE)

mean_original
mean_simulated

shdi_reclass <- lsm_l_shdi(cdl_reclass)
shdi_patched <- lsm_l_shdi(patch_sim_1)

shdi_reclass
shdi_patched



################################################################################
#               Redo with Simplified Raster
################################################################################
## create raster with three classes (patches of all the same size)
library(terra)
library(dplyr)
# Define raster dimensions and classes
ncol <- 90
nrow <- 90
classes <- 3

# Initialize a SpatRaster with the desired dimensions
r <- rast(ncol = ncol, nrow = nrow, vals = NA)

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

# Visualize the raster
plot(r, main = "90x90 Raster with 3 Classes in 30x30 Blocks")

# Set parameters
n_classes <- 3        # Number of classes
square_size <- 30     # Each square is 30x30 pixels
n_squares <- 9        # 9x9 grid of squares

# Calculate raster dimensions
ncol <- square_size * n_squares   # Width to fit 9 squares across
nrow <- square_size * n_squares   # Height to fit 9 squares down

# Create an empty raster
r <- rast(ncol = ncol, nrow = nrow)
values(r) <- NA  # Initialize with NA values

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
    r[row_start:row_end, col_start:col_end] <- class_value
  }
}

# Plot to visualize
rbig <- r

################################################################################
#               Create Simulations with 10, 100, 1000 iterations
################################################################################
set.seed(NULL)
# Define confusion matrix, background not transitioning
conf_mat <- matrix(1/3, nrow = 3, ncol = 3)

# Assign row and column names
rownames(conf_mat) <- c( "1", "2", "3")
colnames(conf_mat) <- c( "1", "2", "3")
conf_mat

set.seed(1891)
simple_sim_1000 <- simulate_raster_patch(original_raster = r,
                                        transition_matrix = conf_mat,
                                        iterations = 1000)

irreg_sim_1000 <- simulate_raster_patch(original_raster = simple_sim_1000[[6]],
                                          transition_matrix = conf_mat,
                                          iterations = 1000)

rbig_sim_1000 <- simulate_raster_patch(original_raster = rbig,
                                        transition_matrix = conf_mat,
                                        iterations = 1000)

plot(rbig)
################################################################################
#                          10/31 Quantile Plots
################################################################################
################################################################################
#                  Shannon's Diversity Index
################################################################################
library(landscapemetrics)
library(ggplot2)
library(stats)

## Test shannon's on  most diverse landscape
################################################################################
shdi_patch <- landscapemetrics::lsm_l_sidi(irreg_sim_1000)
interval <- quantile(shdi_patch$value, c(0.025, 0.5, 0.975))
shdi_patch_sim <- as_tibble(interval) |>
  mutate(type = "sim",
         quantile = c(2.5, 50, 97.5))

shdi_patch_og <- landscapemetrics::lsm_l_sidi(simple_sim_1000[[6]]) |>
  mutate(type = "og") |>
  select(value, type)

shidi_1000 <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  mutate(grid = "small")

# Plot
ggplot(shidi_1000, aes(x = type, y = value)) +
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | is.na(quantile)), aes(color = type)) +
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = min(value[quantile == 2.5]), ymax = max(value[quantile == 97.5])),
                width = 0.2) +
  labs(x = "Type", y = "Value") +
  ggtitle("Simulation with Original at Max SHDI") +
  theme_minimal() +
  scale_y_continuous(limits = c(0.0, 1.3)) +
  theme(legend.position = "none")

plot(simple_sim_1000[[6]])

## Test shannon's on  less diverse landscape
################################################################################
shdi_patch <- landscapemetrics::lsm_l_shdi(irreg_sim_1000)
interval <- quantile(shdi_patch_sim$value, c(0.025, 0.5, 0.975))
shdi_patch_sim <- as_tibble(interval) |>
  mutate(type = "sim",
         quantile = c(2.5, 50, 97.5))
mean(shdi_patch_sim$value)

shdi_patch_og <- landscapemetrics::lsm_l_shdi(simple_sim_1000[[6]]) |>
  mutate(type = "og") |>
  select(value, type)

shidi_1000_r <- bind_rows(shdi_patch_sim, shdi_patch_og)|>
  mutate(grid = "irreg")


# Plot
ggplot(shidi_1000, aes(x = type, y = value)) +
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | is.na(quantile)), aes(color = type)) +
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = min(value[quantile == 2.5]), ymax = max(value[quantile == 97.5])),
                width = 0.2) +
  labs(x = "Type", y = "Value") +
  ggtitle("Simulation with Original at Lower SHDI") +
  theme_minimal() +
  scale_y_continuous(limits = c(0.02, 1.3)) +
  theme(legend.position = "none")

## Test shannon's on larger most diverse
################################################################################
shdi_patch <- landscapemetrics::lsm_l_sidi(rbig_sim_1000)
interval <- quantile(shdi_patch$value, c(0.025, 0.5, 0.975))
shdi_patch_sim <- as_tibble(interval) |>
  mutate(type = "sim",
         quantile = c(2.5, 50, 97.5))
mean(shdi_patch_sim$value)

shdi_patch_og <- landscapemetrics::lsm_l_sidi(rbig) |>
  mutate(type = "og") |>
  select(value, type)

shidi_1000 <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  mutate(grid = "large")


# Plot
ggplot(shidi_1000, aes(x = type, y = value)) +
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | is.na(quantile)), aes(color = type)) +
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = min(value[quantile == 2.5]), ymax = max(value[quantile == 97.5])),
                width = 0.2) +
  labs(x = "Type", y = "Value") +
  ggtitle("Simulation vs Original at Max SIDI (9 by 9)") +
  theme_minimal() +
  scale_y_continuous(limits = c(0.02, 1.3)) +
  theme(legend.position = "none")

sidi_all <- bind_rows(shidi_1000_s, shidi_1000_r, shidi_1000_g)


################################################################################
#                 Simpson's Diversity Index
################################################################################
## Test shannon's on  most diverse landscape
################################################################################
shdi_patch <- landscapemetrics::lsm_l_sidi(simple_sim_1000)
interval <- quantile(shdi_patch$value, c(0.025, 0.5, 0.975))
shdi_patch_sim <- as_tibble(interval) |>
  mutate(type = "sim",
         quantile = c(2.5, 50, 97.5))

shdi_patch_og <- landscapemetrics::lsm_l_sidi(r) |>
  mutate(type = "og") |>
  select(value, type)

shidi_1000_s <- bind_rows(shdi_patch_sim, shdi_patch_og)|>
  mutate(grid = "small")

# Plot
ggplot(shidi_1000, aes(x = type, y = value)) +
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | is.na(quantile)), aes(color = type)) +
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = min(value[quantile == 2.5]), ymax = max(value[quantile == 97.5])),
                width = 0.2) +
  labs(x = "Type", y = "Value") +
  ggtitle("Simulation with Original at Max SIDI") +
  theme_minimal() +
  scale_y_continuous(limits = c(0.02, 1.3)) +
  theme(legend.position = "none")

## Test shannon's on  less diverse landscape
################################################################################
shdi_patch <- landscapemetrics::lsm_l_sidi(irreg_sim_1000)
interval <- quantile(shdi_patch_sim$value, c(0.025, 0.5, 0.975))
shdi_patch_sim <- as_tibble(interval) |>
  mutate(type = "sim",
         quantile = c(2.5, 50, 97.5))
mean(shdi_patch_sim$value)

shdi_patch_og <- landscapemetrics::lsm_l_sidi(simple_sim_1000[[6]]) |>
  mutate(type = "og") |>
  select(value, type)

shidi_1000_r <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  mutate(grid = "irreg")


# Plot
ggplot(shidi_1000, aes(x = type, y = value)) +
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | is.na(quantile)), aes(color = type)) +
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = min(value[quantile == 2.5]), ymax = max(value[quantile == 97.5])),
                width = 0.2) +
  labs(x = "Type", y = "Value") +
  ggtitle("Simulation with Original at Lower SIDI") +
  theme_minimal() +
  scale_y_continuous(limits = c(0.02, 1.3)) +
  theme(legend.position = "none")

## Test shannon's on larger most diverse
################################################################################
shdi_patch <- landscapemetrics::lsm_l_sidi(rbig_sim_1000)
interval <- quantile(shdi_patch$value, c(0.025, 0.5, 0.975))
shdi_patch_sim <- as_tibble(interval) |>
  mutate(type = "sim",
         quantile = c(2.5, 50, 97.5))
mean(shdi_patch_sim$value)

shdi_patch_og <- landscapemetrics::lsm_l_sidi(rbig) |>
  mutate(type = "og") |>
  select(value, type)

shidi_1000_g <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  mutate(grid = "large")


# Plot
ggplot(shidi_1000, aes(x = type, y = value)) +
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | is.na(quantile)), aes(color = type)) +
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = min(value[quantile == 2.5]), ymax = max(value[quantile == 97.5])),
                width = 0.2) +
  labs(x = "Type", y = "Value") +
  ggtitle("Simulation vs Original at Max SIDI (9 by 9)") +
  theme_minimal() +
  scale_y_continuous(limits = c(0.02, 1.3)) +
  theme(legend.position = "none")

sidi_all <- bind_rows(shidi_1000_s, shidi_1000_r, shidi_1000_g)


################################################################################
#       Test Diversity and Mean Patch Area on BL
################################################################################
library(dplyr)
library(terra)
library(ggplot2)

## load 2008 example data
cdl_2008 <- terra::rast("inst/extdata/bl_cdl_90m.tif")

# first crop data to smaller watershed unit around Bear Lake
# load in the watershed polygon
bl_watershed <- sf::st_read("C:/Users/A02425259/Git/LandUseBL/shapefiles")
bl_watershed_trans <- sf::st_transform(bl_watershed, crs = sf::st_crs(cdl_2008))

# subset sbu watershed to just include Bear Lake polygon
bl_sub <- bl_watershed_trans %>%
  dplyr::filter(name == "Bear Lake")

# transform to the correct CRS and crop the CDL data to this area
bl_sub_raster <- mask(cdl_2008, bl_sub)
bl_sub_raster <- crop(bl_sub_raster, bl_sub)

#### Reclassify the rasters to major ag categories
# Non-ag values
ag_mask <- c(
  61:65, 81:83, 87:88, 92, 111:112, 121:124,
  131, 141:143, 152, 176, 181, 190, 195
)
# alfalfa and other hay
alfalfa <- c(36:37)
# corn, soy, cotton, wheat (and mixed categories)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)

# reclassify into background = 0, major-ag = 1, alfalfa = 2, non-ag = 3, gen_ag = 4
cdl_reclass <- terra::app(bl_sub_raster, fun = function(x) {
  ifelse(is.na(x), 0,
         ifelse(x %in% major_ag, 1,
                ifelse(x %in% alfalfa, 2,
                       ifelse(x %in% ag_mask, 3, 4)
                )
         )
  )
})

plot(cdl_reclass)

# Define confusion matrix, background not transitioning
conf_mat <- matrix(1/4, nrow = 5, ncol = 5)
conf_mat[1, ] <- c(1, 0, 0, 0, 0)
conf_mat[, 1] <- c(1, 0, 0, 0, 0)

# Assign row and column names
rownames(conf_mat) <- c("0", "1", "2", "3", "4")
colnames(conf_mat) <- c("0", "1", "2", "3", "4")
conf_mat


simple_sim_100 <- simulate_raster_patch(original_raster = cdl_reclass,
                                         transition_matrix = conf_mat,
                                         iterations = 100)

bl_sim_1000 <- simulate_raster_patch(original_raster = cdl_reclass,
                                        transition_matrix = conf_mat,
                                        iterations = 1000)

###############################################################################
##      Test shannon's
################################################################################
shdi_patch <- landscapemetrics::lsm_l_shdi(bl_sim_1000)
interval <- quantile(shdi_patch$value, c(0.025, 0.5, 0.975))
shdi_patch_sim <- as_tibble(interval) |>
  mutate(type = "sim",
         quantile = c(2.5, 50, 97.5))
mean(shdi_patch_sim$value)

shdi_patch_og <- landscapemetrics::lsm_l_shdi(cdl_reclass) |>
  mutate(type = "og") |>
  select(value, type)

shdi_bl <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  mutate(grid = "large")


# Plot
ggplot(shdi_bl, aes(x = type, y = value)) +
  geom_point(data = shdi_bl %>% filter(quantile == 50 | is.na(quantile)), aes(color = type)) +
  geom_errorbar(data = shdi_bl %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = min(value[quantile == 2.5]), ymax = max(value[quantile == 97.5])),
                width = 0.2) +
  labs(x = "Type", y = "Value") +
  ggtitle("CDL Reclass Simulation Shannon's (1000)") +
  theme_minimal() +
  scale_y_continuous(limits = c(0.5, 1.1)) +
  theme(legend.position = "none")


################################################################################
#                 Simpson's Diversity Index
################################################################################
shdi_patch <- landscapemetrics::lsm_l_sidi(bl_sim_1000)
interval <- quantile(shdi_patch$value, c(0.025, 0.5, 0.975))
shdi_patch_sim <- as_tibble(interval) |>
  mutate(type = "sim",
         quantile = c(2.5, 50, 97.5))

shdi_patch_og <- landscapemetrics::lsm_l_sidi(cdl_reclass) |>
  mutate(type = "og") |>
  select(value, type)

shidi_1000 <- bind_rows(shdi_patch_sim, shdi_patch_og)|>
  mutate(grid = "small")

# Plot
ggplot(shidi_1000, aes(x = type, y = value)) +
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | is.na(quantile)), aes(color = type)) +
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = min(value[quantile == 2.5]), ymax = max(value[quantile == 97.5])),
                width = 0.2) +
  labs(x = "Type", y = "Value") +
  ggtitle("CDL Reclass Simulation Simpson's (1000)") +
  theme_minimal() +
  scale_y_continuous(limits = c(0.5, 0.6)) +
  theme(legend.position = "none")

################################################################################
#                 Mean Patch Area
################################################################################
shdi_patch <- landscapemetrics::lsm_c_area_mn(bl_sim_1000)
interval <- shdi_patch |>
  group_by(class) |>
  summarize(quants = quantile(value, c(0.025, 0.5, 0.975)))
shdi_patch_sim <- as_tibble(interval) |>
  mutate(type = "sim",
         quantile = rep(c(2.5, 50, 97.5), times =  5))

shdi_patch_og <- landscapemetrics::lsm_c_area_mn(cdl_reclass) |>
  mutate(type = "og",
         quants = value) |>
  select(class, quants, type)

shidi_1000 <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  mutate(metric = rep("mean_patch", times = length(class)))
  filter(class == 1)

# Plot
p1 <- ggplot(shidi_1000, aes(x = type, y = quants, color = type)) +
  # Plot original (og) values and median (50th quantile) simulated values only if value is not NA
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | type == "og"), size = 3) +
  # Add error bars for the simulation values (2.5 and 97.5 quantiles)
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = quants[1],
                    ymax = quants[2]),
                width = 0.2) +
  # Facet by class to separate each class into its own subplot
  #facet_wrap(~ class, scales = "free_y") +
  labs(x = shidi_1000$class, y = "Value") +
  ggtitle("Mean Patch Area") +
  theme_minimal() +
  scale_y_continuous(limits = c(-50, 3500)) +
  theme(legend.position = "none")

shidi_1000 <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  filter(class == 2)

# Plot
p2 <- ggplot(shidi_1000, aes(x = type, y = quants, color = type)) +
  # Plot original (og) values and median (50th quantile) simulated values only if value is not NA
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | type == "og"), size = 3) +
  # Add error bars for the simulation values (2.5 and 97.5 quantiles)
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = quants[1],
                    ymax = quants[2]),
                width = 0.2) +
  # Facet by class to separate each class into its own subplot
  #facet_wrap(~ class, scales = "free_y") +
  labs(x = shidi_1000$class, y = "Value") +
  ggtitle("Mean Patch Area") +
  theme_minimal() +
  scale_y_continuous(limits = c(-50, 3500)) +
  theme(legend.position = "none")

shidi_1000 <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  filter(class == 3)

# Plot
p3 <- ggplot(shidi_1000, aes(x = type, y = quants, color = type)) +
  # Plot original (og) values and median (50th quantile) simulated values only if value is not NA
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | type == "og"), size = 3) +
  # Add error bars for the simulation values (2.5 and 97.5 quantiles)
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = quants[1],
                    ymax = quants[2]),
                width = 0.2) +
  # Facet by class to separate each class into its own subplot
  #facet_wrap(~ class, scales = "free_y") +
  labs(x = shidi_1000$class, y = "Value") +
  ggtitle("Mean Patch Area") +
  theme_minimal() +
  scale_y_continuous(limits = c(-50, 3500)) +
  theme(legend.position = "none")
shidi_1000 <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  filter(class == 4)

# Plot
p4 <- ggplot(shidi_1000, aes(x = type, y = quants, color = type)) +
  # Plot original (og) values and median (50th quantile) simulated values only if value is not NA
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | type == "og"), size = 3) +
  # Add error bars for the simulation values (2.5 and 97.5 quantiles)
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = quants[1],
                    ymax = quants[2]),
                width = 0.2) +
  # Facet by class to separate each class into its own subplot
  #facet_wrap(~ class, scales = "free_y") +
  labs(x = shidi_1000$class, y = "Value") +
  ggtitle("Mean Patch Area") +
  theme_minimal() +
  scale_y_continuous(limits = c(-50, 3500)) +
  theme(legend.position = "none")

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)


################################################################################
#                  Pland
################################################################################
shdi_patch <- landscapemetrics::lsm_c_pland(simple_sim_100)
interval <- shdi_patch |>
  group_by(class) |>
  summarize(quants = quantile(value, c(0.025, 0.5, 0.975)))
shdi_patch_sim <- as_tibble(interval) |>
  mutate(type = "sim",
         quantile = rep(c(2.5, 50, 97.5), times =  5))

shdi_patch_og <- landscapemetrics::lsm_c_pland(cdl_reclass) |>
  mutate(type = "og",
         quants = value) |>
  select(class, quants, type)

shidi_1000 <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  mutate(metric = rep("pland", times = length(class))) |>
filter(class == 1)

# Plot
p1 <- ggplot(shidi_1000, aes(x = type, y = quants, color = type)) +
  # Plot original (og) values and median (50th quantile) simulated values only if value is not NA
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | type == "og"), size = 3) +
  # Add error bars for the simulation values (2.5 and 97.5 quantiles)
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = quants[1],
                    ymax = quants[2]),
                width = 0.2) +
  # Facet by class to separate each class into its own subplot
  #facet_wrap(~ class, scales = "free_y") +
  labs(x = shidi_1000$class, y = "Value") +
  ggtitle("PLAND") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100)) +
  theme(legend.position = "none")

shidi_1000 <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  filter(class == 2)

# Plot
p2 <- ggplot(shidi_1000, aes(x = type, y = quants, color = type)) +
  # Plot original (og) values and median (50th quantile) simulated values only if value is not NA
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | type == "og"), size = 3) +
  # Add error bars for the simulation values (2.5 and 97.5 quantiles)
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = quants[1],
                    ymax = quants[2]),
                width = 0.2) +
  # Facet by class to separate each class into its own subplot
  #facet_wrap(~ class, scales = "free_y") +
  labs(x = shidi_1000$class, y = "Value") +
  ggtitle("PLAND") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100)) +
  theme(legend.position = "none")

shidi_1000 <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  filter(class == 3)

# Plot
p3 <- ggplot(shidi_1000, aes(x = type, y = quants, color = type)) +
  # Plot original (og) values and median (50th quantile) simulated values only if value is not NA
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | type == "og"), size = 3) +
  # Add error bars for the simulation values (2.5 and 97.5 quantiles)
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = quants[1],
                    ymax = quants[2]),
                width = 0.2) +
  # Facet by class to separate each class into its own subplot
  #facet_wrap(~ class, scales = "free_y") +
  labs(x = shidi_1000$class, y = "Value") +
  ggtitle("PLAND") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100)) +
  theme(legend.position = "none")
shidi_1000 <- bind_rows(shdi_patch_sim, shdi_patch_og) |>
  filter(class == 4)

# Plot
p4 <- ggplot(shidi_1000, aes(x = type, y = quants, color = type)) +
  # Plot original (og) values and median (50th quantile) simulated values only if value is not NA
  geom_point(data = shidi_1000 %>% filter(quantile == 50 | type == "og"), size = 3) +
  # Add error bars for the simulation values (2.5 and 97.5 quantiles)
  geom_errorbar(data = shidi_1000 %>% filter(type == "sim" & quantile %in% c(2.5, 97.5)),
                aes(ymin = quants[1],
                    ymax = quants[2]),
                width = 0.2) +
  # Facet by class to separate each class into its own subplot
  #facet_wrap(~ class, scales = "free_y") +
  labs(x = shidi_1000$class, y = "Value") +
  ggtitle("PLAND") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100)) +
  theme(legend.position = "none")

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

