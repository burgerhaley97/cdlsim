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



######################################################################
#  Example using simple two category raster
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

# Plot the raster to visualize it
par(mfrow = c(1, 1))
plot(r, main="Raster with Two 30x30 Class 2 Squares")

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
transition_matrix

simple_rasters <- simulate_raster_changes(r, n_simulations = 5, confusion_matrix = transition_matrix)
plot(simple_rasters)

simple_counts <- freq(simple_rasters)
simple_counts[simple_counts$value == 2, ]

r_counts <- freq(r)
r_counts

# Call the function with the raster and the edge mapping
tagged_r <- tag_edges(r, edge_depth = 1)
tagged_r

# try highlighting the tagged edges to see change
negative_cells <- tagged_r < 0
plot(r, main="Raster with Edges in Red", colNA="transparent")
plot(negative_cells, add=TRUE, col=c("transparent", "red"), legend=FALSE)




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



