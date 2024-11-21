### Create figure for proposal/report

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

edge_1 <- simulate_raster_rcpp(r, transition_matrix, iterations = 3)
par(mfrow = c(2, 2))
plot(edge_1, main = "Simulation")
plot(r, main = "Original")

par(mfrow = c(2, 2))
# Plot the original object
plot(r, main = "Original")

# Plot the first object from the simulation
plot(edge_1[[1]], main = "Simulation 1")

# Plot the second object from the simulation
plot(edge_1[[2]], main = "Simulation 2")

# Plot the third object from the simulation
plot(edge_1[[3]], main = "Simulation 3")

edge_3 <- simulate_raster_rcpp(r, transition_matrix, iterations = 3, edge_depth = 3)
edge_20 <- simulate_raster_rcpp(r, transition_matrix, iterations = 3, edge_depth = 20)
edge_120 <- simulate_raster_rcpp(r, transition_matrix, iterations = 3, edge_depth = 120)
plot(edge_20)

par(mfrow = c(3, 3))

# Plot the first object from the simulation
plot(edge_3[[1]], main = "Simulation 1")

# Plot the second object from the simulation
plot(edge_3[[2]], main = "Simulation 2")

# Plot the third object from the simulation
plot(edge_3[[3]], main = "Simulation 3")

# Plot the first object from the simulation
plot(edge_20[[1]], main = "Simulation 1")

# Plot the second object from the simulation
plot(edge_20[[2]], main = "Simulation 2")

# Plot the third object from the simulation
plot(edge_20[[3]], main = "Simulation 3")

# Plot the first object from the simulation
plot(edge_120[[1]], main = "Simulation 1")

# Plot the second object from the simulation
plot(edge_120[[2]], main = "Simulation 2")

# Plot the third object from the simulation
plot(edge_120[[3]], main = "Simulation 3")



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
sim_bl_3 <- simulate_raster_rcpp(ag_raster, iterations = 3, transition_matrix = trans_mat_2008)

par(mfrow = c(2, 2))

# Plot the first object from the simulation
plot(ag_raster, main = "Original")

# Plot the second object from the simulation
plot(sim_bl_3[[2]], main = "Simulation 1")

# Plot the third object from the simulation
plot(sim_bl_3[[3]], main = "Simulation 2")

# Plot the first object from the simulation
plot(sim_bl_3[[1]], main = "Simulation 3")

# frequency plots kind of hard to look at
og_freq <- freq(ag_raster)
sim_freq <- freq(sim_bl_3)

og_freq
sim_freq


###### Scatter Plot of mean patch area

# mean patch area (in hectares)
# The metric summarizes each class as the mean of all patch areas belonging to class i.
mean_patch_area <- lsm_c_area_mn(sim_terra_cpp_100)

og_mean_area <- lsm_c_area_mn(ag_raster_all[[1]])
og_mean_area

ggplot(mean_patch_area, aes(x = layer, y = value)) +
  geom_point(color = "blue") +  # Set all points to blue
  facet_wrap(~ class, scales = "free") +
  labs(
    x = "Class",
    y = "Hectares",
    title = "Mean Patch Area by Class"
  ) +
  theme_minimal() +
  # Add horizontal lines from 'og_mean_area'
  geom_hline(data = og_mean_area,
             aes(yintercept = value),
             color = "darkorange", linetype = "dashed", size = 1)

# total edge length
# The metric summarizes the total edge length in meters for each class
total_edge <- lsm_c_te(sim_terra_cpp_100)

og_total_edge <- lsm_c_te(ag_raster_all[[1]])

ggplot(total_edge, aes(x = layer, y = value)) +
  geom_point(color = "blue") +  # Set all points to blue
  facet_wrap(~ class, scales = "free") +
  labs(
    x = "Class",
    y = "Meters",
    title = "Total Edge Length by Class"
  ) +
  theme_minimal() +
  # Add horizontal lines from 'og_mean_area'
  geom_hline(data = og_total_edge,
             aes(yintercept = value),
             color = "darkorange", linetype = "dashed", size = 1)




library(gridExtra)
library(grid)
library(png)

# Create a data frame
table_data <- data.frame(
  Class = c("Background", "Non-Ag", "Alfalfa", "Major Ag", "Ag"),
  Values = c(
    "0",
    "61-65, 81-83, 87-88, 92, 111-112, 121-124, 131, 141-143, 152, 176, 181, 190, 195",
    "36-37",
    "1, 2, 5, 12, 13, 22-24, 26, 225-226, 228, 234, 236, 238-241, 254",
    "All other numbers from 1-256, excluding those listed in Background, Non-Ag, Alfalfa, and Major Ag"
  )
)

# Create the table grob (graphical object)
table_grob <- tableGrob(table_data)

# Save the table as a PNG file
png("classification_table.png", width = 600, height = 200)
grid.draw(table_grob)
dev.off()



#############################################################################
#             Try the simulation at pixel level before aggregating
##############################################################################
## Get just bear lake sub data
# Starting from raw spatrasters
bl_cdl_all <- terra::rast("C:/Users/A02425259/Git/LandUseBL/Intermediate_rasters/bl_crop.tif")

# first crop data to smaller watershed unit around Bear Lake
# load in the watershed polygon
bl_watershed <- st_read("C:/Users/A02425259/Git/LandUseBL/shapefiles")
bl_watershed_trans <- st_transform(bl_watershed, crs = st_crs(bl_cdl_all_90[[1]]))

# subset sbu watershed to just include Bear Lake polygon
bl_sub <- bl_watershed_trans %>%
  filter(name == "Bear Lake")

# transform to the correct CRS and crop the CDL data to this area
bl_sub_raster <- mask(bl_cdl_all_90, bl_sub)
bl_sub_raster <- crop(bl_sub_raster, bl_sub)
bl_sub_2012 <- bl_sub_raster[[5]]

## Make transition matrix for 2012
UT <- get_mat_data("UT")
ID <- get_mat_data("ID")
WY <- get_mat_data("WY")

df_trans <- as.data.frame(UT[[5]])

# Calculate total and proportions
df_trans <- df_trans %>%
  mutate(total = rowSums(across(everything()))) %>%
  mutate(across(-total, ~ . / total)) %>%
  replace(is.na(.), 0)

# Remove the last column that represented the totals
df_trans <- df_trans[, -ncol(df_trans)]

# Update row/col names to be the negative value of the row number
rownames(df_trans) <- as.numeric(-(1:nrow(df_trans)))
colnames(df_trans) <- as.numeric(1:ncol(df_trans))

## try simulating just 2012
test_raster <- terra::app(bl_sub_2012, fun = function(x) {
  ifelse(is.na(x), 0, x)
})

test_sim_pix <- simulate_raster_rcpp(test_raster, iterations = 2, transition_matrix = df_trans)

plot(test_sim_pix)

## then aggregating to 90m
sim_test_90 <- terra::aggregate(test_sim_pix, fact = 3, fun = "modal")
test_raster_90  <- terra::aggregate(test_raster, fact = 3, fun = "modal")

plot(is.na(sim_test_90[[1]]))

lsm_c_area_mn(sim_test_90)
lsm_c_area_mn(test_raster_90)

# Non-ag values
background <- 0
ag_mask <- c(
  61:65, 81:83, 87:88, 92, 111:112, 121:124,
  131, 141:143, 152, 176, 181, 190, 195
)
# alfalfa and other hay
alfalfa <- c(36:37)
# corn, soy, cotton, wheat (and mixed categories)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)

# reclassify into background = 0, major-ag = 1, alfalfa = 2, non-ag = 3, gen_ag = 4
sim_reclass <- terra::app(sim_test_90, fun = function(x) {
  ifelse(is.na(x), NA,
      ifelse(x %in% background, 0,
          ifelse(x %in% major_ag, 1,
                ifelse(x %in% alfalfa, 2,
                       ifelse(x %in% ag_mask, 3, 4)
                )
         )
    )
  )
})


# Define confusion matrix, background not transitioning
conf_mat <- matrix(1/5, nrow = 5, ncol = 5)
conf_mat[1, ] <- c(1, 0, 0, 0, 0)
conf_mat[, 1] <- c(1, 0, 0, 0, 0)

# Assign row and column names
rownames(conf_mat) <- c("0", "1", "2", "3", "4")
colnames(conf_mat) <- c("0", "1", "2", "3", "4")
conf_mat


patch_sim <- simulate_raster_patch(original_raster = og_reclass,
                                   transition_matrix = conf_mat,
                                   iterations = 10)
plot(patch_sim[[1:4]])

plot(og_reclass)
