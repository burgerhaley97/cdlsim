#### Bear Lake Slide Land Use Change


extract_and_combine_metric <- function(data_list, metric_name) {
  combined_df <- bind_rows(lapply(data_list, function(sublist) {
    bind_rows(sublist) %>%
      filter(metric == metric_name)
  }), .id = "year_idx")

  return(combined_df)
}


# functions to calculate:

func_list_slides <- list(lsm_c_area_mn, lsm_c_cohesion)

func_list_slides <- list(lsm_c_pland, lsm_c_clumpy,lsm_l_area_mn, lsm_c_lpi)

func_list_slides <- list(lsm_l_area_mn, lsm_l_np, lsm_l_pd, lsm_l_ta,
                         lsm_l_shei,lsm_l_sidi, lsm_c_te, lsm_c_mesh)
func_list_slides <- list(lsm_c_core_mn)

pland_ag <- list(lsm_c_pland)

# apply the functions to each year in our list of ag data
slide_funcs <- apply_functions_to_simulations(ag_raster_all, func_list_slides)
slide_funcs_1 <- apply_functions_to_simulations(ag_raster_all, func_list_slides)
slide_funcs_2 <- apply_functions_to_simulations(ag_raster_all, func_list_slides)
slide_funcs_3 <- apply_functions_to_simulations(ag_raster_all, func_list_slides)

pland <- apply_functions_to_simulations(ag_raster_all, pland_ag)

mean_patch_area <- extract_and_combine_metric(slide_funcs, "area_mn")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
mean_patch_area <- mean_patch_area %>%
  mutate(year = years[as.numeric(year_idx)])

area_metrics_sub <- mean_patch_area %>%
  filter(class %in% c(1, 2, 3, 4))
area_metrics_sub

class_labels <- c("0" = "background", "1" = "Non-Ag", "2" = "Gen-Ag", "3" = "Alfalfa", "4" = "Major-Ag")
ggplot(area_metrics_sub, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area by Class",
       x = "Year",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange", "black"), labels = class_labels) +
  theme_minimal()


class_labels <- c("0" = "background", "1" = "Non-Ag", "2" = "Gen-Ag", "3" = "Alfalfa", "4" = "Major-Ag")
ggplot(area_metrics_sub, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area by Class",
       x = "Year",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange", "black"), labels = class_labels) +
  theme_minimal()



cohesion <- extract_and_combine_metric(slide_funcs, "cohesion")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
cohesion <- cohesion %>%
  mutate(year = years[as.numeric(year_idx)])

cohesion_sub <- cohesion %>%
  filter(class %in% c(1, 2, 3, 4))


class_labels <- c("0" = "background", "1" = "Non-Ag", "2" = "Gen-Ag", "3" = "Alfalfa", "4" = "Major-Ag")
ggplot(cohesion_sub, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Patch Cohesion",
       x = "Year",
       y = "Percent",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange", "black"), labels = class_labels) +
  theme_minimal()

pland <- extract_and_combine_metric(slide_funcs_1, "pland")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
pland <- pland %>%
  mutate(year = years[as.numeric(year_idx)])

pland_sub <- pland %>%
  filter(class %in% c(2, 3, 4))


class_labels <- c("0" = "background", "1" = "Non-Ag", "2" = "Gen-Ag", "3" = "Alfalfa", "4" = "Major-Ag")
ggplot(pland_sub, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Percent Land Cover by Class",
       x = "Year",
       y = "Percent",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange", "black"), labels = class_labels) +
  theme_minimal()

lpi <- extract_and_combine_metric(slide_funcs_1, "lpi")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
lpi <- lpi %>%
  mutate(year = years[as.numeric(year_idx)])

lpi <- lpi %>%
  filter(class %in% c(2, 3, 4))


class_labels <- c("0" = "background", "1" = "Non-Ag", "2" = "Gen-Ag", "3" = "Alfalfa", "4" = "Major-Ag")
ggplot(lpi, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Largest Patch Index",
       x = "Year",
       y = "Percent",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange", "black"), labels = class_labels) +
  theme_minimal()


clumpy <- extract_and_combine_metric(slide_funcs_1, "clumpy")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
clumpy <- clumpy %>%
  mutate(year = years[as.numeric(year_idx)])

clumpy <- clumpy %>%
  filter(class %in% c(1, 2, 3, 4))


class_labels <- c("0" = "background", "1" = "Non-Ag", "2" = "Gen-Ag", "3" = "Alfalfa", "4" = "Major-Ag")
ggplot(clumpy, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Clumpiness Index",
       x = "Year",
       y = "",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange", "black"), labels = class_labels) +
  theme_minimal()

pland <- extract_and_combine_metric(pland, "pland")
# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(pland$year_idx)))
mean_patch_area <- pland %>%
  mutate(year = years[as.numeric(year_idx)])


class_labels <- c("1" = "Non-Ag", "2" = "Ag")
ggplot(mean_patch_area, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Percent Land Cover",
       x = "Year",
       y = "Percent",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "orange"), labels = class_labels) +
  theme_minimal()

# lsm_l_area_mn, lsm_l_np, lsm_l_pd, lsm_l_ta,
# lsm_l_shei,lsm_l_sidi, lsm_c_te, lsm_c_mesh

l_area_mn <- extract_and_combine_metric(slide_funcs_2, "area_mn")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
l_area_mn <- l_area_mn %>%
  mutate(year = years[as.numeric(year_idx)])


ggplot(l_area_mn, aes(x = as.numeric(year), y = value)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Mean Patch Area of All Classes",
       x = "Year",
       y = "Area (hectares)") +
  theme_minimal()

ta <- extract_and_combine_metric(slide_funcs_2, "ta")

np <- extract_and_combine_metric(slide_funcs_2, "np")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
np <- np %>%
  mutate(year = years[as.numeric(year_idx)])


ggplot(np , aes(x = as.numeric(year), y = value)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Total Number of Patches",
       x = "Year",
       y = "") +
  theme_minimal()

pd <- extract_and_combine_metric(slide_funcs_2, "pd")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
pd <- pd %>%
  mutate(year = years[as.numeric(year_idx)])


ggplot(pd , aes(x = as.numeric(year), y = value)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Patch Density",
       x = "Year",
       y = "Patches per 100 Hectares") +
  theme_minimal()

sidi <- extract_and_combine_metric(slide_funcs_2, "sidi")
shei <- extract_and_combine_metric(slide_funcs_2, "shei")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
sidi <- sidi %>%
  mutate(year = years[as.numeric(year_idx)])
shei <- shei %>%
  mutate(year = years[as.numeric(year_idx)])

ggplot(sidi, aes(x = as.numeric(year), y = value)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Simpson's Diversity Index",
       x = "Year",
       y = "") +
  theme_minimal()

ggplot(shei, aes(x = as.numeric(year), y = value)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Shannon's ",
       x = "Year",
       y = "") +
  theme_minimal()

te <- extract_and_combine_metric(slide_funcs_2, "te")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
te <- te %>%
  mutate(year = years[as.numeric(year_idx)])

te <- te %>%
  filter(class %in% c(1, 2, 3, 4))


class_labels <- c("0" = "background", "1" = "Non-Ag", "2" = "Gen-Ag", "3" = "Alfalfa", "4" = "Major-Ag")
ggplot(te, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Total Edge Length by Class",
       x = "Year",
       y = " Meters",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange", "black"), labels = class_labels) +
  theme_minimal()

mesh <- extract_and_combine_metric(slide_funcs_2, "mesh")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
mesh <- mesh %>%
  mutate(year = years[as.numeric(year_idx)])

mesh <- mesh %>%
  filter(class %in% c(2, 3, 4))


class_labels <- c("0" = "background", "1" = "Non-Ag", "2" = "Gen-Ag", "3" = "Alfalfa", "4" = "Major-Ag")
ggplot(mesh, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mesh",
       x = "Year",
       y = " ",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange", "black"), labels = class_labels) +
  theme_minimal()

c_core_mn <- extract_and_combine_metric(slide_funcs_3, "core_mn")

# Adding year information based on the number of unique years in the data
years <- seq(2008, 2023, length.out = length(unique(mean_patch_area$year_idx)))
c_core_mn <- c_core_mn %>%
  mutate(year = years[as.numeric(year_idx)])

c_core_mn <- c_core_mn %>%
  filter(class %in% c(2, 3, 4))


class_labels <- c("0" = "background", "1" = "Non-Ag", "2" = "Gen-Ag", "3" = "Alfalfa", "4" = "Major-Ag")
ggplot(c_core_mn, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Core Area by Class",
       x = "Year",
       y = " Hectares ",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange", "black"), labels = class_labels) +
  theme_minimal()

###############################################################################
library(magick)
# add gif of non ag vs ag pixels over time
# Define the years
years <- 2008:2023

# Directory to save the images
dir.create("plots2")

# Loop through each layer, plot, and save as PNG
for (i in 1:nlyr(ag_raster_all)) {
  layer <- ag_raster_all[[i]]  # Extract the i-th layer
  year <- years[i]  # Corresponding year
  plot_title <- paste("Year:", year)

  # Create the plot and save it as a PNG file
  png_filename <- paste0("plots/plot_", year, ".png")
  png(png_filename)
  plot(layer, main = plot_title, col = c("gray", "blue", "green", "red", "orange"), legend = TRUE)
  dev.off()
}

# Create a GIF from the PNG files
png_files <- list.files("plots", pattern = "plot_.*\\.png", full.names = TRUE)
gif <- image_read(png_files)
gif <- image_animate(gif, fps = 1) # Set frames per second

# Save the GIF
image_write(gif, "plots/animation.gif")



# Define the class labels
class_labels <- c("0" = "background", "1" = "Non-Ag", "2" = "Gen-Ag", "3" = "Alfalfa", "4" = "Major-Ag")

# Define the colors for each class
class_colors <- c("gray", "blue", "green", "red", "orange")

# Directory to save the images
dir.create("plots2")

# Loop through each layer, plot, and save as PNG
for (i in 1:nlyr(ag_raster_all)) {
  layer <- ag_raster_all[[i]]  # Extract the i-th layer
  year <- years[i]  # Corresponding year
  plot_title <- paste("Year:", year)

  # Create the plot and save it as a PNG file
  png_filename <- paste0("plots2/plot_", year, ".png")
  png(png_filename)

  # Plot the raster with custom colors and no legend
  plot(layer, main = plot_title, col = class_colors, legend = FALSE)

  # Add a custom legend
  legend("topright", legend = class_labels, fill = class_colors, title = "Class", bty = "n")

  dev.off()
}

# Create a GIF from the PNG files
png_files <- list.files("plots2", pattern = "plot_.*\\.png", full.names = TRUE)
gif <- image_read(png_files)
gif <- image_animate(gif, fps = 1) # Set frames per second

# Save the GIF
image_write(gif, "plots2/animation.gif")

# Clean up
unlink("plots2", recursive = TRUE)

# Define the class labels and colors
class_labels <- c("0" = "Background", "1" = "Non-Ag", "2" = "Gen-Ag", "3" = "Alfalfa", "4" = "Major-Ag")
class_colors <- c("gray", "blue", "green", "red", "orange")

# Create a standalone legend plot
png("plots/legend.png", width = 400, height = 400)
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("center", legend = class_labels, fill = class_colors, title = "Class", bty = "n")
dev.off()



##### Percent Land Cover Ag vs Non-ag
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
         ifelse(x %in% major_ag, 2,
                ifelse(x %in% alfalfa, 2,
                       ifelse(x %in% ag_mask, 1, 2))))
})
