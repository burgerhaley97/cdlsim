#### load packages
library(sf)
library(terra)
library(tidyterra)
library(dplyr)
library(ggplot2)
library(landscapemetrics)
library(RColorBrewer)
library(stringr)

###############################################################################
#                  Import CDL Data/watershed file and Crop to AOI
###############################################################################
# Starting from raw spatrasters
# replace with you own file path to the CDL data
bl_cdl_all <- terra::rast("C:/Users/A02425259/Git/LandUseBL/Intermediate_rasters/bl_crop.tif")

# aggregate to 90m
bl_cdl_all_90 <- terra::aggregate(bl_cdl_all, fact = 3, fun = "modal")

# first crop data to smaller watershed unit around Bear Lake
# load in the watershed polygon
bl_watershed <- st_read("C:/Users/A02425259/Git/LandUseBL/shapefiles")
bl_watershed_trans <- st_transform(bl_watershed, crs = st_crs(bl_cdl_all_90[[1]]))

# subset sbu watershed to just include Bear Lake polygon
bl_sub <- bl_watershed_trans %>%
  filter(name == "Bear Lake")

# subset larger watershed to just include HUC1601
bl_greater <- bl_watershed_trans %>%
  filter(str_starts(huc8, "1601"))

# transform to the correct CRS and crop the CDL data to this area
bl_sub_raster <- mask(bl_cdl_all_90, bl_sub)
bl_sub_raster <- crop(bl_sub_raster, bl_sub)

# crop and mask the nlcd raster to the greater watershed area
cdl_raster <- mask(bl_cdl_all_90, bl_greater)
cdl_raster <- crop(cdl_raster, bl_greater)

# Save the raster to add to box folder
file_path <- "C:/Users/A02425259/Box/blw_eda/Data/rasters/CDL/cdl_raster.tif"
writeRaster(cdl_raster, file_path, overwrite = TRUE)

# Prep data frame with city names and coordinates
cities <- data.frame(
  city = c("Garden City, UT", "Montpelier, ID", "Soda Springs, ID"),
  lat = c(41.9469, 42.3221, 42.6547),
  long = c(-111.3934, -111.2977, -111.6049)
)

cities_sf <- st_as_sf(cities, coords = c("long", "lat"), crs = 4326)
cities_sf <- st_transform(cities_sf, crs = st_crs(cdl_raster[[1]]))

###############################################################################
#                       Prep Data for Non-Ag Analysis
###############################################################################
### reclassify original CDL values
crops <- c(1:60, 66:80, 196:255)
forest <- c(141:143)
developed <- c(121:124)
wetlands <- c(190, 195)
shrub_meadow <- c(176, 152)
fallow_barren <- c(61, 131)
water <- c(111)
non_ag <- c(141:143, 121:124, 190, 195, 176, 152, 61, 131, 111)

# Reclassify into background = NA, crops = 1, forest = 2, developed = 3,
# wetlands = 4, shrub_meadow = 5, fallow_barren = 6, water = 7
non_ag_raster <- terra::app(bl_sub_raster, fun = function(x) {
  ifelse(is.na(x), NA, # Keep NA as background
    ifelse(x %in% crops, 1, # Crops = 1
      ifelse(x %in% forest, 2, # Forest = 2
        ifelse(x %in% developed, 3, # Developed = 3
          ifelse(x %in% wetlands, 4, # Wetlands = 4
            ifelse(x %in% shrub_meadow, 5, # Shrub/meadow = 5
              ifelse(x %in% fallow_barren, 6, 7 # Water = 7
              )
            )
          )
        )
      )
    )
  )
})


###############################################################################
#                    Import NLCD Raster Data and Crop to AOI
###############################################################################
# NLCD data downloaded from https://www.mrlc.gov/viewer/
# Define the folder path where your NLCD files are located
# replace with you own file path to the NLCD data
folder_path <- "C:/Users/A02425259/Downloads/NLCD_AOI"

# Get the .tif file and associated .tif.vat.dbf file
tiff_file <- list.files(path = folder_path, pattern = "\\.tiff$", full.names = TRUE)

# Step 1: Load the .tif raster file using terra::rast
nlcd_raster <- rast(tiff_file[4:9])

# aggregate to 90 meters like CDL data
nlcd_raster <- terra::aggregate(nlcd_raster, fact = 3, fun = "modal")

# crop and mask the nlcd raster to the greater watershed area
bl_nlcd <- mask(nlcd_raster, bl_greater)
bl_nlcd <- crop(bl_nlcd, bl_greater)

# Save the raster to add to box folder
file_path <- "C:/Users/A02425259/Box/blw_eda/Data/rasters/NLCD/nlcd_raster.tif"
writeRaster(bl_nlcd, file_path, overwrite = TRUE)

# crop and mask the nlcd raster to the sub watershed area
bl_nlcd_sub <- mask(nlcd_raster, bl_sub)
bl_nlcd_sub <- crop(bl_nlcd_sub, bl_sub)

###############################################################################
#                     Non-Ag Analysis Prep NLCD Raster Data
###############################################################################
### reclassify original CDL values
crops <- c(81, 82)
forest <- c(41:43)
developed <- c(21:24)
wetlands <- c(90, 95)
shrub_meadow <- c(71, 52)
fallow_barren <- c(31)
water <- c(11, 12) # includes open water and perennial snow/ice

# Reclassify into background = NA, crops = 1, forest = 2, developed = 3,
# wetlands = 4, shrub_meadow = 5, fallow_barren = 6, water = 7
nlcd_reclass <- terra::app(bl_nlcd, fun = function(x) {
  ifelse(is.na(x), NA, # Keep NA as background
    ifelse(x %in% crops, 1, # Crops = 1
      ifelse(x %in% forest, 2, # Forest = 2
        ifelse(x %in% developed, 3, # Developed = 3
          ifelse(x %in% wetlands, 4, # Wetlands = 4
            ifelse(x %in% shrub_meadow, 5, # Shrub/meadow = 5
              ifelse(x %in% fallow_barren, 6, 7 # Water = 7
              )
            )
          )
        )
      )
    )
  )
})

# Reclassify into background = NA, crops = 1, forest = 2, developed = 3,
# wetlands = 4, shrub_meadow = 5, fallow_barren = 6, water = 7
nlcd_reclass_sub <- terra::app(bl_nlcd_sub, fun = function(x) {
  ifelse(is.na(x), NA, # Keep NA as background
    ifelse(x %in% crops, 1, # Crops = 1
      ifelse(x %in% forest, 2, # Forest = 2
        ifelse(x %in% developed, 3, # Developed = 3
          ifelse(x %in% wetlands, 4, # Wetlands = 4
            ifelse(x %in% shrub_meadow, 5, # Shrub/meadow = 5
              ifelse(x %in% fallow_barren, 6, 7 # Water = 7
              )
            )
          )
        )
      )
    )
  )
})

###############################################################################
#                        Plot NCLD and CDL for 2008 and 2021
###############################################################################
# subset raster for years 2008 and 2021
nlcd_rast <- subset(nlcd_reclass_sub, c(1, nlyr(nlcd_reclass_sub)))

# Assign categories (levels) to each layer of the raster
levels(nlcd_rast[[1]]) <- data.frame(ID = 0:7, "lyr3" = paste("Category", 0:7))
levels(nlcd_rast[[2]]) <- data.frame(ID = 0:7, "lyr4" = paste("Category", 0:7))

# Create a named vector to match the layer numbers with custom labels
label_vector <- setNames(custom_labels, paste0("lyr", 3:4))

# subset CDL raster for years 2008 and 2021
rast <- subset(non_ag_raster, c(1, 14))

# Assign categories (levels) to each layer of the raster
levels(rast[[1]]) <- data.frame(ID = 0:7, "lyr1" = paste("Category", 0:7))
levels(rast[[2]]) <- data.frame(ID = 0:7, "lyr2" = paste("Category", 0:7))

# Define custom labels for each layer
custom_labels <- c("2008", "2021")

# Create a named vector to match the layer numbers with custom labels
label_vector <- setNames(custom_labels, paste0("cdl_lyr", 1:2))

###  combined the CDL sub raster with NLCD
# Resample rasters to match the extent
rast_resampled <- resample(rast, nlcd_rast)
both_rasters <- c(rast_resampled, nlcd_rast)
label_vector <- setNames(rep(custom_labels, times = 2), paste0("lyr", 1:4))

# Define your categories and labels
cat_labels <- c(
  "crops", "forest", "developed", "wetlands", "shrub meadow",
  "fallow barren", "water", "background"
)

land_covers <- c(
  # "Category 0" = "#666666",
  "Category 1" = "#d95f02",
  "Category 2" = "#66a61e",
  "Category 3" = "#e7298a",
  "Category 4" = "#7570b3",
  "Category 5" = "#a6761d",
  "Category 6" = "#e6ab02",
  "Category 7" = "#1b9e77"
)

png("C:/Users/A02425259/Desktop/MeetingUpdates/BL_report/non_ag_rasters.png",
  width = 600, height = 800
)

# Plot with customized legend
ggplot() +
  geom_spatraster(data = both_rasters) +
  facet_wrap(~lyr, ncol = 2, labeller = labeller(lyr = label_vector)) +
  scale_fill_manual(values = land_covers, labels = cat_labels) +
  theme_minimal() +
  labs(title = "") +
  theme(
    legend.position = "bottom", # Position the legend at the bottom
    legend.direction = "vertical", # Make the legend vertical
    legend.box = "horizontal",
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    panel.spacing = unit(0.05, "lines"),
    axis.text = element_blank(), # Remove axis text (tick labels)
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid = element_blank(),
    legend.margin = margin(-25, 0, 0, 0),
    strip.text.x = element_text(margin = margin(t = 1, b = -1)),
    plot.margin = margin(-10, 5, 5, 5)
  ) +
  guides(fill = guide_legend(
    nrow = 3, # Three rows
    ncol = 3, # Three columns
    label.position = "right", # Labels to the right of boxes
    title = "", # Optional title for the legend
    byrow = FALSE,
    override.aes = list(size = 5)
  ))

dev.off()

###############################################################################
#               Calculate and Plot PLAND for non-ag CDL rasters
###############################################################################
### Calculate percent land cover (pland)
non_ag_pland <- lsm_c_pland(non_ag_raster)

years <- rep(2008:2023, each = 7)
pland_sub <- non_ag_pland %>%
  mutate(year = years) %>%
  filter(class %in% c(1:7))
pland_sub$class <- as.factor(pland_sub$class)

land_covers <- c(
  "1" = "#d95f02",
  "2" = "#66a61e",
  "3" = "#e7298a",
  "4" = "#7570b3",
  "5" = "#a6761d",
  "6" = "#e6ab02",
  "7" = "#1b9e77"
)

cat_labels <- c(
  "crops", "forest", "developed", "wetlands", "shrub meadow",
  "fallow barren", "water"
)

# plot pland
ggplot(pland_sub, aes(x = as.numeric(year), y = value, color = class)) +
  geom_line(size = 0.7) +
  labs(
    title = "",
    x = "Year",
    y = "Percent Land Cover"
  ) +
  scale_color_manual(values = land_covers, labels = cat_labels) +
  theme_minimal() +
  theme(
    legend.position = "bottom", # Position the legend at the bottom
    legend.direction = "vertical", # Make the legend vertical
    legend.box = "horizontal",
    axis.title = element_text(size = 14), # Adjusts axis labels size
    axis.text = element_text(size = 14), # Adjusts axis tick marks size
    legend.text = element_text(size = 14), # Adjusts legend text size
  ) +
  guides(color = guide_legend(
    nrow = 2, # Three rows
    ncol = 4, # Three columns
    label.position = "right", # Labels to the right of boxes
    title = "", # Optional title for the legend
    byrow = FALSE,
  ))


ggsave("C:/Users/A02425259/Desktop/MeetingUpdates/BL_report/pland_non_ag.png",
  width = 12, height = 8
)


###############################################################################
#                 Prep Data for Agricultural Analysis
###############################################################################
# subset larger watershed to just include HUC1601 (greater watershed)
bl_greater <- bl_watershed_trans %>%
  filter(str_starts(huc8, "1601"))
bl_ag <- mask(bl_cdl_all_90, bl_greater)
bl_ag <- crop(bl_ag, bl_greater)


# get the sub-watershed
# load in the watershed polygon
bl_sub <- bl_watershed_trans %>%
  filter(name == "Bear Lake")
bl_sub_ag <- mask(bl_cdl_all_90, bl_sub)
bl_sub_ag <- crop(bl_sub_ag, bl_sub)


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
bl_ag_reclass <- terra::app(bl_ag, fun = function(x) {
  ifelse(is.na(x), NA,
    ifelse(x %in% major_ag, 1,
      ifelse(x %in% alfalfa, 2,
        ifelse(x %in% ag_mask, 3, 4)
      )
    )
  )
})

# reclassify into background = 0, major_ag = 1, alfalfa = 2, non_ag = 3, gen_ag = 4
bl_sub_ag_reclass <- terra::app(bl_sub_ag, fun = function(x) {
  ifelse(is.na(x), NA,
    ifelse(x %in% major_ag, 1,
      ifelse(x %in% alfalfa, 2,
        ifelse(x %in% ag_mask, 3, 4)
      )
    )
  )
})


###############################################################################
#             Plot the larger and smaller watershed in 2008 & 2023
###############################################################################

### plot the larger and smaller watershed areas
# raster with just years 2008 and 2023
ag_rast <- subset(bl_ag_reclass, c(1, nlyr(bl_ag_reclass)))
ag_rast_sub <- subset(bl_sub_ag_reclass, c(1, nlyr(bl_sub_ag_reclass)))


# Assign categories (levels) to each layer of the raster
levels(ag_rast[[1]]) <- data.frame(ID = 1:4, "lyr1" = 1:4)
levels(ag_rast[[2]]) <- data.frame(ID = 1:4, "lyr2" = 1:4)

levels(ag_rast_sub[[1]]) <- data.frame(ID = 1:4, "lyr1" = 1:4)
levels(ag_rast_sub[[2]]) <- data.frame(ID = 1:4, "lyr2" = 1:4)

crop_covers <- c(
  "1" = "#d95f02",
  "2" = "#66a61e",
  "3" = "#7570b3",
  "4" = "#e6ab02"
)

class_labels <- c("major ag", "alfalfa", "non ag", "ag", "background")

# Plot with customized legend
ggplot() +
  geom_spatraster(data = ag_rast) +
  geom_sf(data = bl_sub, color = "lightgray", fill = NA, size = 0.7) +
  geom_sf(data = bl_shp, color = "blue", fill = "blue", size = 1, alpha = 0.5) +
  facet_wrap(~lyr, ncol = 2, labeller = labeller(lyr = label_vector)) +
  scale_fill_manual(values = crop_covers, labels = class_labels) +
  theme_minimal() +
  labs(title = "") +
  theme(
    legend.position = "bottom", # Position the legend at the bottom
    legend.direction = "vertical", # Make the legend vertical
    legend.box = "horizontal",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    panel.spacing = unit(0.05, "lines"),
    axis.text = element_blank(), # Remove axis text (tick labels)
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid = element_blank(),
    legend.margin = margin(-25, 0, 0, 0) # Remove grid lines (optional)
  ) +
  guides(fill = guide_legend(
    nrow = 2, # Three rows
    ncol = 3, # Three columns
    label.position = "right", # Labels to the right of boxes
    title = "", # Optional title for the legend
    byrow = FALSE,
    override.aes = list(size = 5)
  )) +
  geom_sf(data = cities_sf, col = "black", size = 2) +
  geom_sf_text(
    data = cities_sf, aes(label = city), nudge_x = -9000,
    nudge_y = 5000
  )

ggsave("C:/Users/A02425259/Desktop/MeetingUpdates/BL_report/BL_watersheds.png",
  width = 12, height = 12
)

# Plot with same parameters just the sub raster
ggplot() +
  geom_spatraster(data = ag_rast_sub) +
  geom_sf(data = bl_shp, color = "blue", fill = "blue", size = 0.5, alpha = 0.5) +
  facet_wrap(~lyr, ncol = 2, labeller = labeller(lyr = label_vector)) +
  scale_fill_manual(values = crop_covers, labels = class_labels) +
  theme_minimal() +
  labs(title = "") +
  theme(
    legend.position = "bottom", # Position the legend at the bottom
    legend.direction = "vertical", # Make the legend vertical
    legend.box = "horizontal",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 12),
    strip.text = element_text(size = 16),
    panel.spacing = unit(0.05, "lines"),
    axis.text = element_blank(), # Remove axis text (tick labels)
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid = element_blank(),
    legend.margin = margin(-25, 0, 0, 0),
    strip.text.x = element_text(margin = margin(t = -1, b = -1)),
    plot.margin = margin(0, 0, , 0)
  ) +
  guides(fill = guide_legend(
    nrow = 2, # Three rows
    ncol = 3, # Three columns
    label.position = "right", # Labels to the right of boxes
    title = "", # Optional title for the legend
    byrow = FALSE,
    override.aes = list(size = 5)
  ))

ggsave("C:/Users/A02425259/Desktop/MeetingUpdates/BL_report/BL_watersheds_sub.png",
  width = 12, height = 12
)

###############################################################################
#          Calculate and Plot PLAND for larger and smaller watershed
###############################################################################
## plot pland for watershed
ag_pland <- lsm_c_pland(bl_ag_reclass)
years <- rep(2008:2023, each = 4)
ag_pland <- ag_pland %>%
  mutate(
    year = years,
    class = as.factor(class),
    watershed_type = "Watershed"
  ) %>%
  filter(class %in% c(1, 2, 4))

## plot pland for sub-watershed
ag_sub_pland <- lsm_c_pland(bl_sub_ag_reclass)
ag_sub_pland <- ag_sub_pland %>%
  mutate(
    year = years,
    class = as.factor(class),
    watershed_type = "Sub-Watershed"
  ) %>%
  filter(class %in% c(1, 2, 4))

combined_pland <- rbind(ag_pland, ag_sub_pland)


crop_covers <- c(
  "1" = "#d95f02",
  "2" = "#66a61e",
  # "3" = "#e6ab02",
  "4" = "#7570b3"
)

class_labels <- c("1" = "major ag", "2" = "alfalfa", "4" = "ag")

# Plot both watershed's results with common legend
ggplot(combined_pland, aes(x = as.numeric(year), y = value, color = class)) +
  geom_line(size = 0.70) +
  labs(
    title = "",
    x = "Year", y = "Percent Land Cover"
  ) +
  theme_minimal() +
  scale_color_manual(values = crop_covers, labels = class_labels) +
  facet_wrap(~watershed_type, ncol = 1) + # Facet by Watershed/Sub-Watershed
  theme(
    legend.position = "bottom", # Position the legend at the bottom
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(
    keywidth = unit(0.8, "cm"), # Adjust the width of the legend key symbols
    keyheight = unit(2, "cm"),
    nrow = 1, # Arrange legend items in 1 row
    ncol = 5, # Arrange legend items in 5 columns
    label.position = "right", # Labels to the right of boxes
    title = ""
  ))


ggsave("C:/Users/A02425259/Desktop/MeetingUpdates/BL_report/pland_ag.png",
  width = 10, height = 8
)

###############################################################################
#          Calculate and Plot COHESION for larger and smaller watershed
###############################################################################
## cohesion for greater-watershed
ag_coh <- lsm_c_cohesion(bl_ag_reclass)
years <- rep(2008:2023, each = 4)
ag_coh <- ag_coh %>%
  mutate(
    year = years,
    class = as.factor(class),
    watershed_type = "Watershed"
  ) %>%
  filter(class %in% c(1:4))


## cohesion for sub-watershed
ag_sub_coh <- lsm_c_cohesion(bl_sub_ag_reclass)
ag_sub_coh <- ag_sub_coh %>%
  mutate(
    year = years,
    class = as.factor(class),
    watershed_type = "Sub-Watershed"
  ) %>%
  filter(class %in% c(1:4))

# combined to facet wrap, only agricultural categories
combined_coh <- rbind(ag_coh, ag_sub_coh)

crop_covers <- c(
  "1" = "#d95f02",
  "2" = "#66a61e",
  "3" = "#e6ab02",
  "4" = "#7570b3"
)

class_labels <- c("1" = "major ag", "2" = "alfalfa", "3" = "non ag", "4" = "ag")

# Plot both sub and greater watershed results with facet wrap
ggplot(combined_coh, aes(x = as.numeric(year), y = value, color = class)) +
  geom_line(size = 0.7) +
  labs(
    title = "",
    x = "Year", y = "Percent Cohesion"
  ) +
  theme_minimal() +
  scale_color_manual(values = crop_covers, labels = class_labels) +
  facet_wrap(~watershed_type, ncol = 1) + # Facet Watershed/Sub-Watershed
  theme(
    legend.position = "bottom", # Position the legend at the bottom
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(
    keywidth = unit(0.8, "cm"), # Adjust the width of the legend key symbols
    keyheight = unit(2, "cm"),
    nrow = 1, # Arrange legend items in 1 row
    ncol = 5, # Arrange legend items in 5 columns
    label.position = "right", # Labels to the right of boxes
    title = ""
  ))

ggsave("C:/Users/A02425259/Desktop/MeetingUpdates/BL_report/bl_coh.png",
  width = 10, height = 8
)

###############################################################################
#                  Calculate and Plot PLAND with 2 classes (Ag vs Non-ag)
###############################################################################
### Classify into non-ag vs ag for pland calculation
# Non-ag values
ag_mask <- c(
  61:65, 81:83, 87:88, 92, 111:112, 121:124,
  131, 141:143, 152, 176, 181, 190, 195
)

# reclassify into background = 0, non-ag = 1, ag = 2
two_class <- terra::app(bl_ag, fun = function(x) {
  ifelse(is.na(x), NA,
    ifelse(x %in% ag_mask, 1, 2)
  )
})

# reclassify into background = 0, non-ag = 1, ag = 2
two_class_sub <- terra::app(bl_sub_ag, fun = function(x) {
  ifelse(is.na(x), NA,
    ifelse(x %in% ag_mask, 1, 2)
  )
})

## plot pland for watershed
ag_pland_2 <- lsm_c_pland(two_class)
years <- rep(2008:2023, each = 2)
ag_pland_2 <- ag_pland_2 %>%
  mutate(
    year = years,
    class = as.factor(class),
    watershed_type = "Watershed"
  )

## plot pland for sub-watershed
ag_sub_pland_2 <- lsm_c_pland(two_class_sub)
ag_sub_pland_2 <- ag_sub_pland_2 %>%
  mutate(
    year = years,
    class = as.factor(class),
    watershed_type = "Sub-Watershed"
  )

combined_pland_2 <- rbind(ag_pland_2, ag_sub_pland_2)

# plot
crop_covers <- c(
  "1" = "#7570b3",
  "2" = "#e6ab02"
)
class_labels <- c("1" = "non ag", "2" = "ag")

# Plot both sub and greater watershed results with one common legend
ggplot(combined_pland_2, aes(x = as.numeric(year), y = value, color = class)) +
  geom_line(size = 0.70) +
  labs(
    title = "",
    x = "Year", y = "Percent Land Cover"
  ) +
  theme_minimal() +
  scale_color_manual(values = crop_covers, labels = class_labels) +
  facet_wrap(~watershed_type, ncol = 1) + # Facet Watershed/Sub-Watershed
  theme(
    legend.position = "bottom", # Position the legend at the bottom
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(
    keywidth = unit(0.8, "cm"), # Adjust the width of the legend key symbols
    keyheight = unit(2, "cm"),
    nrow = 1, # Arrange legend items in 1 row
    ncol = 5, # Arrange legend items in 5 columns
    label.position = "right", # Labels to the right of boxes
    title = ""
  ))

ggsave("C:/Users/A02425259/Desktop/MeetingUpdates/BL_report/pland_two_class.png",
  width = 12, height = 8
)







###############################################################################
####                          Extra Stuff
###############################################################################


# make a list of the names and CDL numeric code
cdl_names <- c(
  "Background", "Crops", "Fallow/Idle Cropland", "Open Water", "Developed/Open Space", "Developed/Low Intensity", "Developed/Med Intensity",
  "Developed/High Intensity", "Barren", "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrubland", "Grass/Pasture",
  "Woody Wetlands", "Herbaceous Wetlands"
)
cdl_numbs <- unique(non_ag_raster[[1]])

# Combine them into a data frame
categories <- data.frame(cdl_code = cdl_numbs, name = cdl_names, stringsAsFactors = FALSE)
colnames(categories) <- c("numeric_code", "name")


#### Look at frequency changes over time in non-ag categories

raster_stack <- non_ag_raster

# Function to calculate the frequency of each category, excluding 0 and 1
calculate_frequencies <- function(raster_layer) {
  freq_table <- freq(raster_layer) # Get the frequency of each category in the layer
  freq_table <- freq_table[!freq_table$value %in% c(0, 1), ] # Exclude categories 0 and 1
  return(freq_table)
}

# Apply the frequency calculation across all layers (time steps)
frequency_list <- lapply(1:nlyr(raster_stack), function(i) {
  freq_data <- calculate_frequencies(raster_stack[[i]])
  freq_data$time <- i # Add a time variable to track changes over time
  return(freq_data)
})

# Combine the results into a single data frame
frequency_df <- bind_rows(frequency_list)

# `pivot_longer` is used to reshape wide data to long format for easier plotting
frequency_df_long <- frequency_df %>%
  select(time, value, count) %>%
  pivot_longer(cols = count, names_to = "variable", values_to = "count")

# Plot the frequencies of each category over time, excluding 0 and 1
ggplot(frequency_df_long, aes(x = time, y = count, fill = as.factor(value))) +
  geom_col(position = "stack") +
  labs(
    title = "Category Frequency Over Time (Excluding Categories 0 and 1)",
    x = "Time", y = "Frequency", fill = "Category"
  ) +
  theme_minimal()



# Combine the plots side by side
p1 + p2 + plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) &
  labs(fill = "Categories")


###############################################################################
#                         Non-Ag Analysis at 30 meters
###############################################################################

### reclassify original CDL values
crops <- c(1:60, 66:80, 196:255)
forest <- c(141:143)
developed <- c(121:124)
wetlands <- c(190, 195)
shrub_meadow <- c(176, 152)
fallow_barren <- c(61, 131)
water <- c(111)
non_ag <- c(141:143, 121:124, 190, 195, 176, 152, 61, 131, 111)

# transform to the correct CRS and crop the CDL data to this area
bl_watershed_trans <- st_transform(bl_sub, crs = st_crs(bl_cdl_all[[1]]))
bl_sub_raster_30 <- mask(bl_cdl_all, bl_watershed_trans)
bl_sub_raster_30 <- crop(bl_sub_raster, bl_watershed_trans)

# Reclassify into background = NA, crops = 1, forest = 2, developed = 3, wetlands = 4,
# shrub_meadow = 5, fallow_barren = 6, water = 7
non_ag_30 <- terra::app(bl_sub_raster_30, fun = function(x) {
  ifelse(is.na(x), NA, # Keep NA as background
    ifelse(x %in% crops, 1, # Crops = 1
      ifelse(x %in% forest, 2, # Forest = 2
        ifelse(x %in% developed, 3, # Developed = 3
          ifelse(x %in% wetlands, 4, # Wetlands = 4
            ifelse(x %in% shrub_meadow, 5, # Shrub/meadow = 5
              ifelse(x %in% fallow_barren, 6, 7 # Water = 7
              )
            )
          )
        )
      )
    )
  )
})


### plot the smaller watershed areas 2008 & 2023
# raster with just years 2008 and 2023
rast_30 <- subset(non_ag_30, c(1, nlyr(non_ag_30)))

# Assign categories (levels) to each layer of the raster
levels(rast_30[[1]]) <- data.frame(ID = 0:7, "lyr1" = paste("Category", 0:7))
levels(rast_30[[2]]) <- data.frame(ID = 0:7, "lyr2" = paste("Category", 0:7))

# Define custom labels for each layer
custom_labels <- c("2008", "2023")

# Create a named vector to match the layer numbers with custom labels
label_vector <- setNames(custom_labels, paste0("lyr", 1:2))

# Define your categories and labels
cat_labels <- c("crops", "forest", "developed", "wetlands", "shrub_meadow", "fallow_barren", "water", "background")

land_covers <- c(
  # "Category 0" = "#666666",
  "Category 1" = "#d95f02",
  "Category 2" = "#66a61e",
  "Category 3" = "#e7298a",
  "Category 4" = "#7570b3",
  "Category 5" = "#a6761d",
  "Category 6" = "#e6ab02",
  "Category 7" = "#1b9e77"
)


# Plot with customized legend
ggplot() +
  geom_spatraster(data = rast_30) +
  facet_wrap(~lyr, ncol = 2, labeller = labeller(lyr = label_vector)) +
  scale_fill_manual(values = land_covers, labels = cat_labels) +
  theme_minimal() +
  labs(title = "") +
  theme(
    legend.position = "bottom", # Position the legend at the bottom
    legend.direction = "vertical", # Make the legend vertical
    legend.box = "horizontal",
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    panel.spacing = unit(0.05, "lines"),
    axis.text = element_blank(), # Remove axis text (tick labels)
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid = element_blank(),
    legend.margin = margin(-25, 0, 0, 0),
    strip.text.x = element_text(margin = margin(t = 1, b = -1)),
    plot.margin = margin(-10, 5, 5, 5)
  ) +
  guides(fill = guide_legend(
    nrow = 3, # Three rows
    ncol = 3, # Three columns
    label.position = "right", # Labels to the right of boxes
    title = "", # Optional title for the legend
    byrow = FALSE,
    override.aes = list(size = 5)
  ))

ggsave("C:/Users/A02425259/Desktop/MeetingUpdates/BL_report/non_ag_rasters.png", width = 12, height = 12)



### Calculate percent land cover (pland)
non_ag_pland_30 <- lsm_c_pland(non_ag_30)
print(non_ag_pland_30, n = 50)
# add years
years <- rep(2008:2023, each = 7)
pland_sub_30 <- non_ag_pland_30 %>%
  mutate(year = years)

pland_sub_30$class <- as.factor(pland_sub_30$class)
pland_sub_30

land_covers <- c(
  "1" = "#d95f02",
  "2" = "#66a61e",
  "3" = "#e7298a",
  "4" = "#7570b3",
  "5" = "#a6761d",
  "6" = "#e6ab02",
  "7" = "#1b9e77"
)

cat_labels <- c("crops", "forest", "developed", "wetlands", "shrub_meadow", "fallow_barren", "water")

# plot
ggplot(pland_sub_30, aes(x = as.numeric(year), y = value, color = class)) +
  geom_line(size = 0.7) +
  labs(
    title = "",
    x = "Year",
    y = "Percent"
  ) +
  scale_color_manual(values = land_covers, labels = cat_labels) +
  theme_minimal() +
  theme(
    legend.position = "bottom", # Position the legend at the bottom
    legend.direction = "vertical", # Make the legend vertical
    legend.box = "horizontal",
    axis.title = element_text(size = 14), # Adjusts axis labels size
    axis.text = element_text(size = 14), # Adjusts axis tick marks size
    legend.text = element_text(size = 14), # Adjusts legend text size
  ) +
  guides(color = guide_legend(
    nrow = 2, # Three rows
    ncol = 4, # Three columns
    label.position = "right", # Labels to the right of boxes
    title = "", # Optional title for the legend
    byrow = FALSE,
  ))

ggsave("C:/Users/A02425259/Desktop/MeetingUpdates/BL_report/pland_non_ag.png", width = 12, height = 8)


###############################################################################
#             Plot reclassified CDL non-ag rasters (sub watershed)
###############################################################################
### plot the smaller watershed areas 2008 & 2023
# raster with just years 2008 and 2023
rast <- subset(non_ag_raster, c(1, 16))

# Assign categories (levels) to each layer of the raster
levels(rast[[1]]) <- data.frame(ID = 0:7, "lyr1" = paste("Category", 0:7))
levels(rast[[2]]) <- data.frame(ID = 0:7, "lyr2" = paste("Category", 0:7))

# Define custom labels for each layer
custom_labels <- c("2008", "2023")

# Create a named vector to match the layer numbers with custom labels
label_vector <- setNames(custom_labels, paste0("cdl_lyr", 1:2))

# Define your categories and labels
cat_labels <- c("crops", "forest", "developed", "wetlands", "shrub meadow", "fallow barren", "water", "background")

land_covers <- c(
  # "Category 0" = "#666666",
  "Category 1" = "#d95f02",
  "Category 2" = "#66a61e",
  "Category 3" = "#e7298a",
  "Category 4" = "#7570b3",
  "Category 5" = "#a6761d",
  "Category 6" = "#e6ab02",
  "Category 7" = "#1b9e77"
)


# Plot with customized legend
ggplot() +
  geom_spatraster(data = rast) +
  facet_wrap(~lyr, ncol = 2, labeller = labeller(lyr = label_vector)) +
  scale_fill_manual(values = land_covers, labels = cat_labels) +
  theme_minimal() +
  labs(title = "") +
  theme(
    legend.position = "bottom", # Position the legend at the bottom
    legend.direction = "vertical", # Make the legend vertical
    legend.box = "horizontal",
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    panel.spacing = unit(0.05, "lines"),
    axis.text = element_blank(), # Remove axis text (tick labels)
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid = element_blank(),
    legend.margin = margin(-25, 0, 0, 0),
    strip.text.x = element_text(margin = margin(t = 1, b = -1)),
    plot.margin = margin(-10, 5, 5, 5)
  ) +
  guides(fill = guide_legend(
    nrow = 3, # Three rows
    ncol = 3, # Three columns
    label.position = "right", # Labels to the right of boxes
    title = "", # Optional title for the legend
    byrow = FALSE,
    override.aes = list(size = 5)
  ))

ggsave("C:/Users/A02425259/Desktop/MeetingUpdates/BL_report/non_ag_rasters.png", width = 12, height = 12)
