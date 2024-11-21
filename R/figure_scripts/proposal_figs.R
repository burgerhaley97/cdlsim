### Attach packages
library(dplyr)
library(terra)
library(ggplot2)
library(readxl)
library(landscapemetrics)


###############################################################################
#         Load in the BL CDL Data and USGS Watershed Boundaries
###############################################################################
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


################################################################################
#             Set up free run data ( 5, 10, 25 classes)
################################################################################

#### Reclassify the rasters to 5 major categories
###############################################################################
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
cdl_reclass_5 <- terra::app(bl_sub_raster, fun = function(x) {
  ifelse(is.na(x), 0,
         ifelse(x %in% major_ag, 1,
                ifelse(x %in% alfalfa, 2,
                       ifelse(x %in% ag_mask, 3, 4)
                )
         )
  )
})

# Define confusion matrix, background not transitioning
mat_free_5 <- matrix(1/4, nrow = 5, ncol = 5)

# Set background not transitioning (first row and column)
mat_free_5[1, ] <- c(1, rep(0, 4))
mat_free_5[, 1] <- c(1, rep(0, 4))

# Assign row and column names in a single step
dimnames(mat_free_5) <- list(as.character(0:4), as.character(0:4))

#### Reclassify the raster to 10 major categories
###############################################################################
# Get the 25 values from the input raster
all_values <- sort(na.omit(unique(values(bl_sub_raster))))

# Group them into categories of 2 and 3 numbers
groups <- list(
  c(21, 23),
  c(24, 28, 33),
  c(36, 37, 43),
  c(61, 68),
  c(111, 121, 122),
  c(123, 124),
  c(131, 141, 142),
  c(143, 152),
  c(176, 190, 195, 205)
)

# Reclassify into 10 categories
cdl_reclass_10 <- terra::app(bl_sub_raster, fun = function(x) {
  ifelse(is.na(x), 0,
         ifelse(x %in% groups[[1]], 1,
                ifelse(x %in% groups[[2]], 2,
                       ifelse(x %in% groups[[3]], 3,
                              ifelse(x %in% groups[[4]], 4,
                                     ifelse(x %in% groups[[5]], 5,
                                            ifelse(x %in% groups[[6]], 6,
                                                   ifelse(x %in% groups[[7]], 7,
                                                          ifelse(x %in% groups[[8]], 8, 9)))))))))
})


# Define confusion matrix, background not transitioning
mat_free_10 <- matrix(1/9, nrow = 10, ncol = 10)
mat_free_10[1, ] <- c(1, rep(0, 9))
mat_free_10[, 1] <- c(1, rep(0, 9))

# Assign row and column names
dimnames(mat_free_10) <- list(as.character(0:9), as.character(0:9))

#### Reclassify the raster to 15 major categories
###############################################################################
# Get the unique sorted values from the input raster
all_values <- sort(na.omit(unique(values(bl_sub_raster))))

# Original groups split further into 15 groups
groups <- list(
  c(21),               # Group 0
  c(23),               # Group 1
  c(24),               # Group 2
  c(28),               # Group 3
  c(33),               # Group 4
  c(36),               # Group 5
  c(37),          # Group 6
  c(43),               # Group 7
  c(61),               # Group 8
  c(68),               # Group 9
  c(111),              # Group 10
  c(121, 122),         # Group 11
  c(123, 124),         # Group 12
  c(131, 141, 142),    # Group 13
  c(143, 152, 176, 190, 195, 205) # Group 14
)

# Reclassify into 15 categories
cdl_reclass_15 <- terra::app(bl_sub_raster, fun = function(x) {
  ifelse(is.na(x), 0,
         ifelse(x %in% groups[[1]], 1,
                ifelse(x %in% groups[[2]], 2,
                       ifelse(x %in% groups[[3]], 3,
                              ifelse(x %in% groups[[4]], 4,
                                     ifelse(x %in% groups[[5]], 5,
                                            ifelse(x %in% groups[[6]], 6,
                                                   ifelse(x %in% groups[[7]], 7,
                                                          ifelse(x %in% groups[[8]], 8,
                                                                 ifelse(x %in% groups[[9]], 9,
                                                                        ifelse(x %in% groups[[10]], 10,
                                                                               ifelse(x %in% groups[[11]], 11,
                                                                                      ifelse(x %in% groups[[12]], 12,
                                                                                             ifelse(x %in% groups[[13]], 13,  14
                                                                                                   ))))))))))))))
})


unique(values(cdl_reclass_15))
# Define a confusion matrix for 15 categories, with background not transitioning
mat_free_15 <- matrix(1/14, nrow = 15, ncol = 15)
mat_free_15[1, ] <- c(1, rep(0, 14))
mat_free_15[, 1] <- c(1, rep(0, 14))

# Assign row and column names
dimnames(mat_free_15) <- list(as.character(0:14), as.character(0:14))

#### Reclassify the raster to 25 major categories
###############################################################################
# reclassify into background = 0, major-ag = 1, alfalfa = 2, non-ag = 3, gen_ag = 4
cdl_reclass_25 <- terra::app(bl_sub_raster, fun = function(x) {
  ifelse(is.na(x), 0, x)
})

# Define confusion matrix, background not transitioning
mat_free_25 <- matrix(1/24, nrow = 25, ncol = 25)
mat_free_25[1, ] <- c(1, rep(0, 24))
mat_free_25[, 1] <- c(1, rep(0, 24))

# Assign row and column names
dimnames(mat_free_25) <- list(as.character(c(0, all_values)), as.character(c(0, all_values)))

################################################################################
#        Time 1000 simulations using the free matrix over 5 runs
################################################################################
set.seed(1891)

## This sim will be used for the figures in the results section:
fig_free_sim <- simulate_raster_patch(original_raster = cdl_reclass_5,
                                         transition_matrix = mat_free_5,
                                         iterations = 1000)


result_free_5 <- bench::mark(
  bl_free_5 <- simulate_raster_patch(original_raster = cdl_reclass_5,
                                       transition_matrix = mat_free_5,
                                       iterations = 1000), iterations = 5
)


bl_free_5 <- simulate_raster_patch(original_raster = cdl_reclass_5,
                                   transition_matrix = mat_free_5,
                                   iterations = 2)

result_free_10 <- bench::mark(
  bl_free_10 <- simulate_raster_patch(original_raster = cdl_reclass_10,
                                     transition_matrix = mat_free_10,
                                     iterations = 1000), iterations = 5
)

Sys.time()
bl_free_10 <- simulate_raster_patch(original_raster = cdl_reclass_10,
                                    transition_matrix = mat_free_10,
                                    iterations = 1000)
Sys.time()

result_free_15 <- bench::mark(
  bl_free_15 <- simulate_raster_patch(original_raster = cdl_reclass_15,
                                      transition_matrix = mat_free_15,
                                      iterations = 1000), iterations = 5
)

Sys.time()
bl_free_15 <- simulate_raster_patch(original_raster = cdl_reclass_15,
                                    transition_matrix = mat_free_15,
                                    iterations = 1000)
Sys.time()


result_free_25 <- bench::mark(
  bl_free_25 <- simulate_raster_patch(original_raster = cdl_reclass_25,
                                      transition_matrix = mat_free_25,
                                      iterations = 1000), iterations = 5
)

length(unique(values(cdl_reclass_25)))
length(unique(values(cdl_reclass_10)))
###############################################################################
####       Make Restricted Matrices of BL watershed
##############################################################################
## Plot with real confusion matrix
download_cdl_mat_files(years = 2008, temp_dir = "extracted_files")
bl_mat_data <- get_mat_data(c("UT", "ID", "WY"))

#### 5 major categories
###############################################################################
# Define the values that represent our classes of interest
non_ag <- c(61:65, 81:83, 87:88, 92, 111:112, 121:124, 131, 141:143, 152, 176, 181, 190, 195)
alfalfa <- c(36:37)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)
all_numbers <- 1:256
ag <- setdiff(all_numbers, c(non_ag, alfalfa, major_ag))

# List of categories with their corresponding vectors
cat_5 <- list(non_ag = non_ag, ag = ag, alfalfa = alfalfa, major_ag = major_ag)

# get the confusion matrix for just 2008
mat_rest_5_all <- get_trans_mat(bl_mat_data, cat_5)
mat_rest_5 <- mat_rest_5_all[[1]]
mat_rest_5[1, ] <- c(1, rep(0, 4))
mat_rest_5[, 1] <- c(1, rep(0, 4))

# Assign row and column names in a single step
dimnames(mat_rest_5) <- list(as.character(0:4), as.character(0:4))

#### 10 major categories
###############################################################################
# List of categories with their corresponding vectors
cat_10 <- groups

# get the confusion matrix for just 2008
mat_rest_10_all <- get_trans_mat(bl_mat_data, cat_10)
mat_rest_10 <- mat_rest_10_all[[1]]
mat_rest_10[1, ] <- c(1, rep(0, 9))
mat_rest_10[, 1] <- c(1, rep(0, 9))

# Assign row and column names in a single step
dimnames(mat_rest_10) <- list(as.character(0:9), as.character(0:9))

#### 25 major categories
###############################################################################
# List of categories with their corresponding vectors
cat_25 <- c(0, na.omit(unique(values(bl_sub_raster))))

# get the confusion matrix for just 2008
mat_rest_25_all <- get_trans_mat(bl_mat_data, cat_25)
mat_rest_25 <- mat_rest_25_all[[1]]
mat_rest_25[1, ] <- c(1, rep(0, 24))
mat_rest_25[, 1] <- c(1, rep(0, 24))

# Assign row and column names in a single step
dimnames(mat_rest_25) <- list(as.character(0:24), as.character(0:24))


################################################################################
#        Time 1000 simulations using the free matrix over 5 runs
################################################################################
set.seed(1891)

## This sim will be used for the figures in the results section:
fig_rest_sim <- simulate_raster_patch(original_raster = cdl_reclass_5,
                      transition_matrix = mat_rest_5,
                      iterations = 1000)

result_rest_5 <- bench::mark(
  bl_rest_5 <- simulate_raster_patch(original_raster = cdl_reclass_5,
                                     transition_matrix = mat_rest_5,
                                     iterations = 1000), runs = 5
)

result_rest_10 <- bench::mark(
  bl_rest_10 <- simulate_raster_patch(original_raster = cdl_reclass_10,
                                      transition_matrix = mat_rest_10,
                                      iterations = 1000), runs = 5
)

result_rest_25 <- bench::mark(
  bl_rest_25 <- simulate_raster_patch(original_raster = cdl_reclass_25,
                                      transition_matrix = mat_rest_25,
                                      iterations = 1000), runs = 5
)

################################################################################
#                 Mean Patch Area
################################################################################
mn_patch <- landscapemetrics::lsm_c_area_mn(bl_sim_1000)
interval <- mn_patch |>
  group_by(class) |>
  summarize(quants = quantile(value, c(0.025, 0.5, 0.975)))

mn_patch_int <- as_tibble(interval) |>
  mutate(type = "sim",
         quantile = rep(c(2.5, 50, 97.5), times =  5))

mn_patch_og <- landscapemetrics::lsm_c_area_mn(cdl_reclass) |>
  mutate(type = "og",
         quants = value) |>
  select(class, quants, type)

mn_patch_1000 <- bind_rows(mn_patch_int, mn_patch_og) |>
  mutate(metric = rep("mean_patch", times = length(class))) |>
  filter(class > 0)

mn_patch_1000_error <- mn_patch_1000 %>%
  filter(type == "sim" & quantile %in% c(2.5, 97.5)) %>%
  pivot_wider(names_from = quantile, values_from = quants, names_prefix = "quantile_") %>%
  rename(ymin = quantile_2.5, ymax = quantile_97.5)

ggplot() +
  # Plot original (og) values and median (50th quantile) simulated values if not NA
  geom_point(data = mn_patch_1000 %>% filter(type == "og" | (type == "sim" & quantile == 50)),
             aes(x = class, y = quants, color = type), size = 3) +
  # Add error bars for 2.5% and 97.5% quantiles
  geom_errorbar(data = mn_patch_1000_error, aes(x = class, ymin = ymin, ymax = ymax), width = 0.2) +
  labs(x = "Type", y = "Value") +
  ggtitle("Mean Patch Area") +
  theme_minimal() +
  scale_y_continuous(limits = c(-50, 3000)) +
  theme(legend.position = "none")

################################################################################
#               Calculate all metrics to test:
################################################################################

# extract all level stats from simulations all at once
free_sim_metrics <- landscapemetrics::calculate_lsm(bl_sim_1000)

restricted_sim_metrics <- landscapemetrics::calculate_lsm(restricted_sim_1000)


###############################################################################
#         Look for metrics where the original value falls within
###############################################################################
# Function that take the sim/og inputs and return metrics where the quantiles
# capture the og value in a 95% confidence interval
compare_quants <- function(sim_input, og_input,
                           level = c("class", "patch", "landscape")) {

  level <- match.arg(level)

  if (level == "class") {
    unique_metrics <- unique(sim_input$metric)
    comparison_results <- list()

    for (metric in unique_metrics) {
      # Filter for the current metric and remove NA values
      sim_metric_data <- sim_input |>
        dplyr::filter(metric == !!metric, !is.na(value))

      og_metric_data <- og_input |>
        dplyr::filter(metric == !!metric, !is.na(value))

      # Group sim_input by metric and class, and calculate quantiles across all layers
      sim_quantiles <- sim_metric_data |>
        dplyr::group_by(metric, class) |>
        dplyr::reframe(
          lower = quantile(value, 0.025),
          upper = quantile(value, 0.975),
          .groups = 'drop'
        ) |>
        dplyr::distinct(metric, class, lower, upper)

      # Group og_input by metric and class, and extract the unique value
      og_summary <- og_metric_data |>
        dplyr::group_by(metric, class) |>
        dplyr::summarize(
          og_value = unique(value),
          .groups = 'drop'
        ) |>
        dplyr::distinct(metric, class, og_value)

      # Compare each unique og_value to the corresponding quantiles of the same class and metric
      comparison <- og_summary |>
        dplyr::left_join(sim_quantiles, by = c("metric", "class")) |>
        dplyr::mutate(
          within_range = og_value >= lower & og_value <= upper
        )

      comparison_results[[metric]] <- comparison
    }

    # Bind all data frames by row to return a single data frame
    final_comparison <- dplyr::bind_rows(comparison_results)

    # Split into two tibbles: one where all within_range are TRUE for each metric, and one for the else case
    all_within_range <- final_comparison |>
      dplyr::group_by(metric) |>
      dplyr::filter(all(within_range)) |>
      dplyr::mutate(type = "in") |>
      dplyr::ungroup()

    not_all_within_range <- final_comparison |>
      dplyr::group_by(metric) |>
      dplyr::filter(!all(within_range)) |>
      dplyr::mutate(type = "out") |>
      dplyr::ungroup()

    return(list(all_within_range = all_within_range, not_all_within_range = not_all_within_range))
  }

  if (level == "landscape") {
    unique_metrics <- unique(sim_input$metric)
    comparison_results <- list()

    for (metric in unique_metrics) {
      # Filter for the current metric and remove NA values
      sim_metric_data <- sim_input |>
        dplyr::filter(metric == !!metric, !is.na(value))

      og_metric_data <- og_input |>
        dplyr::filter(metric == !!metric, !is.na(value))

      # Group sim_input by metric, and calculate quantiles across all layers
      sim_quantiles <- sim_metric_data |>
        dplyr::group_by(metric) |>
        dplyr::reframe(
          lower = quantile(value, 0.025),
          upper = quantile(value, 0.975),
          .groups = 'drop'
        ) |>
        dplyr::distinct(metric, lower, upper)

      # Group og_input by metric, and extract the unique value
      og_summary <- og_metric_data |>
        dplyr::group_by(metric) |>
        dplyr::reframe(
          og_value = unique(value),
          .groups = 'drop'
        ) |>
        dplyr::distinct(metric, og_value)

      # Compare each unique og_value to the corresponding quantiles of the same metric
      comparison <- og_summary |>
        dplyr::left_join(sim_quantiles, by = "metric") |>
        dplyr::mutate(
          within_range = og_value >= lower & og_value <= upper
        )

      comparison_results[[metric]] <- comparison
    }

    # Bind all data frames by row to return a single data frame
    final_comparison <- dplyr::bind_rows(comparison_results)

    # Split into two tibbles: one where all within_range are TRUE for each metric, and one for the else case
    all_within_range <- final_comparison |>
      dplyr::group_by(metric) |>
      dplyr::filter(all(within_range)) |>
      dplyr::mutate(type = "in") |>
      dplyr::ungroup()

    not_all_within_range <- final_comparison |>
      dplyr::group_by(metric) |>
      dplyr::filter(!all(within_range)) |>
      dplyr::mutate(type = "out") |>
      dplyr::ungroup()

    return(list(all_within_range = all_within_range, not_all_within_range = not_all_within_range))
  }

  return(NULL)
}

free_sim_landscape <- free_sim_metrics |>
  filter(level == "landscape")

restricted_sim_landscape <- restricted_sim_metrics |>
  filter(level == "landscape")

og_sim_metrics_l <- og_sim_metrics |>
  filter(level == "landscape")


#### look at the number of landscape metrics that capture the true value in a CI
results_all_land <- compare_quants(sim_input = free_sim_landscape,
                                   og_input = og_sim_metrics_l ,
                                   level = "landscape")
results_all_land_r <- compare_quants(sim_input = restricted_sim_landscape,
                                     og_input = og_sim_metrics_l ,
                                     level = "landscape")

## get the number of unique metrics included as a percentage
in_free_l <- length(unique(results_all_land$all_within_range$metric))
out_free_l <- length(unique(results_all_land$not_all_within_range$metric))

in_restrict_l <- length(unique(results_all_land_r$all_within_range$metric))
out_restrict_l <- length(unique(results_all_land_r$not_all_within_range$metric))

percent_in_free_l <- in_free_l / (in_free_l + out_free_l) * 100
percent_in_restrict_l <- in_restrict_l / (in_restrict_l + out_restrict_l) * 100


#### intervals for classes 1 through 4, smallest intervals first
land_free_sorted <- results_all_land$all_within_range |>
  dplyr::mutate(diff = upper - lower) |>
  dplyr::arrange(diff)

print(unique(land_free_sorted$metric))

#### intervals for classes 1 through 4, smallest intervals first
land_restricted_sorted <- results_all_land_r$all_within_range |>
  dplyr::mutate(diff = upper - lower) |>
  dplyr::arrange(diff)

print(unique(land_restricted_sorted$metric))

#### look at the number of class metrics that capture the true value in a CI
results_all_restricted <- compare_quants(sim_input = restricted_class,
                                         og_input = og_sim_metrics_c ,
                                         level = "class")

results_all_free <- compare_quants(sim_input = free_sim_class,
                                   og_input = og_sim_metrics_c ,
                                   level = "class")

## get the number of unique metrics included as a percentage
in_restrict_c <- length(unique(results_all_restricted$all_within_range$metric))
out_restrict_c <- length(unique(results_all$not_all_within_range$metric))

in_free_c <- length(unique(results_all_free$all_within_range$metric))
out_free_c  <- length(unique(results_all_free$not_all_within_range$metric))

percent_in_free_c <- in_free_c / (in_free_c + out_free_c) * 100
percent_in_restrict_c <- in_restrict_c / (in_restrict_c + out_restrict_c) * 100

#### intervals for classes 1 through 4, smallest intervals first
class_free_sorted <- results_all_free$all_within_range |>
  dplyr::filter(class > 0) |>
  dplyr::mutate(diff = upper - lower) |>
  dplyr::arrange(diff)

print(unique(class_free_sorted$metric))

#### intervals for classes 1 through 4, smallest intervals first
class_restricted_sorted <- results_all_restricted$all_within_range |>
  dplyr::filter(class > 0) |>
  dplyr::mutate(diff = upper - lower) |>
  dplyr::arrange(diff)

print(unique(class_restricted_sorted$metric))

###############################################################################
## get the interval length ratios (restricted / free run)
###############################################################################
## Start with class level metrics
class_in_rest <- rbind(results_all_restricted$all_within_range,
                       results_all_restricted$not_all_within_range) |>
  mutate(diff_rest = upper - lower) |>
  select(metric, class, diff_rest, type)

class_in_free <- rbind(results_all_free$all_within_range,
                       results_all_free$not_all_within_range) |>
  mutate(diff_free = upper - lower)|>
select(metric, class, diff_free, type)

ratios_in_c <- full_join(class_in_rest, class_in_free, by = join_by(metric, class)) |>
  filter(class > 0 ) |>
  mutate(ratio = diff_rest / diff_free,
         rank = rank(ratio)) |>
  group_by(metric) |>
  mutate(
    average_ratio = mean(ratio, na.rm = TRUE)  # Calc average across class 1:4
  ) |>
  arrange(rank)

ave_ratios_c <- ratios_in_c |>
  mutate(rest_sim = type.x,
         free_sim = type.y) |>
  select(metric, average_ratio, rest_sim, free_sim) |>
  distinct() |>
  arrange(average_ratio) |>
  filter(rest_sim == "in" & free_sim == "in")

## Get Landscape level intervals
land_in_rest <- rbind(results_all_land_r$all_within_range,
                      results_all_land_r$not_all_within_range) |>
  mutate(diff_rest = upper - lower) |>
  select(metric, diff_rest, type)

land_in_free <- rbind(results_all_land$all_within_range,
                      results_all_land$not_all_within_range) |>
  mutate(diff_free = upper - lower)|>
  select(metric, diff_free, type)

ratios_in_l <- full_join(land_in_rest, land_in_free, by = join_by(metric)) |>
  mutate(ratio = diff_rest / diff_free,
         rank = rank(ratio)) |>
  group_by(metric) |>
  mutate(
    average_ratio = mean(ratio, na.rm = TRUE)  # Calc average across class 1:4
  ) |>
  arrange(rank)


ratios_in_l |>
  filter(metric == "pr")


ave_ratios_l <- ratios_in_l |>
  mutate(rest_sim = type.x,
         free_sim = type.y) |>
  distinct() |>
  select(metric, average_ratio, rest_sim, free_sim) |>
  arrange(average_ratio) |>
  filter(rest_sim == "in" & free_sim == "in")



print(ratios_in)
tail(ratios_in)


nrow(class_in_free)
nrow(class_in_rest)

ratios_in |>
  filter(is.na(diff_free))

class_in_free |>
  filter(is.na(diff_free))

###############################################################################
##  Figure 1: BL Map
###############################################################################
library(sf)
library(tidyterra)

# set levels
levels(cdl_reclass) <- data.frame(ID = 0:4, "lyr1" = 0:4)
# Adjust color mappings to include Bear Lake
crop_covers <- c(
  "0" = "lightgray",
  "1" = "#d95f02",
  "2" = "#66a61e",
  "3" = "#7570b3",
  "4" = "#e6ab02",
  "Bear Lake" = "blue" # Add blue for the Bear Lake polygon
)

# Add corresponding label
class_labels <- c(
  "Background", "Major Ag", "Alfalfa", "Non Ag", "Gen Ag", "Bear Lake"
)

# import Bear Lake polygon
bl_shp <- st_read("C:/Users/A02425259/Desktop/bl_land_use/data/Bl_shp")

png("C:/Users/A02425259/Desktop/MeetingUpdates/proposal_figures/real_propsoal_figs/bl_map.png",
    width = 10, height = 10, units = "in", res = 73)
# Plot
ggplot() +
  geom_spatraster(data = cdl_reclass, aes(fill = lyr1)) + # Use the fill aesthetic for the raster
  geom_sf(data = bl_shp, aes(fill = "Bear Lake"), color = NA, size = 0.5) + # Map Bear Lake to "Bear Lake"
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
dev.off()




###############################################################################
##  Figure 2: BL Div Metrics
###############################################################################
## set up the simulations that will be used for the following figures
set.seed(1891)
bl_free_fig <- simulate_raster_patch(original_raster = cdl_reclass_5,
                                     transition_matrix = mat_free_5,
                                     iterations = 1000)
set.seed(1891)
bl_rest_fig <- simulate_raster_patch(original_raster = cdl_reclass_5,
                                   transition_matrix = mat_rest_5,
                                   iterations = 1000)

# Calculate Shannon's and Simpson on the 1000 iter sims and og models
all_rasters <- list(free = bl_free_fig, rest = bl_rest_fig)

sim_bl_div <- map(
  names(all_rasters),
  function(name) {
    r <- all_rasters[[name]]
    sidi <- landscapemetrics::lsm_l_sidi(r)
    shdi <- landscapemetrics::lsm_l_shdi(r)
    list(sidi = sidi, shdi = shdi, raster = name)
  }
)

# Flatten and combine outside the loop
results_bl_sims <- map_dfr(
  sim_bl_div ,
  ~ bind_rows(.x$sidi, .x$shdi) |> mutate(raster = .x$raster)
)

sim_quants_div <- results_bl_sims |>
  dplyr::group_by(metric, raster) |>
  dplyr::reframe(
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    median = quantile(value, 0.50)
  )

# Calculate Shannon's and Simpson on the 1000 iter sims and og models
og_rasters <- list(og = cdl_reclass)

og_bl_div <- map_dfr(
  names(og_rasters),
  function(name) {
    r <- og_rasters[[name]]
    sidi <- landscapemetrics::lsm_l_sidi(r)
    shdi <- landscapemetrics::lsm_l_shdi(r)
    bind_rows(sidi, shdi) %>% mutate(raster = name)
  }
)

og_values_div <- og_bl_div |>
  mutate(og_value = value) |>
  select(metric, og_value, raster)

new_rows <- tibble(
  metric = c("sidi", "sidi", "shdi", "shdi"),
  raster = factor(c("free", "rest", "free", "rest"), levels = levels(div_combo_bl$raster)),
  og_value = c(0.559, 0.559, 0.949, 0.949)
)

# combine the og and sim results into one dataframe
div_combo_bl <- full_join(sim_quants_div, new_rows, join_by(metric, raster)) |>
  mutate(raster = factor(raster, levels =  c("free", "rest")))


ggplot(div_combo_bl, aes(x = metric)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray40", width = 0.2) +
  geom_point(aes(y = og_value, color = "Original"), size = 3) +
  geom_point(aes(y = median, color = "Median"), size = 3) +
  facet_wrap(vars(raster),
             labeller = labeller(raster = c(
               free = "Free Running",
               rest = "Restricted"
             ))) +
  # Add labels and themes
  labs(x = "Metric", y = "Value", color = "Type") +
  theme_minimal() +
  scale_color_manual(
    values = c("Median" = "#1b9e77", "Original" = "#d95f02")
  ) +
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.25), limits = c(0.5, 1))
    +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )



###############################################################################
##  Figure 3: Metric: Patch Area, Pland,
###############################################################################
div_f <- landscapemetrics::lsm_c_division(bl_free_fig)
div_r <- landscapemetrics::lsm_c_division(bl_rest_fig )
mn_f <- landscapemetrics::lsm_c_area_mn(bl_free_fig)
mn_r <- landscapemetrics::lsm_c_area_mn(bl_rest_fig)
pd_f <- landscapemetrics::lsm_c_pd(bl_free_fig)
pd_r <- landscapemetrics::lsm_c_pd(bl_rest_fig)
pland_f <- landscapemetrics::lsm_c_pland(bl_free_fig)
pland_r <- landscapemetrics::lsm_c_pland(bl_rest_fig)



sim_bl_metrics <- map(
  names(all_rasters),
  function(name) {
    r <- all_rasters[[name]]
     div <- landscapemetrics::lsm_c_division(r)
     mn <- landscapemetrics::lsm_c_area_mn(r)
     pd <- landscapemetrics::lsm_c_pd(r)
     pland <- landscapemetrics::lsm_c_pland(r)
    list(div = div, mn = mn, pd = pd, pland = pland, raster = name)
  }
)

# Flatten and combine outside the loop
results_bl_metrics <- map_dfr(
  sim_bl_metrics,
  ~ bind_rows(.x$div, .x$mn, .x$pd, .x$pland) |> mutate(raster = .x$raster)
)

sim_quants_metrics <- results_bl_metrics |>
  dplyr::group_by(metric, class, raster) |>
  dplyr::reframe(
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    median = quantile(value, 0.50)
  )

# Calculate Shannon's and Simpson on the 1000 iter sims and og models
og_rasters <- list(og = cdl_reclass)

og_bl_metrics <- map_dfr(
  names(og_rasters),
  function(name) {
    r <- og_rasters[[name]]
    div <- landscapemetrics::lsm_c_division(r)
    mn <- landscapemetrics::lsm_c_area_mn(r)
    pd <- landscapemetrics::lsm_c_pd(r)
    pland <- landscapemetrics::lsm_c_pland(r)
    bind_rows(div, mn, pd, pland) %>% mutate(raster = name)
  }
)

og_values_metrics <- og_bl_metrics |>
  mutate(og_value = value) |>
  select(metric, og_value, class, raster)

og_values_metrics_repeated <- og_values_metrics |>
  slice(rep(1:n(), each = 2)) |>
  arrange(metric) |>
  mutate(
    raster = factor(
      rep(c("free", "rest"), length.out = length(og_value)),
      levels = levels(div_combo_bl$raster)
    )
  )

# combine the og and sim results into one dataframe
div_combo_bl <- full_join(sim_quants_metrics, og_values_metrics_repeated , join_by(metric, class, raster)) |>
  mutate(raster = factor(raster, levels =  c("free", "rest"))) |>
  filter(class > 0)

png("C:/Users/A02425259/Desktop/MeetingUpdates/proposal_figures/real_propsoal_figs/bl_metrics_all.png",
    width = 10, height = 10, units = "in", res = 73)
ggplot(div_combo_bl, aes(x = metric)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray40", width = 0.2) +
  geom_point(aes(y = og_value, color = "Original"), size = 3) +
  geom_point(aes(y = median, color = "Median"), size = 3) +
  facet_grid(
    class ~ raster, # Facet by raster and class
    labeller = labeller(
      raster = c(
        free = "Free Running",
        rest = "Restricted"
      ),
      class = c(
        "1" = "Class 1",
        "2" = "Class 2",
        "3" = "Class 3",
        "4" = "Class 4"
      )
    )
    ) +
  # Add labels and themes
  labs(x = "Metric", y = "Log of Value", color = "Type") +
  theme_minimal() +
  scale_color_manual(
    values = c("Median" = "#1b9e77", "Original" = "#d95f02")
  ) +
  scale_y_log10() +
  theme(
    legend.position = "bottom",
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
dev.off()

