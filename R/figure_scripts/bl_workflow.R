###############################################################################
#               Load in the BL data and reclassify
###############################################################################
# attach packages
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
plot(bl_sub_raster)


################################################################################
#        Create Simulations with 10, 100, 1000 iterations (reclass)
################################################################################

# Define confusion matrix, background not transitioning
conf_mat <- matrix(1/4, nrow = 5, ncol = 5)
conf_mat[1, ] <- c(1, 0, 0, 0, 0)
conf_mat[, 1] <- c(1, 0, 0, 0, 0)

# Assign row and column names
rownames(conf_mat) <- c("0", "1", "2", "3", "4")
colnames(conf_mat) <- c("0", "1", "2", "3", "4")
conf_mat

# Start time
Sys.time()
bl_sim_1000 <- simulate_raster_patch(original_raster = cdl_reclass,
                                     transition_matrix = conf_mat,
                                     iterations = 1000)
# End time
Sys.time()



################################################################################
#       Create Simulation 1000 iterations (unclassed)
################################################################################
# reclassify into background = 0, major-ag = 1, alfalfa = 2, non-ag = 3, gen_ag = 4
cdl_all_class <- terra::app(bl_sub_raster, fun = function(x) {
  ifelse(is.na(x), 0, x)
})


# make fake transition matrix for the unclassed version
unclass <- sort(unique(values(cdl_all_class)))
# unclass <- c(NA, unclass)
row <- length(unclass)
col <- length(unclass)
conf_mat <- matrix(1/25, nrow = row, ncol = col)

# Assign row and column names
rownames(conf_mat) <- as.character(unclass)
colnames(conf_mat) <- as.character(unclass)
conf_mat[, 1] <- 0
conf_mat[1, ] <- 0
conf_mat[1, 1] <- 1
conf_mat[, 12] <- 0
conf_mat[12, ] <- 0
conf_mat[12, 12] <- 1
conf_mat





# Start time
Sys.time()
all_sim_1000 <- simulate_raster_patch(original_raster = cdl_all_class,
                                     transition_matrix = conf_mat,
                                     iterations = 1000)
# End time
Sys.time()

# Start time
bench::mark(
  all_sim_1000_5 <- simulate_raster_patch(original_raster = cdl_all_class,
                                               transition_matrix = conf_mat,
                                               iterations = 1000),
  iterations = 5)

###############################################################################
#### Get transition matrices for states around Bear Lake watershed
library(readxl)
library(dplyr)
library(terra)
library(ggplot2)
library(landscapemetrics)

## Plot with real confusion matrix
# Get the confusion matrices for your states of interest
# Process the raw data downloaded from the zip file web address
download_cdl_mat_files(years = 2008, temp_dir = "extracted_files")
UT <- get_mat_data("UT")
ID <- get_mat_data("ID")

# Combine the lists for each state
combined_data <- Map(list, UT, ID)

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
transition_matrix <- result[[1]] # get just 2008
trans_mat <- rbind(0, cbind(0, transition_matrix))
trans_mat[1,1] <- 1

# Assign row and column names
rownames(trans_mat) <- c("0", "1", "2", "3", "4")
colnames(trans_mat) <- c("0", "1", "2", "3", "4")

conf_mat <- trans_mat

# Start time
Sys.time()
restricted_sim_1000 <- simulate_raster_patch(original_raster = cdl_reclass,
                                      transition_matrix = conf_mat,
                                      iterations = 1000)
# End time
Sys.time()


# End time
Sys.time()

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
      dplyr::ungroup()

    not_all_within_range <- final_comparison |>
      dplyr::group_by(metric) |>
      dplyr::filter(!all(within_range)) |>
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
      dplyr::ungroup()

    not_all_within_range <- final_comparison |>
      dplyr::group_by(metric) |>
      dplyr::filter(!all(within_range)) |>
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

percent_in_free <- in_free_l / (in_free_l + out_free_l) * 100
percent_in_restrict <- in_restrict_l / (in_restrict_l + out_restrict_l) * 100


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


## get the interval length ratios (restricted / free run)
