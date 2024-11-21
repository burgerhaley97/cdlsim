################################################################################
#                    Create Test Rasters
################################################################################
## create raster with three classes (patches of all the same size)
library(terra)
library(dplyr)


################################################################################
#### Make the smaller max diversity raster

# Define raster dimensions and classes
ncol <- 90
nrow <- 90
classes <- 3

# Initialize a SpatRaster with the desired dimensions
r_small <- rast(ncol = ncol, nrow = nrow, vals = NA)

# Assign values in 30x30 blocks for each class
r_small[1:30, 1:30] <- 1  # Top-left 30x30 block
r_small[1:30, 31:60] <- 2  # Top-center 30x30 block
r_small[1:30, 61:90] <- 3  # Top-right 30x30 block

r_small[31:60, 1:30] <- 2  # Middle-left 30x30 block
r_small[31:60, 31:60] <- 3  # Middle-center 30x30 block
r_small[31:60, 61:90] <- 1  # Middle-right 30x30 block

r_small[61:90, 1:30] <- 3  # Bottom-left 30x30 block
r_small[61:90, 31:60] <- 1  # Bottom-center 30x30 block
r_small[61:90, 61:90] <- 2  # Bottom-right 30x30 block

## Show patches before simulation
plot(r_small)

# Define confusion matrix, background not transitioning
conf_mat <- matrix(1/3, nrow = 3, ncol = 3)

# Assign row and column names
rownames(conf_mat) <- c("1", "2", "3")
colnames(conf_mat) <- c("1", "2", "3")
conf_mat

set.seed(1891)
small_simple_sim <- simulate_raster_patch(original_raster = r_small,
                                       transition_matrix = conf_mat,
                                       iterations = 1000)

###############################################################################
#### Make the large max diversity raster

# Set parameters
n_classes <- 3        # Number of classes
square_size <- 30     # Each square is 30x30 pixels
n_squares <- 9        # 9x9 grid of squares

# Calculate raster dimensions
ncol <- square_size * n_squares   # Width to fit 9 squares across
nrow <- square_size * n_squares   # Height to fit 9 squares down

# Create an empty raster
r_big <- rast(ncol = ncol, nrow = nrow)
values(r_big) <- NA  # Initialize with NA values

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
    r_big[row_start:row_end, col_start:col_end] <- class_value
  }
}

plot(r_big)


set.seed(1891)
# simulate with free trans_mat
big_simple_sim <- simulate_raster_patch(original_raster = r_big,
                                          transition_matrix = conf_mat,
                                          iterations = 1000)


###############################################################################
#### Make the irregular raster

# Define raster dimensions and classes
ncol <- 90
nrow <- 90
classes <- 3

# Initialize a SpatRaster with the desired dimensions
r_irreg <- rast(ncol = ncol, nrow = nrow, vals = NA)

# Assign irregular class proportions with a smaller top-left square for class 1
r_irreg[1:30, 1:20] <- 1  # Smaller Class 1 region in the top-left
r_irreg[1:30, 21:90] <- 3  # Class 2 in the top-center and top-right
r_irreg[31:90, 1:45] <- 3  # Class 3 dominates bottom-left
r_irreg[31:60, 46:67] <- 1  # Class 1 in the left half of the middle-right
r_irreg[31:60, 68:90] <- 3  # Class 2 in the right half of the middle-right
r_irreg[61:90, 46:90] <- 3  # Class 2 in the bottom-right
r_irreg[1:30, 68:90] <- 1  # Top-right square of class 1

# Plot the raster
plot(r_irreg)

set.seed(1891)
# simulate with free trans_mat
irreg_simple_sim <- simulate_raster_patch(original_raster = r_irreg,
                                        transition_matrix = conf_mat,
                                        iterations = 1000)



###############################################################################
#                Make Figure for Results Section
###############################################################################

# Figure one
##############################################################################
# Plot All 3 Models together
library(RColorBrewer)
# Function to convert a SpatRaster to a data frame
raster_to_df <- function(r, name) {
  as.data.frame(r, xy = TRUE) %>%
    rename(value = 3) %>%  # Ensure the value column is named consistently
    mutate(raster = name)
}

# Convert rasters to data frames
df1 <- raster_to_df(r_small, "Small Regular")
df2 <- raster_to_df(r_big, "Large Regular")
df3 <- raster_to_df(r_irreg, "Irregular")

# Combine all data frames
combined_df <- rbind(df1, df2, df3)
combined_df$raster <- factor(combined_df$raster,
                             levels = c("Small Regular",
                                        "Large Regular",
                                        "Irregular"))

jpeg(
  file = "C:/Users/A02425259/Desktop/MeetingUpdates/proposal_figures/real_propsoal_figs/simple_models.jpg",
  width = 8,  # Set width to 6 inches
  height = 5,  # Set height to 6 inches for a square aspect ratio
  units = "in",  # Specify units (inches, pixels, cm, etc.)
  res = 400  # Set resolution for high-quality output
)

# Plot with ggplot2
ggplot(combined_df, aes(x = x, y = y, fill = as.factor(value))) +
  geom_raster() +
  facet_wrap(~ raster) +  # Free scales for different sizes
  scale_fill_brewer(palette = "Accent", name = "Class") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14),                    # Increase legend title font size
    legend.text = element_text(size = 12),                     # Increase legend text font size
    legend.position = "bottom",                                # Position legend at the bottom
    legend.box = "horizontal"
  )
dev.off()


# Figure Two
###############################################################################
library(landscapemetrics)
library(terra)
library(dplyr)
library(purrr)
# Calculate Shannon's and Simpson on the 1000 iter sims and og models
all_rasters <- list(small = small_simple_sim, large = big_simple_sim, irreg = irreg_simple_sim)

# Calculate metrics and bind directly into a single tibble
results_df_sim <- map_dfr(
  names(all_rasters),
  function(name) {
    r <- all_rasters[[name]]
    # Calculate metrics
    sidi <- landscapemetrics::lsm_l_sidi(r)
    shdi <- landscapemetrics::lsm_l_shdi(r)
    # Combine metrics and add raster name
    bind_rows(sidi, shdi) |> mutate(raster = name)
  }
)


sim_quants <- results_df_sim |>
  dplyr::group_by(metric, raster) |>
  dplyr::reframe(
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    median = quantile(value, 0.50)
  )

sim_quants2 <- results_df_sim |>
  dplyr::group_by(metric, raster) |>
  dplyr::reframe(
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    median = quantile(value, 0.50)
  )

# List of SpatRaster objects
og_rasters <- list(small = r_small, large = r_big, irreg = r_irreg)

# Calculate metrics and bind directly into a single tibble
results_df_og <- map_dfr(
  names(og_rasters),
  function(name) {
    r <- og_rasters[[name]]
    # Calculate metrics
    sidi <- landscapemetrics::lsm_l_sidi(r)
    shdi <- landscapemetrics::lsm_l_shdi(r)
    # Combine metrics and add raster name
    bind_rows(sidi, shdi) %>% mutate(raster = name)
  }
)

sidi_s <- landscapemetrics::lsm_l_sidi(r_small)
shdi_s <- landscapemetrics::lsm_l_shdi(r_small)
sidi_l <- landscapemetrics::lsm_l_sidi(r_big)
shdi_l <- landscapemetrics::lsm_l_shdi(r_big)

og_values <- results_df_og |>
  mutate(og_value = value) |>
  select(metric, og_value, raster)


# combine the og and sim results into one dataframe
results_combo <- full_join(sim_quants2, og_values, join_by(metric, raster)) |>
  mutate(raster = factor(raster, levels =  c("small", "large", "irreg")))


  ggplot(results_combo, aes(x = metric)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray40", width = 0.2) +
    geom_point(aes(y = og_value, color = "Original"), size = 3) +
    geom_point(aes(y = median, color = "Median"), size = 3) +
    facet_wrap(vars(raster),
               labeller = labeller(raster = c(
                 irreg = "Irregular",
                 large = "Large Regular",
                 small = "Small Regular"
               ))) +
    # Add labels and themes
    labs(x = "Metric", y = "Value", color = "Type") +
    theme_minimal() +
    scale_color_manual(
      values = c("Median" = "#1b9e77", "Original" = "#d95f02")
    ) +
    scale_y_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0.0, 1.25)) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )

