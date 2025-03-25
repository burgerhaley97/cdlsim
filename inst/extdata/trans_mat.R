# make the data frame for bl example
bl_mat_data <- get_mat_data(c("UT"))


# List of categories with their corresponding vectors
# Define the values that represent our classes of interest
non_ag <- c(61:65, 81:83, 87:88, 92, 111:112, 121:124, 131, 141:143, 152, 176, 181, 190, 195)
alfalfa <- c(36:37)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)
all_numbers <- 1:256
ag <- setdiff(all_numbers, c(non_ag, alfalfa, major_ag))

cat_5 <- list(non_ag = non_ag, ag = ag, alfalfa = alfalfa, major_ag = major_ag)

# get the confusion matrix for just 2008
trans_mat_5 <- get_trans_mat(bl_mat_data, cat_5)
trans_mat_08 <- trans_mat_5[[1]]

saveRDS(trans_mat_08, file = "inst/extdata/trans_mat_08.rds")
