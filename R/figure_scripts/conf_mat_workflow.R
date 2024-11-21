#### Get the confusion matrices for your states of interest

# Process the raw data downloaded from the zip file web address
# still need to check this
download_cdl_mat_files(years = 2008, temp_dir = "extracted_files")
test <- get_mat_data(c("UT", "ID"), file_path = "inst/extdata/extracted_files")
test
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
cats <- list(non_ag = non_ag, ag = ag, alfalfa = alfalfa, major_ag = major_ag)

trans_mat <- get_trans_mat_gen(test, categories = cats)
trans_mat_2008 <- as.matrix(tran_mat[[1]])

