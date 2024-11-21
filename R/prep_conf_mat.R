#' Function to Read in and upzip confusion matrix files from the web
#'
#' @param years The years of data that you want to be downloaded.
#' @param temp_dir The file name you want the extracted files to be stored in.
#' @returns The raw excel books containing the confusion matrix data for
#'    every US state's USDA Crop Land Data Layer.
#' @importFrom utils head tail download.file unzip
#' @export
download_cdl_mat_files <- function(years, temp_dir = "extracted_files") {
  base_url <- "https://www.nass.usda.gov/Research_and_Science/Cropland/docs/"
  file_pattern <- "CDL_%d_accuracy_assessments.zip"

  # Function to download and extract zip files
  download_and_extract <- function(url, temp_dir) {
    temp <- tempfile()
    download.file(url, temp, mode = "wb")
    unzip(temp, exdir = temp_dir)
    unlink(temp)
  }

  # Get the current working directory and combine it with the destination directory
  temp_dir_full <- file.path(getwd(), temp_dir)

  # Create directory to store the extracted files
  dir.create(temp_dir_full, showWarnings = FALSE)

  # Download and extract files for each year
  lapply(years, function(year) {
    file_url <- sprintf(paste0(base_url, file_pattern), year)
    message("Downloading and extracting: ", file_url)
    download_and_extract(file_url, temp_dir_full)
  })
}


#' Function to retrieve confusion matrix data for multiple states of interest
#' @param state_abbreviation A vector of two-letter abbreviations for US states.
#' @param file_path The path to the directory where files are stored
#'    (default is "inst/extdata/extracted_files").
#' @return A named list where each element is a list of data frames representing
#'    confusion matrices for each state.
#' @importFrom dplyr intersect union
#' @import readxl
#' @export
get_mat_data <- function(state_abbreviation, file_path = "inst/extdata/extracted_files") {
  # Ensure all state abbreviations are uppercase
  state_abbreviation <- toupper(state_abbreviation)

  # Define the directory where the extracted files are stored
  extracted_dir <- file.path(getwd(), file_path)

  # List of files to process (assuming files are already extracted)
  file_list <- list.files(path = extracted_dir, recursive = TRUE, full.names = TRUE, pattern = "\\.xlsx$")

  # Function to subset a data frame
  subset_data_frame <- function(df) {
    df[1:256, 3:258]
  }

  # Internal function to process files for a single state abbreviation
  process_state_files <- function(state_abbrev) {
    # Initialize a list to store the data for this state
    state_data <- list()

    # Create the regex patterns dynamically using the state abbreviation
    patterns <- list(
      sprintf("CDL_200[8-9]_accuracy_assessments/NASS_CDL_%s[0-9]{2}_accuracy_30m\\.xlsx$", state_abbrev),
      sprintf("CDL_201[0-5]_accuracy_assessments/NASS_CDL_%s[0-9]{2}_accuracy\\.xlsx$", state_abbrev),
      sprintf("CDL_2016_accuracy_assessments/unbuffered_validation/NASS_CDL_%s16_accuracy_unbuf\\.xlsx$", state_abbrev),
      sprintf("CDL_2017_accuracy_assessments/NASS_CDL_%s17_accuracy\\.xlsx$", state_abbrev),
      sprintf("NASS_CDL_%s(1[8-9]|2[0-3])_accuracy\\.xlsx$", state_abbrev) # 2018-2023
    )

    # Loop through each file and process it
    for (file in file_list) {
      # Initialize data_file as NULL at the beginning of each iteration
      data_file <- NULL

      # Check if the file matches any of the patterns
      for (pattern in patterns) {
        if (grepl(pattern, file)) {
          data_file <- file
          break
        }
      }

      # Read the third sheet from the Excel file if a match is found
      if (!is.null(data_file) && file.exists(data_file)) {
        message("Processing: ", data_file)
        data <- read_excel(data_file, sheet = 3)

        # Subset the data
        data <- subset_data_frame(data)

        # Extract the year from the file name using regex
        year_match <- sub(".*([0-9]{2})_accuracy.*\\.xlsx$", "\\1", data_file)
        year <- ifelse(length(year_match) > 0, year_match, "Unknown Year")

        # Store the data in the state_data list with year as the key
        state_data[[year]] <- data
      } else {
        message("File does not match pattern or does not exist: ", file)
      }
    }

    return(state_data)
  }

  # Use Map to apply process_state_files to each state abbreviation
  all_states_data <- Map(process_state_files, state_abbreviation)

  # Set names of the list as the state abbreviations for easy reference
  names(all_states_data) <- state_abbreviation

  # Return the combined list of all states' data
  return(all_states_data)
}

## update for the case when the list of categories is the same as the length list of
# orignal classes
#' Function to format the confusion matrices as transition matrices
#' @param df_list A list of lists of data frames extracted using get_mat_data(). Each sublist should contain two or more data frames to be summed.
#' @param categories A list of categories defining the numbers between 1 and 256.
#' @returns A list of data frames where row names represent pixels that will
#'    transition and column names represent the class they will transition to.
#' @import dplyr
get_trans_mat <- function(df_list, categories) {
  # Check if the input is a list of lists of data frames and if it contains data frames
  if (length(df_list) == 0 || !all(sapply(df_list, function(x) all(sapply(x, is.data.frame))))) {
    stop("Input must be a list of lists, each containing data frames")
  }

  # Flatten the categories and check for duplicates and completeness
  all_category_numbers <- unlist(categories)

  # Check for duplicates
  if (any(duplicated(all_category_numbers))) {
    stop("There are duplicate numbers in the list of vectors describing the categories")
  }

  # Check for completeness (1 to 256)
  if (!all(sort(all_category_numbers) == 1:256)) {
    stop("Not all numbers between 1 and 256 are included in the list of vectors describing the categories")
  }

  # Initialize a list to store the results by year
  result_list <- vector("list", length(df_list[[1]]))
  names(result_list) <- names(df_list[[1]]) # Assign names for each year

  # Iterate over each year (position in the sublists)
  for (k in seq_along(result_list)) {
    # Sum the matrices from all data frames for the same year across states
    sum_mat <- Reduce(`+`, lapply(df_list, function(x) as.matrix(x[[k]])))

    # Convert the summed matrix back to a data frame
    df <- as.data.frame(sum_mat)

    # Initialize an empty matrix to store the category sums
    category_matrix <- matrix(0, nrow = length(categories), ncol = length(categories))

    # Sum rows and columns for each category based on the provided vectors
    for (i in seq_along(categories)) {
      for (j in seq_along(categories)) {
        category_rows <- df %>%
          slice(categories[[i]]) %>%
          summarize(across(everything(), sum))

        category_sum <- category_rows %>%
          select(all_of(categories[[j]])) %>%
          summarize(across(everything(), sum))

        category_matrix[i, j] <- sum(category_sum)
      }
    }

    # Convert the matrix to a data frame
    df_trans <- as.data.frame(category_matrix)

    # Calculate total and proportions
    df_trans <- df_trans |>
      mutate(total = rowSums(across(everything()))) |>
      mutate(across(-total, ~ . / total))

    # Update row names to be the negative value of the row number
    rownames(df_trans) <- as.character((1:nrow(df_trans)))

    # Update column names to be the column numbers
    colnames(df_trans) <- as.character(1:ncol(df_trans))

    # Remove the last column
    df_trans <- df_trans[, -ncol(df_trans)]

    # Store the resulting data frame for this year
    result_list[[k]] <- df_trans
  }

  # Return the list of results
  # insert a row of zeros / column of zeros to each matrix
  return(result_list)
}
