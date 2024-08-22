#' Function to Read in and upzip confusion matrix files from the web
#'
#' @param years The years of data that you want to be downloaded.
#' @param temp_dir The file name you want the extracted files to be stored in.
#' @returns The raw excel books containing the confusion matrix data for
#'    every US state's USDA Crop Land Data Layer.
#' @importFrom utils head tail
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


# Function to process cld confusion matrix data for utah (Old)
get_mat_data_ut <- function(output_file = "all_matrix.RData") {

  # Define the directory where the extracted files are stored
  extracted_dir <- file.path(getwd(), "extracted_files")

  # Initialize a list to store the data
  all_data <- list()

  # List of files to process (assuming files are already extracted)
  file_list <- list.files(path = extracted_dir, recursive = TRUE, full.names = TRUE, pattern = "\\.xlsx$")

  # Function to subset a data frame
  subset_data_frame <- function(df) {
    df[1:256, 3:258]
  }

  # Loop through each file and process it
  for (file in file_list) {
    # Initialize data_file as NULL at the beginning of each iteration
    data_file <- NULL

    # Handle different formats based on the year using regex patterns
    if (grepl("CDL_200[8-9]_accuracy_assessments/NASS_CDL_UT[0-9]{2}_accuracy_30m\\.xlsx$", file)) {
      data_file <- file
    } else if (grepl("CDL_201[0-5]_accuracy_assessments/NASS_CDL_UT[0-9]{2}_accuracy\\.xlsx$", file)) {
      data_file <- file
    } else if (grepl("CDL_2016_accuracy_assessments/unbuffered_validation/NASS_CDL_UT16_accuracy_unbuf\\.xlsx$", file)) {
      data_file <- file
    } else if (grepl("CDL_2017_accuracy_assessments/NASS_CDL_UT17_accuracy\\.xlsx$", file)) {
      data_file <- file
    } else if (grepl("NASS_CDL_UT1[89]_accuracy\\.xlsx$", file) ||
               grepl("NASS_CDL_UT2[0-3]_accuracy\\.xlsx$", file)) {
      data_file <- file
    }

    # Read the third sheet from the Excel file if a match is found
    if (!is.null(data_file) && file.exists(data_file)) {
      message("Processing: ", data_file)
      data <- read_excel(data_file, sheet = 3)

      # Subset the data
      data <- subset_data_frame(data)

      # Extract the year from the file name using regex
      year <- sub(".*CDL_([0-9]{4}).*", "\\1", data_file)

      # Store the data in the list
      all_data[[year]] <- data
    } else {
      message("File does not match pattern or does not exist: ", file)
    }
  }

  # Save the list as an RData file
  names(all_data) <- as.character(2008:2023)
  save(all_data, file = output_file)

  # Return the all_data list for further use

  return(all_data)
}

#' Function to confusion matrix data for your state of interest
#' @param state_abbreviation A two letter abbreviation for US states.
#' @returns The data frames representing the confusion matrices for your states
#'    of interest for all years previously downloaded.
#' @importFrom dplyr intersect union
#' @import readxl
#' @export
get_mat_data <- function(state_abbreviation) {
  # Ensure the state abbreviation is uppercase
  state_abbreviation <- toupper(state_abbreviation)

  # Define the directory where the extracted files are stored
  extracted_dir <- file.path(getwd(), "extdata/extracted_files")

  # Initialize a list to store the data
  all_data <- list()

  # List of files to process (assuming files are already extracted)
  file_list <- list.files(path = extracted_dir, recursive = TRUE, full.names = TRUE, pattern = "\\.xlsx$")

  # Function to subset a data frame
  subset_data_frame <- function(df) {
    df[1:256, 3:258]
  }

  # Loop through each file and process it
  for (file in file_list) {
    # Initialize data_file as NULL at the beginning of each iteration
    data_file <- NULL

    # Create the regex patterns dynamically using the state abbreviation
    patterns <- list(
      sprintf("CDL_200[8-9]_accuracy_assessments/NASS_CDL_%s[0-9]{2}_accuracy_30m\\.xlsx$", state_abbreviation),
      sprintf("CDL_201[0-5]_accuracy_assessments/NASS_CDL_%s[0-9]{2}_accuracy\\.xlsx$", state_abbreviation),
      sprintf("CDL_2016_accuracy_assessments/unbuffered_validation/NASS_CDL_%s16_accuracy_unbuf\\.xlsx$", state_abbreviation),
      sprintf("CDL_2017_accuracy_assessments/NASS_CDL_%s17_accuracy\\.xlsx$", state_abbreviation),
      sprintf("NASS_CDL_%s1[89]_accuracy\\.xlsx$", state_abbreviation),
      sprintf("NASS_CDL_%s2[0-3]_accuracy\\.xlsx$", state_abbreviation)
    )

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
      year <- sub(".*CDL_([0-9]{4}).*", "\\1", data_file)

      # Store the data in the list
      all_data[[year]] <- data
    } else {
      message("File does not match pattern or does not exist: ", file)
    }
  }

  # Return the all_data list for further use
  return(all_data)
}


#' Function to format the confusion matrices as transition matrices
#' @param df_list The list of data frames extracted using get_mat_data()
#' @param categories A list of categories defining the numbers between 1 and 256.
#' @returns A list of data frames where row names represent pixels that will
#'    transition and column names represent the class they will transition to.
#' @import dplyr
#' @export
get_trans_mat <- function(df_list, categories) {

  # Check if the input is a list of lists of data frames and if it contains data frames
  if (length(df_list) == 0 || !all(sapply(df_list, function(x) length(x) == 2 && all(sapply(x, is.data.frame))))) {
    stop("Input must be a list of lists, each containing two data frames")
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

  # Initialize a list to store the results for each matching pair of data frames
  result_list <- vector("list", length(df_list))

  # Iterate over each pair of data frames in the list
  for (k in seq_along(df_list)) {
    # Extract the current pair of data frames
    current_pair <- df_list[[k]]
    df1 <- current_pair[[1]]
    df2 <- current_pair[[2]]

    # Convert data frames to matrices
    mat1 <- as.matrix(df1)
    mat2 <- as.matrix(df2)

    # Add the two matrices from the same pair together
    sum_mat <- mat1 + mat2

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
    df_trans <- df_trans %>%
      mutate(total = rowSums(across(everything()))) %>%
      mutate(across(-total, ~ . / total))

    # Update row names to be the negative value of the row number
    rownames(df_trans) <- as.character(-(1:nrow(df_trans)))

    # Update column names to be the column numbers
    colnames(df_trans) <- as.character(1:ncol(df_trans))

    # Remove the last column
    df_trans <- df_trans[, -ncol(df_trans)]

    # Store the resulting data frame in the result list
    result_list[[k]] <- df_trans
  }

  # Return the list of results
  return(result_list)
}

#' Function to format the confusion matrices as transition matrices
#' @param df_list A list of lists of data frames extracted using get_mat_data(). Each sublist should contain two or more data frames to be summed.
#' @param categories A list of categories defining the numbers between 1 and 256.
#' @returns A list of data frames where row names represent pixels that will
#'    transition and column names represent the class they will transition to.
#' @import dplyr
#' @export
get_trans_mat_gen <- function(df_list, categories) {

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

  # Initialize a list to store the results for each position in the sublists
  result_list <- vector("list", length(df_list[[1]]))

  # Iterate over each position in the sublists
  for (k in seq_along(result_list)) {
    # Sum the matrices from all data frames at the same position across all sublists
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
    df_trans <- df_trans %>%
      mutate(total = rowSums(across(everything()))) %>%
      mutate(across(-total, ~ . / total))

    # Update row names to be the negative value of the row number
    rownames(df_trans) <- as.character(-(1:nrow(df_trans)))

    # Update column names to be the column numbers
    colnames(df_trans) <- as.character(1:ncol(df_trans))

    # Remove the last column
    df_trans <- df_trans[, -ncol(df_trans)]

    # Store the resulting data frame in the result list
    result_list[[k]] <- df_trans
  }

  # Return the list of results
  return(result_list)
}

# Suppress undefined global variable warnings if 'total' is indeed a global variable
# utils::globalVariables("total")
