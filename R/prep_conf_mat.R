utils::globalVariables("total")

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
#' @param verbose The stops the messages from printing to the console.
#' @return A named list where each element is a list of data frames representing
#'    confusion matrices for each state.
#' @importFrom dplyr intersect union
#' @importFrom stats na.omit
#' @import readxl
#' @export
get_mat_data_dep <- function(state_abbreviation, file_path = "inst/extdata/extracted_files", verbose = FALSE) {
  # Ensure all state abbreviations are uppercase
  state_abbreviation <- toupper(state_abbreviation)

  # Use system.file() to correctly locate the installed files
  extracted_dir <- system.file("extdata/extracted_files", package = "cdlsim")

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
        if (verbose) message("Processing: ", data_file)
        data <- readxl::read_excel(data_file, sheet = 3, .name_repair = "minimal")

        # Subset the data to the required range
        data <- data[1:256, 3:258]

        # Extract the year from the file name
        year_match <- sub(".*([0-9]{2})_accuracy.*\\.xlsx$", "\\1", data_file)
        year <- ifelse(length(year_match) > 0, year_match, "Unknown Year")

        # Store the data in the state_data list
        state_data[[year]] <- data
      } else if (verbose) {
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


#' Function to retrieve confusion matrix data for multiple states of interest
#' @param state_abbreviation A vector of two-letter abbreviations for US states.
#' @param file_path The path to the directory where files are stored
#'    (default is "inst/extdata/extracted_files").
#' @param verbose The stops the messages from printing to the console.
#' @return A named list where each element is a list of data frames representing
#'    confusion matrices for each state.
#' @importFrom dplyr intersect union
#' @importFrom stats na.omit
#' @import readxl
#' @export
get_mat_data <- function(state_abbreviation, file_path = "inst/extdata/extracted_files", verbose = FALSE) {
  state_abbreviation <- toupper(state_abbreviation)

  # Track whether this is a direct file input
  single_file_input <- FALSE

  # Resolve system file path if default
  if (file_path == "inst/extdata/extracted_files") {
    file_path <- system.file("extdata/extracted_files", package = "cdlsim")
  }

  # Detect single file input
  if (file.exists(file_path) && grepl("\\.xlsx$", file_path)) {
    file_list <- file_path
    single_file_input <- TRUE
  } else {
    file_list <- list.files(path = file_path, recursive = TRUE, full.names = TRUE, pattern = "\\.xlsx$")
  }

  subset_data_frame <- function(df) {
    df[1:256, 3:258]
  }

  process_state_files <- function(state_abbrev) {
    state_data <- list()


    if (single_file_input && file.exists(file_list)) {
      if (verbose) message("Using provided file directly: ", file_list)
      data <- readxl::read_excel(file_list, sheet = 3, .name_repair = "minimal")
      data <- subset_data_frame(data)

      return(data)
    }

    # Otherwise use pattern matching
    patterns <- list(
      sprintf("CDL_200[8-9]_accuracy_assessments/NASS_CDL_%s[0-9]{2}_accuracy_30m\\.xlsx$", state_abbrev),
      sprintf("CDL_201[0-5]_accuracy_assessments/NASS_CDL_%s[0-9]{2}_accuracy\\.xlsx$", state_abbrev),
      sprintf("CDL_2016_accuracy_assessments/unbuffered_validation/NASS_CDL_%s16_accuracy_unbuf\\.xlsx$", state_abbrev),
      sprintf("CDL_2017_accuracy_assessments/NASS_CDL_%s17_accuracy\\.xlsx$", state_abbrev),
      sprintf("NASS_CDL_%s(1[8-9]|2[0-3])_accuracy\\.xlsx$", state_abbrev),
      sprintf("NASS_CDL_%s[0-9]{2}_accuracy_30m\\.xlsx$", state_abbrev)  # catch-all
    )

    for (file in file_list) {
      data_file <- NULL
      file_basename <- basename(file)

      for (pattern in patterns) {
        if (grepl(pattern, file) || grepl(pattern, file_basename)) {
          data_file <- file
          break
        }
      }

      if (!is.null(data_file) && file.exists(data_file)) {
        if (verbose) message("Processing: ", data_file)
        data <- readxl::read_excel(data_file, sheet = 3, .name_repair = "minimal")
        data <- subset_data_frame(data)

        year_match <- sub(".*([0-9]{2})_accuracy.*\\.xlsx$", "\\1", basename(data_file))
        year <- ifelse(length(year_match) > 0, year_match, "Unknown Year")

        state_data[[year]] <- data
      } else if (verbose) {
        message("File does not match pattern or does not exist: ", file)
      }
    }

    return(state_data)
  }


  if (single_file_input) {
    return(process_state_files(state_abbreviation[1]))
  }


  all_states_data <- Map(process_state_files, state_abbreviation)
  names(all_states_data) <- state_abbreviation

  return(all_states_data)
}


## update for the case when the list of categories is the same as the length list of
# original classes
#' Function to format the confusion matrices as transition matrices
#' @param df_list A list of lists of data frames extracted using get_mat_data(). Each sublist should contain two or more data frames to be summed.
#' @param categories A list of categories defining the numbers between 1 and 256.
#' @returns A list of data frames where row names represent pixels that will
#'    transition and column names represent the class they will transition to.
#' @import dplyr
#' @importFrom stats na.omit
get_trans_mat_dep <- function(df_list, categories) {
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


    # Insert a row and column of zeros at the first positions
    n <- nrow(df_trans) + 1  # Updated number of rows and columns
    updated_matrix <- matrix(0, nrow = n, ncol = n)
    updated_matrix[-1, -1] <- as.matrix(df_trans)  # Fill with existing values

    # Convert the updated matrix back to a data frame
    updated_matrix[1, 1] <- 1
    df_trans_updated <- as.data.frame(updated_matrix)

    # Update row and column names for the new matrix
    rownames(df_trans_updated) <- as.character(0:(n - 1))
    colnames(df_trans_updated) <- as.character(0:(n - 1))

    # Store the resulting data frame for this year
    result_list[[k]] <- df_trans_updated
  }

  # Return the list of results
  return(result_list)
}


#' Function to format the confusion matrices as transition matrices
#' @param df_list A single data frame or a list of lists of data frames extracted using get_mat_data().
#'                If a list, each sublist should contain two or more data frames to be summed.
#' @param categories A list of categories defining the numbers between 1 and 256.
#' @returns A list of data frames where row names represent pixels that will
#'    transition and column names represent the class they will transition to.
#' @import dplyr
#' @export
get_trans_mat <- function(df_list, categories) {
  # If input is a single data frame, convert it to a list containing one list
  if (is.data.frame(df_list)) {
    df_list <- list(list(df_list))
  }

  # If input is a list of data frames (not nested), wrap it in an additional list
  if (is.list(df_list) && all(sapply(df_list, is.data.frame))) {
    df_list <- list(df_list)
  }

  # Check if the input is a properly formatted list of lists of data frames
  if (length(df_list) == 0 || !all(sapply(df_list, function(x) all(sapply(x, is.data.frame))))) {
    stop("Input must be a single data frame, a list of data frames, or a list of lists containing data frames")
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
  names(result_list) <- names(df_list[[1]]) # Assign names for each year if applicable

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

    # Insert a row and column of zeros at the first positions
    n <- nrow(df_trans) + 1  # Updated number of rows and columns
    updated_matrix <- matrix(0, nrow = n, ncol = n)
    updated_matrix[-1, -1] <- as.matrix(df_trans)  # Fill with existing values

    # Convert the updated matrix back to a data frame
    updated_matrix[1, 1] <- 1
    df_trans_updated <- as.data.frame(updated_matrix)

    # Update row and column names for the new matrix
    rownames(df_trans_updated) <- as.character(0:(n - 1))
    colnames(df_trans_updated) <- as.character(0:(n - 1))

    # Store the resulting data frame for this year
    result_list[[k]] <- df_trans_updated
  }

  # Return the list of results
  return(result_list)
}



get_trans_mat_simple <- function(df_list, categories) {
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

    # Initialize an empty matrix to store the category sums
    category_matrix <- matrix(0, nrow = length(categories), ncol = length(categories))

    # Sum rows and columns for each category based on the provided vectors
    for (i in seq_along(categories)) {
      for (j in seq_along(categories)) {
        category_rows <- sum_mat[categories[[i]], , drop = FALSE]
        category_matrix[i, j] <- sum(category_rows[, categories[[j]], drop = FALSE])
      }
    }

    # Add totals row and column
    total_row <- rowSums(category_matrix)
    total_col <- colSums(category_matrix)
    total_sum <- sum(total_row) # Grand total

    # Expand the matrix to include totals
    category_matrix <- rbind(category_matrix, total_col)
    category_matrix <- cbind(category_matrix, c(total_row, total_sum))

    # Convert to a data frame
    df_trans <- as.data.frame(category_matrix)

    # Set row and column names
    rownames(df_trans) <- c(paste0("Cat_", seq_along(categories)), "Total")
    colnames(df_trans) <- c(paste0("Cat_", seq_along(categories)), "Total")

    # Store the resulting data frame for this year
    result_list[[k]] <- df_trans
  }

  # Return the list of results
  return(result_list)
}
