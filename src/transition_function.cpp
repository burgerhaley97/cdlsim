#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int transition_function_single(NumericVector cell_values, NumericMatrix transition_matrix,
                               IntegerVector row_names, IntegerVector col_names ) {
  // Initialize variables
  bool all_positive = true;
  int row_value = 0;
  int row_index = -1;
  NumericVector probabilities;
  int new_value_index = 0;
  int new_value = 0;

  // Check if both cell values are positive
  for(int i = 0; i < cell_values.size(); ++i) {
    if(cell_values[i] <= 0) {
      all_positive = false;
      break;
    }
  }

  if (all_positive) {
    return cell_values[0];  // Return the first cell value unchanged
  } else {
    // Get the row value corresponding to the second element's value
    row_value = cell_values[1];

    // Find the row index that matches the row value
    for (int i = 0; i < row_names.size(); ++i) {
      if (row_names[i] == row_value) {
        row_index = i;
        break;
      }
    }

    // If the row index was not found, return -1 to indicate an error or invalid state
    if (row_index == -1) {
      return -1;
    }

    // Get the probabilities from the transition matrix
    probabilities = transition_matrix(row_index, _);

    // Generate a random number between 0 and 1 using R's random number stream
    double random_value = unif_rand();

    // Calculate cumulative probabilities to determine the new value index
    double cumulative_sum = 0.0;
    for (int i = 0; i < probabilities.size(); ++i) {
      cumulative_sum += probabilities[i];
      if (random_value <= cumulative_sum) {
        new_value_index = i;
        break;
      }
    }

    // Get the new value from col_names using the new_value_index
    new_value = col_names[new_value_index];

    // Return the new value
    return new_value;
  }
}



// [[Rcpp::export]]
IntegerVector transition_function(NumericVector cell_values, NumericMatrix transition_matrix,
                                  IntegerVector row_names, IntegerVector col_names, int iterations) {
  // Initialize variables
  bool all_positive = true;
  int row_value = 0;
  int row_index = -1;
  NumericVector probabilities;
  int new_value_index = 0;
  IntegerVector new_values(iterations);

  // Check if both cell values are positive
  for(int i = 0; i < cell_values.size(); ++i) {
    if(cell_values[i] <= 0) {
      all_positive = false;
      break;
    }
  }

  if (all_positive) {
    // Return a vector of the first cell value repeated for the number of iterations
    std::fill(new_values.begin(), new_values.end(), cell_values[0]);
    return new_values;
  } else {
    // Get the row value corresponding to the second element's value
    row_value = cell_values[1];

    // Find the row index that matches the row value
    for (int i = 0; i < row_names.size(); ++i) {
      if (row_names[i] == row_value) {
        row_index = i;
        break;
      }
    }

    // If the row index was not found, return a vector of -1 to indicate an error or invalid state
    if (row_index == -1) {
      std::fill(new_values.begin(), new_values.end(), -1);
      return new_values;
    }

    // Get the probabilities from the transition matrix
    probabilities = transition_matrix(row_index, _);

    // Generate new values for each iteration
    for (int iter = 0; iter < iterations; ++iter) {
      // Generate a random number between 0 and 1 using R's random number stream
      double random_value = unif_rand();

      // Calculate cumulative probabilities to determine the new value index
      double cumulative_sum = 0.0;
      for (int i = 0; i < probabilities.size(); ++i) {
        cumulative_sum += probabilities[i];
        if (random_value <= cumulative_sum) {
          new_value_index = i;
          break;
        }
      }

      // Get the new value from col_names using the new_value_index
      new_values[iter] = col_names[new_value_index];
    }

    // Return the vector of new values
    return new_values;
  }
}


//[[Rcpp::export]]
IntegerVector transition_patches(NumericVector cell_values, NumericVector neg_values,
                                 List trans_vecs, int iterations) {
  // Initialize variables
  bool all_positive = true;
  int row_value = 0;
  int row_index = -1;
  int new_value_index = 0;
  IntegerVector new_values(iterations);

  // Check that the value of the first cell to get the class and correct matrix
  for(int i = 0; i < cell_values.size(); ++i) {
    if(cell_values[i] <= 0) {
      all_positive = false;
      break;
    }
  }

  if (all_positive) {
    // Return a vector of the first cell value repeated for the number of iterations
    std::fill(new_values.begin(), new_values.end(), cell_values[0]);
    return new_values;
  } else {
    // Get the row value corresponding to the second element's value
    row_value = cell_values[1];

    // Find the list name that matches the cell's value
    CharacterVector names = trans_vecs.names();
    for (int i = 0; i < names.size(); ++i) {
      if (std::stoi(std::string(names[i])) == row_value) {
        IntegerVector transition_vector = trans_vecs[i];
        return transition_vector;   // return the vector in the list
      }
    }
    return new_values;
  }
}



#include <Rcpp.h>
using namespace Rcpp;
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector transition_patches_random(NumericVector cell_values, List trans_vecs, int iterations) {
  // Initialize the output vector with length equal to `iterations`
  IntegerVector new_values(iterations);

  // Extract the first and second cell values
  int first_value = cell_values[0];
  int second_value = cell_values[1];

  // Convert `second_value` to a string for comparison with row names
  std::string second_value_str = std::to_string(second_value);

  // Check if `trans_vecs` contains a matrix corresponding to `first_value`
  if (!trans_vecs.containsElementNamed(std::to_string(first_value).c_str())) {
    stop("No transition matrix found for the specified first value");
  }

  // Retrieve the matrix associated with `first_value`
  IntegerMatrix trans_matrix = as<IntegerMatrix>(trans_vecs[std::to_string(first_value)]);

  // Retrieve row names from the matrix
  CharacterVector row_names = rownames(trans_matrix);

  // Check if row names are available
  if (row_names.size() == 0) {
    stop("No row names found for the specified transition matrix");
  }

  // Step 4: Find the row in `trans_matrix` where the row name matches `second_value_str`
  for (int i = 0; i < row_names.size(); i++) {
    if (row_names[i] == second_value_str) {
      // Fill `new_values` with this row’s content for the specified `iterations`
      for (int j = 0; j < iterations; ++j) {
        new_values[j] = trans_matrix(i, j % trans_matrix.ncol());
      }
      return new_values;
    }
  }

  // If no matching row name is found, return an NA vector or error
  new_values.fill(NA_INTEGER);
  return new_values;
}
