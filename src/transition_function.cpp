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
    // works if I don't explicitly convert to integers but integers are more
    // memory efficient and could avoid issues with floating point precision ?
    // row_value = static_cast<int>(cell_values[1]);
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
