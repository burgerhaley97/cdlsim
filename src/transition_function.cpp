#include <Rcpp.h>
using namespace Rcpp;

// Define the randWrapper function
// inline int randWrapper(const int n) {
//   return floor(unif_rand() * n);
// }


// [[Rcpp::export]]
int transition_function(NumericVector cell_values, NumericMatrix transition_matrix, IntegerVector row_names, IntegerVector col_names) {
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
