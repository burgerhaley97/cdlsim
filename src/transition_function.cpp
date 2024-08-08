#include <Rcpp.h>
using namespace Rcpp;

// Define the randWrapper function
inline int randWrapper(const int n) {
  return floor(unif_rand() * n);
}

// [[Rcpp::export]]
int transition_function(NumericVector cell_values, NumericMatrix transition_matrix, CharacterVector row_names, CharacterVector col_names) {
  // Check if both cell values are positive
  bool all_positive = true;
  for(int i = 0; i < cell_values.size(); ++i) {
    if(cell_values[i] <= 0) {
      all_positive = false;
      break;
    }
  }

  if (all_positive) {
    return cell_values[0];  // Return the first cell value unchanged
  } else {
    // Get the row name corresponding to the second element's value
    std::string row_name = std::to_string(static_cast<int>(cell_values[1]));

    // Find the row index that matches the row name
    int row_index = -1;
    for (int i = 0; i < row_names.size(); ++i) {
      if (row_names[i] == row_name) {
        row_index = i;
        break;
      }
    }

    // If the row index was not found, return the original value to avoid invalid transitions
    if (row_index == -1) {
      Rcpp::Rcout << "Row index not found. Returning original value: " << cell_values[0] << std::endl;
      return cell_values[0];
    }

    // Get the probabilities from the transition matrix
    NumericVector probabilities = transition_matrix(row_index, _);

    // Generate a random number to determine the new state
    int new_value_index = randWrapper(probabilities.size());

    // Ensure the new value corresponds to a valid class and not zero
    int new_value = std::stoi(std::string(col_names[new_value_index]));

    // Return the new value
    return new_value;
  }
}
