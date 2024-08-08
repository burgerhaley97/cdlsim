#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector transitionFunctionC(NumericVector cell_values, NumericMatrix transition_matrix) {
  int n = cell_values.size();
  NumericVector result(n);

  // Iterate over each cell value
  for (int i = 0; i < n; ++i) {
    double cell_value = cell_values[i];

    if (cell_value > 0) {
      // If the value is positive, return it unchanged
      result[i] = cell_value;
    } else {
      // Map the negative value to an index in the transition matrix
      int row_index = (cell_value == -1) ? 0 : 1;  // Assume -1 maps to 0, -2 maps to 1

      // Get the probabilities from the matrix
      NumericVector probabilities = transition_matrix(row_index, _);

      // Sample a new value based on the probabilities
      double random_sample = R::runif(0, 1);
      double cumulative_prob = 0;
      int new_value = 1;

      for (int j = 0; j < probabilities.size(); ++j) {
        cumulative_prob += probabilities[j];
        if (random_sample <= cumulative_prob) {
          new_value = j + 1;  // Assuming transition to states 1, 2, etc.
          break;
        }
      }

      result[i] = new_value;
    }
  }

  return result;
}
