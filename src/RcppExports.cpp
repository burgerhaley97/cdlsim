// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// transition_function_single
int transition_function_single(NumericVector cell_values, NumericMatrix transition_matrix, IntegerVector row_names, IntegerVector col_names);
RcppExport SEXP _cdlsim_transition_function_single(SEXP cell_valuesSEXP, SEXP transition_matrixSEXP, SEXP row_namesSEXP, SEXP col_namesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type cell_values(cell_valuesSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type transition_matrix(transition_matrixSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type row_names(row_namesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type col_names(col_namesSEXP);
    rcpp_result_gen = Rcpp::wrap(transition_function_single(cell_values, transition_matrix, row_names, col_names));
    return rcpp_result_gen;
END_RCPP
}
// transition_function
IntegerVector transition_function(NumericVector cell_values, NumericMatrix transition_matrix, IntegerVector row_names, IntegerVector col_names, int iterations);
RcppExport SEXP _cdlsim_transition_function(SEXP cell_valuesSEXP, SEXP transition_matrixSEXP, SEXP row_namesSEXP, SEXP col_namesSEXP, SEXP iterationsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type cell_values(cell_valuesSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type transition_matrix(transition_matrixSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type row_names(row_namesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type col_names(col_namesSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    rcpp_result_gen = Rcpp::wrap(transition_function(cell_values, transition_matrix, row_names, col_names, iterations));
    return rcpp_result_gen;
END_RCPP
}
// transition_patches
IntegerVector transition_patches(NumericVector cell_values, NumericVector neg_values, List trans_vecs, int iterations);
RcppExport SEXP _cdlsim_transition_patches(SEXP cell_valuesSEXP, SEXP neg_valuesSEXP, SEXP trans_vecsSEXP, SEXP iterationsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type cell_values(cell_valuesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type neg_values(neg_valuesSEXP);
    Rcpp::traits::input_parameter< List >::type trans_vecs(trans_vecsSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    rcpp_result_gen = Rcpp::wrap(transition_patches(cell_values, neg_values, trans_vecs, iterations));
    return rcpp_result_gen;
END_RCPP
}
// transition_patches_random
IntegerVector transition_patches_random(NumericVector cell_values, List trans_vecs, int iterations);
RcppExport SEXP _cdlsim_transition_patches_random(SEXP cell_valuesSEXP, SEXP trans_vecsSEXP, SEXP iterationsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type cell_values(cell_valuesSEXP);
    Rcpp::traits::input_parameter< List >::type trans_vecs(trans_vecsSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    rcpp_result_gen = Rcpp::wrap(transition_patches_random(cell_values, trans_vecs, iterations));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_cdlsim_transition_function_single", (DL_FUNC) &_cdlsim_transition_function_single, 4},
    {"_cdlsim_transition_function", (DL_FUNC) &_cdlsim_transition_function, 5},
    {"_cdlsim_transition_patches", (DL_FUNC) &_cdlsim_transition_patches, 4},
    {"_cdlsim_transition_patches_random", (DL_FUNC) &_cdlsim_transition_patches_random, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_cdlsim(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
