#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector tsamp(std::string expr, NumericVector x, Environment env) {
  Function parse("parse");
  SEXP expr_vec = parse(Named("text") = expr);
  if (Rf_length(expr_vec) < 1){
    stop("Failed to parse expression text.");
  }
  SEXP expr1 = VECTOR_ELT(expr_vec, 0);

  R_xlen_t n = x.size();
  NumericVector out(n);

  for (R_xlen_t i = 0; i < n; ++i){
    env.assign("x", x[i]);
    SEXP val = Rf_eval(expr1, env);
    out[i] = as<double>(val);
  }
  return out;
}

