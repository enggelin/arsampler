#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector tsamp(std::string expr, NumericVector x, Environment env) {
  Function parse("parse"), evalF("eval");

  std::string fn_text = std::string("function(x) ") + expr;
  SEXP expr_vec = parse(Named("text") = fn_text);
  if (Rf_length(expr_vec) < 1){
    stop("Failed to parse expression text.");
  }
  SEXP expr1 = VECTOR_ELT(expr_vec, 0);
  Function fun = as<Function>(evalF(expr1, env));

  R_xlen_t n = x.size();
  NumericVector out(n);

  for (R_xlen_t i = 0; i < n; ++i){
    out[i] = as<double>(fun(x[i]));
  }
  return out;
}
