#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double corC(NumericVector x, NumericVector y) {
  int nx = x.size(), ny = y.size();
  
  if (nx != ny) stop("Input vectors must have equal length!");
  
  double sum_x = sum(x), sum_y = sum(y);
  
  NumericVector xy = x * y;
  NumericVector x_squ = x * x, y_squ = y * y;
  
  double sum_xy = sum(xy);
  double sum_x_squ = sum(x_squ), sum_y_squ = sum(y_squ);
  
  double out = ((nx * sum_xy) - (sum_x * sum_y)) /
               sqrt((nx * sum_x_squ - pow(sum_x, 2.0)) *
                    (nx * sum_y_squ - pow(sum_y, 2.0)));
  
  return out;
}
