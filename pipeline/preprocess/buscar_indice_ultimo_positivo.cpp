#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int buscar_indice_ultimo_positivo(NumericVector x, float min_x) {
  int n = x.size();
  int i = n - 1;
  while(x[i] <= min_x && i >= 0) {
    i--;
  }
  return i + 1;
}

/*** R
buscar_indice_ultimo_positivo(c(0,0,0,0,0,0), 0)
buscar_indice_ultimo_positivo(c(0,0,5,2,0,8,7,9,2,0,0,0), 5)
*/