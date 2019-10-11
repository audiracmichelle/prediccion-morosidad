#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int buscar_indice_primer_positivo(NumericVector x, float min_x) {
  int n = x.size();
  int i = 0;
  while(x[i] <= min_x && i < n) {
    i++;
  }
  return (i == n)? 0 : i + 1;
}

/*** R
buscar_indice_primer_positivo(c(0,0,0,0,0,0), 0)
buscar_indice_primer_positivo(c(0,0,0,0,0,0,5,2,0,8,7,9,2,0), 0)
buscar_indice_primer_positivo(c(0,0,0,0,0,0,0,2,2,5,5,5,5,5), 4)
*/
