#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]

int PARTITION(NumericVector x, int p, int r){
  float temp, pivot;
  int i, j;
  pivot = x[r];
  i = p-1;
  for(j = p; j < r; j++){
    if(x[j] <= pivot){
      i = i+1;
      temp = x[j];
      x[j] = x[i];
      x[i] = temp;
    }
  }
  temp = x[i+1];
  x[i+1] = x[r];
  x[r] = temp;
  return i+1; 
}

// [[Rcpp::export]]

float FIND_ORDER_STAT( NumericVector x, int w, int p, int r){
  int q;
  q = PARTITION(x, p, r);
  if (q < w){
    return FIND_ORDER_STAT(x, w, q+1, r);
  }
  else if (q > w) {
    return FIND_ORDER_STAT(x, w, p, q-1);
  }
  else if (q == w){
    return x[q];
  }
}

// [[Rcpp::export]]

float order_statistic(NumericVector x, int w){
  int n;
  n = x.size();
  if (w <= n){
    w = w-1;
    return FIND_ORDER_STAT(x, w, 0, n-1);
  }
}
