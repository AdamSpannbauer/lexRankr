#include <Rcpp.h>
using namespace Rcpp;

double idfCosineSimilVector(NumericVector x, NumericVector y) {
  int n=x.size();
  double numerator=0;
  double denomenatorX=0;
  double denomenatorY=0;
  double result;

  for (int i = 0; i<n; ++i) {
    numerator += x[i]*y[i];
    denomenatorX += x[i]*x[i];
    denomenatorY += y[i]*y[i];
  }

  result = numerator/(sqrt(denomenatorX)*sqrt(denomenatorY));

  return result;
}

// [[Rcpp::export]]
NumericVector idfCosineSimil(NumericMatrix mat) {
  int n = mat.nrow();
  int nChoose2 = n*(n-1)/2;
  NumericVector result(nChoose2);
  int resInd = 0;

  for (int i = 0; i<n; ++i) {
    for (int j = i+1; j<n; ++j){
      if(resInd % 10000 == 0) Rcpp::checkUserInterrupt();
      result[resInd] = idfCosineSimilVector(mat(i,_), mat(j,_));
      ++resInd;
    }
  }

  return result;

}
