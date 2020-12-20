#include <Rcpp.h>
using namespace Rcpp;
//[[Rcpp::export]]
double laplace(double x){
  return exp(-abs(x));
}
//[[Rcpp::export]]
List MetropolisC(double sigma, double x0, int N) {
  NumericVector x(N);
  x[0]=x0;
  NumericVector u(N);
  u=runif(N);
  int k=0;
  for(int i=1; i<N; i++){
    double y;
    y=rnorm(1,x[i-1],sigma)[0];
    if(u[i]<=(laplace(y)/laplace(x[i-1])))
      x[i]=y;
    else{
      x[i]=x[i-1];
      k++;
    }
  }
  List out;
  out["x"]=x;
  out["k"]=k;
  return out;
  //return x;
}

