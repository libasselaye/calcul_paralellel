#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
Void calculerDistances_cpp(NumericVector x, NumericVector y) {
  int n = x.length();
  NumericVector lx ={};
  NumericVector ly ={};
  for(int i=0;i<n-1;i++){
    //if(x[i])
  }
  
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# calculerDistance_simplifiée <- function(x, y) {
#   lx<-levels(x); ly<-levels(y)
#   nx<-length(lx); ny<-length(ly)
#   tab<-matrix(0,nx,ny)
#   for(i in 1:nx) { 
#     for(j in 1:ny) {
#       tab[i, j] <- sum(x==lx[i]&y==ly[j])
#     } 
#   } 
#   
#   dl<-(nx-1)*(ny-1)
#   Tst<-sqrt(chisq.test(tab)$statistic/n/sqrt(dl))
#   sqrt(2*(1-Tst^2))
# }

calculerDistances_cpp(x,y)
*/
