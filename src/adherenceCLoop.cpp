#include <Rcpp.h>
using namespace Rcpp;

//' Creates C++ loop 
//'
//' Meant to be a helper within date_check(), and subsequently propagate_date()
//
//' @param x Our date vector
//' @param y Our days supply vector
//' @return A new vector to be appended to a users dataframe with adjusted dates

// [[Rcpp::export]]
NumericVector date_checkCpp(NumericVector x, NumericVector y){
  int data_length = x.size();
  NumericVector out(data_length);
  
  
  for(int i = 1; i < data_length; ++i){
    int prior_date = x(i-1) + y(i-1);
    int date_to_check = x(i);
    
    // if the date prior is greater than the current date... 
    if(prior_date > date_to_check){
      // update the current date
      out[i] =  prior_date;
    } else {
      out[i] = date_to_check;
    }
  }
  out[0] = x[0];
  return out;
}

 
