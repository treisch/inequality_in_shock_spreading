#define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

// [[Rcpp::export]]
double DR2012_i(int i, arma::sp_mat W, arma::vec v){
  // A_ij liability from i to j ... i owes money to j
  // e_i equity of bank i
  // W_ij = min(A_ij/e_j)
  // s_t = {0, 1, 2} = {undist, distr, inactive}
  
  // arma::vec v(val);
  
  int n = v.n_elem;
  // Rcout << "n banks: " << n << std::endl ;
  
  arma::mat one_vec = arma::ones<arma::vec>(n);
  
  double DR;
  
  
  
  arma::vec h_t = arma::zeros<arma::vec>(n);
  arma::vec h_tm1 = arma::zeros<arma::vec>(n);
  arma::vec s_t = arma::zeros<arma::vec>(n);
  arma::vec s_tm1 = arma::zeros<arma::vec>(n);
  arma::vec stress = arma::zeros<arma::vec>(n);
  
  h_tm1(i) = 1.0;
  s_tm1(i) = 1.0;
  
  // Rcout << "bank default: " << i << std::endl ;
  // Rcout << " " << std::endl ;
  
  
  int counter = 0;
  bool crit = true;
  while(crit){
    counter += 1;
    stress = arma::conv_to< arma::vec >::from(s_tm1 == 1);
    //     stress.print("stress");
    
    h_t = arma::min(one_vec, h_tm1 + arma::trans(W)*(h_tm1%stress) );
    //     h_t.print("h_t");
    
    s_t = 1 * arma::conv_to< arma::vec >::from( (h_t > 0) && (s_tm1 ==0)) +
      2 * arma::conv_to< arma::vec >::from( (s_tm1 == 1)) +
      s_t % arma::conv_to< arma::vec >::from( (s_tm1 == 0) || (s_tm1 == 2));
    //      s_t.print("s_t update");
    
    crit = arma::accu(arma::abs(s_t-s_tm1)) > 0;
    h_tm1 = h_t;
    s_tm1 = s_t;
    
    //crit = counter <6;
  }
  
  DR = arma::sum(v%h_t) - v(i);
  return DR;
}


// [[Rcpp::export]]
arma::vec Dij(int i, arma::sp_mat W, arma::vec v){
  // A_ij liability from i to j ... i owes money to j
  // e_i equity of bank i
  // W_ij = min(A_ij/e_j)
  // s_t = {0, 1, 2} = {undist, distr, inactive}
  
  // arma::vec v(val);
  
  int n = v.n_elem;
  // Rcout << "n banks: " << n << std::endl ;
  
  arma::mat one_vec = arma::ones<arma::vec>(n);
  
  arma::vec h_t = arma::zeros<arma::vec>(n);
  arma::vec h_tm1 = arma::zeros<arma::vec>(n);
  arma::vec s_t = arma::zeros<arma::vec>(n);
  arma::vec s_tm1 = arma::zeros<arma::vec>(n);
  arma::vec stress = arma::zeros<arma::vec>(n);
  
  h_tm1(i) = 1.0;
  s_tm1(i) = 1.0;
  
  // Rcout << "bank default: " << i << std::endl ;
  // Rcout << " " << std::endl ;
  
  
  int counter = 0;
  bool crit = true;
  while(crit){
    counter += 1;
    stress = arma::conv_to< arma::vec >::from(s_tm1 == 1);
    //     stress.print("stress");
    
    h_t = arma::min(one_vec, h_tm1 + arma::trans(W)*(h_tm1%stress) );
    //     h_t.print("h_t");
    
    s_t = 1 * arma::conv_to< arma::vec >::from( (h_t > 0) && (s_tm1 ==0)) +
      2 * arma::conv_to< arma::vec >::from( (s_tm1 == 1)) +
      s_t % arma::conv_to< arma::vec >::from( (s_tm1 == 0) || (s_tm1 == 2));
    //      s_t.print("s_t update");
    
    crit = arma::accu(arma::abs(s_t-s_tm1)) > 0;
    h_tm1 = h_t;
    s_tm1 = s_t;
    
    //crit = counter <6;
  }
  return h_t;
}
