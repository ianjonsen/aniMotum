#ifndef joint_mpm_hpp
#define joint_mpm_hpp 1

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

using namespace density;

template <class Type>
Type joint_mpm(objective_function<Type>* obj) {
  
  DATA_MATRIX(x);                   // locations
  DATA_INTEGER(A);                  // number of animals
  DATA_FACTOR(idx);                // cumsum of number of locations for each animal
  
  PARAMETER_VECTOR(lg);		          // Autocorrelation parameter (link scale)
  PARAMETER_VECTOR(l_sigma);	      // Innovation variance (log scale)
  PARAMETER(l_sigma_g);             // logistic scale parameter of rw on lg (log scale)
  
  // Backtransform parameters from link scale
  vector<Type> gamma = Type(1.0) / (Type(1.0) + exp(-lg));
  vector<Type> sigma = exp(l_sigma);
  Type sigma_g = exp(l_sigma_g);
  
  // 2x2 covariance matrix for innovations
  matrix<Type> cov(2,2);
  cov(0,0) = sigma(0) * sigma(0);
  cov(0,1) = 0.0;
  cov(1,0) = 0.0;
  cov(1,1) = sigma(1) * sigma(1);
  
  Type jnll = 0.0;
  vector<Type> mu(2);
  
  MVNORM_t<Type> nll_dens(cov);   // Multivariate Normal density
  int i,j;
  
  for(i = 0; i < A; ++i) {
    for(j = (idx(i)+1); j < idx(i+1); ++j) {
      jnll -= dnorm(lg(j), lg(j-1), sigma_g, TRUE);  // RW on logit(gamma)
    }
    
    for(j = (idx(i)+2); j < idx(i+1); ++j){
      mu = x.row(j) - x.row(j-1) - gamma(j-1) * (x.row(j-1) - x.row(j-2));  // first diff RW on locations
      jnll += nll_dens(mu);
    }
  }
  
  ADREPORT(sigma_g);
  ADREPORT(sigma);
  
  return jnll;
}
#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif
