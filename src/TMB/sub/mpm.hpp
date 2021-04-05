#ifndef mpm_hpp
#define mpm_hpp 1

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

using namespace density;

template <class Type>
Type mpm(objective_function<Type>* obj) {
  
  DATA_MATRIX(x);                   // locations
  DATA_VECTOR(dt);                  // dt is time interval between x_i and x_{i-1}
  PARAMETER_VECTOR(lg);		          // Autocorrelation parameter 
  PARAMETER_VECTOR(log_sigma);	      // Innovation variance (log scale)
  PARAMETER(log_sigma_g);             // logistic scale parameter of rw on lg (log scale)
  
  
  // transform parameters
  vector<Type> g = Type(1.0) / (Type(1.0) + exp(-lg)); 
  vector<Type> sigma = exp(log_sigma);
  Type sigma_g = exp(log_sigma_g);
  
  // 2x2 covariance matrix for innovations
  matrix<Type> cov(2,2);
//  cov(0,0) = sigma(0) * sigma(0);
//  cov(0,1) = 0.0;
//  cov(1,0) = 0.0;
//  cov(1,1) = sigma(1) * sigma(1);
  
  Type jnll = 0.0;
  vector<Type> mu(2);
    //jnll -= dnorm(lg(0), Type(0.0), sigma_g, TRUE);
    for(int j = 1; j < x.rows(); ++j) {
      jnll -= dnorm(lg(j), lg(j-1), dt(j) * sigma_g, TRUE);  // RW on logit(gamma)
    }
    
    for(int j = 2; j < x.rows(); ++j){
      // var-cov depends on time interval
      cov.setZero();
      cov(0,0) = sigma(0) * sigma(0) * dt(j) * dt(j);
      cov(1,1) = sigma(1) * sigma(1) * dt(j) * dt(j);
      // first diff RW on locations
      mu = x.row(j) - x.row(j-1) - g(j) * (dt(j)/dt(j-1)) * (x.row(j-1) - x.row(j-2));  
      
      MVNORM_t<Type> nll_dens(cov);   // Multivariate Normal density
      jnll += nll_dens(mu);
    }
  
  ADREPORT(sigma_g);
  ADREPORT(sigma);
  
  return jnll;
}
#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif
