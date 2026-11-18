#ifndef mpm_hpp
#define mpm_hpp 1

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

using namespace density;

template <class Type>
Type mpm(objective_function<Type>* obj) {
  
  DATA_MATRIX(x);                   // locations
  DATA_VECTOR(dt);                  // dt is time interval between x_i and x_{i-1}
  PARAMETER_VECTOR(g);		          // move persistence (see move_persistence.hpp)
  PARAMETER_VECTOR(log_sigma);	      // Innovation variance (log scale)
  PARAMETER(log_sigma_g);             // SD of the random walk on g (log scale)
  
  
  // transform parameters
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
    // prior on the first g, random walk on g, and the soft barrier at 0, 1
    jnll += g_track_nll(g, dt, sigma_g, 0, int(x.rows()) - 1);
    
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
