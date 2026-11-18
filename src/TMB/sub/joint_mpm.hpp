#ifndef joint_mpm_hpp
#define joint_mpm_hpp 1

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

using namespace density;

template <class Type>
Type joint_mpm(objective_function<Type>* obj) {
  
  DATA_MATRIX(x);                   // locations
  DATA_INTEGER(A);                  // number of animals
  DATA_VECTOR(dt);                  // dt is time interval between x_i and x_{i-1}
  DATA_IVECTOR(idx);                // cumsum of number of locations for each animal
  
  PARAMETER_VECTOR(g);		          // move persistence (see move_persistence.hpp)
  PARAMETER_VECTOR(log_sigma);	      // Innovation variance (log scale)
  PARAMETER(log_sigma_g);             // SD of the random walk on g (log scale)
  
  // Backtransform parameters from link scale
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
  
  int i,j;
  
  for(i = 0; i < A; ++i) {
    // prior on the track's first g, random walk on g, and the soft barrier
    // at 0, 1. One track with an unidentified g breaks the joint fit, so
    // every track needs the prior on its first g.
    jnll += g_track_nll(g, dt, sigma_g, idx(i), idx(i+1) - 1);
    
    for(j = (idx(i)+2); j < idx(i+1); ++j){
      // var-cov depends on time interval
      cov.setZero();
      cov(0,0) = sigma(0) * sigma(0) * dt(j) * dt(j);
      cov(1,1) = sigma(1) * sigma(1) * dt(j) * dt(j);
      
      mu = x.row(j) - x.row(j-1) - g(j) * (dt(j)/dt(j-1)) * (x.row(j-1) - x.row(j-2));  // first diff RW on locations
      
      MVNORM_t<Type> nll_dens(cov);   // Multivariate Normal density
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
