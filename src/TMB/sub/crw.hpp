#include <cmath>
#ifndef crw_hpp
#define crw_hpp 1

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

using namespace density; 
using std::sqrt;

template <class Type>
Type crw(objective_function<Type>* obj) {
  
  // DATA
  DATA_ARRAY(Y);	                  //  (x, y) observations
  DATA_VECTOR(dt);         	        //  time diff in some appropriate unit. this should contain dt for both interp and obs positions.
  DATA_VECTOR(state0);              //  initial state
  DATA_IVECTOR(isd);                //  indexes observations (1) vs. interpolation points (0)
  DATA_IVECTOR(obs_mod);            //  indicates which obs error model to be used
  DATA_ARRAY_INDICATOR(keep, Y);    // for one step predictions
  
  // for KF observation model
  DATA_VECTOR(m);                 //  m is the semi-minor axis length
  DATA_VECTOR(M);                 //  M is the semi-major axis length
  DATA_VECTOR(c);                 //  c is the orientation of the error ellipse
  // for LS/GPS observation model
  DATA_MATRIX(K);                 // error weighting factors for LS obs model
  // for GL observation model
  DATA_MATRIX(GLerr);             // error SD's in lon, lat for GL obs model
  
  // PROCESS PARAMETERS
  PARAMETER(logD);				  // 1-d Diffusion coefficient
  // random variables
  PARAMETER_ARRAY(mu);     /* State location */
  PARAMETER_ARRAY(v);      /* state velocities */
  
  // OBSERVATION PARAMETERS
  // for KF OBS MODEL
  PARAMETER(l_psi); 				  // error SD scaling parameter to account for possible uncertainty in Argos error ellipse variables
  // for LS/GPS OBS MODEL
  PARAMETER_VECTOR(l_tau);     	// error dispersion for LS obs model (log scale)
  PARAMETER(l_rho_o);             // error correlation
  
  // Transform parameters
  vector<Type> tau = exp(l_tau);
  Type rho_o = Type(2.0) / (Type(1.0) + exp(-l_rho_o)) - Type(1.0);
  Type psi = exp(l_psi);
  Type D = exp(logD);
  
  /* Define likelihood */
  Type jnll = 0.0;
  Type tiny = 1e-5;
  
  // Setup object for evaluating multivariate normal likelihood
  matrix<Type> cov(4,4);
  cov.setZero();
  cov(0,0) = tiny;
  cov(1,1) = tiny;
  cov(2,2) = 2 * D * dt(0);
  cov(3,3) = 2 * D * dt(0);
    
  // loop over 2 coords and update nll of start location and velocities.
  for(int i = 0; i < Y.rows(); i++) {
    jnll -= dnorm(mu(i,0), state0(i), tiny, true);
    jnll -= dnorm(v(i,0), state0(i+2), tiny, true);
  }
    
  // CRW PROCESS MODEL
  vector<Type> x_t(4);
  for(int i = 1; i < dt.size(); i++) {
    // process cov at time t
    cov.setZero();
    cov(0,0) = tiny;
    cov(1,1) = tiny;
    cov(2,2) = 2 * D * dt(i);
    cov(3,3) = 2 * D * dt(i);
      
    // location innovations
    x_t(0) = mu(0,i) - (mu(0,i-1) + (v(0,i) * dt(i)));
    x_t(1) = mu(1,i) - (mu(1,i-1) + (v(1,i) * dt(i)));
      
    // velocity innovations
    x_t(2) = (v(0,i) - v(0,i-1)); // /dt(i);
    x_t(3) = (v(1,i) - v(1,i-1)); // /dt(i);
    jnll += MVNORM<Type>(cov)(x_t); // Process likelihood
  }
  
  // OBSERVATION MODEL
  // 2 x 2 covariance matrix for observations
  matrix<Type> cov_obs(2, 2);
  MVNORM_t<Type> nll_obs; // Multivariate Normal for observations
  
  for(int i = 0; i < dt.size(); ++i) {
    if(isd(i) == 1) {
      if(obs_mod(i) == 0) {
        // Argos Least Squares & GPS observations
        Type s = tau(0) * K(i,0);
        Type q = tau(1) * K(i,1);
        cov_obs(0,0) = s * s;
        cov_obs(1,1) = q * q;
        cov_obs(0,1) = s * q * rho_o;
        cov_obs(1,0) = cov_obs(0,1);
        
      } else if(obs_mod(i) == 1) {
        // Argos Kalman Filter (or Kalman Filtered & Smoothed) observations
        double z = sqrt(2.);
        double h = 0.5;
        Type s2c = sin(c(i)) * sin(c(i));
        Type c2c = cos(c(i)) * cos(c(i));
        Type M2  = (M(i) / z) * (M(i) / z);
        Type m2 = (m(i) * psi / z) * (m(i) * psi / z);
        cov_obs(0,0) = (M2 * s2c + m2 * c2c);
        cov_obs(1,1) = (M2 * c2c + m2 * s2c);
        cov_obs(0,1) = (h * (M(i) * M(i) - (m(i) * psi * m(i) * psi))) * cos(c(i)) * sin(c(i));
        cov_obs(1,0) = cov_obs(0,1);
        
      } else if(obs_mod(i) == 2) {
        // GLS observations
        Type sdLon = GLerr(i,0);
        Type sdLat = GLerr(i,1);
        cov_obs(0,0) = sdLon * sdLon;
        cov_obs(1,1) = sdLat * sdLat;
        cov_obs(0,1) = sdLon * sdLat * rho_o;
        cov_obs(1,0) = cov_obs(0,1);
        
      } else{
        Rf_error ("C++: unexpected obs_mod value");
      }
      
      nll_obs.setSigma(cov_obs);   // set up i-th obs cov matrix
      jnll += nll_obs((Y.col(i) - mu.col(i)), keep.col(i));   // CRW innovations
        
    } else if(isd(i) == 0) {
      continue;
    } else {  
      Rf_error ("C++: unexpected isd value");
    }
  }
  

  ADREPORT(D);
  ADREPORT(rho_o);
  ADREPORT(tau);
  ADREPORT(psi);
  
  return jnll;
}
#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif

