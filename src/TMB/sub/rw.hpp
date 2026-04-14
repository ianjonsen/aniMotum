#include <cmath>
#ifndef rw_hpp
#define rw_hpp 1

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

using namespace density; 
using std::sqrt;

template <class Type>
Type rw(objective_function<Type>* obj) {
  
  // DATA
  DATA_ARRAY(Y);	                  //  (x, y) observations
  DATA_VECTOR(dt);         	        //  time diff in some appropriate unit. this should contain dt for both interp and obs positions.
  DATA_VECTOR(state0);              //  initial state
  DATA_IVECTOR(isd);                //  indexes observations (1) vs. interpolation points (0)
  DATA_IVECTOR(obs_mod);            //  indicates which obs error model to be used
  DATA_ARRAY_INDICATOR(keep, Y);    // for one step predictions
  DATA_IVECTOR(gap_flag);           //  1 = long data gap precedes this time step.
                                    //  Declared for API consistency with crw and mp models.
                                    //  In the RW, gaps are already expressed through the dt^2
                                    //  variance scaling -- there is no directional persistence
                                    //  to suppress -- so gap_flag is a structural no-op here.
  DATA_IVECTOR(ho_flag);            //  1 = haulout: scale process variance by ho_scale to keep
                                    //  location estimates nearly stationary. gap_flag takes
                                    //  precedence (ho_flag is cleared wherever gap_flag is set
                                    //  on the R side before reaching this template).
  DATA_SCALAR(ho_scale);            //  variance scale factor during haulout (0, 1].
                                    //  Small values (e.g. 0.01) make the process nearly
                                    //  stationary regardless of observation precision or density.
                                    //  Supplied via ssm_control(ho_scale = ...).
  
  // for KF observation model
  DATA_VECTOR(m);                 //  m is the semi-minor axis length
  DATA_VECTOR(M);                 //  M is the semi-major axis length
  DATA_VECTOR(c);                 //  c is the orientation of the error ellipse
  // for LS/GPS observation model
  DATA_MATRIX(K);                 // error weighting factors for LS obs model
  // for GL observation model
  DATA_MATRIX(GLerr);             // error SD's in lon, lat for GL obs model
  
  // PROCESS PARAMETERS
  // for RW
  PARAMETER_VECTOR(l_sigma);    //  Innovation variance (link scale)
  PARAMETER(l_rho_p);           //  Innovation correlation (link scale)
  PARAMETER_ARRAY(X);           //  Predicted locations - length(X) same as length(dt)
  
  // OBSERVATION PARAMETERS
  // for KF OBS MODEL
  PARAMETER(l_psi); 				  // error SD scaling parameter to account for possible uncertainty in Argos error ellipse variables
  // for LS/GPS OBS MODEL
  PARAMETER_VECTOR(l_tau);     	// error dispersion for LS obs model (log scale)
  PARAMETER(l_rho_o);             // error correlation

  // Transform parameters
  vector<Type> sigma = exp(l_sigma);
  Type rho_p = Type(2.0) / (Type(1.0) + exp(-l_rho_p)) - Type(1.0);
  vector<Type> tau = exp(l_tau);
  Type rho_o = Type(2.0) / (Type(1.0) + exp(-l_rho_o)) - Type(1.0);
  Type psi = exp(l_psi);
  
  /* Define likelihood */
  Type jnll = 0.0;
  
  // 2 x 2 covariance matrix for innovations
  matrix<Type> cov(2, 2);
  matrix<Type> cov_dt(2, 2);
    
  cov(0, 0) = sigma(0) * sigma(0);
  cov(0, 1) = rho_p * sigma(0) * sigma(1);
  cov(1, 0) = cov(0, 1);
  cov(1, 1) = sigma(1) * sigma(1);
    
  MVNORM_t<Type> nll_proc(cov);
    
  // ---------------------------------------------------------------------------
  // RW PROCESS MODEL
  //
  // Two cases:
  //
  //  ho_flag(i) == 1  [haulout]
  //    Scale cov_dt by ho_scale to keep estimated locations nearly stationary.
  //    For GPS / dense data the tight observations already anchor locations, but
  //    for Argos / sparse data the tightened variance prevents location drift
  //    during haulout regardless of observation quality.
  //    gap_flag takes precedence over ho_flag: a haulout period long enough to
  //    exceed gap.thresh is treated as a normal gap (large variance, free drift)
  //    rather than a tightly-constrained period. This priority is enforced on
  //    the R side (ho_flag cleared where gap_flag is set) so no explicit check
  //    is needed here.
  //
  //  normal step (including gap_flag steps)
  //    Standard dt^2 scaled variance. gap_flag is a no-op in the RW: long
  //    gaps are already expressed through the large dt, inflating variance
  //    naturally without any additional mechanism.
  // ---------------------------------------------------------------------------

  for(int i = 1; i < dt.size(); i++) {
    if(ho_flag(i) == 1) {
      cov_dt = ho_scale * dt(i) * dt(i) * cov;
    } else {
      cov_dt = dt(i) * dt(i) * cov;
    }
    nll_proc.setSigma(cov_dt);
    jnll += nll_proc(X.col(i) - X.col(i - 1));
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
        
      } else {
        Rf_error ("C++: unexpected obs_mod value");
      }
      
      nll_obs.setSigma(cov_obs);   // set up i-th obs cov matrix
      jnll += nll_obs((Y.col(i) - X.col(i)), keep.col(i));   // RW innovations
      
      SIMULATE {
        Y.col(i) = nll_obs.simulate() + X.col(i);
        REPORT(Y);
      }  
    } else if(isd(i) == 0) {
      continue;
    } else {  
      Rf_error ("C++: unexpected isd value");
    }
  }
  
  ADREPORT(rho_p);
  ADREPORT(sigma);
  ADREPORT(rho_o);
  ADREPORT(tau);
  ADREPORT(psi);
  
  return jnll;
}
#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif
