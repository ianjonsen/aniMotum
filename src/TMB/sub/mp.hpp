#ifndef mp_hpp
#define mp_hpp 1

#include "observation_covariance.hpp"

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

using namespace density;

template <class Type>
Type mp(objective_function<Type>* obj) {
  
  // DATA
  DATA_ARRAY(Y);                    //  (x, y) observations
  DATA_VECTOR(dt);                  //  time diff in some appropriate unit
  DATA_IVECTOR(isd);                //  indexes observations (1) vs. interpolation points (0)
  DATA_IVECTOR(obs_mod);            //  indicates which obs error model to be used
  DATA_ARRAY_INDICATOR(keep, Y);    //  for one step predictions
  DATA_IVECTOR(gap_flag);           //  1 = long data gap precedes this time step.
                                    //  Zeroes the correlated first-difference term
                                    //  and uses normal process variance.
  DATA_IVECTOR(ho_flag);            //  1 = haulout: animal is known to be stationary.
                                    //  Zeroes the correlated first-difference term AND
                                    //  scales process variance down by ho_scale, tightly
                                    //  constraining location drift. Applied instead of
                                    //  gap_flag when ho takes priority (see logic below).
                                    //  Handles Argos data and sparse-observation haulouts
                                    //  where observations alone cannot anchor locations.
  DATA_SCALAR(ho_scale);            //  Variance scale factor during haulout (0, 1).
                                    //  Small values (e.g. 0.01) make the process nearly
                                    //  stationary. Supplied via ssm_control(ho_scale).
  
  // for KF observation model
  DATA_VECTOR(m);                 //  m is the semi-minor axis length
  DATA_VECTOR(M);                 //  M is the semi-major axis length
  DATA_VECTOR(c);                 //  c is the orientation of the error ellipse
  // for LS/GPS observation model
  DATA_MATRIX(K);                 //  error weighting factors for LS obs model
  // for GL observation model
  DATA_MATRIX(GLerr);             //  error SD's in lon, lat for GL obs model
  // for Kinéis (KI) observation model
  DATA_VECTOR(KIrad);              //  Kinéis error radius (km)
  DATA_MATRIX(KIjac);              //  projection Jacobian: dx/dE, dx/dN, dy/dE, dy/dN
  
  // PROCESS PARAMETERS
  PARAMETER_VECTOR(l_sigma);      //  Innovation variance (link scale)
  PARAMETER(l_sigma_g);           //  logistic scale parameter of rw on lg (log scale)
  PARAMETER(l_rho_p);             //  Innovation correlation (link scale)
  PARAMETER_ARRAY(X);             //  Predicted locations
  PARAMETER_VECTOR(lg);           //  logit(gamma): move persistence on link scale
  // OBSERVATION PARAMETERS
  PARAMETER(l_psi);               //  error SD scaling parameter (KF obs model)
  PARAMETER_VECTOR(l_tau);        //  error dispersion for LS obs model (log scale)
  PARAMETER(l_rho_o);             //  observation error correlation b/w x,y
  // for Kinéis (KI) observation model
  PARAMETER_VECTOR(l_kineis_scale); // log error scales (east, north) relative to the error radius
  PARAMETER(l_kineis_exponent);     // exponent on the error radius (identity link)
  
  // Transform parameters
  vector<Type> sigma = exp(l_sigma);
  Type rho_p   = Type(2.0) / (Type(1.0) + exp(-l_rho_p)) - Type(1.0);
  vector<Type> tau = exp(l_tau);
  Type rho_o   = Type(2.0) / (Type(1.0) + exp(-l_rho_o)) - Type(1.0);
  Type psi     = exp(l_psi);
  vector<Type> kineis_scale = exp(l_kineis_scale);
  Type kineis_exponent = l_kineis_exponent;
  Type sigma_g = exp(l_sigma_g);
  vector<Type> g = Type(1.0) / (Type(1.0) + exp(-lg)); 
  
  Type jnll = 0.0;
  
  matrix<Type> cov(2, 2);
  matrix<Type> cov_dt(2, 2);
  
  cov(0, 0) = sigma(0) * sigma(0);
  cov(0, 1) = rho_p * sigma(0) * sigma(1);
  cov(1, 0) = cov(0, 1);
  cov(1, 1) = sigma(1) * sigma(1);
  
  MVNORM_t<Type> nll_proc(cov);
  vector<Type> mu(2);
  
  // ---------------------------------------------------------------------------
  // PROCESS MODEL
  // ---------------------------------------------------------------------------

  // RW on logit(gamma): variance scales with dt so gamma is naturally free to
  // drift over long gaps or haulout periods. No modification needed here --
  // during haulout, near-zero displacements (enforced by the tight location
  // process below) will drive gamma toward zero without explicit intervention.
  for(int i = 1; i < dt.size(); ++i) {
    jnll -= dnorm(lg(i), lg(i-1), dt(i) * sigma_g, TRUE);  
  }

  // Location first-differences.
  // First step is always a pure RW (no previous difference to condition on).
  jnll += nll_proc(X.col(1) - X.col(0));

  // ---------------------------------------------------------------------------
  // Three cases in the main process loop:
  //
  //  gap_flag(i) == 1  [data gap]
  //    Pure RW, normal process variance. Directional persistence is broken
  //    because there is no meaningful previous displacement to condition on.
  //    Uncertainty grows appropriately across the gap.
  //
  //  ho_flag(i) == 1  [haulout, not a gap]
  //    Pure RW, reduced process variance (ho_scale * dt * dt * cov).
  //    The correlated term is zeroed for the same reason as gap_flag, but
  //    the variance is also scaled down to prevent location drift when
  //    observations are imprecise (Argos) or sparse. ho_scale is small
  //    (e.g. 0.01), making the process nearly stationary during haulout.
  //    gap_flag takes precedence over ho_flag when both are set: a long
  //    haulout with no observations should be treated as a gap.
  //
  //  normal step
  //    Standard correlated first-difference process.
  // ---------------------------------------------------------------------------

  for(int i = 2; i < dt.size(); i++) {

    if(gap_flag(i) == 1) {
      // Data gap: pure RW, normal variance
      mu    = X.col(i) - X.col(i-1);
      cov_dt = dt(i) * dt(i) * cov;

    } else if(ho_flag(i) == 1) {
      // Haulout: pure RW, reduced process variance
      // ho_scale (e.g. 0.01) keeps locations nearly stationary regardless
      // of observation precision. Might need to rethink ho_scale...
      // The first step after a haulout
      // (i+1) recovers normal behaviour: dt(i+1)/dt(i) ~ 1 for
      // regular data so the near-zero haulout displacement contributes
      // little to the post-haulout correlated term.
      mu     = X.col(i) - X.col(i-1);
      cov_dt = ho_scale * dt(i) * dt(i) * cov;

    } else {
      // Normal step: correlated first-difference
      mu     = X.col(i) - X.col(i-1) - g(i) * (dt(i)/dt(i-1)) * (X.col(i-1) - X.col(i-2));
      cov_dt = dt(i) * dt(i) * cov;
    }

    nll_proc.setSigma(cov_dt);
    jnll += nll_proc(mu);
  }
  
  // ---------------------------------------------------------------------------
  // OBSERVATION MODEL
  // ---------------------------------------------------------------------------
  // Haulout observations remain in the likelihood. They constrain location
  // estimates during haulout for GPS/precise data; for Argos/sparse data the
  // tight process variance (ho_scale) does the heavy lifting.

  matrix<Type> cov_obs(2, 2);
  MVNORM_t<Type> nll_obs;
  
  for(int i = 0; i < dt.size(); ++i) {
    if(isd(i) == 1) {
      cov_obs = observation_covariance(obs_mod(i), i, tau, K, M, m, c, psi, GLerr, rho_o,
                                       KIrad, KIjac, kineis_scale, kineis_exponent);
      
      nll_obs.setSigma(cov_obs);
      jnll += nll_obs((Y.col(i) - X.col(i)), keep.col(i));
      
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
  ADREPORT(kineis_scale);
  ADREPORT(kineis_exponent);
  ADREPORT(sigma_g);
  
  return jnll;
}
#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif
