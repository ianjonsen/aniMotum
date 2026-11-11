#ifndef observation_covariance_hpp
#define observation_covariance_hpp 1

// -----------------------------------------------------------------------------
// Observation error covariance matrix for one observation.
// Shared by the rw, crw and mp models.
//
// obs_mod values:
//   0 = Argos least-squares and GPS observations: standard deviations are the
//       error multiplication factors K scaled by tau
//   1 = Argos Kalman filter (or Kalman filtered and smoothed) observations:
//       error ellipse semi-major axis M, semi-minor axis m (scaled by psi) and
//       orientation c, with each semi-axis divided by sqrt(2)
//   2 = light-level geolocation observations: standard deviations in
//       longitude and latitude
//   3 = Kinéis observations (KI): the CLS error radius r (km) gives ground
//       standard deviations scale_east * r^exponent and
//       scale_north * r^exponent. These are mapped to the projected x, y
//       coordinates through the local Jacobian J of the projection
//       (KIjac row i: dx/dEast, dx/dNorth, dy/dEast, dy/dNorth), so that
//       cov = J diag(sd_east^2, sd_north^2) J'
// -----------------------------------------------------------------------------

template <class Type>
matrix<Type> observation_covariance(int obs_mod_i,
                                    int i,
                                    const vector<Type>& tau,
                                    const matrix<Type>& K,
                                    const vector<Type>& M,
                                    const vector<Type>& m,
                                    const vector<Type>& c,
                                    Type psi,
                                    const matrix<Type>& GLerr,
                                    Type rho_o,
                                    const vector<Type>& KIrad,
                                    const matrix<Type>& KIjac,
                                    const vector<Type>& kineis_scale,
                                    Type kineis_exponent) {

  matrix<Type> cov_obs(2, 2);

  if(obs_mod_i == 0) {
    // Argos Least Squares & GPS observations
    Type s = tau(0) * K(i,0);
    Type q = tau(1) * K(i,1);
    cov_obs(0,0) = s * s;
    cov_obs(1,1) = q * q;
    cov_obs(0,1) = s * q * rho_o;
    cov_obs(1,0) = cov_obs(0,1);

  } else if(obs_mod_i == 1) {
    // Argos Kalman Filter (or Kalman Filtered & Smoothed) observations
    double z = sqrt(2.);
    double h = 0.5;
    Type s2c = sin(c(i)) * sin(c(i));
    Type c2c = cos(c(i)) * cos(c(i));
    Type M2  = (M(i) / z) * (M(i) / z);
    Type m2  = (m(i) * psi / z) * (m(i) * psi / z);
    cov_obs(0,0) = (M2 * s2c + m2 * c2c);
    cov_obs(1,1) = (M2 * c2c + m2 * s2c);
    cov_obs(0,1) = (h * (M(i) * M(i) - (m(i) * psi * m(i) * psi))) * cos(c(i)) * sin(c(i));
    cov_obs(1,0) = cov_obs(0,1);

  } else if(obs_mod_i == 2) {
    // GL observations
    Type sdLon = GLerr(i,0);
    Type sdLat = GLerr(i,1);
    cov_obs(0,0) = sdLon * sdLon;
    cov_obs(1,1) = sdLat * sdLat;
    cov_obs(0,1) = sdLon * sdLat * rho_o;
    cov_obs(1,0) = cov_obs(0,1);

  } else if(obs_mod_i == 3) {
    // Kinéis observations
    Type radius_term = pow(KIrad(i), kineis_exponent);
    Type var_east = pow(kineis_scale(0) * radius_term, 2);
    Type var_north = pow(kineis_scale(1) * radius_term, 2);
    Type x_east = KIjac(i,0);
    Type x_north = KIjac(i,1);
    Type y_east = KIjac(i,2);
    Type y_north = KIjac(i,3);
    cov_obs(0,0) = x_east * x_east * var_east + x_north * x_north * var_north;
    cov_obs(1,1) = y_east * y_east * var_east + y_north * y_north * var_north;
    cov_obs(0,1) = x_east * y_east * var_east + x_north * y_north * var_north;
    cov_obs(1,0) = cov_obs(0,1);

  } else {
    Rf_error ("C++: unexpected obs_mod value");
  }

  return cov_obs;
}

#endif
