#ifndef ou_hpp
#define ou_hpp 1

#include "observation_covariance.hpp"

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

using namespace density;

// Ornstein-Uhlenbeck velocity process (Johnson, London, Lea & Durban 2008,
// Ecology 89: 1208-1215). Per coordinate, velocity v is an OU process with
// decay rate beta (per hour) and mean 0; location is the integral of v:
//
//   dv = -beta v dt + sigma dW,    dx = v dt,    sigma^2 = 2 D.
//
// sigma^2 = 2 D makes D the same quantity as in the crw model, which is the
// limit beta -> 0 of this model. The transition over a step of length dt from
// (x0, v0) is Gaussian, with a = beta * dt:
//
//   E[x]     = x0 + v0 dt phi1(a)          E[v] = v0 exp(-a)
//   Var[x]   = sigma^2 dt^3 phi3(a)        Var[v] = sigma^2 dt phi2(a)
//   Cov[x,v] = sigma^2 dt^2 phi1(a)^2 / 2
//
//   phi1(a) = (1 - exp(-a)) / a
//   phi2(a) = (1 - exp(-2a)) / (2a)
//   phi3(a) = (a - 2 (1 - exp(-a)) + (1 - exp(-2a)) / 2) / a^3
//
// All three tend to 1 (phi3 to 1/3) as a -> 0. Their exact forms lose
// precision there by cancellation, so below OU_SERIES_LIMIT their Taylor
// series are used instead. CppAD evaluates both branches of a conditional
// expression, so the exact branch is evaluated at a value no smaller than the
// limit, which keeps it finite.

const double OU_SERIES_LIMIT = 1e-2;  // a = beta * dt below which series are used
const double OU_NUGGET = 1e-5;        // added to the location variances, as in crw

template <class Type>
Type ou_phi1(Type a) {
  Type safe = CppAD::CondExpLt(a, Type(OU_SERIES_LIMIT), Type(OU_SERIES_LIMIT), a);
  Type exact = (Type(1.0) - exp(-safe)) / safe;
  Type series = Type(1.0) - a / Type(2.0) + a * a / Type(6.0) - a * a * a / Type(24.0);
  return CppAD::CondExpLt(a, Type(OU_SERIES_LIMIT), series, exact);
}

template <class Type>
Type ou_phi2(Type a) {
  return ou_phi1(Type(2.0) * a);
}

template <class Type>
Type ou_phi3(Type a) {
  Type safe = CppAD::CondExpLt(a, Type(OU_SERIES_LIMIT), Type(OU_SERIES_LIMIT), a);
  Type exact = (safe - Type(2.0) * (Type(1.0) - exp(-safe)) +
                (Type(1.0) - exp(Type(-2.0) * safe)) / Type(2.0)) / (safe * safe * safe);
  Type series = Type(1.0) / Type(3.0) - a / Type(4.0) + Type(7.0) * a * a / Type(60.0) -
                a * a * a / Type(24.0);
  return CppAD::CondExpLt(a, Type(OU_SERIES_LIMIT), series, exact);
}

// Negative log density of one transition, (mu, v) at i-1 to (mu, v) at i,
// for step length dt and decay rate beta. Sigma is the 2 x 2 covariance of the
// velocity diffusion between the two coordinates (sigma^2 = 2 D on the
// diagonal, correlation rho_p). The 4 x 4 covariance of (x, y, u, v) is
// S kron Sigma, where S is the 2 x 2 (location, velocity) covariance per unit
// sigma^2, so it is positive definite whenever Sigma is.
//
// persist = 0 drops the dependence on the previous velocity (data gaps and
// haulouts, as in crw); scale multiplies the whole covariance (ho_scale
// during haulouts, 1 otherwise).
template <class Type>
Type ou_step_nll(array<Type>& mu, array<Type>& v, int i, Type dt, Type beta,
                 matrix<Type> Sigma, int persist, Type scale) {
  Type a = beta * dt;
  Type p1 = ou_phi1(a);

  matrix<Type> S(2, 2);
  S(0, 0) = dt * dt * dt * ou_phi3(a);
  S(1, 1) = dt * ou_phi2(a);
  S(0, 1) = dt * dt * p1 * p1 / Type(2.0);
  S(1, 0) = S(0, 1);

  matrix<Type> cov(4, 4);
  for (int r = 0; r < 2; ++r) {
    for (int s = 0; s < 2; ++s) {
      for (int j = 0; j < 2; ++j) {
        for (int k = 0; k < 2; ++k) {
          cov(2 * r + j, 2 * s + k) = scale * S(r, s) * Sigma(j, k);
        }
      }
    }
  }
  cov(0, 0) += Type(OU_NUGGET);
  cov(1, 1) += Type(OU_NUGGET);

  vector<Type> r(4);
  for (int j = 0; j < 2; ++j) {
    Type v0 = persist ? v(j, i - 1) : Type(0.0);
    r(j) = mu(j, i) - (mu(j, i - 1) + v0 * dt * p1);
    r(j + 2) = v(j, i) - v0 * exp(-a);
  }
  MVNORM_t<Type> nll(cov);
  return nll(r);
}

// Negative log density of the first velocity: the stationary distribution of
// the OU process, Normal(0, Sigma / (2 beta)).
template <class Type>
Type ou_first_velocity_nll(array<Type>& v, Type beta, matrix<Type> Sigma) {
  matrix<Type> cov = Sigma / (Type(2.0) * beta);
  vector<Type> r(2);
  r(0) = v(0, 0);
  r(1) = v(1, 0);
  MVNORM_t<Type> nll(cov);
  return nll(r);
}

// Observation likelihood and speeds along the track
template <class Type>
Type ou_observation_nll(objective_function<Type>* obj, array<Type>& Y, array<Type>& mu,
                        data_indicator<array<Type>, Type>& keep, vector<int>& isd,
                        vector<int>& obs_mod, vector<Type>& tau, matrix<Type>& K,
                        vector<Type>& M, vector<Type>& m, vector<Type>& c, Type psi,
                        matrix<Type>& GLerr, Type rho_o, vector<Type>& KIrad,
                        matrix<Type>& KIjac, vector<Type>& kineis_scale,
                        Type kineis_exponent) {
  Type nll = 0.0;
  matrix<Type> cov_obs(2, 2);
  MVNORM_t<Type> nll_obs;
  for (int i = 0; i < isd.size(); ++i) {
    if (isd(i) == 1) {
      cov_obs = observation_covariance(obs_mod(i), i, tau, K, M, m, c, psi, GLerr, rho_o,
                                       KIrad, KIjac, kineis_scale, kineis_exponent);
      nll_obs.setSigma(cov_obs);
      nll += nll_obs((Y.col(i) - mu.col(i)), keep.col(i));
    } else if (isd(i) != 0) {
      Rf_error("C++: unexpected isd value");
    }
  }
  return nll;
}

template <class Type>
vector<Type> ou_speeds(array<Type>& mu, vector<int>& idx, vector<Type>& idt) {
  vector<Type> s(idx.size());
  s.setZero();
  for (int i = 1; i < idx.size(); ++i) {
    s(i) = sqrt(pow(mu(0, idx(i)) - mu(0, idx(i - 1)), 2) +
                pow(mu(1, idx(i)) - mu(1, idx(i - 1)), 2)) / idt(i);
  }
  return s;
}

// Constant decay rate beta
template <class Type>
Type ou(objective_function<Type>* obj) {

  // DATA
  DATA_ARRAY(Y);                    //  (x, y) observations
  DATA_VECTOR(dt);                  //  time differences (h), observation and prediction times
  DATA_VECTOR(state0);              //  initial state
  DATA_IVECTOR(isd);                //  observations (1) vs. prediction times (0)
  DATA_IVECTOR(fidx);
  DATA_VECTOR(fdt);
  DATA_IVECTOR(pidx);
  DATA_VECTOR(pdt);
  DATA_IVECTOR(obs_mod);            //  observation error model for each observation
  DATA_ARRAY_INDICATOR(keep, Y);    //  for one-step-ahead residuals
  DATA_SCALAR(se);                  //  delta-method standard errors for speeds on (1) or off (0)
  DATA_IVECTOR(gap_flag);           //  1 = long data gap: velocity does not persist across it
  DATA_IVECTOR(ho_flag);            //  1 = haulout: no persistence, covariance scaled by ho_scale
  DATA_SCALAR(ho_scale);            //  covariance scale factor during haulout, (0, 1]
  DATA_VECTOR(m);                   //  KF: semi-minor axis
  DATA_VECTOR(M);                   //  KF: semi-major axis
  DATA_VECTOR(c);                   //  KF: ellipse orientation
  DATA_MATRIX(K);                   //  LS, GPS: error weighting factors
  DATA_MATRIX(GLerr);               //  GL: error SDs
  DATA_VECTOR(KIrad);               //  KI: error radius (km)
  DATA_MATRIX(KIjac);               //  KI: projection Jacobian

  // PROCESS PARAMETERS
  PARAMETER_VECTOR(l_D);            //  log diffusion coefficients, sigma^2 = 2 D
  PARAMETER(l_beta);                //  log decay rate of velocity (per hour)
  PARAMETER(l_rho_p);               //  correlation of the x, y diffusions (link scale)
  PARAMETER_ARRAY(mu);              //  locations
  PARAMETER_ARRAY(v);               //  velocities

  // OBSERVATION PARAMETERS
  PARAMETER(l_psi);
  PARAMETER_VECTOR(l_tau);
  PARAMETER(l_rho_o);
  PARAMETER_VECTOR(l_kineis_scale);
  PARAMETER(l_kineis_exponent);

  vector<Type> tau = exp(l_tau);
  Type rho_p = Type(2.0) / (Type(1.0) + exp(-l_rho_p)) - Type(1.0);
  Type rho_o = Type(2.0) / (Type(1.0) + exp(-l_rho_o)) - Type(1.0);
  Type psi = exp(l_psi);
  vector<Type> kineis_scale = exp(l_kineis_scale);
  Type kineis_exponent = l_kineis_exponent;
  vector<Type> D = exp(l_D);
  Type beta = exp(l_beta);

  matrix<Type> Sigma(2, 2);
  Sigma(0, 0) = Type(2.0) * D(0);
  Sigma(1, 1) = Type(2.0) * D(1);
  Sigma(0, 1) = rho_p * sqrt(Sigma(0, 0) * Sigma(1, 1));
  Sigma(1, 0) = Sigma(0, 1);

  Type jnll = 0.0;
  Type tiny = 1e-5;

  // first location pinned to state0, as in crw; first velocity stationary
  for (int j = 0; j < 2; ++j) {
    jnll -= dnorm(mu(j, 0), state0(j), tiny, true);
  }
  jnll += ou_first_velocity_nll(v, beta, Sigma);

  for (int i = 1; i < dt.size(); ++i) {
    int persist = (gap_flag(i) == 1 || ho_flag(i) == 1) ? 0 : 1;
    Type scale = (ho_flag(i) == 1) ? ho_scale : Type(1.0);
    jnll += ou_step_nll(mu, v, i, dt(i), beta, Sigma, persist, scale);
  }

  jnll += ou_observation_nll(obj, Y, mu, keep, isd, obs_mod, tau, K, M, m, c, psi, GLerr,
                             rho_o, KIrad, KIjac, kineis_scale, kineis_exponent);

  vector<Type> sf = ou_speeds(mu, fidx, fdt);
  vector<Type> sp = ou_speeds(mu, pidx, pdt);

  ADREPORT(D);
  ADREPORT(beta);
  ADREPORT(rho_p);
  ADREPORT(rho_o);
  ADREPORT(tau);
  ADREPORT(psi);
  ADREPORT(kineis_scale);
  ADREPORT(kineis_exponent);
  if (se == 1) {
    ADREPORT(sf);
    ADREPORT(sp);
  } else {
    REPORT(sf);
    REPORT(sp);
  }

  return jnll;
}

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif
