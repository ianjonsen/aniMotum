#ifndef mrm_hpp
#define mrm_hpp 1

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

using namespace density;

// Movement rate model ("mrm"), fitted to a track whose locations are treated
// as data (e.g. one imputed track of a multiple imputation).
//
// Displacements (km, east and north on the ground) follow a Brownian random
// walk whose rate varies in time:
//   d(i) ~ N(0, exp(2 * l_rate(i)) * dt(i) * R),  R = [1, rho_p; rho_p, 1],
// so exp(l_rate(i)) is the standard deviation of displacement along each axis
// per square root of hour (km h^-1/2). The displacement variance grows with
// elapsed time, so the rate does not depend on the time step.
//
// The log rate is a random walk in continuous time:
//   l_rate(i) - l_rate(i-1) ~ N(0, sigma_rate^2 * dt(i)).
//
// The locations are data, so every displacement is fixed and the likelihood is
// bounded: a rate far below the observed displacements is penalised by the
// squared displacement. (With latent locations, as in a one-step SSM, the
// joint likelihood is unbounded: locations can be pulled together while the
// log rate is driven towards -Inf.)
template <class Type>
Type mrm(objective_function<Type>* obj) {

  DATA_MATRIX(d);                   // displacements (km): east, north; row i is from i-1 to i
  DATA_VECTOR(dt);                  // time differences (h)
  DATA_IVECTOR(start);              // 1 = first location of a track segment (no displacement)

  PARAMETER_VECTOR(l_rate);         // log movement rate, one per location
  PARAMETER(l_sigma_rate);          // log SD of the random walk on l_rate (per h^1/2)
  PARAMETER(l_rho_p);               // correlation between east and north displacements (link scale)

  Type sigma_rate = exp(l_sigma_rate);
  Type rho_p = Type(2.0) / (Type(1.0) + exp(-l_rho_p)) - Type(1.0);

  Type jnll = 0.0;

  // random walk on the log movement rate
  for (int i = 1; i < dt.size(); ++i) {
    jnll -= dnorm(l_rate(i), l_rate(i - 1), sqrt(dt(i)) * sigma_rate, true);
  }

  // displacements
  matrix<Type> R(2, 2);
  R(0, 0) = Type(1.0);
  R(0, 1) = rho_p;
  R(1, 0) = rho_p;
  R(1, 1) = Type(1.0);
  matrix<Type> cov_dt(2, 2);
  MVNORM_t<Type> nll_proc(R);
  vector<Type> di(2);

  for (int i = 1; i < dt.size(); ++i) {
    if (start(i) == 1) continue;
    cov_dt = exp(Type(2.0) * l_rate(i)) * dt(i) * R;
    nll_proc.setSigma(cov_dt);
    di(0) = d(i, 0);
    di(1) = d(i, 1);
    jnll += nll_proc(di);
  }

  ADREPORT(sigma_rate);
  ADREPORT(rho_p);

  return jnll;
}
#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif
