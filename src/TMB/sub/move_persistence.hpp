#ifndef move_persistence_hpp
#define move_persistence_hpp 1

// Move persistence g, shared by the mpm, jmpm and mp models (and by the RTMB
// joint model in R/jmp_nll.R, which must be kept in step with this file).
//
// g is a random walk on its own scale, kept within 0 to 1 by a convex soft
// barrier, not passed through a logistic link. With a logistic link the
// locations' likelihood, quadratic in g, is not convex in logit(g): the inner
// (Laplace) problem can have several modes, and fits often failed with false
// convergence or a Hessian that was not positive definite. Here the random
// walk, the barrier and (in mpm, jmpm) the locations' likelihood are all
// convex in g, so the inner problem has one mode.
//
// The first g of each track has a diffuse Normal(0.5, 2) prior. Without it the
// random walk is improper: the level of a track whose g sits at a bound
// throughout is not identified and the inner Hessian is singular.

const double G_FIRST_MEAN = 0.5;    // mean of the prior on a track's first g
const double G_FIRST_SD = 2.0;      // SD of the prior on a track's first g
const double G_BOUND_SCALE = 0.02;  // width of the soft barrier at 0 and 1

// soft barrier: about 0 inside (0, 1), rising quadratically outside, as
// 0.5 * ((distance beyond the bound) / G_BOUND_SCALE)^2
template <class Type>
Type g_bound_nll(Type g) {
  Type s = Type(G_BOUND_SCALE);
  Type below = logspace_add(Type(0.0), -g / s);
  Type above = logspace_add(Type(0.0), (g - Type(1.0)) / s);
  return Type(0.5) * (below * below + above * above);
}

// negative log density of g along one track, elements first to last of g:
// prior on the first g, random walk with SD dt * sigma_g, and the barrier
template <class Type>
Type g_track_nll(vector<Type> g, vector<Type> dt, Type sigma_g, int first, int last) {
  Type nll = -dnorm(g(first), Type(G_FIRST_MEAN), Type(G_FIRST_SD), true);
  for (int j = first + 1; j <= last; ++j) {
    nll -= dnorm(g(j), g(j - 1), dt(j) * sigma_g, true);
  }
  for (int j = first; j <= last; ++j) {
    nll += g_bound_nll(g(j));
  }
  return nll;
}

#endif
