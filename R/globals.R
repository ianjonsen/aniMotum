## Variables that R CMD check cannot see being defined.
##  1) Data and parameters of the RTMB objective functions in jcrw_nll() and
##     jmp_nll(). RTMB::getAll() creates them in the function's environment at
##     run time. Note that dt here is the time-interval data vector, not
##     stats::dt.
##  2) Column names used in dplyr verbs in route_path().
utils::globalVariables(c(
  ## RTMB data and parameters
  "A", "an_mode", "c_eor", "dt", "est_ho", "g_rho_p", "g_sg", "G_tau",
  "gap_flag", "GLerr", "ho_flag", "ho_scale_fixed", "i_gl", "i_kf", "i_ls",
  "i1", "i2", "K", "l_aniso", "l_ho_scale", "l_psi", "l_rho_o", "l_rho_p",
  "l_sd_lsig", "l_sd_m", "l_sigma_g", "l_tau", "lg", "lsig_pop", "lsig_re",
  "m", "M", "m_ax", "m_mode", "m_pop", "m_re", "mu", "n_gl", "n_kf", "n_ls",
  "r_psi", "r_rho_o", "r_tau", "sig_mode", "state0", "theta", "u_re", "zeroA",
  ## dplyr column names
  "ho", "on_land"
))
