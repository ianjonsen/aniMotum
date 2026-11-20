## fit_mpm() is deprecated in favour of fit_bm(), but the tests still exercise
##  it directly. Mark its once-per-session deprecation warning as already
##  given, so the warning does not appear in the test results. The warning
##  itself is tested in test-fit_mpm.R
assign("fit_mpm_warned", TRUE, envir = aniMotum:::aniMotum_state)
