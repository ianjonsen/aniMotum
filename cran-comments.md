## Submission
This update addresses errors found by CRAN incoming automated checks on previous submission. The update also fixes the last released version's errors listed under CRAN check results on most linux platforms, however, I am currently unable to test R-devel on the following platforms (via R-hub): Debian gcc; clang-UBSAN.

## Test environments
* local OS X install (10.14.6), R 3.6.1
* ubuntu 16.04.6 (via travis-ci), R-release, R-devel
* Debian clang (via R-hub), R-devel
* Debian gcc (via R-hub), R-release, R-patched
* windows (via win-builder), R-release, R-devel, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 1 note

NOTES:  

1. installed C++ files total 12.3 Mb (OS X), 19.6 Mb (windows), 33.1 Mb (linux). I have been unable to reduce this further.  
