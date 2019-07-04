## Submission
This version addresses an error discovered on a reverse dependency check for the `sf` package. The geometry column in sf objects is no longer guaranteed to be the last column and this situation resulted in failed tests built into `foieGras`. 
* I have modified the test code to handle arbitrary positioning of the geometry column

## Test environments
* local OS X install (10.14.4), R 3.6.0
* ubuntu 16.04.5 (on travis-ci), R 3.6.0, R-devel
* fedora (via rhub), R-devel
* windows (via rhub), x86_64-release, x86_64-devel

## R CMD check results

0 errors | 0 warnings | 1 note

NOTES:  

1. installed C++ files total 12.3 Mb (OS X), 19.5 Mb (windows), 28.1 Mb (linux). I have been unable to reduce this further.  
