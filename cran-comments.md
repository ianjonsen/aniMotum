## Submission
This update addresses the clang-UBSAN runtime errors found during two consecutive CRAN checks where NA's were passed from R to a C++ object of type int. Testing via rocker/r-devel-ubsan-clang has resulted in successful Rdevel CMD check --as-cran. 

## Test environments
* local OS X install (10.15.5), R 4.0.0
* ubuntu 16.04.6 (via travis-ci), r-release, r-devel, r-oldrelease
* Debian clang (via R-hub), r-devel
* windows (via win-builder), r-release, r-devel
* rocker/r-devel-ubsan-clang

## R CMD check results

0 errors | 0 warnings | 1 note

NOTES:  

1. installed C++ files total 13.9 Mb (OS X), 21.5 Mb (windows), 34.9 Mb (linux). I have been unable to reduce this further.  
