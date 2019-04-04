## Submission
This version addresses a compile error on r-patched-solaris-x86
* I have modified C++ source code to avoid `sqrt(int)` overload ambiguity compile error on Solaris platforms
* I am unable to verify this fix with testing on a comparable Solaris platform

## Test environments
* local OS X install (10.14.3), R 3.5.3
* ubuntu 16.04.5 (on travis-ci), R 3.5.2, R-devel
* win-builder (release, R-devel)

## R CMD check results

0 errors | 0 warnings | 1 note

NOTES:  

1. installed C++ files total 12.3 Mb (OS X), 19.5 Mb (windows), 28.1 Mb (linux). I have been unable to reduce this further.  
