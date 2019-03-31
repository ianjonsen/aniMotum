## Resubmission
This is a resubmission. In this version I have:
* used the lower case for non-specific names in Description
* used single quotes around the package name 'TMB' in Description
* removed "Internal function" descriptions in exported functions
* included small executable examples for all exported functions 
* further reduced size of example data files
* removed a plot function that produces complex errors on some R-devel platforms

## Test environments
* local OS X install (10.14.3), R 3.5.3
* ubuntu 16.04.5 (on travis-ci), R 3.5.2, r-devel
* windows server 2008 R2 SP1 (rhub), release, r-devel, 32/64 bit
* win-builder, release 

## R CMD check results

0 errors | 0 warnings | 2 notes

NOTES:  

1. This is a new release.  

2. installed C++ files total 12.9 Mb (OS X), 19.5 Mb (windows), 28.2 Mb (linux). I have been unable to reduce this further.  
