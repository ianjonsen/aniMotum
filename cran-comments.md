## Submission
This is a resubmission, addressing issues identified by CRAN on initial submission:
1. package tarball size reduced to 4.8 Mb
2. invalid url in README.md updated

## Test environments
* local macOS arm-64 (12.3.1), R 4.2.0, R 4.1.3
* macOS 11.2.3 (via GitHub Actions), r-devel, r-release, r-oldrel
* ubuntu 20.04 (via GitHub Actions), r-devel, r-release, r-oldrel
* windows latest (via GitHub Actions), r-devel, r-release
* windows x86_64 (via win-builder), r-devel, r-release, r-oldrel
* solaris 10 x86 32 bit (via R-hub), r-release


## R CMD check results
0 errors | 0 warnings | up to 2 notes, depending on platform

NOTES:  

1. installed size of up to 21.5 Mb (macOS), 29 Mb (windows), 61.2 Mb (linux). 
I have been unable to reduce this further.  

2. 2 suggested packages (urls in Additional_repositories) are not available 
for testing.