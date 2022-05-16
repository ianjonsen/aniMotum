## Submission  
This is a 2nd resubmission, addressing issues identified by CRAN automated checks:  
1. vignette build time has been reduced to speed up pkg checks
2. url's have been replaced (although I am unsure how they were corrupted at submission)

## Test environments  
* local macOS arm-64 (12.3.1), R 4.2.0, R 4.1.3
* macOS 11.2.3 (via GitHub Actions), r-devel, r-release, r-oldrel  
* ubuntu 20.04 (via GitHub Actions), r-devel, r-release, r-oldrel  
* windows latest (via GitHub Actions), r-devel, r-release  
* windows x86_64 (via win-builder), r-devel, r-release, r-oldrel  
* solaris 10 x86 32 bit (via R-hub), r-release  
* ubuntu GCC (via R-hub), r-release
* debian GCC (via R-hub), r-devel


## R CMD check results  
0 errors | 0 warnings | up to 2 notes, depending on platform  

NOTES:  

1. installed size of up to 12.6 Mb (macOS), 13 Mb (windows), 52.5 Mb (linux). I have been unable to reduce this further.  

2. 2 suggested packages (urls in Additional_repositories) are not available for testing.