checkTMBPackageVersion <- function() {
  ## aniMotum should be used with the version of TMB it was originally built
  ##  against. Earlier TMB versions may work unstably, later versions will
  ##  likely cause errors when attempting to fit aniMotum models.
  
  file <- paste0(system.file(package="aniMotum"), "/TMB-version")
  cur.TMB.version <- as.character(utils::packageVersion("TMB"))
  if(!file.exists(file)) {
    writeLines(cur.TMB.version, con = file)
  }
  
  aniMotum.TMB.version <- readLines(file)
  if(!identical(aniMotum.TMB.version, cur.TMB.version)) {
    warning(
      "Package version inconsistency detected.\n",
      "aniMotum was built with TMB version ",
      aniMotum.TMB.version,
      "\n",
      "Current TMB version is ",
      cur.TMB.version,
      "\n",
      paste0("Please re-install 'aniMotum' from source using install.packages('aniMotum', type = 'source') ",
      "or remove your current TMB version and re-install using remotes::install_version('TMB', version = '", 
          aniMotum.TMB.version, "')")
    )
  }
}

.onLoad <- function(lib, pkg) {
#  library.dynam("aniMotum", pkg, lib)
  checkTMBPackageVersion()
}

.onUnload <- function(libpath) {
#  library.dynam.unload("aniMotum", libpath)
}
