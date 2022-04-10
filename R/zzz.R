checkTMBPackageVersion <- function() {
  ## foieGras should be used with the version of TMB it was originally built
  ##  against. Earlier TMB versions may work unstably, later versions will
  ##  likely cause errors when attempting to fit foieGras models.
  
  file <- paste0(system.file(package="foieGras"), "/TMB-version")
  cur.TMB.version <- as.character(utils::packageVersion("TMB"))
  if(!file.exists(file)) {
    writeLines(cur.TMB.version, con = file)
  }
  
  foieGras.TMB.version <- readLines(file)
  if(!identical(foieGras.TMB.version, cur.TMB.version)) {
    warning(
      "Package version inconsistency detected.\n",
      "foieGras was built with TMB version ",
      foieGras.TMB.version,
      "\n",
      "Current TMB version is ",
      cur.TMB.version,
      "\n",
      paste0("Please re-install 'foieGras' from source using install.packages('foieGras', type = 'source') ",
      "or remove your current TMB version and re-install using remotes::install_version('TMB', version = '", 
          foieGras.TMB.version, "')")
    )
  }
}

.onLoad <- function(lib, pkg) {
#  library.dynam("foieGras", pkg, lib)
  checkTMBPackageVersion()
}

.onUnload <- function(libpath) {
#  library.dynam.unload("foieGras", libpath)
}
