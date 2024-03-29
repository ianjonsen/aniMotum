# compile tmb models
TMBver <- as.character(packageVersion("TMB"))
write(TMBver, file = file.path("..", "..", "inst", "TMB-version"))

invisible(sapply(Sys.glob("*.cpp"),
                 TMB::compile,
                 safebounds = FALSE, safeunload = FALSE))
# copy dynlibs to src
invisible(file.copy(from = Sys.glob(paste0("*", .Platform$dynlib.ext)),
                    to = "..", overwrite = TRUE))
# cleanup done in ../Makevars[.win]

## args <- commandArgs(trailingOnly = TRUE)
## TMB::compile(args, safebounds = FALSE, safeunload = FALSE)