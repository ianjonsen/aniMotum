d <- read.csv("data-raw/SnowPetrelExample.csv")
names(d) <- tolower(names(d))
names(d)[1:2] <- c("id", "date")
d$lc <- rep("G", nrow(d))
d$date <- lubridate::dmy_hms(d$date, tz = "UTC")

fit <- fit_ssm(d |> dplyr::filter(seg == "sea"), 
               vmax=20, 
               model = "crw", 
               time.step = 1, 
               map = list(rho_o = factor(NA)))

## load gradient rasters for potential function
load(system.file("extdata/grad.rda", package = "aniMotum"))
trs <- sim_fit(fit, 
               what = "p", 
               reps = 5, 
               grad = grad, 
               beta = c(-35,-35),
               cpf = TRUE)

plot(y ~ x, grab(fit, "p"), cex = 0.3)



