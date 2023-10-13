## install dev version of aniMotum
remotes::install_github("ianjonsen/aniMotum@dev")
require(tidyverse)
require(aniMotum)

d <- read.csv("data-raw/SnowPetrelExample.csv")
names(d) <- tolower(names(d))
names(d)[1:2] <- c("id", "date")
d$lc <- rep("G", nrow(d))
d$date <- lubridate::dmy_hms(d$date, tz = "UTC")

fit <- fit_ssm(d, 
               vmax=20, 
               model = "crw", 
               time.step = 1, 
               map = list(rho_o = factor(NA)))

trs <- sim_fit(fit, 
               what = "p", 
               reps = 200, 
               start = c(-2.555, -69.938),
               end = c(9.633, -69.902))

plot(trs) + geom_point(data = data.frame(x=c(-2.555,9.633), 
                                         y = c(-69.938,-69.902), 
                                         pt = c("start","end")), 
                       aes(x,y, col = pt), 
                       shape = 17, 
                       size = 2)

trs1 <- sim_filter(trs, keep = 0.25)

plot(trs1) + geom_point(data = data.frame(x=c(-2.555,9.633), 
                                          y = c(-69.938,-69.902), 
                                          pt = c("start","end")), 
                        aes(x,y, col = pt), 
                        shape = 17, 
                        size = 2)
