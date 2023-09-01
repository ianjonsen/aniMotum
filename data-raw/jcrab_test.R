require(tidyverse)
require(lubridate)
require(aniMotum)

jc <- readRDS("data-raw/Jcrab35.rds")
names(jc) <- tolower(names(jc))

d <- jc %>%
  select(id = transmitter,
         date = datetime,
         x,
         y,
         sd.x = sdx,
         sd.y = sdy) %>%
  mutate(date = mdy_hm(date, tz = "America/New_York")) %>%
  tibble()

#d1 <- format_data(d, coord = c("lon","lat"), sderr = c("sd.x","sd.y"))
d1 <- format_data(d, coord = c("x","y"), sderr = c("sd.x","sd.y"))

fit <- fit_ssm(d1, model = "crw", time.step = 5/60)
