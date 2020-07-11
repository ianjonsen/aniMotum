require(tidyverse)
require(foieGras)
require(sf)
require(future)
require(furrr)
require(wesanderson)

sese <- read_csv("~/Dropbox/collab/RAATD_st_data/RAATD_SOES_standardized.csv", 
                 col_types = cols(
                   individual_id = col_character(),
                   breeding_stage = col_character(),
                   year = col_double(),
                   month = col_double(),
                   day = col_double(),
                   time = col_time(format = ""),
                   decimal_latitude = col_double(),
                   decimal_longitude = col_double(),
                   location_quality = col_character(),
                   location_to_keep = col_double()
                 )) 

sese <- sese %>% 
  filter(location_to_keep == 1) %>% 
  rename(id = individual_id, lat=decimal_latitude, lon=decimal_longitude, lc=location_quality) %>%
  select(id, year, month, day, time, lc, lon, lat) %>%
  mutate(date = paste(paste(year, month, day, sep="-"), time)) %>% 
  mutate(date = lubridate::ymd_hms(date)) %>% 
  select(id, date, lc, lon, lat, year) %>%
  filter(year %in% c(2009,2010,2011,2012,2013,2014)) %>%
  select(-year) %>%
  filter(lat < -30 & lat > -80) %>%
  nest_by(id, .keep = TRUE)

## drop any tracks with < 1000 observations
len <- sapply(sese$data, nrow) >= 1000
sese <- sese[len, ]

## get start dates for each deployment
st <- sapply(sese$data, function(x) min(x$date)) %>% 
  as.POSIXct(., origin = "1970-01-01 00:00:00", tz = "UTC")

## 2009 deployments
#s2009 <- sese[which(lubridate::year(st) == 2009), "data"] %>%
#  unnest(., cols = c(data))


## parallelise foieGras::fit_ssm
plan("multisession")
sese_fit <- sese$data %>%
  future_map(~ try(
    fit_ssm(
      d = .x,
      vmax = 4,
      model = "crw",
      time.step = 4,
      verbose = 0
    ), silent = TRUE),
    .progress = TRUE
  )
      
## drop convergence failures & cases where Hessian is not PD
cH <- sapply(sese_fit, function(x) class(x$pdHess))
idx <- which(cH == "list")
if(length(idx) > 0) {
  sese_fit <- sese_fit[-idx]
} 
sese_fit <- sese_fit %>%
  bind_rows() %>%
  filter(converged)

saveRDS(sese_fit, file = "data/sese_fit.RDS", compress = "xz")

## grab predicted & observed locations - can't use grab as projections differ among fits
plocs <- lapply(sese_fit$ssm, function(x) st_transform(x$predicted, crs = "+proj=stere +units=km +ellps=WGS84")) %>%
  bind_rows()
dlocs <- lapply(sese_fit$ssm, function(x) st_transform(x$data, crs = "+proj=stere +units=km +ellps=WGS84")) %>%
  bind_rows()

## get world coastline
world_sf <- st_as_sf(rworldmap::getMap(resolution = "high")) %>%
  st_transform(., crs = "+proj=stere +units=km +ellps=WGS84")

## define colour palette
wpal <- wes_palette("Zissou1")

## define bounds of predicted data
bounds <- plocs %>% st_bbox()

## cast predicted locations to lines
plines <- plocs %>%
  group_by(id) %>%
  summarise(do_union = FALSE, groups = "drop") %>%
  st_cast("MULTILINESTRING")

## cast observed location to MULTIPOINT
dlocs <- dlocs %>%
  filter(keep) %>%
  group_by(id) %>%
  summarise(do_union = FALSE, groups = "drop") %>%
  st_cast("MULTIPOINT")

## map
map <- ggplot() +
  geom_sf(data = dlocs, size = 0.75, colour = wpal[2], alpha = 0.5) +
  geom_sf(data = plines, size = 0.2, colour = wpal[5]) +
  geom_sf(data = world_sf, fill = "snow2", lwd = 0, colour = NA) +
  coord_sf(xlim = bounds[c("xmin","xmax")], ylim = bounds[c("ymin","ymax")]) +
  theme_minimal() +
  theme(panel.grid = element_line(size = 0.4)) #,
        #panel.background = element_rect(fill = wpal[2]))

ggsave(filename = "sese_map.png", plot = map)  

fmap(sese_fit, "p", size=0.8, conf=F, crs = "+proj=stere +units=km +ellps=WGS84")
