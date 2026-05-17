install.packages("terra")
library(terra)
library(geodata)
library(raster)
 bioclim_data <- worldclim_global(var = "bio", res = 0.5, path = "data/")

coords <- read.csv("longlat_May152025.csv")

#BIO1
bio1 <- raster("wc2.1_30s_bio_1.tif")
crs(bio1) <- "+proj=longlat +datum=WGS84 +no_defs"
coords$bio1 <- extract(bio1, coords) 
write.csv(coords, "my_points_with_bio1.csv", row.names = FALSE)

#BIO2
bio2 <- raster("wc2.1_30s_bio_2.tif")
crs(bio2) <- "+proj=longlat +datum=WGS84 +no_defs"
coords <- data.frame(lon = -110.1644, lat = 44.3805)
value_raw <- extract(bio2, coords)
value_raw

coords <- read.csv("longlat_Nov152025.csv")

coords$bio2 <- extract(bio2, coords) 
write.csv(coords, "my_points_with_bio2.csv", row.names = FALSE)

#BIO7
bio7 <- raster("wc2.1_30s_bio_7.tif")
crs(bio7) <- "+proj=longlat +datum=WGS84 +no_defs"
coords <- data.frame(lon = -110.1644, lat = 44.3805)
value_raw <- extract(bio7, coords)
value_raw

coords <- read.csv("longlat_Nov152025.csv")

coords$bio7 <- extract(bio7, coords) 
write.csv(coords, "my_points_with_bio7.csv", row.names = FALSE)

#BIO8
bio8 <- raster("wc2.1_30s_bio_8.tif")
crs(bio8) <- "+proj=longlat +datum=WGS84 +no_defs"
coords <- data.frame(lon = -110.1644, lat = 44.3805)
value_raw <- extract(bio8, coords)
value_raw

coords <- read.csv("longlat_Nov152025.csv")

coords$bio8 <- extract(bio8, coords) 
write.csv(coords, "my_points_with_bio8.csv", row.names = FALSE)

#BIO9
bio9 <- raster("wc2.1_30s_bio_9.tif")
crs(bio9) <- "+proj=longlat +datum=WGS84 +no_defs"
coords <- data.frame(lon = -110.1644, lat = 44.3805)
value_raw <- extract(bio9, coords)
value_raw

coords <- read.csv("longlat_Nov152025.csv")

coords$bio9 <- extract(bio9, coords) 
write.csv(coords, "my_points_with_bio9.csv", row.names = FALSE)

#BIO10
bio10 <- raster("wc2.1_30s_bio_10.tif")
crs(bio10) <- "+proj=longlat +datum=WGS84 +no_defs"
coords <- data.frame(lon = -110.1644, lat = 44.3805)
value_raw2 <- extract(bio10, coords)
value_raw2

coords <- read.csv("longlat_Nov152025.csv")

coords$bio10 <- extract(bio10, coords) 
write.csv(coords, "my_points_with_bio10.csv", row.names = FALSE)

#BIO11
bio11 <- raster("wc2.1_30s_bio_11.tif")
crs(bio11) <- "+proj=longlat +datum=WGS84 +no_defs"
coords <- data.frame(lon = -110.1644, lat = 44.3805)
value_raw2 <- extract(bio11, coords)
value_raw2


coords <- read.csv("longlat_Nov152025.csv")

coords$bio11 <- extract(bio11, coords) 
write.csv(coords, "my_points_with_bio11.csv", row.names = FALSE)

#BIO12
bio12 <- raster("wc2.1_30s_bio_12.tif")
crs(bio12) <- "+proj=longlat +datum=WGS84 +no_defs"
coords <- data.frame(lon = -110.1644, lat = 44.3805)
value_raw2 <- extract(bio12, coords)
value_raw2

coords <- read.csv("longlat_Nov152025.csv")

coords$bio12 <- extract(bio12, coords) 
write.csv(coords, "my_points_with_bio12.csv", row.names = FALSE)

#BIO15
bio15 <- raster("wc2.1_30s_bio_15.tif")
crs(bio15) <- "+proj=longlat +datum=WGS84 +no_defs"
coords <- data.frame(lon = -110.1644, lat = 44.3805)
value_raw2 <- extract(bio15, coords)
value_raw2

coords <- read.csv("longlat_Nov152025.csv")

coords$bio15 <- extract(bio15, coords) 
write.csv(coords, "my_points_with_bio15.csv", row.names = FALSE)

## CALCULATE DAYLENGTH 
library(geosphere)
dat <- read.csv ("cleaned_wolf_parturition_May15_final_2026.csv", header=TRUE)
dat$daylength_hours <- daylength(dat$Lat, dat$originalMatingDate)
write.csv(dat, "cleaned_wolf_parturition_May15_final_2026_withdaylength.csv", row.names = FALSE)


#NDVI
install.packages(
  "MODISTools",
  repos = c("https://bluegreen-labs.r-universe.dev", "https://cloud.r-project.org")
)
library(MODISTools)
#Define your point and time range
library(terra)
library(MODISTools)

start_date <- "2022-01-01"
end_date   <- "2022-12-31"

# Download NDVI time series via MODIS Subsets API
ndvi_df <- mt_subset(
  product = "MOD13Q1",
  band    = "250m_16_days_NDVI",   # MODIS NDVI band
  lat     = lat,
  lon     = lon,
  start   = start_date,
  end     = end_date,
  km_lr   = 0,      # no extra buffer around the point
  km_ab   = 0,      # no extra buffer
  internal= TRUE
)

## Cloud-free NDVI

get_ndvi_stats_cloudfree <- function(lat, lon, year, filter_clouds = TRUE) {
  start_date <- paste0(year, "-01-01")
  end_date   <- paste0(year, "-12-31")

  library(readr)
points <- read.csv("csv_NDVI_measure.csv")
head(points)

# Prepare a results data frame
results <- data.frame(id = points$name,
                      lat = points$latitude,
                      lon = points$longitude,
                      mean_ndvi = NA,
                      sd_ndvi   = NA)

# Loop over each point
for(i in 1:nrow(points)) {
  cat("Processing point:", points$name[i], "\n")
  
  # Run cloud-filtered NDVI function
  res <- get_ndvi_stats_cloudfree(lat = points$latitude[i],
                                  lon = points$longitude[i],
                                  year = 2022,
                                  filter_clouds = TRUE)
  
  # Store mean and SD
  results$mean_ndvi[i] <- res$mean
  results$sd_ndvi[i]   <- res$sd
}

write.csv(results, "NDVI_results.csv", row.names = FALSE)


