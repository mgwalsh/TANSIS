# Tanzania 2018 GeoSurvey 250m cropland area predictions
# M. Walsh, June 2019

# Required packages
# install.packages(c("downloader","rgdal","raster","MASS","caret")), dependencies=TRUE)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
  require(MASS)
  require(caret)
})

# Data downloads -----------------------------------------------------------
# set working directory
dir.create("TZ_crop_area", showWarnings = F)
setwd("./TZ_crop_area")

# download GeoSurvey data
# see sampling frame @ https://github.com/mgwalsh/Sampling/blob/master/TZ_GS_sample.R
download("https://www.dropbox.com/s/64u27k4pk3j4vz1/TZ_cropland_area.csv.zip?raw=1", "TZ_cropland_area.csv.zip", mode = "wb")
unzip("TZ_cropland_area.csv.zip", overwrite = T)
geos <- read.table("TZ_cropland_area.csv", header = T, sep = ",")

# download GADM-L3 shapefile (courtesy: http://www.gadm.org)
download("https://www.dropbox.com/s/bhefsc8u120uqwp/TZA_adm3.zip?raw=1", "TZA_adm3.zip", mode = "wb")
unzip("TZA_adm3.zip", overwrite = T)
shape <- shapefile("TZA_adm3.shp")

# attach GADM-L3 admin unit names from shape
coordinates(geos) <- ~lon+lat
projection(geos) <- projection(shape)
gadm <- geos %over% shape
geos <- as.data.frame(geos)
geos <- cbind(gadm[ ,c(5,7,9)], geos)
colnames(geos) <- c("region","district","ward","lat","lon","bp","cp","wp","bcount","ccount","cprob","cpred")

# Models ------------------------------------------------------------------
# negative binomial models of GeoSurvey cropland grid counts
summary(m1 <- glm.nb(ccount ~ cprob, geos))
(est <- cbind(Estimate = coef(m1), confint(m1)))

