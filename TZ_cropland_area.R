# Tanzania 2018 GeoSurvey 250m cropland area predictions
# M. Walsh, June 2019

# Required packages
# install.packages(c("downloader","rgdal","raster","MASS","ggplot2")), dependencies=TRUE)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
  require(MASS)
  require(ggplot2)
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

# download landcover raster masks
download("https://www.dropbox.com/s/ijbsk94cx4wlb5f/TZ_GS_preds.zip?raw=1", "TZ_GS_preds.zip", mode = "wb")
unzip("TZ_GS_preds.zip", overwrite = T)
glist <- list.files(pattern="tif", full.names = T)
grids <- stack(glist)

# Data setup --------------------------------------------------------------
# attach GADM-L3 admin unit names from shape
coordinates(geos) <- ~lon+lat
projection(geos) <- projection(shape)
gadm <- geos %over% shape
geos <- as.data.frame(geos)
geos <- cbind(gadm[ ,c(5,7,9)], geos)
colnames(geos) <- c("region","district","ward","lat","lon","bp","cp","wp","bcount","ccount","cprob","cpred")

# project GeoSurvey coords to grid CRS
geos.proj <- as.data.frame(project(cbind(geos$lon, geos$lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(geos.proj) <- c("x","y")
geos <- cbind(geos, geos.proj)
coordinates(geos) <- ~x+y
projection(geos) <- projection(grids)

# extract gridded variables at GeoSurvey locations
geosgrid <- extract(grids, geos)
gsdat <- as.data.frame(cbind(geos, geosgrid)) 
# gsdat <- gsdat[!duplicated(gsdat), ] ## removes any duplicates ... if needed
gsdat <- gsdat[complete.cases(gsdat[ ,c(8:57)]),] ## removes incomplete cases
# gsdat <- gsdat[ which(gsdat$CP=='Y'), ] ## selects croplands only
gsdat$observer <- sub("@.*", "", as.character(gsdat$observer)) ## shortens observer ID's

# Write data frame --------------------------------------------------------
dir.create("Results", showWarnings = F)
write.csv(gsdat, "./Results/TZ_gsdat.csv", row.names = F)

# Models ------------------------------------------------------------------
# negative binomial models of GeoSurvey cropland grid counts
summary(m1 <- glm.nb(ccount ~ cpred, geos)) ## scaling model
(est1 <- cbind(Estimate = coef(m1), confint(m1)))
# geos$m1 <- predict(m1, geos)

# +additional covariates
summary(m2 <- glm.nb(ccount ~ cpred+bcount+wp, geos))
(est2 <- cbind(Estimate = coef(m2), confint(m2)))
anova(m1, m2) ## model comparison

# zero-inflated negative binomial

# Plots -------------------------------------------------------------------
m1pred <- data.frame(cprob = rep(seq(from = 0.0, to = 1.0, length.out = 100)))
m1pred <- cbind(m1pred, predict(m1, m1pred, type = "link", se.fit=TRUE))
m1pred <- within(m1pred, {
  ccount <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

ggplot(m1pred, aes(cprob, ccount)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .5) +
  labs(x = "Cropland probability", y = "GeoSurvey cropland grid count")
