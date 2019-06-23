# Tanzania 2018 GeoSurvey 250m cropland area predictions
# M. Walsh, June 2019

# Required packages
# install.packages(c("downloader","rgdal","raster","MASS","arm",leaflet","htmlwidgets")), dependencies=TRUE)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
  require(MASS)
  require(arm)
  require(leaflet)
  require(htmlwidgets)
})

# set working directory
dir.create("TZ_crop_area", showWarnings = F)
setwd("./TZ_crop_area")

# Data dow!nloads -----------------------------------------------------------
# download GeoSurvey data
# see sampling frame @ https://github.com/mgwalsh/Sampling/blob/master/TZ_GS_sample.R
download("https://www.dropbox.com/s/f4ucx0mv7jqn12u/TZ_cropland_area.csv.zip?raw=1", "TZ_cropland_area.csv.zip", mode = "wb")
unzip("TZ_cropland_area.csv.zip", overwrite = T)
geos <- read.table("TZ_cropland_area.csv", header = T, sep = ",")

# download GADM-L3 shapefile (courtesy: http://www.gadm.org)
download("https://www.dropbox.com/s/bhefsc8u120uqwp/TZA_adm3.zip?raw=1", "TZA_adm3.zip", mode = "wb")
unzip("TZA_adm3.zip", overwrite = T)
shape <- shapefile("TZA_adm3.shp")

# download landcover rasters
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
colnames(geos) <- c("region","district","ward","lat","lon","bp","cp","wp","bcount","ccount")

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
gsdat <- gsdat[complete.cases(gsdat[ ,c(1:15)]),] ## removes incomplete cases

# Write data frame --------------------------------------------------------
dir.create("Results", showWarnings = F)
write.csv(gsdat, "./Results/TZ_crop_area.csv", row.names = F)

# Models ------------------------------------------------------------------
# binomial models of GeoSurvey cropland grid counts
# cp <-  gsdat[which(gsdat$cp=='Y'), ] ## actual cropland observations only
summary(m0 <- glm(cbind(ccount, 16-ccount) ~ 1, family=binomial, gsdat)) ## mean model
(est0 <- cbind(Estimate = coef(m0), confint(m0))) ## standard 95% confidence intervals

# with cropland spatial presence prediction (CP18)
summary(m1 <- glm(cbind(ccount, 16-ccount) ~ CP18, family=binomial, gsdat)) ## scaling model
(est1 <- cbind(Estimate = coef(m1), confint(m1))) ## standard 95% confidence intervals
m1.pred <- predict(grids, m1, type="response")
m1.area <- cellStats(m1.pred*6.25, sum) ## calculates total cropland area (ha)
plot(m1.pred, axes=F)
gsdat$m1 <- predict(m1, gsdat, type="response")

# +additional LCC covariates
summary(m2 <- glm(cbind(ccount, 16-ccount) ~ CP18+BP18+WP18, family=binomial, gsdat)) ## $BP18 predicted building presence, $WP18 predicted woody cover
(est2 <- cbind(Estimate = coef(m2), confint(m2))) ## standard 95% confidence intervals
anova(m1, m2) ## model comparison
m2.pred <- predict(grids, m2, type="response")
(m2.area <- cellStats(m2.pred*6.25, sum)) ## calculates total cropland area (ha)
plot(m2.pred, axes=F)
gsdat$m2 <- predict(m2, gsdat, type="response")
anova(m1, m2) ## model comparison

# Multilevel regressions
# post-stratified by regions
summary(m3 <- glmer(cbind(ccount, 16-ccount) ~ 1 + (1|region), family=binomial, gsdat))
(m3.ran <- ranef(m3)) ## extract random regional effects

# +additional LCC covariates
summary(m4 <- glmer(cbind(ccount, 16-ccount) ~ CP18+BP18+WP18 + (1|region), family=binomial, gsdat))
(m4.ran <- ranef(m4)) ## extract random regional effects
anova(m3, m4) ## model comparison

# Write prediction grids --------------------------------------------------
gspreds <- stack(m1.pred, m2.pred)
names(gspreds) <- c("m1","m2")
writeRaster(gspreds, filename="./Results/TZ_cp_area.tif", datatype="FLT4S", options="INTERLEAVE=BAND", overwrite=T)

# Prediction map widget ---------------------------------------------------
pred <- m2.pred*100 ## GeoSurvey cropland percentage
pal <- colorBin("Reds", domain = 0:100, na.color = "light grey") ## set color palette
w <- leaflet() %>% 
  setView(lng = mean(gsdat$lon), lat = mean(gsdat$lat), zoom = 7) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addRasterImage(pred, colors = pal, opacity = 0.6, maxBytes=6000000) %>%
  addLegend(pal = pal, values = values(pred), title = "Cropland area (%)")
w ## plot widget 
saveWidget(w, 'TZ_cp_area.html', selfcontained = T)

