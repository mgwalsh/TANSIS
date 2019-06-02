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

# attach GADM-L3 admin unit names from shape
coordinates(geos) <- ~lon+lat
projection(geos) <- projection(shape)
gadm <- geos %over% shape
geos <- as.data.frame(geos)
geos <- cbind(gadm[ ,c(5,7,9)], geos)
colnames(geos) <- c("region","district","ward","lat","lon","bp","cp","wp","bcount","ccount","cprob","cpred")

# Models ------------------------------------------------------------------
# negative binomial models of GeoSurvey cropland grid counts
summary(m1 <- glm.nb(ccount ~ cprob, geos)) ## scaling model
(est1 <- cbind(Estimate = coef(m1), confint(m1)))

# +additional covariates
summary(m2 <- glm.nb(ccount ~ cprob+bcount+wp, geos))
(est2 <- cbind(Estimate = coef(m2), confint(m2)))
anova(m1, m2) ## model comparison

# Plots -------------------------------------------------------------------
m1pred <- data.frame(cprob = rep(seq(from = 0.0, to = 1.0, length.out = 100)))
m1pred <- cbind(m1pred, predict(m1, m1pred, type = "link", se.fit=TRUE))
m1pred <- within(m1pred, {
  ccount <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

ggplot(newdata2, aes(cprob, ccount)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
  geom_line(aes(colour = prog), size = 2) +
  labs(x = "Math Score", y = "Predicted Days Absent")