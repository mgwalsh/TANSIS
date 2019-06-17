# Tanzania "Crop scout" data setup based on MobileSurvey ground observations
# M. Walsh, December 2018

# Required packages
# install.packages(c("devtools","plyr","rgdal","raster","leaflet","htmlwidgets","wordcloud")), dependencies=TRUE)
suppressPackageStartupMessages({
  require(downloader)
  require(plyr)
  require(rgdal)
  require(raster)
  require(leaflet)
  require(htmlwidgets)
  require(wordcloud)
})

# Data downloads -----------------------------------------------------------
# set working directory
dir.create("TZ_mobs", showWarnings = F)
setwd("./TZ_mobs")

# download MobileSurvey data
download("https://www.dropbox.com/s/2dbt0kefgjj5xhq/TZ_crop_scout_2019.csv.zip?raw=1", "TZ_crop_scout_2019.csv.zip", mode = "wb")
unzip("TZ_crop_scout_2019.csv.zip", overwrite = T)
mobs <- read.table("TZ_crop_scout_2019.csv", header = T, sep = ",")

# download GADM-L3 shapefile (courtesy: http://www.gadm.org)
download("https://www.dropbox.com/s/bhefsc8u120uqwp/TZA_adm3.zip?raw=1", "TZA_adm3.zip", mode = "wb")
unzip("TZA_adm3.zip", overwrite = T)
shape <- shapefile("TZA_adm3.shp")

# download raster stack (note this is a big 1+ Gb download)
download("https://www.dropbox.com/s/ejl3h62hojnhh3a/TZ_250m_2019.zip?raw=1", "TZ_250m_2019.zip", mode = "wb")
unzip("TZ_250m_2019.zip", overwrite = T)
glist <- list.files(pattern="tif", full.names = T)
grids <- stack(glist)

# Data setup --------------------------------------------------------------
# attach GADM-L3 admin unit names from shape
coordinates(mobs) <- ~lon+lat
projection(mobs) <- projection(shape)
gadm <- mobs %over% shape
mobs <- as.data.frame(mobs)
mobs <- cbind(gadm[ ,c(5,7,9)], mobs)
mobs <- rename(mobs, c("NAME_1"="region", "NAME_2"="district", "NAME_3"="ward"))

# project GeoSurvey coords to grid CRS
mobs.proj <- as.data.frame(project(cbind(mobs$lon, mobs$lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(mobs.proj) <- c("x","y")
mobs <- cbind(mobs, mobs.proj)
coordinates(mobs) <- ~x+y
projection(mobs) <- projection(grids)

# extract gridded variables at GeoSurvey locations
mobsgrid <- extract(grids, mobs)
msdat <- as.data.frame(cbind(mobs, mobsgrid)) 
# msdat <- msdat[!duplicated(msdat), ] ## removes any duplicates ... if needed
msdat <- msdat[complete.cases(msdat[ ,c(39:90)]),] ## removes incomplete cases
msdat <- msdat[ which(msdat$CP=='Y'), ] ## selects croplands only

# Write data frame --------------------------------------------------------
dir.create("Results", showWarnings = F)
write.csv(msdat, "./Results/TZ_crop_scout_2019.csv", row.names = F)

# GeoSurvey map widget ----------------------------------------------------
w <- leaflet() %>%
  setView(lng = mean(msdat$lon), lat = mean(msdat$lat), zoom = 6) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(msdat$lon, msdat$lat, clusterOptions = markerClusterOptions())
w ## plot widget 
saveWidget(w, 'TZ_crop_scout_2019.html', selfcontained = T) ## save widget

# MobileSurvey contributions ----------------------------------------------
mscon <- as.data.frame(table(msdat$observer))
set.seed(1235813)
wordcloud(mscon$Var1, freq = mscon$Freq, scale = c(2,0.1), random.order = T)

