#' https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod09ga
#' Table 1: 1-kilometer State QA Descriptions (16-bit)
#' http://www.khufkens.com/2016/04/20/modis-hdf-data-extraction-in-r/
#' https://pvanb.wordpress.com/2012/11/07/opening-modis-tiles-in-qgis/
#' https://explorer.earthengine.google.com/#search/tag:modis
#' 
kpacks <- c('raster', 'rgdal', 'gdalUtils','tidyverse',
            'lubridate', 'stringr',
            'ggplot2')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#' ------------------------------ ADMIN DATA -------------------------------
aoi <- readRDS(file.path(getwd(),'rdata', 'GNB_adm2.rds'))

aoiutm38n <- spTransform(aoi, crs_utm38n)
plot(aoiutm38n)
mybbox <- extent(aoiutm38n)
#' -------------------------------------------------------------------------

#' ------------------------------- CRS -------------------------------------
crs_wgs84 <- CRS('+init=EPSG:4326')
crs_utm38n <- CRS('+init=EPSG:32628')
crs_sinu <- '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'
#' ---------------------------------------------------------------------------

#' --------------------------- GDAL Installation ------------------------------
gdal_setInstallation(search_path = 'C:/OSGeo4W64/bin' # set my path to gdal
                     , verbose=TRUE)
getOption("gdalUtils_gdalPath")[[1]]$version # check version
#' ----------------------------------------------------------------------------

#' ----------------------------- Burned Area: MOD64A1 -------------------------
#' Get a list of hdf names
path2mod64a1 <- 'G:/Sig/raster/MODIS/MCD64A1' # change here to local path
path2mod64a1tif <- 'G:/Sig/raster/MODIS/MCD64A1/tif'
lhdf <- list.files(file.path(path2mod64a1), pattern = '.hdf$' # $:ending with .hdf
                   , full.names = F)

lburn <- list()
for(i in 1:length(lhdf)){
  iname <- paste0(substr(lhdf[i], 10, 16)) # name for tif file
  idata <- as.Date(iname, format=c("%Y%j"))
  # ilua <- lunar::lunar.illumination(idata, shift = 0) # Lunar illumination
  #gdalUtils::gdalinfo(file.path(pathtomodis,lhdf[i])) # hdf info
  #sdset <- gdalUtils::get_subdatasets(file.path('C:/sig/MODIS',lhdf[1])
  #                                    , verbose = T)
  #
  gdalUtils::gdal_translate(src_dataset = file.path(path2mod64a1, lhdf[i])
                            , ot = 'Int16'
                            , dst_dataset = file.path(path2mod64a1tif
                                                      , paste0(iname, 'burnedarea.tif'))
                            , sd_index = 1 # ssd state_1km_1
                            , projwin_srs = crs_sinu
                            #, a_srs = 'EPSG:4326'
  )
}

#' Stack individual tif files
burn <- stack(list.files(file.path(path2mod64a1tif), pattern = 'tif$'
                         , full.names = T))

#' Reclass rasterstack 
mt <- c(-Inf, 0, 0,  0, Inf, 1)
rclmat <- matrix(mt, ncol=3, byrow=TRUE)
rc <- reclassify(burn, rclmat)

#' Get GNB Burned Area(for 2016)
rcsum <- raster::calc(rc, fun = sum)
rcsum <- projectRaster(rcsum, crs = crs_utm38n, res = 500, method="ngb",
                       filename = file.path(path2mod64a1tif, paste0('tburnedarea.tif')),
                       overwrite = TRUE)
plot(rcsum)

#' Burned area by Admin sector (level 2)
gnb_burn <- raster::extract(rcsum, aoiutm38n
                            , fun = function(x,...) sum(x[x>0], na.rm = T)
                            , df = TRUE)
gnb_burn <- gnb_burn %>%
  mutate(admin2 = aoiutm38n@data$NAME_2
         , adm_sqkm = rgeos::gArea(aoiutm38n, byid=T)/1000000
         , burn_sqkm = (tburnedarea * 0.25)
         , frac = burn_sqkm/adm_sqkm)
#' ----------------------------------------------------------------------------

#' ----------------------------- MODIS Vegetation -----------------------------
#' Get a list of hdf names
path2mod13q1 <- 'G:/Sig/raster/MODIS/MYD13Q1' # change here to local path
path2mod13q1tif <- 'G:/Sig/raster/MODIS/MYD13Q1/tif'
lhdf <- list.files(file.path(path2mod13q1), pattern = '.hdf$' # $:ending with .hdf
                   , full.names = F)

lndvi <- list('name', 'date')
for(i in 1:length(lhdf)){
  gdalUtils::gdal_translate(src_dataset = file.path(path2mod13q1, lhdf[i])
                            , ot = 'Int16'
                            , dst_dataset = file.path(path2mod13q1tif # output
                                                      , paste0(iname, '_vegind.tif'))
                            , sd_index = 1 # 250m 16 days NDVI
                            , projwin_srs = crs_sinu
                            #, a_srs = 'EPSG:4326'
  )
}
unlist(lapply(lhdf, function(x) paste0(substr(x, 10, 16))))
                         
lapply(lhdf, function(x){as.Date(paste0(substr(x, 10, 16)), format=c("%Y%j"))}, USE.NAMES = F)
                       

#' Stack individual tif files
ndvi <- stack(list.files(file.path(path2mod13q1tif), pattern = 'tif$'
                         , full.names = T))
ndvi <- ndvi * 0.0001 # MODIS Scaling factor
#' Reclass rasterstack 
mt <- c(-Inf, 0, 0,  0, Inf, 1)
rclmat <- matrix(mt, ncol=3, byrow=TRUE)
rc <- reclassify(burn, rclmat)

#' Get GNB Burned Area(for 2016)
rcsum <- raster::calc(rc, fun = sum)
rcsum <- projectRaster(rcsum, crs = crs_utm38n, res = 500, method="ngb",
                       filename = file.path(path2mod64a1tif, paste0('tburnedarea.tif')),
                       overwrite = TRUE)
plot(rcsum)

#' Burned area by Admin sector (level 2)
gnb_burn <- raster::extract(rcsum, aoiutm38n
                            , fun = function(x,...) sum(x[x>0], na.rm = T)
                            , df = TRUE)
gnb_burn <- gnb_burn %>%
  mutate(admin2 = aoiutm38n@data$NAME_2
         , adm_sqkm = rgeos::gArea(aoiutm38n, byid=T)/1000000
         , burn_sqkm = (tburnedarea * 0.25)
         , frac = burn_sqkm/adm_sqkm)
#' ----------------------------------------------------------------------------

#' ------------------------- HUMAN FOOTPRINT 2009 -----------------------------
hfp09 <- raster(file.path(getwd(), 'rdata', 'HFP2009_utm28n.tif'))
gnb_hfp09 <- raster::extract(hfp09, aoiutm38n
                             , fun = function(x,...) mean(x, na.rm = T)
                             , df = TRUE)
gnb_hfp09$median <- raster::extract(hfp09, aoiutm38n
                                    , fun = function(x,...) median(x, na.rm = T)
                                    , df = F)
gnb_hfp09$variety <- raster::extract(hfp09, aoiutm38n
                                     , fun = function(x,...) length(unique(x, na.rm = T))
                                     , df = F)
gnb_hfp09$ranges <- raster::extract(hfp09, aoiutm38n
                                    , fun = function(x,...) range(x, na.rm = T)
                                    , df = F)
gnb_hfp09$diff <- raster::extract(hfp09, aoiutm38n
                                  , fun = function(x,...) diff(range(x, na.rm = T))
                                  , df = F)
#' ----------------------------------------------------------------------------

