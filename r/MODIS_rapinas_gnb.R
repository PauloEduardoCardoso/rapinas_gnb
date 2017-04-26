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
#' -------------------------------------------------------------------------

#' --------------------------- GDAL Installation ---------------------------
gdal_setInstallation(search_path = 'C:/OSGeo4W64/bin' # set my path to gdal
                     , verbose=TRUE)
getOption("gdalUtils_gdalPath")[[1]]$version # check version
#' -------------------------------------------------------------------------

#' ------------------------------Burnt Area: MOD64A1 -----------------------
#' Get a list of hdf names
path2mod64a1 <- 'G:/Sig/raster/MODIS/MCD64A1' # change here to local path
path2mod64a1tif <- 'G:/Sig/raster/MODIS/MCD64A1/tif'
lhdf <- list.files(file.path(path2mod64a1), pattern = '.hdf$' # $:ending with .hdf
                   , full.names = F)

#' ----------------------------------------------------------------------------
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
                            , dst_dataset = file.path(path2mod64a1tif, paste0(iname, 'burnedarea.tif'))
                            , sd_index = 1 # ssd state_1km_1
                            , projwin_srs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'
                            #, a_srs = 'EPSG:4326'
  )
  #' Load the Geotiff created into R
  #ir <- raster(file.path(path2mod64a1, paste0(iname, 'burnedarea.tif'))) # read tif as raster obj
  #ir <- raster::crop(ir, mybbox)
  #irb <- raster::calc(ir, fun=fun_bite10)
  #plot(irb);plot(aoisin, add = T)
  #icloud <- raster::extract(irb, mybbox)
  #lcloud[[i]] <- data.frame(date = idata
  #                          , cloudp = unname(table(icloud)[2]/ncell(irb))
  #                          , moon = ilua)
  #writeRaster(irb, filename = file.path(pathtotif, paste0(iname, 'cloudbit10.tif')), overwrite=TRUE)
}

burn <- stack(list.files(file.path(path2mod64a1tif), pattern = 'tif$'
                         , full.names = T))

#' Reclass rasterstack 
mt <- c(-Inf, 0, 0,  0, 1, 0,  1, Inf, 1)
rclmat <- matrix(mt, ncol=3, byrow=TRUE)
rc <- reclassify(burn, rclmat)

#' GNB Burned Area(2016)
rcsum <- raster::calc(rc, fun=sum, filename=file.path(path2mod64a1tif, paste0('tburnedarea.tif')))
rcsum <- projectRaster(rcsum, crs = )
plot(rcsum)

 
# Data handle and plot
lcloud %>%
  bind_rows() %>%
  gather(var, value, 2:3, factor_key=TRUE) %>%
  ggplot(aes(x=date, y=value,color = var)) + 
  geom_path() + 
  geom_point() +
  facet_grid(var~.)

lcloud %>%
  bind_rows()

