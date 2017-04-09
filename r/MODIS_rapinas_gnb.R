#' https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod09ga
#' Table 1: 1-kilometer State QA Descriptions (16-bit)
#' http://www.khufkens.com/2016/04/20/modis-hdf-data-extraction-in-r/
#' https://pvanb.wordpress.com/2012/11/07/opening-modis-tiles-in-qgis/
#' https://explorer.earthengine.google.com/#search/tag:modis
#' 
kpacks <- c('raster', 'rgdal', 'gdalUtils','dplyr',
            'tidyr' ,'lubridate', 'stringr', 'lunar',
            'ggplot2')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

gdal_setInstallation(search_path = 'C:/OSGeo4W64/bin' # set my path to gdal
                     , verbose=TRUE)
getOption("gdalUtils_gdalPath")[[1]]$version # check version

pathtomodis <- 'C:/sig/MODIS/in' # change here to local path
pathtotif <- 'C:/sig/MODIS/out'
#' Get a list of sds names
#' MOD09GA.A2013211.h16v05.006.2015264041445.hdf 10:16
lhdf <- list.files(file.path(pathtomodis), pattern = '.hdf$' # $:ending with .hdf
                   , full.names = F)

aoi <- readOGR(dsn = 'G:/Sig/vetor/administrativo/portugal', layer= 'ArqMadeira_AAd_CAOP2016')
#aoi <- aoi[aoi$Concelho == 'Santa Cruz', ]
aoisin <- spTransform(aoi[aoi$Concelho == 'Santa Cruz', ]
                      , CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'))
plot(aoisin)
mybbox <- extent(aoisin)
#' Function to extract bite data
fun_bite10 <- function(x){
  a <- intToBits(x)[11:11] # bite 10
  ## bite 0 e 1: internal cloud algorithm flag=1:cloud
  ## 00	clear
  ## 01	cloudy
  ## 10	mixed
  ## 11	not set, assumed clear
  ## bite 10 Internal alg cloud flag
  ## 0 nocloud
  ## 1 cloud
  flag <- as.numeric(paste(as.numeric(a), collapse = "")) # numeric bite 10
  flag
}

#' Function to get % of cloud covering the aoi
f_mod <- function(x, ...) {
  uniqx <- unique(icloud)
  #uniqx <- uniqx[!is.na(uniqx)]
  m <- uniqx[which.max(tabulate(match(icloud, uniqx)))]
  return(m)
}

#' ----------------------------------------------------------------------------
lcloud <- list()
for(i in 1:length(lhdf)){
  iname <- paste0(substr(lhdf[i], 10, 16)) # name for tif file
  idata <- as.Date(iname, format=c("%Y%j"))
  ilua <- lunar::lunar.illumination(idata, shift = 0) # Lunar illumination
  #gdalUtils::gdalinfo(file.path(pathtomodis,lhdf[i])) # hdf info
  #sdset <- gdalUtils::get_subdatasets(file.path('C:/sig/MODIS',lhdf[1])
  #                                    , verbose = T)
  #
  gdalUtils::gdal_translate(src_dataset = file.path(pathtomodis, lhdf[i])
                            , dst_dataset = file.path(pathtotif, paste0(iname, 'state1.tif'))
                            , sd_index = 2 # ssd state_1km_1
                            , projwin_srs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'
                            #, a_srs = 'EPSG:4326'
  )
  #' Load the Geotiff created into R
  ir <- raster(file.path(pathtotif, paste0(iname, 'state1.tif'))) # read tif as raster obj
  ir <- raster::crop(ir, mybbox)
  irb <- raster::calc(ir, fun=fun_bite10)
  plot(irb);plot(aoisin, add = T)
  icloud <- raster::extract(irb, mybbox)
  lcloud[[i]] <- data.frame(date = idata
                            , cloudp = unname(table(icloud)[2]/ncell(irb))
                            , moon = ilua)
  writeRaster(irb, filename = file.path(pathtotif, paste0(iname, 'cloudbit10.tif')), overwrite=TRUE)
}

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

