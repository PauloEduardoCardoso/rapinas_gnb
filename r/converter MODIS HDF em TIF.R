#' Read MODIS hdf files (subfiles)
kpacks <- c("gdalUtils", 'rgdal', 'rgeos', 'raster')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#'read subsets
sdats <- get_subdatasets('C:/sig/MOD13Q1.A2016081.h16v07.006.2016111112951.hdf')
sdats
#' Isolate the name of the first sds
ndvi1 <- sdats[1]
filename <- 'C:/sig/ndvi.tif'
gdal_translate(ndvi1, dst_dataset = filename)
#' Load the Geotiff created into R
r <- raster(filename)
r1 <- r*0.00000001
r1
#plot(r1)
writeRaster(r1, 'C:/sig/ndvi01.tif')


## a rever
filename <- substr(files,11,14)
filename <- paste0("NPP", filename, ".tif")
filename

for (i in 1:15){
  sds <- get_subdatasets(files[i])
  gdal_translate(sds[1], dst_dataset = filename[i])
}