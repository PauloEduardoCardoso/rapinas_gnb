#' Create spatial summaries from gridded data
#' Guine-Bissau

#'Install automatically the relevant packages ---------------------------------
kpacks <- c('sp','raster', 'rgdal', 'dplyr', 'ggplot2', 'dismo')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

save.image("D:/Programacao/guinebissau_get_envvars.RData")
load("D:/Programacao/guinebissau_get_envvars.RData")


#' MODIS NDVI MOD44 -----------------------------------------------------------
#' RAW MODIS MOD44 NDVI  Product obtained from
#' GLAM NDVIDB Data Access - Africa (West 1)
#' http://pekko.geog.umd.edu/usda/test/data_new.php?dsRegionId=8
#' (1) ndvi_byte = (ndvi_raw * 200.0) + 50.0
#' (2) values <=50 or >250 indicate bad/missing data
#' NDVI File folder
myfolder16 <- 'S:/Raster/MODIS/2016'
myfolder15 <- 'S:/Raster/MODIS/2015'

#' List Tiff files from folder
tifs <- list.files(path = myfolder15, pattern = '\\.tif$')

#' Stack raster files
stk_raw <- raster::stack(file.path(myfolder15, tifs))

#' Original coord ref of MODIS MOD44 is Sinusoidal
crs(stk_raw[[1]])

#' Administratvive division ---------------------------------------------------
gnb2 <- raster::getData('GADM', country='GNB', level=2)
plot(gnb2)

#' Reproject GADM Admin to UTM28N e Sinusoidal
gnb2sin <- spTransform(gnb2, crs(stk_raw[[1]])) # sinusoidal
gnb2utm <- spTransform(gnb2, CRS('+init=epsg:32628')) # utm28N

#' Crop NDVI raster stack to GNB extent
stk_gnb <- raster::crop(stk_raw, gnb2sin)
plot(stk_gnb[[1]]); plot(gnb2sin, add = T)

#' Replace ndvi_byte: (1) and (2)
stk_gnb[stk_gnb <51] <- NA
stk_gnb[stk_gnb >250] <- NA
stk_gnb1 <- calc(stk_gnb, fun=function(x) {(x-50)/200}) # ndvi_bite (2)

#' NDVI annual Mean value
mean_ndvi <- calc(stk_gnb1, fun = function(x,...) mean(x, na.rm = T))
plot(mean_ndvi); plot(gnb2sin, add = T)

#' get mean NDVI summary by admin area 
mean_ndvi_adm <- raster::extract(stk_gnb1, gnb2sin
                                 ,fun=function(x,...) mean(x, na.rm=T)
                                 ,df = T)
mean_ndvi_adm$adm1 <- gnb2@data$NAME_1 # add column with adm1
mean_ndvi_adm$adm2 <- gnb2@data$NAME_2 # add column with adm2

mean2015 <- mean_ndvi_adm
mean2016 <- mean_ndvi_adm

write.table(mean2015, file='G:/Sig/Bissau/Sumario_Stats_Sector/mean_ndvi2015.txt'
            ,sep = '\t', row.names = F)
write.table(mean_ndvi_adm, file='G:/Sig/Bissau/Sumario_Stats_Sector/16days_meanndvi2015.txt'
            ,sep = '\t', row.names = F)

#' FAO Global Livestok - Cattle density (heads per grid square) ---------------
cattle <- raster('G:/Sig/Bissau/fao_cattle/Glb_Cattle_CC2006_utm28n_GNB.tif')
cattle_adm <- raster::extract(cattle, gnb2utm
                                   ,fun=function(x,...){
                                     sum(x, na.rm=T)}
                                   ,df = T)
cattle_adm$adm1 <- gnb2@data$NAME_1
cattle_adm$adm2 <- gnb2@data$NAME_2
write.table(cattle_adm, file='G:/Sig/Bissau/Sumario_Stats_Sector/cattle.txt'
            ,sep = '\t', row.names = F)

#' WORLD POP Population Density www.afripop.org
#' UNITS: Estimated persons per grid square
pop <- raster('G:/Sig/Bissau/worldpop/GNB10adjv3_utm28n.tif') # utm28n
pop_adm <- raster::extract(pop, gnb2utm
                                   ,fun=function(x,...){
                                     sum(x, na.rm=T)}
                                   ,df = T)
pop_adm$adm1 <- gnb2utm@data$NAME_1
pop_adm$adm2 <- gnb2@data$NAME_2
write.table(pop_adm, file='G:/Sig/Bissau/Sumario_Stats_Sector/populacao.txt'
            ,sep = '\t', row.names = F)

#' Land Cover Data ------------------------------------------------------------
#'# Jaxa Forest Non-Forest
fnf_gnb <- raster('G:/Sig/Bissau/Cartografias Uso Solo/bissau_forest_nforest2015_utm28n.tif')
#'# IICT Land Cover 2010
iict_lcov <- raster ('S:/Bissau/coberto_solo/2010_LANDCOVER_IICT_10CL_GNB.tif')
#'# GlobCover 2009 v2.3
gcov2009 <- raster('G:/Sig/Bissau/Cartografias Uso Solo/GLOBCOVER_V2_3_GNB_utm28n.tif')
#'# ESA Global Cover 2010
esa2010 <- raster('G:/Sig/Bissau/Cartografias Uso Solo/ESAGlobalCover-2010-v1.6.1_utm28n_GNB.tif')


#' Climate data ---------------------------------------------------------------
#BIO1 = Annual Mean Temperature
#BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
#BIO3 = Isothermality (BIO2/BIO7) (* 100)
#BIO4 = Temperature Seasonality (standard deviation *100)
#BIO5 = Max Temperature of Warmest Month
#BIO6 = Min Temperature of Coldest Month
#BIO7 = Temperature Annual Range (BIO5-BIO6)
#BIO8 = Mean Temperature of Wettest Quarter
#BIO9 = Mean Temperature of Driest Quarter
#BIO10 = Mean Temperature of Warmest Quarter
#BIO11 = Mean Temperature of Coldest Quarter
#BIO12 = Annual Precipitation
#BIO13 = Precipitation of Wettest Month
#BIO14 = Precipitation of Driest Month
#BIO15 = Precipitation Seasonality (Coefficient of Variation)
#BIO16 = Precipitation of Wettest Quarter
#BIO17 = Precipitation of Driest Quarter
#BIO18 = Precipitation of Warmest Quarter
#BIO19 = Precipitation of Coldest Quarter

wcl <- raster::getData('worldclim', var = 'bio', res = 0.5
                       , lon = -15, lat = 12) # WorldClim Vars
wclgnb <- raster::crop(wcl, gnb2)
wclgnbutm <- projectRaster(wclgnb, crs = crs(gnb2utm))
plot(wclgnb[[1]])
wcl_adm <- raster::extract(wclgnbutm, gnb2utm
                           ,fun=function(x,...){
                             mean(x, na.rm=T)}
                           ,df = T)
wcl_adm$adm1 <- gnb2utm@data$NAME_1
wcl_adm$adm2 <- gnb2@data$NAME_2
write.table(wcl_adm, file='F:/Sig/Bissau/Sumario_Stats_Sector/worldclim_med.txt'
            ,sep = '\t', row.names = F)

#' Loop over admin @polygons to tabulate areas (pixels) -----------------------
gr <- gnb2utm # admin in utm28n
gr <- gnb2sin # admin in sinusoidal
gr <- gnb2
lgr <- list()
for(i in 1:nrow(gr@data)){
  gri <- gr[i, ]
  gr_i <- raster::extract(wclgnb[[1]], gri, df = F, na.rm=TRUE)
  gr_i <- unlist(gr_i)
  uniqx <- unique(unlist(gr_i))
  #uniqx[is.na(uniqx)] <- -99
  #uniqx <- uniqx[uniqx > 0]
  if(length(uniqx) == 0) uniqx <- 0
  m <- cbind(u=uniqx, n=tabulate(match(gr_i, uniqx)))
  lgr[[i]] <- m
}
#lgr
names(lgr) <- gnb2@data$NAME_2

#' DataFrame LandCover
dfl_Var <- do.call(rbind.data.frame, lgr)
dfl_Var$sector <- rownames(dfl_Var)

dfl_Var$ha <- (dfl_Var$n*90000)/10000 # GLOBCOVER 2009
dfl_Var$ha <- (dfl_Var$n*90000)/10000 # ESA GlobalCover 2010
dfl_Var$ha <- (dfl_Var$n*625)/10000 # IICT 2010
dfl_Var$ha <- (dfl_Var$n*625)/10000 # JAXA Forest-NForest


write.table(dflcov, file='G:/Sig/Bissau/Sumario_Stats_Sector/globcover2009.txt'
            ,sep = '\t', row.names = F)
write.table(dflcov, file='G:/Sig/Bissau/Sumario_Stats_Sector/esaglobalcover2010.txt'
            ,sep = '\t', row.names = F)
write.table(dflcov, file='G:/Sig/Bissau/Sumario_Stats_Sector/jaxafnf2015.txt'
            ,sep = '\t', row.names = F)

extract(r, polys, fun=function(x, ...) length(na.omit(x))/length(x))
