#### AGE MAP ####

# LOAD PACKAGES 
library(tidyr)
library(stringr)
library(raster)
library(readr)
library(parallel)
library(gdalUtils)
library(rgdal)

# FUNCTIONS
fncs <- list.files('./fncs/')
lapply(fncs, source)

# READ GEBCO  
# 2019
p_r <- '/Users/jedgroev/Downloads/1_IBED/P_TCE_PIAC_ISLAND_GEOGRAPHY/datasets/original/GEBCO_2019.nc'
r <- raster::raster(p_r, var='elevation') # THIS NEEDS TO BE CORRECTED - ONLY VALID FOR GEBCO! NOT FOR OTHER NC FILES
# 2021
p_r <- '/Users/jedgroev/Downloads/2_GITHUB/piac/data_raw/gebco_2021_sub_ice_topo/GEBCO_2021_sub_ice_topo.nc'
r <- terra::rast(p_r) %>% raster()
proj4string(r) <- "+proj=longlat +datum=WGS84"

# LIST SEACURVE PATHS 
looper <- seq(0,90,10)
looper <- unique(c(looper,-looper)) 

for(i in 1:length(looper)){
  # set path to sea level curve tiles 
  p <- '/Users/jedgroev/ownCloud/Compsup (Projectfolder)/PIAC/koene/MAPS/'
  p_k <- list.files(p,recursive=TRUE, pattern='.ASC') # list tiles 
  p_k_looper <- p_k[grep(pattern = paste0('^',looper[i],'.*'), p_k)] # subset latitude
  p_k <- gsub(paste0('^',looper[i],'/'),paste0('+',looper[i],'/'), p_k_looper) # replace when + is missing
   
  # p_m30 <- p_k[grep(pattern = '^90.*', p_k)]
  # p_p30 <- p_k[grep(pattern = '^-90.*', p_k)]
  # p_k <- c(p_m30,p_p30)
  # p_k <- gsub('^90/','+90/', p_k)
  p_k_l <- split(p_k,f=substr(p_k,1,11)) # split by longitude
  seacurve_path_l <- lapply(p_k_l, function(x) paste0(p,gsub('\\+','',x))) # list path names 
  
  # SET PATH TO SEA CURVE DIRECTORY 
  setwd('/Users/jedgroev/TEMP/AGE/AGE/')
  # CREATE DROWNING AGE RASTERS FOR A CERTAIN LATITUDE 
  parallel::mclapply(seacurve_path_l, function(x) agemap(r=r, seacurve=x, res_fact = 8),mc.cores = 4)
  # in case of missing tiles
  seacurve_all <- gsub('/','-',names(seacurve_path_l))
  seacurve_done <- gsub('.asc','',gsub('AGE','',list.files(pattern='.asc$',recursive = T)))
  seacurve_path_missing_l <- seacurve_path_l[c(!seacurve_all %in% seacurve_done)]
  parallel::mclapply(seacurve_path_missing_l, function(x) agemap(r=r, seacurve=x, res_fact = 8),mc.cores = 4)
  print(i)
}

# merge and save as tif
p_age <- list.files(pattern='.asc$')

# make an empty raster with the wished extent
e_gebco <- extent(r)
template <- raster(e_gebco)
projection(template) <- crs(r)
writeRaster(template, file="AGE.tif", format="GTiff")

# mosaic the raster
mosaic_rasters(gdalfile=p_age,dst_dataset="AGE.tif",of="GTiff")
gdalinfo("AGE.tif")

#######################

# convert to isochrones
library(terra)
library(sf)
library(smoothr)
library(dplyr)
library(raster)

# read 
r <- rast('~/CDI.tif')

# isochrone polygons smoothing
p <- as.polygons(r)
cdi_p <- sf::st_as_sf(p) %>% smoothr::smooth(method = "ksmooth", smoothness = 1) # slow!

# isochrone lines smoothing
l <- as.lines(p)
cdi_l <- sf::st_as_sf(l) %>% smoothr::smooth(method = "ksmooth", smoothness = 1) # fast!






# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

#### SHALLOW WATER MAP ####

# new directory to store shallow water rasters 
copy.dir.tree <- function (from, to){
  # to = full path to copy the structure to, it will create the directory as well
  # from = full path to copy the structure from 
  directory <- to
  dir.create(directory)
  p <- from
  dir_l <- list.dirs(p,recursive=TRUE, full.names=FALSE) # list tiles 
  lapply(dir_l[2:length(dir_l)], function(x) dir.create(paste0(directory,x)))
}

scialla <- function(r,seacurve, res_fact = 8, shallow=40){
  # r = bathymetric model 
  # seacurve = path to a single tile; seacurve = seacurve_path_l
  #tile <- gsub('/','-',gsub('^/','+',str_sub(seacurve[1],-23,-13)))
  seacurvestack <- stack(seacurve)
  crs(seacurvestack) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  e <- extent(seacurvestack)
  elev <-raster::crop(r, e)
  
  # depending from the wished resolution we will need to aggregate 
  elev <- raster::aggregate(elev, fact=res_fact)
  #elev_test <- aggregate(elev, fact=8, fun=fasterAgg.Fun)
  #elev_res <-aggregate(elev,fact=8,fun=aggrRcppW)
  
  # RESAMPLE CURVE 
  seacurvestack <- resample(seacurvestack, elev, method="bilinear")
  print('seacurve stack resampled')
  period <- data.frame(index=1:dim(seacurvestack)[3],period=readr::parse_number(names(seacurvestack)) * 1000)
  
  shallow_water_only <- elev > (seacurvestack - shallow) & elev < (seacurvestack)
  above_shallow_water <- elev > (seacurvestack - shallow)
  scialla <- shallow_water_only + above_shallow_water
  NAvalue(scialla) <- 0 
  scialla <- scialla - 1
  
  # THE DIRECTORY NAME OF THE NEW DATASET 
  DIR <- paste0('SLW', shallow,'/')
  # THE PATH NAMES 
  paths <- gsub('RSL','SLW',gsub('koene/MAPS/',DIR,seacurve))
  
  # IF DIR DOES NOT EXIST, GENERATE THE FOLDER STRUCTURE  
  from_path <- paste0(dirname(dirname(dirname(seacurve))),'/')[1]
  to_path <- paste0(dirname(dirname(dirname(dirname(dirname(seacurve))))),'/',DIR)[1] 
  if(dir.exists(dirname(paths[1])) == FALSE){
    copy.dir.tree(from = from_path, to = to_path)
  }
  # WRITE RASTERS 
  writeRaster(scialla,filename=paths,bylayer=TRUE, format='ascii')
  return(list(from_path, to_path))
}


# COPY DIRECTORY TREE 
from_path <- '/Users/jedgroev/ownCloud/Compsup (Projectfolder)/PIAC/koene/MAPS/'
to_path <- '/Users/jedgroev/TEMP/SLW/'
copy.dir.tree(from = from_path, to = to_path)

# READ GEBCO  
p_r <- '/Users/jedgroev/Downloads/1_IBED/P_TCE_PIAC_ISLAND_GEOGRAPHY/datasets/original/GEBCO_2019.nc'
r <- raster::raster(p_r, var='elevation') # THIS NEEDS TO BE CORRECTED - ONLY VALID FOR GEBCO! NOT FOR OTHER NC FILES
# 2021
p_r <- '/Users/jedgroev/Downloads/2_GITHUB/piac/data_raw/gebco_2021_sub_ice_topo/GEBCO_2021_sub_ice_topo.nc'
p_r <- '/Volumes/Extreme SSD/TEMP/gebco_2021/GEBCO_2021.nc'

r <- terra::rast(p_r) %>% raster()
proj4string(r) <- "+proj=longlat +datum=WGS84"

# LIST SEACURVE PATHS 
looper <- seq(0,90,10)
looper <- unique(c(looper,-looper)) 

for(i in 1:length(looper)){
  start <- Sys.time()
  # set path to sea level curve tiles 
  p <- from_path
  p_k <- list.files(p,recursive=TRUE, pattern='.ASC') # list tiles 
  p_from <- p_k
  p_k_looper <- p_k[grep(pattern = paste0('^',looper[i],'.*'), p_k)] # subset latitude
  p_k <- gsub(paste0('^',looper[i],'/'),paste0('+',looper[i],'/'), p_k_looper) # replace when + is missing
  
  # p_m30 <- p_k[grep(pattern = '^90.*', p_k)]
  # p_p30 <- p_k[grep(pattern = '^-90.*', p_k)]
  # p_k <- c(p_m30,p_p30)
  # p_k <- gsub('^90/','+90/', p_k)
  p_k_l <- split(p_k,f=substr(p_k,1,11)) # split by longitude
  seacurve_path_l <- lapply(p_k_l, function(x) paste0(p,gsub('\\+','',x))) # list path names 
  #shallow_path_l <- lapply(p_k_l, function(x) paste0(t,gsub('\\+','',x))) # list path names 
  setwd(to_path)
  # CREATE SHALLOW WATER RASTER FOR A CERTAIN LATITUDE 
  parallel::mclapply(seacurve_path_l, function(x) scialla(r=r, seacurve=x, res_fact = 8, shallow=40),mc.cores = 4)
  print(paste0(looper[i], ' exported'))
  end <- Sys.time()
  print(end - start)
}

# in case of missing tiles

seacurve_all <- gsub('/','-',names(seacurve_path_l))
p_to <- list.files(to_path,recursive=TRUE, pattern='.asc') # list tiles 
p_to <- gsub('asc','ASC',gsub('SLW','RSL',p_to))
seacurve_path_missing <- p_from[c(!p_from %in% p_to)]
seacurve_path_missing_l <- split(seacurve_path_missing,f=substr(seacurve_path_missing,1,11)) # split by longitude
seacurve_path_missing_l <- lapply(seacurve_path_missing_l, function(x) paste0(p,gsub('\\+','',x))) # list path names 
parallel::mclapply(seacurve_path_missing_l, function(x) scialla(r=r, seacurve=x, res_fact = 8, shallow=40),mc.cores = 4)


# MOSAIC RASTERS 
setwd('/Users/jedgroev/ownCloud/Compsup (Projectfolder)/PIAC/JO-KO/SLW/')
substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}

p_SLW <- list.files(pattern='.asc$', recursive = TRUE)
p_SLW <- gsub('^-','\\-',p_SLW) 
p_SLW <- split(p_SLW, f=substrRight(p_SLW,7))

for(i in 1:length(p_SLW)){
  # make an empty raster with the wished extent
  e_gebco <- extent(r)
  template <- raster(e_gebco)
  projection(template) <- crs(r)
  
  SLW <- p_SLW[[i]]
  writeRaster(template, file=paste0("/Volumes/Extreme SSD/SHALLOW/SLW",gsub('.asc','',names(p_SLW))[i],".tif"), format="GTiff")
  # mosaic the raster
  mosaic_rasters(gdalfile=SLW,dst_dataset=paste0("/Volumes/Extreme SSD/SHALLOW/SLW",gsub('.asc','',names(p_SLW))[i],".tif"),of="GTiff")
  gdalinfo(paste0("/Volumes/Extreme SSD/SHALLOW/SLW",gsub('.asc','',names(p_SLW))[i],".tif"))
}


# WRITE ALL TILES OF ONE PERIOD IN ONE FOLDER FOR 125. 
SLW <- p_SLW[[]] # SELECT THE RIGHT LIST ELEMENT 
r_read<- lapply(SLW, function(x) raster(x))
for(i in 1:length(r_read)){
writeRaster(r_read[[i]],filename=paste("~/SLW125/",gsub(" ","",gsub('/','_',SLW[i]))),format='ascii')
}

# USE GDAL MERGE IN TERMINAL TO MOSAIC AND USE INIT nan AS INITIATION VALUE OF THE RASTER
gdal_merge.py -init nan -o SLW125.tif *.asc # nan = two tiles give issues and are all nan values. Therefore we set init value to nan. 

# READ THE SLW125.tif AND WRITE TO THE SHARED FOLDER 
t <- raster('~/SLW125/SLW125.tif')
writeRaster(t, '/Users/jedgroev/ownCloud/Compsup (Projectfolder)/PIAC/JO-KO/SLW/SLW_global/SLW125.tif')

# READ AND WRITE ALL OTHER GLOBAL RASTERS TO REDUCE THE SIZE 
setwd("/Users/jedgroev/ownCloud/Compsup (Projectfolder)/PIAC/JO-KO/SLW/SLW_global/")
f_l <- list.files(pattern='tif')
for(i in 1:length(r_l)){
  r_l <- raster(f_l[i])
  writeRaster(r_l, filename=paste0('../SLW_globalR/',f_l[i]))
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

#### CUMULATIVE SHELF INCREASE ####
# COPY DIRECTORY TREE 
shallow <- 140 
from_path <- '/Users/jedgroev/ownCloud/Compsup (Projectfolder)/PIAC/koene/MAPS/'
to_path <- paste0('/Volumes/Extreme SSD/TEMP/')
#copy.dir.tree(from = from_path, to = to_path)

# READ GEBCO  
p_r <- '/Users/jedgroev/OneDrive - UvA/1_IBED/P_TCE_PIAC_ISLAND_GEOGRAPHY/data/original/GEBCO_2019.nc'
r <- raster::raster(p_r, var='elevation') # THIS NEEDS TO BE CORRECTED - ONLY VALID FOR GEBCO! NOT FOR OTHER NC FILES
# 2021
#p_r <- '/Users/jedgroev/Downloads/2_GITHUB/piac/data_raw/gebco_2021_sub_ice_topo/GEBCO_2021_sub_ice_topo.nc'
p_r <- '/Volumes/Extreme SSD/TEMP/gebco_2021/GEBCO_2021.nc'
r <- terra::rast(p_r) %>% raster()
proj4string(r) <- "+proj=longlat +datum=WGS84"

# LIST SEACURVE PATHS 
looper <- seq(0,90,10)
looper <- unique(c(looper,-looper)) 

for(i in 1:length(looper)){
  start <- Sys.time()
  # set path to sea level curve tiles 
  p <- from_path
  p_k <- list.files(p,recursive=TRUE, pattern='.ASC') # list tiles 
  p_from <- p_k
  p_k_looper <- p_k[grep(pattern = paste0('^',looper[i],'.*'), p_k)] # subset latitude
  p_k <- gsub(paste0('^',looper[i],'/'),paste0('+',looper[i],'/'), p_k_looper) # replace when + is missing
  
  # p_m30 <- p_k[grep(pattern = '^90.*', p_k)]
  # p_p30 <- p_k[grep(pattern = '^-90.*', p_k)]
  # p_k <- c(p_m30,p_p30)
  # p_k <- gsub('^90/','+90/', p_k)
  p_k_l <- split(p_k,f=substr(p_k,1,11)) # split by longitude
  seacurve_path_l <- lapply(p_k_l, function(x) paste0(p,gsub('\\+','',x))) # list path names 
  #shallow_path_l <- lapply(p_k_l, function(x) paste0(t,gsub('\\+','',x))) # list path names 
  #setwd(to_path)
  # CREATE SHALLOW WATER RASTER FOR A CERTAIN LATITUDE 
  parallel::mclapply(seacurve_path_l, function(x) scialla(r=r, seacurve=x, res_fact = 8, shallow=shallow, to_path=to_path),mc.cores = 4)
  print(paste0(looper[i], ' exported'))
  end <- Sys.time()
  print(end - start)
}

# in case of missing tiles
seacurve_all <- gsub('/','-',names(seacurve_path_l))
p_to <- list.files(paste0(to_path,'SLW',shallow,'/'),recursive=TRUE, pattern='.asc') # list tiles 
p_to <- gsub('asc','ASC',gsub('SLW','RSL',p_to))
seacurve_path_missing <- p_from[c(!p_from %in% p_to)]
seacurve_path_missing_l <- split(seacurve_path_missing,f=substr(seacurve_path_missing,1,11)) # split by longitude
seacurve_path_missing_l <- lapply(seacurve_path_missing_l, function(x) paste0(p,gsub('\\+','',x))) # list path names 
parallel::mclapply(seacurve_path_missing_l, function(x) scialla(r=r, seacurve=x, res_fact = 8, shallow=shallow),mc.cores = 4)



# MOSAIC RASTERS 
SLW_path <- paste0(to_path,'SLW',shallow,'/')
setwd(SLW_path)
substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}

p_SLW <- list.files(pattern='.asc$', recursive = TRUE)
p_SLW <- gsub('^-','\\-',p_SLW) 
p_SLW <- split(p_SLW, f=substrRight(p_SLW,7))

      
for(i in 1:length(p_SLW)){ #1:length(p_SLW)
  # make an empty raster with the wished extent
  e_gebco <- extent(r)
  template <- raster(e_gebco)
  projection(template) <- crs(r)
  
  SLW <- p_SLW[[i]]
  writeRaster(template, file=paste0("SLW",gsub('.asc','',names(p_SLW))[i],".tif"), format="GTiff",overwrite=TRUE)
  # mosaic the raster
  mosaic_rasters(gdalfile=SLW,dst_dataset=paste0("SLW",gsub('.asc','',names(p_SLW))[i],".tif"),of="GTiff")
  gdalinfo(paste0("SLW",gsub('.asc','',names(p_SLW))[i],".tif"))
}

# READ AND WRITE ALL OTHER GLOBAL RASTERS TO REDUCE THE SIZE 
setwd(SLW_path)
dir.create('SLW_global')
f_l <- list.files(pattern='tif')
for(i in 1:length(f_l)){
  r_l <- raster(f_l[i])
  writeRaster(r_l, filename=paste0('./SLW_global/',f_l[i]),overwrite=T)
}
#lapply(f_l, function(x) file.remove(x))

# setwd(to_path)
# 
# for(i in 1:length(p_SLW)){
#   tiles <- p_SLW[[i]]
#   for(j in 1:length(tiles)){
#     r_l <- raster(tiles[j])
#     crs(r_l) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#     writeRaster(r_l, filename=paste0(tiles[j]),overwrite=TRUE)    
#   }
# }
