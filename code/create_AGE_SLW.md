### 1. SETTINGS

``` r
to_path <- '/Volumes/Extreme SSD/TEMP/' # specify the root path where the datasets (AGE/SLW) will be generated (example external drive)
```

### 2. PACKAGES

``` r
library(readr);library(parallel)
library(tidyr);library(stringr);library(dplyr)
library(raster);library(gdalUtils);library(rgdal);library(terra);library(sf);library(smoothr)
```

#### 3. FUNCTIONS

``` r
fncs <- list.files('./fncs',full.names = T)
lapply(fncs,source)
```

#### 4. DOWNLOAD DATA

Download link of [GEBCO
2021](https://www.bodc.ac.uk/data/open_download/gebco/gebco_2021/zip/)
and the [Spatio-temporal Sea Level
Curve](https://uvaauas.figshare.com/account/articles/20029991). To
obtain access to the curve please contact Johannes De Groeve.

#### 5. IMPORT DATA

``` r
gebco_path <- '~/Downloads/GEBCO_2021.nc' # GEBCO path - modify if path is different
curve_path <- '~/Downloads/RSL_tiles/' # SEACURVE path - modify if path is different

# 2021
p_r <- gebco_path  # '/Volumes/Extreme SSD/TEMP/gebco_2021/GEBCO_2021.nc'
r <- terra::rast(p_r) %>% raster()
proj4string(r) <- "+proj=longlat +datum=WGS84"
```

#### 6. CREATE AGE MAP

``` r
# LIST SEACURVE PATHS 
looper <- seq(0,90,10)
looper <- unique(c(looper,-looper)) 

for(i in 1:length(looper)){
  # set path to sea level curve tiles 
  p <- curve_path
  p_k <- list.files(p,recursive=TRUE, pattern='.ASC') # list tiles 
  p_k_looper <- p_k[grep(pattern = paste0('^',looper[i],'.*'), p_k)] # subset latitude
  p_k <- gsub(paste0('^',looper[i],'/'),paste0('+',looper[i],'/'), p_k_looper) # replace when + is missing
  p_k_l <- split(p_k,f=substr(p_k,1,11)) # split by longitude
  seacurve_path_l <- lapply(p_k_l, function(x) paste0(p,gsub('\\+','',x))) # list path names 
  
  # CREATE DROWNING AGE RASTERS FOR A CERTAIN LATITUDE 
  parallel::mclapply(seacurve_path_l, function(x) agemap(r=r, seacurve=x, res_fact = 8, to_path=to_path),mc.cores = 4)
  # in case of missing tiles
  seacurve_all <- gsub('/','-',names(seacurve_path_l))
  seacurve_done <- gsub('.asc','',gsub('AGE','',list.files(pattern='.asc$',recursive = T)))
  seacurve_path_missing_l <- seacurve_path_l[c(!seacurve_all %in% seacurve_done)]
  parallel::mclapply(seacurve_path_missing_l, function(x) agemap(r=r, seacurve=x, res_fact = 8, to_path=to_path),mc.cores = 4)
  print(i)
}

# merge and save as tif
setwd(paste0(to_path,'AGE'))
p_age <- list.files(pattern='.asc$')

# make an empty raster with the wished extent
e_gebco <- extent(r)
template <- raster(e_gebco)
projection(template) <- crs(r)
writeRaster(template, file="AGE.tif", format="GTiff")

# mosaic the raster
mosaic_rasters(gdalfile=p_age,dst_dataset="AGE.tif",of="GTiff")
gdalinfo("AGE.tif")
```

#### 7. CREATE CUMULATIVE SEA SHELF MAPS

``` r
shallow <- 140
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
  p_k_l <- split(p_k,f=substr(p_k,1,11)) # split by longitude
  seacurve_path_l <- lapply(p_k_l, function(x) paste0(p,gsub('\\+','',x))) # list path names 

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

      
for(i in 1:length(p_SLW)){
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
```

#### EXTRA: CONVERT AGE TO ISOCHRONES

``` r
r <- rast('AGE.tif')

# isochrone polygons smoothing
p <- as.polygons(r)
cdi_p <- sf::st_as_sf(p) %>% smoothr::smooth(method = "ksmooth", smoothness = 1) # slow!

# isochrone lines smoothing
l <- as.lines(p)
cdi_l <- sf::st_as_sf(l) %>% smoothr::smooth(method = "ksmooth", smoothness = 1) # fast!
```
