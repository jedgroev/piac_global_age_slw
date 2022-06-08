#' @title scialla
#' 
#' @author Johannes De Groeve
#' @description generates sea shelf extent map for every sea level stand using a spatiotemporal sea level curve. 
#' 
#' @param r bathymetric model (tile/extent)
#' @param seacurve paths of the sea level curve dataset 
#' @param res_fact resolution factor compared to the original resolution of the bathymetry 
#' @param shallow sea level stand below sea level for which to calculate the sea shelf extent (e.g. -40 to 0; shallow=40)
#' @param to_path output path where the resulting dataset is stored
#' 
#' @return exports a map for every time period for which the sea level stand is known
#'
#' @export
#' 
scialla <- function(r,seacurve, res_fact = 8, shallow=40, to_path){
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
  DIR <- paste0(to_path, 'SLW', shallow,'/')
  # THE PATH NAMES 
  paths <- gsub('RSL','SLW',gsub(gsub('[()]','',from_path),DIR,gsub('[()]','',seacurve)))
  #paths <- gsub('RSL','SLW',gsub('koene/MAPS/',DIR,seacurve))
  
  # IF DIR DOES NOT EXIST, GENERATE THE FOLDER STRUCTURE  
  from_path <- paste0(dirname(dirname(dirname(seacurve))),'/')[1]
  #to_path <- paste0(dirname(dirname(dirname(dirname(dirname(seacurve))))),'/',DIR)[1] 
  if(dir.exists(dirname(paths[1])) == FALSE){
    copy.dir.tree(from = from_path, to = DIR)
  }
  # WRITE RASTERS 
  writeRaster(scialla,filename=paths,bylayer=TRUE, format='ascii',overwrite=T)
  return(list(from_path, to_path))
}
