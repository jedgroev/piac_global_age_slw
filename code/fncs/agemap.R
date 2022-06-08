#' @title agemap
#' 
#' @author Johannes De Groeve
#' @description generates an agemap for a specific tile
#' @param r bathymetric model (tile/extent)
#' @param seacurve paths of the sea level curve dataset 
#' @param res_fact resolution factor compared to the original resolution of the bathymetry 
#' @param to_path output path where the resulting dataset is stored
#' 
#' @return exports a map with most recent drowning age 
#'
#' @export
#' 
agemap <- function(r,seacurve, res_fact = 8, to_path){
  # r = bathymetric model 
  # seacurve = path to a single tile; seacurve = seacurve_path_l
  tile <- gsub('/','-',gsub('^S/','-',gsub('^/','+',str_sub(seacurve[1],-23,-13))))
  seacurvestack <- stack(seacurve)
  crs(seacurvestack) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  e <- extent(seacurvestack)
  elev <-raster::crop(r, e)
  print('cropped')
  # depending from the wished resolution we will need to aggregate 
  elev <- raster::aggregate(elev, fact=res_fact)
  print('aggregated')
  #elev_test <- aggregate(elev, fact=8, fun=fasterAgg.Fun)
  #elev_res <-aggregate(elev,fact=8,fun=aggrRcppW)
  
  # RESAMPLE CURVE 
  seacurvestack <- resample(seacurvestack, elev, method="bilinear")
  
  period <- data.frame(index=1:dim(seacurvestack)[3],period=readr::parse_number(names(seacurvestack)) * 1000)
  
  elev_bool <- elev > seacurvestack 
  elev_bool_df <- as.data.frame(elev_bool, xy=T)
  coords <- elev_bool_df[,c('x','y')]
  
  coords$index <- apply(elev_bool_df[,3:ncol(elev_bool_df)], 1, function(x) min(which(x == TRUE)))
  coords <- plyr::join(coords, period, type='left', by='index')[,c('x','y','period')]
  age <- rasterFromXYZ(coords)  #Convert first two columns as lon-lat and third as value     
  print('converted')
  #dfr[is.infinite(dfr)] <- NA
  crs(age) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  DIR <- gsub('//','/',paste0(to_path,'AGE/'))
  if(dir.exists(DIR) == FALSE){
    dir.create(DIR)
  }
  writeRaster(age,paste0(DIR,'AGE',tile,'.ASC'))
}
