#' @title download_link_gebco
#' 
#' @author Johannes De Groeve
#' @description return the download link of the global bathymetry for three reference years (2019,2020,2021).
#' See historical (2019, 2020) datasets at \url{https://www.gebco.net/data_and_products/historical_data_sets/} and 
#' 2021 datasets at \url{https://www.gebco.net/data_and_products/gridded_bathymetry_data/}. For 2021 there are two datasets (ice surface elevation and sub_ice_topo)
#'
#' @param dataset dataset to download (gebco_2019, gebco_2020, gebco_2021, gebco_2021_sub_ice_topo).  
#' 
#' @return a vector with the download link
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # download link bathymetric model 
#' download_link_gebco('gebco_2021')
#' download_link_gebco('gebco_2021_sub_ice_topo')
#'
#' download_link_gebco('gebco_2020')
#' download_link_gebco('gebco_2019')
#' }
#' 
download_link_gebco <- function(dataset=c('gebco_2021','gebco_2021_sub_ice_topo','gebco_2020','gebco_2019')){
  dataset <- match.arg(dataset)
  if(dataset=='gebco_2019'){
    url <- 'https://www.bodc.ac.uk/data/open_download/gebco/GEBCO_15SEC/zip/'
  } else {
    url <- paste0('https://www.bodc.ac.uk/data/open_download/gebco/',dataset,'/zip/')
  }
  print(paste0('copy this link in your browser to download the dataset: ', url))
} 

