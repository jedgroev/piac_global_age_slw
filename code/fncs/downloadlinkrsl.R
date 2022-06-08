#' @title download_link_rsl
#' 
#' @author Johannes De Groeve
#' @description return the download link of the spatio-temporal sea level curve (RSL)
#' 
#' @return a vector with the download link
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # download link RSL
#' download_link_rsl()
#' }
#' 
download_link_rsl <- function(){
  url <- 'https://uvaauas.figshare.com/ndownloader/articles/20029991'
  print(paste0('copy this link in your browser to download the dataset: ', url))
  print('If not available contact Johannes De Groeve.')
} 

