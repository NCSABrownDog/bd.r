#' Get Token
#'
#' Get a Token from the BD API gateway to access BD services
#' @param url URL of the BD API gateway
#' @param key permanet key for BD API
#' @return BD API Token 
#' @import RCurl
#' @import jsonlite
#' @examples 
#' \dontrun{
#' key <- get_key("https://bd-api-dev.ncsa.illinois.edu", "your email", "password")
#' get_token("https://bd-api-dev.ncsa.illinois.edu", key)
#' }
#' @export
get_token = function(url, key){

  httpheader <- c("Accept" = "application/json")
  bdsURL <- paste0(url,"/v1/keys/",key,"/tokens")
  responseToken <- RCurl::httpPOST(url = bdsURL, httpheader = httpheader)
  token <- jsonlite::fromJSON(responseToken)[[1]]
  return(token)
}
