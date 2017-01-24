#' Get Token
#'
#' Get a Token from the BD API gateway to access BD services
#' @param url: URL of the BD API gateway
#' @param key: permanet key for BD API
#' @return BD API Token 
#' @export
get_token = function(url, key){
  library(RCurl)
  library(jsonlite)
  httpheader <- c("Accept" = "application/json")
  bdsURL <- paste0(url,"/keys/",key,"/tokens")
  responseToken <- httpPOST(url = bdsURL, httpheader = httpheader)
  token <- fromJSON(responseToken)[[1]]
  return(token)
}
