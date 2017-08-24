#' Get Key
#'
#' Get a key from the BD API gateway to access BD services
#' @param url URL of the BD API gateway
#' @param username user name for BrownDog
#' @param password password for BrownDog
#' @return BD API key
#' @import RCurl
#' @import jsonlite
#' @import utils
#' @examples 
#' \dontrun{
#' get_key("https://bd-api-dev.ncsa.illinois.edu", "your email", "password")
#' }
#' @export
get_key = function(url, username, password){

  if(grepl("@", url)){
    auth_host   <- strsplit(url,'@')
    url         <- auth_host[[1]][2]
    auth        <- strsplit(auth_host[[1]][1],'//')
    userpass    <- utils::URLdecode(auth[[1]][2])
    bdsURL      <- paste0(auth[[1]][1],"//", url, "/v1/keys")
  }else{
    userpass <- paste0(username,":", password)
    bdsURL <- paste0(url,"/v1/keys")
  }
  curloptions <- list(userpwd = userpass, httpauth = 1L)
  httpheader <- c("Accept" = "application/json")
  responseKey <- RCurl::httpPOST(url = bdsURL, httpheader = httpheader,curl = RCurl::curlSetOpt(.opts = curloptions))
  key <- jsonlite::fromJSON(responseKey)[[1]]
  return(key) 
}
