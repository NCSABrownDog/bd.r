#' Get Key
#'
#' Get a key from the BD API gateway to access BD services
#' @param url: URL of the BD API gateway
#' @param username: user name for BrownDog
#' @param password: password for BrownDog
#' @return BD API key 
#' @export
get_key = function(url, username, password){
  library(bitops)
  library(RCurl)
  library(jsonlite)
  if(grepl("@", url)){
    auth_host   <- strsplit(url,'@')
    url         <- auth_host[[1]][2]
    auth        <- strsplit(auth_host[[1]][1],'//')
    userpass    <- URLdecode(auth[[1]][2])
    bdsURL      <- paste0(auth[[1]][1],"//", url, "/keys")
  }else{
    userpass <- paste0(username,":", password)
    bdsURL <- paste0(url,"/keys")
  }
  curloptions <- list(userpwd = userpass, httpauth = 1L)
  httpheader <- c("Accept" = "application/json")
  responseKey <- httpPOST(url = bdsURL, httpheader = httpheader,curl = curlSetOpt(.opts = curloptions))
  key <- fromJSON(responseKey)[[1]]
  return(key) 
}
