#' Download file from browndog
#' 
#' This will download a file, if a 404 is returned it will wait until
#' the file is available. If the file is still not available after
#' timeout tries, it will return NA. If the file is downloaded it will
#' return the name of the file
#' @param url the url of the file to download
#' @param file the filename
#' @param token Brown Dog access token
#' @param timeout timeout number of seconds to wait for file (default 60)
#' @return the name of file if successfull or NA if not.
#' @import RCurl
#' @examples 
#' \dontrun{
#' key <- get_key("https://bd-api-dev.ncsa.illinois.edu", "your email", "password")
#' token <- get_token("https://bd-api-dev.ncsa.illinois.edu", key)
#' download("https://bd-api-dev.ncsa.illinois.edu", "vdc.csv", token)
#' }
#' @export
download = function(url, file, token, timeout = 60) {
  count <- 0
  httpheader <- c(Authorization = token)
  .opts <- list(httpheader = httpheader, httpauth = 1L, followlocation = TRUE)
  while (!RCurl::url.exists(url,.opts = .opts) && count < timeout) {
    count <- count + 1
    Sys.sleep(1)
  }
  if (count >= timeout) {
    return(NA)
  }
  f = RCurl::CFILE(file, mode = "wb")
  RCurl::curlPerform(url = url, writedata = f@ref, .opts = .opts)
  RCurl::close(f)
  return(file)
}
