#' Get input format.
#'
#' Check Brown Dog Service for available output formats for the given input format.
#' @param url The URL to the Brown Dog server to use.
#' @param inputformat The format of the input file.
#' @param token Brown Dog access token
#' @return: A string array of reachable output format extensions.
#' @import RCurl
#' @examples 
#' \dontrun{
#' key <- get_key("https://bd-api-dev.ncsa.illinois.edu", "your email", "password")
#' token <- get_token("https://bd-api-dev.ncsa.illinois.edu", key)
#' get_output_formats("https://bd-api-dev.ncsa.illinois.edu", "csv", 
#' token)
#' }
#' @export
get_output_formats = function(url, inputformat, token){
  api_call    <- paste0(url, "/v1/conversions/inputs/", inputformat)
  httpheader  <- c("Accept" = "text/plain", "Authorization" = token)
  r   <- RCurl::httpGET(url = api_call, httpheader = httpheader)
  arr <- strsplit(r,"\n")
  if(length(arr[[1]]) == 0){
    return(list())
  } else{
    return(arr)
  }
}
