#' convert file
#' 
#' Convert a file using Brown Dog Conversion service
#' @param url The URL to the Brown Dog Server to use
#' @param input_file The input file, either local file with path, or file url
#' @param output The output format extension
#' @param output_path The path for the created output file. May contain different filename. note the path ends with '/'
#' @param token Brown Dog access token
#' @param wait The amount of time to wait for the DAP service to respond. Default is 60
#' @param download The flag to download the result file. Default is true
#' @return The output filename 
#' @import RCurl
#' @import httpuv
#' @examples 
#' \dontrun{
#' key <- get_key("https://bd-api-dev.ncsa.illinois.edu", "your email", "password")
#' token <- get_token("https://bd-api-dev.ncsa.illinois.edu", key)
#' convert_file("https://bd-api-dev.ncsa.illinois.edu", 
#' "http://browndog.ncsa.illinois.edu/examples/gi/Dongying_sample.csv", "xlsx", "/", 
#' token)
#' }
#' @export
convert_file = function (url, input_file, output, output_path, token, wait=60, download=TRUE){
  httpheader <- c(Accept="text/plain", Authorization = token)
  curloptions <- list(httpheader = httpheader)
  if(startsWith(input_file,'http://') || startsWith(input_file,'https://') || startsWith(input_file,'ftp://')){
    convert_api <- paste0(url,"/v1/conversions/", output, "/", httpuv::encodeURIComponent(input_file)) 
    result_bds <- getURL(convert_api,.opts = curloptions)
  }
  else{
    convert_api <- paste0(url,"/v1/conversions/", output, "/") 
    result_bds <- RCurl::postForm(convert_api,"file"= RCurl::fileUpload(input_file),.opts = curloptions)
  }
  #convert is not success
  if(!startsWith(result_bds, "http")){
    return(result_bds)
  }
  result_url <- gsub('.*<a.*>(.*)</a>.*', '\\1', result_bds)
  if (download){
    inputbasename   <- strsplit(basename(input_file),'\\.')
    outputfile      <- paste0(output_path,inputbasename[[1]][1],".", output)
    output_filename <- download(result_url[1], outputfile, token, wait)
  }else{
    return(result_url[1]) 
  }
  return(output_filename)
}
