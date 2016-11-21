#' convert file
#' 
#' Convert a file using Brown Dog Conversion service
#' @param url: The URL to the Brown Dog Server to use
#' @param input_filename: The input filename
#' @param output: The output format extension
#' @param output_path: The path for the created output file. May contain different filename
#' @param token: Brown Dog access token
#' @param wait: The amount of time to wait for the DAP service to respond. Default is 60
#' @return: The output filename 
#' @export
convert_file = function (url, input_filename, output, output_path, token, wait=60, download=TRUE){
  library(RCurl)
  convert_api <- paste0(url,"/dap/convert/", output, "/") 
  httpheader <- c(Accept="text/plain", Authorization = token)
  curloptions <- list(httpheader = httpheader)
  result_bds <- postForm(convert_api,"file"= fileUpload(input_filename),.opts = curloptions)
  url             <- gsub('.*<a.*>(.*)</a>.*', '\\1', result_bds)
  inputbasename   <- strsplit(basename(input_filename),'\\.')
  outputfile      <- paste0(output_path,inputbasename[[1]][1],".", output)
  if (download){
    output_filename <- download(url[1], outputfile, token, wait)
  }else{
    return(url[1]) 
  }
  return(output_filename)
}
