#' Extract file 
#' 
#' Extract content-based metadata from the given input file's content using Brown Dog extraction service
#' @param url The URL to the Brown Dog server to use.
#' @param file The input file could be URL or file with the path
#' @param token Brown Dog access token
#' @param wait The amount of time to wait for the DTS to respond. Default is 60 seconds
#' @return The extracted metadata in JSON format
#' @import RCurl
#' @import jsonlite
#' @examples
#' \dontrun{
#' key <- get_key("https://bd-api-dev.ncsa.illinois.edu", "your email", "password")
#' token <- get_token("https://bd-api-dev.ncsa.illinois.edu", key)
#' extract_file("https://bd-api-dev.ncsa.illinois.edu", 
#' "http://browndog.ncsa.illinois.edu/examples/gi/Dongying_sample.csv", token)
#' }
#' @export
#'  
extract_file = function (url, file, token, wait = 60){

  if(startsWith(file,'http://') || startsWith(file,'https://') || startsWith(file,'ftp://')){
    postbody   <- jsonlite::toJSON(list(fileurl = unbox(file)))
    httpheader <- c("Content-Type" = "application/json", "Accept" = "application/json", "Authorization" = token)
    uploadurl  <- paste0(url,"/dts/api/extractions/upload_url") 
    res_upload <- RCurl::httpPOST(url = uploadurl, postfields = postbody, httpheader = httpheader)
  } else{
    httpheader <- c("Accept" = "application/json", "Authorization" = token)
    curloptions <-list(httpheader=httpheader)
    res_upload <- RCurl::postForm(paste0(url,"/dts/api/extractions/upload_file"),
                           "File" = fileUpload(file),
                           .opts = curloptions)
  }
  r           <- jsonlite::fromJSON(res_upload)
  file_id     <- r$id
  print(file_id)
  httpheader  <- c("Accept" = "application/json", "Authorization" = token )
  if (file_id != ""){
    while (wait > 0){
      res_status <- httpGET(url = paste0(url, "/dts/api/extractions/",file_id,"/status"), httpheader = httpheader)
      status     <- jsonlite::fromJSON(res_status)
      if (status$Status == "Done"){
        #print(status)
        break
      }
      Sys.sleep(2)
      wait <- wait -1  
    }
    res_tags     <- RCurl::httpGET(url = paste0(url, "/dts/api/files/", file_id,"/tags"), httpheader = httpheader)
    tags         <- jsonlite::fromJSON(res_tags)
    res_techmd   <- RCurl::httpGET(url = paste0(url,"/dts/api/files/",file_id,"/metadata.jsonld"), httpheader = httpheader)
    techmd       <- jsonlite::fromJSON(res_techmd, simplifyDataFrame = FALSE)
    res_vmd      <- RCurl::httpGET(url = paste0(url, "/dts/api/files/",file_id,"/versus_metadata"), httpheader = httpheader)
    versusmd     <- jsonlite::fromJSON(res_vmd)
    metadatalist <- list(id = jsonlite::unbox(tags$id), filename = jsonlite::unbox(tags$filename), tags = tags$tags, technicalmetadata = techmd, versusmetadata = versusmd)
    #metadatalist <- list(id = unbox(tags$id), filename = unbox(tags$filename), tags = tags$tags, technicalmetadata = techmd)
    metadata <- jsonlite::toJSON(metadatalist)
    return(metadata)
  }
}
