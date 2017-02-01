#' Extract file 
#' 
#' Extract content-based metadata from the given input file's content using Brown Dog extraction service
#' @param url: The URL to the Brown Dog server to use.
#' @param file: The input file could be URL or file with the path
#' @param token: Brown Dog access token
#' @param wait: The amount of time to wait for the DTS to respond. Default is 60 seconds
#' @return The extracted metadata in JSON format
#'  
extract_file = function (url, file, token, wait = 60){
  library(RCurl)
  library(jsonlite)
  if(startsWith(file,'http://') || startsWith(file,'https://') || startsWith(file,'ftp://')){
    postbody   <- toJSON(list(fileurl = unbox(file)))
    httpheader <- c("Content-Type" = "application/json", "Accept" = "application/json", "Authorization" = token)
    uploadurl  <- paste0(bds,"/dts/api/extractions/upload_url") 
    res_upload <- httpPOST(url = uploadurl, postfields = postbody, httpheader = httpheader)
  } else{
    httpheader <- c("Accept" = "application/json", "Authorization" = token)
    curloptions <-list(httpheader=httpheader)
    res_upload <- postForm(paste0(url,"/dts/api/extractions/upload_file"),
                           "File" = fileUpload(file),
                           .opts = curloptions)
  }
  r           <- fromJSON(res_upload)
  file_id     <- r$id
  print(file_id)
  httpheader  <- c("Accept" = "application/json", "Authorization" = token )
  if (file_id != ""){
    while (wait > 0){
      res_status <- httpGET(url = paste0("https://", bds, "/dts/api/extractions/",file_id,"/status"), httpheader = httpheader)
      status     <- fromJSON(res_status)
      if (status$Status == "Done"){
        #print(status)
        break
      }
      Sys.sleep(2)
      wait <- wait -1  
    }
    res_tags     <- httpGET(url = paste0(url, "/dts/api/files/", file_id,"/tags"), httpheader = httpheader)
    tags         <- fromJSON(res_tags)
    res_techmd   <- httpGET(url = paste0(url,"/dts/api/files/",file_id,"/metadata.jsonld"), httpheader = httpheader)
    techmd       <- fromJSON(res_techmd, simplifyDataFrame = FALSE)
    res_vmd      <- httpGET(url = paste0(url, "/dts/api/files/",file_id,"/versus_metadata"), httpheader = httpheader)
    versusmd     <- fromJSON(res_vmd)
    metadatalist <- list(id = unbox(tags$id), filename = unbox(tags$filename), tags = tags$tags, technicalmetadata = techmd, versusmetadata = versusmd)
    #metadatalist <- list(id = unbox(tags$id), filename = unbox(tags$filename), tags = tags$tags, technicalmetadata = techmd)
    metadata <- toJSON(metadatalist)
    return(metadata)
  }
}
