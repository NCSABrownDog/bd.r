#!/usr/bin/Rscript
library(RCurl)
library(bitops)
library(hash)
library(jsonlite)
library(gdata)

args <- commandArgs(trailingOnly = TRUE)
username <- grep('username', args, value=TRUE)
username <- substr(username, 12, nchar(username))
password <- grep('password', args, value=TRUE)
password <- substr(password, 12, nchar(password))
key <- ""

print (username)
# Get a key from the BD API gateway to access BD services
#' @param bds: URL of the BD API gateway
#' @return BD API key 
browndog.getKey = function(bds){
  if(key != ""){
    return(key)
  }
  if(grepl("@", bds)){
    auth_host   <- strsplit(bds,'@')
    bds         <- auth_host[[1]][2]
    auth        <- strsplit(auth_host[[1]][1],'//')
    userpass    <- URLdecode(auth[[1]][2])
    bdsURL      <- paste0(auth[[1]][1],"//", bds, "/keys")
  }else{
    userpass <- paste0(username,":", password)
    bdsURL <- paste0(bds,"/keys")
  }
  curloptions <- list(userpwd = userpass, httpauth = 1L)
  httpheader <- c("Accept" = "application/json")
  responseKey <- httpPOST(url = bdsURL, httpheader = httpheader,curl = curlSetOpt(.opts = curloptions))
  key <- fromJSON(responseKey)[[1]]
  return(key) 
}

# Save a key obtained from the BD API gateway to a file for future access to BD services 
#' @param key: BD API key
#' @return 
browndog.saveKey = function(key){
  fileConn <- file(key_file)
  keys <- paste0("key <-", "\"",key,"\"")
  write(c(keys), fileConn, append = TRUE)
  close(fileConn)
}

# Get a token for a specific key from the BD API gateway to access BD services
#' @param bds: URL of the BD API gateway
#' @return BD API token 
browndog.getToken = function(bds, key){
  userpass <- paste0(username,":", password)
  curloptions <- list(userpwd = userpass, httpauth = 1L)
  httpheader <- c("Accept" = "application/json")
  bdsURL <- paste0(bds,"/keys/",key,"/tokens")
  responseToken <- httpPOST(url = bdsURL, httpheader = httpheader,curl = curlSetOpt(.opts = curloptions))
  token <- fromJSON(responseToken)[[1]]
  return(token)
}

# Save a token obtained from the BD API gateway to a file for 24 hr access to BD services 
#' @param token: BD API token
#' @return
browndog.saveToken = function(token){
  fileConn <- file(key_file)
  tokens <- paste0("token <-", "\"",token,"\"")
  write(c(tokens), fileConn, append = TRUE)
  close(fileConn)
}

# Save both key and corresponding token obtained from the BD API gateway to a file to access BD services 
#' @param key: BD API key
#' @param token: BD API token
#' @return
browndog.saveKeyToken = function(key, token){
  fileConn <- file(key_file)
  key <- paste0("key <-", "\"",key,"\"")
  token <- paste0("token <-", "\"",token,"\"")
  write(c(key, token), fileConn, append = TRUE)
  close(fileConn)
}
  
#'Check Brown Dog Service for available output formats for the given input format.
#'@param bds: The URL to the Brown Dog server to use.
#'@param input: The format of the input file.
#'@param token: Brown Dog access token
#'@return: A string array of reachable output format extensions.
browndog.outputs = function(bds, inputformat, token){
  api_call    <- paste0("https://", bds, "/dap/inputs/", inputformat)
  #api_call    <- paste0(bds, "/dap/inputs/", inputformat)  #localhost
  httpheader  <- c("Accept" = "text/plain", "Authorization" = token)
  r   <- httpGET(url = api_call, httpheader = httpheader)
  arr <- strsplit(r,"\n")
  if(length(arr[[1]]) == 0){
    return(list())
  } else{
    return(arr)
  }
}

# Try and download a file.
# 
# This will download a file, if a 404 is returned it will wait until
# the file is available. If the file is still not available after
# timeout tries, it will return NA. If the file is downloaded it will
# return the name of the file
# 
#' @name browndog.download
#' @title Download file from browndog.
#' @param url: the url of the file to download
#' @param file: the filename
#' @param token: Brown Dog access token
#' @param timeout : timeout number of seconds to wait for file (default 60)
#' @return returns name of file if successfull or NA if not.
#' 
browndog.download = function(url, file, token, timeout = 60) {
  count <- 0
  httpheader <- c(Authorization = token)
  .opts <- list(httpheader = httpheader, httpauth = 1L, followlocation = TRUE)
  while (!url.exists(url,.opts = .opts) && count < timeout) {
    count <- count + 1
    Sys.sleep(1)
  }
  if (count >= timeout) {
    return(NA)
  }
  f = CFILE(file, mode = "wb")
  curlPerform(url = url, writedata = f@ref, .opts = .opts)
  RCurl::close(f)
  return(file)
}

# Convert a file using Brown Dog Conversion service
#' @param bds: The URL to the Brown Dog Server to use
#' @param input_filename: The input filename
#' @param output: The output format extension
#' @param output_path: The path for the created output file. May contain different filename
#' @param token: Brown Dog access token
#' @param wait: The amount of time to wait for the DAP service to respond. Default is 60
#' @return: The output filename 
browndog.convert = function (bds, input_filename, output, output_path, token, wait=60, download=TRUE){
  convert_api <- paste0("https://", bds,"/dap/convert/", output, "/") 
  #convert_api <- paste0(bds,"/dap/convert/", output, "/")  # for localhost
  httpheader <- c(Accept="text/plain", Authorization = token)
  curloptions <- list(httpheader = httpheader)
  result_bds <- postForm(convert_api,"file"= fileUpload(input_filename),.opts = curloptions)
  url             <- gsub('.*<a.*>(.*)</a>.*', '\\1', result_bds)
  inputbasename   <- strsplit(basename(input_filename),'\\.')
  outputfile      <- paste0(output_path,inputbasename[[1]][1],".", output)
  if (download){
    output_filename <- browndog.download(url[1], outputfile, token, wait)
  }else{
    return(url[1]) 
  }
  return(output_filename)
}

#' Extract content-based metadata from the given input file's content using Brown Dog extraction service
#' 
#' @param bds: The URL to the Brown Dog server to use.
#' @param file: The input file could be URL or file with the path
#' @param token: Brown Dog access token
#' @param wait: The amount of time to wait for the DTS to respond. Default is 60 seconds
#' @return The extracted metadata in JSON format
#'  
browndog.extract = function (bds, file, token, wait = 60){
  if(startsWith(file,'http://') || startsWith(file,'https://')){
    postbody   <- toJSON(list(fileurl = unbox(file)))
    httpheader <- c("Content-Type" = "application/json", "Accept" = "application/json", "Authorization" = token)
    uploadurl  <- paste0("https://", bds,"/dts/api/extractions/upload_url") 
    #uploadurl  <- paste0(bds,"/dts/api/extractions/upload_url") #for localhost
    res_upload <- httpPOST(url = uploadurl, postfields = postbody, httpheader = httpheader)
  } else{
    httpheader <- c("Accept" = "application/json", "Authorization" = token)
    curloptions <-list(httpheader=httpheader)
    res_upload <- postForm(paste0("https://", bds,"/dts/api/extractions/upload_file"),
                  "File" = fileUpload(file),
                 .opts = curloptions)
    #res_upload <- postForm(paste0(bds,"/dts/api/extractions/upload_file"),
    #                       "File" = fileUpload(file),
    #                       .opts = curloptions)
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
    res_tags     <- httpGET(url = paste0("https://", bds, "/dts/api/files/", file_id,"/tags"), httpheader = httpheader)
    #res_tags     <- httpGET(url = paste0(bds, "/dts/api/files/", file_id,"/tags"), httpheader = httpheader)
    tags         <- fromJSON(res_tags)
    res_techmd   <- httpGET(url = paste0("https://", bds,"/dts/api/files/",file_id,"/metadata.jsonld"), httpheader = httpheader)
    #res_techmd   <- httpGET(url = paste0("https://", bds,"/dts/api/files/",file_id,"/technicalmetadatajson"), httpheader = httpheader) #deprecated
    #res_techmd   <- httpGET(url = paste0(bds,"/dts/api/files/",file_id,"/metadata.jsonld"), httpheader = httpheader)
    techmd       <- fromJSON(res_techmd, simplifyDataFrame = FALSE)
    res_vmd      <- httpGET(url = paste0("https://", bds, "/dts/api/files/",file_id,"/versus_metadata"), httpheader = httpheader)
    #res_vmd      <- httpGET(url = paste0(bds, "/dts/api/files/",file_id,"/versus_metadata"), httpheader = httpheader)
    versusmd     <- fromJSON(res_vmd)
    metadatalist <- list(id = unbox(tags$id), filename = unbox(tags$filename), tags = tags$tags, technicalmetadata = techmd, versusmetadata = versusmd)
    #metadatalist <- list(id = unbox(tags$id), filename = unbox(tags$filename), tags = tags$tags, technicalmetadata = techmd)
    metadata <- toJSON(metadatalist)
    return(metadata)
  }
}

#' Extracts signatures/tags from files via BD in order to index their contents
#' @param bds: The URL to the Brown Dog server to use
#' @param directory: The directory of files to index
#' @param wait: The amount of time per file to wait for the BD to respond. Default is 60 seconds
#' @param token: Brown Dog access token
#' @return: The indexed directory. A '.index.tsv' file will now be present containing the derived data
browndog.index = function(bds, directory, token, wait=60){
  #if(!endsWith(directory,'/')){
  #  directory<-paste0(directory,'/')
  #}
  output_filename <- paste0(directory,'.index.tsv')
 
  files<-list.files(directory)
  for(i in 1:length(files)){
    #print(files[i])
    metadata <- browndog.extract(bds, paste0(directory,files[i]), token, wait)
    tags<-fromJSON(metadata)$tags
    #print(tags)
    line<- toString(files[i])
    if(!is.null(tags) && length(tags)!= 0){
      for(i in 1:length(tags)){
        line<- paste0(line,'\t["',toString(tags[i]),'"]')
      }
    }
    versusmd<-fromJSON(metadata)$versusmetadata
    print(versusmd)
    if(!is.null(versusmd) && length(versusmd)!= 0){
      line<- paste0(line,'\t',toJSON(versusmd$descriptor[[1]]))
    }
    write(line,file = output_filename,append = TRUE)
  }
}

#' Compute the distance between the two descriptors
#' 
#' @param descriptor1: A content descriptor for a file
#' @param descriptor2: A content descriptor for another file
#' @return The distance between the teo descriptor
#'  

descriptor_distance = function(descriptor1,descriptor2){
  descriptor1<-descriptor1[[1]]
  descriptor2<-fromJSON(descriptor2)
  
  # Check if exactly the same for 'character' type 
  if(typeof(descriptor1) == "character" && typeof(descriptor2) == "character"){
    if(descriptor1 == descriptor2){
      print("extactly same")
      return(0)
    }
  }
  # Check for the array of numbers
  if(typeof(descriptor1) == "double" && typeof(descriptor2) == "double"){
    print("Both double")
    print(paste0("Dimension=", dim(descriptor1)[1]," ",dim(descriptor1)[2]," length =",length(dim(descriptor1))))
    print(paste0("Dimension=", dim(descriptor2)[1]," ",dim(descriptor2)[2]," length =",length(dim(descriptor2))))
    
    if(length(dim(descriptor1)) == length(dim(descriptor2))){
      if(length(dim(descriptor1)) == 1){
        d <- norm(descriptor1-descriptor2,"2")
        return(d) 
      }else if(length(dim(descriptor1)) == 2 && dim(descriptor1)[1] == dim(descriptor2)[1] && dim(descriptor1)[2] == dim(descriptor2)[2]){
        print("Dimensions are same")
        d<- 0.0
        n<-dim(descriptor1)[1]
        for(i in 1:n ){
          print("norm")
          print(norm((descriptor1[i,]-descriptor2[i,]),"2"))
          d <- d + norm((descriptor1[i,]-descriptor2[i,]),"2")
        }
        d<- d/n
        return(d)
      }else{
        return(.Machine$double.xmax)
      }
    } else{
      return(.Machine$double.xmax)
    }
  }else{
    return(.Machine$double.xmax)
  }
}

#' Calculate the closest distance between the two sets of descriptors
#' @param descriptor_set1: A set of descriptors for a file
#' @param descriptor_set2: A set of descriptors for another file
#' @return The distance between the closest two descriptors in each set.
#' 
descriptor_set_distance = function(descriptor_set1, descriptor_set2){
  d <- .Machine$double.xmax
  for(i in 1: length(descriptor_set1)){
    if(length(descriptor_set2) > 0){
      for(j in 1:length(descriptor_set2)){
        descriptor_set2c <- descriptor_set2[[j]]
        dij <- descriptor_distance(descriptor_set1[i],descriptor_set2c)
        print(dij)
        if(dij < d){
            d <- dij
        }
      }
    }
  }
  return(d)
}

#' Search a directory for similar files to the query. Directory must be indexed already and a '.index.tsv' present
#' @param bds: The URL to the Brown Dog server to use
#' @param query_filename: The query file
#' @param token: Brown Dog access token
#' @return: The name of the file that is most similar
browndog.find = function(bds,query_filename,token,wait=60){
  metadata  <- browndog.extract(bds,query_filename,token,wait)
  tags      <- fromJSON(metadata)$tags
  versusmd  <- fromJSON(metadata)$versusmetadata
  query_descriptors <- list(versusmd$descriptor[[1]])
  if(!is.null(tags) && length(tags)!= 0){
    for(i in 1:length(tags)){
      query_descriptors <- append(query_descriptors,tags[i])
    }  
  }
  ranking       <- hash()
  #path_to_index <- "/path/to/index/"
  index_fr      <- file(paste0(path_to_index,".index.tsv"),"r")
  lines         <- readLines(index_fr)
  
  for(i in 1:length(lines)){
    line        <- strsplit(lines[i],'\t')
    filename    <- line[[1]][1]
    descriptors <- list()
    if(length(line[[1]])>1){
      for(j in 2:length(line[[1]])){
        descriptors <- c(descriptors,line[[1]][j])
      }
    }
    print(paste0("filename: ",filename))
    d <- descriptor_set_distance(query_descriptors,descriptors)
    ranking[filename] <- d
    print(ranking)
  }
  print("Distance of each file to the query file")
  print(ranking)
  close(index_fr)
}



