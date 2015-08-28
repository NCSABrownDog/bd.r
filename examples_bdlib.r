source("path/to/bd.r")
example.convert = function(){
  dap            <- "dap-dev.ncsa.illinois.edu"
  input_filename <- "path/to/filename"
  output         <- "png" # the output format
  output_path    <-"path/to/output"
  wait           <- 80
  output_file    <- browndog.convert(dap,input_filename,output,output_path,wait)
  output_file
}

example.extractwithusernamepwd = function(){
  file <- "path/to/file" #complete path to the file or file URL
  wait <- 30
  dts  <- "username:password@dts.ncsa.illinois.edu" # note: username of the format 'bd.user@gmail.com' should be converted to 'bd.user%40gmail.com'
  key  <- ""
  metadata   <- browndog.extract(dts,file,wait,key)
 }
example.extractwithkey = function(){
  file <- "path/to/file" #complete path to the file or file URL
  wait <- 30
  dts  <- "dts.ncsa.illinois.edu"
  key  <- "DTS key"
  metadata   <- browndog.extract(dts,file,wait,key)
 }
metadata <- example.extractwithusernamepwd()
metadata <- example.extractwithkey()
#print(metadata$versusmetadata)
print(metadata)

#browndog.index("http://dts.ncsa.illinois.edu","/complete/path/directory/",120,key)
#browndog.find("http://dts.ncsa.illinois.edu","complete/path/to/queryfile",60,key)
