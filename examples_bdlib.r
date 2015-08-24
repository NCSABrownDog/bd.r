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

example.extract = function(){
  file <- "path/to/file" #complete path to the file or URL
  wait <- 30
  dts  <- "http://dts.ncsa.illinois.edu"
  key  <- "DTS key"
  metadata   <- browndog.extract(dts,file,wait,key)
  print(metadata)
}
metadata <- example.extract()
print(metadata$versusmetadata)

#browndog.index("http://dts.ncsa.illinois.edu","/complete/path/directory/",120,key)
#browndog.find("http://dts.ncsa.illinois.edu","complete/path/to/queryfile",60,key)
