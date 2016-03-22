source("/path/to/bd.r")

#key <- browndog.getKey("https://bd-api.ncsa.illinois.edu/keys")
#print(key)
#browndog.saveKey(key)
#token <- browndog.getToken("localhost:8080")
#print(token)

#outputs <- browndog.outputs("localhost:8080", "png")
#print(outputs)

example.convert = function(){
  bds            <- "localhost:8080"
  input_filename <- "path/to/inputfilename"
  output         <- "png" # the output format
  output_path    <-"/path/for/outputfile/"
  wait           <- 80
  output_file    <- browndog.convert(bds,input_filename,output,output_path,wait)
  print(output_file)
}
#example.convert()

example.bdextract = function(){
  #file <- "path/to/file" #complete path to the file or file URL
  wait <- 60
  bds <- "localhost:8080" 
  #bds <- "bd-api.ncsa.illinois.edu"
  metadata   <- browndog.extract(bds,file,wait)
 }

metadata <- example.bdextract()
#print(metadata$versusmetadata)
print(metadata)

#browndog.index("https://bd-api.ncsa.illinois.edu","/complete/path/directory/",120)
#browndog.find("https://bd-api.ncsa.illinois.edu","complete/path/to/queryfile",60)
