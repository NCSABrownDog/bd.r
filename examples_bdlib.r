#source("/path/to/bd.r")

#Obtain Brown Dog Access Key
bds <- "https://bd-api.ncsa.illinois.edu"
key <- browndog.getKey(bds)
print(key)

#Saving the key
#browndog.saveKey(key)

#Obtain and save an access token for the key
token <- browndog.getToken("https://bd-api.ncsa.illinois.edu")
#token <- browndog.getToken("localhost:8080")
print(token)
#browndog.saveToken(token)

#Save both key and corresponsing token to a file for Brown Dog access
browndog.saveKeyToken(key,token)

#Obtain all possible output formats from Brown Dog
#outputs <- browndog.outputs("bd-api.ncsa.illinois.edu", "png", token)
#print(outputs)

# Use Brown Dog conversion
example.bdconvert = function(){
  #bds            <- "localhost:8080"
  bds            <- "bd-api.ncsa.illinois.edu"
  input_filename <- "path/to/inputfilename"
  output         <- "png" # an output format
  output_path    <-"/path/for/outputfile/" # note the path ends with '/'
  wait           <- 80
  output_file    <- browndog.convert(bds,input_filename,output,output_path,token, wait)
  print(output_file)
}
#example.bdconvert()

#Use Borwn Dog extraction 
example.bdextract = function(){
  file <- "path/to/file" #complete path to the file or file URL
  wait <- 60
  #bds <- "localhost:8080" 
  bds <- "bd-api.ncsa.illinois.edu"
  metadata   <- browndog.extract(bds,file,token,wait)
 }
#metadata <- example.bdextract()
#print(metadata$versusmetadata)
#print(metadata)

#Index and query a collection of files
#browndog.index("bd-api.ncsa.illinois.edu","/complete/path/to/directory/",token,120)
#browndog.find("bd-api.ncsa.illinois.edu","complete/path/to/queryfile",token,60)

