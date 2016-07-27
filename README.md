* Install R 

  * Download R from anyone of the mirror site: ```https://cran.r-project.org/mirrors.html```
  * Install R following the instructions provided by the website: ```https://cran.r-project.org/doc/manuals/r-release/R-admin.html```  
  
  * Install packages required for BrownDog Library - RCurl, bitops, hash, jsonlite, gdata
  
      * Example: 
      ```> install.packages("RCurl", lib="/home/data/Rpackages/")```
  * Create `.Renviron` in the home directory and add `R_LIBS=/home/data/Rpackages/` to it so that whenever R starts, it knows where to find the installed packages.
   
* (optional)Install R studio if want to test the Brown Dog R library in more convienient way: ```https://www.rstudio.com/products/rstudio/download/```

* Brown Dog R Library (BD-R) consists of 4 files:
 	* bd.r - It contains all Brown Dog service related methods
	* config.r - Provid your BD user name and password in this file. 
	* key.r - Provide your Brown Dog access key and token in this file. If you do not have one, use browndog.getKey(), browndog.getToken to obtain and use browndog.saveKeyToken() to save. See the complete method definition in bd.r and usage in examples_bdlib.r
 	* examples_bdlib.r - provide some examples usage as how to use BD-R
* 	Note that you need to use ```source("/path/to/bd.r")``` to use BD-R
* 	Also set the path for config.r, key.r, and index file in bd.r 
* 	getKeys and getToken method take complete URL as input while other methods take the host URL without protocol specified, i.e., http ot https. 
       
       
