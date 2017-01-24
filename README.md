# Brown Dog R Library (BD-R):

## Installation

 * Download R from anyone of the mirror site: ```https://cran.r-project.org/mirrors.html```
 * Install R following the instructions provided by the website: ```https://cran.r-project.org/doc/manuals/r-release/R-admin.html```  
``` 
library(devtools)
install_github("NCSABrownDog/bd.r")
library(BrownDog)
```

* Brown Dog R Library (BD-R) consists of 4 files:
 	* bd.r - It contains all Brown Dog service related methods
	* config.r - Provide your BD user name and password in this file. 
	* key.r - Provide your Brown Dog access key and token in this file. If you do not have one, use browndog.getKey(), browndog.getToken to obtain and use browndog.saveKeyToken() to save. See the complete method definition in bd.r and usage in examples_bdlib.r
 	* examples_bdlib.r - provide some examples usage as how to use BD-R
* 	Note that you need to use ```source("/path/to/bd.r")``` to use BD-R
* 	Also set the path for config.r, key.r, and index file in bd.r 
* 	getKeys and getToken method take complete URL as input while other methods take the host URL without protocol specified, i.e., http ot https. 
* A set of example files for test can be found here : ``` http://browndog.ncsa.illinois.edu/examples/caltech101/```

*   Index method creates .index.tsv file with file name and correspondingg tags and content descriptors obtained from Brown Dog service. An example .index.tsv file content is shown below:

	airplane.jpg	[[0.0721,0.0508,0.777,0.1001,0,0,0,0],[0.1502,0.2493,0.5582,0.0424,0,0,0,0],[0.137,0.0398,0.7637,0.0594,0,0,0,0]]

	brontosaurus.jpg

	cougar.jpg	[[0.2571,0.5403,0.1928,0.0098,0,0,0,0],[0.4692,0.5001,0.0308,0,0,0,0,0],[0.3381,0.59,0.0713,0.0007,0,0,0,0]]

	dollar_bill.jpg	["Human Face Automatically Detected"]	["Person Automatically Detected"]	[[0.2035,0.2141,0.5724,0.0101,0,0,0,0],[0.2509,0.2266,0.5225,0,0,0,0,0],[0.1939,0.2189,0.5707,0.0165,0,0,0,0]]

	person.jpg	["Mid Close Up Automatically Detected"]	["Human Face Automatically Detected"]	["Person Automatically Detected"]	["Human Eyes Automatically Detected"]	["Human Profile Automatically Detected"]	[[0.1969,0.393,0.4079,0.0022,0,0,0,0],[0.1983,0.6102,0.1912,0.0004,0,0,0,0],[0.177,0.5175,0.3048,0.0007,0,0,0,0]]
   
*   Find method allows to find similar images in a folder for a given query image using the .index.tsv file.  An example output looks like:

	[1] "Distance of each file to the query file"
	<hash> containing 5 key-value pair(s).
  	airplane.jpg : 0.2321195
  	brontosaurus.jpg : 1.797693e+308
  	cougar.jpg : 0.5819163
  	dollar_bill.jpg : 0
  	person.jpg : 0

