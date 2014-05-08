# avidaR

avidaR is an R package for working with data sets generated by the
[Avida](http://avida.devosoft.org/) digital evolution platform.

# Installation

The latest and greatest version of avidaR (as well as older versions) can be
installed using
[devtools](http://cran.r-project.org/web/packages/devtools/index.html):


```r
install.packages("devtools")
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library(devtools)
```

```
## 
## Attaching package: 'devtools'
## 
## The following objects are masked from 'package:utils':
## 
##     ?, help
## 
## The following object is masked from 'package:base':
## 
##     system.file
```

```r
install_github("briandconnelly/avidaR")
```

```
## Installing github repo avidaR/master from briandconnelly
## Downloading master.zip from https://github.com/briandconnelly/avidaR/archive/master.zip
```

```
## Error: client error: (404) Not Found
```


You'll also need the `[stringr](https://github.com/hadley/stringr)` package:


```r
install.packages("stringr")
```

```
## Error: trying to use CRAN without setting a mirror
```


# Functions

You'll first need to load avidaR:


```r
library(avidaR)
```

```
## Error: there is no package called 'avidaR'
```


## `read.avida`

`read.avida` allows you to import Avida data files directly into R. The result
is a data frame. Column names for the data frame are extracted from the data
file.


```r
mydata <- read.avida("time.dat")
```

```
## Error: could not find function "read.avida"
```

```r
head(mydata)
```

```
## Error: object 'mydata' not found
```


Files compressed with bzip2 or gzip are automatically decompressed when loaded:


```r
mydata <- read.data("resource.dat.gz")
```

```
## Error: could not find function "read.data"
```
