# avidaR

avidaR is an R package for working with data sets generated by the
[Avida](http://avida.devosoft.org/) digital evolution platform.

# Installation

The latest and greatest version of avidaR (as well as older versions) can be
installed using
[devtools](http://cran.r-project.org/web/packages/devtools/index.html):


```r
install.packages("devtools")
library(devtools)
install_github("briandconnelly/avidaR")
```


You'll also need the [stringr](https://github.com/hadley/stringr) package:


```r
install.packages("stringr")
```


# Functions

You'll first need to load avidaR:


```r
library(avidaR)
```


## read.avida

`read.avida` allows you to import Avida data files directly into R. The result
is a data frame. Column names for the data frame are extracted from the data
file. Characters in column names that aren't letters, numbers, underscore, or 
period are replaced by underscores.


```r
mydata <- read.avida("time.dat")
head(mydata)
```

```
##   update avida.time average.generation num_executed.
## 1      0      0.000              0.000            30
## 2    100      1.061              7.069           840
## 3    200      2.170             15.583          4140
## 4    300      3.274             23.414          9030
## 5    400      4.362             31.595         17040
## 6    500      5.430             39.635         27390
```


Files compressed with bzip2 or gzip are automatically decompressed when loaded:


```r
mydata <- read.avida("resource.dat.gz")
```

