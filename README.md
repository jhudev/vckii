
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

Simple error detection and version control for the importing, exporting,
and transfering of data using CRC32 checksum. A hash file is generated
for each dataset containing the summary hashes for all columns and rows
along with col/row names. Each file is created before exporting a
dataset and compared to hashes after importing to ensure consistency in
the datasets between working sessions of each user. In addition, each
file contains hashes organized by row and column which can be used for
easy debugging of potential errors.

Tutorial/Documentation:

  - Host: <http://j-huang.com/resources/tutorial/vckii>
  - Open Science Framework(OSF): <https://osf.io/xgd7q/>

## 1\. Installing package

First, install the R package “vckii”. The package can also be download
from the following repositories:

  - Host: <http://j-huang.com/resources/pk/vckii.tar.gz>
  - Github: <https://github.com/jhudev/vckii>
  - Open Science Framework(OSF): <https://osf.io/xgd7q/>

<!-- end list -->

``` r
#Install R package from host site
install.packages("http://j-huang.com/resources/pk/vckii.tar.gz")

# Or the development version from GitHub
# install.packages("devtools")
devtools::install_github("jhudev/vckii")

# Source: https://osf.io/xgd7q/
```

## 2\. Generating a hash file and data verification

The two primary functions are makeHash() and checkHash().

Exporting: The makeHash() function is used to generate a hash file and
should be used during the data exporting process. The hash file is a
simple text file that contains a set of hashes used for data
verification and debugging any issues during the importing process. The
structure of the file is detailed in the following section (Section 3).
Therefore, this file should be provided along with the exported data for
verification by the receiver.

Importing: The checkHash() function is used to verify a dataset after
importing into the working environment by comparing it with its
corresponding hash file. It will return either TRUE, if all checksums
match those in the hash file; otherwise, it will return FALSE. If FALSE,
the hash file can be used to determine the source of any inconsistencies
and assist in the debugging of the importing process.

A simplified example of the process is provided below.

``` r
#Step 1: User 1 exporting dataset “iris”
makeHash(iris, “iris_hash.txt”)
write.csv(iris, “iris.csv”)

#Step 2: Transferring data and hash files from User 1 to User 2

#Step 3: User 2 importing dataset “iris.csv” and validating with “iris_hash.txt”
iris_2 <- read.csv(“iris.csv”)
checkHash(iris_2, “iris_hash.txt”)
#Output >> TRUE
```
