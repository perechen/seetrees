# seetrees

Package that extends some `stylo` capabilities and enhances interpretation of (unsupervised clustering) analysis.

## Installation

Install from GitHub (make sure you have `devtools` package):

```r
devtools::install_github("perechen/seetrees")
```

## Example

```r
library(stylo)
library(seetrees)

data(lee) ## load one of the stylo datasets

stylo_res <- stylo(frequencies=lee,gui=F)
view_tree(stylo_res, k=2) ## redraws a dendrogram based on distance matrix, cuts it to k groups, shows associated features 
```
