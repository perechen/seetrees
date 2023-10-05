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
view_tree(stylo_res, k=2,right_margin=12) ## redraws a dendrogram based on distance matrix, cuts it to k groups, shows associated features 
```
Check `?view_tree()` for more details.

It should produce two plots (a dendrogram cut to groups , and lists of words associated with groups)
![dendro](https://i.imgur.com/YI7Ov1z.png)
![words](https://i.imgur.com/99zEklK.png)

**Note:** words associated with clusters are determined by calculating  correlation ratio $\eta^2$ of word frequency ($f$) across clusters ($c$) and documents ($d$). Then results are filtered by p-value (which might not make sense at all). Notation adopted from [Cafiero & Camps 2020](https://www.science.org/doi/full/10.1126/sciadv.aax5489#sec-4), implementation by `catdes()` from [FactoMineR](http://factominer.free.fr/)  
 
$$
\eta^2 = \frac{\sum_\nolimits{c} \sum_\nolimits{d}(f_{d,c}-\bar{f_c})^2}{\sum_\nolimits{c} \sum_\nolimits{d}(f_{d,c} - \bar{f})^2}
$$

 


Check `?view_tree()` for more details.
