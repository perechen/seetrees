# seetrees

Package that extends some `stylo` capabilities and enhances interpretation of (unsupervised clustering) analysis. Includes few convenience functions for teaching and demonstration purposes.

## Installation

Install from GitHub (make sure you have `devtools` package):

```r
devtools::install_github("perechen/seetrees")
```

## view_tree()

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

 ## view_scores()

 Simple visualisation of most distinctive features in a text, or a class (an author). It uses corpus-wide scaled (z-scored) feature frequencies, and returns deviations (in both directions) of the top $n$ features. Quick and dirty way to ask "What is going on in here?".  

 ```r
library(stylo)
library(seetrees)

data(lee) ## load one of the stylo datasets

stylo_res <- stylo(frequencies=lee,gui=F)
# ask for 20 (positive and negative) features in Faulkner's "Absalom! Absalom!" that deviate from the corpus mean the most 
view_scores(stylo_res, target_text="Faulkner_Absalom_1936",top=20) 
```

Returns a column plot that shows preferred (pink) and avoided (lightblue) words. **NB** Numbers on columns indicate the feature's corpus-wide frequency rank. Dashed lines mark the mean, +-1 and +-2 SD.  

Check `?view_scores()` for more details.


![absalom](https://i.imgur.com/7WuRqdM.png)

## compare_scores()

Compares two documents based on used features in `stylo()`. Draws z-scores profile, or difference profile with the option to annotate largest differences

 ```r
library(stylo)
library(seetrees)

data(lee) ## load one of the stylo datasets

stylo_res <- stylo(frequencies=lee,gui=F)
# compare "To Kill a Mocking Bird" and "In Cold Blood", annotate 10 features that behave most differently
compare_scores(stylo_res,
			   source_text="HarperLee_Mockingbird_1960",
			   target_text="Capote_Blood_1966",
			   top_diff=10,
			   type="profile")

```

![profile](https://i.imgur.com/NOVHf5A.png)  

Also supports `type="diff"` flavor of visualisation (profile of differences)

![diff](https://i.imgur.com/hwTQgIk.png)


