% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_scores.R
\name{compare_scores}
\alias{compare_scores}
\title{Curves of composition}
\usage{
compare_scores(
  stylo_res,
  source_text = NULL,
  target_text = NULL,
  top_diff = 10,
  type = "profile"
)
}
\arguments{
\item{stylo_res}{Input data. Expects results from a `stylo()` run. Function will yell if you pass something other than 'stylo.results' class.}

\item{source_text}{String. Filename of a document to select.}

\item{target_text}{String. A class (e.g. author) to select. In `stylo()` a class is defined by a string before the first underscore in a filename.}

\item{top_diff}{Integer. Will display top n different features between two texts. Provide a **negative** integer to, conversely, mark most similar ones.}

\item{type}{String. The flavor of visualization. Supports "profile" and "diff".}
}
\value{
Visualizes top z-scores for used features in a selected.
}
\description{
Compare used feature distributions in two documents.
}
\examples{
\dontrun{
library(stylo)
library(seetrees)

stylo_res <- stylo(gui=F) ## run if you can
compare_scores(stylo_res, source_text="Austen_Pride",target_text="CBronte_Jane",top_diff=10)

## (reproducible) example with a `stylo` embedded dataset

data(lee) ## load the data
stylo_res <- stylo(gui=F,frequencies=lee)
compare_scores(stylo_res,source_text="HarperLee_Mockingbird_1960", target_text="Capote_Blood_1966",top_diff=10)
}
}
