
#' Looks into feature contribution (z-scores)
#' @description Compare a text or an author against corpus.
#'
#' @param stylo_res Input data. Expects results from a `stylo()` run. Function will yell if you pass something other than 'stylo.results' class.
#' @param pattern String. Regex to extract class from the filename
#' @param author String. A class (e.g. author) to visualize intra-class distances.
#' @param group Logical. If TRUE, plots density distribution for intra- and iter-class distances separately.
#' @return Visualizes distance distribution.
#' @export
#'
#' @examples
#'\dontrun{
#' library(stylo)
#' library(seetrees)
#'
#' stylo_res <- stylo(gui=F) ## run if you can
#' view_scores(stylo_res, target_text="CBronte_Jane")
#'
#' ## (reproducible) example with a `stylo` embedded dataset
#'
#' data(lee) ## load the data
#' stylo_res <- stylo(gui=F,frequencies=lee)
#' view_scores(stylo_res, target_text="Faulkner_Absalom_1936")
#' }

view_distances <- function(stylo_res,
                           pattern="^.*?(?=_)",
                           group=TRUE,
                           author=NA) {

#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @importFrom stats median

  same_author=NULL
  ## check if stylo.results are passed down
  if(!inherits(stylo_res,what = "stylo.results")) {
    stop("\nWrong input! Data should come from `stylo()` results!")
  }


  ## get distance table

  d <- stylo_res$distance.table


  ## get classes
  classes <- stringr::str_extract(rownames(d),"^.*?(?=_)")
  ## check for classes
  if(!is.na(author) & !author %in% classes) {
    stop("\nSorry! The class you provided is not in the data!")
  } else {

  }


  ## lower triangle positions
  lower <- lower.tri(d,diag = F)

  ## copy names
  rownames(lower) <- rownames(d)
  colnames(lower) <- rownames(d)

  matching_matrix <- matrix(NA, nrow = nrow(d), ncol = ncol(d))
  names_matrix <- matrix(NA, nrow = nrow(d), ncol = ncol(d))

  ## check if distance is between the same/different authors

  ## for each row
  for (i in 1:nrow(d)) {
    ## for each column
    for (j in 1:ncol(d)) {
      ## if strings are equal, annotate as TRUE
      matching_matrix[i, j] <- ifelse(str_extract(rownames(d)[i],pattern)==str_extract(colnames(d)[j],pattern), T, F)

      names_matrix[i, j] <- ifelse(str_extract(rownames(d)[i],pattern)==str_extract(colnames(d)[j],pattern), str_extract(colnames(d)[j],pattern), NA)
  }
  }


  df <- tibble(d=d[lower],
               same_author=matching_matrix[lower],
               class=names_matrix[lower])

  meds <- df %>% group_by(same_author) %>% summarise(d=median(d))
  med <- df %>% summarise(d=median(d))


  if(group) {

  p <- df %>% ggplot(aes(d)) +
    geom_density(aes(fill=same_author), alpha=0.5) +
    theme_classic() +
    geom_vline(data=meds,aes(xintercept=d),linetype=2,color="white",size=1) +
    scale_x_continuous(expand = c(0,0.05),limits = c(0,NA)) +
    scale_y_continuous(expand = c(0,0.05)) +
    scale_fill_manual(values=c("lightblue","pink"),breaks = c("TRUE","FALSE")) +
      theme(legend.position = "top") +
      labs(x="Distance",y="Density")


  } else {

   p <- df %>% ggplot(aes(d)) + geom_density(fill="grey80",alpha=0.5) +
      theme_classic() +
      geom_vline(data=med,aes(xintercept=d),linetype=2,color="white",size=1) +
      scale_x_continuous(expand = c(0,0.05),limits = c(0,NA)) +
      scale_y_continuous(expand = c(0,0.05))  +
      labs(x="Distance",y="Density")


  }

  if(!is.na(author)) {

    d_intra <- df %>% filter(class==author)



    p<-p + geom_point(data=d_intra,aes(x=d,y=0),size=4) + labs(x="Distance",y="Density",subtitle=paste0("Points: distances between works of ", author))



  }

  return(p)

}

