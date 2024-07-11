
#' Looks into feature contribution (z-scores)
#' @description Compare a text or an author against corpus.
#'
#' @param stylo_res Input data. Expects results from a `stylo()` run. Function will yell if you pass something other than 'stylo.results' class.
#' @param target_text String. Filename of a document to select.
#' @param target_class String. A class (e.g. author) to select. In `stylo()` a class is defined by a string before the first underscore in a filename.
#' @param top Integer. Top distinctive features to consider
#' @return Visualizes top z-scores for used features in a selected.
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

view_scores <- function(stylo_res,
                        target_text=NULL,
                        target_class=NULL,
                        top=NULL) {

#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @importFrom stats reorder

  ## check if stylo.results are passed down
  if(!inherits(stylo_res,what = "stylo.results")) {
    stop("\nWrong input! Data should come from `stylo()` results!")
  }

  ## check if selection is not empty
  if(is.null(target_text) & is.null(target_class)) {
    stop("\nNo selection is provided! Select a document, or a class (author)")
  }

  ## check if selection is not conflicting
  if(!is.null(target_text) & !is.null(target_class)) {
    stop("\nError! Both document and class is provided. Settle on either of those, please.")
  }
  direction=score=NULL



  ## get labels
  lbls <- rownames(stylo_res$table.with.all.freqs)
  ## get classes
  classes <- stringr::str_extract(lbls,"^.*?(?=_)")
  ## get used features in the run
  used_features <- stylo_res$table.with.all.zscores[,stylo_res$features.actually.used]


  ## determine frequency ranks of used features from frequency table
  ffranks <- which(colnames(stylo_res$table.with.all.freqs) %in% stylo_res$features.actually.used)

  ## make selection (if author is not provided, assume it's a single text)
  if(is.null(target_class)) {
    target <- used_features[target_text,]
  } else {
    ## if class is provided detect all rows associated with a class and average over features
    target <- used_features[str_detect(rownames(used_features),paste0("^", target_class)),] %>% colMeans()
  }

  df_target <- tibble(score=target,word=names(target)) %>%
    mutate(rank=ffranks,
           direction=ifelse(score < 0, "avoided", "preferred"))

  ## if no top
  if(!is.null(top)) {
    df_select <- df_target %>% group_by(direction) %>% top_n(top,abs(score))
  } else {
    df_select <- df_target
    top=length(stylo_res$features.actually.used)
  }

  ## determine the label for plotting
  title_label <- if(is.null(target_class)) {
    target_text
  } else {
    target_class
  }


  word_label <- df_select %>% ungroup() %>% mutate(score=ifelse(score > 0, score <- -0.2,score <- 0.2))



  ## draw
  p <- df_select %>%
    ggplot(aes(score,reorder(word,score),fill=direction)) +
    geom_col() +
    ## text labels
    geom_text(data=word_label %>% filter(score < 0), aes(label=word),color="grey30",size=3.5,hjust=1,nudge_x = 0.1) +
    geom_text(data=word_label %>% filter(score > 0), aes(label=word),color="grey30",size=3.5,hjust=0,nudge_x = -0.1) +
    geom_text(aes(score/2,label=rank),color="white",size=3) +
    ## mean + SD lines
    geom_vline(xintercept=0,color="red",linetype=2) +
    geom_vline(xintercept=c(-1,1),color="grey",linetype=2) +
    geom_vline(xintercept=c(-2,2),color="lightgrey",linetype=2) +
    ### visuals / theme
    guides(fill="none") +
    scale_fill_manual(values=c("lightblue", "pink")) +
    labs(title=paste0("Top ", top, " z-scores in ", title_label),
         x="Standard deviation from the corpus mean",
         y=NULL) +
    theme_classic() +
    theme(axis.line = element_blank(),
          panel.grid.major.y = element_line(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5))

  return(p)

}

