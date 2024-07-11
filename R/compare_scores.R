
#' Curves of composition
#' @description Compare used feature distributions in two documents.
#'
#' @param stylo_res Input data. Expects results from a `stylo()` run. Function will yell if you pass something other than 'stylo.results' class.
#' @param source_text String. Filename of a document to select.
#' @param target_text String. A class (e.g. author) to select. In `stylo()` a class is defined by a string before the first underscore in a filename.
#' @param top_diff Integer. Will display top n different features between two texts. Provide a **negative** integer to, conversely, mark most similar ones.
#' @param type String. The flavor of visualization. Supports "profile" and "diff".
#' @return Visualizes top z-scores for used features in a selected.
#' @export
#'
#' @examples
#'\dontrun{
#' library(stylo)
#' library(seetrees)
#'
#' stylo_res <- stylo(gui=F) ## run if you can
#' compare_scores(stylo_res, source_text="Austen_Pride",target_text="CBronte_Jane",top_diff=10)
#'
#' ## (reproducible) example with a `stylo` embedded dataset
#'
#' data(lee) ## load the data
#' stylo_res <- stylo(gui=F,frequencies=lee)
#' compare_scores(stylo_res,source_text="HarperLee_Mockingbird_1960", target_text="Capote_Blood_1966",top_diff=10)
#' }

compare_scores <- function(stylo_res,
                           source_text=NULL,
                           target_text=NULL,
                           top_diff=10,
                           type="profile") {

  #' @import dplyr
  #' @import stringr
  #' @import ggplot2
  #' @import ggrepel

  inscribe=p=scoreA=scoreB=to_fill=text=value=xpos=ypos=NULL
  ## check if stylo.results are passed down
  if(!inherits(stylo_res,what = "stylo.results")) {
    stop("\nWrong input! Data should come from `stylo()` results!")
  }

  ## check if selection is not empty
  if(is.null(target_text) | is.null(source_text)) {
    stop("\nNo selection is provided! You need to select two sets of texts, or classes")
  }




  ## get labels
  lbls <- rownames(stylo_res$table.with.all.freqs)
  ## get classes
  classes <- stringr::str_extract(lbls,"^.*?(?=_)")
  ## get used features in the run
  used_features <- stylo_res$table.with.all.zscores[,stylo_res$features.actually.used]


  ## determine frequency ranks of used features from frequency table
  ffranks <- which(colnames(stylo_res$table.with.all.freqs) %in% stylo_res$features.actually.used)

  # ## make selection (if author is not provided, assume it's a single text)
  # if(is.null(target_class)) {
  #   target <- used_features[target_text,]
  # } else {
  #   ## if class is provided detect all rows associated with a class and average over features
  #   target <- used_features[str_detect(rownames(used_features),paste0("^", target_class)),] %>% colMeans()
  # }
  source <- used_features[source_text,]
  target <- used_features[target_text,]
  df_select <- tibble(scoreA=source,
                      scoreB=target,
                      rank=ffranks,
                      word=names(source),
                      textA=source_text,
                      textB=target_text) %>%
    mutate(diff=scoreA-scoreB)

  labels <- df_select %>% top_n(top_diff,abs(diff))

  df_long<-df_select %>%
    pivot_longer(cols = c("scoreA", "scoreB"),names_to = "text") %>%
    mutate(text=ifelse(text=="scoreA", source_text,target_text),
           text=factor(text,levels = c(source_text,target_text)))

### profile plot

if(type=="profile") {

    p <- df_long %>% ggplot(aes(x=rank)) +
    geom_line(aes(y=value,color=text),alpha=0.7) +
    geom_segment(data=labels,aes(y=scoreA,yend=scoreB),color="grey30",linetype=3) +
    scale_color_manual(values=c('pink',"lightblue")) +
    geom_point(data=labels %>% filter(diff < 0),aes(y=scoreB),color="lightblue",size=2) +
    geom_point(data=labels %>% filter(diff > 0),aes(y=scoreA),color="pink",size=2) +
    geom_text(data=labels %>% filter(diff < 0),aes(y=scoreB,label=word),color="grey30",size=3.5, nudge_y=0.1,hjust=1) +
    geom_text(data=labels %>% filter(diff > 0),aes(y=scoreA,label=word),color="grey30",size=3.5,nudge_y=0.1) +
    labs(x="Feature frequency rank",y="Standard deviation from the corpus mean") +
    theme_classic() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.key.size = unit(3,"line"),
          legend.text = element_text(size=12),
          # axis.line = element_blank(),
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5))

} else if (type=="diff") {
  supp_text <- tibble(inscribe=factor(c(paste0("more in ", source_text),
                                        paste0("more in ", target_text)),levels=c(paste0("more in ", source_text),paste0("more in ", target_text))),
                      ypos=c(1, -1),
                      xpos=-5)

  p<-df_select %>%
    mutate(to_fill=word %in% labels$word) %>%
    ggplot(aes(rank,diff)) +
    geom_col(aes(fill=to_fill)) +
    labs(y="Difference between z-scores",x="Feature frequency rank") +
    theme_classic() +
    geom_hline(aes(yintercept = 0)) +
    geom_text(data=supp_text,aes(xpos,ypos, color=inscribe,label=inscribe),angle=90,size=3.5) +
    guides(color="none",fill="none") +
    scale_color_manual(values=c("pink", "lightblue")) +
    scale_fill_manual(values=c("grey30", "red"))

  if(top_diff > 0 ) {

    p<- p + geom_text(data=labels %>% filter(diff > 0),aes(label=word),color="grey30",nudge_y = 0.1) +
      geom_text(data=labels %>% filter(diff < 0),aes(label=word),color="grey30",nudge_y = -0.1)


  } else {
    p <- p + geom_label_repel(data=labels %>% filter(diff > 0),aes(label=word),color="grey30",nudge_y = 0.1) +
      geom_label_repel(data=labels %>% filter(diff < 0),aes(label=word),color="grey30",nudge_y = -0.1)
  }


  }


### difference profile



  return(p)

}

