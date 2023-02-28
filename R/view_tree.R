
#' Cut the trees, see the words!
#' @description A bundled function that takes an input from stylo() run, (re)draws a dendrogram (Ward's linkage, stylo's default), cuts it to an arbitrary numbers of groups and finds features that are associated with them.
#'
#' @param stylo_res Input data. Results from a stylo() run. Function will yell if you pass something other than 'stylo.results' class.
#' @param k Cuts a tree to a specified number of groups. Unexpected behavior possible if *k* > 5.
#' @param p p-value threshold for feature-to-cluster correlations. All features with p-value > p are discarded. Feel free to increase/decrease 0.05 cut-off.
#' @param color_leaves TRUE/FALSE. Enable coloring leaves by class (following `stylo` logic, a class is defined by a string before the first underscore in a filename)
#' @param output TRUE/FALSE. Enable .txt output with cluster information.
#' @param label_size Numeric. Controls the size of features on 'list' plots.
#' @param leaf_size Numeric. Controls the size of leaf labels in dendrogram.
#' @param right_margin Numeric. Controls the right margin of the dendrogram (use it if leaf labels do not fit). Default is 2
#'
#' @return Draws two plots and saves an optional .txt file
#' @export
#'
#' @examples
#'\dontrun{
#' library(stylo)
#' library(seetrees)
#'
#' stylo_res <- stylo(gui=F) ## run if you can
#' view_tree(stylo_res, k=2) ## rebuilds a tree, cuts to k=2, calculates correlations
#'
#' ## (reproducible) example with stylo dataset frequencies
#'
#' data(lee) ## load stylo dataset
#' stylo_res <- stylo(gui=F,frequencies=lee)
#' view_tree(stylo_res)
#' }
view_tree <- function(stylo_res,
                      k=2,
                      p=0.05,
                      color_leaves=F,
                      output=T,
                      label_size=6,
                      leaf_size=1,
                      right_margin=2) {

#' @import tidyr
#' @import dplyr
#' @import stringr
#' @import FactoMineR
#' @import dendextend
#' @import ggplot2



if(class(stylo_res) != "stylo.results") {
  stop("\nWrong input! Data should come from `stylo()` results!")
}



## get labels
lbls <- rownames(stylo_res$table.with.all.freqs)
## get classes
classes <- stringr::str_extract(lbls,"^.*?(?=_)")


## get palette

if(color_leaves) {
  ## paletteer's basetheme::minimal
clrs <- c("#5DA5DAFF", "#FAA43AFF", "#60BD68FF", "#F15854FF", "#B276B2FF", "#8D4B08FF", "#DECF3FFF", "#F17CB0FF", "#66E3D9FF", "#00FF7FFF")

## expand palette if needed
if(length(classes) > length(clrs)) {
  multiplier <- ceiling(length(classes)/length(clrs))
    clrs <- rep(clrs, multiplier)}

} else {
  clrs <- "black"
} ## end of if color statement

## get cluster palette
## paletteer's Polychrome::dark
clust_clrs <- c("#2E91E5FF", "#E15F99FF", "#1CA71CFF", "#FB0D0DFF", "#DA16FFFF", "#222A2AFF", "#B68100FF", "#750D86FF", '#EB663BFF', "#511CFBFF", "#00A08BFF", "#FB00D1FF", "#FC0080FF", "#B2828DFF", "#6C7C32FF", "#778AAEFF", "#862A16FF", "#A777F1FF", "#620042FF", "#1616A7FF", "#DA60CAFF", "#6C4516FF", "#0D2A63FF", "#AF0038FF")

## expand cluster palette if needed
if(length(k) > length(clust_clrs)) {
  multiplier <- ceiling(length(k)/length(clust_clrs))
    clust_clrs <- rep(clust_clrs, multiplier)
}



## assign colors to classes
tdf <- tibble(class=unique(classes)) %>%
  mutate(class_id=row_number(),
         colors=clrs[1:length(unique(classes))])

## hierarchical clustering with Ward's linkage (the method that stylo uses)
tr <- hclust(as.dist(stylo_res$distance.table),method = "ward.D2") %>%
  as.dendrogram()

## cut tree
tr_cut <- dendextend::cutree(tr,
                             k=k,
                             order_clusters_as_data=FALSE) ## this is crucial to be consistent with rect.dendrogram() that draws around clusters.
k_df <- tibble(k_id = tr_cut,label=names(tr_cut))

## match dendro labels with colors
meta <- tibble(label=labels(tr))  %>%
  mutate(class=str_extract(label,"^.*?(?=_)")) %>%
  left_join(tdf,by="class") %>%
  left_join(k_df,by="label")

## color labels
# v <- rep(NA,9)
# v[c(4,3,8,6,9)] <- 19
# vs <- v
# vs[c(4,3,8,6,9)] <- 3

tr <- tr %>%
  set("labels_col", value=meta$colors) %>% # label colors
  set("labels_cex", leaf_size)
#  set("nodes_pch",v) %>% # draw needed nodes
#  set("nodes_cex",vs)# size of nodes


## plotting
par(mfrow=c(1,1),mar = c(2,2,2,right_margin))
tr %>% plot(horiz=T,main=paste("Hierarchical clustering,"," cut at k=",k))
tr %>% rect.dendrogram(horiz = TRUE, border = clust_clrs,lty=5,lwd=3,k=k)

## subset frequencies table by features used
t <- stylo_res$table.with.all.freqs[,stylo_res$features.actually.used]

k_sort <- meta %>% arrange(label) %>% pull(k_id)
t <- bind_cols(tibble(key1k_=as.character(k_sort)),as_tibble(t))

corr <- FactoMineR::catdes(t, num.var = 1,proba = p)

k_names <- names(corr$quanti)

dfs <- lapply(corr$quanti, function(x) {
  f <- rownames(x)
  tb <- as_tibble(x) %>%
    mutate(feature=f)

  if(length(tb) == 0) {
    tb = tibble(v.test=1,
                `Mean in category`=NA,
                `Overall mean`=NA,
                `sd in category`=NA,
                `Overall sd`=NA,
                p.value=NA,
                feature="NO_FEATURE_AT_P_THRESHOLD")
  }
  return(tb)

})
n <- sapply(dfs,nrow)


f_df <- bind_rows(dfs) %>%mutate(k_id=rep(k_names,n)) %>%
  group_by(k_id) %>%
  mutate(rank=row_number()) %>%
  ungroup()

suppressWarnings(
fplot <- f_df %>% filter(v.test>0) %>%
  ggplot(aes(k_id, -rank)) +
  geom_text(aes(label=feature,color=k_id),size=label_size,hjust=0) +
  scale_size(range=c(8,15)) +
  scale_x_discrete(position="top",labels=paste("Cluster", k_names, "\n")) +
  scale_color_manual(values=clust_clrs) +
  theme_void() +
  guides(size="none", color="none") +
  theme(axis.text.x = element_text(size=24,color = clust_clrs[1:k]))
)

print(fplot)


## prepare collapsed feature-per-vector representation
output_df <- bind_rows(dfs) %>%mutate(k_id=rep(k_names,n)) %>%
  group_by(k_id) %>%
  mutate(rank=row_number(),
         k_id=as.integer(k_id)) %>%
  ungroup() %>% filter(v.test>0) %>%
  select(k_id,v.test,rank,feature) %>%
  group_by(k_id) %>%
  summarize(feature=paste(feature,collapse = " "),.groups="keep")

report <- meta %>%
  left_join(output_df,by="k_id")  %>%
  group_by(feature,k_id) %>%
  summarize(label=paste(label,collapse="\n"),.groups="keep") %>%
  arrange(k_id)

if(output) {

filename <- paste0("words_behind_trees_k",k,"_p",str_remove(as.character(p),"\\."), ".txt")
file.create(filename,overwrite=T)
for(i in 1:nrow(report)) {

  txt <- paste0("CLUSTER ",report$k_id[i], "\n==============\nTEXTS\n",report$label[i], "\n==============\nFEATURES associated (p<",p,")\n\n", report$feature[i], "\n\n\n\n")

  write(txt,file = filename,append = T)

}

}

}
