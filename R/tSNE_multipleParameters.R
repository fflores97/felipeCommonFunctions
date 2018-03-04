#' Multiple-parameter tSNE Run
#'
#' @description Automated tSNE runs through a set of parameters specified by the user
#' @param matrix Matrix on which the runs of tSNE will be performed. Should have samples as columns
#' @param perplexities Vector of the perplexities to attempt
#' @param thetas Vector of the thetas to try
#' @param iterations Max Number of iterations (defaults to 800)
#' @param clusterDF Cluster data frame. Rows should be sample names and cells should have the name of the cluster for each sample
#' @return
#' @export
#' @author Felipe Flores
#' @import Rtsne
tSNE_multipleParameters<-function(matrix, perplexities,thetas,iterations,clusterDF,filename='./plots/dimension-reduction/tSNE.pdf'){
  pdf(file = filename,width=7,height=7)
  tsneParameters<-list(perplexities,thetas)
  for(ii in 1:length(tsneParameters[[1]])){
    for(j in 1:length(tsneParameters[[2]])){
      rtsne_out <- Rtsne::Rtsne(t(matrix),dims=2,max_iter = iterations,pca = T,perplexity = tsneParameters[[1]][ii],theta=tsneParameters[[2]][j])
      tsneOriginal<-as_tibble(rtsne_out$Y) %>%
        rename("tSNE1"=!!names(.[1]),"tSNE2"=!!names(.[2])) %>%
        mutate("d"=colnames(matrix))
      if(missing(clusterDF)){
        tsnePlot<- ggplot(tsneOriginal, aes(x=tSNE1, y=tSNE2))
      } else {
        if(!is.tibble(clusterDF)) clusterDF<-as_tibble(rownames_to_column(clusterDF))
        clusterDF %<>%
          rename("d"=!!names(.[1]),"cluster"=!!names(.[2]))
        tsneOriginal %<>% right_join(clusterDF)
        tsnePlot<- ggplot(tsneOriginal, aes(x=tSNE1, y=tSNE2,color=cluster))
      }
      tsnePlot <- tsnePlot + geom_point(size=2) + theme_bw() +
        labs(caption=paste("perplexity = ",tsneParameters[[1]][ii],", theta =",tsneParameters[[2]][j]),sep="")
      print(tsnePlot)
    }
  }
  dev.off()
}
