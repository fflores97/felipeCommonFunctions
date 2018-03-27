#' Sample PCA plot for RNASeq
#' @description PCA plots of samples for DE analysis
#' @param expressionMatrix Data frame with expression data from a read.csv() function
#' @param numberOfPCs Integer value for number of principal components
#' @return List with two plots, variancePlot has cumulative and individual importance of each PC, and pcaGRid is a grid of PCA plots
#' @author Felipe Flores
#' @import tidyverse, GGally
#' @export
samplePCA<-function(expressionMatrix,numberOfPCs=2){
  if(!is.tibble(expressionMatrix)){
    expressionMatrix1 <- as_tibble(expressionMatrix,rownames = "Sample")
  } else{
    expressionMatrix1 <- expressionMatrix
  }
  pca <- expressionMatrix1 %>%
    nest() %>%
    mutate(pca = map(data, ~ stats::prcomp(.x %>% select(-Sample),
                                    center = TRUE, scale = TRUE)),
           pca_aug = map2(pca, data, ~broom::augment(.x, data = .y)))

  var_exp_df <- pca %>%
    unnest(.,pca_aug) %>%
    summarize_at(.vars = vars(contains("PC")), .funs = funs(var)) %>%
    gather(key = pc, value = variance) %>%
    mutate(var_exp = variance/sum(variance),cum_var_exp=cumsum(var_exp),pc = str_replace(pc, ".fitted", ""))

  importancePlot <- var_exp_df %>%
    dplyr::rename(Individual = var_exp,Cumulative = cum_var_exp)  %>%
    gather(key,value,Individual,Cumulative) %>%
  #   # gather(key,value,var_exp,cum_var_exp) %>%
    ggplot(aes(x=pc,y=value,group=key,colour=key))+
    geom_point()+
    geom_line()+
    labs(y="Percent of Variance",x="",title="Variance Explained by each PC")+
    theme_bw()

  pcaRaw <- prcomp(t(expressionMatrix))

  pcaDF <- as_data_frame(pcaRaw$x,rownames = "Sample")

  pcaPlot<-GGally::ggpairs(data=pcaDF,columns=1:numberOfPCs+1,mapping = aes(colour=Sample),upper=NULL,legend = c(2,2))+
    theme_bw()+
    labs(title="Principal Components")

  returnList<-list(variancePlot=importancePlot, pcaGrid=pcaPlot)
  # returnList<-list(pcaGrid=var_exp_df)
  return(returnList)
}
