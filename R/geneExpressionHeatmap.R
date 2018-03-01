#' Creating Gene Expression Heatmaps
#'
#' @description Beautiful heatmap creator
#' @param expressionMatrix Matrix with genes as rows and samples as columns
#' @param genesOfInterest Vector of genes to be put on a heatmap
#' @param samples (optional) Indexing vector if only want to visualize a subset of samples. Can also be the sample names
#' @param annotationDataFrame  Data frame with categories (clusters, groups, etc) that can be used to annotate a heatmap. Rows should be sample names and each column represents an annotation
#' @param kMeans (optional) Number of k-means used to cluster rows
#' @param fontSize Global font size
#' @param title (optional) Heatmap Title
#' @return hm, a heatmap object
#' @author Felipe Flores
#' @export

geneExpressionHeatmap<- function(expressionMatrix,genesOfInterest,samples,annotationDataFrame,clusterColumns=T,clusterRows=T,fontSize,kMeans=1,title=character(0),...){
  if(missing(samples)){
    mat<-minimalSet[genesOfInterest,]
  } else{
    mat<-expressionMatrix[genesOfInterest,samples]
  }
  base_mean<-rowMeans(mat)
  mat_scaled<-t(apply(mat,1,scale))
  type<- sapply(annotationDataFrame[samples,],function(x) unique(x))
  #type<-as.data.frame(type)
  #names(type)<-rownames(annotationDataFrame[samples,])
  if(class(type)=="list"){
    colors<-list()
    for(i in 1:length(type)){
      colors[[i]]<-rainbow(length(unique(type[[i]])))
      names(colors[[i]])<-unique(type[[i]])
    }
    colors<-setNames(colors,names(type))
    ha = ComplexHeatmap::HeatmapAnnotation(df = annotationDataFrame,col=colors)
  } else {
    colors<-rainbow(length(unique(type)))
    names(colors)<-as.character(unique(type))
    ha = HeatmapAnnotation(df = annotationDataFrame,col=list(type=colors))
  }

  ht_global_opt(heatmap_row_names_gp = gpar(fontface = "italic",fontsize=fontSize))

  hm<-Heatmap(mat_scaled, name = "expression", col = colorRamp2(c(-2, 0, 2), c("blue", "white", "red")),
              top_annotation = ha, km=kMeans, top_annotation_height = unit(4, "mm"),
              show_row_names = T, cluster_rows = clusterRows,show_column_names = FALSE,row_names_side = 'left',show_column_dend = T,cluster_columns = clusterColumns,column_title = title,...) +
    Heatmap(base_mean, name = "base_mean", show_row_names = FALSE, width = unit(5, "mm"))
  return(hm)
}
