#' Generic Venn Diagram
#'
#' @description Quick and Dirty Venn Diagram
#' @param listOfCohorts Sets to compare as a named list. Names will be used to label the diagram
#' @param filename To be saved
#' @param title Title of Diagram
#' @author Felipe Flores
#' @return Sorted vector of elements that are different
vennDiagrams<-function(listOfCohorts,fileName=NULL,title=NULL){
  vennPlot <- VennDiagram::venn.diagram(listOfCohorts, filename = fileName, height = 10,
                                        width = 10, units = 'in', resolution = 300, fill = c('blue', 'red'),
                                        imagetype='pdf', alpha = c(0.2, 0.2), cex = 1.5, cat.cex = 1.5,
                                        fontfamily='arial',cat.fontface = 'bold', cat.pos = c(180,180), cat.dist = c(0.02, 0.02),
                                        main = title, main.cex = 2, main.pos = c(0.5,1.05), main.fontface = 'bold')
  return(vennPlot)
}
