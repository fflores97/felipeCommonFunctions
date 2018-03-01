#' Find elements that are not in common
#'
#' @description Gaussian Mixture Model for gene expression distributions
#' @param expressionMatrix Numeric Matrix with genes as rows and samples as columns
#' @param genesOfInterest Character vector listing the genes you're interested in looking at
#' @param k Number of Gaussians to be used. This mixture model uses as many distributions as specified
#' @author Felipe Flores
#' @return List with posterior probabilitiy data frames
gaussianMixtureModel<-function(expressionMatrix,genesOfInterest,k){
  if(!dir.exists('plots/gaussian-mixture-model')) {
    dir.create('plots/gaussian-mixture-model',recursive=T)
  }
  posteriorProbabilities<-list()
  set.seed(1)
  for (i in genesOfInterest){
    mixmdl <- mixtools::normalmixEM(expressionMatrix[i,], k = numberOfGaussians)
    data.frame(x = mixmdl$x) %>%
      ggplot2::ggplot() +
      geom_histogram(aes(x, ..density..), binwidth = 0.3, colour = "black",
                     fill = "white") +
      stat_function(geom = "line", fun = plot_mix_comps,
                    args = list(mixmdl$mu[1], mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                    colour = "red", lwd = 1.5) +
      stat_function(geom = "line", fun = plot_mix_comps,
                    args = list(mixmdl$mu[2], mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                    colour = "blue", lwd = 1.5) +
      ylab("Density") +
      xlab("Expression Level") +
      labs(title= paste(rownames(receptorExpression)[i],"Expression Distribution"))
    ggsave(paste('./plots/gaussian-mixture-model/Distribution_',i,'.pdf', sep=""),plot=last_plot())
    post.df <- as.data.frame(cbind(x = mixmdl$x, mixmdl$posterior))
    posteriorProbabilities[[i]]<-post.df
  }
}
