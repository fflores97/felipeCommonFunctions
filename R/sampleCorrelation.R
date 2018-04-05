# #
# countsData <- read.csv("~/Downloads/shNLGN4X-combined-gene-level-counts.csv",header = TRUE,row.names = 1,stringsAsFactors = F)
# countsData <- countsData[rowSums(countsData) > 10,]
# countsData <- countsData[,order(colnames(countsData))]
# # countsData <- countsData[1:200,]
# metaData<-read.csv("~/Downloads/infoTable.csv", header = TRUE,row.names = 1)
# # dds <- DESeqDataSetFromMatrix(countData = countsData,colData=metaData,design = ~ Treatment)
# # dds <- estimateSizeFactors(dds)
# # vsd <- varianceStabilizingTransformation(dds, blind=FALSE)
# # normalizedTableVSD <- assay(vsd)
# # normalizedTableVSD <- normalizedTableVSD[,sort(colnames(normalizedTableVSD))]
# # # normalizedCountsTable <- as_data_frame(counts(dds, normalized = TRUE),rownames = "Genes")
# normalizedCountsTable <- counts(dds, normalized = TRUE)
#

#' Sample Correlation Plot
#' @description Function that creates a correlation plot with circles colored and sized based on correlation on the lower triangle and correlation values on the upper triangle
#' @param normalizedCountsTable Numeric matrix with genes as rows and samples as columns. Ideally variance-stabilized and normalized by DESeq methods
#'
#' @return Complex heatmap object
#' @export
#' @import circlize ComplexHeatmap
#' @examples
#'

sampleCorrelation <- function(normalizedCountsTable){

  # Compute correlation through Pearson
  correlationMatrix <- stats::cor(normalizedCountsTable)

  # Order based on clustering (euclidean by default)
  order <-  stats::hclust(dist(correlationMatrix))$order
  correlationMatrix <- correlationMatrix[order, order]

  # The function scales correlation colors to existing values
  coloringFunction = circlize::colorRamp2(c(min(correlationMatrix), max(correlationMatrix)), c("blue", "red"))

  #
  sampleCorrelationPlot <-
    ComplexHeatmap::Heatmap(
      matrix = correlationMatrix,
      name = "correlation",
      col = coloringFunction,
      rect_gp = gpar(type = "none"),
      cell_fun = function(j, i, x, y, width, height, fill) {
        grid.rect(
          x = x,
          y = y,
          width = width,
          height = height,
          gp = gpar(col = "grey", fill = NA)
        )

        # This line constructs the diagonal based on sample names
        if (i == j) {
          grid.text(rownames(correlationMatrix)[i], x = x, y = y)
        }

        # Construct lower triangle
        else if (i > j) {
          grid.circle(
            x = x,
            y = y,
            r = abs(correlationMatrix[i, j]) / 2 * min(unit.c(width, height)),
            gp = gpar(fill = coloringFunction(correlationMatrix[i, j]), col = NA)
          )
        }

        # Construct upper triangle
        else {
          grid.text(sprintf("%.3f", correlationMatrix[i, j]), x, y, gp = gpar(fontsize = 10))
        }
      },
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      show_row_names = FALSE,
      show_column_names = FALSE,
      column_title = "Pearson Correlation Between Samples",
      column_title_gp = gpar(fontface="bold",fontsize=14)
    )

  return(sampleCorrelationPlot)

}

