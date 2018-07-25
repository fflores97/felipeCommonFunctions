#' Metacore Pathway Plotter
#'
#' @param metacore_files Files with pathways from metacore in xlsx format. They originally come in xls format but this won't work. Must convert by hand with excel
#' @param titles Vector of plot titles. Example: c("Plot 1", "Plot 2")
#' @param outputnames Vector of names passed to ggsave. Example: c("plot1.pdf", "plot2.pdf")
#'
#' @return Saves plots as pdfs
#' @export
metacore_pathway_FDR_plot <- function(metacore_files, titles, outputnames, palette) {
  for (i in 1:length(metacore_files)) {
    file <- metacore_files[i]
    # palette <- RColorBrewer::brewer.pal(n = length(metacore_files), name = "Set3")

    data <- readxl::read_excel(file, skip = 2) %>%
      select(Maps, FDR) %>%
      na.omit() %>%
      mutate(logFDR = -1*log10(FDR))

    plot <- data %>%
      ggplot(aes(x = reorder(Maps, logFDR), y = logFDR, fill=palette[i]))+
      geom_col(col = "black")+
      coord_flip()+
      theme_classic()+
      scale_fill_manual(values = palette[i])+
      guides(fill = FALSE)+
      labs(x = "Pathway", y = "- Log10(FDR)", title = titles[i])+
      theme(
        axis.text.y = element_text(size = 20, margin = margin(l = 25)),
        axis.text.x = element_text(size = 20),
        axis.title = element_text(size = 25),
        plot.title = element_text(size = 25, face = "bold"))
    ggsave(filename = outputnames[i], plot = plot, width = 30, height = 7, units = "in")
  }
}
