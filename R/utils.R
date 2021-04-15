#' Source multiple R script from a directory
#'
#' @param path (character) Specify directory where .R script located
#' @param local passed to `source`
#' @param trace (logical) whether to display file names message
#' @param ... passed to `source`
#'
#' @return message (or none)
#' @export
#'
#' @examples # sourceDir("path/to/dir")
sourceDir <- function(path, local = knitr::knit_global(), trace = TRUE, ...) {

  R.script <- list.files(path, pattern = "[.][RrSsQq]$")
  if(length(R.script) == 0) stop("No .R file in that directory", call. = F)

  for (nm in R.script) {
    if(trace) cat(nm)
    source(file.path(path, nm), local = local,  ...)
    if(trace) cat(" ; ")
  }
}


# Custom ggsave for mac ---------------------------------------------------


#' My custom ggsave for macbook pro
#'
#' @param filename (charater) 	File name to create on disk.
#' @param plot `last_plot()` passed to ggsave
#' @param path Path of the directory to save plot to: path and filename are combined to create the fully qualified file name. Defaults to the working directory.
#' @param width Plot size in units ("in", "cm", or "mm").
#' @param height Plot size in units ("in", "cm", or "mm").
#' @param units Plot size in units ("in", "cm", or "mm").
#' @param dpi Plot resolution. Also accepts a string input: "retina" (320), "print" (300), or "screen" (72). Applies only to raster output types.
#' @param scale Multiplicative scaling factor.
#' @param device Device to use. Can either be a device function (e.g. png()), or one of "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' @param ... passed to `ggsave`
#' @export
#'
#' @examples # ggsave_mac("path/to/plot.png")
ggsave_mac <- function(filename,
                       plot = last_plot(), path = NULL,
                       width = 25.6, height = 16, units = "cm",
                       dpi = "retina",
                       scale = 1,
                       device = NULL, ...) {

  ggplot2::ggsave(filename, plot, path = path, width = width, height = height,
                  units = units, dpi = dpi, scale = scale, device = device, ...)
}

# Save multiple plots by ggsave_mac ---------------------------------------------------


#' My custom ggsave for macbook pro (Multi-plots)
#'
#' @param plot_objs plot objects
#' @param path Path of the directory to save plot to: path and filename are combined to create the fully qualified file name. Defaults to the working directory.
#' @param width Plot size in units ("in", "cm", or "mm").
#' @param height Plot size in units ("in", "cm", or "mm").
#' @param units Plot size in units ("in", "cm", or "mm").
#' @param dpi Plot resolution. Also accepts a string input: "retina" (320), "print" (300), or "screen" (72). Applies only to raster output types.
#' @param scale Multiplicative scaling factor.
#' @param device Device to use. Can either be a device function (e.g. png()), or one of "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' @param ... passed to `ggsave`
#'
#' @return print plots and save to files
#' @export
#'
#' @examples # ggsave_mac_multi(plot_objs, c("plot_1.png","plot_2.png"))
ggsave_mac_multi <- function(plot_objs, # plot objects
                             paths,     # Output paths
                             width = 25.6, height = 16, units = "cm",
                             dpi = "retina",
                             scale = 1,
                             device = NULL,
                             ...) {     # to ggsave_mac

  for (i in seq_along(plot_objs)) {

    print(plot_objs[[i]])
    ggsave_mac(paths[[i]], width = width, height = height, units = units,
               dpi = dpi,
               scale = scale,
               device = device,
               ...)
    cat(basename(paths[[i]]), "\n")

  }

}
