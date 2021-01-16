#' Correlation plot from a data frame
#'
#' @description Correlation plot from a data frame, a wrapper around `corrplot::corrplot()`
#' @param df Data frame
#' @param ... Pass to `corrplot::corrplot()` argument
#' @param method_cor Character: Specify method for calculate
#' correlation; "pearson" (default), "spearman" or "kendall"
#' @param method_plot Character: Specify plotting output;
#' "custom" (default) other options are "circle", "square", "ellipse", "number", "shade",
#' "color", "pie" .
#' @param na.rm_type Character: If NA's is present in data frame,
#' it will be removed by "rows"(default): any rows that has NA's or "cols" any column that has NA's.
#' @param type Character: "upper" (default), "lower" or "full", display full matrix, lower triangular or upper triangular matrix.
#' @param tl.srt Numeric: For text label string rotation in degrees.
#' @param tl.col Character: Specify text color "black" (default)
#' @param sig.level Numeric: P-value significant level, default = 0.05.
#' @param insig Character: Character, specialized insignificant correlation coefficients,
#' "pch" (default), "p-value", "blank", "n", or "label_sig". If "blank", wipe away the corresponding glyphs;
#' if "p-value", add p-values the corresponding glyphs; if "pch", add characters (see pch for details) on corresponding glyphs;
#' if "n", don't take any measures; if "label_sig", mark significant correlations with pch (see sig.level).
#' @seealso `corrplot` package
#'  \url{https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html}
#'
#' @return Plot
#' @import purrr dplyr tidyselect
#' @importFrom magrittr %>%
#' @importFrom corrplot corrplot
#' @importFrom stats cor cor.test
#'
#'
#' @export
#'
#' @examples
#' # Plot correlation of numeric column
#' corrplot_df(iris)
#'
#' mtcars %>%
#'   corrplot_df(method_plot = "circle", # specify graphic method of correlation
#'              type = "lower",         # display lower half of correlation plot
#'              sig.level = 0.01,       # p-value = 0.01
#'              insig = "pch")          # insignificant p-value will be crossed out
#'
#'
corrplot_df <- function(df,                      # df = Dataframe
                        ... ,                    # ... pass to corrplot()
                        method_cor = "pearson",   # Pass to `cor.test()` arguments ; other options are "kendall", "spearman"
                        method_plot = "custom",   # "custom" for my custom plot; other option as `method` argument of corrplot()
                        na.rm_type = "rows",      # If NA present, "rows" = remove rows that has NA's; "cols" = remove columns that has NA's
                        type = "upper",           # Type of plot : "full" = show rectagular plot, "uppper" = show upper half (default), "lower" = show lower half
                        tl.srt=45,                # Text rolation: 45 degree
                        tl.col="black",           # Text color: back
                        sig.level = 0.05,         # p-value significant level: 0.05
                        insig = "blank"){         # If p-value is insignificant: "blank" (default); "p-value"-> add p-value; "pch" -> add character;"n" -> do nothing; "label_sig" -> mark significant

  require(magrittr)
  requireNamespace("corrplot")
  requireNamespace("stats")
  requireNamespace("tidyselect")
  requireNamespace("dplyr")
  requireNamespace("purrr")

  # cor.mtest <- function(mat, method_cor) {
  #   mat <- as.matrix(mat)
  #   n <- ncol(mat)
  #   p.mat<- matrix(NA, n, n)
  #   diag(p.mat) <- 0
  #   for (i in 1:(n - 1)) {
  #     for (j in (i + 1):n) {
  #       tmp <- stats::cor.test(mat[, i], mat[, j], method = method_cor) # *cor.test
  #       p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
  #     }
  #   }
  #   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  #   p.mat
  # }

  df.num <- df %>%
    dplyr::select(tidyselect::vars_select_helpers$where(is.numeric))   # Choose only numeric column


  na_col <- df.num %>% purrr::keep(~any(is.na(.x))) %>% length()

  na_row <- df.num %>%
    purrr::map_df(is.na) %>%
    dplyr::rowwise() %>%
    dplyr::transmute(miss = sum(dplyr::c_across(dplyr::everything()))) %>%
    dplyr::filter(miss > 0) %>%
    nrow()

  switch (na.rm_type,
          "cols" = if(na_col > 0) warning(" Removed ", na_col ," column(s) contained missing value(s)\n"),
          "rows" = if(na_row > 0) warning(" Removed ", na_row ," row(s) contained missing value(s)\n")
  )

  df.complete <- switch (na.rm_type,
                         "cols" = df.num %>% purrr::discard(~any(is.na(.x))),  # Remove column that has any NA's
                         "rows" = df.num %>% dplyr::filter(dplyr::across(dplyr::everything(), ~!is.na(.x))), # Remove rows that has any NA's
                         stop("invalid `na.rm_type` argument")
  )


  cor.mat <- df.complete %>% stats::cor(method = method_cor)

  p.mat <- df.complete %>% cor.mtest(method = method_cor)

  if(method_plot == "custom"){

    col <- grDevices::colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

    cor.mat %>%
      corrplot::corrplot(
        p.mat = p.mat,
        col = col(200),
        method = "color", addCoef.col = "#333333",
        type = type,
        tl.col= tl.col, tl.srt=tl.srt,
        sig.level = sig.level,
        insig = insig, ...  # In significant p-values leaves as blank
      )


  }else{

    cor.mat %>%
      corrplot::corrplot(
        p.mat = p.mat,
        method = method_plot, type = type,
        tl.srt=tl.srt, tl.col = tl.col,
        sig.level = sig.level,
        insig = insig , ...)
  }
}

#' Helper function for corrplot_df
#'
#' @param mat data frame only numeric column
#' @param method method for calc p-value
#'
#' @return matrix of p-value
#'
cor.mtest <- function(mat, method) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- stats::cor.test(mat[, i], mat[, j], method = method) # *cor.test
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
