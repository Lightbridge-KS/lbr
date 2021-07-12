#' Count Variables in a data.frame
#'
#' Count all or many variables in a data.frame and display the results as list of data.frame.
#'
#' @param x A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... Variables to count (one level); If not supply, count all variables.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param name The name of the new column in the output.
#'
#' @return A named list of data.frame
#' @export
#'
#' @examples
#' # Count All variables
#' count_vars(iris)
#' # Count Specified variables
#' count_vars(iris, Species, Sepal.Length)
count_vars <- function(x, ... ,sort = TRUE, name = NULL) {

  dot <- rlang::enexprs(...)

  vars_chr <- if(!rlang::is_empty(dot)){
    # If supply ... as variable
    chr <- purrr::map_chr(dot, rlang::as_string)
    stats::setNames(chr, chr)
  }else{
    # If not supply ... count all varibles in data.frame
    stats::setNames(names(x), names(x))
  }

  vars_chr %>% purrr::map(
    ~count(x, dplyr::across(.x), sort = sort, name = name)
  )
}
