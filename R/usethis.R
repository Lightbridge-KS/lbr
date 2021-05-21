#' Mirror function arguments
#' @description Mirror names of ... to Rt side. Use to write function argument faster
#' @param ... Arguments that has `Name = Value` pair
#' @param .new_line (logical) Print out each arguments in new line or not
#'
#' @return print to console
#' @importFrom rlang list2
#' @importFrom purrr map_chr reduce
#' @export
#'
#' @examples mirror_args(x = 5, y = NULL, z = "hi")
mirror_args <- function(..., .new_line = F) {

  ls <- rlang::list2(...)
  names(ls) %>%
    purrr::map_chr(~paste0(.x , " = ", .x)) %>%
    purrr::reduce(paste, sep = ifelse(.new_line, ",\n", ", ")) %>%
    cat()
}
