
# Mirror Args -------------------------------------------------------------



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


# Wrap Quote --------------------------------------------------------------


#' Wrap Quote to Text
#'
#' Useful for turning texts to quoted variable names. The output will ignore any white spaces,
#' new lines, or any punctuation (except underscore and dot).
#'
#' @param .chr Character vector containing texts to wrap
#'
#' @return A message to copy
#' @export
#'
#' @examples wrap_quote("var_1, !var.2,  ,  `var3`")
wrap_quote <- function(.chr) {

  quoting <- c("\"")
  # Split at White space or Newline
  chr_splited <- .chr %>% stringr::str_split("[:space:]+")

  # Extract Letters, Number, underscore or dot
  chr_extracted <- chr_splited %>%
    purrr::map(~stringr::str_extract(.x, "([:alnum:]|_|\\.)+")) %>%
    purrr::map(na.omit)

  chr_extracted %>%
    # Surround Quote
    purrr::map(~purrr::map_chr(.x, ~paste0(quoting, .x , quoting))) %>%
    # Reduce by comma
    purrr::map(~purrr::reduce(.x, paste, sep = ", ")) %>%
    print_messages()

}
