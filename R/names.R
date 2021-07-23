#' Set Names in an Object that Match Lookup table
#'
#' Set names of object by matching object's old names to names of lookup table.
#' This will ensure that object's names will always named correctly.
#' This function also wraps `rlang::set_names()`.
#'
#' @param x Vector to name.
#' @param lookup Lookup vector: the names of `lookup` must corresponding to "old names" of `x` and the
#'   values of `lookup` will be set as "new names" of `x`
#' @param invert Logical: If `TRUE`, invert names and values of `lookup`.
#' @param quiet Logical: If `FALSE`, the message of non-matching names of `x` will be displayed.
#' @param ... passed to `rlang:set_names()`
#'
#' @return `x` with new names
#' @export
#'
#' @examples
#' set_names_match(iris, c("Sepal.Length" = "SL",
#'                         "Sepal.Width" = "SW",
#'                         "Petal.Length" = "PL")
#'                )
set_names_match <- function(x,
                            lookup = x, # New Name from lookup Value, Old name from names(lookup)
                            invert = FALSE, # Invert names and value of lookup
                            quiet = FALSE,
                            ... # passed to rlang::set_names()
) {

  ## If not Invert Names and Values of Lookup table
  if(!invert){
    value <- names(lookup)
    nm <- lookup
    lookup <- value
    names(lookup) <- nm
  }

  ## Rearrange Lookup vector
  lookup_index <-  match(names(x),  lookup)
  is_any_no_match <- any(is.na(lookup_index))

  if(is_any_no_match){
    no_match_index <- which(is.na(lookup_index))
    no_match_nm <- names(x)[no_match_index]
    message("The following names of `x` are not match:\n")
    print_messages(no_match_nm)
  }

  lookup_arranged <- lookup[lookup_index]
  ## Set Names
  rlang::set_names(x, names(lookup_arranged), ...)

}
