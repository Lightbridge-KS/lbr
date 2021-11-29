#' Encode Vector
#'
#' @description Match and Encode vector
#' @param x vector: input data to be matched.
#' @param match vector: matching value to `x`
#' @param encode vector: encoding vector same length as `match`
#' @param nomatch Character: Indicate result if elements in `x` was not matched with `match`.
#' It must be one of:
#' * \strong{"NA"}: return `NA`
#' * \strong{"original"}: return original elements in `x`
#'
#' @return encoded vector
#' @details Elements that not match will return values according to `nomatch`.
#' @export
#'
#' @examples
#' encoder(c("a","b","d"), c("a","b","c"), c("A","B","C"))
#' encoder(c("a","b","d"), c("a","b","c"), c("A","B","C"), nomatch = "original")
encoder <- function(x, # Any vector
                    match,
                    encode = match, # Encode that pair with match
                    nomatch = c("NA", "original")
) {

  nomatch <- match.arg(nomatch)
  if(length(match) != length(encode)) stop("`match` and `encode` must have same length", call. = F)

  index <- match(x, match)
  encoded_may_NA <- encode[index]

  # Control no matching cases
  out <- switch (nomatch,
                 # Leaves as `NA`
                 "NA" = { encoded_may_NA },
                 # Use Original `x`
                 "original" = {
                   encoded_may_NA[is.na(encoded_may_NA)] <- x[is.na(encoded_may_NA)]
                   encoded <- encoded_may_NA
                   encoded
                 }
  )
  out
}


