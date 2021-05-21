#' Encode Vector
#' @description Match and Encode vector
#' @param x vector: input data to be matched.
#' @param match vector: matching value to `x`
#' @param encode vector: encoding vector same length as `match`
#'
#' @return encoded vector
#' @details Elements that not match will return `NA`.
#' @export
#'
#' @examples
#' encoder(c("a","b","d"), c("a","b","c"), c("A","B","C"))
encoder <- function(x, # Any vector
                    match,
                    encode = match # Encode that pair with match
) {

  if(length(match) != length(encode)) stop("`match` and `encode` must have same length", call. = F)

  df <- data.frame(match = match, encode = encode)
  index <- match(x, df$match)

  df$encode[index]

}
