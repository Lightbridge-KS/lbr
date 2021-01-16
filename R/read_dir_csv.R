#' Read CSV files form a directory
#'
#' @param dir_path Path to desired directory. If `NULL`, path to current working directory is assumed.
#' @param ... Argument to pass to `readr::read_csv()`
#' @param file_pattern Regular expression to read from file name, default = "\\.csv$"
#' @param strict_csv_ext If `TRUE` (default): Strictly read only .csv file
#' @param recursive If `TRUE`: Also read in sub-directory
#' @param snake_case If `TRUE`: include snake_case in file_names (require snakecase package)
#' @inheritParams readr::read_csv()
#' @return A list of tibbles
#' @import readr stringr purrr tools utils
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' # Not run
#' if(FALSE){
#' # Read form current working directory by default
#'   read_dir_csv()
#' # And file names are set to names of each data frame in a list
#'
#' # Give a directory path
#'   read_dir_csv("path/to/dir")
#'
#' # Also, can read from every sub-directory of given directory
#'   read_dir_csv("path/to/dir", recursive = T)
#'
#' # Can specify regular expression of file names to read
#'   read_dir_csv(file_pattern = "[:digit:]+\\.csv$")  # file name contains numbers
#'
#' # If set strict_csv_ext = F and remove regex which require file name ending with .csv
#' # it might read other file type as well (if you like)
#'   read_dir_csv(file_pattern = "[:digit:]+", strict_csv_ext = F)
#'
#' # If you want file names in snake_case (require snakecase package)
#'   read_dir_csv(snake_case = T)
#' }
read_dir_csv <- function(dir_path = NULL, # if NULL: dir_path is current working dir
                         ... , # ... pass to read_csv
                         file_pattern = "\\.csv$", # Regex to read from .csv file
                         strict_csv_ext = T, # if TRUE: Strictly read only .csv
                         recursive = F,    # if TRUE: Also read in sub-dir
                         snake_case = F){  # if TRUE: include snake_case in file_names

  require(magrittr)
  requireNamespace("readr", quietly = FALSE)
  requireNamespace("stringr", quietly = FALSE)
  requireNamespace("purrr", quietly = FALSE)
  requireNamespace("tools", quietly = FALSE)
  requireNamespace("utils", quietly = FALSE)


  msg_no_csv <- function(file_names){

    not_csv_file_names <- file_names[ tools::file_ext(file_names) != "csv"]

    if(length(not_csv_file_names) != 0){

      message("Not parsing", length(not_csv_file_names), "non-csv file(s) that match", " `file_pattern`:\n\n")
      purrr::walk(not_csv_file_names, message)
    }
  }

  dir_path <- if(is.null(dir_path)) getwd() else dir_path

  if( utils::file_test("-d", dir_path) == F ){
    warning(" Not a directory, attempting to use regular `read_csv`\n")
    return( readr::read_csv(dir_path , ...) )
  }

  logic_pattern <- dir(dir_path, recursive = recursive) %>% stringr::str_detect(file_pattern)

  file_names <- dir(dir_path, recursive = recursive)[logic_pattern]


  csv_file_names <- if(strict_csv_ext == T){

    msg_no_csv(file_names = file_names)
    file_names[ tools::file_ext(file_names) == "csv"] # strictly filter only .csv

  }else{ # this may be read other format as well eg. pdf, html, etc.

    dir(path = dir_path,pattern = file_pattern, recursive = recursive)
  }

  abs_file_path <- file.path(dir_path, csv_file_names)

  names <- stringr::str_remove(csv_file_names,"\\.[^\\.]+$") #Remove last dot and everything after

  if(length(names) == 0) stop("Not found .csv file",call. = F)

  if(snake_case == T){
    if( !requireNamespace(snakecase, quietly = TRUE) ) stop("This feature require `snakecase` package installed")
    names <- names %>% purrr::map_chr(snakecase::to_snake_case)
  }

  abs_file_path %>%
    purrr::map(readr::read_csv, ...) %>%
    purrr::set_names(names)


}
