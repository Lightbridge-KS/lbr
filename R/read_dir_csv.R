#' Read CSV files form a directory
#' @description Read multiple CSV files from a directory (folder)
#' @param path Character: Path to desired directory which contain file, default is "." working directory. (passed to `fs::dir_ls()`)
#' @param ... Argument to pass to `readr::read_csv()`
#' @param all If `TRUE` hidden files are also returned. (passed to `fs::dir_ls()`)
#' @param recurse If `TRUE` read recursively in sub-directory, if a positive number the number of levels to recurse. (passed to `fs::dir_ls()`)
#' @param strict_csv_ext If `TRUE` (default): Strictly read only files, which has files name ending with .csv
#' @param regexp Character: Specify a regular expression to read from file names (passed to `grep()`) . If you want to match file names other than `.csv`, turn `strict_csv_ext = FALSE`.
#' @param invert Logical: If `TRUE` read from file that file names do not match `regexp`.(passed to `grep()`)
#' @param ignore.case If FALSE, the regular expression matching is case sensitive and if `TRUE`, case is ignored during matching.(passed to `grep()`)
#' @param perl Logical: Should Perl-compatible regexps be used? (passed to `grep()`)
#' @param fixed Logical: If `TRUE`, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param snake_case If `TRUE`: Convert data frame names to snake_case_format.  (require `snakecase` package)
#'
#' @seealso `readr::read_csv()`
#' \url{https://readr.tidyverse.org}
#'
#' @seealso `fs` package
#' \url{https://fs.r-lib.org/index.html}
#'
#' @return A list of tibbles
#' @import readr fs
#'
#' @export
#'
#'
#' @examples
#'
#' #' # Not run
#' if(FALSE){
#' # Read form current working directory by default
#'   read_dir_csv()
#' # And file names are set to names of each data frame in a list
#'
#' # Give a directory path
#'   read_dir_csv("path/to/dir")
#'
#' # Also, can read from every sub-directory of given directory
#'   read_dir_csv("path/to/dir", recurse = T)
#'
#' # Can specify regular expression of file names to read.
#'   read_dir_csv(regexp = "[[:digit:]]+")  # CSV file name contains numbers
#'
#' # If set strict_csv_ext = F and remove regex which require file name ending with .csv
#' # it might read other file type as well (if you like)
#'   read_dir_csv(regexp = "[[:digit:]]+\\.txt$", strict_csv_ext = FALSE) # Try read .txt file
#'
#' # If you want file names in snake_case (require snakecase package)
#'   read_dir_csv(snake_case = TRUE)
#' }
#'
read_dir_csv <- function(path = ".", # Pass to fs::dir_ls()
                           ... ,       # Pass to readr::read_csv()
                           all = F,     # Pass to fs::dir_ls() ; If TRUE also read hidden file
                           recurse = F,   # Pass to fs::dir_ls() ; If TRUE also read in sub dir
                           strict_csv_ext = T ,
                           regexp = ".", # Pass to grep()
                           invert = F, ignore.case = F, perl = F, fixed = F, # Pass to grep()
                           snake_case = F
){
  ## ! Dependency - readr::read_csv(), fs :: is_dir, is_link, is_file, dir_ls ,path_file



  # 1. Test file validity --------------------------------------------------------------------
  ###  test if path is a directory
  if( !fs::is_dir(path) ){

    if( !fs::is_file(path) && !fs::is_link(path)) stop("Path is not a file or directory")
    message("Path is not a directory, attempting to read by `readr::read_csv()`")
    return(readr::read_csv(path, ...))

  } else { ### path is directory

    # 2. Get absolute path +/- filter .csv ------------------------------------------------

    abs_path <- fs::dir_ls(path, all = all ,recurse = recurse, type = "file")

    abs_path_filtered <- grep(regexp, abs_path , value = T,
                              invert = invert, ignore.case = ignore.case,
                              perl = perl, fixed = fixed)

    i_csv <- grep("\\.csv$", abs_path_filtered, value = F)
    i_non_csv <- grep("\\.csv$", abs_path_filtered, value = F, invert = T)

    final_path <- if(strict_csv_ext){ ## Strictly filter only .csv file

      # Message : read + not read

      message("Read ", length(i_csv), " csv files\n") # Msg read ... csv files

      if(length(i_non_csv) != 0){ # Msg not read ... non-csv file
        warning("Not read from ", length(i_non_csv), " non-csv file(s) listed here:\n\n")
        print_warnings(abs_path_filtered[i_non_csv])

      }

      # Get abs path for strict .csv file

      abs_path_filtered[i_csv]

    } else { ## Not-strictly filter only .csv file

      if(length(i_non_csv) != 0){ # Msg not read ... non-csv file
        warning("Attempt to read ", length(i_non_csv), " file(s) not ending with `.csv`:\n")
        print_warnings(abs_path_filtered[i_non_csv])
      }

      abs_path_filtered

    }

    # 2. Read and rename ---------------------------------------------------------------

    ls_df <- lapply(final_path, read_csv, ...)

    file_names <- fs::path_file(final_path)
    names(ls_df)<- sub("\\.[^\\.]+$","",file_names)

    if(snake_case == T){
      if( !requireNamespace("snakecase", quietly = TRUE) ) {stop("This feature require `snakecase` package installed")}
      names(ls_df) <- lapply(names(ls_df), snakecase::to_snake_case)
    }

    ls_df


  }

}



#' Helper function : for read_dir_csv
#'
#' @param chr_vec character
#'
#' @return warning
#'
#'
#'
print_warnings <- function(chr_vec){

  invisible( lapply(sub("$","\n", chr_vec), warning) )
}
