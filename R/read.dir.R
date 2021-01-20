#' Read files form a directory by a given function
#' @description Read multiple files from a directory (folder) using reading engine supply by a function.
#'
#' @param fun A function to read files from a directory. First argument of the function must be file path.
#'            Example : `utils::read.csv`, `readr::read_csv`, `rio::import`, `readxl::read_excel`, `base::readRDS`
#' @param path Character: Path to desired directory which contain file, default is "." working directory. (passed to `base::list.files`)
#' @param pattern Character: Specify regular expression to match file extension and file names. (passed to `grep()`) .
#'                Default is to read .csv file "\\.csv$". Please select according to `fun`; Example, use `\\.xlsx$` to read from excel file.
#' @param ... Argument to pass to `fun`
#' @param invert Logical: If `TRUE` read from file that file names do not match `pattern`.(passed to `grep()`)
#' @param ignore.case If FALSE, the regular expression matching is case sensitive and if `TRUE`, case is ignored during matching.(passed to `grep()`)
#' @param perl Logical: Should Perl-compatible regexps be used? (passed to `grep()`)
#' @param fixed Logical: If `TRUE`, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param all.files If `TRUE` hidden files are also returned. (passed to `base::list.files`)
#' @param recursive If `TRUE` read recursively in sub-directory.(passed to `base::list.files`)
#' @param no.. 	logical. Should both "." and ".." be excluded also from non-recursive listings? (passed to `base::list.files`)
#' @param snake_case If `TRUE`: Convert names to snake_case_format.  (require `snakecase` package)
#'
#'
#' @return A list of object returned by `fun`
#' @export
#'
#' @examples # Not run
#' if(FALSE){
#'
#'  # Read .csv file form working directory (default) using `utils::read.csv`.
#'  ## file names are set to each elements of the output.
#'
#'    read.dir(utils::read.csv) # default `pattern` is "\\.csv$"
#'
#'  # Read .xlsx file from a directory using `readxl::read_excel`.
#'  ## Must specify regular expression to match file extension.
#'
#'    read.dir(readxl::read_excel, path = "path/to/dir" ,pattern = "\\.xlsx$")
#'
#'  # Read .rds file ; To also read form sub-directory set `recursive = TRUE` .
#'
#'    read.dir(readRDS, pattern = "\\.rds$", path = "path/to/dir" ,recursive = TRUE)
#'
#'  # Read files using multiple engine from multiple path and multiple file extension.
#'
#'    params <- list(fun = c(read_csv, readxl::read_excel),
#'                 path = c("path/to/dir_1", "path/to/dir_2"),
#'                 pattern = c("\\.csv$", "\\.xlsx$"))
#'
#'    purrr::pmap(params, read.dir)
#'
#'    # or using base R
#'
#'    ls <- vector("list", 2)
#'
#'    for(i in seq_along(params[[1]])){
#'
#'      ls[[i]]  <- read.dir(fun = params[[1]][[i]],  path = params[[2]][[i]],
#'                           pattern = params[[3]][[i]])
#'    }
#' }
read.dir <- function(fun, path = ".",    # ... to fun
                     pattern = "\\.csv$", # to grep()
                     ... , # to fun
                     invert = F, ignore.case = F, # to grep()
                     perl = F, fixed = F,
                     all.files = F, recursive = F, no.. = F, # to list.files()
                     snake_case = F
){
  # Dependency rlang !! (suggest snake_case pkg)

  # 1. Test file validity --------------------------------------------------------------

  ##  test file exist ?
  if(!file.exists(path)) stop("Path is not a file or directory !")

  ##  Now file is exist ; test is it a file or link ?
  if( file.info(path)["isdir"] == F ){

    fun_name <- names(rlang::enquos(fun, .named = T))
    message("Path is not a directory, attempting to read by `", fun_name ,"`")
    return(fun(path, ...))

  } else { ### Path is directory
    # Get abs path
    get_file <- function(type){
      list.files(path, full.names = switch (type,"path" = TRUE, "name" = FALSE),
                 all.files = all.files,recursive = recursive, no.. = no..)
    }

    fp <- get_file(type = "path")
    fn <- get_file(type = "name")

    # Filter pattern
    i_fp_f <- grep(pattern, fp , value = F, invert = invert, ignore.case = ignore.case,
                   perl = perl, fixed = fixed)

    fp_filt <- fp[i_fp_f]      # filter file path
    fp_not_filt <- fp[-i_fp_f]

    fn_filt <- fn[i_fp_f]      # filter file name

    if(length(fp_not_filt) != 0){
      message("Not read ",length(fp_not_filt),
              " file(s) that filename contain non-matching pattern: \n")
      print_messages(fp_not_filt)
    }

    # Read and rename ---------------------------------------------------------------
    if(missing(fun)){
      stop("You must supply `fun` to read file(s) from `path`, for example utils::read.csv")
    }

    ls_df <- lapply(fp_filt, fun, ...)
    names(ls_df) <- sub("\\.[^\\.]+$","",fn_filt)

    # Rename to snake_case
    if(snake_case == T){
      if( !requireNamespace("snakecase", quietly = TRUE) ) {stop("This feature require `snakecase` package installed")}
      names(ls_df) <- lapply(names(ls_df), snakecase::to_snake_case)
    }

    ls_df

  }

}

# Helper Fn
#' Message function
#'
#' @param chr_vec character
#'
#' @return message
#'
#'
print_messages <- function(chr_vec){

  invisible( lapply(sub("$","\n", chr_vec), message) )
}
