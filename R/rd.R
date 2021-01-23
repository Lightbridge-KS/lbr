#' Read files form a directory by a given function
#' @description Read multiple files from a directory (folder) using reading engine supply by a function.
#'
#' @param .f A function to read files from a directory. First argument of the function must be file path. Support formula interface.
#'            Example : `utils::read.csv`, `readr::read_csv`, `rio::import`, `readxl::read_excel`, `base::readRDS`
#' @param path Character: Path to desired directory which contain file, default is "." working directory. (passed to `base::list.files`)
#' @param pattern Character: Specify regular expression to match file extension and file names. (passed to `grep()`) .
#'                Default is to read .csv file "\\.csv$". Please select according to `fun`; Example, use `\\.xlsx$` to read from excel file.
#' @param ... Argument to pass to `.f`
#' @param invert Logical: If `TRUE` read from file that file names do not match `pattern`.(passed to `grep()`)
#' @param ignore.case If FALSE, the regular expression matching is case sensitive and if `TRUE`, case is ignored during matching.(passed to `grep()`)
#' @param perl Logical: Should Perl-compatible regexps be used? (passed to `grep()`)
#' @param fixed Logical: If `TRUE`, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param all.files If `TRUE` hidden files are also returned. (passed to `base::list.files`)
#' @param recursive If `TRUE` read recursively in sub-directory.(passed to `base::list.files`)
#' @param no.. logical. Should both "." and ".." be excluded also from non-recursive listings? (passed to `base::list.files`)
#' @param snake_case If `TRUE`: Convert names to snake_case_format.  (require `snakecase` package)
#'
#' @returnA list of object returned by `.f`
#' @export
#'
#' @examples
#'  if(FALSE){ "Under development" }
rd <- function(.f ,
               path = ".",
               pattern = "\\.csv$",
               ... ,
               invert = F, ignore.case = F, # to grep()
               perl = F, fixed = F,
               all.files = F, recursive = F, no.. = F, # to list.files()
               snake_case = F # to snakecase
){
  # Dependency : rlang , snakecase(suggest)
  ## Check .f missing
  if(missing(.f)){
    stop("You must supply `.f` to read file from `path`, for example readr::read_csv")
  }
  ## Check .f is a function
  if(!rlang::is_function(.f) && !rlang::is_formula(.f)){
    stop("first argument `.f` must be a function or formular")
  }
  ##  Check file exist ?
  if(!file.exists(path)) stop("Path is not a file or directory !")

  ## Get file path, file name
  fp <- list.files(path, full.names = T, all.files = all.files,
                   recursive = recursive, no.. = no..) # Abs path
  i_fp_fil <- grep(pattern, fp , value = F, invert = invert,
                   ignore.case = ignore.case, perl = perl,
                   fixed = fixed) # Index Abs path filtered

  fp_fil <- fp[i_fp_fil]    # Path filtered in
  fp_n_fil <- fp[-i_fp_fil] # Path filtered out

  fn_fil <- basename(fp_fil)  # file Names

  ## Message file that not read
  if(length(fp_n_fil) != 0){
    message("Not read ",length(fp_n_fil),
            " file(s) that filename contain non-matching pattern: \n")
    print_messages(fp_n_fil)
  }

  ##### Manage expression
  if( !rlang::is_formula(.f) ){
    ### Not a formula
    ef <- enexprs(.f , .named = TRUE)
    fun <- ef[[1]]
    fun_args <- list(...)

  } else {

    ### Formula interface
    el <- rlang::enexprs(.f , .named = TRUE)
    ell <- el[[1]][[2]] # strip out `~`

    fun <- ell[[1]] # fun
    fun_args <- rlang::call_args(ell)[-1] # store args as list

  }

  ## If Path is not dir
  if( !file.info(path)["isdir"] ){
    fun_name <- rlang::as_label(fun)
    message("Path is not a directory, attempting to read by `", fun_name ,"`")
    return(  eval( rlang::call2( fun, path, ...) ))
  }

  ## If Path is dir
  # Read
  call_final <- rlang::call2(lapply, fp_fil , fun , !!!fun_args)
  ls_df <- eval(call_final)

  # Rename
  names(ls_df) <- sub("\\.[^\\.]+$","",fn_fil)

  # Rename to snake_case
  if(snake_case == T){
    if( !requireNamespace("snakecase", quietly = TRUE) ) {
      stop("This feature require `snakecase` package installed, please `install.packages(\"snakecase\")`")
    }
    names(ls_df) <- lapply(names(ls_df), snakecase::to_snake_case)
  }

  ls_df

}

#' Helper function
#'
#' @param chr_vec character
#'
#' @return messages
#'
#'
print_messages <- function(chr_vec){

  invisible( lapply(sub("$","\n", chr_vec), message) )
}
