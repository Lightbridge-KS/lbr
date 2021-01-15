# Less strict : Read csv form directory (default = current wd) -----------------------------

read_dir_csv <- function(dir_path = NULL, # if NULL: dir_path is current working dir
                         ... , # ... pass to read_csv
                         file_pattern = "\\.csv$", # Regex to read from .csv file
                         strict_csv_ext = T, # if TRUE: Strictly read only .csv
                         recursive = F,    # if TRUE: Also read in sub-dir
                         snake_case = F){  # if TRUE: include snake_case in file_names

  require(readr)
  require(stringr)
  require(purrr)
  require(tools)
  require(utils)

  msg_no_csv <- function(file_names){

    not_csv_file_names <- file_names[ tools::file_ext(file_names) != "csv"]

    if(length(not_csv_file_names) != 0){

      message("Not parsing", length(not_csv_file_names), "non-csv file(s) that match", " `file_pattern`:\n\n")
      walk(not_csv_file_names, message)
    }
  }

  dir_path <- if(is.null(dir_path)) getwd() else dir_path

  if( utils::file_test("-d", dir_path) == F ){
    warning(" Not a directory, attempting to use regular `read_csv`\n")
    return( read_csv(dir_path , ...) )
  }

  logic_pattern <- dir(dir_path, recursive = recursive) %>% str_detect(file_pattern)

  file_names <- dir(dir_path, recursive = recursive)[logic_pattern]


  csv_file_names <- if(strict_csv_ext == T){

    msg_no_csv(file_names = file_names)
    file_names[ tools::file_ext(file_names) == "csv"] # strictly filter only .csv

  }else{ # this may be read other format as well eg. pdf, html, etc.

    dir(path = dir_path,pattern = file_pattern, recursive = recursive)
  }

  abs_file_path <- file.path(dir_path, csv_file_names)

  names <- str_remove(csv_file_names,"\\.[^\\.]+$") #Remove last dot and everything after

  if(length(names) == 0) stop("Not found .csv file",call. = F)

  if(snake_case == T){
    require(snakecase)
    names <- names %>% map_chr(snakecase::to_snake_case)
  }

  abs_file_path %>%
    map(read_csv, ...) %>%
    setNames(names)


}
