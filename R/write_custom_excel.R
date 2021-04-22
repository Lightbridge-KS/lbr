#' Write Custom Excel
#' @description A Wrapper around `openxlsx::write.xlsx`
#' @param x Object or a list of objects that can be handled by `openxlsx::writeData` to write to file
#' @param file xlsx file name
#' @param asTable Write using writeDataTable as opposed to writeData
#' @param borders Cell borders, Either "surrounding", "columns" or "rows" or NULL. If "surrounding", a border is drawn around the data. If "rows", a surrounding border is drawn a border around each row. If "columns", a surrounding border is drawn with a border between each column. If "all" all cell borders are drawn
#' @param keepNA  If `TRUE`, NA values are converted to #N/A (or na.string, if not `NULL`) in Excel, else `NA` cells will be empty. Defaults to `FALSE`.
#' @param colWidths  May be a single value for all columns (or "auto"), or a list of vectors that will be recycled for each sheet
#' @param freeze_firstRow If `TRUE`, freezes the first row (equivalent to firstActiveRow = 2)
#' @param freeze_firstCol If `TRUE`, freezes the first column (equivalent to firstActiveCol = 2)
#' @param head_text Header text styling - one of "bold", "strikeout", "italic", "underline", "underline2" (passed to `openxlsx::createStyle`)
#' @param head_fgfill Header cell foreground fill colour. (passed to `openxlsx::createStyle`)
#' @param head_border Header Cell border. A vector of "top", "bottom", "left", "right" or a single string). (passed to `openxlsx::createStyle`)
#' @param head_halign Header Horizontal alignment of cell contents (passed to `openxlsx::createStyle`)
#' @param head_valign Header Vertical alignment of cell contents (passed to `openxlsx::createStyle`)
#' @param ... to `openxlsx::write.xlsx`
#'
#' @return A workbook object
#' @importFrom purrr map_lgl walk
#' @importFrom openxlsx createStyle write.xlsx freezePane saveWorkbook
#' @export
#'
#' @examples # write_custom_xlsx(list(a = iris, b = mtcars), "path/to/file.xlsx")
write_custom_xlsx <- function(x, file, asTable = F,
                              borders = "columns", # Text border
                              keepNA = F,
                              colWidths = 8.43,
                              # freezePane
                              freeze_firstRow = F,
                              freeze_firstCol = F,
                              # Header style
                              head_text = "bold", # textDecoration
                              head_fgfill = "#d9ead3", # fgFill
                              head_border = "TopBottomLeftRight", # header border
                              head_halign = "center",
                              head_valign = "center",
                              ...){


  is.df <- is.data.frame(x)
  is.list_df <- all(purrr::map_lgl(x, is.data.frame))
  if(!is.df & !is.list_df) stop("`x` must be data frame of list of data frame", call. = F)

  sheets_seq <- if(is.df){ 1 }else{ seq_along(x) }

  # Create Header Style
  head_style <- openxlsx::createStyle(textDecoration = head_text,
                                      halign = head_halign, valign = head_valign,
                                      fgFill = head_fgfill,
                                      border = head_border)

  wb <- openxlsx::write.xlsx(x, file, asTable = asTable,
                             headerStyle = head_style,
                             borders = borders,
                             keepNA = keepNA,
                             colWidths = colWidths,
                             ...)
  # Freeze First Row
  purrr::walk(sheets_seq ,
              ~openxlsx::freezePane(wb, sheet = .x ,
                                    firstRow = freeze_firstRow,
                                    firstCol = freeze_firstCol) )

  openxlsx::saveWorkbook(wb,  file, overwrite = T)

}
