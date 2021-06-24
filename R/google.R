#' Create Google Sheet at Specified Location
#'
#' Calls `gs4_create()` then move file to specified location with `drive_mv()`
#'
#' @param path Specifies target destination for the file on Google Drive. Can be an actual path (character), a file id marked with as_id(), or a dribble.
#' @param name The name of the new spreadsheet.
#' @param sheets Optional input for initializing (work)sheets. If unspecified, the Sheets API automatically creates an empty "Sheet1". You can provide a vector of sheet names, a data frame, or a (possibly named) list of data frames.
#' @param overwrite
#'   * `NA` (default): Just do the operation, even if it results in multiple files with the same filepath.
#'   * `TRUE`: Check for a pre-existing file at the filepath. If there is zero or one, move a pre-existing file to the trash, then carry on. Note that the new file does not inherit any properties from the old one, such as sharing or publishing settings. It will have a new file ID. An error is thrown if two or more pre-existing files are found.
#'   * `FALSE`: Error if there is any pre-existing file at the filepath.
#' @param ... to `gs4_create()`: Optional spreadsheet properties that can be set through this API endpoint, such as locale and time zone.
#'
#' @return Object class: "sheets_id", "drive_id"
#' @export
#'
#' @examples # gs4_create_at("folder_name", name = "file_name", sheets = "sheet_name")
gs4_create_at <- function(path = NULL,
                          name = gs4_random(),
                          sheets = NULL,
                          overwrite = NA,
                          ...
) {

  ss <- googlesheets4::gs4_create(name = name, sheets = sheets, ...)

  googledrive::drive_mv(ss, path = path, name = name, overwrite = overwrite)

  ss
}
