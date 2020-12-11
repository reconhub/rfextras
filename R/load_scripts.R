#' Load R scripts within factory folder
#'
#' This helper function will load all `.R` scripts within a specified factory
#' and folder. All scripts are loaded within the calling environment and in
#' alphanumeric order.
#'
#' @param factory The path to the report factory or a folder within the desired
#'   factory. Defaults to the current directory.
#' @param folder The folder (relative to the factory) from which to source `.R`
#'   files; defaults to `scripts/`.
#' @param quiet A logical indicating whether messages should be displayed to the
#'   console (`TRUE`, default), or not.
#'
#' @export
load_scripts <- function(factory = ".", folder = "scripts", quiet = FALSE) {

  ## Approach: we list all .R files in the specified folder, and load all of
  ## them in alphanumeric order; we need to make sure all objects are loaded in
  ## the parent environment. This is achieved using `sys.source` rather than
  ## `source`.

  # get the factory root and the script directory
  tmp <- reportfactory::validate_factory(factory)
  root <- tmp$root
  scripts_dir <- file.path(root, folder)

  # check the directory exists
  if(!dir.exists(scripts_dir)) {
    stop(sprintf("directory %s does not exist", scripts_dir))
  }

  ## get parent environment
  parent <- parent.frame()

  ## process .R files in scripts/
  script_files <- list.files(
    scripts_dir,
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  )

  script_files <- sort(script_files)

  if (length(script_files)) {
    for (file in script_files) {
      if (!quiet) {
        msg <- sprintf("Loading %s\n", file)
      }
      sys.source(file, envir = parent)
    }
  } else {
    message("No `.R` files in %s", scripts_dir)
  }

  ## nothing to return, bye everyone
  return(invisible(NULL))
}
