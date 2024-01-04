#' Check WhiteboxTools binaries installation
#'
#' @description
#' Checks for the existence of WhiteboxTools in its default directory and installs it if necessary or if `force = TRUE`.
#'
#' @param force Set TRUE to force update of WhiteboxTools binaries.
#'
#' @return Returns the version number of the installed binaries and (if necessary) installs WhiteboxTools in its default location.
#' @export
#'

wbtCheck <- function(force = FALSE) {
  wbt_check <- whitebox::check_whitebox_binary()
  if (!wbt_check){
    message("Installing WhiteboxTools binaries...")
    whitebox::wbt_install()
    version <- invisible(utils::capture.output(whitebox::wbt_version()))
    message("Installed WhiteboxTools version ", substr(version[1], 16, 20))
  } else {
    if (force) {
      whitebox::wbt_install(force = TRUE)
      version <- whitebox::wbt_version()
      message("Installing WhiteboxTools version ", substr(version[1], 16, 20), " (force update).")
    } else {
      version <- whitebox::wbt_version()
      message("WhiteboxTools is already installed: using version ", substr(version[1], 16, 20), ".")
    }
  }

  wbt_version <- substr(version[1], 1, 20)
  return(wbt_version)
}
