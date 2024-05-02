#' Check WhiteboxTools binaries installation
#'
#' @author Ghislain de Laplante (gdela069@uottawa.ca or ghislain.delaplante@yukon.ca)
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Checks for the existence of WhiteboxTools in its default directory and installs it if necessary or if `force = TRUE`.
#'
#' @param force Set TRUE to force update of WhiteboxTools binaries.
#' @param silent TRUE to suppress console messages other than those explaining that Whitebox Tools binaries are being installed.
#'
#' @return Returns the version number of the installed binaries and (if necessary) installs WhiteboxTools in its default location.
#' @export
#' @examplesIf interactive()
#'
#' #Check if WhiteboxTools binaries are installed. If not, install latest version.
#' wbtCheck()
#'
#' # Update WhiteboxTools binaries if they are already installed.
#' wbtCheck(force = TRUE)
#'

wbtCheck <- function(force = FALSE, silent) {

  rlang::check_installed("whitebox", reason = "required to use function drainageBasins") #This is here because whitebox is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"

  if (silent) {
    old_option <- whitebox::wbt_verbose()
    whitebox::wbt_verbose(FALSE)
    if (!is.null(old_option)) {
      if (old_option) {
        on.exit(options("whitebox.verbose_mode" = TRUE))
      }
    } else {
      on.exit(whitebox::wbt_verbose(TRUE))
    }
  }

  wbt_check <- whitebox::check_whitebox_binary()
  if (!wbt_check) {
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
      version <- suppressMessages(whitebox::wbt_version())
      if (!silent) message("WhiteboxTools is already installed: using version ", substr(version[1], 16, 20), ".")
    }
  }

  wbt_version <- substr(version[1], 1, 20)
  return(wbt_version)
}
