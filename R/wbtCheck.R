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
#' @param silent_wbt Should Whitebox tools messages be suppressed? This function prints messages to the console already but these messages can be useful if you need to do some debugging.

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

wbtCheck <- function(force = FALSE, silent_wbt = FALSE) {

  rlang::check_installed("whitebox", reason = "required to use function drainageBasins") #This is here because whitebox is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"

  if (silent_wbt) {
    old_option <- options("whitebox.verbose_mode")
    options("whitebox.verbose_mode" = FALSE)
    if (old_option$whitebox.verbose_mode) {
      on.exit(options("whitebox.verbose_mode" = TRUE))
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
      message("WhiteboxTools is already installed: using version ", substr(version[1], 16, 20), ".")
    }
  }

  wbt_version <- substr(version[1], 1, 20)
  return(wbt_version)
}
