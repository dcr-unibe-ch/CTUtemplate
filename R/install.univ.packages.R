#' Install packages from an R-universe
#'
#' R-universes are a little like mini-CRANs. Packages are generally sourced from
#' github and compiled into binaries for easy installation. To install them,
#' though, you have to specify the repository argument in the install.packages
#' function. This function removes that requirement.
#'
#' @param x package(s) to install
#' @param univ universe to install from
#'
#' @returns NULL
#' @export
#' @importFrom jsonlite stream_in
#'
#' @examples
#'
#' # install.univ.packages("randotools")
#'
install.univ.packages <- function(x, univ = "https://ctu-bern.r-universe.dev"){

  pkgs <- jsonlite::stream_in(url(paste0(univ, "/stats/descriptions/")))
  if(any(!x %in% pkgs$Package)){
    out <- x[!x %in% pkgs$Package]
    stop(out, " not in universe ", univ)
  }

  install.packages(
    x,
    repos = c(univ, "https://cloud.r-project.org")
  )

  return(NULL)

}
