

# Base url of the API
url_base <- function(type = c("prod", "beta")) {
  type <- match.arg(type)
  paste0("https://sampo.thl.fi/pivot/", type, "/")
}

#' API URL
#'
#' @param type Production or beta version?
#'
#' @examples
#' url_api()
#'
#' @export
url_api <- function(type = c("prod", "beta")) {
  type <- match.arg(type)
  paste0(url_base(type), "api")
}

api2base <- function(x) {
  gsub("api$", "", x)
}

api_data_url <- function(api, path, format = "json") {
  x <- list(api = api, path = path, format = format)
  structure(x, class = "api_data_url")
}

#' print method for api_data_url
#'
#' @param x object of class api_data_url
#'
#' @export
print.api_data_url <- function(x, no.print = FALSE, ...) {
  url <- paste0(x$api, "/", x$path, ".", x$format)
  if(!no.print)
    cat(url)
  invisible(url)
}

#' API URL of a subject
#'
#' @examples
#' if(FALSE)
#'   url_subject("toitu")
#'
#' @noRd
url_subject <- function(subject, api = url_api(), format = "json") {
  api_data_url(api = api, path = subject, format = format )
}

#' API URL of a hydra
#'
#' @examples
#'
#' if(FALSE)
#'   url_hydra("toitu", "ennakko3")
#'
#' @noRd
url_hydra <- function(subject, hydra, api = url_api(), format = "json") {
  api_data_url(api = api, path = paste0(subject, "/", hydra), format = format)
}


