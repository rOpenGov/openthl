

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

#' API URL of a subject
#'
#' @examples
#' if(FALSE)
#'   url_subject("toitu")
#'
#' @noRd
url_subject <- function(subject, api = url_api()) {
  file.path(api, subject)
}

#' API URL of a hydra
#'
#' @examples
#'
#' if(FALSE)
#'   url_hydra("toitu", "ennakko3")
#'
#' @noRd
url_hydra <- function(subject, hydra, api = url_api()) {
  url <- url_subject(subject, api = api)
  file.path(url, hydra)
}


