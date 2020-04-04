


#' Get hydra
#'
#' @param subject Subject name, chracter
#' @param hydra Hydra name, character
#' @param api URL of the API
#'
#' @examples
#'
#' subject <- "toitu"
#' hydra <- "ennakko3"
#' getHydra(subject, hydra)
#'
#' @export
getHydra <- function(subject, hydra, api = url_api()) {
  url <- url_hydra(subject = subject, hydra = hydra, api = api)
  x <- getFromAPI(url)
  hydra <- parse_hydra(x)
  structure(hydra, class = c(class(hydra), "thl_hydra"))
}

#' @describeIn getHydra Get subject
#'
#' @examples
#' getSubject("thil")
#'
#' @export
getSubject <- function(subject, api = url_api()) {
  url <- url_subject(subject, api = api)
  x <- getFromAPI(url)
  subj <- parse_subject(x)
  structure(subj, class = c(class(subj), "thl_subject"))
}

#' @describeIn getHydra Show data sets for subject or hydra
#'
#' @param x An object of class 'thl-subject' or 'thl-hydra'
#'
#' @examples
#'
#' subject <- getSubject("toitu")
#' showDatasets(subject)
#'
#' @export
showDatasets <- function(x, api = url_api()) {
  x[x$class == "dataset", ]
}


