


#' THL Hydra
#'
#' @param subject Subject name, chracter
#' @param hydra Hydra name, character
#' @param api URL of the API
#'
#' @examples
#'
#' subject <- "toitu"
#' hydra <- "ennakko3"
#' h <- thlHydra(subject, hydra)
#'
#' @export
thlHydra <- function(subject, hydra, api = url_api()) {
  url <- openthl:::url_hydra(subject = subject, hydra = hydra, api = api)

  x <- getFromAPI(url)
  meta <- parse_hydra(x)
  structure(meta,
            class = c(class(meta), "thlHydra"),
            url = url)
}

#' @describeIn thlHydra THL subject
#'
#' @examples
#' thlSubject("thil")
#'
#' @export
thlSubject <- function(subject, api = url_api()) {
  url <- url_subject(subject, api = api)
  x <- getFromAPI(url)
  subj <- parse_subject(x)
  structure(subj,
            class = c(class(subj), "thlSubject"),
            url = url)
}

#' @describeIn thlHydra Show datasets for a thlSubject or a thlHydra
#'
#' @param x An object of class 'thlSubject' or 'thlHydra'
#'
#' @examples
#'
#' x <- thlSubject("epirapo")
#' datasets <- thlDatasets(x)
#'
#' @export
thlDatasets <- function(x) {

  api_url <- attr(attr(x, "url"), "api-url")
  base_url <- openthl:::api2base(api_url)
  df <- x[x$class == "dataset", ]
  df2 <- openthl:::parse_datasets(df, base_url = base_url)
  structure(df2,
            class = c(class(df2), "api_datasets"))
}


#' Get cube
#'
#' @param datasets An object of class "api_datasets" object,
#' or a data frame with columns 'lang', 'subject', 'hydra', 'cube'.
#' @param index an integer defining the dataset of interest from the datasets data.frame.
#'
#' @examples
#'
#' hydra <- getHydra("epirapo", "covid19case")
#' cube <- getCube(datasets, index = 1L)
#' @export
#'
getCube <- function(datasets, index = 1L) {

  # The idea is that the object returned by this function
  # would know it's location and dimensions
  # then methods can be written to retrieve data.


  if(index > nrow(datasets)) stop("Index is larger than the number of datasets")
  ds <- datasets[index, , drop = FALSE]

  fact_path <- paste0(ds$lang, "/", ds$subject, "/", ds$hydra, "/fact_",ds$cube)
  dimension_path <-  paste0(fact_path, ".dimensions")
  dimensionURL <- api_data_url(api = ds$base_url,
               path = dimension_path,
               format = "json")

  dimensions <- getFromAPI(dimensionURL)

  # TODO: the messy dimensions object needs a smart format and methods to handle it
  list(dataset = ds,
       dimensions = dimensions)
}


