


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
  structure(hydra,
            class = c(class(hydra), "api_response", "thl_hydra"),
            api_data_url = attr(x, "api_data_url"))
}

#' @describeIn getHydra Get subject
#'
#' @examples
#' getSubject("thil")
#'
#' @export
getSubject <- function(subject, api = url_api()) {
  data_url <- url_subject(subject, api = api)
  x <- getFromAPI(data_url)
  subj <- parse_subject(x)
  structure(subj,
            class = c(class(subj), "api_response", "thl_subject"),
            api_data_url = attr(x, "api_data_url"))
}

#' @describeIn getHydra Show data sets for subject or hydra
#'
#' @param x An object of class 'thl-subject' or 'thl-hydra'
#'
#' @examples
#'
#' x <- getSubject("epirapo")
#' datasets <- getDatasets(x)
#'
#' @export
getDatasets <- function(x) {

  api_url <- api_url_from_response(x)
  base_url <- api2base(api_url)
  df <- x[x$class == "dataset", ]
  df2 <- parse_datasets(df, base_url = base_url)
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
#' x <- getSubject("epirapo")
#' datasets <- getDatasets(x)
#' cube <- getCube(datasets, index = 1L)
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


