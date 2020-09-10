


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
            class = c(class(df2), "thlDatasets"))
}


#' get dimension data of a hydra
#'
#' @param url URL to hydra fact
#'
#' @examples
#'
#' url <- "https://sampo.thl.fi/pivot/prod/fi/toitu/ennakko3/fact_toitu_ennakko.json"
#' dimensions <- get_dimensions(url)
get_dimensions <- function(url) {

  url_new <- gsub(".json$","", url)
  url_new <- unlist(strsplit(url_new, split = "/"))

  base_url <- paste0(url_new[1:5], collapse = "/")
  url_new <- url_new[c(-c(1:5))]

  dimension_path <- paste0(paste(url_new, collapse = "/"), ".dimensions")

  dimensionURL <- api_data_url(api =base_url,
                               path = dimension_path,
                               format = "json")

  dimensions <- getFromAPI(dimensionURL)
  dimensions

}

#' THL cube
#'
#' @param datasets An object of class "api_datasets" object,
#' or a data frame with columns 'lang', 'subject', 'hydra', 'cube'.
#' @param index an integer defining the dataset of interest from the datasets data.frame.
#'
#'
#' TODO: this is an object which can be utilised to build quaeris to retrieve data
#' and attach labels to the data. Methods need to be written which
#' implement these
#'
#' @examples
#'
#' url <- "https://sampo.thl.fi/pivot/prod/fi/toitu/ennakko3/fact_toitu_ennakko.json"
#' cube <- thlCube(url)
#' names(cube$dimensions)
#'
#' @export
thlCube <- function(url) {

  # The idea is that the object returned by this function
  # would know it's location and dimensions
  # then methods can be written to retrieve data.

  dimensions <- get_dimensions(url)

  hydra_dimensions <- parse_dimensions(dimensions)

  res <- list(url = url,
       dimensions = hydra_dimensions)

  class(res) <- "thl_cube"
  res
}

#' Print a THL cube
#'
#' @param x Object of class "thl_cube"
#'
#' @export
print.thl_cube <- function(x) {
  cat("Cube with columns", colnames(x))
}



