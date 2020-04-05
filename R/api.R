getFromAPI <- function(x) {
  UseMethod("getFromAPI", x)
}

#' Retrieve data from the pivot API
#'
#' @param api_data_url Object of class api_data_url
#'
getFromAPI.api_data_url <- function(api_data_url) {
  url <- print(api_data_url, no.print = TRUE)
  r <- httr::GET(url)
  status <- httr::status_code(r)

  if(status != 200) {
    warning(paste("API returned HTTP error", status))
  }

  response <- parse_api_response(r, format = api_data_url$format)
  structure(response,
            class = "api_response",
            api_data_url = api_data_url,
            status = status)
}


api_url_from_response  <- function(x) {
  attr(x, "api_data_url")$api
}

#' Parse api response
#'
#' @noRd
parse_api_response <- function(r, format) {

  if(format == "json") {
    txt <- httr::content(r, as = "text")
    if(grepl("^thl\\.pivot\\.loadDimensions\\(", txt))
      txt <- strip_dimension_callback(txt)

    x <- jsonlite::fromJSON(txt)
    return(x)
  }

  else{
    stop("Format not supported")
  }

}

#' strip the call to thl.pivot.loadDimensions from JSONP response
#'
#' @noRd
strip_dimension_callback <- function(txt) {
  txt <- gsub("^thl\\.pivot\\.loadDimensions\\(", "", txt)
  txt <- gsub("\\);\n$", "", txt)
  txt
}
