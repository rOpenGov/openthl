

#' Retrieve data from the THL open data API
#'
#' @param url An URL
#' @param type 'meta' uses jsonlite package for response content parsing,
#' 'data' uses rjstat.
#'
#' @export
getFromAPI <- function(url, type = c("meta", "data")) {
  r <- httr::GET(url)
  status <- httr::status_code(r)

  if(status != 200) {
    warning(paste("API returned HTTP error", status))
  }

  type <- match.arg(type)
  response <- parse_api_response(r, type = type)
  structure(response,
            class = "api_response",
            url = url,
            status = status)
}



#' Parse api response
#'
#' @noRd
parse_api_response <- function(r, type = c("meta", "data")) {

  txt <- httr::content(r, as = "text")
  if(grepl("^thl\\.pivot\\.loadDimensions\\(", txt))
    txt <- strip_dimension_callback(txt)

  type <- match.arg(type)
  x <- switch(type,
              "meta"= jsonlite::fromJSON(txt),
              "data" = rjstat::fromJSONstat(url))
  return(x)


}

#' strip the call to thl.pivot.loadDimensions from JSONP response
#'
#' @noRd
strip_dimension_callback <- function(txt) {
  txt <- gsub("^thl\\.pivot\\.loadDimensions\\(", "", txt)
  txt <- gsub("\\);\n$", "", txt)
  txt
}
