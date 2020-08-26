

#' Retrieve data from the THL open data API
#'
#' @param url An URL
#'
#' @export
getFromAPI <- function(url) {
  r <- httr::GET(url)
  status <- httr::status_code(r)

  if(status != 200) {
    warning(paste("API returned HTTP error", status))
  }

  response <- parse_api_response(r)
  structure(response,
            class = "api_response",
            url = url,
            status = status)
}



#' Parse api response
#'
#' @noRd
parse_api_response <- function(r) {

  txt <- httr::content(r, as = "text")
  if(grepl("^thl\\.pivot\\.loadDimensions\\(", txt))
    txt <- strip_dimension_callback(txt)

  x <- jsonlite::fromJSON(txt)
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
