#' Retrieve data from the pivot API
#'
#' @param url URL to get
#' @param format format to parse into
#'
getFromAPI <- function(url, format = "json") {
  url <- paste0(url, ".", format)
  r <- httr::GET(url)
  status <- httr::status_code(r)
  if(status != 200) {
    warning(paste("API returned HTTP error", status))
  }
  parse_api_response(r, format)
}

#' Parse api response
#'
#' @noRd
parse_api_response <- function(r, format) {

  if(format == "json") {
    x <- httr::content(r, as = "text")
    x <- jsonlite::fromJSON(x)
    return(x)
  }

  else{
    stop("Format not supported")
  }

}
