


parse_api_meta <- function(x) {
  df <- tibble::as_tibble(x$link$item)
  attr(df, "label") <- x$label
  attr(df, "version") <- x$version
  attr(df, "updates") <- x$updated
  attr(df, "api-class") <- x$class
  df
}

parse_subject <- function(x) {
  parse_api_meta(x)
}

parse_hydra <- function(x) {
  parse_api_meta(x)
}


#' Parse dataset hrefs into a matrix with colums lang, subject, hydra, data matrix
parse_datasets <- function(x, base_url) {
  refs <- gsub(base_url, "", x$href)
  m <- matrix(unlist(strsplit(refs, split = "/")), nrow = nrow(x), byrow = TRUE)
  colnames(m) <- c("lang", "subject", "hydra", "cube")
  m[, "cube"] <- gsub("^fact_|\\.json$", "",m[, "cube"])

  df <- tibble::as_tibble(m)
  cbind(tibble::tibble(base_url), df)
}
