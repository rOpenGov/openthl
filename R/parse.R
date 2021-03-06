


parse_api_meta <- function(x) {
  df <- tibble::as_tibble(x$link$item)
  attr(df, "label") <- x$label
  attr(df, "version") <- x$version
  attr(df, "updated") <- x$updated
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
#'
#' @param x a list/data.frame with element named 'href'.
#' @param base_url base URL of the API. See url_base()
parse_datasets <- function(x, base_url) {
  refs <- gsub(base_url, "", x$href)
  m <- matrix(unlist(strsplit(refs, split = "/")), nrow = nrow(x), byrow = TRUE)
  colnames(m) <- c("lang", "subject", "hydra", "cube")
  m[, "cube"] <- gsub("^fact_|\\.json$", "",m[, "cube"])

  df <- tibble::as_tibble(m)
  cbind(tibble::tibble(base_url), df)
}


#' Parse dimensions
#'
#' @param dimensions Object returned by openthl:::get_dimensions.
#'
#' @author Tuomo Nieminen
#'
#' @examples
#'
#' url <- "https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.json"
#' dimensions <- openthl:::get_dimensions(url)
#' x <- openthl:::parse_dimensions(dimensions)
#' names(x)
#' str(x[[1]])
parse_dimensions <- function(dimensions) {

  dims <- list()
  for(i in seq_along(dimensions$id)) {
    dimension_df <- getHierarchy(dimensions$children[[i]], parent_id = dimensions$id[[i]])
    class(dimension_df) <- c("hydra_dimension_df", class(dimension_df))
    dims <- c(dims, list(dimension_df))
  }
  names(dims) <- dimensions$id
  class(dims) <- "hydra_dimensions"
  dims
}


#' Dimension hierarchy as a data frame
#'
#' @param stage a hierarchical list with dimension stage and it's children.
#' @param parent_id Id of the parent of the stage.
#' @param nstage The depth of the stage relative to the root (which is 0).
#'
#' Recursively retrieve all children of the dimension and flatten as a single data frame.
#' Helper for parse_dimensions()
#'
#' @note Kiitos HY TIRA-kurssi 2013
#'
#' @author Tuomo Nieminen
#'
#' @examples
#'
#' url <- "https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.json"
#' dimensions <- openthl:::get_dimensions(url)
#'
#' df <- openthl:::getHierarchy(dimensions$children[[1]], parent_id = dimensions$id[[1]])
#' str(df)
getHierarchy <- function(stage, parent_id = NA, nstage = 0) {

  children <- stage$children
  stage$children <- NULL

  # add id of parent
  stage$parent_id <- parent_id

  # convert to df, rename columns names by appending paste0('stage_',nstage)
  if(nstage > 0) # if not root
    stage_df <- as.data.frame.dimension_stage(stage, nstage)

  # keep record of number of stages
  deepest_stage <- nstage

  # recursively retrieve children
  newchilds <- list()
  for(i in seq_along(children)) {
    if(length(children[[i]]) > 0) {
      newchilds[[i]] <- getHierarchy(children[[i]], parent_id = stage$id[i], nstage = nstage + 1)
      deepest_stage <- attr(newchilds[[1]], "nstage")
    }
  }
  children_df <- dplyr::bind_rows(newchilds)

  # join parent and child using parent id
  if(nrow(children_df) > 0) {
    if(nstage >0) {
    by <- stats::setNames(paste0("stage", nstage +1, "_parent_id"),
                   nm = paste0("stage", nstage, "_id") )
    df <- dplyr::inner_join(stage_df, children_df, by = by)
    } else {
      df <- children_df
    }
  } else {
    df <- stage_df
  }

  # add root as attribute
  if(nstage == 0)
    attr(df, "root") <- stage

  # return parent and children data.frame
  attr(df, "nstage") <- max(nstage, deepest_stage)
  df
}


as.data.frame.dimension_stage <- function(stage, i) {
  stage_df <- as.data.frame(stage)
  properties <- stage_df$properties # may include
  stage_df$properties <- NULL
  if(!is.null(properties))
    stage_df <- cbind(stage_df, properties)
  newnames <-  paste0("stage", i, "_", colnames(stage_df))
  colnames(stage_df) <- newnames
  stage_df
}
