


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
#' @param format One of 'wide', 'long'. Controls whether dimension
#' data.frames are parsed in long or wide format. Default is 'wide' (may change).
#'
#' @author Tuomo Nieminen
#'
#' @examples
#'
#' url <- "https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.json"
#' dimensions <- openthl:::get_dimensions(url)
#' x1 <- openthl:::parse_dimensions(dimensions)
#' names(x1)
#' str(x1[[1]])
#'
#' x2 <- openthl:::parse_dimensions(dimensions, format = "long")
#' names(x2)
#' str(x2[[1]])
parse_dimensions <- function(dimensions, format = c("wide", "long")) {

  dims <- list()
  format <- match.arg(format)
  for(i in seq_along(dimensions$id)) {
    dimension_df <- parse_dimension_hierarchy(dimensions$children[[i]],
                                              parent_id = dimensions$id[[i]],
                                              format = format)
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
#' df <- openthl:::parse_dimension_hierarchy(dimensions$children[[1]], parent_id = dimensions$id[[1]])
#'
#' df_long <- openthl:::parse_dimension_hierarchy(dimensions$children[[1]], parent_id = dimensions$id[[1]], format = "long")
#'
#' str(df)
#' str(df_long)
parse_dimension_hierarchy <- function(stage, parent_id = NA, nstage = 0, format = c("wide", "long")) {

  format <- match.arg(format)

  children <- stage$children
  stage$children <- NULL

  # add id of parent
  stage$parent_id <- parent_id

  # convert to df if not root
  # for the long format convert also root
  if(nstage > 0 || format == "long")
    stage_df <- as.data.frame.dimension_stage(stage, nstage, format = format)

  # keep track of the number of stages
  deepest_stage <- nstage

  # recursively retrieve information for all children
  newchilds <- list()
  for(i in seq_along(children)) {
    if(length(children[[i]]) > 0) {
      newchilds[[i]] <- parse_dimension_hierarchy(children[[i]],
                                                  parent_id = stage$id[i],
                                                  nstage = nstage + 1,
                                                  format = format)
      deepest_stage <- attr(newchilds[[1]], "nstage")
    }
  }
  # rowbind the children
  children_df <- dplyr::bind_rows(newchilds)

  # combine the parent and children
  df <- merge_stages(stage_df = stage_df, children_df = children_df, nstage = nstage, format = format)

  # add root as attribute to the wide data
  if(nstage == 0 && format == "wide")
    attr(df, "root") <- stage

  if(nstage == 0) {
    attr(df, "dimension_id") <- parent_id
    attr(df, "format") <- format
  }

  # keep track of the maximum stage depth reached
  attr(df, "nstage") <- max(nstage, deepest_stage)

  # return the data for the parent and all children
  df
}

merge_stages <- function(stage_df, children_df, nstage, format = c("wide", "long")) {

  format <- match.arg(format)

  if(nrow(children_df) > 0) {
    if(format == "long") {
      df <- dplyr::bind_rows(stage_df, children_df)
    } else  { # format wide
      if(nstage > 0) {
        by <- stats::setNames(paste0("stage", nstage +1, "_parent_id"),
                              nm = paste0("stage", nstage, "_id") )
        df <- dplyr::inner_join(stage_df, children_df, by = by)
      } else { # format wide and root (nstage = 0)
        df <- children_df
      }
    }
  } else { # no children
    df <- stage_df
  }
  df
}

as.data.frame.dimension_stage <- function(stage, i, format = c("wide", "long")) {

  format <- match.arg(format)
  stage_df <- as.data.frame(stage)
  properties <- stage_df$properties # may include
  stage_df$properties <- NULL
  if(!is.null(properties))
    stage_df <- cbind(stage_df, properties)

  if(format == "wide") {
    newnames <-  paste0("stage", i, "_", colnames(stage_df))
    colnames(stage_df) <- newnames
  } else { # format  = long
    stage_df$stage_depth <- i
  }
  stage_df
}
