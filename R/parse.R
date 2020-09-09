


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
parse_datasets <- function(x, base_url) {
  refs <- gsub(base_url, "", x$href)
  m <- matrix(unlist(strsplit(refs, split = "/")), nrow = nrow(x), byrow = TRUE)
  colnames(m) <- c("lang", "subject", "hydra", "cube")
  m[, "cube"] <- gsub("^fact_|\\.json$", "",m[, "cube"])

  df <- tibble::as_tibble(m)
  cbind(tibble::tibble(base_url), df)
}


#' @examples
#'
#'  url <- "https://sampo.thl.fi/pivot/prod/fi/toitu/ennakko3/fact_toitu_ennakko.json"
#' cube <- thlCube(url)
#' dimensions <- cube$dimensions
parse_dimensions <- function(dimensions) {

  i <- 1

  dims <- list()
  for(i in seq_along(dimensions$id)) {
    dimension_hierarchy <- getHierarchy(dimensions$children[[i]], parent_id = dimensions$id[[i]])
    dims <- c(dims, list(dimension_hierarchy))
}

}



getHierarchy <- function(stage, parent_id = NA, nstage = 0) {
  children <- stage$children
  stage$children <- NULL
  stage_id <- stage$id
  if(length(stage) == 0) return(NULL)
  stage$parent_id <- parent_id
  stage_df <- as.data.frame.dimension_stage(stage, nstage)

  if(ncol(stage_df) == 0) return(NULL)
  if(length(children) != 0) {
    newchilds <- list()
    for(i in seq_along(children)) {
      if(length(children[[i]]$stage) > 0)
        newchilds[[i]] <- getHierarchy(children[[i]], parent_id = stage_id[i], nstage = nstage + 1)
    }
    children_df <- dplyr::bind_rows(newchilds)
  } else {
    children_df <-  dplyr::bind_rows(list())
  }

  if(nrow(children_df) > 0) {
    by <- setNames(paste0("stage", nstage +1, "_parent_id"),
                   nm = paste0("stage", nstage, "_id") )
    df <- dplyr::inner_join(stage_df, children_df, by = by)
  } else {
    df <- stage_df
  }

  df
}


as.data.frame.dimension_stage <- function(stage, i) {
  stage_df <- as.data.frame(stage)
  newnames <-  paste0("stage", i, "_", colnames(stage_df))
  colnames(stage_df) <- newnames
  stage_df
}
