#' Simple interface for interacting with API
#'
#' @param subject Subject name, character
#' @param prod "prod" or "beta" API (beta currently unimplemented)
#' @importFrom utils menu
#' @infortFrom dplyr %>% 
#'
#' @examples
#' 
#' \dontrun{
#' openthl_interface("epirapo")
#' }
#'
#' @export
openthl_interface <- function(subject, prod = "prod") {
  # Subject can be e.g. "epirapo" --> openthl_interface("epirapo)
  # Probably in the future if subject is NULL then the function could provide a list of available subjects?
  
  lang_choices <- subject %>% 
    openthl::thlSubject() %>% 
    openthl::thlDatasets() %>%
    tibble::as_tibble() %>% 
    dplyr::select(lang) %>% 
    unique()
  
  message1 <- (" \n
  **************************************************************************\r
  * Select preferred language\r
  * en: English; fi: Finnish; sv: Swedish\r
  **************************************************************************")
  
  lang_code <- select.list(lang_choices$lang, title = message1, multiple = FALSE, graphics = FALSE)

  # Pick only datasets with preferred language
  hydra_choices <- subject %>% 
    openthl::thlSubject() %>%
    openthl::thlDatasets() %>%
    tibble::as_tibble() %>%
    dplyr::filter(lang == lang_code) 
  
  # Choose the desired hydra 
  # (or rather, a cube, since hydra is a collection of cubes with different languages and the language was already chosen?)
  hydra <- select.list(choices = hydra_choices$hydra, 
                       title = "Choose a hydra", 
                       multiple = FALSE, 
                       graphics = FALSE)
  
  # Construct an url that contains the prod, language, subject and hydra
  url_construct <- paste0("https://sampo.thl.fi/pivot/", prod, "/", lang_code, "/", subject, "/", hydra)
  
  chosen_hydra <- openthl::thlHydra(subject, hydra) %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(stringr::str_detect(href, url_construct))
  
  # Old way of doing things
  # hydra_table <- openthl::thlHydra(subject, hydra)
  # row_num <- grep(url_construct, hydra_table$href)
  # chosen_hydra <- hydra_table[row_num,]
  
  message2 <- paste0(" \n
  **************************************************************************\r
  * The following ", chosen_hydra$class, " has been chosen:\r
  * ",chosen_hydra$label,"\r
  **************************************************************************")
  message(message2)
  
  url <- chosen_hydra$href
  cube <- openthl::thlCube(url)
  dimensions <- openthl::get_dimensions(url) # list
  
  message3 <- paste0(" \n
  **************************************************************************\r
  * The following dimensions are available in this cube:\r
  * ",toString(names(cube$dimensions)), " \r
  * With the following labels: \r
  * ",toString(dimensions$label), "\r
  * Proceeding to choose stages from these dimensions\r
  **************************************************************************")
  message(message3)
  
  message4 <- paste0(" \n
  **************************************************************************\r
  * Choose which dimensions you would like to use\r
  * If you choose none, the API query will be done with no parameters \r
  * (meaning: output nothing after ? in the query url)\r
  **************************************************************************")
  
  chosen_dimensions <- select.list(choices = dimensions$id, 
                                   multiple = TRUE, 
                                   graphics = FALSE, 
                                   title = message4)

  dimension_query <- NULL

 for (i in 1:length(dimensions$id)) {
   if (dimensions$id[i] %in% chosen_dimensions) {
     
     df <- openthl::parse_dimension_hierarchy(dimensions$children[[i]], parent_id = dimensions$id[[i]], format = "long")
     unique_stages <- unique(df$stage)
     chosen_labels <- select.list(choices = df$label, 
                                  title = paste("Choose a stage from dimension", dimensions$id)[i],
                                  multiple = TRUE,
                                  graphics = FALSE)
     
    chosen_cases <- df %>% 
      dplyr::filter(label %in% chosen_labels)
    
    if (length(chosen_ids$sid) == 1){
      all_or_one <- select.list(choices = c("L", "\\."),
                                title = "Would you like to choose all information in your chosen stage (L) or only specific information (.)?",
                                multiple = FALSE,
                                graphics = FALSE)
    } else {
      all_or_one <- "."
    }
    
    chosen_sids <- paste0(chosen_cases$sid, all_or_one)
    chosen_sids <- paste(chosen_sids, collapse = "")
    
    row_or_column <- select.list(choices = c("row", "column"), 
                                 title = "Is this a row or a column? (Think in terms of tidy data!)",
                                 multiple = FALSE,
                                 graphics = FALSE)
  
    dimension_query[i] <- paste0(row_or_column, "=", chosen_sids, "&")
    
   } else {
     # If the name of the dimension in question was not in chosen_dimension: 
     # Do nothing, just move on to next case
   }
   
  }
  paste(c(url, "?", dimension_query), collapse = "")
}
