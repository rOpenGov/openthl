#' @title Simple interface for interacting with API
#' @description Utilize an interactive menu in building a query.
#' 
#' @param subject Subject name, character
#' @param prod "prod" or "beta" API (beta currently unimplemented)
#' @param menu_gui use graphical menus (default: FALSE)
#' @importFrom dplyr %>% 
#' @importFrom utils select.list
#' @return Returns an URL which can be used to download data.
#' @author Pyry Kantanen
#' 
#' @examples
#' 
#' \dontrun{
#' # Explore subject "epirapo"
#' openthl_interface("epirapo")
#' # Use GUI in all menus (not recommended but functional)
#' openthl_interface(subject = "epirapo", menu_gui = TRUE)
#' }
#'
#' @export
openthl_interface <- function(subject, prod = "prod", menu_gui = FALSE) {
  # Subject can be e.g. "epirapo" --> openthl_interface("epirapo)
  # Probably in the future if subject is NULL then the function could provide a list of available subjects?
  
  # Produces a vector of available unique languages
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
  
  lang_code <- select.list(lang_choices$lang, title = message1, multiple = FALSE, graphics = menu_gui)

  # Selects datasets with preferred language
  hydra_choices <- subject %>% 
    openthl::thlSubject() %>%
    openthl::thlDatasets() %>%
    tibble::as_tibble() %>%
    dplyr::filter(lang == lang_code) 
  
  # Choose the desired hydra 
  # (or rather, a cube, since hydra is a collection of cubes with different languages)
  hydra <- select.list(choices = hydra_choices$hydra, 
                       title = "Choose a hydra", 
                       multiple = FALSE, 
                       graphics = menu_gui)
  
  # Construct an url that contains the prod, language, subject and hydra
  url_construct <- paste0(url_base(type = prod), lang_code, "/", subject, "/", hydra)
  
  # Get a singular row from hydra language options
  chosen_hydra <- openthl::thlHydra(subject, hydra) %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(grepl(url_construct, href))
  
  # The above presumes that chosen_hydra will always be a single row. Could it happen that this was not always the case?
  
  message2 <- paste0(" \n
  **************************************************************************\r
  * The following ", chosen_hydra$class, " has been chosen:\r
  * ",chosen_hydra$label,"\r
  **************************************************************************")
  message(message2)
  
  url <- chosen_hydra$href
  cube <- openthl::thlCube(url)
  dimensions <- openthl::get_dimensions(url)
  
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
                                   graphics = menu_gui, 
                                   title = message4)

  dimension_query <- NULL

 for (i in 1:length(dimensions$id)) {
   if (dimensions$id[i] %in% chosen_dimensions) {
     
     df <- openthl::parse_dimension_hierarchy(dimensions$children[[i]], parent_id = dimensions$id[[i]], format = "long")
     unique_stages <- unique(df$stage)
     chosen_labels <- select.list(choices = df$label, 
                                  title = paste("Choose a stage from dimension", dimensions$id)[i],
                                  multiple = TRUE,
                                  graphics = menu_gui)
     
    chosen_cases <- df %>% 
      dplyr::filter(label %in% chosen_labels)
    
    # If only one case chosen, ask the user if the desired outcome is to print only that information or all information on that stage
    # If > 1 cases chosen, it can be assumed (?) that the user wants information for only those cases
    # Chaining together multiple cases (444448L444116L etc) would not work anyway
    
    if (length(chosen_cases$sid) == 1){
      all_or_one <- select.list(choices = c("L", "."),
                                title = "Would you like to choose all information in your chosen stage (L) or only specific information (.)?",
                                multiple = FALSE,
                                graphics = menu_gui)
    } else {
      all_or_one <- "."
    }
    
    chosen_sids <- paste0(chosen_cases$sid, all_or_one)
    chosen_sids <- paste(chosen_sids, collapse = "")
    
    # Possibly an unneeded stage as row/column dichotomy does not matter?
    
    row_or_column <- select.list(choices = c("row", "column"), 
                                 title = "Is this a row or a column? (Think in terms of tidy data!)",
                                 multiple = FALSE,
                                 graphics = menu_gui)
  
    dimension_query[i] <- paste0(row_or_column, "=", chosen_sids, "&")
    
   } else {
     # If the name of the dimension in question was not in chosen_dimension: 
     # Do nothing, just move on to next case
   }
   
 }
  # Removes possible NA's from query
  dimension_query <- dimension_query[!is.na(dimension_query)]
  # Produces the final url that can be used to download data with e.g. rjstat::fromJSONstat
  paste(c(url, "?", dimension_query), collapse = "")
}
