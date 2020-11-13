#' Simple interface for interacting with API
#'
#' @param subject Subject name, character
#' @param prod "prod" or "beta" API (beta currently unimplemented)
#' @importFrom utils menu
#'
#' @examples
#' 
#' \dontrun{
#' openthl_interface("epirapo")
#' }
#'
#' @export
openthl_interface <- function(subject, prod = "prod") {
  # Subject can be "epirapo" --> openthl_interface("epirapo)
  
  x1 <- openthl::thlSubject(subject)
  thl_dataset <- openthl::thlDatasets(x1)
  unique_langs <- unique(thl_dataset$lang)
  
  # Find which languages are represented in the subject / hydra
  values <- NULL
  for (i in 1:length(unique_langs)) {
    (values[i] <- paste0("(lang = dataset_lang[", i, "])"))
  }
  
  # Build a menu to choose preferred language
  lang_choice <- menu(unique_langs, title = "Choose language")
  lang <- unique_langs[lang_choice]
                        
  # Pick only datasets with preferred language
  x2 <- openthl::thlDatasets(openthl::thlSubject(subject))
  x2 <- x2[x2$lang == lang,]
  
  # After subsetting with preferred language, choose the desired hydra (or rather, cube)
  hydra_choice <- menu(x2$hydra, title = "Choose a hydra")
  hydra <- x2$hydra[hydra_choice]
  
  choice <- paste0("https://sampo.thl.fi/pivot/", prod, "/", lang, "/", subject, "/", hydra)
  hydra_table <- openthl::thlHydra(subject, hydra)
  choice_num <- grep(choice, hydra_table$href)
  message(paste("The following", hydra_table[choice_num, "class"], "has been chosen:", hydra_table[choice_num,"label"]))
  
  url <- hydra_table[choice_num, "href"]
  cube <- openthl::thlCube(url)
  
  # Messages for clarity's sake, these should probably be deleted from final version
  message(paste("The following dimensions are available in this cube:", toString(names(cube$dimensions))))
  message("Proceeding to choose stages from dimensions")
  
  dimensions <- openthl::get_dimensions(url) # list
  
  df_choice <- NULL
   # Selecting the dimensions
   # example:
   # Choose a stage from dimension dateweek2020010120201231 (this could be the label instead of stage name?)
   #
   # 1: root (these could also have labels, at least when applicable?)
   # 2: week
   # 3: date
   
 for (i in 1:length(cube$dimensions)) {
    df <- openthl::parse_dimension_hierarchy(dimensions$children[[i]], parent_id = dimensions$id[[i]], format = "long")
    df_choice[i] <- menu(unique(df$stage), title = paste("Choose a stage from dimension", names(cube$dimensions)[i]))
 }
   # Produces a vector with numbers, such as [1] 1 2 2 1 1, with 1 meaning root and 2 being the next stage, e.g. week
   df_choice
   # Still unimplemented: Going deeper into stage hierarchy, i.e. choosing only specific agegroups or sex
  
}