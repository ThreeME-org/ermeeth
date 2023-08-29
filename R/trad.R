#' Title
#'
#' @param x a character
#' @param lang
#' @param trad_data_base
#' @param lang_source
#'
#' @import tidyr
#' @return a character vector
#' @export
#'
#' @examples \dontrun{trad("Alternatives", lang = "fr")}

trad <- function(x,trad_data_base = trad_language_base,
                 lang = language, lang_source = "en"){

  if(exists("language") == FALSE){

    paste0("Please set translation destination language.") %>%
      message_warning()
  }

  trad_data <- trad_data_base
  trad_data$lang_from = trad_data_base[,lang_source]

  trad_data[which(trad_data$lang_from %in% x),lang]

}
