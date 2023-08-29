#' Title
#'
#' @param x a charcter
#' @param lang
#' @param trad_data_base
#' @param lang_source
#'
#' @return a character vector
#' @export
#'
#' @examples trad("Alternatives", lang = "fr")

trad <- function(x,trad_data_base = trad_language_base,
                 lang = language, lang_source = "en"){

  ## Add arguments check

  trad_data <- trad_data_base
  trad_data$lang_from = trad_data_base[,lang_source]

  trad_data[which(trad_data$lang_from %in% x),lang]

  ##corriger base de donnees de langues pour remplacer accents par utf
}
