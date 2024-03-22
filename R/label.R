#' Title
#'
#' @param variable_code a character or a character vector
#' @param data a R data frame
#' @param label_database a R data frame
#' @param lang label destination language
#'
#' @import dplyr
#' @return a character or a character vector
#' @export
#'
#' @examples \dontrun{label("GDP")}

label <- function(variable_code,
                  data = label_database,
                  lang = language){

  if(length(setdiff(variable_code,data$code)) > 0){
    paste0("The vector variable_code does not match the selected database.") |>
      message_warning()
  }

  res <- variable_code |>
    as.data.frame() |>
    dplyr::left_join(data,by=c("variable_code"="code")) |>
    dplyr::pull(label) |>
    ermeeth::trad()

  res

}
