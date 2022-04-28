## Small and useful functions

#' Title
#'
#' @param data A ThreeMe data.frame in long format.
#' @param test A regular expression to test for matching variables.
#' @param view If TRUE (default) results will be displayed in a separate window.
#'
#' @import dplyr utils
#' @return A data.frame containing the variables matching the test expression, as well as sectors and commodities corresponding to these variables where applicable.
#' @export
#'
variables_like <- function(data,test,view = TRUE){
  VariableShow <- data %>% dplyr::filter(grepl(test, variable)) %>% select(variable,sector,commodity) %>% unique()
  if(view == TRUE)
    {
      View(VariableShow)
    }else
    {
      VariableShow
    }

}

# variables_like(data, "^EMS_CI")
