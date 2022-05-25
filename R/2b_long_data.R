#
# ##fonciton long_data
#
# data <- readRDS("tests/oilprice_fra_c28_s32.rds")
#
# variables <- c(variables_like(data,"^Y_",view = FALSE))
# all_variables <- unique(data$variable)
# my_variables<- sample(all_variables, size = 10000)
#
# out_format = "list"
# scenarios = c("baseline","oilprice_fra")
#
#
# plop <- ermeeth::wide_data(data = data,
#                   scenarios = scenarios,
#                   variables = variables,
#                   out_format = "list")
#
# # data = ploplist
# long_data <- function(data, sector_names_name = NULL, commodity_names_table = NULL){
#
#
#   ## Checking data class and scenarios
#
#   if(class(data) == "data.frame"){
#
#     scenarios <- str_extract(names(data),"^.+\\.") %>% str_remove("\\.") %>% unique() %>% stats::na.omit()
#
#     if (length(scenarios)==0){
#       stop(message = "Could not find scenario names. Make sure you use a wide ThreeMe data.frame with scenario names attached to the variables i.e. 'baseline.GDP'.")
#     }
#
#     data_list <- purrr::set_names(scenarios)  %>%
#       purrr::map(~data %>%
#                    dplyr::select(year, dplyr::starts_with(paste0(.x,"."))) %>%
#                    dplyr::rename_all(~str_remove(.x,"^.+\\.")) ## should change to use scenario.name
#       )
#   }else{
#     if(class(data)=="list"){
#       data_list <- data
#       scenarios <- names(data_list)
#     }else{
#       stop(message = "data must be a ThreeMe data.frame or a ThreeMe data list created by wide.data.")
#     }
#   }
#
#   long_data <- data_list %>% purrr::imap(~.x  %>%
#                                     tidyr::pivot_longer(cols = !year,
#                                                         names_to = "variable") %>%
#                                       rename(!!.y := value)  ) %>%
#     purrr::reduce(full_join,by = c("year","variable")) %>%
#     mutate(sector    = as.character(NA),
#            commodity = as.character(NA))
#
#
#
#
#
#
#
# }
