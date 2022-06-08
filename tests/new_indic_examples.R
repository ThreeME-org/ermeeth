library(tidyverse)
load("tests/newindicators.rda")
scenario_name = "carbontax_lux"


variables_selection <-   c(  variables_like(new_indicators_bis,"^EMS_CI_C0",FALSE),
                             variables_like(new_indicators_bis,"^EMS_CH_C0",FALSE),
                             "EMS_CI", "EMS_CH"  )

list_data <- new_indicators_bis %>%
  # 1. Put in Wide format
  wide_data(variables = variables_selection , out_format = "list")

data <- list_data$baseline

## New Version








#
#
# #### Old Version
# calcul_indic <- function(data){
#   res <- data %>%      mutate(
#
#     EMS_CI_CH = EMS_CI + EMS_CH,
#     EMS_CI_CH_C001 = EMS_CI_C001 + EMS_CH_C001,
#     EMS_CI_CH_C002 = EMS_CI_C002 + EMS_CH_C002,
#     EMS_CI_CH_C003 = EMS_CI_C003 + EMS_CH_C003,
#
#     # EMS_CI_C004 = 0,
#     # EMS_CH_C004 = 0,
#     # EMS_CI_CH_C004 = EMS_CI_C004 + EMS_CH_C004,
#     #
#     # EMS_CI_C005 = 0,
#     # EMS_CH_C005 = 0,
#     # EMS_CI_CH_C005 = EMS_CI_C005 + EMS_CH_C005,
#     #
#     # EMS_CI_C006 = 0,
#     # EMS_CH_C006 = 0,
#     # EMS_CI_CH_C006 = EMS_CI_C006 + EMS_CH_C006,
#     EMS_CI_CH_C007 = EMS_CI_C007 + EMS_CH_C007,
#     EMS_CI_CH_C008 = EMS_CI_C008 + EMS_CH_C008
#   )
#
#
# }
#
# new_indicators <-  new_indicators_bis %>%
#   # 1. Put in Wide format
#   wide_data(variables = variables_selection , out_format = "list")  %>%
#   # 2. Calculate indicators
#   map(~calcul_indic(.x)) %>%
#   long_data()
# ####
