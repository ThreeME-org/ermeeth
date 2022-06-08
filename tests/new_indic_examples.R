library(tidyverse)
load("tests/newindicators.rda")

data_base <- new_indicators

variables_selection <-   c(  variables_like(data_base,"^EMS_CI_C0",FALSE),
                             variables_like(data_base,"^EMS_CH_C0",FALSE),
                             "^EMS_CI$", "^EMS_CH$"  )

calcul_indic <- function(data){
  res <- data %>%      mutate(

    EMS_CI_CH = EMS_CI + EMS_CH,
    EMS_CI_CH_C001 = EMS_CI_C001 + EMS_CH_C001,
    EMS_CI_CH_C002 = EMS_CI_C002 + EMS_CH_C002,
    EMS_CI_CH_C003 = EMS_CI_C003 + EMS_CH_C003,

    # EMS_CI_C004 = 0,
    # EMS_CH_C004 = 0,
    # EMS_CI_CH_C004 = EMS_CI_C004 + EMS_CH_C004,
    #
    # EMS_CI_C005 = 0,
    # EMS_CH_C005 = 0,
    # EMS_CI_CH_C005 = EMS_CI_C005 + EMS_CH_C005,
    #
    # EMS_CI_C006 = 0,
    # EMS_CH_C006 = 0,
    # EMS_CI_CH_C006 = EMS_CI_C006 + EMS_CH_C006,
    EMS_CI_CH_C007 = EMS_CI_C007 + EMS_CH_C007,
    EMS_CI_CH_C008 = EMS_CI_C008 + EMS_CH_C008
  )


}

data <-










new_indicators <- new_indicators_bis %>%
  # 1. Put in Wide format
  wide_data(variables = variables_selection , out_format = "list") %>%  ## 1. passe en wide
  # 2. Calculate indicators
  map(~calcul_indic(.x)) %>%
  long_data()
