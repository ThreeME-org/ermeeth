charge_data <- function(scenario_name){
  cat("Creating database 1 / 4. Please wait.:) \n")

  data_full <- loadResults(scenario_name,
                           by_sector = FALSE, by_commodity = FALSE,
                           bridge_s = bridge_sectors, bridge_c = bridge_commodities,
                           names_s = names_sectors, names_c = names_commodities)

  cat("Creating database 2 / 4. Please wait.:) \n")

  data_ag_sector <- loadResults(scenario_name,
                                by_sector = TRUE, by_commodity = FALSE,
                                bridge_s = bridge_sectors, bridge_c = bridge_commodities,
                                names_s = names_sectors, names_c = names_commodities)

  cat("Creating database 3 / 4. Please wait.:) \n")

  data_ag_commodity <- loadResults(scenario_name,
                                   by_sector = FALSE, by_commodity = TRUE,
                                   bridge_s = bridge_sectors, bridge_c = bridge_commodities,
                                   names_s = names_sectors, names_c = names_commodities)

  cat("Creating database 4 / 4. Please wait.:) \n")

  data_ag_commodity_sector <- loadResults(scenario_name,
                                          by_sector = TRUE, by_commodity = TRUE,
                                          bridge_s = bridge_sectors, bridge_c = bridge_commodities,
                                          names_s = names_sectors, names_c = names_commodities)

  cat("Saving the databases. Almost there! \n")


  saveRDS(data_full,
          file = paste0("databases/",scenario_name,"_",classification,".rds"))


  saveRDS(data_ag_sector,
          file = paste0("databases/",scenario_name,"_",classification,"_sectors",".rds"))


  saveRDS(data_ag_commodity,
          file = paste0("databases/",scenario_name,"_",classification,"_commodities",".rds"))


  saveRDS(data_ag_commodity_sector,
          file = paste0("databases/",scenario_name,"_",classification,"_commodities_sectors",".rds"))

  cat("Done! \n")
  gc()
}

jin <- c("blank_bel","blank_fra","blank_hun","blank_lva",
         "blank_pol","blank_ita","blank_prt","blank_swe") %>%
  purrr::set_names(c("Belgium","France","Hungary","Letovia",
                     "Poland","Italy","Portugal","Sweden")) %>%
  map(~ charge_data(.x))
