#' Aggregate commodities and and sectors in new divisions to reduce the database post-simulation
#'
#' @param data ThreeME result database
#' @param scenarios scenarios existing
#' @param agg_s_table which aggregation rules to use for sectors. Default is "aggregation_rules" to use the ones from aggregation_rules.xlsx
#' @param agg_c_table which aggregation rules to use for commodities. Default is "aggregation_rules" to use the ones from aggregation_rules.xlsx
#' @param by_com Boolean. Whether to aggregate by commodities. Default is TRUE
#' @param by_sec Boolean. Whether to aggregate by commodities. Default is TRUE
#' @param bridge_com Commodities bridge file  indicating how to aggregate by commodities
#' @param bridge_sec Sectors bridge file  indicating how to aggregate by sectors
#' @param exception_s_c Character strings of length 4 that start with C or S that do not refer to a sector or commodity
#' @param detailed.warnings
#'
#' @return A list of data_full with each version of reaggregated data
#'
#' @keywords internal
#'
#' @importFrom readxl read_excel
#' @importFrom readr read_csv2
#' @importFrom purrr safely imap reduce
#' @importFrom dtplyr lazy_dt
#' @import data.table dplyr stringr
#'
#' @export
#'
aggregate_com_sec <- function(data=data_full,
                              scenarios = c("baseline",scenario |> unname()),
                              agg_s_table = "aggregation_rules",
                              agg_c_table = "aggregation_rules",
                              by_com = TRUE,
                              by_sec = TRUE,
                              bridge_com = bridge_commodities,
                              bridge_sec = bridge_sectors,
                              exception_s_c = c("CONS","CONT"),
                              detailed.warnings = TRUE

){

  # data_full <- NULL
  # scenario <- NULL
  # bridge_commodities <- NULL
  # bridge_sectors <- NULL

  variable <- NULL
  sec_com <- NULL
  s_code <- NULL
  c_code <- NULL
  code <- NULL
  super_code <- NULL
  s_check <- NULL
  root <- NULL
  super_c <- NULL
  c_check <- NULL
  super_s <- NULL
  weight_var <- NULL
  weighted_mean <- NULL
  var_root <- NULL
  weight_com <- NULL
  weight_sec <- NULL
  root_com <- NULL
  tot <- NULL
  og_value <- NULL
  weight <- NULL
  test1 <- NULL
  weightedmean <- NULL
  root_sec <- NULL
  root_com_sec <- NULL
  weight_com_sec <- NULL

  # browser()
  og_data = data.table::as.data.table(data) |> dplyr::select(variable,year,dplyr::all_of(scenarios))

  # Retrieve right version of aggregation rules

  if(agg_s_table == "aggregation_rules" & agg_s_table == "aggregation_rules"){

    if(file.exists(file.path("src","bridges", paste0(agg_s_table, ".xlsx")))){
      agg_s_table <- readxl::read_excel(file.path("src","bridges", paste0(agg_s_table, ".xlsx")), sheet = "sectors")
      agg_c_table <- readxl::read_excel(file.path("src","bridges", paste0(agg_c_table, ".xlsx")), sheet = "commodities")
    }else{
      # If the aggregation rules are not changed, the program will not pass by get_remote_file()
      agg_s_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth"), sheet = "sectors")
      agg_c_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth"), sheet = "commodities")
    }

  }else{
    if(file.exists(file.path("src","bridges", paste0(agg_s_table, ".csv")))){
      agg_s_table <- readr::read_csv2(file.path("src","bridges", paste0(agg_s_table, ".csv"))) %>%
        dplyr::filter(sec_com == "sectors") %>% dplyr::select(-sec_com)
    }else{
      safe_get_remote_file <- purrr::safely(get_remote_file)
      downloader <- safe_get_remote_file(object = paste0(agg_s_table, ".csv"),
                                         destination.folder = file.path("src","bridges"))
      if(!is.null(downloader$error)){ # If the file cannot be downloaded
        message_warning("Aggregation rule cannot be downloaded, using default rule instead.")
        agg_s_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth"), sheet = "sectors")
      }else if(downloader$result){
        agg_s_table <- readr::read_csv2(file.path("src","bridges", paste0(agg_s_table, ".csv"))) %>%
          dplyr::filter(sec_com == "sectors") %>% dplyr::select(-sec_com)
      }else{ # If the file does not exist on the remote
        message_warning("Aggregation rule does not exist, using default rule instead.")
        agg_s_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth"), sheet = "sectors")
      }
    }

    if(file.exists(file.path("src","bridges", paste0(agg_c_table, ".csv")))){
      agg_c_table <- readr::read_csv2(file.path("src","bridges", paste0(agg_c_table, ".csv"))) %>%
        dplyr::filter(sec_com == "commodities") %>% dplyr::select(-sec_com)
    }else{
      safe_get_remote_file <- purrr::safely(get_remote_file)
      downloader <- safe_get_remote_file(object = paste0(agg_c_table, ".csv"),
                                         destination.folder = file.path("src","bridges"))

      if(!is.null(downloader$error)){ # If the file cannot be downloaded
        message_warning("Aggregation rule cannot be downloaded, using default rule instead.")
        agg_c_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth"), sheet = "commodities")
      }else if(downloader$result){
        agg_c_table <- readr::read_csv2(file.path("src","bridges", paste0(agg_c_table, ".csv"))) %>%
          dplyr::filter(sec_com == "commodities") %>% dplyr::select(-sec_com)
      }else{ # If the file does not exist on the remote
        message_warning("Aggregation rule does not exist, using default rule instead.")
        agg_c_table = readxl::read_excel(path = system.file("aggregation_rules.xlsx",package = "ermeeth"), sheet = "commodities")
      }
    }
  }

  if((by_com+by_sec) >0){
    ## Step 1 determine variables that need to be re-aggregated


    ##tictoc::tic("checks")
    variable_frame <- data.table::data.table(variable = og_data[,variable] |> unique() |> toupper() ,
                                 unit = 1) |>
      dplyr::filter(grepl(("^.+_(C|S)[A-Za-z0-9]{3}$") , variable) ) |>
      dplyr::mutate(root = stringr::str_remove_all(variable, "(_C[A-Za-z0-9]{3})?(_S[A-Za-z0-9]{3})?$"),
             s_code = stringr::str_extract(variable,"_(S[0-9A-Za-z]{3})$", group=1 ),
             c_code = stringr::str_extract(variable,"_(C[0-9A-Za-z]{3})(_S|$)", group=1  )
      ) |>

      ## Step 2 remove exceptionned codes
      dplyr::mutate(s_code = ifelse(s_code %in% exception_s_c ,NA ,s_code),
             c_code = ifelse(c_code %in% exception_s_c ,NA ,c_code),
             s_check = ifelse(is.na(s_code),0 , 1) ,
             c_check = ifelse(is.na(c_code),0 , 1) )|>
      dplyr::filter((is.na(s_code) + is.na(c_code) )< 2)

    ## Step 3 add super code from bridge

    fullbridge = c(bridge_com,bridge_sec) |>  purrr::imap(~data.frame(code=toupper(.x), super_code = toupper(.y)) ) |> purrr::reduce(rbind)



    variable_guide <- variable_frame |> left_join(fullbridge |> rename(s_code = code, super_s = super_code), by= "s_code") |>
      dplyr::left_join(fullbridge |> dplyr::rename(c_code = code, super_c = super_code), by= "c_code") |>
      dplyr::mutate(root_com =  ifelse(s_check == 0 ,stringr::str_c(root , super_c , sep = "_" ) , stringr::str_c(root , super_c, s_code  , sep = "_" ) ) ,
             root_sec =  ifelse(c_check == 0 ,stringr::str_c(root , super_s  ,sep = "_" ) , stringr::str_c(root , c_code , super_s , sep = "_" ) ),
             root_com_sec =  stringr::str_c(root , super_c,super_s, sep = "_" )

      )


    ## check for unmatch sectors and commodities

    unmatch_com <- variable_guide |> dplyr::select(c_code, super_c) |> dplyr::distinct() |> dplyr::filter(!is.na(c_code) & is.na(super_c)) |> dplyr::select(c_code) |> as.vector() |> unlist()
    unmatch_sec <- variable_guide |> dplyr::select(s_code, super_s) |> dplyr::distinct() |> dplyr::filter(!is.na(s_code) & is.na(super_s)) |> dplyr::select(s_code) |> as.vector() |> unlist()


    if(length(unmatch_com)>0){

      message_warning("Some commodity codes were unmateched in the bridge. Please check the bridge or the exception_s_c argument to ignore them, otherwise they will be counted as an aggregated commodity ")
      if(detailed.warnings){cat(unmatch_com, sep = "  ")}

    }

    if(length(unmatch_sec)>0){
      message_warning("Some sector codes were unmateched in the bridge. Please check the bridge or the exception_s_c argument to ignore them, otherwise they will be counted as an aggregated sector.")
      if(detailed.warnings){cat(unmatch_sec, sep = "  ")}
    }



    cat("\n")

    ## adding supergroup ingo
    disaggregated_data <- data.table::as.data.table(og_data  |> dplyr::left_join(variable_guide |> dplyr::select(-unit), by="variable") |> dplyr::rename(var_root = root))


    com_data <- disaggregated_data[c_check==1,]  |> dplyr::left_join(agg_c_table, by = "var_root") |>
      dplyr::mutate( weight_com =   ifelse(s_check == 0 ,stringr::str_c(weight_var , c_code , sep = "_" ) , stringr::str_c(weight_var , c_code, s_code  , sep = "_" ) ) ,
              weight_sec = NA_character_)

    sec_data <- disaggregated_data[s_check==1,] |> left_join(agg_s_table, by = "var_root")|>
      dplyr::mutate( weight_sec =   ifelse(c_check == 0 ,stringr::str_c(weight_var , s_code , sep = "_" ) , stringr::str_c(weight_var , c_code, s_code  , sep = "_" ) ) ,
              weight_com = NA_character_)


    gen_data <- disaggregated_data[is.na(s_check) & is.na(c_check) ,] |>
      dplyr::mutate(sum = NA, mean = NA , weighted_mean = NA , weight_var = NA , weight_com =NA, weight_com = NA , weight_sec = NA, s_check = 0,  c_check = 0 )

    ## aggregation rule check : if no method is given, revert to normal mean

    ### for commodities
    com_data_agg_check <- com_data[is.na(sum) & is.na(mean) & is.na(weighted_mean),list(sum, mean,weighted_mean,var_root)] |> dplyr::distinct()

    if(nrow(com_data_agg_check) >0 ){check_agg1 <-FALSE}else{check_agg1 <-TRUE}
    if (check_agg1 == FALSE ){
      message_warning("Some commodity variables do not have any assigned method for aggregation, simple mean will be used. To modify this, specify the aggregation in rule in the rules database.")
      if(detailed.warnings){cat(com_data_agg_check$var_root, sep = "  ")}
      cat("\n")

      com_data[is.na(sum) & is.na(mean) & is.na(weighted_mean), `:=`(sum = 0, mean = 1, weighted_mean = 0) ]

    }

    ### for sectors
    sec_data_agg_check <- sec_data[is.na(sum) & is.na(mean) & is.na(weighted_mean),list(sum, mean,weighted_mean,var_root)] |> dplyr::distinct()

    if(nrow(sec_data_agg_check) >0 ){check_agg2 <-FALSE}else{check_agg2 <-TRUE}
    if (check_agg2 == FALSE ){
      message_warning("Some sectors variables do not have any assigned method for aggregation, simple mean will be used. To modify this, specify the aggregation in rule in the rules database.\n")
      if(detailed.warnings){cat(sec_data_agg_check$var_root, sep = "  ")}
      cat("\n")

      sec_data[is.na(sum) & is.na(mean) & is.na(weighted_mean), `:=`(sum = 0, mean = 1, weighted_mean = 0) ]

    }

    ## weighted means: 2 checks
    ### - first if wmean== 1 and no weight var given, revert to normal mean
    #### for commodities
    com_data_agg_check2 <- com_data[weighted_mean == 1 & is.na(weighted_mean), list(var_root)] |> dplyr::distinct()

    if(nrow(com_data_agg_check2) >0 ){check_agg3<-FALSE}else{check_agg3 <-TRUE}
    if (check_agg3 == FALSE ){
      message_warning("No weight variable specified for these commodity variables that are supposed to be aggregated by weighted mean. They will be aggregated by simple mean instead.\n")
      if(detailed.warnings){cat(com_data_agg_check2$var_root, sep = "  ")}
      cat("\n")

      com_data[weighted_mean == 1 & is.na(weighted_mean), `:=`(sum = 0, mean = 1, weighted_mean = 0) ]

    }

    #### for sectors
    sec_data_agg_check2 <- sec_data[weighted_mean == 1 & is.na(weighted_mean), list(var_root)] |> dplyr::distinct()

    if(nrow(sec_data_agg_check2) >0 ){check_agg4<-FALSE}else{check_agg4 <-TRUE}
    if (check_agg4 == FALSE ){
      message_warning("No weight variable specified for these sector variables that are supposed to be aggregated by weighted mean. They will be aggregated by simple mean instead.")
      if(detailed.warnings){cat(sec_data_agg_check2$var_root, sep = "  ")}
      cat("\n")

      sec_data[weighted_mean == 1 & is.na(weighted_mean), `:=`(sum = 0, mean = 1, weighted_mean = 0) ]

    }


    ### - second  if wmean== 1 and weight var given does not exist, revert to normal mean

    com_wmean_data <- com_data[weighted_mean == 1,]
    weight_vars <- com_wmean_data$weight_com |> unique()
    test_weight_vars <- base::setdiff(weight_vars,variable_guide$variable)
    problem_vars <- com_wmean_data[which(weight_com %in% test_weight_vars),"variable"]  |> dplyr::distinct()

    if (length(test_weight_vars) >0){
      message_warning("Some weight variables are not present in the database, the corresponding variables will be aggregated through a simple mean instead. ")
      if(detailed.warnings){cat("\nHere's a summary of the variables :\n")

        print(com_wmean_data[which(weight_com %in% test_weight_vars),c("variable", "weight_com")]  |> dplyr::distinct())
      }
      cat("\n")

      com_data[weighted_mean == 1 & weight_com %in% test_weight_vars, `:=`(sum =0 , mean = 1 , weighted_mean = 0 , weight_com = NA ,  weight_var = NA )]
      com_wmean_data <- com_data[weighted_mean == 1,]

    }


    sec_wmean_data <- sec_data[weighted_mean == 1,]
    weight_vars <- sec_wmean_data$weight_sec |> unique()
    test_weight_vars <- base::setdiff(weight_vars,variable_guide$variable)
    problem_vars <- sec_wmean_data[which(weight_sec %in% test_weight_vars),"variable"]  |> dplyr::distinct()

    if (length(test_weight_vars) >0){
      message_warning("Some weight variables are not present in the database, the corresponding variables will be aggregated through a simple mean instead. ")
      if(detailed.warnings){cat("\nHere's a summary of the variables :\n")

        print(sec_wmean_data[which(weight_sec %in% test_weight_vars),c("variable", "weight_sec")]  |> dplyr::distinct())
      }
      cat("\n")

      sec_data[weighted_mean == 1 & weight_sec %in% test_weight_vars, `:=`(sum =0 , mean = 1 , weighted_mean = 0 , weight_sec = NA ,  weight_var = NA )]
      sec_wmean_data <- sec_data[weighted_mean == 1,]

    }
    ##tictoc::toc()

  }




  #### Commodities only

  if (by_com == TRUE){
    ##tictoc::tic("commodity aggregation")
    com_sum_data  <- com_data[sum == 1,][,lapply(.SD, sum), .SDcols = scenarios, by = list(root_com,year)  ]  |>
      dplyr::rename(variable = root_com)
    com_mean_data <- com_data[mean == 1,][,lapply(.SD, mean), .SDcols = scenarios, by = list(root_com,year)  ]  |>
      dplyr::rename(variable = root_com)

    ## weighted mean method
    ### step 1 compute the weighted mean var

    ## get the variables where we get the weight from
    weight_com_vars <- com_wmean_data$weight_com |>  unique()

    data_weight_com <- disaggregated_data[variable %in% weight_com_vars,] |>
      dplyr::select(variable,year,dplyr::all_of(scenarios) ,root_com) |>
      data.table::melt(id.vars =c("variable","year","root_com"),
           measure.vars = scenarios,
           variable.name = "scenario",variable.factor = FALSE,
           value.name = "og_value")

    weights_com <- data_weight_com[,tot := sum(og_value),by = list(root_com,year,scenario)][,weight := (og_value/tot)][, test1 := sum(weight),by = list(root_com, year, scenario)] |>
      dplyr::rename(weight_com = variable) |> dplyr::select(weight_com, year, scenario,weight,test1)

    value_com <- com_wmean_data |> dplyr::select(variable, year, dplyr::all_of(scenarios),weight_com,root_com) |>
      data.table::melt(id.vars =c("variable","year","root_com","weight_com"),
           measure.vars = scenarios,
           variable.name = "scenario",variable.factor = FALSE,
           value.name = "og_value")

    compute_com_res_wmean <- data.table::merge.data.table(value_com,weights_com,by= c("scenario","year","weight_com")) |>
      dtplyr::lazy_dt() |>
      dplyr::mutate(weightedmean = ifelse(is.nan(test1) == 0,
                                   og_value*weight,
                                   og_value) ) |>
      data.table::as.data.table()

    com_weightmean_data <- compute_com_res_wmean[,weightedmean := mean(og_value), by=list(scenario,root_com,year)] |> dplyr::select(root_com, year, scenario,weightedmean) |> dplyr::distinct() |>
      data.table::dcast(root_com+ year ~ scenario , value.var = "weightedmean", fun.aggregate =mean) |>
      dplyr::rename(variable = root_com)

    ### End here
    com_agg_data <- rbind(com_mean_data,com_sum_data,com_weightmean_data ,
                          (disaggregated_data[s_check ==1 & c_check == 0,] |> dplyr::select(variable, year, dplyr::all_of(scenarios))),
                          (gen_data |> dplyr::select(variable, year, dplyr::all_of(scenarios))) )


    rm(com_weightmean_data ,compute_com_res_wmean,value_com,weights_com, data_weight_com, weight_com_vars, com_mean_data,com_sum_data)
    ##tictoc::toc()
  }else{
    com_agg_data <- NULL
  }

  if (by_sec == TRUE){

    ##tictoc::tic("sector aggregation")
    sec_sum_data  <- sec_data[sum == 1,][,lapply(.SD, sum), .SDcols = scenarios, by = list(root_sec,year)  ]  |>
      dplyr::rename(variable = root_sec)
    sec_mean_data <- sec_data[mean == 1,][,lapply(.SD, mean), .SDcols = scenarios, by = list(root_sec,year)  ]  |>
      dplyr::rename(variable = root_sec)

    ## weighted mean method
    ### step 1 compute the weighted mean var

    ## get the variables where we get the weight from
    weight_sec_vars <- sec_wmean_data$weight_sec |>  unique()

    data_weight_sec <- disaggregated_data[variable %in% weight_sec_vars,] |>
      dplyr::select(variable,year,dplyr::all_of(scenarios) ,root_sec) |>
      data.table::melt(id.vars =c("variable","year","root_sec"),
           measure.vars = scenarios,
           variable.name = "scenario",variable.factor = FALSE,
           value.name = "og_value")

    weights_sec <- data_weight_sec[,tot := sum(og_value),by = list(root_sec,year,scenario)][,weight := (og_value/tot)][, test1 := sum(weight),by = list(root_sec, year, scenario)] |>
      dplyr::rename(weight_sec = variable) |> dplyr::select(weight_sec, year, scenario,weight,test1)

    value_sec <- sec_wmean_data |> dplyr::select(variable, year, dplyr::all_of(scenarios),weight_sec,root_sec) |>
      data.table::melt(id.vars =c("variable","year","root_sec","weight_sec"),
           measure.vars = scenarios,
           variable.name = "scenario",variable.factor = FALSE,
           value.name = "og_value")

    compute_sec_res_wmean <- data.table::merge.data.table(value_sec,weights_sec,by= c("scenario","year","weight_sec")) |>
      dtplyr::lazy_dt() |>
      dtplyr::mutate(weightedmean = ifelse(is.nan(test1) == 0,
                                   og_value*weight,
                                   og_value) ) |>
      data.table::as.data.table()

    sec_weightmean_data <- compute_sec_res_wmean[,weightedmean := mean(og_value), by=list(scenario,root_sec,year)] |> dplyr::select(root_sec, year, scenario,weightedmean) |> dplyr::distinct() |>
      data.table::dcast(root_sec+ year ~ scenario , value.var = "weightedmean", fun.aggregate =mean) |>
      dplyr::rename(variable = root_sec)

    ### End here
    sec_agg_data <- rbind(sec_mean_data,sec_sum_data,sec_weightmean_data ,
                          disaggregated_data[s_check ==1 & c_check == 0,] |> dplyr::select(variable, year, dplyr::all_of(scenarios)),
                          gen_data |> dplyr::select(variable, year, dplyr::all_of(scenarios)) )

    rm(sec_weightmean_data ,compute_sec_res_wmean,value_sec,weights_sec, data_weight_sec, weight_sec_vars, sec_mean_data,sec_wmean_data,sec_sum_data)
    ##tictoc::toc()
  }else{
    sec_agg_data <- NULL
  }


  if(by_com + by_sec == 2){
    ## total aggregation
    ## Start with com agg data to aggregate sectors
    ##tictoc::tic("commodity and sector aggregation")

    com_sec_var_guide <- variable_guide |>
      dplyr::mutate(variable = ifelse(c_check == 1 ,root_com ,variable),
             root_com_sec = ifelse(c_check == 0 & s_check == 1,stringr::str_c(root,super_s,sep="_"),root_com_sec),
             root_com_sec = ifelse(s_check == 0 & c_check == 1,stringr::str_c(root,super_c,sep="_"),root_com_sec)) |>
      dplyr::select(-root_sec,-c_code) |> dplyr::distinct()

    new_disagg <- dplyr::left_join(com_agg_data,com_sec_var_guide, by= "variable")

    ok_data <- new_disagg[s_check == 0 |is.na(s_check),]
    to_treat_data <- new_disagg[s_check == 1,]


    sec_data_2 <- dplyr::select(sec_data, variable,root_sec,root_com,root_com_sec, super_c, super_s,s_code, sum, mean,weighted_mean, weight_var,weight_com_sec= weight_sec ) |> dplyr::distinct() |>
      dplyr:: mutate(root_com_sec = ifelse(is.na(root_com_sec),root_sec,root_com_sec ),
             variable =  ifelse(is.na(root_com),variable,root_com ),
             weight_com_sec = ifelse(weighted_mean == 1 & !is.na(root_com),stringr::str_c(weight_var,super_c,s_code, sep = "_") ,weight_com_sec)
      ) |>
      dplyr::select(-root_sec, -root_com,-root_com_sec,-super_c,-super_s,-s_code)|> dplyr::distinct()
    # |> mutate(sec_data2 = 1, dupes = duplicated(variable)) |>
    #group_by(variable) |> mutate(count =sum(sec_data2) , n = max(count)) |> ungroup() |> filter(n>1)

    com_sec_base <- to_treat_data |> left_join(sec_data_2 ,by="variable")

    ######

    com_sec_sum_data  <- com_sec_base[sum == 1,][,lapply(.SD, sum), .SDcols = scenarios, by = list(root_com_sec,year)  ]  |>
      dplyr::rename(variable = root_com_sec)
    com_sec_mean_data <- com_sec_base[mean == 1,][,lapply(.SD, mean), .SDcols = scenarios, by = list(root_com_sec,year)  ]  |>
      dplyr::rename(variable = root_com_sec)
    com_sec_wmean_data <- com_sec_base[weighted_mean == 1,]
    ## weighted mean method
    ### step 1 compute the weighted mean var
    ## get the variables where we get the weight from
    weight_com_sec_vars <- com_sec_base$weight_com_sec |>  unique()


    data_weight_com_sec <- new_disagg[variable %in% weight_com_sec_vars,] |>

      dplyr::select(variable,year,dplyr::all_of(scenarios) ,root_com_sec) |>
      data.table::melt(id.vars =c("variable","year","root_com_sec"),
           measure.vars = scenarios,
           variable.name = "scenario",variable.factor = FALSE,
           value.name = "og_value")

    weights_com_sec <- data_weight_com_sec[,tot := sum(og_value),by = list(root_com_sec,year,scenario)][,weight := (og_value/tot)][, test1 := sum(weight),by = list(root_com_sec, year, scenario)] |>
      dplyr::rename(weight_com_sec = variable) |> dplyr::select(weight_com_sec, year, scenario,weight,test1)

    value_com_sec <- com_sec_wmean_data |> dplyr::select(variable, year, dplyr::all_of(scenarios),weight_com_sec,root_com_sec) |>
      data.table::melt(id.vars =c("variable","year","root_com_sec","weight_com_sec"),
           measure.vars = scenarios,
           variable.name = "scenario",variable.factor = FALSE,
           value.name = "og_value")

    compute_com_sec_res_wmean <- data.table::merge.data.table(value_com_sec,weights_com_sec,by= c("scenario","year","weight_com_sec")) |>
      data.table::lazy_dt() |>
      dplyr::mutate(weightedmean = ifelse(is.nan(test1) == 0,
                                   og_value*weight,
                                   og_value) ) |>
      data.table::as.data.table()

    com_sec_weightmean_data <- compute_com_sec_res_wmean[,weightedmean := mean(og_value), by=list(scenario,root_com_sec,year)] |> dplyr::select(root_com_sec, year, scenario,weightedmean) |> dplyr::distinct() |>
      data.table::dcast(root_com_sec+ year ~ scenario , value.var = "weightedmean", fun.aggregate =mean) |>
      dplyr::rename(variable = root_com_sec)

    ### End here
    com_sec_agg_data <- rbind(com_sec_mean_data,com_sec_sum_data,com_sec_weightmean_data ,
                              ok_data |> dplyr::select(variable, year, dplyr::all_of(scenarios))
    )

    rm(com_sec_weightmean_data ,compute_com_sec_res_wmean,value_com_sec,weights_com_sec, data_weight_com_sec, weight_com_sec_vars, com_sec_mean_data,com_sec_wmean_data,com_sec_sum_data)
    ##tictoc::toc()
    #####

  }else{
    com_sec_agg_data <- NULL
  }
  res <- list(og_data,com_agg_data,sec_agg_data,com_sec_agg_data)

}
