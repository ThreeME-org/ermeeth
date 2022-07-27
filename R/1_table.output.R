#' Table.output
#'
#' @param data double(1) a dataframe created with the function loadResults()
#' @param scenario character(1) string, vecteur avec le label des variables qui seront tracées
#' @param full.table boolean if TRUE, include other macroeconomic indicators ,TRUE by default
#' @param export.doc boolean if TRUE, export a docx into the folder results
#' @param title character(1) string of character for the title of the table
#' @param langue character(1) string to specify the language of comments c('en', 'fr)
#' @param results.folder character(1): pathfile to export the results. By defaut the program folder
#' @param decimal numeric : number of decimal in the datable
#'
#' @import flextable officer
#' @return a flextable
#' @export
#' @examples
#' \dontrun{
#' table.output(data = data, scenario = "oilprice_fra",
#' full.table = TRUE, export.doc = FALSE, title = 'Gros test')
#' }

table.output <- function(data = data,
                         scenario = scenario_name,
                         export.doc = TRUE,
                         langue = "en",
                         full.table = TRUE,
                         title = NULL,
                         decimal = 2,
                         results.folder = getwd()){

  # General conditions
  if (is.null(title)){
    title = paste0("scenario:", scenario)
  }

  # Parameters for all flextable: aesthetics arguments
  flextable::set_flextable_defaults(
    digits = decimal
  )


  ## Choice of years to include in the table
  # years <- c("2022", "2023","2024","2025","2027", "2050")
  startyear <- 2022
  endyear <- 2050
  time.horizon <- c(0,1,2,3,5,10,endyear-startyear)
  years <- c(rep(startyear, length(time.horizon))) + time.horizon
  # years


  # Variable in relative deviation
  var_list.1 <- c("GDP","CH","I", "X", "M",
                  "PVA","PCH","PY" ,"PX","PM",
                  "DISPINC_BT_VAL", "W", "RSAV_H_VAL")
  if (langue == "fr"){
    years_label <- c("Variable", "t", "t+1","t+2","t+3","t+5","t+10", "long terme")
    var_label.1 <- c("PIB (a)","Consommation des m\u00e9nages (a)","Investissement (a)","Exportations (a)", "Importations (a)",
                     "Prix de VA (a)","Prix \u00e0 la consommation (a)", "Prix \u00e0 la production (a)", "Prix des exportations (a)", "Prix des importations (a)",
                     "Revenu des m\u00e9nages en valeur (a)", "Salaire (a)", "Taux d'\u00e9pargne des m\u00e9nages (a)")
  }
  if (langue == "en"){
    years_label <- c("Variable", "t", "t+1","t+2","t+3","t+5","t+10","long-term")
    var_label.1 <- c("GDP (a)","Households consumption (a)","Investment (a)","Exports (a)", "Imports (a)",
                     "Price of VA (a)","Consumption price (a)", "Production price (a)", "Export price (a)", "Import price (a)",
                     "Households disposable income (a)", "Nominal wages (a)", "Households saving rate (a)")
  }

  data_table.1 <- data %>% dplyr::filter(year %in% years ,
                                         variable %in% var_list.1) %>%
    dplyr::mutate(variation = round(100 * (get(scenario)/baseline -1),decimal),
                  variable = str_replace_all(variable, purrr::set_names(var_label.1, paste0("^",var_list.1,"$")))) %>%
    dplyr::select(variable, year, variation) %>%
    tidyr::pivot_wider(names_from = year, values_from = variation) %>%
    dplyr::arrange(match(variable, var_label.1)) %>%
    `colnames<-`(years_label)

  if (full.table == TRUE){

    #Variable en diff absolue
    var_list.2 <- c("F_L")
    if (langue == "fr"){
      var_label.2 <- c("Emploi en milliers (b)")
    }
    if (langue == "en"){
      var_label.2 <- c("Employment in thousand (b)")
    }

    data_table.2 <- data %>% dplyr::filter(year %in% years ,
                                           variable %in% var_list.2) %>%
      dplyr::mutate(variation = round((get(scenario) - baseline),decimal),
                    variable = stringr::str_replace_all(variable, purrr::set_names(var_label.2, paste0("^",var_list.2,"$")))) %>%
      dplyr::select(variable, year, variation) %>%
      tidyr::pivot_wider(names_from = year, values_from = variation) %>%
      dplyr::arrange(match(variable, var_label.2)) %>%
      `colnames<-`(years_label)

    #Variable en pt de PIB
    var_list.3 <- c( "RBAL_TRADE_VAL", "RBAL_G_TOT_VAL", "RSAV_H_VAL", "MARKUP", "UNR")
    if (langue == "fr"){
      var_label.3 <- c("Balance commerciale (c)","Solde Public (c)","Taux d'\u00e9pargne des m\u00e9nages (d)",
                       "Taux de marge des entreprises (d)", "Taux de ch\u00f4mage (d)")
    }
    if (langue == "en"){
      var_label.3 <- c("Trade balance (c)","Government balance (c)","Households saving rate (d)",
                       "Mark-up rate (d)", "Unemployment rate (d)")
    }

    data_table.3 <- data %>% filter(year %in% years ,
                                    variable %in% var_list.3) %>%
      dplyr::mutate(variation = 100 * round(get(scenario),2 + decimal),
                    variable = stringr::str_replace_all(variable, purrr::set_names(var_label.3, paste0("^",var_list.3,"$")))) %>%
      dplyr::select(variable, year, variation) %>%
      tidyr::pivot_wider(names_from = year, values_from = variation) %>%
      dplyr::arrange(match(variable, var_label.3)) %>%
      `colnames<-`(years_label)

    # Binding the dataframes
    data_table <- dplyr::bind_rows(data_table.1,data_table.2,data_table.3)

    # Footnote string
    if (langue == "fr"){
      footnote.tab <- c("(a): En d\u00e9viation relative par rapport au baseline",
                        "(b): En d\u00e9viation absolue par rapport au baseline",
                        "(c): en points de pourcentage du PIB",
                        "(d): en pourcentage")
    }
    if (langue == "en"){
      footnote.tab <- c("(a): In relative deviation wrt baseline",
                        "(b): In absolute deviation wrt baseline",
                        "(c): In percentage points of GDP",
                        "(d): In percentage")
    }
    j.tab <- (1:4)

  } else {
    data_table <- data_table.1
    if (langue == "fr"){
      footnote.tab <- c("(a): En deviation relative par rapport au baseline")
    }
    if (langue == "en"){
      footnote.tab <- c("(a): In relative deviation wrt baseline")
    }
    j.tab <- 1
  }
  ## Flextable
  output <- flextable::flextable(data_table) %>%
    width(width = 2.75, j = 1)

  ## Flextable: add of the footnotes
  output <- flextable::footnote(output, i = 1, j = j.tab,
                                value = as_paragraph(
                                  footnote.tab),
                                part = "header",
                                ref_symbols = "",
                                inline = TRUE)
  ## Flextable: add of the title
  output <-  flextable::set_caption(output, caption = title,
                                    style = "Table Caption")


  if (export.doc == TRUE){
    ## Export in doc and csv formats
    sect_properties <- officer::prop_section(page_size = page_size(orient = "landscape",
                                                                   width = 12.3, height = 11.7),
                                             type = "continuous",
                                             page_margins = page_mar())

    flextable::save_as_docx(output, path = file.path(results.folder, paste0(scenario,".docx")),
                            pr_section = sect_properties)

    utils::write.table(data_table, file.path(results.folder, paste0(scenario,".csv")))
  }
  output
}
