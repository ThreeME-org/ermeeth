## contribplot requires selectseries to be loaded

#' Simple plot
#'
#' Créer un graphe de type geom_line à partir d'un vecteur de variables issues d'une base de données
#' D'autres arguments de la fonction permette de choisir le type d'indicateur et de compléter les labels
#'
#' @param data double(1) a dataframe created with the function loadResults()
#' @param series character string, vecteur avec le noms des variables qui seront plottées
#' @param label_series character string, vecteur avec le label des variables qui seront plottées
#' @param startyear numeric(1), date de début de la période plottée
#' @param endyear numeric(1), date de fin  de la période plottée
#' @param unit character(1) string , choix de legende pour l'axe des x (en pourcentage ou en niveau)
#' @param decimal numeric(1) , choix de la décimale retenue après la virgule
#' @param titleplot character(1) string , titre du graphique
#' @param template character(1) string, nom du thème ggplot retenu pour le plot
#'
#' @return  ggplot
#' @import ggh4x ggplot2 dplyr tidyr scales
#' @export

contrib.plot <- function(data,
                       series,
                       label_series = NULL,
                       startyear = NULL,
                       endyear = NULL,
                       unit = "percent",
                       decimal = 0.1,
                       titleplot = "",
                       template=template_default) {

  # To debug the function step by step, activate line below
  # browser()


  # series <- as.list(series)

  ## Format of the output plot
  format_img <- c("svg")  # Choose format: "png", "svg"



  # Palette de 4 couleurs en attendant d'en rajouter dans celles OFCE
  pal <- c("#fbc572", "#fb8072","#68a6c5","#466963")


  if (is.null(label_series)){
    label_series =  series
  }

  if (is.null(startyear)){startyear  =  min(data$year)}
  if (is.null(endyear)){endyear  =  max(data$year)}

  data <- data %>% dplyr::filter(year > startyear & year < endyear)

  plotseries <- ggplot(data = data , aes(x = year, y = value, fill = variable)) +
    geom_bar(stat= "identity", width = 0.9, position = position_stack(reverse = TRUE)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = decimal)) +
    scale_fill_manual(values = pal[1:length(series)], limits = series,
                       labels = label_series)  +
    labs(x = "", title = titleplot)


  if(unit != "percent") {
    plotseries <- plotseries +
      scale_y_continuous(labels = scales::label_number(accuracy = decimal, scale = unit))
  }

  ifelse(template =="ofce",

         plotseries <- plotseries +  ofce::theme_ofce(base_family = ""),

         plotseries <- plotseries +  theme(legend.position = "bottom")
  )

  plotseries <- plotseries + theme(legend.title = element_blank(),
                                   axis.title.y = element_blank() )

  plotseries
}
