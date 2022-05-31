## simpleplot requires selectseries to be loaded

#' Simple plot
#'
#' Créer un graphe de type geom_line à partir d'un vecteur de variables issues d'une base de données
#' D'autres arguments de la fonction permette de choisir le type d'indicateur et de compléter les labels
#'
#' @param data double(1) a dataframe created with the function loadResults()
#' @param series character(1) string, vecteur avec le noms des variables qui seront plottées
#' @param label_series character(1) string, vecteur avec le label des variables qui seront plottées
#' @param startyear numeric(1), date de début de la période plottée
#' @param endyear numeric(1), date de fin  de la période plottée
#' @param transformation character(1) string, choix de l'option de l'indicateur (en niveau, en variation, différence absolue, relative )
#' @param unit character(1) string , choix de legende pour l'axe des x (en pourcentage ou en niveau)
#' @param decimal numeric(1) , choix de la décimale retenue après la virgule
#' @param titleplot character(1) string , titre du graphique
#' @param scenario character(1) string, nom du scénario à plotter, doit être dans la base de données.
#' @param template character(1) string, nom du thème ggplot retenu pour le plot
#' @param percent_label boolean(1) permet d'afficher ou non le signe des pourcentage  lorsque l'échelle est en pourcentage, TRUE par défaut.
#' @param custom_x_breaks integer(1) permet de choisir manuellement les ticks des années à afficher. Si NULL (défaut) alors utilise l'algorithme de la fonction. Si "R" : algorithme par défaut de R sera utilisé.
#'
#' @return un ggplot
#' @import ggh4x ggplot2 dplyr ofce tidyr
#' @importFrom scales percent label_number percent_format
#'
#' @export
simpleplot <- function(data,
                       series,
                       label_series = NULL,
                       startyear = NULL,
                       endyear = NULL,
                       transformation ="reldiff",
                       unit = "percent",
                       decimal = 0.1,
                       titleplot = "",
                       scenario=scenario_name,
                       template=template_default,
                       percent_label = TRUE,
                       custom_x_breaks = NULL
                       ) {

  # To debug the function step by step, activate line below
  # browser()



  # series <- as.list(series)

  ## Format of the output plot
  format_img <- c("svg")  # Choose format: "png", "svg"



  # Palette de 4 couleurs en attendant d'en rajouter dans celles OFCE
  pal <- custom.palette(n = length(series)) %>% purrr::set_names(.,series)

  selection <- selectseries(data,scenario, series, startyear, endyear, transformation)

  if (is.null(label_series)){
    label_series =  series
  }


  ifelse(transformation =="gr",
           plotseries <- ggplot(data = selection, aes(x=year)) + aes(group = variable) +
             geom_line(aes(y=gr_base, col=variable), linetype = "dashed") +
             geom_line(aes(y=gr_scen, col=variable), linetype = "solid"),



         ifelse(transformation =="level",
              plotseries <- ggplot(data = selection, aes(x=year)) + aes(group = variable) +
                    geom_line(aes(y=base, col=variable), linetype = "dashed") +
                    geom_line(aes(y=scen, col=variable), linetype = "solid"),

              plotseries <- ggplot(data = selection, aes(group = variable, y = transfo, x = year, color = variable))  +
                     geom_line() +
                     geom_point(size = 0)

         )
  )

  if(percent_label == TRUE){
    lab_percent <- "%"
  }else{
    lab_percent <- ""
  }

  ## calculating the optimal number of ticks to show

  if(is.null(custom_x_breaks)){
    n_years <- endyear - startyear
    algo_x_breaks <- 10

    if(n_years <= 35){ algo_x_breaks <- 5 }
    if(n_years <= 20){ algo_x_breaks <- 2 }
    if(n_years <= 10){ algo_x_breaks <- 1 }


    break_x_sequence <- seq(from = startyear, to =  endyear, by = algo_x_breaks)

  }else{
    if(is.numeric(custom_x_breaks)){
    break_x_sequence <- seq(from =  startyear, to = endyear, by = custom_x_breaks)

    }else{
    break_x_sequence <- waiver()}

  }


  plotseries <- plotseries +
    scale_x_continuous(breaks = break_x_sequence)+
    scale_y_continuous(labels = scales::percent_format(accuracy = decimal, suffix = lab_percent))  +
    scale_color_manual(values = pal, limits = series,
                       labels = label_series)  +
    labs(x = "", y = transformation, title = titleplot)

  if(unit != "percent") {
    plotseries <- plotseries + scale_y_continuous(labels = scales::label_number(accuracy = decimal, scale = unit))
  }

  if(template == "ofce"){
       plotseries <- plotseries +  ofce::theme_ofce(base_family = "")
  }else{
      plotseries <- plotseries +  theme(legend.position = "bottom")
  }


  plotseries <- plotseries + theme(legend.title = element_blank(),
                                   axis.title.y = element_blank() ,
                                   axis.ticks = element_line(size = 0.5, colour = "grey42"),
                                   legend.position="bottom")

  plotseries
}


