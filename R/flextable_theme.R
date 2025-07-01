#' flextable_theme used in standard shocs
#'
#' @param x A flextable table
#' @param colnumber number of columns
#' @param vline vector of columns where to draw thicker lines
#' @param donotcolor vector of columns where not to colour
#' @param scales Boolean, whether or not to use the red-green colours scale. Default: TRUE
#' @param minimum minimum of colour scale
#' @param maximum maximum of colour scale
#' @param fontsize size of font in the table. Default: 12
#' @param palette_confetti Palette to use, made by the confetti function
#'
#' @importFrom flextable fp_border_default fontsize font theme_zebra align valign bg color border_remove border_inner border_outer vline fix_border_issues line_spacing set_table_properties colformat_num italic padding
#' @importFrom scales col_numeric
#'
#' @returns a flextable with the format
#' @export
#'
flextable_theme <- function(x,
                            colnumber = if(exists("col_numb")){col_numb}else{6},
                            vline = c(2),
                            palette_confetti = NULL ,
                            donotcolor = 1,
                            scales = TRUE,
                            minimum = -3,
                            maximum = 3,
                            fontsize = 12){

  # col_numb <- NULL ### To correct
  table_striped <- NULL

  if(is.null(palette_confetti)){palette_confetti <- confetti() }
  palette_confetti |> list2env(envir = environment())

  scale <- scales::col_numeric(domain = c(minimum,maximum), palette = c("red","green4"))

  std_border_h <- flextable::fp_border_default(width= 0.4,color = border_inner_header)
  std_border_b <- flextable::fp_border_default(width= 0.4,color = border_inner_body)
  std_border_o <- flextable::fp_border_default(width= 1,color = border_outer_all)

  x <- flextable::fontsize(x,size = fontsize, part = "all") |>
    flextable::font(fontname = "Arial", part = "all") |>

    flextable::theme_zebra(
                odd_header = "transparent",
                even_header = "transparent",
                odd_body = "transparent",
                even_body = table_striped) |>

    flextable::align(j=c(2:colnumber),align="center") |>
    flextable::align(i=1:2,j=NULL,align="center",part="header") |>
    flextable::align(i=1:2,j=1:2,align="left",part="header") |>
    flextable::valign(j=c(1:colnumber),valign = "center", part = "all") |>

    # flextable::bg(bg = table_body, part = "body") |>
    # flextable::bg(bg = table_header, part = "header") |>
    # flextable::bg(bg = table_footer, part = "footer") |>
    flextable::color(color = table_header_text, part = "header") |>
    flextable::color(color = table_footer_text, part = "footer") |>
    flextable::color(color = table_body_text, part = "body") |>

    flextable::border_remove() |>
    flextable::border_inner(border = std_border_h, part="header") |>
    flextable::border_inner(border = std_border_b, part="body") |>
    flextable::border_outer(part="all", border = std_border_o) |>
    flextable::vline(border = std_border_o, j = vline) |>
    flextable::fix_border_issues() |>

    flextable::line_spacing(space = 0.3, part = "all") |>
    flextable::set_table_properties(layout = "autofit") |>
    flextable::colformat_num(i = ~ !is.na(Variable),na_str = "n/a") |>

    flextable::color(j=-c(1:2),color=if(scales==TRUE){scale}else{table_body_text},part = "body") |>
    flextable::color(i=c(donotcolor),color=table_body_text) |>

    flextable::bold(bold = FALSE, part = "footer") |>
    flextable::italic(part = "footer", j = 1:colnumber)
    #flextable::padding(i= c(3,6,10),j=1, padding = 15) |>
    #flextable::line_spacing(i= c(3,6,10),space = 1.6)

  x
}
