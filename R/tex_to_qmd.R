
#' Decompose equation tex file
#'
#' @param preface_file path to preface file
#' @param maintex_file path to main tex file
#'
#' @return data.frame list of data frames with lines treated from the texfiles
#' @import dplyr stringr
#' @importFrom purrr map reduce
decompose_tex <-function(preface_file = "03.1-eq_preface.tex",
                         maintex_file = "03.1-eq.tex"){

  eq_table <- data.frame(rawline = read_lines(preface_file)  ) |> filter(grepl("repeatablebody",rawline)) |>
    mutate(
      label_tex = stringr::str_replace(rawline, "\\\\repeatablebody\\{([A-Za-z_0-9\\-\\.]+)\\}.+$","\\1"),

      label_quarto = stringr::str_c("eq-", stringr::str_replace_all(label_tex,"(\\.|\\-)","_")),

      equation = stringr::str_remove(rawline,"\\\\repeatablebody\\{([A-Za-z_0-9\\-\\.]+)\\}") |>
        stringr::str_replace("^\\{","$$") |> stringr::str_replace("\\}$","$$")
    )

  quarto_body<- data.frame(rawline = read_lines(maintex_file) ) |>
    mutate(
      section = ifelse(grepl("^\\s*(\\\\newpage)?\\\\section\\{",rawline),1,0),
      sumsection = cumsum(section),
      subsection = ifelse(grepl("^\\s*\\\\subsection\\{",rawline),1,0),
    )

  return(list(eq_table,quarto_body))
}

#################

#' Transform the tex equation files into 3 qmds
#'
#' @param preface path to preface tex file created by teXdoc function
#' @param maintex path to maintex tex file created by teXdoc function
#' @param out.dir output directory
#'
#' @return 3 or less qmd files
#' @export
#'
make_eq_qmd <-function(preface = "03.1-eq_preface.tex",
                       maintex = "03.1-eq.tex",
                       out.dir = "documentation"){

  ### File read

  readtexfile<-decompose_tex(preface, maintex)
  quarto_body <-readtexfile[[2]]
  eq_table <-readtexfile[[1]]
  if(dir.exists(out.dir)==FALSE){dir.create(out.dir)}

  exo_bool = TRUE
  glossary_bool = TRUE

  if(max(quarto_body$sumsection,na.rm = TRUE) == 1){
    exo_bool = FALSE
    glossary_bool = FALSE
  }

  if(max(quarto_body$sumsection,na.rm = TRUE) == 2 & sum(grepl("Glossary", quarto_body$rawline)) >0) {
    exo_bool = FALSE
    glossary_bool = TRUE
    quarto_body$sumsection <- ifelse(quarto_body$sumsection ==2 , 3, quarto_body$sumsection)
  }

  if(max(quarto_body$sumsection,na.rm = TRUE) == 2 & sum(grepl("Glossary", quarto_body$rawline)) == 0) {
    exo_bool = TRUE
    glossary_bool = FALSE
  }


  ### Section 1 equation

  section_1 <- quarto_body |> dplyr::filter(sumsection==1) |>
    dplyr::mutate(
      newline = stringr::str_replace(rawline,"\\\\section\\{(.+)\\}","## \\1"),

      newline = ifelse(grepl("\\\\subsection",newline),
                       stringr::str_replace(rawline,"\\\\subsection\\{(.+)\\}","### \\1"),
                       newline ),

      newline = ifelse(grepl("\\\\noindent\\s*\\\\textbf",newline),
                       stringr::str_replace(newline,"\\\\noindent\\s*\\\\textbf\\{\\s*(.+)\\s*\\}","**\\1**"),
                       newline ),

      label_tex = ifelse(grepl("\\\\repeatable",newline),
                         stringr::str_replace(newline,"\\\\repeatable\\s*\\{(.+)\\}",
                                     "\\1") |> stringr::str_remove_all(" ") ,
                         newline )

    ) |> dplyr::full_join(eq_table |> dplyr::select(-rawline) ,by="label_tex" ) |>
    dplyr::mutate(
      newline = ifelse(is.na(label_quarto), newline,
                       stringr::str_c(equation,"{#",label_quarto,"}"))


    )



  writeLines(text = section_1$newline,
             con = file.path(out.dir,"equations.qmd")
  )

  labs_correspondance_table <- section_1 |> dplyr::select(label_tex, label_quarto) |> dplyr::filter(!is.na(label_quarto))

  ### Section 2 exogenous (optional)

  if(exo_bool){
    section_2 <- quarto_body |> dplyr::filter(sumsection ==2) |>
      dplyr::mutate(
        newline=stringr::str_replace(rawline,"^(\\\\newpage)?\\s*\\\\section\\{(.+)\\}","## \\2"),

        labelline = ifelse(grepl("\\\\refstepcounter\\{equation",rawline),1,0),
        varline = ifelse(grepl("\\\\ensuremath",rawline),1,0)

      ) |>
      dplyr::group_by(labelline) |> dplyr::mutate(
        counter_l = cumsum(labelline)
      ) |>
      dplyr::group_by(varline) |> dplyr::mutate(
        counter_v = cumsum(varline)
      ) |> ungroup() |>
      dplyr::filter(counter_l!=0|counter_v!=0)

    lab_table <- section_2 |> dplyr::filter(counter_l>0) |> dplyr::rename(counter=counter_l) |>
      dplyr::select(rawline,counter) |>
      dplyr::mutate(
        label_tex = stringr::str_extract(rawline, "\\\\label\\{([A-Za-z_0-9\\-\\.]+)\\}" ) |>
          stringr::str_replace("\\\\label\\{([A-Za-z_0-9\\-\\.]+)\\}","\\1"),

        label_quarto = stringr::str_c("eq-", stringr::str_replace_all(label_tex,"(\\.|\\-)","_"))
      )

    var_table <- section_2 |> dplyr::filter(counter_v>0) |> dplyr::rename(counter=counter_v) |>
      dplyr::select(rawline2= rawline,counter) |>
      dplyr::mutate(
        formula = stringr::str_extract(rawline2,"\\\\ensuremath\\{.+\\~") |>
          stringr::str_replace_all("(\\\\ensuremath\\{|\\}\\~)","$$") ,

        name = stringr::str_extract(rawline2,"\\\\textbf\\{.+\\}")  |>
          stringr::str_replace_all("(\\\\textbf\\{\\s*|\\s*\\})","**") ,


      )

    exo_table<-dplyr::full_join(var_table,lab_table,by = "counter") |>
      dplyr::mutate(newline =  stringr::str_c(name , " ", formula, "{#",label_quarto,"}", "  " ),
             order = 1

      )

    lines_to_write <- purrr::map(exo_table$newline, ~c(.x,c(" ","___"," "))) |> purrr::reduce(c)

    labs_correspondance_table <- rbind(labs_correspondance_table,
                                       exo_table |> dplyr::select(label_tex, label_quarto)

    )


    writeLines(text = c("## Exognenous variables"," ", lines_to_write),
               con = file.path(out.dir,"exogenous.qmd")  )

  }else{
    writeLines(text = c(" "),
               con = file.path(out.dir,"exogenous.qmd") )
  }

  ### Section 3 glossary
  if(glossary_bool){

    section_3 <- quarto_body |> dplyr::filter(sumsection == 3)  |> dplyr::filter(grepl("\\\\midrule|\\\\ref\\{",rawline)) |>
      dplyr::mutate(variable = stringr::str_extract(rawline,"^\\s*\\$\\s*.+\\$") ,
             variable_def = stringr::str_extract(rawline,"&[^&]*&") |> stringr::str_remove_all("\\s*&\\s*"),
             label_tex = stringr::str_extract(rawline, "\\\\ref\\{[^\\}]+\\}") |> stringr::str_remove_all("\\\\ref|\\{|\\}")) |>

      dplyr::left_join(labs_correspondance_table |> select(label_tex,label_quarto), by="label_tex") |>

      dplyr::mutate(label_quarto = stringr::str_c("[-@",label_quarto, "]"),
             linetowrite = stringr::str_c("| ",variable," | ",variable_def," | ", label_quarto),
      ) |> dplyr::filter(!is.na(variable))

    header <- c("| | | |",
                "|:---:|:----------- |---------: |")
    writeLines(text = c("## Glossary of variables"," ",header,  section_3$linetowrite),
               con = file.path(out.dir,"glossary.qmd")  )
  }else{
    writeLines(text = c(" "),
               con = file.path(out.dir,"glossary.qmd") )
  }


}







