
themes <- function(){

}


#Define palettes
color_themes = c("SAND", "ROSE")
lines = list(c("#00496FFF","#BF812DFF","#0F85A0FF","#7D4F73FF","#87AFD1FF"), #SAND
             c("#C70E7BFF","#FC6882FF","#DE1F6CFF","#C00559FF","#C86C7CFF")  #ROSE
) #enter at least five colors

background = c("#CB9060FF", #SAND
               "#EE4C97FF"  #ROSE
)

text = c("black", #SAND
         "white"  #ROSE
)

x = match(color_theme,color_themes)

#Set table colors
table_body = "white"
table_header = background[x]
table_footer = background[x]
table_header_text = text[x]
table_footer_text = text[x]
table_body_text = "black"

#Set table borderline colors
border_inner_header = "black"
border_inner_body = "grey83"
border_outer_all = "black"

#Set graphics colors
plot_header = background[x]
plot_header_text = text[x]
lines_palette = lines[[x]]
