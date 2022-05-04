library(tidyverse)
library(farver)
library(colorspace)



custom.palette <- function(n = NULL,
                           bridge = TRUE,
                           sub.bridge = FALSE,
                           type = "sectors")
{
  # browser()

  # Palette of OFCE diverging colors. Base to differentiate sectors
  pal_base <- c("#B61615",
                "#017FC2",
                "#2E7437",
                "#F9B000",
                "#EA5B0C",
                "#9473AB",
                "#D3A170",
                "#DA2311",
                "#662483",
                "#C0087F",
                "#7A9E1A",
                "#009FE3",
                "#009FE3",
                "#846A3B",
                "#25378D",
                "#568E2F",
                "#1D1D1B",
                "#FBBA00",
                "#E30613")



  if (bridge == TRUE)
  {

    source("data/bridge_c28_s32.R")


    if (type == "sectors"){

      #length of subsectors for a selected sector
      brd <- bridge_sectors
      n.brd <- length(bridge_sectors)
    } else {
      #length of subsectors for a selected sector
      brd <- bridge_commodities
      n.brd <- length(bridge_commodities)
    }

    # Option to get the full set of sectors/commodities (before bridging)
    if (sub.bridge = TRUE){

      pal_list <- list()

      for (i in 1:length(brd)){

        n.in <- length(brd[[i]])
        #
        #         if (n.in == 1){
        #
        #           pal_list <- append(pal_list,str_c(i) = pal_base[i])
        #
        #         } else{


        col_hcl <- coords(as( hex2RGB(pal_base[i], gamma = FALSE), "polarLAB"))

        pal_out <- colorspace::sequential_hcl(n = n.in +1,
                                              h = col_hcl[3],
                                              c = c(col_hcl[2], NA, NA),
                                              l = c(20, 70),
                                              # l = c(col_hcl[1]-25,
                                              #       col_hcl[1] + 25),
                                              power = 1.2 )


        pal_list <- append(pal_list,list(pal_out[-length(pal_out)]))
      }


      pal <-pal_list %>% unlist()
      print(pal)

    # Option to get the subset of sectors/commodities (after bridging)
    } else { pal <- pal_base[seq(1,n.brd)]
    print(pal)
    }
  }  else {
  # Option by default to get a palette of n colors
  pal_base[seq(1,n)]
  }
}




custom.palette(bridge= FALSE,sub.bridge = FALSE, "sectors")
