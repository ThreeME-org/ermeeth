
#' custom_palette
#'
#' @param n numeric(1) A number of colors to pick
#' @param bridge If not null, ThreeME bridge to be loaded takes precedent over n
#' @param sub.bridge If TRUE, the complete list of sectors is chosen when defined by aggregated sectors, by default FALSE
#' @param palette_base If not null, a vector of colors to based the palette
#'
#' @return a vector of colors (hex code)
#' @export
#' @import colorspace
#' @examples
#' custom.palette(n = 6)
custom.palette <- function(n = NULL,
                           bridge = NULL,
                           sub.bridge = FALSE,
                           palette_base = NULL){

  if (is.null(bridge) & is.null(n)){
    stop(message = " Pleeeease, specifiy at least a number n or a bridge (ex:bridge_sectors) .\n")
  }

  if (!is.null(bridge) & !is.null(n)){
    cat("Droping n and retaining bridge option .\n")
    n <- NULL
  }

  if (is.null(palette_base)){
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
  } else {
    pal_base <- palette_base
  }


  if (!is.null(bridge)){
    brd <- bridge
    n.brd <- length(bridge)

    # Option to get the full set of sectors/commodities (before bridging)
      pal_list <- list()

      for (i in 1:length(brd)){

        n.in <- length(brd[[i]])

        if (n.in == 1){

          pal_list <- append(pal_list, pal_base[i])

        } else{


          col_hcl <- colorspace::coords(as( colorspace::hex2RGB(pal_base[i], gamma = FALSE), "polarLAB"))

          pal_out <- colorspace::sequential_hcl(n = n.in +1,
                                                h = col_hcl[3],
                                                c = c(col_hcl[2], NA, NA),
                                                l = c(20, 70),
                                                # l = c(col_hcl[1]-25,
                                                #       col_hcl[1] + 25),
                                                power = 1.2 )

        }
        pal_list <- append(pal_list,list(pal_out[-length(pal_out)]))
      }
      # Rajouter argument pour lier color & names_brd <- codenames...R/names_commodities

      pal <- unlist(pal_list) #%>% set.names(names_brd) # cagr, csug, c...
      return(pal)

      # Option to get the subset of sectors/commodities (after bridging)
    } else {
      pal <- pal_base[seq(1,n.brd)]
      return(pal)
    }
  }  else {
    # Option by default to get a palette of n colors
    return(pal_base[seq(1,n)])
  }
}




