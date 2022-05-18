##### methods of interpolation

#' linear_fill
#' @description Takes a vector and computes the 2nd to the penultimate values using the first and last value of the input vector using linear interpolation, assuming each element of the vector are regularly spaced out.
#'
#' @param vector a numeric vector
#'
#' @return a numeric vector of the same length as vector
#' @export
#'
#' @examples
#' linear_fill(c(1,100,2))
#' linear_fill(c(42,NA,NA,NA,32))
linear_fill <- function(vector){
  k <- length(vector)
  slope <- (vector[k]-vector[1])/(k-1)

  index_vector <- c(0:(k-1))

  res<- vector[1] + slope*(index_vector)
  res[1] <- vector[1]
  res[k] <- vector[k]
  res
}

#################
### interpolation series
#################

#' interpolation_series
#' @description Given a date vector and corresponding value vector, computes the values in between each date using some form of interpolation .
#'
#' @param date_vector numeric or Date vector to match with value vector
#' @param value_vector numeric vector of anchor values from which to compute interpolation
#' @param method "linear" only for now
#' @param seq.step regular interval expected between each date
#' @param first.date date from which to start the final sequence.
#' @param last.date date where the final sequence stops
#' @param showplot show the plot of the computations
#'
#' @return a numeric vector
#' @export
#' @import ggplot2 purrr dplyr
#' @examples
interpolation_series <- function(date_vector  = NULL,
                                 value_vector = NULL,
                                 method = "linear" ,
                                 seq.step = 1 ,
                                 first.date = first_date,
                                 last.date = last_date,
                                 showplot = FALSE){

  ### running checks
  if(is.null(date_vector)){
    stop(message="No date_vector specified.\n")
  }
  if(is.null(value_vector)){
    stop(message="No value_vector specified.\n")
  }

  if(length(date_vector) != length(value_vector)){
    stop(message="value_vector and date_vector must be of the same length.\n")
  }

  if(max(date_vector) > last.date){
    message("The specified last.date is earlier than lastest date provided in date_vector, the argument will be ignored.\n")
    last.date <- max(date_vector)
  }

  if(min(date_vector) < first.date){
    message("The specified first.date is later than earliest date provided in date_vector, the argument will be ignored.\n")
    first.date <- min(date_vector)
  }

  if(!is.numeric(value_vector)){
    stop(message = "value_vector must be numeric")
  }

  input_array <- data.frame(date = date_vector,input_vector = value_vector) %>% dplyr::arrange(date)

  index_vector <- seq(from = first.date, to = last.date, by = seq.step )

  if(prod(date_vector %in% index_vector) == 0){
    stop(message="Some dates from date_vector could not be found in the sequence from = first.date to = last.date by seq.step. Please check seq.step specification. \n")
  }

  res_array <- data.frame(date = index_vector,res_vector = rep(NA, length(index_vector))) %>%
    dplyr::full_join(input_array, by = "date") %>%
    dplyr::mutate(res_vector = ifelse(is.na(input_vector),res_vector,input_vector),
           res_vector = ifelse(date < min(date_vector),
                               input_array$input_vector[which(input_array$date == min(date_vector))] , res_vector) ,
           res_vector = ifelse(date > max(date_vector),
                               input_array$input_vector[which(input_array$date == max(date_vector))] , res_vector) ,
           test.r = dplyr::lag(date)
    )  %>% dplyr::select(-input_vector)

  ## step 1 locate the intervals that need filling in

  if(is.na(res_array$res_vector[1])){
    stop(message= "Functional error : NA found in first position")
  }
  if(is.na(res_array$res_vector[nrow(res_array)])){
    stop(message= "Functional error : NA found in last position")
  }

  non_na_dates <- data.frame(date = res_array$date[which(!is.na(res_array$res_vector))]) %>% dplyr::mutate(test.t = dplyr::lag(date))

  intervals <- res_array %>% dplyr::left_join(non_na_dates, by = "date") %>% dplyr::filter(test.r != test.t) %>% dplyr::select(test.t,test.r) %>% dplyr::mutate(test.r = test.r + 1) %>% dplyr::rename(start = test.t, end = test.r)



  na.vectors <- c(1:nrow(intervals)) %>%
    purrr::map(~c(intervals$start[.x],intervals$end[.x])) %>%
    purrr::map(~seq(from = .x[1], to = .x[2], by = seq.step)) %>%
    purrr::map(~res_array %>% dplyr::filter(date %in% .x) %>%
          dplyr::select(-test.r) %>%
          ## insert different methods here
          dplyr::mutate(res_vector = linear_fill(res_vector)) )

  final_res <-na.vectors %>%  purrr::reduce(rbind) %>%
    rbind(res_array %>% dplyr::select(date, res_vector) %>% dplyr::filter(!is.na(res_vector))) %>%
    unique() %>% dplyr::arrange(date)


  if(showplot == TRUE){

    ggplot2::ggplot(final_res, aes(x=date, y=res_vector)) +
      ggplot2::geom_line(colour = "grey42")+
      ggplot2::geom_point(colour = "blue", size =0.5)+
      ggplot2::geom_point(data = input_array, aes(y = input_vector), colour = "red") +
      ggplot2::theme_light() +
      ggplot2::xlab("")+
      ggplot2::ylab("")

  }else{ return(final_res$res_vector %>% purrr::set_names(final_res$date))}


}


