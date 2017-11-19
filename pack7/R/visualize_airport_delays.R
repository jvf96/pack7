#' Visualize airport delays
#'
#' @return a plot with latitude and longitude
#' @importFrom stats na.omit
#' @export visualize_airport_delays
#'

visualize_airport_delays <- function(){
  
  flights <- nycflights13::flights
  airports <- nycflights13::airports
  
  data_flights <- na.omit(flights)
  data_flights <- dplyr:: summarise(dplyr::group_by(flights, dest), delay = mean(arr_delay))
  
  data_airports <- dplyr::inner_join(airports, data_flights, by = c("faa" = "dest"))
  
  ggplot2::ggplot(data_airports, ggplot2::aes(y = data_airports$lat, x = data_airports$lon)) + 
    ggplot2::geom_point(na.rm =TRUE) + ggplot2::theme_gray() + 
    ggplot2::scale_color_gradient(low = "black", high = "#F5F5F5") + 
    ggplot2::labs(title = "Flights and Airport", subtitle = "Longitude and Latitude",
                  y = "Latitude", x = "Longitude") + 
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}
