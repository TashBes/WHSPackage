


#' Polygon area calculation
#'
#' Calculates the area of all polygons, and the percentage of area within the world heritage polygon that they occupy both individually and as a whole.
#' @param data The polygon or shape file that you want to calculate the area for.
#' @param col The column in the shape file that represents the polygons
#' @param name The name of the polygons
#' @details The packages tidyverse, janitor, sf, and gt need to be installed
#' @export
area <- function(data, col, name){

  data$extent <- sf::st_area(data)

  area_new <- data %>%
    dplyr::group_by({{col}}) %>%
    dplyr::summarise(area =  as.numeric(sum(extent))/1000,
              percent = round((as.numeric(sum(extent))/1000)/(as.numeric(sum(whs_extent$extent))/1000)*100, digits = 2))%>%
    dplyr::as_tibble() %>%
    dplyr::select({{col}}, area, percent)%>%
    janitor::adorn_totals("row")

  a <- paste(name,"Name")
  area_new%>%
    gt::gt() %>%
    gt::tab_header(title = html(paste(name, "extent in the proposed world heitage site"))) %>%
    gt::cols_label(
      {{col}} := html(a),
      area = html("**Total area <br> km**"),
      percent = html("**Percentage cover <br> %**"),
      .fn = md) %>%
    gt::cols_align(align = c("center"),
               columns = c(area, percent))
}

library(roxygen2) # Read in the roxygen2 R package
roxygenise()      # Builds the help files
