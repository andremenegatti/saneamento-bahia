library(tmap)
library(ggplot2)
library(stringr)
library(dplyr)

# add_geometry_municipios -----------------------------------------------------
#' Adds a geometry column to dataframe with municipality-level data
#'
#' Joins provided dataframe with another containing municipality-level
#' \code{MULTIPOLYGON} geospatial data. The key used to perform the join is the
#' numeric column \emph{Codmun7}, which must contain cities' 6-digit code, as
#' used by IBGE.
#'
#' @param df A dataframe containing data from cities belonging to the state of
#'   Bahia. The dataframe must have a numeric column named \emph{codigo_municipio}
#'   with cities' IBGE 6-digit code.
#'
#' @return A dataframe containing the original data and an additional
#'   \emph{geometry} column with municipality-level geospatial data.
add_geometry_municipios <- function(df) {
  polygons_municipios_bahia %>%
    left_join(df, by = 'codigo_municipio')
}

# custom_map_settings ---------------------------------------------------------
custom_map_settings <- 
  tm_layout(main.title.size = 1.2, fontfamily = 'serif', scale = 1.1,
            main.title.fontface = 'bold', bg.color = "white",
            inner.margins = c(.1, .1, .1, .1)) +
  tm_compass(north = 0, type = "8star",size = 2,
             position = c("right", "bottom")) +
  tm_scale_bar(text.size = 0.6, text.color = NA, lwd = 1,
               color.dark = "black", color.light = "white") +
  tm_legend(legend.position = c(0.01,0.08)) +
  tm_borders(col = "black", lwd = 0.3)


# custom_theme ----------------------------------------------------------------
custom_theme <- function() {
  theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          text = element_text(family = 'serif'),
          plot.title = element_text(face = 'bold'),
          axis.title = element_text(face = 'bold'))
}


# substituir_caracteres_especiais ---------------------------------------------
substituir_caracteres_especiais <- function(x) {
  toupper(x) %>% 
    str_replace_all('Á', 'A') %>% 
    str_replace_all('Ó', 'O') %>% 
    str_replace_all('É', 'E') %>% 
    str_replace_all('Ã', 'A') %>% 
    str_replace_all('Õ', 'O') %>% 
    str_replace_all('Ç', 'C') %>% 
    str_replace_all('Í', 'I') %>% 
    str_replace_all('À', 'A') %>% 
    str_replace_all('Ê', 'E') %>% 
    str_replace_all('Ê', 'O') %>% 
    str_replace_all('Ú', 'U')
}


# geom_flat_violin ------------------------------------------------------------
# Code by David Robinson
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x - width / 2,
                     xmax = x)
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, 
                              xmaxv = x,
                              xminv = x + violinwidth * (xmin - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )