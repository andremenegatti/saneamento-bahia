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