#' Deja solo las variables relevantes en la validación
#'
#' Deja solo las variables relevantes en la validación
#'
#' @param report Data frame
#' @param label Nombre de la columna que que se quiere limpiar
#' @param id Identificador de cada registro. Es un string
#' @import tidyr
#' @export

clean_report <- function(report, label, id) {
  id_sym <- sym(id)
  unpacked_data <- report %>%
    select(all_of(id_sym), !!label := all_rules_true)
  return(unpacked_data)

}
