#' Publica issue con feedback sobre el paquete
#'
#' @param checks lista con el resultado de [chequea_paquete()]
#' @param reporte archivo .md generado por [genera_reporte()]
#'
#' @returns No devuelve un objeto
#' @export
#'
#' @examples
#' \dontrun{
#' publica_feedback(checks, "paquetepruba_feedback.md")
#' }
publica_reporte <- function(checks, reporte) {

  paquete <- checks$description$nombre_paquete
  version <- checks$description$version
  issues <- unglue::unglue(checks$description$issue_url, "https://github.com/{issues}")

  body <- readLines(reporte)

  issue <- gh::gh(paste0("POST /repos/", issues[[1]][["issues"]]),
              title = glue::glue("Feedback del {paquete} {version}"),
              body = paste0(body, "\n", collapse = ""))

}

