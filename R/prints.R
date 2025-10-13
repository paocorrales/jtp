# Formato de los checks

#' @export
print.evaluacion_description <- function(x, ...) {

  autores <- lapply(x$autores, function(person) {

    person <- unclass(person[[1]])

    list(
      nombre = paste(person[[1]]$given, person[[1]]$family),
      rol = paste(person[[1]]$role, collapse = " - "),
      comentarios = person[[1]]$comment
    )

  }) |>
    Reduce(f = "rbind")


  with(x, cat(glue::glue("**{nombre_paquete} v{version}** \n
                          * T\U00EDtulo: {titulo}
                          * Descripci\U00F3n: {descripcion}
                          * Mantainer: {mantainer}
                          * Issues: {issue_url} \n
                         ")))


  cat("\n### Autores\n")
  cat(knitr::kable(autores), sep = "\n")
  cat("\n")
  with(x, cat(glue::glue("\n### Licencia \n{licencia}\n")))

  if (x$licencia_valida) {
    cat("\nLa licencia es v\U00E1lida", emoji::emoji("white_check_mark"))
  } else {
    cat("\nLa licencia no es v\U00E1lida", emoji::emoji("x"))
  }
}

#' @export
print.evaluacion_general <- function(x, ...) {

  if (x$paquete_instala) {
    cat("El paquete se instala correctamente", emoji::emoji("white_check_mark"))
  } else {
    cat("El paquete se no instala correctamente", emoji::emoji("x"))
  }
}

#' @export
print.evaluacion_codigo <- function(x, ...) {

  # cat(paste0(paste(seq_along(x$funciones), x$funciones, sep = ". "), collapse = " \n"))
  cat("\n\nDetalles:\n")
  cat(knitr::kable(x$funciones), sep = "\n")

  cat("\n\n### Dependencias \n")

  cat(knitr::kable(x$dependencias), sep = "\n")

  if (!x$mal_import) {
    cat("No importa paquetes enteros", emoji::emoji("white_check_mark"))
  } else {
    cat("Importa paquetes enteros", emoji::emoji("x"))
  }
}

#' @export
print.evaluacion_documentacion <- function(x, ...) {

  cat("\n### Ejemplos\n")
  cat(knitr::kable(x$ejemplos), sep = "\n")

  cat("\n### Vi\U00F1etas\n\n")
  cat("Cantidad de vi\U00F1etas: ", x$n_vinieta, "\n")

  if (!x$vinieta_no_corre) {
    cat("La vi\U00F1eta corre correctamente", emoji::emoji("white_check_mark"))
  } else {
    cat("La vi\U00F1eta no corre correctamente", emoji::emoji("x"))
  }
}

#' @export
print.evaluacion_coverage <- function(x, group = "functions", by = "line", ...) {

  type <- attr(x, "type")

  if (is.null(type) || type == "none") {
    type <- NULL
  }

  class(x) <- "coverage"

  df <- covr::tally_coverage(x, by = by)

  if (!NROW(df)) {
    return(invisible())
  }

  percents <- tapply(df$value, df[[group]], FUN = function(x) (sum(x > 0) / length(x)) * 100)

  overall_percentage <- covr::percent_coverage(df, by = by)
  cat("\nCovertura global: ", format(overall_percentage, digits = 4), "%\n")

  by_coverage <- percents[order(percents,
                                names(percents))]

  cat("**Funciones:**\n")
  for (i in seq_along(by_coverage)) {

    cat(paste0(
      names(by_coverage)[i], ": ",
      format(by_coverage[i], digits = 4), " %\n"
    ))
  }
}

#' @export
print.evaluacion_ci <- function(x, ...) {

  cat(paste0("\nBadges: ", x$badges, "\n\n"))
  cat("### GitHub Actions\n")
  cat(knitr::kable(x$ga), sep = "\n")
}

#' @export
print.evaluacion_git <- function(x, ...) {

  cat(paste0("N\U00FAmero de autores de commits: ", x$n_autores, "\n"))
  cat(paste0("N\U00FAmero de commits: ", x$n_commits, "\n"))

}

#' @export
print.evaluacion_extras <- function(x, ...) {

  if (length(x$usan_TF) == 0) {
    cat("Usan TRUE o FALSE", emoji::emoji("white_check_mark"))
  } else {
    cat("Usan T o F en vez de TRUE o FALSE", emoji::emoji("x"))
  }

  cat("\n")

  if (!x$archivos_extras) {
    cat("Estructura de archivos correcta", emoji::emoji("white_check_mark"))
  } else {
    cat("Se encontraron carpetas o archivos no estandar", emoji::emoji("x"))
  }

}

#' @export
print.evaluaciones <- function(x, ...) {
  cat("# Reporte\n")
  for (i in seq_along(x)) {
    cat(glue::glue("## {tools::toTitleCase(names(x))[[i]]}"))
    cat("\n")
    print(x[[i]])
    cat("\n\n")
  }
}

