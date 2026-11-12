#' Chequea el estado de un paquete
#'
#' @param ruta_paquete ruta al directorio raiz del paquete
#'
#' @returns una lista con el resultado de los checks.
#' @export
#'
#' @examples
#' \dontrun{
#' chequea_paquete(".")
#' }
chequea_paquete <- function(ruta_paquete) {

  ruta_paquete <- rprojroot::find_root(rprojroot::is_r_package, ruta_paquete)

  description <- desc::desc(ruta_paquete)
  namespace <- parseNamespaceFile(basename(ruta_paquete), file.path(ruta_paquete, ".."))
  check <- rcmdcheck::rcmdcheck(ruta_paquete, quiet = TRUE)
  cov <- covr::package_coverage(ruta_paquete)
  git <- gert::git_log (repo = ruta_paquete, max = 1e6)


  state <- list()


  # El paquete se instala
  state$general$paquete_instala <- length(grep("DONE", check$install_out)) > 0

  class(state$general) <- "evaluacion_general"

  # y es posible correr algunos ejemplos sin problema


  # Título, Descripción
  state$description$nombre_paquete <- description$get_field("Package")
  state$description$titulo <- description$get_field("Title")
  state$description$descripcion <- description$get_field("Description")
  state$description$version <- description$get_version()

  # Las personas involucradas estan listadas en Auth@rs

  state$description$autores <- description$get_authors()
  state$description$mantainer <- description$get_maintainer()

  class(state$description) <- "evaluacion_description"

  # Se listan las dependencias en el DESCRIPTION



  # Funciones

  state$codigo$exports <- namespace$exports
  state$codigo$dependencias <- description$get_deps()
  state$codigo$mal_import <- all(vapply(namespace$imports, length, 1L)) > 1 # Importa todo un paquete

  state$codigo$funciones <- pkgstats:::roxygen_block_stats(ruta_paquete)


  # Datos

  state$codigo$datos <- pkgstats:::get_data_stats(ruta_paquete)

  class(state$codigo) <- "evaluacion_codigo"

  # Todas las funciones y datos están documentados

  r_files <- fs::dir_ls (fs::path(ruta_paquete, "R"), regexp = "\\.(R|r|q)$")
  state$documentacion$ejemplos <- lapply(r_files, function(f) {

    other <- utils::capture.output(b <- roxygen2::parse_file(f, env = NULL), type = "message")

    if (length(b) == 0) {

      summary <- list(file = basename(f),
                      title = "roxygen2 no puede parsear el archivo",
                      tiene_ejemplo = NA,
                      ejemplos = paste0("<details><summary>Ejemplos</summary><p>", NA, "</p></details>"),
                      mensajes = other)
    } else {

      ejemplo <- as.character(roxygen2::block_get_tag_value(b[[1]], "examples"))
      ejemplo <- gsub("\\n", "<br>", ejemplo)

      summary <- list(file = basename(f),
                      title = roxygen2::block_get_tag_value(b[[1]], "title"),
                      tiene_ejemplo = roxygen2::block_has_tags(b[[1]], "examples"),
                      ejemplos = paste0("<details><summary>Ejemplos</summary><p>", ejemplo, "</p></details>"),
                      mensajes = NA)
    }

    if (length(other)!= 0){
      summary$mensajes <- other
    }

    return(summary)
  }) |> Reduce(f = "rbind")

  class(state$documentacion) <- "evaluacion_documentacion"

  # README completo
  # Incluye una viñeta que explica como usar las funciones

  state$documentacion$n_vinieta <- pkgstats:::get_num_vignettes(ruta_paquete)
  state$documentacion$vinieta_no_corre <- length(grep("Errors in running code in vignettes", check)) > 0


  # El paquete tiene una página web
  # El paquete pasa R CMD checks

  state$rcmdchecks <- check

  # El paquete tiene tests que cubren al menos el 75% del código de las funciones

  state$covr <- cov

  class(state$covr) <- "evaluacion_coverage"

  # El paquete tiene una GitHub action que revisa la covertura de los tests
  # El paquete tiene una GitHub action que chequea el paquete en distintos sistemas operativos

  url <- description$get_urls()[1]

  state$ci$badges <- pkgcheck:::pkgchk_ci_badges(url)
  state$ci$ga <- pkgcheck:::ci_results_gh(ruta_paquete)

  class(state$ci) <- "evaluacion_ci"

  # Incluye una licencia

  state$description$licencia <- description$get_field("License")
  state$description$licencia_valida <- length(grep("Invalid license file pointers", check)) == 0 # Invalid file


  # Incluye contributing

  state$description$issue_url <- description$get_field("BugReports")
  # Uso de Git

  state$git$n_commits <- nrow(git)
  state$git$n_autores <- length(unique(git$author))
  state$git$commits_author <- git |>
    dplyr::summarise(commits = dplyr::n(), .by = author)

  # TODO: list number of issues and pull requests

  class(state$git) <- "evaluacion_git"

  # Restan
  state$extras$usan_TF <- tools::checkTnF(dir = ruta_paquete)
  state$extras$archivos_extras <- length(grep("Non-standard files?/director(ies|y) found at top level", check)) > 0

  class(state$extras) <- "evaluacion_extras"

  class(state) <- "evaluaciones"
  return(state)
}

utils::globalVariables("author")

