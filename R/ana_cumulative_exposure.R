#' Exposition cumulée
#'
#' Évaluation de l'exposition cumulée des composantes valorisées aux stresseurs environnementaux
#'
#' @keywords exposition cumulée
#' @keywords stresseurs
#' @keywords composantes valorisées
#'
#' @export
#'
#' @details Cette fonction effectue une partie des analyses du projet d'évaluation des effets cumulatifs
#'

ana_cumulative_exposure <- function(st_files = NULL, cv_files = NULL, out = "data/data-output") {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # L'évaluation de l'exposition cumulée fournit une évaluation des milieux
  # où il y a un chevauchement plus important entre les composantes valorisées
  # et les stresseurs environnementaux. Bien que cette étape n'établisse pas une
  # prédiction de l'effets des stresseurs sur les composantes valorisées, qui
  # qui nécessiterait une évaluation de la sensibilité des composantes valorisées
  # aux stresseurs, elle permet tout de même d'identifier les milieux où les
  # composantes valorisées sont le plus susceptibles d'être soumises aux effets
  # des stresseurs environnementaux considérés.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # -----
  data(grid1p)

  # -----
  if (is.null(st_files)) {
    load_output("cumulative_stresseurs")
  } else {
    cumulative_stresseurs <- sf::st_read(st_files)
  }

  if (is.null(cv_files)) {
    load_output("cumulative_composantes_valorisees")
  } else {
    cumulative_composantes_valorisees <- sf::st_read(cv_files)
  }

  st <- st_drop_geometry(cumulative_stresseurs)
  cv <- st_drop_geometry(cumulative_composantes_valorisees)

  # -----
  cm <- list()
  cm$cumulative_exposure <- st$cumulative_st * cv$cumulative_cv
  cm$cumulative_exposure_norm <- st$cumulative_st_norm * cv$cumulative_cv_norm
  cm$cumulative_exposure_berge <- st$cumulative_st_norm * cv$cumulative_cv_berge
  cm$cumulative_exposure_habitat <- st$cumulative_st_norm * cv$cumulative_cv_habitat
  cm$cumulative_exposure_mammiferes_marins <- st$cumulative_st_norm * cv$cumulative_cv_mammiferes_marins
  cm$cumulative_exposure_site <- st$cumulative_st_norm * cv$cumulative_cv_site
  cm <- cm |> keep(~ length(.x) > 0)

  # -----
  cumulative_exposure <- list(
    grid1p = grid1p,
    cm
  ) |>
    dplyr::bind_cols()

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(
    obj = cumulative_exposure,
    dsn = here::here(out, "cumulative_exposure.geojson"),
    delete_dsn = TRUE,
    quiet = TRUE
  )
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
