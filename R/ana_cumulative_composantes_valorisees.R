#' Empreinte cumulée des composantes valorisées
#'
#' Évaluation de l'empreinte cumulée des composantes valorisées considérés
#'
#' @keywords empreinte cumulée
#' @keywords composantes valorisées
#'
#' @export
#'
#' @details Cette fonction effectue une partie des analyses du projet d'évaluation des effets cumulatifs
#'

ana_cumulative_composantes_valorisees <- function(cv_files = NULL, out = "data/data-output") {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # L'évaluation de l'empreinte cumulée permet d'identifier les sites qui sont
  # le plus importants en terme de présentes de composantes valorisées. Elle ne
  # fournit pas d'évaluation des effets, puisqu'elle ne considère que les
  # composantes valorisées. Elle permet toutefois d'obtenir une évaluation des
  # milieux qui sont les plus importants pour les composantes valorisées dans
  # la zone d'étude.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # -----
  data(grid1p)

  # -----
  if (is.null(cv_files)) {
    load_output("composantes_valorisees_format")
  } else {
    composantes_valorisees_format <- sf::st_read(cv_files)
  }
  cv <- st_drop_geometry(composantes_valorisees_format)

  # -----
  cv_cumul <- function(x, cv, normaliser = FALSE) {
    uid <- str_detect(colnames(x), cv)
    if (sum(uid > 0)) {
      dat <- cumulativeFootprint(x[, uid, drop = FALSE], normaliser)
    } else {
      dat <- NULL
    }
    dat
  }

  # -----
  cv_list <- list()
  cv_list$berge <- cv_cumul(cv, "berge", normaliser = TRUE)
  cv_list$habitat <- cv_cumul(cv, "habitat", normaliser = TRUE)
  cv_list$mammiferes_marins <- cv_cumul(cv, "mammiferes_marins", normaliser = TRUE)
  cv_list$site <- cv_cumul(cv, "site", normaliser = TRUE)
  cv_list <- cv_list |> keep(~ length(.x) > 0)
  cumulative_cv_norm <- dplyr::bind_cols(cv_list) |> rowSums()


  # -----
  cumulative_cv <- cumulativeFootprint(cv)

  # -----
  cv_list <- list()
  cv_list$cumulative_cv_berge <- cv_cumul(cv, "berge")
  cv_list$cumulative_cv_habitat <- cv_cumul(cv, "habitat")
  cv_list$cumulative_cv_mammiferes_marins <- cv_cumul(cv, "mammiferes_marins")
  cv_list$cumulative_cv_site <- cv_cumul(cv, "site")
  cv_list <- cv_list |> keep(~ length(.x) > 0)

  # -----
  cumulative_composantes_valorisees <- list(
    grid1p = grid1p,
    cumulative_cv = cumulative_cv,
    cumulative_cv_norm = cumulative_cv_norm,
    cv_list
  ) |>
    dplyr::bind_cols()


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(
    obj = cumulative_composantes_valorisees,
    dsn = here::here(out, "cumulative_composantes_valorisees.geojson"),
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
