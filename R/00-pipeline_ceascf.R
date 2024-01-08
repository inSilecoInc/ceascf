#' Filtering script for Québec Canadian Wildlife Service contract
#'
#' @keywords filter
#'
#' @export
#'

pipeline_ceascf <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  # Selection of VCs
  berge <- c(
    "naturelle_semi_vegetalisee",
    "naturelle_vive",
    "artificielle_semi_vegetalisee",
    "artificielle_vive"
  )

  habitat <- c(
    "zone_inondable",
    "meuble_sans_falaise",
    "rocheuse_sans_falaise",
    "rocheuse_sans_escarpement",
    "terrasse_fluviale",
    "terrasse_plage",
    "eau_peu_profonde",
    "marais",
    "marecage",
    "milieu_humide",
    "oiseaux",
    "faune_susceptible",
    "faune_vulnerable",
    "faune_menacee",
    "flore_susceptible",
    "flore_vulnerable",
    "flore_menacee",
    "lep_menacee",
    "lep_voie_disparition",
    "frayere",
    "gisement_coquilliers" # ,
    # "biovolume_herbier_faible",
    # "biovolume_herbier_modere",
    # "biovolume_herbier_eleve"
  )

  site <- c(
    "public_milieu_protege" # ,
    # "kahnawake_culture_patrimoine",
    # "kahnawake_chasse",
    # "kahnawake_peche_rivage",
    # "kahnawake_peche_offshore",
    # "kahnawake_sssm",
    # "kahnawake_traffic",
    # "kahnawake_vegatation",
    # "huronne_wendat_activite_recreative",
    # "huronne_wendat_peche",
    # "huronne_wendat_chasse",
    # "huronne_wendat_vegetaux",
    # "huronne_wendat_occupation",
    # "huronne_wendat_archeologie",
    # "huronne_wendat_toponymie",
    # "huronne_wendat_histoire",
    # "huronne_wendat_espece_peril",
    # "gcnwa_gibier",
    # "gcnwa_oiseaux_migrateurs",
    # "gcnwa_animaux_fourrure",
    # "gcnwa_cueillette_collecte",
    # "gcnwa_sites_coucher",
    # "gcnwa_sites_culturels",
    # "gcnwa_sites_essentiels",
    # "gcnwa_problemes_territoire",
    # "gcnwa_zones_activites",
    # "gcnwa_peche",
    # "gcnwa_navigation",
    # "gcnwa_sites_archeologiques",
    # "gcnwa_sites_potentiel_archeologique"
  )

  # VCs
  vcs <- c(
    glue::glue("berge_{berge}"),
    glue::glue("habitat_{habitat}"),
    glue::glue("site_{site}")
  )
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  # Filter and export integrated VC and stressors data
  out <- here::here("data", "ceascf", "data-integrated")
  chk_create(out)

  # Load and export integrated data
  sf::st_read("data/data-integrated/cv_berge.geojson") |>
    dplyr::select(dplyr::matches(berge)) |>
    sf::st_write(here::here(out, "cv_berge.geojson"))

  sf::st_read("data/data-integrated/cv_habitat.geojson") |>
    dplyr::select(dplyr::matches(habitat)) |>
    sf::st_write(here::here(out, "cv_habitat.geojson"))

  sf::st_read("data/data-integrated/cv_site.geojson") |>
    dplyr::select(dplyr::matches(site)) |>
    sf::st_write(here::here(out, "cv_site.geojson"))

  # sf::st_read("data/data-integrated_full/cv_mammiferes_marins.geojson") |>
  #   dplyr::select(dplyr::matches(mammiferes_marins)) |>
  # sf::st_write(here::here(out, "cv_mammiferes_marins.geojson"))

  # Copy stressor data (Filtering option should be added in case stressors are also filtered)
  copy_stress <- function(st) {
    file.copy(
      glue::glue("data/data-integrated/st_{st}.geojson"),
      glue::glue("data/ceascf/data-integrated/st_{st}.geojson")
    )
  }
  copy_stress("ancrage")
  copy_stress("deversement")
  copy_stress("dragage")
  copy_stress("naufrage")
  copy_stress("navigation")
  copy_stress("peche_commerciale")
  copy_stress("pollution_maritime")
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  # Filter vulnerability data
  vulnerability <- read.csv("./data/data-integrated/vulnerability.csv", row.names = 1)
  uid <- rownames(vulnerability) %in% vcs
  vulnerability <- vulnerability[uid, ]
  write.csv(
    vulnerability,
    here::here(out, "vulnerability.csv"),
    row.names = TRUE
  )
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  # Ananlyses
  # Stresseurs et composantes valorisées
  ana_stresseurs_raw()
  ana_composantes_valorisees_raw()

  # Transformer stresseurs et composantes valorisées
  ana_stresseurs_format()
  ana_composantes_valorisees_format()

  # Stresseurs, hotspots et composantes valorisées cumulés
  ana_cumulative_stresseurs()
  ana_cumulative_hotspots()
  ana_cumulative_composantes_valorisees()

  # Cumulative exposure
  ana_cumulative_exposure()

  # Cumulative effects
  ana_cumulative_effects()

  # Cumulative effects per km2 for valued components
  ana_cumulative_effects_cv_km2()

  # Cumulative effects per km2 for administrative regions
  ana_cumulative_effects_region_km2()


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~








  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  # Filter VC outputs
  out <- here::here("data", "ceascf", "data-output")
  chk_create(out)
  filt <- function(fl) {
    sf::st_read(glue::glue("data/data-output/{fl}.geojson")) |>
      dplyr::select(dplyr::matches(vcs)) |>
      sf::st_write(glue::glue("data/data-output/{fl}.geojson"))
  }
  dat <- c("composantes_valorisees_raw", "composantes_valorisees_format", "cumulative_composantes_valorisees")
  lapply(dat, filt)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~

  # Copy stressor data (Filtering option should be added in case stressors are also filtered)
  file.copy(
    "data/data-output_full/stresseurs_raw.geojson",
    "data/data-output/stresseurs_raw.geojson"
  )
  file.copy(
    "data/data-output_full/stresseurs_format.geojson",
    "data/data-output/stresseurs_format.geojson"
  )


  # Analyses
  # Stresseurs, hotspots et composantes valorisées cumulés
  ana_cumulative_stresseurs()
  ana_cumulative_hotspots()
  ana_cumulative_composantes_valorisees()

  # Cumulative exposure
  ana_cumulative_exposure()

  # Cumulative effects
  ana_cumulative_effects()

  # Cumulative effects per km2 for valued components
  ana_cumulative_effects_cv_km2()

  # Cumulative effects per km2 for administrative regions
  ana_cumulative_effects_region_km2()
}
