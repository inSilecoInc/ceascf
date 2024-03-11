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
  # Filter VC and stressor output files for analyses
  out <- here::here("data", "ceascf", "data-output")
  chk_create(out)
  filt <- function(fl) {
    x <- sf::st_read(glue::glue("data/data-output/{fl}.geojson")) |>
      dplyr::select(dplyr::matches(vcs)) |>
      sf::st_write(glue::glue("data/ceascf/data-output/{fl}.geojson"))
  }
  dat <- c("composantes_valorisees_raw", "composantes_valorisees_format")
  lapply(dat, filt)

  # Copy stressor data (Filtering option should be added in case stressors are also filtered)
  copy_out <- function(files) {
    file.copy(
      glue::glue("data/data-output/{files}.geojson"),
      glue::glue("data/ceascf/data-output/{files}.geojson")
    )
  }
  copy_out("stresseurs_raw")
  copy_out("stresseurs_format")
  copy_out("cumulative_stresseurs")
  copy_out("cumulative_hotspots")
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  # Ananlyses
  out <- here::here("data", "ceascf", "data-output")
  chk_create(out)

  # Cumulative composantes valorisees
  ana_cumulative_composantes_valorisees(
    cv_files = here::here(out, "composantes_valorisees_format.geojson"),
    out = out
  )

  # Cumulative exposure
  out <- here::here("data", "ceascf", "data-output")
  ana_cumulative_exposure(
    st_files = here::here(out, "cumulative_stresseurs.geojson"),
    cv_files = here::here(out, "cumulative_composantes_valorisees.geojson"),
    out = out
  )

  # Cumulative effects
  out <- here::here("data", "ceascf", "data-output")
  ana_cumulative_effects(
    st_files = here::here(out, "stresseurs_format.geojson"),
    cv_files = here::here(out, "composantes_valorisees_format.geojson"),
    vuln_files = here::here("data", "ceascf", "data-integrated", "vulnerability.csv"),
    out = out
  )

  # Cumulative effects per km2 for valued components
  out <- here::here("data", "ceascf", "data-output")
  ana_cumulative_effects_cv_km2(
    st_files = here::here(out, "stresseurs_format.geojson"),
    cv_files = here::here(out, "composantes_valorisees_format.geojson"),
    out = out
  )

  # Cumulative effects per km2 for administrative regions
  out <- here::here("data", "ceascf", "data-output")
  ana_cumulative_effects_region_km2(
    st_files = here::here(out, "stresseurs_format.geojson"),
    cv_files = here::here(out, "composantes_valorisees_format.geojson"),
    out = out
  )
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  # Figures
  out <- here::here("figures", "ceascf")
  chk_create(out)

  # Integrated figures (not necessary)

  # Stressors
  files <- dir(here::here("figures", "figures-output"))
  files <- files[stringr::str_detect(files, "_st_")]
  input <- here::here("figures", "figures-output")
  output <- here::here("figures", "ceascf", "figures-output")
  for (i in 1:length(files)) {
    file.copy(
      here::here(input, files[i]),
      here::here(output, files[i])
    )
  }
  file.copy(
    here::here(input, "cumulative_st.png"),
    here::here(output, "cumulative_st.png")
  )
  file.copy(
    here::here(input, "cumulative_hotspots.png"),
    here::here(output, "cumulative_hotspots.png")
  )

  # Analyses
  fig_cumulative_composantes_valorisees_ceascf()
  fig_cumulative_exposure_ceascf()
  fig_cumulative_effects_ceascf()
  fig_regional_contribution_ceascf()
  fig_metanetwork_ceascf()
  fig_region_cea_km2_ceascf()
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  # Publications
  render_frontpage()
  render_report()
  render_prez()

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  # Export des données spatiales à fournir au SCF
  #  - Données spatiales en format geopackage (.gpkg)
  #  - Projection NAD 1983 Quebec Lambert Conique Conforme

  # Function to export geopackages
  exp_gpkg <- function(input, output, name) {
    # Output folder
    chk_create(output)

    # Files
    files <- dir(input, pattern = "geojson")

    # Names
    nm <- tools::file_path_sans_ext(files)

    # Load and export first file
    sf::st_read(here::here(input, files[1]), quiet = TRUE) |>
      sf::st_write(here::here(output, glue::glue("{name}.gpkg")), nm[1])

    # Add rest of files to geopackage
    for (i in 2:length(files)) {
      sf::st_read(here::here(input, files[i]), quiet = TRUE) |>
        sf::st_write(here::here(output, glue::glue("{name}.gpkg")), nm[i], append = TRUE)
    }
  }

  # Formatted data
  exp_gpkg(
    input = here::here("data", "ceascf", "data-format"),
    output = here::here("data", "ceascf", "geopackages"),
    name = "formatted_data"
  )

  # Integrated data
  exp_gpkg(
    input = here::here("data", "ceascf", "data-integrated"),
    output = here::here("data", "ceascf", "geopackages"),
    name = "model_input_data"
  )

  # Model outputs
  exp_gpkg(
    input = here::here("data", "ceascf", "data-output"),
    output = here::here("data", "ceascf", "geopackages"),
    name = "model_output_data"
  )


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
}
