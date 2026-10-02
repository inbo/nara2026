run_invest_ndr <- function(studiegebied = "kleine_nete", kaart = "ecosysteem_2022.tif", suffix = "") {
  
  # ==============================================================================
  # 0. CONTROLE EN AUTOMATISCHE ACTIVATIE VAN PACKAGES EN OMGEVING
  # ==============================================================================
  nodige_packages <- c("reticulate", "tidyverse", "terra")
  ontbrekende_packages <- c()
  
  for (pkg in nodige_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      ontbrekende_packages <- c(ontbrekende_packages, pkg)
    } else {
      if (!paste0("package:", pkg) %in% search()) {
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      }
    }
  }
  
  invest_omgeving_ok <- FALSE
  if (length(ontbrekende_packages) == 0) {
    invest_omgeving_ok <- tryCatch({
      use_condaenv("env-invest", required = TRUE)
      test_import <- import("natcap.invest.ndr.ndr")
      TRUE
    }, error = function(e) { FALSE })
  }
  
  if (length(ontbrekende_packages) > 0 || !invest_omgeving_ok) {
    stop("Conda-omgeving of benodigde R-packages ontbreken.", call. = FALSE)
  }
  
  if (!exists("invest_ndr", envir = .GlobalEnv) || !exists("logging", envir = .GlobalEnv)) {
    use_condaenv("env-invest", required = TRUE)
    invest_ndr <<- import("natcap.invest.ndr.ndr")
    logging    <<- import("logging")
  }
  
  # ==============================================================================
  # 1. GEÏSOLEERDE MAPSTRUCTUUR PER RUN (THREAD-SAFE)
  # ==============================================================================
  clean_suffix <- if (nchar(suffix) > 0 && !startsWith(suffix, "_")) paste0("_", suffix) else suffix
  folder_naam  <- if (nchar(suffix) > 0) gsub("^_", "", clean_suffix) else "referentie"
  
  workspace_path_ndr    <- file.path("C:/GIS/NARA2026/invest/R/outputs", studiegebied, "NDR", folder_naam)
  workspace_path_swy    <- file.path("C:/GIS/NARA2026/invest/R/outputs", studiegebied, folder_naam)
  intermediate_path <- file.path(workspace_path_ndr, "intermediate_outputs")
  if (!dir.exists(workspace_path_ndr)) dir.create(workspace_path_ndr, recursive = TRUE)
  
  scenarios_dir <- file.path("C:/R/NARA2026/nara2026-git/scenarios", studiegebied, "output")
  gis_data_dir  <- "C:/GIS/NARA2026/invest/data"
  
  path_lulc     <- file.path(scenarios_dir, kaart)
  path_bio_in   <- file.path(gis_data_dir, "biophysical_table_nutrient.csv")
  path_qf <- list.files(path = workspace_path_swy, pattern = "^QF.*\\.tif$", full.names = TRUE)

  # 2. Biophysical tabel inlezen en checken op LULC codes
  df_bio <- read_delim(path_bio_in, delim = ",", locale = locale(decimal_mark = "."), show_col_types = FALSE) %>% 
    rename_with(tolower)
  
  r_lulc <- rast(path_lulc)
  raster_codes <- unique(r_lulc)[[1]]
  raster_codes <- raster_codes[!is.na(raster_codes)]
  missing_codes <- setdiff(raster_codes, df_bio$lucode)
  
  if (length(missing_codes) > 0) {
    r_lulc <- classify(r_lulc, cbind(missing_codes, NA))
  }
  
  # ==============================================================================
  # 3. MODELPARAMETERS INSTELLEN
  # ==============================================================================
  args <- list(
    workspace_dir  = workspace_path_ndr,
    results_suffix = suffix,
    
    dem_path               = file.path(gis_data_dir, "dhm_10m.tif"),
    lulc_path              = path_lulc,
    runoff_proxy_path      = path_qf,
    watersheds_path        = file.path(scenarios_dir, "gebied.shp"),
    biophysical_table_path = path_bio_in,
    
    calc_p = TRUE,
    calc_n = TRUE,
    subsurface_critical_length_n = 200,
    subsurface_eff_n             = 0.9,
    
    flow_dir_algorithm    = "MFD",
    threshold_flow_accumulation = 1750L,
    k_param                     = 2.0
  )
  
  logging$basicConfig(
    level   = logging$INFO,
    format  = "%(asctime)s [%(levelname)s] %(message)s",
    datefmt = "%H:%M:%S",
    force   = TRUE
  )
  
  # Start simulatie
  invest_ndr$execute(args)
  
  # ==============================================================================
  # 4. OUTPUTS MASKEREN EN CLEANUP
  # ==============================================================================
  
  # Geselecteerde bestanden in intermediate folder wegschrijven naar hoofdfolder
  if (dir.exists(intermediate_path)) {
    load_patroon <- paste0("^modified_load_[np]", clean_suffix, "\\.(tif|tif\\.aux\\.xml|tfw|prj)$")
    load_bestanden <- list.files(intermediate_path, pattern = load_patroon, full.names = TRUE)
    
    if (length(load_bestanden) > 0) {
      file.rename(from = load_bestanden, to = file.path(workspace_path_ndr, basename(load_bestanden)))
    }
  }
  
  # Outputs NDR
  tif_patroon <- paste0("^(n_total_export|p_surface_export|modified_load_n|modified_load_p)", clean_suffix, "\\.tif$")
  tif_outputs <- list.files(workspace_path_ndr, pattern = tif_patroon, full.names = TRUE, recursive = TRUE) # recursive = TRUE -> ook in subfolders zoeken
  
  for (f in tif_outputs) {
    r_out <- rast(f)
    r_lulc_aligned <- r_lulc
    crs(r_lulc_aligned) <- crs(r_out)
    
    r_lulc_aligned <- resample(r_lulc_aligned, r_out, method = "near")
    r_out_masked   <- mask(r_out, r_lulc_aligned)
    
    writeRaster(r_out_masked, f, overwrite = TRUE)
  }
  
  # Verwijder intermediate_outputs map
  if (dir.exists(intermediate_path)) {
    unlink(intermediate_path, recursive = TRUE, force = TRUE)
  }
  
  # Verwijder alle overige bestanden die we niet expliciet willen behouden (bijv. .txt logs of ongewenste rasters)
  # Hier behouden we de core TIFs en optioneel de resulterende CSV rapportages (.csv)
  te_behouden_patroon <- paste0("^(n_total_export|p_surface_export|modified_load_n|modified_load_p)", clean_suffix, "\\.(tif|csv|tif\\.aux\\.xml|tfw|prj)$")
  alle_items <- list.files(workspace_path_ndr, full.names = TRUE, include.dirs = FALSE)
  items_om_te_verwijderen <- alle_items[!grepl(te_behouden_patroon, basename(alle_items), ignore.case = TRUE)]
  
  unlink(items_om_te_verwijderen, force = TRUE)
}