run_invest_swy <- function(studiegebied = "kleine_nete", kaart = "ecosysteem_2022.tif", suffix = "") {
  
  # ==============================================================================
  # 0. CONTROLE EN AUTOMATISCHE ACTIVATIE VAN PACKAGES EN OMGEVING
  # ==============================================================================
  
  # 0a. Controleer en laad benodigde R-packages automatisch
  nodige_packages <- c("reticulate", "tidyverse", "terra")
  ontbrekende_packages <- c()
  
  for (pkg in nodige_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      ontbrekende_packages <- c(ontbrekende_packages, pkg)
    } else {
      # Laad het package als het nog niet geactiveerd is in de zoekpaden
      if (!paste0("package:", pkg) %in% search()) {
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      }
    }
  }
  
  # 0b. Controleer Python, Miniconda en InVEST-omgeving
  invest_omgeving_ok <- FALSE
  if (length(ontbrekende_packages) == 0) {
    invest_omgeving_ok <- tryCatch({
      use_condaenv("env-invest", required = TRUE)
      test_import <- import("natcap.invest.seasonal_water_yield.seasonal_water_yield")
      TRUE
    }, error = function(e) {
      FALSE
    })
  }
  
  # 0c. Onderbreek met duidelijke instructies als er iets ontbreekt
  if (length(ontbrekende_packages) > 0 || !invest_omgeving_ok) {
    stop(
      "\n\n[FOUT] De benodigde software of Conda-omgeving is niet (volledig) geïnstalleerd.\n",
      "Voer eenmalig de volgende stappen uit in R voordat je de functie gebruikt:\n\n",
      "1. Laad/installeer de benodigde R-packages:\n",
      "   install.packages(c('reticulate', 'tidyverse', 'terra'))\n\n",
      "2. Installeer Python v3.11:\n",
      "   reticulate::install_python(version = '3.11')\n\n",
      "3. Installeer Miniconda:\n",
      "   reticulate::install_miniconda()\n\n",
      "4. Maak de Conda-omgeving aan:\n",
      "   reticulate::conda_create('env-invest', python_version = '3.11')\n\n",
      "5. Installeer InVEST via conda-forge:\n",
      "   reticulate::conda_install(envname = 'env-invest', packages = 'natcap.invest', channel = 'conda-forge')\n\n",
      call. = FALSE
    )
  }
  
  # 0d. Eenmalig inladen van Python-modules in .GlobalEnv als ze er nog niet zijn
  if (!exists("invest_swy", envir = .GlobalEnv) || !exists("logging", envir = .GlobalEnv)) {
    message("Python-omgeving koppelen en InVEST-modules laden...")
    use_condaenv("env-invest", required = TRUE)
    invest_swy <<- import("natcap.invest.seasonal_water_yield.seasonal_water_yield")
    logging    <<- import("logging")
  }
  
  # ==============================================================================
  # 1. HOOFDFUNCTIE
  # ==============================================================================
  
  # 1. Paden opbouwen
  scenarios_dir <- file.path("C:/R/NARA2026/nara2026-git/scenarios", studiegebied, "data")
  gis_data_dir  <- "C:/GIS/NARA2026/invest/data"
  
  path_lulc     <- file.path(scenarios_dir, kaart)
  path_soil     <- file.path(gis_data_dir, "bodem_hsg_lucorrected.tif")
  path_bio_in   <- file.path(gis_data_dir, "biophysical_table.csv")
  path_rain_in  <- file.path(gis_data_dir, "rain_events.csv")
  path_soil_out <- file.path(scenarios_dir, "bodem_hsg_lucorrected.tif")
  
  # 2. Biophysical tabel inlezen (nodig voor LULC-code controle)
  df_bio <- read_delim(path_bio_in, delim = ",", locale = locale(decimal_mark = "."), show_col_types = FALSE) %>% 
    rename_with(tolower)
  
  # 3. Klimaat-rastertabellen genereren
  precip_dir <- file.path(gis_data_dir, "precip_klimportaal_2020")
  et0_dir    <- file.path(gis_data_dir, "et0_klimportaal_2020")
  
  precip_files <- list.files(precip_dir, pattern = "\\.(tif|TIF|asc)$", full.names = TRUE)
  et0_files    <- list.files(et0_dir, pattern = "\\.(tif|TIF|asc)$", full.names = TRUE)
  
  precip_csv <- file.path(gis_data_dir, "precip_raster_table.csv")
  et0_csv    <- file.path(gis_data_dir, "et0_raster_table.csv")
  
  write_csv(tibble(month = 1:12, path = precip_files), precip_csv)
  write_csv(tibble(month = 1:12, path = et0_files), et0_csv)
  
  # 4. LULC-raster opkuisen op ontbrekende codes
  r_lulc <- rast(path_lulc)
  raster_codes <- unique(r_lulc)[[1]]
  raster_codes <- raster_codes[!is.na(raster_codes)]
  
  missing_codes <- setdiff(raster_codes, df_bio$lucode)
  
  if (length(missing_codes) > 0) {
    message("GEVONDEN: De volgende LULC-codes in het raster ontbreken in biophysical_table: ", 
            paste(missing_codes, collapse = ", "))
    r_lulc <- classify(r_lulc, cbind(missing_codes, NA))
    writeRaster(r_lulc, path_lulc, overwrite = TRUE)
  } else {
    message("Alle LULC-rastercodes komen overeen met biophysical_table.csv.")
  }
  
  # 5. Bodemraster snel bijsnijden met buffer
  r_soil <- rast(path_soil)
  aoi_poly <- buffer(as.polygons(ext(r_lulc), crs = crs(r_lulc)), width = 500)
  
  if (crs(r_soil) != crs(r_lulc)) {
    aoi_poly_soil <- project(aoi_poly, crs(r_soil))
    r_soil_sub    <- crop(r_soil, aoi_poly_soil)
    r_soil_sub    <- project(r_soil_sub, crs(r_lulc), method = "near")
  } else {
    r_soil_sub    <- crop(r_soil, aoi_poly)
  }
  
  r_soil_clean <- classify(r_soil_sub, matrix(c(-Inf, 0.5, NA, 4.5, Inf, NA), ncol = 3, byrow = TRUE))
  r_soil_clean <- resample(r_soil_clean, r_lulc, method = "near")
  
  # Facultatief: ontbrekende HSG-cellen opvullen met waarde 2 of 3
  # r_fill <- r_lulc
  # values(r_fill) <- ifelse(is.na(values(r_lulc)), NA, 2)
  # 
  # r_soil_clean <- cover(r_soil_clean, r_fill)
  r_soil_clean <- mask(r_soil_clean, r_lulc)
  
  writeRaster(r_soil_clean, path_soil_out, datatype = "INT1U", overwrite = TRUE)
  
  # 6. Wis eventuele InVEST tussenbestanden van vorige runs
  workspace_path    <- file.path("C:/GIS/NARA2026/invest/R/outputs", studiegebied)
  intermediate_path <- file.path(workspace_path, "intermediate_outputs")
  if (dir.exists(intermediate_path)) {
    unlink(intermediate_path, recursive = TRUE)
  }
  
  # 7. Modelparameters instellen
  args <- list(
    workspace_dir = workspace_path,
    results_suffix = suffix,
    
    lulc_raster_path = path_lulc,
    dem_raster_path  = file.path(gis_data_dir, "dhm_breached_10m.tif"),
    soil_group_path  = path_soil_out,
    aoi_path         = file.path(scenarios_dir, "gebied.shp"),
    
    biophysical_table_path = path_bio_in,
    rain_events_table_path = path_rain_in,
    precip_raster_table    = precip_csv,
    et0_raster_table       = et0_csv,
    
    alpha_m = "1/12",
    beta_i  = "1",
    gamma   = "1",
    threshold_flow_accumulation = 1750L,
    
    monthly_alpha               = FALSE,
    user_defined_local_recharge = FALSE,
    user_defined_climate_zones  = FALSE,
    flow_dir_algorithm    = "MFD"
  )
  
  # Activeer logging (om voortgang van modelrun te volgen)
  logging$basicConfig(
    level = logging$INFO,
    format = "%(asctime)s [%(levelname)s] %(message)s",
    datefmt = "%H:%M:%S",
    force = TRUE
  )
  
  # Start simulatie
  message("Modelrun gestart voor studiegebied '", studiegebied, "' met kaart '", kaart, "'...")
  invest_swy$execute(args)
  message("Modelrun succesvol afgerond")
  
  
  # ==============================================================================
  # Ouputs opkuisen en afstemmen op ecosysteemkaart
  # ==============================================================================
  # InVEST voegt automatisch een '_' toe als suffix niet leeg is en niet met '_' begint
  clean_suffix <- if (nchar(suffix) > 0 && !startsWith(suffix, "_")) paste0("_", suffix) else suffix
  
  # 8. Opkuisen outputfolder
  message("Bestanden opkuisen: overbodige bestanden en de intermediate folder worden verwijderd...")
  
  if (dir.exists(intermediate_path)) {
    # Dynamisch patroon voor 'aet' met suffix
    aet_patroon <- paste0("^aet", clean_suffix, "\\.(tif|tif\\.aux\\.xml|tfw|prj)$")
    
    aet_bestanden <- list.files(intermediate_path, 
                                pattern = aet_patroon, 
                                full.names = TRUE)
    
    if (length(aet_bestanden) > 0) {
      file.rename(from = aet_bestanden, 
                  to = file.path(workspace_path, basename(aet_bestanden)))
    }
  }
  
  # Dynamisch patroon voor alle te behouden bestanden
  te_behouden_patroon <- paste0("^(QF|B|B_sum|L|aet)", clean_suffix, "\\.(tif|tif\\.aux\\.xml|tfw|prj)$")
  
  alle_items <- list.files(workspace_path, full.names = TRUE, include.dirs = TRUE)
  mag_blijven <- grepl(te_behouden_patroon, basename(alle_items))
  items_om_te_verwijderen <- alle_items[!mag_blijven]
  
  unlink(items_om_te_verwijderen, recursive = TRUE, force = TRUE)
  
  # 9. Finale outputs maskeren met de ecosysteemkaart
  message("Outputs afsnijden op basis van NoData-waarden van de ecosysteemkaart...")
  
  # Dynamisch patroon voor enkel de .tif rasters
  tif_patroon <- paste0("^(QF|B|B_sum|L|aet)", clean_suffix, "\\.tif$")
  tif_outputs <- list.files(workspace_path, pattern = tif_patroon, full.names = TRUE)
  
  for (f in tif_outputs) {
    r_out <- rast(f)
    
    r_lulc_aligned <- r_lulc
    crs(r_lulc_aligned) <- crs(r_out)
    
    r_lulc_aligned <- resample(r_lulc_aligned, r_out, method = "near")
    r_out_masked   <- mask(r_out, r_lulc_aligned)
    
    writeRaster(r_out_masked, f, overwrite = TRUE)
  }
  
  message("Proces afgerond. Enkel de gemaskerde bestanden blijven over in ", workspace_path)
}