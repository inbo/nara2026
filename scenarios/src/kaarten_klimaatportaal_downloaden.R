#///////////////////////////////////////////////////
# Plunder de kaartencatalogus van het klimaatportaal
#///////////////////////////////////////////////////

# Via het script kan je voor een gebied naar keuzse (gebied.shp) de gewenste kaarten van het klimaatportaal downloaden
# Het script verdeeld het gebied in bounding boxes van 10 x 10 km, download de kaarten per box, voegt ze samen en zet ze ten slotte om naar een raster met een resolutie van 10 x 10 m.

library(sf)
library(terra)
library(dplyr)

studiegebied <- "kleine_nete"

# 1. Mapstructuur
str_c("C:/R/NARA2026/nara2026-git/scenarios/", studiegebied, "/data/gebied.shp")
shape_pad <- str_c("C:/R/NARA2026/nara2026-git/scenarios/", studiegebied, "/data/gebied.shp")
output_map <- "C:/R/NARA2026/vmm_downloads"

# Maak een map aan voor de losse tegels als deze nog niet bestaat
if (!dir.exists(output_map)) dir.create(output_map)

# 2. Gebied inlezen en grid maken voor download
cat("Studiegebied inlezen en opknippen...\n")
gebied <- st_read(shape_pad) %>% 
  st_transform(31370) # Cruciaal: Dwing naar Lambert 72 (meters)

# Maak een grid van 10.000m x 10.000m over het gebied
grid_totaal <- st_make_grid(gebied, cellsize = 7000, square = TRUE)

# Selecteer alleen de grid-cellen die je studiegebied daadwerkelijk raken
grid_selectie <- grid_totaal[st_intersects(grid_totaal, gebied, sparse = FALSE)]
st_write(st_as_sf(grid_selectie), file.path(output_map, "download_grid.gpkg"), delete_dsn = TRUE)

cat("Totaal aantal te downloaden tegels:", length(grid_selectie), "\n")

# 3. URL voor download definiëren
# Om de url op te halen, download je eerst een voorbeeldgemeente en
# kopieer je de coverageId uit de downloadlink (Google -> downloads)
# vervang de coverageId in onderstaande url

# Agrarische droogte
base_url <- paste0(
  "https://kaartencatalogus.toepassingen.vmm.vlaanderen.be/geoserver/wcs?",
  "service=WCS&version=2.0.0&request=GetCoverage&",
  "coverageId=effectenenimpacts:s0_droogteduurAgrarischExtreemDagen_huidig_2019_t10&",
  "outputCRS=http://www.opengis.net/def/crs/EPSG/0/31370&",
  "subsettingcrs=http://www.opengis.net/def/crs/EPSG/0/31370"
)

# Maatregelenscenario 7/3/2
base_url <- paste0(
  "https://kaartencatalogus.toepassingen.vmm.vlaanderen.be/geoserver/wcs?",
  "service=WCS&version=2.0.0&request=GetCoverage&",
  "coverageId=planningtool:maatregelscenario_s2_totaal&",
  "outputCRS=http://www.opengis.net/def/crs/EPSG/0/31370&",
  "subsettingcrs=http://www.opengis.net/def/crs/EPSG/0/31370"
)



# --- 4. DE DOWNLOAD LOOP ---
cat("Start met downloaden van de tegels...\n")

i <- 19

for (i in 1:length(grid_selectie)) {
  # Haal de bounding box op van de huidige gridcel
  bbox <- st_bbox(grid_selectie[i])
  
  # Afronden op hele meters voor schone URL's
  xmin <- floor(bbox$xmin)
  xmax <- ceiling(bbox$xmax)
  ymin <- floor(bbox$ymin)
  ymax <- ceiling(bbox$ymax)
  
  # Bouw de specifieke downloadlink voor deze tegel
  download_url <- paste0(base_url, "&subset=X(", xmin, ",", xmax, ")&subset=Y(", ymin, ",", ymax, ")")
  dest_file <- file.path(output_map, sprintf("tegel_%d_%d.tif", xmin, ymin))
  
  cat(sprintf("[%d/%d] Downloaden tegel X: %d-%d, Y: %d-%d...\n", i, length(grid_selectie), xmin, xmax, ymin, ymax))
  
  # Download de file (met een tryCatch voor het geval een tegel leeg is of de server weigert)
  tryCatch({
    download.file(url = download_url, destfile = dest_file, mode = "wb", quiet = TRUE)
  }, error = function(e) {
    cat("Fout bij downloaden van deze tegel, waarschijnlijk buiten data-omvang. Sla over.\n")
  })
}

# 5. Rasters samenvoegen (mozaïek) ---
cat("\nDownloads voltooid. Start samenvoegen van rasters...\n")

# Zoek alle succesvol gedownloade .tif bestanden op
tif_files <- list.files(output_map, pattern = "\\.tif$", full.names = TRUE)

if (length(tif_files) == 0) stop("Geen succesvolle downloads gevonden om samen te voegen.")

# Gebruik een Virtueel Raster (VRT) voor het samenvoegen.
# Dit werkt exact zoals 'Mosaic to New Raster' in ArcMap, maar kost 0 MB werkgeheugen
# omdat het pas fysiek wordt weggeschreven bij de volgende stap.
mozaiek_virtueel <- vrt(tif_files)

# 6. Opschalen resolutie (Van 1x1m naar 10x10m) ---
cat("Resolutie opschalen van 1m naar 10m...\n")

# Gebruik fun = "modal" als het om klasse-kaarten (bodem/gewas) gaat.
raster_10m <- aggregate(mozaiek_virtueel, fact = 10, fun = "modal")

# 7. Opkuisen en opslaan
cat("Resultaat wegschrijven naar schijf...\n")

# Snij het resultaat bij op basis van gebied.shp
raster_10m_clipped <- mask(crop(raster_10m, vect(gebied)), vect(gebied))

# Sla het definitieve bestand efficiënt op met DEFLATE compressie
writeRaster(raster_10m_clipped, 
            filename = paste0(output_map, "/klimportaal_sc2.tif"), 
            overwrite = TRUE,
            datatype = "FLT4S", # Behoud decimalen
            gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"))

cat("Proces succesvol afgerond! Het bestand staat klaar.\n")
