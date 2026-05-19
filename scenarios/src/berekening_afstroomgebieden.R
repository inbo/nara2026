library(tidyverse)
library(sf)
library(terra)
library(whitebox)
library(mapview)
library(RColorBrewer)

#/////////////////////
# A. Data inlezen ####
#/////////////////////

setwd(dir = "C:/R/NARA2026/scenarios")

wbt_init() # Initialiseer WhiteboxTools

# dtm_path <- "C:/GIS/NARA2026/invest/data/dhm_10m.tif" # dtm 10m
dtm_path <- "C:/R/NARA2026/scenarios/data/dhm_breached_50m.tif" # dtm 50m breached
buffer_path <- "C:/R/NARA2026/scenarios/data/rivierb_buf.shp" # shape studiegebied + 1.5 km buffer
sub_basins_path <- "C:/R/NARA2026/scenarios/data/rivierb_sbzshp.shp" # SBZ-gebieden in studiegebied
vha_path <- "C:/R/NARA2026/scenarios/rivierbeek/data/vha.shp" # vha in studiegebied

output_dir <- "C:/R/NARA2026/scenarios/output_watersheds/" # Om de afstroomgebieden weg te schrijven
if(!dir.exists(output_dir)) dir.create(output_dir)

dtm_full <- rast(dtm_path)
buffer_poly <- st_read(buffer_path)
sb_poly <- st_read(sub_basins_path)
vha <- st_read(vha_path) %>% st_make_valid() %>% st_transform(crs(sb_poly))

#///////////////////////
# B. Data aanpassen ####
#///////////////////////

# sbz-polygonen opknippen met vha -> om te vermijden dat watershed het hele afstroomgebied van een rivier meeneemt als de sbz-polygonen de rivieren bevatten
vha_buf <- st_buffer(vha, dist = 10) # buffer van 10m langs vha (aan te passen)
vha_buf <- st_union(vha_buf)
# sb_poly_gat <- st_difference(sb_poly, vha_buf) %>% st_cast("POLYGON") %>% # Alle verknipte polygonen apart
#   mutate(id = row_number())
sb_poly_gat <- st_difference(sb_poly, vha_buf) %>% # verknipte polygonen bijeen houden
  mutate(id = row_number())

# Clip DTM
dtm_clip <- crop(dtm_full, buffer_poly) |>
  mask(buffer_poly)
writeRaster(dtm_clip, "temp_dtm_raw.tif", overwrite = TRUE) # Voor gebruik in Whitebox

# VHA naar raster om als barrières in DTM te branden
wbt_vector_lines_to_raster(input = vha_path, output = "vha_raster.tif", base = "temp_dtm_raw.tif")

# 'Burn' de rivieren in de DTM -> cruciaal, anders wordt het hele stroomgebied geselecteerd
wbt_fill_burn(dem = "temp_dtm_raw.tif", streams = vha_path, output = "dtm_burned.tif")

#////////////////////////////
# C. Watershed berekenen ####
#////////////////////////////

# Hydrologische correctie -> niet nodig als je dtm_breached gebruikt (is al gesmooth)
# wbt_fill_depressions(dem = "dtm_burned.tif", output = "temp_dtm_filled.tif")

# Flow Direction
# wbt_d8_pointer(dem = "temp_dtm_filled.tif", output = "temp_flow_ptr.tif") # voor dtm_10m
wbt_d8_pointer(dem = "dtm_burned.tif", output = "temp_flow_ptr.tif")

# Afstroomgebied van alle deelpolygonen berekenen -> zie X. Extra voor aparte polygonen
afstroomgebieden_list <- list()

for (i in 1:nrow(sb_poly_gat)) {
  message(paste0("Verwerken van deelgebied ", i, "..."))
  # Selecteer deelgebied
  single_sbz <- sb_poly_gat[i, ]
  write_sf(single_sbz, "temp_poly.shp", delete_dsn = TRUE)
  
  # Maak raster van de polygoon voor Whitebox
  sbz_rast_path <- paste0("temp_sbz_", i, ".tif")
  wbt_vector_polygons_to_raster(
    input = "temp_poly.shp",
    output = sbz_rast_path,
    field = "id",
    nodata = TRUE,
    cell_size = 10,
    base = "dtm_burned.tif"
  )
  
  # Watershed berekening
  output_name <- paste0(output_dir, "afstroomgebied_sbz_", i, ".tif")
  wbt_watershed(
    d8_pntr = "temp_flow_ptr.tif",
    pour_pts = sbz_rast_path,
    output = output_name
  )
  afstroomgebieden_list[[i]] <- rast(output_name)
}

# Opruimen tijdelijke bestanden
verwijder_tifs <- paste0("temp_sbz_", 1:nrow(sb_poly_gat), ".tif")

file.remove(c("temp_poly.shp", "temp_poly.shx", "temp_poly.dbf", "temp_poly.prj", "temp_dtm_filled.tif", "temp_dtm_raw.tif", verwijder_tifs))

final_stack <- rast(afstroomgebieden_list)

plot(afstroomgebieden_list[[1]]) # plot 1 infiltratiegebied

#///////////////////////////////
# D. Visualiseren resultaat ####
#///////////////////////////////

## D.1. Methode als je veel polygonen hebt (bv. 30)####
raster_lijst_old <- lapply(as.list(final_stack), raster) # omzetten naar raster ipv spatraster

start_index <- which(sapply(raster_lijst_old, function(r) !all(is.na(values(r)))))[1]

if (is.na(start_index)) {
  stop("Alle rasters zijn leeg! Controleer je watershed-berekening.")
}
m <- mapview(raster_lijst_old[[start_index]], 
             col.regions = terrain.colors(10), 
             layer.name = paste("Watershed", start_index),
             na.color = "transparent")
for (i in (start_index + 1):length(raster_lijst_old)) {
    if (!all(is.na(values(raster_lijst_old[[i]])))) {
    m <- m + mapview(raster_lijst_old[[i]], 
                     col.regions = terrain.colors(10), 
                     layer.name = paste("Watershed", i),
                     na.color = "transparent")
  } else {
    message(paste("Skipping empty raster:", i))
  }
}
m + mapview(sb_poly_gat)

## D.2. Methode als je een paar polygonen hebt (bv. 3)####

# Kleurenpallet herschalen op basis van stdev voor meer contrast (cf. ArcMap)
stats <- global(dtm_clip, fun=c("mean", "sd"), na.rm=TRUE)
sd_min <- stats$mean - (2 * stats$sd)
sd_max <- stats$mean + (2 * stats$sd)

# Elk raster apart toevoegen
mapview(dtm_clip, at = seq(sd_min, sd_max, length.out = 255), col.regions = terrain.colors(255), na.color = "transparent", maxpixels = 10000000) + mapview(final_stack$afstroomgebied_sbz_1, col.regions = "cyan", na.color = "transparent", maxpixels = 10000000) + mapview(final_stack$afstroomgebied_sbz_2, col.regions = "#FFD700", na.color = "transparent", maxpixels = 10000000) + mapview(final_stack$afstroomgebied_sbz_3, col.regions = "#A0522D", na.color = "transparent", maxpixels = 10000000) + mapview(sb_poly_gat, alpha.regions = 0)

# Resample wanneer de dtm_breached gebruikt is (heeft resolutie 50m)
resample(rast(extent=ext(buffer_poly), res=10, crs=crs(dtm_full)), method="bilinear")

#//////////////
# X. Extra ####
#//////////////

# Als je het infiltratiegebied van één polygoon wil berekenen
i <- 7 # kies id polygoon
single_sbz <- sb_poly_gat[i, ]
write_sf(single_sbz, "temp_poly.shp", delete_dsn = TRUE)
sbz_rast_path <- paste0("temp_sbz_", i, ".tif")
wbt_vector_polygons_to_raster(
  input = "temp_poly.shp",
  output = sbz_rast_path,
  field = "id",
  nodata = TRUE,
  cell_size = 10,
  base = "dtm_burned.tif"
)
output_name <- paste0("afstroomgebied_sbz_", i, ".tif")

wbt_watershed(
  d8_pntr = "temp_flow_ptr.tif",
  pour_pts = sbz_rast_path,
  output = output_name
)

watershed <- rast(output_name)
mapview(watershed) + mapview(single_sbz)

