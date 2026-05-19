
# Packages
library(dplyr)
library(terra)
library(sf)
library(tools)
library(readxl)
library(igraph)
library(viridis)
library(mapview)
library(beepr)
library(leaflet) # Nodig voor colorFactor

# A. Data voorbereiden ####
# Rasterbestanden
path <- "C:/R/NARA2026/scenarios/rivierbeek/data"

# Definieer de gewenste bestanden
targets <- c("ecosysteem", "gewestplan_hfdc", "hag", "kernen", 
             "natdoel", "publiek_kad", "rbh", "sbz", "ven_ivon")

# "doorgrond", "droogtegevoelig", "ecosysteem", "erosie", "gebied_nit", "gewestplan_hfdc", "hag", "kernen", "natdoel", "peilgestuurd", "permnat", "publiek_kad", "publiek_owner", "rbh", "sbz", "tempnat", "ven_ivon", "watersysteem" -> Alle rasters -> controleer! 

# Haal de volledige lijst met rasters op en filter op de targets
tif_files <- list.files(path, pattern = "\\.tif$", full.names = TRUE)
names(tif_files) <- file_path_sans_ext(basename(tif_files))
tif_files_select <- tif_files[names(tif_files) %in% targets]

master_path <- tif_files[grepl("ecosysteem", names(tif_files))][1]
master_template <- rast(master_path)

raster_list <- lapply(tif_files_select, function(f) {
  r <- rast(f)
  if (crs(r) != crs(master_template)) {
    r <- project(r, master_template, method = "near")
  }
  if (ext(r) != ext(master_template) || any(res(r) != res(master_template))) {
    r <- resample(r, master_template, method = "near")
  }
  return(r)
})

list2env(raster_list, envir = .GlobalEnv)


# Vectorbestanden

vha <- st_read("C:/R/NARA2026/scenarios/rivierbeek/data/vha.shp") %>% st_make_valid()
vha <- st_transform(vha, crs(ecosysteem))
gebied <- st_read("C:/R/NARA2026/scenarios/rivierbeek/data/gebied.shp") %>% st_make_valid()
gebied <- st_transform(gebied, crs(ecosysteem))

# Agnaspolygonen bosuitbreiding
bbox <- st_bbox(gebied)
# polygonen van bestaand bos volgens AGNAS
# selectie_bos <- st_read("C:/R/NARA2026/scenarios/data/agnas_bestaandbos.shp", 
#                         wkt_filter = st_as_text(st_as_sfc(bbox))) %>% st_make_valid() %>%
#   st_transform(crs(ecosysteem))

# cirkels van bosuitbreidingslocaties volgens AGNAS
selectie_bosuitb <- st_read("C:/R/NARA2026/scenarios/data/agnas_bosuitbreiding.shp", 
                        wkt_filter = st_as_text(st_as_sfc(bbox))) %>% st_make_valid() %>%
  st_transform(crs(ecosysteem))

# agnas_bestaandbos <- selectie_bos[gebied, ] |> 
#   st_union() |> 
#   st_collection_extract("POLYGON") |> 
#   st_cast("POLYGON") |>
#   st_as_sf() |>
#   mutate(cluster_id = row_number())

agnas_bosuitbreiding <- selectie_bosuitb[st_intersects(selectie_bosuitb, gebied, sparse = FALSE), ]

# # bestaand bos thv bosuitbreidingslocaties
# agnas_boslocaties <- agnas_bestaandbos[agnas_bosuitbreiding, ] |> 
#   st_union()


# Bijknippen rasters met polygoon gebied (extent is gebied + 1.5 km)
rasters_geclipt <- lapply(targets, function(nm) {
  r <- get(nm)
  r_crop <- crop(r, gebied)
  r_mask <- mask(r_crop, gebied)
  return(r_mask)
})

names(rasters_geclipt) <- targets
list2env(rasters_geclipt, envir = .GlobalEnv)

# Tabellen
# 1. tabel om kaart publieke eigendommen (publiek_kad) om te zetten in kaart met
# cellen die vergroenbaar zijn (= niet bebouwd of moeilijk aanpasbare cellen zoals kerkhoven)
tbl_publiek <- read_excel("C:/R/NARA2026/scenarios/data/deel_3_reclass_publiekpercelen.xlsx")
# 2. tabel om de lu-kaart (ecosysteem) te reclassen voor geschiktheid bosuitbreiding,
# terrestrische natuurtypes (landnat), water (water), stedelijk gebied (stedelijk) of landbouw
tbl_lu <- read_excel("C:/R/NARA2026/scenarios/data/deel_3_reclass_lu.xlsx")
# 3. tabel om de natuurdoelenkaart te reclassen naar bos of gwates
tbl_natdoel <- read_excel("C:/R/NARA2026/scenarios/data/deel_3_reclass_natdoel.xlsx", sheet = "tabel")

## Kleuren voor de ecosysteemkaart ####
clr_data <- read.table("C:/R/NARA2026/scenarios/color_ecosysteemkaart_niv2.clr", skip = 0) 

named_colors <- rgb(clr_data$V2, clr_data$V3, clr_data$V4, maxColorValue = 255)
names(named_colors) <- as.character(clr_data$V1)

#///////////////////////
# B. Bosuitbreiding ####
#///////////////////////

# 1. Harde randvoorwaarden
# Regel 1: Alleen bebosbaar volgens tbl_lu
reclass_lu <- tbl_lu %>% select(code, bosuitbreiding) %>% as.matrix()
mask_lu <- classify(ecosysteem, reclass_lu)

# Regel 2: Niet op hag (waarde 1 = geschikt)
mask_hag <- ifel(is.na(hag), 1, 0)

# Regel 3: niet op open natuurdoelen (1 = geschikt)
# duid aan welke natuurdoelen geen bos zijn
lev <- levels(natdoel)[[1]]
lev2 <- merge(lev, tbl_natdoel, by.x = "HAB1", by.y = "hab1", all.x = TRUE)
lookup   <- lev2[,c("Value","bos")]
natdoel_01  <- classify(natdoel, lookup)

mask_natd <- ifel(is.na(natdoel_01) | natdoel_01 == 1, 1, 0) # open natuur niet bebossen

# bosdoelcellen die nu nog geen bos zijn (voor volgende stap - geschiktheidskaart)
bestaand_bos <- ifel(ecosysteem %in% c(103, 400, 401, 402, 403), 1, 100)

nat_bebos <- ifel(natdoel_01 == 1 & bestaand_bos == 100, 1, NA) # te bebossen ifv natuurdoelen

# Combineer harde maskers (0 = nooit bos, 1 = mogelijk bos)
total_mask <- mask_lu * mask_hag * mask_natd

# 2. Geschiktheidskaart (Scores)

# Regel 3: Voorkeur rbh '03', '04', '05'
score_rbh <- ifel(rbh %in% c("03", "04", "05"), 1, 0)

# Regel 4: Voorkeur publiek_natuur == 1
score_pub <- subst(publiek_kad, from = tbl_publiek$KAD_AARD, to = tbl_publiek$publiek_natuur)
score_pub <- as.numeric(ifel(is.na(score_pub), 0, score_pub))
score_pub <- ifel(score_pub == 124, 1, 0)

# Regel 5: Afstand tot kernen (3-30-300 regel: binnen 300m = 1, daarna lin afnemend tot 600m)
# afstand tot cellen kernen (kernen = woonkernen)
dist_kernen <- distance(kernen)
score_kernen <- ifel(dist_kernen <= 300, 1,
                  ifel(dist_kernen <= 600, (600 - dist_kernen) / 300, 0))

# Regel 6: Aansluitend bij bestaand bos AGNAS
# Zoek bestaand bos (codes 400, 401, 402, 403) -> 103 (hoog groen meenemen?)
bestaand_bos <- ifel(ecosysteem %in% c(400, 401, 402, 403), 1, NA) 
bos_patches <- patches(bestaand_bos, directions = 8)
patch_sizes <- freq(bos_patches)
grote_bos_ids <- patch_sizes$value[patch_sizes$count > 50] # alleen bosclusters > 0.5 ha
bos_gefilterd <- ifel(bos_patches %in% grote_bos_ids, bos_patches, NA)

# selecteer bosclusters die overlappen met agnas-locaties
intersect_ids <- extract(bos_gefilterd, vect(agnas_bosuitbreiding), ID = FALSE)
unique_ids <- unique(na.omit(intersect_ids[[1]]))
bos_agnas <- ifel(bos_gefilterd %in% unique_ids, 1, NA)

dist_bos <- distance(bos_agnas)
# score 1 tot 100m van bos en lineair afnemende score voor afstand 100m < d < 300m
score_bos <- ifel(dist_bos <= 100, 1,
                  ifel(dist_bos <= 300, (300 - dist_bos) / 200, 0))

# Geschiktheidskaart compileren

# Tel alle scores op (eventueel weging toepassen)
suitability <- 2*score_rbh + score_pub + score_kernen + 2*score_bos

# Pas het masker toe (niet bebosbare cellen worden 0)
suitability <- suitability * total_mask

# Normaliseer naar range 0-1
limits <- minmax(suitability)
suit_min <- limits[1, 1]
suit_max <- limits[2, 1]

if (suit_max > suit_min) {
  suit_normalized <- (suitability - suit_min) / (suit_max - suit_min)
} else {
  suit_normalized <- suitability # Geen variatie om te normaliseren
}


# 4. Optimalisatie - allocatie met clustering ---

# Parameters
target_cells  <- 18200      # bosuitbreidingsoppervlakte: Rivierbeek = 182 ha
min_cluster   <- 50        # 0.5 ha (minimale clustergrootte)
connectivity  <- 8         # 8 buren (4 kan ook)
compact_weight <- 0.01 # compactheidsfactor

# Input
values_vec <- values(suit_normalized)
allocated <- rep(FALSE, ncell(suit_normalized))
total_allocated <- 0

# seeds = lokale maxima selecteren als start plaatsing clusters (clusters groeien aan rond de lokale maxima)
max_neighbors <- focal(suit_normalized, w=matrix(1,3,3), fun=max, na.rm=TRUE, pad=TRUE)
# lokaal maxima: waarde > 0 en ≥ max van buren
local_max_raster <- (suit_normalized > 0) & (suit_normalized >= max_neighbors)

seeds <- which(values(local_max_raster) == 1)
seeds <- seeds[!allocated[seeds]] 
seeds <- seeds[order(values_vec[seeds], decreasing=TRUE)]

# Om rekentijd te beperken, selecteren we alleen de top x% van seeds (startpunten voor bosclusters)
top_n <- round(length(seeds) * 0.2) # top 20%
seeds_sorted <- seeds[order(values_vec[seeds], decreasing = TRUE)]
seeds_filtered <- seeds_sorted[1:top_n]
seeds <- seeds_filtered

n_seeds <- length(seeds)
pb <- txtProgressBar(min = 0, max = n_seeds, style = 3) # progress bar

# HULPFUNCTIE: compacte cluster groeien
grow_compact_cluster <- function(seed_cell, allocated, min_size) {
  cluster <- c(seed_cell)
  frontier <- c(seed_cell)
  while(length(frontier) > 0) {
    neigh <- unique(unlist(adjacent(suit_normalized, frontier, directions=connectivity)))
    neigh <- neigh[!allocated[neigh] & values_vec[neigh] > 0 & !(neigh %in% cluster)]
    if(length(neigh) == 0) break
    
    # cluster centrum
    coords <- rowColFromCell(suit_normalized, cluster)
    center <- colMeans(coords)
    neigh_coords <- rowColFromCell(suit_normalized, neigh)
    dist_center <- sqrt((neigh_coords[,1]-center[1])^2 + (neigh_coords[,2]-center[2])^2)
    # score = hoge geschiktheid + nabijheid clustercentrum
    score <- values_vec[neigh] - dist_center*compact_weight
    next_cell <- neigh[which.max(score)]
    cluster <- c(cluster, next_cell)
    frontier <- next_cell
    # Stop als cluster groot genoeg en totaal overschrijding dreigt
    if(length(cluster) >= min_size & (length(cluster) + sum(allocated)) >= target_cells) break
  }
  if(length(cluster) >= min_size) return(cluster) else return(NULL)
}

# Clusters plaatsen
for(i in 1:n_seeds) {
  seed <- seeds[i]
  # Update voortgangsbalk bij elke iteratie
  setTxtProgressBar(pb, i)
  
  if(total_allocated >= target_cells) break
  if(allocated[seed]) next
  cluster <- grow_compact_cluster(seed, allocated, min_cluster)
  if(!is.null(cluster)) {
    # Overschrijding target voorkomen
    remaining <- target_cells - total_allocated
    if(length(cluster) > remaining) {
      cluster <- cluster[1:remaining]
    }
    allocated[cluster] <- TRUE
    total_allocated <- total_allocated + length(cluster)
  }
}

close(pb)
beep(3) # grapje


# Bosuitbreidingsraster
bosuitbreiding <- suit_normalized
values(bosuitbreiding) <- as.integer(allocated)

mapview(bos_gefilterd, col.regions = "darkgreen", na.color = "transparent") + 
  mapview(ifel(bosuitbreiding == 0, NA, bosuitbreiding), na.color = "transparent") + mapview(agnas_bosuitbreiding)

#//////////////////////////////////
# C. Oeverzones ####
#//////////////////////////////////

# 1. Bestanden inladen
# raster (ecosysteem) al ingeladen in stap A.
paars <- st_read("C:/R/NARA2026/scenarios/rivierbeek/data/oever_paars.shp") %>% st_make_valid()
blauw_poly <- st_read("C:/R/NARA2026/scenarios/rivierbeek/data/oever_blauw_poly.shp") %>% st_make_valid()
blauw_lijn <- st_read("C:/R/NARA2026/scenarios/rivierbeek/data/oever_blauw_lijn.shp") %>% st_make_valid()
blauw_poly <- st_read("C:/R/NARA2026/scenarios/rivierbeek/data/oever_blauw_poly.shp") %>% st_make_valid()
gebnit <- st_read("C:/R/NARA2026/scenarios/rivierbeek/data/gebnit.shp") %>% st_make_valid()
ps_ven <- st_read("C:/R/NARA2026/scenarios/rivierbeek/data/ven.shp") %>% st_make_valid()
habrl <- st_read("C:/R/NARA2026/scenarios/rivierbeek/data/habrl.shp") %>% st_make_valid()
rbh <- st_read("C:/R/NARA2026/scenarios/rivierbeek/data/rbh.shp") %>% st_make_valid()
gebnit <- st_read("C:/R/NARA2026/scenarios/rivierbeek/data/gebnit.shp") %>% st_make_valid()

target_crs <- crs(ecosysteem)
paars <- st_transform(paars, target_crs)
blauw_lijn <- st_transform(blauw_lijn, target_crs)
blauw_poly <- st_transform(blauw_poly, target_crs)
gebnit <- st_transform(gebnit, target_crs)
ps_ven <- st_transform(ps_ven, target_crs)
habrl <- st_transform(habrl, target_crs)
rbh <- st_transform(rbh, target_crs)
gebnit <- st_transform(gebnit, target_crs)

# 2. Blauw_lijn omzetten naar polygoon (1m breedte = 0.5m buffer)
blauw_lijn_poly <- st_buffer(blauw_lijn, dist = 0.5)

# 3. Bestanden samenvoegen met een identificatieveld
paars$type <- "paars"
blauw_poly$type <- "blauw_poly"
blauw_lijn_poly$type <- "blauw_lijn"

# Selecteer enkel het type veld en voeg samen (binden)
rivieren <- rbind(
  paars[, "type"],
  blauw_poly[, "type"],
  blauw_lijn_poly[, "type"]
)

# polygonen wegschrijven
st_write(rivieren, "C:/R/NARA2026/scenarios/data/rivieren_buffer_output.gpkg", delete_dsn = TRUE)
rivieren <- st_read("C:/R/NARA2026/scenarios/data/rivieren_buffer_output.gpkg")

# 4. Omgevingsfactoren toevoegen via ruimtelijke joins
# We kijken voor elke rivierpolygoon in welke zones ze ligt
rivieren <- rivieren %>%
  st_join(ps_ven %>% 
            mutate(is_ven = TRUE) %>% 
            dplyr::select(is_ven)) %>%
  st_join(habrl %>% 
            mutate(is_sbz = TRUE) %>% 
            dplyr::select(is_sbz)) %>%
  st_join(gebnit %>% 
            dplyr::select(GTOMSCH)) %>%
  st_join(rbh %>% 
            dplyr::select(rbh)) %>%
  mutate(
    is_ven = ifelse(is.na(is_ven), FALSE, TRUE),
    is_sbz = ifelse(is.na(is_sbz), FALSE, TRUE),
    is_groen = rbh %in% c("03", "04", "05")
  )

# 5. Logica voor het veld 'oever' toepassen
rivieren <- rivieren %>%
  mutate(oever = case_when(
    # Regel 1: Paars (= niet vha)
    type == "paars" & is_ven ~ 19,
    type == "paars" & !is_ven ~ 14,
    
    # Regel 2.1 voor Blauw (lijn & poly) in VEN en meest kwetsbaar gebied (som = 10m)
    (type %in% c("blauw_poly", "blauw_lijn")) & is_ven & (is_sbz | GTOMSCH %in% c("Gebiedstype 2", "Gebiedstype 3") | is_groen) ~ 145,
    
    # Regel 2.2 voor Blauw (lijn & poly) in VEN en minder kwetsbaar gebied (som = 10m)
    (type %in% c("blauw_poly", "blauw_lijn")) & is_ven & (GTOMSCH %in% c("Gebiedstype 0", "Gebiedstype 1")) & !is_sbz & !is_groen ~ 127,
    
    # Regel 3.1: Blauw buiten VEN maar wel in SBZ, GTOMSCH 2/3 of groene bestemming
    (type %in% c("blauw_poly", "blauw_lijn")) & !is_ven & (is_sbz | GTOMSCH %in% c("Gebiedstype 2", "Gebiedstype 3") | is_groen) ~ 140,
    
    # Regel 3.2: Blauw buiten VEN in GTOMSCH 0/1 en GEEN SBZ/Groen
    (type %in% c("blauw_poly", "blauw_lijn")) & !is_ven & (GTOMSCH %in% c("Gebiedstype 0", "Gebiedstype 1")) & !is_sbz & !is_groen ~ 120,
    
    TRUE ~ 0 # Fallback waarde
  ))


# Buffer van 10m rond water
rivieren_buffer_10m <- st_buffer(rivieren, dist = 10)

# De hoogste 'oever' waarde gebruiken bij overlappende buffers
# Dit voorkomt dat rasterize willekeurig een waarde kiest bij overlap
rivieren_buffer_clean <- rivieren_buffer_10m %>%
  arrange(oever) %>% # Laagste waarden eerst
  group_by(geometry) %>% 
  summarize(oever = max(oever, na.rm = TRUE)) %>%
  st_make_valid()

# 6. Omzetten naar Raster
# Gebruik het 'ecosysteem' raster als template voor resolutie en extent
# 6.1 Alleen de rivieren verrasteren
rivieren_raster <- rasterize(rivieren, ecosysteem, field = "oever", fun = max)

# 6.2 De rivieren met hun buffer verrasteren
rivieren_buf_raster <- terra::rasterize(
  x = vect(rivieren_buffer_clean), # Omzetten naar terra-formaat
  y = ecosysteem,                  # Het template raster
  field = "oever",                 # De waarde die in de cellen komt
  fun = "max",                     # Bij overlap de maximale oeverwaarde
  background = NA                  # Cellen buiten de buffer worden NA (of kies 0)
)

# 7. Alleen de oevercellen uit het ecosysteemraster overhouden
# De watercellen uit het ecosysteemraster krijgen waarde NULL
water_codes <- c(105, 801, 802, 901, 902, 1001)

# Maak een 'masker' van de watercellen
is_water <- ecosysteem %in% water_codes

# Als een cel in 'is_water' TRUE is, zetten we de waarde in ons oever-raster op NA
oever_zone <- mask(rivieren_buf_raster, is_water, maskvalues = TRUE, updatevalue = NA)

# Uitleg codes oever_zone (https://www.vlm.be/nl/SiteCollectionDocuments/Mestbank/Algemeen/Afstandsregels_2026.pdf) + hoe interpreteren in ESD-analyses (% gras en reductie bemesting)
# 14 = 1m teeltvrij + 4m bemestingsvrij = 10% gras in cel en 50% minder bemesting
# 19 = 1m teeltvrij + 9m bemestingsvrij (binnen VEN) = 10% gras en 0 bemesting
# 120 = 1m teeltvrij + 2m spontane vegetatie of buffergewas = 30% gras en 30% minder bemesting
# 127 = 1m teeltvrij + 2m spontane veg. of buffergewas + 7m bemestingsvrij = 30% gras en 0 bemesting
# 140 = 1m teeltvrij + 4m spont. veg. of bufferg. = 50% gras en 50% minder bemesting
# 145 = 1m teeltvrij + 4m spont. veg. of bufferg. + 5m bemestingsvrij = 50% gras en 0 bemesting


## Resultaat plotten ####
ecosysteem_fact <- as.factor(ecosysteem) # maak een factor van de klassen

unieke_waardes <- unique(values(ecosysteem_fact))[,1] # haal de waardes in de kaart op
unieke_waardes <- as.character(sort(unieke_waardes[!is.na(unieke_waardes)]))

final_palette <- named_colors[as.character(unieke_waardes)] # gebruik alleen de kleuren van de klassen die voorkomen in de kaart

names(final_palette) <- unieke_waardes

# Maak een kleurenpalet-functie
pal_fun <- colorFactor(
  palette = as.character(final_palette), 
  domain = unieke_waardes,
  ordered = TRUE
)

mapview(ecosysteem_fact, col.regions = pal_fun(unieke_waardes), at = as.numeric(unieke_waardes), na.color = "transparent", legend = TRUE) + mapview(oever_zone, col.regions = "white", na.color = "transparent")

#//////////////////////////////////
# D. Landbouwgebied & KLE ####
#//////////////////////////////////

# Afbakening landbouwgebied = alle landbouwgebruikspercelen (2022)



