#//////////////////////////////////////////////////////////////////
# Welke bestanden worden gebruikt in de scripts?
#//////////////////////////////////////////////////////////////////


#//////////////////////////////////////////////////////////////////
# 1. Bestandsnamen in ArcGIS-scripts
#//////////////////////////////////////////////////////////////////

library(stringr)

# 1. Lees het R-script in als tekst (= bestandsnamen uit pythonversie van ArcGIS script)
script_tekst <- readLines("C:/R/NARA2026/bestandsnamen_scenarios.R", warn = FALSE)

# Als je de tekst al in een R-variabele hebt staan, sla stap 1 over.

# 2. Extraheer het deel na %outputfolder%\ of %outputfolder%\\
matches <- str_match_all(script_tekst, '%outputfolder%\\\\+([^"]+)')

# 3. Haal de geselecteerde bestandsnamen op en maak een schone lijst/vector
bestanden_arcgis <- unlist(lapply(matches, function(m) m[, 2]))
bestanden_arcgis <- unique(na.omit(bestanden_arcgis))

print(bestanden_arcgis)


#//////////////////////////////////////////////////////////////////
# 2. Bestandsnamen in Rmarkdown-scripts
#//////////////////////////////////////////////////////////////////

folder_path <- "C:/R/NARA2026/nara2026-git/scenarios/src"

# 1. Lijst van Rmd-bestanden
rmd_bestanden <- list.files(
  path = folder_path,
  pattern = "^([0-9]{2}[a-z]*|x[0-9]+)_.*\\.Rmd$",
  full.names = TRUE,
  ignore.case = TRUE
)

# 2. Functie die een dataframe/tabel teruggeeft per Rmd-bestand
extraheer_targets_df <- function(bestand_pad) {
  script_naam <- basename(bestand_pad) # Haalt alleen de bestandsnaam op (bijv. 01_data.Rmd)
  tekst <- paste(readLines(bestand_pad, warn = FALSE), collapse = "\n")
  
  target_blokken <- str_extract_all(tekst, "(?s)targets\\s*(?:<-|=)\\s*c\\((.*?)\\)")[[1]]
  
  if (length(target_blokken) == 0) {
    return(tibble(target = character(0), rmd_script = character(0)))
  }
  
  bestanden <- unlist(str_extract_all(target_blokken, "[\"']([^\"']+)[\"']"))
  bestanden_schoon <- str_remove_all(bestanden, "[\"']")
  
  tibble(
    target = bestanden_schoon,
    rmd_script = script_naam
  )
}

# 3. Maak de overzichtstabel (1 rij per target + script combinatie)
tabel_targets <- rmd_bestanden %>%
  map(extraheer_targets_df) %>%
  bind_rows() %>%
  distinct()

# 4. (Optioneel) Geaggregeerde tabel: 1 rij per target met alle scripts op een rij
tabel_samengevat <- tabel_targets %>%
  group_by(target) %>%
  summarise(gebruikt_in = paste(rmd_script, collapse = ", ")) %>%
  ungroup()

# De unieke lijst ophalen kan nu eenvoudig uit de tabel:
bestanden_r <- unique(tabel_targets$target)

print(bestanden_r)


#//////////////////////////////////////////////////////////////////
# 3. Bestandsnamen vergelijken
#//////////////////////////////////////////////////////////////////

# 1. In 'bestandsnamen', maar NIET in 'unieke_targets'
alleen_in_arcgis <- setdiff(bestanden_arcgis, bestanden_r)

# 2. In 'unieke_targets', maar NIET in 'bestandsnamen'
alleen_in_r <- setdiff(bestanden_r, bestanden_arcgis)

# Overzicht bekijken als lijst
overzicht <- list(
  alleen_in_arcgis = alleen_in_arcgis,
  alleen_in_r = alleen_in_r
)

print(overzicht)


#//////////////////////////////////////////////////////////////////
# Worden de bestanden ook gebruikt in de scripts?
#//////////////////////////////////////////////////////////////////

library(stringr)
library(purrr)
library(dplyr)
library(tools)

folder_path <- "C:/R/NARA2026/nara2026-git/scenarios/src"

# Geef hier de bekende studiegebieden op
studiegebieden_lijst <- c("kleine_nete", "herk_mombeek", "rivierbeek", "ijzer", "dijle", "dommel")

rmd_bestanden <- list.files(
  path = folder_path,
  pattern = "^([0-9]{2}[a-z]*|x[0-9]+)_.*\\.Rmd$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Hulpfunctie die zoekt op schijf en het geüniformeerde pad teruggeeft
bepaal_folder_locatie <- function(target, studiegebieden) {
  base_pad <- "C:/R/NARA2026/nara2026-git/scenarios"
  
  # 1. Controleer in <studiegebied>/data
  for (sg in studiegebieden) {
    if (file.exists(file.path(base_pad, sg, "data", target))) {
      return(file.path(base_pad, "studiegebied", "data"))
    }
  }
  
  # 2. Controleer in de centrale /data map
  if (file.exists(file.path(base_pad, "data", target))) {
    return(file.path(base_pad, "data"))
  }
  
  # 3. Controleer in <studiegebied>/output
  for (sg in studiegebieden) {
    if (file.exists(file.path(base_pad, sg, "output", target))) {
      return(file.path(base_pad, "studiegebied", "output"))
    }
  }
  
  return("Niet gevonden op schijf")
}

controleer_ongebruikte_targets <- function(bestand_pad, sg_lijst) {
  script_naam <- basename(bestand_pad)
  tekst <- paste(readLines(bestand_pad, warn = FALSE), collapse = "\n")
  
  # Als het Rmd-script zelf een studiegebied definieert, voeg die toe aan de zoeklijst
  sg_match <- str_match(tekst, "studiegebied\\s*(?:<-|=)\\s*[\"']([^\"']+)[\"']")
  actieve_sg_lijst <- if (!is.na(sg_match[1, 2])) unique(c(sg_match[1, 2], sg_lijst)) else sg_lijst
  
  # Haal R-code chunks op
  chunk_matches <- str_match_all(tekst, "(?s)```\\{r[^\n]*\n(.*?)```")[[1]]
  if (length(chunk_matches) == 0 || nrow(chunk_matches) == 0) return(NULL)
  
  r_chunks_inhoud <- chunk_matches[, 2]
  
  # Strip commentaar (#)
  alle_regels <- unlist(str_split(r_chunks_inhoud, "\n"))
  actieve_code_regels <- str_remove(alle_regels, "#.*$")
  actieve_code <- paste(actieve_code_regels, collapse = "\n")
  
  # Vind targets-blok
  target_blok <- str_extract(actieve_code, "(?s)targets\\s*(?:<-|=)\\s*c\\((.*?)\\)")
  if (is.na(target_blok)) return(NULL)
  
  # Haal bestandsnamen op
  bestanden <- unlist(str_extract_all(target_blok, "[\"']([^\"']+)[\"']"))
  bestanden_schoon <- str_remove_all(bestanden, "[\"']")
  if (length(bestanden_schoon) == 0) return(NULL)
  
  # Verwijder de definitie van 'targets'
  code_zonder_targets <- str_remove(actieve_code, fixed(target_blok))
  
  # Controleer per bestand het gebruik en de folderlocatie
  map_df(bestanden_schoon, function(target) {
    obj_naam <- file_path_sans_ext(target)
    pattern <- paste0("\\b", obj_naam, "\\b")
    
    is_gebruikt <- str_detect(code_zonder_targets, pattern)
    locatie <- bepaal_folder_locatie(target, actieve_sg_lijst)
    
    tibble(
      rmd_script = script_naam,
      target_bestand = target,
      object_naam = obj_naam,
      gebruikt = is_gebruikt,
      folder_locatie = locatie
    )
  })
}

# Uitvoeren over alle Rmd-bestanden
overzicht_gebruik <- rmd_bestanden %>%
  map(~controleer_ongebruikte_targets(.x, sg_lijst = studiegebieden_lijst)) %>%
  compact() %>%
  bind_rows()


ongebruikte_bestanden <- overzicht_gebruik %>%
  group_by(target_bestand, folder_locatie) %>%
  summarise(
    geladen_in_scripts = paste(unique(rmd_script), collapse = ", "),
    ooit_gebruikt = any(gebruikt),
    .groups = "drop"
  ) %>%
  filter(!ooit_gebruikt) %>%
  select(-ooit_gebruikt)



#///////////////////////////////////////////////////////////////////
# Welke data uit de folder .../scenarios/data worden niet gebruikt?
#///////////////////////////////////////////////////////////////////

data_folder <- "C:/R/NARA2026/nara2026-git/scenarios/data"

# 1. Lees alle fysieke .shp, .tif en .xlsx bestanden in de folder
fysische_bestanden <- list.files(
  path = data_folder,
  pattern = "\\.(shp|tif|xlsx)$",
  ignore.case = TRUE,
  full.names = FALSE
)

# 2. Filter uit 'overzicht_gebruik' de bestanden die effectief gebruikt worden (gebruikt == TRUE)
actief_gebruikte_targets <- overzicht_gebruik %>%
  filter(gebruikt == TRUE) %>%
  group_by(target_bestand) %>%
  summarise(
    gebruikt_in_script = paste(unique(rmd_script), collapse = ", "),
    .groups = "drop"
  )

# 3. Koppel de fysieke bestanden aan de gebruikte targets
analyse_data_folder <- tibble(target_bestand = fysische_bestanden) %>%
  left_join(actief_gebruikte_targets, by = "target_bestand") %>%
  mutate(
    effectief_gebruikt = !is.na(gebruikt_in_script)
  ) %>%
  select(target_bestand, effectief_gebruikt, gebruikt_in_script)

# Bekijk het volledige overzicht
print(analyse_data_folder)

# Optioneel: Bekijk alleen de bestanden in de folder die NIET gebruikt worden
ongebruikt_in_folder <- analyse_data_folder %>%
  filter(!effectief_gebruikt)

print(ongebruikt_in_folder)
