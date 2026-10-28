library(sf)
library(tidyverse)
library(terra)

data2050 <- "kleine_nete_2050" # "herk_mombeek_2050"
data2020 <- "kleine_nete" # "herk_mombeek"

# Data 2020
ref_2020 <- str_c("C:/GIS/NARA2026/invest/R/outputs/", data2020, "/referentie")
sc1_2020 <- str_c("C:/GIS/NARA2026/invest/R/outputs/", data2020, "/sc1_ont")
sc2_2020 <- str_c("C:/GIS/NARA2026/invest/R/outputs/", data2020, "/sc2_ont")
sc3_2020 <- str_c("C:/GIS/NARA2026/invest/R/outputs/", data2020, "/sc3_gras_ont")
sc3_75_2020 <- str_c("C:/GIS/NARA2026/invest/R/outputs/", data2020, "/sc3_75_ont")

# Data 2050
ref_2050 <- str_c("C:/GIS/NARA2026/invest/R/outputs/", data2050, "/referentie")
sc1_2050 <- str_c("C:/GIS/NARA2026/invest/R/outputs/", data2050, "/sc1_ont")
sc2_2050 <- str_c("C:/GIS/NARA2026/invest/R/outputs/", data2050, "/sc2_ont")
sc3_2050 <- str_c("C:/GIS/NARA2026/invest/R/outputs/", data2050, "/sc3_gras_ont")
sc3_75_2050 <- str_c("C:/GIS/NARA2026/invest/R/outputs/", data2050, "/sc3_75_ont")


#/////////////////////////////
# Infiltratie
#/////////////////////////////

inf_ref_2020 <- rast(file.path(ref_2020, "L.tif"))
inf_sc1_2020 <- rast(file.path(sc1_2020, "L_sc1_ont.tif"))
inf_sc2_2020 <- rast(file.path(sc2_2020, "L_sc2_ont.tif"))
inf_sc3_2020 <- rast(file.path(sc3_2020, "L_sc3_gras_ont.tif"))

inf_ref_2050 <- rast(file.path(ref_2050, "L.tif"))
inf_sc1_2050 <- rast(file.path(sc1_2050, "L_sc1_ont.tif"))
inf_sc2_2050 <- rast(file.path(sc2_2050, "L_sc2_ont.tif"))
inf_sc3_2050 <- rast(file.path(sc3_2050, "L_sc3_gras_ont.tif"))

inf_ref_2020 <- pull(global(inf_ref_2020, "sum", na.rm = TRUE))
inf_sc1_2020 <- pull(global(inf_sc1_2020, "sum", na.rm = TRUE))
inf_sc2_2020 <- pull(global(inf_sc2_2020, "sum", na.rm = TRUE))
inf_sc3_2020 <- pull(global(inf_sc3_2020, "sum", na.rm = TRUE))

inf_ref_2050 <- pull(global(inf_ref_2050, "sum", na.rm = TRUE))
inf_sc1_2050 <- pull(global(inf_sc1_2050, "sum", na.rm = TRUE))
inf_sc2_2050 <- pull(global(inf_sc2_2050, "sum", na.rm = TRUE))
inf_sc3_2050 <- pull(global(inf_sc3_2050, "sum", na.rm = TRUE))

# Verschil zonder landgebruiksveranderingen
100 * (inf_ref_2050 - inf_ref_2020)/inf_ref_2020

# Verschil met landgebruiksveranderingen
100 * (inf_sc1_2050 - inf_ref_2020)/inf_ref_2020 # sc1
100 * (inf_sc2_2050 - inf_ref_2020)/inf_ref_2020 # sc2
100 * (inf_sc3_2050 - inf_ref_2020)/inf_ref_2020 # sc3


#/////////////////////////////
# Afstroming
#/////////////////////////////

qf_ref_2020 <- rast(file.path(ref_2020, "QF.tif"))
qf_sc1_2020 <- rast(file.path(sc1_2020, "QF_sc1_ont.tif"))
qf_sc2_2020 <- rast(file.path(sc2_2020, "QF_sc2_ont.tif"))
qf_sc3_2020 <- rast(file.path(sc3_2020, "QF_sc3_gras_ont.tif"))
qf_sc3_75_2020 <- rast(file.path(sc3_75_2020, "QF_sc3_75_ont.tif"))

qf_ref_2050 <- rast(file.path(ref_2050, "QF.tif"))
qf_sc1_2050 <- rast(file.path(sc1_2050, "QF_sc1_ont.tif"))
qf_sc2_2050 <- rast(file.path(sc2_2050, "QF_sc2_ont.tif"))
qf_sc3_75_2050 <- rast(file.path(sc3_2050, "QF_sc3_75_ont.tif"))
qf_sc3_75_2050 <- rast(file.path(sc3_75_2050, "QF_sc3_75_ont.tif"))

qf_ref_2020 <- pull(global(qf_ref_2020, "sum", na.rm = TRUE))
qf_sc1_2020 <- pull(global(qf_sc1_2020, "sum", na.rm = TRUE))
qf_sc2_2020 <- pull(global(qf_sc2_2020, "sum", na.rm = TRUE))
qf_sc3_2020 <- pull(global(qf_sc3_2020, "sum", na.rm = TRUE))
qf_sc3_75_2020 <- pull(global(qf_sc3_75_2020, "sum", na.rm = TRUE))

qf_ref_2050 <- pull(global(qf_ref_2050, "sum", na.rm = TRUE))
qf_sc1_2050 <- pull(global(qf_sc1_2050, "sum", na.rm = TRUE))
qf_sc2_2050 <- pull(global(qf_sc2_2050, "sum", na.rm = TRUE))
qf_sc3_2050 <- pull(global(qf_sc3_2050, "sum", na.rm = TRUE))
qf_sc3_75_2050 <- pull(global(qf_sc3_75_2050, "sum", na.rm = TRUE))

# Verschil zonder landgebruiksveranderingen
100 * (qf_ref_2050 - qf_ref_2020)/qf_ref_2020

# Verschil met landgebruiksveranderingen
100 * (qf_sc1_2050 - qf_ref_2020)/qf_ref_2020 # sc1
100 * (qf_sc2_2050 - qf_ref_2020)/qf_ref_2020 # sc2
100 * (qf_sc3_2050 - qf_ref_2020)/qf_ref_2020 # sc3
100 * (qf_sc3_75_2050 - qf_ref_2020)/qf_ref_2020 # sc3



# Infiltratie 2020
inf_ref_2020 <- 1206708674

# Infiltratie 2050 (na correctie voor infiltratiemaatregelen)
inf_ref_2050 <- 894634890
inf_sc1_2050 <- 923662645
inf_sc2_2050 <- 918527779
inf_sc3_2050 <- 979214999
inf_sc3_75_2050 <- 916707147

# Verschil zonder landgebruiksveranderingen
100 * (inf_ref_2050 - inf_ref_2020)/inf_ref_2020

# Verschil met landgebruiksveranderingen
100 * (inf_sc1_2050 - inf_ref_2020)/inf_ref_2020 # sc1
100 * (inf_sc2_2050 - inf_ref_2020)/inf_ref_2020 # sc2
100 * (inf_sc3_2050 - inf_ref_2020)/inf_ref_2020 # sc3
100 * (inf_sc3_75_2050 - inf_ref_2020)/inf_ref_2020 # sc3 75%



# Afstroming 2020
qf_ref_2020 <- 1376830189

# Afstroming 2050 (na correctie voor infiltratiemaatregelen)
qf_ref_2050 <- 1529300317 
qf_sc1_2050 <- 1474509223
qf_sc2_2050 <- 1481489192
qf_sc3_2050 <- 1431440323
qf_sc3_75_2050 <- 1466047176

# Verschil zonder landgebruiksveranderingen
100 * (qf_ref_2050 - qf_ref_2020)/qf_ref_2020

# Verschil met landgebruiksveranderingen
100 * (qf_sc1_2050 - qf_ref_2020)/qf_ref_2020 # sc1
100 * (qf_sc2_2050 - qf_ref_2020)/qf_ref_2020 # sc2
100 * (qf_sc3_2050 - qf_ref_2020)/qf_ref_2020 # sc3
100 * (qf_sc3_75_2050 - qf_ref_2020)/qf_ref_2020 # sc3
