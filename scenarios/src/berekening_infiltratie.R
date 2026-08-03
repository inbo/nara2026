studiegebied <- "kleine_nete"

#/////////////////////////////////////////////////////
# Scenario's klimaatportaal - sc3, sc7 en trend (sc2)
#/////////////////////////////////////////////////////

gebied <- st_read(str_c("C:/R/NARA2026/nara2026-git/scenarios/", studiegebied,
                        "/data/gebied.shp"), , quiet = TRUE)
gebied <- st_transform(gebied, crs = crs(ecosysteem))


sc7 <- rast(str_c("C:/R/NARA2026/nara2026-git/scenarios/", studiegebied,
                  "/data/klimportaal_sc7.tif"))
sc7 <- project(sc7, crs(ecosysteem))
sc7 <- mask(crop(sc7, vect(gebied)), vect(gebied))
sc7 <- resample(sc7, ecosysteem_2022, "near")

sc3 <- rast(str_c("C:/R/NARA2026/nara2026-git/scenarios/", studiegebied,
                  "/data/klimportaal_sc3.tif"))
sc3 <- project(sc3, crs(ecosysteem))
sc3 <- mask(crop(sc3, vect(gebied)), vect(gebied))
sc3 <- resample(sc3, ecosysteem_2022, "near")

sc2 <- rast(str_c("C:/R/NARA2026/nara2026-git/scenarios/", studiegebied,
                  "/data/klimportaal_sc2.tif"))
sc2 <- project(sc2, crs(ecosysteem))
sc2 <- mask(crop(sc2, vect(gebied)), vect(gebied))
sc2 <- resample(sc2, ecosysteem_2022, "near")

# infiltratie onbebouwd gebied = code 4 (Berging en infiltratie onbebouwdgebied)
# en 14 (Boomschaduw & berging en infiltratie onbebouwd gebied)
sc7_onbeb <- ifel(sc7 %in% c(4, 14), 1, NA)
sc2_onbeb <- ifel(sc2 %in% c(4, 14), 1, NA)
sc3_onbeb <- ifel(sc3 %in% c(4, 14), 1, NA)


sc7_onbeb_f <- freq(sc7_onbeb)[freq(sc7_onbeb)$value == 1, "count"]
sc3_onbeb_f <- freq(sc3_onbeb)[freq(sc3_onbeb)$value == 1, "count"]
sc2_onbeb_f <- freq(sc2_onbeb)[freq(sc2_onbeb)$value == 1, "count"]

buffer_sc7 <- (sc7_onbeb_f * 100 / 10000) * 75
buffer_sc3 <- (sc3_onbeb_f * 100 / 10000) * 75
buffer_sc2 <- (sc2_onbeb_f * 100 / 10000) * 75






#//////////////////////////////////////////////////////
# Op basis van output Investmodel seasonal water yield
#//////////////////////////////////////////////////////

# Data inladen en afstemmen
ecosysteem <- rast("C:/GIS/NARA2026/invest/data/ecosysteemkaart_zonderzee.tif")

ecosysteem_2022 <- rast(str_c("C:/R/NARA2026/nara2026-git/scenarios/", studiegebied, "/data/ecosysteem_2022.tif"))

# Infiltratie (local recharge)
r_oost_2050 <- rast("C:/GIS/NARA2026/invest/outputs/swy_oost_2050/L_bek_oost_2050_.tif")
r_west_2050 <- rast("C:/GIS/NARA2026/invest/outputs/swy_west_2050/L_bek_west_2050_.tif")

r_oost_2020 <- rast("C:/GIS/NARA2026/invest/outputs/swy_oost_2020/L_bek_oost_2020_.tif")
r_west_2020 <- rast("C:/GIS/NARA2026/invest/outputs/swy_west_2020/L_bek_west_2020_.tif")

r_mosaic_2050 <- mosaic(r_oost_2050, r_west_2050, fun = "mean")
r_mosaic_2020 <- mosaic(r_oost_2020, r_west_2020, fun = "mean")


r_mosaic_2050 <- r_mosaic_2050 |>
  project(ecosysteem, method = "near") |>
  resample(ecosysteem, method = "near")
  
r_mosaic_2020 <- r_mosaic_2020 |>
  project(ecosysteem, method = "near") |>
  resample(ecosysteem, method = "near")
  
invest_L_2050 <- mask(r_mosaic_2050, ecosysteem) # mm/jaar
invest_L_2020 <- mask(r_mosaic_2020, ecosysteem) # mm/jaar

# Inladen invest_L_20xx
invest_L_2020 <- rast("C:/R/NARA2026/nara2026-git/scenarios/data/l_vl_2022.tif")
invest_L_2050 <- rast("C:/R/NARA2026/nara2026-git/scenarios/data/l_vl_2050.tif")

L_gebied_2020 <- crop(invest_L_2020, ecosysteem_2022)
L_gebied_2020 <- mask(L_gebied_2020, ecosysteem_2022)

L_gebied_2050 <- crop(invest_L_2050, ecosysteem_2022)
L_gebied_2050 <- mask(L_gebied_2050, ecosysteem_2022)

writeRaster(invest_L_2020, str_c("C:/R/NARA2026/nara2026-git/scenarios/data/l_vl_2022.tif"))
writeRaster(invest_L_2050, str_c("C:/R/NARA2026/nara2026-git/scenarios/data/l_vl_2050.tif"))


# Afstroming (Quickflow)
r_oost_2020 <- rast("C:/GIS/NARA2026/invest/outputs/swy_oost_2020/QF_bek_oost_2020_.tif")
r_west_2020 <- rast("C:/GIS/NARA2026/invest/outputs/swy_west_2020/QF_bek_west_2020_.tif")

r_mosaic_2020 <- mosaic(r_oost_2020, r_west_2020, fun = "mean")

r_mosaic_2020 <- r_mosaic_2020 |>
  project(ecosysteem, method = "near") |>
  resample(ecosysteem, method = "near")

invest_QF_2020 <- mask(r_mosaic_2020, ecosysteem) # mm/jaar

QF_gebied_2020 <- crop(invest_QF_2020, ecosysteem_2022)
QF_gebied_2020 <- mask(QF_gebied_2020, ecosysteem_2022)

writeRaster(QF_gebied_2020, str_c("C:/R/NARA2026/nara2026-git/scenarios/", studiegebied,
                                  "/data/qf_2022.tif"))

writeRaster(invest_QF_2020, str_c("C:/R/NARA2026/nara2026-git/scenarios/data/qf_vl_2022.tif"))



# Infiltratie in m³
# Kaartresolutie = 10 x 10 m = 100 m² -> (infiltratie (mm) / 1000) * 100 m² 
invest_L_2050_m3 <- invest_L_2050/1000 * 100
invest_L_2020_m3 <- invest_L_2020/1000 * 100

invest_L_verschil <- invest_L_2020_m3 - invest_L_2050_m3 # m³/jaar


# Infiltratie studiegebied

landbouw <- ifel(ecosysteem_2022 %in% c(201, 202, 203, 204, 205, 206, 301), 1 , NA)

L_gebied_2020 <- crop(invest_L_2020_m3, landbouw)
L_gebied_2020 <- mask(L_gebied_2020, landbouw)

L_gebied_2050 <- crop(invest_L_2050_m3, landbouw)
L_gebied_2050 <- mask(L_gebied_2050, landbouw)


L_verschil_gebied <- crop(invest_L_verschil, landbouw)
L_verschil_gebied <- mask(L_verschil_gebied, landbouw)


# Infiltratieverlies in m³/jaar
verschil_m3 <- round(pull(global(L_verschil_gebied, fun = "sum", na.rm = TRUE)), 0) # m³/jaar

# Hoeveel ha buffervolume voor infiltratie is nodig?
# Het streefdoel voor onbebouwde ruimte is 75 m³/ha buffervolume (scenario's klimaatportaal)
# Met een diepte van 0.2 m voor een infiltratiepoel betekent dat 375 m²/ha
# opp. infiltratiemaatregel = infiltratiedoel (m³) / 75 m³ per ha

maatregel_ha <- verschil_m3/0.2/10000 # opp. infiltratiepoelen nodig om infiltratieverlies te compenseren


opp_gebied_ha <- round(100*pull(global(L_verschil_gebied, fun = "notNA"))/10000, 0)

maatregel_ha_75 <- opp_gebied_ha * 75/0.2/10000 # opp. infiltratiepoelen bij een max. buffervolume van 75 m³/ha

writeRaster(L_gebied_2020, "C:/GIS/NARA2026/3_Scenarios/infiltratie2020_kleinenete.tif", overwrite = TRUE)



