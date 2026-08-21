
studiegebied <- "kleine_nete"


#//////////////////////////////////////////////////////
# Op basis van output Investmodel seasonal water yield
#//////////////////////////////////////////////////////

# Data inladen en afstemmen
ecosysteem <- rast("C:/GIS/NARA2026/invest/data/ecosysteemkaart_zonderzee.tif")

ecosysteem_2022 <- rast(str_c("C:/R/NARA2026/nara2026-git/scenarios/", studiegebied, "/data/ecosysteem_2022.tif"))



#////////////////////////////////////
# Infiltratie (local recharge)
#////////////////////////////////////

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



#////////////////////////////////////
# Afstroming (Quickflow)
#////////////////////////////////////

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



#////////////////////////////////////
# Evapotranspiratie (AET)
#////////////////////////////////////

r_oost_2020 <- rast("C:/GIS/NARA2026/invest/outputs/swy_oost_2020/intermediate_outputs/aet_bek_oost_2020_.tif")
r_west_2020 <- rast("C:/GIS/NARA2026/invest/outputs/swy_west_2020/intermediate_outputs/aet_bek_west_2020_.tif")

r_mosaic_2020 <- mosaic(r_oost_2020, r_west_2020, fun = "mean")

r_mosaic_2020 <- r_mosaic_2020 |>
  project(ecosysteem, method = "near") |>
  resample(ecosysteem, method = "near")

invest_AET_2020 <- mask(r_mosaic_2020, ecosysteem) # mm/jaar

AET_gebied_2020 <- crop(invest_AET_2020, ecosysteem_2022)
AET_gebied_2020 <- mask(AET_gebied_2020, ecosysteem_2022)

writeRaster(AET_gebied_2020, str_c("C:/R/NARA2026/nara2026-git/scenarios/", studiegebied,
                                  "/data/aet_2022.tif"))

writeRaster(invest_AET_2020, str_c("C:/R/NARA2026/nara2026-git/scenarios/data/aet_vl_2022.tif"))



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



