# Functie om een deel (vierkant) van het gebied te plotten
# De coördinaten (x_midden en y_midden) zijn het middelpunt van het vierkant
# Extract coördinaten in QGIS via rechtermuisknop -> copy coordinate (EPSG:31370)

180616.9,220406.9
183213,214903
182638,219010 # vrijstromendheid
188649.9,216777.6 # Tielen (bever)
189899.9,209902.6 # de zegge (bever)

plot_part <- function(x, col, add = FALSE) {
  x_midden <- 189899.9
  y_midden <- 209902.6
  # We nemen 500m naar links/rechts en naar boven/onder vanaf het midden
  extent <- ext(
    x_midden - 1500, x_midden + 1500, 
    y_midden - 1500, y_midden + 1500
  )
  # Knip de rasterkaart bij met deze extent
  plot_crop <- crop(x, extent)
  plot(plot_crop, col = col, add = add)
}

# plot_part(kaart)
