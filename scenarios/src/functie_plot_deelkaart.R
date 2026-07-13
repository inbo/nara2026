# Functie om een deel (vierkant) van het gebied te plotten
# De coördinaten (x_midden en y_midden) zijn het middelpunt van het vierkant
# Extract coördinaten in QGIS via rechtermuisknop -> copy coordinate (EPSG:31370)

plot_part <- function(x) {
  x_midden <- 180274.5
  y_midden <- 215902.5
  # We nemen 500m naar links/rechts en naar boven/onder vanaf het midden
  extent <- ext(
    x_midden - 500, x_midden + 500, 
    y_midden - 500, y_midden + 500
  )
  # Knip de rasterkaart bij met deze extent
  plot_crop <- crop(x, extent)
  plot(plot_crop)
}

# plot_part(kaart)
