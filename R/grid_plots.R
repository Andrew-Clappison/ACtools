library(ggplot2)
library(magrittr)
library(tidyverse)

data <- as.data.frame(matrix(runif(5*5), nrow = 5, ncol = 5)) %>%
  mutate(row = rownames(.)) %>%
  tidyr::gather(key = "col", value = "cor", -row)

ggplot2::ggplot(data= data) +
  geom_tile(mapping = aes(x = row, y = col, fill = cor)) +
  geom_tile_text(mapping = aes(x = row, y = col, value = cor))

GeomTileText <- ggproto("GeomTileText",
                        ggplot2::Geom,
                        required_aes = c("x", "y", "value"),
                        default_aes = c(),
                        draw_key = ggplot2::draw_key_text,
                        draw_panel = function(data, panel_params, coord) {
                          coords <- coord$transform(data, panel_params)
                          grid::textGrob(
                            x = coords$x,
                            y = coords$y,
                            label = round(coords$value, digits = 3)
                          )
                        })

geom_tile_text <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTileText, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot2::ggplot(data= data) +
  geom_tile(mapping = aes(x = row, y = col, fill = cor)) +
  ggplot2::layer(geom = GeomTileText,
                 mapping = aes(x = row, y = col, value = cor),
                 stat = "identity",
                 position = "identity")
