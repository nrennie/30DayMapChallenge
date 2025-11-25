library(gridmappr)
library(tidyverse)
library(sf)
library(showtext)
library(patchwork)


# Fonts -------------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Data --------------------------------------------------------------------

uk_data <- read_sf("2025/data/Local_Authority_Districts_December_2021_UK_BUC_2022_6611447234830791685/LAD_DEC_2021_UK_BUC.shp")
wales_data <- uk_data |>
  filter(str_starts(LAD21CD, "W")) |>
  select(LAD21NM, geometry)
coords <- wales_data |>
  st_centroid() |>
  st_coordinates()
wales_data$x <- coords[, 1]
wales_data$y <- coords[, 2]
pts <- wales_data |>
  st_drop_geometry() |>
  select(area_name = LAD21NM, x, y)


solution <- points_to_grid(pts, n_row = 5, n_col = 6, compactness = 0.5,
                           spacers = list(c(5, 3), c(4, 3)))
grid <- make_grid(wales_data, n_row = 5, n_col = 6)
plot_data <- left_join(grid, solution, by = c("col", "row")) |> 
  filter(!is.na(area_name))


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = "grey97",
  icon_colour = "#006625",
  font_colour = "#006625",
  font_family = body_font,
  mastodon = NA
)
title <- "A gridmap allocation for local authorities in Wales"
st <- "Gridmap allocations that accurately represent the underlying geography can be difficult to compute, especially when variability between area sizes is large."
cap <- paste0(
  "**Data**: Open Geography Portal from the Office for National Statistics. Processed with {gridmappr}.<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

p1 <- ggplot() +
  geom_sf(
    data = wales_data,
    fill = "#C2FFD8",
    colour = "#00F55A"
  ) +
  geom_point(
    data = wales_data,
    mapping = aes(x = x, y = y),
    colour = "#006625"
  ) +
  geom_text(
    data = wales_data,
    mapping = aes(x = x, y = y + 3000, label = str_wrap(LAD21NM, 10)),
    colour = "#006625",
    family = body_font,
    size = 1.7,
    vjust = 0,
    lineheight = 0.8
  ) +
  theme_void()


p2 <- ggplot() +
  geom_sf(
    data = plot_data,
    fill = "#C2FFD8",
    colour = "#00F55A"
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(x = x, y = y),
    colour = "#006625"
  ) +
  geom_text(
    data = plot_data,
    mapping = aes(x = x, y = y + 3000, label = str_wrap(area_name, 10)),
    colour = "#006625",
    family = body_font,
    size = 1.7,
    vjust = 0,
    lineheight = 0.8
  ) +
  theme_void()

p1 + p2 +
  plot_annotation(
    title = title,
    subtitle = st,
    caption = cap,
    theme = theme(
      plot.background = element_rect(fill = "grey97", colour = "grey97"),
      plot.title = element_textbox_simple(
        colour = "#006625",
        hjust = 0,
        halign = 0,
        margin = margin(b = 0, t = 5),
        family = title_font,
        face = "bold",
        size = rel(1.4),
        vjust = 0
      ),
      plot.subtitle = element_textbox_simple(
        colour = "#006625",
        hjust = 0,
        halign = 0,
        margin = margin(b = 10, t = 5),
        family = body_font,
        maxwidth = 1,
        lineheight = 1,
        vjust = 1
      ),
      plot.caption = element_textbox_simple(
        colour = "#006625",
        hjust = 0,
        halign = 0,
        margin = margin(b = 0, t = 10),
        family = body_font
      )
    )
  )



# Save --------------------------------------------------------------------

gg_stop_recording()
ggsave(
  filename = "2025/maps/19_projections.png",
  bg = "grey97", height = 5.5, width = 7, units = "in"
)
