library(tidyverse)
library(sf)
library(showtext)
library(ggtext)
library(nrBrand)
library(camcorder)
library(glue)


# Data --------------------------------------------------------------------

uk_data <- read_sf("2025/data/Local_Authority_Districts_December_2021_UK_BUC_2022_6611447234830791685/LAD_DEC_2021_UK_BUC.shp")

scot_map <- uk_data |>
  filter(str_starts(LAD21CD, "S")) |>
  select(LAD21CD, LAD21NM, geometry)

# get bounding box of each geometry
bboxes <- scot_map |>
  mutate(bbox = map(geometry, ~ st_as_sfc(st_bbox(.x)))) %>%
  st_set_geometry(do.call(c, .$bbox)) |>
  select(-bbox)

north <- bboxes |>
  mutate(x = map_dbl(geometry, ~ st_bbox(.x)[["ymax"]])) |>
  slice_max(x, n = 1)
south <- bboxes |>
  mutate(x = map_dbl(geometry, ~ st_bbox(.x)[["ymin"]])) |>
  slice_min(x, n = 1)
east <- bboxes |>
  mutate(x = map_dbl(geometry, ~ st_bbox(.x)[["xmax"]])) |>
  slice_max(x, n = 1)
west <- bboxes |>
  mutate(x = map_dbl(geometry, ~ st_bbox(.x)[["xmin"]])) |>
  slice_min(x, n = 1)


# Start recording ---------------------------------------------------------

gg_record(
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)


# Colours -----------------------------------------------------------------

bg_col <- "grey90"
text_col <- "black"


# Fonts -------------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "Scottish Councils"
st <- " Scotland is divided into 32 areas designated as **council areas** (*comhairlean*), which are all governed by single-tier authorities."
cap <- paste0(
  "**Data**: geoportal.statistics.gov.uk<br>**Graphic**: ", social
)

# annotation text
annot1 <- glue("**Furthest North**<br>{north$LAD21NM}")
annot2 <- glue("**Furthest East**<br>{east$LAD21NM}")
annot3 <- glue("**Furthest South**<br>{south$LAD21NM}")
annot4 <- glue("**Furthest West**<br>{west$LAD21NM}")
text_data <- tibble(
  x = c(
    st_bbox(north)[["xmin"]], st_bbox(east)[["xmax"]],
    st_bbox(south)[["xmin"]], st_bbox(west)[["xmin"]]
  ),
  y = c(
    st_bbox(north)[["ymax"]], st_bbox(east)[["ymax"]],
    st_bbox(south)[["ymin"]], st_bbox(west)[["ymax"]]
  ),
  label = c(annot1, annot2, annot3, annot4),
  hjust = c(0, 0, 0, 1),
  vjust = c(0, 1, 1, 1)
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = bboxes,
    fill = alpha(text_col, 0.1),
    colour = text_col
  ) +
  # annotations (furthest NSEW)
  geom_textbox(
    data = text_data,
    mapping = aes(
      x = x, y = y, label = label,
      hjust = hjust, halign = hjust,
      vjust = vjust, valign = vjust
    ),
    colour = text_col,
    family = body_font,
    fill = "transparent",
    box.colour = "transparent",
    size = 3,
    box.padding = unit(c(3, 3, 3, 3), "pt"),
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_sf(clip = "off") +
  theme_void(base_size = 11, base_family = body_font) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5, l = -120),
      family = title_font,
      face = "bold",
      size = rel(1.6),
      vjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -20, l = -120),
      family = body_font,
      width = 0.6,
      lineheight = 1,
      vjust = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 20, l = -120),
      family = body_font
    ),
    plot.margin = margin(5, -60, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/day_27.png",
  bg = bg_col, height = 6, width = 6, units = "in"
)
