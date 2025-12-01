library(gridmappr) # devtools::install_github("rogerbeecham/gridmappr")
library(tidyverse)
library(sf)
library(rnaturalearth)
library(showtext)
library(ggtext)
library(nrBrand)
library(camcorder)
library(readxl)
library(rcartocolor)
library(glue)


# Data --------------------------------------------------------------------

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationandhouseholdestimatesenglandandwales/census2021unroundeddata
age_data <- read_xlsx("2025/data/datadownload.xlsx", skip = 9)
plot_age_data <- age_data |>
  select(
    Code = `Area code`,
    Name = `Area name`,
    Age = `Median age`
  )

uk_data <- read_sf("2025/data/Local_Authority_Districts_December_2021_UK_BUC_2022_6611447234830791685/LAD_DEC_2021_UK_BUC.shp")

wales_map <- uk_data |>
  filter(str_starts(LAD21CD, "W")) |>
  select(LAD21CD, geometry)

plot_data <- left_join(ew_map, plot_age_data, by = c("LAD21CD" = "Code"))

coords <- plot_data |>
  st_centroid() |>
  st_coordinates()
plot_data$x <- coords[, 1]
plot_data$y <- coords[, 2]

pts <- plot_data |>
  st_drop_geometry() |>
  select(area_name = LAD21CD, x, y)

solution <- points_to_grid(pts, n_row = 6, n_col = 6, compactness = 1)

final_data <- solution |>
  as_tibble() |>
  left_join(select(st_drop_geometry(plot_data), LAD21CD, Age, Name), by = c("area_name" = "LAD21CD"))


# Start recording ---------------------------------------------------------

gg_record(
  device = "png",
  width = 4,
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
title <- "Cardiff is the youngest local authority in Wales"
st <- "The median age of people in Cardiff in the 2021 Census was 34, lower compared to the overall median age in Wales of 42."
cap <- paste0(
  "**Data**: Population and household estimates, England and Wales: Census 2021, unrounded data from Office for National Statistics<br>**Graphic**: ", social
)

min_label <- final_data |>
  slice_min(Age) |>
  mutate(label = glue("{Name} has the lowest median age at {Age}")) |>
  pull(label)
max_label <- final_data |>
  slice_max(Age) |>
  mutate(label = glue("{Name} has the highest median age at {Age}")) |>
  pull(label)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_raster(
    data = final_data,
    mapping = aes(x = 1, y = 1, fill = Age)
  ) +
  geom_text(
    data = final_data,
    mapping = aes(x = 1, y = 1.2, label = str_wrap(Name, 8)),
    size = 2,
    lineheight = 0.8
  ) +
  geom_text(
    data = final_data,
    mapping = aes(x = 1, y = 0.8, label = Age),
    fontface = "bold"
  ) +
  facet_grid(-row ~ col) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_fill_carto_c(
    palette = "Temps", type = "diverging",
    breaks = c(35.3, 48.7),
    limits = c(34, 50),
    labels = str_wrap(c(min_label, max_label), 12),
    guide = guide_colorbar(barheight = unit(0.5, "cm"),
                           barwidth = unit(9.5, "cm"), 
                           direction = "horizontal")
  ) +
  coord_fixed(expand = FALSE) +
  theme_void(base_size = 10) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    strip.text = element_blank(),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.4),
      vjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 5),
      family = body_font,
      maxwidth = 1,
      lineheight = 1,
      vjust = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(
      colour = text_col,
      hjust = 0.5,
      margin = margin(5, 0, 0, 0),
      family = body_font,
      lineheight = 0.9
    ),
    legend.ticks = element_blank(),
    legend.text.position = "bottom",
    panel.spacing = unit(0.1, "lines")
  ) 


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/day_17.png",
  bg = bg_col, height = 6, width = 4, units = "in"
)
