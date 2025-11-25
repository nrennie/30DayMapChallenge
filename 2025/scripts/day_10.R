# Set up ------------------------------------------------------------------

library(glue)
library(tidyverse)
library(camcorder)
library(nrBrand)
library(ggtext)
library(openair)
library(showtext)
library(rnaturalearth)
library(sf)
library(ggnewscale)


# Data --------------------------------------------------------------------

sites2025 <- importMeta(
  source = "aurn",
  year = 2025
)

all_data <- importUKAQ(
  site = sites2025$code, year = 2025,
  pollutant = "no2",
  data_type = "daily",
  source = "aurn"
)
subset_data <- all_data |>
  filter(date == "2025-11-06" | date == "2025-10-30")

plot_data <- sites2024 |>
  left_join(subset_data, by = "code") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  drop_na(date) |>
  arrange(no2) |> 
  mutate(date = case_when(
           date == "2025-11-06" ~ paste0(
             format(date, "%a %b %d %Y"),
             "<br><span style='font-family:", body_font, "; font-size:12pt;'>Day after Bonfire Night</span>"),
           TRUE ~ paste0(
             format(date, "%a %b %d %Y"),
             "<br><span style='font-family:", body_font, "; font-size:12pt;'>One week earlier</span>")
         ))
  


# Map ---------------------------------------------------------------------

uk <- ne_countries(
  country = c("united kingdom", "ireland"), scale = 50
) |>
  st_transform(crs = 4326)


# Fonts -------------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours ----------------------------------------------------------

bg_col <- "grey10"
text_col <- "grey90"
highlight_col <- "#EE4B2B"


# Start recording ---------------------------------------------------------

gg_record(
  "recording",
  device = "png",
  width = 7,
  height = 7.5,
  units = "in",
  dpi = 300
)


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Bonfire Night worsens air pollution"
st <- glue("Data from the Automatic Urban and Rural Network which measures air quality shows the dramatic effect of Bonfire Night on air pollution in the UK. Each circle represents a monitoring site. The size of the circle represents the amount of Nitrogen Dioxide measured in the air with <span style='color:{highlight_col}'>**sites exceeding a daily average of 40 µg/m<sup>3</sup>**</span> highighted in red.")
cap <- paste0(
  "**Data**: Automatic Urban and Rural Network via openair R package<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = uk,
    mapping = aes(fill = sovereignt),
    colour = bg_col,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c("grey15", "grey25"), guide = "none") +
  new_scale_fill() +
  geom_sf(
    data = plot_data,
    mapping = aes(size = no2, fill = (no2 >= 40), colour = (no2 >= 40), alpha = (no2 >= 40)),
    pch = 21
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_colour_manual(values = c(text_col, highlight_col), guide = "none") +
  scale_fill_manual(values = c(text_col, highlight_col), guide = "none") +
  facet_wrap(~date) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.6)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 5),
      family = body_font,
      lineheight = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 0, l = 5),
      family = body_font
    ),
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
    strip.text = element_textbox_simple(
      colour = text_col, family = title_font,
      size = rel(1.2),
      margin = margin(t = 5)
    )
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/10_air.png",
  bg = bg_col, height = 7.5, width = 7, units = "in"
)
