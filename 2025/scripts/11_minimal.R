
# Packages ----------------------------------------------------------------

library(gapminder)
library(geofacet)
library(tidyverse)
library(ggforce)
library(camcorder)
library(glue)
library(nrBrand)
library(ggtext)
library(showtext)


# Start recording ---------------------------------------------------------

gg_record(
  device = "png",
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)


# Data --------------------------------------------------------------------

plot_data <- gapminder |>
  filter(year == 2007) |>
  left_join(
    country_codes,
    by = "country"
  ) |>
  select(iso_alpha, pop)


# Fonts -------------------------------------------------------------------

font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Text --------------------------------------------------------------------

social <- social_caption(
  bg_colour = "#0C2431",
  icon_colour = "#FFFFFF",
  font_colour = "#FFFFFF",
  font_family = "Ubuntu",
  mastodon = NA,
  bluesky = NA,
  linkedin = NA
)
cap <- glue("<span style='font-size: 24pt'>**Where are all the people?**</span><br><br>**Data** : Gapminder | **Graphic**: {social}")


# Plot --------------------------------------------------------------------

p <- ggplot(
  data = plot_data
) +
  geom_circle(
    mapping = aes(x0 = 1, y0 = 1, r = sqrt(pop)),
    colour = "transparent",
    fill = "#FFFFFF"
  ) +
  facet_geo(~iso_alpha,
    grid = "world_countries_grid1",
    label = "code_alpha3"
  ) +
  labs(caption = cap) +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "#0C2431", colour = "#0C2431"
    ),
    strip.text = element_blank(),
    legend.position = "none",
    panel.spacing = unit(-0.5, "lines"),
    plot.margin = margin(10, 10, 15, 10),
    plot.caption = element_textbox_simple(
      margin = margin(b = 0, t = -25),
      hjust = 0,
      halign = 0,
      colour = "#FFFFFF",
      family = "Ubuntu",
      size = 10
    )
  )
p
record_polaroid()


# Save --------------------------------------------------------------------

ggsave(p,
  filename = "2025/maps/11_minimal.png",
  bg = "#0C2431", height = 5, width = 6, units = "in"
)
