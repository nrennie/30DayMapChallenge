
source("2025/scripts/setup.R")


# Packages ----------------------------------------------------------------

library(gapminder)
library(geofacet)
library(tidyverse)
library(ggforce)
library(camcorder)
library(glue)


# Start recording ---------------------------------------------------------

gg_record(
  device = "png",
  width = 6,
  height = 5.5,
  units = "in",
  dpi = 300
)


# Data --------------------------------------------------------------------

plot_data <- gapminder |> 
  filter(year == 2007) |> 
  left_join(
    country_codes, by = "country"
  ) |> 
  select(iso_alpha, pop)


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = col_palette[5],
  icon_colour = col_palette[3],
  font_colour = col_palette[3],
  font_family = "Ubuntu",
  mastodon = NA
)
cap <- glue("<span style='font-size: 24pt'>**The World's Population**</span><br><br>**Data**: Gapminder<br>**Graphic**: {social}")


# Plot --------------------------------------------------------------------

p <- ggplot(
  data = plot_data
) +
  geom_circle(
    mapping = aes(x0 = 1, y0 = 1, r = sqrt(pop)),
    colour = "transparent",
    fill = col_palette[3]
  ) +
  facet_geo(~iso_alpha, 
            grid = "world_countries_grid1",
            label="code_alpha3") +
  labs(caption = cap) +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = col_palette[5], colour = col_palette[5]
    ),
    strip.text = element_blank(),
    legend.position = "none",
    panel.spacing = unit(-0.5, "lines"),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = ggtext::element_textbox_simple(
      margin = ggplot2::margin(b = 0, t = -25),
      hjust = 0,
      halign = 0,
      colour = col_palette[3],
      family = "Ubuntu",
      size = 10
    )
  )


# Save --------------------------------------------------------------------

ggsave(p,
  filename = "2025/maps/11_minimal.png",
  bg = col_palette[5], height = 5.5, width = 6, units = "in"
)


