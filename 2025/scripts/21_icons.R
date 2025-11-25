# Packages ----------------------------------------------------------------

library(gapminder)
library(geofacet)
library(tidyverse)
library(ggforce)
library(camcorder)
library(glue)
library(nrBrand)
library(ggtext)
library(scales)
library(showtext)


# Start recording ---------------------------------------------------------

gg_record(
  device = "png",
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)


# Define colours ----------------------------------------------------------

bg_col <- "grey97"
text_col <- "#0C2431"


# Data --------------------------------------------------------------------

plot_data <- gapminder |>
  filter(year == 2007) |>
  left_join(
    country_codes,
    by = "country"
  ) |>
  select(iso_alpha, pop, continent) |>
  mutate(
    r = sqrt(pop),
    r = rescale(r, to = c(0, 0.5))
  )


# Fonts -------------------------------------------------------------------

font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)
body_font <- "Ubuntu"


# Text --------------------------------------------------------------------

social <- social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA,
  bluesky = NA,
  linkedin = NA
)
cap <- glue("<span style='font-size: 24pt'>**Where are all the people?**</span><br><br>**Data** : Gapminder | **Graphic**: {social}")


# Plot --------------------------------------------------------------------

p <- ggplot(
  data = plot_data
) +
  geom_tile(
    mapping = aes(x = 1, y = 1, fill = continent),
    colour = bg_col,
    height = 1,
    width = 1,
    alpha = 0.2
  ) +
  geom_circle(
    mapping = aes(
      x0 = 1, y0 = 1, r = r,
      fill = continent
    ),
    colour = "transparent",
    linewidth = 0.5,
    alpha = 1
  ) +
  scale_fill_manual(
    values = c("#24796C", "#DAA51B", "#2F8AC4",
               "#764E9F", "#ED645A")
  ) +
  facet_geo(~iso_alpha,
    grid = "world_countries_grid1",
    label = "code_alpha3"
  ) +
  labs(caption = cap) +
  coord_fixed(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    strip.text = element_blank(),
    legend.position = "none",
    panel.spacing = unit(0, "lines"),
    plot.margin = margin(10, 10, 15, 10),
    plot.caption = element_textbox_simple(
      margin = margin(b = 0, t = -25),
      hjust = 0,
      halign = 0,
      colour = text_col,
      family = body_font,
      size = 10
    )
  )
p
record_polaroid()


# Save --------------------------------------------------------------------

gg_stop_recording()
ggsave(p,
  filename = "2025/maps/21_icons.png",
  bg = bg_col, height = 5, width = 6, units = "in"
)
