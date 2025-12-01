# Packages ----------------------------------------------------------------

library(tidyverse)
library(rnaturalearth)
library(sf)
library(patchwork)
library(glue)
library(showtext)
library(nrBrand)
library(ggtext)


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
showtext_auto()
showtext_opts(dpi = 300)


# Data --------------------------------------------------------------------

world_data <- ne_countries(type = "countries", scale = "small")


# Function ----------------------------------------------------------------

plot_map <- function(crs, data = world_data) {
  map_data <- world_data |>
    st_transform(crs = crs[1])
  map_data$title <- glue("{crs[1]} ({crs[2]})")
  p <- ggplot(
    data = map_data
  ) +
    geom_sf(fill = "grey95", colour = "grey20") +
    facet_wrap(~title) +
    coord_sf(expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "grey10", colour = "grey10"),
      panel.background = element_rect(fill = "grey10", colour = "grey10"),
      strip.text = element_text(
        colour = "grey95",
        margin = margin(b = 3, t = 8),
        family = "Oswald"
      )
    )
  return(p)
}


# Plot --------------------------------------------------------------------

crs_list <- list(
  c("EPSG:4326", "World Geodetic System 1984"),
  c("ESRI:54034", "World Cylindrical Equal Area"),
  c("ESRI:54030", "Robinson"),
  c("ESRI:54009", "Mollweide")
)


plot_list <- map(
  .x = crs_list,
  .f = ~ plot_map(.x)
)

wrap_plots(plot_list, ncol = 1) +
  plot_annotation(title = "How do you look at the Earth?",
                  caption = paste0("Graphic: ", social_caption(
                    mastodon = NA,
                    bg_colour = "grey10",
                    icon_colour = "grey95",
                    font_colour = "grey95",
                    font_family = "Oswald"
                  ))) &
  theme(
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "lines"),
    plot.title = element_text(
      colour = "grey95", family = "Oswald",
      hjust = 0.5,
      size = rel(1.8),
      margin = margin(t = 5, b = 5)
    ),
    plot.caption = element_textbox_simple(
      halign = 0.5, hjust = 0.5,
      colour = "grey95",
      margin = margin(t = 10)
    ),
    plot.background = element_rect(fill = "grey10", colour = "grey10"),
    panel.background = element_rect(fill = "grey10", colour = "grey10")
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/day_05.png",
  bg = "grey10", height = 10, width = 5, units = "in"
)
