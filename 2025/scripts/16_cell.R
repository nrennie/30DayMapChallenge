
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(nrBrand)
library(showtext)
library(ggforce)


# Load fonts --------------------------------------------------------------

font_add_google("Sour Gummy", "sour", db_cache = F)
showtext_auto()
showtext_opts(dpi = 300)


# Parameters --------------------------------------------------------------

bg_col <- "#005EB8"
text_col <- "white"
n <- 10
min_size <- 0.3


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = "sour",
  mastodon = NA
)


# Data --------------------------------------------------------------------

# Load data from https://geoportal.statistics.gov.uk/datasets/06d13e0421784911aa669768f25dcb18_0/explore?location=55.068500%2C-3.316942%2C5.86
# Get elevation data
uk <- sf::st_read("2025/data/CTRY_DEC_2024_UK_BUC/CTRY_DEC_2024_UK_BUC.shp")
scot_sf <- uk %>%
  select(CTRY24NM, geometry) %>%
  filter(CTRY24NM == "Scotland")
elev_data <- elevatr::get_elev_raster(
  locations = scot_sf,
  z = 3,
  clip = "locations"
)


# Data wrangling ----------------------------------------------------------

elev_mat <- terra::as.matrix(elev_data, wide = TRUE)
colnames(elev_mat) <- 1:ncol(elev_mat)
elev_df <- elev_mat |>
  as_tibble() |>
  mutate(y = row_number()) |>
  pivot_longer(-y, names_to = "x") |>
  mutate(x = as.numeric(x))
elev_plot <- elev_df |>
  mutate(value = ntile(value, n = n)) |>
  drop_na() |>
  mutate(
    r =
      min_size + (value - min(value, na.rm = TRUE)) /
        (max(value, na.rm = TRUE) - min(value, na.rm = TRUE)) * (1 - min_size)
  )


# Plot --------------------------------------------------------------------

ggplot() +
  geom_circle(
    data = elev_plot,
    mapping = aes(x0 = x, y0 = y, r = r / 2),
    fill = "white",
    colour = "transparent"
  ) +
  labs(title = "Scotland", caption = social) +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(5, 5, 5, 5),
    plot.title = element_text(
      family = "sour",
      face = "bold",
      colour = text_col,
      hjust = 0.2,
      size = rel(1.8),
      margin = margin(t = 30, b = -30)
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      size = rel(0.6),
      lineheight = 0.5,
      family = "sour",
      halign = 0.5,
      hjust = 0.5,
      margin = margin(b = 0, t = 15)
    )
  )


# Save --------------------------------------------------------------------

ggsave("2025/maps/16_cell.png",
  height = 6,
  width = 4,
  bg = bg_col,
  unit = "in"
)
