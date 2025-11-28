library(ggplot2)
library(dplyr)
library(stringr)
library(showtext)
library(nrBrand)
library(ggtext)
library(geofacet)
library(camcorder)
library(glue)


# Fonts -------------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Space Grotesk", "Space")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Space"


# Define colours ----------------------------------------------------------

bg_col <- "grey97"
text_col <- "grey10"


# Functions ---------------------------------------------------------------

is_even <- function(x) {
  return((x %% 2) == 0)
}

make_hex_coords <- function(x0, y0, r) {
  angles <- seq(pi / 6, 2 * pi + pi / 6, length.out = 7)
  hexagon_coords <- function(xc, yc, rad, id) {
    x <- xc + rad * cos(angles)
    y <- yc + rad * sin(angles)
    data.frame(x = x, y = y, grp = id, x_grp = xc, y_grp = yc)
  }
  result <- do.call(
    rbind,
    mapply(hexagon_coords, x0, y0, r, seq_along(x0),
      SIMPLIFY = FALSE
    )
  )

  return(result)
}


# Start recording ---------------------------------------------------------

gg_record(
  "recording",
  device = "png",
  width = 5,
  height = 7,
  units = "in",
  dpi = 300
)


# Data --------------------------------------------------------------------

grid_data <- world_countries_grid1
class(grid_data) <- "data.frame"

final_grid_data <- grid_data |>
  as_tibble() |>
  select(name, code_alpha3, col, row) |>
  mutate(
    new_col = if_else(
      is_even(row), col + 0.5, col
    )
  )

hex_data <- make_hex_coords(
  x0 = final_grid_data$new_col,
  y0 = final_grid_data$row,
  r = rep(0.5, nrow(final_grid_data))
) |>
  tibble::as_tibble()

plot_data <- left_join(
  hex_data, final_grid_data, by = c("x_grp" = "new_col", "y_grp" = "row")
) |> 
  mutate(
    first_letter = str_sub(code_alpha3, 1, 1)
  )


# Text --------------------------------------------------------------------

single_use <- plot_data |> 
  group_by(grp) |> 
  slice_head() |> 
  ungroup() |> 
  count(first_letter) |> 
  filter(n == 1) |> 
  pull(first_letter) |> 
  sort()
single_use_str <- glue_collapse(single_use, sep = ', ', last = ', and ')
single_use_names <- plot_data |> 
  filter(first_letter %in% single_use) |> 
  select(name, first_letter) |> 
  distinct() |> 
  arrange(first_letter) |> 
  pull(name)
single_use_names_str <- glue_collapse(single_use_names, sep = ', ', last = ', and ')

title <- glue("What do the letters {single_use_str} have in common?")
st <- glue("Each of the 26 letters in the modern English alphabet is used as the first letter of the ISO 3166-1 alpha-3 code for at least one country. The letters {single_use_str} are used as the first letter for only one country each - {single_use_names_str} respectively.")
social <- social_caption(
  mastodon = NA,
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
cap <- paste0(
  "**Data**: geofacet R package<br>**Graphic**: ", social
)

all_palette <- PrettyCols::prettycols("Fun")
fill_palette <- c(rep(all_palette, each = 5), text_col)
col_palette <- c(rep(all_palette, times = 5), text_col)

names(fill_palette) <- LETTERS
names(col_palette) <- LETTERS

# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_polygon(
    mapping = aes(x = x, y = y, group = grp, fill = first_letter, 
                  colour = first_letter),
    linewidth = 1
  ) +
  scale_fill_manual(
    values = fill_palette
  ) +
  scale_colour_manual(
    values = col_palette
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_y_reverse() +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  coord_fixed(expand = FALSE, clip = "off") +
  theme_void(base_family = body_font, base_size = 11) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text.position = "bottom",
    legend.key.size = unit(0.15, "in"),
    legend.key.spacing = unit(0.03, "in"),
    legend.box.spacing = unit(0.3, "in"),
    plot.margin = margin(0, 10, 0, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col, family = title_font,
      halign = 0, hjust = 0,
      size = rel(1.6),
      face = "bold",
      margin = margin(t = 0, b = 5)
    ),
    plot.subtitle = element_textbox_simple(
      halign = 0, hjust = 0,
      colour = text_col,
      family = body_font,
      margin = margin(t = 5, b = 15),
      maxwidth = 1
    ),
    plot.caption = element_textbox_simple(
      halign = 0, hjust = 0,
      colour = text_col,
      family = body_font,
      margin = margin(t = 15)
    )
  )


# Save --------------------------------------------------------------------

ggsave("2025/maps/day_25.png",
  height = 7,
  width = 5,
  bg = bg_col,
  unit = "in"
)
