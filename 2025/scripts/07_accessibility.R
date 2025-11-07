library(tidyverse)
library(rnaturalearth)
library(patchwork)
library(showtext)
library(ggtext)
library(nrBrand)
library(camcorder)
library(glue)


# Fonts -------------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Start recording ---------------------------------------------------------

gg_record(
  device = "png",
  width = 5,
  height = 6,
  units = "in",
  dpi = 300
)


# Data --------------------------------------------------------------------

france <- ne_states(country = "france")
plot_data <- france |>
  filter(type_en == "Metropolitan department") |>
  select(name, geometry)


# Functions ---------------------------------------------------------------

deut <- function(c) colorspace::deutan(c, severity = 1)
prot <- function(c) colorspace::protan(c, severity = 1)
trit <- function(c) colorspace::tritan(c, severity = 1)
des <- function(c) colorspace::desaturate(c, severity = 1)

plot_map <- function(n_cols_i) {
  plot_data$col <- as.character(
    sample(seq_len(n_cols_i),
      size = nrow(plot_data),
      replace = TRUE
    )
  )
  p <- ggplot(
    data = plot_data
  ) +
    geom_sf(
      mapping = aes(fill = col)
    ) +
    coord_sf(expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0)
    )
  return(p)
}


# Plot --------------------------------------------------------------------

set.seed(7)
n_cols <- c(2, 3, 4, 7, 10)

all_plots <- list()
for (i in seq_len(length(n_cols))) {
  g <- cowplot::plot_grid(plot_map(n_cols[i])) +
    theme(
      plot.title = element_text(
        colour = "grey10", family = "Oswald",
        hjust = 0.5,
        size = rel(0.8),
        margin = margin(t = 5, b = 5)
      )
    )
  p1 <- cowplot::plot_grid(colorblindr:::edit_colors(g, deut)) +
    theme(
      plot.title = element_text(
        colour = "grey10", family = "Oswald",
        hjust = 0.5,
        size = rel(0.8),
        margin = margin(t = 5, b = 5)
      )
    )
  p2 <- cowplot::plot_grid(colorblindr:::edit_colors(g, prot)) +
    theme(
      plot.title = element_text(
        colour = "grey10", family = "Oswald",
        hjust = 0.5,
        size = rel(0.8),
        margin = margin(t = 5, b = 5)
      )
    )
  p3 <- cowplot::plot_grid(colorblindr:::edit_colors(g, trit)) +
    theme(
      plot.title = element_text(
        colour = "grey10", family = "Oswald",
        hjust = 0.5,
        size = rel(0.8),
        margin = margin(t = 5, b = 5)
      )
    )
  p4 <- cowplot::plot_grid(colorblindr:::edit_colors(g, des)) +
    theme(
      plot.title = element_text(
        colour = "grey10", family = "Oswald",
        hjust = 0.5,
        size = rel(0.8),
        margin = margin(t = 5, b = 5)
      )
    )
  g <- cowplot::plot_grid(plot_map(n_cols[i]),
    labels = glue("{n_cols[i]}\ncolours"),
    label_size = 10,
    label_fontfamily = "Oswald",
    label_fontface = "plain",
    label_colour = "grey10",
    label_y = 0.8,
    label_x = -0.1,
    hjust = 0.5
  ) +
    theme(
      plot.title = element_text(
        colour = "grey10", family = "Oswald",
        hjust = 0.5,
        size = rel(0.8),
        margin = margin(t = 5, b = 5)
      )
    )
  if (i == 1) {
    g <- g + labs(title = "Original")
    p1 <- p1 + labs(title = "Deutanomaly")
    p2 <- p2 + labs(title = "Protanomaly")
    p3 <- p3 + labs(title = "Tritanomaly")
    p4 <- p4 + labs(title = "Desaturated")
  }
  all_plots[[i]] <- list(g, p1, p2, p3, p4)
}
all_plots_list <- list_flatten(all_plots)

wrap_plots(all_plots_list) +
  plot_annotation(
    title = "How accessible is your map?",
    subtitle = "The default categorical colour palette used in *ggplot2* is not accessible to people with different types of colour vision deficiency, especially when more colours are required.",
    caption = paste0("Graphic: ", social_caption(
      mastodon = NA,
      bg_colour = "grey95",
      icon_colour = "grey10",
      font_colour = "grey10",
      font_family = "Ubuntu"
    )),
    theme = theme(
      plot.margin = margin(10, 10, 10, 30),
      panel.spacing = unit(0, "lines"),
      plot.title = element_text(
        colour = "grey10", family = "Oswald",
        hjust = 0,
        size = rel(1.6),
        margin = margin(t = 0, b = 5, l = -20)
      ),
      plot.subtitle = element_textbox_simple(
        halign = 0, hjust = 0,
        colour = "grey10",
        family = "Ubuntu",
        margin = margin(t = 5, b = 10, l = -20),
        maxwidth = 1.1
      ),
      plot.caption = element_textbox_simple(
        halign = 0, hjust = 0,
        colour = "grey10",
        family = "Ubuntu",
        margin = margin(t = 15, l = -20)
      ),
      plot.background = element_rect(fill = "grey95", colour = "grey95"),
      panel.background = element_rect(fill = "grey95", colour = "grey95")
    )
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/07_accessibility.png",
  bg = "grey95", height = 6, width = 5, units = "in"
)
