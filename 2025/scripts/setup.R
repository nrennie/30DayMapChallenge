library(nrBrand)
library(maprints)
library(showtext)
library(ggplot2)
library(camcorder)

# Recording
gg_record(
  device = "png",
  width = 7,
  height = 7,
  units = "in",
  dpi = 300
)

# Colours
col_palette <- c("#16425B", "#48A9A6", "#FFFFFF", "#9F1934", "#0C2431")
bg_col <- "#E4DFDA"

# Fonts
font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)

# Functions
map_caption <- function(bg_col, text_col) {
  social <- social_caption(
    bg_colour = bg_col,
    icon_colour = text_col,
    font_colour = text_col,
    font_family = "Ubuntu",
    mastodon = NA
  )
  cap <- paste0("#30DayMapChallenge | ", social)
  return(cap)
}

theme_map <- function(bg_col, text_col) {
  ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(
        fill = bg_col, color = bg_col
      ),
      panel.background = ggplot2::element_rect(
        fill = bg_col, colour = bg_col
      ),
      plot.margin = ggplot2::margin(5, 10, 5, 10),
      plot.caption.position = "plot",
      plot.caption = ggtext::element_textbox_simple(
        margin = ggplot2::margin(b = -30, t = 30),
        hjust = 0.5,
        halign = 0.5,
        colour = text_col,
        family = "Ubuntu",
        size = 11
      )
    )
}
