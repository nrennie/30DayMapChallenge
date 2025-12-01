library(tidyverse)
library(sf)
library(showtext)
library(ggtext)
library(nrBrand)
library(camcorder)
library(glue)
library(MetBrewer)


# Data --------------------------------------------------------------------

uk_data <- read_sf("2025/data/Local_Authority_Districts_December_2021_UK_BUC_2022_6611447234830791685/LAD_DEC_2021_UK_BUC.shp")

uk_map <- uk_data |>
  select(LAD21NM, geometry) |> 
  mutate(n = nchar(LAD21NM))

longest <- uk_map |> 
  slice_max(n)
shortest <- uk_map |> 
  slice_min(n)


# Start recording ---------------------------------------------------------

gg_record(
  device = "png",
  width = 6,
  height = 8,
  units = "in",
  dpi = 300
)


# Colours -----------------------------------------------------------------

bg_col <- "grey99"
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
title <- "How long is a local authority district name?"
st <- glue("The median number of characters in a local authority district name is {median(uk_map$n)}. The longest name is {longest$LAD21NM} with {longest$n} characters whilst {glue_collapse(shortest$LAD21NM, sep = ', ', last = ', and ')} are tied for shortest with {unique(shortest$n)} characters.")
cap <- paste0(
  "**Data**: geoportal.statistics.gov.uk (2021)<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = uk_map,
    mapping = aes(fill = n),
    colour = bg_col
  ) +
  scale_fill_met_c("Tam", limits = c(0, 40)) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    fill = "Number of\ncharacters"
  ) +
  coord_sf(clip = "off") +
  theme_void(base_size = 11, base_family = body_font) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(-0.2, 0.5),
    legend.direction = "vertical",
    legend.key.height = unit(0.6, "in"),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5, l = -100),
      family = title_font,
      face = "bold",
      size = rel(1.6),
      vjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -50, t = 10, l = -100),
      family = body_font,
      width = 0.55,
      lineheight = 1,
      vjust = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 0, l = -100),
      family = body_font
    ),
    plot.margin = margin(5, -60, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/day_24.png",
  bg = bg_col, height = 8, width = 6, units = "in"
)
