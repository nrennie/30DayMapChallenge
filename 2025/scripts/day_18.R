# Packages ----------------------------------------------------------------

library(rvest)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(showtext)
library(ggtext)
library(ggfx)
library(nrBrand)
library(camcorder)


# Data --------------------------------------------------------------------

raw_html <- read_html_live(
  "https://en.wikipedia.org/wiki/List_of_rocket_launch_sites"
)

coords_df <- raw_html |>
  html_elements(".geo-dec") |>
  html_text() |>
  tibble(raw = _) |>
  separate_wider_delim(raw, delim = " ", names = c("latitude", "longitude")) |>
  mutate(
    latitude = case_when(
      str_ends(latitude, "N") ~ parse_number(latitude),
      str_ends(latitude, "S") ~ parse_number(latitude) * -1
    ),
    longitude = case_when(
      str_ends(longitude, "E") ~ parse_number(longitude),
      str_ends(longitude, "W") ~ parse_number(longitude) * -1
    )
  )

dms_string_to_dd <- function(dms_strings) {
  # Apply over each element
  vapply(dms_strings, function(dms_string) {
    # Extract degrees, minutes, seconds, hemisphere
    parts <- regmatches(dms_string,
                        regexec("([0-9]+)°([0-9]+)′([0-9.]+)″([NSEW])", dms_string))[[1]]
    
    if (length(parts) == 0) {
      return(NA_real_)  # return NA if format invalid
    }
    
    degrees <- as.numeric(parts[2])
    minutes <- as.numeric(parts[3])
    seconds <- as.numeric(parts[4])
    hemisphere <- parts[5]
    
    # Convert to decimal degrees
    dd <- degrees + minutes/60 + seconds/3600
    
    # Adjust sign for S/W
    if (hemisphere %in% c("S", "W")) {
      dd <- -dd
    }
    
    return(dd)
  }, numeric(1)) 
}

lats <- raw_html |>
  html_elements(".latitude") |> 
  html_text()

lons <- raw_html |>
  html_elements(".longitude") |> 
  html_text()

coords_df2 <- tibble(
  longitude = lons,
  latitude = lats
) |> 
  mutate(longitude = dms_string_to_dd(longitude),
         latitude = dms_string_to_dd(latitude)) 

plot_sf <- rbind(coords_df, coords_df2) |> 
  drop_na() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

world_data <- ne_countries(type = "countries", scale = "small")


# Start recording ---------------------------------------------------------

gg_record(
  device = "png",
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)


# Define colours ----------------------------------------------------------

bg_col <- "grey10"
text_col <- "grey90"


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
title <- "Out of this world!"
st <- "Rocket launch sites, also known as spaceports or cosmodromes are located around the world. This map includes sites which are no longer active."
cap <- paste0(
  "**Data**: Wikipedia<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = world_data,
    fill = "grey15", colour = "grey15"
  ) +
  with_outer_glow(
    geom_sf(
      data = plot_sf,
      fill = "white", colour = "white",
      size = 0.5
    ),
    colour = "white",
    sigma = 3,
    expand = 0
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.6),
      vjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font,
      maxwidth = 1,
      lineheight = 1,
      vjust = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 0),
      family = body_font
    ),
    plot.margin = margin(5, 5, 5, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/18_out_of_this_world.png",
  bg = bg_col, height = 4, width = 6, units = "in"
)
