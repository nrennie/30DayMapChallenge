library(tidyverse)
library(jsonlite)
library(rnaturalearth)
library(geosphere)


# Functions ---------------------------------------------------------------

# Function to create a great-circle path
make_arc <- function(from_lon, from_lat, to_lon, to_lat, n = 100) {
  pts <- gcIntermediate(
    c(from_lon, from_lat),
    c(to_lon, to_lat),
    n = n,
    addStartEnd = TRUE,
    breakAtDateLine = FALSE
  )
  as.data.frame(pts) |>
    setNames(c("lon", "lat"))
}


# Data --------------------------------------------------------------------

json_url <- "https://raw.githubusercontent.com/Jonty/airline-route-data/refs/heads/main/airline_routes.json"
download.file(json_url, "2026/data/airline_routes.json")

raw_json <- fromJSON("2026/data/airline_routes.json") |>
  as_tibble()

edi_data <- raw_json |>
  select(EDI)

destinations <- edi_data[12, ][[1]] |>
  as_tibble() |>
  unnest(routes)

iata_lat <- raw_json[9, ] |>
  pivot_longer(everything(), names_to = "iata", values_to = "latitude") |>
  unnest(latitude)

iata_lon <- raw_json[10, ] |>
  pivot_longer(everything(), names_to = "iata", values_to = "longitude") |>
  unnest(longitude)

map_data <- iata_lat |>
  left_join(iata_lon, by = "iata") |>
  filter(iata %in% c("EDI", destinations$iata))

edi_coords <- map_data |>
  filter(iata == "EDI")

plot_data <- map_data |>
  filter(iata != "EDI") |>
  rename(
    to_lat = latitude,
    to_lon = longitude
  ) |>
  mutate(
    from_lat = edi_coords$latitude,
    from_lon = edi_coords$longitude
  ) |>
  mutate(across(-iata, as.numeric))

world <- ne_countries(scale = "medium", returnclass = "sf")
arcs <- map_dfr(seq_len(nrow(plot_data)), function(i) {
  arc <- make_arc(
    plot_data$from_lon[i],
    plot_data$from_lat[i],
    plot_data$to_lon[i],
    plot_data$to_lat[i]
  )

  arc$flight_id <- i
  arc
})

furthest <- destinations |> slice_max(km)

furthest_data <- raw_json |>
  select(all_of(furthest$iata))

# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#5C164E"


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Where can you fly to from Edinburgh?"
st <- glue("Direct flight routes from Edinburgh Airport (EDI) cover {nrow(destinations)} destinations.")
cap <- source_caption(source = "github.com/Jonty/airline-route-data", graphic = social)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(data = world, fill = "grey70", colour = bg_col) +
  geom_path(
    data = arcs,
    mapping = aes(x = lon, y = lat, group = flight_id),
    linewidth = 0.4,
    alpha = 0.5
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(x = to_lon, y = to_lat),
    size = 0.5
  ) +
  geom_textbox(
    data = plot_data |>
      filter(iata == furthest$iata),
    mapping = aes(
      x = to_lon, y = to_lat,
      label = glue("Longest flight: **{furthest_data[1,][[1]]}**<br>{formatC(furthest$km, big.mark = ',')}km<br>{round(furthest$min/60)} hours {furthest$min - round(furthest$min/60)*60} minutes")
    ),
    fill = alpha(bg_col, 0.3),
    box.colour = "transparent",
    colour = text_col,
    family = body_font,
    size = 3.5,
    hjust = 0.5,
    halign = 0.5,
    vjust = 1,
    valign = 1
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 85),
    expand = FALSE
  ) +
  labs(title = title, subtitle = st, caption = cap) +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    )
  ) +
  canvas(
    width = 7, height = 4,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = "2026/maps/day_02.png"
)
