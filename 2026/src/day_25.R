# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(rvest)
library(terra)


# Functions ---------------------------------------------------------------

# Convert lat and lon data
dms_to_decimal <- function(x) {
  pattern <- "^\\s*(\\d+(?:\\.\\d+)?)\\s*[°º]\\s*"
  pattern <- paste0(
    pattern,
    "(\\d+(?:\\.\\d+)?)\\s*[′'’]\\s*",
    "(\\d+(?:\\.\\d+)?)\\s*[″\"”]\\s*",
    "([NSEW])\\s*$"
  )

  m <- regexec(pattern, x, ignore.case = TRUE)
  r <- regmatches(x, m)

  vapply(r, function(z) {
    if (length(z) == 0) {
      return(NA_real_)
    }

    degrees <- as.numeric(z[2])
    minutes <- as.numeric(z[3])
    seconds <- as.numeric(z[4])
    direction <- toupper(z[5])

    result <- degrees + minutes / 60 + seconds / 3600

    if (direction %in% c("S", "W")) {
      result <- -result
    }

    result
  }, numeric(1))
}

# Get elevation data
contours_data <- function(long, lat, dist_x, dist_y, local_crs = "EPSG:27700") {
  center_proj <-
    tibble::tibble(lat = lat, long = long) |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    sf::st_transform(crs = local_crs)

  center_coords <- sf::st_coordinates(center_proj)

  bbx_proj <- sf::st_bbox(
    c(
      xmin = center_coords[[1, "X"]] - dist_x,
      xmax = center_coords[[1, "X"]] + dist_x,
      ymin = center_coords[[1, "Y"]] - dist_y,
      ymax = center_coords[[1, "Y"]] + dist_y
    ),
    crs = local_crs
  )

  bbx <-
    bbx_proj |>
    sf::st_as_sfc() |>
    sf::st_transform(crs = 4326) |>
    sf::st_bbox()

  xmin <- bbx["xmin"]
  xmax <- bbx["xmax"]
  ymin <- bbx["ymin"]
  ymax <- bbx["ymax"]

  # get elevation data
  elev_data <- suppressMessages(elevatr::get_elev_raster(
    locations = data.frame(x = c(xmin, xmax), y = c(ymin, ymax)),
    z = 12,
    prj = "EPSG:4326",
    clip = "locations"
  ))
  elev_data_rast <- terra::rast(elev_data)
  return(elev_data_rast)
}

# Process elevation data
process_data <- function(i) {
  tidy_elev <- elev_data[[i]] |>
    as.data.frame(xy = TRUE) |>
    as_tibble() |>
    rename(elev = 3)
  
  summit_i <- tidy_elev |> 
    slice_max(elev) |> 
    mutate(
      diff = y - hill_data$latitude[i],
      abs_diff = abs(diff)
    ) |> 
    slice_min(abs_diff, with_ties = FALSE)
  
  tidy_elev |> 
    filter(y == summit_i$y)
}


# Load data ---------------------------------------------------------------

wiki_url <- "https://en.wikipedia.org/wiki/Hills_in_Edinburgh"

raw_html <- read_html(wiki_url)

tidy_html <- raw_html |>
  html_element("ul#mwQQ") |>
  html_elements("li")

hill_heights <- tidy_html |> 
  html_text2() |> 
  str_extract("(?<=\\().*?(?=m)") |> 
  parse_number()

hill_names <- tidy_html |>
  html_element("a") |>
  html_text()

hill_lats <- tidy_html |>
  html_element(".latitude") |>
  html_text() |>
  dms_to_decimal()

hill_long <- tidy_html |>
  html_element(".longitude") |>
  html_text() |>
  dms_to_decimal()

hill_data <- tibble(
  name = hill_names,
  latitude = hill_lats,
  longitude = hill_long,
  height = hill_heights
) |>
  arrange(desc(latitude)) |>
  mutate(name = factor(name, level = name)) |> 
  mutate(height = if_else(name == "Craiglockhart Hill", "East: 158m / West: 175m", paste0(height, "m")))


# Elevation data ----------------------------------------------------------

elev_data <- purrr::map(
  .x = 1:nrow(hill_data),
  .f = ~ contours_data(
    lat = hill_data$latitude[.x],
    long = hill_data$longitude[.x],
    dist_x = 500,
    dist_y = 500,
    local_crs = "EPSG:27700"
  )
)


# Data wrangling ----------------------------------------------------------

all_data <- purrr::map(
  .x = 1:nrow(hill_data),
  .f = ~ process_data(.x)
)
names(all_data) <- hill_data$name

plot_data <- all_data |>
  bind_rows(.id = "name") |>
  mutate(name = factor(name, level = levels(hill_data$name))) |> 
  left_join(hill_data, by = "name")

label_data <- plot_data |> 
  group_by(name) |> 
  slice_max(elev, with_ties = FALSE) |> 
  ungroup()


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
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Seven Hills of Edinburgh"
st <- "Height above sea-level of East-West cross sections of the traditional *Seven Hills of Edinburgh*, 500m either side of the summit, ordered from north to south."
cap <- source_caption(source = "Wikipedia | Open Topography", graphic = social)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_area(
    data = plot_data,
    mapping = aes(x = x, y = elev),
    fill = highlight_col,
    alpha = 0.5
  ) +
  geom_line(
    data = plot_data,
    mapping = aes(x = x, y = elev),
    colour = highlight_col,
    linewidth = 1
  ) +
  geom_text(
    data = label_data,
    mapping = aes(x = longitude, y = elev + 15, label = height),
    vjust = 0,
    size = 3.5,
    colour = text_col,
    family = body_font
  ) +
  labs(title = title, 
       subtitle = st,
       caption = cap,
       x = NULL, y = NULL) +
  facet_wrap(~name, ncol = 1, scales = "free_x", strip.position = "bottom") +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
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
      margin = margin(b = 5, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.text.x.bottom = element_text(
      face = "bold",
      margin = margin(t = 3),
      size = rel(1),
      angle = 0,
      hjust = 0
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    panel.spacing.y = unit(1, "lines"),
    strip.clip = "off"
  ) +
  canvas(
    width = 4, height = 8,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = "2026/maps/day_25.png"
)
