make_jitter_sf <- function(start_x = 1,
                           start_y = 1,
                           end_x = 5,
                           end_y = 5,
                           deg_jitter = 0.4,
                           col_palette = c(
                             "#008C45", "#F4F9FF", "#cdcdcd", "#CD212A"
                           ),
                           s = 1234) {
  set.seed(s)
  n_x <- end_x - start_x + 1
  n_y <- end_y - start_y + 1
  x <- rep(start_x:end_x, times = n_y) +
    stats::runif(n_x * n_y, 0, deg_jitter)
  y <- rep(start_y:end_y, each = n_x) +
    stats::runif(n_x * n_y, 0, deg_jitter)
  poly_data <- tibble::tibble(x = x, y = y) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      x_id = rep(start_x:end_x, times = n_y),
      y_id = rep(start_y:end_y, each = n_x)
    )
  x1 <- matrix(NA, ncol = n_x - 1, nrow = n_y - 1)
  x2 <- matrix(NA, ncol = n_x - 1, nrow = n_y - 1)
  x3 <- matrix(NA, ncol = n_x - 1, nrow = n_y - 1)
  x4 <- matrix(NA, ncol = n_x - 1, nrow = n_y - 1)
  y1 <- matrix(NA, ncol = n_x - 1, nrow = n_y - 1)
  y2 <- matrix(NA, ncol = n_x - 1, nrow = n_y - 1)
  y3 <- matrix(NA, ncol = n_x - 1, nrow = n_y - 1)
  y4 <- matrix(NA, ncol = n_x - 1, nrow = n_y - 1)
  group <- matrix(NA_character_, ncol = n_x - 1, nrow = n_y - 1)

  # create points

  x_vals <- start_x:(end_x - 1)
  y_vals <- start_y:(end_y - 1)

  for (i in 1:length(x_vals)) {
    for (j in 1:length(y_vals)) {
      x1[j, i] <- dplyr::filter(poly_data, .data$x_id == x_vals[i], .data$y_id == y_vals[j]) |> dplyr::pull(x)
      x2[j, i] <- dplyr::filter(poly_data, .data$x_id == x_vals[i] + 1, .data$y_id == y_vals[j]) |> dplyr::pull(x)
      x3[j, i] <- dplyr::filter(poly_data, .data$x_id == x_vals[i] + 1, .data$y_id == y_vals[j] + 1) |> dplyr::pull(x)
      x4[j, i] <- dplyr::filter(poly_data, .data$x_id == x_vals[i], .data$y_id == y_vals[j] + 1) |> dplyr::pull(x)

      y1[j, i] <- dplyr::filter(poly_data, .data$x_id == x_vals[i], .data$y_id == y_vals[j]) |> dplyr::pull(y)
      y2[j, i] <- dplyr::filter(poly_data, .data$x_id == x_vals[i] + 1, .data$y_id == y_vals[j]) |> dplyr::pull(y)
      y3[j, i] <- dplyr::filter(poly_data, .data$x_id == x_vals[i] + 1, .data$y_id == y_vals[j] + 1) |> dplyr::pull(y)
      y4[j, i] <- dplyr::filter(poly_data, .data$x_id == x_vals[i], .data$y_id == y_vals[j] + 1) |> dplyr::pull(y)

      i_val <- ifelse(i <= 9, paste0(i), paste0(0, i))
      j_val <- ifelse(j <= 9, paste0(j), paste0(0, j))
      group[j, i] <- paste0(i_val, "-", j_val)
    }
  }

  plot_data <- tibble::tibble(
    x = c(as.vector(x1), as.vector(x2), as.vector(x3), as.vector(x4)),
    y = c(as.vector(y1), as.vector(y2), as.vector(y3), as.vector(y4)),
    group = c(as.vector(group), as.vector(group), as.vector(group), as.vector(group))
  )
  polygons <- unique(plot_data$group)
  final_plot_data <- tibble::tibble(
    x = c(), y = c(), grp = c(), fill = c()
  )
  for (i in seq_len(length(polygons))) {
    p <- polygons[i]
    p_data <- dplyr::filter(plot_data, group == p)
    x_corners <- p_data$x
    y_corners <- p_data$y
    # convert to polygon
    sf_poly <- sf::st_polygon(
      list(cbind(
        c(x_corners, x_corners[1]),
        c(y_corners, y_corners[1])
      ))
    )
    # sample from middle
    mid <- sf::st_sample(sf_poly, 1) |>
      sf::st_coordinates() |>
      as.vector()
    # make polygons
    new_plot_data <- tibble::tribble(
      ~x, ~y, ~grp,
      # bottom
      x_corners[1], y_corners[1], paste0(p, "-", "1"),
      mid[1], mid[2], paste0(p, "-", "1"),
      x_corners[2], y_corners[2], paste0(p, "-", "1"),
      x_corners[1], y_corners[1], paste0(p, "-", "1"),
      # left
      x_corners[1], y_corners[1], paste0(p, "-", "2"),
      mid[1], mid[2], paste0(p, "-", "2"),
      x_corners[4], y_corners[4], paste0(p, "-", "2"),
      x_corners[1], y_corners[1], paste0(p, "-", "2"),
      # top
      x_corners[4], y_corners[4], paste0(p, "-", "3"),
      mid[1], mid[2], paste0(p, "-", "3"),
      x_corners[3], y_corners[3], paste0(p, "-", "3"),
      x_corners[4], y_corners[4], paste0(p, "-", "3"),
      # right
      x_corners[3], y_corners[3], paste0(p, "-", "4"),
      mid[1], mid[2], paste0(p, "-", "4"),
      x_corners[2], y_corners[2], paste0(p, "-", "4"),
      x_corners[3], y_corners[3], paste0(p, "-", "4")
    )
    # select colours
    new_plot_data$fill <- as.character(
      rep(sample(col_palette, size = 4), each = 4)
    )
    # join new data
    final_plot_data <- rbind(final_plot_data, new_plot_data)
  }

  # convert to sf
  final_plot_sf <- sf::st_as_sf(final_plot_data,
    coords = c("x", "y"),
    remove = FALSE
  ) |>
    dplyr::group_by(grp) |>
    dplyr::mutate(geometry = sf::st_combine(geometry)) |>
    dplyr::distinct() |>
    sf::st_cast("POLYGON")

  return(final_plot_sf)
}

# Example art data
sf_art <- make_jitter_sf(1, 1, 40, 40)
line_col <- "transparent"
text_col <- "gray30"
linewidth <- 0.1
bg_col <- "white"

# Example country data
italy <- rgeoboundaries::geoboundaries("Italy")
sf::st_crs(italy) <- 4326
italy_sf <- italy |>
  sf::st_transform(crs = 3003)

# rescale to unit square
new_sf_art <- sf_art |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    x = (x - sf::st_bbox(sf_art)[["xmin"]]) / ((sf::st_bbox(sf_art)[["xmax"]]) - sf::st_bbox(sf_art)[["xmin"]]),
    y = (y - sf::st_bbox(sf_art)[["ymin"]]) / ((sf::st_bbox(sf_art)[["ymax"]]) - sf::st_bbox(sf_art)[["ymin"]])
  ) |>
  dplyr::ungroup() |>
  sf::st_as_sf(
    coords = c("x", "y"),
    remove = FALSE
  ) |>
  dplyr::group_by(grp) |>
  dplyr::mutate(geometry = sf::st_combine(geometry)) |>
  dplyr::distinct() |>
  sf::st_cast("POLYGON")

x_diff <- sf::st_bbox(italy_sf)[["xmax"]] - sf::st_bbox(italy_sf)[["xmin"]]
y_diff <- sf::st_bbox(italy_sf)[["ymax"]] - sf::st_bbox(italy_sf)[["ymin"]]

if (y_diff > x_diff) {
  new_italy_sf <- italy_sf |>
    sf::st_coordinates() |>
    as.data.frame() |>
    dplyr::mutate(
      X = (x_diff / y_diff) * (X - sf::st_bbox(italy_sf)[["xmin"]]) / (sf::st_bbox(italy_sf)[["xmax"]] - sf::st_bbox(italy_sf)[["xmin"]]),
      Y = (Y - sf::st_bbox(italy_sf)[["ymin"]]) / (sf::st_bbox(italy_sf)[["ymax"]] - sf::st_bbox(italy_sf)[["ymin"]])
    ) |>
    sf::st_as_sf(
      coords = c("X", "Y"),
      remove = FALSE
    ) |>
    dplyr::group_by(L1, L2, L3) |>
    dplyr::summarise(do_union = FALSE) |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_cast("MULTIPOLYGON")
} else {
  new_italy_sf <- italy_sf |>
    sf::st_coordinates() |>
    as.data.frame() |>
    dplyr::mutate(
      X = (X - sf::st_bbox(italy_sf)[["xmin"]]) / (sf::st_bbox(italy_sf)[["xmax"]] - sf::st_bbox(italy_sf)[["xmin"]]),
      Y = (y_diff / x_diff) * (Y - sf::st_bbox(italy_sf)[["ymin"]]) / (sf::st_bbox(italy_sf)[["ymax"]] - sf::st_bbox(italy_sf)[["ymin"]])
    ) |>
    sf::st_as_sf(
      coords = c("X", "Y"),
      remove = FALSE
    ) |>
    dplyr::group_by(L1, L2, L3) |>
    dplyr::summarise(do_union = FALSE) |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_cast("MULTIPOLYGON")
}

# intersection
italy_crop <- sf::st_intersection(new_sf_art, new_italy_sf)

# load fonts
sysfonts::font_add_google("Special Elite", "elite")
showtext::showtext_auto()

# prep text
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = "elite"
)

# plot with ggplot2
ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = italy_crop,
    mapping = ggplot2::aes(fill = fill),
    colour = line_col,
    linewidth = linewidth
  ) +
  ggplot2::scale_fill_identity() +
  ggplot2::labs(
    title = "Italy",
    caption = social
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
    panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
    plot.title = ggplot2::element_text(
      family = "elite",
      size = 65,
      face = "bold",
      hjust = 1.1,
      margin = ggplot2::margin(t = 10, b = -20),
      colour = text_col
    ),
    plot.caption = ggtext::element_textbox_simple(
      colour = text_col,
      family = "elite",
      halign = -0.2,
      hjust = -0.2,
      size = 18,
      margin = ggplot2::margin(b = 5, t = -15)
    ),
    plot.margin = ggplot2::margin(2, 4, 4, 2)
  )

# save
ggplot2::ggsave("2023/maps/26_minimal.png",
  width = 6,
  height = 6,
  bg = bg_col,
  unit = "in"
)
