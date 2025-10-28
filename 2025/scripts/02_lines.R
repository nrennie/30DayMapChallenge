source("2025/scripts/setup.R")

library(maprints)

p <- street_map(
  location = "Edinburgh, UK",
  local_crs = 6384,
  dist = 4500,
  x_nudge = 0.1,
  y_nudge = 0.03,
  bg_col = bg_col,
  line_col = col_palette[4]
) +
  ggplot2::labs(caption = NULL)

ggplot2::ggsave(p,
                filename = "2025/maps/02_lines.png",
                bg = bg_col, height = 8, width = 8, units = "in"
)
