library(camcorder)

gg_record(
  "recording",
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

gg_playback(
  name = "2025/maps/day_23.gif",
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "black"
)
