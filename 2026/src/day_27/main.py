# Packages
import geopandas as gp
import pandas as pd
import plotnine as pn
import textwrap

# Load data
ratios = pd.read_csv('2026/data/affordability.csv')

gdf = gp.read_file("2026/data/lad.geojson")
gdf = gdf[gdf["LAD25CD"].str.startswith(("E", "W"))]
gdf = gdf.to_crs("EPSG:27700")

# Join data
gdf = gdf.merge(
    ratios,
    how="left",
    left_on="LAD25CD",
    right_on="Local authority code"
)

# Fix data type
gdf["2025"] = pd.to_numeric(gdf["2025"], errors="coerce")

# Text
title_text = "The number of local authority areas with an affordability ratio above 12 has more than halved since 2021"
wrapped_title = '\n'.join(textwrap.wrap(title_text, width=45))

st_text = "Housing affordability ratio by local authority district, England and Wales, 2025"
wrapped_subtitle = '\n'.join(textwrap.wrap(st_text, width=65))

cap_text = "Housing affordability ratio, earnings, and house sales prices by local authority district, England and Wales, 1997 to 2025"
wrapped_cap = '\n'.join(textwrap.wrap(cap_text, width=80))

# Map
p = (
    pn.ggplot()
    + pn.geom_map(data=gdf, mapping=pn.aes(fill="2025"), color="#f4f4f4", size=0.1)
    + pn.scale_fill_cmap(cmap_name="viridis_r")
    + pn.labs(title=wrapped_title,
              subtitle=wrapped_subtitle,
              caption=wrapped_cap)
    + pn.theme_void()
    + pn.theme(
        plot_background=pn.element_rect(fill="#f4f4f4", color="#f4f4f4"),
        plot_margin_top=0.03,
        plot_margin_right=0.01,
        plot_margin_bottom=0.03,
        plot_margin_left=0.01,
        plot_title=pn.element_text(fontweight="bold"),
        legend_position="bottom",
        legend_key_width=250,
        legend_key_height=10,
        legend_title=pn.element_blank())
    + pn.coord_fixed()
)

# Save
pn.ggsave(p, filename="2026/maps/day_27.png", height=7.5, width=5, dpi=300)
