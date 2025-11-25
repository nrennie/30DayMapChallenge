from ridge_map import RidgeMap
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.font_manager as fm
from ridge_map import FontManager

# override default font
fm = FontManager(github_url="https://github.com/google/fonts/blob/main/ofl/arvo/Arvo-Regular.ttf?raw=true")

# set up figure size and dpi
plt.figure(figsize=(8, 4), dpi=300, frameon=False)

# ridge map
rm = RidgeMap((-2.946396,53.884873,-2.352448,54.124), font=fm.prop)
values = rm.get_elevation_data(num_lines=100)
values=rm.preprocess(
    values=values,
    lake_flatness=1,
    water_ntile=0,
    vertical_ratio=140)
rm.plot_map(values=values,
            line_color = plt.get_cmap('cool'),
            label='Lancaster and the Forest of Bowland',
            label_y=0.01,
            label_x=0.7,
            background_color=np.array([65,74,76])/255,
            label_size=24,
            size_scale=30,
            linewidth=2)

# save figure
plt.savefig('2023/maps/02_lines.png', bbox_inches='tight', pad_inches = 0)
