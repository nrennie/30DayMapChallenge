import geopandas as gpd
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable

world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
sa = world[(world.continent=="South America")]

plt.figure()
fig, ax = plt.subplots(1, 1)
divider = make_axes_locatable(ax)
cax = divider.append_axes("bottom", size="5%", pad=0.5)
sa.plot(column='gdp_md_est',
        ax=ax,
        legend=True,
        cax=cax,
        cmap='viridis',
        legend_kwds={'label': "Estimated GDP",
                     'orientation': "horizontal",
                     'pad': 0.01,
                     'fmt': '%f'})
plt.title('South America')
ax.axis('off')
plt.ticklabel_format(useOffset=False, style='plain')
plt.show()

fig = plt.gcf()
fig.set_size_inches(6, 6)
fig.savefig('2022/maps/day_29.png', dpi=300)
