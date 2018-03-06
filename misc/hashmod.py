import matplotlib.pyplot as plt
import numpy as np
import plotly.tools as tls
import plotly

n = 1000


mods_changed = [[m1 % m2 for m1 in range(1, n + 1)] for m2 in range(1, n + 1)]


def num_hash_changed(mod1, mod2):
    count = 0
    for m1, m2 in zip(mods_changed[mod1], mods_changed[mod2]):
        if m1 is not m2:
            count += 1
    return count


values = []
for mod1 in range(n):
    row = []
    for mod2 in range(n):
        row.append(num_hash_changed(mod1, mod2))
    values.append(row)

trace = dict(z=values, type="heatmap", zmin=0, zmax=n)
print(trace)
fig = plt.figure()
ax = fig.add_subplot(111)

ax.set_title('Number of changes from mod x to mod y')

plotly_fig = tls.mpl_to_plotly(fig)

plotly_fig['data'] = [trace]

plotly_fig['layout']['xaxis'].update({'autorange':True})
plotly_fig['layout']['yaxis'].update({'autorange':True})

plotly.offline.plot(plotly_fig)