# %% [markdown]
# # A notebook that is a Python file
# The cells are comments, so the file runs as a script as well — and a
# markdown cell renders its math: $\sin^2 x + \cos^2 x = 1$.

# %%
import numpy as np

grid = np.linspace(0, 2 * np.pi, 9)
np.round(np.sin(grid), 3)

# %%
import matplotlib.pyplot as plt

fig, ax = plt.subplots(figsize=(5, 1.8))
ax.plot(grid, np.sin(grid), marker='o')
ax.set_title('sin over a turn')
fig.tight_layout()
plt.show()
