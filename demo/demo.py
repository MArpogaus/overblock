# %% [markdown]
# # pycell
#
# Notebook style results for Python code cells, built from `python.el`
# and `comint-mime` alone. No Jupyter kernel, no zmq module.
#
# Markdown cells like this one render in place, with **bold** text,
# `code`, links and simple math such as $E = mc^2$.

# %%
import numpy as np

rng = np.random.default_rng(0)
data = rng.normal(size=1000)
print(f"mean {data.mean():+.4f}   std {data.std():.4f}")

# %%
import time

for step in range(3):
    time.sleep(1.2)
print("done")

# %% [markdown]
# ## Figures show up inline
#
# The result block carries whatever `comint-mime` rendered, images
# included. Press the buttons in its header bar to fold it, to open it
# in its own buffer, or to copy it.

# %%
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

fig, ax = plt.subplots(figsize=(5.2, 2.4), dpi=110)
ax.hist(data, bins=42, color="#7aa2f7", edgecolor="none")
ax.set_title("1000 samples")
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout()
fig
