import numpy as np

def lims_xy(ax):
    lims = [
        np.min([ax.get_xlim(), ax.get_ylim()]),  # min of both axes
        np.max([ax.get_xlim(), ax.get_ylim()]),  # max of both axes
    ]
    return lims

def plot_diag(ax):
    lims = lims_xy(ax)
    ax.plot(lims, lims, ls='dotted', color='gray')


def x_linspace(ax, n=500):
    xlim = ax.get_xlim()
    return np.linspace(xlim[0], xlim[1], n)
