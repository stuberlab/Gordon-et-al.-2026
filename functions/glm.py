# libraries
import numpy as np
import pandas as pd
from scipy.linalg import svd
import scipy.stats
from matplotlib import pyplot as plt
from sklearn import linear_model
import statsmodels.api as sm

# Use better wrapping to get null Y
# (Algorithm 1 in report)
def get_ynull_wrapping(y_full, seed=1):

    T = y_full.shape[0]

    # Wrap function
    def wrap_y(y_array, q):
        if q != 0:
            return(np.hstack((y_array[q:y_array.shape[0]],
                              y_array[0:q])))
        else:
            return(y_array)

    rng = np.random.default_rng(seed=seed)

    return_y = np.empty((y_full.shape[0], y_full.shape[1]))
    for i in range(y_full.shape[1]):
        q = rng.integers(0, T- 1)
        return_y[:, i] = wrap_y(y_full[:, i], q)
    return return_y

# #### NOTE: This function is modified to also get $\Delta R^2$ and the wrapping $p$-values for comparison.
def compute_pvalues_null_data(X_list, Y_full, Y_null, toggle_beta):
    # Error check input
    T = Y_null.shape[0]
    num_cells = np.shape(Y_null)[1]
    if Y_full.shape[0] != T or Y_full.shape[1] != Y_null.shape[1]:
        raise Exception('Y_full and Y_null should have same dimensions.')
    for xi, x_comp in enumerate(X_list):
        if x_comp.shape[0] != T:
            raise Exception(
                'X_list at index ' + str(xi) + ' has ' + x_comp.shape[0] + ' time points but should have ' + str(
                    T) + '.')

    # Compute null distribution of F-statistics
    num_cells_gen_ITI_F = Y_null.shape[1]
    f_stats_ITIs_allcells = np.empty((num_cells_gen_ITI_F, len(X_list)))  # (cells in dataset, chunks)
    f_stats_observed = np.empty((num_cells, len(X_list)))

    # Build full matrix of X and get SVD
    K = len(X_list)
    #X_full_func = np.ones(T).reshape(-1, 1)
    X_full_func = np.empty((T, 0))  # remove intercept 20260427
    for X_c in X_list:
        if len(X_c.shape) == 1:
            X_full_func = np.hstack((X_full_func, X_c.reshape(-1, 1)))
        else:
            X_full_func = np.hstack((X_full_func, X_c))
    M = X_full_func.shape[1]

    if toggle_beta:
        U, s, Vt = svd(X_full_func, full_matrices=False)  # keep s and Vt for beta weights 
    else:
        U = svd(X_full_func, full_matrices=False)[0]
    

    # Optional: Also get Delta R2
    delta_r2 = np.empty((num_cells, K))
    r2 = np.empty(num_cells)

    beta_weights = np.empty((num_cells, M))  # M includes intercept + all predictor columns 


    # Loop through dropped chunks
    for k in range(K):
        # Build reduced matrix
        X_c = X_list[k]
        m_k = np.shape(X_c)[1]
        #X_red = np.ones(T).reshape(-1, 1)
        X_red = np.empty((T, 0))  # remove intercept 20260427
        for kt in range(K):
            if kt != k:
                X_ct = X_list[kt]
                if len(X_ct.shape) == 1:
                    X_red = np.hstack((X_red, X_ct.reshape(-1, 1)))
                else:
                    X_red = np.hstack((X_red, X_ct))
        U_red = svd(X_red, full_matrices=False)[0]

        for cell in range(num_cells_gen_ITI_F):
            # F-stats for NULL data
            # ---------------------
            # Get RSS with SVD approach
            y_neu = Y_null[:, cell]
            rss = np.sum(y_neu ** 2) - np.sum((np.transpose(U) @ y_neu) ** 2)
            rss_red = np.sum(y_neu ** 2) - np.sum((np.transpose(U_red) @ y_neu) ** 2)
            f_stat_rand = ((rss_red - rss) / m_k) / (rss / (T - M))

            # Store F_stat
            f_stats_ITIs_allcells[cell, k] = f_stat_rand

            # F-stats for OBS data
            # --------------------
            y_neu = Y_full[:, cell]
            rss = np.sum(y_neu ** 2) - np.sum((np.transpose(U) @ y_neu) ** 2)
            rss_red = np.sum(y_neu ** 2) - np.sum((np.transpose(U_red) @ y_neu) ** 2)
            f_stat_rand = ((rss_red - rss) / m_k) / (rss / (T - M))

            # Store F_stat
            f_stats_observed[cell, k] = f_stat_rand

            # EXTRA: also get delta R2
            tss = np.sum((y_neu - np.mean(y_neu)) ** 2)  # total sum of squares
            delta_r2[cell, k] = (1.0 - (rss / tss)) - (1.0 - (rss_red / tss))

            if toggle_beta:
                beta_weights[cell, :] = (Vt.T * (1/s)) @ U.T @ y_neu 

            # store R2
            r2[cell] = (1.0 - (rss / tss))



    # Turn F-statistics into p-values and return them
    return_pvals = np.empty((num_cells, len(X_list)))
    for cell in range(num_cells):
        for k in range(K):
            return_pvals[cell, k] = (np.sum(f_stats_ITIs_allcells[:, k] >= f_stats_observed[cell, k]) + 1) / (
                        f_stats_ITIs_allcells.shape[0] + 1)
            

    return((return_pvals, delta_r2, r2, beta_weights))  # returns blank beta_weights if not toggled



