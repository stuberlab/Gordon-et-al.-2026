# libraries

#%%
## standard libraries
import sys
import numpy as np
import pandas as pd
import math
from pathlib import Path
from matplotlib import pyplot as plt
import os
#%%

## custom libraries
sys.path.append(r'../functions')
from glm import get_ynull_wrapping
from glm import compute_pvalues_null_data


# GLM for multi-spout data
#%%
# notes
# current organization
#  indiviudal sessions located in ./data/sessions/
#  combined predictors  located in ./data/sessions_combined_subject/##/glm/
#  combined signals  located in ./data/sessions_combined_subject/##/glm/signals/


#%%
# define functions

def ensure_directory_exists(directory_path):
    if not os.path.exists(directory_path):
        os.makedirs(directory_path)

def get_folders(directory):
    return [name for name in os.listdir(directory) if os.path.isdir(os.path.join(directory, name))]

def get_file_names(directory):
    return [name for name in os.listdir(directory) if os.path.isfile(os.path.join(directory, name))]


def read_data(base_dir, file_names):
    data = {}
    for file_name in file_names:
        file_path = f"{base_dir}{file_name}"
        data[file_name[:-4]] = pd.read_csv(file_path, header=None).to_numpy()

    return data

def repeat_array_horizontally(array, n):
    # Replicate the array horizontally n times
    return np.tile(array, (1, n))



def padd_and_trim_null_vertically(Y_null, Y_all):
    Y_null_padded =  Y_null

    # stack Y_null to match or exceed rows in Y_all
    n = len(Y_all) / len(Y_null)  # Calculate how many times to stack Y_null

    for n_stack in np.arange(1, math.ceil(n)):

        # shuffle columns of Y_null and bind vertically
        shuffled_columns = np.random.permutation(Y_null.shape[1])

        Y_null_padded = np.vstack([Y_null_padded, Y_null[:, shuffled_columns]])

    # trim to length of Y_null to match Y_all
    Y_null_padded = Y_null_padded[np.arange(0, len(Y_all)),:]

    return(Y_null_padded)


def run_glm(subject_set, mode, dir_sets, dir_nulls, blockname_region_filtered, toggle_beta=0):
    """Run GLM for a subject set in 'true' or 'shuffled' mode.

    mode='true'     uses diagonal_true_* predictors, saves to analysis_output_glm/true/
    mode='shuffled' uses diagonal_shuffle_* predictors, saves to analysis_output_glm/shuffled/
    """

    if mode == 'true':
        predictor_ids = [
            'lick_kernal',
            'diagonal_true_trial',
            'diagonal_true_solution_conc_scaled_0_p1',
            'diagonal_true_solution_conc_scaled_0_p1_history03'
        ]
    elif mode == 'shuffled':
        predictor_ids = [
            'lick_kernal',
            'diagonal_shuffle_trial',
            'diagonal_shuffle_solution_conc_scaled_0_p1',
            'diagonal_shuffle_solution_conc_scaled_0_p1_history03'
        ]
    else:
        raise ValueError(f"mode must be 'true' or 'shuffled', got '{mode}'")

    dir_output = dir_sets + subject_set + f'/analysis_output_glm/{mode}/'
    ensure_directory_exists(dir_output)

    # read in predictors
    dir_predictors = dir_sets + subject_set + '/predictors/'
    fn_predictors = get_file_names(dir_predictors)
    data_x = read_data(dir_predictors, fn_predictors)

    X_full = []
    for predictor_id in predictor_ids:
        X_full.append(data_x[predictor_id])

    # read in categorical info
    df_cat = pd.read_csv(dir_sets + subject_set + '/sample_info.csv')

    # get signal file names
    dir_signals = dir_sets + subject_set + '/signals/'
    fn_signals = get_file_names(dir_signals)

    # filter to zscoreblsub
    fn_signals = [file for file in fn_signals if 'zscoreblsub' in file]

    for signal in fn_signals:
        print(' -' + signal)

        Y_all = pd.read_csv(dir_signals + signal, header=None).to_numpy()

        # for signals with individual sessions filtered
        blocks_in_df_cat = df_cat['blockname'].unique()
        signal_id = signal[:signal.rfind("_")]

        # for str fibers, filter blockname_region_filtered based on signal_id
        if signal_id.find("lha") == -1:
            blockname_region_filtered_signal_id = blockname_region_filtered[blockname_region_filtered['region_original'] == signal_id]
        else:
            blockname_region_filtered_signal_id = blockname_region_filtered

        blocks_filtered = blockname_region_filtered_signal_id[blockname_region_filtered_signal_id['blockname'].isin(blocks_in_df_cat)]

        filt_index = df_cat['blockname'].isin(blocks_filtered['blockname'])

        # if all blocks are filtered out, skip
        if filt_index.sum() == 0:
            print(f'  ~ filtered out by blockname_region_filtered for {signal_id}')
            continue

        # get index for licking trials
        filt_index = filt_index * df_cat['trial_lick']

        # filter Y and X based on index
        X_filt = [matrix[filt_index.astype(bool), :] for matrix in X_full]
        Y_filt = Y_all[filt_index.astype(bool)]

        print(f'   Y rows - full: {len(Y_all)}; filt: {len(Y_filt)}')
        print(f'   X rows - full: {len(X_filt[0])}; filt: {len(X_filt[0])}')

        # read in corresponding null matrix (produced in r)
        null_prefix = signal.split('.', 1)[0]
        Y_null = pd.read_csv(dir_nulls + null_prefix + '_null_data.csv', header=None).to_numpy()

        Y_null = padd_and_trim_null_vertically(Y_null, Y_filt)

        Y_all_padded = repeat_array_horizontally(Y_filt, Y_null.shape[1])

        Y_null_wrap = get_ynull_wrapping(Y_all_padded)

        if len(X_filt[1]) == len(Y_all_padded):

            pvals_wrap, delta_r2_wrap, r2_wrap, beta_weights = compute_pvalues_null_data(
                X_list=X_filt,
                Y_full=Y_all_padded,
                Y_null=Y_null_wrap,
                toggle_beta=toggle_beta
            )

            pvals_wrap_df = pd.DataFrame(pvals_wrap, columns=np.array(predictor_ids))
            pvals_wrap_df.head(1).to_csv(os.path.join(dir_output, f'pvals_{signal}'), index=False)

            delta_r2_wrap_df = pd.DataFrame(delta_r2_wrap, columns=np.array(predictor_ids))
            delta_r2_wrap_df.head(1).to_csv(os.path.join(dir_output, f'deltar2_{signal}'), index=False)

            r2_wrap_df = pd.DataFrame(r2_wrap)
            r2_wrap_df.head(1).to_csv(os.path.join(dir_output, f'r2_{signal}'), index=False)

            if toggle_beta:
                beta_weights_df = pd.DataFrame(beta_weights)
                beta_weights_df.head(1).to_csv(os.path.join(dir_output, f'beta_{signal}'), index=False)

        else:
            print('   * error, pred/sig length missmatch')


#%%
# define subject_sets
dir_sets = '../data/sessions_combined_subject/'
dir_nulls = '../data/glm_null_distributions/'
subject_sets = get_folders(dir_sets)
subject_sets = [s for s in subject_sets if (Path(dir_sets) / s / 'predictors').is_dir()]

blockname_region_filtered = pd.read_csv('../data/key_blockname_region_filtered.csv')

#%%
# note to self - running beta for true only

for subject_set in subject_sets:

    print(subject_set + ' true')
    run_glm(subject_set, 'true',     dir_sets, dir_nulls, blockname_region_filtered, toggle_beta=0)

    print(subject_set + ' shuffled')
    run_glm(subject_set, 'shuffled', dir_sets, dir_nulls, blockname_region_filtered, toggle_beta=0)


# %%
subject_sets
