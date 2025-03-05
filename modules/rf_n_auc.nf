nextflow.enable.dsl=2

process rf_n_auc {
label 'rf_n_auc'
publishDir "result", mode: 'copy'
input:
path input_df
path input_par
path input_res

output:
path "rf_n_auc.csv"

"""
python3 /scripts/rf_n_auc.py $input_df $input_par $input_res rf_n_auc.csv
"""
}