nextflow.enable.dsl=2

process lr_n_auc {
label 'lr_n_auc'
publishDir "result", mode: 'copy'
input:
path input_df
path input_res2

output:
path "lr_n_auc.csv"

"""
python3 /scripts/lr_n_auc.py $input_df $input_res2 lr_n_auc.csv
"""
}