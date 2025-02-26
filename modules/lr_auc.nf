nextflow.enable.dsl=2

process lr_auc {
label 'lr_auc'
input:
path input_df
path input_immun
path res_lr

output:
path "auc_curv.csv"

"""
python3 /scripts/lr_auc.py $input_df $input_immun $res_lr auc_curv.csv
"""
}