nextflow.enable.dsl=2

process rf_res {
label 'rf_res'
input:
path input_df
path input_immun

output:
path "RF_auc1.csv"
path "RF_auc2.csv"

"""
python3 /scripts/rf_res.py $input_df $input_immun RF_auc1.csv RF_auc2.csv
"""
}