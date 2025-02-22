nextflow.enable.dsl=2

process rf_imp {
label 'rf_imp'
publishDir "result", mode: 'copy'
input:
path input_df
path input_immun

output:
path "rf_imp.csv"

"""
python3 /scripts/rf_imp.py $input_df $input_immun rf_imp.csv
"""
}