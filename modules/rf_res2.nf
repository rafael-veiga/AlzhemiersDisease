nextflow.enable.dsl=2

process rf_res2 {
label 'rf_res2'
publishDir "result", mode: 'copy'
input:
path input_df
path input_immun
path input_res2

output:
path "RF_imp.csv"

"""
python3 /scripts/rf_res2.py $input_df $input_immun $input_res2 RF_imp.csv
"""
}