nextflow.enable.dsl=2

process logistic_mark {
label 'logistic_mark'

input:
path input_file

output:
path "*.csv"

"""
Rscript /scripts/logistic_mark.R $input_file res_lr.csv 15
"""
}