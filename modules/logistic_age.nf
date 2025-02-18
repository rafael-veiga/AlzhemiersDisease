nextflow.enable.dsl=2

process logistic_age {
label 'logistic_age'

input:
path input_file

output:
path "*.csv"

"""
Rscript /scripts/logistic_mark.R $input_file res_lr_age.csv 15
"""
}