nextflow.enable.dsl=2

process logistic_age {
label 'logistic_age'
publishDir "result", mode: 'copy'
input:
path input_file

output:
path "*.csv"

"""
Rscript /scripts/logistic_age.R $input_file res_lr_age.csv 15
"""
}
