nextflow.enable.dsl=2

process logistic_mark_par {
label 'logistic_mark_par'
publishDir "result", mode: 'copy'
input:
path input_file

output:
path "res_lr_par.csv"

"""
Rscript /scripts/logistic_mark_par.R $input_file res_lr_par.csv 15
"""
}
