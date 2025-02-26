nextflow.enable.dsl=2

process pars_1 {
label 'pars_1'
publishDir "result", mode: 'copy'
input:
path input_file
path res_lr

output:
path "res_pars1.csv"
path "*.pdf"

"""
Rscript /scripts/pars_1.R $input_file $res_lr res_pars1.csv
"""
}
