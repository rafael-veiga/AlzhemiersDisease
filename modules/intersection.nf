nextflow.enable.dsl=2

process intersection {
label 'intersection'

input:
path input_file

output:
path "imp_aux_data.rds"

"""
Rscript /scripts/intersection.R $input_file imp_aux_data.rds
"""
}