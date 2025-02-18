nextflow.enable.dsl=2

process select_control {
label 'select_control'

input:
path input_file

output:
path "data.rds"

"""
Rscript /scripts/select_control.R $input_file data.rds
"""
}