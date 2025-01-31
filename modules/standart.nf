nextflow.enable.dsl=2

process standart {
label 'standart'

input:
path input_file

output:
path "data.rds"

"""
Rscript /scripts/Standart.R $input_file data.rds
"""
}