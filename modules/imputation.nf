nextflow.enable.dsl=2

process imputation {
label 'imputation'

input:
path input_file

output:
path "imp_data.rds"

"""
Rscript /scripts/imputation.R $input_file imp_data.rds
"""
}