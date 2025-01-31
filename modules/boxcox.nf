nextflow.enable.dsl=2

process boxcox {
label 'boxcox'

input:
path input_file

output:
path "data.rds"

"""
Rscript /scripts/Boxcox.R $input_file data.rds
"""
}
