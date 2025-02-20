nextflow.enable.dsl=2

process standart1 {
label 'standart1'

input:
path input_file

output:
path "data1.rds"

"""
Rscript /scripts/Standart.R $input_file data1.rds
"""
}

process standart2 {
label 'standart2'

input:
path input_file

output:
path "data2.rds"

"""
Rscript /scripts/Standart.R $input_file data2.rds
"""
}
