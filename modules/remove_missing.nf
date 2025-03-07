nextflow.enable.dsl=2

process remove_missing {
label 'remove_missing'

input:
path cn_data

output:
    path "data_raw_f.rds"

"""
Rscript /scripts/remove_missing.R $cn_data data_raw_f.rds
"""
}
