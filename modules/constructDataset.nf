nextflow.enable.dsl=2

process constructDataset {
label 'constructDataset'

input:
path ADDataCombined
path AgeCohort

output:
    path "data_raw.rds"

"""
Rscript /scripts/construct_dataset.R $ADDataCombined $AgeCohort
"""
}
