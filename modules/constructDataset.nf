process constructDataset {
label 'constructDataset'

input:
path ADDataCombined
path AgeCohort

"""
Rscript /scripts/construct_dataset.R $ADDataCombined $AgeCohort
"""
}
