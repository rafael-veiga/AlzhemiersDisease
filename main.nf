nextflow.enable.dsl=2
include { constructDataset } from './modules/constructDataset.nf'

params.adDataCombined = "$projectDir/data/AD_data_combined.csv"
params.ageCohort = "$projectDir/data/age_cohort.csv"

adDataCombined_ch = Channel.of(params.adDataCombined)
ageCohort_ch = Channel.of(params.ageCohort)


workflow{
    dados_ch = constructDataset(adDataCombined_ch,ageCohort_ch)
}
