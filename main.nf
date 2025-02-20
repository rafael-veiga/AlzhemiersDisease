nextflow.enable.dsl=2

include { constructDataset } from "$projectDir/modules/constructDataset.nf"
include { remove_missing } from "$projectDir/modules/remove_missing.nf"
include { boxcox } from "$projectDir/modules/boxcox.nf"
include { standart1 } from "$projectDir/modules/standart.nf"
include { standart2 } from "$projectDir/modules/standart.nf"
include { intersection } from "$projectDir/modules/intersection.nf"
include { imputation } from "$projectDir/modules/imputation.nf"
include { select_control } from "$projectDir/modules/select_control.nf"
include { logistic_mark } from "$projectDir/modules/logistic_mark.nf"
include { logistic_age } from "$projectDir/modules/logistic_age.nf"
include { logistic_mark_par } from "$projectDir/modules/logistic_mark_par.nf"


params.adDataCombined = "$projectDir/data/AD_data_combined.csv"
params.ageCohort = "$projectDir/data/age_cohort.csv"


adDataCombined_ch = Channel.of(params.adDataCombined)
ageCohort_ch = Channel.of(params.ageCohort)

workflow {
    (data_raw_ch, st1_ch, st2_ch, st3_ch) = constructDataset(adDataCombined_ch, ageCohort_ch)
    clean_data_ch = remove_missing(data_raw_ch)
    norm1_data_ch = boxcox(clean_data_ch)
    norm1_data_ch = standart1(norm1_data_ch)
    imp_ch = imputation(intersection(norm1_data_ch)) // for ml
    norm2_data_ch = standart2(clean_data_ch) // for LR
    res_lr_ch = logistic_mark(norm2_data_ch)
    res_lr_age_ch = logistic_age(select_control(norm2_data_ch))
    res_lr_par_ch = logistic_mark_par(norm2_data_ch)
}
