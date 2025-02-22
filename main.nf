nextflow.enable.dsl=2

include { constructDataset } from "$projectDir/modules/constructDataset.nf"
include { boxcox } from "$projectDir/modules/boxcox.nf"
include { standart1 } from "$projectDir/modules/standart.nf"
include { standart2 } from "$projectDir/modules/standart.nf"
include { intersection } from "$projectDir/modules/intersection.nf"
include { imputation } from "$projectDir/modules/imputation.nf"
include { select_control } from "$projectDir/modules/select_control.nf"
include { logistic_mark } from "$projectDir/modules/logistic_mark.nf"
include { logistic_age } from "$projectDir/modules/logistic_age.nf"
include { logistic_mark_par } from "$projectDir/modules/logistic_mark_par.nf"
include { r_to_python } from "$projectDir/modules/r_to_python.nf"


params.adDataCombined = "$projectDir/data/AD_data_combined.csv"
params.ageCohort = "$projectDir/data/age_cohort.csv"


adDataCombined_ch = Channel.of(params.adDataCombined)
ageCohort_ch = Channel.of(params.ageCohort)

workflow {
    clean_data_ch = constructDataset(adDataCombined_ch, ageCohort_ch)
    norm1_data_ch = boxcox(clean_data_ch)
    norm1_data_ch = standart1(norm1_data_ch)
    imp_ch = imputation(intersection(norm1_data_ch)) // for ml
    norm2_data_ch = standart2(clean_data_ch) // for LR
    res_lr_ch = logistic_mark(norm2_data_ch)
    res_lr_age_ch = logistic_age(select_control(norm2_data_ch))
    res_lr_par_ch = logistic_mark_par(norm2_data_ch)
    (df_ch,imuno_ch) = r_to_python(imp_ch)
}
