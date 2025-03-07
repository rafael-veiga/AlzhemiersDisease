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
include { r_to_python } from "$projectDir/modules/r_to_python.nf"
include { lr_auc } from "$projectDir/modules/lr_auc.nf"
include { pars_1 } from "$projectDir/modules/pars_1.nf"
include { rf_res } from "$projectDir/modules/rf_res.nf"
include { rf_res2 } from "$projectDir/modules/rf_res2.nf"
include { lr_n_auc } from "$projectDir/modules/lr_n_auc.nf"
include { rf_n_auc } from "$projectDir/modules/rf_n_auc.nf"


params.adDataCombined = "$projectDir/data/AD_data_combined.csv"
params.ageCohort = "$projectDir/data/age_cohort.csv"


adDataCombined_ch = Channel.of(params.adDataCombined)
ageCohort_ch = Channel.of(params.ageCohort)

workflow {
    pre_ch = constructDataset(adDataCombined_ch, ageCohort_ch)
    clean_data_ch = remove_missing(pre_ch)
    b_data_ch = boxcox(clean_data_ch)
    norm_data_ch = standart1(b_data_ch)
    imp_ch = imputation(intersection(norm_data_ch)) // for ml
    res_lr_ch = logistic_mark(norm_data_ch)
    res_lr_age_ch = logistic_age(select_control(norm_data_ch))
    res_lr_par_ch = logistic_mark_par(norm_data_ch)
    (df_ch,imuno_ch) = r_to_python(imp_ch)
    importance_lr_ch = lr_auc(df_ch,imuno_ch,res_lr_ch)
    (rf_auc1_ch,rf_auc2_ch) = rf_res(df_ch,imuno_ch)
    rf_imp_ch = rf_res2(df_ch,imuno_ch,rf_auc2_ch)
    pars_1(imp_ch,res_lr_ch)
    lr_n_auc_ch = lr_n_auc(df_ch,res_lr_ch)
    rf_n_auc_ch = rf_n_auc(df_ch,rf_auc2_ch,rf_imp_ch)
}
