nextflow.enable.dsl=2

process fig1 {
label 'fig1'
publishDir "result", mode: 'copy'
input:
path res_lr
path lr_n_auc
path auc_curv
path imp_data

output:
path "fig1.pdf"

"""
Rscript /scripts/fig1.R $res_lr $lr_n_auc $auc_curv $imp_data
"""
}
