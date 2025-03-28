nextflow.enable.dsl=2

process fig2 {
label 'fig2'
publishDir "result", mode: 'copy'
input:
path res_lr
path data

output:
path "fig2.pdf"

"""
Rscript /scripts/fig2.R $res_lr $data
"""
}
