nextflow.enable.dsl=2

process r_to_python {
label 'r_to_python'

input:
path input_file

output:
path "data_py_df.csv"
path "data_py_immun.csv"

"""
Rscript /scripts/r_to_python.R $input_file data_py
"""
}