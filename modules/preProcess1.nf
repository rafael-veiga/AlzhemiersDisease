nextflow.enable.dsl=2

include { select_control } from './select_control.nf' as SelectControl
include { boxcox } from './boxcox.nf' as BoxCox
include { standart } from './standart.nf' as Standart

workflow preProcess2 {
    take:
    path file

    main:
    file_cleaned = SelectControl(file)         // Passa o arquivo para select_control
    file_boxcox = BoxCox(file_cleaned)         // Passa a saída para boxcox
    cleaned_ch = Standart(file_boxcox)         // Passa a saída para standart

    emit:
    cleaned_ch                                 // Retorna a saída final
}
