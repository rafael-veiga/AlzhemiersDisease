nextflow.enable.dsl=2

include { select_control } from "$projectDir/modules/select_control.nf"
include { boxcox        } from "$projectDir/modules/boxcox.nf"
include { standart      } from "$projectDir/modules/standart.nf"

workflow preProcess2 {
    take:
        path data_raw_ch

    main:
        file_cleaned = select_control(data_raw_ch)
        file_boxcox  = boxcox(file_cleaned)
        cleaned_ch   = standart(file_boxcox)

    emit:
        cleaned_ch
}
