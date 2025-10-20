DATA vl_data_aux(10) type c.

write sy-datum to vl_data_aux.

CONCATENATE: FORNECEDOR-CITY2
             ','
             vl_data_aux
             INTO VG_DATA.

REPLACE ALL OCCURRENCES OF '.' IN vg_data WITH '/'.

VG_ASS_EMPRESA = FORNECEDOR-BUTXT.
VG_ASS_PARCEIRO = PARCEIRO-NAME1.


















