DATA: zed_lookup_value_type_coupa TYPE zed_lookup_value_type_coupa.
DATA: ze_oper_obj_cont_coupa      TYPE ze_oper_obj_cont_coupa.
DATA: ze_chave_obj_cont_coupa     TYPE ze_chave_obj_cont_coupa.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01. "Text-s01: Parâmetros de execução

SELECT-OPTIONS: s_lookup FOR zed_lookup_value_type_coupa NO INTERVALS OBLIGATORY. "Lookup Value Coupa

PARAMETERS: p_days TYPE i. "Quantidade de dias

PARAMETERS p_batch TYPE c NO-DISPLAY.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-s02.

PARAMETERS: p_op_obj TYPE ze_oper_obj_cont_coupa.

SELECT-OPTIONS: s_chave FOR ze_chave_obj_cont_coupa NO INTERVALS.

SELECTION-SCREEN END OF BLOCK b2.
