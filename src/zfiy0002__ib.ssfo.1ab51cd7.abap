DATA: vl_importe_tot TYPE char25,
v_waers1       type char10,
v_decley       type char3 ,
v_dec          type char10,
vl_imp         type char20,
st_spell       TYPE spell .


CALL FUNCTION 'SPELL_AMOUNT'
EXPORTING
amount    = V_RET
currency  = GS_RETENCION-waers
filler    = space
language  = 'S'
IMPORTING
in_words  = st_spell      "Output structure
EXCEPTIONS
not_found = 1
too_large = 2
OTHERS    = 3.

IF sy-subrc <> 0.
MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

write GS_RETENCION-retencion to vl_imp.
write  V_RET to vl_imp.
IF GS_RETENCION-waers EQ 'ARS'.
v_waers1    = 'PESOS con'.
concatenate   vl_imp
'PESOS'
into V_IMP_TOTAL separated by space.
ENDIF.


CONCATENATE st_spell-decimal(2) '/100' INTO v_dec.

CONCATENATE  'Son '
st_spell-word
v_waers1
v_dec
v_decley
INTO   V_IMPLETRA SEPARATED BY space.

CALL FUNCTION 'CONVERSION_EXIT_LDATE_OUTPUT'
  EXPORTING
    input         = GS_RETENCION-AUGDT
 IMPORTING
   OUTPUT        = v_fecha_2.

v_fecha_letras = v_fecha_2+4(15).

replace space WITH ',' into v_fecha_letras.
