DATA: vl_importe_tot TYPE char25,
v_waers1       TYPE char10,
v_decley       TYPE char3 ,
v_dec          TYPE char10,
st_spell       TYPE spell .

CALL FUNCTION 'SPELL_AMOUNT'
EXPORTING
amount    = v_IMP_NETO_T
*amount    = v_dmbtr_t
currency  = 'ARS'
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

v_waers1    = 'PESOS con'.

CONCATENATE st_spell-decimal(2) '/100' INTO v_dec.

CONCATENATE  'Son '
st_spell-word
v_waers1
v_dec
v_decley
INTO v_impletra SEPARATED BY space.











