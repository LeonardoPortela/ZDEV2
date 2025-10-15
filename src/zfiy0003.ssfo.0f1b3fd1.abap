DATA: vl_importe_tot TYPE char25,
v_waers1       TYPE char10,
v_decley       TYPE char3 ,
v_dec          TYPE char10,
vl_imp         TYPE char20,
vl_imp_total   TYPE char50,
st_spell       TYPE spell,
v_flag         TYPE char1.

*IF sfsy-page EQ 1.

  LOOP AT t_recibo_pos.
    vl_dmbtr = vl_dmbtr + t_recibo_pos-dmbtr.
  ENDLOOP.
  vl_wrbtr = vl_dmbtr.

*ENDIF.
*Modificación 20/03
READ TABLE T_IMP_TOT INTO W_IMP_TOT
                      WITH KEY AUGBL = J_1AI02-AUGBL.
*Modificación 20/03
CALL FUNCTION 'SPELL_AMOUNT'
EXPORTING
*amount    = j_1ai02-WRBTR
*Modificación 20/03
amount    = W_IMP_TOT-importe
*Modificación 20/03
*Modificado 20/03
*amount    = v_total_f
*Modificado 20/03
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


WRITE j_1ai02-wrbtr TO vl_imp.
*IF j_1ai02-waers EQ 'ARS'.
v_waers1    = 'PESOS con'.
CONCATENATE   vl_imp
             'PESOS'
INTO vl_imp_total SEPARATED BY space.
*ENDIF.

*IF j_1ai02-waers EQ 'USD'.
*v_waers1   = 'DOLLARS'.
*v_decley   = 'USD'.
*ENDIF.

CONCATENATE st_spell-decimal(2) '/100' INTO v_dec.

CONCATENATE
st_spell-word
v_waers1
v_dec
v_decley
INTO   v_impletra SEPARATED BY space.

WRITE j_1ai02-wrbtr TO v_total RIGHT-JUSTIFIED.



