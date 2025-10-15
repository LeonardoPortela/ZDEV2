"Name: \TY:CL_J_1BNF_CONSISTENCY_CHECK\ME:CHK_TRIBUTARY_SIT\SE:BEGIN\EI
ENHANCEMENT 0 Z_MIGO_835.
*
  LOOP AT ms_nfe_data-t_item ASSIGNING FIELD-SYMBOL(<ls_item2>).
    IF <ls_item2>-matorg IS INITIAL.
      SELECT SINGLE mtorg
        FROM mbew
        INTO <ls_item2>-matorg
        WHERE matnr = <ls_item2>-matnr
        AND   bwkey = <ls_item2>-bwkey.
    ENDIF.
  ENDLOOP.

ENDENHANCEMENT.
