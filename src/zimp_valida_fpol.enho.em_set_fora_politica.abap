METHOD set_fora_politica .

  IF me->nota-ck_fpol NE i_ck_fpol.
    me->ck_alterou = abap_true.
  ENDIF.

  me->nota-ck_fpol = i_ck_fpol.

ENDMETHOD.
