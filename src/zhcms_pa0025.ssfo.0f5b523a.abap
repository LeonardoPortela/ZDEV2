IF pernr IS NOT INITIAL.
  SELECT SINGLE bukrs INTO gv_bukrs
    FROM pa0001
    WHERE pernr = pernr.
ENDIF.

IF gv_bukrs = '0043'.
  v_logo = 'LOGO MARCA AL5 BANK'.
ELSE.
  IF gv_bukrs = '0039'.
    v_logo = 'LOGO UNITAPAJOS'.
  ELSE.
    IF gv_bukrs = '0048'.
      v_logo = 'LOGO_TGG_SMART'.
    ELSE.
      v_logo = 'LOGO_NOVO'.
    ENDIF.
  ENDIF.
ENDIF.


















