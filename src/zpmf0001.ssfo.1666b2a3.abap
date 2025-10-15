READ TABLE it_equi INTO DATA(wa_equi) INDEX 1.

SELECT bwkey
  INTO @DATA(l_bwkey)
  FROM t001w
    UP TO 1 ROWS
 WHERE werks = @wa_equi-iwerk.
ENDSELECT.

SELECT bukrs
  INTO @DATA(l_bukrs)
  FROM t001k
    UP TO 1 ROWS
 WHERE bwkey = @l_bwkey.
ENDSELECT.

CASE l_bukrs.
  WHEN '0048'.
    g_logo_imagem = 'LOGO_NOVO'.
  WHEN '0039'.
    g_logo_imagem = 'LOGO_UNITAPAJOS_NOVO2'.
  WHEN OTHERS.
    g_logo_imagem = 'LOGO_NOVO'.
ENDCASE.























