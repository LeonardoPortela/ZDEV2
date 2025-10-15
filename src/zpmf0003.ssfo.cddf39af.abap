
SELECT bwkey
  INTO @DATA(l_bwkey)
  FROM t001w
    UP TO 1 ROWS
 WHERE werks = @gs_ordem-iwerk.
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
    g_logo_imagem = 'LOGO_UNITAPAJOS_NOVO'.
  WHEN '0035' OR '0038'. "187073 bug solto / RGA
    g_logo_imagem = 'LOGO_ALZGRAOS'.
  WHEN OTHERS.
    g_logo_imagem = 'LOGO_NOVO'.
ENDCASE.














