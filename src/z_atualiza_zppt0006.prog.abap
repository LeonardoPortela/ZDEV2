*&---------------------------------------------------------------------*
*& Report  Z_ATUALIZA_ZPPT0006
*&---------------------------------------------------------------------*
REPORT z_atualiza_zppt0006.

SELECT *
  FROM zppt0030
  INTO TABLE @DATA(t_0030).

CHECK t_0030[] IS NOT INITIAL.

SELECT *
  FROM zppt0006
  INTO TABLE @DATA(t_0006)
   FOR ALL ENTRIES IN @t_0030
 WHERE id_referencia2 = @t_0030-id_referencia
   AND id_cotton      = @t_0030-id_cotton.

LOOP AT  t_0030 INTO DATA(w_0030).
  LOOP AT t_0006 INTO DATA(w_0006) WHERE id_referencia2 = w_0030-id_referencia
                                     AND id_cotton      = w_0030-id_cotton.
    w_0006-chave_quebra = w_0030-chave_quebra.
    MODIFY zppt0006  FROM w_0006.
  ENDLOOP.
  COMMIT WORK.
ENDLOOP.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
