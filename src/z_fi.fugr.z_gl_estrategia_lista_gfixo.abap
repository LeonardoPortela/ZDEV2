FUNCTION Z_GL_ESTRATEGIA_LISTA_GFIXO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) LIKE  SY-UNAME
*"     VALUE(V_LOTE) LIKE  ZGLT034-LOTE OPTIONAL
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_LOTES STRUCTURE  ZELOTES_GFIXO
*"      T_ESTRA STRUCTURE  ZEESTRATEGIA_GFIXO OPTIONAL
*"      T_DOCS STRUCTURE  ZEDOCS_GFIXO OPTIONAL
*"----------------------------------------------------------------------
*{   INSERT         DEVK9A297X                                        1

SELECT
  'I' AS sign,
 'EQ' AS option,
 CAJO_NUMBER AS low,
 CAJO_NUMBER AS high
   FROM ZFIT0120
  WHERE uname = @V_USUARIO
  AND val_ini <= @SY-datum
  AND val_fim >= @SY-datum
  INTO TABLE @DATA(LR_ZFIT0120).

IF SY-SUBRC = 0.
SELECT * FROM ZFIT0217 WHERE werks IN @LR_ZFIT0120
INTO TABLE @DATA(LT_ZFIT0217).
  IF SY-subrc = 0.
    LOOP AT LT_ZFIT0217 ASSIGNING FIELD-SYMBOL(<T_LOTES>).
MOVE-CORRESPONDING <T_LOTES> TO t_lotes.
APPEND t_lotes TO T_LOTES[].
    ENDLOOP.
  ENDIF.
ENDIF.

*}   INSERT
ENDFUNCTION.
