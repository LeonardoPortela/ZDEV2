*----------------------------------------------------------------------*
***INCLUDE LZLEST0036I02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZVERIFICA_INICIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zverifica_inicial INPUT.

  "Implementar Validação Incial
  data : LT_ZLEST0036 TYPE TABLE OF ZLEST0036.


SELECT *
  INTO TABLE LT_ZLEST0036
  FROM ZLEST0036
  WHERE TRANSPORTADORA = ZLEST0036-TRANSPORTADORA
    AND branch         = zlest0036-branch
    AND NR_CF_INICIAL <= ZLEST0036-NR_CF_INICIAL
    AND NR_CF_FINAL   >= ZLEST0036-NR_CF_INICIAL
    AND NRPARAMETRO   <> ZLEST0036-NRPARAMETRO.

   if LT_ZLEST0036 is not initial.
     message e000(z01) with 'Existem cartas fretes parametrizadas nesse Intervalo!!'.
   ENDIF.

  IF ZLEST0036-NR_CF_FINAL IS NOT INITIAL .
    SELECT *
      INTO TABLE LT_ZLEST0036
      FROM ZLEST0036
      WHERE TRANSPORTADORA  = ZLEST0036-TRANSPORTADORA
        AND branch          = zlest0036-branch
        AND NR_CF_INICIAL  >= ZLEST0036-NR_CF_INICIAL
        AND NR_CF_FINAL    <= ZLEST0036-NR_CF_FINAL
        AND NRPARAMETRO    <> ZLEST0036-NRPARAMETRO.


    if LT_ZLEST0036 is not initial.
      message e000(z01) with 'Existem cartas fretes parametrizadas nesse Intervalo!!'.
    ENDIF.

  ENDIF.

*
*  SELECT MIN( NR_CF_INICIAL )
*    INTO NR_INICIAL
*    FROM ZLEST0036
*    WHERE TRANSPORTADORA = ZLEST0036-TRANSPORTADORA.
*
*  SELECT MAX( NR_CF_FINAL )
*    INTO  NR_FINAL
*    FROM ZLEST0036
*    WHERE TRANSPORTADORA = ZLEST0036-TRANSPORTADORA.
*
*
*    IF  ZLEST0036-NR_CF_INICIAL >= NR_INICIAL AND ZLEST0036-NR_CF_INICIAL <= NR_FINAL .
*      message e000(z01) with 'Existem cartas fretes parametrizadas nesse Intervalo!!'.
*    ENDIF.




ENDMODULE.                 " ZVERIFICA_INICIAL  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZVERIFICA_FINAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zverifica_final INPUT.


  SELECT *
    INTO TABLE LT_ZLEST0036
    FROM ZLEST0036
    WHERE TRANSPORTADORA  = ZLEST0036-TRANSPORTADORA
      AND branch          = zlest0036-branch
      AND NR_CF_INICIAL  <= ZLEST0036-NR_CF_FINAL
      AND NR_CF_FINAL    >= ZLEST0036-NR_CF_FINAL
      AND NRPARAMETRO    <> ZLEST0036-NRPARAMETRO .

   if LT_ZLEST0036 is not initial.
     message e000(z01) with 'Existem cartas fretes parametrizadas nesse Intervalo!!'.
   ENDIF.


  SELECT *
    INTO TABLE LT_ZLEST0036
    FROM ZLEST0036
    WHERE TRANSPORTADORA  = ZLEST0036-TRANSPORTADORA
      AND branch          = zlest0036-branch
      AND NR_CF_INICIAL  >= ZLEST0036-NR_CF_INICIAL
      AND NR_CF_FINAL    <= ZLEST0036-NR_CF_FINAL
      AND NRPARAMETRO    <> ZLEST0036-NRPARAMETRO .

   if LT_ZLEST0036 is not initial.
     message e000(z01) with 'Existem cartas fretes parametrizadas nesse Intervalo!!'.
   ENDIF.

*  SELECT *
*    INTO TABLE LT_ZLEST0036
*    FROM ZLEST0036
*    WHERE TRANSPORTADORA  = ZLEST0036-TRANSPORTADORA
*      AND NR_CF_INICIAL  <= ZLEST0036-NR_CF_FINAL
*      AND NR_CF_FINAL    >= ZLEST0036-NR_CF_FINAL.
*
*    IF ZLEST0036-NR_CF_INICIAL >= NR_CF_FINAL AND ZLEST0036-NR_CF_FINAL <= NR_CF_FINAL.
*      message e000(z01) with 'Existem cartas fretes parametrizadas nesse Intervalo!!'.
*    ENDIF.


ENDMODULE.                 " ZVERIFICA_FINAL  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZGERANUMERO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zgeranumero INPUT.

  CHECK zlest0036-nrparametro IS INITIAL.

  DATA: vn_parametro TYPE zlest0036-nrparametro.

  SELECT MAX( nrparametro )
    INTO vn_parametro
    FROM zlest0036.

  IF vn_parametro IS INITIAL.
    vn_parametro = 1.
  ELSE.
    vn_parametro = vn_parametro + 1.
  ENDIF.

  zlest0036-dt_lancamento = sy-datum.
  zlest0036-nrparametro   = vn_parametro.

ENDMODULE.                 " ZGERANUMERO  INPUT

*&---------------------------------------------------------------------*
*&      Module  TRAVACAMPOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE travacampos OUTPUT.

  LOOP AT SCREEN.

    IF ( screen-name = 'ZLEST0036-NRPARAMETRO' ) OR ( screen-name = 'ZLEST0036-DT_LANCAMENTO' ).
      screen-output = '1'.
      screen-input  = '0'.
      MODIFY SCREEN.
    ENDIF.

*    IF  zlest0036-nrparametro IS NOT INITIAL.
*      screen-output = '1'.
*      screen-input  = '0'.
*      MODIFY SCREEN.
*    ENDIF.

  ENDLOOP.

ENDMODULE.                 " TRAVACAMPOS  OUTPUT
