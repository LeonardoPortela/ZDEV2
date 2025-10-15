*----------------------------------------------------------------------*
***INCLUDE LZIM02F01 .
*----------------------------------------------------------------------*
FORM Z_VERIFICA_CENTRO.

  DATA: V_KOSTL TYPE CSKS-KOSTL.
  BREAK ABAP.
  SELECT SINGLE KOSTL
    FROM CSKS
    INTO V_KOSTL
    WHERE KOKRS = 'MAGI'
      AND KOSTL = ZIM02_SOL_AP_CTL-KOSTL.

  IF SY-SUBRC NE 0.

    MESSAGE E010(Z01) WITH ZIM02_SOL_AP_CTL-KOSTL.
*   Centro de Custo &1 não cadastrado na tabela CSKS.(Verificar Entrada)

  ENDIF.


ENDFORM.                    "Z_VERIFICA_CENTRO
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_DESCRICAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATUALIZA_DESCRICAO INPUT.

  IF NOT ZIM02_SOL_AP_CTL-KOSTL IS INITIAL.
    SELECT SINGLE LTEXT FROM CSKT INTO ZIM02_SOL_AP_CTL-LTEXT
      WHERE SPRAS = SY-LANGU AND
            KOKRS IN ('MAGI', 'MGLD') AND
            KOSTL = ZIM02_SOL_AP_CTL-KOSTL.
  ENDIF.

ENDMODULE.                 " ATUALIZA_DESCRICAO  INPUT
