*----------------------------------------------------------------------*
***INCLUDE LZIM02F02 .
*----------------------------------------------------------------------*

FORM ZF_CADASTRA_USUARIO.

  ZIM02_SOL_AP_CTL-RESPONSAVEL = SY-UNAME.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_DESCRICAO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATUALIZA_DESCRICAO OUTPUT.
  IF NOT ZIM02_SOL_AP_CTL-KOSTL IS INITIAL.
    SELECT SINGLE LTEXT FROM CSKT INTO ZIM02_SOL_AP_CTL-LTEXT
      WHERE SPRAS = SY-LANGU AND
            KOKRS IN ('MAGI', 'MGLD') AND
            KOSTL = ZIM02_SOL_AP_CTL-KOSTL.
  ENDIF.
ENDMODULE.                 " ATUALIZA_DESCRICAO  OUTPUT
