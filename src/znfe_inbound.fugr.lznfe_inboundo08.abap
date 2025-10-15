*----------------------------------------------------------------------*
***INCLUDE LZNFE_INBOUNDO08.
*----------------------------------------------------------------------*

DATA: CK_ALTEROU_VALOR_PRECO_PED TYPE CHAR01.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1504  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1504 OUTPUT.

  SET PF-STATUS 'PF1504'.
  SET TITLEBAR 'TL1504'.

  CLEAR: CK_ALTEROU_VALOR_PRECO_PED.

  ZDE_NFE_DIST_ITM_PRECO_PED-LIQUIDO = ( ( ZDE_NFE_DIST_ITM_PRECO_PED-BRTWR -
                          (
                            ( ( ZDE_NFE_DIST_ITM_PRECO_PED-BRTWR * ( ZDE_NFE_DIST_ITM_PRECO_PED-BICMS   / 100 ) ) * ( ZDE_NFE_DIST_ITM_PRECO_PED-PICMS   / 100 ) )  +
                            ( ( ZDE_NFE_DIST_ITM_PRECO_PED-BRTWR * ( ZDE_NFE_DIST_ITM_PRECO_PED-BPIS    / 100 ) ) * ( ZDE_NFE_DIST_ITM_PRECO_PED-PPIS    / 100 ) )  +
                            ( ( ZDE_NFE_DIST_ITM_PRECO_PED-BRTWR * ( ZDE_NFE_DIST_ITM_PRECO_PED-BCOFINS / 100 ) ) * ( ZDE_NFE_DIST_ITM_PRECO_PED-PCOFINS / 100 ) )
                          )
                        ) ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1504_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1504_EXIT INPUT.
  CLEAR: CK_ALTEROU_VALOR_PRECO_PED.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1504  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1504 INPUT.

  CASE OK_CODE.
    WHEN 'CONFIRMAR'.
      CHECK CK_ALTEROU_VALOR_PRECO_PED IS INITIAL.
      CK_INFORMADO_PRECO = ABAP_TRUE.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_VALOR_PRECO_PED  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_VALOR_PRECO_PED INPUT.
  CK_ALTEROU_VALOR_PRECO_PED = ABAP_TRUE.
ENDMODULE.
