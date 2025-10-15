*&---------------------------------------------------------------------*
*& Report  Z_DESTINACAO_MERCADORIA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT Z_DESTINACAO_MERCADORIA.

FORM RETORNA_TEXTO USING P_DOCNUM TYPE J_1BDOCNUM CHANGING P_TEXTO TYPE STRING.

  TRY .
      P_TEXTO = ZCL_MATERIAL_DESTINACAO=>ZIF_MATERIAL_DESTINACAO~GET_TEXTO_NOTA_FISCAL_DOCNUM( I_DOCNUM = P_DOCNUM ).
    CATCH ZCX_MATERIAL_DESTINACAO.    "
  ENDTRY.

ENDFORM.
