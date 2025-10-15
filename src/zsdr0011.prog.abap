*&---------------------------------------------------------------------*
*& Report  ZSDR0011
*&
*&---------------------------------------------------------------------*
*&  Programa para job de envio de e-mail de documentos eletrônicos
*&  CT-e/NF-e pendentes de ajuste em monitores
*&---------------------------------------------------------------------*

REPORT  zsdr0011.

CALL FUNCTION 'Z_EMAIL_NFE_CTE_PENDENTES'.
