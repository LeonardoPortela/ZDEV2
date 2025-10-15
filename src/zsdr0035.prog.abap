*&---------------------------------------------------------------------*
*& Report  ZSDR0035
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZSDR0035.

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS P_DOCNUM TYPE J_1BNFDOC-DOCNUM OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  CALL FUNCTION 'Z_SD_PRINT_BOLETO'
    EXPORTING
      DOC_NUMERO     = P_DOCNUM
      TIPO           = 'N'  "Nota
    EXCEPTIONS
      NAO_LOCALIZADO = 1
      OTHERS         = 2.

END-OF-SELECTION.
