*&---------------------------------------------------------------------*
*& Report  ZPM_DISPARA_API_ORDEM_MOBMAN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zpm_dispara_api_ordem_mobman.

DATA: lt_header    TYPE zcaufvdb,
      lt_operation TYPE zafvgb.



IMPORT lt_operation TO lt_operation FROM MEMORY ID 'ZPMORDEM_OP'.
IMPORT lt_header    TO lt_header FROM MEMORY ID 'ZPMORDEM_HEAD'.

CALL FUNCTION 'ZPM_DISPARA_API_ORDEM'
  EXPORTING
    header    = lt_header
    operation = lt_operation.
