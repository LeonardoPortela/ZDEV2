*&---------------------------------------------------------------------*
*& Report ZTESTE_FATAUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zteste_fataut.

DATA:  lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  PARAMETERS : p_ch_fat TYPE zid_integracao.
SELECTION-SCREEN END   OF BLOCK b1.

START-OF-SELECTION.

  CREATE OBJECT lc_faturamento_automatico.

  TRY.
      lc_faturamento_automatico->set_iniciar_faturamento( i_ch_faturamento   = p_ch_fat ).

    CATCH zcx_error INTO DATA(ex_error).
  ENDTRY.
