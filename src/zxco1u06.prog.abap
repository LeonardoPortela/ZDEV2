*&---------------------------------------------------------------------*
*&  Include           ZXCO1U06
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(HEADER_IMP) LIKE  CAUFVD STRUCTURE  CAUFVD
*"       EXPORTING
*"             VALUE(NO_UPDATE) LIKE  SY-DATAR
*"             VALUE(HEADER_EXP) LIKE  CAUFVD STRUCTURE  CAUFVD
*"----------------------------------------------------------------------

  SELECT COUNT(*)
    FROM setleaf
     WHERE setname EQ 'MAGGI_FERTILIZANTES_PP'
       AND valfrom EQ header_imp-werks.

  CHECK sy-subrc IS INITIAL.

  MOVE header_imp TO header_exp.

  DATA(_additional_data) = NEW zcl_pp_services( )->get_additional_data( ).

  header_exp-gluzs = sy-uzeit.

  IF NOT ( _additional_data IS INITIAL ).
    MOVE-CORRESPONDING _additional_data TO header_exp.
  ENDIF.
