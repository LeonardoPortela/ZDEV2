*&---------------------------------------------------------------------*
*& Report  ZLESR0160
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0160.

DATA: _notifheader   TYPE bapi2080_nothdri,
      _notifheader_x TYPE bapi2080_nothdri_x,
      lv_nota        TYPE bapi2080_nothdre-notif_no,
      lt_return      TYPE TABLE OF bapiret2.

IMPORT _notifheader TO _notifheader FROM MEMORY ID 'CRIAR_NOTAS_HEADER'.
IMPORT _notifheader_x TO _notifheader_x FROM MEMORY ID 'CRIAR_NOTAS_HEADERX'.
IMPORT lv_nota TO lv_nota FROM MEMORY ID 'CRIAR_NOTAS_NOTA'.

IF lv_nota IS NOT INITIAL.

  CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
    EXPORTING
      number        = lv_nota
      notifheader   = _notifheader
      notifheader_x = _notifheader_x
    TABLES
      return        = lt_return.

  IF NOT line_exists( lt_return[ type = 'E' ] ).
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDIF.

ENDIF.
