"Name: \FU:FCJ_DELETE_ENTRIES\SE:BEGIN\EI
ENHANCEMENT 0 Z_FBCJ_FECHA5.
*
  if vg_display_mens = 'X'.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
  EXPORTING
   TITEL              = 'Lançamentos'
   textline1          = 'Existem documentos sem laçamentos.É necessário realizar'
   TEXTLINE2          = 'os lançamento pendentes antes de sair da transação '
   START_COLUMN       = 25
   START_ROW          = 6.
   clear: vg_display_mens.
   FIELD-SYMBOLS: <fs_cod> type any.
   ASSIGN ('(SAPMFCJ0)OK_CODE') to <fs_cod>.
   if <fs_cod> is ASSIGNED.
     clear: <fs_cod>.
     endif.
    endif.
ENDENHANCEMENT.
