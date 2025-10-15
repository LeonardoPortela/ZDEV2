METHOD set_tratar_manifesto .

  DATA: t_callstack TYPE abap_callstack,
        wa_zsdt0127 TYPE zsdt0127.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    EXPORTING
      max_level = 0
    IMPORTING
      callstack = t_callstack.

*------------------
* transacao foi executada pela ZSST0110?
*------------------
  READ TABLE t_callstack INTO DATA(w_callstack) WITH KEY mainprogram = 'ZSDR0059'.

  IF sy-subrc NE 0.
    READ TABLE t_callstack INTO w_callstack WITH KEY mainprogram = 'ZFIS44'.
  ENDIF.

  CHECK sy-subrc = 0.

*-------------------------------------
*-- Importar manifesto p/ memoria (zcl_manifesto_dest)ENVIAR_MANIFESTO
*-------------------------------------
  IMPORT wa_zsdt0127 = wa_zsdt0127 FROM MEMORY ID 'ZSDT0127'.

  CHECK sy-subrc = 0.

  rv_answer = '1'.

ENDMETHOD.
