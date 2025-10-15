*&---------------------------------------------------------------------*
*& Include ZDRC_TRATAR_MANIFESTO
*&---------------------------------------------------------------------*

FORM f_tratamento_manifesto    USING is_mail_preview TYPE edoc_br_mail_preview
                            CHANGING ev_reason       TYPE edoc_br_reject_code
                                     ev_confirmed    TYPE abap_bool.

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

*  SELECT *
*    FROM zsdt0127
*    INTO TABLE @DATA(t_0127)
*   WHERE chave = @is_mail_preview-edobrincoming-accesskey.
*
**------------------
** recupera o manifesto mais recente
**------------------
*  SORT t_0127 BY doc_manifesto DESCENDING.
*
*  READ TABLE t_0127 INTO DATA(w_127) INDEX 1.

  CHECK sy-subrc = 0.

  ev_reason    = wa_zsdt0127-cd_operacao.
  ev_confirmed = abap_true.

ENDFORM.
