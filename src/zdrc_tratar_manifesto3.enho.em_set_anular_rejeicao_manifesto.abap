METHOD set_anular_rejeicao_manifesto .

  DATA: l_chave     TYPE edoc_accesskey,
        ls_event    TYPE zde_ret_event_inb,
        wa_zsdt0127 TYPE zsdt0127.

  FREE: l_chave, wa_zsdt0127.

  SELECT SINGLE *
     FROM edobrincoming
     INTO @DATA(w_edoc)
    WHERE edoc_guid = @i_edoc_guid.

  IF sy-subrc <> 0.
    SELECT SINGLE *
       FROM edobrcteincoming
       INTO @DATA(w_edoccte)
    WHERE edoc_guid = @i_edoc_guid.

    IF sy-subrc = 0.
      l_chave = w_edoccte-accesskey.
    ENDIF.
  ELSE.
    l_chave   = w_edoc-accesskey.
  ENDIF.

  CHECK l_chave IS NOT INITIAL.

*-------------------------------------
*-- Importar manifesto p/ memoria (zcl_manifesto_dest)ENVIAR_MANIFESTO
*-------------------------------------
  IMPORT wa_zsdt0127 = wa_zsdt0127 FROM MEMORY ID 'ZSDT0127'.

  IF wa_zsdt0127 IS INITIAL.
    SELECT *
      FROM zsdt0127
      INTO TABLE @DATA(t_0127)
     WHERE chave            = @l_chave
       AND cd_operacao     IN ('210240','210220' ,'610110')
       AND rejeicao_anulada = @abap_off.

    SORT t_0127 BY doc_manifesto.
    READ TABLE t_0127 INTO wa_zsdt0127 INDEX 1.
  ENDIF.

  CHECK wa_zsdt0127 IS NOT INITIAL.

  ls_event-chave        = wa_zsdt0127-chave.
  ls_event-ext_event    = wa_zsdt0127-cd_operacao.

*-------------------------------------
*-- gravar informacoes manifesto
*-------------------------------------
  CALL FUNCTION 'Z_GRC_RETORNO_EVENT_INB'
    EXPORTING
      i_event           = ls_event
      i_anular_rejeicao = abap_true.

ENDMETHOD.
