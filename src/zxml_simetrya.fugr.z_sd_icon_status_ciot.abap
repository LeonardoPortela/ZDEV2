FUNCTION Z_SD_ICON_STATUS_CIOT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_ST_CIOT) TYPE  ZST_CIOT
*"  EXPORTING
*"     VALUE(ICONE) TYPE  CHAR04
*"  CHANGING
*"     VALUE(TEXTO) TYPE  CHAR_60 OPTIONAL
*"----------------------------------------------------------------------

  CASE P_ST_CIOT.
    WHEN '0'.
      "Pendente
      ICONE = ICON_WARNING.
      TEXTO = 'Pendente'.
    WHEN '1'.
      "Enviado
      ICONE = ICON_IMPORT_TRANSPORT_REQUEST.
      TEXTO = 'Enviado'.
    WHEN '2'.
      "Autorizado
      ICONE = ICON_IMPORT_ALL_REQUESTS.
      TEXTO = 'Autorizado'.
    WHEN '3'.
      "Rejeitado
      ICONE = ICON_DEFECT.
      TEXTO = 'Rejeitado'.
    WHEN '4'.
      "Enviado Aut. Credito
      ICONE = ICON_ACTIVITY.
      TEXTO = 'Enviado Aut. Credito'.
    WHEN '5'.
      "Creditado
      ICONE = ICON_RELEASE.
      TEXTO = 'Creditado'.
    WHEN '6'.
      "Fechado (Pago Cockpit)
      ICONE = ICON_COMPLETE.
      TEXTO = 'Fechado (Pago Cockpit)'.
    WHEN '7'.
      "Enviado Cancelamento
      ICONE = ICON_ACTIVITY.
      TEXTO = 'Enviado Cancelamento'.
    WHEN '8'.
      "Cancelado
      ICONE = ICON_COMPLETE.
      TEXTO = 'Cancelado'.
    WHEN '9'.
      "Sem TipFrete
      ICONE = ICON_MESSAGE_OUTOFDATE.
      TEXTO = 'Sem TipFrete'.
  ENDCASE.


ENDFUNCTION.
