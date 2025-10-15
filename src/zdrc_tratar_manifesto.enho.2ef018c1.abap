"Name: \FU:EDOC_BR_POPUP_REASON_REJ\SE:BEGIN\EI
ENHANCEMENT 0 ZDRC_TRATAR_MANIFESTO.
*
*#127333 - 24.11.2023 - JT - inicio
PERFORM f_tratamento_manifesto    USING is_mail_preview
                               CHANGING ev_reason
                                        ev_confirmed.
IF ev_reason IS NOT INITIAL.
  EXIT.
ENDIF.
*#127333 - 24.11.2023 - JT - fim
*
ENDENHANCEMENT.
