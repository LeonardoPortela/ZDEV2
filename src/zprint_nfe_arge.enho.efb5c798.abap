"Name: \PR:J_1A_WS_EXPORT_MONITOR\FO:SET_STATUS_ICONS\SE:BEGIN\EI
ENHANCEMENT 0 ZPRINT_NFE_ARGE.
*
  LOOP AT it_j1acae_alv INTO wa_j1acae_alv.

    if wa_j1acae_alv-RFC_SENT_LOCK eq ABAP_TRUE.
      wa_j1acae_alv-ZRFC_UNLOCK = ICON_UNLOCKED.
    ENDIF.

    CASE wa_j1acae_alv-cae_status.
      WHEN 'A'.
        wa_j1acae_alv-ZPRINT_NFE_AGR = ICON_PRINT.
      WHEN OTHERS.
        wa_j1acae_alv-ZPRINT_NFE_AGR = ICON_PRINT.
    ENDCASE.
    MODIFY it_j1acae_alv FROM wa_j1acae_alv.
  ENDLOOP.

ENDENHANCEMENT.
