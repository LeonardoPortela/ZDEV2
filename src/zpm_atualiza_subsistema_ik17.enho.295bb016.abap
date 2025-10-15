"Name: \PR:RIIMR020\FO:FILL_OBJECT_TAB_L\SE:END\EI
ENHANCEMENT 0 ZPM_ATUALIZA_SUBSISTEMA_IK17.

IF object_tab[] IS NOT INITIAL.

  SELECT mdocm, zzcodegruppe, zzdesc_sistema, zzcode, zzkurztext
    FROM imrg FOR ALL ENTRIES IN @object_tab
    WHERE mdocm = @object_tab-mdocm
    INTO TABLE @DATA(lt_imrg).

  IF sy-subrc = 0.
    LOOP AT object_tab[] ASSIGNING FIELD-SYMBOL(<object>).
      TRY.
          READ TABLE lt_imrg WITH KEY mdocm = <object>-mdocm INTO DATA(wa_imrg).
        IF sy-subrc = 0.
          <object>-zzcode = wa_imrg-zzcode.
          <object>-zzkurztext = wa_imrg-zzkurztext.

          <object>-zzcodegruppe = wa_imrg-zzcodegruppe.
          <object>-zzdesc_sistema = wa_imrg-zzdesc_sistema.
        ENDIF.
      CATCH cx_root.
      ENDTRY.
    ENDLOOP.
  ENDIF.

ENDIF.
ENDENHANCEMENT.
