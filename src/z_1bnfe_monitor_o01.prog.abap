*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_O01
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.

  PERFORM set_screen_status_100.

  CASE sy-tcode.
    WHEN 'ZNFE'.
      SET TITLEBAR 'TITLE_100'.
    WHEN 'ZNFE_TERC'.
      SET TITLEBAR 'TITLE_101'.
    WHEN 'ZCTE'.
      SET TITLEBAR 'TITLE_100C'.
    WHEN 'ZCTE_TERC'.
      SET TITLEBAR 'TITLE_101C'.
    WHEN 'ZMDFE'.
      SET TITLEBAR 'TITLE_107'.
  ENDCASE.

ENDMODULE.                 " status_0100  OUTPUT
*&---------------------------------------------------------------------*
*& Module ZM_STATUS_0110 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE zm_status_0110 OUTPUT.

  DATA vl_autor TYPE c.

  IF NOT eg_guia_agro IS INITIAL.
    APPEND eg_guia_agro TO tg_guia_agro.
* Verifica se a NF da Guia Agropecuária está autorizada.
    PERFORM zf_nfe_guia_agro_autor TABLES   tg_guia_agro
                                   CHANGING vl_autor.

  ENDIF.

  IF NOT vl_autor IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'VLR'.
        screen-input = 0.
        MODIFY SCREEN.

      ENDIF.

    ENDLOOP.

  ENDIF.

  CLEAR: vl_autor, tg_guia_agro.

  SET PF-STATUS 'ZFI_POPUP_GUIA'.
  SET TITLEBAR 'TITLE_110'.

ENDMODULE.
