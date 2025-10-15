*----------------------------------------------------------------------*
***INCLUDE LZMM_NFSEO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  TC_COND_LIST_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
MODULE tc_cond_list_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_scr_condition_list LINES tc_condition_list-lines.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2000 OUTPUT.

  PERFORM f_set_status_2000.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_3000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_3000 OUTPUT.
  SET PF-STATUS '3000'.
  SET TITLEBAR '3000'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC_POPUP_LIST_ASSIGN_ATTR  OUTPUT
*&---------------------------------------------------------------------*
MODULE tc_popup_list_assign_attr OUTPUT.

  "PERFORM f_po_associados.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_CREATE_ALVS  OUTPUT
*&---------------------------------------------------------------------*
MODULE m_create_alvs OUTPUT.

  PERFORM f_create_alv_pedidos.
  PERFORM f_create_alv_associados.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC_POPUP_LIST_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_popup_list_get_lines OUTPUT.
  gv_tc_popup_list_lines = sy-loopc.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC_POPUP_LIST_SET_INPUT_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_popup_list_set_input_field OUTPUT.

*  IF gw_scr_popup_list-checkbox EQ 'X'.
*    IF gw_scr_popup_list-menge_iv EQ 0.
*      SET CURSOR FIELD 'GW_SCR_POPUP_LIST-MENGE_IV' LINE tc_popup_list-current_line.
*    ENDIF.
*    LOOP AT SCREEN.
*      IF screen-name = 'GW_SCR_POPUP_LIST-MENGE_IV'.
*        screen-input = 1.
*        MODIFY SCREEN.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  IF gw_scr_popup_list-pstyp NE '9'.

    LOOP AT SCREEN.

      IF screen-name = 'GW_SCR_POPUP_LIST-MENGE_IV'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'BTN_SERV'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.


  ELSE.

    LOOP AT SCREEN.

      IF screen-name = 'GW_SCR_POPUP_LIST-MENGE_IV'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_3010  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_3010 OUTPUT.

  SET PF-STATUS '3010'.
  SET TITLEBAR '3010'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_INPUT_FIELD  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_input_field OUTPUT.

  LOOP AT SCREEN.

    IF   screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-NFSE_NUMERO'
      OR screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-NFSE_SERIE'
      OR screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-DTEMISSAO'
      OR screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-NFSE_VALUE'
      OR screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-MWSKZ'
      OR screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-BUKRS'
      OR screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-LIFNR'
      OR screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-NAME1'
      OR screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-STCD3'
      OR screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-STCD2'
      OR screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-STCD1'.

      IF gv_edit_2000 = 'X' OR gv_edit_2000 = 'C'. " <- 'C' - Cabeçalho
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

  LOOP AT SCREEN.

    IF screen-name = 'ZSPAYMENT_DATA_NFSE_INBOUND-ZLSPR'
      OR screen-name = 'ZSPAYMENT_DATA_NFSE_INBOUND-DT_VENCIMENTO'
      OR screen-name = 'ZSPAYMENT_DATA_NFSE_INBOUND-BOLETO'
      OR screen-name = 'ZSPAYMENT_DATA_NFSE_INBOUND-OBS_FINANCEIRA'
      OR screen-name = 'ZSPAYMENT_DATA_NFSE_INBOUND-BVTYP'
*      OR screen-name = 'ZSPAYMENT_DATA_NFSE_INBOUND-WAERS'
      OR screen-name = 'ZSPAYMENT_DATA_NFSE_INBOUND-KTWRT'
      OR screen-name = 'ZSPAYMENT_DATA_NFSE_INBOUND-BANKN'
      OR screen-name = 'ZSPAYMENT_DATA_NFSE_INBOUND-BANKL'
      OR screen-name = 'ZSPAYMENT_DATA_NFSE_INBOUND-CK_FPOL'. "US 170323 - MMSILVA - 02.05.2025

      IF gv_edit_2000 = 'X' OR gv_edit_2000 = 'P'. "<- 'P' - Pagamento
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

  " Regras exclusivas por campo

  LOOP AT SCREEN.

    IF screen-name = 'ZSHEADER_DATA_NFSE_INBOUND-RETER_ISS'.

      IF zibs_nfse_001-belnr_fret IS INITIAL.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FILL_TOTALS  OUTPUT
*&---------------------------------------------------------------------*
MODULE fill_totals OUTPUT.

*  PERFORM f_fill_totals_3000.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_SH_BVTYP  INPUT
*&---------------------------------------------------------------------*
MODULE zm_sh_bvtyp INPUT.

  DATA lw_payment TYPE  zspayment_data_nfse_inbound.
  DATA lv_show TYPE flag.

  CHECK zsheader_data_nfse_inbound-lifnr IS NOT INITIAL.

  IF gv_edit_2000 IS NOT INITIAL.
    lv_show = space.
  ELSE.
    lv_show = 'X'.
  ENDIF.

  CALL FUNCTION 'FI_F4_BVTYP'
    EXPORTING
      i_lifnr        = zsheader_data_nfse_inbound-lifnr
      i_xshow        = lv_show
    IMPORTING
      e_bvtyp        = zspayment_data_nfse_inbound-bvtyp
    EXCEPTIONS
      no_bvtyp_found = 1
      invalid_call   = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
  ENDIF.

  PERFORM f_dados_banco_tela
    USING zsheader_data_nfse_inbound
 CHANGING zspayment_data_nfse_inbound.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_4000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_4000 OUTPUT.
  SET PF-STATUS '4000'.
  SET TITLEBAR '4000'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALV_4000_INIT  OUTPUT
*&---------------------------------------------------------------------*
MODULE alv_4000_init OUTPUT.

  PERFORM f_alv_4000_init_01.

  PERFORM f_alv_4000_init_02.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_RICHTEXT  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_richtext OUTPUT.

  DATA lv_c TYPE c LENGTH 255.

  DATA lt_char LIKE TABLE OF lv_c.

  IF go_cc_2000_text IS NOT BOUND.

    CREATE OBJECT go_cc_2000_text
      EXPORTING
        container_name = 'CC_RICHTEXT'
        repid          = sy-repid
        dynnr          = sy-dynnr.

  ENDIF.

  IF go_text_2000 IS NOT BOUND.

    CREATE OBJECT go_text_2000
      EXPORTING
        wordwrap_mode     = 1
        wordwrap_position = 254
        parent            = go_cc_2000_text.

    "go_text_2000->set_enable( abap_false ).
    go_text_2000->set_readonly_mode( 1 ).

    go_text_2000->set_toolbar_mode( '0' ).
    "go_text_2000->set_wordwrap

    LOOP AT gt_2000_text ASSIGNING FIELD-SYMBOL(<fs_text>).

      lv_c = <fs_text>-cvalue.

      APPEND lv_c TO lt_char.

    ENDLOOP.

    CALL METHOD go_text_2000->set_text_as_r3table
      EXPORTING
        table           = lt_char
      EXCEPTIONS
        error_dp        = 1
        error_dp_create = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9070  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9070 OUTPUT.

  SET PF-STATUS 'SCREEN_9070'.

  REFRESH: gt_cancr_text[].

  IF go_editor_popup IS INITIAL.

    CREATE OBJECT go_container_popup
      EXPORTING
        container_name              = 'CC_CANCR_TEXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT go_editor_popup
      EXPORTING
        parent                     = go_container_popup
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    IF gv_9070_readonly = 'X'.

      LOOP AT SCREEN.

        IF screen-name = '/TCSR/T_CANCRT-CANCREASON'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

      CALL METHOD go_editor_popup->set_readonly_mode
        EXPORTING
          readonly_mode = cl_gui_textedit=>true.

      /tcsr/t_cancrt-cancreason = gv_9070_motivo.
      "gv_9070_descr

      PERFORM f_preenche_canc_reason.

      DATA lv_stream TYPE string.

      lv_stream = gv_9070_descr.

      CALL METHOD go_editor_popup->set_textstream
        EXPORTING
          text = lv_stream.

    ENDIF.


  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
MODULE screen_control OUTPUT.

  LOOP AT SCREEN.

    IF screen-name = 'BTN_ASSIGN'.

      IF gv_desa_assign = 'X'.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.

    IF screen-name = 'BT_EDIT_02'
    OR screen-name = 'BT_SAVE_02'
    OR screen-name = 'BTN_ASSIGN_02'.

      IF gv_desa_btn_02 = 'X'.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.



  ENDLOOP.


ENDMODULE.

*** Inicio - ALX
*&---------------------------------------------------------------------*
*&      Module  SET_ISS  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_iss_2000 OUTPUT.

*  SELECT SINGLE retem_iss
*    FROM zibt_nfse_001
*    INTO @DATA(lv_iss)
*    WHERE nfse_numero = @zsheader_data_nfse_inbound-nfse_numero.
*
*  IF lv_iss = abap_true.
*    gv_iss_2000 = abap_false.
*  ELSE.
*    gv_iss_2000 = abap_true.
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_ISS_4000  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_iss_4000 OUTPUT.

  PERFORM f_atualiza_iss_4000.

ENDMODULE.
*** Fim - ALX
