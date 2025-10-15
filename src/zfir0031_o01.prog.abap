*----------------------------------------------------------------------*
***INCLUDE ZFIR0031_O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F_INICIAR_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_iniciar_tela OUTPUT.

  IF wg_acao IS INITIAL.
    REFRESH: tg_fields.

    wg_cadlan-icon = icon_message_warning.
    PERFORM f_trata_campos USING  space
                                  'GR2'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM f_trata_campos USING  space
                                  'GR3'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0

    PERFORM f_trata_campos USING  space
                                  'GR1'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0

*    PERFORM F_TRATA_CAMPOS USING  SPACE
*                                  'GR4'
*                                  C_0       "INPUT 1     NO INPUT 0
*                                  C_0.      "INVISIBLE 1 VISIBLE 0

  ENDIF.

*  IF WG_CADLAN-ORIG_PGT NE 'E'.
*  PERFORM F_TRATA_CAMPOS USING  SPACE
*                                  'GR4'
*                                  C_1       "INPUT 1     NO INPUT 0
*                                  C_1.      "INVISIBLE 1 VISIBLE 0
*
*
*    PERFORM F_TRATA_CAMPOS USING  SPACE
*                                  'GR4'
*                                  C_1       "INPUT 1     NO INPUT 0
*                                  C_0.      "INVISIBLE 1 VISIBLE 0



*  IF WG_ACAO EQ 'DISPLA'.
*    ZLC_ZFIR0031=>HAB_INPUT( I_ACAO =  WG_ACAO ).
*  ENDIF.

*  ENDIF.
  "LISTBOX
  IF list[] IS INITIAL.
    name = 'WG_CADLAN-ORIG_PGT'.
    value-key  = 'B'.
    value-text = 'Brasil'.
    APPEND value TO list.

    value-key  = 'E'.
    value-text = 'Exterior'.
    APPEND value TO list.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = list.

    REFRESH list.

    name = 'WG_CADLAN-FORM_PGT'.
    value-key  = 'C'.
    value-text = 'Fech. Câmbio'.
    APPEND value TO list.

    value-key  = 'T'.
    value-text = 'Transf. Bancária'.
    APPEND value TO list.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = list.
  ENDIF.


  IF list_oper IS INITIAL.
    name_oper = 'WG_CADLAN-TP_OPER'.
    value_oper-key  = '01'.
    value_oper-text = 'Compra Importação'.
    APPEND value_oper TO list_oper.

    value_oper-key  = '02'.
    value_oper-text = 'Compra Serviço'.
    APPEND value_oper TO list_oper.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name_oper
        values = list_oper.

    REFRESH list_oper.
  ENDIF.

ENDMODULE.                 " F_INICIAR_TELA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE trata_fields OUTPUT.
  LOOP AT tg_fields.
    LOOP AT SCREEN.
      IF    screen-name   EQ tg_fields-campo
        OR  screen-group1 EQ tg_fields-group1.

        screen-input     = tg_fields-value.
        screen-invisible = tg_fields-invisible.
        MODIFY SCREEN.
        IF    screen-name   EQ 'WG_CADLAN-FORM_PGT'.
          IF vg_habilitar = 'S'.
            screen-input     = 1.
            screen-invisible = 0.
          ELSE.
            screen-input     = 0.
            screen-invisible = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
      IF  screen-name   EQ 'WG_CADLAN-HBKID'.
        IF wg_cadlan-orig_pgt = 'E' AND
          wg_cadlan-form_pgt  = 'T' AND
          wg_cadlan-tp_oper   = '01'.
          CLEAR wg_cadlan-hbkid.
          screen-invisible = 0.
          screen-input = 0.
        ELSE.
*          screen-invisible = 0.
*          screen-input = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF  ( screen-name   EQ 'WG_CADLAN-NRO_SOL_CP' OR
            screen-name   EQ 'TXTSOLPD' )           AND
            sy-calld      IS INITIAL.
        IF wg_cadlan-nro_sol_cp IS INITIAL.
          screen-invisible = 1.
          screen-input = 0.
        ELSE.
          screen-invisible = 0.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  IF x_field IS NOT INITIAL.
    SET CURSOR FIELD x_field.
  ENDIF.

  zlc_zfir0031=>hab_tip_oper( ).
  zlc_zfir0031=>check_pstyp_pedido( i_ebeln =  wg_cadlan-ebeln ).

ENDMODULE.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: event       TYPE cntl_simple_event,
        events      TYPE cntl_simple_events,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function  WITH HEADER LINE,
        lt_f4       TYPE lvc_t_f4     WITH HEADER LINE.

  DATA: waref TYPE REF TO data.
  DATA: wa_obj  TYPE borident,
        ip_mode TYPE sgs_rwmod.

  "workflow documentos
  IF manager IS NOT INITIAL.
    CALL METHOD manager->unpublish.
    CLEAR: manager.
  ENDIF.


  IF it_seq_lcto[] IS NOT INITIAL.
    READ TABLE it_seq_lcto INDEX 1.
    wa_obj-objtype = 'ZFIR0031'.
    CONCATENATE sy-mandt it_seq_lcto-seq_lcto INTO wa_obj-objkey.

    IF gf_authorization_ft_09 EQ abap_true.
      ip_mode = 'E'.
    ELSE.
      ip_mode = 'D'.
    ENDIF.

    CREATE OBJECT manager
      EXPORTING
        is_object        = wa_obj
        ip_no_commit     = 'R'
        ip_mode          = ip_mode
      EXCEPTIONS
        object_invalid   = 1
        callback_invalid = 2
        OTHERS           = 3.

    SET TITLEBAR 'TLWORK' WITH wg_cadlan-nro_sol.

  ELSE.
*    IF SY-TCODE EQ C_DISPLAY.
*      SET PF-STATUS 'Z002' EXCLUDING FCODE.
*    ELSE.
*      SET PF-STATUS 'Z001' EXCLUDING FCODE.
*    ENDIF.
  ENDIF.

  IF g_custom_container IS INITIAL.
    wa_layout-cwidth_opt  = c_x.
    wa_layout-zebra       = c_x.
    wa_layout-no_toolbar  = c_x.
    wa_layout-no_rowmark  = c_x.
    wa_stable-row         = c_x.
    wa_layout-sel_mode    = 'A'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container_1.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

** Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command FOR grid1.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    FREE: tg_fieldcatalog.
    PERFORM f_montar_layout USING space.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = tg_fieldcatalog[]
        it_outtab            = tg_itens[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
      lcl_event_handler=>on_data_changed_finished FOR grid1,
      lcl_event_handler=>on_data_changed          FOR grid1.


*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ELSE.
    IF wg_acao = c_modif.
      PERFORM f_montar_layout USING c_x.
    ELSE.
      PERFORM f_montar_layout USING space.
    ENDIF.

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = tg_fieldcatalog[].

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.

  REFRESH: fcode.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = tl_parametros
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
  ENDIF.

  READ TABLE tl_parametros INTO wl_parametros
   WITH KEY parid = 'ZADTO_INSUMOS'.
  IF sy-subrc NE 0.
    APPEND 'LIB_INS' TO fcode.
  ENDIF.

  IF wg_acao IS INITIAL.
    APPEND c_save   TO fcode.
    APPEND c_deldoc TO fcode.
*  ELSEIF WG_ZGLT035-DOC_LCTO IS INITIAL.
*    APPEND C_DELDOC TO FCODE.
  ENDIF.

  SET PF-STATUS 'Z001' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR 'Z001'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*& Module INIT_POPUP OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_popup OUTPUT.

  PERFORM init_container.
*  PERFORM init_alv_popup.
*  PERFORM build_fcat.
*  PERFORM show_popup_data.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0120 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0120 OUTPUT.
 SET PF-STATUS 'Z002'.
ENDMODULE.
