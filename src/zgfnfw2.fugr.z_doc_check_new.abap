FUNCTION z_doc_check_new.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SCREEN) TYPE  DYNNR
*"     REFERENCE(I_SHOW) TYPE  TRUE DEFAULT SPACE
*"     REFERENCE(I_REPID) TYPE  REPID
*"     REFERENCE(I_PRESSED_TAB) TYPE  CHAR30 OPTIONAL
*"     REFERENCE(I_SET_FIELD) TYPE  CHAR30 OPTIONAL
*"     REFERENCE(I_POPUP) TYPE  I DEFAULT 0
*"     REFERENCE(I_SET_CELL) TYPE  CHAR30 OPTIONAL
*"     REFERENCE(I_SET_OBJ) TYPE  CHAR30 OPTIONAL
*"     REFERENCE(I_INFO) TYPE  C DEFAULT ABAP_FALSE
*"  EXPORTING
*"     REFERENCE(E_MESSAGEM) TYPE  CHAR30
*"     REFERENCE(E_DB_CLICK) TYPE  ZFIWRS0002
*"  TABLES
*"      IT_MSGS STRUCTURE  ZFIWRS0002
*"----------------------------------------------------------------------
  tg_msgs[] = it_msgs[].
  v_info    = i_info.

  DATA: wl_campo(60),
        wl_cont          TYPE sy-tabix,
        wl_linha(6),
        wg_mensagens(30) VALUE '@5C@ Messagens'.

  CONCATENATE '(' i_repid ')' i_pressed_tab INTO wl_campo .
  ASSIGN (wl_campo) TO <fs_aba>.

  CONCATENATE '(' i_repid ')'  i_set_field INTO wl_campo.
  ASSIGN (wl_campo) TO <fs_field>.

  CONCATENATE '(' i_repid ')'  i_set_cell INTO wl_campo.
  ASSIGN (wl_campo) TO <fs_set_cell>.

  CONCATENATE '(' i_repid ')'  i_set_obj INTO wl_campo.
  ASSIGN (wl_campo) TO <fs_set_obj>.

  DESCRIBE TABLE it_msgs LINES wl_cont.
  IF wl_cont GE 1.
    wl_linha = wl_cont.
    CONDENSE wl_linha NO-GAPS.
*    CONCATENATE '@5C@ Mensagens(' wl_linha ')' INTO e_messagem SEPARATED BY space.
    CONCATENATE icon_led_red text-b02  '(' wl_linha ')'  INTO e_messagem SEPARATED BY space.
  ELSE.
*    CONCATENATE '@5B@ Não ha erros!' INTO wg_mensagens.
*    e_messagem = '@5B@ Não ha erros!'.
    e_messagem = text-b01.
    CONCATENATE icon_led_green  text-b01 INTO e_messagem.
  ENDIF.

  IF i_show IS NOT INITIAL.
    IF it_msgs[] IS NOT INITIAL.
      IF obg_grid1 IS INITIAL.
        wg_layout-cwidth_opt = c_x.
        wg_layout-zebra      = c_x.
*    wg_layout-no_toolbar = c_x.
        wg_layout-col_opt    = c_x.
        wg_stable-row        = c_x.

        screen = i_screen.

        IF i_popup EQ 0.
* create the docking container
          CREATE OBJECT obg_docking
            EXPORTING
              side      = obg_docking->dock_at_bottom
              extension = 100.

          CALL METHOD obg_docking->set_visible
            EXPORTING
              visible = i_show. "C_X.

          CREATE OBJECT obg_grid1
            EXPORTING
              i_parent = obg_docking.

        ELSEIF i_popup EQ 1.
* create the docking container
          CREATE OBJECT obg_dialogbox
            EXPORTING
*             STYLE  = obg_dialogbox->ADUST_DESIGN_TRUE
              width  = 540
              height = 100
              top    = 150
              left   = 150.

          SET HANDLER:
                   lcl_event_handler=>handle_close_box FOR obg_dialogbox.

          CALL METHOD obg_dialogbox->set_visible
            EXPORTING
              visible = i_show. "C_X.

          CREATE OBJECT obg_grid1
            EXPORTING
              i_parent = obg_dialogbox.
        ENDIF.
        PERFORM montar_layout.

        IF obg_grid1 IS NOT INITIAL.
*      CREATE alv event handler
          CREATE OBJECT obg_toolbar
            EXPORTING
              io_alv_grid = obg_grid1.

*      * Register event handler
          SET HANDLER obg_toolbar->on_toolbar FOR obg_grid1.
          SET HANDLER obg_toolbar->handle_user_command FOR obg_grid1.

          CALL METHOD obg_grid1->set_table_for_first_display
            EXPORTING
              is_layout       = wg_layout
            CHANGING
              it_fieldcatalog = tg_fieldcatalog[]
              it_outtab       = it_msgs[].



*      SET HANDLER lcl_event_receiver=>handle_user_command
*                  lcl_event_receiver=>handle_menu_button
*                  lcl_event_receiver=>handle_toolbar.
          SET HANDLER:
                       lcl_event_handler=>on_double_click FOR obg_grid1.

* Método de atualização de dados na Tela
          CALL METHOD obg_grid1->refresh_table_display
            EXPORTING
              is_stable = wg_stable.

        ENDIF.

      ELSE.
        screen = i_screen.
        IF i_popup EQ 0.
          CALL METHOD obg_docking->set_visible
            EXPORTING
              visible = i_show. "C_X.

        ELSEIF i_popup EQ 1.
          CALL METHOD obg_dialogbox->set_visible
            EXPORTING
              visible = i_show. "C_X.
        ENDIF.

* Método de atualização de dados na Tela
        CALL METHOD obg_grid1->refresh_table_display
          EXPORTING
            is_stable = wg_stable.


      ENDIF.
    ENDIF.
  ELSE.
    IF obg_grid1 IS NOT INITIAL.
* Método de atualização de dados na Tela
      CALL METHOD obg_grid1->refresh_table_display
        EXPORTING
          is_stable = wg_stable.
    ENDIF.
  ENDIF.

ENDFUNCTION.
