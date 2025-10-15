FUNCTION Z_DOC_CHECK.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SCREEN) TYPE  DYNNR
*"     REFERENCE(I_SHOW) TYPE  TRUE DEFAULT SPACE
*"     REFERENCE(I_REPID) TYPE  REPID
*"     REFERENCE(I_PRESSED_TAB) TYPE  CHAR30 OPTIONAL
*"     REFERENCE(I_SET_FIELD) TYPE  CHAR30 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MESSAGEM) TYPE  CHAR30
*"  TABLES
*"      IT_MSGS STRUCTURE  ZFIWRS0002
*"----------------------------------------------------------------------
  TG_MSGS[] = IT_MSGS[].

  DATA: WL_CAMPO(60),
        WL_CONT TYPE SY-TABIX,
        WL_LINHA(6),
        WG_MENSAGENS(30)  VALUE '@5C@ Messagens'.

  CONCATENATE '(' I_REPID ')' I_PRESSED_TAB INTO WL_CAMPO .
  ASSIGN (WL_CAMPO) TO <FS_ABA>.

  CONCATENATE '(' I_REPID ')'  I_SET_FIELD INTO WL_CAMPO.
  ASSIGN (WL_CAMPO) TO <FS_FIELD>.

  DESCRIBE TABLE IT_MSGS LINES WL_CONT.
  IF WL_CONT GE 1.
    WL_LINHA = WL_CONT.
    CONDENSE WL_LINHA NO-GAPS.
    CONCATENATE '@5C@ Mensagens(' WL_LINHA ')' INTO E_MESSAGEM SEPARATED BY SPACE.
  ELSE.
*    CONCATENATE '@5B@ Não ha erros!' INTO wg_mensagens.
    E_MESSAGEM = '@5B@ Não ha erros!'.
  ENDIF.

  IF I_SHOW IS NOT INITIAL.
    IF IT_MSGS[] IS NOT INITIAL.
      IF OBG_GRID1 IS INITIAL.
        WG_LAYOUT-CWIDTH_OPT = C_X.
        WG_LAYOUT-ZEBRA      = C_X.
*    wg_layout-no_toolbar = c_x.
        WG_LAYOUT-COL_OPT    = C_X.
        WG_STABLE-ROW        = C_X.

        SCREEN = I_SCREEN.
* create the docking container
        CREATE OBJECT OBG_DOCKING
          EXPORTING
            SIDE      = OBG_DOCKING->DOCK_AT_BOTTOM
            EXTENSION = 100.


        CALL METHOD OBG_DOCKING->SET_VISIBLE
          EXPORTING
            VISIBLE = I_SHOW. "C_X.

        CREATE OBJECT OBG_GRID1
          EXPORTING
            I_PARENT = OBG_DOCKING.

        PERFORM MONTAR_LAYOUT.

        IF OBG_GRID1 IS NOT INITIAL.
*      CREATE alv event handler
          CREATE OBJECT OBG_TOOLBAR
            EXPORTING
              IO_ALV_GRID = OBG_GRID1.

*      * Register event handler
          SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR OBG_GRID1.
          SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR OBG_GRID1.

          CALL METHOD OBG_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
            EXPORTING
              IS_LAYOUT       = WG_LAYOUT
            CHANGING
              IT_FIELDCATALOG = TG_FIELDCATALOG[]
              IT_OUTTAB       = IT_MSGS[].



*      SET HANDLER lcl_event_receiver=>handle_user_command
*                  lcl_event_receiver=>handle_menu_button
*                  lcl_event_receiver=>handle_toolbar.
          SET HANDLER:
                       LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK FOR OBG_GRID1.

* Método de atualização de dados na Tela
          CALL METHOD OBG_GRID1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WG_STABLE.

        ENDIF.

      ELSE.
        SCREEN = I_SCREEN.

        CALL METHOD OBG_DOCKING->SET_VISIBLE
          EXPORTING
            VISIBLE = I_SHOW. "C_X.

* Método de atualização de dados na Tela
        CALL METHOD OBG_GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WG_STABLE.


      ENDIF.
    ENDIF.
  else.
    if obg_grid1 is not initial.
* Método de atualização de dados na Tela
        CALL METHOD OBG_GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WG_STABLE.
    endif.
  ENDIF.

ENDFUNCTION.
