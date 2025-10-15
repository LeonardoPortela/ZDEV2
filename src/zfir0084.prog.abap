*&---------------------------------------------------------------------*
*& Report  ZFIR0084
*&
*&---------------------------------------------------------------------*
* Programa..: ZFIR0084                                                 *
* Tipo......: Report                                                   *
* Transação.: ZFIS54                                                   *
* Descrição.: Parâmetros – Liberação Usuário J1BTAX                    *
*             Liberar transação J1BTAX para usuário que tenham         *
*             permissão cadastrado na tabela Z                         *
* Autor.....: Sara Oikawa                                              *
* Data......: 28.08.2020                                               *
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data       | Change     | Autor        | Alteração                   *
*----------------------------------------------------------------------*
* 28.08.20   |DEVK9A0MWX  |Sara Oikawa   | Codificação Inicial         *
*----------------------------------------------------------------------*
REPORT zfir0084.

*&---------------------------------------------------------------------*
*& Tabelas Transparentes                                               *
*&---------------------------------------------------------------------*
TABLES zmmt0131.     "Tabela de Parâmetros – Cadastro J1BTAX


*&---------------------------------------------------------------------*
*& Declaração de Tipos                                                 *
*&---------------------------------------------------------------------*
TYPES: BEGIN OF  ty_saida,
         usuario_lib TYPE zmmt0131-usuario_lib,
         werks       TYPE zmmt0131-werks,
         nao_chave   TYPE zmmt0131-nao_chave,
         usnam       TYPE zmmt0131-usnam,
         zdt_atual   TYPE zmmt0131-zdt_atual,
         zhr_atual   TYPE zmmt0131-zhr_atual,
         celltab     TYPE lvc_t_styl,
       END OF ty_saida.

*&---------------------------------------------------------------------*
*& Declaração de Tabelas Internas / Estruturas                         *
*&---------------------------------------------------------------------*
DATA: it_saida TYPE TABLE OF ty_saida,
      wa_saida TYPE ty_saida.

DATA: it_texto TYPE STANDARD TABLE OF tline,
      wa_texto TYPE tline,
      tl_texto TYPE catsxt_longtext_itab,
      wl_texto TYPE LINE OF catsxt_longtext_itab,
      wl_name  TYPE thead-tdname.

DATA: tl_tlines TYPE TABLE OF tline,
      wl_tlines TYPE tline.

DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      g_grid             TYPE REF TO cl_gui_alv_grid,
      it_fieldcat        TYPE lvc_t_fcat,
      wa_fieldcat        TYPE lvc_s_fcat,
      tl_function        TYPE ui_functions,
      wl_function        LIKE tl_function  WITH HEADER LINE,
      wa_layout          TYPE lvc_s_layo,
      wa_variant         TYPE disvariant,
      wa_estilo          TYPE lvc_t_styl,
      wa_stable          TYPE lvc_s_stbl VALUE 'XX'.

DATA: BEGIN OF act_sellist OCCURS 10.
        INCLUDE STRUCTURE vimsellist.
      DATA: END OF act_sellist.

DATA: BEGIN OF act_exclfun OCCURS 10.
        INCLUDE STRUCTURE vimexclfun.
      DATA: END OF act_exclfun.


*&---------------------------------------------------------------------*
*&       P R O C E S S A M E N T O                                     *
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM  zf_seleciona_dados.

  "Chama tela de Parâmetros
  CALL SCREEN 0100.


CLASS lcl_hander DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,

      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.

*      ON_BUTTON FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
*        IMPORTING ES_COL_ID ES_ROW_NO  SENDER.
ENDCLASS.

CLASS lcl_hander IMPLEMENTATION.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_data_changed_finished.
  ENDMETHOD.

*  METHOD ON_BUTTON.
*
*
*   ENDMETHOD.
ENDCLASS.

FORM zf_seleciona_dados.

  REFRESH it_saida.

  SELECT *
    FROM zmmt0131 INTO TABLE @DATA(it_zmmt0131).

  LOOP AT it_zmmt0131 INTO DATA(wa_zmmt0131).

    wa_saida-usuario_lib   = wa_zmmt0131-usuario_lib.
    wa_saida-werks         = wa_zmmt0131-werks.
    wa_saida-nao_chave     = wa_zmmt0131-nao_chave.

    wa_saida-usnam         = wa_zmmt0131-usnam.
    wa_saida-zdt_atual     = wa_zmmt0131-zdt_atual.
    wa_saida-zhr_atual     = wa_zmmt0131-zhr_atual.


    FREE wa_saida-celltab.
    wa_estilo =  VALUE #(  ( fieldname = 'USUARIO_LIB'     style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'WERKS'           style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'USNAM'           style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'ZDT_ATUAL'       style = cl_gui_alv_grid=>mc_style_disabled )
                           ( fieldname = 'ZHR_ATUAL'       style = cl_gui_alv_grid=>mc_style_disabled )   ).
    INSERT LINES OF wa_estilo INTO TABLE wa_saida-celltab.
    APPEND wa_saida TO it_saida.

    CLEAR: wa_saida, wl_name.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR  'TL_0100'.

  PERFORM zf_alv.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'CONTAINER'.

    IF g_grid IS INITIAL AND g_custom_container IS NOT INITIAL.

      CREATE OBJECT g_grid
        EXPORTING
          i_parent = g_custom_container.
    ENDIF.

    wa_layout-stylefname = 'CELLTAB'.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        i_save          = 'X'
        is_layout       = wa_layout
      CHANGING
        it_outtab       = it_saida
        it_fieldcatalog = it_fieldcat.

    SET HANDLER: lcl_hander=>on_data_changed FOR g_grid.
*                LCL_HANDER=>ON_BUTTON FOR G_GRID.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
    CALL METHOD g_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcat.

    CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM zf_save.
    WHEN '&EDIT'.
      CALL METHOD g_grid->get_selected_rows
        IMPORTING
          et_index_rows = DATA(it_selected).

      IF it_selected IS INITIAL.
        MESSAGE 'Favor selecionar uma linha!' TYPE 'I'.
        EXIT.
      ELSE.

        LOOP AT it_selected INTO DATA(wa_selected).

          READ TABLE it_saida INTO wa_saida INDEX wa_selected-index.
          FREE wa_saida-celltab.

          wa_estilo = VALUE #( ( fieldname = 'GROUP_TAX'      style = cl_gui_alv_grid=>mc_style_enabled )  ).

          INSERT LINES OF wa_estilo INTO TABLE wa_saida-celltab.

          MODIFY it_saida FROM wa_saida INDEX wa_selected-index.
        ENDLOOP.
      ENDIF.

    WHEN '&INS'.

      CLEAR wa_saida.

      FREE wa_saida-celltab.

      wa_estilo =  VALUE #( ( fieldname = 'USUARIO_LIB'     style = cl_gui_alv_grid=>mc_style_enabled )
                            ( fieldname = 'WERKS'           style = cl_gui_alv_grid=>mc_style_enabled ) ).


      INSERT LINES OF wa_estilo INTO TABLE wa_saida-celltab.
      APPEND wa_saida TO it_saida.

    WHEN '&DEL'.

      CALL METHOD g_grid->get_selected_rows
        IMPORTING
          et_index_rows = it_selected.

      IF it_selected[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'I'.
        EXIT.
      ELSE.
        LOOP AT it_selected INTO wa_selected.

          READ TABLE it_saida INTO wa_saida INDEX wa_selected-index.

          DELETE FROM zmmt0131 WHERE usuario_lib = wa_saida-usuario_lib
                                 AND werks       = wa_saida-werks.


        ENDLOOP.
      ENDIF.

      PERFORM zf_seleciona_dados.

      CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).



  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_alv .

  it_fieldcat =  VALUE lvc_t_fcat(
  ( ref_table = 'USR21'  ref_field = 'BNAME'   fieldname = 'USUARIO_LIB'      coltext =   'Usuário Liberado'        outputlen = '16'  checkbox = ''   edit = ''   )
  ( ref_table = 'T001W'  ref_field = 'WERKS'   fieldname = 'WERKS'            coltext =   'Centro'                  outputlen = '06'  checkbox = ''   edit = ''   )
  ( ref_table = ' '      ref_field = ' '       fieldname = 'NAO_CHAVE'        coltext =   'Não Informa Chave'       outputlen = '20'  checkbox = 'X'  edit = 'X'   ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_save.

  DATA: it_save TYPE TABLE OF zmmt0131,
        wa_save TYPE zmmt0131.

  LOOP AT it_saida INTO wa_saida.

    IF wa_saida-usuario_lib IS INITIAL.
      MESSAGE 'Campo Usuário Liberado obrigatório!' TYPE 'E'.
      EXIT.
    ELSE.
      MOVE  wa_saida-usuario_lib TO wa_save-usuario_lib.
    ENDIF.

    IF wa_saida-werks IS INITIAL.
      MESSAGE 'Campo Centro obrigatório!' TYPE 'E'.
      EXIT.
    ELSE.
      MOVE  wa_saida-werks  TO wa_save-werks.
    ENDIF.
    MOVE wa_saida-nao_chave TO wa_save-nao_chave.
    "
    wa_save-usnam       = sy-uname.
    wa_save-zdt_atual   = sy-datum.
    wa_save-zhr_atual   = sy-uzeit.

    APPEND wa_save TO it_save.
    CLEAR: wa_save, wa_saida.

  ENDLOOP.

  IF it_save IS NOT INITIAL.
    MODIFY zmmt0131 FROM TABLE it_save.
    MESSAGE 'Dados gravado com sucesso!' TYPE 'S'.

    LOOP AT it_saida INTO wa_saida.
      FREE wa_saida-celltab.
      wa_estilo =  VALUE #(  ( fieldname = 'USUARIO_LIB'     style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'WERKS'           style = cl_gui_alv_grid=>mc_style_disabled )
                             ( fieldname = 'NAO_CHAVE'       style = cl_gui_alv_grid=>mc_style_disabled ) ).
      INSERT LINES OF wa_estilo INTO TABLE wa_saida-celltab.
      MODIFY it_saida FROM wa_saida.
    ENDLOOP.
  ENDIF.

  REFRESH it_save.
ENDFORM.
