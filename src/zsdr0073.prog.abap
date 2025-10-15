*&---------------------------------------------------------------------*
*& Report  ZSDR0073
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0073.

TYPE-POOLS: rmdi.
INCLUDE: <icon>.
INCLUDE <cl_alv_control>.

TYPES: BEGIN OF ty_saida,
         gravado TYPE c.
         INCLUDE STRUCTURE zsdt0146.
       TYPES  END OF ty_saida.


*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_x              TYPE c VALUE 'X',
           c_add(3)         TYPE c VALUE 'ADD',
           c_del(3)         TYPE c VALUE 'DEL',
           c_limite(6)      TYPE c VALUE 'LIMITE',
           c_exit(4)        TYPE c VALUE 'EXIT',
           c_back(4)        TYPE c VALUE 'BACK',
           c_save(4)        TYPE c VALUE 'SAVE',
           c_proces(6)      TYPE c VALUE 'PROCES',
           c_cancel(6)      TYPE c VALUE 'CANCEL',
           c_atuali(6)      TYPE c VALUE 'ATUALI',
           c_search(6)      TYPE c VALUE 'SEARCH',
           c_show_msgre(10) TYPE c VALUE 'SHOW_MSGRE'.


*&--------------------------------------------------------------------&*
*& Declaração de Variaveis/Tabelas/Workarea                           &*
*&--------------------------------------------------------------------&*
DATA: tg_0146         TYPE TABLE OF zsdt0146 WITH HEADER LINE,
      tg_saida        TYPE TABLE OF ty_saida WITH HEADER LINE,
      tg_saida_aux    TYPE TABLE OF ty_saida WITH HEADER LINE,
      ok_code         TYPE sy-ucomm,
      init,
      wg_display,
      x_field(30),
      wg_mensagem(30),
      wg_obj(40).

DATA: tg_msg_ret TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.
*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
*Class definition for ALV toolbar
CLASS: lcl_alv_toolbar   DEFINITION DEFERRED.

DATA: grid1                TYPE REF TO cl_gui_alv_grid,
      container1           TYPE REF TO cl_gui_docking_container,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: t_fieldcatalog  TYPE lvc_t_fcat,
      w_fieldcatalog  TYPE lvc_s_fcat,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
      tg_selectedrow  TYPE lvc_t_row,
      wg_selectedrow  TYPE lvc_s_row,
      wa_layout       TYPE lvc_s_layo,
      gs_variant_c    TYPE disvariant,
      v_report        LIKE sy-repid,
      wa_stable       TYPE lvc_s_stbl,
      wg_cell         TYPE lvc_s_cell,
      tg_cell         TYPE lvc_t_cell,
      wa_style        TYPE lvc_s_styl,
      style2          TYPE lvc_t_styl WITH HEADER LINE.

START-OF-SELECTION.
  CALL SCREEN 100.

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: wl_desactive.

    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    IF wg_display IS INITIAL.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    IF wg_display IS INITIAL.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_ods_act.
    ty_toolbar-function  =  c_limite.
    ty_toolbar-text      = 'Limite'.
    IF wg_display IS INITIAL.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
    DATA: wl_saida   TYPE ty_saida,
          var_answer TYPE c.

    CASE e_ucomm.
      WHEN c_limite.
        CALL TRANSACTION 'ZSDT0183'.
      WHEN c_add.

        CLEAR wl_saida.
        wl_saida-usnam    = sy-uname.
        wl_saida-dt_atual = sy-datum.
        wl_saida-hr_atual = sy-uzeit.
        APPEND wl_saida TO tg_saida.

        SORT tg_saida BY dt_atual hr_atual DESCENDING.

      WHEN c_del.
        CALL METHOD grid1->get_selected_rows
          IMPORTING
            et_index_rows = tg_selectedrow.

        CHECK tg_selectedrow IS NOT INITIAL.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = 'Deseja realmente excluir os registros selecionados?'
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        CHECK var_answer EQ '1'.

        LOOP AT tg_selectedrow INTO wg_selectedrow.
          READ TABLE tg_saida INTO wl_saida INDEX wg_selectedrow-index.
          CHECK sy-subrc = 0.

          DELETE FROM zsdt0146 WHERE docnum = wl_saida-docnum.
          IF sy-subrc = 0.
            DELETE tg_saida INDEX wg_selectedrow-index.
          ENDIF.
        ENDLOOP.

        MESSAGE 'Registros excluídos com sucesso!' TYPE 'S'.

    ENDCASE.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.
  IF wg_display IS NOT INITIAL.
    APPEND c_save TO fcode.
  ENDIF.

  IF wg_cell IS NOT INITIAL .
    REFRESH: tg_cell.
    APPEND wg_cell TO tg_cell.
    CALL METHOD grid1->set_selected_cells "(
      EXPORTING
        it_cells = tg_cell[].
  ENDIF.

  SET PF-STATUS 'Z001' EXCLUDING fcode.
  SET TITLEBAR 'Z001'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: wl_repid    TYPE sy-repid,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        lt_f4       TYPE lvc_t_f4 WITH HEADER LINE,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt.

  wl_repid = sy-repid.

  IF container1 IS INITIAL.
    wa_layout-zebra      = c_x.
    wa_stable-row        = c_x.
    wa_layout-sel_mode   = 'C'.
    wa_layout-box_fname  = 'MARK'.

    CREATE OBJECT container1
      EXPORTING
        repid     = wl_repid
        dynnr     = '0100'
        side      = container1->dock_at_top
        extension = 400.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container1.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.


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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    PERFORM montar_layout.

    gs_variant_c-report = sy-repid.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant_c
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
        i_save               = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_saida[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid1->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].
  ELSE.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT

MODULE init OUTPUT.
  DATA: wl_view_name TYPE ocus-table VALUE 'ZSDT0146',
        tl_rangetab  TYPE TABLE OF vimsellist,
        wl_lockuser  TYPE sy-uname,
        answer.

  IF init IS INITIAL.
    CLEAR: wg_display.
    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        view_name        = wl_view_name
        action           = 'E'
        enqueue_mode     = 'E'
      TABLES
        sellist          = tl_rangetab
      EXCEPTIONS
        foreign_lock     = 1
        system_failure   = 2
        table_not_found  = 5
        client_reference = 7.
    CASE sy-subrc.
      WHEN 1.
        wl_lockuser = sy-msgv1(12).
        CALL FUNCTION 'POPUP_TO_DECIDE_LOCKED_DATA'
          EXPORTING
            i_user   = sy-msgv1(12)
          IMPORTING
            e_answer = answer.
        IF answer = '2'.
          MESSAGE s049(sv) WITH wl_lockuser RAISING foreign_lock.
          EXIT.
        ELSEIF answer = '1'.
          MOVE: c_x TO wg_display.
        ENDIF.
      WHEN 2.
        MESSAGE e050(sv) WITH wl_view_name RAISING system_failure.
      WHEN 5.
        MESSAGE e028(sv) WITH wl_view_name RAISING view_not_found.
      WHEN 7.
        MESSAGE e054(sv) WITH sy-mandt RAISING client_reference.
    ENDCASE.

    PERFORM seleciona_dados.
    PERFORM organiza_dados.
    init = c_x.
  ENDIF.
ENDMODULE.                 " INIT  OUTPUT

FORM seleciona_dados .

  REFRESH: tg_saida, tg_0146, tg_saida_aux[].

  SELECT *
    FROM zsdt0146 INTO TABLE tg_0146.

ENDFORM.                    " SELECIONA_DADOS

FORM montar_layout.
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        0 'ZSDT0146'            'DOCNUM'           'TG_SAIDA' 'DOCNUM'            'Nr.Doc.'     ' '   'X' ' ' ' ',
        1 'ZSDT0146'            'OBJ_KEY'          'TG_SAIDA' 'OBJ_KEY'           'Planilha'    ' '   'X' ' ' ' ',
        2 'ZSDT0146'            'USNAM'            'TG_SAIDA' 'USNAM'             'Usuário'     ' '   ' ' ' ' ' ',
        3 'ZSDT0146'            'DT_ATUAL'         'TG_SAIDA' 'DT_ATUAL'          'Data'        ' '   ' ' ' ' ' ',
        4 'ZSDT0146'            'HR_ATUAL'         'TG_SAIDA' 'HR_ATUAL'          'Hora'        ' '   ' ' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT

FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).
  "VALUE(P_F4).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  IF wg_display IS INITIAL.
    w_fieldcatalog-edit          = p_edit.
  ENDIF.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura

MODULE user_command_0100 INPUT.
  DATA: wl_cell    TYPE lvc_s_cell,
        tl_cell    TYPE lvc_t_cell,
        wl_obj(30).

  REFRESH: tl_cell.
  CLEAR: wl_cell, wl_obj.

  CASE ok_code.
    WHEN c_save.
      CALL METHOD grid1->check_changed_data.

      PERFORM grava_dados.
      PERFORM seleciona_dados.
      PERFORM organiza_dados.

    WHEN c_search.
    WHEN c_show_msgre.
    WHEN c_back.
      LEAVE TO SCREEN 0.
    WHEN c_cancel.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 100.
    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

FORM organiza_dados .

  SORT: tg_0146 BY dt_atual hr_atual DESCENDING.

  LOOP AT tg_0146.

    CLEAR: tg_saida.

    MOVE: tg_0146-docnum            TO  tg_saida-docnum,
          tg_0146-obj_key           TO  tg_saida-obj_key,
          tg_0146-usnam             TO  tg_saida-usnam,
          tg_0146-dt_atual          TO  tg_saida-dt_atual,
          tg_0146-hr_atual          TO  tg_saida-hr_atual,
          'X'                       TO  tg_saida-gravado.

    APPEND tg_saida.
  ENDLOOP.

  SORT tg_saida BY dt_atual hr_atual DESCENDING.

  IF tg_saida[] IS NOT INITIAL.
    tg_saida_aux[] = tg_saida[].
  ENDIF.
ENDFORM.                    " ORGANIZA_DADOS

FORM grava_dados .
  DATA: tl_input_0146 TYPE TABLE OF zsdt0146 WITH HEADER LINE,
        tl_0146       TYPE TABLE OF zsdt0146 WITH HEADER LINE.

  CLEAR: tl_input_0146, tl_input_0146[].

  LOOP AT tg_saida WHERE gravado IS INITIAL.
    MOVE-CORRESPONDING: tg_saida TO tl_input_0146.

    APPEND tl_input_0146.
    CLEAR: tl_input_0146.
  ENDLOOP.


  MODIFY zsdt0146 FROM TABLE tl_input_0146.
  IF sy-subrc IS INITIAL.
    COMMIT WORK AND WAIT.
    MESSAGE s836(sd) WITH 'Os dados foram salvos!'.
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ocorreu um erro na gravação!'.
  ENDIF.
ENDFORM.                    " GRAVA_DADOS
