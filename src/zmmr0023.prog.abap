*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Report  ZMMR0023                                                     *
* Descrição  : Cadastro de Centro ( Ativo / Inativo ) Negativo         *
* Módulo     : MM                               Transação: ZMM0029     *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Camila Brand                            Data: 06/02/2012*
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
REPORT  zmmr0023.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: rmdi.


INCLUDE <icon>.
*----------------------------------------------------------------------*
* ESTRUTURA
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_saida,
*  status   type zmm0023-status ,
         status(4),
         lgort      TYPE zmmt0017-lgort,
         werks      TYPE zmm0023-werks,
         matnr      TYPE zmmt0017-matnr, "AOENNING #108346
         maktx      TYPE makt-maktx, "AOENNING #108346
         matkl      TYPE zmm0023-matkl, "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)
         wgbez      TYPE t023t-wgbez, "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)
         usrcad     TYPE zmm0023-usrcad,
         usralt     TYPE zmm0023-usralt,
         erdat      TYPE zmm0023-erdat,
         aedat      TYPE zmm0023-aedat,
         motivo     TYPE zmm0023-motivo,
         style      TYPE lvc_t_styl,
         cwerks     TYPE zmm0023-cwerks,
         hrcad      TYPE zmm0023-hrcad,
         hralt      TYPE zmm0023-hralt,
         histor(15),
       END OF ty_saida.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_x              TYPE c VALUE 'X',
           c_add(3)         TYPE c VALUE 'ADD',
           c_del(3)         TYPE c VALUE 'DEL',
           c_exit(4)        TYPE c VALUE 'EXIT',
           c_back(4)        TYPE c VALUE 'BACK',
           c_save(4)        TYPE c VALUE 'SAVE',
           c_ativa(5)       TYPE c VALUE 'DESAT',
           c_proces(6)      TYPE c VALUE 'PROCES',
           c_cancel(6)      TYPE c VALUE 'CANCEL',
           c_atuali(6)      TYPE c VALUE 'ATUALI',
           c_search(6)      TYPE c VALUE 'SEARCH',
           c_show_msgre(10) TYPE c VALUE 'SHOW_MSGRE'.

*&--------------------------------------------------------------------&*
*& Declaração de Variaveis/Tabelas/Workarea                           &*
*&--------------------------------------------------------------------&*
DATA: tg_0023         TYPE TABLE OF  zmm0023 WITH HEADER LINE,
      tg_0017         TYPE TABLE OF  zmmt0017,
      tg_134          TYPE TABLE OF  t134g,
      tg_zmmt0017_log TYPE TABLE OF  zmmt0017_log,
      tg_makt         TYPE TABLE OF  makt,
      tg_t023t        TYPE TABLE OF  t023t,"SMC #108346 - 16-05 (Adicionado grupo de mercadoria)
*      tg_matkl        TYPE TABLE OF  mara,"SMC #108346 - 16-05 (Adicionado grupo de mercadoria)
      ok_code         TYPE sy-ucomm,
      init,
      wg_display,
      x_field(30),
      wg_mensagem(30),
      wg_obj(40),
      tg_saida        TYPE TABLE OF ty_saida WITH HEADER LINE,
      tg_saida2       TYPE TABLE OF ty_saida WITH HEADER LINE,
      style           TYPE lvc_t_styl WITH HEADER LINE,
      wa_style        TYPE lvc_s_styl,
      editor          TYPE REF TO cl_gui_textedit,
      wg_flag,
      p_seq_cwerks    TYPE zmm0023-cwerks.

DATA: tg_msg_ret TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.
DATA: wl_repid    TYPE sy-repid,
      tl_function TYPE ui_functions,
      wl_function LIKE tl_function WITH HEADER LINE.
*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.


*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.

DATA: grid1                TYPE REF TO cl_gui_alv_grid,
      grid2                TYPE REF TO cl_gui_alv_grid,
      container1           TYPE REF TO cl_gui_docking_container,
      container2           TYPE REF TO cl_gui_docking_container,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: container_2 TYPE REF TO cl_gui_container,        "splitter conteiner 1
      container_3 TYPE REF TO cl_gui_container.        "splitter conteiner 2
DATA splitter               TYPE REF TO cl_gui_splitter_container.

DATA: t_fieldcatalog  TYPE lvc_t_fcat,
      w_fieldcatalog  TYPE lvc_s_fcat,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
      wa_layout       TYPE lvc_s_layo,
      wa_stable       TYPE lvc_s_stbl,
      wg_cell         TYPE lvc_s_cell,
      tg_cell         TYPE lvc_t_cell.

CALL SCREEN 100.

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
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

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      handle_button_click FOR EVENT button_click
        OF cl_gui_alv_grid
        IMPORTING es_col_id
                  es_row_no.
    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

* SMC - #108346 - 16-05-24 - adicionado grupo de mercadoria
    CLASS-METHODS:
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells sender.

    CLASS-METHODS:
      on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname er_event_data es_row_no sender.


    CLASS-METHODS:
      on_DATA_CHANGED FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.
* SMC - #108346 - 16-05-24 - adicionado grupo de mercadoria

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION


**&---------------------------------------------------------------------*
**&      Form  get_next_number
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_7995   text
**      -->P_7996   text
**      <--P_VL_NRONC  text
**----------------------------------------------------------------------*
FORM get_next_number  USING    p_object   "TYPE nrobj
                               p_nr_range "TYPE nrnr
                      CHANGING p_number.

  CLEAR p_number.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = p_nr_range
      object                  = p_object
    IMPORTING
      number                  = p_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc NE 0.
    CLEAR: p_number.
    MESSAGE e836(sd) WITH 'O intervalo de numeração,'
                      'não foi encontrado!'.
  ELSE.
    wg_flag = c_x.
  ENDIF.

ENDFORM.                    " get_next_number

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.
  IF wg_display IS NOT INITIAL.
    APPEND c_save TO fcode.

  ENDIF.

  IF wg_cell IS NOT INITIAL .
    REFRESH: tg_cell.
    APPEND wg_cell TO tg_cell.
*          CONCATENATE wl_obj '->SET_SELECTED_CELLS' INTO wg_obj.
    CALL METHOD grid1->set_selected_cells "(wg_obj)          "(wg_msgs)=>set_selected_cells
      EXPORTING
        it_cells = tg_cell[].
  ENDIF.

  SET PF-STATUS 'Z001' EXCLUDING fcode.
  SET TITLEBAR 'Z001'.
  IF sy-subrc IS INITIAL.


  ENDIF.


ENDMODULE.                    "STATUS_0100 OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: wl_cell    TYPE lvc_s_cell,
        tl_cell    TYPE lvc_t_cell,
        wl_obj(30).

  REFRESH: tl_cell.
  CLEAR: wl_cell, wl_obj.

  CASE ok_code.
    WHEN c_save.
      PERFORM verifica_erros.
      IF tg_msg_ret[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen   = '100'
            i_show     = c_x
            i_repid    = sy-repid
            i_popup    = 1
*           i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*           I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            e_messagem = wg_mensagem
          TABLES
            it_msgs    = tg_msg_ret.
      ELSE.
        PERFORM grava_dados.
      ENDIF.

    WHEN c_ativa.
      PERFORM ativa_centro. " Ativar e desativar centro

  ENDCASE.

  " call method cl_gui_cfw=>dispatch. " CSB

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init OUTPUT.
  DATA: wl_view_name TYPE ocus-table VALUE 'ZMM0023',
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
        wl_lockuser = sy-msgv1(12).     "HCG sy-msgv1 lost at popup call
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
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .
  REFRESH: tg_0023.

  SELECT *
  FROM zmm0023
  INTO TABLE tg_0023.

  IF sy-subrc = 0.

    SELECT * FROM zmmt0017
      INTO TABLE tg_0017
      FOR ALL ENTRIES IN tg_0023
      WHERE centro_fixo = tg_0023-werks.

    IF sy-subrc = 0.

      FREE: tg_makt.
      SELECT * FROM makt
        INTO TABLE tg_makt
        FOR ALL ENTRIES IN tg_0017
        WHERE matnr  = tg_0017-matnr
          AND spras = sy-langu.


      "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)
      SELECT zmmt0017~matnr,mara~matkl
        INTO TABLE @DATA(tg_matkl)
        FROM zmmt0017
        INNER JOIN mara
         ON mara~matnr = zmmt0017~matnr.

      FREE: tg_t023t.
      SELECT * FROM t023t
        INTO TABLE tg_t023t
        FOR ALL ENTRIES IN tg_matkl
        WHERE matkl  = tg_matkl-matkl
          AND spras = sy-langu.
       "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)


      SELECT * FROM t134g
        INTO TABLE  tg_134
        FOR ALL ENTRIES IN tg_0023
        WHERE werks = tg_0023-werks.

      SELECT * FROM zmmt0017_log
        INTO TABLE tg_zmmt0017_log
        FOR ALL ENTRIES IN tg_0023
           WHERE centro_fixo = tg_0023-werks.

    ENDIF.


  ENDIF.

ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organiza_dados .
  " Ordena e pega os registros da tabela.
  "sort tg_0023 by  werks descending . "cwerks
  SORT tg_0023 BY  werks matnr matkl DESCENDING. "SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria - add nessa linha "MATKL"
  " Exclui os registros antigos e pega somente o recente.
  DELETE ADJACENT DUPLICATES FROM tg_0023 COMPARING werks matnr matkl."SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria - add nessa linha "MATKL"

  DATA: wl_insert LIKE LINE OF tg_saida.

  REFRESH: tg_saida-style,
         style,
         wl_insert-style.

  REFRESH: tg_saida.

  LOOP AT tg_0023.


*    READ TABLE tg_0017 INTO DATA(ls_0017) WITH KEY centro_fixo = tg_0023-werks.


    tg_saida-matnr = tg_0023-matnr.
    tg_saida-matkl = tg_0023-matkl."SMC #108346 - 16-05 (Adicionado grupo de mercadoria)

    READ TABLE tg_134 INTO DATA(ls_134) WITH KEY werks = tg_0023-werks.
    tg_saida-lgort = ls_134-gsber.


    IF sy-subrc = 0.

      READ TABLE tg_makt INTO DATA(ls_makt) WITH KEY matnr  = tg_0023-matnr.
      tg_saida-maktx = ls_makt-maktx.

      "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)
      READ TABLE tg_t023t INTO DATA(ls_t023t) WITH KEY matkl  = tg_0023-matkl.
      tg_saida-wgbez = ls_t023t-wgbez.
      "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)
    ENDIF.

    MOVE: " Usar icone no status.
          tg_0023-werks   TO  tg_saida-werks  ,
          tg_0023-usrcad  TO  tg_saida-usrcad ,
          tg_0023-usralt  TO  tg_saida-usralt ,
          tg_0023-erdat   TO  tg_saida-erdat  ,
          tg_0023-aedat   TO  tg_saida-aedat  ,
          tg_0023-motivo  TO  tg_saida-motivo ,
          tg_0023-cwerks  TO  tg_saida-cwerks ,
          tg_0023-hrcad   TO  tg_saida-hrcad,
          tg_0023-hralt   TO  tg_saida-hralt,
          'Historico'     TO  tg_saida-histor.


*      READ TABLE tg_zmmt0017_log INTO DATA(ls_17log) WITH KEY matnr  = tg_saida-matnr
*                                                              centro_fixo = tg_saida-werks.
*
*      IF sy-subrc = 0.
*        tg_saida-status = ls_17log-status.
*
*      ELSE.
    " para usar icone nos status
    IF tg_0023-status EQ 'I'.
      MOVE: icon_locked TO tg_saida-status.
    ELSEIF tg_0023-status EQ 'A'.
      MOVE: icon_unlocked TO tg_saida-status.
    ELSE.
      MOVE: icon_yellow_light  TO tg_saida-status.
    ENDIF.
*      ENDIF.
    wa_style-fieldname = 'WERKS'.
    wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT  wa_style INTO TABLE style .

    wa_style-fieldname = 'MATNR'.
    wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT  wa_style INTO TABLE style .

    "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)
    wa_style-fieldname = 'MATKL'.
    wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT  wa_style INTO TABLE style .
    "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)

    wa_style-fieldname = 'MOTIVO'.
    wa_style-style = cl_gui_alv_grid=>mc_style_button.
    INSERT  wa_style INTO TABLE style .

    wa_style-fieldname = 'HISTOR'.
    wa_style-style = cl_gui_alv_grid=>mc_style_button.
    INSERT  wa_style INTO TABLE style .

    INSERT LINES OF style INTO TABLE tg_saida-style.


    tg_0023-matnr = tg_saida-matnr.
    tg_0023-matkl = tg_saida-matkl."SMC #108346 - 16-05 (Adicionado grupo de mercadoria)
    MODIFY zmm0023 FROM tg_0023.

    APPEND tg_saida.
    CLEAR: tg_saida.
*    ENDLOOP.
    CLEAR:tg_0023, ls_makt, ls_t023t.
  ENDLOOP.


  DELETE tg_saida WHERE lgort = ' '.

ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  REFRESH tl_function.
  wl_repid = sy-repid.

  IF container1 IS INITIAL.
    wa_layout-zebra      = c_x.
    wa_stable-row        = c_x.

    "wa_layout-no_rowmark = c_x.

    CREATE OBJECT container1
      EXPORTING
        repid     = wl_repid
        dynnr     = '0100'
        side      = container1->dock_at_top
        extension = 500.

    wa_layout-stylefname = 'STYLE'.

*    create object splitter
*      exporting
*        parent  = container1
*        rows    = 2
*        columns = 1.
*
*    call method splitter->get_container
*      exporting
*        row       = 1
*        column    = 1
*      receiving
*        container = container_2.
*
*    call method splitter->set_row_height
*      exporting
*        id     = 1
*        height = 100.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container1. "container_2.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command FOR grid1.
    "SET HANDLER obg_toolbar->on_toolbar FOR grid1.

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

    PERFORM montar_layout.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_saida[].

    SET HANDLER:
              lcl_event_handler=>handle_button_click FOR grid1,
              lcl_event_handler=>on_data_changed_finished FOR grid1,
              lcl_event_handler=>on_f4 FOR grid1, "SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria
              lcl_event_handler=>on_f1 FOR grid1,
              lcl_event_handler=>on_data_changed FOR grid1.


    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


  ELSE.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1  ' ' ' '               'TG_SAIDA' 'STATUS'     'Status'                  '06' ' ' '' '',
        2  ' ' ' '               'TG_SAIDA' 'LGORT'      'Divisão'                 '06' ' ' '' '',
        3  'ZMM0023' 'WERKS'     'TG_SAIDA' 'WERKS'      ' '                       '12' 'X' '' '',
        4  'MARA   ' 'MATNR'     'TG_SAIDA' 'MATNR'      'Material'                '18' 'X' '' '',
        5  ' ' ' '               'TG_SAIDA' 'MAKTX'      'Desc.Material'           '20' ' ' '' '',
        6  'ZMM0023' 'MATKL'     'TG_SAIDA' 'MATKL'      'Grp Mercadoria'          '18' 'X' '' '',"SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria
        7  ' ' ' '               'TG_SAIDA' 'WGBEZ'      'Desc.Gp Mercadoria'      '20' ' ' '' '',"SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria
        8  'ZMM0023' 'USRCAD'    'TG_SAIDA' 'USRCAD'     'Cadastrado por'          '14' ' ' '' '',
        9  'ZMM0023' 'USRALT'    'TG_SAIDA' 'USRALT'     'Alterado por'            '12' ' ' '' '',
        10  'ZMM0023' 'ERDAT'     'TG_SAIDA' 'ERDAT'      'Data Cadastro'           '13' ' ' '' '',
        11  'ZMM0023' 'AEDAT'     'TG_SAIDA' 'AEDAT'      'Data de Alteração'      '17' ' ' '' '',
        12  'ZMM0023' 'HRCAD'     'TG_SAIDA' 'HRCAD'      'Hora Cad.'              '08' ' ' '' '',
        13 'ZMM0023' 'HRALT'     'TG_SAIDA' 'HRALT'      'Hora Alt.'               '08' ' ' '' '',
        14 'ZMM0023' 'MOTIVO'    'TG_SAIDA' 'MOTIVO'     'Motivo'                  '12' ' ' '' '',
        15 ' '        ' '        'TG_SAIDA' 'HISTOR'     'Historico'               '12' ' ' '' ''.
ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0610   text
*      -->P_0611   text
*      -->P_0612   text
*      -->P_0613   text
*      -->P_0614   text
*      -->P_0615   text
*      -->P_0616   text
*      -->P_0617   text
*      -->P_0618   text
*----------------------------------------------------------------------*
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

  CLEAR w_fieldcatalog.

  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  IF wg_display IS INITIAL.
    w_fieldcatalog-edit          = p_edit.
  ENDIF.
  w_fieldcatalog-do_sum          = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen     = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.


  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_alv_toolbar
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
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
    ty_toolbar-butn_type   = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    IF wg_display IS INITIAL.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type   = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
    CASE e_ucomm.

      WHEN c_add.

        "APPEND INITIAL LINE TO TG_SAIDA.


        DATA: wl_insert LIKE LINE OF tg_saida.

        REFRESH: tg_saida-style,
                 style,
                 wl_insert-style.

        MOVE: icon_locked   TO   wl_insert-status , "SMC - US #108346 bug #139737
              sy-uname      TO   wl_insert-usrcad ,
              sy-datum      TO   wl_insert-erdat  ,
              sy-uzeit      TO   wl_insert-hrcad.

        wa_style-fieldname = 'WERKS'.
        wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'MATNR'.
        wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
        INSERT  wa_style INTO TABLE style .

        "SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria
        wa_style-fieldname = 'MATKL'.
        wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
        INSERT  wa_style INTO TABLE style .
        "SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria

        wa_style-fieldname = 'MOTIVO'.
        wa_style-style = cl_gui_alv_grid=>mc_style_button.
        INSERT  wa_style INTO TABLE style .

        INSERT LINES OF style INTO TABLE wl_insert-style.


        PERFORM get_next_number  USING  'ZCWERKS'
                                '1'
                                CHANGING p_seq_cwerks.

        MOVE p_seq_cwerks TO wl_insert-cwerks.

        APPEND wl_insert TO  tg_saida .
        SORT tg_saida BY werks.

        CLEAR: wl_insert.

      WHEN 'DEL'.
*        elimina a linha do alv e da tabela.

        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          READ TABLE tg_saida INTO tg_saida INDEX wg_selectedcell-row_id-index.

          DELETE FROM zmm0023 WHERE werks = tg_saida-werks
                            AND matnr = tg_saida-matnr
                            AND matkl = tg_saida-matkl."SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria

          DELETE tg_saida WHERE werks = tg_saida-werks
                            AND matnr = tg_saida-matnr
                            AND matkl = tg_saida-matkl."SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria

        ENDLOOP.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

        CALL METHOD grid1->set_table_for_first_display
          EXPORTING
            it_toolbar_excluding = tl_function
            is_layout            = wa_layout
          CHANGING
            it_fieldcatalog      = t_fieldcatalog[]
            it_outtab            = tg_saida[].

    ENDCASE.

    CALL METHOD grid1->get_selected_cells
      IMPORTING
        et_cell = tg_selectedcell.

    LOOP AT tg_selectedcell INTO wg_selectedcell.
      READ TABLE tg_saida ASSIGNING FIELD-SYMBOL(<ws_saida>) INDEX wg_selectedcell-row_id-index.

      "Preenche a descrição material.
      IF <ws_saida>-matnr IS NOT INITIAL.
        SELECT SINGLE maktx FROM makt
        INTO <ws_saida>-maktx
          WHERE matnr  = <ws_saida>-matnr
           AND spras EQ sy-langu.
      ENDIF.

      "Preenche a descrição Grupo de Mercadoria. "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)
      IF <ws_saida>-matkl IS NOT INITIAL.
        SELECT SINGLE wgbez FROM t023t
        INTO <ws_saida>-wgbez
          WHERE matkl  = <ws_saida>-matkl
           AND spras EQ sy-langu.
      ENDIF.
      "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)

    ENDLOOP.


    PERFORM verifica_erros.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = space
        i_repid    = sy-repid
        i_popup    = 1
*       i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*       I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        e_messagem = wg_mensagem
      TABLES
        it_msgs    = tg_msg_ret.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.               "lcl_alv_toolbar
"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD  handle_button_click.
    DATA:  tg_texto TYPE catsxt_longtext_itab.
    DATA : wa_texto TYPE LINE OF catsxt_longtext_itab.
    DATA:  wl_texto TYPE LINE OF catsxt_longtext_itab.
    DATA:  wl_display.

    READ TABLE tg_saida INTO tg_saida INDEX  es_row_no-row_id.

    IF es_col_id-fieldname = 'MOTIVO'.
      IF tg_saida-motivo IS NOT INITIAL.
        wl_texto = tg_saida-motivo.
        wl_display = 'X'.
        APPEND wl_texto TO tg_texto.
      ELSE.
        CLEAR: wl_display.
        REFRESH: tg_texto.
      ENDIF.


      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title        = 'Texto para Motivo'
          im_display_mode = wl_display " Somente vizualizar ou inserir
        CHANGING
          ch_text         = tg_texto.
      IF wl_display IS INITIAL.
        DATA: wa_linha TYPE cacs_char1000sf.

        CLEAR: wa_texto, wa_linha.

        " Se tiver mais de uma linha.
        LOOP AT tg_texto INTO wa_texto.
          IF sy-tabix EQ 1.
            wa_linha = wa_texto.
          ELSE.
            CONCATENATE wa_linha wa_texto
            INTO wa_linha SEPARATED BY space.
          ENDIF.
        ENDLOOP.

        MOVE wa_linha TO tg_saida-motivo.

        MODIFY tg_saida FROM tg_saida INDEX es_row_no-row_id .

      ENDIF.
    ELSE.
      SELECT *
        FROM zmm0023
        INTO TABLE tg_0023
        WHERE werks = tg_saida-werks
        AND matnr = tg_saida-matnr "smc
        AND matkl = tg_saida-matkl "smc
        ORDER BY cwerks.
      REFRESH tg_saida2.
      LOOP AT tg_0023 INTO tg_0023.
        MOVE: " Usar icone no status.
              tg_0023-werks   TO  tg_saida2-werks  ,
              tg_0023-usrcad  TO  tg_saida2-usrcad ,
              tg_0023-usralt  TO  tg_saida2-usralt ,
              tg_0023-erdat   TO  tg_saida2-erdat  ,
              tg_0023-aedat   TO  tg_saida2-aedat  ,
              tg_0023-motivo  TO  tg_saida2-motivo ,
              tg_0023-cwerks  TO  tg_saida2-cwerks ,
              tg_0023-hrcad   TO  tg_saida2-hrcad,
              tg_0023-hralt   TO  tg_saida2-hralt.

        " para usar icone nos status
        IF tg_0023-status EQ 'I'.
          MOVE: icon_locked TO tg_saida2-status.
        ELSEIF tg_0023-status EQ 'A'.
          MOVE: icon_unlocked TO tg_saida2-status.
        ELSE.
          MOVE: icon_yellow_light  TO tg_saida2-status.
        ENDIF.

        APPEND tg_saida2 TO tg_saida2.
        CLEAR: tg_saida2, tg_0023.
      ENDLOOP.
      CALL SCREEN 0200 STARTING AT 050 3
                       ENDING   AT 170 20.


    ENDIF.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.


  ENDMETHOD.                    "handle_button_click

* SMC - #108346 16-05-2024 - Adicionado grupo de Mercadoria
  METHOD on_f4.
    BREAK-POINT.
  ENDMETHOD.
  METHOD on_f1.
    BREAK-POINT.
  ENDMETHOD.
  METHOD on_data_changed.
*    BREAK-POINT.
  ENDMETHOD.
* SMC - #108346 16-05-2024 - Adicionado grupo de Mercadoria


  METHOD on_data_changed_finished.
    " Verificar se a inserção já existe na tabela zsdt0023.
    "break abap.

    DATA: v_werks  TYPE zmm0023-werks,
          v_matnr  TYPE zmm0023-matnr,
          v_matkl  TYPE zmm0023-matkl, "SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria
          tl_werks TYPE zmm0023-werks,
          ls_good  TYPE lvc_s_modi.

    CLEAR: v_werks,
           tl_werks.


    LOOP AT et_good_cells INTO ls_good
       WHERE tabix GT 0.

      READ TABLE tg_saida ASSIGNING FIELD-SYMBOL(<ws_saida>) INDEX ls_good-row_id.
      v_werks = <ws_saida>-werks.
      v_matnr = <ws_saida>-matnr.
      v_matkl = <ws_saida>-matkl."SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria


      "Preenche a descrição material.
      IF <ws_saida>-matnr IS NOT INITIAL.
        SELECT SINGLE maktx FROM makt
        INTO <ws_saida>-maktx
          WHERE matnr  = <ws_saida>-matnr
          AND spras EQ sy-langu.
      ENDIF.

      "Preenche a descrição Grupo de Mercadoria. "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)
      IF <ws_saida>-matkl IS NOT INITIAL.
        SELECT SINGLE wgbez FROM t023t
        INTO <ws_saida>-wgbez
          WHERE matkl  = <ws_saida>-matkl
           AND spras EQ sy-langu.
      ENDIF.
      "SMC #108346 - 16-05 (Adicionado grupo de mercadoria)


    ENDLOOP.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    "SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria
    IF v_werks IS NOT INITIAL AND v_matnr IS NOT INITIAL AND v_werks IS NOT INITIAL AND v_matkl IS NOT INITIAL.

      MESSAGE e000(z01) WITH 'Informe só Centro e Material ou Centro e Grp Mercadoria' .

    ENDIF.

    IF v_werks IS NOT INITIAL AND v_matnr IS INITIAL AND v_matkl IS INITIAL.
    ELSEIF v_werks IS INITIAL AND v_matnr IS NOT INITIAL AND v_matkl IS INITIAL.
    ELSEIF v_werks IS INITIAL AND v_matnr IS INITIAL AND v_matkl IS NOT INITIAL.

      MESSAGE e000(z01) WITH 'Informe ao menos Centro e Material ou Centro e Grp Mercadoria' .

    ENDIF.
    "SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria

    IF v_werks IS NOT INITIAL AND v_matnr IS NOT INITIAL. "SMC #108346 - 16-05-2024 Comentado
      SELECT SINGLE werks
        FROM zmm0023
        INTO  tl_werks
      WHERE werks EQ v_werks
        AND matnr EQ v_matnr.
*        AND matkl EQ v_matkl."SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria

      IF tl_werks IS NOT  INITIAL.
        MESSAGE e024(sd) WITH 'O Centro e Material informado já existe.'.
        EXIT.
      ENDIF.
*      ENDIF.
    ENDIF.
    "SMC #108346 - 16-05-2024
    IF v_werks IS NOT INITIAL AND v_matkl IS NOT INITIAL.
      SELECT SINGLE werks
        FROM zmm0023
        INTO  tl_werks
      WHERE werks EQ v_werks
*        AND matnr EQ v_matnr
        AND matkl EQ v_matkl."SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria

      IF tl_werks IS NOT  INITIAL.
        MESSAGE e024(sd) WITH 'O Centro e Grp Mercadoria informado já existe.'.
        EXIT.
      ENDIF.
    ENDIF.
    "SMC #108346 - 16-05-2024



  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .

  DATA: tl_input_0023 TYPE TABLE OF zmm0023 WITH HEADER LINE,
        tg_0023_aux   TYPE TABLE OF  zmm0023 WITH HEADER LINE.

  SELECT *
   FROM zmm0023
   INTO TABLE tg_0023_aux.


  LOOP AT tg_saida.

    " Move status correto.
    IF tg_saida-status EQ icon_locked.
      MOVE: 'I'  TO tg_saida-status.
    ELSEIF tg_saida-status EQ icon_unlocked .
      MOVE: 'A' TO tg_saida-status.
    ENDIF.

    MOVE-CORRESPONDING: tg_saida TO tl_input_0023.
    APPEND tl_input_0023.
    "clear: tl_input_0023.

    READ TABLE tg_0023_aux INTO tg_0023_aux WITH KEY cwerks =  tl_input_0023-cwerks.
    IF sy-subrc EQ 0.

    ELSE.
      MODIFY zmm0023 FROM TABLE tl_input_0023.
    ENDIF.

  ENDLOOP.

  PERFORM seleciona_dados.
  PERFORM organiza_dados.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = wa_stable.


  IF sy-subrc IS INITIAL.
    MESSAGE s836(sd) WITH 'Os dados foram salvos!'.
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ocorreu um erro na gravação,'
                                           'verificar as entradas!'.
  ENDIF.



ENDFORM.                    " GRAVA_DADOS

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit INPUT.
  CASE ok_code.
    WHEN c_back.
      LEAVE TO SCREEN 0.
    WHEN c_cancel.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 100.
    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  ATIVA_CENTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ativa_centro .

  DATA:  tg_texto TYPE catsxt_longtext_itab.
  DATA : wa_texto TYPE LINE OF catsxt_longtext_itab.
  DATA:  wl_texto TYPE LINE OF catsxt_longtext_itab.
  DATA:  wl_display.

  DATA:ls_zmmt0017_log TYPE zmmt0017_log.

  CALL METHOD grid1->get_selected_cells
    IMPORTING
      et_cell = tg_selectedcell.

  LOOP AT tg_selectedcell INTO wg_selectedcell.
    READ TABLE tg_saida INTO tg_saida INDEX wg_selectedcell-row_id-index.

    CLEAR tg_saida-motivo.

    wl_display = ' '.

    IF tg_saida-status EQ icon_unlocked.
      tg_saida-status = icon_locked."I

      UPDATE zmm0023
      SET status = 'I'
      WHERE werks  = tg_saida-werks
        AND matnr  = tg_saida-matnr
        AND matkl  = tg_saida-matkl."SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria

      COMMIT WORK.

    ELSEIF tg_saida-status EQ icon_locked .
      tg_saida-status = icon_unlocked."A.

      UPDATE zmm0023
        SET status = 'A'
        WHERE werks  = tg_saida-werks
          AND matnr  = tg_saida-matnr
          AND matkl  = tg_saida-matkl."SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria

      COMMIT WORK.

    ENDIF.




*    ls_zmmt0017_log-matnr   = tg_saida-matnr.
*    ls_zmmt0017_log-centro_fixo = tg_saida-werks.
*    ls_zmmt0017_log-status = tg_saida-status.
*
*    MODIFY zmmt0017_log FROM ls_zmmt0017_log.



    tg_saida-usralt =  sy-uname.
    tg_saida-aedat  =  sy-datum.
    tg_saida-hralt  =  sy-uzeit.

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title        = 'Informe Motivo de Alteração'
        im_display_mode = wl_display " Somente vizualizar ou inserir
      CHANGING
        ch_text         = tg_texto.


    IF tg_texto IS NOT INITIAL.
      DATA: wa_linha TYPE cacs_char1000sf.

      CLEAR: wa_texto, wa_linha.

      " Se tiver mais de uma linha.
      LOOP AT tg_texto INTO wa_texto.
        IF sy-tabix EQ 1.
          wa_linha = wa_texto.
        ELSE.
          CONCATENATE wa_linha wa_texto
          INTO wa_linha SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      MOVE wa_linha TO tg_saida-motivo.

    ELSE.
      MESSAGE e000(z01) WITH 'Motivo deve ser informado!' .

    ENDIF.

    PERFORM get_next_number  USING  'ZCWERKS'
                                    '1'
                                    CHANGING p_seq_cwerks.

    MOVE p_seq_cwerks TO tg_saida-cwerks.


    MODIFY tg_saida FROM tg_saida INDEX wg_selectedcell-row_id-index.



    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDLOOP.


ENDFORM.                    " ATIVA_CENTRO
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_erros .
  DATA: wl_linha(6).

  REFRESH:  tg_msg_ret.
  CLEAR:    tg_msg_ret.

  IF tg_saida[] IS NOT INITIAL.

    LOOP AT tg_saida.
      wl_linha = sy-tabix.
      IF tg_saida-werks IS INITIAL.
        MOVE: 'WERKS' TO tg_msg_ret-field,
              'GRID1'  TO tg_msg_ret-obj,
              wl_linha TO tg_msg_ret-tabix.

        CONCATENATE TEXT-e01 ' CENTRO.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
*"SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria
      ELSE.
        IF tg_saida-werks IS NOT INITIAL AND tg_saida-matnr IS NOT INITIAL AND tg_saida-matkl IS NOT INITIAL.
          MOVE: 'MATNR'  TO tg_msg_ret-field,
                 'MATKL' TO tg_msg_ret-field,
                'GRID1'  TO tg_msg_ret-obj,
                wl_linha TO tg_msg_ret-tabix.

          CONCATENATE TEXT-e02 ' CENTRO OU GRP MERCADORIA.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ELSE.
        ENDIF.
*"SMC #108346 - 16-05-2024 - adicionando grupo de mercadoria
      ENDIF.

      IF tg_saida-motivo IS INITIAL.
        MOVE: 'MOTIVO' TO tg_msg_ret-field,
              'GRID1'  TO tg_msg_ret-obj,
              wl_linha TO tg_msg_ret-tabix.

        CONCATENATE TEXT-e01 ' MOTIVO.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'Z002'.
  SET TITLEBAR 'Z002'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'SAIR'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_0200 OUTPUT.
*  DATA: WL_REPID    TYPE SY-REPID,
*        TL_FUNCTION TYPE UI_FUNCTIONS,
*        WL_FUNCTION LIKE TL_FUNCTION WITH HEADER LINE.

  REFRESH tl_function .
  wl_repid = sy-repid.

  IF container2 IS INITIAL.
    wa_layout-zebra      = c_x.
    wa_layout-no_toolbar = c_x.
    wa_stable-row        = c_x.

    "wa_layout-no_rowmark = c_x.

    CREATE OBJECT container2
      EXPORTING
        repid     = wl_repid
        dynnr     = '0200'
        side      = container2->dock_at_top
        extension = 500.

    wa_layout-stylefname = 'STYLE'.

    CREATE OBJECT grid2
      EXPORTING
        i_parent = container2.



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

    PERFORM montar_layout_0200.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_saida2[].


    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  ELSE.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_0200 .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1  ' ' ' '               'TG_SAIDA' 'STATUS'     'Status'                  '06' ' ' '' '',
        2  'ZMM0023' 'USRCAD'    'TG_SAIDA'  'USRCAD'     'Cadastrado por'          '14' ' ' '' '',
        3  'ZMM0023' 'USRALT'    'TG_SAIDA2' 'USRALT'     'Alterado por'            '12' ' ' '' '',
        4  'ZMM0023' 'ERDAT'     'TG_SAIDA2' 'ERDAT'      'Data Cadastro'           '12' ' ' '' '',
        5  'ZMM0023' 'AEDAT'     'TG_SAIDA2' 'AEDAT'      'Data de Alt.'            '12' ' ' '' '',
        6  'ZMM0023' 'HRCAD'     'TG_SAIDA2' 'HRCAD'      'Hora Cad.'               '08' ' ' '' '',
        7  'ZMM0023' 'HRALT'     'TG_SAIDA2' 'HRALT'      'Hora Alt.'               '08' ' ' '' '',
        8  'ZMM0023' 'MOTIVO'    'TG_SAIDA2' 'MOTIVO'     'Motivo'                  '30' ' ' '' ''.
ENDFORM.
