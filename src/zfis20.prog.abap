*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 10/01/2013                                              &*
*& Descrição: Cadastro de Grupo de usuário Fechamento Mensal          &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         10.01.2012                            &*
*&--------------------------------------------------------------------&*

REPORT  zfis20.
TYPE-POOLS: rmdi.
INCLUDE: <icon>.
INCLUDE <cl_alv_control>.
TYPES: BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
       END OF ty_t001w,

       BEGIN OF ty_usrefus,
         bname     TYPE usrefus-bname,
         useralias TYPE usrefus-useralias,
       END OF ty_usrefus,

       BEGIN OF ty_emp,
         bukrs    TYPE zfit0033-bukrs,
         monat    TYPE zfit0033-monat,
         gjahr    TYPE zfit0033-gjahr,
         monatc   TYPE zfit0033-monat,
         gjahrc   TYPE zfit0033-gjahr,
         dep_resp TYPE zfit0033-dep_resp,
       END OF ty_emp,

       BEGIN OF ty_zfit0033_date,
         us_modif     TYPE zfit0033_log-us_modif,
         dt_modif_ini TYPE zfit0033_log-dt_modif,
         dt_modif_fim TYPE zfit0033_log-dt_modif,
       END OF ty_zfit0033_date,

       BEGIN OF ty_saida,
         mark,
         dep_resp         TYPE zimp_cad_depto-dep_resp,
         dep_resp_desc    TYPE zimp_cad_depto-dep_resp_desc,
         usnam            TYPE zfit0033-usnam,
         desc_usnam(40),
         data_lim         TYPE zfit0033-data_lim,
         hora_lim         TYPE zfit0033-hora_lim,
         dias_uteis       TYPE zfit0033-dias_uteis,
         status_contab(4),
         status_fiscal(4),
         cellcolors       TYPE lvc_t_scol,
         style2           TYPE lvc_t_styl,
       END OF ty_saida.



DATA: BEGIN OF gt_values OCCURS 0,
        domvalue_l TYPE domvalue_l,
        ddtext     TYPE val_text,
      END OF gt_values.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_x              TYPE c VALUE 'X',
           c_add(3)         TYPE c VALUE 'ADD',
           c_del(3)         TYPE c VALUE 'DEL',
           c_exit(4)        TYPE c VALUE 'EXIT',
           c_log(4)         TYPE c VALUE  'LOG',
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
DATA: tg_0033         TYPE TABLE OF zfit0033 WITH HEADER LINE,
      tg_setleaf      TYPE TABLE OF setleaf WITH HEADER LINE,
      tg_usrefus      TYPE TABLE OF ty_usrefus WITH HEADER LINE,
      tg_caddep       TYPE TABLE OF zimp_cad_depto WITH HEADER LINE,
      tg_saida        TYPE TABLE OF ty_saida WITH HEADER LINE,
      tg_saida_memory TYPE TABLE OF ty_saida WITH HEADER LINE,
      tg_saida_aux    TYPE TABLE OF ty_saida WITH HEADER LINE,
      tg_zfit0033_log TYPE TABLE OF zfit0033_log,
      ws_zfit0033_log TYPE zfit0033_log,
      ws_zfit0033     TYPE ty_zfit0033_date,
      ok_code         TYPE sy-ucomm,
      init,
      wg_display,
      x_field(30),
      wg_mensagem(30),
      wg_obj(40),
      vg_anomes(6),
      vg_anomesc(6),
      w_answer(1),
      wl_n_uteis      TYPE sy-index,
      wl_n_uteis2     TYPE sy-index,
      wl_datai        TYPE sy-datum,
      wl_dataf        TYPE sy-datum,
      tl_sab_dom_fer  TYPE TABLE OF iscal_day,
      wl_sab_dom_fer  TYPE iscal_day,
      wg_emp          TYPE ty_emp.

DATA: it_fcat         TYPE TABLE OF lvc_s_fcat,
      obj_custom_0300 TYPE REF TO cl_gui_custom_container,
      obj_alv_0300    TYPE REF TO cl_gui_alv_grid,
      gt_exc_button   TYPE ui_functions,
      tl_function     TYPE ui_functions,
      wa_layout       TYPE lvc_s_layo.


DATA: tg_msg_ret TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.
*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.

DATA: grid1                TYPE REF TO cl_gui_alv_grid,
      container1           TYPE REF TO cl_gui_custom_container,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: t_fieldcatalog  TYPE lvc_t_fcat,
      w_fieldcatalog  TYPE lvc_s_fcat,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
*      TG_FIELDCATALOG_LCTOS TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      wa_layout_0300  TYPE lvc_s_layo,
      wa_stable       TYPE lvc_s_stbl,
      wg_cell         TYPE lvc_s_cell,
      tg_cell         TYPE lvc_t_cell,
      wa_style        TYPE lvc_s_styl,
      style2          TYPE lvc_t_styl WITH HEADER LINE.

DATA: vmes(02),
      nmes     TYPE i,
      nmesc    TYPE i,
      vano(4),
      nano     TYPE i.
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
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

*    CLASS-METHODS:
*      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*                                   IMPORTING  E_ROW_ID E_COLUMN_ID.
*
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
    CLASS-METHODS:
      on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: wl_desactive.
*   Add customized toolbar buttons.
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
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
    DATA: wl_saida LIKE LINE OF tg_saida.

    CASE e_ucomm.
      WHEN c_add.
        tg_saida_aux[] = tg_saida[].
        REFRESH: tg_saida.
        LOOP AT tg_saida_aux INTO wl_saida.
          APPEND wl_saida TO tg_saida.
        ENDLOOP.
        CLEAR: wl_saida.
        MOVE icon_unlocked TO wl_saida-status_contab.
        APPEND wl_saida TO tg_saida.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      WHEN c_del.
        "LP - USER STORY 53776
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
*           TITLEBAR              = ' '
*           DIAGNOSE_OBJECT       = ' '
            text_question         = 'Deseja Realmente Excluir os Usuários Cadastrados'
            text_button_1         = 'Sim'(001)
            icon_button_1         = 'ICON_OKAY '
            text_button_2         = 'Não'(002)
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = ' '
*           USERDEFINED_F1_HELP   = ' '
            start_column          = 25
            start_row             = 6
          IMPORTING
            answer                = w_answer
*         TABLES
*           PARAMETER             =
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF w_answer = '1'.

          CALL METHOD grid1->get_selected_cells
            IMPORTING
              et_cell = tg_selectedcell.

          LOOP AT tg_selectedcell INTO wg_selectedcell.
            DELETE tg_saida INDEX wg_selectedcell-row_id-index.
          ENDLOOP.

          CALL METHOD grid1->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ENDIF.

    ENDCASE.
    PERFORM verifica_erros.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = space
        i_repid    = sy-repid
        i_popup    = 1
      IMPORTING
        e_messagem = wg_mensagem
      TABLES
        it_msgs    = tg_msg_ret.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
*    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_double_click.


  ENDMETHOD.                    "ON_DOUBLE_CLICK
  METHOD on_data_changed.
    DATA: ls_good    TYPE lvc_s_modi,
          lv_value   TYPE lvc_value,
          vl_tabix   TYPE sy-tabix,
          vl_value   TYPE lvc_value,
          wl_usrefus LIKE LINE OF tg_usrefus,
          wl_depto   TYPE zimp_cad_depto,
          wl_saida   LIKE LINE OF tg_saida,
          vgrupo     TYPE zfit0032-grupo.

*
    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'USNAM'.
      READ TABLE tg_saida INTO wl_saida INDEX ls_good-row_id.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
*
      SELECT SINGLE bname useralias
      FROM usrefus
        INTO wl_usrefus
          WHERE bname EQ lv_value.

      MOVE: wl_usrefus-useralias TO lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESC_USNAM'
          i_value     = lv_value.

      CLEAR: wl_usrefus, lv_value.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE fieldname = 'DEP_RESP'.
      READ TABLE tg_saida INTO wl_saida INDEX ls_good-row_id.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
*
      SELECT SINGLE *
      FROM zimp_cad_depto
        INTO wl_depto
          WHERE dep_resp EQ lv_value.

      MOVE: wl_depto-dep_resp_desc TO lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DEP_RESP_DESC'
          i_value     = lv_value.

      CLEAR: wl_usrefus, lv_value.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                    INTO ls_good
                                    WHERE fieldname = 'DIAS_UTEIS'.
      READ TABLE tg_saida INTO wl_saida INDEX ls_good-row_id.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
*
      IF wg_emp-gjahr IS NOT INITIAL AND
         wg_emp-monat IS NOT INITIAL.
        CONCATENATE  wg_emp-gjahr   wg_emp-monat '01' INTO wl_datai.
        CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
          EXPORTING
            i_date = wl_datai
          IMPORTING
            e_date = wl_datai.

        ADD 1 TO wl_datai. "primeiro dia do mês seguinte.

        wl_n_uteis2 =  lv_value.
        wl_dataf = wl_datai + ( wl_n_uteis2 - 1 ).
        REFRESH tl_sab_dom_fer.
        CALL FUNCTION 'HOLIDAY_GET'
          EXPORTING
            factory_calendar           = 'ZF'
            date_from                  = wl_datai
            date_to                    = wl_dataf
          TABLES
            holidays                   = tl_sab_dom_fer
          EXCEPTIONS
            factory_calendar_not_found = 1
            holiday_calendar_not_found = 2
            date_has_invalid_format    = 3
            date_inconsistency         = 4
            OTHERS                     = 5.

        DESCRIBE TABLE tl_sab_dom_fer LINES wl_n_uteis.
        ADD wl_n_uteis TO wl_dataf.
        DO.
          REFRESH tl_sab_dom_fer.
          CALL FUNCTION 'HOLIDAY_GET'
            EXPORTING
              factory_calendar           = 'ZF'
              date_from                  = wl_datai
              date_to                    = wl_dataf
            TABLES
              holidays                   = tl_sab_dom_fer
            EXCEPTIONS
              factory_calendar_not_found = 1
              holiday_calendar_not_found = 2
              date_has_invalid_format    = 3
              date_inconsistency         = 4
              OTHERS                     = 5.

          READ TABLE tl_sab_dom_fer INTO wl_sab_dom_fer
                    WITH KEY date = wl_dataf.
          IF sy-subrc NE 0. "não é feriado ou fds
            EXIT.
          ENDIF.
          ADD 1 TO wl_dataf.
        ENDDO.

        REFRESH tl_sab_dom_fer.
        CALL FUNCTION 'HOLIDAY_GET'
          EXPORTING
            factory_calendar           = 'ZF'
            date_from                  = wl_datai
            date_to                    = wl_dataf
          TABLES
            holidays                   = tl_sab_dom_fer
          EXCEPTIONS
            factory_calendar_not_found = 1
            holiday_calendar_not_found = 2
            date_has_invalid_format    = 3
            date_inconsistency         = 4
            OTHERS                     = 5.

        wl_dataf = wl_datai + ( wl_n_uteis2 - 1 ).
        DESCRIBE TABLE tl_sab_dom_fer LINES wl_n_uteis.

        ADD wl_n_uteis TO wl_dataf.

        MOVE: wl_dataf TO lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'DATA_LIM'
            i_value     = lv_value.

      ENDIF.
      CLEAR: lv_value.
    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED
  METHOD on_data_changed_finished.
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
  ENDMETHOD.                    "on_data_changed_finisheD

  METHOD on_button_click.
    DATA: wl_saida LIKE LINE OF tg_saida.
    IF es_row_no-row_id GT 0.
      READ TABLE tg_saida INTO wl_saida INDEX es_row_no-row_id.
      IF es_col_id EQ 'STATUS_CONTAB'.
        IF wl_saida-status_contab EQ icon_locked.
          MOVE icon_unlocked TO wl_saida-status_contab.
        ELSE.
          MOVE icon_locked TO wl_saida-status_contab.
        ENDIF.
        MODIFY tg_saida FROM wl_saida INDEX es_row_no-row_id TRANSPORTING status_contab.
      ENDIF.

    ENDIF.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "on_button_click

  METHOD on_onf4.
    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,     "Nome da tabela
             fieldname TYPE dd03l-fieldname,   "Nome de campo
             S(1)      TYPE c,
           END OF ty_field,

           BEGIN OF ty_value,
             tabname    TYPE dd03l-tabname,     "Nome da tabela
             fieldname  TYPE dd03l-fieldname,   "Nome de campo
             char79(79) TYPE c,
           END OF ty_value.

    DATA: BEGIN OF wl_valuetab,
            field(50),
          END OF wl_valuetab.

    DATA: tl_valuetab      LIKE TABLE OF wl_valuetab,
          tl_field         TYPE TABLE OF ty_field,
          wl_field         TYPE ty_field,
          tl_value         TYPE TABLE OF ty_value,
          wl_value         TYPE ty_value,

          tl_depto         TYPE TABLE OF zimp_cad_depto,
          wl_depto         TYPE zimp_cad_depto,

          tl_user          TYPE TABLE OF usrefus,
          wl_user          TYPE usrefus,

          wg_itens         LIKE LINE OF tg_saida,

          wl_index         TYPE sy-tabix,
          wl_char(20),
          wl_fieldname(30),
          wl_tabname(30).

    READ TABLE tg_saida INTO wg_itens INDEX es_row_no-row_id.
    CASE e_fieldname.
      WHEN 'USNAM'.
        SELECT *
          FROM usrefus
          INTO TABLE tl_user.

        CHECK tl_user IS NOT INITIAL.

        wl_fieldname  = 'BNAME'.
        wl_tabname    = 'USREFUS'.

        LOOP AT tl_user INTO wl_user.
          MOVE: wl_user-bname TO wl_valuetab-field.
          APPEND wl_valuetab TO tl_valuetab.

          MOVE: wl_user-useralias TO wl_valuetab-field.
          APPEND wl_valuetab TO tl_valuetab.

        ENDLOOP.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'BNAME'.
        wl_field-s = 'X'.
        APPEND wl_field TO tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'USERALIAS'.
        wl_field-s = ' '.
        APPEND wl_field TO tl_field.

      WHEN 'DEP_RESP'.
        SELECT * FROM zimp_cad_depto INTO TABLE tl_depto.

        CHECK tl_depto IS NOT INITIAL.

        wl_fieldname  = 'DEP_RESP'.
        wl_tabname    = 'ZIMP_CAD_DEPTO'.

        LOOP AT tl_depto INTO wl_depto.
          MOVE: wl_depto-dep_resp TO wl_valuetab-field.
          APPEND wl_valuetab TO tl_valuetab.

          MOVE: wl_depto-dep_resp_desc TO wl_valuetab-field.
          APPEND wl_valuetab TO tl_valuetab.

        ENDLOOP.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'DEP_RESP'.
        wl_field-s = 'X'.
        APPEND wl_field TO tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'DEP_RESP_DESC'.
        wl_field-s = ' '.
        APPEND wl_field TO tl_field.
    ENDCASE.

    IF    wl_fieldname  IS NOT INITIAL
      AND wl_tabname    IS NOT INITIAL
      AND tl_field[]    IS NOT INITIAL
      AND tl_valuetab[] IS NOT INITIAL.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
*         cucol                     = '3'
          fieldname                 = wl_fieldname
          tabname                   = wl_tabname
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_valuetab
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      IF sy-subrc IS INITIAL.
        CASE e_fieldname.
          WHEN 'USNAM'.
            READ TABLE tl_user INTO wl_user INDEX wl_index.
          WHEN 'DEP_RESP'.
            READ TABLE tl_depto INTO wl_depto INDEX wl_index.
        ENDCASE.

        IF es_row_no-row_id GT 0.
          READ TABLE tg_saida INTO wg_itens INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL.
            CASE e_fieldname.
              WHEN 'USNAM'.
                MOVE: wl_user-bname          TO wg_itens-usnam.
                MOVE: wl_user-useralias      TO wg_itens-desc_usnam.
              WHEN 'DEP_RESP'.
                MOVE: wl_depto-dep_resp TO wg_itens-dep_resp.
                MOVE: wl_depto-dep_resp_desc TO wg_itens-dep_resp_desc.
            ENDCASE.

            MODIFY tg_saida FROM wg_itens INDEX es_row_no-row_id.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

**** Método de atualização de dados na Tela
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                                                "on_ONF4
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
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
  PERFORM verifica_erros.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen   = '100'
      i_show     = space
      i_repid    = sy-repid
      i_popup    = 1
*     i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*     I_SET_FIELD   = 'X_FIELD'
    IMPORTING
      e_messagem = wg_mensagem
    TABLES
      it_msgs    = tg_msg_ret.

  IF wg_cell IS NOT INITIAL .
    REFRESH: tg_cell.
    APPEND wg_cell TO tg_cell.
*          CONCATENATE wl_obj '->SET_SELECTED_CELLS' INTO wg_obj.
    CALL METHOD grid1->set_selected_cells "(wg_obj)          "(wg_msgs)=>set_selected_cells
      EXPORTING
        it_cells = tg_cell[].
  ENDIF.

  SET PF-STATUS 'Z002' EXCLUDING fcode.
  SET TITLEBAR 'Z001'.
  IF sy-subrc IS INITIAL.


  ENDIF.


ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: wl_repid     TYPE sy-repid,
*        tl_function  TYPE ui_functions,
        wl_function  LIKE tl_function WITH HEADER LINE,
        lt_f4        TYPE lvc_t_f4 WITH HEADER LINE,
        gs_variant_c TYPE disvariant.

  wl_repid = sy-repid.

  IF container1 IS INITIAL.
    wa_layout-zebra      = c_x.
*    wa_layout-no_rowmark = c_x.
*    wa_layout-cwidth_opt = c_x.
    wa_stable-row        = c_x.
    wa_layout-sel_mode   = 'C'.
    wa_layout-box_fname  = 'MARK'.

    CREATE OBJECT container1
      EXPORTING
        container_name = 'CC_USER'.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container1.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*      * Register event handler
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
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
*    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    PERFORM montar_layout.

    wa_layout-ctab_fname = 'CELLCOLORS'.
    wa_layout-stylefname = 'STYLE2'.

    lt_f4-fieldname = 'DEP_RESP'.
    lt_f4-register = 'X' .
    lt_f4-getbefore = 'X' .
    APPEND lt_f4 .
*
    lt_f4-fieldname = 'USNAM'.
    lt_f4-register = 'X' .
    lt_f4-getbefore = 'X' .
    APPEND lt_f4 .

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
        i_save               = 'A'
        is_variant           = gs_variant_c
      CHANGING
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

    SET HANDLER:
              lcl_event_handler=>on_double_click          FOR grid1,
              lcl_event_handler=>on_data_changed_finished FOR grid1,
              lcl_event_handler=>on_data_changed          FOR grid1,
              lcl_event_handler=>on_button_click          FOR grid1,
              lcl_event_handler=>on_onf4                  FOR grid1.
  ELSE.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init OUTPUT.
  DATA: wl_view_name TYPE ocus-table VALUE 'ZFIT0033',
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
*       enqueue_range    = "header-subsetflag
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
*           I_START_COLUMN       = 9
*           I_START_ROW          = 9
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
*    ENDIF.

*    PERFORM SELECIONA_DADOS.
*    PERFORM ORGANIZA_DADOS.
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
FORM seleciona_dados USING tipo .
  CLEAR wg_emp-bukrs.
  IF tipo = 'N'.
    SELECT *
      FROM zfit0033
      INTO TABLE tg_0033
      WHERE bukrs = wg_emp-bukrs
      AND   monat = wg_emp-monat
      AND   gjahr = wg_emp-gjahr.
  ELSE.
    SELECT *
    FROM zfit0033
    INTO TABLE tg_0033
    WHERE bukrs = wg_emp-bukrs
    AND   monat = wg_emp-monatc
    AND   gjahr = wg_emp-gjahrc.
  ENDIF.

  IF sy-subrc IS INITIAL.
    SELECT bname useralias
      FROM usrefus
      INTO TABLE tg_usrefus
       FOR ALL ENTRIES IN tg_0033
       WHERE bname EQ tg_0033-usnam.

    SELECT *
      FROM zimp_cad_depto
      INTO TABLE tg_caddep
       FOR ALL ENTRIES IN tg_0033
       WHERE dep_resp EQ tg_0033-dep_resp.
  ENDIF.

  IF wg_emp-dep_resp IS NOT INITIAL.
    DELETE tg_0033 WHERE dep_resp NE wg_emp-dep_resp.
  ENDIF.
ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 ' '         ' '              'TG_SAIDA' 'DEP_RESP'      'Depto'            '07' 'X' ' ' ' ',
        2 'ZFIT0032T' 'DEP_RESP_DESC'  'TG_SAIDA' 'DEP_RESP_DESC' ' '                '20' ' ' ' ' ' ',
        3 ' '         ' '              'TG_SAIDA' 'USNAM'         'Usuário '         '15' 'X' '' ' ',
        4 'USREFUS'   'USERALIAS'      'TG_SAIDA' 'DESC_USNAM'    'Nome de Usuário'  ' '  ' ' ' ' ' ',
        5 ' '         ' '              'TG_SAIDA' 'DIAS_UTEIS'    'Dias Uteis'       '15' 'X' '' ' ',
        6 'ZFIT0033'  'DATA_LIM'       'TG_SAIDA' 'DATA_LIM'      'Data Limite'      '10' 'X' ' ' ' ',
        7 'ZFIT0033'  'HORA_LIM'       'TG_SAIDA' 'HORA_LIM'      'Hora Limite'      '10' 'X' ' ' ' ',
        0 ' '         ' '              'TG_SAIDA' 'STATUS_CONTAB' 'Status '          '08' ' ' ' ' ' '.


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
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

  IF p_field(6) EQ 'STATUS'.
    IF wg_display IS INITIAL.
      w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
    ELSE.
*      w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
  ENDIF.

  IF p_field EQ 'DEP_RESP' OR
     p_field EQ 'USNAM'.
    w_fieldcatalog-f4availabl = c_x.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
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
    WHEN 'COPIA'.
      IF wg_emp-monat IS INITIAL OR
         wg_emp-gjahr IS INITIAL.
        MESSAGE 'Informe os Mês/Ano Destino' TYPE 'I'.
        SET CURSOR FIELD 'WG_EMP-BUKRS'.
        EXIT.
      ENDIF.
      IF wg_emp-monatc IS INITIAL OR
         wg_emp-gjahrc IS INITIAL.
        MESSAGE 'Informe os Mês/Ano Origem' TYPE 'I'.
        SET CURSOR FIELD 'WG_EMP-BUKRS'.
        EXIT.
      ENDIF.
      CONCATENATE wg_emp-gjahr wg_emp-monat INTO vg_anomes. "DATA DESTINO
      " vg_anomesc = sy-datum+0(6). "DATA ATUAL US: 82268
      CONCATENATE wg_emp-gjahrc wg_emp-monatc INTO vg_anomesc.  "dATA oRIGEM


      "  IF vg_anomes LT vg_anomesc.
      IF vg_anomes GT vg_anomesc.
        ""USER STORY 53776*
**        MESSAGE 'Mês/Ano Destino está no passado, não copiar' TYPE 'I'.
**        SET CURSOR FIELD 'WG_EMP-MONAT'.
**        EXIT.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
*           TITLEBAR              = ' '
*           DIAGNOSE_OBJECT       = ' '
            text_question         = 'Copiar Mês/Ano Destino do passado? '
            text_button_1         = 'Sim'(001)
            icon_button_1         = 'ICON_OKAY '
            text_button_2         = 'Não'(002)
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = ' '
*           USERDEFINED_F1_HELP   = ' '
            start_column          = 25
            start_row             = 6
          IMPORTING
            answer                = w_answer
*         TABLES
*           PARAMETER             =
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF w_answer = '2 '.
          EXIT.

          "ELSE.

        ENDIF.
      ENDIF.

      PERFORM seleciona_dados USING 'N'.
      IF tg_0033[] IS NOT INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
*           TITLEBAR              = ' '
*           DIAGNOSE_OBJECT       = ' '
            text_question         = 'Já existem dados neste mês, confirma a CÓPIA?'
            text_button_1         = 'Sim'(001)
            icon_button_1         = 'ICON_OKAY '
            text_button_2         = 'Não'(002)
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = ' '
*           USERDEFINED_F1_HELP   = ' '
            start_column          = 25
            start_row             = 6
          IMPORTING
            answer                = w_answer
*         TABLES
*           PARAMETER             =
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        IF w_answer = '1'.
          PERFORM seleciona_dados USING 'C'.
          PERFORM organiza_dados USING 'C'.
          PERFORM grava_dados.
        ENDIF.
      ELSE.
        PERFORM seleciona_dados USING 'C'.
        PERFORM organiza_dados USING 'C'.
        PERFORM grava_dados.
      ENDIF.
      CLEAR: wg_emp-monatc,
             wg_emp-gjahrc.
      " ENDIF.  "  USER STORY 53776*
      " ENDIF.
    WHEN c_save.
      CALL METHOD grid1->check_changed_data.
      PERFORM verifica_erros.
      IF tg_msg_ret[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen   = '100'
            i_show     = c_x
            i_repid    = sy-repid
            i_popup    = 1
            i_set_cell = 'WG_CELL'
            i_set_obj  = 'WL_OBJ'
*           i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*           I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            e_messagem = wg_mensagem
          TABLES
            it_msgs    = tg_msg_ret.
      ELSE.
        PERFORM grava_dados.
      ENDIF.
    WHEN c_search.
      IF  wg_emp-monat IS NOT INITIAL AND
          wg_emp-gjahr IS NOT INITIAL.
        PERFORM seleciona_dados USING 'N'.
        PERFORM organiza_dados USING 'N'.
      ENDIF.

    WHEN c_show_msgre.
      PERFORM verifica_erros.
      IF tg_msg_ret[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen   = '100'
            i_show     = c_x
            i_repid    = sy-repid
            i_popup    = 1
            i_set_cell = 'WG_CELL'
            i_set_obj  = 'WL_OBJ'
*           i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*           I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            e_messagem = wg_mensagem
          TABLES
            it_msgs    = tg_msg_ret.
      ENDIF.

    WHEN c_back.
      LEAVE TO SCREEN 0.
    WHEN c_cancel.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 100.
    WHEN c_log.
      CALL SCREEN 0200 STARTING AT 5 5.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organiza_dados USING tipo .
  DATA: ls_cellcolor TYPE lvc_s_scol.


  SORT: tg_usrefus BY bname,
        tg_caddep  BY dep_resp.

  REFRESH tg_saida.
  LOOP AT tg_0033.
    READ TABLE tg_usrefus
      WITH KEY bname = tg_0033-usnam
               BINARY SEARCH.

    READ TABLE tg_caddep
          WITH KEY dep_resp = tg_0033-dep_resp
                   BINARY SEARCH.

    IF tipo = 'C'.
      CONCATENATE  wg_emp-gjahr   wg_emp-monat '01' INTO wl_datai.
      CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
        EXPORTING
          i_date = wl_datai
        IMPORTING
          e_date = wl_datai.

      ADD 1 TO wl_datai. "primeiro dia do mês seguinte.

      wl_n_uteis2 =  tg_0033-dias_uteis.
      wl_dataf = wl_datai + ( wl_n_uteis2 - 1 ).
      REFRESH tl_sab_dom_fer.
      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          factory_calendar           = 'ZF'
          date_from                  = wl_datai
          date_to                    = wl_dataf
        TABLES
          holidays                   = tl_sab_dom_fer
        EXCEPTIONS
          factory_calendar_not_found = 1
          holiday_calendar_not_found = 2
          date_has_invalid_format    = 3
          date_inconsistency         = 4
          OTHERS                     = 5.

      DESCRIBE TABLE tl_sab_dom_fer LINES wl_n_uteis.
      ADD wl_n_uteis TO wl_dataf.
      DO.
        REFRESH tl_sab_dom_fer.
        CALL FUNCTION 'HOLIDAY_GET'
          EXPORTING
            factory_calendar           = 'ZF'
            date_from                  = wl_datai
            date_to                    = wl_dataf
          TABLES
            holidays                   = tl_sab_dom_fer
          EXCEPTIONS
            factory_calendar_not_found = 1
            holiday_calendar_not_found = 2
            date_has_invalid_format    = 3
            date_inconsistency         = 4
            OTHERS                     = 5.

        READ TABLE tl_sab_dom_fer INTO wl_sab_dom_fer
                  WITH KEY date = wl_dataf.
        IF sy-subrc NE 0. "não é feriado ou fds
          EXIT.
        ENDIF.
        ADD 1 TO wl_dataf.
      ENDDO.

      REFRESH tl_sab_dom_fer.
      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          factory_calendar           = 'ZF'
          date_from                  = wl_datai
          date_to                    = wl_dataf
        TABLES
          holidays                   = tl_sab_dom_fer
        EXCEPTIONS
          factory_calendar_not_found = 1
          holiday_calendar_not_found = 2
          date_has_invalid_format    = 3
          date_inconsistency         = 4
          OTHERS                     = 5.

      wl_dataf = wl_datai + ( wl_n_uteis2 - 1 ).
      DESCRIBE TABLE tl_sab_dom_fer LINES wl_n_uteis.

      ADD wl_n_uteis TO wl_dataf.
      tg_0033-data_lim = wl_dataf.
    ENDIF.
    "
    MOVE: tg_0033-dep_resp        TO tg_saida-dep_resp,
          tg_caddep-dep_resp_desc TO tg_saida-dep_resp_desc,
          tg_0033-usnam           TO tg_saida-usnam,
          tg_usrefus-useralias    TO tg_saida-desc_usnam,
          tg_0033-dias_uteis      TO tg_saida-dias_uteis,
          tg_0033-data_lim        TO tg_saida-data_lim,
          tg_0033-hora_lim        TO tg_saida-hora_lim.


    IF tg_0033-lib_contab NE c_x.
      MOVE: icon_locked      TO  tg_saida-status_contab.
    ELSE.
      MOVE: icon_unlocked    TO  tg_saida-status_contab.
    ENDIF.

    IF tg_0033-lib_fiscal NE c_x.
      MOVE: icon_locked      TO  tg_saida-status_fiscal.
    ELSE.
      MOVE: icon_unlocked    TO  tg_saida-status_fiscal.
    ENDIF.

    APPEND tg_saida.
    CLEAR: tg_saida, tg_0033, tg_usrefus.

  ENDLOOP.

  FREE: tg_saida_memory.
  tg_saida_memory[] = tg_saida[].

ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .
  DATA: tl_input_0033 TYPE TABLE OF zfit0033 WITH HEADER LINE.

  CLEAR: tl_input_0033.
  REFRESH: tl_input_0033.
  CLEAR wg_emp-bukrs.

  IF wg_emp-dep_resp IS NOT INITIAL.
    DELETE FROM zfit0033
               WHERE monat = wg_emp-monat
               AND   gjahr = wg_emp-gjahr
               AND   dep_resp = wg_emp-dep_resp.
  ELSE.
    DELETE FROM zfit0033
           WHERE monat = wg_emp-monat
           AND   gjahr = wg_emp-gjahr.
  ENDIF.

  DELETE tg_saida WHERE dep_resp  IS INITIAL
                    AND usnam     IS INITIAL
                    AND data_lim  IS INITIAL
                    AND hora_lim  IS INITIAL.

  LOOP AT tg_saida.
    MOVE-CORRESPONDING: tg_saida TO tl_input_0033.
    tl_input_0033-bukrs = wg_emp-bukrs.
    tl_input_0033-monat = wg_emp-monat.
    tl_input_0033-gjahr = wg_emp-gjahr.
    tl_input_0033-us_modif = sy-uname.
    tl_input_0033-dt_modif = sy-datum.
    tl_input_0033-hr_modif = sy-uzeit.

    IF tg_saida-status_contab EQ icon_locked.
      MOVE : space TO tl_input_0033-lib_contab.
    ELSE.
      MOVE : c_x TO tl_input_0033-lib_contab.
    ENDIF.

    IF tg_saida-status_fiscal EQ icon_locked.
      MOVE : space TO tl_input_0033-lib_fiscal.
    ELSE.
      MOVE : c_x TO tl_input_0033-lib_fiscal.
    ENDIF.

    APPEND tl_input_0033.
    CLEAR: tl_input_0033.

  ENDLOOP.

  MODIFY zfit0033 FROM TABLE tl_input_0033.

  IF sy-subrc IS INITIAL.
    MESSAGE s836(sd) WITH 'Os dados foram salvos!'.
    "   REFRESH TG_SAIDA.
    CLEAR: wg_emp-monatc, wg_emp-gjahrc.

    PERFORM fm_grava_log USING tl_input_0033.


  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ocorreu um erro na gravação,'
                                           'verificar as entradas!'.
  ENDIF.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_erros .
  DATA: tl_usrefus  LIKE TABLE OF tg_usrefus,
        wl_usrefus  LIKE LINE OF tg_usrefus,
        tl_depto    TYPE TABLE OF zimp_cad_depto,
        wl_depto    TYPE zimp_cad_depto,
        tl_t001     TYPE TABLE OF t001,
        tl_cell     TYPE lvc_t_cell,
        wl_linha(6).

  REFRESH: tl_usrefus, tg_msg_ret.
  CLEAR: wl_usrefus, tg_msg_ret.

  IF wg_emp-monat IS INITIAL.
    MOVE: 'MONAT' TO tg_msg_ret-field.
    CONCATENATE text-e01 ' Mês'  INTO  tg_msg_ret-msg.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  IF wg_emp-gjahr IS INITIAL.
    MOVE: 'GJAHR' TO tg_msg_ret-field.
    CONCATENATE text-e01 ' Ano'  INTO  tg_msg_ret-msg.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  IF tg_saida[] IS NOT INITIAL.
    SELECT bname useralias
      FROM usrefus
      INTO TABLE tl_usrefus
       FOR ALL ENTRIES IN tg_saida
       WHERE bname EQ tg_saida-usnam.

    SELECT * FROM zimp_cad_depto
      INTO TABLE tl_depto
     FOR ALL ENTRIES IN tg_saida
       WHERE dep_resp EQ tg_saida-dep_resp.
  ENDIF.

  SORT: tl_usrefus BY bname.

  LOOP AT tg_saida.
    wl_linha = sy-tabix.
    IF wg_emp-dep_resp IS NOT INITIAL.
      IF tg_saida-dep_resp NE wg_emp-dep_resp.
        MOVE: 'DEP_RESP' TO tg_msg_ret-field,
          'GRID1'    TO tg_msg_ret-obj,
          wl_linha   TO tg_msg_ret-tabix.

        CONCATENATE text-e11 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.
    IF tg_saida-dep_resp IS INITIAL.
      MOVE: 'DEP_RESP' TO tg_msg_ret-field,
            'GRID1'    TO tg_msg_ret-obj,
            wl_linha   TO tg_msg_ret-tabix.

      CONCATENATE text-e01 ' Depto' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      READ TABLE tl_depto INTO wl_depto
        WITH KEY dep_resp = tg_saida-dep_resp.
      IF sy-subrc IS NOT INITIAL.
        MOVE: 'DEP_RESP' TO tg_msg_ret-field,
              'GRID1'    TO tg_msg_ret-obj,
              wl_linha TO tg_msg_ret-tabix.

        CONCATENATE text-e08 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF tg_saida-usnam IS INITIAL.
      MOVE: 'USNAM' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE text-e01 ' Usuário' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      READ TABLE tl_usrefus TRANSPORTING NO FIELDS
        WITH KEY bname = tg_saida-usnam
                 BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        MOVE: 'USNAM' TO tg_msg_ret-field,
              'GRID1'  TO tg_msg_ret-obj,
              wl_linha TO tg_msg_ret-tabix.

        CONCATENATE text-e09 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF tg_saida-data_lim IS INITIAL.
      MOVE: 'DATA_LIM' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE text-e01 ' DATA_LIM.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF tg_saida-hora_lim IS INITIAL.
      MOVE: 'HORA_LIM' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE text-e01 ' HORA_LIM.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    nmes  = wg_emp-monat.
    nmesc = tg_saida-data_lim+4(2).


*    IF NMESC LE NMES.
*      MOVE: 'DATA_LIM' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E10 ' DATA_LIM.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
  ENDLOOP.

ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  GET_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_CELLS  text
*----------------------------------------------------------------------*
FORM get_cell TABLES  tl_cell TYPE lvc_t_cell
              USING wl_cell
                    wl_tabix.
*  REFRESH: tl_cell.
*  MOVE : wl_cell  TO tl_cell-col_id-fieldname,
*         wl_tabix TO tl_cell-row_id-index.
*
*  APPEND
*  call method grid1->set_selected_cells
*    exporting
*      it_cell = tl_cell[].

ENDFORM.                    " GET_CELL
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST0200'.
  SET TITLEBAR 'T0200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'BT_CONF'.

      PERFORM fm_selec_log.
    WHEN 'BT_CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_SELEC_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_selec_log .

  DATA: rg_date TYPE RANGE OF zfit0033_log-dt_modif.

  FREE: tg_zfit0033_log.

  IF ws_zfit0033-dt_modif_ini IS INITIAL.
    MESSAGE 'Preencha a data inicial' TYPE 'I'.
    EXIT.
  ENDIF.



  rg_date = VALUE #( ( sign = 'I' option = 'BT' low = ws_zfit0033-dt_modif_ini high = ws_zfit0033-dt_modif_fim ) ).


  "Seleção de log com base no periodo.

  SELECT * FROM zfit0033_log INTO TABLE tg_zfit0033_log WHERE dt_modif IN rg_date.
  ws_zfit0033-us_modif = sy-uname.
  SORT tg_zfit0033_log DESCENDING BY dt_modif hr_modif.
*  IF sy-subrc EQ 0.
  CALL SCREEN 0300  STARTING AT 10 10
                      ENDING   AT 160 70.

  LEAVE TO SCREEN 0.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'ST0300'.
  SET TITLEBAR 'TIT0300'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_0300 OUTPUT.

  REFRESH it_fcat.
  PERFORM fm_mont_layout_0300.

  IF ( obj_custom_0300 IS INITIAL ).
    CREATE OBJECT obj_custom_0300
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0300
      EXPORTING
        i_parent          = obj_custom_0300
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

*  wa_layout-grid_title = 'Log modificação'.
  wa_layout-cwidth_opt = 'X'.

  "Registra os eventos a serem utilizados
*  SET HANDLER:
*      lcl_event_handler=>on_data_changed  FOR obj_alv_0300,
*      lcl_event_handler=>on_double_click  FOR obj_alv_0300,
*      lcl_event_handler=>set_toolbar      FOR obj_alv_0300,
*      lcl_event_handler=>get_ucomm        FOR obj_alv_0300.

*  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL  TO GT_EXC_BUTTON.

*  PERFORM exclude_tb_functions CHANGING gt_exc_button.




  CALL METHOD obj_alv_0300->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      it_toolbar_excluding          = gt_exc_button
      i_save                        = 'A'
    CHANGING
      it_fieldcatalog               = it_fcat
      it_outtab                     = tg_zfit0033_log
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_alv_0300->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0300->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_MONT_LAYOUT_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_mont_layout_0300 .

  PERFORM alv_preenche_cat USING:
       'MONAT          ' 'Mês do exercício    ' '10'  ''  ''  ''  '' '' '' '' '' '',
       'GJAHR          ' 'Exercício           ' '10'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'USNAM          ' 'Nome do usuário     ' '40'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'CAMPO_MOD      ' 'Campo modificado    ' '40'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'DESC_CAMPO_MOD ' 'Descrição do campo  ' '40'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'VALOR_ORI      ' 'Valor original      ' '20'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'VALOR_NOVO     ' 'Valor novo          ' '20'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'US_MODIF       ' 'Usuario modificação ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'DT_MODIF       ' 'Data de modificação ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'HR_MODIF       ' 'Hora de modificação ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'TIPO           ' 'Tipo ação           ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM alv_preenche_cat    USING: p_campo         TYPE c
                                p_desc          TYPE c
                                p_tam           TYPE c
                                p_hot           TYPE c
                                p_zero          TYPE c
                                p_sum           TYPE c
                                p_edit          TYPE c
                                p_check         TYPE c
                                p_ref_tabname   LIKE dd02d-tabname
                                p_ref_fieldname LIKE dd03d-fieldname
                                p_tabname       LIKE dd02d-tabname
                                p_no_out        TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat.
  CLEAR: wa_layout, wl_fcat.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-edit      = p_edit.
  wl_fcat-checkbox  = p_check.
  wl_fcat-ref_table = p_ref_tabname.
  wl_fcat-ref_field = p_ref_fieldname.
  wl_fcat-tabname   = p_ref_tabname.
  wl_fcat-no_out    = p_no_out.
  APPEND wl_fcat TO it_fcat.
ENDFORM.                    "ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  FM_GRAVA_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_INPUT_0033  text
*----------------------------------------------------------------------*
FORM fm_grava_log  USING    it_zfit0033 LIKE tg_0033.

  FREE: tg_zfit0033_log.

  IF tg_saida[] IS NOT INITIAL.
    LOOP AT tg_saida_memory INTO DATA(ws_saida_memory).
      READ TABLE tg_saida INTO DATA(ws_saida) WITH KEY usnam = ws_saida_memory-usnam.
      IF sy-subrc EQ 0. "Achou então verifica se houve alteração.

        "dep_resp
        IF ws_saida-dep_resp NE ws_saida_memory-dep_resp.
          ws_zfit0033_log-monat             = wg_emp-monat.
          ws_zfit0033_log-gjahr             = wg_emp-gjahr.
          ws_zfit0033_log-usnam             = ws_saida_memory-usnam.
          ws_zfit0033_log-campo_mod         = 'DEP_RESP'.
          ws_zfit0033_log-desc_campo_mod    = 'Departamento Responsavel'.

          ws_zfit0033_log-valor_ori         = ws_saida_memory-dep_resp.
          ws_zfit0033_log-valor_novo        = ws_saida-dep_resp.
          ws_zfit0033_log-us_modif          = sy-uname.
          ws_zfit0033_log-dt_modif          = sy-datum.
          ws_zfit0033_log-hr_modif          = sy-uzeit.
          ws_zfit0033_log-tipo              = 'ALTERAÇÃO'.

          IF ws_zfit0033_log IS NOT INITIAL.
            APPEND ws_zfit0033_log TO tg_zfit0033_log.
            CLEAR: ws_zfit0033_log.
          ENDIF.
        ENDIF.

        "data_lim
        IF ws_saida-data_lim NE ws_saida_memory-data_lim.
          ws_zfit0033_log-monat             = wg_emp-monat.
          ws_zfit0033_log-gjahr             = wg_emp-gjahr.
          ws_zfit0033_log-usnam             = ws_saida_memory-usnam.
          ws_zfit0033_log-campo_mod         = 'DATA_LIM'.
          ws_zfit0033_log-desc_campo_mod    = 'Data final'.

          ws_zfit0033_log-valor_ori         = |{ ws_saida_memory-data_lim+6(2) }.{ ws_saida_memory-data_lim+4(2) }.{ ws_saida_memory-data_lim(4) }|.
          ws_zfit0033_log-valor_novo        = |{ ws_saida-data_lim+6(2) }.{ ws_saida-data_lim+4(2) }.{ ws_saida-data_lim(4) }|.
          ws_zfit0033_log-us_modif          = sy-uname.
          ws_zfit0033_log-dt_modif          = sy-datum.
          ws_zfit0033_log-hr_modif          = sy-uzeit.
          ws_zfit0033_log-tipo              = 'ALTERAÇÃO'.

          IF ws_zfit0033_log IS NOT INITIAL.
            APPEND ws_zfit0033_log TO tg_zfit0033_log.
            CLEAR: ws_zfit0033_log.
          ENDIF.
        ENDIF.

        "hora_lim
        IF ws_saida-hora_lim NE ws_saida_memory-hora_lim.
          ws_zfit0033_log-monat             = wg_emp-monat.
          ws_zfit0033_log-gjahr             = wg_emp-gjahr.
          ws_zfit0033_log-usnam             = ws_saida_memory-usnam.
          ws_zfit0033_log-campo_mod         = 'HORA_LIM'.
          ws_zfit0033_log-desc_campo_mod    = 'Hora final'.

          ws_zfit0033_log-valor_ori         = |{ ws_saida_memory-hora_lim(2) }:{ ws_saida_memory-hora_lim+2(2) }:{ ws_saida_memory-hora_lim+4(2) }|.
          ws_zfit0033_log-valor_novo        = |{ ws_saida-hora_lim(2) }:{ ws_saida-hora_lim+2(2) }:{ ws_saida-hora_lim+4(2) }|.
          ws_zfit0033_log-us_modif          = sy-uname.
          ws_zfit0033_log-dt_modif          = sy-datum.
          ws_zfit0033_log-hr_modif          = sy-uzeit.
          ws_zfit0033_log-tipo              = 'ALTERAÇÃO'.

          IF ws_zfit0033_log IS NOT INITIAL.
            APPEND ws_zfit0033_log TO tg_zfit0033_log.
            CLEAR: ws_zfit0033_log.
          ENDIF.
        ENDIF.

        "dias_uteis
        IF ws_saida-dias_uteis NE ws_saida_memory-dias_uteis.
          ws_zfit0033_log-monat             = wg_emp-monat.
          ws_zfit0033_log-gjahr             = wg_emp-gjahr.
          ws_zfit0033_log-usnam             = ws_saida_memory-usnam.
          ws_zfit0033_log-campo_mod         = 'DIAS_UTEIS'.
          ws_zfit0033_log-desc_campo_mod    = 'Dias liberado'.

          ws_zfit0033_log-valor_ori         = ws_saida_memory-dias_uteis.
          ws_zfit0033_log-valor_novo        = ws_saida-dias_uteis.
          ws_zfit0033_log-us_modif          = sy-uname.
          ws_zfit0033_log-dt_modif          = sy-datum.
          ws_zfit0033_log-hr_modif          = sy-uzeit.
          ws_zfit0033_log-tipo              = 'ALTERAÇÃO'.

          IF ws_zfit0033_log IS NOT INITIAL.
            APPEND ws_zfit0033_log TO tg_zfit0033_log.
            CLEAR: ws_zfit0033_log.
          ENDIF.
        ENDIF.

      ELSE.
        "Verifica se foi excluido.
        IF ws_saida_memory-usnam IS NOT INITIAL.
          "dep_resp
          APPEND VALUE #(
          monat             = wg_emp-monat
          gjahr             = wg_emp-gjahr
          usnam             = ws_saida_memory-usnam
          campo_mod         = 'DEP_RESP'
          desc_campo_mod    = 'Departamento Responsavel'
          valor_ori        = ws_saida_memory-dep_resp
          us_modif          = sy-uname
          dt_modif          = sy-datum
          hr_modif          = sy-uzeit
          tipo              = 'EXCLUSÃO' ) TO tg_zfit0033_log.

          "data_lim
          APPEND VALUE #(
          monat             = wg_emp-monat
          gjahr             = wg_emp-gjahr
          usnam             = ws_saida_memory-usnam
          campo_mod         = 'DATA_LIM'
          desc_campo_mod    = 'Data final'
          valor_ori         = |{ ws_saida_memory-data_lim+6(2) }.{ ws_saida_memory-data_lim+4(2) }.{ ws_saida_memory-data_lim(4) }|
          us_modif          = sy-uname
          dt_modif          = sy-datum
          hr_modif          = sy-uzeit
          tipo              = 'EXCLUSÃO' ) TO tg_zfit0033_log.

          "hora_lim
          APPEND VALUE #(
          monat             = wg_emp-monat
          gjahr             = wg_emp-gjahr
          usnam             = ws_saida_memory-usnam
          campo_mod         = 'HORA_LIM'
          desc_campo_mod    = 'Hora final'
          valor_ori         = |{ ws_saida_memory-hora_lim(2) }:{ ws_saida_memory-hora_lim+2(2) }:{ ws_saida_memory-hora_lim+4(2) }|
          us_modif          = sy-uname
          dt_modif          = sy-datum
          hr_modif          = sy-uzeit
          tipo              = 'EXCLUSÃO' ) TO tg_zfit0033_log.

          "dias_uteis
          APPEND VALUE #(
          monat             = wg_emp-monat
          gjahr             = wg_emp-gjahr
          usnam             = ws_saida_memory-usnam
          campo_mod         = 'DIAS_UTEIS'
          desc_campo_mod    = 'Dias liberados'
          valor_ori         = ws_saida_memory-dias_uteis
          us_modif          = sy-uname
          dt_modif          = sy-datum
          hr_modif          = sy-uzeit
          tipo              = 'EXCLUSÃO' ) TO tg_zfit0033_log.

        ELSE. "Senão achar então é novo.

          "dep_resp
          APPEND VALUE #(
          monat             = wg_emp-monat
          gjahr             = wg_emp-gjahr
          usnam             = ws_saida-usnam
          campo_mod         = 'DEP_RESP'
          desc_campo_mod    = 'Departamento Responsavel'
          valor_novo        = ws_saida-dep_resp
          us_modif          = sy-uname
          dt_modif          = sy-datum
          hr_modif          = sy-uzeit
          tipo              = 'NOVO' ) TO tg_zfit0033_log.

          "data_lim
          APPEND VALUE #(
          monat             = wg_emp-monat
          gjahr             = wg_emp-gjahr
          usnam             = ws_saida-usnam
          campo_mod         = 'DATA_LIM'
          desc_campo_mod    = 'Data final'
          valor_novo        = ws_saida-data_lim
          us_modif          = sy-uname
          dt_modif          = sy-datum
          hr_modif          = sy-uzeit
          tipo              = 'NOVO' ) TO tg_zfit0033_log.

          "hora_lim
          APPEND VALUE #(
          monat             = wg_emp-monat
          gjahr             = wg_emp-gjahr
          usnam             = ws_saida-usnam
          campo_mod         = 'HORA_LIM'
          desc_campo_mod    = 'Hora final'
          valor_novo        = ws_saida-hora_lim
          us_modif          = sy-uname
          dt_modif          = sy-datum
          hr_modif          = sy-uzeit
          tipo              = 'NOVO' ) TO tg_zfit0033_log.

          "dias_uteis
          APPEND VALUE #(
          monat             = wg_emp-monat
          gjahr             = wg_emp-gjahr
          usnam             = ws_saida-usnam
          campo_mod         = 'DIAS_UTEIS'
          desc_campo_mod    = 'Dias liberados'
          valor_novo        = ws_saida-dias_uteis
          us_modif          = sy-uname
          dt_modif          = sy-datum
          hr_modif          = sy-uzeit
          tipo              = 'NOVO' ) TO tg_zfit0033_log.
        ENDIF.
      ENDIF.

      CLEAR: ws_zfit0033_log, ws_saida_memory, ws_saida.
    ENDLOOP.

  ELSE. "Verifica se existe dados na memoria se exitir é porque os dados foram excluidos.

    CLEAR: ws_saida_memory, ws_saida.
    LOOP AT tg_saida_memory INTO ws_saida_memory.

      "dep_resp
      APPEND VALUE #(
      monat             = wg_emp-monat
      gjahr             = wg_emp-gjahr
      usnam             = ws_saida_memory-usnam
      campo_mod         = 'DEP_RESP'
      desc_campo_mod    = 'Departamento Responsavel'
      valor_ori        = ws_saida_memory-dep_resp
      us_modif          = sy-uname
      dt_modif          = sy-datum
      hr_modif          = sy-uzeit
      tipo              = 'EXCLUSÃO' ) TO tg_zfit0033_log.

      "data_lim
      APPEND VALUE #(
      monat             = wg_emp-monat
      gjahr             = wg_emp-gjahr
      usnam             = ws_saida_memory-usnam
      campo_mod         = 'DATA_LIM'
      desc_campo_mod    = 'Data final'
      valor_ori         = |{ ws_saida_memory-data_lim+6(2) }.{ ws_saida_memory-data_lim+4(2) }.{ ws_saida_memory-data_lim(4) }|
      us_modif          = sy-uname
      dt_modif          = sy-datum
      hr_modif          = sy-uzeit
      tipo              = 'EXCLUSÃO' ) TO tg_zfit0033_log.

      "hora_lim
      APPEND VALUE #(
      monat             = wg_emp-monat
      gjahr             = wg_emp-gjahr
      usnam             = ws_saida_memory-usnam
      campo_mod         = 'HORA_LIM'
      desc_campo_mod    = 'Hora final'
      valor_ori         = |{ ws_saida_memory-hora_lim(2) }:{ ws_saida_memory-hora_lim+2(2) }:{ ws_saida_memory-hora_lim+4(2) }|
      us_modif          = sy-uname
      dt_modif          = sy-datum
      hr_modif          = sy-uzeit
      tipo              = 'EXCLUSÃO' ) TO tg_zfit0033_log.

      "dias_uteis
      APPEND VALUE #(
      monat             = wg_emp-monat
      gjahr             = wg_emp-gjahr
      usnam             = ws_saida_memory-usnam
      campo_mod         = 'DIAS_UTEIS'
      desc_campo_mod    = 'Dias liberados'
      valor_ori         = ws_saida_memory-dias_uteis
      us_modif          = sy-uname
      dt_modif          = sy-datum
      hr_modif          = sy-uzeit
      tipo              = 'EXCLUSÃO' ) TO tg_zfit0033_log.

      CLEAR: ws_zfit0033_log, ws_saida_memory, ws_saida.
    ENDLOOP.
  ENDIF.

  "Save log.
  IF tg_zfit0033_log IS NOT INITIAL.
    MODIFY zfit0033_log FROM TABLE tg_zfit0033_log.
    COMMIT WORK.

  ENDIF.

  "Pegar o valor atualizado e passar para memoria.
  FREE: tg_saida_memory.
  tg_saida_memory[] = tg_saida[].
ENDFORM.
