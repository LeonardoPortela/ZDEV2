REPORT zmmr0010_v2 NO STANDARD PAGE HEADING MESSAGE-ID sd.

*----------------------------------------------------------------------*
*                                Type Pools                            *
*----------------------------------------------------------------------*
TYPE-POOLS icon.

TABLES: mchb, zppt0002, sscrfields..

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_saida,
         matnr             TYPE mchb-matnr,
         werks             TYPE mchb-werks,
         lgort             TYPE mchb-lgort,
         charg             TYPE mchb-charg,
         clabs             TYPE mchb-clabs,
         cspem             TYPE mchb-cspem,
         maktx             TYPE makt-maktx,
         lgortr            TYPE mchb-lgort,
         chargr            TYPE mchb-charg,
         matnc             TYPE mchb-matnr,
         nr_fardo_completo TYPE zsdt0330-acharg,
         cd_sai            TYPE zppt0002-cd_sai,
         icon              TYPE char05,
         log               TYPE char40,
         possuiacts        TYPE string,
         status            TYPE string,
       END   OF ty_saida,

       BEGIN OF type_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END   OF type_makt,

       BEGIN OF type_msn,
         tp_msn   TYPE bapi_mtype,
         doc_mat  TYPE bapi2017_gm_head_ret-mat_doc,
         ano      TYPE bapi2017_gm_head_ret-doc_year,
         lote     TYPE charg_d,
         messagem TYPE bapi_msg,
       END   OF type_msn.

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_mb1b    TYPE char4  VALUE 'MB1B',
           c_estorno TYPE c LENGTH 50  VALUE 'ESTORNO',
           c_table   TYPE char10 VALUE 'GIT_SAIDA',
           c_x       TYPE char1  VALUE 'X',
           c_p       TYPE char1  VALUE 'P'.

**----------------------------------------------------------------------*
**                               Classes                                *
**----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA s_event TYPE REF TO lcl_event_receiver.
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION                            *
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor
        IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_event_receiver DEFINITION INHERITING FROM cl_gui_alv_grid..
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING
                    i_parent TYPE REF TO cl_gui_custom_container,

      set_delay_time,
      on_rows_selection FOR EVENT delayed_changed_sel_callback
        OF cl_gui_alv_grid.


ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION                        *
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.

  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM f_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM f_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        i_parent = i_parent.
    SET HANDLER me->on_rows_selection FOR ALL INSTANCES.
*    SET HANDLER ME->ZM_HANDLE_TOOLBAR FOR ALL INSTANCES.
  ENDMETHOD. "constructor

  METHOD set_delay_time.
    CALL METHOD me->set_delay_change_selection( 1 ).
  ENDMETHOD. "set_delay_time

  METHOD on_rows_selection.
    PERFORM f_do_sum.
  ENDMETHOD.                    "on_rows_selection

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: git_saida        TYPE TABLE OF ty_saida,
      gwa_makt         TYPE type_makt,
      git_fardos_trace TYPE zpps0007_t,
      git_fcat         TYPE TABLE OF lvc_s_fcat,
      git_tool         TYPE ui_functions,
      git_msn_log      TYPE TABLE OF type_msn,
      gob_cont         TYPE REF TO cl_gui_custom_container,
      gob_alv          TYPE REF TO lcl_event_receiver, "CL_GUI_ALV_GRID,
      gob_toolbar      TYPE REF TO lcl_alv_toolbar,
      gwa_layout       TYPE lvc_s_layo,
      gwa_sel_button   TYPE smp_dyntxt.

DATA: gva_msg_aux    TYPE string.
DATA: gva_charg_proc TYPE charg_d.
DATA: gva_sucesso    TYPE c.
DATA: gva_error      TYPE c.


*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-035.
  PARAMETERS    : p_normal RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND us1,
                  p_storno RADIOBUTTON GROUP g1.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
    s_matnr  FOR mchb-matnr NO INTERVALS NO-EXTENSION,
    s_werks  FOR mchb-werks NO INTERVALS NO-EXTENSION,
    s_lgorta FOR mchb-lgort NO INTERVALS NO-EXTENSION,
    s_lgortr FOR mchb-lgort NO INTERVALS NO-EXTENSION MODIF ID bbb,
    s_charg  FOR mchb-charg NO INTERVALS NO-EXTENSION MODIF ID aaa,
    p_cdsai  FOR zppt0002-cd_sai NO INTERVALS NO-EXTENSION MODIF ID aaa.
SELECTION-SCREEN END   OF BLOCK a1.

SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'

INITIALIZATION.

  gwa_sel_button-icon_id   = ICON_TRANSPORT.
  gwa_sel_button-icon_text = 'Fardos Embarcados'.
  sscrfields-functxt_01  = gwa_sel_button.

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'FC01'.

      SELECT SINGLE *
        FROM zppt0027 INTO @DATA(lwa_zppt0027)
       WHERE usuario = @sy-uname.

      IF sy-subrc ne 0.
        MESSAGE 'Sem autorização para essa ação!' TYPE 'S'.
        RETURN.
      ENDIF.

      SUBMIT zregister_data       WITH p_db_tab = 'ZMMT0008'
                                  WITH p_stcnam = 'ZMMT0008'
                                  WITH p_scmant = '0232'
                                  WITH p_scpesq = '0231'
                                  WITH p_stpesq = 'ZMMT0008'
                                  WITH p_maxdel = 124
                                  WITH p_title  = 'Fardos Embarcados'
       AND RETURN.

  ENDCASE.


*----------------------------------------------------------------------*
*                           AT SELECTION-SCREEN                        *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'BBB'.
      IF p_storno = abap_true.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

*----------------------------------------------------------------------*
*                           Start of Selection                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF s_matnr IS INITIAL.
    MESSAGE i836 WITH TEXT-028.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_werks IS INITIAL.
    MESSAGE i836 WITH TEXT-026.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_lgorta IS INITIAL.
    MESSAGE i836 WITH TEXT-027.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_lgortr IS INITIAL AND p_normal IS NOT INITIAL.
    MESSAGE i836 WITH TEXT-024.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_charg IS INITIAL.
    MESSAGE i836 WITH TEXT-025.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_charg-low <= 2010 OR s_charg-low > 2050.
    MESSAGE i836 WITH TEXT-033.
    LEAVE LIST-PROCESSING.
  ENDIF.

* Seleciona Dados
  PERFORM: f_seleciona_dados CHANGING gva_error.

  CHECK gva_error IS INITIAL.

  PERFORM: f_processa_dados,
           f_monta_fieldcat .

  IF git_saida[] IS INITIAL.
    MESSAGE i836 WITH TEXT-016.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CALL SCREEN 0100.


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR                                         *
*&---------------------------------------------------------------------*
*                           Incluindo Botão ALV                        *
*----------------------------------------------------------------------*
FORM f_handle_toolbar USING p_object      TYPE REF TO cl_alv_event_toolbar_set
                            p_interactive TYPE char1.

* Constants for button type
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.

  CASE p_storno.
    WHEN abap_false.

      " Botão Transfêrencia
      CLEAR sl_toolbar.
      MOVE:  c_mb1b            TO sl_toolbar-function ,
             icon_ws_truck     TO sl_toolbar-icon     ,
             TEXT-017          TO sl_toolbar-quickinfo,
             TEXT-017          TO sl_toolbar-text     ,
             space             TO sl_toolbar-disabled .
      APPEND sl_toolbar TO p_object->mt_toolbar.


    WHEN abap_true.

      " Botão Transfêrencia
      CLEAR sl_toolbar.
      MOVE:  c_estorno         TO sl_toolbar-function ,
             icon_storno       TO sl_toolbar-icon     ,
             TEXT-036          TO sl_toolbar-quickinfo,
             TEXT-036          TO sl_toolbar-text     ,
             space             TO sl_toolbar-disabled .
      APPEND sl_toolbar TO p_object->mt_toolbar.

    WHEN OTHERS.
  ENDCASE.


ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND                                         *
*&---------------------------------------------------------------------*
*                      User Command Botões Incluidos                   *
*----------------------------------------------------------------------*
FORM f_handle_command USING p_ucomm TYPE syucomm.

  CLEAR: gva_sucesso.
  CASE p_ucomm.
    WHEN c_mb1b. "Gerar Transferência
      PERFORM f_gerar_transf.
    WHEN c_estorno.
      PERFORM f_estornar_transf.
  ENDCASE.

  IF gva_sucesso IS NOT INITIAL.
    PERFORM: f_seleciona_dados CHANGING gva_error.

    IF gva_error IS INITIAL.
      PERFORM f_processa_dados.
    ENDIF.

    CALL METHOD gob_alv->refresh_table_display.
  ENDIF.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_TRANSF                                           *
*&---------------------------------------------------------------------*
*                           Gerar Transferência                        *
*----------------------------------------------------------------------*
FORM f_gerar_transf.

  DATA: lit_fardos_transf TYPE zpps0010_t.

  DATA: lit_rows     TYPE lvc_t_row,
        lva_resposta TYPE c,
        lva_msg(150) TYPE c.

  CLEAR: lit_fardos_transf[],  git_msn_log[].

  CALL METHOD gob_alv->get_selected_rows
    IMPORTING
      et_index_rows = lit_rows.

  IF lit_rows[] IS INITIAL.
    MESSAGE i836 WITH TEXT-012.
    EXIT.
  ENDIF.

  LOOP AT lit_rows INTO DATA(lwa_row).

    READ TABLE git_saida INTO DATA(lwa_saida)  INDEX lwa_row-index.
    CHECK sy-subrc EQ 0.

    APPEND VALUE #( nr_fardo_completo = lwa_saida-nr_fardo_completo ) TO lit_fardos_transf.

  ENDLOOP.

  zcl_comercializacao_algodao=>disp_fardos_comercializar( EXPORTING i_safra                 = CONV #( s_charg-low )
                                                                    i_filial_algodoeira     = CONV #( s_werks-low )
                                                                    i_bloco                 = CONV #( s_lgorta-low )
                                                                    i_matnr                 = CONV #( s_matnr-low )
                                                                    i_lgort_destino         = CONV #( s_lgortr-low )
                                                                    i_fardos_disponibilizar = lit_fardos_transf
                                                          IMPORTING e_msg_sucesso           = DATA(lva_msg_sucess)
                                                                    e_mblnr                 = DATA(lva_mblnr)
                                                                    e_mjahr                 = DATA(lva_mjahr)
                                                                    e_msg_error             = DATA(lva_msg_error) ).

  IF lva_msg_error IS NOT INITIAL.
    PERFORM f_monta_mensagem USING lva_msg_error 'E' '' '0000'.
  ELSEIF lva_msg_sucess IS NOT INITIAL.
    PERFORM f_monta_mensagem USING lva_msg_sucess 'S' lva_mblnr lva_mjahr.
    gva_sucesso = abap_true.
  ENDIF.

  IF git_msn_log[] IS NOT INITIAL.
    CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
      TABLES
        table    = git_msn_log
      EXCEPTIONS
        fb_error = 1
        OTHERS   = 2.
    RETURN.
  ENDIF.

ENDFORM.                    " Z_GERAR_TRANSF

FORM f_estornar_transf.

  DATA: lit_fardos_transf TYPE zpps0010_t.

  DATA: lit_rows     TYPE lvc_t_row,
        lva_resposta TYPE c,
        lva_msg(150) TYPE c.

  CLEAR: lit_fardos_transf[],  git_msn_log[].

  CALL METHOD gob_alv->get_selected_rows
    IMPORTING
      et_index_rows = lit_rows.

  IF lit_rows[] IS INITIAL.
    MESSAGE i836 WITH TEXT-012.
    EXIT.
  ENDIF.

  LOOP AT lit_rows INTO DATA(lwa_row).

    READ TABLE git_saida INTO DATA(lwa_saida)  INDEX lwa_row-index.
    CHECK sy-subrc EQ 0.

    APPEND VALUE #( nr_fardo_completo = lwa_saida-nr_fardo_completo ) TO lit_fardos_transf.

  ENDLOOP.

  zcl_comercializacao_algodao=>estornar_disp_fardos_comerc( EXPORTING i_safra                 = CONV #( s_charg-low )
                                                                      i_filial_algodoeira     = CONV #( s_werks-low )
                                                                      i_bloco                 = CONV #( s_lgorta-low )
                                                                      i_matnr                 = CONV #( s_matnr-low )
                                                                      i_fardos_estornar       = lit_fardos_transf
                                                            IMPORTING e_msg_sucesso           = DATA(lva_msg_sucess)
                                                                      e_mblnr                 = DATA(lva_mblnr)
                                                                      e_mjahr                 = DATA(lva_mjahr)
                                                                      e_msg_error             = DATA(lva_msg_error) ).

  IF lva_msg_error IS NOT INITIAL.
    PERFORM f_monta_mensagem USING lva_msg_error 'E' '' '0000'.
  ELSEIF lva_msg_sucess IS NOT INITIAL.
    PERFORM f_monta_mensagem USING lva_msg_sucess 'S' lva_mblnr lva_mjahr.
    gva_sucesso = abap_true.
  ENDIF.

  IF git_msn_log[] IS NOT INITIAL.
    CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
      TABLES
        table    = git_msn_log
      EXCEPTIONS
        fb_error = 1
        OTHERS   = 2.
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                             Seleciona Dados                          *
*----------------------------------------------------------------------*
FORM f_seleciona_dados CHANGING c_error.

  CLEAR: git_saida[], git_fardos_trace[], gwa_makt, c_error.

  zcl_comercializacao_algodao=>processar_disp_fardos( EXPORTING i_safra                     = CONV #( s_charg-low )
                                                                i_filial_algodoeira         = CONV #( s_werks-low )
                                                                i_bloco                     = CONV #( s_lgorta-low )
                                                                i_matnr                     = CONV #( s_matnr-low )
                                                                i_lgort_destino             = CONV #( s_lgortr-low )
                                                                i_estorno                   = CONV #( p_storno )
                                                      IMPORTING e_fardos_bloco_trace_cotton = git_fardos_trace
                                                                e_charg_proc                = gva_charg_proc
                                                                e_msg_error                 = DATA(lva_msg_error) ).
  IF lva_msg_error IS NOT INITIAL.
    c_error = abap_true.
    MESSAGE lva_msg_error TYPE 'S'.
    RETURN.
  ENDIF.

  CASE p_storno.
    WHEN abap_false.
      DELETE git_fardos_trace WHERE embarcado_sap = abap_true.
    WHEN abap_true.
      DELETE git_fardos_trace WHERE embarcado_sap = abap_false.
  ENDCASE.

  DELETE git_fardos_trace WHERE cd_sai NOT IN p_cdsai.

  CHECK git_fardos_trace[] IS NOT INITIAL.

  SELECT SINGLE matnr maktx
    FROM makt INTO gwa_makt
  WHERE matnr IN s_matnr
    AND spras EQ c_p.


ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_FIELDCAT                                         *
*&---------------------------------------------------------------------*
*                           Monta FieldCat                             *
*----------------------------------------------------------------------*
FORM f_monta_fieldcat.

  REFRESH: git_fcat,
           git_tool.

* Preenche FieldCat
  PERFORM f_preenche_fieldcat USING:
    c_table 'ICON'                TEXT-030 10 space 'C' ''     ,
    c_table 'WERKS'               TEXT-002 06 space ''  ''     ,
    c_table 'MATNR'               TEXT-003 10 c_x   ''  ''     ,
    c_table 'MAKTX'               TEXT-004 20 space ''  ''     ,
    c_table 'CHARG'               TEXT-005 12 space ''  ''     ,
    c_table 'NR_FARDO_COMPLETO'   TEXT-034 12 space ''  ''     ,
    c_table 'CD_SAI'              TEXT-029 20 space ''  ''     ,
    c_table 'CLABS'               TEXT-006 14 space ''  ''     ,
    c_table 'LGORT'               TEXT-007 17 space 'C' ''     ,
    c_table 'LGORTR'              TEXT-008 12 space 'C' ''     ,
    c_table 'CHARGR'              TEXT-009 21 space 'C' ''     ,
    c_table 'POSSUIACTS'          TEXT-031 10 space 'C' 'X'    ,
    c_table 'STATUS'              TEXT-032 10 space '' ''      .
  "c_table 'MATNC'               TEXT-020 10 c_x.

* Monta Layout
  PERFORM f_layout.

* Deleta Botões
  PERFORM f_deleta_bot USING: '&LOCAL&APPEND'       ,
                              '&LOCAL&COPY'         ,
                              '&LOCAL&COPY_ROW'     ,
                              '&LOCAL&CUT'          ,
                              '&LOCAL&DELETE_ROW'   ,
                              '&LOCAL&INSERT_ROW'   ,
                              '&LOCAL&MOVE_ROW'     ,
                              '&LOCAL&PASTE'        ,
                              '&LOCAL&PASTE_NEW_ROW',
                              '&LOCAL&UNDO'         ,
                              '&CHECK'              .

ENDFORM.                    " Z_MONTA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM f_layout.

  CLEAR gwa_layout.

  gwa_layout-zebra = 'X'.

ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  Z_DELETA_BOT                                             *
*&---------------------------------------------------------------------*
*                             Deleta Botões                            *
*----------------------------------------------------------------------*
FORM f_deleta_bot USING p_bot TYPE c.

  DATA sl_tool TYPE ui_func.

  sl_tool = p_bot.
  APPEND sl_tool TO git_tool.

ENDFORM.                    " Z_DELETA_BOT

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT                                      *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat                          *
*----------------------------------------------------------------------*
FORM f_preenche_fieldcat USING p_table      TYPE c
                               p_field      TYPE c
                               p_desc       TYPE c
                               p_len        TYPE n
                               p_zero       TYPE c
                               p_just       TYPE c
                               p_checkbox   TYPE c.

  DATA sl_fcat TYPE lvc_s_fcat.

  sl_fcat-tabname   = p_table.
  sl_fcat-fieldname = p_field.
  sl_fcat-scrtext_l = p_desc.
  sl_fcat-scrtext_m = p_desc.
  sl_fcat-scrtext_s = p_desc.
  sl_fcat-outputlen = p_len.
  sl_fcat-no_zero   = p_zero.
  sl_fcat-just      = p_just.
  sl_fcat-checkbox  = p_checkbox.

  APPEND sl_fcat TO git_fcat.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR 'TB0100'.
  ENDCASE.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_OBJ_ALV  OUTPUT                                     *
*&---------------------------------------------------------------------*
*                                 Obj Alv                              *
*----------------------------------------------------------------------*
MODULE zm_obj_alv OUTPUT.

* Instancia Container
  PERFORM: f_inst_cont ,
* Instancia Alv
           f_inst_alv  ,
* Instancia Eventos
           f_inst_event,
* Exibe Alv
           f_exibe_alv .

ENDMODULE.                 " ZM_OBJ_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_CONT                                              *
*&---------------------------------------------------------------------*
*                       Instancia Container                            *
*----------------------------------------------------------------------*
FORM f_inst_cont.

  CHECK gob_cont IS INITIAL.

  CREATE OBJECT gob_cont
    EXPORTING
      container_name              = 'CC_ALV'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH TEXT-010.
  ENDIF.

ENDFORM.                    " Z_INST_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_ALV                                               *
*&---------------------------------------------------------------------*
*                              Instancia Alv                           *
*----------------------------------------------------------------------*
FORM f_inst_alv.

  CHECK gob_alv IS INITIAL.

  CREATE OBJECT gob_alv
    EXPORTING
      i_parent = gob_cont.

  CREATE OBJECT gob_toolbar
    EXPORTING
      io_alv_grid = gob_alv.

ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_INST_EVENT                                             *
*&---------------------------------------------------------------------*
*                           Instancia Eventos                          *
*----------------------------------------------------------------------*
FORM f_inst_event.

  SET HANDLER gob_toolbar->zm_handle_user_command FOR gob_alv.
  SET HANDLER gob_toolbar->zm_handle_toolbar      FOR gob_alv.

  CALL METHOD gob_alv->register_delayed_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_delayed_change_select.

  CALL METHOD gob_alv->set_delay_time( ).

ENDFORM.                    " Z_INST_EVENT

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM f_exibe_alv.

  DATA vl_int TYPE int4.

  CALL METHOD gob_alv->set_table_for_first_display
    EXPORTING
      i_default                     = c_x
      is_layout                     = gwa_layout
      it_toolbar_excluding          = git_tool
    CHANGING
      it_outtab                     = git_saida
      it_fieldcatalog               = git_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD gob_alv->set_ready_for_input.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS                                         *
*&---------------------------------------------------------------------*
*                              Processa Dados                          *
*----------------------------------------------------------------------*
FORM f_processa_dados.

  DATA: lwa_saida       TYPE ty_saida.

  LOOP AT git_fardos_trace INTO DATA(lwa_fardo_trace).
    CLEAR: lwa_saida.

    lwa_saida-matnr               = s_matnr-low.
    lwa_saida-werks               = s_werks-low.
    lwa_saida-lgort               = s_lgorta-low.
    lwa_saida-charg               = gva_charg_proc.
    lwa_saida-nr_fardo_completo   = lwa_fardo_trace-nr_fardo_completo.
    lwa_saida-clabs               = lwa_fardo_trace-peso_liquido.
    lwa_saida-lgortr              = s_lgortr-low.
    lwa_saida-chargr              = s_charg-low.
    lwa_saida-cd_sai              = lwa_fardo_trace-cd_sai.
    lwa_saida-maktx               = gwa_makt-maktx.
    lwa_saida-icon                = lwa_fardo_trace-validacao_acts-icon.
    lwa_saida-status              = lwa_fardo_trace-validacao_acts-status.
    lwa_saida-possuiacts          = lwa_fardo_trace-validacao_acts-possuiacts.

    APPEND lwa_saida TO git_saida.
  ENDLOOP.

ENDFORM.                    " Z_PROCESSA_DADOS


FORM f_monta_mensagem USING p_message TYPE string
                            p_type    TYPE string
                            p_mat_doc TYPE bapi2017_gm_head_ret-mat_doc
                            p_year    TYPE bapi2017_gm_head_ret-doc_year.

  DATA: lwa_msn_log TYPE type_msn.

  CLEAR: lwa_msn_log.

  lwa_msn_log-messagem  = p_message.
  lwa_msn_log-tp_msn    = p_type.
  lwa_msn_log-doc_mat   = p_mat_doc.
  lwa_msn_log-ano       = p_year.

  APPEND lwa_msn_log TO git_msn_log.


ENDFORM.



*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              Exit Command                            *
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  DO_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_do_sum .
  DATA: tl_rows        TYPE lvc_t_row.

  CALL METHOD gob_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  DATA(_sel) = lines( tl_rows ).

  IF _sel > 0.
    MESSAGE |Selecionadas { _sel } linhas | TYPE 'S'.
  ENDIF.
ENDFORM.
