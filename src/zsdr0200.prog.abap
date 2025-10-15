* ==================================================================== *
*                         © AMAGGI                                     *
* ==================================================================== *
* Program.....: ZSDR0200                                               *
* Title.......: Cadastro de Checklist                                  *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 22/04/2025                                             *
* -------------------------------------------------------------------- *
REPORT zsdr0200.

TYPE-POOLS: slis, abap, icon.

" criar title gui 9000

" IMPLEMENTAR F_SELECIONA

TABLES: zsds379, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV1'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZSDS379'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

"dados alv
DATA gt_dados_alv1 TYPE STANDARD TABLE OF zsds379.
DATA gt_dados_alv2 TYPE STANDARD TABLE OF zsds380.

DATA g_container TYPE scrfname VALUE 'CC_ALV_9000'.
DATA g_custom_container TYPE REF TO cl_gui_custom_container.

DATA go_cc_alv_01 TYPE REF TO cl_gui_alv_grid.
DATA go_cc_alv_02 TYPE REF TO cl_gui_alv_grid.

DATA gt_fieldcat1 TYPE lvc_t_fcat.
DATA gt_fieldcat2 TYPE lvc_t_fcat.

DATA gt_exclude TYPE ui_functions.
DATA go_cont_i TYPE REF TO cl_gui_container.
DATA go_splitter TYPE REF TO cl_gui_splitter_container.

DATA gt_bapiret2 TYPE TABLE OF bapiret2.
DATA go_docking TYPE REF TO cl_gui_docking_container.

DATA gs_clicked_alv_01 TYPE zsds379.
DATA gv_changed TYPE c.
"SELECTION-SCREEN: FUNCTION KEY 1

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_chkli FOR zsds379-checklistid NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  "PERFORM f_preenche_data.
  "PERFORM F_BOTAO_FUNCTION.
  "PERFORM default_variant CHANGING p_vari.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
*  PERFORM f4_for_variant CHANGING p_vari.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

START-OF-SELECTION.
  PERFORM f_seleciona.
  PERFORM f_processa.
  PERFORM f_exibe_alv.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  CLEAR gv_changed.

  PERFORM f_sap_indicator
    USING 'Lendo informações do checklist, aguarde. . .' 50.

  SELECT * FROM zsdt0379
    INTO CORRESPONDING FIELDS OF TABLE gt_dados_alv1
   WHERE checklistid IN so_chkli.

  SORT gt_dados_alv1 BY checklistid ASCENDING.

  IF gt_dados_alv1 IS NOT INITIAL.

    SELECT * FROM zsdt0381
      INTO TABLE @DATA(lt_0381)
        FOR ALL ENTRIES IN @gt_dados_alv1
          WHERE checklistid = @gt_dados_alv1-checklistid
            AND status <> '03'.

  ENDIF.


  LOOP AT gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>).

    READ TABLE lt_0381 TRANSPORTING NO FIELDS
      WITH KEY checklistid = <fs_alv1>-checklistid.

    IF sy-subrc EQ 0.
      <fs_alv1>-com_simulador = abap_true.
    ENDIF.

    PERFORM f_altera_status_alv_01 CHANGING <fs_alv1>.

  ENDLOOP.

ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_FUNCTION
*&---------------------------------------------------------------------*
FORM f_botao_function.

  sscrfields-functxt_01 = 'BOTAO 1'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_COMMAND
*&---------------------------------------------------------------------*
FORM f_botao_command.

  IF sy-ucomm = 'FC01'.
    "EXECUTA FUNÇÃO DO BOTAO 1
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
FORM f_exibe_alv .

  DATA lt_eventos TYPE slis_t_event.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.
  DATA lw_settings TYPE lvc_s_glay.

  "IF gt_dados_alv1 IS NOT INITIAL.

  CALL SCREEN 9000.

  "ELSE.

  "MESSAGE s213(v4) DISPLAY LIKE 'E'.
  "EXIT.

  "ENDIF.

ENDFORM.                    " F_EXIBE_ALV
FORM f_top_of_page.
**  ALV Header declarations
*  DATA: t_header      TYPE slis_t_listheader,
*        wa_header     TYPE slis_listheader,
*        t_line        LIKE wa_header-info,
*        ld_lines      TYPE i,
*        ld_linesc(10) TYPE c,
*        lv_cnpj       TYPE char20,
*        lv_qtd        TYPE char20,
*        lv_data       TYPE char20.
*
*  wa_header-typ = 'H'.
*  wa_header-info = 'Processamento Manual - Transferência de Produção'.
*  APPEND wa_header TO t_header.
*  CLEAR wa_header.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = gw_kna1-kunnr
*    IMPORTING
*      output = gw_kna1-kunnr.
*
*  wa_header-typ = 'S'.
*  wa_header-key = 'Cliente: '.
*  CONCATENATE gw_kna1-kunnr gw_kna1-name1  INTO wa_header-info SEPARATED BY space.
*  APPEND wa_header TO t_header.
*  CLEAR: wa_header.
*
*  CALL FUNCTION 'CONVERSION_EXIT_CGCRT_OUTPUT'
*    EXPORTING
*      input  = gw_kna1-stcd1
*    IMPORTING
*      output = lv_cnpj.
*
*  wa_header-typ = 'S'.
*  wa_header-key = 'CNPJ: '.
*  wa_header-info = lv_cnpj.
*  APPEND wa_header TO t_header.
*  CLEAR: wa_header.
*
*  wa_header-typ = 'S'.
*  wa_header-key = 'OV: '.
*  WRITE gw_vbak-vbeln TO   wa_header-info .
*  APPEND wa_header TO t_header.
*  CLEAR: wa_header.
*
*  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
*    EXPORTING
*      input  = gw_vbak-erdat
*    IMPORTING
*      output = lv_data.
*
*  wa_header-typ = 'S'.
*  wa_header-key = 'Data: '.
*  wa_header-info = lv_data.
*
*  APPEND wa_header TO t_header.
*  CLEAR: wa_header.
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      it_list_commentary = t_header.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SET_EVENT
*&---------------------------------------------------------------------*
FORM f_set_event CHANGING p_event_tab TYPE slis_t_event.

  APPEND INITIAL LINE TO p_event_tab ASSIGNING FIELD-SYMBOL(<fs_event>).

  <fs_event>-name = 'DATA_CHANGED'.
  <fs_event>-form = 'USER_DT_CHANGED'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_CALLER_EXIT
*&---------------------------------------------------------------------*
FORM f_caller_exit USING p_data TYPE slis_data_caller_exit.

  DATA lo_grid TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lo_grid.

  CHECK lo_grid IS BOUND.

  CALL METHOD lo_grid->refresh_table_display( ).

ENDFORM.
CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no sender,

      handle_toolbar_top       FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command_top   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    "handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid.

*      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
*        IMPORTING er_data_changed.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD  handle_double_click.
    PERFORM f_double_click USING e_row e_column es_row_no sender.
  ENDMETHOD.                    "on_user_command

  METHOD handle_toolbar_top.

    DATA: ls_toolbar TYPE stb_button.

    "--- Append a separator to normal toolbar
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'ICON_CREATE' TO ls_toolbar-function.
    MOVE icon_create TO ls_toolbar-icon.
    MOVE 'Novo Checklist' TO ls_toolbar-quickinfo.
    MOVE 'Novo' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    "--- Append a separator to normal toolbar
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'ICON_QUESTION' TO ls_toolbar-function.
    MOVE icon_question TO ls_toolbar-icon.
    MOVE 'Perguntas' TO ls_toolbar-quickinfo.
    MOVE 'Perguntas' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    "--- Append a separator to normal toolbar
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'ICON_SET_STATE' TO ls_toolbar-function.
    MOVE icon_set_state TO ls_toolbar-icon.
    MOVE 'Ativo/Inativo' TO ls_toolbar-quickinfo.
    MOVE 'Ativo/Inativo' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_user_command_top. "sy-ucomm "ucomm " desativar " novo

    CASE e_ucomm.
      WHEN 'ICON_CREATE'.
        PERFORM f_create_top.
      WHEN 'ICON_SET_STATE'.
        PERFORM f_ativar_checklist.
      WHEN 'ICON_QUESTION'.
        PERFORM f_select_alv_perguntas.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_handle_events_popup DEFINITION.

  PUBLIC SECTION.
    METHODS : on_user_command
      FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_handle_events_popup IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM f_user_command_popup USING e_salv_function.
  ENDMETHOD.                    "on_user_command

ENDCLASS.                    "lcl_handle_events DEFINITION
*&---------------------------------------------------------------------*
*& Form f_user_command
*&---------------------------------------------------------------------*
FORM f_user_command_popup USING p_ucomm TYPE salv_de_function.

*  CASE p_ucomm.
*
*    WHEN 'PROC'.
*
*      gv_popup_processa = 'X'.
*
*      LEAVE TO SCREEN 0.
*    WHEN OTHERS.
*
*      gv_popup_processa = space.
*
*      LEAVE TO SCREEN 0.
*
*  ENDCASE.

ENDFORM.
FORM user_dt_changed USING rr_data_changed TYPE REF TO
                           cl_alv_changed_data_protocol.

  DATA lv_field TYPE c LENGTH 40.

  LOOP AT rr_data_changed->mt_mod_cells ASSIGNING FIELD-SYMBOL(<fs_mod_cells>).

    READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_saida>)
     INDEX <fs_mod_cells>-row_id.

    CHECK sy-subrc EQ 0.

    lv_field = '<FS_SAIDA>-' && <fs_mod_cells>-fieldname.

    ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_field>).

    CHECK <fs_field> IS ASSIGNED.

    <fs_field> = <fs_mod_cells>-value.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PFSTATUS
*&---------------------------------------------------------------------*
FORM f_status_set USING p_extab TYPE slis_t_extab.          "#EC CALLED

  SET PF-STATUS 'STANDARD'.

ENDFORM.                    "F_PFSTATUS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat_1.

  CLEAR gt_fieldcat1.

  " SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = gc_struc_name
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
      i_internal_tabname     = gc_internal_tab
    CHANGING
      ct_fieldcat            = gt_fieldcat1
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  DELETE gt_fieldcat1 WHERE fieldname = 'ICON'.
  DELETE gt_fieldcat1 WHERE fieldname = 'SELEC'.
  DELETE gt_fieldcat1 WHERE fieldname = 'DESATIVAR'.
  DELETE gt_fieldcat1 WHERE fieldname = 'CHANGED'.
  DELETE gt_fieldcat1 WHERE fieldname = 'COM_SIMULADOR'.

  READ TABLE gt_fieldcat1 ASSIGNING FIELD-SYMBOL(<fs_field>)
  WITH KEY fieldname = 'MSGTX'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    <fs_field>-reptext = 'Status'.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  PERFORM f_coluna_descr USING 'DESCR' 'Descrição' gt_fieldcat1.

ENDFORM.                    " F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat_2.

  CLEAR gt_fieldcat2.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSDS380'
      i_internal_tabname     = 'GT_DADOS_ALV2'
    CHANGING
      ct_fieldcat            = gt_fieldcat2
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  DELETE gt_fieldcat2 WHERE fieldname = 'SELEC'.
  DELETE gt_fieldcat2 WHERE fieldname = 'CHECKLISTID'.
  DELETE gt_fieldcat2 WHERE fieldname = 'INATIVAR'.
  DELETE gt_fieldcat2 WHERE fieldname = 'CHANGED'.
  DELETE gt_fieldcat2 WHERE fieldname = 'ICON'.

  READ TABLE gt_fieldcat2 ASSIGNING FIELD-SYMBOL(<fs_field>)
    WITH KEY fieldname = 'ICON'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    <fs_field>-reptext = 'Status'.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  READ TABLE gt_fieldcat2 ASSIGNING <fs_field>
    WITH KEY fieldname = 'PERGUNTA'.

  IF sy-subrc EQ 0.
    "<fs_field>-just = 'L'.
    <fs_field>-reptext = 'Pergunta'.
    <fs_field>-dd_outlen = 000040.
    <fs_field>-outputlen = 000040.
  ENDIF.

  READ TABLE gt_fieldcat2 ASSIGNING <fs_field>
    WITH KEY fieldname = 'SIM_NAO_INCONFOR'.

  IF sy-subrc EQ 0.
    <fs_field>-dd_outlen = 000020.
    <fs_field>-outputlen = 000020.
  ENDIF.


  PERFORM f_fieldcat_modi USING 'SIM_NAO_INCONFOR' 'CHECKBOX' 'X' CHANGING gt_fieldcat2.

  PERFORM f_coluna_descr USING 'SIM_NAO_INCONFOR' 'Inconsistencia quando SIM' CHANGING gt_fieldcat2.

  "DELETE gt_fieldcat2 WHERE fieldname = gc_select_field.

  "PERFORM f_coluna_descr USING 'MSGTX' 'Mensagem' gt_fieldcat2.

  "PERFORM f_coluna_descr USING 'COUNT_SAP' 'Notas SAP'.

  "PERFORM f_set_edit USING 'WERKS'.

  "PERFORM f_coluna_edita2 USING 'FFIN_NOTIF' 'Data' 'Data'.

  "<fs_fcat>-edit = p_editavel.

ENDFORM.                    " F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema.

  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema_s.

  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_SISTEMA_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema_insere.

  PERFORM f_mensagem_insere
    TABLES gt_bapiret2
     USING sy-msgty
           sy-msgid
           sy-msgno
           sy-msgv1
           sy-msgv2
           sy-msgv3
           sy-msgv4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
FORM f_sap_indicator USING p_text TYPE c
                           p_percent TYPE i.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percent
      text       = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
FORM f4_for_variant CHANGING f_vari TYPE slis_vari.

  DATA: lw_variant TYPE disvariant.

  lw_variant-variant = f_vari.
  lw_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = lw_variant
      i_save     = 'A'
    IMPORTING
      es_variant = lw_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    f_vari = lw_variant-variant.
  ENDIF.

ENDFORM.                    "f4_for_variant
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VARIANT
*&---------------------------------------------------------------------*
FORM default_variant CHANGING f_vari TYPE slis_vari.
  DATA: lw_variant TYPE disvariant.

  lw_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = lw_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc = 0.
    f_vari = lw_variant-variant.
  ENDIF.

ENDFORM.                    " DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_DATA
*&---------------------------------------------------------------------*
FORM f_preenche_data .

  "DATA(lv_ini) = sy-datum.
  "DATA(lv_fim) = sy-datum.

  "CHECK so_data[] IS INITIAL.

  "SUBTRACT 15 FROM lv_ini.
  "ADD 15 TO lv_fim.

  "APPEND 'IBT' && lv_ini && lv_fim TO so_data.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HYPERLINK
*&---------------------------------------------------------------------*
FORM f_hyperlink USING rs_selfield TYPE slis_selfield.

  DATA lw_saida_alv LIKE LINE OF gt_dados_alv1.

  CHECK rs_selfield-value IS NOT INITIAL.

  READ TABLE gt_dados_alv1 INTO lw_saida_alv INDEX rs_selfield-tabindex.

  CASE rs_selfield-fieldname.
    WHEN 'COLUNA'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " F_HYPERLINK

"PERFORM f_mensagem_insere TABLES p_ret2
"USING 'E' 'ZMM' '000' 'SYSID' text-t02
"gw_034-logsys space space.

FORM f_mensagem_bapiret USING p_mess TYPE bapiret2.

  MESSAGE ID p_mess-id TYPE 'S' NUMBER p_mess-number
    WITH p_mess-message_v1 p_mess-message_v2
         p_mess-message_v3 p_mess-message_v4.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_insere TABLES p_ret_tab STRUCTURE bapiret2
                        USING i_type TYPE bapi_mtype
                              i_id  TYPE  symsgid
                              i_number  TYPE  symsgno
                              i_mess_v1 TYPE any
                              i_mess_v2 TYPE any
                              i_mess_v3 TYPE any
                              i_mess_v4 TYPE any.

  APPEND INITIAL LINE TO p_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

  <fs_ret>-type = i_type.
  <fs_ret>-id = i_id.
  <fs_ret>-number = i_number.
  <fs_ret>-message_v1 = i_mess_v1.
  <fs_ret>-message_v2 = i_mess_v2.
  <fs_ret>-message_v3 = i_mess_v3.
  <fs_ret>-message_v4 = i_mess_v4.
  <fs_ret>-system = sy-sysid.

  MESSAGE ID <fs_ret>-id TYPE <fs_ret>-type NUMBER <fs_ret>-number
    WITH <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
      <fs_ret>-message_v4 INTO <fs_ret>-message.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_EXIBE_POPUP
*&---------------------------------------------------------------------*
FORM f_mensagem_exibe_popup USING p_bapiret2_tab TYPE bapiret2_t.

  DATA: l_lines TYPE i.

  DESCRIBE TABLE p_bapiret2_tab LINES l_lines.

  IF l_lines <= 1 OR sy-batch = 'X'.

    LOOP AT p_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_ret2>).

      MESSAGE ID <fs_ret2>-id
            TYPE 'S'
          NUMBER <fs_ret2>-number
            WITH <fs_ret2>-message_v1
                 <fs_ret2>-message_v2
                 <fs_ret2>-message_v3
                 <fs_ret2>-message_v4 DISPLAY LIKE <fs_ret2>-type.

    ENDLOOP.

  ELSE.

    CALL FUNCTION 'MESSAGES_INITIALIZE'.

    LOOP AT p_bapiret2_tab ASSIGNING <fs_ret2>.

      IF <fs_ret2>-id IS INITIAL.

        <fs_ret2>-id = 'DS'. "<-classe padrao abap
        <fs_ret2>-number = '016'.
        <fs_ret2>-message_v1 = <fs_ret2>-message.

      ENDIF.

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb                  = <fs_ret2>-id
          "EXCEPTION_IF_NOT_ACTIVE  = 'X'
          msgty                  = <fs_ret2>-type
          msgv1                  = <fs_ret2>-message_v1
          msgv2                  = <fs_ret2>-message_v2
          msgv3                  = <fs_ret2>-message_v3
          msgv4                  = <fs_ret2>-message_v4
          txtnr                  = <fs_ret2>-number
          "ZEILE                    = ' '
          "IMPORTING
          "ACT_SEVERITY             =
          "MAX_SEVERITY             =
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2
          OTHERS                 = 3.     "#EC CI_SUBRC

    ENDLOOP.

    CALL FUNCTION 'MESSAGES_STOP'
      EXCEPTIONS
        a_message = 1
        e_message = 2
        i_message = 3
        w_message = 4
        OTHERS    = 5.     "#EC CI_SUBRC

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        "CORRECTIONS_OPTION          = ' '
        "CORRECTIONS_FUNC_TEXT       = ' '
        "LINE_FROM                   = ' '
        "LINE_TO                     = ' '
        "OBJECT                      = ' '
        "SEND_IF_ONE                 = ' '
        batch_list_type     = 'B'
        show_linno          = ' '
        show_linno_text     = 'X'
        show_linno_text_len = '3'
        i_use_grid          = ' '
        i_amodal_window     = ' '
        "MSG_SELECT_FUNC             = ' '
        "MSG_SELECT_FUNC_TEXT        = ' '
        "IMPORTING
        "CORRECTIONS_WANTED          =
        "E_EXIT_COMMAND              =
        "MSG_SELECTED                =
      EXCEPTIONS
        inconsistent_range  = 1
        no_messages         = 2
        OTHERS              = 3.     "#EC CI_SUBRC

  ENDIF.

ENDFORM.
* CONTROLE DE ATUALIZAÇÃO DE TELA DINAMICAMENTE

*    DATA lt_return TYPE TABLE OF ddshretval.
*    DATA lt_fields TYPE TABLE OF dynpread.
*
*    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
*      EXPORTING
*        tabname           = 'ZTPP_009'
*        fieldname         = 'MATNR_DUMMY'
*        "searchhelp        = 'ZHPP_DUMMY'
*        "shlpparam         = 'MATNR_DUMMY'
*        "IMPORTING
*        "user_reset        =
*      TABLES
*        return_tab        = lt_return
*      EXCEPTIONS
*        field_not_found   = 1
*        no_help_for_field = 2
*        inconsistent_help = 3
*        no_values_found   = 4
*        OTHERS            = 5.
*
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_ret>).
*
*      APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_dyn>).
*
**      stepl
**
**      fieldinp
*
*      CASE <fs_ret>-fieldname.
*        WHEN 'WERKS'.
*          <fs_dyn>-fieldname = 'P_WERKS'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*        WHEN 'ARBPL'.
*          <fs_dyn>-fieldname = 'P_ARBPL'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*        WHEN 'VORNR'.
*
*          <fs_dyn>-fieldname = 'P_VORNR'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*        WHEN 'FLAG_DUMPS'.
*
*          APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_dyn2>).
*
*          IF <fs_ret>-fieldval = 'X'.
*
*            <fs_dyn>-fieldname = 'P_DUM_S'.
*            <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*            <fs_dyn2>-fieldname = 'P_DUM_N'.
*            <fs_dyn2>-fieldvalue = space.
*
*          ELSE.
*            <fs_dyn>-fieldname = 'P_DUM_N'.
*            <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*            <fs_dyn2>-fieldname = 'P_DUM_S'.
*            <fs_dyn2>-fieldvalue = space.
*          ENDIF.
*
*        WHEN 'MATNR_DUMMY'.
*
*          <fs_dyn>-fieldname = 'P_MATNR'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*
*      ENDCASE.
*
*    ENDLOOP.
*
*    CALL FUNCTION 'DYNP_VALUES_UPDATE'
*      EXPORTING
*        dyname               = '1000'
*        dynumb               = '1000'
*      TABLES
*        dynpfields           = lt_fields
*      EXCEPTIONS
*        invalid_abapworkarea = 1
*        invalid_dynprofield  = 2
*        invalid_dynproname   = 3
*        invalid_dynpronummer = 4
*        invalid_request      = 5
*        no_fielddescription  = 6
*        undefind_error       = 7
*        OTHERS               = 8.
*
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.


***AT SELECTION-SCREEN OUTPUT.
**
**    LOOP AT SCREEN.
**
**      IF screen-name CP '*P_WERKS*'
**        OR screen-name CP '*P_DUM_N*'
**        OR screen-name CP '*P_ARBPL*'
**        OR screen-name CP '*P_VORNR*'.
**
**        screen-input = 0.
**        MODIFY SCREEN.
**      ENDIF.
**
**    ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  f_coluna_descr
*&---------------------------------------------------------------------*
FORM f_set_edit  USING p_fieldname TYPE slis_fieldname.

  READ TABLE gt_fieldcat1 ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-edit = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VERIFICA_LINHA_SELEC
*&---------------------------------------------------------------------*
FORM f_verifica_linha_selec CHANGING p_error TYPE c.

*  READ TABLE GT_DADOS_ALV1 WITH KEY selec = 'X' TRANSPORTING NO FIELDS.
*
*  IF sy-subrc NE 0.
*    MESSAGE s851(v4) DISPLAY LIKE 'E'.
*    p_error = 'X'.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa
*&---------------------------------------------------------------------*
FORM f_processa.

  LOOP AT gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_dados>).



  ENDLOOP.

  SORT gt_dados_alv1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM f_popup_to_confirm USING p_question TYPE c
                     CHANGING p_answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = sy-title
      text_question  = p_question
    IMPORTING
      answer         = p_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

ENDFORM.                    " F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_INSERE_TXT
*&---------------------------------------------------------------------*
FORM f_mensagem_insere_txt USING i_type TYPE bapi_mtype
                                 p_string TYPE string.

  DATA: lt_trtexts     TYPE trtexts,
        lw_trtexts     TYPE trtext,
        lv_texto(4000).

  DATA lv_msg1 TYPE sy-msgv1.
  DATA lv_msg2 TYPE sy-msgv1.
  DATA lv_msg3 TYPE sy-msgv1.
  DATA lv_msg4 TYPE sy-msgv1.

  lv_texto = p_string.

  CALL FUNCTION 'TR_SPLIT_TEXT'
    EXPORTING
      iv_text  = lv_texto
      iv_len   = 30
    IMPORTING
      et_lines = lt_trtexts.

  LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE sy-tabix.
      WHEN 1.
        lv_msg1 = <fs_line>.
      WHEN 2.
        lv_msg2 = <fs_line>.
      WHEN 3.
        lv_msg3 = <fs_line>.
      WHEN 4.
        lv_msg4 = <fs_line>.
    ENDCASE.

  ENDLOOP.

  PERFORM f_mensagem_insere
    TABLES gt_bapiret2
     USING i_type
           'DS'
           '016'
           lv_msg1
           lv_msg2
           lv_msg3
           lv_msg4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_REFRESH_SELECT
*&---------------------------------------------------------------------*
FORM f_alv_refresh_select  USING    p_rs_selfield.

** Atualiza dados da linha corrente caso não tenha dado enter
*  READ TABLE GT_DADOS_ALV1 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX rs_selfield-tabindex.
*
*  CHECK sy-subrc EQ 0.
*
*  CASE rs_selfield-fieldname.
*    WHEN 'WERKS_D'.
*
*      IF <fs_saida>-werks_d <> rs_selfield-value.
*
*        <fs_saida>-werks_d = rs_selfield-value.
*
*      ENDIF.
*
*    WHEN 'WERKS_I'.
*
*      IF <fs_saida>-werks_i <> rs_selfield-value.
*
*        <fs_saida>-werks_i = rs_selfield-value.
*
*      ENDIF.
*
*    WHEN 'TIPO_PROCE'.
*
*      IF <fs_saida>-tipo_proce <> rs_selfield-value.
*
*        <fs_saida>-tipo_proce = rs_selfield-value.
*
*      ENDIF.
*
*  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_PROCESS_UPDATE
*&---------------------------------------------------------------------*
FORM f_alv_process_update.

*  LOOP AT GT_DADOS_ALV1 ASSIGNING FIELD-SYMBOL(<fs_saida>).
*
*    IF <fs_saida>-tipo_proce IS NOT INITIAL.
*
*      READ TABLE gt_dd07t ASSIGNING FIELD-SYMBOL(<fs_dd07t>)
*        WITH KEY domvalue_l = <fs_saida>-tipo_proce.
*
*      IF sy-subrc EQ 0.
*        <fs_saida>-tipo_proce_dd = <fs_dd07t>-ddtext.
*      ENDIF.
*
*    ELSE.
*
*      CLEAR <fs_saida>-tipo_proce_dd.
*
*    ENDIF.
*
*  ENDLOOP.

  DATA lo_grid TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lo_grid.

  CHECK lo_grid IS BOUND.

  CALL METHOD lo_grid->refresh_table_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR '9000'.

  PERFORM f_create_alv_01.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      PERFORM f_leave_to_screen.
    WHEN 'EXIT'.
      PERFORM f_leave_to_screen.
    WHEN 'CANC'.
      PERFORM f_leave_to_screen.
    WHEN 'SAVE'.

      PERFORM f_save.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALV_01_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_01_init OUTPUT.

  PERFORM f_create_alv_01.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  f_coluna_descr
*&---------------------------------------------------------------------*
FORM f_coluna_descr USING p_fieldname TYPE slis_fieldname
                          p_text TYPE scrtext_l
                 CHANGING p_tab TYPE lvc_t_fcat..

  READ TABLE p_tab ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-scrtext_s = p_text.
  <fs_cat>-scrtext_m = p_text.
  <fs_cat>-scrtext_l = p_text.
  <fs_cat>-reptext = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*&  DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_application1 DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_hotspot_click        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,

      handle_double_click         FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,

      handle_toolbar              FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

*      handle_toolbar_bottom       FOR EVENT toolbar OF cl_gui_alv_grid
*        IMPORTING e_object e_interactive,

      handle_user_command         FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.
*&--------------------------------------------------------------------*
*& IMPLEMENTATION
*&--------------------------------------------------------------------*
CLASS lcl_application1 IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM hotspot_click USING e_column_id e_row_id.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM f_double_click1 USING es_row_no-row_id e_column.
  ENDMETHOD.

  METHOD handle_toolbar.
    "PERFORM hotspot_click USING e_column_id e_row_id.
  ENDMETHOD.

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'ICON_SYSTEM_UNDO'.
        PERFORM f_close_bottom.

*      WHEN 'CONF_ANALISE'.
*        PERFORM f_conf_analise.

      WHEN OTHERS.
    ENDCASE.

*    gv_alv->set_frontend_layout( is_layout = gs_layout ).
*    gv_alv->refresh_table_display( ).

    CALL METHOD cl_gui_cfw=>flush.



  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*&  DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_application2 DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_hotspot_click        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,

      handle_double_click         FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,

      handle_toolbar              FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_toolbar_bottom       FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command         FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.
*&--------------------------------------------------------------------*
*& IMPLEMENTATION
*&--------------------------------------------------------------------*
CLASS lcl_application2 IMPLEMENTATION.

  METHOD handle_hotspot_click.
    "PERFORM hotspot_click USING e_column_id e_row_id.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM f_double_click2 USING es_row_no-row_id e_column.
  ENDMETHOD.

  METHOD handle_toolbar.
    "PERFORM hotspot_click USING e_column_id e_row_id.
  ENDMETHOD.

  METHOD handle_toolbar_bottom.

    DATA: ls_toolbar TYPE stb_button.

    "--- Append a separator to normal toolbar
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

*    CLEAR ls_toolbar.
*    MOVE 'ICON_SET_STATE' TO ls_toolbar-function.
*    MOVE icon_set_state TO ls_toolbar-icon.
*    MOVE 'Ativar/Inativar' TO ls_toolbar-quickinfo.
*    MOVE 'Ativar/Inativar' TO ls_toolbar-text.
*    MOVE ' ' TO ls_toolbar-disabled.
*    APPEND ls_toolbar TO e_object->mt_toolbar.
*
*    "--- Append a separator to normal toolbar
*    CLEAR ls_toolbar.
*    MOVE 3 TO ls_toolbar-butn_type.
*    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'ICON_INSERT_ROW' TO ls_toolbar-function.
    MOVE icon_insert_row TO ls_toolbar-icon.
    MOVE 'Adicionar' TO ls_toolbar-quickinfo.
    MOVE 'Adicionar' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

*    CLEAR ls_toolbar.
*    MOVE 'ICON_DELETE_ROW' TO ls_toolbar-function.
*    MOVE ICON_DELETE_ROW TO ls_toolbar-icon.
*    MOVE 'Excluir' TO ls_toolbar-quickinfo.
*    MOVE 'Excluir' TO ls_toolbar-text.
*    MOVE ' ' TO ls_toolbar-disabled.
*    APPEND ls_toolbar TO e_object->mt_toolbar.

    "--- Append a separator to normal toolbar
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'ICON_SYSTEM_UNDO' TO ls_toolbar-function.
    MOVE icon_system_undo TO ls_toolbar-icon.
    MOVE 'Fechar Inferior' TO ls_toolbar-quickinfo.
    MOVE 'Fechar' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'ICON_SYSTEM_UNDO'.
        PERFORM f_close_bottom.

      WHEN 'ICON_INSERT_ROW'.
        PERFORM f_insert_row_bottom.

*      WHEN 'ICON_SET_STATE'.
*        PERFORM f_ativar_bottom.

      WHEN OTHERS.
    ENDCASE.

*    gv_alv->set_frontend_layout( is_layout = gs_layout ).
*    gv_alv->refresh_table_display( ).

    CALL METHOD cl_gui_cfw=>flush.



  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ALV_01
*&---------------------------------------------------------------------*
FORM f_create_alv_01 .

  DATA lw_settings TYPE lvc_s_glay.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.

  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_event_handler.

  DATA go_application TYPE REF TO lcl_application1.

  "lw_layout-grid_title = sy-title.

  lw_layout-sel_mode = 'A'.
  "lw_layout-no_headers = 'X'.
  "lw_layout-no_toolbar = 'X'. XXXXX
  lw_layout-col_opt = 'X'.
  lw_layout-cwidth_opt = 'X'.

  lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.
  "lw_layout-

  "lw_settings-edt_cll_cb = 'X'.

*data G_CONTAINER TYPE SCRFNAME VALUE 'CC_ALV_9000'.
*data G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.


  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.
  ENDIF.

  IF go_cc_alv_01 IS INITIAL.

    CREATE OBJECT go_cc_alv_01
      EXPORTING
        i_parent = g_custom_container.


*    CREATE OBJECT go_cc_alv_01
*      EXPORTING
*        i_parent = cl_gui_container=>default_screen.

    PERFORM f_monta_fieldcat_1.

*  CALL METHOD go_005_alv->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CREATE OBJECT lo_handle.

    SET HANDLER lo_handle->handle_double_click FOR go_cc_alv_01.
    SET HANDLER lo_handle->handle_toolbar_top FOR go_cc_alv_01.
    SET HANDLER lo_handle->handle_user_command_top  FOR go_cc_alv_01.

    "set HANDLER lo_handle->
    "SET HANDLER lo_handle->handle_data_changed FOR go_005_alv.

    "SET HANDLER lo_handle->handle_top_of_page FOR go_005_alv.

    "PERFORM f_filtro_lote_alv.

    "go_cc_alv_01->SET_

    lw_variant-report       = sy-repid.
    lw_variant-username     = sy-uname.


    lw_layout-sel_mode   = 'A'.
    lw_layout-zebra      = 'X'.
    lw_layout-cwidth_opt = 'X'.
    "lw_layout-ctab_fname = 'COLOR'. " #DESCOMENTAR

    lw_variant-report  = sy-repid.
    lw_variant-variant = lw_layout.

    " Configuration for first display.
    CALL METHOD go_cc_alv_01->set_table_for_first_display
      EXPORTING
        is_layout       = lw_layout
        is_variant      = lw_variant
        i_save          = 'A'
      CHANGING
        it_outtab       = gt_dados_alv1
        it_fieldcatalog = gt_fieldcat1.

    CREATE OBJECT go_application.

    SET HANDLER go_application->handle_hotspot_click FOR go_cc_alv_01.
    SET HANDLER go_application->handle_double_click  FOR go_cc_alv_01.
    SET HANDLER go_application->handle_toolbar       FOR go_cc_alv_01.
    SET HANDLER go_application->handle_user_command  FOR go_cc_alv_01.

  ELSE.

    CALL METHOD go_cc_alv_01->set_frontend_layout
      EXPORTING
        is_layout = lw_layout.

    CALL METHOD go_cc_alv_01->refresh_table_display( ).


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
FORM f_coluna_edita2  USING p_fieldname TYPE slis_fieldname
                            p_text_s TYPE scrtext_s
                            p_text_l TYPE scrtext_l.

  READ TABLE gt_fieldcat1 ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-scrtext_s = p_text_s.
  <fs_cat>-scrtext_m = p_text_s.
  <fs_cat>-scrtext_l = p_text_l.
  <fs_cat>-reptext = p_text_l.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ON_CLICK
*&---------------------------------------------------------------------*
FORM f_double_click USING e_row TYPE lvc_s_row
                      e_column TYPE lvc_s_col
                      es_row_no TYPE lvc_s_roid
                      es_sender.

  CLEAR gt_dados_alv2[].

  PERFORM f_alv_perguntas USING e_row-index.

****  READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv>)
****    INDEX e_row-index.
****
****  CHECK sy-subrc EQ 0.
****
****  gs_clicked_alv_01 = <fs_alv>.
****
****
****  DATA lt_row TYPE lvc_t_roid.
****  DATA lv_erro.
****
****  PERFORM f_get_selected_rows USING go_cc_alv_01 CHANGING lt_row lv_erro.
****
****  LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<fs_row>).
****
****    READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>)
****      INDEX <fs_row>-row_id.
****
****
****
****    BREAK-POINT.

*  SELECT * FROM zsdt0380
*    INTO CORRESPONDING FIELDS OF TABLE gt_dados_alv2
*    WHERE checklistid = <fs_alv>-checklistid.
*
*  SORT gt_dados_alv2 BY checkid ASCENDING.
*
*  LOOP AT gt_dados_alv2 ASSIGNING FIELD-SYMBOL(<fs_alv2>).
*    PERFORM f_altera_status_alv_02 CHANGING <fs_alv2>.
*  ENDLOOP.
*
*  PERFORM row_color USING e_row-index '1'.
*
*  CALL METHOD cl_gui_cfw=>flush.
*
*  PERFORM close_bottom.
*
**  IF gt_dados_alv2[] IS INITIAL.
**
**    MESSAGE 'Nenhum registro encontrado.' TYPE 'S' DISPLAY LIKE 'E'.
**
**    PERFORM clear_row_color USING '1'.
**
**    CLEAR gt_dados_alv2.
**
**    EXIT.
**
**  ENDIF.
*
*  PERFORM f_monta_fieldcat_2.
*
*  PERFORM create_botton TABLES gt_dados_alv2.

ENDFORM.
"PERFORM f_fieldcat_modi USING 'PESO' 'EDIT' 'X' CHANGING lt_fieldcat.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_MODI
*&---------------------------------------------------------------------*
FORM f_fieldcat_modi USING p_fieldname TYPE slis_fieldname
                           p_column TYPE c
                           p_value TYPE any
                  CHANGING p_field_cat TYPE lvc_t_fcat.

  READ TABLE p_field_cat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  DATA(lv_name) = '<FS_FCAT>-' && p_column.

  ASSIGN (lv_name) TO FIELD-SYMBOL(<fs_colum>).

  CHECK sy-subrc EQ 0.

  <fs_colum> = p_value.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PREENCHE_X
*&---------------------------------------------------------------------*
FORM f_preenche_x USING iv_struct TYPE any
               CHANGING cv_structx TYPE any.

  DATA lo_linetype TYPE REF TO cl_abap_structdescr.

  ASSIGN ('IV_STRUCT') TO FIELD-SYMBOL(<lf_data>).

  CHECK sy-subrc EQ 0.

  lo_linetype ?= cl_abap_typedescr=>describe_by_data( <lf_data> ).

  LOOP AT lo_linetype->components ASSIGNING FIELD-SYMBOL(<lf_component>).

    DATA(lv_campo) = 'IV_STRUCT-' && <lf_component>-name.
    DATA(lv_campox) = 'CV_STRUCTX-' && <lf_component>-name.

    ASSIGN (lv_campo) TO FIELD-SYMBOL(<lf_campo>).

    CHECK sy-subrc EQ 0.

    ASSIGN (lv_campox) TO FIELD-SYMBOL(<lf_campox>).

    IF <lf_campo> IS NOT INITIAL.
      <lf_campox> = 'X'.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ROW_COLOR
*&---------------------------------------------------------------------*
FORM row_color  USING p_index
                      p_alv.

  DATA lt_stable TYPE lvc_s_stbl.
  DATA lw_cellcolor TYPE lvc_s_scol.

  PERFORM clear_row_color USING '1'.

  CASE p_alv.
    WHEN '1'.
      "--- Atualiza a cor da linha selecionado
      READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_dados>) INDEX p_index.
      IF sy-subrc = 0.
        lw_cellcolor-color-col = 5.    " color code 1-7, if outside rage defaults to 7
        lw_cellcolor-color-int = '0'.  " 1 = Intensified on, 0 = Intensified off
        lw_cellcolor-color-inv = '0'.  " 1 = text colour, 0 = background colour
        "APPEND lw_cellcolor TO <fs_dados>-color. " #DESCOMENTAR
        CLEAR lw_cellcolor.

        lt_stable-row = p_index.
        go_cc_alv_01->refresh_table_display( is_stable = lt_stable ).

      ENDIF.

      "WHEN c_alv_bottom_left.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLEAR_ROW_COLOR
*&---------------------------------------------------------------------*
FORM clear_row_color USING    p_alv.

  CASE p_alv.
    WHEN '1'.
      "--- Limpar as cores
      LOOP AT gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_dados_1>).
*        CLEAR <fs_dados_1>-color.
      ENDLOOP.


      "WHEN c_alv_bottom_left.
*      "--- Limpar as cores
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLOSE_BOTTOM
*&---------------------------------------------------------------------*
FORM close_bottom .

  IF go_docking IS BOUND.

    go_cc_alv_02->free( ).
    go_docking->free( ).

    PERFORM clear_row_color USING '1'.

    CLEAR go_docking.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM f_double_click1 USING p_index p_column.

*  READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv_01>)
*    INDEX p_index.
*
*  CHECK sy-subrc EQ 0.
*
*  PERFORM row_color USING p_index '1'.
*
*  CALL METHOD cl_gui_cfw=>flush.
*
*  PERFORM close_bottom.
*
*  " #DESCOMENTAR -- READ TABLE DO ALV 1
*
*  IF sy-subrc NE 0.
*
*    MESSAGE 'Nenhum registro encontrado.' TYPE 'S' DISPLAY LIKE 'E'.
*
*    PERFORM clear_row_color USING '1'.
*
*    CLEAR gt_dados_alv2.
*
*    EXIT.
*
*
*  ENDIF.
*
*  PERFORM f_monta_fieldcat_2.
*
*  CLEAR gt_dados_alv2.
*
*  " #DESCOMENTAR -- CRIAR AÇÃO DO DOUBLE CLICK DO ALV SUPERIOR
*
*  PERFORM create_botton TABLES gt_dados_alv2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM f_double_click2 USING p_index p_column.

  " #DESCOMENTAR -- CRIAR AÇÃO DO ALV 2 CLICK

*  READ TABLE gt_dados_alv2 ASSIGNING FIELD-SYMBOL(<fs_alv_02>)
*    INDEX p_index.
*
*  CHECK sy-subrc EQ 0.
*
*  SET PARAMETER ID 'MAT' FIELD <fs_alv_02>-matnr.
*  SET PARAMETER ID 'WRK' FIELD <fs_alv_02>-werks.
*
*  IF <fs_alv_02>-charg IS NOT INITIAL.
*    SET PARAMETER ID 'CHA' FIELD <fs_alv_02>-charg.
*  ELSE.
*    SET PARAMETER ID 'CHA' FIELD <fs_alv_02>-codigo.
*  ENDIF.
*
*  CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_BOTTON
*&---------------------------------------------------------------------*
FORM create_botton  TABLES p_t_alv.

  DATA ls_layout_i TYPE lvc_s_layo.
  DATA ls_variant TYPE disvariant.

  DATA lo_application TYPE REF TO lcl_application2.

  IF go_docking IS NOT BOUND.

    CREATE OBJECT go_docking
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = cl_gui_docking_container=>dock_at_bottom
        extension = '100'.

    go_cont_i ?= go_docking.

    CREATE OBJECT go_splitter
      EXPORTING
        parent            = go_cont_i
        rows              = 1
        columns           = 1 "2
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    "--- LEFT
    CALL METHOD go_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = go_cont_i.

    CREATE OBJECT go_cc_alv_02
      EXPORTING
        i_parent          = go_cont_i
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM exclude_tb_functions CHANGING gt_exclude.

    ls_layout_i-sel_mode   = 'A'.
    ls_layout_i-zebra      = 'X'.
    "ls_layout_i-cwidth_opt = 'X'.
    ls_layout_i-ctab_fname = 'CELLCOLOR'.
    "ls_layout_i-grid_title = 'Agrupador'.
    ls_variant-report  = sy-repid.
    ls_variant-handle  = 'COMP'.
    ls_variant-variant = '/DEFAULT'.

    CALL METHOD go_cc_alv_02->set_table_for_first_display
      EXPORTING
        is_layout            = ls_layout_i
        is_variant           = ls_variant
        i_save               = 'A'
        it_toolbar_excluding = gt_exclude
      CHANGING
        it_outtab            = gt_dados_alv2[]
        it_fieldcatalog      = gt_fieldcat2.

    CREATE OBJECT lo_application.

    SET HANDLER lo_application->handle_toolbar_bottom FOR go_cc_alv_02.
    SET HANDLER lo_application->handle_user_command   FOR go_cc_alv_02.
    SET HANDLER lo_application->handle_double_click  FOR go_cc_alv_02.

    CALL METHOD go_cc_alv_02->set_toolbar_interactive.

    go_splitter->set_column_width( id = 1 width = 100 ).

    go_cc_alv_02->set_frontend_layout( is_layout = ls_layout_i ).
    go_cc_alv_02->refresh_table_display( ).

  ELSE.

*    gv_alv_right->set_visible( visible = abap_false ).
    go_splitter->set_column_width( id = 1 width = 100 ).
*    gv_alv_right->refresh_table_display( ).

    go_cc_alv_02->set_frontend_layout( is_layout = ls_layout_i ).
    go_cc_alv_02->refresh_table_display( ).

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
FORM exclude_tb_functions  CHANGING p_lt_exclude TYPE ui_functions.

  DATA ls_exclude TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_print.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO p_lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO p_lt_exclude.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM hotspot_click  USING p_index p_column.

  READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv_01>)
    INDEX p_index.

  CHECK sy-subrc EQ 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CLOSE_BOTTOM
*&---------------------------------------------------------------------*
FORM f_close_bottom .

  PERFORM f_clear_row_color USING '1'.

  IF go_docking IS BOUND.

    go_cc_alv_02->free( ).
*    gv_alv_right->free( ).
    go_docking->free( ).

    CLEAR go_docking.
    CLEAR gt_dados_alv2.

  ENDIF.

  IF go_cc_alv_01 IS BOUND.
    go_cc_alv_01->refresh_table_display( ).
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CLEAR_ROW_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3136   text
*----------------------------------------------------------------------*
FORM f_clear_row_color  USING    p_alv.

  CASE p_alv.
    WHEN '1'.
      "--- Limpar as cores
      LOOP AT gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv_o>).
*        CLEAR <fs_alv_o>-color.
      ENDLOOP.


    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_ativar_bottom
*&---------------------------------------------------------------------*
FORM f_ativar_bottom .

  DATA lt_row TYPE lvc_t_roid.
  DATA lv_erro.

  PERFORM f_get_selected_rows USING go_cc_alv_02 CHANGING lt_row lv_erro.

  LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<fs_row>).

    READ TABLE gt_dados_alv2 ASSIGNING FIELD-SYMBOL(<fs_alv2>)
      INDEX <fs_row>-row_id.

    CHECK sy-subrc EQ 0 .

    IF <fs_alv2>-inativar = abap_false.
      <fs_alv2>-inativar = abap_true.
    ELSE.
      <fs_alv2>-inativar = abap_false.
    ENDIF.

    PERFORM f_update_sys_fields CHANGING <fs_alv2>.
    PERFORM f_altera_status_alv_02 CHANGING <fs_alv2>.

  ENDLOOP.

  "PERFORM f_refresh_alv USING '2'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_altera_status
*&---------------------------------------------------------------------*
FORM f_altera_status_alv_02 CHANGING cs_alv TYPE zsds380.

  IF cs_alv-inativar = abap_true.
    "cs_alv-icon = '@0A@'.
  ELSE.
    "cs_alv-icon = '@08@'.
  ENDIF.

  cs_alv-changed = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_altera_status
*&---------------------------------------------------------------------*
FORM f_update_sys_fields CHANGING cs_alv TYPE any.

  ASSIGN ('CS_ALV-USER_CHANGE') TO FIELD-SYMBOL(<fs_user_change>).
  ASSIGN ('CS_ALV-DATE_CHANGE') TO FIELD-SYMBOL(<fs_date_change>).
  ASSIGN ('CS_ALV-TIME_CHANGE') TO FIELD-SYMBOL(<fs_time_change>).

  CHECK <fs_user_change> IS ASSIGNED.
  CHECK <fs_date_change> IS ASSIGNED.
  CHECK <fs_time_change> IS ASSIGNED.

  <fs_user_change> = sy-uname.
  <fs_date_change> = sy-datum.
  <fs_time_change> = sy-uzeit.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_desativar_checklist
*&---------------------------------------------------------------------*
FORM f_ativar_checklist .

  DATA lt_row TYPE lvc_t_roid.
  DATA lv_erro.

  PERFORM f_get_selected_rows USING go_cc_alv_01 CHANGING lt_row lv_erro.

  LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<fs_row>).

    READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>)
      INDEX <fs_row>-row_id.

    CHECK sy-subrc EQ 0.

    <fs_alv1>-desativar = abap_false.

    PERFORM f_altera_status_alv_01 USING <fs_alv1>.

    PERFORM f_desativa_outro_checklist USING <fs_alv1>.

  ENDLOOP.

  PERFORM f_refresh_alv USING abap_true abap_true abap_false.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_selected_rows
*&---------------------------------------------------------------------*
FORM f_get_selected_rows USING uo_alv TYPE REF TO cl_gui_alv_grid
                      CHANGING ct_row TYPE lvc_t_roid
                               cv_erro.

  CHECK uo_alv IS BOUND.

  CALL METHOD uo_alv->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_index)
      et_row_no     = ct_row.

  IF ct_row IS INITIAL.
    cv_erro = abap_true.
    MESSAGE s016(ds) WITH 'Selecionar ao menos uma linha' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_refresh_alv
*&---------------------------------------------------------------------*
FORM f_refresh_alv USING uv_changed TYPE c
                         uv_stable TYPE c
                         uv_soft TYPE c.

  DATA ls_stable TYPE lvc_s_stbl.

  ls_stable-col = abap_true.
  ls_stable-row = abap_true.

  IF go_cc_alv_01 IS BOUND.

    IF uv_stable = abap_false.
      go_cc_alv_01->refresh_table_display( i_soft_refresh = uv_soft ).
    ELSE.
      go_cc_alv_01->refresh_table_display( EXPORTING is_stable = ls_stable i_soft_refresh = uv_soft ).
    ENDIF.

  ENDIF.

  gv_changed = uv_changed.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_altera_status_alv_01
*&---------------------------------------------------------------------*
FORM f_altera_status_alv_01 CHANGING cs_alv1 TYPE zsds379.

  IF cs_alv1-desativar = abap_false.
    cs_alv1-icon = '@5B@'.
    cs_alv1-msgtx = 'Ativo'.
  ELSE.
    cs_alv1-msgtx = 'Desativado'.
    cs_alv1-icon = '@5C@'.
  ENDIF.

  cs_alv1-changed = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_global_change
*&---------------------------------------------------------------------*
FORM f_set_global_change .

  gv_changed = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_save
*&---------------------------------------------------------------------*
FORM f_save .

  DATA lt_0379 TYPE TABLE OF zsdt0379.
  DATA lt_0380 TYPE TABLE OF zsdt0380.

  IF gv_changed = abap_true.

    LOOP AT gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>) WHERE changed = abap_true.

      APPEND INITIAL LINE TO lt_0379 ASSIGNING FIELD-SYMBOL(<fs_0379>).

      MOVE-CORRESPONDING <fs_alv1> TO <fs_0379>.

      <fs_alv1>-changed = abap_false.

    ENDLOOP.

    LOOP AT gt_dados_alv2 ASSIGNING FIELD-SYMBOL(<fs_alv2>) WHERE changed = abap_true.

      APPEND INITIAL LINE TO lt_0380 ASSIGNING FIELD-SYMBOL(<fs_0380>).

      MOVE-CORRESPONDING <fs_alv2> TO <fs_0380>.

      <fs_alv2>-changed = abap_false.

    ENDLOOP.

    IF lt_0379 IS NOT INITIAL.
      MODIFY zsdt0379 FROM TABLE lt_0379.
    ENDIF.

    IF lt_0380 IS NOT INITIAL.
      MODIFY zsdt0380 FROM TABLE lt_0380.
    ENDIF.

    COMMIT WORK AND WAIT.

    gv_changed = abap_false.

    MESSAGE s016(ds) WITH 'Dados gravados'.

  ENDIF.

  "LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LEAVE_TO_SCREEN
*&---------------------------------------------------------------------*
FORM f_leave_to_screen .

  DATA lv_answer TYPE c VALUE '1'.

  IF gv_changed = abap_true.
    PERFORM f_popup_to_confirm USING 'Dados não foram gravados, sair mesmo assim?' CHANGING lv_answer.
  ENDIF.

  CHECK lv_answer EQ '1'.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_create_top
*&---------------------------------------------------------------------*
FORM f_create_top .

  DATA lv_code TYPE c.

  DATA lt_fields  TYPE TABLE OF sval.

  lt_fields =
    VALUE #( ( tabname   = 'ZSDS379' fieldname = 'BUKRS' field_obl = 'X' fieldtext = 'Empresa'  )
             ( tabname   = 'ZSDS379' fieldname = 'DESCR' field_obl = 'X' fieldtext = 'Descrição'  ) ).

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = sy-title
    IMPORTING
      returncode      = lv_code
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  CHECK lv_code IS INITIAL AND lt_fields[] IS NOT INITIAL.

  APPEND INITIAL LINE TO gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>).

  LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_field>) WHERE value IS NOT INITIAL.

    CASE <fs_field>-fieldname.
      WHEN 'BUKRS'.
        <fs_alv1>-bukrs = <fs_field>-value.
      WHEN 'DESCR'.
        <fs_alv1>-descr = <fs_field>-value.
    ENDCASE.

  ENDLOOP.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZSDCHECKL'
    IMPORTING
      number                  = <fs_alv1>-checklistid
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.
    EXIT. " mensagem aqui **teste
  ENDIF.

  <fs_alv1>-desativar = abap_true.
  <fs_alv1>-user_create = sy-uname.
  <fs_alv1>-time_create = sy-uzeit.
  <fs_alv1>-date_create = sy-datum.
  <fs_alv1>-changed = abap_true.

  PERFORM f_altera_status_alv_01 USING <fs_alv1>.

  "PERFORM f_desativa_outro_checklist USING <fs_alv1>.

  PERFORM f_refresh_alv USING abap_true abap_true abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insert_row_bottom
*&---------------------------------------------------------------------*
FORM f_insert_row_bottom .

  DATA lt_perguntas TYPE  zsdc380.
  DATA lt_0380 TYPE TABLE OF zsds380.
  DATA lv_canc.

  CHECK gs_clicked_alv_01 IS NOT INITIAL.

  IF gs_clicked_alv_01-desativar = abap_false.
    MESSAGE s016(ds) WITH 'Não é possível inserir com o checklist ativo' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  lt_0380 = CORRESPONDING #( gt_dados_alv2 ).

  CALL FUNCTION 'ZSDMF_SEL_PERGUNTAS_POPUP'
    EXPORTING
      iv_checklistid = gs_clicked_alv_01-checklistid
      it_0380        = lt_0380
    IMPORTING
      ev_canc        = lv_canc
      et_perguntas   = lt_perguntas.

  APPEND LINES OF lt_perguntas TO gt_dados_alv2.

*  PERFORM f_refresh_alv USING '2'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_desativa_outro_checklist
*&---------------------------------------------------------------------*
FORM f_desativa_outro_checklist USING us_alv1 TYPE zsds379.

  LOOP AT gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>)
    WHERE bukrs = us_alv1-bukrs AND checklistid <> us_alv1-checklistid
      AND desativar = abap_false.

    <fs_alv1>-desativar = abap_true.

    <fs_alv1>-date_create = sy-datum.
    <fs_alv1>-time_create = sy-uzeit.
    <fs_alv1>-user_create = sy-uname.

    PERFORM f_altera_status_alv_01 USING <fs_alv1>.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_perguntas
*&---------------------------------------------------------------------*
FORM f_alv_perguntas USING uv_index TYPE lvc_index.

  DATA lt_zsdc380 TYPE zsdc380.
  DATA lv_editavel TYPE flag.
  DATA lv_canc.

  READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv>)
    INDEX uv_index.

  CHECK sy-subrc EQ 0.

  " SE 'X' ATIVA EDIÇÃO, SÓ PODE EDITAR STATUS DESATIVADO
  lv_editavel = <fs_alv>-desativar.

  " se tiver simulador, não pode editar
  IF <fs_alv>-com_simulador = abap_true.
    lv_editavel = abap_false.
    MESSAGE s016(ds) WITH 'Checklist contém simulador, não é possível editar' DISPLAY LIKE 'W'.
  ENDIF.

  CALL FUNCTION 'ZSDMF_TELA_CHECKLIST_PERGUNTAS'
    EXPORTING
      iv_checklistid     = <fs_alv>-checklistid
      iv_bukrs           = <fs_alv>-bukrs
      iv_descr           = <fs_alv>-descr
      iv_editavel        = lv_editavel
    IMPORTING
      ev_canc            = lv_canc
      et_checklist_items = lt_zsdc380.

  CHECK lv_canc IS INITIAL.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_alv_perguntas
*&---------------------------------------------------------------------*
FORM f_select_alv_perguntas .


  DATA lt_row TYPE lvc_t_roid.
  DATA lv_index TYPE lvc_index.
  DATA lv_erro.

  PERFORM f_get_selected_rows USING go_cc_alv_01 CHANGING lt_row lv_erro.

  LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<fs_row>).

    lv_index = <fs_row>-row_id.

    PERFORM f_alv_perguntas USING lv_index.

    EXIT.

  ENDLOOP.

ENDFORM.
