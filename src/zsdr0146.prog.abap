* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZSDR0146                                               *
* Title.......: Boleta Vinculada                                       *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 15.12.2022                                             *
* -------------------------------------------------------------------- *
REPORT zsdr0146.

TYPE-POOLS: slis, abap, icon.
TABLES: zsde013, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZSDE013'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

DATA go_cc_cus_01 TYPE REF TO cl_gui_custom_container.
DATA go_cc_alv_01 TYPE REF TO cl_gui_alv_grid.
"dados alv
DATA gt_dados_alv TYPE STANDARD TABLE OF zsde013.
DATA gt_fieldcat TYPE lvc_t_fcat.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.

"SELECTION-SCREEN: FUNCTION KEY 1

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS so_simu FOR zsde013-doc_simu.
SELECTION-SCREEN END OF BLOCK b1.

" DADOS DO ALV
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  "PERFORM f_preenche_data.
  "PERFORM F_BOTAO_FUNCTION.
  PERFORM default_variant CHANGING p_vari.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant CHANGING p_vari.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

START-OF-SELECTION.
  PERFORM f_seleciona.
  PERFORM f_processa.

END-OF-SELECTION.
  PERFORM f_exibe_alv.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  CLEAR gt_dados_alv.

  IF sy-sysid = 'DEV'.

    DO 7 TIMES.

      APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

      <fs_alv>-doc_simu = '1010101'.
*    <fs_alv>-seq = sy-index.
*    <fs_alv>-werks = '1010'.
*    <fs_alv>-werks_dest = '1011'.
*    <fs_alv>-proc_finlz = 'X'.
*    <fs_alv>-user_crt = sy-uname.
*    <fs_alv>-data_crt = sy-datum.
*    <fs_alv>-hora_crt = sy-uzeit.

    ENDDO.
  ELSE.

    LOOP AT so_simu ASSIGNING FIELD-SYMBOL(<fs_simu>).
      UNPACK <fs_simu>-low TO <fs_simu>-low.
    ENDLOOP.

    SELECT * FROM zsdt0308
     INTO CORRESPONDING FIELDS OF TABLE gt_dados_alv
       WHERE doc_simu IN so_simu.
  ENDIF.







ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
            rs_selfield TYPE slis_selfield.                 "#EC CALLED

  PERFORM f_alv_refresh_select USING rs_selfield.

  PERFORM f_alv_process_update.

  CASE r_ucomm.
    WHEN 'USER_COMMAND'.
      PERFORM f_processa.
    WHEN '&IC1'.
      PERFORM f_hyperlink   USING rs_selfield.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

  rs_selfield-refresh = 'X'.
  r_ucomm = '&REFRESH'.

ENDFORM.                    "user_command
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

  IF gt_dados_alv IS NOT INITIAL.

    CALL SCREEN 9000.

  ELSE.

    MESSAGE s213(v4) DISPLAY LIKE 'E'.
    EXIT.

  ENDIF.

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
        IMPORTING e_row e_column es_row_no.

    "handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid.

*      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
*        IMPORTING er_data_changed.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD  handle_double_click.
    PERFORM f_on_click USING e_row e_column es_row_no.
  ENDMETHOD.                    "on_user_command

*  METHOD handle_top_of_page.
*    PERFORM f_top_of_page.
*  ENDMETHOD.

*  METHOD handle_data_changed.
*    PERFORM f_on_data_changed USING er_data_changed.
*  ENDMETHOD.

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

    READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_saida>)
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
FORM f_monta_fieldcat.

  CLEAR gt_fieldcat.

  " SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = gc_struc_name
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
      i_internal_tabname     = gc_internal_tab
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  DELETE gt_fieldcat WHERE fieldname = 'SELEC'.

  "DELETE gt_fieldcat WHERE fieldname = 'DOC_SIMU'.
  DELETE gt_fieldcat WHERE fieldname = 'TP_OBRI'.
  DELETE gt_fieldcat WHERE fieldname = 'SALDO'.

  "PERFORM f_coluna_descr USING 'MSGTX' 'Status'.

  "PERFORM f_coluna_descr USING 'COUNT_SAP' 'Notas SAP'.

  "PERFORM f_set_edit USING 'WERKS'.

  PERFORM f_coluna_edita2 USING 'VLR_BRL_ENVIADO' 'BRL Sim.' 'Valor BRL Sim.'.
  PERFORM f_coluna_edita2 USING 'VLR_USD_ENVIADO' 'USD Sim.' 'Valor USD Sim.'.

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

  DATA lw_saida_alv LIKE LINE OF gt_dados_alv.

  CHECK rs_selfield-value IS NOT INITIAL.

  READ TABLE gt_dados_alv INTO lw_saida_alv INDEX rs_selfield-tabindex.

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

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-edit = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VERIFICA_LINHA_SELEC
*&---------------------------------------------------------------------*
FORM f_verifica_linha_selec CHANGING p_error TYPE c.

  CALL METHOD go_cc_alv_01->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_index_rows)
      et_row_no     = DATA(lt_rows).

  IF lt_rows IS INITIAL.
    MESSAGE s851(v4) DISPLAY LIKE 'E'.
    p_error = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa
*&---------------------------------------------------------------------*
FORM f_processa.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    IF <fs_alv>-vlr_usd_enviado IS INITIAL AND <fs_alv>-taxa_neg IS NOT INITIAL.

      <fs_alv>-vlr_usd_enviado = <fs_alv>-vlr_brl_enviado / <fs_alv>-taxa_neg.

    ENDIF.

    PERFORM f_formata_comment CHANGING <fs_alv>-coment.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_formata_comment
*&---------------------------------------------------------------------*
FORM f_formata_comment CHANGING p_ret TYPE c.

  "p_ret = p_string.

  REPLACE ALL OCCURRENCES OF 'u00C0' IN p_ret WITH 'À'.
  REPLACE ALL OCCURRENCES OF 'u00C1' IN p_ret WITH 'Á'.
  REPLACE ALL OCCURRENCES OF 'u00C2' IN p_ret WITH 'Â'.
  REPLACE ALL OCCURRENCES OF 'u00C3' IN p_ret WITH 'Ã'.
  REPLACE ALL OCCURRENCES OF 'u00C7' IN p_ret WITH 'Ç'.
  REPLACE ALL OCCURRENCES OF 'u00C8' IN p_ret WITH 'È'.
  REPLACE ALL OCCURRENCES OF 'u00C9' IN p_ret WITH 'É'.
  REPLACE ALL OCCURRENCES OF 'u00CA' IN p_ret WITH 'Ê'.
  REPLACE ALL OCCURRENCES OF 'u00CC' IN p_ret WITH 'Ì'.
  REPLACE ALL OCCURRENCES OF 'u00CD' IN p_ret WITH 'Í'.
  REPLACE ALL OCCURRENCES OF 'u00CE' IN p_ret WITH 'Î'.
  REPLACE ALL OCCURRENCES OF 'u00E0' IN p_ret WITH 'à'.
  REPLACE ALL OCCURRENCES OF 'u00E1' IN p_ret WITH 'á'.
  REPLACE ALL OCCURRENCES OF 'u00E2' IN p_ret WITH 'â'.
  REPLACE ALL OCCURRENCES OF 'u00E3' IN p_ret WITH 'ã'.
  REPLACE ALL OCCURRENCES OF 'u00E4' IN p_ret WITH 'ä'.
  REPLACE ALL OCCURRENCES OF 'u00E7' IN p_ret WITH 'ç'.
  REPLACE ALL OCCURRENCES OF 'u00E8' IN p_ret WITH 'è'.
  REPLACE ALL OCCURRENCES OF 'u00E9' IN p_ret WITH 'é'.
  REPLACE ALL OCCURRENCES OF 'u00EA' IN p_ret WITH 'ê'.
  REPLACE ALL OCCURRENCES OF 'u00EB' IN p_ret WITH 'ë'.

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
*  READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX rs_selfield-tabindex.
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

*  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_saida>).
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

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'ATUALIZAR'.
      PERFORM f_atualizar.
    WHEN 'REENVIO'.
      PERFORM f_reenvio.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE PROGRAM.
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
*&      Form  F_CREATE_ALV_01
*&---------------------------------------------------------------------*
FORM f_create_alv_01 .

  DATA lw_settings TYPE lvc_s_glay.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.

  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_event_handler.

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

  IF go_cc_alv_01 IS INITIAL.

    CREATE OBJECT go_cc_alv_01
      EXPORTING
        i_parent = cl_gui_container=>default_screen.


    PERFORM f_monta_fieldcat.

*  CALL METHOD go_005_alv->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CREATE OBJECT lo_handle.

    SET HANDLER lo_handle->handle_double_click FOR go_cc_alv_01.
    "set HANDLER lo_handle->
    "SET HANDLER lo_handle->handle_data_changed FOR go_005_alv.

    "SET HANDLER lo_handle->handle_top_of_page FOR go_005_alv.

    "PERFORM f_filtro_lote_alv.

    "go_cc_alv_01->SET_

    lw_variant-report       = sy-repid.
    lw_variant-username     = sy-uname.

    " Configuration for first display.
    CALL METHOD go_cc_alv_01->set_table_for_first_display
      EXPORTING
        is_layout       = lw_layout
        is_variant      = lw_variant
        i_save          = 'A'
      CHANGING
        it_outtab       = gt_dados_alv
        it_fieldcatalog = gt_fieldcat.

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

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_cat>)
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
FORM f_on_click USING e_row TYPE lvc_s_row
                      e_column TYPE lvc_s_col
                      es_row_no TYPE lvc_s_roid.

  READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)
    INDEX e_row-index.

  CHECK sy-subrc EQ 0.

*  IF e_column = 'ICON_PROD_PERM'.
*
*    CALL FUNCTION 'ZMM_ALV_GRID_POPUP'
*      EXPORTING
*        i_title = 'Produtores permitidos'
**     IMPORTING
**       ET_ROWS =
*      TABLES
*        it_alv  = <fs_005>-prod_perm_tab.
*
*  ELSE.
*
*    CLEAR gt_0006.
*
*    LOOP AT gt_0006_global ASSIGNING FIELD-SYMBOL(<fs_0006>)
*      WHERE nu_compra = <fs_005>-nu_compra
*        AND lote = <fs_005>-lote
*        AND tcode = gv_param-tcode.
*
*      APPEND <fs_0006> TO gt_0006.
*
*    ENDLOOP.
*
*    PERFORM f_append_006_test.
*
*    PERFORM f_create_alv_006.
*
*    PERFORM f_refresh_grid USING space 'X'.
*
*    IF gt_0006 IS INITIAL.
*
*      PERFORM f_put_mensagem
*        USING 'W'
*              'Não há OV/Pedidos de transferencia gerados para esse lote de compra'.
*
*    ENDIF.
*
*  ENDIF.

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
*&      Form  F_REENVIO
*&---------------------------------------------------------------------*
FORM f_reenvio .

  DATA ls_request TYPE zsdt0308.
  DATA lv_erro.

  PERFORM f_verifica_linha_selec CHANGING lv_erro.

  CHECK lv_erro IS INITIAL.

  CALL METHOD go_cc_alv_01->get_selected_rows
    IMPORTING
      et_row_no = DATA(lt_rows).

  LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_row>).

    READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)
      INDEX <fs_row>-row_id.

    CHECK sy-subrc EQ 0.

    IF <fs_alv>-ident_rev_sigam IS NOT INITIAL.

      MESSAGE s016(ds) WITH 'Ident. Reversão-Sigam já está preenchido' DISPLAY LIKE 'E'.

      lv_erro = 'X'.

      CONTINUE.

    ENDIF.

    ls_request-doc_simu = <fs_alv>-doc_simu.
    ls_request-ident = <fs_alv>-ident.
    ls_request-vlr_brl_enviado = <fs_alv>-vlr_brl_enviado.
    ls_request-data_venc = <fs_alv>-data_venc.
    ls_request-coment = <fs_alv>-coment.

    CALL FUNCTION 'ZSDMF001_REVERTE_BOLETA_API'
      IMPORTING
        ev_erro    = lv_erro
      CHANGING
        cs_request = ls_request.

    <fs_alv>-ident_rev_sigam = ls_request-ident_rev_sigam.

    IF <fs_alv>-ident_rev_sigam IS NOT INITIAL.
      MODIFY zsdt0308 FROM ls_request.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR
*&---------------------------------------------------------------------*
FORM f_atualizar .

  PERFORM f_seleciona.

  CHECK go_cc_alv_01 IS BOUND.

  go_cc_alv_01->refresh_table_display( ).

ENDFORM.
