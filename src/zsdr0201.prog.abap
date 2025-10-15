* ==================================================================== *
*                         © AMAGGI                                     *
* ==================================================================== *
* Program.....: ZSDR0201                                               *
* Title.......: Checklist X Simulador                                  *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 24/04/2025                                             *
* -------------------------------------------------------------------- *
REPORT zsdr0201.

TYPE-POOLS: slis, abap, icon.

" criar title gui 9000

" IMPLEMENTAR F_SELECIONA

TABLES: zsds0005, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV1'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZSDS0005'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

"dados alv
DATA gt_dados_alv1 TYPE STANDARD TABLE OF zsds0005.
DATA gt_dados_alv2 TYPE STANDARD TABLE OF zsds0006.

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

DATA gv_block_bottom TYPE c.
DATA gs_clicked_alv_01 TYPE zsds0005.
DATA gv_changed TYPE c.

"SELECTION-SCREEN: FUNCTION KEY 1

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS so_simu FOR  zsds0005-doc_simulacao.
  SELECT-OPTIONS so_kunnr FOR zsds0005-kunnr.
  SELECT-OPTIONS so_vkorg FOR zsds0005-vkorg.
  SELECT-OPTIONS so_vkbur FOR zsds0005-vkbur.
  SELECT-OPTIONS so_vtweg FOR zsds0005-vtweg.
  SELECT-OPTIONS so_spart FOR zsds0005-spart.
  SELECT-OPTIONS so_cultu FOR zsds0005-cultura.
  SELECT-OPTIONS so_safra FOR zsds0005-safra.
  SELECT-OPTIONS so_erdat FOR zsds0005-erdat.

SELECTION-SCREEN END OF BLOCK b1.

" DADOS DO ALV
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS p_no_chk RADIOBUTTON GROUP rg01.
  PARAMETERS p_co_chk RADIOBUTTON GROUP rg01.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  "PERFORM f_preenche_data.
  "PERFORM F_BOTAO_FUNCTION.
*  PERFORM default_variant CHANGING p_vari.
*
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

  CLEAR gt_dados_alv1.

  PERFORM f_sap_indicator
    USING 'Lendo informações do simulador, aguarde. . .' 50.

  SELECT * FROM zi_in_chklist_sim
    INTO CORRESPONDING FIELDS OF TABLE @gt_dados_alv1
   WHERE doc_simulacao IN @so_simu
      AND kunnr IN @so_kunnr
      AND vkorg IN @so_vkorg
      AND vkbur IN @so_vkbur
      AND vtweg IN @so_vtweg
      AND spart IN @so_spart
      AND cultura IN @so_cultu
      AND safra IN @so_safra
      AND erdat IN @so_erdat.

  CASE abap_true.
    WHEN p_no_chk.
      DELETE gt_dados_alv1 WHERE checklistid IS NOT INITIAL.
    WHEN p_co_chk.
      DELETE gt_dados_alv1 WHERE checklistid IS INITIAL.
    WHEN OTHERS.
  ENDCASE.

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

  IF gt_dados_alv1 IS NOT INITIAL.

    CALL SCREEN 9000.

  ELSE.

    MESSAGE s213(v4) DISPLAY LIKE 'E'.
    EXIT.

  ENDIF.

ENDFORM.                    " F_EXIBE_ALV
CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no sender.

    "handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid.

*      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
*        IMPORTING er_data_changed.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD  handle_double_click.
    PERFORM f_on_click USING e_row e_column es_row_no sender.
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

  DELETE gt_fieldcat1 WHERE fieldname = 'SELEC'.
  DELETE gt_fieldcat1 WHERE fieldname = 'DESCR'.

  IF p_co_chk IS INITIAL.
    DELETE gt_fieldcat1 WHERE fieldname = 'CHECKLISTID'.
  ENDIF.

  READ TABLE gt_fieldcat1 ASSIGNING FIELD-SYMBOL(<fs_field>)
  WITH KEY fieldname = 'ICON'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    <fs_field>-reptext = 'Status'.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  READ TABLE gt_fieldcat1 ASSIGNING <fs_field>
    WITH KEY fieldname = 'DESCR'.

  IF sy-subrc EQ 0.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  DELETE gt_fieldcat1 WHERE fieldname = 'COLOR'.
  DELETE gt_fieldcat1 WHERE fieldname = 'STATUS'.
  DELETE gt_fieldcat1 WHERE fieldname = 'CHECKLIST_DISPO'.
  DELETE gt_fieldcat1 WHERE fieldname = 'FLAG_INCOFORMIDADE'.

  PERFORM f_coluna_descr USING 'SAFRA' 'Safra' gt_fieldcat1.

  PERFORM f_coluna_descr USING 'STATUS_DESC' 'Status' gt_fieldcat1.

  "PERFORM f_fieldcat_modi USING 'STATUS_DESC' 'DD_OUTLEN' '000020' CHANGING gt_fieldcat1.

  "PERFORM f_set_edit USING 'WERKS'.

  PERFORM f_coluna_edita2 USING 'USER_RESPOSTA' 'UserQuesti' 'User.Questionario'.
  PERFORM f_coluna_edita2 USING 'STATUS_CHECKLIST' 'StatQuesti' 'Status.Questionario'.

  PERFORM f_coluna_edita2 USING 'NAME1' 'Cliente' 'Nome Cliente'.
  PERFORM f_coluna_edita2 USING 'VLRTOT' 'VlrTotal' 'VlrTotal'.
  PERFORM f_coluna_edita2 USING 'VENCI' 'Vencimento' 'Vencimento'.
  PERFORM f_coluna_edita2 USING 'WAERK' 'Moeda' 'Moeda'.
  PERFORM f_coluna_edita2 USING 'TP_SIM_DESC' 'Modalidade' 'Modalidade'.

  "<fs_fcat>-edit = p_editavel.

ENDFORM.                    " F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat_2.

  CLEAR gt_fieldcat1.

  " SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = 'ZSDS0006'
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
      i_internal_tabname     = 'GT_FIELDCAT2'
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
  DELETE gt_fieldcat2 WHERE fieldname = 'ATIVO'.
  DELETE gt_fieldcat2 WHERE fieldname = 'DOC_SIMULACAO'.
  DELETE gt_fieldcat2 WHERE fieldname = 'CHECKLISTID'.
  DELETE gt_fieldcat2 WHERE fieldname = 'SIM_NAO_INCONFOR'.
  DELETE gt_fieldcat2 WHERE fieldname = 'FLAG_SIM'.
  DELETE gt_fieldcat2 WHERE fieldname = 'FLAG_NAO'.

  READ TABLE gt_fieldcat2 ASSIGNING FIELD-SYMBOL(<fs_field>)
  WITH KEY fieldname = 'ICON'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    <fs_field>-reptext = 'Ativo'.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  READ TABLE gt_fieldcat2 ASSIGNING <fs_field>
    WITH KEY fieldname = 'RADIO_SIM'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    <fs_field>-reptext = 'Sim'.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  READ TABLE gt_fieldcat2 ASSIGNING <fs_field>
    WITH KEY fieldname = 'RADIO_NAO'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    <fs_field>-reptext = 'Não'.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  READ TABLE gt_fieldcat2 ASSIGNING <fs_field>
    WITH KEY fieldname = 'PERGUNTA'.

  IF sy-subrc EQ 0.
    <fs_field>-dd_outlen = 000020.
  ENDIF.

  "PERFORM f_coluna_descr USING 'RADIO_SIM' 'SIM' gt_fieldcat2.
  "PERFORM f_coluna_descr USING 'RADIO_NAO' 'NÃO' gt_fieldcat2.
*
*  PERFORM f_fieldcat_modi USING 'FLAG_SIM' 'EDIT' 'X' CHANGING gt_fieldcat2.
*  PERFORM f_fieldcat_modi USING 'FLAG_SIM' 'CHECKBOX' 'X' CHANGING gt_fieldcat2.
*
*  PERFORM f_fieldcat_modi USING 'FLAG_NAO' 'EDIT' 'X' CHANGING gt_fieldcat2.
*  PERFORM f_fieldcat_modi USING 'FLAG_NAO' 'CHECKBOX' 'X' CHANGING gt_fieldcat2.
*
*  READ TABLE gt_fieldcat2 ASSIGNING <fs_field>
*  WITH KEY fieldname = 'FLAG_SIM'.
*
*  IF sy-subrc EQ 0.
*    <fs_field>-just = 'C'.
*    <fs_field>-dd_outlen = 000010.
*  ENDIF.



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

    IF <fs_dados>-checklistid IS INITIAL.
      <fs_dados>-icon = icon_light_out.
    ELSE.

      CASE <fs_dados>-status.

          " Pendente de aprovação
        WHEN '01'.
          <fs_dados>-icon = icon_yellow_light.

          "02	Aprovado com inconformidade
        WHEN '02'.
          <fs_dados>-icon = icon_green_light.

          " 03  Reprovado
        WHEN '03'.
          <fs_dados>-icon = icon_red_light.

          "04	Aprovado
        WHEN '04'.
          <fs_dados>-icon = icon_green_light.
        WHEN OTHERS.
      ENDCASE.

    ENDIF.

    IF <fs_dados>-checklistid IS INITIAL.
      <fs_dados>-status_desc = 'Não criado'.
    ELSE.
      IF <fs_dados>-flag_incoformidade = abap_true.
        <fs_dados>-status_checklist = 'Com Não conformidade'.
      ELSE.
        <fs_dados>-status_checklist = 'Sem Não Conformidade'.
      ENDIF.
    ENDIF.

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

      handle_toolbar_bottom       FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

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
    DATA: ls_toolbar TYPE stb_button.

    IF p_co_chk = abap_true.

      CLEAR ls_toolbar.
      MOVE 3 TO ls_toolbar-butn_type.
      APPEND ls_toolbar TO e_object->mt_toolbar.

      CLEAR ls_toolbar.
      MOVE 'ICON_PREVIOUS_OBJECT' TO ls_toolbar-function.
      MOVE icon_system_undo TO ls_toolbar-icon.
      MOVE 'Reiniciar checklist' TO ls_toolbar-quickinfo.
      MOVE 'Reiniciar' TO ls_toolbar-text.
      MOVE ' ' TO ls_toolbar-disabled.
      APPEND ls_toolbar TO e_object->mt_toolbar.

    ELSE.

      CLEAR ls_toolbar.
      MOVE 3 TO ls_toolbar-butn_type.
      APPEND ls_toolbar TO e_object->mt_toolbar.

      CLEAR ls_toolbar.
      MOVE 'ICON_QUESTION' TO ls_toolbar-function.
      MOVE icon_question TO ls_toolbar-icon.
      MOVE 'Preencher Checklist' TO ls_toolbar-quickinfo.
      MOVE 'Preencher Checklist' TO ls_toolbar-text.
      MOVE ' ' TO ls_toolbar-disabled.
      APPEND ls_toolbar TO e_object->mt_toolbar.

    ENDIF.

  ENDMETHOD.

  METHOD handle_toolbar_bottom.

*    DATA: ls_toolbar TYPE stb_button.
*
*    "--- Append a separator to normal toolbar
*    CLEAR ls_toolbar.
*    MOVE 3 TO ls_toolbar-butn_type.
*    APPEND ls_toolbar TO e_object->mt_toolbar.
*
*    CLEAR ls_toolbar.
*    MOVE 'ICON_SYSTEM_UNDO' TO ls_toolbar-function.
*    MOVE icon_system_undo TO ls_toolbar-icon.
*    MOVE 'Fechar Inferior' TO ls_toolbar-quickinfo.
*    MOVE 'Fechar' TO ls_toolbar-text.
*    MOVE ' ' TO ls_toolbar-disabled.
*    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'ICON_SYSTEM_UNDO'.
        PERFORM f_close_bottom.
      WHEN 'ICON_PREVIOUS_OBJECT'.
        PERFORM f_reiniciar_checklist.
      WHEN 'ICON_QUESTION'.
        PERFORM f_preencher_checklist.
      WHEN OTHERS.
    ENDCASE.

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
        IMPORTING e_ucomm,

      handle_data_changed FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

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



  ENDMETHOD.

  METHOD handle_data_changed.
    BREAK-POINT.
  ENDMETHOD.

  METHOD handle_toolbar_bottom.

    DATA: ls_toolbar TYPE stb_button.

    "--- Append a separator to normal toolbar
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    IF p_co_chk IS INITIAL.

      CLEAR ls_toolbar.
      MOVE 'ICON_DELETE_ROW' TO ls_toolbar-function.
      MOVE icon_delete_row TO ls_toolbar-icon.
      MOVE 'Excluir' TO ls_toolbar-quickinfo.
      MOVE 'Excluir' TO ls_toolbar-text.
      MOVE ' ' TO ls_toolbar-disabled.
      APPEND ls_toolbar TO e_object->mt_toolbar.

      "--- Append a separator to normal toolbar
      CLEAR ls_toolbar.
      MOVE 3 TO ls_toolbar-butn_type.
      APPEND ls_toolbar TO e_object->mt_toolbar.

    ENDIF.

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

      WHEN 'ICON_DELETE_ROW'.
        PERFORM f_excluir_linha.

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

  lw_variant-report       = sy-repid.
  lw_variant-username     = sy-uname.

  lw_layout-sel_mode   = 'A'.
  lw_layout-zebra      = 'X'.
  lw_layout-info_fname = 'COLOR'.
  lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.

  lw_layout-cwidth_opt = 'X'.

  lw_variant-report  = sy-repid.
  lw_variant-variant = lw_layout.

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

    CREATE OBJECT lo_handle.

    SET HANDLER lo_handle->handle_double_click FOR go_cc_alv_01.

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
FORM f_on_click USING e_row TYPE lvc_s_row
                      e_column TYPE lvc_s_col
                      es_row_no TYPE lvc_s_roid
                      es_sender.

  READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv>)
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
*&      Form  ROW_COLOR
*&---------------------------------------------------------------------*
FORM row_color  USING p_index TYPE i p_alv.

  DATA lt_stable TYPE lvc_s_stbl.
  DATA lw_cellcolor TYPE lvc_s_scol.

  PERFORM clear_row_color USING '1'.

  CASE p_alv.
    WHEN '1'.
      "--- Atualiza a cor da linha selecionado
      READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_dados>) INDEX p_index.
      IF sy-subrc = 0.
        <fs_dados>-color = 'C500'.
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
        CLEAR <fs_dados_1>-color.
      ENDLOOP.
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

  DATA lv_canc.

  READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>)
    INDEX p_index.

  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'ZSDMF_TELA_RESPONDER_CHECKLIST'
    EXPORTING
      iv_checklistid = <fs_alv1>-checklistid
      iv_simulador   = <fs_alv1>-doc_simulacao
      iv_bukrs       = <fs_alv1>-vkorg
      iv_vkbur       = <fs_alv1>-vkbur
      iv_edit        = p_no_chk
    IMPORTING
      ev_canc        = lv_canc.

  CHECK lv_canc IS INITIAL.


  PERFORM f_seleciona.
  PERFORM f_processa.

  PERFORM f_refresh_alv USING '1' abap_false.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM f_double_click2 USING p_index p_column.

  READ TABLE gt_dados_alv2 ASSIGNING FIELD-SYMBOL(<fs_alv2>)
    INDEX p_index.

  CHECK sy-subrc EQ 0.

  CHECK gv_block_bottom IS INITIAL.

  IF p_column = 'RADIO_SIM'.
    <fs_alv2>-flag_sim = abap_true.
    <fs_alv2>-flag_nao = abap_false.
  ENDIF.

  IF p_column = 'RADIO_NAO'.
    <fs_alv2>-flag_sim = abap_false.
    <fs_alv2>-flag_nao = abap_true.
  ENDIF.

  PERFORM f_alv_2_icons CHANGING <fs_alv2>.

  PERFORM f_refresh_alv USING '2' abap_true.

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
    ls_layout_i-box_fname = 'SELEC'.
    "ls_layout_i-col_opt = 'X'.
    "ls_layout_i-cwidth_opt = 'X'.
    "ls_layout_i-no_colexpd = 'X'.
    "ls_layout_i-ctab_fname = 'CELLCOLOR'.
    "ls_layout_i-grid_title = 'Agrupador'.
    "ls_variant-report  = sy-repid.
    "ls_variant-handle  = 'COMP'.
    "ls_variant-variant = '/DEFAULT'.

    CALL METHOD go_cc_alv_02->set_table_for_first_display
      EXPORTING
        is_layout            = ls_layout_i
        is_variant           = ls_variant
        i_save               = 'A'
        it_toolbar_excluding = gt_exclude
      CHANGING
        it_outtab            = p_t_alv[]
        it_fieldcatalog      = gt_fieldcat2.

    CREATE OBJECT lo_application.

    SET HANDLER lo_application->handle_toolbar_bottom FOR go_cc_alv_02.
    SET HANDLER lo_application->handle_user_command   FOR go_cc_alv_02.
    SET HANDLER lo_application->handle_double_click  FOR go_cc_alv_02.
    "SET HANDLER lo_application->handle_data_changed FOR go_cc_alv_02.

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
        CLEAR <fs_alv_o>-color.
      ENDLOOP.


    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_refresh_alv
*&---------------------------------------------------------------------*
FORM f_refresh_alv USING uv_alv TYPE c
                         uv_changed TYPE c.

  CASE uv_alv.
    WHEN '1'.

      IF go_cc_alv_01 IS BOUND.
        go_cc_alv_01->refresh_table_display( ).
      ENDIF.

    WHEN '2'.

      IF go_cc_alv_02 IS BOUND.
        go_cc_alv_02->refresh_table_display( ).
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

  CHECK uv_changed IS NOT INITIAL.

  PERFORM f_set_global_change.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_global_change
*&---------------------------------------------------------------------*
FORM f_set_global_change .

  gv_changed = abap_true.

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
*& Form f_save
*&---------------------------------------------------------------------*
FORM f_save .

  DATA lt_0381 TYPE TABLE OF zsdt0381.
  DATA lt_0382 TYPE TABLE OF zsdt0382.
  DATA lv_inconfor TYPE c.
  DATA lv_checklistid TYPE zsdchecklistid.

  CHECK gv_changed IS NOT INITIAL.

  LOOP AT gt_dados_alv2 ASSIGNING FIELD-SYMBOL(<fs_alv2>).

    APPEND INITIAL LINE TO lt_0382 ASSIGNING FIELD-SYMBOL(<fs_0382>).

    <fs_0382>-doc_simulacao = <fs_alv2>-doc_simulacao.
    lv_checklistid = <fs_0382>-checklistid = <fs_alv2>-checklistid.
    <fs_0382>-checkid = <fs_alv2>-checkid.
    <fs_0382>-pergunta = <fs_alv2>-pergunta.

    <fs_0382>-flag_sim = <fs_alv2>-flag_sim.
    <fs_0382>-flag_nao = <fs_alv2>-flag_nao.
    <fs_0382>-user_create = sy-uname.
    <fs_0382>-date_create = sy-datum.
    <fs_0382>-time_create = sy-uzeit.

  ENDLOOP.

  LOOP AT gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>) WHERE doc_simulacao = gs_clicked_alv_01-doc_simulacao.

    APPEND INITIAL LINE TO lt_0381 ASSIGNING FIELD-SYMBOL(<fs_0381>).

    <fs_0381>-doc_simulacao = <fs_alv1>-doc_simulacao.
    <fs_0381>-checklistid = lv_checklistid.
    <fs_0381>-status = '01'. "<--- PENDENTE DE APROVAÇÃO

    <fs_0381>-bukrs = <fs_alv1>-vkorg.
    <fs_0381>-vkbur = <fs_alv1>-vkbur.
    <fs_0381>-descr = <fs_alv1>-descr.

    <fs_0381>-user_create = sy-uname.
    <fs_0381>-date_create = sy-datum.
    <fs_0381>-time_create = sy-uzeit.

  ENDLOOP.

  IF lt_0382 IS NOT INITIAL.
    MODIFY zsdt0382 FROM TABLE lt_0382.
  ENDIF.

  IF lt_0381 IS NOT INITIAL.
    MODIFY zsdt0381 FROM TABLE lt_0381.
  ENDIF.

  PERFORM f_commit_work.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_2
*&---------------------------------------------------------------------*
FORM f_processa_2 .

  LOOP AT gt_dados_alv2 ASSIGNING FIELD-SYMBOL(<fs_alv2>).

    PERFORM f_alv_2_icons CHANGING <fs_alv2>.

  ENDLOOP.

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
*& Form F_ALV_2_ICONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <FS_ALV2>
*&---------------------------------------------------------------------*
FORM f_alv_2_icons CHANGING cs_alv2 TYPE zsds0006.

  IF cs_alv2-ativo IS INITIAL.
    cs_alv2-icon = '@02@'.
  ELSE.
    cs_alv2-icon = '@01@'.
  ENDIF.

  IF cs_alv2-flag_sim IS INITIAL.
    cs_alv2-radio_sim = '@SR@'.
  ELSE.
    cs_alv2-radio_sim = '@R6@'.
  ENDIF.

  IF cs_alv2-flag_nao IS INITIAL.
    cs_alv2-radio_nao = '@SR@'.
  ELSE.
    cs_alv2-radio_nao = '@R6@'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_reiniciar_checklist
*&---------------------------------------------------------------------*
FORM f_reiniciar_checklist .

*  DATA lt_0381 TYPE TABLE OF zsdt0381.
*  DATA lt_0382 TYPE TABLE OF zsdt0382.

  DATA lt_row TYPE lvc_t_roid.
  DATA lv_erro.

  PERFORM f_get_selected_rows
    USING go_cc_alv_01
   CHANGING lt_row lv_erro.

  CHECK lv_erro IS INITIAL.

  LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<fs_row>).

    READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>)
      INDEX <fs_row>-row_id.

    CHECK sy-subrc EQ 0.

    SELECT COUNT(*) FROM zsdt0387
        WHERE doc_simulacao = <fs_alv1>-doc_simulacao.

    IF sy-dbcnt > 0.

      MESSAGE s016(ds) WITH 'Impossível reiniciar,' 'Estrategia já foi' 'iniciada' DISPLAY LIKE 'E'.
      lv_erro = 'X'.
      EXIT.
    ENDIF.

    DELETE FROM zsdt0381 WHERE doc_simulacao = <fs_alv1>-doc_simulacao
                      AND checklistid = <fs_alv1>-checklistid.

    DELETE FROM zsdt0382 WHERE doc_simulacao = <fs_alv1>-doc_simulacao
                      AND checklistid = <fs_alv1>-checklistid.

  ENDLOOP.

  CHECK lv_erro IS INITIAL.

  PERFORM f_commit_work.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_commit_work
*&---------------------------------------------------------------------*
FORM f_commit_work .

  COMMIT WORK AND WAIT.

  gv_changed = abap_false.

  CLEAR gs_clicked_alv_01.

  MESSAGE s016(ds) WITH 'Dados gravados'.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_excluir_linha
*&---------------------------------------------------------------------*
FORM f_excluir_linha .

  DATA lt_row TYPE lvc_t_roid.
  DATA lv_erro.

  PERFORM f_get_selected_rows
    USING go_cc_alv_02
   CHANGING lt_row lv_erro.

  CHECK lv_erro = abap_false.

  LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<fs_row>).

    DELETE gt_dados_alv2 INDEX <fs_row>-row_id.

  ENDLOOP.

  PERFORM f_refresh_alv USING '2' abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preencher_checklist
*&---------------------------------------------------------------------*
FORM f_preencher_checklist .

  DATA lt_row TYPE lvc_t_roid.
  DATA lv_erro.

  PERFORM f_get_selected_rows
    USING go_cc_alv_01
   CHANGING lt_row lv_erro.

  CHECK lv_erro IS INITIAL.

  LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<fs_row>).

    READ TABLE gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>)
      INDEX <fs_row>-row_id.

    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZSDMF_TELA_RESPONDER_CHECKLIST'
      EXPORTING
        iv_checklistid = <fs_alv1>-checklistid
        iv_simulador   = <fs_alv1>-doc_simulacao
        iv_bukrs       = <fs_alv1>-vkorg
        iv_vkbur       = <fs_alv1>-vkbur
        iv_edit        = p_no_chk
      IMPORTING
        ev_canc        = lv_erro.

    CHECK lv_erro IS INITIAL.

  ENDLOOP.

ENDFORM.
