* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZSDR0119                                               *
* Title.......: Correção disparo do Hedge                              *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 11/06/2024                                             *
* -------------------------------------------------------------------- *
REPORT zsdr0119_in.

TYPE-POOLS: slis, abap, icon.

TABLES: zsde0171_saida, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZSDE0171_SAIDA'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

DATA go_cc_cus_01 TYPE REF TO cl_gui_custom_container.
DATA go_cc_alv_01 TYPE REF TO cl_gui_alv_grid.

"dados alv
DATA gt_dados_alv TYPE STANDARD TABLE OF zsde0171_saida.
DATA gt_fieldcat TYPE lvc_t_fcat.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.
DATA gt_tipo_disparo TYPE TABLE OF zsded_tp_disparo.
DATA gs_data TYPE RANGE OF sy-datum.

DATA gt_0090 TYPE TABLE OF zsdt0090.
DATA gt_vbak TYPE TABLE OF zsde0171_vbak.
DATA gt_vbap TYPE TABLE OF zsde0171_vbap.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS rb_mi RADIOBUTTON GROUP rg01 USER-COMMAND comm.
  PARAMETERS rb_in RADIOBUTTON GROUP rg01.
  PARAMETERS rb_sa RADIOBUTTON GROUP rg01.
  PARAMETERS rb_mm RADIOBUTTON GROUP rg01.
  PARAMETERS rb_todos RADIOBUTTON GROUP rg01.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS so_vkorg FOR zsde0171_saida-vkorg NO-EXTENSION NO INTERVALS.
  SELECT-OPTIONS so_sim FOR zsde0171_saida-doc_simulacao NO-EXTENSION NO INTERVALS.
  SELECT-OPTIONS so_data FOR zsde0171_saida-data_atual.
  PARAMETERS p_vist AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  PERFORM f_processa_tela.

START-OF-SELECTION.

  PERFORM f_seleciona.

  IF sy-batch = abap_true.
    PERFORM f_processa.
  ELSE.
    PERFORM f_exibe_alv.
  ENDIF.

  " 03.04.2025 - 170634 - RAMON -->
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION .
    METHODS:
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command_itens FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD on_toolbar.
    PERFORM f_toolbar USING e_object.
  ENDMETHOD.

  METHOD handle_user_command_itens.
    PERFORM f_marcar_linha USING e_ucomm.
  ENDMETHOD.


ENDCLASS.
" 03.04.2025 - 170634 - RAMON --<

CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,

      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD  handle_double_click.
    PERFORM f_on_click USING e_row e_column es_row_no.
  ENDMETHOD.                    "on_user_command

  METHOD  handle_data_changed.
    PERFORM f_data_changed USING er_data_changed.
  ENDMETHOD.                    "on_user_command

ENDCLASS.                    "lcl_handle_events DEFINITION
*&---------------------------------------------------------------------*
*&      Form  F_ON_CLICK
*&---------------------------------------------------------------------*
FORM f_on_click USING e_row TYPE lvc_s_row
                      e_column TYPE lvc_s_col
                      es_row_no TYPE lvc_s_roid.

  READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)
    INDEX e_row-index.

  CHECK sy-subrc EQ 0.

  CASE e_column.
    WHEN 'DOC_SIMULACAO'.
      PERFORM f_call_zsdt0087 USING <fs_alv>.
    WHEN 'VL_T0094'.
      PERFORM f_call_zsdt0090 USING <fs_alv>.
    WHEN OTHERS.
  ENDCASE.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  CLEAR: gt_dados_alv[], gt_bapiret2[], gt_tipo_disparo[].

  PERFORM f_init.

  PERFORM f_debugger. CHECK gt_dados_alv IS INITIAL.

  PERFORM f_seleciona_mi.

  PERFORM f_seleciona_in.

  PERFORM f_seleciona_sa.

  PERFORM f_seleciona_mm.

  CHECK p_vist IS INITIAL.

  DELETE gt_dados_alv WHERE desconsiderar = abap_true.

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

  IF gt_dados_alv IS NOT INITIAL.

    CALL SCREEN 100.

  ELSE.

    MESSAGE s213(v4) DISPLAY LIKE 'E'.
    EXIT.

  ENDIF.

ENDFORM.                    " F_EXIBE_ALV
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat.

  CLEAR gt_fieldcat.

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

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  DELETE gt_fieldcat WHERE fieldname = 'SELEC'.
  DELETE gt_fieldcat WHERE fieldname = 'ICON'.
  DELETE gt_fieldcat WHERE fieldname = 'MSGTX'.

  DELETE gt_fieldcat WHERE fieldname = 'COLOR'.
  DELETE gt_fieldcat WHERE fieldname = 'VKORG'.
  DELETE gt_fieldcat WHERE fieldname = 'VKBUR'.

  DELETE gt_fieldcat WHERE fieldname = 'DATA_ATUAL'.
  DELETE gt_fieldcat WHERE fieldname = 'AREA_PROC'.
  DELETE gt_fieldcat WHERE fieldname = 'BEZEI'.

  DELETE gt_fieldcat WHERE fieldname = 'VL_T0094_BRL'.
  DELETE gt_fieldcat WHERE fieldname = 'VL_T0094_USD'.
  DELETE gt_fieldcat WHERE fieldname = 'VL_T0059_BRL'.
  DELETE gt_fieldcat WHERE fieldname = 'VL_T0059_USD'.
  DELETE gt_fieldcat WHERE fieldname = 'DIFERENCA_BRL'.
  DELETE gt_fieldcat WHERE fieldname = 'DIFERENCA_USD'.
  DELETE gt_fieldcat WHERE fieldname = 'DESCONSIDERAR'.

  PERFORM f_coluna_edita USING 'DOC_SIMULACAO' 'Solicitação'.
  PERFORM f_coluna_edita USING 'FIXACAO' 'Fixação'.
  PERFORM f_coluna_edita USING 'TP_VENDA' 'Tipo'.
  PERFORM f_coluna_edita USING 'SPART' 'Atividade'.
  PERFORM f_coluna_edita USING 'TPSIM' 'Cond.PG'.
  PERFORM f_coluna_edita USING 'WAERK' 'Moeda'.
  PERFORM f_coluna_edita USING 'STATUS' 'Status'.

  PERFORM f_coluna_edita USING 'TIPO' 'Tp.Venda'.
  PERFORM f_coluna_edita USING 'VL_T0094' 'Vl Hedge'.
  PERFORM f_coluna_edita USING 'VL_T0059' 'Vl Simulador'.
  PERFORM f_coluna_edita USING 'DIFERENCA' 'Diferença'.

  PERFORM f_coluna_edita USING 'SEM_EMAIL' 'Sem Envia Email'.
  PERFORM f_fieldcat_modi USING 'SEM_EMAIL' 'EDIT' 'X' CHANGING gt_fieldcat.
  PERFORM f_fieldcat_modi USING 'SEM_EMAIL' 'CHECKBOX' 'X' CHANGING gt_fieldcat.

  CASE abap_true.
    WHEN rb_mi.
    WHEN rb_in.
      DELETE gt_fieldcat WHERE fieldname = 'FIXACAO'.
    WHEN rb_sa.
    WHEN rb_mm.
    WHEN rb_todos.
  ENDCASE.

ENDFORM.                    " F_MONTA_FIELDCAT
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

  IF sy-batch = 'X'.

    IF strlen( p_text ) > 40.
      MESSAGE s000(d2) WITH p_text(40) p_text+40.
    ELSE.
      MESSAGE s000(d2) WITH p_text.
    ENDIF.

  ELSE.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = p_percent
        text       = p_text.

  ENDIF.

ENDFORM.
*& --------------------------------------------------------------------*
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
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
FORM f_coluna_edita  USING p_fieldname TYPE slis_fieldname
                           p_text TYPE scrtext_l.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-scrtext_s = p_text.
  <fs_cat>-scrtext_m = p_text.
  <fs_cat>-scrtext_l = p_text.
  <fs_cat>-reptext = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VERIFICA_LINHA_SELEC
*&---------------------------------------------------------------------*
FORM f_verifica_linha_selec CHANGING p_error TYPE c.


  "IF REDUCE i( INIT x = 0 FOR ls IN it_saida WHERE ( check IS NOT INITIAL ) NEXT x = x + 1 ) NE 1.
  "MESSAGE 'Selecione uma linha por vez' TYPE 'I'.
  "EXIT.
  "ENDIF.

  READ TABLE gt_dados_alv WITH KEY selec = 'X' TRANSPORTING NO FIELDS.

  IF sy-subrc NE 0.
    MESSAGE s851(v4) DISPLAY LIKE 'E'.
    p_error = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa
*&---------------------------------------------------------------------*
FORM f_processa.

  DATA lr_selec TYPE RANGE OF flag.

  DATA lv_ret.

  IF sy-batch IS INITIAL.

    PERFORM f_verifica_linha_selec CHANGING lv_ret.

    CHECK lv_ret IS INITIAL.

    PERFORM f_popup_to_confirm USING TEXT-t01 CHANGING lv_ret.

    CHECK lv_ret = '1'.

    APPEND 'IEQX' TO lr_selec.

  ELSE.
    CLEAR lr_selec.

  ENDIF.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec IN lr_selec.



  ENDLOOP.

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
*& Form f_init
*&---------------------------------------------------------------------*
FORM f_init .

  " -------- DATA

  gs_data[] = so_data[].

  IF gs_data[] IS INITIAL.

    APPEND INITIAL LINE TO gs_data ASSIGNING FIELD-SYMBOL(<fs_data>).

    <fs_data>-sign = 'I'.
    <fs_data>-option = 'BT'.
    <fs_data>-low = '20200101'.
    <fs_data>-high = sy-datum.

  ELSE.

    READ TABLE gs_data ASSIGNING <fs_data> INDEX 1.

    IF sy-subrc EQ 0.

      IF <fs_data>-low <= '20200101'..
        <fs_data>-low = '20200101'.
      ENDIF.

    ENDIF.

  ENDIF.

  " ---- TIPO DISPARO

  APPEND 'VDI' TO gt_tipo_disparo.
  APPEND 'FRI' TO gt_tipo_disparo.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_seleciona_mi
*&---------------------------------------------------------------------*
FORM f_seleciona_mi .

  CHECK rb_mi = abap_true OR rb_todos = abap_true.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_seleciona_in
*&---------------------------------------------------------------------*
FORM f_seleciona_in .

  DATA lt_0040 TYPE TABLE OF zsdt0040.
  DATA lt_0117 TYPE TABLE OF zsdt0117.
  DATA lt_0041 TYPE TABLE OF zsdt0041.
  DATA lt_0094 TYPE TABLE OF zsdt0094.
  DATA lt_0037 TYPE TABLE OF zsdt0037.
  DATA lt_ovs TYPE RANGE OF vbeln.

  DATA lt_nro_sol_ov TYPE zrsdsselopts.

  DATA lv_vlr_sim_brl TYPE zdmbtr.
  DATA lv_vlr_sim_usd TYPE zdmbtr.
  DATA lv_taxa TYPE kursf.
  DATA lv_trv_ant TYPE flag. "<- flag se houver trv antiga de cambio

  CHECK rb_in = abap_true OR rb_todos = abap_true..

  SELECT 'I' AS sign, 'EQ' AS option, doc_simulacao AS low FROM zsdt0040
    INTO TABLE @lt_nro_sol_ov
      WHERE data_atual    IN @gs_data
        AND vkorg         IN @so_vkorg
        AND doc_simulacao IN @so_sim.

  CHECK sy-subrc EQ 0.

  SORT lt_nro_sol_ov BY low.

  DELETE ADJACENT DUPLICATES FROM lt_nro_sol_ov COMPARING low.

  SELECT * FROM zsdt0040
   INTO TABLE lt_0040
    WHERE doc_simulacao IN lt_nro_sol_ov.

  CHECK lt_0040 IS NOT INITIAL.

  SELECT * FROM zsdt0117
    INTO TABLE lt_0117
    FOR ALL ENTRIES IN lt_0040
   WHERE bukrs      EQ lt_0040-vkorg
     AND desativado EQ abap_false.

  SELECT * FROM zsdt0041
    INTO TABLE lt_0041
   WHERE doc_simulacao IN lt_nro_sol_ov.

  SELECT * FROM zsdt0094
    INTO TABLE lt_0094
   WHERE nro_sol_ov IN lt_nro_sol_ov.

  SELECT *
    FROM zsdt0090
    INTO TABLE gt_0090
   WHERE doc_simulacao IN lt_nro_sol_ov
     AND estorno EQ abap_false
     AND categoria NOT IN ( 'F','G','P' ).

  LOOP AT lt_0041 ASSIGNING FIELD-SYMBOL(<fs_0041>).

    IF <fs_0041>-vbeln IS NOT INITIAL.
      APPEND 'IEQ' && <fs_0041>-vbeln TO lt_ovs.
    ENDIF.

  ENDLOOP.

  LOOP AT gt_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>).

    IF <fs_0090>-vbelv IS NOT INITIAL.
      APPEND 'IEQ' && <fs_0090>-vbelv TO lt_ovs.
    ENDIF.

    IF <fs_0090>-vbeln IS NOT INITIAL.
      APPEND 'IEQ' && <fs_0090>-vbeln TO lt_ovs.
    ENDIF.

  ENDLOOP.

  SORT lt_ovs ASCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_ovs.

  IF lt_ovs IS NOT INITIAL.

    SELECT vbak~vbeln, vbtyp, vbak~vbeln AS vbeln_o, knumv, vbkd~kurrf FROM vbak
      INNER JOIN vbkd ON vbkd~vbeln = vbak~vbeln AND posnr = '000000'
      INTO TABLE @gt_vbak
      FOR ALL ENTRIES IN @lt_ovs
     WHERE vbak~vbeln EQ @lt_ovs-low.
    "AND vbak~vbtyp = 'C'."<-- Somente OV

    DELETE gt_vbak WHERE vbtyp = 'L'. " <-- retira Nota de credito

    CHECK gt_vbak IS NOT INITIAL.

    LOOP AT lt_ovs ASSIGNING FIELD-SYMBOL(<fs_ovs>).

      DATA(lv_vbeln) = <fs_ovs>-low.

      READ TABLE gt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
        WITH KEY vbeln = lv_vbeln.

      CHECK sy-subrc EQ 0.

      READ TABLE gt_0090 ASSIGNING FIELD-SYMBOL(<fs_0090_o>)
        WITH KEY vbeln = lv_vbeln.

      IF sy-subrc EQ 0.

        <fs_vbak>-doc_sim = <fs_0090_o>-doc_simulacao.

        IF <fs_0090_o>-vbelv IS NOT INITIAL.
          <fs_vbak>-vbeln_o = <fs_0090_o>-vbelv.
        ELSE.
          <fs_vbak>-vbeln_o = lv_vbeln.
        ENDIF.

      ELSE.

        <fs_vbak>-vbeln_o = lv_vbeln.

        READ TABLE lt_0041 ASSIGNING <fs_0041>
          WITH KEY vbeln = lv_vbeln.

        IF sy-subrc EQ 0.
          <fs_vbak>-doc_sim = <fs_0041>-doc_simulacao.
        ENDIF.

      ENDIF.

    ENDLOOP.

    SELECT vbap~vbeln, vbap~posnr, vbap~matnr, spart, netwr,vbap~waerk,
      brgew, mwsbp, kwmeng, kmein, lifsp FROM vbap AS vbap
      LEFT JOIN vbep ON vbep~vbeln = vbap~vbeln
                     AND vbep~posnr = vbap~posnr
                     AND vbep~etenr = '0001'
      INTO TABLE @gt_vbap
        FOR ALL ENTRIES IN @gt_vbak
          WHERE vbap~vbeln EQ @gt_vbak-vbeln.

  ENDIF.

  "BREAK rblima.

  LOOP AT gt_tipo_disparo ASSIGNING FIELD-SYMBOL(<fs_disparo>).

    LOOP AT lt_0040 ASSIGNING FIELD-SYMBOL(<fs_0040>).

      CLEAR: lv_taxa, lv_vlr_sim_brl, lv_vlr_sim_usd.

      READ TABLE lt_0041 ASSIGNING <fs_0041>
        WITH KEY doc_simulacao = <fs_0040>-doc_simulacao.

      CHECK sy-subrc EQ 0.

      APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

      PERFORM f_check_trv_antiga USING <fs_0040>-doc_simulacao CHANGING lv_trv_ant.

      <fs_alv>-vkorg = <fs_0040>-vkorg.
      <fs_alv>-vkbur = <fs_0040>-vkbur.
      <fs_alv>-data_atual = <fs_0040>-data_atual.
      <fs_alv>-area_proc = 'IN'.
      <fs_alv>-doc_simulacao = <fs_0040>-doc_simulacao.
      <fs_alv>-fixacao = '0000'. "<-- sem fixação para insumos
      <fs_alv>-tp_diparo = <fs_disparo>.
      <fs_alv>-spart = <fs_0041>-spart.
      <fs_alv>-tpsim = <fs_0040>-tpsim.
      <fs_alv>-waerk = <fs_0040>-waerk.
      <fs_alv>-status = <fs_0040>-status.
      <fs_alv>-bezei = SWITCH #( <fs_0041>-spart
                                      WHEN '03' THEN 'DF'
                                      WHEN '02' THEN 'FT'
                                      WHEN '04' THEN |S{ <fs_0040>-cultura(1) }|
                                    ).


      "<fs_alv>-TP_VENDA =  .

      CLEAR <fs_alv>-vl_t0094_brl.
      CLEAR <fs_alv>-vl_t0094_usd.
      CLEAR <fs_alv>-vl_t0059_brl.
      CLEAR <fs_alv>-vl_t0059_usd.

      IF <fs_0040>-kursf IS NOT INITIAL.
        lv_taxa = <fs_0040>-kursf.
      ENDIF.

      LOOP AT lt_0094 ASSIGNING FIELD-SYMBOL(<fs_0094>)
          WHERE nro_sol_ov = <fs_alv>-doc_simulacao
            AND tipo = <fs_alv>-tp_diparo.

        <fs_alv>-vl_t0094_brl = <fs_alv>-vl_t0094_brl + <fs_0094>-total_proporc.

        CHECK <fs_0094>-taxa_cambio IS NOT INITIAL.
        <fs_alv>-vl_t0094_usd = <fs_alv>-vl_t0094_brl / <fs_0094>-taxa_cambio.

      ENDLOOP.

      IF lv_taxa IS INITIAL AND <fs_0094> IS ASSIGNED.
        lv_taxa = <fs_0094>-taxa_cambio.
      ENDIF.

      " proteção do codigo contra dump
      IF lv_taxa IS INITIAL.
        lv_taxa = 1.
      ENDIF.

      IF <fs_alv>-tp_diparo = 'VDI'.

        PERFORM f_valor_simulador2
          USING <fs_alv>-doc_simulacao
                <fs_alv>-waerk
                lv_taxa
                lv_trv_ant
       CHANGING lv_vlr_sim_brl
                lv_vlr_sim_usd.

        " se o valor do simulador estiver vazio, significa que nao tem ov ainda, então pega o valor total da 0040
        " AND não pode ter registro de encerramento na tabela ZSDT0090
        IF lv_vlr_sim_brl IS INITIAL AND NOT ( line_exists( gt_0090[ doc_simulacao = <fs_alv>-doc_simulacao categoria = 'E' ] ) ).

          IF <fs_alv>-waerk = 'USD'.

            lv_vlr_sim_brl = <fs_0040>-vlrtot * lv_taxa.
            lv_vlr_sim_usd = <fs_0040>-vlrtot.

          ELSE.

            lv_vlr_sim_brl = <fs_0040>-vlrtot.
            lv_vlr_sim_usd = <fs_0040>-vlrtot / lv_taxa.

          ENDIF.

        ENDIF.

      ELSE.

*        PERFORM f_valor_frete
*          USING <fs_alv>-doc_simulacao
*                <fs_alv>-spart
*       CHANGING lv_vlr_sim.

      ENDIF.

      <fs_alv>-vl_t0059_usd = lv_vlr_sim_usd.
      <fs_alv>-vl_t0059_brl = lv_vlr_sim_brl.

*      IF <fs_alv>-waerk = 'USD'.
*
*        <fs_alv>-vl_t0059_usd = lv_vlr_sim.
*        <fs_alv>-vl_t0059_brl = lv_vlr_sim * lv_taxa.
*
*      ELSEIF <fs_alv>-waerk = 'BRL'.
*
*        <fs_alv>-vl_t0059_brl = lv_vlr_sim.
*        <fs_alv>-vl_t0059_usd = lv_vlr_sim / lv_taxa.
*
*      ENDIF.

      <fs_alv>-diferenca_brl = <fs_alv>-vl_t0094_brl - <fs_alv>-vl_t0059_brl.
      <fs_alv>-diferenca_usd = <fs_alv>-vl_t0094_usd - <fs_alv>-vl_t0059_usd.

*      IF <fs_alv>-waerk = 'BRL'.

      <fs_alv>-vl_t0094 = <fs_alv>-vl_t0094_brl .
      <fs_alv>-vl_t0059 = <fs_alv>-vl_t0059_brl.
      <fs_alv>-diferenca = <fs_alv>-diferenca_brl.

*      ELSEIF <fs_alv>-waerk = 'USD'.
*
*        <fs_alv>-vl_t0094 = <fs_alv>-vl_t0094_usd .
*        <fs_alv>-vl_t0059 = <fs_alv>-vl_t0059_usd.
*        <fs_alv>-diferenca = <fs_alv>-diferenca_usd.
*
*      ENDIF.

      PERFORM f_valida_exibicao CHANGING <fs_alv>.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_seleciona_sa
*&---------------------------------------------------------------------*
FORM f_seleciona_sa .

  CHECK rb_sa = abap_true OR rb_todos = abap_true..


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_seleciona_mm
*&---------------------------------------------------------------------*
FORM f_seleciona_mm .

  CHECK rb_mm = abap_true OR rb_todos = abap_true..


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_debugger
*&---------------------------------------------------------------------*
FORM f_debugger .

  CHECK sy-sysid = 'DEV'.

  DO 20 TIMES.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-vkorg = sy-index.
    <fs_alv>-data_atual = sy-index.
    <fs_alv>-area_proc = 'IN'.
    <fs_alv>-doc_simulacao = sy-index.
    <fs_alv>-fixacao = sy-index.
    <fs_alv>-tp_venda = 'VV'.
    <fs_alv>-spart = sy-index.
    <fs_alv>-tpsim = 'SS'.
    <fs_alv>-waerk = 'BRL'.
    <fs_alv>-status = 'A'.
    <fs_alv>-bezei = 'DF'.

*    <fs_alv>-tipo = 'A'.
    <fs_alv>-vl_t0094_brl = '100000.00'.
    <fs_alv>-vl_t0059_brl = '34020.10'.
    <fs_alv>-diferenca_brl = <fs_alv>-vl_t0094_brl - <fs_alv>-vl_t0059_brl.

  ENDDO.


ENDFORM.
*&---------------------------------------------------------------------*
*& Module ALV_01_INIT OUTPUT
*&---------------------------------------------------------------------*
MODULE alv_01_init OUTPUT.

  DATA lw_settings TYPE lvc_s_glay.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.

  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_event_handler.
  DATA lo_ontool TYPE REF TO lcl_alv_toolbar.

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

    CREATE OBJECT lo_ontool.
    SET HANDLER lo_ontool->on_toolbar FOR go_cc_alv_01.
    SET HANDLER lo_ontool->handle_user_command_itens FOR go_cc_alv_01.

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

    PERFORM f_refresh_alv_table.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  "SET PF-STATUS 'STANDARD'.
  SET PF-STATUS 'PF0100'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM f_gravar_dados.
    WHEN 'DIFE'.
      PERFORM f_executa_correcao_hedge.
      PERFORM f_refresh_alv.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE PROGRAM.
    WHEN 'REFRESH'.
      PERFORM f_refresh_alv.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_valor_simulador
*&---------------------------------------------------------------------*
FORM f_valor_simulador2 USING uv_doc_sim TYPE zsded003
                              uv_waerk TYPE waerk
                              uv_kursf TYPE kursf
                              uv_trv_ant TYPE c
                     CHANGING cv_vlr_sim_brl TYPE zdmbtr
                              cv_vlr_sim_usd TYPE zdmbtr.

  DATA lv_brgew TYPE brgew_ap.
  DATA lv_netwr_sum_brl TYPE netwr_ap.
  DATA lv_mwsbp_sum_brl TYPE mwsbp.

  DATA lv_netwr_sum_usd TYPE netwr_ap.
  DATA lv_mwsbp_sum_usd TYPE mwsbp.

  DATA lv_netwr_brl TYPE netwr_ap.
  DATA lv_mwsbp_brl TYPE mwsbp.

  DATA lv_netwr_usd TYPE netwr_ap.
  DATA lv_mwsbp_usd TYPE mwsbp.

  DATA lv_continua TYPE c.
*  " Verificar se existe trava de cambio nova
*  PERFORM f_trava_cambio_nova
*    USING uv_doc_sim
* CHANGING cv_vlr_sim.

*  CHECK cv_vlr_sim IS INITIAL.

  LOOP AT gt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>) WHERE doc_sim = uv_doc_sim.

    IF uv_trv_ant = abap_true.

      " VERIFICA SE É TRV ANTIGA
      PERFORM f_tem_trv_antiga USING <fs_vbak> CHANGING lv_continua.

      CHECK lv_continua = abap_true.

    ENDIF.

    LOOP AT gt_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>)
      WHERE vbeln = <fs_vbak>-vbeln.

      CHECK <fs_vbap>-lifsp <> '12'.

      IF <fs_vbak>-kursk IS NOT INITIAL AND uv_waerk = 'USD'.

        lv_netwr_usd = <fs_vbap>-netwr.
        lv_mwsbp_usd = <fs_vbap>-mwsbp.

        lv_netwr_brl = <fs_vbap>-netwr * <fs_vbak>-kursk.
        lv_mwsbp_brl = <fs_vbap>-mwsbp * <fs_vbak>-kursk.

      ELSE.

        lv_netwr_brl = <fs_vbap>-netwr.
        lv_mwsbp_brl = <fs_vbap>-mwsbp.

        lv_netwr_usd = <fs_vbap>-netwr / uv_kursf.
        lv_mwsbp_usd = <fs_vbap>-mwsbp / uv_kursf.

      ENDIF.

      IF <fs_vbak>-vbtyp = 'C'. "<-- se for ov soma

        ADD <fs_vbap>-brgew TO lv_brgew.

        ADD lv_netwr_brl TO lv_netwr_sum_brl.
        ADD lv_mwsbp_brl TO lv_mwsbp_sum_brl.

        ADD lv_netwr_usd TO lv_netwr_sum_usd.
        ADD lv_mwsbp_usd TO lv_mwsbp_sum_usd.

        " se for devolução deduz
      ELSE.

        SUBTRACT <fs_vbap>-brgew FROM lv_brgew.

        SUBTRACT lv_netwr_brl FROM lv_netwr_sum_brl.
        SUBTRACT lv_mwsbp_brl FROM lv_mwsbp_sum_brl.

        SUBTRACT lv_netwr_usd FROM lv_netwr_sum_usd.
        SUBTRACT lv_mwsbp_usd FROM lv_mwsbp_sum_usd.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

  " Valor do simulador, é a soma das OVS + IMPOSTOS
  cv_vlr_sim_brl = lv_netwr_sum_brl + lv_mwsbp_sum_brl.
  cv_vlr_sim_usd = lv_netwr_sum_usd + lv_mwsbp_sum_usd.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_valor_simulador
*&---------------------------------------------------------------------*
FORM f_valor_simulador USING uv_doc_sim TYPE zsded003
                             uv_waerk TYPE waerk
                             uv_trv_ant TYPE c
                    CHANGING cv_vlr_sim  TYPE zdmbtr.

  DATA lv_brgew TYPE brgew_ap.
  DATA lv_netwr TYPE netwr_ap.
  DATA lv_mwsbp TYPE mwsbp.

  CLEAR cv_vlr_sim.

  " Verificar se existe trava de cambio nova
  PERFORM f_trava_cambio_nova
    USING uv_doc_sim
 CHANGING cv_vlr_sim.

  CHECK cv_vlr_sim IS INITIAL.

  LOOP AT gt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>) WHERE doc_sim = uv_doc_sim.

    IF uv_trv_ant = abap_true.

      " VERIFICA SE É TRV ANTIGA
      READ TABLE gt_0090 TRANSPORTING NO FIELDS
        WITH KEY vbelv = <fs_vbak>-vbeln
                 categoria = 'C'
                 prev_vl_liq_usd = '0.0000'.

      CHECK sy-subrc EQ 0.

    ENDIF.

    LOOP AT gt_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>)
      WHERE vbeln = <fs_vbak>-vbeln.

      CHECK <fs_vbap>-lifsp <> '12'.

      IF <fs_vbak>-vbtyp = 'C'. "<-- se for ov soma

        ADD <fs_vbap>-brgew TO lv_brgew.
        ADD <fs_vbap>-netwr TO lv_netwr.
        ADD <fs_vbap>-mwsbp TO lv_mwsbp.

        " se for devolução deduz
      ELSE.

        SUBTRACT <fs_vbap>-brgew FROM lv_brgew.
        SUBTRACT <fs_vbap>-netwr FROM lv_netwr.
        SUBTRACT <fs_vbap>-mwsbp FROM lv_mwsbp.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

  " Valor do simulador, é a soma das OVS + IMPOSTOS
  cv_vlr_sim = lv_netwr + lv_mwsbp.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_valor_simulador
*&---------------------------------------------------------------------*
FORM f_valor_frete USING uv_doc_sim TYPE zsded003
                         uv_spart   TYPE spart
                CHANGING cv_vlr_sim TYPE zdmbtr.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_soma_trava_cambio
*&---------------------------------------------------------------------*
FORM f_soma_valores_ov USING uv_doc_sim TYPE zsded003
                             uv_vbeln TYPE vbeln
                             uv_spart TYPE spart
                    CHANGING cv_brgew TYPE brgew_ap
                             cv_netwr TYPE netwr_ap
                             cv_mwsbp TYPE mwsbp.

  LOOP AT gt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>) WHERE doc_sim = uv_doc_sim AND vbeln = uv_vbeln.

    LOOP AT gt_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>)
      WHERE vbeln = <fs_vbak>-vbeln
        AND spart = uv_spart.

      CHECK <fs_vbap>-lifsp <> '12'.

      ADD <fs_vbap>-brgew TO cv_brgew.
      ADD <fs_vbap>-netwr TO cv_netwr.
      ADD <fs_vbap>-mwsbp TO cv_mwsbp.


    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_call_zsdt0087
*&---------------------------------------------------------------------*
FORM f_call_zsdt0087 USING us_line TYPE zsde0171_saida.

  SUBMIT zsdr0042
    WITH s_docsi EQ us_line-doc_simulacao
    WITH s_vkbur EQ us_line-vkbur
    WITH s_vkorg EQ us_line-vkorg AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_REFRESH_ALV
*&---------------------------------------------------------------------*
FORM f_refresh_alv .

  PERFORM f_seleciona.
  PERFORM f_refresh_alv_table.

ENDFORM.

FORM f_refresh_alv_table.

  DATA lw_layout TYPE lvc_s_layo.

  lw_layout-sel_mode = 'A'.
  lw_layout-col_opt = 'X'.
  lw_layout-cwidth_opt = 'X'.

  lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.

  CALL METHOD go_cc_alv_01->set_frontend_layout
    EXPORTING
      is_layout = lw_layout.

  CALL METHOD go_cc_alv_01->refresh_table_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXECUTA_CORRECAO_HEDGE
*&---------------------------------------------------------------------*
FORM f_executa_correcao_hedge.

  DATA lt_rows TYPE lvc_t_row.

  DATA lv_erro.

  PERFORM f_get_rows CHANGING lt_rows lv_erro.
  "BREAK rblima.
  CHECK lv_erro IS INITIAL.

  LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_index>).

    READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)
      INDEX <fs_index>-index.

    CHECK sy-subrc EQ 0.

    PERFORM f_correcao_insumos CHANGING <fs_alv>.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_rows
*&---------------------------------------------------------------------*
FORM f_get_rows CHANGING ct_rows TYPE lvc_t_row
                         cv_erro TYPE c.

  DATA lv_answer.

  IF sy-batch IS INITIAL.

    CALL METHOD go_cc_alv_01->get_selected_rows
      IMPORTING
        et_index_rows = ct_rows.

  ELSE.

    LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

      DATA(lv_tabix) = sy-tabix.

      APPEND INITIAL LINE TO ct_rows ASSIGNING FIELD-SYMBOL(<fs_row>).

      <fs_row>-index = lv_tabix.

    ENDLOOP.

  ENDIF.

  IF ct_rows IS INITIAL.
    cv_erro = abap_true.
  ELSE.

    PERFORM f_popup_to_confirm USING TEXT-t01 CHANGING lv_answer.

    IF lv_answer <> '1'.
      cv_erro = abap_true.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_correcao_insumos
*&---------------------------------------------------------------------*
FORM f_correcao_insumos CHANGING cs_dados_alv TYPE zsde0171_saida.

  DATA lv_diferenca TYPE zdmbtr.

  CHECK cs_dados_alv-area_proc EQ 'IN'.

  CHECK cs_dados_alv-tpsim <> 'BN'.

  CHECK cs_dados_alv-tpsim <> 'PM'.

  CHECK cs_dados_alv-diferenca_brl <> 0.

  sy-cprog = 'ZSDR016'.

  DATA(lo_obj)  = NEW zcl_taxa_curva_db( ).

  "BREAK-POINT.

  " Verifica se esta aprovado
  IF cs_dados_alv-status EQ 'A'.

    " Disparo inicial?
    IF cs_dados_alv-vl_t0094_brl IS INITIAL.

      CALL METHOD zcl_webservice_tx_curva=>hedge_insumos
        EXPORTING
          i_numero = cs_dados_alv-doc_simulacao
          i_tipo   = cs_dados_alv-tp_diparo.

    ELSE.

      lv_diferenca = cs_dados_alv-diferenca_brl.

      " inverte o sinal
      IF lv_diferenca > 0.

        lv_diferenca = lv_diferenca * -1.

      ENDIF.

      CALL METHOD lo_obj->diferenca
        EXPORTING
          i_numero    = cs_dados_alv-doc_simulacao
          i_fixacao   = 0
          i_diferenca = lv_diferenca
          i_tcode     = 'ZSDT0062'
          i_tipo      = cs_dados_alv-tp_diparo.

    ENDIF.

    " não esta aprovado
  ELSE.

    " se nao esta aprovado e tem valor, então tem que estornar

    CALL METHOD zcl_webservice_tx_curva=>hedge_insumos
      EXPORTING
        i_numero = cs_dados_alv-doc_simulacao
        i_tipo   = 'EST'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_trava_cambio_nova
*&---------------------------------------------------------------------*
FORM f_trava_cambio_nova USING uv_doc_sim TYPE zsded003
                      CHANGING cv_vlr_sim  TYPE zdmbtr.

  CLEAR cv_vlr_sim.

  LOOP AT gt_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>)
      WHERE doc_simulacao = uv_doc_sim
        AND prev_pgto_usd IS NOT INITIAL.

    cv_vlr_sim = cv_vlr_sim + <fs_0090>-prev_pgto_usd.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_trava_cambio_nova
*&---------------------------------------------------------------------*
FORM f_trava_cambio_antiga USING uv_doc_sim TYPE zsded003
                      CHANGING cv_vlr_sim  TYPE zdmbtr.

*  LOOP AT gt_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>)
*      WHERE doc_simulacao = uv_doc_sim
*        AND categoria = 'C' AND prev_pgto_usd IS INITIAL.
*
*    LOOP AT gt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
*        WHERE doc_sim = uv_doc_sim
*          AND vbeln = <fs_0090>-vbelv.
*
*      LOOP AT gt_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>)
*        WHERE vbeln = <fs_vbak>-vbeln.
*
*        CHECK <fs_vbap>-lifsp <> '12'.
*
*        IF <fs_vbak>-vbtyp = 'C'. "<-- se for ov soma
*
*          ADD <fs_vbap>-brgew TO lv_brgew.
*          ADD <fs_vbap>-netwr TO lv_netwr.
*          ADD <fs_vbap>-mwsbp TO lv_mwsbp.
*
*          " se for devolução deduz
*        ELSE.
*
*          SUBTRACT <fs_vbap>-brgew FROM lv_brgew.
*          SUBTRACT <fs_vbap>-netwr FROM lv_netwr.
*          SUBTRACT <fs_vbap>-mwsbp FROM lv_mwsbp.
*
*        ENDIF.
*
*      ENDLOOP.
*
*    ENDLOOP.
*
*  ENDLOOP.
*
*  cv_vlr_sim = lv_netwr + lv_mwsbp.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDA_EXIBICAO
*&---------------------------------------------------------------------*
FORM f_valida_exibicao CHANGING cs_alv TYPE zsde0171_saida.


  " condições de pagamento PM e BN não dispara Hedge
  IF cs_alv-tpsim = 'PM' OR cs_alv-tpsim = 'BN'.

    cs_alv-desconsiderar = abap_true.
    EXIT.

  ENDIF.

  " Se não tiver status, não exibe
  IF cs_alv-status IS INITIAL.

    cs_alv-desconsiderar = abap_true.
    EXIT.

  ENDIF.

  " desconsiderar se status <> 'A' e sem disparo
  IF cs_alv-status <> 'A' AND cs_alv-vl_t0094_brl IS INITIAL.
    cs_alv-desconsiderar = abap_true.
    EXIT.
  ENDIF.

  " se não tem diferença, não exibe
  IF cs_alv-diferenca = 0.

    cs_alv-desconsiderar = abap_true.
    EXIT.

  ENDIF.

  " verifica se é dolar e se não tem trava de cambio
  IF cs_alv-waerk = 'USD' AND NOT ( line_exists( gt_0090[ doc_simulacao = cs_alv-doc_simulacao categoria = 'C' ] ) ).

    cs_alv-desconsiderar = abap_true.
    EXIT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_tela
*&---------------------------------------------------------------------*
FORM f_processa_tela .

*  LOOP AT SCREEN.
*
*    IF screen-name CS 'RB_TODOS'.
*
*      screen-invisible = 1.
*
*      MODIFY SCREEN.
*
*    ENDIF.
*
*    CHECK sy-sysid = 'PRD'.
*
*    IF screen-name CS 'P_VIST'.
*
*      screen-invisible = 1.
*
*      MODIFY SCREEN.
*
*    ENDIF.
*
*
*
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_call_zsdt0090
*&---------------------------------------------------------------------*
FORM f_call_zsdt0090 USING us_line TYPE zsde0171_saida.

  DATA lv_flg_venda TYPE c.
  DATA lv_flg_frete TYPE c.

  IF us_line-tp_diparo = 'FRI'.
    lv_flg_frete = abap_true.
  ELSE.
    lv_flg_venda = abap_true.
  ENDIF.

  SUBMIT zsdr0046
    WITH p_nr_sol EQ us_line-doc_simulacao
    WITH p_vkorg EQ us_line-vkorg

    WITH r_mi EQ abap_false
    WITH r_in EQ abap_true
    WITH r_aq EQ abap_false
    WITH r_tp EQ abap_false

    WITH r_venda EQ lv_flg_venda
    WITH r_frete EQ lv_flg_frete AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_trv_antiga
*&---------------------------------------------------------------------*
FORM f_check_trv_antiga USING uv_doc_sim TYPE zsded003
                     CHANGING cv_trv_ant TYPE c.

  CLEAR cv_trv_ant.

  LOOP AT gt_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>)
        WHERE doc_simulacao = uv_doc_sim
          AND categoria = 'C' AND prev_pgto_usd IS INITIAL.

    cv_trv_ant = abap_true.
    EXIT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_tem_trv_antiga
*&---------------------------------------------------------------------*
FORM f_tem_trv_antiga USING us_vbak TYPE zsde0171_vbak
                   CHANGING cs_sim TYPE c.

  CLEAR cs_sim.

  " Verifica se tem trava na OV
  READ TABLE gt_0090 TRANSPORTING NO FIELDS
    WITH KEY vbelv = us_vbak-vbeln
             categoria = 'C'
             prev_vl_liq_usd = '0.0000'.

  IF sy-subrc EQ 0.

    cs_sim = abap_true.

  ELSE.

    READ TABLE gt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
      WITH KEY vbeln = us_vbak-vbeln_o.

    IF sy-subrc EQ 0.

      CHECK us_vbak-vbeln NE us_vbak-vbeln_o.

      PERFORM f_tem_trv_antiga
        USING <fs_vbak>
     CHANGING cs_sim.


    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_marcar_linha
*&---------------------------------------------------------------------*
FORM f_marcar_linha USING uv_ucomm TYPE syucomm.

  DATA wa_stable      TYPE lvc_s_stbl VALUE 'XX'.

  go_cc_alv_01->check_changed_data( ).

  CALL METHOD go_cc_alv_01->get_selected_rows
    IMPORTING
      et_row_no = DATA(lt_rows).

  LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).

    READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_saida>)
      INDEX <fs_rows>-row_id.

    CHECK sy-subrc EQ 0.

    IF <fs_saida>-sem_email IS INITIAL.
      <fs_saida>-sem_email = abap_true.
    ELSE.
      <fs_saida>-sem_email = abap_false.
    ENDIF.

  ENDLOOP.

  CALL METHOD go_cc_alv_01->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_data_changed
*&---------------------------------------------------------------------*
FORM f_data_changed USING uo_obj TYPE REF TO cl_alv_changed_data_protocol.

  DATA lv_field TYPE c LENGTH 40.

  LOOP AT uo_obj->mt_mod_cells ASSIGNING FIELD-SYMBOL(<fs_mod>).

    READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <fs_mod>-row_id.

    CHECK sy-subrc EQ 0.

    lv_field = '<FS_SAIDA>-' && <fs_mod>-fieldname.

    ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_field>).

    CHECK sy-subrc EQ 0.

    <fs_field> = <fs_mod>-value.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_toolbar
*&---------------------------------------------------------------------*
FORM f_toolbar USING uo_object TYPE REF TO cl_alv_event_toolbar_set.


  APPEND INITIAL LINE TO uo_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_toolbar>).

  <fs_toolbar>-text = 'Marcar Linhas'.
  <fs_toolbar>-icon = '@4B@'.
  <fs_toolbar>-function = 'SELECT'.
  <fs_toolbar>-quickinfo = 'Marcar/Desmarcar linhas'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_gravar_dados
*&---------------------------------------------------------------------*
FORM f_gravar_dados .

  DATA lt_zsdt0373 TYPE TABLE OF zsdt0373.

  go_cc_alv_01->check_changed_data( ).

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_saida>).

    APPEND INITIAL LINE TO lt_zsdt0373 ASSIGNING FIELD-SYMBOL(<fs_0373>).

    <fs_0373>-nro_sol_ov = <fs_saida>-doc_simulacao.
    <fs_0373>-fixacao = <fs_saida>-fixacao.
    <fs_0373>-tp_venda = <fs_saida>-tpsim.

    <fs_0373>-sem_email = <fs_saida>-sem_email.

  ENDLOOP.

  CHECK lt_zsdt0373 IS NOT INITIAL.

  MODIFY zsdt0373 FROM TABLE lt_zsdt0373.

  COMMIT WORK AND WAIT.

ENDFORM.
