* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZSDR0157                                               *
* Title.......: Envio de e-mail da trava de cambio                     *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 14/06/2023                                             *
* -------------------------------------------------------------------- *
REPORT zsdr0157.

TYPE-POOLS: slis, abap, icon.

TYPES: BEGIN OF ty_email,
         email TYPE ad_smtpadr.
TYPES END OF ty_email.

TYPES ty_email_tab TYPE TABLE OF ty_email.

TABLES: zsds0090_email, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZSDS0090_EMAIL'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

"dados alv
DATA gt_dados_alv TYPE STANDARD TABLE OF zsds0090_email.
DATA gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.
"SELECTION-SCREEN: FUNCTION KEY 1

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS so_docsi FOR zsds0090_email-doc_simulacao NO INTERVALS NO-EXTENSION.
SELECT-OPTIONS so_seq FOR zsds0090_email-sequencia NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS p_exec AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

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

  IF p_exec = abap_true.
    PERFORM f_email.
  ELSE.
    PERFORM f_exibe_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  SELECT zsdt0090~doc_simulacao,sequencia,vbeln,zsdt0040~vkorg,
         zsdt0040~vkbur, zsdt0040~vtweg, zsdt0040~spart,zsdt0090~data_atual,
         zsdt0090~usnam, zsdt0040~kunnr, vbelv,categoria, kurrf, valdt,
         data_prevpgto, data_prevpgtov, prev_pgto_usd, prev_pgto_brl,
         prev_multa_usd, prev_juros_usd, prev_vl_liq_usd
    FROM zsdt0090
    INNER JOIN zsdt0040 ON zsdt0090~doc_simulacao = zsdt0040~doc_simulacao
    INTO TABLE @DATA(lt_0090)
      WHERE zsdt0090~doc_simulacao IN @so_docsi
        AND sequencia IN @so_seq
        AND categoria = 'C'
        AND estorno = @space.

  LOOP AT lt_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>).

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    MOVE-CORRESPONDING <fs_0090> TO <fs_alv>.

    <fs_alv>-icon = '@08@'.
    <fs_alv>-msgtx = 'Pronto para execução'.

  ENDLOOP.

ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
            rs_selfield TYPE slis_selfield.                 "#EC CALLED

  CASE r_ucomm.
    WHEN '&IC1'.
      PERFORM f_hyperlink   USING rs_selfield.
    WHEN 'EMAIL'.
      PERFORM f_email.
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

  DATA lw_layout TYPE slis_layout_alv.
  DATA lw_variant TYPE disvariant.

  IF gt_dados_alv IS NOT INITIAL.

*    IF p_vari IS NOT INITIAL.
*      lw_variant-report = sy-repid.
*      lw_variant-variant = p_vari.
*    ENDIF.

    PERFORM f_monta_fieldcat.

    lw_layout-zebra             = abap_true.
    lw_layout-colwidth_optimize = abap_true.
    lw_layout-box_fieldname = gc_select_field.
    "lw_layout-info_fieldname = 'COLOR'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'F_STATUS_SET'
        i_callback_user_command  = 'F_USER_COMMAND'
        is_layout                = lw_layout
        it_fieldcat              = gt_fieldcat
        i_save                   = 'A'
        is_variant               = lw_variant
      TABLES
        t_outtab                 = gt_dados_alv
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

    IF sy-subrc <> 0.
      PERFORM f_mensagem_sistema.
    ENDIF.

  ELSE.
    MESSAGE s213(v4) DISPLAY LIKE 'E'.
    EXIT.

  ENDIF.

ENDFORM.                    " F_EXIBE_ALV
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


  "LVC_FIELDCATALOG_MERGE
  " SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-cprog
      i_internal_tabname     = gc_internal_tab
      i_structure_name       = gc_struc_name
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = gc_icon_field.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext_ddic = 'Status'.
    <fs_fcat>-ddic_outputlen = 000010.
  ENDIF.

  DELETE gt_fieldcat WHERE fieldname = gc_select_field.
  "DELETE gt_fieldcat WHERE fieldname = 'COLOR'.

  PERFORM f_coluna_edita USING 'MSGTX' 'Status'.

  "PERFORM f_coluna_edita USING 'COUNT_SAP' 'Notas SAP'.


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

  <fs_cat>-seltext_s = p_text.
  <fs_cat>-seltext_m = p_text.
  <fs_cat>-seltext_l = p_text.
  <fs_cat>-reptext_ddic = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COLUNA_EXCLUI
*&---------------------------------------------------------------------*
FORM f_coluna_exclui USING p_fieldname TYPE slis_fieldname.

  DELETE gt_fieldcat WHERE fieldname = p_fieldname.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_VERIFICA_LINHA_SELEC
*&---------------------------------------------------------------------*
FORM f_verifica_linha_selec CHANGING p_error TYPE c.

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

    PERFORM f_popup_to_confirm USING text-t01 CHANGING lv_ret.

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
*&      Form  F_EMAIL
*&---------------------------------------------------------------------*
FORM f_email .

  DATA lr_selec TYPE RANGE OF flag.

  DATA lv_ret.

  DATA lt_email TYPE ty_email_tab.

  DATA lv_erro.

  IF p_exec IS INITIAL.

    PERFORM f_verifica_linha_selec CHANGING lv_ret.

    CHECK lv_ret IS INITIAL.

    PERFORM f_popup_to_confirm USING text-t01 CHANGING lv_ret.

    CHECK lv_ret = '1'.

    APPEND 'IEQX' TO lr_selec.

  ELSE.
    CLEAR lr_selec.

  ENDIF.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec IN lr_selec.

    CLEAR lv_erro.

    PERFORM f_get_emails
      USING <fs_dados>
   CHANGING lt_email.

    PERFORM f_enviar_email USING <fs_dados> lt_email CHANGING lv_erro.

    IF lv_erro IS INITIAL.
      <fs_dados>-icon = '@DF@'.
      <fs_dados>-msgtx = 'Enviado'.
    ELSE.
      <fs_dados>-icon = '@0A@'.
      <fs_dados>-msgtx = 'Não enviado'.
    ENDIF.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_EMAILS
*&---------------------------------------------------------------------*
FORM f_get_emails USING us_0090 TYPE zsds0090_email
               CHANGING ct_email TYPE ty_email_tab.

  DATA lr_vkbur TYPE RANGE OF vkbur.
  DATA lr_vkgrp TYPE RANGE OF vkgrp.
  DATA lr_vkorg TYPE RANGE OF vkorg.
  DATA lr_vtweg TYPE RANGE OF vtweg.
  DATA lr_spart TYPE RANGE OF spart.

  CLEAR ct_email[].

  APPEND 'IEQ' TO lr_vkbur.
  APPEND 'IEQ' TO lr_vkgrp.
  APPEND 'IEQ' TO lr_vkorg.
  APPEND 'IEQ' TO lr_vtweg.
  APPEND 'IEQ' TO lr_spart.

  APPEND 'IEQ' && us_0090-vkbur TO lr_vkbur.
  APPEND 'IEQ' && us_0090-vkorg TO lr_vkorg.
  APPEND 'IEQ' && us_0090-vtweg TO lr_vtweg.
  APPEND 'IEQ' && us_0090-spart TO lr_spart.

  SELECT * FROM zsdt0060
    INTO TABLE @DATA(lt_0060)
      WHERE programa EQ 'ZSDR016'
        AND vkbur IN @lr_vkbur
        "AND vkgrp IN @lr_vkgrp
        AND vkorg IN @lr_vkorg
        AND vtweg IN @lr_vtweg.
  "AND spart IN @lr_spart.

  DELETE lt_0060 WHERE email IS INITIAL.

  LOOP AT lt_0060 ASSIGNING FIELD-SYMBOL(<fs_0060>).

    APPEND INITIAL LINE TO ct_email ASSIGNING FIELD-SYMBOL(<fs_email>).

    <fs_email>-email = <fs_0060>-email.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ENVIAR_EMAIL
*&---------------------------------------------------------------------*
FORM f_enviar_email USING us_0090 TYPE zsds0090_email
                          ut_email TYPE ty_email_tab
                 CHANGING cv_erro TYPE c.

  DATA lt_contents TYPE html_table.
  DATA lt_receivers TYPE somlreci1_t.
  DATA ls_doc_dat TYPE sodocchgi1.
  DATA lt_list TYPE sopcklsti1_t.

  LOOP AT ut_email ASSIGNING FIELD-SYMBOL(<fs_stvar>).

    APPEND INITIAL LINE TO lt_receivers ASSIGNING FIELD-SYMBOL(<fs_rec>).

    <fs_rec>-receiver = <fs_stvar>-email.
    <fs_rec>-rec_type = 'U'.

  ENDLOOP.

  IF sy-uname = 'RBLIMA'. " #debug #apagar

    "BREAK-POINT.
    CLEAR lt_receivers.

    APPEND INITIAL LINE TO lt_receivers ASSIGNING <fs_rec>.

    <fs_rec>-receiver = 'ramon.lima@reclike.com.br'.
    <fs_rec>-rec_type = 'U'.

  ENDIF.

  IF sy-uname = 'SCABANA'. " #debug #apagar

    CLEAR lt_receivers.

    APPEND INITIAL LINE TO lt_receivers ASSIGNING <fs_rec>.

    <fs_rec>-receiver = 'samuel.cabana@amaggi.com.br'.
    <fs_rec>-rec_type = 'U'.

  ENDIF.

  SORT lt_receivers.

  DELETE ADJACENT DUPLICATES FROM lt_receivers.

  CHECK lt_receivers IS NOT INITIAL.

  APPEND INITIAL LINE TO lt_list ASSIGNING FIELD-SYMBOL(<fs_list>).

  <fs_list>-head_start = 1.
  <fs_list>-head_num = 0.
  <fs_list>-body_start = 1.
  <fs_list>-body_num = 99999.
  <fs_list>-doc_type = 'HTM'.

  ls_doc_dat-obj_name = 'Trava de cambio'.
  ls_doc_dat-obj_descr = 'Trava de cambio'.
  ls_doc_dat-no_change = 'X'.

  PERFORM f_get_html USING us_0090 CHANGING lt_contents.

  "Enviar
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = ls_doc_dat
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = lt_list
      contents_txt               = lt_contents
      receivers                  = lt_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      OTHERS                     = 99.

  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_HTML
*&---------------------------------------------------------------------*
FORM f_get_html USING us_0090 TYPE zsds0090_email
             CHANGING ct_contents TYPE html_table.

  DATA lv_html TYPE c LENGTH 3000.

  DATA lv_syst TYPE c LENGTH 10.
  DATA lv_data_reg TYPE c LENGTH 30.
  DATA lv_doc_sim TYPE c LENGTH 30.
  DATA lv_vbeln TYPE c LENGTH 30.
  DATA lv_data_ven TYPE c LENGTH 30.
  DATA lv_user TYPE c LENGTH 30.
  DATA lv_taxa TYPE c LENGTH 30.
  DATA lv_tipo_tax TYPE c LENGTH 30.
  DATA lv_vlr_usd TYPE c LENGTH 30.
  DATA lv_vlr_brl TYPE c LENGTH 30.
  DATA lv_liq_brl TYPE zsde_prev_vl_liq_usd.

  lv_liq_brl = us_0090-prev_vl_liq_usd * us_0090-kurrf.

  lv_html = '<div align="center"><span style="font-family:' &&
            'Verdana; font-size: large;">#SYSTEM#</span></div><div align="center">' &&
            '<span style="font-family: Verdana; font-size: medium;">TRAVA DE C&Acirc;MBIO' &&
            '</span></div><div align="center"><table width="1017"><tbody><tr style="background-color: #effceb;"><td width="113">' &&
            'Data Registro</td><td width="113">N&ordm; Simulador de Venda</td><td width="113">' &&
            'N&deg; Ordem de Venda</td><td width="113">Data de Venc.</td><td width="113">' &&
            'Usu&aacute;rio</td><td width="113">Taxa Cambio</td><td width="113">Tipo Taxa' &&
            '</td><td width="113">Valor USD</td><td width="113">Valor BRL</td></tr><tr style="background-color: #FFFFEF;">' &&
            '<td width="113">#01#</td><td width="113">#02#</td><td width="113">#03#</td>' &&
            '<td width="113">#04#</td><td width="113">#05#</td><td width="113">#06#</td>' &&
            '<td width="113">#07#</td><td width="113">#08#</td><td width="113">#09#</td>' &&
            '</tr></tbody></table></div>'.

  lv_syst = sy-host.

  TRANSLATE lv_syst TO UPPER CASE.

  WRITE us_0090-data_atual TO lv_data_reg LEFT-JUSTIFIED.
  WRITE us_0090-doc_simulacao TO lv_doc_sim NO-ZERO LEFT-JUSTIFIED.
  WRITE us_0090-vbelv TO lv_vbeln NO-ZERO LEFT-JUSTIFIED.
  WRITE us_0090-data_prevpgto TO lv_data_ven LEFT-JUSTIFIED.
  WRITE us_0090-usnam TO lv_user LEFT-JUSTIFIED.
  WRITE us_0090-kurrf TO lv_taxa LEFT-JUSTIFIED NO-GAP.
  WRITE us_0090-categoria TO lv_tipo_tax LEFT-JUSTIFIED.
  "WRITE us_0090-prev_pgto_usd TO lv_vlr_usd LEFT-JUSTIFIED DECIMALS 2.
  "WRITE us_0090-prev_pgto_brl TO lv_vlr_brl LEFT-JUSTIFIED DECIMALS 2.

  WRITE us_0090-prev_vl_liq_usd TO lv_vlr_usd LEFT-JUSTIFIED DECIMALS 2.
  WRITE lv_liq_brl TO lv_vlr_brl LEFT-JUSTIFIED DECIMALS 2.

  CONDENSE lv_taxa NO-GAPS.

  REPLACE '#SYSTEM#' IN lv_html WITH lv_syst.
  REPLACE '#01#' IN lv_html WITH lv_data_reg.
  REPLACE '#02#' IN lv_html WITH lv_doc_sim.
  REPLACE '#03#' IN lv_html WITH lv_vbeln.
  REPLACE '#04#' IN lv_html WITH lv_data_ven.
  REPLACE '#05#' IN lv_html WITH lv_user.
  REPLACE '#06#' IN lv_html WITH lv_taxa.
  REPLACE '#07#' IN lv_html WITH lv_tipo_tax.
  REPLACE '#08#' IN lv_html WITH lv_vlr_usd.
  REPLACE '#09#' IN lv_html WITH lv_vlr_brl.

  CALL FUNCTION 'RKD_WORD_WRAP'
    EXPORTING
      textline            = lv_html
      delimiter           = ' '
      outputlen           = 255
    TABLES
      out_lines           = ct_contents
    EXCEPTIONS
      outputlen_too_large = 1
      OTHERS              = 2.

ENDFORM.
