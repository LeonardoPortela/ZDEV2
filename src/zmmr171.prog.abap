* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZMMR171                                                *
* Title.......: Integração COUPA - Grupo de mercadorias                *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 14/02/2021                                             *
* -------------------------------------------------------------------- *
REPORT zmmr171.

TYPE-POOLS: slis, abap, icon.

TABLES: zmms_gr_merc_coupa_alv, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZMMS_GR_MERC_COUPA_ALV'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.
CONSTANTS gc_service TYPE /ui2/service_name VALUE 'COUPA_INT_ENVIA_GRP_MERCADORIA'.

DATA gt_dados_alv TYPE STANDARD TABLE OF zmms_gr_merc_coupa_alv.
DATA gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.
DATA gt_t023t TYPE TABLE OF t023t.
DATA gt_coupa01 TYPE TABLE OF zintegrcoupa01.
DATA gt_0039 TYPE TABLE OF zmmt0039.
DATA gv_erro TYPE c.

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS so_matkl FOR zmms_gr_merc_coupa_alv-matkl.
SELECT-OPTIONS so_wgbez FOR zmms_gr_merc_coupa_alv-wgbez.
SELECTION-SCREEN END OF BLOCK b1.

" DADOS DO ALV
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS p_envi AS CHECKBOX.
PARAMETERS p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  PERFORM default_variant CHANGING p_vari.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant CHANGING p_vari.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

START-OF-SELECTION.

  PERFORM f_verifica_job CHANGING gv_erro.

  CHECK gv_erro IS INITIAL.

  PERFORM f_seleciona.
  PERFORM f_processa_dados.

  IF sy-batch = 'X'.
    PERFORM f_enviar_coupa.
  ELSE.
    PERFORM f_exibe_alv.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  DATA lr_id_int TYPE RANGE OF zcoupa_id_integr.

  SELECT * FROM t023t
    INTO TABLE gt_t023t
      WHERE spras = sy-langu
        AND matkl IN so_matkl
        AND wgbez IN so_wgbez.

  DELETE gt_t023t WHERE matkl IS INITIAL.

  CHECK gt_t023t IS NOT INITIAL.

  SELECT * FROM zmmt0039
    INTO TABLE gt_0039
      FOR ALL ENTRIES IN gt_t023t
        WHERE matkl = gt_t023t-matkl.

  LOOP AT gt_t023t ASSIGNING FIELD-SYMBOL(<fs_t023t>).
    APPEND 'IEQ' && <fs_t023t>-matkl TO lr_id_int.
  ENDLOOP.

  SELECT * FROM zintegrcoupa01
    INTO TABLE gt_coupa01
        WHERE ident_proc = 'GM'
          AND id_integr IN lr_id_int.

  CHECK sy-subrc EQ 0 AND p_envi IS INITIAL.

  LOOP AT gt_coupa01 ASSIGNING FIELD-SYMBOL(<fs_coupa>).

    DELETE gt_t023t WHERE matkl = <fs_coupa>-id_integr.

  ENDLOOP.


ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
            rs_selfield TYPE slis_selfield.                 "#EC CALLED

  CASE r_ucomm.
    WHEN 'ENVIAR'.
      PERFORM f_enviar_coupa.
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

  DATA lw_layout TYPE slis_layout_alv.
  DATA lw_variant TYPE disvariant.

  IF gt_dados_alv IS NOT INITIAL.

    IF p_vari IS NOT INITIAL.
      lw_variant-report = sy-repid.
      lw_variant-variant = p_vari.
    ENDIF.

    PERFORM f_monta_fieldcat.

    lw_layout-zebra             = abap_true.
    lw_layout-colwidth_optimize = abap_true.
    lw_layout-box_fieldname = gc_select_field.

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
  DELETE gt_fieldcat WHERE fieldname = 'SPRAS'.
  DELETE gt_fieldcat WHERE fieldname = 'IDENT_PROC'.
  DELETE gt_fieldcat WHERE fieldname = 'DT_ATUAL'.
  DELETE gt_fieldcat WHERE fieldname = 'HR_ATUAL'.
  DELETE gt_fieldcat WHERE fieldname = 'STATUS'.
  "DELETE gt_fieldcat WHERE fieldname = 'SAKNR'.

  PERFORM f_coluna_edita USING 'MSGTX' 'Status'.
  PERFORM f_coluna_edita USING 'WGBEZ' 'Denominação Pai'.


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

  IF l_lines <= 1.

    LOOP AT p_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_ret2>).

      MESSAGE ID <fs_ret2>-id
            TYPE <fs_ret2>-type
          NUMBER <fs_ret2>-number
            WITH <fs_ret2>-message_v1
                 <fs_ret2>-message_v2
                 <fs_ret2>-message_v3
                 <fs_ret2>-message_v4.

    ENDLOOP.

  ELSE.

    CALL FUNCTION 'MESSAGES_INITIALIZE'.

    LOOP AT p_bapiret2_tab ASSIGNING <fs_ret2>.

      IF <fs_ret2>-id IS INITIAL OR <fs_ret2>-system <> sy-sysid.

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
*&      Form  F_ENVIAR_COUPA
*&---------------------------------------------------------------------*
FORM f_enviar_coupa .

  DATA lw_integr TYPE zintegrcoupa01.
  DATA lo_int TYPE REF TO zcl_integracao_coupa_grp_merca.

  DATA lr_selec TYPE RANGE OF flag.

  IF sy-batch IS INITIAL.
    APPEND 'IEQX' TO lr_selec.
  ENDIF.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE icon = icon_generate AND selec IN lr_selec.

    TRY.

        CLEAR lw_integr.

        lo_int ?= zcl_integracao_coupa_grp_merca=>zif_integracao_coupa_grp_merca~get_instance( gc_service ).

        CHECK lo_int IS BOUND.

        lw_integr = lo_int->zif_integracao_coupa_grp_merca~enviar_coupa( <fs_dados> ).

        MOVE-CORRESPONDING lw_integr TO <fs_dados>.

        CASE lw_integr-status.
          WHEN 'S'.
            <fs_dados>-icon = icon_complete.
            <fs_dados>-msgtx = 'Integrado com sucesso'.
          WHEN 'D'.
            <fs_dados>-icon = icon_complete.
            <fs_dados>-msgtx = 'Já existente no COUPA'.
          WHEN OTHERS.
        ENDCASE.

      CATCH zcx_integracao INTO DATA(ex_int).

        DATA(lv_text) = ex_int->get_longtext( ).

        PERFORM f_mensagem_insere_txt USING 'E' lv_text.

      CATCH zcx_error INTO DATA(ex_erro).

        lv_text = ex_erro->get_longtext( ).

        PERFORM f_mensagem_insere_txt USING 'E' lv_text.

    ENDTRY.

  ENDLOOP.

  IF line_exists( gt_bapiret2[ type = 'E' ] ).
    PERFORM f_mensagem_exibe_popup USING gt_bapiret2.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& FORM F_REMOVE_CHAR_SPECIAL
*&---------------------------------------------------------------------*
FORM f_remove_char_special CHANGING p_text TYPE c.

  DATA lv_text2 TYPE char100.

  lv_text2 = p_text.

  CALL FUNCTION 'ES_REMOVE_SPECIAL_CHARACTER'
    EXPORTING
      text1       = lv_text2
    IMPORTING
      corr_string = lv_text2.

  p_text = lv_text2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
FORM f_processa_dados .

  LOOP AT gt_t023t ASSIGNING FIELD-SYMBOL(<fs_t023t>).

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>).

    MOVE-CORRESPONDING <fs_t023t> TO <fs_dados>.

    READ TABLE gt_0039 ASSIGNING FIELD-SYMBOL(<fs_0039>)
      WITH KEY matkl = <fs_t023t>-matkl.

    IF sy-subrc EQ 0.
      <fs_dados>-saknr = <fs_0039>-saknr.
    ENDIF.

    <fs_dados>-wgbez60 = <fs_t023t>-wgbez60 && ` (` && <fs_t023t>-matkl && ')'.

    READ TABLE gt_coupa01 ASSIGNING FIELD-SYMBOL(<fs_coupa>)
      WITH KEY id_integr = <fs_t023t>-matkl.

    " JA EXISTE NO COUP
    IF sy-subrc EQ 0.

      CASE <fs_coupa>-status.
        WHEN 'S'.
          <fs_dados>-icon = icon_complete.
          <fs_dados>-msgtx = 'Envio já realizado'.
        WHEN 'D'.
          <fs_dados>-icon = icon_complete.
          <fs_dados>-msgtx = 'Já existente no COUPA'.
        WHEN OTHERS.
      ENDCASE.

      MOVE-CORRESPONDING <fs_coupa> TO <fs_dados>.

    ELSE.

      <fs_dados>-icon  = icon_generate.
      <fs_dados>-msgtx = 'Aguardando envio'.

    ENDIF.

  ENDLOOP.

  "DELETE gt_dados_alv WHERE saknr IS INITIAL.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ICON
*&---------------------------------------------------------------------*
FORM f_atualiza_icon USING p_status TYPE zemm_status_coupa
                  CHANGING p_coupa_icon TYPE icon_d.

*  CASE p_status.
*    WHEN gc_aguardando.
*      p_coupa_icon = icon_generate.
*    WHEN gc_enviado.
*      p_coupa_icon = icon_complete.
*    WHEN gc_nao_env.
*      p_coupa_icon = ''. "#DUVIDA - QUAL ICONE VAI NESSA SITUAÇÃO
*    WHEN gc_liberado.
*      p_coupa_icon = icon_page_right.
*  ENDCASE.

ENDFORM.
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
*&      Form  F_ATUALIZA_TABELA
*&---------------------------------------------------------------------*
FORM f_atualiza_tabela USING p_id_int TYPE zde_id_integracao
                             p_integ  TYPE zintegracao
                    CHANGING p_dados TYPE zmms_gr_merc_coupa_alv.

  DATA lw_integr TYPE zintegrcoupa01.
  DATA lo_xml_ret TYPE REF TO cl_xml_document.

  CREATE OBJECT lo_xml_ret.

  CHECK lo_xml_ret->parse_string( p_integ-ds_data_retorno ) = 0.

  DATA(lv_node) = lo_xml_ret->find_simple_element( 'id' ).

  IF lv_node IS NOT INITIAL.

    lw_integr-id_integr = p_dados-matkl.
    lw_integr-ident_proc = 'GM'.
    lw_integr-dt_atual = sy-datum.
    lw_integr-hr_atual = sy-uzeit.
    lw_integr-status = 'S'.

    MOVE-CORRESPONDING lw_integr TO p_dados.

    p_dados-icon = icon_complete.
    p_dados-msgtx = 'Integrado com sucesso'.

  ELSEIF lo_xml_ret->parse_string( p_integ-ds_data_retorno ) = 0.

    lv_node = lo_xml_ret->find_simple_element( 'error' ).

    SHIFT lv_node LEFT DELETING LEADING space.

    PERFORM f_mensagem_insere_txt USING 'E' lv_node.

  ENDIF.

  CHECK lw_integr IS NOT INITIAL.

  MODIFY zintegrcoupa01 FROM lw_integr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_JOB
*&---------------------------------------------------------------------*
FORM f_verifica_job CHANGING p_erro.

  CALL FUNCTION 'ZMM_JOB_CHECK'
    EXPORTING
      i_cprog   = sy-cprog
    IMPORTING
      ev_active = p_erro.

  IF p_erro = 'X'.
    MESSAGE s016(ds) WITH 'Já existe um job para o programa' sy-cprog DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
