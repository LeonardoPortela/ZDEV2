* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZINR0001                                               *
* Title.......: Integrações - Assinatura Digital COUPA X BRy           *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 17/02/2022                                             *
* -------------------------------------------------------------------- *
REPORT zinr0001.

TYPE-POOLS: slis, abap, icon.

TABLES: zins_dados_coleta, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZINS_DADOS_COLETA'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

"dados alv
DATA gt_dd07v TYPE TABLE OF dd07v.
DATA gt_assina TYPE TABLE OF zins_dados_coleta.
DATA gt_dados_alv TYPE STANDARD TABLE OF zins_dados_coleta.
DATA gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.
DATA gt_etapas TYPE TABLE OF dd07v.
DATA gv_erro TYPE c.
DATA gr_id TYPE RANGE OF zin_nome_contrato.
DATA gv_params TYPE string.
"SELECTION-SCREEN: FUNCTION KEY 1

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS so_ref  FOR zins_dados_coleta-id_referencia.
  SELECT-OPTIONS so_id   FOR zins_dados_coleta-nome NO INTERVALS.
  SELECT-OPTIONS so_proc FOR zins_dados_coleta-id_processo.
  SELECT-OPTIONS so_etap FOR zins_dados_coleta-etapa.
  SELECT-OPTIONS so_chav FOR zins_dados_coleta-chave_coleta.
  SELECT-OPTIONS so_data FOR zins_dados_coleta-log_date.
  PARAMETERS p_dias TYPE int4 DEFAULT 60.
SELECTION-SCREEN END OF BLOCK b1.

" DADOS SECUNDARIOS
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS p_bus1 AS CHECKBOX DEFAULT 'X'.
  "PARAMETERS p_bus2 AS CHECKBOX DEFAULT 'X'.
  "PARAMETERS p_bus22 AS CHECKBOX DEFAULT space.
  PARAMETERS p_bus3 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

" DADOS DO ALV
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002.
  PARAMETERS p_vari TYPE slis_vari NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  PERFORM default_variant CHANGING p_vari.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

START-OF-SELECTION.

  PERFORM f_verifica_job CHANGING gv_erro.

  CHECK gv_erro IS INITIAL.

  PERFORM f_set_object_tab.

  IF p_bus1 IS NOT INITIAL.

    "IF 1 = 3.
    PERFORM f_executa_busca_inicial.
    "ELSE.
    "PERFORM f_executa_busca_inicial.
    "ENDIF.

  ENDIF.

*  IF p_bus2 IS NOT INITIAL.
*    PERFORM f_executa_set_finalizados.
*  ENDIF.

  IF p_bus3 IS NOT INITIAL.

    "IF 1 = 3.
    PERFORM f_executa_busca_docs_assinado.
    "ENDIF.
    "PERFORM f_executa_busca_docs_assinados.
  ENDIF.

  PERFORM f_seleciona.

  PERFORM f_processa_selec.

  IF sy-batch IS INITIAL.
    PERFORM f_exibe_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  PERFORM f_preenche_id_interno.

  SELECT * FROM zint_assina01
    INTO CORRESPONDING FIELDS OF TABLE gt_assina
    WHERE id_referencia IN so_ref
      AND nome          IN gr_id
      AND id_processo   IN so_proc
      AND etapa         IN so_etap
      AND chave_coleta  IN so_chav
      AND log_date      IN so_data.


  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZIN_ETAPA'
    TABLES
      values_tab      = gt_etapas
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
            rs_selfield TYPE slis_selfield.                 "#EC CALLED

  CASE r_ucomm.
    WHEN 'CONF_ASSIN'.
      PERFORM f_confirma_bry.
    WHEN 'CANC_ASSIN'.
      PERFORM f_cancela_bry.
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

  SET PF-STATUS 'STANDARD2'.

ENDFORM.                    "F_PFSTATUS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat.


  "LVC_FIELDCATALOG_MERGE
  SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.

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

  READ TABLE gt_fieldcat ASSIGNING <fs_fcat>
    WITH KEY fieldname = 'ETAPA_TXT'.

  IF sy-subrc EQ 0.
    <fs_fcat>-col_pos = 7.
  ENDIF.

  PERFORM f_coluna_edita USING 'ETAPA_TXT' 'Descrição Etapa'.

  PERFORM f_coluna_edita USING 'ETAPA' 'Etapa Realizada'.
  PERFORM f_coluna_edita USING 'MSGTX' 'Status atual'.
  PERFORM f_coluna_edita USING 'DT_ATUAL' 'Dt.Atual'.
  PERFORM f_coluna_edita USING 'HR_ATUAL' 'Hr.Atual'.
  PERFORM f_coluna_edita USING 'CREATED_D' 'Dt.Criação'.
  PERFORM f_coluna_edita USING 'CREATED_T' 'Hr.Criação'.
  PERFORM f_coluna_edita USING 'UPDATED_D' 'Dt.Atualização'.
  PERFORM f_coluna_edita USING 'UPDATED_T' 'Hr.Atualização'.
  PERFORM f_coluna_edita USING 'NAME' 'Nome'.
  PERFORM f_coluna_edita USING 'NUMBER_CONTRACT' 'Nr.Contrato'.
  PERFORM f_coluna_edita USING 'VERSION' 'Versão'.
  PERFORM f_coluna_edita USING 'STATUS' 'Status'.
  PERFORM f_coluna_edita USING 'TYPE' 'Tipo'.
  PERFORM f_coluna_edita USING 'START_DATE' 'Dt.Inicio'.
  PERFORM f_coluna_edita USING 'START_TIME' 'Hr.Inicio'.
  PERFORM f_coluna_edita USING 'CONTRATO_SISTMICO' 'Contrato Sistemico'.
  PERFORM f_coluna_edita USING 'TAXA_FIXA' 'Taxa Fixa'.
  PERFORM f_coluna_edita USING 'EXIGIR_DOWNLOAD' 'Exigir Download'.
  PERFORM f_coluna_edita USING 'PROIBIR_REJEIO' 'Proibir Rejeio'.
  PERFORM f_coluna_edita USING 'ASSINATURA_SEQUENCIAL' 'Assina.Sequencial'.
  PERFORM f_coluna_edita USING 'AGRUPAR_DOCUMENTOS' 'AgrupaDocs'.
  PERFORM f_coluna_edita USING 'RELATRIO_ASSINATURAS' 'RelatorioAssinatura'.
  PERFORM f_coluna_edita USING 'PROTOCOLO_ASSINATURAS' 'ProtocolAssinaturas'.

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

  CHECK p_bapiret2_tab IS NOT INITIAL.

  DESCRIBE TABLE p_bapiret2_tab LINES l_lines.

  CALL FUNCTION 'MESSAGES_INITIALIZE'.

  LOOP AT p_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_ret2>).

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

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  CALL FUNCTION 'MESSAGES_STOP'
    EXCEPTIONS
      a_message = 1
      e_message = 2
      i_message = 3
      w_message = 4
      OTHERS    = 5.     "#EC CI_SUBRC

  IF sy-subrc <> 0.
    "EXIT.
  ENDIF.

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
      OTHERS              = 3.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

ENDFORM.
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

  IF <fs_cat>-datatype = 'CHAR' AND <fs_cat>-intlen = '000001'.
    <fs_cat>-checkbox = 'X'.
  ENDIF.

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
*&      Form  F_EXECUTA_BUSCA_INICIAL
*&---------------------------------------------------------------------*
FORM f_executa_busca_inicial2 .

  DATA lo_object TYPE REF TO zif_integracao_adigital_get.

  CLEAR gt_bapiret2.

  LOOP AT gt_dd07v ASSIGNING FIELD-SYMBOL(<fs_dd07v>).

    DATA(lv_class) = 'ZCL_INTEGRACAO_' && <fs_dd07v>-ddtext && '_AD_GET'.

    TRY.

        CREATE OBJECT lo_object TYPE (lv_class).

        lo_object->process_contracts( so_ref[] ).

        DATA(lt_mess) = lo_object->get_messages( ).

        IF lt_mess IS NOT INITIAL.
          APPEND LINES OF lt_mess TO gt_bapiret2.
        ENDIF.

      CATCH zcx_error.

        CONTINUE.

      CATCH cx_sy_create_object_error.

        CONTINUE.

    ENDTRY.

  ENDLOOP.

  PERFORM f_mensagem_exibe_popup USING gt_bapiret2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTA_BUSCA_INICIAL
*&---------------------------------------------------------------------*
FORM f_executa_busca_inicial .

  DATA lo_object TYPE REF TO zcl_default_documents_step_01.

  CLEAR gt_bapiret2.

  LOOP AT gt_dd07v ASSIGNING FIELD-SYMBOL(<fs_dd07v>).

    DATA(lv_class) = 'ZCL_' && <fs_dd07v>-ddtext && '_DOCUMENTS_STEP_01'.

    TRY.

        CREATE OBJECT lo_object TYPE (lv_class).

        CLEAR gv_params.

        IF p_dias > 0.
          gv_params = p_dias.
        ENDIF.

        lo_object->process_contracts( EXPORTING ir_id_ref_range = so_ref[] iv_params = gv_params ).

        DATA(lt_mess) = lo_object->get_messages( ).

        IF lt_mess IS NOT INITIAL.
          APPEND LINES OF lt_mess TO gt_bapiret2.
        ENDIF.

      CATCH zcx_error INTO DATA(WCX_ERRO).

        CONTINUE.

      CATCH cx_sy_create_object_error.

        CONTINUE.

    ENDTRY.

  ENDLOOP.

  PERFORM f_mensagem_exibe_popup USING gt_bapiret2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_SELEC
*&---------------------------------------------------------------------*
FORM f_processa_selec.

  LOOP AT gt_assina ASSIGNING FIELD-SYMBOL(<fs_assina>).

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    MOVE-CORRESPONDING <fs_assina> TO <fs_alv>.

    CASE <fs_alv>-etapa.
      WHEN '01'.
        <fs_alv>-icon = icon_green_light.
        <fs_alv>-msgtx = 'Aguardando coleta de assinaturas...'.
      WHEN '02'.
        <fs_alv>-icon = icon_yellow_light.
        <fs_alv>-msgtx = 'Coleta realizada, buscando documentos assinados...'.
      WHEN '03'.
        <fs_alv>-icon = icon_checked.
        <fs_alv>-msgtx = 'Assinaturas recolhidas corretamente'.
      WHEN '98'.
        <fs_alv>-icon = icon_light_out.
        <fs_alv>-msgtx = 'Coleta cancelada, atualizando origem...'.
      WHEN '99'.
        <fs_alv>-icon = icon_red_light.
        <fs_alv>-msgtx = 'Coleta cancelada no destino'.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_GUI_INFO
*&---------------------------------------------------------------------*
FORM f_sap_gui_info USING p_etapa TYPE zin_etapa
                         p_prefix TYPE ddtext.

  DATA lv_msgx TYPE msgtx.

  IF p_etapa IS INITIAL.

    lv_msgx = 'Coletando docs'.

  ELSE.

    CASE p_etapa.
      WHEN '01'.
        lv_msgx = 'Solicitando anexos'.
      WHEN '02'.
        lv_msgx = 'Baixando anexos'.
      WHEN '03'.
        lv_msgx = 'Enviando para aprovação'.
    ENDCASE.

  ENDIF.

  lv_msgx = lv_msgx && ` no ` && p_prefix && `...`.

  PERFORM f_sap_indicator USING lv_msgx 50 .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTA_BUSCA_DOCS_PENDENTES
*&---------------------------------------------------------------------*
FORM f_executa_busca_docs_pendentes .

  DATA lo_object TYPE REF TO zcl_integracao_coupa_adigital.

  TRY.

      CREATE OBJECT lo_object.

      lo_object->zif_integracao_assinatura_digi~get_documents(
        )->send_document_to_approval( ).

    CATCH zcx_integracao INTO DATA(ex_int).

      DATA(lv_text) = ex_int->get_longtext( ).

      PERFORM f_mensagem_insere_txt USING 'E' lv_text.

    CATCH zcx_error INTO DATA(ex_erro).

      lv_text = ex_erro->get_longtext( ).

      PERFORM f_mensagem_insere_txt USING 'E' lv_text.

  ENDTRY.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTA_BUSCA_DOCS_ASSINADOS
*&---------------------------------------------------------------------*
FORM f_executa_busca_docs_assinados.

  DATA lo_object TYPE REF TO zif_integracao_adigital_post.

  LOOP AT gt_dd07v ASSIGNING FIELD-SYMBOL(<fs_dd07v>).

    DATA(lv_class) = 'ZCL_INTEGRACAO_' && <fs_dd07v>-ddtext && '_AD_POST'.

    CLEAR gt_bapiret2.

    TRY.

        CREATE OBJECT lo_object TYPE (lv_class).

        lo_object->process_contracts( so_ref[] ).

        APPEND LINES OF lo_object->get_messages( ) TO gt_bapiret2.

      CATCH zcx_error.
        CONTINUE.
      CATCH cx_sy_create_object_error.
        CONTINUE.
    ENDTRY.

    PERFORM f_mensagem_exibe_popup USING gt_bapiret2.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTA_BUSCA_DOCS_ASSINADOS
*&---------------------------------------------------------------------*
FORM f_executa_busca_docs_assinado.

  DATA lo_object TYPE REF TO zcl_default_documents_step_02.

  LOOP AT gt_dd07v ASSIGNING FIELD-SYMBOL(<fs_dd07v>).

    DATA(lv_class) = 'ZCL_' && <fs_dd07v>-ddtext && '_DOCUMENTS_STEP_02'.

    CLEAR gt_bapiret2.

    TRY.

        CREATE OBJECT lo_object TYPE (lv_class).

        lo_object->process_contracts( so_ref[] ).

        APPEND LINES OF lo_object->get_messages( ) TO gt_bapiret2.

      CATCH zcx_error.
        CONTINUE.
      CATCH cx_sy_create_object_error.
        CONTINUE.
    ENDTRY.

    PERFORM f_mensagem_exibe_popup USING gt_bapiret2.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SET_OBJECT_TAB
*&---------------------------------------------------------------------*
FORM f_set_object_tab .

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZIN_ID_PROCESSO'
    TABLES
      values_tab      = gt_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.


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

  SELECT SINGLE low FROM tvarvc
    INTO @DATA(lv_stop)
      WHERE name = 'ZINR0001_PAUSA_JOB'.

  IF lv_stop IS NOT INITIAL.
    p_erro = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTA_SET_FINALIZADOS
*&---------------------------------------------------------------------*
FORM f_executa_set_finalizados .

*  DATA lv_info TYPE zde_integracao_http_config.
*  DATA lv_conc TYPE string.
*  DATA lv_template TYPE string.
*
*  lv_template = '{ "nome": "#01#", "descricao": "#02#",' &&
*            '"nomeResponsavel": "#03#", "codigoResponsavel": "#04#",' &&
*            '"status": "#05#", "chave": "#06#"}'.
*
*  SELECT * FROM zint_assina01
*    INTO TABLE @DATA(lt_assin)
*      WHERE id_referencia IN @so_ref
*        AND etapa = '01'.
*
*  CHECK sy-subrc EQ 0 AND sy-batch = space.
*
*  IF p_bus22 IS INITIAL.
*    lv_conc = 'CONCLUIDO'.
*  ELSE.
*    lv_conc = 'RECUSADO'.
*  ENDIF.
*
*  LOOP AT lt_assin ASSIGNING FIELD-SYMBOL(<fs_assina>).
*
*    lv_info-ds_body = lv_template.
*
*    REPLACE '#01#' IN lv_info-ds_body WITH <fs_assina>-nome.
*    REPLACE '#02#' IN lv_info-ds_body WITH <fs_assina>-nome.
*    REPLACE '#03#' IN lv_info-ds_body WITH space.
*    REPLACE '#04#' IN lv_info-ds_body WITH space.
*    REPLACE '#05#' IN lv_info-ds_body WITH lv_conc.
*    REPLACE '#06#' IN lv_info-ds_body WITH <fs_assina>-chave_coleta.
*
*    TRY.
*
*        zcl_integracao_bry_adigital_in=>zif_integracao_bry_adigital_in~get_instance(
*           )->set_ds_data( i_info = lv_info
*           )->set_send_msg(
*                IMPORTING
*                  e_msg = DATA(o_cdata)
*                  e_zintegracao_log  = DATA(e_zintegracao_log)
*           ).
*
*      CATCH zcx_integracao INTO DATA(ex_integracao).
*      CATCH zcx_nfe_xml INTO DATA(ex_nfe_xml).
*      CATCH zcx_error INTO DATA(ex_error).
*
*    ENDTRY.
*
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONFIRMA_BRY
*&---------------------------------------------------------------------*
FORM f_confirma_bry .

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

    IF <fs_dados>-etapa <> '01'.

      MESSAGE 'Etapa Realizada deve ser 01' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.

    IF <fs_dados>-chave_coleta IS INITIAL.

      MESSAGE 'Chave de coleta BRY está vazio, impossivel confirmar assinatura' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.

    <fs_dados>-etapa = '02'.

    UPDATE zint_assina01 SET etapa = '02' WHERE id_referencia = <fs_dados>-id_referencia.

  ENDLOOP.

  COMMIT WORK AND WAIT.

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
*&      Form  F_CANCELA_BRY
*&---------------------------------------------------------------------*
FORM f_cancela_bry .

  DATA lr_selec TYPE RANGE OF flag.

  DATA lv_ret.

  IF sy-batch IS INITIAL.

    PERFORM f_verifica_linha_selec CHANGING lv_ret.

    CHECK lv_ret IS INITIAL.

    PERFORM f_popup_to_confirm USING TEXT-t02 CHANGING lv_ret.

    CHECK lv_ret = '1'.

    APPEND 'IEQX' TO lr_selec.

  ELSE.
    CLEAR lr_selec.

  ENDIF.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec IN lr_selec.

    IF <fs_dados>-etapa <> '01'.

      MESSAGE 'Etapa Realizada deve ser 01' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.

    IF <fs_dados>-chave_coleta IS INITIAL.

      MESSAGE 'Chave de coleta BRY está vazio, impossivel cancelar assinatura' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.

    <fs_dados>-etapa = '98'.

    UPDATE zint_assina01 SET etapa = '98' WHERE id_referencia = <fs_dados>-id_referencia.

  ENDLOOP.

  COMMIT WORK AND WAIT.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_ID_INTERNO
*&---------------------------------------------------------------------*
FORM f_preenche_id_interno .

  LOOP AT so_id.

    CHECK so_id-low IS NOT INITIAL.

    APPEND 'ICP*' && so_id-low && '*' TO gr_id.

  ENDLOOP.

ENDFORM.
