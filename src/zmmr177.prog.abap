* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZMMR177                                                *
* Title.......: Cockpit geração de Pedido                              *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 26/09/2022                                             *
* -------------------------------------------------------------------- *
REPORT zmmr177.

TYPE-POOLS: slis, abap, icon.

TABLES: zmme0104, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZMME0104'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

"dados alv
DATA gt_0158 TYPE TABLE OF zsdt0158.
DATA gt_0187 TYPE TABLE OF zsdt0187.
DATA gt_dados_alv TYPE STANDARD TABLE OF zmme0104.
DATA gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.
DATA gt_ponto_c TYPE TABLE OF lfa1.
DATA gt_local_d TYPE TABLE OF lfa1.
DATA gt_trolz TYPE TABLE OF trolz.
"SELECTION-SCREEN: FUNCTION KEY 1

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS so_bukrs FOR zmme0104-bukrs.
SELECT-OPTIONS so_branc FOR zmme0104-filial.
SELECT-OPTIONS so_solic FOR zmme0104-nro_sol_ov.
SELECT-OPTIONS so_ebeln FOR zmme0104-ebeln.
SELECT-OPTIONS so_data  FOR zmme0104-data_atual OBLIGATORY.
SELECT-OPTIONS so_matnr FOR zmme0104-id_produto.
SELECTION-SCREEN END OF BLOCK b1.

" DADOS SECUNDARIOS
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS p_gera RADIOBUTTON GROUP gb01 USER-COMMAND cmd.
PARAMETERS p_nger RADIOBUTTON GROUP gb01.
PARAMETERS p_agua AS CHECKBOX.
PARAMETERS p_sol AS CHECKBOX.
PARAMETERS p_tods RADIOBUTTON GROUP gb01 DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK b2.

" DADOS DO ALV
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  PERFORM f_preenche_data.
  PERFORM f_controle_tela.
  PERFORM default_variant CHANGING p_vari.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant CHANGING p_vari.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_controle_tela.

START-OF-SELECTION.
  PERFORM f_seleciona.
  PERFORM f_processa_dados.
  PERFORM f_exibe_alv.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  SELECT * FROM zsdt0158
    INTO TABLE gt_0158
    WHERE bukrs IN so_bukrs
      AND filial IN so_branc
      AND nro_sol_ov IN so_solic
      AND ebeln IN so_ebeln
      AND data_atual IN so_data
      AND id_produto IN so_matnr
      AND tp_solicitacao = 'P'.

  CHECK sy-subrc EQ 0.

  SELECT * FROM lfa1
    INTO TABLE gt_ponto_c
      FOR ALL ENTRIES IN gt_0158
        WHERE lifnr = gt_0158-id_ponto_coleta.

  IF sy-subrc EQ 0.

    SELECT * FROM trolz
      INTO TABLE gt_trolz
        FOR ALL ENTRIES IN gt_ponto_c
          WHERE azone = gt_ponto_c-lzone.

  ENDIF.

  SELECT * FROM lfa1
    INTO TABLE gt_local_d
      FOR ALL ENTRIES IN gt_0158
        WHERE lifnr = gt_0158-id_local_destino.

  SELECT * FROM zsdt0187
    INTO TABLE gt_0187
      FOR ALL ENTRIES IN gt_0158
        WHERE sequencial = gt_0158-sequencial.

ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
FORM f_processa_dados.

  CLEAR gt_dados_alv.

  LOOP AT gt_0158 ASSIGNING FIELD-SYMBOL(<fs_0158>).

    READ TABLE gt_ponto_c ASSIGNING FIELD-SYMBOL(<fs_ponto_c>)
      WITH KEY lifnr = <fs_0158>-id_ponto_coleta.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_local_d ASSIGNING FIELD-SYMBOL(<fs_local_d>)
      WITH KEY lifnr = <fs_0158>-id_local_destino.

    CHECK sy-subrc EQ 0.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>).

    <fs_dados>-icon = icon_green_light.
    <fs_dados>-msgtx = `Aguardando criação pedido`.

    MOVE-CORRESPONDING <fs_0158> TO <fs_dados>.

    IF <fs_0158>-ebeln IS NOT INITIAL.

      <fs_dados>-icon = icon_complete.
      <fs_dados>-msgtx = `Pedido gerado`.

    ENDIF.

    IF <fs_0158>-nro_sol_ov IS INITIAL.

      <fs_dados>-icon = icon_red_light.
      <fs_dados>-msgtx = `Solicitação aguardando criação`.

    ENDIF.


    IF <fs_0158>-status NE 'L' AND <fs_0158>-status NE 'C'.

      <fs_dados>-icon = icon_red_light.
      <fs_dados>-msgtx = `Solicitação aguardando liberação`.

    ENDIF.

    IF <fs_0158>-status = 'C'.

      <fs_dados>-icon = icon_red_light.
      <fs_dados>-msgtx = `Solicitação cancelada`.

    ENDIF.

    IF <fs_ponto_c>-lzone IS INITIAL.

      "Não foi encontrado a Zona de Transporte do Parceiro Fornecedor:  ZSDT0158-ID_PONTO_COLETA

      <fs_dados>-icon = icon_red_light.
      <fs_dados>-msgtx = `Não existe zona de transporte para o Parc.: ` && <fs_0158>-id_ponto_coleta.

    ENDIF.

    IF <fs_local_d>-lzone IS INITIAL.

      "Não foi encontrado a Zona de Transporte do Parceiro Fornecedor:  ZSDT0158-ID_PONTO_COLETA

      <fs_dados>-icon = icon_red_light.
      <fs_dados>-msgtx = `Não existe zona de transporte para o Parc.: ` && <fs_0158>-id_local_destino.

    ENDIF.

    READ TABLE gt_trolz ASSIGNING FIELD-SYMBOL(<fs_trolz>)
      WITH KEY azone = <fs_ponto_c>-lzone
               lzone = <fs_local_d>-lzone.

    IF sy-subrc NE 0.

      <fs_dados>-icon = icon_red_light.
      <fs_dados>-msgtx = `Não existe itinerário transp.origem: ` && <fs_ponto_c>-lzone && `e transp.destino: ` && <fs_local_d>-lzone.

    ELSE.

      <fs_dados>-route = <fs_trolz>-route.

    ENDIF.

    <fs_dados>-entrega = <fs_ponto_c>-lifnr.
    <fs_dados>-desc_entrega = <fs_ponto_c>-name1.
    <fs_dados>-ponto_c = <fs_local_d>-lifnr.
    <fs_dados>-desc_ponto_c = <fs_local_d>-name1.

  ENDLOOP.

  SORT gt_dados_alv BY sequencial DESCENDING.

  CASE 'X'.
    WHEN p_gera.
      DELETE gt_dados_alv WHERE icon NE icon_complete.
    WHEN p_nger.

      DELETE gt_dados_alv WHERE ebeln IS NOT INITIAL.

      IF p_agua = 'X'.
        DELETE gt_dados_alv WHERE msgtx NE 'Aguardando criação pedido'.
      ENDIF.

      IF p_sol = 'X'.
        DELETE gt_dados_alv WHERE msgtx NE 'Solicitação aguardando liberação'.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
            rs_selfield TYPE slis_selfield.                 "#EC CALLED

  CASE r_ucomm.
    WHEN '&UPD'.
      PERFORM f_refresh.
    WHEN '&IC1'.
      PERFORM f_hyperlink   USING rs_selfield.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'PEDIDO'.
      PERFORM f_inicia_mensagens.
      PERFORM f_gera_pedido.
      PERFORM f_gravar_dados.
      PERFORM f_mensagem_exibe_popup.
      PERFORM f_refresh.
    WHEN 'RECUSAR'.
      PERFORM f_inicia_mensagens.
      PERFORM f_recusar.
      PERFORM f_gravar_dados.
      PERFORM f_mensagem_exibe_popup.
      PERFORM f_refresh.
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
    lw_layout-info_fieldname = 'COLOR'.

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
  DELETE gt_fieldcat WHERE fieldname = 'ROUTE'.

  PERFORM f_coluna_edita USING 'MSGTX' 'Status'.

  PERFORM f_coluna_edita USING 'FILIAL' 'Filial'.
  PERFORM f_coluna_edita2 USING 'SEQUENCIAL' 'SeqNr' 'Sequencial' 'Sequencial'.
  PERFORM f_coluna_edita2 USING 'NRO_SOL_OV' 'NrSol' 'Nr.Solic' 'Nr Solicitação'.

  PERFORM f_coluna_edita2 USING 'ENTREGA' 'LclColeta' 'Coleta' 'Local Coleta'.
  PERFORM f_coluna_edita2 USING 'DESC_ENTREGA' 'LoclCol' 'Local Coleta' 'Descrição Local Coleta'.
  PERFORM f_coluna_edita2 USING 'PONTO_C' 'LclEntreg' 'LocalEntr' 'Local Entrega'.
  PERFORM f_coluna_edita2 USING 'DESC_PONTO_C' 'LoclEnt' 'LocalEntr' 'Descrição Local Entrega'.

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

  DATA(lv_ini) = sy-datum.
  DATA(lv_fim) = sy-datum.

  CHECK so_data[] IS INITIAL.

  SUBTRACT 15 FROM lv_ini.
  "ADD 15 TO lv_fim.

  APPEND 'IBT' && lv_ini && lv_fim TO so_data.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HYPERLINK
*&---------------------------------------------------------------------*
FORM f_hyperlink USING rs_selfield TYPE slis_selfield.

  DATA lw_saida_alv LIKE LINE OF gt_dados_alv.

  CHECK rs_selfield-value IS NOT INITIAL.

  READ TABLE gt_dados_alv INTO lw_saida_alv INDEX rs_selfield-tabindex.

  CASE rs_selfield-fieldname.
    WHEN 'EBELN'.

      DATA lv_ebeln TYPE ebeln.

      lv_ebeln = rs_selfield-value.

      CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
        EXPORTING
          i_ebeln = lv_ebeln.

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
FORM f_mensagem_exibe_popup.

  DATA: l_lines TYPE i.

  DESCRIBE TABLE gt_bapiret2 LINES l_lines.

  IF l_lines <= 1 OR sy-batch = 'X'.

    LOOP AT gt_bapiret2 ASSIGNING FIELD-SYMBOL(<fs_ret2>).

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

    LOOP AT gt_bapiret2 ASSIGNING <fs_ret2>.

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
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
FORM f_coluna_edita2  USING p_fieldname TYPE slis_fieldname
                            p_text_s TYPE scrtext_s
                            p_text_m TYPE scrtext_m
                            p_text_l TYPE scrtext_l.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-seltext_s = p_text_s.
  <fs_cat>-seltext_m = p_text_m.
  <fs_cat>-seltext_l = p_text_l.
  <fs_cat>-reptext_ddic = p_text_l.

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
FORM f_gera_pedido.

  DATA lt_ret TYPE TABLE OF bapiret2.
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

  CLEAR gt_bapiret2.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec IN lr_selec.

    CLEAR lt_ret.

    IF <fs_dados>-icon NE icon_green_light.

      PERFORM f_mensagem_insere_txt USING 'E' 'Status inválido para gerar pedido'.
      CONTINUE.

    ENDIF.

    READ TABLE gt_0158 ASSIGNING FIELD-SYMBOL(<fs_0158>)
      WITH KEY sequencial = <fs_dados>-sequencial.

    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZMM_ME21N_GERA_PEDIDO_COMPRAS'
      EXPORTING
        iw_zsdt0158 = <fs_0158>
        i_route     = <fs_dados>-route
      IMPORTING
        ev_ebeln    = <fs_0158>-ebeln
      TABLES
        et_bapiret2 = lt_ret.

    READ TABLE gt_0187 ASSIGNING FIELD-SYMBOL(<fs_0187>)
      WITH KEY sequencial = <fs_dados>-sequencial.

    IF sy-subrc EQ 0.

      <fs_0187>-ebeln = <fs_0158>-ebeln.
      <fs_0187>-ebelp = '00010'.

    ENDIF.

    APPEND LINES OF lt_ret TO gt_bapiret2.

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
*&      Form  F_REFRESH
*&---------------------------------------------------------------------*
FORM f_refresh .

  PERFORM f_seleciona.
  PERFORM f_processa_dados.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RECUSAR
*&---------------------------------------------------------------------*
FORM f_recusar .

  DATA lt_0158 TYPE TABLE OF zsdt0158.
  DATA lr_selec TYPE RANGE OF flag.
  DATA lv_mess TYPE string.

  DATA lv_ret.

  IF sy-batch IS INITIAL.

    PERFORM f_verifica_linha_selec CHANGING lv_ret.

    CHECK lv_ret IS INITIAL.

    PERFORM f_popup_to_confirm USING text-t02 CHANGING lv_ret.

    CHECK lv_ret = '1'.

    APPEND 'IEQX' TO lr_selec.

  ELSE.
    CLEAR lr_selec.

  ENDIF.

  PERFORM f_get_message_popup CHANGING lv_mess.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec IN lr_selec.

    " esta ok para recusar
    IF <fs_dados>-icon NE icon_green_light.

      PERFORM f_mensagem_insere_txt USING 'E' 'Status inválido para recusar'.
      CONTINUE.

    ENDIF.

    READ TABLE gt_0158 ASSIGNING FIELD-SYMBOL(<fs_0158>)
      WITH KEY sequencial = <fs_dados>-sequencial.

    CHECK sy-subrc EQ 0.

    <fs_0158>-status = 'C'. " <---CANCELADO
    <fs_0158>-mensagem = lv_mess.

    READ TABLE gt_0187 ASSIGNING FIELD-SYMBOL(<fs_0187>)
      WITH KEY sequencial = <fs_dados>-sequencial.

    IF sy-subrc EQ 0.

      "<fs_0187>-cancelado = 'X'.

    ENDIF.

  ENDLOOP.

*  CHECK gt_0158 IS NOT INITIAL.
*
*  MODIFY zsdt0158 FROM TABLE gt_0158.
*
*  COMMIT WORK AND WAIT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_MESSAGE_POPUP
*&---------------------------------------------------------------------*
FORM f_get_message_popup CHANGING p_string TYPE string.

  DATA lt_text TYPE catsxt_longtext_itab.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title = sy-title
    CHANGING
      ch_text  = lt_text.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVAR_DADOS
*&---------------------------------------------------------------------*
FORM f_gravar_dados .

  IF gt_0158 IS NOT INITIAL.

    MODIFY zsdt0158 FROM TABLE gt_0158.
    COMMIT WORK AND WAIT.

  ENDIF.

  IF gt_0187 IS NOT INITIAL .

    MODIFY zsdt0187 FROM TABLE gt_0187.
    COMMIT WORK AND WAIT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INICIA_MENSAGENS
*&---------------------------------------------------------------------*
FORM f_inicia_mensagens .

  CLEAR gt_bapiret2[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONTROLE_TELA
*&---------------------------------------------------------------------*
FORM f_controle_tela .

  LOOP AT SCREEN.

    CASE 'X'.
      WHEN p_gera.

        IF screen-name CS 'P_AGUA' OR screen-name CS 'P_SOL'.
          screen-invisible = 1.
          "screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      WHEN p_nger.

        IF screen-name CS 'P_AGUA' OR screen-name CS 'P_SOL'.
          screen-invisible = 0.
          screen-active = 1.
          MODIFY SCREEN.
        ENDIF.

      WHEN p_tods.

        IF screen-name CS 'P_AGUA' OR screen-name CS 'P_SOL'.
          screen-invisible = 1.
          "screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

    ENDCASE.

  ENDLOOP.

ENDFORM.
