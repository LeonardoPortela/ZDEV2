* ==================================================================== *
*                         © AMAGGI                                     *
* ==================================================================== *
* Program.....: ZSDR0233                                               *
* Title.......: JOB Recebimento ZSDT0116 - EMBARQUE                    *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 23/07/2025                                             *
* -------------------------------------------------------------------- *
REPORT zsdr0233.

TYPE-POOLS: slis, abap, icon.

TABLES: zsds_zsdt0116_rec_alv, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZSDS_ZSDT0116_REC_ALV'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

"dados alv
DATA gt_ovs TYPE TABLE OF zsd_est_filtro_rec_01.
DATA gt_simu TYPE TABLE OF zi_in_simu_ov.
DATA gt_0116 TYPE TABLE OF zsdt0116.
DATA gt_0116_mod TYPE TABLE OF zsdt0116.
DATA gt_0090 TYPE TABLE OF zsdt0090.

DATA gt_recebi TYPE TABLE OF zi_in_lctos_ov_sum.

DATA gt_dados_alv TYPE STANDARD TABLE OF zsds_zsdt0116_rec_alv.
DATA gt_exc_alv TYPE STANDARD TABLE OF zsds_zsdt0116_rec_alv.
DATA gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.
DATA gv_erro.

"SELECTION-SCREEN: FUNCTION KEY 1

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS so_vbeln FOR zsds_zsdt0116_rec_alv-vbeln NO INTERVALS..
  "SELECT-OPTIONS so_seq FOR zsds_zsdt0116_rec_alv-seq NO-EXTENSION NO INTERVALS.
  "SELECT-OPTIONS so_ori FOR zsds_zsdt0116_rec_alv-saldo_origem NO-EXTENSION NO INTERVALS.
  PARAMETERS p_vis AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

START-OF-SELECTION.

  PERFORM f_job_exec CHANGING gv_erro.

  IF gv_erro = abap_false.

    " seleciona as ovs com recebimento
    PERFORM f_seleciona.

    " processa as atualizações na tabela zsdt0116
    PERFORM f_processar_atualizacao.

    " processa para quais ovs irão o recebimento
    PERFORM f_processar_recebimento.

    IF sy-batch = abap_true.
      PERFORM f_gravar_dados.
    ELSE.

      IF p_vis IS INITIAL.
        PERFORM f_gravar_dados.
      ENDIF.

      PERFORM f_exibe_alv.
    ENDIF.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  CLEAR: gt_0116_mod, gt_0116[], gt_recebi[].

  PERFORM f_sap_indicator USING 'Selecionando Ovs...' 50.

  SELECT doc_simulacao,vbeln,waerk,existe,valor_total_ov,
         valor_total_ov_m,valor_recebido_ov,valor_recebido_ov_m,
         recebido_total,saldo_positivo
    FROM zsd_est_filtro_rec_01
        WHERE vbeln IN @so_vbeln
          AND ( recebido_total = @abap_false "<-- nao foram 100% recebidas
              OR saldo_positivo = @abap_true ) "<-- ou com saldo positivo
     INTO TABLE @gt_ovs.

  CHECK sy-subrc EQ 0.


  PERFORM f_sap_indicator USING 'Selecionando simuladores...' 50.

  SELECT * FROM zi_in_simu_ov
    INTO TABLE @gt_simu
      FOR ALL ENTRIES IN @gt_ovs
        WHERE doc_simulacao = @gt_ovs-doc_simulacao.

  " busca os desmembramentos
  IF sy-subrc EQ 0.

    SELECT * FROM zsdt0090
      INTO TABLE @gt_0090
       FOR ALL ENTRIES IN @gt_simu
         WHERE doc_simulacao = @gt_simu-doc_simulacao
         AND vbelv = @gt_simu-vbeln
          AND categoria = 'D'. "<- desmembramento

  ENDIF.

  PERFORM f_sap_indicator USING 'Selecionando embarques...' 50.

  SELECT * FROM zsdt0116
    INTO TABLE @gt_0116
      FOR ALL ENTRIES IN @gt_simu
        WHERE vbeln = @gt_simu-vbeln
        AND posnr = '000000'.

  SORT gt_0116 BY vbeln seq ASCENDING.

  PERFORM f_sap_indicator USING 'Selecionando recebimentos...' 50.

  SELECT * FROM zi_in_lctos_ov_sum
    INTO TABLE @gt_recebi
      FOR ALL ENTRIES IN @gt_simu
      WHERE vbeln_va = @gt_simu-vbeln
        AND num_reg > 0.

ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
            rs_selfield TYPE slis_selfield.                 "#EC CALLED

  CASE r_ucomm.
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
  DELETE gt_fieldcat WHERE fieldname = 'MANDT'.

  PERFORM f_coluna_edita USING 'MSGTX' 'Status'.

  PERFORM f_coluna_edita USING 'SEQ' 'Sequencia'.
  PERFORM f_coluna_edita USING 'VBELN' 'OV'.

  PERFORM f_coluna_edita USING 'SALDO_ORIGEM' 'Origem'.
  PERFORM f_coluna_edita USING 'VLR_RECEBIDO' 'Recebido.USD'.
  PERFORM f_coluna_edita USING 'VLR_RECEBIDO_MOEDA' 'Recebido.MOEDA'.
  PERFORM f_coluna_edita USING 'VLR_LIBERADO' 'Liberado.USD'.
  PERFORM f_coluna_edita USING 'VLR_LIBERADO_MOEDA' 'Liberado.MOEDA'.
  PERFORM f_coluna_edita USING 'VLR_RESTANTE' 'Restante.USD'.
  PERFORM f_coluna_edita USING 'VLR_RESTANTE_MOEDA' 'Restante.MOEDA'.

ENDFORM.                    " F_MONTA_FIELDCAT
"PERFORM f_fieldcat_modi USING 'PESO' 'EDIT' 'X' CHANGING lt_fieldcat.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_MODI
*&---------------------------------------------------------------------*
FORM f_fieldcat_modi USING p_fieldname TYPE slis_fieldname
                           p_column TYPE c
                           p_value TYPE any
                  CHANGING p_field_cat TYPE slis_t_fieldcat_alv.

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
FORM f_sap_indicator_vbeln USING p_text TYPE c
                           p_vbeln TYPE vbeln
                           p_percent TYPE i.

  DATA lv_mess TYPE c LENGTH 60.

  lv_mess = p_text.

  REPLACE '&' IN lv_mess WITH p_vbeln.

  IF sy-batch = 'X'.

    IF strlen( lv_mess ) > 40.
      MESSAGE s000(d2) WITH lv_mess(40) lv_mess+40.
    ELSE.
      MESSAGE s000(d2) WITH lv_mess.
    ENDIF.

  ELSE.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = p_percent
        text       = lv_mess.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
FORM f_sap_indicator USING p_text TYPE c
                           p_percent TYPE i.

  IF sy-batch = 'X'.

    IF strlen( p_text ) > 40.
      MESSAGE s016(ds) WITH p_text(40) p_text+40.
    ELSE.
      MESSAGE s016(ds) WITH p_text.
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
*& Form f_processar_recebimento
*&---------------------------------------------------------------------*
FORM f_processar_recebimento.

  DATA lv_msgx TYPE c LENGTH 50.
  DATA lv_vbeln TYPE vbeln_va.
  DATA lv_a_rec TYPE netwr_ap.
  DATA lv_a_rec_moeda TYPE netwr_ap.
  DATA lv_vlr_rec TYPE netwr_ap.
  DATA lv_vlr_rec_moeda TYPE netwr_ap.
  DATA lv_ordem TYPE i.

  CLEAR gt_dados_alv[].

  LOOP AT gt_recebi ASSIGNING FIELD-SYMBOL(<fs_rece>).

    CLEAR lv_ordem.

    READ TABLE gt_simu ASSIGNING FIELD-SYMBOL(<fs_simu>) WITH KEY vbeln = <fs_rece>-vbeln_va.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_ovs ASSIGNING FIELD-SYMBOL(<fs_ov>) WITH KEY doc_simulacao = <fs_simu>-doc_simulacao.

    CHECK sy-subrc EQ 0.

    PERFORM f_sap_indicator_vbeln USING 'Inicio processamento recebimento: ' lv_vbeln 50.

    lv_vbeln = <fs_rece>-vbeln_va.

    "05.09.2025 -->
    " verificação de status cancelado
    IF line_exists( gt_0116[ vbeln =  lv_vbeln status = abap_true ] ).
      APPEND CORRESPONDING #( gt_0116[ vbeln =  lv_vbeln status = abap_true ] ) TO gt_exc_alv.
      "CONTINUE.
    ENDIF.
    "05.09.2025 --<

    " recupera o valor total a receber da zsdt0116
    PERFORM f_valor_a_rec USING lv_vbeln CHANGING gt_0116 lv_a_rec lv_a_rec_moeda.

    lv_vlr_rec = <fs_rece>-vlr_rec_usd_sum.

    " recuperar o valor recebido
    IF gt_ovs[ doc_simulacao = <fs_simu>-doc_simulacao ]-waerk <> 'USD'.

      lv_vlr_rec_moeda = <fs_rece>-vlr_rec_brl_sum.

      DATA(lv_kursf) = VALUE #( gt_0116[ vbeln = lv_vbeln ]-kursf DEFAULT '1.00' ).

      IF lv_kursf IS INITIAL.
        lv_kursf = '1.00'.
      ENDIF.

      IF lv_kursf <> '1.00'.
        lv_vlr_rec = lv_vlr_rec_moeda / lv_kursf.
      ENDIF.

    ELSE.

      lv_vlr_rec_moeda = <fs_rece>-vlr_rec_usd_sum.

    ENDIF.

    " 1°. Calcula pelo valor liberado pelo Financeiro
    PERFORM f_calcular_linha USING lv_vbeln 'F' gt_0116 CHANGING lv_ordem lv_a_rec lv_a_rec_moeda lv_vlr_rec lv_vlr_rec_moeda gt_dados_alv[].

    " 2°. Calcula pelo valor liberado pelo OPUS
    PERFORM f_calcular_linha USING lv_vbeln 'O' gt_0116 CHANGING lv_ordem lv_a_rec lv_a_rec_moeda lv_vlr_rec lv_vlr_rec_moeda gt_dados_alv[].

    " 3°. Calcula pelo valor liberado pelo SAP
    PERFORM f_calcular_linha USING lv_vbeln 'S' gt_0116 CHANGING lv_ordem lv_a_rec lv_a_rec_moeda lv_vlr_rec lv_vlr_rec_moeda gt_dados_alv[].

    " 4°. Calcula pelo valor liberado pela ZSDT0117
    PERFORM f_calcular_linha USING lv_vbeln 'Z' gt_0116 CHANGING lv_ordem lv_a_rec lv_a_rec_moeda lv_vlr_rec lv_vlr_rec_moeda gt_dados_alv[].

    "03.10.2025 - RAMON - 192303 -->
    " 5°. Calcula valores aprovador por referencia
    PERFORM f_calcular_ref USING lv_vbeln gt_0116 CHANGING lv_ordem lv_a_rec lv_a_rec_moeda lv_vlr_rec lv_vlr_rec_moeda gt_dados_alv[].
    "03.10.2025 - RAMON - 192303 --<

    " 6°. Calcula valores sem origem ( Valores da ov antes de abrir para aprovação )
    PERFORM f_calcular_linha_sem USING lv_vbeln <fs_ov>-waerk CHANGING lv_ordem lv_a_rec lv_a_rec_moeda lv_vlr_rec lv_vlr_rec_moeda gt_dados_alv[].

    IF line_exists( gt_dados_alv[ vbeln =  lv_vbeln ] ).
      PERFORM f_sap_indicator_vbeln USING 'Dados de recebimentos processados para a OV:' lv_vbeln 50.
    ENDIF.

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
*& Form f_valor_a_rec
*&---------------------------------------------------------------------*
FORM f_valor_a_rec USING uv_vbeln TYPE vbeln_va
                CHANGING ct_0116 TYPE  zsds_zsdt0116_tab
                         cv_a_rec TYPE netwr_ap
                         cv_a_rec_moeda TYPE netwr_ap.

  DATA lv_moeda TYPE netwr_ap.
  CLEAR: cv_a_rec, cv_a_rec_moeda.

  LOOP AT ct_0116 ASSIGNING FIELD-SYMBOL(<fs_116>) WHERE vbeln = uv_vbeln.

    ADD <fs_116>-vlr_liberado TO cv_a_rec.
    ADD <fs_116>-vlr_liberado_moeda TO cv_a_rec_moeda.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_calcular_linha
*&---------------------------------------------------------------------*
FORM f_calcular_linha USING uv_vbeln TYPE vbeln_va
                            uv_origem TYPE zsde_saldo_origem
                            ut_0116 TYPE  zsds_zsdt0116_tab
                   CHANGING cv_ordem TYPE i "<-- ordem em que foi recebido
                            cv_a_rec TYPE netwr_ap
                            cv_a_rec_moeda TYPE netwr_ap
                            cv_rec TYPE netwr_ap
                            cv_rec_moeda TYPE netwr_ap
                            ct_rec TYPE zsdc0116_rec.

  DATA lv_a_rec_aux TYPE netwr_ap.
  DATA lv_a_rec_moeda_aux TYPE netwr_ap.

  LOOP AT ut_0116 ASSIGNING FIELD-SYMBOL(<fs_0116>)
      WHERE vbeln = uv_vbeln
        AND saldo_origem = uv_origem
        AND status = space.

    IF cv_rec IS INITIAL.
      EXIT.
    ENDIF.

    ADD 1 TO cv_ordem.

    lv_a_rec_aux = cv_rec.
    lv_a_rec_moeda_aux = cv_rec_moeda.

    APPEND INITIAL LINE TO ct_rec ASSIGNING FIELD-SYMBOL(<fs_calc>).

    <fs_calc>-vbeln = uv_vbeln.
    <fs_calc>-waerk = <fs_0116>-waerk.
    <fs_calc>-saldo_origem = <fs_0116>-saldo_origem.
    <fs_calc>-seq = <fs_0116>-seq.

    <fs_calc>-id_limite = <fs_0116>-id_limite.
    <fs_calc>-ordem_check = cv_ordem.

    <fs_calc>-vlr_liberado = <fs_0116>-vlr_liberado.
    <fs_calc>-vlr_liberado_moeda = <fs_0116>-vlr_liberado_moeda.

    SUBTRACT <fs_0116>-vlr_liberado FROM lv_a_rec_aux.
    SUBTRACT <fs_0116>-vlr_liberado_moeda FROM lv_a_rec_moeda_aux.


    " se ficar positivo, recebi tudo
    IF lv_a_rec_aux > 0.

      <fs_calc>-vlr_recebido_moeda = <fs_0116>-vlr_liberado_moeda.
      <fs_calc>-vlr_recebido = <fs_0116>-vlr_liberado.

      cv_rec = lv_a_rec_aux.
      cv_rec_moeda = lv_a_rec_moeda_aux.

      " se negativo, recebi menos do que esperava.
    ELSE.
      <fs_calc>-vlr_recebido = cv_rec.
      <fs_calc>-vlr_recebido_moeda = cv_rec_moeda.

      " zera o que foi recebido
      cv_rec = 0.
      cv_rec_moeda = 0.

    ENDIF.

    <fs_calc>-vlr_restante_moeda = <fs_0116>-vlr_liberado_moeda - <fs_calc>-vlr_recebido_moeda.
    <fs_calc>-vlr_restante = <fs_0116>-vlr_liberado - <fs_calc>-vlr_recebido.

    IF <fs_calc>-vlr_restante IS INITIAL.
      <fs_calc>-icon = icon_checked.
      <fs_calc>-msgtx = 'Recebido'.
    ELSEIF <fs_calc>-vlr_recebido IS INITIAL.
      <fs_calc>-icon = icon_incomplete.
      <fs_calc>-msgtx = 'Sem Recebimento'.
    ELSEIF <fs_calc>-vlr_restante > 0.
      <fs_calc>-icon = icon_led_yellow.
      <fs_calc>-msgtx = 'Parcial'.
    ENDIF.

    GET TIME.

    <fs_calc>-user_create = sy-uname.
    <fs_calc>-date_create = sy-datum.
    <fs_calc>-time_create = sy-uzeit.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_calcular_ref
*&---------------------------------------------------------------------*
FORM f_calcular_ref USING uv_vbeln TYPE vbeln_va
                          ut_0116 TYPE  zsds_zsdt0116_tab
                 CHANGING cv_ordem TYPE i
                          cv_a_rec TYPE netwr_ap
                          cv_a_rec_moeda TYPE netwr_ap
                          cv_rec TYPE netwr_ap
                          cv_rec_moeda TYPE netwr_ap
                          ct_rec TYPE zsdc0116_rec.

  DATA lv_a_rec_aux TYPE netwr_ap.
  DATA lv_a_rec_moeda_aux TYPE netwr_ap.

  " verifica se ainda tem saldo a ser distribuido
  CHECK cv_rec > 0.

  LOOP AT gt_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>) WHERE vbelv = uv_vbeln.

    " percorre a OV, procurando por referenciadas a ela mesmo
    LOOP AT ut_0116 ASSIGNING FIELD-SYMBOL(<fs_0116>)
        WHERE vbeln = <fs_0090>-vbelv
          AND status = space.

      " percorre as OVS REF. ( somente aquelas que existem na ZSDT0116)
      LOOP AT ut_0116 ASSIGNING FIELD-SYMBOL(<fs_ref>)
        WHERE vbeln = <fs_0090>-vbeln "<- OV desmembrada
              AND seq_ref = <fs_0116>-seq "< - seq de referencia
              AND vbeln_ref = <fs_0116>-vbeln
              AND posnr_ref = '000000'
              AND aprov_por_ref = abap_true.

        PERFORM f_linha_recebimento
          USING <fs_ref>-vbeln
                <fs_ref>-seq
                <fs_ref>-waerk
                <fs_ref>-saldo_origem
                <fs_ref>-id_limite
                <fs_ref>-vlr_liberado
                <fs_ref>-vlr_liberado_moeda
       CHANGING cv_ordem
                cv_rec
                cv_rec_moeda
                ct_rec.

      ENDLOOP.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_calcular_linha
*&---------------------------------------------------------------------*
FORM f_calcular_linha_sem USING uv_vbeln TYPE vbeln_va
                                uv_waerk TYPE waerk
                       CHANGING cv_ordem TYPE i "<-- ordem em que foi recebido
                                cv_a_rec TYPE netwr_ap
                                cv_a_rec_moeda TYPE netwr_ap
                                cv_rec TYPE netwr_ap
                                cv_rec_moeda TYPE netwr_ap
                                ct_rec TYPE zsdc0116_rec.

  DATA lv_a_rec_aux TYPE netwr_ap.
  DATA lv_a_rec_moeda_aux TYPE netwr_ap.

  CHECK cv_rec > 0.

  APPEND INITIAL LINE TO ct_rec ASSIGNING FIELD-SYMBOL(<fs_calc>).

  <fs_calc>-vbeln = uv_vbeln.
  <fs_calc>-waerk = uv_waerk.
  <fs_calc>-saldo_origem = ''. "<--- sem origem
  <fs_calc>-seq = '0000000000'. "<--- sem sequencia da zsdt0116

  ADD 1 TO cv_ordem.

  <fs_calc>-ordem_check = cv_ordem.

  <fs_calc>-vlr_liberado = 0."<--- nenhum valor liberado
  <fs_calc>-vlr_liberado_moeda = 0."<--- nenhum valor liberado

  <fs_calc>-vlr_recebido = cv_rec.

  IF uv_waerk = 'USD'.
    <fs_calc>-vlr_recebido_moeda = cv_rec.
  ELSE.
    <fs_calc>-vlr_recebido_moeda = cv_rec_moeda.

  ENDIF.

  <fs_calc>-vlr_restante_moeda = 0.
  <fs_calc>-vlr_restante = 0.

  <fs_calc>-icon = icon_checked.
  <fs_calc>-msgtx = 'Saldo Financeiro'.

  <fs_calc>-user_create = sy-uname.
  <fs_calc>-date_create = sy-datum.
  <fs_calc>-time_create = sy-uzeit.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_gravar_dados
*&---------------------------------------------------------------------*
FORM f_gravar_dados .

  DATA lt_rece TYPE TABLE OF zsdt0116_rec.
  DATA lt_backup TYPE TABLE OF zsdt0116_rec.

  PERFORM f_sap_indicator USING 'Gravando dados...' 50.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>).

    APPEND INITIAL LINE TO lt_rece ASSIGNING FIELD-SYMBOL(<fs_rece>).

    <fs_rece>-seq = <fs_dados>-seq.
    <fs_rece>-vbeln = <fs_dados>-vbeln.
    <fs_rece>-waerk = <fs_dados>-waerk.
    <fs_rece>-saldo_origem = <fs_dados>-saldo_origem.
    <fs_rece>-id_limite = <fs_dados>-id_limite.

    <fs_rece>-ordem_check = <fs_dados>-ordem_check.

    <fs_rece>-date_create = <fs_dados>-date_create.
    <fs_rece>-user_create = <fs_dados>-user_create.
    <fs_rece>-time_create = <fs_dados>-time_create.


    <fs_rece>-vlr_recebido = <fs_dados>-vlr_recebido.
    <fs_rece>-vlr_recebido_moeda = <fs_dados>-vlr_recebido_moeda.
    "<fs_rece>-vlr_restante = <fs_dados>-vlr_restante.

  ENDLOOP.

  "PERFORM f_check_change_db TABLES lt_rece lt_backup.

  "CHECK lt_rece[] IS NOT INITIAL.

  PERFORM f_exc_status_x.

  PERFORM f_atualiza_zsdt0016.

  " exclui registros de saldo financeiro
  " recebidos em execuções passadas
  " isso porque pode ser que o valor tenha sido usado por outra ov
  PERFORM f_exc_receb_saldo_fin TABLES lt_rece[].

  MODIFY zsdt0116_rec FROM TABLE lt_rece[].

  COMMIT WORK AND WAIT.

  PERFORM f_sap_indicator USING 'Dados de recebimento gravados' 100.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_JOB_EXEC
*&---------------------------------------------------------------------*
FORM f_job_exec CHANGING cv_erro.

  SELECT * INTO TABLE @DATA(it_tbtco)
    FROM tbtco AS j
   WHERE j~status EQ 'R'.

  IF sy-subrc IS INITIAL.
    SELECT * INTO TABLE @DATA(it_tbtcp)
      FROM tbtcp
       FOR ALL ENTRIES IN @it_tbtco
     WHERE progname EQ @sy-cprog
       AND jobname  EQ @it_tbtco-jobname
       AND jobcount EQ @it_tbtco-jobcount.
  ENDIF.

  DESCRIBE TABLE it_tbtcp LINES DATA(e_qtd).

  IF e_qtd GT 1.

    PERFORM f_sap_indicator USING 'Já existe um job travando o processamento' 50.
    cv_erro = abap_true.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_exc_receb_saldo_fin
*&---------------------------------------------------------------------*
FORM f_exc_receb_saldo_fin TABLES t_receb STRUCTURE zsdt0116_rec.

  DATA(lt_adia) = t_receb[].

  SORT lt_adia BY vbeln ASCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_adia COMPARING vbeln.

  CHECK lt_adia[] IS NOT INITIAL.

  PERFORM f_sap_indicator USING 'Excluindo saldo financeiro...' 50.

  LOOP AT lt_adia ASSIGNING FIELD-SYMBOL(<fs_adia>).

    DELETE FROM zsdt0116_rec
      WHERE seq = '0000000000'
        AND vbeln = <fs_adia>-vbeln.

  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_change_db
*&---------------------------------------------------------------------*
FORM f_check_change_db TABLES t_receb STRUCTURE zsdt0116_rec
                              t_backup STRUCTURE zsdt0116_rec.
  DATA lv_change.

  CHECK t_receb[] IS NOT INITIAL.

  PERFORM f_sap_indicator USING 'Excluindo inalterados...' 50.

  SELECT * FROM zsdt0116_rec
    INTO TABLE t_backup
      FOR ALL ENTRIES IN t_receb
        WHERE vbeln = t_receb-vbeln
          AND seq = t_receb-seq.

  LOOP AT t_receb ASSIGNING FIELD-SYMBOL(<fs_rece>).

    CLEAR lv_change.

    READ TABLE t_backup ASSIGNING FIELD-SYMBOL(<fs_backup>)
      WITH KEY vbeln = <fs_rece>-vbeln
               seq = <fs_rece>-seq.

    CHECK sy-subrc EQ 0.

    IF <fs_rece>-waerk <> <fs_backup>-waerk. lv_change = abap_true.ENDIF.
    IF <fs_rece>-saldo_origem <> <fs_backup>-saldo_origem. lv_change = abap_true.ENDIF.
    IF <fs_rece>-vlr_recebido <> <fs_backup>-vlr_recebido. lv_change = abap_true.ENDIF.
    IF <fs_rece>-vlr_recebido_moeda <> <fs_backup>-vlr_recebido_moeda. lv_change = abap_true.ENDIF.

    CHECK lv_change IS INITIAL.

    PERFORM f_sap_indicator_vbeln USING 'Registro: & não foi alterado' <fs_rece>-vbeln 50.

    DELETE t_receb WHERE vbeln = <fs_backup>-vbeln AND seq = <fs_backup>-seq.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exc_status_x
*&---------------------------------------------------------------------*
FORM f_exc_status_x .

  DATA lt_rece_exc TYPE TABLE OF zsdt0116_rec.
  DATA lv_msgx TYPE c LENGTH 200.

  LOOP AT gt_exc_alv ASSIGNING FIELD-SYMBOL(<fs_dados>).

    APPEND INITIAL LINE TO lt_rece_exc ASSIGNING FIELD-SYMBOL(<fs_rece>).

    <fs_rece>-seq = <fs_dados>-seq.
    <fs_rece>-vbeln = <fs_dados>-vbeln.
    <fs_rece>-waerk = <fs_dados>-waerk.
    <fs_rece>-saldo_origem = <fs_dados>-saldo_origem.
    <fs_rece>-id_limite = <fs_dados>-id_limite.

    <fs_rece>-ordem_check = <fs_dados>-ordem_check.

    "<fs_rece>-vlr_liberado = <fs_dados>-vlr_liberado.
    <fs_rece>-vlr_recebido = <fs_dados>-vlr_recebido.
    <fs_rece>-vlr_recebido_moeda = <fs_dados>-vlr_recebido_moeda.
    "<fs_rece>-vlr_restante = <fs_dados>-vlr_restante.

    lv_msgx =  |{ 'Excluindo registro: ' } - { <fs_rece>-vbeln } sequencia: { <fs_rece>-seq } |.

    PERFORM f_sap_indicator USING lv_msgx 50.

  ENDLOOP.

  IF lt_rece_exc[] IS NOT INITIAL.

    DELETE zsdt0116_rec FROM TABLE lt_rece_exc[].
    COMMIT WORK AND WAIT.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_atualiza_zsdt0016
*&---------------------------------------------------------------------*
FORM f_atualiza_zsdt0016 .

  CHECK gt_0116_mod IS NOT INITIAL.

  MODIFY zsdt0116 FROM TABLE gt_0116_mod.

  CLEAR gt_0116_mod[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_linha_recebimento
*&---------------------------------------------------------------------*
FORM f_linha_recebimento USING uv_vbeln TYPE vbeln_va
                               uv_seq   TYPE numc10
                               uv_waerk TYPE waerk
                               uv_origem TYPE zsde_saldo_origem
                               uv_id_limite TYPE zsde_id_limite_sap
                               uv_vlr_liberado TYPE netwr_ap
                               uv_vlr_liberado_moeda TYPE netwr_ap
                      CHANGING cv_ordem TYPE i
                               cv_rec TYPE netwr_ap
                               cv_rec_moeda TYPE netwr_ap
                               ct_rec TYPE zsdc0116_rec.

  DATA lv_a_rec_aux TYPE netwr_ap.
  DATA lv_a_rec_moeda_aux TYPE netwr_ap.

  IF cv_rec IS INITIAL.
    EXIT.
  ENDIF.

  ADD 1 TO cv_ordem.

  lv_a_rec_aux = cv_rec.
  lv_a_rec_moeda_aux = cv_rec_moeda.

  APPEND INITIAL LINE TO ct_rec ASSIGNING FIELD-SYMBOL(<fs_calc>).

  " Recebe pela referencia
  <fs_calc>-vbeln = uv_vbeln.

  <fs_calc>-waerk = uv_waerk.
  <fs_calc>-saldo_origem = uv_origem.
  <fs_calc>-seq = uv_seq.

  <fs_calc>-id_limite = uv_id_limite.
  <fs_calc>-ordem_check = cv_ordem.

  <fs_calc>-vlr_liberado = uv_vlr_liberado.
  <fs_calc>-vlr_liberado_moeda = uv_vlr_liberado_moeda.

  SUBTRACT uv_vlr_liberado FROM lv_a_rec_aux.
  SUBTRACT uv_vlr_liberado_moeda FROM lv_a_rec_moeda_aux.


  " se ficar positivo, recebi tudo
  IF lv_a_rec_aux > 0.

    <fs_calc>-vlr_recebido_moeda = uv_vlr_liberado_moeda.
    <fs_calc>-vlr_recebido = uv_vlr_liberado.

    cv_rec = lv_a_rec_aux.
    cv_rec_moeda = lv_a_rec_moeda_aux.

    " se negativo, recebi menos do que esperava.
  ELSE.
    <fs_calc>-vlr_recebido = cv_rec.
    <fs_calc>-vlr_recebido_moeda = cv_rec_moeda.

    " zera o que foi recebido
    cv_rec = 0.
    cv_rec_moeda = 0.

  ENDIF.

  <fs_calc>-vlr_restante_moeda = uv_vlr_liberado_moeda - <fs_calc>-vlr_recebido_moeda.
  <fs_calc>-vlr_restante = uv_vlr_liberado - <fs_calc>-vlr_recebido.

  IF <fs_calc>-vlr_restante IS INITIAL.
    <fs_calc>-icon = icon_checked.
    <fs_calc>-msgtx = 'Recebido'.
  ELSEIF <fs_calc>-vlr_recebido IS INITIAL.
    <fs_calc>-icon = icon_incomplete.
    <fs_calc>-msgtx = 'Sem Recebimento'.
  ELSEIF <fs_calc>-vlr_restante > 0.
    <fs_calc>-icon = icon_led_yellow.
    <fs_calc>-msgtx = 'Parcial'.
  ENDIF.

  GET TIME.

  <fs_calc>-user_create = sy-uname.
  <fs_calc>-date_create = sy-datum.
  <fs_calc>-time_create = sy-uzeit.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processar_atualizacao
*&---------------------------------------------------------------------*
FORM f_processar_atualizacao .

  DATA lv_valor TYPE netwr_ap.
  DATA lv_valor_moeda TYPE netwr_ap.
  DATA lv_moeda TYPE netwr_ap.
  DATA lv_simulador TYPE zsded003.
  CLEAR gt_0116_mod.

  LOOP AT gt_simu ASSIGNING FIELD-SYMBOL(<fs_simu>).

*    " ( não pode ter recebido tudo E sem saldo positivo(inicial) ) OU ter saldo positivo
*    CHECK ( <fs_ovs>-recebido_total = abap_false
*      AND <fs_ovs>-saldo_positivo = abap_false ) OR <fs_ovs>-saldo_positivo = abap_true.

    CLEAR: lv_valor, lv_valor_moeda.

    " recuperar o total original do lançamento da ZSDT0116
    LOOP AT gt_0116 ASSIGNING FIELD-SYMBOL(<fs_0116>) WHERE vbeln = <fs_simu>-vbeln.

      ADD <fs_0116>-vlr_original TO lv_valor.
      ADD <fs_0116>-vlr_original_moeda TO lv_valor_moeda.

    ENDLOOP.

    " se nao encontrou no loop de cima, pode sair
    CHECK sy-subrc EQ 0.

    " recuperar valores corrigidos pelo desmembramento
    LOOP AT gt_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>) WHERE vbelv = <fs_simu>-vbeln.

      IF gt_ovs[ doc_simulacao = <fs_0090>-doc_simulacao ]-waerk <> 'USD'.

        IF <fs_0090>-kurrf IS NOT INITIAL.
          lv_moeda = <fs_0090>-netwr / <fs_0090>-kurrf.
        ELSE.
          lv_moeda = <fs_0090>-netwr / 1.
        ENDIF.

        SUBTRACT lv_moeda FROM lv_valor.

      ELSE.

        SUBTRACT <fs_0090>-netwr FROM lv_valor.

      ENDIF.

      SUBTRACT <fs_0090>-netwr FROM lv_valor_moeda.

    ENDLOOP.

    " verificar se houve mudança no valor
    LOOP AT gt_0116 ASSIGNING <fs_0116> WHERE vbeln = <fs_simu>-vbeln.

      CHECK <fs_0116>-vlr_liberado <> lv_valor
        OR <fs_0116>-vlr_liberado_moeda <> lv_valor_moeda.

      " se sim, atualiza os valores na tabela interna
      <fs_0116>-vlr_liberado = lv_valor.
      <fs_0116>-vlr_liberado_moeda = lv_valor_moeda.

      PERFORM f_sap_indicator_vbeln USING 'Atualizando valor embarque OV: ' <fs_simu>-vbeln 50.

      " manda para atualização a linha da tabela
      APPEND <fs_0116> TO gt_0116_mod.


    ENDLOOP.

  ENDLOOP.



ENDFORM.
