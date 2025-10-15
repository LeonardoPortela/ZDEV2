*----------------------------------------------------------------------*
***INCLUDE Z_1BNFE_MONITOR_F34 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  MOVE_SELECTED_TO_MDFE
*&---------------------------------------------------------------------*
FORM new_mdfe_selected.

  PERFORM limpa_list_mdfe. "Limpa Lista MDF-e

  IF vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
    REFRESH: gt_mdfe, gt_uf_perc, gt_hist_mdfe, gt_mdfe_enc, gt_transp_mdfe.
  ELSE.
    REFRESH: gt_mdfe, gt_hist_mdfe, gt_mdfe_enc, gt_transp_mdfe.
  ENDIF.

  CLEAR: gw_mdfe_status, gw_transp_mdfe.

*-#133089-21.02.2024-JT-inicio
  IF vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
    CALL METHOD gc_alv_uf_perc->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD obg_desc_mdfe_canc->set_readonly_mode
      EXPORTING
        readonly_mode = 1.

    CALL METHOD obg_desc_mdfe_canc->set_toolbar_mode
      EXPORTING
        toolbar_mode = '0'.

    CALL METHOD obg_desc_mdfe_canc->set_readonly_mode
      EXPORTING
        readonly_mode = 1.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  "Limpa Justificativa Cancelamento
  REFRESH: tg_editor_mdfe.

  IF vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
    CALL METHOD obg_desc_mdfe_canc->set_text_as_r3table
      EXPORTING
        table = tg_editor_mdfe.
  ENDIF.

  LOOP AT  it_alv_selection INTO wa_alv_selection.
    gw_mdfe-status = icon_warning.
    gw_mdfe-nmdfe  = gw_mdfe_status-nmdfe.
    gw_mdfe-docnum = wa_alv_selection-docnum.
    gw_mdfe-ncte   = wa_alv_selection-nfnum9.
    gw_mdfe-bukrs  = wa_alv_selection-bukrs.
    gw_mdfe-branch = wa_alv_selection-branch.
    APPEND gw_mdfe TO gt_mdfe.
    CLEAR: wa_alv_selection, gw_mdfe.
  ENDLOOP.

  IF gt_uf_perc[] IS INITIAL.
    READ TABLE gt_mdfe INTO DATA(wa_mdfe) INDEX 1.

    zcl_mdfe=>get_parceiro(
      EXPORTING
        i_docnum            = wa_mdfe-docnum
      IMPORTING
        e_id_local_coleta   = DATA(e_id_local_coleta)
        e_id_local_descarga = DATA(e_id_local_descarga) ).

    DATA(it_ufs) = zcl_mdfe=>get_ufs_percurso(
                   i_id_local_coleta   = e_id_local_coleta
                   i_id_local_descarga = e_id_local_descarga ).

    LOOP AT it_ufs INTO DATA(wa_ufs).
      CLEAR: gw_uf_perc.
      gw_uf_perc-uf = wa_ufs-uf.
      APPEND gw_uf_perc TO gt_uf_perc.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " MOVE_SELECTED_TO_MDFE

FORM carrega_docs_mdfe USING e_docnum TYPE j_1bdocnum .

  DATA: it_zsdt0105 TYPE TABLE OF zsdt0105,
        wa_zsdt0105 TYPE zsdt0105.

  DATA: vl_count_mdfe TYPE i.
  DATA: vl_count(2) TYPE c.

  DATA: values  TYPE vrm_values WITH HEADER LINE.

  REFRESH: values.
  CLEAR: values.

  SELECT *
    FROM zsdt0105
    INTO TABLE it_zsdt0105
   WHERE docnum EQ e_docnum.

  SORT  it_zsdt0105 BY docnum_ref DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0105 COMPARING docnum_ref.

  vl_count_mdfe = 1.
  LOOP AT it_zsdt0105 INTO wa_zsdt0105.

    IF NOT ( wa_zsdt0105-docnum_ref IS INITIAL ).
      "AND NOT ( WA_ZSDT0105-NMDFE IS INITIAL ).
      vl_count = vl_count_mdfe.
      CONCATENATE vl_count '-' wa_zsdt0105-docnum_ref INTO values-text SEPARATED BY space.
      values-key  = wa_zsdt0105-docnum_ref.
      APPEND values.

      IF vl_count_mdfe = 1.
        gw_mdfe_docnum_list = wa_zsdt0105-docnum_ref.
      ENDIF.
      ADD 1 TO vl_count_mdfe.
    ENDIF.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'GW_MDFE_DOCNUM_LIST'
      values          = values[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.


ENDFORM.


FORM define_sel_mdfe USING e_docnum TYPE j_1bdocnum .

  DATA: values  TYPE vrm_values WITH HEADER LINE.

  REFRESH: values.
  CLEAR: values.

  CONCATENATE '1 -' e_docnum INTO values-text SEPARATED BY space.
  values-key  = e_docnum.
  APPEND values.

  gw_mdfe_docnum_list = e_docnum.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'GW_MDFE_DOCNUM_LIST'
      values          = values[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    "MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    "        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.

FORM limpa_list_mdfe.

  DATA: values  TYPE vrm_values WITH HEADER LINE.

  REFRESH: values.
  CLEAR: values.

  CLEAR : gw_mdfe_docnum_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'GW_MDFE_DOCNUM_LIST'
      values          = values[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CREATE_CATALOG_MDFE
*&---------------------------------------------------------------------*
FORM create_catalog_mdfe .

*  CLEAR GW_CATALOG_MDFE.
*  GW_CATALOG_MDFE-FIELDNAME = 'STATUS'.
*  GW_CATALOG_MDFE-REPTEXT   = 'Status'.
*  GW_CATALOG_MDFE-JUST      = 'CENTER'.
*  GW_CATALOG_MDFE-OUTPUTLEN = 6.
*  APPEND GW_CATALOG_MDFE TO GT_CATALOG_MDFE.

  CLEAR gw_catalog_mdfe.
  gw_catalog_mdfe-fieldname = 'DOCNUM'.
  gw_catalog_mdfe-reptext   = 'Doc. Num.'.
  gw_catalog_mdfe-outputlen = 10.
  APPEND gw_catalog_mdfe TO gt_catalog_mdfe.

*  CLEAR GW_CATALOG_MDFE.
*  GW_CATALOG_MDFE-FIELDNAME = 'NCTE'.
*  GW_CATALOG_MDFE-REPTEXT   = 'Nº Cte-e'.
*  GW_CATALOG_MDFE-OUTPUTLEN = 10.
*  APPEND GW_CATALOG_MDFE TO GT_CATALOG_MDFE.
*
  CLEAR gw_catalog_mdfe.
  gw_catalog_mdfe-fieldname = 'BUKRS'.
  gw_catalog_mdfe-reptext   = 'Empresa'.
  gw_catalog_mdfe-outputlen = 10.
  APPEND gw_catalog_mdfe TO gt_catalog_mdfe.

  CLEAR gw_catalog_mdfe.
  gw_catalog_mdfe-fieldname = 'BRANCH'.
  gw_catalog_mdfe-reptext   = 'Local de Negocio'.
  gw_catalog_mdfe-outputlen = 20.
  APPEND gw_catalog_mdfe TO gt_catalog_mdfe.


ENDFORM.                    " CREATE_CATALOG_MDFE

*&---------------------------------------------------------------------*
*&      Form  CREATE_CATALOG_UF_PERC
*&---------------------------------------------------------------------*
FORM create_catalog_uf_perc .

  CLEAR gw_catalog_uf_perc.
  gw_catalog_uf_perc-fieldname = 'UF'.
  gw_catalog_uf_perc-reptext   = 'UF'.
  gw_catalog_uf_perc-edit      = 'X'.
  gw_catalog_uf_perc-just      = 'CENTER'.
  gw_catalog_uf_perc-outputlen = 2.
  APPEND gw_catalog_uf_perc TO gt_catalog_uf_perc.

ENDFORM.                    " CREATE_CATALOG_UF_PERC


FORM create_catalog_hist_mdfe .

  CLEAR gw_catalog_hist_mdfe.
  gw_catalog_hist_mdfe-fieldname = 'DS_EVENTO'.
  gw_catalog_hist_mdfe-reptext   = 'Operação'.
  gw_catalog_hist_mdfe-edit      = ''.
  gw_catalog_hist_mdfe-just      = 'CENTER'.
  gw_catalog_hist_mdfe-outputlen = 12.
  APPEND gw_catalog_hist_mdfe TO gt_catalog_hist_mdfe.


*  CLEAR GW_CATALOG_HIST_MDFE.
*  GW_CATALOG_HIST_MDFE-FIELDNAME = 'TP_AUTHCOD'.
*  GW_CATALOG_HIST_MDFE-REPTEXT   = 'Tipo Autorização'.
*  GW_CATALOG_HIST_MDFE-EDIT      = ''.
*  GW_CATALOG_HIST_MDFE-JUST      = 'CENTER'.
*  GW_CATALOG_HIST_MDFE-OUTPUTLEN = 13.
*  APPEND GW_CATALOG_HIST_MDFE TO GT_CATALOG_HIST_MDFE.

  CLEAR gw_catalog_hist_mdfe.
  gw_catalog_hist_mdfe-fieldname = 'AUTHCODE'.
  gw_catalog_hist_mdfe-reptext   = 'Código Autorização'.
  gw_catalog_hist_mdfe-edit      = ''.
  gw_catalog_hist_mdfe-just      = 'CENTER'.
  gw_catalog_hist_mdfe-outputlen = 15.
  APPEND gw_catalog_hist_mdfe TO gt_catalog_hist_mdfe.

  CLEAR gw_catalog_hist_mdfe.
  gw_catalog_hist_mdfe-fieldname = 'DT_AUTHCOD'.
  gw_catalog_hist_mdfe-reptext   = 'Data Autorização'.
  gw_catalog_hist_mdfe-edit      = ''.
  gw_catalog_hist_mdfe-just      = 'CENTER'.
  gw_catalog_hist_mdfe-outputlen = 16.
  APPEND gw_catalog_hist_mdfe TO gt_catalog_hist_mdfe.

  CLEAR gw_catalog_hist_mdfe.
  gw_catalog_hist_mdfe-fieldname = 'HR_AUTHCOD'.
  gw_catalog_hist_mdfe-reptext   = 'Hora Autorização'.
  gw_catalog_hist_mdfe-edit      = ''.
  gw_catalog_hist_mdfe-just      = 'CENTER'.
  gw_catalog_hist_mdfe-outputlen = 16.
  APPEND gw_catalog_hist_mdfe TO gt_catalog_hist_mdfe.

  CLEAR gw_catalog_hist_mdfe.
  gw_catalog_hist_mdfe-fieldname = 'MSG'.
  gw_catalog_hist_mdfe-reptext   = 'Descrição'.
  gw_catalog_hist_mdfe-edit      = ''.
  "GW_CATALOG_HIST_MDFE-JUST      = 'CENTER'.
  gw_catalog_hist_mdfe-outputlen = 80.
  APPEND gw_catalog_hist_mdfe TO gt_catalog_hist_mdfe.

ENDFORM.                    " CREATE_CATALOG_HIST_MDFE


FORM create_catalog_mdfe_enc.

  CLEAR gw_catalog_mdfe_enc.
  gw_catalog_mdfe_enc-fieldname = 'DOCNUM'.
  gw_catalog_mdfe_enc-reptext   = 'Doc.Num MDF-e'.
  gw_catalog_mdfe_enc-edit      = ''.
  gw_catalog_mdfe_enc-just      = 'CENTER'.
  gw_catalog_mdfe_enc-outputlen = 10.
  APPEND gw_catalog_mdfe_enc TO gt_catalog_mdfe_enc.

  CLEAR gw_catalog_mdfe_enc.
  gw_catalog_mdfe_enc-fieldname = 'NMDFE'.
  gw_catalog_mdfe_enc-reptext   = 'Nro. MDF-e'.
  gw_catalog_mdfe_enc-edit      = ''.
  gw_catalog_mdfe_enc-just      = 'CENTER'.
  gw_catalog_mdfe_enc-outputlen = 10.
  APPEND gw_catalog_mdfe_enc TO gt_catalog_mdfe_enc.

  CLEAR gw_catalog_mdfe_enc.
  gw_catalog_mdfe_enc-fieldname = 'ENCERRADO'.
  gw_catalog_mdfe_enc-reptext   = 'Encerrado'.
  gw_catalog_mdfe_enc-edit      = ''.
  gw_catalog_mdfe_enc-just      = 'CENTER'.
  gw_catalog_mdfe_enc-outputlen = 10.
  APPEND gw_catalog_mdfe_enc TO gt_catalog_mdfe_enc.

  CLEAR gw_catalog_mdfe_enc.
  gw_catalog_mdfe_enc-fieldname = 'MSG'.
  gw_catalog_mdfe_enc-reptext   = 'Msg.Retorno'.
  gw_catalog_mdfe_enc-edit      = ''.
  gw_catalog_mdfe_enc-just      = ''.
  gw_catalog_mdfe_enc-outputlen = 80.
  APPEND gw_catalog_mdfe_enc TO gt_catalog_mdfe_enc.

ENDFORM.                    " CREATE_CATALOG_HIST_MDFE


*&---------------------------------------------------------------------*
*&      Form  EMITIR_MDFE
*&---------------------------------------------------------------------*
FORM emitir_mdfe .

  DATA: zcl_mdfe          TYPE REF TO zcl_mdfe,
        vl_docnum_sol_enc TYPE zsdt0105-docnum_ref.

  IF ( gw_mdfe_docnum_list IS INITIAL ).
*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_false.
        MESSAGE 'Nenhum MDF-e Selecionado!' TYPE 'W'.
        RETURN.
      WHEN abap_true.
        MESSAGE s024(sd) WITH 'Nenhum MDF-e Selecionado!'.
        RETURN.
    ENDCASE.
*-#133089-21.02.2024-JT-fim
  ENDIF.

  PERFORM atualizar_mdfe.

  IF NOT ( gw_mdfe_status-docnum IS INITIAL ).
    "AND NOT ( GW_MDFE_STATUS-NMDFE  IS INITIAL ).

    SELECT SINGLE * INTO @DATA(wa_mdfe)
      FROM j_1bnfdoc
     WHERE docnum EQ @gw_mdfe_status-docnum.

    IF sy-subrc IS INITIAL.
      IF vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
        SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_mdfe-docnum.
        SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_mdfe-bukrs.
        CALL TRANSACTION 'ZMDFE' AND SKIP FIRST SCREEN.
      ENDIF.
    ELSE.
      FREE: zcl_mdfe.
      CREATE OBJECT zcl_mdfe
        EXPORTING
          i_nmdfe  = gw_mdfe_status-nmdfe
          i_docnum = gw_mdfe_status-docnum.

      REFRESH: gt_mdfe_enc_aux.
      CLEAR: vl_docnum_sol_enc, gw_mdfe_enc_aux.

      zcl_mdfe->enviar_mdfe( RECEIVING e_docnum_sol_enc = vl_docnum_sol_enc ).

      "Se solicitou encerramento de um outro MDF-e com a mesma placa,
      "pega docnum do MDF-e e acompanha encerramento na própria tela.
      IF ( vl_docnum_sol_enc IS NOT INITIAL ).
        gw_mdfe_enc_aux-docnum = vl_docnum_sol_enc.
        APPEND gw_mdfe_enc_aux TO gt_mdfe_enc_aux.
      ENDIF.
    ENDIF.

    PERFORM atualizar_mdfe.
  ELSE.
    MESSAGE 'MDF-e não localizado para Emissão!' TYPE 'E'.
    RETURN.
  ENDIF.

ENDFORM.                    " EMITIR_MDFE

FORM gravar_mdfe .

  DATA: zcl_mdfe TYPE REF TO zcl_mdfe.
  DATA: vl_docnum_mdfe TYPE j_1bdocnum.
  DATA: vl_docnum_sol_enc TYPE zsdt0105-docnum_ref.

  IF NOT ( gw_mdfe_docnum_list IS INITIAL ).
    MESSAGE 'MDF-e já gravado!' TYPE 'W'.
    EXIT.
  ENDIF.

  IF vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
    CALL METHOD gc_alv_uf_perc->check_changed_data.
  ENDIF.

  IF NOT ( gt_mdfe IS INITIAL ).

    FREE: zcl_mdfe.
    CREATE OBJECT zcl_mdfe.

*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
*---------Validar UFs de Percurso.
    LOOP AT gt_uf_perc INTO gw_uf_perc WHERE NOT ( uf IS INITIAL ).
      DATA(l_erro) = zcl_mdfe->set_validar_uf( gw_uf_perc-uf ).
      IF l_erro = abap_true.
        MESSAGE i024(sd) WITH 'UF informada incorreta!'.
        RETURN.
      ENDIF.
    ENDLOOP.
*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

    "Grava Dados MDF-e
    CASE sy-tcode.
      WHEN 'ZCTE'.
        zcl_mdfe->set_tp_doc_ref( i_tp_doc_ref = '1' ).
      WHEN 'ZNFE'.
        zcl_mdfe->set_tp_doc_ref( i_tp_doc_ref = '2' ).
    ENDCASE.

*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_false.
      WHEN abap_true.
        DATA: lc_tp_doc_ref TYPE zdoc_ref_mdfe.                                      "*-#158056-11.11.2024-JT-inicio
        IMPORT lc_tp_doc_ref = lc_tp_doc_ref FROM MEMORY ID 'FAT_AUTOMATICO_TPDOC'.  "*-#158056-11.11.2024-JT-inicio
        zcl_mdfe->set_tp_doc_ref( i_tp_doc_ref = lc_tp_doc_ref ).                    "*-#158056-11.11.2024-JT-inicio
        FREE MEMORY ID 'FAT_AUTOMATICO_TPDOC'.
    ENDCASE.
*-#133089-21.02.2024-JT-fim

    IF sy-datlo IS INITIAL.
      zcl_mdfe->set_data_emi( sy-datum ).
    ELSE.
      zcl_mdfe->set_data_emi( sy-datlo ).
    ENDIF.

    IF sy-timlo IS INITIAL.
      zcl_mdfe->set_hora_emi( sy-uzeit ).
    ELSE.
      zcl_mdfe->set_hora_emi( sy-timlo ).
    ENDIF.

    "Ini. CS2017002043 04.10.2017
    READ TABLE gt_mdfe INTO gw_mdfe INDEX 1.
    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(_wl_doc)
     WHERE docnum = @gw_mdfe-docnum.

    IF ( sy-subrc = 0 ) AND ( gw_mdfe-docnum IS NOT INITIAL ).
      DATA: v_time_br TYPE erzet.
      CALL FUNCTION 'Z_FUSO_HORARIO_FILIAL'
        EXPORTING
          i_bukrs  = _wl_doc-bukrs
          i_branch = _wl_doc-branch
        IMPORTING
          e_time   = v_time_br.
      IF v_time_br IS NOT INITIAL.
        zcl_mdfe->set_hora_emi( v_time_br ).
      ENDIF.
    ENDIF.
    "Fim. CS2017002043 04.10.2017

    "Adiciona UFs de Percurso.
    LOOP AT gt_uf_perc INTO gw_uf_perc WHERE NOT ( uf IS INITIAL ).
      zcl_mdfe->add_uf_perc( gw_uf_perc-uf ).
    ENDLOOP.

    zcl_mdfe->set_placa_cav(  i_placa_cav  = gw_transp_mdfe-placa_cav ).
    zcl_mdfe->set_placa_car1( i_placa_car1 = gw_transp_mdfe-placa_car1 ).
    zcl_mdfe->set_placa_car2( i_placa_car2 = gw_transp_mdfe-placa_car2 ).
    zcl_mdfe->set_placa_car3( i_placa_car3 = gw_transp_mdfe-placa_car3 ).
    zcl_mdfe->set_motorista(  i_motorista  = gw_transp_mdfe-motorista ).

    "Seta dados transporte Manual.
    "CASE SY-TCODE.
    "  WHEN 'ZNFE'.
    zcl_mdfe->set_cunid(  i_cunid  = gw_transp_mdfe-cunid ).
    zcl_mdfe->set_qcarga( i_qcarga = gw_transp_mdfe-qcarga ).
    zcl_mdfe->set_vcarga( i_vcarga = gw_transp_mdfe-vcarga ).
    "ENDCASE.

    "Adiciona Documentos ao MDF-e
    LOOP AT gt_mdfe INTO gw_mdfe.
      zcl_mdfe->add_documento( gw_mdfe-docnum ).
    ENDLOOP.

    REFRESH: gt_mdfe_enc_aux.
    CLEAR: vl_docnum_sol_enc, gw_mdfe_enc_aux.

    zcl_mdfe->gravar_mdfe( EXPORTING i_faturamento_autom = vg_faturamento_autom "*-#133089-21.02.2024-JT
                                     i_ch_referencia     = vg_ch_referencia     "*-#133089-21.02.2024-JT
                           IMPORTING e_docnum_sol_enc    = vl_docnum_sol_enc
                           RECEIVING e_docnum            = vl_docnum_mdfe ).

    "Se solicitou encerramento de um outro MDF-e com a mesma placa,
    "pega docnum do MDF-e e acompanha encerramento na própria tela.
    IF ( vl_docnum_sol_enc IS NOT INITIAL ).
      gw_mdfe_enc_aux-docnum = vl_docnum_sol_enc.
      APPEND gw_mdfe_enc_aux TO gt_mdfe_enc_aux.
    ENDIF.

    IF ( vl_docnum_mdfe IS INITIAL ).
      PERFORM atualizar_mdfe.
      RETURN.
    ENDIF.

    PERFORM define_sel_mdfe USING vl_docnum_mdfe.
    PERFORM atualizar_mdfe.

  ENDIF.

ENDFORM.                    " GRAVAR_MDFE

FORM cancelar_mdfe .

  DATA: vl_length TYPE i.
  DATA: zcl_mdfe TYPE REF TO zcl_mdfe.
  DATA: txt_just_canc_mdfe TYPE string.

  IF ( gw_mdfe_docnum_list IS INITIAL ).
    MESSAGE 'Nenhum MDF-e Selecionado!' TYPE 'W'.
    RETURN.
  ENDIF.

  PERFORM atualizar_mdfe.

  IF NOT ( gw_mdfe_status-docnum IS INITIAL ) AND
     NOT ( gw_mdfe_status-nmdfe  IS INITIAL ).

    CLEAR: txt_just_canc_mdfe.

    FREE: zcl_mdfe.
    CREATE OBJECT zcl_mdfe
      EXPORTING
        i_nmdfe  = gw_mdfe_status-nmdfe
        i_docnum = gw_mdfe_status-docnum.

    "Justificativa Cancelamento
    IF obg_desc_mdfe_canc IS NOT INITIAL.
      CALL METHOD obg_desc_mdfe_canc->get_text_as_r3table
        IMPORTING
          table = tg_editor_mdfe.

      DELETE tg_editor_mdfe WHERE line IS INITIAL.

      LOOP AT tg_editor_mdfe INTO wg_editor_mdfe.
        IF sy-tabix EQ 1.
          txt_just_canc_mdfe = wg_editor_mdfe-line.
        ELSEIF sy-tabix GE 2.
          CONCATENATE txt_just_canc_mdfe wg_editor_mdfe-line INTO txt_just_canc_mdfe SEPARATED BY space.
        ENDIF.
      ENDLOOP.
    ENDIF.

    txt_just_canc_mdfe = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( txt_just_canc_mdfe ) ) ).

    zcl_mdfe->set_just_canc( txt_just_canc_mdfe ).
    zcl_mdfe->cancelar_mdfe( ).

    PERFORM atualizar_mdfe.

  ELSE.
    MESSAGE 'MDF-e não localizado para Cancelamento!' TYPE 'E'.
    RETURN.
  ENDIF.

ENDFORM.                    " EMITIR_MDFE


FORM encerrar_mdfe .

  DATA: zcl_mdfe TYPE REF TO zcl_mdfe.

  IF ( gw_mdfe_docnum_list IS INITIAL ).
    MESSAGE 'Nenhum MDF-e Selecionado!' TYPE 'W'.
    RETURN.
  ENDIF.

  PERFORM atualizar_mdfe.

  IF ( gw_mdfe_status-docnum IS NOT INITIAL ).

    FREE: zcl_mdfe.
    CREATE OBJECT zcl_mdfe
      EXPORTING
        i_nmdfe  = gw_mdfe_status-nmdfe
        i_docnum = gw_mdfe_status-docnum.

    zcl_mdfe->encerrar_mdfe( i_solicita_motivo = abap_true ).

    PERFORM atualizar_mdfe.

  ELSE.
    MESSAGE 'MDF-e não localizado para Encerramento!' TYPE 'E'.
    RETURN.
  ENDIF.

ENDFORM.                    " EMITIR_MDFE

FORM atualizar_mdfe .

  DATA: it_zsdt0105 TYPE TABLE OF zsdt0105,
        wa_zsdt0105 TYPE zsdt0105,
        it_zsdt0104 TYPE TABLE OF zsdt0104,
        wa_zsdt0104 TYPE zsdt0104,
        it_zsdt0107 TYPE TABLE OF zsdt0107,
        wa_zsdt0107 TYPE zsdt0107,
        wa_j1bnfdoc TYPE j_1bnfdoc.

  CLEAR: gw_mdfe_status, gw_transp_mdfe.
  REFRESH: it_zsdt0105, it_zsdt0104, it_zsdt0107,  gt_uf_perc, gt_mdfe, gt_hist_mdfe, gt_mdfe_enc.

  IF NOT ( gw_mdfe_docnum_list IS INITIAL ).

    "MDF-e Status
    SELECT SINGLE *
      INTO gw_mdfe_status
      FROM zsdt0102
     WHERE docnum = gw_mdfe_docnum_list.

    IF sy-subrc = 0.
      IF ( gw_mdfe_status-autorizado IS NOT INITIAL ) AND ( gw_mdfe_status-estornado IS NOT INITIAL ).
        UPDATE zsdt0102 SET estornado = ''
                      WHERE docnum = gw_mdfe_docnum_list.
        CLEAR: gw_mdfe_status-estornado.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    "MDF-e Documentos
    SELECT *
      FROM zsdt0105
      INTO TABLE it_zsdt0105
     WHERE docnum_ref EQ gw_mdfe_docnum_list.

    "MDF-e UFs Percurso
    SELECT *
      FROM zsdt0104
      INTO TABLE it_zsdt0104
     WHERE docnum EQ gw_mdfe_docnum_list.
    SORT it_zsdt0104 BY ordem_uf ASCENDING.

    "MDF-e Dados Transporte Manual
    SELECT SINGLE *
      FROM zsdt0118 INTO CORRESPONDING FIELDS OF gw_transp_mdfe
     WHERE docnum = gw_mdfe_docnum_list.

    "MDF-e Histórico
    SELECT *
      FROM zsdt0107
      INTO TABLE it_zsdt0107
     WHERE docnum EQ gw_mdfe_docnum_list.

    LOOP AT  it_zsdt0105 INTO wa_zsdt0105.

      gw_mdfe-status = icon_warning.
      gw_mdfe-nmdfe  = wa_zsdt0105-nmdfe.
      gw_mdfe-docnum = wa_zsdt0105-docnum.

      SELECT SINGLE *
        INTO wa_j1bnfdoc
        FROM j_1bnfdoc
       WHERE docnum = wa_zsdt0105-docnum.

      gw_mdfe-bukrs  = wa_j1bnfdoc-bukrs.
      gw_mdfe-branch = wa_j1bnfdoc-branch.

      APPEND gw_mdfe TO gt_mdfe.
      CLEAR: wa_zsdt0105, gw_mdfe.
    ENDLOOP.

    LOOP AT  it_zsdt0104 INTO wa_zsdt0104.

      gw_uf_perc-uf = wa_zsdt0104-uf.

      APPEND gw_uf_perc TO gt_uf_perc.
      CLEAR: wa_zsdt0104, gw_uf_perc.
    ENDLOOP.

    LOOP AT it_zsdt0107 INTO wa_zsdt0107.

      MOVE-CORRESPONDING wa_zsdt0107 TO gw_hist_mdfe.

      CASE gw_hist_mdfe-tp_authcod.
        WHEN '4'.
          gw_hist_mdfe-ds_evento = 'Autorização'.
        WHEN '5'.
          gw_hist_mdfe-ds_evento = 'Cancelamento'.
        WHEN '6'.
          gw_hist_mdfe-ds_evento = 'Encerramento'.
      ENDCASE.

      APPEND gw_hist_mdfe TO gt_hist_mdfe.
      CLEAR: wa_zsdt0107, gw_hist_mdfe.
    ENDLOOP.

*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
*   IF vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
*     CALL METHOD gc_alv_uf_perc->set_ready_for_input
*       EXPORTING
*         i_ready_for_input = 0.
*   ENDIF.
*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    READ TABLE tl_parametros INTO DATA(wl_parametros) WITH KEY parid = 'ZREENVIAR_DOC_GRC'.
    IF sy-subrc = 0.
      IF vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
        IF gw_mdfe_docnum_list IS NOT INITIAL AND  gw_mdfe_status-estornado = abap_false.
          CALL METHOD gc_alv_uf_perc->set_ready_for_input
            EXPORTING
              i_ready_for_input = 1.
        ELSE.
          CALL METHOD gc_alv_uf_perc->set_ready_for_input
            EXPORTING
              i_ready_for_input = 0.
        ENDIF.
      ENDIF.
*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    ELSE.
      IF vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
        CALL METHOD gc_alv_uf_perc->set_ready_for_input
          EXPORTING
            i_ready_for_input = 0.

        CALL METHOD obg_desc_mdfe_canc->set_readonly_mode
          EXPORTING
            readonly_mode = 1.
      ENDIF.
    ENDIF.

    IF ( gw_mdfe_status-autorizado EQ 'X' ) AND
       ( gw_mdfe_status-cancel     NE 'X' ) AND
       ( gw_mdfe_status-encerrado  NE 'X' ) AND
       ( gw_mdfe_status-estornado  NE 'X' ).

      IF vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
        CALL METHOD obg_desc_mdfe_canc->set_readonly_mode
          EXPORTING
            readonly_mode = 0.
      ENDIF.
    ENDIF.

    IF ( gw_mdfe_status-just_canc IS NOT INITIAL ) AND ( gw_mdfe_status-cancel IS NOT INITIAL ).

      "Get Justificativa Cancelamento
      REFRESH: tg_editor_mdfe.

      CLEAR wg_editor_mdfe.
      wg_editor_mdfe-line = gw_mdfe_status-just_canc(100).
      APPEND wg_editor_mdfe TO tg_editor_mdfe.

      CLEAR wg_editor_mdfe.
      wg_editor_mdfe-line = gw_mdfe_status-just_canc+100(100).
      APPEND wg_editor_mdfe TO tg_editor_mdfe.

      CLEAR wg_editor_mdfe.
      wg_editor_mdfe-line = gw_mdfe_status-just_canc+200(55).
      APPEND wg_editor_mdfe TO tg_editor_mdfe.

      IF vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
        CALL METHOD obg_desc_mdfe_canc->set_text_as_r3table
          EXPORTING
            table = tg_editor_mdfe.
      ENDIF.
    ENDIF.

  ENDIF.

  "MDF-e's em processamento de Encerramento
  IF ( gt_mdfe_enc_aux[] IS NOT INITIAL ).
    SELECT *
      FROM zsdt0102 INTO TABLE gt_mdfe_enc
       FOR ALL ENTRIES IN gt_mdfe_enc_aux
     WHERE docnum = gt_mdfe_enc_aux-docnum.
  ENDIF.

ENDFORM.                    " EMITIR_MDFE


FORM print_mdfe .

  DATA: zcl_mdfe TYPE REF TO zcl_mdfe.

  IF ( gw_mdfe_docnum_list IS INITIAL ).
    MESSAGE 'Nenhum MDF-e Selecionado!' TYPE 'W'.
    RETURN.
  ENDIF.

  PERFORM atualizar_mdfe.

  IF NOT ( gw_mdfe_status-docnum IS INITIAL ) AND
     NOT ( gw_mdfe_status-nmdfe  IS INITIAL ).

    FREE: zcl_mdfe.
    CREATE OBJECT zcl_mdfe
      EXPORTING
        i_nmdfe  = gw_mdfe_status-nmdfe
        i_docnum = gw_mdfe_status-docnum.

    zcl_mdfe->print_mdfe( ).

  ELSE.
    MESSAGE 'Impressão não pronta!' TYPE 'W'.
    RETURN.
  ENDIF.


ENDFORM.                    " EMITIR_MDFE

FORM novo_mdfe .

  PERFORM new_mdfe_selected.

ENDFORM.

FORM estornar_mdfe .

  DATA: zcl_mdfe TYPE REF TO zcl_mdfe.

  IF ( gw_mdfe_docnum_list IS INITIAL ).
    MESSAGE 'Nenhum MDF-e Selecionado!' TYPE 'W'.
    RETURN.
  ENDIF.

  PERFORM atualizar_mdfe.

  IF NOT ( gw_mdfe_status-docnum IS INITIAL ) .
    "AND NOT ( GW_MDFE_STATUS-NMDFE  IS INITIAL ).

    FREE: zcl_mdfe.
    CREATE OBJECT zcl_mdfe
      EXPORTING
        i_nmdfe  = gw_mdfe_status-nmdfe
        i_docnum = gw_mdfe_status-docnum.

    zcl_mdfe->estornar_mdfe( ).

    PERFORM atualizar_mdfe.

  ELSE.
    MESSAGE 'MDF-e não localizado para Estorno!' TYPE 'E'.
    RETURN.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INF_TRANSP_MDFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inf_transp_mdfe.

  DATA: values  TYPE vrm_values WITH HEADER LINE.

  CLEAR: values, values[].

  values-text = 'KG'.
  values-key  = '01'.
  APPEND values.

  values-text = 'TON'.
  values-key  = '02'.
  APPEND values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'GW_TRANSP_MDFE-CUNID'
      values          = values[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  CALL SCREEN 0108 STARTING AT 5 5 ENDING AT 60 23.

ENDFORM.

FORM atualiza_transp_mdfe.

  DATA: wa_zlest0002_aux TYPE zlest0002,
        wa_lfa1_aux      TYPE lfa1,
        vl_dados_incompl TYPE c,
        vl_msg_exibir    TYPE string,
        vl_invalid       TYPE c.

  IF ( gw_transp_mdfe-placa_cav IS NOT INITIAL ).

    CLEAR: wa_lfa1_aux, vl_dados_incompl.

    PERFORM valida_placa_mdfe USING '0' "Tração
                           CHANGING gw_transp_mdfe-placa_cav
                                    vl_invalid
                                    wa_zlest0002_aux.
    CHECK vl_invalid IS INITIAL.

    gw_transp_mdfe-cd_cidade         = wa_zlest0002_aux-cd_cidade.
    gw_transp_mdfe-cd_uf             = wa_zlest0002_aux-cd_uf.
    gw_transp_mdfe-cd_renavam        = wa_zlest0002_aux-cd_renavam.
    gw_transp_mdfe-proprietario      = wa_zlest0002_aux-proprietario.
    gw_transp_mdfe-tp_veiculo        = wa_zlest0002_aux-tp_veiculo.

    SELECT SINGLE * INTO wa_lfa1_aux FROM lfa1 WHERE lifnr = gw_transp_mdfe-proprietario.
    gw_transp_mdfe-des_proprietario  = wa_lfa1_aux-name1.

    IF wa_lfa1_aux-stcd1 IS NOT INITIAL.
      gw_transp_mdfe-cnpj_cpf_prop = wa_lfa1_aux-stcd1.
    ELSEIF wa_lfa1_aux-stcd2 IS NOT INITIAL.
      gw_transp_mdfe-cnpj_cpf_prop = wa_lfa1_aux-stcd2.
    ENDIF.

    IF gw_transp_mdfe-cd_cidade IS INITIAL.
      MESSAGE 'Cidade não encontrada!' TYPE 'S'.
      vl_dados_incompl = 'X'.
    ENDIF.

    IF gw_transp_mdfe-cd_uf IS INITIAL.
      MESSAGE 'UF não encontrada!' TYPE 'S'.
      vl_dados_incompl = 'X'.
    ENDIF.

    IF gw_transp_mdfe-cd_renavam IS INITIAL.
      MESSAGE 'Renavam não encontrado!' TYPE 'S'.
      vl_dados_incompl = 'X'.
    ENDIF.

    IF gw_transp_mdfe-proprietario IS INITIAL.
      MESSAGE 'Cod.Proprietário não encontrado!' TYPE 'S'.
      vl_dados_incompl = 'X'.
    ENDIF.

    IF gw_transp_mdfe-des_proprietario IS INITIAL.
      MESSAGE 'Descrição proprietário não encontrada!' TYPE 'S'.
      vl_dados_incompl = 'X'.
    ENDIF.

    IF gw_transp_mdfe-cnpj_cpf_prop IS INITIAL.
      MESSAGE 'CNPJ Proprietário não encontrado!' TYPE 'S'.
      vl_dados_incompl = 'X'.
    ENDIF.

    IF ( vl_dados_incompl IS NOT INITIAL  ).
      CLEAR: gw_transp_mdfe-placa_cav, gw_transp_mdfe-cd_cidade, gw_transp_mdfe-cd_uf,
             gw_transp_mdfe-cd_renavam, gw_transp_mdfe-proprietario, gw_transp_mdfe-tp_veiculo,
             gw_transp_mdfe-des_proprietario, gw_transp_mdfe-cnpj_cpf_prop.
      RETURN.
    ENDIF.

  ENDIF.

  IF ( gw_transp_mdfe-placa_car1 IS NOT INITIAL ).
    PERFORM valida_placa_mdfe USING '1' "Reboque
                           CHANGING gw_transp_mdfe-placa_car1
                                    vl_invalid
                                    wa_zlest0002_aux.
    CHECK vl_invalid IS INITIAL.
  ENDIF.

  IF ( gw_transp_mdfe-placa_car2 IS NOT INITIAL ).
    PERFORM valida_placa_mdfe USING '1' "Reboque
                           CHANGING gw_transp_mdfe-placa_car2
                                    vl_invalid
                                    wa_zlest0002_aux.
    CHECK vl_invalid IS INITIAL.
  ENDIF.

  IF ( gw_transp_mdfe-placa_car3 IS NOT INITIAL ).
    PERFORM valida_placa_mdfe USING '1' "Reboque
                           CHANGING gw_transp_mdfe-placa_car3
                                    vl_invalid
                                    wa_zlest0002_aux.
    CHECK vl_invalid IS INITIAL.
  ENDIF.

  IF ( gw_transp_mdfe-motorista IS NOT INITIAL ).

    PERFORM valida_motorista_mdfe USING gw_transp_mdfe-motorista
                               CHANGING vl_invalid
                                        wa_lfa1_aux.
    CHECK vl_invalid IS INITIAL.

    gw_transp_mdfe-ds_motorista  = wa_lfa1_aux-name1.
    gw_transp_mdfe-cpf_motorista = wa_lfa1_aux-stcd2.
  ENDIF.

ENDFORM.                    " ATUALIZA_DADOS_VEIC

FORM valida_placa_mdfe USING p_tp_veiculo TYPE c
                    CHANGING p_placa      TYPE zplaca
                             p_invalid
                             p_zlest0002  TYPE zlest0002.

  DATA: wa_zlest0002_aux TYPE zlest0002,
        vl_msg_exibir    TYPE string.

  CLEAR: wa_zlest0002_aux, p_invalid, p_zlest0002.

  SELECT SINGLE *
    FROM zlest0002 INTO wa_zlest0002_aux
   WHERE pc_veiculo = p_placa.

  IF sy-subrc NE 0.
    CLEAR: vl_msg_exibir.
    CONCATENATE 'Cadastro da Placa ' p_placa ', não encontrado!'
           INTO vl_msg_exibir SEPARATED BY space.
    MESSAGE vl_msg_exibir TYPE 'S'.
    CLEAR: p_placa.
    p_invalid = c_x.
    RETURN.
  ENDIF.

  IF ( wa_zlest0002_aux-tp_veiculo NE p_tp_veiculo ).
    CASE p_tp_veiculo.
      WHEN '0'.
        CONCATENATE 'Placa' p_placa ', não se refere à um veículo de Tração!'
               INTO vl_msg_exibir SEPARATED BY space.
      WHEN '1'.
        CONCATENATE 'Placa' p_placa ', não se refere à um veículo de Reboque!'
               INTO vl_msg_exibir SEPARATED BY space.
    ENDCASE.

    MESSAGE vl_msg_exibir TYPE 'S'.
    CLEAR: p_placa.
    p_invalid = c_x.
    RETURN.
  ENDIF.

  MOVE-CORRESPONDING wa_zlest0002_aux TO p_zlest0002.

ENDFORM.

FORM valida_motorista_mdfe USING p_lifnr   TYPE lifnr
                        CHANGING p_invalid
                                 p_lfa1    TYPE lfa1.

  DATA: wa_lfa1_aux   TYPE lfa1,
        vl_msg_exibir TYPE string.

  CLEAR: wa_lfa1_aux, p_invalid, p_lfa1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_lifnr
    IMPORTING
      output = p_lifnr.

  SELECT SINGLE *
    INTO wa_lfa1_aux
    FROM lfa1
   WHERE lifnr = p_lifnr.

  IF wa_lfa1_aux-ktokk <> 'ZMOT'.
    CLEAR: gw_transp_mdfe-ds_motorista, gw_transp_mdfe-cpf_motorista, gw_transp_mdfe-motorista.
    MESSAGE 'Dados do cadastro do Motorista estão inválidos!' TYPE 'S'.
    p_invalid = c_x.
    RETURN.
  ENDIF.

  IF wa_lfa1_aux-name1 IS INITIAL.
    CLEAR: gw_transp_mdfe-ds_motorista, gw_transp_mdfe-cpf_motorista, gw_transp_mdfe-motorista.
    MESSAGE 'Nome do Motorista não encontrado!' TYPE 'S'.
    p_invalid = c_x.
    RETURN.
  ENDIF.

  IF wa_lfa1_aux-stcd2 IS INITIAL.
    CLEAR: gw_transp_mdfe-ds_motorista, gw_transp_mdfe-cpf_motorista, gw_transp_mdfe-motorista.
    MESSAGE 'CPF do Motorista não encontrado!' TYPE 'S'.
    p_invalid = c_x.
    RETURN.
  ENDIF.

  MOVE-CORRESPONDING wa_lfa1_aux TO p_lfa1.

ENDFORM.
