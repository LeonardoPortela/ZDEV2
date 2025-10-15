*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I02
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: l_subrc TYPE sy-subrc.

  l_subrc = 0.

  CASE ok_code.
*-#131273-15.01.2024-JT-inicio
*   WHEN 'ARQXML' OR 'OPENXML'.
*     PERFORM DOWNLOAD_XML.
*     CLEAR: OK_CODE.
*     EXIT.
    WHEN 'ARQXML'.
      PERFORM download_xml USING '1' abap_false 'N'.        "2135316
    WHEN 'ARQXMLA'.
      PERFORM download_xml USING '1' abap_false 'S'.
    WHEN 'OPENXML'.
      PERFORM download_xml USING '2' abap_false 'N'.        "2135316
*-#131273-15.01.2024-JT-fim
    WHEN 'ARQPDF' OR 'OPENPDF'.
      PERFORM download_pdf.
      CLEAR: ok_code.
      EXIT.
  ENDCASE.

  IF sy-tcode = 'ZCTE' AND ( ok_code EQ 'SET_NUM2' OR ok_code EQ 'RESEND_CTE' OR ok_code EQ 'SEND_CTE' ).
    PERFORM verifica_envio_cte USING l_subrc.
  ELSEIF sy-tcode = 'ZCTE' AND
     (
       ( ok_code EQ 'REQ_CANCEL' ) OR
       ( ok_code EQ 'REQ_AGAIN'  ) OR
       ( ok_code EQ 'REQ_CANCE2' ) OR
       ( ok_code EQ 'REQ_AGAIN2' )
     ).

    PERFORM verifica_ciot_cancelado USING l_subrc.
    PERFORM f_check_permissao_cancel_cte USING l_subrc.

  ELSEIF sy-tcode = 'ZNFE' AND ( ( ok_code EQ 'REQ_CANCEL' ) OR ( ok_code EQ 'REQ_AGAIN' ) ).
    PERFORM verifica_nota_possui_cte USING l_subrc.
    IF l_subrc = 0.
      PERFORM verifica_canc_nota USING l_subrc.
      CHECK l_subrc EQ 0.
    ENDIF.
  ENDIF.

  IF ok_code EQ 'SET_NUM' OR
     ok_code EQ 'SET_NUM2' OR
     ok_code EQ 'RESEND_NFE' OR
     ok_code EQ 'RESEND_CTE' OR
     ok_code EQ 'SEND_NFE' OR
     ok_code EQ 'SEND_CTE' OR
     ok_code EQ 'SEND_MDFE'.
    PERFORM verifica_envio_data USING l_subrc.
  ENDIF.

  CHECK l_subrc EQ 0.

  CASE ok_code.
    WHEN 'SELECT' OR 'SELECT2'.
*   Select/de-select present row
      PERFORM row_selection.
    WHEN 'REFRESH' OR 'REFRESH2'.
*   Update content of ALV Grid
      PERFORM grid_refresh.
    WHEN 'NF_WRITER' OR 'NF_WRITER2'.
*   Display selected NF-e in NF writer
      PERFORM display_nfe USING c_100.
    WHEN 'REQ_CANCEL' OR 'REQ_CANCE2'.
*   Call outbound FB to request authorization for NF-e cancellation
      PERFORM request_cancellation.
    WHEN 'REQ_AGAIN' OR 'REQ_AGAIN2'.                       "1161951
*   Call outbound FB to request author. to cancel NF-e again   "1161951
      PERFORM request_cancellation_again.                   "1161951
    WHEN 'CONTING' OR 'CONTING2'.
*   Continue processing under contingency
      PERFORM contingency.
    WHEN 'CONTING_RS' OR 'CONTING_R2'.
*   Reset contingency for selected NF-e
      PERFORM contingency_reset.
    WHEN 'RES_REJECT' OR 'RES_REJEC2'.                      "1248320
*   Rest NF-e rejected from SEFAZ or with validation error     "1248320
      PERFORM reset_rejected_nfe.                           "1248320
    WHEN 'RESEND_NFE' OR 'RESEND_CTE' OR 'RESENDMDFE'.
*   Resend NF-e request (authorization, cancellation, skipping)
      PERFORM send_nfe_again.
    WHEN 'SEND_NFE' OR 'SEND_CTE' OR 'SEND_MDFE'.
*   Send NF-e to SEFAZ
      PERFORM send_nfe USING abap_true.
    WHEN 'SET_NUM' OR 'SET_NUM2' OR 'SET_NUM3'.
*   Determine NF-e number and Send NF-e to SEFAZ               "1265172
      PERFORM set_nfe_number.                               "1265172
    WHEN 'DELETE_LOG' OR 'DELETE_LO2'.
*   Delete error-log entries for selected NF-e
      PERFORM delete_error_log USING abap_true.
    WHEN 'ACCEPT_REJ' OR 'ACCEPT_RE2'.
*   Set SCS to '8' - "Rejection of cancellation request accepte"
      PERFORM set_scs_to_8.
    WHEN 'FIRST_PAGE'.
      PERFORM grid_scroll USING c_100.
    WHEN 'NEXT_PAGE'.
      PERFORM grid_scroll USING c_100.
    WHEN 'PREV_PAGE'.
      PERFORM grid_scroll USING c_100.
    WHEN 'LAST_PAGE'.
      PERFORM grid_scroll USING c_100.
    WHEN 'C_REGION'.
*   Maintain contingency settings per region
      PERFORM contingency_central USING 'REGION'.
    WHEN 'C_BUPLA'.
*   Maintain contingency settings per business place (branch)
      PERFORM contingency_central USING 'BUPLA'.
    WHEN 'CHECK_MS'.
*   Check connection to messaging system
      PERFORM check_connection.
    WHEN 'DANFE' OR 'DACTE' OR 'DAMDFE'.
      PERFORM imprime_danfe_dacte.
    WHEN 'EXPORTA'.
      PERFORM set_dados_exportacao.
    WHEN 'IMPORTA'.
      PERFORM set_dados_importacao.
    WHEN 'CTE_TELA'.
      PERFORM chama_visualizacao_cte.
    WHEN 'CTE_CIOT'.
      PERFORM solicitar_ciot.
    WHEN 'CCORREC'.
      PERFORM carta_correcao.
    WHEN 'CCORR_CTE'.
      PERFORM carta_correcao_cte.
    WHEN: 'MDFE' OR 'MDF_WRITER'.
      PERFORM: manifesto_eletronico.
    WHEN: 'CANC_EXTEM'.
      PERFORM: cancelamento_extemp_interface.
    WHEN: 'REENV_GRC'.
      PERFORM: force_reenvio_grc.
    WHEN: 'CNG_DOC'.
      wg_active_change-change_st_doc = abap_true.
      CALL SCREEN 0109 STARTING AT 02 02.
    WHEN: 'ENC_MDFE'.
      PERFORM: encerrar.
    WHEN c_fcode_emitir_mdfe.
      PERFORM: criar_mdfe_avulsa.
    WHEN 'ESTORNAR'.
      PERFORM: estornar.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    WHEN 'RE_CONTING'.
      PERFORM: reenvia_contingencia.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    WHEN c_fcode_sincdocecc.
      PERFORM f_sinc_docnum_ecc.
**<<<------"188425 - NMS - INI------>>>
    WHEN c_fcode_guia_agro.   "Guia Agropecuária
* Chamada do preenchimento da Guia Agropecuária.
      PERFORM zf_guia_agropecuaria.
**<<<------"188425 - NMS - FIM------>>>
*** Inicio - Rubenilson Pereira - 09.10.2025 #192341
    WHEN c_fcode_frete_segur.
      PERFORM f_insere_frete_seguro.
*** Fim - Rubenilson Pereira - 09.10.2025 #192341
  ENDCASE.
  CLEAR ok_code.

ENDMODULE.                 " user_command_0100  INPUT

*======================================================================
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
*======================================================================
FORM reenvia_contingencia.

  DATA: l_chave          TYPE zde_chave_doc_e,
        l_acckey         TYPE j_1b_nfe_access_key,
        l_erro           TYPE char01,
        l_var_answer     TYPE c,
        w_j_1bnfdoc      TYPE j_1bnfdoc,
        w_j_1bnfe_active TYPE j_1bnfe_active,
        zcl_util         TYPE REF TO zcl_util.

  CREATE OBJECT zcl_util.

* Check if an NF-e selection was made
  IF it_selected_rows[] IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
    RETURN.
  ENDIF.

* Send NF-e that was posted under contingency
  CLEAR l_erro.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

    SELECT SINGLE *
      FROM zsdt0102
      INTO @DATA(w_0102)
     WHERE docnum = @wa_nfe_alv-docnum.

    SELECT SINGLE *
      INTO w_j_1bnfdoc
      FROM j_1bnfdoc
     WHERE docnum = w_0102-docnum.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      INTO w_j_1bnfe_active
      FROM j_1bnfe_active
     WHERE docnum = w_0102-docnum.

    IF NOT ( sy-subrc = 0 AND w_0102-contingencia = abap_true ).
      MESSAGE s024(sd) WITH 'Há documento(s) Selecionado(s) que' ' não é Contingencia!' DISPLAY LIKE 'E'.
      l_erro = abap_true.
      EXIT.
    ENDIF.

    IF NOT ( w_j_1bnfe_active-docsta <> '1' AND
             w_j_1bnfdoc-candat      IS INITIAL ).
      MESSAGE s024(sd) WITH 'Status Documento incompativel para Reenvio!' DISPLAY LIKE 'E'.
      l_erro = abap_true.
      EXIT.
    ENDIF.

  ENDLOOP.

  CHECK l_erro = abap_false.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Atenção'
      text_question         = 'Deseja efetuar o Reenvio de Contingêcia?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = l_var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK l_var_answer = '1'.

  LOOP AT it_selected_rows INTO wa_selected_rows.

    CLEAR: w_0102, w_j_1bnfdoc, w_j_1bnfe_active.

    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

    SELECT SINGLE *
      FROM zsdt0102
      INTO w_0102
     WHERE docnum = wa_nfe_alv-docnum.

    CHECK sy-subrc = 0 AND w_0102-contingencia = abap_true.

    CLEAR: w_j_1bnfdoc, w_j_1bnfe_active.

    SELECT SINGLE *
      INTO w_j_1bnfdoc
      FROM j_1bnfdoc
     WHERE docnum = w_0102-docnum.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      INTO w_j_1bnfe_active
      FROM j_1bnfe_active
     WHERE docnum = w_0102-docnum.

    CHECK sy-subrc = 0.

    IF NOT ( w_j_1bnfe_active-docsta <> '1' AND
             w_j_1bnfdoc-candat      IS INITIAL ).
      CONTINUE.
    ENDIF.

*--- J_1BNFDOC -------------------------------------------
    w_j_1bnfdoc-conting          = abap_true.
    MODIFY j_1bnfdoc          FROM w_j_1bnfdoc.

*--- J_1BNFE_ACTIVE---------------------------------------
    w_j_1bnfe_active-conting     = abap_true.
    w_j_1bnfe_active-tpemis      = '2'.
    w_j_1bnfe_active-docnum9(1)  = w_j_1bnfe_active-tpemis.
    w_j_1bnfe_active-docsta      = abap_off.
    w_j_1bnfe_active-scssta      = abap_off.
    w_j_1bnfe_active-action_requ = '3'.
    w_j_1bnfe_active-msstat      = abap_off.

    l_chave                      = zcl_util->get_chave_nfe( w_0102-docnum ).
    l_acckey                     = l_chave.
    l_acckey-docnum9(1)          = w_j_1bnfe_active-tpemis.

    CALL FUNCTION 'J_1B_NFE_CREATE_CHECK_DIGIT'
      CHANGING
        c_acckey = l_acckey.

    IF w_j_1bnfe_active-nfnum9 IS NOT INITIAL.
      w_j_1bnfe_active-cdv       = l_acckey+43(1).
    ENDIF.

    MODIFY j_1bnfe_active     FROM w_j_1bnfe_active.

    COMMIT WORK AND WAIT.
  ENDLOOP.

*--------------------------
* eliminar entradas log
*--------------------------
  PERFORM delete_error_log USING abap_false.

*--------------------------
* enviar mdfe
*--------------------------
  PERFORM send_nfe         USING abap_false.

*--------------------------
* Update ALV display                                          "1090279
*--------------------------
  PERFORM grid_refresh.                                     "1090279

  MESSAGE s024(sd) WITH 'Documentos atualizados com Sucesso.'.

ENDFORM.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
