*&---------------------------------------------------------------------*
*& Report  ZMMR0186
*&
*&---------------------------------------------------------------------*
*&  API para consultar formulario de alteração cadastro fornecedor sistema Coupa e fazer as alterações sistema SAP.
*&  ABAP: Anderson Oenning
*&  Analista: Antonio Rodrigues
*&  Modulo de MM
*&---------------------------------------------------------------------*
REPORT zmmr0186.

*&---------------------------------------------------------------------*
*& Declaração tabelas.
*&---------------------------------------------------------------------*
DATA: ws_zmmt0171         TYPE zmmt0171,
      ws_zmmt0172         TYPE zmmt0172,
      it_zmmt0172         TYPE TABLE OF zmmt0172,
      it_tq04s            TYPE TABLE OF tq04s,
      it_lfa1             TYPE TABLE OF lfa1,
      it_adrct            TYPE TABLE OF adrct,
      it_adrct_update     TYPE TABLE OF adrct,
      lc_retorno          TYPE zmme0011_t,
      lc_retorno_form     TYPE zmme0012,
      lc_retorno_form_aux TYPE zmme0015,
      zvg_execut          TYPE char01,
      zdata               TYPE string.

DATA: i_lfa1  TYPE lfa1,
      i_lfb1  TYPE lfb1,
      i_lfm1  TYPE lfm1,
      i_ylfa1 TYPE lfa1,
      i_ylfb1 TYPE lfb1,
      i_ylfm1 TYPE lfm1.

DATA: t_xlfas TYPE TABLE OF  flfas,
      t_xlfb5 TYPE TABLE OF  flfb5,
      t_xlfbk TYPE TABLE OF  flfbk,
      t_xlfza TYPE TABLE OF  flfza,
      t_ylfas TYPE TABLE OF  flfas,
      t_ylfb5 TYPE TABLE OF  flfb5,
      t_ylfbk TYPE TABLE OF  flfbk,
      t_ylfza TYPE TABLE OF  flfza.


START-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Consulta informações tabela de parametro.
*&---------------------------------------------------------------------*
  FREE: it_zmmt0172.

  CLEAR: ws_zmmt0171.
  SELECT SINGLE * FROM zmmt0171
  INTO ws_zmmt0171.

  CHECK ws_zmmt0171 IS NOT INITIAL.

*&---------------------------------------------------------------------*
*& Consulta dados fornecedor aprovados para realizar alteração no SAP
*&---------------------------------------------------------------------*
  CLEAR: ws_zmmt0171-zcheck_api, zvg_execut.
  ws_zmmt0171-zcheck_api = 1.
  ws_zmmt0171-zoffset = 0.
  zvg_execut = abap_true.
  WHILE zvg_execut EQ abap_true.

    TRY .
        zcl_int_ob_cons_form_coupa=>zif_integracao_outbound~get_instance(
        )->execute_request(
         EXPORTING
           i_info_request = ws_zmmt0171
           IMPORTING
             e_integracao = DATA(r_response) ).

        IF r_response IS NOT INITIAL.
          CLEAR: zdata.
          zdata = r_response-ds_data_retorno.
          REPLACE ALL OCCURRENCES OF '-' IN zdata WITH '_'.

          /ui2/cl_json=>deserialize( EXPORTING json = zdata CHANGING data = lc_retorno ).
          IF lc_retorno IS NOT INITIAL.

*&---------------------------------------------------------------------*
*& Consulta dados do formulario individual.
*&---------------------------------------------------------------------*
            CLEAR: r_response.
            LOOP AT lc_retorno ASSIGNING FIELD-SYMBOL(<ls_retorno>).
              TRY .
                  CLEAR: ws_zmmt0171-zcheck_api.
                  ws_zmmt0171-zcheck_api = 3.
                  CLEAR: ws_zmmt0171-id_formulario.
                  ws_zmmt0171-id_formulario = <ls_retorno>-approvable_id.

                  zcl_int_ob_cons_form_coupa=>zif_integracao_outbound~get_instance(
                  )->execute_request(
                   EXPORTING
                     i_info_request = ws_zmmt0171
                     IMPORTING
                       e_integracao = r_response ).

                  IF r_response IS NOT INITIAL.
                    CLEAR: zdata.
                    zdata = r_response-ds_data_retorno.
                    REPLACE ALL OCCURRENCES OF '-' IN zdata WITH '_'.

                    /ui2/cl_json=>deserialize( EXPORTING json = zdata CHANGING data = lc_retorno_form ). "Estrutura de resposta da API.
                    /ui2/cl_json=>deserialize( EXPORTING json = zdata CHANGING data = lc_retorno_form_aux ). "Estrutura de resposta da API.

                    IF lc_retorno_form-current_approval-id IS NOT INITIAL.
                      LOOP AT lc_retorno_form-easy_form_widget_responses ASSIGNING FIELD-SYMBOL(<ws_dados>).
                        "Estrutura principal.
                        ws_zmmt0172-zid_formulario = ws_zmmt0171-id_formulario.
                        ws_zmmt0172-id_integracao  = r_response-id_integracao.
                        ws_zmmt0172-us_criacao     = sy-uname.
                        ws_zmmt0172-dt_criacao     = sy-datum.
                        ws_zmmt0172-hr_criacao     = sy-uzeit.
                        ws_zmmt0172-zid_approvals  = lc_retorno_form-current_approval-id.

                        <ws_dados>-easy_form_widget_id = |{ <ws_dados>-easy_form_widget_id ALPHA = OUT }|.
                        CONDENSE <ws_dados>-easy_form_widget_id NO-GAPS.

                        ws_zmmt0171-id_msg_01 = |{ ws_zmmt0171-id_msg_01 ALPHA = OUT }|.
                        CONDENSE ws_zmmt0171-id_msg_01 NO-GAPS.

                        ws_zmmt0171-id_msg_02 = |{ ws_zmmt0171-id_msg_02 ALPHA = OUT }|.
                        CONDENSE ws_zmmt0171-id_msg_02 NO-GAPS.

                        ws_zmmt0171-id_msg_03 = |{ ws_zmmt0171-id_msg_03 ALPHA = OUT }|.
                        CONDENSE ws_zmmt0171-id_msg_03 NO-GAPS.

                        CASE <ws_dados>-easy_form_widget_id.
                          WHEN ws_zmmt0171-id_msg_01.
                            ws_zmmt0172-lifnr = <ws_dados>-answer-external_ref_num.
                            ws_zmmt0172-lifnr = |{ ws_zmmt0172-lifnr ALPHA = IN }|.
                            ws_zmmt0172-zid_msg_01 = ws_zmmt0171-id_msg_01.
                          WHEN ws_zmmt0171-id_msg_02.
                            ws_zmmt0172-ztipo_bloq = <ws_dados>-answer-external_ref_num.
                            ws_zmmt0172-zid_msg_02 = ws_zmmt0171-id_msg_02.
                          WHEN ws_zmmt0171-id_msg_03.

                            READ TABLE lc_retorno_form_aux-easy_form_widget_responses ASSIGNING FIELD-SYMBOL(<ws_dados_aux>) WITH KEY easy_form_widget_id = <ws_dados>-easy_form_widget_id.
                            IF <ws_dados_aux>-easy_form_widget_id EQ ws_zmmt0171-id_msg_03.
                              ws_zmmt0172-zmsg_justif = <ws_dados_aux>-answer.
                              ws_zmmt0172-zid_msg_03 = ws_zmmt0171-id_msg_03.
                            ENDIF.
                          WHEN OTHERS.
                        ENDCASE.
                      ENDLOOP.
                    ENDIF.
                  ENDIF.
                CATCH zcx_integracao.
                CATCH zcx_error.
              ENDTRY.

              IF ws_zmmt0172 IS NOT INITIAL.
                APPEND ws_zmmt0172 TO it_zmmt0172.
              ENDIF.
              CLEAR: ws_zmmt0172.
            ENDLOOP.
          ENDIF.
        ENDIF.
      CATCH zcx_integracao INTO DATA(ex_integracao).

      CATCH zcx_error INTO DATA(ex_erro).
    ENDTRY.

    IF lc_retorno IS INITIAL.
      zvg_execut = abap_false.
    ELSE.
      ADD 50 TO ws_zmmt0171-zoffset.
      CONDENSE ws_zmmt0171-zoffset NO-GAPS.
      ws_zmmt0171-zcheck_api = 1.
    ENDIF.
  ENDWHILE.

*&---------------------------------------------------------------------*
*& Executa alteração cadastro fornecedor.
*&---------------------------------------------------------------------*
  IF it_zmmt0172 IS NOT INITIAL.

    FREE: it_tq04s.
    SELECT * FROM tq04s INTO TABLE it_tq04s
       WHERE sprache EQ sy-langu.

    FREE: it_lfa1, it_adrct.
    SELECT * FROM lfa1 INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_zmmt0172
    WHERE lifnr EQ it_zmmt0172-lifnr.

    IF it_lfa1 IS NOT INITIAL.
      SELECT * FROM adrct INTO TABLE it_adrct
      FOR ALL ENTRIES IN it_lfa1
        WHERE addrnumber EQ it_lfa1-adrnr.
    ENDIF.

    SORT it_zmmt0172 BY lifnr.
*    DELETE ADJACENT DUPLICATES FROM it_zmmt0172 COMPARING lifnr.

    FREE: it_adrct_update.
    LOOP AT it_zmmt0172 ASSIGNING FIELD-SYMBOL(<ls_zmmt0172>).

      <ls_zmmt0172>-ztipo_proc = '1'.

      CLEAR: i_lfa1.
      READ TABLE it_lfa1 INTO i_lfa1 WITH KEY lifnr = <ls_zmmt0172>-lifnr.
      IF sy-subrc NE 0.
        <ls_zmmt0172>-log_proc = 'Fornecedor não encontrado na base de dados SAP'.
        CONTINUE.
      ENDIF.

      IF i_lfa1-sperr EQ abap_true AND i_lfa1-sperm EQ abap_true.
        <ls_zmmt0172>-log_proc = 'Fornecedor ja se encontra bloqueado, operação não realizada'.
        CONTINUE.
      ENDIF.

      READ TABLE it_tq04s INTO DATA(ws_tq04s) WITH KEY sperrfkt = <ls_zmmt0172>-ztipo_bloq.
      IF sy-subrc NE 0.
        i_lfa1-sperq = '99'.
      ENDIF.

      i_lfa1-sperr = 'X'.
      i_lfa1-sperm = 'X'.

      CALL FUNCTION 'VENDOR_UPDATE' IN UPDATE TASK
        EXPORTING
          i_lfa1  = i_lfa1.

      IF sy-subrc EQ 0.
        COMMIT WORK.
        <ls_zmmt0172>-log_proc = 'Bloqueio realizado com sucesso'.
        <ls_zmmt0172>-zvalida  = abap_true.

        READ TABLE it_adrct INTO DATA(ws_adrct) WITH KEY addrnumber = i_lfa1-adrnr.
        IF sy-subrc EQ 0.
          ws_adrct-remark = <ls_zmmt0172>-zmsg_justif.
          APPEND ws_adrct TO it_adrct_update.
        ENDIF.
      ENDIF.

      CLEAR: ws_adrct.
    ENDLOOP.
  ENDIF.


*&---------------------------------------------------------------------*
*& Enviar o status de processamento do formulario aprovado.
*&---------------------------------------------------------------------*

  LOOP AT it_zmmt0172 ASSIGNING FIELD-SYMBOL(<lw_zmmt0172>) WHERE zvalida IS NOT INITIAL.

    CLEAR: ws_zmmt0171-zcheck_api, r_response.
    ws_zmmt0171-id_formulario = <lw_zmmt0172>-zid_approvals.
    ws_zmmt0171-zcheck_api = 4. "API PUT Aproval

    TRY .
        zcl_int_ob_cons_form_coupa=>zif_integracao_outbound~get_instance(
        )->execute_request(
         EXPORTING
           i_info_request = ws_zmmt0171
           IMPORTING
             e_integracao = r_response ).

        IF r_response IS NOT INITIAL.
          <lw_zmmt0172>-log_proc_status = 'Enviado com sucesso a confirmação alteração'.
        ENDIF.
      CATCH zcx_integracao.
        <lw_zmmt0172>-log_proc_status = 'Erro na comunicação ao enviar a confirmação alteração.'.
      CATCH zcx_error.
        <lw_zmmt0172>-log_proc_status = 'Erro na comunicação ao enviar a confirmação alteração.'.
    ENDTRY.
  ENDLOOP.



*&---------------------------------------------------------------------*
*& Enviar o status de processamento do formulario rejeitado.
*&---------------------------------------------------------------------*

  LOOP AT it_zmmt0172 ASSIGNING FIELD-SYMBOL(<l_zmmt0172>) WHERE zvalida IS INITIAL.

    CLEAR: ws_zmmt0171-zcheck_api, r_response.
    ws_zmmt0171-id_formulario = <l_zmmt0172>-zid_approvals.
    ws_zmmt0171-zmsg_rej      = <l_zmmt0172>-log_proc.
    ws_zmmt0171-zcheck_api    = 5. "API PUT Aproval

    TRY .
        zcl_int_ob_cons_form_coupa=>zif_integracao_outbound~get_instance(
        )->execute_request(
         EXPORTING
           i_info_request = ws_zmmt0171
           IMPORTING
             e_integracao = r_response ).

        IF r_response IS NOT INITIAL.
          <l_zmmt0172>-log_proc_status = 'Enviado com sucesso a confirmação alteração'.
        ENDIF.
      CATCH zcx_integracao.
        <l_zmmt0172>-log_proc_status = 'Erro na comunicação ao enviar a confirmação alteração.'.
      CATCH zcx_error.
        <l_zmmt0172>-log_proc_status = 'Erro na comunicação ao enviar a confirmação alteração.'.
    ENDTRY.
  ENDLOOP.




*&---------------------------------------------------------------------*
*& Salvando alterações tabelas.
*&---------------------------------------------------------------------*

*  SORT it_zmmt0172 BY zvalida.
*  DELETE it_zmmt0172 WHERE zvalida NE abap_true.

  IF ( it_zmmt0172[] IS NOT INITIAL ).

    MODIFY zmmt0172 FROM TABLE it_zmmt0172.

    IF ( it_adrct_update[] IS NOT INITIAL ).
      MODIFY adrct FROM TABLE it_adrct_update.
    ENDIF.

    COMMIT WORK AND WAIT.
  ENDIF.
