*&---------------------------------------------------------------------*
*& Report  ZMMR187
*&
*&---------------------------------------------------------------------*
*&  API para consultar formulario de alteração cadastro fornecedor sistema Coupa e fazer as alterações sistema SAP.
*&  ABAP: Anderson Oenning
*&  Analista: Antonio Rodrigues
*&  Modulo de MM
*&---------------------------------------------------------------------*
report zmmr187.

*&---------------------------------------------------------------------*
*& Declaração tabelas.
*&---------------------------------------------------------------------*
types: begin of ty_result_id_cliente,
         id     type string,
         number type string,
         status type string,
       end of ty_result_id_cliente.

types: begin of ty_list_lookup,
         id               type string,
         active           type string,
         external_ref_num type string,
       end of ty_list_lookup.

data: ws_zmmt0171           type zmmt0171,
      ws_zmmt0172           type zmmt0172,
      it_zmmt0172           type table of zmmt0172,
      it_tq04s              type table of tq04s,
      it_lfa1               type table of lfa1,
      it_but000             type table of but000,
      it_adrct              type table of adrct,
      it_adrct_update       type table of adrct,

      lc_retorno            type zmme0011_t,
      lc_retorno_form       type zmme0012,
      lc_result_id_cliente  type ty_result_id_cliente,
      lc_result_list_lookup type table of ty_list_lookup,
      lc_retorno_form_aux   type zmme0015,
      zvg_execut            type char01,
      zdata                 type string,
      zv_just               type string,
      zv_status             type string,
      zv_id                 type string,
      zjson                 type string,
      zv_lines              type p.

data: i_lfa1    type lfa1,
      i_lfb1    type lfb1,
      i_lfm1    type lfm1,
      i_ylfa1   type lfa1,
      i_ylfb1   type lfb1,
      i_ylfm1   type lfm1,
      i_ybut000 type but000.

data: t_xlfas type table of  flfas,
      t_xlfb5 type table of  flfb5,
      t_xlfbk type table of  flfbk,
      t_xlfza type table of  flfza,
      t_ylfas type table of  flfas,
      t_ylfb5 type table of  flfb5,
      t_ylfbk type table of  flfbk,
      t_ylfza type table of  flfza.

data: vl_message(150) type c.


start-of-selection.

*&---------------------------------------------------------------------*
*& Consulta informações tabela de parametro.
*&---------------------------------------------------------------------*
  free: it_zmmt0172.

  clear: ws_zmmt0171.
  select single * from zmmt0171
  into ws_zmmt0171.

  check ws_zmmt0171 is not initial.

*&---------------------------------------------------------------------*
*& Consulta dados fornecedor aprovados para realizar alteração no SAP
*&---------------------------------------------------------------------*
  clear: ws_zmmt0171-zcheck_api, zvg_execut.
  ws_zmmt0171-zcheck_api = 2.
  ws_zmmt0171-zoffset = 0.
  zvg_execut = abap_true.
  ws_zmmt0171-para_serv_url = 'CONS_FORM_COUPA_SAP'.
  while zvg_execut eq abap_true.

    try .
        zcl_int_ob_cons_form_coupa=>zif_integracao_outbound~get_instance(
        )->execute_request(
         exporting
           i_info_request = ws_zmmt0171
           importing
             e_integracao = data(r_response) ).

        if r_response is not initial.
          clear: zdata.
          zdata = r_response-ds_data_retorno.
          replace all occurrences of '-' in zdata with '_'.

          free: lc_retorno.
          /ui2/cl_json=>deserialize( exporting json = zdata changing data = lc_retorno ).
          if lc_retorno is not initial.


*&---------------------------------------------------------------------*
*& Consulta dados do formulario individual.
*&---------------------------------------------------------------------*
            clear: r_response.
            loop at lc_retorno assigning field-symbol(<ls_retorno>).

              vl_message = |Aguarde, processando formulario { <ls_retorno>-approvable_id }|.

              call function 'SAPGUI_PROGRESS_INDICATOR'
                exporting
                  percentage = 99
                  text       = vl_message.

              try .
                  clear: ws_zmmt0171-zcheck_api.
                  ws_zmmt0171-zcheck_api = 3.
                  clear: ws_zmmt0171-id_formulario.
                  ws_zmmt0171-id_formulario = <ls_retorno>-approvable_id.
                  ws_zmmt0171-para_serv_url = 'CONS_FORM_COUPA_SAP'.

                  zcl_int_ob_cons_form_coupa=>zif_integracao_outbound~get_instance(
                  )->execute_request(
                   exporting
                     i_info_request = ws_zmmt0171
                     importing
                       e_integracao = r_response ).

                  if r_response is not initial.
                    clear: zdata.
                    zdata = r_response-ds_data_retorno.
                    replace all occurrences of '-' in zdata with '_'.

                    clear: lc_retorno_form, lc_retorno_form_aux.
                    /ui2/cl_json=>deserialize( exporting json = zdata changing data = lc_retorno_form ). "Estrutura de resposta da API.
                    /ui2/cl_json=>deserialize( exporting json = zdata changing data = lc_retorno_form_aux ). "Estrutura de resposta da API.
                    ws_zmmt0171-id_desat_forn = |{ ws_zmmt0171-id_desat_forn alpha = out }|.
                    condense ws_zmmt0171-id_desat_forn no-gaps.



                    if lc_retorno_form-current_approval-approver_id eq ws_zmmt0171-id_desat_forn.

                      ws_zmmt0172-lifnr = lc_retorno_form-subject-supplier_number.
                      ws_zmmt0172-lifnr = |{ ws_zmmt0172-lifnr alpha = in }|.

*                    estrutura principal.
                      ws_zmmt0172-zid_formulario = ws_zmmt0171-id_formulario.
                      ws_zmmt0172-id_integracao  = r_response-id_integracao.
                      ws_zmmt0172-us_criacao     = sy-uname.
                      ws_zmmt0172-dt_criacao     = sy-datum.
                      ws_zmmt0172-hr_criacao     = sy-uzeit.
                      ws_zmmt0172-zid_approvals  = lc_retorno_form-current_approval-id.


                      loop at lc_retorno_form-easy_form_widget_responses assigning field-symbol(<ws_dados>).
                        if ws_zmmt0172-lifnr is initial.
                          ws_zmmt0172-lifnr = <ws_dados>-answer-external_ref_num.
                          ws_zmmt0172-lifnr = |{ ws_zmmt0172-lifnr alpha = in }|.
                        endif.
**
*                        <ws_dados>-easy_form_widget_id = |{ <ws_dados>-easy_form_widget_id ALPHA = OUT }|.
*                        CONDENSE <ws_dados>-easy_form_widget_id NO-GAPS.
*
*                        ws_zmmt0171-id_msg_01_desb = |{ ws_zmmt0171-id_msg_01_desb ALPHA = OUT }|.
*                        CONDENSE ws_zmmt0171-id_msg_01_desb NO-GAPS.
*
*                        ws_zmmt0171-id_msg_02_desb = |{ ws_zmmt0171-id_msg_02_desb ALPHA = OUT }|.
*                        CONDENSE ws_zmmt0171-id_msg_02_desb NO-GAPS.
*
*                        ws_zmmt0171-id_msg_03_desb = |{ ws_zmmt0171-id_msg_03_desb ALPHA = OUT }|.
*                        CONDENSE ws_zmmt0171-id_msg_03_desb NO-GAPS.
*
*                        CASE <ws_dados>-easy_form_widget_id.
*                          WHEN ws_zmmt0171-id_msg_01_desb.
*
*                            ws_zmmt0172-ztipo_bloq = <ws_dados>-answer-custom_fields-codigo_de_bloqueio_fornecedor.
*                            ws_zmmt0172-zid_msg_01 = ws_zmmt0171-id_msg_01.
*
**                            " Início - DEVK9A1QIG - 25.10.2023 MM - Int. SAP x Coupa Ativar/Des Fornecedor #117180 AO
**                            CLEAR: ws_zmmt0171-zcheck_api.
**                            ws_zmmt0171-zcheck_api = 6.
**                            CLEAR: ws_zmmt0171-id_formulario.
**                            READ TABLE lc_retorno_form_aux-easy_form_widget_responses ASSIGNING FIELD-SYMBOL(<ws_dados_responses>) WITH KEY easy_form_widget_id = <ws_dados>-easy_form_widget_id.
**                            IF sy-subrc EQ 0.
**                              ws_zmmt0171-id_formulario = <ws_dados_responses>-answer.
**
**                              clear: r_response.
**                              zcl_int_ob_cons_form_coupa=>zif_integracao_outbound~get_instance(
**                              )->execute_request(
**                               EXPORTING
**                                 i_info_request = ws_zmmt0171
**                                 IMPORTING
**                                   e_integracao = r_response ).
**                            ENDIF.
**
**                            IF r_response IS NOT INITIAL.
**                              /ui2/cl_json=>deserialize( EXPORTING json = r_response-ds_data_retorno CHANGING data = lc_result_id_cliente ). "Estrutura de resposta da API.
**                            CONDENSE ws_zmmt0171-id_ativa_forn NO-GAPS.
**                            ws_zmmt0172-lifnr = <ws_dados>-answer-external_ref_num.
**                            ws_zmmt0172-lifnr = |{ ws_zmmt0172-lifnr ALPHA = IN }|.
**                            CONDENSE ws_zmmt0172-lifnr NO-GAPS.
**                            ws_zmmt0172-zid_msg_01 = ws_zmmt0171-id_msg_01_desb.
**                            ENDIF.
*                            " Fim - DEVK9A1QIG - 25.10.2023 MM - Int. SAP x Coupa Ativar/Des Fornecedor #117180 AO
*
*                          WHEN ws_zmmt0171-id_msg_02_desb.
*                            ws_zmmt0172-ztipo_bloq = <ws_dados>-answer-external_ref_num.
*                            ws_zmmt0172-zid_msg_02 = ws_zmmt0171-id_msg_02_desb.
*                          WHEN ws_zmmt0171-id_msg_03_desb.
*
*                            READ TABLE lc_retorno_form_aux-easy_form_widget_responses ASSIGNING FIELD-SYMBOL(<ws_dados_aux>) WITH KEY easy_form_widget_id = <ws_dados>-easy_form_widget_id.
*                            IF <ws_dados_aux>-easy_form_widget_id EQ ws_zmmt0171-id_msg_03_desb.
*                              ws_zmmt0172-zmsg_justif = <ws_dados_aux>-answer.
*                              ws_zmmt0172-zid_msg_03 = ws_zmmt0171-id_msg_03_desb.
*                            ENDIF.
*                          WHEN OTHERS.
*                        ENDCASE.
                      endloop.
                    endif.
                  endif.
                catch zcx_integracao.
                catch zcx_error.
              endtry.

              if ws_zmmt0172 is not initial.
                append ws_zmmt0172 to it_zmmt0172.
              endif.
              clear: ws_zmmt0172, r_response.
            endloop.
          endif.
        endif.
      catch zcx_integracao into data(ex_integracao).

      catch zcx_error into data(ex_erro).
    endtry.

    if lc_retorno is initial.
      zvg_execut = abap_false.
    else.
      add 50 to ws_zmmt0171-zoffset.
      condense ws_zmmt0171-zoffset no-gaps.
      ws_zmmt0171-zcheck_api = 2.
    endif.
  endwhile.

*&---------------------------------------------------------------------*
*& Executa alteração cadastro fornecedor.
*&---------------------------------------------------------------------*
  if it_zmmt0172 is not initial.

    free: it_tq04s.
    select * from tq04s into table it_tq04s
       where sprache eq sy-langu.

    free: it_lfa1, it_adrct, it_but000.
    select * from lfa1 into table it_lfa1
    for all entries in it_zmmt0172
    where lifnr eq it_zmmt0172-lifnr.

    if it_lfa1 is not initial.
      select * from adrct into table it_adrct
      for all entries in it_lfa1
        where addrnumber eq it_lfa1-adrnr.

      select * from but000 into table it_but000
       for all entries in it_lfa1
         where partner eq it_lfa1-lifnr.
    endif.

    sort it_zmmt0172 by lifnr.
*    DELETE ADJACENT DUPLICATES FROM it_zmmt0172 COMPARING lifnr.

    free: it_adrct_update.
    loop at it_zmmt0172 assigning field-symbol(<ls_zmmt0172>).

      vl_message = |Aguarde, validando fornecedor { <ls_zmmt0172>-lifnr } form:{ <ls_zmmt0172>-zid_formulario }|.
      <ls_zmmt0172>-ztipo_proc = '2'.

      clear: i_lfa1.
      read table it_lfa1 into i_lfa1 with key lifnr = <ls_zmmt0172>-lifnr.
      if sy-subrc ne 0.
        <ls_zmmt0172>-log_proc = 'Fornecedor não encontrado na base de dados SAP'.
        continue.
      endif.

      if i_lfa1-sperr eq abap_false and i_lfa1-sperm eq abap_false.
        <ls_zmmt0172>-log_proc = 'Fornecedor ja se encontra ativo, operação não realizada'.
        continue.
      endif.

      read table it_but000 into i_ybut000 with key partner = <ls_zmmt0172>-lifnr.
      if sy-subrc eq 0.
        i_ybut000-xblck = abap_false.
        i_ybut000-xdele = abap_false.
      endif.


      i_lfa1-sperq = space.
      i_lfa1-sperr = abap_false.
      i_lfa1-sperm = abap_false.
      i_lfa1-nodel = abap_false.
      i_lfa1-loevm = abap_false.
      i_lfa1-cvp_xblck = abap_false.

      i_lfb1-sperr = abap_false.
      i_lfb1-nodel = abap_false.
      i_lfb1-loevm = abap_false.

*-----------------------------------------
* Desbloquear fornecedor
*-----------------------------------------


      if i_lfa1 is not initial.
        if i_ybut000 is not initial.
          modify but000 from i_ybut000.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              wait = 'X'.
        endif.

        call function 'VENDOR_UPDATE' in update task
          exporting
            i_lfa1 = i_lfa1
            i_lfb1 = i_lfb1.

*        modify lfa1 from i_lfa1.
        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = 'X'.

        if sy-subrc ne 0.
          <ls_zmmt0172>-log_proc = 'Erro modificar fornecedor'.
          <ls_zmmt0172>-zvalida  = abap_false.
        else.

*          call function 'BAPI_TRANSACTION_COMMIT'
*            exporting
*              wait = 'X'.

          <ls_zmmt0172>-log_proc = 'Desbloqueio realizado com sucesso'.
          <ls_zmmt0172>-zvalida  = abap_true.


          read table it_adrct into data(ws_adrct) with key addrnumber = i_lfa1-adrnr.
          if sy-subrc eq 0.
            ws_adrct-remark = <ls_zmmt0172>-zmsg_justif.
            append ws_adrct to it_adrct_update.
          else.
            if i_lfa1-adrnr is not initial.
              append value #(
                        addrnumber  = i_lfa1-adrnr
                        date_from   = '00010101'
                        nation      = ''
                        langu       = sy-langu
                        remark      = <ls_zmmt0172>-zmsg_justif "'1785'
              ) to it_adrct_update.
            endif.
          endif.

*&---------------------------------------------------------------------*
*& Consultar fornecedor na lista lookup
**&---------------------------------------------------------------------*
          try .

              clear: ws_zmmt0171-zcheck_api, r_response.
              ws_zmmt0171-zcheck_api = 7.
              ws_zmmt0171-para_serv_url = 'CONS_LIST_LOOKUP_COUPA_SAP'.
              ws_zmmt0171-id_param = |{ <ls_zmmt0172>-lifnr alpha = out }|.

              zcl_int_ob_cons_form_coupa=>zif_integracao_outbound~get_instance(
              )->execute_request(
               exporting
                 i_info_request = ws_zmmt0171
                 importing
                   e_integracao = r_response ).

              clear: zv_lines.
              /ui2/cl_json=>deserialize( exporting json = r_response-ds_data_retorno changing data = lc_result_list_lookup ). "Estrutura de resposta da API.
              describe table lc_result_list_lookup lines zv_lines.
              if lc_result_list_lookup is not initial.
                read table lc_result_list_lookup into data(ws_list) index 1.
                if sy-subrc eq 0.
                  clear: ws_zmmt0171-zcheck_api, r_response.
                  ws_zmmt0171-zcheck_api = 9.
                  ws_zmmt0171-para_serv_url = 'UPDATE_LIST_LOOKUP_COUPA_SAP'.
                  ws_zmmt0171-id_param = ws_list-id.
                  condense ws_zmmt0171-id_param no-gaps.
                  zv_status = 'false'.

                  ws_zmmt0171-json_pram = '{"active":' && zv_status && '}'.

                  try.
                      zcl_int_ob_cons_form_coupa=>zif_integracao_outbound~get_instance(
                   )->execute_request(
                    exporting
                      i_info_request = ws_zmmt0171
                      importing
                        e_integracao = r_response ).
                      if r_response-ds_data_retorno is not initial..
                        <ls_zmmt0172>-log_proc = <ls_zmmt0172>-log_proc && '/ Adicionado na lista com sucesso'.
                      endif.

                    catch zcx_integracao.
                      <ls_zmmt0172>-log_proc = <ls_zmmt0172>-log_proc && '/ Erro ao adicioar na lista lookup'.
                    catch zcx_error.
                      <ls_zmmt0172>-log_proc = <ls_zmmt0172>-log_proc && '/ Erro ao adicioar na lista lookup'.
                  endtry.

                else.
                  <ls_zmmt0172>-log_proc = <ls_zmmt0172>-log_proc && '/ Erro ao localizar fornecedor na lista lookup'.
                endif.

              else.
                <ls_zmmt0172>-log_proc = <ls_zmmt0172>-log_proc && '/ não localizado fornecedor na lista lista lookup'.
              endif.

            catch zcx_integracao.
              <ls_zmmt0172>-log_proc = <ls_zmmt0172>-log_proc && '/ não localizado fornecedor na lista lista lookup'.
            catch zcx_error.
              <ls_zmmt0172>-log_proc = <ls_zmmt0172>-log_proc && '/ não localizado fornecedor na lista lista lookup'.
          endtry.
        endif.
      endif.

      clear: ws_adrct.
    endloop.
  endif.


*&---------------------------------------------------------------------*
*& Enviar o status de processamento do formulario aprovado.
*&---------------------------------------------------------------------*

  loop at it_zmmt0172 assigning field-symbol(<lw_zmmt0172>) where zvalida is not initial.

    clear: ws_zmmt0171-zcheck_api, r_response.
    ws_zmmt0171-id_formulario = <lw_zmmt0172>-zid_approvals.
    ws_zmmt0171-zcheck_api = 4. "API PUT Aproval
    ws_zmmt0171-para_serv_url = 'CONS_FORM_COUPA_SAP'.

    try .
        zcl_int_ob_cons_form_coupa=>zif_integracao_outbound~get_instance(
        )->execute_request(
         exporting
           i_info_request = ws_zmmt0171
           importing
             e_integracao = r_response ).

        if r_response is not initial.
          <lw_zmmt0172>-log_proc_status = 'Enviado com sucesso a confirmação alteração'.
        endif.
      catch zcx_integracao.
        <lw_zmmt0172>-log_proc_status = 'Erro na comunicação ao enviar a confirmação alteração.'.
      catch zcx_error.
        <lw_zmmt0172>-log_proc_status = 'Erro na comunicação ao enviar a confirmação alteração.'.
    endtry.
  endloop.



*&---------------------------------------------------------------------*
*& Enviar o status de processamento do formulario rejeitado.
*&---------------------------------------------------------------------*

  loop at it_zmmt0172 assigning field-symbol(<l_zmmt0172>) where zvalida is initial.

    clear: ws_zmmt0171-zcheck_api, r_response.
    ws_zmmt0171-id_formulario = <l_zmmt0172>-zid_approvals.
    ws_zmmt0171-zmsg_rej      = <l_zmmt0172>-log_proc.
    ws_zmmt0171-zcheck_api    = 5. "API PUT REJET
    ws_zmmt0171-para_serv_url = 'CONS_FORM_COUPA_SAP'.

    try .
        zcl_int_ob_cons_form_coupa=>zif_integracao_outbound~get_instance(
        )->execute_request(
         exporting
           i_info_request = ws_zmmt0171
           importing
             e_integracao = r_response ).

        if r_response is not initial.
          <l_zmmt0172>-log_proc_status = 'Enviado status de rejeição, não sendo possivél a alteração'.
        endif.
      catch zcx_integracao.
        <l_zmmt0172>-log_proc_status = 'Erro na comunicação ao enviar a confirmação alteração.'.
      catch zcx_error.
        <l_zmmt0172>-log_proc_status = 'Erro na comunicação ao enviar a confirmação alteração.'.
    endtry.
  endloop.




*&---------------------------------------------------------------------*
*& Salvando alterações tabelas.
*&---------------------------------------------------------------------*

*  SORT it_zmmt0172 BY zvalida.
*  DELETE it_zmmt0172 WHERE zvalida NE abap_true.

  if ( it_zmmt0172[] is not initial ).

    modify zmmt0172 from table it_zmmt0172.

    if ( it_adrct_update[] is not initial ).
      modify adrct from table it_adrct_update.
    endif.

    commit work and wait.
  endif.
