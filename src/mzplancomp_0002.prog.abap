*----------------------------------------------------------------------*
***INCLUDE MZPLANCOMP_0002 .
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0020 INPUT.
  "PSA
*  DATA: vl_vbeln               TYPE bapivbeln-vbeln,
*        vg_verifica_selecao_pr TYPE sy-subrc,
*        tl_programacao         TYPE TABLE OF ty_zplac_nom_programacao WITH HEADER LINE,
*        wl_programacao         TYPE znom_programacao,
*        wa_header              TYPE bapisdhd1,
*        wa_item                TYPE bapisditm,
*        wa_schedules           TYPE bapischdl,
*        wa_partner             TYPE bapiparnr,
*        wa_conditions          TYPE bapicond,
*        t_return               TYPE TABLE OF bapiret2,
*        t_item                 TYPE TABLE OF bapisditm,
*        t_partner              TYPE TABLE OF bapiparnr,
*        t_schedules            TYPE TABLE OF bapischdl,
*        t_conditions           TYPE TABLE OF bapicond,
*        t_bapiparex            TYPE TABLE OF bapiparex.
*
*  DATA: vl_erro           TYPE i,
*        vl_sum_programada TYPE j_1bnetqty.
*
*  DATA: wl_header_in      TYPE bapisdhd1,
*        wl_header_inx     TYPE bapisdhd1x,
*        doc_venda         LIKE bapivbeln-vbeln,
*        tl_items_in       TYPE TABLE OF bapisditm  WITH HEADER LINE,
*        tl_items_inx      TYPE TABLE OF bapisditmx WITH HEADER LINE,
*        tl_return         TYPE TABLE OF bapiret2   WITH HEADER LINE,
*        tl_partners       TYPE TABLE OF bapiparnr  WITH HEADER LINE,
*        tl_conditions_in  TYPE TABLE OF bapicond   WITH HEADER LINE,
*        tl_conditions_inx TYPE TABLE OF bapicondx  WITH HEADER LINE,
*        f_headinx         LIKE bapisdh1x,
*        tl_return2        TYPE TABLE OF bapiret2   WITH HEADER LINE,
*        tl_return3        TYPE TABLE OF bapiret2   WITH HEADER LINE,
*        tl_text_in        TYPE TABLE OF bapisdtext WITH HEADER LINE,
*        tl_bapiparex      TYPE TABLE OF bapiparex  WITH HEADER LINE,
*        wl_bape_vbak      TYPE bape_vbak,
*        wl_bape_vbakx     TYPE bape_vbakx,
*        tl_contrato       TYPE TABLE OF bapictr    WITH HEADER LINE,
*        lv_msg            TYPE c LENGTH 255,
*        lv_ukurs          TYPE ukurs_curr,
*        lv_ukurs_         TYPE char9,
*        lv_data_atual     TYPE gdatu_inv,
*        wl_param          TYPE znom_param_cont.

  CASE ok_code_0001.
    WHEN ok_add_prog.
      CLEAR: ok_code_0001.
      CLEAR: znom_programacao, vg_total_alt.
      znom_programacao-id_unidade = 'KG'.
      CALL SCREEN 0021 STARTING AT 07 05 ENDING AT 75 14.
      PERFORM consulta_programacoes.
    WHEN ok_edi_prog OR ok_exc_prog.
*      IF .
      PERFORM verifica_selecao_programa USING vg_verifica_selecao_pr.
      IF vg_verifica_selecao_pr IS INITIAL.
        IF ok_code_0001 EQ ok_exc_prog.

          DATA(_bloq_exclusao) = abap_false.

          IF ( wa_znom_programacao_alv-nr_qtde_remetente IS NOT INITIAL ) OR
             ( wa_znom_programacao_alv-nr_qtde_plan_fili IS NOT INITIAL ) OR
             ( wa_znom_programacao_alv-nr_efetivada      IS NOT INITIAL ).
            _bloq_exclusao = abap_true.
          ENDIF.

          IF wa_znom_programacao-contrato IS NOT INITIAL.
            DATA(_count_prog_contrato) = 0.
            LOOP AT it_znom_programacao_alv INTO DATA(_prog_alv_aux) WHERE id_nomeacao_tran = wa_znom_programacao-id_nomeacao_tran
                                                                       AND id_empresa       = wa_znom_programacao-id_empresa
                                                                       AND contrato         = wa_znom_programacao-contrato.
              ADD 1 TO _count_prog_contrato.
            ENDLOOP.
            IF _count_prog_contrato < 2.
              _bloq_exclusao = abap_true.
            ENDIF.
          ENDIF.

          IF _bloq_exclusao EQ abap_false.
            CLEAR: ok_code_0001.
            PERFORM apagar_programacao USING wa_znom_programacao.
          ELSE.
            MESSAGE 'A programação selecionada possui um contrato gerado, não é possível excluir.' TYPE 'E'.
          ENDIF.
        ELSE.
          CLEAR: ok_code_0001.
          MOVE-CORRESPONDING wa_znom_programacao TO znom_programacao.
          vg_total_alt = znom_programacao-nr_programada.
          CALL SCREEN 0021 STARTING AT 07 05 ENDING AT 75 14.
        ENDIF.
        PERFORM consulta_programacoes.
      ENDIF.
      CLEAR: ok_code_0001.
*      ENDIF.
    WHEN ok_ger_mail OR ok_ger_mail_e.
      PERFORM verifica_selecao_programa USING vg_verifica_selecao_pr.
      IF vg_verifica_selecao_pr IS INITIAL.
        IF ok_code_0001 NE ok_ger_mail_e.
          CALL FUNCTION 'ZPLANCOMP_EMAIL_PROGRAMACAO'
            EXPORTING
              p_id_nomeacao_tran = wa_znom_programacao-id_nomeacao_tran
              p_znom_programacao = wa_znom_programacao
            EXCEPTIONS
              not_qualified      = 1
              user_not_found     = 2
              address_not_found  = 3
              OTHERS             = 4.
        ELSE.
          CALL FUNCTION 'ZPLANCOMP_EMAIL_PROGRAMACAO'
            EXPORTING
              p_id_nomeacao_tran = wa_znom_programacao-id_nomeacao_tran
              p_znom_programacao = wa_znom_programacao
              p_executado        = c_x
            EXCEPTIONS
              not_qualified      = 1
              user_not_found     = 2
              address_not_found  = 3
              OTHERS             = 4.
        ENDIF.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDIF.
      CLEAR: ok_code_0001.
    WHEN ok_ger_cont.
      tl_programacao[] = it_znom_programacao_alv[].

      LOOP AT tl_programacao INTO wa_znom_programacao_alv .
        IF wa_znom_programacao_alv-chk IS NOT INITIAL.
          IF wa_znom_programacao_alv-contrato IS NOT INITIAL.
            ADD 1 TO vl_erro.
          ENDIF.

          IF wl_programacao IS INITIAL.
            MOVE-CORRESPONDING wa_znom_programacao_alv TO wl_programacao.
          ELSE.
            IF wa_znom_programacao_alv-id_empresa <> wl_programacao-id_empresa
            OR wa_znom_programacao_alv-id_material <> wl_programacao-id_material.
              ADD 1 TO vl_erro.
            ENDIF.
          ENDIF.

          vl_sum_programada = vl_sum_programada + wa_znom_programacao_alv-nr_programada.
        ENDIF.
      ENDLOOP.

**      Buscar parâmetros de contrato
      SELECT SINGLE *
        FROM znom_param_cont
        INTO wl_param
        WHERE bukrs = wl_programacao-id_empresa.

      IF sy-subrc IS INITIAL.
**      Dados do cabeçalho do contrato
        CLEAR wl_header_in.
        wl_header_in-doc_type   = wl_param-auart.
        wl_header_in-distr_chan = wl_param-vtweg.
        wl_header_in-sales_org  = wl_param-bukrs.
        wl_header_in-division   = wl_param-spart.
        wl_header_in-purch_date = sy-datum.
        wl_header_in-incoterms1 = wl_param-inco1.
        wl_header_in-incoterms2 = wl_param-inco2.
        wl_header_in-pmnttrms   = wl_param-zterm.
        wl_header_in-purch_no_c = wa_znom_transporte_alv-ds_nome_transpor.
        wl_header_in-ct_valid_f = sy-datum.
        wl_header_in-ct_valid_t = sy-datum + 10.
        wl_header_in-currency   = wl_param-waerk.

**      Busca valor do dólar para o dia
        CLEAR: obj_zcl_util_sd.
        CREATE OBJECT obj_zcl_util_sd.

        lv_data_atual = sy-datum.

        obj_zcl_util_sd->set_data(  EXPORTING i_data  = lv_data_atual ). "TCURR-GDATU

*        OBJ_ZCL_UTIL_SD->SET_KURST( EXPORTING I_KURST = 'B' ).

        obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'G' ).    " TCURR-KURST
        obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'USD' ).  " TCURR-FCURR
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'BRL' ).  " TCURR-TCURR
        obj_zcl_util_sd->taxa_cambio(  RECEIVING e_ukurs = lv_ukurs ).
**      Valor taxa de câmbio

        wl_header_in-exrate_fi = lv_ukurs.
        wl_header_in-exchg_rate = lv_ukurs.

        FREE tl_conditions_in.
        tl_conditions_in-conexchrat = lv_ukurs.
        APPEND tl_conditions_in.

        FREE obj_zcl_util_sd.

**      Dados dos parceiros
        REFRESH tl_partners.
        tl_partners-partn_role = 'AG' .
        tl_partners-partn_numb = wl_param-kunnr.
        APPEND tl_partners.

        tl_partners-partn_role = 'RE' .
        tl_partners-partn_numb = wl_param-kunnr.
        APPEND tl_partners.

        tl_partners-partn_role = 'RG' .
        tl_partners-partn_numb = wl_param-kunnr.
        APPEND tl_partners.

        tl_partners-partn_role = 'WE' .
        tl_partners-partn_numb = wl_param-kunnr.
        APPEND tl_partners.

**      Item do contrato
        REFRESH tl_items_in.
        tl_items_in-material    = wl_programacao-id_material.
        tl_items_in-target_qty  = vl_sum_programada.
        tl_items_in-target_qu   = wl_programacao-id_unidade.
        tl_items_in-usage_ind   = wl_param-vkaus.
        APPEND tl_items_in.
        tl_items_inx-sales_unit = 'X'.
        APPEND  tl_items_inx.

**      Extension - Campo ZPESAGEM
        CLEAR tl_bapiparex.
        tl_bapiparex-structure  = 'BAPE_VBAK'.
        wl_bape_vbak-zpesagem   = wl_param-zpesagem.
        tl_bapiparex-valuepart1 = wl_bape_vbak.
        APPEND tl_bapiparex.

        CLEAR tl_bapiparex.
        tl_bapiparex-structure  = 'BAPE_VBAKX'.
        wl_bape_vbakx-zpesagem  = 'X'.
        tl_bapiparex-valuepart1 = wl_bape_vbakx.
        APPEND  tl_bapiparex.

        IF wl_programacao IS NOT INITIAL.
          IF vl_erro IS INITIAL.
            CALL FUNCTION 'BAPI_CONTRACT_CREATEFROMDATA'
              EXPORTING
                contract_header_in     = wl_header_in
                convert                = 'X'
              IMPORTING
                salesdocument          = doc_venda
              TABLES
                return                 = tl_return
                contract_items_in      = tl_items_in
                contract_items_inx     = tl_items_inx
                contract_partners      = tl_partners
                contract_conditions_in = tl_conditions_in
*               contract_data_in       = tl_contrato
                extensionin            = tl_bapiparex.

            IF doc_venda IS NOT INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

              CLEAR lv_msg.
              CONCATENATE 'Contrato criado:' doc_venda INTO lv_msg SEPARATED BY space.
              MESSAGE lv_msg TYPE 'I'.

              LOOP AT tl_programacao INTO wa_znom_programacao_alv WHERE chk IS NOT INITIAL.
                UPDATE znom_programacao SET contrato = doc_venda WHERE id_cliente       = wa_znom_programacao_alv-id_cliente  AND
                                                                       id_material      = wa_znom_programacao_alv-id_material AND
                                                                       id_filial        = wa_znom_programacao_alv-id_filial   AND
                                                                       id_empresa       = wa_znom_programacao_alv-id_empresa  AND
                                                                       id_nomeacao_tran = wa_znom_programacao_alv-id_nomeacao_tran.
              ENDLOOP.
              PERFORM troca_aba_02 USING space.

            ELSE.
              lv_msg = 'O contrato não foi gerado, '.
              READ TABLE tl_return INDEX  1.
              CONCATENATE lv_msg tl_return-message INTO lv_msg SEPARATED BY space.
              MESSAGE lv_msg TYPE 'I'.
            ENDIF.
          ELSE.
            MESSAGE 'A seleção contem empresas ou materiais diferentes!' TYPE 'I'.
          ENDIF.
        ELSE.
          MESSAGE 'Selecione um registro.' TYPE 'I'.
        ENDIF.
      ELSE.
        MESSAGE 'Parâmetro não configurado na transação ZMEMO12. Não é possível gerar contrato.' TYPE 'I'.
      ENDIF.
      CLEAR: ok_code_0001, wl_programacao, vl_sum_programada, vl_erro.

      CALL METHOD plan_alv_programa->refresh_table_display.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0020  INPUT

MODULE user_command_0060 INPUT.

  DATA: vg_verifica_selecao_da TYPE sy-subrc,
        var_answer             TYPE c.

  DATA: wa_registro_due TYPE zde_registro_due.

  CASE ok_code_0001.
    WHEN ok_add_due.
      CLEAR: ok_code_0001.

      FREE zcl_due.
      CREATE OBJECT zcl_due.

      CLEAR: wa_registro_due.
      wa_registro_due-modo                           = c_due_novo.  "Novo
      wa_registro_due-id_nomeacao_tran               = wa_znom_transporte-id_nomeacao_tran.
      wa_registro_due-forma_exportacao               = '1001'.      "Forma Exportação/Por conta própria
      wa_registro_due-situacao_especial              = '2002'.      "Situação Especial/Embarque antecipado
      wa_registro_due-moeda_cambio                   = 'USD'.       "Moeda Negociação
      wa_registro_due-tp_due                         = '1'.         "Sem NF-e
      wa_registro_due-tp_cod_local_despacho          = '281'.       "Em Recinto Alfandegado
      wa_registro_due-fatura_tp_codigo               = '388'.       "Nota Fiscal
      wa_registro_due-codigo_cond_venda              = 'FOB'.       "Código Condição de Venda
      wa_registro_due-codigo_enquadramento           = '80000'.     "Código Enquadramento
      wa_registro_due-fatura_motivo_dispensa_nf      = '3004'.      "NF/Embarque antecipado

      zcl_due->registro_due( EXPORTING i_registro_due = wa_registro_due ).

      PERFORM consulta_due_antecipada.

    WHEN ok_moni_due.

      PERFORM verifica_sel_due_antecipada USING vg_verifica_selecao_da.

      IF vg_verifica_selecao_da EQ 0.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = 'Deseja listar somente DU-e selecionada?'
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF var_answer EQ '1'.
          SUBMIT zsdr0095 WITH p_bukrs EQ wa_due_antecipada-bukrs
                          WITH p_id    EQ wa_due_antecipada-id_due
                      AND RETURN.
        ELSE.
**  Begin of CS2020000625 #36596 FF  24.01.2023 / Equa. PSA
* =====================================================================Comentado 13/06/2024 / aoenning.
*          RANGES p_idref FOR zsdt0170-id_due_ref.
*          p_idref-low = ' '.
*          p_idref-sign = 'I'.
*          p_idref-option = 'NE'.
*          APPEND p_idref.
* * End of FF  24.01.2023
* =====================================================================Comentado 13/06/2024 / aoenning.
          SUBMIT zsdr0095 WITH p_bukrs EQ wa_due_antecipada-bukrs
                          WITH p_idnom EQ wa_due_antecipada-id_nomeacao_tran
*                          WITH p_idref IN p_idref "CS2020000625 #36596 FF  24.01.2023 * =====================================================================Comentado 13/06/2024 / aoenning.
                      AND RETURN.
        ENDIF.

        PERFORM consulta_due_antecipada.
      ENDIF.

      CLEAR: ok_code_0001.

    WHEN ok_view_due.
      PERFORM verifica_sel_due_antecipada USING vg_verifica_selecao_da.
      IF vg_verifica_selecao_da EQ 0.
        FREE zcl_due.
        CREATE OBJECT zcl_due.

        CLEAR: wa_registro_due.
        wa_registro_due-id_due = wa_due_antecipada-id_due.
        wa_registro_due-modo   = c_due_view.

        zcl_due->registro_due( wa_registro_due ).

        PERFORM consulta_due_antecipada.
      ENDIF.
      CLEAR: ok_code_0001.
    WHEN ok_edit_mat.
      DATA : t_filedcat  TYPE lvc_t_fcat,
             wa_filedcat TYPE lvc_s_fcat.
      PERFORM consulta_nomeacoes.
      READ TABLE it_znom_transporte_alv TRANSPORTING NO FIELDS WITH KEY ds_status = 'Com Programação'.

      IF sy-subrc NE 0.
        CALL METHOD plan_alv_due_antecipada->get_frontend_fieldcatalog
          IMPORTING
            et_fieldcatalog = t_filedcat[].

        LOOP AT t_filedcat INTO wa_filedcat .
          IF wa_filedcat-fieldname = 'MATNR' .
            wa_filedcat-edit = 'X' .
            MODIFY t_filedcat FROM wa_filedcat .
          ENDIF .
        ENDLOOP .

        CALL METHOD plan_alv_due_antecipada->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_filedcat[].
      ELSE.
        MESSAGE 'DU-e vinculada à programação do navio, o material não pode ser alterado!!' TYPE 'I'.
      ENDIF.
    WHEN ok_bloq_desbl_due.
      PERFORM verifica_sel_due_antecipada USING vg_verifica_selecao_da.
      IF vg_verifica_selecao_da EQ 0.
        FREE zcl_due.
        CREATE OBJECT zcl_due
          EXPORTING
            i_id_due = wa_due_antecipada-id_due.

        TRY.
            zcl_due->bloq_desbloq( ).
          CATCH zcx_due INTO ex_due.
            ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        ENDTRY.

        PERFORM consulta_due_antecipada.
      ENDIF.
      CLEAR: ok_code_0001.
    WHEN  ok_troca_due.

      PERFORM verifica_sel_due_antecipada USING vg_verifica_selecao_da.
      IF vg_verifica_selecao_da EQ 0.

        CLEAR: wg_id_due_troca.

        CALL SCREEN 0063 STARTING AT 07 05 ENDING AT 38 04.

        PERFORM consulta_due_antecipada.
      ENDIF.
      CLEAR: ok_code_0001.

    WHEN ok_edit_regio.

      PERFORM verifica_sel_due_antecipada USING vg_verifica_selecao_da.
      IF vg_verifica_selecao_da EQ 0.

        CLEAR: wg_regio_due.
        wg_regio_due-land1 = 'BR'.

        CALL SCREEN 0061 STARTING AT 07 05 ENDING AT 18 05.

        PERFORM consulta_due_antecipada.
      ENDIF.
      CLEAR: ok_code_0001.

*"// WBARBOSA 25102024 - US-153330
    WHEN ok_edit_eudr.

      PERFORM verifica_sel_due_antecipada USING vg_verifica_selecao_da.
      IF vg_verifica_selecao_da IS INITIAL.

        CLEAR: wg_reclassificacao_eudr.

        CALL SCREEN 0064 STARTING AT 07 05 ENDING AT 45 05.

        PERFORM consulta_due_antecipada.
      ENDIF.
      CLEAR: ok_code_0001.
*"// WBARBOSA 25102024 - US-153330

    WHEN ok_edit_tp_ex.

      PERFORM verifica_sel_due_antecipada USING vg_verifica_selecao_da.
      IF vg_verifica_selecao_da EQ 0.

        CLEAR: wg_tp_exp_due.

        CALL SCREEN 0062 STARTING AT 07 05 ENDING AT 45 05.

        PERFORM consulta_due_antecipada.
      ENDIF.
      CLEAR: ok_code_0001.
    WHEN ok_edit_due.

      PERFORM verifica_sel_due_antecipada USING vg_verifica_selecao_da.
      IF vg_verifica_selecao_da EQ 0.
        FREE zcl_due.
        CREATE OBJECT zcl_due.

        CLEAR: wa_registro_due.
        wa_registro_due-id_due = wa_due_antecipada-id_due.
        wa_registro_due-modo   = c_due_change.

        zcl_due->registro_due( wa_registro_due ).

        PERFORM consulta_due_antecipada.
      ENDIF.
      CLEAR: ok_code_0001.
  ENDCASE.

ENDMODULE.


MODULE user_command_0070 INPUT.

  DATA: vg_verifica_selecao_dr TYPE sy-subrc.

  CONSTANTS: lc_cancel_ret_due TYPE c LENGTH 50 VALUE 'CANCEL_DUE'." Rubenilson - 11.09.24 - 150675

  CASE ok_code_0001.
    WHEN ok_ret_due.

      PERFORM retificar_due USING abap_false. "Lançar

    WHEN ok_moni_due.

      PERFORM verifica_sel_due_retificacao USING vg_verifica_selecao_dr.

      IF vg_verifica_selecao_dr EQ 0.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = 'Deseja listar somente DU-e selecionada?'
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF var_answer EQ '1'.
          SUBMIT zsdr0095 WITH p_bukrs EQ wa_due_retificacao_alv-bukrs
                          WITH p_id    EQ wa_due_retificacao_alv-id_due_ret
                      AND RETURN.
        ELSE.
**  Begin of CS2020000625 #36596 FF  24.01.2023 / Equa. PSA
* =====================================================================Comentado 13/06/2024 / aoenning.
*          p_idref-low = ' '.
*          p_idref-sign = 'I'.
*          p_idref-option = 'NE'.
*          APPEND p_idref.
* * End of FF  24.01.2023
* =====================================================================Comentado 13/06/2024 / aoenning.
          SUBMIT zsdr0095 WITH p_bukrs EQ wa_due_retificacao_alv-bukrs
                          WITH p_idnom EQ wa_due_retificacao_alv-id_nomeacao_tran
*                          WITH p_idref IN p_idref "CS2020000625 #36596 FF  24.01.2023 / Equa. PSA * =====================================================================Comentado 13/06/2024 / aoenning.
                      AND RETURN.
        ENDIF.

        PERFORM consulta_due_retificacao.
      ENDIF.

      CLEAR: ok_code_0001.

    WHEN ok_view_due.
      PERFORM verifica_sel_due_retificacao USING vg_verifica_selecao_dr.
      IF vg_verifica_selecao_dr EQ 0.
        FREE zcl_due.
        CREATE OBJECT zcl_due.

        CLEAR: wa_registro_due.
        wa_registro_due-id_due = wa_due_retificacao_alv-id_due_ret.
        wa_registro_due-modo   = c_due_view.

        zcl_due->registro_due( wa_registro_due ).

        PERFORM consulta_due_retificacao.
      ENDIF.
      CLEAR: ok_code_0001.
    WHEN ok_edit_due.

      PERFORM verifica_sel_due_retificacao USING vg_verifica_selecao_dr.
      IF vg_verifica_selecao_dr EQ 0.
        FREE zcl_due.
        CREATE OBJECT zcl_due.

        CLEAR: wa_registro_due.
        wa_registro_due-id_due = wa_due_retificacao_alv-id_due_ret.
        wa_registro_due-modo   = c_due_change.

        zcl_due->registro_due( wa_registro_due ).

        PERFORM consulta_due_retificacao.
      ENDIF.
      CLEAR: ok_code_0001.

    WHEN ok_lanc_ret_due.

      PERFORM retificar_due USING abap_true. "Lançar

    WHEN ok_del_due.

      PERFORM verifica_sel_due_retificacao USING vg_verifica_selecao_dr.
      IF vg_verifica_selecao_dr EQ 0.

        FREE zcl_due.
        CREATE OBJECT zcl_due
          EXPORTING
            i_id_due = wa_due_retificacao_alv-id_due_ret.

        TRY.
            zcl_due->eliminar_registro( ).
          CATCH zcx_due INTO ex_due.
            ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        ENDTRY.

        PERFORM consulta_due_retificacao.
        PERFORM delete_drawback. "/ Equa. PSA
      ENDIF.
      CLEAR: ok_code_0001.

*** Inicio - Rubenilson - 11.09.24 - 150675
    WHEN lc_cancel_ret_due.

      PERFORM verifica_sel_due_retificacao USING vg_verifica_selecao_dr.

      SELECT SINGLE *
        FROM zsdt0170
        INTO @DATA(ls_zsdt0170)
        WHERE id_due     = @wa_due_retificacao_alv-id_due_ret
          AND numero_due = @wa_due_retificacao_alv-numero_due.
      IF sy-subrc IS INITIAL.
        CLEAR ls_zsdt0170-id_due_ref.

        ls_zsdt0170-us_canc = sy-uname.
        ls_zsdt0170-dt_canc = sy-datum.
        ls_zsdt0170-hr_canc = sy-uzeit.

        MODIFY zsdt0170 FROM ls_zsdt0170.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
          MESSAGE 'Retificação DU-e cancelada com sucesso!' TYPE 'S'.
          PERFORM consulta_due_retificacao.
        ENDIF.
      ENDIF.
*** Fim - Rubenilson - 11.09.24 - 150675

  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_PROGRAMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verifica_selecao_programa  USING    vg_verifica_selecao TYPE sy-subrc.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR: wa_znom_programacao_alv.

  CALL METHOD plan_alv_programa->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_znom_programacao_alv INTO wa_znom_programacao_alv INDEX wa_selected_rows-index.
    READ TABLE it_znom_programacao     INTO wa_znom_programacao
    WITH KEY id_nomeacao_tran = wa_znom_programacao_alv-id_nomeacao_tran
             id_empresa       = wa_znom_programacao_alv-id_empresa
             id_filial        = wa_znom_programacao_alv-id_filial
             id_material      = wa_znom_programacao_alv-id_material
             id_cliente       = wa_znom_programacao_alv-id_cliente.

  ENDLOOP.

  IF NOT wa_znom_programacao_alv IS INITIAL.
    IF ( wa_znom_programacao_alv-id_cliente IS INITIAL )
    OR ( ok_code_0001 EQ ok_exc_prog )
    OR ( ok_code_0001 EQ ok_edi_prog ) .
      vg_verifica_selecao = 0.
    ELSE.
      vg_verifica_selecao = 1.
    ENDIF.
  ELSE.
    vg_verifica_selecao = 1.
  ENDIF.

ENDFORM.                    " VERIFICA_SELECAO_PROGRAMA

FORM verifica_sel_due_antecipada USING vg_verifica_selecao TYPE sy-subrc.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR: wa_due_antecipada_alv, wa_due_antecipada.

  CALL METHOD plan_alv_due_antecipada->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  IF lines( it_selected_rows[] ) NE 1.
    vg_verifica_selecao = 1.
    MESSAGE 'Selecione um registro!' TYPE 'I'.
    EXIT.
  ENDIF.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_due_antecipada_alv INTO wa_due_antecipada_alv INDEX wa_selected_rows-index.
    CHECK sy-subrc = 0.
    READ TABLE it_due_antecipada     INTO wa_due_antecipada
      WITH KEY id_nomeacao_tran = wa_due_antecipada_alv-id_nomeacao_tran
               id_due           = wa_due_antecipada_alv-id_due.

  ENDLOOP.

  IF wa_due_antecipada IS NOT INITIAL.
    IF ( ok_code_0001  EQ ok_edit_due       ) OR
       ( ok_code_0001  EQ ok_view_due       ) OR
       ( ok_code_0001  EQ ok_edit_regio     ) OR
       ( ok_code_0001  EQ ok_edit_tp_ex     ) OR
       ( ok_code_0001  EQ ok_edit_eudr      ) OR  "// wbarbosa 25/10/2024 - US-153330
       ( ok_code_0001  EQ ok_bloq_desbl_due ) OR
       ( ok_code_0001  EQ ok_troca_due      ) OR
       ( ok_code_0001  EQ ok_moni_due       ).
      vg_verifica_selecao = 0.
    ELSE.
      vg_verifica_selecao = 1.
    ENDIF.
  ELSE.
    vg_verifica_selecao = 1.
  ENDIF.

ENDFORM.                    " VERIFICA_SELECAO_PROGRAMA


FORM verifica_sel_due_retificacao USING vg_verifica_selecao TYPE sy-subrc.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR: wa_due_retificacao_alv, wa_due_retificacao.

  CALL METHOD plan_alv_due_retificacao->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  IF lines( it_selected_rows[] ) NE 1.
    vg_verifica_selecao = 1.
    MESSAGE 'Selecione um registro!' TYPE 'I'.
    EXIT.
  ENDIF.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_due_retificacao_alv INTO wa_due_retificacao_alv INDEX wa_selected_rows-index.
    CHECK sy-subrc = 0.
    READ TABLE it_due_retificacao INTO wa_due_retificacao
      WITH KEY id_nomeacao_tran = wa_due_retificacao_alv-id_nomeacao_tran
               id_due           = wa_due_retificacao_alv-id_due.

  ENDLOOP.

  IF wa_due_retificacao IS NOT INITIAL.
    CASE ok_code_0001.
      WHEN ok_ret_due OR ok_lanc_ret_due.
        vg_verifica_selecao = 0.

        IF wa_due_retificacao_alv-id_due_ret IS NOT INITIAL.
          vg_verifica_selecao = 1.
          MESSAGE 'DU-e de Retificação já foi gerada!' TYPE 'I'.
        ENDIF.
      WHEN ok_edit_due OR
           ok_view_due OR
           ok_del_due OR
           ok_moni_due.
        vg_verifica_selecao = 0.

        IF wa_due_retificacao_alv-id_due_ret IS INITIAL.
          vg_verifica_selecao = 1.
          MESSAGE 'DU-e de Retificação ainda não foi gerada!' TYPE 'I'.
        ENDIF.

      WHEN OTHERS.
        vg_verifica_selecao = 1.
    ENDCASE.

  ELSE.
    vg_verifica_selecao = 1.
  ENDIF.

ENDFORM.                    " VERIFICA_SELECAO_PROGRAMA



*&---------------------------------------------------------------------*
*&      Form  APAGAR_PROGRAMACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM apagar_programacao  USING  p_programacao TYPE znom_programacao.

  DATA: answer   TYPE c LENGTH 1,
        lv_ordem TYPE znom_remetente-nr_ordem.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel     = 'Atenção!'
      textline1 = 'Toda a Programação será removida!'
      textline2 = 'Deseja realmente exluir?'
    IMPORTING
      answer    = answer.

  CASE answer.
    WHEN c_j.

      CLEAR: it_znom_prog_reme[].

      SELECT SINGLE nr_ordem
        INTO lv_ordem
        FROM znom_remetente
       WHERE id_nomeacao_tran = p_programacao-id_nomeacao_tran
         AND id_empresa       = p_programacao-id_empresa
         AND id_filial        = p_programacao-id_filial
         AND id_material      = p_programacao-id_material
         AND docnum_rt        = ''
         AND nr_ordem         = ''.

      IF sy-subrc IS NOT INITIAL.
        SELECT * INTO TABLE it_znom_prog_reme
          FROM znom_prog_reme
         WHERE id_nomeacao_tran EQ p_programacao-id_nomeacao_tran
           AND id_empresa  EQ p_programacao-id_empresa
           AND id_filial   EQ p_programacao-id_filial
           AND id_material EQ p_programacao-id_material.

        IF it_znom_prog_reme[] IS INITIAL.
          DELETE FROM znom_programacao WHERE id_nomeacao_tran EQ p_programacao-id_nomeacao_tran
             AND id_empresa  EQ p_programacao-id_empresa
             AND id_filial   EQ p_programacao-id_filial
             AND id_material EQ p_programacao-id_material.
          DELETE FROM znom_remetente WHERE id_nomeacao_tran EQ p_programacao-id_nomeacao_tran
             AND id_empresa  EQ p_programacao-id_empresa
             AND id_filial   EQ p_programacao-id_filial
             AND id_material EQ p_programacao-id_material.
          DELETE FROM znom_reme_notas WHERE id_nomeacao_tran EQ p_programacao-id_nomeacao_tran
             AND id_empresa  EQ p_programacao-id_empresa
             AND id_filial   EQ p_programacao-id_filial
             AND id_material EQ p_programacao-id_material.
          COMMIT WORK.
        ELSE.
          MESSAGE s011.
        ENDIF.
      ELSE.
        MESSAGE s011.
      ENDIF.
  ENDCASE.

ENDFORM.                    " APAGAR_PROGRAMACAO


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0021_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0021_exit INPUT.
  CLEAR ok_code_0021.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0021_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0021  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0021 OUTPUT.

  SET PF-STATUS 'PF0021'.
  SET TITLEBAR 'TL0021'.

  DATA: wa_t001       TYPE t001,
        wa_j_1bbranch TYPE j_1bbranch,
        wa_makt       TYPE makt.

  CLEAR: vg_txt_empresa, vg_txt_filial, vg_txt_material, vg_txt_cliente.

  IF NOT znom_programacao-id_empresa IS INITIAL.

    SELECT SINGLE * INTO wa_t001
      FROM t001
     WHERE bukrs EQ znom_programacao-id_empresa.

    IF sy-subrc IS INITIAL.
      vg_txt_empresa = wa_t001-butxt.
    ENDIF.

    IF NOT znom_programacao-id_filial IS INITIAL.
      SELECT SINGLE * INTO wa_j_1bbranch
        FROM j_1bbranch
       WHERE bukrs  EQ znom_programacao-id_empresa
         AND branch EQ znom_programacao-id_filial.
      IF sy-subrc IS INITIAL.
        vg_txt_filial = wa_j_1bbranch-name.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT znom_programacao-id_material IS INITIAL.
    SELECT SINGLE * INTO wa_makt
      FROM makt
     WHERE spras EQ sy-langu
       AND matnr EQ znom_programacao-id_material.
    IF sy-subrc IS INITIAL.
      vg_txt_material = wa_makt-maktx.
    ENDIF.
  ENDIF.

  IF NOT znom_programacao-id_cliente IS INITIAL.
    SELECT SINGLE name1 INTO vg_txt_cliente
      FROM kna1
     WHERE kunnr EQ znom_programacao-id_cliente.
  ENDIF.


ENDMODULE.                 " STATUS_0021  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  VISIBILIDADE_CAMPOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE visibilidade_campos OUTPUT.

  LOOP AT SCREEN.
    IF NOT znom_programacao-id_nomeacao_tran IS INITIAL.
      IF ( screen-group1 EQ 'INS' ).
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " VISIBILIDADE_CAMPOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0021  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0021 INPUT.

  DATA: vg_total               LIKE znom_programacao-nr_programada,
        vg_sobra               LIKE znom_programacao-nr_programada,
        vg_verif               TYPE sy-subrc,
        it_znom_remetente_aux2 TYPE TABLE OF znom_remetente WITH HEADER LINE,
        vg_nr_programada       TYPE j_1bnetqty.

  IF ok_code_0021 EQ ok_salvar.

    PERFORM verifica_planeja_perf_fob_fob USING znom_programacao vg_verif.
    CHECK vg_verif IS INITIAL.

    PERFORM verifica_unicidade USING znom_programacao vg_verif.
    CHECK vg_verif IS INITIAL.

    PERFORM verifica_planeja_filial USING znom_programacao-id_filial vg_verif.

    IF vg_verif IS INITIAL.

      IF znom_programacao-nr_programada LE 0.
        MESSAGE s002.
        EXIT.
      ENDIF.

      SELECT SUM( nr_programada ) INTO vg_total
        FROM znom_programacao
       WHERE id_nomeacao_tran = wa_znom_transporte-id_nomeacao_tran.

      vg_sobra = wa_znom_transporte-nr_qtde_nomeada - vg_total - ( znom_programacao-nr_programada - vg_total_alt ).

      "Comentado. Inicio USER STORY 64117 - Anderson Oenning - 12/05/2022
*      IF vg_sobra LT 0.
*        MESSAGE s003 WITH vg_sobra.
*        EXIT.
*      ENDIF.

      "Fim USER STORY 64117 - Anderson Oenning - 12/05/2022


      znom_programacao-id_nomeacao_tran = wa_znom_transporte-id_nomeacao_tran.

      SELECT *
        INTO TABLE it_znom_remetente_aux2
        FROM znom_remetente
       WHERE id_nomeacao_tran EQ znom_programacao-id_nomeacao_tran
         AND id_empresa       EQ znom_programacao-id_empresa
         AND id_filial        EQ znom_programacao-id_filial
         AND id_material      EQ znom_programacao-id_material.

      IF sy-subrc IS INITIAL.
        vg_nr_programada = 0.
        LOOP AT it_znom_remetente_aux2.
          vg_nr_programada = vg_nr_programada + it_znom_remetente_aux2-nr_programada.
        ENDLOOP.
        IF znom_programacao-nr_programada LT vg_nr_programada.
          MESSAGE s025.
          EXIT.
        ENDIF.
      ENDIF.

      MODIFY znom_programacao.
      LEAVE TO SCREEN 0.

    ELSE.
      MESSAGE s021 WITH znom_programacao-id_filial.
    ENDIF.

    CLEAR ok_code_0021.

  ENDIF.

ENDMODULE.                 " USER_COMMAND_0021  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_FILIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_filial INPUT.

  DATA: vg_verif_filial TYPE sy-subrc.

  PERFORM verifica_planeja_filial USING znom_programacao-id_filial vg_verif_filial.

ENDMODULE.                 " CHECK_FILIAL  INPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_PLANEJA_FILIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verifica_planeja_filial  USING  p_filial    TYPE werks_d
                                     p_vg_filial TYPE sy-subrc.

  p_vg_filial = 4.

  AUTHORITY-CHECK OBJECT 'ZPLA_EXPOR' ID 'ZPLA_EXPOR' FIELD p_filial.
  IF sy-subrc IS INITIAL.
    p_vg_filial = sy-subrc.
  ELSE.
    p_vg_filial = 4.
  ENDIF.

ENDFORM.                    " VERIFICA_PLANEJA_FILIAL

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_PLANEJA_PERF_FOB_FOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verifica_planeja_perf_fob_fob  USING    p_programacao TYPE znom_programacao
                                             p_verif       TYPE sy-subrc.
  p_verif = 4.

  IF ( p_programacao-id_filial IS INITIAL ) AND ( p_programacao-id_cliente IS INITIAL ).
    MESSAGE s027.
  ELSE.
    p_verif = 0.
  ENDIF.

ENDFORM.                    " VERIFICA_PLANEJA_PERF_FOB_FOB

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_UNICIDADE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verifica_unicidade  USING   p_programacao TYPE znom_programacao
                                 p_verif       TYPE sy-subrc.

  DATA: programacao TYPE znom_programacao.

  p_verif = 1.

  IF znom_programacao-id_nomeacao_tran IS INITIAL.
    SELECT SINGLE * INTO programacao
      FROM znom_programacao
     WHERE id_nomeacao_tran EQ wa_znom_transporte-id_nomeacao_tran
       AND id_empresa  EQ p_programacao-id_empresa
       AND id_filial   EQ p_programacao-id_filial
       AND id_material EQ p_programacao-id_material
       AND id_cliente  EQ p_programacao-id_cliente .

    IF NOT sy-subrc IS INITIAL.
      p_verif = 0.
    ENDIF.
  ELSE.
    p_verif = 0.
  ENDIF.

ENDFORM.                    " VERIFICA_UNICIDADE "/ Equa. PSA
*&---------------------------------------------------------------------*
*&      Form  DELETE_DRAWBACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_drawback .

  SELECT COUNT(*) INTO @DATA(lv_count) FROM zsdt0305
    WHERE id_due = @wa_due_retificacao_alv-id_due_ret.

  IF sy-subrc IS INITIAL.

    DELETE FROM  zsdt0305
    WHERE id_due =  wa_due_retificacao_alv-id_due_ret.
  ENDIF.

ENDFORM.
