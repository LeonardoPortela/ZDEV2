*&---------------------------------------------------------------------*
*& Report ZFI_R_NEW_DOC_CONTABIL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_r_new_doc_contabil.

DATA: wa_ibctb     TYPE zib_contabil,
      it_ibctb     TYPE TABLE OF zib_contabil,
      wa_ibctb_log TYPE zib_contabil_log,
      it_ibctb_log TYPE TABLE OF zib_contabil_log,
      wa_document  TYPE zfie_document,
      it_document  TYPE TABLE OF zfie_document.

DATA i_dados TYPE zde_new_doc_ctb.

PARAMETERS p_idfila TYPE zde_id_fila_job OBLIGATORY.

DATA e_documento TYPE bkpf.
DATA e_erros  TYPE zib_contabil_err_t.

START-OF-SELECTION.

  PERFORM f_processa.

  PERFORM f_grava.


*&---------------------------------------------------------------------*
*& Form F_processa
*&---------------------------------------------------------------------*
FORM f_processa.

  SELECT SINGLE dados_processar
    FROM zjob0003 INTO @DATA(lv_dados_processar)
    WHERE id_fila_job EQ @p_idfila.

  CHECK sy-subrc EQ 0.

  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = lv_dados_processar
    CHANGING
      data = i_dados.

  CLEAR: it_ibctb[], it_ibctb_log[], it_document[], e_erros, e_documento.

  SELECT SINGLE * INTO @e_documento
    FROM bkpf
   WHERE awkey EQ @i_dados-ch_referencia.

  IF sy-subrc EQ 0.
    e_erros = VALUE #( ( obj_key = i_dados-ch_referencia
                       "nr_item = wa_item-sq_item
                       interface = i_dados-id_interface
                       dt_atualizacao = sy-datum
                       hr_atualizacao = sy-uzeit
                       type = 'E'
                       id = zcx_integracao=>zcx_treinamento_cpf_obriga-msgid
                       num = zcx_integracao=>zcx_treinamento_cpf_obriga-msgno
                       message = space
                       message_v1 = 'CH Referencia:'
                       message_v2 = i_dados-ch_referencia
                       message_v3 = 'já existe'
                       message_v4 = space ) ).

    EXIT.

  ENDIF.

  DELETE FROM zib_contabil_chv WHERE obj_key EQ i_dados-ch_referencia AND obj_key NE space.
  DELETE FROM zib_contabil WHERE obj_key EQ i_dados-ch_referencia AND obj_key NE space.
  DELETE FROM zib_contabil_err WHERE obj_key EQ i_dados-ch_referencia AND obj_key NE space.

  LOOP AT i_dados-itens INTO DATA(wa_item).

    "Incluir Validações de Negócio """""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(lc_validado) = abap_true.
    "append value #( message = "erro aqui" ) to E_ERROS.

    IF wa_item-id_lms_treinamento IS NOT INITIAL.

      IF wa_item-ds_cpf_treinamento IS INITIAL.

        e_erros = VALUE #( ( obj_key = i_dados-ch_referencia
                           nr_item = wa_item-sq_item
                           interface = i_dados-id_interface
                           dt_atualizacao = sy-datum
                           hr_atualizacao = sy-uzeit
                           type = 'E'
                           id = zcx_integracao=>zcx_treinamento_cpf_obriga-msgid
                           num = zcx_integracao=>zcx_treinamento_cpf_obriga-msgno
                           message = space
                           message_v1 = space
                           message_v2 = space
                           message_v3 = space
                           message_v4 = space ) ).

        lc_validado = abap_false.

*        RAISE EXCEPTION TYPE zcx_integracao
*          EXPORTING
*           textid = VALUE #( msgid = zcx_integracao=>zcx_treinamento_cpf_obriga-msgid
*                              msgno = zcx_integracao=>zcx_treinamento_cpf_obriga-msgno )
*            msgid  = zcx_integracao=>zcx_treinamento_cpf_obriga-msgid
*            msgno  = zcx_integracao=>zcx_treinamento_cpf_obriga-msgno
*            msgty  = 'E'.
      ENDIF.

      FIND REGEX '[0-9]{11}' IN wa_item-ds_cpf_treinamento.
      IF sy-subrc IS NOT INITIAL.

        e_erros = VALUE #( ( obj_key = i_dados-ch_referencia
                           nr_item = wa_item-sq_item
                           interface = i_dados-id_interface
                           dt_atualizacao = sy-datum
                           hr_atualizacao = sy-uzeit
                           type = 'E'
                           id = zcx_integracao=>zcx_treinamento_cpf_obriga-msgid
                           num = zcx_integracao=>zcx_treinamento_cpf_obriga-msgno
                           message = space
                           message_v1 = space
                           message_v2 = space
                           message_v3 = space
                           message_v4 = space ) ).

        lc_validado = abap_false.

*        RAISE EXCEPTION TYPE zcx_integracao
*          EXPORTING
*            textid = VALUE #( msgid = zcx_integracao=>zcx_treinamento_cpf_padrao-msgid
*                              msgno = zcx_integracao=>zcx_treinamento_cpf_padrao-msgno )
*            msgid  = zcx_integracao=>zcx_treinamento_cpf_padrao-msgid
*            msgno  = zcx_integracao=>zcx_treinamento_cpf_padrao-msgno
*            msgty  = 'E'.
      ENDIF.

      SELECT SINGLE * INTO @DATA(wa_zmmt0103)
        FROM zmmt0103
       WHERE saknr EQ @wa_item-cn_cliente_fornecedor.

      IF sy-subrc IS NOT INITIAL.

        e_erros = VALUE #( ( obj_key = i_dados-ch_referencia
                   nr_item = wa_item-sq_item
                   interface = i_dados-id_interface
                   dt_atualizacao = sy-datum
                   hr_atualizacao = sy-uzeit
                   type = 'E'
                   id = zcx_integracao=>zcx_treinamento_cpf_obriga-msgid
                   num = zcx_integracao=>zcx_treinamento_cpf_obriga-msgno
                   message = space
                   message_v1 = CONV #( wa_item-cn_cliente_fornecedor )
                   message_v2 = space
                   message_v3 = space
                   message_v4 = space ) ).


        lc_validado = abap_false.

*        RAISE EXCEPTION TYPE zcx_integracao
*          EXPORTING
*            textid = VALUE #( msgid = zcx_integracao=>zcx_treinamento_conta_erro-msgid
*                              msgno = zcx_integracao=>zcx_treinamento_conta_erro-msgno
*                              attr1 = wa_item-cn_cliente_fornecedor )
*            msgid  = zcx_integracao=>zcx_treinamento_conta_erro-msgid
*            msgno  = zcx_integracao=>zcx_treinamento_conta_erro-msgno
*            msgv1  = CONV #( wa_item-cn_cliente_fornecedor )
*            msgty  = 'E'.
      ENDIF.

                                                            "ZMM0148
      SELECT SINGLE * INTO @DATA(wa_zmmt0104)
        FROM zmmt0104
       WHERE cpf    EQ @wa_item-ds_cpf_treinamento
         AND id_lms EQ @wa_item-id_lms_treinamento.

      IF sy-subrc IS NOT INITIAL.

        e_erros = VALUE #( ( obj_key = i_dados-ch_referencia
                   nr_item = wa_item-sq_item
                   interface = i_dados-id_interface
                   dt_atualizacao = sy-datum
                   hr_atualizacao = sy-uzeit
                   type = 'E'
                   id = zcx_integracao=>zcx_treinamento_cpf_obriga-msgid
                   num = zcx_integracao=>zcx_treinamento_cpf_obriga-msgno
                   message = space
                   message_v1 = CONV #( wa_item-id_lms_treinamento )
                   message_v2 = CONV #( wa_item-ds_cpf_treinamento )
                   message_v3 = space
                   message_v4 = space ) ).

        lc_validado = abap_false.

*        RAISE EXCEPTION TYPE zcx_integracao
*          EXPORTING
*            textid = VALUE #( msgid = zcx_integracao=>zcx_treinamento_curso_cpf-msgid
*                              msgno = zcx_integracao=>zcx_treinamento_curso_cpf-msgno
*                              attr1 = wa_item-id_lms_treinamento
*                              attr2 = wa_item-ds_cpf_treinamento )
*            msgid  = zcx_integracao=>zcx_treinamento_curso_cpf-msgid
*            msgno  = zcx_integracao=>zcx_treinamento_curso_cpf-msgno
*            msgv1  = CONV #( wa_item-id_lms_treinamento )
*            msgv2  = CONV #( wa_item-ds_cpf_treinamento )
*            msgty  = 'E'.
      ENDIF.

    ENDIF.


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF lc_validado EQ abap_false.
      RETURN.
    ENDIF.

    CLEAR: wa_ibctb.
    wa_ibctb-obj_key   = i_dados-ch_referencia.
    wa_ibctb-interface = i_dados-id_interface.
    wa_ibctb-bktxt     = i_dados-tx_cabecalho.
    wa_ibctb-bukrs     = i_dados-cd_empresa.
    wa_ibctb-bldat     = i_dados-dt_documento.
    wa_ibctb-budat     = i_dados-dt_lancamento.
    wa_ibctb-gjahr     = i_dados-dt_lancamento+6(4).
    wa_ibctb-monat     = i_dados-dt_lancamento+3(2).
    wa_ibctb-blart     = i_dados-tp_documento.
    wa_ibctb-xblnr     = i_dados-nu_doc_referencia.
    wa_ibctb-rg_atualizado = 'S'.

    "Dados do Item
    wa_ibctb-bschl     = wa_item-ch_lancamento.
    wa_ibctb-hkont     = wa_item-cn_cliente_fornecedor.
    wa_ibctb-wrbtr     = wa_item-vr_item.
    wa_ibctb-waers     = wa_item-cd_moeda.
    wa_ibctb-zfbdt     = wa_item-dt_vencimento.
    wa_ibctb-zlspr     = wa_item-ch_bloqueio.
    wa_ibctb-zlsch     = wa_item-fr_pagamento.
    wa_ibctb-kidno     = wa_item-rf_pagamento.
    wa_ibctb-sgtxt     = wa_item-tx_item.
    wa_ibctb-xref1     = wa_item-ch_referencia1.
    wa_ibctb-xref2     = wa_item-ch_referencia2.
    wa_ibctb-xref3     = wa_item-ch_referencia_item.
    wa_ibctb-bupla     = wa_item-cd_filial.
    wa_ibctb-zuonr     = wa_item-atribuicao.
    wa_ibctb-umskz     = wa_item-cd_razao_especial.
    wa_ibctb-kostl     = wa_item-cd_centro_custo.
    wa_ibctb-aufnr     = wa_item-nu_ordem.
    wa_ibctb-prctr     = wa_item-cd_centro_lucro.
    wa_ibctb-gsber     = wa_item-divisao.
    wa_ibctb-waers_i   = wa_item-cd_moeda_interna.
    wa_ibctb-dmbtr     = wa_item-vr_moeda_interna.
    wa_ibctb-waers_f   = wa_item-cd_moeda_forte.
    wa_ibctb-dmbe2     = wa_item-vr_moeda_forte.
    wa_ibctb-bvtyp     = wa_item-tp_banco_parceiro.
    wa_ibctb-hbkid     = wa_item-ch_banco_empresa.
    wa_ibctb-seqitem   = wa_item-sq_item.
    wa_ibctb-tax_code  = wa_item-cd_iva.
    wa_ibctb-matnr     = wa_item-id_material_sap.
    wa_ibctb-land1     = wa_item-pais.
    wa_ibctb-controle_vat = wa_item-controle_vat.
    wa_ibctb-wf_saldo_finan_a = wa_item-wf_saldo_finan_a.
    wa_ibctb-vl_bc_iva_wrbtr  = wa_item-vl_bc_iva_wrbtr.
    wa_ibctb-vl_bc_iva_dmbtr  = wa_item-vl_bc_iva_dmbtr.
    wa_ibctb-vl_bc_iva_dmbe2  = wa_item-vl_bc_iva_dmbe2.
    wa_ibctb-ds_cpf_treinamento = wa_item-ds_cpf_treinamento.
    wa_ibctb-id_lms_treinamento = wa_item-id_lms_treinamento.
    APPEND wa_ibctb TO it_ibctb.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Dados para LOG
    CLEAR: wa_ibctb_log, wa_document.
    MOVE-CORRESPONDING wa_ibctb TO wa_ibctb_log.
    MOVE-CORRESPONDING wa_ibctb TO wa_document.
    wa_ibctb_log-dtreg = sy-datum.
    wa_ibctb_log-hrreg = sy-uzeit.
    APPEND wa_ibctb_log TO it_ibctb_log.
    APPEND wa_document TO it_document.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_grava
*&---------------------------------------------------------------------*
FORM f_grava .

  DATA lv_dados_json TYPE string.
  DATA lv_ret_full TYPE string.
  DATA lv_erros_json TYPE string.

  IF e_erros[] IS INITIAL.

    DELETE FROM zib_contabil WHERE obj_key EQ i_dados-ch_referencia AND obj_key NE space.
    MODIFY zib_contabil FROM TABLE it_ibctb.
    MODIFY zib_contabil_log FROM TABLE it_ibctb_log.
    COMMIT WORK AND WAIT.

    "Processar dado """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_FI_MANYDOCUMENT_PROCESSA'
      TABLES
        it_document = it_document.

    COMMIT WORK AND WAIT.

    SELECT SINGLE * INTO @e_documento
      FROM bkpf
     WHERE awkey EQ @i_dados-ch_referencia.

    IF sy-subrc IS NOT INITIAL.

      SELECT * APPENDING TABLE @e_erros
        FROM zib_contabil_err
       WHERE obj_key EQ @i_dados-ch_referencia.

    ENDIF.

  ENDIF.

  IF e_documento IS NOT INITIAL AND p_idfila IS NOT INITIAL.
    lv_dados_json = /ui2/cl_json=>serialize( data = e_documento ).
  ENDIF.

  IF e_erros IS NOT INITIAL.

    PERFORM f_formata_erro.

    lv_erros_json = /ui2/cl_json=>serialize( data = e_erros ).


  ENDIF.

  lv_ret_full = lv_dados_json && '###' && lv_erros_json.

  UPDATE zjob0003 SET dados_processados = lv_ret_full
    WHERE id_fila_job = p_idfila.

  COMMIT WORK AND WAIT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_formata_erro
*&---------------------------------------------------------------------*
FORM f_formata_erro .

  LOOP AT e_erros ASSIGNING FIELD-SYMBOL(<fs_erro>) WHERE message is INITIAL.

    MESSAGE ID <fs_erro>-id TYPE <fs_erro>-type NUMBER <fs_erro>-num
      WITH <fs_erro>-message_v1 <fs_erro>-message_v2 <fs_erro>-message_v3
            <fs_erro>-message_v4 INTO <fs_erro>-message.

  ENDLOOP.

ENDFORM.
