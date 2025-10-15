FUNCTION znfse_miro_automatica.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_GUID_NFE) TYPE  /TCSR/E_GUID_HEADER
*"     REFERENCE(I_DEPARTAMENTO) TYPE  ZDE_DEPARTAMENTO OPTIONAL
*"     REFERENCE(I_DATA_VENCIMENTO) TYPE  ZDE_DT_VENCIMENTO OPTIONAL
*"     REFERENCE(I_BANCO_PARCEIRO) TYPE  BVTYP OPTIONAL
*"     REFERENCE(I_ITENS_NFE) TYPE  ZDE_NFE_INFO_ITEM_T OPTIONAL
*"     REFERENCE(I_OBS_FINANCEIRA) TYPE  ZDE_OBS_FINANCEIRA_CTR
*"       OPTIONAL
*"     REFERENCE(I_BLOQUEIO_PAGAMENTO) TYPE  DZLSPR OPTIONAL
*"     REFERENCE(I_CK_SOMENTE_UMA_MIGO_PEDIDO) TYPE  CHAR01 DEFAULT 'X'
*"     REFERENCE(I_CK_ESTORNAR) TYPE  CHAR01 DEFAULT ' '
*"     REFERENCE(I_CPF) TYPE  STRING OPTIONAL
*"     REFERENCE(I_CK_REVISAR) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_DS_REVISAR_MOTIVO) TYPE  STRING OPTIONAL
*"     REFERENCE(I_ISSQN) TYPE  J_1BTAXVAL OPTIONAL
*"     REFERENCE(I_INSS) TYPE  J_1BTAXVAL OPTIONAL
*"     REFERENCE(I_PIS) TYPE  J_1BTAXVAL OPTIONAL
*"     REFERENCE(I_COF) TYPE  J_1BTAXVAL OPTIONAL
*"     REFERENCE(I_CSLL) TYPE  J_1BTAXVAL OPTIONAL
*"     REFERENCE(I_IRRF) TYPE  J_1BTAXVAL OPTIONAL
*"     REFERENCE(I_PISCOFCSLL) TYPE  J_1BTAXVAL OPTIONAL
*"     REFERENCE(I_BASE_ISSQN) TYPE  J_1BTAXVAL OPTIONAL
*"     REFERENCE(I_BASE_INSS) TYPE  J_1BTAXVAL OPTIONAL
*"     REFERENCE(I_BASE_IRRF) TYPE  J_1BTAXVAL OPTIONAL
*"     REFERENCE(I_CALC_IMP) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MESSAGEM_ERRO) TYPE  STRING
*"     REFERENCE(E_SUCESSO) TYPE  CHAR01
*"     REFERENCE(E_BELNR) TYPE  RE_BELNR
*"     REFERENCE(E_GJAHR) TYPE  GJAHR
*"     REFERENCE(E_VLRLIQ) TYPE  J_1BTAXVAL
*"----------------------------------------------------------------------

  DATA: nfe               TYPE REF TO zcl_nfse_inbound,
        e_caracteristicas	TYPE zib_nfe_dist_lca_t,
        cpf_limpo         TYPE string.

  e_sucesso = '0'.

  "Encontrar Usuário pelo CPF
  IF i_cpf IS NOT INITIAL.

    cpf_limpo = i_cpf.
    REPLACE ALL OCCURRENCES OF '.' IN cpf_limpo WITH '' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '-' IN cpf_limpo WITH '' IGNORING CASE.

    DATA(qtd_cpf) = strlen( cpf_limpo ).
    IF qtd_cpf GT 11.
      MESSAGE s127 WITH cpf_limpo INTO e_messagem_erro.
      EXIT.
    ENDIF.

    SELECT * INTO TABLE @DATA(it_adcp)
      FROM adcp
     WHERE fax_number EQ @cpf_limpo.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s128 WITH cpf_limpo INTO e_messagem_erro.
      EXIT.
    ENDIF.

    SELECT * INTO TABLE @DATA(it_usr21)
      FROM usr21
      FOR ALL ENTRIES IN @it_adcp
    WHERE persnumber EQ @it_adcp-persnumber.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s128 WITH cpf_limpo INTO e_messagem_erro.
      EXIT.
    ENDIF.

    DESCRIBE TABLE it_usr21 LINES DATA(qtd_usuarios).
    IF qtd_usuarios GT 1.
      MESSAGE s129 WITH cpf_limpo INTO e_messagem_erro.
      EXIT.
    ENDIF.

    READ TABLE it_usr21 INDEX 1 INTO DATA(wa_usr21).

  ENDIF.

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Revisão de Lançamento de SM no SE."""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF i_ck_revisar EQ abap_true.

    TRY .

        CREATE OBJECT nfe
          EXPORTING
            i_guid = i_guid_nfe.

        nfe->zif_cadastro~set_registro( i_id_registro = i_guid_nfe ).

        nfe->set_nfse_em_revisao(
          EXPORTING
            i_motivo_revisao      = i_ds_revisar_motivo
            i_usuario_solicitante = wa_usr21-bname
        ).

        e_sucesso = '1'.

      CATCH zcx_nfe_inbound_exception INTO DATA(ex_nfein).
        MESSAGE ID ex_nfein->msgid TYPE 'S' NUMBER ex_nfein->msgno WITH ex_nfein->msgv1 ex_nfein->msgv2 ex_nfein->msgv3 ex_nfein->msgv4 INTO e_messagem_erro.
    ENDTRY.

    IF nfe IS NOT INITIAL.
      nfe->free( ).
      CLEAR: nfe.
    ENDIF.

  ENDIF.

  CHECK i_ck_revisar NE abap_true.
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


  IF i_calc_imp EQ abap_true .

    SELECT SINGLE nfse_value FROM zibt_nfse_001
      INTO e_vlrliq
        WHERE guid_header = i_guid_nfe.
    e_vlrliq = e_vlrliq - ( i_issqn + i_inss + i_pis + i_cof + i_csll + i_irrf + i_piscofcsll ).
    CLEAR: e_belnr, e_gjahr.
  ENDIF.

  CHECK i_calc_imp NE abap_true.


  " 04.11.2024 - RAMON - 156941 -->
  DATA: job_name   TYPE c LENGTH 10,
        number     TYPE tbtcjob-jobcount,
        name       TYPE tbtcjob-jobname,
        t_rsparams TYPE rsparams_tt,
        w_rsparams TYPE rsparams,
        e_ret      TYPE zmme_saida_miro_automatica.

  job_name = i_guid_nfe.

  CONCATENATE 'JOB_MIRO_AUTOMATICA' job_name  INTO name SEPARATED BY '_'.

  t_rsparams = VALUE #( ( selname = 'P_BINSS' kind = 'P' sign = 'I' option = 'EQ' low = i_base_inss )
            ( selname = 'P_BIRRF' kind = 'P' sign = 'I' option = 'EQ' low = i_base_irrf )
            ( selname = 'P_BISSQN' kind = 'P' sign = 'I' option = 'EQ' low = i_base_issqn )
            ( selname = 'P_COF' kind = 'P' sign = 'I' option = 'EQ' low = i_cof )
            ( selname = 'P_CSLL' kind = 'P' sign = 'I' option = 'EQ' low = i_csll )
            ( selname = 'P_INSS' kind = 'P' sign = 'I' option = 'EQ' low = i_inss )
            ( selname = 'P_IRRF' kind = 'P' sign = 'I' option = 'EQ' low = i_irrf )
            ( selname = 'P_ISSQN' kind = 'P' sign = 'I' option = 'EQ' low = i_issqn )
            ( selname = 'P_NFE' kind = 'P' sign = 'I' option = 'EQ' low = i_guid_nfe )
            ( selname = 'P_PCCSLL' kind = 'P' sign = 'I' option = 'EQ' low = i_piscofcsll )
            ( selname = 'P_PIS' kind = 'P' sign = 'I' option = 'EQ' low = i_pis )
            ( selname = 'P_ESTOR' kind = 'P' sign = 'I' option = 'EQ' low = i_ck_estornar )
            ( selname = 'P_NAME' kind = 'P' sign = 'I' option = 'EQ' low = wa_usr21-bname )
            ( selname = 'P_VENC' kind = 'P' sign = 'I' option = 'EQ' low = i_data_vencimento ) ).

  "solicitar e Aguardar execução do job
  DATA(lv_string) = zcl_job=>insert_job_fila_escalonamento( EXPORTING i_nome_job      = name
                                                              i_report        = 'ZMMR207'
                                                              i_user_job      = sy-uname
                                                              i_rsparams_t    = t_rsparams
                                                              i_processar_retorno = abap_true
                                                              i_wait_schedule = abap_true
                                                              i_wait_finish   = abap_true ).

  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = lv_string
    CHANGING
      data = e_ret.

  e_messagem_erro = e_ret-e_messagem_erro.
  e_sucesso       = e_ret-e_sucesso.
  e_belnr         = e_ret-e_belnr.
  e_gjahr         = e_ret-e_gjahr.

  " 04.11.2024 - RAMON - 156941 --<

*  TRY .
*
*      CREATE OBJECT nfe
*        EXPORTING
*          i_guid = i_guid_nfe.
*
*      "Localizando a NF-e
*      nfe->zif_cadastro~set_registro( i_id_registro = i_guid_nfe ).
*
*      " 20.09.2022 - Set valor imposto -->
*
*      CALL METHOD nfe->set_impostos
*        EXPORTING
*          i_issqn           = i_issqn
*          i_inss            = i_inss
*          i_pis             = i_pis
*          i_cof             = i_cof
*          i_csll            = i_csll
*          i_irrf            = i_irrf
*          i_piscofcsll      = i_piscofcsll
*          i_base_issqn      = i_base_issqn
*          i_base_inss       = i_base_inss
*          i_base_irrf       = i_base_irrf
*          i_data_vencimento = i_data_vencimento.
*
*
*      CASE i_ck_estornar.
*
*        WHEN abap_true.
*
*          nfe->nfse_inbound_cancela_fatura( ).
*
*          e_sucesso = '1'.
*
*        WHEN abap_false.
*
*          "Aceite Fatura
*          "nfe->ck_ignora_data_se_vencimento = abap_true.
*          nfe->set_aceitar_faturar( EXPORTING i_us_miro = wa_usr21-bname ).
*
*          IF nfe->zif_cadastro~gravar_registro( ) EQ abap_true.
*            "DATA(nota) = nfe->get_info_nota( ).
*
*            e_belnr   = nfe->ms_nfse_001-belnr.
*
*            e_gjahr   = nfe->ms_nfse_001-gjahr.
*
*            IF e_belnr IS NOT INITIAL AND e_gjahr IS NOT INITIAL.
*
*              e_sucesso = '1'.
*
*            ELSE.
*
*              DATA(lw_ret) = nfe->get_last_message( ).
*
*              IF lw_ret IS NOT INITIAL.
*                e_messagem_erro = lw_ret-message.
*              ENDIF.
*
*            ENDIF.
*
*          ELSE.
*
*            lw_ret = nfe->get_last_message( ).
*
*            IF lw_ret IS NOT INITIAL.
*              e_messagem_erro = lw_ret-message.
*            ENDIF.
*
*          ENDIF.
*
*      ENDCASE.
*
*      IF nfe IS NOT INITIAL.
*        nfe->free( ).
*        CLEAR: nfe.
*      ENDIF.
*
*  ENDTRY.

ENDFUNCTION.
