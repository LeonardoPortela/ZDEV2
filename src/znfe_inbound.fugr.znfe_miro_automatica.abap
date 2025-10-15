function znfe_miro_automatica.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CHAVE_NFE) TYPE  ZDE_CHAVE_DOC_E
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
*"  EXPORTING
*"     REFERENCE(E_MESSAGEM_ERRO) TYPE  STRING
*"     REFERENCE(E_SUCESSO) TYPE  CHAR01
*"     REFERENCE(E_BELNR) TYPE  RE_BELNR
*"     REFERENCE(E_GJAHR) TYPE  GJAHR
*"----------------------------------------------------------------------


  data: nfe               type ref to zcl_nfe_inbound,
        e_caracteristicas	type zib_nfe_dist_lca_t,
        cpf_limpo         type string,
        t_return          like bapiret2 occurs 0 with header line.



  e_sucesso = '0'.

  "Encontrar Usuário pelo CPF
  if i_cpf is not initial.

    cpf_limpo = i_cpf.
    replace all occurrences of '.' in cpf_limpo with '' ignoring case.
    replace all occurrences of '-' in cpf_limpo with '' ignoring case.

    data(qtd_cpf) = strlen( cpf_limpo ).
    if qtd_cpf gt 11.
      message s127 with cpf_limpo into e_messagem_erro.
      exit.
    endif.

    select * into table @data(it_adcp)
      from adcp
     where fax_number eq @cpf_limpo.

    if sy-subrc is not initial.
      message s128 with cpf_limpo into e_messagem_erro.
      exit.
    endif.

    select * into table @data(it_usr21)
      from usr21
      for all entries in @it_adcp
    where persnumber eq @it_adcp-persnumber.

    if sy-subrc is not initial.
      message s128 with cpf_limpo into e_messagem_erro.
      exit.
    endif.

    describe table it_usr21 lines data(qtd_usuarios).
    if qtd_usuarios gt 1.
      message s129 with cpf_limpo into e_messagem_erro.
      exit.
    endif.

    read table it_usr21 index 1 into data(wa_usr21).

  endif.

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Revisão de Lançamento de SM no SE."""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  if i_ck_revisar eq abap_true.
    try .
        create object nfe.
        "Localizando a NF-e
        nfe->zif_cadastro~set_registro( i_id_registro = i_chave_nfe ).
        nfe->ck_ignora_data_se_vencimento = abap_false.
        nfe->set_nfe_em_revisao(
          exporting
            i_motivo_revisao      = i_ds_revisar_motivo
            i_usuario_solicitante = wa_usr21-bname
        ).

        e_sucesso = '1'.

      catch zcx_nfe_inbound_exception into data(ex_nfein).
        message id ex_nfein->msgid type 'S' number ex_nfein->msgno with ex_nfein->msgv1 ex_nfein->msgv2 ex_nfein->msgv3 ex_nfein->msgv4 into e_messagem_erro.
    endtry.
    if nfe is not initial.
      nfe->free( ).
      clear: nfe.
    endif.
  endif.
  check i_ck_revisar ne abap_true.
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  "cria JOB
  perform gera_miro_job using i_chave_nfe i_ck_estornar i_cpf.

  "desbloqueia tabela de retorno se JOB bloqueou
  call function 'ZDENQUEUE_NFE_INBOUND'
    exporting
      chave = i_chave_nfe.

  "atualiza retorno function
  select single *
  from zib_nfe_dist_ter
  into @data(wzib_nfe_dist_ter)
  where chave_nfe = @i_chave_nfe.
  if sy-subrc = 0.
    e_messagem_erro = wzib_nfe_dist_ter-mensagem_erro.
    e_sucesso       = wzib_nfe_dist_ter-sucesso.
    e_belnr         = wzib_nfe_dist_ter-belnr.
    e_gjahr         = wzib_nfe_dist_ter-gjahr.
  endif.
  "cria JOB
endfunction.

form gera_miro_job using i_chave_nfe i_ck_estornar i_cpf.

  data: number     type tbtcjob-jobcount,
        name       type tbtcjob-jobname,
        t_rsparams type rsparams_tt,
        w_rsparams type rsparams.


  w_rsparams-selname = 'PCHAVE'.
  w_rsparams-kind    = 'P'.
  w_rsparams-sign    = 'I'.
  w_rsparams-option  = 'EQ'.
  w_rsparams-low     = i_chave_nfe.
  append w_rsparams to t_rsparams.
  clear w_rsparams.
  w_rsparams-selname = 'PESTORNO'.
  w_rsparams-kind    = 'P'.
  w_rsparams-sign    = 'I'.
  w_rsparams-option  = 'EQ'.
  w_rsparams-low     = i_ck_estornar.
  append w_rsparams to t_rsparams.
  clear w_rsparams.
  w_rsparams-selname = 'PCPF'.
  w_rsparams-kind    = 'P'.
  w_rsparams-sign    = 'I'.
  w_rsparams-option  = 'EQ'.
  w_rsparams-low     = i_cpf.
  append w_rsparams to t_rsparams.

  concatenate 'JOB_MIRO' i_chave_nfe into name separated by '_'.

  "solicitar e Aguardar execução do job
   zcl_job=>insert_job_fila_escalonamento( exporting
                                            i_nome_job      = name
                                            i_report        = 'ZMMR206'
                                            i_user_job      = sy-uname
                                            i_rsparams_t    = t_rsparams
                                            i_wait_schedule = abap_true
                                            i_wait_finish   = abap_true
                                           ).

endform.
