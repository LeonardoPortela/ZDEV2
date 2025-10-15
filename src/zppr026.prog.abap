*&---------------------------------------------------------------------*
*& Report  ZPPR026
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zppr026.

tables: zpps_ximfbf_log.

***********************************************************************
* Constantes
***********************************************************************

constants: cc_prefix_header_ep     type c length 50 value 'CHV_ROM-',
           cc_a                    value 'A',
           cc_c                    value 'C',
           cc_e                    value 'E',
           cc_i                    value 'I',
           cc_s                    value 'S',
           cc_p                    value 'P',
           cc_n                    value 'N',
           cc_m                    value 'M',
           cc_x                    value 'X',
           cc_y                    value 'Y',
           cc_w                    value 'W',
           cc_16                   type i          value 16,
           cc_eq(2)                type c          value 'EQ',
           cc_ge(2)                type c          value 'GE',
           cc_classe_mensagem      type arbgb      value 'Z01',
           cc_tpexec_01(2)         type c          value '01',
           cc_tpexec_02(2)         type c          value '02',
           cc_mfbf                 type sy-tcode   value 'MFBF',
           cc_pdc_number           type sa_bdenr   value '15',
           cc_deposito_pr01        type alort      value 'PR01',
           cc_deposito_in01        type alort      value 'IN01',
           cc_bwart_131            type bwart      value '131',
           cc_bwart_261            type bwart      value '261',
           cc_bwart_262            type bwart      value '262',
           cc_tipo_conf_01         type bckfltype  value '01',
           cc_tipo_conf_11         type bckfltype  value '11',
           cc_z_fi_outbound_return type rs38l_fnam value 'Z_FI_OUTBOUND_RETURN'.

***********************************************************************
* Tabelas Internas e WorkAreas -
***********************************************************************

data: git_zpps_ximfbf_log type table of zpps_ximfbf_log,
      git_zsdt0001        type table of zsdt0001,
      "USER STORY 165353 / AOENNING --->>> Ini
      "git_matkl_alg_caroco TYPE TABLE OF tvarvc,
      git_matkl           type table of tvarvc,
      "USER STORY 165353 / AOENNING <<<--- Fim
      git_bdc             type table of bdcdata,
      git_msg             type table of bdcmsgcoll,
      git_log_return_mfpf type table of zfie_ret_document with header line initial size 0.

***********************************************************************
* Variaveis Globais
***********************************************************************

data: gva_msg_aux type string.

***********************************************************************
* Start of Selection
***********************************************************************

start-of-selection.

  try.
      zcl_job=>get_ck_program_execucao( exporting i_nome_program = sy-cprog importing e_qtd = data(e_qtd) ).
    catch zcx_job.
  endtry.

  if e_qtd gt 1.
    leave program.
  endif.

  perform: f_seleciona_dados,
           f_processa_dados,
           f_envia_log_proc_xi.

form f_seleciona_dados.

  data: lit_ch_ref_romaneio type table of zsdt0001.

  data: lva_obj_key type zpps_ximfbf_log-obj_key.

  clear: git_zpps_ximfbf_log[].

  perform f_check_reinicia_processamento.

  if lva_obj_key is not initial. "For Debug

    select *
      from zpps_ximfbf_log into table git_zpps_ximfbf_log
     where obj_key = lva_obj_key
       and id_interface  eq 'S' order by primary key.

  else.

    select *
      from zpps_ximfbf_log into table git_zpps_ximfbf_log
     where zrg_atulizado eq 'N'
       and id_interface  eq 'S'
       and processado    ne 'P' order by primary key ."Não esteja em processamento

    perform f_block_reg_for_process.  "Realiza Lock nos registros para processamento

    data(_git_zpps_ximfbf_log_lock) = git_zpps_ximfbf_log[].  "Guarda registros que conseguiram realizar o lock
    clear: git_zpps_ximfbf_log[].

    "Recarregar registros que efeturam bloqueio
    if _git_zpps_ximfbf_log_lock[] is not initial.
      select *
        from zpps_ximfbf_log into table git_zpps_ximfbf_log
         for all entries in _git_zpps_ximfbf_log_lock
       where obj_key       eq _git_zpps_ximfbf_log_lock-obj_key
         and nrobol        eq _git_zpps_ximfbf_log_lock-nrobol
         and fgorigem      eq _git_zpps_ximfbf_log_lock-fgorigem
         and werks         eq _git_zpps_ximfbf_log_lock-werks
         and matnr         eq _git_zpps_ximfbf_log_lock-matnr
         and cd_ccusto     eq _git_zpps_ximfbf_log_lock-cd_ccusto
         and charg         eq _git_zpps_ximfbf_log_lock-charg.
    endif.

  endif.

  loop at git_zpps_ximfbf_log into data(lwa_mfbl_log).

    check strlen( lwa_mfbl_log-obj_key ) ge 11.

    append value #( ch_referencia = lwa_mfbl_log-obj_key(11) ) to lit_ch_ref_romaneio.

  endloop.

  if lit_ch_ref_romaneio[] is not initial.
    select *
      from zsdt0001 into table git_zsdt0001
      for all entries in lit_ch_ref_romaneio
     where ch_referencia = lit_ch_ref_romaneio-ch_referencia.
  endif.

  "USER STORY 165353 / AOENNING --->>> Ini
*  select *
*    from tvarvc into table git_matkl_alg_caroco
*   where name = 'MAGGI_GR_ALGODAO_CAROCO'.

  select *
    from tvarvc into table git_matkl
   where name = 'MAGGI_GR_LOTE_CENTRO'.

  "USER STORY 165353 / AOENNING <<<--- Fim



endform.                    "seleciona_dados


form f_processa_dados .

  loop at git_zpps_ximfbf_log into data(lwa_mfbf_log).

    data(_error) = abap_false.

    case lwa_mfbf_log-zst_atlz.
      when cc_i or "Entrada Produção
           cc_x.   "Entrada Produção - Origem Transf.
        perform f_bapi_entrada_producao using lwa_mfbf_log
                                     changing _error.
      when cc_e or cc_y or  "Estorno
           cc_y.            "Estorno - Origem Transf.
        perform f_bapi_estorno_producao using lwa_mfbf_log
                                     changing _error.
      when others.
    endcase.

    if _error eq abap_true.
      perform f_set_processado using lwa_mfbf_log-obj_key.
    endif.

  endloop.


endform.


form f_bapi_entrada_producao using p_zpps_ximfbf_log type zpps_ximfbf_log
                          changing c_error.

  data: lwa_bflushflags   type bapi_rm_flg,
        lwa_bflushdatagen type bapi_rm_datgen,
        lwa_bapiret       type bapiret2.

  data: lwa_ordem_prod    type ckmlmv013,
        lva_matnr_entrada type matnr,
        "USER STORY 165353 / AOENNING --->>> Ini
        "lva_algodao       type c,
        lva_grupo_merc    type c, "'MAGGI_GR_ALGODAO_CAROCO'.
        "USER STORY 165353 / AOENNING <<<--- Fim
        lva_chv_romaneio  type zsdt0001-ch_referencia,
        lva_confirmation  type prtnr.

  data: lit_mkpf_exists  type table of mkpf,
        lva_bktxt        type mkpf-bktxt,
        lva_nro_romaneio type zsdt0001-nr_romaneio.

  data: lva_is_block         type char01.

  "USER STORY 165353 / AOENNING --->>> Ini
  "clear: c_error, lva_nro_romaneio, lva_matnr_entrada, lva_algodao.
  clear: c_error, lva_nro_romaneio, lva_matnr_entrada, lva_grupo_merc.
  "USER STORY 165353 / AOENNING <<<--- Fim


*-----------------------------------------------------------------------------------------------------------*
* Validações
*-----------------------------------------------------------------------------------------------------------*
  call function 'CONVERSION_EXIT_MATN1_INPUT'
    exporting
      input  = p_zpps_ximfbf_log-matnr
    importing
      output = lva_matnr_entrada.

  select single *
    from mara into @data(lwa_mara)
   where matnr eq @lva_matnr_entrada.

  if sy-subrc eq 0 and lva_matnr_entrada is not initial.
    "USER STORY 165353 / AOENNING --->>> Ini
*    read table git_matkl_alg_caroco with key low = lwa_mara-matkl transporting no fields.
*    if sy-subrc eq 0.
*      lva_algodao = abap_true.
*    endif.
    read table git_matkl with key low = lwa_mara-matkl transporting no fields.
    if sy-subrc eq 0.
      lva_grupo_merc = abap_true.
    endif.
    "USER STORY 165353 / AOENNING <<<--- Fim
  endif.

  lva_chv_romaneio = p_zpps_ximfbf_log-obj_key(11).

  read table git_zsdt0001 into data(lwa_zsdt0001) with key ch_referencia = p_zpps_ximfbf_log-obj_key(11).
  if sy-subrc ne 0 and p_zpps_ximfbf_log-zst_atlz = cc_i.
    c_error = abap_true.
    gva_msg_aux = |Não encontrado o romaneio entrada para a Chv.Referencia: { lva_chv_romaneio } |.
    perform f_add_mensagem_retorno_xi using p_zpps_ximfbf_log gva_msg_aux 'E'.
    return.
  endif.

  case p_zpps_ximfbf_log-zst_atlz.
    when cc_i.

      "MM - Ajuste Deposito EUDR Ent. Estoque US #154729 - WPP -->>>
      if p_zpps_ximfbf_log-cd_safra is initial.
        try.
            zcl_deposito=>zif_deposito~get_instance(
                )->get_deposito_material_filial(
                    exporting
                      i_matnr      = lwa_zsdt0001-matnr
                      i_tp_produto = conv #( cond string( when lwa_zsdt0001-tp_transgenia(1) eq 'C' then zif_carga=>st_tp_transgeniase_co else 'RR' ) )    " Tipo de Produto
                      i_bukrs      = lwa_zsdt0001-bukrs
                      i_branch     = lwa_zsdt0001-branch
                      i_eudr       = lwa_zsdt0001-eudr
                    importing
                      e_lgort      = p_zpps_ximfbf_log-cd_safra  ).

            loop at git_zpps_ximfbf_log assigning field-symbol(<fs_zpps_ximfbf_log_tmp>) where obj_key = p_zpps_ximfbf_log-obj_key.
              <fs_zpps_ximfbf_log_tmp>-cd_safra = p_zpps_ximfbf_log-cd_safra.
            endloop.

          catch zcx_deposito into data(zcx_deposito). " Classe de Erro de Depósito
            c_error = abap_true.
            gva_msg_aux = zcx_deposito->zif_error~get_msg_erro( ).
            perform f_add_mensagem_retorno_xi using p_zpps_ximfbf_log gva_msg_aux 'E'.
            return.
        endtry.
      endif.
      "MM - Ajuste Deposito EUDR Ent. Estoque US #154729 - WPP <----

      lva_nro_romaneio = lwa_zsdt0001-nr_romaneio.
    when cc_x.
      lva_nro_romaneio = p_zpps_ximfbf_log-nr_romaneio.
  endcase.

  if p_zpps_ximfbf_log-zst_atlz = cc_i.
    if lwa_zsdt0001-nr_safra ne p_zpps_ximfbf_log-charg.
      c_error = abap_true.
      gva_msg_aux = |Safra informada na entrada { p_zpps_ximfbf_log-charg } diferente da safra do romaneio { lwa_zsdt0001-nr_safra } |.
      perform f_add_mensagem_retorno_xi using p_zpps_ximfbf_log gva_msg_aux 'E'.
      return.
    endif.
  endif.

  perform f_valida_dados_ordem_prod using p_zpps_ximfbf_log
                                          cc_x
                                 changing c_error
                                          lwa_ordem_prod.
  check c_error is initial.

  lva_bktxt = cc_prefix_header_ep && lva_chv_romaneio.

  select a~mblnr a~mjahr
    from mkpf as a into corresponding fields of table lit_mkpf_exists
   where bktxt = lva_bktxt
     and not exists (  select mblnr
                         from mseg as b
                        where b~smbln = a~mblnr
                          and b~sjahr = a~mjahr )
    and not exists ( select mblnr
                       from mseg as b
                      where b~mblnr = a~mblnr
                        and b~smbln ne space ).

  loop at lit_mkpf_exists into data(lwa_mkpf_exists).

    c_error = abap_true.

    gva_msg_aux = |Documento Produção: { lwa_mkpf_exists-mblnr } / { lwa_mkpf_exists-mjahr } , já gerado para o romaneio entrada Chv.Referencia: { lva_chv_romaneio } |.

    perform f_add_mensagem_retorno_xi using p_zpps_ximfbf_log
                                            gva_msg_aux
                                           'E'.
    return.

  endloop.


*-----------------------------------------------------------------------------------------------------------*
* Preenche Header da BAPI
*-----------------------------------------------------------------------------------------------------------*

  clear: lwa_bflushflags, lwa_bflushdatagen, lva_confirmation, lwa_bapiret.

  lwa_bflushflags-bckfltype      = cc_tipo_conf_01.
  lwa_bflushdatagen-storageloc   = p_zpps_ximfbf_log-cd_safra.
  lwa_bflushdatagen-backflquant  = p_zpps_ximfbf_log-qteprod.
  lwa_bflushdatagen-pdc_number   = cc_pdc_number.
  lwa_bflushdatagen-postdate     = p_zpps_ximfbf_log-dtmvto.
  lwa_bflushdatagen-docdate      = sy-datum.

  case p_zpps_ximfbf_log-zst_atlz.
    when cc_i. "Entrada Produção
      "USER STORY 165353 / AOENNING --->>> Ini
      if lva_grupo_merc = abap_true.
      "if lva_algodao = abap_true.
      "USER STORY 165353 / AOENNING <<<--- Fim
        lwa_bflushdatagen-batch        = p_zpps_ximfbf_log-charg && '_' && p_zpps_ximfbf_log-werks.
      else.
        lwa_bflushdatagen-batch        = p_zpps_ximfbf_log-charg.
      endif.
    when cc_x. "Entrada Produção Originada Transf.
      lwa_bflushdatagen-batch        = p_zpps_ximfbf_log-charg.
  endcase.

  lwa_bflushdatagen-materialnr   = lwa_ordem_prod-pmatn.
  lwa_bflushdatagen-prodplant    = lwa_ordem_prod-prwrk.
  lwa_bflushdatagen-prodversion  = lwa_ordem_prod-verid.
  lwa_bflushdatagen-docheadertxt = cc_prefix_header_ep && lva_chv_romaneio.

  update zpps_ximfbf_log set charg = lwa_bflushdatagen-batch
   where obj_key     = p_zpps_ximfbf_log-obj_key.

  commit work.

  if 1 = 2."Apenas para teste
    perform f_entrada_producao_shdb using lwa_bflushdatagen
                                          p_zpps_ximfbf_log
                                          lva_nro_romaneio
                                 changing c_error.

    exit.
  endif.


  do 10 times.

    data(_index) = sy-index.

    clear: lwa_bapiret, lva_is_block.

    " Setando parametro para não retornar pela interface de ordem de servico pelo metodo:IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE
    set parameter id 'ZINBOUNDGEO' field 'X'.

    call function 'BAPI_REPMANCONF1_CREATE_MTS'
      exporting
        bflushflags   = lwa_bflushflags
        bflushdatagen = lwa_bflushdatagen
      importing
        confirmation  = lva_confirmation
        return        = lwa_bapiret.

    set parameter id 'ZINBOUNDGEO' field space.

    if lva_confirmation is initial.

      call function 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
        exporting
          id       = lwa_bapiret-id
          number   = lwa_bapiret-number
        importing
          is_block = lva_is_block.

      if lva_is_block is not initial and _index le 10.
        wait up to 2 seconds.
      else.
        call function 'BAPI_TRANSACTION_ROLLBACK'.

        if lva_is_block is not initial. "Se nao conseguiu criar movimentação por bloqueio de registro, nao marcar registro como processado e tentar reprocessaar novamente
          perform f_reinicia_processamento using p_zpps_ximfbf_log-obj_key.
        else.

          c_error = abap_true.

          perform f_set_processado using p_zpps_ximfbf_log-obj_key.

          perform f_add_mensagem_retorno_xi_r using p_zpps_ximfbf_log
                                                    lwa_bapiret.
        endif.

        exit.
      endif.

    else.

      "Sucesso na execução da BAPI entrada/consumo
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.

      perform f_after_entrada_producao using p_zpps_ximfbf_log
                                             lva_confirmation
                                             lwa_bflushdatagen-postdate(4)
                                             lva_nro_romaneio.
      exit.

    endif.

  enddo.


endform.

form f_reinicia_processamento using p_obj_key type zpps_ximfbf_log-obj_key.

  update zpps_ximfbf_log set processado    = 'N'
                             zrg_atulizado = 'N'
                     where obj_key       = p_obj_key.

endform.


form f_bapi_estorno_producao using p_zpps_ximfbf_log type zpps_ximfbf_log
                          changing c_error.

  data: lva_confirmation type bapi_rm_datkey-confirmation,
        lva_cancconfirm  type canc_prtnr,
        lwa_blpp         type blpp,
        lit_mkpf_estorno type table of mkpf,
        lva_mblnr        type mkpf-mblnr,
        lva_budat        type mkpf-budat,
        lwa_return       type bapiret2.

  data: lwa_ordem_prod type ckmlmv013,
        lva_is_block   type char01.


  clear: c_error.

*-----------------------------------------------------------------------------------------------------------*
* Validações
*-----------------------------------------------------------------------------------------------------------*

  perform f_valida_dados_ordem_prod using p_zpps_ximfbf_log
                                          space
                                 changing c_error
                                          lwa_ordem_prod.
  check c_error is initial.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = p_zpps_ximfbf_log-mblnr
    importing
      output = lva_mblnr.

  if lva_mblnr is initial.
    c_error = abap_true.
    gva_msg_aux = |Documento material para estorno não informado!|.
    perform f_add_mensagem_retorno_xi using p_zpps_ximfbf_log
                                            gva_msg_aux
                                            'E'.
    return.
  endif.

  clear: lit_mkpf_estorno[].
  select *
    from mkpf into table lit_mkpf_estorno
   where mblnr eq lva_mblnr.

  sort lit_mkpf_estorno by mjahr descending.
  read table lit_mkpf_estorno into data(lwa_mkpf_estorno) index 1.
  lva_budat = lwa_mkpf_estorno-budat.

* Obtem a confirmação para o documento de material
  select single t1~prtnr
    into lva_confirmation
    from blpk as t1
   inner join blpp as t2
      on t2~prtnr = t1~prtnr
     and t2~belnr = lva_mblnr
  where t1~matnr = lwa_ordem_prod-pmatn
    and t1~werks = lwa_ordem_prod-prwrk
    and t1~verid = lwa_ordem_prod-verid
    and t1~budat = lva_budat.

  if sy-subrc ne 0 or lva_confirmation is initial.
    c_error = abap_true.
    gva_msg_aux = |Documento de Confirmação para Documento material { lva_mblnr } não encontrada! |.
    perform f_add_mensagem_retorno_xi using p_zpps_ximfbf_log
                                            gva_msg_aux
                                            'E'.
    return.
  endif.

*-----------------------------------------------------------------------------------------------------------*
* Chamar BAPI
*-----------------------------------------------------------------------------------------------------------*

  do 10 times.

    data(_index) = sy-index.

    clear: lwa_return, lva_is_block.

    call function 'BAPI_REPMANCONF1_CANCEL'
      exporting
        confirmation     = lva_confirmation
      importing
        cancconfirmation = lva_cancconfirm
        return           = lwa_return.

    if lva_cancconfirm is not initial. "Sucesso

      commit work and wait.

      perform f_get_item_confirmacao using lva_cancconfirm changing lwa_blpp.

      gva_msg_aux = text-m04.
      replace first occurrence of '&1' in gva_msg_aux with lva_confirmation.
      replace first occurrence of '&2' in gva_msg_aux with lva_cancconfirm.
      replace first occurrence of '&3' in gva_msg_aux with lwa_blpp-belnr.

      perform f_add_mensagem_retorno_xi_f  using p_zpps_ximfbf_log
                                                 gva_msg_aux
                                                 'S'
                                                 lva_confirmation
                                                 lva_cancconfirm
                                                 lwa_blpp-belnr
                                                 ''.

      perform f_set_processado using p_zpps_ximfbf_log-obj_key.

      exit.

    else.

      call function 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
        exporting
          id       = lwa_return-id
          number   = lwa_return-number
        importing
          is_block = lva_is_block.

      if lva_is_block is not initial and _index le 10.
        wait up to 2 seconds.
      else.
        call function 'BAPI_TRANSACTION_ROLLBACK'.

        if lva_is_block is not initial. "Se nao conseguiu criar movimentação por bloqueio de registro, nao marcar registro como processado e tentar reprocessaar novamente
          perform f_reinicia_processamento using p_zpps_ximfbf_log-obj_key.
        else.
          c_error = abap_true.

          perform f_add_mensagem_retorno_xi_r using p_zpps_ximfbf_log
                                                    lwa_return.
        endif.

        exit.
      endif.

    endif.


  enddo.

endform.

form f_envia_log_proc_xi.

  data: lv_rfc type rfcdest,
        lv_fm  type rs38l_fnam.

  check git_log_return_mfpf[] is not initial.

  call function 'ZFMCPI_UTIL_GET_RFC'
    exporting
      i_fm          = cc_z_fi_outbound_return
    importing
      e_rfc         = lv_rfc
    exceptions
      no_rfc        = 1
      no_rfc_config = 2
      others        = 3.

  if sy-subrc eq 0.
    call function cc_z_fi_outbound_return in background task
      destination lv_rfc
      as separate unit
      tables
        outreturn = git_log_return_mfpf.
  else.
    call function cc_z_fi_outbound_return in background task
      tables
        outreturn = git_log_return_mfpf.
  endif.


  commit work.

endform.

form f_valida_dados_ordem_prod  using p_zpps_ximfbf_log  type zpps_ximfbf_log
                                      p_check_matr_werks
                             changing c_error
                                      c_ordem_prod type ckmlmv013.

  data: lva_matnr_aux   type matnr,
        lva_aufnr       type aufnr,
        lva_matnr_ordem type matnr,
        lva_werks_ordem type werks_d,
        lva_lgort_ordem type lgort_d,
        lva_verid_ordem type verid.


  clear: c_error, c_ordem_prod.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = p_zpps_ximfbf_log-aufnr
    importing
      output = lva_aufnr.

  select single *
    from ckmlmv013 into c_ordem_prod
   where aufnr = lva_aufnr.

  if sy-subrc is not initial.
    c_error = abap_true.
    gva_msg_aux = text-m05.
    replace first occurrence of '&1' in gva_msg_aux with p_zpps_ximfbf_log-aufnr.

    perform f_add_mensagem_retorno_xi_f1  using p_zpps_ximfbf_log
                                                gva_msg_aux
                                                'E'
                                                p_zpps_ximfbf_log-aufnr.
    exit.
  endif.

  check: p_check_matr_werks = cc_x.

  call function 'CONVERSION_EXIT_MATN1_INPUT'
    exporting
      input  = p_zpps_ximfbf_log-matnr
    importing
      output = lva_matnr_ordem.

  if c_ordem_prod-pmatn <> lva_matnr_ordem or
     c_ordem_prod-prwrk <> p_zpps_ximfbf_log-werks.

    c_error = abap_true.
    gva_msg_aux = text-m06.

    replace first occurrence of '&1' in gva_msg_aux  with p_zpps_ximfbf_log-matnr.
    replace first occurrence of '&2' in gva_msg_aux  with p_zpps_ximfbf_log-werks.
    replace first occurrence of '&3' in gva_msg_aux  with p_zpps_ximfbf_log-aufnr.

    perform f_add_mensagem_retorno_xi_f  using p_zpps_ximfbf_log
                                               gva_msg_aux
                                               'E'
                                               p_zpps_ximfbf_log-matnr
                                               p_zpps_ximfbf_log-werks
                                               p_zpps_ximfbf_log-aufnr
                                               ''.

  endif.

endform.                    " YF_OBTEM_DADOS_ORDEMPROD

form f_entrada_producao_shdb using p_bflushdatagen    type bapi_rm_datgen
                                   p_zpps_ximfbf_log  type zpps_ximfbf_log
                                   p_nro_romaneio    type zsdt0001-nr_romaneio
                          changing c_error.

  data: vl_quebra(2)     type n,
        vl_qtde          type c length 16,
        vl_budat_char    type char10,
        vl_bldat_char    type char10,
        vl_field_mat     type c length 30,
        vl_field_quant   type c length 30,
        vl_field_centro  type c length 30,
        vl_field_dep     type c length 30,
        vl_message       type string,
        vl_count         type i,
        vl_mode          type c length 1,
        lva_confirmation type prtnr.


  clear: c_error,  git_msg[], git_bdc[].

  concatenate p_bflushdatagen-postdate+6(2) '. ' p_bflushdatagen-postdate+4(2) '.' p_bflushdatagen-postdate(4) into vl_budat_char.
  concatenate sy-datum+6(2) '. ' sy-datum+4(2) '.' sy-datum(4) into vl_bldat_char.

  perform f_bdc using:


  'X'   'SAPLBARM'            '0800',
  ' '   'BDC_CURSOR'          'RM61B-RB_KOMPO',
  ' '   'BDC_OKCODE'          '=RBTYP',
  ' '   'RM61B-RB_KOMPO'      'X',
  ' '   'RM61B-BUDAT'         vl_budat_char, "w_xi_mfbf-dtmvto
  ' '   'RM61B-BLDAT'         vl_bldat_char, "w_xi_mfbf-dtmvto
  ' '   'RM61B-WERKS'         p_bflushdatagen-prodplant,

  'X'   'SAPLBARM'            '0800',
  ' '   'BDC_OKCODE'          '=ISTDA',
  ' '   'RM61B-RB_KOMPO'      'X',
  ' '   'RM61B-BUDAT'         vl_budat_char, "w_xi_mfbf-dtmvto
  ' '   'RM61B-BLDAT'         vl_bldat_char, "w_xi_mfbf-dtmvto
  ' '   'RM61B-BKTXT'         p_bflushdatagen-docheadertxt,
  ' '   'BDC_CURSOR'          'RM61B-VERID',
  ' '   'RM61B-MATNR'         p_bflushdatagen-materialnr,
  ' '   'RM61B-WERKS'         p_bflushdatagen-prodplant,
  ' '   'RM61B-VERID'         p_bflushdatagen-prodversion,
  ' '   'RM61B-BOM_OFF'       'X',
  'X'   'SAPLCOWB'            '0130',
  ' '   'BDC_OKCODE'          '/00'.

  perform f_bdc using:
  'X'   'SAPLCOWB'            '0130',
  ' '   'BDC_OKCODE'          '=WEIT',
  ' '   'BDC_SUBSCR'          'SAPLCOWB',
  'X'   'SAPLBARM'            '0800',
  ' '   'BDC_OKCODE'          '/EEND'.

  call transaction cc_mfbf
     using git_bdc
     mode   vl_mode
     update cc_s
     messages into git_msg.

  check git_msg[] is not initial.

  read table git_msg into data(wa_msg)  with key msgtyp = cc_s.

  if sy-subrc eq 0.

    if wa_msg-msgnr = '193'.

      lva_confirmation    = wa_msg-msgv1.

      perform f_after_entrada_producao using p_zpps_ximfbf_log
                                             lva_confirmation
                                             p_bflushdatagen-postdate(4)
                                             p_nro_romaneio.

    else.

      c_error = abap_true.

      gva_msg_aux = |Houe um erro ao gerar o documento de produção para a chave { p_zpps_ximfbf_log-obj_key } |.

      perform f_add_mensagem_retorno_xi using p_zpps_ximfbf_log
                                              gva_msg_aux
                                             'E'.
      return.
    endif.
  else.

    loop at git_msg into data(lwa_msg_shdb) where msgtyp = cc_e.
      perform f_add_mensagem_retorno_xi_s using p_zpps_ximfbf_log
                                                lwa_msg_shdb.
    endloop.

  endif.

endform.

form f_bdc  using  p_dynbegin type any
                   p_name     type any
                   p_value    type any.

  data: lwa_bdc    type bdcdata.

  clear lwa_bdc.

  if p_dynbegin eq cc_x.

    lwa_bdc-program  = p_name.
    lwa_bdc-dynpro   = p_value.
    lwa_bdc-dynbegin = p_dynbegin.
  else.
    lwa_bdc-fnam     = p_name.
    lwa_bdc-fval     = p_value.
  endif.

  append lwa_bdc to git_bdc.

endform.

form f_add_mensagem_retorno_xi_f  using  p_zpps_ximfbf_log type zpps_ximfbf_log
                                         p_mensagem
                                         p_type
                                         p_msgv1
                                         p_msgv2
                                         p_msgv3
                                         p_msgv4.
  clear: git_log_return_mfpf.


  git_log_return_mfpf-obj_key         = p_zpps_ximfbf_log-obj_key.

  case p_zpps_ximfbf_log-zst_atlz.
    when 'I' or 'X'.
      git_log_return_mfpf-interface       = '09'.
    when 'E' or 'Y'.
      git_log_return_mfpf-interface       = '27'.
    when others.
  endcase.

  git_log_return_mfpf-dt_atualizacao  = sy-datum.
  git_log_return_mfpf-hr_atualizacao  = sy-uzeit.

  git_log_return_mfpf-type            = p_type.
  git_log_return_mfpf-id              = cc_classe_mensagem.
  git_log_return_mfpf-num             = '000'.
  git_log_return_mfpf-message         = p_mensagem.
  git_log_return_mfpf-message_v1      = p_msgv1.
  git_log_return_mfpf-message_v2      = p_msgv2.
  git_log_return_mfpf-message_v3      = p_msgv3.
  git_log_return_mfpf-message_v4      = p_msgv4.

  case p_zpps_ximfbf_log-zst_atlz.
    when 'I'.
      perform f_set_inf_add_ret using git_log_return_mfpf.
  endcase.



  append git_log_return_mfpf.

endform.                    " YF_BLOQUEIA_MATERIAL


form f_set_inf_add_ret changing p_return type zfie_ret_document.

  data: lwa_dados_add      type zsfi0001,
        lva_json_dados_add type c length 30000,
        lwa_bkpf_migo      type bkpf.

  clear: lwa_bkpf_migo.

  check p_return-type = 'S' and p_return-message_v2 is not initial.

  select single *
    from mkpf into @data(_lwa_mkpf)
   where mblnr eq @p_return-message_v2(10).

  check sy-subrc eq 0.

  read table git_zpps_ximfbf_log into data(lwa_zpps_ximfbf_log_tmp) with key obj_key = p_return-obj_key.
  if sy-subrc eq 0 and lwa_zpps_ximfbf_log_tmp-cd_safra is not initial.
    lwa_dados_add-lgort = lwa_zpps_ximfbf_log_tmp-cd_safra.
  endif.

  lva_json_dados_add  = /ui2/cl_json=>serialize( exporting data = lwa_dados_add ).

  p_return-info_adicional_1 = lva_json_dados_add+0000(4000).
  p_return-info_adicional_2 = lva_json_dados_add+4000(4000).
  p_return-info_adicional_3 = lva_json_dados_add+8000(4000).

endform.


form f_add_mensagem_retorno_xi_f1 using  p_zpps_ximfbf_log type zpps_ximfbf_log
                                         p_mensagem
                                         p_type
                                         p_msgv1.
  clear: git_log_return_mfpf.


  git_log_return_mfpf-obj_key         = p_zpps_ximfbf_log-obj_key.

  case p_zpps_ximfbf_log-zst_atlz.
    when 'I' or 'X'.
      git_log_return_mfpf-interface       = '09'.
    when 'E' or 'Y'.
      git_log_return_mfpf-interface       = '27'.
    when others.
  endcase.

  git_log_return_mfpf-dt_atualizacao  = sy-datum.
  git_log_return_mfpf-hr_atualizacao  = sy-uzeit.

  git_log_return_mfpf-type            = p_type.
  git_log_return_mfpf-id              = cc_classe_mensagem.
  git_log_return_mfpf-num             = '000'.
  git_log_return_mfpf-message         = p_mensagem.
  git_log_return_mfpf-message_v1      = p_msgv1.

  append git_log_return_mfpf.

endform.                    " YF_BLOQUEIA_MATERIAL

form f_add_mensagem_retorno_xi using p_zpps_ximfbf_log type zpps_ximfbf_log
                                     p_mensagem
                                     p_type.
  clear: git_log_return_mfpf.

  git_log_return_mfpf-obj_key         = p_zpps_ximfbf_log-obj_key.

  case p_zpps_ximfbf_log-zst_atlz.
    when 'I' or 'X'.
      git_log_return_mfpf-interface       = '09'.
    when 'E' or 'Y'.
      git_log_return_mfpf-interface       = '27'.
    when others.
  endcase.

  git_log_return_mfpf-dt_atualizacao  = sy-datum.
  git_log_return_mfpf-hr_atualizacao  = sy-uzeit.

  git_log_return_mfpf-type            = p_type.
  git_log_return_mfpf-id              = cc_classe_mensagem.
  git_log_return_mfpf-num             = '000'.
  git_log_return_mfpf-message         = p_mensagem.

  append git_log_return_mfpf.

endform.

form f_add_mensagem_retorno_xi_r using p_zpps_ximfbf_log type zpps_ximfbf_log
                                       p_bapi_ret        type bapiret2.
  clear: git_log_return_mfpf.

  git_log_return_mfpf-obj_key         = p_zpps_ximfbf_log-obj_key.

  case p_zpps_ximfbf_log-zst_atlz.
    when 'I' or 'X'.
      git_log_return_mfpf-interface       = '09'.
    when 'E' or 'Y'.
      git_log_return_mfpf-interface       = '27'.
    when others.
  endcase.

  git_log_return_mfpf-dt_atualizacao  = sy-datum.
  git_log_return_mfpf-hr_atualizacao  = sy-uzeit.

  git_log_return_mfpf-type            = p_bapi_ret-type.
  git_log_return_mfpf-id              = p_bapi_ret-id.
  git_log_return_mfpf-num             = p_bapi_ret-number.
  git_log_return_mfpf-message         = p_bapi_ret-message.
  git_log_return_mfpf-message_v1      = p_bapi_ret-message_v1.
  git_log_return_mfpf-message_v2      = p_bapi_ret-message_v2.
  git_log_return_mfpf-message_v3      = p_bapi_ret-message_v3.
  git_log_return_mfpf-message_v4      = p_bapi_ret-message_v4.

  append git_log_return_mfpf.
endform.


form f_add_mensagem_retorno_xi_s using p_zpps_ximfbf_log type zpps_ximfbf_log
                                       p_bapi_ret        type bdcmsgcoll.
  clear: git_log_return_mfpf.

  git_log_return_mfpf-obj_key         = p_zpps_ximfbf_log-obj_key.

  case p_zpps_ximfbf_log-zst_atlz.
    when 'I' or 'X'.
      git_log_return_mfpf-interface       = '09'.
    when 'E' or 'Y'.
      git_log_return_mfpf-interface       = '27'.
    when others.
  endcase.

  git_log_return_mfpf-dt_atualizacao  = sy-datum.
  git_log_return_mfpf-hr_atualizacao  = sy-uzeit.

  git_log_return_mfpf-type            = p_bapi_ret-msgtyp.
  git_log_return_mfpf-id              = p_bapi_ret-msgid.
  git_log_return_mfpf-num             = p_bapi_ret-msgnr.
  git_log_return_mfpf-message_v1      = p_bapi_ret-msgv1.
  git_log_return_mfpf-message_v2      = p_bapi_ret-msgv2.
  git_log_return_mfpf-message_v3      = p_bapi_ret-msgv3.
  git_log_return_mfpf-message_v4      = p_bapi_ret-msgv4.

  message id p_bapi_ret-msgid type 'S' number p_bapi_ret-msgnr with p_bapi_ret-msgv1
                                                                    p_bapi_ret-msgv2
                                                                    p_bapi_ret-msgv3
                                                                    p_bapi_ret-msgv4 into git_log_return_mfpf-message.


  append git_log_return_mfpf.
endform.


form f_set_xblnr_doc_material  using p_mblnr         type mkpf-mblnr
                                     p_mjahr         type mkpf-mjahr
                                     p_nro_romaneio  type zsdt0001-nr_romaneio.


  data: lwa_blpp type blpp,
        lit_mkpf type table of mkpf,
        lit_mseg type table of mseg.

  clear: lit_mkpf[], lit_mseg[].

  do 10 times.

    select *
      from mkpf
      into table lit_mkpf
     where mblnr = p_mblnr
       and mjahr = p_mjahr.

    if sy-subrc <> 0.
      wait up to 2 seconds.
      continue.
    endif.

    exit.
  enddo.

  read table lit_mkpf assigning field-symbol(<fs_mkpf>) index 1.
  check sy-subrc eq 0.

  if <fs_mkpf>-bktxt(08) = cc_prefix_header_ep.

    <fs_mkpf>-xblnr = p_nro_romaneio.

    call function 'MB_CHANGE_DOCUMENT'
      tables
        zmkpf = lit_mkpf
        zmseg = lit_mseg.

  endif.

endform.

form f_after_entrada_producao  using p_zpps_ximfbf_log type zpps_ximfbf_log
                                     p_confirmation    type prtnr
                                     p_mjahr           type mkpf-mjahr
                                     p_nro_romaneio    type zsdt0001-nr_romaneio.

  data: lwa_blpp type blpp.

  perform f_get_item_confirmacao using p_confirmation changing lwa_blpp.

  perform f_set_xblnr_doc_material using lwa_blpp-belnr p_mjahr p_nro_romaneio.

  gva_msg_aux = text-m02.
  replace first occurrence of '&1' in gva_msg_aux with p_confirmation.
  replace first occurrence of '&2' in gva_msg_aux with lwa_blpp-belnr.
  replace first occurrence of '&3' in gva_msg_aux with p_mjahr.

  perform f_add_mensagem_retorno_xi_f  using p_zpps_ximfbf_log
                                             gva_msg_aux
                                             'S'
                                             p_confirmation
                                             lwa_blpp-belnr
                                             p_mjahr
                                             ''.

  update zpps_ximfbf_log set mblnr            = lwa_blpp-belnr
                             confirmation     = p_confirmation
                             processado       = 'S'
   where obj_key = p_zpps_ximfbf_log-obj_key.

endform.

form f_lock_registro  using p_zpps_ximfbf_log type zpps_ximfbf_log.

  call function 'ENQUEUE_EZPPS_XIMFBF_LOG'
    exporting
      obj_key   = p_zpps_ximfbf_log-obj_key
      nrobol    = p_zpps_ximfbf_log-nrobol
      fgorigem  = p_zpps_ximfbf_log-fgorigem
      werks     = p_zpps_ximfbf_log-werks
      matnr     = p_zpps_ximfbf_log-matnr
      cd_ccusto = p_zpps_ximfbf_log-cd_ccusto
      charg     = p_zpps_ximfbf_log-charg
    exceptions
      others    = 3.


endform.

form f_unlock_registro  using p_zpps_ximfbf_log type zpps_ximfbf_log.

  call function 'DEQUEUE_EZPPS_XIMFBF_LOG'
    exporting
      obj_key   = p_zpps_ximfbf_log-obj_key
      nrobol    = p_zpps_ximfbf_log-nrobol
      fgorigem  = p_zpps_ximfbf_log-fgorigem
      werks     = p_zpps_ximfbf_log-werks
      matnr     = p_zpps_ximfbf_log-matnr
      cd_ccusto = p_zpps_ximfbf_log-cd_ccusto
      charg     = p_zpps_ximfbf_log-charg.

endform.


form f_block_reg_for_process.

  loop at git_zpps_ximfbf_log into data(lwa_zpps_ximfbf_log).

    data(_tabix) = sy-tabix.

    perform f_lock_registro using lwa_zpps_ximfbf_log.

    if sy-subrc = 0.
      lwa_zpps_ximfbf_log-zrg_atulizado = 'S'.
      lwa_zpps_ximfbf_log-processado    = 'P'."Em processamento
      lwa_zpps_ximfbf_log-data          = sy-datum.
      lwa_zpps_ximfbf_log-hora          = sy-uzeit.
      modify zpps_ximfbf_log from lwa_zpps_ximfbf_log.
    else.
      delete git_zpps_ximfbf_log index _tabix.
    endif.

  endloop.

  commit work.

endform.

form f_get_item_confirmacao  using p_confirmation type prtnr
                          changing c_blpp type blpp.

  clear: c_blpp.

  do 10 times.
    select single * into c_blpp
      from blpp
     where prtnr = p_confirmation
       and prtps = '0001'.
    if sy-subrc <> 0.
      wait up to 2 seconds.
      continue.
    endif.
    exit.
  enddo.

endform.

form f_set_processado  using p_obj_key type zpps_ximfbf_log-obj_key.

  update zpps_ximfbf_log set processado = 'S'
   where obj_key  eq p_obj_key.

endform.

form f_check_reinicia_processamento.

  data: lit_log type table of zpps_ximfbf_log.

  select *
    from zpps_ximfbf_log into table lit_log
   where zrg_atulizado eq 'S'
     and id_interface  eq 'S'
     and processado    eq 'P'. "Processamento incompleto

  delete lit_log where mblnr is not initial.

  loop at lit_log assigning field-symbol(<fs_log>).
    perform f_lock_registro using <fs_log>.
    if sy-subrc eq 0.
      perform f_reinicia_processamento using <fs_log>-obj_key.
      perform f_unlock_registro using <fs_log>.
    endif.
  endloop.

endform.
