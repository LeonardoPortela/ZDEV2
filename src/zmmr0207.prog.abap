*&---------------------------------------------------------------------*
*& Report ZMMR0207
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zmmr0207.
tables: j_1bnfdoc.



types: begin of ty_saida,
         "Dados CT-e
         docnum             type j_1bnfdoc-docnum,
         nfenum             type j_1bnfdoc-nfenum,
         docdat             type j_1bnfdoc-docdat,
         werks              type j_1bnflin-werks,
         name               type j_1bbranch-name,
         stcd1              type j_1bbranch-stcd1,
         parid              type j_1bnfnad-parid,
         name1              type lfa1-name1,
         cpnj_transb        type lfa1-stcd1,
         direct             type j_1bnfdoc-direct,
         cancel             type j_1bnfdoc-cancel,
         parvw              type j_1bnfnad-parvw,
         quantidade         type j_1bnflin-menge,
         valor              type j_1bnflin-netpr,
         netwr              type j_1bnflin-netwr,
         matnr              type j_1bnflin-matnr,
         maktx              type j_1bnflin-maktx,
         refkey             type j_1bnflin-refkey,
         cod_coleta         type j_1bnfnad-parid,
         desc_coleta        type lfa1-name1,
         cnpj_coleta        type lfa1-stcd1,
         cod_loc_dest       type j_1bnfnad-parid,
         loc_dest           type lfa1-name1,
         cnpj_dest          type lfa1-stcd1,
         placa_cav          type zsdt0001-placa_cav,
         placa_car1         type zsdt0001-placa_car1,
         placa_car2         type zsdt0001-placa_car2,
         placa_car3         type zsdt0001-placa_car3,
         chave_nfe          type zib_nfe_dist_ter-chave_nfe,
         regio              type j_1bnfe_active-regio,
         nfyear             type j_1bnfe_active-nfyear,
         nfmonth            type j_1bnfe_active-nfmonth,
         cnpj_active        type j_1bnfe_active-stcd1,
         model              type j_1bnfe_active-model,
         serie              type j_1bnfe_active-serie,
         nfnum9             type j_1bnfe_active-nfnum9,
         docnum9            type j_1bnfe_active-docnum9,
         cdv                type j_1bnfe_active-cdv,
         budat              type zfiwrt0008-budat,
         operacao           type zfiwrt0008-operacao,
         ch_referencia      type zfiwrt0008-ch_referencia,
         lifex              type  likp-lifex,
         tp_veic            type char20,
         agent_frete        type lfa1-lifnr,

         "Dados CT-e
         docnum_cte_p       type zib_cte_dist_ter-docnum_cte_p,
         numr_cte           type zib_cte_dist_ter-numr_cte,
         dt_emissao         type zib_cte_dist_ter-dt_emissao,
         emit_cnpj          type lfa1-stcd1,
         forn_emit          type lfa1-name1,
         inicio_muni        type zib_cte_dist_ter-inicio_muni,
         inicio_uf          type zib_cte_dist_ter-inicio_uf,
         termino_muni       type zib_cte_dist_ter-termino_muni,
         termino_uf         type zib_cte_dist_ter-termino_uf,
         reme_cnpj          type lfa1-stcd1,
         Remetente          type lfa1-name1,
         dest_cnpj          type lfa1-stcd1,
         destinatario       type lfa1-name1,
         exped_cnpj         type lfa1-stcd1,
         expedidor          type lfa1-name1,
         receb_cnpj         type lfa1-stcd1,
         recebedor          type lfa1-name1,
         f_tomadora         type zib_cte_dist_ter-f_tomadora,
         tomadora           type lfa1-name1,
         tomadora_cnpj      type lfa1-stcd1,
         subcontratado      type lfa1-name1,
         subcontratado_cnpj type lfa1-stcd1,
         placa_cav_cte      type zsdt0001-placa_cav,
         placa_car1_cte     type zsdt0001-placa_car1,
         placa_car2_cte     type zsdt0001-placa_car2,
         placa_car3_cte     type zsdt0001-placa_car3,
         qt_carga_cte       type zib_cte_dist_ter-qt_carga_cte,
         check_parc         type char01,
         "Dados MDF-e
         docnum_mdfe        type zsdt0105-docnum,
         nmdfe              type zsdt0102-nmdfe,
         cnpj_emi_mdfe      type zsdt0102-cnpj_emi,
         data_emi_mdfe      type zsdt0102-data_emi,
         desc_emit_mdfe     type lfa1-name1,
         placa_cav_mdfe     type zsdt0118-placa_cav,
         placa_car1_mdfe    type zsdt0118-placa_car1,
         placa_car2_mdfe    type zsdt0118-placa_car2,
         placa_car3_mdfe    type zsdt0118-placa_car3,
         motorista          type zsdt0118-motorista,
       end of ty_saida,

       begin of ty_dados_mdfe,
         docnum     type zsdt0105-docnum,
         docnum_ref type zsdt0105-docnum,
         nmdfe      type zsdt0102-nmdfe,
         data_emi   type zsdt0102-data_emi,
         status     type zsdt0102-status,
         cancel     type zsdt0102-cancel,
         cnpj_emi   type zsdt0102-cnpj_emi,
         name1      type lfa1-name1,
         placa_cav  type zsdt0118-placa_cav,
         placa_car1 type zsdt0118-placa_car1,
         placa_car2 type zsdt0118-placa_car2,
         placa_car3 type zsdt0118-placa_car3,
         motorista  type zsdt0118-motorista,
       end of ty_dados_mdfe,

       begin of ty_dados_cte,
         n55_chave_acesso   type zib_cte_dist_n55-n55_chave_acesso,
         docnum_cte_p       type zib_cte_dist_ter-docnum_cte_p,
         numr_cte           type zib_cte_dist_ter-numr_cte,
         dt_emissao         type zib_cte_dist_ter-dt_emissao,
         emit_cnpj          type lfa1-stcd1,
         emit_cpf           type lfa1-stcd2,
         forn_emit          type lfa1-name1,
         inicio_muni        type zib_cte_dist_ter-inicio_muni,
         inicio_uf          type zib_cte_dist_ter-inicio_uf,
         termino_muni       type zib_cte_dist_ter-termino_muni,
         termino_uf         type zib_cte_dist_ter-termino_uf,
         reme_cnpj          type lfa1-stcd1,
         reme_cpf           type lfa1-stcd2,
         reme_rsocial       type zib_cte_dist_ter-reme_rsocial,
         Remetente          type lfa1-name1,
         dest_cnpj          type lfa1-stcd1,
         dest_cpf           type lfa1-stcd2,
         dest_rsocial       type zib_cte_dist_ter-dest_rsocial,
         destinatario       type lfa1-name1,
         exped_cnpj         type lfa1-stcd1,
         exped_cpf          type lfa1-stcd2,
         exped_rsocial      type zib_cte_dist_ter-exped_rsocial,
         expedidor          type lfa1-name1,
         receb_cnpj         type lfa1-stcd1,
         receb_cpf          type lfa1-stcd2,
         receb_rsocial      type zib_cte_dist_ter-receb_rsocial,
         recebedor          type lfa1-name1,
         f_tomadora         type zib_cte_dist_ter-f_tomadora,
         tomadora           type lfa1-name1,
         tomadora_cnpj      type lfa1-stcd1,
         subcontratado      type lfa1-name1,
         subcontratado_cnpj type lfa1-stcd1,
         placa_cav_cte      type zsdt0001-placa_cav,
         placa_car1_cte     type zsdt0001-placa_car1,
         placa_car2_cte     type zsdt0001-placa_car2,
         placa_car3_cte     type zsdt0001-placa_car3,
         qt_carga_cte       type zib_cte_dist_ter-qt_carga_cte,
       end of ty_dados_cte,

       begin of ty_dados_veic_cte,
         vbeln         type vbfa-vbeln,
         ch_referencia type zsdt0001-ch_referencia,
         xblnr         type likp-xblnr,
         placa_cav     type zsdt0001-placa_cav,
         placa_car1    type zsdt0001-placa_car1,
         placa_car2    type zsdt0001-placa_car2,
         placa_car3    type zsdt0001-placa_car3,
       end of ty_dados_veic_cte,

       begin of ty_j_1bnfnad,
         docnum type j_1bnfnad-docnum,
         parid  type j_1bnfnad-parid,
         parvw  type j_1bnfnad-parvw,
         name1  type lfa1-name1,
         stcd1  type lfa1-stcd1,
         stcd2  type lfa1-stcd2,
       end of ty_j_1bnfnad.

data: it_saida              type table of ty_saida,
      it_j_1bnfnad          type table of ty_j_1bnfnad,
      it_local_dest         type table of ty_j_1bnfnad,
      it_dados_veic         type table of ty_dados_veic_cte,
      it_dados_cte          type table of ty_dados_cte,
      it_dados_mdfe         type table of ty_dados_mdfe,
      it_dados_mdfe_prop    type table of ty_dados_mdfe,
      it_dados_mdfe_prop_av type table of ty_dados_mdfe,
      zv_tabix              type sy-tabix.

data: dg_splitter_1        type ref to cl_gui_splitter_container,
      g_grid               type ref to cl_gui_alv_grid,
      g_custom_container   type ref to cl_gui_custom_container,
      c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,
      container_1          type ref to cl_gui_container,
      cl_container_95      type ref to cl_gui_docking_container,
      obj_dyndoc_id        type ref to cl_dd_document,
      tl_function          type ui_functions,
      wl_function          type ui_func,
*
      t_fieldcat           type lvc_t_fcat,
      w_fieldcat           type lvc_s_fcat,
      t_colorcell          type table of lvc_s_scol,
      w_colorcell          type lvc_s_scol,
      t_exctab             type slis_t_extab,
      w_exctab             type slis_extab,
      w_layout             type lvc_s_layo,
      w_stable             type lvc_s_stbl,
      t_style              type lvc_t_styl,
      w_style              type lvc_s_styl,
      t_rows               type lvc_t_row,
      w_rows               type lvc_s_row,
      ok_code              type sy-ucomm.

data: variante         like disvariant.
data: gs_variant_c type disvariant.

"Parametro de seleção.

selection-screen begin of block b1 with frame title text-001.
  select-options: p_docdat  for j_1bnfdoc-docdat obligatory, "obligatory.
                  p_docnum  for j_1bnfdoc-docnum.
selection-screen end   of block b1.

selection-screen begin of block b2 with frame title text-003.

  parameters: p_ent type c radiobutton group g1 user-command mass default 'X',
              p_sai type c radiobutton group g1.


selection-screen end of block b2.


start-of-selection.
  if p_ent is not initial.
    perform fm_sel_dados_entrada.
  endif.
  if p_sai is not initial.
    perform fm_sel_dados_saida.
  endif.
*&---------------------------------------------------------------------*
*& Form fm_sel_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form fm_sel_dados_saida.
  free: it_saida.
  data: it_saida_aux type table of ty_saida.


  select * from zi_dados_saida_produto
    into corresponding fields of table @it_saida
    where budat between @p_docdat-low and @p_docdat-high
      and docnum in @p_docnum
      and inco1 <> 'FOB'
*      and parid in ( '0000000161', '0000000119' )
      and docsta   =  '1'
      and direct   =  '1'
      and nr_romaneio ne @space.
*      and parvw    =  'PC'.


  select * from zi_dados_receb_produto
  appending corresponding fields of table @it_saida
  where docdat in @p_docdat
  and docnum in @p_docnum
  and parid in ( '0000000161', '0000000119' )
  and direct   =  '2'
  and parvw    =  'PC'
  and  docsta   =  '1'
  and inco1 <> 'FOB'.

  "Se o parid do parvw 'PC' for igual 'LR'.


  check it_saida is not initial.

  sort it_saida by docnum.
  delete adjacent duplicates from it_saida comparing docnum.

  "Selecionar todos os parceiro da nota.
*  select docnum, parvw, parid from j_1bnfnad  into table @data(it_J_1BNFNAD)
*  for all entries in @it_saida
*  where docnum eq @it_saida-docnum
*    and parvw eq 'LR'.



  loop at it_saida assigning field-symbol(<ws_saida>).
    <ws_saida>-chave_nfe = |{ <ws_saida>-regio }{ <ws_saida>-nfyear }{ <ws_saida>-nfmonth }{ <ws_saida>-cnpj_active }{ <ws_saida>-model
   }{ <ws_saida>-serie }{ <ws_saida>-nfnum9 }{ <ws_saida>-docnum9 }{ <ws_saida>-cdv }|.
    data(lv_serie) =  |{ <ws_saida>-serie alpha = out }|.
    data(lv_nfnum) =  |{ <ws_saida>-nfnum9 alpha = out }|.
    <ws_saida>-lifex =  |{ lv_nfnum }-{ lv_serie }|.
    if <ws_saida>-ch_referencia is initial.
      <ws_saida>-ch_referencia = '00000000000000000000'.
    endif.

*    read table it_J_1BNFNAD into data(wa_ibnfad) with key docnum = <ws_saida>-docnum
*                                                          parid  = <ws_saida>-parid.
*    if sy-subrc eq 0.
*      <ws_saida>-check_parc = abap_true.
*    endif.
  endloop.

*  sort it_saida by check_parc.
*  delete it_saida where check_parc eq abap_true.
*  sort it_saida by docnum.


*-----------------------------------------------------------------------------------------
  "Fluxo para encontrar NFe de simples remessa de importação.
*  Para os docnum extraídos da seleção acima  e com ZFIWRT0008-CH_REFERENCIA <> vazio
  select * from likp into table @data(it_likp)
  for all entries in @it_saida
  where berot  =  @it_saida-ch_referencia.
*    and lifex  =  @it_saida-lifex.

*   Pegar o doc de transporte rodoviário vinculado ao aviso
  if it_likp is not initial.
    select a~tknum as doc_transp , a~vbeln, b~shtyp, b~text1, b~text2, b~text3, b~text4, c~vsart
    from vttp as a
    inner join vttk as b on b~tknum eq a~tknum
    inner join tvtk as c on c~shtyp eq  b~shtyp
    into table @data(it_dados_transp)
    for all entries in @it_likp
    where a~vbeln = @it_likp-vbeln
    and c~vsart = '01'.
  endif.
* pegar parceiros do transporte
* Ponto de coleta, motorista e proprietário veiculo
  if it_dados_transp is not initial.
    select a~vbeln, a~parvw, a~lifnr, b~name1, b~stcd1, b~stcd2, b~ktokk
    from vtpa as a
    inner join lfa1 as b on b~lifnr eq a~lifnr
    into table @data(it_ponto_coleta)
    for all entries in @it_dados_transp
    where a~vbeln = @it_dados_transp-doc_transp
    and a~posnr eq '000000'
    and a~parvw eq 'PC'.

*    * pegar parceiros do transporte
* Local de entrega

    select a~vbeln, a~parvw, b~kunnr, b~name1, b~stcd1, b~stcd2, b~ktokd
    from vtpa as a
    inner join kna1 as b on b~kunnr eq a~kunnr
    into table @data(it_local_entrega)
    for all entries in @it_dados_transp
    where a~vbeln = @it_dados_transp-doc_transp
    and a~posnr eq '000000'
    and a~parvw eq 'LR'.

    select a~vbeln, a~parvw, a~lifnr, b~name1, b~stcd1, b~stcd2, b~ktokk
    from vtpa as a
    inner join lfa1 as b on b~lifnr eq a~lifnr
    into table @data(it_prop_veiculo)
    for all entries in @it_dados_transp
    where a~vbeln eq @it_dados_transp-doc_transp
    and a~posnr eq '000000'
    and a~parvw eq 'PV'.

    select a~vbeln, a~parvw, a~lifnr, b~name1, b~stcd1, b~stcd2, b~ktokk
    from vtpa as a
    inner join lfa1 as b on b~lifnr eq a~lifnr
    into table @data(it_agent_frete)
    for all entries in @it_dados_transp
    where a~vbeln eq @it_dados_transp-doc_transp
    and a~posnr eq '000000'
    and a~parvw eq 'SP'.

  endif.
*      Fim do fluxo de NFe simples remessa de importação
*-----------------------------------------------------------------------------------------

*      Fluxo nota fiscal de venda.
*      Para os docnum acima localizados , buscar o doc remessa
  select vbelv as doc_remessa, vbeln
  from vbfa
  into table @data(it_vbfa)
  for all entries in @it_saida
  where vbeln   = @it_saida-refkey+0(10)
  and vbtyp_n = 'M'
  and VBTYP_v = 'J'.

*      Pegar o doc de transporte rodoviário vinculado a remessa
  if it_vbfa is not initial.
    select a~tknum as doc_transp , a~vbeln, b~shtyp, b~text1, b~text2, b~text3, b~text4, c~vsart
    from vttp as a
    inner join vttk as b on b~tknum eq a~tknum
    inner join tvtk as c on c~shtyp eq  b~shtyp
    into table @data(it_dados_transp_remessa)
    for all entries in @it_vbfa
    where a~vbeln = @it_vbfa-doc_remessa
    and c~vsart = '01'.

    select a~vbeln, a~parvw, a~lifnr, b~name1, b~stcd1, b~stcd2, b~ktokk
    from vtpa as a
    inner join lfa1 as b on b~lifnr eq a~lifnr
    appending table @it_ponto_coleta
    for all entries in @it_dados_transp_remessa
    where a~vbeln eq @it_dados_transp_remessa-doc_transp
    and a~posnr eq '000000'
    and a~parvw eq 'PC'.

*    * pegar parceiros do transporte
* Local de entrega

    select a~vbeln, a~parvw, b~kunnr, b~name1, b~stcd1, b~stcd2, b~ktokd
    from vtpa as a
    inner join kna1 as b on b~kunnr eq a~kunnr
    appending table @it_local_entrega
    for all entries in @it_dados_transp_remessa
    where a~vbeln eq @it_dados_transp_remessa-doc_transp
    and a~posnr eq '000000'
    and a~parvw eq 'LR'.

    select a~vbeln, a~parvw, a~lifnr, b~name1, b~stcd1, b~stcd2, b~ktokk
    from vtpa as a
    inner join lfa1 as b on b~lifnr eq a~lifnr
    appending table @it_prop_veiculo
    for all entries in @it_dados_transp_remessa
    where a~vbeln eq @it_dados_transp_remessa-doc_transp
    and a~posnr eq '000000'
    and a~parvw eq 'PV'.


    select a~vbeln, a~parvw, a~lifnr, b~name1, b~stcd1, b~stcd2, b~ktokk
    from vtpa as a
    inner join lfa1 as b on b~lifnr eq a~lifnr
    appending table @it_agent_frete
    for all entries in @it_dados_transp_remessa
    where a~vbeln eq @it_dados_transp_remessa-doc_transp
    and a~posnr eq '000000'
    and a~parvw eq 'SP'.

    "Fluxo para pegar o dados mdf-e.
    select * from likp appending table @it_likp
    for all entries in @it_vbfa
    where vbeln eq  @it_vbfa-doc_remessa.
    if sy-subrc eq 0.
      select * from zfiwrt0008 into table @data(it_zfiwrt0008)
      for all entries in @it_likp
      where ch_referencia eq  @it_likp-xblnr+0(20).

    endif.
  endif.
**  Fim fluxo nota fiscal de venda.
* -----------------------------------------------------------------------------------------------

*  *Busca para OV de serviço de frete  ( quando for veiculo frota transportando para Amaggi não tem OV)
  if it_dados_transp is not initial.
    select * from vbak into table @data(it_vbak)
      for all entries in @it_dados_transp
    where tknum = @it_dados_transp-doc_transp .
  endif.

  if it_dados_transp_remessa is not initial.
    select * from vbak appending table it_vbak
    for all entries in it_dados_transp_remessa
    where tknum = it_dados_transp_remessa-doc_transp.
  endif.


*  Buscar o docnum do Ct-e
  if it_vbak is not initial.
    select a~vbeln, a~vbelv, b~docnum as docnum_cte
    from vbfa as a
    inner join j_1bnflin as b on b~refkey eq a~vbeln
      into table @data(it_dados_cte_aux)
      for all entries in @it_vbak
    where a~vbelv = @it_vbak-vbeln
    and a~vbtyp_n    = 'M'
    and a~VBTYP_v     = 'C'.
  endif.

*Verifica se o docnum do Ct-e esta ativo
  if it_dados_cte_aux is not initial.
    select d~docnum, e~nfenum, e~series
    from j_1bnfe_active as d
      inner join j_1bnfdoc as e on e~docnum eq d~docnum
      into table @data(it_cte_ativo)
      for all entries in @it_dados_cte_aux
    where d~docnum eq @it_dados_cte_aux-docnum_cte
    and d~docsta   eq '1'
    and d~code     eq '100'
    and e~cancel   ne 'X'.
  endif.

  "Seleção de dados CT-e.
  if it_cte_ativo is not initial.
    select distinct b~docnum_cte_p, a~n55_chave_acesso, b~numr_cte, b~dt_emissao, b~emit_cnpj, c~name1 as forn_emit,
    b~inicio_muni, b~inicio_uf, b~termino_muni, b~termino_uf, b~reme_cnpj, d~name1 as Remetente,
    b~dest_cnpj, e~name1 as destinatario, b~exped_cnpj, f~name1 as expedidor,
    b~receb_cnpj, j~name1 as recebedor, b~f_tomadora, i~name as tomadora, i~stcd1 as tomadora_cnpj, b~qt_carga_cte,
    b~emit_cpf, b~reme_cpf, b~dest_cpf, b~exped_cpf, b~dest_cpf,
    b~emit_rsocial, b~reme_rsocial, b~dest_rsocial, b~exped_rsocial, b~dest_rsocial
    from zib_cte_dist_n55 as a
    left join zib_cte_dist_ter as b on b~cd_chave_cte eq a~cd_chave_cte
    left join lfa1 as c on c~stcd1 eq b~emit_cnpj
    left join lfa1 as d on d~stcd1 eq b~reme_cnpj or d~stcd2 eq b~reme_cpf
    left join lfa1 as e on e~stcd1 eq b~dest_cnpj or e~stcd2 eq b~dest_cpf
    left join lfa1 as f on f~stcd1 eq b~exped_cnpj or f~stcd2 eq b~exped_cpf
    left join lfa1 as j on j~stcd1 eq b~receb_cnpj or j~stcd2 eq b~receb_cpf
    left join j_1bbranch as i on i~branch eq b~f_tomadora
    into corresponding fields of table @it_dados_cte
    for all entries in @it_cte_ativo
    where b~docnum_cte_p eq @it_cte_ativo-docnum
      and b~cd_modal eq '01'
      and b~cd_tipo_servico eq '0'.

    if it_dados_cte is not initial.
*  Seleção de dados veiculo CT-e.
      select * from zcte_trans
      into table @data(it_dados_veic_cte)
        for all entries in @it_dados_cte
      where docnum eq @it_dados_cte-docnum_cte_p.
    endif.
  endif.


*  Busca do MDf-e
  if it_dados_cte is not initial.
*    select a~docnum_ref, b~docnum, b~nmdfe, b~data_emi, b~hora_emi
*    from zsdt0105 as a
*    inner join zsdt0102 as b on b~docnum eq a~docnum_ref
*    into table @data(it_mdfe)
*      for all entries in @it_cte_ativo
*    where a~docnum eq @it_cte_ativo-docnum
*    and b~autorizado eq 'X'
*    and b~estornado    eq ''
*    and b~cancel       eq ''.

    "Dados do MDF-e.
    select distinct a~docnum, a~docnum_ref, b~nmdfe, b~data_emi, b~status, b~cancel, b~cnpj_emi, d~name1,
    c~placa_cav, c~placa_car1, c~placa_car2, c~placa_car3, c~motorista
    from zsdt0105 as a
    inner join zsdt0102 as b on b~docnum eq a~docnum_ref
    inner join lfa1 as d on d~stcd1 eq b~cnpj_emi
    inner join zsdt0118 as c on c~docnum eq b~docnum
    into corresponding fields of table @it_dados_mdfe
      for all entries in @it_dados_cte
      where a~docnum eq @it_dados_cte-docnum_cte_p
      and b~autorizado eq 'X'
      and b~estornado    eq ''
      and b~cancel       eq ''.
  endif.

*  Se faturamento do frete com a frota , a MDf-e  é emitida sobre a NF
  select distinct a~docnum, a~docnum_ref, b~nmdfe, b~data_emi, b~status, b~cancel, b~cnpj_emi, d~name1,
    c~placa_cav, c~placa_car1, c~placa_car2, c~placa_car3, c~motorista
    from zsdt0105 as a
    inner join zsdt0102 as b on b~docnum eq a~docnum_ref
    inner join lfa1 as d on d~stcd1 eq b~cnpj_emi
    inner join zsdt0118 as c on c~docnum eq b~docnum
    into table @data(it_dados_mdfe_prop)
    for all entries in @it_saida
  where a~docnum   eq @it_saida-docnum
  and b~autorizado eq 'X'
  and b~estornado  eq ''
  and b~cancel     eq ''.

  if it_zfiwrt0008 is not initial.
    select distinct a~docnum, a~docnum_ref, b~nmdfe, b~data_emi, b~status, b~cancel, b~cnpj_emi, d~name1,
     c~placa_cav, c~placa_car1, c~placa_car2, c~placa_car3, c~motorista
     from zsdt0105 as a
     inner join zsdt0102 as b on b~docnum eq a~docnum_ref
     inner join lfa1 as d on d~stcd1 eq b~cnpj_emi
     inner join zsdt0118 as c on c~docnum eq b~docnum
     appending table @it_dados_mdfe_prop
     for all entries in @it_zfiwrt0008
   where a~docnum   eq @it_zfiwrt0008-docnum
   and b~autorizado eq 'X'
   and b~estornado  eq ''
   and b~cancel     eq ''.
  endif.


* Se não encontrar a Mdf-e sobre a NF na seleção acima , buscar no fluxo avulso
  select distinct a~docnum, a~docnum_ref, b~nmdfe, b~data_emi, b~status, b~cancel, b~cnpj_emi, d~name1,
  c~placa_cav, c~placa_car1, c~placa_car2, c~placa_car3, c~motorista
  from zsdt0241 as a
  inner join zsdt0102 as b on b~docnum eq a~docnum_ref
  inner join lfa1 as d on d~stcd1 eq b~cnpj_emi
  inner join zsdt0118 as c on c~docnum eq b~docnum
  into table @data(it_dados_mdfe_prop_av)
  for all entries in @it_saida
  where a~chave    eq @it_saida-chave_nfe
  and b~autorizado eq 'X'
  and b~estornado  eq ''
  and b~cancel     eq ''.

  loop at it_saida assigning field-symbol(<wa_saida>).
    clear: zv_tabix.

    read table it_likp into data(wa_likp) with key berot  =  <wa_saida>-ch_referencia.
*                                                   lifex  =  <wa_saida>-lifex.

    if sy-subrc eq 0.
      read table it_dados_transp into data(wa_transp) with key vbeln = wa_likp-vbeln.
      if sy-subrc eq 0.
        read table it_ponto_coleta into data(wa_coleta) with key vbeln = wa_transp-doc_transp
                                                                 parvw = 'PC'.
        if sy-subrc eq 0 ." and ( wa_coleta-lifnr eq  '0000000161' or wa_coleta-lifnr eq '0000000119' ).
          <wa_saida>-cod_coleta  = wa_coleta-lifnr.
          <wa_saida>-desc_coleta = wa_coleta-name1.

          if wa_coleta-stcd1 is not initial.
            <wa_saida>-cnpj_coleta = wa_coleta-stcd1.
          else.
            <wa_saida>-cnpj_coleta = wa_coleta-stcd2.
          endif.
*        else.
*          <wa_saida>-check_parc = abap_true.
*          continue.
        endif.
      endif.
    endif.


    read table it_likp into wa_likp with key berot  =  <wa_saida>-ch_referencia.
*                                                   lifex  =  <wa_saida>-lifex.

    if sy-subrc eq 0.
      read table it_dados_transp into wa_transp with key vbeln = wa_likp-vbeln.
      if sy-subrc eq 0.
        <wa_saida>-placa_cav  = wa_transp-text1+0(7).
        <wa_saida>-placa_car1 = wa_transp-text2+0(7).
        <wa_saida>-placa_car2 = wa_transp-text3+0(7).
        <wa_saida>-placa_car3 = wa_transp-text4+0(7).

        read table it_local_entrega into data(wa_local_entrega) with key vbeln = wa_transp-doc_transp
                                                                 parvw = 'LR'.
        if sy-subrc eq 0.
          "Dados local de destino
          <wa_saida>-cod_loc_dest  = wa_local_entrega-kunnr.
          <wa_saida>-loc_dest      = wa_local_entrega-name1.

          "Dados local de entrega
          <wa_saida>-parid         = wa_local_entrega-kunnr.
          <wa_saida>-name1         = wa_local_entrega-name1.

          if wa_local_entrega-stcd1 is not initial.
            "Dados local de destino
            <wa_saida>-cnpj_dest    = wa_local_entrega-stcd1.

            "Dados local de entrega
            <wa_saida>-cpnj_transb  = wa_local_entrega-stcd1.
          else.
            "Dados local de destino
            <wa_saida>-cnpj_dest    = wa_local_entrega-stcd2.

            "Dados local de entrega
            <wa_saida>-cpnj_transb  = wa_local_entrega-stcd2.
          endif.
        endif.
      endif.
      read table it_vbak into data(wa_vbak) with key tknum = wa_transp-doc_transp.

      read table it_prop_veiculo into data(wa_pro_veic) with key vbeln = wa_transp-doc_transp.
      if sy-subrc eq 0.
        case wa_pro_veic-ktokk.
          when 'ZFIC'.
            <wa_saida>-tp_veic = 'Frota'.
          when others.
            <wa_saida>-tp_veic = 'Terceiro'.
        endcase.
      endif.
    else.
      read table it_vbfa into data(wa_vbfa) with key vbeln = <wa_saida>-refkey+0(10).
      if  sy-subrc eq 0.
        read table it_dados_transp_remessa into wa_transp with key vbeln = wa_vbfa-doc_remessa.
        if sy-subrc eq 0.
          read table it_ponto_coleta into wa_coleta with key vbeln = wa_transp-doc_transp
                                                                   parvw = 'PC'.
          if sy-subrc eq 0 ."and ( wa_coleta-lifnr eq  '0000000161' or wa_coleta-lifnr eq '0000000119' ).
            <wa_saida>-cod_coleta  = wa_coleta-lifnr.
            <wa_saida>-desc_coleta = wa_coleta-name1.

            if wa_coleta-stcd1 is not initial.
              <wa_saida>-cnpj_coleta = wa_coleta-stcd1.
            else.
              <wa_saida>-cnpj_coleta = wa_coleta-stcd2.
            endif.
*          else.
*            <wa_saida>-check_parc = abap_true.
*            continue.
          endif.

          <wa_saida>-placa_cav  = wa_transp-text1+0(7).
          <wa_saida>-placa_car1 = wa_transp-text2+0(7).
          <wa_saida>-placa_car2 = wa_transp-text3+0(7).
          <wa_saida>-placa_car3 = wa_transp-text4+0(7).

          read table it_local_entrega into wa_local_entrega with key vbeln = wa_transp-doc_transp
                                                                     parvw = 'LR'.
          if sy-subrc eq 0.
            "Dados local de destino
            <wa_saida>-cod_loc_dest  = wa_local_entrega-kunnr.
            <wa_saida>-loc_dest      = wa_local_entrega-name1.

            "Dados local de entrega
            <wa_saida>-parid         = wa_local_entrega-kunnr.
            <wa_saida>-name1         = wa_local_entrega-name1.

            if wa_local_entrega-stcd1 is not initial.
              "Dados local de destino
              <wa_saida>-cnpj_dest    = wa_local_entrega-stcd1.

              "Dados local de entrega
              <wa_saida>-cpnj_transb  = wa_local_entrega-stcd1.
            else.
              "Dados local de destino
              <wa_saida>-cnpj_dest    = wa_local_entrega-stcd2.

              "Dados local de entrega
              <wa_saida>-cpnj_transb  = wa_local_entrega-stcd2.
            endif.
          endif.

          read table it_vbak into wa_vbak with key tknum = wa_transp-doc_transp.
          read table it_prop_veiculo into wa_pro_veic with key vbeln = wa_transp-doc_transp.
          if sy-subrc eq 0.
            case wa_pro_veic-ktokk.
              when 'ZFIC'.
                <wa_saida>-tp_veic = 'Frota'.
              when others.
                <wa_saida>-tp_veic = 'Terceiro'.
            endcase.
          endif.
        endif.
      endif.
    endif.

*    read table it_vbak into wa_vbak with key tknum = wa_transp-doc_transp.
*    if sy-subrc eq 0.
    read table it_dados_cte_aux into data(wa_dados_cte_aux) with key vbelv = wa_vbak-vbeln.
    if sy-subrc eq 0.
      read table it_cte_ativo into data(wa_dados_cte_ativo) with key docnum = wa_dados_cte_aux-docnum_cte.
      if sy-subrc eq 0.

*          Dados cte.
        read table it_dados_cte into data(ws_dados_cte) with key docnum_cte_p = wa_dados_cte_ativo-docnum.
        if sy-subrc eq 0.
          <wa_saida>-docnum_cte_p = ws_dados_cte-docnum_cte_p.
          <wa_saida>-numr_cte     = ws_dados_cte-numr_cte    .
          <wa_saida>-dt_emissao   = ws_dados_cte-dt_emissao  .

          if ws_dados_cte-emit_cnpj is not initial.
            <wa_saida>-emit_cnpj    = ws_dados_cte-emit_cnpj.
          else.
            <wa_saida>-emit_cnpj    = ws_dados_cte-emit_cpf .
          endif.

          <wa_saida>-forn_emit    = ws_dados_cte-forn_emit   .
          <wa_saida>-inicio_muni  = |{ ws_dados_cte-inicio_muni }-{ ws_dados_cte-inicio_uf }|.
*      <wa_saida>-inicio_uf    = ws_dados_cte-inicio_uf   .
          <wa_saida>-termino_muni = |{ ws_dados_cte-termino_muni }-{ ws_dados_cte-termino_uf }|.
*      <wa_saida>-termino_uf   = ws_dados_cte-termino_uf  .

          if ws_dados_cte-reme_cnpj is not initial and ws_dados_cte-reme_cnpj ne '00000000000000'.
            <wa_saida>-reme_cnpj    = ws_dados_cte-reme_cnpj   .
          else.
            <wa_saida>-reme_cnpj    = ws_dados_cte-reme_cpf   .
          endif.

          if ws_dados_cte-dest_cnpj is not initial and ws_dados_cte-dest_cnpj ne '00000000000000'.
            <wa_saida>-dest_cnpj    = ws_dados_cte-dest_cnpj   .
          else.
            <wa_saida>-dest_cnpj    = ws_dados_cte-dest_cpf   .
          endif.

          if ws_dados_cte-exped_cnpj is not initial and ws_dados_cte-exped_cnpj ne '00000000000000'.
            <wa_saida>-exped_cnpj   = ws_dados_cte-exped_cnpj  .
          else.
            <wa_saida>-exped_cnpj   = ws_dados_cte-exped_cpf  .
          endif.

          if ws_dados_cte-receb_cnpj is not initial and ws_dados_cte-receb_cnpj ne '00000000000000'.
            <wa_saida>-receb_cnpj   = ws_dados_cte-receb_cnpj  .
          else.
            <wa_saida>-receb_cnpj   = ws_dados_cte-receb_cpf  .
          endif.

*      if ws_dados_cte-tomadora_cnpj is not initial.
          <wa_saida>-tomadora_cnpj = ws_dados_cte-tomadora_cnpj.
*      else.
*        <wa_saida>-tomadora_cnpj = ws_dados_cte-tomadora_cpf.
*      endif.

          <wa_saida>-Remetente    = ws_dados_cte-reme_rsocial.
          <wa_saida>-destinatario = ws_dados_cte-dest_rsocial.
          <wa_saida>-expedidor    = ws_dados_cte-exped_rsocial.
          <wa_saida>-recebedor    = ws_dados_cte-receb_rsocial.
          <wa_saida>-f_tomadora   = ws_dados_cte-f_tomadora  .
          <wa_saida>-tomadora     = ws_dados_cte-tomadora    .
          <wa_saida>-qt_carga_cte = ws_dados_cte-qt_carga_cte.
        endif.
      endif.
*      endif.
    endif.

    loop at it_dados_veic_cte into data(wa_dados_veic_cte) where docnum = ws_dados_cte-docnum_cte_p.
      add 1 to zv_tabix.
*      if sy-subrc eq 0.
      <wa_saida>-subcontratado       =  wa_dados_veic_cte-prop_nome.
      if wa_dados_veic_cte-prop_cnpj is not initial.
        <wa_saida>-subcontratado_cnpj  =  wa_dados_veic_cte-prop_cnpj.
      else.
        <wa_saida>-subcontratado_cnpj  =  wa_dados_veic_cte-prop_cpf.
      endif.
      if wa_dados_veic_cte-tp_veiculo eq '0'.
        <wa_saida>-placa_cav_cte       =  wa_dados_veic_cte-pc_veiculo.
      else.

        if zv_tabix = '2' and wa_dados_veic_cte-tp_veiculo eq '1'.
          <wa_saida>-placa_car1_cte      =  wa_dados_veic_cte-pc_veiculo.
        endif.

        if zv_tabix = '3' and <wa_saida>-placa_car2_cte is initial and wa_dados_veic_cte-tp_veiculo eq '1'.
          <wa_saida>-placa_car2_cte      =  wa_dados_veic_cte-pc_veiculo.
        endif.

        if zv_tabix = '4' and <wa_saida>-placa_car3_cte is initial and wa_dados_veic_cte-tp_veiculo eq '1'.
          <wa_saida>-placa_car3_cte      =  wa_dados_veic_cte-pc_veiculo.
        endif.
      endif.
    endloop.

*    Dados MDF-e.
    read table it_dados_mdfe into data(ws_dados_mdfe) with key docnum = ws_dados_cte-docnum_cte_p.
    if sy-subrc eq 0.
      <wa_saida>-docnum_mdfe      = ws_dados_mdfe-docnum_ref.
      <wa_saida>-nmdfe            =  ws_dados_mdfe-nmdfe.
      <wa_saida>-data_emi_mdfe    =  ws_dados_mdfe-data_emi.
      <wa_saida>-cnpj_emi_mdfe    =  ws_dados_mdfe-cnpj_emi.
      <wa_saida>-desc_emit_mdfe   =  ws_dados_mdfe-name1.
      <wa_saida>-placa_cav_mdfe   =  ws_dados_mdfe-placa_cav.
      <wa_saida>-placa_car1_mdfe  =  ws_dados_mdfe-placa_car1.
      <wa_saida>-placa_car2_mdfe  =  ws_dados_mdfe-placa_car2.
      <wa_saida>-placa_car3_mdfe  =  ws_dados_mdfe-placa_car3.
      <wa_saida>-motorista        =  ws_dados_mdfe-motorista.
    else.
      "Se não achar o MDF-e vinculado ao CT-e, verificar se existe MDF-e vinculado a NF-e.
      clear: ws_dados_mdfe.
      read table it_dados_mdfe_prop into ws_dados_mdfe with key docnum = <wa_saida>-docnum.
      if sy-subrc eq 0.
        <wa_saida>-docnum_mdfe      =  ws_dados_mdfe-docnum_ref.
        <wa_saida>-nmdfe            =  ws_dados_mdfe-nmdfe.
        <wa_saida>-data_emi_mdfe    =  ws_dados_mdfe-data_emi.
        <wa_saida>-cnpj_emi_mdfe    =  ws_dados_mdfe-cnpj_emi.
        <wa_saida>-desc_emit_mdfe   =  ws_dados_mdfe-name1.
        <wa_saida>-placa_cav_mdfe   =  ws_dados_mdfe-placa_cav.
        <wa_saida>-placa_car1_mdfe  =  ws_dados_mdfe-placa_car1.
        <wa_saida>-placa_car2_mdfe  =  ws_dados_mdfe-placa_car2.
        <wa_saida>-placa_car3_mdfe  =  ws_dados_mdfe-placa_car3.
        <wa_saida>-motorista        =  ws_dados_mdfe-motorista.
      endif.

      "Verificar se tem MDF-e avulsa.
      clear: ws_dados_mdfe.
      read table it_dados_mdfe_prop_av into ws_dados_mdfe with key docnum = <wa_saida>-docnum.
      if sy-subrc eq 0.
        <wa_saida>-docnum_mdfe      =  ws_dados_mdfe-docnum_ref.
        <wa_saida>-nmdfe            =  ws_dados_mdfe-nmdfe.
        <wa_saida>-data_emi_mdfe    =  ws_dados_mdfe-data_emi.
        <wa_saida>-cnpj_emi_mdfe    =  ws_dados_mdfe-cnpj_emi.
        <wa_saida>-desc_emit_mdfe   =  ws_dados_mdfe-name1.
        <wa_saida>-placa_cav_mdfe   =  ws_dados_mdfe-placa_cav.
        <wa_saida>-placa_car1_mdfe  =  ws_dados_mdfe-placa_car1.
        <wa_saida>-placa_car2_mdfe  =  ws_dados_mdfe-placa_car2.
        <wa_saida>-placa_car3_mdfe  =  ws_dados_mdfe-placa_car3.
        <wa_saida>-motorista        =  ws_dados_mdfe-motorista.
      endif.

      read table it_likp into wa_likp with key vbeln = wa_vbfa-doc_remessa.
      if sy-subrc eq 0.
        read table it_zfiwrt0008 into data(wa_znfwrt0008) with key ch_referencia = wa_likp-xblnr+0(20).
        if sy-subrc eq 0.
          read table it_dados_mdfe_prop into ws_dados_mdfe with key docnum = wa_znfwrt0008-docnum.
          if sy-subrc eq 0.
            <wa_saida>-docnum_mdfe      =  ws_dados_mdfe-docnum_ref.
            <wa_saida>-nmdfe            =  ws_dados_mdfe-nmdfe.
            <wa_saida>-data_emi_mdfe    =  ws_dados_mdfe-data_emi.
            <wa_saida>-cnpj_emi_mdfe    =  ws_dados_mdfe-cnpj_emi.
            <wa_saida>-desc_emit_mdfe   =  ws_dados_mdfe-name1.
            <wa_saida>-placa_cav_mdfe   =  ws_dados_mdfe-placa_cav.
            <wa_saida>-placa_car1_mdfe  =  ws_dados_mdfe-placa_car1.
            <wa_saida>-placa_car2_mdfe  =  ws_dados_mdfe-placa_car2.
            <wa_saida>-placa_car3_mdfe  =  ws_dados_mdfe-placa_car3.
            <wa_saida>-motorista        =  ws_dados_mdfe-motorista.
          endif.
        endif.
      endif.
    endif.

    read table it_agent_frete into data(wa_agent_frete) with key vbeln = wa_transp-doc_transp
                                                                 parvw = 'SP'.
    if sy-subrc eq 0.
      <wa_saida>-agent_frete = wa_agent_frete-lifnr.
    endif.

    clear: ws_dados_cte, wa_dados_veic_cte, ws_dados_mdfe, wa_vbak, wa_vbfa, wa_local_entrega, wa_transp, wa_likp, wa_znfwrt0008, wa_agent_frete.
  endloop.

*  sort it_saida by check_parc.
*  delete it_saida where check_parc eq abap_true.
*  sort it_saida by docnum.

  call screen 0100.
endform.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'ST0100'.
  set titlebar 'TIT0100'.

  perform fm_alv.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.


  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.
    when 'EXIT'.
      leave program.
    when others.
  endcase.

endmodule.
*&---------------------------------------------------------------------*
*& Form fm_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form fm_alv .
  data: wl_layout type slis_layout_alv.
  data:
    p_text      type sdydo_text_element,
    filtros	    type zif_screen_linha_filtro,
    i_filtros	  type zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) type c,
    v_uzeit(10) type c.


  perform f_fieldcatalog.

  variante = value #( report = sy-repid ).


  if g_grid is initial.

    clear: i_filtros.
    concatenate sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) into v_datum.
    concatenate sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) into v_uzeit.
    describe table It_saida lines data(v_lines).
    p_text = 'Exibir dados entrega produto nos terminais estado RO'.
    append value #( parametro = 'Data:' valor = v_datum ) to i_filtros.
    append value #( parametro = 'Hora:' valor = v_uzeit ) to i_filtros.
    append value #( parametro = 'Registros:' valor = v_lines ) to i_filtros.

  endif.

  if zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      exporting
        i_titulo  = conv #( p_text )
        i_filtros = i_filtros
      changing
        split     = dg_splitter_1
        alv       = g_grid ) = abap_true.


    w_layout-sel_mode    = 'A'.
    w_layout-col_opt     = abap_true.
    w_layout-cwidth_opt  = abap_true.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    append wl_function to tl_function.

    call method g_grid->set_table_for_first_display
      exporting
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      changing
        it_outtab                     = it_saida[]
        it_fieldcatalog               = t_fieldcat
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.


    call method g_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    if lines( t_rows ) > 0.
      call method g_grid->set_selected_rows
        exporting
          it_index_rows = t_rows.
    endif.

  else.
    call method g_grid->refresh_table_display( is_stable = w_stable ).
  endif.

  wl_layout-colwidth_optimize = 'X'.
endform.
*&---------------------------------------------------------------------*
*& Form f_fieldcatalog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_fieldcatalog .

  free t_fieldcat[].

  perform f_estrutura_alv using:
 01  ''   ''   'IT_SAIDA'  'CHAVE_NFE  ' 'Chave Nfe                      '  '48'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'IT_SAIDA'  'DOCNUM     ' 'Docnum Nfe                     '  '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'IT_SAIDA'  'NFENUM     ' 'Número DANFE                   '  '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'IT_SAIDA'  'DOCDAT     ' 'Data Emissão                   '  '12'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'IT_SAIDA'  'WERKS      ' 'Código Filial embarque         '  '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''   ''   'IT_SAIDA'  'NAME       ' 'Desc.Filial Embarque           '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  ''   ''   'IT_SAIDA'  'STCD1      ' 'CNPJ Filial Embarque           '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',

 08  ''   ''   'IT_SAIDA'  'COD_COLETA ' 'Código Ponto Coleta            '  '12'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''   ''   'IT_SAIDA'  'DESC_COLETA' 'Desc.Ponto Coleta              '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 10  ''   ''   'IT_SAIDA'  'CNPJ_COLETA' 'CNPJ Ponto Coleta              '  '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',

 11  ''   ''   'IT_SAIDA'  'COD_LOC_DEST' 'Código local destino          '  '12'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 12  ''   ''   'IT_SAIDA'  'LOC_DEST   ' 'Desc.Local Destino             '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 13  ''   ''   'IT_SAIDA'  'CNPJ_DEST  ' 'CNPJ Local Destino             '  '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',

 14  ''   ''   'IT_SAIDA'  'PARID      ' 'Código Local Entrega/Transbordo'  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 15  ''   ''   'IT_SAIDA'  'NAME1      ' 'Desc.Local Entrega/Transbordo  '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 16  ''   ''   'IT_SAIDA'  'CNPJ_TRANSB' 'CNPJ Local Entrega/Transbordo  '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 17  ''   ''   'IT_SAIDA'  'QUANTIDADE ' 'Quantidade                     '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 18  ''   ''   'IT_SAIDA'  'NETWR      ' 'Valor                          '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 19  ''   ''   'IT_SAIDA'  'MATNR      ' 'Cód.Material                   '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 20  ''   ''   'IT_SAIDA'  'MAKTX      ' 'Desc.Material                  '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 21  ''   ''   'IT_SAIDA'  'PLACA_CAV  ' 'Placa do Cavalo                '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 22  ''   ''   'IT_SAIDA'  'PLACA_CAR1 ' 'Placa da Carreta1              '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 23  ''   ''   'IT_SAIDA'  'PLACA_CAR2 ' 'Placa da Carreta2              '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 24  ''   ''   'IT_SAIDA'  'PLACA_CAR3 ' 'Placa da Carreta3              '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',

 25  ''   ''   'IT_SAIDA'  'DOCNUM_CTE_P' 'Docnum CT-e                    '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 26  ''   ''   'IT_SAIDA'  'NUMR_CTE    ' 'Numero CT-e                    '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 27  ''   ''   'IT_SAIDA'  'DT_EMISSAO  ' 'Data Emissão CT-e              '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 28  ''   ''   'IT_SAIDA'  'EMIT_CNPJ   ' 'CNPJ Emitente                  '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 29  ''   ''   'IT_SAIDA'  'FORN_EMIT   ' 'Fornecedor Emitente            '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 30  ''   ''   'IT_SAIDA'  'INICIO_MUNI ' 'Inicio Prestação               '  '25'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
* 31  ''   ''   'IT_SAIDA'  'INICIO_UF   ' 'Desc.Material                  '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 32  ''   ''   'IT_SAIDA'  'TERMINO_MUNI' 'Término Prestação              '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
* 33  ''   ''   'IT_SAIDA'  'TERMINO_UF  ' 'Placa da Carreta1              '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 34  ''   ''   'IT_SAIDA'  'REME_CNPJ   ' 'REME CNPJ                      '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 35  ''   ''   'IT_SAIDA'  'REMETENTE   ' 'Remetente                      '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 36  ''   ''   'IT_SAIDA'  'DEST_CNPJ   ' 'CNPJ Destinatario              '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 37  ''   ''   'IT_SAIDA'  'DESTINATARIO' 'Destinatário                   '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 38  ''   ''   'IT_SAIDA'  'EXPED_CNPJ  ' 'CNPJ Expedidor                 '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 39  ''   ''   'IT_SAIDA'  'EXPEDIDOR   ' 'Expedidor                      '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 40  ''   ''   'IT_SAIDA'  'RECEB_CNPJ  ' 'CNPJ Recebedor                 '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 41  ''   ''   'IT_SAIDA'  'RECEBEDOR   ' 'Recebedor                      '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 42  ''   ''   'IT_SAIDA'  'TOMADORA_CNPJ' 'CNPJ Tomadora                 '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 43  ''   ''   'IT_SAIDA'  'TOMADORA    ' 'Filial Tomadora                '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 44  ''   ''   'IT_SAIDA'  'QT_CARGA_CTE' 'Quantidade CT-e                '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 45  ''   ''   'IT_SAIDA'  'SUBCONTRATADO' 'Subcontratado                 '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 46  ''   ''   'IT_SAIDA'  'SUBCONTRATADO_CNPJ' 'CNPJ Subcontratado       '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 47  ''   ''   'IT_SAIDA'  'PLACA_CAV_CTE' 'Placa Cav CT-e                '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 48  ''   ''   'IT_SAIDA'  'PLACA_CAR1_CTE' 'Placa Car1 CT-e              '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 49  ''   ''   'IT_SAIDA'  'PLACA_CAR2_CTE' 'Placa Car2 CT-e              '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 50  ''   ''   'IT_SAIDA'  'PLACA_CAR3_CTE' 'Placa Car3 CT-e              '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',

 51  ''   ''   'IT_SAIDA'  'DOCNUM_MDFE    ' 'Docnum MDFE                 '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 52  ''   ''   'IT_SAIDA'  'NMDFE          ' 'Número MDFE                 '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 53  ''   ''   'IT_SAIDA'  'DATA_EMI_MDFE  ' 'Data Emissão MDF-e          '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 54  ''   ''   'IT_SAIDA'  'CNPJ_EMI_MDFE  ' 'CNPJ Emissor MDF-e          '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 55  ''   ''   'IT_SAIDA'  'DESC_EMIT_MDFE ' 'Desc Emissor MDF-e          '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 56  ''   ''   'IT_SAIDA'  'PLACA_CAV_MDFE ' 'Placa Cavalo MDF-e          '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 57  ''   ''   'IT_SAIDA'  'PLACA_CAR1_MDFE' 'Placa Car1 MDF-e            '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 58  ''   ''   'IT_SAIDA'  'PLACA_CAR2_MDFE' 'Placa Car2 MDF-e            '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 59  ''   ''   'IT_SAIDA'  'PLACA_CAR3_MDFE' 'Placa Car3 MDF-e            '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 61  ''   ''   'IT_SAIDA'  'MOTORISTA      ' 'Motorista                   '  '40'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 62  ''   ''   'IT_SAIDA'  'TP_VEIC        ' 'Tipo de veiculo             '  '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 63  ''   ''   'IT_SAIDA'  'AGENT_FRETE     ' 'Agente de frete             '  '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

endform.
*&---------------------------------------------------------------------*
*& Form f_estrutura_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_01
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
form f_estrutura_alv  using value(p_col_pos)       type i                    "1
                           value(p_ref_tabname)   like dd02d-tabname        "2
                           value(p_ref_fieldname) like dd03d-fieldname      "3
                           value(p_tabname)       like dd02d-tabname        "4
                           value(p_field)         like dd03d-fieldname      "5
                           value(p_scrtext_l)     like dd03p-scrtext_l      "6
                           value(p_outputlen)                               "7
                           value(p_edit)                                    "8
                           value(p_sum)                                     "9
                           value(p_just)                                    "10
                           value(p_hotspot)                                 "11
                           value(p_f4)                                      "12
                           value(p_checkbox)                                "13
                           value(p_style)                                   "14
                           value(p_no_out)                                  "15
                           value(p_icon)                                    "16
                           value(p_fix).                                    "17

  clear w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.

  if w_fieldcat-fieldname = 'MATNR'.
    w_fieldcat-no_zero = abap_true.
  endif.


  append w_fieldcat to t_fieldcat.

endform.
*&---------------------------------------------------------------------*
*& Form fm_sel_dados_entrada
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form fm_sel_dados_entrada .

  free: it_saida.
  data: it_saida_aux type table of ty_saida.


  select * from zi_dados_receb_produto
  into corresponding fields of table @it_saida
  where docdat in @p_docdat
  and docnum in @p_docnum
  and parid in ( '0000000161', '0000001003' )
   and direct   =  '2'
   and parvw    =  'LR'
  and  docsta   =  '1'
  and inco1 <> 'FOB'.

  check it_saida is not initial.

  loop at it_saida assigning field-symbol(<ws_saida>).
    <ws_saida>-chave_nfe = |{ <ws_saida>-regio }{ <ws_saida>-nfyear }{ <ws_saida>-nfmonth }{ <ws_saida>-cnpj_active }{ <ws_saida>-model
   }{ <ws_saida>-serie }{ <ws_saida>-nfnum9 }{ <ws_saida>-docnum9 }{ <ws_saida>-cdv }|.
  endloop.

  "Seleção ponto de coleta.
  select a~docnum, a~parid, a~parvw, b~name1, b~stcd1, b~stcd2
  from j_1bnfnad as a
  left join lfa1 as b on b~lifnr = a~parid
  into corresponding fields of table @it_j_1bnfnad
  for all entries in @it_saida
  where docnum eq @it_saida-docnum
    and parvw eq 'PC'.

  "Seleção dados local destino.
  select a~docnum, a~parid, a~parvw, b~name1, b~stcd1, b~stcd2
  from j_1bnfnad as a
  left join kna1 as b on b~kunnr = a~parid
  into corresponding fields of table @it_local_dest
  for all entries in @it_saida
  where docnum eq @it_saida-docnum
    and parvw eq 'AG'.

  "Seleção dados veiculo.
  select distinct a~vbeln, c~ch_referencia, b~xblnr, c~placa_cav, c~placa_car1, c~placa_car2, c~placa_car3
  from vbfa as a
  inner join likp as b on b~vbeln eq a~vbelv
  inner join zsdt0001 as c on c~ch_referencia eq b~xblnr
  into corresponding fields of table @it_dados_veic
    for all entries in @it_saida
    where a~vbeln eq @it_saida-refkey+0(10)
      and a~vbtyp_v eq 'J'
      and a~vbtyp_n eq 'M'.


  "Seleção de dados CT-e.
  select distinct b~docnum_cte_p, a~n55_chave_acesso, b~numr_cte, b~dt_emissao, b~emit_cnpj, c~name1 as forn_emit,
    b~inicio_muni, b~inicio_uf, b~termino_muni, b~termino_uf, b~reme_cnpj, d~name1 as Remetente,
    b~dest_cnpj, e~name1 as destinatario, b~exped_cnpj, f~name1 as expedidor,
    b~receb_cnpj, j~name1 as recebedor, b~f_tomadora, i~name as tomadora, i~stcd1 as tomadora_cnpj, b~qt_carga_cte,
    b~emit_cpf, b~reme_cpf, b~dest_cpf, b~exped_cpf, b~dest_cpf,
    b~emit_rsocial, b~reme_rsocial, b~dest_rsocial, b~exped_rsocial, b~dest_rsocial
  from zib_cte_dist_n55 as a
  inner join zib_cte_dist_ter as b on b~cd_chave_cte eq a~cd_chave_cte
  left join lfa1 as c on c~stcd1 eq b~emit_cnpj
  left join lfa1 as d on d~stcd1 eq b~reme_cnpj
  left join lfa1 as e on e~stcd1 eq b~dest_cnpj
  left join lfa1 as f on f~stcd1 eq b~exped_cnpj
  left join lfa1 as j on j~stcd1 eq b~receb_cnpj
  left join j_1bbranch as i on i~branch eq b~f_tomadora
  into corresponding fields of table @it_dados_cte
  for all entries in @it_saida
  where n55_chave_acesso eq @it_saida-chave_nfe
    and b~cd_modal eq '01'
    and b~cd_tipo_servico eq '0'.

  if it_dados_cte is not initial.
*  Seleção de dados veiculo CT-e.
    select * from zcte_trans
    into table @data(it_dados_veic_cte)
      for all entries in @it_dados_cte
    where docnum eq @it_dados_cte-docnum_cte_p.
  endif.

  "Dados do MDF-e.
  select a~docnum, a~docnum_ref, b~nmdfe, b~data_emi, b~status, b~cancel, b~cnpj_emi, d~name1,
  c~placa_cav, c~placa_car1, c~placa_car2, c~placa_car3, c~motorista
  from zsdt0105 as a
  inner join zsdt0102 as b on b~docnum eq a~docnum_ref
  inner join lfa1 as d on d~stcd1 eq b~cnpj_emi
  inner join zsdt0118 as c on c~docnum eq b~docnum
  into corresponding fields of table @it_dados_mdfe
    for all entries in @it_dados_cte
    where a~docnum eq @it_dados_cte-docnum_cte_p
      and b~autorizado eq 'X'.

*    *  Se faturamento do frete com a frota , a MDf-e  é emitida sobre a NF
  select distinct a~docnum, a~docnum_ref, b~nmdfe, b~data_emi, b~status, b~cancel, b~cnpj_emi, d~name1,
    c~placa_cav, c~placa_car1, c~placa_car2, c~placa_car3, c~motorista
    from zsdt0105 as a
    inner join zsdt0102 as b on b~docnum eq a~docnum_ref
    inner join lfa1 as d on d~stcd1 eq b~cnpj_emi
    inner join zsdt0118 as c on c~docnum eq b~docnum
    into table @data(it_dados_mdfe_prop)
    for all entries in @it_saida
  where a~docnum   eq @it_saida-docnum
  and b~autorizado eq 'X'
  and b~estornado  eq ''
  and b~cancel     eq ''.


* Se não encontrar a Mdf-e sobre a NF na seleção acima , buscar no fluxo avulso
  select distinct a~docnum, a~docnum_ref, b~nmdfe, b~data_emi, b~status, b~cancel, b~cnpj_emi, d~name1,
  c~placa_cav, c~placa_car1, c~placa_car2, c~placa_car3, c~motorista
  from zsdt0241 as a
  inner join zsdt0102 as b on b~docnum eq a~docnum_ref
  inner join lfa1 as d on d~stcd1 eq b~cnpj_emi
  inner join zsdt0118 as c on c~docnum eq b~docnum
  into table @data(it_dados_mdfe_prop_av)
  for all entries in @it_saida
  where a~chave    eq @it_saida-chave_nfe
  and b~autorizado eq 'X'
  and b~estornado  eq ''
  and b~cancel     eq ''.

  sort: it_saida by docnum, it_j_1bnfnad by docnum, it_local_dest by docnum.

  loop at it_saida assigning field-symbol(<wa_saida>).
    clear: zv_tabix.

*    <wa_saida>-chave_nfe = |{ <wa_saida>-regio }{ <wa_saida>-nfyear }{ <wa_saida>-nfmonth }{ <wa_saida>-cnpj_active }{ <wa_saida>-model
*    }{ <wa_saida>-serie }{ <wa_saida>-nfnum9 }{ <wa_saida>-docnum9 }{ <wa_saida>-cdv }|.

    read table it_j_1bnfnad into data(wa_j_1bnfnad) with key docnum = <wa_saida>-docnum binary search.
    if sy-subrc eq 0.
      <wa_saida>-cod_coleta  = wa_j_1bnfnad-parid.
      <wa_saida>-desc_coleta = wa_j_1bnfnad-name1.
      if wa_j_1bnfnad-stcd1 is not initial.
        <wa_saida>-cnpj_coleta = wa_j_1bnfnad-stcd1.
      else.
        <wa_saida>-cnpj_coleta = wa_j_1bnfnad-stcd2.
      endif.
    endif.

    read table it_local_dest into data(wa_local_dest) with key docnum = <wa_saida>-docnum binary search.
    if sy-subrc eq 0.
      <wa_saida>-cod_loc_dest = wa_local_dest-parid.
      <wa_saida>-loc_dest     = wa_local_dest-name1.

      if wa_local_dest-stcd1 is not initial.
        <wa_saida>-cnpj_dest    = wa_local_dest-stcd1.
      else.
        <wa_saida>-cnpj_dest    = wa_local_dest-stcd2.
      endif.
    endif.

    read table it_dados_veic into data(wa_dados_veic) with key vbeln = <wa_saida>-refkey.
    if sy-subrc eq 0.
      <wa_saida>-placa_cav = wa_dados_veic-placa_cav.
      <wa_saida>-placa_car1 = wa_dados_veic-placa_car1.
      <wa_saida>-placa_car2 = wa_dados_veic-placa_car2.
      <wa_saida>-placa_car3 = wa_dados_veic-placa_car3.
    endif.

    "Dados cte.
    read table it_dados_cte into data(ws_dados_cte) with key n55_chave_acesso = <wa_saida>-chave_nfe.
    if sy-subrc eq 0.
      <wa_saida>-docnum_cte_p = ws_dados_cte-docnum_cte_p.
      <wa_saida>-numr_cte     = ws_dados_cte-numr_cte    .
      <wa_saida>-dt_emissao   = ws_dados_cte-dt_emissao  .

      if ws_dados_cte-emit_cnpj is not initial.
        <wa_saida>-emit_cnpj    = ws_dados_cte-emit_cnpj.
      else.
        <wa_saida>-emit_cnpj    = ws_dados_cte-emit_cpf .
      endif.

      <wa_saida>-forn_emit    = ws_dados_cte-forn_emit   .
      <wa_saida>-inicio_muni  = |{ ws_dados_cte-inicio_muni }-{ ws_dados_cte-inicio_uf }|.
*      <wa_saida>-inicio_uf    = ws_dados_cte-inicio_uf   .
      <wa_saida>-termino_muni = |{ ws_dados_cte-termino_muni }-{ ws_dados_cte-termino_uf }|.
*      <wa_saida>-termino_uf   = ws_dados_cte-termino_uf  .

      if ws_dados_cte-reme_cnpj is not initial and ws_dados_cte-reme_cnpj ne '00000000000000'.
        <wa_saida>-reme_cnpj    = ws_dados_cte-reme_cnpj   .
      else.
        <wa_saida>-reme_cnpj    = ws_dados_cte-reme_cpf   .
      endif.

      if ws_dados_cte-dest_cnpj is not initial and ws_dados_cte-dest_cnpj ne '00000000000000'.
        <wa_saida>-dest_cnpj    = ws_dados_cte-dest_cnpj   .
      else.
        <wa_saida>-dest_cnpj    = ws_dados_cte-dest_cpf   .
      endif.

      if ws_dados_cte-exped_cnpj is not initial and ws_dados_cte-exped_cnpj ne '00000000000000'.
        <wa_saida>-exped_cnpj   = ws_dados_cte-exped_cnpj  .
      else.
        <wa_saida>-exped_cnpj   = ws_dados_cte-exped_cpf  .
      endif.

      if ws_dados_cte-receb_cnpj is not initial and ws_dados_cte-receb_cnpj ne '00000000000000'.
        <wa_saida>-receb_cnpj   = ws_dados_cte-receb_cnpj  .
      else.
        <wa_saida>-receb_cnpj   = ws_dados_cte-receb_cpf  .
      endif.

*      if ws_dados_cte-tomadora_cnpj is not initial.
      <wa_saida>-tomadora_cnpj = ws_dados_cte-tomadora_cnpj.
*      else.
*        <wa_saida>-tomadora_cnpj = ws_dados_cte-tomadora_cpf.
*      endif.

      <wa_saida>-Remetente    = ws_dados_cte-reme_rsocial.
      <wa_saida>-destinatario = ws_dados_cte-dest_rsocial.
      <wa_saida>-expedidor    = ws_dados_cte-exped_rsocial.
      <wa_saida>-recebedor    = ws_dados_cte-receb_rsocial.
      <wa_saida>-f_tomadora   = ws_dados_cte-f_tomadora  .
      <wa_saida>-tomadora     = ws_dados_cte-tomadora    .
      <wa_saida>-qt_carga_cte = ws_dados_cte-qt_carga_cte.
    endif.

    loop at it_dados_veic_cte into data(wa_dados_veic_cte) where docnum = ws_dados_cte-docnum_cte_p.
      add 1 to zv_tabix.
*      if sy-subrc eq 0.
      <wa_saida>-subcontratado       =  wa_dados_veic_cte-prop_nome.
      if wa_dados_veic_cte-prop_cnpj is not initial.
        <wa_saida>-subcontratado_cnpj  =  wa_dados_veic_cte-prop_cnpj.
      else.
        <wa_saida>-subcontratado_cnpj  =  wa_dados_veic_cte-prop_cpf.
      endif.
      if wa_dados_veic_cte-tp_veiculo eq '0'.
        <wa_saida>-placa_cav_cte       =  wa_dados_veic_cte-pc_veiculo.
      else.

        if zv_tabix = '2' and wa_dados_veic_cte-tp_veiculo eq '1'.
          <wa_saida>-placa_car1_cte      =  wa_dados_veic_cte-pc_veiculo.
        endif.

        if zv_tabix = '3' and <wa_saida>-placa_car2_cte is initial and wa_dados_veic_cte-tp_veiculo eq '1'.
          <wa_saida>-placa_car2_cte      =  wa_dados_veic_cte-pc_veiculo.
        endif.

        if zv_tabix = '4' and <wa_saida>-placa_car3_cte is initial and wa_dados_veic_cte-tp_veiculo eq '1'.
          <wa_saida>-placa_car3_cte      =  wa_dados_veic_cte-pc_veiculo.
        endif.
      endif.

    endloop.

    "Dados MDF-e.
    read table it_dados_mdfe into data(ws_dados_mdfe) with key docnum = ws_dados_cte-docnum_cte_p.
    if sy-subrc eq 0.
      <wa_saida>-docnum_mdfe      = ws_dados_mdfe-docnum_ref.
      <wa_saida>-nmdfe            =  ws_dados_mdfe-nmdfe.
      <wa_saida>-data_emi_mdfe    =  ws_dados_mdfe-data_emi.
      <wa_saida>-cnpj_emi_mdfe    =  ws_dados_mdfe-cnpj_emi.
      <wa_saida>-desc_emit_mdfe   =  ws_dados_mdfe-name1.
      <wa_saida>-placa_cav_mdfe   =  ws_dados_mdfe-placa_cav.
      <wa_saida>-placa_car1_mdfe  =  ws_dados_mdfe-placa_car1.
      <wa_saida>-placa_car2_mdfe  =  ws_dados_mdfe-placa_car2.
      <wa_saida>-placa_car3_mdfe  =  ws_dados_mdfe-placa_car3.
      <wa_saida>-motorista        =  ws_dados_mdfe-motorista.
    else.
      "Se não achar o MDF-e vinculado ao CT-e, verificar se existe MDF-e vinculado a NF-e.
      clear: ws_dados_mdfe.
      read table it_dados_mdfe_prop into ws_dados_mdfe with key docnum = <wa_saida>-docnum.
      if sy-subrc eq 0.
        <wa_saida>-docnum_mdfe      =  ws_dados_mdfe-docnum_ref.
        <wa_saida>-nmdfe            =  ws_dados_mdfe-nmdfe.
        <wa_saida>-data_emi_mdfe    =  ws_dados_mdfe-data_emi.
        <wa_saida>-cnpj_emi_mdfe    =  ws_dados_mdfe-cnpj_emi.
        <wa_saida>-desc_emit_mdfe   =  ws_dados_mdfe-name1.
        <wa_saida>-placa_cav_mdfe   =  ws_dados_mdfe-placa_cav.
        <wa_saida>-placa_car1_mdfe  =  ws_dados_mdfe-placa_car1.
        <wa_saida>-placa_car2_mdfe  =  ws_dados_mdfe-placa_car2.
        <wa_saida>-placa_car3_mdfe  =  ws_dados_mdfe-placa_car3.
        <wa_saida>-motorista        =  ws_dados_mdfe-motorista.
      endif.

      "Verificar se tem MDF-e avulsa.
      clear: ws_dados_mdfe.
      read table it_dados_mdfe_prop_av into ws_dados_mdfe with key docnum = <wa_saida>-docnum.
      if sy-subrc eq 0.
        <wa_saida>-docnum_mdfe      =  ws_dados_mdfe-docnum_ref.
        <wa_saida>-nmdfe            =  ws_dados_mdfe-nmdfe.
        <wa_saida>-data_emi_mdfe    =  ws_dados_mdfe-data_emi.
        <wa_saida>-cnpj_emi_mdfe    =  ws_dados_mdfe-cnpj_emi.
        <wa_saida>-desc_emit_mdfe   =  ws_dados_mdfe-name1.
        <wa_saida>-placa_cav_mdfe   =  ws_dados_mdfe-placa_cav.
        <wa_saida>-placa_car1_mdfe  =  ws_dados_mdfe-placa_car1.
        <wa_saida>-placa_car2_mdfe  =  ws_dados_mdfe-placa_car2.
        <wa_saida>-placa_car3_mdfe  =  ws_dados_mdfe-placa_car3.
        <wa_saida>-motorista        =  ws_dados_mdfe-motorista.
      endif.
    endif.
    clear: ws_dados_cte, wa_dados_veic_cte, wa_dados_veic, wa_j_1bnfnad, wa_local_dest.
  endloop.
  call screen 0100.

endform.
