*&---------------------------------------------------------------------*
*& Report  ZLESR0138
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zlesr0138.

tables: vttk, vtpa, adrc, vttp, lips, makt, vfkp, bkpf, bsis, vbak, vbfa, j_1bnflin, j_1bnfdoc, zlest0002.

types: begin of ty_saida,
         tknum              type vttk-tknum, " Doc. Transp com 0 a esquerda
         tknum1             type vttk-tknum, " Doc. Transp
         fknum              type vfkp-fknum, " Doc. Custo com 0 a esquerda
         fknum1             type vfkp-fknum, " Doc. Custo
         tplst              type vttk-tplst, " Filial
         erdat              type vttk-erdat, " Data Documento
         matnr              type lips-matnr, " Cod. Produto
         matnr1             type lips-matnr, " Cod. Produto zeros a esquerda
         maktx              type makt-maktx, " Descrição Produto
         text1              type vttk-text1, " Placa Cavalo
         text2              type vttk-text2, " Carreta1
         text3              type vttk-text3, " Carreta2
         text4              type vttk-text4, " Carreta2
         placa_carreta      type vttk-text1, " Placa Cavalo
         carreta1           type vttk-text2, " Carreta1
         carreta2           type vttk-text3, " Carreta2
         carreta3           type vttk-text3, " Carreta3
         shtyp              type vttk-shtyp, " Tp. Transporte
         tdlnr              type vttk-tdlnr, " Agente Fretecom 0 a esquerda
         tdlnr1             type vttk-tdlnr, " Agente Frete
         lifnr              type vtpa-lifnr, " Origem - Código --- Código do parceiro PC com 0 a esquerda
         lifnr1             type vtpa-lifnr, " Origem - Código --- Código do parceiro PC
         desc_origem        type adrc-name1, " Origem - Descrição --- concatenar (ADRC-NAME1 - ADRC-CITY1) do parceiro PC
         kunnr              type vtpa-kunnr, "Destino - Código --- Código do parceiro LR com 0 a esquerda
         kunnr1             type vtpa-kunnr, "Destino - Código --- Código do parceiro LR
         desc_destino       type adrc-name1, "Destino - Descrição --- concatenar (ADRC-NAME1 - ADRC-CITY1) do parceiro LR
         tipo               type char15, " Tipo --- Próprio , Terceiros e Intercompany
         dt_protocolo       type zib_cte_dist_ter-dt_protocolo,
         hr_protocolo       type zib_cte_dist_ter-hr_protocolo,
         mwsbp              type vfkp-mwsbp, " Imp_Receita
         ntgew              type vfkp-netwr, " Kg Trasportados
         preco_tonelada_rs  type vfkp-netwr, " Preço por Ton_R$
         netwr              type vfkp-netwr, " Valor Total R$
         taxa_dolar         type p decimals 4, " Tx_Dólar
         dmbe2              type vfkp-netwr, " Valor Total USD
         bkpf_belnr         type bkpf-belnr, " Doc Cont  Custo
         belnr2             type bkpf-belnr, " Doc Cont  Receita
         docnum             type j_1bnfdoc-docnum, " Docnum_Receita
         nfenum             type j_1bnfdoc-nfenum, " Nr_Ct-e
         nfenum1            type j_1bnfdoc-nfenum, " Nr_Ct-e
*        hora                 TYPE zlest0039-hora, "Hora / 111592 CS2023000322 SIM4.0 - Incerir a hora na coluna Data Documento ZLES0177 PSA
         adrnr              type vtpa-adrnr,
         parvw              type vtpa-parvw,
         xblnr              type bkpf-xblnr,
         bukrs              type bkpf-bukrs,
         gjahr              type bkpf-gjahr,
         tp_veiculo         type char01,
         vbak_vbeln         type vbak-vbeln,
         vbfa_vbeln         type vbfa-vbeln,
         vbak_bukrs_vf      type vbak-bukrs_vf,
         vbak_auart         type vbak-auart,
         vbfa_erdat         type vbfa-erdat,
         vttp_vbeln         type vttp-vbeln,
         grupo_veiculo      type zlest0002-grupo,
         frota              type zlest0002-frota,
         grupo_produto      type lips-matwa,
         cod_motorista      type vtpa-lifnr,  "DO PARCEIRO MT
         nome_motorista     type adrc-name1, " DO PARCEIRO MT
         cpf_motorista      type lfa1-stcd1,

         bezei              type ttdst-bezei,  "Nome Filial
         munic_origem       type adrc-city1,   " Origem do parceiro PC
         uf_origem          type adrc-country, " UF do parceiro PC Origem
         munic_destino      type adrc-city1,   "Destino do parceiro LR
         uf_destino         type adrc-country, " UF do parceiro PC Destino
         ov_pedido          type lips-vgbel,   "OV do pedido
         tp_ov              type vbak-auart,   "Tipo de ordem de venda
         transgenese        type vbak-kvgr3,   "
         dt_descarga        type zlest0039-datachegada, " Data da descarga
         peso_descarga      type zlest0039-pesotransb, "Peso transporte
         cod_tran_efetivo   type zlest0039-transb_efetivo,  "Codigo do transporte efetivo
         descr_tran_efetivo type kna1-name1, "Descrição transporte efetivo
         docnum_mdfe        type zsdt0105-docnum_ref, "Documento referencia
         nr_mdfe            type zsdt0102-nmdfe, "Numero da MDFE
         ordem_car          type zsdt0001od-nr_ordem, "Ordem de carregamento
         dt_oc              type zsdt0001od-dt_emissao, " Data de emissão
         cd_chave_cte       type zib_cte_dist_n55-cd_chave_cte,   "Chave CTe.
         chave_nfe          type zib_cte_dist_n55-n55_chave_acesso, "Chave de acesso NFe.
         hora_mdfe          type uzeit, "RJF
         data_mdfe          type datum, "RJF
         kbetr              type konv-kbetr, " Valor Pedágio - RJF
         vlr_frete          type vfkp-netwr, " Valor Frete R$ - RJF
       end of ty_saida,

       begin of ty_saida_ordem_carregamento,
         nr_ordem             type zsdt0001od-nr_ordem,            "Nr. Od  Carreg.
         tp_status            type zsdt0001od-tp_status,           "Status_OD
         dt_emissao           type zsdt0001od-dt_emissao,          "Dt_emissao_OD
         dt_validade          type zsdt0001od-dt_validade,         "Dt_validade_OD
         id_bukrs             type zsdt0001od-id_bukrs,            "Empresa
         id_branch            type zsdt0001od-id_branch,           "Filial
         name                 type j_1bbranch-name,                "Descr_filial
         id_bukrs_ag          type zsdt0001od-id_bukrs_ag,         "Empresa_Transp
         id_branch_ag         type zsdt0001od-id_branch_ag,        "Filial_Transp
         id_local_coleta      type zsdt0001od-id_local_coleta,     "Cód_Coleta
         local_coleta_name1   type  lfa1-name1,                    "Descr. Ponto Coleta
         local_coleta_ort01   type  lfa1-ort01,                    "Munic. Ponto Coleta
         local_coleta_regio   type  lfa1-regio,                    "UF_PC
         id_local_descarga    type  zsdt0001od-id_local_descarga,  "Cód. Descarga
         descarga_kname1      type  kna1-name1,                    "Desc. Descarga
         descarga_ort01       type  kna1-ort01,                    "Munic. Descarga
         descarga_regio       type  kna1-regio,                    "UF_Descarga
         id_local_destino     type  zsdt0001od-id_local_destino,       "Cód. Destino
         local_destino_name1  type  lfa1-name1,                    "Desc. Destino
         local_destino_ort01  type  lfa1-ort01,                    "Munic. Destino
         local_destino_regio  type  lfa1-regio,                    "UF_Destino
         doc_transp           type  zsdt0001-doc_transp,           "Doc_transp
         tknum                type  zsdt0001-tknum,                "Doc_custo
         erdat                type  vttk-erdat,                    "Dt_doc_Transp
         peso_fiscal          type  zsdt0001-peso_fiscal,          "Peso_embarque
         datatransb           type  zlest0039-datatransb,          "Dt_descarga
         pesotransb           type  zlest0039-pesotransb,          "Peso_descarga
         transb_efetivo       type  zlest0039-transb_efetivo,      "Cód_transb_efetivo
         transb_efetivo_name1 type  kna1-name1,                    "Descr. Transb_efetivo
         ds_placa_trator      type  zsdt0001od-ds_placa_trator,    "Placa_Cavalo
         s_placa_reboq_1      type  zsdt0001od-ds_placa_reboq_1,   "Carreta_1
         ds_placa_reboq_2     type  zsdt0001od-ds_placa_reboq_2,   "Carreta_2
         ds_placa_reboq_3     type  zsdt0001od-ds_placa_reboq_3,   "Carreta_3
         grupo                type  zlest0002-grupo,               "Grupo
         frota                type  zlest0002-frota,               "Frota
         id_motorista         type  zsdt0001od-id_motorista,       "Cód+motorista
         motorista_stcd2      type lfa1-stcd2,                     "CPF_motorista
         motorista_name1      type lfa1-name1,                     "Nome_motorista
         nr_frete_comb        type  zsdt0001od-nr_frete_comb,      "Frete_Combinado
         id_ordem             type zsdt0001od-id_ordem,             "Nr_ordem
         cd_chave_cte         type zib_cte_dist_n55-cd_chave_cte,   "Chave CTe.
         chave_nfe            type zib_cte_dist_n55-n55_chave_acesso, "Chave de acesso NFe.
*         hora                 TYPE zlest0039-hora,                      "Hora / 111592 CS2023000322 SIM4.0 - Incerir a hora na coluna Data Documento ZLES0177 PSA
         nro_nf_prod          type j_1bdocnum,
       end of ty_saida_ordem_carregamento,


       begin of ty_zsdt0001od,
         nr_ordem          type zsdt0001od-nr_ordem,
         dt_validade       type zsdt0001od-dt_validade,
         dt_emissao        type zsdt0001od-dt_emissao,
         id_bukrs          type zsdt0001od-id_bukrs,
         id_branch         type zsdt0001od-id_branch,
         id_bukrs_ag       type zsdt0001od-id_bukrs_ag,
         id_branch_ag      type zsdt0001od-id_branch_ag,
         id_local_coleta   type zsdt0001od-id_local_coleta,
         id_local_destino  type zsdt0001od-id_local_destino,
         id_local_descarga type zsdt0001od-id_local_descarga,
         ds_placa_trator   type zsdt0001od-ds_placa_trator,
         ds_placa_reboq_1  type zsdt0001od-ds_placa_reboq_1,
         ds_placa_reboq_2  type zsdt0001od-ds_placa_reboq_2,
         ds_placa_reboq_3  type zsdt0001od-ds_placa_reboq_3,
         id_motorista      type zsdt0001od-id_motorista,
         nr_frete_comb     type zsdt0001od-nr_frete_comb,
         tp_status         type zsdt0001od-tp_status,
         id_ordem          type zsdt0001od-id_ordem,
       end of ty_zsdt0001od,

       begin of ty_vttk, "PRINCIOAL
         tknum type  vttk-tknum,
         shtyp type  vttk-shtyp,
         tplst type  vttk-tplst,
         tdlnr type  vttk-tdlnr,
         text1 type  vttk-text1,
         text2 type  vttk-text2,
         text3 type  vttk-text3,
         erdat type  vttk-erdat,
         xblnr type  bkpf-xblnr,
       end of ty_vttk,


       begin of ty_lfa1,
         lifnr type lfa1-lifnr,
         stcd1 type lfa1-stcd1,
       end of ty_lfa1,

       begin of ty_zlest0002,
         pc_veiculo type  zlest0002-pc_veiculo,
         grupo      type  zlest0002-grupo,
         frota      type  zlest0002-frota,
       end of ty_zlest0002,

       begin of ty_vtpa, "parceiros  do  transporte
         kunnr type vtpa-kunnr,
         lifnr type vtpa-lifnr,
         adrnr type vtpa-adrnr,
         parvw type vtpa-parvw,
         vbeln type vtpa-vbeln,
       end of ty_vtpa,

       begin of ty_adrc, " nome dos  parceiros
         name1 type adrc-name1,
         city1 type adrc-city1,
       end of ty_adrc,

       begin of ty_vttp, " Identificar material e quantidade transportada
         vbeln type vttp-vbeln,
         tknum type vttp-tknum,
       end of ty_vttp,

       begin of ty_lips,
         matnr type lips-matnr,
         "NTGEW TYPE LIPS-NTGEW,
         vbeln type lips-vbeln,
         brgew type lips-brgew,
         matwa type lips-matwa,
         vgbel type lips-vgbel,
         pstyv type lips-pstyv,
         matkl type lips-matkl,
       end of ty_lips,

       begin of ty_makt, "Descritivo do  material
         maktx type makt-maktx,
         matnr type makt-matnr,
       end of ty_makt,

       begin of ty_vfkp, "buscar  dados  de  custo e  valores  em BRL e impostos
         knumv type vfkp-knumv,
         fknum type vfkp-fknum,
         netwr type vfkp-netwr,
         mwsbp type vfkp-mwsbp,
         rebel type vfkp-rebel,
         fkpty type vfkp-fkpty,
       end of ty_vfkp,

       begin of ty_bkpf, "Buscar  valor em USD
         belnr type bkpf-belnr,
         bukrs type bkpf-bukrs,
         gjahr type bkpf-gjahr,
       end of ty_bkpf,

       begin of ty_bsis,
         dmbtr type bsis-dmbtr,
         dmbe2 type bsis-dmbe2,
       end of ty_bsis,

       begin of ty_vbak, "Documento Contábil Receita
         vbeln    type vbak-vbeln,
         bukrs_vf type vbak-bukrs_vf,
         auart    type vbak-auart,
       end of ty_vbak,

       begin of ty_vbfa,
         vbeln type char35,
         erdat type vbfa-erdat,
         awkey type bkpf-awkey,
       end of ty_vbfa,

       begin of ty_j_1bnflin, " Docnum da  Receita
         refkey type j_1bnflin-refkey,
         docnum type j_1bnflin-docnum,
       end of ty_j_1bnflin,

       begin of ty_j_1bnfdoc,
         nfenum type j_1bnfdoc-nfenum,
         docnum type j_1bnfdoc-docnum,
         belnr  type j_1bnfdoc-belnr,
       end of ty_j_1bnfdoc.


class lcl_timer definition deferred.
*
"DECLARA AS TABELAS TEMPORARIAS E AS WORK_AREAS PARA AS ESTRUTURAS DECLARADAS A CIMA
data: it_saida                    type table of ty_saida,
      it_zsdt0001                 type table of zsdt0001,
      it_zsdt0001od               type table of ty_zsdt0001od,
      it_saida_carregamento       type table of ty_saida_ordem_carregamento,
      it_saida_aux                type table of ty_saida,
      t_zsdt0001od                type table of zsdt0001od,
      it_vttk                     type table of  ty_vttk,
      it_vtpa                     type table of  ty_vtpa,
      it_sort_vtpa                type sorted table of ty_vtpa with non-unique default key,
      it_adrc                     type table of  ty_adrc,
      it_vttp                     type table of  ty_vttp,
      it_sort_vttp                type sorted table of ty_vttp with non-unique default key,
      it_lips                     type table of  ty_lips,
      t_vbak                      type table of  vbak,
      t_vbfa                      type table of  vbfa,
      t_j_1bnflin                 type table of j_1bnflin,
      it_makt                     type table of  ty_makt,
      it_vfkp                     type table of  ty_vfkp,
      it_bkpf                     type table of  ty_bkpf,
      it_bsis                     type table of  ty_bsis,
      it_vbak                     type table of  ty_vbak,
      it_vbfa                     type table of  ty_vbfa,
      it_j_1bnflin                type table of  ty_j_1bnflin,
      it_j_1bnfdoc                type table of  ty_j_1bnfdoc,
      it_zlest0002                type table of  ty_zlest0002,
      it_lfa1                     type table of ty_lfa1,
      wa_saida                    type ty_saida,
      wa_saida_ordem_carregamento type ty_saida_ordem_carregamento,
      wa_zsdt0001od               type ty_zsdt0001od,
      wa_vttp                     type ty_vttp,
      wa_lips                     type ty_lips,
      wa_makt                     type ty_makt,
      wa_vfkp                     type ty_vfkp,
      wa_bkpf                     type ty_bkpf,
      wa_bsis                     type ty_bsis,
      wa_vbak                     type ty_vbak,
      wa_vbfa                     type ty_vbfa,
      wa_j_1bnflin                type ty_j_1bnflin,
      wa_j_1bnfdoc                type ty_j_1bnfdoc,
      v_name1                     like adrc-name1,
      v_city1                     like adrc-city1,
      v_descricao                 type string,
      cpf_motorista(14)           type c.

data: gs_variant   type disvariant, " é para poder escolher o layout
      variante     like disvariant,
      gs_variant_c type disvariant,
      vs_refkey    type j_1bnflin-refkey.

data: r_refkey     type range of j_1brefkey,
      r_refkey_aux type range of j_1brefkey.

data: r_vbeln     type range of vbeln,
      r_vbeln_aux type range of vbeln.

* ALV

data:
  g_custom_container type ref to cl_gui_custom_container,
  dg_splitter_1      type ref to cl_gui_splitter_container,
  dg_parent_1        type ref to cl_gui_container,
  dg_splitter_2      type ref to cl_gui_splitter_container,
  dg_parent_2        type ref to cl_gui_container,
  dg_parent_2a       type ref to cl_gui_container,
  dg_parent_alv      type ref to cl_gui_container,
  picture            type ref to cl_gui_picture,
  ctl_alv            type ref to cl_gui_alv_grid,
  dg_dyndoc_id       type ref to cl_dd_document,
  table_element      type ref to cl_dd_table_element,
  column             type ref to cl_dd_area,
  table_element2     type ref to cl_dd_table_element,
  column_1           type ref to cl_dd_area,
  dg_html_cntrl      type ref to cl_gui_html_viewer,
  it_exclude_fcode   type ui_functions,
  wa_exclude_fcode   like line of it_exclude_fcode,
  gs_layout          type lvc_s_layo,
  it_fieldcatalog    type lvc_t_fcat,
  wa_fieldcatalog    type lvc_s_fcat,
  it_sort            type lvc_t_sort,
  ls_stable          type lvc_s_stbl,
  str                type ref to data,
  ob_timer           type ref to cl_gui_timer,
  ob_recev           type ref to lcl_timer,
  frota              type zlest0002-frota,
  grupo_veiculo      type zlest0002-grupo.

data: url(255)                type c,
      p_text                  type sdydo_text_element,
      sdydo_text_element(255),
      p_text_table            type sdydo_text_table,
      vl_cont                 type i,
      vl_butxt                type t001-butxt,
      vl_dates1               type char10,
      vl_dates2               type char10.

data: vg_repid   like sy-repid,
      vg_variant type disvariant.

class lcl_timer definition.
  public section.
    methods:
      handle_finished for event finished of cl_gui_timer.
endclass.



selection-screen begin of block b1 with frame  title text-001.

  select-options: p_erdat     for    vttk-erdat obligatory, "DATA
                  p_tknum   for   vttk-tknum, " DOC. TRANSP.
                  p_tplst for vttk-tplst, " FILIAL
                  p_placa for zlest0002-pc_veiculo. "placa

  selection-screen begin of line.

    parameters: p_prop radiobutton group r1 default 'X'."AS CHECKBOX.
    selection-screen comment 4(13) text-c02 for field p_prop. "P_EX_TES.
    selection-screen position 20.

    parameters: p_terc radiobutton group r1."AS CHECKBOX.
    selection-screen comment 24(15) text-c03 for field p_terc. "P_C_LANC.
    selection-screen position 40.
*
    parameters: p_carre radiobutton group r1."AS CHECKBOX.
    selection-screen comment 44(18) text-c04 for field p_carre. "P_C_LANC.
    selection-screen position 60.

  selection-screen end of line.

selection-screen end of block b1.

*------------------------------------------- LAYOUT ----------------------------------------------------------"

selection-screen: begin of block b2 with frame title text-002.
  parameters: p_varia type disvariant-variant.
selection-screen: end of block b2.


if variante is initial.
  variante-report = sy-repid.
  variante-variant = p_varia.
endif.


at selection-screen on value-request for p_varia.



  variante-report = sy-repid.
  variante-variant = p_varia.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant    = variante
      i_save        = 'A'
    importing
      es_variant    = variante
    exceptions
      not_found     = 1
      program_error = 2
      others        = 3.

  if ( sy-subrc ne 0 ).
    message s000(z01) with 'Não existe variante'.
    stop.
  else.
    move variante-variant to p_varia.
    move variante-variant to gs_variant_c-variant.
  endif.


* ------------------------------------------- FIM LAYOUT -------------------------------------------------------"

class lcl_timer implementation.
  method handle_finished.

    data: mensagem(30) type c.

    perform limpa_campos.
    perform busca_dados.

    concatenate 'Última Atualização: ' sy-uzeit(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2) into mensagem.

    message  mensagem type 'S' display like 'S'.


    call method ctl_alv->refresh_table_display
      exporting
        is_stable = ls_stable.
    call method ob_timer->run.
  endmethod.                    "handle_finished
endclass.

initialization.


start-of-selection.
  perform busca_dados.

  if p_carre ne 'X'.
    if it_saida[] is not initial or it_saida_aux[] is not initial.
      perform imprimir_alv.
    else.
      message 'Dados não encontrado' type 'S' display like 'E'.
    endif.
  else.
    if it_saida_carregamento[] is not initial.

      "...129367 ZLES0177 - Ord. Carregamento  - status - PSA
      "... CASO STATUS FE E DOC_TRANSP VAZIO

      loop at it_saida_carregamento[] assigning field-symbol(<find_nf_orig>) where doc_transp is initial and tp_status = 'FE'.

        data: aux_id_ordem type  zsdt0001od-id_ordem.
        clear: aux_id_ordem .
        if <find_nf_orig>-nr_ordem is not initial.
          select single id_ordem from zsdt0001od
            into  @aux_id_ordem
            where nr_ordem = @<find_nf_orig>-nr_ordem
            and dt_emissao = @<find_nf_orig>-dt_emissao
            and id_branch = @<find_nf_orig>-id_branch
            and ds_placa_trator = @<find_nf_orig>-ds_placa_trator.
        endif.

        data: aux_nro_nf_prod type j_1bnfdoc-docnum.
        clear: aux_nro_nf_prod.
        if aux_id_ordem is not initial.
          select single nro_nf_prod from zsdt0001
          into  @aux_nro_nf_prod
          where id_ordem = @aux_id_ordem.
        endif.

        if aux_nro_nf_prod is not initial . "SE EXISTIR O DOCNUM E FOR CANCELADO ALTERAR PARA ES

          data: orig_docnum type j_1bnfdoc-docnum.
          clear: orig_docnum.
          select single docnum from j_1bnfdoc where docnum = @aux_nro_nf_prod and cancel = 'X' into @orig_docnum.

          if orig_docnum is not initial.
            clear: <find_nf_orig>-tp_status.
            <find_nf_orig>-tp_status = 'ES'.
          endif.

        else.
          data: aux_vbeln type zlest0108-vbeln.
          data: aux_zlest0108_id_ordem type zlest0108-id_ordem.
          aux_zlest0108_id_ordem = aux_id_ordem.
          clear: aux_vbeln.
          select single vbeln from zlest0108 where id_ordem = @aux_zlest0108_id_ordem into @aux_vbeln. "VERIFICA A zlest0108

          if aux_vbeln is not initial.


            types: begin of ty_aux_likp,
                     parid  type j_1bnfdoc-parid,
                     nfenum type j_1bnfdoc-nfenum,
                     series type j_1bnfdoc-series,
                   end of ty_aux_likp.

            data: orig_likp type ty_aux_likp.

            clear: orig_likp.

            select single lifnr as parid,
          rtrim( ltrim( upper( substring( lifex ,1, ( instr( lifex ,'-' ) * 1  ) - 1 ) ), ' ' ) ,' ' ) as nfenum,
          rtrim( ltrim( upper( substring( lifex ,length( lifex ), ( instr( lifex ,'-' ) * 1  ) - 1 ) ), ' ' ) ,' ' ) as series
          from likp where vbeln = @aux_vbeln into @orig_likp.

            condense orig_likp-nfenum no-gaps.
            condense orig_likp-parid no-gaps.
            condense orig_likp-series no-gaps.


            data: aux_nfenum type j_1bnfdoc-nfenum.
            data: aux_parid type j_1bnfdoc-parid.
            data: aux_series type j_1bnfdoc-series.

            clear:aux_nfenum,aux_parid,aux_series.


            aux_series = orig_likp-series.
            aux_nfenum = orig_likp-nfenum.
            aux_parid = orig_likp-parid.

            data: orig_docnum2 type j_1bnfdoc-docnum.
            clear:aux_nfenum,aux_parid,aux_series.

            select single docnum from j_1bnfdoc where parid = @aux_parid and nfenum = @aux_nfenum and series = @aux_series and cancel = 'X' into @orig_docnum2.

            if orig_docnum2 is initial.
              clear: <find_nf_orig>-tp_status.
              <find_nf_orig>-tp_status = 'ES'.
            endif.

          else.

            clear: <find_nf_orig>-tp_status.
            <find_nf_orig>-tp_status = 'ES'.

          endif.


        endif.

      endloop.


      perform imprimir_alv_carregamento.
    else.
      message 'Dados não encontrado' type 'S' display like 'E'.
    endif.
  endif.

class lcl_event_handler definition.
  public section.
    class-methods:  on_hotspot_click for event hotspot_click of cl_gui_alv_grid
      importing e_column_id e_row_id es_row_no sender.


endclass.

class lcl_event_handler implementation.

  method on_hotspot_click.

    data: vbranch type j_1bbranch-branch.
    read table it_saida into data(wa_saida) index e_row_id-index.

    case e_column_id.
      when 'DOCNUM'.

        clear vbranch.

        vbranch = wa_saida-tdlnr+6(4).
        "VBRANCH = |{ VBRANCH ALPHA = IN }|.

        select single * from j_1bbranch into @data(wa_j_1bbranch)
          where branch eq @vbranch.

        set parameter id 'Z_MY_PARAMETER_1' field wa_saida-docnum.
        set parameter id 'Z_MY_PARAMETER_2' field wa_j_1bbranch-bukrs.
        call transaction 'ZCTE' and skip first screen.
    endcase.
  endmethod.

endclass.

form busca_dados.
  data: i_t_select    type sbiwa_t_select,
        wa_i_t_select type sbiwa_s_select.
* Inicio - LES - Replicação da ZLES0177 em uma tabela - FA - #94050 - 10.11.2022

*  TYPES: BEGIN OF ty_saida.
*           INCLUDE STRUCTURE zlest0227.
*           TYPES: hora TYPE zlest0039-hora.
*  TYPES END OF ty_saida.

  data: t_saida_final type table of zlest0227. "Hora / 111592 CS2023000322 SIM4.0 - Incerir a hora na coluna Data Documento ZLES0177 PSA

  data: r_erdat  type range of zlese0160,
        lw_erdat like line of r_erdat,
        r_tknum  type range of zlese0160,
        lw_tknum like line of r_tknum,
        r_tplst  type range of zlese0160,
        lw_tplst like line of r_tplst,
        r_placa  type range of zlese0160,
        lw_placa like line of r_placa.

  loop at p_erdat.
    clear wa_i_t_select.
    wa_i_t_select-fieldnm = 'erdat'.
    wa_i_t_select-sign    = p_erdat-sign.
    wa_i_t_select-option  = p_erdat-option.
    wa_i_t_select-low     = p_erdat-low.
    wa_i_t_select-high    = p_erdat-high.
    append wa_i_t_select to i_t_select.
    clear: p_erdat.
  endloop.

  loop at p_tknum.
    clear wa_i_t_select.
    wa_i_t_select-fieldnm = 'tknum'.
    wa_i_t_select-sign    = p_tknum-sign.
    wa_i_t_select-option  = p_tknum-option.
    wa_i_t_select-low     = p_tknum-low.
    wa_i_t_select-high    = p_tknum-high.
    append wa_i_t_select to i_t_select.
  endloop.

  loop at p_tplst.
    clear wa_i_t_select.
    wa_i_t_select-fieldnm = 'tplst'.
    wa_i_t_select-sign    = p_tplst-sign.
    wa_i_t_select-option  = p_tplst-option.
    wa_i_t_select-low     = p_tplst-low.
    wa_i_t_select-high    = p_tplst-high.
    append wa_i_t_select to i_t_select.
  endloop.

  loop at p_placa.
    clear wa_i_t_select.
    wa_i_t_select-fieldnm = 'placa'.
    wa_i_t_select-sign    = p_placa-sign.
    wa_i_t_select-option  = p_placa-option.
    wa_i_t_select-low     = p_placa-low.
    wa_i_t_select-high    = p_placa-high.
    append wa_i_t_select to i_t_select.
  endloop.

  call function 'ZLES0177_SELECIONAR_DADOS'
    exporting
      p_prop           = p_prop
      p_terc           = p_terc
      p_carre          = p_carre
    importing
      it_saida_carrega = it_saida_carregamento
    tables
      i_t_select       = i_t_select
      t_saida          = t_saida_final.


  loop at t_saida_final into data(ls_saida_final).
    if ls_saida_final-tipo+0(6) = 'Subcon' and ls_saida_final-data_mdfe is initial.
      ls_saida_final-data_mdfe = ls_saida_final-dt_protocolo.
      ls_saida_final-hora_mdfe = ls_saida_final-hr_protocolo.
* Início - 24/04/2024 - 2000005582/IR177751 - - Stefanini - Correção Valor Frete Subcontratos - PRB
      ls_saida_final-vlr_frete = ls_saida_final-netwr + ls_saida_final-kbetr.
* Fim    - 24/04/2024 - 2000005582/IR177751 - - Stefanini - Correção Valor Frete Subcontratos - PRB
    endif.
    move-corresponding ls_saida_final to wa_saida.
    append wa_saida to it_saida_aux.
    append wa_saida to it_saida.
    clear wa_saida.
  endloop.

*>>>>>>>>>>>>>Ajuste seleção MDF-e BUG #163799 / AOENNING
  if  p_tknum[] is not initial.
    sort it_saida by tknum.
    sort it_saida_aux by tknum.
    delete it_saida where tknum not in p_tknum.
    delete it_saida_aux where tknum not in p_tknum.
  endif.
*>>>>>>>>>>>>>Ajuste seleção MDF-e BUG #163799 / AOENNING


*  TYPES: ty_rg_text1    TYPE RANGE OF vttk-text1. " Placa Cavalo
*  TYPES: ty_rg_tknum    TYPE RANGE OF vttk-tknum. " Doc. Transp com 0 a esquerda
*  TYPES: ty_rg_vbeln    TYPE RANGE OF vbak-vbeln. " Ordem de venda
*  TYPES: ty_rg_bukrs_vf TYPE RANGE OF vbak-bukrs_vf. " Empresa
*  TYPES: ty_rg_docnum   TYPE RANGE OF j_1bnflin-docnum. " Docnum
*  DATA: zcl_faturamento TYPE REF TO zcl_faturamento.
*  DATA: message    TYPE itex132.
*
*  FREE: it_saida, it_saida_aux.
*
*  FREE zcl_faturamento.
*  CREATE OBJECT zcl_faturamento.
*
*  IF p_carre EQ 'X'.
*
*    IF p_tknum IS NOT INITIAL.
*      MESSAGE 'Não é possível selecionar ordens de carregamento informando Doc. de transporte' TYPE 'I' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*
*    SELECT
*      nr_ordem
*      dt_validade
*      dt_emissao
*      id_bukrs
*      id_branch
*      id_bukrs_ag
*      id_branch_ag
*      id_local_coleta
*      id_local_destino
*      id_local_descarga
*      ds_placa_trator
*      ds_placa_reboq_1
*      ds_placa_reboq_2
*      ds_placa_reboq_3
*      id_motorista
*      nr_frete_comb
*      tp_status
*      id_ordem
*    FROM
*      zsdt0001od
*    INTO TABLE it_zsdt0001od
*    WHERE
*      dt_emissao IN p_erdat AND
*      id_branch IN p_tplst AND
*      ds_placa_trator IN p_placa.
*
*    IF it_zsdt0001od[] IS NOT INITIAL.
*
*      "- Busca dados parceiro ponto de coleta
*      SELECT
*       name1,
*        ort01,
*        regio,
*        lifnr
*     FROM
*        lfa1
*     INTO TABLE
*        @DATA(it_lfa1_coleta)
*     FOR ALL ENTRIES IN
*        @it_zsdt0001od
*     WHERE
*        lifnr EQ @it_zsdt0001od-id_local_coleta.
*
*      "Busca dados do local de Descarga
*      SELECT
*        name1,
*         ort01,
*         regio,
*         kunnr
*      FROM
*         kna1
*      INTO TABLE
*         @DATA(it_kna1_descarga)
*      FOR ALL ENTRIES IN
*         @it_zsdt0001od
*      WHERE
*         kunnr EQ @it_zsdt0001od-id_local_descarga.
*
*      "Busca dados do local de destino
*      SELECT
*       name1,
*        ort01,
*        regio,
*        lifnr
*     FROM
*        lfa1
*     INTO TABLE
*        @DATA(it_lfa1_destino)
*     FOR ALL ENTRIES IN
*        @it_zsdt0001od
*     WHERE
*        lifnr EQ @it_zsdt0001od-id_local_destino.
*
*      "Busca dados de faturamento da ordem de carregamento
*      SELECT
*        id_ordem,
*        doc_transp,
*        fknum,
*        peso_fiscal,
*        doc_rem,
*        fatura_prod,
*        nro_nf_prod
*      FROM
*        zsdt0001
*     INTO TABLE
*         @DATA(it_zsdt0001_faturamento)
*      FOR ALL ENTRIES IN
*         @it_zsdt0001od
*      WHERE
*         tp_movimento = 'S' AND
*         id_ordem  EQ @it_zsdt0001od-id_ordem.
*
*
*      IF it_zsdt0001_faturamento[] IS NOT INITIAL.
*        "Busca data de criação da VT
*        SELECT
*          erdat,
*          tknum,
*          tplst
*        FROM
*           vttk
*        INTO TABLE
*            @DATA(it_vttk)
*         FOR ALL ENTRIES IN
*            @it_zsdt0001_faturamento
*         WHERE
*            tknum  EQ @it_zsdt0001_faturamento-doc_transp.
*
*        IF it_vttk IS NOT INITIAL.
*          "Seleção Nome Filial.
*          select distinct * FROM ttds
*            INTO TABLE @DATA(t_ttds)
*            FOR ALL ENTRIES IN  @it_vttk
*            WHERE tplst EQ @it_vttk-tplst.
*        ENDIF.
*
*        "Busca  data descarga, peso de descarga e transbordo efetivo
*        SELECT
*          pontotransb,
*          datatransb,
*          pesotransb,
*          datachegada,
*          pesochegada,
*          transb_efetivo,
*          docnum
*        FROM
*          zlest0039
*        INTO TABLE
*            @DATA(it_zlest0039)
*         FOR ALL ENTRIES IN
*            @it_zsdt0001_faturamento
*         WHERE
*            docnum  EQ @it_zsdt0001_faturamento-nro_nf_prod.
*
*        "Busca descrição Transbordo Efetivo
*        IF it_zlest0039 IS NOT INITIAL.
*          SELECT
*            name1,
*            kunnr
*          FROM
*            kna1
*          INTO TABLE
*            @DATA(it_kna1_transb_efetivo)
*          FOR ALL ENTRIES IN
*            @it_zlest0039
*          WHERE
*            kunnr EQ @it_zlest0039-transb_efetivo.
*        ENDIF.
*
*      ENDIF.
*
*      "Busca dados frota e grupo
*      SELECT
*        grupo,
*        frota,
*        pc_veiculo
*      FROM
*        zlest0002
*      INTO TABLE
*          @DATA(it_zlest0002)
*       FOR ALL ENTRIES IN
*          @it_zsdt0001od
*       WHERE
*          pc_veiculo EQ @it_zsdt0001od-ds_placa_trator.
*
*      "Busca dados Motorista
*      SELECT
*        name1,
*        stcd2,
*        lifnr
*      FROM
*        lfa1
*      INTO TABLE
*          @DATA(it_lfa1_motorista)
*       FOR ALL ENTRIES IN
*          @it_zsdt0001od
*       WHERE
*          lifnr EQ @it_zsdt0001od-id_motorista.
*
*      "Busca descrição filial
*      SELECT
*        name,
*        bukrs,
*        branch
*      FROM
*        j_1bbranch
*      INTO TABLE
*          @DATA(it_j_1bbranch)
*       FOR ALL ENTRIES IN
*          @it_zsdt0001od
*       WHERE
*          bukrs EQ @it_zsdt0001od-id_bukrs AND
*          branch EQ @it_zsdt0001od-id_branch.
*
*    ENDIF.
*
*    LOOP AT it_zsdt0001od[] INTO wa_zsdt0001od.
*
*      TRY.
*          zcl_faturamento->zif_faturamento~get_tipo_veiculo(
*            EXPORTING
*              i_placa = wa_zsdt0001od-ds_placa_trator
*              "I_TKNUM = I_TKNUM
*            IMPORTING
*              e_tipo = DATA(e_tipo_veiculo) ).
*          "E_PROPRIETARIO = DATA(E_PROPRIETARIO) ).
*        CATCH zcx_error .
*
*      ENDTRY.
*
*      IF e_tipo_veiculo EQ 'P'.
*
*
*        READ TABLE it_zsdt0001_faturamento INTO DATA(wa_zsdt0001) WITH KEY id_ordem  = wa_zsdt0001od-id_ordem.
*        IF sy-subrc EQ 0.
*
*          wa_saida_ordem_carregamento-doc_transp = wa_zsdt0001-doc_transp.
*          wa_saida_ordem_carregamento-tknum = wa_zsdt0001-fknum.
*          wa_saida_ordem_carregamento-peso_fiscal = wa_zsdt0001-peso_fiscal.
*
*          READ TABLE it_vttk INTO DATA(wa_vttk) WITH KEY tknum  = wa_zsdt0001-doc_transp.
*          IF sy-subrc EQ 0.
*
*            wa_saida_ordem_carregamento-erdat = wa_vttk-erdat.
*
*          ENDIF.
*          READ TABLE it_zlest0039 INTO DATA(wa_zlest0039) WITH KEY docnum  = wa_zsdt0001-nro_nf_prod.
*          IF sy-subrc EQ 0.
*
*            IF wa_zlest0039-pontotransb IS NOT INITIAL.
*
*              wa_saida_ordem_carregamento-datatransb = wa_zlest0039-datatransb.
*              wa_saida_ordem_carregamento-pesotransb = wa_zlest0039-pesotransb.
*
*            ELSE.
*
*              wa_saida_ordem_carregamento-datatransb = wa_zlest0039-datachegada.
*              wa_saida_ordem_carregamento-pesotransb = wa_zlest0039-pesochegada.
*
*            ENDIF.
*
*            wa_saida_ordem_carregamento-transb_efetivo = wa_zlest0039-transb_efetivo.
*
*            READ TABLE it_kna1_transb_efetivo INTO DATA(wa_kna1_transb_efetivo) WITH KEY kunnr  = wa_zlest0039-transb_efetivo.
*            IF sy-subrc EQ 0.
*
*              wa_saida_ordem_carregamento-transb_efetivo_name1 = wa_kna1_transb_efetivo-name1.
*
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*
*        wa_saida_ordem_carregamento-nr_ordem =   wa_zsdt0001od-nr_ordem.
*        wa_saida_ordem_carregamento-tp_status  =   wa_zsdt0001od-tp_status.
*        wa_saida_ordem_carregamento-dt_emissao =   wa_zsdt0001od-dt_emissao.
*        wa_saida_ordem_carregamento-dt_validade   =   wa_zsdt0001od-dt_validade.
*        wa_saida_ordem_carregamento-id_bukrs    =   wa_zsdt0001od-id_bukrs.
*        wa_saida_ordem_carregamento-id_branch   =   wa_zsdt0001od-id_branch.
*        wa_saida_ordem_carregamento-id_bukrs_ag =   wa_zsdt0001od-id_bukrs_ag.
*        wa_saida_ordem_carregamento-id_branch_ag =   wa_zsdt0001od-id_branch_ag.
*        wa_saida_ordem_carregamento-id_local_coleta =   wa_zsdt0001od-id_local_coleta.
*
*        READ TABLE it_j_1bbranch INTO DATA(wa_j_1bbranch) WITH KEY bukrs = wa_zsdt0001od-id_bukrs
*                                                                   branch = wa_zsdt0001od-id_branch.
*        IF sy-subrc EQ 0.
*
*          wa_saida_ordem_carregamento-name = wa_j_1bbranch-name.
*
*        ENDIF.
*
*        READ TABLE it_lfa1_coleta INTO DATA(wa_lfa1_coleta) WITH KEY lifnr = wa_zsdt0001od-id_local_coleta.
*
*        IF sy-subrc EQ 0.
*
*          wa_saida_ordem_carregamento-local_coleta_name1 = wa_lfa1_coleta-name1.
*          wa_saida_ordem_carregamento-local_coleta_ort01 = wa_lfa1_coleta-ort01.
*          wa_saida_ordem_carregamento-local_coleta_regio = wa_lfa1_coleta-regio.
*
*        ENDIF.
*
*
*        wa_saida_ordem_carregamento-id_local_descarga = wa_zsdt0001od-id_local_descarga.
*
*        READ TABLE it_kna1_descarga INTO DATA(wa_kna1_descarga) WITH KEY kunnr = wa_zsdt0001od-id_local_descarga.
*        IF sy-subrc EQ 0.
*
*          wa_saida_ordem_carregamento-descarga_kname1 = wa_kna1_descarga-name1.
*          wa_saida_ordem_carregamento-descarga_ort01 = wa_kna1_descarga-ort01.
*          wa_saida_ordem_carregamento-descarga_regio = wa_kna1_descarga-regio.
*
*        ENDIF.
*
*
*        wa_saida_ordem_carregamento-id_local_destino = wa_zsdt0001od-id_local_destino.
*
*        READ TABLE it_lfa1_destino INTO DATA(wa_lfa1_destino) WITH KEY lifnr = wa_zsdt0001od-id_local_destino.
*
*        IF sy-subrc EQ 0.
*
*          wa_saida_ordem_carregamento-local_destino_name1 = wa_lfa1_destino-name1.
*          wa_saida_ordem_carregamento-local_destino_ort01 = wa_lfa1_destino-ort01.
*          wa_saida_ordem_carregamento-local_destino_regio = wa_lfa1_destino-regio.
*
*        ENDIF.
*
*        wa_saida_ordem_carregamento-ds_placa_trator =   wa_zsdt0001od-ds_placa_trator.
*        wa_saida_ordem_carregamento-s_placa_reboq_1 =   wa_zsdt0001od-ds_placa_reboq_1.
*        wa_saida_ordem_carregamento-ds_placa_reboq_2 =   wa_zsdt0001od-ds_placa_reboq_2.
*        wa_saida_ordem_carregamento-ds_placa_reboq_3 =   wa_zsdt0001od-ds_placa_reboq_3.
*
*        READ TABLE it_zlest0002 INTO DATA(wa_zlest0002) WITH KEY pc_veiculo  = wa_zsdt0001od-ds_placa_trator.
*        IF sy-subrc EQ 0.
*
*          wa_saida_ordem_carregamento-grupo = wa_zlest0002-grupo.
*          wa_saida_ordem_carregamento-frota = wa_zlest0002-frota.
*
*        ENDIF.
*
*        wa_saida_ordem_carregamento-id_motorista = wa_zsdt0001od-id_motorista.
*
*        READ TABLE it_lfa1_motorista INTO DATA(wa_lfa1_motorista) WITH KEY lifnr = wa_zsdt0001od-id_motorista.
*        IF sy-subrc EQ 0.
*
*          wa_saida_ordem_carregamento-motorista_stcd2 = wa_lfa1_motorista-stcd2.
*          wa_saida_ordem_carregamento-motorista_name1 = wa_lfa1_motorista-name1.
*
*        ENDIF.
*
*
*        wa_saida_ordem_carregamento-nr_frete_comb = wa_zsdt0001od-nr_frete_comb.
*
*        APPEND wa_saida_ordem_carregamento TO it_saida_carregamento.
*        CLEAR: wa_saida_ordem_carregamento, wa_zsdt0001od.
*      ENDIF.
*    ENDLOOP.
*
*
*
*
*  ELSE.
*
*    SELECT erdat tknum shtyp tplst  tdlnr  text1 text2 text3 text4
*      FROM vttk
*     INTO CORRESPONDING FIELDS OF TABLE it_saida
*     WHERE erdat IN p_erdat
*       AND add03 EQ '0000000001'
*       AND tknum IN p_tknum
*       AND tplst IN p_tplst.
*
*    IF it_saida IS NOT INITIAL.
*      "Seleção Nome Filial.
*
*      select distinct * FROM ttdst
*        INTO TABLE @DATA(t_ttdst)
*        FOR ALL ENTRIES IN  @it_saida
*        WHERE tplst EQ @it_saida-tplst.
*
*
*      select distinct * FROM zsdt0001 INTO TABLE it_zsdt0001
*      FOR ALL ENTRIES IN it_saida
*      WHERE doc_transp EQ it_saida-tknum.
*
*      IF it_zsdt0001 IS NOT INITIAL.
*        select distinct * FROM zsdt0001od INTO TABLE t_zsdt0001od
*          FOR ALL ENTRIES IN it_zsdt0001
*          WHERE id_ordem EQ it_zsdt0001-id_ordem.
*      ENDIF.
*    ENDIF.
*
*    DATA(qtd_lines) = lines( it_saida ).
*    DATA qtd_ini TYPE sy-tabix.
*
*    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<wa_ajuste>).
*
*      qtd_ini = sy-tabix.
*
*      <wa_ajuste>-xblnr = <wa_ajuste>-tknum.
*      <wa_ajuste>-placa_carreta = <wa_ajuste>-text1(7).
*
*      CHECK <wa_ajuste>-placa_carreta IS NOT INITIAL.
*
*      CLEAR message.
*      message = | ( { qtd_ini } de { qtd_lines } ) - Checando Placa: { <wa_ajuste>-placa_carreta }|.
*
*      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*        EXPORTING
*          text = message.
*
*      TRY.
*          zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
*            EXPORTING
*              i_tknum                = <wa_ajuste>-tknum
*            IMPORTING
*              e_tipo_veiculo         = DATA(_tp_veiculo)
*              e_tipo_remetente       = DATA(_tp_remetente) ).
*        CATCH zcx_faturamento.
*        CATCH zcx_error.
*      ENDTRY.
*
*      <wa_ajuste>-tp_veiculo = _tp_veiculo.
*
*      CASE <wa_ajuste>-tp_veiculo.
*        WHEN 'P'.
*          <wa_ajuste>-tipo =
*          SWITCH #( _tp_remetente WHEN 'P' THEN 'PRÓPRIO'
*                                  WHEN 'T' THEN 'TERCEIRO'
*                                  WHEN 'I' THEN 'INTERCOMPANY'
*                   ).
*        WHEN 'T'.
*          <wa_ajuste>-tipo = 'TERCEIRO'.
*      ENDCASE.
*
*    ENDLOOP.
*
*    IF p_prop = 'X'.
*      DELETE it_saida WHERE tp_veiculo = 'T'.
*    ELSE.
*      DELETE it_saida WHERE tp_veiculo = 'P'.
*    ENDIF.
*
*    DELETE it_saida WHERE placa_carreta EQ ' '.
*
*    IF p_placa IS NOT INITIAL.
*      DELETE it_saida WHERE placa_carreta NOT IN p_placa.
*    ENDIF.
*
**    CHECK it_saida IS NOT INITIAL.
*    IF it_saida IS NOT INITIAL.
*
*      SELECT *
*      FROM vtpa
*        INTO CORRESPONDING FIELDS OF TABLE it_vtpa
*            FOR ALL ENTRIES IN it_saida
*      WHERE vbeln EQ it_saida-tknum
*      AND parvw IN ('PC' ,'LR', 'PV', 'MT').
*      SORT it_vtpa[] BY vbeln ASCENDING.
*      IF ( sy-subrc = 0 ).
*        it_sort_vtpa[] = it_vtpa[].
*      ENDIF.
*
*      SELECT *
*        FROM vttp
*      INTO CORRESPONDING FIELDS OF TABLE it_vttp
*              FOR ALL ENTRIES IN it_saida
*      WHERE tknum EQ it_saida-tknum.
*      SORT it_vttp[] BY vbeln ASCENDING.
*
*      IF it_vttp IS NOT INITIAL.
*
*        it_sort_vttp[] = it_vttp[].
*
*        SELECT *
*          FROM lips
*         INTO CORRESPONDING FIELDS OF TABLE it_lips
*            FOR ALL ENTRIES IN it_vttp
*         WHERE vbeln EQ it_vttp-vbeln
*         ORDER BY PRIMARY KEY.
*
*        IF it_lips IS NOT INITIAL.
*          FREE: t_vbak.
*          select distinct * FROM vbak
*            INTO CORRESPONDING FIELDS OF TABLE t_vbak
*            FOR ALL ENTRIES IN it_lips
*            WHERE vbeln EQ it_lips-vgbel.
*
**        r_vbeln = VALUE #( FOR l IN it_lips ( sign   = 'I' option = 'EQ' low    = l-vbeln ) ).
**        r_vbeln_aux = VALUE #( FOR t IN it_vttp ( sign   = 'I' option = 'EQ' low    = t-vbeln  ) ).
**        APPEND LINES OF r_vbeln_aux TO r_vbeln.
**        SORT r_vbeln BY low.
**        DELETE ADJACENT DUPLICATES FROM r_vbeln COMPARING low.
*
*          FREE: t_vbfa.
*          select distinct * FROM vbfa
*          INTO CORRESPONDING FIELDS OF TABLE t_vbfa
*            FOR ALL ENTRIES IN it_lips
*          WHERE vbelv EQ it_lips-vbeln.
*
*          IF t_vbfa IS NOT INITIAL.
*            r_refkey = VALUE #( FOR g IN t_vbfa ( sign   = 'I' option = 'EQ' low    = g-vbeln ) ).
*            r_refkey_aux = VALUE #( FOR s IN t_vbfa ( sign   = 'I' option = 'EQ' low    = |{ s-vbeln }{ s-mjahr }|  ) ).
*            APPEND LINES OF r_refkey_aux TO r_refkey.
*            SORT r_refkey BY low.
*            DELETE ADJACENT DUPLICATES FROM r_refkey COMPARING low.
*
*            select distinct * FROM j_1bnflin INTO TABLE t_j_1bnflin
*            WHERE refkey IN r_refkey.
*
*
*            select distinct * FROM j_1bnfe_active
*                INTO TABLE @DATA(t_j_1bnfe_active)
*              FOR ALL ENTRIES IN @t_j_1bnflin
*                WHERE docnum EQ @t_j_1bnflin-docnum
*                 AND docsta EQ '1'
*                 AND cancel EQ @space.
*
*
*            IF t_j_1bnfe_active IS NOT INITIAL.
*              select distinct * FROM zsdt0105 INTO TABLE @DATA(t_zsdt0105)
*                FOR ALL ENTRIES IN @t_j_1bnfe_active
*                WHERE docnum EQ @t_j_1bnfe_active-docnum.
*
*              IF t_zsdt0105 IS NOT INITIAL.
*                select distinct * FROM zsdt0102 INTO TABLE @DATA(t_zsdt0102)
*                FOR ALL ENTRIES IN @t_zsdt0105
*                WHERE docnum EQ @t_zsdt0105-docnum_ref
*                  AND autorizado EQ 'X'
*                  AND estornado  EQ @space
*                  AND cancel     EQ @space.
*              ENDIF.
*
*
*              select distinct * FROM zlest0039
*                INTO TABLE @DATA(t_zlest0039)
*                FOR ALL ENTRIES IN @t_j_1bnfe_active
*                WHERE docnum EQ @t_j_1bnfe_active-docnum.
*
*              IF t_zlest0039 IS NOT INITIAL.
*                SELECT name1, kunnr FROM kna1
*               INTO TABLE @DATA(it_kna1)
*               FOR ALL ENTRIES IN @t_zlest0039
*                  WHERE kunnr EQ @t_zlest0039-transb_efetivo.
*
*              ENDIF.
*            ENDIF.
*
*          ENDIF.
*        ENDIF.
*
*        SELECT *
*          FROM makt
*          INTO CORRESPONDING FIELDS OF TABLE it_makt
*          FOR ALL ENTRIES IN it_lips
*          WHERE matnr = it_lips-matnr
*            AND spras = sy-langu
*            ORDER BY PRIMARY KEY.
*
*      ENDIF.
*
*      DATA(rg_text1) = VALUE ty_rg_text1( FOR lwa_saida IN it_saida[] (
*          sign   = 'I'
*          option = 'EQ'
*          low    = lwa_saida-text1(7)
*          high   = lwa_saida-text1(7) )  ).
*      SORT rg_text1[] BY low ASCENDING.
*      DELETE ADJACENT DUPLICATES FROM rg_text1[] COMPARING low.
*
*      DATA(rg_tknum) = VALUE ty_rg_tknum( FOR lwa_saida IN it_saida[] (
*          sign   = 'I'
*          option = 'EQ'
*          low    = lwa_saida-tknum
*          high   = lwa_saida-tknum )  ).
*      SORT rg_tknum[] BY low ASCENDING.
*      DELETE ADJACENT DUPLICATES FROM rg_tknum[] COMPARING low.
*
*      SELECT pc_veiculo, frota, grupo
*          FROM zlest0002
*          INTO TABLE @DATA(lt_zlest0002)
*        WHERE pc_veiculo IN @rg_text1
*        ORDER BY pc_veiculo ASCENDING.
*
*      SELECT tknum, vbeln, bukrs_vf, auart, kvgr3
*        FROM vbak
*        INTO TABLE @DATA(lt_vbak)
*        WHERE tknum IN @rg_tknum
*        ORDER BY tknum ASCENDING.
*
*      IF ( lt_vbak[] IS NOT INITIAL ).
*
*        DATA(rg_vbeln) = VALUE ty_rg_vbeln( FOR lwa_vbak IN lt_vbak[] (
*          sign   = 'I' option = 'EQ' low = lwa_vbak-vbeln high = lwa_vbak-vbeln )  ).
*        SORT rg_vbeln[] BY low ASCENDING.
*        DELETE ADJACENT DUPLICATES FROM rg_vbeln[] COMPARING low.
*
*        DATA(rg_bukrs_vf) = VALUE ty_rg_bukrs_vf( FOR lwa_vbak IN lt_vbak[] (
*          sign   = 'I' option = 'EQ' low = lwa_vbak-bukrs_vf high = lwa_vbak-bukrs_vf )  ).
*        SORT rg_bukrs_vf[] BY low ASCENDING.
*        DELETE ADJACENT DUPLICATES FROM rg_bukrs_vf[] COMPARING low.
*
*        SELECT vbeln, erdat, vbtyp_n, vbtyp_v, mjahr, vbelv FROM vbfa
*            INTO TABLE @DATA(lt_vbfa)
*            WHERE vbelv IN @rg_vbeln[]
*            AND vbtyp_n = 'M'
*          ORDER BY vbeln ASCENDING.
*
*        IF ( sy-subrc = 0 ).
*
*          DATA(rg_vbeln_vbfa) = VALUE ty_rg_vbeln( FOR lwa_vbfa IN lt_vbfa[] (
*            sign   = 'I' option = 'EQ' low = lwa_vbfa-vbeln high = lwa_vbfa-vbeln )  ).
*          SORT rg_vbeln_vbfa[] BY low ASCENDING.
*          DELETE ADJACENT DUPLICATES FROM rg_vbeln_vbfa[] COMPARING low.
*
*          SELECT refkey, docnum FROM j_1bnflin
*              INTO TABLE @DATA(lt_j_1bnflin)
*            WHERE refkey IN @rg_vbeln_vbfa[] AND reftyp EQ 'BI'
*          ORDER BY refkey ASCENDING.
*
*
*          IF lt_j_1bnflin IS NOT INITIAL.
*            select distinct * FROM zsdt0105 INTO TABLE @DATA(it_zsdt0105)
*              FOR ALL ENTRIES IN @lt_j_1bnflin
*              WHERE docnum EQ @lt_j_1bnflin-docnum.
*
*            IF it_zsdt0105 IS NOT INITIAL.
*              select distinct * FROM zsdt0102 INTO TABLE @DATA(it_zsdt0102)
*              FOR ALL ENTRIES IN @it_zsdt0105
*              WHERE docnum EQ @it_zsdt0105-docnum_ref
*                AND autorizado EQ 'X'
*                AND estornado  EQ @space
*                AND cancel     EQ @space.
*            ENDIF.
*
*
*
*
*            select distinct * FROM zlest0039
*              INTO TABLE @DATA(tl_zlest0039)
*               FOR ALL ENTRIES IN @lt_j_1bnflin
*              WHERE docnum EQ @lt_j_1bnflin-docnum.
*
*            IF it_zlest0039 IS NOT INITIAL.
*              SELECT name1, kunnr FROM kna1
*             INTO TABLE @DATA(tl_kna1)
*             FOR ALL ENTRIES IN @tl_zlest0039
*                WHERE kunnr EQ @tl_zlest0039-transb_efetivo.
*
*            ENDIF.
*          ENDIF.
*
*
*          IF ( sy-subrc = 0 ).
*
*            DATA(rg_docnum) = VALUE ty_rg_docnum( FOR lwa_j_1bnflin IN lt_j_1bnflin[] (
*            sign   = 'I' option = 'EQ' low = lwa_j_1bnflin-docnum high = lwa_j_1bnflin-docnum )  ).
*            SORT rg_docnum[] BY low ASCENDING.
*            DELETE ADJACENT DUPLICATES FROM rg_docnum[] COMPARING low.
*
*            SELECT docnum, nfenum, belnr
*                  FROM j_1bnfdoc
*                INTO TABLE @DATA(lt_j_1bnfdoc)
*                  WHERE docnum  IN @rg_docnum[]
*                    AND cancel  EQ ''
*            ORDER BY docnum.
*
*
*
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*      IF ( it_vtpa[] IS NOT INITIAL ).
*        SELECT addrnumber, name1, city1, country, region
*              FROM adrc
*              INTO TABLE @DATA(lt_adrc)
*              FOR ALL ENTRIES IN @it_vtpa[]
*            WHERE addrnumber  = @it_vtpa-adrnr.
*        SORT lt_adrc[] BY addrnumber ASCENDING.
*
*        SELECT lifnr, stcd2 FROM lfa1
*              INTO TABLE @DATA(lt_lfa1)
*              FOR ALL ENTRIES IN @it_vtpa[]
*             WHERE lifnr = @it_vtpa-lifnr.
*        SORT lt_lfa1[] BY lifnr ASCENDING.
*      ENDIF.
*
*      LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<wa_saida_simplicada>).
*
****    SELECT SINGLE frota grupo
****      FROM zlest0002
****      INTO (frota, grupo_veiculo)
****    WHERE pc_veiculo = <wa_saida_simplicada>-text1.
*
*        READ TABLE lt_zlest0002[] ASSIGNING FIELD-SYMBOL(<lfs_zlest0002>) WITH KEY pc_veiculo = <wa_saida_simplicada>-text1 BINARY SEARCH.
*
*        IF sy-subrc IS INITIAL.
*          <wa_saida_simplicada>-frota = <lfs_zlest0002>-frota.
*          <wa_saida_simplicada>-grupo_veiculo = <lfs_zlest0002>-grupo.
*        ENDIF.
*
*        READ TABLE t_ttdst ASSIGNING FIELD-SYMBOL(<ws_ttdst>) WITH KEY tplst = <wa_saida_simplicada>-tplst.
*        IF sy-subrc EQ 0.
*          <wa_saida_simplicada>-bezei = <ws_ttdst>-bezei.
*        ENDIF.
*
*        READ TABLE it_zsdt0001 ASSIGNING FIELD-SYMBOL(<ws_zsdt0001>) WITH KEY doc_transp = <wa_saida_simplicada>-tknum.
*        IF sy-subrc EQ 0.
*          READ TABLE t_zsdt0001od ASSIGNING FIELD-SYMBOL(<ws_zsdt0001od>) WITH KEY id_ordem = <ws_zsdt0001>-id_ordem.
*          IF sy-subrc EQ 0.
*            <wa_saida_simplicada>-ordem_car = <ws_zsdt0001od>-nr_ordem.
*            <wa_saida_simplicada>-dt_oc     = <ws_zsdt0001od>-dt_emissao.
*          ENDIF.
*        ENDIF.
*
**    SELECT SINGLE BELNR BUKRS GJAHR FROM BKPF INTO
**    CORRESPONDING FIELDS OF WA_BKPF
**    WHERE XBLNR = <WA_SAIDA_SIMPLICADA>-XBLNR
**     AND BUDAT >= <WA_SAIDA_SIMPLICADA>-ERDAT.
**
**    <WA_SAIDA_SIMPLICADA>-BUKRS = WA_BKPF-BUKRS.
**    <WA_SAIDA_SIMPLICADA>-GJAHR = WA_BKPF-GJAHR.
**
**    SELECT SINGLE DMBE2 FROM BSIS INTO CORRESPONDING FIELDS OF WA_BSIS
**    WHERE BELNR = WA_BKPF-BELNR
**     AND BUKRS = <WA_SAIDA_SIMPLICADA>-BUKRS
**     AND GJAHR = <WA_SAIDA_SIMPLICADA>-GJAHR
**     AND SHKZG = 'S'.
**
**    <WA_SAIDA_SIMPLICADA>-DMBE2 = WA_BSIS-DMBE2.
**
**    IF <WA_SAIDA_SIMPLICADA>-NETWR > 0 AND <WA_SAIDA_SIMPLICADA>-NTGEW > 0.
**      <WA_SAIDA_SIMPLICADA>-PRECO_TONELADA_RS = ( <WA_SAIDA_SIMPLICADA>-NETWR / <WA_SAIDA_SIMPLICADA>-NTGEW ) * 1000.
**    ENDIF.
**
**    IF <WA_SAIDA_SIMPLICADA>-NETWR > 0 AND <WA_SAIDA_SIMPLICADA>-DMBE2 > 0.
**      <WA_SAIDA_SIMPLICADA>-TAXA_DOLAR = <WA_SAIDA_SIMPLICADA>-NETWR / <WA_SAIDA_SIMPLICADA>-DMBE2.
**    ENDIF.
*
*        <wa_saida_simplicada>-placa_carreta = <wa_saida_simplicada>-text1(7).
*        <wa_saida_simplicada>-carreta1 = <wa_saida_simplicada>-text2(7).
*        <wa_saida_simplicada>-carreta2 = <wa_saida_simplicada>-text3(7).
*        <wa_saida_simplicada>-carreta3 = <wa_saida_simplicada>-text4(7).
*
**      " está retindo muito o WA_VBAK-VBELN.
****    SELECT SINGLE vbeln bukrs_vf auart FROM vbak
****    INTO CORRESPONDING FIELDS OF wa_vbak
****    WHERE tknum EQ <wa_saida_simplicada>-tknum.
**      AND AUART NE 'ZTRO'.
*
*        READ TABLE lt_vbak ASSIGNING FIELD-SYMBOL(<lfs_vbak>) WITH KEY tknum = <wa_saida_simplicada>-tknum BINARY SEARCH.
*        IF ( sy-subrc = 0 ).
*          wa_vbak = CORRESPONDING #( <lfs_vbak> ).
*        ENDIF.
*
*        IF wa_vbak IS NOT INITIAL.
*
*          <wa_saida_simplicada>-vbak_vbeln =    wa_vbak-vbeln.
*          <wa_saida_simplicada>-vbak_bukrs_vf = wa_vbak-bukrs_vf.
*          <wa_saida_simplicada>-vbak_auart =    wa_vbak-auart.
*
****      SELECT SINGLE vbeln erdat FROM vbfa
****      INTO CORRESPONDING FIELDS OF wa_vbfa
****      WHERE vbelv EQ <wa_saida_simplicada>-vbak_vbeln
****        AND vbtyp_n = 'M'.
*
*          READ TABLE lt_vbfa ASSIGNING FIELD-SYMBOL(<lfs_vbfa>) WITH KEY vbeln = <wa_saida_simplicada>-vbak_vbeln BINARY SEARCH.
*          IF ( sy-subrc = 0 ).
*            wa_vbfa = CORRESPONDING #( <lfs_vbfa> ).
*          ENDIF.
*
*          IF wa_vbfa IS NOT INITIAL.
*
***        Adicionando zeros as esquerdas.
*            "WA_VBFA-VBELN = |{ WA_VBFA-VBELN ALPHA = IN }|.
*            <wa_saida_simplicada>-vbfa_vbeln = wa_vbfa-vbeln.
*            <wa_saida_simplicada>-vbfa_erdat = wa_vbfa-erdat.
*
*            SELECT SINGLE belnr bukrs gjahr FROM bkpf INTO CORRESPONDING FIELDS OF wa_bkpf
*            WHERE  awkey EQ <wa_saida_simplicada>-vbfa_vbeln
*            AND bukrs EQ <wa_saida_simplicada>-vbak_bukrs_vf
*            AND gjahr EQ <wa_saida_simplicada>-vbfa_erdat(4).
*
*            IF wa_bkpf IS NOT INITIAL.
*
*              <wa_saida_simplicada>-belnr2 = wa_bkpf-belnr.
*
*              READ TABLE lt_j_1bnflin[] ASSIGNING FIELD-SYMBOL(<lfs_j_1bnflin>) WITH KEY refkey = <wa_saida_simplicada>-vbfa_vbeln BINARY SEARCH.
*              IF ( sy-subrc = 0 ).
*                wa_j_1bnflin = CORRESPONDING #( <lfs_j_1bnflin> ).
*              ENDIF.
*
****          SELECT SINGLE  docnum FROM j_1bnflin INTO CORRESPONDING FIELDS OF wa_j_1bnflin
****          WHERE refkey EQ <wa_saida_simplicada>-vbfa_vbeln AND reftyp EQ 'BI'.
*
*              IF wa_j_1bnflin IS NOT INITIAL.
*
*                <wa_saida_simplicada>-docnum = wa_j_1bnflin-docnum.
*
****            SELECT SINGLE nfenum belnr
****              FROM j_1bnfdoc
****            INTO CORRESPONDING FIELDS OF wa_j_1bnfdoc
****              WHERE docnum  EQ <wa_saida_simplicada>-docnum
****                AND cancel  EQ ''.
*
*                READ TABLE lt_j_1bnfdoc[] ASSIGNING FIELD-SYMBOL(<lfs_j_1bnfdoc>) WITH KEY docnum = <wa_saida_simplicada>-docnum BINARY SEARCH.
*                IF ( sy-subrc = 0 ).
*                  wa_j_1bnfdoc = CORRESPONDING #( <lfs_j_1bnfdoc> ).
*                ENDIF.
*
*                IF wa_j_1bnfdoc IS NOT INITIAL.
*                  <wa_saida_simplicada>-nfenum = wa_j_1bnfdoc-nfenum.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
*
*
*
*        CLEAR: wa_vbak, wa_vbfa, wa_bkpf, wa_j_1bnflin, wa_j_1bnfdoc.
*
*        "Loop para pegar a origem e destino
****    LOOP AT it_vtpa ASSIGNING FIELD-SYMBOL(<w_vtpa>) WHERE vbeln EQ <wa_saida_simplicada>-tknum.
*        LOOP AT it_sort_vtpa[] ASSIGNING FIELD-SYMBOL(<w_vtpa>) WHERE vbeln EQ <wa_saida_simplicada>-tknum.
*
*          <wa_saida_simplicada>-parvw = <w_vtpa>-parvw.
*          <wa_saida_simplicada>-adrnr = <w_vtpa>-adrnr.
*
****      SELECT SINGLE name1 city1
****          FROM adrc
****        INTO (v_name1, v_city1)
****        WHERE addrnumber  = <wa_saida_simplicada>-adrnr.
*
*          READ TABLE lt_adrc[] ASSIGNING FIELD-SYMBOL(<lfs_adrc>) WITH KEY addrnumber  = <wa_saida_simplicada>-adrnr BINARY SEARCH.
*          IF ( sy-subrc = 0 ).
*            CONCATENATE <lfs_adrc>-name1 '-' <lfs_adrc>-city1 INTO v_descricao SEPARATED BY space.
*          ENDIF.
*
****      CONCATENATE v_name1 '-' v_city1 INTO v_descricao SEPARATED BY space.
*
*          IF <w_vtpa>-parvw = 'LR'.
*            <wa_saida_simplicada>-kunnr = <w_vtpa>-kunnr.
*            <wa_saida_simplicada>-desc_destino  = <lfs_adrc>-name1.  "v_descricao.
*            <wa_saida_simplicada>-munic_destino = <lfs_adrc>-city1.
*            <wa_saida_simplicada>-uf_destino    = <lfs_adrc>-region.
*          ELSEIF <w_vtpa>-parvw = 'PC'.
*            <wa_saida_simplicada>-lifnr = <w_vtpa>-lifnr.
*            <wa_saida_simplicada>-desc_origem  = <lfs_adrc>-name1.  "v_descricao.
*            <wa_saida_simplicada>-munic_origem = <lfs_adrc>-city1.
*            <wa_saida_simplicada>-uf_origem    = <lfs_adrc>-region.
*          ELSEIF <w_vtpa>-parvw = 'MT'.
*
*            <wa_saida_simplicada>-cod_motorista = <w_vtpa>-lifnr.
*            <wa_saida_simplicada>-nome_motorista = <lfs_adrc>-name1."v_name1.
*
****        SELECT SINGLE stcd2 FROM lfa1
****          INTO cpf_motorista
****         WHERE lifnr = <w_vtpa>-lifnr.
*
****        <wa_saida_simplicada>-cpf_motorista = cpf_motorista.
*
*            READ TABLE lt_lfa1[] ASSIGNING FIELD-SYMBOL(<lfs_lfa1>) WITH KEY lifnr = <w_vtpa>-lifnr BINARY SEARCH.
*            IF ( sy-subrc = 0 ).
*              <wa_saida_simplicada>-cpf_motorista = <lfs_lfa1>-stcd2.
*            ENDIF.
*
*          ENDIF.
*
*        ENDLOOP.
*
*        SELECT SINGLE *
*          FROM vfkp
*        INTO CORRESPONDING FIELDS OF wa_vfkp
*          WHERE rebel = <wa_saida_simplicada>-tknum
*            AND fkpty = 'Z001'.
*
*        <wa_saida_simplicada>-fknum =  wa_vfkp-fknum.
*        IF <wa_saida_simplicada>-tipo <> 'PRÓPRIO'.
*          <wa_saida_simplicada>-mwsbp = wa_vfkp-mwsbp.
*        ENDIF.
*
*        READ TABLE it_vttp ASSIGNING FIELD-SYMBOL(<ws_it_vttp>) WITH KEY tknum = <wa_saida_simplicada>-tknum.
*
*        CLEAR: wa_lips.
*        READ TABLE it_lips INTO wa_lips WITH KEY vbeln = <ws_it_vttp>-vbeln .
*        IF sy-subrc IS INITIAL.
*          <wa_saida_simplicada>-ov_pedido = wa_lips-vgbel.
*
*          READ TABLE t_vbak ASSIGNING FIELD-SYMBOL(<ws_vbak>) WITH KEY vbeln = wa_lips-vgbel .
*          IF ( sy-subrc EQ 0 ).
*            IF  ( wa_lips-pstyv NE 'ELN' AND wa_lips-pstyv NE 'NLN').
*              IF wa_lips-matkl = '700110'.
*                <wa_saida_simplicada>-transgenese = <ws_vbak>-kvgr3.
*              ENDIF.
*              <wa_saida_simplicada>-tp_ov = <ws_vbak>-auart.
*
**            READ TABLE t_vbfa INTO DATA(ws_vbfa) WITH KEY vbeln = <ws_it_vttp>-vbeln
**                                                                       vbtyp_n = 'M'
**                                                                       vbtyp_v = 'J'.
*
*
*              READ TABLE t_vbfa INTO DATA(ws_vbfa) WITH KEY vbelv = wa_lips-vbeln
*                                                         vbtyp_n  = 'M'
*                                                         vbtyp_v  = 'J'.
*              IF sy-subrc EQ 0.
*                READ TABLE t_j_1bnflin ASSIGNING FIELD-SYMBOL(<ws_j_1bnflin>) WITH KEY refkey = ws_vbfa-vbeln.
*                IF sy-subrc EQ 0.
*                  READ TABLE t_j_1bnfe_active ASSIGNING FIELD-SYMBOL(<ls_1bnfe_active>) WITH KEY docnum = <ws_j_1bnflin>-docnum.
*                  IF sy-subrc EQ 0.
*                    READ TABLE t_zlest0039 ASSIGNING FIELD-SYMBOL(<ws_zlest0039>) WITH KEY docnum = <ls_1bnfe_active>-docnum.
*                    IF sy-subrc EQ 0.
*
**                      IF <ws_zlest0039>-pontotransb NE 0.
**                        <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datatransb.
**                        <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesotransb.
**                      ELSE.
**                        <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datachegada.
**                        <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesochegada.
**                      ENDIF.
*
*                      <wa_saida_simplicada>-cod_tran_efetivo = <ws_zlest0039>-transb_efetivo.
*
*                      READ TABLE it_kna1 ASSIGNING FIELD-SYMBOL(<ws_kna1>) WITH KEY kunnr = <ws_zlest0039>-transb_efetivo.
*                      IF sy-subrc EQ 0.
*                        <wa_saida_simplicada>-descr_tran_efetivo = <ws_kna1>-name1.
*                      ENDIF.
*                    ENDIF.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*
*          IF wa_lips-pstyv EQ 'NLN'.
*            CLEAR: ws_vbfa.
*            READ TABLE t_vbfa INTO ws_vbfa WITH KEY vbeln = wa_lips-vgbel
*                                                  vbtyp_n = 'R'
*                                                  vbtyp_v = 'J'.
*
*
*            IF ( sy-subrc = 0 ).
*              CLEAR: vs_refkey.
*              vs_refkey = |{ ws_vbfa-vbeln }{ ws_vbfa-mjahr }|.
*              READ TABLE t_j_1bnflin ASSIGNING <ws_j_1bnflin> WITH KEY refkey = vs_refkey.
*              IF sy-subrc EQ 0.
*                READ TABLE t_j_1bnfe_active ASSIGNING <ls_1bnfe_active> WITH KEY docnum = <ws_j_1bnflin>-docnum.
*                IF sy-subrc EQ 0.
*                  READ TABLE t_zlest0039 ASSIGNING <ws_zlest0039> WITH KEY docnum = <ls_1bnfe_active>-docnum.
*                  IF sy-subrc EQ 0.
**                    IF <ws_zlest0039>-pontotransb NE 0.
**                      <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datatransb.
**                      <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesotransb.
**                    ELSE.
**                      <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datachegada.
**                      <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesochegada.
**                    ENDIF.
*
*                    <wa_saida_simplicada>-cod_tran_efetivo = <ws_zlest0039>-transb_efetivo.
*
*                    READ TABLE it_kna1 ASSIGNING <ws_kna1> WITH KEY kunnr = <ws_zlest0039>-transb_efetivo.
*                    IF sy-subrc EQ 0.
*                      <wa_saida_simplicada>-descr_tran_efetivo = <ws_kna1>-name1.
*                    ENDIF.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
*        CLEAR: ws_vbfa.
*        READ TABLE t_vbfa INTO ws_vbfa WITH KEY vbelv = wa_lips-vbeln
*                                                         vbtyp_n  = 'M'
*                                                         vbtyp_v  = 'J'.
*        CLEAR: vs_refkey.
*        vs_refkey = ws_vbfa-vbeln.
*        IF ws_vbfa IS INITIAL.
*          READ TABLE t_vbfa INTO ws_vbfa WITH KEY vbelv = wa_lips-vbeln
*                                                       vbtyp_n  = 'R'
*                                                       vbtyp_v  = 'J'.
*
*          CLEAR: vs_refkey.
*          vs_refkey = |{ ws_vbfa-vbeln }{ ws_vbfa-mjahr }|.
*        ENDIF.
*
*        IF <wa_saida_simplicada>-tipo EQ 'PRÓPRIO'.
*          IF sy-subrc EQ 0.
*            READ TABLE t_j_1bnflin ASSIGNING <ws_j_1bnflin> WITH KEY refkey = vs_refkey.
*            IF sy-subrc EQ 0.
*              READ TABLE t_j_1bnfe_active ASSIGNING <ls_1bnfe_active> WITH KEY docnum = <ws_j_1bnflin>-docnum.
*              IF sy-subrc EQ 0.
*                READ TABLE t_zsdt0105 ASSIGNING FIELD-SYMBOL(<ws_zsdt0105>) WITH KEY docnum = <ls_1bnfe_active>-docnum.
*                IF sy-subrc EQ 0.
*                  READ TABLE t_zsdt0102 ASSIGNING FIELD-SYMBOL(<ws_zsdt0102>) WITH KEY docnum = <ws_zsdt0105>-docnum_ref.
*                  IF sy-subrc EQ 0.
*                    <wa_saida_simplicada>-nr_mdfe = <ws_zsdt0102>-nmdfe.
*                    <wa_saida_simplicada>-docnum_mdfe = <ws_zsdt0102>-docnum.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
*        CLEAR: wa_vbak, wa_vbfa, wa_j_1bnflin.
*        IF <wa_saida_simplicada>-tipo NE 'PRÓPRIO'.
*          READ TABLE lt_vbak ASSIGNING <lfs_vbak> WITH KEY tknum = <wa_saida_simplicada>-tknum BINARY SEARCH.
*          IF ( sy-subrc = 0 ).
*            wa_vbak = CORRESPONDING #( <lfs_vbak> ).
*          ENDIF.
*
*          READ TABLE lt_vbfa ASSIGNING <lfs_vbfa> WITH KEY vbelv = wa_vbak-vbeln BINARY SEARCH.
*          IF ( sy-subrc = 0 ).
*            wa_vbfa = CORRESPONDING #( <lfs_vbfa> ).
*          ENDIF.
*
*          READ TABLE lt_j_1bnflin[] ASSIGNING <lfs_j_1bnflin> WITH KEY refkey = wa_vbfa-vbeln BINARY SEARCH.
*          IF ( sy-subrc = 0 ).
*            wa_j_1bnflin = CORRESPONDING #( <lfs_j_1bnflin> ).
*          ENDIF.
*
*          READ TABLE it_zsdt0105 ASSIGNING <ws_zsdt0105> WITH KEY docnum = wa_j_1bnflin-docnum.
*          IF sy-subrc EQ 0.
*            READ TABLE it_zsdt0102 ASSIGNING <ws_zsdt0102> WITH KEY docnum = <ws_zsdt0105>-docnum_ref.
*            IF sy-subrc EQ 0.
*              <wa_saida_simplicada>-nr_mdfe = <ws_zsdt0102>-nmdfe.
*              <wa_saida_simplicada>-docnum_mdfe = <ws_zsdt0102>-docnum.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
****    LOOP AT it_vttp INTO wa_vttp WHERE tknum EQ <wa_saida_simplicada>-tknum.
*        LOOP AT it_sort_vttp INTO wa_vttp WHERE tknum EQ <wa_saida_simplicada>-tknum.
*
*          <wa_saida_simplicada>-vttp_vbeln = wa_vttp-vbeln.
*
*          SELECT SINGLE *
*            FROM vfsi
*           INTO @DATA(wa_vfsi)
*          WHERE knumv EQ @wa_vfkp-knumv
*            AND vbeln EQ @wa_vttp-vbeln.
*
*          <wa_saida_simplicada>-netwr = wa_vfsi-netwr.
*
*          SELECT SINGLE belnr bukrs gjahr
*            FROM bkpf
*            INTO CORRESPONDING FIELDS OF wa_bkpf
*            WHERE xblnr = <wa_saida_simplicada>-xblnr
*             AND budat >= <wa_saida_simplicada>-erdat.
*
*          <wa_saida_simplicada>-bukrs = wa_bkpf-bukrs.
*          <wa_saida_simplicada>-gjahr = wa_bkpf-gjahr.
*
*          SELECT SINGLE dmbtr dmbe2
*            FROM bsis
*           INTO CORRESPONDING FIELDS OF wa_bsis
*          WHERE belnr = wa_bkpf-belnr
*           AND bukrs = <wa_saida_simplicada>-bukrs
*           AND gjahr = <wa_saida_simplicada>-gjahr
*           AND shkzg = 'S'.
*
*          CLEAR: wa_lips.
*          READ TABLE it_lips INTO wa_lips WITH KEY vbeln = <wa_saida_simplicada>-vttp_vbeln BINARY SEARCH.
*          IF sy-subrc IS INITIAL.
*            <wa_saida_simplicada>-matnr1 = wa_lips-matnr.
*            <wa_saida_simplicada>-ntgew = wa_lips-brgew.
*            <wa_saida_simplicada>-grupo_produto = wa_lips-matkl.
*            READ TABLE t_vbak ASSIGNING FIELD-SYMBOL(<ws_vbak_aux>) WITH KEY vbeln = wa_lips-vgbel .
*            IF ( sy-subrc EQ 0 ).
*
*              IF  ( wa_lips-pstyv NE 'ELN' AND wa_lips-pstyv NE 'NLN').
*                READ TABLE t_vbfa INTO DATA(ws_vbfa_aux) WITH KEY vbelv = wa_lips-vbeln
*                                                        vbtyp_n  = 'M'
*                                                        vbtyp_v  = 'J'.
*                IF sy-subrc EQ 0.
*                  READ TABLE t_j_1bnflin ASSIGNING FIELD-SYMBOL(<ws_j_1bnflin_aux>) WITH KEY refkey = ws_vbfa_aux-vbeln.
*                  IF sy-subrc EQ 0.
*                    READ TABLE t_j_1bnfe_active ASSIGNING FIELD-SYMBOL(<ls_1bnfe_active_aux>) WITH KEY docnum = <ws_j_1bnflin_aux>-docnum.
*                    IF sy-subrc EQ 0.
*                      READ TABLE t_zlest0039 ASSIGNING FIELD-SYMBOL(<ws_zlest0039_aux>) WITH KEY docnum = <ls_1bnfe_active_aux>-docnum.
*                      IF sy-subrc EQ 0.
*
*                        IF <ws_zlest0039_aux>-pontotransb NE 0.
*                          <wa_saida_simplicada>-dt_descarga = <ws_zlest0039_aux>-datatransb.
*                          <wa_saida_simplicada>-peso_descarga = <ws_zlest0039_aux>-pesotransb.
*                        ELSE.
*                          <wa_saida_simplicada>-dt_descarga = <ws_zlest0039_aux>-datachegada.
*                          <wa_saida_simplicada>-peso_descarga = <ws_zlest0039_aux>-pesochegada.
*                        ENDIF.
*
*
*                      ENDIF.
*                    ENDIF.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*            IF wa_lips-pstyv EQ 'NLN'.
*              CLEAR: ws_vbfa.
*              READ TABLE t_vbfa INTO ws_vbfa WITH KEY vbeln = wa_lips-vgbel
*                                                    vbtyp_n = 'R'
*                                                    vbtyp_v = 'J'.
*
*
*              IF ( sy-subrc = 0 ).
*                CLEAR: vs_refkey.
*                vs_refkey = |{ ws_vbfa-vbeln }{ ws_vbfa-mjahr }|.
*                READ TABLE t_j_1bnflin ASSIGNING <ws_j_1bnflin> WITH KEY refkey = vs_refkey.
*                IF sy-subrc EQ 0.
*                  READ TABLE t_j_1bnfe_active ASSIGNING <ls_1bnfe_active> WITH KEY docnum = <ws_j_1bnflin>-docnum.
*                  IF sy-subrc EQ 0.
*                    READ TABLE t_zlest0039 ASSIGNING <ws_zlest0039> WITH KEY docnum = <ls_1bnfe_active>-docnum.
*                    IF sy-subrc EQ 0.
*                      IF <ws_zlest0039>-pontotransb NE 0.
*                        <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datatransb.
*                        <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesotransb.
*                      ELSE.
*                        <wa_saida_simplicada>-dt_descarga = <ws_zlest0039>-datachegada.
*                        <wa_saida_simplicada>-peso_descarga = <ws_zlest0039>-pesochegada.
*                      ENDIF.
*
*                    ENDIF.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*
*          ENDIF.
*
*          READ TABLE it_makt INTO wa_makt WITH KEY matnr = <wa_saida_simplicada>-matnr1 BINARY SEARCH.
*          IF sy-subrc IS INITIAL.
*            <wa_saida_simplicada>-maktx = wa_makt-maktx.
*          ENDIF.
*
*          IF <wa_saida_simplicada>-netwr > 0 AND wa_bsis-dmbtr > 0 AND wa_bsis-dmbe2 > 0.
*            TRY.
*                <wa_saida_simplicada>-dmbe2 = <wa_saida_simplicada>-netwr / ( wa_bsis-dmbtr / wa_bsis-dmbe2 ).
*              CATCH cx_sy_zerodivide.
*            ENDTRY.
*          ENDIF.
*
*
*          IF <wa_saida_simplicada>-netwr > 0 AND <wa_saida_simplicada>-ntgew > 0.
*            <wa_saida_simplicada>-preco_tonelada_rs = ( <wa_saida_simplicada>-netwr / <wa_saida_simplicada>-ntgew ) * 1000.
*          ENDIF.
*
*          IF <wa_saida_simplicada>-netwr > 0 AND <wa_saida_simplicada>-dmbe2 > 0.
*            <wa_saida_simplicada>-taxa_dolar = <wa_saida_simplicada>-netwr / <wa_saida_simplicada>-dmbe2.
*          ENDIF.
*
*          APPEND <wa_saida_simplicada> TO it_saida_aux.
*
*        ENDLOOP.
*
*      ENDLOOP.
*
*
*
*      IF p_placa IS NOT INITIAL.
*        DELETE it_saida_aux WHERE placa_carreta NOT IN p_placa.
*      ENDIF.
*
*      IF p_prop = 'X'.
*        DELETE it_saida_aux WHERE tp_veiculo = 'T'.
*      ELSE.
*        DELETE it_saida_aux WHERE tp_veiculo = 'P'.
*      ENDIF.
*
*    ENDIF.
*
*    IF p_prop = 'X'.
*      PERFORM fm_sel_frete_subcontratado.
*    ENDIF.
*
*  ENDIF.

* Fim - LES - Replicação da ZLES0177 em uma tabela - FA - #94050 - 10.11.2022

endform. "BUSCA_DADOS


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'STATUS0100'.
  set titlebar 'TITULO100'.
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
  endcase.

endmodule.

form fill_it_fieldcatalog using value(p_colnum)
                                value(p_fieldname)
                                value(p_tabname)
                                value(p_len)
                                value(p_edit)
                                value(p_icon)
                                value(p_do_sum)
                                value(p_header)
                                value(p_hotspot)
                                value(p_no_zero)
                                value(p_lzero).

  data: wa_fieldcatalog type lvc_s_fcat.

  wa_fieldcatalog-col_pos     = p_colnum.
  wa_fieldcatalog-fieldname   = p_fieldname.
  wa_fieldcatalog-tabname     = p_tabname.
  wa_fieldcatalog-outputlen   = p_len.
  wa_fieldcatalog-coltext     = p_header.
  wa_fieldcatalog-edit        = p_edit.
  wa_fieldcatalog-icon        = p_icon.
  wa_fieldcatalog-ref_table   = p_tabname.
  wa_fieldcatalog-checktable  = p_tabname.
  wa_fieldcatalog-do_sum      = p_do_sum.
  wa_fieldcatalog-hotspot    = p_hotspot.
*  wa_fieldcatalog-no_zero    = p_no_zero.
  wa_fieldcatalog-lzero      = p_lzero.
  append wa_fieldcatalog to it_fieldcatalog.

endform.

form fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

endform.

form f_pega_imagem  using    nome_logo
                    changing url.

  data: begin of graphic_table occurs 0,
          line(255) type x,
        end of graphic_table.

  data: l_graphic_xstr type xstring.
  data: graphic_size   type i.
  data: l_graphic_conv type i.
  data: l_graphic_offs type i.

  refresh graphic_table.

  call method cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    exporting
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    receiving
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.

  while l_graphic_conv > 255.

    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    append graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.

  endwhile.

  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  append graphic_table.

  call function 'DP_CREATE_URL'
    exporting
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    tables
      data     = graphic_table
    changing
      url      = url.

endform.

form imprimir_alv.
  if g_custom_container is initial.

    create object g_custom_container
      exporting
        container_name              = 'CONTAINER'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.


    if sy-subrc <> 0.
      message a000(tree_control_msg).
    endif.
    create object dg_splitter_1
      exporting
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    call method dg_splitter_1->get_container
      exporting
        row       = 1
        column    = 1
      receiving
        container = dg_parent_1.

    call method dg_splitter_1->get_container
      exporting
        row       = 2
        column    = 1
      receiving
        container = dg_parent_alv.

    create object dg_splitter_2
      exporting
        parent  = dg_parent_1
        rows    = 1
        columns = 2.

    call method dg_splitter_2->get_container
      exporting
        row       = 1
        column    = 1
      receiving
        container = dg_parent_2.

    call method dg_splitter_2->get_container
      exporting
        row       = 1
        column    = 2
      receiving
        container = dg_parent_2a.

    call method dg_splitter_1->set_row_height
      exporting
        id     = 1
        height = 16.

    call method dg_splitter_2->set_column_width
      exporting
        id    = 1
        width = 40.

    create object picture
      exporting
        parent = dg_parent_2a.

    perform f_pega_imagem using 'LOGO_NOVO' changing url.

    call method picture->load_picture_from_url
      exporting
        url = url.

    call method picture->set_display_mode
      exporting
        display_mode = picture->display_mode_fit_center.


    perform fill_it_fieldcatalog using:

          01 'TKNUM              ' ' '  '10'  ' '     ' '    ' '   'Doc. Transp.           '   ' '   'X'   ' ',
          02 'FKNUM              ' ' '  '10'  ' '     ' '    ' '   'Doc. Custo             '   ' '   'X'   ' ',
          03 'TPLST              ' ' '  '5'   ' '     ' '    ' '   'Filial                 '   ' '   ' '   ' ',
          04 'BEZEI              ' ' '  '12'  ' '     ' '    ' '   'Nome Filial            '   ' '   ' '   ' ',
          05 'ERDAT              ' ' '  '12'  ' '     ' '    ' '   'Data Documento         '   ' '   ' '   ' ',
          06 'MATNR1             ' ' '  '12'  ' '     ' '    ' '   'Cod. Produto           '   ' '   'X'   ' ',
          07 'MAKTX              ' ' '  '25'  ' '     ' '    ' '   'Descrição do Produto   '   ' '   ' '   ' ',
          08 'PLACA_CARRETA      ' ' '  '12'  ' '     ' '    ' '   'Placa Cavalo           '   ' '   ' '   ' ',
          09 'CARRETA1           ' ' '  '12'  ' '     ' '    ' '   'Carreta 1              '   ' '   ' '   ' ',
          10 'CARRETA2           ' ' '  '12'  ' '     ' '    ' '   'Carreta 2              '   ' '   ' '   ' ',
          11 'CARRETA3           ' ' '  '12'  ' '     ' '    ' '   'Carreta 3              '   ' '   ' '   ' ',
          12 'SHTYP              ' ' '  '12'  ' '     ' '    ' '    'Tp_Tranp              '   ' '   ' '   ' ',
          13 'TDLNR              ' ' '  '12'  ' '     ' '    ' '   'Agente Frete           '   ' '   'X'   ' ',
          14 'LIFNR              ' ' '  '12'  ' '     ' '    ' '   'Origem - Código        '   ' '   'X'   ' ',
          15 'DESC_ORIGEM        ' ' '  '36'  ' '     ' '    ' '   'Origem - Descrição     '   ' '   ' '   ' ',
          16 'MUNIC_ORIGEM       ' ' '  '12'  ' '     ' '    ' '   'Origem - Município     '   ' '   ' '   ' ',
          17 'UF_ORIGEM          ' ' '  '12'  ' '     ' '    ' '   'Origem - UF            '   ' '   ' '   ' ',
          18 'KUNNR              ' ' '  '12'  ' '     ' '    ' '   'Destino – Código       '   ' '   'X'   ' ',
          19 'DESC_DESTINO       ' ' '  '36'  ' '     ' '    ' '   'Destino - Descrição    '   ' '   ' '   ' ',
          20 'MUNIC_DESTINO      ' ' '  '12'  ' '     ' '    ' '   'Destino - Município    '   ' '   ' '   ' ',
          21 'UF_DESTINO         ' ' '  '12'  ' '     ' '    ' '   'Destino - UF           '   ' '   ' '   ' ',
          22 'TIPO               ' ' '  '15'  ' '     ' '    ' '   'Tipo                   '   ' '   ' '   ' ',
*          23 'DT_PROTOCOLO       ' ' '  '15'  ' '     ' '    ' '   'Data Mdf-e             '   ' '   ' '   ' ',
*          24 'HR_PROTOCOLO       ' ' '  '15'  ' '     ' '    ' '   'Hora Mdf-e             '   ' '   ' '   ' ',
          25 'MWSBP              ' ' '  '12'  ' '     ' '    ' '   'Imp_Receita            '   ' '   ' '   ' ',
          26 'NTGEW              ' ' '  '12'  ' '     ' '    ' '   'Kg Trasportados        '   ' '   ' '   ' ',
          27 'PRECO_TONELADA_RS  ' ' '  '17'  ' '     ' '    ' '   'Preço por Ton_R$       '   ' '   ' '   ' ',
          28 'NETWR              ' ' '  '12'  ' '     ' '    ' '   'Valor Total R$         '   ' '   ' '   ' ',
          29 'TAXA_DOLAR         ' ' '  '12'  ' '     ' '    ' '   'Tx_Dólar               '   ' '   ' '   ' ',
          30 'DMBE2              ' ' '  '12'  ' '     ' '    ' '   'Valor Total USD        '   ' '   ' '   ' ',
          31 'BELNR2             ' ' '  '12'  ' '     ' '    ' '   'Doc Cont  Receita      '   ' '   ' '   ' ',
          32 'DOCNUM             ' ' '  '12'  ' '     ' '    ' '   'Docnum_Receita         '   'X'   ' '   ' ',
          33 'NFENUM             ' ' '  '10'  ' '     ' '    ' '   'Nr_Ct-e                '   ' '   'X'   ' ',
          34 'VTTP_VBELN         ' ' '  '10'  ' '     ' '    ' '   'Remessa                '   ' '   ' '   ' ',
          35 'OV_PEDIDO          ' ' '  '12'  ' '     ' '    ' '   'OV/Pedido              '   ' '   ' '   ' ',
          36 'TP_OV              ' ' '  '12'  ' '     ' '    ' '   'Tipo de OV             '   ' '   ' '   ' ',
          37 'TRANSGENESE        ' ' '  '12'  ' '     ' '    ' '   'Transgenese            '   ' '   ' '   ' ',
          38 'DT_DESCARGA        ' ' '  '10'  ' '     ' '    ' '   'Dt descarga            '   ' '   ' '   ' ',
          39 'PESO_DESCARGA      ' ' '  '10'  ' '     ' '    ' '   'Peso Descarga          '   ' '   ' '   ' ',
          40 'COD_TRAN_EFETIVO   ' ' '  '10'  ' '     ' '    ' '   'Cód transb efetivo     '   ' '   ' '   ' ',
          41 'DESCR_TRAN_EFETIVO ' ' '  '10'  ' '     ' '    ' '   'Descr transb efetivo   '   ' '   ' '   ' ',
          42 'GRUPO_VEICULO      ' ' '  '10'  ' '     ' '    ' '   'Grupo Veículo          '   ' '   ' '   'X',
          43 'FROTA              ' ' '  '10'  ' '     ' '    ' '   'Frota                  '   ' '   ' '   'X',
          44 'GRUPO_PRODUTO      ' ' '  '10'  ' '     ' '    ' '   'Grupo Produto          '   ' '   ' '   ' ',
          45 'COD_MOTORISTA      ' ' '  '10'  ' '     ' '    ' '   'Cod Motorista          '   ' '   ' '   ' ',
          46 'NOME_MOTORISTA     ' ' '  '10'  ' '     ' '    ' '   'Nome Motorista         '   ' '   ' '   ' ',
          47 'CPF_MOTORISTA      ' ' '  '10'  ' '     ' '    ' '   'CPF motorista          '   ' '   ' '   ' ',
          48 'DOCNUM_MDFE        ' ' '  '10'  ' '     ' '    ' '   'Docnum MDF-e           '   ' '   ' '   ' ',
          49 'NR_MDFE            ' ' '  '10'  ' '     ' '    ' '   'Nr. MDF-e              '   ' '   ' '   ' ',
          50 'ORDEM_CAR          ' ' '  '10'  ' '     ' '    ' '   'Ordem Carreg.          '   ' '   ' '   ' ',
          51 'DT_OC              ' ' '  '10'  ' '     ' '    ' '   'Dt OC                  '   ' '   ' '   ' ',
          52 'CD_CHAVE_CTE       ' ' '  '20'  ' '     ' '    ' '   'Chave Cte.             '   ' '   ' '   ' ',
          53 'CHAVE_NFE          ' ' '  '20'  ' '     ' '    ' '   'Chave Nfe              '   ' '   ' '   ' ',
          54 'DATA_MDFE          ' ' '  '10'  ' '     ' '    ' '   'Data Mdf-e             '   ' '   ' '   ' ',
          55 'HORA_MDFE          ' ' '  '10'  ' '     ' '    ' '   'Hora Mdf-e             '   ' '   ' '   ' ',
          56 'KBETR              ' ' '  '15'  ' '     ' '    ' '   'Vlr. Pedágio R$        '   ' '   ' '   ' ',
          57 'VLR_FRETE          ' ' '  '15'  ' '     ' '    ' '   'Vlr. Frete R$          '   ' '   ' '   ' '.
*          54 'HORA               ' ' '  '15'  ' '     ' '    ' '   'Hora                   '   ' '   ' '   ' '. "Hora / 111592 CS2023000322 SIM4.0 - Incerir a hora na coluna Data Documento ZLES0177 PSA


    perform fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
*    gs_layout-col_opt = 'X'.
    gs_layout-cwidth_opt = 'X'.
    gs_layout-zebra = 'X'.
    clear: it_exclude_fcode, it_exclude_fcode[].


    create object ctl_alv
      exporting
        i_shellstyle    = 0
        i_parent        = dg_parent_alv
        i_appl_events   = abap_false
        i_fcat_complete = abap_false.

    set handler lcl_event_handler=>on_hotspot_click for ctl_alv.

    call method ctl_alv->set_table_for_first_display
      exporting
        is_layout                     = gs_layout
        i_save                        = 'A'
        is_variant                    = variante
"       IT_TOOLBAR_EXCLUDING          = IT_EXCLUDE_FCODE
      changing
        it_fieldcatalog               = it_fieldcatalog
        it_outtab                     = it_saida_aux
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.

*    step 1 : initialise time control
    create object ob_timer
      exporting
        parent = g_custom_container.

* Step 2 : Initialise object to receive event
    create object ob_recev.

* Step 3 : Couple event to method
    set handler ob_recev->handle_finished for ob_timer.

* Step 4 : Set Interval in seconds
    ob_timer->interval = 300.

    call method ob_timer->run.



    create object dg_dyndoc_id
      exporting
        style = 'ALV_GRID'.

    call method dg_dyndoc_id->initialize_document.

    call method dg_dyndoc_id->add_table
      exporting
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      importing
        table         = table_element.

    call method table_element->add_column
      importing
        column = column.

    call method table_element->set_column_style
      exporting
        col_no    = 1
        "SAP_ALIGN = 'CENTER'
        sap_style = cl_dd_document=>heading.

    p_text = text-003.

    call method column->add_text
      exporting
        text      = p_text
        sap_style = 'HEADING'.

    call method dg_dyndoc_id->add_table
      exporting
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      importing
        table         = table_element2.

    call method table_element2->add_column
      exporting
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      importing
        column      = column_1.

    clear: p_text_table.




    "------------------
    loop at p_erdat.
      if p_erdat-option eq 'BT'.
        concatenate p_erdat-low+6(2) '.' p_erdat-low+4(2) '.' p_erdat-low(4) into vl_dates1.
        concatenate p_erdat-high+6(2) '.' p_erdat-high+4(2) '.' p_erdat-high(4) into vl_dates2.
        concatenate 'Período:' vl_dates1 '-' vl_dates2 into sdydo_text_element separated by space.
        exit.
      else.
        concatenate p_erdat-low+6(2) '.' p_erdat-low+4(2) '.' p_erdat-low(4) into vl_dates1.
        concatenate 'Período:' vl_dates1 into sdydo_text_element separated by space.
      endif.
    endloop.
    append sdydo_text_element to p_text_table.
    clear: sdydo_text_element, vl_dates1, vl_dates2.

    "------------------
    if p_tknum is not initial.
      loop at p_tknum.
        if p_tknum-option ne 'EQ' and p_tknum-option ne 'BT'.
          sdydo_text_element = 'Doc Transp.: Multiplas Seleções'.
          exit.
        elseif p_tknum-option eq 'BT'.
          concatenate 'Doc Transp.:' p_tknum-low '-' p_tknum-high into sdydo_text_element separated by space.
          exit.
        else.
          vl_cont = vl_cont + 1.
          if vl_cont gt 1.
            sdydo_text_element = 'Doc Transp.: Multiplas Seleções'.
          else.
            concatenate 'Doc Transp.:' p_tknum-low into sdydo_text_element separated by space.
          endif.
        endif.
      endloop.
      append sdydo_text_element to p_text_table.
      clear: vl_cont, sdydo_text_element.
    endif.
    clear: vl_cont, sdydo_text_element.

    "------------------
    if p_tplst is not initial.
      loop at p_tplst.
        if p_tplst-option ne 'EQ' and p_tplst-option ne 'BT'.
          sdydo_text_element = 'Filial: Multiplas Seleções'.
          exit.
        elseif p_tplst-option eq 'BT'.
          concatenate 'Filial:' p_tplst-low '-' p_tplst-high into sdydo_text_element separated by space.
          exit.
        else.
          vl_cont = vl_cont + 1.
          if vl_cont gt 1.
            sdydo_text_element = 'Filial: Multiplas Seleções'.
          else.
            concatenate 'Filial:' p_tplst-low into sdydo_text_element separated by space.
          endif.
        endif.
      endloop.
      append sdydo_text_element to p_text_table.
      clear: vl_cont, sdydo_text_element.
    endif.
    clear: vl_cont, sdydo_text_element.
    "------------------
    if p_placa is not initial.
      loop at p_placa.
        if p_placa-option ne 'EQ' and p_placa-option ne 'BT'.
          sdydo_text_element = 'Placa_Cavalo: Multiplas Seleções'.
          exit.
        elseif p_placa-option eq 'BT'.
          concatenate 'Placa_Cavalo:' p_placa-low '-' p_placa-high into sdydo_text_element separated by space.
          exit.
        else.
          vl_cont = vl_cont + 1.
          if vl_cont gt 1.
            sdydo_text_element = 'Placa_Cavalo: Multiplas Seleções'.
          else.
            concatenate 'Placa_Cavalo:' p_placa-low into sdydo_text_element separated by space.
          endif.
        endif.
      endloop.
      append sdydo_text_element to p_text_table.
      clear: vl_cont, sdydo_text_element.
    endif.
    clear: vl_cont, sdydo_text_element.

    "------------------
    if p_prop = 'X'.
      sdydo_text_element = 'Tipo: Frota Próprio'.
    else.
      sdydo_text_element = 'Tipo: Frota Terceiro'.
    endif.
    append sdydo_text_element to p_text_table.
    clear: vl_cont, sdydo_text_element.
    clear: vl_cont, sdydo_text_element.

    "------------------
    call method column_1->add_text
      exporting
        text_table = p_text_table
        fix_lines  = 'X'.

    call method dg_dyndoc_id->merge_document.

    create object dg_html_cntrl
      exporting
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    call method dg_dyndoc_id->display_document
      exporting
        reuse_control      = 'X'
        parent             = dg_parent_2
      exceptions
        html_display_error = 1.

    "PERFORM AJUSTA_TOTAIS.

  else.

    ls_stable-row = 'X'.
    ls_stable-col = 'X'.

    call method ctl_alv->refresh_table_display
      exporting
        is_stable = ls_stable
      exceptions
        finished  = 1
        others    = 2.

    if sy-subrc <> 0.
    endif.

  endif.

  call screen 0100.

endform.

form imprimir_alv_carregamento.
  if g_custom_container is initial.

    create object g_custom_container
      exporting
        container_name              = 'CONTAINER'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.


    if sy-subrc <> 0.
      message a000(tree_control_msg).
    endif.
    create object dg_splitter_1
      exporting
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    call method dg_splitter_1->get_container
      exporting
        row       = 1
        column    = 1
      receiving
        container = dg_parent_1.

    call method dg_splitter_1->get_container
      exporting
        row       = 2
        column    = 1
      receiving
        container = dg_parent_alv.

    create object dg_splitter_2
      exporting
        parent  = dg_parent_1
        rows    = 1
        columns = 2.

    call method dg_splitter_2->get_container
      exporting
        row       = 1
        column    = 1
      receiving
        container = dg_parent_2.

    call method dg_splitter_2->get_container
      exporting
        row       = 1
        column    = 2
      receiving
        container = dg_parent_2a.

    call method dg_splitter_1->set_row_height
      exporting
        id     = 1
        height = 16.

    call method dg_splitter_2->set_column_width
      exporting
        id    = 1
        width = 40.

    create object picture
      exporting
        parent = dg_parent_2a.

    perform f_pega_imagem using 'LOGO_NOVO' changing url.

    call method picture->load_picture_from_url
      exporting
        url = url.

    call method picture->set_display_mode
      exporting
        display_mode = picture->display_mode_fit_center.


    perform fill_it_fieldcatalog using:

01 'id_ordem'              ' '  '15'  ''    ''   ''     'Id. Od  Carreg.'	        ''    ' '    ' ',
01 'nro_nf_prod'           ' '  '15'  ''    ''   ''     'Docnum Nfe'              ''    ' '    ' ',
01 'nr_ordem'              ' '  '15'  ''    ''   ''     'Nr. Od  Carreg.'	        ''    ' '    ' ',
02 'tp_status'             ' '  '9'   ''    ''   ''     'Status_OD'	              ''    ' '    ' ',
03 'dt_emissao'            ' '  '13'  ''    ''   ''     'Dt_emissao_OD'	          ''    ' '    ' ',
04 'dt_validade'           ' '  '14'  ''    ''   ''     'Dt_validade_OD'          ''    ' '    ' ',
05 'id_bukrs'              ' '  '7'   ''    ''   ''     'Empresa'	                ''    ' '    ' ',
06 'id_branch'             ' '  '6'   ''    ''   ''     'Filial'                  ''    ' '    ' ',
07 'name'                  ' '  '12'  ''    ''   ''     'Descr_filial'            ''    ' '    ' ',
08 'id_bukrs_ag'           ' '  '14'  ''    ''   ''     'Empresa_Transp'          ''    ' '    ' ',
09 'id_branch_ag'          ' '  '13'  ''    ''   ''     'Filial_Transp'	          ''    ' '    ' ',
10 'id_local_coleta'       ' '  '10'  ''    ''   ''     'Cód_Coleta'              ''    ' '    ' ',
11 'local_coleta_name1'    ' '  '19'  ''    ''   ''     'Descr. Ponto Coleta'     ''    ' '    ' ',
12 'local_coleta_ort01'    ' '  '19'  ''    ''   ''     'Munic. Ponto Coleta'     ''    ' '    ' ',
13 'local_coleta_regio'    ' '  '5'  ''    ''   ''     'UF_PC'                    ''    ' '    ' ',
14 'id_local_descarga'     ' '  '13'  ''    ''   ''     'Cód. Descarga'	          ''    ' '    ' ',
15 'descarga_kname1'       ' '  '14'  ''    ''   ''     'Desc. Descarga'          ''    ' '    ' ',
16 'descarga_ort01'        ' '  '15'  ''    ''   ''     'Munic. Descarga'	        ''    ' '    ' ',
17 'descarga_regio'        ' '  '11'  ''    ''   ''     'UF_Descarga'	            ''    ' '    ' ',
18 'id_local_destino'      ' '  '12'  ''    ''   ''     'Cód. Destino'            ''    ' '    ' ',
19 'local_destino_name1'   ' '  '13'  ''    ''   ''     'Desc. Destino'	          ''    ' '    ' ',
20 'local_destino_ort01'   ' '  '14'  ''    ''   ''     'Munic. Destino'          ''    ' '    ' ',
21 'local_destino_regio'   ' '  '10'  ''    ''   ''     'UF_Destino'              ''    ' '    ' ',
22 'doc_transp'            ' '  '10'  ''    ''   ''     'Doc_transp'              ''    ' '    ' ',
23 'tknum'                 ' '  '10'  ''    ''   ''     'Doc_custo'	              ''    ' '    ' ',
24 'erdat'                 ' '  '13'  ''    ''   ''     'Dt_doc_Transp'	          ''    ' '    ' ',
25 'peso_fiscal'           ' '  '13'  ''    ''   ''     'Peso_embarque'	          ''    ' '    ' ',
26 'datatransb'            ' '  '11'  ''    ''   ''     'Dt_descarga'             ''    ' '    ' ',
27 'pesotransb'            ' '  '13'  ''    ''   ''     'Peso_descarga'           ''    ' '    ' ',
28 'transb_efetivo'        ' '  '18'  ''    ''   ''     'Cód_transb_efetivo'      ''    ' '    ' ',
29 'transb_efetivo_name1'  ' '  '21'  ''    ''   ''     'Descr. Transb_efetivo'   ''    ' '    ' ',
30 'ds_placa_trator'       ' '  '12'  ''    ''   ''     'Placa_Cavalo'            ''    ' '    ' ',
31 's_placa_reboq_1'       ' '  '10'  ''    ''   ''     'Carreta_1'	              ''    ' '    ' ',
32 'ds_placa_reboq_2'      ' '  '10'  ''    ''   ''     'Carreta_2'	              ''    ' '    ' ',
33 'ds_placa_reboq_3'      ' '  '10'  ''    ''   ''     'Carreta_3'	              ''    ' '    ' ',
34 'grupo'                 ' '  '5'   ''    ''   ''     'Grupo'	                  ''    ' '    'X',
35 'frota'                 ' '  '5'   ''    ''   ''     'Frota'	                  ''    ' '    'X',
36 'id_motorista'          ' '  '13'  ''    ''   ''     'Cód_motorista'	          ''    ' '    ' ',
37 'motorista_stcd2'       ' '  '13'  ''    ''   ''     'CPF_motorista'	          ''    ' '    ' ',
38 'motorista_name1'       ' '  '14'  ''    ''   ''     'Nome_motorista'          ''    ' '    ' ',
*39 'DATA_MDFe'             ' '  '10'  ''    ''   ''     'Data'                    ''    ' '    ' ', "RJF
*40 'HORA_MDFe'             ' '  '10'  ''    ''   ''     'Hora'                    ''    ' '    ' ', "Hora / 111592 CS2023000322 SIM4.0 - Incerir a hora na coluna Data Documento ZLES0177 PSA
41 'nr_frete_comb'         ' '  '15'  ''    ''   ''     'Frete_Combinado'	        ''    ' '    ' '.

    perform fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
    gs_layout-cwidth_opt = 'X'.
    clear: it_exclude_fcode, it_exclude_fcode[].


    create object ctl_alv
      exporting
        i_shellstyle    = 0
        i_parent        = dg_parent_alv
        i_appl_events   = abap_false
        i_fcat_complete = abap_false.

    set handler lcl_event_handler=>on_hotspot_click for ctl_alv.

    call method ctl_alv->set_table_for_first_display
      exporting
        is_layout                     = gs_layout
        i_save                        = 'A'
        is_variant                    = variante
"       IT_TOOLBAR_EXCLUDING          = IT_EXCLUDE_FCODE
      changing
        it_fieldcatalog               = it_fieldcatalog
        it_outtab                     = it_saida_carregamento
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.

*    step 1 : initialise time control
    create object ob_timer
      exporting
        parent = g_custom_container.

* Step 2 : Initialise object to receive event
    create object ob_recev.

* Step 3 : Couple event to method
    set handler ob_recev->handle_finished for ob_timer.

* Step 4 : Set Interval in seconds
    ob_timer->interval = 300.

    call method ob_timer->run.



    create object dg_dyndoc_id
      exporting
        style = 'ALV_GRID'.

    call method dg_dyndoc_id->initialize_document.

    call method dg_dyndoc_id->add_table
      exporting
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      importing
        table         = table_element.

    call method table_element->add_column
      importing
        column = column.

    call method table_element->set_column_style
      exporting
        col_no    = 1
        "SAP_ALIGN = 'CENTER'
        sap_style = cl_dd_document=>heading.

    p_text = text-003.

    call method column->add_text
      exporting
        text      = p_text
        sap_style = 'HEADING'.

    call method dg_dyndoc_id->add_table
      exporting
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      importing
        table         = table_element2.

    call method table_element2->add_column
      exporting
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      importing
        column      = column_1.

    clear: p_text_table.




    "------------------
    loop at p_erdat.
      if p_erdat-option eq 'BT'.
        concatenate p_erdat-low+6(2) '.' p_erdat-low+4(2) '.' p_erdat-low(4) into vl_dates1.
        concatenate p_erdat-high+6(2) '.' p_erdat-high+4(2) '.' p_erdat-high(4) into vl_dates2.
        concatenate 'Período:' vl_dates1 '-' vl_dates2 into sdydo_text_element separated by space.
        exit.
      else.
        concatenate p_erdat-low+6(2) '.' p_erdat-low+4(2) '.' p_erdat-low(4) into vl_dates1.
        concatenate 'Período:' vl_dates1 into sdydo_text_element separated by space.
      endif.
    endloop.
    append sdydo_text_element to p_text_table.
    clear: sdydo_text_element, vl_dates1, vl_dates2.

    "------------------
    if p_tplst is not initial.
      loop at p_tplst.
        if p_tplst-option ne 'EQ' and p_tplst-option ne 'BT'.
          sdydo_text_element = 'Filial: Multiplas Seleções'.
          exit.
        elseif p_tplst-option eq 'BT'.
          concatenate 'Filial:' p_tplst-low '-' p_tplst-high into sdydo_text_element separated by space.
          exit.
        else.
          vl_cont = vl_cont + 1.
          if vl_cont gt 1.
            sdydo_text_element = 'Filial: Multiplas Seleções'.
          else.
            concatenate 'Filial:' p_tplst-low into sdydo_text_element separated by space.
          endif.
        endif.
      endloop.
      append sdydo_text_element to p_text_table.
      clear: vl_cont, sdydo_text_element.
    endif.
    clear: vl_cont, sdydo_text_element.
    "------------------
    if p_placa is not initial.
      loop at p_placa.
        if p_placa-option ne 'EQ' and p_placa-option ne 'BT'.
          sdydo_text_element = 'Placa_Cavalo: Multiplas Seleções'.
          exit.
        elseif p_placa-option eq 'BT'.
          concatenate 'Placa_Cavalo:' p_placa-low '-' p_placa-high into sdydo_text_element separated by space.
          exit.
        else.
          vl_cont = vl_cont + 1.
          if vl_cont gt 1.
            sdydo_text_element = 'Placa_Cavalo: Multiplas Seleções'.
          else.
            concatenate 'Placa_Cavalo:' p_placa-low into sdydo_text_element separated by space.
          endif.
        endif.
      endloop.
      append sdydo_text_element to p_text_table.
      clear: vl_cont, sdydo_text_element.
    endif.
    clear: vl_cont, sdydo_text_element.

    "------------------
    if p_carre = 'X'.
      sdydo_text_element = 'Tipo: Ordem Carregamento'.
    endif.
    append sdydo_text_element to p_text_table.
    clear: vl_cont, sdydo_text_element.

    "------------------
    call method column_1->add_text
      exporting
        text_table = p_text_table
        fix_lines  = 'X'.

    call method dg_dyndoc_id->merge_document.

    create object dg_html_cntrl
      exporting
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    call method dg_dyndoc_id->display_document
      exporting
        reuse_control      = 'X'
        parent             = dg_parent_2
      exceptions
        html_display_error = 1.

    "PERFORM AJUSTA_TOTAIS.

  else.

    ls_stable-row = 'X'.
    ls_stable-col = 'X'.

    call method ctl_alv->refresh_table_display
      exporting
        is_stable = ls_stable
      exceptions
        finished  = 1
        others    = 2.

    if sy-subrc <> 0.
    endif.

  endif.

  call screen 0100.

endform.


*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form limpa_campos .

  refresh: it_saida, it_saida_aux, it_vttk, it_vtpa, it_adrc, it_vttp, it_lips, it_makt, it_vfkp, it_bkpf,
  it_bsis, it_vbak, it_vbfa, it_j_1bnflin, it_j_1bnfdoc, it_zlest0002, it_lfa1, it_zsdt0001od, it_saida_carregamento.

  clear: wa_saida, wa_vttp, wa_lips, wa_makt, wa_vfkp, wa_bkpf, wa_bsis, wa_vbak,
  wa_vbfa, wa_j_1bnflin, wa_j_1bnfdoc,  v_name1, v_city1, v_descricao, cpf_motorista, wa_zsdt0001od, wa_saida_ordem_carregamento.


endform.
*&---------------------------------------------------------------------*
*&      Form  FM_SEL_FRETE_SUBCONTRATADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_sel_frete_subcontratado .

  types: begin of ty_saida_frete_subcontratado,
           vbeln   type vbak-vbeln,
           vbeln_a type vbfa-vbelv,
           docnum  type j_1bnflin-docnum,
         end of ty_saida_frete_subcontratado.



  data: t_zlest0194      type table of zlest0194,
        t_saida_aux_sub  type table of ty_saida_frete_subcontratado,
        t_vbak           type table of vbak,
        t_vbfa           type table of vbfa,
        t_j_1bnflin      type table of j_1bnflin,
        t_j_1bnfe_active type table of j_1bnfe_active,
        t_lfa1           type table of lfa1,
        it_lfa1          type table of lfa1,
        t_kna1           type table of kna1,
        t_bkpf           type table of bkpf,
        t_j_1bnfdoc      type table of j_1bnfdoc,
        t_zlest0002      type table of zlest0002,
        t_vbrk           type table of vbrk,
        t_saida_sub      type table of ty_saida,
        ws_saida_sub     type ty_saida.

  data: r_awkey     type range of awkey,
        r_gjahr     type range of gjahr,
        z_awkey     type awkey,
        z_gjahr     type gjahr,
        z_kurs2     type kurs2,
        z_kurs2_aux type char9.

  free: r_refkey, t_j_1bnflin, t_j_1bnfe_active, t_zlest0194, t_lfa1, t_kna1, t_kna1, t_saida_aux_sub.
  select distinct * from vbak
    into corresponding fields of table t_vbak
    where auart eq 'ZSSF'
      and erdat in p_erdat.

  sort t_vbak by vbeln ascending.
  delete adjacent duplicates from t_vbak comparing all fields.

  check t_vbak is not initial.

  select distinct * from vbfa as a
   into corresponding fields of table t_vbfa
    for all entries in t_vbak
    where a~vbelv eq t_vbak-vbeln
      and a~vbtyp_n = 'M'
      and a~vbtyp_v = 'C'
      and not exists ( select distinct * from vbfa  as b
                          where b~vbelv eq a~vbeln
                            and b~vbtyp_n eq 'N' ).

  sort t_vbfa by vbeln ascending.
  delete adjacent duplicates from t_vbfa comparing all fields.

  check t_vbfa is not initial.


  r_refkey = value #( for l in t_vbfa ( sign   = 'I' option = 'EQ' low    = l-vbeln ) ).

  sort r_refkey by low.
  delete adjacent duplicates from r_refkey comparing low.


  select distinct * from j_1bnflin into table t_j_1bnflin
    where refkey in r_refkey.

  sort t_j_1bnflin by docnum ascending.
  delete adjacent duplicates from t_j_1bnflin comparing all fields.

  if t_j_1bnflin is not initial.
    select distinct * from j_1bnfe_active into table t_j_1bnfe_active
      for all entries in t_j_1bnflin
      where docnum eq t_j_1bnflin-docnum.
    sort t_j_1bnfe_active by cancel.

    sort t_j_1bnfe_active by docnum ascending.
    delete adjacent duplicates from t_j_1bnfe_active comparing all fields.

    if t_j_1bnfe_active is not initial.
      delete t_j_1bnfe_active where cancel eq 'X'.
    endif.

  endif.

  move-corresponding t_vbak to t_saida_aux_sub.

  loop at t_saida_aux_sub assigning field-symbol(<ws_saida>).
    read table t_vbfa into data(ws_vbfa) with key vbelv = <ws_saida>-vbeln.
    if sy-subrc eq 0.
      <ws_saida>-vbeln_a = ws_vbfa-vbeln.
      read table t_j_1bnflin into data(ws_j_1bnflin) with key refkey = ws_vbfa-vbeln.
      if sy-subrc eq 0.
        read table t_j_1bnfe_active into data(ws_j_1bnfe_active) with key docnum = ws_j_1bnflin-docnum.
        if sy-subrc eq 0.
          <ws_saida>-docnum = ws_j_1bnfe_active-docnum.
        endif.
      endif.
    endif.
  endloop.

  select distinct * from zlest0194 into table t_zlest0194
    for all entries in t_saida_aux_sub
    where ov_sub eq t_saida_aux_sub-vbeln
      and fat_sub eq t_saida_aux_sub-vbeln_a
      and docnum_sub eq t_saida_aux_sub-docnum.

  sort t_zlest0194 ascending.
  delete adjacent duplicates from t_zlest0194 comparing all fields.

  check t_zlest0194 is not initial.

* RJF
*ZIB_CTE_DIST_TER-CD_CHAVE_CTE = t_zlest0194-chave_xml_cte
*pegar
*ZIB_CTE_DIST_TER-DT_PROTOCOLO
*ZIB_CTE_DIST_TER-HR_PROTOCOLO

  free: t_lfa1.
  select distinct * from lfa1 into table t_lfa1
    for all entries in t_zlest0194
    where lifnr eq t_zlest0194-reme_cod_forn.

  sort t_lfa1 ascending.
  delete adjacent duplicates from t_lfa1 comparing all fields.

  select distinct * from lfa1 into table it_lfa1
  for all entries in t_zlest0194
  where lifnr eq t_zlest0194-motorista.

  sort it_lfa1 ascending.
  delete adjacent duplicates from it_lfa1 comparing all fields.

  select distinct * from kna1 into table t_kna1
    for all entries in t_zlest0194
    where kunnr eq t_zlest0194-dest_cod_cliente.

  sort t_kna1 ascending.
  delete adjacent duplicates from t_kna1 comparing all fields.

  r_awkey = value #( for t in t_zlest0194 ( sign   = 'I' option = 'EQ' low    = t-fat_sub ) ).
  sort r_awkey by low.
  delete adjacent duplicates from r_awkey comparing low.

  r_gjahr = value #( for g in t_zlest0194 ( sign   = 'I' option = 'EQ' low    = g-dt_mov_ov(4) ) ).
  sort r_gjahr by low.
  delete adjacent duplicates from r_refkey comparing low.

  select distinct * from bkpf into table t_bkpf
    for all entries in t_zlest0194
    where awkey in r_awkey
     and  bukrs eq  t_zlest0194-bukrs_ov
     and  gjahr in r_gjahr.

  sort t_bkpf ascending.
  delete adjacent duplicates from t_bkpf comparing all fields.

  select distinct * from j_1bnfdoc into table t_j_1bnfdoc
    for all entries in t_zlest0194
    where docnum   eq t_zlest0194-docnum_sub.

  sort t_j_1bnfdoc ascending.
  delete adjacent duplicates from t_j_1bnfdoc comparing all fields.

  select distinct * from zlest0002 into table t_zlest0002
    for all entries in t_zlest0194
    where pc_veiculo eq t_zlest0194-placa_cav.

  sort t_zlest0002 ascending.
  delete adjacent duplicates from t_zlest0002 comparing all fields.

  select distinct * from vbrk into table t_vbrk
    for all entries in t_zlest0194
    where vbeln eq t_zlest0194-fat_sub.

  sort t_vbrk ascending.
  delete adjacent duplicates from t_vbrk comparing all fields.

  loop at t_zlest0194 into data(ws_zlest0194).
    ws_saida_sub-erdat             = ws_zlest0194-dt_mov_ov.
    ws_saida_sub-maktx             = ws_zlest0194-ds_prod_pred.
    ws_saida_sub-placa_carreta     = ws_zlest0194-placa_cav.
    ws_saida_sub-carreta1          = ws_zlest0194-placa_car1.
    ws_saida_sub-carreta2          = ws_zlest0194-placa_car2.
    ws_saida_sub-carreta3          = ws_zlest0194-placa_car3.
    ws_saida_sub-tdlnr             = ws_zlest0194-branch_ov.
    ws_saida_sub-lifnr             = ws_zlest0194-reme_cod_forn.
    ws_saida_sub-desc_origem       = ws_zlest0194-reme_rsocial.

    ws_saida_sub-kunnr             = ws_zlest0194-dest_cod_cliente.
    ws_saida_sub-desc_destino      = ws_zlest0194-dest_rsocial.
    ws_saida_sub-tipo              = 'Subcontratação'.
    ws_saida_sub-ntgew             = ws_zlest0194-qt_carga_cte.
    ws_saida_sub-netwr             = ws_zlest0194-valor_prestacao.
    ws_saida_sub-docnum            = ws_zlest0194-docnum_sub.
    ws_saida_sub-dt_descarga       = ws_zlest0194-dt_descarga.
    ws_saida_sub-peso_descarga     = ws_zlest0194-qt_descarga_cte.
    ws_saida_sub-cod_motorista     = ws_zlest0194-motorista.
    ws_saida_sub-frota             = ws_zlest0194-frota.
    ws_saida_sub-cpf_motorista     = ws_zlest0194-mot_cpf.
    ws_saida_sub-preco_tonelada_rs = ( ws_zlest0194-valor_prestacao / ws_zlest0194-qt_carga_cte ) * 1000.


    read table t_lfa1 into data(ws_lfa1) with key lifnr = ws_zlest0194-reme_cod_forn.
    if sy-subrc eq 0.
      ws_saida_sub-munic_origem      = ws_lfa1-ort01.
      ws_saida_sub-uf_origem         = ws_lfa1-regio.
    endif.

    read table t_kna1 into data(ws_kna1) with key kunnr = ws_zlest0194-dest_cod_cliente.
    if sy-subrc eq 0.
      ws_saida_sub-munic_destino      = ws_kna1-ort01.
      ws_saida_sub-uf_destino         = ws_kna1-regio.
    endif.

    read table t_vbrk into data(ws_vbrk) with key vbeln = ws_zlest0194-fat_sub.
    if sy-subrc eq 0.
      ws_saida_sub-mwsbp                = ws_vbrk-mwsbk.
    endif.


    clear: z_awkey, z_gjahr.
    z_awkey = conv #( ws_zlest0194-fat_sub ).
    z_gjahr = ws_zlest0194-dt_mov_ov(4).


    read table t_bkpf into data(ws_bkpf) with key awkey = z_awkey
                                                  bukrs = ws_zlest0194-bukrs_ov
                                                  gjahr = z_gjahr.

    if sy-subrc eq 0.

      if ws_bkpf-kurs2 is not initial.
        z_kurs2_aux = ws_bkpf-kurs2.
        replace all occurrences of '/' in z_kurs2_aux with space.
        replace all occurrences of '-' in z_kurs2_aux with space.

        z_kurs2 = conv #( z_kurs2_aux ).
      endif.

      ws_saida_sub-taxa_dolar        = z_kurs2. "(desconsiderar a / do início)
      ws_saida_sub-dmbe2             = ( ws_zlest0194-valor_prestacao * z_kurs2 ).
      ws_saida_sub-belnr2            = ws_bkpf-belnr.
    endif.

    read table t_j_1bnfdoc into data(ws_j_1bnfdoc) with key docnum = ws_zlest0194-docnum_sub.
    if sy-subrc eq 0.
      ws_saida_sub-nfenum             = ws_j_1bnfdoc-nfenum.
    endif.

    read table t_zlest0002 into data(ws_zlest0002) with key pc_veiculo = ws_zlest0194-placa_cav.
    if sy-subrc eq 0.
      ws_saida_sub-grupo_veiculo        =  ws_zlest0002-grupo.
    endif.

    clear: ws_lfa1.
    read table it_lfa1 into ws_lfa1 with key lifnr = ws_zlest0194-motorista.
    if sy-subrc eq 0.
      ws_saida_sub-nome_motorista    = ws_lfa1-name1.
    endif.

    append ws_saida_sub  to it_saida_aux.
    clear: ws_saida_sub, ws_lfa1, ws_kna1, ws_vbrk, ws_bkpf, ws_j_1bnfdoc, ws_zlest0002, z_kurs2_aux, z_kurs2.
  endloop.

endform.
