*&---------------------------------------------------------------------*
*& Report  ZSDR021
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zsdr021.

tables: zsdt0212, zsdt0214, j_1bnfdoc.

types: begin of ty_dados_webservice,
         docnum     type j_1bnfe_active-docnum,
         docsta     type j_1bnfe_active-docsta,
         cancel     type j_1bnfe_active-cancel,
         pstdat     type j_1bnfdoc-pstdat,
         nfenum     type j_1bnfdoc-nfenum,
         nfnum      type j_1bnfdoc-nfnum,
         series     type j_1bnfdoc-series,
         model      type j_1bnfdoc-model,
         bukrs      type j_1bnfdoc-bukrs,
         branch     type j_1bnfdoc-branch,
         nftype     type j_1bnfdoc-nftype,
         doctyp     type j_1bnfdoc-doctyp,
         cnpj_bupla type j_1bnfdoc-cnpj_bupla,
         cgc        type j_1bnfdoc-cgc,
         parid      type j_1bnfdoc-parid,
         docdat     type j_1bnfdoc-docdat,
         regio      type j_1bnfdoc-regio,
         parvw      type j_1bnfdoc-parvw,
         cfop       type j_1bnflin-cfop,
         grupo_cfop type i,
       end of ty_dados_webservice.

types: begin of ty_itens_entrada_sementes,
         cod_cultivar           type i,
         cod_categoria_sementes type i,
         nr_dar_indea           type string,
         nr_dar_fase            type string,
         lote                   type string,
         qnt_produto            type kwmeng,
       end of ty_itens_entrada_sementes,
       it_itens_sementes type table of ty_itens_entrada_sementes with default key.

types: begin of ty_entrada_sementes,
         cnpj_revenda             type char14,
         cod_lancamento_usuario   type i,
         hash_revenda             type string,
         origem_armazenador       type string,
         cpf_cnpj_fornecedor      type char14,
         cod_municipio_fornecedor type string,
         renasem_fornecedor       type string,
         nome_fornecedor          type string,
         observacao               type string,
         nr_nf                    type i,
         serie_nf                 type string,
         dt_nf                    type char10,
         data_entrada             type char10,
         cod_tipo_produto         type string,
         itens                    type it_itens_sementes,
       end of  ty_entrada_sementes.

types: begin of ty_cancel_entrada_sementes,
         cnpj_revenda           type string,
         hash_revenda           type string,
         oigem_armazenador      type char10,
         cod_lancamento_usuario type i,
         motivo_cancelamento    type string,
       end of  ty_cancel_entrada_sementes.

types: begin of ty_itens_saida_sementes,
         cod_cultivar type i,
         lote         type string,
         "  NR_DAR_INDEA TYPE STRING,
         "  NR_DAR_FASE  TYPE STRING,
         qnt_produto  type kwmeng,
       end of  ty_itens_saida_sementes,
       it_itens_saida_sm type table of ty_itens_saida_sementes with default key.

types: begin of ty_saida_sementes,
         cnpj_revenda           type string,
         hash_revenda           type string,
         destino_armazenador    type string,
         nr_nf                  type i,
         serie_nf               type string,
         dt_nf                  type char10,
         destino_fora_m_t       type string,
         destino_revenda        type string,
         cod_tipo_produto       type i,
         renasem_destino        type string,
         nome_destino           type string,
         observacao             type string,
         destino_internacional  type string,
         cpf_cnpj_destino       type string,
         cod_municipio_destino  type i,
         cod_lancamento_usuario type i,
         itens                  type it_itens_saida_sm,
       end of ty_saida_sementes.

types: begin of ty_cancel_saida_sementes,
         cnpj_revenda           type string,
         hash_revenda           type string,
         cod_lancamento_usuario type i,
         destino_armazenador    type string,
         motivo_cancelamento    type string,
       end of ty_cancel_saida_sementes.

types: begin of ty_itens_devolucao_sementes,
         cod_cultivar type i,
         lote         type string,
         qnt_produto  type kwmeng,
       end of        ty_itens_devolucao_sementes,
       it_item_devolucao_sementes type table of ty_itens_devolucao_sementes with empty key.

types: begin of ty_devolucao_sementes,
         cnpj_revenda                 type string,
         hash_revenda                 type string,
         cod_saida_lancamento_usuario type i,
         cod_lancamento_usuario       type i,
         data_devolucao               type char10,
         observacao                   type string,
         itens                        type it_item_devolucao_sementes,
       end of  ty_devolucao_sementes.

types: begin of ty_cancel_devolucao_sementes,
         cnpj_revenda           type string,
         hash_revenda           type string,
         cod_lancamento_usuario type i,
         motivo_cancelamento    type string,
       end of ty_cancel_devolucao_sementes.

types:begin of ty_item_entrada_agrotoxico,
        cod_produto type i,
        lote        type char20,
        qnt_produto type kwmeng,
      end of  ty_item_entrada_agrotoxico,
      t_itens type table of ty_item_entrada_agrotoxico with default key.

types: begin of ty_entrada_agrotoxico,
         cnpj_revenda           type char14,
         hash_revenda           type string,
         origem_armazenador     type string,
         nr_nf                  type i,
         serie_nf               type char10,
         dt_nf                  type char10,
         dt_entrada             type char10,
         observacao             type string,
         cnpj_fornecedor        type char14,
         cod_lancamento_usuario type i,
         itens                  type  t_itens,
       end of ty_entrada_agrotoxico.

types: begin of ty_cancel_entrada_agrotoxico,
         cnpj_revenda           type char14,
         hash_revenda           type string,
         destino_armazenador    type string,
         cod_lancamento_usuario type i,
         motivo_cancelamento    type string,
       end of ty_cancel_entrada_agrotoxico.

types: begin of ty_itens_saida_agrotoxico,
         cod_produto        type i,
         lote               type string,
         cpf_agronomo       type string,
         nr_art             type string,
         nr_receita         type string,
         cod_cultura        type i,
         cod_praga          type i,
         cod_tipo_aplicacao type i,
         cod_unidade_medida type i,
         area_qnt_tratada   type kwmeng,
         qnt_embalagem      type kwmeng,
         qtdsementes        type kwmeng,
       end of ty_itens_saida_agrotoxico,
       it_itens type table of ty_itens_saida_agrotoxico with default key.

types: begin of ty_saida_agrotoxico,
         cnpj_revenda           type string,
         hash_revenda           type string,
         cod_lancamento_usuario type i,
         destino_fora_m_t       type char10,
         destino_revenda        type char10,
         destino_armazenador    type char10,
         nr_nf                  type i,
         serie_nf               type string,
         data_nf                type char10,
         cnpj_ure               type string,
         cpf_cnpj_destino       type string,
         nome_destino           type string,
         cod_municipio          type i,
         cod_propriedade        type i,
         observacao             type string,
         itens                  type it_itens,
       end of ty_saida_agrotoxico.

types: begin of ty_cancel_saida_agrotoxico,
         cnpj_revenda           type string,
         hash_revenda           type string,
         destino_armazenador    type char10,
         cod_lancamento_usuario type i,
         motivo_cancelamento    type string,
       end of ty_cancel_saida_agrotoxico.


types: begin of ty_itens_devolucao_agrotoxico,
         cod_produto type i,
         lote        type string,
         qnt_produto type kwmeng,
       end of ty_itens_devolucao_agrotoxico,
       it_itens_devolucao_agro type table of ty_itens_devolucao_agrotoxico with default key.

types: begin of ty_devolucao_agrotoxico,
         cnpj_revenda                 type string,
         hash_revenda                 type string,
         cod_saida_lancamento_usuario type i,
         cod_lancamento_usuario       type i,
         data_devolucao               type char10,
         observacao                   type string,
         itens                        type it_itens_devolucao_agro,
       end of ty_devolucao_agrotoxico.

types: begin of ty_cancel_devolucao_agrotoxico,
         cnpj_revenda           type string,
         hash_revenda           type string,
         cod_lancamento_usuario type i,
         motivo_cancelamento    type string,
       end of ty_cancel_devolucao_agrotoxico.

types: begin of ty_itens_devolucao_ent_agro,
         cod_produto        type i,
         lote               type string,
         cpf_agronomo       type string,
         nr_art             type string,
         nr_receita         type string,
         cod_cultura        type string,
         cod_praga          type string,
         cod_tipo_aplicacao type string,
         area_qnt_tratada   type string,
         cod_unidade_medida type string,
         qnt_embalagem      type kwmeng,
       end of   ty_itens_devolucao_ent_agro,
       it_itens_devolucao_ent_agro type table of ty_itens_devolucao_ent_agro with default key.

types: begin of ty_devolucao_ent_agro,
         cnpj_revenda           type string,
         hash_revenda           type string,
         cod_lancamento_usuario type i,
         destino_fora_m_t       type char10,
         destino_revenda        type char10,
         destino_armazenador    type char10,
         nr_nf                  type i,
         serie_nf               type string,
         data_nf                type char10,
         cnpj_ure               type string,
         cpf_cnpj_destino       type string,
         nome_destino           type string,
         cod_municipio          type i,
         cod_propriedade        type i,
         itens                  type it_itens_devolucao_ent_agro,
       end of   ty_devolucao_ent_agro.

types: begin of ty_cancel_devolucao_ent_agro,
         cnpj_revenda           type string,
         hash_revenda           type string,
         destino_armazenador    type string,
         cod_lancamento_usuario type i,
         motivo_cancelamento    type string,
       end of ty_cancel_devolucao_ent_agro.

types: begin of ty_itens_transfe_agrotoxico,
         cod_produto        type i,
         lote               type string,
         cpf_agronomo       type string,
         nr_art             type string,
         nr_receita         type string,
         cod_cultura        type string,
         cod_praga          type string,
         cod_tipo_aplicacao type string,
         cod_unidade_medida type string,
         area_qnt_tratada   type string,
         qnt_embalagem      type kwmeng,
       end of ty_itens_transfe_agrotoxico,
       it_itens_transfe_agrotoxico type table of ty_itens_transfe_agrotoxico with default key.

types: begin of ty_transferencia_agrotoxico,
         cnpj_revenda           type string,
         hash_revenda           type string,
         cod_lancamento_usuario type string,
         destino_armazenador    type string,
         destino_fora_m_t       type string,
         destino_revenda        type string,
         nr_nf                  type i,
         serie_nf               type string,
         data_nf                type string,
         cpf_cnpj_destino       type string,
         cnpj_ure               type string,
         nome_destino           type string,
         cod_municipio          type string,
         cod_propriedade        type string,
         itens                  type it_itens_transfe_agrotoxico,
       end of    ty_transferencia_agrotoxico.

types: begin of ty_cancel_transf_agro,
         cnpj_revenda           type string,
         hash_revenda           type string,
         destino_armazenador    type string,
         cod_lancamento_usuario type i,
         motivo_cancelamento    type string,
       end of ty_cancel_transf_agro.

types: begin of ty_item_entrada_transf_agro,
         cod_produto type i,
         lote        type string,
         qnt_produto type kwmeng,
       end of ty_item_entrada_transf_agro,
       it_item_entrada_transf_agro type table of  ty_item_entrada_transf_agro with default key.

types: begin of ty_entrada_transf_agro,
         cnpj_revenda           type string,
         hash_revenda           type string,
         origem_armazenador     type string,
         nr_nf                  type string,
         serie_nf               type string,
         dt_nf                  type string,
         dt_entrada             type char10,
         cnpj_fornecedor        type string,
         cod_lancamento_usuario type i,
         itens                  type it_item_entrada_transf_agro,
       end of ty_entrada_transf_agro.

types: begin of ty_cancel_entrada_transf_agro,
         cnpj_revenda           type string,
         hash_revenda           type string,
         origem_armazenador     type string,
         cod_lancamento_usuario type i,
         motivo_cancelamento    type string,
       end of ty_cancel_entrada_transf_agro.

types: begin of ty_item_devolucao_entrada_sm,
         cod_cultivar type i,
         lote         type string,
         nr_dar_indea type string,
         nr_dar_fase  type string,
         qnt_produto  type i,
       end of ty_item_devolucao_entrada_sm,
       it_item_devolucao_entrada_sm type table of ty_item_devolucao_entrada_sm with default key.

types: begin of ty_devolucao_entrada_sm,
         cnpj_revenda           type string,
         hash_revenda           type string,
         cod_lancamento_usuario type i,
         destino_armazenador    type string,
         nr_nf                  type string,
         serie_nf               type string,
         dt_nf                  type string,
         destino_fora_m_t       type string,
         destino_revenda        type string,
         cpf_cnpj_destino       type string,
         renasem_destino        type string,
         nome_destino           type string,
         cod_municipio_destino  type string,
         cod_tipo_produto       type kwmeng,
         itens                  type it_item_devolucao_entrada_sm,
       end of   ty_devolucao_entrada_sm.

types: begin of ty_cancel_devolucao_ent_sm,
         cnpj_revenda           type string,
         hash_revenda           type string,
         cod_lancamento_usuario type i,
         destino_armazenador    type char10,
         motivo_cancelamento    type string,
       end of  ty_cancel_devolucao_ent_sm.

types: begin of ty_item_saida_transf_sm,
         cod_cultivar type i,
         lote         type string,
         nr_dar_indea type string,
         nr_dar_fase  type string,
         qnt_produto  type kwmeng,
       end of ty_item_saida_transf_sm,
       it_item_saida_transf_sm type table of  ty_item_saida_transf_sm with default key.

types: begin of ty_saida_transferencia_sm,
         cnpj_revenda           type string,
         hash_revenda           type string,
         cod_lancamento_usuario type i,
         destino_armazenador    type string,
         nr_nf                  type string,
         serie_nf               type string,
         dt_nf                  type string,
         destino_fora_m_t       type string,
         destino_revenda        type string,
         destino_internacional  type string,
         cpf_cnpj_destino       type string,
         renasem_destino        type string,
         nome_destino           type string,
         cod_municipio_destino  type string,
         observacao             type string,
         cod_tipo_produto       type i,
         itens                  type it_item_saida_transf_sm,
       end of ty_saida_transferencia_sm.

types: begin of ty_cancel_saida_transf_sm,
         cnpj_revenda           type string,
         hash_revenda           type string,
         cod_lancamento_usuario type i,
         destino_armazenador    type string,
         motivo_cancelamento    type string,
       end of ty_cancel_saida_transf_sm.

types: begin of ty_item_entrada_transf_sm,
         cod_cultivar           type i,
         cod_categoria_sementes type i,
         nr_dar_indea           type string,
         nr_dar_fase            type string,
         lote                   type string,
         qnt_produto            type kwmeng,
       end of ty_item_entrada_transf_sm,
       it_item_entrada_transf_sm type table of ty_item_entrada_transf_sm with default key.


types: begin of ty_entrada_transferencia_sm,
         cnpj_revenda             type string,
         cod_lancamento_usuario   type i,
         hash_revenda             type string,
         origem_armazenador       type string,
         cpf_cnpj_fornecedor      type char14,
         cod_municipio_fornecedor type string,
         renasem_fornecedor       type string,
         nome_fornecedor          type string,
         nr_nf                    type string,
         serie_nf                 type string,
         dt_nf                    type string,
         data_entrada             type string,
         cod_tipo_produto         type i,
         itens                    type it_item_entrada_transf_sm,
       end of ty_entrada_transferencia_sm.

types: begin of ty_cancel_entrada_transf_sm,
         cnpj_revenda           type string,
         hash_revenda           type string,
         oigem_armazenador      type char10,
         cod_lancamento_usuario type i,
         motivo_cancelamento    type string,
       end of  ty_cancel_entrada_transf_sm.

types: begin of ty_zauth_webservice,
         service type zauth_webservice-service,
         url     type zauth_webservice-url,
       end of ty_zauth_webservice.

types: begin of ty_j_1bnflin,
         ihivi type mara-ihivi.
         include structure  j_1bnflin.
types: end of ty_j_1bnflin.

types: begin of ty_zmmt0102,
         ebeln     type zmmt0102-ebeln,
         ebelp     type zmmt0102-ebelp,
         line_id   type zmmt0102-line_id,
         mblnr     type zmmt0102-mblnr,
         mjahr     type zmmt0102-mjahr,
         charg     type zmmt0102-charg,
         nr_fase   type zmmt0102-nr_fase,
         matnr     type zmmt0102-matnr,
         werks     type zmmt0102-werks,
         lgort     type zmmt0102-lgort,
         categoria type zmmt0102-categoria,
         menge     type zmmt0102-menge,
         tcode     type zmmt0102-tcode,
         renas     type zmmt0102-renas,
         docnum    type j_1bnflin-docnum,
         reftyp    type j_1bnflin-reftyp,
       end of ty_zmmt0102.

types: begin of ty_quantidade,
         codigomapa type zsdt0219-codigomapa,
         quantidade type zsdt0219-quantidade,
       end of ty_quantidade.

types: begin of ty_mseg.
         include structure mseg.
types docnum type j_1bnfdoc-docnum.
types: end of ty_mseg.

types: begin of ty_tekbe,
         ebeln type ekbe-ebeln,
         ebelp type ekbe-ebelp,
         belnr type ekbe-belnr,
         gjahr type ekbe-gjahr,
         lfbnr type ekbe-lfbnr,
         lfpos type ekbe-lfpos,
         lfgja type ekbe-lfgja,
         xblnr type ekbe-xblnr,
       end of  ty_tekbe.

types: begin of ty_cfop,
         cfop  type j_1bnflin-cfop,
         grupo type i,
       end of  ty_cfop.

types: begin of ty_obj,
         matnr type matnr,
         charg type charg_d,
       end of ty_obj.

data: ls_obj type ty_obj.

data: it_defensivos type table of ty_dados_webservice,
      wa_defensivos type ty_dados_webservice,
      it_sementes   type table of ty_dados_webservice,
      wa_sementes   type ty_dados_webservice,
      it_doc        type table of ty_dados_webservice,
      wa_doc        type ty_dados_webservice,
      it_cfop       type table of ty_cfop,
      wa_cfop       type ty_cfop.

data: wa_entrada_agrotoxico          type ty_entrada_agrotoxico,
      wa_cancel_entrada_agrotoxico   type ty_cancel_entrada_agrotoxico,
      wa_saida_agrotoxico            type ty_saida_agrotoxico,
      wa_cancel_saida_agrotoxico     type ty_cancel_saida_agrotoxico,
      wa_devolucao_agrotoxico        type ty_devolucao_agrotoxico,
      wa_cancel_devolucao_agrotoxico type ty_cancel_devolucao_agrotoxico,
      wa_saida_arm_agrotoxico        type ty_saida_agrotoxico,
      wa_cancel_saida_arm_agrotoxico type ty_cancel_entrada_agrotoxico,
      wa_entrada_arm_agrotoxico      type ty_entrada_agrotoxico,
      wa_cancel_entrada_arm_agrot    type ty_cancel_entrada_agrotoxico,
      wa_entrada_sementes            type ty_entrada_sementes,
      wa_cancel_entrada_sementes     type ty_cancel_entrada_sementes,
      wa_saida_sementes              type ty_saida_sementes,
      wa_cancel_saida_sementes       type ty_cancel_saida_sementes,
      wa_devolucao_sementes          type ty_devolucao_sementes,
      wa_cancel_devolucao_sementes   type ty_cancel_devolucao_sementes,
      wa_saida_arm_sementes          type ty_saida_sementes,
      wa_cancel_saida_arm_sementes   type ty_cancel_saida_sementes,
      wa_entrada_sementes_arm        type ty_entrada_sementes,
      wa_cancel_entrada_arm_sementes type ty_cancel_entrada_sementes,
      wa_devolucao_ent_agro          type ty_devolucao_ent_agro,
      wa_cancel_devolucao_ent_agro   type ty_cancel_devolucao_ent_agro,
      wa_transferencia_agrotoxico    type ty_transferencia_agrotoxico,
      wa_cancel_transf_agro          type ty_cancel_transf_agro,
      wa_entrada_transf_agro         type ty_entrada_transf_agro,
      wa_cancel_entrada_transf_agro  type ty_cancel_entrada_transf_agro,
      wa_devolucao_entrada_sm        type ty_devolucao_entrada_sm,
      wa_cancel_devolucao_ent_sm     type ty_cancel_devolucao_ent_sm,
      wa_saida_transferencia_sm      type ty_saida_transferencia_sm,
      wa_cancel_saida_transf_sm      type ty_cancel_saida_transf_sm,
      wa_entrada_transferencia_sm    type ty_entrada_transferencia_sm,
      wa_cancel_entrada_transf_sm    type ty_cancel_entrada_transf_sm.


data: it_lfa1           type table of lfa1,
      wa_lfa1           type lfa1,
      it_kna1           type table of kna1,
      wa_kna1           type kna1,
      it_t001w          type table of t001w,
      it_ekbe           type table of ekbe,
      wa_ekbe           type ekbe,
      wa_lin            type ty_j_1bnflin,
      wa_lin_02         type  j_1bnflin,
      it_j_1bnflin      type table of ty_j_1bnflin,
      wa_j_1bnflin      type ty_j_1bnflin,
      it_j_1bnflin_02   type table of j_1bnflin,
      it_j_1bnfe_active type table of j_1bnfe_active,
      wa_j_1bnflin_02   type  j_1bnflin,
      tzmmt0102         type table of ty_zmmt0102,
      it_zmmt0102       type table of ty_zmmt0102,
      it_fiwrt0009      type table of zfiwrt0009,
      wa_mm0102         type ty_zmmt0102,
      it_mm0102         type table of ty_zmmt0102,
      wa_zmmt0102       type ty_zmmt0102,
      it_zsdt0210       type table of zsdt0210,
      wa_zsdt0210       type zsdt0210,
      it_zsdt0212       type table of zsdt0212,
      wa_zsdt0212       type zsdt0212,
      wzsdt0212         type zsdt0212,
      it_zsdt0214       type table of zsdt0214,
      wa_zsdt0214       type zsdt0214,
      wa_zsdt0001       type zsdt0001,
      it_quantidade     type table of ty_quantidade,
      wa_quanidade      type ty_quantidade,
      tmseg             type table of ty_mseg,
      wmseg             type ty_mseg,
      it_mseg           type table of ty_mseg,
      wa_mseg           type ty_mseg,
      it_mseg_02        type table of ty_mseg,
      wa_mseg_02        type ty_mseg,
      it_mseg_03        type table of ty_mseg,
      wa_mseg_03        type ty_mseg,
      tekbe             type table of ty_tekbe,
      wekbe             type ty_tekbe,
      t_status          type zde_btcstatus_t,
      it_zsdt0218       type table of zsdt0218.  "*-CS2021000218-05.12.2022-#94933-JT

data: mensagem          type string,
      calcular          type j_1bnflin-menge,
      v_frac            type p decimals 3,
      resultado         type string,
      vsubmit           type c,
      verro             type c,
      purl              type string,
      wg_flag,
      tp_metodo         type string,
      vcod_mapa_receita type string.

data: tg_return type table of zmme_cl,
      wg_return type zmme_cl,
      wg_matnr  type zmme_cl,
      l_tabix   type sy-tabix.

data: it_zauth_webservice type table of ty_zauth_webservice,
      wa_zauth_webservice type ty_zauth_webservice,
      it_zauth_ws_0001    type table of zauth_ws_0001,
      wa_zauth_ws_0001    type zauth_ws_0001.

data: ob_web_service type ref to zcl_webservice.
data: e_reason     type string,
      json_retorno type string,
      e_http       type ref to  if_http_client,
      e_xml        type string.


data: ra_nftype type range of j_1bnfdoc-nftype,
      wr_nftype like line of ra_nftype,
      ra_docnum type range of j_1bnfdoc-docnum,
      wr_docnum like line of ra_docnum.

data: it_value like rgsb4 occurs 0 with header line,
      r_dt_def type range of j_1bnfdoc-docdat with header line,
      r_dt_sem type range of j_1bnfdoc-docdat with header line,
      r_burks  type range of j_1bnfdoc-bukrs  with header line,
      r_erros  type range of zsdt0267-msg  with header line.

constants: c_023          type c  length 3 value '023',
           c_mch1         type c  length 4 value 'MCH1',
           retorno_cancel type char30 value 'Registro salvo com sucesso'.


create object ob_web_service.

*-CS2022000910-06.06.2023-#91627-JT-inicio
select-options: p_docnum for j_1bnfdoc-docnum,                  " NO-DISPLAY,
                p_nftype for j_1bnfdoc-nftype,                  " NO-DISPLAY,
                p_docdat for j_1bnfdoc-docdat default sy-datum. " NO-DISPLAY.
*-CS2022000910-06.06.2023-#91627-JT-fim

initialization.


start-of-selection.

*-CS2022000910-06.06.2023-#91627-JT-inicio
*---------------------------------------------
* se tem Job ativo, abandona
*---------------------------------------------
  free: t_status.
  append 'R' to t_status.

  if sy-batch = abap_true.
    try .
        zcl_job=>get_job_programa_execucao(
          exporting
            i_progname   = sy-cprog    " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = t_status    " Status de Jobs
          importing
            e_quantidade = data(e_qtd) ).
      catch zcx_job.
    endtry.

    if e_qtd > 1.
      exit.
    endif.
  endif.
*-CS2022000910-06.06.2023-#91627-JT-fim

  select  service  url  from zauth_webservice
      into table it_zauth_webservice
   where service like 'INDEA_%'.

  check it_zauth_webservice is not initial.

  select *  from zauth_ws_0001 into table it_zauth_ws_0001
    for all entries in it_zauth_webservice
   where service eq it_zauth_webservice-service .


  select *
    from t001w into table it_t001w
    where regio = 'MT'.

  select 'I' as sign,
         'EQ' as option,
         msg as low
    from zsdt0267
    into table @r_erros.

  perform: z_trata_filtro.

  perform: z_busca_defensivos,            "Busca Notas de Defensivos
           z_tratar_dados_defensivos.

  refresh: it_lfa1, it_kna1,  it_j_1bnflin,  it_zmmt0102,  it_zsdt0210, it_doc, it_mseg, it_ekbe, tmseg, tekbe, it_mseg_03.


  perform: z_busca_sementes,              "Buca Notas de Sementes
           z_tratar_dados_sementes.


form z_trata_filtro.

  refresh: ra_nftype, ra_docnum.
  clear:   wr_nftype, wr_docnum.

  if p_docnum  is not initial and p_nftype  is not initial.

    loop at p_nftype.
      wr_nftype-sign    = 'I'.
      wr_nftype-option  = 'EQ'.
      wr_nftype-low     = p_nftype-low.
      wr_nftype-high    = p_nftype-high.
      append wr_nftype to ra_nftype.
      clear wr_nftype.
    endloop.

    loop at p_docnum.
      wr_docnum-sign    = 'I'.
      wr_docnum-option  = 'EQ'.
      wr_docnum-low     = p_docnum-low.
      wr_docnum-high    = p_docnum-high.
      append wr_docnum to ra_docnum.
      clear wr_docnum.
    endloop.

    loop at p_docdat.
      r_dt_def-sign    = 'I'.
      r_dt_def-option  = 'GE'.
      r_dt_def-low     =  p_docdat-low.
      append r_dt_def.
    endloop.

    loop at p_docdat.
      r_dt_sem-sign    = 'I'.
      r_dt_sem-option  = 'GE'.
      r_dt_sem-low     =  p_docdat-low.
      append r_dt_sem.  "*-CS2022000910-06.06.2023-#91627-JT
    endloop.

    perform z_busca_seq_cfop.

  else.
    wr_nftype-sign    = 'I'.
    wr_nftype-option  = 'EQ'.
    wr_nftype-low     = 'NE'. "OK
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'YE'.  "OK
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'YD'.  "OK
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'YF'. "OK
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'ZA'.  "OK
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'ZB'.  "OK
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'ZD'.   "OK
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'ZH'.   "OK
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'ZL'.   "OK
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'ZO'.   "OK
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'ZR'.
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'ZT'.
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'ZU'.
    append wr_nftype to ra_nftype.
    wr_nftype-low     = 'ZV'. "OK
    append wr_nftype to ra_nftype.
*    WR_NFTYPE-LOW     = 'N4'.
*    APPEND WR_NFTYPE TO RA_NFTYPE.
    clear wr_nftype.

    perform z_busca_set.

  endif.
endform.

***************************************************************
* valida status defensivos
***************************************************************
form f_valida_status_defensivos.

  check p_docnum[] is initial.

  select *
    from zsdt0212
    into table @data(t_0212)
     for all entries in @it_defensivos
   where docnum = @it_defensivos-docnum.

  sort t_0212 by docnum.

  loop at it_defensivos into wa_defensivos.
    l_tabix = sy-tabix.

    read table t_0212 into data(w_0212) with key docnum = wa_defensivos-docnum
                                        binary search.
    if sy-subrc = 0 and w_0212-status <> 'E'.
      delete it_defensivos index l_tabix.
    endif.
  endloop.

endform.

***************************************************************
* valida status sementes
***************************************************************
form f_valida_status_sementes.

  check p_docnum[] is initial.

  select *
    from zsdt0212
    into table @data(t_0212)
     for all entries in @it_sementes
   where docnum = @it_sementes-docnum.

  sort t_0212 by docnum.

  loop at it_sementes into wa_sementes.
    l_tabix = sy-tabix.

    read table t_0212 into data(w_0212) with key docnum = wa_sementes-docnum
                                        binary search.
    if sy-subrc = 0 and w_0212-status <> 'E'.
      delete it_sementes index l_tabix.
    endif.
  endloop.

endform.

***************************************************************
* busca defensivos
***************************************************************
form z_busca_defensivos .

  data vxblnr  type ekbe-xblnr.
  data vnfenum  type j_1bnfdoc-nfenum.

  select a~docnum, a~docsta, a~cancel,     b~pstdat, b~nfenum,
         b~nfnum,  b~series, b~model,      b~bukrs,  b~branch,
         b~nftype, b~doctyp, b~cnpj_bupla, b~cgc,    b~parid,
         b~docdat, b~regio,  c~cfop
    from j_1bnfdoc  as  b
    inner join j_1bnfe_active as a on a~docnum = b~docnum
    inner join j_1bnflin      as c on c~docnum = b~docnum
    into corresponding fields of table @it_defensivos
    for all entries in @it_t001w
   where  b~docnum in @ra_docnum
     and  b~bukrs  in @r_burks
     and  b~branch eq @it_t001w-werks
     and  b~pstdat in @r_dt_def
     and  b~nftype in @ra_nftype
     and  b~model  in ('55', '01', '04')
     and  a~docsta = '1'
     and  c~matkl eq '658445'.

*  SELECT a~docnum, a~docsta, a~cancel,     b~pstdat, b~nfenum,
*         b~nfnum,  b~series, b~model,      b~bukrs,  b~branch,
*         b~nftype, b~doctyp, b~cnpj_bupla, b~cgc,    b~parid,
*         b~docdat, b~regio
*    FROM j_1bnfe_active AS  a
*    INNER JOIN j_1bnfdoc AS b ON b~docnum = a~docnum
*    INTO CORRESPONDING FIELDS OF TABLE @it_defensivos
*    FOR ALL ENTRIES IN @it_t001w
*   WHERE  a~docnum IN @ra_docnum
*     AND  b~bukrs  IN @r_burks
*     AND  b~branch EQ @it_t001w-werks
*     AND  a~docsta = '1'
*     AND  b~docdat IN @r_dt_def
*     AND  b~nftype IN @ra_nftype
*     AND  b~model  IN ('55', '01', '04')
*     AND EXISTS ( SELECT docnum FROM j_1bnflin AS  c WHERE c~docnum = b~docnum
*                                                     AND   c~matkl EQ '658445' ).
*
  select b~docnum, b~docstat, b~cancel,     b~pstdat, b~nfenum,
         b~nfnum,  b~series,  b~model,      b~bukrs,  b~branch,
         b~nftype, b~doctyp,  b~cnpj_bupla, b~cgc,    b~parid,
         b~docdat, b~regio
      from j_1bnfdoc as b
     into table @it_doc
        for all entries in @it_t001w
      where b~docnum  in @ra_docnum
       and  b~bukrs   in @r_burks
       and  b~branch  eq @it_t001w-werks
       and  b~pstdat  in @r_dt_def
       and  b~nftype  in ('E1', 'N4')
*       AND  B~DOCSTAT EQ '1'
       and  b~model   in ('55', '01', '04')
       and exists ( select docnum from j_1bnflin as  c where c~docnum = b~docnum
                                                        and   c~matkl eq '658445' ).

  loop at it_doc into wa_doc.
    move wa_doc to wa_defensivos.
    append wa_defensivos to it_defensivos.
    clear:  wa_doc, wa_defensivos.
  endloop.

*-CS2022000910-06.06.2023-#91627-JT-inicio
  perform f_valida_status_defensivos.
*-CS2022000910-06.06.2023-#91627-JT-fim

  check it_defensivos is not initial.

  select *
    from lfa1 into table it_lfa1
    for all entries in it_defensivos
  where lifnr eq it_defensivos-parid.

  select *
    from kna1 into table it_kna1
   for all entries in it_defensivos
     where kunnr eq it_defensivos-parid.

  select m~ihivi,  j~*
    from j_1bnflin  as j  inner join mara as m on m~matnr = j~matnr
   into table @it_j_1bnflin
   for all entries in @it_defensivos
  where j~docnum eq @it_defensivos-docnum
    and j~matkl  eq '658445'
    and m~ihivi  ne 'X'.

  select *
    from zsdt0210 into table it_zsdt0210
   for all entries in it_j_1bnflin
   where matnr eq   it_j_1bnflin-matnr.

  select *
     from zsdt0212 into table it_zsdt0212
    for all entries in it_defensivos
   where docnum eq   it_defensivos-docnum.

  loop at it_defensivos into wa_defensivos.

    if wa_defensivos-nfnum is not initial.
      vnfenum = |{ wa_defensivos-nfnum alpha = out }|.
    else.
*      VNFENUM = |{ WA_DEFENSIVOS-NFENUM ALPHA = OUT }|.
      vnfenum =  wa_defensivos-nfenum.
      shift vnfenum left deleting leading '0'.
    endif.

    vxblnr = |{ vnfenum }{ '-' }{ wa_defensivos-series }|.

    read table it_j_1bnflin into wa_j_1bnflin with key docnum = wa_defensivos-docnum.

    case wa_j_1bnflin-reftyp.

      when 'LI'.
        select   ebeln  ebelp  belnr
                 gjahr  lfbnr  lfpos
                 lfgja  xblnr
          from ekbe into table tekbe
         where belnr eq wa_j_1bnflin-refkey+0(10)
          and  gjahr eq wa_j_1bnflin-refkey+10(4).

        sort tekbe by ebeln ebelp.
        delete adjacent duplicates from tekbe comparing ebeln ebelp.

        if wa_defensivos-nftype eq 'ZR'.

          loop at tekbe into wekbe.
            select  distinct *
              from ekbe  as e into table it_ekbe
               where  e~ebeln eq wekbe-ebeln
                and   e~ebelp eq wekbe-ebelp
                and   e~vgabe eq '1'
                and   e~xblnr eq vxblnr
                and   e~shkzg eq  'H'.

            if sy-subrc = 0.
              select  s~*
                from mseg as s
                inner join mara as m on m~matnr = s~matnr
                into table @tmseg
                for all entries in @it_ekbe
              where s~mblnr eq @it_ekbe-belnr
               and  s~mjahr eq @it_ekbe-gjahr
               and  s~shkzg eq 'H'
               and  m~ihivi ne 'X'.
            endif.
          endloop.

          loop at tmseg  into wmseg.
            wmseg-docnum = wa_defensivos-docnum.
            append wmseg to it_mseg.
            clear wmseg.
          endloop.

        else.

          loop at  tekbe into wekbe.

            if wekbe-lfbnr is initial.
              select  distinct *
                from ekbe  as e into table it_ekbe
              where  e~ebeln eq wekbe-ebeln
               and   e~ebelp eq wekbe-ebelp
               and   e~vgabe eq '1'
               and   e~xblnr eq vxblnr
               and   e~shkzg eq  'S'
               and   e~belnr eq ( select max( k~belnr )
                                  from  ekbe as k
                                  where  k~ebeln = e~ebeln
                                   and   k~ebelp = e~ebelp
                                   and   k~vgabe = e~vgabe
                                   and   k~xblnr = e~xblnr
                                   and   k~shkzg = e~shkzg ) .

              if sy-subrc = 0.
                select  s~*
                  from mseg as s
                  inner join mara as m on m~matnr = s~matnr
                  into table @tmseg
                  for all entries in @it_ekbe
                where s~mblnr eq @it_ekbe-belnr
                 and  s~mjahr eq @it_ekbe-gjahr
                 and  s~shkzg eq 'S'
                 and  m~ihivi ne 'X'.
              endif.

            else.

              select  s~*
                from mseg as s
                inner join mara as m on m~matnr = s~matnr
                into table @tmseg
                where s~mblnr eq  @wekbe-lfbnr
                 and  s~mjahr eq  @wekbe-lfgja
                 and  s~shkzg eq 'S'
                 and  m~ihivi ne 'X'.

            endif.

            loop at tmseg  into wmseg.
              wmseg-docnum = wa_defensivos-docnum.
              append wmseg to it_mseg.
              clear wmseg.
            endloop.

            clear wekbe.
          endloop.
        endif.

      when 'ZW'.

        select  *
          from zfiwrt0008 into table @data(it_zfiwrt0008)
         where seq_lcto eq @wa_j_1bnflin-refkey.

        if it_zfiwrt0008 is not initial.
          select  s~*
            from mseg  as s
            inner join mara as m on m~matnr = s~matnr
            into table @tmseg
            for all entries in @it_zfiwrt0008
          where  s~mblnr eq @it_zfiwrt0008-mblnr
            and  s~mjahr eq @it_zfiwrt0008-mjahr
            and  s~shkzg eq 'S'
            and  m~ihivi ne 'X'.

          loop at tmseg  into wmseg.
            wmseg-docnum = wa_defensivos-docnum.
            append wmseg to it_mseg.
            clear wmseg.
          endloop.
        endif.

      when 'MD'.

        select s~*
          from mseg as s
            inner join mara as m on m~matnr = s~matnr
          into table @tmseg
         where s~mblnr eq @wa_j_1bnflin-refkey+0(10)
          and  s~mjahr eq @wa_j_1bnflin-refkey+10(4)
          and  s~shkzg eq 'S'
          and  m~ihivi ne 'X'.

        loop at tmseg  into wmseg.
          wmseg-docnum = wa_defensivos-docnum.
          append wmseg to it_mseg.
          clear wmseg.
        endloop.

    endcase.

    sort it_mseg by mblnr mjahr zeile docnum.
    delete adjacent duplicates from it_mseg comparing mblnr mjahr zeile docnum.
*    MOVE-CORRESPONDING it_mseg TO it_mseg_02.
    append lines of it_mseg[] to it_mseg_02[].

    clear: wa_defensivos, wa_j_1bnflin, vxblnr, wa_mseg.
    free: it_mseg[].
  endloop.

  it_mseg[] = it_mseg_02[].

  sort it_mseg by matnr werks charg docnum.
  delete adjacent duplicates from it_mseg comparing matnr werks charg docnum.

  loop at it_mseg into wa_mseg.

    move wa_mseg to wa_mseg_03.
    clear wa_mseg_03-menge.

    loop at it_mseg_02 into wa_mseg_02
        where matnr = wa_mseg-matnr
         and  charg = wa_mseg-charg
         and  mjahr = wa_mseg-mjahr
         and  docnum = wa_mseg-docnum.

      wa_mseg_03-menge =  wa_mseg_03-menge + wa_mseg_02-menge.
    endloop.

    append wa_mseg_03 to it_mseg_03.
    clear: wa_mseg,  wa_mseg_02, wa_mseg_03.

  endloop.

*** SOC - CS0989356 - Anderson Matoso - 02/06/2022
  data lv_unid     type c length 50.
  data lv_unit_out type t006-msehi.

  sort it_zsdt0210 by matnr.

  loop at it_mseg_03 assigning field-symbol(<it_mseg>).
    if it_zsdt0210 is not initial.

      clear wa_zsdt0210.

      read table it_zsdt0210
            into wa_zsdt0210
        with key matnr = <it_mseg>-matnr binary search.

      if sy-subrc is initial.

        select single unidade
          from zsdt0201
          into @data(lv_unidade)
         where id_produto = @wa_zsdt0210-id_matnr_idea.

        if sy-subrc is initial.

          lv_unid = lv_unidade.

          select single sigla
            from zsdt0204
            into @data(lv_sigla)
           where nome = @lv_unid.

          if sy-subrc is initial.
            if <it_mseg>-meins ne lv_sigla.

              lv_unit_out = lv_sigla.

              if <it_mseg>-meins = 'KG' or <it_mseg>-meins = 'G' and  lv_unit_out = 'KG' or lv_unit_out = 'G'.
                try.
                    call function 'UNIT_CONVERSION_SIMPLE'
                      exporting
                        input                = <it_mseg>-menge
                        unit_in              = <it_mseg>-meins
                        unit_out             = lv_unit_out
                      importing
                        output               = <it_mseg>-menge
                      exceptions
                        conversion_not_found = 01
                        overflow             = 02
                        others               = 03.
                  catch cx_root.
                    write: / 'Erro Conversão ' , <it_mseg>-mblnr.
                endtry.
              endif.

              if sy-subrc eq 0.
                <it_mseg>-meins = lv_unit_out.
                <it_mseg>-erfme = lv_unit_out.
                <it_mseg>-bprme = lv_unit_out.

                <it_mseg>-erfmg = <it_mseg>-menge.
                <it_mseg>-bpmng = <it_mseg>-menge.
              endif.

            endif.
          endif.
        endif.
      endif.
    endif.
  endloop.
*** EOC - CS0989356 - Anderson Matoso - 02/06/2022

  refresh:  it_mseg, it_mseg_02.
  move-corresponding it_mseg_03 to it_mseg.

endform.

form z_envia_json using p_url      type string
                        p_username type zauth_ws_0001-username
                        p_password type zauth_ws_0001-password
                        p_metodo   type string
  changing resultado type string.

  data: v_url             type string,
        v_username        type string,
        v_password        type string,
        v_metodo          type string,
        lc_integra_sisdev type ref to zcl_integracao_sisdev. "*-CS2025000249-10.03.2025-#169739-JT-inicio

  create object: lc_integra_sisdev.  "*-CS2025000249-10.03.2025-#169739-JT-inicio

  v_url        = p_url.
  v_username   = p_username.
  v_password   = p_password.
  v_metodo     = p_metodo.

*-CS2025000249-10.03.2025-#169739-JT-inicio
*  cl_http_client=>create_by_url(
*    EXPORTING
*      url                = |{ v_url }|
*    IMPORTING
*      client             = e_http
*    EXCEPTIONS
*      argument_not_found = 1
*      plugin_not_active  = 2
*      internal_error     = 3 ).
*
*  CALL METHOD e_http->request->set_header_field
*    EXPORTING
*      name  = '~request_method'
*      value = v_metodo.
*
*  CALL METHOD e_http->request->set_header_field
*    EXPORTING
*      name  = '~server_protocol'
*      value = 'HTTP/1.1'.
*
*  CALL METHOD e_http->request->set_header_field
*    EXPORTING
*      name  = 'Content-Type'
*      value = 'application/json; charset=UTF-8'.
*
*
*  CALL METHOD e_http->request->set_header_field
*    EXPORTING
*      name  = 'Accept'
*      value = 'application/json; charset=UTF-8'.
*
*  e_http->authenticate( username = v_username  password = v_password ).
*
*  CLEAR json_retorno.
*  ob_web_service->zif_webservice~consultar(
*    EXPORTING
*      i_http                     = e_http
*      i_xml                      = e_xml
*    IMPORTING
*      e_reason                   = e_reason
*    RECEIVING
*      e_resultado                = json_retorno
*    EXCEPTIONS
*      http_communication_failure = 1
*      http_invalid_state         = 2
*      http_processing_failed     = 3
*      http_invalid_timeout       = 4
*      OTHERS                     = 5 ).
*
*  resultado = json_retorno.
*-CS2025000249-10.03.2025-#169739-JT-fim

*-CS2025000249-10.03.2025-#169739-JT-inicio
*------------------------------------
*---integrar SISDEV / INDEA
*------------------------------------
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = 50
      text       = 'Aguarde...Enviando Informações...'.

  try .
      resultado = lc_integra_sisdev->set_integra_sisdev( exporting i_servico = conv #( wa_zauth_webservice-service )
                                                                   i_metodo  = v_metodo
                                                                   i_docnum  = wzsdt0212-docnum
                                                                   i_bukrs   = wa_zauth_ws_0001-bukrs
                                                                   i_branch  = wa_zauth_ws_0001-branch
                                                                   i_json    = e_xml ).
    catch zcx_integracao into data(ex_integra).
      message id ex_integra->msgid type 'S' number ex_integra->msgno with ex_integra->msgv1 ex_integra->msgv2 ex_integra->msgv3 ex_integra->msgv4
      display like 'E'.

    catch zcx_error into data(ex_error).
      message id ex_error->msgid type 'S' number ex_error->msgno with ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4
      display like 'E'.
  endtry.
*-CS2025000249-10.03.2025-#169739-JT-fim

endform.


form get_next_number using p_object
                           p_nr_range
                      changing p_number.

  clear p_number.

  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = p_nr_range
      object                  = p_object
    importing
      number                  = p_number
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.
  if sy-subrc <> 0.
    clear p_number.
    message e836(sd) with 'O intervalo de numeração não foi encontrado!'.
  else.
    wg_flag = 'X'.
  endif.
endform.


form z_ws_entrada_df.
  data wa_itens    type ty_item_entrada_agrotoxico.

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro  = abap_false.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                               bukrs   = wa_defensivos-bukrs
                                                               branch  = wa_defensivos-branch.
    if sy-subrc = 0.
      wa_entrada_agrotoxico-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      verro  = abap_true.
      clear mensagem.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.
      exit.
    endif.

    wa_entrada_agrotoxico-cnpj_revenda = wa_defensivos-cnpj_bupla.
    wa_entrada_agrotoxico-origem_armazenador = 'false'.

    if wa_defensivos-nfenum is not initial.
      wa_entrada_agrotoxico-nr_nf = wa_defensivos-nfenum.
    else.
      wa_entrada_agrotoxico-nr_nf = wa_defensivos-nfnum.
    endif.

    if wa_defensivos-series is not initial.
      wa_entrada_agrotoxico-serie_nf = wa_defensivos-series.
    else.
      clear mensagem.
      verro = abap_true.
      mensagem = 'A Série da nota Fiscal não está preenchido!'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.


    concatenate wa_defensivos-docdat+0(4) '-'  wa_defensivos-docdat+4(2) '-' wa_defensivos-docdat+6(2) into wa_entrada_agrotoxico-dt_nf.
    concatenate wa_defensivos-pstdat+0(4) '-'  wa_defensivos-pstdat+4(2) '-' wa_defensivos-pstdat+6(2) into wa_entrada_agrotoxico-dt_entrada.

    read table it_lfa1 into wa_lfa1 with key lifnr = wa_defensivos-parid.
    if sy-subrc = 0.
      if wa_lfa1-stcd1 is not initial.
        wa_entrada_agrotoxico-cnpj_fornecedor = wa_lfa1-stcd1.
      else.
        wa_entrada_agrotoxico-cnpj_fornecedor = wa_lfa1-stcd2.
      endif.
    endif.


    wa_entrada_agrotoxico-observacao = ' '.

    loop at  it_mseg into wa_mseg   where docnum = wa_defensivos-docnum.

      select single *
         from zsdt0210 into wa_zsdt0210
        where matnr eq wa_mseg-matnr.

      if sy-subrc = 0.
        wa_itens-cod_produto  =  wa_zsdt0210-id_matnr_idea.
      else.
        verro  = abap_true.
        wa_mseg-matnr = |{ wa_mseg-matnr alpha = out }|.

        concatenate 'Material' wa_mseg-matnr 'não realizado o DE/PARA na transação ZSDT0154 ' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.

      wa_itens-lote   = wa_mseg-charg.

      select single *
        from zsdt0201 into @data(wa_zsdt0201)
        where id_produto eq @wa_zsdt0210-id_matnr_idea.

      if sy-subrc = 0.

        select single *
          from zsdt0204 into @data(wa_zsdt0204)
         where nome eq @wa_zsdt0201-unidade.

        if sy-subrc = 0.
          if wa_mseg-meins =  wa_zsdt0204-sigla.
            clear calcular.
            calcular =  wa_mseg-menge / wa_zsdt0201-volume.

            clear  v_frac.
            v_frac = frac( calcular ).

            if v_frac <> '0'.
              verro  = abap_true.
*              MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
              concatenate 'Quantidade vendida do Material' wa_mseg-matnr 'não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            else.
              wa_itens-qnt_produto = calcular.
            endif.
          else.
            verro  = abap_true.
*            MENSAGEM = 'Unidade de Material diferente da unidade Material INDEA'.
            concatenate 'A U.M. ' wa_mseg-meins 'do Material' wa_mseg-matnr 'é diferente da U.M.' wa_zsdt0204-sigla 'do Material INDEA.' into mensagem separated by space.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          endif.
        else.
          clear: mensagem.
          verro  = abap_true.
*          MENSAGEM  = ' Unidade de Medida do produto autorizado INDEA não encontrada na tabela de Unidade de Medidas do Indea !'.
          concatenate 'A Unidade de Medida' wa_zsdt0201-unidade 'do produto autorizado INDEA, não foi encontrada na tabela de Unidade de Medidas do próprio INDEA' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          clear wa_zsdt0214.
          commit work.
          exit.

        endif.
      endif.

      append value #( cod_produto  = wa_itens-cod_produto
                      lote         = wa_itens-lote
                      qnt_produto  = wa_itens-qnt_produto
                     ) to wa_entrada_agrotoxico-itens.
    endloop.

    check verro = abap_false.

    if wa_entrada_agrotoxico-itens  is initial.

      verro  = abap_true.
      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = 'Nota com todos itens Adjuvante'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_entrada_agrotoxico-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  'Nota com todos itens Adjuvante'.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
      exit.
    endif.

    check verro is initial.

    if wzsdt0212-id is not initial.
      wa_entrada_agrotoxico-cod_lancamento_usuario = wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021 using  'ZSEQ_INDEA'
                                                        '01'
                                                 changing wa_entrada_agrotoxico-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.
    e_xml = /ui2/cl_json=>serialize( data = wa_entrada_agrotoxico compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                               wa_zauth_ws_0001-username
                               wa_zauth_ws_0001-password
                               tp_metodo  changing resultado.

    translate resultado to upper case.

    if  resultado ca sy-abcde .

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_entrada_agrotoxico-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros.  "= 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.

      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.

    elseif  resultado ca '0123456789'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_entrada_agrotoxico-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
    endif.
  endif.

endform.

form z_ws_cancel_df.

  clear: mensagem,  tp_metodo, resultado, purl.

  tp_metodo = 'PUT'.


  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ) .

    verro  = abap_false.

    wa_cancel_entrada_agrotoxico-cnpj_revenda = wa_defensivos-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key  service = wa_zauth_webservice-service
                                                                bukrs   = wa_defensivos-bukrs
                                                                branch  = wa_defensivos-branch.
    if sy-subrc = 0.
      wa_cancel_entrada_agrotoxico-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro  = abap_false.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_cancel_entrada_agrotoxico-cod_lancamento_usuario =  wzsdt0212-id.
    wa_cancel_entrada_agrotoxico-motivo_cancelamento    = 'Dados da Nota estão incorretos'.

    check verro = abap_false.

    if wzsdt0212-status = 'A'.

      purl =  wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_entrada_agrotoxico compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo  changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_agrotoxico-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'X'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_agrotoxico-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
    endif.

  else.
    wa_zsdt0212-docnum       =  wa_defensivos-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
  endif.
endform.


form z_ws_saida_df.

  data vstcd          like zsdt0205-cpfcnpj.
  data wa_itens_saida type ty_itens_saida_agrotoxico.
  data vnumeronf      type zsdt0218-numeronf.
  data vfator         type p decimals 8.
  data pessoa_aut     type char1.
  data cod_praga_aux  type char10.
  data lv_unit_out    type t006-msehi.

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro  = abap_false.

    read table it_j_1bnflin into wa_j_1bnflin with key docnum = wa_defensivos-docnum.

    if it_j_1bnflin is initial.

*&**************************************************************************
*&    Check se o material é Adjuvante.
*&
****************************************************************************
      verro  = abap_true.
      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = 'Nota com todos itens Adjuvante'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_saida_agrotoxico-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  'Nota com todos itens Adjuvante'.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
      exit.
    endif.

    if wa_defensivos-nftype = 'ZA' and wa_j_1bnflin-itmtyp <> 'ZC'.

      read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_AGROTOXICO'.

      read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                                 bukrs   =  wa_defensivos-bukrs
                                                                 branch  =  wa_defensivos-branch.
      if sy-subrc = 0.
        wa_saida_agrotoxico-hash_revenda = wa_zauth_ws_0001-add01.
      else.
        clear mensagem.
        verro  = abap_true.
        concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.

      wa_saida_agrotoxico-cnpj_revenda =  wa_defensivos-cnpj_bupla.

      if  wa_defensivos-regio = 'MT'.
        wa_saida_agrotoxico-destino_fora_m_t  = 'false'.
      else.
        wa_saida_agrotoxico-destino_fora_m_t  = 'true'.
      endif.

      select single * from zsdt0216 into @data(wa_zsdt0216)
        where kunnr           eq @wa_defensivos-parid
         and  tipo            eq 'C'
         and  setor_atividade eq 'A'
         and  revenda         eq 'S'.

      if sy-subrc = 0.
        wa_saida_agrotoxico-destino_revenda     = 'true'.
      else.
        wa_saida_agrotoxico-destino_revenda     = 'false'.
      endif.

      wa_saida_agrotoxico-destino_armazenador = 'false'.

      wa_saida_agrotoxico-observacao = ' '.

      if wa_defensivos-nfenum is not initial.
        wa_saida_agrotoxico-nr_nf = wa_defensivos-nfenum.
      else.
        wa_saida_agrotoxico-nr_nf = wa_defensivos-nfnum.
      endif.

      wa_saida_agrotoxico-serie_nf = wa_defensivos-series.

      concatenate wa_defensivos-docdat+0(4) '-' wa_defensivos-docdat+4(2) '-' wa_defensivos-docdat+6(2) into wa_saida_agrotoxico-data_nf.

      read table it_j_1bnflin into data(wa_flin) with key docnum = wa_defensivos-docnum.

*      IF WA_SAIDA_AGROTOXICO-DESTINO_REVENDA = 'false' AND WA_SAIDA_AGROTOXICO-DESTINO_ARMAZENADOR = 'false'.
      if wa_saida_agrotoxico-destino_revenda eq 'false' and wa_saida_agrotoxico-destino_armazenador eq 'false' and wa_saida_agrotoxico-destino_fora_m_t eq 'false'.
        clear wa_zsdt0001.
        select  single *  from zsdt0001 into @wa_zsdt0001
         where fatura_prod eq @wa_flin-refkey.
*       SELECT  SINGLE *  FROM zsdt0001 INTO @DATA(wa_zsdt0001)
*        WHERE fatura_prod EQ @wa_flin-refkey.

        select single *  from zsdt0139 into @data(wa_zsdt0139)
         where nro_cgd eq @wa_zsdt0001-nro_cg
          and  status  ne 'X'.

        select single * from lfa1 into @data(wa_lfa1)
          where lifnr eq @wa_zsdt0139-cod_ce.

        select  single * from zsdt0208 into @data(wa_zsdt0208)
          where cnpj eq @wa_lfa1-stcd1.

        if sy-subrc = 0.

          if wa_zsdt0208-autorizado = 'X'.
            wa_saida_agrotoxico-cnpj_ure = wa_zsdt0208-cnpj.
          else.
            clear mensagem.
            verro     = abap_true.
            mensagem  = 'URE não Autorizada pela base do SISDEV-INDEA'.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          endif.
        else.
          clear mensagem.
          verro     = abap_true.
          mensagem  = 'URE não Cadastrada na base do SISDEV-INDEA'.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          clear wa_zsdt0214.
          commit work.
          exit.
        endif.
      else.
        wa_saida_agrotoxico-cnpj_ure = ''.
      endif.


      read table it_kna1 into wa_kna1 with key kunnr = wa_defensivos-parid.

      if wa_saida_agrotoxico-destino_fora_m_t  = 'false'.

        select single * from zsdt0206 into @data(wa_zsdt0206)
          where  kunnr eq @wa_defensivos-parid.

        if sy-subrc = 0.
          wa_saida_agrotoxico-cpf_cnpj_destino = wa_zsdt0206-cnpj.
        else.

          if wa_kna1-stcd1 is not initial.
            vstcd = wa_kna1-stcd1.
          else.
            vstcd = wa_kna1-stcd2.
          endif.

          select single * from zsdt0205 into @data(wa_zsdt0205)
            where cpfcnpj eq @vstcd.

          if sy-subrc = 0.
            wa_saida_agrotoxico-cpf_cnpj_destino = wa_zsdt0205-cpfcnpj.
            pessoa_aut = 'X'.

          else.
            clear mensagem.
            verro     = abap_true.
            mensagem  = 'Cliente SAP não Localizado na base do SISDEV-INDEA'.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          endif.
        endif.

      else.
        if wa_kna1-stcd1 is not initial.
          wa_saida_agrotoxico-cpf_cnpj_destino = wa_kna1-stcd1.
        elseif wa_kna1-stcd2 is not initial.
          wa_saida_agrotoxico-cpf_cnpj_destino = wa_kna1-stcd2.
        endif.
      endif.

      wa_kna1-name1 = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = conv #( wa_kna1-name1 ) ) ).

      if wa_defensivos-regio <> 'MT'.
        wa_saida_agrotoxico-nome_destino  = wa_kna1-name1.
        wa_saida_agrotoxico-cod_municipio = wa_kna1-txjcd+3(12).
      else.
        wa_saida_agrotoxico-nome_destino  = ' '.
        wa_saida_agrotoxico-cod_municipio = ' '.
      endif.

      clear vnumeronf.

      vnumeronf = wa_saida_agrotoxico-nr_nf.
      condense  vnumeronf.

      if wa_saida_agrotoxico-destino_revenda eq 'false'.

*-CS2021000218-05.12.2022-#94933-JT-inicio
        free: it_zsdt0218.

        select nro_cg, ch_referencia
          into @data(w_zsdt0001)
          from zsdt0001
            up to 1 rows
         where nro_nf_prod = @wa_defensivos-docnum.
        endselect.

        if sy-subrc = 0.
          select nro_cgd, ch_referencia, id, receitakey
            into table @data(t_zsdt0298)
            from zsdt0298
           where nro_cgd       = @w_zsdt0001-nro_cg
             and ch_referencia = @w_zsdt0001-ch_referencia
             and status        = '4'
             and cancelado     = @abap_false.

          if sy-subrc = 0.
            select *
              from zsdt0218
              into table it_zsdt0218
               for all entries in t_zsdt0298
             where receitakey = t_zsdt0298-receitakey
               and cancelada  = abap_false.
          endif.
        endif.

        if it_zsdt0218[] is initial.
          select *
            from zsdt0218
            into table it_zsdt0218
           where numeronf     eq vnumeronf
             and numeropedido eq wa_defensivos-branch
             and cancelada    ne 'X'.
        endif.
*-CS2021000218-05.12.2022-#94933-JT-fim

*       IF sy-subrc EQ 0.                "*-CS2021000218-05.12.2022-#94933-JT
        if it_zsdt0218[] is not initial. "*-CS2021000218-05.12.2022-#94933-JT
          select * from zsdt0219 into table @data(it_zsdt0219)
            for all entries in @it_zsdt0218
            where numeroreceita  eq @it_zsdt0218-numeroreceita
              and numeropedido   eq @wa_defensivos-branch
              and cpfrt          eq @it_zsdt0218-cpfrt.

          if sy-subrc eq 0.
            loop at it_zsdt0219 assigning field-symbol(<fs_219>).
              call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
                exporting
                  input  = <fs_219>-codigomapa
                importing
                  output = <fs_219>-codigomapa.
            endloop.
            select * from zsdt0203 into table @data(it_zsdt0203)
               for all entries in @it_zsdt0219
               where id_praga  eq @it_zsdt0219-codigoindeapraga.

          endif.
        else.
          clear mensagem.
          verro     = abap_true.
          mensagem  = 'Não Localizado no SAP o RA para esta nota fiscal'.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          clear wa_zsdt0214.
          commit work.
          exit.
        endif.
      endif.

      if  pessoa_aut <> 'X'.

        if wa_saida_agrotoxico-destino_revenda = 'false' and wa_saida_agrotoxico-destino_armazenador = 'false' and wa_saida_agrotoxico-destino_fora_m_t = 'false'.

          select single * from vbfa into @data(wa_vbfa)
            where vbeln   eq @wa_flin-refkey
              and vbtyp_n	eq 'M'
              and vbtyp_v	eq 'C'.

          select single * from zsdt0140 into @data(wa_zsdt0140)
            where nro_cgd eq @wa_zsdt0001-nro_cg
            and   vbeln   eq @wa_vbfa-vbelv
            and   status  ne 'X'.

          select single * from zsdt0082 into @data(wa_zsdt0082)
            where nro_sol eq @wa_zsdt0140-nro_sol
            and   seq     eq @wa_zsdt0140-seq.

          select single * from zsdt0132 into @data(wa_zsdt0132)
            where nr_rot eq @wa_zsdt0082-nr_rot.

          if wa_zsdt0132-id_propriedade eq 0.

            clear mensagem.
            verro     = abap_true.
*            MENSAGEM  = 'Roteiro SAP do Cliente está sem De/Para com ID propriedade INDEA'.
            concatenate 'Roteiro SAP: ' wa_zsdt0082-nr_rot ' do Cliente está sem De/Para com ID propriedade INDEA na Transação ZSDT0154.' into mensagem separated by space.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.

          else.
            wa_saida_agrotoxico-cod_propriedade = wa_zsdt0132-id_propriedade.
          endif.

        else.
          wa_saida_agrotoxico-cod_propriedade =  ' '.
        endif.

      endif.

      loop at it_j_1bnflin into wa_j_1bnflin
        where docnum =  wa_defensivos-docnum.

        select single * from zsdt0210 into @data(wa_zsdt0210)
          where matnr eq @wa_j_1bnflin-matnr.
**** IR024542
        if sy-subrc = 0.

          select single * from zsdt0201 into @data(wt201)
            where id_produto eq @wa_zsdt0210-id_matnr_idea.

          wa_itens_saida-cod_produto = wa_zsdt0210-id_matnr_idea.

        else.
          clear mensagem.
          verro  = abap_true.
          wa_j_1bnflin-matnr = |{ wa_j_1bnflin-matnr alpha = out }|.

          concatenate 'Material' wa_j_1bnflin-matnr 'não realizado o DE/PARA na transação ZSDT0154 ' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          clear wa_zsdt0214.
          commit work.
          exit.
        endif.

*        SELECT SINGLE * FROM ZSDT0201 INTO @DATA(WT201)
*          WHERE ID_PRODUTO EQ @WA_ZSDT0210-ID_MATNR_IDEA.
*
*        WA_ITENS_SAIDA-COD_PRODUTO = WA_ZSDT0210-ID_MATNR_IDEA.
****

        wa_itens_saida-lote          = wa_j_1bnflin-charg.

        if wa_saida_agrotoxico-destino_revenda   = 'false'.

          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
            exporting
              input  = wt201-codmapa
            importing
              output = wt201-codmapa.

          read table it_zsdt0219 into data(wa_zsdt0219) with key  codigomapa = wt201-codmapa.

          if sy-subrc = 0.
            clear vfator.

            wa_itens_saida-cod_cultura         =  wa_zsdt0219-codigoindeacultura.

            read table it_zsdt0203 into data(wa_zsdt0203) with key  id_praga = wa_zsdt0219-codigoindeapraga.

            if sy-subrc = 0.
              wa_itens_saida-cod_praga           =  wa_zsdt0219-codigoindeapraga.
            else.
              clear: mensagem, cod_praga_aux.
              verro     = abap_true.

              cod_praga_aux = wa_zsdt0219-codigoindeapraga.

              concatenate  'Código da Praga:' cod_praga_aux 'do RA, não encontrado na base do INDEA. Item' wa_j_1bnflin-itmnum 'da NF'  into mensagem  separated by space .

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            endif.

            if wa_zsdt0219-codigoindeatipoaplicacao eq 0.

              clear mensagem.
              verro     = abap_true.
              concatenate  'Receituário Nr:' wa_zsdt0219-numeroreceita 'está sem o código Tipo Aplicação' into mensagem  separated by space .

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            else.
              wa_itens_saida-cod_tipo_aplicacao  =   wa_zsdt0219-codigoindeatipoaplicacao.
            endif.

            if wa_zsdt0219-codigoindeatipoaplicacao <> '4'.
              if wa_zsdt0219-area eq 0. "Verificar se a quantidade de semente esta zerada. Ajuste 30/03/2021 IR026498 - AO
                clear mensagem.
                verro     = abap_true.
                concatenate  'Receituário Nr:' wa_zsdt0219-numeroreceita 'está sem a quantidade da area de aplicação' into mensagem  separated by space .

                perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
                wa_zsdt0214-data_atual   = sy-datum.
                wa_zsdt0214-hora_atual   = sy-uzeit.
                wa_zsdt0214-docnum       = wa_defensivos-docnum.
                wa_zsdt0214-origem       = '1'.
                wa_zsdt0214-mensagem     = mensagem.
                wa_zsdt0214-usnam        = sy-uname.
                modify zsdt0214 from wa_zsdt0214.
                clear wa_zsdt0214.
                commit work.
                exit.
              else.
* 122737 - PQ e SMC
                select single * from zsdt0202 into @data(wa_zsdt0202)
                  where id_tp_aplicacao eq @wa_zsdt0219-codigoindeatipoaplicacao
                  and id_unid_medida ne 0.
                if sy-subrc = 0.
                  wa_itens_saida-cod_unidade_medida = wa_zsdt0202-id_unid_medida.
                endif.
*                wa_itens_saida-cod_unidade_medida = '72'.
* 122737 - PQ e SMC
                vfator = ( wa_zsdt0219-area /  wa_zsdt0219-quantidade ).
                wa_itens_saida-area_qnt_tratada  = ( vfator * wa_j_1bnflin-menge ).
              endif.

            else.
              if wa_zsdt0219-qtdsementes eq 0. "Verificar se a quantidade de semente esta zerada. Ajuste 30/03/2021 IR026498 - AO
                clear mensagem.
                verro     = abap_true.
                concatenate  'Receituário Nr:' wa_zsdt0219-numeroreceita 'está sem a quantidade de semente tratada'   into mensagem  separated by space .

                perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
                wa_zsdt0214-data_atual   = sy-datum.
                wa_zsdt0214-hora_atual   = sy-uzeit.
                wa_zsdt0214-docnum       = wa_defensivos-docnum.
                wa_zsdt0214-origem       = '1'.
                wa_zsdt0214-mensagem     = mensagem.
                wa_zsdt0214-usnam        = sy-uname.
                modify zsdt0214 from wa_zsdt0214.
                clear wa_zsdt0214.
                commit work.
                exit.
              else.
* 122737 - PQ e SMC
                select single * from zsdt0202 into wa_zsdt0202
                  where id_tp_aplicacao eq wa_zsdt0219-codigoindeatipoaplicacao
                  and id_unid_medida ne 0.
                if sy-subrc = 0.
                  wa_itens_saida-cod_unidade_medida = wa_zsdt0202-id_unid_medida.
                endif.
*                wa_itens_saida-cod_unidade_medida = '107'.

                if wa_zsdt0219-codigoindeaunidademedida eq wa_zsdt0202-id_unid_medida.
                  wa_itens_saida-area_qnt_tratada = wa_zsdt0219-qtdsementes.
                else.
                  if wa_zsdt0219-codigoindeaunidademedida eq '107' and wa_zsdt0202-id_unid_medida eq '249'.
                    wa_itens_saida-area_qnt_tratada = wa_zsdt0219-qtdsementes / 1000.

                  else.
                    clear mensagem.
                    verro     = abap_true.
                    concatenate  'Receituário Nr:' wa_zsdt0219-numeroreceita 'está com divergencia na unidade de medida no Código Mapa (' wt201-codmapa ')' into mensagem  separated by space .

                    perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
                    wa_zsdt0214-data_atual   = sy-datum.
                    wa_zsdt0214-hora_atual   = sy-uzeit.
                    wa_zsdt0214-docnum       = wa_defensivos-docnum.
                    wa_zsdt0214-origem       = '1'.
                    wa_zsdt0214-mensagem     = mensagem.
                    wa_zsdt0214-usnam        = sy-uname.
                    modify zsdt0214 from wa_zsdt0214.
                    clear wa_zsdt0214.
                    commit work.
                    exit.

                  endif.
                endif.
*                wa_itens_saida-area_qnt_tratada = wa_zsdt0219-qtdsementes.
* 122737 - PQ e SMC
              endif.
            endif.
          else.
**** IR024542
*            CLEAR MENSAGEM.
*            VERRO     = ABAP_TRUE.
*            CONCATENATE  'Não encontrado a receita para o item ' WA_J_1BNFLIN-ITMNUM  INTO MENSAGEM  SEPARATED BY SPACE .
*
*            PERFORM GET_NEXT_NUMBER IN PROGRAM ZSDR021 USING  'ZSEQ_L_IND'  '01'  CHANGING  WA_ZSDT0214-ID.
*            WA_ZSDT0214-DATA_ATUAL   = SY-DATUM.
*            WA_ZSDT0214-HORA_ATUAL   = SY-UZEIT.
*            WA_ZSDT0214-DOCNUM       = WA_DEFENSIVOS-DOCNUM.
*            WA_ZSDT0214-ORIGEM       = '1'.
*            WA_ZSDT0214-MENSAGEM     = MENSAGEM.
*            WA_ZSDT0214-USNAM        = SY-UNAME.
*            MODIFY ZSDT0214 FROM WA_ZSDT0214.
*            CLEAR WA_ZSDT0214.
*            COMMIT WORK.

            loop at it_zsdt0219 into wa_zsdt0219.
              if vcod_mapa_receita is initial.
                vcod_mapa_receita = |{ wa_zsdt0219-codigomapa }|.
              else.
                vcod_mapa_receita = |{ vcod_mapa_receita } / { wa_zsdt0219-codigomapa }|.
              endif.
            endloop.

            clear mensagem.
            verro     = abap_true.
            concatenate  'Código Mapa (' wt201-codmapa ') informado no DE/PARA, difere dos códigos (' vcod_mapa_receita ') do R.A.' into mensagem  separated by space .

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear: wa_zsdt0214, vcod_mapa_receita.
            commit work.
            exit.
**** IR024542
          endif.

          read table  it_zsdt0218 into data(wa_zsdt0218) with key numeroreceita = wa_zsdt0219-numeroreceita.

          if sy-subrc = 0.
            wa_itens_saida-cpf_agronomo   =  wa_zsdt0218-cpfrt.
            wa_itens_saida-nr_art         =  wa_zsdt0218-numeroart.
            wa_itens_saida-nr_receita     =  wa_zsdt0218-numeroreceita.
*          ELSE.
*            CLEAR MENSAGEM.
*            VERRO     = ABAP_TRUE.
*            MENSAGEM  = 'Nota Fiscal sem Receituario Agronômico'.
*
*            PERFORM GET_NEXT_NUMBER IN PROGRAM ZSDR021 USING  'ZSEQ_L_IND'  '01'  CHANGING  WA_ZSDT0214-ID.
*            WA_ZSDT0214-DATA_ATUAL   = SY-DATUM.
*            WA_ZSDT0214-HORA_ATUAL   = SY-UZEIT.
*            WA_ZSDT0214-DOCNUM       = WA_DEFENSIVOS-DOCNUM.
*            WA_ZSDT0214-ORIGEM       = '1'.
*            WA_ZSDT0214-MENSAGEM     = MENSAGEM.
*            WA_ZSDT0214-USNAM        = SY-UNAME.
*            MODIFY ZSDT0214 FROM WA_ZSDT0214.
*            CLEAR WA_ZSDT0214.
*            COMMIT WORK.
*            EXIT.
          endif.

          wa_quanidade-codigomapa = wt201-codmapa.
          wa_quanidade-quantidade = wa_j_1bnflin-menge.
          collect wa_quanidade into it_quantidade.
        endif.


        select single * from zsdt0201 into @data(wa_zsdt0201)
          where id_produto eq @wa_zsdt0210-id_matnr_idea.

        select single * from zsdt0204 into @data(wa_zsdt0204)
          where nome eq @wa_zsdt0201-unidade.

        if sy-subrc = 0.

          if wa_j_1bnflin-meins = wa_zsdt0204-sigla.
            clear calcular.
            calcular = wa_j_1bnflin-menge / wa_zsdt0201-volume.

            clear  v_frac.
            v_frac = frac( calcular ).

            if v_frac <> '0'.
              verro  = abap_true.

*              MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
              concatenate 'Quantidade vendida do Material' wa_j_1bnflin-matnr ', Item' wa_j_1bnflin-itmnum 'da NFe, não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            else.
              wa_itens_saida-qnt_embalagem = calcular.
            endif.
          else.
            "PQ
*            IF wa_j_1bnflin-meins = 'KG' OR wa_j_1bnflin-meins = 'G' OR wa_zsdt0204-sigla = 'KG' OR wa_zsdt0204-sigla = 'G'.
            if wa_j_1bnflin-meins = 'KG' or wa_j_1bnflin-meins = 'G' and  wa_zsdt0204-sigla = 'KG' or wa_zsdt0204-sigla = 'G'.

              lv_unit_out = wa_zsdt0204-sigla.

              call function 'UNIT_CONVERSION_SIMPLE'
                exporting
                  input    = wa_j_1bnflin-menge
                  unit_in  = wa_j_1bnflin-meins
                  unit_out = lv_unit_out
                importing
                  output   = wa_j_1bnflin-menge.

*                   CLEAR calcular.
              calcular = wa_j_1bnflin-menge / wa_zsdt0201-volume.
              clear  v_frac.
              v_frac = frac( calcular ).

              if v_frac <> '0'.
                verro  = abap_true.
*              MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
                concatenate 'Quantidade vendida do Material' wa_j_1bnflin-matnr ', Item' wa_j_1bnflin-itmnum 'da NFe, não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.
                perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
                wa_zsdt0214-data_atual   = sy-datum.
                wa_zsdt0214-hora_atual   = sy-uzeit.
                wa_zsdt0214-docnum       = wa_defensivos-docnum.
                wa_zsdt0214-origem       = '1'.
                wa_zsdt0214-mensagem     = mensagem.
                wa_zsdt0214-usnam        = sy-uname.
                modify zsdt0214 from wa_zsdt0214.
                clear wa_zsdt0214.
                commit work.
                exit.
              else.
                wa_itens_saida-qnt_embalagem = calcular.
              endif.
              "PQ
            else.
              verro  = abap_true.
*                  MENSAGEM = 'Unidade de Material diferente da unidade Material INDEA'.
              concatenate 'A U.M. ' wa_j_1bnflin-meins 'do Material' wa_j_1bnflin-matnr 'é diferente da U.M.' wa_zsdt0204-sigla 'do Material INDEA.' into mensagem separated by space.

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            endif.
          endif.
        else.

          clear: mensagem.
          verro  = abap_true.
*          MENSAGEM  = ' Unidade de Medida do produto autorizado INDEA não encontrada na tabela de Unidade de Medidas do Indea !'.
          concatenate 'A Unidade de Medida' wa_zsdt0201-unidade 'do produto autorizado INDEA, não foi encontrada na tabela de Unidade de Medidas do próprio INDEA' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          clear wa_zsdt0214.
          commit work.
          exit.
        endif.

        append value #(   cod_produto         = wa_itens_saida-cod_produto
                          lote                = wa_itens_saida-lote
                          cpf_agronomo        = wa_itens_saida-cpf_agronomo
                          nr_art              = wa_itens_saida-nr_art
                          nr_receita          = wa_itens_saida-nr_receita
                          cod_cultura         = wa_itens_saida-cod_cultura
                          cod_praga           = wa_itens_saida-cod_praga
                          cod_tipo_aplicacao  = wa_itens_saida-cod_tipo_aplicacao
                          cod_unidade_medida  = wa_itens_saida-cod_unidade_medida
                          area_qnt_tratada    = wa_itens_saida-area_qnt_tratada
                          qnt_embalagem       = wa_itens_saida-qnt_embalagem
                  ) to wa_saida_agrotoxico-itens.

        clear: wa_j_1bnflin, wa_quanidade.
      endloop.

      check  verro  = abap_false.

      if wa_saida_agrotoxico-itens is initial.

        verro  = abap_true.
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = 'Nota com todos itens Adjuvante'.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_saida_agrotoxico-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  'Nota com todos itens Adjuvante'.
        wa_zsdt0212-status       =  'X'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        clear wa_zsdt0212.
        commit work.
        exit.
      endif.

      if wa_saida_agrotoxico-destino_revenda   = 'false'.
        loop at it_zsdt0219 into wa_zsdt0219.
          read table it_quantidade into wa_quanidade with key codigomapa = wa_zsdt0219-codigomapa.
          if sy-subrc = 0.

            if wa_quanidade-quantidade <> wa_zsdt0219-quantidade.
              verro  = abap_true.
              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              concatenate 'Quantidade da Receita diferente dos itens da Nota fical. Código Mapa: ' wa_zsdt0219-codigomapa into wa_zsdt0214-mensagem separated by space.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            endif.
          endif.
        endloop.
      endif.

      check  verro  = abap_false.

      if wzsdt0212-id is not initial.
        wa_saida_agrotoxico-cod_lancamento_usuario = wzsdt0212-id.
      else.
        perform get_next_number in program zsdr021
                                using  'ZSEQ_INDEA' '01'  changing wa_saida_agrotoxico-cod_lancamento_usuario.
      endif.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_saida_agrotoxico compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo
      changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_saida_agrotoxico-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.

        if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
          wa_zsdt0212-status   =  'A'.
          wa_zsdt0212-id_indea =  '9999999999'.
        else.
          wa_zsdt0212-status     =  'E'.
        endif.
*       wa_zsdt0212-status       =  'E'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        clear wa_zsdt0212.
        commit work.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_saida_agrotoxico-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        clear wa_zsdt0212.
        commit work.
      endif.
    endif.
  endif.

  clear: wa_zsdt0216.

endform.


form z_ws_cancel_saida_df .

  clear: mensagem, tp_metodo, resultado, purl.
  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro  = abap_false.

    wa_cancel_saida_agrotoxico-cnpj_revenda =   wa_defensivos-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_AGROTOXICO'.

    read table  it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                                bukrs   = wa_defensivos-bukrs
                                                                branch  = wa_defensivos-branch.
    if sy-subrc = 0.
      wa_cancel_saida_agrotoxico-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro  = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.


    wa_cancel_saida_agrotoxico-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_saida_agrotoxico-destino_armazenador    = 'false'.
    wa_cancel_saida_agrotoxico-motivo_cancelamento    = 'Dados da Nota estão incorretos'.

    check  verro  = abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_saida_agrotoxico compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo
                           changing resultado.
      translate resultado to upper case.


      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_saida_agrotoxico-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_saida_agrotoxico-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_defensivos-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
  endif.

endform.


form z_ws_devolucao_df.

  data wa_item_devolucao type ty_itens_devolucao_agrotoxico.
  data lv_unit_out    type t006-msehi.

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.
  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro  = abap_false.

    wa_devolucao_agrotoxico-cnpj_revenda = wa_defensivos-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_DEVOLUCAO_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service =  wa_zauth_webservice-service
                                                               bukrs   =  wa_defensivos-bukrs
                                                               branch  =  wa_defensivos-branch.
    if sy-subrc = 0.
      wa_devolucao_agrotoxico-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro  = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.
      exit.
    endif.

    read table it_j_1bnflin into data(wa_flin) with key docnum = wa_defensivos-docnum.

    select single * from vbfa into @data(wa_vbfa)
      where vbeln	  eq @wa_flin-refkey
      and   posnn	  eq @wa_flin-refitm
      and   vbtyp_n	eq 'O'
      and   vbtyp_v	eq 'M'.


    select single * from j_1bnflin into @data(wa_j1bnf)
      where refkey  eq @wa_vbfa-vbelv
        and refitm  eq @wa_vbfa-posnv.


    select single * from zsdt0212 into wa_zsdt0212
      where docnum eq wa_j1bnf-docnum.

    if sy-subrc <> 0.
      clear mensagem.
      verro  = abap_true.
      concatenate 'A nota de saída ( DocNum:' wa_j1bnf-docnum ') ainda não foi enviada ao INDEA' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.
      exit.
    else.
      if  wa_zsdt0212-status <> 'A'.
        clear mensagem.
        verro  = abap_true.
        concatenate 'Verifique o Status de envio ao Indea da nota de saída ( DocNum:' wa_j1bnf-docnum ')' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      else.
        wa_devolucao_agrotoxico-cod_saida_lancamento_usuario = wa_zsdt0212-id.
      endif.
    endif.

    concatenate wa_defensivos-docdat+0(4) '-'
                wa_defensivos-docdat+4(2) '-'
                wa_defensivos-docdat+6(2)  into wa_devolucao_agrotoxico-data_devolucao.

    wa_devolucao_agrotoxico-observacao = 'Cliente devolveu o Produto. '.

    loop at it_j_1bnflin into wa_j_1bnflin
      where docnum = wa_defensivos-docnum.

      read table it_zsdt0210 into wa_zsdt0210 with key matnr = wa_j_1bnflin-matnr.

      if sy-subrc = 0.
        wa_item_devolucao-cod_produto = wa_zsdt0210-id_matnr_idea.
        wa_item_devolucao-lote        = wa_j_1bnflin-charg.

      else.
        clear mensagem.
        verro  = abap_true.
        wa_j_1bnflin-matnr = |{ wa_j_1bnflin-matnr alpha = out }|.

        concatenate 'Material' wa_j_1bnflin-matnr 'não realizado o DE/PARA na transação ZSDT0154 ' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.

      select single *
        from zsdt0201 into @data(wa_zsdt0201)
       where id_produto eq @wa_zsdt0210-id_matnr_idea.

      select single *
        from zsdt0204 into @data(wa_zsdt0204)
       where nome eq @wa_zsdt0201-unidade.

      if sy-subrc = 0.

        if wa_j_1bnflin-meins =  wa_zsdt0204-sigla.
          clear:  calcular, v_frac.
          calcular = wa_j_1bnflin-menge / wa_zsdt0201-volume.
          v_frac = frac( calcular ).

          if v_frac <> '0'.
            verro  = abap_true.
*            MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
            concatenate 'Quantidade vendida do Material' wa_j_1bnflin-matnr ', Item' wa_j_1bnflin-itmnum 'da NFe, não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          else.
            wa_item_devolucao-qnt_produto =  calcular.
          endif.
        else.
          "PQ
*          IF wa_j_1bnflin-meins = 'KG' OR wa_j_1bnflin-meins = 'G' OR wa_zsdt0204-sigla = 'KG' OR wa_zsdt0204-sigla = 'G'.
          if wa_j_1bnflin-meins = 'KG' or wa_j_1bnflin-meins = 'G' and wa_zsdt0204-sigla = 'KG' or wa_zsdt0204-sigla = 'G'.

            lv_unit_out = wa_zsdt0204-sigla.

            call function 'UNIT_CONVERSION_SIMPLE'
              exporting
                input    = wa_j_1bnflin-menge
                unit_in  = wa_j_1bnflin-meins
                unit_out = lv_unit_out
              importing
                output   = wa_j_1bnflin-menge.

*                   CLEAR calcular.
            calcular = wa_j_1bnflin-menge / wa_zsdt0201-volume.
            clear  v_frac.
            v_frac = frac( calcular ).

            if v_frac <> '0'.
              verro  = abap_true.
*              MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
              concatenate 'Quantidade vendida do Material' wa_j_1bnflin-matnr ', Item' wa_j_1bnflin-itmnum 'da NFe, não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.
              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            else.
              wa_item_devolucao-qnt_produto =  calcular.
            endif.
            "PQ
          else.
            verro  = abap_true.
*          MENSAGEM = 'Unidade de Material diferente da unidade Material INDEA'.
            concatenate 'A U.M. ' wa_j_1bnflin-meins 'do Material' wa_j_1bnflin-matnr 'é diferente da U.M.' wa_zsdt0204-sigla 'do Material INDEA.' into mensagem separated by space.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          endif.
        endif.
      else.
        clear: mensagem.
        verro  = abap_true.
*        MENSAGEM  = ' Unidade de Medida do produto autorizado INDEA não encontrada na tabela de Unidade de Medidas do Indea !'.
        concatenate 'A Unidade de Medida' wa_zsdt0201-unidade 'do produto autorizado INDEA, não foi encontrada na tabela de Unidade de Medidas do próprio INDEA' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.

      append value #( cod_produto  = wa_item_devolucao-cod_produto
                      lote         = wa_item_devolucao-lote
                      qnt_produto  = wa_item_devolucao-qnt_produto
                     ) to wa_devolucao_agrotoxico-itens.
    endloop.

    check verro  = abap_false.

    if  wa_devolucao_agrotoxico-itens is initial.

      verro  = abap_true.
      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = 'Nota com todos itens Adjuvante'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_devolucao_agrotoxico-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  'Nota com todos itens Adjuvante'.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
      exit.
    endif.

    if wzsdt0212-id is not initial.
      wa_devolucao_agrotoxico-cod_lancamento_usuario =  wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021 using  'ZSEQ_INDEA' '01'
                              changing wa_devolucao_agrotoxico-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.
    e_xml = /ui2/cl_json=>serialize( data = wa_devolucao_agrotoxico compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                                wa_zauth_ws_0001-username
                                wa_zauth_ws_0001-password
                                tp_metodo   changing resultado.

    translate resultado to upper case.

    if  resultado ca sy-abcde .

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_devolucao_agrotoxico-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.
*     wa_zsdt0212-status       =  'E'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.

    elseif  resultado ca '0123456789'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_devolucao_agrotoxico-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
    endif.
  endif.

endform.


form z_ws_cancel_devol_df.

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.
  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.
  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro  = abap_false.
    wa_cancel_devolucao_agrotoxico-cnpj_revenda =  wa_defensivos-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_DEVOLUCAO_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                               bukrs   = wa_defensivos-bukrs
                                                               branch  = wa_defensivos-branch.
    if sy-subrc = 0.
      wa_cancel_devolucao_agrotoxico-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro  = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       =  wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_cancel_devolucao_agrotoxico-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_devolucao_agrotoxico-motivo_cancelamento    = ' '.

    check  verro  = abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_devolucao_agrotoxico compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo
                           changing resultado.
      translate resultado to upper case.

      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_devolucao_agrotoxico-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_devolucao_agrotoxico-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_defensivos-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
  endif.

endform.

form z_ws_saida_arm_df.

  data wa_itens_saida_arm type ty_itens_saida_agrotoxico.
  data lv_unit_out    type t006-msehi.

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro  = abap_false.
    read table it_j_1bnflin into wa_j_1bnflin with key docnum = wa_defensivos-docnum.

    if wa_defensivos-nftype = 'ZA' and wa_j_1bnflin-itmtyp = 'ZC'.

      read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_AGROTOXICO'.

      read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                                 bukrs   =  wa_defensivos-bukrs
                                                                 branch  =  wa_defensivos-branch.
      if sy-subrc = 0.
        wa_saida_arm_agrotoxico-hash_revenda = wa_zauth_ws_0001-add01.
      else.
        clear mensagem.
        verro  = abap_true.
        concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.

      wa_saida_arm_agrotoxico-cnpj_revenda =  wa_defensivos-cnpj_bupla.

      if  wa_defensivos-regio = 'MT'.
        wa_saida_arm_agrotoxico-destino_fora_m_t  = 'false'.
      else.
        wa_saida_arm_agrotoxico-destino_fora_m_t  = 'true'.
      endif.

      wa_saida_arm_agrotoxico-destino_revenda     = 'false'.
      wa_saida_arm_agrotoxico-destino_armazenador = 'true'.

      if wa_defensivos-nfenum is not initial.
        wa_saida_arm_agrotoxico-nr_nf = wa_defensivos-nfenum.
      else.
        wa_saida_arm_agrotoxico-nr_nf = wa_defensivos-nfnum.
      endif.

      wa_saida_arm_agrotoxico-serie_nf = wa_defensivos-series.

      concatenate wa_defensivos-docdat+0(4) '-' wa_defensivos-docdat+4(2) '-' wa_defensivos-docdat+6(2) into wa_saida_arm_agrotoxico-data_nf.

      wa_saida_arm_agrotoxico-cnpj_ure = ''.
      wa_saida_arm_agrotoxico-cod_propriedade = ''.

      read table it_lfa1 into wa_lfa1 with key lifnr = wa_defensivos-parid.
      if sy-subrc = 0.

        wa_lfa1-name1 = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = conv #( wa_lfa1-name1 ) ) ).

        wa_saida_arm_agrotoxico-cpf_cnpj_destino = wa_lfa1-stcd1.

        if wa_defensivos-regio <> 'MT'.
          wa_saida_arm_agrotoxico-nome_destino  = wa_lfa1-name1.
          wa_saida_arm_agrotoxico-cod_municipio = wa_lfa1-txjcd+3(12).
        else.
          wa_saida_arm_agrotoxico-nome_destino  = ' '.
          wa_saida_arm_agrotoxico-cod_municipio = ' '.
        endif.

      endif.

      loop at it_j_1bnflin into wa_j_1bnflin
        where docnum =  wa_defensivos-docnum.

        select single * from zsdt0210 into @data(wa_zsdt0210)
          where matnr eq @wa_j_1bnflin-matnr.

**** IR024542
        if sy-subrc = 0.
          wa_itens_saida_arm-cod_produto = wa_zsdt0210-id_matnr_idea.

          select single * from zsdt0201 into @data(wa_zsdt0201)
           where id_produto eq @wa_zsdt0210-id_matnr_idea.

        else.
          verro  = abap_true.
          wa_j_1bnflin-matnr = |{ wa_j_1bnflin-matnr alpha = out }|.

          concatenate 'Material' wa_j_1bnflin-matnr 'não realizado o DE/PARA na transação ZSDT0154 ' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          clear wa_zsdt0214.
          commit work.
          exit.
        endif.
*        WA_ITENS_SAIDA_ARM-COD_PRODUTO = WA_ZSDT0210-ID_MATNR_IDEA.
**** IR024542

        wa_itens_saida_arm-lote          = wa_j_1bnflin-charg.

        select single * from zsdt0204 into @data(wa_zsdt0204)
          where nome eq @wa_zsdt0201-unidade.

        if sy-subrc = 0.

          if wa_j_1bnflin-meins = wa_zsdt0204-sigla.
            clear calcular.
            calcular = wa_j_1bnflin-menge / wa_zsdt0201-volume.

            clear  v_frac.
            v_frac = frac( calcular ).

            if v_frac <> '0'.
              verro  = abap_true.
*              MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
              concatenate 'Quantidade vendida do Material' wa_j_1bnflin-matnr ', Item' wa_j_1bnflin-itmnum 'da NFe, não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            else.
              wa_itens_saida_arm-qnt_embalagem = calcular.
            endif.
          else.
            "PQ
*            IF wa_j_1bnflin-meins = 'KG' OR wa_j_1bnflin-meins = 'G' OR wa_zsdt0204-sigla = 'KG' OR wa_zsdt0204-sigla = 'G'.
            if wa_j_1bnflin-meins = 'KG' or wa_j_1bnflin-meins = 'G' and wa_zsdt0204-sigla = 'KG' or wa_zsdt0204-sigla = 'G'.

              lv_unit_out = wa_zsdt0204-sigla.

              call function 'UNIT_CONVERSION_SIMPLE'
                exporting
                  input    = wa_j_1bnflin-menge
                  unit_in  = wa_j_1bnflin-meins
                  unit_out = lv_unit_out
                importing
                  output   = wa_j_1bnflin-menge.

*                   CLEAR calcular.
              calcular = wa_j_1bnflin-menge / wa_zsdt0201-volume.
              clear  v_frac.
              v_frac = frac( calcular ).

              if v_frac <> '0'.
                verro  = abap_true.
*              MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
                concatenate 'Quantidade vendida do Material' wa_j_1bnflin-matnr ', Item' wa_j_1bnflin-itmnum 'da NFe, não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.
                perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
                wa_zsdt0214-data_atual   = sy-datum.
                wa_zsdt0214-hora_atual   = sy-uzeit.
                wa_zsdt0214-docnum       = wa_defensivos-docnum.
                wa_zsdt0214-origem       = '1'.
                wa_zsdt0214-mensagem     = mensagem.
                wa_zsdt0214-usnam        = sy-uname.
                modify zsdt0214 from wa_zsdt0214.
                clear wa_zsdt0214.
                commit work.
                exit.
              else.
                wa_itens_saida_arm-qnt_embalagem = calcular.
              endif.
              "PQ
            else.
              verro  = abap_true.
*            MENSAGEM = 'Unidade de Material diferente da unidade Material INDEA'.
              concatenate 'A U.M. ' wa_j_1bnflin-meins 'do Material' wa_j_1bnflin-matnr 'é diferente da U.M.' wa_zsdt0204-sigla 'do Material INDEA.' into mensagem separated by space.

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            endif.
          endif.
        else.
          clear: mensagem.
          verro  = abap_true.
*          MENSAGEM  = ' Unidade de Medida do produto autorizado INDEA não encontrada na tabela de Unidade de Medidas do Indea !'.
          concatenate 'A Unidade de Medida' wa_zsdt0201-unidade 'do produto autorizado INDEA, não foi encontrada na tabela de Unidade de Medidas do próprio INDEA' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          clear wa_zsdt0214.
          commit work.
          exit.
        endif.


        append value #(   cod_produto         = wa_itens_saida_arm-cod_produto
                          lote                = wa_itens_saida_arm-lote
                          cpf_agronomo        = wa_itens_saida_arm-cpf_agronomo
                          nr_art              = wa_itens_saida_arm-nr_art
                          nr_receita          = wa_itens_saida_arm-nr_receita
                          cod_cultura         = wa_itens_saida_arm-cod_cultura
                          cod_praga           = wa_itens_saida_arm-cod_praga
                          cod_tipo_aplicacao  = wa_itens_saida_arm-cod_tipo_aplicacao
                          cod_unidade_medida  = wa_itens_saida_arm-cod_unidade_medida
                          area_qnt_tratada    = wa_itens_saida_arm-area_qnt_tratada
                          qnt_embalagem       = wa_itens_saida_arm-qnt_embalagem
                  ) to wa_saida_arm_agrotoxico-itens.

        clear wa_j_1bnflin.
      endloop.

      check verro = abap_false.

      if wa_saida_arm_agrotoxico-itens is initial.

        verro = abap_true.
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = 'Nota com todos itens Adjuvante'.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_saida_arm_agrotoxico-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  'Nota com todos itens Adjuvante'.
        wa_zsdt0212-status       =  'X'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
        exit.
      endif.


      if wzsdt0212-id is not initial.
        wa_saida_arm_agrotoxico-cod_lancamento_usuario =  wzsdt0212-id.
      else.
        perform get_next_number in program zsdr021
                                using  'ZSEQ_INDEA' '01'
                                changing wa_saida_arm_agrotoxico-cod_lancamento_usuario.
      endif.

      purl = wa_zauth_webservice-url.

      e_xml = /ui2/cl_json=>serialize( data = wa_saida_arm_agrotoxico compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo
      changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_saida_arm_agrotoxico-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.

        if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
          wa_zsdt0212-status   =  'A'.
          wa_zsdt0212-id_indea =  '9999999999'.
        else.
          wa_zsdt0212-status     =  'E'.
        endif.

*       wa_zsdt0212-status       =  'E'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.


        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_saida_arm_agrotoxico-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.
    endif.
  endif.

endform.

form z_ws_cancel_arm_df.

  clear: mensagem, tp_metodo, resultado, purl.
  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro = abap_false.
    read table it_j_1bnflin into wa_j_1bnflin with key docnum = wa_defensivos-docnum.

    if wa_defensivos-nftype = 'ZA' and wa_j_1bnflin-itmtyp = 'ZC'.

      wa_cancel_saida_arm_agrotoxico-cnpj_revenda =   wa_defensivos-cnpj_bupla.

      read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_AGROTOXICO'.

      read table  it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                                  bukrs   = wa_defensivos-bukrs
                                                                  branch  = wa_defensivos-branch.
      if sy-subrc = 0.
        wa_cancel_saida_arm_agrotoxico-hash_revenda = wa_zauth_ws_0001-add01.
      else.
        clear mensagem.
        verro = abap_true.
        concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify  zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.
        exit.
      endif.

      wa_cancel_saida_arm_agrotoxico-cod_lancamento_usuario = wzsdt0212-id.
      wa_cancel_saida_arm_agrotoxico-destino_armazenador    = 'true'.
      wa_cancel_saida_arm_agrotoxico-motivo_cancelamento    = 'Dados da Nota estão incorretos'.

      check verro = abap_false.

      if wzsdt0212-status = 'A'.

        purl = wa_zauth_webservice-url.

        e_xml = /ui2/cl_json=>serialize( data = wa_cancel_saida_arm_agrotoxico compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

        perform z_envia_json using purl
                                   wa_zauth_ws_0001-username
                                   wa_zauth_ws_0001-password
                                   tp_metodo
                             changing resultado.
        translate resultado to upper case.

        if  resultado ca sy-abcde .
          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '2'.
          wa_zsdt0214-mensagem     = resultado.
          wa_zsdt0214-usnam        = sy-uname.
          modify  zsdt0214 from wa_zsdt0214.
          commit work.
          clear: wa_zsdt0214.

          wa_zsdt0212-docnum       =  wa_defensivos-docnum.
          wa_zsdt0212-id           =  wa_cancel_saida_arm_agrotoxico-cod_lancamento_usuario.
**          wa_zsdt0212-id_indea     =  resultado.
          wa_zsdt0212-usnam        =  sy-uname.
          wa_zsdt0212-data_atual   =  sy-datum.
          wa_zsdt0212-hora_atual   =  sy-uzeit.

          if resultado eq retorno_cancel.
            wa_zsdt0212-status       =  'C'.
          else.
            wa_zsdt0212-status       =  'E'.
          endif.
          modify  zsdt0212 from wa_zsdt0212.
          commit work.
          clear wa_zsdt0212.

        elseif  resultado ca '0123456789'.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '2'.
          wa_zsdt0214-mensagem     = resultado.
          wa_zsdt0214-usnam        = sy-uname.
          modify  zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.

          wa_zsdt0212-docnum       =  wa_defensivos-docnum.
          wa_zsdt0212-id           =  wa_cancel_saida_arm_agrotoxico-cod_lancamento_usuario.
          wa_zsdt0212-id_indea     =  resultado.
          wa_zsdt0212-status       =  'A'.
          wa_zsdt0212-usnam        =  sy-uname.
          wa_zsdt0212-data_atual   =  sy-datum.
          wa_zsdt0212-hora_atual   =  sy-uzeit.
          modify  zsdt0212 from wa_zsdt0212.
          commit work.
          clear wa_zsdt0212.
        endif.

      elseif wzsdt0212-status = 'E'.

        wa_zsdt0212-docnum       =  wzsdt0212-docnum.
        wa_zsdt0212-id           =  wzsdt0212-id.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'X'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify  zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_defensivos-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
  endif.
endform.

form z_ws_entrada_arm_df.

  data wa_itens_arm   type ty_item_entrada_agrotoxico.
  data lv_unit_out    type t006-msehi.

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212  with key docnum = wa_defensivos-docnum.

  if (  wzsdt0212-status <> 'X' and  wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro = abap_false.
    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                               bukrs   = wa_defensivos-bukrs
                                                               branch  = wa_defensivos-branch.
    if sy-subrc = 0.
      wa_entrada_arm_agrotoxico-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_entrada_arm_agrotoxico-cnpj_revenda = wa_defensivos-cnpj_bupla.
    wa_entrada_arm_agrotoxico-origem_armazenador = 'true'.

    if wa_defensivos-nfenum is not initial.
      wa_entrada_arm_agrotoxico-nr_nf = wa_defensivos-nfenum.
    else.
      wa_entrada_arm_agrotoxico-nr_nf = wa_defensivos-nfnum.
    endif.

    if wa_defensivos-series is not initial.
      wa_entrada_arm_agrotoxico-serie_nf = wa_defensivos-series.
    else.
      clear mensagem.
      verro = abap_true.
      mensagem = 'A Série da nota Fiscal não está preenchido!'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    concatenate wa_defensivos-docdat+0(4) '-'  wa_defensivos-docdat+4(2) '-' wa_defensivos-docdat+6(2) into wa_entrada_arm_agrotoxico-dt_nf.
    concatenate wa_defensivos-pstdat+0(4) '-'  wa_defensivos-pstdat+4(2) '-' wa_defensivos-pstdat+6(2) into wa_entrada_arm_agrotoxico-dt_entrada.

    read table it_lfa1 into wa_lfa1 with key lifnr = wa_defensivos-parid.
    if sy-subrc = 0.
      if wa_lfa1-stcd1 is not initial.
        wa_entrada_arm_agrotoxico-cnpj_fornecedor = wa_lfa1-stcd1.
      else.
        wa_entrada_arm_agrotoxico-cnpj_fornecedor = wa_lfa1-stcd2.
      endif.
    endif.


    loop at  it_mseg into wa_mseg
    where docnum = wa_defensivos-docnum.

      select single *
         from zsdt0210 into wa_zsdt0210
        where matnr eq wa_mseg-matnr.

      if sy-subrc = 0.
        wa_itens_arm-cod_produto  =  wa_zsdt0210-id_matnr_idea.
      else.
        wa_mseg-matnr = |{ wa_mseg-matnr alpha = out }|.
        verro = abap_true.

        concatenate 'Material' wa_mseg-matnr 'não realizado o DE/PARA na transação ZSDT0154 ' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.
        exit.
      endif.

      wa_itens_arm-lote   = wa_mseg-charg.

      select single *
        from zsdt0201 into @data(wa_zsdt0201)
        where id_produto eq @wa_zsdt0210-id_matnr_idea.

      if sy-subrc = 0.

        select single *
          from zsdt0204 into @data(wa_zsdt0204)
         where nome eq @wa_zsdt0201-unidade.

        if sy-subrc = 0.
          if wa_mseg-meins =  wa_zsdt0204-sigla.
            clear calcular.
            calcular =  wa_mseg-menge / wa_zsdt0201-volume.

            clear  v_frac.
            v_frac = frac( calcular ).

            if v_frac <> '0'.
              verro = abap_true.
*              MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
              concatenate 'Quantidade vendida do Material' wa_mseg-matnr 'não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              commit work.
              clear wa_zsdt0214.
              exit.
            else.
              wa_itens_arm-qnt_produto = calcular.
            endif.
          else.
            "PQ
*            IF wa_mseg-meins = 'KG' OR wa_mseg-meins = 'G' OR wa_zsdt0204-sigla = 'KG' OR wa_zsdt0204-sigla = 'G'.
            if wa_mseg-meins = 'KG' or wa_mseg-meins = 'G' and wa_zsdt0204-sigla = 'KG' or wa_zsdt0204-sigla = 'G'.

              lv_unit_out = wa_zsdt0204-sigla.

              call function 'UNIT_CONVERSION_SIMPLE'
                exporting
                  input    = wa_mseg-menge
                  unit_in  = wa_mseg-meins
                  unit_out = lv_unit_out
                importing
                  output   = wa_mseg-menge.

*                   CLEAR calcular.
              calcular =  wa_mseg-menge / wa_zsdt0201-volume.

              clear  v_frac.
              v_frac = frac( calcular ).

              if v_frac <> '0'.
                verro = abap_true.
*              MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
                concatenate 'Quantidade vendida do Material' wa_mseg-matnr 'não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.

                perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
                wa_zsdt0214-data_atual   = sy-datum.
                wa_zsdt0214-hora_atual   = sy-uzeit.
                wa_zsdt0214-docnum       = wa_defensivos-docnum.
                wa_zsdt0214-origem       = '1'.
                wa_zsdt0214-mensagem     = mensagem.
                wa_zsdt0214-usnam        = sy-uname.
                modify zsdt0214 from wa_zsdt0214.
                commit work.
                clear wa_zsdt0214.
                exit.
              else.
                wa_itens_arm-qnt_produto = calcular.
              endif.
              "PQ
            else.
              verro = abap_true.
*            MENSAGEM = 'Unidade de Material diferente da unidade Material INDEA'.
              concatenate 'A U.M. ' wa_mseg-meins 'do Material' wa_mseg-matnr 'é diferente da U.M.' wa_zsdt0204-sigla 'do Material INDEA.' into mensagem separated by space.

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              commit work.
              clear wa_zsdt0214.
              exit.
            endif.
          endif.
        else.
          clear: mensagem.
          verro  = abap_true.
*          MENSAGEM  = ' Unidade de Medida do produto autorizado INDEA não encontrada na tabela de Unidade de Medidas do Indea !'.
          concatenate 'A Unidade de Medida' wa_zsdt0201-unidade 'do produto autorizado INDEA, não foi encontrada na tabela de Unidade de Medidas do próprio INDEA' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          clear wa_zsdt0214.
          commit work.
          exit.
        endif.
      endif.


      append value #( cod_produto  = wa_itens_arm-cod_produto
                      lote         = wa_itens_arm-lote
                      qnt_produto  = wa_itens_arm-qnt_produto
                     ) to wa_entrada_arm_agrotoxico-itens.
    endloop.


    check verro = abap_false.

    if wa_entrada_arm_agrotoxico-itens is initial.

      verro = abap_true.
      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = 'Nota com todos itens Adjuvante'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.


      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_entrada_arm_agrotoxico-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  'Nota com todos itens Adjuvante'.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
      exit.
    endif.

    if wzsdt0212-id is not initial.
      wa_entrada_arm_agrotoxico-cod_lancamento_usuario  =  wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021 using  'ZSEQ_INDEA'  '01'
                                                 changing wa_entrada_arm_agrotoxico-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.

    e_xml = /ui2/cl_json=>serialize( data = wa_entrada_arm_agrotoxico compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                               wa_zauth_ws_0001-username
                               wa_zauth_ws_0001-password
                               tp_metodo   changing resultado.
    translate resultado to upper case.

    if  resultado ca sy-abcde .
      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.


      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_entrada_arm_agrotoxico-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.
*     wa_zsdt0212-status       =  'E'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.

    elseif  resultado ca '0123456789'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.


      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_entrada_arm_agrotoxico-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  endif.

endform.

form z_ws_cancel_entrada_arm_df.

  clear: mensagem,  tp_metodo, resultado, purl.

  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro = abap_false.

    wa_cancel_entrada_arm_agrot-cnpj_revenda = wa_sementes-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key  service = wa_zauth_webservice-service
                                                                bukrs   = wa_defensivos-bukrs
                                                                branch  = wa_defensivos-branch.
    if sy-subrc = 0.
      wa_cancel_entrada_arm_agrot-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_cancel_entrada_arm_agrot-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_entrada_arm_agrot-destino_armazenador    = 'true'.
    wa_cancel_entrada_arm_agrot-motivo_cancelamento    = 'Dados da Nota estão incorretos'.

    check verro = abap_false.

    if wzsdt0212-status = 'A'.

      purl =  wa_zauth_webservice-url.

      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_entrada_arm_agrot compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).


      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo  changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_arm_sementes-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_arm_sementes-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_defensivos-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
  endif.
endform.

***************************************************************
* busca sementes
***************************************************************
form z_busca_sementes.
  data vxblnr type ekbe-xblnr.
  data vxblnr_ref type ekbe-xblnr.

  data vnfenum type j_1bnfdoc-nfenum.
  data vnfenum_ref type j_1bnfdoc-nfenum.

  select a~docnum, a~docsta, a~cancel,     b~pstdat, b~nfenum,
         b~nfnum,  b~series, b~model,      b~bukrs,  b~branch,
         b~nftype, b~doctyp, b~cnpj_bupla, b~cgc,    b~parid,
         b~docdat, b~regio,  b~parvw,      c~cfop
    from j_1bnfdoc  as  b
    inner join j_1bnfe_active as a on a~docnum = b~docnum
    inner join j_1bnflin      as c on c~docnum = b~docnum
    into corresponding fields of table @it_sementes
    for all entries in @it_t001w
   where  b~docnum in @ra_docnum
     and  b~bukrs  in @r_burks
     and  b~branch eq @it_t001w-werks
     and  b~pstdat in @r_dt_def
     and  b~nftype in @ra_nftype
     and  b~model  in ('55', '01', '04')
     and  a~docsta = '1'
     and  c~matkl  in ('700130', '700230', '700240', '700350').

*  SELECT a~docnum, a~docsta, a~cancel,     b~pstdat, b~nfenum,
*         b~nfnum,  b~series, b~model,      b~bukrs,  b~branch,
*         b~nftype, b~doctyp, b~cnpj_bupla, b~cgc,    b~parid,
*         b~docdat, b~regio
*    FROM j_1bnfe_active AS a
*    INNER JOIN j_1bnfdoc AS b ON b~docnum = a~docnum
*    INTO CORRESPONDING FIELDS OF TABLE @it_sementes
*     FOR ALL ENTRIES IN @it_t001w
*   WHERE  a~docnum IN @ra_docnum
*     AND  b~bukrs  IN @r_burks
*     AND  b~branch EQ @it_t001w-werks
*     AND  a~docsta = '1'
*     AND  b~docdat IN @r_dt_sem
*     AND  b~nftype IN @ra_nftype
*     AND  b~model  IN ('55', '01', '04')
*    AND EXISTS (  SELECT docnum FROM j_1bnflin AS c  WHERE c~docnum = b~docnum
*                                                     AND   c~matkl IN ('700130', '700230', '700240', '700350') ).



  select b~docnum, b~docstat, b~cancel,     b~pstdat, b~nfenum,
         b~nfnum,  b~series,  b~model,      b~bukrs,  b~branch,
         b~nftype, b~doctyp,  b~cnpj_bupla, b~cgc,    b~parid,
         b~docdat, b~regio,   b~parvw
  from j_1bnfdoc as b
    into table @it_doc
    for all entries in @it_t001w
  where b~docnum  in @ra_docnum
   and  b~bukrs   in @r_burks
   and  b~branch  eq @it_t001w-werks
   and  b~pstdat  in @r_dt_sem
   and  b~nftype  in ('E1', 'N4')
*   AND  B~DOCSTAT EQ '1'
   and  b~model   in ('55', '01', '04')
   and exists ( select docnum from j_1bnflin as  c where c~docnum = b~docnum
                                                   and   c~matkl in ('700130', '700230', '700240', '700350') ).

  loop at it_doc into wa_doc.
    move wa_doc to wa_sementes.
*    IF WA_SEMENTES-PARVW EQ 'BR'.
*      WA_SEMENTES-PARID = WA_DOC-PARID+4.
*      WA_SEMENTES-PARID = |{ WA_SEMENTES-PARID ALPHA = IN }|.
*    ENDIF.
    append wa_sementes to it_sementes.
    clear:  wa_doc, wa_sementes.
  endloop.

*-CS2022000910-06.06.2023-#91627-JT-inicio
  perform f_valida_status_sementes.
*-CS2022000910-06.06.2023-#91627-JT-fim

  check it_sementes is not initial.

*  LOOP AT IT_SEMENTES ASSIGNING FIELD-SYMBOL(<FS_SEMENTES>).
*    IF <FS_SEMENTES>-PARVW EQ 'BR'.
*      <FS_SEMENTES>-PARID = <FS_SEMENTES>-PARID+4.
*      <FS_SEMENTES>-PARID = |{ <FS_SEMENTES>-PARID ALPHA = IN }|.
*    ENDIF.
*  ENDLOOP.

  select *
    from lfa1 into table it_lfa1
    for all entries in it_sementes
  where lifnr eq it_sementes-parid.

  select *
    from kna1 into table it_kna1
    for all entries in it_sementes
  where kunnr eq it_sementes-parid.

  select *
    from j_1bnflin into table it_j_1bnflin_02
   for all entries in it_sementes
  where docnum eq it_sementes-docnum.

  if it_j_1bnflin_02 is not initial.

    data(it_j_1bnflin_aux) = it_j_1bnflin_02.

    delete it_j_1bnflin_aux where docref is initial.

    if it_j_1bnflin_aux is not initial.

      select *
        from j_1bnfe_active into table it_j_1bnfe_active
       for all entries in it_j_1bnflin_aux
      where docnum eq it_j_1bnflin_aux-docref.

    endif.
  endif.

  select *
    from zsdt0210 into table it_zsdt0210
   for all entries in it_j_1bnflin_02
   where matnr eq   it_j_1bnflin_02-matnr.

  select *
     from zsdt0212 into table it_zsdt0212
    for all entries in it_sementes
   where docnum eq   it_sementes-docnum.

  loop at it_sementes into wa_sementes.

    if wa_sementes-nfnum is not initial.
      vnfenum = |{ wa_sementes-nfnum alpha = out }|.
    else.
*      VNFENUM = |{ WA_SEMENTES-NFENUM ALPHA = OUT }|.
      vnfenum =  wa_sementes-nfenum.
      shift vnfenum left deleting leading '0'.
    endif.

    vxblnr = |{ vnfenum }{ '-' }{ wa_sementes-series }|.

    read table it_j_1bnflin_02 into wa_j_1bnflin_02 with key docnum  = wa_sementes-docnum.
*    READ TABLE IT_J_1BNFE_ACTIVE INTO DATA(WA_J_1BNFE_ACTIVE) WITH KEY DOCNUM  = WA_J_1BNFLIN_02-DOCREF.
*    IF SY-SUBRC IS INITIAL.
*      IF WA_J_1BNFE_ACTIVE-NFNUM9 IS NOT INITIAL.
*        VNFENUM_REF = |{ WA_J_1BNFE_ACTIVE-NFNUM9 ALPHA = OUT }|.
*        SHIFT VNFENUM_REF LEFT DELETING LEADING '0'.
*      ENDIF.
*      VXBLNR_REF = |{ VNFENUM_REF }{ '-' }{ WA_J_1BNFE_ACTIVE-SERIE }|.
*    ENDIF.

    case wa_j_1bnflin_02-reftyp.
      when 'LI'.

        select   ebeln  ebelp  belnr
                 gjahr  lfbnr  lfpos
                 lfgja  xblnr
          from ekbe into table tekbe
         where belnr eq wa_j_1bnflin_02-refkey+0(10)
          and  gjahr eq wa_j_1bnflin_02-refkey+10(4).

        sort tekbe by ebeln ebelp.
        delete adjacent duplicates from tekbe comparing ebeln ebelp.

        if wa_sementes-nftype eq 'ZR'.

          loop at tekbe into wekbe.

            select distinct *
                from ekbe  as e into table it_ekbe
              where  e~ebeln eq wekbe-ebeln
               and   e~ebelp eq wekbe-ebelp
               and   e~vgabe eq '1'
               and   e~xblnr eq vxblnr
               and   e~shkzg eq  'H'.

            if sy-subrc is not initial.

              select distinct *
                 from ekbe as e
                  appending table it_ekbe
               where  e~ebeln eq wekbe-ebeln
                and   e~ebelp eq wekbe-ebelp
                and   e~bwart eq '122'
                and   e~shkzg eq  'H'
                and   e~lfbnr eq wekbe-lfbnr.

            endif.

            if sy-subrc is initial.
              select  *
                from mseg into table  tmseg
                for all entries in it_ekbe
              where mblnr eq it_ekbe-belnr
               and  mjahr eq it_ekbe-gjahr
               and  shkzg eq 'H'.
            endif.

          endloop.

          sort tmseg by mblnr mjahr zeile.
          delete adjacent duplicates from tmseg comparing mblnr mjahr zeile.

          loop at tmseg  into wmseg.
            wmseg-docnum = wa_sementes-docnum.
            append wmseg to it_mseg.

            "#####################
            " localiza a entrada, para pegar as informações do numero FASE E RENASEM
            select  ebeln  ebelp line_id mblnr
                    mjahr  charg nr_fase matnr
                    werks  lgort categoria  menge
                    tcode  renas
              from zmmt0102 into table tzmmt0102
              for all entries in tekbe
               where mblnr eq tekbe-lfbnr
                 and mjahr eq tekbe-lfgja
                 and matnr eq wmseg-matnr
                 and charg eq wmseg-charg.

            "######################
            clear wmseg.
          endloop.

          if tzmmt0102 is not initial.

            loop at tzmmt0102 into wa_zmmt0102.
              wa_zmmt0102-docnum = wa_sementes-docnum.
              wa_zmmt0102-reftyp = wa_j_1bnflin_02-reftyp.
              append wa_zmmt0102 to it_zmmt0102.
              clear wa_zmmt0102.
            endloop.
          endif.

        else.
          loop at tekbe into wekbe.

            if wekbe-lfbnr is initial.

              select distinct *
                from ekbe  as e into table it_ekbe
              where  e~ebeln eq wekbe-ebeln
               and   e~ebelp eq wekbe-ebelp
               and   e~vgabe eq '1'
               and   e~xblnr eq vxblnr
               and   e~shkzg eq  'S'
               and   e~belnr eq ( select max( k~belnr )
                                  from  ekbe as k
                                  where  k~ebeln = e~ebeln
                                   and   k~ebelp = e~ebelp
                                   and   k~vgabe = e~vgabe
                                   and   k~xblnr = e~xblnr
                                   and   k~shkzg = e~shkzg ) .

              if sy-subrc = 0.

                select  *
                  from mseg into table  tmseg
                  for all entries in it_ekbe
                where mblnr eq it_ekbe-belnr
                 and  mjahr eq it_ekbe-gjahr
                 and  shkzg eq 'S'.

              endif.

            else.

              select *
                from mseg into table tmseg
                where mblnr eq wekbe-lfbnr
                 and  mjahr eq wekbe-lfgja
                 and  shkzg eq 'S'.

            endif.

            sort tmseg by mblnr mjahr zeile.
            delete adjacent duplicates from tmseg comparing mblnr mjahr zeile.

            loop at tmseg  into wmseg.
              wmseg-docnum = wa_sementes-docnum.
              append wmseg to it_mseg.
              clear wmseg.
            endloop.

            if it_mseg is not initial.

              select  ebeln  ebelp line_id mblnr
                      mjahr  charg nr_fase matnr
                      werks  lgort categoria  menge
                      tcode  renas
                from zmmt0102 into table tzmmt0102
                for all entries in it_mseg
                 where mblnr eq it_mseg-mblnr
                   and mjahr eq it_mseg-mjahr
                   and matnr eq it_mseg-matnr
                   and charg eq it_mseg-charg.

              loop at tzmmt0102 into wa_zmmt0102.
                wa_zmmt0102-docnum = wa_sementes-docnum.
                wa_zmmt0102-reftyp = wa_j_1bnflin_02-reftyp.
                append wa_zmmt0102 to it_zmmt0102.
                clear wa_zmmt0102.
              endloop.

            endif.

            clear: wekbe, wa_ekbe, wa_zmmt0102.
          endloop.
        endif.

      when 'ZW'.

        select single *
          from zfiwrt0008 into @data(wa_zfiwrt0008)
         where seq_lcto eq @wa_j_1bnflin_02-refkey.

        if sy-subrc = 0.
          select  *
            from mseg into table tmseg
          where  mblnr eq wa_zfiwrt0008-mblnr
            and  mjahr eq wa_zfiwrt0008-mjahr
            and  shkzg eq 'S'.

          sort tmseg by mblnr mjahr zeile.
          delete adjacent duplicates from tmseg comparing mblnr mjahr zeile.

          loop at tmseg  into wmseg.
            wmseg-docnum = wa_sementes-docnum.
            append wmseg to it_mseg.
            clear wmseg.
          endloop.

*"// WBARBOSA - 16/05/2025 #US-168932
          select *
            from zfiwrt0009
            into table it_fiwrt0009
          where seq_lcto eq wa_zfiwrt0008-seq_lcto.
*"// WBARBOSA - 16/05/2025 #US-168932

        endif.

        select  ebeln  ebelp line_id mblnr
                mjahr  charg nr_fase matnr
                werks  lgort categoria  menge
                tcode  renas
        from zmmt0102 into table tzmmt0102
       where ebeln eq wa_j_1bnflin_02-refkey
         and charg eq wa_j_1bnflin_02-charg.

        loop at tzmmt0102 into wa_zmmt0102.
          wa_zmmt0102-docnum = wa_sementes-docnum.
          wa_zmmt0102-reftyp = wa_j_1bnflin_02-reftyp.
          append wa_zmmt0102 to it_zmmt0102.
          clear wa_zmmt0102.
        endloop.

      when 'MD'.

        select *
          from mseg into table tmseg
         where mblnr eq wa_j_1bnflin_02-refkey+0(10)
          and  mjahr eq wa_j_1bnflin_02-refkey+10(4).

*"// WBARBOSA - 16/05/2025 #US-168932
        read table tmseg into data(lsmseg) with key shkzg = 'H'.
        loop at tmseg assigning field-symbol(<fs_tmseg>) where charg is initial.
          <fs_tmseg>-charg = lsmseg-charg.
        endloop.
*"// WBARBOSA - 16/05/2025 #US-168932

        sort tmseg by mblnr mjahr zeile.
        delete adjacent duplicates from tmseg comparing mblnr mjahr zeile.

        loop at tmseg  into wmseg.
          wmseg-docnum = wa_sementes-docnum.
          append wmseg to it_mseg.
          clear wmseg.
        endloop.

        select single *
          from zfiwrt0008 into wa_zfiwrt0008
         where mblnr eq wa_j_1bnflin_02-refkey+0(10).

*"// WBARBOSA - 16/05/2025 #US-168932
        if sy-subrc is initial.
          select *
            from zfiwrt0009
            into table it_fiwrt0009
          where seq_lcto eq wa_zfiwrt0008-seq_lcto.
        endif.
*"// WBARBOSA - 16/05/2025 #US-168932

*          SELECT EBELN  EBELP LINE_ID MBLNR
*                 MJAHR  CHARG NR_FASE MATNR
*                 WERKS  LGORT CATEGORIA  MENGE
*                 TCODE  RENAS
*          FROM ZMMT0102 INTO  TABLE TZMMT0102
*         WHERE EBELN EQ WA_ZFIWRT0008-SEQ_LCTO
*           AND CHARG EQ WA_J_1BNFLIN_02-CHARG
*           AND WERKS EQ WA_J_1BNFLIN_02-WERKS.

        if sy-subrc = 0.
          if it_mseg is not initial.
            select ebeln  ebelp line_id mblnr
                   mjahr  charg nr_fase matnr
                   werks  lgort categoria  menge
                   tcode  renas
            from zmmt0102 into  table tzmmt0102
            for all entries in it_mseg
           where ebeln eq wa_zfiwrt0008-seq_lcto
             and charg eq it_mseg-charg.
*             AND WERKS EQ IT_MSEG-WERKS.

            loop at tzmmt0102 into wa_zmmt0102.
              wa_zmmt0102-docnum = wa_sementes-docnum.
              wa_zmmt0102-reftyp = wa_j_1bnflin_02-reftyp.
              append wa_zmmt0102 to it_zmmt0102.
              clear wa_zmmt0102.
            endloop.
          endif.
        endif.
    endcase.

    sort it_mseg by mblnr mjahr zeile.
    delete adjacent duplicates from it_mseg comparing mblnr mjahr zeile.
    move-corresponding it_mseg to it_mseg_02.

    clear: wa_sementes, wa_j_1bnflin_02, vxblnr,  wa_zmmt0102, wa_mseg, wa_zfiwrt0008.
  endloop.

  sort it_mseg by matnr werks charg.
  delete adjacent duplicates from it_mseg comparing matnr werks charg.


  loop at it_mseg into wa_mseg.

    move wa_mseg to wa_mseg_03.
    clear wa_mseg_03-menge.

    loop at it_mseg_02 into wa_mseg_02
        where matnr = wa_mseg-matnr
         and  charg = wa_mseg-charg
         and  mjahr = wa_mseg-mjahr.

      wa_mseg_03-menge =  wa_mseg_03-menge + wa_mseg_02-menge.
    endloop.

    append wa_mseg_03 to it_mseg_03.
    clear: wa_mseg,  wa_mseg_02, wa_mseg_03.

  endloop.

  refresh:  it_mseg, it_mseg_02.
  move-corresponding it_mseg_03 to it_mseg.


endform.


form  z_ws_entrada_sm.

  data: wa_itens_semente type ty_itens_entrada_sementes,
*** Inicio - Rubenilson Pereira - 07.04.25 #168932
        lo_mm_util       type ref to zcl_mm_util,
        lv_matnr         type matnr18.

  data: vl_object type cuobn.

  create object lo_mm_util.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro = abap_false.

    wa_entrada_sementes-cnpj_revenda = wa_sementes-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service  = wa_zauth_webservice-service
                                                               bukrs    = wa_sementes-bukrs
                                                               branch   = wa_sementes-branch.
    if sy-subrc = 0.
      wa_entrada_sementes-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_entrada_sementes-origem_armazenador = 'false'.

    read table it_lfa1 into wa_lfa1 with key lifnr = wa_sementes-parid.
    if sy-subrc = 0.
      wa_entrada_sementes-cod_municipio_fornecedor = wa_lfa1-txjcd+3(12).
      wa_entrada_sementes-nome_fornecedor          = wa_lfa1-name1.
      wa_entrada_sementes-cpf_cnpj_fornecedor   = wa_lfa1-stcd1.
    endif.

    wa_entrada_sementes-observacao = ' '.

    if wa_sementes-nfenum is not initial.
      wa_entrada_sementes-nr_nf = wa_sementes-nfenum.
    else.
      wa_entrada_sementes-nr_nf = wa_sementes-nfnum.
    endif.

    if wa_sementes-series is not initial.
      wa_entrada_sementes-serie_nf = wa_sementes-series.
    else.
      clear mensagem.
      verro = abap_true.
      mensagem = 'A Série da nota Fiscal não está preenchido!'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    concatenate wa_sementes-docdat+0(4) '-' wa_sementes-docdat+4(2) '-' wa_sementes-docdat+6(2) into wa_entrada_sementes-dt_nf.
    concatenate wa_sementes-pstdat+0(4) '-' wa_sementes-pstdat+4(2) '-' wa_sementes-pstdat+6(2) into wa_entrada_sementes-data_entrada.
    wa_entrada_sementes-cod_tipo_produto = '2'.

    select  single * from zsdt0216 into @data(wa_zsdt0216)
      where kunnr eq @wa_sementes-parid
      and   tipo  eq 'F'.

    if  sy-subrc = 0.
      wa_entrada_sementes-renasem_fornecedor = wa_zsdt0216-renasem.
    else.
      clear mensagem.
      verro = abap_true.
      mensagem = 'RENASEM não informado no cadastro na transação ZSDT0154'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.


    "itens
    if it_mseg is not initial.
      loop at it_mseg into wa_mseg  where docnum = wa_sementes-docnum.

        read table it_zmmt0102 into wa_zmmt0102 with key mblnr  = wa_mseg-mblnr
                                                         mjahr  = wa_mseg-mjahr
                                                         charg  = wa_mseg-charg
                                                         matnr  = wa_mseg-matnr.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
        lv_matnr = wa_mseg-matnr(18).
        call method lo_mm_util->get_codigo_indea(
          exporting
            i_material = lv_matnr
          importing
            id_indea   = wa_itens_semente-cod_cultivar ).

*** Fim - Rubenilson Pereira - 07.04.25 #168932
        if wa_itens_semente-cod_cultivar is initial.
          select single id_matnr_idea
            from zsdt0210
          into wa_itens_semente-cod_cultivar
            where matnr eq wa_mseg-matnr.
          if sy-subrc is not initial.

            clear mensagem.
            verro =  abap_true.
            mensagem = 'De/Para entre Material SAP x Cod. Cultivar Indea não realizado!'.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_sementes-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            commit work.
            clear wa_zsdt0214.
            exit.
          endif.
        endif.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

*      "// Material + Lote
        clear: ls_obj, vl_object.
        ls_obj-matnr = wa_mseg-matnr.
        ls_obj-charg = wa_mseg-charg.
        vl_object = ls_obj.

        call method lo_mm_util->get_caracteristica_geral
          exporting
            i_object               = vl_object
            i_caracteristica       = 'SEMENTE_CATEGORIA'
          importing
            e_valor_caracteristica = data(lv_semente_categoria).

        wa_zmmt0102-categoria = lv_semente_categoria.

        wa_itens_semente-lote = wa_mseg-charg.

        if wa_zmmt0102-categoria is not initial.
          wa_itens_semente-cod_categoria_sementes =  wa_zmmt0102-categoria.
        else.
          clear mensagem.
          verro = abap_true.
*          MENSAGEM = 'O Lote está sem  Categoria da Semente'.
          concatenate 'O Lote' wa_mseg-charg 'está sem  Categoria da Semente, verifique as informações da MIGO.' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        endif.

        wa_itens_semente-nr_dar_indea = ' '.

        if wa_zmmt0102-nr_fase is not initial.
          wa_itens_semente-nr_dar_fase = wa_zmmt0102-nr_fase.
        else.
          clear mensagem.
          verro = abap_true.
*          MENSAGEM = 'O lote está sem o Código Fase'.
          concatenate 'O Lote' wa_mseg-charg 'está sem o Código Fase, verifique as informações da MIGO.' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        endif.

        case  wa_mseg-meins.
          when 'KG'.
            wa_itens_semente-qnt_produto = wa_mseg-menge.
          when 'TO'.
            wa_itens_semente-qnt_produto = wa_mseg-menge * 1000.
          when 'BAG' or 'SAC' or 'BIG'.
            select single *
              from mara into @data(wa_mara)
             where matnr eq @wa_mseg-matnr.

*            data: vl_object type ausp-objek.

*      "// Material + Lote
            clear: ls_obj, vl_object.
            ls_obj-matnr = wa_mseg-matnr.
            ls_obj-charg = wa_mseg-charg.
            vl_object = ls_obj.

            call method lo_mm_util->get_caracteristica_geral
              exporting
                i_object               = vl_object
                i_caracteristica       = 'PESO_BAG'
              importing
                e_valor_caracteristica = data(lv_peso_bag).

            wa_itens_semente-qnt_produto = wa_mseg-menge * lv_peso_bag.

**** Inicio - Rubenilson Pereira - 07.04.25 #168932
*            CALL METHOD LO_MM_UTIL->GET_PESO_BAG(
*              EXPORTING
*                I_MATERIAL = LV_MATNR
*              IMPORTING
*                R_PESO_BAG = LV_PESO_BAG ).
*
**            wa_itens_semente-qnt_produto = wa_mseg-menge * wa_mara-brgew.
*            WA_ITENS_SEMENTE-QNT_PRODUTO = LV_PESO_BAG * WA_MARA-BRGEW.
**** Fim - Rubenilson Pereira - 07.04.25 #168932

        endcase.

        append value #(  cod_cultivar              =     wa_itens_semente-cod_cultivar
                         cod_categoria_sementes    =     wa_itens_semente-cod_categoria_sementes
                         nr_dar_indea              =     wa_itens_semente-nr_dar_indea
                         nr_dar_fase               =     wa_itens_semente-nr_dar_fase
                         lote                      =     wa_itens_semente-lote
                         qnt_produto               =     wa_itens_semente-qnt_produto
                      ) to wa_entrada_sementes-itens.


        clear: wa_zmmt0102, wa_mseg.
      endloop.

    else.
      clear mensagem.
      verro = abap_true.
      mensagem = 'Não foi possível localizar os itens da nota (MIGO) para envio ao SISDEV!'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.


    check verro = abap_false.

    if wzsdt0212-id is not initial.
      wa_entrada_sementes-cod_lancamento_usuario =  wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021  using  'ZSEQ_INDEA' '01'
                                                  changing wa_entrada_sementes-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.
    e_xml = /ui2/cl_json=>serialize( data = wa_entrada_sementes compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                               wa_zauth_ws_0001-username
                               wa_zauth_ws_0001-password
                               tp_metodo  changing resultado.

    translate resultado to upper case.

    if  resultado ca sy-abcde .

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.

      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_entrada_sementes-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.
*     wa_zsdt0212-status       =  'E'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.

    elseif  resultado ca '0123456789'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.


      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_entrada_sementes-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  endif.
endform.


form z_ws_cancel_sm.

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.
  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).
    verro = abap_false.

    wa_cancel_entrada_sementes-cnpj_revenda = wa_sementes-cnpj_bupla.
    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                               bukrs  =  wa_sementes-bukrs
                                                               branch =  wa_sementes-branch.
    if sy-subrc = 0.
      wa_cancel_entrada_sementes-hash_revenda =  wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_cancel_entrada_sementes-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_entrada_sementes-oigem_armazenador      = 'false'.
    wa_cancel_entrada_sementes-motivo_cancelamento    = 'Dados da Nota estão incorretos'.


    check verro = abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_entrada_sementes compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo changing resultado.
      translate resultado to upper case.

      if  resultado ca sy-abcde .

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_sementes-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_sementes-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif  wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_sementes-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
    clear wa_zsdt0212.
  endif.
endform.

form z_ws_saida_sm.
  data: wa_itens_saida_sementes type ty_itens_saida_sementes,
*** Inicio - Rubenilson Pereira - 07.04.25 #168932
        lo_mm_util              type ref to zcl_mm_util,
        lv_matnr                type matnr18,
        lv_renasem              type zsdt0216-renasem.

  create object lo_mm_util.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro = abap_false.
    "DIFERENTE DE 'ZC'
    read table it_j_1bnflin_02 into wa_j_1bnflin_02 with key docnum = wa_sementes-docnum.

    if ( wa_sementes-nftype = 'ZA' or wa_sementes-nftype = 'YD' ) and wa_j_1bnflin_02-itmtyp <> 'ZC'.

      wa_saida_sementes-cnpj_revenda = wa_sementes-cnpj_bupla.

      read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_SEMENTES'.

      read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service =  wa_zauth_webservice-service
                                                                 bukrs   =  wa_sementes-bukrs
                                                                 branch  =  wa_sementes-branch.
      if sy-subrc = 0.
        wa_saida_sementes-hash_revenda = wa_zauth_ws_0001-add01.
      else.
        clear mensagem.
        verro = abap_true.
        concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.
        exit.
      endif.

      if wa_sementes-regio = 'MT'.
        wa_saida_sementes-destino_fora_m_t = 'false'.
      else.
        wa_saida_sementes-destino_fora_m_t =  'true'.
      endif.

      wa_saida_sementes-observacao = ' '.
      wa_saida_sementes-destino_internacional = 'false'.

      if wa_sementes-nfenum is not initial.
        wa_saida_sementes-nr_nf = wa_sementes-nfenum.
      else.
        wa_saida_sementes-nr_nf = wa_sementes-nfnum.
      endif.

      wa_saida_sementes-serie_nf = wa_sementes-series.
      concatenate  wa_sementes-docdat+0(4) '-' wa_sementes-docdat+4(2) '-'  wa_sementes-docdat+6(2) into  wa_saida_sementes-dt_nf.


*** Inicio - Rubenilson Pereira - 07.04.25 #168932
      lv_matnr = wa_j_1bnflin_02-matnr(18).

      call method lo_mm_util->get_renasem(
        exporting
          i_material = lv_matnr
        importing
          r_renasem  = lv_renasem ).

      if lv_renasem is not initial.
        wa_saida_sementes-renasem_destino      = lv_renasem.
      else.

        select single * from zsdt0216 into @data(wa_zsdt0216)
          where kunnr           eq @wa_sementes-parid
          and   tipo            eq 'C'
          and   setor_atividade eq 'S'
          and   revenda         eq 'S'
          and   renasem         ne ''.

        wa_saida_sementes-renasem_destino      = wa_zsdt0216-renasem.

      endif.

      if wa_saida_sementes-renasem_destino is not initial.
        wa_saida_sementes-destino_revenda      = 'true'.
      else.
        wa_saida_sementes-destino_revenda      = 'false'.
        wa_saida_sementes-renasem_destino      = ' '.
      endif.

      wa_saida_sementes-destino_armazenador  = 'false'.

      if wa_saida_sementes-destino_fora_m_t =  'false'.

        select single  *  from zsdt0206 into @data(wa_zsdt0206)
         where kunnr eq @wa_sementes-parid.

        if sy-subrc = 0.
          wa_saida_sementes-cpf_cnpj_destino = wa_zsdt0206-cnpj.
        else.
          clear mensagem.
          verro =  abap_true.
          mensagem = 'Cliente SAP não localizado na base do SISDEV-INDEA'.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        endif.

      endif.

      read table it_kna1 into wa_kna1 with key kunnr = wa_sementes-parid.

      wa_kna1-name1 = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = conv #( wa_kna1-name1 ) ) ).

      wa_saida_sementes-nome_destino          = wa_kna1-name1.
      wa_saida_sementes-cod_municipio_destino = wa_kna1-txjcd+3(12).
      wa_saida_sementes-cod_tipo_produto      = 2.
      if wa_saida_sementes-destino_fora_m_t =  'true'.

        if wa_kna1-stcd1 is not initial.
          wa_saida_sementes-cpf_cnpj_destino = wa_kna1-stcd1.
        elseif wa_kna1-stcd2 is not initial.
          wa_saida_sementes-cpf_cnpj_destino = wa_kna1-stcd2.
        endif.
      endif.

      loop at it_j_1bnflin_02 into wa_j_1bnflin_02
        where docnum = wa_sementes-docnum.

        wa_itens_saida_sementes-lote         = wa_j_1bnflin_02-charg.
        "WA_ITENS_SAIDA_SEMENTES-NR_DAR_INDEA = ' '.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932

        lv_matnr = wa_j_1bnflin_02-matnr. "GABRIEL AVILA - AJUSTE COD ID INDEA - 09.10.2025"

        call method lo_mm_util->get_codigo_indea(
          exporting
            i_material = lv_matnr
          importing
            id_indea   = wa_itens_saida_sementes-cod_cultivar ).
*        IF sy-subrc = 0.
        if wa_itens_saida_sementes-cod_cultivar is initial.
          read table it_zsdt0210 into wa_zsdt0210 with key matnr = wa_j_1bnflin_02-matnr.
          if sy-subrc is initial.
            wa_itens_saida_sementes-cod_cultivar =  wa_zsdt0210-id_matnr_idea.
          else.
*** Fim - Rubenilson Pereira - 07.04.25 #168932
            clear mensagem.
            verro =  abap_true.
            mensagem = 'De/Para entre Material SAP x Cod. Cultivar Indea não realizado!'.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_sementes-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            commit work.
            clear wa_zsdt0214.
            exit.
          endif.
        endif.

        select single *  from zsdt0001 into @data(wa_zsdt0001)
         where fatura_prod eq @wa_j_1bnflin_02-refkey.

        select single * from vbfa into @data(wa_vbfa)
          where vbeln   eq @wa_j_1bnflin_02-refkey
            and posnn   eq @wa_j_1bnflin_02-refitm
            and vbtyp_n	eq 'M'
            and vbtyp_v	eq 'C'.

        select single * from zsdt0134 into @data(wa_zsdt0134)
          where  vbeln   eq @wa_vbfa-vbelv
             and posnr 	 eq @wa_vbfa-posnv
             and nro_cg  eq @wa_zsdt0001-nro_cg.

*        IF WA_ZSDT0134-NR_FASE IS NOT INITIAL.
*          WA_ITENS_SAIDA_SEMENTES-NR_DAR_FASE = WA_ZSDT0134-NR_FASE.
*        ELSE.
*          CLEAR MENSAGEM.
*          VERRO = ABAP_TRUE.
*          MENSAGEM = 'O lote está sem o Código Fase'.
*
*          PERFORM GET_NEXT_NUMBER IN PROGRAM ZSDR021 USING  'ZSEQ_L_IND'  '01'  CHANGING  WA_ZSDT0214-ID.
*          WA_ZSDT0214-DATA_ATUAL   = SY-DATUM.
*          WA_ZSDT0214-HORA_ATUAL   = SY-UZEIT.
*          WA_ZSDT0214-DOCNUM       = WA_SEMENTES-DOCNUM.
*          WA_ZSDT0214-ORIGEM       = '1'.
*          WA_ZSDT0214-MENSAGEM     = MENSAGEM.
*          WA_ZSDT0214-USNAM        = SY-UNAME.
*          MODIFY ZSDT0214 FROM WA_ZSDT0214.
*          COMMIT WORK.
*          CLEAR WA_ZSDT0214.
*          EXIT.
*        ENDIF.


        case wa_j_1bnflin_02-meins.
          when 'KG'.
            wa_itens_saida_sementes-qnt_produto = wa_j_1bnflin_02-menge.
          when 'TO'.
            wa_itens_saida_sementes-qnt_produto = wa_j_1bnflin_02-menge * 1000.
          when 'BAG' or 'SAC' or 'BIG'.
            select single * from mara into @data(wa_mara)
              where matnr eq @wa_j_1bnflin_02-matnr.

            data: vl_object type ausp-objek.

*      "// Material + Lote
            clear: ls_obj, vl_object.
            ls_obj-matnr = wa_j_1bnflin_02-matnr.
            ls_obj-charg = wa_j_1bnflin_02-charg.
            vl_object = ls_obj.

            call method lo_mm_util->get_caracteristica_geral
              exporting
                i_object               = vl_object
                i_caracteristica       = 'PESO_BAG'
              importing
                e_valor_caracteristica = data(lv_peso_bag).

            wa_itens_saida_sementes-qnt_produto = wa_j_1bnflin_02-menge * lv_peso_bag.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
*            CALL METHOD LO_MM_UTIL->GET_PESO_BAG(
*              EXPORTING
*                I_MATERIAL = LV_MATNR
*              IMPORTING
*                R_PESO_BAG = LV_PESO_BAG ).
*            wa_itens_saida_sementes-qnt_produto = wa_j_1bnflin_02-menge * wa_mara-brgew.
*            WA_ITENS_SAIDA_SEMENTES-QNT_PRODUTO = LV_PESO_BAG * WA_MARA-BRGEW.
*** Fim - Rubenilson Pereira - 07.04.25 #168932
        endcase.


        append value #( cod_cultivar   = wa_itens_saida_sementes-cod_cultivar
                        lote           = wa_itens_saida_sementes-lote
                        "NR_DAR_INDEA   = WA_ITENS_SAIDA_SEMENTES-NR_DAR_INDEA
                        "NR_DAR_FASE    = WA_ITENS_SAIDA_SEMENTES-NR_DAR_FASE
                        qnt_produto    = wa_itens_saida_sementes-qnt_produto
                       )   to wa_saida_sementes-itens.
      endloop.

      check verro = abap_false.

      if wzsdt0212-id is not initial.
        wa_saida_sementes-cod_lancamento_usuario =  wzsdt0212-id.
      else.
        perform get_next_number in program zsdr021
                                using  'ZSEQ_INDEA' '01' changing wa_saida_sementes-cod_lancamento_usuario.
      endif.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_saida_sementes compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo  changing resultado.
      translate resultado to upper case.

      if  resultado ca sy-abcde .

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_saida_sementes-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.

        if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
          wa_zsdt0212-status   =  'A'.
          wa_zsdt0212-id_indea =  '9999999999'.
        else.
          wa_zsdt0212-status     =  'E'.
        endif.
*       wa_zsdt0212-status       =  'E'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.


        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_saida_sementes-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.
    endif.
  endif.

  clear: wa_zsdt0216.

endform.


form z_ws_cancel_saida_sm .

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro = abap_false.

    wa_cancel_saida_sementes-cnpj_revenda = wa_sementes-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key  service = wa_zauth_webservice-service
                                                                bukrs   = wa_sementes-bukrs
                                                                branch  = wa_sementes-branch.
    if sy-subrc = 0.
      wa_cancel_saida_sementes-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_cancel_saida_sementes-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_saida_sementes-motivo_cancelamento    = 'Dados da Nota estão incorretos'.
    wa_cancel_saida_sementes-destino_armazenador    = 'false'.


    check  verro = abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_saida_sementes compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo
         changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_saida_sementes-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.


      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.


        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_saida_sementes-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_sementes-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
    clear wa_zsdt0212.
  endif.
endform.


form z_ws_devolucao_sm.

  data: wa_itens_devolucao_sementes type ty_itens_devolucao_sementes,
*** Inicio - Rubenilson Pereira - 07.04.25 #168932
        lo_mm_util                  type ref to zcl_mm_util,
        lv_matnr                    type matnr18.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro =  abap_false.

    wa_devolucao_sementes-cnpj_revenda =  wa_sementes-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_DEVOLUCAO_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service =  wa_zauth_webservice-service
                                                               bukrs   =  wa_sementes-bukrs
                                                               branch  =  wa_sementes-branch.
    if sy-subrc = 0.
      wa_devolucao_sementes-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro =  abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    read table it_j_1bnflin_02 into data(wa_flin)  with key docnum = wa_sementes-docnum.

    select single * from vbfa into @data(wa_vbfa)
      where  vbeln    eq @wa_flin-refkey
      and    posnn    eq @wa_flin-refitm
      and    vbtyp_n  eq 'O'
      and    vbtyp_v  eq 'M'.

    select single * from j_1bnflin into @data(wa_1bn)
      where refkey  eq @wa_vbfa-vbelv
      and   refitm  eq @wa_vbfa-posnv.

    select single * from zsdt0212 into wa_zsdt0212
      where docnum eq wa_1bn-docnum.

    if sy-subrc = 0.
      wa_devolucao_sementes-cod_saida_lancamento_usuario =  wa_zsdt0212-id.
    else.
      clear mensagem.
      verro =  abap_true.
      concatenate 'A nota fiscal de venda (DocNum:' wa_1bn-docnum ') ainda não foi enviada ao Indea.' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.


    concatenate  wa_sementes-docdat+0(4) '-'
                 wa_sementes-docdat+4(2) '-'
                 wa_sementes-docdat+6(2) into wa_devolucao_sementes-data_devolucao.

    wa_devolucao_sementes-observacao = ' '.

    loop at it_j_1bnflin_02 into wa_j_1bnflin_02
      where docnum = wa_sementes-docnum.

      read table it_zsdt0210 into wa_zsdt0210 with key matnr = wa_j_1bnflin_02-matnr.
      if sy-subrc = 0.
        wa_itens_devolucao_sementes-cod_cultivar = wa_zsdt0210-id_matnr_idea.
      else.
        clear mensagem.
        verro =  abap_true.
        mensagem = 'De/Para entre Material SAP x Cod. Cultivar Indea não realizado!'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.
        exit.
      endif.

      wa_itens_devolucao_sementes-lote = wa_j_1bnflin_02-charg.

      case wa_j_1bnflin_02-meins.
        when 'KG'.
          wa_itens_devolucao_sementes-qnt_produto = wa_j_1bnflin_02-menge.
        when 'TO'.
          wa_itens_devolucao_sementes-qnt_produto = wa_j_1bnflin_02-menge * 1000.
        when 'BAG' or 'SAC' or 'BIG'.
          select single * from mara into @data(wa_mara)
            where matnr eq @wa_j_1bnflin_02-matnr.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
          lv_matnr = wa_j_1bnflin_02-matnr(18).

          data: vl_object type ausp-objek.

*      "// Material + Lote
          clear: ls_obj, vl_object.
          ls_obj-matnr = wa_j_1bnflin_02-matnr.
          ls_obj-charg = wa_j_1bnflin_02-charg.
          vl_object = ls_obj.

          call method lo_mm_util->get_caracteristica_geral
            exporting
              i_object               = vl_object
              i_caracteristica       = 'PESO_BAG'
            importing
              e_valor_caracteristica = data(lv_peso_bag).

          wa_itens_devolucao_sementes-qnt_produto = wa_j_1bnflin_02-menge * lv_peso_bag.

*          CALL METHOD LO_MM_UTIL->GET_PESO_BAG(
*            EXPORTING
*              I_MATERIAL = LV_MATNR
*            IMPORTING
*              R_PESO_BAG = LV_PESO_BAG ).
*
*          wa_itens_devolucao_sementes-qnt_produto = wa_j_1bnflin_02-menge * wa_mara-brgew.
*          WA_ITENS_DEVOLUCAO_SEMENTES-QNT_PRODUTO = LV_PESO_BAG * WA_MARA-BRGEW.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

      endcase.
      append value #( cod_cultivar  = wa_itens_devolucao_sementes-cod_cultivar
                      lote          = wa_itens_devolucao_sementes-lote
                      qnt_produto   = wa_itens_devolucao_sementes-qnt_produto
                     ) to wa_devolucao_sementes-itens.
    endloop.

    check verro = abap_false.

    if wzsdt0212-id is not initial.
      wa_devolucao_sementes-cod_lancamento_usuario = wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021 using  'ZSEQ_INDEA' '01' changing wa_devolucao_sementes-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.
    e_xml = /ui2/cl_json=>serialize( data = wa_devolucao_sementes compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                                wa_zauth_ws_0001-username
                                wa_zauth_ws_0001-password
                                tp_metodo   changing resultado.

    translate resultado to upper case.

    if  resultado ca sy-abcde .

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.

      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_devolucao_sementes-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.
*     wa_zsdt0212-status       =  'E'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.

    elseif  resultado ca '0123456789'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.


      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_devolucao_sementes-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  endif.

endform.


form z_ws_cancel_devol_sm .

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.
  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro  = abap_false.

    wa_cancel_devolucao_sementes-cnpj_revenda =  wa_sementes-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service =  'INDEA_DEVOLUCAO_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                               bukrs   = wa_sementes-bukrs
                                                               branch  = wa_sementes-branch.
    if sy-subrc = 0.
      wa_cancel_devolucao_sementes-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro  = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_cancel_devolucao_sementes-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_devolucao_sementes-motivo_cancelamento    = ' '.

    check verro  = abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_devolucao_sementes compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo  changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_devolucao_sementes-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.


      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_devolucao_sementes-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_sementes-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
    clear wa_zsdt0212.
  endif.

endform.

form z_ws_saida_arm_sm .
  data: wa_itens_saida_arm_sm type ty_itens_saida_sementes,
*** Inicio - Rubenilson Pereira - 10.04.25 #168932
        lo_mm_util            type ref to zcl_mm_util,
        lv_matnr              type matnr18.
*** Fim - Rubenilson Pereira - 10.04.25 #168932

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro = abap_false.

    read table it_j_1bnflin_02 into data(wa_j1bnflin) with key docnum = wa_sementes-docnum.

    if wa_sementes-nftype = 'ZA' and wa_j1bnflin-itmtyp = 'ZC'.

      wa_saida_arm_sementes-cnpj_revenda = wa_sementes-cnpj_bupla.

      read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_SEMENTES'.

      read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service =  wa_zauth_webservice-service
                                                                 bukrs   =  wa_sementes-bukrs
                                                                 branch  =  wa_sementes-branch.
      if sy-subrc = 0.
        wa_saida_arm_sementes-hash_revenda = wa_zauth_ws_0001-add01.
      else.
        clear mensagem.
        verro = abap_true.
        concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.
        exit.
      endif.

      if wa_sementes-regio = 'MT'.
        wa_saida_arm_sementes-destino_fora_m_t = 'false'.
      else.
        wa_saida_arm_sementes-destino_fora_m_t =  'true'.
      endif.

      if wa_sementes-nfenum is not initial.
        wa_saida_arm_sementes-nr_nf = wa_sementes-nfenum.
      else.
        wa_saida_arm_sementes-nr_nf = wa_sementes-nfnum.
      endif.

      wa_saida_arm_sementes-serie_nf = wa_sementes-series.
      concatenate  wa_sementes-docdat+0(4) '-' wa_sementes-docdat+4(2) '-'  wa_sementes-docdat+6(2) into  wa_saida_arm_sementes-dt_nf.

      wa_saida_arm_sementes-destino_revenda      = 'false'.
      wa_saida_arm_sementes-destino_armazenador  = 'true'.
      wa_saida_arm_sementes-renasem_destino      = ' '.
      wa_saida_arm_sementes-destino_internacional = 'false'.

      wa_saida_arm_sementes-observacao = ' '.

      read table it_lfa1 into wa_lfa1 with key lifnr = wa_sementes-parid.
      if sy-subrc = 0.
        wa_lfa1-name1 = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = conv #( wa_lfa1-name1 ) ) ).

        wa_saida_arm_sementes-cpf_cnpj_destino      = wa_lfa1-stcd1.
        wa_saida_arm_sementes-nome_destino          = wa_lfa1-name1.
        wa_saida_arm_sementes-cod_municipio_destino = wa_lfa1-txjcd+3(12).
      endif.

      wa_saida_arm_sementes-cod_tipo_produto      = 2.

      loop at it_mseg into wa_mseg
            where docnum = wa_sementes-docnum.

        read table it_zsdt0210 into wa_zsdt0210 with key matnr = wa_mseg-matnr.
        if sy-subrc = 0.
          wa_itens_saida_arm_sm-cod_cultivar =  wa_zsdt0210-id_matnr_idea.
        else.
          clear mensagem.
          verro = abap_true.
          mensagem = 'De/Para entre Material SAP x Cod. Cultivar Indea não realizado!'.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        endif.

        wa_itens_saida_arm_sm-lote   = wa_mseg-charg.

        "        READ TABLE IT_ZMMT0102 INTO WA_ZMMT0102 WITH KEY CHARG  = WA_MSEG-CHARG
        "                                                         MATNR  = WA_MSEG-MATNR
        "                                                         DOCNUM = WA_SEMENTES-DOCNUM.

*        IF WA_ZMMT0102-NR_FASE IS NOT INITIAL.
*          WA_ITENS_SAIDA_ARM_SM-NR_DAR_FASE  = WA_ZMMT0102-NR_FASE.
*          WA_ITENS_SAIDA_ARM_SM-NR_DAR_INDEA = ''.
*        ELSE.
*          CLEAR MENSAGEM.
*          VERRO = ABAP_TRUE.
*          MENSAGEM = 'O lote está sem o Código Fase'.
*
*          PERFORM GET_NEXT_NUMBER IN PROGRAM ZSDR021 USING  'ZSEQ_L_IND'  '01'  CHANGING  WA_ZSDT0214-ID.
*          WA_ZSDT0214-DATA_ATUAL   = SY-DATUM.
*          WA_ZSDT0214-HORA_ATUAL   = SY-UZEIT.
*          WA_ZSDT0214-DOCNUM       = WA_SEMENTES-DOCNUM.
*          WA_ZSDT0214-ORIGEM       = '1'.
*          WA_ZSDT0214-MENSAGEM     = MENSAGEM.
*          WA_ZSDT0214-USNAM        = SY-UNAME.
*          MODIFY ZSDT0214 FROM WA_ZSDT0214.
*          COMMIT WORK.
*          CLEAR WA_ZSDT0214.
*          EXIT.
*        ENDIF.

        case wa_mseg-meins.
          when 'KG'.
            wa_itens_saida_arm_sm-qnt_produto = wa_mseg-menge.
          when 'TO'.
            wa_itens_saida_arm_sm-qnt_produto = wa_mseg-menge * 1000.
          when 'BAG' or 'SAC' or 'BIG'.
            select single * from mara into @data(wa_mara) where matnr eq @wa_mseg-matnr.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
            lv_matnr = wa_mseg-matnr(18).

            data: vl_object type ausp-objek.

*      "// Material + Lote
            clear: ls_obj, vl_object.
            ls_obj-matnr = wa_mseg-matnr.
            ls_obj-charg = wa_mseg-charg.
            vl_object = ls_obj.

            call method lo_mm_util->get_caracteristica_geral
              exporting
                i_object               = vl_object
                i_caracteristica       = 'PESO_BAG'
              importing
                e_valor_caracteristica = data(lv_peso_bag).

            wa_itens_saida_arm_sm-qnt_produto = wa_mseg-menge * lv_peso_bag.

*            CALL METHOD LO_MM_UTIL->GET_PESO_BAG(
*              EXPORTING
*                I_MATERIAL = LV_MATNR
*              IMPORTING
*                R_PESO_BAG = LV_PESO_BAG ).
*            wa_itens_saida_arm_sm-qnt_produto = wa_mseg-menge * wa_mara-brgew.
*            WA_ITENS_SAIDA_ARM_SM-QNT_PRODUTO = LV_PESO_BAG * WA_MARA-BRGEW.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

        endcase.

        append value #( cod_cultivar   = wa_itens_saida_arm_sm-cod_cultivar
                        lote           = wa_itens_saida_arm_sm-lote
                        "NR_DAR_INDEA   = WA_ITENS_SAIDA_ARM_SM-NR_DAR_INDEA
                        "NR_DAR_FASE    = WA_ITENS_SAIDA_ARM_SM-NR_DAR_FASE
                        qnt_produto    = wa_itens_saida_arm_sm-qnt_produto
                       )   to wa_saida_arm_sementes-itens.

        clear wa_zmmt0102.
        clear wa_mseg.
      endloop.

      check verro = abap_false.

      if wzsdt0212-id is not initial.
        wa_saida_arm_sementes-cod_lancamento_usuario = wzsdt0212-id.
      else.
        perform get_next_number in program zsdr021 using  'ZSEQ_INDEA' '01' changing wa_saida_arm_sementes-cod_lancamento_usuario.
      endif.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_saida_arm_sementes  compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo  changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_saida_arm_sementes-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.

        if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
          wa_zsdt0212-status   =  'A'.
          wa_zsdt0212-id_indea =  '9999999999'.
        else.
          wa_zsdt0212-status     =  'E'.
        endif.
*       wa_zsdt0212-status       =  'E'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.


        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_saida_arm_sementes-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.
    endif.
  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATA_DADOS_CANCEL_ARM_SM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_cancel_arm_sm .

  clear: mensagem,  tp_metodo, resultado, purl.

  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro = abap_false.

    read table it_j_1bnflin_02 into data(wa_j1bnflin) with key docnum = wa_sementes-docnum.

    if wa_sementes-nftype = 'ZA' and wa_j1bnflin-itmtyp = 'ZC'.

      wa_cancel_saida_arm_sementes-cnpj_revenda = wa_sementes-cnpj_bupla.

      read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_SEMENTES'.

      read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service =  wa_zauth_webservice-service
                                                                 bukrs   =  wa_sementes-bukrs
                                                                 branch  =  wa_sementes-branch.
      if sy-subrc = 0.
        wa_cancel_saida_arm_sementes-hash_revenda = wa_zauth_ws_0001-add01.
      else.
        clear mensagem.
        verro = abap_true.
        concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.
        exit.
      endif.

      wa_cancel_saida_arm_sementes-cod_lancamento_usuario = wzsdt0212-id.
      wa_cancel_saida_arm_sementes-destino_armazenador    = 'false'.
      wa_cancel_saida_arm_sementes-motivo_cancelamento    = 'Dados da Nota estão incorretos'.


      check verro = abap_false.

      if wzsdt0212-status = 'A'.

        purl = wa_zauth_webservice-url.
        e_xml = /ui2/cl_json=>serialize( data = wa_cancel_saida_arm_sementes compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

        perform z_envia_json using purl
                                   wa_zauth_ws_0001-username
                                   wa_zauth_ws_0001-password
                                   tp_metodo   changing resultado.
        translate resultado to upper case.

        if  resultado ca sy-abcde .
          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '2'.
          wa_zsdt0214-mensagem     = resultado.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.

          wa_zsdt0212-docnum       =  wa_sementes-docnum.
          wa_zsdt0212-id           =  wa_saida_arm_sementes-cod_lancamento_usuario.
**          wa_zsdt0212-id_indea     =  resultado.
          wa_zsdt0212-usnam        =  sy-uname.
          wa_zsdt0212-data_atual   =  sy-datum.
          wa_zsdt0212-hora_atual   =  sy-uzeit.
          if resultado eq retorno_cancel.
            wa_zsdt0212-status       =  'C'.
          else.
            wa_zsdt0212-status       =  'E'.
          endif.
          modify zsdt0212 from wa_zsdt0212.
          commit work.
          clear wa_zsdt0212.

        elseif  resultado ca '0123456789'.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '2'.
          wa_zsdt0214-mensagem     = resultado.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.

          wa_zsdt0212-docnum       =  wa_sementes-docnum.
          wa_zsdt0212-id           =  wa_saida_arm_sementes-cod_lancamento_usuario.
          wa_zsdt0212-id_indea     =  resultado.
          wa_zsdt0212-status       =  'A'.
          wa_zsdt0212-usnam        =  sy-uname.
          wa_zsdt0212-data_atual   =  sy-datum.
          wa_zsdt0212-hora_atual   =  sy-uzeit.
          modify zsdt0212 from wa_zsdt0212.
          commit work.
          clear wa_zsdt0212.
        endif.

      elseif wzsdt0212-status = 'E'.

        wa_zsdt0212-docnum       =  wzsdt0212-docnum.
        wa_zsdt0212-id           =  wzsdt0212-id.
        wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
        wa_zsdt0212-status       =  'X'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_sementes-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
    clear wa_zsdt0212.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATA_DADOS_ENTRADA_ARM_SM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_entrada_arm_sm .

  data: wa_itens_semente_arm type ty_itens_entrada_sementes,
*** Inicio - Rubenilson Pereira - 07.04.25 #168932
        lv_matnr             type matnr18,
        lo_mm_util           type ref to zcl_mm_util.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro = abap_false.

    wa_entrada_sementes_arm-cnpj_revenda = wa_sementes-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service  = wa_zauth_webservice-service
                                                               bukrs    = wa_sementes-bukrs
                                                               branch   = wa_sementes-branch.
    if sy-subrc = 0.
      wa_entrada_sementes_arm-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify  zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_entrada_sementes_arm-origem_armazenador = 'true'.

    read table it_lfa1 into wa_lfa1 with key lifnr = wa_sementes-parid.
    if sy-subrc = 0.
      wa_entrada_sementes_arm-cpf_cnpj_fornecedor   = wa_lfa1-stcd1.
    endif.

    "teste realizado pelo Postman, não foi obrigário o envio dessas informações
    "-----
    wa_entrada_sementes_arm-cod_municipio_fornecedor = ' '.
    wa_entrada_sementes_arm-nome_fornecedor          = ' '.
    wa_entrada_sementes_arm-renasem_fornecedor       = ' '.
    "------

    if wa_sementes-nfenum is not initial.
      wa_entrada_sementes_arm-nr_nf = wa_sementes-nfenum.
    else.
      wa_entrada_sementes_arm-nr_nf = wa_sementes-nfnum.
    endif.

    if wa_sementes-series is not initial.
      wa_entrada_sementes_arm-nr_nf = wa_sementes-series.
    else.
      clear mensagem.
      verro = abap_true.
      mensagem = 'A Série da nota Fiscal não está preenchido!'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.


    concatenate wa_sementes-docdat+0(4) '-' wa_sementes-docdat+4(2) '-' wa_sementes-docdat+6(2) into wa_entrada_sementes_arm-dt_nf.
    concatenate wa_sementes-pstdat+0(4) '-' wa_sementes-pstdat+4(2) '-' wa_sementes-pstdat+6(2) into wa_entrada_sementes_arm-data_entrada.
    wa_entrada_sementes_arm-cod_tipo_produto = '2'.

    "itens
    if it_mseg is not initial.
      loop at it_mseg into wa_mseg
        where docnum = wa_sementes-docnum.

        select single id_matnr_idea  from zsdt0210  into wa_itens_semente_arm-cod_cultivar
          where matnr eq wa_mseg-matnr.

        wa_itens_semente_arm-lote = wa_mseg-charg.

        read table it_zmmt0102 into wa_zmmt0102 with key mblnr  = wa_mseg-mblnr
                                                         mjahr  = wa_mseg-mjahr
                                                         charg  = wa_mseg-charg
                                                         matnr  = wa_mseg-matnr.
        if wa_zmmt0102-categoria is not initial.
          wa_itens_semente_arm-cod_categoria_sementes =  wa_zmmt0102-categoria.
        else.
          clear mensagem.
          verro = abap_true.
*          MENSAGEM = 'O Lote está sem  Categoria da Semente'.
          concatenate 'O Lote' wa_mseg-charg 'está sem  Categoria da Semente, verifique as informações da MIGO.' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        endif.

        if wa_zmmt0102-nr_fase is not initial.
          wa_itens_semente_arm-nr_dar_fase = wa_zmmt0102-nr_fase.
        else.
          clear mensagem.
          verro = abap_true.
*          MENSAGEM = 'O lote está sem o Código Fase'.
          concatenate 'O Lote' wa_mseg-charg 'está sem o Código Fase, verifique as informações da MIGO.' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        endif.

        wa_itens_semente_arm-nr_dar_indea = ' '.

        case  wa_mseg-meins.
          when 'KG'.
            wa_itens_semente_arm-qnt_produto = wa_mseg-menge.
          when 'TO'.
            wa_itens_semente_arm-qnt_produto = wa_mseg-menge * 1000.
          when 'BAG' or 'SAC' or 'BIG'.
            select single *
              from mara into @data(wa_mara)
             where matnr eq @wa_mseg-matnr.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932

            lv_matnr = wa_mseg-matnr(18).

            data: vl_object type ausp-objek.

*      "// Material + Lote
            clear ls_obj.
            ls_obj-matnr = wa_mseg-matnr.
            ls_obj-charg = wa_mseg-charg.
            vl_object = ls_obj.

            call method lo_mm_util->get_caracteristica_geral
              exporting
                i_object               = vl_object
                i_caracteristica       = 'PESO_BAG'
              importing
                e_valor_caracteristica = data(lv_peso_bag).

            wa_itens_semente_arm-qnt_produto = wa_mseg-menge * lv_peso_bag.

*            CALL METHOD LO_MM_UTIL->GET_PESO_BAG(
*              EXPORTING
*                I_MATERIAL = LV_MATNR
*              IMPORTING
*                R_PESO_BAG = LV_PESO_BAG ).

*            wa_itens_semente_arm-qnt_produto = wa_mseg-menge * wa_mara-brgew.

*            WA_ITENS_SEMENTE_ARM-QNT_PRODUTO = LV_PESO_BAG * WA_MARA-BRGEW.
*** Fim - Rubenilson Pereira - 07.04.25 #168932
        endcase.

        append value #(  cod_cultivar              =     wa_itens_semente_arm-cod_cultivar
                         cod_categoria_sementes    =     wa_itens_semente_arm-cod_categoria_sementes
                         nr_dar_indea              =     wa_itens_semente_arm-nr_dar_indea
                         nr_dar_fase               =     wa_itens_semente_arm-nr_dar_fase
                         lote                      =     wa_itens_semente_arm-lote
                         qnt_produto               =     wa_itens_semente_arm-qnt_produto
                      ) to wa_entrada_sementes_arm-itens.

        clear wa_zmmt0102.
        clear wa_j_1bnflin_02.
      endloop.

    else.
      clear mensagem.
      verro = abap_true.
      mensagem = 'Não foi possível localizar os itens da nota (MIGO) para envio ao SISDEV!'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.


    check verro =  abap_false.

    if wzsdt0212-id is not initial.
      wa_entrada_sementes_arm-cod_lancamento_usuario = wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021 using  'ZSEQ_INDEA' '01' changing wa_entrada_sementes_arm-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.
    e_xml = /ui2/cl_json=>serialize( data = wa_entrada_sementes_arm compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                               wa_zauth_ws_0001-username
                               wa_zauth_ws_0001-password
                               tp_metodo   changing resultado.

    translate resultado to upper case.

    if  resultado ca sy-abcde .

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify  zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.

      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_entrada_sementes_arm-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.
*     wa_zsdt0212-status       =  'E'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify  zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.

    elseif  resultado ca '0123456789'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify  zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.


      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_entrada_sementes_arm-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify  zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  endif.

endform.

form z_ws_cancel_ent_arm_sm .

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro = abap_false.

    wa_cancel_entrada_arm_sementes-cnpj_revenda = wa_sementes-cnpj_bupla.
    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service  = wa_zauth_webservice-service
                                                               bukrs    = wa_sementes-bukrs
                                                               branch   = wa_sementes-branch.
    if sy-subrc = 0.
      wa_cancel_entrada_arm_sementes-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.
      exit.
    endif.

    wa_cancel_entrada_arm_sementes-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_entrada_arm_sementes-oigem_armazenador      = 'true'.
    wa_cancel_entrada_arm_sementes-motivo_cancelamento    = 'Dados da Nota estão incorretos'.

    check verro =  abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_entrada_arm_sementes compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo    changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_arm_sementes-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        clear wa_zsdt0212.
        commit work.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_arm_sementes-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        clear wa_zsdt0212.
        commit work.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_sementes-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
    clear wa_zsdt0212.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_busca_set .

  data: l_dias type i.

  "SET DEFENSIVOS
  call function 'G_SET_GET_ALL_VALUES'
    exporting
      setnr         = 'ZSDR021_DT_DF'
      class         = '0000'
    tables
      set_values    = it_value
    exceptions
      set_not_found = 1
      others        = 2.

*-CS2022000910-06.06.2023-#91627-JT-inicio
* CHECK it_value IS NOT INITIAL.

  read table it_value index 1.
  l_dias = cond #( when sy-subrc <> 0 then 0
                                      else it_value-from ).

  clear r_dt_def.
  loop at it_value.
    r_dt_def-sign    = 'I'.
    r_dt_def-option  = 'GE'.
    r_dt_def-low     =  p_docdat-low - l_dias. "it_value-from.
    append r_dt_def.
  endloop.
*-CS2022000910-06.06.2023-#91627-JT-fim

  "SET SEMENTES
  refresh   it_value.
  call function 'G_SET_GET_ALL_VALUES'
    exporting
      setnr         = 'ZSDR021_DT_SM'
      class         = '0000'
    tables
      set_values    = it_value
    exceptions
      set_not_found = 1
      others        = 2.

*-CS2022000910-06.06.2023-#91627-JT-inicio
* CHECK it_value IS NOT INITIAL.

  read table it_value index 1.
  l_dias = cond #( when sy-subrc <> 0 then 0
                                      else it_value-from ).

  clear r_dt_sem.
  loop at it_value.
    r_dt_sem-sign    = 'I'.
    r_dt_sem-option  = 'GE'.
    r_dt_sem-low     = p_docdat-low - l_dias. "it_value-from. it_value-from.
    append r_dt_sem.
  endloop.
*-CS2022000910-06.06.2023-#91627-JT-fim

  refresh  it_value.
  call function 'G_SET_GET_ALL_VALUES'
    exporting
      setnr         = 'ZSDR021_BURKS'
      class         = '0000'
    tables
      set_values    = it_value
    exceptions
      set_not_found = 1
      others        = 2.

  check it_value is not initial.

  clear r_burks.
  loop at it_value.
    r_burks-sign    = 'I'.
    r_burks-option  = 'EQ'.
    r_burks-low     =  it_value-from.
    append r_burks.
  endloop.

  perform z_busca_seq_cfop.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_tratar_dados_defensivos.

*---------------------------------
* ordenar por grupo cfop
*---------------------------------
  loop at it_defensivos into wa_defensivos.
    l_tabix = sy-tabix.

    clear wa_cfop.
    read table it_cfop into wa_cfop with key cfop = wa_defensivos-cfop
                                    binary search.
    if sy-subrc <> 0.
      wa_cfop-grupo = 99.
    endif.

    wa_defensivos-grupo_cfop = wa_cfop-grupo.
    modify it_defensivos from wa_defensivos index l_tabix.
  endloop.

  sort it_defensivos by grupo_cfop.

*---------------------------------
* saida json
*---------------------------------
  loop at it_defensivos into wa_defensivos.

    if wa_defensivos-cancel <> 'X'.

      case  wa_defensivos-nftype.

        when 'NE' or 'YE' or 'YF' or 'ZL' or 'E1'. "Entrada de Agrotoxico 'E1'
          perform z_ws_entrada_df.

        when 'YD' or 'ZA' or 'ZB'.

          if wa_defensivos-nftype = 'ZA'.
            read table it_j_1bnflin into wa_lin with key docnum = wa_defensivos-docnum.
            if wa_lin-itmtyp <> 'ZC'.
              perform z_ws_saida_df. "Saida de Agrotoxico
            else.
              perform z_ws_saida_arm_df. "Saída para Armazenagem de agrotoxico
            endif.
          else.
            perform z_ws_saida_df.
          endif.

        when 'ZD' or 'ZV' or 'N4' . "Devolução da Saida de Agrotoxico
          perform z_ws_devolucao_df.

        when 'ZO' . "Saída para Armazenagem de agrotoxico
          perform z_ws_saida_arm_df.

        when 'ZH'. "Entrada Armazenagem de Agrotoxico
          perform z_ws_entrada_arm_df.

        when 'ZR'. "DEVOLUÇÃO ENTRADA DE AGROTÓXICO
          perform z_ws_devolucao_ent_df.

        when 'ZT'. "SAÍDA -  TRANSFERENCIA ENTRE FILIAL - DE AGROTÓXICO
          perform z_ws_saida_transferencia_df.

        when 'ZU'. "ENTRADA - TRANSFERENCIA FILIAL DE AGROTÓXICO
          perform z_ws_entrada_transferencia_df.

      endcase.

    else.

      case  wa_defensivos-nftype.
        when 'NE' or 'YE' or 'YF' or 'ZL' or 'E1'.  "Cancelamento da entrada de Agrotoxico
          perform z_ws_cancel_df.

        when 'YD' or 'ZA' or 'ZB'.

          if wa_defensivos-nftype = 'ZA'.
            read table it_j_1bnflin into wa_lin with key docnum = wa_defensivos-docnum.

            if wa_lin-itmtyp <> 'ZC'.
              perform z_ws_cancel_saida_df. "Cancelamento da Saida de Agrotoxico
            else.
              perform z_ws_cancel_arm_df. "Cancelamento da Saida Armazenagem de Agrotoxico
            endif.
          else.
            perform z_ws_cancel_saida_df.
          endif.

        when 'ZD' or 'ZV' or 'N4' .   "Cancelamento de Devolução da Saida De Agrotoxico
          perform z_ws_cancel_devol_df.

        when 'ZO' .                  "Cancelamento da Saida Armazenagem de Agrotoxico
          perform z_ws_cancel_arm_df.

        when 'ZH'.                           "Cancelamento da entrada Armazenagem de Agrotoxico
          perform z_ws_cancel_entrada_arm_df.

        when 'ZR'. "CANCELAMENTO  DA DEVOLUÇÃO ENTRADA DE AGROTÓXICO
          perform z_ws_cancel_devolucao_ent_df.

        when 'ZT'. "CANCELAMENTO DA SAÍDA TRANSFERENCIA FILIAL DE AGROTÓXICO
          perform z_ws_cancel_saida_transf_df.

        when 'ZU'. "CANCELAMENTO ENTRADA - TRANSFERENCIA FILIAL DE AGROTÓXICO
          perform z_ws_cancel_entrada_transf_df.

      endcase.
    endif.

    refresh it_quantidade.

    clear: wa_defensivos, wa_entrada_agrotoxico, wa_saida_agrotoxico, wa_devolucao_agrotoxico, wa_cancel_transf_agro, wa_entrada_transf_agro,
           wa_saida_arm_agrotoxico, wa_entrada_arm_agrotoxico, wa_cancel_entrada_agrotoxico, wa_cancel_saida_agrotoxico, wa_cancel_devolucao_ent_agro, wa_cancel_entrada_transf_agro,
           wa_cancel_devolucao_agrotoxico, wa_cancel_saida_arm_agrotoxico, wa_cancel_entrada_arm_sementes, wa_devolucao_ent_agro, wa_transferencia_agrotoxico,
           wa_zsdt0214, wa_zsdt0212, wa_zsdt0210, wa_zauth_webservice, wa_zauth_ws_0001, wzsdt0212, wa_lin, wa_quanidade.

  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATAR_DADOS_SEMENTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_tratar_dados_sementes .

*---------------------------------
* ordenar por grupo cfop
*---------------------------------
  loop at it_sementes   into wa_sementes.
    l_tabix = sy-tabix.

    clear wa_cfop.
    read table it_cfop  into wa_cfop with key cfop = wa_sementes-cfop
                                    binary search.
    if sy-subrc <> 0.
      wa_cfop-grupo = 99.
    endif.

    wa_sementes-grupo_cfop   = wa_cfop-grupo.
    modify it_sementes    from wa_sementes index l_tabix.
  endloop.

  sort it_sementes by grupo_cfop.

*---------------------------------
* saida json
*---------------------------------
  loop at it_sementes into wa_sementes.

    if wa_sementes-cancel <> 'X'.

      case wa_sementes-nftype.
        when 'NE' or 'YE' or 'YF' or 'ZL' or 'E1'. "Entrada de Sementes
          perform z_ws_entrada_sm.

        when 'YD' or 'ZA' or 'ZB'.

          if wa_sementes-nftype = 'ZA'.
            read table it_j_1bnflin_02 into wa_lin_02 with key docnum = wa_sementes-docnum.

            if wa_lin_02-itmtyp <> 'ZC'. "Saida de Sementes
              perform z_ws_saida_sm.
            else.
              perform z_ws_saida_arm_sm. "Saída Armazanagem de Sementes
            endif.
          else.
            perform z_ws_saida_sm.
          endif.

        when 'ZD' or 'ZV' or 'N4' . "Devolução da Saida de Sementes
          perform z_ws_devolucao_sm.

        when 'ZO' . "Saída Armazanagem de Sementes
          perform z_ws_saida_arm_sm.

        when 'ZH'. "Entrada de Armazenagem Sementes
          perform z_ws_entrada_arm_sm.

        when 'ZR'. "  Devolução Entrada de Sementes
          perform z_ws_devolucao_entrada_sm.

        when 'ZT'. " SAÍDA – TRANSFERENCIA FILIAL DE SEMENTES
          perform z_ws_saida_transferencia_sm.

        when 'ZU'. " ENTRADA – TRANSFERENCIA FILIAL DE SEMENTES
          perform z_ws_entrada_transferencia_sm.
      endcase.

    else.

      case wa_sementes-nftype.
        when 'NE' or 'YE' or 'YF' or 'ZL' or 'E1'. "Cancelamento de Entrada de Sementes
          perform z_ws_cancel_sm.

        when 'YD' or 'ZA' or 'ZB'.

          if wa_sementes-nftype = 'ZA'.
            read table it_j_1bnflin_02 into wa_lin_02 with key docnum = wa_sementes-docnum.

            if wa_lin_02-itmtyp <> 'ZC'.
              perform z_ws_cancel_saida_sm. "Cancelamento da Saida de Sementes
            else.
              perform z_ws_cancel_arm_sm. "Cancelamento da Saida Armazenagem de Sementes
            endif.
          else.
            perform z_ws_cancel_saida_sm.
          endif.

        when 'ZD' or 'ZV' or 'N4' . "Cancelamento da Devolução da Saida de Sementes
          perform z_ws_cancel_devol_sm.

        when 'ZO' . "Cancelamento da Saida Armazenagem de Sementes
          perform z_ws_cancel_arm_sm.

        when 'ZH'. "Cancelamento da Entrada de armazenagem de sementes
          perform z_ws_cancel_ent_arm_sm.

        when 'ZR'. "Cancelamento Devolução Entrada de Sementes
          perform z_ws_cancel_devolucao_ent_sm.

        when 'ZT'. "CANCELAMENTO SAÍDA – TRANSFERENCIA FILIAL DE SEMENTES
          perform z_ws_cancel_saida_transf_sm.

        when 'ZU'. "   CANCELAMENTO DA TRANSFERENCIA FILIAL DE SEMENTES
          perform z_ws_cancel_entrada_transf_sm.

      endcase.
    endif.

    clear: wa_sementes, wa_entrada_sementes,  wa_saida_sementes, wa_devolucao_sementes, wa_saida_arm_sementes,  wa_entrada_sementes_arm,
           wa_cancel_entrada_sementes,  wa_cancel_saida_sementes, wa_cancel_devolucao_sementes, wa_saida_arm_sementes, wa_cancel_entrada_arm_sementes, wa_cancel_devolucao_ent_sm,
           wa_cancel_saida_transf_sm, wa_entrada_transferencia_sm, wa_cancel_entrada_transf_sm, wa_saida_transferencia_sm,
           wa_cancel_entrada_arm_sementes, wa_zsdt0214, wa_zsdt0212, wa_zmmt0102, wa_zsdt0210, wa_zauth_webservice, wa_zauth_ws_0001, wa_lin_02, wzsdt0212.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_DEVOLUCAO_ENT_DF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_devolucao_ent_df .

  data wa_item_devolucao_ent_agro type ty_itens_devolucao_ent_agro.
  data lv_unit_out    type t006-msehi.

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro  = abap_false.

    wa_devolucao_ent_agro-cnpj_revenda = wa_defensivos-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service =  wa_zauth_webservice-service
                                                               bukrs   =  wa_defensivos-bukrs
                                                               branch  =  wa_defensivos-branch.
    if sy-subrc = 0.
      wa_devolucao_ent_agro-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro  = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.
      exit.
    endif.

    read table it_lfa1 into wa_lfa1 with key lifnr = wa_defensivos-parid.

    wa_lfa1-name1 = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = conv #( wa_lfa1-name1 ) ) ).

    wa_devolucao_ent_agro-destino_armazenador = 'false'.

    if wa_defensivos-regio = 'MT'.
      wa_devolucao_ent_agro-destino_fora_m_t = 'false'.
      wa_devolucao_ent_agro-nome_destino     = ' '.
      wa_devolucao_ent_agro-cod_municipio    = ' '.
    else.
      wa_devolucao_ent_agro-destino_fora_m_t = 'true'.
      wa_devolucao_ent_agro-nome_destino     = wa_lfa1-name1.
      wa_devolucao_ent_agro-cod_municipio    = wa_lfa1-txjcd+3(12).
    endif.

    wa_devolucao_ent_agro-destino_revenda =  'true'.

    if wa_defensivos-nfenum is not initial.
      wa_devolucao_ent_agro-nr_nf = wa_defensivos-nfenum.
    else.
      wa_devolucao_ent_agro-nr_nf = wa_defensivos-nfnum.
    endif.

    wa_devolucao_ent_agro-serie_nf = wa_defensivos-series.

    concatenate wa_defensivos-docdat+0(4) '-'
                wa_defensivos-docdat+4(2) '-'
                wa_defensivos-docdat+6(2)  into wa_devolucao_ent_agro-data_nf.


    if wa_lfa1-stcd1 is not initial.
      wa_devolucao_ent_agro-cpf_cnpj_destino = wa_lfa1-stcd1.
    else.
      wa_devolucao_ent_agro-cpf_cnpj_destino = wa_lfa1-stcd2.
    endif.
    wa_devolucao_ent_agro-cod_propriedade = ' '.

    "Itens
    loop at  it_mseg into wa_mseg   where docnum = wa_defensivos-docnum.

      read table it_zsdt0210 into wa_zsdt0210 with key matnr = wa_mseg-matnr.

      if sy-subrc = 0.
        wa_item_devolucao_ent_agro-cod_produto = wa_zsdt0210-id_matnr_idea.
      else.
        verro  = abap_true.
        wa_mseg-matnr = |{ wa_mseg-matnr alpha = out }|.

        concatenate 'Material' wa_mseg-matnr 'não realizado o DE/PARA na transação ZSDT0154 ' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.

      wa_item_devolucao_ent_agro-lote                  = wa_mseg-charg.
      wa_item_devolucao_ent_agro-cpf_agronomo          = ' '.
      wa_item_devolucao_ent_agro-nr_art                = ' '.
      wa_item_devolucao_ent_agro-nr_receita            = ' '.
      wa_item_devolucao_ent_agro-cod_cultura           = ' '.
      wa_item_devolucao_ent_agro-cod_praga             = ' '.
      wa_item_devolucao_ent_agro-cod_tipo_aplicacao    = ' '.
      wa_item_devolucao_ent_agro-area_qnt_tratada      = ' '.
      wa_item_devolucao_ent_agro-cod_unidade_medida    = ' '.


      select single *
        from zsdt0201 into @data(wa_zsdt0201)
       where id_produto eq @wa_zsdt0210-id_matnr_idea.

      select single *
        from zsdt0204 into @data(wa_zsdt0204)
       where nome eq @wa_zsdt0201-unidade.

      if sy-subrc = 0.

        if wa_mseg-meins =  wa_zsdt0204-sigla.
          clear:  calcular, v_frac.

          calcular = wa_mseg-menge / wa_zsdt0201-volume.
          v_frac = frac( calcular ).

          if v_frac <> '0'.
            verro  = abap_true.
*            MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
            concatenate 'Quantidade vendida do Material' wa_mseg-matnr 'não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          else.
            wa_item_devolucao_ent_agro-qnt_embalagem =  calcular.
          endif.
        else.
          "PQ
*          IF wa_mseg-meins = 'KG' OR wa_mseg-meins = 'G' OR wa_zsdt0204-sigla = 'KG' OR wa_zsdt0204-sigla = 'G'.
          if wa_mseg-meins = 'KG' or wa_mseg-meins = 'G' and wa_zsdt0204-sigla = 'KG' or wa_zsdt0204-sigla = 'G'.

            lv_unit_out = wa_zsdt0204-sigla.

            call function 'UNIT_CONVERSION_SIMPLE'
              exporting
                input    = wa_mseg-menge
                unit_in  = wa_mseg-meins
                unit_out = lv_unit_out
              importing
                output   = wa_mseg-menge.

*                   CLEAR calcular.
            calcular =  wa_mseg-menge / wa_zsdt0201-volume.

            clear  v_frac.
            v_frac = frac( calcular ).

            if v_frac <> '0'.
              verro = abap_true.
*              MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
              concatenate 'Quantidade vendida do Material' wa_mseg-matnr 'não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              commit work.
              clear wa_zsdt0214.
              exit.
            else.
              wa_item_devolucao_ent_agro-qnt_embalagem =  calcular.
            endif.
            "PQ
          else.
            verro  = abap_true.
*          MENSAGEM = 'Unidade de Material diferente da unidade Material INDEA'.
            concatenate 'A U.M. ' wa_mseg-meins 'do Material' wa_mseg-matnr 'é diferente da U.M.' wa_zsdt0204-sigla 'do Material INDEA.' into mensagem separated by space.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          endif.
        endif.
      else.
        clear: mensagem.
        verro  = abap_true.
*        MENSAGEM  = ' Unidade de Medida do produto autorizado INDEA não encontrada na tabela de Unidade de Medidas do Indea !'.
        concatenate 'A Unidade de Medida' wa_zsdt0201-unidade 'do produto autorizado INDEA, não foi encontrada na tabela de Unidade de Medidas do próprio INDEA' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.


      append value #(     cod_produto         = wa_item_devolucao_ent_agro-cod_produto
                          lote                = wa_item_devolucao_ent_agro-lote
                          cpf_agronomo        = wa_item_devolucao_ent_agro-cpf_agronomo
                          nr_art              = wa_item_devolucao_ent_agro-nr_art
                          nr_receita          = wa_item_devolucao_ent_agro-nr_receita
                          cod_cultura         = wa_item_devolucao_ent_agro-cod_cultura
                          cod_praga           = wa_item_devolucao_ent_agro-cod_praga
                          cod_tipo_aplicacao  = wa_item_devolucao_ent_agro-cod_tipo_aplicacao
                          area_qnt_tratada    = wa_item_devolucao_ent_agro-area_qnt_tratada
                          cod_unidade_medida  = wa_item_devolucao_ent_agro-cod_unidade_medida
                          qnt_embalagem       = wa_item_devolucao_ent_agro-qnt_embalagem
                     ) to wa_devolucao_ent_agro-itens.

    endloop.


* ##################################
*    LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN
*      WHERE DOCNUM = WA_DEFENSIVOS-DOCNUM.
*
*      READ TABLE IT_ZSDT0210 INTO WA_ZSDT0210 WITH KEY MATNR = WA_J_1BNFLIN-MATNR.
*
*      WA_ITEM_DEVOLUCAO_ENT_AGRO-COD_PRODUTO = WA_ZSDT0210-ID_MATNR_IDEA.
*      WA_ITEM_DEVOLUCAO_ENT_AGRO-LOTE        = WA_J_1BNFLIN-CHARG.
*
*      WA_ITEM_DEVOLUCAO_ENT_AGRO-CPF_AGRONOMO          = ' '.
*      WA_ITEM_DEVOLUCAO_ENT_AGRO-NR_ART                = ' '.
*      WA_ITEM_DEVOLUCAO_ENT_AGRO-NR_RECEITA            = ' '.
*      WA_ITEM_DEVOLUCAO_ENT_AGRO-COD_CULTURA           = ' '.
*      WA_ITEM_DEVOLUCAO_ENT_AGRO-COD_PRAGA             = ' '.
*      WA_ITEM_DEVOLUCAO_ENT_AGRO-COD_TIPO_APLICACAO    = ' '.
*      WA_ITEM_DEVOLUCAO_ENT_AGRO-AREA_QNT_TRATADA      = ' '.
*      WA_ITEM_DEVOLUCAO_ENT_AGRO-COD_UNIDADE_MEDIDA    = ' '.
*
*      SELECT SINGLE *
*        FROM ZSDT0201 INTO @DATA(WA_ZSDT0201)
*       WHERE ID_PRODUTO EQ @WA_ZSDT0210-ID_MATNR_IDEA.
*
*      SELECT SINGLE *
*        FROM ZSDT0204 INTO @DATA(WA_ZSDT0204)
*       WHERE NOME EQ @WA_ZSDT0201-UNIDADE.
*
*      IF WA_J_1BNFLIN-MEINS =  WA_ZSDT0204-SIGLA.
*        CLEAR:  CALCULAR, V_FRAC.
*        CALCULAR = WA_J_1BNFLIN-MENGE / WA_ZSDT0201-VOLUME.
*        V_FRAC = FRAC( CALCULAR ).
*
*        IF V_FRAC <> '0'.
*          VERRO  = ABAP_TRUE.
*          MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
*
*          PERFORM GET_NEXT_NUMBER IN PROGRAM ZSDR021 USING  'ZSEQ_L_IND'  '01'  CHANGING  WA_ZSDT0214-ID.
*          WA_ZSDT0214-DATA_ATUAL   = SY-DATUM.
*          WA_ZSDT0214-HORA_ATUAL   = SY-UZEIT.
*          WA_ZSDT0214-DOCNUM       = WA_DEFENSIVOS-DOCNUM.
*          WA_ZSDT0214-ORIGEM       = '1'.
*          WA_ZSDT0214-MENSAGEM     = MENSAGEM.
*          WA_ZSDT0214-USNAM        = SY-UNAME.
*          MODIFY ZSDT0214 FROM WA_ZSDT0214.
*          CLEAR WA_ZSDT0214.
*          COMMIT WORK.
*          EXIT.
*        ELSE.
*          WA_ITEM_DEVOLUCAO_ENT_AGRO-QNT_EMBALAGEM =  CALCULAR.
*        ENDIF.
*      ELSE.
*        VERRO  = ABAP_TRUE.
*        MENSAGEM = 'Unidade de Material diferente da unidade Material INDEA'.
*
*        PERFORM GET_NEXT_NUMBER IN PROGRAM ZSDR021 USING  'ZSEQ_L_IND'  '01'  CHANGING  WA_ZSDT0214-ID.
*        WA_ZSDT0214-DATA_ATUAL   = SY-DATUM.
*        WA_ZSDT0214-HORA_ATUAL   = SY-UZEIT.
*        WA_ZSDT0214-DOCNUM       = WA_DEFENSIVOS-DOCNUM.
*        WA_ZSDT0214-ORIGEM       = '1'.
*        WA_ZSDT0214-MENSAGEM     = MENSAGEM.
*        WA_ZSDT0214-USNAM        = SY-UNAME.
*        MODIFY ZSDT0214 FROM WA_ZSDT0214.
*        CLEAR WA_ZSDT0214.
*        COMMIT WORK.
*        EXIT.
*      ENDIF.
*
*      APPEND VALUE #(     COD_PRODUTO         = WA_ITEM_DEVOLUCAO_ENT_AGRO-COD_PRODUTO
*                          LOTE                = WA_ITEM_DEVOLUCAO_ENT_AGRO-LOTE
*                          CPF_AGRONOMO        = WA_ITEM_DEVOLUCAO_ENT_AGRO-CPF_AGRONOMO
*                          NR_ART              = WA_ITEM_DEVOLUCAO_ENT_AGRO-NR_ART
*                          NR_RECEITA          = WA_ITEM_DEVOLUCAO_ENT_AGRO-NR_RECEITA
*                          COD_CULTURA         = WA_ITEM_DEVOLUCAO_ENT_AGRO-COD_CULTURA
*                          COD_PRAGA           = WA_ITEM_DEVOLUCAO_ENT_AGRO-COD_PRAGA
*                          COD_TIPO_APLICACAO  = WA_ITEM_DEVOLUCAO_ENT_AGRO-COD_TIPO_APLICACAO
*                          AREA_QNT_TRATADA    = WA_ITEM_DEVOLUCAO_ENT_AGRO-AREA_QNT_TRATADA
*                          COD_UNIDADE_MEDIDA  = WA_ITEM_DEVOLUCAO_ENT_AGRO-COD_UNIDADE_MEDIDA
*                          QNT_EMBALAGEM       = WA_ITEM_DEVOLUCAO_ENT_AGRO-QNT_EMBALAGEM
*                     ) TO WA_DEVOLUCAO_ENT_AGRO-ITENS.
*
*    ENDLOOP.
* #################

    check verro = abap_false.

    if  wa_devolucao_ent_agro-itens is initial.

      verro  = abap_true.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = 'Nota com todos itens Adjuvante'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_devolucao_ent_agro-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  'Nota com todos itens Adjuvante'.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
      exit.
    endif.

    check verro  = abap_false.

    if wzsdt0212-id is not initial.
      wa_devolucao_ent_agro-cod_lancamento_usuario =  wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021 using  'ZSEQ_INDEA' '01'
                              changing wa_devolucao_ent_agro-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.
    e_xml = /ui2/cl_json=>serialize( data = wa_devolucao_ent_agro compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                                wa_zauth_ws_0001-username
                                wa_zauth_ws_0001-password
                                tp_metodo   changing resultado.

    translate resultado to upper case.

    if  resultado ca sy-abcde .

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_devolucao_ent_agro-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.

    elseif  resultado ca '0123456789'.
      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.


      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_devolucao_ent_agro-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
    endif.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_CANCEL_DEVOLUCAO_ENT_DF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_cancel_devolucao_ent_df .

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.
  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.
  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro  = abap_false.
    wa_cancel_devolucao_ent_agro-cnpj_revenda =  wa_defensivos-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                               bukrs   = wa_defensivos-bukrs
                                                               branch  = wa_defensivos-branch.
    if sy-subrc = 0.
      wa_cancel_devolucao_ent_agro-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro  = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       =  wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_cancel_devolucao_ent_agro-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_devolucao_ent_agro-motivo_cancelamento    = 'Dados da Nota estão incorretas'.
    wa_cancel_devolucao_ent_agro-destino_armazenador    = 'false'.

    check  verro  = abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_devolucao_ent_agro compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo
                           changing resultado.
      translate resultado to upper case.

      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_devolucao_ent_agro-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_devolucao_ent_agro-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_defensivos-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
  endif.



endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_TRANSFERENCIA_DF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_saida_transferencia_df .

  data wa_itens_transferencia type ty_itens_transfe_agrotoxico.
  data vkunnr type kna1-kunnr.
  data vnumeronf      type zsdt0218-numeronf.
  data vfator         type p decimals 8.
  data cod_praga_aux  type char10.
  data lv_unit_out    type t006-msehi.

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl, vkunnr.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro  = abap_false.

    read table it_j_1bnflin into wa_j_1bnflin with key docnum = wa_defensivos-docnum.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                               bukrs   =  wa_defensivos-bukrs
                                                               branch  =  wa_defensivos-branch.
    if sy-subrc = 0.
      wa_transferencia_agrotoxico-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro  = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.
      exit.
    endif.

    vkunnr = wa_defensivos-parid+4(4).

    vkunnr = |{ vkunnr alpha = in }|.

    select single * from kna1 into @data(wkna1)
      where kunnr eq @vkunnr.


    wkna1-name1 = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = conv #( wkna1-name1 ) ) ).

    wa_transferencia_agrotoxico-cpf_cnpj_destino =  wkna1-stcd1.

    wa_transferencia_agrotoxico-cnpj_revenda =  wa_defensivos-cnpj_bupla.
    wa_transferencia_agrotoxico-destino_armazenador = 'false'.

    if  wa_defensivos-regio = 'MT'.
      wa_transferencia_agrotoxico-destino_fora_m_t  = 'false'.
      wa_transferencia_agrotoxico-nome_destino      = ' '.
      wa_transferencia_agrotoxico-cod_municipio     = ' '.
    else.
      wa_transferencia_agrotoxico-destino_fora_m_t  = 'true'.
      wa_transferencia_agrotoxico-nome_destino      =  wkna1-name1.
      wa_transferencia_agrotoxico-cod_municipio     =  wkna1-txjcd+3(10).
    endif.

    select single * from zsdt0216 into @data(wa_zsdt0216)
      where kunnr           eq @vkunnr
       and  tipo            eq 'C'
       and  setor_atividade eq 'A'.

    if sy-subrc = 0.

      if wa_zsdt0216-revenda         eq 'S'.
        wa_transferencia_agrotoxico-destino_revenda     = 'true'.
      else.
        wa_transferencia_agrotoxico-destino_revenda     = 'false'.
      endif.

    else.
      clear mensagem.
      verro  = abap_true.
      concatenate 'A Filial' vkunnr 'não está cadastrada para Atividade Agrotóxico na transação ZSDT0154.' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.
      exit.
    endif.

    if wa_defensivos-nfenum is not initial.
      wa_transferencia_agrotoxico-nr_nf = wa_defensivos-nfenum.
    else.
      wa_transferencia_agrotoxico-nr_nf = wa_defensivos-nfnum.
    endif.

    wa_transferencia_agrotoxico-serie_nf = wa_defensivos-series.

    concatenate wa_defensivos-docdat+0(4) '-'  wa_defensivos-docdat+4(2) '-' wa_defensivos-docdat+6(2) into wa_transferencia_agrotoxico-data_nf.

    wa_transferencia_agrotoxico-cnpj_ure        = ' '.
    wa_transferencia_agrotoxico-cod_propriedade = ' '.

    clear vnumeronf.

    vnumeronf = wa_transferencia_agrotoxico-nr_nf.
    condense  vnumeronf.

    "***************************************************************
    if wa_transferencia_agrotoxico-destino_revenda   = 'false'.

*-CS2021000218-05.12.2022-#94933-JT-inicio
      free: it_zsdt0218.

      select nro_cg, ch_referencia
        into @data(w_zsdt0001)
        from zsdt0001
          up to 1 rows
       where nro_nf_prod = @wa_defensivos-docnum.
      endselect.

      if sy-subrc = 0.
        select nro_cgd, ch_referencia, id, receitakey
          into table @data(t_zsdt0298)
          from zsdt0298
         where nro_cgd       = @w_zsdt0001-nro_cg
           and ch_referencia = @w_zsdt0001-ch_referencia
           and status        = '4'
           and cancelado     = @abap_false.

        if sy-subrc = 0.
          select *
            from zsdt0218
            into table it_zsdt0218
             for all entries in t_zsdt0298
           where receitakey = t_zsdt0298-receitakey
             and cancelada  = abap_false.
        endif.
      endif.

      if it_zsdt0218[] is initial.
        select *
          from zsdt0218
          into table it_zsdt0218
         where numeronf     eq vnumeronf
           and numeropedido eq wa_defensivos-branch
           and cancelada    ne 'X'.
      endif.
*-CS2021000218-05.12.2022-#94933-JT-fim

      "***************************************************************

*     IF sy-subrc EQ 0.                "*-CS2021000218-05.12.2022-#94933-JT
      if it_zsdt0218[] is not initial. "*-CS2021000218-05.12.2022-#94933-JT
        select * from zsdt0219 into table @data(it_zsdt0219)
          for all entries in @it_zsdt0218
          where numeroreceita  eq @it_zsdt0218-numeroreceita
            and numeropedido   eq @wa_defensivos-branch
            and cpfrt          eq @it_zsdt0218-cpfrt.

        if sy-subrc eq 0.
          loop at it_zsdt0219 assigning field-symbol(<fs_219>).
            call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
              exporting
                input  = <fs_219>-codigomapa
              importing
                output = <fs_219>-codigomapa.
          endloop.
          select * from zsdt0203 into table @data(it_zsdt0203)
             for all entries in @it_zsdt0219
             where id_praga  eq @it_zsdt0219-codigoindeapraga.

        endif.
      else.
***************************************************************
        clear mensagem.
        verro     = abap_true.
        mensagem  = 'Não Localizado no SAP o RA para esta nota fiscal'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.
    endif.


    loop at it_j_1bnflin into wa_j_1bnflin
      where docnum = wa_defensivos-docnum.

      read table it_zsdt0210 into wa_zsdt0210 with key matnr = wa_j_1bnflin-matnr.

      if sy-subrc = 0.
        select single * from zsdt0201 into @data(wt201)
          where id_produto eq @wa_zsdt0210-id_matnr_idea.

        wa_itens_transferencia-cod_produto = wa_zsdt0210-id_matnr_idea.
        wa_itens_transferencia-lote        = wa_j_1bnflin-charg.

      else.
        clear mensagem.
        verro  = abap_true.
        wa_j_1bnflin-matnr = |{ wa_j_1bnflin-matnr alpha = out }|.

        concatenate 'Material' wa_j_1bnflin-matnr 'não realizado o DE/PARA na transação ZSDT0154 ' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.

      if wa_transferencia_agrotoxico-destino_revenda   = 'false'.

        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = wt201-codmapa
          importing
            output = wt201-codmapa.

        read table it_zsdt0219 into data(wa_zsdt0219) with key  codigomapa = wt201-codmapa.

        if sy-subrc = 0.
          clear vfator.

          wa_itens_transferencia-cod_cultura         =  wa_zsdt0219-codigoindeacultura.

          read table it_zsdt0203 into data(wa_zsdt0203) with key  id_praga = wa_zsdt0219-codigoindeapraga.

          if sy-subrc = 0.
            wa_itens_transferencia-cod_praga           =  wa_zsdt0219-codigoindeapraga.
          else.
            clear: mensagem, cod_praga_aux.
            verro     = abap_true.

            cod_praga_aux = wa_zsdt0219-codigoindeapraga.

            concatenate  'Código da Praga:' cod_praga_aux 'do RA, não encontrado na base do INDEA. Item' wa_j_1bnflin-itmnum 'da NF'  into mensagem  separated by space .

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          endif.

          if wa_zsdt0219-codigoindeatipoaplicacao eq 0.

            clear mensagem.
            verro     = abap_true.
            concatenate  'Receituário Nr:' wa_zsdt0219-numeroreceita 'está sem o código Tipo Aplicação' into mensagem  separated by space .

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          else.
            wa_itens_transferencia-cod_tipo_aplicacao  =   wa_zsdt0219-codigoindeatipoaplicacao.
          endif.

          if wa_zsdt0219-codigoindeatipoaplicacao <> '4'.
            if wa_zsdt0219-area eq 0. "Verificar se a quantidade de semente esta zerada. Ajuste 30/03/2021 IR026498 - AO
              clear mensagem.
              verro     = abap_true.
              concatenate  'Receituário Nr:' wa_zsdt0219-numeroreceita 'está sem a quantidade da area de aplicação' into mensagem  separated by space .

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            else.

              wa_itens_transferencia-cod_unidade_medida = '72'.
              vfator = ( wa_zsdt0219-area /  wa_zsdt0219-quantidade ).
              wa_itens_transferencia-area_qnt_tratada  = ( vfator * wa_j_1bnflin-menge ).
            endif.
          else.

            if wa_zsdt0219-qtdsementes eq 0. "Verificar se a quantidade de semente esta zerada. Ajuste 30/03/2021 IR026498 - AO
              clear mensagem.
              verro     = abap_true.
              concatenate  'Receituário Nr:' wa_zsdt0219-numeroreceita 'está sem a quantidade de semente tratada' into mensagem  separated by space .

              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            else.

              wa_itens_transferencia-cod_unidade_medida = '107'.
              wa_itens_transferencia-area_qnt_tratada  = wa_zsdt0219-qtdsementes.
            endif.
          endif.


        else.
**** IR024542
*          CLEAR MENSAGEM.
*          VERRO     = ABAP_TRUE.
*          CONCATENATE  'Não encontrado a receita para o item ' WA_J_1BNFLIN-ITMNUM  INTO MENSAGEM  SEPARATED BY SPACE .
*
*          PERFORM GET_NEXT_NUMBER IN PROGRAM ZSDR021 USING  'ZSEQ_L_IND'  '01'  CHANGING  WA_ZSDT0214-ID.
*          WA_ZSDT0214-DATA_ATUAL   = SY-DATUM.
*          WA_ZSDT0214-HORA_ATUAL   = SY-UZEIT.
*          WA_ZSDT0214-DOCNUM       = WA_DEFENSIVOS-DOCNUM.
*          WA_ZSDT0214-ORIGEM       = '1'.
*          WA_ZSDT0214-MENSAGEM     = MENSAGEM.
*          WA_ZSDT0214-USNAM        = SY-UNAME.
*          MODIFY ZSDT0214 FROM WA_ZSDT0214.
*          CLEAR WA_ZSDT0214.
*          COMMIT WORK.
*          EXIT.
          loop at it_zsdt0219 into wa_zsdt0219.
            if vcod_mapa_receita is initial.
              vcod_mapa_receita = |{ wa_zsdt0219-codigomapa }|.
            else.
              vcod_mapa_receita = |{ vcod_mapa_receita } / { wa_zsdt0219-codigomapa }|.
            endif.
          endloop.

          clear mensagem.
          verro     = abap_true.
          concatenate  'Código Mapa (' wt201-codmapa ') informado no DE/PARA, difere dos códigos (' vcod_mapa_receita ') do R.A.' into mensagem  separated by space .

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          clear: wa_zsdt0214, vcod_mapa_receita.
          commit work.
          exit.
****
        endif.

        read table  it_zsdt0218 into data(wa_zsdt0218) with key numeroreceita = wa_zsdt0219-numeroreceita.

        if sy-subrc = 0.
          wa_itens_transferencia-cpf_agronomo   =  wa_zsdt0218-cpfrt.
          wa_itens_transferencia-nr_art         =  wa_zsdt0218-numeroart.
          wa_itens_transferencia-nr_receita     =  wa_zsdt0218-numeroreceita.
*        ELSE.
*          CLEAR MENSAGEM.
*          VERRO     = ABAP_TRUE.
*          MENSAGEM  = 'Nota Fiscal sem Receituario Agronômico'.
*
*          PERFORM GET_NEXT_NUMBER IN PROGRAM ZSDR021 USING  'ZSEQ_L_IND'  '01'  CHANGING  WA_ZSDT0214-ID.
*          WA_ZSDT0214-DATA_ATUAL   = SY-DATUM.
*          WA_ZSDT0214-HORA_ATUAL   = SY-UZEIT.
*          WA_ZSDT0214-DOCNUM       = WA_DEFENSIVOS-DOCNUM.
*          WA_ZSDT0214-ORIGEM       = '1'.
*          WA_ZSDT0214-MENSAGEM     = MENSAGEM.
*          WA_ZSDT0214-USNAM        = SY-UNAME.
*          MODIFY ZSDT0214 FROM WA_ZSDT0214.
*          CLEAR WA_ZSDT0214.
*          COMMIT WORK.
*          EXIT.
        endif.

        wa_quanidade-codigomapa = wt201-codmapa.
        wa_quanidade-quantidade = wa_j_1bnflin-menge.
        collect wa_quanidade into it_quantidade.

      else.

        wa_itens_transferencia-cpf_agronomo          = ' '.
        wa_itens_transferencia-nr_art                = ' '.
        wa_itens_transferencia-nr_receita            = ' '.
        wa_itens_transferencia-cod_cultura           = ' '.
        wa_itens_transferencia-cod_praga             = ' '.
        wa_itens_transferencia-cod_tipo_aplicacao    = ' '.
        wa_itens_transferencia-area_qnt_tratada      = ' '.
        wa_itens_transferencia-cod_unidade_medida    = ' '.

      endif.


      select single *
        from zsdt0201 into @data(wa_zsdt0201)
       where id_produto eq @wa_zsdt0210-id_matnr_idea.

      select single *
        from zsdt0204 into @data(wa_zsdt0204)
       where nome eq @wa_zsdt0201-unidade.

      if sy-subrc = 0.

        if wa_j_1bnflin-meins =  wa_zsdt0204-sigla.
          clear:  calcular, v_frac.
          calcular = wa_j_1bnflin-menge / wa_zsdt0201-volume.
          v_frac = frac( calcular ).

          if v_frac <> '0'.
            verro  = abap_true.
*            MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
            concatenate 'Quantidade vendida do Material' wa_j_1bnflin-matnr ', Item' wa_j_1bnflin-itmnum 'da NFe, não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          else.
            wa_itens_transferencia-qnt_embalagem =  calcular.
          endif.
        else.
          "PQ
*          IF wa_j_1bnflin-meins = 'KG' OR wa_j_1bnflin-meins = 'G' OR wa_zsdt0204-sigla = 'KG' OR wa_zsdt0204-sigla = 'G'.
          if wa_j_1bnflin-meins = 'KG' or wa_j_1bnflin-meins = 'G' and wa_zsdt0204-sigla = 'KG' or wa_zsdt0204-sigla = 'G'.

            lv_unit_out = wa_zsdt0204-sigla.

            call function 'UNIT_CONVERSION_SIMPLE'
              exporting
                input    = wa_j_1bnflin-menge
                unit_in  = wa_j_1bnflin-meins
                unit_out = lv_unit_out
              importing
                output   = wa_j_1bnflin-menge.

*                   CLEAR calcular.
            calcular = wa_j_1bnflin-menge / wa_zsdt0201-volume.
            clear  v_frac.
            v_frac = frac( calcular ).

            if v_frac <> '0'.
              verro  = abap_true.
*              MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
              concatenate 'Quantidade vendida do Material' wa_j_1bnflin-matnr ', Item' wa_j_1bnflin-itmnum 'da NFe, não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.
              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            else.
              wa_itens_transferencia-qnt_embalagem =  calcular.
            endif.
            "PQ
          else.

            verro  = abap_true.
*          MENSAGEM = 'Unidade de Material diferente da unidade Material INDEA'.
            concatenate 'A U.M. ' wa_j_1bnflin-meins 'do Material' wa_j_1bnflin-matnr 'é diferente da U.M.' wa_zsdt0204-sigla 'do Material INDEA.' into mensagem separated by space.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          endif.
        endif.
      else.
        clear: mensagem.
        verro  = abap_true.
*        MENSAGEM  = ' Unidade de Medida do produto autorizado INDEA não encontrada na tabela de Unidade de Medidas do Indea !'.
        concatenate 'A Unidade de Medida' wa_zsdt0201-unidade 'do produto autorizado INDEA, não foi encontrada na tabela de Unidade de Medidas do próprio INDEA' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.

      append value #(     cod_produto         = wa_itens_transferencia-cod_produto
                          lote                = wa_itens_transferencia-lote
                          cpf_agronomo        = wa_itens_transferencia-cpf_agronomo
                          nr_art              = wa_itens_transferencia-nr_art
                          nr_receita          = wa_itens_transferencia-nr_receita
                          cod_cultura         = wa_itens_transferencia-cod_cultura
                          cod_praga           = wa_itens_transferencia-cod_praga
                          cod_tipo_aplicacao  = wa_itens_transferencia-cod_tipo_aplicacao
                          area_qnt_tratada    = wa_itens_transferencia-area_qnt_tratada
                          cod_unidade_medida  = wa_itens_transferencia-cod_unidade_medida
                          qnt_embalagem       = wa_itens_transferencia-qnt_embalagem
                     ) to wa_transferencia_agrotoxico-itens.

      clear: wa_j_1bnflin, wa_quanidade.
    endloop.


    check verro  = abap_false.

    if  wa_transferencia_agrotoxico-itens is initial.

      verro  = abap_true.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = 'Nota com todos itens Adjuvante'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_transferencia_agrotoxico-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  'Nota com todos itens Adjuvante'.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
      exit.
    endif.

    if wa_transferencia_agrotoxico-destino_revenda   = 'false'.
      loop at it_zsdt0219 into wa_zsdt0219.
        read table it_quantidade into wa_quanidade with key codigomapa = wa_zsdt0219-codigomapa.
        if sy-subrc = 0.

          if wa_quanidade-quantidade <> wa_zsdt0219-quantidade.
            verro  = abap_true.
            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            concatenate 'Quantidade da Receita diferente dos itens da Nota fical. Código Mapa: ' wa_zsdt0219-codigomapa into wa_zsdt0214-mensagem separated by space.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          endif.
        endif.
      endloop.
    endif.

    check  verro  = abap_false.


    if wzsdt0212-id is not initial.
      wa_transferencia_agrotoxico-cod_lancamento_usuario =  wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021 using  'ZSEQ_INDEA' '01'
                              changing wa_transferencia_agrotoxico-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.
    e_xml = /ui2/cl_json=>serialize( data = wa_transferencia_agrotoxico compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                                wa_zauth_ws_0001-username
                                wa_zauth_ws_0001-password
                                tp_metodo   changing resultado.

    translate resultado to upper case.

    if  resultado ca sy-abcde .

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_transferencia_agrotoxico-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.
*     wa_zsdt0212-status       =  'E'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.

    elseif  resultado ca '0123456789'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_transferencia_agrotoxico-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
    endif.
  endif.

  clear: wa_zsdt0216.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_CANCEL_TRANSFERENCIA_DF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_cancel_saida_transf_df .

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.
  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.
  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro  = abap_false.
    wa_cancel_transf_agro-cnpj_revenda =  wa_defensivos-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                               bukrs   = wa_defensivos-bukrs
                                                               branch  = wa_defensivos-branch.
    if sy-subrc = 0.
      wa_cancel_transf_agro-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro  = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       =  wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_cancel_transf_agro-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_transf_agro-motivo_cancelamento    = 'Dados da Nota estão incorretas'.
    wa_cancel_transf_agro-destino_armazenador    = 'true'.

    check  verro  = abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_transf_agro compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo
                           changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_transf_agro-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_transf_agro-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_defensivos-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_ENTRADA_TRANSFERENCIA_DF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_entrada_transferencia_df .

  data wa_item_entrada_transf_agro   type  ty_item_entrada_transf_agro.
  data vlifnr type lfa1-lifnr.
  data lv_unit_out    type t006-msehi.

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl, vlifnr .

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro  = abap_false.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                               bukrs   = wa_defensivos-bukrs
                                                               branch  = wa_defensivos-branch.
    if sy-subrc = 0.
      wa_entrada_transf_agro-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      verro  = abap_true.
      clear mensagem.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.
      exit.
    endif.

    wa_entrada_transf_agro-cnpj_revenda = wa_defensivos-cnpj_bupla.
    wa_entrada_transf_agro-origem_armazenador = 'false'.

    if wa_defensivos-nfenum is not initial.
      wa_entrada_transf_agro-nr_nf = wa_defensivos-nfenum.
    else.
      wa_entrada_transf_agro-nr_nf = wa_defensivos-nfnum.
    endif.

    if wa_defensivos-series is not initial.
      wa_entrada_transf_agro-serie_nf = wa_defensivos-series.
    else.
      clear mensagem.
      verro = abap_true.
      mensagem = 'A Série da nota Fiscal não está preenchido!'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    concatenate wa_defensivos-docdat+0(4) '-'  wa_defensivos-docdat+4(2) '-' wa_defensivos-docdat+6(2) into wa_entrada_transf_agro-dt_nf.
    concatenate wa_defensivos-pstdat+0(4) '-'  wa_defensivos-pstdat+4(2) '-' wa_defensivos-pstdat+6(2) into wa_entrada_transf_agro-dt_entrada.

    vlifnr =  wa_defensivos-parid+4(4).
    vlifnr = |{ vlifnr alpha = in }|.

    select single * from lfa1 into @data(wlfa1)
      where lifnr eq @vlifnr.

    if wlfa1-stcd1 is not initial.
      wa_entrada_transf_agro-cnpj_fornecedor =  wlfa1-stcd1.
    else.
      wa_entrada_transf_agro-cnpj_fornecedor =  wlfa1-stcd2.
    endif.

    loop at it_j_1bnflin into wa_j_1bnflin
      where docnum = wa_defensivos-docnum.

      read table it_zsdt0210 into wa_zsdt0210 with key matnr = wa_j_1bnflin-matnr.
      if sy-subrc = 0.
        wa_item_entrada_transf_agro-cod_produto = wa_zsdt0210-id_matnr_idea.

      else.
        clear mensagem.
        verro  = abap_true.
        wa_j_1bnflin-matnr = |{ wa_j_1bnflin-matnr alpha = out }|.

        concatenate 'Material' wa_j_1bnflin-matnr 'não realizado o DE/PARA na transação ZSDT0154 ' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.

      wa_item_entrada_transf_agro-lote = wa_j_1bnflin-charg.

      select single *
        from zsdt0201 into @data(wa_zsdt0201)
       where id_produto eq @wa_zsdt0210-id_matnr_idea.

      select single *
        from zsdt0204 into @data(wa_zsdt0204)
       where nome eq @wa_zsdt0201-unidade.

      if sy-subrc = 0.

        if wa_j_1bnflin-meins =  wa_zsdt0204-sigla.
          clear:  calcular, v_frac.
          calcular = wa_j_1bnflin-menge / wa_zsdt0201-volume.
          v_frac = frac( calcular ).

          if v_frac <> '0'.
            verro  = abap_true.
*            MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
            concatenate 'Quantidade vendida do Material' wa_j_1bnflin-matnr ', Item' wa_j_1bnflin-itmnum 'da NFe, não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          else.
            wa_item_entrada_transf_agro-qnt_produto =  calcular.
          endif.
        else.
          "PQ
*          IF wa_j_1bnflin-meins = 'KG' OR wa_j_1bnflin-meins = 'G' OR wa_zsdt0204-sigla = 'KG' OR wa_zsdt0204-sigla = 'G'.
          if wa_j_1bnflin-meins = 'KG' or wa_j_1bnflin-meins = 'G' and wa_zsdt0204-sigla = 'KG' or wa_zsdt0204-sigla = 'G'.

            lv_unit_out = wa_zsdt0204-sigla.

            call function 'UNIT_CONVERSION_SIMPLE'
              exporting
                input    = wa_j_1bnflin-menge
                unit_in  = wa_j_1bnflin-meins
                unit_out = lv_unit_out
              importing
                output   = wa_j_1bnflin-menge.

*                   CLEAR calcular.
            calcular = wa_j_1bnflin-menge / wa_zsdt0201-volume.
            clear  v_frac.
            v_frac = frac( calcular ).

            if v_frac <> '0'.
              verro  = abap_true.
*              MENSAGEM = 'Quantidade Vendida não condiz com a base de Volume do Material INDEA'.
              concatenate 'Quantidade vendida do Material' wa_j_1bnflin-matnr ', Item' wa_j_1bnflin-itmnum 'da NFe, não condiz com a base de Volume do Material INDEA.' into mensagem separated by space.
              perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
              wa_zsdt0214-data_atual   = sy-datum.
              wa_zsdt0214-hora_atual   = sy-uzeit.
              wa_zsdt0214-docnum       = wa_defensivos-docnum.
              wa_zsdt0214-origem       = '1'.
              wa_zsdt0214-mensagem     = mensagem.
              wa_zsdt0214-usnam        = sy-uname.
              modify zsdt0214 from wa_zsdt0214.
              clear wa_zsdt0214.
              commit work.
              exit.
            else.
              wa_item_entrada_transf_agro-qnt_produto =  calcular.
            endif.
            "PQ




          else.

            verro  = abap_true.
*          MENSAGEM = 'Unidade de Material diferente da unidade Material INDEA'.
            concatenate 'A U.M. ' wa_j_1bnflin-meins 'do Material' wa_j_1bnflin-matnr 'é diferente da U.M.' wa_zsdt0204-sigla 'do Material INDEA.' into mensagem separated by space.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_defensivos-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            clear wa_zsdt0214.
            commit work.
            exit.
          endif.
        endif.
      else.
        clear: mensagem.
        verro  = abap_true.
*        MENSAGEM  = ' Unidade de Medida do produto autorizado INDEA não encontrada na tabela de Unidade de Medidas do Indea !'.
        concatenate 'A Unidade de Medida' wa_zsdt0201-unidade 'do produto autorizado INDEA, não foi encontrada na tabela de Unidade de Medidas do próprio INDEA' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      endif.


      append value #(     cod_produto    = wa_item_entrada_transf_agro-cod_produto
                          lote           = wa_item_entrada_transf_agro-lote
                          qnt_produto    = wa_item_entrada_transf_agro-qnt_produto
                     ) to wa_entrada_transf_agro-itens.

    endloop.

    check verro  = abap_false.

    if  wa_entrada_transf_agro-itens is initial.

      verro  = abap_true.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = 'Nota com todos itens Adjuvante'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_entrada_transf_agro-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  'Nota com todos itens Adjuvante'.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
      exit.
    endif.

    check  verro  = abap_false.

    if wzsdt0212-id is not initial.
      wa_entrada_transf_agro-cod_lancamento_usuario =  wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021 using  'ZSEQ_INDEA' '01'
                              changing wa_entrada_transf_agro-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.
    e_xml = /ui2/cl_json=>serialize( data = wa_entrada_transf_agro compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                                wa_zauth_ws_0001-username
                                wa_zauth_ws_0001-password
                                tp_metodo   changing resultado.
    translate resultado to upper case.


    if  resultado ca sy-abcde .

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.

      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_entrada_transf_agro-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.
*     wa_zsdt0212-status       =  'E'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.

    elseif  resultado ca '0123456789'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_defensivos-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.


      wa_zsdt0212-docnum       =  wa_defensivos-docnum.
      wa_zsdt0212-id           =  wa_entrada_transf_agro-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      clear wa_zsdt0212.
      commit work.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_CANCEL_ENTRADA_TRANSF_DF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_cancel_entrada_transf_df .


  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.
  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_defensivos-docnum.
  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro  = abap_false.
    wa_cancel_entrada_transf_agro-cnpj_revenda =  wa_defensivos-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_AGROTOXICO'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                               bukrs   = wa_defensivos-bukrs
                                                               branch  = wa_defensivos-branch.
    if sy-subrc = 0.
      wa_cancel_entrada_transf_agro-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro  = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_defensivos-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       =  wa_defensivos-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_cancel_entrada_transf_agro-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_entrada_transf_agro-motivo_cancelamento    = 'Dados da Nota estão incorretas'.
    wa_cancel_entrada_transf_agro-origem_armazenador     = 'true'.

    check  verro  = abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_entrada_transf_agro compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo
                           changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_transf_agro-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_defensivos-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_defensivos-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_transf_agro-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_defensivos-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_DEVOLUCAO_ENTRADA_SM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_devolucao_entrada_sm.

  data: wa_item_devolucao_entrada_sm type ty_item_devolucao_entrada_sm,
*** Inicio - Rubenilson Pereira - 07.04.25 #168932
        lo_mm_util                   type ref to zcl_mm_util,
        lv_matnr                     type matnr18,
        lv_renasem                   type zsdt0216-renasem.

  create object lo_mm_util.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro = abap_false.

    read table it_j_1bnflin_02 into wa_j_1bnflin_02 with key docnum = wa_sementes-docnum.

    wa_devolucao_entrada_sm-cnpj_revenda = wa_sementes-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service =  wa_zauth_webservice-service
                                                               bukrs   =  wa_sementes-bukrs
                                                               branch  =  wa_sementes-branch.
    if sy-subrc = 0.
      wa_devolucao_entrada_sm-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_devolucao_entrada_sm-destino_armazenador = 'false'.
    wa_devolucao_entrada_sm-destino_revenda      = 'true'.

    if wa_sementes-regio = 'MT'.
      wa_devolucao_entrada_sm-destino_fora_m_t = 'false'.
    else.
      wa_devolucao_entrada_sm-destino_fora_m_t =  'true'.
    endif.

    if wa_sementes-nfenum is not initial.
      wa_devolucao_entrada_sm-nr_nf = wa_sementes-nfenum.
    else.
      wa_devolucao_entrada_sm-nr_nf = wa_sementes-nfnum.
    endif.
    wa_devolucao_entrada_sm-serie_nf = wa_sementes-series.

    concatenate  wa_sementes-docdat+0(4) '-' wa_sementes-docdat+4(2) '-'  wa_sementes-docdat+6(2) into  wa_devolucao_entrada_sm-dt_nf.


*** Inicio - Rubenilson Pereira - 07.04.25 #168932
    lv_matnr = wa_j_1bnflin_02-matnr(18).

    call method lo_mm_util->get_renasem(
      exporting
        i_material = lv_matnr
      importing
        r_renasem  = lv_renasem ).

    if lv_renasem is not initial.
      wa_devolucao_entrada_sm-renasem_destino = lv_renasem.
    else.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

      select single * from zsdt0216 into @data(wa_zsdt0216)
        where kunnr   eq @wa_sementes-parid
        and   tipo    eq 'F'
        and   setor_atividade eq 'S'.

      if sy-subrc = 0.
        if wa_zsdt0216-renasem is initial.
          clear mensagem.
          verro = abap_true.
          concatenate 'O Fornecedor' wa_sementes-parid 'está sem a informação do Renasem na Transação ZSDT0154.' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        else.
          wa_devolucao_entrada_sm-renasem_destino = wa_zsdt0216-renasem.
        endif.
      else.
        clear mensagem.
        verro = abap_true.
        concatenate 'O Fornecedor' wa_sementes-parid 'não está cadastrado na Tranação ZSDT0154.' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.
        exit.
      endif.
    endif.

    read table it_lfa1 into wa_lfa1 with key lifnr = wa_sementes-parid.

    wa_lfa1-name1 = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = conv #( wa_lfa1-name1 ) ) ).


    wa_devolucao_entrada_sm-nome_destino = wa_lfa1-name1.
    wa_devolucao_entrada_sm-cod_municipio_destino = wa_lfa1-txjcd+3(10).

    if wa_lfa1-stcd1 is not initial.
      wa_devolucao_entrada_sm-cpf_cnpj_destino = wa_lfa1-stcd1.
    else.
      wa_devolucao_entrada_sm-cpf_cnpj_destino = wa_lfa1-stcd2.
    endif.
    wa_devolucao_entrada_sm-cod_tipo_produto = 2.

    "Itens
    if it_mseg is not initial.
      loop at it_mseg into wa_mseg  where docnum = wa_sementes-docnum.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
        lv_matnr = wa_mseg-matnr(18).

        call method lo_mm_util->get_codigo_indea(
          exporting
            i_material = lv_matnr
          importing
            id_indea   = wa_item_devolucao_entrada_sm-cod_cultivar ).

        if wa_item_devolucao_entrada_sm-cod_cultivar is initial.
          read table it_zsdt0210 into wa_zsdt0210 with key matnr = wa_mseg-matnr.
          if sy-subrc is initial.
            wa_item_devolucao_entrada_sm-cod_cultivar =  wa_zsdt0210-id_matnr_idea.
          else.

            clear mensagem.
            verro = abap_true.
            mensagem = 'De/Para entre Material SAP x Cod. Cultivar Indea não realizado!'.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_sementes-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            commit work.
            clear wa_zsdt0214.
            exit.
          endif.
        endif.
*** FIM - RUBENILSON PEREIRA - 07.04.25 #168932

        wa_item_devolucao_entrada_sm-lote         = wa_mseg-charg.
        wa_item_devolucao_entrada_sm-nr_dar_indea = ' '.

        read table it_zmmt0102 into wa_zmmt0102 with key charg  = wa_mseg-charg
                                                         matnr  = wa_mseg-matnr
                                                         docnum = wa_mseg-docnum.

        if wa_zmmt0102-nr_fase is not initial.
          wa_item_devolucao_entrada_sm-nr_dar_fase = wa_zmmt0102-nr_fase.
        else.
          clear mensagem.
          verro = abap_true.
*          MENSAGEM = 'O lote está sem o Código Fase'.
          concatenate 'O Lote' wa_mseg-charg 'está sem o Código Fase.' into mensagem separated by space.


          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        endif.


        case wa_mseg-meins.
          when 'KG'.
            wa_item_devolucao_entrada_sm-qnt_produto = wa_mseg-menge.
          when 'TO'.
            wa_item_devolucao_entrada_sm-qnt_produto = wa_mseg-menge * 1000.
          when 'BAG' or 'SAC' or 'BIG'.
            select single * from mara into @data(wa_mara) where matnr eq @wa_mseg-matnr.

            data: vl_object type ausp-objek.

*      "// Material + Lote
            clear: ls_obj, vl_object.
            ls_obj-matnr = wa_mseg-matnr.
            ls_obj-charg = wa_mseg-charg.
            vl_object = ls_obj.

            call method lo_mm_util->get_caracteristica_geral
              exporting
                i_object               = vl_object
                i_caracteristica       = 'PESO_BAG'
              importing
                e_valor_caracteristica = data(lv_peso_bag).

            wa_item_devolucao_entrada_sm-qnt_produto = wa_mseg-menge * lv_peso_bag.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
*            CALL METHOD LO_MM_UTIL->GET_PESO_BAG(
*              EXPORTING
*                I_MATERIAL = LV_MATNR
*              IMPORTING
*                R_PESO_BAG = LV_PESO_BAG ).

*            wa_item_devolucao_entrada_sm-qnt_produto = wa_mseg-menge * wa_mara-brgew.
*            WA_ITEM_DEVOLUCAO_ENTRADA_SM-QNT_PRODUTO = LV_PESO_BAG * WA_MARA-BRGEW.
*** Fim - Rubenilson Pereira - 07.04.25 #168932
        endcase.

        append value #( cod_cultivar   =  wa_item_devolucao_entrada_sm-cod_cultivar
                        lote           =  wa_item_devolucao_entrada_sm-lote
                        nr_dar_indea   =  wa_item_devolucao_entrada_sm-nr_dar_indea
                        nr_dar_fase    =  wa_item_devolucao_entrada_sm-nr_dar_fase
                        qnt_produto    =  wa_item_devolucao_entrada_sm-qnt_produto
                       )   to wa_devolucao_entrada_sm-itens.
      endloop.
    else.
      clear mensagem.
      verro = abap_true.
      mensagem = 'Não foi possível localizar os itens da nota (MIGO) para envio ao SISDEV!'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

* #############################
*    LOOP AT IT_J_1BNFLIN_02 INTO WA_J_1BNFLIN_02
*      WHERE DOCNUM = WA_SEMENTES-DOCNUM.
*
*      READ TABLE IT_ZSDT0210 INTO WA_ZSDT0210 WITH KEY MATNR = WA_J_1BNFLIN_02-MATNR.
*      IF SY-SUBRC = 0.
*        WA_ITEM_DEVOLUCAO_ENTRADA_SM-COD_CULTIVAR =  WA_ZSDT0210-ID_MATNR_IDEA.
*      ENDIF.
*
*      WA_ITEM_DEVOLUCAO_ENTRADA_SM-LOTE         = WA_J_1BNFLIN_02-CHARG.
*      WA_ITEM_DEVOLUCAO_ENTRADA_SM-NR_DAR_INDEA = ' '.
*
*      READ TABLE IT_ZMMT0102 INTO WA_ZMMT0102 WITH KEY DOCNUM = WA_J_1BNFLIN_02-DOCNUM.
*
*      IF WA_ZMMT0102-NR_FASE IS NOT INITIAL.
*        WA_ITEM_DEVOLUCAO_ENTRADA_SM-NR_DAR_FASE = WA_ZMMT0102-NR_FASE.
*      ELSE.
*        CLEAR MENSAGEM.
*        VERRO = ABAP_TRUE.
*        MENSAGEM = 'O lote está sem o Código Fase'.
*
*        PERFORM GET_NEXT_NUMBER IN PROGRAM ZSDR021 USING  'ZSEQ_L_IND'  '01'  CHANGING  WA_ZSDT0214-ID.
*        WA_ZSDT0214-DATA_ATUAL   = SY-DATUM.
*        WA_ZSDT0214-HORA_ATUAL   = SY-UZEIT.
*        WA_ZSDT0214-DOCNUM       = WA_SEMENTES-DOCNUM.
*        WA_ZSDT0214-ORIGEM       = '1'.
*        WA_ZSDT0214-MENSAGEM     = MENSAGEM.
*        WA_ZSDT0214-USNAM        = SY-UNAME.
*        MODIFY ZSDT0214 FROM WA_ZSDT0214.
*        COMMIT WORK.
*        CLEAR WA_ZSDT0214.
*        EXIT.
*      ENDIF.
*
*
*      CASE WA_J_1BNFLIN_02-MEINS.
*        WHEN 'KG'.
*          WA_ITEM_DEVOLUCAO_ENTRADA_SM-QNT_PRODUTO = WA_J_1BNFLIN_02-MENGE.
*        WHEN 'TO'.
*          WA_ITEM_DEVOLUCAO_ENTRADA_SM-QNT_PRODUTO = WA_J_1BNFLIN_02-MENGE * 1000.
*        WHEN 'BAG' OR 'SAC'.
*          SELECT SINGLE * FROM MARA INTO @DATA(WA_MARA)
*            WHERE MATNR EQ @WA_J_1BNFLIN_02-MATNR.
*
*          WA_ITEM_DEVOLUCAO_ENTRADA_SM-QNT_PRODUTO = WA_J_1BNFLIN_02-MENGE * WA_MARA-BRGEW.
*      ENDCASE.
*
*
*      APPEND VALUE #( COD_CULTIVAR   =  WA_ITEM_DEVOLUCAO_ENTRADA_SM-COD_CULTIVAR
*                      LOTE           =  WA_ITEM_DEVOLUCAO_ENTRADA_SM-LOTE
*                      NR_DAR_INDEA   =  WA_ITEM_DEVOLUCAO_ENTRADA_SM-NR_DAR_INDEA
*                      NR_DAR_FASE    =  WA_ITEM_DEVOLUCAO_ENTRADA_SM-NR_DAR_FASE
*                      QNT_PRODUTO    =  WA_ITEM_DEVOLUCAO_ENTRADA_SM-QNT_PRODUTO
*                     )   TO WA_DEVOLUCAO_ENTRADA_SM-ITENS.
*    ENDLOOP.
* #############################

    check verro = abap_false.

    if wzsdt0212-id is not initial.
      wa_devolucao_entrada_sm-cod_lancamento_usuario =  wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021
                              using  'ZSEQ_INDEA' '01' changing wa_devolucao_entrada_sm-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.
    e_xml = /ui2/cl_json=>serialize( data = wa_devolucao_entrada_sm compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                               wa_zauth_ws_0001-username
                               wa_zauth_ws_0001-password
                               tp_metodo  changing resultado.

    translate resultado to upper case.

    if  resultado ca sy-abcde .

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.

      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_devolucao_entrada_sm-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.
*     wa_zsdt0212-status       =  'E'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.

    elseif  resultado ca '0123456789'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.

      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_devolucao_entrada_sm-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_CANCEL_DEVOLUCAO_ENT_SM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_cancel_devolucao_ent_sm .

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro = abap_false.

    wa_cancel_devolucao_ent_sm-cnpj_revenda = wa_sementes-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key  service = wa_zauth_webservice-service
                                                                bukrs   = wa_sementes-bukrs
                                                                branch  = wa_sementes-branch.
    if sy-subrc = 0.
      wa_cancel_devolucao_ent_sm-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.


    wa_cancel_devolucao_ent_sm-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_devolucao_ent_sm-motivo_cancelamento    = 'Dados da Nota estão incorretos'.
    wa_cancel_devolucao_ent_sm-destino_armazenador    = 'false'.


    check  verro = abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_devolucao_ent_sm compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo
         changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_devolucao_ent_sm-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.


      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.


        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_devolucao_ent_sm-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_sementes-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
    clear wa_zsdt0212.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_SAIDA_TRANSFERENCIA_SM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_saida_transferencia_sm .
  data wa_item_saida_transf_sm type ty_item_saida_transf_sm.
  data: vkunnr     type kna1-kunnr,
*** Inicio - Rubenilson Pereira - 07.04.25 #168932
        lo_mm_util type ref to zcl_mm_util,
        lv_matnr   type matnr18,
        lv_renasem type zsdt0216-renasem.

  create object lo_mm_util.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl, vkunnr.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro = abap_false.
    "DIFERENTE DE 'ZC'
    read table it_j_1bnflin_02 into wa_j_1bnflin_02 with key docnum = wa_sementes-docnum.

    wa_saida_transferencia_sm-cnpj_revenda = wa_sementes-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service =  wa_zauth_webservice-service
                                                               bukrs   =  wa_sementes-bukrs
                                                               branch  =  wa_sementes-branch.
    if sy-subrc = 0.
      wa_saida_transferencia_sm-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    if wa_sementes-regio = 'MT'.
      wa_saida_transferencia_sm-destino_fora_m_t = 'false'.
    else.
      wa_saida_transferencia_sm-destino_fora_m_t =  'true'.
    endif.

    if wa_sementes-nfenum is not initial.
      wa_saida_transferencia_sm-nr_nf = wa_sementes-nfenum.
    else.
      wa_saida_transferencia_sm-nr_nf = wa_sementes-nfnum.
    endif.

    wa_saida_transferencia_sm-serie_nf = wa_sementes-series.
    concatenate  wa_sementes-docdat+0(4) '-' wa_sementes-docdat+4(2) '-'  wa_sementes-docdat+6(2) into  wa_saida_transferencia_sm-dt_nf.

    wa_saida_transferencia_sm-destino_revenda      = 'true'.
    wa_saida_transferencia_sm-destino_armazenador  = 'false'.
    wa_saida_transferencia_sm-destino_internacional = 'false'.
    wa_saida_transferencia_sm-observacao = ' '.

    vkunnr   = wa_sementes-parid+4(4).
    vkunnr = |{ vkunnr alpha = in }|.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
    lv_matnr = wa_j_1bnflin_02-matnr(18).

    call method lo_mm_util->get_renasem(
      exporting
        i_material = lv_matnr
      importing
        r_renasem  = lv_renasem ).
    " WBARBOSA - 08.05.25 #168932
    if lv_renasem is not initial.
      wa_saida_transferencia_sm-renasem_destino      = lv_renasem.
    else.
      " WBARBOSA - 08.05.25 #168932
*** Fim - Rubenilson Pereira - 07.04.25 #168932

      select single * from zsdt0216 into @data(wa_zsdt0216)
        where kunnr            eq @vkunnr
        and   tipo             eq 'C'
        and   setor_atividade  eq 'S'
        and   revenda          eq 'S'.

      if sy-subrc = 0.

        if wa_zsdt0216-renasem is initial.

          clear mensagem.
          verro     = abap_true.
          concatenate 'A Filial' vkunnr+6(4) 'está sem a informação do Renasem na Transação ZSDT0154.' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_defensivos-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          clear wa_zsdt0214.
          commit work.
          exit.

        else.
          wa_saida_transferencia_sm-renasem_destino      = wa_zsdt0216-renasem. " WBARBOSA - 08.05.25 #168932
        endif.
      else.
        clear mensagem.
        verro = abap_true.
        concatenate 'A Filial' vkunnr+6(4) 'não está cadastrada como Revenda de Semente na Tranação ZSDT0154.' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.
        exit.
      endif.
    endif.

    select single * from kna1 into @data(wkna1)
      where kunnr eq @vkunnr.

    wkna1-name1 = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = conv #( wkna1-name1 ) ) ).

    wa_saida_transferencia_sm-cpf_cnpj_destino      = wkna1-stcd1.
    wa_saida_transferencia_sm-nome_destino          = wkna1-name1.
    wa_saida_transferencia_sm-cod_municipio_destino = wkna1-txjcd+3(12).
    wa_saida_transferencia_sm-cod_tipo_produto      = 2.

    loop at it_j_1bnflin_02 into wa_j_1bnflin_02
      where docnum = wa_sementes-docnum.

      wa_item_saida_transf_sm-lote         = wa_j_1bnflin_02-charg.
      wa_item_saida_transf_sm-nr_dar_indea = ' '.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
      lv_matnr = wa_j_1bnflin_02-matnr(18).

      call method lo_mm_util->get_codigo_indea(
        exporting
          i_material = lv_matnr
        importing
          id_indea   = wa_item_saida_transf_sm-cod_cultivar ).

      if wa_item_saida_transf_sm-cod_cultivar is initial.
        read table it_zsdt0210 into wa_zsdt0210 with key matnr = wa_j_1bnflin_02-matnr.
        if sy-subrc is initial.
          wa_item_saida_transf_sm-cod_cultivar =  wa_zsdt0210-id_matnr_idea.
        else.

          clear mensagem.
          verro = abap_true.
          mensagem = 'De/Para entre Material SAP x Cod. Cultivar Indea não realizado!'.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        endif.
      endif.

*** Fim - Rubenilson Pereira - 07.04.25 #168932
      read table it_fiwrt0009 into data(ls_fiwrt0009) with key charg = wa_item_saida_transf_sm-lote.
      if ls_fiwrt0009-nr_fase is not initial.
        wa_item_saida_transf_sm-nr_dar_fase = ls_fiwrt0009-nr_fase.
      else.

        read table it_zmmt0102 into wa_zmmt0102 with key docnum = wa_j_1bnflin_02-docnum.

        if wa_zmmt0102-nr_fase is not initial.
          wa_item_saida_transf_sm-nr_dar_fase = wa_zmmt0102-nr_fase.
        else.
          clear mensagem.
          verro = abap_true.
*        MENSAGEM = 'O lote está sem o Código Fase'.
          concatenate 'O Lote' wa_j_1bnflin_02-charg 'está sem o Código Fase.' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        endif.
      endif.

      case wa_j_1bnflin_02-meins.
        when 'KG'.
          wa_item_saida_transf_sm-qnt_produto = wa_j_1bnflin_02-menge.
        when 'TO'.
          wa_item_saida_transf_sm-qnt_produto = wa_j_1bnflin_02-menge * 1000.
        when 'BAG' or 'SAC' or 'BIG'.
          select single * from mara into @data(wa_mara)
            where matnr eq @wa_j_1bnflin_02-matnr.

          data: vl_object type ausp-objek.

*      "// Material + Lote
          clear ls_obj.
          ls_obj-matnr = wa_mara-matnr.
          ls_obj-charg = wa_j_1bnflin_02-charg.
          vl_object = ls_obj.

          call method lo_mm_util->get_caracteristica_geral
            exporting
              i_object               = vl_object
              i_caracteristica       = 'PESO_BAG'
            importing
              e_valor_caracteristica = data(lv_peso_bag).

          wa_item_saida_transf_sm-qnt_produto = wa_j_1bnflin_02-menge * lv_peso_bag.
      endcase.


      append value #( cod_cultivar   = wa_item_saida_transf_sm-cod_cultivar
                      lote           = wa_item_saida_transf_sm-lote
                      nr_dar_indea   = wa_item_saida_transf_sm-nr_dar_indea
                      nr_dar_fase    = wa_item_saida_transf_sm-nr_dar_fase
                      qnt_produto    = wa_item_saida_transf_sm-qnt_produto
                     )   to wa_saida_transferencia_sm-itens.
    endloop.

    check verro = abap_false.

    if wzsdt0212-id is not initial.
      wa_saida_transferencia_sm-cod_lancamento_usuario =  wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021
                              using  'ZSEQ_INDEA' '01' changing wa_saida_transferencia_sm-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.

    e_xml = /ui2/cl_json=>serialize( data = wa_saida_transferencia_sm compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                               wa_zauth_ws_0001-username
                               wa_zauth_ws_0001-password
                               tp_metodo  changing resultado.

    translate resultado to upper case.

    if  resultado ca sy-abcde .

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.

      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_saida_transferencia_sm-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.
*     wa_zsdt0212-status       =  'E'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.

    elseif  resultado ca '0123456789'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.

      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_saida_transferencia_sm-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  endif.

  clear: wa_zsdt0216.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_CANCEL_SAIDA_TRANSF_SM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_cancel_saida_transf_sm .

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).

    verro = abap_false.

    wa_cancel_saida_transf_sm-cnpj_revenda = wa_sementes-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_SAIDA_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key  service = wa_zauth_webservice-service
                                                                bukrs   = wa_sementes-bukrs
                                                                branch  = wa_sementes-branch.
    if sy-subrc = 0.
      wa_cancel_saida_transf_sm-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_cancel_saida_transf_sm-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_saida_transf_sm-motivo_cancelamento    = 'Dados da Nota estão incorretos'.
    wa_cancel_saida_transf_sm-destino_armazenador    = 'false'.

    check  verro = abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_saida_transf_sm compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo
         changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_saida_transf_sm-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.


        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_saida_transf_sm-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_sementes-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
    clear wa_zsdt0212.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_ENTRADA_TRANSFERENCIA_SM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_entrada_transferencia_sm .

  data: wa_item_entrada_transf_sm type ty_item_entrada_transf_sm.
  data: vlifnr     type lfa1-lifnr,
*** Inicio - Rubenilson Pereira - 07.04.25 #168932
        lo_mm_util type ref to zcl_mm_util,
        lv_matnr   type matnr18.

  create object lo_mm_util.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.

  tp_metodo = 'POST'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if ( wzsdt0212-status <> 'A' and  wzsdt0212-status <> 'C' ) .

    verro = abap_false.
    wa_entrada_transferencia_sm-cnpj_revenda = wa_sementes-cnpj_bupla.

    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service  = wa_zauth_webservice-service
                                                               bukrs    = wa_sementes-bukrs
                                                               branch   = wa_sementes-branch.
    if sy-subrc = 0.
      wa_entrada_transferencia_sm-hash_revenda = wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_entrada_transferencia_sm-origem_armazenador       = 'false'.

    read table it_lfa1 into wa_lfa1 with key lifnr = wa_sementes-parid.
    if sy-subrc = 0.
      wa_entrada_sementes_arm-cpf_cnpj_fornecedor   = wa_lfa1-stcd1.
    endif.

    vlifnr   = wa_sementes-parid+4(4).
    vlifnr = |{ vlifnr alpha = in }|.

    select single * from lfa1 into @data(wa_lfa1)
      where lifnr eq @vlifnr.

    if sy-subrc = 0.
      wa_entrada_transferencia_sm-cod_municipio_fornecedor = wa_lfa1-txjcd+3(12).
      wa_entrada_transferencia_sm-nome_fornecedor          = wa_lfa1-name1.
    endif.

    select single * from zsdt0216 into @data(wa_zsdt0216)
      where kunnr   eq @vlifnr
       and  tipo    eq 'F'
       and  setor_atividade eq 'S'.

    if sy-subrc = 0.

      if wa_zsdt0216-renasem is initial.

        clear mensagem.
        verro     = abap_true.
        concatenate 'O Fornecedor' vlifnr+6(4) 'está sem a informação do Renasem na transação ZSDT0154.' into mensagem separated by space.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '1'.
        wa_zsdt0214-mensagem     = mensagem.
        wa_zsdt0214-usnam        = sy-uname.

        modify zsdt0214 from wa_zsdt0214.
        clear wa_zsdt0214.
        commit work.
        exit.
      else.
        wa_entrada_transferencia_sm-renasem_fornecedor       = wa_zsdt0216-renasem.
      endif.
    else.

      clear mensagem.
      verro     = abap_true.
      concatenate 'O Fornecedor' vlifnr+6(4) 'não está cadastrado como Fornecedor de Semente na Transação ZSDT0154.' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.

      modify zsdt0214 from wa_zsdt0214.
      clear wa_zsdt0214.
      commit work.
      exit.

    endif.

    if wa_sementes-nfenum is not initial.
      wa_entrada_transferencia_sm-nr_nf = wa_sementes-nfenum.
    else.
      wa_entrada_transferencia_sm-nr_nf = wa_sementes-nfnum.
    endif.

    if wa_sementes-series is not initial.
      wa_entrada_transferencia_sm-serie_nf = wa_sementes-series.
    else.
      clear mensagem.
      verro = abap_true.
      mensagem = 'A Série da nota Fiscal não está preenchido!'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.


    concatenate wa_sementes-docdat+0(4) '-' wa_sementes-docdat+4(2) '-' wa_sementes-docdat+6(2) into wa_entrada_transferencia_sm-dt_nf.
    concatenate wa_sementes-pstdat+0(4) '-' wa_sementes-pstdat+4(2) '-' wa_sementes-pstdat+6(2) into wa_entrada_transferencia_sm-data_entrada.
    wa_entrada_transferencia_sm-cod_tipo_produto = '2'.

    "itens
    if it_mseg is not initial.
      loop at it_mseg into wa_mseg
        where docnum = wa_sementes-docnum.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
        lv_matnr = wa_mseg-matnr.

        call method lo_mm_util->get_codigo_indea(
          exporting
            i_material = lv_matnr
          importing
            id_indea   = wa_item_entrada_transf_sm-cod_cultivar ).
*** Fim - Rubenilson Pereira - 07.04.25 #168932
        if wa_item_entrada_transf_sm-cod_cultivar is initial.
          select single id_matnr_idea
            from zsdt0210
          into wa_item_entrada_transf_sm-cod_cultivar
            where matnr eq wa_mseg-matnr.
          if sy-subrc is not initial.

            clear mensagem.
            verro =  abap_true.
            mensagem = 'De/Para entre Material SAP x Cod. Cultivar Indea não realizado!'.

            perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
            wa_zsdt0214-data_atual   = sy-datum.
            wa_zsdt0214-hora_atual   = sy-uzeit.
            wa_zsdt0214-docnum       = wa_sementes-docnum.
            wa_zsdt0214-origem       = '1'.
            wa_zsdt0214-mensagem     = mensagem.
            wa_zsdt0214-usnam        = sy-uname.
            modify zsdt0214 from wa_zsdt0214.
            commit work.
            clear wa_zsdt0214.
            exit.
          endif.
        endif.

        data: vl_object type ausp-objek.

*      "// Material + Lote
        ls_obj-matnr = wa_mseg-matnr.
        ls_obj-charg = wa_mseg-charg.
        vl_object = ls_obj.

        wa_item_entrada_transf_sm-lote = wa_mseg-charg.

        call method lo_mm_util->get_caracteristica_geral
          exporting
            i_object               = vl_object
            i_caracteristica       = 'SEMENTE_CATEGORIA'
          importing
            e_valor_caracteristica = data(vlr_caracteristica).

        wa_item_entrada_transf_sm-cod_categoria_sementes =  vlr_caracteristica.

        if wa_item_entrada_transf_sm-cod_categoria_sementes is initial.
          read table it_zmmt0102 into wa_zmmt0102 with key mblnr  = wa_mseg-mblnr
                                                           mjahr  = wa_mseg-mjahr
                                                           charg  = wa_mseg-charg
                                                           matnr  = wa_mseg-matnr.

          if wa_zmmt0102-categoria is not initial.
            wa_item_entrada_transf_sm-cod_categoria_sementes =  wa_zmmt0102-categoria.
          endif.
        endif.

        if wa_item_entrada_transf_sm-cod_categoria_sementes is initial.
          clear mensagem.
          verro = abap_true.
*          MENSAGEM = 'O Lote está sem  Categoria da Semente'.
          concatenate 'O Lote' wa_mseg-charg 'está sem  Categoria da Semente.' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        endif.

        wa_item_entrada_transf_sm-nr_dar_indea = ' '.

        read table it_fiwrt0009 into data(ls_fiwrt0009) with key charg = wa_item_entrada_transf_sm-lote.
        if ls_fiwrt0009-nr_fase is not initial.
          wa_item_entrada_transf_sm-nr_dar_fase = ls_fiwrt0009-nr_fase.
        else.
          if wa_zmmt0102-nr_fase is not initial.
            wa_item_entrada_transf_sm-nr_dar_fase = wa_zmmt0102-nr_fase.
          endif.
        endif.

        if wa_item_entrada_transf_sm-nr_dar_fase is initial.
          clear mensagem.
          verro = abap_true.
*          MENSAGEM = 'O lote está sem o Código Fase'.
          concatenate 'O Lote' wa_mseg-charg 'está sem o Código Fase.' into mensagem separated by space.

          perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
          wa_zsdt0214-data_atual   = sy-datum.
          wa_zsdt0214-hora_atual   = sy-uzeit.
          wa_zsdt0214-docnum       = wa_sementes-docnum.
          wa_zsdt0214-origem       = '1'.
          wa_zsdt0214-mensagem     = mensagem.
          wa_zsdt0214-usnam        = sy-uname.
          modify zsdt0214 from wa_zsdt0214.
          commit work.
          clear wa_zsdt0214.
          exit.
        endif.

        case  wa_mseg-meins.
          when 'KG'.
            wa_item_entrada_transf_sm-qnt_produto = wa_mseg-menge.
          when 'TO'.
            wa_item_entrada_transf_sm-qnt_produto = wa_mseg-menge * 1000.
          when 'BAG' or 'SAC' or 'BIG'.
            select single *
              from mara into @data(wa_mara)
             where matnr eq @wa_mseg-matnr.

            lv_matnr = wa_mseg-matnr.

*      "// Material + Lote
            clear: ls_obj, vl_object.
            ls_obj-matnr = wa_mseg-matnr.
            ls_obj-charg = wa_mseg-charg.
            vl_object = ls_obj.

            call method lo_mm_util->get_caracteristica_geral
              exporting
                i_object               = vl_object
                i_caracteristica       = 'PESO_BAG'
              importing
                e_valor_caracteristica = data(lv_peso_bag).

            wa_item_entrada_transf_sm-qnt_produto = wa_mseg-menge * lv_peso_bag.

        endcase.

        append value #(  cod_cultivar              =     wa_item_entrada_transf_sm-cod_cultivar
                         cod_categoria_sementes    =     wa_item_entrada_transf_sm-cod_categoria_sementes
                         nr_dar_indea              =     wa_item_entrada_transf_sm-nr_dar_indea
                         nr_dar_fase               =     wa_item_entrada_transf_sm-nr_dar_fase
                         lote                      =     wa_item_entrada_transf_sm-lote
                         qnt_produto               =     wa_item_entrada_transf_sm-qnt_produto
                      ) to wa_entrada_transferencia_sm-itens.

        clear:  wa_zmmt0102.
        clear wa_mseg.
      endloop.

    else.
      clear mensagem.
      verro = abap_true.
      mensagem = 'Não foi possível localizar os itens da nota (MIGO) para envio ao SISDEV!'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    check verro = abap_false.

    if wzsdt0212-id is not initial.
      wa_entrada_transferencia_sm-cod_lancamento_usuario =  wzsdt0212-id.
    else.
      perform get_next_number in program zsdr021  using  'ZSEQ_INDEA' '01'
                                                  changing wa_entrada_transferencia_sm-cod_lancamento_usuario.
    endif.

    purl = wa_zauth_webservice-url.
    e_xml = /ui2/cl_json=>serialize( data = wa_entrada_transferencia_sm compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    perform z_envia_json using purl
                               wa_zauth_ws_0001-username
                               wa_zauth_ws_0001-password
                               tp_metodo  changing resultado.

    translate resultado to upper case.

    if  resultado ca sy-abcde .

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = resultado.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.

      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_entrada_transferencia_sm-cod_lancamento_usuario.
**      wa_zsdt0212-id_indea     =  resultado.

      if resultado in r_erros. "IF resultado = 'JÁ EXISTE UM LANÇAMENTO DE ENTRADA PRA ESSA NF DESSE FORNECEDOR' OR resultado EQ 'LANÇAMENTO JÁ REGISTRADO'.
        wa_zsdt0212-status   =  'A'.
        wa_zsdt0212-id_indea =  '9999999999'.
      else.
        wa_zsdt0212-status     =  'E'.
      endif.
*     wa_zsdt0212-status       =  'E'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.

    elseif  resultado ca '0123456789'.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '2'.
      wa_zsdt0214-mensagem     = 'Lançamento enviado ao Indea com Sucesso'.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.

      wa_zsdt0212-docnum       =  wa_sementes-docnum.
      wa_zsdt0212-id           =  wa_entrada_transferencia_sm-cod_lancamento_usuario.
      wa_zsdt0212-id_indea     =  resultado.
      wa_zsdt0212-status       =  'A'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_CANCEL_ENTRADA_TRANSF_SM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_ws_cancel_entrada_transf_sm .

  clear: mensagem, calcular,  v_frac,   resultado, tp_metodo, purl.
  tp_metodo = 'PUT'.

  read table it_zsdt0212 into wzsdt0212 with key docnum = wa_sementes-docnum.

  if sy-subrc = 0 and ( wzsdt0212-status = 'A' or wzsdt0212-status = 'E' ).
    verro = abap_false.

    wa_cancel_entrada_transf_sm-cnpj_revenda = wa_sementes-cnpj_bupla.
    read table it_zauth_webservice into wa_zauth_webservice with key service = 'INDEA_ENTRADA_SEMENTES'.

    read table it_zauth_ws_0001 into wa_zauth_ws_0001 with key service = wa_zauth_webservice-service
                                                               bukrs  =  wa_sementes-bukrs
                                                               branch =  wa_sementes-branch.
    if sy-subrc = 0.
      wa_cancel_entrada_transf_sm-hash_revenda =  wa_zauth_ws_0001-add01.
    else.
      clear mensagem.
      verro = abap_true.
      concatenate 'Hash INDEA da Revenda' wa_sementes-branch 'não cadastrada na transação ZAUTH_WEBSERVICE' into mensagem separated by space.

      perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
      wa_zsdt0214-data_atual   = sy-datum.
      wa_zsdt0214-hora_atual   = sy-uzeit.
      wa_zsdt0214-docnum       = wa_sementes-docnum.
      wa_zsdt0214-origem       = '1'.
      wa_zsdt0214-mensagem     = mensagem.
      wa_zsdt0214-usnam        = sy-uname.
      modify zsdt0214 from wa_zsdt0214.
      commit work.
      clear wa_zsdt0214.
      exit.
    endif.

    wa_cancel_entrada_transf_sm-cod_lancamento_usuario = wzsdt0212-id.
    wa_cancel_entrada_transf_sm-oigem_armazenador      = 'true'.
    wa_cancel_entrada_transf_sm-motivo_cancelamento    = 'Dados da Nota estão incorretos'.


    check verro = abap_false.

    if wzsdt0212-status = 'A'.

      purl = wa_zauth_webservice-url.
      e_xml = /ui2/cl_json=>serialize( data = wa_cancel_entrada_transf_sm compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      perform z_envia_json using purl
                                 wa_zauth_ws_0001-username
                                 wa_zauth_ws_0001-password
                                 tp_metodo changing resultado.

      translate resultado to upper case.

      if  resultado ca sy-abcde .
        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_transf_sm-cod_lancamento_usuario.
**        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        if resultado eq retorno_cancel.
          wa_zsdt0212-status       =  'C'.
        else.
          wa_zsdt0212-status       =  'E'.
        endif.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.

      elseif  resultado ca '0123456789'.

        perform get_next_number in program zsdr021 using  'ZSEQ_L_IND'  '01'  changing  wa_zsdt0214-id.
        wa_zsdt0214-data_atual   = sy-datum.
        wa_zsdt0214-hora_atual   = sy-uzeit.
        wa_zsdt0214-docnum       = wa_sementes-docnum.
        wa_zsdt0214-origem       = '2'.
        wa_zsdt0214-mensagem     = resultado.
        wa_zsdt0214-usnam        = sy-uname.
        modify zsdt0214 from wa_zsdt0214.
        commit work.
        clear wa_zsdt0214.

        wa_zsdt0212-docnum       =  wa_sementes-docnum.
        wa_zsdt0212-id           =  wa_cancel_entrada_transf_sm-cod_lancamento_usuario.
        wa_zsdt0212-id_indea     =  resultado.
        wa_zsdt0212-status       =  'A'.
        wa_zsdt0212-usnam        =  sy-uname.
        wa_zsdt0212-data_atual   =  sy-datum.
        wa_zsdt0212-hora_atual   =  sy-uzeit.
        modify zsdt0212 from wa_zsdt0212.
        commit work.
        clear wa_zsdt0212.
      endif.

    elseif  wzsdt0212-status = 'E'.

      wa_zsdt0212-docnum       =  wzsdt0212-docnum.
      wa_zsdt0212-id           =  wzsdt0212-id.
      wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
      wa_zsdt0212-status       =  'X'.
      wa_zsdt0212-usnam        =  sy-uname.
      wa_zsdt0212-data_atual   =  sy-datum.
      wa_zsdt0212-hora_atual   =  sy-uzeit.
      modify zsdt0212 from wa_zsdt0212.
      commit work.
      clear wa_zsdt0212.
    endif.
  else.
    wa_zsdt0212-docnum       =  wa_sementes-docnum.
    wa_zsdt0212-id           =  wzsdt0212-id.
    wa_zsdt0212-id_indea     =  wzsdt0212-id_indea.
    wa_zsdt0212-status       =  'X'.
    wa_zsdt0212-usnam        =  sy-uname.
    wa_zsdt0212-data_atual   =  sy-datum.
    wa_zsdt0212-hora_atual   =  sy-uzeit.
    modify zsdt0212 from wa_zsdt0212.
    commit work.
    clear wa_zsdt0212.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_SEQ_CFOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_busca_seq_cfop .

  data: l_dias type i.

*-------------------------------------------------
* Grupos CFOP
*-------------------------------------------------
  refresh  it_value.
  call function 'G_SET_GET_ALL_VALUES'
    exporting
      setnr           = 'ZSDR021_GRUPOS_CFOP'
      class           = '0000'
      no_descriptions = ''
    tables
      set_values      = it_value
    exceptions
      set_not_found   = 1
      others          = 2.

  loop at it_value.
    wa_cfop-cfop      = it_value-from.
    wa_cfop-grupo     = it_value-title.
    append wa_cfop   to it_cfop.
  endloop.

  sort it_cfop by cfop.
  delete adjacent duplicates from it_cfop
                        comparing cfop.

endform.
