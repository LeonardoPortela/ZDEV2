*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
* Cliente....: AMAGGI                                                  *
* Autor......: Alexandre Ferrari                                       *
* Data.......: 17.07.2010                                              *
* Descrição  : Carta Frete                                             *
* Transação..: ZLES0001                                                *
* Projeto....:                                                         *
* Cód Espec..:                                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Autor      : BBKO/Vagner Santos                                      *
* Observações: Somar o valor da condição IOF ao valor do seguro, con-  *
*              dição ZSEG.                                             *
* Data       : 14/10/2010                                              *
*----------------------------------------------------------------------*
* Autor      : Camila da Silva Brand                                   *
* Observações: Bloqueio de reimpressão                                 *
* Data       :                                                         *
*----------------------------------------------------------------------*

report  zlesr0001 message-id zles.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
constants: c_1(1)     type c value '1',
           c_2(1)     type c value '2',
           c_3(1)     type c value '3',
           c_4(1)     type c value '4',
           c_8(1)     type c value '8',
           c_c(1)     type c value 'C',
           c_v(1)     type c value 'V',
           c_e(1)     type c value 'E',
           c_i(1)     type c value 'I',
           c_eq(2)    type c value 'EQ',
           c_q(1)     type c value 'Q',
           c_trac(1)  type c value '-',
           c_bi(2)    type c value 'BI',
           c_lr(2)    type c value 'LR',
           c_pc(2)    type c value 'PC',
           c_pv(2)    type c value 'PV',
           c_mt(2)    type c value 'MT',
           c_md(2)    type c value 'MD',
           c_li(2)    type c value 'LI',
           c_flag(1)  type c value 'X',
           c_j(1)     type c value 'J',
           c_m(1)     type c value 'M', "Fatura
           c_n(1)     type c value 'N', "Estorno de Fatura / Alteração Carta Frete
           c_bar(1)   type c value '/',
           c_kappl(1) type c value 'F',
           c_kschl(4) type c value 'ZMRG',
           c_zffj(4)  type c value 'ZFFJ', " Pessoa Juridíca
           c_zfff(4)  type c value 'ZFFF', " Pessoa Física
           c_zfre(4)  type c value 'ZFRE',
           c_zlot(4)  type c value 'ZLOT',
           c_zins(4)  type c value 'ZINS',
           c_zset(4)  type c value 'ZSET',
           c_zirf(4)  type c value 'ZIRF',
           c_zseg(4)  type c value 'ZSEG',
*BBKO/Vagner Santos - 14.10.2010 - Início da alteração.
           c_ziof(4)  type c value 'ZIOF',
*BBKO/Vagner Santos - 14.10.2010 - Fim da alteração.
           c_zadm(4)  type c value 'ZADM',
           c_zped(4)  type c value 'ZPED'.

*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*f
data: vg_conhec           type vttk-exti1,
      vg_lifnr_pv         type lfa1-lifnr,
      vg_lifnr_mt         type lfa1-lifnr,
      vg_pc               type lfa1-lifnr,
      vg_lr               type lfa1-lifnr,
      vg_erdat(10)        type c,
      vg_docmun           type j_1bnfdoc-docnum,
      vg_frete_contratado type kbetr,      " Frete Contratado
      vg_nfenum(20)       type c,
      vl_cfrete           type vttk-exti2,
      vl_vbeln            type vbak-vbeln,
      sl_doc_prc          type j_1bnfdoc .

*----------------------------------------------------------------------*
* Protótipo
*----------------------------------------------------------------------*
* Janela Cabeçalho
*----------------------------------------------------------------------*

* Cabeçalho transporte
types: begin of y_vttk,
        tknum type vttk-tknum, " Nº transporte
        erdat type vttk-erdat, " Data de criação do registro
        abfer type vttk-abfer, " Tipo de processamento para transporte
        exti1 type vttk-exti1, " Conhecimento de embarque
        exti2 type vttk-exti2, " Carta Frete
        tdlnr type vttk-tdlnr, " Nº do agente de frete
        shtyp type vttk-shtyp, "Tipo de transporte
      end of y_vttk.

* Item de transporte
types: begin of y_vttp,
        tknum type vttp-tknum, " Nº transporte
        tpnum type vttp-tpnum, " Item de transporte
        vbeln type vttp-vbeln, " Fornecimento
       end of y_vttp.

* Margem de tolerância de frete
types: begin of y_zlest0001,
        bukrs      type zlest0001-bukrs,      " Empresa
        branch     type zlest0001-branch,     " Local de negócios
        subobj1    type zlest0001-subobj1,    " Grupo de nºs de nota fiscal
        nrrange1   type zlest0001-nrrange1,   " Nº do intervalo de numeração
        subobj2    type zlest0001-subobj2,    " Grupo de nºs de nota fiscal
        nrrange2   type zlest0001-nrrange2,   " Nº do intervalo de numeração
        series     type zlest0001-series,     " Série
      end of y_zlest0001.

* Informações de Transporte
types: begin of y_zlest0014.
        include structure zlest0014.
types: end of y_zlest0014.

* Mestre de fornecedores (parte geral)
types: begin of y_lfa1,
        lifnr type lfa1-lifnr, " Nº conta do fornecedor
        name1 type lfa1-name1,                              " Nome 1
        ort01 type lfa1-ort01, " Local
        ort02 type lfa1-ort02, " Bairro
        pstlz type lfa1-pstlz, " Código postal da cx.postal
        regio type lfa1-regio, " Região (país, estado, província, condado)
        stras type lfa1-stras, " Rua e nº
        stcd1 type lfa1-stcd1, " Nº ID fiscal 1
        stcd3 type lfa1-stcd3, " Nº identificação fiscal 3
      end of y_lfa1.

* Mestre de fornecedores - Coleta
types: begin of y_coleta,
        lifnr type lfa1-lifnr, " Nº conta do fornecedor
        name1 type lfa1-name1,                              " Nome 1
        ort01 type lfa1-ort01, " Local
        regio type lfa1-regio, " Região (país, estado, província, condado)
        stcd1 type lfa1-stcd1, " Nº ID fiscal 1
        stcd2 type lfa1-stcd2, " N  ID Fiscal
      end of y_coleta.

* Mestre de fornecedores - Entrega
types: begin of y_entrega,
        lifnr type lfa1-lifnr, " Nº conta do fornecedor
        name1 type lfa1-name1,                              " Nome 1
        ort01 type lfa1-ort01, " Local
        regio type lfa1-regio, " Região (país, estado, província, condado)
        stcd1 type lfa1-stcd1, " Nº ID fiscal
        stcd2 type lfa1-stcd2, " N  ID Fiscal
      end of y_entrega.

* Fluxo de documentos de vendas e distribuição
types: begin of y_vbfa,
        vbelv   type vbfa-vbelv,       " Documento de vendas e distribuição
        vbeln   type vbfa-vbeln,       " Documento de vendas e distribuição
        posnn   type vbfa-posnn,       " Itens subseqüentes de um documento de venda
        vbtyp_n type vbfa-vbtyp_n,     " Categoria de documento SD subseqüente
        vbtyp_v type vbfa-vbtyp_v,     " Ctg.documento de venda e distribuição
        refkey  type j_1bnflin-refkey, " Ref.doc.origem
      end of y_vbfa.

* Partidas individuais da nota fiscal
types: begin of y_j1bnflin,
        docnum type j_1bnflin-docnum, " Nº documento
        itmnum type j_1bnflin-itmnum, " Nº item do documento
        reftyp type j_1bnflin-reftyp, " Tipo referência
        refkey type j_1bnflin-refkey, " Referência ao documento de origem
      end of y_j1bnflin.

* Cabeçalho da nota fiscal
types: begin of y_j_1bnfdoc.
        include structure j_1bnfdoc.
types: end of y_j_1bnfdoc.

types: begin of y_j_1binlin_add.
        include structure j_1binlin.
types: end of y_j_1binlin_add.

* Documento SD: fornecimento: dados de item
types: begin of y_ar,
        vbeln type lips-vbeln, " Fornecimento
        posnr type lips-posnr, " Item de remessa
        vgbel type lips-vgbel, " Nº documento do documento de referência
        vgpos type lips-vgpos, " Nº item do item comercial modelo
        ebelp type ekpo-ebelp, " Item do Doc. Compra,
      end of y_ar.

* Histórico para o documento de compra
types: begin of y_ekbe,
        ebeln  type ekbe-ebeln, " Nº do documento de compras
        ebelp  type ekbe-ebelp, " Nº item do documento de compra
        gjahr  type ekbe-gjahr, " Ano do documento do material
        belnr  type ekbe-belnr, " Nº documento de material
        bewtp  type ekbe-bewtp, " Ctg.de histórico de pedido
        refkey type j_1bnflin-refkey,
      end of y_ekbe.

* Controle de Carta frete emitida
types: begin of y_zlest0014_f,
      tknum     type zlest0014-tknum    ,
      conhec    type zlest0014-conhec   ,
      ctafrete  type zlest0014-ctafrete ,
      actafrete type zlest0014-actafrete,
    end of y_zlest0014_f.

*----------------------------------------------------------------------*
* Janela Proprietário
*----------------------------------------------------------------------*

* Parceiro de transporte
types: begin of y_vtpa,
        lifnr type vtpa-lifnr, " Nº conta do fornecedor
      end of y_vtpa.

* Mestre de Fornecedor
types: begin of y_pv,
        lifnr type lfa1-lifnr, " Nº conta do fornecedor
        name1 type lfa1-name1, " Nome
        ort01 type lfa1-ort01, " Local
        ort02 type lfa1-ort02, " Bairro
        pstlz type lfa1-pstlz, " Código postal da cx.postal
        regio type lfa1-regio, " Região (país, estado, província, condado)
        stras type lfa1-stras, " Rua e nº
        ktokk type lfa1-ktokk, " Grupo de contas do fornecedor
        stcd1 type lfa1-stcd1, " Nº ID fiscal 1
        stcd2 type lfa1-stcd2, " Nº ID fiscal 2
        stcd3 type lfa1-stcd3, " Nº identificação fiscal 3
        stkzn type lfa1-stkzn, " Tipo pessoa
      end of y_pv.

* Documento SD: fornecimento: dados de item
types: begin of y_ordem,
        vbeln type lips-vbeln, " Fornecimento
        posnr type lips-posnr, " Item de remessa
        vgbel type lips-vgbel, " Nº documento do documento de referência
        vgtyp type lips-vgtyp, " Ctg.documento de vendas e distribuição
      end of y_ordem.

* Documento SD: fornecimento: dados de item
types: begin of y_pedido,
        vgbel type lips-vgbel, " Nº documento do documento de referência
        vgpos type lips-vgpos, " Nº item do item comercial modelo
       end of y_pedido.

* Informações para preenchimento dos dados de remessa. (OPUS)
types: begin of y_zsdt0001,
        tp_movimento type zsdt0001-tp_movimento,  " Tipo de Movimento
        vbeln        type zsdt0001-vbeln,         "
        placa_cav    type zsdt0001-placa_cav,     " Placa do Veículo - Cavalo
        placa_car1   type zsdt0001-placa_car1,    " Placa do Veículo 1
        placa_car2   type zsdt0001-placa_car2,    " Placa do Veículo 2
        placa_car3   type zsdt0001-placa_car3,    " Placa do Veículo 3
        motorista    type zsdt0001-motorista,     " Código do Motorista
      end of y_zsdt0001.

* Controle de veículos
types: begin of y_zlest0002,
        pc_veiculo type zlest0002-pc_veiculo, " Placa veículo
        cd_cidade  type zlest0002-cd_cidade,  " Local
        cd_uf      type zlest0002-cd_uf,      " Região (país, estado, província, condado)
        ct_veiculo type zlest0002-ct_veiculo, " Categoria do veículo
      end of y_zlest0002.

*----------------------------------------------------------------------*
* Janela Motorista
*----------------------------------------------------------------------*

* Mestre de Fornecedor - MT
types: begin of y_mt,
        lifnr type lfa1-lifnr, " Nº conta do fornecedor
        name1 type lfa1-name1, "
        ort01 type lfa1-ort01, " Local
        regio type lfa1-regio, " Região (país, estado, província, condado
        stras type lfa1-stras, " Rua e nº
        bbbnr type lfa1-bbbnr, " Nº global de localização (parte 1)
        bbsnr type lfa1-bbsnr, " Número global de localização (parte 2)
        ktokk type lfa1-ktokk, " Grupo de contas do fornecedor
        stcd3 type lfa1-stcd3, " Nº ID fiscal 3
        telf1 type lfa1-telf1, " 1º Nº telefone
        stcd4 type lfa1-stcd4, " Nº identificação fiscal 4
      end of y_mt.

*----------------------------------------------------------------------*
* Janela Price
*----------------------------------------------------------------------*

* Custos de frete: dados do item
types: begin of y_vfkp,
        knumv type vfkp-knumv, " Nº condição do documento
        refty type vfkp-refty, " Ctg.documento de vendas e distribuição
        rebel type vfkp-rebel, " Doc.referenciado
       end of y_vfkp.

* Custos de frete: dados dos subitens
types: begin of y_vfsi,
        knumv type vfsi-knumv, " Nº condição do documento
        kposn type vfsi-kposn, " Subitem de custos de frete
        vbeln type vfsi-vbeln, " Nº documento de vendas e distribuição
        posnr type vfsi-posnr, " Nº item do documento de vendas e distribuição
        kzwi1 type vfsi-kzwi1, " Subtotal-condição 1 do esquema de cálculo
        kzwi2 type vfsi-kzwi2, " Subtotal-condição 2 do esquema de cálculo
       end of y_vfsi.

types: begin of y_lips,
        vbeln type lips-vbeln, " Fornecimento
        posnr type lips-posnr, " Item de remessa
        matnr type lips-matnr, " Material
        arktx type lips-arktx, " Texto breve do item da ordem do cliente
      end of y_lips.

* Condições (dados de operação)
types: begin of y_konv,
        knumv type konv-knumv, " Nº condição do documento
        kposn type konv-kposn, " Nº item ao qual se aplicam as condições
        kschl type konv-kschl, " Tipo de condição
        kbetr type konv-kbetr, " Montante ou porcentagem da condição
        kwert type konv-kwert, " Valor condição
        kinak type konv-kinak, " Condição está inativa
      end of y_konv.

*----------------------------------------------------------------------*
* Janela Assinatura
*----------------------------------------------------------------------*

* Usuário de referência para aplicações de Internet
types: begin of y_user,
        useralias type usrefus-useralias, " Alias para usuário da Internet
      end of y_user,

      begin of y_0001,
         bukrs      type zlest0001-bukrs,
         branch     type zlest0001-branch,
         subobj1    type zlest0001-subobj1,
         nrrange1   type zlest0001-nrrange1,
         subobj2    type zlest0001-subobj2,
         nrrange2   type zlest0001-nrrange2,
         series     type zlest0001-series,
      end of y_0001.

*----------------------------------------------------------------------*
* Funções
*----------------------------------------------------------------------*

types: begin of y_doc_partner.
        include structure j_1bnfnad.
types: end of y_doc_partner.

types: begin of y_doc_item.
        include structure j_1bnflin.
types: end of y_doc_item.

types: begin of y_doc_item_tax.
        include structure j_1bnfstx.
types: end of y_doc_item_tax.

types: begin of y_doc_header_msg.
        include structure j_1bnfftx.
types: end of y_doc_header_msg.

types: begin of y_doc_refer_msg.
        include structure j_1bnfref.
types: end of y_doc_refer_msg.

types begin of y_ext_header.
        include structure j_1bindoc.
types: end of y_ext_header.

types: begin of y_nf_item.
        include structure j_1bnflin.
types: end of y_nf_item.

types: begin of y_nf_item_tax.
        include structure j_1bnfstx.
types: end of y_nf_item_tax.

types: begin of y_ext_item.
        include structure j_1binlin.
types: end of y_ext_item.

*----------------------------------------------------------------------*
* SAIDA
*----------------------------------------------------------------------*

* Protótipo de Campos para Frete de Saida
types: begin of y_frete_cabec,

* Janela Cabeçalho
        dacte           type vttk-exti1,           " Número do conhecimento
        data_cria(10)   type c,                    " Data de Criação
        emitente_1(75)  type c,                    " Emitente Linha 1
        emitente_2(100) type c,                    " Emitente Linha 2
        coleta          type lfa1-name1,           " Coleta
        local_col(29)   type c,                    " Local de Coleta
        cnpj_col(18)    type c,                    " CNPJ Coleta
        entrega         type lfa1-name1,           " Entrega
        local_entr(29)  type c,                    " Local de Entrega
        cnpj_entr(18)   type c,                    " CNPJ Entrega
        n_fiscal(19)    type c,                    " Nota Fiscal
        peso_saida      type j_1bnfdoc-brgew,      " Peso bruto
        vl_n_fiscal     type j_1bnfdoc-nftot,      " Vlr.N. Fiscal

* Janela Proprietário
        proprietario      type lfa1-name1,           " Proprietário
        endereco_prop     type lfa1-stras,           " Endereço do Proprietário
        cidade_prop       type lfa1-ort01,           " Cidade do Proprietário
        uf_cidade_prop    type lfa1-regio,           " UF Cidade Proprietário
        cnjp_cpf_prop(18) type c,                    " CNPJ/CPF
        pis_nit_n         type lfa1-stcd4,           " PIS
        placa_cav         type zlest0002-pc_veiculo, " Placa CAV
        cid_cav           type zlest0002-cd_cidade,  " CID CAV
        placa_carr        type zlest0002-pc_veiculo, " Placa CARR
        cid_car           type zlest0002-cd_cidade,  " CID CARR
        placa_carr1       type zlest0002-pc_veiculo, " Placa CARR
        cid_car1          type zlest0002-cd_cidade,  " CID CARR

* Janela Motorista
        motorista       type lfa1-name1,          " Motorista
        endereco_motor  type lfa1-stras,          " Endereço do Motorista
        cidade_motor    type lfa1-ort01,          " Cidade do Motorista
        uf_cidade_motor type lfa1-regio,          " UF Cidade Motorista
        rg_nr_motor     type lfa1-stcd1,          " N RG Motorista
        uf_rg_nr_motor  type lfa1-regio,          " UF  Rg Nr.
        habilitacao     type lfa1-stcd4,          " Habilitação
        uf_habilitacao  type lfa1-regio,          " UF Habilitação
        fone_motor      type lfa1-telf1,          " Nº telefone

* Janela Contratado
        contratado      type lfa1-name1,          " Contrato
        endereco_contr  type lfa1-stras,          " Endereço Contrato
        cidade_contr    type lfa1-ort01,          " Cidade Contrato
        uf_contr        type lfa1-regio,          " UF Contrato
        cnpj_cpf_contr(18)  type c,               " CNPJ/CPF Contrato

* Janela Price
        frete_contratado(50) type c,              " Frete Contratado
        vlr_bruto            type kwert,              " VLR. Bruto
        inss                 type kwert,              " INSS a reter
        sest_snat            type kwert,              " SEST/SNAT
        irrf                 type kwert,              " IRRF
        seguro               type kwert,              " Seguro
        sub_total            type kwert,              " Sub-Total
        adiantamento         type kwert,              " Adiantamento
        liquido              type kwert,              " Líquido
        tolerancia(55)       type c,                  " Tolerância

* Janela Fatura
        fatura           type lfa1-name1,          " Fatura
        valor_fatura     type kbetr,               " Valor da Fatura

* Janela Total
        recebimento      type lfa1-name1,          " Recebi(emos)

* Janela Assinatura
        nome_assinatura  type usrefus-useralias,   " Assinatura
        carta_frete      type vttk-exti2,          " Carta Frete
        pedagio          type char30,

       end of y_frete_cabec.

*----------------------------------------------------------------------*
* Tabela Interna
*----------------------------------------------------------------------*
data: begin of wa_linhas,
      linha type c length 150,
    end of wa_linhas.


data: it_remessa        type table of y_vttp with header line,          " Item de transporte
      it_remessa_aux    type table of y_vttp,                           " Item de transporte
      it_aviso          type table of y_vttp,                           " Item de transporte
      it_aviso_aux      type table of y_vttp,                           " Item de transporte
      it_fatura         type table of y_vbfa,                           " Fluxo de documentos de vendas e distribuição
      it_fatura2        type table of y_vbfa,                           " Fluxo de documentos de vendas e distribuição
      it_nfitem         type table of y_j1bnflin,                       " Partidas individuais da nota fiscal
      it_item_add       type table of y_j_1binlin_add,                  " Partidas individuais da nota fiscal
      it_ordem          type table of y_ordem,                          " Documento SD: fornecimento: dados de item
      it_vc             type table of y_zsdt0001,                       " Informações para preenchimento dos dados de remessa.
      it_cavcar         type table of y_zlest0002,                      " Controle de veículos
      it_vfkp           type table of y_vfkp,                           " Custos de frete: dados do item
      it_vfsi           type table of y_vfsi,                           " Custos de frete: dados dos subitens
      it_konv           type table of y_konv,                           " Condições (dados de operação)
      it_ar             type table of y_ar,                             " Documento SD: fornecimento: dados de item
      it_ekbe           type table of y_ekbe,                           " Histórico para o documento de compra
      it_lips           type table of y_lips with header line,
      it_doc_partner    type table of y_doc_partner,
      it_doc_item       type table of y_doc_item,
      it_doc_item_tax   type table of y_doc_item_tax,
      it_doc_header_msg type table of y_doc_header_msg,
      it_doc_refer_msg  type table of y_doc_refer_msg,
      it_nf_item        type table of y_nf_item,
      it_pedido         type table of y_pedido,
      it_nf_item_tax    type table of y_nf_item_tax,
      it_nfitem_aux     type table of y_j1bnflin,
      it_zlest0014_f    type table of y_zlest0014_f,
      it_linhas         like standard table of wa_linhas,
      nf_aviso_receb    type table of zmmt_ee_zgr_docs with header line.


*----------------------------------------------------------------------*
* Estrutura
*----------------------------------------------------------------------*

data: wk_vttk               type y_vttk,            " Cabeçalho transporte
      wk_lfa1               type y_lfa1,            " Mestre de fornecedores (parte geral)
      wk_remessa_aux        type y_vttp,            " Item de transporte
      wk_aviso_aux          type y_vttp,            " Item de transporte
      wk_0001               type y_0001,            " Margens,
      wk_912                type a912,              " Tolerância
      wk_konp               type konp,              " Aliquota de margens
      wk_0014               type y_zlest0014,       " Informações de Transporte
      wk_zlest0014          type y_zlest0014,       " Informações de Transporte para alteração
      wk_frete_cabec        type y_frete_cabec,     " Protótipo de Campos para Frete de Saida
      wk_coleta             type y_coleta,          " Mestre de Fornecedor - Coleta
      wk_entrega            type y_entrega,         " Mestre de Fornecedor - Entrega
      wk_fatura             type y_vbfa,            " Fluxo de documentos de vendas e distribuição
      wk_nfitem             type y_j1bnflin,        " Partidas individuais da nota fiscal
      wk_header             type y_j_1bnfdoc,       " Cabeçalho da nota fiscal
      wk_item_add           type y_j_1binlin_add,   " Partidas individuais da nota fiscal
      wk_pv                 type y_pv,              " Mestre de fornecedores (parte geral)
      wk_vc                 type y_zsdt0001,        " Informações para preenchimento dos dados de remessa.
      wk_cavcar             type y_zlest0002,       " Controle de veículos
      wk_lips               type y_lips,            " Fornecimento
      wk_mt                 type y_mt,              " Mestre de Fornecedor - MT
      wk_konv               type y_konv,            " Condições (dados de operação)
      wk_konv_aux           type y_konv,            " Condições (dados de operação)
      wk_user               type y_user,            " Usuário de referência para aplicações de Internet
      wk_ar                 type y_ar,              " Documento SD: fornecimento: dados de item
      wk_ekbe               type y_ekbe,          " Histórico para o documento de compra
      wk_ext_header         type y_ext_header,
      wk_matnr              type mara-matnr,
      wk_zlest0014_f        type y_zlest0014_f,
      wa_headerdata         type bapishipmentheader  ,
      wa_headerdataaction   type bapishipmentheaderaction .

data: wk_frete2      type zless0001.

*----------------------------------------------------------------------*
* Ranges
*----------------------------------------------------------------------*
data : begin of rg_placa occurs 0,
        sign       type c,
        option(2)  type c,
        low        type zsdt0001-placa_car1,
        tipo       type c,
       end of rg_placa.

* RANGES: rg_placa FOR zsdt0001-placa_car1.

*----------------------------------------------------------------------*
* Tela de Seleção
*----------------------------------------------------------------------*

selection-screen begin of block b1 with frame title text-000.
parameters: p_tknum  type vttk-tknum     obligatory.
selection-screen end of block b1.


*----------------------------------------------------------------------*
* Iniciar Desenvolvimento
*----------------------------------------------------------------------*
start-of-selection.

  clear: sl_doc_prc.

  perform: z_limpar_campos,           " Limpar Campos
           z_validar_registro_transp, " Validar Registro de Transporte
           z_seleciona_zlest0001.     " Seleciona ZLEST0001

  if wk_vttk-abfer eq c_1 or
     wk_vttk-abfer eq c_3.
    perform z_frete_saida.   " Frete de Saída
  elseif wk_vttk-abfer eq c_2 or
         wk_vttk-abfer eq c_4.
    perform z_frete_entrada. " Frete de Entrada
  endif.

  if not wk_frete_cabec is initial.
    perform: z_verifica_placas   ,
             z_carrega_smartforms. " Carregar Formulário - Smartforms
  else.
    message i000(zles) with text-003.
  endif.

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDAR_REGISTRO_TRANSP
*&---------------------------------------------------------------------*
*       Validar Registro de Transporte
*----------------------------------------------------------------------*
form z_validar_registro_transp .

  clear: wk_vttk, vl_vbeln.

  select
    single tknum  " Nº transporte
           erdat  " Data de criação do registro
           abfer  " Tipo de processamento para transporte
           exti1  " Conhecimento de embarque
           exti2  " Carta Frete
           tdlnr  " Nº do agente de frete
           shtyp  "Tipo de Transporte
         from vttk
            into wk_vttk
              where tknum eq p_tknum and
                    add03 <> '0000000002'.

  if sy-subrc <> 0.
    message s000(zles) with text-002 display like 'E'.
    leave list-processing.
  else.

    condense: wk_vttk-exti1,
              wk_vttk-exti1 no-gaps.


    select single *
    from zlest0014
      into wk_0014
    where tknum      = wk_vttk-tknum     and
          conhec     = wk_vttk-exti1(10) and
          ctafrete   = wk_vttk-exti2(10).

    if sy-subrc is initial and wk_0014-reimp = c_n.
      message s000(zles) with text-013 display like 'E'.
      leave list-processing.
    endif.

* Ordem de vendas de serviço vinculada ao transporte
    select single vbeln
    from   vbak
    into   vl_vbeln
    where  tknum eq p_tknum.

    if sy-subrc <> 0.
      message s000(zles) with text-009 display like 'E'.
      leave list-processing.
    endif.

* Determinação de nota fiscal de serviço
    perform z_busca_dacte.

* Documento de carta frete.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wk_vttk-exti2
      importing
        output = vl_cfrete.

  endif.

endform.                    " Z_VALIDAR_REGISTRO_TRANSP

*&---------------------------------------------------------------------*
*&      Form  Z_FRETE_SAIDA
*&---------------------------------------------------------------------*
*       Frete de Saída
*----------------------------------------------------------------------*
form z_frete_saida .

  perform: z_janela_cabec_frete_saida,
           z_janela_prop_frete_saida,
           z_janela_motor_frete_saida,
           z_janela_contra_frete_saida,
           z_janela_price_frete_saida,
           z_janela_fatura_frete_saida,
           z_janela_total_frete_saida,
           z_janela_assinat_frete_saida.

endform.                    " Z_FRETE_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_FRETE_ENTRADA
*&---------------------------------------------------------------------*
*       Frete de Entrada
*----------------------------------------------------------------------*
form z_frete_entrada .

  perform: z_janela_cabec_frete_entr,
           z_janela_prop_frete_entr,
           z_janela_motor_frete_entr,
           z_janela_contra_frete_entr,
           z_janela_price_frete_entr,
           z_janela_fatura_frete_entr,
           z_janela_total_frete_entr,
           z_janela_assinat_frete_entr.

endform.                    " Z_FRETE_ENTRADA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_zlest0001
*&---------------------------------------------------------------------*
*       Seleciona ZLEST003
*----------------------------------------------------------------------*
form z_seleciona_zlest0001 .

  clear: wk_0001.

endform.                    " Z_SELECIONA_zlest0001

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_CABEC_FRETE_SAIDA
*&---------------------------------------------------------------------*
*       Cabeçalho para Frete de Saída.
*----------------------------------------------------------------------*
form z_janela_cabec_frete_saida .

  data: wa_vbrk type vbrk.

  types: begin of y_vbfa_docm,
          vbeln         type vbfa-vbeln,
          mjahr         type vbfa-mjahr,
          posnn         type vbfa-posnn,
          vbeln_35(35)  type c,
         end of y_vbfa_docm.

  data: tl_fatura type table of y_vbfa.
  data: vl_range_nr type inri-nrrangenr,
        vl_object   type inri-object,
        vl_fatura   type y_vbfa.

  data: ti_vbfap_docm  type table of y_vbfa_docm,
        st_vbfa_docm   type y_vbfa_docm,
        vl_tabix       type sy-tabix,
        vl_mblnr       type mseg-mblnr,
        vg_auart       type auart.

* Número do conhecimento de embarque
  move: vg_conhec to wk_frete_cabec-dacte.


* Campos validos para Cabeçalho - Frete de Saida/Entrada
  perform: z_campos_cabecalho_geral.

* Selecionar remessas referente ao documento de transporte
  select tknum tpnum vbeln
     from vttp
       into table it_remessa
         where tknum eq p_tknum.

  if sy-subrc is initial.

    free: it_remessa_aux.
    it_remessa_aux[] = it_remessa[].
    sort it_remessa_aux by tknum.
    delete adjacent duplicates from it_remessa_aux
                    comparing tknum.

    read table it_remessa_aux into wk_remessa_aux index 1.

    select lifnr
      up to 1 rows
       from vtpa
*      FROM vbpa
       into vg_pc
*      WHERE vbeln EQ wk_remessa_aux-vbeln
       where vbeln eq p_tknum
       and parvw eq c_pc.
    endselect.

* Dados de Coleta
    perform: z_dados_cabec_coleta.

    select kunnr
      up to 1 rows
       from vtpa
*      FROM vbpa
       into vg_lr
*      WHERE vbeln EQ wk_remessa_aux-vbeln
       where vbeln eq p_tknum
        and parvw  eq c_lr.
    endselect.

* Dados de Cabeçalho - Entrega
    perform: z_dados_cabec_entrega.

    free: it_remessa_aux.
    it_remessa_aux[] = it_remessa[].
    sort it_remessa_aux by vbeln.
    delete adjacent duplicates from it_remessa_aux
                    comparing vbeln.

    if ( wk_vttk-shtyp eq 'Z020' ).

* Obter o documento de material
      refresh ti_vbfap_docm.
      select vbeln mjahr posnn vbeln into table ti_vbfap_docm
               from vbfa
          for all entries in it_remessa
              where vbelv = it_remessa-vbeln
                and vbtyp_v = 'J'       "Entrega
                and vbtyp_n = 'R'.      "Movimento de Mercadoria
* Eliminar o documento de material estornado.
      loop at ti_vbfap_docm into st_vbfa_docm.
        vl_tabix = sy-tabix.
        clear vl_mblnr.
        select mblnr into vl_mblnr
                     from mseg
                up to 1 rows
                    where smbln = st_vbfa_docm-vbeln
                      and sjahr = st_vbfa_docm-mjahr
                      and bwart = '864'.   "Movimento de estorno
        endselect.
        if sy-subrc eq 0.
          delete ti_vbfap_docm index vl_tabix.
        else.
* Agrupar o documento de material e o ano em um mesmo campo.
          concatenate st_vbfa_docm-vbeln_35
                      st_vbfa_docm-mjahr
                      into st_vbfa_docm-vbeln_35.
          modify ti_vbfap_docm from st_vbfa_docm index vl_tabix
                                    transporting vbeln_35.
        endif.
      endloop.
* Obter a nota fiscal
      refresh it_nfitem.
      select docnum itmnum
               reftyp refkey
        into table it_nfitem
        from j_1bnflin
        for all entries in ti_vbfap_docm
        where refkey = ti_vbfap_docm-vbeln_35
          and refitm = ti_vbfap_docm-posnn
          and reftyp = 'MD'.      "Documento de material

      if sy-subrc is initial.
        perform: z_funcao_document_read.
      endif. " Fechar condição para Função

    elseif wk_vttk-shtyp eq 'Z026'.

      perform: z_funcao_document_avulso using sl_doc_prc.

    else.
*************************************************************************
*************************************************************************
***** Alterei para pegar faturas e seus estornos para pegar somente o
***** ultimo lançamento

      select vbelv vbeln posnn
             vbtyp_n vbtyp_v
        from vbfa
          into table it_fatura2
            for all entries in it_remessa_aux
              where vbelv eq it_remessa_aux-vbeln
                and vbtyp_v eq c_j
                and vbtyp_n eq c_m.

      sort it_fatura by vbeln posnn.

      loop at it_fatura2 into vl_fatura.

        select single * into wa_vbrk
          from vbrk
         where vbeln = vl_fatura-vbeln
           and fksto = 'X'.

        if sy-subrc is initial.
          continue.
        endif.

        if vl_fatura-vbtyp_n eq c_m.
          "Fatura
          if wk_vttk-shtyp eq 'Z020'.
            append vl_fatura to it_fatura.
          elseif wk_vttk-shtyp eq 'Z018'.

            select single v~auart
              into vg_auart
              from vbrp as p
             inner join vbak as v on v~vbeln eq p~aubel
             where p~vbeln eq vl_fatura-vbeln
               and p~posnr eq vl_fatura-posnn.

            if vg_auart eq 'ZREM'.
              append vl_fatura to it_fatura.
            endif.
          else.
            append vl_fatura to it_fatura.
          endif.
        endif.
      endloop.
*************************************************************************
*************************************************************************

      if sy-subrc is initial.

        perform z_tratar_campo_vbeln.

        tl_fatura[] = it_fatura[].
        sort tl_fatura by refkey.
        delete adjacent duplicates from tl_fatura comparing refkey.

        select docnum itmnum
               reftyp refkey
          from j_1bnflin
            into table it_nfitem
              for all entries in tl_fatura
              where refkey eq tl_fatura-refkey
              and reftyp eq c_bi.

        if sy-subrc is initial.
          perform: z_funcao_document_read.
        endif. " Fechar condição para Função
      endif. " Fechar condição FATURA
    endif. " Fechar condição para REMESSA

  endif.

endform.                    " Z_JANELA_CABEC_FRETE_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_PROP_FRETE_SAIDA
*&---------------------------------------------------------------------*
*       Janela Proprietário - Frete de Saída
*----------------------------------------------------------------------*
form z_janela_prop_frete_saida .

  data: tl_fatura         type table of y_vbfa,
        tl_ordem          type table of y_ordem.

* Janela Proprietario - Geral - Frete de Entrada/Saida.
  perform: z_prop_geral.

  if wk_vttk-shtyp eq 'Z020'.

    select vbeln posnr
           vgbel vgtyp into table it_ordem
      from lips
       for all entries in it_remessa
     where vbeln = it_remessa-vbeln.

    if sy-subrc is initial.

      tl_ordem[] = it_ordem[].
      sort tl_ordem by vgbel.
      delete adjacent duplicates from tl_ordem comparing vgbel.

      select
        tp_movimento
        vbeln
        placa_cav
        placa_car1
        placa_car2
        placa_car3
        motorista
        from zsdt0001
          into table it_vc
            for all entries in tl_ordem
              where doc_rem eq tl_ordem-vbeln
                and tp_movimento eq 'S'.
* Dados complementares para Proprietario do Veiculo
      perform: z_dados_proprietario_compl.

    endif.

  else.

    if not it_fatura[] is initial.
      tl_fatura[] = it_fatura[].
      sort tl_fatura by vbelv posnn.
      delete adjacent duplicates from tl_fatura comparing vbelv posnn.

      select vbeln posnr
             vgbel vgtyp
        from lips
         into table it_ordem
           for all entries in it_fatura
             where vbeln eq it_fatura-vbelv
*           WHERE vbeln EQ it_fatura-vbeln
*             AND posnr EQ it_fatura-posnn
               and vgtyp eq c_c.

      if sy-subrc is initial.

        tl_ordem[] = it_ordem[].
        sort tl_ordem by vgbel.
        delete adjacent duplicates from tl_ordem comparing vgbel.

* Buscar Placa do Veiculo
        select
          tp_movimento
          vbeln
          placa_cav
          placa_car1
          placa_car2
          placa_car3
          motorista
          from zsdt0001
            into table it_vc
              for all entries in tl_ordem
                where doc_rem eq tl_ordem-vbeln
                  and tp_movimento eq 'S'.

*     IF not sy-subrc IS INITIAL.
*     Obter informações do cavalo e carreta a partir do documento de transporte.
*     ENDIF. " Fechar condição IT_VC

* Dados complementares para Proprietario do Veiculo
        perform: z_dados_proprietario_compl.

      endif. " Fechar condição IT_VC


    endif. " Fechar condição IT_FATURA
  endif.

endform.                    " Z_JANELA_PROP_FRETE_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_MOTOR_FRETE_SAIDA
*&---------------------------------------------------------------------*
*       Janela Motorista - Frete de Saída
*----------------------------------------------------------------------*
form z_janela_motor_frete_saida .

* Dados de Motorista - Informações Gerais
  perform: z_dados_motorista_geral.

endform.                    " Z_JANELA_MOTOR_FRETE_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_CONTRA_FRETE_SAIDA
*&---------------------------------------------------------------------*
*       Janela Contrato - Frete de Saída
*----------------------------------------------------------------------*
form z_janela_contra_frete_saida .

* Dados de Contrato
  perform: z_dados_contrato_geral.

endform.                    " Z_JANELA_CONTRA_FRETE_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_PRICE_FRETE_SAIDA
*&---------------------------------------------------------------------*
*       Janela Price - Frete de Saída
*----------------------------------------------------------------------*
form z_janela_price_frete_saida .

* Dados de Price
  perform: z_dados_price_geral.

endform.                    " Z_JANELA_PRICE_FRETE_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_FATURA_FRETE_SAIDA
*&---------------------------------------------------------------------*
*       Janela Fatura - Frete de Saída
*----------------------------------------------------------------------*
form z_janela_fatura_frete_saida .

  move: wk_lfa1-name1       to wk_frete_cabec-fatura,       " Fatura
        vg_frete_contratado to wk_frete_cabec-valor_fatura. " Valor da Fatura

endform.                    " Z_JANELA_FATURA_FRETE_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_TOTAL_FRETE_SAIDA
*&---------------------------------------------------------------------*
*       Janela Total - Frete de Saída
*----------------------------------------------------------------------*
form z_janela_total_frete_saida .

  move: wk_lfa1-name1 to wk_frete_cabec-recebimento. " Recebi(emos)

endform.                    " Z_JANELA_TOTAL_FRETE_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_ASSINAT_FRETE_SAIDA
*&---------------------------------------------------------------------*
*       Janela Assinatura - Frete de Saída
*----------------------------------------------------------------------*
form z_janela_assinat_frete_saida .

* Dados para Assinatura
  perform: z_dados_assinatura_geral.

endform.                    " Z_JANELA_ASSINAT_FRETE_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_CABEC_FRETE_ENTR
*&---------------------------------------------------------------------*
*       Janela Cabeçalho - Frete de Entrada
*----------------------------------------------------------------------*
form z_janela_cabec_frete_entr .

  data: tl_ar   type table of y_ar,
        tl_ekbe type table of y_ekbe.

  "move: wk_vttk-exti1 to wk_frete_cabec-dacte.
* Número do conhecimento de embarque
  move: vg_conhec to wk_frete_cabec-dacte.

* Campos validos para Cabeçalho - Frete de Saida/Entrada
  perform z_campos_cabecalho_geral.

* Selecionar remessas referente ao documento de transporte
  select tknum tpnum vbeln
     from vttp
     into table it_aviso
     where tknum eq p_tknum.

  if sy-subrc is initial.

    free: it_aviso_aux.
    it_aviso_aux[] = it_aviso[].
    sort it_aviso_aux by tknum.
    delete adjacent duplicates from it_aviso_aux comparing tknum.

    read table it_aviso_aux into wk_aviso_aux index 1.

    if ( wk_vttk-abfer eq 1 ) or ( wk_vttk-abfer eq 3 ).

      select lifnr
        up to 1 rows
         from vbpa
         into vg_pc
         where vbeln eq wk_aviso_aux-vbeln
          and parvw eq c_pc.
      endselect.

      select kunnr
        up to 1 rows
         from vbpa
          into vg_lr
              where vbeln eq wk_aviso_aux-vbeln
                and parvw eq c_lr.
      endselect.

    elseif ( wk_vttk-abfer eq 2 ) or ( wk_vttk-abfer eq 4 ).

      select lifnr
        up to 1 rows
         from vtpa
         into vg_pc
         where vbeln eq p_tknum
           and parvw eq c_pc.
      endselect.

      select kunnr
        up to 1 rows
         from vtpa
          into vg_lr
              where vbeln eq p_tknum
                and parvw eq c_lr.
      endselect.

    endif.

* Dados de Coleta
    perform: z_dados_cabec_coleta.

* Dados de Cabeçalho - Entrega
    perform z_dados_cabec_entrega.

    free: it_aviso_aux.
    it_aviso_aux[] = it_aviso[].
    sort it_aviso_aux by vbeln.
    delete adjacent duplicates from it_aviso_aux
                    comparing vbeln.

    select * into table nf_aviso_receb
      from zmmt_ee_zgr_docs
       for all entries in it_aviso_aux
     where av_vbeln eq it_aviso_aux-vbeln.

    if sy-subrc is initial.

      select docnum itmnum
             reftyp refkey
        from j_1bnflin
          into table it_nfitem
            for all entries in nf_aviso_receb
              where docnum eq nf_aviso_receb-docnum.

      if sy-subrc is initial.
        perform z_funcao_document_read.
      endif. " Fechar condição para Função

    endif. " Fechar condição IT_AR
  endif. " Fechar condição IT_AVISO

endform.                    " Z_JANELA_CABEC_FRETE_ENTR

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_PROP_FRETE_ENTR
*&---------------------------------------------------------------------*
*       Janela Proprietário - Frete de Entrada
*----------------------------------------------------------------------*
form z_janela_prop_frete_entr .

  data: tl_aviso   type table of y_vttp,
        tl_pedido  type table of y_pedido.

* Janela Proprietario - Geral - Frete de Entrada/Saida.
  perform: z_prop_geral.

  if not it_aviso[] is initial.
    tl_aviso[] = it_aviso[].
    sort tl_aviso by vbeln.
    delete adjacent duplicates from tl_aviso comparing vbeln.

    select vgbel
           vgpos
       from lips
         into table it_pedido
           for all entries in tl_aviso
             where vbeln eq tl_aviso-vbeln
               and vgtyp eq c_v.

    if sy-subrc is initial.
      tl_pedido[] = it_pedido[].
      sort tl_pedido by vgbel.
      delete adjacent duplicates from tl_pedido comparing vgbel.

* Buscar Placa do Veiculo
      select
        tp_movimento
        vbeln
        placa_cav
        placa_car1
        placa_car2
        placa_car3
        motorista
        from zsdt0001
          into table it_vc
            for all entries in it_pedido
              where tp_movimento eq c_e
                and vbeln        eq it_pedido-vgbel.

      if sy-subrc is initial.

* Dados complementares para Proprietario do Veiculo
        perform: z_dados_proprietario_compl.

      endif. " Fechar condição IT_VC
    endif. " Fechar condição IT_PEDIDO
  endif. " Fechar condição IT_EKBE

endform.                    " Z_JANELA_PROP_FRETE_ENTR

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_MOTOR_FRETE_ENTR
*&---------------------------------------------------------------------*
*       Janela Motorista - Frete de Entrada
*----------------------------------------------------------------------*
form z_janela_motor_frete_entr .

* Dados de Motorista - Informações Gerais
  perform: z_dados_motorista_geral.

endform.                    " Z_JANELA_MOTOR_FRETE_ENTR

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_CONTRA_FRETE_ENTR
*&---------------------------------------------------------------------*
*       Janela Contrato - Frete de Entrada
*----------------------------------------------------------------------*
form z_janela_contra_frete_entr .

* Dados de Contrato
  perform: z_dados_contrato_geral.

endform.                    " Z_JANELA_CONTRA_FRETE_ENTR

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_PRICE_FRETE_ENTR
*&---------------------------------------------------------------------*
*       Janela Price - Frete de Entrada
*----------------------------------------------------------------------*
form z_janela_price_frete_entr .

* Dados de Price
  perform: z_dados_price_geral_entr.

endform.                    " Z_JANELA_PRICE_FRETE_ENTR

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_FATURA_FRETE_ENTR
*&---------------------------------------------------------------------*
*       Janela Fatura - Frete de Entrada
*----------------------------------------------------------------------*
form z_janela_fatura_frete_entr .

  move: wk_lfa1-name1       to wk_frete_cabec-fatura,       " Fatura
        vg_frete_contratado to wk_frete_cabec-valor_fatura. " Valor da Fatura

endform.                    " Z_JANELA_FATURA_FRETE_ENTR

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_TOTAL_FRETE_ENTR
*&---------------------------------------------------------------------*
*       Janela Total - Frete de Entrada
*----------------------------------------------------------------------*
form z_janela_total_frete_entr .

  move: wk_lfa1-name1 to wk_frete_cabec-recebimento. " Recebi(emos)

endform.                    " Z_JANELA_TOTAL_FRETE_ENTR

*&---------------------------------------------------------------------*
*&      Form  Z_JANELA_ASSINAT_FRETE_ENTR
*&---------------------------------------------------------------------*
*       Janela Assinatura - Frete de Entrada
*----------------------------------------------------------------------*
form z_janela_assinat_frete_entr .

* Dados para Assinatura
  perform: z_dados_assinatura_geral.

endform.                    " Z_JANELA_ASSINAT_FRETE_ENTR

*&---------------------------------------------------------------------*
*&      Form  Z_LIMPAR_CAMPOS
*&---------------------------------------------------------------------*
*       Limpar Campos de Estruturas e Tabelas Internas
*----------------------------------------------------------------------*
form z_limpar_campos .

  free: it_remessa,
        it_aviso,
        it_fatura,
        it_nfitem,
        it_item_add,
        it_ordem,
        it_vc,
        it_cavcar,
        it_vfkp,
        it_vfsi,
        it_konv,
        it_ar,
        it_ekbe,
        it_lips.

  clear: wk_vttk,
         wk_lfa1,
         wk_0001,
         wk_0014,
         wk_frete_cabec,
         wk_coleta,
         wk_entrega,
         wk_fatura,
         wk_nfitem,
         wk_header,
         wk_item_add,
         wk_pv,
         wk_vc,
         wk_cavcar,
         wk_mt,
         wk_konv,
         wk_user,
         wk_ar,
         wk_ekbe.

  clear: vg_conhec,
         vg_lifnr_pv,
         vg_lifnr_mt,
         vg_pc,
         vg_lr,
         vg_erdat.

endform.                    " Z_LIMPAR_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  Z_TRATAR_CAMPO_VBELN
*&---------------------------------------------------------------------*
*       Tratar Campo VBELN
*----------------------------------------------------------------------*
form z_tratar_campo_vbeln .

  loop at it_fatura into wk_fatura.

    move: wk_fatura-vbeln to wk_fatura-refkey.
    modify it_fatura from wk_fatura index sy-tabix.

    clear: wk_fatura.
  endloop.

endform.                    " Z_TRATAR_CAMPO_VBELN

*&---------------------------------------------------------------------*
*&      Form  Z_FUNCAO_DOCUMENT_READ
*&---------------------------------------------------------------------*
*       Função Document Read
*----------------------------------------------------------------------*
form z_funcao_document_read .

  data: vl_tabix type sy-tabix,
        "Início - Marcus Scauri - 12.08.2010
        ls_doc_item type y_doc_item.
  "Fim    - Marcus Scauri - 12.08.2010

  free: it_nfitem_aux.
  it_nfitem_aux[] = it_nfitem[].
  sort it_nfitem_aux by docnum.
  delete adjacent duplicates from it_nfitem_aux
                  comparing docnum.

  clear: vg_nfenum.

  loop at it_nfitem_aux into wk_nfitem.
    vl_tabix = sy-tabix.

    refresh: it_doc_refer_msg,it_doc_item,
             it_doc_item_tax, it_doc_header_msg,
             it_doc_refer_msg.
    clear: wk_header.

    move: wk_nfitem-docnum to vg_docmun.

    call function 'J_1B_NF_DOCUMENT_READ'
      exporting
        doc_number     = vg_docmun
      importing
        doc_header     = wk_header
      tables
        doc_partner    = it_doc_partner
        doc_item       = it_doc_item
        doc_item_tax   = it_doc_item_tax
        doc_header_msg = it_doc_header_msg
        doc_refer_msg  = it_doc_refer_msg
      exceptions
        others         = 1.

    if sy-subrc <> 0.
* VAZIO
    endif. " Fechar Retorno da Função

* Nota Fiscal Eletrônica
    if not wk_header-nfenum is initial.

      if vg_nfenum is initial.
        vg_nfenum = wk_header-nfenum.
      else.
* Mover apenas até 2 NFs para o campo, senão não mover
* pois não caberá na Impressão
        if vl_tabix le 2.
          concatenate: vg_nfenum
                       wk_header-nfenum
                       into vg_nfenum separated by c_bar.
        endif.
      endif. " Fechar condição para NFENUM
    else.
      if vg_nfenum is initial.
        vg_nfenum = wk_header-nfnum.
      else.
* Mover apenas até 2 NFs para o campo, senão não mover
* pois não caberá na Impressão
        if vl_tabix le 2.
          concatenate: vg_nfenum
                       wk_header-nfnum
                       into vg_nfenum separated by c_bar.
        endif.
      endif. " Fechar condição para NFENUM

    endif. " Verificar se NFe contem valor

    wk_frete_cabec-peso_saida = wk_frete_cabec-peso_saida + wk_header-brgew.

***Início Marcus Scauri
*    CLEAR wk_ext_header.
*    REFRESH: it_nf_item, it_nf_item_tax, it_item_add.
*
*    CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION'
*      EXPORTING
*        nf_header   = wk_header
*      IMPORTING
*        ext_header  = wk_ext_header
*      TABLES
*        nf_item     = it_nf_item
*        nf_item_tax = it_nf_item_tax
*        ext_item    = it_item_add.
*
*    LOOP AT it_item_add INTO wk_item_add.
*
*      wk_frete_cabec-vl_n_fiscal = wk_frete_cabec-vl_n_fiscal + wk_item_add-nftot.
*
*      CLEAR: wk_item_add.
*    ENDLOOP.
    loop at it_doc_item into ls_doc_item.
      wk_frete_cabec-vl_n_fiscal = wk_frete_cabec-vl_n_fiscal + ls_doc_item-netwr.
    endloop.
***Fim Marcus Scauri

    clear: wk_nfitem.

  endloop.

  if not vg_nfenum is initial.
    move: vg_nfenum  to wk_frete_cabec-n_fiscal.
  endif.

endform.                    " Z_FUNCAO_DOCUMENT_READ

*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_PLACA
*&---------------------------------------------------------------------*
*       Obter Placas
*----------------------------------------------------------------------*
form z_obter_placa .

  loop at it_vc into wk_vc.

* Primeira Placa
    move: c_i             to rg_placa-sign,
          c_eq            to rg_placa-option,
          wk_vc-placa_cav to rg_placa-low,
          '1'             to rg_placa-tipo.

    append rg_placa.

* Segunda Placa
    move: c_i              to rg_placa-sign,
          c_eq             to rg_placa-option,
          wk_vc-placa_car1 to rg_placa-low,
          '2'             to rg_placa-tipo.

    append rg_placa.

* Terceira Placa
    move: c_i              to rg_placa-sign,
          c_eq             to rg_placa-option,
          wk_vc-placa_car2 to rg_placa-low,
          '2'             to rg_placa-tipo.

    append rg_placa.

* Quarta Placa
    move: c_i              to rg_placa-sign,
          c_eq             to rg_placa-option,
          wk_vc-placa_car3 to rg_placa-low,
          '2'             to rg_placa-tipo.

    append rg_placa.

    clear: wk_vc.
  endloop.

  sort rg_placa by low.
  delete rg_placa where low = space.
  delete adjacent duplicates from rg_placa comparing low.


endform.                    " Z_OBTER_PLACA

*&---------------------------------------------------------------------*
*&      Form  Z_CARREGA_PLACAS
*&---------------------------------------------------------------------*
*       Carregar a Placa do Veiculo
*----------------------------------------------------------------------*
form z_carrega_placas .

  data : vl_cont type i.

  clear vl_cont.

  loop at rg_placa.

    clear: wk_cavcar.

    case rg_placa-tipo.

      when '1'.

        read table it_cavcar into wk_cavcar
                             with key pc_veiculo = rg_placa-low
                             binary search.

        if sy-subrc is initial.

          move: wk_cavcar-pc_veiculo to wk_frete_cabec-placa_cav. " Placa Cavalo

          concatenate: wk_cavcar-cd_cidade
                       c_trac
                       wk_cavcar-cd_uf
                       into wk_frete_cabec-cid_cav.

        endif. " Placa Cavalo

      when '2'.

        read table it_cavcar into wk_cavcar
                             with key pc_veiculo = rg_placa-low
                              binary search.

        if sy-subrc is initial.

          vl_cont = vl_cont + 1.

          case vl_cont.

            when 1.

              move: wk_cavcar-pc_veiculo to wk_frete_cabec-placa_carr. " Placa Carreta

              concatenate: wk_cavcar-cd_cidade
                           c_trac
                           wk_cavcar-cd_uf
                           into wk_frete_cabec-cid_car.
            when others.

              move: wk_cavcar-pc_veiculo to wk_frete_cabec-placa_carr1. " Placa Carreta

              concatenate: wk_cavcar-cd_cidade
                           c_trac
                           wk_cavcar-cd_uf
                           into wk_frete_cabec-cid_car1.
          endcase.

        endif. " Placa Carreta

    endcase.

  endloop.

endform.                    " Z_CARREGA_PLACAS

*&---------------------------------------------------------------------*
*&      Form  Z_VALORES_CONDICAO_KONV
*&---------------------------------------------------------------------*
*       Valores de Condição
*----------------------------------------------------------------------*
form z_valores_condicao_konv .

  data: vl_frete_contratado2(11) type c,          " Frete Contratado
        vl_inss                  type kwert,      " INSS a reter
        vl_sest_snat             type kwert,      " SEST/SNAT
        vl_irrf                  type kwert,      " IRRF
        vl_seguro                type kwert,      " Seguro
        vl_adiantamento          type kwert,      " Adiantamento
        vl_arktx                 type lips-arktx. " Texto

  clear: vl_frete_contratado2,
         vl_inss,
         vl_sest_snat,
         vl_irrf,
         vl_seguro,
         vl_adiantamento,
         vl_arktx.

  loop at it_konv into wk_konv.

    read table it_lips index 1.

    if sy-subrc is initial.
      move: it_lips-arktx to vl_arktx.
    endif.

*    CASE wk_konv_aux-kschl.
    case wk_konv-kschl.
      when c_zfre or c_zlot. " Frete Contratado

        vg_frete_contratado = vg_frete_contratado + wk_konv-kbetr.
*       vg_frete_contratado = vg_frete_contratado + wk_konv-kbetr.

      when c_zins. " INSS a reter

        vl_inss = vl_inss + ( - ( wk_konv-kwert ) ).

      when c_zset. " SEST/SNAT

        vl_sest_snat = vl_sest_snat + ( - ( wk_konv-kwert ) ) .

      when c_zirf. " IRRF

        vl_irrf =  vl_irrf + ( - ( wk_konv-kwert ) ).

      when c_zseg. " Seguro

        vl_seguro = vl_seguro + wk_konv-kwert.

*BBKO/Vagner Santos - Início da alteração - 14.10.2010
* Somar o valor do IOF ao valor do seguro.
      when c_ziof. " IOF
        vl_seguro = vl_seguro + wk_konv-kwert.
*BBKO/Vagner Santos - Fim da alteração - 14.10.2010
      when c_zadm. " Adiantamento

        vl_adiantamento = vl_adiantamento + wk_konv-kwert.

      when c_zped. " Pedágio
        if not wk_konv-kwert is initial.
          wk_frete_cabec-pedagio = 'Pedágio pago com cartão Repom.'.
        endif.
      when others.

    endcase.

    clear: wk_konv_aux.

  endloop.

  move: vl_inss to wk_frete_cabec-inss.
  move: vl_sest_snat to wk_frete_cabec-sest_snat.
  move: vl_irrf to wk_frete_cabec-irrf.
  move: vl_seguro to wk_frete_cabec-seguro.
  move: vl_adiantamento to wk_frete_cabec-adiantamento.


* Frete Contratado
  move: vg_frete_contratado to vl_frete_contratado2.

  translate  vl_frete_contratado2 using ',..,'.

  concatenate: vl_frete_contratado2
               text-008
               '('
               vl_arktx
               ')'
               into wk_frete_cabec-frete_contratado
               separated by space.

endform.                    " Z_VALORES_CONDICAO_KONV

*&---------------------------------------------------------------------*
*&      Form  Z_CAMPOS_CABECALHO_GERAL
*&---------------------------------------------------------------------*
*       Campos Cabeçalho Geral - Valido tanto para Entrada quanto Saida
*----------------------------------------------------------------------*
form z_campos_cabecalho_geral .

  concatenate: wk_vttk-erdat+6(2)
               c_bar
               wk_vttk-erdat+4(2)
               c_bar
               wk_vttk-erdat+0(4)
               into vg_erdat.

* Data de Criação
  move: vg_erdat to wk_frete_cabec-data_cria.

  if not wk_vttk-tdlnr is initial.

* Buscar Emitente
    select single
           lifnr name1 ort01 ort02
           pstlz regio stras stcd1 stcd3
         from lfa1
          into wk_lfa1
            where lifnr eq wk_vttk-tdlnr.

    if sy-subrc is initial.

* Emitente 1
      concatenate: wk_lfa1-name1
                   wk_lfa1-stcd1
                   wk_lfa1-stcd3
                   into wk_frete_cabec-emitente_1
                   separated by space.

* Emitente 2
      concatenate: wk_lfa1-stras
                   wk_lfa1-ort02
                   wk_lfa1-pstlz
                   wk_lfa1-ort01
                   wk_lfa1-regio
                   into wk_frete_cabec-emitente_2
                   separated by space.

    endif. " Fechar condição da LFA1
  endif. " Fechar condição para LFA1-LIFNN

endform.                    " Z_CAMPOS_CABECALHO_GERAL

*&---------------------------------------------------------------------*
*&      Form  Z_DADOS_CABEC_COLETA
*&---------------------------------------------------------------------*
*       Dados de Cabeçalho - Coleta
*----------------------------------------------------------------------*
form z_dados_cabec_coleta .

  if not vg_pc is initial.

    select single
           lifnr name1
           ort01 regio stcd1 stcd2
        from lfa1
          into wk_coleta
            where lifnr eq vg_pc.

    concatenate: wk_coleta-ort01
                 c_trac
                 wk_coleta-regio
                 into wk_frete_cabec-local_col.

    move: wk_coleta-name1 to wk_frete_cabec-coleta.

    if not wk_coleta-stcd1 is initial.
      move wk_coleta-stcd1 to wk_frete_cabec-cnpj_col.
    else.
      move wk_coleta-stcd2 to wk_frete_cabec-cnpj_col.
    endif.

  endif. " Fechar condição de VG_PC

endform.                    " Z_DADOS_CABEC_COLETA

*&---------------------------------------------------------------------*
*&      Form  Z_DADOS_CABEC_ENTREGA
*&---------------------------------------------------------------------*
*       Dados de Cabeçalho - Entrega
*----------------------------------------------------------------------*
form z_dados_cabec_entrega .

  if not vg_lr is initial.

    select single
            lifnr name1
            ort01 regio stcd1 stcd2
     from kna1
     into wk_entrega
     where kunnr eq vg_lr.

    concatenate: wk_entrega-ort01
                 c_trac
                 wk_entrega-regio
                 into wk_frete_cabec-local_entr.

    move: wk_entrega-name1 to wk_frete_cabec-entrega.

    if not wk_entrega-stcd1 is initial.
      move wk_entrega-stcd1 to wk_frete_cabec-cnpj_entr.
    else.
      move wk_entrega-stcd2 to wk_frete_cabec-cnpj_entr.
    endif.

  endif. " Fechar condição de VG_LR

endform.                    " Z_DADOS_CABEC_ENTREGA

*&---------------------------------------------------------------------*
*&      Form  Z_TRASF_VGPOS_P_VBELP
*&---------------------------------------------------------------------*
*       Transferir o VGPOS para o VBELP
*----------------------------------------------------------------------*
form z_trasf_vgpos_p_vbelp .

  loop at it_ar into wk_ar.
    move: wk_ar-vgpos+1(5) to wk_ar-ebelp.
    modify it_ar from wk_ar index sy-tabix.
    clear: wk_ar.
  endloop.

endform.                    " Z_TRASF_VGPOS_P_VBELP

*&---------------------------------------------------------------------*
*&      Form  Z_UNIR_GJAHR_BELNR
*&---------------------------------------------------------------------*
*       Unir GJAHR e BELNR -> REFKEY
*----------------------------------------------------------------------*
form z_unir_gjahr_belnr .

  loop at it_ekbe into wk_ekbe.

    concatenate: wk_ekbe-belnr
                 wk_ekbe-gjahr
                 into wk_ekbe-refkey.

    modify it_ekbe from wk_ekbe index sy-tabix.
    clear: wk_ekbe.
  endloop.


endform.                    " Z_UNIR_GJAHR_BELNR

*&---------------------------------------------------------------------*
*&      Form  Z_PROP_GERAL
*&---------------------------------------------------------------------*
*       Janela Proprietario Geral - Frete de Entrada/Saida
*----------------------------------------------------------------------*
form z_prop_geral .

  if not wk_vttk-tknum is initial.

    select lifnr
      up to 1 rows
       from vtpa
       into vg_lifnr_pv
        where vbeln eq wk_vttk-tknum
          and parvw eq c_pv.
    endselect.

    if not vg_lifnr_pv is initial.

      select single
          lifnr name1 ort01 ort02 pstlz regio
          stras ktokk stcd1 stcd2 stcd3 stkzn
        from lfa1
         into wk_pv
           where lifnr eq vg_lifnr_pv.

      if sy-subrc is initial.
        move: wk_pv-name1 to wk_frete_cabec-proprietario,   " Proprietário
              wk_pv-stras to wk_frete_cabec-endereco_prop,  " Endereço
              wk_pv-ort01 to wk_frete_cabec-cidade_prop,    " Cidade
              wk_pv-regio to wk_frete_cabec-uf_cidade_prop. " UF

        if wk_pv-stkzn is initial. " Pessoa Juridica
          move: wk_pv-stcd1 to wk_frete_cabec-cnjp_cpf_prop. " CNPJ
        else.
          move: wk_pv-stcd2 to wk_frete_cabec-cnjp_cpf_prop. " CPF
        endif.
      endif.

    endif. " Fechar VG_LIFNR_PV
  endif. " Fechar VTPA

endform.                    " Z_PROP_GERAL

*&---------------------------------------------------------------------*
*&      Form  Z_DADOS_PROPRIETARIO_COMPL
*&---------------------------------------------------------------------*
*       Dados do Proprietario - Informações Complementares
*----------------------------------------------------------------------*
form z_dados_proprietario_compl .

  perform: z_obter_placa.

  if not rg_placa[] is initial.

* Buscar dados de Veiculo
    select pc_veiculo cd_cidade
           cd_uf ct_veiculo
      from zlest0002
        into table it_cavcar
      for all entries in rg_placa
         where pc_veiculo eq rg_placa-low.

    if sy-subrc is initial.
      sort it_cavcar by ct_veiculo.
      perform: z_carrega_placas. " Carregar a Placa do Veiculo

    endif. " Fechar ZLEST0002
  endif. " Fehcar condição RG_PLACA

  move: wk_pv-name1 to wk_frete_cabec-proprietario,   " Proprietário
        wk_pv-stras to wk_frete_cabec-endereco_prop,  " Endereço
        wk_pv-ort01 to wk_frete_cabec-cidade_prop,    " Cidade
        wk_pv-regio to wk_frete_cabec-uf_cidade_prop. " UF

  if wk_pv-stkzn is initial. " Pessoa Juridica

    move: wk_pv-stcd1 to wk_frete_cabec-cnjp_cpf_prop. " CNPJ

  else.

    move: wk_pv-stcd2 to wk_frete_cabec-cnjp_cpf_prop. " CPF

  endif.

endform.                    " Z_DADOS_PROPRIETARIO_COMPL

*&---------------------------------------------------------------------*
*&      Form  Z_DADOS_MOTORISTA_GERAL
*&---------------------------------------------------------------------*
*       Dados de Motorista
*----------------------------------------------------------------------*
form z_dados_motorista_geral .

  data: vl_eikto  type lfb1-eikto,
        vl_zsabe  type lfb1-zsabe.

  if not wk_vttk-tknum is initial.

    select lifnr
      up to 1 rows
      from vtpa
        into vg_lifnr_mt
        where vbeln eq wk_vttk-tknum
          and parvw =  c_mt.
    endselect.

    if sy-subrc is initial.

*      SELECT SINGLE
*             lifnr name1 ort01 regio
*             stras bbbnr bbsnr ktokk
*             stcd1 telf1 stcd4
*       FROM lfa1
*         INTO wk_mt
*           WHERE lifnr = vg_lifnr_mt.

      select single
             lifnr name1 ort01 regio
             stras bbbnr bbsnr ktokk
             stcd3 telf1 stcd4
       from lfa1
         into wk_mt
           where lifnr = vg_lifnr_mt.

      select single eikto zsabe
        from lfb1
        into (vl_eikto, vl_zsabe)
      where  lifnr eq vg_lifnr_mt.
*       AND  bukrs EQ p_bukrs.

      move: wk_mt-name1 to wk_frete_cabec-motorista,       " Motorista
            wk_mt-stras to wk_frete_cabec-endereco_motor,  " Endereço
            wk_mt-ort01 to wk_frete_cabec-cidade_motor,    " Cidade
            wk_mt-regio to wk_frete_cabec-uf_cidade_motor, " UF Cidade
*            wk_mt-stcd1 TO wk_frete_cabec-rg_nr_motor,    " RG Nr
            wk_mt-stcd3 to wk_frete_cabec-rg_nr_motor,     " RG Nr
            vl_eikto    to wk_frete_cabec-uf_rg_nr_motor,  " UF  Rg Nr
            wk_mt-stcd4 to wk_frete_cabec-habilitacao,     " Habilitação
            vl_zsabe to wk_frete_cabec-uf_habilitacao,     " UF Habilitação
            wk_mt-telf1 to wk_frete_cabec-fone_motor.      " Fone

    endif. " Fechar condição VG_LIFNR_MT
  endif. " Fechar condição VTTK-TKNUM

endform.                    " Z_DADOS_MOTORISTA_GERAL

*&---------------------------------------------------------------------*
*&      Form  Z_DADOS_CONTRATO_GERAL
*&---------------------------------------------------------------------*
*       Dados de Contrato - Informações Gerais
*----------------------------------------------------------------------*
form z_dados_contrato_geral .

  move: wk_pv-name1 to wk_frete_cabec-contratado,     " Contrato
        wk_pv-stras to wk_frete_cabec-endereco_contr, " Endereço Contrato
        wk_pv-ort01 to wk_frete_cabec-cidade_contr,   " Cidade Contrato
        wk_pv-regio to wk_frete_cabec-uf_contr.       " UF Contrato

  if wk_pv-ktokk eq c_zffj. " Pessoa Juridica

    move: wk_pv-stcd1 to wk_frete_cabec-cnpj_cpf_contr. " CNPJ Contrato

  elseif wk_pv-ktokk eq c_zfff. " Pessoa Fisica

    move: wk_pv-stcd2 to wk_frete_cabec-cnpj_cpf_contr. " CPF Contrato

  endif.

endform.                    " Z_DADOS_CONTRATO_GERAL
*&---------------------------------------------------------------------*
*&      Form  Z_DADOS_PRICE_GERAL
*&---------------------------------------------------------------------*
*       Dados de Price
*----------------------------------------------------------------------*
form z_dados_price_geral .

  data: tl_vfkp type table of y_vfkp,
        tl_vfsi type table of y_vfsi.

  data: vl_tolerancia(6) type c.

  select knumv refty rebel
     from vfkp
      into table it_vfkp
        where refty eq c_8 and
              rebel eq p_tknum.

  if sy-subrc is initial.
    tl_vfkp[] = it_vfkp[].
    sort tl_vfkp by knumv.
    delete adjacent duplicates from tl_vfkp comparing knumv.

    select knumv kposn vbeln
           posnr kzwi1 kzwi2
      from vfsi
        into table it_vfsi
          for all entries in tl_vfkp
            where knumv eq tl_vfkp-knumv.

    if sy-subrc is initial.
      tl_vfsi[] =  it_vfsi[].
      sort  tl_vfsi by knumv kposn.
      delete adjacent duplicates from tl_vfsi comparing knumv kposn.

* Documento SD: fornecimento
      select vbeln posnr matnr arktx
         from lips
         into table it_lips
         for all entries in it_remessa
         where vbeln eq it_remessa-vbeln.

      if sy-subrc = 0.
        sort it_lips by vbeln posnr.
      endif.

      free tl_vfsi.
      tl_vfsi[] =  it_vfsi[].
      sort  tl_vfsi by knumv.
      delete adjacent duplicates from tl_vfsi comparing knumv.

* Tabela de Condições
      SELECT FROM V_KONV FIELDS KNUMV , KPOSN , KSCHL , KBETR , KWERT , KINAK FOR ALL ENTRIES IN @TL_VFSI WHERE KNUMV EQ @TL_VFSI-KNUMV AND KINAK NE 'Y' INTO TABLE @IT_KONV .

      if sy-subrc is initial.
        perform: z_valores_condicao_konv.
      endif. " Fechar KONV

    endif. " Fechar VFSI
  endif. " Fechar VFKP

*  IF NOT wk_0001 IS INITIAL.

  read table it_lips index 1.

  wk_matnr = it_lips-matnr.

* Determinação da margem de tolerância
  select single *
  into          wk_912
  from          a912
  where         kappl = c_kappl   and
                kschl = c_kschl   and
                matnr = wk_matnr  and
                datab <= sy-datum and
                datbi >= sy-datum.

  if sy-subrc is initial.

* Valor da condição ZMRG - Margem de tolerância
    select single *
    into          wk_konp
    from          konp
    where         knumh = wk_912-knumh.

    clear: vl_tolerancia.
    wk_konp-kbetr = wk_konp-kbetr / 10.
    move: wk_konp-kbetr to vl_tolerancia.

    concatenate: vl_tolerancia
                 text-005
                 vl_tolerancia
                 text-006
                 into wk_frete_cabec-tolerancia
                 separated by space.

  endif.

*  ENDIF. " Fechar WK_0001

endform.                    " Z_DADOS_PRICE_GERAL

*&---------------------------------------------------------------------*
*&      Form  Z_DADOS_ASSINATURA_GERAL
*&---------------------------------------------------------------------*
*       Dados para Assinatura
*----------------------------------------------------------------------*
form z_dados_assinatura_geral .

  select single useralias
      from usrefus
        into wk_user
          where bname eq sy-uname.

  if sy-subrc is initial.
    move: wk_user-useralias to wk_frete_cabec-nome_assinatura.
  endif. " Fechar condição USREFUS



  "IF rb_imp EQ c_flag.

* Carta Frete
  move: vl_cfrete to wk_frete_cabec-carta_frete.

* Gravar Dados na tabela ZLEST0014
  "PERFORM: z_gravar_dados_zlest0014. " Comentado para somente gravar quando for Imprimir e não visualizar.

  "ELSE.
*
** Carta Frete
*    MOVE: vl_cfrete TO wk_frete_cabec-carta_frete.
*
  "ENDIF. " Fechar condição de Impressão - Marcado com X ou Não

endform.                    " Z_DADOS_ASSINATURA_GERAL

*&---------------------------------------------------------------------*
*&      Form  Z_GRAVAR_DADOS_ZLEST0014
*&---------------------------------------------------------------------*
*       Gravar Dados na tabela ZLEST0014
*----------------------------------------------------------------------*
form z_gravar_dados_zlest0014 .

*BBKO/Vagner Santos - Início da alteração - 15.10.2010
  data: li_subrc     type i,
        actafrete    type zlest0014-actafrete,
        v1_tknum     type zlest0014-tknum,
        v2_conhec    type zlest0014-conhec,
        v3_ctafrete  type zlest0014-ctafrete,
        v4_actafrete type zlest0014-actafrete,
        u_ctafrete   type zlest0014-ctafrete,
        e_tknum      type vttp-tknum,
        e_vbeln      type vttp-vbeln,
        e_werks      type lips-werks,
        v_ctfrete    type zlest0014-actafrete.


*BBKO/Vagner Santos - Fim da alteração - 15.10.2010

  " CSB 26.05.2011
* Busca Filial conforme solicitação Marcos Santos
*Selecionar na tabela VTTP-TKNUM=
*doc.transporte pegar o valor do campo VTTP-VBELN
* ir na tabela LIPS-VBELN= VTTP-VBELN  pegar o valor do campo LIPS-WERKS

* Empresa

  select single tknum vbeln
  into (e_tknum, e_vbeln)
  from vttp
  where tknum = p_tknum.


  select single werks
  into  e_werks
  from lips
  where vbeln = e_vbeln.


  " Alteração feita por Camila Brand 25.05.2011.
  select count( * )
  from zlest0014
  where tknum = p_tknum
  and  conhec =  vg_conhec.

  if not sy-subrc is initial.

    " Log geração de carta frete 20.05.2011
    move: sy-mandt   to wk_0014-mandt,
          p_tknum    to wk_0014-tknum,
          vg_conhec  to wk_0014-conhec,
          vl_cfrete  to wk_0014-ctafrete,
          sy-datum   to wk_0014-data,
          sy-uzeit   to wk_0014-hora,
          sy-uname   to wk_0014-usuario,
          c_n        to wk_0014-reimp,
          c_n        to wk_0014-alterar,
          actafrete  to wk_0014-actafrete,
          e_werks    to wk_0014-werks.

*    MODIFY zlest0014 FROM wk_0014.

*BBKO/Vagner Santos - Início da alteração - 15.10.2010
*    li_subrc = sy-subrc.

    " Fecha registos que estão permitindo alteração
**    SELECT *
**    INTO wk_zlest0014
**    FROM zlest0014
**    WHERE tknum EQ p_tknum
**    AND  conhec EQ vg_conhec
**    AND  ctafrete NE vl_cfrete.
**
**      MOVE: c_n TO wk_zlest0014-alterar,
**            c_n TO wk_zlest0014-reimp.
**
**      MODIFY zlest0014 FROM wk_zlest0014.
**    ENDSELECT.


    "IF li_subrc IS INITIAL.
*   Verifica se o conhecimento está na tabela VTTK
*    SELECT COUNT( * )
*      FROM vttk
*     WHERE tknum = p_tknum
*       AND exti1 = space.
*
*    IF sy-subrc IS INITIAL.
**     Atualiza campo user...
*      UPDATE vttk
*         SET exti1 = vg_conhec
*       WHERE tknum = p_tknum.
*      "li_subrc = sy-subrc.
*    ENDIF.
    "ENDIF.
*BBKO/Vagner Santos - Fim da alteração - 15.10.2010

    "IF sy-subrc IS INITIAL.
*    COMMIT WORK.
    "ELSE.
    "ROLLBACK WORK.
    "ENDIF.

  else.

    clear: actafrete, u_ctafrete.

    "   Pega a Carta frete do primeiro registro
    select  tknum conhec ctafrete actafrete
      into table it_zlest0014_f
      from zlest0014
      where tknum =  p_tknum
      and  conhec =  vg_conhec.

    read table it_zlest0014_f into wk_zlest0014_f with key ctafrete = vl_cfrete.
    if sy-subrc is initial.
      u_ctafrete = wk_zlest0014_f-actafrete.
    else.
      read table it_zlest0014_f into wk_zlest0014_f with key actafrete = space.
      v_ctfrete = wk_zlest0014_f-ctafrete.
      do.
        read table it_zlest0014_f into wk_zlest0014_f with key actafrete = v_ctfrete.
        if sy-subrc is initial.
          v_ctfrete = wk_zlest0014_f-ctafrete.
        else.
          exit.
        endif.
      enddo.

      u_ctafrete = wk_zlest0014_f-ctafrete.

    endif.
    "Log geração de carta frete 20.05.2011
    move: sy-mandt   to wk_0014-mandt,
          p_tknum    to wk_0014-tknum,
          vg_conhec  to wk_0014-conhec,
          vl_cfrete  to wk_0014-ctafrete,
          sy-datum   to wk_0014-data,
          sy-uzeit   to wk_0014-hora,
          sy-uname   to wk_0014-usuario,
          c_n        to wk_0014-reimp,
          c_n        to wk_0014-alterar,
          u_ctafrete to wk_0014-actafrete,
          e_werks    to wk_0014-werks.


*    MODIFY zlest0014 FROM wk_0014.

    " Fecha registos que estão permitindo alteração
*    SELECT *
*    INTO wk_zlest0014
*    FROM zlest0014
*    WHERE tknum EQ p_tknum
*    AND  conhec EQ vg_conhec
*    AND  ctafrete NE vl_cfrete.
*
*      MOVE: c_n TO wk_zlest0014-alterar,
*            c_n TO wk_zlest0014-reimp.
*
*      MODIFY zlest0014 FROM wk_zlest0014.
*
*    ENDSELECT.

*BBKO/Vagner Santos - Fim da alteração - 15.10.2010

*    COMMIT WORK.
  endif.

  if not wk_0014 is initial.
    export wk_0014 to memory id 'ZCARTA'.
  endif.

endform.                    " Z_GRAVAR_DADOS_ZLEST0014

*&---------------------------------------------------------------------*
*&      Form  Z_CARREGA_SMARTFORMS
*&---------------------------------------------------------------------*
*       Carregar Formulário - Smartforms
*----------------------------------------------------------------------*
form z_carrega_smartforms .

  data : fm_name    type rs38l_fnam,
         ls_docu    type ssfcrespd ,
         ls_info    type ssfcrescl ,
         ls_option  type ssfcresop ,
         it_return  type table of bapiret2,
         ver        type char1            .

  clear: wk_frete2.
  move-corresponding: wk_frete_cabec to wk_frete2.

  perform z_gravar_dados_zlest0014.

  call function 'SSF_FUNCTION_MODULE_NAME'                  "#EC *
    exporting
      formname           = 'ZLESSM0001'
    importing
      fm_name            = fm_name
    exceptions
      no_form            = 1
      no_function_module = 2
      others             = 3.

  call function fm_name
    exporting
      wk_frete             = wk_frete2
    importing
      document_output_info = ls_docu
      job_output_info      = ls_info
      job_output_options   = ls_option
    exceptions
      formatting_error     = 1
      internal_error       = 2
      send_error           = 3
      user_canceled        = 4
      others               = 5.

  check 1 = 1.

  " Garavar dados somente quando clica em Imprimir.
  if ls_info-spoolids[] is not initial.
    import ver from memory id 'ZVER'.
    free memory id 'ZVER'.
    if ver is initial.
      perform z_gravar_dados_zlest0014_2.
    endif.
  endif.

endform.                    " Z_CARREGA_SMARTFORMS
*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_PLACAS
*&---------------------------------------------------------------------*
*       Verifica se as placas estão preenchidas
*----------------------------------------------------------------------*
form z_verifica_placas .

  types: begin of ty_placa,
          placa(07)  type c,
          cidade(25) type c,
         end of ty_placa.

  data: vl_name  type thead-tdname,
        ls_placa type ty_placa,
        ls_lines type tline.

  data: lt_lines type table of tline.

  data: vl_text1 type vttk-text1,
        vl_text2 type vttk-text2,
        vl_text3 type vttk-text3.

  constants: c_z001(4) type c value 'Z001',
             c_z002(4) type c value 'Z002',
             c_z003(4) type c value 'Z003',
             c_vttk(4) type c value 'VTTK'.

***Verifica se a placa_cav está vazia
  if wk_frete_cabec-placa_cav is initial.

    clear vl_text1.

    select single text1
      from vttk
      into vl_text1
      where tknum = p_tknum.

    if sy-subrc = 0.

      clear ls_placa.
      ls_placa-placa   = vl_text1(7).
      ls_placa-cidade  = vl_text1+8.

      wk_frete_cabec-placa_cav = ls_placa-placa.
      wk_frete_cabec-cid_cav   = ls_placa-cidade.

    endif.

  endif.

***Verifica se a placa_carr está vazia
  if wk_frete_cabec-placa_carr is initial.

    clear vl_text2.

    select single text2
      from vttk
      into vl_text2
      where tknum = p_tknum.

    if sy-subrc = 0.

      clear ls_placa.
      ls_placa-placa   = vl_text2(7).
      ls_placa-cidade  = vl_text2+8.

      wk_frete_cabec-placa_carr = ls_placa-placa.
      wk_frete_cabec-cid_car    = ls_placa-cidade.

    endif.

  endif.

***Verifica se a placa_carr está vazia
  if wk_frete_cabec-placa_carr1 is initial.

    clear vl_text3.

    select single text3
      from vttk
      into vl_text3
      where tknum = p_tknum.

    if sy-subrc = 0.

      clear ls_placa.
      ls_placa-placa   = vl_text3(7).
      ls_placa-cidade  = vl_text3+8.

      wk_frete_cabec-placa_carr1 = ls_placa-placa.
      wk_frete_cabec-cid_car1    = ls_placa-cidade.

    endif.

  endif.

endform.                    " Z_VERIFICA_PLACAS

*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_DACTE                                            *
*&---------------------------------------------------------------------*
*                           Busca Número Dacte                         *
*----------------------------------------------------------------------*
form z_busca_dacte.

  data: vl_vbeln_fa type vbak-vbeln,
        vl_posnn    type vbfa-posnn,
        sl_doc      type j_1bnfdoc ,
        vl_docnum   type j_1bnfdoc-docnum,
        p_parid     type j_1bparid,
        p_partyp    type j_1bpartyp,
        p_emite     type char01,
        p_tknum     type tknum.

* Fluxo de documento
  select single a~vbeln a~posnn
    from vbfa as a
    inner join vbrk as b
      on a~vbeln eq b~vbeln
    into (vl_vbeln_fa, vl_posnn)
  where  a~vbelv   eq vl_vbeln
    and  a~vbtyp_n eq 'M'
    and  a~vbtyp_v eq 'C'
    and  b~fksto   ne 'X' AND B~DRAFT = SPACE .

  if sy-subrc is initial.

* Itens do documento de nota fiscal de serviço
    select single docnum
      from j_1bnflin
      into vl_docnum
    where  refkey eq vl_vbeln_fa
      and  refitm eq vl_posnn.

    if sy-subrc is initial.

* Documento de nota fiscal de serviço
      select single *
      from   j_1bnfdoc
      into   sl_doc
      where  docnum  eq vl_docnum
      and    cancel  ne 'X'.

      if sy-subrc is initial.
        move-corresponding sl_doc to sl_doc_prc.
      endif.

      call function 'Z_REMETENTE_MERCADORIA_CTE'
        exporting
          p_docnum = vl_docnum
        changing
          p_bukrs  = sl_doc-bukrs
          p_parid  = p_parid
          p_partyp = p_partyp
          p_tknum  = p_tknum.

      call function 'Z_CIOT_EMPRESA_PARCEIRO'
        exporting
          p_empresa    = sl_doc-bukrs
          p_parid      = p_parid
          p_partyp     = p_partyp
          p_dt_posicao = sl_doc-docdat
          p_tknum      = p_tknum
        importing
          p_emite      = p_emite.

      if not p_emite is initial.
        message s000(zles) with text-014 display like 'E'.
        leave list-processing.
      endif.

      if not sy-subrc is initial.
        message s000(zles) with text-010 display like 'E'.
        leave list-processing.
      endif.

      check sy-subrc is initial.

      if sl_doc-nfe is initial.
        vg_conhec = sl_doc-nfnum.
      else.
        if sl_doc-docstat <> 1.
          message s000(zles) with text-011 display like 'E'.
          leave list-processing.
        else.
          vg_conhec = sl_doc-nfenum.
        endif.
      endif.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = vg_conhec
        importing
          output = vg_conhec.

      move: vg_conhec to wk_frete_cabec-dacte.

    else.
      message s000(zles) with text-010 display like 'E'.
      leave list-processing.
    endif.

  else.
    message s000(zles) with text-012 display like 'E'.
    leave list-processing.
  endif.

endform.                    " Z_BUSCA_DACTE
*&---------------------------------------------------------------------*
*&      Form  PRINT_LPT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form print_lpt1 .
  data: vl_index         type sy-tabix,
        vl_peso_saida    type string.

  clear: wk_frete2.
  move-corresponding: wk_frete_cabec to wk_frete2.


  wa_linhas-linha = 'N'.

  append wa_linhas to it_linhas.
  wa_linhas-linha = 'O'.

  append wa_linhas to it_linhas.
  wa_linhas-linha = 'S2'.
  "Cabeçalho

  concatenate  'A50,280,0,3,1,1,N,EMITENTE_1'  wk_frete2-emitente_1 '"'  into wa_linhas-linha .
  append wa_linhas to it_linhas.

  concatenate 'A51,320,0,3,1,1,N,EMITENTE_2:' wk_frete2-emitente_2   into wa_linhas-linha .
  append wa_linhas to it_linhas.

  concatenate 'A52,320,0,3,1,1,N,COLETA : ' wk_frete2-coleta ' LOCAL_COL:'wk_frete2-local_col  'CNPJ_COL: 'wk_frete2-cnpj_col into wa_linhas-linha .
  append wa_linhas to it_linhas.

  concatenate  'A53,320,0,3,1,1,N,DACTE: '  wk_frete2-dacte ' ENTREGA: ' wk_frete2-entrega  ' LOCAL_ENTR: 'wk_frete2-local_entr ' CNPJ_ENTR:'wk_frete2-cnpj_entr  into wa_linhas-linha .
  append wa_linhas to it_linhas.

  "wk_frete2-PESO_SAIDA

  "concatenate 'Dt Criação:' wk_frete2-DATA_CRIA '  NFISCAL:'  wk_frete2-N_FISCAL '  PESO_SAIDA:'wk_frete2-PESO_SAIDA ' VL_N_FISCAL:' wk_frete2-VL_N_FISCAL  into wa_linhas-linha .
  "APPEND wa_linhas TO it_linhas.





*
*
*  "Proprietario
*  wk_frete2-PROPRIETARIO
*  wk_frete2-ENDERECO_PROP
*  wk_frete2-CIDADE_PROP
*  wk_frete2-UF_CIDADE_PROP
*  wk_frete2-CNJP_CPF_PROP.
*  wk_frete2-PIS_NIT_N
*
*  wk_frete2-PLACA_CAV
*  wk_frete2-CID_CAV
*
*  wk_frete2-PLACA_CARR
*  wk_frete2-CID_CAR
*
*  wk_frete2-PLACA_CARR1
*  wk_frete2-CID_CAR1
*
*  "Motorista
*  wk_frete2-MOTORISTA
*  wk_frete2-ENDERECO_MOTOR
*  wk_frete2-CIDADE_MOTOR
*  wk_frete2-UF_CIDADE_MOTOR
*  wk_frete2-RG_NR_MOTOR
*  wk_frete2-UF_RG_NR_MOTOR
*  wk_frete2-HABILITACAO
*  wk_frete2-UF_HABILITACAO
*  wk_frete2-FONE_MOTOR
*
*  "CONTRATADO
*  wk_frete2-CONTRATADO
*  wk_frete2-VLR_BRUTO
*  wk_frete2-INSS
*  wk_frete2-SEST_SNAT
*  wk_frete2-IRRF
*  wk_frete2-SEGURO
*  wk_frete2-SUB_TOTAL
*  wk_frete2-ADIANTAMENTO
*  wk_frete2-LIQUIDO
*  wk_frete2-TOLERANCIA
*
*  "Fatura
*  wk_frete2-FATURA
*  wk_frete2-VALOR_FATURA.

  wa_linhas-linha = 'P1'.
  append wa_linhas to it_linhas.

  call function 'WS_DOWNLOAD'
    exporting
      filename = 'C:\LPT1:'
      filetype = 'ASC'
    tables
      data_tab = it_linhas
    exceptions
      others   = 4.



*wk_frete2-CIDADE_CONTR
*wk_frete2-ENDERECO_CONTR
*wk_frete2-UF_CONTR
*wk_frete2-CNPJ_CPF_CONTR
*wk_frete2-FRETE_CONTRATADO
*wk_frete2-RECEBIMENTO
*wk_frete2-NOME_ASSINATURA
*wk_frete2-CARTA_FRETE
*wk_frete2-PEDAGIO






*  wa_linhas-linha = 'N'.
*  APPEND wa_linhas TO it_linhas.
*  wa_linhas-linha = 'O'.
*  APPEND wa_linhas TO it_linhas.
*  wa_linhas-linha = 'S2'.
*  APPEND wa_linhas TO it_linhas.
*  wa_linhas-linha = 'Q614,27'.
*  APPEND wa_linhas TO it_linhas.
*  wa_linhas-linha = 'q784'.
*  APPEND wa_linhas TO it_linhas.
*
*  CONCATENATE 'B50,40,0,3,2,7,150,B,"' wa_mard-matnr '"' INTO wa_linhas-linha.
*  APPEND wa_linhas TO it_linhas.
*
*  CONCATENATE 'A50,280,0,3,1,1,N,"Material: ' wa_mard-maktx '"' INTO wa_linhas-linha.
*  APPEND wa_linhas TO it_linhas.
*
*  SELECT SINGLE name1 INTO centro
*    FROM t001w
*   WHERE werks EQ wa_mard-werks.
*
*  CONCATENATE 'A50,320,0,3,1,1,N,"Centro: ' wa_mard-werks ' - ' centro '"' INTO wa_linhas-linha.
*  APPEND wa_linhas TO it_linhas.
*
*  CONCATENATE 'A50,360,0,3,1,1,N,"Deposito: ' wa_mard-lgort '"' INTO wa_linhas-linha.
*  APPEND wa_linhas TO it_linhas.
*
*  CONCATENATE 'A50,400,0,3,1,1,N,"Endereco: ' wa_mard-lgpbe '"' INTO wa_linhas-linha.
*  APPEND wa_linhas TO it_linhas.
*
*  wa_linhas-linha = 'P1'.
*  APPEND wa_linhas TO it_linhas.

endform.                    " PRINT_LPT1
*&---------------------------------------------------------------------*
*&      Form  Z_GRAVAR_DADOS_ZLEST0014_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_gravar_dados_zlest0014_2 .

  data: wk_0014              type zlest0014               ,
        wk_zlest0014         type zlest0014               ,
        wa_headerdata        type bapishipmentheader      ,
        wa_headerdataaction  type bapishipmentheaderaction,
        it_return            type table of bapiret2       .

  import wk_0014 from memory id 'ZCARTA'.
  free memory id 'ZCARTA'.

  if not wk_0014 is initial.

    modify zlest0014 from wk_0014.
    "   Fecha registos que estão permitindo alteração
    select *
    into wk_zlest0014
    from zlest0014
    where tknum    eq wk_0014-tknum
    and   conhec   eq wk_0014-conhec
    and   ctafrete ne wk_0014-ctafrete.

      move: 'N' to wk_zlest0014-alterar,
            'N' to wk_zlest0014-reimp.

      modify zlest0014 from wk_zlest0014.
    endselect.

**   Verifica se o conhecimento está na tabela VTTK
*    SELECT COUNT( * )
*      FROM VTTK
*     WHERE TKNUM = WK_0014-TKNUM
*       AND EXTI1 = SPACE.
*
*    IF SY-SUBRC IS INITIAL.
**     Atualiza campo user...
*      UPDATE VTTK
*         SET EXTI1 = WK_0014-CONHEC
*       WHERE TKNUM = WK_0014-TKNUM.
*      "li_subrc = sy-subrc.
*    ENDIF.

    " Fecha transporte se for impressa carta frete

    wa_headerdata-shipment_num             = p_tknum.
    wa_headerdata-external_id_1            = wk_0014-conhec.
    wa_headerdata-status_shpmnt_end        = 'X'.
    wa_headerdataaction-external_id_1      = 'C'.
    wa_headerdataaction-status_shpmnt_end  = 'C'.

    call function 'BAPI_SHIPMENT_CHANGE'
      exporting
        headerdata       = wa_headerdata
        headerdataaction = wa_headerdataaction
      tables
        return           = it_return.

    " Comita
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.

  endif.

endform.                    " Z_GRAVAR_DADOS_ZLEST0014_2

*&---------------------------------------------------------------------*
*&      Form  Z_FUNCAO_DOCUMENT_AVULSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form z_funcao_document_avulso  using i_doc type j_1bnfdoc.

  data: it_notas type table of zcte_info_nota with header line.

  select * into table it_notas
    from zcte_info_nota
   where docnum eq i_doc-docnum.

  clear: wk_header, vg_nfenum.

  loop at it_notas.
    if vg_nfenum is initial.
      vg_nfenum = it_notas-numero.
    else.
      concatenate: vg_nfenum it_notas-numero into vg_nfenum separated by c_bar.
    endif.

    wk_frete_cabec-peso_saida  = wk_frete_cabec-peso_saida  + it_notas-quantidade.
    wk_frete_cabec-vl_n_fiscal = wk_frete_cabec-vl_n_fiscal + it_notas-vl_nota_fiscal.

    if not vg_nfenum is initial.
      move: vg_nfenum  to wk_frete_cabec-n_fiscal.
    endif.
  endloop.

endform.                    " Z_FUNCAO_DOCUMENT_AVULSO

*&---------------------------------------------------------------------*
*&      Form  Z_DADOS_PRICE_GERAL_ENTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form z_dados_price_geral_entr .

  data: tl_vfkp type table of y_vfkp,
        tl_vfsi type table of y_vfsi.

  data: vl_tolerancia(6) type c.

  select knumv refty rebel
     from vfkp
      into table it_vfkp
        where refty eq c_8 and
              rebel eq p_tknum.

  if sy-subrc is initial.
    tl_vfkp[] = it_vfkp[].
    sort tl_vfkp by knumv.
    delete adjacent duplicates from tl_vfkp comparing knumv.

    select knumv kposn vbeln
           posnr kzwi1 kzwi2
      from vfsi
        into table it_vfsi
          for all entries in tl_vfkp
            where knumv eq tl_vfkp-knumv.

    if sy-subrc is initial.
      tl_vfsi[] =  it_vfsi[].
      sort  tl_vfsi by knumv kposn.
      delete adjacent duplicates from tl_vfsi comparing knumv kposn.

* Documento SD: fornecimento
      select vbeln posnr matnr arktx
         from lips
         into table it_lips
         for all entries in it_aviso
         where vbeln eq it_aviso-vbeln.

      if sy-subrc = 0.
        sort it_lips by vbeln posnr.
      endif.

      free tl_vfsi.
      tl_vfsi[] =  it_vfsi[].
      sort  tl_vfsi by knumv.
      delete adjacent duplicates from tl_vfsi comparing knumv.

* Tabela de Condições
      SELECT FROM V_KONV FIELDS KNUMV , KPOSN , KSCHL , KBETR , KWERT , KINAK FOR ALL ENTRIES IN @TL_VFSI WHERE KNUMV EQ @TL_VFSI-KNUMV AND KINAK NE 'Y' INTO TABLE @IT_KONV .

      if sy-subrc is initial.
        perform: z_valores_condicao_konv.
      endif. " Fechar KONV

    endif. " Fechar VFSI
  endif. " Fechar VFKP

*  IF NOT wk_0001 IS INITIAL.

  read table it_lips index 1.

  wk_matnr = it_lips-matnr.

* Determinação da margem de tolerância
  select single *
  into          wk_912
  from          a912
  where         kappl = c_kappl   and
                kschl = c_kschl   and
                matnr = wk_matnr  and
                datab <= sy-datum and
                datbi >= sy-datum.

  if sy-subrc is initial.

* Valor da condição ZMRG - Margem de tolerância
    select single *
    into          wk_konp
    from          konp
    where         knumh = wk_912-knumh.

    clear: vl_tolerancia.
    wk_konp-kbetr = wk_konp-kbetr / 10.
    move: wk_konp-kbetr to vl_tolerancia.

    concatenate: vl_tolerancia
                 text-005
                 vl_tolerancia
                 text-006
                 into wk_frete_cabec-tolerancia
                 separated by space.

  endif.

endform.                    " Z_DADOS_PRICE_GERAL_ENTR
