TYPE-POOL zftte .

TYPES: BEGIN OF zftte_eventos,
         icone    TYPE char4,
         type     TYPE bapi_mtype,
         msg_text TYPE c LENGTH 300,
         msg_id   TYPE msgid,
         msg_no   TYPE symsgno,
         msg_var1 LIKE balm-msgv1,
         msg_var2 LIKE balm-msgv2,
         msg_var3 LIKE balm-msgv3,
         msg_var4 LIKE balm-msgv4,
       END OF zftte_eventos,

       BEGIN OF zftte_dados,
         tdlnr               TYPE vttk-tdlnr,                  "Fornecedor
         name1               TYPE lfa1-name1,                  "Nome do fornecedor
         status              TYPE char30,
         shtyp               TYPE vttk-shtyp,                  "Tipo de transporte
         exti1               TYPE vttk-exti1,                  "Conhecimento de embarque
         exti2               TYPE vttk-exti2,                  "Carta Frete
         vsart               TYPE vttk-vsart,                  "Tipo de expedição
         tknum               TYPE zlest0032-tknum,             "Nr. doc. transporte
         fknum               TYPE zlest0032-fknum,             "Nr. custo de frete
         budat               TYPE vfkk-budat,                  "Data de Lançamento Contábil
         bukrs               TYPE zlest0034-bukrs,             "Empresa do custo de frete
         werks               TYPE zlest0034-werks,             "centro do custo de frete
         waers               TYPE zlest0034-waers,             "Moeda do custo de frete
         kbetr                TYPE zlest0034-kbetr,             "Montante ou porcentagem da condição
         kurst               TYPE zlest0034-kurst,             "Cotação da Moeda do Documento
         lifnr               TYPE zlest0034-lifnr,             "Emissor da Fatura
         nfenum              TYPE zlest0034-nfenum,            "Nr. NFE
         series              TYPE zlest0034-series,            "Série conhecimento
         zdt_mov             TYPE zlest0034-zdt_mov,           "Data do movimento
         zdt_vencto          TYPE zlest0034-zdt_vencto,        "Data do vencimento
         nr_conhec           TYPE zlest0034-nr_conhec,         "Nr. do conhecimento
         zdt_conhec          TYPE zlest0034-zdt_conhec,        "Data do conhecimento
         zpeso_destino       TYPE zlest0034-zpeso_destino,     "Peso destino
         zdt_chegada         TYPE zlest0034-zdt_chegada,       "Peso chegada
         kalsm                TYPE zlest0034-kalsm,             "Esquema (determinação preço, mensagens, determ.contas, ...)
         iva                 TYPE zlest0034-iva,               "Código do IVA
         nfe                 TYPE zlest0034-nfe,               "Documento Eletrônico
         ebeln               TYPE zlest0034-ebeln,             "Pdido de compra
         ebelp               TYPE zlest0034-ebelp,             "iTEM Pdido de compra
         lblni               TYPE zlest0034-lblni,             "Folha de serviço
         lfgja               TYPE zlest0034-lfgja,             "Folha de serviço (ano)
         zpeso_origem        TYPE zlest0034-zpeso_origem,      "Peso original
         gewei               TYPE zlest0034-gewei,             "Unidade de medida
         dmbtr               TYPE zlest0034-dmbtr,             "Valor
         dmbtr_doc           TYPE zlest0034-dmbtr_doc,         "Valor Documento
         zpeso_diferenca     TYPE zlest0034-zpeso_diferenca,   "Diferença entre origem e destino
         zquebra             TYPE zlest0034-zquebra,           "Quebra do peso
         zperda              TYPE zlest0034-zperda,            "Perda de peso
         zvlr_quebra         TYPE zlest0034-zvlr_quebra,       "Valor da quebra
         zvlr_perda          TYPE zlest0034-zvlr_perda,        "Valor da perda
         zvlr_liq_pagar      TYPE zlest0034-zvlr_liq_pagar,    "Valor líquido a pagar
         matnr               TYPE zlest0034-matnr,             "Código do material
         bvtyp               TYPE zlest0034-bvtyp,             "Banco Pareiro
         maktx               TYPE makt-maktx,                  "Descrição do material
         matns               TYPE zlest0034-matns,             "Código do material Serciço
         makts               TYPE makt-maktx,                  "Descrição do material Serviço
         box                 TYPE char1,                  "checkbox
         re_belnr            TYPE zlest0034-re_belnr,          "Documento de Contas a Pagar MIRO
         re_gjahr            TYPE zlest0034-gjahr,             "ANO de Contas a Pagar MIRO
         re_item             TYPE zlest0034-re_item,           "item de Contas a Pagar MIRO
         en_docnum           TYPE zlest0034-en_docnum,         "Docnum de Entrada fiscal
         regio_emissor       TYPE zlest0034-regio_emissor,
         txjcd_emissor       TYPE zlest0034-txjcd_emissor,
         regio_receptor      TYPE zlest0034-regio_receptor,
         base_icms           TYPE zlest0034-base_icms,
         base_pis            TYPE zlest0034-base_pis,
         base_cofins         TYPE zlest0034-base_cofins,
         rate_icms           TYPE zlest0034-rate_icms,
         rate_pis            TYPE zlest0034-rate_pis,
         rate_cofins         TYPE zlest0034-rate_cofins,
         valor_icms          TYPE zlest0034-valor_icms,
         valor_pis           TYPE zlest0034-valor_pis,
         valor_cofins        TYPE zlest0034-valor_cofins,
         valor_pedagio       TYPE zlest0034-valor_pedagio,
         docnum              TYPE zlest0034-docnum,
         valor_mercadoria    TYPE zlest0034-valor_mercadoria,
         comp_valor          TYPE zlest0042-comp_valor,
         tipo                TYPE zlest0042-tipo,
         comp_docnum         TYPE zlest0042-comp_docnum,
         comp_belnr          TYPE zlest0042-comp_belnr,
         comp_gjahr          TYPE zlest0042-comp_gjahr,
         id_envio            TYPE zlest0042-id_envio,
         cotacao             TYPE zlest0042-cotacao,
         multimodal          TYPE zlest0034-multimodal,
         it_impostos_retidos TYPE zles0043_imp_retidos_t,
         add03               TYPE zlest0032-add03,
         tax_dolar           TYPE zlest0061-tax_dolar,
         vlr_brl             TYPE zlest0061-vlr_brl,
         operacao            TYPE zlest0061-operacao, "-CS2024000597-14.10.2024-#146076-JT-inicio
       END OF zftte_dados,

       BEGIN OF zftte_dados2,
         tdlnr            TYPE vttk-tdlnr,                  "Fornecedor
         name1            TYPE lfa1-name1,                  "Nome do fornecedor
         status           TYPE char30,
         shtyp            TYPE vttk-shtyp,                  "Tipo de transporte
         exti1            TYPE vttk-exti1,                  "Conhecimento de embarque
         exti2            TYPE vttk-exti2,                  "Carta Frete
         vsart            TYPE vttk-vsart,                  "Tipo de expedição
         tknum            TYPE zlest0032-tknum,             "Nr. doc. transporte
         fknum            TYPE zlest0032-fknum,             "Nr. custo de frete
         budat            TYPE vfkk-budat,                  "Data de Lançamento Contábil
         bukrs            TYPE zlest0034-bukrs,             "Empresa do custo de frete
         werks            TYPE zlest0034-werks,             "centro do custo de frete
         waers            TYPE zlest0034-waers,             "Moeda do custo de frete
         kbetr             TYPE zlest0034-kbetr,             "Montante ou porcentagem da condição
         kurst            TYPE zlest0034-kurst,             "Cotação da Moeda do Documento
         lifnr            TYPE zlest0034-lifnr,             "Emissor da Fatura
         nfenum           TYPE zlest0034-nfenum,            "Nr. NFE
         series           TYPE zlest0034-series,            "Série conhecimento
         zdt_mov          TYPE zlest0034-zdt_mov,           "Data do movimento
         zdt_vencto       TYPE zlest0034-zdt_vencto,        "Data do vencimento
         nr_conhec        TYPE zlest0034-nr_conhec,         "Nr. do conhecimento
         zdt_conhec       TYPE zlest0034-zdt_conhec,        "Data do conhecimento
         zpeso_destino    TYPE zlest0034-zpeso_destino,     "Peso destino
         zdt_chegada      TYPE zlest0034-zdt_chegada,       "Peso chegada
         kalsm             TYPE zlest0034-kalsm,             "Esquema (determinação preço, mensagens, determ.contas, ...)
         iva              TYPE zlest0034-iva,               "Código do IVA
         nfe              TYPE zlest0034-nfe,               "Documento Eletrônico
         ebeln            TYPE zlest0034-ebeln,             "Pdido de compra
         ebelp            TYPE zlest0034-ebelp,             "iTEM Pdido de compra
         lblni            TYPE zlest0034-lblni,             "Folha de serviço
         lfgja            TYPE zlest0034-lfgja,             "Folha de serviço (ano)
         zpeso_origem     TYPE zlest0034-zpeso_origem,      "Peso original
         gewei            TYPE zlest0034-gewei,             "Unidade de medida
         dmbtr            TYPE zlest0034-dmbtr,             "Valor
         dmbtr_doc        TYPE zlest0034-dmbtr_doc,         "Valor Documento
         zpeso_diferenca  TYPE zlest0034-zpeso_diferenca,   "Diferença entre origem e destino
         zquebra          TYPE zlest0034-zquebra,           "Quebra do peso
         zperda           TYPE zlest0034-zperda,            "Perda de peso
         zvlr_quebra      TYPE zlest0034-zvlr_quebra,       "Valor da quebra
         zvlr_perda       TYPE zlest0034-zvlr_perda,        "Valor da perda
         zvlr_liq_pagar   TYPE zlest0034-zvlr_liq_pagar,    "Valor líquido a pagar
         matnr            TYPE zlest0034-matnr,             "Código do material
         bvtyp            TYPE zlest0034-bvtyp,             "Banco Pareiro
         maktx            TYPE makt-maktx,                  "Descrição do material
         matns            TYPE zlest0034-matns,             "Código do material Serciço
         makts            TYPE makt-maktx,                  "Descrição do material Serviço
         box              TYPE char1,                  "checkbox
         re_belnr         TYPE zlest0034-re_belnr,          "Documento de Contas a Pagar MIRO
         re_gjahr         TYPE zlest0034-gjahr,             "ANO de Contas a Pagar MIRO
         re_item          TYPE zlest0034-re_item,           "item de Contas a Pagar MIRO
         en_docnum        TYPE zlest0034-en_docnum,         "Docnum de Entrada fiscal
         regio_emissor    TYPE zlest0034-regio_emissor,
         regio_receptor   TYPE zlest0034-regio_receptor,
         base_icms        TYPE zlest0034-base_icms,
         base_pis         TYPE zlest0034-base_pis,
         base_cofins      TYPE zlest0034-base_cofins,
         rate_icms        TYPE zlest0034-rate_icms,
         rate_pis         TYPE zlest0034-rate_pis,
         rate_cofins      TYPE zlest0034-rate_cofins,
         valor_icms       TYPE zlest0034-valor_icms,
         valor_pis        TYPE zlest0034-valor_pis,
         valor_cofins     TYPE zlest0034-valor_cofins,
         valor_pedagio    TYPE zlest0034-valor_pedagio,
         docnum           TYPE zlest0034-docnum,
         valor_mercadoria TYPE zlest0034-valor_mercadoria,
         comp_valor       TYPE zlest0042-comp_valor,
         tipo             TYPE zlest0042-tipo,
         omp_docnum       TYPE zlest0042-comp_docnum,
         comp_belnr       TYPE zlest0042-comp_belnr,
         comp_gjahr       TYPE zlest0042-comp_gjahr,
         id_envio         TYPE zlest0042-id_envio,
         multimodal       TYPE zlest0034-multimodal,
         add03            TYPE zlest0032-add03,
       END OF zftte_dados2.
