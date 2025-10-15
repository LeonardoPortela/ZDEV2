FUNCTION-POOL zrfc_fi.                      "MESSAGE-ID ..

* INCLUDE LZRFC_FID...                       " Local class definition

TYPES: tb_zfie_saida1 TYPE TABLE OF zfie_saida1,
       tb_zfie_saida3 TYPE TABLE OF zfie_saida3,
       tb_zfi_saida   TYPE TABLE OF zaging_siacorp.


TYPES : BEGIN OF ty_bsid,
          bukrs TYPE bsid-bukrs, " Empresa
          kunnr TYPE bsid-kunnr, " Nº cliente 1
          blart TYPE bsid-blart, " Tipo de documento
          budat TYPE bsid-budat, " Data de lançamento no documento
          umskz TYPE bsid-umskz, " Código de Razão Especial
          vbel2 TYPE bsid-vbel2, " Documento de vendas
          zuonr TYPE bsid-zuonr, " Nº atribuição
          belnr TYPE bsid-belnr, " Nº documento de um documento contábil
          bldat TYPE bsid-bldat, " Data no documento
          waers TYPE bsid-waers, " Código da moeda
          xblnr TYPE bsid-xblnr, " Nº documento de referência
          shkzg TYPE bsid-shkzg, " Código débito/crédito
          gsber TYPE bsid-gsber, " Divisão
          dmbtr TYPE bsid-dmbtr, " Montante em moeda interna
          sgtxt TYPE bsid-sgtxt, " Texto do item
          saknr TYPE bsid-saknr, " Nº conta do Razão
          zfbdt TYPE bsid-zfbdt, " Data base para cálculo do vencimento
          zbd1t TYPE bsid-zbd1t, " Dias de desconto 1
          zterm TYPE bsid-zterm, " Chave de condições de pagamento
          dmbe2 TYPE bsid-dmbe2, " Documento de faturamento
          xref1 TYPE bsid-xref1, " Montante na 2ª moeda interna
          xref2 TYPE bsid-xref2, " Chave de referência do parceiro de negócios
          xref3 TYPE bsid-xref3, " Chave de referência do parceiro de negócios
          kidno TYPE bsid-kidno, " Chave de referência para item de doc.
          bupla TYPE bsid-bupla, " Referência de pagamento
          gjahr TYPE bsid-gjahr, " Local de negócios
          vbeln TYPE bsid-vbeln, " Documento de faturamento
          augbl TYPE bsid-augbl, " Nº documento de compensação
          augdt TYPE bsid-augdt, " Data de compensação
          rebzg TYPE bsid-rebzg, " Nº documento da fatura à qual pertence a operação
          hkont TYPE bsid-hkont, " Conta do Razão da contabilidade geral
          umsks TYPE bsid-umsks, " Classe de operação de Razão Especial
          bstat TYPE bsid-bstat, " Classe de operação de Razão Especial
        END OF ty_bsid,

        BEGIN OF ty_bsad,
          bukrs TYPE bsad-bukrs, " Empresa
          kunnr TYPE bsad-kunnr, " Nº cliente 1
          blart TYPE bsad-blart, " Tipo de documento
          augdt TYPE bsad-augdt, " Data de compensação
          umskz TYPE bsad-umskz, " Código de Razão Especial
          vbel2 TYPE bsad-vbel2, " Documento de vendas
          zuonr TYPE bsad-zuonr, " Nº atribuição
          belnr TYPE bsad-belnr, " Nº documento de um documento contábil
          budat TYPE bsad-budat, " Data de lançamento no documento
          bldat TYPE bsad-bldat, " Data no documento
          waers TYPE bsad-waers, " Código da moeda
          xblnr TYPE bsad-xblnr, " Nº documento de referência
          shkzg TYPE bsad-shkzg, " Código débito/crédito
          gsber TYPE bsad-gsber, " Divisão
          dmbtr TYPE bsad-dmbtr, " Montante em moeda interna
          sgtxt TYPE bsad-sgtxt, " Texto do item
          saknr TYPE bsad-saknr, " Nº conta do Razão
          zfbdt TYPE bsad-zfbdt, " Data base para cálculo do vencimento
          zbd1t TYPE bsad-zbd1t, " Dias de desconto 1
          zterm TYPE bsad-zterm, " Chave de condições de pagamento
          vbeln TYPE bsad-vbeln, " Documento de faturamento
          dmbe2 TYPE bsad-dmbe2, " Montante na 2ª moeda interna
          xref1 TYPE bsad-xref1, " Chave de referência do parceiro de negócios
          xref2 TYPE bsad-xref2, " Chave de referência do parceiro de negócios
          xref3 TYPE bsad-xref3, " Chave de referência do parceiro de negócios
          bupla TYPE bsad-bupla, " Local de negócios
          augbl TYPE bsad-augbl, " Nº documento de compensação
          gjahr TYPE bsad-gjahr, " Exercício
          kidno TYPE bsad-kidno, " Referência de pagamento
          hkont TYPE bsad-hkont, " Conta do Razão da contabilidade geral
          umsks TYPE bsad-umsks, " Classe de operação de Razão Especial
        END OF ty_bsad,

        BEGIN OF ty_kna1,
          kunnr TYPE kna1-kunnr, " Nº Fornecedor 1
          ktokd TYPE kna1-ktokd, " Grupo de contas do Fornecedor
        END OF ty_kna1,


        BEGIN OF ty_zsdt0041,
          vbeln         TYPE zsdt0041-vbeln, " Nº Fornecedor 1
          doc_simulacao TYPE zsdt0041-doc_simulacao, " Nº Fornecedor 1
        END OF ty_zsdt0041,

        BEGIN OF ty_vbak,
          vbeln TYPE vbak-vbeln, " Nº Fornecedor 1
          spart TYPE vbak-spart, " Nº FORNECEDOR 1
        END OF ty_vbak,

        BEGIN OF ty_tspat,
          spart TYPE tspat-spart, " Nº Fornecedor 1
          vtext TYPE tspat-vtext, " Nº FORNECEDOR 1
        END OF ty_tspat,

        BEGIN OF ty_lfa1,
          lifnr TYPE lfa1-lifnr, " Nº cliente 1
          ktokk TYPE lfa1-ktokk, " Grupo de contas do cliente
        END OF ty_lfa1,

        BEGIN OF ty_saida,
          vbeln     TYPE vbak-vbeln,
          auart     TYPE vbak-auart,
          bukrs     TYPE bsad-bukrs,
          kunnr     TYPE bsad-kunnr,
          blart     TYPE bsad-blart,
          augdt     TYPE bsad-augdt,
          umskz     TYPE bsad-umskz,
          vbel2     TYPE bsad-vbel2,
          zuonr     TYPE bsad-zuonr,
          belnr     TYPE bsad-belnr,
          budat     TYPE bsad-budat,
          bldat     TYPE bsad-bldat,
          xblnr     TYPE bsad-xblnr,
          shkzg     TYPE bsad-shkzg,
          gsber     TYPE bsad-gsber,
          dmbtr     TYPE bsad-dmbtr,
          sgtxt     TYPE bsad-sgtxt,
          saknr     TYPE bsad-saknr,
          zfbdt     TYPE bsad-zfbdt,
          zfbdt_sum TYPE bsad-zfbdt,
          zbd1t     TYPE bsad-zbd1t,
          zterm     TYPE bsad-zterm,
          dmbe2     TYPE bsad-dmbe2,
          xref1     TYPE bsad-xref1,
          xref2     TYPE bsad-xref2,
          xref3     TYPE bsid-xref3,
          bupla     TYPE bsad-bupla,
          augbl     TYPE bsad-augbl,
          gjahr     TYPE bsad-gjahr,
          kidno     TYPE bsad-kidno,
          hkont     TYPE bsad-hkont,
          usnam     TYPE bkpf-usnam,
          tcode     TYPE bkpf-tcode,
          bktxt     TYPE bkpf-bktxt,
          awkey     TYPE bkpf-awkey,
          waers     TYPE bkpf-waers,
          matnr     TYPE vbrp-matnr,
          arktx     TYPE vbrp-arktx,
          fkimg     TYPE vbrp-fkimg,
          bstkd     TYPE vbkd-bstkd,
          name1     TYPE kna1-name1,
          butxt     TYPE t001-butxt,
          name1_tw  TYPE t001w-name1,
          bezei     TYPE tvakt-bezei,
          docnum    TYPE j_1bnfdoc-docnum,
          xnf       TYPE j_1bnfdoc-nfnum,
          status(4) TYPE c,
          alerta(4) TYPE c,
          xvlrrec   TYPE bsid-dmbtr,
          txt20     TYPE skat-txt20,
        END OF ty_saida.


TYPES: lr_range_bu   TYPE RANGE OF bukrs,
       lr_range_ku   TYPE RANGE OF kunnr,
       lr_range_lf   TYPE RANGE OF lifnr,
       lr_range_doc  TYPE RANGE OF zsdt0041-doc_simulacao,
       lr_range_vend TYPE RANGE OF vbak-vbeln.
DATA: it_range_bukrs TYPE RANGE OF bukrs,
      it_range_kunnr TYPE RANGE OF kunnr,
      it_range_lifnr TYPE RANGE OF lifnr,
      it_range_budat TYPE RANGE OF budat,
      it_range_zfbdt TYPE RANGE OF bsik-zfbdt,
      it_range_doc_simu TYPE RANGE OF zsdt0041-doc_simulacao,
      it_range_vbeln TYPE RANGE OF vbak-vbeln.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA:
  it_bsid          TYPE TABLE OF ty_bsid,
  it_zsdt0041      TYPE TABLE OF ty_zsdt0041,
  it_vbak          TYPE TABLE OF ty_vbak,
  it_tspat         TYPE TABLE OF ty_tspat,

  it_bsad          TYPE TABLE OF ty_bsad,

  it_bsik          TYPE TABLE OF bsik,
  it_bsak          TYPE TABLE OF bsak,

  it_kna1          TYPE TABLE OF ty_kna1,
  it_lfa1          TYPE TABLE OF ty_lfa1,
  it_aging_siacorp TYPE TABLE OF zaging_siacorp.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_cont          TYPE REF TO cl_gui_custom_container,
      wa_alv           TYPE REF TO cl_gui_alv_grid,
      wa_layout        TYPE lvc_s_layo,
      wa_bsid          TYPE ty_bsid,
      wa_bsad          TYPE ty_bsad,

      wa_bsik          TYPE bsik,
      wa_bsak          TYPE bsak,

      wa_bsad_aux      TYPE ty_bsad,
      wa_kna1          TYPE ty_kna1,
      wa_lfa1          TYPE ty_lfa1,
      wa_aging_siacorp TYPE zaging_siacorp.
