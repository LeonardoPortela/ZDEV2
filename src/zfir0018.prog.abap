*----------------------------------------------------------------------*
* Report  ZFIR0018                                                     *
* Descrição  : Consulta fluxo de doc. de uma remessa. Transporte       *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Victor Hugo                            Data: / /        *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*

REPORT zfir0018.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
        lips            , " Documento SD: fornecimento: dados de item
        likp            , " Documento SD: fornecimento: dados de cabeçalho
        vfkp            , " Custos de frete: dados do item
        vbak            .

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
      "Buscar Itens da Remessa
      BEGIN OF ty_lips,
          vbeln      TYPE lips-vbeln,
          vgbel      TYPE lips-vgbel,
          charg      TYPE lips-charg,
          posnr      TYPE lips-posnr,
          werks      TYPE lips-werks,
          brgew      TYPE lips-brgew,
          vgtyp      TYPE lips-vgtyp,
          matnr      TYPE lips-matnr,
      END OF ty_lips,

      "Buscar Remessas
      BEGIN OF ty_likp,
          vbeln      TYPE likp-vbeln,
          lfdat      TYPE likp-lfdat,
          vkorg      TYPE likp-vkorg,
      END OF ty_likp,

      BEGIN OF ty_mkpf,
          mblnr      TYPE mkpf-mblnr,
          mjahr      TYPE mkpf-mjahr,
          le_vbeln   TYPE mkpf-le_vbeln,
      END OF ty_mkpf,

      "Buscar Romaneio de Saida da Remessa
      BEGIN OF ty_zsdt0001,
        vbeln        TYPE zsdt0001-vbeln,
        doc_rem      TYPE zsdt0001-doc_rem,
        nr_romaneio  TYPE zsdt0001-nr_romaneio,
        nr_safra     TYPE zsdt0001-nr_safra,
      END OF ty_zsdt0001,

      "Item de transporte
      BEGIN OF ty_vttp,
          vbeln      TYPE vttp-vbeln,
          tknum      TYPE vttp-tknum,
          tpnum      TYPE vttp-tpnum,
      END OF ty_vttp,  " Fim do bloco ty_vttp


    "Cabeçalho transporte
      BEGIN OF ty_vttk,
          tknum      TYPE  vttk-tknum,
          exti2      TYPE  vttk-exti2,
      END OF ty_vttk,  " Fim do bloco ty_vttk


      BEGIN OF ty_vbak,
          vbeln      TYPE vbak-vbeln,
          tknum      TYPE vbak-tknum,
      END OF ty_vbak,

      BEGIN OF ty_vfkp,
          fknum      TYPE vfkp-fknum,
          bukrs      TYPE vfkp-bukrs,
          rebel      TYPE vfkp-rebel,
      END OF ty_vfkp, " Fim do bloco ty_vfkp

      "Buscar documento de fatura da remessa
      BEGIN OF ty_vbfa,
        vbelv        TYPE vbfa-vbelv,
        posnv        TYPE vbfa-posnv,
        vbeln        TYPE vbfa-vbeln,
        posnn        TYPE vbfa-posnn,
        vbtyp_n      TYPE vbfa-vbtyp_n,
        vbtyp_v      TYPE vbfa-vbtyp_v,
      END OF ty_vbfa, " Fim do bloco ty_vbfa

      "Buscar Item da Nota Fiscal Referenciada
      BEGIN OF ty_j_1bnflin,
        docnum      TYPE j_1bnflin-docnum,
        itmnum      TYPE j_1bnflin-itmnum,
        reftyp      TYPE j_1bnflin-reftyp,
        refkey      TYPE j_1bnflin-refkey,
        refitm      TYPE j_1bnflin-refitm,
        menge       TYPE j_1bnflin-menge ,
      END OF   ty_j_1bnflin,

      BEGIN OF ty_j_1bnflin_frete_saida,

        docnum      TYPE j_1bnflin-docnum,
        itmnum      TYPE j_1bnflin-itmnum,
        reftyp      TYPE j_1bnflin-reftyp,
        refkey      TYPE j_1bnflin-refkey,
        refitm      TYPE j_1bnflin-refitm,

      END OF   ty_j_1bnflin_frete_saida,


"FRETE SAIDA TESTE
      BEGIN OF ty_j_1bnflin_fretesaidateste,
              docnum      TYPE j_1bnflin-docnum,
              itmnum      TYPE j_1bnflin-itmnum,
              reftyp      TYPE j_1bnflin-reftyp,
              refkey      TYPE j_1bnflin-refkey,
              refitm      TYPE j_1bnflin-refitm,
      END OF ty_j_1bnflin_fretesaidateste,
"FIM FRETE SAIDA TESTE

      BEGIN OF ty_j_1bnflin_frete_entrada,
        docnum      TYPE j_1bnflin-docnum,
        itmnum      TYPE j_1bnflin-itmnum,
        reftyp      TYPE j_1bnflin-reftyp,
        refkey      TYPE j_1bnflin-refkey,
        refitm      TYPE j_1bnflin-refitm,
      END OF   ty_j_1bnflin_frete_entrada,

      BEGIN OF ty_vbrp2,
        refkey     TYPE j_1bnflin-refkey,
      END   OF ty_vbrp2,

      "Buscar cabeçalho de nota fiscal
      "DOCNUM = j_1bnflin-DOCNUM
      BEGIN OF ty_j_1bnfdoc,
        docnum      TYPE j_1bnfdoc-docnum,
        model       TYPE j_1bnfdoc-model,
        nfnum       TYPE j_1bnfdoc-nfnum,
        nfenum      TYPE j_1bnfdoc-nfenum,
        direct      TYPE j_1bnfdoc-direct,
      END OF   ty_j_1bnfdoc,

      BEGIN OF ty_j_1bnfdoc_cte,
        docnum      TYPE j_1bnfdoc-docnum,
        model       TYPE j_1bnfdoc-model,
        nfnum       TYPE j_1bnfdoc-nfnum,
        nfenum      TYPE j_1bnfdoc-nfenum,
        direct      TYPE j_1bnfdoc-direct,

        docnum_lin  TYPE j_1bnflin-docnum,
        itmnum      TYPE j_1bnflin-itmnum,
        reftyp      TYPE j_1bnflin-reftyp,
        refkey      TYPE j_1bnflin-refkey,
        refitm      TYPE j_1bnflin-refitm,

      END OF ty_j_1bnfdoc_cte,

      BEGIN OF ty_j_1bnfdoc_cte2,
        docnum      TYPE j_1bnfdoc-docnum,
        model       TYPE j_1bnfdoc-model,
        nfnum       TYPE j_1bnfdoc-nfnum,
        nfenum      TYPE j_1bnfdoc-nfenum,
        direct      TYPE j_1bnfdoc-direct,
      END OF ty_j_1bnfdoc_cte2,

     BEGIN OF ty_vbrp,
        vbeln       TYPE vbrp-vbeln,
        posnr       TYPE vbrp-posnr,
        aubel       TYPE vbrp-aubel,
        refkey      TYPE j_1brefkey,
     END OF ty_vbrp,

     BEGIN OF ty_zlest0032,
       tknum        TYPE zlest0032-tknum,
       ebeln        TYPE zlest0032-ebeln,
       lblni        TYPE zlest0032-lblni,
       belnr        TYPE zlest0032-belnr,
       gjahr        TYPE zlest0032-gjahr,
       docnum       TYPE zlest0032-docnum,
     END OF ty_zlest0032,

     BEGIN OF ty_zlest0032_conct,
       belnr        TYPE zlest0032-belnr,
       gjahr        TYPE zlest0032-gjahr,
       belnr_gjahr  TYPE bkpf-xblnr,
       tknum        TYPE zlest0032-tknum,
       ebeln        TYPE zlest0032-ebeln,
       lblni        TYPE zlest0032-lblni,
       docnum        TYPE zlest0032-docnum,
     END OF ty_zlest0032_conct,

     BEGIN OF ty_bkpf,
       bukrs        TYPE bkpf-bukrs,
       belnr        TYPE bkpf-belnr,
       gjahr        TYPE bkpf-gjahr,
       xblnr        TYPE bkpf-xblnr,
     END OF ty_bkpf,

     BEGIN OF ty_saida,

        vbeln_fornecimento  TYPE lips-vbeln,
        werks_centro        TYPE lips-werks,
        vgbel_ord_venda     TYPE lips-vgbel,
        charg_safra         TYPE lips-charg,
        vgtyp               TYPE lips-vgtyp,
        matnr_material      TYPE lips-matnr,
        lfdat_dataremessa   TYPE likp-lfdat,
        vkorg_empresa       TYPE likp-vkorg,
        nr_romaneio         TYPE zsdt0001-nr_romaneio,
        tknum               TYPE vttp-tknum,
        exti2_cartafrete    TYPE vttk-exti2,
        vbeln_ztro          TYPE vbak-vbeln,
        fknum_doc_custo     TYPE vfkp-fknum,
        vbelv_fat_mat       TYPE vbfa-vbelv,
        docnum_material     TYPE j_1bnflin-docnum,
        menge_qtdremessa    TYPE j_1bnflin-menge,
        docnum_fretesaida   TYPE j_1bnflin-docnum,
        nfnum_entrada       TYPE j_1bnfdoc-nfnum ,
        docnum_doccte       TYPE j_1bnfdoc-nfenum,
        nfenum_nfe          TYPE j_1bnfdoc-nfenum,
        vbeln_docfrete      TYPE vbrp-vbeln,
        ebeln_pedidofrete   TYPE zlest0032-ebeln,
        lblni_folhaservico  TYPE zlest0032-lblni,
        belnr_mirofrete     TYPE zlest0032-belnr,
        f02                 TYPE bkpf-belnr,

      END OF ty_saida                              . " Fim do bloco ty_saida / Fim do Types


*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

DATA:
     it_lips            TYPE TABLE OF ty_lips,
     it_likp            TYPE TABLE OF ty_likp,
     it_mkpf            TYPE TABLE OF ty_mkpf,
     it_zsdt0001        TYPE TABLE OF ty_zsdt0001,
     it_vttp            TYPE TABLE OF ty_vttp,
     it_vttk            TYPE TABLE OF ty_vttk,
     it_vbak            TYPE TABLE OF ty_vbak,
     it_vfkp            TYPE TABLE OF ty_vfkp,
     it_vbfa            TYPE TABLE OF ty_vbfa,
     it_vbfa_ztro       TYPE TABLE OF ty_vbfa,
     it_j_1bnflin       TYPE TABLE OF ty_j_1bnflin,


     it_j_1bnfdoc       TYPE TABLE OF ty_j_1bnfdoc,
     it_vbrp            TYPE TABLE OF ty_vbrp,
     it_vbrp2           TYPE TABLE OF ty_vbrp2,
     it_zlest0032       TYPE TABLE OF ty_zlest0032,
     it_bkpf            TYPE TABLE OF ty_bkpf,

     it_j_1bnflin_frete_saida   TYPE TABLE OF ty_j_1bnflin_frete_saida,
     it_j_1bnflin_frete_entrada TYPE TABLE OF ty_j_1bnflin_frete_entrada,

     it_j_1bnflin_fretesaidateste   TYPE TABLE OF ty_j_1bnflin_fretesaidateste,
     it_j_1bnfdoc_fretesaida        TYPE TABLE OF ty_j_1bnfdoc,


     " Tabelas Internas Auxiliares
     it_vbfa_aux        TYPE TABLE OF ty_vbfa,
     it_lips_aux        TYPE TABLE OF ty_lips,
     it_likp_aux        TYPE TABLE OF ty_likp,
     it_j_1bnflin_aux   TYPE TABLE OF ty_j_1bnflin,

     it_vttp_aux        TYPE TABLE OF ty_vttp,
     it_j_1bnfdoc_aux   TYPE TABLE OF ty_j_1bnfdoc,
     it_vttk_aux        TYPE TABLE OF ty_vttk,
     it_j_1bnfdoc_cte   TYPE TABLE OF ty_j_1bnfdoc_cte,
     it_j_1bnfdoc_cte2  TYPE TABLE OF ty_j_1bnfdoc_cte2,

     it_zlest0032_conct TYPE TABLE OF ty_zlest0032_conct,

     t_bdc              TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
     t_messtab          TYPE TABLE OF bdcmsgcoll,

     "Tabela Interna de Saida
      it_saida          TYPE TABLE OF ty_saida.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

DATA:
     wa_lips        TYPE ty_lips,
     wa_likp        TYPE ty_likp,
     wa_mkpf        TYPE ty_mkpf,
     wa_zsdt0001    TYPE ty_zsdt0001,
     wa_vttp        TYPE ty_vttp,
     wa_vttk        TYPE ty_vttk,
     wa_vbak        TYPE ty_vbak,
     wa_vfkp        TYPE ty_vfkp,
     wa_vbfa        TYPE ty_vbfa,
     wa_vbfa_ztro   TYPE ty_vbfa,
     wa_j_1bnflin   TYPE ty_j_1bnflin,
     wa_j_1bnfdoc   TYPE ty_j_1bnfdoc,
     wa_vbrp        TYPE ty_vbrp,
     wa_vbrp2       TYPE ty_vbrp2,
     wa_zlest0032   TYPE ty_zlest0032,
     wa_saida       TYPE ty_saida,
     wa_bkpf        TYPE ty_bkpf,

     wa_j_1bnfdoc_cte TYPE ty_j_1bnfdoc_cte,
     wa_j_1bnfdoc_cte2 TYPE ty_j_1bnfdoc_cte2,
     wa_j_1bnflin_frete_saida TYPE ty_j_1bnflin_frete_saida,

     wa_j_1bnflin_fretesaidateste TYPE ty_j_1bnflin_fretesaidateste,
     wa_j_1bnfdoc_fretesaida   TYPE ty_j_1bnfdoc,

     wa_j_1bnflin_frete_entrada TYPE ty_j_1bnflin_frete_entrada,
     wa_zlest0032_conct TYPE ty_zlest0032_conct,

     wa_cont        TYPE REF TO cl_gui_custom_container,
     wa_alv         TYPE REF TO cl_gui_alv_grid,
     wa_layout      TYPE lvc_s_layo.


*&---------------------------------------------------------------------*
*& Estrutura ALV
*&---------------------------------------------------------------------*

DATA:
      it_fcat TYPE TABLE OF lvc_s_fcat.


*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: p_werks FOR lips-werks,
                p_lfdat FOR likp-lfdat,
                p_vbeln FOR lips-vbeln NO INTERVALS,
                p_matnr FOR lips-matnr.

SELECTION-SCREEN: END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM:  f_seleciona_dados, " Form seleciona dados
            f_saida, " Form de saida
            f_alv. " Form ALV

  CALL SCREEN 0100.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Form para selecionar os dados e relacionar as tabelas.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados.

  DATA: vg_refkey TYPE j_1brefkey.

  REFRESH:
           it_lips,
           it_likp,
           it_zsdt0001,
           it_vttp,
           it_vttk,
           it_vbak,
           it_vfkp,
           it_vbfa,
           it_zlest0032,
           it_j_1bnflin,
           it_j_1bnfdoc.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR, FORNECIMENTO, Nº DA ORDEM DE VENDA
*                         SAFRA (ANO), CENTRO (FILIAL), MATERIAL
* DESCRIÇÃO: Documento SD: fornecimento: dados de item
* COLUNA: Fornecimento
*         Nº da Ordem de Venda
*         Safra
*         Centro
*         Filial
*----------------------------------------------------------------------*
  SELECT
        vbeln
        vgbel
        charg
        posnr
        werks
        brgew
        vgtyp
        matnr
  FROM lips
  INTO TABLE it_lips
  WHERE werks IN p_werks
        AND vbeln IN p_vbeln
        AND matnr IN p_matnr.

*----------------------------------------------------------------------*
* DELETA DUPLICAÇÃO PARA PESQUISA DA LIPS COMPARANDO O VBELN (FORNECIMENTO).
*----------------------------------------------------------------------*
  it_lips_aux[] = it_lips[].
  SORT it_lips_aux BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_lips_aux COMPARING vbeln.
  CHECK NOT it_lips_aux[] IS INITIAL.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR EMPRESA, DATA DA REMESSA
* DESCRIÇÃO: Documento SD: fornecimento: dados de cabeçalho
* COLUNA: Empresa
*         Data da Remessa
*----------------------------------------------------------------------*
  SELECT
      vbeln
      lfdat
      vkorg
  FROM likp
  INTO TABLE it_likp
  FOR ALL ENTRIES IN it_lips
  WHERE vbeln EQ it_lips-vbeln.

*----------------------------------------------------------------------*
* DELETA DUBPLICAÇÃO PARA PESQUISA DA LIKP COMPARANDO O VBELN
*----------------------------------------------------------------------*
  it_likp_aux[] = it_likp[].
  SORT it_likp_aux BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_likp_aux COMPARING vbeln.
  CHECK NOT it_likp_aux[] IS INITIAL.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR DOCUMENTO DE FATURA DA REMESSA
* DESCRIÇÃO: Fluxo de documentos de vendas e distribuição
* COLUNA: Doc. Fatura - Material
*----------------------------------------------------------------------*
  SELECT
        vbelv
        posnv
        vbeln
        posnn
        vbtyp_n
        vbtyp_v
  FROM vbfa
  INTO TABLE it_vbfa
  FOR ALL ENTRIES IN it_likp_aux
  WHERE vbelv EQ it_likp_aux-vbeln
        AND vbtyp_n EQ 'R'
        AND vbtyp_v EQ 'J'.
*----------------------------------------------------------------------*
* DELETA DUBPLICAÇÃO PARA PESQUISA DA LIKP COMPARANDO O VBELN
*----------------------------------------------------------------------*
  it_vbfa_aux[] = it_vbfa[].
  SORT it_vbfa_aux BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_vbfa_aux COMPARING vbeln posnn.
  CHECK NOT it_vbfa_aux[] IS INITIAL.

*----------------------------------------------------------------------*
* DESCRIÇÃO: Cabeçalho do documento do material
*----------------------------------------------------------------------*
  SELECT
        mblnr
        mjahr
  FROM mkpf
  INTO TABLE it_mkpf
  FOR ALL ENTRIES IN it_vbfa_aux
  WHERE mblnr EQ it_vbfa_aux-vbeln.

  SORT it_mkpf BY mblnr.

  DATA: sytabix TYPE sy-tabix.

  LOOP AT it_mkpf INTO wa_mkpf.
    sytabix = sy-tabix.
    READ TABLE it_vbfa_aux INTO wa_vbfa WITH KEY vbeln = wa_mkpf-mblnr.
    wa_mkpf-le_vbeln = wa_vbfa-vbelv.
    MODIFY it_mkpf INDEX sytabix FROM wa_mkpf TRANSPORTING le_vbeln.
  ENDLOOP.
  CLEAR: it_vbfa_aux[], it_vbfa[].

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR NÚMERO DO ROMANEIO
* DESCRIÇÃO: Informações para preenchimento dos dados de remessa. (OPUS)
* COLUNA: Nº DO ROMANEIO
*----------------------------------------------------------------------*
  SELECT
          vbeln
          doc_rem
          nr_romaneio
          nr_safra
  INTO TABLE it_zsdt0001
  FROM zsdt0001
  FOR ALL ENTRIES IN it_lips_aux
  WHERE doc_rem EQ it_lips_aux-vbeln.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR DOCUMENTO DO TRANSPORTE
* DESCRIÇÃO: Item de transporte
* COLUNA: DOC. TRANSPORTE
*----------------------------------------------------------------------*
  SELECT
        vbeln
        tknum
        tpnum
  FROM vttp
  INTO TABLE it_vttp
  FOR ALL ENTRIES IN it_lips_aux
   WHERE vbeln EQ it_lips_aux-vbeln.

*----------------------------------------------------------------------*
* DELETA DUBPLICAÇÃO PARA PESQUISA DA VTTP COMPARANDO O TKNUM (Nº DO TRANSPORTE)
*----------------------------------------------------------------------*
  it_vttp_aux[] = it_vttp[].
  SORT it_vttp_aux BY tknum.
  DELETE ADJACENT DUPLICATES FROM it_vttp_aux COMPARING tknum.
  CHECK NOT it_vttp_aux[] IS INITIAL.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR CARTA FRETE
* DESCRIÇÃO: Cabeçalho transporte
* COLUNA:
*        Carta Frete (EXTI2)
*----------------------------------------------------------------------*
  SELECT
        tknum
        exti2
  FROM vttk
  INTO TABLE it_vttk
  FOR ALL ENTRIES IN it_vttp_aux
  WHERE tknum EQ it_vttp_aux-tknum.

*----------------------------------------------------------------------*
* DELETA DUBPLICAÇÃO PARA PESQUISA DA VTTK COMPARANDO O TKNUM
*----------------------------------------------------------------------*
  it_vttk_aux[] = it_vttk[].
  SORT it_vttk_aux BY tknum.
  DELETE ADJACENT DUPLICATES FROM it_vttk_aux COMPARING tknum.
  CHECK NOT it_vttk_aux[] IS INITIAL.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR A ZTRO
* DESCRIÇÃO: CDocumento de vendas: dados de cabeçalho
* COLUNA:
*        ZTRO (VBELN)
*----------------------------------------------------------------------*
  SELECT
        vbeln
        tknum
  FROM vbak
  INTO TABLE it_vbak
  FOR ALL ENTRIES IN it_vttk_aux
  WHERE tknum EQ it_vttk_aux-tknum.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR O DOCUMENTO DE CUSTO
* DESCRIÇÃO: CCustos de frete: dados do item
* COLUNA:
*        Doc. de Custo (FKNUM)
*----------------------------------------------------------------------*
  SELECT
        fknum
        bukrs
        rebel
  FROM vfkp
  INTO TABLE it_vfkp
  FOR ALL ENTRIES IN it_vttk_aux
  WHERE rebel EQ it_vttk_aux-tknum.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR O DOCUMENTO DA FATURA
* DESCRIÇÃO: Fluxo de documentos de vendas e distribuição
* COLUNA:
*        Doc. Fatura - Material
*----------------------------------------------------------------------*
  SELECT
         vbelv
         posnv
         vbeln
         posnn
         vbtyp_n
         vbtyp_v
  FROM vbfa
  INTO TABLE it_vbfa
  FOR ALL ENTRIES IN it_lips_aux
  WHERE vbelv EQ it_lips_aux-vbeln
  AND vbtyp_n EQ 'M'
  AND vbtyp_v EQ 'J'.

*----------------------------------------------------------------------*
* DELETA DUBPLICAÇÃO PARA PESQUISA DA VBFA COMPARANDO O VBELN E POSNN
*----------------------------------------------------------------------*
  it_vbfa_aux[] = it_vbfa[].
  SORT it_vbfa_aux BY vbeln posnn.
  DELETE ADJACENT DUPLICATES FROM it_vbfa_aux COMPARING vbeln posnn.
  CHECK NOT it_vbfa_aux[] IS INITIAL.

  LOOP AT it_vbfa_aux INTO wa_vbfa.
    wa_j_1bnflin-refkey = wa_vbfa-vbeln.
    wa_j_1bnflin-refitm = wa_vbfa-posnn.
    APPEND wa_j_1bnflin TO it_j_1bnflin_aux.
    CLEAR: wa_vbfa     ,
           wa_j_1bnflin.
  ENDLOOP.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR O DOCUMENTO DO MATERIAL,
*                          DOCUMENTO FRETE DE SAÍDA,
*                          QUANTIDADE DA REMESSA
* DESCRIÇÃO: Partidas individuais da nota fiscal
* COLUNA:
*        Docnum - Material
*        Docnum - Frete Saída
*        Qtd. da Remessa
*----------------------------------------------------------------------*
  SELECT
        docnum
        itmnum
        reftyp
        refkey
        refitm
        menge
  FROM j_1bnflin
  INTO TABLE it_j_1bnflin
  FOR ALL ENTRIES IN it_j_1bnflin_aux
  WHERE reftyp EQ 'BI'
        AND refkey EQ it_j_1bnflin_aux-refkey
        AND refitm EQ it_j_1bnflin_aux-refitm.

*----------------------------------------------------------------------*
* DELETA DUBPLICAÇÃO PARA PESQUISA DA J_1BNFLIN COMPARANDO O DOCNUM
*----------------------------------------------------------------------*
  it_j_1bnflin_aux[] = it_j_1bnflin[].
  SORT it_j_1bnflin_aux BY docnum.
  DELETE ADJACENT DUPLICATES FROM it_j_1bnflin_aux COMPARING docnum.
  CHECK NOT it_j_1bnflin_aux[] IS INITIAL.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR O NÚMERO DA NOTA FISCAL ELETRONICA (NF-E),
*                          CTE DE SAÍDA,
* DESCRIÇÃO: Cabeçalho da nota fiscal
* COLUNA:
*        Nº NF-e
*        Doc. Cte
*----------------------------------------------------------------------*
  SELECT
        docnum
        model
        nfnum
        nfenum
        direct
  FROM j_1bnfdoc
  INTO TABLE it_j_1bnfdoc
  FOR ALL ENTRIES IN it_j_1bnflin_aux
  WHERE docnum EQ it_j_1bnflin_aux-docnum
        AND direct = 2.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR O DOCNUM E COMPARAR COM A J_1BNFDOC
* PARA RECUPERAR A NOTA FISCAL DE ENTRADA
*----------------------------------------------------------------------*
  SELECT doc~docnum
         doc~model
         doc~nfnum
         doc~nfenum
         doc~direct
         lin~docnum
 	       lin~itmnum
         lin~reftyp
         lin~refkey
         lin~refitm
  FROM j_1bnfdoc AS doc
  INNER JOIN j_1bnflin AS lin ON lin~docnum EQ doc~docnum
  INTO TABLE it_j_1bnfdoc_cte
  FOR ALL ENTRIES IN it_j_1bnflin
  WHERE doc~docnum EQ it_j_1bnflin-docnum.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR O DOCNUM E COMPARAR COM A J_1BNFDOC
* PARA RECUPERAR A NOTA FISCAL DE ENTRADA
*----------------------------------------------------------------------*
  SELECT
        tknum
        ebeln
        lblni
        belnr
        gjahr
        docnum
  FROM  zlest0032
  INTO TABLE it_zlest0032
  FOR ALL ENTRIES IN it_vbak
  WHERE tknum EQ it_vbak-tknum.

  SELECT
        docnum
        model
        nfnum
        nfenum
        direct
  FROM j_1bnfdoc
  INTO TABLE it_j_1bnfdoc_cte2
  FOR ALL ENTRIES IN it_zlest0032
  WHERE docnum EQ it_zlest0032-docnum.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR O DOCUMENTO DE FATURA DO FRETE
* DESCRIÇÃO: Documento de faturamento: dados de item
* COLUNA:
*        Doc. Fatura - Frete
*----------------------------------------------------------------------*
  SELECT
        vbeln
        posnr
        aubel
  FROM vbrp
  INTO TABLE it_vbrp
  FOR ALL ENTRIES IN it_vbak
  WHERE aubel EQ it_vbak-vbeln AND DRAFT = SPACE .

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR
* DESCRIÇÃO: Partidas individuais da nota fiscal
* COLUNA:
*
*----------------------------------------------------------------------*
  LOOP AT it_vbrp INTO wa_vbrp.
    wa_vbrp-refkey = wa_vbrp-vbeln.
    MODIFY it_vbrp FROM wa_vbrp INDEX sy-tabix TRANSPORTING refkey.
  ENDLOOP.

  SELECT
        docnum
        itmnum
        reftyp
        refkey
        refitm
   FROM j_1bnflin
   INTO TABLE it_j_1bnflin_frete_saida
    FOR ALL ENTRIES IN it_vbrp
  WHERE refkey EQ it_vbrp-refkey.

  SELECT
       docnum
       model
       nfnum
       nfenum
       direct
  FROM j_1bnfdoc
  INTO TABLE it_j_1bnfdoc_cte
  FOR ALL ENTRIES IN it_j_1bnflin_frete_saida
  WHERE docnum EQ it_j_1bnflin_frete_saida-docnum.

  LOOP AT it_zlest0032  INTO wa_zlest0032.
    CONCATENATE
              wa_zlest0032-belnr
              wa_zlest0032-gjahr
              INTO wa_zlest0032_conct-belnr_gjahr.

    wa_zlest0032_conct-belnr   =  wa_zlest0032-belnr.
    wa_zlest0032_conct-gjahr   =  wa_zlest0032-gjahr.
    wa_zlest0032_conct-tknum   =  wa_zlest0032-tknum.
    wa_zlest0032_conct-ebeln   =  wa_zlest0032-ebeln.
    wa_zlest0032_conct-lblni   =  wa_zlest0032-lblni.
    wa_zlest0032_conct-docnum  =  wa_zlest0032-docnum.

    APPEND wa_zlest0032_conct TO it_zlest0032_conct.
    CLEAR wa_zlest0032_conct.
  ENDLOOP.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR O CAMPO F-02
* DESCRIÇÃO: Cabeçalho do documento contábil
* COLUNA:
*        F-02
*----------------------------------------------------------------------*
  SELECT
        bukrs
        belnr
        gjahr
        xblnr
  FROM bkpf
  INTO TABLE it_bkpf
  FOR ALL ENTRIES IN it_zlest0032_conct
  WHERE xblnr EQ it_zlest0032_conct-belnr_gjahr.


ENDFORM.                    "f_seleciona_dados


*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv.
  PERFORM alv_preenche_cat USING:
        'VKORG_EMPRESA'        text-001      '4'       ' '  'X'            , " Empresa
        'WERKS_CENTRO'         text-002      '4'       ' '  'X'            , " Centro (Filial)
        'NR_ROMANEIO'          text-006      '9'       ' '  'X'            , " Nº do Romaneio
        'CHARG_SAFRA'          text-007      '10'      ' '  ' '            , " Safra
        'VGBEL_ORD_VENDA'      text-004      '10'      'X'  'X'            , " Nº da Ordem de Venda
        'VBELN_FORNECIMENTO'   text-003      '10'      ' '  'X'            , " Fornecimento
        'VBELV_FAT_MAT'        text-008      '10'      ' '  'X'            , " Doc. Fatura - Material
        'DOCNUM_MATERIAL'      text-009      '10'      ' '  'X'            , " Docnum - Material
        'MATNR_MATERIAL'       text-031      '18'      ' '  'X'            , " Material
        'NFENUM_NFE'           text-020      '9'       ' '  ' '            , " Nº Nf-e
        'TKNUM'                text-011      '10'      ' '  'X'            , " Documento de Transporte
        'FKNUM_DOC_CUSTO'      text-012      '10'      ' '  'X'            , " Documento de Custo
        'EXTI2_CARTAFRETE'     text-019      '10'      ' '  ' '            , " Carta Frete
        'DOCNUM_DOCCTE'        text-015      '10'      ' '  'X'            , " Documento do CTE
        'VBELN_ZTRO'           text-014      '10'      ' '  'X'            , " ZTRO
        'VBELN_DOCFRETE'       text-022      '10'      ' '  'X'            , " Documento Fatura - Frete
        'DOCNUM_FRETESAIDA'    text-028      '20'      ' '  'X'            , " Docnum - Frete Saída
        'EBELN_PEDIDOFRETE'    text-023      '10'      ' '  'X'            , " Pedido de Frete
        'LBLNI_FOLHASERVICO'   text-024      '10'      ' '  'X'            , " Folha de Serviço
        'BELNR_MIROFRETE'      text-025      '10'      ' '  'X'            , " Miro Frete
        'NFNUM_ENTRADA'        text-029      '20'      ' '  'X'            , " Docnum - Frete Entrada
        'F02'                  text-030      '10'      ' '  'X'            , " F-02
        'MENGE_QTDREMESSA'     text-016      '15'      ' '  ' '            , " Quantidade de Remessa
        'LFDAT_DATAREMESSA'    text-017      '8'       ' '  ' '            . " Data da Remessa

ENDFORM.                    " F_ALV

*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bdc_field  USING    value(p_flag)
                           value(p_fnam)
                           value(p_fval).

  CLEAR t_bdc.
  IF NOT p_flag IS INITIAL.
    t_bdc-program  = p_fnam.
    t_bdc-dynpro   = p_fval.
    t_bdc-dynbegin = 'X'.
  ELSE.
    t_bdc-fnam = p_fnam.
    t_bdc-fval = p_fval.
  ENDIF.
  APPEND t_bdc.

ENDFORM.                    " F_BDC_FIELD


*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_status OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "z_status OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_layout .
  wa_layout-zebra = 'X'.
ENDFORM.                    " Z_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0334   text
*      -->P_TEXT_002  text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT


CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA wa_event TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
            zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
            IMPORTING e_row_id
                      e_column_id
                      es_row_no,

            zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING
                e_object e_interactive,

            zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING
                 e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot.
    PERFORM z_handle_hotspot USING    e_row_id
                                      e_column_id
                                      es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_exibe_alv OUTPUT.

  IF wa_cont IS INITIAL.

    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CC_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.
  IF wa_alv IS INITIAL AND NOT
    wa_cont IS INITIAL.

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event IS INITIAL.

    CREATE OBJECT wa_event.
    SET HANDLER: wa_event->zm_handle_hotspot FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv.

  ENDIF.

  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
    CHANGING
      it_outtab                     = it_saida
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK NOT wa_alv IS INITIAL.
ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_user_command INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM z_handle_hotspot  USING    p_e_row_id TYPE lvc_s_row
                                p_e_column_id TYPE  lvc_s_col
                                p_es_row_no TYPE  lvc_s_roid.

  DATA opt TYPE ctu_params.

  CASE p_e_column_id.
    WHEN 'VGBEL'.
      READ TABLE it_saida INTO wa_saida INDEX p_e_row_id.
      IF wa_saida-vgtyp EQ 'V'.
        CLEAR: t_bdc[], t_messtab.
        PERFORM f_bdc_field USING: 'X' 'SAPLMEGUI'           '0014'             ,
                                   ' ' 'BDC_OKCODE'	         '=MECHOB'          ,
                                   ' ' 'DYN_6000-LIST'       '1'                ,
                                   'X' 'SAPLMEGUI'           '0002'             ,
                                   ' ' 'BDC_OKCODE'	         '=MEOK'            ,
                                   ' ' 'BDC_SUBSCR'	         'SAPLMEGUI'        ,
                                   ' ' 'BDC_CURSOR'	         'MEPO_SELECT-EBELN',
                                   ' ' 'MEPO_SELECT-EBELN'   wa_saida-vgbel_ord_venda     ,
                                   ' ' 'MEPO_SELECT-BSTYP_F' 'X'.

        opt-dismode = 'E'.
        opt-defsize = 'X'.
        CALL TRANSACTION 'ME23N' USING t_bdc OPTIONS FROM opt MESSAGES INTO t_messtab.
      ELSE.
        SET PARAMETER ID 'AUN' FIELD wa_saida-vgbel_ord_venda.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

ENDFORM.                    " Z_HANDLE_HOTSPOT


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

FORM z_handle_toolbar  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
                                p_interactive TYPE char1 .

** Constants for button type
  CONSTANTS:
        c_button_normal           TYPE i VALUE 0        ,
        c_menu_and_default_button TYPE i VALUE 1        ,
        c_menu                    TYPE i VALUE 2        ,
        c_separator               TYPE i VALUE 3        ,
        c_radio_button            TYPE i VALUE 4        ,
        c_checkbox                TYPE i VALUE 5        ,
        c_menu_entry              TYPE i VALUE 6        .

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.



ENDFORM.                    " Z_HANDLE_TOOLBAR


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM z_handle_command  USING p_ucomm TYPE syucomm       .

  CASE p_ucomm.
    WHEN 'REMESSA'.
*     Gera Remessa
      CALL METHOD wa_alv->refresh_table_display .
  ENDCASE.
ENDFORM.                    " Z_HANDLE_COMMAND


*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .

  SORT:
       it_lips            BY vbeln posnr,
       it_likp            BY vbeln,
       it_zsdt0001        BY doc_rem,
       it_vttp            BY vbeln tknum tpnum,
       it_vttk            BY tknum,
       it_vbak            BY tknum,
       it_vfkp            BY rebel fknum,
       it_vbfa            BY vbelv posnv vbeln posnn,
       it_j_1bnflin       BY refkey docnum itmnum,
       it_j_1bnfdoc       BY docnum,
       it_vbrp            BY vbeln posnr,
       it_bkpf            BY xblnr,
       it_zlest0032_conct BY tknum,
       it_j_1bnflin_frete_saida BY refkey,
       it_j_1bnfdoc_cte   BY docnum.

  DATA: vl_index TYPE i.

  LOOP AT it_lips INTO wa_lips.

    IF ( NOT p_matnr IS INITIAL ) AND ( wa_lips-matnr NOT IN p_matnr ).
      CONTINUE.
    ENDIF.

    wa_saida-vbeln_fornecimento = wa_lips-vbeln.
    wa_saida-vgbel_ord_venda = wa_lips-vgbel.
    wa_saida-charg_safra = wa_lips-charg.
    wa_saida-werks_centro = wa_lips-werks.
    wa_saida-matnr_material = wa_lips-matnr.

    READ TABLE it_likp INTO wa_likp WITH KEY vbeln = wa_lips-vbeln BINARY SEARCH.
    IF  ( NOT p_lfdat IS INITIAL ) AND ( wa_likp-lfdat NOT IN p_lfdat ).
      CONTINUE.
    ENDIF.

    IF sy-subrc EQ 0.
      wa_saida-lfdat_dataremessa = wa_likp-lfdat.
      wa_saida-vkorg_empresa = wa_likp-vkorg.
    ENDIF.

    READ TABLE it_vbfa INTO wa_vbfa WITH KEY vbelv = wa_lips-vbeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-vbelv_fat_mat   = wa_vbfa-vbeln.
    ENDIF.

    READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY doc_rem = wa_lips-vbeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-nr_romaneio = wa_zsdt0001-nr_romaneio.
    ENDIF.

    READ TABLE it_vttp INTO wa_vttp WITH KEY vbeln = wa_likp-vbeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-tknum = wa_vttp-tknum.
    ENDIF.

    READ TABLE it_vttk INTO wa_vttk WITH KEY tknum = wa_vttp-tknum BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-tknum = wa_vttk-tknum.
      wa_saida-exti2_cartafrete = wa_vttk-exti2.
    ENDIF.

    READ TABLE it_vbak INTO wa_vbak WITH KEY tknum = wa_vttk-tknum BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-vbeln_ztro = wa_vbak-vbeln.
      wa_saida-tknum     = wa_vbak-tknum.
    ENDIF.

    READ TABLE it_vfkp INTO wa_vfkp WITH KEY rebel = wa_vttk-tknum BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-fknum_doc_custo = wa_vfkp-fknum.

    ENDIF.

    READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = wa_vbfa-vbeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-docnum_material   = wa_j_1bnflin-docnum.
      wa_saida-menge_qtdremessa = wa_j_1bnflin-menge.

    ENDIF.

    READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-nfenum_nfe     = wa_j_1bnfdoc-nfenum.
    ENDIF.

    READ TABLE it_vbrp INTO wa_vbrp WITH KEY aubel = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-vbeln_docfrete  = wa_vbrp-vbeln.
      READ TABLE it_j_1bnflin_frete_saida INTO wa_j_1bnflin_frete_saida WITH KEY refkey = wa_vbrp-refkey BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_saida-docnum_fretesaida = wa_j_1bnflin_frete_saida-docnum.
      ENDIF.
    ENDIF.

    READ TABLE it_zlest0032_conct INTO wa_zlest0032_conct WITH KEY tknum = wa_vbak-tknum BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-ebeln_pedidofrete = wa_zlest0032_conct-ebeln.
      wa_saida-lblni_folhaservico = wa_zlest0032_conct-lblni.
      wa_saida-belnr_mirofrete = wa_zlest0032_conct-belnr.

    ENDIF.

    READ TABLE it_bkpf INTO wa_bkpf WITH KEY xblnr = wa_zlest0032_conct-belnr_gjahr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_saida-f02   = wa_bkpf-belnr.
    ENDIF.

*---> 05/07/2023 - Migração S4 - DL
SORT it_j_1bnfdoc_cte2 BY docnum.
*<--- 05/07/2023 - Migração S4 - DL


    READ TABLE it_j_1bnfdoc_cte2 INTO wa_j_1bnfdoc_cte2 WITH KEY docnum = wa_zlest0032_conct-docnum BINARY SEARCH.
    IF sy-subrc EQ 0.
     wa_saida-nfnum_entrada   = wa_j_1bnfdoc_cte2-nfnum.

    ENDIF.

     READ TABLE it_j_1bnfdoc_cte INTO wa_j_1bnfdoc_cte WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.
     IF sy-subrc EQ 0.
      wa_saida-docnum_doccte  = wa_j_1bnfdoc_cte-nfenum.
    ENDIF.




    APPEND wa_saida TO it_saida.

    CLEAR:
               wa_lips,
               wa_likp,
               wa_zsdt0001,
               wa_vttp,
               wa_vttk,
               wa_vbak,
               wa_vfkp,
               wa_vbfa,
               wa_j_1bnflin,
               wa_j_1bnfdoc,
               wa_vbrp,
               wa_j_1bnfdoc_cte2,
               wa_zlest0032_conct,
               wa_bkpf,
               wa_saida.
  ENDLOOP.

ENDFORM.                    " F_SAIDA
