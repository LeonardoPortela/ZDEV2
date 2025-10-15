*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZLESR0011                                               *
* Descrição  : Controle de Carregamento de Frete c/ Seguro             *
* Módulo     : LES                               Transação: ZSDT0016   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 14/09/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zlesr0011 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TABLES: vttk,
        lips.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_alv,
         vbeln_va  TYPE vbfa-vbeln,
         vbeln     TYPE vttp-vbeln,
         vbeln_vf  TYPE vbfa-vbeln,
         nfenum    TYPE j_1bnfdoc-nfenum,
         netwr_lin TYPE j_1bnflin-netwr,
         series    TYPE j_1bnfdoc-series,
         maktx     TYPE makt-maktx,
         lfimg     TYPE lips-lfimg,
         vrkme     TYPE lips-vrkme,
         brgew     TYPE lips-brgew,
         city1     TYPE adrc-city1,
         region    TYPE adrc-region,
         name1_kn  TYPE kna1-name1,
         stcd1_kn  TYPE kna1-stcd1,
         stcd2_kn  TYPE kna1-stcd2,
         ort01_kn  TYPE kna1-ort01,
         regio_kn  TYPE kna1-regio,
         bezei     TYPE ttdst-bezei,
         tknum     TYPE vttk-tknum,
         name1_lf  TYPE lfa1-name1,
         exti1     TYPE vttk-exti1,
         exti2     TYPE vttk-exti2,
         signi     TYPE vttk-signi,
         text1     TYPE vttk-text1,
         name1_mt  TYPE lfa1-name1,
         route     TYPE tvro-route,
         datbg     TYPE vttk-datbg,
         fknum     TYPE vfkp-fknum,
         netwr     TYPE vfsi-netwr,
         mwsbp     TYPE vfsi-mwsbp,
         kwert_seg TYPE konv-kwert,
         kwert_iof TYPE konv-kwert,
         ebeln     TYPE vfkp-ebeln,
         lblni     TYPE vfkp-lblni,
       END   OF type_alv.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_vttk    TYPE TABLE OF vttk,
      t_vttp    TYPE TABLE OF vttp,
      t_vfkp    TYPE TABLE OF vfkp,
      t_ekbe    TYPE TABLE OF ekbe,
      t_vbfa_va TYPE TABLE OF vbfa,
      t_vbfa_vf TYPE TABLE OF vbfa,
      t_likp    TYPE TABLE OF likp,
      t_lips    TYPE TABLE OF lips,
      t_lin     TYPE TABLE OF j_1bnflin,
      t_doc     TYPE TABLE OF j_1bnfdoc,
      t_kna1    TYPE TABLE OF kna1,
      t_t0016   TYPE TABLE OF zlest0016,
      t_ttds    TYPE TABLE OF ttds,
      t_ttdst   TYPE TABLE OF ttdst,
      t_tvtkt   TYPE TABLE OF tvtkt,
      t_adrc    TYPE TABLE OF adrc,
      t_vtpa    TYPE TABLE OF vtpa,
      t_lfa1    TYPE TABLE OF lfa1,
      t_lfa1_2  TYPE TABLE OF lfa1,
      t_alv     TYPE TABLE OF type_alv,
      t_fcat    TYPE TABLE OF lvc_s_fcat,
      t_vfsi    TYPE TABLE OF vfsi,
      t_konv    TYPE TABLE OF konv,
      t_vbpa_pc TYPE TABLE OF vbpa,
      t_vbpa_lr TYPE TABLE OF vbpa,
      t_lfa1_pc TYPE TABLE OF lfa1,
      t_lfa1_lr TYPE TABLE OF lfa1,
      t_lfa1_mt TYPE TABLE OF lfa1,
      t_makt    TYPE TABLE OF makt,
      t_tvrot   TYPE TABLE OF tvrot.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: s_alv    TYPE REF TO cl_gui_alv_grid,
      s_layout TYPE lvc_s_layo,
      s_cont   TYPE REF TO cl_gui_custom_container.

CONTROLS tc_alv TYPE TABLEVIEW USING SCREEN '0100'.
CONSTANTS c_table TYPE char9 VALUE 'ZLESR0011'.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE text-002.
PARAMETERS:
  p_bukrs TYPE t001-bukrs,
  p_tdlnr TYPE vttk-tdlnr OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK a2.
SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME TITLE text-003.
SELECT-OPTIONS:
  s_erdat FOR vttk-erdat OBLIGATORY,
  s_tplst FOR vttk-tplst,
  s_shtyp FOR vttk-shtyp,
  s_tknum FOR vttk-tknum,
  s_route FOR vttk-route,
  s_safra FOR lips-lgort.
SELECTION-SCREEN END   OF BLOCK a3.
SELECTION-SCREEN BEGIN OF BLOCK a4 WITH FRAME TITLE text-004.
PARAMETERS:
  rb_out TYPE char1 RADIOBUTTON GROUP rb01
                    DEFAULT 'X',
  rb_in  TYPE char1 RADIOBUTTON GROUP rb01.
SELECTION-SCREEN END   OF BLOCK a4.
SELECTION-SCREEN END   OF BLOCK a1.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF NOT rb_out IS INITIAL.
*   Seleciona Dados Saída
    PERFORM z_seleciona_saida USING 1.
*   Agrupa Dados
    PERFORM z_agrupa_dados.
  ELSE.
*   Seleciona Dados Saída
    PERFORM z_seleciona_saida_2 USING 2.
*   Agrupa Dados
    PERFORM z_agrupa_dados_2.
  ENDIF.

  IF NOT t_alv[] IS INITIAL.
*   Estruturas ALV
    PERFORM z_estruturas_alv.
    CALL SCREEN 0100.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_SAIDA                                        *
*&---------------------------------------------------------------------*
*                       Seleciona Dados Saída                          *
*----------------------------------------------------------------------*
FORM z_seleciona_saida USING p_tipo TYPE i.

* Seleciona Documentos de Transporte
  PERFORM: z_seleciona_vttk USING p_tipo,
* Seleciona Parceiros Transporte
           z_seleciona_vtpa,
* Seleciona Itens do Transporte
           z_seleciona_vttp,
* Seleciona Documento de Vendas
           z_seleciona_vbfa,
* Seleciona Remessas
           z_seleciona_likp,
* Seleciona Itens Remessa
           z_seleciona_lips,
* Seleciona Itens Nota Fiscal
           z_seleciona_lin ,
* Seleciona Documentos NF
           z_seleciona_doc ,
* Seleciona Itens Custo de Frete
           z_seleciona_vfkp,
* Seleciona Categoria Item
           z_seleciona_vfsi,
* Seleciona Condições de Preço
           z_seleciona_konv,
* Seleciona Dados Parceiro
           z_seleciona_vbpa,
* Seleciona Dados Fornecedor
           z_seleciona_lfa1,
* Seleciona Dados Cliente
           z_seleciona_kna1,
* Seleciona Dados Org. Transporte
           z_seleciona_ttds,
* Seleciona Dados de Endereço
           z_seleciona_adrc,
* Seleciona Descrição dos Materiais
           z_seleciona_makt,
* Seleciona Descrição Itinerários
           z_seleciona_tvrot,
* Seleciona Texto Tipo Transporte
           z_seleciona_tvtkt.

ENDFORM.                    " Z_SELECIONA_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VTTK                                         *
*&---------------------------------------------------------------------*
*                 Seleciona Documentos de Transporte                   *
*----------------------------------------------------------------------*
FORM z_seleciona_vttk USING p_tipo TYPE i.

  SELECT *
    FROM vttk
    INTO TABLE t_vttk
  WHERE  tknum IN s_tknum
    AND  shtyp IN s_shtyp
    AND  tplst IN s_tplst
    AND  erdat IN s_erdat
    AND  tdlnr EQ p_tdlnr.

  DELETE t_vttk WHERE route NOT IN s_route.
  IF p_tipo EQ 1.
    DELETE t_vttk WHERE abfer NE '1'
                    AND abfer NE '3'.
  ELSE.
    DELETE t_vttk WHERE abfer NE '2'
                    AND abfer NE '4'.
  ENDIF.

  IF t_vttk[] IS INITIAL.
    MESSAGE i836 WITH text-005.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT t_vttk BY tknum ASCENDING.

ENDFORM.                    " Z_SELECIONA_VTTK

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VTTP                                         *
*&---------------------------------------------------------------------*
*                      Seleciona Itens do Transporte                   *
*----------------------------------------------------------------------*
FORM z_seleciona_vttp.

  CHECK NOT t_vttk[] IS INITIAL.

  SELECT *
    FROM vttp
    INTO TABLE t_vttp
    FOR ALL ENTRIES IN t_vttk
  WHERE  tknum EQ t_vttk-tknum.

  SORT t_vttp BY tknum ASCENDING
                 tpnum ASCENDING.

ENDFORM.                    " Z_SELECIONA_VTTP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBFA                                         *
*&---------------------------------------------------------------------*
*                  Seleciona Documento de Vendas                       *
*----------------------------------------------------------------------*
FORM z_seleciona_vbfa.

  DATA tl_vttp TYPE TABLE OF vttp.

  CHECK NOT t_vttp[] IS INITIAL.

  tl_vttp[] = t_vttp[].
  SORT tl_vttp BY vbeln ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vttp COMPARING vbeln.

  SELECT *
    FROM vbfa
    INTO TABLE t_vbfa_va
    FOR ALL ENTRIES IN tl_vttp
  WHERE  vbeln   EQ tl_vttp-vbeln
    AND  vbtyp_n EQ 'J'
    AND  vbtyp_v EQ 'C'.

  SELECT *
    FROM vbfa
    INTO TABLE t_vbfa_vf
    FOR ALL ENTRIES IN tl_vttp
  WHERE  vbelv   EQ tl_vttp-vbeln
    AND  vbtyp_n EQ 'M'
    AND  vbtyp_v EQ 'J'.

  SORT t_vbfa_va BY vbeln ASCENDING
                    posnn ASCENDING.
  SORT t_vbfa_vf BY vbelv ASCENDING
                    posnv ASCENDING.

ENDFORM.                    " Z_SELECIONA_VBFA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIKP                                         *
*&---------------------------------------------------------------------*
*                         Seleciona Remessas                           *
*----------------------------------------------------------------------*
FORM z_seleciona_likp.

  DATA tl_vttp TYPE TABLE OF vttp.

  CHECK NOT t_vttp[] IS INITIAL.

  tl_vttp[] = t_vttp[].
  SORT tl_vttp BY vbeln ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vttp COMPARING vbeln.

  SELECT *
    FROM likp
    INTO TABLE t_likp
    FOR ALL ENTRIES IN tl_vttp
  WHERE  vbeln EQ tl_vttp-vbeln.

  SORT t_likp BY vbeln ASCENDING.

ENDFORM.                    " Z_SELECIONA_LIKP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIPS                                         *
*&---------------------------------------------------------------------*
*                         Seleciona Itens Remessa                      *
*----------------------------------------------------------------------*
FORM z_seleciona_lips.

  CHECK NOT t_likp[] IS INITIAL.

  SELECT *
    FROM lips
    INTO TABLE t_lips
    FOR ALL ENTRIES IN t_likp
  WHERE  vbeln EQ t_likp-vbeln.

  SORT t_lips BY vbeln ASCENDING
                 posnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_LIPS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIN                                          *
*&---------------------------------------------------------------------*
*                     Seleciona Itens Nota Fiscal                      *
*----------------------------------------------------------------------*
FORM z_seleciona_lin.

  DATA: BEGIN OF tl_vbfa OCCURS 0.
          INCLUDE TYPE vbfa.
  DATA    refkey TYPE j_1bnflin-refkey.
  DATA:  END  OF tl_vbfa.

  DATA: sl_vbfa  LIKE LINE OF tl_vbfa,
        vl_index TYPE i.

  CHECK NOT t_vbfa_vf[] IS INITIAL.
  tl_vbfa[] = t_vbfa_vf[].
  SORT tl_vbfa BY vbeln ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vbfa COMPARING vbeln.

  LOOP AT tl_vbfa INTO sl_vbfa.
    vl_index = sy-tabix.
    sl_vbfa-refkey = sl_vbfa-vbeln.
    MODIFY tl_vbfa FROM sl_vbfa
      INDEX vl_index
      TRANSPORTING refkey.
    CLEAR sl_vbfa.
  ENDLOOP.

  SELECT *
    FROM j_1bnflin
    INTO TABLE t_lin
    FOR ALL ENTRIES IN tl_vbfa
  WHERE  refkey EQ tl_vbfa-refkey
    AND  reftyp EQ 'BI'.

  SORT t_lin BY refkey ASCENDING
                refitm ASCENDING.

ENDFORM.                    " Z_SELECIONA_LIN

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DOC                                          *
*&---------------------------------------------------------------------*
*                      Seleciona Documentos NF                         *
*----------------------------------------------------------------------*
FORM z_seleciona_doc.

  DATA tl_lin TYPE TABLE OF j_1bnflin.

  CHECK NOT t_lin[] IS INITIAL.
  tl_lin[] = t_lin[].
  SORT tl_lin BY docnum ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_lin COMPARING docnum.

  SELECT *
    FROM j_1bnfdoc
    INTO TABLE t_doc
    FOR ALL ENTRIES IN tl_lin
  WHERE  docnum EQ tl_lin-docnum.

  SORT t_doc BY docnum ASCENDING.

ENDFORM.                    " Z_SELECIONA_DOC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VFKP                                         *
*&---------------------------------------------------------------------*
*                     Seleciona Itens Custo de Frete                   *
*----------------------------------------------------------------------*
FORM z_seleciona_vfkp.

  CHECK NOT t_vttk[] IS INITIAL.

  SELECT *
    FROM vfkp
    INTO TABLE t_vfkp
    FOR ALL ENTRIES IN t_vttk
  WHERE  rebel EQ t_vttk-tknum
    AND  refty EQ '8'
    AND  fkpty EQ 'Z001'.

  SORT t_vfkp BY rebel ASCENDING.

  IF t_vfkp[] IS INITIAL.
    MESSAGE i836 WITH text-006.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_VFKP


*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_KNA1                                         *
*&---------------------------------------------------------------------*
*                        Seleciona Dados Cliente                       *
*----------------------------------------------------------------------*
FORM z_seleciona_kna1.

  DATA tl_likp TYPE TABLE OF likp.

  CHECK NOT t_likp[] IS INITIAL.
  tl_likp[] = t_likp[].
  SORT tl_likp BY kunnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_likp COMPARING kunnr.

  SELECT *
    FROM kna1
    INTO TABLE t_kna1
    FOR ALL ENTRIES IN tl_likp
  WHERE  kunnr EQ tl_likp-kunnr.

  SORT t_kna1 BY kunnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_KNA1

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LANC                                         *
*&---------------------------------------------------------------------*
*                      Seleciona Dados Lançamentos                     *
*----------------------------------------------------------------------*
FORM z_seleciona_lanc.

  DATA tl_vttk TYPE TABLE OF vttk.

  CHECK NOT t_vttk[] IS INITIAL.
  tl_vttk[] = t_vttk[].
  SORT tl_vttk[] BY exti1 ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vttk COMPARING exti1.

  SELECT *
    FROM zlest0016
    INTO TABLE t_t0016
    FOR ALL ENTRIES IN tl_vttk
  WHERE  conhecimento EQ tl_vttk-exti1.

  SORT t_t0016 BY conhecimento ASCENDING.

ENDFORM.                    " Z_SELECIONA_LANC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_TTDS                                         *
*&---------------------------------------------------------------------*
*                  Seleciona Dados Org. Transporte                     *
*----------------------------------------------------------------------*
FORM z_seleciona_ttds.

  DATA tl_vttk TYPE TABLE OF vttk.

  CHECK NOT t_vttk[] IS INITIAL.
  tl_vttk[] = t_vttk[].
  SORT tl_vttk[] BY tplst ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vttk COMPARING tplst.

  SELECT *
    FROM ttds
    INTO TABLE t_ttds
    FOR ALL ENTRIES IN tl_vttk
  WHERE  tplst EQ tl_vttk-tplst.

  SORT t_ttds BY tplst ASCENDING.

  CHECK NOT t_ttds[] IS INITIAL.

  SELECT *
    FROM ttdst
    INTO TABLE t_ttdst
    FOR ALL ENTRIES IN t_ttds
  WHERE  spras EQ 'PT'
    AND  tplst EQ t_ttds-tplst.

  SORT t_ttdst BY tplst ASCENDING.

ENDFORM.                    " Z_SELECIONA_TTDS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ADRC                                         *
*&---------------------------------------------------------------------*
*                       Seleciona Dados de Endereço                    *
*----------------------------------------------------------------------*
FORM z_seleciona_adrc.

  DATA tl_ttds TYPE TABLE OF ttds.

  CHECK NOT t_ttds[] IS INITIAL.
  tl_ttds[] = t_ttds[].
  SORT tl_ttds[] BY adrnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_ttds COMPARING adrnr.

  SELECT *
    FROM adrc
    INTO TABLE t_adrc
    FOR ALL ENTRIES IN tl_ttds
  WHERE  addrnumber EQ tl_ttds-adrnr.

  SORT t_adrc BY addrnumber ASCENDING.

ENDFORM.                    " Z_SELECIONA_ADRC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VTPA                                         *
*&---------------------------------------------------------------------*
*                   Seleciona Parceiros Transporte                     *
*----------------------------------------------------------------------*
FORM z_seleciona_vtpa.

  CHECK NOT t_vttk[] IS INITIAL.

  SELECT *
    FROM vtpa
    INTO TABLE t_vtpa
    FOR ALL ENTRIES IN t_vttk
  WHERE  vbeln EQ t_vttk-tknum
    AND  parvw EQ 'MT'.

  SORT t_vtpa BY vbeln ASCENDING.

ENDFORM.                    " Z_SELECIONA_VTPA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LFA1                                         *
*&---------------------------------------------------------------------*
*                      Seleciona Dados Parceiros                       *
*----------------------------------------------------------------------*
FORM z_seleciona_lfa1.

  DATA: tl_vbpa TYPE TABLE OF vbpa,
        tl_vtpa TYPE TABLE OF vtpa.

  IF NOT t_vbpa_pc[] IS INITIAL .
    tl_vbpa[] = t_vbpa_pc[].
    SORT tl_vbpa BY lifnr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM tl_vbpa COMPARING lifnr.

    SELECT *
      FROM lfa1
      INTO TABLE t_lfa1_pc
      FOR ALL ENTRIES IN tl_vbpa
    WHERE  lifnr EQ tl_vbpa-lifnr.

    SORT t_lfa1_pc BY lifnr ASCENDING.

  ENDIF.

  IF NOT t_vbpa_lr[] IS INITIAL .
    tl_vbpa[] = t_vbpa_lr[].
    SORT tl_vbpa BY lifnr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM tl_vbpa COMPARING lifnr.

    SELECT *
      FROM lfa1
      INTO TABLE t_lfa1_lr
      FOR ALL ENTRIES IN tl_vbpa
    WHERE  lifnr EQ tl_vbpa-lifnr.

    SORT t_lfa1_lr BY lifnr ASCENDING.

  ENDIF.

  IF NOT t_vtpa[] IS INITIAL.
    tl_vtpa[] = t_vtpa[].
    SORT tl_vtpa BY lifnr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM tl_vtpa COMPARING lifnr.

    SELECT *
      FROM lfa1
      INTO TABLE t_lfa1_mt
      FOR ALL ENTRIES IN tl_vtpa
    WHERE  lifnr EQ tl_vtpa-lifnr.

    SORT t_lfa1_mt BY lifnr ASCENDING.

  ENDIF.

ENDFORM.                    " Z_SELECIONA_LFA1

*&---------------------------------------------------------------------*
*&      Form  Z_AGRUPA_DADOS                                           *
*&---------------------------------------------------------------------*
*                               Agrupa Dados                           *
*----------------------------------------------------------------------*
FORM z_agrupa_dados.

  DATA: sl_vttk    TYPE vttk,
        sl_vbfa_va TYPE vbfa,
        sl_vbfa_vf TYPE vbfa,
        sl_vttp    TYPE vttp,
        sl_doc     TYPE j_1bnfdoc,
        sl_lin     TYPE j_1bnflin,
        sl_lips    TYPE lips,
        sl_likp    TYPE likp,
        sl_kna1    TYPE kna1,
        sl_ttdst   TYPE ttdst,
        sl_lfa1_pc TYPE lfa1,
        sl_lfa1_lr TYPE lfa1,
        sl_tvro    TYPE tvro,
        sl_vfkp    TYPE vfkp,
        sl_vfsi    TYPE vfsi,
        sl_ttds    TYPE ttds,
        sl_alv     TYPE type_alv,
        sl_vbpa_pc TYPE vbpa,
        sl_vbpa_lr TYPE vbpa,
        sl_tvtkt   TYPE tvtkt,
        sl_makt    TYPE makt,
        sl_tvrot   TYPE tvrot,
        sl_adrc    TYPE adrc,
        sl_vtpa    TYPE vtpa,
        sl_lfa1_mt TYPE lfa1.

  REFRESH t_alv.

  LOOP AT t_vttk INTO sl_vttk.

    LOOP AT t_vttp INTO sl_vttp
      WHERE tknum EQ sl_vttk-tknum.

      READ TABLE t_vtpa INTO sl_vtpa
        WITH KEY vbeln = sl_vttk-tknum
        BINARY SEARCH.

      READ TABLE t_lfa1_mt INTO sl_lfa1_mt
        WITH KEY lifnr = sl_vtpa-lifnr
        BINARY SEARCH.

      READ TABLE t_vfkp INTO sl_vfkp
        WITH KEY rebel = sl_vttk-tknum
        BINARY SEARCH.

      READ TABLE t_vbfa_va INTO sl_vbfa_va
        WITH KEY vbeln = sl_vttp-vbeln
        BINARY SEARCH.

      READ TABLE t_vbfa_vf INTO sl_vbfa_vf
        WITH KEY vbelv = sl_vttp-vbeln
        BINARY SEARCH.

      READ TABLE t_likp INTO sl_likp
        WITH KEY vbeln = sl_vttp-vbeln
        BINARY SEARCH.

      READ TABLE t_lips INTO sl_lips
        WITH KEY vbeln = sl_likp-vbeln
        BINARY SEARCH.

      READ TABLE t_lin INTO sl_lin
        WITH KEY refkey = sl_vbfa_vf-vbeln
        BINARY SEARCH.

      READ TABLE t_doc INTO sl_doc
        WITH KEY docnum = sl_lin-docnum
        BINARY SEARCH.

      READ TABLE t_vfsi INTO sl_vfsi
        WITH KEY knumv = sl_vfkp-knumv
        BINARY SEARCH.

      READ TABLE t_vbpa_pc INTO sl_vbpa_pc
         WITH KEY vbeln = sl_vttp-vbeln
         BINARY SEARCH.

      READ TABLE t_lfa1_pc INTO sl_lfa1_pc
        WITH KEY lifnr = sl_vbpa_pc-lifnr
        BINARY SEARCH.

      READ TABLE t_kna1 INTO sl_kna1
        WITH KEY kunnr = sl_likp-kunnr
        BINARY SEARCH.

      READ TABLE t_vbpa_lr INTO sl_vbpa_lr
         WITH KEY vbeln = sl_vttp-vbeln
         BINARY SEARCH.

      READ TABLE t_lfa1_lr INTO sl_lfa1_lr
        WITH KEY lifnr = sl_vbpa_lr-lifnr
        BINARY SEARCH.

      READ TABLE t_ttds INTO sl_ttds
        WITH KEY tplst = sl_vttk-tplst
        BINARY SEARCH.

      READ TABLE t_ttdst INTO sl_ttdst
        WITH KEY tplst = sl_ttds-tplst
        BINARY SEARCH.

      READ TABLE t_tvtkt INTO sl_tvtkt
        WITH KEY shtyp = sl_vttk-shtyp
        BINARY SEARCH.

      READ TABLE t_makt INTO sl_makt
        WITH KEY matnr = sl_lips-matnr
        BINARY SEARCH.

      READ TABLE t_tvrot INTO sl_tvrot
        WITH KEY route  = sl_vttk-route
        BINARY SEARCH.

      READ TABLE t_adrc INTO sl_adrc
        WITH KEY addrnumber = sl_ttds-adrnr
        BINARY SEARCH.

*   Leitura KONV
      PERFORM z_leitura_konv USING sl_vfsi-knumv
                                   sl_vfsi-kposn
                          CHANGING sl_alv.

      sl_alv-vbeln_va        = sl_vbfa_va-vbelv.
      sl_alv-vbeln           = sl_vttp-vbeln.
      sl_alv-vbeln_vf        = sl_vbfa_vf-vbeln.
      sl_alv-nfenum          = sl_doc-nfenum.
      sl_alv-netwr_lin       = sl_lin-netwr.
      sl_alv-series          = sl_doc-series.
      sl_alv-maktx           = sl_makt-maktx.
      sl_alv-lfimg           = sl_lips-lfimg.
      sl_alv-vrkme           = sl_lips-vrkme.
      sl_alv-brgew           = sl_lips-brgew.
      sl_alv-city1           = sl_adrc-city1.
      sl_alv-region          = sl_adrc-region.
      sl_alv-name1_kn        = sl_kna1-name1.
      sl_alv-stcd1_kn        = sl_kna1-stcd1.
      sl_alv-stcd2_kn        = sl_kna1-stcd2.
      sl_alv-ort01_kn        = sl_kna1-ort01.
      sl_alv-regio_kn        = sl_kna1-regio.
      sl_alv-bezei           = sl_ttdst-bezei.
      sl_alv-tknum           = sl_vttk-tknum.
      sl_alv-name1_lf        = sl_lfa1_pc-name1.
      sl_alv-exti1           = sl_vttk-exti1.
      sl_alv-exti2           = sl_vttk-exti2.
      sl_alv-signi           = sl_vttk-signi.
      sl_alv-text1           = sl_vttk-text1.
      sl_alv-name1_mt        = sl_lfa1_mt-name1.
      sl_alv-route           = sl_tvro-route.
      sl_alv-datbg           = sl_vttk-datbg.
      sl_alv-fknum           = sl_vfkp-fknum.
      sl_alv-netwr           = sl_vfsi-netwr.
      sl_alv-mwsbp           = sl_vfsi-mwsbp.
      sl_alv-ebeln           = sl_vfkp-ebeln.
      sl_alv-lblni           = sl_vfkp-lblni.

      APPEND sl_alv TO t_alv.

      CLEAR: sl_vbfa_va,
             sl_vbfa_vf,
             sl_vttp   ,
             sl_doc    ,
             sl_lin    ,
             sl_lips   ,
             sl_likp   ,
             sl_kna1   ,
             sl_ttdst  ,
             sl_lfa1_pc,
             sl_lfa1_lr,
             sl_tvro   ,
             sl_vfkp   ,
             sl_vfsi   ,
             sl_ttds   ,
             sl_vbpa_pc,
             sl_vbpa_lr,
             sl_tvtkt  ,
             sl_makt   ,
             sl_tvrot  ,
             sl_adrc   .

    ENDLOOP.

    CLEAR sl_vttk.

  ENDLOOP.

ENDFORM.                    " Z_AGRUPA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_ESTRUTURAS_ALV                                         *
*&---------------------------------------------------------------------*
*                              Estruturas ALV                          *
*----------------------------------------------------------------------*
FORM z_estruturas_alv.

  DATA: vl_campo1 TYPE char50,
        vl_campo2 TYPE char50.

  REFRESH t_fcat.

* Monta Layout
  PERFORM z_layout.

  IF NOT rb_out IS INITIAL.
    vl_campo1 = text-007.
    vl_campo2 = text-008.
  ELSE.
    vl_campo1 = text-039.
    vl_campo2 = text-040.
  ENDIF.

* Preenche FieldCat
  PERFORM z_preenche_fieldcat USING:
    'VBELN_VA'        vl_campo1 10,
    'VBELN'           vl_campo2 10,
    'VBELN_VF'        text-009 10,
    'NFENUM'          text-010 09,
    'SERIES'          text-011 05,
    'NETWR_LIN'       text-058 16,
    'MAKTX'           text-041 20,
    'LFIMG'           text-042 20,
    'VRKME'           text-043 10,
    'BRGEW'           text-059 10,
    'CITY1'           text-060 13,
    'REGION'          text-061 10,
    'NAME1_KN'        text-018 20,
    'STCD1_KN'        text-019 16,
    'STCD2_KN'        text-020 11,
    'ORT01_KN'        text-062 14,
    'REGIO_KN'        text-063 10,
    'BEZEI'           text-023 20,
    'TKNUM'           text-024 10,
    'NAME1_LF'        text-025 20,
    'EXTI1'           text-026 20,
    'EXTI2'           text-027 20,
    'SIGNI'           text-028 20,
    'TEXT1'           text-029 26,
    'NAME1_MT'        text-030 20,
    'ROUTE'           text-064 10,
    'DATBG'           text-033 10,
    'FKNUM'           text-034 10,
    'NETWR'           text-035 21,
    'MWSBP'           text-036 18,
    'KWERT_SEG'       text-047 14,
    'KWERT_IOF'       text-049 14,
    'EBELN'           text-037 10,
    'LBLNI'           text-038 10.

  IF rb_out IS INITIAL.
    DELETE t_fcat INDEX 3 .
  ENDIF.

ENDFORM.                    " Z_ESTRUTURAS_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM z_layout.

  CLEAR s_layout.

  s_layout-zebra = 'X'.

ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT                                      *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat                          *
*----------------------------------------------------------------------*
FORM z_preenche_fieldcat USING p_field TYPE c
                               p_desc  TYPE c
                               p_len   TYPE n.

  DATA sl_fcat TYPE lvc_s_fcat.

  sl_fcat-tabname   = c_table.
  sl_fcat-fieldname = p_field.
  sl_fcat-scrtext_l = p_desc.
  sl_fcat-scrtext_m = p_desc.
  sl_fcat-scrtext_s = p_desc.
  sl_fcat-outputlen = p_len.

  APPEND sl_fcat TO t_fcat.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR  'TB0100'.
  ENDCASE.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_OBJ_ALV  OUTPUT                                     *
*&---------------------------------------------------------------------*
*                                 Obj Alv                              *
*----------------------------------------------------------------------*
MODULE zm_obj_alv OUTPUT.

* Instancia Container
  PERFORM: z_inst_cont ,
* Instancia Alv
           z_inst_alv  ,
* Exibe Alv
           z_exibe_alv .

ENDMODULE.                 " ZM_OBJ_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_CONT                                              *
*&---------------------------------------------------------------------*
*                       Instancia Container                            *
*----------------------------------------------------------------------*
FORM z_inst_cont.

  CHECK s_cont IS INITIAL.

  CREATE OBJECT s_cont
    EXPORTING
      container_name              = 'CC_ALV'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH text-013.
  ENDIF.

ENDFORM.                    " Z_INST_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_ALV                                               *
*&---------------------------------------------------------------------*
*                              Instancia Alv                           *
*----------------------------------------------------------------------*
FORM z_inst_alv.

  CHECK s_alv IS INITIAL.

  CREATE OBJECT s_alv
    EXPORTING
      i_parent          = s_cont
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH text-014.
  ENDIF.

ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  DATA vl_int TYPE int4.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
*     i_save                        =
      i_default                     = 'X'
      is_layout                     = s_layout
    CHANGING
      it_outtab                     = t_alv
      it_fieldcatalog               = t_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

*  CALL METHOD s_alv->set_ready_for_input.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_SAIDA_2                                      *
*&---------------------------------------------------------------------*
*                          Seleciona Dados Saída                       *
*----------------------------------------------------------------------*
FORM z_seleciona_saida_2 USING p_tipo TYPE i.

* Seleciona Documentos de Transporte
  PERFORM: z_seleciona_vttk
               USING p_tipo,
* Seleciona Parceiros Transporte
           z_seleciona_vtpa,
* Seleciona Itens do Transporte
           z_seleciona_vttp,
* Seleciona Remessas
           z_seleciona_likp,
* Seleciona Itens Remessa
           z_seleciona_lips,
* Seleciona Documento Material
           z_seleciona_ekbe,
* Seleciona Itens Nota Fiscal
           z_seleciona_lin2,
* Seleciona Documentos NF
           z_seleciona_doc2,
* Seleciona Itens Custo de Frete
           z_seleciona_vfkp,
* Seleciona Categoria Item
           z_seleciona_vfsi,
* Seleciona Condições de Preço
           z_seleciona_konv,
* Seleciona Dados Parceiro
           z_seleciona_vbpa,
* Seleciona Dados Parceiros
           z_seleciona_lfa1,
* Seleciona Dados Org. Transporte
           z_seleciona_ttds,
* Seleciona Dados de Endereço
           z_seleciona_adrc,
* Seleciona Descrição dos Materiais
           z_seleciona_makt,
* Seleciona Descrição Itinerários
           z_seleciona_tvrot,
* Seleciona Texto Tipo Transporte
           z_seleciona_tvtkt.

ENDFORM.                    " Z_SELECIONA_SAIDA_2

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_EKBE                                         *
*&---------------------------------------------------------------------*
*                     Seleciona Documento Material                     *
*----------------------------------------------------------------------*
FORM z_seleciona_ekbe.

  DATA tl_lips TYPE TABLE OF lips.

  CHECK NOT t_lips[] IS INITIAL.
  tl_lips[] = t_lips[].
  SORT tl_lips BY vgbel ASCENDING
                  vgpos ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_lips COMPARING vgbel vgpos.

  SELECT *
    FROM ekbe
    INTO TABLE t_ekbe
    FOR ALL ENTRIES IN tl_lips
  WHERE  ebeln EQ tl_lips-vgbel
    AND  bewtp EQ 'Q'.

  SORT t_ekbe BY ebeln ASCENDING
                 ebelp ASCENDING.

ENDFORM.                    " Z_SELECIONA_EKBE

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIN2                                         *
*&---------------------------------------------------------------------*
*                      Seleciona Itens Nota Fiscal                     *
*----------------------------------------------------------------------*
FORM z_seleciona_lin2.

  DATA: BEGIN OF tl_ekbe OCCURS 0.
          INCLUDE TYPE ekbe.
  DATA    refkey TYPE j_1bnflin-refkey.
  DATA:  END  OF tl_ekbe.

  DATA: sl_ekbe  LIKE LINE OF tl_ekbe,
        vl_index TYPE i.

  CHECK NOT t_ekbe[] IS INITIAL.
  tl_ekbe[] = t_ekbe[].
  SORT tl_ekbe BY belnr ASCENDING
                  gjahr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_ekbe COMPARING belnr gjahr.

  LOOP AT tl_ekbe INTO sl_ekbe.
    vl_index = sy-tabix.
    CONCATENATE sl_ekbe-belnr
                sl_ekbe-gjahr
           INTO sl_ekbe-refkey.
    MODIFY tl_ekbe FROM sl_ekbe
      INDEX vl_index
      TRANSPORTING refkey.
    CLEAR sl_ekbe.
  ENDLOOP.

  SELECT *
    FROM j_1bnflin
    INTO TABLE t_lin
    FOR ALL ENTRIES IN tl_ekbe
  WHERE  refkey EQ tl_ekbe-refkey
    AND  reftyp EQ 'MD'.

  SORT t_lin BY refkey ASCENDING
                refitm ASCENDING.

ENDFORM.                    " Z_SELECIONA_LIN2

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DOC2                                         *
*&---------------------------------------------------------------------*
*                          Seleciona Documentos NF                     *
*----------------------------------------------------------------------*
FORM z_seleciona_doc2.

  DATA tl_lin TYPE TABLE OF j_1bnflin.

  CHECK NOT t_lin[] IS INITIAL.
  tl_lin[] = t_lin[].
  SORT tl_lin BY docnum ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_lin COMPARING docnum.

  SELECT *
    FROM j_1bnfdoc
    INTO TABLE t_doc
    FOR ALL ENTRIES IN tl_lin
  WHERE  docnum EQ tl_lin-docnum.

  SORT t_doc BY docnum ASCENDING.

ENDFORM.                    " Z_SELECIONA_DOC2

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LFA12                                        *
*&---------------------------------------------------------------------*
*                       Seleciona Dados Parceiros                      *
*----------------------------------------------------------------------*
FORM z_seleciona_lfa12.

  DATA tl_likp TYPE TABLE OF likp.

  CHECK NOT t_likp[] IS INITIAL.
  tl_likp[] = t_likp[].
  SORT tl_likp BY lifnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_likp COMPARING lifnr.

  SELECT *
    FROM lfa1
    INTO TABLE t_lfa1_2
    FOR ALL ENTRIES IN tl_likp
  WHERE  lifnr EQ tl_likp-lifnr.

  SORT t_lfa1_2 BY lifnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_LFA12

*&---------------------------------------------------------------------*
*&      Form  Z_AGRUPA_DADOS_2                                         *
*&---------------------------------------------------------------------*
*                               Agrupa Dados                           *
*----------------------------------------------------------------------*
FORM z_agrupa_dados_2.

  DATA: sl_vttk    TYPE vttk,
        sl_vbfa_va TYPE vbfa,
        sl_vbfa_vf TYPE vbfa,
        sl_vttp    TYPE vttp,
        sl_doc     TYPE j_1bnfdoc,
        sl_lin     TYPE j_1bnflin,
        sl_lips    TYPE lips,
        sl_likp    TYPE likp,
        sl_kna1    TYPE kna1,
        sl_ttdst   TYPE ttdst,
        sl_lfa1_pc TYPE lfa1,
        sl_lfa1_lr TYPE lfa1,
        sl_tvro    TYPE tvro,
        sl_vfkp    TYPE vfkp,
        sl_vfsi    TYPE vfsi,
        sl_ttds    TYPE ttds,
        sl_alv     TYPE type_alv,
        sl_vbpa_pc TYPE vbpa,
        sl_vbpa_lr TYPE vbpa,
        sl_tvtkt   TYPE tvtkt,
        sl_makt    TYPE makt,
        sl_tvrot   TYPE tvrot,
        sl_ekbe    TYPE ekbe,
        vl_refkey  TYPE j_1brefkey,
        sl_adrc    TYPE adrc,
        sl_lfa1_mt TYPE lfa1,
        sl_vtpa    TYPE vtpa.

  REFRESH t_alv.

  LOOP AT t_vttk INTO sl_vttk.

    LOOP AT t_vttp INTO sl_vttp
      WHERE tknum EQ sl_vttk-tknum.

      READ TABLE t_vtpa INTO sl_vtpa
        WITH KEY vbeln = sl_vttk-tknum
        BINARY SEARCH.

      READ TABLE t_lfa1_mt INTO sl_lfa1_mt
        WITH KEY lifnr = sl_vtpa-lifnr
        BINARY SEARCH.

      READ TABLE t_vfkp INTO sl_vfkp
        WITH KEY rebel = sl_vttk-tknum
        BINARY SEARCH.

      READ TABLE t_vbfa_va INTO sl_vbfa_va
        WITH KEY vbeln = sl_vttp-vbeln
        BINARY SEARCH.

      READ TABLE t_vbfa_vf INTO sl_vbfa_vf
        WITH KEY vbelv = sl_vttp-vbeln
        BINARY SEARCH.

      READ TABLE t_likp INTO sl_likp
        WITH KEY vbeln = sl_vttp-vbeln
        BINARY SEARCH.

      READ TABLE t_lips INTO sl_lips
        WITH KEY vbeln = sl_likp-vbeln
        BINARY SEARCH.

      READ TABLE t_ekbe INTO sl_ekbe
        WITH KEY ebeln = sl_lips-vgbel
                 ebelp = sl_lips-vgpos
        BINARY SEARCH.

      CONCATENATE sl_ekbe-belnr
                  sl_ekbe-gjahr
             INTO vl_refkey.

      READ TABLE t_lin INTO sl_lin
        WITH KEY refkey = vl_refkey
        BINARY SEARCH.

      READ TABLE t_doc INTO sl_doc
        WITH KEY docnum = sl_lin-docnum
        BINARY SEARCH.

      READ TABLE t_vfsi INTO sl_vfsi
        WITH KEY knumv = sl_vfkp-knumv
        BINARY SEARCH.

      READ TABLE t_vbpa_pc INTO sl_vbpa_pc
         WITH KEY vbeln = sl_vttp-vbeln
         BINARY SEARCH.

      READ TABLE t_lfa1_pc INTO sl_lfa1_pc
        WITH KEY lifnr = sl_vbpa_pc-lifnr
        BINARY SEARCH.

      READ TABLE t_kna1 INTO sl_kna1
        WITH KEY kunnr = sl_likp-kunnr
        BINARY SEARCH.

      READ TABLE t_vbpa_lr INTO sl_vbpa_lr
         WITH KEY vbeln = sl_vttp-vbeln
         BINARY SEARCH.

      READ TABLE t_lfa1_lr INTO sl_lfa1_lr
        WITH KEY lifnr = sl_vbpa_lr-lifnr
        BINARY SEARCH.

      READ TABLE t_ttds INTO sl_ttds
        WITH KEY tplst = sl_vttk-tplst
        BINARY SEARCH.

      READ TABLE t_ttdst INTO sl_ttdst
        WITH KEY tplst = sl_ttds-tplst
        BINARY SEARCH.

      READ TABLE t_tvtkt INTO sl_tvtkt
        WITH KEY shtyp = sl_vttk-shtyp
        BINARY SEARCH.

      READ TABLE t_makt INTO sl_makt
        WITH KEY matnr = sl_lips-matnr
        BINARY SEARCH.

      READ TABLE t_tvrot INTO sl_tvrot
        WITH KEY route  = sl_vttk-route
        BINARY SEARCH.

      READ TABLE t_adrc INTO sl_adrc
        WITH KEY addrnumber  = sl_ttds-adrnr
        BINARY SEARCH.

*   Leitura KONV
      PERFORM z_leitura_konv USING sl_vfsi-knumv
                                   sl_vfsi-kposn
                          CHANGING sl_alv.

      sl_alv-vbeln_va        = sl_ekbe-belnr.
      sl_alv-vbeln           = sl_vttp-vbeln.
      sl_alv-nfenum          = sl_doc-nfenum.
      sl_alv-netwr_lin       = sl_lin-netwr.
      sl_alv-series          = sl_doc-series.
      sl_alv-maktx           = sl_makt-maktx.
      sl_alv-lfimg           = sl_lips-lfimg.
      sl_alv-vrkme           = sl_lips-vrkme.
      sl_alv-brgew           = sl_lips-brgew.
      sl_alv-name1_kn        = sl_kna1-name1.
      sl_alv-stcd1_kn        = sl_kna1-stcd1.
      sl_alv-stcd2_kn        = sl_kna1-stcd2.
      sl_alv-ort01_kn        = sl_kna1-ort01.
      sl_alv-regio_kn        = sl_kna1-regio.
      sl_alv-city1           = sl_adrc-city1.
      sl_alv-region          = sl_adrc-region.
      sl_alv-bezei           = sl_ttdst-bezei.
      sl_alv-tknum           = sl_vttk-tknum.
      sl_alv-name1_lf        = sl_lfa1_pc-name1.
      sl_alv-exti1           = sl_vttk-exti1.
      sl_alv-exti2           = sl_vttk-exti2.
      sl_alv-signi           = sl_vttk-signi.
      sl_alv-text1           = sl_vttk-text1.
      sl_alv-name1_mt        = sl_lfa1_mt-name1.
      sl_alv-route           = sl_tvro-route.
      sl_alv-datbg           = sl_vttk-datbg.
      sl_alv-fknum           = sl_vfkp-fknum.
      sl_alv-netwr           = sl_vfsi-netwr.
      sl_alv-mwsbp           = sl_vfsi-mwsbp.
      sl_alv-ebeln           = sl_vfkp-ebeln.
      sl_alv-lblni           = sl_vfkp-lblni.

      APPEND sl_alv TO t_alv.

      CLEAR: sl_vbfa_va,
             sl_vbfa_vf,
             sl_vttp   ,
             sl_doc    ,
             sl_lin    ,
             sl_lips   ,
             sl_likp   ,
             sl_kna1   ,
             sl_ttdst  ,
             sl_lfa1_pc,
             sl_lfa1_lr,
             sl_lfa1_mt,
             sl_tvro   ,
             sl_vfkp   ,
             sl_vfsi   ,
             sl_ttds   ,
             sl_vbpa_pc,
             sl_vbpa_lr,
             sl_tvtkt  ,
             sl_makt   ,
             sl_tvrot  ,
             sl_ekbe   ,
             sl_adrc   .

    ENDLOOP.

    CLEAR sl_vttk.

  ENDLOOP.

ENDFORM.                    " Z_AGRUPA_DADOS_2

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VFSI                                         *
*&---------------------------------------------------------------------*
*                      Seleciona Categoria Item                        *
*----------------------------------------------------------------------*
FORM z_seleciona_vfsi.

  DATA tl_vfkp TYPE TABLE OF vfkp.

  CHECK NOT t_vfkp[] IS INITIAL.
  tl_vfkp[] = t_vfkp[].
  SORT tl_vfkp BY knumv ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vfkp COMPARING knumv.

  SELECT *
    FROM vfsi
    INTO TABLE t_vfsi
    FOR ALL ENTRIES IN tl_vfkp
  WHERE  knumv EQ tl_vfkp-knumv.

  SORT t_vfsi BY knumv ASCENDING
                 kposn ASCENDING.

ENDFORM.                    " Z_SELECIONA_VFSI

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_KONV                                         *
*&---------------------------------------------------------------------*
*                     Seleciona Condições de Preço                     *
*----------------------------------------------------------------------*
FORM z_seleciona_konv.

  CHECK NOT t_vfsi[] IS INITIAL.

  SELECT FROM V_KONV FIELDS * FOR ALL ENTRIES IN @T_VFSI WHERE KNUMV EQ @T_VFSI-KNUMV AND KPOSN EQ @T_VFSI-KPOSN INTO CORRESPONDING FIELDS OF TABLE @T_KONV .

  SORT t_konv BY knumv ASCENDING
                 kposn ASCENDING
                 kschl ASCENDING.

ENDFORM.                    " Z_SELECIONA_KONV

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBPA                                         *
*&---------------------------------------------------------------------*
*                         Seleciona Dados Parceiro                     *
*----------------------------------------------------------------------*
FORM z_seleciona_vbpa.

  DATA tl_vttp TYPE TABLE OF vttp.

  CHECK NOT t_vttp[] IS INITIAL.
  tl_vttp[] = t_vttp[].
  SORT tl_vttp BY vbeln ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vttp COMPARING vbeln.

  SELECT *
    FROM vbpa
    INTO TABLE t_vbpa_pc
    FOR ALL ENTRIES IN tl_vttp
  WHERE  vbeln EQ tl_vttp-vbeln.

  t_vbpa_lr[] = t_vbpa_pc[].
  DELETE: t_vbpa_pc WHERE parvw NE 'PC',
          t_vbpa_lr WHERE parvw NE 'LR'.
  SORT: t_vbpa_pc BY vbeln ASCENDING,
        t_vbpa_lr BY vbeln ASCENDING.

ENDFORM.                    " Z_SELECIONA_VBPA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MAKT                                         *
*&---------------------------------------------------------------------*
*                  Seleciona Descrição dos Materiais                   *
*----------------------------------------------------------------------*
FORM z_seleciona_makt.

  DATA tl_lips TYPE TABLE OF lips.

  CHECK NOT t_lips[] IS INITIAL.
  tl_lips[] = t_lips[].
  SORT tl_lips BY matnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_lips COMPARING matnr.

  SELECT *
    FROM makt
    INTO TABLE t_makt
    FOR ALL ENTRIES IN tl_lips
  WHERE  spras EQ sy-langu
    AND  matnr EQ tl_lips-matnr.

  SORT t_makt BY matnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_MAKT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_TVROT                                        *
*&---------------------------------------------------------------------*
*                   Selecionar Descrição Itinerários                   *
*----------------------------------------------------------------------*
FORM z_seleciona_tvrot.


  DATA tl_vttk TYPE TABLE OF vttk.

  CHECK NOT t_vttk[] IS INITIAL.
  tl_vttk[] = t_vttk[].
  SORT tl_vttk BY route ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vttk COMPARING route.

  SELECT *
    FROM tvrot
    INTO TABLE t_tvrot
    FOR ALL ENTRIES IN tl_vttk
  WHERE  spras EQ 'PT'
    AND  route EQ tl_vttk-route.

  SORT t_tvrot BY route ASCENDING.

ENDFORM.                    " Z_SELECIONA_TVROT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_TVTKT                                        *
*&---------------------------------------------------------------------*
*                  Seleciona Texto Tipo Transporte                     *
*----------------------------------------------------------------------*
FORM z_seleciona_tvtkt.

  DATA tl_vttk TYPE TABLE OF vttk.

  CHECK NOT t_vttk[] IS INITIAL.
  tl_vttk[] = t_vttk[].
  SORT tl_vttk BY shtyp ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vttk COMPARING shtyp.

  SELECT *
    FROM tvtkt
    INTO TABLE t_tvtkt
    FOR ALL ENTRIES IN tl_vttk
  WHERE  spras EQ 'PT'
    AND  shtyp EQ tl_vttk-shtyp.

  SORT t_tvtkt BY shtyp ASCENDING.

ENDFORM.                    " Z_SELECIONA_TVTKT

*&---------------------------------------------------------------------*
*&      Form  Z_LEITURA_KONV                                           *
*&---------------------------------------------------------------------*
*                                Leitura KONV                          *
*----------------------------------------------------------------------*
FORM z_leitura_konv USING p_knumv TYPE vfsi-knumv
                          p_kposn TYPE vfsi-kposn
                 CHANGING p_alv   TYPE type_alv.

  DATA sl_konv TYPE konv.

* Leitura Condições
  PERFORM z_ler_cond USING p_knumv
                           p_kposn
                          'ZSEG'
                  CHANGING p_alv-kwert_seg.

  PERFORM z_ler_cond USING p_knumv
                           p_kposn
                          'ZIOF'
                  CHANGING p_alv-kwert_iof.

ENDFORM.                    " Z_LEITURA_KONV

*&---------------------------------------------------------------------*
*&      Form  Z_LER_COND                                               *
*&---------------------------------------------------------------------*
*                              Ler Condições                           *
*----------------------------------------------------------------------*
FORM z_ler_cond USING p_knumv TYPE vfsi-knumv
                      p_kposn TYPE vfsi-kposn
                      p_kschl TYPE c
             CHANGING p_kwert TYPE konv-kwert.


  DATA sl_konv TYPE konv.

  READ TABLE t_konv INTO sl_konv
    WITH KEY knumv = p_knumv
             kposn = p_kposn
             kschl = p_kschl
    BINARY SEARCH.

  CHECK sy-subrc IS INITIAL.

  p_kwert = sl_konv-kwert.

ENDFORM.                    " Z_LER_COND
