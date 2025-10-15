*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
* Cliente....: AMAGGI                                                  *
* Autor......: Cristiane Israel                                        *
* Data.......: 12.08.2010                                              *
* Descrição  : Consulta de Carregamento  por valor                     *
* Transação..: ZLES00019                                               *
* Projeto....:                                                         *
* Cód Espec..:                                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Autor      :                                                         *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT  zlesr0006.

*&---------------------------------------------------------------------*
*&      Tables
*&---------------------------------------------------------------------*
TABLES: ttds, vttk, lips.

TYPE-POOLS: slis.

*&---------------------------------------------------------------------*
*&      Types
*&---------------------------------------------------------------------*
TYPES: BEGIN OF type_saida,
         ordem   	TYPE vbfa-vbeln,
         remessa  TYPE vttp-vbeln,
         fat      TYPE vbfa-vbeln,
         nf       TYPE j_1bnfdoc-nfenum,
         serie     TYPE j_1bnfdoc-series,
         peso_sa   TYPE lips-brgew,
         peso_che  TYPE brgew_15,
         peso_dif  TYPE brgew_15,
         valor_ton TYPE brgew_15,
         valor_tot TYPE	brgew_15,
         data_che  TYPE sy-datum,
         cid_or    TYPE adrc-city1,
         uf_origem TYPE adrc-region,
         cliente   TYPE kna1-name1,
         cnpj      TYPE kna1-stcd1,
         inscr_est TYPE kna1-stcd2,
         cid_dest  TYPE	kna1-ort01,
         uf_dest   TYPE kna1-regio,
         bezei     TYPE ttdst-bezei,
         tknum     TYPE vttk-tknum,
         transp    TYPE lfa1-name1,
         conhec    TYPE vttk-exti1,
         carta     TYPE vttk-exti2,
         lote      TYPE vttk-signi,
         placa     TYPE vttk-tndr_trkid,
         motorista  TYPE vttk-tpbez,
         proprietario TYPE lfa1-lifnr,
         itinerario  TYPE tvro-route,
         data_trans  TYPE vttk-datbg,
         doc_frete   TYPE vfkp-fknum,
         vlr_frete   TYPE vfsi-netwr,
         vlr_imp     TYPE vfsi-mwsbp,
         pedido      TYPE vfkp-ebeln,
         folha       TYPE vfkp-lblni,
       END OF type_saida,

* Fluxo de documentos de vendas e distribuição
      BEGIN OF  type_vbfa,
        vbelv TYPE vbfa-vbelv,
        posnv TYPE vbfa-posnv,
        vbeln TYPE vbfa-vbeln ,
        posnn TYPE vbfa-posnn ,
        vbtyp_n TYPE vbfa-vbtyp_n,
        vbtyp_v TYPE vbfa-vbtyp_v,
        refkey TYPE j_1bnflin-refkey,
      END OF type_vbfa,

* Item de transporte
      BEGIN OF  type_vttp,
        tknum TYPE vttp-tknum,
        vbeln TYPE vttp-vbeln,
       END OF type_vttp,

* Cabeçalho da nota fiscal
      BEGIN OF   type_doc,
        docnum TYPE j_1bnfdoc-docnum,
        series TYPE j_1bnfdoc-series,
        nfenum TYPE j_1bnfdoc-nfenum,
       END OF type_doc,

* Partidas individuais da nota fiscal
       BEGIN OF type_lin,
        docnum TYPE j_1bnflin-docnum,
        itmnum TYPE j_1bnflin-itmnum,
        refkey TYPE j_1bnflin-refkey,
        reftyp TYPE j_1bnflin-reftyp ,
       END OF type_lin,

*Documento SD: fornecimento: dados de item
      BEGIN OF  type_lips,
        vbeln TYPE lips-vbeln,
        posnr TYPE lips-posnr,
        lgort TYPE lips-lgort,
        brgew TYPE lips-brgew ,
        vgbel TYPE lips-vgbel,
        vgpos TYPE lips-vgpos,
        ebelp TYPE ekbe-ebelp,
       END OF  type_lips,

* Documento SD: fornecimento: dados de cabeçalho
      BEGIN OF type_likp,
        vbeln TYPE likp-vbeln,
        kunnr TYPE likp-kunnr,
        lifnr TYPE likp-lifnr,
      END OF type_likp,

* Posto - Lançamentos
      BEGIN OF  type_0016,
        conhecimento TYPE zlest0016-conhecimento,
        peso_confirmado TYPE zlest0016-peso_confirmado,
        vlr_confirmado TYPE zlest0016-vlr_confirmado,
        dta_chegada TYPE zlest0016-dta_chegada,
      END OF  type_0016,

*Unidade organizacional: locais de organização do transporte
      BEGIN OF type_ttds,
        tplst TYPE ttds-tplst,
        adrnr TYPE ttds-adrnr,
        bukrs TYPE ttds-bukrs,
      END OF type_ttds,

*Mestre de clientes (parte geral)
      BEGIN OF  type_kna1,
        kunnr TYPE kna1-kunnr,
        name1 TYPE kna1-name1,
        ort01 TYPE kna1-ort01,
        regio TYPE kna1-regio,
        stcd1 TYPE kna1-stcd1,
        stcd2 TYPE kna1-stcd2,
       END OF type_kna1,

*Endereços (administração de endereços central)
      BEGIN OF  type_adrc,
        addrnumber TYPE adrc-addrnumber,
        city1 TYPE adrc-city1,
        region TYPE adrc-region,
       END OF type_adrc,

*Unidade organizac.: locais de organização do transp.: textos
      BEGIN OF   type_ttdst,
        spras TYPE ttdst-spras,
        tplst TYPE ttdst-tplst,
        bezei TYPE ttdst-bezei,
       END OF  type_ttdst,

* Mestre de fornecedores (parte geral)
      BEGIN OF  type_lfa1,
        lifnr TYPE lfa1-lifnr,
        name1 TYPE lfa1-name1,
        ort01 TYPE lfa1-ort01,
        regio TYPE lfa1-regio,
        stcd1 TYPE lfa1-stcd1,
        stcd2 TYPE lfa1-stcd2,
         END OF  type_lfa1,

* Cabeçalho transporte
      BEGIN OF   type_vttk,
        tknum TYPE vttk-tknum,
        shtyp	TYPE vttk-shtyp,
        tplst	TYPE vttk-tplst,
        erdat TYPE vttk-erdat,
         abfer TYPE vttk-abfer,
        route TYPE vttk-route,
        signi TYPE vttk-signi,
        exti1 TYPE vttk-exti1,
        exti2 TYPE vttk-exti2,
        tpbez TYPE vttk-tpbez,
        datbg  TYPE vttk-datbg,
        tdlnr TYPE vttk-tdlnr,
        tndr_trkid TYPE vttk-tndr_trkid,
       END OF  type_vttk,

* Custos de frete: dados do item
      BEGIN OF  type_vfkp,
        fknum TYPE vfkp-fknum,
        fkpos TYPE vfkp-fkpos,
        fkpty TYPE vfkp-fkpty,
        knumv TYPE vfkp-knumv,
        ebeln TYPE vfkp-ebeln,
        lblni TYPE vfkp-lblni,
        rebel TYPE vfkp-rebel,
        repos TYPE vfkp-repos,
      END OF type_vfkp,

* Custos de frete: dados dos subitens
      BEGIN OF   type_vfsi,
        knumv TYPE vfsi-knumv,
        kposn TYPE vfsi-kposn,
        vbeln TYPE vfsi-vbeln,
        posnr TYPE vfsi-posnr,
        netwr TYPE vfsi-netwr,
        mwsbp TYPE vfsi-mwsbp,
        kzwi1 TYPE vfsi-kzwi1,
        kzwi2 TYPE vfsi-kzwi2,
       END OF type_vfsi,

* Histórico para o documento de compra
       BEGIN OF  type_ekbe,
        ebeln TYPE ekbe-ebeln,
        ebelp TYPE ekbe-ebelp,
        gjahr TYPE ekbe-gjahr,
        belnr TYPE ekbe-belnr,
        bewtp TYPE ekbe-bewtp,
        refkey TYPE j_1bnflin-refkey,
       END OF  type_ekbe,

*Parceiro de transporte
       BEGIN OF type_vtpa,
         vbeln TYPE vtpa-vbeln,
         posnr TYPE vtpa-posnr,
         parvw TYPE vtpa-parvw,
         lifnr TYPE vtpa-lifnr,
       END OF type_vtpa.

*&---------------------------------------------------------------------*
*&      Tabelas internas
*&---------------------------------------------------------------------*
DATA: ti_saida    TYPE TABLE OF type_saida,
      ti_vbfa_va  TYPE TABLE OF type_vbfa,
      ti_vttp     TYPE TABLE OF type_vttp,
      ti_vbfa_vf  TYPE TABLE OF type_vbfa,
      ti_doc      TYPE TABLE OF type_doc,
      ti_lin      TYPE TABLE OF type_lin,
      ti_lips     TYPE TABLE OF type_lips,
      ti_likp     TYPE TABLE OF type_likp,
      ti_0016     TYPE TABLE OF type_0016,
      ti_kna1     TYPE TABLE OF type_kna1,
      ti_adrc     TYPE TABLE OF type_adrc,
      ti_ttds    TYPE TABLE OF type_ttds,
      ti_ttdst    TYPE TABLE OF type_ttdst,
      ti_vtpa     TYPE TABLE OF type_vtpa,
      ti_vttk     TYPE TABLE OF type_vttk,
      ti_lfa1     TYPE TABLE OF type_lfa1,
      ti_pv       TYPE TABLE OF type_lfa1,
      ti_vfkp     TYPE TABLE OF type_vfkp,
      ti_vfsi     TYPE TABLE OF type_vfsi,
      ti_ekbe     TYPE TABLE OF type_ekbe,
      ti_fieldcat TYPE slis_t_fieldcat_alv,
      ti_sort     TYPE slis_t_sortinfo_alv.

*&---------------------------------------------------------------------*
*&     Work-Áreas
*&---------------------------------------------------------------------*
DATA:  w_saida    TYPE type_saida,
       w_vbfa_va  TYPE type_vbfa,
       w_vttp     TYPE type_vttp,
       w_vbfa_vf  TYPE type_vbfa,
       w_doc      TYPE type_doc,
       w_lin      TYPE type_lin,
       w_lips     TYPE type_lips,
       w_likp     TYPE type_likp,
       w_0016     TYPE type_0016,
       w_kna1     TYPE type_kna1,
       w_adrc     TYPE type_adrc,
       w_ttds     TYPE type_ttds,
       w_ttdst    TYPE type_ttdst,
       w_vtpa     TYPE type_vtpa,
       w_vttk     TYPE type_vttk,
       w_lfa1     TYPE type_lfa1,
       w_pv       TYPE type_lfa1,
       w_vfkp     TYPE type_vfkp,
       w_vfsi     TYPE type_vfsi,
       w_ekbe     TYPE type_ekbe,
       w_fieldcat    TYPE slis_fieldcat_alv,
       w_layout      TYPE slis_layout_alv,
       w_top_header  TYPE slis_t_listheader.

*&---------------------------------------------------------------------*
*&      Constantes
*&---------------------------------------------------------------------*
CONSTANTS: c_x        TYPE c VALUE 'X',
      c_e        TYPE c VALUE 'E',
      c_a        TYPE c VALUE 'A',
      c_h        TYPE c VALUE 'H',
      c_s        TYPE c VALUE 'S',
      c_bar      TYPE c VALUE '/',
      c_1        TYPE c VALUE '1',
      c_2        TYPE c VALUE '2',
      c_3        TYPE c VALUE '3',
      c_4        TYPE c VALUE '4',
      c_8        TYPE c VALUE '8',
      c_c        TYPE c VALUE 'C',
      c_j        TYPE c VALUE 'J',
      c_m        TYPE c VALUE 'M',
      c_i        TYPE c VALUE 'I',
      c_q        TYPE c VALUE 'Q',
      c_eq(2)    TYPE c VALUE 'EQ',
      c_bi(2)    TYPE c VALUE 'BI',
      c_pv(2)    TYPE c VALUE 'PV',
      c_md(2)    TYPE c VALUE 'MD',
      c_z001(4)  TYPE c VALUE 'Z001',
      c_ate(3)   TYPE c VALUE 'até',
      c_ordem     TYPE slis_fieldname VALUE 'ORDEM',
      c_remessa  TYPE slis_fieldname VALUE 'REMESSA',
      c_fat      TYPE slis_fieldname VALUE 'FAT',
      c_nf       TYPE slis_fieldname VALUE 'NF',
      c_serie    TYPE slis_fieldname VALUE 'SERIE',
      c_peso_sa  TYPE slis_fieldname VALUE 'PESO_SA',
      c_peso_che TYPE slis_fieldname VALUE 'PESO_CHE',
      c_peso_dif TYPE slis_fieldname VALUE 'PESO_DIF',
      c_valor_ton TYPE slis_fieldname VALUE 'VALOR_TON',
      c_valor_tot TYPE  slis_fieldname VALUE 'VALOR_TOT',
      c_data_che  TYPE slis_fieldname VALUE 'DATA_CHE',
      c_cid_or    TYPE slis_fieldname VALUE 'CID_OR',
      c_uf_origem TYPE slis_fieldname VALUE 'UF_ORIGEM',
      c_cliente   TYPE slis_fieldname VALUE 'CLIENTE',
      c_cnpj      TYPE slis_fieldname VALUE 'CNPJ',
      c_inscr_est TYPE slis_fieldname VALUE 'INSCR_EST',
      c_cid_dest  TYPE  slis_fieldname VALUE 'CID_DEST',
      c_uf_dest   TYPE slis_fieldname VALUE 'UF_DEST',
      c_bezei     TYPE slis_fieldname VALUE 'BEZEI',
      c_tknum     TYPE slis_fieldname VALUE 'TKNUM',
      c_transp    TYPE slis_fieldname VALUE 'TRANSP',
      c_conhec    TYPE slis_fieldname VALUE 'CONHEC',
      c_carta     TYPE slis_fieldname VALUE 'CARTA',
      c_lote      TYPE slis_fieldname VALUE 'LOTE',
      c_placa     TYPE slis_fieldname VALUE 'PLACA',
      c_motorista  TYPE slis_fieldname VALUE 'MOTORISTA',
      c_prop TYPE slis_fieldname VALUE 'PROP',
      c_itin TYPE slis_fieldname VALUE 'ITIN',
      c_data_trans  TYPE slis_fieldname VALUE 'DATA_TRANS',
      c_doc_frete   TYPE slis_fieldname VALUE 'DOC_FRETE',
      c_vlr_frete   TYPE slis_fieldname VALUE 'VLR_FRETE',
      c_vlr_imp     TYPE slis_fieldname VALUE 'VLR_IMP',
      c_pedido      TYPE slis_fieldname VALUE 'PEDIDO',
      c_folha       TYPE slis_fieldname VALUE 'FOLHA',
      c_saida(8)     TYPE c VALUE 'TI_SAIDA',
      c_traco       TYPE c VALUE '-',
      c_yf_top_of_page    TYPE slis_formname   VALUE 'YF_TOP_OF_PAGE',
      c_yf_user_command   TYPE slis_formname   VALUE 'YF_USER_COMMAND'.

*----------------------------------------------------------------------*
*  RANGES
*----------------------------------------------------------------------*
RANGES: rc_tplst FOR vttk-tplst.

*&---------------------------------------------------------------------*
*&      Tela de seleção
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_bukrs TYPE ttds-tplst OBLIGATORY,
            p_tdlnr TYPE vttk-tdlnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_erdat FOR vttk-erdat OBLIGATORY,
                s_tplst FOR vttk-tplst,
                s_shtyp FOR vttk-shtyp,
                s_tknum FOR vttk-tknum,
                s_route FOR vttk-route,
                s_safra FOR lips-lgort.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.

PARAMETERS: rb_saida   RADIOBUTTON GROUP g2 DEFAULT 'X',
            rb_entra RADIOBUTTON GROUP g2.

SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Start-of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: yf_seleciona_dados,
           yf_trata_dados,
           yf_monta_rel.

*&---------------------------------------------------------------------*
*&      Form  YF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM yf_seleciona_dados .

  IF NOT rb_saida IS INITIAL.
    PERFORM yf_sel_vttk USING c_1 c_3.
  ELSE.
    PERFORM yf_sel_vttk USING c_2 c_4.
  ENDIF.

  PERFORM: yf_sel_ttds,
           yf_sel_vttp,
           yf_sel_vbfa_va,
           yf_sel_vbfa_vf,
           yf_sel_lips.

  IF rb_saida IS INITIAL.
    PERFORM yf_sel_ekbe.
  ENDIF.

  PERFORM: yf_sel_likp,
           yf_sel_j_1bnflin,
           yf_sel_j_1bnfdoc,
           yf_sel_vfkp,
           yf_sel_vfsi,
           yf_sel_kna1,
           yf_sel_lfa1,
           yf_sel_zlest0016,
           yf_sel_adrc,
           yf_sel_vtpa,
           yf_sel_lfa1_pv.

ENDFORM.                    " YF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  YF_TRATA_DADOS
*&---------------------------------------------------------------------*
FORM yf_trata_dados .

  DATA:vl_netwr TYPE vfsi-netwr,
       vl_mwsbp TYPE vfsi-mwsbp,
       vl_tabix TYPE sy-tabix.

  LOOP AT ti_vttk INTO w_vttk.

    CLEAR: vl_netwr, vl_mwsbp.

    CLEAR w_ttds.
    READ TABLE ti_ttds INTO w_ttds WITH KEY tplst = w_vttk-tplst
                                   BINARY SEARCH.

    IF sy-subrc = 0.

      CLEAR w_vttp.
      READ TABLE ti_vttp INTO w_vttp WITH KEY tknum = w_vttk-tknum
                                     BINARY SEARCH.
      IF sy-subrc = 0.

* Filtro Safra
        CLEAR w_lips.
        READ TABLE ti_lips INTO w_lips WITH KEY vbeln = w_vttp-vbeln
                                             BINARY SEARCH.

        IF sy-subrc = 0.

          CLEAR w_adrc.
          READ TABLE ti_adrc INTO w_adrc WITH KEY addrnumber  = w_ttds-adrnr
                                         BINARY SEARCH.

          CLEAR w_vfkp.
          READ TABLE ti_vfkp INTO w_vfkp WITH KEY rebel  = w_vttk-tknum
                                         BINARY SEARCH.
          IF sy-subrc = 0.
            CLEAR w_vfsi.
            READ TABLE ti_vfsi INTO w_vfsi WITH KEY knumv = w_vfkp-knumv
                                   BINARY SEARCH.
            IF sy-subrc = 0.

              vl_tabix = sy-tabix.

              LOOP AT ti_vfsi INTO w_vfsi FROM vl_tabix.

                IF w_vfsi-knumv NE w_vfkp-knumv.
                  EXIT.
                ENDIF.
                vl_netwr = vl_netwr + w_vfsi-netwr.
                vl_mwsbp = vl_mwsbp + w_vfsi-mwsbp.

              ENDLOOP.

            ENDIF.

          ENDIF.

          CLEAR w_0016.
          READ TABLE ti_0016 INTO w_0016 WITH KEY conhecimento = w_vttk-exti1
                                         BINARY SEARCH.

          CLEAR w_vtpa.
          READ TABLE ti_vtpa INTO w_vtpa WITH KEY vbeln   = w_vttk-tknum
                                         BINARY SEARCH.
          IF sy-subrc = 0.
            CLEAR w_pv.
            READ TABLE ti_pv INTO w_pv WITH KEY   lifnr = w_vtpa-lifnr
                                       BINARY SEARCH.
          ENDIF.

          IF not rb_saida is initial.
          CLEAR w_vbfa_va.
          READ TABLE ti_vbfa_va INTO w_vbfa_va WITH KEY vbeln = w_vttp-vbeln
                                               BINARY SEARCH.

          CLEAR w_vbfa_vf.
          READ TABLE ti_vbfa_vf INTO w_vbfa_vf WITH KEY vbelv = w_vttp-vbeln
                                               BINARY SEARCH.

          IF sy-subrc = 0.
            CLEAR w_lin.
            READ TABLE ti_lin INTO w_lin WITH KEY refkey = w_vbfa_vf-refkey
                                                  reftyp = c_bi
                                         BINARY SEARCH.
          ENDIF.

          ENDIF.

          CLEAR w_likp.
          READ TABLE ti_likp INTO w_likp WITH KEY vbeln = w_lips-vbeln
                                         BINARY SEARCH.
          IF sy-subrc = 0.
            IF not  rb_saida is initial.
            CLEAR w_kna1.
            READ TABLE ti_kna1 INTO w_kna1 WITH KEY kunnr = w_likp-kunnr
                                           BINARY SEARCH.
            ELSE.
            CLEAR w_lfa1.
            READ TABLE ti_lfa1 INTO w_lfa1 WITH KEY lifnr = w_likp-lifnr
                                           BINARY SEARCH.
            ENDIf.
          ENDIF.

          IF rb_saida IS INITIAL.

            CLEAR w_ekbe.
            READ TABLE ti_ekbe INTO w_ekbe WITH KEY ebeln = w_lips-vgbel
                                                    ebelp = w_lips-ebelp
                                           BINARY SEARCH.
            IF sy-subrc = 0.
              CLEAR w_lin.
              READ TABLE ti_lin INTO w_lin WITH KEY refkey = w_ekbe-refkey
                                                reftyp = c_md
                                       BINARY SEARCH.
            ENDIF.
          ENDIF.

          IF NOT w_lin IS INITIAL.

            CLEAR w_doc.
            READ TABLE ti_doc INTO w_doc WITH KEY docnum 	= w_lin-docnum
                                         BINARY SEARCH.

          ENDIF.

          IF NOT rb_saida IS INITIAL.
            w_saida-ordem   =  w_vbfa_va-vbeln.
            w_saida-remessa  = w_vttp-vbeln.
            w_saida-fat      = w_vbfa_vf-vbeln.
          ELSE.
            w_saida-ordem   =  w_ekbe-belnr.
            w_saida-remessa  = w_vttp-vbeln.
          ENDIF.

          w_saida-nf       = w_doc-nfenum.
          w_saida-serie    = w_doc-series.

          w_saida-peso_sa   = w_lips-brgew.
          w_saida-peso_che  = w_0016-peso_confirmado.
          w_saida-peso_dif  = w_saida-peso_sa - w_saida-peso_che .
          w_saida-valor_tot = w_0016-vlr_confirmado.

          IF NOT w_saida-peso_che IS INITIAL.
            w_saida-valor_ton = w_saida-valor_tot / w_saida-peso_che.
          ENDIF.
          w_saida-data_che  = w_0016-dta_chegada.

          w_saida-cid_or    = w_adrc-city1.
          w_saida-uf_origem = w_adrc-region.
          IF NOT rb_saida IS INITIAL.
            w_saida-cliente   = w_kna1-name1.
            w_saida-cnpj      = w_kna1-stcd1.
            w_saida-inscr_est = w_kna1-stcd2.
            w_saida-cid_dest  = w_kna1-ort01.
            w_saida-uf_dest   = w_kna1-regio.
          ELSE.
            w_saida-cliente   = w_lfa1-name1.
            w_saida-cnpj      = w_lfa1-stcd1.
            w_saida-inscr_est = w_lfa1-stcd2.
            w_saida-cid_dest  = w_lfa1-ort01.
            w_saida-uf_dest   = w_lfa1-regio.
          ENDIF.

          w_saida-transp    = w_lfa1-name1.
          w_saida-bezei     = w_ttdst-bezei.

          w_saida-tknum      = w_vttk-tknum.
          w_saida-conhec     = w_vttk-exti1.
          w_saida-carta      = w_vttk-exti2.
          w_saida-lote       = w_vttk-signi.
          w_saida-placa      = w_vttk-tndr_trkid.
          w_saida-data_trans = w_vttk-datbg.
          w_saida-itinerario = w_vttk-route.
          w_saida-motorista  = w_vttk-tpbez.

          w_saida-proprietario = w_pv-lifnr.

          w_saida-doc_frete = w_vfkp-fknum.
          w_saida-pedido     = w_vfkp-ebeln.
          w_saida-folha     = w_vfkp-lblni.

          w_saida-vlr_frete = vl_netwr.
          w_saida-vlr_imp   = vl_mwsbp.

          APPEND w_saida TO ti_saida.
          CLEAR w_saida.
        ENDIF. " ti_lips
      ENDIF. " ti_vttp
    ENDIF. " Filtro Empresa -ti_ttds

  ENDLOOP.

ENDFORM.                    " YF_TRATA_DADOS
*&---------------------------------------------------------------------*
*&      Form  YF_MONTA_REL
*&---------------------------------------------------------------------*
FORM yf_monta_rel .

  IF NOT ti_saida[] IS INITIAL.

    SORT ti_saida BY ordem remessa.

    PERFORM: yf_alv_estrutura,
             yf_top_header USING w_top_header,
             yf_alv_fieldcat,
             yf_call_alv.
  ENDIF.

ENDFORM.                    " YF_MONTA_REL
*&---------------------------------------------------------------------*
*&      Form  YF_ALV_ESTRUTURA
*&---------------------------------------------------------------------*
FORM yf_alv_estrutura .

  w_layout-cell_merge        = c_x.
  w_layout-expand_all        = c_x.
  w_layout-zebra             = c_x.
  w_layout-window_titlebar   = text-004. " Consulta Carregamento
  w_layout-detail_titlebar   = text-004.
  w_layout-box_tabname       = c_saida.
  w_layout-colwidth_optimize = c_x.

ENDFORM.                    " YF_ALV_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  YF_TOP_HEADER
*&---------------------------------------------------------------------*
FORM yf_top_header  USING    p_top_header TYPE slis_t_listheader.

  DATA: ls_line     TYPE slis_listheader,  "Títulos no ALV.
        vl_date(10)  TYPE c,
        vl_dateh(10) TYPE c,
        vl_transp TYPE lfa1-name1.

  SELECT SINGLE name1
    FROM  lfa1
    INTO vl_transp
    WHERE lifnr  = p_tdlnr.

  CLEAR ls_line.
  ls_line-typ  = c_h.

* AMAGGI EXPORTAÇÃO E IMPORTAÇÃO LTDA
  ls_line-info = text-005.
  APPEND ls_line TO p_top_header.

  CLEAR ls_line.
  ls_line-typ  = c_s.
* Consulta de Carregamentos
  ls_line-info = text-008.
  APPEND ls_line TO p_top_header.

  READ TABLE s_erdat INDEX 1.
  CONCATENATE s_erdat-low+6(2) c_bar
              s_erdat-low+4(2) c_bar
              s_erdat-low(4)
              INTO vl_date.

  CONCATENATE s_erdat-high+6(2) c_bar
              s_erdat-high+4(2) c_bar
              s_erdat-high(4)
              INTO vl_dateh.

* Transportador
  CLEAR ls_line.
  ls_line-typ  = c_a.
  CONCATENATE  text-006 p_tdlnr c_traco vl_transp INTO ls_line-info SEPARATED BY space .
  APPEND ls_line TO p_top_header.

  CLEAR ls_line.
* Periodo
  IF NOT vl_dateh IS INITIAL.
    ls_line-typ  = c_a.
    CONCATENATE text-007 vl_date c_ate vl_dateh INTO ls_line-info SEPARATED BY space.
    APPEND ls_line TO p_top_header.
  ELSE.
    ls_line-typ  = c_a.
    CONCATENATE text-007 vl_date INTO ls_line-info SEPARATED BY space.
    APPEND ls_line TO p_top_header.

  ENDIF.

ENDFORM.                    " YF_TOP_HEADER
*&---------------------------------------------------------------------*
*&      Form  YF_ALV_FIELDCAT
*&---------------------------------------------------------------------*
FORM yf_alv_fieldcat .

  IF rb_saida = c_x.
** Ordem
    CLEAR w_fieldcat.
    w_fieldcat-fieldname     = c_ordem.
    w_fieldcat-tabname       = c_saida.
    w_fieldcat-seltext_l     = text-t01.
    w_fieldcat-seltext_m     = text-t01.
    w_fieldcat-col_pos       = 1.
    APPEND w_fieldcat TO ti_fieldcat.

*REMESSA
    CLEAR w_fieldcat.
    w_fieldcat-fieldname     = c_remessa.
    w_fieldcat-tabname       = c_saida.
    w_fieldcat-seltext_l     = text-t02.
    w_fieldcat-seltext_m     = text-t02.
    w_fieldcat-col_pos       = 2.
    APPEND w_fieldcat TO ti_fieldcat.

* Faturamento
    CLEAR w_fieldcat.
    w_fieldcat-fieldname     = c_remessa.
    w_fieldcat-tabname       = c_saida.
    w_fieldcat-seltext_l     = text-t03.
    w_fieldcat-seltext_m     = text-t03.
    w_fieldcat-col_pos       = 3.
    APPEND w_fieldcat TO ti_fieldcat.
  ELSE.
* Pedido de Compra
    CLEAR w_fieldcat.
    w_fieldcat-fieldname     = c_ordem.
    w_fieldcat-tabname       = c_saida.
    w_fieldcat-seltext_l     = text-t35.
    w_fieldcat-seltext_m     = text-t35.
    w_fieldcat-col_pos       = 1.
    APPEND w_fieldcat TO ti_fieldcat.

*Aviso de recebimento
    CLEAR w_fieldcat.
    w_fieldcat-fieldname     = c_remessa.
    w_fieldcat-tabname       = c_saida.
    w_fieldcat-seltext_l     = text-t36.
    w_fieldcat-seltext_m     = text-t36.
    w_fieldcat-col_pos       = 2.
    APPEND w_fieldcat TO ti_fieldcat.

  ENDIF.

* NF
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_nf.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t04.
  w_fieldcat-seltext_m     = text-t04.
  w_fieldcat-col_pos       = 4.
  APPEND w_fieldcat TO ti_fieldcat.

* Série
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_serie.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t05.
  w_fieldcat-seltext_m     = text-t05.
  w_fieldcat-col_pos       = 5.
  APPEND w_fieldcat TO ti_fieldcat.

*Peso Saída
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_peso_sa.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t06.
  w_fieldcat-seltext_m     = text-t06.
  w_fieldcat-col_pos       = 6.
  APPEND w_fieldcat TO ti_fieldcat.

*Peso Chegada
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_peso_che.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t07.
  w_fieldcat-seltext_m     = text-t07.
  w_fieldcat-col_pos       = 7.
  APPEND w_fieldcat TO ti_fieldcat.

*Diferença
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_peso_dif.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t08.
  w_fieldcat-seltext_m     = text-t08.
  w_fieldcat-col_pos       = 8.
  APPEND w_fieldcat TO ti_fieldcat.

*Valor Ton
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_valor_ton.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t09.
  w_fieldcat-seltext_m     = text-t09.
  w_fieldcat-col_pos       = 9.
  APPEND w_fieldcat TO ti_fieldcat.

*Valor Total
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_valor_tot.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t10.
  w_fieldcat-seltext_m     = text-t10.
  w_fieldcat-col_pos       = 10.
  APPEND w_fieldcat TO ti_fieldcat.

*Data chegada
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_data_che.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t11.
  w_fieldcat-seltext_m     = text-t11.
  w_fieldcat-col_pos       = 11.
  APPEND w_fieldcat TO ti_fieldcat.

  IF rb_saida = c_x.
*Cliente
    CLEAR w_fieldcat.
    w_fieldcat-fieldname     = c_cliente.
    w_fieldcat-tabname       = c_saida.
    w_fieldcat-seltext_l     = text-t12.
    w_fieldcat-seltext_m     = text-t12.
    w_fieldcat-col_pos       = 12.
    APPEND w_fieldcat TO ti_fieldcat.
  ELSE.
*Fornecedor
    CLEAR w_fieldcat.
    w_fieldcat-fieldname     = c_cliente.
    w_fieldcat-tabname       = c_saida.
    w_fieldcat-seltext_l     = text-t37.
    w_fieldcat-seltext_m     = text-t37.
    w_fieldcat-col_pos       = 12.
    APPEND w_fieldcat TO ti_fieldcat.

  ENDIF.

*CNPJ
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_cnpj.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t15.
  w_fieldcat-seltext_m     = text-t15.
  w_fieldcat-col_pos       = 13.
  APPEND w_fieldcat TO ti_fieldcat.

*Inscrição Estadual
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_inscr_est.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t16.
  w_fieldcat-seltext_m     = text-t16.
  w_fieldcat-col_pos       = 14.
  APPEND w_fieldcat TO ti_fieldcat.

*Cidade Destino
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_cid_dest.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t17.
  w_fieldcat-seltext_m     = text-t17.
  w_fieldcat-col_pos       = 15.
  APPEND w_fieldcat TO ti_fieldcat.

*UF Destino
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_uf_dest.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t18.
  w_fieldcat-seltext_m     = text-t18.
  w_fieldcat-col_pos       = 16.
  APPEND w_fieldcat TO ti_fieldcat.

*Cidade origem
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_cid_or.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t12.
  w_fieldcat-seltext_m     = text-t12.
  w_fieldcat-col_pos       = 17.
  APPEND w_fieldcat TO ti_fieldcat.

*UF origem
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_uf_origem.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t13.
  w_fieldcat-seltext_m     = text-t13.
  w_fieldcat-col_pos       = 18.
  APPEND w_fieldcat TO ti_fieldcat.

*LocOrgTrp.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_bezei.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t19.
  w_fieldcat-seltext_m     = text-t19.
  w_fieldcat-col_pos       = 19.
  APPEND w_fieldcat TO ti_fieldcat.

*Doc.Transporte
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_tknum.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t20.
  w_fieldcat-seltext_m     = text-t20.
  w_fieldcat-col_pos       = 20.
  APPEND w_fieldcat TO ti_fieldcat.

*Transportador (TDLNR)
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_transp.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t21.
  w_fieldcat-seltext_m     = text-t21.
  w_fieldcat-col_pos       = 21.
  APPEND w_fieldcat TO ti_fieldcat.

*Conhecimento
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_conhec.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t22.
  w_fieldcat-seltext_m     = text-t22.
  w_fieldcat-col_pos       = 22.
  APPEND w_fieldcat TO ti_fieldcat.

*Carta Frete
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_carta.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t23.
  w_fieldcat-seltext_m     = text-t23.
  w_fieldcat-col_pos       = 23.
  APPEND w_fieldcat TO ti_fieldcat.

*Lote
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_lote.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t24.
  w_fieldcat-seltext_m     = text-t24.
  w_fieldcat-col_pos       = 24.
  APPEND w_fieldcat TO ti_fieldcat.

*Placa Veículo
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_placa.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t25.
  w_fieldcat-seltext_m     = text-t25.
  w_fieldcat-col_pos       = 25.
  APPEND w_fieldcat TO ti_fieldcat.

*Motorista
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_motorista.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t26.
  w_fieldcat-seltext_m     = text-t26.
  w_fieldcat-col_pos       = 26.
  APPEND w_fieldcat TO ti_fieldcat.

*Proprietario
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_prop.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t27.
  w_fieldcat-seltext_m     = text-t27.
  w_fieldcat-col_pos       = 27.
  APPEND w_fieldcat TO ti_fieldcat.

*Itinerario
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_itin.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t28.
  w_fieldcat-seltext_m     = text-t28.
  w_fieldcat-col_pos       = 28.
  APPEND w_fieldcat TO ti_fieldcat.

*Data transporte
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_data_trans.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t29.
  w_fieldcat-seltext_m     = text-t29.
  w_fieldcat-col_pos       = 29.
  APPEND w_fieldcat TO ti_fieldcat.

*Doc.Custo Frete
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_doc_frete.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t30.
  w_fieldcat-seltext_m     = text-t30.
  w_fieldcat-col_pos       = 30.
  APPEND w_fieldcat TO ti_fieldcat.

*Vlr. Total do frete
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_vlr_frete.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t31.
  w_fieldcat-seltext_m     = text-t31.
  w_fieldcat-col_pos       = 31.
  APPEND w_fieldcat TO ti_fieldcat.

*Vlr. Total impostos
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_vlr_frete.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t32.
  w_fieldcat-seltext_m     = text-t32.
  w_fieldcat-col_pos       = 32.
  APPEND w_fieldcat TO ti_fieldcat.

*Pedido Serviço
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_pedido.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t33.
  w_fieldcat-seltext_m     = text-t33.
  w_fieldcat-col_pos       = 33.
  APPEND w_fieldcat TO ti_fieldcat.

*Folha Serviço
  CLEAR w_fieldcat.
  w_fieldcat-fieldname     = c_folha.
  w_fieldcat-tabname       = c_saida.
  w_fieldcat-seltext_l     = text-t34.
  w_fieldcat-seltext_m     = text-t34.
  w_fieldcat-col_pos       = 34.
  APPEND w_fieldcat TO ti_fieldcat.

ENDFORM.                    " YF_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  YF_CALL_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM yf_call_alv .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_top_of_page  = c_yf_top_of_page
      i_callback_user_command = c_yf_user_command
      is_layout               = w_layout
      it_fieldcat             = ti_fieldcat
      i_default               = c_x
      i_save                  = c_a
    TABLES
      t_outtab                = ti_saida
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " YF_CALL_ALV
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_VTTK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM yf_sel_vttk  USING    p_abfer1
                           p_abfer2.

  SELECT  a~tknum a~shtyp a~tplst a~erdat a~abfer
          a~route a~signi a~exti1 a~exti2 a~tpbez
          a~datbg a~tdlnr a~tndr_trkid
    INTO TABLE ti_vttk
    FROM vttk AS a
    INNER JOIN tvtk AS b ON a~shtyp  = b~shtyp
    WHERE  a~tknum IN  s_tknum AND
           a~shtyp IN  s_shtyp AND
           a~tplst IN  s_tplst AND
           a~erdat IN  s_erdat AND
           a~abfer IN  (p_abfer1, p_abfer2) AND
           a~route IN  s_route AND
           a~tdlnr EQ  p_tdlnr.

  IF sy-subrc NE 0.
*Não há registros a serem processados
    MESSAGE text-m01 TYPE c_s DISPLAY LIKE c_e.
    REJECT.
  ELSE.
    SORT ti_vttk BY tknum.
  ENDIF.

ENDFORM.                    " YF_SEL_VTTK
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_VTTP
*&---------------------------------------------------------------------*
FORM yf_sel_vttp .

  IF NOT ti_vttk IS INITIAL.

    SELECT tknum  vbeln
    FROM vttp
    INTO TABLE ti_vttp
    FOR ALL ENTRIES IN ti_vttk
    WHERE    tknum = ti_vttk-tknum.

    IF sy-subrc = 0.
      SORT ti_vttp BY tknum vbeln.
    ENDIF.

  ENDIF.

ENDFORM.                    " YF_SEL_VTTP

*&---------------------------------------------------------------------*
*&      Form  YF_SEL_VBFA_VA
*&---------------------------------------------------------------------*
FORM yf_sel_vbfa_va .

  DATA: tl_vttp TYPE TABLE OF type_vttp.

  IF NOT ti_vttp IS INITIAL.
    tl_vttp[] = ti_vttp[].
    SORT tl_vttp BY vbeln.
    DELETE ADJACENT DUPLICATES FROM tl_vttp COMPARING vbeln.

    SELECT vbelv posnv vbeln posnn vbtyp_n  vbtyp_v
    FROM vbfa
    INTO TABLE ti_vbfa_va
    FOR ALL ENTRIES IN tl_vttp
    WHERE vbeln = tl_vttp-vbeln AND
          vbtyp_v = c_c      AND
          vbtyp_n = c_j.

    IF sy-subrc = 0.
      SORT ti_vbfa_va BY vbeln.
    ENDIF.

  ENDIF.

ENDFORM.                    " YF_SEL_VBFA_VA
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_VBFA_VF
*&---------------------------------------------------------------------*
FORM yf_sel_vbfa_vf .

  DATA: tl_vttp TYPE TABLE OF type_vttp.

  IF NOT ti_vttp IS INITIAL.
    tl_vttp[] = ti_vttp[].
    SORT tl_vttp BY vbeln.
    DELETE ADJACENT DUPLICATES FROM tl_vttp COMPARING vbeln.

    SELECT vbelv posnv vbeln posnn vbtyp_n  vbtyp_v
    FROM vbfa
    INTO TABLE ti_vbfa_vf
    FOR ALL ENTRIES IN tl_vttp
    WHERE vbelv = tl_vttp-vbeln AND
          vbtyp_v = c_j      AND
          vbtyp_n = c_m.

    IF sy-subrc = 0.
      SORT ti_vbfa_vf BY vbelv.
    ENDIF.

  ENDIF.

ENDFORM.                    " YF_SEL_VBFA_VF
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_LIPS
*&---------------------------------------------------------------------*
FORM yf_sel_lips .
  DATA: tl_vttp TYPE TABLE OF type_vttp.

  IF NOT ti_vttp IS INITIAL.
    tl_vttp[] = ti_vttp[].
    SORT tl_vttp BY vbeln.
    DELETE ADJACENT DUPLICATES FROM tl_vttp COMPARING vbeln.

    SELECT vbeln posnr lgort brgew vgbel vgpos
    FROM lips
    INTO TABLE ti_lips
    FOR ALL ENTRIES IN tl_vttp
    WHERE	vbeln = tl_vttp-vbeln AND
          lgort IN s_safra.

    IF sy-subrc = 0.
      SORT ti_lips BY vbeln.
    ENDIF.

  ENDIF.

ENDFORM.                    " YF_SEL_LIPS
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_EKBE
*&---------------------------------------------------------------------*
FORM yf_sel_ekbe .

  DATA: tl_lips TYPE TABLE OF type_lips.

  IF NOT ti_lips[] IS INITIAL.

    LOOP AT ti_lips INTO w_lips.
      w_lips-ebelp = w_lips-vgpos.
      MODIFY ti_lips FROM w_lips INDEX sy-tabix.
    ENDLOOP.

    tl_lips[] = ti_lips[].
    SORT tl_lips BY vgbel vgpos.
    DELETE ADJACENT DUPLICATES FROM tl_lips COMPARING vgbel vgpos.

    SELECT ebeln ebelp gjahr belnr bewtp
    FROM ekbe
    INTO TABLE ti_ekbe
    FOR ALL ENTRIES IN tl_lips
    WHERE  ebeln = tl_lips-vgbel AND
           ebelp = tl_lips-ebelp AND
           bewtp = c_q.

    IF sy-subrc = 0.
      SORT ti_ekbe BY ebeln ebelp.
    ENDIF.
  ENDIF.

ENDFORM.                    " YF_SEL_EKBE
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_LIKP
*&---------------------------------------------------------------------*
FORM yf_sel_likp .

  DATA: tl_lips TYPE TABLE OF type_lips.

  IF NOT ti_lips IS INITIAL.
    tl_lips[] = ti_lips[].
    SORT tl_lips BY vbeln.
    DELETE ADJACENT DUPLICATES FROM tl_lips COMPARING vbeln.

    SELECT vbeln kunnr lifnr
    FROM likp
    INTO TABLE ti_likp
    FOR ALL ENTRIES IN tl_lips
    WHERE vbeln = tl_lips-vbeln.

    IF sy-subrc = 0.
      SORT ti_likp BY kunnr.
    ENDIF.

  ENDIF.
ENDFORM.                    " YF_SEL_LIKP
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_J_1BNFLIN
*&---------------------------------------------------------------------*
FORM yf_sel_j_1bnflin .

  DATA: tl_vbfa TYPE TABLE OF type_vbfa,
        tl_ekbe TYPE TABLE OF type_ekbe.

  IF NOT rb_saida IS INITIAL.

    LOOP AT ti_vbfa_vf INTO w_vbfa_vf.
      w_vbfa_vf-refkey = w_vbfa_vf-vbeln.
      MODIFY ti_vbfa_vf FROM w_vbfa_vf INDEX sy-tabix.
    ENDLOOP.

    IF NOT ti_vbfa_vf IS INITIAL.
      tl_vbfa[] = ti_vbfa_vf[].
      SORT tl_vbfa BY refkey.
      DELETE ADJACENT DUPLICATES FROM tl_vbfa COMPARING refkey.

      SELECT docnum itmnum  refkey reftyp
      FROM j_1bnflin
      INTO TABLE ti_lin
      FOR ALL ENTRIES IN tl_vbfa
      WHERE refkey = tl_vbfa-refkey AND
            reftyp = c_bi.

    ENDIF.

  ELSE.

    LOOP AT ti_ekbe INTO w_ekbe.
      CONCATENATE w_ekbe-belnr w_ekbe-gjahr INTO w_ekbe-refkey.
      MODIFY ti_ekbe FROM w_ekbe INDEX sy-tabix.
    ENDLOOP.

    tl_ekbe[] = ti_ekbe.
    SORT tl_ekbe BY refkey.
    DELETE ADJACENT DUPLICATES FROM tl_ekbe COMPARING refkey.

    SELECT docnum itmnum  refkey reftyp
    FROM j_1bnflin
    INTO TABLE ti_lin
    FOR ALL ENTRIES IN tl_ekbe
    WHERE refkey = tl_ekbe-refkey AND
          reftyp = c_md.

  ENDIF.

  IF sy-subrc = 0.
    SORT ti_lin BY refkey reftyp.
  ENDIF.

ENDFORM.                    " YF_SEL_J_1BNFLIN
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_J_1BNFDOC
*&---------------------------------------------------------------------*
FORM yf_sel_j_1bnfdoc .

  DATA: tl_lin TYPE TABLE OF type_lin.

  IF NOT ti_lin IS INITIAL.
    tl_lin[] = ti_lin[].
    SORT tl_lin BY docnum.
    DELETE ADJACENT DUPLICATES FROM tl_lin COMPARING docnum.

    SELECT docnum  series  nfenum
    FROM j_1bnfdoc
    INTO TABLE ti_doc
    FOR ALL ENTRIES IN tl_lin
    WHERE docnum = tl_lin-docnum.

    IF sy-subrc = 0.
      SORT ti_doc BY docnum.
    ENDIF.

  ENDIF.

ENDFORM.                    " YF_SEL_J_1BNFDOC
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_VFKP
*&---------------------------------------------------------------------*
FORM yf_sel_vfkp .

  IF NOT ti_vttk IS INITIAL.

    SELECT fknum fkpos fkpty knumv  ebeln
           lblni rebel  repos
    FROM vfkp
    INTO TABLE ti_vfkp
    FOR ALL ENTRIES IN ti_vttk
    WHERE  rebel  = ti_vttk-tknum AND
           refty  = c_8       AND
           fkpty  = c_z001.

    IF sy-subrc = 0.
      SORT ti_vfkp BY rebel.
    ENDIF.

  ENDIF.

ENDFORM.                    " YF_SEL_VFKP
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_VFSI
*&---------------------------------------------------------------------*
FORM yf_sel_vfsi .

  DATA: tl_vfkp TYPE TABLE OF type_vfkp.

  IF NOT ti_vfkp IS INITIAL.
    tl_vfkp[] = ti_vfkp[].
    SORT tl_vfkp BY knumv.
    DELETE ADJACENT DUPLICATES FROM tl_vfkp COMPARING knumv.

    SELECT knumv kposn vbeln posnr
           netwr mwsbp kzwi1 kzwi2
     FROM vfsi
     INTO TABLE ti_vfsi
     FOR ALL ENTRIES IN tl_vfkp
     WHERE knumv = tl_vfkp-knumv.

    IF sy-subrc = 0.
      SORT ti_vfsi BY knumv.
    ENDIF.

  ENDIF.

ENDFORM.                    " YF_SEL_VFSI
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_KNA1
*&---------------------------------------------------------------------*
FORM yf_sel_kna1 .

  DATA: tl_likp TYPE TABLE OF type_likp.

  IF NOT ti_likp IS INITIAL.
    tl_likp[] = ti_likp[].
    SORT tl_likp BY kunnr.
    DELETE ADJACENT DUPLICATES FROM tl_likp COMPARING kunnr.

    SELECT kunnr name1 ort01 regio stcd1 stcd2
    FROM kna1
    INTO TABLE ti_kna1
    FOR ALL ENTRIES IN tl_likp
    WHERE kunnr = tl_likp-kunnr.

    IF sy-subrc = 0.
      SORT ti_kna1 BY kunnr.
    ENDIF.

  ENDIF.

ENDFORM.                    " YF_SEL_KNA1
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_LFA1
*&---------------------------------------------------------------------*
FORM yf_sel_lfa1 .

  DATA: tl_likp TYPE TABLE OF type_likp.

  IF NOT ti_likp IS INITIAL.

    tl_likp[] = ti_likp[].
    SORT ti_likp BY lifnr.
    DELETE ADJACENT DUPLICATES FROM tl_likp COMPARING lifnr.

    SELECT lifnr name1 ort01 regio stcd1 stcd2
    FROM lfa1
    INTO TABLE ti_lfa1
    FOR ALL ENTRIES IN tl_likp
    WHERE lifnr = tl_likp-lifnr.

    IF sy-subrc = 0.
      SORT ti_lfa1 BY lifnr.
    ENDIF.

  ENDIF.

ENDFORM.                    " YF_SEL_LFA1
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_ZLEST0016
*&---------------------------------------------------------------------*
FORM yf_sel_zlest0016 .

  DATA: tl_vttk TYPE TABLE OF type_vttk.

  IF NOT ti_vttk IS INITIAL.

    tl_vttk[] = ti_vttk[].
    SORT tl_vttk BY exti1.
    DELETE ADJACENT DUPLICATES FROM tl_vttk COMPARING exti1.

    SELECT conhecimento peso_confirmado vlr_confirmado dta_chegada
    FROM zlest0016
    INTO TABLE ti_0016
    FOR ALL ENTRIES IN tl_vttk
    WHERE conhecimento = tl_vttk-exti1.

    IF sy-subrc = 0.
      SORT ti_0016 BY conhecimento.
    ENDIF.
  ENDIF.

ENDFORM.                    " YF_SEL_ZLEST0016
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_TTDS
*&---------------------------------------------------------------------*
FORM yf_sel_ttds .

  DATA: tl_vttk TYPE TABLE OF type_vttk.

  IF NOT ti_vttk IS INITIAL.

    tl_vttk[] = ti_vttk[].
    SORT tl_vttk BY tplst.
    DELETE ADJACENT DUPLICATES FROM tl_vttk COMPARING tplst.

    SELECT tplst adrnr bukrs
    FROM ttds
    INTO TABLE ti_ttds
    FOR ALL ENTRIES IN tl_vttk
    WHERE tplst = tl_vttk-tplst AND
          bukrs EQ p_bukrs.

    IF sy-subrc = 0.
      SORT ti_ttds BY tplst.
    ENDIF.

* Filtra VTTK
    IF NOT ti_ttds IS INITIAL.

      rc_tplst-sign = c_i.
      rc_tplst-option = c_eq.

      LOOP AT ti_ttds INTO w_ttds.
        rc_tplst-low = w_ttds-tplst.
        APPEND  rc_tplst.
      ENDLOOP.

      DELETE ti_vttk WHERE tplst NOT IN rc_tplst.
    ENDIF.
  ENDIF.

ENDFORM.                    " YF_SEL_TTDS
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_ADRC
*&---------------------------------------------------------------------*
FORM yf_sel_adrc .

  DATA: tl_ttds TYPE TABLE OF type_ttds.

  IF NOT ti_ttds IS INITIAL.

    tl_ttds[] =  ti_ttds[].
    SORT tl_ttds BY adrnr.
    DELETE ADJACENT DUPLICATES FROM tl_ttds COMPARING adrnr.

    SELECT addrnumber city1 region
    FROM adrc
    INTO TABLE ti_adrc
    FOR ALL ENTRIES IN tl_ttds
    WHERE  addrnumber  = tl_ttds-adrnr.

    IF sy-subrc = 0.
      SORT ti_adrc BY addrnumber.
    ENDIF.

  ENDIF.

ENDFORM.                    " YF_SEL_ADRC
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_VTPA
*&---------------------------------------------------------------------*
FORM yf_sel_vtpa .

  IF NOT ti_vttk IS INITIAL.

    SELECT vbeln posnr parvw lifnr
    FROM vtpa
    INTO TABLE ti_vtpa
    FOR ALL ENTRIES IN ti_vttk
    WHERE vbeln = ti_vttk-tknum AND
          parvw  =   c_pv.

    IF sy-subrc = 0.
      SORT ti_vtpa BY vbeln.
    ENDIF.
  ENDIF.

ENDFORM.                    " YF_SEL_VTPA
*&---------------------------------------------------------------------*
*&      Form  YF_SEL_LFA1_PV
*&---------------------------------------------------------------------*
FORM yf_sel_lfa1_pv .

  DATA: tl_vtpa TYPE TABLE OF type_vtpa.

  IF NOT  ti_vtpa IS INITIAL.

    tl_vtpa[] = ti_vtpa[].
    SORT tl_vtpa BY lifnr.
    DELETE ADJACENT DUPLICATES FROM tl_vtpa COMPARING lifnr.

    SELECT lifnr name1 ort01 regio stcd1 stcd2
    FROM lfa1
    INTO TABLE ti_pv
    FOR ALL ENTRIES IN tl_vtpa
    WHERE lifnr = tl_vtpa-lifnr.

    IF sy-subrc = 0.
      SORT ti_pv BY lifnr.
    ENDIF.
  ENDIF.

ENDFORM.                    " YF_SEL_LFA1_PV
*&---------------------------------------------------------------------*
*&      Form  YF_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM yf_top_of_page .                                       "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = w_top_header.

ENDFORM.                    " YF_TOP_OF_PAGE
