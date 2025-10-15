*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZLESR0007                                               *
* Descrição  : Consulta de Carregamentos                               *
* Módulo     : LES                               Transação: ZLES0033   *
*                                                                      *
*----------------------------------------------------------------------*

REPORT zlesr0007 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TABLES: vttk,
        lips,
        mara,
        vtts,
        t001.

TYPE-POOLS icon.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_alv,
         name1_kn        TYPE kna1-name1,
         stcd1_kn        TYPE char18,
         stcd3_kn        TYPE kna1-stcd3,
         city1_ad        TYPE adrc-city1,
         region_ad       TYPE adrc-region,
         ort01_kn        TYPE kna1-ort01,
         regio_kn        TYPE kna1-regio,
         name1_lf        TYPE lfa1-name1,
         exti1           TYPE vttk-exti1,
         exti2           TYPE vttk-exti2,
         text1           TYPE vttk-text1,
         name1_mt        TYPE lfa1-name1,
         stcd_mt         TYPE lfa1-stcd1,
         stcd_pv         TYPE lfa1-stcd1,
         name1_pv        TYPE lfa1-name1,
         route           TYPE tvro-route,
         vbeln_va        TYPE vbfa-vbeln, "ordem de venda
         vbeln           TYPE vttp-vbeln,
         vbeln_vf        TYPE vbfa-vbeln,
         nfenum          TYPE j_1bnfdoc-nfenum,
         series          TYPE j_1bnfdoc-series,
         brgew           TYPE lips-brgew,
         peso_confirmado TYPE zlest0016-peso_confirmado,
         diferenca       TYPE zlest0016-peso_confirmado,
         dta_chegada     TYPE zlest0016-dta_chegada,
         bezei           TYPE ttdst-bezei,
         tknum           TYPE vttk-tknum,
         signi           TYPE vttk-signi,
         datbg           TYPE vttk-datbg,
         identi          TYPE vttk-tndr_trkid,
         fknum           TYPE vfkp-fknum,
         netwr           TYPE vfsi-netwr,
         mwsbp           TYPE vfsi-mwsbp,
         ebeln           TYPE vfkp-ebeln,
         ebeln1          TYPE vfkp-ebeln,
         lblni           TYPE vfkp-lblni,
         kbetr           TYPE konv-kbetr,
         matnr           TYPE mara-matnr,
         maktx           TYPE makt-maktx,
         mblnr           TYPE mkpf-mblnr,
         name1_lr        TYPE kna1-name1,
         ukurs           TYPE tcurr-ukurs,
         netwr1          TYPE vfsi-netwr,
         zseg            TYPE konv-kwert,
         ziof            TYPE konv-kwert,
         zped            TYPE konv-kwert,
         shtyp           TYPE vttk-shtyp,
         kbetr1          TYPE konv-kbetr,
         werks           TYPE t001w-name1,
         frete_ton       TYPE konv-kwert,
         cd_carreg       TYPE lfa1-lifnr,
         desc_carreg     TYPE lfa1-name1,
         cd_terminal     TYPE lfa1-lifnr,
         desc_terminal   TYPE lfa1-name1,
         cid_terminal    TYPE lfa1-ort01,
         end_terminal    TYPE lfa1-stras,
         cid_entrega     TYPE lfa1-ort01,
         reg_entrega     TYPE lfa1-regio,
         charg           TYPE lips-charg,
         vlr_nf          TYPE j_1bnfdoc-nftot,
         uaten           TYPE vttk-uaten,
         werks_aux       TYPE zsdt_depara_depo-werks,
         kvgr3           TYPE vbak-kvgr3,
         zvct            TYPE konv-kwert,
         instrucao       TYPE zsded030,
         kbetr_per       TYPE p LENGTH 16 DECIMALS 5,
         data_in_porto   TYPE zsdt0045-data_in_porto,
         data_porto      TYPE zsdt0045-data_porto,
         xstatus(20)     TYPE c,
         observacao(20)  TYPE c,
         tp_transp       TYPE char8,
         equip_tac       TYPE char6,
         data_tp         TYPE sy-datum,
         hora_tp         TYPE sy-uzeit,
         base_tipbank    TYPE char6,
         grupo           TYPE zlest0002-grupo,
         frota           TYPE zlest0002-frota,
         nm_bloco        TYPE zsdt0001fd-nm_bloco,
         qt_fardos       TYPE zsdt0001fd-qt_fardos,
       END   OF type_alv,

       BEGIN OF ty_vbak,
         vbeln TYPE vbak-vbeln,
         kvgr3 TYPE vbak-kvgr3,
       END OF ty_vbak,

       BEGIN OF ty_zlest0035,
         docnum     TYPE zlest0035-docnum,
         qtd_cheg   TYPE zlest0035-qtd_cheg,
         dtachegada TYPE zlest0035-dtachegada,
       END OF ty_zlest0035,

       BEGIN OF ty_zlest0039,
         vbeln       TYPE zlest0039-vbeln,
         datachegada TYPE zlest0039-datachegada,
         pesochegada TYPE zlest0039-pesochegada,
         datatransb  TYPE zlest0039-datatransb,
         pesotransb  TYPE zlest0039-pesotransb,
         pontotransb TYPE zlest0039-pontotransb,
       END OF ty_zlest0039,

       BEGIN OF ty_filtro,
         tknum TYPE vttk-tknum,
       END OF ty_filtro.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_vttk              TYPE TABLE OF vttk,
      t_vbak              TYPE TABLE OF ty_vbak,
      t_vttp              TYPE TABLE OF vttp,
      t_vtts              TYPE TABLE OF vtts,
      t_vtts_line4        TYPE TABLE OF vtts,
      t_vtts_line3        TYPE TABLE OF vtts,
      t_vfkp              TYPE TABLE OF vfkp,
      t_tcurr             TYPE TABLE OF tcurr,
      t_ekbe              TYPE TABLE OF ekbe,
      t_vbfa_va           TYPE TABLE OF vbfa,
      t_vbfa_vf           TYPE TABLE OF vbfa,
      t_vbfa_qt           TYPE TABLE OF vbfa,
      t_vbrk              TYPE TABLE OF vbrk,
      t_likp              TYPE TABLE OF likp,
      t_lips              TYPE TABLE OF lips WITH HEADER LINE,
      t_t001w             TYPE TABLE OF t001w,
      t_makt              TYPE TABLE OF makt,
      t_lin               TYPE TABLE OF j_1bnflin,
      t_doc               TYPE TABLE OF j_1bnfdoc,
      t_kna1              TYPE TABLE OF kna1,
      t_kna2              TYPE TABLE OF kna1,
      t_t0016             TYPE TABLE OF zlest0016,
      t_ttds              TYPE TABLE OF ttds,
      t_ttdst             TYPE TABLE OF ttdst,
      t_adrc              TYPE TABLE OF adrc,
      t_vtpa              TYPE TABLE OF vtpa,
      t_vtpa_p            TYPE TABLE OF vtpa,
      t_vfsi              TYPE TABLE OF vfsi,
      t_lfa1              TYPE TABLE OF lfa1,
      t_lfa1_2            TYPE TABLE OF lfa1,
      t_lfa1_3            TYPE TABLE OF lfa1,
      ti_mkpf             TYPE TABLE OF mkpf,
      t_alv               TYPE TABLE OF type_alv,
      ti_tvst             TYPE TABLE OF tvst,
      ti_tvkn             TYPE TABLE OF tvkn,
      ti_lfa1             TYPE TABLE OF lfa1,
      t_fcat              TYPE TABLE OF lvc_s_fcat,
      t_konv              TYPE TABLE OF konv,
      t_tvro              TYPE TABLE OF tvro,
      t_vbpa              TYPE TABLE OF vbpa,
      t_filtro            TYPE TABLE OF ty_filtro,
      t_zlest0039         TYPE TABLE OF ty_zlest0039,
      t_zlest0035         TYPE TABLE OF ty_zlest0035,
      t_zlest0173         TYPE TABLE OF zlest0173,
      t_zsdt0001ovro      TYPE TABLE OF zsdt0001ovro,
      t_zlest0078         TYPE TABLE OF zlest0078,
      t_zsdt0053          TYPE TABLE OF zsdt0053 WITH HEADER LINE,
      t_zsdt0066          TYPE TABLE OF zsdt0066 WITH HEADER LINE,
      it_zmmt_ee_zgr_docs TYPE TABLE OF zmmt_ee_zgr_docs WITH HEADER LINE.

DATA: sl_vttk          TYPE vttk,
      sl_vbak          TYPE ty_vbak,
      sl_vbfa_va       TYPE vbfa,
      sl_vbfa_vf       TYPE vbfa,
      sl_vbfa_qt       TYPE vbfa,
      sl_vttp          TYPE vttp,
      sl_doc           TYPE j_1bnfdoc,
      sl_lin           TYPE j_1bnflin,
      sl_lips          TYPE lips,
      sl_t001w         TYPE t001w,
      sl_makt          TYPE makt,
      sl_likp          TYPE likp,
      sl_t0016         TYPE zlest0016,
      sl_adrc          TYPE adrc,
      sl_kna1          TYPE kna1,
      sl_ttdst         TYPE ttdst,
      sl_tcurr         TYPE tcurr,
      sl_lfa1          TYPE lfa1,
      sl_lfa1_3        TYPE lfa1,
      sl_tvro          TYPE tvro,
      sl_vfkp          TYPE vfkp,
      sl_vfsi          TYPE vfsi,
      sl_ekbe          TYPE ekbe,
      sl_ttds          TYPE ttds,
      sl_tvst          TYPE tvst,
      sl_vtpa          TYPE vtpa,
      sl_mkpf          TYPE mkpf,
      sl_vtpa_mt       TYPE vtpa,
      sl_vtts          TYPE vtts,
      sl_alv           TYPE type_alv,
      sl_lfa1_mt       TYPE lfa1,
      sl_konv          TYPE konv,
      sl_vbpa          TYPE vbpa,
      vl_dias          TYPE i,
      wa_j_cabe        TYPE j_1bindoc,
      vl_refkey        TYPE j_1bnflin-refkey,
      p_gdatu          TYPE tcurr-gdatu,
      w_alv            TYPE type_alv,
      tabix            TYPE sy-tabix,
      w_vttk           TYPE vttk,
      w_ttds           TYPE ttds,
      w_zlest0039      TYPE ty_zlest0039,
      w_zlest0035      TYPE ty_zlest0035,
      sl_data(10)      TYPE c,
      v_data_in_porto  TYPE zsdt0045-data_in_porto,
      v_data_porto     TYPE zsdt0045-data_porto,
      v_observacao     TYPE zsdt0045-observacao.

*** csb/ini 10092020
DATA: wl_name   TYPE thead-tdname,
      wl_id     TYPE thead-tdid,
      tg_texto  TYPE STANDARD TABLE OF tline WITH HEADER LINE,
      wg_texto  LIKE LINE OF tg_texto,
      wl_header TYPE thead.
*      TG_TEXTO  TYPE TABLE OF TLINE WITH HEADER LINE,
*      WG_TEXTO  TYPE TLINE,

DATA: tl_texto TYPE catsxt_longtext_itab,
      wl_texto TYPE LINE OF catsxt_longtext_itab,
      v_cont   TYPE i.

*** csb/fim 10092020
*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: s_alv    TYPE REF TO cl_gui_alv_grid,
      s_layout TYPE lvc_s_layo,
      s_cont   TYPE REF TO cl_gui_custom_container,
      wa_lfa1  TYPE lfa1.
*BBKO/Vagner Santos - Início da alteração - 25.10.2010
DATA: s_variant TYPE disvariant.
*BBKO/Vagner Santos - Início da alteração - 25.10.2010
DATA e_variant TYPE disvariant.
CONTROLS  tc_alv TYPE TABLEVIEW USING SCREEN '0100'.

CONSTANTS: c_table  TYPE char9 VALUE 'ZLESR0007',
           c_x      TYPE c     VALUE 'X',
           c_b      TYPE c     VALUE 'B',
           c_usd(3) TYPE c     VALUE 'USD',
           c_brl(3) TYPE c     VALUE 'BRL',
           c_fre(4) TYPE c     VALUE 'ZFRE',
           c_ped(4) TYPE c     VALUE 'ZPED',
           c_seg(4) TYPE c     VALUE 'ZSEG',
           c_vct(4) TYPE c     VALUE 'ZVCT',
           c_iof(4) TYPE c     VALUE 'ZIOF'.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE TEXT-002.
    SELECT-OPTIONS:
      p_bukrs FOR t001-bukrs OBLIGATORY.  "NO INTERVALS NO-EXTENSION.
    PARAMETERS:
      p_vsart TYPE vttk-vsart  OBLIGATORY.
    SELECT-OPTIONS:
      s_tdlnr FOR vttk-tdlnr.
    PARAMETERS:
      p_propv TYPE lfa1-lifnr,
      p_placa TYPE zlest0002-pc_veiculo.
  SELECTION-SCREEN END   OF BLOCK a2.

  SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME TITLE TEXT-003.
    SELECT-OPTIONS:
      s_erdat FOR vttk-erdat OBLIGATORY,
      s_tplst FOR vttk-tplst,
      s_shtyp FOR vttk-shtyp,
      s_tknum FOR vttk-tknum,
      s_route FOR vttk-route,
      s_safra FOR lips-lgort,
      s_matnr FOR mara-matnr.
  SELECTION-SCREEN END   OF BLOCK a3.

* Ponto de partida
  SELECTION-SCREEN BEGIN OF BLOCK a6 WITH FRAME TITLE TEXT-042.
    SELECT-OPTIONS:
      s_knota  FOR vtts-knota,  " Ponto de partida
      s_vstel  FOR vtts-vstel,  " loc.expedição
      s_lstel  FOR vtts-lstel,  " ponto de carregamento no local de expedição
      s_werka  FOR vtts-werka,  " centro
      s_lgorta FOR vtts-lgorta, " depósito no centro
      s_kunna  FOR vtts-kunna,  " cliente
      s_lifna  FOR vtts-lifna.  " fornecedor
  SELECTION-SCREEN END   OF BLOCK a6.

* Local de chegada
  SELECTION-SCREEN BEGIN OF BLOCK a5 WITH FRAME TITLE TEXT-043.
    SELECT-OPTIONS:
      s_knotz  FOR vtts-knotz,  " entroncamento
      s_vstez  FOR vtts-vstez,  " local de expedição
      s_lstez  FOR vtts-lstez,  " ponto carregamento no local de expedição
      s_werkz  FOR vtts-werkz,  " centro
      s_lgortz FOR vtts-lgortz, " depósito no centro
      s_kunnz  FOR vtts-kunnz,  " cliente
      s_lifnz  FOR vtts-lifnz.  " fornecedor
  SELECTION-SCREEN END   OF BLOCK a5.

  SELECTION-SCREEN BEGIN OF BLOCK a4 WITH FRAME TITLE TEXT-004.

    PARAMETERS:
      rb_out TYPE char1 AS CHECKBOX DEFAULT 'X',

      rb_tra TYPE char1 AS CHECKBOX,

      rb_in  TYPE char1 AS CHECKBOX.

*  rb_out  TYPE char1 RADIOBUTTON GROUP rb02 DEFAULT 'X',
*
*  rb_tra  TYPE char1 RADIOBUTTON GROUP rb02,
*
*  rb_in   TYPE char1 RADIOBUTTON GROUP rb02.

  SELECTION-SCREEN END   OF BLOCK a4.
*&-------------------------------------------------------------------&*
*& Modificação - Eduardo Ruttkowski Tavares - 06.02.2014   >>> INI   &*
*& ZLES0033 - Atualização BPC - Logistica                            &*
*&-------------------------------------------------------------------&*
  SELECTION-SCREEN: BEGIN OF BLOCK a7 WITH FRAME TITLE TEXT-074.

    PARAMETERS:
      rb_atual TYPE char1 AS CHECKBOX.

  SELECTION-SCREEN: END OF BLOCK a7.
*&-------------------------------------------------------------------&*
*& Modificação - Eduardo Ruttkowski Tavares - 14.11.2013 >>> END     &*
*&-------------------------------------------------------------------&*
SELECTION-SCREEN END   OF BLOCK a1.


*---------------------------------------------------------------------*
*       CLASS lcl_eventhandler DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_eventhandler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no
          sender.

ENDCLASS.                    "lcl_eventhandler DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_eventhandler IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_eventhandler IMPLEMENTATION.
*** csb/ini 10092020
  METHOD handle_button_click.
*   define local data
    DATA:
      ls_alv    TYPE type_alv,
      ls_col_id TYPE lvc_s_col.

    READ TABLE t_alv INTO ls_alv INDEX es_row_no-row_id.

    CONCATENATE ls_alv-tknum ls_alv-instrucao INTO wl_name.

    CONDENSE wl_name NO-GAPS.

    IF ls_alv-instrucao IS NOT INITIAL.

      PERFORM read_text USING 'OBS' wl_name.

      IF tg_texto[]  IS NOT INITIAL.
        LOOP AT tg_texto INTO wg_texto.
          MOVE: wg_texto-tdline TO wl_texto.
          APPEND wl_texto TO tl_texto.
          CLEAR: wl_texto,wg_texto.
        ENDLOOP.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Observação'
            im_display_mode = abap_true
          CHANGING
            ch_text         = tl_texto.

        wl_header-tdobject = 'ZLESR0007'.
        wl_header-tdname   =  wl_name.
        wl_header-tdid     = 'OBS'.
        wl_header-tdspras  = sy-langu.

        PERFORM save_text USING 'OBS'
                                wl_header.

        FREE: tl_texto, tg_texto.

      ELSE.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title = 'Observação'
          CHANGING
            ch_text  = tl_texto.

        IF tl_texto[] IS NOT INITIAL.

          wl_header-tdobject = 'ZLESR0007'.
          wl_header-tdname   =  wl_name.
          wl_header-tdid     = 'OBS'.
          wl_header-tdspras  = sy-langu.


          PERFORM save_text USING 'OBS'
                                  wl_header.
        ENDIF.

        FREE: tl_texto, tg_texto.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "handle_button_click
*** csb/fim 10092020
ENDCLASS.                    "lcl_eventhandler IMPLEMENTATION

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  REFRESH t_alv.

  IF NOT rb_out IS INITIAL.
*   Seleciona Dados Saída
    PERFORM z_seleciona_saida USING 1.
*   Agrupa Dados
    PERFORM z_agrupa_dados.
  ENDIF.

  IF NOT rb_tra IS INITIAL.
*   Seleciona Dados Saída
    PERFORM z_seleciona_saida_3 USING 3.
*   Agrupa Dados
    PERFORM z_agrupa_dados_3.
  ENDIF.

  IF NOT rb_in IS INITIAL.
*   Seleciona Dados Saída
    PERFORM z_seleciona_saida_2 USING 2.
*   Agrupa Dados
    PERFORM z_agrupa_dados_2.

  ENDIF.

  IF NOT t_alv[] IS INITIAL.
    IF NOT rb_atual IS INITIAL.
*     Atualiza B3PC
      PERFORM z_atualiza_bpc.
    ELSE.
*   Estruturas ALV

*--------------------------------------------------------------------------------------------------------*
*   "seleção dados referente documento VT US 117449 / AOENNING / 06-07-2023
*--------------------------------------------------------------------------------------------------------*
      PERFORM fm_sel_dados_txt_vt.

*--------------------------------------------------------------------------------------------------------*
*   "Fim seleção dados referente documento VT US 117449 / AOENNING / 06-07-2023
*--------------------------------------------------------------------------------------------------------*


      PERFORM z_estruturas_alv.
      CALL SCREEN 0100.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_SAIDA                                        *
*&---------------------------------------------------------------------*
*                       Seleciona Dados Saída                          *
*----------------------------------------------------------------------*
FORM z_seleciona_saida USING p_tipo TYPE i.

  PERFORM:
* Seleciona Documentos de Transporte
           z_seleciona_vttk USING p_tipo,
* Seleciona Etapa do transporte
           z_seleciona_vtts,
* Seleciona Itens do Transporte
           z_seleciona_vttp,
* Seleciona Documento de Vendas
           z_seleciona_vbfa,
* Seleciona Remessas
           z_seleciona_likp,
* Seleciona dados de chegada
           z_seleciona_zlest0039,
*Seleciona parceiros da remessa
           z_seleciona_vbpa,
*Seleciona parceiros
           z_seleciona_lfa13,
* Seleciona Itens Remessa
           z_seleciona_lips,
* Seleciona Centro
           z_seleciona_werks,
* Seleciona Itens Remessa - material
           z_seleciona_makt,
* Seleciona Itens Nota Fiscal
           z_seleciona_lin ,
* Seleciona Documentos NF
           z_seleciona_doc ,
* Seleciona Itens Custo de Frete
           z_seleciona_vfkp,
* Seleciona Dados Lançamentos
           z_seleciona_lanc,
* Seleciona Dados Org. Transporte
           z_seleciona_ttds,
* Seleciona Dados de Endereço
           z_seleciona_adrc,
* Seleciona Parceiros Transporte
           z_seleciona_vtpa,
* Seleciona Dados Cliente
           z_seleciona_kna1,
* Seleciona Dados Parceiros
           z_seleciona_lfa1,
* Seleciona Dados Custos de frete
           z_seleciona_vfsi,
* Seleciona Condição ZFRE
           z_seleciona_konv,
* Seleciona Dados Itinerários
          z_seleciona_tvro,
* Seleciona Moeda
          z_seleciona_moeda.

ENDFORM.                    " Z_SELECIONA_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VTTK                                         *
*&---------------------------------------------------------------------*
*                 Seleciona Documentos de Transporte                   *
*----------------------------------------------------------------------*
FORM z_seleciona_vttk USING p_tipo TYPE i.
  REFRESH t_filtro.
  SELECT vttk~tknum
    FROM vttk
    INNER JOIN ttds ON ttds~tplst  = vttk~tplst
    INTO TABLE t_filtro
    WHERE  vttk~tknum IN s_tknum
    AND  vttk~shtyp IN s_shtyp
    AND  vttk~tplst IN s_tplst
    AND  vttk~vsart EQ p_vsart
    AND  vttk~erdat IN s_erdat
    AND  vttk~tdlnr IN s_tdlnr
    AND  ttds~bukrs IN p_bukrs.

  CHECK t_filtro[] IS NOT INITIAL.

  SELECT *
  FROM vttk
  INTO TABLE t_vttk
  FOR ALL ENTRIES IN t_filtro
  WHERE tknum EQ t_filtro-tknum.



*  SELECT *
*  FROM VTTK
*  INTO TABLE T_VTTK
*  WHERE  TKNUM IN S_TKNUM
*    AND  SHTYP IN S_SHTYP
*    AND  TPLST IN S_TPLST
*    AND  VSART EQ P_VSART
*    AND  ERDAT IN S_ERDAT
*    AND  TDLNR IN S_TDLNR.

  DELETE t_vttk WHERE route NOT IN s_route.

  IF p_placa IS NOT INITIAL.
    DELETE t_vttk  WHERE text1(7) NE p_placa.
  ENDIF.

  IF p_propv IS NOT INITIAL.
    IF t_vttk[] IS NOT INITIAL.
      SELECT *
      FROM vtpa
      INTO TABLE t_vtpa_p
      FOR ALL ENTRIES IN t_vttk
      WHERE  vbeln EQ t_vttk-tknum
      AND  parvw   EQ 'PV'
      AND lifnr    EQ p_propv.
    ENDIF.

    SORT t_vtpa_p BY vbeln.
    LOOP AT t_vttk  INTO sl_vttk.
      tabix = sy-tabix  .
      READ TABLE t_vtpa_p INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum BINARY SEARCH.
      IF sy-subrc NE 0.
        DELETE t_vttk INDEX tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CASE p_tipo.

    WHEN 1.

* Transporte de saída
      DELETE t_vttk WHERE abfer NE '1'
                      AND abfer NE '3'.

      DELETE t_vttk WHERE shtyp = 'Z020'.

    WHEN 2.

* Transporte de entrada
      DELETE t_vttk WHERE abfer NE '2'
                      AND abfer NE '4'.

    WHEN 3.

* Transporte de entrada
      DELETE t_vttk WHERE shtyp <> 'Z020'.

  ENDCASE.

  IF t_vttk[] IS INITIAL.
    MESSAGE i836 WITH TEXT-005.
*Inicio de alteração - FMARTINSFM - IR086185 - CS0973588 - 22/03/20022
*    LEAVE LIST-PROCESSING.
*Fim de alteração - FMARTINSFM - IR086185 - CS0973588 - 22/03/20022
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

  DATA: tl_vttp  TYPE TABLE OF vttp,
        sl_vbfa  TYPE vbfa,
        vl_index TYPE i.

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

  IF t_vbfa_va IS NOT INITIAL.
    SELECT vbeln kvgr3
       FROM vbak
       INTO TABLE t_vbak
       FOR ALL ENTRIES IN t_vbfa_va
       WHERE  vbeln   EQ t_vbfa_va-vbelv.

    SORT t_vbak BY vbeln.

    IF t_vbak[] IS NOT INITIAL.
      SELECT * INTO TABLE t_zsdt0053
        FROM zsdt0053
         FOR ALL ENTRIES IN t_vbak
       WHERE vbeln = t_vbak-vbeln.
      SORT t_zsdt0053 BY vbeln.

      SELECT * INTO TABLE t_zsdt0066
        FROM zsdt0066
         FOR ALL ENTRIES IN t_vbak
       WHERE vbeln = t_vbak-vbeln.
      SORT t_zsdt0066 BY vbeln.
    ENDIF.

  ENDIF.

  SELECT *
    FROM vbfa
    INTO TABLE t_vbfa_vf
    FOR ALL ENTRIES IN tl_vttp
  WHERE  vbelv   EQ tl_vttp-vbeln
    AND  vbtyp_n EQ 'M'
    AND  vbtyp_v EQ 'J'.

  IF t_vbfa_vf[] IS NOT INITIAL.
    SELECT *
      FROM vbrk
      INTO TABLE t_vbrk
      FOR ALL ENTRIES IN t_vbfa_vf
    WHERE  vbeln EQ t_vbfa_vf-vbeln
      AND  fksto NE 'X'.
  ENDIF.

  SORT t_vbrk    BY vbeln ASCENDING.

  SORT t_vbfa_va BY vbeln ASCENDING
                    posnn ASCENDING.

  SORT t_vbfa_vf BY vbelv ASCENDING
                    posnv ASCENDING.

* Deleção Estornados
  LOOP AT t_vbfa_vf INTO sl_vbfa.

    vl_index = sy-tabix.

    READ TABLE t_vbrk
      WITH KEY vbeln = sl_vbfa-vbeln
      BINARY SEARCH
      TRANSPORTING NO FIELDS.

    IF NOT sy-subrc IS INITIAL.
      DELETE t_vbfa_vf INDEX vl_index.
    ENDIF.

    CLEAR sl_vbfa.

  ENDLOOP.

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

  IF t_likp[] IS NOT INITIAL.
    SELECT *
      FROM vbfa
      INTO TABLE t_vbfa_qt
      FOR ALL ENTRIES IN t_likp
     WHERE vbelv    EQ t_likp-vbeln
     AND   vbtyp_n  EQ 'i'
     AND   bwart    EQ '861'.
  ENDIF.

  SORT t_likp BY vbeln ASCENDING.
  SORT t_vbfa_qt BY vbelv.

ENDFORM.                    " Z_SELECIONA_LIKP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIPS                                         *
*&---------------------------------------------------------------------*
*                         Seleciona Itens Remessa                      *
*----------------------------------------------------------------------*
FORM z_seleciona_lips.

  DATA : vl_tabix TYPE sy-tabix.

  CHECK NOT t_likp[] IS INITIAL.

  SELECT *
    FROM lips
    INTO TABLE t_lips
    FOR ALL ENTRIES IN t_likp
  WHERE  vbeln EQ t_likp-vbeln AND
         matnr IN s_matnr.

  SORT t_lips BY vbeln ASCENDING
                 posnr ASCENDING.

* Filtro do documento de transporte por material.
  LOOP AT t_vttp INTO sl_vttp.

    vl_tabix = sy-tabix.

    READ TABLE t_lips INTO sl_lips WITH KEY vbeln = sl_vttp-vbeln BINARY SEARCH.

    IF NOT sy-subrc IS INITIAL.
      DELETE t_vttk WHERE tknum = sl_vttp-tknum.
      DELETE t_vttp INDEX vl_tabix.
      CLEAR sl_vttp.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " Z_SELECIONA_LIPS


*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_WERKS                                        *
*&---------------------------------------------------------------------*
*                         Seleciona Itens Remessa                      *
*----------------------------------------------------------------------*
FORM z_seleciona_werks.

  DATA : vl_tabix TYPE sy-tabix.

  CHECK NOT t_lips[] IS INITIAL.

  SELECT *
    FROM t001w
    INTO TABLE t_t001w
    FOR ALL ENTRIES IN t_lips
    WHERE werks EQ t_lips-werks.


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

  CLEAR: t_zlest0173[], t_zlest0173, t_zsdt0001ovro[], t_zsdt0001ovro.

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

  IF t_doc[] IS NOT INITIAL.
    SORT t_doc       BY docnum ASCENDING.
    SELECT  docnum qtd_cheg dtachegada
     FROM zlest0035
     INTO TABLE t_zlest0035
     FOR ALL ENTRIES IN t_doc
   WHERE nr_nf    EQ t_doc-nfenum
   "AND   SERIE_NF EQ T_DOC-SERIES
   AND   docnum   EQ t_doc-docnum.

    SELECT * INTO TABLE t_zlest0173
      FROM zlest0173
       FOR ALL ENTRIES IN t_doc
     WHERE docnum EQ t_doc-docnum
       AND ch_referencia NE space.

    IF sy-subrc IS INITIAL.
      SELECT * INTO TABLE t_zsdt0001ovro
        FROM zsdt0001ovro
         FOR ALL ENTRIES IN t_zlest0173
       WHERE ch_referencia_sai EQ t_zlest0173-ch_referencia.
    ENDIF.

  ENDIF.

  SORT t_zlest0035 BY docnum ASCENDING.
  SORT t_zlest0035 BY docnum.
  SORT t_zsdt0001ovro BY ch_referencia_sai.

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
    MESSAGE i836 WITH TEXT-006.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_VFKP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_KNA1                                         *
*&---------------------------------------------------------------------*
*                        Seleciona Dados Cliente                       *
*----------------------------------------------------------------------*
FORM z_seleciona_kna1.

  DATA : tl_likp TYPE TABLE OF likp,
         tl_vtpa TYPE TABLE OF vtpa.

  CHECK NOT t_likp[] IS INITIAL.

  tl_likp[] = t_likp[].

  SORT tl_likp BY kunnr ASCENDING.

  DELETE ADJACENT DUPLICATES FROM tl_likp COMPARING kunnr.

  SELECT *
    FROM kna1
    INTO TABLE t_kna1
     FOR ALL ENTRIES IN tl_likp
   WHERE kunnr EQ tl_likp-kunnr.

  SORT t_kna1 BY kunnr ASCENDING.

  IF NOT t_vtpa[] IS INITIAL.

    tl_vtpa[] = t_vtpa[].
    SORT tl_vtpa BY kunnr ASCENDING.
    DELETE tl_vtpa WHERE kunnr IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM tl_vtpa COMPARING kunnr.

    SELECT *
    FROM kna1
    APPENDING TABLE t_kna1
    FOR ALL ENTRIES IN tl_vtpa
    WHERE kunnr EQ tl_vtpa-kunnr.

    SORT t_kna1 BY kunnr ASCENDING.

  ENDIF.

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
  WHERE conhecimento EQ tl_vttk-exti1.

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
  WHERE  spras EQ sy-langu
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
    APPENDING TABLE t_adrc
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
  WHERE  vbeln EQ t_vttk-tknum.

  SORT t_vtpa BY vbeln ASCENDING
                 parvw ASCENDING.

ENDFORM.                    " Z_SELECIONA_VTPA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LFA1                                         *
*&---------------------------------------------------------------------*
*                      Seleciona Dados Parceiros                       *
*----------------------------------------------------------------------*
FORM z_seleciona_lfa1.

  DATA tl_vtpa TYPE TABLE OF vtpa.

  CHECK NOT t_vtpa[] IS INITIAL.

  tl_vtpa[] = t_vtpa[].
  SORT tl_vtpa BY lifnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vtpa COMPARING lifnr.

  SELECT *
    FROM lfa1
    APPENDING TABLE t_lfa1
    FOR ALL ENTRIES IN tl_vtpa
  WHERE  lifnr EQ tl_vtpa-lifnr.

  SORT t_lfa1 BY lifnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_LFA1

*&---------------------------------------------------------------------*
*&      Form  Z_AGRUPA_DADOS                                           *
*&---------------------------------------------------------------------*
*                               Agrupa Dados                           *
*----------------------------------------------------------------------*
FORM z_agrupa_dados.

  DATA: text_cliente_cgc TYPE c LENGTH 18,
        text_cliente_cpf TYPE c LENGTH 14.

  DATA: wa_kna1 TYPE kna1.

  DATA: sl_vttk    TYPE vttk,
        sl_vbfa_va TYPE vbfa,
        sl_vbfa_vf TYPE vbfa,
        sl_vttp    TYPE vttp,
        sl_doc     TYPE j_1bnfdoc,
        sl_lin     TYPE j_1bnflin,
        sl_lips    TYPE lips,
        sl_makt    TYPE makt,
        sl_likp    TYPE likp,
        sl_t0016   TYPE zlest0016,
        sl_adrc    TYPE adrc,
        sl_lfa1    TYPE lfa1,
        sl_tvkn    TYPE tvkn,
        sl_kna1    TYPE kna1,
        sl_ttdst   TYPE ttdst,
        sl_tvro    TYPE tvro,
        sl_vfkp    TYPE vfkp,
        sl_vfsi    TYPE vfsi,
        sl_ekbe    TYPE ekbe,
        sl_ttds    TYPE ttds,
        sl_tvst    TYPE tvst,
        sl_vtpa    TYPE vtpa,
        sl_mkpf    TYPE mkpf,
        sl_vtpa_mt TYPE vtpa,
        sl_vtts    TYPE vtts,
        sl_alv     TYPE type_alv,
        sl_lfa1_mt TYPE lfa1,
        sl_konv    TYPE konv,
        vl_dias    TYPE i,
        vl_refkey  TYPE j_1bnflin-refkey.

  "  REFRESH t_alv.

  SORT:
  t_vtts   BY tknum              ASCENDING,
  t_likp   BY vbeln              ASCENDING,
  t_lips   BY vbeln              ASCENDING,
  t_makt   BY matnr              ASCENDING,
  t_ekbe   BY belnr              ASCENDING,
  ti_mkpf  BY le_vbeln           ASCENDING,
  t_lin    BY refkey             ASCENDING,
  t_doc    BY docnum             ASCENDING,
  t_vfkp   BY rebel              ASCENDING,
  t_kna1   BY kunnr              ASCENDING,
  t_t0016  BY conhecimento       ASCENDING,
  ti_tvst  BY vstel              ASCENDING,
  t_adrc   BY addrnumber         ASCENDING,
  t_vtpa   BY vbeln parvw        ASCENDING,
  t_lfa1   BY lifnr              ASCENDING,
  t_vfkp   BY rebel              ASCENDING,
  t_vfsi   BY knumv vbeln posnr  ASCENDING,
  t_ttdst  BY tplst              ASCENDING,
  ti_tvkn  BY knote              ASCENDING,
  t_konv   BY knumv kposn kschl  ASCENDING,
  t_tcurr  BY gdatu              DESCENDING,
  t_vbpa   BY vbeln              ASCENDING,
  t_lfa1_3 BY lifnr              ASCENDING,
  t_t001w  BY werks              ASCENDING.

*---> 05/07/2023 - Migração S4 - DL
  SORT t_zlest0173 BY docnum.
  SORT ti_lfa1 BY lifnr.
  SORT t_zlest0035 BY docnum.
  SORT t_adrc BY addrnumber.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT t_vttk INTO sl_vttk.
    IF ( NOT p_placa IS INITIAL ) AND ( sl_vttk-text1(7) NE p_placa ).
      CONTINUE.
    ENDIF.

    CLEAR sl_vttp.
* Remessa referentes ao documento de transporte
    LOOP AT t_vttp INTO sl_vttp WHERE tknum EQ sl_vttk-tknum.

      CLEAR : sl_vtts, sl_likp, sl_lips, sl_makt, sl_ekbe, sl_mkpf, sl_lin,  sl_doc, sl_t0016,
              sl_vfkp, sl_alv,  sl_kna1, sl_adrc, sl_lfa1, sl_tvro, sl_konv, sl_vfkp, sl_tcurr,
              sl_vbpa, sl_lfa1_3, sl_t001w, wa_kna1 .

* Tipo de documento de transporte
      sl_alv-shtyp = sl_vttk-shtyp.
      sl_alv-identi = sl_vttk-tndr_trkid.

* Etapas do documento de transporte
      READ TABLE t_vtts INTO sl_vtts WITH KEY tknum = sl_vttk-tknum BINARY SEARCH.

* Documento de remessa - cabeçalho
      READ TABLE t_likp INTO sl_likp WITH KEY vbeln = sl_vttp-vbeln BINARY SEARCH.

* Documento de remessa - Item
      READ TABLE t_lips INTO sl_lips WITH KEY vbeln = sl_likp-vbeln BINARY SEARCH.

* Material - Descrição
      READ TABLE t_makt INTO sl_makt WITH KEY matnr = sl_lips-matnr BINARY SEARCH.

* Peso Confirmado
      CLEAR sl_t0016.
      IF sl_vttk-exti1 IS NOT INITIAL.
        READ TABLE t_t0016 INTO sl_t0016 WITH KEY conhecimento = sl_vttk-exti1 BINARY SEARCH.
        IF sy-subrc = 0.
          CLEAR  sl_t0016.
        ENDIF.
      ENDIF.

* Endereço de destino - Cliente
      READ TABLE t_kna1 INTO sl_kna1 WITH KEY kunnr = sl_likp-kunnr BINARY SEARCH.

* Cliente
      sl_alv-name1_kn        = sl_kna1-name1.

      IF sl_kna1-stkzn IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = sl_kna1-stcd1
          IMPORTING
            output = text_cliente_cgc.
        sl_alv-stcd1_kn = text_cliente_cgc.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            input  = sl_kna1-stcd2
          IMPORTING
            output = text_cliente_cpf.
        sl_alv-stcd1_kn = text_cliente_cpf.
      ENDIF.

      sl_alv-stcd3_kn        = sl_kna1-stcd3.

      IF sl_vttk-vsart = '01'.

* Endereço de origem
        READ TABLE ti_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtts-lifna BINARY SEARCH.

        IF sy-subrc IS INITIAL.

* Endereço de origem
          CLEAR sl_adrc.
          READ TABLE t_adrc INTO sl_adrc WITH KEY addrnumber = sl_lfa1-adrnr BINARY SEARCH.

* Endereço de origem
*          IF sy-subrc IS INITIAL.
*            sl_alv-city1_ad         = sl_adrc-city1.
*            sl_alv-region_ad        = sl_adrc-region.
*          ENDIF.

        ELSE.

* Endereço de origem
          READ TABLE ti_tvst INTO sl_tvst WITH KEY vstel = sl_vtts-vstel BINARY SEARCH.

* Endereço de origem
          CLEAR sl_adrc.
          READ TABLE t_adrc INTO sl_adrc WITH KEY addrnumber = sl_tvst-adrnr BINARY SEARCH.

* Endereço de origem
          IF sy-subrc IS INITIAL.
            sl_alv-city1_ad         = sl_adrc-city1.
            sl_alv-region_ad        = sl_adrc-region.
          ENDIF.

        ENDIF.

      ELSE.

        READ TABLE ti_tvkn INTO sl_tvkn WITH KEY knote = sl_vtts-knota BINARY SEARCH.
        READ TABLE t_adrc  INTO sl_adrc WITH KEY addrnumber = sl_tvkn-adrnr BINARY SEARCH.

* Endereço de origem
        IF sy-subrc IS INITIAL.
          sl_alv-city1_ad         = sl_adrc-city1.
          sl_alv-region_ad        = sl_adrc-region.
        ENDIF.

      ENDIF.

* Endereço de destino
      IF NOT sl_vtts-knotz IS INITIAL.

        CLEAR sl_adrc.

        READ TABLE ti_tvkn INTO sl_tvkn WITH KEY knote = sl_vtts-knotz BINARY SEARCH.

        IF ( sl_tvkn-adrnr IS INITIAL ).
          SELECT SINGLE * FROM kna1 INTO wa_kna1 WHERE kunnr EQ sl_tvkn-kunnr.
        ELSE.
          READ TABLE t_adrc  INTO sl_adrc WITH KEY addrnumber = sl_tvkn-adrnr.
        ENDIF.

      ELSE.

        SELECT SINGLE * FROM kna1 INTO wa_kna1 WHERE kunnr EQ sl_vtts-kunnz.
        READ TABLE t_adrc  INTO sl_adrc WITH KEY addrnumber = sl_vtts-adrnz .

      ENDIF.

* Endereço de destino
      IF NOT ( wa_kna1-name1 IS INITIAL ).

        sl_alv-ort01_kn        = wa_kna1-ort01.
        sl_alv-regio_kn        = wa_kna1-regio.

      ELSE.

        IF NOT sl_adrc-city1 IS INITIAL.
          sl_alv-ort01_kn        = sl_adrc-city1.
          sl_alv-regio_kn        = sl_adrc-region.
        ELSE.
          sl_alv-ort01_kn = sl_alv-cid_entrega.
          sl_alv-regio_kn = sl_alv-reg_entrega.
        ENDIF.
      ENDIF.

* Transportador
      READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vttk-tdlnr BINARY SEARCH.
      sl_alv-name1_lf        = sl_lfa1-name1.

      IF sl_vttk-vsart = '01'.

* Conhecimento e carta frete
        sl_alv-exti1           = sl_vttk-exti1.
        sl_alv-exti2           = sl_vttk-exti2.

* Placa do veiculo
        sl_alv-text1           = sl_vttk-text1.

* Proprietário
        READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'PV' .
        READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtpa-lifnr               .
        sl_alv-name1_pv        = sl_lfa1-name1.
        IF sl_lfa1-stkzn = 'X'.
          sl_alv-stcd_pv = sl_lfa1-stcd2 .
        ELSE.
          sl_alv-stcd_pv = sl_lfa1-stcd1 .
        ENDIF.

        IF ( NOT p_propv IS INITIAL ) AND ( p_propv <> sl_lfa1-lifnr ).
          CONTINUE.
        ENDIF.

*Local de carregamento
        READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'PC' BINARY SEARCH.
        READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtpa-lifnr BINARY SEARCH.

        sl_alv-cd_carreg   = sl_lfa1-lifnr.
        sl_alv-desc_carreg = sl_lfa1-name1.
        sl_alv-city1_ad    = sl_lfa1-ort01.
        sl_alv-region_ad   = sl_lfa1-regio.

* Terminal
        READ TABLE t_vbpa  INTO sl_vbpa  WITH KEY vbeln = sl_likp-vbeln  parvw = 'Z1'  .
        READ TABLE t_lfa1_3 INTO sl_lfa1_3 WITH KEY lifnr = sl_vbpa-lifnr BINARY SEARCH.

        sl_alv-cd_terminal   = sl_lfa1_3-lifnr.
        sl_alv-desc_terminal = sl_lfa1_3-name1.
        sl_alv-cid_terminal  = sl_lfa1_3-ort01.
        sl_alv-end_terminal  = sl_lfa1_3-stras.

* Local de Entrega
        READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'LR' BINARY SEARCH.
        READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtpa-lifnr BINARY SEARCH.

        sl_alv-cid_entrega   = sl_lfa1-ort01.
        sl_alv-reg_entrega   = sl_lfa1-regio.

* Motorista
        READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'MT' BINARY SEARCH.
        READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtpa-lifnr BINARY SEARCH.
        sl_alv-name1_mt        = sl_lfa1-name1.
        IF sl_lfa1-stkzn = 'X'.
          sl_alv-stcd_mt = sl_lfa1-stcd2 .
        ELSE.
          sl_alv-stcd_mt = sl_lfa1-stcd1 .
        ENDIF.

* Local de entrega
        IF NOT ( wa_kna1-name1 IS INITIAL ).
          sl_alv-name1_lr        = wa_kna1-name1.
        ELSE.
          READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'LR' BINARY SEARCH.
          READ TABLE t_kna1 INTO sl_kna1 WITH KEY kunnr = sl_vtpa-kunnr BINARY SEARCH              .
          sl_alv-name1_lr        = sl_kna1-name1.
        ENDIF.


      ENDIF.

* Itinerario
      sl_alv-route           = sl_vttk-route.

* Material
      sl_alv-matnr           = sl_lips-matnr.
      sl_alv-maktx           = sl_makt-maktx.
      sl_alv-charg           = sl_lips-charg.

* Peso saída
      sl_alv-brgew           = sl_lips-brgew.

* Nome da Filial
      READ TABLE t_t001w INTO sl_t001w WITH KEY werks = sl_lips-werks BINARY SEARCH.
      sl_alv-werks   = sl_t001w-name1.

      IF sl_t0016 IS INITIAL.

*        READ TABLE t_tvro INTO sl_tvro WITH KEY route = sl_vttk-route  BINARY SEARCH.
*
*        IF sy-subrc IS INITIAL.
*
** Calcula dias
*          PERFORM z_calcula_dias USING sl_tvro-traztd
*                              CHANGING vl_dias.
*
*          sl_alv-peso_confirmado = sl_lips-brgew.
*          sl_alv-diferenca       = 0.
*          sl_alv-dta_chegada     = sl_vttk-datbg + vl_dias.

*        ELSE.
*
*          sl_alv-peso_confirmado = sl_lips-brgew.
*          sl_alv-diferenca       = 0.
*          sl_alv-dta_chegada     = sl_vttk-datbg + 1.
*
*        ENDIF.
        " ALRS 07.10.13
*        READ TABLE T_ZLEST0039 INTO W_ZLEST0039 WITH KEY VBELN = SL_VTTP-VBELN BINARY SEARCH.
*        IF SY-SUBRC = 0.
*          IF W_ZLEST0039-PONTOTRANSB IS INITIAL.
*            SL_ALV-PESO_CONFIRMADO = W_ZLEST0039-PESOCHEGADA.
*            SL_ALV-DIFERENCA       = SL_LIPS-BRGEW - W_ZLEST0039-PESOCHEGADA.
*            SL_ALV-DTA_CHEGADA     = W_ZLEST0039-DATACHEGADA.
*          ELSE.
*            SL_ALV-PESO_CONFIRMADO = W_ZLEST0039-PESOTRANSB.
*            SL_ALV-DIFERENCA       = SL_LIPS-BRGEW - W_ZLEST0039-PESOTRANSB.
*            SL_ALV-DTA_CHEGADA     = W_ZLEST0039-DATATRANSB.
*          ENDIF.
*        ENDIF.


      ELSE.

        sl_alv-peso_confirmado = sl_t0016-peso_confirmado.
        sl_alv-diferenca       = sl_lips-brgew - sl_t0016-peso_confirmado.
        sl_alv-dta_chegada     = sl_t0016-dta_chegada.

      ENDIF.

* Data do transporte
      sl_alv-datbg           = sl_vttk-datbg.

* Documento de custo de frete
      READ TABLE t_vfkp INTO sl_vfkp WITH KEY rebel = sl_vttk-tknum BINARY SEARCH                 .
      READ TABLE t_vfsi INTO sl_vfsi WITH KEY knumv = sl_vfkp-knumv  vbeln = sl_lips-vbeln  posnr = sl_lips-posnr BINARY SEARCH .
      READ TABLE t_konv INTO sl_konv WITH KEY knumv = sl_vfkp-knumv  kposn = sl_vfsi-kposn kschl = c_fre BINARY SEARCH         .

* Analitico
********************************************************************************************************************************
* Valor unitario
      sl_alv-kbetr           = sl_konv-kbetr.

* Total do frete
      sl_alv-netwr           = sl_vfsi-kzwi1.

* Total de impostos
      sl_alv-mwsbp           = sl_vfsi-kzwi6.

********************************************************************************************************************************

* Cambio - Dolar
* Documento de custo de frete
      "      CONCATENATE sl_vfkp-prsdt+6(2) '.' sl_vfkp-prsdt+4(2) '.' sl_vfkp-prsdt(4) INTO sl_data.
      CONCATENATE sl_vttk-erdat+6(2) '.' sl_vttk-erdat+4(2) '.' sl_vttk-erdat(4) INTO sl_data.

      IF NOT sl_data = '00.00.0000'.

        CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
          EXPORTING
            input  = sl_data
          IMPORTING
            output = p_gdatu.

* Cambio / Valor unitario e total em dolar.
        READ TABLE t_tcurr INTO sl_tcurr WITH KEY gdatu = p_gdatu.
        IF sy-subrc IS INITIAL.
          sl_alv-ukurs           = sl_tcurr-ukurs.
          sl_alv-kbetr1          = sl_konv-kbetr / sl_tcurr-ukurs.
          sl_alv-netwr1          = sl_alv-netwr / sl_tcurr-ukurs.
        ELSE.
          READ TABLE t_tcurr INTO sl_tcurr INDEX 1.
          IF sy-subrc IS INITIAL AND sl_tcurr-ukurs <> 0.
            sl_alv-ukurs           = sl_tcurr-ukurs.
            sl_alv-kbetr1          = sl_konv-kbetr / sl_tcurr-ukurs.
            sl_alv-netwr1          = sl_alv-netwr / sl_tcurr-ukurs.
          ELSE.
            sl_alv-ukurs           = 0.
            sl_alv-kbetr1          = 0.
            sl_alv-netwr1          = 0.
          ENDIF.
        ENDIF.
      ENDIF.

* IOF
      READ TABLE t_vfkp INTO sl_vfkp WITH KEY rebel = sl_vttk-tknum  BINARY SEARCH                                             .
      READ TABLE t_vfsi INTO sl_vfsi WITH KEY knumv = sl_vfkp-knumv  vbeln = sl_lips-vbeln  posnr = sl_lips-posnr BINARY SEARCH.
      READ TABLE t_konv INTO sl_konv WITH KEY knumv = sl_vfkp-knumv  kposn = sl_vfsi-kposn kschl = c_iof   BINARY SEARCH       .

      IF sy-subrc IS INITIAL.
        sl_alv-ziof           = sl_konv-kwert.
      ENDIF.

* Seguro
      READ TABLE t_vfkp INTO sl_vfkp WITH KEY rebel = sl_vttk-tknum BINARY SEARCH                                              .
      READ TABLE t_vfsi INTO sl_vfsi WITH KEY knumv = sl_vfkp-knumv  vbeln = sl_lips-vbeln  posnr = sl_lips-posnr BINARY SEARCH.
      READ TABLE t_konv INTO sl_konv WITH KEY knumv = sl_vfkp-knumv  kposn = sl_vfsi-kposn  kschl = c_seg   BINARY SEARCH      .
      IF sy-subrc IS INITIAL.
        sl_alv-zseg  = sl_konv-kwert.
        sl_alv-kbetr_per = sl_konv-kbetr / 100.
      ENDIF.

* Valor triagem
      READ TABLE t_vfkp INTO sl_vfkp WITH KEY rebel = sl_vttk-tknum BINARY SEARCH                                              .
      READ TABLE t_vfsi INTO sl_vfsi WITH KEY knumv = sl_vfkp-knumv  vbeln = sl_lips-vbeln  posnr = sl_lips-posnr BINARY SEARCH.
      READ TABLE t_konv INTO sl_konv WITH KEY knumv = sl_vfkp-knumv  kposn = sl_vfsi-kposn  kschl = c_vct   BINARY SEARCH      .
      IF sy-subrc IS INITIAL.
        sl_alv-zvct  = sl_konv-kwert.
      ENDIF.

* Pegagio
      READ TABLE t_vfkp INTO sl_vfkp WITH KEY rebel = sl_vttk-tknum  BINARY SEARCH                                             .
      READ TABLE t_vfsi INTO sl_vfsi WITH KEY knumv = sl_vfkp-knumv  vbeln = sl_lips-vbeln  posnr = sl_lips-posnr BINARY SEARCH.
      READ TABLE t_konv INTO sl_konv WITH KEY knumv = sl_vfkp-knumv  kposn = sl_vfsi-kposn  kschl = c_ped   BINARY SEARCH      .
      IF sy-subrc IS INITIAL.
        sl_alv-zped  = sl_konv-kwert.
      ENDIF.

      IF sl_lips-brgew GT 0.
        sl_alv-frete_ton = ( ( sl_vfsi-kzwi1 + sl_konv-kwert ) / sl_lips-brgew ) * 1000.
      ELSE.
        sl_alv-frete_ton = 0.
      ENDIF.

* Local de organização de transporte
      READ TABLE t_ttdst INTO sl_ttdst WITH KEY tplst = sl_vttk-tplst  .
      sl_alv-bezei           = sl_ttdst-bezei.

* Documento de transporte
      sl_alv-tknum           = sl_vttk-tknum.
      sl_alv-uaten           = sl_vttk-uatbg.

* Documento de custo
      sl_alv-fknum           = sl_vfkp-fknum.

* Pedido de serviço de frete
      sl_alv-ebeln           = sl_vfkp-ebeln.

* Folha de registros de serviço de frete
      sl_alv-lblni           = sl_vfkp-lblni.

* Ordem de vendas
      READ TABLE t_vbfa_va INTO sl_vbfa_va WITH KEY vbeln = sl_lips-vbeln.
      sl_alv-vbeln_va          = sl_vbfa_va-vbelv.

* Instrução de Algodão
      IF sy-subrc IS INITIAL AND sl_alv-vbeln_va IS NOT INITIAL.
        READ TABLE t_zsdt0053 WITH KEY vbeln = sl_alv-vbeln_va.
        IF sy-subrc IS INITIAL.
          sl_alv-instrucao = t_zsdt0053-instrucao.
        ELSE.
          READ TABLE t_zsdt0066 WITH KEY vbeln = sl_alv-vbeln_va.
          IF sy-subrc IS INITIAL.
            sl_alv-instrucao = t_zsdt0066-instrucao.
          ENDIF.
        ENDIF.
      ENDIF.

* Instruções de Embarque de Algodão
      PERFORM  z_seleciona_zsdt0045 USING sl_alv-instrucao
            CHANGING v_data_in_porto  v_data_porto." V_OBSERVACAO .

      sl_alv-data_in_porto = v_data_in_porto.
      sl_alv-data_porto    = v_data_porto.
      MOVE icon_create_text  TO sl_alv-observacao. "'@01@'.

      IF sl_alv-data_porto IS NOT INITIAL.
        IF sl_alv-data_porto <= sl_alv-dta_chegada .
          sl_alv-xstatus = 'Dentro da Janela'.
        ELSE.
          IF sl_alv-data_porto >= sl_alv-dta_chegada.
            sl_alv-xstatus = 'Fora da Janela'.
          ENDIF.
        ENDIF.
      ENDIF.
* Tipo Prod
      CLEAR sl_vbak.
      READ TABLE t_vbak INTO sl_vbak WITH KEY vbeln = sl_vbfa_va-vbelv.
      sl_alv-kvgr3          = sl_vbak-kvgr3.

* Remessas
      sl_alv-vbeln           = sl_lips-vbeln.

* Faturamento
      READ TABLE t_vbfa_vf INTO sl_vbfa_vf WITH KEY vbelv = sl_likp-vbeln BINARY SEARCH.
      sl_alv-vbeln_vf           = sl_vbfa_vf-vbeln.

* Nota fiscal
      vl_refkey =  sl_vbfa_vf-vbeln.
      READ TABLE t_lin INTO sl_lin WITH KEY refkey = vl_refkey .
      READ TABLE t_doc INTO sl_doc WITH KEY docnum = sl_lin-docnum .
* Nota fiscal-série

* Pega as quantidade chegada ALRS 07.10.13
      READ TABLE t_zlest0035 INTO w_zlest0035 WITH KEY docnum = sl_lin-docnum  BINARY SEARCH.
      IF sy-subrc = 0.
        sl_alv-peso_confirmado = w_zlest0035-qtd_cheg.
        sl_alv-diferenca       = sl_lips-brgew - w_zlest0035-qtd_cheg.
        sl_alv-dta_chegada     = w_zlest0035-dtachegada.
      ENDIF.

* Pega Quantidade de Romaneio de Saida Algodão = Peso de Chegada
      READ TABLE t_zlest0173 INTO DATA(w_zlest0173) WITH KEY docnum = sl_lin-docnum  BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE t_zsdt0001ovro INTO DATA(wa_zsdt0001ovro) WITH KEY ch_referencia_sai = w_zlest0173-ch_referencia BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          sl_alv-peso_confirmado = wa_zsdt0001ovro-nm_peso_subtotal.
          sl_alv-diferenca       = sl_lips-brgew - wa_zsdt0001ovro-nm_peso_subtotal.
          sl_alv-dta_chegada     = w_zlest0173-dt_chegada.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'Z_1B_NF_VALUE_DETERMINATION'
        EXPORTING
          p_docnum   = sl_doc-docnum
        IMPORTING
          ext_header = wa_j_cabe.

      sl_alv-vlr_nf = wa_j_cabe-nftot.

      sl_alv-vlr_nf   = sl_lin-netpr * sl_lin-menge.
      sl_alv-nfenum   = sl_doc-nfenum.
      sl_alv-series   = sl_doc-series.

      APPEND sl_alv TO t_alv.

    ENDLOOP.

  ENDLOOP.

* 133799 CS2024000094 Inclusão de campos para atendimento das operações de algodão - PSA

  LOOP AT t_alv ASSIGNING FIELD-SYMBOL(<get_new_values>) WHERE vbeln_va IS NOT INITIAL AND tknum IS NOT INITIAL.

    IF <get_new_values>-vbeln_va IS NOT INITIAL AND <get_new_values>-tknum IS NOT INITIAL.

      SELECT SINGLE  a~id_carga
      FROM vttk AS a
      WHERE a~tknum = @<get_new_values>-tknum
      INTO @DATA(l_aux_id_carga).

      IF l_aux_id_carga IS NOT INITIAL.
        SELECT SINGLE a~nm_bloco,a~qt_fardos
        FROM zsdt0001fd AS a
        WHERE a~id_carga = @l_aux_id_carga
        and a~nr_ordem_venda = @<get_new_values>-vbeln_va
        INTO (@<get_new_values>-nm_bloco,@<get_new_values>-qt_fardos).
      ENDIF.
    ENDIF.

  ENDLOOP.






  "==========================================================================================





















** Documento de transporte
*  LOOP AT T_VTTK INTO SL_VTTK.
*    IF ( NOT P_PLACA IS INITIAL ) AND ( SL_VTTK-TEXT1(7) NE P_PLACA ).
*      CONTINUE.
*    ENDIF.
*
*    CLEAR SL_VTTP.
** Remessa referentes ao documento de transporte
*    LOOP AT T_VTTP INTO SL_VTTP WHERE TKNUM EQ SL_VTTK-TKNUM.
*
*      CLEAR : SL_VTTS, SL_LIKP, SL_LIPS, SL_MAKT, SL_EKBE, SL_MKPF, SL_LIN,  SL_DOC, SL_T0016,
*              SL_VFKP, SL_ALV,  SL_KNA1, SL_ADRC, SL_LFA1, SL_TVRO, SL_KONV, SL_VFKP, SL_TCURR,
*              SL_VBPA, SL_LFA1_3, SL_T001W .
*
** Tipo de documento de transporte
*      SL_ALV-SHTYP = SL_VTTK-SHTYP.
*      SL_ALV-IDENTI = SL_VTTK-TNDR_TRKID.
*
** Etapas do documento de transporte
*      READ TABLE T_VTTS INTO SL_VTTS WITH KEY TKNUM = SL_VTTK-TKNUM BINARY SEARCH.
*
** Documento de remessa - cabeçalho
*      READ TABLE T_LIKP INTO SL_LIKP WITH KEY VBELN = SL_VTTP-VBELN BINARY SEARCH.
*
** Documento de remessa - Item
*      READ TABLE T_LIPS INTO SL_LIPS WITH KEY VBELN = SL_LIKP-VBELN BINARY SEARCH.
*
** Material - Descrição
*      READ TABLE T_MAKT INTO SL_MAKT WITH KEY MATNR = SL_LIPS-MATNR BINARY SEARCH.
*
** Peso Confirmado
*      READ TABLE T_T0016 INTO SL_T0016 WITH KEY CONHECIMENTO = SL_VTTK-EXTI1 BINARY SEARCH.
*
** Endereço de destino - Cliente
*      READ TABLE T_KNA1 INTO SL_KNA1 WITH KEY KUNNR = SL_LIKP-KUNNR BINARY SEARCH.
*
** Cliente
*      SL_ALV-NAME1_KN        = SL_KNA1-NAME1.
*
*      IF SL_KNA1-STKZN IS INITIAL.
*        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
*          EXPORTING
*            INPUT  = SL_KNA1-STCD1
*          IMPORTING
*            OUTPUT = TEXT_CLIENTE_CGC.
*        SL_ALV-STCD1_KN = TEXT_CLIENTE_CGC.
*      ELSE.
*        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
*          EXPORTING
*            INPUT  = SL_KNA1-STCD2
*          IMPORTING
*            OUTPUT = TEXT_CLIENTE_CPF.
*        SL_ALV-STCD1_KN = TEXT_CLIENTE_CPF.
*      ENDIF.
*
*      SL_ALV-STCD3_KN        = SL_KNA1-STCD3.
*
*      IF SL_VTTK-VSART = '01'.
*
** Endereço de origem
*        READ TABLE TI_LFA1 INTO SL_LFA1 WITH KEY LIFNR = SL_VTTS-LIFNA  BINARY SEARCH.
*
*        IF SY-SUBRC IS INITIAL.
*
** Endereço de origem
*          CLEAR SL_ADRC.
*          READ TABLE T_ADRC INTO SL_ADRC WITH KEY ADDRNUMBER = SL_LFA1-ADRNR BINARY SEARCH.
*
** Endereço de origem
**          IF sy-subrc IS INITIAL.
**            sl_alv-city1_ad         = sl_adrc-city1.
**            sl_alv-region_ad        = sl_adrc-region.
**          ENDIF.
*
*        ELSE.
*
** Endereço de origem
*          READ TABLE TI_TVST INTO SL_TVST WITH KEY VSTEL = SL_VTTS-VSTEL  BINARY SEARCH.
*
** Endereço de origem
*          CLEAR SL_ADRC.
*          READ TABLE T_ADRC INTO SL_ADRC WITH KEY ADDRNUMBER = SL_TVST-ADRNR BINARY SEARCH.
*
** Endereço de origem
*          IF SY-SUBRC IS INITIAL.
*            SL_ALV-CITY1_AD         = SL_ADRC-CITY1.
*            SL_ALV-REGION_AD        = SL_ADRC-REGION.
*          ENDIF.
*
*        ENDIF.
*
*      ELSE.
*
*        READ TABLE TI_TVKN INTO SL_TVKN WITH KEY KNOTE = SL_VTTS-KNOTA      BINARY SEARCH.
*        READ TABLE T_ADRC  INTO SL_ADRC WITH KEY ADDRNUMBER = SL_TVKN-ADRNR BINARY SEARCH.
*
** Endereço de origem
*        IF SY-SUBRC IS INITIAL.
*          SL_ALV-CITY1_AD         = SL_ADRC-CITY1.
*          SL_ALV-REGION_AD        = SL_ADRC-REGION.
*        ENDIF.
*
*      ENDIF.
*
** Endereço de destino
*      IF NOT SL_VTTS-KNOTZ IS INITIAL.
*
*        CLEAR SL_ADRC.
*
*        READ TABLE TI_TVKN INTO SL_TVKN WITH KEY KNOTE = SL_VTTS-KNOTZ      BINARY SEARCH.
*
*        IF ( SL_TVKN-ADRNR IS INITIAL ).
*          SELECT SINGLE * FROM KNA1 INTO WA_KNA1 WHERE KUNNR EQ SL_TVKN-KUNNR.
*        ELSE.
*          READ TABLE T_ADRC  INTO SL_ADRC WITH KEY ADDRNUMBER = SL_TVKN-ADRNR BINARY SEARCH.
*        ENDIF.
*
*      ELSE.
*        READ TABLE T_ADRC  INTO SL_ADRC WITH KEY ADDRNUMBER = SL_VTTS-ADRNZ BINARY SEARCH.
*      ENDIF.
*
** Endereço de destino
*      IF NOT ( WA_KNA1-NAME1 IS INITIAL ).
*
*        SL_ALV-ORT01_KN        = WA_KNA1-ORT01.
*        SL_ALV-REGIO_KN        = WA_KNA1-REGIO.
*
*      ELSE.
*
*        IF NOT SL_ADRC-CITY1 IS INITIAL.
*          SL_ALV-ORT01_KN        = SL_ADRC-CITY1.
*          SL_ALV-REGIO_KN        = SL_ADRC-REGION.
*        ELSE.
*          SL_ALV-ORT01_KN = SL_ALV-CID_ENTREGA.
*          SL_ALV-REGIO_KN = SL_ALV-REG_ENTREGA.
*        ENDIF.
*      ENDIF.
*
** Transportador
*      READ TABLE T_LFA1 INTO SL_LFA1 WITH KEY LIFNR = SL_VTTK-TDLNR BINARY SEARCH.
*      SL_ALV-NAME1_LF        = SL_LFA1-NAME1.
*
*      IF SL_VTTK-VSART = '01'.
*
** Conhecimento e carta frete
*        SL_ALV-EXTI1           = SL_VTTK-EXTI1.
*        SL_ALV-EXTI2           = SL_VTTK-EXTI2.
*
** Placa do veiculo
*        SL_ALV-TEXT1           = SL_VTTK-TEXT1.
*
** Proprietário
*        READ TABLE T_VTPA INTO SL_VTPA WITH KEY VBELN = SL_VTTK-TKNUM  PARVW = 'PV' .
*        READ TABLE T_LFA1 INTO SL_LFA1 WITH KEY LIFNR = SL_VTPA-LIFNR               .
*        SL_ALV-NAME1_PV        = SL_LFA1-NAME1.
*
*        IF ( NOT P_PROPV IS INITIAL ) AND ( P_PROPV <> SL_LFA1-LIFNR ).
*          CONTINUE.
*        ENDIF.
*
**Local de carregamento
*        READ TABLE T_VTPA INTO SL_VTPA WITH KEY VBELN = SL_VTTK-TKNUM  PARVW = 'PC'.
*        READ TABLE T_LFA1 INTO SL_LFA1 WITH KEY LIFNR = SL_VTPA-LIFNR              .
*
*        SL_ALV-CD_CARREG   = SL_LFA1-LIFNR.
*        SL_ALV-DESC_CARREG = SL_LFA1-NAME1.
*        SL_ALV-CITY1_AD    = SL_LFA1-ORT01.
*        SL_ALV-REGION_AD   = SL_LFA1-REGIO.
*
** Terminal
*        READ TABLE T_VBPA  INTO SL_VBPA  WITH KEY VBELN = SL_LIKP-VBELN  PARVW = 'Z1'  BINARY SEARCH.
*        READ TABLE T_LFA1_3 INTO SL_LFA1_3 WITH KEY LIFNR = SL_VBPA-LIFNR                BINARY SEARCH.
*
*        SL_ALV-CD_TERMINAL   = SL_LFA1_3-LIFNR.
*        SL_ALV-DESC_TERMINAL = SL_LFA1_3-NAME1.
*        SL_ALV-CID_TERMINAL  = SL_LFA1_3-ORT01.
*        SL_ALV-END_TERMINAL  = SL_LFA1_3-STRAS.
*
** Local de Entrega
*        READ TABLE T_VTPA INTO SL_VTPA WITH KEY VBELN = SL_VTTK-TKNUM  PARVW = 'LR'  .
*        READ TABLE T_LFA1 INTO SL_LFA1 WITH KEY LIFNR = SL_VTPA-LIFNR                .
*
*        SL_ALV-CID_ENTREGA   = SL_LFA1-ORT01.
*        SL_ALV-REG_ENTREGA   = SL_LFA1-REGIO.
*
** Motorista
*        READ TABLE T_VTPA INTO SL_VTPA WITH KEY VBELN = SL_VTTK-TKNUM  PARVW = 'MT' .
*        READ TABLE T_LFA1 INTO SL_LFA1 WITH KEY LIFNR = SL_VTPA-LIFNR               .
*        SL_ALV-NAME1_MT        = SL_LFA1-NAME1.
*
** Local de entrega
*        IF NOT ( WA_KNA1-NAME1 IS INITIAL ).
*          SL_ALV-NAME1_LR        = WA_KNA1-NAME1.
*        ELSE.
*          READ TABLE T_VTPA INTO SL_VTPA WITH KEY VBELN = SL_VTTK-TKNUM  PARVW = 'LR' .
*          READ TABLE T_KNA1 INTO SL_KNA1 WITH KEY KUNNR = SL_VTPA-KUNNR               .
*          SL_ALV-NAME1_LR        = SL_KNA1-NAME1.
*        ENDIF.
*
*
*      ENDIF.
*
** Itinerario
*      SL_ALV-ROUTE           = SL_VTTK-ROUTE.
*
** Material
*      SL_ALV-MATNR           = SL_LIPS-MATNR.
*      SL_ALV-MAKTX           = SL_MAKT-MAKTX.
*      SL_ALV-CHARG           = SL_LIPS-CHARG.
*
** Peso saída
*      SL_ALV-BRGEW           = SL_LIPS-BRGEW.
*
** Nome da Filial
*      READ TABLE T_T001W INTO SL_T001W WITH KEY WERKS = SL_LIPS-WERKS.
*      SL_ALV-WERKS   = SL_T001W-NAME1.
*
*      IF SL_T0016 IS INITIAL.
*
**        READ TABLE t_tvro INTO sl_tvro WITH KEY route = sl_vttk-route  BINARY SEARCH.
**
**        IF sy-subrc IS INITIAL.
**
*** Calcula dias
**          PERFORM z_calcula_dias USING sl_tvro-traztd
**                              CHANGING vl_dias.
**
**          sl_alv-peso_confirmado = sl_lips-brgew.
**          sl_alv-diferenca       = 0.
**          sl_alv-dta_chegada     = sl_vttk-datbg + vl_dias.
*
**        ELSE.
**
**          sl_alv-peso_confirmado = sl_lips-brgew.
**          sl_alv-diferenca       = 0.
**          sl_alv-dta_chegada     = sl_vttk-datbg + 1.
**
**        ENDIF.
*
*      ELSE.
*
*        SL_ALV-PESO_CONFIRMADO = SL_T0016-PESO_CONFIRMADO.
*        SL_ALV-DIFERENCA       = SL_LIPS-BRGEW - SL_T0016-PESO_CONFIRMADO.
*        SL_ALV-DTA_CHEGADA     = SL_T0016-DTA_CHEGADA.
*
*      ENDIF.
*
** Data do transporte
*      SL_ALV-DATBG           = SL_VTTK-DATBG.
*
** Documento de custo de frete
*      READ TABLE T_VFKP INTO SL_VFKP WITH KEY REBEL = SL_VTTK-TKNUM                                               BINARY SEARCH.
*      READ TABLE T_VFSI INTO SL_VFSI WITH KEY KNUMV = SL_VFKP-KNUMV  VBELN = SL_LIPS-VBELN  POSNR = SL_LIPS-POSNR BINARY SEARCH.
*      READ TABLE T_KONV INTO SL_KONV WITH KEY KNUMV = SL_VFKP-KNUMV  KPOSN = SL_VFSI-KPOSN KSCHL = C_FRE          BINARY SEARCH.
*
** Analitico
*********************************************************************************************************************************
** Valor unitario
*      SL_ALV-KBETR           = SL_KONV-KBETR.
*
** Total do frete
*      SL_ALV-NETWR           = SL_VFSI-KZWI1.
*
** Total de impostos
*      SL_ALV-MWSBP           = SL_VFSI-KZWI6.
*********************************************************************************************************************************
*
** Cambio - Dolar
** Documento de custo de frete
*      "      CONCATENATE sl_vfkp-prsdt+6(2) '.' sl_vfkp-prsdt+4(2) '.' sl_vfkp-prsdt(4) INTO sl_data.
*      CONCATENATE SL_VTTK-ERDAT+6(2) '.' SL_VTTK-ERDAT+4(2) '.' SL_VTTK-ERDAT(4) INTO SL_DATA.
*
*      IF NOT SL_DATA = '00.00.0000'.
*
*        CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
*          EXPORTING
*            INPUT  = SL_DATA
*          IMPORTING
*            OUTPUT = P_GDATU.
*
** Cambio / Valor unitario e total em dolar.
*        READ TABLE T_TCURR INTO SL_TCURR WITH KEY GDATU = P_GDATU.
*        IF SY-SUBRC IS INITIAL.
*          SL_ALV-UKURS           = SL_TCURR-UKURS.
*          SL_ALV-KBETR1          = SL_KONV-KBETR / SL_TCURR-UKURS.
*          SL_ALV-NETWR1          = SL_ALV-NETWR / SL_TCURR-UKURS.
*        ELSE.
*          READ TABLE T_TCURR INTO SL_TCURR INDEX 1.
*          IF SY-SUBRC IS INITIAL AND SL_TCURR-UKURS <> 0.
*            SL_ALV-UKURS           = SL_TCURR-UKURS.
*            SL_ALV-KBETR1          = SL_KONV-KBETR / SL_TCURR-UKURS.
*            SL_ALV-NETWR1          = SL_ALV-NETWR / SL_TCURR-UKURS.
*          ELSE.
*            SL_ALV-UKURS           = 0.
*            SL_ALV-KBETR1          = 0.
*            SL_ALV-NETWR1          = 0.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
** IOF
*      READ TABLE T_VFKP INTO SL_VFKP WITH KEY REBEL = SL_VTTK-TKNUM                                               BINARY SEARCH.
*      READ TABLE T_VFSI INTO SL_VFSI WITH KEY KNUMV = SL_VFKP-KNUMV  VBELN = SL_LIPS-VBELN  POSNR = SL_LIPS-POSNR BINARY SEARCH.
*      READ TABLE T_KONV INTO SL_KONV WITH KEY KNUMV = SL_VFKP-KNUMV  KPOSN = SL_VFSI-KPOSN KSCHL = C_IOF          BINARY SEARCH.
*
*      IF SY-SUBRC IS INITIAL.
*        SL_ALV-ZIOF           = SL_KONV-KWERT.
*      ENDIF.
*
** Seguro
*      READ TABLE T_VFKP INTO SL_VFKP WITH KEY REBEL = SL_VTTK-TKNUM                                               BINARY SEARCH.
*      READ TABLE T_VFSI INTO SL_VFSI WITH KEY KNUMV = SL_VFKP-KNUMV  VBELN = SL_LIPS-VBELN  POSNR = SL_LIPS-POSNR BINARY SEARCH.
*      READ TABLE T_KONV INTO SL_KONV WITH KEY KNUMV = SL_VFKP-KNUMV  KPOSN = SL_VFSI-KPOSN  KSCHL = C_SEG         BINARY SEARCH.
*      IF SY-SUBRC IS INITIAL.
*        SL_ALV-ZSEG  = SL_KONV-KWERT.
*      ENDIF.
*
** Pegagio
*      READ TABLE T_VFKP INTO SL_VFKP WITH KEY REBEL = SL_VTTK-TKNUM                                               BINARY SEARCH.
*      READ TABLE T_VFSI INTO SL_VFSI WITH KEY KNUMV = SL_VFKP-KNUMV  VBELN = SL_LIPS-VBELN  POSNR = SL_LIPS-POSNR BINARY SEARCH.
*      READ TABLE T_KONV INTO SL_KONV WITH KEY KNUMV = SL_VFKP-KNUMV  KPOSN = SL_VFSI-KPOSN  KSCHL = C_PED         BINARY SEARCH.
*      IF SY-SUBRC IS INITIAL.
*        SL_ALV-ZPED  = SL_KONV-KWERT.
*      ENDIF.
*
*      IF SL_LIPS-BRGEW GT 0.
*        SL_ALV-FRETE_TON = ( ( SL_VFSI-KZWI1 + SL_KONV-KWERT ) / SL_LIPS-BRGEW ) * 1000.
*      ELSE.
*        SL_ALV-FRETE_TON = 0.
*      ENDIF.
*
** Local de organização de transporte
*      READ TABLE T_TTDST INTO SL_TTDST WITH KEY TPLST = SL_VTTK-TPLST  BINARY SEARCH.
*      SL_ALV-BEZEI           = SL_TTDST-BEZEI.
*
** Documento de transporte
*      SL_ALV-TKNUM           = SL_VTTK-TKNUM.
*
** Documento de custo
*      SL_ALV-FKNUM           = SL_VFKP-FKNUM.
*
** Pedido de serviço de frete
*      SL_ALV-EBELN           = SL_VFKP-EBELN.
*
** Folha de registros de serviço de frete
*      SL_ALV-LBLNI           = SL_VFKP-LBLNI.
*
** Ordem de vendas
*      READ TABLE T_VBFA_VA INTO SL_VBFA_VA WITH KEY VBELN = SL_LIPS-VBELN BINARY SEARCH.
*      SL_ALV-VBELN_VA          = SL_VBFA_VA-VBELV.
*
** Remessas
*      SL_ALV-VBELN           = SL_LIPS-VBELN.
*
** Faturamento
*      READ TABLE T_VBFA_VF INTO SL_VBFA_VF WITH KEY VBELV = SL_LIKP-VBELN BINARY SEARCH.
*      SL_ALV-VBELN_VF           = SL_VBFA_VF-VBELN.
*
** Nota fiscal
*      VL_REFKEY =  SL_VBFA_VF-VBELN.
*      READ TABLE T_LIN INTO SL_LIN WITH KEY REFKEY = VL_REFKEY BINARY SEARCH.
*      READ TABLE T_DOC INTO SL_DOC WITH KEY DOCNUM = SL_LIN-DOCNUM  BINARY SEARCH.
** Nota fiscal-série
*
*
*      CALL FUNCTION 'Z_1B_NF_VALUE_DETERMINATION'
*        EXPORTING
*          P_DOCNUM   = SL_DOC-DOCNUM
*        IMPORTING
*          EXT_HEADER = WA_J_CABE.
*
*      SL_ALV-VLR_NF = WA_J_CABE-NFTOT.
*
*      SL_ALV-VLR_NF   = SL_LIN-NETPR * SL_LIN-MENGE.
*      SL_ALV-NFENUM   = SL_DOC-NFENUM.
*      SL_ALV-SERIES   = SL_DOC-SERIES.
*
*      APPEND SL_ALV TO T_ALV.
*
*    ENDLOOP.
*
*  ENDLOOP.

ENDFORM.                    " Z_AGRUPA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_ESTRUTURAS_ALV                                         *
*&---------------------------------------------------------------------*
*                              Estruturas ALV                          *
*----------------------------------------------------------------------*
FORM z_estruturas_alv.

  REFRESH t_fcat.

* Monta Layout
  PERFORM z_layout.

* Preenche FieldCat
  PERFORM z_preenche_fieldcat USING:
* Safra
    'CHARG'           TEXT-065 5,
* Cliente
    'NAME1_KN'        TEXT-018 20,
    'STCD1_KN'        TEXT-019 16,
    'STCD3_KN'        TEXT-020 18,

* Origem
    'CITY1_AD'        TEXT-016 20,
    'REGION_AD'       TEXT-017 09,

* Destino
    'ORT01_KN'        TEXT-021 20,
    'REGIO_KN'        TEXT-022 10,
    'NAME1_LR'        TEXT-050 35,

* Transportador
    'NAME1_LF'        TEXT-025 20,

* Conhecimento e Carta frete
    'EXTI1'           TEXT-026 20,
    'EXTI2'           TEXT-027 20,

*Destinatário
    'CD_TERMINAL'     TEXT-061 20,
    'DESC_TERMINAL'   TEXT-062 20,
    'END_TERMINAL'    TEXT-067 9,
    'CID_TERMINAL'    TEXT-068 8,


*Carregamento
    'CD_CARREG'       TEXT-063 20,
    'DESC_CARREG'     TEXT-064 20,

* Placa do veiculo / Motorista / Prorpietário
    'TEXT1'           TEXT-029 35,
    'STCD_PV'         TEXT-072 15,
    'NAME1_PV'        TEXT-031 35,
    'STCD_MT'         TEXT-071 15,
    'NAME1_MT'        TEXT-030 35,


* Itinerario
    'ROUTE'           TEXT-032 10.

  PERFORM z_preenche_fieldcat USING:
    'MATNR'           TEXT-045 19,
    'MAKTX'           TEXT-046 40,
    'WERKS'           TEXT-057 40 .

* Preenche FieldCat
  PERFORM z_preenche_fieldcat USING:

* Peso
  'BRGEW'             TEXT-012 19,
  'PESO_CONFIRMADO'   TEXT-013 19,
  'DIFERENCA'         TEXT-014 19,
  'DATBG'             TEXT-033 16,
  'UATEN'             TEXT-073 16,
  'DTA_CHEGADA'       TEXT-015 16,

* Valor unitario do frete
  'KBETR'             TEXT-041 20,

* Valor do frete / Qtde
  'NETWR'             TEXT-035 20,

* Frete + Pedagio / Peso Saida * 1000
  'FRETE_TON'         TEXT-058 12,

* Valor de impostos
  'MWSBP'             TEXT-036 20,

*  Valor do IOF
  'ZIOF'              TEXT-069 20,

* Cambio
  'UKURS'             TEXT-051 32,

* Valor unitario do frete / USD
  'KBETR1'            TEXT-056 22,

* Valor do frete / USD
  'NETWR1'             TEXT-052 27,

* Vlr Seguro
  'ZSEG'              TEXT-053 15,

* Vlr Pedagio
  'ZPED'              TEXT-054 15,

* Per.Seguro
  'KBETR_PER'              TEXT-078 22,

* Local de organização de transporte
  'BEZEI'             TEXT-023 20.

* Fluxo de documentos

* rb_out  TYPE char1 AS CHECKBOX DEFAULT 'X',
*
*  rb_tra  TYPE char1 AS CHECKBOX ,
*
*  rb_in

  PERFORM z_preenche_fieldcat USING:
* Documento de transporte
      'TKNUM'           TEXT-024 10,

* Documento de transporte
      'SHTYP'           TEXT-055 20,

* documento de custo de frete
      'FKNUM'           TEXT-034 10,

* Pedido de serviço de frete
      'EBELN'           TEXT-037 14,

* Folha de registro de serviço
      'LBLNI'           TEXT-038 22,

* Folha de registro de serviço
      'IDENTI'           TEXT-070 35.

  IF rb_tra = c_x OR rb_in = c_x.

* Preenche FieldCat
    PERFORM z_preenche_fieldcat USING:

* Pedido de compra do produto
    'EBELN1'          TEXT-047 20,

* Documento de material
    'MBLNR'           TEXT-048 20.

  ENDIF.

  IF rb_out = c_x.

* Preenche FieldCat
    PERFORM z_preenche_fieldcat USING:


* Ordem de vendas
    'VBELN_VA'        TEXT-007 17,

    'INSTRUCAO'       TEXT-077 20,

* Documento de faturamento
    'VBELN'           TEXT-008 10,

* Documento de faturamento
    'VBELN_VF'        TEXT-009 12,

* Documento de faturamento
    'KVGR3'           TEXT-075 10,

* Valor de triagem
    'ZVCT'           TEXT-076 10.
  ENDIF.

* Nota fiscal e serie
  PERFORM z_preenche_fieldcat USING:
  'NFENUM'          TEXT-010 10,
  'SERIES'          TEXT-011 05,
  'VLR_NF'          TEXT-066 09.

* Instruções de Embarque de Algodão
  PERFORM z_preenche_fieldcat USING:
  'DATA_IN_PORTO'   TEXT-079 10,
  'DATA_PORTO'      TEXT-080 10,
  'XSTATUS'         TEXT-081 20,
  'OBSERVACAO'      TEXT-082 20.


* Dados gerais vinculado ao documento de transporte.
  PERFORM z_preenche_fieldcat USING:
  'TP_TRANSP   '   TEXT-083 15,
  'EQUIP_TAC   '   TEXT-084 15,
  'DATA_TP     '   TEXT-085 15,
  'HORA_TP     '   TEXT-086 15,
  'BASE_TIPBANK'   TEXT-087 15,
  'GRUPO       '   TEXT-088 15,
  'FROTA       '   TEXT-089 15.

* 133799 CS2024000094 Inclusão de campos para atendimento das operações de algodão - PSA
  PERFORM z_preenche_fieldcat USING:
  'NM_BLOCO   '   TEXT-090 15, "Nr.Lote = ZSDT0001FD-NM_BLOCO
  'QT_FARDOS  '   TEXT-091 15. "Qtd.Fardos = ZSDT0001FD-QT_FARDOS




ENDFORM.                    " Z_ESTRUTURAS_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM z_layout.

  CLEAR s_layout.

  s_layout-zebra = 'X'.
  s_layout-cwidth_opt = 'X'.

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
    MESSAGE i836 WITH TEXT-013.
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
    MESSAGE i836 WITH TEXT-014.
  ENDIF.

* Set event handler
  SET HANDLER:
    lcl_eventhandler=>handle_button_click FOR s_alv.


ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  DATA:
  ls_fcat        TYPE lvc_s_fcat.

  DATA vl_int TYPE int4.
  s_variant-report = sy-repid.
  s_variant-variant = '/STANDARD'.

  LOOP AT t_fcat INTO ls_fcat
        WHERE ( fieldname = 'OBSERVACAO' ).
    ls_fcat-style   = cl_gui_alv_grid=>mc_style_button.
    ls_fcat-icon    = 'X'.
    MODIFY t_fcat FROM ls_fcat.
  ENDLOOP.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
      i_save                        = 'A'
      i_default                     = 'X'
      is_layout                     = s_layout
      is_variant                    = s_variant
    CHANGING
      it_outtab                     = t_alv
      it_fieldcatalog               = t_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

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
* Seleciona Itens do Transporte
           z_seleciona_vttp,
* Seleciona dados de chegada
           z_seleciona_zlest0039,
* Seleciona Remessas
           z_seleciona_likp,
*Seleciona parceiros da remessa
           z_seleciona_vbpa,
*Seleciona parceiros
           z_seleciona_lfa13,
* Seleciona Itens Remessa
           z_seleciona_lips,
* Seleciona Centro
           z_seleciona_werks,
* Seleciona Documento Material
           z_seleciona_ekbe,
* Seleciona Itens Remessa - material
           z_seleciona_makt,
* Seleciona Itens Nota Fiscal
           z_seleciona_lin2,
* Seleciona Documentos NF
*           z_seleciona_doc2,
* Seleciona Itens Custo de Frete
           z_seleciona_vfkp,
* Seleciona Dados Parceiros
           z_seleciona_lfa12,
* Seleciona Dados Lançamentos
           z_seleciona_lanc,
* Seleciona Dados Org. Transporte
           z_seleciona_ttds,
* Seleciona Dados de Endereço
           z_seleciona_adrc,
* Seleciona Parceiros Transporte
           z_seleciona_vtpa,
* Seleciona Dados Cliente
           z_seleciona_kna1,
* Seleciona Dados Parceiros
           z_seleciona_lfa1,
* Seleciona Dados Custos de frete
           z_seleciona_vfsi,
* Seleciona Condição ZFRE
           z_seleciona_konv,
* Seleciona Dados Itinerários
          z_seleciona_tvro,
* Seleciona Moeda
          z_seleciona_moeda.

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

  DATA: t_lips_aux TYPE TABLE OF lips.

  CLEAR: it_zmmt_ee_zgr_docs[].

  CHECK NOT t_lips[] IS INITIAL.

  MOVE t_lips[] TO t_lips_aux[].
  SORT t_lips_aux BY vbeln.
  DELETE ADJACENT DUPLICATES FROM t_lips_aux COMPARING vbeln.

  SELECT * INTO TABLE it_zmmt_ee_zgr_docs
    FROM zmmt_ee_zgr_docs
     FOR ALL ENTRIES IN t_lips_aux
   WHERE av_vbeln EQ t_lips_aux-vbeln.

  DELETE it_zmmt_ee_zgr_docs WHERE docnum EQ space.
  CHECK NOT it_zmmt_ee_zgr_docs[] IS INITIAL.

  SELECT *
    FROM j_1bnflin
    INTO TABLE t_lin
    FOR ALL ENTRIES IN it_zmmt_ee_zgr_docs
  WHERE docnum EQ it_zmmt_ee_zgr_docs-docnum.

  SELECT *
    FROM j_1bnfdoc
    INTO TABLE t_doc
    FOR ALL ENTRIES IN it_zmmt_ee_zgr_docs
  WHERE  docnum EQ it_zmmt_ee_zgr_docs-docnum.

  SORT t_lin BY refkey ASCENDING
                refitm ASCENDING.

  SORT t_doc BY docnum ASCENDING.

ENDFORM.                    " Z_SELECIONA_LIN2

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
*&      Form  Z_SELECIONA_LFA12                                        *
*&---------------------------------------------------------------------*
*                       Seleciona Dados Parceiros                      *
*----------------------------------------------------------------------*
FORM z_seleciona_lfa13.

  DATA tl_likp TYPE TABLE OF likp.

  CHECK NOT t_vbpa[] IS INITIAL.

  SORT t_vbpa BY lifnr ASCENDING.

  SELECT *
    FROM lfa1
    INTO TABLE t_lfa1_3
    FOR ALL ENTRIES IN t_vbpa
  WHERE  lifnr EQ t_vbpa-lifnr.

  SORT t_lfa1_3 BY lifnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_LFA12

*&---------------------------------------------------------------------*
*&      Form  Z_AGRUPA_DADOS_2                                         *
*&---------------------------------------------------------------------*
*                               Agrupa Dados                           *
*----------------------------------------------------------------------*
FORM z_agrupa_dados_2.

  DATA: text_cliente_cgc TYPE c LENGTH 18,
        text_cliente_cpf TYPE c LENGTH 14.

  DATA: sl_vttk    TYPE vttk,
        sl_vttp    TYPE vttp,
        sl_doc     TYPE j_1bnfdoc,
        sl_lin     TYPE j_1bnflin,
        sl_lips    TYPE lips,
        sl_likp    TYPE likp,
        sl_t0016   TYPE zlest0016,
        sl_adrc    TYPE adrc,
        sl_kna1    TYPE kna1,
        sl_ttdst   TYPE ttdst,
        sl_lfa1    TYPE lfa1,
        sl_lfa1_2  TYPE lfa1,
        sl_tvro    TYPE tvro,
        sl_vfkp    TYPE vfkp,
        sl_vfsi    TYPE vfsi,
        sl_ttds    TYPE ttds,
        sl_vtpa    TYPE vtpa,
        sl_vtpa_mt TYPE vtpa,
        sl_alv     TYPE type_alv,
        sl_ekbe    TYPE ekbe,
        sl_konv    TYPE konv,
        vl_dias    TYPE i.

  SORT t_t001w  BY werks ASCENDING..

  "  REFRESH t_alv.

  LOOP AT t_vttk INTO sl_vttk.

    IF ( NOT p_placa IS INITIAL ) AND ( sl_vttk-text1(7) NE p_placa ).
      CONTINUE.
    ENDIF.

    LOOP AT t_vttp INTO sl_vttp
      WHERE tknum EQ sl_vttk-tknum.

      READ TABLE t_vfkp INTO sl_vfkp
        WITH KEY rebel = sl_vttk-tknum
        BINARY SEARCH.

      READ TABLE t_likp INTO sl_likp
        WITH KEY vbeln = sl_vttp-vbeln
        BINARY SEARCH.


* Terminal
      " sort t_vbpa by vbeln parvw  ascending.
      READ TABLE t_vbpa  INTO sl_vbpa
        WITH KEY vbeln = sl_likp-vbeln  parvw = 'Z1'
        BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        READ TABLE t_lfa1_3 INTO sl_lfa1_3
          WITH KEY lifnr = sl_vbpa-lifnr
          BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          sl_alv-cd_terminal   = sl_lfa1_3-lifnr.
          sl_alv-desc_terminal = sl_lfa1_3-name1.
          sl_alv-end_terminal  = sl_lfa1_3-stras.
          sl_alv-cid_terminal  = sl_lfa1_3-ort01.
        ENDIF.
      ENDIF.

      READ TABLE t_lips INTO sl_lips
        WITH KEY vbeln = sl_likp-vbeln
        BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        READ TABLE it_zmmt_ee_zgr_docs
          WITH KEY av_vbeln = sl_likp-vbeln.
        IF sy-subrc IS INITIAL.
          sl_alv-vbeln_va  = it_zmmt_ee_zgr_docs-po_number.

          READ TABLE t_lin INTO sl_lin WITH KEY docnum = it_zmmt_ee_zgr_docs-docnum BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            READ TABLE t_doc INTO sl_doc WITH KEY docnum = it_zmmt_ee_zgr_docs-docnum BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              CALL FUNCTION 'Z_1B_NF_VALUE_DETERMINATION'
                EXPORTING
                  p_docnum   = sl_doc-docnum
                IMPORTING
                  ext_header = wa_j_cabe.

              IF NOT sl_doc-nfe IS INITIAL.
                sl_alv-nfenum = sl_doc-nfenum.
              ELSE.
                MOVE sl_doc-nfnum TO sl_alv-nfenum.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = sl_alv-nfenum
                  IMPORTING
                    output = sl_alv-nfenum.
              ENDIF.
              sl_alv-series = sl_doc-series.
              sl_alv-vlr_nf = wa_j_cabe-nftot.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE t_lfa1_2 INTO sl_lfa1_2 WITH KEY lifnr = sl_likp-lifnr BINARY SEARCH.
      sl_alv-name1_kn = sl_lfa1_2-name1.
      sl_alv-stcd3_kn = sl_lfa1_2-stcd3.

      IF sl_vttk-exti1 IS NOT INITIAL.
        READ TABLE t_t0016 INTO sl_t0016 WITH KEY conhecimento = sl_vttk-exti1 BINARY SEARCH.
        IF sy-subrc = 0.
          CLEAR  sl_t0016.
        ENDIF.
      ELSE.
        CLEAR sl_t0016.
      ENDIF.

      READ TABLE t_ttds INTO sl_ttds WITH KEY tplst = sl_vttk-tplst BINARY SEARCH.

      READ TABLE t_ttdst INTO sl_ttdst WITH KEY tplst = sl_ttds-tplst BINARY SEARCH.

      READ TABLE t_adrc INTO sl_adrc WITH KEY addrnumber = sl_ttds-adrnr BINARY SEARCH.

* Material - Descrição
      READ TABLE t_makt INTO sl_makt WITH KEY matnr = sl_lips-matnr BINARY SEARCH.

*Transportador
      READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vttk-tdlnr BINARY SEARCH.
      sl_alv-name1_lf  = sl_lfa1-name1.

* Proprietário
      READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'PV'  BINARY SEARCH.
      READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtpa-lifnr                BINARY SEARCH.
      sl_alv-name1_pv        = sl_lfa1-name1.
      IF sl_lfa1-stkzn = 'X'.
        sl_alv-stcd_pv = sl_lfa1-stcd2 .
      ELSE.
        sl_alv-stcd_pv = sl_lfa1-stcd1 .
      ENDIF.

      IF ( NOT p_propv IS INITIAL ) AND ( p_propv <> sl_vtpa-lifnr ).
        CONTINUE.
      ENDIF.

* Motorista
      READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'MT'  BINARY SEARCH.
      READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtpa-lifnr                BINARY SEARCH.
      sl_alv-name1_mt        = sl_lfa1-name1.
      IF sl_lfa1-stkzn = 'X'.
        sl_alv-stcd_mt = sl_lfa1-stcd2 .
      ELSE.
        sl_alv-stcd_mt = sl_lfa1-stcd1 .
      ENDIF.

* Local de entrega
      READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'LR'  BINARY SEARCH.
      READ TABLE t_kna1 INTO sl_kna1 WITH KEY kunnr = sl_vtpa-kunnr                BINARY SEARCH.
      sl_alv-name1_lr      = sl_kna1-name1.
      sl_alv-cid_entrega   = sl_kna1-ort01.
      sl_alv-reg_entrega   = sl_kna1-regio.

      sl_alv-ort01_kn      = sl_kna1-ort01.
      sl_alv-regio_kn      = sl_kna1-regio.

*Local de carregamento
      READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'PC'  BINARY SEARCH.
      READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtpa-lifnr                BINARY SEARCH.
      sl_alv-cd_carreg     = sl_lfa1-lifnr.
      sl_alv-desc_carreg   = sl_lfa1-name1.
      sl_alv-city1_ad      = sl_lfa1-ort01.
      sl_alv-region_ad     = sl_lfa1-regio.

* Pegagio
      READ TABLE t_vfkp INTO sl_vfkp WITH KEY rebel = sl_vttk-tknum                                               BINARY SEARCH.
      READ TABLE t_vfsi INTO sl_vfsi WITH KEY knumv = sl_vfkp-knumv  vbeln = sl_lips-vbeln  posnr = sl_lips-posnr BINARY SEARCH.
      READ TABLE t_konv INTO sl_konv WITH KEY knumv = sl_vfkp-knumv  kposn = sl_vfsi-kposn  kschl = c_ped         BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        sl_alv-zped           = sl_konv-kwert.

      ENDIF.

* Terminal
      "sort t_vbpa by vbeln parvw  ascending.
      READ TABLE t_vbpa  INTO sl_vbpa  WITH KEY vbeln = sl_likp-vbeln  parvw = 'Z1'  BINARY SEARCH.
      READ TABLE t_lfa1_3 INTO sl_lfa1_3 WITH KEY lifnr = sl_vbpa-lifnr              BINARY SEARCH.

      sl_alv-cd_terminal   = sl_lfa1_3-lifnr.
      sl_alv-desc_terminal = sl_lfa1_3-name1.
      sl_alv-end_terminal  = sl_lfa1_3-stras.
      sl_alv-cid_terminal  = sl_lfa1_3-ort01.

      READ TABLE t_vfsi INTO sl_vfsi
        WITH KEY vbeln = sl_vttp-vbeln
        BINARY SEARCH.

      READ TABLE t_konv INTO sl_konv
       WITH KEY knumv = sl_vfsi-knumv
                kposn = sl_vfsi-kposn
       BINARY SEARCH.

      READ TABLE t_t001w INTO sl_t001w
       WITH KEY werks = sl_lips-werks.
      sl_alv-werks = sl_t001w-name1.

      IF sl_t0016 IS INITIAL.
        " ALRS 07.10.13
*        READ TABLE T_TVRO INTO SL_TVRO
*          WITH KEY ROUTE = SL_VTTK-ROUTE
*          BINARY SEARCH.
*
*        IF SY-SUBRC IS INITIAL.
**         Calcula Dias
*          PERFORM Z_CALCULA_DIAS USING SL_TVRO-TRAZTD
*                              CHANGING VL_DIAS.
*          SL_ALV-PESO_CONFIRMADO = SL_LIPS-BRGEW.
*          SL_ALV-DIFERENCA       = 0.
*          SL_ALV-DTA_CHEGADA     = SL_VTTK-DATBG + VL_DIAS.
*        ELSE.
*          SL_ALV-PESO_CONFIRMADO = SL_LIPS-BRGEW.
*          SL_ALV-DIFERENCA       = 0.
*          SL_ALV-DTA_CHEGADA     = SL_VTTK-DATBG + 1.
*        ENDIF.
        sl_alv-peso_confirmado = sl_likp-ntgew.
        sl_alv-diferenca       = 0.
        sl_alv-dta_chegada     = sl_likp-erdat.
      ELSE.
        sl_alv-peso_confirmado = sl_t0016-peso_confirmado.
        sl_alv-diferenca       = sl_lips-brgew - sl_t0016-peso_confirmado.
        sl_alv-dta_chegada     = sl_t0016-dta_chegada.
      ENDIF.

* Tipo de documento de transporte
      sl_alv-shtyp           = sl_vttk-shtyp.

      sl_alv-vbeln           = sl_vttp-vbeln.
      sl_alv-brgew           = sl_lips-brgew.

      IF sl_lfa1_2-stkzn IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = sl_lfa1_2-stcd1
          IMPORTING
            output = text_cliente_cgc.
        sl_alv-stcd1_kn = text_cliente_cgc.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            input  = sl_lfa1_2-stcd2
          IMPORTING
            output = text_cliente_cpf.
        sl_alv-stcd1_kn = text_cliente_cpf.
      ENDIF.

      sl_alv-matnr     = sl_lips-matnr.
      sl_alv-maktx     = sl_makt-maktx.

      sl_alv-bezei     = sl_ttdst-bezei.
      sl_alv-tknum     = sl_vttk-tknum.
      sl_alv-uaten     = sl_vttk-uatbg.
      sl_alv-exti1     = sl_vttk-exti1.
      sl_alv-exti2     = sl_vttk-exti2.
      sl_alv-signi     = sl_vttk-signi.
      sl_alv-text1     = sl_vttk-text1.
      sl_alv-route     = sl_tvro-route.
      sl_alv-datbg     = sl_vttk-datbg.
      sl_alv-fknum     = sl_vfkp-fknum.
*** US #182165 - MMSILVA - 20.06.2025 - Ini ***
*      sl_alv-netwr     = sl_vfsi-netwr.
      sl_alv-netwr     = sl_vfsi-kzwi1.
*** US #182165 - MMSILVA - 20.06.2025 - Fim ***
      sl_alv-mwsbp     = sl_vfsi-mwsbp.
      sl_alv-ebeln     = sl_vfkp-ebeln.
      sl_alv-lblni     = sl_vfkp-lblni.
      sl_alv-kbetr     = sl_konv-kbetr.
      sl_alv-charg     = sl_lips-charg.

      APPEND sl_alv TO t_alv.

      CLEAR: sl_vttp   ,
             sl_ekbe   ,
             sl_doc    ,
             sl_lin    ,
             sl_lips   ,
             sl_likp   ,
             sl_t0016  ,
             sl_adrc   ,
             sl_lfa1_2 ,
             sl_ttdst  ,
             sl_lfa1   ,
             sl_tvro   ,
             sl_vfkp   ,
             sl_vfsi   ,
             sl_ttds   ,
             sl_vtpa   ,
             vl_refkey ,
             sl_konv   ,
             sl_alv    ,
             sl_vbpa   ,
             sl_lfa1_3 ,
             sl_t001w  .

    ENDLOOP.

    CLEAR sl_vttk.

  ENDLOOP.

ENDFORM.                    " Z_AGRUPA_DADOS_2

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VFSI                                         *
*&---------------------------------------------------------------------*
*                  Seleciona Dados Custos de frete                     *
*----------------------------------------------------------------------*
FORM z_seleciona_vfsi.

  DATA tl_vfkp TYPE TABLE OF vfkp.

  REFRESH t_vfsi.
  CHECK NOT t_vfkp[] IS INITIAL.

  tl_vfkp[] = t_vfkp[].
  SORT tl_vfkp BY knumv ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vfkp COMPARING knumv.

  SELECT *
   FROM vfsi
   INTO TABLE t_vfsi
   FOR ALL ENTRIES IN tl_vfkp
  WHERE knumv = tl_vfkp-knumv.

  SORT t_vfsi BY knumv ASCENDING
                 vbeln ASCENDING
                 posnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_VFSI

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_KONV                                         *
*&---------------------------------------------------------------------*
*                        Seleciona Condição ZFRE                       *
*----------------------------------------------------------------------*
FORM z_seleciona_konv.

  DATA tl_vfkp TYPE TABLE OF vfkp.

  REFRESH t_konv.
  CHECK NOT t_vfkp[] IS INITIAL.

  tl_vfkp[] = t_vfkp[].
  SORT tl_vfkp BY knumv ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vfkp COMPARING knumv.

*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
*  SELECT *
*   FROM konv
*   INTO TABLE t_konv
*   FOR ALL ENTRIES IN tl_vfkp
*  WHERE knumv EQ tl_vfkp-knumv
*    AND kschl IN ('ZFRE', 'ZPED', 'ZSEG', 'ZIOF', 'ZVCT' ).

  SELECT FROM v_konv FIELDS * FOR ALL ENTRIES IN @tl_vfkp WHERE knumv EQ @tl_vfkp-knumv AND kschl IN ( 'ZFRE' , 'ZPED' , 'ZSEG' , 'ZIOF' , 'ZVCT' ) INTO CORRESPONDING FIELDS OF TABLE @t_konv .
*-Equalização RISE x PRD - 19.07.2023 - JT - fim

  SORT t_konv BY knumv ASCENDING
                 kposn ASCENDING
                 kschl ASCENDING.

ENDFORM.                    " Z_SELECIONA_KONV

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_TVRO                                         *
*&---------------------------------------------------------------------*
*                       Seleciona Dados Itinerários                    *
*----------------------------------------------------------------------*
FORM z_seleciona_tvro.

  DATA tl_vttk TYPE TABLE OF vttk.

  REFRESH t_tvro.
  CHECK NOT t_vttk[] IS INITIAL.
  tl_vttk[] = t_vttk[].
  SORT tl_vttk BY route ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vttk COMPARING route.

  SELECT *
    FROM tvro
    INTO TABLE t_tvro
    FOR ALL ENTRIES IN tl_vttk
  WHERE  route EQ tl_vttk-route.

ENDFORM.                    " Z_SELECIONA_TVRO

*&---------------------------------------------------------------------*
*&      Form  Z_CALCULA_DIAS                                           *
*&---------------------------------------------------------------------*
*                             Calcula Dias                             *
*----------------------------------------------------------------------*
FORM z_calcula_dias USING p_traztd TYPE traztd
                 CHANGING p_dias   TYPE i.

  DATA: vl_traztd TYPE char20,
        vl_dias   TYPE i.

  CLEAR p_dias.

  CALL FUNCTION 'CONVERSION_EXIT_TSTRG_OUTPUT'
    EXPORTING
      input  = p_traztd
    IMPORTING
      output = vl_traztd.

  REPLACE ALL OCCURRENCES OF ',' IN vl_traztd WITH '.'.
  CONDENSE vl_traztd.

  IF vl_traztd LE 24.
    p_dias = 1.
    EXIT.
  ENDIF.

  p_dias  = vl_traztd / 24.
  vl_dias = vl_traztd MOD 24.

  IF NOT vl_dias IS INITIAL.
    ADD 1 TO p_dias.
  ENDIF.

ENDFORM.                    " Z_CALCULA_DIAS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VTTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_vtts .

  DATA: sl_vtts LIKE vtts,
        sl_vttk TYPE vttk.

  REFRESH :  t_vtts, ti_tvst, t_adrc, ti_tvkn.

  CHECK NOT t_vttk[] IS INITIAL.

  SELECT *
  FROM vtts
  INTO TABLE t_vtts
  FOR ALL ENTRIES IN t_vttk
  WHERE  tknum EQ t_vttk-tknum AND
         knota    IN s_knota   AND  " Ponto de partida
         vstel    IN s_vstel   AND  " loc.expedição
         lstel    IN s_lstel   AND  " ponto de carregamento no local de expedição
         werka    IN s_werka   AND  " centro
         lgorta   IN s_lgorta  AND  " depósito no centro
         kunna    IN s_kunna   AND  " cliente
         lifna    IN s_lifna   AND  " fornecedor
         knotz    IN s_knotz   AND  " entroncamento
         vstez    IN s_vstez   AND  " local de expedição
         lstez    IN s_lstez   AND  " ponto carregamento no local de expedição
         werkz    IN s_werkz   AND  " centro
         lgortz   IN s_lgortz  AND  " depósito no centro
         kunnz    IN s_kunnz   AND  " cliente
         lifnz    IN s_lifnz.       " fornecedor


  SELECT *
FROM vtts
INTO TABLE t_vtts_line4
FOR ALL ENTRIES IN t_vtts
WHERE  tknum EQ  t_vtts-tknum AND
       knota    IN s_knota   AND  " Ponto de partida
       laufk    EQ  '4'      AND
       lstel    IN s_lstel   AND  " ponto de carregamento no local de expedição
       werka    IN s_werka   AND  " centro
       lgorta   IN s_lgorta  AND  " depósito no centro
       kunna    IN s_kunna   AND  " cliente
       lifna    IN s_lifna   AND  " fornecedor
       knotz    IN s_knotz   AND  " entroncamento
       vstez    IN s_vstez   AND  " local de expedição
       lstez    IN s_lstez   AND  " ponto carregamento no local de expedição
       werkz    IN s_werkz   AND  " centro
       lgortz   IN s_lgortz  AND  " depósito no centro
       kunnz    IN s_kunnz   AND  " cliente
       lifnz    IN s_lifnz.       " fornecedor


  SELECT *
FROM vtts
INTO TABLE t_vtts_line3
FOR ALL ENTRIES IN t_vtts
WHERE  tknum EQ  t_vtts-tknum AND
     knota    IN s_knota   AND  " Ponto de partida
     laufk    EQ  '3'      AND
     lstel    IN s_lstel   AND  " ponto de carregamento no local de expedição
     werka    IN s_werka   AND  " centro
     lgorta   IN s_lgorta  AND  " depósito no centro
     kunna    IN s_kunna   AND  " cliente
     lifna    IN s_lifna   AND  " fornecedor
     knotz    IN s_knotz   AND  " entroncamento
     vstez    IN s_vstez   AND  " local de expedição
     lstez    IN s_lstez   AND  " ponto carregamento no local de expedição
     werkz    IN s_werkz   AND  " centro
     lgortz   IN s_lgortz  AND  " depósito no centro
     kunnz    IN s_kunnz   AND  " cliente
     lifnz    IN s_lifnz.       " fornecedor


  IF  t_vtts_line4[] IS INITIAL AND t_vtts[] IS INITIAL.
    MESSAGE i836 WITH TEXT-049.
    LEAVE LIST-PROCESSING.
  ENDIF.
  SORT t_vtts_line4 BY tknum  ASCENDING.
  SORT t_vtts_line3 BY tknum  ASCENDING.
  SORT t_vtts BY tknum ASCENDING
                 tsnum ASCENDING.

  LOOP AT t_vttk INTO sl_vttk.

    READ TABLE t_vtts INTO sl_vtts WITH KEY tknum = sl_vttk-tknum BINARY SEARCH.

    IF NOT sy-subrc IS INITIAL.
      DELETE t_vttk WHERE tknum = sl_vttk-tknum.
    ENDIF.

  ENDLOOP.

  IF t_vttk[] IS INITIAL.
    MESSAGE i836 WITH TEXT-005.
    LEAVE LIST-PROCESSING.
  ENDIF.

* Endereço de origem - Address

  SELECT *
  INTO TABLE ti_tvst
  FROM tvst
  FOR ALL ENTRIES IN t_vtts
  WHERE vstel = t_vtts-vstel.

  IF sy-subrc IS INITIAL.

* Endereço de origem - Address
    SELECT *
    APPENDING TABLE t_adrc
    FROM adrc
    FOR ALL ENTRIES IN ti_tvst
    WHERE addrnumber = ti_tvst-adrnr.

  ENDIF.

* Endereço de origem - Address
  SELECT *
  APPENDING TABLE ti_tvkn
  FROM tvkn
  FOR ALL ENTRIES IN t_vtts
  WHERE knote = t_vtts-knota.

  IF sy-subrc IS INITIAL.

* Endereço de origem - Address
    SELECT *
    APPENDING TABLE t_adrc
    FROM adrc
    FOR ALL ENTRIES IN ti_tvkn
    WHERE addrnumber = ti_tvkn-adrnr.

  ENDIF.

* Endereço de origem - Address
  SELECT *
  APPENDING TABLE ti_lfa1
  FROM lfa1
  FOR ALL ENTRIES IN t_vtts
  WHERE lifnr = t_vtts-lifna.

  IF sy-subrc IS INITIAL.

* Endereço de origem - Address
    SELECT *
    APPENDING TABLE t_adrc
    FROM adrc
    FOR ALL ENTRIES IN ti_lfa1
    WHERE addrnumber = ti_lfa1-adrnr.

  ENDIF.

  IF rb_tra = c_x.

    IF t_vtts[] IS NOT INITIAL.
* Endereço de destino - Adress
      SELECT *
      APPENDING TABLE t_adrc
      FROM adrc
      FOR ALL ENTRIES IN t_vtts
      WHERE addrnumber = t_vtts-adrnz.
    ENDIF.

  ENDIF.

  IF rb_out = c_x.

* Endereço de destino - Adress - Entroncamento
    IF t_vtts[] IS NOT INITIAL.
      SELECT *
      APPENDING TABLE ti_tvkn
      FROM tvkn
      FOR ALL ENTRIES IN t_vtts
      WHERE knote = t_vtts-knotz.
    ENDIF.

* Endereço de destino - Adress
    IF ti_tvkn[] IS NOT INITIAL.
      SELECT *
      APPENDING TABLE t_adrc
      FROM adrc
      FOR ALL ENTRIES IN ti_tvkn
      WHERE addrnumber = ti_tvkn-adrnr.
    ENDIF.

* Endereço de destino - Adress
    IF t_vtts[] IS NOT INITIAL.
      SELECT * APPENDING TABLE t_adrc
        FROM adrc
       FOR ALL ENTRIES IN t_vtts
      WHERE addrnumber = t_vtts-adrnz.
    ENDIF.

  ENDIF.

ENDFORM.                    " Z_SELECIONA_VTTS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MAKT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_makt .

  CHECK NOT t_lips[] IS INITIAL.

  SELECT *
  FROM makt
  INTO TABLE t_makt
  FOR ALL ENTRIES IN t_lips
  WHERE  matnr EQ t_lips-matnr AND
         spras EQ sy-langu.

  SORT t_makt BY matnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_MAKT


*&---------------------------------------------------------------------*
*&      Form  z_seleciona_vbpa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_vbpa .

  CHECK NOT t_likp[] IS INITIAL.

  SELECT *
  FROM vbpa
  INTO TABLE t_vbpa
  FOR ALL ENTRIES IN t_likp
  WHERE vbeln EQ t_likp-vbeln.

  SORT t_vbpa BY vbeln ASCENDING.

ENDFORM.                    " Z_SELECIONA_MAKT


*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_EKBE_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_ekbe_saida .

  DATA : vl_buzei TYPE ekbe-buzei,
         sl_lips  TYPE lips,
         vl_tabix TYPE sy-tabix,
         sl_ekbe  TYPE ekbe.

  CHECK NOT t_lips[] IS INITIAL.

  SELECT *
  FROM ekbe
  INTO TABLE t_ekbe
  FOR ALL ENTRIES IN t_lips
  WHERE  belnr EQ t_lips-vbeln  AND
         vgabe EQ '8'           AND
         bewtp EQ 'L'.


  SORT t_ekbe BY belnr ASCENDING
                 buzei ASCENDING.

  LOOP AT t_lips INTO sl_lips.

    vl_tabix = sy-tabix.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = sl_lips-posnr
      IMPORTING
        output = vl_buzei.

    CLEAR sl_ekbe.

    READ TABLE t_ekbe INTO sl_ekbe WITH KEY belnr = sl_lips-vbeln
                                            buzei = vl_buzei.

    IF NOT sy-subrc IS INITIAL.
      DELETE t_lips INDEX vl_tabix.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " Z_SELECIONA_EKBE_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIN_TRANSF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_lin_transf .

* Definição de tipo ----------------------------------------------------
  TYPES :
    BEGIN OF y_ref,
      refkey TYPE j_1bnflin-refkey,
    END OF y_ref.

  DATA :
    ti_reflin TYPE TABLE OF y_ref  WITH HEADER LINE,
    sl_mkpf   TYPE mkpf.

  CHECK NOT t_ekbe[] IS INITIAL.

  REFRESH ti_mkpf.

* Determinação dos documentos de faturamento
  SELECT        *
  INTO TABLE    ti_mkpf
  FROM          mkpf
  FOR ALL ENTRIES IN t_ekbe
  WHERE         le_vbeln = t_ekbe-belnr.
  IF sy-subrc IS INITIAL.

* Ajuste de tipo de campo
    LOOP AT ti_mkpf INTO sl_mkpf.
      CONCATENATE sl_mkpf-mblnr sl_mkpf-mjahr INTO ti_reflin-refkey.
      APPEND ti_reflin.
    ENDLOOP.

* Item de nota fiscal
    SELECT      *
    INTO TABLE  t_lin
    FROM        j_1bnflin
    FOR ALL ENTRIES IN ti_reflin
    WHERE       refkey = ti_reflin-refkey AND
                reftyp = 'MD'.

  ENDIF.

ENDFORM.                    " Z_SELECIONA_LIN_TRANSF

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_SAIDA_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3      text
*----------------------------------------------------------------------*
FORM z_seleciona_saida_3  USING p_tipo TYPE i.

  PERFORM:
* Seleciona Documentos de Transporte
           z_seleciona_vttk USING p_tipo,
* Seleciona Etapa do transporte
           z_seleciona_vtts,
* Seleciona Itens do Transporte
           z_seleciona_vttp,
* Seleciona dados de chegada
           z_seleciona_zlest0039,
* Seleciona Remessas
           z_seleciona_likp,
*Seleciona parceiros da remessa
           z_seleciona_vbpa,
*Seleciona parceiros
           z_seleciona_lfa13,
* Seleciona Itens Remessa
           z_seleciona_lips,
* Seleciona Centro
           z_seleciona_werks,
* Seleciona Itens Remessa - material
           z_seleciona_makt,
* Seleciona Dados Cliente
           z_seleciona_kna1_transf,
* Seleciona Documento de compras
           z_seleciona_ekbe_saida,
* Seleciona Itens Nota Fiscal - transferencia
           z_seleciona_lin_transf,
* Seleciona Documentos NF
           z_seleciona_doc ,
* Seleciona Itens Custo de Frete
           z_seleciona_vfkp,
* Seleciona Dados Lançamentos
           z_seleciona_lanc,
* Seleciona Dados Org. Transporte
           z_seleciona_ttds,
* Seleciona Dados de Endereço da organização de transporte
           z_seleciona_adrc,
* Seleciona Parceiros Transporte
           z_seleciona_vtpa,
* Seleciona Dados Parceiros
           z_seleciona_lfa1,
* Seleciona Dados Custos de frete
           z_seleciona_vfsi,
* Seleciona Condição ZFRE
           z_seleciona_konv,
* Seleciona Dados Itinerários
          z_seleciona_tvro,
* Seleciona Moeda
          z_seleciona_moeda.

ENDFORM.                    " Z_SELECIONA_SAIDA_3

*&---------------------------------------------------------------------*
*&      Form  Z_AGRUPA_DADOS_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_agrupa_dados_3 .

  " REFRESH t_alv.

  DATA: text_cliente_cgc TYPE c LENGTH 18,
        text_cliente_cpf TYPE c LENGTH 14.

  DATA: wl_kna1 TYPE kna1,
        wl_adrc TYPE adrc.

  SORT:
  t_vtts   BY tknum              ASCENDING,
  t_likp   BY vbeln              ASCENDING,
  t_lips   BY vbeln              ASCENDING,
  t_makt   BY matnr              ASCENDING,
  t_ekbe   BY belnr              ASCENDING,
  ti_mkpf  BY le_vbeln           ASCENDING,
  t_lin    BY refkey             ASCENDING,
  t_doc    BY docnum             ASCENDING,
  t_vfkp   BY rebel              ASCENDING,
  t_kna1   BY kunnr              ASCENDING,
  t_t0016  BY conhecimento       ASCENDING,
  ti_tvst  BY vstel              ASCENDING,
  t_adrc   BY addrnumber         ASCENDING,
  t_vtpa   BY vbeln parvw        ASCENDING,
  t_lfa1   BY lifnr              ASCENDING,
  t_vfkp   BY rebel              ASCENDING,
  t_vfsi   BY knumv vbeln posnr  ASCENDING,
  t_ttdst  BY tplst              ASCENDING,
  t_konv   BY knumv kposn kschl  ASCENDING,
  t_tcurr  BY ukurs              ASCENDING,
  t_vbpa   BY vbeln              ASCENDING,
  t_t001w  BY werks              ASCENDING.

* Documento de transporte
  LOOP AT t_vttk INTO sl_vttk.

    IF ( NOT p_placa IS INITIAL ) AND ( sl_vttk-text1(7) NE p_placa ).
      CONTINUE.
    ENDIF.

    CLEAR sl_vttp.

* Remessa referentes ao documento de transporte
    LOOP AT t_vttp INTO sl_vttp WHERE tknum EQ sl_vttk-tknum.

      CLEAR : sl_vtts, sl_likp, sl_lips, sl_makt, sl_ekbe, sl_mkpf, sl_lin,  sl_doc, sl_t0016,
              sl_vfkp, sl_alv,  sl_kna1, sl_adrc, sl_lfa1, sl_tvro, sl_konv, sl_vfkp, sl_vbpa,
              sl_lfa1_3, sl_t001w .

* Tipo de documento de transporte
      sl_alv-shtyp = sl_vttk-shtyp.

* Etapas do documento de transporte
      READ TABLE t_vtts_line4 INTO sl_vtts WITH KEY tknum = sl_vttk-tknum BINARY SEARCH.

* Documento de remessa - cabeçalho
      READ TABLE t_likp INTO sl_likp WITH KEY vbeln = sl_vttp-vbeln BINARY SEARCH.

* Parceiros da remessa
      READ TABLE t_vbpa INTO sl_vbpa WITH KEY vbeln = sl_likp-vbeln BINARY SEARCH.

* Documento de remessa - Item
      READ TABLE t_lips INTO sl_lips WITH KEY vbeln = sl_likp-vbeln BINARY SEARCH.

* Material - Descrição
      READ TABLE t_makt INTO sl_makt WITH KEY matnr = sl_lips-matnr BINARY SEARCH.

* Endereço de destino - Cliente
      IF sl_vttk-exti1 IS NOT INITIAL.
        READ TABLE t_t0016 INTO sl_t0016 WITH KEY conhecimento = sl_vttk-exti1 BINARY SEARCH.
        IF sy-subrc = 0.
          CLEAR  sl_t0016.
        ENDIF.
      ELSE.
        CLEAR sl_t0016.
      ENDIF.

* Endereço de destino - Cliente
      READ TABLE t_kna1 INTO sl_kna1 WITH KEY kunnr = sl_likp-kunnr BINARY SEARCH.

      IF ( sy-subrc NE 0 ).
* Cliente
        READ TABLE t_kna2 INTO sl_kna1 WITH KEY kunnr = sl_likp-kunnr BINARY SEARCH.

        sl_alv-name1_kn        = sl_kna1-name1.
        IF sl_kna1-stkzn IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
            EXPORTING
              input  = sl_kna1-stcd1
            IMPORTING
              output = text_cliente_cgc.
          sl_alv-stcd1_kn = text_cliente_cgc.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
            EXPORTING
              input  = sl_kna1-stcd2
            IMPORTING
              output = text_cliente_cpf.
          sl_alv-stcd1_kn = text_cliente_cpf.
        ENDIF.
        sl_alv-stcd3_kn = sl_kna1-stcd3.

      ELSE.

        sl_alv-name1_kn        = sl_kna1-name1.
        IF sl_kna1-stkzn IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
            EXPORTING
              input  = sl_kna1-stcd1
            IMPORTING
              output = text_cliente_cgc.
          sl_alv-stcd1_kn = text_cliente_cgc.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
            EXPORTING
              input  = sl_kna1-stcd2
            IMPORTING
              output = text_cliente_cpf.
          sl_alv-stcd1_kn = text_cliente_cpf.
        ENDIF.
        sl_alv-stcd3_kn = sl_kna1-stcd3.

      ENDIF.

* Cliente
      sl_alv-name1_kn        = sl_kna1-name1.
      IF sl_kna1-stkzn IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = sl_kna1-stcd1
          IMPORTING
            output = text_cliente_cgc.
        sl_alv-stcd1_kn = text_cliente_cgc.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            input  = sl_kna1-stcd2
          IMPORTING
            output = text_cliente_cpf.
        sl_alv-stcd1_kn = text_cliente_cpf.
      ENDIF.
      sl_alv-stcd3_kn = sl_kna1-stcd3.

* Endereço de origem
      READ TABLE ti_tvst INTO sl_tvst WITH KEY vstel = sl_vtts-vstel  BINARY SEARCH.

* Endereço de origem
      CLEAR sl_adrc.
      READ TABLE t_adrc INTO sl_adrc WITH KEY addrnumber = sl_tvst-adrnr BINARY SEARCH.

* Endereço de origem
      sl_alv-city1_ad         = sl_adrc-city1.
      sl_alv-region_ad        = sl_adrc-region.

* Endereço de destino
      CLEAR sl_adrc.
      READ TABLE t_adrc INTO sl_adrc WITH KEY addrnumber = sl_vtts-adrnz BINARY SEARCH.

      IF ( sy-subrc NE 0 ).
        " SORT T_VTTS_TESTE BY TKNUM  ASCENDING.

        DELETE t_vtts_line4 WHERE kunnz = ''.
        READ TABLE t_vtts_line4 INTO DATA(line4) WITH KEY tknum = sl_vtts-tknum ." BINARY SEARCH.


        SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ line4-kunnz.
* ENDEREÇO DE DESTINO
        sl_alv-ort01_kn        = wl_kna1-ort01.
        sl_alv-regio_kn        = wl_kna1-regio.
      ELSE.
* Endereço de destino
        sl_alv-ort01_kn        = sl_adrc-city1.
        sl_alv-regio_kn        = sl_adrc-region.
      ENDIF.



* Transportador
      READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vttk-tdlnr BINARY SEARCH.
      sl_alv-name1_lf        = sl_lfa1-name1.

* Conhecimento e carta frete
      sl_alv-exti1           = sl_vttk-exti1.
      sl_alv-exti2           = sl_vttk-exti2.

* Placa do veiculo
      sl_alv-text1           = sl_vttk-text1.

* Proprietário
      READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'PV'  BINARY SEARCH.
      READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtpa-lifnr                BINARY SEARCH.
      sl_alv-name1_pv        = sl_lfa1-name1.
      IF sl_lfa1-stkzn = 'X'.
        sl_alv-stcd_pv = sl_lfa1-stcd2 .
      ELSE.
        sl_alv-stcd_pv = sl_lfa1-stcd1 .
      ENDIF.
      IF ( NOT p_propv IS INITIAL ) AND ( p_propv <> sl_vtpa-lifnr ).
        CONTINUE.
      ENDIF.

* Motorista
      READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'MT'  BINARY SEARCH.
      READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtpa-lifnr                BINARY SEARCH.
      sl_alv-name1_mt        = sl_lfa1-name1.
      IF sl_lfa1-stkzn = 'X'.
        sl_alv-stcd_mt = sl_lfa1-stcd2 .
      ELSE.
        sl_alv-stcd_mt = sl_lfa1-stcd1 .
      ENDIF.

* Local de entrega
      READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'LR'  BINARY SEARCH.
      READ TABLE t_kna1 INTO sl_kna1 WITH KEY kunnr = sl_vtpa-kunnr                BINARY SEARCH.
      sl_alv-name1_lr        = sl_kna1-name1.

*Local de carregamento
      READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'PC'  BINARY SEARCH.
      READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtpa-lifnr                BINARY SEARCH.

      sl_alv-cd_carreg   = sl_lfa1-lifnr.
      sl_alv-desc_carreg = sl_lfa1-name1.
      sl_alv-city1_ad    = sl_lfa1-ort01.
      sl_alv-region_ad   = sl_lfa1-regio.

* Terminal
      READ TABLE t_vbpa  INTO sl_vbpa  WITH KEY vbeln = sl_likp-vbeln  parvw = 'Z1'  BINARY SEARCH.
      READ TABLE t_lfa1_3 INTO sl_lfa1_3 WITH KEY lifnr = sl_vbpa-lifnr              BINARY SEARCH.

      sl_alv-cd_terminal   = sl_lfa1_3-lifnr.
      sl_alv-desc_terminal = sl_lfa1_3-name1.
      sl_alv-end_terminal  = sl_lfa1_3-stras.
      sl_alv-cid_terminal  = sl_lfa1_3-ort01.

* Local de Entrega
      READ TABLE t_vtpa INTO sl_vtpa WITH KEY vbeln = sl_vttk-tknum  parvw = 'LR'  BINARY SEARCH.
      READ TABLE t_lfa1 INTO sl_lfa1 WITH KEY lifnr = sl_vtpa-lifnr                BINARY SEARCH.

      sl_alv-cid_entrega   = sl_lfa1-ort01.
      sl_alv-reg_entrega   = sl_lfa1-regio.

* Itinerario
      sl_alv-route           = sl_vttk-route.

* Material
      sl_alv-matnr           = sl_lips-matnr.
      sl_alv-maktx           = sl_makt-maktx.
      sl_alv-charg           = sl_lips-charg.

* Peso saída
      sl_alv-brgew           = sl_lips-brgew.

* Nome da Filial
      READ TABLE t_t001w INTO sl_t001w WITH KEY werks = sl_lips-werks.
      sl_alv-werks = sl_t001w-name1.


      IF sl_t0016 IS INITIAL.
*
*        READ TABLE t_tvro INTO sl_tvro WITH KEY route = sl_vttk-route  BINARY SEARCH.
*
*        IF sy-subrc IS INITIAL.
*
** Calcula dias
*          PERFORM z_calcula_dias USING sl_tvro-traztd
*                              CHANGING vl_dias.
*
*          sl_alv-peso_confirmado = sl_lips-brgew.
*          sl_alv-diferenca       = 0.
*          sl_alv-dta_chegada     = sl_vttk-datbg + vl_dias.
*
*        ELSE.
*
*          sl_alv-peso_confirmado = sl_lips-brgew.
*          sl_alv-diferenca       = 0.
*          sl_alv-dta_chegada     = sl_vttk-datbg + 1.
*
*        ENDIF.

        " ALRS 07.10.13
*        READ TABLE T_ZLEST0039 INTO W_ZLEST0039 WITH KEY VBELN = SL_VTTP-VBELN BINARY SEARCH.
*        IF SY-SUBRC = 0.
*          SL_ALV-PESO_CONFIRMADO = W_ZLEST0039-PESOCHEGADA.
*          SL_ALV-DIFERENCA       = SL_LIPS-BRGEW - W_ZLEST0039-PESOCHEGADA.
*          SL_ALV-DTA_CHEGADA     = W_ZLEST0039-DATACHEGADA.
*        ENDIF.
        READ TABLE t_vbfa_qt INTO sl_vbfa_qt WITH KEY vbelv = sl_likp-vbeln BINARY SEARCH.
        IF sy-subrc = 0.
          sl_alv-peso_confirmado = sl_vbfa_qt-rfmng.
          sl_alv-diferenca       = sl_lips-brgew - sl_vbfa_qt-rfmng.
          sl_alv-dta_chegada     = sl_vbfa_qt-aedat.
        ENDIF.

      ELSE.

        sl_alv-peso_confirmado = sl_t0016-peso_confirmado.
        sl_alv-diferenca       = sl_lips-brgew - sl_t0016-peso_confirmado.
        sl_alv-dta_chegada     = sl_t0016-dta_chegada.

      ENDIF.

* Data do transporte
      sl_alv-datbg           = sl_vttk-datbg.

* Documento de custo de frete
      READ TABLE t_vfkp INTO sl_vfkp WITH KEY rebel = sl_vttk-tknum                                               BINARY SEARCH.
      READ TABLE t_vfsi INTO sl_vfsi WITH KEY knumv = sl_vfkp-knumv  vbeln = sl_lips-vbeln  posnr = sl_lips-posnr BINARY SEARCH.
      READ TABLE t_konv INTO sl_konv WITH KEY knumv = sl_vfkp-knumv  kposn = sl_vfsi-kposn  kschl = c_fre         BINARY SEARCH.

* Analitico
********************************************************************************************************************************
* Valor unitario
      sl_alv-kbetr           = sl_konv-kbetr.

* Total do frete
      sl_alv-netwr           = sl_vfsi-kzwi1.

* Total de imposots
      sl_alv-mwsbp           = sl_vfsi-kzwi6.

********************************************************************************************************************************

* Cambio - Dolar
* Documento de custo de frete
      "      CONCATENATE sl_vfkp-prsdt+6(2) '.' sl_vfkp-prsdt+4(2) '.' sl_vfkp-prsdt(4) INTO sl_data.
      CONCATENATE sl_vttk-erdat+6(2) '.' sl_vttk-erdat+4(2) '.' sl_vttk-erdat(4) INTO sl_data.
      IF NOT sl_data = '00.00.0000'.

        CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
          EXPORTING
            input  = sl_data
          IMPORTING
            output = p_gdatu.

* Cambio / Valor unitario e total em dolar.
        READ TABLE t_tcurr INTO sl_tcurr WITH KEY gdatu = p_gdatu.
        IF sy-subrc IS INITIAL.
          sl_alv-ukurs           = sl_tcurr-ukurs.
          sl_alv-kbetr1          = sl_konv-kbetr / sl_tcurr-ukurs.
          sl_alv-netwr1          = sl_alv-netwr / sl_tcurr-ukurs.
        ELSE.
          READ TABLE t_tcurr INTO sl_tcurr INDEX 1.
          IF sy-subrc IS INITIAL AND sl_tcurr-ukurs <> 0.
            sl_alv-ukurs           = sl_tcurr-ukurs.
            sl_alv-kbetr1          = sl_konv-kbetr / sl_tcurr-ukurs.
            sl_alv-netwr1          = sl_alv-netwr / sl_tcurr-ukurs.
          ELSE.
            sl_alv-ukurs           = 0.
            sl_alv-kbetr1          = 0.
            sl_alv-netwr1          = 0.
          ENDIF.
        ENDIF.

      ENDIF.

* IOF
      READ TABLE t_vfkp INTO sl_vfkp WITH KEY rebel = sl_vttk-tknum                                               BINARY SEARCH.
      READ TABLE t_vfsi INTO sl_vfsi WITH KEY knumv = sl_vfkp-knumv  vbeln = sl_lips-vbeln  posnr = sl_lips-posnr BINARY SEARCH.
      READ TABLE t_konv INTO sl_konv WITH KEY knumv = sl_vfkp-knumv  kposn = sl_vfsi-kposn kschl = c_iof          BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        sl_alv-ziof           = sl_konv-kwert.
      ENDIF.

* Seguro
      READ TABLE t_vfkp INTO sl_vfkp WITH KEY rebel = sl_vttk-tknum                                               BINARY SEARCH.
      READ TABLE t_vfsi INTO sl_vfsi WITH KEY knumv = sl_vfkp-knumv  vbeln = sl_lips-vbeln  posnr = sl_lips-posnr BINARY SEARCH.
      READ TABLE t_konv INTO sl_konv WITH KEY knumv = sl_vfkp-knumv  kposn = sl_vfsi-kposn  kschl = c_seg         BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        sl_alv-zseg           = sl_konv-kwert.

        sl_alv-kbetr_per = sl_konv-kbetr / 100.
      ENDIF.

* Pegagio
      READ TABLE t_vfkp INTO sl_vfkp WITH KEY rebel = sl_vttk-tknum                                               BINARY SEARCH.
      READ TABLE t_vfsi INTO sl_vfsi WITH KEY knumv = sl_vfkp-knumv  vbeln = sl_lips-vbeln  posnr = sl_lips-posnr BINARY SEARCH.
      READ TABLE t_konv INTO sl_konv WITH KEY knumv = sl_vfkp-knumv  kposn = sl_vfsi-kposn  kschl = c_ped         BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        sl_alv-zped           = sl_konv-kwert.
      ENDIF.
      IF  sl_lips-brgew  > 0 .
        sl_alv-frete_ton = ( ( sl_vfsi-kzwi1 + sl_konv-kwert ) / sl_lips-brgew ) * 1000.
      ELSE.
        sl_alv-frete_ton = 0.
      ENDIF.

* Local de organização de transporte
      READ TABLE t_ttdst INTO sl_ttdst WITH KEY tplst = sl_vttk-tplst  BINARY SEARCH.
      sl_alv-bezei           = sl_ttdst-bezei.

* Documento de transporte
      sl_alv-tknum           = sl_vttk-tknum.
      sl_alv-uaten           = sl_vttk-uatbg.

* Documento de custo
      sl_alv-fknum           = sl_vfkp-fknum.

* Pedido de serviço de frete
      sl_alv-ebeln           = sl_vfkp-ebeln.

* Folha de registros de serviço de frete
      sl_alv-lblni           = sl_vfkp-lblni.

* Pedido de compras de produto
      READ TABLE t_ekbe INTO sl_ekbe WITH KEY belnr = sl_likp-vbeln BINARY SEARCH.
      sl_alv-ebeln1           = sl_ekbe-ebeln.

* Documento de material
      READ TABLE ti_mkpf INTO sl_mkpf WITH KEY le_vbeln = sl_likp-vbeln BINARY SEARCH.
      sl_alv-mblnr           = sl_mkpf-mblnr.

* Nota fiscal
      CONCATENATE sl_mkpf-mblnr sl_mkpf-mjahr INTO vl_refkey.
      READ TABLE t_lin INTO sl_lin WITH KEY refkey = vl_refkey BINARY SEARCH.
      READ TABLE t_doc INTO sl_doc WITH KEY docnum = sl_lin-docnum  BINARY SEARCH.

      CALL FUNCTION 'Z_1B_NF_VALUE_DETERMINATION'
        EXPORTING
          p_docnum   = sl_doc-docnum
        IMPORTING
          ext_header = wa_j_cabe.

      sl_alv-vlr_nf = wa_j_cabe-nftot.

      " sl_alv-VLR_NF  = sl_lin-netpr * sl_lin-menge.
      sl_alv-nfenum          = sl_doc-nfenum.
      sl_alv-series          = sl_doc-series.

* 133799 CS2024000094 Inclusão de campos para atendimento das operações de algodão - PSA


      APPEND sl_alv TO t_alv.

      CLEAR: sl_alv,  sl_vbak ,  sl_vbfa_va, sl_vbfa_vf,  sl_vbfa_qt, sl_vttp,  sl_doc,  sl_lin,  sl_lips,
             sl_t001w, sl_makt, sl_likp ,  sl_t0016,   sl_adrc,  sl_kna1,  sl_ttdst, sl_tcurr,  sl_lfa1,  sl_lfa1_3,
             sl_tvro, sl_vfkp, sl_vfsi,  sl_ekbe,  sl_ttds,  sl_tvst, sl_vtpa, sl_mkpf, sl_vtpa_mt, sl_vtts, sl_alv,
             sl_konv, sl_vbpa, wa_j_cabe, vl_refkey, p_gdatu, w_alv, w_vttk, w_ttds, w_zlest0039, w_zlest0035, wl_kna1.

    ENDLOOP.

    CLEAR sl_vttk.
  ENDLOOP.
ENDFORM.                    " Z_AGRUPA_DADOS_3

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_KNA1_TRANSF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM z_seleciona_kna1_transf .

  DATA tl_likp TYPE TABLE OF likp.

  CHECK NOT t_vtts_line4[] IS INITIAL.

  SELECT *
  FROM kna1
  INTO TABLE t_kna1
  FOR ALL ENTRIES IN t_vtts_line4
  WHERE  kunnr EQ t_vtts_line4-kunnz.

  SELECT *
FROM kna1
INTO TABLE t_kna2
FOR ALL ENTRIES IN t_vtts_line3
WHERE  kunnr EQ t_vtts_line3-kunnz.

  SORT t_kna1 BY kunnr ASCENDING.
  SORT t_kna2 BY kunnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_KNA1_TRANSF

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MOEDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_moeda .

  SELECT *
  INTO TABLE t_tcurr
  FROM tcurr
  WHERE
  kurst EQ c_b          AND
  fcurr EQ c_usd        AND
  tcurr EQ c_brl.

ENDFORM.                    " Z_SELECIONA_MOEDA
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0039
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_zlest0039 .
  DATA tl_vttp TYPE TABLE OF vttp.

  CHECK NOT t_vttp[] IS INITIAL.

  tl_vttp[] = t_vttp[].
  SORT tl_vttp BY vbeln ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vttp COMPARING vbeln.

  SELECT vbeln
         datachegada
         pesochegada
         datatransb
         pesotransb
         pontotransb
    FROM zlest0039
    INTO TABLE t_zlest0039
    FOR ALL ENTRIES IN tl_vttp
  WHERE  vbeln EQ tl_vttp-vbeln.

  SORT t_zlest0039 BY vbeln ASCENDING.
ENDFORM.                    " Z_SELECIONA_ZLEST0039
*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZA_BPC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_atualiza_bpc .
  TYPES: BEGIN OF ty_vbak,
           vbeln TYPE vbak-vbeln,
           vkorg TYPE vbak-vkorg,
         END OF ty_vbak,

         BEGIN OF ty_depo_aux,
           werks    TYPE lfa1-lifnr,
           lifnr    TYPE zsdt_depara_depo-lifnr,
           operacao TYPE zsdt_depara_depo-operacao,
           werks_v  TYPE zsdt_depara_depo-werks_v,
         END OF ty_depo_aux.

  DATA: sl_atual         TYPE zlest0078,
        tl_depo          TYPE TABLE OF zsdt_depara_depo,
        tl_depo_aux      TYPE TABLE OF ty_depo_aux,
        sl_depo          TYPE zsdt_depara_depo,
        sl_depo_aux      TYPE ty_depo_aux,
        tl_vbak          TYPE TABLE OF ty_vbak,
        sl_vbak          TYPE ty_vbak,
        sl_vttk          TYPE vttk,
        wl_contn         TYPE i,
        wl_cont(4),
        wl_cd_carreg(10).

  CLEAR:wl_cont, wl_contn.

  LOOP AT t_alv INTO sl_alv.
    sl_alv-werks_aux = sl_alv-cd_carreg+6(4).
    MODIFY t_alv FROM sl_alv INDEX sy-tabix TRANSPORTING werks_aux.
  ENDLOOP.

  SELECT vbeln vkorg
    FROM vbak
      INTO TABLE tl_vbak
      FOR ALL ENTRIES IN t_alv
        WHERE vbeln EQ t_alv-vbeln_va.

  SELECT *
    FROM zsdt_depara_depo
      INTO TABLE tl_depo
      FOR ALL ENTRIES IN t_alv
        WHERE  werks EQ t_alv-werks_aux
        AND    lifnr EQ t_alv-cd_terminal.

  REFRESH tl_depo_aux.
  LOOP AT tl_depo INTO sl_depo.
    MOVE: sl_depo-werks   TO sl_depo_aux-werks,
          sl_depo-lifnr   TO sl_depo_aux-lifnr,
          sl_depo-werks_v TO sl_depo_aux-werks_v,
          sl_depo-operacao TO sl_depo_aux-operacao.

    APPEND: sl_depo_aux TO tl_depo_aux.
    CLEAR: sl_depo_aux.
  ENDLOOP.

  LOOP AT t_alv INTO sl_alv.
    ADD 1 TO wl_contn .
    wl_cont = wl_contn.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_cont
      IMPORTING
        output = wl_cont.

    CLEAR: wl_cd_carreg.

    READ TABLE t_vttk INTO sl_vttk
      WITH KEY tknum = sl_alv-tknum.

    READ TABLE tl_vbak INTO sl_vbak
      WITH KEY vbeln = sl_alv-vbeln_va.

    SELECT SINGLE auart
      FROM vbak
      INTO @DATA(v_auart)
      WHERE vbeln = @sl_vbak-vbeln.

    DATA(_opera) = 'RF'.
    IF  v_auart EQ 'ZIND'.
      _opera = 'RI'.
    ENDIF.

    CLEAR sl_depo_aux.
    READ TABLE tl_depo_aux INTO sl_depo_aux
      WITH KEY werks = sl_alv-werks_aux
               lifnr = sl_alv-cd_terminal
               operacao = _opera.

*    CONCATENATE SL_ALV-CD_CARREG+9(1) SL_ALV-CD_CARREG+8(1) SL_ALV-CD_CARREG+7(1) SL_ALV-CD_CARREG+6(1)
*                SL_ALV-CD_CARREG+5(1) SL_ALV-CD_CARREG+4(1) SL_ALV-CD_CARREG+3(1) SL_ALV-CD_CARREG+2(1)
*                SL_ALV-CD_CARREG+1(1) SL_ALV-CD_CARREG(1) INTO WL_CD_CARREG.
*
*    CONCATENATE wl_cd_carreg+3(1) wl_cd_carreg+2(1)


    MOVE: sl_alv-tknum          TO sl_atual-tknum,
          sl_vbak-vkorg         TO sl_atual-bukrs,
          sl_depo_aux-werks_v   TO sl_atual-centro_destino,
          sl_alv-cd_carreg+6(4) TO sl_atual-centro_origem,
          sl_alv-matnr          TO sl_atual-matnr,
          sl_vttk-vsart         TO sl_atual-vsart,
          sl_alv-datbg          TO sl_atual-erdat,
          sl_alv-brgew          TO sl_atual-menge,
          sl_alv-netwr          TO sl_atual-vlr_frete_r,
          sl_vttk-add03         TO sl_atual-tp_frete,
          sl_alv-name1_lr       TO sl_atual-local_entrega,
          sl_alv-cid_terminal   TO sl_atual-cid_terminal,
          sl_alv-cd_terminal    TO sl_atual-cod_terminal,
          sl_alv-desc_terminal  TO sl_atual-nm_terminal,
          sl_alv-charg          TO sl_atual-safra,
          sl_alv-netwr1         TO sl_atual-vlr_frete_us.

*    TRY .
*        SL_ATUAL-VLR_FRETE_US = SL_ALV-NETWR / SL_ALV-KBETR1.
*      CATCH CX_SY_ZERODIVIDE.
*
*    ENDTRY.

    APPEND: sl_atual TO t_zlest0078.
    CLEAR: sl_atual, sl_depo_aux, sl_vttk, sl_vbak.

  ENDLOOP.

  MODIFY zlest0078 FROM TABLE t_zlest0078.
  COMMIT WORK.

  MESSAGE s000(z01) DISPLAY LIKE 'S' WITH  'Foram atualizadas:' wl_cont 'linhas.'.
ENDFORM.                    " Z_ATUALIZA_BPC
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0045
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt0045 USING p_instrucao CHANGING v_data_in_porto
                                                     v_data_porto.
  CLEAR:  v_data_in_porto,
          v_data_porto.
  "V_OBSERVACAO.


  SELECT SINGLE
    z~data_in_porto
    z~data_porto
   " Z~OBSERVACAO
  FROM zsdt0045 AS z
  INTO (v_data_in_porto, v_data_porto)
  WHERE z~instrucao =  p_instrucao.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1268   text
*      -->P_WL_NAME  text
*----------------------------------------------------------------------*
FORM read_text  USING    p_id
                         p_name.

  MOVE p_id    TO wl_id.
  MOVE p_name  TO wl_name.

  FREE tg_texto.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = wl_id
      language                = sy-langu
      name                    = wl_name
      object                  = 'ZLESR0007'
    TABLES
      lines                   = tg_texto
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc IS INITIAL.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1364   text
*      -->P_WL_HEADER  text
*----------------------------------------------------------------------*
FORM save_text  USING    VALUE(p_1364)
                         p_wl_header.

  DATA: it_lines TYPE STANDARD TABLE OF tline WITH HEADER LINE,
        wa_lines LIKE LINE OF it_lines.


  LOOP AT tl_texto INTO wl_texto.
    wa_lines-tdformat = '*'.
    " WA_LINES-TDLINE+0(72) = WL_TEXTO.
    wa_lines-tdline  = wl_texto.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN wa_lines-tdline WITH space.
    APPEND wa_lines TO it_lines.
    CLEAR:  wa_lines, wl_texto.
  ENDLOOP.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      client          = sy-mandt
      header          = wl_header
      savemode_direct = 'X'
    TABLES
      lines           = it_lines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.

ENDFORM.                    " SAVE_TEXT
*&---------------------------------------------------------------------*
*&      Form  FM_SEL_DADOS_TXT_VT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_sel_dados_txt_vt .

  DATA: tl_tline TYPE TABLE OF tline,
        lv_data  TYPE sy-datum,
        lv_hora  TYPE sy-uzeit,
        lv_txt   TYPE char15,
        lv_txt1  TYPE char15,
        lv_txt2  TYPE char15,
        lv_txt3  TYPE char15.



*--------------------------------------------------------------------------------------------------------*
*   Atraves do documento de transporte, selecionar as informações gerais txt.
*--------------------------------------------------------------------------------------------------------*

  LOOP AT t_alv[] ASSIGNING FIELD-SYMBOL(<ws_alv>).
    FREE: tl_tline.
    CALL FUNCTION 'Z_LES_GET_TEXT_VT'
      EXPORTING
        i_tknum   = <ws_alv>-tknum
        i_id_text = 'CM18'
      TABLES
        tl_tline  = tl_tline.
    IF tl_tline IS NOT INITIAL.
      LOOP AT tl_tline INTO DATA(ws_tline).

*        CONDENSE ws_tline-tdline NO-GAPS.

        CLEAR: lv_txt, lv_txt1, lv_txt2, lv_txt3.
        SPLIT ws_tline-tdline AT ':' INTO: lv_txt1 lv_txt lv_txt2 lv_txt3.
        CONDENSE lv_txt NO-GAPS.

        SPLIT lv_txt AT '-' INTO: lv_txt2 lv_txt3.
        CONDENSE lv_txt3 NO-GAPS.

        CASE sy-tabix.
          WHEN 1.
            IF lv_txt2 IS NOT INITIAL AND lv_txt3 IS NOT INITIAL.

              <ws_alv>-tp_transp = |{ lv_txt2 } - { lv_txt3 }|.
            ELSE.
              <ws_alv>-tp_transp = lv_txt.
            ENDIF.

          WHEN 2.
            <ws_alv>-equip_tac = lv_txt.
          WHEN 3.
            "Converter data.
            CLEAR: lv_data.
            lv_data = |{ lv_txt+6(4) }{ lv_txt+3(2) }{ lv_txt+0(2) }|.
            <ws_alv>-data_tp = lv_data.
          WHEN 4.
            "Converter data.
            CLEAR: lv_hora.
            lv_hora = |{ lv_txt+0(2) }{ lv_txt+3(2) }{ lv_txt+6(2) }|.
            <ws_alv>-hora_tp = lv_hora.
          WHEN 5.
            <ws_alv>-base_tipbank = lv_txt.
          WHEN 6.
            <ws_alv>-grupo = lv_txt.
          WHEN 7.
            <ws_alv>-frota = lv_txt.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.
