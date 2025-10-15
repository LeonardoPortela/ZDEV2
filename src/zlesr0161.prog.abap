*&---------------------------------------------------------------------*
*& Report  ZLESR0161
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0161.

TABLES: vttk, vtpa, adrc, vttp, lips, makt, vfkp, bkpf, bsis, vbak, vbfa, j_1bnflin, j_1bnfdoc, zlest0002.

TYPES: BEGIN OF ty_saida,
         tknum              TYPE vttk-tknum, " Doc. Transp com 0 a esquerda
         tknum1             TYPE vttk-tknum, " Doc. Transp
         fknum              TYPE vfkp-fknum, " Doc. Custo com 0 a esquerda
         fknum1             TYPE vfkp-fknum, " Doc. Custo
         tplst              TYPE vttk-tplst, " Filial
         erdat              TYPE vttk-erdat, " Data Documento
         matnr              TYPE lips-matnr, " Cod. Produto
         matnr1             TYPE lips-matnr, " Cod. Produto zeros a esquerda
         maktx              TYPE makt-maktx, " Descrição Produto
         text1              TYPE vttk-text1, " Placa Cavalo
         text2              TYPE vttk-text2, " Carreta1
         text3              TYPE vttk-text3, " Carreta2
         text4              TYPE vttk-text4, " Carreta2
         placa_carreta      TYPE vttk-text1, " Placa Cavalo
         carreta1           TYPE vttk-text2, " Carreta1
         carreta2           TYPE vttk-text3, " Carreta2
         carreta3           TYPE vttk-text3, " Carreta3
         shtyp              TYPE vttk-shtyp, " Tp. Transporte
         tdlnr              TYPE vttk-tdlnr, " Agente Fretecom 0 a esquerda
         tdlnr1             TYPE vttk-tdlnr, " Agente Frete
         lifnr              TYPE vtpa-lifnr, " Origem - Código --- Código do parceiro PC com 0 a esquerda
         lifnr1             TYPE vtpa-lifnr, " Origem - Código --- Código do parceiro PC
         desc_origem        TYPE adrc-name1, " Origem - Descrição --- concatenar (ADRC-NAME1 - ADRC-CITY1) do parceiro PC
         kunnr              TYPE vtpa-kunnr, "Destino - Código --- Código do parceiro LR com 0 a esquerda
         kunnr1             TYPE vtpa-kunnr, "Destino - Código --- Código do parceiro LR
         desc_destino       TYPE adrc-name1, "Destino - Descrição --- concatenar (ADRC-NAME1 - ADRC-CITY1) do parceiro LR
         tipo               TYPE char15, " Tipo --- Próprio , Terceiros e Intercompany
         mwsbp              TYPE vfkp-mwsbp, " Imp_Receita
         ntgew              TYPE vfkp-netwr, " Kg Trasportados
         preco_tonelada_rs  TYPE vfkp-netwr, " Preço por Ton_R$
         netwr              TYPE vfkp-netwr, " Valor Total R$
         taxa_dolar         TYPE p DECIMALS 4, " Tx_Dólar
         dmbe2              TYPE vfkp-netwr, " Valor Total USD
         bkpf_belnr         TYPE bkpf-belnr, " Doc Cont  Custo
         belnr2             TYPE bkpf-belnr, " Doc Cont  Receita
         docnum             TYPE j_1bnfdoc-docnum, " Docnum_Receita
         nfenum             TYPE j_1bnfdoc-nfenum, " Nr_Ct-e
         nfenum1            TYPE j_1bnfdoc-nfenum, " Nr_Ct-e

         adrnr              TYPE vtpa-adrnr,
         parvw              TYPE vtpa-parvw,
         xblnr              TYPE bkpf-xblnr,
         bukrs              TYPE bkpf-bukrs,
         gjahr              TYPE bkpf-gjahr,
         tp_veiculo         TYPE char01,
         vbak_vbeln         TYPE vbak-vbeln,
         vbfa_vbeln         TYPE vbfa-vbeln,
         vbak_bukrs_vf      TYPE vbak-bukrs_vf,
         vbak_auart         TYPE vbak-auart,
         vbfa_erdat         TYPE vbfa-erdat,
         vttp_vbeln         TYPE vttp-vbeln,
         grupo_veiculo      TYPE zlest0002-grupo,
         frota              TYPE zlest0002-frota,
         grupo_produto      TYPE lips-matwa,
         cod_motorista      TYPE vtpa-lifnr,  "DO PARCEIRO MT
         nome_motorista     TYPE adrc-name1, " DO PARCEIRO MT
         cpf_motorista      TYPE lfa1-stcd1,

         bezei              TYPE ttdst-bezei,  "Nome Filial
         munic_origem       TYPE adrc-city1,   " Origem do parceiro PC
         uf_origem          TYPE adrc-country, " UF do parceiro PC Origem
         munic_destino      TYPE adrc-city1,   "Destino do parceiro LR
         uf_destino         TYPE adrc-country, " UF do parceiro PC Destino
         ov_pedido          TYPE lips-vgbel,   "OV do pedido
         tp_ov              TYPE vbak-auart,   "Tipo de ordem de venda
         transgenese        TYPE vbak-kvgr3,   "
         dt_descarga        TYPE zlest0039-datachegada, " Data da descarga
         peso_descarga      TYPE zlest0039-pesotransb, "Peso transporte
         cod_tran_efetivo   TYPE zlest0039-transb_efetivo,  "Codigo do transporte efetivo
         descr_tran_efetivo TYPE kna1-name1, "Descrição transporte efetivo
         docnum_mdfe        TYPE zsdt0105-docnum_ref, "Documento referencia
         nr_mdfe            TYPE zsdt0102-nmdfe, "Numero da MDFE
         ordem_car          TYPE zsdt0001od-nr_ordem, "Ordem de carregamento
         dt_oc              TYPE zsdt0001od-dt_emissao, " Data de emissão
       END OF ty_saida,

       BEGIN OF ty_saida_ordem_carregamento,
         nr_ordem             TYPE zsdt0001od-nr_ordem,            "Nr. Od  Carreg.
         tp_status            TYPE zsdt0001od-tp_status,           "Status_OD
         dt_emissao           TYPE zsdt0001od-dt_emissao,          "Dt_emissao_OD
         dt_validade          TYPE zsdt0001od-dt_validade,         "Dt_validade_OD
         id_bukrs             TYPE zsdt0001od-id_bukrs,            "Empresa
         id_branch            TYPE zsdt0001od-id_branch,           "Filial
         name                 TYPE j_1bbranch-name,                "Descr_filial
         id_bukrs_ag          TYPE zsdt0001od-id_bukrs_ag,         "Empresa_Transp
         id_branch_ag         TYPE zsdt0001od-id_branch_ag,        "Filial_Transp
         id_local_coleta      TYPE zsdt0001od-id_local_coleta,     "Cód_Coleta
         local_coleta_name1   TYPE  lfa1-name1,                    "Descr. Ponto Coleta
         local_coleta_ort01   TYPE  lfa1-ort01,                    "Munic. Ponto Coleta
         local_coleta_regio   TYPE  lfa1-regio,                    "UF_PC
         id_local_descarga    TYPE  zsdt0001od-id_local_descarga,  "Cód. Descarga
         descarga_kname1      TYPE  kna1-name1,                    "Desc. Descarga
         descarga_ort01       TYPE  kna1-ort01,                    "Munic. Descarga
         descarga_regio       TYPE  kna1-regio,                    "UF_Descarga
         id_local_destino     TYPE  zsdt0001od-id_local_destino,       "Cód. Destino
         local_destino_name1  TYPE  lfa1-name1,                    "Desc. Destino
         local_destino_ort01  TYPE  lfa1-ort01,                    "Munic. Destino
         local_destino_regio  TYPE  lfa1-regio,                    "UF_Destino
         doc_transp           TYPE  zsdt0001-doc_transp,           "Doc_transp
         tknum                TYPE  zsdt0001-tknum,                "Doc_custo
         erdat                TYPE  vttk-erdat,                    "Dt_doc_Transp
         peso_fiscal          TYPE  zsdt0001-peso_fiscal,          "Peso_embarque
         datatransb           TYPE  zlest0039-datatransb,          "Dt_descarga
         pesotransb           TYPE  zlest0039-pesotransb,          "Peso_descarga
         transb_efetivo       TYPE  zlest0039-transb_efetivo,      "Cód_transb_efetivo
         transb_efetivo_name1 TYPE  kna1-name1,                    "Descr. Transb_efetivo
         ds_placa_trator      TYPE  zsdt0001od-ds_placa_trator,    "Placa_Cavalo
         s_placa_reboq_1      TYPE  zsdt0001od-ds_placa_reboq_1,   "Carreta_1
         ds_placa_reboq_2     TYPE  zsdt0001od-ds_placa_reboq_2,   "Carreta_2
         ds_placa_reboq_3     TYPE  zsdt0001od-ds_placa_reboq_3,   "Carreta_3
         grupo                TYPE  zlest0002-grupo,               "Grupo
         frota                TYPE  zlest0002-frota,               "Frota
         id_motorista         TYPE  zsdt0001od-id_motorista,       "Cód+motorista
         motorista_stcd2      TYPE lfa1-stcd2,                     "CPF_motorista
         motorista_name1      TYPE lfa1-name1,                     "Nome_motorista
         nr_frete_comb        TYPE  zsdt0001od-nr_frete_comb,      "Frete_Combinado
         id_ordem             TYPE zsdt0001od-id_ordem,             "Nr_ordem

       END OF ty_saida_ordem_carregamento,


       BEGIN OF ty_zsdt0001od,
         nr_ordem          TYPE zsdt0001od-nr_ordem,
         dt_validade       TYPE zsdt0001od-dt_validade,
         dt_emissao        TYPE zsdt0001od-dt_emissao,
         id_bukrs          TYPE zsdt0001od-id_bukrs,
         id_branch         TYPE zsdt0001od-id_branch,
         id_bukrs_ag       TYPE zsdt0001od-id_bukrs_ag,
         id_branch_ag      TYPE zsdt0001od-id_branch_ag,
         id_local_coleta   TYPE zsdt0001od-id_local_coleta,
         id_local_destino  TYPE zsdt0001od-id_local_destino,
         id_local_descarga TYPE zsdt0001od-id_local_descarga,
         ds_placa_trator   TYPE zsdt0001od-ds_placa_trator,
         ds_placa_reboq_1  TYPE zsdt0001od-ds_placa_reboq_1,
         ds_placa_reboq_2  TYPE zsdt0001od-ds_placa_reboq_2,
         ds_placa_reboq_3  TYPE zsdt0001od-ds_placa_reboq_3,
         id_motorista      TYPE zsdt0001od-id_motorista,
         nr_frete_comb     TYPE zsdt0001od-nr_frete_comb,
         tp_status         TYPE zsdt0001od-tp_status,
         id_ordem          TYPE zsdt0001od-id_ordem,
       END OF ty_zsdt0001od,

       BEGIN OF ty_vttk, "PRINCIOAL
         tknum TYPE  vttk-tknum,
         shtyp TYPE  vttk-shtyp,
         tplst TYPE  vttk-tplst,
         tdlnr TYPE  vttk-tdlnr,
         text1 TYPE  vttk-text1,
         text2 TYPE  vttk-text2,
         text3 TYPE  vttk-text3,
         erdat TYPE  vttk-erdat,
         xblnr TYPE  bkpf-xblnr,
       END OF ty_vttk,


       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         stcd1 TYPE lfa1-stcd1,
       END OF ty_lfa1,

       BEGIN OF ty_zlest0002,
         pc_veiculo TYPE  zlest0002-pc_veiculo,
         grupo      TYPE  zlest0002-grupo,
         frota      TYPE  zlest0002-frota,
       END OF ty_zlest0002,

       BEGIN OF ty_vtpa, "parceiros  do  transporte
         kunnr TYPE vtpa-kunnr,
         lifnr TYPE vtpa-lifnr,
         adrnr TYPE vtpa-adrnr,
         parvw TYPE vtpa-parvw,
         vbeln TYPE vtpa-vbeln,
       END OF ty_vtpa,

       BEGIN OF ty_adrc, " nome dos  parceiros
         name1 TYPE adrc-name1,
         city1 TYPE adrc-city1,
       END OF ty_adrc,

       BEGIN OF ty_vttp, " Identificar material e quantidade transportada
         vbeln TYPE vttp-vbeln,
         tknum TYPE vttp-tknum,
       END OF ty_vttp,

       BEGIN OF ty_lips,
         matnr TYPE lips-matnr,
         "NTGEW TYPE LIPS-NTGEW,
         vbeln TYPE lips-vbeln,
         brgew TYPE lips-brgew,
         matwa TYPE lips-matwa,
         vgbel TYPE lips-vgbel,
         pstyv TYPE lips-pstyv,
         matkl TYPE lips-matkl,
       END OF ty_lips,

       BEGIN OF ty_makt, "Descritivo do  material
         maktx TYPE makt-maktx,
         matnr TYPE makt-matnr,
       END OF ty_makt,

       BEGIN OF ty_vfkp, "buscar  dados  de  custo e  valores  em BRL e impostos
         knumv TYPE vfkp-knumv,
         fknum TYPE vfkp-fknum,
         netwr TYPE vfkp-netwr,
         mwsbp TYPE vfkp-mwsbp,
         rebel TYPE vfkp-rebel,
         fkpty TYPE vfkp-fkpty,
       END OF ty_vfkp,

       BEGIN OF ty_bkpf, "Buscar  valor em USD
         belnr TYPE bkpf-belnr,
         bukrs TYPE bkpf-bukrs,
         gjahr TYPE bkpf-gjahr,
       END OF ty_bkpf,

       BEGIN OF ty_bsis,
         dmbtr TYPE bsis-dmbtr,
         dmbe2 TYPE bsis-dmbe2,
       END OF ty_bsis,

       BEGIN OF ty_vbak, "Documento Contábil Receita
         vbeln    TYPE vbak-vbeln,
         bukrs_vf TYPE vbak-bukrs_vf,
         auart    TYPE vbak-auart,
       END OF ty_vbak,

       BEGIN OF ty_vbfa,
         vbeln TYPE char35,
         erdat TYPE vbfa-erdat,
         awkey TYPE bkpf-awkey,
       END OF ty_vbfa,

       BEGIN OF ty_j_1bnflin, " Docnum da  Receita
         refkey TYPE j_1bnflin-refkey,
         docnum TYPE j_1bnflin-docnum,
       END OF ty_j_1bnflin,

       BEGIN OF ty_j_1bnfdoc,
         nfenum TYPE j_1bnfdoc-nfenum,
         docnum TYPE j_1bnfdoc-docnum,
         belnr  TYPE j_1bnfdoc-belnr,
       END OF ty_j_1bnfdoc.


CLASS lcl_timer DEFINITION DEFERRED.
*
"DECLARA AS TABELAS TEMPORARIAS E AS WORK_AREAS PARA AS ESTRUTURAS DECLARADAS A CIMA
DATA: it_saida                    TYPE TABLE OF ty_saida,
      it_zsdt0001                 TYPE TABLE OF zsdt0001,
      it_zsdt0001od               TYPE TABLE OF ty_zsdt0001od,
      it_saida_carregamento       TYPE TABLE OF ty_saida_ordem_carregamento,
      it_saida_aux                TYPE TABLE OF ty_saida,
      t_zsdt0001od                TYPE TABLE OF zsdt0001od,
      it_vttk                     TYPE TABLE OF  ty_vttk,
      it_vtpa                     TYPE TABLE OF  ty_vtpa,
      it_sort_vtpa                TYPE SORTED TABLE OF ty_vtpa WITH NON-UNIQUE DEFAULT KEY,
      it_adrc                     TYPE TABLE OF  ty_adrc,
      it_vttp                     TYPE TABLE OF  ty_vttp,
      it_sort_vttp                TYPE SORTED TABLE OF ty_vttp WITH NON-UNIQUE DEFAULT KEY,
      it_lips                     TYPE TABLE OF  ty_lips,
      t_vbak                      TYPE TABLE OF  vbak,
      t_vbfa                      TYPE TABLE OF  vbfa,
      t_j_1bnflin                 TYPE TABLE OF j_1bnflin,
      it_makt                     TYPE TABLE OF  ty_makt,
      it_vfkp                     TYPE TABLE OF  ty_vfkp,
      it_bkpf                     TYPE TABLE OF  ty_bkpf,
      it_bsis                     TYPE TABLE OF  ty_bsis,
      it_vbak                     TYPE TABLE OF  ty_vbak,
      it_vbfa                     TYPE TABLE OF  ty_vbfa,
      it_j_1bnflin                TYPE TABLE OF  ty_j_1bnflin,
      it_j_1bnfdoc                TYPE TABLE OF  ty_j_1bnfdoc,
      it_zlest0002                TYPE TABLE OF  ty_zlest0002,
      it_lfa1                     TYPE TABLE OF ty_lfa1,
      wa_saida                    TYPE ty_saida,
      wa_saida_ordem_carregamento TYPE ty_saida_ordem_carregamento,
      wa_zsdt0001od               TYPE ty_zsdt0001od,
      wa_vttp                     TYPE ty_vttp,
      wa_lips                     TYPE ty_lips,
      wa_makt                     TYPE ty_makt,
      wa_vfkp                     TYPE ty_vfkp,
      wa_bkpf                     TYPE ty_bkpf,
      wa_bsis                     TYPE ty_bsis,
      wa_vbak                     TYPE ty_vbak,
      wa_vbfa                     TYPE ty_vbfa,
      wa_j_1bnflin                TYPE ty_j_1bnflin,
      wa_j_1bnfdoc                TYPE ty_j_1bnfdoc,
      v_name1                     LIKE adrc-name1,
      v_city1                     LIKE adrc-city1,
      v_descricao                 TYPE string,
      cpf_motorista(14)           TYPE c,

      t_saida_final               TYPE TABLE OF zlest0227.

DATA: gs_variant   TYPE disvariant, " é para poder escolher o layout
      variante     LIKE disvariant,
      gs_variant_c TYPE disvariant,
      vs_refkey    TYPE j_1bnflin-refkey.

DATA: r_refkey     TYPE RANGE OF j_1brefkey,
      r_refkey_aux TYPE RANGE OF j_1brefkey.

DATA: r_vbeln     TYPE RANGE OF vbeln,
      r_vbeln_aux TYPE RANGE OF vbeln.

* ALV

DATA:
  g_custom_container TYPE REF TO cl_gui_custom_container,
  dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
  dg_parent_1        TYPE REF TO cl_gui_container,
  dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
  dg_parent_2        TYPE REF TO cl_gui_container,
  dg_parent_2a       TYPE REF TO cl_gui_container,
  dg_parent_alv      TYPE REF TO cl_gui_container,
  picture            TYPE REF TO cl_gui_picture,
  ctl_alv            TYPE REF TO cl_gui_alv_grid,
  dg_dyndoc_id       TYPE REF TO cl_dd_document,
  table_element      TYPE REF TO cl_dd_table_element,
  column             TYPE REF TO cl_dd_area,
  table_element2     TYPE REF TO cl_dd_table_element,
  column_1           TYPE REF TO cl_dd_area,
  dg_html_cntrl      TYPE REF TO cl_gui_html_viewer,
  it_exclude_fcode   TYPE ui_functions,
  wa_exclude_fcode   LIKE LINE OF it_exclude_fcode,
  gs_layout          TYPE lvc_s_layo,
  it_fieldcatalog    TYPE lvc_t_fcat,
  wa_fieldcatalog    TYPE lvc_s_fcat,
  it_sort            TYPE lvc_t_sort,
  ls_stable          TYPE lvc_s_stbl,
  str                TYPE REF TO data,
  ob_timer           TYPE REF TO cl_gui_timer,
  ob_recev           TYPE REF TO lcl_timer,
  frota              TYPE zlest0002-frota,
  grupo_veiculo      TYPE zlest0002-grupo.


DATA: r_erdat  TYPE RANGE OF zlese0160,
      lw_erdat LIKE LINE OF r_erdat,
      r_tknum  TYPE RANGE OF zlese0160,
      lw_tknum LIKE LINE OF r_tknum,
      r_tplst  TYPE RANGE OF zlese0160,
      lw_tplst LIKE LINE OF r_tplst,
      r_placa  TYPE RANGE OF zlese0160,
      lw_placa LIKE LINE OF r_placa.

DATA: url(255)                TYPE c,
      p_text                  TYPE sdydo_text_element,
      sdydo_text_element(255),
      p_text_table            TYPE sdydo_text_table,
      vl_cont                 TYPE i,
      vl_butxt                TYPE t001-butxt,
      vl_dates1               TYPE char10,
      vl_dates2               TYPE char10.

DATA: vg_repid   LIKE sy-repid,
      vg_variant TYPE disvariant.

CLASS lcl_timer DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_finished FOR EVENT finished OF cl_gui_timer.
ENDCLASS.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME  TITLE TEXT-001.

  SELECT-OPTIONS: p_erdat     FOR    vttk-erdat, "OBLIGATORY, "DATA
                  p_tknum   FOR   vttk-tknum, " DOC. TRANSP.
                  p_tplst FOR vttk-tplst, " FILIAL
                  p_placa FOR zlest0002-pc_veiculo. "placa

  SELECTION-SCREEN BEGIN OF LINE.

    PARAMETERS: p_prop RADIOBUTTON GROUP r1 DEFAULT 'X'."AS CHECKBOX.
    SELECTION-SCREEN COMMENT 4(13) TEXT-c02 FOR FIELD p_prop. "P_EX_TES.
    SELECTION-SCREEN POSITION 20.

    PARAMETERS: p_terc RADIOBUTTON GROUP r1."AS CHECKBOX.
    SELECTION-SCREEN COMMENT 24(15) TEXT-c03 FOR FIELD p_terc. "P_C_LANC.
    SELECTION-SCREEN POSITION 40.
*
    PARAMETERS: p_carre RADIOBUTTON GROUP r1."AS CHECKBOX.
    SELECTION-SCREEN COMMENT 44(18) TEXT-c04 FOR FIELD p_carre. "P_C_LANC.
    SELECTION-SCREEN POSITION 60.

  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.

*------------------------------------------- LAYOUT ----------------------------------------------------------"

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETER: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b2.

INITIALIZATION.
  REFRESH  p_erdat.
  p_erdat-sign = 'I'.
  p_erdat-option = 'BT'.
  p_erdat-low = sy-datum - 30.
  p_erdat-high = sy-datum .
  APPEND p_erdat.

  IF variante IS INITIAL.
    variante-report = sy-repid.
    variante-variant = p_varia.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  variante-report = sy-repid.
  variante-variant = p_varia.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
    MOVE variante-variant TO gs_variant_c-variant.
  ENDIF.

* ------------------------------------------- FIM LAYOUT -------------------------------------------------------"

CLASS lcl_timer IMPLEMENTATION.
  METHOD handle_finished.

    DATA: mensagem(30) TYPE c.

    PERFORM limpa_campos.
    PERFORM busca_dados.

    CONCATENATE 'Última Atualização: ' sy-uzeit(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2) INTO mensagem.

    MESSAGE  mensagem TYPE 'S' DISPLAY LIKE 'S'.


    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable = ls_stable.
    CALL METHOD ob_timer->run.
  ENDMETHOD.                    "handle_finished
ENDCLASS.

INITIALIZATION.


START-OF-SELECTION.

  p_erdat-low = sy-datum - 30.
  p_erdat-high = sy-datum .
  APPEND p_erdat.

  PERFORM busca_dados.

  IF p_carre NE 'X'.

    MODIFY zlest0227 FROM TABLE t_saida_final.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDIF.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:  on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id e_row_id es_row_no sender.


ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_hotspot_click.

    DATA: vbranch TYPE j_1bbranch-branch.
    READ TABLE it_saida INTO DATA(wa_saida) INDEX e_row_id-index.

    CASE e_column_id.
      WHEN 'DOCNUM'.

        CLEAR vbranch.

        vbranch = wa_saida-tdlnr+6(4).
        "VBRANCH = |{ VBRANCH ALPHA = IN }|.

        SELECT SINGLE * FROM j_1bbranch INTO @DATA(wa_j_1bbranch)
          WHERE branch EQ @vbranch.

        SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida-docnum.
        SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_j_1bbranch-bukrs.
        CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

FORM busca_dados.
  DATA: i_t_select    TYPE sbiwa_t_select,
        wa_i_t_select TYPE sbiwa_s_select.

  IF sy-calld = 'X'.
    REFRESH p_erdat.
    p_erdat-low = sy-datum - 30.
    p_erdat-high = sy-datum .
    p_erdat-sign = 'I'.
    p_erdat-option = 'BT'.
    APPEND p_erdat.






  ENDIF.








*  LOOP AT p_erdat.
*    lw_erdat-sign    = p_erdat-sign.
*    lw_erdat-option  = p_erdat-option.
*    lw_erdat-low     = p_erdat-low.
*    lw_erdat-high    = p_erdat-high.
*    APPEND lw_erdat TO r_erdat.
*    CLEAR lw_erdat.
*  ENDLOOP.
*
*  LOOP AT p_tknum.
*    lw_tknum-sign    = p_tknum-sign.
*    lw_tknum-option  = p_tknum-option.
*    lw_tknum-low     = p_tknum-low.
*    lw_tknum-high    = p_tknum-high.
*    APPEND lw_tknum TO r_tknum.
*    CLEAR lw_tknum.
*  ENDLOOP.
*
*  LOOP AT p_tplst.
*    lw_tplst-sign    = p_tplst-sign.
*    lw_tplst-option  = p_tplst-option.
*    lw_tplst-low     = p_tplst-low.
*    lw_tplst-high    = p_tplst-high.
*    APPEND lw_tplst TO r_tplst.
*    CLEAR lw_tplst.
*  ENDLOOP.
*
*  LOOP AT p_placa.
*    lw_placa-sign    = p_placa-sign.
*    lw_placa-option  = p_placa-option.
*    lw_placa-low     = p_placa-low.
*    lw_placa-high    = p_placa-high.
*    APPEND lw_placa TO r_placa.
*    CLEAR lw_placa.
*  ENDLOOP.

  LOOP AT p_erdat.
    CLEAR wa_i_t_select.
    wa_i_t_select-fieldnm = 'erdat'.
    wa_i_t_select-sign    = p_erdat-sign.
    wa_i_t_select-option  = p_erdat-option.
    wa_i_t_select-low     = p_erdat-low.
    wa_i_t_select-high    = p_erdat-high.
    APPEND wa_i_t_select TO i_t_select.
    CLEAR: p_erdat.
  ENDLOOP.

  LOOP AT p_tknum.
    CLEAR wa_i_t_select.
    wa_i_t_select-fieldnm = 'tknum'.
    wa_i_t_select-sign    = p_tknum-sign.
    wa_i_t_select-option  = p_tknum-option.
    wa_i_t_select-low     = p_tknum-low.
    wa_i_t_select-high    = p_tknum-high.
    APPEND wa_i_t_select TO i_t_select.
  ENDLOOP.

  LOOP AT p_tplst.
    CLEAR wa_i_t_select.
    wa_i_t_select-fieldnm = 'tplst'.
    wa_i_t_select-sign    = p_tplst-sign.
    wa_i_t_select-option  = p_tplst-option.
    wa_i_t_select-low     = p_tplst-low.
    wa_i_t_select-high    = p_tplst-high.
    APPEND wa_i_t_select TO i_t_select.
  ENDLOOP.

  LOOP AT p_placa.
    CLEAR wa_i_t_select.
    wa_i_t_select-fieldnm = 'placa'.
    wa_i_t_select-sign    = p_placa-sign.
    wa_i_t_select-option  = p_placa-option.
    wa_i_t_select-low     = p_placa-low.
    wa_i_t_select-high    = p_placa-high.
    APPEND wa_i_t_select TO i_t_select.
  ENDLOOP.

  CALL FUNCTION 'ZLES0177_SELECIONAR_DADOS'
    EXPORTING
      p_prop           = p_prop
      p_terc           = p_terc
      p_carre          = p_carre
    IMPORTING
      it_saida_carrega = it_saida_carregamento
    TABLES
      i_t_select       = i_t_select
*     t_erdat          = r_erdat
*     t_tknum          = r_tknum
*     t_tplst          = r_tplst
*     t_placa          = r_placa
      t_saida          = t_saida_final.

ENDFORM. "BUSCA_DADOS


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS0100'.
  SET TITLEBAR 'TITULO100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

FORM fill_it_fieldcatalog USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header)
                                VALUE(p_hotspot)
                                VALUE(p_no_zero)
                                VALUE(p_lzero).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

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
  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.

FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.

FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.

  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

  REFRESH graphic_table.

  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.

  WHILE l_graphic_conv > 255.

    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.

  ENDWHILE.

  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.

ENDFORM.

FORM imprimir_alv.
  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.


    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.
    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.


    PERFORM fill_it_fieldcatalog USING:

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
          23 'MWSBP              ' ' '  '12'  ' '     ' '    ' '   'Imp_Receita            '   ' '   ' '   ' ',
          24 'NTGEW              ' ' '  '12'  ' '     ' '    ' '   'Kg Trasportados        '   ' '   ' '   ' ',
          25 'PRECO_TONELADA_RS  ' ' '  '17'  ' '     ' '    ' '   'Preço por Ton_R$       '   ' '   ' '   ' ',
          26 'NETWR              ' ' '  '12'  ' '     ' '    ' '   'Valor Total R$         '   ' '   ' '   ' ',
          27 'TAXA_DOLAR         ' ' '  '12'  ' '     ' '    ' '   'Tx_Dólar               '   ' '   ' '   ' ',
          28 'DMBE2              ' ' '  '12'  ' '     ' '    ' '   'Valor Total USD        '   ' '   ' '   ' ',
          29 'BELNR2             ' ' '  '12'  ' '     ' '    ' '   'Doc Cont  Receita      '   ' '   ' '   ' ',
          30 'DOCNUM             ' ' '  '12'  ' '     ' '    ' '   'Docnum_Receita         '   'X'   ' '   ' ',
          31 'NFENUM             ' ' '  '10'  ' '     ' '    ' '   'Nr_Ct-e                '   ' '   'X'   ' ',
          32 'VTTP_VBELN         ' ' '  '10'  ' '     ' '    ' '   'Remessa                '   ' '   ' '   ' ',
          33 'OV_PEDIDO          ' ' '  '12'  ' '     ' '    ' '   'OV/Pedido              '   ' '   ' '   ' ',
          34 'TP_OV              ' ' '  '12'  ' '     ' '    ' '   'Tipo de OV             '   ' '   ' '   ' ',
          35 'TRANSGENESE        ' ' '  '12'  ' '     ' '    ' '   'Transgenese            '   ' '   ' '   ' ',
          36 'DT_DESCARGA        ' ' '  '10'  ' '     ' '    ' '   'Dt descarga            '   ' '   ' '   ' ',
          37 'PESO_DESCARGA      ' ' '  '10'  ' '     ' '    ' '   'Peso Descarga          '   ' '   ' '   ' ',
          38 'COD_TRAN_EFETIVO   ' ' '  '10'  ' '     ' '    ' '   'Cód transb efetivo     '   ' '   ' '   ' ',
          39 'DESCR_TRAN_EFETIVO ' ' '  '10'  ' '     ' '    ' '   'Descr transb efetivo   '   ' '   ' '   ' ',
          40 'GRUPO_VEICULO      ' ' '  '10'  ' '     ' '    ' '   'Grupo Veículo          '   ' '   ' '   'X',
          41 'FROTA              ' ' '  '10'  ' '     ' '    ' '   'Frota                  '   ' '   ' '   'X',
          42 'GRUPO_PRODUTO      ' ' '  '10'  ' '     ' '    ' '   'Grupo Produto          '   ' '   ' '   ' ',
          43 'COD_MOTORISTA      ' ' '  '10'  ' '     ' '    ' '   'Cod Motorista          '   ' '   ' '   ' ',
          44 'NOME_MOTORISTA     ' ' '  '10'  ' '     ' '    ' '   'Nome Motorista         '   ' '   ' '   ' ',
          45 'CPF_MOTORISTA      ' ' '  '10'  ' '     ' '    ' '   'CPF motorista          '   ' '   ' '   ' ',
          46 'DOCNUM_MDFE        ' ' '  '10'  ' '     ' '    ' '   'Docnum MDF-e           '   ' '   ' '   ' ',
          47 'NR_MDFE            ' ' '  '10'  ' '     ' '    ' '   'Nr. MDF-e              '   ' '   ' '   ' ',
          48 'ORDEM_CAR          ' ' '  '10'  ' '     ' '    ' '   'Ordem Carreg.          '   ' '   ' '   ' ',
          49 'DT_OC              ' ' '  '10'  ' '     ' '    ' '   'Dt OC                  '   ' '   ' '   ' '.


    PERFORM fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
*    gs_layout-col_opt = 'X'.
    gs_layout-cwidth_opt = 'X'.
    gs_layout-zebra = 'X'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].


    CREATE OBJECT ctl_alv
      EXPORTING
        i_shellstyle    = 0
        i_parent        = dg_parent_alv
        i_appl_events   = abap_false
        i_fcat_complete = abap_false.

    SET HANDLER lcl_event_handler=>on_hotspot_click FOR ctl_alv.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        i_save                        = 'A'
        is_variant                    = variante
"       IT_TOOLBAR_EXCLUDING          = IT_EXCLUDE_FCODE
      CHANGING
        it_fieldcatalog               = it_fieldcatalog
        it_outtab                     = it_saida_aux
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

*    step 1 : initialise time control
    CREATE OBJECT ob_timer
      EXPORTING
        parent = g_custom_container.

* Step 2 : Initialise object to receive event
    CREATE OBJECT ob_recev.

* Step 3 : Couple event to method
    SET HANDLER ob_recev->handle_finished FOR ob_timer.

* Step 4 : Set Interval in seconds
    ob_timer->interval = 300.

    CALL METHOD ob_timer->run.



    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        "SAP_ALIGN = 'CENTER'
        sap_style = cl_dd_document=>heading.

    p_text = TEXT-003.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.




    "------------------
    LOOP AT p_erdat.
      IF p_erdat-option EQ 'BT'.
        CONCATENATE p_erdat-low+6(2) '.' p_erdat-low+4(2) '.' p_erdat-low(4) INTO vl_dates1.
        CONCATENATE p_erdat-high+6(2) '.' p_erdat-high+4(2) '.' p_erdat-high(4) INTO vl_dates2.
        CONCATENATE 'Período:' vl_dates1 '-' vl_dates2 INTO sdydo_text_element SEPARATED BY space.
        EXIT.
      ELSE.
        CONCATENATE p_erdat-low+6(2) '.' p_erdat-low+4(2) '.' p_erdat-low(4) INTO vl_dates1.
        CONCATENATE 'Período:' vl_dates1 INTO sdydo_text_element SEPARATED BY space.
      ENDIF.
    ENDLOOP.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: sdydo_text_element, vl_dates1, vl_dates2.

    "------------------
    IF p_tknum IS NOT INITIAL.
      LOOP AT p_tknum.
        IF p_tknum-option NE 'EQ' AND p_tknum-option NE 'BT'.
          sdydo_text_element = 'Doc Transp.: Multiplas Seleções'.
          EXIT.
        ELSEIF p_tknum-option EQ 'BT'.
          CONCATENATE 'Doc Transp.:' p_tknum-low '-' p_tknum-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Doc Transp.: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Doc Transp.:' p_tknum-low INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.
    CLEAR: vl_cont, sdydo_text_element.

    "------------------
    IF p_tplst IS NOT INITIAL.
      LOOP AT p_tplst.
        IF p_tplst-option NE 'EQ' AND p_tplst-option NE 'BT'.
          sdydo_text_element = 'Filial: Multiplas Seleções'.
          EXIT.
        ELSEIF p_tplst-option EQ 'BT'.
          CONCATENATE 'Filial:' p_tplst-low '-' p_tplst-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Filial: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Filial:' p_tplst-low INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.
    CLEAR: vl_cont, sdydo_text_element.
    "------------------
    IF p_placa IS NOT INITIAL.
      LOOP AT p_placa.
        IF p_placa-option NE 'EQ' AND p_placa-option NE 'BT'.
          sdydo_text_element = 'Placa_Cavalo: Multiplas Seleções'.
          EXIT.
        ELSEIF p_placa-option EQ 'BT'.
          CONCATENATE 'Placa_Cavalo:' p_placa-low '-' p_placa-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Placa_Cavalo: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Placa_Cavalo:' p_placa-low INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.
    CLEAR: vl_cont, sdydo_text_element.

    "------------------
    IF p_prop = 'X'.
      sdydo_text_element = 'Tipo: Frota Próprio'.
    ELSE.
      sdydo_text_element = 'Tipo: Frota Terceiro'.
    ENDIF.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, sdydo_text_element.
    CLEAR: vl_cont, sdydo_text_element.

    "------------------
    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_2
      EXCEPTIONS
        html_display_error = 1.

    "PERFORM AJUSTA_TOTAIS.

  ELSE.

    ls_stable-row = 'X'.
    ls_stable-col = 'X'.

    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
    ENDIF.

  ENDIF.

  CALL SCREEN 0100.

ENDFORM.

FORM imprimir_alv_carregamento.
  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.


    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.
    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.


    PERFORM fill_it_fieldcatalog USING:

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
38 'nr_frete_comb'         ' '  '15'  ''    ''   ''     'Frete_Combinado'	        ''    ' '    ' '.

    PERFORM fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
    gs_layout-cwidth_opt = 'X'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].


    CREATE OBJECT ctl_alv
      EXPORTING
        i_shellstyle    = 0
        i_parent        = dg_parent_alv
        i_appl_events   = abap_false
        i_fcat_complete = abap_false.

    SET HANDLER lcl_event_handler=>on_hotspot_click FOR ctl_alv.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        i_save                        = 'A'
        is_variant                    = variante
"       IT_TOOLBAR_EXCLUDING          = IT_EXCLUDE_FCODE
      CHANGING
        it_fieldcatalog               = it_fieldcatalog
        it_outtab                     = it_saida_carregamento
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

*    step 1 : initialise time control
    CREATE OBJECT ob_timer
      EXPORTING
        parent = g_custom_container.

* Step 2 : Initialise object to receive event
    CREATE OBJECT ob_recev.

* Step 3 : Couple event to method
    SET HANDLER ob_recev->handle_finished FOR ob_timer.

* Step 4 : Set Interval in seconds
    ob_timer->interval = 300.

    CALL METHOD ob_timer->run.



    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        "SAP_ALIGN = 'CENTER'
        sap_style = cl_dd_document=>heading.

    p_text = TEXT-003.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.




    "------------------
    LOOP AT p_erdat.
      IF p_erdat-option EQ 'BT'.
        CONCATENATE p_erdat-low+6(2) '.' p_erdat-low+4(2) '.' p_erdat-low(4) INTO vl_dates1.
        CONCATENATE p_erdat-high+6(2) '.' p_erdat-high+4(2) '.' p_erdat-high(4) INTO vl_dates2.
        CONCATENATE 'Período:' vl_dates1 '-' vl_dates2 INTO sdydo_text_element SEPARATED BY space.
        EXIT.
      ELSE.
        CONCATENATE p_erdat-low+6(2) '.' p_erdat-low+4(2) '.' p_erdat-low(4) INTO vl_dates1.
        CONCATENATE 'Período:' vl_dates1 INTO sdydo_text_element SEPARATED BY space.
      ENDIF.
    ENDLOOP.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: sdydo_text_element, vl_dates1, vl_dates2.

    "------------------
    IF p_tplst IS NOT INITIAL.
      LOOP AT p_tplst.
        IF p_tplst-option NE 'EQ' AND p_tplst-option NE 'BT'.
          sdydo_text_element = 'Filial: Multiplas Seleções'.
          EXIT.
        ELSEIF p_tplst-option EQ 'BT'.
          CONCATENATE 'Filial:' p_tplst-low '-' p_tplst-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Filial: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Filial:' p_tplst-low INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.
    CLEAR: vl_cont, sdydo_text_element.
    "------------------
    IF p_placa IS NOT INITIAL.
      LOOP AT p_placa.
        IF p_placa-option NE 'EQ' AND p_placa-option NE 'BT'.
          sdydo_text_element = 'Placa_Cavalo: Multiplas Seleções'.
          EXIT.
        ELSEIF p_placa-option EQ 'BT'.
          CONCATENATE 'Placa_Cavalo:' p_placa-low '-' p_placa-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Placa_Cavalo: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Placa_Cavalo:' p_placa-low INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.
    CLEAR: vl_cont, sdydo_text_element.

    "------------------
    IF p_carre = 'X'.
      sdydo_text_element = 'Tipo: Ordem Carregamento'.
    ENDIF.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, sdydo_text_element.

    "------------------
    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_2
      EXCEPTIONS
        html_display_error = 1.

    "PERFORM AJUSTA_TOTAIS.

  ELSE.

    ls_stable-row = 'X'.
    ls_stable-col = 'X'.

    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
    ENDIF.

  ENDIF.

  CALL SCREEN 0100.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_campos .

  REFRESH: it_saida, it_saida_aux, it_vttk, it_vtpa, it_adrc, it_vttp, it_lips, it_makt, it_vfkp, it_bkpf,
  it_bsis, it_vbak, it_vbfa, it_j_1bnflin, it_j_1bnfdoc, it_zlest0002, it_lfa1, it_zsdt0001od, it_saida_carregamento.

  CLEAR: wa_saida, wa_vttp, wa_lips, wa_makt, wa_vfkp, wa_bkpf, wa_bsis, wa_vbak,
  wa_vbfa, wa_j_1bnflin, wa_j_1bnfdoc,  v_name1, v_city1, v_descricao, cpf_motorista, wa_zsdt0001od, wa_saida_ordem_carregamento.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SEL_FRETE_SUBCONTRATADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_sel_frete_subcontratado .

  TYPES: BEGIN OF ty_saida_frete_subcontratado,
           vbeln   TYPE vbak-vbeln,
           vbeln_a TYPE vbfa-vbelv,
           docnum  TYPE j_1bnflin-docnum,
         END OF ty_saida_frete_subcontratado.



  DATA: t_zlest0194      TYPE TABLE OF zlest0194,
        t_saida_aux_sub  TYPE TABLE OF ty_saida_frete_subcontratado,
        t_vbak           TYPE TABLE OF vbak,
        t_vbfa           TYPE TABLE OF vbfa,
        t_j_1bnflin      TYPE TABLE OF j_1bnflin,
        t_j_1bnfe_active TYPE TABLE OF j_1bnfe_active,
        t_lfa1           TYPE TABLE OF lfa1,
        it_lfa1          TYPE TABLE OF lfa1,
        t_kna1           TYPE TABLE OF kna1,
        t_bkpf           TYPE TABLE OF bkpf,
        t_j_1bnfdoc      TYPE TABLE OF j_1bnfdoc,
        t_zlest0002      TYPE TABLE OF zlest0002,
        t_vbrk           TYPE TABLE OF vbrk,
        t_saida_sub      TYPE TABLE OF ty_saida,
        ws_saida_sub     TYPE ty_saida.

  DATA: r_awkey     TYPE RANGE OF awkey,
        r_gjahr     TYPE RANGE OF gjahr,
        z_awkey     TYPE awkey,
        z_gjahr     TYPE gjahr,
        z_kurs2     TYPE kurs2,
        z_kurs2_aux TYPE char9.

  FREE: r_refkey, t_j_1bnflin, t_j_1bnfe_active, t_zlest0194, t_lfa1, t_kna1, t_kna1, t_saida_aux_sub.
  SELECT * FROM vbak
    INTO CORRESPONDING FIELDS OF TABLE t_vbak
    WHERE auart EQ 'ZSSF'
      AND erdat IN p_erdat.

  CHECK t_vbak IS NOT INITIAL.

  SELECT * FROM vbfa AS a
   INTO CORRESPONDING FIELDS OF TABLE t_vbfa
    FOR ALL ENTRIES IN t_vbak
    WHERE a~vbelv EQ t_vbak-vbeln
      AND a~vbtyp_n = 'M'
      AND a~vbtyp_v = 'C'
      AND NOT EXISTS ( SELECT * FROM vbfa  AS b
                          WHERE b~vbelv EQ a~vbeln
                            AND b~vbtyp_n EQ 'N' ).


  CHECK t_vbfa IS NOT INITIAL.


  r_refkey = VALUE #( FOR l IN t_vbfa ( sign   = 'I' option = 'EQ' low    = l-vbeln ) ).
  SORT r_refkey BY low.
  DELETE ADJACENT DUPLICATES FROM r_refkey COMPARING low.


  SELECT * FROM j_1bnflin INTO TABLE t_j_1bnflin
    WHERE refkey IN r_refkey.

  IF t_j_1bnflin IS NOT INITIAL.
    SELECT * FROM j_1bnfe_active INTO TABLE t_j_1bnfe_active
      FOR ALL ENTRIES IN t_j_1bnflin
      WHERE docnum EQ t_j_1bnflin-docnum.
    SORT t_j_1bnfe_active BY cancel.

    IF t_j_1bnfe_active IS NOT INITIAL.
      DELETE t_j_1bnfe_active WHERE cancel EQ 'X'.
    ENDIF.

    IF t_j_1bnfe_active IS NOT INITIAL.
      SORT t_j_1bnfe_active BY docnum.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING t_vbak TO t_saida_aux_sub.

  LOOP AT t_saida_aux_sub ASSIGNING FIELD-SYMBOL(<ws_saida>).
    READ TABLE t_vbfa INTO DATA(ws_vbfa) WITH KEY vbelv = <ws_saida>-vbeln.
    IF sy-subrc EQ 0.
      <ws_saida>-vbeln_a = ws_vbfa-vbeln.
      READ TABLE t_j_1bnflin INTO DATA(ws_j_1bnflin) WITH KEY refkey = ws_vbfa-vbeln.
      IF sy-subrc EQ 0.
        READ TABLE t_j_1bnfe_active INTO DATA(ws_j_1bnfe_active) WITH KEY docnum = ws_j_1bnflin-docnum.
        IF sy-subrc EQ 0.
          <ws_saida>-docnum = ws_j_1bnfe_active-docnum.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SELECT * FROM zlest0194 INTO TABLE t_zlest0194
    FOR ALL ENTRIES IN t_saida_aux_sub
    WHERE ov_sub EQ t_saida_aux_sub-vbeln
      AND fat_sub EQ t_saida_aux_sub-vbeln_a
      AND docnum_sub EQ t_saida_aux_sub-docnum.


  CHECK t_zlest0194 IS NOT INITIAL.

  FREE: t_lfa1.
  SELECT * FROM lfa1 INTO TABLE t_lfa1
    FOR ALL ENTRIES IN t_zlest0194
    WHERE lifnr EQ t_zlest0194-reme_cod_forn.

  SELECT * FROM lfa1 INTO TABLE it_lfa1
  FOR ALL ENTRIES IN t_zlest0194
  WHERE lifnr EQ t_zlest0194-motorista.

  SELECT * FROM kna1 INTO TABLE t_kna1
    FOR ALL ENTRIES IN t_zlest0194
    WHERE kunnr EQ t_zlest0194-dest_cod_cliente.


  r_awkey = VALUE #( FOR t IN t_zlest0194 ( sign   = 'I' option = 'EQ' low    = t-fat_sub ) ).
  SORT r_awkey BY low.
  DELETE ADJACENT DUPLICATES FROM r_awkey COMPARING low.

  r_gjahr = VALUE #( FOR g IN t_zlest0194 ( sign   = 'I' option = 'EQ' low    = g-dt_mov_ov(4) ) ).
  SORT r_gjahr BY low.
  DELETE ADJACENT DUPLICATES FROM r_refkey COMPARING low.

  SELECT * FROM bkpf INTO TABLE t_bkpf
    FOR ALL ENTRIES IN t_zlest0194
    WHERE awkey IN r_awkey
     AND  bukrs EQ  t_zlest0194-bukrs_ov
     AND  gjahr IN r_gjahr.

  SELECT * FROM j_1bnfdoc INTO TABLE t_j_1bnfdoc
    FOR ALL ENTRIES IN t_zlest0194
    WHERE docnum   EQ t_zlest0194-docnum_sub.


  SELECT * FROM zlest0002 INTO TABLE t_zlest0002
    FOR ALL ENTRIES IN t_zlest0194
    WHERE pc_veiculo EQ t_zlest0194-placa_cav.


  SELECT * FROM vbrk INTO TABLE t_vbrk
    FOR ALL ENTRIES IN t_zlest0194
    WHERE vbeln EQ t_zlest0194-fat_sub.

  LOOP AT t_zlest0194 INTO DATA(ws_zlest0194).
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


    READ TABLE t_lfa1 INTO DATA(ws_lfa1) WITH KEY lifnr = ws_zlest0194-reme_cod_forn.
    IF sy-subrc EQ 0.
      ws_saida_sub-munic_origem      = ws_lfa1-ort01.
      ws_saida_sub-uf_origem         = ws_lfa1-regio.
    ENDIF.

    READ TABLE t_kna1 INTO DATA(ws_kna1) WITH KEY kunnr = ws_zlest0194-dest_cod_cliente.
    IF sy-subrc EQ 0.
      ws_saida_sub-munic_destino      = ws_kna1-ort01.
      ws_saida_sub-uf_destino         = ws_kna1-regio.
    ENDIF.

    READ TABLE t_vbrk INTO DATA(ws_vbrk) WITH KEY vbeln = ws_zlest0194-fat_sub.
    IF sy-subrc EQ 0.
      ws_saida_sub-mwsbp                = ws_vbrk-mwsbk.
    ENDIF.


    CLEAR: z_awkey, z_gjahr.
    z_awkey = CONV #( ws_zlest0194-fat_sub ).
    z_gjahr = ws_zlest0194-dt_mov_ov(4).


    READ TABLE t_bkpf INTO DATA(ws_bkpf) WITH KEY awkey = z_awkey
                                                  bukrs = ws_zlest0194-bukrs_ov
                                                  gjahr = z_gjahr.

    IF sy-subrc EQ 0.

      IF ws_bkpf-kurs2 IS NOT INITIAL.
        z_kurs2_aux = ws_bkpf-kurs2.
        REPLACE ALL OCCURRENCES OF '/' IN z_kurs2_aux WITH space.
        REPLACE ALL OCCURRENCES OF '-' IN z_kurs2_aux WITH space.

        z_kurs2 = CONV #( z_kurs2_aux ).
      ENDIF.

      ws_saida_sub-taxa_dolar        = z_kurs2. "(desconsiderar a / do início)
      ws_saida_sub-dmbe2             = ( ws_zlest0194-valor_prestacao * z_kurs2 ).
      ws_saida_sub-belnr2            = ws_bkpf-belnr.
    ENDIF.

    READ TABLE t_j_1bnfdoc INTO DATA(ws_j_1bnfdoc) WITH KEY docnum = ws_zlest0194-docnum_sub.
    IF sy-subrc EQ 0.
      ws_saida_sub-nfenum             = ws_j_1bnfdoc-nfenum.
    ENDIF.

    READ TABLE t_zlest0002 INTO DATA(ws_zlest0002) WITH KEY pc_veiculo = ws_zlest0194-placa_cav.
    IF sy-subrc EQ 0.
      ws_saida_sub-grupo_veiculo        =  ws_zlest0002-grupo.
    ENDIF.

    CLEAR: ws_lfa1.
    READ TABLE it_lfa1 INTO ws_lfa1 WITH KEY lifnr = ws_zlest0194-motorista.
    IF sy-subrc EQ 0.
      ws_saida_sub-nome_motorista    = ws_lfa1-name1.
    ENDIF.

    APPEND ws_saida_sub  TO it_saida_aux.
    CLEAR: ws_saida_sub, ws_lfa1, ws_kna1, ws_vbrk, ws_bkpf, ws_j_1bnfdoc, ws_zlest0002, z_kurs2_aux, z_kurs2.
  ENDLOOP.

ENDFORM.
