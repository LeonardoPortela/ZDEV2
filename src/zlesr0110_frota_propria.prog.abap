*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Autor......: Rogério Filipsick                                       *
* Data.......: 08/08/2019                                              *
* Descrição  : Relatório Frota própria - carregamentos                 *
* Transação..: ZLES0148                                                *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
REPORT zlesr0110_frota_propria.

*----------------------------------------------------------------------*
* Tabelas -------------------------------------------------------------*
*----------------------------------------------------------------------*
TABLES: zlest0061.

*----------------------------------------------------------------------*
* Tipos ---------------------------------------------------------------*
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_zlest0039,
         docnum         TYPE zlest0039-docnum,
         nfenum         TYPE zlest0039-nfenum,
         serie          TYPE zlest0039-serie,
         vbeln          TYPE zlest0039-vbeln,
         bukrs          TYPE zlest0039-bukrs,
         werks          TYPE zlest0039-werks,
         pesosaida      TYPE zlest0039-pesosaida,
         matnr          TYPE zlest0039-matnr,
         placa_cav      TYPE zlest0039-placa_cav,
         chave_nfe      TYPE zlest0039-chave_nfe,
         transb_efetivo TYPE zlest0039-transb_efetivo,
         datasaida      TYPE zlest0039-datasaida,
       END OF ty_zlest0039,

       BEGIN OF ty_vttp,
         tknum TYPE vttp-tknum,
         tpnum TYPE vttp-tpnum,
         vbeln TYPE vttp-vbeln,
       END OF ty_vttp,

       BEGIN OF ty_vtpa,
         vbeln TYPE vtpa-vbeln,
         posnr TYPE vtpa-posnr,
         parvw TYPE vtpa-parvw,
         lifnr TYPE vtpa-lifnr,
         adrnr TYPE vtpa-adrnr,
         kunnr TYPE vtpa-kunnr,
       END OF ty_vtpa,

       BEGIN OF ty_vttk,
         tknum TYPE vttk-tknum,
         vsart TYPE vttk-vsart,
         text1 TYPE vttk-text1,
       END OF ty_vttk,

       BEGIN OF ty_adrc,
         addrnumber TYPE adrc-addrnumber,
         date_from  TYPE adrc-date_from,
         nation     TYPE adrc-nation,
         name1      TYPE adrc-name1,
         city1      TYPE adrc-city1,
         region     TYPE adrc-region,
       END OF ty_adrc,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         ort01 TYPE kna1-ort01,
         regio TYPE kna1-regio,
       END OF ty_kna1,

       BEGIN OF ty_vbak,
         vbeln TYPE vbak-vbeln,
         tknum TYPE vbak-tknum,
       END OF ty_vbak,

       BEGIN OF ty_vbfa,
         vbelv   TYPE vbfa-vbelv,
         posnv   TYPE vbfa-posnv,
         vbeln   TYPE vbfa-vbeln,
         posnn   TYPE vbfa-posnn,
         vbtyp_n TYPE vbfa-vbtyp_n,
         erdat   TYPE vbfa-erdat,
         refkey  TYPE j_1bnflin-refkey,
       END OF ty_vbfa,

       BEGIN OF ty_j_1bnflin,
         docnum     TYPE j_1bnflin-docnum,
         itmnum     TYPE j_1bnflin-itmnum,
         refkey     TYPE j_1bnflin-refkey,
         reftyp     TYPE j_1bnflin-reftyp,
         refitm     TYPE j_1bnflin-refitm,
         cnpj_bupla TYPE j_1bnfdoc-cnpj_bupla,
         meins      TYPE j_1bnflin-meins,
       END OF ty_j_1bnflin,

       BEGIN OF ty_j_1bnfdoc,
         docnum     TYPE j_1bnfdoc-docnum,
         cancel     TYPE j_1bnfdoc-cancel,
         chadat     TYPE j_1bnfdoc-chadat,
         nfenum     TYPE j_1bnfdoc-nfenum,
         series     TYPE j_1bnfdoc-series,
         nftot      TYPE j_1bnfdoc-nftot,
         bukrs      TYPE j_1bnfdoc-bukrs,
         branch     TYPE j_1bnfdoc-branch,
         parid      TYPE j_1bnfdoc-parid,
         cnpj_bupla TYPE j_1bnfdoc-cnpj_bupla,
       END OF ty_j_1bnfdoc,

       BEGIN OF ty_zcte_ciot,
         cd_ciot          TYPE zcte_ciot-cd_ciot,
         docnum           TYPE zcte_ciot-docnum,
         vlr_pedagio      TYPE zcte_ciot-vlr_pedagio,
         vlr_impostos     TYPE zcte_ciot-vlr_impostos,
         vlr_frete        TYPE zcte_ciot-vlr_frete,
         vlr_adiantamento TYPE zcte_ciot-vlr_adiantamento,
         vlr_triagem      TYPE zcte_ciot-vlr_triagem,
         vlr_saldo        TYPE zcte_ciot-vlr_saldo,
         vlr_seguro       TYPE zcte_ciot-vlr_seguro,
       END OF ty_zcte_ciot,

       BEGIN OF ty_zlest0143,
         cd_averbacao  TYPE zlest0143-cd_averbacao,
         docnum        TYPE zlest0143-docnum,
         cd_seguradora TYPE zlest0143-cd_seguradora,
         cd_token      TYPE zlest0143-cd_token,
         tp_documento  TYPE zlest0143-tp_documento,
         dt_cadastro   TYPE zlest0143-dt_cadastro,
         hr_cadastro   TYPE zlest0143-hr_cadastro,
         us_cadastro   TYPE zlest0143-us_cadastro,
         nr_averbacao  TYPE zlest0143-nr_averbacao,
         nr_protocolo  TYPE zlest0143-nr_protocolo,
       END OF ty_zlest0143,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
         meins TYPE mara-meins,
       END OF ty_mara,

       BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
         spras TYPE makt-spras,
       END OF ty_makt,

       BEGIN OF ty_likp,
         vbeln TYPE likp-vbeln,
         erdat TYPE likp-erdat,
         lfart TYPE likp-lfart,
         vbtyp TYPE likp-vbtyp,
         fkarv TYPE likp-fkarv,
         btgew TYPE likp-btgew,
         lifex TYPE likp-lifex,
         lifnr TYPE likp-lifnr,
       END OF ty_likp,

       BEGIN OF ty_lips,
         vbeln TYPE lips-vbeln,
         posnr TYPE lips-posnr,
         matnr TYPE lips-matnr,
         matkl TYPE lips-matkl,
         werks TYPE lips-werks,
         meins TYPE lips-meins,
       END OF ty_lips,

       BEGIN OF ty_j_1bbranch,
         bukrs  TYPE j_1bbranch-bukrs,
         branch TYPE j_1bbranch-branch,
       END OF ty_j_1bbranch,

       BEGIN OF ty_zcte_info_nota,
         nfe         TYPE zcte_info_nota-nfe,
         modelo      TYPE zcte_info_nota-modelo,
         serie       TYPE zcte_info_nota-serie,
         numero      TYPE zcte_info_nota-numero,
         cliente     TYPE zcte_info_nota-cliente,
         docnum      TYPE zcte_info_nota-docnum,
         chave       TYPE zcte_info_nota-chave,
         vl_produtos TYPE zcte_info_nota-vl_produtos,
       END OF ty_zcte_info_nota.

TYPES: BEGIN OF ty_report.
         INCLUDE TYPE zfrota_propia.
       TYPES: END OF ty_report.

*----------------------------------------------------------------------*
* Tabelas internas ----------------------------------------------------*
*----------------------------------------------------------------------*
DATA: t_zlest0039      TYPE TABLE OF ty_zlest0039,
      t_vttp           TYPE TABLE OF ty_vttp,
      t_vtpa           TYPE TABLE OF ty_vtpa,
      t_vttk           TYPE TABLE OF ty_vttk,
      t_adrc           TYPE TABLE OF ty_adrc,
      t_kna1           TYPE TABLE OF ty_kna1,
      t_vbak           TYPE TABLE OF ty_vbak,
      t_vbfa           TYPE TABLE OF ty_vbfa,
      t_j_1bnflin      TYPE TABLE OF ty_j_1bnflin,
      t_j_1bnfdoc      TYPE TABLE OF ty_j_1bnfdoc,
      t_j_1bnfdoc_vlr  TYPE TABLE OF ty_j_1bnfdoc,
      t_j_1bnfdoc_nfe  TYPE TABLE OF ty_j_1bnfdoc,
      t_j_1bnfdoc_cte  TYPE TABLE OF ty_j_1bnfdoc,
      t_zcte_ciot      TYPE TABLE OF ty_zcte_ciot,
      t_zlest0143      TYPE TABLE OF ty_zlest0143,
      t_zcte_info_nota TYPE TABLE OF ty_zcte_info_nota,
      t_mara           TYPE TABLE OF ty_mara,
      t_makt           TYPE TABLE OF ty_makt,
      t_t023t          TYPE TABLE OF t023t,
      t_likp           TYPE TABLE OF ty_likp,
      t_zsdt0001       TYPE TABLE OF zsdt0001,
      tl_j_1bnfdoc     TYPE TABLE OF j_1bnfdoc,
      t_lips           TYPE TABLE OF ty_lips,
      t_j_1bbranch     TYPE TABLE OF ty_j_1bbranch,
      t_report         TYPE TABLE OF ty_report,
      t_report_s       TYPE TABLE OF zde_zlest0143_alv_sint,
      t_fieldcat       TYPE   slis_t_fieldcat_alv,
      git_fcat         TYPE lvc_t_fcat,
      wa_saida_sint    TYPE zde_zlest0143_alv_sint,
      it_saida_sint    TYPE TABLE OF zde_zlest0143_alv_sint.

DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,
      gs_variant                  TYPE disvariant,
      gs_variant_s                TYPE disvariant,

      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,
      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid.
*----------------------------------------------------------------------*
* Estruturas ----------------------------------------------------------*
*----------------------------------------------------------------------*
DATA: wa_layout  TYPE slis_layout_alv,
      lv_program TYPE sy-repid.


" Classe
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA:  event_receiver   TYPE REF TO lcl_event_receiver.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:

      hotspot_click
                  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no .

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD hotspot_click.
    CASE e_column_id.
      WHEN 'STATUS_PROCESSAMENTO'.

    ENDCASE.

    CALL METHOD gob_gui_alv_grid->refresh_table_display.

  ENDMETHOD.

ENDCLASS.
*----------------------------------------------------------------------*
* Ranges --------------------------------------------------------------*
*----------------------------------------------------------------------*
DATA:
  r_bukrs  TYPE RANGE OF j_1bbranch-bukrs,
  wa_bukrs LIKE LINE OF r_bukrs,
  r_werks  TYPE RANGE OF lips-werks,
  wa_werks LIKE LINE OF r_werks.

*----------------------------------------------------------------------*
* Constantes ----------------------------------------------------------*
*----------------------------------------------------------------------*
CONSTANTS: c_s        TYPE string VALUE 'S',
           c_p        TYPE string VALUE 'P',
           c_i        TYPE string VALUE 'I',
           c_j        TYPE string VALUE 'J',
           c_eq       TYPE string VALUE 'EQ',
           c_7        TYPE string VALUE '7',
           c_01       TYPE string VALUE '01',
           c_55       TYPE string VALUE '55',
           c_pc       TYPE string VALUE 'PC',
           c_lr       TYPE string VALUE 'LR',
           c_pv       TYPE string VALUE 'PV',
           c_bi       TYPE string VALUE 'BI',
           c_el       TYPE string VALUE 'EL',
           c_zlf      TYPE string VALUE 'ZLF',
           c_00010    TYPE string VALUE '00010',
           c_000010   TYPE string VALUE '000010',
           c_00000000 TYPE string VALUE '00000000'.

*----------------------------------------------------------------------*
* Variaveis -----------------------------------------------------------*
*----------------------------------------------------------------------*
DATA: v_tabix TYPE sy-tabix.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-004.
PARAMETER ck_anil RADIOBUTTON GROUP tpp DEFAULT 'X' USER-COMMAND muda_tela.
PARAMETER ck_sint RADIOBUTTON GROUP tpp.
SELECTION-SCREEN: END OF BLOCK b2.

*----------------------------------------------------------------------*
* Tela de seleção -----------------------------------------------------*
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_data FOR zlest0061-dt_fatura OBLIGATORY.
*                s_werks FOR zlest0061-werks ,
*                s_bukrs FOR zlest0061-bukrs .\

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* START-OF-SELECTION --------------------------------------------------*
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Seleção principal de  dados  para  saídas próprias
  PERFORM f_seleciona_dados_1.

  PERFORM f_monta_dados_1.

* Seleção principal de  dados  para  entradas
  PERFORM f_seleciona_dados_2.

  PERFORM f_monta_dados_2.

* Seleção principal de  dados  fretes prestado a terceiros
  PERFORM f_seleciona_dados_3.

  PERFORM f_monta_dados_3.

  IF ck_sint IS NOT INITIAL.
    PERFORM f_monta_dados_sint.
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION ----------------------------------------------------*
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF t_report[] IS NOT INITIAL.
    CALL SCREEN 0100.
*    PERFORM f_monta_saida.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS_1
*&---------------------------------------------------------------------*
*   Seleção principal de  dados  para  saídas próprias
*----------------------------------------------------------------------*
FORM f_seleciona_dados_1.

* Seleção principal de  dados  para  saídas próprias
  SELECT docnum nfenum serie vbeln bukrs werks pesosaida
         matnr placa_cav chave_nfe transb_efetivo datasaida
    FROM zlest0039
    INTO TABLE t_zlest0039
   WHERE datasaida IN s_data.
*     AND werks IN s_werks
*     AND bukrs IN s_bukrs.

  IF sy-subrc NE 0.
    MESSAGE s000(z_les) WITH text-002 DISPLAY LIKE c_s.
    STOP.
  ENDIF.

  SELECT docnum cancel chadat
         nfenum series nftot
         bukrs branch parid
         cnpj_bupla
    FROM j_1bnfdoc
    INTO TABLE t_j_1bnfdoc_vlr
     FOR ALL ENTRIES IN t_zlest0039
   WHERE docnum = t_zlest0039-docnum.

  SELECT tknum tpnum vbeln
    FROM vttp
    INTO TABLE t_vttp
     FOR ALL ENTRIES IN t_zlest0039
   WHERE vbeln = t_zlest0039-vbeln.

  IF sy-subrc IS INITIAL.

    SELECT tknum vsart text1
      FROM vttk
      INTO TABLE t_vttk
       FOR ALL ENTRIES IN t_vttp
     WHERE tknum = t_vttp-tknum
       AND vsart = c_01.

    IF sy-subrc IS INITIAL.

      LOOP AT t_vttk ASSIGNING FIELD-SYMBOL(<fs_vttk>).
        v_tabix = sy-tabix.

*        TRY.
*            zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
*            EXPORTING
*              i_tknum                =    <fs_vttk>-tknum "<-- Doc. Transporte
*            IMPORTING
*              e_tipo_veiculo         =    DATA(tp_veiculo)
*              e_tipo_remetente       =    DATA(tp_remetente) ).
*          CATCH zcx_faturamento.
*          CATCH zcx_error.
*        ENDTRY.
*
*        IF tp_veiculo NE c_p.
*          DELETE t_vttk INDEX v_tabix.
*        ENDIF.


        DATA(e_placa_cavalo) = <fs_vttk>-text1(07).

        TRY.
            zcl_faturamento=>zif_faturamento~get_instance(
            )->get_tipo_veiculo(
              EXPORTING
                i_placa = e_placa_cavalo
                i_tknum = <fs_vttk>-tknum
              IMPORTING
                e_tipo = DATA(tp_veiculo)
                e_proprietario = DATA(e_proprietario) ).

*              tp_veiculo = e_proprietario.
          CATCH zcx_faturamento .
            tp_veiculo = zif_faturamento=>st_tp_prop_veiculo_terceiro.
          CATCH zcx_error .
            tp_veiculo = zif_faturamento=>st_tp_prop_veiculo_terceiro.
        ENDTRY.

        IF tp_veiculo NE c_p.
          DELETE t_vttk INDEX v_tabix.
        ENDIF.
      ENDLOOP.


      IF t_vttk[] IS NOT INITIAL.

* Dados  do  transportes
        SELECT vbeln posnr parvw
               lifnr adrnr kunnr
          FROM vtpa
          INTO TABLE t_vtpa
           FOR ALL ENTRIES IN t_vttk
         WHERE vbeln = t_vttk-tknum
           AND parvw = c_pc.

        IF sy-subrc IS INITIAL.

          SELECT addrnumber date_from nation name1
                 city1 region
            FROM adrc
            INTO TABLE t_adrc
             FOR ALL ENTRIES IN t_vtpa
           WHERE addrnumber = t_vtpa-adrnr.

        ENDIF.

* Proprietário do veículo
        SELECT vbeln posnr parvw
               lifnr adrnr kunnr
          FROM vtpa
     APPENDING TABLE t_vtpa
           FOR ALL ENTRIES IN t_vttk
         WHERE vbeln = t_vttk-tknum
           AND parvw = c_pv.

* Local de  entrega
        SELECT vbeln posnr parvw
               lifnr adrnr kunnr
          FROM vtpa
     APPENDING TABLE t_vtpa
           FOR ALL ENTRIES IN t_vttk
         WHERE vbeln = t_vttk-tknum
           AND parvw = c_lr.

        IF sy-subrc IS INITIAL.

          SELECT addrnumber date_from nation name1
                 city1 region
            FROM adrc
       APPENDING TABLE t_adrc
             FOR ALL ENTRIES IN t_vtpa
           WHERE addrnumber = t_vtpa-adrnr.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT kunnr name1 ort01 regio
    FROM kna1
    INTO TABLE t_kna1
     FOR ALL ENTRIES IN t_zlest0039
   WHERE kunnr = t_zlest0039-transb_efetivo.

  IF t_vttk[] IS NOT INITIAL.

    SELECT vbeln tknum
      FROM vbak
      INTO TABLE t_vbak
       FOR ALL ENTRIES IN t_vttk
     WHERE tknum = t_vttk-tknum.

    IF sy-subrc IS INITIAL.

      SELECT vbelv posnv vbeln posnn vbtyp_n erdat
        FROM vbfa
        INTO TABLE t_vbfa
         FOR ALL ENTRIES IN t_vbak
       WHERE vbelv = t_vbak-vbeln
         AND posnv = c_000010.

      IF sy-subrc IS INITIAL.
        LOOP AT t_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>).
          <fs_vbfa>-refkey = <fs_vbfa>-vbeln.
        ENDLOOP.

        SELECT docnum itmnum refkey reftyp refitm meins
          FROM j_1bnflin
          INTO CORRESPONDING FIELDS OF TABLE t_j_1bnflin
           FOR ALL ENTRIES IN t_vbfa
         WHERE refkey = t_vbfa-refkey
           AND reftyp = c_bi
           AND refitm = t_vbfa-posnv.

        IF sy-subrc IS INITIAL.

          SELECT docnum cancel chadat
                 nfenum series nftot
                 bukrs branch parid
            FROM j_1bnfdoc
            INTO CORRESPONDING FIELDS OF TABLE t_j_1bnfdoc
             FOR ALL ENTRIES IN t_j_1bnflin
           WHERE docnum = t_j_1bnflin-docnum
             AND cancel = space
*             AND branch IN s_werks
*             AND bukrs  IN s_bukrs
             AND chadat = c_00000000.

          SELECT cd_ciot docnum vlr_pedagio vlr_impostos
                 vlr_frete vlr_adiantamento vlr_triagem
                 vlr_saldo vlr_seguro
            FROM zcte_ciot
            INTO TABLE t_zcte_ciot
             FOR ALL ENTRIES IN t_j_1bnflin
           WHERE docnum = t_j_1bnflin-docnum.

          SELECT cd_averbacao docnum cd_seguradora cd_token
                 tp_documento dt_cadastro hr_cadastro
                 us_cadastro nr_averbacao nr_protocolo
            FROM zlest0143
            INTO TABLE t_zlest0143
             FOR ALL ENTRIES IN t_j_1bnflin
           WHERE docnum = t_j_1bnflin-docnum.


        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


  SELECT matnr matkl meins
    FROM mara
    INTO TABLE t_mara
     FOR ALL ENTRIES IN t_zlest0039
   WHERE matnr = t_zlest0039-matnr.

  SELECT matnr maktx spras
     FROM makt
     INTO TABLE t_makt
      FOR ALL ENTRIES IN t_zlest0039
    WHERE matnr = t_zlest0039-matnr
      AND spras = sy-langu.

  IF t_mara IS NOT INITIAL.
    SELECT *
   FROM t023t
   INTO TABLE t_t023t
    FOR ALL ENTRIES IN t_mara
  WHERE matkl EQ t_mara-matkl
    AND spras EQ sy-langu.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS_1
*&---------------------------------------------------------------------*
*       Monta Dados
*----------------------------------------------------------------------*
FORM f_monta_dados_1.

  DATA: lw_chave  TYPE c LENGTH 44.

  SORT: t_vttk BY tknum,
        t_vttp BY tknum,
        t_vbak BY tknum,
        t_vtpa BY vbeln,
        t_zlest0039 BY docnum,
        t_j_1bnfdoc_vlr BY docnum
  .

  LOOP AT t_vttk ASSIGNING FIELD-SYMBOL(<fs_vttk>).

    APPEND INITIAL LINE TO t_report ASSIGNING FIELD-SYMBOL(<fs_report>).

    READ TABLE t_vttp ASSIGNING FIELD-SYMBOL(<fs_vttp>)
                                    WITH KEY tknum = <fs_vttk>-tknum BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE t_zlest0039 ASSIGNING FIELD-SYMBOL(<fs_zlest0039>)
                                              WITH KEY vbeln = <fs_vttp>-vbeln.
      IF sy-subrc IS INITIAL.
        <fs_report>-data      = <fs_zlest0039>-datasaida.
        <fs_report>-docnum_nf = <fs_zlest0039>-docnum.
        <fs_report>-nr_nf     = <fs_zlest0039>-nfenum.
        <fs_report>-serie     = <fs_zlest0039>-serie.

        CLEAR: lw_chave.
        CALL METHOD zcl_util=>monta_chave_nfe
          EXPORTING
            i_docnum = <fs_zlest0039>-docnum
            i_valida = 'X'
          RECEIVING
            e_chave  = lw_chave.

        <fs_report>-chave_nfe = lw_chave.
        <fs_report>-bukrs     = <fs_zlest0039>-bukrs.
        <fs_report>-filial    = <fs_zlest0039>-werks.
        <fs_report>-peso      = <fs_zlest0039>-pesosaida.
        <fs_report>-material  = <fs_zlest0039>-matnr.
        <fs_report>-placa_cav = <fs_zlest0039>-placa_cav(7).

        READ TABLE t_j_1bnfdoc_vlr ASSIGNING FIELD-SYMBOL(<fs_j_1bnfdoc_vlr>)
                                                 WITH KEY docnum = <fs_zlest0039>-docnum.
        IF sy-subrc IS INITIAL.
          <fs_report>-vlr_mercadoria = <fs_j_1bnfdoc_vlr>-nftot.
        ENDIF.
      ENDIF.
    ENDIF.


* Proprietário do veículo
    READ TABLE t_vtpa ASSIGNING FIELD-SYMBOL(<fs_vtpa_pv>)
                                    WITH KEY vbeln = <fs_vttk>-tknum
                                             parvw = c_pv .
    IF sy-subrc IS INITIAL.
      <fs_report>-proprietario = <fs_vtpa_pv>-lifnr.
    ENDIF.


* PC
    READ TABLE t_vtpa ASSIGNING FIELD-SYMBOL(<fs_vtpa_pc>)
                                    WITH KEY vbeln = <fs_vttk>-tknum
                                             parvw = c_pc.
    IF sy-subrc IS INITIAL.

      <fs_report>-ponto_coleta = <fs_vtpa_pc>-lifnr.

      READ TABLE t_adrc ASSIGNING FIELD-SYMBOL(<fs_adrc_pc>)
                                      WITH KEY addrnumber = <fs_vtpa_pc>-adrnr.

      IF sy-subrc IS INITIAL.
        <fs_report>-descr_pc  = <fs_adrc_pc>-name1.
        <fs_report>-cidade_pc = <fs_adrc_pc>-city1.
        <fs_report>-uf_pc     = <fs_adrc_pc>-region.
      ENDIF.
    ENDIF.

* LR
    IF <fs_zlest0039>-transb_efetivo IS NOT INITIAL.

      READ TABLE t_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1_lr>)
                                        WITH KEY kunnr = <fs_zlest0039>-transb_efetivo.
      IF sy-subrc IS INITIAL.
        <fs_report>-local_entrega = <fs_zlest0039>-transb_efetivo.
        <fs_report>-descr_lr      = <fs_kna1_lr>-name1.
        <fs_report>-cidade_lr     = <fs_kna1_lr>-ort01.
        <fs_report>-uf_lr         = <fs_kna1_lr>-regio.
      ENDIF.

    ELSE.


      READ TABLE t_vtpa ASSIGNING FIELD-SYMBOL(<fs_vtpa_lr>)
                                      WITH KEY vbeln = <fs_vttk>-tknum
                                               parvw = c_lr.
      IF sy-subrc IS INITIAL.

        <fs_report>-local_entrega = <fs_vtpa_lr>-kunnr.

        READ TABLE t_adrc ASSIGNING FIELD-SYMBOL(<fs_adrc_lr>)
                                        WITH KEY addrnumber = <fs_vtpa_lr>-adrnr.

        IF sy-subrc IS INITIAL.
          <fs_report>-descr_lr  = <fs_adrc_lr>-name1.
          <fs_report>-cidade_lr = <fs_adrc_lr>-city1.
          <fs_report>-uf_lr     = <fs_adrc_lr>-region.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE t_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
                                    WITH KEY tknum = <fs_vttk>-tknum.

    IF sy-subrc IS INITIAL.

      READ TABLE t_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>)
                                      WITH KEY vbelv = <fs_vbak>-vbeln.

      IF sy-subrc IS INITIAL.

        READ TABLE t_j_1bnflin ASSIGNING FIELD-SYMBOL(<fs_j_1bnflin>)
                                             WITH KEY refkey = <fs_vbfa>-refkey
                                                       refitm = <fs_vbfa>-posnv.
        IF sy-subrc IS INITIAL.

          READ TABLE t_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_j_1bnfdoc>)
                                               WITH KEY docnum = <fs_j_1bnflin>-docnum.
          IF sy-subrc IS INITIAL.

            CLEAR: lw_chave.
            CALL METHOD zcl_util=>monta_chave_nfe
              EXPORTING
                i_docnum = <fs_j_1bnfdoc>-docnum
                i_valida = 'X'
              RECEIVING
                e_chave  = lw_chave.

            <fs_report>-docnum_cte     = <fs_j_1bnfdoc>-docnum.
            <fs_report>-cnpj_bupla     = <fs_j_1bnfdoc>-cnpj_bupla.
            <fs_report>-nr_cte         = <fs_j_1bnfdoc>-nfenum.
            <fs_report>-serie_cte      = <fs_j_1bnfdoc>-series.
*            <FS_REPORT>-VLR_MERCADORIA = <FS_J_1BNFDOC>-NFTOT.
            <fs_report>-chave_cte      = lw_chave.
          ENDIF.

          READ TABLE t_zcte_ciot ASSIGNING FIELD-SYMBOL(<fs_zcte_ciot>)
                                               WITH KEY docnum = <fs_j_1bnflin>-docnum.
          IF sy-subrc IS INITIAL.
            <fs_report>-vlr_pedagio  = <fs_zcte_ciot>-vlr_pedagio.
            <fs_report>-vlr_impostos = <fs_zcte_ciot>-vlr_impostos.
            <fs_report>-vlr_frete    = <fs_zcte_ciot>-vlr_frete.
            <fs_report>-vlr_adto     = <fs_zcte_ciot>-vlr_adiantamento.
            <fs_report>-vlr_triagem  = <fs_zcte_ciot>-vlr_triagem.
            <fs_report>-vlr_saldo    = <fs_zcte_ciot>-vlr_saldo.
            <fs_report>-vlr_seguro   = <fs_zcte_ciot>-vlr_seguro.
          ENDIF.

          READ TABLE t_zlest0143 ASSIGNING FIELD-SYMBOL(<fs_zlest0143>)
                                               WITH KEY docnum = <fs_j_1bnflin>-docnum.
          IF sy-subrc IS INITIAL.
            <fs_report>-cod_averbacao      = <fs_zlest0143>-cd_averbacao.
            <fs_report>-cod_seguradora     = <fs_zlest0143>-cd_seguradora.
            <fs_report>-cod_token          = <fs_zlest0143>-cd_token.
            <fs_report>-tp_doc             = <fs_zlest0143>-tp_documento.
            <fs_report>-dt_averb           = <fs_zlest0143>-dt_cadastro.
            <fs_report>-hora_averb         = <fs_zlest0143>-hr_cadastro.
            <fs_report>-usuario_averb      = <fs_zlest0143>-us_cadastro.
            <fs_report>-nr_averbacao       = <fs_zlest0143>-nr_averbacao.
            <fs_report>-nr_protocolo_averb = <fs_zlest0143>-nr_protocolo.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE t_makt ASSIGNING FIELD-SYMBOL(<fs_makt>)
                                    WITH KEY matnr = <fs_zlest0039>-matnr.
    IF sy-subrc IS INITIAL.
      <fs_report>-desc_material = <fs_makt>-maktx.
    ENDIF.

    READ TABLE t_mara ASSIGNING FIELD-SYMBOL(<fs_mara>)
                                    WITH KEY matnr = <fs_zlest0039>-matnr.
    IF sy-subrc IS INITIAL.
      <fs_report>-unidade     = <fs_mara>-meins.
      <fs_report>-gp_material = <fs_mara>-matkl.
      <fs_report>-gp_material = <fs_mara>-matkl.
    ENDIF.

    READ TABLE t_t023t ASSIGNING FIELD-SYMBOL(<t023t>) WITH KEY matkl = <fs_mara>-matkl.
    IF sy-subrc IS INITIAL.
      <fs_report>-wgbez = <t023t>-wgbez.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS_2
*&---------------------------------------------------------------------*
*       Seleção principal de  dados  para  entradas
*----------------------------------------------------------------------*
FORM f_seleciona_dados_2 .

  PERFORM f_limpa_tabelas.

  DATA: lt_result TYPE match_result_tab,
        wa_result TYPE match_result,
        l_lines   TYPE i,
        r_nfenum  TYPE RANGE OF j_1bnfdoc-nfenum,
        wa_nfenum LIKE LINE OF r_nfenum,
        r_series  TYPE RANGE OF j_1bnfdoc-series,
        wa_series LIKE LINE OF r_series,
*        tp_veiculo TYPE char01,
        r_docnum  TYPE RANGE OF docnum.
  DATA zcl_faturamento TYPE REF TO zcl_faturamento.
  CREATE OBJECT zcl_faturamento.


  SELECT vbeln erdat lfart vbtyp fkarv btgew lifex lifnr
    FROM likp
    INTO TABLE t_likp
   WHERE erdat IN s_data
     AND lfart = c_el
     AND vbtyp = c_7.
*     AND werks IN s_werks.


**======================================================== Inicio ajuste BUG70197 - 25/01/2022 - Anderson Oenning
  IF sy-subrc IS INITIAL.

    FREE: t_zsdt0001.
    SELECT * FROM zsdt0001 INTO TABLE t_zsdt0001
       FOR ALL ENTRIES IN t_likp
       WHERE doc_aviso EQ t_likp-vbeln.
*         AND bukrs IN s_bukrs.
    IF t_zsdt0001 IS NOT INITIAL.
      "Seleção de dados documento exportação.
*      r_docnum = VALUE #( FOR l IN t_zsdt0001 ( sign   = c_i option = c_eq low = l-nro_nf_rem ) ).
*      IF r_docnum IS NOT INITIAL.
      SELECT *
      FROM j_1bnfdoc
      INTO TABLE tl_j_1bnfdoc
        FOR ALL ENTRIES IN t_zsdt0001
      WHERE docnum EQ t_zsdt0001-nro_nf_rem.
*      ENDIF.
    ENDIF.
**======================================================== Fim ajuste BUG70197 - 25/01/2022 - Anderson Oenning


    LOOP AT t_likp ASSIGNING FIELD-SYMBOL(<fs_likp>).

      CLEAR: lt_result, wa_result, l_lines, wa_nfenum, wa_series.
      FIND ALL OCCURRENCES OF '-' IN <fs_likp>-lifex RESULTS lt_result.
      DESCRIBE TABLE lt_result LINES l_lines.
      READ TABLE lt_result INTO wa_result INDEX l_lines.

* os  números  antes  do  traços
      IF <fs_likp>-lifex IS NOT INITIAL.

        wa_nfenum-sign   = c_i.
        wa_nfenum-option = c_eq.
*        WA_NFENUM-LOW    = <FS_LIKP>-LIFEX(WA_RESULT-OFFSET).

        TRY .
            wa_nfenum-low    = <fs_likp>-lifex(wa_result-offset).
          CATCH cx_root.
            CONTINUE.
        ENDTRY.


        IF wa_nfenum-low IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_nfenum-low
            IMPORTING
              output = wa_nfenum-low.

          APPEND wa_nfenum TO r_nfenum.
          CLEAR  wa_nfenum.
        ENDIF.

* o que  estiver  após o traço
        wa_result-offset = wa_result-offset + 1.
        wa_series-sign   = c_i.
        wa_series-option = c_eq.
        wa_series-low = <fs_likp>-lifex+wa_result-offset.

        IF wa_series-low IS NOT INITIAL.
          APPEND wa_series TO r_series.
          CLEAR  wa_series.
        ENDIF.

      ENDIF.
    ENDLOOP.


    SELECT vbeln posnr matnr matkl werks meins
      FROM lips
      INTO TABLE t_lips
       FOR ALL ENTRIES IN t_likp
     WHERE vbeln = t_likp-vbeln
*       AND werks IN s_werks
       AND posnr = c_00010.

    IF sy-subrc IS INITIAL.

      LOOP AT t_lips ASSIGNING FIELD-SYMBOL(<fs_lips>).
        wa_werks-sign   = c_i.
        wa_werks-option = c_eq.
        wa_werks-low    = <fs_lips>-werks.
        APPEND wa_werks TO r_werks.
        CLEAR  wa_werks.
      ENDLOOP.

      SELECT matnr maktx spras
        FROM makt
        INTO TABLE t_makt
         FOR ALL ENTRIES IN t_lips
       WHERE matnr = t_lips-matnr
         AND spras = sy-langu.

      FREE: t_mara.
      SELECT matnr matkl meins
      FROM mara
      INTO TABLE t_mara
      FOR ALL ENTRIES IN t_lips
      WHERE matnr = t_lips-matnr.


      IF t_mara IS NOT INITIAL.
        SELECT *
       FROM t023t
       INTO TABLE t_t023t
        FOR ALL ENTRIES IN t_mara
      WHERE matkl EQ t_mara-matkl
        AND spras EQ sy-langu.

      ENDIF.

      SELECT bukrs branch
        FROM j_1bbranch
        INTO TABLE t_j_1bbranch
         FOR ALL ENTRIES IN t_lips
       WHERE branch  = t_lips-werks.

      LOOP AT t_j_1bbranch ASSIGNING FIELD-SYMBOL(<fs_j_1bbranch>).
        wa_bukrs-sign   = c_i.
        wa_bukrs-option = c_eq.
        wa_bukrs-low    = <fs_j_1bbranch>-bukrs.
        APPEND wa_bukrs TO r_bukrs.
        CLEAR  wa_bukrs.
      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM r_bukrs.
      DELETE ADJACENT DUPLICATES FROM r_werks.
      DELETE ADJACENT DUPLICATES FROM r_nfenum.
      DELETE ADJACENT DUPLICATES FROM r_series.

      DATA(r_burk)  = r_bukrs.
      DATA(r_werk)  = r_werks.
      DATA(r_nfen) = r_nfenum.
      DATA(r_seri)  = r_series.
      DATA(tl_likp) = t_likp.



      SORT: r_burk    BY low,
            r_werk    BY low,
            r_nfen    BY low,
            r_seri    BY low,
            tl_likp   BY lifnr.

      DELETE ADJACENT DUPLICATES FROM r_burk   COMPARING low.
      DELETE ADJACENT DUPLICATES FROM r_werk   COMPARING low.
      DELETE ADJACENT DUPLICATES FROM r_nfen COMPARING low.
      DELETE ADJACENT DUPLICATES FROM r_seri   COMPARING low.
      DELETE ADJACENT DUPLICATES FROM tl_likp COMPARING lifnr.

      SELECT docnum cancel chadat
             nfenum series nftot
             a~bukrs a~branch parid
        INTO TABLE t_j_1bnfdoc_nfe
        FROM j_1bnfdoc AS a
         FOR ALL ENTRIES IN tl_likp
       WHERE parid  EQ tl_likp-lifnr
         AND series IN r_seri
         AND nfenum IN r_nfen
         AND bukrs  IN r_burk
         AND branch IN r_werk
         AND cancel = space.
*         AND chadat = c_00000000.

*      IF sy-subrc EQ 0.
*        IF r_bukrs IS NOT INITIAL.
*          DELETE t_j_1bnfdoc_nfe WHERE bukrs NOT IN r_bukrs.
*        ENDIF.
*
*        IF r_werks IS NOT INITIAL.
*          DELETE t_j_1bnfdoc_nfe WHERE branch NOT IN r_werks.
*        ENDIF.
*      ENDIF.

    ENDIF.


    SELECT tknum tpnum vbeln
      FROM vttp
      INTO TABLE t_vttp
       FOR ALL ENTRIES IN t_likp
     WHERE vbeln = t_likp-vbeln.

    IF sy-subrc IS INITIAL.

      SELECT tknum vsart text1
        FROM vttk
        INTO TABLE t_vttk
         FOR ALL ENTRIES IN t_vttp
       WHERE tknum = t_vttp-tknum
         AND vsart = c_01.

      IF sy-subrc IS INITIAL.
*
        LOOP AT t_vttk ASSIGNING FIELD-SYMBOL(<fs_vttk>).
          v_tabix = sy-tabix.
*          TRY.
*              zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
*              EXPORTING
*                i_tknum                =    <fs_vttk>-tknum "<-- Doc. Transporte
*              IMPORTING
*                e_tipo_veiculo         =    DATA(tp_veiculo)
*                e_tipo_remetente       =    DATA(tp_remetente) ).
*            CATCH zcx_faturamento.
*            CATCH zcx_error.
*          ENDTRY.

          """ Tipo do Veículo """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          DATA(e_placa_cavalo) = <fs_vttk>-text1(07).

          TRY.
              zcl_faturamento=>zif_faturamento~get_instance(
              )->get_tipo_veiculo(
                EXPORTING
                  i_placa = e_placa_cavalo
                  i_tknum = <fs_vttk>-tknum
                IMPORTING
                  e_tipo = DATA(tp_veiculo)
                  e_proprietario = DATA(e_proprietario) ).

*              tp_veiculo = e_proprietario.
            CATCH zcx_faturamento .
              tp_veiculo = zif_faturamento=>st_tp_prop_veiculo_terceiro.
            CATCH zcx_error .
              tp_veiculo = zif_faturamento=>st_tp_prop_veiculo_terceiro.
          ENDTRY.

          IF tp_veiculo NE c_p.
            DELETE t_vttk INDEX v_tabix.
          ENDIF.
        ENDLOOP.

        IF t_vttk[] IS NOT INITIAL.

* Ponto de coleta
          SELECT vbeln posnr parvw
                 lifnr adrnr kunnr
            FROM vtpa
            INTO TABLE t_vtpa
             FOR ALL ENTRIES IN t_vttk
           WHERE vbeln = t_vttk-tknum
             AND parvw = c_pc.

          IF sy-subrc IS INITIAL.

            SELECT addrnumber date_from nation name1
                   city1 region
              FROM adrc
              INTO TABLE t_adrc
               FOR ALL ENTRIES IN t_vtpa
             WHERE addrnumber = t_vtpa-adrnr.

          ENDIF.

* Proprietário do veículo
          SELECT vbeln posnr parvw
                 lifnr adrnr kunnr
            FROM vtpa
       APPENDING TABLE t_vtpa
             FOR ALL ENTRIES IN t_vttk
           WHERE vbeln = t_vttk-tknum
             AND parvw = c_pv.

* Local de  entrega
          SELECT vbeln posnr parvw
                 lifnr adrnr kunnr
            FROM vtpa
            APPENDING TABLE t_vtpa
             FOR ALL ENTRIES IN t_vttk
           WHERE vbeln = t_vttk-tknum
             AND parvw = c_lr.

          IF sy-subrc IS INITIAL.
            SELECT addrnumber date_from nation name1
                   city1 region
              FROM adrc
              APPENDING TABLE t_adrc
               FOR ALL ENTRIES IN t_vtpa
             WHERE addrnumber = t_vtpa-adrnr.
          ENDIF.


          SELECT vbeln tknum
            FROM vbak
            INTO TABLE t_vbak
             FOR ALL ENTRIES IN t_vttk
           WHERE tknum = t_vttk-tknum.

          IF sy-subrc IS INITIAL.

            SELECT vbelv posnv vbeln posnn vbtyp_n erdat
              FROM vbfa
              INTO TABLE t_vbfa
               FOR ALL ENTRIES IN t_vbak
             WHERE vbelv = t_vbak-vbeln
               AND posnv = c_000010.

            IF sy-subrc IS INITIAL.
              LOOP AT t_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>).
                <fs_vbfa>-refkey = <fs_vbfa>-vbeln.
              ENDLOOP.

              SELECT docnum itmnum refkey reftyp refitm meins
                FROM j_1bnflin
                INTO CORRESPONDING FIELDS OF TABLE t_j_1bnflin
                 FOR ALL ENTRIES IN t_vbfa
               WHERE refkey = t_vbfa-refkey
                 AND reftyp = c_bi
                 AND refitm = t_vbfa-posnv.

              IF sy-subrc IS INITIAL.

                SELECT docnum cancel chadat
                       nfenum series nftot
                       bukrs branch parid
                  FROM j_1bnfdoc
                  INTO TABLE t_j_1bnfdoc_cte
                   FOR ALL ENTRIES IN t_j_1bnflin
                 WHERE docnum = t_j_1bnflin-docnum
*                   AND branch IN s_werks
*                   AND bukrs  IN s_bukrs
                   AND cancel = space
                   AND chadat = c_00000000.

                SELECT cd_ciot docnum vlr_pedagio vlr_impostos
                       vlr_frete vlr_adiantamento vlr_triagem
                       vlr_saldo vlr_seguro
                  FROM zcte_ciot
                  INTO TABLE t_zcte_ciot
                   FOR ALL ENTRIES IN t_j_1bnflin
                 WHERE docnum = t_j_1bnflin-docnum.

                SELECT cd_averbacao docnum cd_seguradora cd_token
                       tp_documento dt_cadastro hr_cadastro
                       us_cadastro nr_averbacao nr_protocolo
                  FROM zlest0143
                  INTO TABLE t_zlest0143
                   FOR ALL ENTRIES IN t_j_1bnflin
                 WHERE docnum = t_j_1bnflin-docnum.

              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_monta_dados_2 .

  DATA: lw_chave  TYPE c LENGTH 44.

  DATA: lt_result TYPE match_result_tab,
        wa_result TYPE match_result,
        l_lines   TYPE i,
        l_nfenum  TYPE j_1bnfdoc-nfenum,
        l_series  TYPE j_1bnfdoc-series,
        wa_active TYPE j_1bnfe_active.

  SORT: t_vttk BY tknum,
        t_vttp BY tknum,
        t_vbak BY tknum,
        t_vtpa BY vbeln,
        t_lips BY vbeln,
        t_likp BY vbeln,
        t_zsdt0001 BY vbeln,
        tl_j_1bnfdoc BY docnum,
        t_j_1bnflin BY docnum,
        t_j_1bnfdoc_cte BY docnum,
        t_zcte_ciot BY docnum,
        t_zlest0143 BY docnum.

  LOOP AT t_vttk ASSIGNING FIELD-SYMBOL(<fs_vttk>).

    APPEND INITIAL LINE TO t_report ASSIGNING FIELD-SYMBOL(<fs_report>).

    READ TABLE t_vttp ASSIGNING FIELD-SYMBOL(<fs_vttp>)
                                    WITH KEY tknum = <fs_vttk>-tknum BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      READ TABLE t_likp ASSIGNING FIELD-SYMBOL(<fs_likp>)
                                      WITH KEY vbeln = <fs_vttp>-vbeln.
      IF sy-subrc IS INITIAL.

        <fs_report>-data = <fs_likp>-erdat.
        <fs_report>-peso = <fs_likp>-btgew.

        IF <fs_likp>-lifex IS NOT INITIAL.
          CLEAR: lt_result, wa_result, l_lines, l_series, l_nfenum, lw_chave.
          FIND ALL OCCURRENCES OF '-' IN <fs_likp>-lifex RESULTS lt_result.
          DESCRIBE TABLE lt_result LINES l_lines.
          READ TABLE lt_result INTO wa_result INDEX l_lines.
          IF wa_result-offset NE 0.
            l_nfenum = <fs_likp>-lifex(wa_result-offset).
          ELSE.
            l_nfenum = <fs_likp>-lifex(5).
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_nfenum
            IMPORTING
              output = l_nfenum.

          wa_result-offset = wa_result-offset + 1.
          l_series = <fs_likp>-lifex+wa_result-offset.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <fs_likp> IS ASSIGNED.
      READ TABLE t_lips ASSIGNING FIELD-SYMBOL(<fs_lips>)
                                      WITH KEY vbeln = <fs_likp>-vbeln.
      IF sy-subrc IS INITIAL.
        <fs_report>-unidade      = <fs_lips>-meins.
        <fs_report>-filial      = <fs_lips>-werks.
        <fs_report>-material    = <fs_lips>-matnr.
        <fs_report>-gp_material = <fs_lips>-matkl.

        READ TABLE t_makt ASSIGNING FIELD-SYMBOL(<fs_makt>)
                                        WITH KEY matnr = <fs_lips>-matnr.
        IF sy-subrc IS INITIAL.
          <fs_report>-desc_material = <fs_makt>-maktx.
        ENDIF.

        READ TABLE t_t023t ASSIGNING FIELD-SYMBOL(<t023t>)
                                       WITH KEY matkl = <fs_report>-gp_material.
        IF sy-subrc IS INITIAL.
          <fs_report>-wgbez = <t023t>-wgbez.
        ENDIF.

        READ TABLE t_j_1bbranch ASSIGNING FIELD-SYMBOL(<fs_j_1bbranch>)
                                              WITH KEY branch = <fs_lips>-werks.
        IF sy-subrc IS INITIAL.
          <fs_report>-bukrs = <fs_j_1bbranch>-bukrs.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <fs_lips> IS ASSIGNED.
      IF <fs_likp> IS ASSIGNED.
        IF <fs_j_1bbranch> IS ASSIGNED.
          READ TABLE t_j_1bnfdoc_nfe ASSIGNING FIELD-SYMBOL(<fs_j_1bnfdoc_nfe>)
                                                   WITH KEY parid  = <fs_likp>-lifnr
                                                            bukrs  = <fs_j_1bbranch>-bukrs
                                                            branch = <fs_lips>-werks
                                                            nfenum = l_nfenum
                                                            series = l_series.
          IF sy-subrc IS INITIAL.

            CLEAR: lw_chave.
            CALL METHOD zcl_util=>monta_chave_nfe
              EXPORTING
                i_docnum = <fs_j_1bnfdoc_nfe>-docnum
                i_valida = 'X'
              RECEIVING
                e_chave  = lw_chave.

            <fs_report>-cnpj_bupla   = <fs_j_1bnfdoc_nfe>-cnpj_bupla.
            <fs_report>-docnum_nf    = <fs_j_1bnfdoc_nfe>-docnum.
            <fs_report>-nr_nf        = <fs_j_1bnfdoc_nfe>-nfenum.
            <fs_report>-serie        = <fs_j_1bnfdoc_nfe>-series.
            <fs_report>-vlr_mercadoria = <fs_j_1bnfdoc_nfe>-nftot.
            <fs_report>-chave_nfe = lw_chave.
          ELSE.
**======================================================== Inicio ajuste BUG70197 - 25/01/2022 - Anderson Oenning
            READ TABLE t_zsdt0001 INTO DATA(ws_zsdt0001) WITH KEY doc_aviso = <fs_likp>-vbeln.
            IF sy-subrc EQ 0.
              CLEAR: lw_chave.
              CALL METHOD zcl_util=>monta_chave_nfe
                EXPORTING
                  i_docnum = ws_zsdt0001-nro_nf_rem
                  i_valida = 'X'
                RECEIVING
                  e_chave  = lw_chave.

              CLEAR: wa_active.

              <fs_report>-chave_nfe = lw_chave.
            ENDIF.
            READ TABLE tl_j_1bnfdoc INTO DATA(ws_doc) WITH KEY docnum = ws_zsdt0001-nro_nf_rem.
            IF sy-subrc EQ 0.
              <fs_report>-docnum_nf = ws_doc-docnum.
              <fs_report>-nr_nf     = ws_doc-nfenum.
              <fs_report>-serie     = ws_doc-series.
              <fs_report>-vlr_mercadoria = ws_doc-nftot.
              <fs_report>-cnpj_bupla   = ws_doc-cnpj_bupla.
            ENDIF.
          ENDIF.
**======================================================== Fim ajuste BUG70197 - 25/01/2022 - Anderson Oenning
        ENDIF.
      ENDIF.
    ENDIF.


    <fs_report>-placa_cav = <fs_vttk>-text1(7).

* Proprietário do veículo
    READ TABLE t_vtpa ASSIGNING FIELD-SYMBOL(<fs_vtpa_pv>)
                                    WITH KEY vbeln = <fs_vttk>-tknum
                                             parvw = c_pv .
    IF sy-subrc IS INITIAL.
      <fs_report>-proprietario = <fs_vtpa_pv>-lifnr.
    ENDIF.

* PC
    READ TABLE t_vtpa ASSIGNING FIELD-SYMBOL(<fs_vtpa_pc>)
                                    WITH KEY vbeln = <fs_vttk>-tknum
                                             parvw = c_pc .
    IF sy-subrc IS INITIAL.
      <fs_report>-ponto_coleta = <fs_vtpa_pc>-lifnr.

      READ TABLE t_adrc ASSIGNING FIELD-SYMBOL(<fs_adrc_pc>)
                                      WITH KEY addrnumber = <fs_vtpa_pc>-adrnr.
      IF sy-subrc IS INITIAL.
        <fs_report>-descr_pc  = <fs_adrc_pc>-name1.
        <fs_report>-cidade_pc = <fs_adrc_pc>-city1.
        <fs_report>-uf_pc     = <fs_adrc_pc>-region.
      ENDIF.
    ENDIF.

* LR
    READ TABLE t_vtpa ASSIGNING FIELD-SYMBOL(<fs_vtpa_lr>)
                                       WITH KEY vbeln = <fs_vttk>-tknum
                                                parvw = c_lr .
    IF sy-subrc IS INITIAL.
      <fs_report>-local_entrega  = <fs_vtpa_lr>-kunnr.

      READ TABLE t_adrc ASSIGNING FIELD-SYMBOL(<fs_adrc_lr>)
                                      WITH KEY addrnumber = <fs_vtpa_lr>-adrnr.
      IF sy-subrc IS INITIAL.
        <fs_report>-descr_lr  = <fs_adrc_lr>-name1.
        <fs_report>-cidade_lr = <fs_adrc_lr>-city1.
        <fs_report>-uf_lr     = <fs_adrc_lr>-region.
      ENDIF.
    ENDIF.

    READ TABLE t_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
                                     WITH KEY tknum = <fs_vttk>-tknum .
    IF sy-subrc IS INITIAL.

      READ TABLE t_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>)
                                      WITH KEY vbelv = <fs_vbak>-vbeln.

      IF sy-subrc IS INITIAL.

        READ TABLE t_j_1bnflin ASSIGNING FIELD-SYMBOL(<fs_j_1bnflin>)
                                        WITH KEY refkey = <fs_vbfa>-refkey
                                                 refitm = <fs_vbfa>-posnv.
        IF sy-subrc IS INITIAL.
          <fs_report>-unidade      = <fs_j_1bnflin>-meins.
          READ TABLE t_j_1bnfdoc_cte ASSIGNING FIELD-SYMBOL(<fs_j_1bnfdoc_cte>)
                                                   WITH KEY docnum = <fs_j_1bnflin>-docnum .
          IF sy-subrc IS INITIAL.

            CLEAR: lw_chave.
            CALL METHOD zcl_util=>monta_chave_nfe
              EXPORTING
                i_docnum = <fs_j_1bnfdoc_cte>-docnum
                i_valida = 'X'
              RECEIVING
                e_chave  = lw_chave.

            <fs_report>-docnum_cte = <fs_j_1bnfdoc_cte>-docnum.
            <fs_report>-nr_cte     = <fs_j_1bnfdoc_cte>-nfenum.
            <fs_report>-serie_cte  = <fs_j_1bnfdoc_cte>-series.
            <fs_report>-chave_cte  = lw_chave.
          ENDIF.


          READ TABLE t_zcte_ciot ASSIGNING FIELD-SYMBOL(<fs_zcte_ciot>)
                                               WITH KEY docnum = <fs_j_1bnflin>-docnum .
          IF sy-subrc IS INITIAL.
            <fs_report>-vlr_pedagio  = <fs_zcte_ciot>-vlr_pedagio.
            <fs_report>-vlr_impostos = <fs_zcte_ciot>-vlr_impostos.
            <fs_report>-vlr_frete    = <fs_zcte_ciot>-vlr_frete.
            <fs_report>-vlr_adto     = <fs_zcte_ciot>-vlr_adiantamento.
            <fs_report>-vlr_triagem  = <fs_zcte_ciot>-vlr_triagem.
            <fs_report>-vlr_saldo    = <fs_zcte_ciot>-vlr_saldo.
            <fs_report>-vlr_seguro   = <fs_zcte_ciot>-vlr_seguro.
          ENDIF.

          READ TABLE t_zlest0143 ASSIGNING FIELD-SYMBOL(<fs_zlest0143>)
                                               WITH KEY docnum = <fs_j_1bnflin>-docnum.
          IF sy-subrc IS INITIAL.
            <fs_report>-cod_averbacao      = <fs_zlest0143>-cd_averbacao.
            <fs_report>-cod_seguradora     = <fs_zlest0143>-cd_seguradora.
            <fs_report>-cod_token          = <fs_zlest0143>-cd_token.
            <fs_report>-tp_doc             = <fs_zlest0143>-tp_documento.
            <fs_report>-dt_averb           = <fs_zlest0143>-dt_cadastro.
            <fs_report>-hora_averb         = <fs_zlest0143>-hr_cadastro.
            <fs_report>-usuario_averb      = <fs_zlest0143>-us_cadastro.
            <fs_report>-nr_averbacao       = <fs_zlest0143>-nr_averbacao.
            <fs_report>-nr_protocolo_averb = <fs_zlest0143>-nr_protocolo.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

*  IF s_bukrs IS NOT INITIAL AND t_report IS NOT INITIAL.
*    DELETE t_report WHERE bukrs NOT IN s_bukrs.
*  ENDIF.
*
*  IF s_werks IS NOT INITIAL AND t_report IS NOT INITIAL.
*    DELETE t_report WHERE filial NOT IN s_werks.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS_3
*&---------------------------------------------------------------------*
*       Seleção principal de  dados  fretes prestado a terceiros
*----------------------------------------------------------------------*
FORM f_seleciona_dados_3 .

  PERFORM f_limpa_tabelas.

  SELECT vbeln erdat lfart vbtyp
         fkarv btgew lifex lifnr
      FROM likp
      INTO TABLE t_likp
     WHERE erdat IN s_data
       AND lfart = c_zlf
       AND vbtyp = c_j
*       AND werks IN s_werks
       AND fkarv = space.

  IF sy-subrc IS INITIAL.

    SELECT vbeln posnr matnr matkl werks
      FROM lips
      INTO TABLE t_lips
       FOR ALL ENTRIES IN t_likp
     WHERE vbeln = t_likp-vbeln
       AND posnr = c_00010.

    IF sy-subrc IS INITIAL.

      SELECT matnr maktx spras
        FROM makt
        INTO TABLE t_makt
         FOR ALL ENTRIES IN t_lips
       WHERE matnr = t_lips-matnr
         AND spras = sy-langu.

      SELECT bukrs branch
        FROM j_1bbranch
        INTO TABLE t_j_1bbranch
         FOR ALL ENTRIES IN t_lips
       WHERE branch  = t_lips-werks.

      SELECT *
   FROM t023t
   INTO TABLE t_t023t
    FOR ALL ENTRIES IN t_lips
  WHERE matkl EQ t_lips-matkl
    AND spras EQ sy-langu.

    ENDIF.

    SELECT tknum tpnum vbeln
      FROM vttp
      INTO TABLE t_vttp
       FOR ALL ENTRIES IN t_likp
     WHERE vbeln = t_likp-vbeln.

    IF sy-subrc IS INITIAL.

      SELECT tknum vsart text1
        FROM vttk
        INTO TABLE t_vttk
         FOR ALL ENTRIES IN t_vttp
       WHERE tknum = t_vttp-tknum
         AND vsart = c_01.

      LOOP AT t_vttk ASSIGNING FIELD-SYMBOL(<fs_vttk>).
        v_tabix = sy-tabix.
*        TRY.
*            zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
*            EXPORTING
*              i_tknum                =    <fs_vttk>-tknum "<-- Doc. Transporte
*            IMPORTING
*              e_tipo_veiculo         =    DATA(tp_veiculo)
*              e_tipo_remetente       =    DATA(tp_remetente) ).
*          CATCH zcx_faturamento.
*          CATCH zcx_error.
*        ENDTRY.

        DATA(e_placa_cavalo) = <fs_vttk>-text1(07).

        TRY.
            zcl_faturamento=>zif_faturamento~get_instance(
            )->get_tipo_veiculo(
              EXPORTING
                i_placa = e_placa_cavalo
                i_tknum = <fs_vttk>-tknum
              IMPORTING
                e_tipo = DATA(tp_veiculo)
                e_proprietario = DATA(e_proprietario) ).

*              tp_veiculo = e_proprietario.
          CATCH zcx_faturamento .
            tp_veiculo = zif_faturamento=>st_tp_prop_veiculo_terceiro.
          CATCH zcx_error .
            tp_veiculo = zif_faturamento=>st_tp_prop_veiculo_terceiro.
        ENDTRY.

        IF tp_veiculo NE c_p.
          DELETE t_vttk INDEX v_tabix.
        ENDIF.
      ENDLOOP.

      IF t_vttk[] IS NOT INITIAL.

        SELECT vbeln posnr parvw
               lifnr adrnr kunnr
          FROM vtpa
          INTO TABLE t_vtpa
           FOR ALL ENTRIES IN t_vttk
         WHERE vbeln = t_vttk-tknum
           AND parvw = c_pc.

        IF sy-subrc IS INITIAL.

          SELECT addrnumber date_from nation name1
                 city1 region
            FROM adrc
            INTO TABLE t_adrc
             FOR ALL ENTRIES IN t_vtpa
           WHERE addrnumber = t_vtpa-adrnr.

        ENDIF.

* Proprietário do veículo
        SELECT vbeln posnr parvw
               lifnr adrnr kunnr
          FROM vtpa
     APPENDING TABLE t_vtpa
           FOR ALL ENTRIES IN t_vttk
         WHERE vbeln = t_vttk-tknum
           AND parvw = c_pv.

* Local de  entrega
        SELECT vbeln posnr parvw
               lifnr adrnr kunnr
          FROM vtpa
     APPENDING TABLE t_vtpa
           FOR ALL ENTRIES IN t_vttk
         WHERE vbeln = t_vttk-tknum
           AND parvw = c_lr.

        IF sy-subrc IS INITIAL.
          SELECT addrnumber date_from nation name1
                 city1 region
            FROM adrc
       APPENDING TABLE t_adrc
             FOR ALL ENTRIES IN t_vtpa
           WHERE addrnumber = t_vtpa-adrnr.
        ENDIF.

        SELECT vbeln tknum
          FROM vbak
          INTO TABLE t_vbak
           FOR ALL ENTRIES IN t_vttk
         WHERE tknum = t_vttk-tknum.

        IF sy-subrc IS INITIAL.

          SELECT vbelv posnv vbeln posnn vbtyp_n erdat
            FROM vbfa
            INTO TABLE t_vbfa
             FOR ALL ENTRIES IN t_vbak
           WHERE vbelv = t_vbak-vbeln
             AND posnv = c_000010.

          IF sy-subrc IS INITIAL.
            LOOP AT t_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>).
              <fs_vbfa>-refkey = <fs_vbfa>-vbeln.
            ENDLOOP.

            SELECT docnum itmnum refkey reftyp refitm meins
              FROM j_1bnflin
              INTO CORRESPONDING FIELDS OF TABLE t_j_1bnflin
               FOR ALL ENTRIES IN t_vbfa
             WHERE refkey = t_vbfa-refkey
               AND reftyp = c_bi
               AND refitm = t_vbfa-posnv.

            IF sy-subrc IS INITIAL.

              SELECT docnum cancel chadat
                    nfenum series nftot
                    bukrs branch parid cnpj_bupla
               FROM j_1bnfdoc
               INTO TABLE t_j_1bnfdoc
                FOR ALL ENTRIES IN t_j_1bnflin
              WHERE docnum = t_j_1bnflin-docnum
                AND cancel = space
                AND chadat = c_00000000.

              SELECT cd_ciot docnum vlr_pedagio vlr_impostos
                     vlr_frete vlr_adiantamento vlr_triagem
                     vlr_saldo vlr_seguro
                FROM zcte_ciot
                INTO TABLE t_zcte_ciot
                 FOR ALL ENTRIES IN t_j_1bnflin
               WHERE docnum = t_j_1bnflin-docnum.

              SELECT cd_averbacao docnum cd_seguradora cd_token
                     tp_documento dt_cadastro hr_cadastro
                     us_cadastro nr_averbacao nr_protocolo
                FROM zlest0143
                INTO TABLE t_zlest0143
                 FOR ALL ENTRIES IN t_j_1bnflin
               WHERE docnum = t_j_1bnflin-docnum.

              SELECT nfe modelo serie numero cliente docnum
                     chave vl_produtos
                FROM zcte_info_nota
                INTO TABLE t_zcte_info_nota
                 FOR ALL ENTRIES IN t_j_1bnflin
               WHERE docnum = t_j_1bnflin-docnum
                 AND nfe    = abap_true
                 AND modelo = c_55.

              IF sy-subrc NE 0.

                SELECT nfe modelo serie numero cliente docnum
                       chave vl_produtos
                  FROM zcte_info_nota
                  INTO TABLE t_zcte_info_nota
                   FOR ALL ENTRIES IN t_j_1bnflin
                 WHERE docnum = t_j_1bnflin-docnum
                   AND nfe    = space
                   AND modelo = c_01.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.


      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_monta_dados_3 .

  DATA: lw_chave  TYPE c LENGTH 44.

  DATA: lt_result TYPE match_result_tab,
        wa_result TYPE match_result,
        l_lines   TYPE i,
        l_nfenum  TYPE j_1bnfdoc-nfenum,
        l_series  TYPE j_1bnfdoc-series.

  SORT: t_vttk BY tknum,
       t_vttp BY tknum,
       t_vbak BY tknum,
       t_vtpa BY vbeln,
       t_lips BY vbeln,
       t_likp BY vbeln,
       t_j_1bnflin BY docnum,
       t_zcte_ciot BY docnum,
       t_zcte_info_nota BY docnum,
       t_zlest0039 BY docnum,
       t_zlest0143 BY docnum,
       t_j_1bnfdoc BY docnum.

  LOOP AT t_vttk ASSIGNING FIELD-SYMBOL(<fs_vttk>).

    APPEND INITIAL LINE TO t_report ASSIGNING FIELD-SYMBOL(<fs_report>).

    READ TABLE t_vttp ASSIGNING FIELD-SYMBOL(<fs_vttp>)
                                    WITH KEY tknum = <fs_vttk>-tknum.

    IF sy-subrc IS INITIAL.
      READ TABLE t_likp ASSIGNING FIELD-SYMBOL(<fs_likp>)
                                      WITH KEY vbeln = <fs_vttp>-vbeln.

      IF sy-subrc IS INITIAL.
        <fs_report>-data = <fs_likp>-erdat.
        <fs_report>-peso = <fs_likp>-btgew.

        IF <fs_likp>-lifex IS NOT INITIAL.
          CLEAR: lt_result, wa_result, l_lines, l_series, l_nfenum, lw_chave.
          FIND ALL OCCURRENCES OF '-' IN <fs_likp>-lifex RESULTS lt_result.
          DESCRIBE TABLE lt_result LINES l_lines.
          READ TABLE lt_result INTO wa_result INDEX l_lines.
          l_nfenum = <fs_likp>-lifex(wa_result-offset).

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_nfenum
            IMPORTING
              output = l_nfenum.

          wa_result-offset = wa_result-offset + 1.
          l_series = <fs_likp>-lifex+wa_result-offset.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <fs_likp> IS ASSIGNED.
      READ TABLE t_lips ASSIGNING FIELD-SYMBOL(<fs_lips>)
                                      WITH KEY vbeln = <fs_likp>-vbeln.
      IF sy-subrc IS INITIAL.
        <fs_report>-unidade      = <fs_lips>-meins.
        <fs_report>-filial      = <fs_lips>-werks.
        <fs_report>-material    = <fs_lips>-matnr.
        <fs_report>-gp_material = <fs_lips>-matkl.

        READ TABLE t_makt ASSIGNING FIELD-SYMBOL(<fs_makt>)
                                        WITH KEY matnr = <fs_lips>-matnr.
        IF sy-subrc IS INITIAL.
          <fs_report>-desc_material = <fs_makt>-maktx.
        ENDIF.

        READ TABLE t_t023t ASSIGNING FIELD-SYMBOL(<ws_t023t>) WITH KEY matkl = <fs_report>-gp_material.
        IF sy-subrc EQ 0.
          <fs_report>-wgbez = <ws_t023t>-wgbez.

        ENDIF.

        READ TABLE t_j_1bbranch ASSIGNING FIELD-SYMBOL(<fs_j_1bbranch>)
                                              WITH KEY branch = <fs_lips>-werks.
        IF sy-subrc IS INITIAL.
          <fs_report>-bukrs = <fs_j_1bbranch>-bukrs.
        ENDIF.
      ENDIF.
    ENDIF.


    <fs_report>-placa_cav = <fs_vttk>-text1(7).

* Proprietário do veículo
    READ TABLE t_vtpa ASSIGNING FIELD-SYMBOL(<fs_vtpa_pv>)
                                    WITH KEY vbeln = <fs_vttk>-tknum
                                             parvw = c_pv .
    IF sy-subrc IS INITIAL.
      <fs_report>-proprietario = <fs_vtpa_pv>-lifnr.
    ENDIF.

* PC
    READ TABLE t_vtpa ASSIGNING FIELD-SYMBOL(<fs_vtpa_pc>)
                                    WITH KEY vbeln = <fs_vttk>-tknum
                                             parvw = c_pc.
    IF sy-subrc IS INITIAL.
      <fs_report>-ponto_coleta = <fs_vtpa_pc>-lifnr.

      READ TABLE t_adrc ASSIGNING FIELD-SYMBOL(<fs_adrc_pc>)
                                      WITH KEY addrnumber = <fs_vtpa_pc>-adrnr.
      IF sy-subrc IS INITIAL.
        <fs_report>-descr_pc  = <fs_adrc_pc>-name1.
        <fs_report>-cidade_pc = <fs_adrc_pc>-city1.
        <fs_report>-uf_pc     = <fs_adrc_pc>-region.
      ENDIF.
    ENDIF.

* LR
    READ TABLE t_vtpa ASSIGNING FIELD-SYMBOL(<fs_vtpa_lr>)
                                       WITH KEY vbeln = <fs_vttk>-tknum
                                                parvw = c_lr.
    IF sy-subrc IS INITIAL.
      <fs_report>-local_entrega  = <fs_vtpa_lr>-kunnr.

      READ TABLE t_adrc ASSIGNING FIELD-SYMBOL(<fs_adrc_lr>)
                                      WITH KEY addrnumber = <fs_vtpa_lr>-adrnr.
      IF sy-subrc IS INITIAL.
        <fs_report>-descr_lr  = <fs_adrc_lr>-name1.
        <fs_report>-cidade_lr = <fs_adrc_lr>-city1.
        <fs_report>-uf_lr     = <fs_adrc_lr>-region.
      ENDIF.
    ENDIF.

    READ TABLE t_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
                                     WITH KEY tknum = <fs_vttk>-tknum.
    IF sy-subrc IS INITIAL.

      READ TABLE t_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>)
                                      WITH KEY vbelv = <fs_vbak>-vbeln.

      IF sy-subrc IS INITIAL.

        READ TABLE t_j_1bnflin ASSIGNING FIELD-SYMBOL(<fs_j_1bnflin>)
                                        WITH KEY refkey = <fs_vbfa>-refkey
                                                 refitm = <fs_vbfa>-posnv.
        IF sy-subrc IS INITIAL.

*          <FS_REPORT>-DOCNUM_NF = <FS_J_1BNFLIN>-DOCNUM.  "Docnum NF
          <fs_report>-unidade = <fs_j_1bnflin>-meins.

          READ TABLE t_zcte_info_nota ASSIGNING FIELD-SYMBOL(<fs_zcte_info_nota>)
                                                    WITH KEY docnum = <fs_j_1bnflin>-docnum .
          IF sy-subrc IS INITIAL.
            <fs_report>-chave_nfe      = <fs_zcte_info_nota>-chave.
            <fs_report>-vlr_mercadoria = <fs_zcte_info_nota>-vl_produtos.
            <fs_report>-nr_nf          = <fs_zcte_info_nota>-numero.
            <fs_report>-serie          = <fs_zcte_info_nota>-serie.
          ENDIF.

          READ TABLE t_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_j_1bnfdoc_cte>)
                                               WITH KEY docnum = <fs_j_1bnflin>-docnum .

          IF sy-subrc IS INITIAL.
            CLEAR: lw_chave.
            CALL METHOD zcl_util=>monta_chave_nfe
              EXPORTING
                i_docnum = <fs_j_1bnfdoc_cte>-docnum
                i_valida = 'X'
              RECEIVING
                e_chave  = lw_chave.

            <fs_report>-cnpj_bupla   = <fs_j_1bnfdoc_cte>-cnpj_bupla.
            <fs_report>-docnum_cte   = <fs_j_1bnfdoc_cte>-docnum. "Docnum CTE
            <fs_report>-nr_cte       = <fs_j_1bnfdoc_cte>-nfenum.
            <fs_report>-serie_cte    = <fs_j_1bnfdoc_cte>-series.
            <fs_report>-chave_cte    = lw_chave.
            <fs_report>-cnpj_bupla   = <fs_j_1bnfdoc_cte>-cnpj_bupla.
          ENDIF.

          READ TABLE t_zcte_ciot ASSIGNING FIELD-SYMBOL(<fs_zcte_ciot>)
                                               WITH KEY docnum = <fs_j_1bnflin>-docnum .
          IF sy-subrc IS INITIAL.
            <fs_report>-vlr_pedagio  = <fs_zcte_ciot>-vlr_pedagio.
            <fs_report>-vlr_impostos = <fs_zcte_ciot>-vlr_impostos.
            <fs_report>-vlr_frete    = <fs_zcte_ciot>-vlr_frete.
            <fs_report>-vlr_adto     = <fs_zcte_ciot>-vlr_adiantamento.
            <fs_report>-vlr_triagem  = <fs_zcte_ciot>-vlr_triagem.
            <fs_report>-vlr_saldo    = <fs_zcte_ciot>-vlr_saldo.
            <fs_report>-vlr_seguro   = <fs_zcte_ciot>-vlr_seguro.
          ENDIF.

          READ TABLE t_zlest0143 ASSIGNING FIELD-SYMBOL(<fs_zlest0143>)
                                               WITH KEY docnum = <fs_j_1bnflin>-docnum .
          IF sy-subrc IS INITIAL.
            <fs_report>-cod_averbacao      = <fs_zlest0143>-cd_averbacao.
            <fs_report>-cod_seguradora     = <fs_zlest0143>-cd_seguradora.
            <fs_report>-cod_token          = <fs_zlest0143>-cd_token.
            <fs_report>-tp_doc             = <fs_zlest0143>-tp_documento.
            <fs_report>-dt_averb           = <fs_zlest0143>-dt_cadastro.
            <fs_report>-hora_averb         = <fs_zlest0143>-hr_cadastro.
            <fs_report>-usuario_averb      = <fs_zlest0143>-us_cadastro.
            <fs_report>-nr_averbacao       = <fs_zlest0143>-nr_averbacao.
            <fs_report>-nr_protocolo_averb = <fs_zlest0143>-nr_protocolo.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

*  IF s_bukrs IS NOT INITIAL AND t_report IS NOT INITIAL.
*    DELETE t_report WHERE bukrs NOT IN s_bukrs.
*  ENDIF.
*
*  IF s_werks IS NOT INITIAL AND t_report IS NOT INITIAL.
*    DELETE t_report WHERE filial NOT IN s_werks.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_SAIDA
*&---------------------------------------------------------------------*
*       Monta Saída
*----------------------------------------------------------------------*
FORM f_monta_saida .
  DATA: lva_data(22) TYPE c.
  CLEAR: wa_layout.
  FREE: t_fieldcat.
  lv_program  = sy-repid.

  IF ck_sint IS INITIAL.

    "Sintetico.
    PERFORM fill_it_fieldcatalog_0001.
    PERFORM fill_gs_variant_0001.

    lv_program  = sy-repid.
    wa_layout-expand_all = abap_true.
    wa_layout-colwidth_optimize = abap_true.

    IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
     EXPORTING
        i_titulo  = 'Relatório Frota Propria'
        i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
      CHANGING
        alv = gob_gui_alv_grid
      )
      EQ abap_true.


      CREATE OBJECT event_receiver.
      SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.


      CALL METHOD gob_gui_alv_grid->set_table_for_first_display
        CHANGING
          it_outtab                     = t_report
          it_fieldcatalog               = git_fcat
*         IT_SORT                       =
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

    ENDIF.




*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*      EXPORTING
*        is_variant         = gs_variant
**        i_callback_program = lv_program
*        is_layout          = wa_layout
*        it_fieldcat        = t_fieldcat
*      TABLES
*        t_outtab           = t_report
*      EXCEPTIONS
*        program_error      = 1
*        OTHERS             = 2.

  ELSE.
    PERFORM fill_it_fieldcatalog_0002.
    PERFORM fill_gs_variant_0001_s.

    wa_layout-expand_all = abap_true.
*    wa_layout-colwidth_optimize = abap_true.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       is_variant         = gs_variant_s
        i_callback_program = lv_program
        is_layout          = wa_layout
        it_fieldcat        = t_fieldcat
      TABLES
        t_outtab           = it_saida_sint
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_TABELAS
*&---------------------------------------------------------------------*
*       Limpa Tabelas
*----------------------------------------------------------------------*
FORM f_limpa_tabelas.

  CLEAR: t_zlest0039, t_vttp, t_vtpa, t_vttk, t_adrc,
         t_kna1, t_vbak, t_vbfa, t_j_1bnflin, t_j_1bnfdoc,
         t_j_1bnfdoc_nfe, t_j_1bnfdoc_cte,
         t_zcte_ciot, t_zlest0143, t_zcte_info_nota,
         t_mara, t_makt, t_likp, t_lips, t_j_1bbranch.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS_SINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_dados_sint .

  DATA: vg_quantidade TYPE p DECIMALS 0,
        qtd_viag      TYPE p.

  CHECK t_report IS NOT INITIAL.
  SORT t_report BY bukrs filial gp_material unidade.
  DATA(t_report_aux) = t_report.

  DELETE ADJACENT DUPLICATES FROM t_report_aux COMPARING bukrs filial gp_material unidade.

  IF t_report_aux[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_t023t)
      FROM t023t
       FOR ALL ENTRIES IN @t_report_aux
     WHERE spras EQ @sy-langu
       AND matkl EQ @t_report_aux-gp_material.

  ENDIF.


  LOOP AT t_report_aux INTO DATA(ws_report_aux).

    LOOP AT t_report INTO DATA(ws_rep) WHERE bukrs        EQ ws_report_aux-bukrs
                                         AND  filial      EQ ws_report_aux-filial
                                         AND  gp_material EQ ws_report_aux-gp_material
                                         AND  unidade     EQ ws_report_aux-unidade.

      ADD 1 TO wa_saida_sint-ztot_viag.

      CLEAR: vg_quantidade.
*      IF ws_rep-unidade EQ 'TO'.
*        ws_rep-unidade = 'KG'.
*        vg_quantidade = ( ws_rep-peso * 1000 ). "Converter em kg
*      ELSE.
      vg_quantidade = ws_rep-peso.
*      ENDIF.

      ADD vg_quantidade TO wa_saida_sint-ztot_v_transp.
      ADD ws_rep-vlr_mercadoria TO wa_saida_sint-zvlor_total.
    ENDLOOP.

    CONDENSE wa_saida_sint-ztot_viag.

    wa_saida_sint-bukrs = ws_report_aux-bukrs.
    wa_saida_sint-unidade = ws_report_aux-unidade.
    wa_saida_sint-branch = ws_report_aux-filial.
    wa_saida_sint-matkl = ws_report_aux-gp_material.

    READ TABLE it_t023t INTO DATA(wa_t023t) WITH KEY matkl = ws_report_aux-gp_material.
    IF sy-subrc IS INITIAL.
      wa_saida_sint-wgbez = wa_t023t-wgbez.
    ENDIF.

    APPEND wa_saida_sint TO it_saida_sint.
    CLEAR: qtd_viag,  wa_saida_sint, vg_quantidade, wa_t023t.
  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0002 .


  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = lv_program
      i_structure_name       = 'ZDE_ZLEST0143_ALV_SINT'
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.


  LOOP AT t_fieldcat ASSIGNING FIELD-SYMBOL(<l_fcat>).
    IF <l_fcat>-fieldname EQ 'ZTOT_VIAG' OR <l_fcat>-fieldname EQ 'ZTOT_V_TRANSP' OR <l_fcat>-fieldname EQ 'ZVLOR_TOTAL'.
      <l_fcat>-outputlen   = 30.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0001_S
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0001_s .

  CLEAR: gs_variant_s.
  gs_variant_s-report      = sy-repid.
  gs_variant_s-handle      = '0001'.
  gs_variant_s-log_group   = abap_false.
  gs_variant_s-username    = abap_false.
  gs_variant_s-variant     = abap_false.
  gs_variant_s-text        = abap_false.
  gs_variant_s-dependvars  = abap_false.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0001 .
  CLEAR: gs_variant.
  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0001'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0001 .
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = lv_program
      i_structure_name       = 'ZFROTA_PROPIA'
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  MOVE-CORRESPONDING t_fieldcat TO git_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'TL0001'.

  IF ck_anil IS NOT INITIAL.
    PERFORM fm_alv_01.
  ELSE.
    PERFORM fm_alv_02.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_ALV_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_alv_01 .

  DATA: lva_data(22) TYPE c.
  FREE: git_fcat.
  CLEAR: gs_variant.
  PERFORM fm_cria_fieldcat.
  PERFORM fill_gs_variant_0001.

  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Relatório Frota Propria Analitico'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


*    CREATE OBJECT event_receiver.
*    SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.


    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = gs_variant
*       is_layout                     = wa_layout
      CHANGING
        it_outtab                     = t_report
        it_fieldcatalog               = git_fcat
*       IT_SORT                       =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat .

  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.
  git_fcat = VALUE lit_fieldcat_aux(
 ( fieldname ='DATA                '               coltext = 'Data                                  '                  col_opt = 'X' no_zero = '' hotspot = '' )
 ( fieldname ='DOCNUM_NF           '               coltext = 'Docnum NF                             '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='CNPJ_BUPLA          '               coltext = 'CNPJ do emitente                      '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='NR_NF               '               coltext = 'Nr. Nf                                '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='SERIE               '               coltext = 'Serie                                 '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='CHAVE_NFE           '               coltext = 'Chave NF-e                            '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='BUKRS               '               coltext = 'Empresa                               '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='FILIAL              '               coltext = 'Filial                                '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='PESO                '               coltext = 'Peso                                  '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='MATERIAL            '               coltext = 'Nº do material                        '                  col_opt = 'X' no_zero = 'X' )
 ( fieldname ='DESC_MATERIAL       '               coltext = 'Desc. Material                        '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='GP_MATERIAL         '               coltext = 'Gp. Material                          '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='UNIDADE             '               coltext = 'Unidade de medida básica              '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='WGBEZ               '               coltext = 'Denominação do grupo de mercadorias   '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='PLACA_CAV           '               coltext = 'Placa do Veículo                      '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='PROPRIETARIO        '               coltext = 'Proprietário do veículo               '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='PONTO_COLETA        '               coltext = 'Ponto Coleta                          '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='DESCR_PC            '               coltext = 'Descr. Pc                             '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='CIDADE_PC           '               coltext = 'Cidade PC                             '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='UF_PC               '               coltext = 'UF PC                                 '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='LOCAL_ENTREGA       '               coltext = 'Local Entrega                         '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='DESCR_LR            '               coltext = 'Descr. LR                             '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='CIDADE_LR           '               coltext = 'Cidade LR                             '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='UF_LR               '               coltext = 'UF LR                                 '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='DOCNUM_CTE          '               coltext = 'Documento CT-e                        '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='NR_CTE              '               coltext = 'Nr Cte                                '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='SERIE_CTE           '               coltext = 'Série CT-e                            '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='CHAVE_CTE           '               coltext = 'Chave Cte                             '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='VLR_PEDAGIO         '               coltext = 'Valor Pedágio                         '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='VLR_IMPOSTOS        '               coltext = 'Valor Impostos                        '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='VLR_FRETE           '               coltext = 'Valor Frete                           '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='VLR_ADTO            '               coltext = 'Valor Adiantamento                    '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='VLR_TRIAGEM         '               coltext = 'Valor Triagem                         '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='VLR_SALDO           '               coltext = 'Valor Saldo Frete                     '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='VLR_SEGURO          '               coltext = 'Valor Seguro                          '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='VLR_MERCADORIA      '               coltext = 'Valor Mercadoria                      '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='COD_AVERBACAO       '               coltext = 'Cod. Averbação                        '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='COD_SEGURADORA      '               coltext = 'Código da Seguradora                  '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='COD_TOKEN           '               coltext = 'Código do Token                       '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='TP_DOC              '               coltext = 'Tipo Doc                              '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='DT_AVERB            '               coltext = 'Dt. Averb.                            '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='HORA_AVERB          '               coltext = 'Hora Averb.                           '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='USUARIO_AVERB       '               coltext = 'Usuário Averb.                        '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='NR_AVERBACAO        '               coltext = 'Número da Averbação                   '                  col_opt = 'X' no_zero = '' )
 ( fieldname ='NR_PROTOCOLO_AVERB  '               coltext = 'Nr Protocolo Averb.                   '                  col_opt = 'X' no_zero = '' ) ).



  LOOP AT git_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    IF <ls_fcat>-fieldname EQ 'CNPJ_BUPLA'.
      <ls_fcat>-convexit    = ''.
      <ls_fcat>-domname     = 'CHAR_20'.
      <ls_fcat>-reptext     = 'Cnpj Emissor CTE'.
      <ls_fcat>-outputlen   = 20.
      <ls_fcat>-inttype     = 'C'.
      <ls_fcat>-ref_table   = ''.
      <ls_fcat>-ref_field   = ''.
      <ls_fcat>-datatype    = 'CHAR'.
      <ls_fcat>-intlen      = '000020'.
      <ls_fcat>-coltext     = 'Cnpj Emissor CTE'.
    ENDIF.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_ALV_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_alv_02 .
  DATA: lva_data(22) TYPE c.
  FREE: git_fcat.
  CLEAR: gs_variant.
  PERFORM fm_cria_fieldcat_2.
  PERFORM fill_gs_variant_0001.

  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Relatório Frota Propria - Sintético'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


*    CREATE OBJECT event_receiver.
*    SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.


    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = gs_variant
*       is_layout                     = wa_layout
      CHANGING
        it_outtab                     = it_saida_sint
        it_fieldcatalog               = git_fcat
*       IT_SORT                       =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat_2 .

  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.
  git_fcat = VALUE lit_fieldcat_aux(
( fieldname ='BUKRS           '     coltext = 'Empresa                             '   col_opt = 'X' no_zero = '' hotspot = '' )
( fieldname ='BRANCH          '     coltext = 'Local de negócios                   '   col_opt = 'X' no_zero = '' )
( fieldname ='MATKL           '     coltext = 'Grupo de mercadorias                '   col_opt = 'X' no_zero = '' )
( fieldname ='WGBEZ           '     coltext = 'Denominação do grupo de mercadorias '   col_opt = 'X' no_zero = '' )
( fieldname ='ZTOT_VIAG       '     coltext = 'Total de viagem                     '   col_opt = 'X' no_zero = '' )
( fieldname ='ZTOT_V_TRANSP   '     coltext = 'Quantidade total transportado       '   col_opt = 'X' no_zero = '' )
( fieldname ='UNIDADE         '     coltext = 'Unidade de medida básica            '   col_opt = 'X' no_zero = '' )
( fieldname ='ZVLOR_TOTAL     '     coltext = 'Valor total                         '   col_opt = 'X' no_zero = '' ) ).

ENDFORM.
