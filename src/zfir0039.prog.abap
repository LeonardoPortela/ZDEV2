*&---------------------------------------------------------------------*
*& Report  ZFIR0039
*&
*&---------------------------------------------------------------------*
*&TITULO: VARREDURA DO SACADO – Contas a Receber
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 18.11.2013
*TRANSACAO: ZFI0037
*&---------------------------------------------------------------------*


REPORT  zfir0039.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.

TABLES: regup.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.
DATA: wl_mode(1).

TYPES: BEGIN OF ty_bsad,
         bukrs TYPE bsad_view-bukrs,
         belnr TYPE bsad_view-belnr,
         gjahr TYPE bsad_view-gjahr,
         augbl TYPE bsad_view-augbl,
       END OF ty_bsad,

       BEGIN OF ty_t012k,
         bukrs TYPE t012k-bukrs,
         hbkid TYPE t012k-hbkid,
         hkont TYPE t012k-hkont,
       END OF ty_t012k,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1,

       BEGIN OF ty_zsdt0054,
         adiant     TYPE zsdt0054-adiant,
         nro_sol_ov TYPE zsdt0054-nro_sol_ov,
       END OF ty_zsdt0054,

       BEGIN OF ty_regup_44,
         belnr         TYPE regup-belnr,
         doc_simulacao TYPE zsdt0040-doc_simulacao,
         xblnr         TYPE bkpf-xblnr, "// WBARBOSA US-163166 28/01/2025
       END OF ty_regup_44,

       BEGIN OF ty_bkpf_62,
         belnr TYPE bkpf-belnr,
         awkey TYPE vbrp-vbeln,
       END OF ty_bkpf_62,

       BEGIN OF ty_znfw,
         bukrs   TYPE bkpf-bukrs,
         belnr   TYPE bkpf-belnr,
         obj_key TYPE zfiwrt0008-obj_key,
         "seq_lcto TYPE zfiwrt0008-seq_lcto,
       END OF ty_znfw,

       BEGIN OF ty_saida,
         checkbox(1),
         descricao          TYPE zfit0058-descricao,            "Arq.Retorno
         augbl              TYPE bsad_view-augbl,                    "Compensado
         kunnr              TYPE regup-kunnr,                   "Cliente
         name1              TYPE kna1-name1,                    "Nome do Cliente
         budat              TYPE regup-budat,                   "Dt.Lcto
         belnr              TYPE regup-belnr,                   "Nro.doc.
         xblnr              TYPE regup-xblnr,                   "Nro.Fatura
         nro_sol_ov         TYPE zsdt0054-nro_sol_ov,           "Nro.Sol.Ov.
         hbkid              TYPE bsid-hbkid,                    "Banco Empresa ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
         dt_vct             TYPE regup-budat,                   "Dt.Vcto
         icon_sit(4),                                           "Situação
         dias_atraso        TYPE i,                             "Dias Atraso   = Data Atual - XVCTO
         dmbtr              TYPE regup-dmbtr,                   "Vlr.Titulo
         dt_credito(10),                                        "Dt.Pgto
         vl_liq_creditado   TYPE zfit0057-vl_liq_creditado,     "Vlr.Credito
         vl_tarifa_cobranca TYPE zfit0194-tarifa_cobranca, "*E-commerce - Ajustes transação ZFI0037 #109706 - BG
         vl_jr_mt_enc       TYPE zfit0057-vl_jr_mt_enc,         "Vlr.Jros/Multa/Enc.
         vl_abatimento      TYPE zfit0057-vl_abatimento,        "Vlr.Abat.Conc.
         vl_iof             TYPE zfit0057-vl_iof,               "Vlr.IOF
         gsber              TYPE regup-gsber,                   "Filial
         vl_pago_sacado     TYPE zfit0057-vl_pago_sacado,       "Vlr.Pg.Sacado
         laufi              TYPE regup-laufi,                   "Ident.Arq.
         laufd              TYPE regup-laufd,                   "Dt.Arq.
         bukrs              TYPE regup-bukrs, "SHDB
         waers              TYPE regup-waers, "SHDB
         xref2              TYPE regup-xref2, "ALV
         xref3              TYPE regup-xref3, "SHDB e ALV
         umsks              TYPE regup-umsks, "SHDB
         hkont              TYPE t012k-hkont, "SHDB
         origem(50),   "US - 62755 - CBRAND
         tx_multa           TYPE zfiwrt0011-taxa_multa,  "US - 62755 - CBRAND
         tx_juros           TYPE zfiwrt0011-taxa_juros,  "US - 62755 - CBRAND
         seq_lcto           TYPE zfiwrt0008-seq_lcto ,   "US - 62755 - CBRAND
         style              TYPE lvc_t_styl, "US - 62755 - CBRAND
         spart(23),
         memo(1),
         origem_receb       TYPE zfit0054-origem_receb, "US - 172238 - CBRAND
       END OF ty_saida.


*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: t_usermd        TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
      it_zfit0056     TYPE TABLE OF zfit0056,
      it_zfit0057     TYPE TABLE OF zfit0057,
      it_zfit0058     TYPE TABLE OF zfit0058,
      it_zsdt0054     TYPE TABLE OF ty_zsdt0054,
      it_zsdt0054_aux TYPE TABLE OF zsdt0054,
      it_zsdt0159     TYPE TABLE OF zsdt0159,
      it_zsdt0159_aux TYPE TABLE OF zsdt0159,
      it_bsad         TYPE TABLE OF ty_bsad,
      it_bsid         TYPE TABLE OF bsid_view,
      it_bsid_aux     TYPE TABLE OF bsid_view,
      it_kna1         TYPE TABLE OF ty_kna1,
      it_t012k        TYPE TABLE OF ty_t012k,
      it_regup        TYPE TABLE OF regup,
      it_regup_aux    TYPE TABLE OF regup,
      it_regup_rz     TYPE TABLE OF regup,
      it_regup_44     TYPE TABLE OF ty_regup_44,
      it_zsdt0040     TYPE TABLE OF zsdt0040,
      it_bkpf         TYPE TABLE OF bkpf,
      it_bkpf_62      TYPE TABLE OF ty_bkpf_62,
      it_vbrp         TYPE TABLE OF vbrp,
      it_zsdt0053     TYPE TABLE OF zsdt0053,
      it_zsdt0051     TYPE TABLE OF zsdt0051,
      it_znfw         TYPE TABLE OF ty_znfw,
      it_zfiwrt0008   TYPE TABLE OF zfiwrt0008,
      it_zfiwrt0011   TYPE TABLE OF zfiwrt0011,
      it_zib_cont_chv TYPE TABLE OF zib_contabil_chv,
      it_saida        TYPE TABLE OF ty_saida,
      it_zfit0194     TYPE TABLE OF zfit0194, "*E-commerce - Ajustes transação ZFI0037 #109706 - BG
      it_zfit0054     TYPE TABLE OF zfit0054. " Rubenilson - 14.03.2025 #169641

DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      t_messtab  TYPE TABLE OF bdcmsgcoll.

DATA: wa_bdcdata    LIKE LINE OF ti_bdcdata .

DATA: tg_bdc TYPE TABLE OF bdcdata,
      wg_bdc TYPE bdcdata,
      opt    TYPE ctu_params.

*----------------------------------------------------------------------*
* WORKAREA
*----------------------------------------------------------------------*
DATA:
  wa_zfit0056     TYPE zfit0056,
  wa_zfit0057     TYPE zfit0057,
  wa_zfit0058     TYPE zfit0058,
  wa_zsdt0054     TYPE ty_zsdt0054,
  wa_zsdt0054_aux TYPE zsdt0054,
  wa_zsdt0159     TYPE zsdt0159,
  wa_zsdt0159_aux TYPE zsdt0159,
  wa_bsad         TYPE ty_bsad,
  wa_bsid         TYPE bsid_view,
  wa_kna1         TYPE ty_kna1,
  wa_t012k        TYPE ty_t012k,
  wa_regup        TYPE regup,
  wa_znfw         TYPE ty_znfw,
  wa_zfiwrt0008   TYPE zfiwrt0008,
  wa_zfiwrt0011   TYPE zfiwrt0011,
  wa_bkpf         TYPE bkpf,
  wa_bkpf_62      TYPE ty_bkpf_62,
  wa_regup_rz     TYPE regup,
  wa_regup_44     TYPE ty_regup_44,
  wa_zsdt0040     TYPE zsdt0040,
  wa_vbrp         TYPE vbrp,
  wa_zsdt0053     TYPE zsdt0053,
  wa_zsdt0051     TYPE zsdt0051,
  wa_zib_cont_chv TYPE zib_contabil_chv,
  wa_saida        TYPE ty_saida,
  wa_zfit0194     TYPE zfit0194. "*E-commerce - Ajustes transação ZFI0037 #109706 - BG

DATA:
  wa_style TYPE lvc_s_styl,
  style    TYPE lvc_t_styl WITH HEADER LINE.


************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************

DATA: editcontainer   TYPE REF TO cl_gui_custom_container,
      cl_container_95 TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id   TYPE REF TO cl_dd_document,
      cl_grid         TYPE REF TO cl_gui_alv_grid,
      wa_stable       TYPE lvc_s_stbl,
      wa_afield       TYPE lvc_s_fcat,
      it_fieldcat     TYPE lvc_t_fcat,
      w_fieldcat      TYPE lvc_s_fcat,
      i_sort          TYPE lvc_t_sort,
      wa_layout       TYPE lvc_s_layo,
      is_stable       TYPE lvc_s_stbl VALUE 'XX',
      wg_repname      LIKE sy-repid,
      wg_x_variant    LIKE disvariant,
      wg_exit(1)      TYPE c,
      wg_save(1)      TYPE c,
      wg_variant      LIKE disvariant,

      gt_f4           TYPE lvc_t_f4 WITH HEADER LINE.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS:  c_x               TYPE c VALUE 'X'.

*&--------------------------------------------------------------------&*
*& Variáveis Genéricas                                                &*
*&--------------------------------------------------------------------&*
DATA: xcontador        TYPE i,
      wl_erro(1),
      wg_documento(10),
      tabix            TYPE sy-tabix,
      nseq_arq         TYPE i,
      vseq_arq         TYPE zfit0056-seq_arq.

DATA: p_zid    TYPE numc10,
      vseq2    TYPE zfit0026-seq,
      vseqc(6).


*** US - 172238 - CBRAND - Inicio
*-----------------------------------------------------------------------
* Classe seleção dados para alimentar ZIB_CONTABIL
*-----------------------------------------------------------------------
CLASS zcl_dados_zib_contabil DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: processa_dados_zib_contabil IMPORTING i_saida TYPE ty_saida.


ENDCLASS.

CLASS zcl_dados_zib_contabil IMPLEMENTATION.
  METHOD processa_dados_zib_contabil.

***  Coletar dados da BSEG.
    DATA: lva_seq_item  TYPE num9 VALUE 000000000.
    DATA: lit_zib_contabil    TYPE TABLE OF zib_contabil.
    DATA: lva_data      TYPE sy-datum,
          lva_data_i    TYPE sy-datum,
          lva_NUMBER(9) TYPE c.


    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSEQZFI003'
      IMPORTING
        number                  = lva_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      SELECT SINGLE * FROM bseg
      INTO @DATA(lwa_bseg)
        WHERE belnr EQ @wg_documento
          AND bukrs EQ @i_saida-bukrs
          AND gjahr EQ @i_saida-budat(4)
          AND hkont EQ @i_saida-hkont
          AND buzei EQ '001'.

      lva_seq_item = ( lva_seq_item + 1 ).

      CONCATENATE i_saida-dt_credito+6(4) i_saida-dt_credito+3(2) i_saida-dt_credito+0(2) INTO  lva_data_i.

      lva_data = lva_data_i + 1.

"BUG solto #183581 - RGA - início
      DO.

        CALL FUNCTION 'DATE_CHECK_WORKINGDAY'
          EXPORTING
            date                       = lva_data
            factory_calendar_id        = 'Z1'
            message_type               = 'E'
          EXCEPTIONS
            date_after_range           = 1
            date_before_range          = 2
            date_invalid               = 3
            date_no_workingday         = 4
            factory_calendar_not_found = 5
            message_type_invalid       = 6
            OTHERS                     = 7.

        IF sy-subrc NE 0.
          ADD 1 TO lva_data.
        ELSE.
          EXIT.
        ENDIF.

      ENDDO.
"BUG solto #183581 - RGA - fim

      DATA(lva_gjhar) = CONV gjahr( lva_data+0(4) ).
      DATA(lva_monat) = CONV monat( lva_data+4(2) ).
      DATA(lva_budat) = |{ lva_data+6(2) }.{ lva_data+4(2) }.{ lva_data+0(4) }|.
      DATA(lva_bldat) = |{ lva_data+6(2) }.{ lva_data+4(2) }.{ lva_data+0(4) }|.

      DATA(lwa_zib_contabil_1) = VALUE zib_contabil(
        obj_key            = |{ 'ZFI0037' }| & |{ lva_number }| & |{ lva_gjhar }|
        seqitem            = CONV #( lva_seq_item )
        bschl              = 50
        gsber              = lwa_bseg-gsber
        bukrs              = lwa_bseg-bukrs
        interface          = '0'
        bldat              = lva_bldat
        budat              = lva_budat
        gjahr              = lva_gjhar
        monat              = lva_monat
        blart              = 'LM'
        xblnr              = lwa_bseg-belnr
        hkont              = lwa_bseg-hkont "conta de  SET criado
        wrbtr              = lwa_bseg-wrbtr
        waers              = i_saida-waers
        sgtxt              = lwa_bseg-sgtxt
        bupla              = lwa_bseg-gsber
        waers_i            = i_saida-waers
        dmbtr              = lwa_bseg-dmbtr
        waers_f            = lwa_bseg-h_hwae2
        dmbe2              = lwa_bseg-dmbe2
        rg_atualizado      = 'N'
    ).
      APPEND lwa_zib_contabil_1 TO lit_zib_contabil[].

      lva_seq_item = ( lva_seq_item + 1 ).


      SELECT SINGLE *
       FROM setleaf
      INTO @DATA(lwa_setleaf)
        WHERE setname = 'CONTA_BAIXA_ECOMMERCE'
        AND   valfrom = @lwa_bseg-hkont.

      DATA(lva_hkont) = lwa_setleaf-valto.

      DATA(lwa_zib_contabil_2) = VALUE zib_contabil(
        obj_key            = |{ 'ZFI0037' }| & |{ lva_number }| & |{ lva_gjhar }|
        seqitem            = CONV #( lva_seq_item )
        bschl              = 40
        gsber              = lwa_bseg-gsber
        bukrs              = lwa_bseg-bukrs
        interface          = '0'
        bldat              = lva_bldat
        budat              = lva_budat
        gjahr              = lva_gjhar
        monat              = lva_monat
        blart              = 'LM'
        xblnr              = lwa_bseg-belnr
        hkont              = lva_hkont   "conta de  SET criado
        wrbtr              = lwa_bseg-wrbtr
        waers              = i_saida-waers
        sgtxt              = lwa_bseg-sgtxt
        bupla              = lwa_bseg-gsber
        waers_i            = i_saida-waers
        dmbtr              = lwa_bseg-dmbtr
        waers_f            = lwa_bseg-h_hwae2
        dmbe2              = lwa_bseg-dmbe2
        rg_atualizado      = 'N'
    ).
      APPEND lwa_zib_contabil_2 TO lit_zib_contabil[].

      MODIFY zib_contabil FROM TABLE lit_zib_contabil[].
      CLEAR: lwa_bseg, lit_zib_contabil[],lva_seq_item, lwa_zib_contabil_1, lwa_zib_contabil_2.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
*** US - 172238 - CBRAND - Fim

CLASS zcl_dados_zfit0026 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: processa_dados_zfit0026 IMPORTING i_saida TYPE ty_saida.


ENDCLASS.


CLASS zcl_dados_zfit0026 IMPLEMENTATION.
  METHOD processa_dados_zfit0026.
    DATA: wa_zfit0026 TYPE zfit0026.
    DATA(obj_auart) = NEW zcl_taxa_curva( ).
    DATA: wa_verifica_zib     TYPE zib_contabil.
    DATA: w_zfit0026 TYPE zfit0026.
*
    DATA(set_mi) = obj_auart->get_auart( 'ZFIS26_SA_MI' ).
    DATA(set_ov) = obj_auart->get_auart( 'ZFIS26_SA_IN' ).
    APPEND LINES OF set_mi TO set_ov.


    DATA: w TYPE i.

    w = 0.

***  Coletar dados da BSEG.
    SELECT SINGLE * FROM bseg
    INTO @DATA(w_bseg)
      WHERE belnr EQ @i_saida-belnr
        AND bukrs EQ @i_saida-bukrs
        AND gjahr EQ @i_saida-budat(4)
        AND buzei EQ '0001'.

    IF w_bseg-vbel2 IS NOT INITIAL.
      SELECT SINGLE * FROM vbak
      INTO @DATA(w_vbak)
        WHERE vbeln EQ @w_bseg-vbel2
          AND spart IN @set_ov.


*      IF W_VBAK-SPART EQ '01'
*      OR W_VBAK-SPART EQ '02'
*      OR W_VBAK-SPART EQ '03'
*      OR W_VBAK-SPART EQ '04'
*      OR W_VBAK-SPART EQ '05'.

      IF w_vbak IS NOT INITIAL.

        SELECT SINGLE * FROM bkpf
        INTO @DATA(w_bkpf)
          WHERE belnr = @w_bseg-belnr
            AND bukrs = @w_bseg-bukrs
            AND gjahr = @w_bseg-gjahr.

        IF w_bkpf IS NOT  INITIAL.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr = '01'
              object      = 'ZID_LANC'
            IMPORTING
              number      = p_zid.

**     Pegar a sequencia doc.
          SELECT COUNT(*)
          INTO vseq2
          FROM zfit0026
         WHERE vbeln EQ w_bseg-vbel2.
          ADD 1 TO vseq2.

          CONCATENATE w_bseg-vbel2 vseq2 sy-datum(4) INTO wa_zfit0026-obj_key.

******Verifica sequencia nova para documento.
          WHILE w EQ 0.

            CLEAR: wa_verifica_zib.
            SELECT SINGLE * FROM zib_contabil INTO wa_verifica_zib WHERE obj_key EQ wa_zfit0026-obj_key.

            CLEAR: w_zfit0026.
            SELECT SINGLE * FROM zfit0026 INTO w_zfit0026 WHERE seq EQ vseq2 AND vbeln  EQ w_bseg-vbel2.

***       Verifica sequencia na ZIB e tambem na ZFIT0026 caso tenha retorna e adiciona outra seguencia.
            IF wa_verifica_zib IS INITIAL AND w_zfit0026 IS INITIAL.

              w = 1.
            ELSE.

              CLEAR: wa_zfit0026-obj_key.
              vseq2 = vseq2 + 1.
              CONCATENATE w_bseg-vbel2 vseq2 sy-datum(4) INTO wa_zfit0026-obj_key.

            ENDIF.
          ENDWHILE.
*****************************************************************************************************************
          DATA: data_pgto TYPE zfit0026-data_pgto.
          data_pgto = |{ i_saida-dt_credito+6(4) }{ i_saida-dt_credito+3(2) }{  i_saida-dt_credito(2) }|.

          wa_zfit0026 = VALUE #( zid_lanc         = p_zid
                                 docnum           = w_bseg-belnr
                                 vbeln            = w_bseg-vbel2
                                 data_pgto        = data_pgto
                                 mont_rbdo        = ( w_bseg-dmbtr + i_saida-vl_jr_mt_enc )
*                                 VLR_MULTA_RB    =
                                 vlr_juros_rbdo   = i_saida-vl_jr_mt_enc
                                 seq              = vseq2
                                 data_venc        = i_saida-dt_vct
                                 moeda            = w_bkpf-waers
                                 mont_moeda       = w_bseg-dmbtr
                                 taxa             = SWITCH #( w_bkpf-waers WHEN 'BRL' THEN 1 ELSE w_bkpf-kursf )
                                 mont_mi          = SWITCH #( w_bkpf-waers WHEN 'BRL' THEN w_bseg-dmbtr ELSE ( w_bseg-dmbtr * w_bkpf-kursf ) )
                                 forma_pag        = w_bseg-zlsch
                                 status           = 'G'
                                 uname            = sy-uname
                                 data_registro    = sy-datum
                                 bukrs            = w_bseg-bukrs
                                 doc_fatura       = w_bseg-vbeln
                                 razao_especial   = ''
                                 eliminado        = ''
                                 observacao       = 'LANÇAMENTO REALIZADO PELA ZFI0037'
                                 zterm            = w_bseg-zterm ).

***       Se existir informações, gravar na tabela ZFIT0026.
          IF wa_zfit0026 IS NOT INITIAL.
            "se ja existe um lançamento na zfit0026 delte para não ficar duplicado
            SELECT SINGLE zid_lanc
            INTO @DATA(v_zid_lanc)
            FROM zfit0026
            WHERE docnum EQ @wa_zfit0026-docnum
                  AND vbeln EQ @wa_zfit0026-vbeln.

            IF sy-subrc IS INITIAL.
              DELETE FROM zfit0026  WHERE docnum EQ wa_zfit0026-docnum AND vbeln EQ wa_zfit0026-vbeln.
            ENDIF.

            MODIFY zfit0026 FROM wa_zfit0026.
            COMMIT WORK.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      catch_hotspot
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no.

* US - 62755 - Inicio - CBRAND
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed
                  e_onf4
                  e_onf4_before
                  e_onf4_after
                  e_ucomm .
* US - 62755 - Fim - CBRAND
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD catch_hotspot.
    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
    IF sy-subrc = 0.
      IF e_column_id = 'BELNR' AND wa_saida-belnr IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD wa_saida-belnr.
        SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
        SET PARAMETER ID 'GJR' FIELD wa_saida-budat+0(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSEIF e_column_id = 'AUGBL' AND wa_saida-augbl IS NOT INITIAL AND wa_saida-augbl NE icon_generate..
        SET PARAMETER ID 'BLN' FIELD wa_saida-augbl.
        SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
        SET PARAMETER ID 'GJR' FIELD wa_saida-budat+0(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSEIF  e_column_id = 'XBLNR' AND wa_saida-xblnr IS NOT INITIAL.
        SET PARAMETER ID 'VF' FIELD wa_saida-xblnr.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ELSEIF  e_column_id = 'NRO_SOL_OV' AND wa_saida-nro_sol_ov IS NOT INITIAL.
        REFRESH tg_bdc.
        IF  wa_saida-memo  = 'X'.
          PERFORM f_preencher_dynpro USING:
             'X' 'ZSDR016'                      '0100',
             ' ' 'WG_HEADER-DOC_SIMULACAO'      wa_saida-nro_sol_ov,
             ' ' 'BDC_OKCODE'                    'ATUAL'.
          opt-dismode = 'E'.
          opt-defsize = ' '.
          CALL TRANSACTION 'ZSDT0044' USING tg_bdc OPTIONS FROM opt.

        ELSE.
          PERFORM f_preencher_dynpro USING:
            'X' 'ZSDR0022'                      '0050',
            ' ' 'WG_HEADER-NRO_SOL_OV'          wa_saida-nro_sol_ov,
            ' ' 'BDC_OKCODE'                    'ATUAL'.
          opt-dismode = 'E'.
          opt-defsize = ' '.
          CALL TRANSACTION 'ZSDT0062' USING tg_bdc OPTIONS FROM opt.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "catch_hotspot_4

  METHOD on_data_changed.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value.


    LOOP AT er_data_changed->mt_good_cells
                         INTO ls_good
                         WHERE fieldname = 'TX_MULTA'.
      CLEAR: lv_value.
      READ TABLE it_saida INTO wa_saida INDEX ls_good-row_id.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      wa_saida-tx_multa = lv_value.
      MODIFY it_saida FROM wa_saida INDEX ls_good-row_id TRANSPORTING tx_multa.


      SELECT SINGLE *
         FROM zfiwrt0011
         INTO @DATA(lwa_zfiwrt0011)
         WHERE seq_lcto = @wa_saida-seq_lcto
           AND zlsch = 'D'
           AND estorno <> 'X' .

      IF sy-subrc IS INITIAL.
        lwa_zfiwrt0011-taxa_multa = lv_value.
        MODIFY zfiwrt0011 FROM lwa_zfiwrt0011.
        COMMIT WORK.
        CLEAR: lwa_zfiwrt0011.
      ENDIF.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                     INTO ls_good
                     WHERE fieldname = 'TX_JUROS'.

      CLEAR: lv_value.
      READ TABLE it_saida INTO wa_saida INDEX ls_good-row_id.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      wa_saida-tx_juros = lv_value.
      MODIFY it_saida FROM wa_saida INDEX ls_good-row_id TRANSPORTING tx_juros.

      SELECT SINGLE *
         FROM zfiwrt0011
         INTO lwa_zfiwrt0011
         WHERE seq_lcto = wa_saida-seq_lcto
           AND zlsch = 'D'
           AND estorno <> 'X' .

      IF sy-subrc IS INITIAL.
        lwa_zfiwrt0011-taxa_juros = lv_value.
        MODIFY zfiwrt0011 FROM lwa_zfiwrt0011.
        COMMIT WORK.
        CLEAR: lwa_zfiwrt0011.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:  s_bukrs FOR regup-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY,
                   s_kunnr FOR regup-kunnr,
                   s_dtvct FOR regup-budat,
                   s_dtlct FOR regup-budat OBLIGATORY,
                   s_dtarq FOR regup-budat .
SELECTION-SCREEN: END OF BLOCK b1.


SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: r_nen AS CHECKBOX  DEFAULT ' ' .
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS:
    r_abe RADIOBUTTON GROUP rad1,
    r_ven RADIOBUTTON GROUP rad1,
    r_liq RADIOBUTTON GROUP rad1,
    r_tod RADIOBUTTON GROUP rad1.
SELECTION-SCREEN: END OF BLOCK b3.

AT SELECTION-SCREEN OUTPUT.
  IF r_abe = 'X' OR r_liq = 'X' OR r_tod = 'X' OR r_ven = 'X'.
    CLEAR r_nen.
  ENDIF.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*************************************************************
  "*E-commerce - Ajustes transação ZFI0037 #109706 - BG - INICIO
*************************************************************
  " Usuários que podem Gerar Baixa
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_BX_COBRANCA'
    TABLES
      set_values    = t_usermd
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT t_usermd BY from.

  IF r_nen = 'X'.
    PERFORM:  f_seleciona_nao,
              f_monta_saida_nao.
  ELSE.

    PERFORM:  f_seleciona_dados,
              f_monta_saida.
  ENDIF.

  PERFORM: f_imprime_dados.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  DATA: lva_text          TYPE string,
        lva_amount        TYPE string,
        lva_num           TYPE i,
        lva_doc_simulacao TYPE zsdt0040-doc_simulacao.


  SELECT *
    FROM regup
    INTO TABLE it_regup
    WHERE bukrs IN s_bukrs
    AND   kunnr NE ''
    AND   xvorl EQ ''
    AND   gjahr BETWEEN s_dtlct-low+0(4) AND s_dtlct-high+0(4)
    AND   kunnr IN s_kunnr
    AND   budat IN s_dtlct
    AND   laufd IN s_dtarq.

  IF r_liq NE 'X'.

    SELECT *
    FROM bsid_view
    INTO TABLE @it_bsid
  WHERE bukrs	 IN @s_bukrs
  AND   hbkid  IN ( 'AL5', 'ITAU3' ) "// wbarbosa - US-163166 - 23/01/2025
  AND   zlsch  IN ('D', 'X')
  AND   umskz  IN ('F','')
  AND   kunnr  IN @s_kunnr
  AND   budat  IN @s_dtlct.

  ENDIF.

  IF r_abe NE 'X' AND r_ven NE 'X'.

    SELECT *
    FROM bsad_view
    APPENDING TABLE @it_bsid
  WHERE bukrs	 IN @s_bukrs
  AND   hbkid  IN ( 'AL5', 'ITAU3' ) "// wbarbosa - US-163166 - 23/01/2025
  AND   zlsch  IN ('D', 'X')
  AND   umskz  IN ('F','')
  AND   kunnr  IN @s_kunnr
  AND   budat  IN @s_dtlct.

  ENDIF.

  IF it_bsid[] IS NOT INITIAL.

*** Inicio - Rubenilson  - 13.03.2025 #169641
    DATA(lt_bsid) = it_bsid.

    DELETE lt_bsid WHERE xref3 IS INITIAL.
    SORT lt_bsid BY xref3.
    DELETE ADJACENT DUPLICATES FROM lt_bsid COMPARING xref3.
    IF lt_bsid IS NOT INITIAL.
      SELECT *
        FROM zfit0056
        INTO TABLE it_zfit0056
        FOR ALL ENTRIES IN lt_bsid
        WHERE nosso_nro = lt_bsid-xref3.
      IF sy-subrc IS INITIAL.
        SORT it_zfit0056 BY nosso_nro.

*** US - 172238 - Inicio - CBRAND
        SELECT *
          FROM zfit0058
        INTO TABLE it_zfit0058
          FOR ALL ENTRIES IN it_zfit0056
        WHERE cod_mov   = it_zfit0056-cod_retorno.

        SELECT *
           FROM zfit0057
        APPENDING CORRESPONDING FIELDS OF TABLE it_zfit0057
           FOR ALL ENTRIES IN it_zfit0056
        WHERE cod_arq  = it_zfit0056-cod_arq.

*** US - 172238 - Fim - CBRAND

        DATA(lt_zfit0056) = it_zfit0056.
        SORT lt_zfit0056 BY cod_arq.
        DELETE ADJACENT DUPLICATES FROM lt_zfit0056 COMPARING cod_arq.

        SELECT *
          FROM zfit0054
          INTO TABLE it_zfit0054
          FOR ALL ENTRIES IN lt_zfit0056
          WHERE cod_arq = lt_zfit0056-cod_arq.
        IF sy-subrc IS INITIAL.
          SORT it_zfit0054 BY cod_arq.
        ENDIF.

      ENDIF.

    ENDIF.
*** Fim - Rubenilson  - 13.03.2025 #169641

    SELECT bukrs, belnr, gjahr, augbl
      FROM bsad_view
      INTO TABLE @it_bsad
      FOR ALL ENTRIES IN @it_bsid
      WHERE bukrs   = @it_bsid-bukrs
      AND   belnr   = @it_bsid-belnr
      AND   gjahr   = @it_bsid-gjahr.

    SELECT kunnr name1
   FROM kna1
   INTO TABLE it_kna1
   FOR ALL ENTRIES IN it_bsid
   WHERE kunnr = it_bsid-kunnr.

    it_bsid_aux[] = it_bsid[].
    DELETE it_bsid_aux WHERE umsks EQ ''.

    IF  it_bsid_aux[] IS NOT INITIAL.
      SELECT adiant nro_sol_ov
        FROM zsdt0054
        INTO TABLE it_zsdt0054
        FOR ALL ENTRIES IN it_bsid_aux
        WHERE adiant = it_bsid_aux-belnr.

      SELECT *
       FROM zsdt0159
       FOR ALL ENTRIES IN @it_bsid_aux
       WHERE vbeln IS NOT INITIAL
        AND adiant = @it_bsid_aux-belnr
        AND doc_simulacao IS NOT INITIAL
        AND vbeln IS NOT INITIAL
        AND seq IS NOT INITIAL
        INTO TABLE @it_zsdt0159.
      IF it_zsdt0159[] IS NOT INITIAL.

        SELECT *
        FROM zfit0194
        INTO TABLE it_zfit0194
        FOR ALL ENTRIES IN it_zsdt0159
        WHERE ident_tit_empr = it_zsdt0159-id_transacao_financeira.
        IF sy-subrc = '0'.

          SELECT *
          FROM zfit0058
          INTO TABLE it_zfit0058
          FOR ALL ENTRIES IN it_zfit0194
          WHERE cod_mov   = it_zfit0194-seq2+1(2).

        ENDIF.

*        "// INICIO WBARBOSA US-163166 23/01/2025
        DATA r_nosso_nro TYPE RANGE OF zfit0056-nosso_nro.

        LOOP AT it_zsdt0159 INTO DATA(ls_zsdt0159).

          APPEND
          VALUE #(
                    option = 'EQ'
                    sign = 'I'
                    low = ls_zsdt0159-id_transacao_financeira
                 ) TO r_nosso_nro.

        ENDLOOP.

        SELECT *
          FROM zfit0056
          APPENDING CORRESPONDING FIELDS OF TABLE it_zfit0056
            WHERE nosso_nro IN r_nosso_nro.

        IF it_zfit0056[] IS NOT INITIAL.
          SELECT *
            FROM zfit0057
            APPENDING CORRESPONDING FIELDS OF TABLE it_zfit0057
            FOR ALL ENTRIES IN it_zfit0056
            WHERE cod_arq	=	it_zfit0056-cod_arq.

          SELECT *
           FROM zfit0058
           APPENDING CORRESPONDING FIELDS OF TABLE it_zfit0058
           FOR ALL ENTRIES IN it_zfit0056
           WHERE cod_mov   = it_zfit0056-cod_retorno.

          SELECT *
          FROM zfit0194
          INTO TABLE it_zfit0194
          FOR ALL ENTRIES IN it_zsdt0159
          WHERE ident_tit_empr = it_zsdt0159-id_transacao_financeira.

        ENDIF.
      ENDIF.
    ENDIF.
*    "// FIM WBARBOSA US-163166 23/01/2025

*    IF it_bsid IS NOT INITIAL.

* ZSDT0044 - Inicio

    LOOP AT it_bsid INTO wa_bsid.
      "VENDA 0000030914 - regup-xblnr
      lva_num = strlen( wa_bsid-xblnr ).
      lva_text = wa_bsid-xblnr.

      DO lva_num TIMES.
        IF lva_text(1) CA '0123456789'.
          CONCATENATE lva_amount lva_text(1) INTO lva_amount.
          CONDENSE lva_amount NO-GAPS.
        ENDIF.
        SHIFT lva_text LEFT CIRCULAR.
      ENDDO.

      MOVE lva_amount TO wa_regup_44-doc_simulacao.
      wa_regup_44-xblnr = wa_bsid-xblnr. "// wbarbosa - US-163166 - 23/01/2025

      IF wa_regup_44-doc_simulacao IS NOT INITIAL.
        wa_regup_44-belnr = wa_regup_rz-belnr.

        APPEND wa_regup_44 TO it_regup_44.
        CLEAR: wa_regup_44 .
      ENDIF.
      CLEAR: lva_text  ,
             lva_amount,
             lva_num,
             wa_bsid.
    ENDLOOP.

    IF it_regup_44 IS NOT INITIAL.
      SELECT  *
       FROM  zsdt0040
       INTO TABLE  it_zsdt0040
       FOR ALL ENTRIES IN it_regup_44
       WHERE doc_simulacao EQ it_regup_44-doc_simulacao
         AND ecommerce EQ abap_true. "// wbarbosa - US-163166 - 23/01/2025
    ENDIF.

    SELECT  *
      FROM  bkpf
      INTO TABLE it_bkpf
          FOR ALL ENTRIES IN it_bsid
      WHERE bukrs EQ s_bukrs-low
        AND belnr EQ it_bsid-belnr.


* ZSDT0062 - Inicio
    LOOP AT it_bkpf INTO wa_bkpf.
      MOVE wa_bkpf-awkey TO wa_bkpf_62-awkey.
      wa_bkpf_62-belnr = wa_bkpf-belnr.

      APPEND wa_bkpf_62 TO it_bkpf_62.
      CLEAR: wa_bkpf_62.
    ENDLOOP.

    SELECT  *
      FROM  vbrp
      INTO TABLE it_vbrp
             FOR ALL ENTRIES IN it_bkpf_62
      WHERE vbeln = it_bkpf_62-awkey.

    IF it_vbrp IS NOT INITIAL.
      SELECT  *
       FROM  zsdt0053
       INTO TABLE it_zsdt0053
              FOR ALL ENTRIES IN it_vbrp
       WHERE vbeln = it_vbrp-aubel.

      SELECT  *
       FROM  zsdt0051
       INTO TABLE it_zsdt0051
              FOR ALL ENTRIES IN it_zsdt0053
       WHERE nro_sol_ov = it_zsdt0053-nro_sol_ov.
    ENDIF.
* ZSDT0062 - Fim

* znfw - inicio
    LOOP AT it_bkpf INTO wa_bkpf.

      MOVE wa_bkpf-awkey TO wa_znfw-obj_key.

      IF wa_znfw-obj_key IS NOT INITIAL.
        wa_znfw-bukrs = wa_bkpf-bukrs.
        wa_znfw-belnr = wa_bkpf-belnr.

        APPEND wa_znfw TO it_znfw.
        CLEAR: wa_znfw.
      ENDIF.

      CLEAR: lva_text  ,
             lva_amount,
             lva_num  .

    ENDLOOP.

    IF it_znfw IS NOT INITIAL.
      SELECT  *
      FROM   zfiwrt0008
      INTO TABLE it_zfiwrt0008
             FOR ALL ENTRIES IN it_znfw
      WHERE obj_key EQ it_znfw-obj_key
        AND bukrs = it_znfw-bukrs .

      IF it_zfiwrt0008 IS NOT INITIAL.
        SELECT  *
          FROM zfiwrt0011
       INTO TABLE it_zfiwrt0011
          FOR ALL ENTRIES IN it_zfiwrt0008
            WHERE seq_lcto  EQ it_zfiwrt0008-seq_lcto
              AND zlsch EQ 'D'
              AND estorno NE 'X'.
      ENDIF.

    ENDIF.

    SELECT bukrs hbkid hkont
    FROM  t012k
    INTO TABLE it_t012k
    FOR ALL ENTRIES IN it_bsid
    WHERE bukrs	= it_bsid-bukrs
    AND   hbkid	=	it_bsid-hbkid.

    "Modifica a conta AL5 p/ conta 111510  - US 128649 ZFI0037 - Compensação de docuemnto AL5 - PSA
    LOOP AT it_t012k ASSIGNING FIELD-SYMBOL(<it_t012k>).
      IF <it_t012k>-hbkid = 'AL5' AND <it_t012k>-hkont = '0000111600'.
        CLEAR: <it_t012k>-hkont.
        <it_t012k>-hkont = '0000111510'.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF  it_regup[] IS NOT INITIAL.
    SELECT kunnr name1
      FROM kna1
      APPENDING CORRESPONDING FIELDS OF TABLE it_kna1
      FOR ALL ENTRIES IN it_regup
      WHERE kunnr = it_regup-kunnr.

    SELECT *
      FROM zfit0056
      APPENDING CORRESPONDING FIELDS OF TABLE it_zfit0056 "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
      FOR ALL ENTRIES IN it_regup
      WHERE nosso_nro	=	it_regup-xref3.

    IF it_zfit0056[] IS NOT INITIAL.
      SELECT *
        FROM zfit0057
        APPENDING CORRESPONDING FIELDS OF TABLE it_zfit0057 "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
        FOR ALL ENTRIES IN it_zfit0056
        WHERE cod_arq	=	it_zfit0056-cod_arq.

      SELECT *
       FROM zfit0058
       APPENDING CORRESPONDING FIELDS OF TABLE it_zfit0058 "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
       FOR ALL ENTRIES IN it_zfit0056
       WHERE cod_mov   = it_zfit0056-cod_retorno.
    ENDIF.

    SELECT bukrs belnr gjahr augbl
      FROM bsad
      APPENDING CORRESPONDING FIELDS OF TABLE it_bsad "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
      FOR ALL ENTRIES IN it_regup
      WHERE bukrs   = it_regup-bukrs
      AND   belnr   = it_regup-belnr
      AND   gjahr   = it_regup-gjahr.

    it_regup_aux[] = it_regup[].
    it_regup_rz[]  = it_regup[]. "US - 62755 - CSB

    DELETE it_regup_aux WHERE umsks EQ ''.

    IF it_regup_aux[] IS NOT INITIAL.
      SELECT  adiant nro_sol_ov
        FROM  zsdt0054
        APPENDING CORRESPONDING FIELDS OF TABLE  it_zsdt0054 "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
        FOR ALL ENTRIES IN it_regup_aux
        WHERE adiant EQ it_regup_aux-belnr.

      SELECT *
      FROM zsdt0159
      APPENDING CORRESPONDING FIELDS OF TABLE  it_zsdt0159 "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
      FOR ALL ENTRIES IN it_regup_aux
      WHERE adiant = it_regup_aux-belnr.
    ENDIF.

** US - 62755 - Inicio - CBRAND
    IF it_regup_rz[] IS NOT INITIAL.
* ZSDT0044

      LOOP AT it_regup_rz INTO wa_regup_rz.
        "VENDA 0000030914 - regup-xblnr
        lva_num = strlen( wa_regup_rz-xblnr ).
        lva_text = wa_regup_rz-xblnr.

        DO lva_num TIMES.
          IF lva_text(1) CA '0123456789'.
            CONCATENATE lva_amount lva_text(1) INTO lva_amount.
            CONDENSE lva_amount NO-GAPS.
          ENDIF.
          SHIFT lva_text LEFT CIRCULAR.
        ENDDO.

        MOVE lva_amount TO wa_regup_44-doc_simulacao.
        wa_regup_44-xblnr = wa_regup_rz-xblnr. "// wbarbosa - US-163166 - 23/01/2025

        IF wa_regup_44-doc_simulacao IS NOT INITIAL.
          wa_regup_44-belnr = wa_regup_rz-belnr.

          APPEND wa_regup_44 TO it_regup_44.
          CLEAR: wa_regup_44 .
        ENDIF.
        CLEAR: lva_text  ,
               lva_amount,
               lva_num  .
      ENDLOOP.

      IF it_regup_44 IS NOT INITIAL.
        SELECT  *
         FROM  zsdt0040
         APPENDING CORRESPONDING FIELDS OF TABLE  it_zsdt0040 "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
         FOR ALL ENTRIES IN it_regup_44
         WHERE doc_simulacao EQ it_regup_44-doc_simulacao
           AND ecommerce EQ abap_true. "// wbarbosa - US-163166 - 23/01/2025
      ENDIF.

* SELECT NA BKPF para os processos seguintes
      SELECT  *
        FROM  bkpf
        APPENDING CORRESPONDING FIELDS OF TABLE it_bkpf "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
            FOR ALL ENTRIES IN it_regup_rz
        WHERE bukrs EQ s_bukrs-low
          AND belnr EQ it_regup_rz-belnr.

* ZSDT0062 - Inicio

      LOOP AT it_bkpf INTO wa_bkpf.
        MOVE wa_bkpf-awkey TO wa_bkpf_62-awkey.
        wa_bkpf_62-belnr = wa_bkpf-belnr.

        APPEND wa_bkpf_62 TO it_bkpf_62.
        CLEAR: wa_bkpf_62.
      ENDLOOP.

      SELECT  *
        FROM  vbrp
        APPENDING CORRESPONDING FIELDS OF TABLE it_vbrp "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
               FOR ALL ENTRIES IN it_bkpf_62
        WHERE vbeln = it_bkpf_62-awkey.

      IF it_vbrp IS NOT INITIAL.
        SELECT  *
         FROM  zsdt0053
         APPENDING CORRESPONDING FIELDS OF TABLE it_zsdt0053 "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
                FOR ALL ENTRIES IN it_vbrp
         WHERE vbeln = it_vbrp-aubel.

        SELECT  *
         FROM  zsdt0051
         APPENDING CORRESPONDING FIELDS OF TABLE it_zsdt0051 "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
                FOR ALL ENTRIES IN it_zsdt0053
         WHERE nro_sol_ov = it_zsdt0053-nro_sol_ov.
      ENDIF.
* ZSDT0062 - Fim

* ZNFW - Inicio
      LOOP AT it_bkpf INTO wa_bkpf.

        MOVE wa_bkpf-awkey TO wa_znfw-obj_key.

        IF wa_znfw-obj_key IS NOT INITIAL.
          wa_znfw-bukrs = wa_bkpf-bukrs.
          wa_znfw-belnr = wa_bkpf-belnr.

          APPEND wa_znfw TO it_znfw.
          CLEAR: wa_znfw.
        ENDIF.

        CLEAR: lva_text  ,
               lva_amount,
               lva_num  .

      ENDLOOP.

      IF it_znfw IS NOT INITIAL.
        SELECT  *
        FROM   zfiwrt0008
        APPENDING CORRESPONDING FIELDS OF TABLE it_zfiwrt0008 "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
               FOR ALL ENTRIES IN it_znfw
        WHERE obj_key EQ it_znfw-obj_key
          AND bukrs = it_znfw-bukrs .

        IF it_zfiwrt0008 IS NOT INITIAL.
          SELECT  *
            FROM zfiwrt0011
         APPENDING CORRESPONDING FIELDS OF TABLE it_zfiwrt0011 "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
            FOR ALL ENTRIES IN it_zfiwrt0008
              WHERE seq_lcto  EQ it_zfiwrt0008-seq_lcto
                AND zlsch EQ 'D'
                AND estorno NE 'X'.
        ENDIF.

      ENDIF.
* ZNFW - Fim
    ENDIF.
** US - 62755 - Fim - CBRAND

    SELECT bukrs hbkid hkont
      FROM  t012k
      APPENDING CORRESPONDING FIELDS OF TABLE it_t012k "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG
      FOR ALL ENTRIES IN it_regup
      WHERE bukrs	=	it_regup-bukrs
      AND   hbkid	=	it_regup-hbkid.

  ENDIF.

*************************************************************
  "*E-commerce - Ajustes transação ZFI0037 #109706 - BG - FIM
*************************************************************

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_saida .

  DATA: v_spart TYPE vbak-spart.
  DATA: v_vbeln TYPE vbak-vbeln.
  DATA: v_vtext	TYPE tspat-vtext.
  DATA: v_origem  TYPE  char30,
        v_juros   TYPE zde003,
        v_multa   TYPE zde003,
        lv_exibir TYPE c. "Rubenilson - 13.03.25 #169641

  "// INICIO wbarbosa - US-163166 - 23/01/2025 "// Correção do IR210189 incluida na US
*** Stefanini - IR210189 - 27/11/2024 - LAZAROSR - Início de Alteração
*  SORT: it_zfit0056    BY nosso_nro cod_retorno,
*  SORT: IT_ZFIT0056    BY NOSSO_NRO ASCENDING CPUDT CPUTM DESCENDING,
*  SORT: IT_ZFIT0056 BY NOSSO_NRO ASCENDING COD_RETORNO DESCENDING,
  SORT: it_zfit0056 BY nosso_nro ASCENDING cpudt DESCENDING cputm DESCENDING seq_arq DESCENDING,
*** Stefanini - IR210189 - 27/11/2024 - LAZAROSR - Fim de Alteração
*** Stefanini - IR218515 - 24/01/2025 - FMEURER - Início de Alteração
  it_zfit0194    BY dt_ocorrencia cod_arq,
*** Stefanini - IR218515 - 24/01/2025 - FMEURER - Fim de Alteração
  "// FIM wbarbosa - US-163166 - 23/01/2025 "// Correção do IR210189 incluida na US

  it_zfit0057    BY cod_arq seq_arq,
  it_zfit0058    BY cod_mov,
  it_bsad        BY bukrs belnr gjahr,
  it_zsdt0054    BY adiant,
  it_zsdt0159    BY adiant,
  it_kna1        BY kunnr,
  it_t012k       BY bukrs hbkid,
  it_regup       BY belnr xblnr.

  DELETE ADJACENT DUPLICATES FROM it_regup COMPARING belnr xblnr.

  LOOP AT it_regup INTO wa_regup.
    CLEAR wa_zfit0056.
    wa_saida-descricao          = icon_message_warning_small.
    READ TABLE it_zfit0056 INTO wa_zfit0056 WITH KEY nosso_nro = wa_regup-xref3 BINARY SEARCH.
    IF sy-subrc = 0.
*** Stefanini - IR210189 - 27/11/2024 - LAZAROSR - Início de Alteração
*      LOOP AT it_zfit0056 INTO wa_zfit0056 WHERE nosso_nro = wa_regup-xref3 .
*      ENDLOOP.
*** Stefanini - IR210189 - 27/11/2024 - LAZAROSR - Fim de Alteração
      READ TABLE it_zfit0058 INTO wa_zfit0058 WITH KEY cod_mov = wa_zfit0056-cod_retorno BINARY SEARCH.
      IF sy-subrc NE 0.
      ELSE.
        wa_saida-descricao          = wa_zfit0058-descricao.            "Arq.Retorno
      ENDIF.

***  US - 172238 - CBRAND - Incio
      READ TABLE it_zfit0054 INTO DATA(lwa_zfit0054)
            WITH KEY cod_arq = wa_zfit0056-cod_arq.

      IF sy-subrc IS INITIAL.
        wa_saida-origem_receb = lwa_zfit0054-origem_receb.
      ENDIF.
      CLEAR:lwa_zfit0054.
*** US - 172238 - CBRAND - Fim

    ENDIF.

    wa_saida-dt_vct             = wa_regup-zfbdt + wa_regup-zbd1t.  "Dt.Vcto
    IF s_dtvct IS NOT INITIAL.
      IF wa_saida-dt_vct NOT IN s_dtvct.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE it_bsad INTO wa_bsad WITH KEY  bukrs   = wa_regup-bukrs
                                              belnr   = wa_regup-belnr
                                              gjahr   = wa_regup-gjahr BINARY SEARCH.
    IF sy-subrc NE 0.
      IF r_liq  = 'X'.
        CONTINUE.
      ENDIF.
      IF r_ven  = 'X' AND wa_saida-dt_vct GE sy-datum.
        CONTINUE.
      ENDIF.
      IF wa_zfit0056-cod_retorno = '06' OR  "pagamento pelo cliente efetuado
         wa_zfit0056-cod_retorno = '17'.
        wa_saida-augbl               = icon_generate.
      ENDIF.
    ELSE.
      IF r_ven  = 'X' AND wa_saida-dt_vct GE sy-datum.
        CONTINUE.
      ENDIF.
      IF r_abe  = 'X'.
        CONTINUE.
      ENDIF.
      wa_saida-augbl              = wa_bsad-augbl.                    "Compensado
    ENDIF.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_regup-kunnr BINARY SEARCH.
    wa_saida-kunnr              = wa_regup-kunnr.                   "Cliente
    IF sy-subrc = 0.
      wa_saida-name1              = wa_kna1-name1.                    "Nome do Cliente
    ENDIF.

    wa_saida-budat              = wa_regup-budat.                   "Dt.Lcto
    wa_saida-belnr              = wa_regup-belnr.                   "Nro.doc.
    wa_saida-xblnr              = wa_regup-xblnr.                   "Nro.Fatura

*Inicio Alteração - Leandro Valentim Ferreira - 15.06.23 - #111774

    CLEAR:v_juros,v_multa,v_origem.

    CALL FUNCTION 'Z_BUSCA_JUROS_MULTA_BOLETO'
      EXPORTING
        i_belnr      = wa_regup-belnr
        i_bukrs      = wa_regup-bukrs
      IMPORTING
        e_origem     = v_origem
        e_juros      = v_juros
        e_multa      = v_multa
        e_nro_sol_ov = wa_saida-nro_sol_ov
        e_seq_lcto   = wa_saida-seq_lcto.

    wa_saida-origem   = v_origem.
    wa_saida-tx_juros = v_juros.
    wa_saida-tx_multa = v_multa.

    IF wa_saida-tx_juros > 0 OR wa_saida-tx_multa > 0.
      REFRESH: wa_saida-style.
      PERFORM block_field_taxa USING 'TX_JUROS' 'X'.
      PERFORM block_field_taxa USING 'TX_MULTA' 'X'.
    ENDIF.

    IF wa_saida-origem IS INITIAL.
      PERFORM block_field_taxa USING 'TX_JUROS' 'X'.
      PERFORM block_field_taxa USING 'TX_MULTA' 'X'.
    ENDIF.
*** US - 62755 - Fim - CBRAND
    IF wa_saida-dt_vct LT sy-datum.
      wa_saida-icon_sit           = icon_alert.                     "Situação
    ELSE.
      wa_saida-icon_sit           = icon_resubmission.              "Situação
    ENDIF.
    wa_saida-dias_atraso        = sy-datum - wa_saida-dt_vct.       "Dias Atraso   = Data Atual - XVCTO
    IF wa_saida-dias_atraso LT 0.
      wa_saida-dias_atraso = 0.
    ENDIF.
    wa_saida-dmbtr              = wa_regup-dmbtr.                   "Vlr.Titulo


    nseq_arq = wa_zfit0056-seq_arq.
    ADD 1 TO nseq_arq.
    vseq_arq = nseq_arq.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vseq_arq
      IMPORTING
        output = vseq_arq.

    READ TABLE it_zfit0057 INTO wa_zfit0057 WITH KEY cod_arq = wa_zfit0056-cod_arq
                                                     seq_arq = vseq_arq BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-dt_credito         = wa_zfit0057-dt_credito.           "Dt.Pgto
      CONCATENATE wa_zfit0057-dt_credito+0(2) '.' wa_zfit0057-dt_credito+2(2) '.' wa_zfit0057-dt_credito+4(4) INTO wa_saida-dt_credito.

      wa_saida-vl_liq_creditado   = wa_zfit0057-vl_liq_creditado.     "Vlr.Credito
      wa_saida-vl_jr_mt_enc       = wa_zfit0057-vl_jr_mt_enc.         "Vlr.Jros/Multa/Enc.
      wa_saida-vl_abatimento      = wa_zfit0057-vl_abatimento.        "Vlr.Abat.Conc.
      wa_saida-vl_iof             = wa_zfit0057-vl_iof.               "Vlr.IOF
      wa_saida-vl_pago_sacado     = wa_zfit0057-vl_pago_sacado.       "Vlr.Pg.Sacado
    ENDIF.

    wa_saida-gsber              = wa_regup-gsber.                   "Filial
    wa_saida-laufi              = wa_regup-laufi.                   "Ident.Arq.
    wa_saida-laufd              = wa_regup-laufd.                   "Dt.Arq.
    wa_saida-hbkid              = wa_regup-hbkid.                   "Banco Empresa ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG

    READ TABLE it_t012k INTO wa_t012k WITH KEY bukrs = wa_regup-bukrs
                                               hbkid = wa_regup-hbkid BINARY SEARCH.
    wa_saida-hkont              = wa_t012k-hkont. "SHDB
    wa_saida-bukrs              = wa_regup-bukrs. "SHDB
    wa_saida-waers              = wa_regup-waers. "SHDB
    wa_saida-xref2              = wa_regup-xref2. "SHDB
    wa_saida-xref3              = wa_regup-xref3. "SHDB
    wa_saida-umsks              = wa_regup-umsks. "SHDB
    v_vbeln = wa_saida-xref2.
    SELECT SINGLE spart
      FROM vbak
      INTO v_spart
      WHERE vbeln = v_vbeln.
    IF sy-subrc = 0.
      SELECT SINGLE vtext
        FROM tspat
        INTO v_vtext
        WHERE spras = sy-langu
        AND   spart = v_spart.
      CONCATENATE v_spart '-' v_vtext INTO wa_saida-spart.
    ENDIF.
    APPEND wa_saida TO it_saida.
    CLEAR wa_saida.

  ENDLOOP.

****************************------------------------*********************************
  "*E-commerce - Ajustes transação ZFI0037 #109706 - BG - INICIO
****************************------------------------*********************************

  SORT: it_zsdt0054 BY adiant,
        it_regup    BY bukrs belnr gjahr,
        it_kna1     BY kunnr.


  LOOP AT it_bsid INTO wa_bsid.
    CLEAR: wa_zfit0056,
           lv_exibir." Rubenilson - 14.03.2025 #169641

    wa_saida-descricao          = icon_message_warning_small.

    READ TABLE  it_zsdt0159 INTO DATA(wa_0159) WITH KEY adiant = wa_bsid-belnr.
    IF sy-subrc IS NOT INITIAL.

*** Inicio - Rubenilson - 13.03.25 #169641
      READ TABLE it_zfit0056 ASSIGNING FIELD-SYMBOL(<fs_zfit0056>)
      WITH KEY nosso_nro = wa_bsid-xref3
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE it_zfit0054 ASSIGNING FIELD-SYMBOL(<fs_zfit0054>)
        WITH KEY cod_arq = <fs_zfit0056>-cod_arq
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          IF <fs_zfit0054>-api IS INITIAL.
            CONTINUE.
          ELSE.
            lv_exibir = abap_true.
            wa_saida-origem_receb = <fs_zfit0054>-origem_receb. " US - 172238 - CBRAND
          ENDIF.
        ENDIF.
      ENDIF.
*** Fim - Rubenilson - 13.03.25 #169641
    ENDIF.

    IF sy-subrc EQ 0.
* "// Inicio wbarbosa - US-163166 - 23/01/2025
* Verifica se o Processo é E-commerce caso não For vai pro proximo
      READ TABLE it_zsdt0040 TRANSPORTING NO FIELDS WITH KEY doc_simulacao = wa_0159-doc_simulacao.
*** Inicio - Rubenilson - 13.03.25 #169641
*      IF sy-subrc IS NOT INITIAL AND lv_exibir IS INITIAL.
      IF sy-subrc IS NOT INITIAL AND lv_exibir IS INITIAL.
*** Fim - Rubenilson - 13.03.25 #169641
        CONTINUE.
      ENDIF.
* "// Fim wbarbosa - US-163166 - 23/01/2025

      wa_saida-dt_vct             = wa_bsid-zfbdt + wa_bsid-zbd1t.  "Dt.Vcto
      IF s_dtvct IS NOT INITIAL.
        IF wa_saida-dt_vct NOT IN s_dtvct.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsid-kunnr BINARY SEARCH.
      wa_saida-kunnr              = wa_bsid-kunnr.                   "Cliente
      IF sy-subrc = 0.
        wa_saida-name1              = wa_kna1-name1.                    "Nome do Cliente
      ENDIF.

      wa_saida-budat              = wa_bsid-budat.                   "Dt.Lcto
      wa_saida-belnr              = wa_bsid-belnr.                   "Nro.doc.
      wa_saida-xblnr              = wa_bsid-xblnr.                   "Nro.Fatura
      wa_saida-hbkid              = wa_bsid-hbkid.                   "Banco Empresa ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG

**************************************************************
      "buscar taxa de juros e multa da função nova
***************************************************************

      CLEAR:v_juros,v_multa,v_origem.

      CALL FUNCTION 'Z_BUSCA_JUROS_MULTA_BOLETO'
        EXPORTING
          i_belnr      = wa_bsid-belnr
          i_bukrs      = wa_bsid-bukrs
        IMPORTING
          e_origem     = v_origem
          e_juros      = v_juros
          e_multa      = v_multa
          e_nro_sol_ov = wa_saida-nro_sol_ov
          e_seq_lcto   = wa_saida-seq_lcto.

      wa_saida-origem   = v_origem.
      wa_saida-tx_juros = v_juros.
      wa_saida-tx_multa = v_multa.

      IF wa_saida-tx_juros > 0 OR wa_saida-tx_multa > 0.
        REFRESH: wa_saida-style.
        PERFORM block_field_taxa USING 'TX_JUROS' 'X'.
        PERFORM block_field_taxa USING 'TX_MULTA' 'X'.
      ENDIF.

      IF wa_saida-origem IS INITIAL.
        PERFORM block_field_taxa USING 'TX_JUROS' 'X'.
        PERFORM block_field_taxa USING 'TX_MULTA' 'X'.
      ENDIF.
*** US - 62755 - Fim - CBRAND
      IF wa_saida-dt_vct LT sy-datum.
        wa_saida-icon_sit           = icon_alert.                     "Situação
      ELSE.
        wa_saida-icon_sit           = icon_resubmission.              "Situação
      ENDIF.
      wa_saida-dias_atraso        = sy-datum - wa_saida-dt_vct.       "Dias Atraso   = Data Atual - XVCTO
      IF wa_saida-dias_atraso LT 0.
        wa_saida-dias_atraso = 0.
      ENDIF.

      wa_saida-dmbtr              = wa_bsid-dmbtr.                    "Vlr.Titulo
      wa_saida-bukrs              = wa_bsid-bukrs. "SHDB
      wa_saida-xref2              = wa_bsid-xref2. "ALV
      wa_saida-xref3              = wa_bsid-xref3. "ALV
      v_vbeln = wa_saida-xref2.

      "nseq_arq = wa_zfit0056-seq_arq.
      ADD 1 TO nseq_arq.
      vseq_arq = nseq_arq.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vseq_arq
        IMPORTING
          output = vseq_arq.

      SORT it_zfit0194 BY cpudt DESCENDING cputm DESCENDING.

      READ TABLE it_zfit0194 INTO wa_zfit0194 WITH KEY ident_tit_empr = wa_0159-id_transacao_financeira.

      IF sy-subrc = 0.
        READ TABLE it_bsad INTO wa_bsad WITH KEY  bukrs   = wa_bsid-bukrs
                                                  belnr   = wa_bsid-belnr
                                                  gjahr   = wa_bsid-gjahr BINARY SEARCH.
        IF sy-subrc NE 0.
          IF r_liq  = 'X'.
            CONTINUE.
          ENDIF.
          IF r_ven  = 'X' AND wa_saida-dt_vct GE sy-datum.
            CONTINUE.
          ENDIF.
          IF wa_saida-augbl IS INITIAL AND ( wa_zfit0194-seq2+1(2) = '06' OR  "pagamento pelo cliente efetuado
             wa_zfit0194-seq2+1(2) = '17' ).
            wa_saida-augbl               = icon_generate.
          ENDIF.
        ELSE.
          IF r_ven  = 'X' AND wa_saida-dt_vct GE sy-datum.
            CONTINUE.
          ENDIF.
          IF r_abe  = 'X'.
            CONTINUE.
          ENDIF.
          wa_saida-augbl              = wa_bsad-augbl.                    "Compensado
        ENDIF.
        IF wa_zfit0194-seq2 = ' 06'.

          CONCATENATE wa_zfit0194-dt_credito+6(2) '.' wa_zfit0194-dt_credito+4(2) '.' wa_zfit0194-dt_credito(4) INTO wa_saida-dt_credito.

          wa_saida-vl_liq_creditado   = wa_zfit0194-vlr_principal.     "Vlr.Credito
          wa_saida-vl_tarifa_cobranca = wa_zfit0194-tarifa_cobranca.
          wa_saida-vl_jr_mt_enc       = wa_zfit0194-juros_mora_multa.         "Vlr.Jros/Multa/Enc.
          wa_saida-vl_abatimento      = wa_zfit0194-vlr_abatimento.    "Vlr.Abat.Conc.
          wa_saida-vl_iof             = wa_zfit0194-vlr_iof.           "Vlr.IOF
          wa_saida-vl_pago_sacado     = wa_zfit0194-vlr_titulo.        "Vlr.Pg.Sacado

        ENDIF.

        READ TABLE it_zfit0058 INTO wa_zfit0058 WITH KEY cod_mov = wa_zfit0194-seq2+1(2) BINARY SEARCH.
        IF sy-subrc NE 0.
        ELSE.
          wa_saida-descricao          = wa_zfit0058-descricao.            "Arq.Retorno
        ENDIF.

      ENDIF.

      "// iNICIO WBARBOSA US-163166 23/01/2025

*** US - 172238 - Inicio - CBRAND
      IF  lv_exibir = 'X' AND wa_0159-id_transacao_financeira IS INITIAL.
        READ TABLE it_zfit0056 INTO DATA(ls_zfit0056) WITH KEY cod_arq = <fs_zfit0056>-cod_arq.
      ELSE.
        READ TABLE it_zfit0056 INTO ls_zfit0056 WITH KEY nosso_nro = wa_0159-id_transacao_financeira.
      ENDIF.

      READ TABLE it_zfit0054 INTO DATA(ls_zfit0054)
          WITH KEY cod_arq = ls_zfit0056-cod_arq
              BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_saida-origem_receb = ls_zfit0054-origem_receb.
      ENDIF.

*      READ TABLE it_zfit0056 INTO DATA(ls_zfit0056) WITH KEY nosso_nro = wa_0159-id_transacao_financeira.
*      IF sy-subrc IS INITIAL.
*** US - 172238 - Fim - CBRAND
      IF sy-subrc IS INITIAL.

        READ TABLE it_zfit0057 INTO DATA(ls_zfit0057) WITH KEY cod_arq = ls_zfit0056-cod_arq.

        READ TABLE it_bsad INTO wa_bsad WITH KEY  bukrs   = wa_bsid-bukrs
                                                  belnr   = wa_bsid-belnr
                                                  gjahr   = wa_bsid-gjahr BINARY SEARCH.
        IF sy-subrc NE 0.
          IF r_liq  = 'X'.
            CONTINUE.
          ENDIF.
          IF r_ven  = 'X' AND wa_saida-dt_vct GE sy-datum.
            CONTINUE.
          ENDIF.
          IF wa_saida-augbl IS INITIAL AND ( ls_zfit0056-cod_retorno = '06' OR  "pagamento pelo cliente efetuado
             ls_zfit0056-cod_retorno = '17' ).
            wa_saida-augbl               = icon_generate.
          ENDIF.
        ELSE.
          IF r_ven  = 'X' AND wa_saida-dt_vct GE sy-datum.
            CONTINUE.
          ENDIF.
          IF r_abe  = 'X'.
            CONTINUE.
          ENDIF.
          wa_saida-augbl              = wa_bsad-augbl.                    "Compensado
        ENDIF.
        IF ls_zfit0056-cod_retorno = '06'.

          CONCATENATE ls_zfit0057-dt_credito+6(2) '.' ls_zfit0057-dt_credito+4(2) '.' ls_zfit0057-dt_credito(4) INTO wa_saida-dt_credito.

          wa_saida-vl_liq_creditado   = ls_zfit0057-vl_liq_creditado.     "Vlr.Credito
*            WA_SAIDA-VL_TARIFA_COBRANCA = LS_ZFIT0057-.
          wa_saida-vl_jr_mt_enc       = ls_zfit0057-vl_jr_mt_enc.         "Vlr.Jros/Multa/Enc.
          wa_saida-vl_abatimento      = ls_zfit0057-vl_abatimento.    "Vlr.Abat.Conc.
          wa_saida-vl_iof             = ls_zfit0057-vl_iof.           "Vlr.IOF
          wa_saida-vl_pago_sacado     = ls_zfit0057-vl_pago_sacado.        "Vlr.Pg.Sacado

        ENDIF.

        READ TABLE it_zfit0058 INTO wa_zfit0058 WITH KEY cod_mov = ls_zfit0056-cod_retorno.
        IF sy-subrc IS INITIAL.
          wa_saida-descricao          = wa_zfit0058-descricao.            "Arq.Retorno
        ENDIF.

      ENDIF.

*      ENDIF.
      "// FIM WBARBOSA US-163166 23/01/2025

      wa_saida-gsber              = wa_bsid-gsber.                   "Filial

      READ TABLE it_t012k INTO wa_t012k WITH KEY bukrs = wa_bsid-bukrs
                                                 hbkid = wa_bsid-hbkid BINARY SEARCH.

      wa_saida-hkont              = wa_t012k-hkont. "SHDB

** US - 172238 - CBRAND - Inicio
      IF wa_saida-origem_receb = 'PIX'.
        SELECT SINGLE *
           FROM setleaf
        INTO @DATA(lwa_setleaf)
          WHERE setname = 'CONTA_BAIXA_ECOMMERCE'
          AND   valfrom = @wa_t012k-hkont.

        IF lwa_setleaf IS NOT INITIAL.
          wa_saida-hkont              = lwa_setleaf-valto. "SHDB
        ENDIF.
      ENDIF.
** US - 172238 - CBRAND - Fim

      wa_saida-bukrs              = wa_bsid-bukrs. "SHDB
      wa_saida-waers              = wa_bsid-waers. "SHDB
      wa_saida-xref2              = wa_bsid-vbel2. "SHDB
      wa_saida-xref3              = wa_bsid-xref3. "SHDB
      wa_saida-umsks              = wa_bsid-umsks. "SHDB
      v_vbeln = wa_saida-xref2.
      SELECT SINGLE spart
        FROM vbak
        INTO v_spart
        WHERE vbeln = v_vbeln.
      IF sy-subrc = 0.
        SELECT SINGLE vtext
          FROM tspat
          INTO v_vtext
          WHERE spras = sy-langu
          AND   spart = v_spart.
        CONCATENATE v_spart '-' v_vtext INTO wa_saida-spart.
      ENDIF.
      APPEND wa_saida TO it_saida.
      "CLEAR: wa_saida, wa_zfit0194 "US - 172238 - CBRAND
      CLEAR: wa_saida, wa_zfit0194, lwa_setleaf, ls_zfit0054, ls_zfit0056. "US - 172238 - CBRAND
    ENDIF.

  ENDLOOP.
****************************------------------------*********************************
  "*E-commerce - Ajustes transação ZFI0037 #109706 - BG - FIM
****************************------------------------*********************************

ENDFORM.                    " F_MONTA_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .
  IF r_nen NE 'X'.
    PERFORM f_alv_fieldcat.
  ELSE.
    PERFORM f_alv_fieldcat_nao.
  ENDIF.

  wa_layout-zebra      = 'X'.
  wa_layout-no_rowmove = 'X'.
  wa_layout-no_rowins  = 'X'.
  wa_layout-no_rowmark = space.

  IF r_nen = 'X'. "Checkbox
    wa_layout-grid_title = 'Títulos não enviados p/ Banco'.
  ELSE. " RadioButton
    IF r_abe = 'X'.
      wa_layout-grid_title = 'Em Aberto'.
    ELSEIF r_liq = 'X'.
      wa_layout-grid_title = 'Liquidados'.
    ELSEIF r_tod = 'X'.
      wa_layout-grid_title = 'Todos'.
    ELSEIF r_ven = 'X'.
      wa_layout-grid_title = 'Vencidos'.
    ELSE.
      CLEAR wa_layout-grid_title .
    ENDIF.
  ENDIF.
  wa_layout-sel_mode   = 'A'.
  wa_layout-cwidth_opt   = 'X'.
  "WA_LAYOUT-BOX_FNAME       = 'MARK'.
  CALL SCREEN 0100.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.

  REFRESH: fcode.
  READ TABLE t_usermd WITH KEY from = sy-uname.

  IF sy-subrc <> 0. "IF sy-subrc NE 0 OR r_nen EQ 'X'.
    APPEND '&LIQT' TO fcode.
  ENDIF.

  SET PF-STATUS 'F_SET_PF' EXCLUDING fcode.
  SET TITLEBAR  'ZFTITLE'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  IF NOT cl_grid IS INITIAL.

    PERFORM zf_alv_header.
    CALL METHOD cl_grid->refresh_table_display.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT obj_dyndoc_id
      EXPORTING
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
        no_margins = 'X'.

    PERFORM zf_alv_header .

    IF editcontainer IS INITIAL .
      CREATE OBJECT editcontainer
        EXPORTING
          container_name = 'HEADER'.
    ENDIF .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = editcontainer
      EXCEPTIONS
        html_display_error = 1.


    CREATE OBJECT cl_grid
      EXPORTING
        i_parent = cl_container_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    CALL METHOD cl_grid->check_changed_data. "US - 62755 - CSB
    cl_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).  "US - 62755 - CSB

    CALL METHOD cl_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    wa_stable-row        = c_x.
    wa_layout-stylefname = 'STYLE'.

    wg_save = 'X'.
    "WA_LAYOUT-INFO_FNAME    = 'LINE_COLOR'.
    wg_x_variant-report      = sy-repid.
    CALL METHOD cl_grid->set_table_for_first_display
      EXPORTING
        is_variant      = wg_x_variant
        is_layout       = wa_layout
        i_save          = wg_save
        i_default       = 'X'
      CHANGING
        it_fieldcatalog = it_fieldcat[]
        it_sort         = i_sort[]
        it_outtab       = it_saida[].


    SET HANDLER:
            lcl_event_handler=>catch_hotspot   FOR cl_grid,
            lcl_event_handler=>on_data_changed FOR cl_grid.

  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'UP'.
      REFRESH it_saida.
      CALL METHOD cl_grid->refresh_table_display.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN '&LIQT'.
      xcontador = 0.
      LOOP AT it_saida INTO wa_saida WHERE checkbox = 'X'.
        ADD 1 TO xcontador.
      ENDLOOP.
      IF xcontador GT 0. " Executar apenas os selecionados.
        LOOP AT it_saida INTO wa_saida WHERE checkbox = 'X'.
          tabix = sy-tabix.
          IF wa_saida-augbl               = icon_generate.
            CLEAR wl_erro.
            PERFORM f_shdb USING wa_saida CHANGING wl_erro.
            IF wl_erro NE 'X'.
              wa_saida-augbl = wg_documento.
              MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING augbl.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE. " Executar todos prontos para GERAR
        LOOP AT it_saida INTO wa_saida.
          tabix = sy-tabix.
          IF wa_saida-augbl              = icon_generate.
            CLEAR wl_erro.
            PERFORM f_shdb USING wa_saida CHANGING wl_erro.
            IF wl_erro NE 'X'.
              wa_saida-augbl = wg_documento.
              MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING augbl.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_fieldcat .
  REFRESH it_fieldcat.
  DATA i TYPE i.
  wa_afield-tabname     = 'IT_SAIDA'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CHECKBOX'.
  wa_afield-checkbox      = 'X'.
  wa_afield-scrtext_s     = 'Chk'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = 'X'.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DESCRICAO'.
  wa_afield-icon          = 'X'.
  wa_afield-scrtext_s     = 'Arq.Retorno'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'AUGBL'.
  wa_afield-icon          = 'X'.
  wa_afield-hotspot       = 'X'.
  wa_afield-scrtext_s     = 'Compensado'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'KUNNR'.
  wa_afield-scrtext_s     = 'Cliente'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAME1'.
  wa_afield-scrtext_m     = 'Nome do Cliente'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'SPART'.
  wa_afield-icon          = ' '.
  wa_afield-scrtext_s     = 'Setor Atividade'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BUDAT'.
  wa_afield-scrtext_m     = 'Dt.Lcto'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BELNR'.
  wa_afield-scrtext_m     = 'Nro.Doc'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-hotspot       = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'XBLNR'.
  wa_afield-scrtext_m     = 'Nro.Fatura'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-hotspot       = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NRO_SOL_OV'.
  wa_afield-scrtext_m     = 'Nro.Sol.Ov.'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-hotspot       = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.


*** US - 62755 - Inicio - CBRAND
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'SEQ_LCTO'.
  wa_afield-scrtext_m     = 'Seq.Lançamento'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-hotspot       = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.
*** US - 62755 - Fim - CBRAND

*** ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG inicio
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'HBKID'.
  wa_afield-scrtext_m     = 'Conta Empresa'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.
*** ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG fim

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DT_VCT'.
  wa_afield-scrtext_m     = 'Dt.Vcto'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ICON_SIT'.
  wa_afield-icon          = 'X'.
  wa_afield-scrtext_s     = 'Situação'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DIAS_ATRASO'.
  wa_afield-scrtext_m     = 'Dias Atraso'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DMBTR'.
  wa_afield-scrtext_m     = 'Vlr.Titulo'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DT_CREDITO'.
  wa_afield-scrtext_m     = 'Dt.Pgto'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VL_LIQ_CREDITADO'.
  wa_afield-scrtext_m     = 'Vlr.Credito'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  "*E-commerce - Ajustes transação ZFI0037 #109706 - BG - INICIO

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VL_TARIFA_COBRANCA'.
  wa_afield-scrtext_m     = 'Vlr.Tarifa Cobrança'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.
  "*E-commerce - Ajustes transação ZFI0037 #109706 - BG - FIM
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VL_JR_MT_ENC'.
  wa_afield-scrtext_m     = 'Vlr.Jros/Multa/Enc.'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VL_ABATIMENTO'.
  wa_afield-scrtext_m     = 'Vlr.Abat.Conc.'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VL_IOF'.
  wa_afield-scrtext_m     = 'Vlr.IOF'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'GSBER'.
  wa_afield-scrtext_m     = 'Filial'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VL_PAGO_SACADO'.
  wa_afield-scrtext_m     = 'Vlr.Pg.Sacado'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'LAUFI'.
  wa_afield-scrtext_m     = 'Ident.Arq.'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'LAUFD'.
  wa_afield-scrtext_m     = 'Dt.Arq.'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.


*** US - 62755 - Inicio - CBRAND
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ORIGEM'.
  wa_afield-scrtext_m     = 'Origem'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TX_MULTA'.
  wa_afield-ref_table     = 'ZFIWRT0011'.
  wa_afield-ref_field     = 'TAXA_MULTA'.
  wa_afield-edit          = 'X'.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TX_JUROS'.
  wa_afield-ref_table     = 'ZFIWRT0011'.
  wa_afield-ref_field     = 'TAXA_JUROS'.
  wa_afield-edit          = 'X'.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.
*** US - 62755 - Fim - CBRAND

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_fieldcat_nao .
  REFRESH it_fieldcat.
  DATA i TYPE i.
  wa_afield-tabname     = 'IT_SAIDA'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DESCRICAO'.
  wa_afield-icon          = 'X'.
  wa_afield-scrtext_s     = 'Arq.Enviado'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.


  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'KUNNR'.
  wa_afield-scrtext_s     = 'Cliente'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAME1'.
  wa_afield-scrtext_m     = 'Nome do Cliente'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'SPART'.
  wa_afield-icon          = ' '.
  wa_afield-scrtext_s     = 'Setor Atividade'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BUDAT'.
  wa_afield-scrtext_m     = 'Dt.Lcto'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BELNR'.
  wa_afield-scrtext_m     = 'Nro.Doc'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-hotspot       = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'XBLNR'.
  wa_afield-scrtext_m     = 'Nro.Fatura'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-hotspot       = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NRO_SOL_OV'.
  wa_afield-scrtext_m     = 'Nro.Sol.Ov.'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-hotspot       = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

*** US - 62755 - Inicio - CBRAND
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'SEQ_LCTO'.
  wa_afield-scrtext_m     = 'Seq.Lançamento'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-hotspot       = 'X'.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.
*** US - 62755 - Fim - CBRAND

*** ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG inicio
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'HBKID'.
  wa_afield-scrtext_m     = 'Conta Empresa'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.
*** ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG fim

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DT_VCT'.
  wa_afield-scrtext_m     = 'Dt.Vcto'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ICON_SIT'.
  wa_afield-icon          = 'X'.
  wa_afield-scrtext_s     = 'Situação'.
  wa_afield-scrtext_l     = wa_afield-scrtext_s.
  wa_afield-scrtext_m     = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DIAS_ATRASO'.
  wa_afield-scrtext_m     = 'Dias Atraso'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DMBTR'.
  wa_afield-scrtext_m     = 'Vlr.Titulo'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'XREF2'.
  wa_afield-scrtext_m     = 'Documento'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'XREF3'.
  wa_afield-scrtext_m     = 'Nosso Numero'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.


*** US - 62755 - Inicio - CBRAND
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ORIGEM'.
  wa_afield-scrtext_m     = 'Origem'.
  wa_afield-scrtext_l     = wa_afield-scrtext_m.
  wa_afield-scrtext_s     = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TX_MULTA'.
  wa_afield-ref_table     = 'ZFIWRT0011'.
  wa_afield-ref_field     = 'TAXA_MULTA'.
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
***  wa_afield-edit          = 'X'.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TX_JUROS'.
  wa_afield-ref_table     = 'ZFIWRT0011'.
  wa_afield-ref_field     = 'TAXA_JUROS'.
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
***  wa_afield-edit          = 'X'.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.
*** US - 62755 - Fim - CBRAND

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_alv_header .
  DATA:   wl_data(10),
          wl_hora(8),
          wl_linha(60),
          wl_text TYPE sdydo_text_element.


  CONCATENATE  'Empresa:' s_bukrs-low
          INTO wl_linha SEPARATED BY space.
  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      sap_fontsize = cl_dd_area=>list_normal.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  IF s_kunnr IS NOT INITIAL.
    IF s_kunnr-high IS INITIAL.
      CONCATENATE 'Cliente  :' s_kunnr-low
       INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE 'Cliente  :' s_kunnr-low 'à' s_kunnr-high
      INTO wl_linha SEPARATED BY space.
    ENDIF.
    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  ENDIF.


  IF s_dtlct IS NOT INITIAL.
    CONCATENATE s_dtlct-low+6(2) s_dtlct-low+4(2) s_dtlct-low+0(4) INTO  wl_data SEPARATED BY '.'.
    IF s_dtlct-high IS INITIAL.
      CONCATENATE 'Data Lançamento  :' wl_data
      INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE 'Data Lançamento  :' wl_data  INTO wl_linha SEPARATED BY space.
      CONCATENATE s_dtlct-high+6(2) s_dtlct-high+4(2) s_dtlct-high+0(4) INTO  wl_data SEPARATED BY '.'.
      CONCATENATE wl_linha 'à' wl_data  INTO wl_linha SEPARATED BY space.
    ENDIF.
    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF s_dtvct IS NOT INITIAL.
    CONCATENATE s_dtvct-low+6(2) s_dtvct-low+4(2) s_dtvct-low+0(4) INTO  wl_data SEPARATED BY '.'.
    IF s_dtvct-high IS INITIAL.
      CONCATENATE 'Data Vencimento  :' wl_data
      INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE 'Data Vencimento  :' wl_data  INTO wl_linha SEPARATED BY space.
      CONCATENATE s_dtvct-high+6(2) s_dtvct-high+4(2) s_dtvct-high+0(4) INTO  wl_data SEPARATED BY '.'.
      CONCATENATE wl_linha 'à' wl_data  INTO wl_linha SEPARATED BY space.
    ENDIF.
    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM f_shdb  USING    p_saida LIKE wa_saida
             CHANGING p_erro.
*-----------------------------------------------------------
  "*E-commerce - Ajustes transação ZFI0037 #109706 - BG - inicio
*-----------------------------------------------------------
  DATA: vdata(10),
        wl_vlr(16),
        wl_vlr2(16).

  WRITE p_saida-vl_liq_creditado TO wl_vlr.

  REFRESH ti_bdcdata.

  IF  p_saida-vl_tarifa_cobranca IS INITIAL.
    PERFORM f_bdc_data USING:
      'SAPMF05A'  '0103'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '/00',
      ''          ''      ''   'BKPF-BLDAT'       p_saida-dt_credito,
      ''          ''      ''   'BKPF-BLART'       'DZ',
      ''          ''      ''   'BKPF-BUKRS'       p_saida-bukrs,
      ''          ''      ''   'BKPF-BUDAT'       p_saida-dt_credito,
      ''          ''      ''   'BKPF-MONAT'       p_saida-dt_credito+3(2),
      ''          ''      ''   'BKPF-WAERS'       p_saida-waers,
      ''          ''      ''   'BKPF-BKTXT'       p_saida-xref3,
      ''          ''      ''   'RF05A-KONTO'      p_saida-hkont,
      ''          ''      ''   'BSEG-GSBER'       p_saida-gsber,
      ''          ''      ''   'BSEG-WRBTR'       wl_vlr,
      ''          ''      ''   'RF05A-AGKON'      p_saida-kunnr,
      ''          ''      ''   'RF05A-AGKOA'      'D'.
    IF p_saida-umsks NE ''. " Adto
      PERFORM f_bdc_data USING:
             ''          ''      ''   'RF05A-AGUMS'      'AF'.
    ENDIF.
    PERFORM f_bdc_data USING:
      ''          ''      ''   'RF05A-XNOPS'      'X',
      ''          ''      ''   'RF05A-XPOS1(01)'  '',
      ''          ''      ''   'RF05A-XPOS1(03)'  'X',

      'SAPMF05A'  '0731'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=PA',
      ''          ''      ''   'RF05A-SEL01(01)'  p_saida-belnr,

      'SAPDF05X'  '3100'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=PI',

      'SAPDF05X'  '3100'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=BS',
      ''          ''      ''   'BDC_CURSOR'       'DF05B-PSBET(01)',
      ''          ''      ''   'RF05A-ABPOS'      '1',

      'SAPMF05A'  '0700'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'	      '=BU'.

  ELSE.

    WRITE p_saida-vl_tarifa_cobranca TO wl_vlr2.
    PERFORM f_bdc_data USING:
  'SAPMF05A'  '0103'  'X'        ''                  ' ',
  ''          ''      ''      'BDC_CURSOR'      'RF05A-XPOS1(03)',
  ''          ''      ''      'BDC_OKCODE'      '=PA',
  ''          ''      ''      'BKPF-BLDAT'      p_saida-dt_credito,
  ''          ''      ''      'BKPF-BLART'      'DZ',
  ''          ''      ''      'BKPF-BUKRS'      p_saida-bukrs,
  ''          ''      ''      'BKPF-BUDAT'      p_saida-dt_credito,
  ''          ''      ''      'BKPF-MONAT'      p_saida-dt_credito+3(2),
  ''          ''      ''      'BKPF-WAERS'      p_saida-waers,
  ''          ''      ''      'RF05A-KONTO'     p_saida-hkont,
  ''          ''      ''      'BSEG-GSBER'      p_saida-gsber,
  ''          ''      ''      'BSEG-WRBTR'      wl_vlr,
  ''          ''      ''      'RF05A-AGKON'     p_saida-kunnr,
  ''          ''      ''      'RF05A-AGKOA'     'D',
  ''          ''      ''      'RF05A-AGUMS'     'F',
  ''          ''      ''      'RF05A-XNOPS'     'X',
  ''          ''      ''      'RF05A-XPOS1(01)' ' ',
  ''          ''      ''      'RF05A-XPOS1(03)' 'X',
  'SAPMF05A'  '0731'  'X'     ''                ' ',
  ''           ''     ''      'BDC_CURSOR'      'RF05A-SEL01(01)',
  ''           ''     ''      'BDC_OKCODE'      '=PA',
  ''           ''     ''      'RF05A-SEL01(01)'	p_saida-belnr,
  'SAPDF05X'  '3100'  'X'     ''                ' ',
  ''           ''     ''      'BDC_OKCODE'      '=KMD',
  ''           ''     ''      'BDC_SUBSCR'      'SAPDF05X                                6102PAGE',
  ''           ''     ''      'BDC_CURSOR'      'DF05B-PSSKT(01)',
  ''           ''     ''      'RF05A-ABPOS'     '1',
  'SAPMF05A'  '0700'  'X'     ''                ' ',
  ''          ''      ''      'BDC_CURSOR'      'RF05A-NEWKO',
  ''          ''      ''      'BDC_OKCODE'      '/00',
  ''          ''      ''      'RF05A-NEWBS'     '40',
  ''          ''      ''      'RF05A-NEWKO'     '422513',
  'SAPMF05A'  '0300'  'X'     ''                ' ',
  ''          ''      ''      'BDC_CURSOR'      'BSEG-BUPLA',
  ''          ''      ''      'BDC_OKCODE'      '=PA',
  ''          ''      ''      'BSEG-WRBTR'      wl_vlr2,
  ''          ''      ''      'BSEG-BUPLA'      p_saida-gsber,
  ''          ''      ''      'BDC_SUBSCR'      'SAPLKACB                                0001BLOCK',
  ''          ''      ''      'DKACB-FMORE'     'X',
  'SAPLKACB'  '0002'  'X'     ''                ' ',
  ''          ''      ''      'BDC_CURSOR'      'COBL-GSBER',
  ''          ''      ''      'BDC_OKCODE'      '=ENTE',
  ''          ''      ''      'COBL-GSBER'      p_saida-gsber,
  ''          ''      ''      'BDC_SUBSCR'      'SAPLKACB                                0003BLOCK1',
  'SAPDF05X'  '3100'  'X'     ''                ' ',
  ''          ''      ''      'BDC_OKCODE'      '=BU',
  ''          ''      ''      'BDC_SUBSCR'      'SAPDF05X                                6102PAGE',
  ''          ''      ''      'BDC_CURSOR'      'DF05B-PSSKT(01)',
  ''          ''      ''      'RF05A-ABPOS'     '1'.


  ENDIF.
*-----------------------------------------------------------
  "*E-commerce - Ajustes transação ZFI0037 #109706 - BG - fim
*-----------------------------------------------------------
  CLEAR p_erro.
  PERFORM zf_call_transaction USING 'F-28' CHANGING p_erro.

  WAIT UP TO 5 SECONDS.

  IF p_erro = 'X'.

*SE DEU ERRO NO SHDB POREM O USUARIO GRAVOU MANUALMENTE VERIFICA SE FO FEITA A COMPENSAÇÃO PARA GRAVAR NA TABELA ZFIT0026.
    SELECT SINGLE augbl
    FROM bsad_view
    INTO @DATA(v_doc_compensacao)
    WHERE bukrs EQ @p_saida-bukrs
      AND gjahr EQ @p_saida-budat(4)
      AND belnr EQ @p_saida-belnr.

    IF sy-subrc IS INITIAL.
      COMMIT WORK.
***    Grava informações na tabela ZFIT0026.
      zcl_dados_zfit0026=>processa_dados_zfit0026( EXPORTING i_saida = p_saida ).

    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ELSE.
    COMMIT WORK.

***    Grava informações na tabela ZFIT0026.
    zcl_dados_zfit0026=>processa_dados_zfit0026( EXPORTING i_saida = p_saida ).

*** US - 172238 - Inicio - CBRAND
    IF p_saida-origem_receb = 'COD.BARRAS'.
      zcl_dados_zib_contabil=>processa_dados_zib_contabil( EXPORTING i_saida = p_saida ).
    ENDIF.
*** US - 172238 - Fim - CBRAND
  ENDIF.
ENDFORM.                    " F_SHDB

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_program   programa
*      -->P_dynpro    tela
*      -->P_start     define a tela
*      -->P_fnam      nome do campo ou comando
*      -->P_fval      conteúdo do campo ou comando
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA


*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*----------------------------------------------------------------------*
FORM zf_call_transaction USING p_trans CHANGING p_erro.
  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  REFRESH it_msg.

  wl_mode = 'E'.

  CALL TRANSACTION p_trans USING ti_bdcdata
        MODE wl_mode
        MESSAGES INTO it_msg.

  READ TABLE it_msg WITH KEY msgtyp = 'A'.
  IF sy-subrc = 0.
    p_erro = 'X'.
  ELSE.
    READ TABLE it_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      p_erro = 'X'.
    ENDIF.
  ENDIF.

  CLEAR wg_documento.
  READ TABLE it_msg WITH KEY msgid = c_msgid
                             msgnr = c_msgnr
                             msgtyp = 'S'.

  IF sy-subrc = 0.
    MOVE it_msg-msgv1 TO wg_documento.
  ENDIF.

  IF  wg_documento IS INITIAL.
    p_erro = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_documento
      IMPORTING
        output = wg_documento.
  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_NAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_nao .

  DATA: lva_text          TYPE string,
        lva_amount        TYPE string,
        lva_num           TYPE i,
        lva_doc_simulacao TYPE zsdt0040-doc_simulacao.

  SELECT *
    FROM bsid_view
    INTO TABLE @it_bsid
  WHERE bukrs	 IN @s_bukrs
  AND   zlsch  IN ('D', 'X')
  AND   hbkid  NE 'AL5' "*E-commerce - Ajustes transação ZFI0037 #109706 - BG
  AND   umskz  IN ('F','')
  AND   kunnr  IN @s_kunnr
  AND   budat  IN @s_dtlct.

  CHECK it_bsid[] IS NOT INITIAL.

*** Inicio - Rubenilson  - 13.03.2025 #169641
  DATA(lt_bsid) = it_bsid.

  DELETE lt_bsid WHERE xref3 IS INITIAL.
  SORT lt_bsid BY xref3.
  DELETE ADJACENT DUPLICATES FROM lt_bsid COMPARING xref3.
  IF lt_bsid IS NOT INITIAL.
    SELECT *
      FROM zfit0056
      INTO TABLE it_zfit0056
      FOR ALL ENTRIES IN lt_bsid
      WHERE nosso_nro = lt_bsid-xref3.
    IF sy-subrc IS INITIAL.
      SORT it_zfit0056 BY nosso_nro.

      DATA(lt_zfit0056) = it_zfit0056.
      SORT lt_zfit0056 BY cod_arq.
      DELETE ADJACENT DUPLICATES FROM lt_zfit0056 COMPARING cod_arq.

      SELECT *
        FROM zfit0054
        INTO TABLE it_zfit0054
        FOR ALL ENTRIES IN lt_zfit0056
        WHERE cod_arq = lt_zfit0056-cod_arq.
      IF sy-subrc IS INITIAL.
        SORT it_zfit0054 BY cod_arq.
      ENDIF.

    ENDIF.

  ENDIF.
*** Fim - Rubenilson  - 13.03.2025 #169641

  SELECT kunnr name1
  FROM kna1
  INTO TABLE it_kna1
  FOR ALL ENTRIES IN it_bsid
  WHERE kunnr = it_bsid-kunnr.

  it_bsid_aux[] = it_bsid[].
  DELETE it_bsid_aux WHERE umsks EQ ''.
  IF  it_bsid_aux[] IS NOT INITIAL.
    SELECT adiant nro_sol_ov
      FROM zsdt0054
      INTO TABLE it_zsdt0054
      FOR ALL ENTRIES IN it_bsid_aux
      WHERE adiant = it_bsid_aux-belnr.

    SELECT *
     FROM zsdt0159
     FOR ALL ENTRIES IN @it_bsid_aux
     WHERE adiant = @it_bsid_aux-belnr
      AND doc_simulacao IS NOT INITIAL
AND vbeln IS NOT INITIAL
AND seq IS NOT INITIAL
           INTO TABLE @it_zsdt0159.

  ENDIF.

  SELECT *
   FROM regup
     INTO TABLE it_regup
     FOR ALL ENTRIES IN it_bsid
     WHERE bukrs  EQ it_bsid-bukrs
     AND   belnr  EQ it_bsid-belnr
     AND   gjahr  EQ it_bsid-gjahr
     AND   xvorl  EQ ''
     AND   kunnr  NE ''.

  "US - 62755 - Inicio - CBRAND
  IF it_bsid IS NOT INITIAL.

* ZSDT0044 - Inicio

    LOOP AT it_bsid INTO wa_bsid.
      "VENDA 0000030914 - regup-xblnr
      lva_num = strlen( wa_bsid-xblnr ).
      lva_text = wa_bsid-xblnr.

      DO lva_num TIMES.
        IF lva_text(1) CA '0123456789'.
          CONCATENATE lva_amount lva_text(1) INTO lva_amount.
          CONDENSE lva_amount NO-GAPS.
        ENDIF.
        SHIFT lva_text LEFT CIRCULAR.
      ENDDO.

      MOVE lva_amount TO wa_regup_44-doc_simulacao.
      wa_regup_44-xblnr = wa_bsid-xblnr. "// wbarbosa - US-163166 - 23/01/2025

      IF wa_regup_44-doc_simulacao IS NOT INITIAL.
        wa_regup_44-belnr = wa_regup_rz-belnr.

        APPEND wa_regup_44 TO it_regup_44.
        CLEAR: wa_regup_44 .
      ENDIF.
      CLEAR: lva_text  ,
             lva_amount,
             lva_num,
             wa_bsid.
    ENDLOOP.

    IF it_regup_44 IS NOT INITIAL.
      SELECT  *
       FROM  zsdt0040
       INTO TABLE  it_zsdt0040
       FOR ALL ENTRIES IN it_regup_44
       WHERE doc_simulacao EQ it_regup_44-doc_simulacao
         AND ecommerce EQ abap_true. "// wbarbosa - US-163166 - 23/01/2025
    ENDIF.


* BUG - 92404 - Inicio
*    SELECT *
*      FROM zsdt0054
*      INTO TABLE it_zsdt0054_aux
*      FOR ALL ENTRIES IN it_bsid
*      WHERE adiant = it_bsid-belnr.
*
*    IF it_zsdt0054_aux IS NOT INITIAL.
*      SELECT  *
*       FROM  zsdt0051
*       INTO TABLE  it_zsdt0051
*       FOR ALL ENTRIES IN it_zsdt0054_aux
*       WHERE nro_sol_ov EQ it_zsdt0054_aux-nro_sol_ov.
*    ENDIF.
* BUG - 92404 - Fim

* SELECT NA BKPF para os processos seguintes
    SELECT  *
      FROM  bkpf
      INTO TABLE it_bkpf
          FOR ALL ENTRIES IN it_bsid
      WHERE bukrs EQ s_bukrs-low
        AND belnr EQ it_bsid-belnr.


* ZSDT0062 - Inicio
    LOOP AT it_bkpf INTO wa_bkpf.
      MOVE wa_bkpf-awkey TO wa_bkpf_62-awkey.
      wa_bkpf_62-belnr = wa_bkpf-belnr.

      APPEND wa_bkpf_62 TO it_bkpf_62.
      CLEAR: wa_bkpf_62.
    ENDLOOP.

    SELECT  *
      FROM  vbrp
      INTO TABLE it_vbrp
             FOR ALL ENTRIES IN it_bkpf_62
      WHERE vbeln = it_bkpf_62-awkey.

    IF it_vbrp IS NOT INITIAL.
      SELECT  *
       FROM  zsdt0053
       INTO TABLE it_zsdt0053
              FOR ALL ENTRIES IN it_vbrp
       WHERE vbeln = it_vbrp-aubel.

      SELECT  *
       FROM  zsdt0051
       INTO TABLE it_zsdt0051
              FOR ALL ENTRIES IN it_zsdt0053
       WHERE nro_sol_ov = it_zsdt0053-nro_sol_ov.
    ENDIF.
* ZSDT0062 - Fim

* znfw - inicio
    LOOP AT it_bkpf INTO wa_bkpf.

      MOVE wa_bkpf-awkey TO wa_znfw-obj_key.

      IF wa_znfw-obj_key IS NOT INITIAL.
        wa_znfw-bukrs = wa_bkpf-bukrs.
        wa_znfw-belnr = wa_bkpf-belnr.


        APPEND wa_znfw TO it_znfw.
        CLEAR: wa_znfw.
      ENDIF.

      CLEAR: lva_text  ,
             lva_amount,
             lva_num  .

    ENDLOOP.

    IF it_znfw IS NOT INITIAL.
      SELECT  *
      FROM   zfiwrt0008
      INTO TABLE it_zfiwrt0008
             FOR ALL ENTRIES IN it_znfw
      WHERE obj_key EQ it_znfw-obj_key
        AND bukrs = it_znfw-bukrs .

      IF it_zfiwrt0008 IS NOT INITIAL.
        SELECT  *
          FROM zfiwrt0011
       INTO TABLE it_zfiwrt0011
          FOR ALL ENTRIES IN it_zfiwrt0008
            WHERE seq_lcto  EQ it_zfiwrt0008-seq_lcto
              AND zlsch EQ 'D'
              AND estorno NE 'X'.
      ENDIF.

    ENDIF.
  ENDIF.
* BUG - 902404 - CBRAND - Inicio
*    SELECT *
*      FROM zsdt0159
*      INTO TABLE it_zsdt0159_aux
*      FOR ALL ENTRIES IN it_bsid
*      WHERE adiant = it_bsid-belnr.
*
*    IF it_zsdt0159 IS NOT INITIAL.
*      SELECT  *
*       FROM  zsdt0040
*       INTO TABLE it_zsdt0040
*      FOR ALL ENTRIES IN it_zsdt0159_aux
*       WHERE doc_simulacao = it_zsdt0159_aux-doc_simulacao.
*
*    ENDIF.
* ZSDT0062 - Fim
* BUG - 902404 - CBRAND - Fim

* ZNFW - Inicio


* BUG - 902404 - CBRAND - Inicio
*    SELECT *
*      FROM zib_contabil_chv
*    INTO TABLE it_zib_cont_chv
*      FOR ALL ENTRIES IN it_bsid
*        WHERE belnr = it_bsid-belnr.
*
*    IF it_zib_cont_chv IS NOT INITIAL.
*      SELECT  *
*      FROM   zfiwrt0008
*      INTO TABLE it_zfiwrt0008
*             FOR ALL ENTRIES IN it_zib_cont_chv
*      WHERE obj_key = it_zib_cont_chv-obj_key.
*
*      IF it_zfiwrt0008 IS NOT INITIAL.
*        SELECT  *
*          FROM zfiwrt0011
*       INTO TABLE it_zfiwrt0011
*          FOR ALL ENTRIES IN it_zfiwrt0008
*            WHERE seq_lcto  EQ it_zfiwrt0008-seq_lcto
*              AND zlsch EQ 'D'
*              AND estorno NE 'X'.
*      ENDIF.
*    ENDIF.
* BUG - 902404 - CBRAND - Fim
** US - 62755 - Fim - CBRAND
ENDFORM.                    " F_SELECIONA_NAO
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_SAIDA_NAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_saida_nao .
  SORT: it_zsdt0054 BY adiant,
        it_regup    BY bukrs belnr gjahr,
        it_kna1     BY kunnr.

  DATA: v_spart  TYPE vbak-spart.
  DATA: v_vbeln  TYPE vbak-vbeln.
  DATA: v_vtext	 TYPE tspat-vtext,
        v_origem TYPE	char30,
        v_juros  TYPE zde003,
        v_multa  TYPE zde003.

  LOOP AT it_bsid INTO wa_bsid.

*** Inicio - Rubenilson - 13.03.25 #169641
    READ TABLE it_zfit0056 ASSIGNING FIELD-SYMBOL(<fs_zfit0056>)
    WITH KEY nosso_nro = wa_bsid-xref3
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE it_zfit0054 ASSIGNING FIELD-SYMBOL(<fs_zfit0054>)
      WITH KEY cod_arq = <fs_zfit0056>-cod_arq
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF <fs_zfit0054>-api IS NOT INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
*** Fim - Rubenilson - 13.03.25 #169641

* Inicio "// wbarbosa - US-163166 - 23/01/2025
* "// Verifica se o processo é E-commerce case seja segue para o proximo
    READ TABLE it_regup_44 INTO DATA(ls_regup_44) WITH KEY xblnr = wa_bsid-xblnr.
    IF sy-subrc IS INITIAL.
      READ TABLE it_zsdt0040 TRANSPORTING NO FIELDS WITH KEY doc_simulacao = ls_regup_44-doc_simulacao.
      IF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.
* Fim "// wbarbosa - US-163166 - 23/01/2025

    READ TABLE it_regup INTO wa_regup WITH KEY bukrs = wa_bsid-bukrs
                                               belnr = wa_bsid-belnr
                                               gjahr = wa_bsid-gjahr BINARY SEARCH.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    wa_saida-descricao          = icon_message_warning_small.
    wa_saida-dt_vct             = wa_bsid-zfbdt + wa_bsid-zbd1t.  "Dt.Vcto
    IF s_dtvct IS NOT INITIAL.
      IF wa_saida-dt_vct NOT IN s_dtvct.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsid-kunnr BINARY SEARCH.
    wa_saida-kunnr              = wa_bsid-kunnr.                   "Cliente
    IF sy-subrc = 0.
      wa_saida-name1              = wa_kna1-name1.                    "Nome do Cliente
    ENDIF.

    wa_saida-budat              = wa_bsid-budat.                   "Dt.Lcto
    wa_saida-belnr              = wa_bsid-belnr.                   "Nro.doc.
    wa_saida-xblnr              = wa_bsid-xblnr.                   "Nro.Fatura
    wa_saida-hbkid              = wa_bsid-hbkid.                   "Banco Empresa "ZFI - ZFI0037 VAREDURA SACADO- BUG #138241 - BG

*Inicio Alteração - Leandro Valentim Ferreira - 15.06.23 - #111774

    CLEAR:v_juros,v_multa,v_origem.

    CALL FUNCTION 'Z_BUSCA_JUROS_MULTA_BOLETO'
      EXPORTING
        i_belnr      = wa_bsid-belnr
        i_bukrs      = wa_bsid-bukrs
      IMPORTING
        e_origem     = v_origem
        e_juros      = v_juros
        e_multa      = v_multa
        e_nro_sol_ov = wa_saida-nro_sol_ov
        e_seq_lcto   = wa_saida-seq_lcto.

    wa_saida-origem   = v_origem.
    wa_saida-tx_juros = v_juros.
    wa_saida-tx_multa = v_multa.

    IF wa_saida-tx_juros > 0 OR wa_saida-tx_multa > 0.
      REFRESH: wa_saida-style.
      PERFORM block_field_taxa USING 'TX_JUROS' 'X'.
      PERFORM block_field_taxa USING 'TX_MULTA' 'X'.
    ENDIF.


*Fim Alteração - Leandro Valentim Ferreira - 15.06.23 - #111774


    IF wa_saida-origem IS INITIAL.
      REFRESH: wa_saida-style.
      PERFORM block_field_taxa USING 'TX_JUROS' 'X'.
      PERFORM block_field_taxa USING 'TX_MULTA' 'X'.
    ENDIF.
*** US - 62755 - Fim - CBRAND



    IF wa_saida-dt_vct LT sy-datum.
      wa_saida-icon_sit           = icon_alert.                     "Situação
    ELSE.
      wa_saida-icon_sit           = icon_resubmission.              "Situação
    ENDIF.
    wa_saida-dias_atraso        = sy-datum - wa_saida-dt_vct.       "Dias Atraso   = Data Atual - XVCTO
    IF wa_saida-dias_atraso LT 0.
      wa_saida-dias_atraso = 0.
    ENDIF.
    wa_saida-dmbtr              = wa_bsid-dmbtr.                    "Vlr.Titulo
    wa_saida-bukrs              = wa_bsid-bukrs. "SHDB
    wa_saida-xref2              = wa_bsid-xref2. "ALV
    wa_saida-xref3              = wa_bsid-xref3. "ALV
    v_vbeln = wa_saida-xref2.

    SELECT SINGLE spart
      FROM vbak
      INTO v_spart
      WHERE vbeln = v_vbeln.
    IF sy-subrc = 0.
      SELECT SINGLE vtext
       FROM tspat
       INTO v_vtext
       WHERE spras = sy-langu
       AND   spart = v_spart.
      CONCATENATE v_spart '-' v_vtext INTO wa_saida-spart.
    ENDIF.
    APPEND wa_saida TO it_saida.
    CLEAR:  wa_saida,
            wa_zib_cont_chv,
            wa_zfiwrt0008 ,
            wa_zfiwrt0011,
            wa_zsdt0040,
            wa_zsdt0159,
            wa_zsdt0159_aux,
            wa_zsdt0051,
            wa_zsdt0054,
            wa_zsdt0054_aux.


  ENDLOOP.
ENDFORM.                    " F_MONTA_SAIDA_NAO

FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wg_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wg_bdc-program,
  l_value TO wg_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wg_bdc-fnam,
      l_value TO wg_bdc-fval.
  ENDIF.
  APPEND wg_bdc TO tg_bdc.
  CLEAR: wg_bdc.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BLOCK_FIELD_TAXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2732   text
*      -->P_2733   text
*----------------------------------------------------------------------*
FORM block_field_taxa  USING    p_field
                                p_block.

  CLEAR: wa_style.
  CLEAR: style[].

  wa_style-fieldname = p_field.

  IF  p_block = 'X'.
    wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
  ELSE.
    wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
  ENDIF.

  INSERT  wa_style INTO TABLE style.


  INSERT LINES OF style INTO TABLE wa_saida-style.

ENDFORM.
