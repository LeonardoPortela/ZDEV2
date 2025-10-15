*&---------------------------------------------------------------------*
*&  Include           ZFIY0038_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTAR
*&---------------------------------------------------------------------*
FORM f_executar .

  PERFORM: f_refresh,
           f_selecionar_dados,
           f_processa_dados.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM f_selecionar_dados .

  CASE abap_true.

    WHEN p_solper. "Solicitud de Permisos de Embarque X Cierre de cambio

      CLEAR: git_vbak[],
             git_vbfa[],
             git_zfiyt0032[],
             git_zfiyt0032_sld[],
             git_vbap[],
             git_vbkd[],
             git_vbfa[],
             git_vbfa_fat[],
             git_vbrk[],
             git_kna1[].

      DATA: it_auart  TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE.

      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          class         = '0000'
          setnr         = 'MAGGI_OV_PERMISO'
        TABLES
          set_values    = it_auart
        EXCEPTIONS
          set_not_found = 1
          OTHERS        = 2.

      CLEAR: rg_auart.
      LOOP AT it_auart.
        rg_auart-sign = 'I'.
        rg_auart-option = 'BT'.
        rg_auart-low  = it_auart-from.
        rg_auart-high = it_auart-to.
        APPEND rg_auart.
      ENDLOOP.

      SELECT *
         FROM vbak  AS a INTO CORRESPONDING FIELDS OF TABLE git_vbak
        WHERE a~auart IN rg_auart
          AND a~erdat IN p_erdat
          AND a~kunnr IN p_kunnr
          AND a~vbeln IN p_vbeln
          AND a~bstnk IN p_bstnk.


      IF git_vbak[] IS NOT INITIAL.
        SELECT *
          FROM vbfa APPENDING TABLE git_vbfa
           FOR ALL ENTRIES IN git_vbak
         WHERE vbelv EQ git_vbak-vbeln
          AND vbtyp_n   EQ  'H'.

        IF git_vbfa[] IS NOT INITIAL.
          CLEAR: gwa_vbfa, gwa_vbak.
          LOOP AT git_vbfa INTO gwa_vbfa.
            CLEAR: gva_idx .
            READ TABLE git_vbak INTO gwa_vbak WITH KEY vbeln = gwa_vbfa-vbelv.
            IF sy-subrc = 0.
              gva_idx = sy-tabix.
              gwa_vbak-vbeln = 0.
              MODIFY git_vbak INDEX gva_idx FROM gwa_vbak TRANSPORTING vbeln.
            ENDIF.
          ENDLOOP.
          DELETE git_vbak WHERE vbeln = 0.
        ENDIF.
      ENDIF.

      IF git_vbak[] IS NOT INITIAL.
* Verificar Ordem de Venda Aplicada:
        SELECT *
          FROM zfiyt0032 APPENDING TABLE git_zfiyt0032
           FOR ALL ENTRIES IN git_vbak
         WHERE vbeln EQ git_vbak-vbeln.

        IF git_zfiyt0032[] IS NOT INITIAL.
          CLEAR: gwa_vbak.
          LOOP AT git_zfiyt0032 INTO gwa_zfiyt0032.
            CLEAR: gva_idx .
            READ TABLE git_vbak INTO gwa_vbak WITH KEY vbeln = gwa_zfiyt0032-vbeln.
            IF sy-subrc = 0.
              gva_idx = sy-tabix.
              gwa_vbak-vbeln = 0.
              MODIFY git_vbak INDEX gva_idx FROM gwa_vbak TRANSPORTING vbeln.
            ENDIF.
          ENDLOOP.
          DELETE git_vbak WHERE vbeln = 0.
        ENDIF.
      ENDIF.

      IF git_vbak[] IS NOT INITIAL.

* Item da Ordem de Venda:
        SELECT *
        FROM vbap APPENDING TABLE git_vbap
         FOR ALL ENTRIES IN git_vbak
       WHERE vbeln EQ git_vbak-vbeln.

        SELECT *
        FROM vbkd APPENDING TABLE git_vbkd
        FOR ALL ENTRIES IN git_vbak
        WHERE vbeln EQ git_vbak-vbeln.

* Remessa:
        CLEAR: git_vbfa.
        SELECT *
          FROM vbfa APPENDING TABLE git_vbfa
           FOR ALL ENTRIES IN git_vbak
         WHERE vbelv  EQ git_vbak-vbeln
          AND vbtyp_n EQ  'J'.

* Fatura:
        SELECT *
         FROM vbfa APPENDING TABLE git_vbfa_fat
          FOR ALL ENTRIES IN git_vbak
        WHERE vbelv  EQ git_vbak-vbeln
         AND vbtyp_n EQ  'M'.

        IF git_vbfa_fat[] IS NOT INITIAL.
          SELECT *
          FROM vbrk APPENDING TABLE git_vbrk
            FOR ALL ENTRIES IN git_vbfa_fat
          WHERE vbeln  EQ git_vbfa_fat-vbeln.
        ENDIF.
      ENDIF.

* Busca de Dados – Ordem de Venda Aplicadas com Saldo
      SELECT *
         FROM zfiyt0032 AS b INTO CORRESPONDING FIELDS OF TABLE git_zfiyt0032_sld
        WHERE b~erdat IN p_erdat
          AND b~kunnr IN p_kunnr
          AND b~vbeln IN p_vbeln
          AND b~bstnk IN p_bstnk.
      "AND b~sdo_a_apl NE 0.

      IF git_zfiyt0032_sld[] IS NOT INITIAL.
        SELECT *
          FROM kna1 APPENDING TABLE git_kna1
            FOR ALL ENTRIES IN git_zfiyt0032_sld
          WHERE kunnr  EQ git_zfiyt0032_sld-kunnr.
      ENDIF.

      IF git_vbak[] IS NOT INITIAL.
        SELECT *
          FROM kna1 APPENDING TABLE git_kna1
            FOR ALL ENTRIES IN git_vbak
          WHERE kunnr  EQ git_vbak-kunnr.
      ENDIF.

      SORT git_kna1 BY kunnr.
      DELETE ADJACENT DUPLICATES FROM git_kna1 COMPARING kunnr.


    WHEN p_cambio. "Cierre de cambio – Realizado

      CLEAR: git_zfit0083[], git_zfiyt0033[].

      SELECT *
       FROM zfit0083  AS a INTO CORRESPONDING FIELDS OF TABLE git_zfit0083
      WHERE a~date_of_deal IN p_erdat
        AND a~source_ref   IN p_bloomb
        AND a~bukrs EQ '0100'.

      IF  git_zfit0083[] IS NOT INITIAL.
        SELECT *
          FROM zfiyt0033 APPENDING TABLE git_zfiyt0033
            FOR ALL ENTRIES IN git_zfit0083
          WHERE source_ref  EQ git_zfit0083-source_ref.

        LOOP AT git_zfiyt0033 INTO gwa_zfiyt0033.
          CLEAR: gva_idx .
          READ TABLE git_zfit0083 INTO gwa_zfit0083 WITH KEY source_ref = gwa_zfiyt0033-source_ref.
          IF sy-subrc = 0.
            gva_idx = sy-tabix.
            gwa_zfit0083-source_ref = '0'.
            MODIFY git_zfit0083 INDEX gva_idx FROM gwa_zfit0083 TRANSPORTING source_ref.
          ENDIF.
        ENDLOOP.
        DELETE git_zfit0083[] WHERE source_ref = '0'.
      ENDIF.

      DELETE git_zfiyt0033[] WHERE source_ref  NOT IN p_bloomb.
      DELETE git_zfiyt0033[] WHERE nro_liq_cb  NOT IN p_nrolc.
      DELETE git_zfiyt0033[] WHERE lote        NOT IN p_lote.


    WHEN p_infapl. "Informe de aplicaciones realizadas

      CLEAR: git_zfiyt0032[], git_zfiyt0033[], git_kna1[].

      SELECT *
       FROM zfiyt0032 AS b INTO CORRESPONDING FIELDS OF TABLE git_zfiyt0032
      WHERE b~erdat IN p_erdat
        AND b~kunnr IN p_kunnr
        AND b~vbeln IN p_vbeln
        AND b~bstnk IN p_bstnk
        AND b~nro_liq_cb IN  p_nrolc
        AND b~sdo_a_apl NE 0.

      DELETE git_zfiyt0032 WHERE valor_aplic IS INITIAL.

      IF git_zfiyt0032[] IS NOT INITIAL.

        SELECT *
          FROM kna1 APPENDING TABLE git_kna1
            FOR ALL ENTRIES IN git_zfiyt0032
          WHERE kunnr  EQ git_zfiyt0032-kunnr.

        SELECT *
        FROM zfiyt0033 APPENDING TABLE git_zfiyt0033
          FOR ALL ENTRIES IN git_zfiyt0032
        WHERE source_ref   EQ git_zfiyt0032-source_ref
          AND   lote       IN p_lote.

      ENDIF.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
FORM f_processa_dados .

  CASE abap_true.

    WHEN p_solper. "Solicitud de Permisos de Embarque X Cierre de cambio

      CLEAR: gwa_vbak,
             gwa_vbfa,
             gwa_zfiyt0032,
             gwa_vbap,
             gwa_vbkd,
             gwa_vbfa_fat,
             gwa_vbrk,
             gwa_zfiyt0032_sld,
             gwa_kna1,
             gwa_saida_0100,
             git_saida_0100[].


      LOOP AT git_zfiyt0032_sld INTO gwa_zfiyt0032_sld.

        gwa_saida_0100-kunnr        = gwa_zfiyt0032_sld-kunnr.

        READ TABLE git_kna1 INTO gwa_kna1 WITH KEY kunnr = gwa_zfiyt0032_sld-kunnr.
        gwa_saida_0100-name1        = gwa_kna1-name1.

        gwa_saida_0100-vgbel        = gwa_zfiyt0032_sld-vgbel.
        gwa_saida_0100-vbeln        = gwa_zfiyt0032_sld-vbeln.
        gwa_saida_0100-remessa      = gwa_zfiyt0032_sld-remessa .
        gwa_saida_0100-erdat        = gwa_zfiyt0032_sld-erdat.
        gwa_saida_0100-bstnk        = gwa_zfiyt0032_sld-bstnk.

        IF gwa_zfiyt0032_sld-seq IS INITIAL.
          gwa_saida_0100-seq          = 1.
        ELSE.
          gwa_saida_0100-seq          = gwa_zfiyt0032_sld-seq.
        ENDIF.

        gwa_saida_0100-xblnr        = gwa_zfiyt0032_sld-xblnr.
        gwa_saida_0100-bstkd_e      = gwa_zfiyt0032_sld-bstkd_e.
        gwa_saida_0100-matnr        = gwa_zfiyt0032_sld-matnr.
        gwa_saida_0100-arktx        = gwa_zfiyt0032_sld-arktx.
        gwa_saida_0100-ntgew        = gwa_zfiyt0032_sld-ntgew.
        gwa_saida_0100-netpr        = gwa_zfiyt0032_sld-netpr.
        gwa_saida_0100-netwr        = gwa_zfiyt0032_sld-netwr.
        gwa_saida_0100-waerk        = gwa_zfiyt0032_sld-waerk.
        gwa_saida_0100-erdat_p      = ( gwa_zfiyt0032_sld-erdat + 15 ).
        gwa_saida_0100-sdo_a_apl    = gwa_zfiyt0032_sld-sdo_a_apl.
        gwa_saida_0100-vlr_aduana   = gwa_zfiyt0032_sld-vlr_aduana.
        gwa_saida_0100-valor_aplic  = gwa_zfiyt0032_sld-valor_aplic.
        gwa_saida_0100-source_ref   = gwa_zfiyt0032_sld-source_ref.
        gwa_saida_0100-nro_liq_cb   = gwa_zfiyt0032_sld-nro_liq_cb.
        gwa_saida_0100-dt_aplic     = gwa_zfiyt0032_sld-dt_aplic.

        IF gwa_zfiyt0032_sld-valor_aplic <> 0.
          MOVE icon_complete TO gwa_saida_0100-status .
        ELSE.
          MOVE icon_generate TO gwa_saida_0100-status.
        ENDIF.

        APPEND gwa_saida_0100 TO git_saida_0100.
        CLEAR: gwa_zfiyt0032_sld ,gwa_kna1, gwa_saida_0100.

      ENDLOOP.

      LOOP AT git_vbak INTO gwa_vbak.

        gwa_saida_0100-kunnr = gwa_vbak-kunnr.
        gwa_saida_0100-auart = gwa_vbak-auart.

        READ TABLE git_kna1 INTO gwa_kna1 WITH KEY kunnr = gwa_vbak-kunnr.
        gwa_saida_0100-name1        = gwa_kna1-name1.

        gwa_saida_0100-vgbel        = gwa_vbak-vgbel.
        gwa_saida_0100-vbeln        = gwa_vbak-vbeln.
        gwa_saida_0100-erdat        = gwa_vbak-erdat.

        READ TABLE git_vbfa INTO gwa_vbfa WITH KEY vbelv = gwa_vbak-vbeln.

        gwa_saida_0100-remessa      = gwa_vbfa-vbeln.
        gwa_saida_0100-bstnk        = gwa_vbak-bstnk.

        gwa_saida_0100-seq          = 1.

        READ TABLE git_vbfa_fat INTO gwa_vbfa_fat WITH KEY vbelv = gwa_vbak-vbeln.

        gwa_saida_0100-fatura        = gwa_vbfa_fat-vbeln.

        READ TABLE git_vbrk INTO gwa_vbrk  WITH KEY vbeln = gwa_vbfa_fat-vbeln.

        gwa_saida_0100-xblnr        = gwa_vbrk-xblnr.

        READ TABLE git_vbkd INTO gwa_vbkd WITH KEY vbeln = gwa_vbak-vbeln.

        gwa_saida_0100-bstkd_e      = gwa_vbkd-bstkd_e.

        READ TABLE git_vbap INTO gwa_vbap WITH KEY vbeln = gwa_vbak-vbeln.

        gwa_saida_0100-matnr        = gwa_vbap-matnr.
        gwa_saida_0100-arktx        = gwa_vbap-arktx.
        gwa_saida_0100-ntgew        = gwa_vbap-ntgew.
        gwa_saida_0100-netpr        = gwa_vbap-netpr.
        gwa_saida_0100-netwr        = gwa_vbap-netwr.
        gwa_saida_0100-waerk        = gwa_vbap-waerk.
        gwa_saida_0100-erdat_p      = ( gwa_saida_0100-erdat + 15 ).
        gwa_saida_0100-sdo_a_apl    = gwa_vbap-netwr.
        gwa_saida_0100-vlr_aduana   = gwa_vbap-netwr.

        MOVE icon_generate TO gwa_saida_0100-status.

        APPEND gwa_saida_0100 TO git_saida_0100.
        CLEAR: gwa_vbak, gwa_kna1, gwa_vbfa,gwa_vbfa_fat,gwa_vbrk, gwa_vbkd, gwa_vbap, gwa_saida_0100.
      ENDLOOP.

    WHEN p_cambio. "Cierre de cambio – Realizado

      CLEAR: gwa_zfit0083, gwa_zfiyt0033, gwa_saida_0200, git_saida_0200[].

      LOOP AT git_zfit0083 INTO gwa_zfit0083.

        MOVE icon_generate TO gwa_saida_0200-status.

        gwa_saida_0200-source_ref             =  gwa_zfit0083-source_ref.
        gwa_saida_0200-currency_1             =  gwa_zfit0083-currency_1.
        gwa_saida_0200-date_of_deal           =  gwa_zfit0083-date_of_deal.
        gwa_saida_0200-time_of_deal           =  gwa_zfit0083-time_of_deal.
        gwa_saida_0200-amount_dealt           =  gwa_zfit0083-amount_dealt.
        gwa_saida_0200-counter_amount         =  gwa_zfit0083-counter_amount.
        gwa_saida_0200-spot_basis_rate        =  gwa_zfit0083-spot_basis_rate.
        gwa_saida_0200-trader_name            =  gwa_zfit0083-trader_name.
        gwa_saida_0200-counterparty_nam       =  gwa_zfit0083-counterparty_nam.
        gwa_saida_0200-cont_part_name         =  gwa_zfit0083-cont_part_name.
        gwa_saida_0200-sdo_a_apl              =  gwa_zfit0083-amount_dealt.

        APPEND gwa_saida_0200 TO git_saida_0200.
        CLEAR: gwa_saida_0200, gwa_zfit0083.

      ENDLOOP.


      LOOP AT git_zfiyt0033 INTO gwa_zfiyt0033.

        gwa_saida_0200-doc_lcto               =  gwa_zfiyt0033-doc_lcto.

        IF gwa_saida_0200-doc_lcto IS INITIAL.
          MOVE icon_generate TO gwa_saida_0200-status.
        ELSE.
          MOVE icon_complete TO  gwa_saida_0200-status.
        ENDIF.

        gwa_saida_0200-source_ref             =  gwa_zfiyt0033-source_ref.
        gwa_saida_0200-currency_1             =  gwa_zfiyt0033-currency_1.
        gwa_saida_0200-date_of_deal           =  gwa_zfiyt0033-date_of_deal.
        gwa_saida_0200-time_of_deal           =  gwa_zfiyt0033-time_of_deal.
        gwa_saida_0200-amount_dealt           =  gwa_zfiyt0033-amount_dealt.
        gwa_saida_0200-counter_amount         =  gwa_zfiyt0033-counter_amount.
        gwa_saida_0200-spot_basis_rate        =  gwa_zfiyt0033-spot_basis_rate.
        gwa_saida_0200-tp_lcto               =   gwa_zfiyt0033-tp_lcto.
        gwa_saida_0200-nro_liq_cb             =  gwa_zfiyt0033-nro_liq_cb.
        gwa_saida_0200-cod_fechto             =  gwa_zfiyt0033-cod_fechto.
        gwa_saida_0200-trader_name            =  gwa_zfiyt0033-trader_name.
        gwa_saida_0200-counterparty_nam       =  gwa_zfiyt0033-counterparty_nam.
        gwa_saida_0200-cont_part_name         =  gwa_zfiyt0033-cont_part_name.
        gwa_saida_0200-sdo_a_apl              =  gwa_zfiyt0033-sdo_a_apl.
        gwa_saida_0200-valor_aplic            =  gwa_zfiyt0033-valor_aplic.
        gwa_saida_0200-dt_aplic               =  gwa_zfiyt0033-dt_aplic.

        APPEND gwa_saida_0200 TO git_saida_0200.
        CLEAR: gwa_saida_0200, gwa_zfiyt0033.
      ENDLOOP.


    WHEN p_infapl. "Informe de aplicaciones realizadas

      CLEAR: gwa_zfiyt0032, gwa_zfiyt0033, gwa_saida_0300, git_saida_0300[].

      LOOP AT git_zfiyt0032 INTO gwa_zfiyt0032.

        gwa_saida_0300-kunnr =       gwa_zfiyt0032-kunnr.

        READ TABLE git_kna1 INTO gwa_kna1 WITH KEY kunnr = gwa_zfiyt0032-kunnr.
        gwa_saida_0300-name1        = gwa_kna1-name1.

        gwa_saida_0300-vgbel        =  gwa_zfiyt0032-vgbel.
        gwa_saida_0300-vbeln        =  gwa_zfiyt0032-vbeln.
        gwa_saida_0300-remessa      =  gwa_zfiyt0032-remessa.
        gwa_saida_0300-erdat        =  gwa_zfiyt0032-erdat.
        gwa_saida_0100-erdat_p      =  ( gwa_zfiyt0032_sld-erdat + 15 ). "Fecha PICKING
        gwa_saida_0300-bstnk        =  gwa_zfiyt0032-bstnk.
        gwa_saida_0300-seq          =  gwa_zfiyt0032-seq.
        gwa_saida_0300-xblnr        =  gwa_zfiyt0032-xblnr.
        gwa_saida_0300-bstkd_e      =  gwa_zfiyt0032-bstkd_e.
        gwa_saida_0300-matnr        =  gwa_zfiyt0032-matnr.
        gwa_saida_0300-arktx        =  gwa_zfiyt0032-arktx.
        gwa_saida_0300-ntgew        =  gwa_zfiyt0032-ntgew.
        gwa_saida_0300-netpr        =  gwa_zfiyt0032-netpr.
        gwa_saida_0300-netwr        =  gwa_zfiyt0032-netwr.
        gwa_saida_0300-waerk        =  gwa_zfiyt0032-waerk.
        gwa_saida_0300-sdo_a_apl    =  gwa_zfiyt0032-sdo_a_apl.
        gwa_saida_0300-vlr_aduana   =  gwa_zfiyt0032-vlr_aduana.
        gwa_saida_0300-valor_aplic  =  gwa_zfiyt0032-valor_aplic.
        gwa_saida_0300-source_ref   =  gwa_zfiyt0032-source_ref.
        gwa_saida_0300-nro_liq_cb   =  gwa_zfiyt0032-nro_liq_cb.


        READ TABLE git_zfiyt0033 INTO gwa_zfiyt0033 WITH KEY source_ref  = gwa_zfiyt0032-source_ref  .
        IF sy-subrc = 0.

          gwa_saida_0300-lote             =  gwa_zfiyt0033-lote.
          gwa_saida_0300-doc_lcto         =  gwa_zfiyt0033-doc_lcto.
          gwa_saida_0300-date_of_deal     =  gwa_zfiyt0033-date_of_deal.
          gwa_saida_0300-time_of_deal     =  gwa_zfiyt0033-time_of_deal.
          gwa_saida_0300-amount_dealt     =  gwa_zfiyt0033-amount_dealt.
          gwa_saida_0300-counter_amount   =  gwa_zfiyt0033-counter_amount.
          gwa_saida_0300-spot_basis_rate  =  gwa_zfiyt0033-spot_basis_rate.
          gwa_saida_0300-cod_fechto       =  gwa_zfiyt0033-cod_fechto.
          gwa_saida_0300-trader_name      =  gwa_zfiyt0033-trader_name.
          gwa_saida_0300-counterparty_nam =  gwa_zfiyt0033-counterparty_nam.
          gwa_saida_0300-cont_part_name   =  gwa_zfiyt0033-cont_part_name.

        ENDIF.

        APPEND gwa_saida_0300 TO git_saida_0300.
        CLEAR: gwa_zfiyt0032, gwa_zfiyt0033, gwa_saida_0300, gwa_kna1.

      ENDLOOP.


  ENDCASE.

  IF obj_alv_0100 IS NOT INITIAL.
    CALL METHOD obj_alv_0100->refresh_table_display.
  ENDIF.

  IF obj_alv_0200 IS NOT INITIAL.
    CALL METHOD obj_alv_0200->refresh_table_display.
  ENDIF.

  IF obj_alv_0300 IS NOT INITIAL.
    CALL METHOD obj_alv_0300->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CRIAR_CATALOG
*&---------------------------------------------------------------------*
FORM f_criar_catalog  USING p_screen.

  CASE p_screen.

    WHEN '0100'.


      CLEAR: git_fcat[].
      PERFORM f_estrutura_alv USING:
      01  ''           'STATUS'      'GIT_SAIDA_0100'   'STATUS'         'Status'               '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
      02  'VBAK'       'KUNNR'       'GIT_SAIDA_0100'   'KUNNR'          'Cliente'              '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
      03  'KNA1'       'NAME1'       'GIT_SAIDA_0100'   'NAME1'          'Nombre del cliente'   '45'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      04  'VBAK'       'VGBEL'       'GIT_SAIDA_0100'   'VGBEL'          'Contrato Nº.'         '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      05  'VBAK'       'VBELN'       'GIT_SAIDA_0100'   'VBELN'          'Órdenes de venta'     '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      06  'VBFA'       'REMESSA'     'GIT_SAIDA_0100'   'REMESSA'        'Nº Picking'           '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      07  'VBFA'       'ERDAT'       'GIT_SAIDA_0100'   'ERDAT'          'Fecha Picking'        '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      08  'VBAK'       'BSTNK'       'GIT_SAIDA_0100'   'BSTNK'          'Nº Pemisso'           '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      09  'ZFIY'       'SEQ'         'GIT_SAIDA_0100'   'SEQ'            'Seq Apl.'             '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      10  'VBRK'       'XBLNR'       'GIT_SAIDA_0100'   'XBLNR'          'Nº Invoice'           '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      11  'VBKD'       'BSTKD_E'     'GIT_SAIDA_0100'   'BSTKD_E'        'Pais'                 '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      12  'VBAP'       'MATNR'       'GIT_SAIDA_0100'   'MATNR'          'Material'             '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      13  'VBAP'       'ARKTX'       'GIT_SAIDA_0100'   'ARKTX'          'Nombre del Material'  '45'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      14  'VBAP'       'NTGEW'       'GIT_SAIDA_0100'   'NTGEW'          'Cantidad'             '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      15  'VBAP'       'NETPR'       'GIT_SAIDA_0100'   'NETPR'          'Precio unitário'      '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      16  'VBAP'       'NETWR'       'GIT_SAIDA_0100'   'NETWR'          'Valor'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      17  'VBAP'       'WAERK'       'GIT_SAIDA_0100'   'WAERK'          'Moneda'               '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      18  'VBFA'       'ERDAT'       'GIT_SAIDA_0100'   'ERDAT_P'        'Fecha Vencimento'     '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      19  'ZFIYT0032'  'SDO_A_APL'   'GIT_SAIDA_0100'   'SDO_A_APL'      'Aplicar Saldo'        '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      20  'ZFIYT0032'  'VLR_ADUANA'  'GIT_SAIDA_0100'   'VLR_ADUANA'     'Vlr.Aduana'           '13'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
      21  'ZFIYT0032'  'VALOR_APLIC' 'GIT_SAIDA_0100'   'VALOR_APLIC'    'Valor Aplicado'       '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      22  'ZFIYT0032'  'SOURCE_REF'  'GIT_SAIDA_0100'   'SOURCE_REF'     'Nº Bloomberg'         '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      23  'ZFIYT0032'  'NRO_LIQ_CB'  'GIT_SAIDA_0100'   'NRO_LIQ_CB'     'Nº Liq.Câmbio'        '13'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0101_CAMBIO'.

      CLEAR: git_fcat[].
      PERFORM f_estrutura_alv USING:

        01 'ZFIYT0033'         'CONT_PART_NAME'   'GIT_SAIDA_0101_CAMBIO'   'CONT_PART_NAME'    'Banco'           '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
        02 'ZFIYT0033'         'SOURCE_REF'       'GIT_SAIDA_0101_CAMBIO'   'SOURCE_REF'        'Nº Bloomberg'    '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        03 'ZFIYT0033'         'NRO_LIQ_CB'       'GIT_SAIDA_0101_CAMBIO'   'NRO_LIQ_CB'        'Nº Liq.Câmbio'   '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        04 'ZFIYT0033'         'DATE_OF_DEAL'     'GIT_SAIDA_0101_CAMBIO'   'DATE_OF_DEAL'      'Fecha de Cierre' '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        05 'ZFIYT0033'         'AMOUNT_DEALT'     'GIT_SAIDA_0101_CAMBIO'   'AMOUNT_DEALT'      'Valor Cierre'    '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        06 'ZFIYT0033'         'VALOR_APLIC'      'GIT_SAIDA_0101_CAMBIO'   'VALOR_APLIC'       'Valor Aplicado'  '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        07 'ZFIYT0033'         'SDO_A_APL'        'GIT_SAIDA_0101_CAMBIO'   'SDO_A_APL'         'Saldo À Aplicar' '20'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0101_PERMISSO'.

      CLEAR: git_fcat[].
      PERFORM f_estrutura_alv USING:

        01  'ZFIYT0032'      'STATUS'            'GIT_SAIDA_0101_PERMISSO'   'STATUS'           'Status'             '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
        02  'ZFIYT0032'      'BSTNK'             'GIT_SAIDA_0101_PERMISSO'   'BSTNK'            'Nº Pemisso'         '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        03  'ZFIYT0032'      'SEQ'               'GIT_SAIDA_0101_PERMISSO'   'SEQ'              'Seq Apl.'           '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        04  'ZFIYT0032'      'VBELN'             'GIT_SAIDA_0101_PERMISSO'   'VBELN'            'Órdenes de venta'   '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        05  'ZFIYT0032'      'ERDAT'             'GIT_SAIDA_0101_PERMISSO'   'ERDAT'            'Fecha Picking'      '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        06  'ZFIYT0032'      'SDO_A_APL'         'GIT_SAIDA_0101_PERMISSO'   'SDO_A_APL'        'Aplicar Saldo'      '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        07  'ZFIYT0032'      'VALOR_APLIC'       'GIT_SAIDA_0101_PERMISSO'   'VALOR_APLIC'      'Valor Aplicado'     '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        08  'ZFIYT0032'      'SOURCE_REF'        'GIT_SAIDA_0101_PERMISSO'   'SOURCE_REF'       'Nº Bloomberg'       '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        09  'ZFIYT0032'      'NRO_LIQ_CB'        'GIT_SAIDA_0101_PERMISSO'   'NRO_LIQ_CB'       'Nº Liq.Câmbio'      '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        10  'ZFIYT0032'      'KUNNR'             'GIT_SAIDA_0101_PERMISSO'   'KUNNR'            'Cliente'            '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        11  'ZFIYT0032'      'NAME1'             'GIT_SAIDA_0101_PERMISSO'   'NAME1'            'Nombre del cliente' '45'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0200'.

      CLEAR: git_fcat[].
      PERFORM f_estrutura_alv USING:

      01 'ZFIYT0033'  'STATUS'              'GIT_SAIDA_0200'   'STATUS'           'Status'              '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
      02 'ZGLT032'    'TP_LCTO'             'GIT_SAIDA_0200'   'TP_LCTO'          'Modelo ZGL'          '10'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
      02 'ZFIYT0033'  'DOC_LCTO'            'GIT_SAIDA_0200'   'DOC_LCTO'         'Doc.Lcto ZGL'        '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      03 'ZFIYT0033'  'SOURCE_REF'          'GIT_SAIDA_0200'   'SOURCE_REF'       'Nº Bloomberg'        '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      04 'ZFIYT0033'  'CURRENCY_1'          'GIT_SAIDA_0200'   'CURRENCY_1'       'Moeda'               '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      05 'ZFIYT0033'  'DATE_OF_DEAL'        'GIT_SAIDA_0200'   'DATE_OF_DEAL'     'Dt.Operação'         '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      06 'ZFIYT0033'  'TIME_OF_DEAL'        'GIT_SAIDA_0200'   'TIME_OF_DEAL'     'Hora Operação'       '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      07 'ZFIYT0033'  'AMOUNT_DEALT'        'GIT_SAIDA_0200'   'AMOUNT_DEALT'     'Valor USD'           '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      08 'ZFIYT0033'  'COUNTER_AMOUNT'      'GIT_SAIDA_0200'   'COUNTER_AMOUNT'   'Valor ARS'           '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      09 'ZFIYT0033'  'SPOT_BASIS_RATE'     'GIT_SAIDA_0200'   'SPOT_BASIS_RATE'  'Tx.Câmbio'           '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      10 'ZFIYT0033'  'NRO_LIQ_CB'          'GIT_SAIDA_0200'   'NRO_LIQ_CB'       'Nº Liq.Câmbio'       '13'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
      11 'ZFIYT0033'  'COD_FECHTO'          'GIT_SAIDA_0200'   'COD_FECHTO'       'Cod.Fechto'          '10'   'X'    ''  ' ' ' ' ' ' 'X' '' ,
      12 'ZFIYT0033'  'TRADER_NAME'         'GIT_SAIDA_0200'   'TRADER_NAME'      'Trader Name'         '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      13 'ZFIYT0033'  'COUNTERPARTY_NAM'    'GIT_SAIDA_0200'   'COUNTERPARTY_NAM' 'Counterparty name'   '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
      14 'ZFIYT0033'  'CONT_PART_NAME'      'GIT_SAIDA_0200'   'CONT_PART_NAME'   'Counterparty Bank'   '20'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0300'.

      CLEAR: git_fcat[].
      PERFORM f_estrutura_alv USING:

        01 'ZFIYT0032'  'KUNNR'            'GIT_SAIDA_0300'      'KUNNR'             'Cliente'               '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        02 'KNA1'       'NAME1'            'GIT_SAIDA_0300'      'NAME1'             'Nombre del cliente'	   '45'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        03 'ZFIYT0032'  'VGBEL'            'GIT_SAIDA_0300'      'VGBEL'             'Contrato Nº.'          '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        04 'ZFIYT0033'  'VBELN'            'GIT_SAIDA_0300'      'VBELN'             'Órdenes de venta'	     '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        05 'ZFIYT0032'  'REMESSA'          'GIT_SAIDA_0300'      'REMESSA'           'Nº Picking'            '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        06 'ZFIYT0032'  'ERDAT'            'GIT_SAIDA_0300'      'ERDAT'             'Fecha Picking'         '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        07 'ZFIYT0032'  'BSTNK'            'GIT_SAIDA_0300'      'BSTNK'             'Nº Pemisso'	           '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        08 'ZFIYT0032'  'SEQ'              'GIT_SAIDA_0300'      'SEQ'               'Seq Apl.'              '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        09 'ZFIYT0032'  'XBLNR'            'GIT_SAIDA_0300'      'XBLNR'             'Nº Invoice'            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        10 'ZFIYT0032'  'BSTKD_E'          'GIT_SAIDA_0300'      'BSTKD_E'           'Pais'                  '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        11 'ZFIYT0032'  'MATNR'            'GIT_SAIDA_0300'      'MATNR'             'Material'              '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        12 'ZFIYT0032'  'ARKTX'            'GIT_SAIDA_0300'      'ARKTX'             'Nombre del Material'   '45'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        13 'ZFIYT0032'  'NTGEW'            'GIT_SAIDA_0300'      'NTGEW'             'Cantidad'              '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        14 'ZFIYT0032'  'NETPR'            'GIT_SAIDA_0300'      'NETPR'             'Precio unitário'       '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        15 'ZFIYT0032'  'NETWR'            'GIT_SAIDA_0300'      'NETWR'             'Valor'                 '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        16 'ZFIYT0032'  'WAERK'            'GIT_SAIDA_0300'      'WAERK'             'Moneda'                '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        17 'VBFA'       'ERDAT'            'GIT_SAIDA_0300'      'ERDAT_P'           'Fecha Vencimento'      '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        18 'ZFIYT0032'  'SDO_A_APL'        'GIT_SAIDA_0300'      'SDO_A_APL'         'Aplicar Saldo'         '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        19 'ZFIYT0032'  'VLR_ADUANA'       'GIT_SAIDA_0300'      'VLR_ADUANA'        'Vlr.ADUANA'            '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        20 'ZFIYT0032'  'VALOR_APLIC'      'GIT_SAIDA_0300'      'VALOR_APLIC'       'Valor Aplicado'        '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        21 'ZFIYT0033'  'LOTE'             'GIT_SAIDA_0300'      'LOTE'              'Lote ZGL'              '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        22 'ZFIYT0033'  'DOC_LCTO'         'GIT_SAIDA_0300'      'DOC_LCTO'          'Doc.Lcto ZGL'          '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        23 'ZFIYT0032'  'SOURCE_REF'       'GIT_SAIDA_0300'      'SOURCE_REF'        'Nº Bloomberg'          '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        24 'ZFIYT0032'  'NRO_LIQ_CB'       'GIT_SAIDA_0300'      'NRO_LIQ_CB'        'Nº Liq.Câmbio'         '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        25 'ZFIYT0033'  'DATE_OF_DEAL'     'GIT_SAIDA_0300'      'DATE_OF_DEAL'      'Dt.Operação'           '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        26 'ZFIYT0033'  'TIME_OF_DEAL'     'GIT_SAIDA_0300'      'TIME_OF_DEAL'      'Hora Operação'         '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        27 'ZFIYT0033'  'AMOUNT_DEALT'     'GIT_SAIDA_0300'      'AMOUNT_DEALT'      'Valor USD'             '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        28 'ZFIYT0033'  'COUNTER_AMOUNT'   'GIT_SAIDA_0300'      'COUNTER_AMOUNT'    'Valor ARS'             '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        29 'ZFIYT0033'  'SPOT_BASIS_RATE'  'GIT_SAIDA_0300'      'SPOT_BASIS_RATE'   'Tx.Câmbio'             '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        30 'ZFIYT0033'  'COD_FECHTO'       'GIT_SAIDA_0300'      'COD_FECHTO'        'Cod.Fechto'            '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        31 'ZFIYT0033'  'TRADER_NAME'      'GIT_SAIDA_0300'      'TRADER_NAME'       'Trader Name'           '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        32 'ZFIYT0033'  'COUNTERPARTY_NAM' 'GIT_SAIDA_0300'      'COUNTERPARTY_NAM'  'Counterparty Name'     '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
        33 'ZFIYT0033'  'CONT_PART_NAME'   'GIT_SAIDA_0300'      'CONT_PART_NAME'    'Counterparty Bank'     '20'   ' '    ''  ' ' ' ' ' ' ' ' '' .

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTRUTURA_ALV
*&---------------------------------------------------------------------*
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                           VALUE(p_tabname)       LIKE dd02d-tabname
                           VALUE(p_field)         LIKE dd03d-fieldname
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                           VALUE(p_outputlen)
                           VALUE(p_edit)
                           VALUE(p_sum)
                           VALUE(p_emphasize)
                           VALUE(p_just)
                           VALUE(p_hotspot)
                           VALUE(p_f4)
                           VALUE(p_check).

  CLEAR gwa_fcat.

  gwa_fcat-fieldname   = p_field.
  gwa_fcat-tabname     = p_tabname.
  gwa_fcat-ref_table   = p_ref_tabname.
  gwa_fcat-ref_field   = p_ref_fieldname.
  gwa_fcat-key         = ' '.
  gwa_fcat-edit        = p_edit.
  gwa_fcat-col_pos     = p_col_pos.
  gwa_fcat-outputlen   = p_outputlen.
  gwa_fcat-no_out      = ' '.
  gwa_fcat-do_sum      = p_sum.
  gwa_fcat-reptext     = p_scrtext_l.
  gwa_fcat-scrtext_s   = p_scrtext_l.
  gwa_fcat-scrtext_m   = p_scrtext_l.
  gwa_fcat-scrtext_l   = p_scrtext_l.
  gwa_fcat-emphasize   = p_emphasize.
  gwa_fcat-style       =
  gwa_fcat-just        = p_just.
  gwa_fcat-hotspot     = p_hotspot.
  gwa_fcat-f4availabl  = p_f4.
  gwa_fcat-checkbox    = p_check.

  APPEND gwa_fcat TO git_fcat.

ENDFORM.                    " ESTRUTURA_ALV
*&---------------------------------------------------------------------*
*&      Form  F_EXCLUDE_FCODE
*&---------------------------------------------------------------------*
FORM f_exclude_fcode USING p_screen.

  CASE p_screen .
    WHEN '0100' .
      CLEAR: git_exclude_fcode[].
      APPEND cl_gui_alv_grid=>mc_fc_refresh           TO git_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO git_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO git_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO git_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO git_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO git_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO git_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO git_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO git_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO git_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_check             TO git_exclude_fcode.
    WHEN '0101_CAMBIO'.
      CLEAR: git_exclude_fcode_0101_cambio[].
      APPEND cl_gui_alv_grid=>mc_fc_refresh           TO git_exclude_fcode_0101_cambio.
      APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO git_exclude_fcode_0101_cambio.
      APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO git_exclude_fcode_0101_cambio.
      APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO git_exclude_fcode_0101_cambio.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO git_exclude_fcode_0101_cambio.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO git_exclude_fcode_0101_cambio.
      APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO git_exclude_fcode_0101_cambio.
      APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO git_exclude_fcode_0101_cambio.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO git_exclude_fcode_0101_cambio.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO git_exclude_fcode_0101_cambio.
      APPEND cl_gui_alv_grid=>mc_fc_check             TO git_exclude_fcode_0101_cambio.
    WHEN '0101_PERMISSO'.
      CLEAR: git_exclude_fcode_0101_perm[].
      APPEND cl_gui_alv_grid=>mc_fc_refresh           TO git_exclude_fcode_0101_perm.
      APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO git_exclude_fcode_0101_perm.
      APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO git_exclude_fcode_0101_perm.
      APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO git_exclude_fcode_0101_perm.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO git_exclude_fcode_0101_perm.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO git_exclude_fcode_0101_perm.
      APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO git_exclude_fcode_0101_perm.
      APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO git_exclude_fcode_0101_perm.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO git_exclude_fcode_0101_perm.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO git_exclude_fcode_0101_perm.
      APPEND cl_gui_alv_grid=>mc_fc_check             TO git_exclude_fcode_0101_perm.

    WHEN '0200' .
      CLEAR: git_exclude_fcode_0200[].
      APPEND cl_gui_alv_grid=>mc_fc_refresh           TO git_exclude_fcode_0200.
      APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO git_exclude_fcode_0200.
      APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO git_exclude_fcode_0200.
      APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO git_exclude_fcode_0200.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO git_exclude_fcode_0200.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO git_exclude_fcode_0200.
      APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO git_exclude_fcode_0200.
      APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO git_exclude_fcode_0200.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO git_exclude_fcode_0200.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO git_exclude_fcode_0200.
      APPEND cl_gui_alv_grid=>mc_fc_check             TO git_exclude_fcode_0200.

    WHEN '0300' .
      CLEAR: git_exclude_fcode_0300[].
      APPEND cl_gui_alv_grid=>mc_fc_refresh           TO git_exclude_fcode_0300.
      APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO git_exclude_fcode_0300.
      APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO git_exclude_fcode_0300.
      APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO git_exclude_fcode_0300.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO git_exclude_fcode_0300.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO git_exclude_fcode_0300.
      APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO git_exclude_fcode_0300.
      APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO git_exclude_fcode_0300.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO git_exclude_fcode_0300.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO git_exclude_fcode_0300.
      APPEND cl_gui_alv_grid=>mc_fc_check             TO git_exclude_fcode_0300.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SOLIC_INTERCAMBIO
*&---------------------------------------------------------------------*
FORM f_solic_intercambio .

* Permissos em Aberto
  CLEAR: git_sel_rows[],
         gwa_sel_rows,
         git_zfiyt0032[],
         gwa_zfiyt0032,
         git_saida_0101_permisso[],
         gwa_saida_0101_permisso,
         gwa_saida_0100.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = git_sel_rows.

  CHECK git_sel_rows[] IS NOT INITIAL.

* Busca de Dados da Bloomberg (Câmbio Fechado e aplicados)
  PERFORM f_busca_dados_bloomberg.

  LOOP AT git_sel_rows INTO gwa_sel_rows.

    READ TABLE git_saida_0100 INTO gwa_saida_0100 INDEX gwa_sel_rows-index.

    CHECK sy-subrc = 0.

    IF gwa_saida_0100-valor_aplic IS INITIAL.

      MOVE-CORRESPONDING gwa_saida_0100 TO gwa_zfiyt0032.

      MOVE icon_generate TO gwa_saida_0101_permisso-status.

      gwa_saida_0101_permisso-bstnk         = gwa_saida_0100-bstnk.
      gwa_saida_0101_permisso-seq           = gwa_saida_0100-seq.
      gwa_saida_0101_permisso-vbeln         = gwa_saida_0100-vbeln.
      gwa_saida_0101_permisso-erdat         = gwa_saida_0100-erdat.
      gwa_saida_0101_permisso-sdo_a_apl     = gwa_saida_0100-sdo_a_apl.
      gwa_saida_0101_permisso-valor_aplic   = gwa_saida_0100-valor_aplic.
      gwa_saida_0101_permisso-source_ref    = gwa_saida_0100-source_ref.
      gwa_saida_0101_permisso-nro_liq_cb    = gwa_saida_0100-nro_liq_cb.
      gwa_saida_0101_permisso-kunnr         = gwa_saida_0100-kunnr.
      gwa_saida_0101_permisso-name1         = gwa_saida_0100-name1.
      gwa_saida_0101_permisso-dt_aplic      = gwa_saida_0100-dt_aplic.

      APPEND gwa_saida_0101_permisso TO git_saida_0101_permisso.
      APPEND gwa_zfiyt0032 TO git_zfiyt0032.

      CLEAR: gwa_saida_0101_permisso, gwa_zfiyt0032 , gwa_saida_0100.

    ENDIF.
  ENDLOOP.
  gva_type = 'APLIC'.
  CALL SCREEN 0101.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ANULA_APLICACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_anula_aplicacao .

  DATA: lva_total TYPE zfiyt0032-sdo_a_apl,
        lva_exit  TYPE c.

* Permissos fechados
  CLEAR: git_sel_rows[],
         gwa_sel_rows,
         git_zfiyt0032[],
         git_zfiyt0033[],
         gwa_zfiyt0033,
         git_zfiyt0032_aux[],
         git_saida_0101_permisso[],
         gwa_saida_0101_permisso,
         git_saida_0101_cambio[],
         gwa_saida_0101_cambio,
         gwa_saida_0100,
         gwa_zfiyt0032_aux.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = git_sel_rows.

  CHECK git_sel_rows[] IS NOT INITIAL.

  IF lines( git_sel_rows[] ) > 1.
    MESSAGE 'Selecione somente uma linha para cancelamento'(010) TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE git_sel_rows INTO gwa_sel_rows INDEX 1.

  READ TABLE git_saida_0100 INTO gwa_saida_0100 INDEX gwa_sel_rows-index.

  IF gwa_saida_0100-valor_aplic IS INITIAL AND gwa_saida_0100-seq = '0001'.

    "Verifico se existe mais sequencias sem aplicação.
    SELECT *
     FROM zfiyt0032 APPENDING TABLE git_zfiyt0032_aux
     WHERE  vbeln   EQ gwa_saida_0100-vbeln
      AND seq <> gwa_saida_0100-seq .

    TYPES: tt_zfiyt0032 TYPE TABLE OF zfiyt0032 WITH EMPTY KEY.
    DATA(lva_count) = lines( VALUE tt_zfiyt0032( FOR line IN git_zfiyt0032_aux WHERE ( valor_aplic IS NOT INITIAL ) ( line ) ) ).

    IF lva_count IS INITIAL.

      LOOP AT git_zfiyt0032_aux INTO gwa_zfiyt0032_aux.
        lva_total = lva_total + gwa_zfiyt0032_aux-sdo_a_apl.
        CLEAR: gwa_zfiyt0032_aux.
      ENDLOOP.

      lva_total = lva_total + gwa_saida_0100-sdo_a_apl.

      IF  lva_total = gwa_saida_0100-netwr.

        CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
          EXPORTING
            i_title              = 'Confirma Exclusão?'(027)
            i_zebra              = 'X'
            i_tabname            = 1
            i_structure_name     = 'zfiyt0032'
            i_allow_no_selection = 'X'
          IMPORTING
            e_exit               = lva_exit
          TABLES
            t_outtab             = git_zfiyt0032_aux
          EXCEPTIONS
            program_error        = 1
            OTHERS               = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF lva_exit IS INITIAL.

          LOOP AT git_zfiyt0032_aux INTO gwa_zfiyt0032_aux.

            DELETE FROM zfiyt0032 WHERE vbeln      =  gwa_zfiyt0032_aux-vbeln AND
                                        seq        =  gwa_zfiyt0032_aux-seq  AND
                                        erdat      = gwa_zfiyt0032_aux-erdat.

            CLEAR gwa_zfiyt0032_aux.
          ENDLOOP.

          DELETE FROM zfiyt0032 WHERE vbeln      =  gwa_saida_0100-vbeln AND
                            seq        =  gwa_saida_0100-seq  AND
                            erdat      =  gwa_saida_0100-erdat.



*          UPDATE zfiyt0032 SET sdo_a_apl    = lva_total
*                     valor_aplic  = 0
*                     dt_aplic     = ''
*                     hr_aplic     = ''
*                     usnam_apl    = ''
*         WHERE seq        EQ  gwa_saida_0100-seq
*           AND vbeln      EQ  gwa_saida_0100-vbeln.

          COMMIT WORK.

          PERFORM: f_refresh_alv USING 0100,
                   f_executar.

          MESSAGE 'Exclusão realizada com sucesso!'(025) TYPE 'S'.
          CALL SCREEN 0100.
          EXIT.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE 'Exclusão não permitida!'(026) TYPE 'S'.
    ENDIF.

  ELSE.

    IF gwa_saida_0100-valor_aplic IS NOT INITIAL.

      MOVE icon_complete TO gwa_saida_0101_permisso-status.

      gwa_saida_0101_permisso-bstnk         = gwa_saida_0100-bstnk.
      gwa_saida_0101_permisso-seq           = gwa_saida_0100-seq.
      gwa_saida_0101_permisso-vbeln         = gwa_saida_0100-vbeln.
      gwa_saida_0101_permisso-erdat         = gwa_saida_0100-erdat.
      gwa_saida_0101_permisso-sdo_a_apl     = gwa_saida_0100-sdo_a_apl.
      gwa_saida_0101_permisso-valor_aplic   = gwa_saida_0100-valor_aplic.
      gwa_saida_0101_permisso-source_ref    = gwa_saida_0100-source_ref.
      gwa_saida_0101_permisso-nro_liq_cb    = gwa_saida_0100-nro_liq_cb.
      gwa_saida_0101_permisso-kunnr         = gwa_saida_0100-kunnr.
      gwa_saida_0101_permisso-name1         = gwa_saida_0100-name1.
      gwa_saida_0101_permisso-dt_aplic      = gwa_saida_0100-dt_aplic.
      gwa_saida_0101_permisso-tp_aplic      = 'S'.

      APPEND gwa_saida_0101_permisso TO git_saida_0101_permisso.
      CLEAR: gwa_saida_0101_permisso.

    ENDIF.

    SELECT *
     FROM zfiyt0033 APPENDING TABLE git_zfiyt0033
       FOR ALL ENTRIES IN  git_saida_0101_permisso
     WHERE  source_ref   EQ git_saida_0101_permisso-source_ref.


    IF git_zfiyt0033[] IS NOT INITIAL.

      LOOP AT git_zfiyt0033 INTO gwa_zfiyt0033 .

        IF gwa_zfiyt0033-valor_aplic IS NOT INITIAL.

          gwa_saida_0101_cambio-cont_part_name =  gwa_zfiyt0033-cont_part_name.
          gwa_saida_0101_cambio-source_ref     =  gwa_zfiyt0033-source_ref.
          gwa_saida_0101_cambio-nro_liq_cb     =  gwa_zfiyt0033-nro_liq_cb.
          gwa_saida_0101_cambio-date_of_deal   =  gwa_zfiyt0033-date_of_deal.
          gwa_saida_0101_cambio-amount_dealt   =  gwa_zfiyt0033-amount_dealt.
          gwa_saida_0101_cambio-sdo_a_apl      =  gwa_zfiyt0033-sdo_a_apl.
          gwa_saida_0101_cambio-valor_aplic    =  gwa_zfiyt0033-valor_aplic.
          gwa_saida_0101_cambio-dt_aplic       =  gwa_zfiyt0033-dt_aplic.
          gwa_saida_0101_cambio-tp_aplic       = 'S'.

          APPEND gwa_saida_0101_cambio TO git_saida_0101_cambio.
        ENDIF.

        CLEAR:  gwa_zfiyt0033 , gwa_saida_0101_cambio.

      ENDLOOP.
    ENDIF.

    CLEAR: git_zfiyt0033[],
           git_zfiyt0032[].

    MOVE-CORRESPONDING git_saida_0101_cambio[]   TO git_zfiyt0033[].
    MOVE-CORRESPONDING git_saida_0101_permisso[] TO git_zfiyt0032[].

    gva_type = 'ANULA'.
    CALL SCREEN 0101.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS_BLOOMBERG
*&---------------------------------------------------------------------*
FORM f_busca_dados_bloomberg .

  CLEAR: git_zfit0083[],
         gwa_zfit0083,
         git_zfiyt0033[],
         gwa_zfiyt0033,
         git_saida_0101_cambio[],
         gwa_saida_0101_cambio.


  SELECT *
   FROM zfiyt0033 APPENDING TABLE git_zfiyt0033
  WHERE date_of_deal IN p_erdat
    AND cod_fechto  <> ''.

  IF git_zfiyt0033[] IS NOT INITIAL.

    LOOP AT git_zfiyt0033 INTO gwa_zfiyt0033 .

      IF gwa_zfiyt0033-sdo_a_apl <> 0.

        gwa_saida_0101_cambio-cont_part_name =  gwa_zfiyt0033-cont_part_name.
        gwa_saida_0101_cambio-source_ref     =  gwa_zfiyt0033-source_ref.
        gwa_saida_0101_cambio-nro_liq_cb     =  gwa_zfiyt0033-nro_liq_cb.
        gwa_saida_0101_cambio-date_of_deal   =  gwa_zfiyt0033-date_of_deal.
        gwa_saida_0101_cambio-amount_dealt   =  gwa_zfiyt0033-amount_dealt.
        gwa_saida_0101_cambio-sdo_a_apl      =  gwa_zfiyt0033-sdo_a_apl.
        gwa_saida_0101_cambio-valor_aplic    =  gwa_zfiyt0033-valor_aplic.
        gwa_saida_0101_cambio-dt_aplic       =  gwa_zfiyt0033-dt_aplic.

        APPEND gwa_saida_0101_cambio TO git_saida_0101_cambio.

      ENDIF.

      CLEAR: gwa_zfiyt0033, gwa_saida_0101_cambio.

    ENDLOOP.
  ENDIF.



*  SELECT *
*   FROM zfit0083  AS b INTO CORRESPONDING FIELDS OF TABLE git_zfit0083
*  WHERE b~date_of_deal EQ '20140912'."IN P_ERDAT.
*
*  SELECT *
*    FROM zfiyt0033 APPENDING TABLE git_zfiyt0033
*      FOR ALL ENTRIES IN git_zfit0083
*    WHERE  source_ref   EQ git_zfit0083-source_ref
*       AND date_of_deal EQ '20140912'."IN P_ERDAT.
*
*  IF git_zfit0083[] IS NOT INITIAL.
*
*    LOOP AT git_zfit0083 INTO gwa_zfit0083.
*
*      READ TABLE git_zfiyt0033 INTO gwa_zfiyt0033 WITH KEY source_ref = gwa_zfit0083-source_ref.
*
*      IF sy-subrc = 0.
*
*        IF gwa_zfiyt0033-sdo_a_apl <> 0.
*
*          gwa_saida_0101_cambio-cont_part_name =  gwa_zfiyt0033-cont_part_name.
*          gwa_saida_0101_cambio-source_ref     =  gwa_zfiyt0033-source_ref.
*          gwa_saida_0101_cambio-nro_liq_cb     =  gwa_zfiyt0033-nro_liq_cb.
*          gwa_saida_0101_cambio-date_of_deal   =  gwa_zfiyt0033-date_of_deal.
*          gwa_saida_0101_cambio-amount_dealt   =  gwa_zfiyt0033-amount_dealt.
*          gwa_saida_0101_cambio-sdo_a_apl      =  gwa_zfiyt0033-sdo_a_apl.
*          gwa_saida_0101_cambio-valor_aplic    =  gwa_zfiyt0033-valor_aplic.
*          gwa_saida_0101_cambio-dt_aplic       =  gwa_zfiyt0033-dt_aplic.
*
*          APPEND gwa_saida_0101_cambio TO git_saida_0101_cambio.
*        ENDIF.
*      ELSE.
*
*        MOVE-CORRESPONDING gwa_zfit0083 TO gwa_zfiyt0033.
*
*        gwa_saida_0101_cambio-cont_part_name  =  gwa_zfit0083-cont_part_name.
*        gwa_saida_0101_cambio-source_ref      =  gwa_zfit0083-source_ref.
*        gwa_saida_0101_cambio-date_of_deal    =  gwa_zfit0083-date_of_deal.
*        gwa_saida_0101_cambio-nro_liq_cb      =  ''.
*        gwa_saida_0101_cambio-amount_dealt    =  gwa_zfit0083-amount_dealt.
*        gwa_saida_0101_cambio-sdo_a_apl       =  gwa_zfit0083-amount_dealt.
*
*        APPEND gwa_saida_0101_cambio TO git_saida_0101_cambio.
*        APPEND gwa_zfiyt0033         TO git_zfiyt0033.
*
*      ENDIF.
*      CLEAR: gwa_zfit0083, gwa_zfiyt0033, gwa_saida_0101_cambio.
*
*    ENDLOOP.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERA_CONTABIL
*&---------------------------------------------------------------------*
FORM f_gera_contabil .

  DATA: lva_dp_resp   TYPE char2,
        lva_descricao TYPE zdescr_lote,
        lva_lote      TYPE zglt035-lote,
        lva_werks     TYPE zglt036-gsber.

  CLEAR: git_sel_rows[], gwa_sel_rows,
         git_zglt031[],
         gwa_saida_0200,
         git_zglt032[],
         gwa_zglt032,
         gwa_zglt035,
         gwa_zglt036.


  CALL METHOD obj_alv_0200->if_cached_prop~set_prop
    EXPORTING
      propname  = 'GridModified'
      propvalue = '1'.

  CALL METHOD obj_alv_0200->check_changed_data.

  IF gva_error_in_data IS INITIAL.

    CALL METHOD obj_alv_0200->get_selected_rows
      IMPORTING
        et_index_rows = git_sel_rows.

    CHECK git_sel_rows[] IS NOT INITIAL.

    LOOP AT git_sel_rows INTO gwa_sel_rows.

      CLEAR: gwa_saida_0200.
      READ TABLE git_saida_0200 INTO gwa_saida_0200 INDEX gwa_sel_rows-index.

      IF gwa_saida_0200-tp_lcto IS NOT INITIAL.

        SELECT *
          FROM zglt031 INTO TABLE git_zglt031
         WHERE tp_lcto EQ gwa_saida_0200-tp_lcto.

        SELECT *
          FROM zglt032 INTO TABLE git_zglt032
         WHERE tp_lcto EQ  gwa_saida_0200-tp_lcto.

        IF git_zglt032[] IS NOT INITIAL.

          CLEAR:lva_lote,
                lva_descricao,
                lva_dp_resp.

          CLEAR: gwa_zglt031, lva_descricao, lva_dp_resp, lva_lote, gwa_zglt035.
          READ TABLE git_zglt031 INTO gwa_zglt031  INDEX 1.

          MOVE gwa_zglt031-descricao TO lva_descricao.
          MOVE gwa_zglt031-dpto_resp TO lva_dp_resp.

          " Gera número do lote
          CALL METHOD zcl_gerar_lote=>create_lote
            EXPORTING
              i_bukrs      = '0100'
              i_descr_lote = lva_descricao
              i_dep_resp   = lva_dp_resp
              i_user_resp  = sy-uname
            IMPORTING
              e_num_lote   = lva_lote.

          MOVE:    '0100'                           TO gwa_zglt035-bukrs,
                   gwa_saida_0200-tp_lcto           TO gwa_zglt035-tp_lcto,
                   lva_lote                         TO gwa_zglt035-lote,
                   gwa_saida_0200-currency_1        TO gwa_zglt035-moeda_doc,
                   'LM'                             TO gwa_zglt035-blart,
                   gwa_saida_0200-nro_liq_cb        TO gwa_zglt035-xblnr,
                   'COBRO EXPORTACION'              TO gwa_zglt035-bktxt,
                   gwa_saida_0200-date_of_deal      TO gwa_zglt035-bldat,
                   gwa_saida_0200-date_of_deal      TO gwa_zglt035-budat,
                   gwa_saida_0200-date_of_deal      TO gwa_zglt035-dt_lcto,
                   sy-uname                         TO gwa_zglt035-usnam,
                   sy-datum                         TO gwa_zglt035-dt_entrada,
                   sy-uzeit                         TO gwa_zglt035-hr_entrada,
                   gwa_saida_0200-date_of_deal+4(2) TO gwa_zglt035-monat,
                   gwa_saida_0200-date_of_deal(4)   TO gwa_zglt035-gjahr.


          LOOP AT git_zglt032 INTO gwa_zglt032.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = gwa_zglt032-buzei
              IMPORTING
                output = gwa_zglt036-seqitem.

            MOVE:
                         gwa_zglt032-tp_lcto        TO gwa_zglt036-tp_lcto,
                         gwa_zglt032-bschl          TO gwa_zglt036-bschl,
                         gwa_zglt032-hkont          TO gwa_zglt036-hkont,
                         'CIERRE CAMBIO'            TO gwa_zglt036-sgtxt,
                         'T001'                     TO gwa_zglt036-gsber,
                         gwa_zglt032-umskz          TO gwa_zglt036-umskz,

                         abs( gwa_saida_0200-amount_dealt )   TO gwa_zglt036-vlr_moeda_doc,
                         abs( gwa_saida_0200-counter_amount ) TO gwa_zglt036-vlr_moeda_int,
                         abs( gwa_saida_0200-amount_dealt )   TO gwa_zglt036-vlr_moeda_forte.


            APPEND gwa_zglt036 TO git_zglt036.
            CLEAR: gwa_zglt036, gwa_zglt032.

          ENDLOOP.

          CALL METHOD zcl_gerar_lote=>contabilizar_lote(
            CHANGING
              i_zglt036 = git_zglt036[]
              i_zglt035 = gwa_zglt035 ).


          MOVE: gwa_zglt035-lote     TO gwa_saida_0200-lote,
                gwa_zglt035-doc_lcto TO gwa_saida_0200-doc_lcto.

          MODIFY git_saida_0200 FROM gwa_saida_0200 INDEX gwa_sel_rows-index.

          "Atualizar tabela ZFIYT0033
          gwa_zfiyt0033-source_ref        = gwa_saida_0200-source_ref.
          gwa_zfiyt0033-date_of_deal      = gwa_saida_0200-date_of_deal.
          gwa_zfiyt0033-time_of_deal      = gwa_saida_0200-time_of_deal.
          gwa_zfiyt0033-cont_part_name    = gwa_saida_0200-cont_part_name.
          gwa_zfiyt0033-amount_dealt      = gwa_saida_0200-amount_dealt.
          gwa_zfiyt0033-counter_amount    = gwa_saida_0200-counter_amount.
          gwa_zfiyt0033-counterparty_nam 	=	gwa_saida_0200-counterparty_nam.
          gwa_zfiyt0033-spot_basis_rate   = gwa_saida_0200-spot_basis_rate.
          gwa_zfiyt0033-trader_name       = gwa_saida_0200-trader_name.
          gwa_zfiyt0033-currency_1        = gwa_saida_0200-currency_1.
          gwa_zfiyt0033-valor_aplic       = gwa_saida_0200-valor_aplic.
          gwa_zfiyt0033-sdo_a_apl         = gwa_saida_0200-sdo_a_apl.
          gwa_zfiyt0033-nro_liq_cb        = gwa_saida_0200-nro_liq_cb.
          gwa_zfiyt0033-cod_fechto        = gwa_saida_0200-cod_fechto.
          gwa_zfiyt0033-lote              = gwa_saida_0200-lote.
          gwa_zfiyt0033-doc_lcto          = gwa_saida_0200-doc_lcto.
          gwa_zfiyt0033-tp_lcto           = gwa_saida_0200-tp_lcto.
          gwa_zfiyt0033-dt_aplic          = sy-datum.
          gwa_zfiyt0033-hr_aplic          = sy-uzeit.
          gwa_zfiyt0033-usnam_apl         = sy-uname.


          APPEND gwa_zfiyt0033 TO git_zfiyt0033.
          CLEAR:gwa_zfiyt0033.

*          MODIFY zfiyt0033 FROM gwa_zfiyt0033 .
*          COMMIT WORK.
*
*          CLEAR: gwa_zfiyt0033.

        ELSE.
          MESSAGE 'Tipo de Lançamento não Encontrado!'(013) TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF git_zfiyt0033[] IS NOT INITIAL.
      MODIFY zfiyt0033 FROM TABLE git_zfiyt0033[].
      COMMIT WORK.

      PERFORM: f_refresh_alv USING 0200,
               f_executar.

      MESSAGE 'Lote Gerado com Sucesso!'(011) TYPE 'S'.
      CALL SCREEN 0200.
      EXIT.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DATA
*&---------------------------------------------------------------------*
FORM f_check_data_0200 USING   p_er_data_changed
                TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_lvc_modi TYPE lvc_s_modi.
  DATA: mod_data TYPE lvc_t_modi.
  DATA: lva_count TYPE i.

  DATA: lva_tp_lcto    TYPE ty_saida_0200-tp_lcto,
        lva_nro_liq_cb TYPE ty_saida_0200-nro_liq_cb,
        lva_cod_fechto TYPE ty_saida_0200-cod_fechto.

  LOOP AT p_er_data_changed->mt_mod_cells INTO ls_lvc_modi.
    CASE ls_lvc_modi-fieldname.

      WHEN 'TP_LCTO'.

        CALL METHOD p_er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = ls_lvc_modi-fieldname
          IMPORTING
            e_value     = lva_tp_lcto.

        READ TABLE p_er_data_changed->mt_good_cells
        INTO ls_lvc_modi
        WITH KEY row_id = ls_lvc_modi-row_id
        fieldname = 'TP_LCTO'.

        IF sy-subrc = 0.
          READ TABLE git_saida_0200 INTO gwa_saida_0200 INDEX ls_lvc_modi-row_id.
          MOVE lva_tp_lcto TO gwa_saida_0200-tp_lcto.
          MODIFY git_saida_0200 FROM gwa_saida_0200 INDEX ls_lvc_modi-row_id TRANSPORTING tp_lcto.
        ENDIF.

      WHEN 'NRO_LIQ_CB'.
        CALL METHOD p_er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = ls_lvc_modi-fieldname
          IMPORTING
            e_value     = lva_nro_liq_cb.

        READ TABLE p_er_data_changed->mt_good_cells
        INTO ls_lvc_modi
        WITH KEY row_id = ls_lvc_modi-row_id
        fieldname = 'NRO_LIQ_CB'.

        IF sy-subrc = 0.
          READ TABLE git_saida_0200 INTO gwa_saida_0200 INDEX ls_lvc_modi-row_id.
          MOVE lva_nro_liq_cb TO gwa_saida_0200-nro_liq_cb.
          MODIFY git_saida_0200 FROM gwa_saida_0200 INDEX ls_lvc_modi-row_id TRANSPORTING nro_liq_cb.
        ENDIF.
      WHEN  'COD_FECHTO'.
        CALL METHOD p_er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = ls_lvc_modi-fieldname
          IMPORTING
            e_value     = lva_cod_fechto.

        READ TABLE p_er_data_changed->mt_good_cells
        INTO ls_lvc_modi
        WITH KEY row_id = ls_lvc_modi-row_id
        fieldname = 'COD_FECHTO'.

        IF sy-subrc = 0.
          READ TABLE git_saida_0200 INTO gwa_saida_0200 INDEX ls_lvc_modi-row_id.
          MOVE lva_cod_fechto TO gwa_saida_0200-cod_fechto.
          MODIFY git_saida_0200 FROM gwa_saida_0200 INDEX ls_lvc_modi-row_id TRANSPORTING cod_fechto.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_STYLE_CELLTAB
*&---------------------------------------------------------------------*
FORM f_style_celltab USING p_screen.
*  Estrutura que ira alimentar a tabela com os layout especificos de cada campo
* de cada registro.
  DATA :
    lst_style TYPE lvc_s_styl,
    lva_tabix TYPE sy-tabix.

  CASE  p_screen..

    WHEN '0100'.
      LOOP AT git_saida_0100 INTO gwa_saida_0100.
        lva_tabix  = sy-tabix.

        IF gwa_saida_0100-vlr_aduana IS NOT INITIAL.

          lst_style-fieldname = 'VLR_ADUANA'.
          lst_style-style     = cl_gui_alv_grid=>mc_style_disabled.
          INSERT lst_style INTO TABLE gwa_saida_0100-celltab.

        ENDIF.
        MODIFY git_saida_0100 FROM gwa_saida_0100 INDEX lva_tabix .
      ENDLOOP.

    WHEN '0200'.
      LOOP AT git_saida_0200 INTO gwa_saida_0200.
        lva_tabix  = sy-tabix.

        IF gwa_saida_0200-tp_lcto IS NOT INITIAL.
          lst_style-fieldname = 'TP_LCTO'.
          lst_style-style     = cl_gui_alv_grid=>mc_style_disabled.
          INSERT lst_style INTO TABLE gwa_saida_0200-celltab.
        ENDIF.

        IF gwa_saida_0200-nro_liq_cb IS NOT INITIAL.
          lst_style-fieldname = 'NRO_LIQ_CB'.
          lst_style-style     = cl_gui_alv_grid=>mc_style_disabled.
          INSERT lst_style INTO TABLE gwa_saida_0200-celltab.
        ENDIF.

        IF gwa_saida_0200-cod_fechto IS NOT INITIAL.
          lst_style-fieldname = 'COD_FECHTO'.
          lst_style-style     = cl_gui_alv_grid=>mc_style_disabled.
          INSERT lst_style INTO TABLE gwa_saida_0200-celltab.
        ENDIF.

        MODIFY git_saida_0200 FROM gwa_saida_0200 INDEX lva_tabix .

      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DATA_0100
*&---------------------------------------------------------------------*
FORM f_check_data_0100  USING  p_er_data_changed
                TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_lvc_modi TYPE lvc_s_modi.
  DATA: mod_data TYPE lvc_t_modi.

  DATA: l_vlr_aduana TYPE ty_saida_0100-vlr_aduana.

  LOOP AT p_er_data_changed->mt_mod_cells INTO ls_lvc_modi.
    CASE ls_lvc_modi-fieldname.

      WHEN 'VLR_ADUANA'.

        CALL METHOD p_er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_lvc_modi-row_id
            i_fieldname = ls_lvc_modi-fieldname
          IMPORTING
            e_value     = l_vlr_aduana.

        READ TABLE p_er_data_changed->mt_good_cells
        INTO ls_lvc_modi
        WITH KEY row_id = ls_lvc_modi-row_id
        fieldname = 'VLR_ADUANA'.

        IF sy-subrc = 0.
          READ TABLE git_saida_0100 INTO gwa_saida_0100 INDEX ls_lvc_modi-row_id.
          MOVE l_vlr_aduana  TO gwa_saida_0100-vlr_aduana.
          MODIFY git_saida_0100 FROM gwa_saida_0100 INDEX ls_lvc_modi-row_id TRANSPORTING vlr_aduana.
        ENDIF.

    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERA_APLIC_MANUAL
*&---------------------------------------------------------------------*
FORM f_gera_aplic_manual .

  DATA: lva_seq TYPE ty_saida_0101_permisso-seq.

  CLEAR: git_sel_rows_cambio[], gwa_sel_rows_cambio,
         git_sel_rows_permis[], gwa_sel_rows_permis.


  CALL METHOD obj_alv_0101_cambio->get_selected_rows
    IMPORTING
      et_index_rows = git_sel_rows_cambio.

  IF lines( git_sel_rows_cambio[] ) > 1.
    MESSAGE 'Selecione somente uma linha, câmbio!'(014) TYPE 'S'.
    EXIT.
  ENDIF.

  CALL METHOD obj_alv_0101_permisso->get_selected_rows
    IMPORTING
      et_index_rows = git_sel_rows_permis.


  IF lines( git_sel_rows_permis[] ) > 1.
    MESSAGE 'Selecione somente uma linha, permisso!'(015) TYPE 'S'.
    EXIT.
  ENDIF.


  CHECK git_sel_rows_cambio[] IS NOT INITIAL.

  LOOP AT git_sel_rows_cambio INTO gwa_sel_rows_cambio.

    CLEAR: gwa_saida_0101_cambio.
    READ TABLE git_saida_0101_cambio  INTO gwa_saida_0101_cambio  INDEX gwa_sel_rows_cambio-index.

    IF gwa_saida_0101_cambio-sdo_a_apl <> 0.

      LOOP AT git_sel_rows_permis INTO gwa_sel_rows_permis.

        CLEAR: gwa_saida_0101_permisso.
        READ TABLE git_saida_0101_permisso  INTO gwa_saida_0101_permisso  INDEX gwa_sel_rows_permis-index.

        IF gwa_saida_0101_permisso-valor_aplic = 0.

          IF gwa_saida_0101_cambio-sdo_a_apl >= gwa_saida_0101_permisso-sdo_a_apl.

            MOVE: gwa_saida_0101_permisso-sdo_a_apl  TO  gwa_saida_0101_permisso-valor_aplic,
                  gwa_saida_0101_cambio-source_ref   TO  gwa_saida_0101_permisso-source_ref,
                  gwa_saida_0101_cambio-nro_liq_cb   TO  gwa_saida_0101_permisso-nro_liq_cb.

            gwa_saida_0101_cambio-valor_aplic = gwa_saida_0101_cambio-valor_aplic +
                                                gwa_saida_0101_permisso-valor_aplic.

            gwa_saida_0101_cambio-sdo_a_apl = gwa_saida_0101_cambio-sdo_a_apl -
                                              gwa_saida_0101_permisso-valor_aplic.

            IF gwa_saida_0101_permisso-valor_aplic = gwa_saida_0101_permisso-sdo_a_apl.
              MOVE icon_complete TO  gwa_saida_0101_permisso-status.
            ENDIF.

            gwa_saida_0101_permisso-tp_aplic =  'M'.
            gwa_saida_0101_cambio-tp_aplic  =   'M'.


            MODIFY git_saida_0101_permisso FROM gwa_saida_0101_permisso INDEX gwa_sel_rows_permis-index.
            MODIFY git_saida_0101_cambio   FROM gwa_saida_0101_cambio   INDEX gwa_sel_rows_cambio-index.

          ELSE.

            MOVE: gwa_saida_0101_cambio-sdo_a_apl   TO  gwa_saida_0101_permisso-valor_aplic,
                  gwa_saida_0101_cambio-source_ref  TO  gwa_saida_0101_permisso-source_ref,
                  gwa_saida_0101_cambio-nro_liq_cb  TO  gwa_saida_0101_permisso-nro_liq_cb,
                  icon_complete                     TO  gwa_saida_0101_permisso-status,
                  'M'                               TO  gwa_saida_0101_permisso-tp_aplic.



            MODIFY git_saida_0101_permisso FROM gwa_saida_0101_permisso INDEX gwa_sel_rows_permis-index.

            IF gwa_saida_0101_permisso-sdo_a_apl > gwa_saida_0101_cambio-sdo_a_apl.

              CLEAR: lva_seq.
              PERFORM f_get_seq USING lva_seq gwa_saida_0101_permisso-vbeln.

              IF lva_seq < gwa_saida_0101_permisso-seq .
                gwa_saida_0101_per_aux-seq =   gwa_saida_0101_permisso-seq  + 1.
              ELSE.
                gwa_saida_0101_per_aux-seq =   lva_seq  + 1.
              ENDIF.


              gwa_saida_0101_per_aux-sdo_a_apl = gwa_saida_0101_permisso-sdo_a_apl -
                                                gwa_saida_0101_cambio-sdo_a_apl.

              gwa_saida_0101_per_aux-valor_aplic = 0.

              MOVE:
                     icon_generate                       TO gwa_saida_0101_per_aux-status,
                     'M'                                 TO gwa_saida_0101_per_aux-tp_aplic,
                     gwa_saida_0101_permisso-bstnk       TO gwa_saida_0101_per_aux-bstnk,
                     gwa_saida_0101_permisso-vbeln       TO gwa_saida_0101_per_aux-vbeln,
                     gwa_saida_0101_permisso-erdat       TO gwa_saida_0101_per_aux-erdat,
                     gwa_saida_0101_permisso-kunnr       TO gwa_saida_0101_per_aux-kunnr,
                     gwa_saida_0101_permisso-name1       TO gwa_saida_0101_per_aux-name1.

              APPEND gwa_saida_0101_per_aux TO git_saida_0101_permisso.
              CLEAR: gwa_saida_0101_per_aux.


              gwa_saida_0101_cambio-valor_aplic = gwa_saida_0101_cambio-valor_aplic + gwa_saida_0101_cambio-sdo_a_apl.

              IF gwa_saida_0101_cambio-valor_aplic =  gwa_saida_0101_cambio-amount_dealt.

                gwa_saida_0101_cambio-sdo_a_apl = 0.
                gwa_saida_0101_cambio-tp_aplic  = 'M'.

                MODIFY git_saida_0101_cambio   FROM gwa_saida_0101_cambio   INDEX gwa_sel_rows_cambio-index.

              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE 'Permisso, já aplicado!'(016) TYPE 'S'.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE 'Câmbio sem saldo disponivel!'(017) TYPE 'S'.
      EXIT.
    ENDIF.
  ENDLOOP.

  CALL METHOD obj_alv_0101_cambio->refresh_table_display
    EXPORTING
      is_stable = gwa_stable.

  CALL METHOD obj_alv_0101_permisso->refresh_table_display
    EXPORTING
      is_stable = gwa_stable.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_APLIC_PERMIS
*&---------------------------------------------------------------------*
FORM f_save_aplic_permis .

  DATA: gva_index TYPE sy-tabix.

  CLEAR: gwa_saida_0101_permisso, gwa_saida_0101_cambio.

  LOOP AT git_saida_0101_permisso INTO gwa_saida_0101_permisso.

    READ TABLE git_zfiyt0032 INTO gwa_zfiyt0032 WITH KEY vbeln = gwa_saida_0101_permisso-vbeln
                                                         seq   = gwa_saida_0101_permisso-seq.

    IF sy-subrc = 0 AND gwa_saida_0101_permisso-valor_aplic IS NOT INITIAL .

      IF gwa_zfiyt0032-dt_aplic IS INITIAL OR gwa_zfiyt0032-dt_aplic = '' .

        gva_index = sy-tabix.

        gwa_zfiyt0032-mandt        =  sy-mandt.
        gwa_zfiyt0032-seq          =  gwa_saida_0101_permisso-seq.
        gwa_zfiyt0032-source_ref   =  gwa_saida_0101_permisso-source_ref.
        gwa_zfiyt0032-nro_liq_cb   =  gwa_saida_0101_permisso-nro_liq_cb.
        gwa_zfiyt0032-valor_aplic  =  gwa_saida_0101_permisso-valor_aplic.
        gwa_zfiyt0032-dt_aplic     =  sy-datum.
        gwa_zfiyt0032-hr_aplic     =  sy-uzeit.
        gwa_zfiyt0032-usnam_apl    =  sy-uname.

        MODIFY git_zfiyt0032 FROM gwa_zfiyt0032 INDEX gva_index.

        MODIFY zfiyt0032 FROM gwa_zfiyt0032.
        COMMIT WORK.

        CLEAR: gwa_zfiyt0032.
      ENDIF.
    ELSE.

      READ TABLE git_zfiyt0032 INTO gwa_zfiyt0032 WITH KEY vbeln = gwa_saida_0101_permisso-vbeln.

      MOVE-CORRESPONDING gwa_saida_0101_permisso TO gwa_zfiyt0032.

      IF gwa_saida_0101_permisso-valor_aplic IS NOT INITIAL.

        gwa_zfiyt0032-mandt        =  sy-mandt.
        gwa_zfiyt0032-dt_aplic     =  sy-datum.
        gwa_zfiyt0032-hr_aplic     =  sy-uzeit.
        gwa_zfiyt0032-usnam_apl    =  sy-uname.

      ELSE.

        gwa_zfiyt0032-mandt        =  sy-mandt.
        CLEAR:  gwa_zfiyt0032-dt_aplic,
                gwa_zfiyt0032-hr_aplic,
                gwa_zfiyt0032-usnam_apl.

      ENDIF.

      APPEND gwa_zfiyt0032 TO git_zfiyt0032.

      MODIFY zfiyt0032 FROM gwa_zfiyt0032.
      COMMIT WORK.

      MOVE-CORRESPONDING gwa_saida_0101_permisso TO gwa_saida_0100.

      CLEAR: gwa_zfiyt0032.

    ENDIF.
  ENDLOOP.


  CLEAR: gva_index.
  LOOP AT git_saida_0101_cambio INTO gwa_saida_0101_cambio.

    IF gwa_saida_0101_cambio-valor_aplic <> 0 .

      READ TABLE git_zfiyt0033 INTO gwa_zfiyt0033 WITH KEY source_ref = gwa_saida_0101_cambio-source_ref.
      gva_index = sy-tabix.

      IF sy-subrc = 0.

        IF gwa_zfiyt0033-dt_aplic IS INITIAL.


          gwa_zfiyt0033-mandt       =  sy-mandt.
          gwa_zfiyt0033-sdo_a_apl   = gwa_saida_0101_cambio-sdo_a_apl.
          gwa_zfiyt0033-valor_aplic = gwa_saida_0101_cambio-valor_aplic.
          gwa_zfiyt0033-nro_liq_cb  = gwa_saida_0101_cambio-nro_liq_cb.
          gwa_zfiyt0033-dt_aplic    =  sy-datum.
          gwa_zfiyt0033-hr_aplic    =  sy-uzeit.
          gwa_zfiyt0033-usnam_apl   =  sy-uname.

          MODIFY git_zfiyt0033 FROM gwa_zfiyt0033 INDEX gva_index.

          MODIFY zfiyt0033 FROM gwa_zfiyt0033 .
          COMMIT WORK.

          CLEAR: gwa_zfiyt0033.

        ELSE.

          IF gwa_zfiyt0033-valor_aplic <> gwa_saida_0101_cambio-valor_aplic.

            gwa_zfiyt0033-mandt       =  sy-mandt.
            gwa_zfiyt0033-sdo_a_apl   = gwa_saida_0101_cambio-sdo_a_apl.
            gwa_zfiyt0033-valor_aplic = gwa_saida_0101_cambio-valor_aplic.
            gwa_zfiyt0033-nro_liq_cb  = gwa_saida_0101_cambio-nro_liq_cb.
            gwa_zfiyt0033-dt_aplic    =  sy-datum.
            gwa_zfiyt0033-hr_aplic    =  sy-uzeit.
            gwa_zfiyt0033-usnam_apl   =  sy-uname.

            MODIFY git_zfiyt0033 FROM gwa_zfiyt0033 INDEX gva_index.

            MODIFY zfiyt0033 FROM gwa_zfiyt0033 .
            COMMIT WORK.

            CLEAR: gwa_zfiyt0033.

          ENDIF.

        ENDIF.

      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT: git_saida_0101_cambio   BY date_of_deal amount_dealt.
  SORT: git_saida_0101_permisso BY erdat sdo_a_apl.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERA_APLIC_AUTOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gera_aplic_autom.

  DATA: lva_seq TYPE ty_saida_0101_permisso-seq.

  DATA: lva_sdo_a_apl    TYPE ty_saida_0101_cambio-sdo_a_apl,
        lva_index        TYPE sy-tabix,
        lva_index_cambio TYPE sy-tabix,
        lva_count        TYPE i.

  SORT: git_saida_0101_cambio   BY date_of_deal amount_dealt.

  TYPES: lit_permisso TYPE TABLE OF ty_saida_0101_permisso WITH EMPTY KEY.


  DATA(lva_pmanual) = lines( VALUE lit_permisso( FOR line IN git_saida_0101_permisso WHERE ( tp_aplic = 'M' ) ( line ) ) ).

  IF lva_pmanual = 0.

    LOOP AT git_saida_0101_cambio  INTO gwa_saida_0101_cambio.
      lva_index_cambio = sy-tabix.

      SORT: git_saida_0101_permisso BY erdat sdo_a_apl.

      CLEAR: lva_count.

      LOOP AT git_saida_0101_permisso TRANSPORTING NO FIELDS WHERE valor_aplic = 0.
        lva_count = lva_count + 1.
      ENDLOOP.

      IF lva_count IS NOT INITIAL.

        LOOP AT git_saida_0101_permisso INTO gwa_saida_0101_permisso WHERE valor_aplic = 0 .
          lva_index = sy-tabix.

          IF gwa_saida_0101_cambio-sdo_a_apl >= gwa_saida_0101_permisso-sdo_a_apl.

            MOVE:  gwa_saida_0101_permisso-sdo_a_apl  TO  gwa_saida_0101_permisso-valor_aplic,
                   gwa_saida_0101_cambio-source_ref   TO  gwa_saida_0101_permisso-source_ref,
                   gwa_saida_0101_cambio-nro_liq_cb   TO  gwa_saida_0101_permisso-nro_liq_cb.

            gwa_saida_0101_cambio-valor_aplic = gwa_saida_0101_cambio-valor_aplic +
                                                gwa_saida_0101_permisso-valor_aplic.

            gwa_saida_0101_cambio-sdo_a_apl = gwa_saida_0101_cambio-sdo_a_apl -
                                              gwa_saida_0101_permisso-valor_aplic.

            IF gwa_saida_0101_permisso-valor_aplic = gwa_saida_0101_permisso-sdo_a_apl.
              MOVE icon_complete TO  gwa_saida_0101_permisso-status.
            ENDIF.

            gwa_saida_0101_permisso-tp_aplic = 'A'.
            gwa_saida_0101_cambio-tp_aplic = 'A'.


            MODIFY git_saida_0101_permisso FROM gwa_saida_0101_permisso INDEX lva_index.
            MODIFY git_saida_0101_cambio   FROM gwa_saida_0101_cambio   INDEX lva_index_cambio.

            IF  gwa_saida_0101_cambio-sdo_a_apl = 0.
              EXIT.
            ENDIF.

          ELSE.

            MOVE: gwa_saida_0101_cambio-sdo_a_apl   TO  gwa_saida_0101_permisso-valor_aplic,
                  gwa_saida_0101_cambio-source_ref  TO  gwa_saida_0101_permisso-source_ref,
                  gwa_saida_0101_cambio-nro_liq_cb  TO  gwa_saida_0101_permisso-nro_liq_cb,
                  icon_complete                     TO  gwa_saida_0101_permisso-status,
                  'A'                               TO  gwa_saida_0101_permisso-tp_aplic.

            MODIFY git_saida_0101_permisso FROM gwa_saida_0101_permisso INDEX lva_index.

            IF gwa_saida_0101_permisso-sdo_a_apl > gwa_saida_0101_cambio-sdo_a_apl.

              CLEAR: lva_seq.
              PERFORM f_get_seq USING lva_seq gwa_saida_0101_permisso-vbeln.

              gwa_saida_0101_per_aux-seq =   gwa_saida_0101_permisso-seq  + 1.

              gwa_saida_0101_per_aux-sdo_a_apl = gwa_saida_0101_permisso-sdo_a_apl -
                                                gwa_saida_0101_cambio-sdo_a_apl.

              gwa_saida_0101_per_aux-valor_aplic = 0.

              MOVE:
                     icon_generate                       TO gwa_saida_0101_per_aux-status,
                     'A'                                 TO gwa_saida_0101_permisso-tp_aplic,
                     gwa_saida_0101_permisso-bstnk       TO gwa_saida_0101_per_aux-bstnk,
                     gwa_saida_0101_permisso-vbeln       TO gwa_saida_0101_per_aux-vbeln,
                     gwa_saida_0101_permisso-erdat       TO gwa_saida_0101_per_aux-erdat,
                     gwa_saida_0101_permisso-kunnr       TO gwa_saida_0101_per_aux-kunnr,
                     gwa_saida_0101_permisso-name1       TO gwa_saida_0101_per_aux-name1.

              APPEND gwa_saida_0101_per_aux TO git_saida_0101_permisso.
              CLEAR: gwa_saida_0101_per_aux.

              gwa_saida_0101_cambio-valor_aplic = gwa_saida_0101_cambio-valor_aplic + gwa_saida_0101_cambio-sdo_a_apl.

              IF gwa_saida_0101_cambio-valor_aplic =  gwa_saida_0101_cambio-amount_dealt.

                gwa_saida_0101_cambio-sdo_a_apl = 0.
                gwa_saida_0101_cambio-tp_aplic = 'A'.

                MODIFY git_saida_0101_cambio   FROM gwa_saida_0101_cambio INDEX lva_index_cambio.
                EXIT.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.

    CALL METHOD obj_alv_0101_cambio->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.

    CALL METHOD obj_alv_0101_permisso->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.

  ELSE.
    MESSAGE 'Cancelar permissos manuais, para geração automática!'(018) TYPE 'S'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_AULA_APLIC_PERMIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_aula_aplic_permis .
  DATA: lva_answer         TYPE c.

  TYPES: lit_permisso TYPE TABLE OF ty_saida_0101_permisso WITH EMPTY KEY.

  DATA(lva_automatico) = lines( VALUE lit_permisso( FOR line IN git_saida_0101_permisso WHERE ( tp_aplic = 'A' ) ( line ) ) ).

  IF lva_automatico <> 0.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Todos os permissos automáticos serão cancelados?'(019)
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = lva_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK lva_answer EQ '1'.

    PERFORM f_anula_per_automatico.

  ENDIF.

  DATA(lva_manual) = lines( VALUE lit_permisso( FOR line IN git_saida_0101_permisso WHERE ( tp_aplic = 'M' ) ( line ) ) ).
  IF lva_manual <> 0.
    PERFORM f_anula_per_manual.
  ENDIF.

  DATA(lva_save) = lines( VALUE lit_permisso( FOR line IN git_saida_0101_permisso WHERE ( tp_aplic = 'S' ) ( line ) ) ).
  IF lva_save <> 0.
    PERFORM f_anula_per_save.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH
*&---------------------------------------------------------------------*
FORM f_refresh.

  IF obj_alv_0100 IS NOT INITIAL.
    CALL METHOD obj_alv_0100->free.
    CALL METHOD obj_container_0100->free.
    CLEAR : obj_alv_0100,
            obj_container_0100.
  ENDIF.

  IF obj_alv_0101_cambio IS NOT INITIAL.
    CALL METHOD obj_alv_0101_cambio->free.
    CALL METHOD obj_container_0101_cambio->free.
    CLEAR: obj_alv_0101_cambio,
           obj_container_0101_cambio.
  ENDIF.

  IF obj_alv_0101_permisso IS NOT INITIAL.
    CALL METHOD obj_alv_0101_permisso->free.
    CALL METHOD obj_container_0101_permisso->free.
    CLEAR: obj_alv_0101_permisso,
           obj_container_0101_permisso.
  ENDIF.

  IF obj_alv_0200 IS NOT INITIAL.
    CALL METHOD obj_alv_0200->free.
    CALL METHOD obj_container_0200->free.
    CLEAR: obj_alv_0200,
           obj_container_0200.
  ENDIF.

  IF obj_alv_0300 IS NOT INITIAL.
    CALL METHOD obj_alv_0300->free.
    CALL METHOD obj_container_0300->free.
    CLEAR: obj_alv_0300,
           obj_container_0300.
  ENDIF.

  CLEAR:git_vbak[],
        git_vbfa[],
        git_zfiyt0032[],
        git_zfiyt0032_aux[],
        git_zfiyt0033[],
        git_zfiyt0033_aux[],
        git_zfit0083[],
        git_vbap[],
        git_vbkd[],
        git_vbfa_fat[],
        git_vbrk[],
        git_zfiyt0032_sld[],
        git_kna1[],
        git_zglt031[],
        git_zglt032[],
        git_zglt035[],
        git_zglt036[],
        git_saida_0100[],
        git_saida_0101_cambio[],
        git_saida_0101_permisso[],
        git_saida_0200[],
        git_saida_0300[].


  CLEAR: gwa_vbak,
         gwa_vbfa,
         gwa_kna1,
         gwa_zfiyt0032,
         gwa_zfiyt0033,
         gwa_zfiyt0033_aux,
         gwa_zfit0083,
         gwa_zfiyt0032_sld,
         gwa_vbfa_fat,
         gwa_vbrk,
         gwa_vbkd,
         gwa_vbap,
         gwa_zglt031,
         gwa_zglt032,
         gwa_zglt035,
         gwa_zglt036,
         gwa_saida_0100,
         gwa_saida_0101_cambio,
         gwa_saida_0101_permisso,
         gwa_saida_0101_per_aux,
         gwa_saida_0200,
         gwa_saida_0300.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0101   text
*----------------------------------------------------------------------*
FORM f_refresh_alv  USING p_alv.

  CASE p_alv.
    WHEN '0100'.
      IF obj_alv_0100 IS NOT INITIAL.
        CALL METHOD obj_alv_0100->free.
        CALL METHOD obj_container_0100->free.
        CLEAR : obj_alv_0100,
                obj_container_0100.
      ENDIF.

    WHEN '0101'.
      IF obj_alv_0100 IS NOT INITIAL.
        CALL METHOD obj_alv_0100->free.
        CALL METHOD obj_container_0100->free.
        CLEAR : obj_alv_0100,
                obj_container_0100.
      ENDIF.

      IF obj_alv_0101_cambio IS NOT INITIAL.
        CALL METHOD obj_alv_0101_cambio->free.
        CALL METHOD obj_container_0101_cambio->free.
        CLEAR: obj_alv_0101_cambio,
               obj_container_0101_cambio.
      ENDIF.

      IF obj_alv_0101_permisso IS NOT INITIAL.
        CALL METHOD obj_alv_0101_permisso->free.
        CALL METHOD obj_container_0101_permisso->free.
        CLEAR: obj_alv_0101_permisso,
               obj_container_0101_permisso.
      ENDIF.
    WHEN '0200'.
      IF obj_alv_0200 IS NOT INITIAL.
        CALL METHOD obj_alv_0200->free.
        CALL METHOD obj_container_0200->free.
        CLEAR: obj_alv_0200,
               obj_container_0200.
      ENDIF.
    WHEN '0300'.
      IF obj_alv_0300 IS NOT INITIAL.
        CALL METHOD obj_alv_0300->free.
        CALL METHOD obj_container_0300->free.
        CLEAR: obj_alv_0300,
               obj_container_0300.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_AULA_PER_AUTOMATICO
*&---------------------------------------------------------------------*
FORM f_anula_per_automatico .

  CLEAR: git_saida_0101_permisso[],
         git_saida_0101_cambio[].

  PERFORM f_solic_intercambio.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ANULA_PER_SAVE
*&---------------------------------------------------------------------*
FORM f_anula_per_save .

  DATA: lva_sdo_a_apl   TYPE zfiyt0032-sdo_a_apl,
        lva_valor_aplic TYPE zfiyt0033-valor_aplic.

  CLEAR: git_sel_rows_permis[], gwa_sel_rows_permis.

  CALL METHOD obj_alv_0101_permisso->get_selected_rows
    IMPORTING
      et_index_rows = git_sel_rows_permis.

  IF lines( git_sel_rows_permis[] ) > 1.
    MESSAGE 'Selecione somente uma linha, permisso!'(015) TYPE 'S'.
    EXIT.
  ENDIF.

  SORT git_saida_0101_permisso BY vbeln seq.
  CLEAR: gwa_saida_0101_permisso,gwa_zfiyt0032.

  READ TABLE git_sel_rows_permis INTO gwa_sel_rows_permis INDEX 1.

  READ TABLE git_saida_0101_permisso  INTO gwa_saida_0101_permisso  INDEX gwa_sel_rows_permis-index.

*** Anula o que já esta salvo em tabela.
  IF gwa_saida_0101_permisso-dt_aplic IS NOT INITIAL.

    lva_sdo_a_apl = gwa_saida_0101_permisso-valor_aplic.

    UPDATE zfiyt0032 SET sdo_a_apl    = lva_sdo_a_apl
                         valor_aplic  = 0
                         source_ref   = ''
                         dt_aplic     = ''
                         hr_aplic     = ''
                         usnam_apl    = ''
     WHERE source_ref EQ  gwa_saida_0101_permisso-source_ref
       AND seq        EQ  gwa_saida_0101_permisso-seq
       AND vbeln      EQ  gwa_saida_0101_permisso-vbeln.

    COMMIT WORK.

    CLEAR: git_zfiyt0033_aux[].
    SELECT *
      FROM zfiyt0033 APPENDING TABLE git_zfiyt0033_aux
      WHERE source_ref       EQ gwa_saida_0101_permisso-source_ref.

    READ TABLE git_zfiyt0033_aux INTO gwa_zfiyt0033_aux INDEX 1.

    CLEAR: lva_sdo_a_apl, lva_valor_aplic.

    lva_valor_aplic = gwa_zfiyt0033_aux-valor_aplic - gwa_saida_0101_permisso-valor_aplic.
    lva_sdo_a_apl   = gwa_zfiyt0033_aux-amount_dealt -  lva_valor_aplic.

    UPDATE zfiyt0033 SET valor_aplic  = lva_valor_aplic
                         sdo_a_apl    = lva_sdo_a_apl
      WHERE source_ref EQ  gwa_saida_0101_permisso-source_ref.

    COMMIT WORK.

    CLEAR: git_saida_0101_permisso[],
           git_saida_0101_cambio[].

    MESSAGE 'Permissos cancelados com sucesso!'(020) TYPE 'S'.
    EXIT.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ANULA_PER_MANUAL
*&---------------------------------------------------------------------*
FORM f_anula_per_manual .

  DATA: lva_sdo_a_apl       TYPE zfiyt0032-sdo_a_apl,
        lva_sdo_a_apl_per   TYPE zfiyt0032-sdo_a_apl,
        lva_valor_aplic     TYPE zfiyt0033-valor_aplic,
        lva_valor_aplic_per TYPE zfiyt0033-valor_aplic,
        lva_source_ref      TYPE zfiyt0032-source_ref,
        lva_vbeln           TYPE zfiyt0032-vbeln,
        lva_index           TYPE sy-tabix.

  CLEAR: git_sel_rows_permis[], gwa_sel_rows_permis,
         gwa_saida_0101_permisso, gwa_saida_0101_cambio.

  CALL METHOD obj_alv_0101_permisso->get_selected_rows
    IMPORTING
      et_index_rows = git_sel_rows_permis.

  IF lines( git_sel_rows_permis[] ) > 1.
    MESSAGE 'Selecione somente uma linha, permisso!'(015) TYPE 'S'.
    EXIT.
  ENDIF.

  CHECK git_sel_rows_permis[] IS NOT INITIAL.

  READ TABLE git_sel_rows_permis INTO gwa_sel_rows_permis INDEX 1.

  READ TABLE git_saida_0101_permisso  INTO gwa_saida_0101_permisso  INDEX gwa_sel_rows_permis-index.

  IF gwa_saida_0101_permisso-valor_aplic <> 0.

    lva_sdo_a_apl_per   = gwa_saida_0101_permisso-sdo_a_apl - gwa_saida_0101_permisso-valor_aplic.
    lva_valor_aplic_per = gwa_saida_0101_permisso-valor_aplic.
    lva_source_ref      = gwa_saida_0101_permisso-source_ref.
    lva_vbeln           = gwa_saida_0101_permisso-vbeln.

    gwa_saida_0101_permisso-valor_aplic = 0.
    gwa_saida_0101_permisso-status = icon_generate.
    gwa_saida_0101_permisso-source_ref = ''.

    MODIFY git_saida_0101_permisso FROM gwa_saida_0101_permisso INDEX gwa_sel_rows_permis-index.

    READ TABLE git_saida_0101_cambio INTO gwa_saida_0101_cambio WITH KEY source_ref = lva_source_ref.
    IF sy-subrc = 0.

      lva_index = sy-tabix.

      lva_valor_aplic  = gwa_saida_0101_cambio-valor_aplic -  lva_valor_aplic_per.
      lva_sdo_a_apl   = gwa_saida_0101_cambio-amount_dealt -  lva_valor_aplic.

      gwa_saida_0101_cambio-valor_aplic = lva_valor_aplic.
      gwa_saida_0101_cambio-sdo_a_apl   = lva_sdo_a_apl.

      MODIFY git_saida_0101_cambio FROM gwa_saida_0101_cambio INDEX lva_index.
    ENDIF.

*** Tem outra perna lançada.
    IF lva_sdo_a_apl_per <> 0 AND gwa_saida_0101_permisso-seq = 1.
      CLEAR: gwa_saida_0101_permisso.
      LOOP AT  git_saida_0101_permisso  INTO gwa_saida_0101_permisso WHERE vbeln = lva_vbeln
                                                                      AND seq <> 1.

        lva_index = sy-tabix.
        gwa_saida_0101_permisso-tp_aplic = 'X'.
        MODIFY git_saida_0101_permisso FROM gwa_saida_0101_permisso INDEX lva_index.

        CLEAR: lva_index.
        READ TABLE git_saida_0101_cambio INTO gwa_saida_0101_cambio WITH KEY source_ref = gwa_saida_0101_permisso-source_ref.
        IF sy-subrc = 0.

          lva_index = sy-tabix.

          lva_valor_aplic  = gwa_saida_0101_cambio-valor_aplic - gwa_saida_0101_permisso-valor_aplic.
          lva_sdo_a_apl   = gwa_saida_0101_cambio-amount_dealt -  lva_valor_aplic.

          gwa_saida_0101_cambio-valor_aplic = lva_valor_aplic.
          gwa_saida_0101_cambio-sdo_a_apl   = lva_sdo_a_apl.

          MODIFY git_saida_0101_cambio FROM gwa_saida_0101_cambio INDEX lva_index.
        ENDIF.

      ENDLOOP.

      DELETE git_saida_0101_permisso WHERE tp_aplic = 'X'.

    ENDIF.

    CALL METHOD obj_alv_0101_cambio->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.


    CALL METHOD obj_alv_0101_permisso->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SEQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LVA_SEQ  text
*----------------------------------------------------------------------*
FORM f_get_seq  USING  p_lva_seq p_vbeln.

  SELECT SINGLE MAX( seq )
   FROM zfiyt0032
   INTO p_lva_seq
   WHERE vbeln EQ p_vbeln.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ON_F4_0200
*&---------------------------------------------------------------------*
FORM f_on_f4_0200  USING    f_fieldname    TYPE lvc_fname
                            f_fieldvalue   TYPE lvc_value
                            fw_row_no      TYPE lvc_s_roid
                            fcl_event_data TYPE REF TO cl_alv_event_data
                            ft_bad_cells   TYPE lvc_t_modi
                            f_display      TYPE char01.

  DATA: lit_ziyt0034   TYPE TABLE OF zfiyt0034,
        lit_return_chv TYPE TABLE OF ddshretval,
        lit_dselchv    TYPE TABLE OF dselc,
        lwa_style      TYPE lvc_s_styl,
        lwa_return_chv TYPE  ddshretval,
        lwa_dselchv    TYPE  dselc.

  FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

  DATA: ls_modi TYPE lvc_s_modi.


  ASSIGN fcl_event_data->m_data->* TO  <itab>.
  CHECK sy-subrc = 0.

  READ TABLE git_saida_0200 INTO gwa_saida_0200 INDEX fw_row_no-row_id.

  CHECK sy-subrc = 0.

  CASE f_fieldname.
    WHEN 'COD_FECHTO'.

      SELECT *
        FROM zfiyt0034
        INTO TABLE lit_ziyt0034 .

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield   = 'COD_FECHTO'
          value_org  = 'S'
        TABLES
          value_tab  = lit_ziyt0034
          return_tab = lit_return_chv.

      CHECK sy-subrc = 0.

      READ TABLE git_saida_0200 INTO gwa_saida_0200 INDEX fw_row_no-row_id.
      READ TABLE gwa_saida_0200-celltab INTO lwa_style WITH KEY fieldname = 'COD_FECHTO'.

      IF sy-subrc NE 0.
        READ TABLE lit_return_chv INTO lwa_return_chv INDEX 1.
        IF sy-subrc = 0 AND lwa_return_chv-fieldval <> ''.
          ASSIGN fcl_event_data->m_data->* TO <itab>.
          ls_modi-row_id    = fw_row_no-row_id.
          ls_modi-fieldname = 'COD_FECHTO'.
          ls_modi-value     = lwa_return_chv-fieldval.
          APPEND ls_modi TO <itab>.

          fcl_event_data->m_event_handled = 'X'.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_FCHTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_save_fchto.

  CLEAR: git_sel_rows_fechto[], gwa_sel_rows_fechto,
         git_zfiyt0033, gwa_zfiyt0033 , gwa_saida_0200.

  CALL METHOD obj_alv_0200->if_cached_prop~set_prop
    EXPORTING
      propname  = 'GridModified'
      propvalue = '1'.

  CALL METHOD obj_alv_0200->check_changed_data.

  IF gva_error_in_data IS INITIAL.
    CALL METHOD obj_alv_0200->get_selected_rows
      IMPORTING
        et_index_rows = git_sel_rows_fechto.

    IF git_sel_rows_fechto[] IS NOT INITIAL.

      LOOP AT  git_sel_rows_fechto INTO gwa_sel_rows_fechto.
        READ TABLE git_saida_0200  INTO gwa_saida_0200 INDEX gwa_sel_rows_fechto-index.

        IF sy-subrc = 0 AND  gwa_saida_0200-dt_aplic IS INITIAL.

          gwa_zfiyt0033-source_ref        =    gwa_saida_0200-source_ref.
          gwa_zfiyt0033-currency_1        =    gwa_saida_0200-currency_1.
          gwa_zfiyt0033-date_of_deal      =    gwa_saida_0200-date_of_deal.
          gwa_zfiyt0033-time_of_deal      =    gwa_saida_0200-time_of_deal.
          gwa_zfiyt0033-amount_dealt      =    gwa_saida_0200-amount_dealt.
          gwa_zfiyt0033-counter_amount    =    gwa_saida_0200-counter_amount.
          gwa_zfiyt0033-spot_basis_rate   =    gwa_saida_0200-spot_basis_rate.
          gwa_zfiyt0033-nro_liq_cb        =    gwa_saida_0200-nro_liq_cb.
          gwa_zfiyt0033-cod_fechto        =    gwa_saida_0200-cod_fechto.
          gwa_zfiyt0033-tp_lcto           =    gwa_saida_0200-tp_lcto.
          gwa_zfiyt0033-trader_name       =    gwa_saida_0200-trader_name.
          gwa_zfiyt0033-counterparty_nam  =    gwa_saida_0200-counterparty_nam.
          gwa_zfiyt0033-cont_part_name    =    gwa_saida_0200-cont_part_name.
          gwa_zfiyt0033-dt_aplic          =    sy-datum.
          gwa_zfiyt0033-hr_aplic          =    sy-uzeit.
          gwa_zfiyt0033-usnam_apl         =    sy-uname.

          IF gwa_saida_0200-sdo_a_apl IS INITIAL.
            gwa_zfiyt0033-sdo_a_apl = gwa_zfiyt0033-amount_dealt.
          ELSE.
            gwa_zfiyt0033-sdo_a_apl = gwa_saida_0200-sdo_a_apl.
          ENDIF.

          APPEND gwa_zfiyt0033 TO git_zfiyt0033.
          CLEAR:gwa_zfiyt0033.
        ENDIF.
      ENDLOOP.

      IF git_zfiyt0033[] IS NOT INITIAL.
        MODIFY zfiyt0033 FROM TABLE git_zfiyt0033[].
        COMMIT WORK.

        PERFORM: f_refresh_alv USING 0200,
                 f_executar.

        MESSAGE 'Dados salvos com sucesso!'(011) TYPE 'S'.
        CALL SCREEN 0200.
        EXIT.
      ENDIF.
    ELSE.
      MESSAGE 'Selecionar Linhas!'(021) TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_PICTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_picture .

  CALL METHOD cl_gui_cfw=>flush.

  IF container IS INITIAL.

    CREATE OBJECT:  container EXPORTING container_name = 'CC_PICTURE',
                     picture  EXPORTING parent = container.

    CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
      EXPORTING
        p_object = 'GRAPHICS'
        p_name   = 'ARROW'
        p_id     = 'BMAP'
        p_btype  = 'BCOL'
      RECEIVING
        p_bmp    = l_graphic_xstr.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

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

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.
  ENDIF.

ENDFORM.
