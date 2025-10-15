FUNCTION z_fi_mercado_interno_usd.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_OPCAO) TYPE  ZCHAR02
*"     REFERENCE(P_TIPO) TYPE  ZCHAR02
*"  EXPORTING
*"     REFERENCE(IT_RESULTADO) TYPE  STANDARD TABLE
*"  TABLES
*"      IT_EMPRESA STRUCTURE  T001 OPTIONAL
*"      IT_DATA STRUCTURE  BSAD OPTIONAL
*"      IT_CLIENTE STRUCTURE  BSAD OPTIONAL
*"      IT_NR_OV STRUCTURE  BSAD OPTIONAL
*"      IT_NR_SOL STRUCTURE  ZSDT0053 OPTIONAL
*"      IT_TVAK STRUCTURE  TVAK OPTIONAL
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_saida,
           bukrs     TYPE bsad-bukrs,
           gsber     TYPE bsad-gsber,
           kunnr     TYPE bsad-kunnr,
           name1     TYPE kna1-name1,
           tipo      TYPE c LENGTH 13,
           belnr     TYPE bsad-belnr,
           augbl     TYPE bsad-augbl,
           budat     TYPE bsad-budat,
           augdt     TYPE bsad-augdt,
           vbel2     TYPE bsad-vbel2,
           auart     TYPE vbak-auart,
           vbeln     TYPE bsad-vbeln,
           nr_sol    TYPE zsdt0053-nro_sol_ov,
           tp_venda  TYPE zsdt0051-tp_venda,
           dmbtr     TYPE bsad-dmbtr,
           dmbe2     TYPE bsad-dmbe2,
           tx_camb   TYPE zlest0061-tax_dolar,
           banco_liq TYPE skat-txt50,
           zfbdt     TYPE bsad-zfbdt,
           butxt     TYPE t001-butxt,
           belnr_bx  TYPE bsad-belnr,
           budat_bx  TYPE bsad-budat,
           augdt_bx  TYPE bsad-augdt,
           dmbtr_bx  TYPE bsad-dmbtr,
           dmbe2_bx  TYPE bsad-dmbe2,
           vbel2_bx  TYPE bsad-vbel2,
           vbeln_bx  TYPE bsad-vbeln,
           matnr     TYPE mara-matnr,
           maktx     TYPE makt-maktx,
           buzei     TYPE bsad-buzei,
           charg     TYPE vbap-charg,
           waers     TYPE bsad-waers,
           tpsim     TYPE char2,
           matkl     TYPE mara-matkl,
           ajuste_financeiro TYPE zfit0087-ajuste_financeiro, "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
         END OF ty_saida,

         BEGIN OF ty_bsad,
           belnr      TYPE bsad-belnr,
           augbl      TYPE bsad-augbl,
           bukrs      TYPE bsad-bukrs,
           gjahr      TYPE bsad-gjahr,
           auggj      TYPE bsad-auggj,
           vbel2      TYPE bsad-vbel2,
           posn2      TYPE bsad-posn2,
           gsber      TYPE bsad-gsber,
           kunnr      TYPE bsad-kunnr,
           zfbdt      TYPE bsad-zfbdt,
           zbd1t      TYPE bsad-zbd1t,
           budat      TYPE bsad-budat,
           augdt      TYPE bsad-augdt,
           vbeln      TYPE bsad-vbeln,
           dmbtr      TYPE bsad-dmbtr,
           dmbe2      TYPE bsad-dmbe2,
           umsks      TYPE bsad-umsks,
           buzei      TYPE bsad-buzei,
           waers      TYPE bsad-waers,
           prop_banco TYPE p DECIMALS 10, "PD Ini
           dmbe2_tot  TYPE bsad-dmbe2,    "PD Ini
           ajuste_finan TYPE c,   "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
         END OF ty_bsad,

         "Estrutura para guardar os totais da BSAD.
         BEGIN OF ty_total_bsad,
           augbl TYPE bsad-augbl,
           total TYPE bsad-dmbtr,
         END OF ty_total_bsad.

*-----------------------------------
* Tabelas Internas
*-----------------------------------
  DATA: gt_bsad              TYPE TABLE OF ty_bsad,     "Contab.financ.: índice secundário p/clientes (partida liq.)
        gt_bsad_ajuste_finan TYPE TABLE OF ty_bsad,     "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
        gt_bsad_pd           TYPE TABLE OF ty_bsad,     "Contab.financ.: índice secundário p/clientes (partida liq.)  "PD Ini
        gt_bsad_aux          TYPE TABLE OF ty_bsad,     "Contab.financ.: índice secundário p/clientes (partida liq.)
        gt_bsad_ba           TYPE TABLE OF ty_bsad,     "Contab.financ.: índice secundário p/clientes (partida liq.)
        gt_bsis              TYPE TABLE OF bsis,     "Contabilidade financ.: índice secundário p/contas do Razão
        gt_bsis_aux          TYPE TABLE OF bsis,     "Contabilidade financ.: índice secundário p/contas do Razão
        gt_bsid              TYPE TABLE OF bsid,
        gt_bsid_ajuste_finan TYPE TABLE OF bsid,      "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
        gt_bsid_aux          TYPE TABLE OF bsid,
        gt_ska1              TYPE TABLE OF ska1,     "Mestre de contas do Razão (plano de contas)
        gt_skat              TYPE TABLE OF skat,     "Mestre de contas do Razão (plano de contas: denominação)
        gt_zsdt0041          TYPE TABLE OF zsdt0041, "Tabela de Solicitação de ordem de venda - MATERIAIS
        gt_zsdt0090          TYPE TABLE OF zsdt0090, "Tabela de Solicitação de Ordem de Venda – Formação de Lote
        gt_zsdt0040          TYPE TABLE OF zsdt0040, "Tabela de Solicitação Ordem de Venda - Cabeçalho
        gt_vbak              TYPE TABLE OF vbak,     "Contabilidade: índice secundário p/fornecedores (part.comp.)
        gt_kna1              TYPE TABLE OF kna1,     "Mestre de clientes (parte geral)
        gt_t001              TYPE TABLE OF t001,     "Empresas
        gt_mara              TYPE TABLE OF mara,     "Dados gerais de material
        gt_makt              TYPE TABLE OF makt,     "Textos breves de material
        gt_bkpf              TYPE TABLE OF bkpf,
        gt_vbfa              TYPE TABLE OF vbfa,
        gt_total             TYPE TABLE OF ty_total_bsad,
        gt_zfit0087_aux      TYPE TABLE OF zfit0087, "O.V's não vinculadas para o processo da ZFI0064
        gt_zfit0087          TYPE TABLE OF zfit0087, "O.V's não vinculadas para o processo da ZFI0064
        gt_saida             TYPE TABLE OF ty_saida, "Estrutura de Saída.
        gt_saida_aux         TYPE TABLE OF ty_saida. "Estrutura de Saída.

*-----------------------------------
* Work Area
*-----------------------------------
  DATA: gw_bsad         TYPE ty_bsad,     "Contab.financ.: índice secundári O P/CLIENTES (PARTIDA LIQ.)
        gw_bsad_aux     TYPE ty_bsad,     "Contab.financ.: índice secundário p/clientes (partida liq.)
        gw_bsad_ba      TYPE ty_bsad,     "Contab.financ.: índice secundário p/clientes (partida liq.)
        gw_bsis         TYPE bsis,     "Contabilidade financ.: índice secundário p/contas do Razão
        gw_bsid         TYPE bsid,
        gw_ska1         TYPE ska1,     "Mestre de contas do Razão (plano de contas)
        gw_skat         TYPE skat,     "Mestre de contas do Razão (plano de contas: denominação)
        gw_zsdt0041     TYPE zsdt0041, "Tabela de Solicitação de ordem de venda - MATERIAIS
        gw_zsdt0090     TYPE zsdt0090, "Tabela de Solicitação de Ordem de Venda – Formação de Lote
        gw_zsdt0040     TYPE zsdt0040, "Tabela de Solicitação Ordem de Venda - Cabeçalho
        gw_vbak         TYPE vbak,     "Contabilidade: índice secundário p/fornecedores (part.comp.)
        gw_kna1         TYPE kna1,     "Mestre de clientes (parte geral)
        gw_t001         TYPE t001,     "Empresas
        gw_mara         TYPE mara,     "Dados gerais de material
        gw_makt         TYPE makt,     "Textos breves de material
        gw_total        TYPE ty_total_bsad,
        gw_zfit0087     TYPE zfit0087, "O.V's não vinculadas para o processo da ZFI0064
        gw_zfit0087_aux TYPE zfit0087,
        gw_bkpf         TYPE bkpf,
        gw_bsak         TYPE bsak,
        gw_bseg         TYPE bseg,
        gw_vbfa         TYPE vbfa,
        gw_vbap         TYPE vbap,
        gw_saida        TYPE ty_saida,
        gw_saida_aux    TYPE ty_saida.

*-----------------------------------
* TABLES
*-----------------------------------
  DATA: gw_empresa TYPE t001,
        gw_centro  TYPE t001w,
        gw_data    TYPE bsad,
        gw_cliente TYPE bsad,
        gw_nr_ov   TYPE bsad,
        gw_nr_sol  TYPE zsdt0053.


  DATA: var_tabix   TYPE sy-tabix,
        var_saknr   TYPE c LENGTH 10,
        "PD Ini
        v_obj_key   TYPE zib_contabil-obj_key,
        v_seqitem   TYPE zib_contabil-seqitem,
        v_dmbe2_tot TYPE bsad-dmbe2.
  "PD Fim


  DATA: var_linhas      TYPE sy-tabix,
        var_data_antiga TYPE c.

  "Ajustes CS2019001627 - Ini
  TYPES: BEGIN OF ty_contas,
           hkont TYPE bsis-hkont,
         END OF ty_contas.

  DATA: gt_bsis_jd TYPE TABLE OF bsis WITH HEADER LINE,
        t_conta    TYPE STANDARD TABLE OF rgsb4 WITH HEADER LINE,
        it_contas  TYPE TABLE OF ty_contas WITH HEADER LINE,
        wa_contas  TYPE ty_contas.
  "Ajustes CS2019001627 - Fim

*-----------------------------------
* VARIAVEIS
*-----------------------------------
  DATA: var_true       TYPE c,
        var_augbl      TYPE bsad-augbl,
        v_dmbtr        TYPE zfit0087-dmbtr,
        v_dmbe2        TYPE zfit0087-dmbe2,
        v_buzei_aux(3) TYPE c.

*------------------------------------
* CONSTANTE PARA GAMBETA
*------------------------------------
  DATA: cs_tax_fix   TYPE zlest0061-tax_dolar VALUE '3.7870'.

*------------------------------------
* VARIAVEIS PARA GAMBETA
*------------------------------------
  DATA: var_dmbe2 TYPE bsad-dmbe2.


*-----------------------------------
* RANGES
*-----------------------------------
  RANGES it_bukrs   FOR t001-bukrs.
  RANGES it_werks   FOR t001w-werks.
  RANGES it_augdt   FOR bsad-augdt.
  RANGES it_kunnr   FOR bsad-kunnr.
  RANGES it_vbel2   FOR bsad-vbel2.
  RANGES it_nro_sol FOR zsdt0053-nro_sol_ov.
  RANGES it_auart   FOR vbak-auart.
  "RANGES r_zterm_troca_acerto FOR bsad-zterm. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
  "RANGES r_zterm_ajuste_finan FOR bsad-zterm. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

  "Criar os Ranges.

  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*  SELECT *
*    FROM tvarvc INTO TABLE @DATA(lit_tvarvc_troca_acerto)
*   WHERE name LIKE 'ZFI0064_ZTERM_TROCA_ACERTO'.
*
*  SELECT *
*    FROM tvarvc INTO TABLE @DATA(lit_tvarvc_ajuste_finan)
*   WHERE name LIKE 'ZFI0064_ZTERM_AJUSTE_FINAN'.
*
*  LOOP AT lit_tvarvc_troca_acerto ASSIGNING FIELD-SYMBOL(<fs_tvarvc_troca_acerto>).
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_tvarvc_troca_acerto>-low ) TO r_zterm_troca_acerto.
*  ENDLOOP.
*
*  LOOP AT lit_tvarvc_ajuste_finan ASSIGNING FIELD-SYMBOL(<fs_tvarvc_ajuste_finan>).
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_tvarvc_ajuste_finan>-low ) TO r_zterm_ajuste_finan.
*  ENDLOOP.
*
*  IF r_zterm_troca_acerto[] IS INITIAL.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'C002') TO r_zterm_troca_acerto.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'C003') TO r_zterm_troca_acerto.
*  ENDIF.
*
*  IF r_zterm_ajuste_finan[] is INITIAL.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'C006') TO r_zterm_ajuste_finan.
*  ENDIF.
  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

  DESCRIBE TABLE it_empresa LINES var_linhas.
  CASE var_linhas.
    WHEN: '1'.
      CLEAR: gw_empresa.
      READ TABLE it_empresa INTO gw_empresa INDEX 1.
      it_bukrs-sign   = 'I'.
      it_bukrs-option = 'EQ'.
      IF NOT ( gw_empresa-bukrs IS INITIAL ).
        it_bukrs-low    = gw_empresa-bukrs.
        it_bukrs-high   = gw_empresa-bukrs.
        APPEND it_bukrs.
      ENDIF.
    WHEN: '2'.
      it_bukrs-sign   = 'I'.
      it_bukrs-option = 'BT'.
      READ TABLE it_empresa INTO gw_empresa INDEX 1.
      it_bukrs-low    = gw_empresa-bukrs.
      READ TABLE it_empresa INTO gw_empresa INDEX 2.
      it_bukrs-high   = gw_empresa-bukrs.
      IF NOT ( it_bukrs-low IS INITIAL  ) AND NOT ( it_bukrs-high IS INITIAL  ).
        APPEND it_bukrs.
      ENDIF.
  ENDCASE.

  CLEAR: var_linhas.

  DESCRIBE TABLE it_data LINES var_linhas.
  CASE var_linhas.
    WHEN: '1'.
      CLEAR: gw_data.
      READ TABLE it_data INTO gw_data INDEX 1.
      it_augdt-sign   = 'I'.
      it_augdt-option = 'EQ'.
      it_augdt-low    = gw_data-augdt.
      APPEND it_augdt.

      IF ( gw_data-augdt(4) EQ '2013' ) OR ( gw_data-augdt(4) EQ '2014').
        var_data_antiga = 'X'.
      ENDIF.

    WHEN: '2'.
      it_augdt-sign   = 'I'.
      it_augdt-option = 'BT'.
      CLEAR: gw_data.
      READ TABLE it_data INTO gw_data INDEX 1.
      it_augdt-low    = gw_data-augdt.

      IF ( gw_data-augdt(4) EQ '2013' ) OR ( gw_data-augdt(4) EQ '2014').
        var_data_antiga = 'X'.
      ENDIF.

      READ TABLE it_data INTO gw_data INDEX 2.
      it_augdt-high   = gw_data-augdt.
      IF NOT ( it_augdt-low IS INITIAL ) AND NOT ( it_augdt-high IS INITIAL ).
        APPEND it_augdt.
      ENDIF.
  ENDCASE.

  CLEAR: var_linhas.
  DESCRIBE TABLE it_cliente LINES var_linhas.


  CASE var_linhas.
    WHEN: '1'.
      CLEAR: gw_cliente.
      READ TABLE it_cliente INTO gw_cliente INDEX 1.
      it_kunnr-sign   = 'I'.
      it_kunnr-option = 'EQ'.
      it_kunnr-low    = gw_cliente-kunnr.
      APPEND it_kunnr.
    WHEN: '2'.

      it_kunnr-sign   = 'I'.
      it_kunnr-option = 'BT'.

      CLEAR: gw_cliente.
      READ TABLE it_cliente INTO gw_cliente INDEX 1.
      it_kunnr-low    = gw_cliente-kunnr.
      READ TABLE it_cliente INTO gw_cliente INDEX 2.
      it_kunnr-high   = gw_cliente-kunnr.
      IF NOT ( it_kunnr-low IS INITIAL ) AND NOT ( it_kunnr-high IS INITIAL ).
        APPEND it_kunnr.
      ENDIF.

    WHEN OTHERS.

      it_kunnr-sign   = 'I'.
      it_kunnr-option = 'EQ'.

      LOOP AT it_cliente INTO gw_cliente.
        it_kunnr-low    = gw_cliente-kunnr.
        APPEND it_kunnr.
      ENDLOOP.
  ENDCASE.


  CLEAR: var_linhas.
  DESCRIBE TABLE it_nr_ov LINES var_linhas.
  CASE var_linhas.
    WHEN: '1'.
      CLEAR: gw_nr_ov.
      READ TABLE it_nr_ov INTO gw_nr_ov INDEX 1.
      it_vbel2-sign   = 'I'.
      it_vbel2-option = 'EQ'.
      it_vbel2-low    = gw_nr_ov-vbel2.
      it_vbel2-high   = gw_nr_ov-vbel2.
      APPEND it_vbel2.
    WHEN: '2'.
      it_vbel2-sign   = 'I'.
      it_vbel2-option = 'BT'.
      READ TABLE it_nr_ov INTO gw_nr_ov INDEX 1.
      it_vbel2-low    = gw_nr_ov-vbel2.
      READ TABLE it_nr_ov INTO gw_nr_ov INDEX 2.
      it_vbel2-high    = gw_nr_ov-vbel2.
      IF NOT ( it_vbel2-low IS INITIAL ) AND NOT ( it_vbel2-high IS INITIAL ).
        APPEND it_vbel2.
      ENDIF.
  ENDCASE.

  CLEAR: var_linhas.
  DESCRIBE TABLE it_nr_sol LINES var_linhas.
  CASE var_linhas.
    WHEN: '1'.
      CLEAR: gw_nr_sol.
      READ TABLE it_nr_sol INTO gw_nr_sol INDEX 1.
      it_nro_sol-sign   = 'I'.
      it_nro_sol-option = 'EQ'.
      it_nro_sol-low    = gw_nr_sol-nro_sol_ov.
      it_nro_sol-high   = gw_nr_sol-nro_sol_ov.
      APPEND it_nro_sol .
    WHEN: '2'.
      it_nro_sol-sign   = 'I'.
      it_nro_sol-option = 'BT'.
      READ TABLE it_nr_sol INTO gw_nr_sol INDEX 1.
      it_nro_sol-low = gw_nr_sol-nro_sol_ov.
      READ TABLE it_nr_sol INTO gw_nr_sol INDEX 2.
      it_nro_sol-high = gw_nr_sol-nro_sol_ov.
      IF NOT ( it_nro_sol-low IS INITIAL ) AND ( it_nro_sol-high IS INITIAL ).
        APPEND it_nro_sol.
      ENDIF.
  ENDCASE.

  "Define Tipo O.V.
  CLEAR: it_auart[].
  it_auart-sign   = 'I'.
  it_auart-option = 'EQ'.

  LOOP AT it_tvak INTO DATA(wl_tvak).
    it_auart-low   = wl_tvak-auart.
    APPEND it_auart.
  ENDLOOP.

  CHECK it_auart[] IS NOT INITIAL.

  "Ajustes CS2019001627 - Ini
  CLEAR: t_conta[], it_contas[], gt_bsis_jd[].
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_HEDGE_64_JROS'
    TABLES
      set_values    = t_conta
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT t_conta WHERE from IS NOT INITIAL.
    CLEAR: wa_contas.
    wa_contas-hkont = t_conta-from(10).
    APPEND wa_contas TO it_contas.
  ENDLOOP.
  ""Ajustes CS2019001627 - Fim

  REFRESH: gt_saida, gt_saida_aux.
  CLEAR: gw_saida, gw_saida_aux.

  "Contab.financ.: índice secundário p/clientes (partida liq.)
  CASE p_opcao.
    WHEN: 'VC'.

      SELECT a~belnr a~augbl a~bukrs a~gjahr a~auggj a~vbel2 a~posn2 a~gsber a~kunnr a~zfbdt
             a~zbd1t a~budat a~augdt a~vbeln a~dmbtr a~dmbe2 a~umsks a~buzei a~waers
        INTO CORRESPONDING FIELDS OF TABLE gt_bsad
        FROM bsad AS a
       WHERE a~bukrs     IN it_bukrs
         AND a~augdt     IN it_augdt
         AND a~kunnr     IN it_kunnr
         AND a~vbel2     IN it_vbel2
         AND a~umskz     NE 'F'
         AND a~augbl     NE space
         AND a~waers     EQ 'USD'
         AND a~vbel2     NE ''
         AND EXISTS ( SELECT b~vbelv
                        FROM zsdt0090 AS b
                       WHERE b~vbelv     EQ a~vbel2
                         AND b~categoria EQ 'C'
                         AND b~estorno   EQ '' ).

      "PD Ini
      "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*      SELECT a~belnr a~augbl a~bukrs a~gjahr a~auggj a~vbel2 a~posn2 a~gsber a~kunnr a~zfbdt
*             a~zbd1t a~budat a~augdt a~vbeln a~dmbtr a~dmbe2 a~umsks a~buzei a~waers
*        APPENDING CORRESPONDING FIELDS OF TABLE gt_bsad_pd
*        FROM bsad AS a
*       WHERE a~bukrs     IN it_bukrs
*         AND a~augdt     IN it_augdt
*         AND a~kunnr     IN it_kunnr
*         AND a~umskz     NE 'F'
*         AND a~augbl     NE space
*         AND a~waers     EQ 'USD'
*         AND a~blart     EQ 'PD'.
      "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<---
      "PD Fim

      SELECT a~belnr a~augbl a~bukrs a~gjahr a~auggj a~vbel2 a~posn2 a~gsber a~kunnr a~zfbdt
             a~zbd1t a~budat a~augdt a~vbeln a~dmbtr a~dmbe2 a~umsks a~buzei a~waers
        INTO CORRESPONDING FIELDS OF TABLE gt_bsid
        FROM bsid AS a
       WHERE a~bukrs      IN it_bukrs
         AND a~budat      IN it_augdt
         AND a~kunnr      IN it_kunnr
         AND a~umsks      NE 'W'
         AND a~waers      EQ 'USD'
         AND a~vbel2      NE ''
         AND EXISTS ( SELECT b~vbelv
                        FROM zsdt0090 AS b
                       WHERE b~vbelv     EQ a~vbel2
                         AND b~categoria EQ 'C'
                         AND b~estorno   EQ '' ).

      SELECT a~belnr a~augbl a~bukrs a~gjahr a~auggj a~vbel2 a~posn2 a~gsber a~kunnr a~zfbdt
             a~zbd1t a~budat a~augdt a~vbeln a~dmbtr a~dmbe2 a~umsks a~buzei a~waers
        APPENDING CORRESPONDING FIELDS OF TABLE gt_bsad
        FROM bsad AS a
       WHERE a~bukrs      IN it_bukrs
         AND a~budat      IN it_augdt
         AND a~kunnr      IN it_kunnr
         AND a~vbel2      IN it_vbel2
         AND a~umsks      EQ 'A'
         AND a~umskz      NE 'F'
         AND a~augbl      NE space
         AND a~waers      EQ 'USD'
         AND a~vbel2      NE ''
         AND EXISTS ( SELECT b~vbelv
                        FROM zsdt0090 AS b
                       WHERE b~vbelv     EQ a~vbel2
                         AND b~categoria EQ 'C'
                         AND b~estorno   EQ '' ).




    WHEN: 'TR'.

      SELECT a~belnr a~augbl a~bukrs a~gjahr a~auggj a~vbel2 a~posn2 a~gsber a~kunnr
             a~zfbdt a~zbd1t a~budat a~augdt a~vbeln a~dmbtr a~dmbe2 a~umsks
             a~buzei a~waers
        INTO CORRESPONDING FIELDS OF TABLE gt_bsad
        FROM bsad AS a
       WHERE a~bukrs      IN it_bukrs
         AND a~budat      IN it_augdt
         AND a~kunnr      IN it_kunnr
         AND a~vbel2      IN it_vbel2
         AND a~zterm      IN r_zterm_troca_acerto "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
         AND a~umsks      NE space
         AND a~waers      EQ 'USD'
         AND a~augbl      NE space
         AND a~umskz      NE 'F'
         AND a~vbel2      NE ''
         AND EXISTS ( SELECT b~vbelv
                        FROM zsdt0090 AS b
                       WHERE b~vbelv     EQ a~vbel2
                         AND b~categoria EQ 'C'
                         AND b~estorno   EQ '' ).

      "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
      SELECT a~belnr a~augbl a~bukrs a~gjahr a~auggj a~vbel2 a~posn2 a~gsber a~kunnr
             a~zfbdt a~zbd1t a~budat a~augdt a~vbeln a~dmbtr a~dmbe2 a~umsks
             a~buzei a~waers
        INTO CORRESPONDING FIELDS OF TABLE gt_bsad_ajuste_finan
        FROM bsad AS a
       WHERE a~bukrs      IN it_bukrs
         AND a~budat      IN it_augdt
         AND a~kunnr      IN it_kunnr
         AND a~vbel2      IN it_vbel2
         AND a~waers      EQ 'USD'
         AND a~augbl      NE space
         AND a~umskz      NE 'F'
         AND a~vbel2      NE ''
         AND EXISTS ( SELECT b~vbelv
                        FROM zsdt0090 AS b
                       WHERE b~vbelv     EQ a~vbel2
                         AND b~categoria EQ 'C'
                         AND b~estorno   EQ '' )
         AND EXISTS ( SELECT docnum
                        FROM zfit0026 AS b
                       WHERE b~bukrs  EQ a~bukrs
                         AND b~docnum EQ a~belnr
                         AND b~zterm  IN r_zterm_ajuste_finan ).

      SORT gt_bsad_ajuste_finan BY bukrs belnr gjahr vbel2 buzei.
      DELETE ADJACENT DUPLICATES FROM gt_bsad_ajuste_finan COMPARING bukrs belnr gjahr vbel2.

      LOOP AT gt_bsad_ajuste_finan ASSIGNING FIELD-SYMBOL(<fs_ajuste_finan_bsad>).
        DATA(_ok) = abap_false.
        PERFORM f_converte_lcto_ajuste_zfis26 USING <fs_ajuste_finan_bsad>-bukrs
                                                    <fs_ajuste_finan_bsad>-belnr
                                                    <fs_ajuste_finan_bsad>-waers
                                                    <fs_ajuste_finan_bsad>-vbel2
                                           CHANGING <fs_ajuste_finan_bsad>-dmbtr
                                                    <fs_ajuste_finan_bsad>-dmbe2
                                                    _ok.
        IF _ok EQ abap_true.
          <fs_ajuste_finan_bsad>-ajuste_finan = abap_true.
          APPEND <fs_ajuste_finan_bsad> TO gt_bsad.
        ENDIF.
      ENDLOOP.
      "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<----

      DELETE gt_bsad WHERE umsks EQ 'F'.

      "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*      "PD Ini
*      SELECT a~belnr a~augbl a~bukrs a~gjahr a~auggj a~vbel2 a~posn2 a~gsber a~kunnr
*             a~zfbdt a~zbd1t a~budat a~augdt a~vbeln a~dmbtr a~dmbe2 a~umsks
*             a~buzei a~waers
*        APPENDING CORRESPONDING FIELDS OF TABLE gt_bsad_pd
*        FROM bsad AS a
*       WHERE a~bukrs      IN it_bukrs
*         AND a~budat      IN it_augdt
*         AND a~kunnr      IN it_kunnr
*         AND a~zterm      IN r_zterm_troca_acerto "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*         AND a~umsks      NE space
*         AND a~waers      EQ 'USD'
*         AND a~augbl      NE space
*         AND a~umskz      NE 'F'
*         AND a~blart      EQ 'PD'.
*      "PD Fim
      "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<---

      SELECT a~belnr a~augbl a~bukrs a~gjahr a~auggj a~vbel2 a~posn2 a~gsber a~kunnr a~zfbdt
             a~zbd1t a~budat a~augdt a~vbeln a~dmbtr a~dmbe2 a~umsks a~buzei a~waers
        INTO CORRESPONDING FIELDS OF TABLE gt_bsid
        FROM bsid AS a INNER JOIN zsdt0090 AS b ON a~vbel2 = b~vbelv
       WHERE a~bukrs      IN it_bukrs
         AND a~budat      IN it_augdt
         AND a~kunnr      IN it_kunnr
         AND a~umsks      NE 'W'
         AND a~waers      EQ 'USD'
         AND a~zterm      IN r_zterm_troca_acerto "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
         AND a~blart      NE 'RV'
         AND a~vbel2      NE ''
         AND EXISTS ( SELECT b~vbelv
                        FROM zsdt0090 AS b
                       WHERE b~vbelv     EQ a~vbel2
                         AND b~categoria EQ 'C'
                         AND b~estorno   EQ '' ).

  ENDCASE.

  IF ( gt_bsad[] IS NOT INITIAL ).

    SELECT *
      FROM bkpf INTO TABLE gt_bkpf
       FOR ALL ENTRIES IN gt_bsad
     WHERE belnr EQ gt_bsad-augbl
       AND gjahr EQ gt_bsad-auggj
       AND bukrs EQ gt_bsad-bukrs
       AND waers EQ 'USD'.

    SELECT *
      FROM bkpf APPENDING TABLE gt_bkpf
       FOR ALL ENTRIES IN gt_bsad
     WHERE belnr EQ gt_bsad-belnr
       AND gjahr EQ gt_bsad-gjahr
       AND bukrs EQ gt_bsad-bukrs
       AND waers EQ 'USD'.

    "PD Ini.
    IF gt_bsad_pd[] IS NOT INITIAL.

      SELECT *
        FROM bkpf APPENDING TABLE gt_bkpf
         FOR ALL ENTRIES IN gt_bsad_pd
       WHERE belnr EQ gt_bsad_pd-belnr
         AND gjahr EQ gt_bsad_pd-gjahr
         AND bukrs EQ gt_bsad_pd-bukrs
         AND waers EQ 'USD'.

      LOOP AT gt_bsad_pd INTO DATA(wl_bsad_pd).

        READ TABLE gt_bkpf INTO gw_bkpf WITH KEY belnr = wl_bsad_pd-belnr
                                                 gjahr = wl_bsad_pd-gjahr
                                                 bukrs = wl_bsad_pd-bukrs.

        CHECK ( sy-subrc = 0 ) AND (  gw_bkpf-awkey(5) = 'ZGL17' ).

        IF wl_bsad_pd-vbel2 IS INITIAL.

          v_obj_key = gw_bkpf-awkey.
          v_seqitem = wl_bsad_pd-buzei.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = v_seqitem
            IMPORTING
              output = v_seqitem.

          SELECT SINGLE *
            FROM zib_contabil INTO @DATA(_wl_zib_ctb)
           WHERE obj_key = @v_obj_key
             AND seqitem = @v_seqitem.

          CHECK ( sy-subrc EQ 0 ) AND ( _wl_zib_ctb-vbeln IS NOT INITIAL ).

          wl_bsad_pd-vbel2 = _wl_zib_ctb-vbeln.
        ENDIF.

        CHECK ( wl_bsad_pd-vbel2 IS NOT INITIAL ) AND ( wl_bsad_pd-vbel2 IN it_vbel2 ).

        "Valida Trava Cambio
        SELECT SINGLE vbelv
          FROM zsdt0090 INTO @DATA(_wl_vbelv)
         WHERE vbelv     EQ @wl_bsad_pd-vbel2
           AND categoria EQ 'C'
           AND estorno   EQ ''.

        CHECK sy-subrc EQ 0.

        CLEAR: v_dmbe2_tot.
        LOOP AT gt_bsad_pd INTO DATA(_wl_pd_aux) WHERE bukrs = wl_bsad_pd-bukrs
                                                   AND gjahr = wl_bsad_pd-gjahr
                                                   AND belnr = wl_bsad_pd-belnr.
          ADD _wl_pd_aux-dmbe2 TO v_dmbe2_tot.
        ENDLOOP.

        CHECK v_dmbe2_tot > 0.

        wl_bsad_pd-prop_banco = wl_bsad_pd-dmbe2 / v_dmbe2_tot.
        wl_bsad_pd-dmbe2_tot  = v_dmbe2_tot.

        CLEAR: gw_bsad.

        DELETE gt_bsad WHERE bukrs = wl_bsad_pd-bukrs
                         AND belnr = wl_bsad_pd-belnr
                         AND buzei = wl_bsad_pd-buzei.

        MOVE-CORRESPONDING wl_bsad_pd TO gw_bsad.
        APPEND gw_bsad TO gt_bsad.
      ENDLOOP.
    ENDIF.
    "PD Fim.

*    Mestre de clientes (parte geral)
    SELECT *
      FROM kna1 INTO TABLE gt_kna1
       FOR ALL ENTRIES IN gt_bsad
    WHERE kunnr EQ gt_bsad-kunnr.

*    Empresa
    SELECT *
      FROM t001 INTO TABLE gt_t001
       FOR ALL ENTRIES IN gt_bsad
    WHERE bukrs EQ gt_bsad-bukrs.

    REFRESH: gt_bsad_aux[].

    SORT: gt_bsad BY augbl.
    gt_bsad_aux[] = gt_bsad[].

    LOOP AT gt_bsad INTO gw_bsad.

      var_tabix = sy-tabix.
      READ TABLE gt_bsad_aux INTO gw_bsad_aux WITH KEY belnr = gw_bsad-augbl.
      IF ( sy-subrc EQ 0 ) AND ( gw_bsad-belnr EQ gw_bsad_aux-belnr ).
        DELETE gt_bsad INDEX var_tabix.
        CONTINUE.
      ENDIF.

      CLEAR: gw_bkpf.
      READ TABLE gt_bkpf INTO gw_bkpf WITH KEY belnr = gw_bsad-augbl
                                               gjahr = gw_bsad-auggj
                                               bukrs = gw_bsad-bukrs.
      IF ( gw_bkpf-tcode EQ 'FB08' ).
        DELETE gt_bsad INDEX var_tabix.
        CONTINUE.
      ENDIF.

      CLEAR: gw_bsad, gw_bsad_aux, gw_total, gw_bkpf.
    ENDLOOP.

    IF NOT ( gt_bsad[] IS INITIAL ).

      IF ( p_opcao EQ 'VC' ).
*        Contabilidade financ.: índice secundário p/contas do Razão
        SELECT *
          FROM bsis INTO TABLE gt_bsis
           FOR ALL ENTRIES IN gt_bsad
         WHERE bukrs EQ gt_bsad-bukrs
           AND belnr EQ gt_bsad-augbl
           AND gjahr EQ gt_bsad-gjahr.

        SELECT *
          FROM bsis APPENDING TABLE gt_bsis
           FOR ALL ENTRIES IN gt_bsad
         WHERE bukrs EQ gt_bsad-bukrs
           AND belnr EQ gt_bsad-belnr
           AND gjahr EQ gt_bsad-gjahr.

        SELECT *
          FROM bsis APPENDING TABLE gt_bsis
           FOR ALL ENTRIES IN gt_bsad
         WHERE bukrs EQ gt_bsad-bukrs
           AND belnr EQ gt_bsad-augbl
           AND gjahr EQ gt_bsad-augdt(4).

        IF gt_bsis[] IS NOT INITIAL.
*          Mestre de contas do Razão (plano de contas)
          SELECT *                     "#EC CI_DB_OPERATION_OK[2431747]
            FROM ska1 INTO TABLE gt_ska1 "#EC CI_DB_OPERATION_OK[2389136]
             FOR ALL ENTRIES IN gt_bsis
           WHERE saknr EQ gt_bsis-hkont
             AND ktopl EQ '0050'
             AND ktoks EQ 'YB04'.

          SELECT *
            FROM skat INTO TABLE gt_skat
             FOR ALL ENTRIES IN gt_ska1
           WHERE saknr EQ gt_ska1-saknr
             AND spras EQ 'P'.
        ENDIF.

        gt_bsis_aux[] = gt_bsis[].

        LOOP AT gt_bsis_aux INTO gw_bsis.
          DATA(del_reg) = abap_false.

          IF ( gw_bsis-bschl NE '40' ) AND
             ( gw_bsis-bschl NE '50' ).
            del_reg = abap_true.
          ELSE.

            "Ajustes CS2019001627 - Ini
            READ TABLE it_contas WITH KEY hkont = gw_bsis-hkont.
            IF sy-subrc EQ 0.
              APPEND gw_bsis TO gt_bsis_jd.
            ENDIF.
            "Ajustes CS2019001627 - Fim

            READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.
            IF sy-subrc NE 0.
              del_reg = abap_true.
            ENDIF.
          ENDIF.

          IF del_reg EQ abap_true.
            DELETE gt_bsis WHERE bukrs = gw_bsis-bukrs
                             AND belnr = gw_bsis-belnr
                             AND buzei = gw_bsis-buzei.
          ENDIF.
        ENDLOOP.

      ENDIF.

*      Tabela de Solicitação de ordem de venda - MATERIAIS
      SELECT *
        FROM zsdt0041 INTO TABLE gt_zsdt0041
         FOR ALL ENTRIES IN gt_bsad
       WHERE vbeln         EQ gt_bsad-vbel2.
         "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

      IF gt_zsdt0041[] IS NOT INITIAL.
*        Tabela de Solicitação Ordem de Venda - Cabeçalho
        SELECT *
          FROM zsdt0040 INTO TABLE gt_zsdt0040
           FOR ALL ENTRIES IN gt_zsdt0041
         WHERE doc_simulacao EQ gt_zsdt0041-doc_simulacao.

*        Dados gerais de material
        SELECT *
          FROM mara INTO TABLE gt_mara
           FOR ALL ENTRIES IN gt_zsdt0041
         WHERE matnr EQ gt_zsdt0041-matnr.
      ENDIF.

*      Tabela de Solicitação de Ordem de Venda – Formação de Lote
      SELECT *
        FROM zsdt0090 INTO TABLE gt_zsdt0090
         FOR ALL ENTRIES IN gt_bsad
       WHERE vbelv         EQ gt_bsad-vbel2.
         "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

      IF ( gt_zsdt0090[] IS NOT INITIAL ).
*        Dados gerais de material
        SELECT *
          FROM mara APPENDING TABLE gt_mara
           FOR ALL ENTRIES IN gt_zsdt0090
         WHERE matnr EQ gt_zsdt0090-matnrv.

        SELECT *
          FROM zsdt0040 APPENDING TABLE gt_zsdt0040
           FOR ALL ENTRIES IN gt_zsdt0090
         WHERE doc_simulacao EQ gt_zsdt0090-doc_simulacao.

      ENDIF.

      IF ( gt_mara[] IS NOT INITIAL ).
*        Descrição do Material
        SELECT *
          FROM makt INTO TABLE gt_makt
           FOR ALL ENTRIES IN gt_mara
         WHERE spras EQ sy-langu
           AND matnr EQ gt_mara-matnr.
      ENDIF.

*      Documento de vendas: dados de cabeçalho
      SELECT *
        FROM vbak INTO TABLE gt_vbak
         FOR ALL ENTRIES IN gt_bsad
       WHERE vbeln EQ gt_bsad-vbel2.
      "AND AUART IN IT_AUART. "LES - ZFI0064 - Ajuste Rateio Impostos US #148838 - WPP
      "AND AUART IN ('ZCOP','ZCPV','ZMIT','ZREB','ZOFE','ZODF', 'ZREM','ZTRI', 'ZFEX',
      "              'ZDEF','ZSEM','ZFTE','ZOSM','ZFUT','ZTAG').

      SELECT *
        FROM vbfa INTO TABLE gt_vbfa
         FOR ALL ENTRIES IN gt_bsad
       WHERE vbeln   EQ gt_bsad-vbel2
         AND vbtyp_n IN ('C','H','L')
         AND vbtyp_v EQ 'C'.

      IF ( gt_vbfa[] IS NOT INITIAL ).
*        Tabela de Solicitação de ordem de venda - MATERIAIS
        SELECT *
          FROM zsdt0041 APPENDING TABLE gt_zsdt0041
           FOR ALL ENTRIES IN gt_vbfa
         WHERE vbeln         EQ gt_vbfa-vbelv.
           "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
      ENDIF.

    ENDIF. "IF NOT ( GT_BSAD[] IS INITIAL ).

  ENDIF. "IF NOT ( GT_BSAD[] IS INITIAL ).

  IF NOT ( gt_bsid[] IS INITIAL ).

    SELECT *
      FROM bkpf APPENDING TABLE gt_bkpf
       FOR ALL ENTRIES IN gt_bsid
     WHERE belnr EQ gt_bsid-augbl
       AND gjahr EQ gt_bsid-gjahr
       AND bukrs EQ gt_bsid-bukrs
       AND waers EQ 'USD'
       AND bstat NE 'S'.

    SELECT *
      FROM bkpf APPENDING TABLE gt_bkpf
       FOR ALL ENTRIES IN gt_bsid
     WHERE belnr EQ gt_bsid-belnr
       AND gjahr EQ gt_bsid-gjahr
       AND bukrs EQ gt_bsid-bukrs
       AND waers EQ 'USD'
       AND bstat NE 'S'.

*    Mestre de clientes (parte geral)
    SELECT *
      FROM kna1 APPENDING TABLE gt_kna1
       FOR ALL ENTRIES IN gt_bsid
    WHERE kunnr EQ gt_bsid-kunnr.

    SELECT *
      FROM t001 INTO TABLE gt_t001
      FOR ALL ENTRIES IN gt_bsid
    WHERE bukrs EQ gt_bsid-bukrs.

    IF ( p_opcao EQ 'VC' ).
*      Contabilidade financ.: índice secundário p/contas do Razão
      SELECT *
        FROM bsis APPENDING TABLE gt_bsis
         FOR ALL ENTRIES IN gt_bsid
       WHERE bukrs EQ gt_bsid-bukrs
         AND belnr EQ gt_bsid-augbl
         AND gjahr EQ gt_bsid-gjahr.

      SELECT *
        FROM bsis APPENDING TABLE gt_bsis
         FOR ALL ENTRIES IN gt_bsid
       WHERE bukrs EQ gt_bsid-bukrs
         AND belnr EQ gt_bsid-belnr
         AND gjahr EQ gt_bsid-gjahr.

      SELECT *
        FROM bsis APPENDING TABLE gt_bsis
         FOR ALL ENTRIES IN gt_bsid
       WHERE bukrs EQ gt_bsid-bukrs
         AND belnr EQ gt_bsid-augbl
         AND gjahr EQ gt_bsid-augdt(4).

      IF gt_bsis[] IS NOT INITIAL.
*        Mestre de contas do Razão (plano de contas)
        SELECT * FROM ska1             "#EC CI_DB_OPERATION_OK[2431747]
           APPENDING TABLE gt_ska1     "#EC CI_DB_OPERATION_OK[2389136]
          FOR ALL ENTRIES IN gt_bsis
        WHERE saknr EQ gt_bsis-hkont
          AND ktopl EQ '0050'
          AND ktoks EQ 'YB04'.

        SELECT *
          FROM skat APPENDING TABLE gt_skat
           FOR ALL ENTRIES IN gt_ska1
         WHERE saknr EQ gt_ska1-saknr
           AND spras EQ 'P'.
      ENDIF.

      gt_bsis_aux[] = gt_bsis[].

      LOOP AT gt_bsis_aux INTO gw_bsis.
        del_reg = abap_false.

        IF ( gw_bsis-bschl NE '40' ) AND
           ( gw_bsis-bschl NE '50' ).
          del_reg = abap_true.
        ELSE.

          "Ajustes CS2019001627 - Ini
          READ TABLE it_contas WITH KEY hkont = gw_bsis-hkont.
          IF sy-subrc EQ 0.
            APPEND gw_bsis TO gt_bsis_jd.
          ENDIF.
          "Ajustes CS2019001627 - Fim

          READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.
          IF sy-subrc NE 0.
            del_reg = abap_true.
          ENDIF.
        ENDIF.

        IF del_reg EQ abap_true.
          DELETE gt_bsis WHERE bukrs = gw_bsis-bukrs
                           AND belnr = gw_bsis-belnr
                           AND buzei = gw_bsis-buzei.
        ENDIF.
      ENDLOOP.

    ENDIF.

*    Tabela de Solicitação de ordem de venda - MATERIAIS
    SELECT *
      FROM zsdt0041 APPENDING TABLE gt_zsdt0041
       FOR ALL ENTRIES IN gt_bsid
     WHERE vbeln         EQ gt_bsid-vbel2.
       "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

    IF gt_zsdt0041[] IS NOT INITIAL.
*      Tabela de Solicitação Ordem de Venda - Cabeçalho
      SELECT *
        FROM zsdt0040 APPENDING TABLE gt_zsdt0040
         FOR ALL ENTRIES IN gt_zsdt0041
      WHERE doc_simulacao EQ gt_zsdt0041-doc_simulacao.

*      Dados gerais de material
      SELECT * FROM mara
        APPENDING TABLE gt_mara
        FOR ALL ENTRIES IN gt_zsdt0041
      WHERE matnr EQ gt_zsdt0041-matnr.
    ENDIF.

*    Tabela de Solicitação de Ordem de Venda – Formação de Lote
    SELECT *
      FROM zsdt0090 APPENDING TABLE gt_zsdt0090
       FOR ALL ENTRIES IN gt_bsid
     WHERE vbelv         EQ gt_bsid-vbel2.
       "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

    IF ( gt_zsdt0090[] IS NOT INITIAL ).
*      Dados gerais de material
      SELECT * FROM mara
        APPENDING TABLE gt_mara
        FOR ALL ENTRIES IN gt_zsdt0090
      WHERE matnr EQ gt_zsdt0090-matnrv.

      SELECT *
        FROM zsdt0040 APPENDING TABLE gt_zsdt0040
         FOR ALL ENTRIES IN gt_zsdt0090
       WHERE doc_simulacao EQ gt_zsdt0090-doc_simulacao.
    ENDIF.

    IF gt_mara[] IS NOT INITIAL.
*      Descrição do Material
      SELECT *
        FROM makt APPENDING TABLE gt_makt
         FOR ALL ENTRIES IN gt_mara
       WHERE spras EQ sy-langu
         AND matnr EQ gt_mara-matnr.
    ENDIF.

*    Documento de vendas: dados de cabeçalho
    SELECT *
      FROM vbak APPENDING TABLE gt_vbak
       FOR ALL ENTRIES IN gt_bsid
     WHERE vbeln EQ gt_bsid-vbel2.
    "AND AUART IN IT_AUART. "LES - ZFI0064 - Ajuste Rateio Impostos US #148838 - WPP
    "AND AUART IN ('ZCOP','ZCPV','ZMIT','ZREB','ZOFE','ZODF', 'ZREM','ZTRI', 'ZFEX',
    "              'ZDEF','ZSEM','ZFTE','ZOSM','ZFUT','ZTAG' ).

    SELECT *
      FROM vbfa APPENDING TABLE gt_vbfa
       FOR ALL ENTRIES IN gt_bsid
     WHERE vbeln   EQ gt_bsid-vbel2
       AND vbtyp_n IN ('C','H','L')
       AND vbtyp_v EQ 'C'.

    IF ( gt_vbfa[] IS NOT INITIAL ).
*      Tabela de Solicitação de ordem de venda - MATERIAIS
      SELECT *
        FROM zsdt0041 APPENDING TABLE gt_zsdt0041
         FOR ALL ENTRIES IN gt_vbfa
       WHERE vbeln         EQ gt_vbfa-vbelv.
         "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
    ENDIF.
  ENDIF.

  SORT: gt_bsad BY belnr buzei.

  DELETE ADJACENT DUPLICATES FROM gt_bsad COMPARING belnr buzei.

  "Ajustes CS2019001627 - Ini
  SORT gt_bsis_jd BY bukrs belnr gjahr buzei.
  DELETE ADJACENT DUPLICATES FROM gt_bsis_jd COMPARING bukrs belnr gjahr buzei.
  "Ajustes CS2019001627 - fIM

*  Saída
  LOOP AT  gt_bsad INTO gw_bsad.

    CLEAR: gw_saida, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0041, gw_zsdt0040, gw_zsdt0090, gw_vbak, gw_total, gw_vbfa, gw_bsis, gw_mara, gw_makt.

    var_tabix = sy-tabix.

    CLEAR: gw_bkpf.
    READ TABLE gt_bkpf INTO gw_bkpf WITH KEY belnr = gw_bsad-augbl
                                             gjahr = gw_bsad-augdt(4)
                                             bukrs = gw_bsad-bukrs.

    "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*    IF ( sy-subrc = 0 ) AND ( gw_bkpf-blart EQ 'PD' ).
*
*      CLEAR: gw_bsak.
*      SELECT SINGLE *
*        FROM bsak INTO gw_bsak
*       WHERE belnr = gw_bsad-augbl
*         AND gjahr = gw_bsad-augdt(4)
*         AND bukrs = gw_bsad-bukrs.
*
*      IF sy-subrc = 0.
*        gw_bsad-augbl = gw_bsak-augbl.
*        gw_bsad-augdt = gw_bsak-augdt.
*        gw_bsad-belnr = gw_bkpf-belnr.
*        gw_bsad-budat = gw_bkpf-budat.
*
*        CLEAR: gw_bsad_aux.
*        SELECT SINGLE *
*          FROM bsad INTO CORRESPONDING FIELDS OF gw_bsad_aux
*         WHERE bukrs = gw_bkpf-bukrs
*           AND belnr = gw_bkpf-belnr
*           AND gjahr = gw_bkpf-gjahr
*           AND dmbtr = gw_bsad-dmbtr.
*        IF sy-subrc = 0.
*          gw_bsad-buzei = gw_bsad_aux-buzei.
*        ENDIF.
*
*      ENDIF.
*
*    ELSE.
*
*      CLEAR: gw_bkpf.
*      READ TABLE gt_bkpf INTO gw_bkpf WITH KEY belnr = gw_bsad-belnr
*                                               gjahr = gw_bsad-gjahr
*                                               bukrs = gw_bsad-bukrs.
*
*      IF ( sy-subrc = 0 ) AND ( gw_bkpf-blart EQ 'PD' ) AND (  gw_bkpf-awkey(5) = 'ZGL17' ).
*        CLEAR: gw_bsak.
*        SELECT SINGLE *
*          FROM bsak INTO gw_bsak
*         WHERE belnr = gw_bsad-belnr
*           AND gjahr = gw_bsad-gjahr
*           AND bukrs = gw_bsad-bukrs
*           AND dmbe2 = gw_bsad-dmbe2.
*
*        "PD Ini
*        IF sy-subrc NE 0.
*          CLEAR: gw_bsak.
*          SELECT SINGLE *
*            FROM bsak INTO gw_bsak
*           WHERE belnr = gw_bsad-belnr
*             AND gjahr = gw_bsad-gjahr
*             AND bukrs = gw_bsad-bukrs
*             AND dmbe2 = gw_bsad-dmbe2_tot.
*        ENDIF.
*        "PD Fim
*
*        IF sy-subrc = 0.
*          gw_bsad-augbl = gw_bsak-augbl.
*          gw_bsad-augdt = gw_bsak-augdt.
*          gw_bsad-belnr = gw_bkpf-belnr.
*          gw_bsad-budat = gw_bkpf-budat.
*
*          CLEAR: gw_bsad_aux.
*          SELECT SINGLE *
*            FROM bsad INTO CORRESPONDING FIELDS OF gw_bsad_aux
*           WHERE bukrs = gw_bkpf-bukrs
*             AND belnr = gw_bkpf-belnr
*             AND gjahr = gw_bkpf-gjahr
*             AND dmbe2 = gw_bsad-dmbe2.
*          IF sy-subrc = 0.
*            gw_bsad-buzei = gw_bsad_aux-buzei.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
    "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<---


    IF ( p_opcao EQ 'VC' ).

      CASE gw_bsad-umsks.
        WHEN: 'A'.
          READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsad-bukrs
                                                   belnr = gw_bsad-belnr
                                                   gjahr = gw_bsad-gjahr.
          IF ( sy-subrc NE 0 ).
            READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsad-bukrs
                                                     belnr = gw_bsad-augbl
                                                     gjahr = gw_bsad-gjahr.
            gw_saida-budat = gw_bsad-budat.
            gw_saida-augdt = gw_bsad-augdt.
          ELSE.

            CLEAR: gw_bsad_aux.
            READ TABLE gt_bsad INTO gw_bsad_aux WITH KEY augbl = gw_bsad-belnr.
            IF ( sy-subrc EQ 0 ) AND ( gw_bsad-umsks NE 'A' ).
              DELETE gt_bsad INDEX var_tabix.
              CONTINUE.
            ENDIF.

            gw_saida-budat = gw_bsad-budat.
            gw_saida-augdt = gw_bsad-budat.

            "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*            IF ( gw_bkpf-blart EQ 'PD' ) AND ( gw_bsak IS NOT INITIAL ).
*              gw_saida-augbl = gw_bsak-augbl.
*              gw_saida-augdt = gw_bsak-augdt.
*            ENDIF.
            "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<----

            IF ( gw_saida-augdt NOT IN it_augdt ).
              CONTINUE.
            ENDIF.
          ENDIF.

          CLEAR: gw_skat.

          READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.
          IF ( sy-subrc NE 0 ) .

            "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*            IF ( gw_bkpf-blart EQ 'PD' ).
*
*              CLEAR: gw_bsis,gw_ska1,gw_skat.
*              SELECT SINGLE *
*                FROM bsis INTO gw_bsis
*               WHERE bukrs = gw_bsad-bukrs
*                 AND belnr = gw_bsad-augbl
*                 AND gjahr = gw_bsad-augdt(4)
*                 AND bschl IN ('40','50').
*
*              "PD Ini
*              IF ( sy-subrc EQ 0 ) AND ( gw_bsad-prop_banco > 0 ).
*                gw_bsis-dmbtr = gw_bsad-prop_banco * gw_bsis-dmbtr.
*                gw_bsis-dmbe2 = gw_bsad-prop_banco * gw_bsis-dmbe2.
*              ENDIF.
*              "PD Fim
*
*              SELECT SINGLE *          "#EC CI_DB_OPERATION_OK[2389136]
*                FROM ska1 INTO gw_ska1 "#EC CI_DB_OPERATION_OK[2431747]
*               WHERE saknr EQ gw_bsis-hkont
*                 AND ktopl EQ '0050'
*                 AND ktoks EQ 'YB04'.
*
*              SELECT SINGLE *
*                FROM skat INTO gw_skat
*               WHERE saknr EQ gw_ska1-saknr
*                 AND spras EQ 'P'.
*
*              IF ( gw_ska1 IS INITIAL ) OR ( gw_bsis IS INITIAL ) OR ( gw_skat IS INITIAL ).
*                CONTINUE.
*              ENDIF.
*
*            ELSE.
            "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<-----
              CONTINUE.
            "ENDIF. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

          ENDIF.

          IF gw_skat IS INITIAL.
            READ TABLE gt_skat INTO gw_skat WITH KEY saknr = gw_ska1-saknr.
            IF ( sy-subrc NE 0 ).
              CONTINUE.
            ENDIF.
          ENDIF.

        WHEN OTHERS.

          READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsad-bukrs
                                                   belnr = gw_bsad-augbl
                                                   gjahr = gw_bsad-gjahr.
          IF ( sy-subrc NE 0 ).

            READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsad-bukrs
                                                     belnr = gw_bsad-augbl
                                                     gjahr = gw_bsad-augdt(4).
            IF ( sy-subrc NE 0 ).
              CONTINUE.
            ENDIF.

          ENDIF.

          READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.
          IF ( sy-subrc NE 0 ).
            CONTINUE.
          ENDIF.

          READ TABLE gt_skat INTO gw_skat WITH KEY saknr = gw_ska1-saknr.
          IF ( sy-subrc NE 0 ).
            CONTINUE.
          ENDIF.

          gw_saida-budat = gw_bsad-budat.
          gw_saida-augdt = gw_bsad-augdt.

      ENDCASE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = gw_ska1-saknr
        IMPORTING
          output = var_saknr.

      CONCATENATE var_saknr '-' gw_skat-txt20 INTO gw_saida-banco_liq.
    ELSE.
      gw_saida-budat = gw_bsad-budat.
      gw_saida-augdt = gw_bsad-augdt.
      gw_saida-banco_liq = 'Troca/Acerto'.
      gw_saida-ajuste_financeiro = gw_bsad-ajuste_finan. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
    ENDIF.

    IF p_opcao EQ 'TR' AND gw_bsad-ajuste_finan is INITIAL. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
      gw_saida-augdt = gw_saida-budat.
    ENDIF.

    gw_saida-bukrs = gw_bsad-bukrs.


    READ TABLE gt_t001 INTO gw_t001 WITH KEY bukrs = gw_bsad-bukrs.
    gw_saida-butxt = gw_t001-butxt.
    gw_saida-gsber = gw_bsad-gsber.
    gw_saida-kunnr = gw_bsad-kunnr.
    gw_saida-zfbdt = gw_bsad-zfbdt + gw_bsad-zbd1t.

    READ TABLE gt_kna1 INTO gw_kna1 WITH KEY kunnr = gw_bsad-kunnr.
    gw_saida-name1 = gw_kna1-name1.

    IF NOT ( gw_bsad-umsks IS INITIAL ).
      gw_saida-tipo = 'Adiantamento'.
    ELSE.
      gw_saida-tipo = 'Fatura'.
    ENDIF.

    gw_saida-belnr = gw_bsad-belnr.
    gw_saida-augbl = gw_bsad-augbl.
    gw_saida-vbel2 = gw_bsad-vbel2.
    gw_saida-vbeln = gw_bsad-vbeln.

    CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
      gw_saida-tx_camb = gw_bsis-dmbtr / gw_bsis-dmbe2.
    ENDCATCH.


    CASE p_opcao.
      WHEN: 'VC'.

        IF ( var_augbl EQ gw_bsad-augbl ).

*          Gambeta, e não me pergunte porque.
          IF ( gw_saida-augdt EQ '20150910').
            gw_saida-dmbtr   = gw_bsad-dmbtr.
            gw_saida-dmbe2   = gw_saida-dmbtr / cs_tax_fix.
          ELSE.
            gw_saida-dmbtr   = gw_bsad-dmbtr.
            CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
              gw_saida-dmbe2   = gw_saida-dmbtr / gw_saida-tx_camb.
            ENDCATCH.
          ENDIF.

        ELSE.

          var_augbl = gw_bsad-augbl.

          CASE gw_bsad-umsks.
            WHEN: 'A'.

*              Gambeta, e não me pergunte porque.
              IF ( gw_saida-augdt EQ '20150910').
                gw_saida-dmbtr = gw_bsad-dmbtr.
                gw_saida-dmbe2 = gw_saida-dmbtr / cs_tax_fix.
              ELSE.
                gw_saida-dmbtr = gw_bsad-dmbtr.
                CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
                  gw_saida-dmbe2 = gw_saida-dmbtr / gw_saida-tx_camb.
                ENDCATCH.
              ENDIF.

            WHEN OTHERS.
*              Gambeta, e não me pergunte porque.
              IF ( gw_saida-augdt EQ '20150910').
                gw_saida-dmbtr   = gw_bsad-dmbtr.
                gw_saida-dmbe2   = gw_saida-dmbtr / cs_tax_fix.
              ELSE.
                gw_saida-dmbtr   = gw_bsad-dmbtr.
                CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
                  gw_saida-dmbe2   = gw_saida-dmbtr / gw_saida-tx_camb.
                ENDCATCH.
              ENDIF.
          ENDCASE.
        ENDIF.

        "Ajustes CS2019001627 - Ini
        IF gw_bsad-umsks IS NOT INITIAL.
          DATA(_possui_juros_descontos) = abap_false.
          LOOP AT gt_bsis_jd WHERE bukrs = gw_bsad-bukrs
                               AND belnr = gw_bsad-belnr
                               AND gjahr = gw_bsad-gjahr.

            _possui_juros_descontos = abap_true.

            CASE gt_bsis_jd-shkzg.
              WHEN 'H'.
                SUBTRACT gt_bsis_jd-dmbtr FROM gw_saida-dmbtr.
              WHEN 'S'.
                ADD gt_bsis_jd-dmbtr TO gw_saida-dmbtr.
            ENDCASE.

            IF gw_saida-dmbtr > 0.
              CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
                gw_saida-dmbe2 = gw_saida-dmbtr / gw_saida-tx_camb.
              ENDCATCH.
            ENDIF.
          ENDLOOP.

          IF ( _possui_juros_descontos EQ abap_true ) AND ( gw_saida-dmbtr <= 0 ).
            CONTINUE.
          ENDIF.
        ENDIF.
        "Ajustes CS2019001627 - Fim

      WHEN: 'TR'.

*        Gambeta, e não me pergunte porque.
        IF ( gw_saida-augdt EQ '20150910').
          gw_saida-dmbtr   = gw_bsad-dmbtr.
          gw_saida-dmbe2   = gw_saida-dmbtr / cs_tax_fix.
        ELSE.
          gw_saida-dmbe2   = gw_bsad-dmbe2.
          gw_saida-dmbtr   = gw_bsad-dmbtr.
        ENDIF.

        CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
          gw_saida-tx_camb = gw_saida-dmbtr / gw_saida-dmbe2.
        ENDCATCH.

    ENDCASE.

*    Gambeta, e não me pergunte porque.
    IF ( gw_saida-augdt EQ '20150910').
      gw_saida-tx_camb = cs_tax_fix.
    ENDIF.

    READ TABLE gt_zsdt0041 INTO gw_zsdt0041 WITH KEY vbeln = gw_bsad-vbel2.
    IF ( sy-subrc NE 0 ).
      READ TABLE gt_zsdt0090 INTO gw_zsdt0090 WITH KEY vbelv = gw_bsad-vbel2.
      IF ( sy-subrc NE 0 ).

        READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsad-vbel2.

        IF ( gw_vbak-auart IS INITIAL ).
          CONTINUE.
        ELSE.
          gw_saida-auart = gw_vbak-auart.
        ENDIF.

        READ TABLE gt_zsdt0040 INTO gw_zsdt0040 WITH KEY doc_simulacao = gw_zsdt0041-doc_simulacao.
        gw_saida-tp_venda = gw_zsdt0040-tpsim.

        CASE gw_vbak-auart.
          WHEN: 'ZCPV' OR 'ZREB' OR 'ZCOP'.
            READ TABLE gt_vbfa INTO gw_vbfa WITH KEY vbeln = gw_bsad-vbel2.
            READ TABLE gt_zsdt0041 INTO gw_zsdt0041 WITH KEY vbeln = gw_vbfa-vbelv.
            gw_saida-nr_sol = gw_zsdt0041-doc_simulacao.
            READ TABLE gt_zsdt0040 INTO gw_zsdt0040 WITH KEY doc_simulacao = gw_zsdt0041-doc_simulacao.
            gw_saida-tp_venda = gw_zsdt0040-tpsim.

            READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0041-matnr.
            gw_saida-matnr = gw_mara-matnr.
            gw_saida-matkl = gw_mara-matkl.
            READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
            gw_saida-maktx = gw_makt-maktx.
        ENDCASE.

      ENDIF.

      READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsad-vbel2.

      IF ( gw_vbak-auart IS INITIAL ).
        CONTINUE.
      ELSE.
        gw_saida-auart = gw_vbak-auart.
      ENDIF.

      READ TABLE gt_zsdt0040 INTO gw_zsdt0040 WITH KEY doc_simulacao = gw_zsdt0041-doc_simulacao.
      gw_saida-tp_venda = gw_zsdt0040-tpsim.

      IF gw_zsdt0090 IS NOT INITIAL.
        READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0090-matnrv.
        gw_saida-matnr = gw_mara-matnr.
        gw_saida-matkl = gw_mara-matkl.
        READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
        gw_saida-maktx = gw_makt-maktx.
      ENDIF.


    ELSE.

      READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsad-vbel2.

      IF ( gw_vbak-auart IS INITIAL ).
        CONTINUE.
      ENDIF.

      READ TABLE gt_zsdt0040 INTO gw_zsdt0040 WITH KEY doc_simulacao = gw_zsdt0041-doc_simulacao.
      IF sy-subrc = 0.
        gw_saida-tpsim  = gw_zsdt0040-tpsim.
        gw_saida-auart  = gw_zsdt0041-auart.
      ENDIF.

      READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0041-matnr.
      IF sy-subrc = 0.
        gw_saida-matnr = gw_mara-matnr.
        gw_saida-matkl = gw_mara-matkl.
        READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
        gw_saida-maktx = gw_makt-maktx.
      ENDIF.

      gw_saida-nr_sol = gw_zsdt0041-doc_simulacao.

    ENDIF.

    "27.09.2018 - Ini
    READ TABLE gt_zsdt0041 INTO gw_zsdt0041 WITH KEY vbeln = gw_bsad-vbel2.
    IF ( gw_saida-nr_sol IS INITIAL ) AND ( sy-subrc = 0 ).

      READ TABLE gt_zsdt0040 INTO gw_zsdt0040 WITH KEY doc_simulacao = gw_zsdt0041-doc_simulacao.
      IF sy-subrc = 0.
        gw_saida-nr_sol = gw_zsdt0041-doc_simulacao.
        gw_saida-tpsim  = gw_zsdt0040-tpsim.

        READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0041-matnr.
        IF sy-subrc = 0.
          gw_saida-matnr = gw_mara-matnr.
          gw_saida-matkl = gw_mara-matkl.
          READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
          gw_saida-maktx = gw_makt-maktx.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE gt_zsdt0090 INTO gw_zsdt0090 WITH KEY vbelv = gw_bsad-vbel2.
    IF sy-subrc NE 0.
      READ TABLE gt_zsdt0090 INTO gw_zsdt0090 WITH KEY vbeln = gw_bsad-vbel2.
    ENDIF.
    IF ( gw_saida-nr_sol IS INITIAL ) AND ( sy-subrc = 0 ).
      READ TABLE gt_zsdt0040 INTO gw_zsdt0040 WITH KEY doc_simulacao = gw_zsdt0090-doc_simulacao.
      IF sy-subrc = 0.
        gw_saida-nr_sol = gw_zsdt0040-doc_simulacao.
        gw_saida-tpsim  = gw_zsdt0040-tpsim.
      ENDIF.
    ENDIF.
    "27.09.2018 - Fim

    "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*    IF ( gw_bkpf-blart EQ 'PD' ) AND
*       ( p_tipo        EQ 'IN' ) .
*      gw_saida-dmbtr = gw_bsis-dmbtr.
*      CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
*        gw_saida-dmbe2 = gw_saida-dmbtr / gw_saida-tx_camb.
*      ENDCATCH.
*    ENDIF.
*
*    IF ( gw_saida-auart EQ 'ZREB' ) OR ( gw_bkpf-blart EQ 'PD' ).
*      gw_saida-dmbe2   = gw_saida-dmbe2 * -1.
*      gw_saida-dmbtr   = gw_saida-dmbtr * -1.
*    ENDIF.
    "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<----

    gw_saida-buzei = gw_bsad-buzei.

    gw_saida-waers = 'USD'.

    IF ( gw_bsad-vbel2 IS NOT INITIAL ) AND ( gw_saida-matnr IS INITIAL OR gw_saida-matkl IS INITIAL ).
      SELECT SINGLE *
        FROM vbap INTO gw_vbap
       WHERE vbeln EQ gw_bsad-vbel2
         AND posnr EQ gw_bsad-posn2.
      IF ( sy-subrc = 0 ) AND ( gw_vbap-matnr IS NOT INITIAL ).
        SELECT SINGLE * INTO gw_mara FROM mara WHERE matnr = gw_vbap-matnr.
        IF sy-subrc = 0.
          gw_saida-matnr = gw_mara-matnr.
          gw_saida-matkl = gw_mara-matkl.
          SELECT SINGLE * INTO gw_makt FROM makt WHERE matnr = gw_mara-matnr.
          gw_saida-maktx = gw_makt-maktx.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND gw_saida TO gt_saida.
  ENDLOOP.


*  =============================== BSID =========================================================================
*  Saída
  LOOP AT  gt_bsid INTO gw_bsid.

    CLEAR: gw_saida, gw_mara, gw_makt, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0041, gw_zsdt0040, gw_zsdt0090, gw_vbak, gw_total, gw_vbfa, gw_bsis.


    var_tabix = sy-tabix.

    IF ( p_opcao EQ 'VC' ).
      CASE gw_bsid-umsks.
        WHEN: 'A'.
          READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsid-bukrs
                                                   belnr = gw_bsid-belnr
                                                   gjahr = gw_bsid-gjahr.
          IF ( sy-subrc NE 0 ).
            READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsid-bukrs
                                                     belnr = gw_bsid-augbl
                                                     gjahr = gw_bsid-gjahr.
            gw_saida-budat = gw_bsid-budat.
            gw_saida-augdt = gw_bsid-augdt.
          ELSE.

          ENDIF.

          READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.
          IF ( sy-subrc NE 0 ).
            CONTINUE.
          ENDIF.

          READ TABLE gt_skat INTO gw_skat WITH KEY saknr = gw_ska1-saknr.
          IF ( sy-subrc NE 0 ).
            CONTINUE.
          ENDIF.

        WHEN OTHERS.
          READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsid-bukrs
                                                   belnr = gw_bsid-augbl
                                                   gjahr = gw_bsid-gjahr.
          IF ( sy-subrc NE 0 ).

            READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsid-bukrs
                                                     belnr = gw_bsid-augbl
                                                     gjahr = gw_bsid-augdt(4).
            IF ( sy-subrc NE 0 ).
              CONTINUE.
            ENDIF.

          ENDIF.

          READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.
          IF ( sy-subrc NE 0 ).
            CONTINUE.
          ENDIF.

          READ TABLE gt_skat INTO gw_skat WITH KEY saknr = gw_ska1-saknr.
          IF ( sy-subrc NE 0 ).
            CONTINUE.
          ENDIF.

          gw_saida-budat = gw_bsid-budat.
          gw_saida-augdt = gw_bsid-augdt.

      ENDCASE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = gw_ska1-saknr
        IMPORTING
          output = var_saknr.

      CONCATENATE var_saknr '-' gw_skat-txt20 INTO gw_saida-banco_liq.
    ELSE.
      gw_saida-budat = gw_bsid-budat.

      IF ( gw_bsid-augdt IS INITIAL ).
        gw_saida-augdt = gw_bsid-budat.
      ELSE.
        gw_saida-augdt = gw_bsid-augdt.
      ENDIF.


      gw_saida-banco_liq = 'Troca/Acerto'.
    ENDIF.

    IF p_opcao EQ 'TR'.
      gw_saida-augdt = gw_saida-budat.
    ENDIF.

    gw_saida-bukrs = gw_bsid-bukrs.

    READ TABLE gt_t001 INTO gw_t001 WITH KEY bukrs = gw_bsid-bukrs.
    gw_saida-butxt = gw_t001-butxt.
    gw_saida-gsber = gw_bsid-gsber.
    gw_saida-kunnr = gw_bsid-kunnr.
    gw_saida-zfbdt = gw_bsid-zfbdt + gw_bsid-zbd1t.

    READ TABLE gt_kna1 INTO gw_kna1 WITH KEY kunnr = gw_bsid-kunnr.
    gw_saida-name1 = gw_kna1-name1.

    IF NOT ( gw_bsid-umsks IS INITIAL ).
      gw_saida-tipo = 'Adiantamento'.
    ELSE.
      gw_saida-tipo = 'Fatura'.
    ENDIF.

    gw_saida-belnr = gw_bsid-belnr.
    gw_saida-augbl = gw_bsid-augbl.
    gw_saida-vbel2 = gw_bsid-vbel2.
    gw_saida-vbeln = gw_bsid-vbeln.

    CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
      gw_saida-tx_camb = gw_bsis-dmbtr / gw_bsis-dmbe2.
    ENDCATCH.

    CASE p_opcao.
      WHEN: 'VC'.

        IF ( var_augbl EQ gw_bsid-augbl ).
*          Gambeta, e não me pergunte porque.
          IF ( gw_saida-augdt EQ '20150910').
            gw_saida-dmbtr   = gw_bsid-dmbtr.
            gw_saida-dmbe2   = gw_saida-dmbtr / cs_tax_fix.
          ELSE.
            gw_saida-dmbtr   = gw_bsid-dmbtr.
            CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
              gw_saida-dmbe2   = gw_saida-dmbtr / gw_saida-tx_camb.
            ENDCATCH.
          ENDIF.
        ELSE.

          var_augbl = gw_bsid-augbl.

          CASE gw_bsid-umsks.
            WHEN: 'A'.
*              Gambeta, e não me pergunte porque.
              IF ( gw_saida-augdt EQ '20150910').
                gw_saida-dmbtr = gw_bsid-dmbtr.
                gw_saida-dmbe2 = gw_saida-dmbtr / cs_tax_fix.
              ELSE.
                gw_saida-dmbtr = gw_bsid-dmbtr.
                CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
                  gw_saida-dmbe2 = gw_saida-dmbtr / gw_saida-tx_camb.
                ENDCATCH.
              ENDIF.

            WHEN OTHERS.
*              Gambeta, e não me pergunte porque.
              IF ( gw_saida-augdt EQ '20150910').
                gw_saida-dmbtr   = gw_bsid-dmbtr.
                gw_saida-dmbe2   = gw_saida-dmbtr / cs_tax_fix.
              ELSE.
                gw_saida-dmbtr   = gw_bsid-dmbtr.
                CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
                  gw_saida-dmbe2   = gw_saida-dmbtr / gw_saida-tx_camb.
                ENDCATCH.
              ENDIF.
          ENDCASE.

        ENDIF.

      WHEN: 'TR'.

*        Gambeta, e não me pergunte porque.
        IF ( gw_saida-augdt EQ '20150910').
          gw_saida-dmbtr   = gw_bsid-dmbtr.
          gw_saida-dmbe2   = gw_saida-dmbtr / cs_tax_fix.
        ELSE.
          gw_saida-dmbe2   = gw_bsid-dmbe2.
          gw_saida-dmbtr   = gw_bsid-dmbtr.
        ENDIF.

        CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
          gw_saida-tx_camb = gw_saida-dmbtr / gw_saida-dmbe2.
        ENDCATCH.

    ENDCASE.

*    Gambeta, e não me pergunte porque.
    IF ( gw_saida-augdt EQ '20150910').
      gw_saida-tx_camb = cs_tax_fix.
    ENDIF.


    READ TABLE gt_zsdt0041 INTO gw_zsdt0041 WITH KEY vbeln = gw_bsid-vbel2.
    IF ( sy-subrc NE 0 ).
      READ TABLE gt_zsdt0090 INTO gw_zsdt0090 WITH KEY vbelv = gw_bsid-vbel2.
      IF ( sy-subrc NE 0 ).

        READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsid-vbel2.

        IF ( gw_vbak-auart IS INITIAL ).
          CONTINUE.
        ELSE.
          gw_saida-auart = gw_vbak-auart.
        ENDIF.

        READ TABLE gt_zsdt0040 INTO gw_zsdt0040 WITH KEY doc_simulacao = gw_zsdt0041-doc_simulacao.
        IF sy-subrc = 0.
          gw_saida-tp_venda = gw_zsdt0040-tpsim.
        ENDIF.

        CASE gw_vbak-auart.
          WHEN: 'ZCPV' OR 'ZREB' OR 'ZCOP'.
            READ TABLE gt_vbfa INTO gw_vbfa WITH KEY vbeln = gw_bsid-vbel2.
            READ TABLE gt_zsdt0041 INTO gw_zsdt0041 WITH KEY vbeln = gw_vbfa-vbelv.
            gw_saida-nr_sol = gw_zsdt0041-doc_simulacao.
            READ TABLE gt_zsdt0040 INTO gw_zsdt0040 WITH KEY doc_simulacao = gw_zsdt0041-doc_simulacao.
            gw_saida-tp_venda = gw_zsdt0040-tpsim.

            READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0041-matnr.
            gw_saida-matnr = gw_mara-matnr.
            gw_saida-matkl = gw_mara-matkl.
            READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
            gw_saida-maktx = gw_makt-maktx.
        ENDCASE.

      ENDIF.

      READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsid-vbel2.

      IF ( gw_vbak-auart IS INITIAL ).
        CONTINUE.
      ELSE.
        gw_saida-auart = gw_vbak-auart.
      ENDIF.

      IF gw_zsdt0041 IS NOT INITIAL.
        READ TABLE gt_zsdt0040 INTO gw_zsdt0040 WITH KEY doc_simulacao = gw_zsdt0041-doc_simulacao.
        gw_saida-tp_venda = gw_zsdt0040-tpsim.
      ENDIF.

      IF gw_zsdt0090 IS NOT INITIAL.
        READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0090-matnrv.
        gw_saida-matnr = gw_mara-matnr.
        gw_saida-matkl = gw_mara-matkl.
        READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
        gw_saida-maktx = gw_makt-maktx.
      ENDIF.

    ELSE.

      READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsid-vbel2.

      IF ( gw_vbak-auart IS INITIAL ).
        CONTINUE.
      ELSE.
        gw_saida-auart = gw_vbak-auart.
      ENDIF.

      READ TABLE gt_zsdt0040 INTO gw_zsdt0040 WITH KEY doc_simulacao = gw_zsdt0041-doc_simulacao.
      IF sy-subrc = 0.
        gw_saida-auart = gw_zsdt0041-auart.
        gw_saida-tpsim = gw_zsdt0040-tpsim.
      ENDIF.

      READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0041-matnr.
      IF sy-subrc = 0.
        gw_saida-matnr = gw_mara-matnr.
        gw_saida-matkl = gw_mara-matkl.
        READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
        gw_saida-maktx = gw_makt-maktx.
      ENDIF.

      gw_saida-nr_sol = gw_zsdt0041-doc_simulacao.

    ENDIF.


    IF gw_saida-auart EQ 'ZREB'.
      gw_saida-dmbe2   = gw_saida-dmbe2 * -1.
      gw_saida-dmbtr   = gw_saida-dmbtr * -1.
    ENDIF.

    gw_saida-buzei = gw_bsid-buzei.

    IF ( gw_saida-budat IS INITIAL ).
      gw_saida-budat = gw_bsid-budat.
      gw_saida-augdt = gw_bsid-budat.
    ENDIF.

    gw_saida-waers = 'USD'.

    IF ( ( gw_saida-auart EQ 'ZFTE' ) OR
         ( gw_saida-auart EQ 'ZSEM' ) OR
         ( gw_saida-auart EQ 'ZDEF' ) ) AND ( p_opcao = 'TR' ).
      IF ( gw_saida-augbl IS INITIAL ) OR
         ( gw_saida-belnr EQ gw_saida-augbl ).
        CONTINUE.
      ENDIF.
    ENDIF.

    IF ( gw_bsid-vbel2 IS NOT INITIAL ) AND ( gw_saida-matnr IS INITIAL OR gw_saida-matkl IS INITIAL ).
      SELECT SINGLE *
        FROM vbap INTO gw_vbap
       WHERE vbeln EQ gw_bsid-vbel2
         AND posnr EQ gw_bsid-posn2.
      IF ( sy-subrc = 0 ) AND ( gw_vbap-matnr IS NOT INITIAL ).
        SELECT SINGLE * INTO gw_mara FROM mara WHERE matnr = gw_vbap-matnr.
        IF sy-subrc = 0.
          gw_saida-matnr = gw_mara-matnr.
          gw_saida-matkl = gw_mara-matkl.
          SELECT SINGLE * INTO gw_makt FROM makt WHERE matnr = gw_mara-matnr.
          gw_saida-maktx = gw_makt-maktx.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND gw_saida TO gt_saida.
  ENDLOOP.

  it_resultado[] = gt_saida[].



ENDFUNCTION.
