FUNCTION z_fi_mercado_interno.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_OPCAO) TYPE  CHAR02
*"     REFERENCE(P_TIPO) TYPE  CHAR02 OPTIONAL
*"  EXPORTING
*"     REFERENCE(IT_RESULTADO) TYPE  STANDARD TABLE
*"  TABLES
*"      IT_EMPRESA STRUCTURE  T001 OPTIONAL
*"      IT_DATA STRUCTURE  BSAD OPTIONAL
*"      IT_CLIENTE STRUCTURE  BSAD OPTIONAL
*"      IT_NR_OV STRUCTURE  BSAD OPTIONAL
*"      IT_NR_SOL STRUCTURE  ZSDT0053 OPTIONAL
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_saida,
           bukrs             TYPE bsad-bukrs,
           gsber             TYPE bsad-gsber,
           kunnr             TYPE bsad-kunnr,
           name1             TYPE kna1-name1,
           tipo              TYPE c LENGTH 13,
           belnr             TYPE bsad-belnr,
           augbl             TYPE bsad-augbl,
           budat             TYPE bsad-budat,
           augdt             TYPE bsad-augdt,
           vbel2             TYPE bsad-vbel2,
           auart             TYPE vbak-auart,
           vbeln             TYPE bsad-vbeln,
           nr_sol            TYPE zsdt0053-nro_sol_ov,
           tp_venda          TYPE zsdt0051-tp_venda,
           dmbtr             TYPE bsad-dmbtr,
           dmbe2             TYPE bsad-dmbe2,
           tx_camb           TYPE zlest0061-tax_dolar,
           banco_liq         TYPE skat-txt50,
           zfbdt             TYPE bsad-zfbdt,
           butxt             TYPE t001-butxt,
           belnr_bx          TYPE bsad-belnr,
           budat_bx          TYPE bsad-budat,
           augdt_bx          TYPE bsad-augdt,
           dmbtr_bx          TYPE bsad-dmbtr,
           dmbe2_bx          TYPE bsad-dmbe2,
           vbel2_bx          TYPE bsad-vbel2,
           vbeln_bx          TYPE bsad-vbeln,
           matnr             TYPE mara-matnr,
           maktx             TYPE makt-maktx,
           buzei             TYPE bsad-buzei,
           charg             TYPE vbap-charg,
           waers             TYPE bsad-waers,
           tpsim             TYPE char2,
           spart             TYPE vbak-spart,
           jr_ds             TYPE c,
           matkl             TYPE mara-matkl,
           ktokd             TYPE kna1-ktokd,
           ajuste_financeiro TYPE zfit0087-ajuste_financeiro, "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
         END OF ty_saida,

         BEGIN OF ty_bsad,
           belnr        TYPE bsad-belnr,
           augbl        TYPE bsad-augbl,
           bukrs        TYPE bsad-bukrs,
           gjahr        TYPE bsad-gjahr,
           vbel2        TYPE bsad-vbel2,
           posn2        TYPE bsad-posn2,
           gsber        TYPE bsad-gsber,
           kunnr        TYPE bsad-kunnr,
           zfbdt        TYPE bsad-zfbdt,
           zbd1t        TYPE bsad-zbd1t,
           budat        TYPE bsad-budat,
           augdt        TYPE bsad-augdt,
           vbeln        TYPE bsad-vbeln,
           dmbtr        TYPE bsad-dmbtr,
           dmbe2        TYPE bsad-dmbe2,
           umsks        TYPE bsad-umsks,
           buzei        TYPE bsad-buzei,
           waers        TYPE bsad-waers,
           prop_banco   TYPE p DECIMALS 10, "PD Ini
           dmbtr_tot    TYPE bsad-dmbtr,    "PD Ini
           ajuste_finan TYPE c, "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
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
        gt_bsid_ajuste_finan TYPE TABLE OF bsid, "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
        gt_bsid_aux          TYPE TABLE OF bsid,
        gt_ska1              TYPE TABLE OF ska1,     "Mestre de contas do Razão (plano de contas)
        gt_skat              TYPE TABLE OF skat,     "Mestre de contas do Razão (plano de contas: denominação)
        gt_zsdt0053          TYPE TABLE OF zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
        gt_zsdt0066          TYPE TABLE OF zsdt0066, "Tabela de Solicitação de Ordem de Venda – Formação de Lote
        gt_zsdt0051          TYPE TABLE OF zsdt0051, "Tabela de Solicitação Ordem de Venda - Cabeçalho
        gt_zsdt0041          TYPE TABLE OF zsdt0041, "Tabela de Solicitação de ordem de venda - MATERIAIS
        gt_zsdt0090          TYPE TABLE OF zsdt0090, "Tabela de Solicitação de Ordem de Venda – Formação de Lote
        gt_zsdt0040          TYPE TABLE OF zsdt0040, "Tabela de Solicitação Ordem de Venda - Cabeçalho
        gt_vbak              TYPE TABLE OF vbak,     "Contabilidade: índice secundário p/fornecedores (part.comp.)
        gt_kna1              TYPE TABLE OF kna1,     "Mestre de clientes (parte geral)
        gt_kna1_ktokd        TYPE TABLE OF kna1 WITH HEADER LINE,     "Mestre de clientes (parte geral)
        gt_t001              TYPE TABLE OF t001,     "Empresas
        gt_mara              TYPE TABLE OF mara,     "Dados gerais de material
        gt_makt              TYPE TABLE OF makt,     "Textos breves de material
        gt_bkpf              TYPE TABLE OF bkpf,
        gt_vbfa              TYPE TABLE OF vbfa,
        gt_total             TYPE TABLE OF ty_total_bsad,
        gt_kna1_tmp          TYPE TABLE OF kna1 WITH HEADER LINE,
        gt_zfit0087_aux      TYPE TABLE OF zfit0087, "O.V's não vinculadas para o processo da ZFI0064
        gt_zfit0087          TYPE TABLE OF zfit0087, "O.V's não vinculadas para o processo da ZFI0064
        gt_saida             TYPE TABLE OF ty_saida, "Estrutura de Saída.
        gt_saida_aux         TYPE TABLE OF ty_saida, "Estrutura de Saída.
        gt_saida_usd         TYPE TABLE OF ty_saida, "Estrutura de Saída.
        gt_saida_jros        TYPE TABLE OF ty_saida, "Estrutura de Saída,
        gt_tvak_aux          TYPE TABLE OF tvak WITH HEADER LINE,
        gt_tvak              TYPE TABLE OF tvak WITH HEADER LINE.

*----------
*Parametros Juros
*----------

  DATA: gt_empresa_jros TYPE TABLE OF t001,
        gt_data_jros    TYPE TABLE OF bsad,
        gt_cliente_jros TYPE TABLE OF bsad,
        gt_vbel2_jros   TYPE TABLE OF bsad,
        gt_nr_sol_jros  TYPE TABLE OF zsdt0053.

*-----------------------------------
* Work Area
*-----------------------------------
  DATA:
    gw_bsad         TYPE ty_bsad,     "Contab.financ.: índice secundári O P/CLIENTES (PARTIDA LIQ.)
    gw_bsad_aux     TYPE ty_bsad,     "Contab.financ.: índice secundário p/clientes (partida liq.)
    gw_bsad_ba      TYPE ty_bsad,     "Contab.financ.: índice secundário p/clientes (partida liq.)
    gw_bsis         TYPE bsis,     "Contabilidade financ.: índice secundário p/contas do Razão
    gw_bsid         TYPE bsid,
    gw_ska1         TYPE ska1,     "Mestre de contas do Razão (plano de contas)
    gw_skat         TYPE skat,     "Mestre de contas do Razão (plano de contas: denominação)
    gw_zsdt0053     TYPE zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
    gw_zsdt0066     TYPE zsdt0066, "Tabela de Solicitação de Ordem de Venda – Formação de Lote
    gw_zsdt0051     TYPE zsdt0051, "Tabela de Solicitação Ordem de Venda - Cabeçalho
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
    gw_saida_aux    TYPE ty_saida,
    gw_saida_usd    TYPE ty_saida,
    gw_saida_jros   TYPE ty_saida.

*-----------------------------------
* TABLES
*-----------------------------------
  DATA: gw_empresa TYPE t001,
        gw_centro  TYPE t001w,
        gw_data    TYPE bsad,
        gw_cliente TYPE bsad,
        gw_nr_ov   TYPE bsad,
        gw_nr_sol  TYPE zsdt0053.

  DATA: var_tabix           TYPE sy-tabix,
        var_saknr           TYPE c LENGTH 10,
        "PD Ini
        v_obj_key           TYPE zib_contabil-obj_key,
        v_seqitem           TYPE zib_contabil-seqitem,
        v_dmbtr_tot         TYPE bsad-dmbtr,
        "PD Fim
        "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP
        lva_proc_retroativo TYPE c,
        lva_dt_aux          TYPE sy-datum,
        lva_dias_busca      TYPE i,
        "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP
        v_aquav_ini         TYPE bsak-augdt,
        v_dmbtr_aux         TYPE bsad-dmbtr,
        v_dmbe2_aux         TYPE bsad-dmbe2.

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

  "FI - MELHORIA DA BAIXA NA ZFI0064 RECEBIMENTO DE JUROS - #175263 - RGA
  DATA: gt_bsis_rec    TYPE TABLE OF bsis,
        gt_hkont_param TYPE TABLE OF zfit0233,
        wa_bsis_rec    LIKE LINE OF gt_bsis_rec.

  "LES - ZFI0064 - Ajuste Rateio Impostos US #148838 - WPP
  DATA: lit_saida_juros_desc TYPE zfis_saida_juros_zfi0064_t,
        lra_bukrs_tmp        TYPE range_c10_t,
        lra_augdt_tmp        TYPE range_c10_t,
        lra_kunnr_tmp        TYPE range_c10_t,
        lra_nr_ov_tmp        TYPE range_c10_t,
        lra_nr_sol_tmp       TYPE range_c10_t,
        lra_auart_tmp        TYPE range_c10_t.
  "LES - ZFI0064 - Ajuste Rateio Impostos US #148838 - WPP - Fim

*-----------------------------------
* VARIAVEIS
*-----------------------------------
  DATA: var_true       TYPE c,
        var_augbl      TYPE bsad-augbl,
        v_dmbtr        TYPE zfit0087-dmbtr,
        v_dmbe2        TYPE zfit0087-dmbe2,
        v_buzei_aux(3) TYPE c,
        vl_estorno     TYPE c,
        vl_tabix       TYPE i.

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
  RANGES r_kunnr_tmp FOR kna1-kunnr.
  RANGES r_bukrs_tmp FOR zfit0087-bukrs.
  "RANGES r_zterm_troca_acerto FOR bsad-zterm. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
  "RANGES r_zterm_ajuste_finan FOR bsad-zterm. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

  CHECK p_tipo IS NOT INITIAL.

  "Criar os Ranges.

  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
  SELECT *
    FROM tvarvc INTO TABLE @DATA(lit_tvarvc_troca_acerto)
   WHERE name LIKE 'ZFI0064_ZTERM_TROCA_ACERTO'.

  SELECT *
    FROM tvarvc INTO TABLE @DATA(lit_tvarvc_ajuste_finan)
   WHERE name LIKE 'ZFI0064_ZTERM_AJUSTE_FINAN'.

  LOOP AT lit_tvarvc_troca_acerto ASSIGNING FIELD-SYMBOL(<fs_tvarvc_troca_acerto>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_tvarvc_troca_acerto>-low ) TO r_zterm_troca_acerto.
  ENDLOOP.

  LOOP AT lit_tvarvc_ajuste_finan ASSIGNING FIELD-SYMBOL(<fs_tvarvc_ajuste_finan>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_tvarvc_ajuste_finan>-low ) TO r_zterm_ajuste_finan.
  ENDLOOP.

  IF r_zterm_troca_acerto[] IS INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'C002') TO r_zterm_troca_acerto.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'C003') TO r_zterm_troca_acerto.
  ENDIF.

  IF r_zterm_ajuste_finan[] IS INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'C006') TO r_zterm_ajuste_finan.
  ENDIF.
  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<---


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
        APPEND gw_empresa TO gt_empresa_jros.
      ENDIF.
    WHEN: '2'.
      it_bukrs-sign   = 'I'.
      it_bukrs-option = 'BT'.
      READ TABLE it_empresa INTO gw_empresa INDEX 1.
      APPEND gw_empresa TO gt_empresa_jros.
      it_bukrs-low    = gw_empresa-bukrs.
      READ TABLE it_empresa INTO gw_empresa INDEX 2.
      APPEND gw_empresa TO gt_empresa_jros.
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
      APPEND gw_data TO gt_data_jros.
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
      APPEND gw_data TO gt_data_jros.
      it_augdt-low    = gw_data-augdt.

      IF ( gw_data-augdt(4) EQ '2013' ) OR ( gw_data-augdt(4) EQ '2014').
        var_data_antiga = 'X'.
      ENDIF.

      READ TABLE it_data INTO gw_data INDEX 2.
      APPEND gw_data TO gt_data_jros.
      it_augdt-high   = gw_data-augdt.
      IF NOT ( it_augdt-low IS INITIAL ) AND NOT ( it_augdt-high IS INITIAL ).
        APPEND it_augdt.
      ENDIF.



  ENDCASE.

  "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP
  CLEAR: lva_proc_retroativo.
  SELECT SINGLE *
   FROM tvarvc INTO @DATA(lwa_tvarc)
  WHERE name EQ 'ZFI0064_DIAS_BUSCA_JOB'.

  IF sy-subrc EQ 0 AND lwa_tvarc-low IS NOT INITIAL.
    lva_dias_busca = lwa_tvarc-low.
  ELSE.
    lva_dias_busca = 30.
  ENDIF.

  lva_dt_aux = sy-datum - lva_dias_busca.

  LOOP AT it_data INTO DATA(gw_data_aux) WHERE augdt < lva_dt_aux.
    lva_proc_retroativo = abap_true.
    EXIT.
  ENDLOOP.
  "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

  CLEAR: var_linhas.
  DESCRIBE TABLE it_cliente LINES var_linhas.


  CASE var_linhas.
    WHEN: '1'.
      CLEAR: gw_cliente.
      READ TABLE it_cliente INTO gw_cliente INDEX 1.
      APPEND gw_cliente TO gt_cliente_jros.
      it_kunnr-sign   = 'I'.
      it_kunnr-option = 'EQ'.
      it_kunnr-low    = gw_cliente-kunnr.
      APPEND it_kunnr.
    WHEN: '2'.

      it_kunnr-sign   = 'I'.
      it_kunnr-option = 'BT'.

      CLEAR: gw_cliente.
      READ TABLE it_cliente INTO gw_cliente INDEX 1.
      APPEND gw_cliente TO gt_cliente_jros.
      it_kunnr-low    = gw_cliente-kunnr.
      READ TABLE it_cliente INTO gw_cliente INDEX 2.
      APPEND gw_cliente TO gt_cliente_jros.
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
        APPEND gw_cliente TO gt_cliente_jros.
      ENDLOOP.


  ENDCASE.


  CLEAR: var_linhas.
  DESCRIBE TABLE it_nr_ov LINES var_linhas.
  CASE var_linhas.
    WHEN: '1'.
      CLEAR: gw_nr_ov.
      READ TABLE it_nr_ov INTO gw_nr_ov INDEX 1.
      APPEND gw_nr_ov TO gt_vbel2_jros.
      it_vbel2-sign   = 'I'.
      it_vbel2-option = 'EQ'.
      it_vbel2-low    = gw_nr_ov-vbel2.
      it_vbel2-high    = gw_nr_ov-vbel2.
      APPEND it_vbel2.
    WHEN: '2'.
      it_vbel2-sign   = 'I'.
      it_vbel2-option = 'BT'.
      READ TABLE it_nr_ov INTO gw_nr_ov INDEX 1.
      APPEND gw_nr_ov TO gt_vbel2_jros.
      it_vbel2-low    = gw_nr_ov-vbel2.
      READ TABLE it_nr_ov INTO gw_nr_ov INDEX 2.
      APPEND gw_nr_ov TO gt_vbel2_jros.
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
      APPEND gw_nr_sol TO gt_nr_sol_jros.
      it_nro_sol-sign   = 'I'.
      it_nro_sol-option = 'EQ'.
      it_nro_sol-low    = gw_nr_sol-nro_sol_ov.
      it_nro_sol-high   = gw_nr_sol-nro_sol_ov.
      APPEND it_nro_sol .
    WHEN: '2'.
      it_nro_sol-sign   = 'I'.
      it_nro_sol-option = 'BT'.
      READ TABLE it_nr_sol INTO gw_nr_sol INDEX 1.
      APPEND gw_nr_sol TO gt_nr_sol_jros.
      it_nro_sol-low = gw_nr_sol-nro_sol_ov.
      READ TABLE it_nr_sol INTO gw_nr_sol INDEX 2.
      APPEND gw_nr_sol TO gt_nr_sol_jros.
      it_nro_sol-high = gw_nr_sol-nro_sol_ov.
      IF NOT ( it_nro_sol-low IS INITIAL ) AND ( it_nro_sol-high IS INITIAL ).
        APPEND it_nro_sol.
      ENDIF.
  ENDCASE.

  "Define Tipo O.V.
  CLEAR: it_auart[].

  PERFORM f_get_auart_tipo TABLES gt_tvak
                            USING p_tipo.
  it_auart-sign   = 'I'.
  it_auart-option = 'EQ'.
  LOOP AT gt_tvak.
    it_auart-low    = gt_tvak-auart.
    APPEND it_auart.
  ENDLOOP.

  CHECK it_auart[] IS NOT INITIAL.

  "Ajustes CS2019001627 - Ini
  CLEAR: t_conta[], it_contas[], gt_bsis_jd[], gt_bsis_rec.
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

  "#175263 - RGA - ini
  SELECT *
    FROM zfit0233
    INTO TABLE gt_hkont_param.
  "#175263 - RGA - fim

  REFRESH: gt_saida, gt_saida_aux, gt_zfit0087.
  CLEAR: gw_saida, gw_saida_aux, gw_zfit0087.

  "O.V's não vinculadas para o processo da ZFI0064
  IF ( var_data_antiga IS NOT INITIAL )
     OR lva_proc_retroativo EQ abap_true. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

    "O.V's não vinculadas para o processo da ZFI0064
    IF ( p_opcao EQ 'VC' ).

      SELECT *
        FROM zfit0087
        INTO TABLE gt_zfit0087
      WHERE bukrs IN it_bukrs
        AND augdt IN it_augdt
        AND kunnr IN it_kunnr
        AND vbel2 IN it_vbel2.

    ELSE.

      SELECT *
        FROM zfit0087
        INTO TABLE gt_zfit0087
      WHERE bukrs       IN it_bukrs
        AND augdt       IN it_augdt
        AND kunnr       IN it_kunnr
        AND vbel2       IN it_vbel2
        AND visao_caixa NE 'X'.

    ENDIF.

    IF ( sy-subrc EQ 0 ).
      "Mestre de clientes (parte geral)
      SELECT * FROM kna1
        INTO TABLE gt_kna1
        FOR ALL ENTRIES IN gt_zfit0087
      WHERE kunnr EQ gt_zfit0087-kunnr.

      "Dados gerais de material
      SELECT * FROM mara
        INTO TABLE gt_mara
        FOR ALL ENTRIES IN gt_zfit0087
      WHERE matnr EQ gt_zfit0087-matnr.

      "Textos breves de material
      IF gt_mara[] IS NOT INITIAL. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP
        SELECT * FROM makt
          INTO TABLE gt_makt
          FOR ALL ENTRIES IN gt_mara
        WHERE matnr EQ gt_mara-matnr
          AND spras EQ sy-langu.
      ENDIF.

      "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP
      SELECT * FROM t001
       APPENDING TABLE gt_t001
       FOR ALL ENTRIES IN gt_zfit0087
     WHERE bukrs EQ gt_zfit0087-bukrs.
      "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP


    ENDIF.

  ELSE.

    "Contab.financ.: índice secundário p/clientes (partida liq.)
    CASE p_opcao.
      WHEN: 'VC'.

        SELECT  belnr augbl bukrs gjahr vbel2 posn2 gsber kunnr zfbdt zbd1t budat augdt vbeln dmbtr dmbe2 umsks buzei waers
          FROM bsad
          INTO TABLE gt_bsad
        WHERE bukrs   IN it_bukrs
            AND augdt IN it_augdt
            AND kunnr IN it_kunnr
            AND vbel2 IN it_vbel2
*            AND UMSKS NE 'A'
            AND umskz NE 'F'
            AND augbl NE space
            AND waers EQ 'BRL'.

        "PD Ini
        "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*        SELECT  belnr augbl bukrs gjahr vbel2 posn2 gsber kunnr zfbdt zbd1t budat augdt vbeln dmbtr dmbe2 umsks buzei waers
*          FROM bsad
*          APPENDING TABLE gt_bsad_pd
*        WHERE bukrs   IN it_bukrs
*            AND augdt IN it_augdt
*            AND kunnr IN it_kunnr
*            AND umskz NE 'F'
*            AND augbl NE space
*            AND waers EQ 'BRL'
*            AND blart EQ 'PD'.
        "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<----
        "PD Fim

        SELECT * FROM bsid INTO TABLE gt_bsid
         WHERE bukrs   IN it_bukrs
             AND budat IN it_augdt
             AND kunnr IN it_kunnr
             AND umsks NE 'W'
             AND waers EQ 'BRL'.


        SELECT  belnr augbl bukrs gjahr vbel2 posn2 gsber kunnr zfbdt zbd1t budat augdt vbeln dmbtr dmbe2 umsks buzei waers
          FROM bsad
          APPENDING TABLE gt_bsad
        WHERE bukrs   IN it_bukrs
            AND budat IN it_augdt
            AND kunnr IN it_kunnr
            AND vbel2 IN it_vbel2
            AND umsks EQ 'A'
            AND umskz NE 'F'
            AND augbl NE space
            AND waers EQ 'BRL'.


*        SELECT * FROM BSIS
*          APPENDING TABLE GT_BSIS
*          FOR ALL ENTRIES IN GT_BSAD
*       WHERE BUKRS EQ GT_BSAD-BUKRS
*         AND BELNR EQ GT_BSAD-BELNR
*         AND BUDAT EQ IT_AUGDT.
*
*        IF ( SY-SUBRC EQ 0 ).
*          LOOP AT GT_BSAD INTO GW_BSAD WHERE UMSKS EQ 'A'.
*            VAR_TABIX = SY-TABIX.
*            READ TABLE GT_BSIS INTO GW_BSIS WITH KEY BELNR = GW_BSAD-BELNR.
*            IF ( SY-SUBRC EQ 0 ).
*              CONTINUE.
*            ELSE.
*               DELETE GT_BSAD INDEX VAR_TABIX.
*            ENDIF.
*            CLEAR: GW_BSAD, GW_BSIS.
*          ENDLOOP.
*        ENDIF.

        SELECT * FROM zfit0087
          INTO TABLE gt_zfit0087
        WHERE bukrs IN it_bukrs
          AND augdt IN it_augdt
          AND kunnr IN it_kunnr
          AND vbel2 IN it_vbel2
          AND visao_caixa EQ 'X'.

      WHEN: 'TR'.

        SELECT  belnr augbl bukrs gjahr vbel2 posn2 gsber kunnr zfbdt zbd1t budat augdt vbeln dmbtr dmbe2 umsks buzei waers
          FROM bsad
          INTO TABLE gt_bsad
        WHERE bukrs IN it_bukrs
          "AND AUGDT IN IT_AUGDT
          AND budat IN it_augdt
          AND kunnr IN it_kunnr
          AND vbel2 IN it_vbel2
          AND zterm IN r_zterm_troca_acerto "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
          AND umsks NE space
          AND waers EQ 'BRL'
          AND augbl NE space
          AND umskz NE 'F'.

        "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

        SELECT belnr augbl bukrs gjahr vbel2 posn2 gsber kunnr zfbdt zbd1t budat augdt vbeln dmbtr dmbe2 umsks buzei waers
          FROM bsad AS a INTO TABLE gt_bsad_ajuste_finan
         WHERE bukrs IN it_bukrs
           AND augdt IN it_augdt
           AND kunnr IN it_kunnr
           AND vbel2 IN it_vbel2
           AND EXISTS ( SELECT docnum
                          FROM zfit0026 AS b
                         WHERE b~bukrs  EQ a~bukrs
                           AND b~docnum EQ a~belnr
                           AND b~zterm  IN r_zterm_ajuste_finan )
          AND waers EQ 'BRL'
          AND augbl NE space
          AND umskz NE 'F'.

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
        "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<---

        DELETE gt_bsad WHERE umsks EQ 'F'.

        "PD Ini
        "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*        SELECT  belnr augbl bukrs gjahr vbel2 posn2 gsber kunnr zfbdt zbd1t budat augdt vbeln dmbtr dmbe2 umsks buzei waers
*          FROM bsad
*          APPENDING TABLE gt_bsad_pd
*        WHERE bukrs IN it_bukrs
*          AND budat IN it_augdt
*          AND kunnr IN it_kunnr
*          AND vbel2 IN it_vbel2
*          AND zterm IN r_zterm_troca_acerto "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*          AND umsks NE space
*          AND waers EQ 'BRL'
*          AND augbl NE space
*          AND umskz NE 'F'
*          AND blart EQ 'PD'.
        "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<----
        "PD Fim

        SELECT * FROM bsid INTO TABLE gt_bsid
         WHERE bukrs   IN it_bukrs
             AND budat IN it_augdt
             AND kunnr IN it_kunnr
             AND umsks NE 'W'
             AND waers EQ 'BRL'
             AND zterm IN r_zterm_troca_acerto "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
             AND blart NE 'RV'.

        SELECT *
          FROM zfit0087
          INTO TABLE gt_zfit0087
        WHERE bukrs       IN it_bukrs
          "AND AUGDT       IN IT_AUGDT
          AND budat       IN it_augdt
          AND kunnr       IN it_kunnr
          AND vbel2       IN it_vbel2
          AND visao_caixa NE 'X'.

        "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
        SELECT *
         FROM zfit0087
         APPENDING TABLE gt_zfit0087
       WHERE bukrs       IN it_bukrs
         AND augdt       IN it_augdt
         AND kunnr       IN it_kunnr
         AND vbel2       IN it_vbel2
         AND visao_caixa NE 'X'.

        SORT gt_zfit0087 BY belnr buzei bukrs estorno.
        DELETE ADJACENT DUPLICATES FROM gt_zfit0087 COMPARING belnr buzei bukrs estorno.
        "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<----

    ENDCASE.

    IF NOT ( gt_bsad[] IS INITIAL ).


      SELECT * FROM bkpf
        INTO TABLE gt_bkpf
        FOR ALL ENTRIES IN gt_bsad
     WHERE belnr EQ gt_bsad-augbl
       AND gjahr EQ gt_bsad-gjahr
       AND bukrs EQ gt_bsad-bukrs
       AND waers EQ 'BRL'.

      SELECT * FROM bkpf
        APPENDING TABLE gt_bkpf
        FOR ALL ENTRIES IN gt_bsad
     WHERE belnr EQ gt_bsad-belnr
       AND gjahr EQ gt_bsad-gjahr
       AND bukrs EQ gt_bsad-bukrs
       AND waers EQ 'BRL'.

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

          CLEAR: v_dmbtr_tot.
          LOOP AT gt_bsad_pd INTO DATA(_wl_pd_aux) WHERE bukrs = wl_bsad_pd-bukrs
                                                     AND gjahr = wl_bsad_pd-gjahr
                                                     AND belnr = wl_bsad_pd-belnr.
            ADD _wl_pd_aux-dmbtr TO v_dmbtr_tot.
          ENDLOOP.

          CHECK v_dmbtr_tot > 0.

          wl_bsad_pd-prop_banco = wl_bsad_pd-dmbtr / v_dmbtr_tot.
          wl_bsad_pd-dmbtr_tot  = v_dmbtr_tot.

          CLEAR: gw_bsad.

          DELETE gt_bsad WHERE bukrs = wl_bsad_pd-bukrs
                           AND belnr = wl_bsad_pd-belnr
                           AND buzei = wl_bsad_pd-buzei.

          MOVE-CORRESPONDING wl_bsad_pd TO gw_bsad.
          APPEND gw_bsad TO gt_bsad.
        ENDLOOP.
      ENDIF.
      "PD Fim.

      "Mestre de clientes (parte geral)
      SELECT * FROM kna1
        INTO TABLE gt_kna1
        FOR ALL ENTRIES IN gt_bsad
      WHERE kunnr EQ gt_bsad-kunnr.

      "Empresa
      SELECT * FROM t001
        INTO TABLE gt_t001
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
                                                 gjahr = gw_bsad-gjahr
                                                 bukrs = gw_bsad-bukrs.
        IF ( gw_bkpf-tcode EQ 'FB08' ).
          DELETE gt_bsad INDEX var_tabix.
          CONTINUE.
        ENDIF.

*    "Aproveitando esse mesmo loop para fazer os totais da BSAD e evitar que seja feito loops/collect.
*    READ TABLE GT_TOTAL INTO GW_TOTAL WITH KEY AUGBL = GW_BSAD-AUGBL.
*    IF ( SY-SUBRC NE 0 ). "Caso não encontre o registro adicionar ele na tabela de totais com o valor inicial.
*      GW_TOTAL-AUGBL = GW_BSAD-AUGBL.
*      GW_TOTAL-TOTAL = GW_BSAD-DMBTR.
*      APPEND GW_TOTAL TO GT_TOTAL.
*    ELSE. "Caso encontre somar o valor com o que já existe.
*      GW_TOTAL-TOTAL = GW_TOTAL-TOTAL + GW_BSAD-DMBTR.
*      MODIFY GT_TOTAL FROM GW_TOTAL TRANSPORTING TOTAL WHERE AUGBL = GW_BSAD-AUGBL.
*    ENDIF.

        CLEAR: gw_bsad, gw_bsad_aux, gw_total, gw_bkpf.
      ENDLOOP.

      IF NOT ( gt_bsad[] IS INITIAL ).

        IF ( p_opcao EQ 'VC' ).
          "Contabilidade financ.: índice secundário p/contas do Razão
          SELECT * FROM bsis
            INTO TABLE gt_bsis
            FOR ALL ENTRIES IN gt_bsad
          WHERE bukrs EQ gt_bsad-bukrs
            AND belnr EQ gt_bsad-augbl
            AND gjahr EQ gt_bsad-gjahr.

          SELECT * FROM bsis
            APPENDING TABLE gt_bsis
            FOR ALL ENTRIES IN gt_bsad
          WHERE bukrs EQ gt_bsad-bukrs
            AND belnr EQ gt_bsad-belnr
            AND gjahr EQ gt_bsad-gjahr.

          SELECT * FROM bsis
            APPENDING TABLE gt_bsis
            FOR ALL ENTRIES IN gt_bsad
          WHERE bukrs EQ gt_bsad-bukrs
            AND belnr EQ gt_bsad-augbl
            AND gjahr EQ gt_bsad-augdt(4).

          IF gt_bsis[] IS NOT INITIAL.

            "Mestre de contas do Razão (plano de contas)
            SELECT * FROM ska1         "#EC CI_DB_OPERATION_OK[2389136]
              INTO TABLE gt_ska1       "#EC CI_DB_OPERATION_OK[2431747]
              FOR ALL ENTRIES IN gt_bsis
            WHERE saknr EQ gt_bsis-hkont
              AND ktopl EQ '0050'
              AND ktoks EQ 'YB04'.

            SELECT * FROM skat
              INTO TABLE gt_skat
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

              ""#175263 - RGA - ini
              READ TABLE gt_hkont_param WITH KEY hkont = gw_bsis-hkont TRANSPORTING NO FIELDS.
              IF sy-subrc EQ 0.
                APPEND gw_bsis TO gt_bsis_rec.
              ENDIF.
              "#175263 - RGA - fim

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


        "Tabela de Solicitação de ordem de venda - MATERIAIS
        SELECT * FROM zsdt0053
          INTO TABLE gt_zsdt0053
          FOR ALL ENTRIES IN gt_bsad
        WHERE vbeln      EQ gt_bsad-vbel2.
        "AND nro_sol_ov   IN it_nro_sol. ""FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

        IF gt_zsdt0053[] IS NOT INITIAL.
          "Tabela de Solicitação Ordem de Venda - Cabeçalho
          SELECT * FROM zsdt0051
            INTO TABLE gt_zsdt0051
            FOR ALL ENTRIES IN gt_zsdt0053
          WHERE nro_sol_ov EQ gt_zsdt0053-nro_sol_ov.

          "Dados gerais de material
          SELECT * FROM mara
            INTO TABLE gt_mara
            FOR ALL ENTRIES IN gt_zsdt0053
          WHERE matnr EQ gt_zsdt0053-matnr.
        ENDIF.

        "Tabela de Solicitação de Ordem de Venda – Formação de Lote
        SELECT * FROM zsdt0066
          INTO TABLE gt_zsdt0066
          FOR ALL ENTRIES IN gt_bsad
        WHERE vbeln EQ gt_bsad-vbel2.
        "AND nro_sol_ov IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

        IF ( gt_zsdt0066[] IS NOT INITIAL ).
          "Dados gerais de material
          SELECT * FROM mara
            APPENDING TABLE gt_mara
            FOR ALL ENTRIES IN gt_zsdt0066
          WHERE matnr EQ gt_zsdt0066-matnr.
        ENDIF.

        "Simulador Vendas --------------------------------------

*       Tabela de Solicitação de ordem de venda - MATERIAIS
        SELECT *
          FROM zsdt0041 APPENDING TABLE gt_zsdt0041
           FOR ALL ENTRIES IN gt_bsad
         WHERE vbeln         EQ gt_bsad-vbel2.
        "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

        IF gt_zsdt0041[] IS NOT INITIAL.
*          Tabela de Solicitação Ordem de Venda - Cabeçalho
          SELECT *
            FROM zsdt0040 APPENDING TABLE gt_zsdt0040
             FOR ALL ENTRIES IN gt_zsdt0041
           WHERE doc_simulacao EQ gt_zsdt0041-doc_simulacao.

*          Dados gerais de material
          SELECT *
            FROM mara APPENDING TABLE gt_mara
             FOR ALL ENTRIES IN gt_zsdt0041
           WHERE matnr EQ gt_zsdt0041-matnr.
        ENDIF.

*        Tabela de Solicitação de Ordem de Venda – Formação de Lote
        SELECT *
          FROM zsdt0090 APPENDING TABLE gt_zsdt0090
           FOR ALL ENTRIES IN gt_bsad
         WHERE vbelv         EQ gt_bsad-vbel2.
        "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

        SELECT *
          FROM zsdt0090 APPENDING TABLE gt_zsdt0090
           FOR ALL ENTRIES IN gt_bsad
         WHERE vbeln         EQ gt_bsad-vbel2.
        "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

        IF ( gt_zsdt0090[] IS NOT INITIAL ).
*          Dados gerais de material
          SELECT *
            FROM mara APPENDING TABLE gt_mara
             FOR ALL ENTRIES IN gt_zsdt0090
           WHERE matnr EQ gt_zsdt0090-matnrv.

*         Tabela de Solicitação Ordem de Venda - Cabeçalho
          SELECT *
            FROM zsdt0040 APPENDING TABLE gt_zsdt0040
             FOR ALL ENTRIES IN gt_zsdt0090
           WHERE doc_simulacao EQ gt_zsdt0090-doc_simulacao.
        ENDIF.
        "Fim Simulador


        IF ( gt_mara[] IS NOT INITIAL ).
          "Descrição do Material
          SELECT * FROM makt
            INTO TABLE gt_makt
            FOR ALL ENTRIES IN gt_mara
          WHERE spras EQ sy-langu
            AND matnr EQ gt_mara-matnr.
        ENDIF.

        "Documento de vendas: dados de cabeçalho
        SELECT * FROM vbak
          INTO TABLE gt_vbak
          FOR ALL ENTRIES IN gt_bsad
        WHERE vbeln EQ gt_bsad-vbel2.
*          AND AUART IN ('ZCOP','ZCPV','ZMIT','ZREB','ZFTE','ZDEF','ZSEM','ZOFE','ZODF','ZOSM','ZREM','ZTRI', 'ZFEX').
        "16.01.2017
        "AND AUART IN ('ZCOP','ZCPV','ZMIT','ZREB','ZOFE','ZODF', 'ZREM','ZTRI', 'ZFEX').
        "AND AUART IN ('ZCOP','ZCPV','ZMIT','ZREB','ZOFE','ZODF', 'ZREM','ZTRI', 'ZFEX',
        "              'ZDEF','ZSEM','ZFTE','ZOSM','ZFUT').
        "AND auart IN it_auart.  "LES - ZFI0064 - Ajuste Rateio Impostos US #148838 - WPP
        "Fim

        SELECT * FROM vbfa
          INTO TABLE gt_vbfa
          FOR ALL ENTRIES IN gt_bsad
       WHERE vbeln   EQ gt_bsad-vbel2
         AND vbtyp_n IN ('C','H','L')
         AND vbtyp_v EQ 'C'.


        IF ( gt_vbfa[] IS NOT INITIAL ).
          "Tabela de Solicitação de ordem de venda - MATERIAIS
          SELECT * FROM zsdt0053
            APPENDING TABLE gt_zsdt0053
            FOR ALL ENTRIES IN gt_vbfa
          WHERE vbeln      EQ gt_vbfa-vbelv.
          "AND nro_sol_ov IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

          SELECT *
            FROM zsdt0041 APPENDING TABLE gt_zsdt0041
             FOR ALL ENTRIES IN gt_vbfa
           WHERE vbeln         EQ gt_vbfa-vbelv.
          "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
        ENDIF.

      ENDIF. "IF NOT ( GT_BSAD[] IS INITIAL ).

    ENDIF. "IF NOT ( GT_BSAD[] IS INITIAL ).

    IF NOT ( gt_bsid[] IS INITIAL ).

      SELECT * FROM bkpf
              APPENDING TABLE gt_bkpf
             FOR ALL ENTRIES IN gt_bsid
          WHERE belnr EQ gt_bsid-augbl
            AND gjahr EQ gt_bsid-gjahr
            AND bukrs EQ gt_bsid-bukrs
            AND waers EQ 'BRL'
            AND bstat NE 'S'.

      SELECT * FROM bkpf
             APPENDING TABLE gt_bkpf
            FOR ALL ENTRIES IN gt_bsid
         WHERE belnr EQ gt_bsid-belnr
           AND gjahr EQ gt_bsid-gjahr
           AND bukrs EQ gt_bsid-bukrs
           AND waers EQ 'BRL'
           AND bstat NE 'S'.

      "Mestre de clientes (parte geral)
      SELECT * FROM kna1
        APPENDING TABLE gt_kna1
        FOR ALL ENTRIES IN gt_bsid
      WHERE kunnr EQ gt_bsid-kunnr.

      SELECT * FROM t001
        INTO TABLE gt_t001
        FOR ALL ENTRIES IN gt_bsid
      WHERE bukrs EQ gt_bsid-bukrs.

      IF ( p_opcao EQ 'VC' ).
        "Contabilidade financ.: índice secundário p/contas do Razão
        SELECT * FROM bsis
          APPENDING TABLE gt_bsis
          FOR ALL ENTRIES IN gt_bsid
        WHERE bukrs EQ gt_bsid-bukrs
          AND belnr EQ gt_bsid-augbl
          AND gjahr EQ gt_bsid-gjahr.

        SELECT * FROM bsis
          APPENDING TABLE gt_bsis
          FOR ALL ENTRIES IN gt_bsid
        WHERE bukrs EQ gt_bsid-bukrs
          AND belnr EQ gt_bsid-belnr
          AND gjahr EQ gt_bsid-gjahr.

        SELECT * FROM bsis
          APPENDING TABLE gt_bsis
          FOR ALL ENTRIES IN gt_bsid
        WHERE bukrs EQ gt_bsid-bukrs
          AND belnr EQ gt_bsid-augbl
          AND gjahr EQ gt_bsid-augdt(4).

        IF gt_bsis[] IS NOT INITIAL.

          "Mestre de contas do Razão (plano de contas)
          SELECT * FROM ska1           "#EC CI_DB_OPERATION_OK[2431747]
             APPENDING TABLE gt_ska1   "#EC CI_DB_OPERATION_OK[2389136]
            FOR ALL ENTRIES IN gt_bsis
          WHERE saknr EQ gt_bsis-hkont
            AND ktopl EQ '0050'
            AND ktoks EQ 'YB04'.

          SELECT * FROM skat
            APPENDING TABLE gt_skat
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

            ""#175263 - RGA - ini
            READ TABLE gt_hkont_param WITH KEY hkont = gw_bsis-hkont TRANSPORTING NO FIELDS.
            IF sy-subrc EQ 0.
              APPEND gw_bsis TO gt_bsis_rec.
            ENDIF.
            "#175263 - RGA - fim

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


      "Tabela de Solicitação de ordem de venda - MATERIAIS
      SELECT * FROM zsdt0053
         APPENDING TABLE gt_zsdt0053
        FOR ALL ENTRIES IN gt_bsid
      WHERE vbeln      EQ gt_bsid-vbel2.
      "AND nro_sol_ov   IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

      IF gt_zsdt0053[] IS NOT INITIAL.
        "Tabela de Solicitação Ordem de Venda - Cabeçalho
        SELECT * FROM zsdt0051
          APPENDING TABLE gt_zsdt0051
          FOR ALL ENTRIES IN gt_zsdt0053
        WHERE nro_sol_ov EQ gt_zsdt0053-nro_sol_ov.

        "Dados gerais de material
        SELECT * FROM mara
          APPENDING TABLE gt_mara
          FOR ALL ENTRIES IN gt_zsdt0053
        WHERE matnr EQ gt_zsdt0053-matnr.
      ENDIF.

      "Tabela de Solicitação de Ordem de Venda – Formação de Lote
      SELECT * FROM zsdt0066
        APPENDING TABLE gt_zsdt0066
        FOR ALL ENTRIES IN gt_bsid
      WHERE vbeln EQ gt_bsid-vbel2.
      "AND nro_sol_ov IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

      IF ( gt_zsdt0066[] IS NOT INITIAL ).
        "Dados gerais de material
        SELECT * FROM mara
          APPENDING TABLE gt_mara
          FOR ALL ENTRIES IN gt_zsdt0066
        WHERE matnr EQ gt_zsdt0066-matnr.
      ENDIF.

      "Simulador de Vendas
*     Tabela de Solicitação de ordem de venda - MATERIAIS
      SELECT *
        FROM zsdt0041 APPENDING TABLE gt_zsdt0041
         FOR ALL ENTRIES IN gt_bsid
       WHERE vbeln         EQ gt_bsid-vbel2.
      "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

      IF gt_zsdt0041[] IS NOT INITIAL.
*        Tabela de Solicitação Ordem de Venda - Cabeçalho
        SELECT *
          FROM zsdt0040 APPENDING TABLE gt_zsdt0040
           FOR ALL ENTRIES IN gt_zsdt0041
        WHERE doc_simulacao EQ gt_zsdt0041-doc_simulacao.

*        Dados gerais de material
        SELECT * FROM mara
          APPENDING TABLE gt_mara
          FOR ALL ENTRIES IN gt_zsdt0041
        WHERE matnr EQ gt_zsdt0041-matnr.
      ENDIF.

*      Tabela de Solicitação de Ordem de Venda – Formação de Lote
      SELECT *
        FROM zsdt0090 APPENDING TABLE gt_zsdt0090
         FOR ALL ENTRIES IN gt_bsid
       WHERE vbelv         EQ gt_bsid-vbel2.
      "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

      SELECT *
        FROM zsdt0090 APPENDING TABLE gt_zsdt0090
         FOR ALL ENTRIES IN gt_bsid
       WHERE vbeln         EQ gt_bsid-vbel2.
      "AND doc_simulacao IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

      IF ( gt_zsdt0090[] IS NOT INITIAL ).
*        Dados gerais de material
        SELECT * FROM mara
          APPENDING TABLE gt_mara
          FOR ALL ENTRIES IN gt_zsdt0090
        WHERE matnr EQ gt_zsdt0090-matnrv.

*       Tabela de Solicitação Ordem de Venda - Cabeçalho
        SELECT *
          FROM zsdt0040 APPENDING TABLE gt_zsdt0040
           FOR ALL ENTRIES IN gt_zsdt0090
         WHERE doc_simulacao EQ gt_zsdt0090-doc_simulacao.
      ENDIF.
      "Fim Simulador de Vendas

      IF gt_mara[] IS NOT INITIAL.
        "Descrição do Material
        SELECT * FROM makt
          APPENDING TABLE gt_makt
          FOR ALL ENTRIES IN gt_mara
        WHERE spras EQ sy-langu
          AND matnr EQ gt_mara-matnr.
      ENDIF.

      "Documento de vendas: dados de cabeçalho
      SELECT * FROM vbak
        APPENDING TABLE gt_vbak
        FOR ALL ENTRIES IN gt_bsid
      WHERE vbeln EQ gt_bsid-vbel2.
*        AND AUART IN ('ZCOP','ZCPV','ZMIT','ZREB','ZFTE','ZDEF','ZSEM','ZOFE','ZODF','ZOSM','ZREM','ZTRI', 'ZFEX').
      "16.01.2017
      "AND AUART IN ('ZCOP','ZCPV','ZMIT','ZREB','ZOFE','ZODF', 'ZREM','ZTRI', 'ZFEX')
      "AND AUART IN ('ZCOP','ZCPV','ZMIT','ZREB','ZOFE','ZODF', 'ZREM','ZTRI', 'ZFEX',
      "              'ZDEF','ZSEM','ZFTE','ZOSM','ZFUT').
      "AND auart IN it_auart. "LES - ZFI0064 - Ajuste Rateio Impostos US #148838 - WPP
      "Fim

      SELECT * FROM vbfa
        APPENDING TABLE gt_vbfa
        FOR ALL ENTRIES IN gt_bsid
     WHERE vbeln   EQ gt_bsid-vbel2
       AND vbtyp_n IN ('C','H','L')
       AND vbtyp_v EQ 'C'.


      IF ( gt_vbfa[] IS NOT INITIAL ).
        "Tabela de Solicitação de ordem de venda - MATERIAIS
        SELECT * FROM zsdt0053
          APPENDING TABLE gt_zsdt0053
          FOR ALL ENTRIES IN gt_vbfa
        WHERE vbeln      EQ gt_vbfa-vbelv.
        " AND nro_sol_ov IN it_nro_sol. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

        SELECT *
        FROM zsdt0041 APPENDING TABLE gt_zsdt0041
         FOR ALL ENTRIES IN gt_vbfa
       WHERE vbeln         EQ gt_vbfa-vbelv. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
        "AND doc_simulacao IN it_nro_sol.

      ENDIF.
    ENDIF.


    IF NOT ( gt_zfit0087[] IS INITIAL ).

      "Mestre de clientes (parte geral)
      SELECT * FROM kna1
        APPENDING TABLE gt_kna1
        FOR ALL ENTRIES IN gt_zfit0087
      WHERE kunnr EQ gt_zfit0087-kunnr.

      "Dados gerais de material
      SELECT * FROM mara
        APPENDING TABLE gt_mara
        FOR ALL ENTRIES IN gt_zfit0087
      WHERE matnr EQ gt_zfit0087-matnr.

      IF gt_mara[] IS NOT INITIAL.
        "Textos breves de material
        SELECT * FROM makt
          APPENDING TABLE gt_makt
          FOR ALL ENTRIES IN gt_mara
        WHERE spras EQ sy-langu
          AND matnr EQ gt_mara-matnr.
      ENDIF.

      SELECT * FROM t001
        APPENDING TABLE gt_t001
        FOR ALL ENTRIES IN gt_zfit0087
      WHERE bukrs EQ gt_zfit0087-bukrs.

    ENDIF.


  ENDIF.

  SORT: gt_bsad BY belnr buzei.

  DELETE ADJACENT DUPLICATES FROM gt_bsad COMPARING belnr buzei.

  "Ajustes CS2019001627 - Ini
  SORT gt_bsis_jd BY bukrs belnr gjahr buzei.
  DELETE ADJACENT DUPLICATES FROM gt_bsis_jd COMPARING bukrs belnr gjahr buzei.
  "Ajustes CS2019001627 - fIM


  "Saída
  LOOP AT  gt_bsad INTO gw_bsad.

    CLEAR: gw_saida, gw_mara, gw_makt, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.

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
*           AND dmbtr = gw_bsad-dmbtr.
*
*        "PD Ini
*        IF sy-subrc NE 0.
*          CLEAR: gw_bsak.
*          SELECT SINGLE *
*            FROM bsak INTO gw_bsak
*           WHERE belnr = gw_bsad-belnr
*             AND gjahr = gw_bsad-gjahr
*             AND bukrs = gw_bsad-bukrs
*             AND dmbtr = gw_bsad-dmbtr_tot.
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
*             AND dmbtr = gw_bsad-dmbtr.
*          IF sy-subrc = 0.
*            gw_bsad-buzei = gw_bsad_aux-buzei.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
    "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<----


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
              CLEAR: gw_saida, gw_bsad, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
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
            "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<---

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
*                CLEAR: gw_saida, gw_bsad, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
*                CONTINUE.
*              ENDIF.
*
*            ELSE.
            "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
            CLEAR: gw_saida, gw_bsad, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
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
              CLEAR: gw_saida, gw_bsad, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
              CONTINUE.
            ENDIF.

          ENDIF.

          READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.
          IF ( sy-subrc NE 0 ).
            CONTINUE.
          ENDIF.

          READ TABLE gt_skat INTO gw_skat WITH KEY saknr = gw_ska1-saknr.
          IF ( sy-subrc NE 0 ).
            CLEAR: gw_saida, gw_bsad, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
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

    IF p_opcao EQ 'TR' AND gw_bsad-ajuste_finan IS INITIAL. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
      gw_saida-augdt = gw_saida-budat.
    ENDIF.

    gw_saida-bukrs = gw_bsad-bukrs.
    gw_saida-waers = gw_bsad-waers.

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

          "Gambeta, e não me pergunte porque.
          IF ( gw_saida-augdt EQ '20150910').
            gw_saida-dmbtr   = gw_bsad-dmbtr.
            gw_saida-dmbe2   = gw_saida-dmbtr / cs_tax_fix.
          ELSE.
            gw_saida-dmbtr   = gw_bsad-dmbtr.
            CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
              gw_saida-dmbe2   = gw_saida-dmbtr / gw_saida-tx_camb.
            ENDCATCH.

            "PERFORM GET_VLR_DOLAR USING '01' P_OPCAO
            "                   CHANGING GW_SAIDA.
          ENDIF.

        ELSE.

          var_augbl = gw_bsad-augbl.

          CASE gw_bsad-umsks.
            WHEN: 'A'.

              "Gambeta, e não me pergunte porque.
              IF ( gw_saida-augdt EQ '20150910').
                gw_saida-dmbtr = gw_bsad-dmbtr.
                gw_saida-dmbe2 = gw_saida-dmbtr / cs_tax_fix.
              ELSE.
                gw_saida-dmbtr = gw_bsad-dmbtr.

                CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
                  gw_saida-dmbe2 = gw_saida-dmbtr / gw_saida-tx_camb.
                ENDCATCH.

                "PERFORM GET_VLR_DOLAR USING '01' P_OPCAO
                "                   CHANGING GW_SAIDA.
              ENDIF.

            WHEN OTHERS.
              "Gambeta, e não me pergunte porque.
              IF ( gw_saida-augdt EQ '20150910').
                gw_saida-dmbtr = gw_bsad-dmbtr.
                gw_saida-dmbe2 = gw_saida-dmbtr / cs_tax_fix.
              ELSE.
                gw_saida-dmbtr = gw_bsad-dmbtr.

                CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
                  gw_saida-dmbe2 = gw_saida-dmbtr / gw_saida-tx_camb.
                ENDCATCH.

                "PERFORM GET_VLR_DOLAR USING '01' P_OPCAO
                "                   CHANGING GW_SAIDA.
              ENDIF.
          ENDCASE.
        ENDIF.




        "RG / #175263 - 15.05.2025 - Início
        DELETE ADJACENT DUPLICATES FROM gt_bsis_rec.

        PERFORM recebimento_juros TABLES gt_bsis_rec
                                         gt_hkont_param
                                  USING gw_saida-bukrs
                                        gw_saida-augbl
                                  CHANGING gw_saida-dmbtr
                                           gw_saida-dmbe2.
        "RG / #175263 - 15.05.2025 - Início

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

        "Gambeta, e não me pergunte porque.
        IF ( gw_saida-augdt EQ '20150910').
          gw_saida-dmbtr   = gw_bsad-dmbtr.
          gw_saida-dmbe2   = gw_saida-dmbtr / cs_tax_fix.
        ELSE.
          gw_saida-dmbtr   = gw_bsad-dmbtr.
          gw_saida-dmbe2   = gw_bsad-dmbe2.
          "PERFORM GET_VLR_DOLAR USING '01' P_OPCAO
          "                   CHANGING GW_SAIDA.
        ENDIF.

        CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
          gw_saida-tx_camb = gw_saida-dmbtr / gw_saida-dmbe2.
        ENDCATCH.

    ENDCASE.


    "Gambeta, e não me pergunte porque.
    IF ( gw_saida-augdt EQ '20150910').
      gw_saida-tx_camb = cs_tax_fix.
    ENDIF.



    READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY vbeln = gw_bsad-vbel2.
    IF ( sy-subrc NE 0 ).
      READ TABLE gt_zsdt0066 INTO gw_zsdt0066 WITH KEY vbeln = gw_bsad-vbel2.
      IF ( sy-subrc NE 0 ).

        READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsad-vbel2.

        IF ( gw_vbak-auart IS INITIAL ).
          CLEAR: gw_saida, gw_bsad, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
          CONTINUE.
        ELSE.
          gw_saida-auart = gw_vbak-auart.
        ENDIF.

        READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
        gw_saida-tp_venda = gw_zsdt0051-tp_venda.

        CASE gw_vbak-auart.
          WHEN: 'ZCPV' OR 'ZREB' OR 'ZCOP'.
            READ TABLE gt_vbfa INTO gw_vbfa WITH KEY vbeln = gw_bsad-vbel2.
            READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY vbeln = gw_vbfa-vbelv.
            gw_saida-nr_sol = gw_zsdt0053-nro_sol_ov.
            READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
            gw_saida-tp_venda = gw_zsdt0051-tp_venda.

            READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0053-matnr.
            gw_saida-matnr = gw_mara-matnr.
            gw_saida-matkl = gw_mara-matkl.
            READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
            gw_saida-maktx = gw_makt-maktx.
        ENDCASE.

      ENDIF.

      READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsad-vbel2.

      IF ( gw_vbak-auart IS INITIAL ).
        CLEAR: gw_saida, gw_bsad, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
        CONTINUE.
      ELSE.
        gw_saida-auart = gw_vbak-auart.
      ENDIF.

      READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
      gw_saida-tp_venda = gw_zsdt0051-tp_venda.

      READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0066-matnr.
      gw_saida-matnr = gw_mara-matnr.
      gw_saida-matkl = gw_mara-matkl.
      READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
      gw_saida-maktx = gw_makt-maktx.

    ELSE.

      READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsad-vbel2.

      IF ( gw_vbak-auart IS INITIAL ).
        CLEAR: gw_saida, gw_bsad, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
        CONTINUE.
      ENDIF.

      READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
      gw_saida-tp_venda = gw_zsdt0051-tp_venda.
      gw_saida-auart    = gw_zsdt0051-auart.



      READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0053-matnr.
      gw_saida-matnr = gw_mara-matnr.
      gw_saida-matkl = gw_mara-matkl.
      READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
      gw_saida-maktx = gw_makt-maktx.

      gw_saida-nr_sol = gw_zsdt0053-nro_sol_ov.
    ENDIF.

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

    "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*    IF ( gw_bkpf-blart EQ 'PD' ) AND
*       ( p_tipo        EQ 'IN' ) .
*      gw_saida-dmbtr = gw_bsis-dmbtr.
*      CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
*        gw_saida-dmbe2 = gw_saida-dmbtr / gw_saida-tx_camb.
*      ENDCATCH.
*    ENDIF.

*    IF ( gw_saida-auart EQ 'ZREB' ) OR ( gw_bkpf-blart EQ 'PD' ).
*      gw_saida-dmbtr   = gw_saida-dmbtr * -1.
*      gw_saida-dmbe2   = gw_saida-dmbe2 * -1.
*    ENDIF.
    "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<---

    gw_saida-buzei = gw_bsad-buzei.

    IF ( gw_bsad-vbel2 IS NOT INITIAL ) AND ( gw_saida-matnr IS INITIAL ).
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
    CLEAR: gw_saida, gw_bsad, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
  ENDLOOP.


  "=============================== BSID =========================================================================
  "Saída
  LOOP AT  gt_bsid INTO gw_bsid.

    CLEAR: gw_saida, gw_mara, gw_makt, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.


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

*            CLEAR: GW_BSID_AUX.
*            READ TABLE GT_BSID INTO GW_BSID_AUX WITH KEY AUGBL = GW_BSID-BELNR.
*            IF ( SY-SUBRC EQ 0 ).
*              CLEAR: GW_SAIDA, GW_BSID, GW_KNA1, GW_BSIS, GW_SKA1, GW_SKAT, GW_ZSDT0053, GW_ZSDT0051, GW_ZSDT0066, GW_VBAK, GW_TOTAL, GW_VBFA, GW_BSIS.
*              DELETE GT_BSID INDEX VAR_TABIX.
*              CONTINUE.
*            ENDIF.
*
*            GW_SAIDA-BUDAT = GW_BSID-BUDAT.
*            GW_SAIDA-AUGDT = GW_BSID-BUDAT.
*            IF ( GW_SAIDA-AUGDT NOT IN IT_AUGDT ).
*              CONTINUE.
*            ENDIF.
          ENDIF.

          READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.
          IF ( sy-subrc NE 0 ).
            CLEAR: gw_saida, gw_bsid, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
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
              CLEAR: gw_saida, gw_bsid, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
              CONTINUE.
            ENDIF.

          ENDIF.

          READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.
          IF ( sy-subrc NE 0 ).
            CONTINUE.
          ENDIF.

          READ TABLE gt_skat INTO gw_skat WITH KEY saknr = gw_ska1-saknr.
          IF ( sy-subrc NE 0 ).
            CLEAR: gw_saida, gw_bsid, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
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
    gw_saida-waers = gw_bsid-waers.

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
          "Gambeta, e não me pergunte porque.
          IF ( gw_saida-augdt EQ '20150910').
            gw_saida-dmbtr   = gw_bsid-dmbtr.
            gw_saida-dmbe2   = gw_saida-dmbtr / cs_tax_fix.
          ELSE.
            gw_saida-dmbtr   = gw_bsid-dmbtr.
            CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
              gw_saida-dmbe2   = gw_saida-dmbtr / gw_saida-tx_camb.
            ENDCATCH.

            "PERFORM GET_VLR_DOLAR USING '02' P_OPCAO
            "                   CHANGING GW_SAIDA.
          ENDIF.


        ELSE.

          var_augbl = gw_bsid-augbl.

          CASE gw_bsid-umsks.
            WHEN: 'A'.

              "Gambeta, e não me pergunte porque.
              IF ( gw_saida-augdt EQ '20150910').
                gw_saida-dmbtr = gw_bsid-dmbtr.
                gw_saida-dmbe2 = gw_saida-dmbtr / cs_tax_fix.
              ELSE.
                gw_saida-dmbtr = gw_bsid-dmbtr.
                CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
                  gw_saida-dmbe2 = gw_saida-dmbtr / gw_saida-tx_camb.
                ENDCATCH.
                "PERFORM GET_VLR_DOLAR USING '02' P_OPCAO
                "                   CHANGING GW_SAIDA.
              ENDIF.

            WHEN OTHERS.
              "Gambeta, e não me pergunte porque.
              IF ( gw_saida-augdt EQ '20150910').
                gw_saida-dmbtr = gw_bsid-dmbtr.
                gw_saida-dmbe2 = gw_saida-dmbtr / cs_tax_fix.

              ELSE.
                gw_saida-dmbtr = gw_bsid-dmbtr.
                CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
                  gw_saida-dmbe2 = gw_saida-dmbtr / gw_saida-tx_camb.
                ENDCATCH.
                "PERFORM GET_VLR_DOLAR USING '02' P_OPCAO
                "                   CHANGING GW_SAIDA.
              ENDIF.
          ENDCASE.



        ENDIF.

      WHEN: 'TR'.

        "Gambeta, e não me pergunte porque.
        IF ( gw_saida-augdt EQ '20150910').
          gw_saida-dmbtr   = gw_bsid-dmbtr.
          gw_saida-dmbe2   = gw_bsid-dmbtr / cs_tax_fix.

        ELSE.
          gw_saida-dmbtr   = gw_bsid-dmbtr.
          gw_saida-dmbe2   = gw_bsid-dmbe2.
          "PERFORM GET_VLR_DOLAR USING '02' P_OPCAO
          "                   CHANGING GW_SAIDA.
        ENDIF.

        CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
          gw_saida-tx_camb = gw_saida-dmbtr / gw_saida-dmbe2.
        ENDCATCH.

    ENDCASE.

    "Gambeta, e não me pergunte porque.
    IF ( gw_saida-augdt EQ '20150910').
      gw_saida-tx_camb = cs_tax_fix.
    ENDIF.


    READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY vbeln = gw_bsid-vbel2.
    IF ( sy-subrc NE 0 ).
      READ TABLE gt_zsdt0066 INTO gw_zsdt0066 WITH KEY vbeln = gw_bsid-vbel2.
      IF ( sy-subrc NE 0 ).

        READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsid-vbel2.

        IF ( gw_vbak-auart IS INITIAL ).
          CLEAR: gw_saida, gw_bsid, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
          CONTINUE.
        ELSE.
          gw_saida-auart = gw_vbak-auart.
        ENDIF.

        READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
        gw_saida-tp_venda = gw_zsdt0051-tp_venda.

        CASE gw_vbak-auart.
          WHEN: 'ZCPV' OR 'ZREB' OR 'ZCOP'.
            READ TABLE gt_vbfa INTO gw_vbfa WITH KEY vbeln = gw_bsid-vbel2.
            READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY vbeln = gw_vbfa-vbelv.
            gw_saida-nr_sol = gw_zsdt0053-nro_sol_ov.
            READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
            gw_saida-tp_venda = gw_zsdt0051-tp_venda.

            READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0053-matnr.
            gw_saida-matnr = gw_mara-matnr.
            gw_saida-matkl = gw_mara-matkl.
            READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
            gw_saida-maktx = gw_makt-maktx.
        ENDCASE.

      ENDIF.

      READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsid-vbel2.

      IF ( gw_vbak-auart IS INITIAL ).
        CLEAR: gw_saida, gw_bsid, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
        CONTINUE.
      ELSE.
        gw_saida-auart = gw_vbak-auart.
      ENDIF.

      READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
      gw_saida-tp_venda = gw_zsdt0051-tp_venda.

      READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0066-matnr.
      gw_saida-matnr = gw_mara-matnr.
      gw_saida-matkl = gw_mara-matkl.
      READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
      gw_saida-maktx = gw_makt-maktx.

    ELSE.

      READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_bsid-vbel2.

      IF ( gw_vbak-auart IS INITIAL ).
        CLEAR: gw_saida, gw_bsid, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
        CONTINUE.
      ELSE.
        gw_saida-auart = gw_vbak-auart.
      ENDIF.

      READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
      gw_saida-tp_venda = gw_zsdt0051-tp_venda.

      READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zsdt0053-matnr.
      gw_saida-matnr = gw_mara-matnr.
      gw_saida-matkl = gw_mara-matkl.

      READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
      gw_saida-maktx = gw_makt-maktx.

      gw_saida-nr_sol = gw_zsdt0053-nro_sol_ov.
    ENDIF.

    READ TABLE gt_zsdt0041 INTO gw_zsdt0041 WITH KEY vbeln = gw_bsid-vbel2.
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

    READ TABLE gt_zsdt0090 INTO gw_zsdt0090 WITH KEY vbelv = gw_bsid-vbel2.
    IF sy-subrc NE 0.
      READ TABLE gt_zsdt0090 INTO gw_zsdt0090 WITH KEY vbeln = gw_bsid-vbel2.
    ENDIF.
    IF ( gw_saida-nr_sol IS INITIAL ) AND ( sy-subrc = 0 ).

      READ TABLE gt_zsdt0040 INTO gw_zsdt0040 WITH KEY doc_simulacao = gw_zsdt0090-doc_simulacao.
      IF sy-subrc = 0.
        gw_saida-nr_sol = gw_zsdt0040-doc_simulacao.
        gw_saida-tpsim  = gw_zsdt0040-tpsim.
      ENDIF.
    ENDIF.

    IF gw_saida-auart EQ 'ZREB'.
      gw_saida-dmbtr   = gw_saida-dmbtr * -1.
      gw_saida-dmbe2   = gw_saida-dmbe2 * -1.
    ENDIF.

    gw_saida-buzei = gw_bsid-buzei.

    IF ( gw_saida-budat IS INITIAL ).
      gw_saida-budat = gw_bsid-budat.
      gw_saida-augdt = gw_bsid-budat.
    ENDIF.

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
    CLEAR: gw_saida, gw_bsid, gw_kna1, gw_bsis, gw_ska1, gw_skat, gw_zsdt0053, gw_zsdt0051, gw_zsdt0066, gw_vbak, gw_total, gw_vbfa, gw_bsis.
  ENDLOOP.

*  IF NOT ( GT_ZFIT0087[] IS INITIAL ).
*
*    LOOP AT GT_ZFIT0087 INTO GW_ZFIT0087.
*
*      GW_SAIDA-BUKRS     = GW_ZFIT0087-BUKRS.
*      GW_SAIDA-GSBER     = GW_ZFIT0087-GSBER.
*      GW_SAIDA-KUNNR     = GW_ZFIT0087-KUNNR.
*      READ TABLE GT_KNA1 INTO GW_KNA1 WITH KEY KUNNR = GW_ZFIT0087-KUNNR.
*      GW_SAIDA-NAME1     = GW_KNA1-NAME1.
*
*      READ TABLE GT_MAKT INTO GW_MAKT WITH KEY MATNR = GW_ZFIT0087-MATNR.
*      GW_SAIDA-MAKTX = GW_MAKT-MAKTX.
*
*      GW_SAIDA-TIPO      = GW_ZFIT0087-TIPO.
*      GW_SAIDA-BELNR     = GW_ZFIT0087-BELNR.
*      GW_SAIDA-AUGBL     = GW_ZFIT0087-AUGBL.
*      GW_SAIDA-BUDAT     = GW_ZFIT0087-BUDAT.
*      GW_SAIDA-AUGDT     = GW_ZFIT0087-AUGDT.
*      GW_SAIDA-VBEL2     = GW_ZFIT0087-VBEL2.
*      GW_SAIDA-AUART     = GW_ZFIT0087-AUART.
*      GW_SAIDA-VBELN     = GW_ZFIT0087-VBELN.
*      GW_SAIDA-NR_SOL    = GW_ZFIT0087-NR_SOL.
*      GW_SAIDA-TP_VENDA  = GW_ZFIT0087-TP_VENDA.
*      GW_SAIDA-DMBTR     = GW_ZFIT0087-DMBTR.
*      GW_SAIDA-DMBE2     = GW_ZFIT0087-DMBE2.
*      GW_SAIDA-TX_CAMB   = GW_ZFIT0087-TX_CAMB.
*      GW_SAIDA-BANCO_LIQ = GW_ZFIT0087-BANCO_LIQ.
*      GW_SAIDA-BUZEI     = GW_ZFIT0087-BUZEI.
*      GW_SAIDA-MATNR     = GW_ZFIT0087-MATNR.
*
*
*      READ TABLE GT_T001 INTO GW_T001 WITH KEY BUKRS = GW_ZFIT0087-BUKRS.
*      GW_SAIDA-BUTXT = GW_T001-BUTXT.
*
*      APPEND GW_SAIDA TO GT_SAIDA.
*      CLEAR: GW_SAIDA, GW_ZFIT0087, GW_KNA1, GW_MAKT, GW_T001.
*
*    ENDLOOP.
*  ENDIF.

  IF lva_proc_retroativo = abap_false. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

    "LES - ZFI0064 - Ajuste Rateio Impostos US #148838 - WPP
    CLEAR: lit_saida_juros_desc[], lra_bukrs_tmp[],lra_augdt_tmp[], lra_kunnr_tmp[],lra_nr_ov_tmp[],lra_nr_sol_tmp[],lra_auart_tmp[].

    MOVE-CORRESPONDING: it_bukrs[]   TO lra_bukrs_tmp[],
                        it_augdt[]   TO lra_augdt_tmp[],
                        it_kunnr[]   TO lra_kunnr_tmp[],
                        it_vbel2[]   TO lra_nr_ov_tmp[],
                        it_nro_sol[] TO lra_nr_sol_tmp[],
                        it_auart[]   TO lra_auart_tmp[].

    CALL FUNCTION 'ZFI_GET_JUROS_DESC_ZFI0064'
      EXPORTING
        i_bukrs  = lra_bukrs_tmp
        i_augdt  = lra_augdt_tmp
        i_kunnr  = lra_kunnr_tmp
        i_nr_ov  = lra_nr_ov_tmp
        i_nr_sol = lra_nr_sol_tmp
        i_auart  = lra_auart_tmp
        i_opcao  = p_opcao
        i_waers  = 'BRL'
        i_tipo   = p_tipo
      IMPORTING
        r_saida  = lit_saida_juros_desc.

    LOOP AT lit_saida_juros_desc INTO DATA(lwa_saida_juros_desc).
      APPEND INITIAL LINE TO gt_saida ASSIGNING FIELD-SYMBOL(<fs_saida_juros>).

      MOVE-CORRESPONDING lwa_saida_juros_desc TO <fs_saida_juros>.
      <fs_saida_juros>-jr_ds = abap_true.
    ENDLOOP.
    ""FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
*    IF 1 = 2.
*      REFRESH gt_saida_jros.
*
*      "Define Tipo O.V.
*      PERFORM f_get_auart_tipo TABLES gt_tvak_aux
*                                USING p_tipo.
*
*      CALL FUNCTION 'Z_FI_JUROS_MI'
*        EXPORTING
*          it_empresa   = gt_empresa_jros
*          it_data      = gt_data_jros
*          it_cliente   = gt_cliente_jros
*          it_nr_ov     = gt_vbel2_jros
*          it_nr_sol    = gt_nr_sol_jros
*          p_opcao      = p_opcao
*          p_tipo       = p_tipo
*        IMPORTING
*          it_resultado = gt_saida_jros
*        TABLES
*          it_tvak      = gt_tvak_aux.
*    ENDIF.
    ""FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<----

*    CLEAR: gw_saida , gw_saida_jros.
*    LOOP AT gt_saida_jros INTO gw_saida_jros.
*      CLEAR: gw_saida.
*
*      MOVE-CORRESPONDING gw_saida_jros TO gw_saida.
*      gw_saida-jr_ds = abap_true.
*
*      APPEND gw_saida TO gt_saida.
*      CLEAR: gw_saida,gw_saida_jros.
*    ENDLOOP.

*    IF ( p_tipo EQ 'AQ' ) OR
*       ( p_tipo EQ 'PA' ).
*
*      "Ajustes Partidas Banco com Valores de Juros/Descontos
*
*      "Buscar 1º Documento de cada compensação
*      gt_saida_aux[] = gt_saida[].
*      DELETE gt_saida_aux WHERE jr_ds = abap_true.
*
*      LOOP AT gt_saida_aux INTO gw_saida_aux.
*        DATA(_tabix) = sy-tabix.
*        IF gw_saida_aux-belnr EQ gw_saida_aux-augbl.
*          DELETE gt_saida_aux INDEX _tabix.
*        ENDIF.
*      ENDLOOP.
*
*      SORT gt_saida_aux BY bukrs augbl.
*      DELETE ADJACENT DUPLICATES FROM gt_saida_aux COMPARING bukrs augbl.
*      SORT gt_saida_aux BY bukrs belnr augbl.
*
*      LOOP AT gt_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
*
*        CHECK <fs_saida>-belnr <> <fs_saida>-augbl.
*
*        "Check se é o primeiro documento da compensação.
*        READ TABLE gt_saida_aux INTO gw_saida_aux WITH KEY bukrs = <fs_saida>-bukrs
*                                                           belnr = <fs_saida>-belnr
*                                                           augbl = <fs_saida>-augbl BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          LOOP AT gt_saida_jros INTO gw_saida_jros WHERE bukrs EQ <fs_saida>-bukrs
*                                                     AND augbl EQ <fs_saida>-augbl.
*
*            CHECK gw_saida_jros-dmbtr NE 0.
*
*            v_dmbtr = abs( gw_saida_jros-dmbtr ).
*            v_dmbe2 = abs( gw_saida_jros-dmbe2 ).
*
*            IF gw_saida_jros-dmbtr < 0 .
*              ADD v_dmbtr TO <fs_saida>-dmbtr.
*              ADD v_dmbe2 TO <fs_saida>-dmbe2.
*            ELSE.
*              SUBTRACT v_dmbtr FROM <fs_saida>-dmbtr.
*              SUBTRACT v_dmbe2 FROM <fs_saida>-dmbe2.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*      ENDLOOP.
*
*      CLEAR: gt_saida_aux[].
*
*    ENDIF.

    "LES - ZFI0064 - Ajuste Rateio Impostos US #148838 - WPP

  ENDIF.  "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

  IF lva_proc_retroativo = abap_false. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

*------------------------------------------------*
*  Docs USD                                      *
*------------------------------------------------*

    IF ( gt_tvak[] IS NOT INITIAL ) AND
       ( p_tipo NE 'AQ' AND
         p_tipo NE 'PA' ).

      REFRESH gt_saida_usd[].
      CALL FUNCTION 'Z_FI_MERCADO_INTERNO_USD'
        EXPORTING
          p_opcao      = p_opcao
          p_tipo       = p_tipo
        IMPORTING
          it_resultado = gt_saida_usd
        TABLES
          it_empresa   = it_empresa
          it_data      = it_data
          it_cliente   = it_cliente
          it_nr_ov     = it_nr_ov
          it_nr_sol    = it_nr_sol
          it_tvak      = gt_tvak.

      LOOP AT gt_saida_usd INTO gw_saida_usd.
        CLEAR: gw_saida.
        MOVE-CORRESPONDING gw_saida_usd TO gw_saida.
        APPEND gw_saida TO gt_saida.
        CLEAR: gw_saida,gw_saida_jros.
      ENDLOOP.

      "LES - ZFI0064 - Ajuste Rateio Impostos US #148838 - WPP - Ini
      CLEAR: lit_saida_juros_desc[], lra_bukrs_tmp[],lra_augdt_tmp[], lra_kunnr_tmp[],lra_nr_ov_tmp[],lra_nr_sol_tmp[],lra_auart_tmp[].

      MOVE-CORRESPONDING: it_bukrs[]   TO lra_bukrs_tmp[],
                          it_augdt[]   TO lra_augdt_tmp[],
                          it_kunnr[]   TO lra_kunnr_tmp[],
                          it_vbel2[]   TO lra_nr_ov_tmp[],
                          it_nro_sol[] TO lra_nr_sol_tmp[],
                          it_auart[]   TO lra_auart_tmp[].

      CALL FUNCTION 'ZFI_GET_JUROS_DESC_ZFI0064'
        EXPORTING
          i_bukrs  = lra_bukrs_tmp
          i_augdt  = lra_augdt_tmp
          i_kunnr  = lra_kunnr_tmp
          i_nr_ov  = lra_nr_ov_tmp
          i_nr_sol = lra_nr_sol_tmp
          i_auart  = lra_auart_tmp
          i_opcao  = p_opcao
          i_waers  = 'USD'
          i_tipo   = p_tipo
        IMPORTING
          r_saida  = lit_saida_juros_desc.

      LOOP AT lit_saida_juros_desc INTO lwa_saida_juros_desc.
        APPEND INITIAL LINE TO gt_saida ASSIGNING <fs_saida_juros>.

        MOVE-CORRESPONDING lwa_saida_juros_desc TO <fs_saida_juros>.
        <fs_saida_juros>-jr_ds = abap_true.
      ENDLOOP.

*      REFRESH gt_saida_jros.

*      "Define Tipo O.V.
*      PERFORM f_get_auart_tipo TABLES gt_tvak_aux
*                                USING p_tipo.

*      CALL FUNCTION 'Z_FI_JUROS_MI'
*        EXPORTING
*          it_empresa   = gt_empresa_jros
*          it_data      = gt_data_jros
*          it_cliente   = gt_cliente_jros
*          it_nr_ov     = gt_vbel2_jros
*          it_nr_sol    = gt_nr_sol_jros
*          p_opcao      = p_opcao
*          p_tipo       = p_tipo
*          i_waers      = 'USD'
*        IMPORTING
*          it_resultado = gt_saida_jros
*        TABLES
*          it_tvak      = gt_tvak.
*
*      CLEAR: gw_saida , gw_saida_jros.
*      LOOP AT gt_saida_jros INTO gw_saida_jros.
*        MOVE-CORRESPONDING gw_saida_jros TO gw_saida.
*        APPEND gw_saida TO gt_saida.
*        CLEAR: gw_saida,gw_saida_jros.
*      ENDLOOP.
      "LES - ZFI0064 - Ajuste Rateio Impostos US #148838 - WPP - Fim

    ENDIF.

*------------------------------------------------*
*  Fim Docs USD                                  *
*------------------------------------------------*

  ENDIF. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

  " 18.09.2024 - 145583 -- RAMON -->
  CASE p_opcao.
    WHEN 'VC'.
      IF ( p_tipo = 'MI' ).

        SELECT *
          FROM zi_fi_pedidos_zub INTO TABLE @DATA(lt_zub)
         WHERE nro_sol_ov IN @it_nro_sol[]
           AND budat IN @it_augdt[]
           AND bukrs IN @it_bukrs[]
           AND kunnr IN @it_kunnr[].

        LOOP AT lt_zub ASSIGNING FIELD-SYMBOL(<fs_zub>).
          CLEAR gw_saida_aux.
          MOVE-CORRESPONDING <fs_zub> TO gw_saida_aux.
          gw_saida_aux-nr_sol = <fs_zub>-nro_sol_ov.
          gw_saida_aux-augbl  = gw_saida_aux-belnr.
          gw_saida_aux-augdt  = gw_saida_aux-budat.
          APPEND gw_saida_aux TO gt_saida.
        ENDLOOP.

      ENDIF.
    WHEN 'TR'.
  ENDCASE.
  " 18.09.2024 - 145583 -- RAMON --<

  CLEAR: gt_kna1_ktokd[].

  IF gt_saida[] IS NOT INITIAL.
    SELECT *
      FROM kna1 APPENDING TABLE gt_kna1_ktokd
       FOR ALL ENTRIES IN gt_saida
     WHERE kunnr EQ gt_saida-kunnr.

    SELECT matnr, matkl
      FROM mara INTO TABLE @DATA(lit_mara_matkl)
       FOR ALL ENTRIES IN @gt_saida
     WHERE matnr = @gt_saida-matnr.

    SELECT vbeln, matkl
      FROM vbap INTO TABLE @DATA(lit_vbap_matkl)
       FOR ALL ENTRIES IN @gt_saida
     WHERE vbeln = @gt_saida-vbel2.
  ENDIF.

  IF gt_zfit0087[] IS NOT INITIAL.
    SELECT *
      FROM kna1 APPENDING TABLE gt_kna1_ktokd
       FOR ALL ENTRIES IN gt_zfit0087
     WHERE kunnr EQ gt_zfit0087-kunnr.

    SELECT matnr, matkl
      FROM mara APPENDING TABLE @lit_mara_matkl
       FOR ALL ENTRIES IN @gt_zfit0087
     WHERE matnr = @gt_zfit0087-matnr.

    SELECT vbeln, matkl
      FROM vbap APPENDING TABLE @lit_vbap_matkl
       FOR ALL ENTRIES IN @gt_zfit0087
     WHERE vbeln = @gt_zfit0087-vbel2.
  ENDIF.

  SORT: lit_mara_matkl BY matnr,
        gt_kna1_ktokd  BY kunnr,
        lit_vbap_matkl BY vbeln.


  LOOP AT gt_saida ASSIGNING FIELD-SYMBOL(<fs_saida_aux>).
    READ TABLE gt_kna1_ktokd WITH KEY kunnr = <fs_saida_aux>-kunnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_saida_aux>-ktokd = gt_kna1_ktokd-ktokd.
    ENDIF.

    IF <fs_saida_aux>-matnr IS INITIAL AND <fs_saida_aux>-matkl IS INITIAL.
      READ TABLE lit_vbap_matkl INTO DATA(lwa_vbap_matkl) WITH KEY vbeln = <fs_saida_aux>-vbel2 BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_saida_aux>-matkl = lwa_vbap_matkl-matkl.
      ENDIF.
    ENDIF.

    IF <fs_saida_aux>-matnr IS NOT INITIAL.
      READ TABLE lit_mara_matkl INTO DATA(lwa_mara_matkl) WITH KEY matnr = <fs_saida_aux>-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_saida_aux>-matkl = lwa_mara_matkl-matkl.
      ELSE.
        CLEAR: <fs_saida_aux>-matkl.
      ENDIF.
    ENDIF.

  ENDLOOP.

  LOOP AT gt_zfit0087 ASSIGNING FIELD-SYMBOL(<fs_zfit0087_aux>).
    READ TABLE gt_kna1_ktokd WITH KEY kunnr = <fs_zfit0087_aux>-kunnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_zfit0087_aux>-ktokd = gt_kna1_ktokd-ktokd.
    ENDIF.

    IF <fs_zfit0087_aux>-matnr IS INITIAL AND <fs_zfit0087_aux>-matkl IS INITIAL.
      READ TABLE lit_vbap_matkl INTO lwa_vbap_matkl WITH KEY vbeln = <fs_zfit0087_aux>-vbel2 BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_zfit0087_aux>-matkl = lwa_vbap_matkl-matkl.
      ENDIF.
    ENDIF.

    IF <fs_zfit0087_aux>-matnr IS NOT INITIAL.
      READ TABLE lit_mara_matkl INTO lwa_mara_matkl WITH KEY matnr = <fs_zfit0087_aux>-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_zfit0087_aux>-matkl = lwa_mara_matkl-matkl.
      ELSE.
        CLEAR: <fs_zfit0087_aux>-matkl.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM f_descartar_lancamentos TABLES gt_saida gt_zfit0087 USING p_tipo. "FI - ZFI0064 - Transação Parametros US #149772 - WPP  --->>>

  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
  DATA(lit_saida_sem_param) = gt_saida[].
  DELETE lit_saida_sem_param WHERE auart IN it_auart.
  IF lit_saida_sem_param[] IS NOT INITIAL.
    SELECT *
      FROM zfit0221 INTO TABLE @DATA(lit_zfit0221)
       FOR ALL ENTRIES IN @lit_saida_sem_param
      WHERE operacao EQ @p_tipo
       AND  auart    EQ @lit_saida_sem_param-auart.

    SORT lit_zfit0221 BY auart.
    LOOP AT lit_saida_sem_param ASSIGNING FIELD-SYMBOL(<fs_saida_sem_param>).
      READ TABLE lit_zfit0221 INTO DATA(lwa_zsdt0021) WITH KEY auart = <fs_saida_sem_param>-auart BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR: <fs_saida_sem_param>-belnr.
      ENDIF.
    ENDLOOP.

    DELETE lit_saida_sem_param WHERE belnr IS INITIAL.
    IF lit_saida_sem_param[] IS NOT INITIAL.
      PERFORM f_email_aviso_param TABLES lit_saida_sem_param USING 'AUART' p_tipo.
    ENDIF.
  ENDIF.
  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<---


  DELETE gt_saida    WHERE auart NOT IN it_auart.
  DELETE gt_zfit0087 WHERE auart NOT IN it_auart.

  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
  DELETE gt_saida    WHERE nr_sol NOT IN it_nro_sol.
  DELETE gt_zfit0087 WHERE nr_sol NOT IN it_nro_sol.

  CASE p_opcao.
    WHEN 'VC'.
      DELETE gt_zfit0087 WHERE visao_caixa IS INITIAL.
    WHEN 'TR'.
      DELETE gt_zfit0087 WHERE visao_caixa IS NOT INITIAL.
  ENDCASE.
  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<-----

  "IF lva_proc_retroativo = abap_false. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

  CASE p_tipo.
    WHEN 'MI'. "Mercado Interno
    WHEN 'IN'. "Insumos
      DELETE gt_saida    WHERE augdt < '20170516'.
      DELETE gt_zfit0087 WHERE augdt < '20170516'.
    WHEN 'AQ'. "Frete Aquav./Transb./Serv.Port.-Hermasa
      DELETE gt_saida    WHERE gsber EQ '0161'.
      DELETE gt_zfit0087 WHERE gsber EQ '0161'.
    WHEN 'PA'. "Portochuelo-Amaggi
      DELETE gt_saida    WHERE gsber NE '0161'.
      DELETE gt_zfit0087 WHERE gsber NE '0161'.
  ENDCASE.

  "CS2023000175 Filtro por Grupo de Mercadoria - Ini
  RANGES: r_matkl_tipo FOR mara-matkl.
  DATA: lit_zfit0222 TYPE TABLE OF zfit0222. "FI - ZFI0064 - Transação Parametros US #149772 - WPP

  CLEAR: r_matkl_tipo[], lit_zfit0222[]. "FI - ZFI0064 - Transação Parametros US #149772 - WPP

  "FI - ZFI0064 - Transação Parametros US #149772 - WPP -->>>
  SELECT *
    FROM zfit0222 INTO TABLE lit_zfit0222
    WHERE operacao EQ p_tipo.

  SORT lit_zfit0222 BY matkl.

  IF lit_zfit0222[] IS NOT INITIAL AND p_tipo IS NOT INITIAL.
    LOOP AT lit_zfit0222 INTO DATA(lwa_zfit0222) WHERE active = abap_true.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_zfit0222-matkl ) TO r_matkl_tipo.
    ENDLOOP.
  ENDIF.

*    CONCATENATE 'ZFI0064_MATKL_' p_tipo INTO DATA(lwa_stvarv_matkl).
*    SELECT *
*      FROM tvarvc INTO TABLE @DATA(lit_matkl_tipo)
*     WHERE name EQ @lwa_stvarv_matkl.
*
*    LOOP AT lit_matkl_tipo INTO DATA(lwa_matkl_tipo).
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_matkl_tipo-low ) TO r_matkl_tipo.
*    ENDLOOP.

  "FI - ZFI0064 - Transação Parametros US #149772 - WPP <<---

  IF r_matkl_tipo[] IS NOT INITIAL.

    "FI - ZFI0064 - Transação Parametros US #149772 - WPP -->>>
    lit_saida_sem_param = gt_saida[].
    DELETE lit_saida_sem_param WHERE matkl IN r_matkl_tipo.
    IF lit_saida_sem_param[] IS NOT INITIAL.

      LOOP AT lit_saida_sem_param ASSIGNING <fs_saida_sem_param>.
        READ TABLE lit_zfit0222 INTO DATA(lwa_zsdt0222) WITH KEY matkl = <fs_saida_sem_param>-matkl BINARY SEARCH.
        IF sy-subrc EQ 0.
          CLEAR: <fs_saida_sem_param>-belnr.
        ENDIF.
      ENDLOOP.
      DELETE lit_saida_sem_param WHERE belnr IS INITIAL.

      IF lit_saida_sem_param[] IS NOT INITIAL.
        PERFORM f_email_aviso_param TABLES lit_saida_sem_param USING 'MATKL' p_tipo.
      ENDIF.
    ENDIF.
    "FI - ZFI0064 - Transação Parametros US #149772 - WPP <<---

    DELETE gt_saida    WHERE matkl NOT IN r_matkl_tipo.
    DELETE gt_zfit0087 WHERE matkl NOT IN r_matkl_tipo.

  ENDIF.
  "CS2023000175 Filtro por Grupo de Mercadoria - Fim


  "FI - ZFI0064 - Transação Parametros US #149772 - WPP  --->>>
*------------------------------------------------*
* Tratamento Lançamentos Empresa 0032
*------------------------------------------------*
*    RANGES: r_matkl_graos FOR mara-matkl.
*
*    CLEAR: r_matkl_graos[].
*
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = '700110' ) TO r_matkl_graos.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = '700170' ) TO r_matkl_graos.
*
*    DELETE gt_saida    WHERE bukrs = '0032' AND ( ktokd EQ 'ZCIC' OR matkl NOT IN r_matkl_graos ).
*    DELETE gt_zfit0087 WHERE bukrs = '0032' AND ( ktokd EQ 'ZCIC' OR matkl NOT IN r_matkl_graos ).
*
*    IF ( p_tipo EQ 'PA' ) OR ( p_tipo EQ 'AQ' ).
*      SELECT SINGLE *
*        FROM setleaf INTO @DATA(_wl_zfi0064_ini_aquav)
*       WHERE setname = 'ZFI0064_INI_AQUAV_2'.
*
*      IF ( sy-subrc EQ 0 ) AND ( _wl_zfi0064_ini_aquav-valfrom IS NOT INITIAL ).
*        v_aquav_ini = _wl_zfi0064_ini_aquav-valfrom.
*
*        LOOP AT gt_zfit0087 INTO gw_zfit0087 WHERE augdt < v_aquav_ini.
*          DELETE FROM zfit0087 WHERE belnr   = gw_zfit0087-belnr
*                                 AND buzei   = gw_zfit0087-buzei
*                                 AND bukrs   = gw_zfit0087-bukrs
*                                 AND augbl   = gw_zfit0087-augbl.
*        ENDLOOP.
*
*        DELETE gt_saida    WHERE augdt < v_aquav_ini.
*        DELETE gt_zfit0087 WHERE augdt < v_aquav_ini.
*      ENDIF.
*
*      "Eliminar Clientes Intercompany
*      IF gt_saida[] IS NOT INITIAL.
*        CLEAR: r_kunnr_tmp[], gt_kna1_tmp[].
*
*        SELECT *
*          FROM kna1 INTO TABLE gt_kna1_tmp
*           FOR ALL ENTRIES IN gt_saida
*         WHERE kunnr EQ gt_saida-kunnr.
*
*        LOOP AT gt_kna1_tmp WHERE ktokd = 'ZCIC'.
*          r_kunnr_tmp-sign   = 'I'.
*          r_kunnr_tmp-option = 'EQ'.
*          r_kunnr_tmp-low    = gt_kna1_tmp-kunnr.
*          APPEND r_kunnr_tmp.
*        ENDLOOP.
*
*        SORT r_kunnr_tmp BY low.
*        DELETE ADJACENT DUPLICATES FROM r_kunnr_tmp COMPARING low.
*
*        IF r_kunnr_tmp[] IS NOT INITIAL.
*
*          CLEAR: r_bukrs_tmp.
*
*          r_bukrs_tmp-sign   = 'I'.
*          r_bukrs_tmp-option = 'EQ'.
*          r_bukrs_tmp-low    = '0041'.  APPEND r_bukrs_tmp.
*          r_bukrs_tmp-low    = '0039'.  APPEND r_bukrs_tmp.
*
*          LOOP AT gt_zfit0087 INTO gw_zfit0087 WHERE kunnr IN r_kunnr_tmp
*                                                 AND bukrs NOT IN r_bukrs_tmp.
*
*
*            DELETE FROM zfit0087 WHERE belnr   = gw_zfit0087-belnr
*                                   AND buzei   = gw_zfit0087-buzei
*                                   AND bukrs   = gw_zfit0087-bukrs
*                                   AND augbl   = gw_zfit0087-augbl.
*          ENDLOOP.
*
*          DELETE gt_saida    WHERE kunnr IN r_kunnr_tmp AND bukrs NOT IN r_bukrs_tmp.
*          DELETE gt_zfit0087 WHERE kunnr IN r_kunnr_tmp AND bukrs NOT IN r_bukrs_tmp.
*        ENDIF.
*      ENDIF. ""Eliminar Clientes Intercompany
*    ENDIF.
  "FI - ZFI0064 - Transação Parametros US #149772 - WPP  <<<---

  IF lva_proc_retroativo = abap_false. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

    "Busca registros das Tables Standard que não existem na ZFIT0087
    LOOP AT gt_saida INTO gw_saida.

      var_tabix = sy-tabix.

      READ TABLE gt_zfit0087 INTO gw_zfit0087
        WITH KEY belnr  = gw_saida-belnr
                 buzei  = gw_saida-buzei
                 bukrs  = gw_saida-bukrs.

      IF sy-subrc <> 0.

        CLEAR gw_zfit0087.

        MOVE-CORRESPONDING gw_saida TO gw_zfit0087.

        IF ( p_opcao EQ 'VC' ).
          gw_zfit0087-visao_caixa   = 'X'.
        ENDIF.

        gw_zfit0087-data_registro = sy-datum.
        gw_zfit0087-hora_registro = sy-uzeit.
        gw_zfit0087-usuario       = sy-uname.

        APPEND gw_zfit0087 TO gt_zfit0087.
        INSERT zfit0087 FROM gw_zfit0087.

        COMMIT WORK.

      ENDIF.

      CLEAR: gw_saida, gw_zfit0087.
    ENDLOOP.

    "Busca registros table ZFIT0087 que não existem na Standard
    "Caso encontre, efetua estorno
    CLEAR: gw_zfit0087, gw_zfit0087_aux.
    LOOP AT gt_zfit0087 INTO gw_zfit0087 WHERE ( ( estorno EQ abap_false  ) AND
                                                 ( dmbtr   NE 0           ) AND
                                                 ( lcto_manual IS INITIAL ) ).

      CLEAR: vl_estorno, vl_tabix.

      vl_tabix = sy-tabix.

      SELECT SINGLE *
        FROM bkpf
        INTO gw_bkpf
       WHERE bukrs EQ gw_zfit0087-bukrs
         AND belnr EQ gw_zfit0087-belnr
         AND gjahr EQ gw_zfit0087-budat(4)
         AND stblg EQ ''.
      "READ TABLE GT_SAIDA INTO GW_SAIDA
      "  WITH KEY BELNR  = GW_ZFIT0087-BELNR
      "           BUZEI  = GW_ZFIT0087-BUZEI
      "           BUKRS  = GW_ZFIT0087-BUKRS.

      IF sy-subrc <> 0.
        vl_estorno = 'X'.
      ELSE.
        IF gw_zfit0087-bukrs IS NOT INITIAL AND
           gw_zfit0087-augbl IS NOT INITIAL AND
           gw_zfit0087-augdt IS NOT INITIAL.

          SELECT SINGLE *
            FROM bkpf INTO gw_bkpf
           WHERE bukrs EQ gw_zfit0087-bukrs
             AND belnr EQ gw_zfit0087-augbl
             "AND gjahr EQ gw_zfit0087-augdt(4)  ""FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
             AND stblg EQ ''.

          IF sy-subrc <> 0.
            DELETE FROM zfit0087 WHERE belnr   = gw_zfit0087-belnr
                                   AND buzei   = gw_zfit0087-buzei
                                   AND bukrs   = gw_zfit0087-bukrs
                                   AND augbl   = gw_zfit0087-augbl.

            DELETE gt_zfit0087 INDEX vl_tabix.
          ENDIF.
        ENDIF.
      ENDIF.

      IF vl_estorno IS NOT INITIAL.

        "CLEAR: V_DMBTR, V_DMBE2.
        "V_DMBTR = GW_ZFIT0087-DMBTR * -1.
        "V_DMBE2 = GW_ZFIT0087-DMBE2 * -1.

        CLEAR: gw_zfit0087_aux.
        READ TABLE gt_zfit0087 INTO gw_zfit0087_aux WITH KEY belnr      = gw_zfit0087-belnr
                                                             buzei+1(2) = gw_zfit0087-buzei+1(2)
                                                             bukrs      = gw_zfit0087-bukrs
                                                             estorno    = 'X'.

        "SELECT SINGLE *
        "  FROM ZFIT0087 INTO GW_ZFIT0087_AUX
        " WHERE BELNR = GW_ZFIT0087-BELNR
        "   AND BUZEI = GW_ZFIT0087-BUZEI
        "   AND BUKRS = GW_ZFIT0087-BUKRS
        "   AND DMBTR < 0
        "    AND DMBE2 < 0.

        IF sy-subrc <> 0.

          MOVE-CORRESPONDING gw_zfit0087 TO gw_zfit0087_aux.

          CLEAR: v_buzei_aux.
          CONCATENATE '9' gw_zfit0087_aux-buzei+1(2) INTO v_buzei_aux.

          gw_zfit0087_aux-buzei         = v_buzei_aux.
          gw_zfit0087_aux-dmbtr = gw_zfit0087-dmbtr * -1.
          gw_zfit0087_aux-dmbe2 = gw_zfit0087-dmbe2 * -1.
          gw_zfit0087_aux-data_registro = sy-datum.
          gw_zfit0087_aux-hora_registro = sy-uzeit.
          gw_zfit0087_aux-usuario       = sy-uname.
          gw_zfit0087_aux-estorno       = 'X'.

          APPEND gw_zfit0087_aux TO gt_zfit0087.
          INSERT zfit0087 FROM gw_zfit0087_aux.

          COMMIT WORK.
        ENDIF.

      ENDIF.

      CLEAR: gw_zfit0087, gw_zfit0087_aux, gw_bkpf.

    ENDLOOP.

  ENDIF. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

  "Copia Dados Tabela Aux.
  REFRESH gt_saida_aux.
  CLEAR: gw_saida_aux, gw_zfit0087.

  LOOP AT gt_zfit0087 INTO gw_zfit0087.

    CLEAR: gw_saida_aux, gw_kna1, gw_makt, gw_t001.

*    IF SY-TCODE <> 'ZFI0064'.
*
*      IF NOT ( GW_ZFIT0087-ESTORNO IS INITIAL ).
*        CLEAR: GW_SAIDA_AUX, GW_ZFIT0087, GW_KNA1, GW_MAKT, GW_T001.
*        CONTINUE.
*      ENDIF.
*
*      "SELECT SINGLE *
*      "  INTO GW_ZFIT0087_AUX
*      "  FROM ZFIT0087
*      " WHERE BELNR  = GW_ZFIT0087-BELNR
*      "   AND BUZEI  = GW_ZFIT0087-BUZEI
*      "   AND BUKRS  = GW_ZFIT0087-BUKRS
*      "   AND ESTORNO <> ''.
*
*      CLEAR: GW_ZFIT0087_AUX.
*      READ TABLE GT_ZFIT0087 INTO GW_ZFIT0087_AUX WITH KEY BELNR      = GW_ZFIT0087-BELNR
*                                                           BUZEI+1(2) = GW_ZFIT0087-BUZEI+1(2)
*                                                           BUKRS      = GW_ZFIT0087-BUKRS
*                                                           ESTORNO    = 'X'.
*
*      IF SY-SUBRC = 0.
*        CLEAR: GW_SAIDA_AUX, GW_ZFIT0087, GW_KNA1, GW_MAKT, GW_T001.
*        CONTINUE.
*      ENDIF.
*
*    ENDIF.


    gw_saida_aux-bukrs     = gw_zfit0087-bukrs.
    gw_saida_aux-gsber     = gw_zfit0087-gsber.
    gw_saida_aux-kunnr     = gw_zfit0087-kunnr.
    READ TABLE gt_kna1 INTO gw_kna1 WITH KEY kunnr = gw_zfit0087-kunnr.
    gw_saida_aux-name1     = gw_kna1-name1.

    READ TABLE gt_makt INTO gw_makt
      WITH KEY matnr = gw_zfit0087-matnr.

    gw_saida_aux-maktx = gw_makt-maktx.

    gw_saida_aux-tipo      = gw_zfit0087-tipo.
    gw_saida_aux-belnr     = gw_zfit0087-belnr.
    gw_saida_aux-augbl     = gw_zfit0087-augbl.
    gw_saida_aux-budat     = gw_zfit0087-budat.
    gw_saida_aux-augdt     = gw_zfit0087-augdt.
    gw_saida_aux-ajuste_financeiro = gw_zfit0087-ajuste_financeiro. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>

    IF p_opcao EQ 'TR' AND gw_saida_aux-ajuste_financeiro IS INITIAL. "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
      gw_saida_aux-augdt   = gw_saida_aux-budat.
    ENDIF.

    IF ( gw_saida_aux-augdt NOT IN it_augdt ).
      CONTINUE.
    ENDIF.

    gw_saida_aux-vbel2     = gw_zfit0087-vbel2.
    gw_saida_aux-auart     = gw_zfit0087-auart.
    gw_saida_aux-vbeln     = gw_zfit0087-vbeln.
    gw_saida_aux-nr_sol    = gw_zfit0087-nr_sol.
    gw_saida_aux-tp_venda  = gw_zfit0087-tp_venda.
    gw_saida_aux-dmbtr     = gw_zfit0087-dmbtr.
    gw_saida_aux-dmbe2     = gw_zfit0087-dmbe2.
    gw_saida_aux-waers     = gw_zfit0087-waers.
    gw_saida_aux-tpsim     = gw_zfit0087-tpsim.
    gw_saida_aux-tx_camb   = gw_zfit0087-tx_camb.
    gw_saida_aux-ktokd     = gw_zfit0087-ktokd.

    PERFORM get_vlr_dolar USING '01' p_opcao
                       CHANGING gw_saida_aux.

*    IF GW_SAIDA_AUX-WAERS = 'USD'.
*
*      IF GW_ZFIT0087-DMBTR <> GW_SAIDA_AUX-DMBTR.
*        UPDATE ZFIT0087 SET DMBTR   = GW_SAIDA_AUX-DMBTR
*                            TX_CAMB = GW_SAIDA_AUX-TX_CAMB
*                      WHERE BELNR   = GW_ZFIT0087-BELNR
*                        AND BUZEI   = GW_ZFIT0087-BUZEI
*                        AND BUKRS   = GW_ZFIT0087-BUKRS
*                        AND ESTORNO = GW_ZFIT0087-ESTORNO.
*      ENDIF.
*
*    ELSE.

    IF lva_proc_retroativo = abap_false. "LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

      IF gw_zfit0087-dmbe2 <> gw_saida_aux-dmbe2.
        UPDATE zfit0087 SET dmbe2   = gw_saida_aux-dmbe2
                            tx_camb = gw_saida_aux-tx_camb
                            data_ajuste = sy-datum  ""FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
                            hora_ajuste = sy-uzeit  ""FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
                      WHERE belnr   = gw_zfit0087-belnr
                        AND buzei   = gw_zfit0087-buzei
                        AND bukrs   = gw_zfit0087-bukrs
                        AND estorno = gw_zfit0087-estorno.
      ENDIF.


*    ENDIF.

      IF gw_zfit0087-nr_sol <> gw_saida_aux-nr_sol.
        UPDATE zfit0087 SET nr_sol      = gw_saida_aux-nr_sol
                            data_ajuste = sy-datum  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
                            hora_ajuste = sy-uzeit  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
                      WHERE belnr   = gw_zfit0087-belnr
                        AND buzei   = gw_zfit0087-buzei
                        AND bukrs   = gw_zfit0087-bukrs
                        AND estorno = gw_zfit0087-estorno.
      ENDIF.

      "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
      IF p_opcao EQ 'TR' AND gw_zfit0087-estorno EQ abap_false.
        READ TABLE gt_zfit0087 INTO DATA(lwa_zfit0087_check) WITH KEY bukrs   = gw_zfit0087-bukrs
                                                                      belnr   = gw_zfit0087-belnr
                                                                      estorno = abap_true.
        IF sy-subrc NE 0. "Lançamento não foi estornado
          READ TABLE gt_saida INTO DATA(lwa_saida_check) WITH KEY bukrs = gw_zfit0087-bukrs
                                                                  belnr = gw_zfit0087-belnr
                                                                  buzei = gw_zfit0087-buzei.

          IF sy-subrc EQ 0 AND ( gw_zfit0087-dmbtr  NE lwa_saida_check-dmbtr OR
                                 gw_zfit0087-nr_sol NE lwa_saida_check-nr_sol  ).

            UPDATE zfit0087 SET dmbtr   = lwa_saida_check-dmbtr
                                dmbe2   = lwa_saida_check-dmbe2
                                nr_sol  = lwa_saida_check-nr_sol
                                tx_camb = lwa_saida_check-tx_camb
                                data_ajuste = sy-datum  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
                                hora_ajuste = sy-uzeit  "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP --->>>
                          WHERE belnr   = gw_zfit0087-belnr
                            AND buzei   = gw_zfit0087-buzei
                            AND bukrs   = gw_zfit0087-bukrs
                            AND estorno = gw_zfit0087-estorno.

            gw_saida_aux-dmbtr    = lwa_saida_check-dmbtr.
            gw_saida_aux-dmbe2    = lwa_saida_check-dmbe2.
            gw_saida_aux-nr_sol   = lwa_saida_check-nr_sol.
            gw_saida_aux-tx_camb  = lwa_saida_check-tx_camb.

          ENDIF.
        ENDIF.
      ENDIF.
      "FI - ZFI0064 - Incluir Lctos Ajuste US #172427 - WPP <<<---

    ENDIF."LES - ZFI0064 - Reprocessando Retroativo US #148838 - WPP

    gw_saida_aux-banco_liq = gw_zfit0087-banco_liq.
    gw_saida_aux-buzei     = gw_zfit0087-buzei.
    gw_saida_aux-matnr     = gw_zfit0087-matnr.
    gw_saida_aux-matkl     = gw_zfit0087-matkl.

    READ TABLE gt_t001 INTO gw_t001 WITH KEY bukrs = gw_zfit0087-bukrs.
    gw_saida_aux-butxt = gw_t001-butxt.

    CLEAR: gw_vbap, gw_saida_aux-charg.

    IF ( ( gw_saida_aux-auart EQ 'ZFTE' ) OR
         ( gw_saida_aux-auart EQ 'ZSEM' ) OR
         ( gw_saida_aux-auart EQ 'ZDEF' ) ) AND ( gw_saida_aux-vbel2 IS NOT INITIAL )
                                            AND ( sy-tcode = 'ZFI0064' ).

*      Tabela de Solicitação de ordem de venda - MATERIAIS
      SELECT SINGLE *
        FROM zsdt0041 INTO gw_zsdt0041
       WHERE vbeln EQ gw_saida_aux-vbel2.

      IF sy-subrc = 0.
*        Tabela de Solicitação Ordem de Venda - Cabeçalho
        SELECT SINGLE *
          FROM zsdt0040 INTO gw_zsdt0040
         WHERE doc_simulacao EQ gw_zsdt0041-doc_simulacao.
        IF ( sy-subrc = 0 ) AND ( gw_zsdt0040-safra IS NOT INITIAL ).
          gw_saida_aux-charg = gw_zsdt0040-safra.
        ENDIF.
      ENDIF.

    ENDIF.

    IF ( gw_saida_aux-vbel2 IS NOT INITIAL ) AND ( gw_saida_aux-charg IS INITIAL ).
      SELECT SINGLE *
        INTO gw_vbap
        FROM vbap
       WHERE vbeln = gw_saida_aux-vbel2
         AND charg NE ''.

      IF ( sy-subrc = 0 ) AND ( sy-tcode = 'ZFI0064' ).
        gw_saida_aux-charg = gw_vbap-charg.
      ENDIF.
    ENDIF.

    IF ( 'ZFUT_ZREM' CS gw_saida_aux-auart ) AND ( gw_saida_aux-vbel2 IS NOT INITIAL ).
      SELECT SINGLE *
        FROM vbak INTO @DATA(wl_vbak_aux)
       WHERE vbeln = @gw_saida_aux-vbel2.

      IF ( sy-subrc EQ 0 ).
        IF p_tipo = 'IN'.
          IF ( wl_vbak_aux-spart NE '02' ) AND
             ( wl_vbak_aux-spart NE '03' ) AND
             ( wl_vbak_aux-spart NE '04' ).
            CONTINUE.
          ENDIF.
        ELSEIF p_tipo = 'MI'.
          IF ( wl_vbak_aux-spart EQ '02' ) OR
             ( wl_vbak_aux-spart EQ '03' ) OR
             ( wl_vbak_aux-spart EQ '04' ).
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF. " IF ( SY-SUBRC EQ 0 ).
    ENDIF. "IF ( 'ZFUT_ZREM' CS GW_SAIDA_AUX-AUART ) AND ( GW_SAIDA_AUX-VBEL2 IS NOT INITIAL ).



*    "RG / #175263 - 15.05.2025 - Início
*
*    DELETE ADJACENT DUPLICATES FROM gt_bsis_rec.
*
*    PERFORM recebimento_juros TABLES gt_bsis_rec
*                                     gt_hkont_param
*                              USING gw_saida_aux-bukrs
*                                    gw_saida_aux-augbl
*                              CHANGING gw_saida_aux-dmbtr
*                                       gw_saida_aux-dmbe2.
*   "RG / #175263 - 15.05.2025 - fim

    APPEND gw_saida_aux TO gt_saida_aux.
    CLEAR: gw_saida_aux, gw_zfit0087, gw_kna1, gw_makt, gw_t001.

  ENDLOOP.

  CASE p_opcao.
    WHEN: 'VC'.
      it_resultado[] = gt_saida_aux[].
    WHEN: 'TR'.
      it_resultado[] = gt_saida_aux[].


*  CASE P_OPCAO.
*    WHEN: 'VC'.
*      IT_RESULTADO[] = GT_SAIDA[].
*    WHEN: 'TR'.
*      IT_RESULTADO[] = GT_SAIDA[].




*      GT_SAIDA_AUX[] = GT_SAIDA[].
*      REFRESH: GT_SAIDA[],GT_BSAD[].
*
*      SELECT BELNR AUGBL BUKRS GJAHR VBEL2 GSBER KUNNR ZFBDT ZBD1T BUDAT AUGDT VBELN DMBTR DMBE2 UMSKS FROM BSAD
*        INTO TABLE GT_BSAD
*        FOR ALL ENTRIES IN GT_SAIDA_AUX
*     WHERE BELNR EQ GT_SAIDA_AUX-AUGBL.
*
*
*      CHECK NOT GT_BSAD[] IS INITIAL.
*
*      SELECT BELNR AUGBL BUKRS GJAHR VBEL2 GSBER KUNNR ZFBDT ZBD1T BUDAT AUGDT VBELN DMBTR DMBE2 UMSKS FROM BSAD
*        INTO TABLE GT_BSAD_BA
*        FOR ALL ENTRIES IN GT_BSAD
*      WHERE BUKRS EQ GT_BSAD-BUKRS
*        AND AUGBL EQ GT_BSAD-AUGBL
*        AND GJAHR EQ GT_BSAD-GJAHR
*        AND UMSKS EQ SPACE.
*
*      REFRESH: GT_BSAD_AUX[].
*      GT_BSAD_AUX[] = GT_BSAD_BA[].
*      LOOP AT GT_BSAD_BA INTO GW_BSAD_BA.
*        VAR_TABIX = SY-TABIX.
*        READ TABLE GT_BSAD_AUX INTO GW_BSAD_AUX WITH KEY BELNR = GW_BSAD_BA-AUGBL.
*        IF ( SY-SUBRC EQ 0 ) AND ( GW_BSAD_BA-BELNR EQ GW_BSAD_AUX-BELNR ).
*          DELETE GT_BSAD_BA INDEX VAR_TABIX.
*        ENDIF.
*        CLEAR: GW_BSAD_BA, GW_BSAD_AUX.
*      ENDLOOP.
*
*      CLEAR: GW_SAIDA.
*
*      LOOP AT GT_SAIDA_AUX INTO GW_SAIDA_AUX.
*
*        READ TABLE GT_BSAD INTO GW_BSAD WITH KEY BELNR = GW_SAIDA_AUX-AUGBL.
*        IF ( SY-SUBRC NE 0 ).
*          CLEAR: GW_SAIDA, GW_BSAD.
*          CONTINUE.
*        ENDIF.
*
*        READ TABLE GT_BSAD_BA INTO GW_BSAD_BA WITH KEY BUKRS = GW_BSAD-BUKRS
*                                                       AUGBL = GW_BSAD-AUGBL
*                                                       GJAHR = GW_BSAD-GJAHR
*                                                       UMSKS = SPACE.
*
*        IF ( SY-SUBRC NE 0 ).
*          CLEAR: GW_SAIDA, GW_BSAD, GW_BSAD_BA.
*          CONTINUE.
*        ENDIF.
*
*        GW_SAIDA-BUKRS     =  GW_SAIDA_AUX-BUKRS.
*        GW_SAIDA-GSBER     =  GW_SAIDA_AUX-GSBER.
*        GW_SAIDA-KUNNR     =  GW_SAIDA_AUX-KUNNR.
*        GW_SAIDA-NAME1     =  GW_SAIDA_AUX-NAME1.
*        GW_SAIDA-TIPO      =  GW_SAIDA_AUX-TIPO.
*        GW_SAIDA-BELNR     =  GW_SAIDA_AUX-BELNR.
*        GW_SAIDA-AUGBL     =  GW_SAIDA_AUX-AUGBL.
*        GW_SAIDA-BUDAT     =  GW_SAIDA_AUX-BUDAT.
*        GW_SAIDA-AUGDT     =  GW_SAIDA_AUX-AUGDT.
*        GW_SAIDA-VBEL2     =  GW_SAIDA_AUX-VBEL2.
*
*        IF ( GW_SAIDA_AUX-AUART IS INITIAL ).
*          CLEAR: GW_SAIDA, GW_BSAD, GW_BSAD_BA.
*          CONTINUE.
*        ELSE.
*          GW_SAIDA-AUART     =  GW_SAIDA_AUX-AUART.
*        ENDIF.
*
*
*        GW_SAIDA-VBELN     =  GW_SAIDA_AUX-VBELN.
*        GW_SAIDA-NR_SOL    =  GW_SAIDA_AUX-NR_SOL.
*        GW_SAIDA-DMBTR     =  GW_SAIDA_AUX-DMBTR.
*        GW_SAIDA-DMBE2     =  GW_SAIDA_AUX-DMBE2.
*        GW_SAIDA-TX_CAMB   =  GW_SAIDA_AUX-TX_CAMB.
*        GW_SAIDA-ZFBDT     =  GW_SAIDA_AUX-ZFBDT.
*        GW_SAIDA-BUTXT     =  GW_SAIDA_AUX-BUTXT.
*
*        GW_SAIDA-BELNR_BX  = GW_BSAD_BA-BELNR.
*        GW_SAIDA-BUDAT_BX  = GW_BSAD_BA-BUDAT.
*        GW_SAIDA-AUGDT_BX  = GW_BSAD_BA-AUGDT.
*        GW_SAIDA-DMBTR_BX  = GW_BSAD_BA-DMBTR.
*        GW_SAIDA-DMBE2_BX  = GW_BSAD_BA-DMBE2.
*        GW_SAIDA-VBEL2_BX  = GW_BSAD_BA-VBEL2.
*        GW_SAIDA-VBELN_BX  = GW_BSAD_BA-VBELN.
*
*        APPEND GW_SAIDA TO GT_SAIDA.
*
*        CLEAR: GW_SAIDA, GW_BSAD_AUX, GW_BSAD.
*
*      ENDLOOP.
*
*      IF NOT ( GT_SAIDA[] IS INITIAL ).
*        DELETE GT_SAIDA WHERE TIPO EQ 'Fatura'.
*        IT_RESULTADO[] = GT_SAIDA[].
*      ENDIF.


  ENDCASE.


ENDFUNCTION.
