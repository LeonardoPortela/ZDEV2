*/===========================================================================\*
*|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*
*/===========================================================================\*
*| Descrição:                                                                |*
*| Relatório de risco para o mercado interno                                 |*
*/===========================================================================\*
*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Victor Hugo Souza Nunes ( victor.hugo@grupomaggi.com.br )            |*
*|                                                                           |*
*|  Tester:                                                                  |*
*|    + Marcos Santos ( marcos.santos@grupomaggi.com.br )                    |*
*|  Changelog:                                                               |*
*|                                                                           |*
*/===========================================================================\*
REPORT  zfir0056.

TABLES: bsad, zsdt0053.

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
       END OF ty_saida.


TYPES: BEGIN OF ty_cadastro,
         augbl TYPE bsad-augbl,
         buzei TYPE bseg-buzei,
         budat TYPE bkpf-budat,
         dmbtr TYPE c LENGTH 15,
       END OF ty_cadastro.

TYPES: BEGIN OF ty_compl_diversos,
         bukrs TYPE bsad-bukrs,
         docnr TYPE faglflexa-docnr,
         buzei TYPE faglflexa-buzei,
         hsl   TYPE faglflexa-hsl,
         ksl   TYPE faglflexa-ksl,
         budat TYPE faglflexa-budat,
         kunnr TYPE kna1-kunnr,
         vbeln TYPE zsdt0053-vbeln,
       END OF ty_compl_diversos.

*-----------------------------------
* Tabelas Internas
*-----------------------------------
DATA: gt_empresa     TYPE TABLE OF t001,
      gt_empresa_job TYPE TABLE OF t001 WITH HEADER LINE,
      gt_centro      TYPE TABLE OF t001w,
      gt_data        TYPE TABLE OF bsad,
      gt_cliente     TYPE TABLE OF bsad,
      gt_vbel2       TYPE TABLE OF bsad,
      gt_nr_sol      TYPE TABLE OF zsdt0053,
      gt_saida       TYPE TABLE OF ty_saida. "Estrutura de Saída.

DATA: gt_bdc TYPE TABLE OF bdcdata,
      gw_bdc TYPE bdcdata.

DATA: ok_code TYPE sy-ucomm.
*-----------------------------------
* Work Area
*-----------------------------------
DATA: gw_empresa  TYPE t001,
      gw_centro   TYPE t001w,
      gw_data     TYPE bsad,
      gw_cliente  TYPE bsad,
      gw_vbel2    TYPE bsad,
      gw_bkpf_aux TYPE bkpf,
      gw_nr_sol   TYPE zsdt0053,
      gw_saida    TYPE ty_saida.

DATA: gw_cadastro TYPE ty_cadastro.
DATA: gw_complemento TYPE ty_compl_diversos.


*-----------------------------------
* ALV
*-----------------------------------
DATA: obj_container TYPE REF TO cl_gui_custom_container,
      obj_alv       TYPE REF TO cl_gui_alv_grid.

DATA: gt_fieldcatalog TYPE lvc_t_fcat,
      gw_fieldcatalog TYPE lvc_s_fcat.

*-----------------------------------
* VARIAVEIS DE TELA
*-----------------------------------
DATA: var_bukrs   TYPE bsad-bukrs,
      var_bukrs_h TYPE bsad-bukrs,
      var_augdt   TYPE bsad-augdt,
      var_augdt_h TYPE bsad-augdt,
      var_kunnr   TYPE bsad-kunnr,
      var_kunnr_h TYPE bsad-kunnr,
      var_tipo    TYPE c LENGTH 2,
      var_opcao   TYPE c LENGTH 2.


DATA: it_rows TYPE lvc_t_row,
      lw_rows TYPE lvc_s_row.

DATA: gw_zfit0087 TYPE zfit0087.



SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
                 p_bukrs  FOR bsad-bukrs NO-EXTENSION, " Empresa
                 p_kunnr  FOR bsad-kunnr, "Cliente
                 p_augdt  FOR bsad-augdt NO-EXTENSION, " Data de Liquidação
                 p_vbel2  FOR bsad-vbel2 NO INTERVALS NO-EXTENSION,
                 p_nr_sol FOR zsdt0053-nro_sol_ov NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN: END OF BLOCK b1.


SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.

  "Radio Buttn da Mercado Interno
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_mer_in RADIOBUTTON GROUP rb2.
    SELECTION-SCREEN COMMENT 3(20) TEXT-004 FOR FIELD p_mer_in.
  SELECTION-SCREEN END OF LINE.

  "Radio Buttn Insumos
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_insum RADIOBUTTON GROUP rb2.
    SELECTION-SCREEN COMMENT 3(20) TEXT-005 FOR FIELD p_insum.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_aquav RADIOBUTTON GROUP rb2.
    SELECTION-SCREEN COMMENT 3(60) TEXT-006 FOR FIELD p_aquav.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_prtam RADIOBUTTON GROUP rb2.
    SELECTION-SCREEN COMMENT 3(60) TEXT-007 FOR FIELD p_prtam.
  SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-001.

  "Radio Buttn da Visão Caixa
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_vs_cx RADIOBUTTON GROUP rb1.
    SELECTION-SCREEN COMMENT 3(20) TEXT-002 FOR FIELD   p_vs_cx.
  SELECTION-SCREEN END OF LINE.

  "Radio Button da Baixa de Adiantamento
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_tro_ad RADIOBUTTON GROUP rb1.
    SELECTION-SCREEN COMMENT 3(20) TEXT-003 FOR FIELD   p_tro_ad.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b3.

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "handle_hotspot_click


ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM handle_hotspot_click  USING    i_row_id     TYPE lvc_s_row
                                    i_column_id  TYPE lvc_s_col
                                    is_row_no    TYPE lvc_s_roid.

  DATA opt TYPE ctu_params.

  CLEAR: gw_saida.
  CASE i_column_id.
    WHEN: 'BELNR'.
      READ TABLE gt_saida INTO gw_saida INDEX i_row_id.
      SET PARAMETER ID 'BLN' FIELD gw_saida-belnr.
      SET PARAMETER ID 'BUK' FIELD gw_saida-bukrs.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN: 'BELNR_BX'.
      READ TABLE gt_saida INTO gw_saida INDEX i_row_id.
      SET PARAMETER ID 'BLN' FIELD gw_saida-belnr_bx.
      SET PARAMETER ID 'BUK' FIELD gw_saida-bukrs.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN: 'AUGBL'.
      READ TABLE gt_saida INTO gw_saida INDEX i_row_id.
      SET PARAMETER ID 'BLN' FIELD gw_saida-augbl.
      SET PARAMETER ID 'BUK' FIELD gw_saida-bukrs.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    WHEN: 'VBEL2'.

      READ TABLE gt_saida INTO gw_saida INDEX i_row_id.

      " 26.11.2024 - 159383 - RAMON -->
      IF gw_saida-auart = 'ZUB'.

        SET PARAMETER ID 'BES' FIELD gw_saida-vbel2.

        CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
          EXPORTING
            i_ebeln              = gw_saida-vbel2
          EXCEPTIONS
            not_found            = 1
            no_authority         = 2
            invalid_call         = 3
            preview_not_possible = 4
            OTHERS               = 5.

        IF sy-subrc <> 0.
        ENDIF.

      ELSE.
        SET PARAMETER ID 'AUN' FIELD gw_saida-vbel2.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

      ENDIF.

      " 26.11.2024 - 159383 - RAMON --<

    WHEN: 'VBELN'.

      READ TABLE gt_saida INTO gw_saida INDEX i_row_id.

      " 26.11.2024 - 159383 - RAMON -->
      IF gw_saida-auart = 'ZUB'.

        DATA lv_mblnr TYPE mblnr.
        DATA lv_mjahr TYPE mjahr.

        lv_mblnr = gw_saida-vbeln.
        lv_mjahr = gw_saida-budat(4).

        SET PARAMETER ID 'MBN' FIELD lv_mblnr.
        SET PARAMETER ID 'MJA' FIELD lv_mjahr.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ''
            i_skip_first_screen = 'X'
            i_deadend           = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = lv_mblnr
            i_mjahr             = lv_mjahr
          EXCEPTIONS
            illegal_combination = 1
            OTHERS              = 2.

      ELSE.

        SET PARAMETER ID 'VF' FIELD gw_saida-vbeln.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.

      ENDIF.

      " 26.11.2024 - 159383 - RAMON --<

    WHEN: 'NR_SOL'.
      READ TABLE gt_saida INTO gw_saida INDEX i_row_id.

      IF gw_saida-tpsim IS NOT INITIAL. "Simulador de Vendas.

        PERFORM f_preencher_dynpro USING:
                    'X' 'ZSDR016'                      '0100',
                    ' ' 'WG_HEADER-DOC_SIMULACAO'      gw_saida-nr_sol,
                    ' ' 'BDC_OKCODE'                   'ATUAL'.

        opt-dismode = 'E'.
        opt-defsize = ' '.

        CALL TRANSACTION 'ZSDT0044' USING gt_bdc OPTIONS FROM opt.

      ELSE.

        PERFORM f_preencher_dynpro USING:
              'X' 'ZSDR0022'                      '0050',
              ' ' 'WG_HEADER-NRO_SOL_OV'          gw_saida-nr_sol,
              ' ' 'BDC_OKCODE'                    'ATUAL'.
        opt-dismode = 'E'.
        opt-defsize = ' '.

        CALL TRANSACTION 'ZSDT0062' USING gt_bdc OPTIONS FROM opt.
      ENDIF.


  ENDCASE.

ENDFORM.                    "HANDLE_HOTSPOT_CLICK


START-OF-SELECTION.

  DATA: lva_dias_busca TYPE i.

*---> 20.06.2023 - Migração S4 - DG
  "  data: TG_TIPO type table of CHAR02 with header line.
  DATA: tg_tipo TYPE TABLE OF zchar02 WITH HEADER LINE.
*<--- 20.06.2023 - Migração S4 - DG

  DATA: vg_job    TYPE i,
        var_visao TYPE zde_visao_zfi0064.

  IF sy-batch = 'X'.

    SELECT SINGLE COUNT( * ) INTO vg_job
      FROM tbtco
     WHERE jobname EQ 'ZFI0064_JOB'
       AND status EQ 'R'.

    IF ( vg_job NE 1 ).
      RETURN.
    ENDIF.
  ELSE.
*    IF P_BUKRS-LOW = '0041'.
*      IF P_AQUAV = 'X' AND P_AUGDT-LOW LT '20181226'.
*        MESSAGE 'Para esta opção, a data de liquidação é a partir de 26.12.2018 empresa 0041' TYPE 'I'.
*        RETURN.
*      ENDIF.
*    ENDIF.
  ENDIF.

  CLEAR: var_bukrs, var_bukrs_h, var_augdt, var_augdt_h.
  var_bukrs   =  p_bukrs-low.
  var_bukrs_h =  p_bukrs-high.
  var_augdt   =  p_augdt-low.
  var_augdt_h =  p_augdt-high.
  var_kunnr   =  p_kunnr-low.
  var_kunnr_h =  p_kunnr-high.

  IF ( p_vs_cx EQ 'X' ). "Caso o Radibutton escolido seja o Visão de Caixa.
    var_opcao = 'VC'.
  ELSEIF ( p_tro_ad EQ 'X '). "Senão é Baixa de Adiantamento.
    var_opcao = 'TR'.
  ENDIF.

  CASE abap_true.
    WHEN p_mer_in.
      var_visao = '01'.
      var_tipo  = 'MI'.
    WHEN p_insum.
      var_visao = '02'.
      var_tipo  = 'IN'.
    WHEN p_aquav.
      var_visao = '03'.
      var_tipo  = 'AQ'.
    WHEN p_prtam.
      var_visao = '04'.
      var_tipo  = 'PA'.
  ENDCASE.

  IF sy-batch = 'X'.

    "FI - ZFI0064 - Transação Parametros US #149772 - WPP -->>>

*    gw_empresa-bukrs = '0001'.
*    APPEND gw_empresa TO gt_empresa_job.
*
*    gw_empresa-bukrs = '0010'.
*    APPEND gw_empresa TO gt_empresa_job.
*
*    gw_empresa-bukrs = '0041'.
*    APPEND gw_empresa TO gt_empresa_job.
*
*    gw_empresa-bukrs = '0032'.
*    APPEND gw_empresa TO gt_empresa_job.
*
*    gw_empresa-bukrs = '0039'.
*    APPEND gw_empresa TO gt_empresa_job.

    "FI - ZFI0064 - Transação Parametros US #149772 - WPP <<---

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarc)
     WHERE name EQ 'ZFI0064_DIAS_BUSCA_JOB'.

    IF sy-subrc EQ 0 AND lwa_tvarc-low IS NOT INITIAL.
      lva_dias_busca = lwa_tvarc-low.
    ELSE.
      lva_dias_busca = 30.
    ENDIF.

    gw_data-augdt = ( sy-datum - lva_dias_busca ).
    APPEND gw_data TO gt_data.

    gw_data-augdt = sy-datum.
    APPEND gw_data TO gt_data.

  ELSE.

    "Check de permissão de visão
    AUTHORITY-CHECK OBJECT 'ZFI0064'
      ID 'ZVS_000002' FIELD var_visao.

    IF sy-subrc <> 0.
      SELECT SINGLE *
        FROM user_addrp INTO @DATA(wl_user)
       WHERE bname = @sy-uname.

      IF ( sy-subrc = 0 ) AND ( wl_user-name_first IS NOT INITIAL ).
        MESSAGE | { wl_user-name_first }, seu perfil está sem acesso ao tipo de visão selecionada! | TYPE 'S'.
      ELSE.
        MESSAGE | Perfil do usuário sem acesso ao tipo de visão! | TYPE 'S'.
      ENDIF.

      RETURN.
    ENDIF.

    IF p_bukrs-low IS INITIAL.
      MESSAGE 'Empresa é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF p_augdt-low IS INITIAL.
      MESSAGE 'Data Liquidação é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.

    gw_empresa-bukrs = p_bukrs-low.
    APPEND gw_empresa TO gt_empresa.

    IF NOT ( p_bukrs-high IS INITIAL ).
      gw_empresa-bukrs = p_bukrs-high.
      APPEND gw_empresa TO gt_empresa.
    ENDIF.

    gw_data-augdt = p_augdt-low.
    APPEND gw_data TO gt_data.

    IF NOT ( p_augdt-high IS INITIAL ).
      gw_data-augdt = p_augdt-high.
      APPEND gw_data TO gt_data.
    ENDIF.

  ENDIF.

*  IF NOT ( P_KUNNR-LOW IS INITIAL ).
*
*    GW_CLIENTE-KUNNR = P_KUNNR-LOW.
*    APPEND GW_CLIENTE TO GT_CLIENTE.
*
*    IF NOT ( P_KUNNR-HIGH IS INITIAL ).
*      GW_CLIENTE-KUNNR = P_KUNNR-HIGH.
*      APPEND GW_CLIENTE TO GT_CLIENTE.
*    ENDIF.
*  ENDIF.

  LOOP AT p_kunnr.
    gw_cliente-kunnr = p_kunnr-low.
    APPEND gw_cliente TO gt_cliente.
    IF NOT ( p_kunnr-high IS INITIAL ).
      gw_cliente-kunnr = p_kunnr-high.
      APPEND gw_cliente TO gt_cliente.
    ENDIF.
  ENDLOOP.



  IF NOT ( p_vbel2-low IS INITIAL ).
    gw_vbel2-vbel2 = p_vbel2-low.
    APPEND gw_vbel2 TO gt_vbel2.

    IF NOT ( p_vbel2-high IS INITIAL ).
      gw_vbel2-vbel2 = p_vbel2-high.
      APPEND gw_vbel2 TO gt_vbel2.
    ENDIF.
  ENDIF.


  IF NOT ( p_nr_sol-low IS INITIAL ).
    gw_nr_sol-nro_sol_ov = p_nr_sol-low.
    APPEND gw_nr_sol TO gt_nr_sol.

    IF NOT ( p_nr_sol-high IS INITIAL ).
      gw_nr_sol-nro_sol_ov = p_nr_sol-high.
      APPEND gw_nr_sol TO gt_nr_sol.
    ENDIF.
  ENDIF.


  IF sy-batch = 'X'.
*-------------------------------------------------------*
*   Execução por JOB
*-------------------------------------------------------*

    "FI - ZFI0064 - Transação Parametros US #149772 - WPP -->>>
    DATA: lit_zfit0223 TYPE TABLE OF zfit0223.

    CLEAR: tg_tipo[].
    tg_tipo = 'MI'.
    APPEND tg_tipo.

    tg_tipo = 'IN'.
    APPEND tg_tipo.

    tg_tipo = 'AQ'.
    APPEND tg_tipo.

    tg_tipo = 'PA'.
    APPEND tg_tipo.

    LOOP AT tg_tipo.

      CLEAR: lit_zfit0223[].
      SELECT *
         FROM zfit0223 INTO TABLE lit_zfit0223
         WHERE operacao EQ tg_tipo.

      CHECK lit_zfit0223[] IS NOT INITIAL.

      LOOP AT lit_zfit0223 INTO DATA(lwa_zfit0223) WHERE bukrs  IS NOT INITIAL.

        CLEAR: gt_empresa[].
        APPEND VALUE #( bukrs = lwa_zfit0223-bukrs ) TO gt_empresa.

        "Executar os as duas opções : Visão Caixa e Troca Acerto
        var_opcao = 'VC'.

        CALL FUNCTION 'Z_FI_MERCADO_INTERNO'
          EXPORTING
            p_opcao      = var_opcao
            p_tipo       = tg_tipo
          IMPORTING
            it_resultado = gt_saida
          TABLES
            it_empresa   = gt_empresa
            it_data      = gt_data
            it_cliente   = gt_cliente
            it_nr_ov     = gt_vbel2
            it_nr_sol    = gt_nr_sol.

        var_opcao = 'TR'.

        CALL FUNCTION 'Z_FI_MERCADO_INTERNO'
          EXPORTING
            p_opcao      = var_opcao
            p_tipo       = tg_tipo
          IMPORTING
            it_resultado = gt_saida
          TABLES
            it_empresa   = gt_empresa
            it_data      = gt_data
            it_cliente   = gt_cliente
            it_nr_ov     = gt_vbel2
            it_nr_sol    = gt_nr_sol.


      ENDLOOP. "LOOP AT lit_zfit0223

    ENDLOOP. "LOOP AT tg_tipo.

*    LOOP AT gt_empresa_job.
*
*      CLEAR: gt_empresa[].
*
*      APPEND gt_empresa_job TO gt_empresa.
*
*      CLEAR: tg_tipo[].
*      tg_tipo = 'MI'.
*      APPEND tg_tipo.
*
*      tg_tipo = 'IN'.
*      APPEND tg_tipo.
*
*      tg_tipo = 'AQ'.
*      APPEND tg_tipo.
*
*      tg_tipo = 'PA'.
*      APPEND tg_tipo.
*
*      LOOP AT tg_tipo.
*
**  --------------------------------------------------------------*
**       Executar os as duas opções : Visão Caixa e Troca Acerto
**  --------------------------------------------------------------*
*
*        var_opcao = 'VC'.
*
*        CALL FUNCTION 'Z_FI_MERCADO_INTERNO'
*          EXPORTING
*            p_opcao      = var_opcao
*            p_tipo       = tg_tipo
*          IMPORTING
*            it_resultado = gt_saida
*          TABLES
*            it_empresa   = gt_empresa
*            it_data      = gt_data
*            it_cliente   = gt_cliente
*            it_nr_ov     = gt_vbel2
*            it_nr_sol    = gt_nr_sol.
*
*        var_opcao = 'TR'.
*
*        CALL FUNCTION 'Z_FI_MERCADO_INTERNO'
*          EXPORTING
*            p_opcao      = var_opcao
*            p_tipo       = tg_tipo
*          IMPORTING
*            it_resultado = gt_saida
*          TABLES
*            it_empresa   = gt_empresa
*            it_data      = gt_data
*            it_cliente   = gt_cliente
*            it_nr_ov     = gt_vbel2
*            it_nr_sol    = gt_nr_sol.
*      ENDLOOP.
*
*    ENDLOOP.

    "FI - ZFI0064 - Transação Parametros US #149772 - WPP <<---

  ELSE.

    CALL FUNCTION 'Z_FI_MERCADO_INTERNO'
      EXPORTING
        p_opcao      = var_opcao
        p_tipo       = var_tipo
      IMPORTING
        it_resultado = gt_saida
      TABLES
        it_empresa   = gt_empresa
        it_data      = gt_data
        it_cliente   = gt_cliente
        it_nr_ov     = gt_vbel2
        it_nr_sol    = gt_nr_sol.

    DELETE gt_saida WHERE ( kunnr  NOT IN p_kunnr  ) OR
                          ( vbel2  NOT IN p_vbel2  ) OR
                          ( nr_sol NOT IN p_nr_sol ).

    PERFORM: criar_alv.
    CALL SCREEN 0100.

  ENDIF.

*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  SET PF-STATUS 'PS0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE pai INPUT.
  CASE sy-ucomm.
    WHEN: 'BACK' OR 'CANC'.
      SET SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
    WHEN: 'COPIAR'.
      PERFORM: copiar_registro.
    WHEN: 'COMP_DIVER'.
      PERFORM: complemento_diversos.
    WHEN 'ESTORNO'.
      PERFORM: gerar_estorno.

  ENDCASE.
ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV
*&---------------------------------------------------------------------*
FORM criar_alv .

  DATA: wa_layout  TYPE lvc_s_layo,
        gs_variant TYPE disvariant.

  DATA: gr_event_handler TYPE REF TO lcl_event_handler.

  wa_layout-zebra      = 'X'.

  CREATE OBJECT obj_container
    EXPORTING
      container_name              = 'CONTAINER_P'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  PERFORM: catalog.

  CREATE OBJECT obj_alv
    EXPORTING
      i_parent          = obj_container
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  CREATE OBJECT gr_event_handler.
  SET HANDLER gr_event_handler->handle_hotspot_click FOR obj_alv.

  gs_variant-report   = sy-repid.
  gs_variant-username = sy-uname.
  wa_layout-sel_mode   = 'A'.

  CALL METHOD obj_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      is_variant                    = gs_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = gt_saida[]
      it_fieldcatalog               = gt_fieldcatalog[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " CRIAR_ALV
*&---------------------------------------------------------------------*
*&      Form  CATALOG
*&---------------------------------------------------------------------*
FORM catalog.

  PERFORM fieldcatalog USING:
    'BUKRS'      'Empresa'           '5'    ' ' '' ' ' '' ' ',
    'BUTXT'      'Descr.'            '25'    ' ' '' ' ' '' ' ',
    'GSBER'      'Filial'            '5'    ' ' '' ' ' '' ' ',
    'KUNNR'      'Cliente'           '10'   'X' '' ' ' '' ' ',
    'NAME1'      'Descr. Cliente'    '25'   ' ' '' ' ' '' ' ',
    'TIPO'       'Tipo'              '10'   ' ' '' ' ' '' ' ',
    'MATNR'      'Material'          '10'   'X' '' ' ' '' ' ',
    'MAKTX'      'Descrição Mat.'    '25'   '' '' ' ' '' ' ',
    'BUZEI'      'Item'              '03'   ' ' '' ' ' '' ' ',
    'BELNR'      'Nr. Doc.'          '10'   ' ' 'X' ' ' '' ' ',
    'AUGBL'      'Doc. Compen.'      '10'   ' ' 'X' ' ' '' ' ',
    'BUDAT'      'Dt. Lçto.'         '10'   ' ' '' ' ' '' ' ',
    'AUGDT'      'Dt. Liquidação.'   '10'   ' ' '' ' ' '' ' ',
    'VBEL2'      'Nr. O.V'           '15'   'X' 'X' ' ' '' ' ',
    'AUART'      'Tp. O.V'           '5'    ' ' '' ' ' '' ' ',
    'VBELN'      'Nr. Fatura'        '10'   ' ' 'X' ' ' '' ' ',
    'NR_SOL'     'Nr. Sol.Venda'     '10'   ' ' 'X' ' ' '' '',
    'TP_VENDA'   'Tipo de Venda'     '15'   ' ' '' ' ' '' '',
    'TPSIM'      'Tipo Sim.'         '15'   ' ' '' ' ' '' '',
    'WAERS'      'Moeda'             '05'   ' ' '' ' ' '' '',
    'DMBTR'      'Vlr. R$'           '10'   ' ' '' ' ' '' ' ',
    'DMBE2'      'Vlr. U$'           '10'   ' ' '' ' ' '' ' ',
    'TX_CAMB'    'Tx. Câmbio'        '10'   ' ' '' ' ' '' ' ',
*  IF ( P_VS_CX EQ 'X' ).
*    PERFORM FIELDCATALOG USING:
    'BANCO_LIQ'  'Banco Liquidado'   '25'   ' ' '' ' ' '' ' ',

*  ELSE.
*    PERFORM FIELDCATALOG USING:
*    'BELNR_BX'    'Doc. Baixa'      '15'   ' ' 'X' ' ' '' ' ',
*    'BUDAT_BX'    'Dt. Lanç'        '10'   ' ' '' ' ' '' ' ',
*    'AUGDT_BX'    'Dt. Comp'        '10'   ' ' '' ' ' '' ' ',
*    'DMBTR_BX'    'Vlr. Baixa R$'   '15'   ' ' '' ' ' '' ' ',
*    'DMBE2_BX'    'Vlr. Baixa U$'   '15'   ' ' '' ' ' '' ' ',
*    'VBEL2_BX'    'Nro. OV'         '15'   ' ' '' ' ' '' ' ',
*    'VBELN'       'Nro. Fatura'     '15'   ' ' '' ' ' '' ' '.
*  ENDIF.
    'CHARG'  'Safra'                 '10'   ' ' '' ' ' '' ' '.

ENDFORM.                    " CATALOG

*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fieldcatalog  USING    VALUE(p_fieldname)
                            VALUE(p_desc)
                            VALUE(p_tam)
                            VALUE(p_no_zero)
                            VALUE(p_hotspot)
                            VALUE(p_cor)
                            VALUE(p_just)
                            VALUE(p_sum).

  gw_fieldcatalog-fieldname = p_fieldname.
  gw_fieldcatalog-scrtext_l = p_desc.
  gw_fieldcatalog-scrtext_m = p_desc.
  gw_fieldcatalog-scrtext_s = p_desc.
  gw_fieldcatalog-outputlen = p_tam.
  gw_fieldcatalog-no_zero   = p_no_zero.
  gw_fieldcatalog-hotspot   = p_hotspot.
  gw_fieldcatalog-emphasize = p_cor.
  gw_fieldcatalog-just      = p_just.
  gw_fieldcatalog-do_sum    = p_sum.

  APPEND gw_fieldcatalog TO gt_fieldcatalog.

  CLEAR: gw_fieldcatalog.

ENDFORM.                    " FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
FORM f_preencher_dynpro   USING l_start TYPE c l_name TYPE c l_value.
  MOVE l_start TO gw_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO gw_bdc-program,
  l_value TO gw_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO gw_bdc-fnam,
      l_value TO gw_bdc-fval.
  ENDIF.
  APPEND gw_bdc TO gt_bdc.
  CLEAR: gw_bdc.
ENDFORM.                    " F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.

  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TB0200'.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN: 'GW_CADASTRO-BELNR'.
        IF ( gw_cadastro-augbl IS INITIAL ).
          screen-input  = 1.
          screen-active = 1.
        ENDIF.
      WHEN: 'GW_CADASTRO-BUDAT'.
        IF ( gw_cadastro-budat IS INITIAL ).
          screen-input  = 1.
          screen-active = 1.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " PBO_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0200 INPUT.
  CASE sy-ucomm.
    WHEN: 'OK'.
      PERFORM: cadastrar_ajuste.
    WHEN: 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      PERFORM: mostrar_valor.
  ENDCASE.
ENDMODULE.                 " PAI_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  COPIAR_REGISTRO
*&---------------------------------------------------------------------*
FORM copiar_registro.

  DATA: qtd_line TYPE i.

  CALL METHOD obj_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_rows.

  qtd_line = lines( it_rows ).

  IF ( qtd_line EQ 1 ).

    LOOP AT  it_rows INTO lw_rows.

      READ TABLE gt_saida INTO gw_saida INDEX lw_rows-index.


      gw_zfit0087-usuario            = sy-uname.
      gw_zfit0087-hora_registro      = sy-uzeit.
      gw_zfit0087-data_registro      = sy-datum.
      gw_zfit0087-bukrs              = gw_saida-bukrs.
      gw_zfit0087-gsber              = gw_saida-gsber.
      gw_zfit0087-kunnr              = gw_saida-kunnr.
      gw_zfit0087-tipo               = gw_saida-tipo.
      gw_zfit0087-matnr              = gw_saida-matnr.
      gw_zfit0087-belnr              = gw_saida-belnr.
      gw_zfit0087-augbl              = gw_saida-augbl.
      gw_zfit0087-budat              = gw_saida-budat.
      gw_zfit0087-augdt              = gw_saida-augdt.
      gw_zfit0087-vbel2              = gw_saida-vbel2.
      gw_zfit0087-auart              = gw_saida-auart.
      gw_zfit0087-vbeln              = gw_saida-vbeln.
      gw_zfit0087-nr_sol             = gw_saida-nr_sol.
      gw_zfit0087-tp_venda           = gw_saida-tp_venda.
      gw_zfit0087-buzei              = gw_saida-buzei.
      gw_zfit0087-dmbtr              = gw_saida-dmbtr.
      gw_zfit0087-dmbe2              = gw_saida-dmbe2.
      gw_zfit0087-tx_camb            = gw_saida-tx_camb.
      gw_zfit0087-banco_liq          = gw_saida-banco_liq.

      CASE var_opcao.
        WHEN: 'VC'.
          gw_zfit0087-visao_caixa = 'X'.
      ENDCASE.

      gw_cadastro-augbl = gw_saida-augbl.
      gw_cadastro-buzei = gw_saida-buzei.

      CLEAR: gw_bkpf_aux.
      SELECT SINGLE *
        FROM bkpf INTO gw_bkpf_aux
       WHERE bukrs = gw_saida-bukrs
         AND belnr = gw_saida-augbl
         AND gjahr = gw_saida-budat(4).

      gw_cadastro-budat = gw_bkpf_aux-budat.

      CLEAR: gw_cadastro-dmbtr.
      CLEAR: gw_saida.

    ENDLOOP.
    CALL SCREEN 0200 STARTING AT 010 2 ENDING   AT 45 05.
  ELSE.
    MESSAGE s899(mm) WITH 'Selecione somente uma linha'.
  ENDIF.

  CLEAR: qtd_line.
ENDFORM.                    " COPIAR_REGISTRO
*&---------------------------------------------------------------------*
*&      Form  CADASTRAR_AJUSTE
*&---------------------------------------------------------------------*
FORM cadastrar_ajuste.

  DATA: lt_bseg TYPE TABLE OF bseg,
        lw_bseg TYPE bseg.

  DATA: lw_zfit0087 TYPE zfit0087.
  DATA: var_text TYPE c LENGTH 100.

  SELECT SINGLE * FROM zfit0087
    INTO lw_zfit0087
    WHERE belnr EQ gw_cadastro-augbl
      AND buzei EQ gw_cadastro-buzei.

  IF ( sy-subrc EQ 0 ).
    CONCATENATE 'Doc.' lw_zfit0087-belnr ' e ' lw_zfit0087-buzei 'já cadastrado' INTO var_text SEPARATED BY space.
    MESSAGE w899(mm) WITH var_text.
  ELSE.

    READ TABLE gt_saida INTO gw_saida WITH KEY augbl = gw_cadastro-augbl
                                               buzei = gw_cadastro-buzei.
* ---> S4 Migration - 15/06/2023 - MA
*    Não tem os campos chaves
    SELECT * FROM bseg
      INTO TABLE lt_bseg
    WHERE belnr EQ gw_cadastro-augbl
      AND buzei EQ gw_cadastro-buzei.  "#EC CI_DB_OPERATION_OK[2431747]

*<--- S4 Migration - 15/06/2023 - MA

    IF ( sy-subrc EQ 0 ).

      READ TABLE lt_bseg INTO lw_bseg INDEX 1.

      CASE lw_bseg-shkzg.
        WHEN: 'H'.
          gw_zfit0087-dmbtr  = lw_bseg-dmbtr * -1.
          gw_zfit0087-dmbe2  = lw_bseg-dmbe2 * -1.
          gw_zfit0087-buzei  = gw_cadastro-buzei.
          "GW_ZFIT0087-DMBE2  = GW_ZFIT0087-DMBTR / GW_ZFIT0087-TX_CAMB.
          gw_zfit0087-tx_camb = gw_zfit0087-dmbtr / gw_zfit0087-dmbe2.
          gw_zfit0087-tx_camb = abs( gw_zfit0087-tx_camb ).
        WHEN OTHERS.
          gw_zfit0087-dmbtr  = lw_bseg-dmbtr.
          gw_zfit0087-dmbe2  = lw_bseg-dmbe2.
          gw_zfit0087-buzei  = gw_cadastro-buzei.
          "GW_ZFIT0087-DMBE2  = LW_BSEG-DMBTR / GW_ZFIT0087-TX_CAMB.
          gw_zfit0087-tx_camb = gw_zfit0087-dmbtr / gw_zfit0087-dmbe2.
      ENDCASE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gw_cadastro-buzei
        IMPORTING
          output = gw_zfit0087-buzei.

      gw_zfit0087-data_registro = sy-datum.
      gw_zfit0087-budat         = gw_cadastro-budat.
      gw_zfit0087-augdt         = gw_cadastro-budat.
      gw_zfit0087-belnr         = gw_cadastro-augbl.
      gw_zfit0087-lcto_manual   = 'X'.




      INSERT INTO zfit0087 VALUES gw_zfit0087.
      COMMIT WORK.
      WAIT UP TO 2 SECONDS.

      CLEAR: lw_zfit0087.

      CALL FUNCTION 'Z_FI_MERCADO_INTERNO'
        EXPORTING
          p_opcao      = var_opcao
        IMPORTING
          it_resultado = gt_saida
        TABLES
          it_empresa   = gt_empresa
          it_data      = gt_data
          it_cliente   = gt_cliente
          it_nr_ov     = gt_vbel2
          it_nr_sol    = gt_nr_sol.

      IF NOT ( gt_saida[] IS INITIAL ).
        CALL METHOD obj_alv->refresh_table_display.
      ENDIF.

      MESSAGE s899(mm) WITH 'Atualizado com sucesso'.

      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.
ENDFORM.                    " CADASTRAR_AJUSTE
*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_VALOR
*&---------------------------------------------------------------------*
FORM mostrar_valor .

  DATA: lt_bseg TYPE TABLE OF bseg,
        lw_bseg TYPE bseg.

* ---> S4 Migration - 15/06/2023 - MA
*Não tem os campos chave
  SELECT * FROM bseg
    INTO TABLE lt_bseg
  WHERE belnr EQ gw_cadastro-augbl
    AND buzei EQ gw_cadastro-buzei.    "#EC CI_DB_OPERATION_OK[2431747]

*<--- S4 Migration - 15/06/2023 - MA

  IF ( sy-subrc EQ 0 ).
    READ TABLE lt_bseg INTO lw_bseg INDEX 1.
    CASE lw_bseg-shkzg.
      WHEN: 'H'.
        gw_cadastro-dmbtr = lw_bseg-dmbtr * -1.
      WHEN OTHERS.
        gw_cadastro-dmbtr = lw_bseg-dmbtr.
    ENDCASE.
  ELSE.

    CLEAR: gw_cadastro-dmbtr.
    MESSAGE w899(mm) WITH 'Documento não existe.'.
  ENDIF.
  CLEAR: lw_bseg.
ENDFORM.                    " MOSTRAR_VALOR
*&---------------------------------------------------------------------*
*&      Form  COMPLEMENTO_DIVERSOS
*&---------------------------------------------------------------------*
FORM complemento_diversos .
  gw_complemento-bukrs = '0001'.
  CALL SCREEN 0300 STARTING AT 010 2 ENDING   AT 45 08.
ENDFORM.                    " COMPLEMENTO_DIVERSOS
*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.
  SET PF-STATUS 'PF0300'.
  SET TITLEBAR 'PF0300'.

ENDMODULE.                 " PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
*&---------------------------------------------------------------------*

MODULE pai_0300 INPUT.
  CASE sy-ucomm.
    WHEN: 'GRAVAR_COM'.
      PERFORM: gravar_complemento_diversos.
    WHEN: 'CANC_COM'.
      CLEAR: gw_complemento.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      PERFORM: mostra_valor_complemento.

  ENDCASE.
ENDMODULE.                 " PAI_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  GRAVAR_COMPLEMENTO_DIVERSOS
*&---------------------------------------------------------------------*
FORM gravar_complemento_diversos .

  DATA: lt_bseg TYPE TABLE OF bseg,
        lw_bseg TYPE bseg.

  DATA: lw_zfit0087 TYPE zfit0087.
  DATA: var_text TYPE c LENGTH 100.
  DATA: lw_zsdt0053 TYPE zsdt0053.
  DATA: lw_zsdt0051 TYPE zsdt0051.
  DATA: lw_zsdt0054 TYPE zsdt0054.


  IF ( gw_complemento-vbeln IS INITIAL ).
    MESSAGE w899(mm) WITH 'Informar a ordem de venda.'.
    EXIT.
  ELSEIF ( gw_complemento-kunnr IS INITIAL ).
    MESSAGE w899(mm) WITH 'Informar o cliente'.
    EXIT.
  ELSE.

    DATA: lw_kna1 TYPE kna1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gw_complemento-buzei
      IMPORTING
        output = gw_complemento-buzei.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gw_complemento-docnr
      IMPORTING
        output = gw_complemento-docnr.

    SELECT SINGLE * FROM zfit0087
      INTO lw_zfit0087
      WHERE belnr EQ gw_complemento-docnr
        AND buzei EQ gw_complemento-buzei.

    IF ( sy-subrc EQ 0 ).
      CONCATENATE 'Doc.' lw_zfit0087-belnr ' e ' lw_zfit0087-buzei 'já cadastrado' INTO var_text SEPARATED BY space.
      MESSAGE w899(mm) WITH var_text.
    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gw_complemento-kunnr
        IMPORTING
          output = gw_complemento-kunnr.

      SELECT SINGLE * FROM kna1 INTO lw_kna1 WHERE kunnr EQ gw_complemento-kunnr.

      IF ( sy-subrc NE 0 ).
        CONCATENATE 'Cliente' gw_complemento-kunnr 'não encontrado' INTO var_text SEPARATED BY space.
        MESSAGE w899(mm) WITH var_text.
      ELSE.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gw_complemento-vbeln
          IMPORTING
            output = gw_complemento-vbeln.


        IF ( p_aquav EQ abap_true ) OR ( p_prtam EQ abap_true ).
          SELECT SINGLE *
            FROM vbak INTO @DATA(_wl_vbak)
           WHERE vbeln = @gw_complemento-vbeln.

          IF sy-subrc NE 0.
            CONCATENATE 'Nº' gw_complemento-vbeln 'não encontrado' INTO var_text SEPARATED BY space.
            MESSAGE w899(mm) WITH var_text.
            RETURN.
          ENDIF.

          SELECT SINGLE *
            FROM vbap INTO @DATA(_wl_vbap)
           WHERE vbeln = @gw_complemento-vbeln.

        ENDIF.

        SELECT SINGLE * FROM zsdt0053
          INTO lw_zsdt0053
        WHERE vbeln EQ gw_complemento-vbeln.

        IF sy-subrc NE 0.
          SELECT SINGLE *
            FROM zsdt0041 INTO @DATA(wl_zsdt0041)
           WHERE vbeln EQ @gw_complemento-vbeln.

          IF sy-subrc = 0.
            SELECT SINGLE *
              FROM zsdt0040 INTO @DATA(wl_zsdt0040)
             WHERE doc_simulacao EQ @wl_zsdt0041-doc_simulacao.
          ENDIF.
        ENDIF.

        IF ( sy-subrc EQ 0 ) OR ( p_aquav EQ abap_true ) OR ( p_prtam EQ abap_true ).

          IF wl_zsdt0041 IS INITIAL.
            SELECT SINGLE * FROM zsdt0051
              INTO lw_zsdt0051
            WHERE nro_sol_ov EQ lw_zsdt0053-nro_sol_ov.

            SELECT SINGLE * FROM zsdt0054
              INTO lw_zsdt0054
             WHERE nro_sol_ov EQ lw_zsdt0053-nro_sol_ov.
          ENDIF.

          gw_zfit0087-usuario            = sy-uname.
          gw_zfit0087-hora_registro      = sy-uzeit.
          gw_zfit0087-bukrs              = gw_complemento-bukrs.
          gw_zfit0087-data_registro      = sy-datum.
          gw_zfit0087-kunnr              = lw_kna1-kunnr.
          gw_zfit0087-tipo               = 'Adiantamento'.
          gw_zfit0087-budat              = gw_complemento-budat.
          gw_zfit0087-augdt              = gw_complemento-budat.

          IF ( p_aquav EQ abap_true ) OR ( p_prtam EQ abap_true ).
            gw_zfit0087-matnr  = _wl_vbap-matnr.
            gw_zfit0087-auart  = _wl_vbak-auart.
            gw_zfit0087-vbel2  = _wl_vbak-vbeln.
          ELSE.
            IF wl_zsdt0041 IS NOT INITIAL.
              gw_zfit0087-matnr  = wl_zsdt0041-matnr.
              gw_zfit0087-auart  = wl_zsdt0041-auart.
              gw_zfit0087-vbel2  = wl_zsdt0041-vbeln.
              gw_zfit0087-nr_sol = wl_zsdt0041-doc_simulacao.
              gw_zfit0087-tpsim  = wl_zsdt0040-tpsim.
            ELSE.
              gw_zfit0087-gsber              = lw_zsdt0053-werks.
              gw_zfit0087-matnr              = lw_zsdt0053-matnr.
              gw_zfit0087-auart              = lw_zsdt0051-auart.
              gw_zfit0087-vbel2              = lw_zsdt0053-vbeln.
              gw_zfit0087-nr_sol             = lw_zsdt0051-nro_sol_ov.
              gw_zfit0087-tp_venda           = lw_zsdt0051-tp_venda.
            ENDIF.
          ENDIF.

          gw_zfit0087-lcto_manual        = 'X'.

          CASE var_opcao.
            WHEN: 'VC'.
              gw_zfit0087-visao_caixa = 'X'.
          ENDCASE.

          gw_zfit0087-belnr = gw_complemento-docnr.
          gw_zfit0087-augbl = gw_complemento-docnr.
          gw_zfit0087-buzei = gw_complemento-buzei.

          gw_zfit0087-dmbtr  = gw_complemento-hsl.
          gw_zfit0087-dmbe2  = gw_complemento-ksl.

          CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
            gw_zfit0087-tx_camb = gw_zfit0087-dmbtr / gw_zfit0087-dmbe2.
          ENDCATCH.

          gw_zfit0087-banco_liq          = '111141-Bco Brasil 9191-X'.


          INSERT INTO zfit0087 VALUES gw_zfit0087.
          COMMIT WORK.
          WAIT UP TO 2 SECONDS.

          CLEAR: lw_zfit0087.

          CALL FUNCTION 'Z_FI_MERCADO_INTERNO'
            EXPORTING
              p_opcao      = var_opcao
            IMPORTING
              it_resultado = gt_saida
            TABLES
              it_empresa   = gt_empresa
              it_data      = gt_data
              it_cliente   = gt_cliente
              it_nr_ov     = gt_vbel2
              it_nr_sol    = gt_nr_sol.

          IF NOT ( gt_saida[] IS INITIAL ).
            CALL METHOD obj_alv->refresh_table_display.
          ENDIF.

          MESSAGE s899(mm) WITH 'Atualizado com sucesso'.

          LEAVE TO SCREEN 0.

        ELSE.
          CONCATENATE 'Nº' gw_complemento-vbeln 'não encontrado' INTO var_text SEPARATED BY space.
          MESSAGE w899(mm) WITH var_text.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR: gw_complemento, lw_zsdt0053, lw_zsdt0051, lw_zfit0087, var_text.

ENDFORM.                    " GRAVAR_COMPLEMENTO_DIVERSOS
*&---------------------------------------------------------------------*
*&      Form  MOSTRA_VALOR_COMPLEMENTO
*&---------------------------------------------------------------------*
FORM mostra_valor_complemento .

  DATA: gw_faglflexa TYPE faglflexa.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gw_complemento-docnr
    IMPORTING
      output = gw_complemento-docnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gw_complemento-buzei
    IMPORTING
      output = gw_complemento-buzei.

  SELECT SINGLE * FROM faglflexa
    INTO gw_faglflexa
  WHERE rbukrs EQ gw_complemento-bukrs
    AND docnr  EQ gw_complemento-docnr
    AND buzei  EQ gw_complemento-buzei
    AND rldnr  EQ '0L'.

  IF ( sy-subrc EQ 0 ).
    gw_complemento-hsl   = abs( gw_faglflexa-hsl ).
    gw_complemento-ksl   = abs( gw_faglflexa-ksl ).
    gw_complemento-budat = gw_faglflexa-budat.
  ELSE.
    MESSAGE w899(mm) WITH 'Documento não existe.'.
  ENDIF.

  CLEAR:gw_faglflexa.
ENDFORM.                    " MOSTRA_VALOR_COMPLEMENTO
*&---------------------------------------------------------------------*
*&      Form  GERAR_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_estorno .

  DATA: var_answer    TYPE c,
        qtd_line      TYPE i,
        vl_msg_exibir TYPE string.

  CALL METHOD obj_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_rows.

  qtd_line = lines( it_rows ).

  CHECK qtd_line > 0.
*
*  IF ( QTD_LINE > 1 ).
*    MESSAGE S899(MM) WITH 'Selecione somente uma linha'.
*    EXIT.
*  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente estornar esse registro?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_rows INTO lw_rows.

    READ TABLE gt_saida INTO gw_saida INDEX lw_rows-index.

    IF sy-subrc NE 0.
      CONCATENATE  'Não foi possível gerar o estorno!'
                   '(Nr.Doc.' gw_saida-belnr ')'
             INTO vl_msg_exibir SEPARATED BY space.
      MESSAGE vl_msg_exibir TYPE 'S'.
      RETURN.
    ENDIF.

    CLEAR: gw_zfit0087.
    SELECT SINGLE *
      FROM zfit0087 INTO gw_zfit0087
     WHERE belnr   = gw_saida-belnr
       AND buzei   = gw_saida-buzei
       AND bukrs   = gw_saida-bukrs
       AND estorno = 'X'.

    IF sy-subrc = 0.
      CONCATENATE  'Lançamento já estornado!'
                   '(Nr.Doc.' gw_saida-belnr ')'
             INTO vl_msg_exibir SEPARATED BY space.
      MESSAGE vl_msg_exibir TYPE 'S'.
      RETURN.
    ENDIF.

    CLEAR: gw_zfit0087.
    SELECT SINGLE *
      FROM zfit0087 INTO gw_zfit0087
     WHERE belnr   = gw_saida-belnr
       AND buzei   = gw_saida-buzei
       AND bukrs   = gw_saida-bukrs
       AND estorno = ''.

    IF sy-subrc NE 0.
      CONCATENATE  'Documento não encontrado para geração do estorno!'
                   '(Nr.Doc.' gw_saida-belnr ')'
             INTO vl_msg_exibir SEPARATED BY space.
      MESSAGE vl_msg_exibir TYPE 'S'.
      RETURN.
    ENDIF.

*    IF GW_ZFIT0087-LCTO_MANUAL IS INITIAL.
*      CONCATENATE  'Selecione um lançamento manual!'
*                   '(Nr.Doc.' GW_SAIDA-BELNR ')'
*             INTO VL_MSG_EXIBIR SEPARATED BY SPACE.
*      MESSAGE VL_MSG_EXIBIR TYPE 'S'.
*      RETURN.
*    ENDIF.

    gw_zfit0087-estorno = 'X'.
    gw_zfit0087-dmbtr = gw_zfit0087-dmbtr * -1 .
    gw_zfit0087-dmbe2 = gw_zfit0087-dmbe2 * -1 .
    gw_zfit0087-data_registro = sy-datum.
    gw_zfit0087-hora_registro = sy-uzeit.
    gw_zfit0087-usuario       = sy-uname.
    MODIFY zfit0087 FROM gw_zfit0087.

    IF sy-subrc NE 0.
      CONCATENATE  'Houve um erro ao gerar o estorno!'
                   '(Nr.Doc.' gw_saida-belnr ')'
             INTO vl_msg_exibir SEPARATED BY space.
      MESSAGE vl_msg_exibir TYPE 'S'.
      RETURN.
    ENDIF.

  ENDLOOP.

  MESSAGE s899(mm) WITH 'Estorno gerado com sucesso!'.

  CALL FUNCTION 'Z_FI_MERCADO_INTERNO'
    EXPORTING
      p_opcao      = var_opcao
    IMPORTING
      it_resultado = gt_saida
    TABLES
      it_empresa   = gt_empresa
      it_data      = gt_data
      it_cliente   = gt_cliente
      it_nr_ov     = gt_vbel2
      it_nr_sol    = gt_nr_sol.

  IF NOT ( gt_saida[] IS INITIAL ).
    CALL METHOD obj_alv->refresh_table_display.
  ENDIF.

ENDFORM.
