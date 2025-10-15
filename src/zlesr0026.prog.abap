*&--------------------------------------------------------------------&*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Victor Hugo                                             &*
*& Data.....: 01/08/2013                                              &*
*& Descrição: Relatório de fretes pagos                               &*
*& Transação: ZLESR0026                                               &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT  zlesr0026.

TABLES: j_1bnfdoc, lfa1, zib_cte_dist_ter, sscrfields.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*

TYPE-POOLS: slis, icon.

TYPES: BEGIN OF ty_fields_0101,
         field TYPE c LENGTH 30.
TYPES  END OF ty_fields_0101.

TYPES: BEGIN OF ty_zles0105_ajuste,
         buzei TYPE bsak-buzei.
         INCLUDE STRUCTURE zlest0105.
TYPES  END OF ty_zles0105_ajuste.

TYPES:
*       BEGIN OF TY_BSAK,
*          BUKRS   TYPE BSAK-BUKRS,
*          AUGDT   TYPE BSAK-AUGDT,
*          BLART   TYPE BSAK-BLART,
*          SHKZG   TYPE BSAK-SHKZG,
*          AUGBL   TYPE BSAK-AUGBL,
*          BELNR   TYPE BSAK-BELNR,
*          GJAHR   TYPE BSAK-GJAHR,
*          DMBTR   TYPE BSAK-DMBTR,
*          DMBE2   TYPE BSAK-DMBE2,
*          MONAT   TYPE BSAK-MONAT,
*          XBLNR   TYPE BSAK-XBLNR,
*          LIFNR   TYPE BSAK-LIFNR,
*          ZFBDT   TYPE BSAK-ZFBDT,
*          ZBD1T   TYPE BSAK-ZBD1T,
*          SGTXT   TYPE BSAK-SGTXT,
*          BUDAT   TYPE BSAK-BUDAT,
*          UMSKS   TYPE BSAK-UMSKS,
*          EBELN   TYPE BSAK-EBELN,
*          GSBER   TYPE BSAK-GSBER,
*          NM_LOTE TYPE ZPFE_LOTE_ITEM-NM_LOTE,
*        END OF TY_BSAK,
*
*        BEGIN OF TY_ZLEST0032,
*          BELNR  TYPE ZLEST0032-BELNR,
*          TKNUM  TYPE ZLEST0032-TKNUM,
*          ADD03  TYPE ZLEST0032-ADD03,
*          FKNUM  TYPE ZLEST0032-FKNUM,
*          DOCNUM TYPE ZLEST0032-DOCNUM,
*          REFKEY TYPE J_1BNFLIN-REFKEY,
*        END OF TY_ZLEST0032,
*
*        BEGIN OF TY_J_1BNFDOC,
*          DOCNUM    TYPE J_1BNFDOC-DOCNUM,
*          NFENUM    TYPE J_1BNFDOC-NFENUM,
*          NFNUM     TYPE J_1BNFDOC-NFNUM,
*          SERIES    TYPE J_1BNFDOC-SERIES,
*          NFE       TYPE J_1BNFDOC-NFE,
*          NFE_SERIE TYPE STRING,
*        END OF TY_J_1BNFDOC,
*
*        BEGIN OF TY_BKPF,
*          AWKEY TYPE BKPF-AWKEY,
*          BKTXT TYPE BKPF-BKTXT,
*          BUKRS TYPE BKPF-BUKRS,
*          BELNR TYPE BKPF-BELNR,
*          GJAHR TYPE BKPF-GJAHR,
*          BLART TYPE BKPF-BLART,
*          STBLG TYPE BKPF-STBLG,
*          RE_BELNR TYPE ZLEST0034-RE_BELNR,
*          RE_GJAHR TYPE ZLEST0034-RE_GJAHR,
*        END OF TY_BKPF,

  BEGIN OF ty_saida,
    bukrs            TYPE bsak-bukrs,
    augdt            TYPE bsak-augdt,
    monat            TYPE bsak-monat,
    gjahr            TYPE bsak-gjahr,
    augbl            TYPE bsak-augbl,
    belnr            TYPE bsak-belnr,
    blart            TYPE bsak-blart,
    miro             TYPE bkpf-awkey,
    lifnr            TYPE bsak-lifnr,
    doc_fiscal       TYPE bsak-xblnr,
    docnum           TYPE j_1bnflin-docnum,
    tknum            TYPE zpfe_lote_item-tknum,
    fknum            TYPE vfkp-fknum,
    modal            TYPE vttk-vsart,
    add03            TYPE zlest0032-add03,
    matnr            TYPE lips-matnr,
    maktx            TYPE makt-maktx,
    matkl            TYPE mara-matkl,
    wgbez            TYPE t023t-wgbez,
    peso             TYPE zpfe_lote_item-peso_chegada,
    zpeso_origem     TYPE zlest0034-zpeso_origem,
    gewei            TYPE lips-gewei,
    vl_pago_lote     TYPE zpfe_lote_item-vl_pago_lote,
    dmbe2            TYPE bsak-dmbe2,
    xblnr            TYPE bsak-xblnr,
    chvid            TYPE zpfe_lote_item-chvid,
    bktxt            TYPE bkpf-bktxt,
    zfbdt            TYPE bsak-zfbdt,
    ebeln            TYPE zfit0045-ebeln,
    name1            TYPE lfa1-name1,
    budat            TYPE bsak-budat,
    dmbtr            TYPE bsak-dmbtr,
    sgtxt            TYPE bsak-sgtxt,
    belnr_fat        TYPE bsak-belnr,
    augbl_fat        TYPE bsak-augbl,
    budat_fat        TYPE bsak-budat,
    dmbtr_fat        TYPE bsak-dmbtr,
    dmbe2_fat        TYPE bsak-dmbe2,
    banco_liq        TYPE skat-txt50,
    gsber            TYPE bsak-gsber,
    bsart            TYPE ekko-bsart,
    charg            TYPE lips-charg,
    tx_camb          TYPE zlest0061-tax_dolar,
    exc_est          TYPE char04,
*---> 20.06.2023 - Migração S4 - DG
    "    tipo             TYPE char02,
    tipo             TYPE zchar02,
*<--- 20.06.2023 - Migração S4 - DG
    inc_manual       TYPE zlest0105-inc_manual,
    bezei            TYPE c LENGTH 40,
    vlr_cte          TYPE j_1bnfdoc-nftot,
    vlr_quebra       TYPE zpfe_lote_item-vl_transacao,
    vlr_perda        TYPE zpfe_lote_item-vl_transacao,
    vlr_quebra_perda TYPE zpfe_lote_item-vl_transacao,
    vlr_tarifa_pgto  TYPE konp-kbetr,
    tp_frete         TYPE c,
    ctenum           TYPE j_1bnfdoc-nfenum,
    ebeln_origem     TYPE zlest0105-ebeln_origem,
    bsart_origem     TYPE zlest0105-bsart_origem,
*-CS2022000256 - 24.03.2022 - JT - inicio
    ktokk            TYPE lfa1-ktokk,
    kostl            TYPE zlest0217-kostl,
    prctr            TYPE zlest0217-prctr,
    matnr_serv       TYPE zlest0217-matnr_serv,
    stcd1            TYPE zlest0218-stcd1,
*-CS2022000256 - 24.03.2022 - JT - fim
  END OF ty_saida.


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.


DATA: tg_fields_0101 TYPE TABLE OF ty_fields_0101 WITH HEADER LINE.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: gt_saida          TYPE TABLE OF ty_saida.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: gw_bsak            TYPE bsak,
      gw_saida           TYPE ty_saida,
      gw_zles0105_ajuste TYPE ty_zles0105_ajuste.

*----------------------------------------------------------------------*
* VARIAVEIS
*----------------------------------------------------------------------*
DATA: var_belnr_gjahr TYPE c LENGTH 14.
DATA: var_bukrs   TYPE bsad-bukrs,
      var_bukrs_h TYPE bsad-bukrs,
      var_augdt   TYPE bsad-augdt,
      var_augdt_h TYPE bsad-augdt,
      var_visao   TYPE zde_visao_zles0079,
      var_opcao   TYPE c LENGTH 10.

DATA: gt_empresa     TYPE TABLE OF t001,
      gt_empresa_job TYPE TABLE OF t001 WITH HEADER LINE,
      gt_data        TYPE TABLE OF bsad.

DATA: gw_empresa TYPE t001,
      gw_data    TYPE bsad.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader.

DATA: variante         LIKE disvariant.
DATA: gs_variant_c TYPE disvariant.

*-CS2022000256 - 24.03.2022 - JT - inicio
DATA: l_sel_button TYPE smp_dyntxt,
      l_param      TYPE   usr05-parva.
*-CS2022000256 - 24.03.2022 - JT - fim

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: s_bukrs  FOR gw_bsak-bukrs NO INTERVALS NO-EXTENSION,
                    s_augdt  FOR gw_bsak-augdt NO-EXTENSION.

    SELECT-OPTIONS: s_lifnr  FOR lfa1-lifnr MODIF ID v2,
                    s_nmcte  FOR j_1bnfdoc-nfenum MODIF ID v2,
                    s_modal  FOR zib_cte_dist_ter-cd_modal MODIF ID v2.

    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: s_fprop  AS CHECKBOX MODIF ID v2 DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 03(14) TEXT-i03 MODIF ID v2.

      PARAMETERS: s_fterc  AS CHECKBOX MODIF ID v2 DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 20(20) TEXT-i04 MODIF ID v2.
    SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN: END OF BLOCK b1.

  SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-003.
    PARAMETER: p_varia TYPE disvariant-variant.
  SELECTION-SCREEN: END OF BLOCK b5.
SELECTION-SCREEN: END OF BLOCK a1.

SELECTION-SCREEN: BEGIN OF BLOCK b6 WITH FRAME.
  PARAMETERS: r_pag    RADIOBUTTON GROUP r1 DEFAULT 'X' MODIF ID v1,
              r_com    RADIOBUTTON GROUP r1             MODIF ID v1,
              "R_PED    RADIOBUTTON GROUP R1            MODIF ID V1 ,
              r_ad_cx  RADIOBUTTON GROUP r1             MODIF ID v1,
              "R_AD_LQ  RADIOBUTTON GROUP R1            MODIF ID V1,
              r_ad_pi  RADIOBUTTON GROUP r1             MODIF ID v1,
              r_ev_prt RADIOBUTTON GROUP r1            MODIF ID v1.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: r_aquav  RADIOBUTTON GROUP r1             MODIF ID v1.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i01              MODIF ID v1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: r_aq_pa  RADIOBUTTON GROUP r1             MODIF ID v1.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i02              MODIF ID v1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: r_fprop  RADIOBUTTON GROUP r1             MODIF ID v1.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i05              MODIF ID v1.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b6.

*-CS2022000256 - 24.03.2022 - JT - inicio
SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'
SELECTION-SCREEN FUNCTION KEY 2.  "Will have a function code of 'FC02'
*-CS2022000256 - 24.03.2022 - JT - inicio

INITIALIZATION.
  gs_variant_c-report      = sy-repid.

*-CS2022000256 - 24.03.2022 - JT - inicio
  CALL FUNCTION 'G_GET_USER_PARAMETER'
    EXPORTING
      parameter_id    = 'ZLES0079_PAR'
    IMPORTING
      parameter_value = l_param.

  IF l_param = '*'.
    l_sel_button-icon_id   = icon_packing.
    l_sel_button-icon_text = 'Parametrização Centro de Custo'.
    sscrfields-functxt_01  = l_sel_button.

    l_sel_button-icon_id   = icon_packing.
    l_sel_button-icon_text = 'Parametrização CNPJ'.
    sscrfields-functxt_02  = l_sel_button.
  ENDIF.
*-CS2022000256 - 24.03.2022 - JT - fim

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
*
  DATA: vg_repid   LIKE sy-repid,
        vg_variant TYPE disvariant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid          = sy-repid.
  variante-report = vg_repid.

  IF ( NOT p_varia IS INITIAL ).
    vg_variant-variant = p_varia.

  ENDIF.
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



AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CHECK screen-group1 IS NOT INITIAL.

    CASE sy-tcode .
      WHEN 'ZLES0167'.
        IF ( screen-group1 EQ 'V1' ).
          IF ( screen-name NE 'R_PAG' ) AND
             ( screen-name NE '%_R_PAG_%_APP_%-TEXT' ).
            screen-active = '0'.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      WHEN 'ZLES0079'.
        IF ( screen-group1 EQ 'V2' ).
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.

    ENDCASE.
  ENDLOOP.

*-CS2022000256 - 24.03.2022 - JT - inicio
AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'FC01'.
      CALL TRANSACTION 'ZLES0202'.
    WHEN 'FC02'.
      CALL TRANSACTION 'ZLES0203'.
  ENDCASE.
*-CS2022000256 - 24.03.2022 - JT - fim

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: vg_job      TYPE i.

  IF sy-batch = 'X'.

    SELECT SINGLE COUNT( * ) INTO vg_job
      FROM tbtco
     WHERE jobname EQ 'ZLES0079_JOB'
       AND status EQ 'R'.

    IF ( vg_job NE 1 ).
      RETURN.
    ENDIF.

  ENDIF.

  var_bukrs   =  s_bukrs-low.
  var_bukrs_h =  s_bukrs-high.
  var_augdt   =  s_augdt-low.
  var_augdt_h =  s_augdt-high.

  IF sy-batch EQ abap_false.

    IF s_bukrs-low IS INITIAL.
      MESSAGE 'Empresa é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF s_augdt-low IS INITIAL.
      MESSAGE 'Data Compensação é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.

    gw_empresa-bukrs = s_bukrs-low.
    APPEND gw_empresa TO gt_empresa.

    IF NOT ( s_bukrs-high IS INITIAL ).
      gw_empresa-bukrs = s_bukrs-high.
      APPEND gw_empresa TO gt_empresa.
    ENDIF.

    gw_data-augdt = s_augdt-low.
    APPEND gw_data TO gt_data.

    IF NOT ( s_augdt-high IS INITIAL ).
      gw_data-augdt = s_augdt-high.
      APPEND gw_data TO gt_data.
    ENDIF.

  ENDIF.

  "Perform Utilizado para selecionar informações do Adiantamento Caixa.
  "Não foi utilizado a seleção do perform SELECIONAR_DADOS porque este
  "fluxo é diferente, partindo primeiro na seleção de um SET e tornando
  "a tabela principal ZLEST0045.

  CASE abap_true.
    WHEN r_pag.
      var_visao = '01'.
      var_opcao = 'PAG'.
    WHEN r_com.
      var_visao = '02'.
      var_opcao = 'COM'.
    WHEN r_ad_cx.
      var_visao = '03'.
      var_opcao = 'R_AD_CX'.
    WHEN r_ad_pi.
      var_visao = '04'.
      var_opcao = 'R_AD_PI'.
    WHEN r_ev_prt.
      var_visao = '05'.
      var_opcao = 'R_EV_PRT'.
    WHEN r_aquav.
      var_visao = '06'.
      var_opcao = 'R_AQUAV'.
    WHEN r_aq_pa.
      var_visao = '07'.
      var_opcao = 'R_AQ_PA'.
    WHEN r_fprop.
      var_visao = '08'.
      var_opcao = 'R_FPROP'.
  ENDCASE.

  IF sy-batch = 'X'.

    "Execução por JOB


    "LES - ZLES0079 - Transação Parametros US #163332 - WPP -->>>
    DATA: lit_zlest0252 TYPE TABLE OF zlest0252.

    CLEAR: lit_zlest0252[].
    SELECT *
      FROM zlest0252 INTO TABLE lit_zlest0252.

    DATA(lit_zlest0252_group) = lit_zlest0252[].
    SORT lit_zlest0252_group BY operacao.
    DELETE ADJACENT DUPLICATES FROM lit_zlest0252_group COMPARING operacao.

    LOOP AT lit_zlest0252_group ASSIGNING FIELD-SYMBOL(<fs_zlest0252_group>).

      CLEAR: gt_empresa_job[], gt_data[].

      LOOP AT lit_zlest0252 ASSIGNING FIELD-SYMBOL(<fs_zlest0252>) WHERE operacao = <fs_zlest0252_group>-operacao.
        APPEND VALUE #( bukrs = <fs_zlest0252>-bukrs ) TO gt_empresa_job.
      ENDLOOP.

      CHECK gt_empresa_job[] IS NOT INITIAL.

      CASE <fs_zlest0252_group>-operacao.
        WHEN 'FP'.    "Pagamento Frete

          var_opcao = 'PAG'.

          APPEND VALUE #( augdt = sy-datum - 2  ) TO gt_data.
          APPEND VALUE #( augdt = sy-datum      ) TO gt_data.

        WHEN 'CP'.    "Compensações Frete
          CONTINUE.
        WHEN 'FA'.    "Adiantamento Caixa - Frete

          var_opcao = 'R_AD_CX'.

          APPEND VALUE #( augdt = sy-datum - 2  ) TO gt_data.
          APPEND VALUE #( augdt = sy-datum      ) TO gt_data.

        WHEN 'IA_IP'. "Pagamento\Adiantamento Insumos - Caixa

          var_opcao = 'R_AD_PI'.

          APPEND VALUE #( augdt = sy-datum - 2  ) TO gt_data.
          APPEND VALUE #( augdt = sy-datum      ) TO gt_data.

        WHEN 'EA_EP'. "Pagamento Elevação Portuária

          var_opcao = 'R_EV_PRT'.

          APPEND VALUE #( augdt = sy-datum - 15 ) TO gt_data.
          APPEND VALUE #( augdt = sy-datum      ) TO gt_data.

        WHEN 'AQ'.    "Pagamento Frete Aquav./Armazenagem

          var_opcao = 'R_AQUAV'.

          APPEND VALUE #( augdt = sy-datum - 2 ) TO gt_data.
          APPEND VALUE #( augdt = sy-datum     ) TO gt_data.

        WHEN 'PA'.    "Pagamento Armaz.Portochuelo Amaggi

          var_opcao = 'R_AQ_PA'.

          APPEND VALUE #( augdt = sy-datum - 2 ) TO gt_data.
          APPEND VALUE #( augdt = sy-datum     ) TO gt_data.

        WHEN 'PR'.    "Frete Próprio

          var_opcao = 'R_FPROP'.

          APPEND VALUE #( augdt = sy-datum - 2  ) TO gt_data.
          APPEND VALUE #( augdt = sy-datum      ) TO gt_data.

      ENDCASE.

      LOOP AT gt_empresa_job.

        CLEAR: gt_empresa[].
        APPEND VALUE #( bukrs = gt_empresa_job-bukrs ) TO gt_empresa.

        CALL FUNCTION 'Z_LES_FRETE_PAGOS'
          EXPORTING
            p_opcao      = var_opcao
          IMPORTING
            it_resultado = gt_saida
          TABLES
            it_empresa   = gt_empresa
            it_data      = gt_data.

      ENDLOOP.

    ENDLOOP.
    "LES - ZLES0079 - Transação Parametros US #163332 - WPP <<---


*
**-----------------------------------------------------------------------------------*
**   Pagamento Frete
**-----------------------------------------------------------------------------------*
*    "Definição Empresas
*    CLEAR: gt_empresa_job[], gt_data[].
*
*    APPEND VALUE #( bukrs = '0001' ) TO gt_empresa_job.
*    APPEND VALUE #( bukrs = '0032' ) TO gt_empresa_job.
*
*
*    "Definição Datas
*    APPEND VALUE #( augdt = sy-datum - 2  ) TO gt_data.
*    APPEND VALUE #( augdt = sy-datum      ) TO gt_data.
*
*    LOOP AT gt_empresa_job.
*
*      CLEAR: gt_empresa[], gw_empresa.
*      gw_empresa-bukrs = gt_empresa_job-bukrs.
*      APPEND gw_empresa TO gt_empresa.
*
*      var_opcao = 'PAG'.
*
*      CALL FUNCTION 'Z_LES_FRETE_PAGOS'
*        EXPORTING
*          p_opcao      = var_opcao
*        IMPORTING
*          it_resultado = gt_saida
*        TABLES
*          it_empresa   = gt_empresa
*          it_data      = gt_data.
*
*    ENDLOOP.
*
**-----------------------------------------------------------------------------------*
**   Compensações Frete
**-----------------------------------------------------------------------------------*
*
**-----------------------------------------------------------------------------------*
**   Adiantamento Caixa - Frete
**-----------------------------------------------------------------------------------*
*
*    "Definição Empresas
*    CLEAR: gt_empresa_job[], gt_data[].
*
*    APPEND VALUE #( bukrs = '0001' ) TO gt_empresa_job.
*    APPEND VALUE #( bukrs = '0032' ) TO gt_empresa_job.
*
*    "Definição Datas
*    APPEND VALUE #( augdt = sy-datum - 2  ) TO gt_data.
*    APPEND VALUE #( augdt = sy-datum      ) TO gt_data.
*
*    LOOP AT gt_empresa_job.
*
*      CLEAR: gt_empresa[], gw_empresa.
*      gw_empresa-bukrs = gt_empresa_job-bukrs.
*      APPEND gw_empresa TO gt_empresa.
*
*      var_opcao = 'R_AD_CX'.
*
*      CALL FUNCTION 'Z_LES_FRETE_PAGOS'
*        EXPORTING
*          p_opcao      = var_opcao
*        IMPORTING
*          it_resultado = gt_saida
*        TABLES
*          it_empresa   = gt_empresa
*          it_data      = gt_data.
*
*    ENDLOOP.
*
**-----------------------------------------------------------------------------------*
**   Pgto.\Adiant. Insumos - Caixa
**-----------------------------------------------------------------------------------*
*
*    "Definição Empresas
*    CLEAR: gt_empresa_job[], gt_data[].
*
*    APPEND VALUE #( bukrs = '0001' ) TO gt_empresa_job.
*
*    "Definição Datas
*    APPEND VALUE #( augdt = sy-datum - 2  ) TO gt_data.
*    APPEND VALUE #( augdt = sy-datum      ) TO gt_data.
*
*    LOOP AT gt_empresa_job.
*
*      CLEAR: gt_empresa[], gw_empresa.
*      gw_empresa-bukrs = gt_empresa_job-bukrs.
*      APPEND gw_empresa TO gt_empresa.
*
*      var_opcao = 'R_AD_PI'.
*
*      CALL FUNCTION 'Z_LES_FRETE_PAGOS'
*        EXPORTING
*          p_opcao      = var_opcao
*        IMPORTING
*          it_resultado = gt_saida
*        TABLES
*          it_empresa   = gt_empresa
*          it_data      = gt_data.
*
*    ENDLOOP.
*
**-----------------------------------------------------------------------------------*
**   Pagamento Elevação Portuária
**-----------------------------------------------------------------------------------*
*
*    "Definição Empresas
*    CLEAR: gt_empresa_job[], gt_data[].
*
*    APPEND VALUE #( bukrs = '0001' ) TO gt_empresa_job.
*    APPEND VALUE #( bukrs = '0004' ) TO gt_empresa_job.
*    APPEND VALUE #( bukrs = '0032' ) TO gt_empresa_job.
*
*    "Definição Datas
*    APPEND VALUE #( augdt = sy-datum - 15 ) TO gt_data.
*    APPEND VALUE #( augdt = sy-datum      ) TO gt_data.
*
*    LOOP AT gt_empresa_job.
*
*      CLEAR: gt_empresa[], gw_empresa.
*      gw_empresa-bukrs = gt_empresa_job-bukrs.
*      APPEND gw_empresa TO gt_empresa.
*
*      var_opcao = 'R_EV_PRT'.
*
*      CALL FUNCTION 'Z_LES_FRETE_PAGOS'
*        EXPORTING
*          p_opcao      = var_opcao
*        IMPORTING
*          it_resultado = gt_saida
*        TABLES
*          it_empresa   = gt_empresa
*          it_data      = gt_data.
*
*    ENDLOOP.
*
**-----------------------------------------------------------------------------------*
**   Pagamento Frete Aquav./Transb.-Hermasa
**   Pagamento Portochuelo-Amaggi
**-----------------------------------------------------------------------------------*
*
*    "Definição Empresas
*    CLEAR: gt_empresa_job[], gt_data[].
*
*    APPEND VALUE #( bukrs = '0001' ) TO gt_empresa_job.
*    APPEND VALUE #( bukrs = '0010' ) TO gt_empresa_job.
*    APPEND VALUE #( bukrs = '0015' ) TO gt_empresa_job.
*
*    "Definição Datas
*    APPEND VALUE #( augdt = sy-datum - 2 ) TO gt_data.
*    APPEND VALUE #( augdt = sy-datum     ) TO gt_data.
*
*    LOOP AT gt_empresa_job.
*
*      CLEAR: gt_empresa[], gw_empresa.
*      gw_empresa-bukrs = gt_empresa_job-bukrs.
*      APPEND gw_empresa TO gt_empresa.
*
*      var_opcao = 'R_AQUAV'.
*
*      CALL FUNCTION 'Z_LES_FRETE_PAGOS'
*        EXPORTING
*          p_opcao      = var_opcao
*        IMPORTING
*          it_resultado = gt_saida
*        TABLES
*          it_empresa   = gt_empresa
*          it_data      = gt_data.
*
*      var_opcao = 'R_AQ_PA'.
*
*      CALL FUNCTION 'Z_LES_FRETE_PAGOS'
*        EXPORTING
*          p_opcao      = var_opcao
*        IMPORTING
*          it_resultado = gt_saida
*        TABLES
*          it_empresa   = gt_empresa
*          it_data      = gt_data.
*    ENDLOOP.
*
**-----------------------------------------------------------------------------------*
**   Frete Proprio
**-----------------------------------------------------------------------------------*
*
*    "Definição Empresas
*    CLEAR: gt_empresa_job[], gt_data[].
*
*    APPEND VALUE #( bukrs = '0001' ) TO gt_empresa_job.
*
*    "Definição Datas
*    APPEND VALUE #( augdt = sy-datum - 2  ) TO gt_data.
*    APPEND VALUE #( augdt = sy-datum      ) TO gt_data.
*
*    LOOP AT gt_empresa_job.
*
*      CLEAR: gt_empresa[], gw_empresa.
*      gw_empresa-bukrs = gt_empresa_job-bukrs.
*      APPEND gw_empresa TO gt_empresa.
*
*      var_opcao = 'R_FPROP'.
*
*      CALL FUNCTION 'Z_LES_FRETE_PAGOS'
*        EXPORTING
*          p_opcao      = var_opcao
*        IMPORTING
*          it_resultado = gt_saida
*        TABLES
*          it_empresa   = gt_empresa
*          it_data      = gt_data.
*
*    ENDLOOP.

  ELSE.

    IF sy-tcode EQ 'ZLES0079'.

      "Check de permissão de visão
      AUTHORITY-CHECK OBJECT 'ZLES0079'
        ID 'ZVS_000001' FIELD var_visao.

      IF sy-subrc <> 0.
        SELECT SINGLE *
          FROM user_addrp INTO @DATA(wl_user)
         WHERE bname = @sy-uname.

        IF ( sy-subrc = 0 ) AND ( wl_user-name_first IS NOT INITIAL ).
          MESSAGE | { wl_user-name_first }, seu perfil está sem acesso ao tipo de visão selecionada! | TYPE 'S'.
        ELSE.
          MESSAGE | Perfil do usuário sem acesso ao tipo de visão! | TYPE 'S'.
        ENDIF.

        EXIT.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'Z_LES_FRETE_PAGOS'
      EXPORTING
        p_opcao      = var_opcao
      IMPORTING
        it_resultado = gt_saida
      TABLES
        it_empresa   = gt_empresa
        it_data      = gt_data.

    LOOP AT s_nmcte.
      IF s_nmcte-low IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = s_nmcte-low
          IMPORTING
            output = s_nmcte-low.
      ENDIF.

      IF s_nmcte-high IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = s_nmcte-high
          IMPORTING
            output = s_nmcte-high.
      ENDIF.

      MODIFY s_nmcte.
    ENDLOOP.

    DELETE gt_saida WHERE lifnr  NOT IN s_lifnr.
    DELETE gt_saida WHERE ctenum NOT IN s_nmcte.
    DELETE gt_saida WHERE modal  NOT IN s_modal.

    IF ( s_fterc EQ abap_false ) AND ( s_fprop EQ abap_true ).
      DELETE gt_saida WHERE tp_frete NE 'P'. "Proprio
    ENDIF.

    IF ( s_fterc EQ abap_true ) AND ( s_fprop EQ abap_false ).
      DELETE gt_saida WHERE tp_frete NE 'T'. "Terceiro
    ENDIF.

    PERFORM iniciar_variaveis.
    PERFORM imprimir_dados.

  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
FORM imprimir_dados .
  DATA: wl_layout TYPE slis_layout_alv.


  PERFORM definir_eventos.
  PERFORM montar_layout.

  wl_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = v_report
      is_variant               = gs_variant_c
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'PF_STATUS_SET'
      it_fieldcat              = estrutura[]
      is_layout                = wl_layout
      i_save                   = 'X'
      it_events                = events
      is_print                 = t_print
    TABLES
      t_outtab                 = gt_saida.

ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*

FORM user_command  USING r_ucomm      LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.

  DATA ref1 TYPE REF TO cl_gui_alv_grid.

  CASE r_ucomm.
    WHEN: '&IC1'.

      IF ( rs_selfield-fieldname EQ 'AUGBL').
        READ TABLE gt_saida INTO gw_saida INDEX rs_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD gw_saida-augbl.
        SET PARAMETER ID 'BUK' FIELD gw_saida-bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSEIF ( rs_selfield-fieldname EQ 'BELNR' ).
        READ TABLE gt_saida INTO gw_saida INDEX rs_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD gw_saida-belnr.
        SET PARAMETER ID 'BUK' FIELD gw_saida-bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ELSEIF ( rs_selfield-fieldname EQ 'EXC_EST' ).
        READ TABLE gt_saida INTO gw_saida INDEX rs_selfield-tabindex.
        CHECK sy-subrc = 0.
        PERFORM f_estornar_lcto USING gw_saida rs_selfield-tabindex.

      ELSEIF ( rs_selfield-fieldname EQ 'EBELN' ).
        READ TABLE gt_saida INTO gw_saida INDEX rs_selfield-tabindex.
        CHECK ( sy-subrc = 0 ) AND  ( gw_saida-ebeln IS NOT INITIAL ).
        SET PARAMETER ID 'BES' FIELD gw_saida-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ELSEIF ( rs_selfield-fieldname EQ 'EBELN_ORIGEM' ).
        READ TABLE gt_saida INTO gw_saida INDEX rs_selfield-tabindex.
        CHECK ( sy-subrc = 0 ) AND  ( gw_saida-ebeln_origem IS NOT INITIAL ).
        SET PARAMETER ID 'BES' FIELD gw_saida-ebeln_origem.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

  CASE r_ucomm.
    WHEN 'AJUS_LCTO'.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR' "<-- Gets the reference of ALV grid object from REUSE functions
        IMPORTING
          e_grid = ref1.

      IF ref1 IS NOT INITIAL.

        CLEAR: gw_saida, gw_zles0105_ajuste, it_sel_rows[], wa_sel_rows.

        CALL METHOD ref1->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE  'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF lines( it_sel_rows[] ) > 1.
          MESSAGE  'Selecione somente uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

        READ TABLE gt_saida INTO gw_saida INDEX wa_sel_rows-index.
        CHECK sy-subrc = 0.

        MOVE-CORRESPONDING gw_saida TO gw_zles0105_ajuste.

        CALL SCREEN 0102 STARTING AT 02 02.

        MOVE-CORRESPONDING gw_zles0105_ajuste TO gw_saida.

        MODIFY gt_saida FROM gw_saida INDEX wa_sel_rows-index.

        CALL METHOD ref1->refresh_table_display.

      ENDIF.

    WHEN 'INC_LCTO'.
      CLEAR: gw_zles0105_ajuste.
      CALL SCREEN 0101 STARTING AT 02 02 ENDING AT 139 27.
  ENDCASE.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM definir_eventos.
  PERFORM f_carregar_eventos USING:
* para tira duplo click            SLIS_EV_USER_COMMAND 'XUSER_COMMAND',
                                   slis_ev_top_of_page  'XTOP_OF_PAGE',
                                   slis_ev_pf_status_set   'PF_STATUS_SET'.


ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
FORM montar_layout.
  DATA: lw_setleaf TYPE setleaf,
        vl_vw_est  TYPE c.

  CLEAR: vl_vw_est.

  SELECT SINGLE * FROM setleaf
    INTO lw_setleaf
   WHERE setname EQ 'MAGGI_EST_ZLES0079'
     AND valfrom EQ sy-uname.

  IF sy-subrc = 0.
    vl_vw_est = 'X'.
  ENDIF.

  REFRESH:  estrutura[].

  IF ( r_ad_cx EQ 'X' ). "Layout para Adiatamento de CAIXA
    PERFORM montar_estrutura USING:

      1   ''  ''              'T_SAIDA' 'BUKRS'   'Empresa'              '10' '',
      2   ''  ''              'T_SAIDA' 'LIFNR'   'Fornecedor'           '10' '',
      3   ''  ''              'T_SAIDA' 'NAME1'   'Nome do Fornecedor'   '25' '',
      4   ''  ''              'T_SAIDA' 'EBELN'   'Pedido de Compra.'    '15' '',
      5   ''  ''              'T_SAIDA' 'BELNR'   'Doc. Cont.'           '15' 'X',
      6   ''  ''              'T_SAIDA' 'AUGBL'   'Doc. Comp.'           '15' 'X',
      7   ''  ''              'T_SAIDA' 'AUGDT'   'Dt. Comp'             '10' '',
      8   ''  ''              'T_SAIDA' 'BUDAT'   'Dt. Lcto.'            '10' '',
      9   ''  ''              'T_SAIDA' 'ZFBDT'   'Dt. Vcto.'            '10' '',
      10  'BSAK'  'DMBTR'     'T_SAIDA' 'DMBTR'   'Valor R$'             '15' '',
      11  'BSAK'  'DMBE2'     'T_SAIDA' 'DMBE2'   'Valor U$'             '15' '',
      12  ''  ''              'T_SAIDA' 'XBLNR'   'Referencia'           '20' '',
      13  ''  ''              'T_SAIDA' 'SGTXT'   'Texto'                '35' '',
      13  'BSAK'  'XBLNR'     'T_SAIDA' 'XBLNR'   'Referência'           '12' '',
      13  'MARA'  'MATKL'     'T_SAIDA' 'MATKL'   'Grp.Merc.'            '10' ''.
    IF vl_vw_est IS NOT INITIAL.
      PERFORM montar_estrutura USING:
       15  ''  ''   'T_SAIDA' 'EXC_EST' 'Estornar'             '08' 'X'.
    ENDIF.



*  ELSEIF ( R_AD_LQ EQ 'X' ).
*
*    PERFORM MONTAR_ESTRUTURA USING:
*
*       1   ''  ''   'T_SAIDA' 'LIFNR'       'Fornecedor'           '10' '',
*       2   ''  ''   'T_SAIDA' 'NAME1'       'Nome do Fornecedor'   '25' '',
*       3   ''  ''   'T_SAIDA' 'EBELN'       'Pedido de Compra.'    '15' '',
*       4   ''  ''   'T_SAIDA' 'BELNR'       'Doc. Cont.'           '15' '',
*       5   ''  ''   'T_SAIDA' 'AUGBL'       'Doc. Comp.'           '15' '',
*       6   ''  ''   'T_SAIDA' 'AUGDT'       'Dt. Comp'             '10' '',
*       7   ''  ''   'T_SAIDA' 'BUDAT'       'Dt. Lcto.'            '10' '',
*       8   ''  ''   'T_SAIDA' 'ZFBDT'       'Dt. Vcto.'            '10' '',
*       9   ''  ''   'T_SAIDA' 'DMBTR'       'Valor R$'             '15' '',
*       10  ''  ''   'T_SAIDA' 'DMBE2'       'Valor U$'             '15' '',
*       11  ''  ''   'T_SAIDA' 'BELNR_FAT'   'Dc.Cta.Fat.'          '15' '',
*       12  ''  ''   'T_SAIDA' 'AUGBL_FAT'   'Doc.Fat.'             '15' '',
*       13  ''  ''   'T_SAIDA' 'BUDAT_FAT'   'Dt.Lc.Fat.'           '10' '',
*       14  ''  ''   'T_SAIDA' 'DMBTR_FAT'   'Fatura R$'            '15' '',
*       15  ''  ''   'T_SAIDA' 'DMBE2_FAT'   'Fatura U$'            '15' ''.


*  ELSEIF ( R_PED EQ 'X' ).
*
*    PERFORM MONTAR_ESTRUTURA USING:
*     1  'BSAK'             'BUKRS'          'T_SAIDA' 'BUKRS'         ' '  ' '                 '',
*     2  'BSAK'             'AUGDT'          'T_SAIDA' 'AUGDT'         'Data de Pagamento'  ' ' '',
*     3  'BSAK'             'MONAT'          'T_SAIDA' 'MONAT'         ' '  ' '                 '',
*     4  'BSAK'             'GJAHR'          'T_SAIDA' 'GJAHR'         ' '  ' '                 '',
*     5  'BSAK'             'AUGBL'          'T_SAIDA' 'AUGBL'         ' '  ' '                 '',
*     6  'BSAK'             'BELNR'          'T_SAIDA' 'BELNR'         ' '  ' '                 '',
*     7  'BSAK'             'BLART'          'T_SAIDA' 'BLART'         ' '  ' '                 '',
*     8  'BKPF'             'AWKEY'          'T_SAIDA' 'MIRO'          'Miro'  ' '              '',
*     9  'BSAK'             'LIFNR'          'T_SAIDA' 'LIFNR'         ' '  ' '                 '',
*    10  'BSAK'             'XBLNR'          'T_SAIDA' 'DOC_FISCAL'    'Doc fiscal'  ' '        '',
*    12  'ZPFE_LOTE_ITEM'   'TKNUM'          'T_SAIDA' 'TKNUM'         ' '  ' '                 '',
*    14  'VTTK'             'VSART'          'T_SAIDA' 'MODAL'         'Modal'  ' '             '',
*    16  'LIPS'             'MATNR'          'T_SAIDA' 'MATNR'         ' '  ' '                 '',
*    17  'MAKT'             'MAKTX'          'T_SAIDA' 'MAKTX'         ' '  ' '                 '',
*    18  'MARA'             'MATKL'          'T_SAIDA' 'MATKL'         ' '  ' '                 '',
*    19  'T023T'            'WGBEZ'          'T_SAIDA' 'WGBEZ'         'Desc. Grupo Merc.'  ' ' '',
*    21  'LIPS'             'GEWEI'          'T_SAIDA' 'GEWEI'         ' '  ' '                 '',
*    22  'ZPFE_LOTE_ITEM'   'VL_PAGO_LOTE'   'T_SAIDA' 'VL_PAGO_LOTE'  'VALOR BRL'  ' '         '',
*    23  'BSAK'             'DMBE2'          'T_SAIDA' 'DMBE2'         'VALOR USD'  ' '         '',
*    24  'BKPF'             'BKTXT'          'T_SAIDA' 'BKTXT'         ' '  ' '                 '',
*    24  'BSAK'             'SGTXT'          'T_SAIDA' 'SGTXT'         ' '  ' '                 ''.


  ELSEIF ( r_ad_pi EQ 'X' ) OR ( r_ev_prt EQ 'X' ) OR ( r_aq_pa = 'X' ).
    PERFORM montar_estrutura USING:
    1   ''      ''        'T_SAIDA'  'BUKRS'         'Empresa'             '10' '',
    2   ''      ''        'T_SAIDA'  'GSBER'         'Filial'              '25' '',
    3   ''      ''        'T_SAIDA'  'LIFNR'         'Fornecedor'          '15' '',
    4   ''      ''        'T_SAIDA'  'NAME1'         'Descr. Fornecedor'   '15' '',
    5   ''      ''        'T_SAIDA'  'BKTXT'         'Tipo'                '15' '',
    6   ''      ''        'T_SAIDA'  'BELNR'         'Nr. Documento'       '10' 'X',
    7   ''      ''        'T_SAIDA'  'AUGBL'         'Doc. Compens.'       '10' 'X',
    8   ''      ''        'T_SAIDA'  'BUDAT'         'Dt. Lcto'            '10' '',
    9   ''      ''        'T_SAIDA'  'AUGDT'         'Dt. Liquidação'      '15' '',
    10  ''      ''        'T_SAIDA'  'ZFBDT'         'Dt. Vcto'            '15' '',
    11  ''      ''        'T_SAIDA'  'EBELN'         'Pedido'              '15' 'X',
    12  ''      ''        'T_SAIDA'  'BSART'         'Tp. Pedido'          '15' '',
    12  ''      ''        'T_SAIDA'  'EBELN_ORIGEM'  'Pedido Origem'       '15' 'X',
    12  ''      ''        'T_SAIDA'  'BSART_ORIGEM ' 'Tp.Ped.Origem'       '15' '',
    13  'BSAK'  'DMBTR'   'T_SAIDA'  'DMBTR'         'Vlr R$'              '10' '',
    14  'BSAK'  'DMBE2'   'T_SAIDA'  'DMBE2'         'Vlr US$'             '15' '',
    15  ''      ''        'T_SAIDA'  'TX_CAMB'       'Tx. Câmbio'          '15' '',
    16  ''      ''        'T_SAIDA'  'BANCO_LIQ'     'Banco Liquidado'     '25' '',
    16  'BSAK'  'XBLNR'   'T_SAIDA'  'XBLNR'         'Referência'          '12' ''.
    IF vl_vw_est IS NOT INITIAL.
      PERFORM montar_estrutura USING:
       18  ''  ''   'T_SAIDA' 'EXC_EST' 'Estornar'               '08' 'X'.
    ENDIF.

*-CS2022000256 - 24.03.2022 - JT - inicio
    IF r_ev_prt = abap_true.
      PERFORM montar_estrutura USING:
        20  ''      ''        'T_SAIDA'  'KOSTL'         'Centro Custo'        '16' '',
        21  ''      ''        'T_SAIDA'  'PRCTR'         'Centro Lucro'        '16' '',
        22  ''      ''        'T_SAIDA'  'STCD1'         'CNPJ'                '16' '',
        23  'MARA'  'MATNR'   'T_SAIDA'  'MATNR'         'Material'            '18' '',
        24  ''      ''        'T_SAIDA'  'MATKL'         'Grp.Material'        '16' '',
        25  ''      ''        'T_SAIDA'  'MAKTX'         'Descr.Grp.Material'  '40' ''.
    ENDIF.
*-CS2022000256 - 24.03.2022 - JT - fim

  ELSEIF ( r_fprop EQ 'X' ).

    PERFORM montar_estrutura USING:

    01   ''      ''        'T_SAIDA'  'BUKRS'         'Empresa'             '07' '',
    02   ''      ''        'T_SAIDA'  'GSBER'         'Filial'              '07' '',
    04   'VTTK'  'TKNUM'   'T_SAIDA'  'TKNUM'         'Doc.Transp'          '10' '',
    05   'VFKK'  'FKNUM'   'T_SAIDA'  'FKNUM'         'Doc.Custo'           '10' '',
    05  'BKPF'   'BKTXT'   'T_SAIDA'  'BKTXT'         ' '                   ' '  '',
    06  'LIPS'   'MATNR'   'T_SAIDA'  'MATNR'         ' '                   ' '  '',
    07  'MAKT'   'MAKTX'   'T_SAIDA'  'MAKTX'         ' '                   ' '  '',
    08  'MARA'   'MATKL'   'T_SAIDA'  'MATKL'         ' '                   ' '  '',
    09  'T023T'  'WGBEZ'   'T_SAIDA'  'WGBEZ'         'Desc. Grupo Merc.'   ' '  '',

    10   ''      ''        'T_SAIDA'  'BUDAT'         'Dt. Lcto'            '10' '',
    11   ''      ''        'T_SAIDA'  'AUGDT'         'Dt.Liquidação'       '15' '',
    12   ''      ''        'T_SAIDA'  'ZFBDT'         'Dt. Vcto'            '15' '',
    13   'BSAK'  'DMBTR'   'T_SAIDA'  'DMBTR'         'Vlr R$'              '10' '',
    14   'BSAK'  'DMBE2'   'T_SAIDA'  'DMBE2'         'Vlr US$'             '15' '',
    15   ''      ''        'T_SAIDA'  'TX_CAMB'       'Tx. Câmbio'          '15' ''.

    IF vl_vw_est IS NOT INITIAL.
      PERFORM montar_estrutura USING:
       18  ''  ''   'T_SAIDA' 'EXC_EST' 'Estornar'               '08' 'X'.
    ENDIF.

  ELSE.
    PERFORM montar_estrutura USING:
    1  'BSAK'             'BUKRS'          'T_SAIDA' 'BUKRS'         ' '  ' '                 '',
    2  'BSAK'             'AUGDT'          'T_SAIDA' 'AUGDT'         'Data de Pagamento'  ' ' '',
    2  'BSAK'             'ZFBDT'          'T_SAIDA' 'ZFBDT'         'Data Vencimento'  ' '   '',
    3  'BSAK'             'MONAT'          'T_SAIDA' 'MONAT'         ' '  ' '                 '',
    4  'BSAK'             'GJAHR'          'T_SAIDA' 'GJAHR'         ' '  ' '                 '',
    5  'BSAK'             'AUGBL'          'T_SAIDA' 'AUGBL'         ' '  ' '                 '',
    6  'BSAK'             'BELNR'          'T_SAIDA' 'BELNR'         ' '  ' '                 '',
    7  'BSAK'             'BLART'          'T_SAIDA' 'BLART'         ' '  ' '                 '',
    8  'BKPF'             'AWKEY'          'T_SAIDA' 'MIRO'          'Miro'  ' '              '',
    9  'BSAK'             'LIFNR'          'T_SAIDA' 'LIFNR'         ' '  ' '                 '',
   10  'BSAK'             'XBLNR'          'T_SAIDA' 'DOC_FISCAL'    'Doc fiscal'  ' '        '',
   11  'J_1BNFLIN'        'DOCNUM'         'T_SAIDA' 'DOCNUM'        'DOCNUM'  ' '            '',
   12  'ZPFE_LOTE_ITEM'   'TKNUM'          'T_SAIDA' 'TKNUM'         ' '  ' '                 '',
   12  ''                 ''               'T_SAIDA' 'BEZEI'         'Tp.Transporte'  ' '     '',
   13  'VFKP'             'FKNUM'          'T_SAIDA' 'FKNUM'         ' '  ' '                 '',
   14  'VTTK'             'VSART'          'T_SAIDA' 'MODAL'         'Modal'  ' '             '',
   15  'ZLEST0032'        'ADD03'          'T_SAIDA' 'ADD03'         'Tipo frete'  ' '        '',
   16  'LIPS'             'MATNR'          'T_SAIDA' 'MATNR'         ' '  ' '                 '',
   17  'MAKT'             'MAKTX'          'T_SAIDA' 'MAKTX'         ' '  ' '                 '',
   18  'MARA'             'MATKL'          'T_SAIDA' 'MATKL'         ' '  ' '                 '',
   19  'T023T'            'WGBEZ'          'T_SAIDA' 'WGBEZ'         'Desc. Grupo Merc.'  ' ' '',
   20  'ZPFE_LOTE_ITEM'   'PESO_CHEGADA'   'T_SAIDA' 'PESO'          'Peso Destino'  ' '      '',
   20  'ZLEST0034'        'ZPESO_ORIGEM'   'T_SAIDA' 'ZPESO_ORIGEM'  'Peso Origem'  ' '       '',
   21  'LIPS'             'GEWEI'          'T_SAIDA' 'GEWEI'         ' '  ' '                 '',
   "22  'ZPFE_LOTE_ITEM'   'VL_PAGO_LOTE'   'T_SAIDA' 'VL_PAGO_LOTE'  'VALOR BRL'  ' '         '', CSB
   22  'BSAK'             'DMBTR'          'T_SAIDA' 'DMBTR'         'VALOR BRL'  ' '         '',
   23  'BSAK'             'DMBE2'          'T_SAIDA' 'DMBE2'         'VALOR USD'  ' '         '',
   24  'BSAK'             'XBLNR'          'T_SAIDA' 'XBLNR'         'Nr. lote'  ' '          '',
   24  'ZPFE_LOTE_ITEM'   'CHVID'          'T_SAIDA' 'CHVID'         ' '  ' '                 '',
   24  'BKPF'             'BKTXT'          'T_SAIDA' 'BKTXT'         ' '  ' '                 '',
   24  'BSAK'             'SGTXT'          'T_SAIDA' 'SGTXT'         ' '  ' '                 '',
   25  'LIPS'             'CHARG'          'T_SAIDA' 'CHARG'         'Safra'  ' '             ''.

    IF sy-tcode EQ 'ZLES0167'.
      PERFORM montar_estrutura USING:
       28  'J_1BNFDOC'          'NFTOT'          'T_SAIDA' 'VLR_CTE'            'Vlr. CT-e'         ' '                 '',
       29  'ZPFE_LOTE_ITEM'     'VL_TRANSACAO'   'T_SAIDA' 'VLR_QUEBRA'         'Vlr.Quebra'        ' '                 '',
       30  'ZPFE_LOTE_ITEM'     'VL_TRANSACAO'   'T_SAIDA' 'VLR_PERDA'          'Vlr.Perda'         ' '                 '',
       31  'ZPFE_LOTE_ITEM'     'VL_TRANSACAO'   'T_SAIDA' 'VLR_QUEBRA_PERDA'   'Vlr.Quebra+Perda'  ' '                 '',
       32  'KONP'               'KBETR'          'T_SAIDA' 'VLR_TARIFA_PGTO'    'Vlr.Tarifa.Pgto '  ' '                 '',
       33  'LFA1'               'NAME1'          'T_SAIDA' 'NAME1'              'Ds.Fornecedor'     ' '                 ''.
    ENDIF.

    IF vl_vw_est IS NOT INITIAL.
      PERFORM montar_estrutura USING:
      50  ''                 ''               'T_SAIDA' 'EXC_EST'       'Estornar'          ''   'X'.
    ENDIF.


  ENDIF.


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_hotspot).

  CLEAR wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-hotspot       = p_hotspot.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  IF p_field EQ 'EXC_EST'.
    wa_estrutura-icon = 'X'.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
FORM iniciar_variaveis.

  DATA: wl_layout2(50) TYPE c,
        wl_layout3(19) VALUE 'Data de pagamento: ',
        wl_data        VALUE '.',
        wl_space       VALUE '-',
        wl_layout4(3)  VALUE 'até',
        wl_low         LIKE s_augdt,
        wl_high        LIKE s_augdt,
        wl_bukrs(50)   TYPE c,
        wl_emp(8)      VALUE 'Empresa:',
        wl_butxt       TYPE butxt.

  SELECT SINGLE butxt
    INTO wl_butxt FROM t001
       WHERE bukrs IN s_bukrs.


  CONCATENATE wl_emp s_bukrs+3(4) wl_space wl_butxt  INTO wl_bukrs SEPARATED BY space.


  IF s_augdt-low IS NOT INITIAL
    AND s_augdt-high IS NOT INITIAL.
    CONCATENATE s_augdt-low+6(2)  wl_data s_augdt-low+4(2)  wl_data s_augdt-low(4)  INTO wl_low.
    CONCATENATE s_augdt-high+6(2) wl_data s_augdt-high+4(2) wl_data s_augdt-high(4) INTO wl_high.
    CONCATENATE  wl_layout3 wl_low wl_layout4 wl_high  INTO wl_layout2 SEPARATED BY space.

  ELSEIF s_augdt-low IS NOT INITIAL.
    CONCATENATE s_augdt-low+6(2)  wl_data s_augdt-low+4(2)  wl_data s_augdt-low(4)  INTO wl_low.
    CONCATENATE  wl_layout3 wl_low  INTO wl_layout2 SEPARATED BY space.
  ENDIF.


  v_report = sy-repid.

  IF r_pag EQ 'X'.
    PERFORM f_construir_cabecalho USING 'H' 'Pagamento – Frete'.
  ELSEIF r_com EQ 'X'.
    PERFORM f_construir_cabecalho USING 'H' 'Compensações – Frete'.
  ELSEIF ( r_ad_cx EQ 'X' ).
    PERFORM f_construir_cabecalho USING 'H' 'Adiantamento Caixa – Frete'.
*  ELSEIF ( R_AD_LQ EQ 'X' ).
*    PERFORM F_CONSTRUIR_CABECALHO USING 'H' 'Frete Pagos - Liquidação Adiantamento'.
  ELSEIF r_ad_pi EQ 'X'.
    PERFORM f_construir_cabecalho USING 'H' 'Pagamento\Adiantamento Insumos – Caixa'.
  ELSEIF r_ev_prt EQ 'X'.
    PERFORM f_construir_cabecalho USING 'H' 'Elevação Portuária'.
  ELSEIF r_aquav EQ 'X'.
    PERFORM f_construir_cabecalho USING 'H' TEXT-i01.
  ELSEIF r_aq_pa EQ 'X'.
    PERFORM f_construir_cabecalho USING 'H' TEXT-i02.
  ELSEIF r_fprop EQ 'X'.
    PERFORM f_construir_cabecalho USING 'H' TEXT-i05.
  ELSE.
    PERFORM f_construir_cabecalho USING 'H' TEXT-002.
  ENDIF.

  PERFORM f_construir_cabecalho USING 'S' wl_bukrs.
  PERFORM f_construir_cabecalho USING 'S' wl_layout2.


ENDFORM.                    " INICIAR_VARIAVES

FORM f_estornar_lcto  USING  p_saida TYPE ty_saida
                             p_tabix TYPE sy-tabix.

  DATA: var_answer       TYPE c,
        var_tipo_reg(10) TYPE c,
        gw_zlest0105     TYPE zlest0105.

  DATA ref1 TYPE REF TO cl_gui_alv_grid.

  CLEAR: var_tipo_reg.

  CASE var_opcao.
    WHEN: 'R_AD_CX'.
      var_tipo_reg = 'FA'.
    WHEN: 'PAG'.
      var_tipo_reg = 'FP'.
    WHEN 'R_AD_PI'.
      var_tipo_reg = 'IA_IP'. "Tipo Definido no Registro
    WHEN 'R_FPROP'.
      var_tipo_reg = 'PR'.
  ENDCASE.

  CHECK ( var_tipo_reg IS NOT INITIAL ).

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente estornar o registro?'
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

  IF p_saida-inc_manual IS NOT INITIAL.

    DELETE FROM zlest0105 WHERE bukrs      = p_saida-bukrs
                            AND belnr      = p_saida-belnr
                            AND augbl      = p_saida-augbl
                            AND docnum     = p_saida-docnum
                            AND tknum      = p_saida-tknum
                            AND chvid      = p_saida-chvid
                            AND tipo       = p_saida-tipo
                            AND inc_manual = 'X'.
    IF sy-subrc = 0.
      MESSAGE 'Lançamento excluído com sucesso!' TYPE 'S'.
    ELSE.
      MESSAGE 'Houve um erro na exclusão do lançamento!' TYPE 'S'.
      EXIT.
    ENDIF.

  ELSE.

    CLEAR: gw_zlest0105.
    SELECT SINGLE *
      FROM zlest0105 INTO gw_zlest0105
     WHERE belnr   = p_saida-belnr
       AND augbl   = p_saida-augbl
       AND bukrs   = p_saida-bukrs
       AND docnum  = p_saida-docnum
       AND tknum   = p_saida-tknum
       AND chvid   = p_saida-chvid
       AND tipo    = p_saida-tipo
       AND estorno = 'X'.

    IF sy-subrc = 0.
      MESSAGE 'Lançamento já estornado!' TYPE 'S'.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING p_saida TO gw_zlest0105.

    gw_zlest0105-us_proc  = sy-uname.
    gw_zlest0105-dt_atual = sy-datum.
    gw_zlest0105-hr_atual = sy-uzeit.
    gw_zlest0105-dmbtr    = gw_zlest0105-dmbtr * -1.
    gw_zlest0105-dmbe2    = gw_zlest0105-dmbe2 * -1.
    gw_zlest0105-vl_pago_lote = gw_zlest0105-vl_pago_lote * -1.
    gw_zlest0105-xblnr    = 'ESTORNO'.
    gw_zlest0105-estorno  = 'X'.
    gw_zlest0105-estorno_manual = 'X'.

    INSERT zlest0105 FROM gw_zlest0105.
    COMMIT WORK.

    MESSAGE 'Lançamento estornado com sucesso!' TYPE 'S'.

  ENDIF.

  DELETE gt_saida INDEX p_tabix.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR' "<-- Gets the reference of ALV grid object from REUSE functions
    IMPORTING
      e_grid = ref1.

  IF ref1 IS NOT INITIAL.
    CALL METHOD ref1->refresh_table_display.
  ENDIF.


ENDFORM.

FORM pf_status_set USING ut_extab TYPE slis_t_extab.        "#EC CALLED

  "DELETE UT_EXTAB WHERE FCODE = '&REFRESH'.

  DATA: tg_fcode TYPE TABLE OF sy-ucomm WITH HEADER LINE.

  REFRESH: tg_fcode.

  SELECT SINGLE * FROM setleaf INTO @DATA(lw_setleaf)
   WHERE setname EQ 'MAGGI_EST_ZLES0079'
     AND valfrom EQ @sy-uname.

  IF sy-subrc NE 0.
    tg_fcode = 'AJUS_LCTO'.
    APPEND tg_fcode.
  ENDIF.

  IF sy-tcode EQ 'ZLES0167'.
    tg_fcode = 'INC_LCTO'.
    APPEND tg_fcode.

    tg_fcode = 'AJUS_LCTO'.
    APPEND tg_fcode.
  ENDIF.

  SET PF-STATUS '0100' EXCLUDING tg_fcode.

  "SET PF-STATUS 'STANDARD_FULLSCREEN' OF PROGRAM 'SAPLKKBL'.


ENDFORM.

MODULE user_command_0101 INPUT.

  DATA ref1 TYPE REF TO cl_gui_alv_grid.

  DATA: wl_zlest0105 TYPE zlest0105.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      CLEAR: wl_zlest0105.

      IF gw_zles0105_ajuste-bukrs IS INITIAL.
        MESSAGE 'Empresa é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF gw_zles0105_ajuste-tipo IS INITIAL.
        MESSAGE 'Tipo é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF gw_zles0105_ajuste-dmbtr IS INITIAL.
        MESSAGE 'Valor R$ é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF gw_zles0105_ajuste-dmbe2 IS INITIAL.
        MESSAGE 'Valor U$ é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF gw_zles0105_ajuste-augdt IS INITIAL.
        MESSAGE 'Data Compensação é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF gw_zles0105_ajuste-tipo EQ 'PR'. "Frota Propria / Frete Proprio

        IF ( gw_zles0105_ajuste-tknum IS INITIAL ).
          MESSAGE 'Doc.Transporte é um campo obrigatório!' TYPE 'S'.
          EXIT.
        ENDIF.

        SELECT SINGLE *
          FROM vfkp INTO @DATA(_wl_vfkp)
         WHERE rebel EQ @gw_zles0105_ajuste-tknum.

        IF sy-subrc NE 0.
          MESSAGE |Doc.Custo do transporte { gw_zles0105_ajuste-tknum } não foi encontrado!| TYPE 'S'.
          EXIT.
        ENDIF.

        gw_zles0105_ajuste-fknum = _wl_vfkp-fknum.

      ELSE.
        IF gw_zles0105_ajuste-belnr IS INITIAL.
          MESSAGE 'Doc.Contabil é um campo obrigatório!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF gw_zles0105_ajuste-augbl IS INITIAL.
          MESSAGE 'Doc.Compensação é um campo obrigatório!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF gw_zles0105_ajuste-buzei IS INITIAL.
          MESSAGE 'Item é um campo obrigatório!' TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.

      "Check Duplicidade
      SELECT SINGLE *
        FROM zlest0105 INTO @DATA(_wl_0105_aux)
       WHERE bukrs   = @gw_zles0105_ajuste-bukrs
         AND belnr   = @gw_zles0105_ajuste-belnr
         AND augbl   = @gw_zles0105_ajuste-augbl
         AND docnum  = @gw_zles0105_ajuste-docnum
         AND tknum   = @gw_zles0105_ajuste-tknum
         AND xblnr   = @gw_zles0105_ajuste-xblnr
         AND chvid   = @gw_zles0105_ajuste-chvid
         AND tipo    = @gw_zles0105_ajuste-tipo
         AND estorno = @gw_zles0105_ajuste-estorno.

      IF sy-subrc = 0.
        MESSAGE | Registro já existente na data: { _wl_0105_aux-augdt+6(2) }.{ _wl_0105_aux-augdt+4(2) }.{ _wl_0105_aux-augdt(4) } ! Operação de inclusão não permitida! | TYPE 'S'.
        EXIT.
      ENDIF.

      gw_zles0105_ajuste-inc_manual = 'X'.
*---> 13/06/2023 - Migração S4 - JS
*       gw_zles0105_ajuste-vl_pago_lote = gw_zles0105_ajuste-dmbtr.
      gw_zles0105_ajuste-vl_pago_lote = CONV #( gw_zles0105_ajuste-dmbtr ).
*<--- 13/06/2023 - Migração S4 - JS
      gw_zles0105_ajuste-us_proc    = sy-uname.
      gw_zles0105_ajuste-dt_atual   = sy-datum.
      gw_zles0105_ajuste-hr_atual   = sy-uzeit.

      MOVE-CORRESPONDING gw_zles0105_ajuste TO wl_zlest0105.
      INSERT zlest0105 FROM wl_zlest0105.
      IF sy-subrc = 0.
        MESSAGE 'Registro incluído com sucesso!' TYPE 'S'.

        CLEAR: gw_saida.
        MOVE-CORRESPONDING wl_zlest0105 TO gw_saida.
        gw_saida-exc_est = icon_execute_object.
        APPEND gw_saida TO gt_saida.

        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR' "<-- Gets the reference of ALV grid object from REUSE functions
          IMPORTING
            e_grid = ref1.

        IF ref1 IS NOT INITIAL.
          CALL METHOD ref1->refresh_table_display.
        ENDIF.

        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao incluir o registro!' TYPE 'S'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

MODULE status_0101 OUTPUT.

  SET PF-STATUS '0101'.
  SET TITLEBAR 'T0101'.

  PERFORM f_monta_tipo_ajuste.

  IF gw_zles0105_ajuste-matnr IS NOT INITIAL.

    SELECT SINGLE *
      FROM mara INTO @DATA(_wa_mara)
     WHERE matnr = @gw_zles0105_ajuste-matnr.

    IF sy-subrc NE 0.
      MESSAGE 'Código de Material não existe!' TYPE 'S'.
      EXIT.
    ENDIF.

    SELECT SINGLE *
      FROM makt INTO @DATA(_wa_makt)
     WHERE matnr = @_wa_mara-matnr
       AND spras = @sy-langu.

    IF sy-subrc = 0.
      gw_zles0105_ajuste-maktx = _wa_makt-maktx.
    ENDIF.

    SELECT SINGLE *
      FROM t023t INTO @DATA(_wa_t023t)
     WHERE matkl = @_wa_mara-matkl
       AND spras = @sy-langu.

    IF sy-subrc = 0.
      gw_zles0105_ajuste-matkl = _wa_t023t-matkl.
      gw_zles0105_ajuste-wgbez = _wa_t023t-wgbez.
    ENDIF.

  ENDIF.

  IF ( gw_zles0105_ajuste-tknum IS NOT INITIAL ) AND ( gw_zles0105_ajuste-tipo EQ 'PR' ).

    SELECT SINGLE *
      FROM vfkp INTO _wl_vfkp
     WHERE rebel EQ gw_zles0105_ajuste-tknum.

    IF sy-subrc NE 0.
      MESSAGE |Doc.Custo do transporte { gw_zles0105_ajuste-tknum } não foi encontrado!| TYPE 'S'.
      CLEAR: gw_zles0105_ajuste-tknum, gw_zles0105_ajuste-fknum.
      EXIT.
    ENDIF.

    gw_zles0105_ajuste-fknum = _wl_vfkp-fknum.
    gw_zles0105_ajuste-bukrs = _wl_vfkp-bukrs.
    gw_zles0105_ajuste-gsber = _wl_vfkp-werks.

    IF gw_zles0105_ajuste-augdt IS INITIAL.
      gw_zles0105_ajuste-augdt = _wl_vfkp-budat.
      gw_zles0105_ajuste-budat = _wl_vfkp-budat.
      gw_zles0105_ajuste-gjahr = _wl_vfkp-budat(4).
      gw_zles0105_ajuste-zfbdt = _wl_vfkp-budat.
    ENDIF.

  ENDIF.


  IF gw_zles0105_ajuste-bukrs IS NOT INITIAL AND
     gw_zles0105_ajuste-belnr IS NOT INITIAL AND
     gw_zles0105_ajuste-buzei IS NOT INITIAL AND
     gw_zles0105_ajuste-augbl IS INITIAL.

    CLEAR: gw_zles0105_ajuste-dmbtr, gw_zles0105_ajuste-dmbe2,
           gw_zles0105_ajuste-augbl, gw_zles0105_ajuste-tx_camb,
           gw_zles0105_ajuste-lifnr, gw_zles0105_ajuste-blart,
           gw_zles0105_ajuste-ebeln, gw_zles0105_ajuste-budat,
           gw_zles0105_ajuste-augdt, gw_zles0105_ajuste-zfbdt,
           gw_zles0105_ajuste-monat, gw_zles0105_ajuste-gjahr.

    SELECT SINGLE *
      FROM bsik INTO @DATA(_wa_bsik)
     WHERE bukrs = @gw_zles0105_ajuste-bukrs
       AND belnr = @gw_zles0105_ajuste-belnr
       AND buzei = @gw_zles0105_ajuste-buzei.

    IF sy-subrc EQ 0.
      gw_zles0105_ajuste-dmbtr = _wa_bsik-dmbtr.
      gw_zles0105_ajuste-dmbe2 = _wa_bsik-dmbe2.
      gw_zles0105_ajuste-augbl = _wa_bsik-belnr.
      gw_zles0105_ajuste-lifnr = _wa_bsik-lifnr.

      gw_zles0105_ajuste-blart = _wa_bsik-blart.
      gw_zles0105_ajuste-ebeln = _wa_bsik-ebeln.
      gw_zles0105_ajuste-budat = _wa_bsik-budat.
      gw_zles0105_ajuste-augdt = _wa_bsik-budat.
      gw_zles0105_ajuste-zfbdt = _wa_bsik-zfbdt + _wa_bsik-zbd1t.

      gw_zles0105_ajuste-monat = gw_zles0105_ajuste-augdt+4(2).
      gw_zles0105_ajuste-gjahr = gw_zles0105_ajuste-augdt(4).


    ELSE.
      SELECT SINGLE *
        FROM bsak INTO @DATA(_wa_bsak)
       WHERE bukrs = @gw_zles0105_ajuste-bukrs
         AND belnr = @gw_zles0105_ajuste-belnr
         AND buzei = @gw_zles0105_ajuste-buzei.

      IF sy-subrc = 0.
        gw_zles0105_ajuste-dmbtr = _wa_bsak-dmbtr.
        gw_zles0105_ajuste-dmbe2 = _wa_bsak-dmbe2.
        gw_zles0105_ajuste-augbl = _wa_bsak-augbl.
        gw_zles0105_ajuste-lifnr = _wa_bsak-lifnr.

        gw_zles0105_ajuste-blart = _wa_bsak-blart.
        gw_zles0105_ajuste-ebeln = _wa_bsak-ebeln.
        gw_zles0105_ajuste-budat = _wa_bsak-budat.
        gw_zles0105_ajuste-augdt = _wa_bsak-augdt.
        gw_zles0105_ajuste-zfbdt = _wa_bsak-zfbdt + _wa_bsak-zbd1t.

        gw_zles0105_ajuste-monat = gw_zles0105_ajuste-augdt+4(2).
        gw_zles0105_ajuste-gjahr = gw_zles0105_ajuste-augdt(4).

      ENDIF.
    ENDIF.

  ENDIF.

  IF gw_zles0105_ajuste-dmbtr IS NOT INITIAL AND
    gw_zles0105_ajuste-dmbe2 IS NOT INITIAL.
    gw_zles0105_ajuste-tx_camb = gw_zles0105_ajuste-dmbtr / gw_zles0105_ajuste-dmbe2.
  ENDIF.

  PERFORM f_config_cat_0101.

  SORT tg_fields_0101 BY field.

  LOOP AT SCREEN.
    READ TABLE tg_fields_0101 WITH KEY field = screen-name BINARY SEARCH.
    IF sy-subrc = 0.
      screen-invisible = 0.
      screen-input     = 1.
    ELSE.
      screen-invisible = 1.
      screen-input     = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


ENDMODULE.

FORM f_monta_tipo_ajuste.

  DATA: values  TYPE vrm_values WITH HEADER LINE.

  CLEAR: values[].

  CASE var_opcao.
    WHEN: 'R_AD_CX'.
      values-text = 'Adiantamento'.
      values-key  = 'FA'.
      APPEND values.
    WHEN: 'PAG'.
      values-text = 'Pagamento'.
      values-key  = 'FP'.
      APPEND values.
    WHEN 'R_AD_PI'.
      values-text = 'Adiantamento'.
      values-key  = 'IA'.
      APPEND values.

      values-text = 'Pagamento'.
      values-key  = 'IP'.
      APPEND values.
    WHEN 'R_EV_PRT'.
      values-text = 'Adiantamento'.
      values-key  = 'EA'.
      APPEND values.

      values-text = 'Pagamento'.
      values-key  = 'EP'.
      APPEND values.
    WHEN 'R_AQUAV'.
      values-text = 'Pagamento'.
      values-key  = 'AQ'.
      APPEND values.
    WHEN 'R_AQ_PA'.
      values-text = 'Pagamento'.
      values-key  = 'PA'.
      APPEND values.
    WHEN 'R_FPROP'.
      values-text = 'Frete Proprio'.
      values-key  = 'PR'.
      APPEND values.
  ENDCASE.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'GW_ZLES0105_AJUSTE-TIPO'
      values          = values[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF gw_zles0105_ajuste-tipo IS INITIAL.
    IF lines(  values[] ) = 1.
      READ TABLE values INDEX 1.
      gw_zles0105_ajuste-tipo = values-key.
    ENDIF.
  ENDIF.

ENDFORM.

MODULE status_0102 OUTPUT.
  SET PF-STATUS 'PF0102'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.

MODULE user_command_0102 INPUT.

  DATA: wl_0105 TYPE zlest0105.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF ( gw_zles0105_ajuste-vl_pago_lote IS INITIAL ) OR
         ( gw_zles0105_ajuste-dmbtr IS INITIAL        ) OR
         ( gw_zles0105_ajuste-dmbe2 IS INITIAL        ).
        MESSAGE 'Informe todos os campos obrigatórios!' TYPE 'S'.
        EXIT.
      ENDIF.

      CLEAR: wl_0105.

      MOVE-CORRESPONDING gw_zles0105_ajuste TO wl_0105.

      wl_0105-inc_manual = 'X'.
      wl_0105-us_proc    = sy-uname.
      wl_0105-dt_atual   = sy-datum.
      wl_0105-hr_atual   = sy-uzeit.

      MODIFY zlest0105 FROM wl_0105.

      IF sy-subrc = 0.
        MESSAGE 'Registro modificado com sucesso!' TYPE 'S'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

FORM f_config_cat_0101 .

  CONSTANTS v_work_area TYPE c LENGTH 50 VALUE 'GW_ZLES0105_AJUSTE-'.

  CLEAR: tg_fields_0101[].

  IF ( r_ad_cx EQ 'X' ). "Layout para Adiatamento de CAIXA

    tg_fields_0101-field = 'TIPO'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BUKRS'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'LIFNR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'EBELN'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BELNR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BUZEI'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'AUGBL'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'AUGDT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BUDAT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'ZFBDT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'DMBTR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'DMBE2'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'XBLNR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'SGTXT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'XBLNR'.
    APPEND tg_fields_0101.

  ELSEIF ( r_ad_pi EQ 'X' ) OR ( r_ev_prt EQ 'X' ).

    tg_fields_0101-field = 'TIPO'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BUKRS'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'GSBER'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'LIFNR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BKTXT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BELNR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BUZEI'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'AUGBL'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'AUGDT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BUDAT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'ZFBDT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'EBELN'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BSART'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'DMBTR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'DMBE2'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'TX_CAMB'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BANCO_LIQ'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'XBLNR'.
    APPEND tg_fields_0101.

  ELSE.

    tg_fields_0101-field = 'TIPO'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BUKRS'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'AUGDT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'ZFBDT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'MONAT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'GJAHR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'AUGBL'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BELNR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BUZEI'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BLART'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'MIRO'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'LIFNR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'DOC_FISCAL'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'DOCNUM'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'TKNUM'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'FKNUM'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'MODAL'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'ADD03'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'MATNR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'MAKTX'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'MATKL'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'WGBEZ'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'PESO'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'ZPESO_ORIGEM'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'GEWEI'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'DMBTR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'DMBE2'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'XBLNR'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'CHVID'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'BKTXT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'SGTXT'.
    APPEND tg_fields_0101.

    tg_fields_0101-field = 'CHARG'.
    APPEND tg_fields_0101.

  ENDIF.

  LOOP AT tg_fields_0101.
    tg_fields_0101-field = v_work_area && tg_fields_0101-field.
    MODIFY tg_fields_0101.
  ENDLOOP.

ENDFORM.
