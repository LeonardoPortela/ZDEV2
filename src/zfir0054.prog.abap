*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 16/06/2014                                              &*
*& Descrição: Fluxo Financeiro  - Atualização de dados pgt realizados &*
*& Transação: ZFI0059                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor          |Request   |Data      |Descrição                    &*
*&--------------------------------------------------------------------&*
*& ABAP           |          |16.06.2014|                             &*
*&--------------------------------------------------------------------&*
*& NSEGATIN       |DEVK9A2R6L|20.08.2025| Inserir campo Doc. Contábil &*
*&                                      |na tela de seleção.          &*
*&                                      |Chamado: 187070.             &*
*&--------------------------------------------------------------------&*

REPORT  zfir0054.

TABLES: bsak, bsis, faglflexa, skb1, bsid, sscrfields.   "<<<------"187070 - NMS ------->>>

TYPES: BEGIN OF ty_contas,
         hkont TYPE bsis-hkont,
       END OF ty_contas,

       BEGIN OF ty_gjahr,
         gjahr TYPE bsid-gjahr,
       END OF ty_gjahr,

       BEGIN OF ty_bsak,
         bukrs  LIKE bsak-bukrs,
         lifnr  LIKE bsak-lifnr,
         budat  LIKE bsak-budat,
         gjahr  LIKE bsak-gjahr,
         belnr  LIKE bsak-belnr,
         buzei  LIKE bsak-buzei,
         bldat  LIKE bsak-bldat,
         waers  LIKE bsak-waers,
         xblnr  LIKE bsak-xblnr,
         blart  LIKE bsak-blart,
         gsber  LIKE bsak-gsber,
         ebeln  LIKE bsak-ebeln,
         ebelp  LIKE bsak-ebelp,
         bschl  LIKE bsak-bschl,
         shkzg  LIKE bsak-shkzg,
         zfbdt  LIKE bsak-zfbdt,
         zbd1t  LIKE bsak-zbd1t,
         augbl  LIKE bsak-augbl,
         augdt  LIKE bsak-augdt,
         dmbtr  LIKE bsak-dmbtr,
         dmbe2  LIKE bsak-dmbe2,
         wrbtr  TYPE bsak-wrbtr,
         kidno  LIKE bsak-kidno,
         zuonr  LIKE bsak-zuonr,
         hkont  LIKE bsak-hkont,
         saknr  LIKE bsak-saknr,
         xref3  LIKE bsak-xref3,
         rebzg  LIKE bsak-rebzg,
         venci  LIKE bsak-zfbdt,
         xconta LIKE ekkn-sakto,
         xtot   LIKE ekkn-menge,
         xtop   LIKE ekkn-menge,
         xvlrr  LIKE ekpo-netwr,
         xvlr_r LIKE ekpo-netwr,
         xvlr_u LIKE ekpo-netwr,
         "LCTO_COMP TYPE ZFIT0078-LCTO_COMP,
       END OF ty_bsak,

       BEGIN OF ty_cls_ped,
         ebeln LIKE bsak-ebeln,
         ebelp LIKE bsak-ebelp,
         saknr LIKE bsak-saknr,
         xtot  LIKE ekkn-menge,
       END OF ty_cls_ped,

       BEGIN OF ty_rseg,
         ebeln TYPE rseg-ebeln,
         ebelp TYPE rseg-ebelp,
       END OF ty_rseg.

TYPES: BEGIN OF ty_0078_bsad,
         bukrs_l TYPE bsad-bukrs,
         gjahr_l TYPE bsad-gjahr,
         belnr_l TYPE bsad-belnr,
         buzei_l TYPE bsad-buzei.
         INCLUDE STRUCTURE zfit0078.
TYPES  END OF ty_0078_bsad.

TYPES: BEGIN OF ty_0078_bsak,
         bukrs_l TYPE bsak-bukrs,
         gjahr_l TYPE bsak-gjahr,
         belnr_l TYPE bsak-belnr,
         buzei_l TYPE bsak-buzei.
         INCLUDE STRUCTURE zfit0078.
TYPES  END OF ty_0078_bsak.

DATA: t_conta   TYPE STANDARD TABLE OF rgsb4 WITH HEADER LINE,
      it_contas TYPE TABLE OF ty_contas,
      wa_contas TYPE ty_contas.

RANGES: it_gjahr FOR bsid-gjahr,
        it_date  FOR bsak-augdt,
        it_fdlev FOR skb1-fdlev.

TYPES: BEGIN OF ty_flx_par,
         orig            TYPE c,
         hkont           TYPE bsak-hkont,       "Conta Razão
         bschl           TYPE zfit0080-bschl,   "Chave Lançamento
         matkl           TYPE vbap-matkl,       "Grupo Mercadoria.
         prctr           TYPE zfit0078-prctr,   "Centro Lucro
         kidno           TYPE bsak-kidno,       "Referencia Pgto.
         zuonr           TYPE bsak-zuonr,       "Atribuição
         blart           TYPE bsak-blart,       "Tp.Doc.
         xblnr           TYPE bsak-xblnr,       "Nº documento de referência
         rmvct           TYPE zfit0078-rmvct,   "Tp. Mov
         lifnr           TYPE zfit0078-lifnr,   "Fornecedor
         cod_imposto     TYPE zfit0078-cod_imposto,   "Cod. Imposto
         xref3           TYPE bsak-xref3,
         id_tipo_invoice TYPE zfit0078-id_tipo_invoice,
         bukrs           TYPE zfit0121-bukrs,
         belnr           TYPE zfit0121-belnr,
         gjahr           TYPE zfit0121-gjahr,
         buzei           TYPE zfit0121-buzei,
       END OF ty_flx_par,

       BEGIN OF ty_sakto,
         sakto TYPE ekkn-sakto,
         menge TYPE ekkn-menge,
         perc  TYPE p DECIMALS 10,
       END OF ty_sakto,

       BEGIN OF ty_matkl,
         matkl   TYPE ekpo-matkl,
         netwr   TYPE ekkn-netwr,
         perc    TYPE p DECIMALS 10,
         cod_flx TYPE zfit0078-cod_flx,
         shkzg   TYPE zfit0078-shkzg,
       END OF ty_matkl,

       BEGIN OF ty_bkpf,
         bukrs LIKE bkpf-bukrs,
         belnr LIKE bkpf-belnr,
         gjahr LIKE bkpf-gjahr,
         stblg LIKE bkpf-stblg,
         awkey LIKE bkpf-awkey,
         blart LIKE bkpf-blart,
         budat LIKE bkpf-budat,
         kursf LIKE bkpf-kursf,
         kurs2 LIKE bkpf-kurs2,
         waers LIKE bkpf-waers,
       END OF ty_bkpf,

       BEGIN OF ty_bsis,
         bukrs     LIKE bsis-bukrs,
         belnr     LIKE bsis-belnr,
         gjahr     LIKE bsis-gjahr,
         hkont     LIKE bsis-hkont,
         cta_part  TYPE zfit0078-cta_part,
         lcto_comp TYPE zfit0078-lcto_comp,
         dmbtr     TYPE bsis-dmbtr,
         dmbe2     TYPE bsis-dmbe2,
         waers     TYPE bsis-waers,
         wrbtr     TYPE bsis-wrbtr,
       END OF ty_bsis,

       BEGIN OF ty_bsas,
         bukrs     LIKE bsas-bukrs,
         belnr     LIKE bsas-belnr,
         gjahr     LIKE bsas-gjahr,
         hkont     LIKE bsas-hkont,
         cta_part  TYPE zfit0078-cta_part,
         lcto_comp TYPE zfit0078-lcto_comp,
         dmbtr     TYPE bsis-dmbtr,
         dmbe2     TYPE bsis-dmbe2,
         waers     TYPE bsis-waers,
         wrbtr     TYPE bsis-wrbtr,
       END OF ty_bsas.

*DATA: BEGIN OF TG_BSAK OCCURS 0,
*        BUKRS   LIKE BSAK-BUKRS,
*        LIFNR   LIKE BSAK-LIFNR,
*        BUDAT   LIKE BSAK-BUDAT,
*        GJAHR   LIKE BSAK-GJAHR,
*        BELNR   LIKE BSAK-BELNR,
*        BUZEI   LIKE BSAK-BUZEI,
*        BLDAT   LIKE BSAK-BLDAT,
*        WAERS   LIKE BSAK-WAERS,
*        XBLNR   LIKE BSAK-XBLNR,
*        BLART   LIKE BSAK-BLART,
*        GSBER   LIKE BSAK-GSBER,
*        EBELN   LIKE BSAK-EBELN,
*        EBELP   LIKE BSAK-EBELP,
*        BSCHL   LIKE BSAK-BSCHL,
*        SHKZG   LIKE BSAK-SHKZG,
*        ZFBDT   LIKE BSAK-ZFBDT,
*        ZBD1T   LIKE BSAK-ZBD1T,
*        AUGBL   LIKE BSAK-AUGBL,
*        AUGDT   LIKE BSAK-AUGDT,
*        DMBTR   LIKE BSAK-DMBTR,
*        DMBE2   LIKE BSAK-DMBE2,
*        KIDNO   LIKE BSAK-KIDNO,
*        ZUONR   LIKE BSAK-ZUONR,
*        HKONT   LIKE BSAK-HKONT,
*        SAKNR   LIKE BSAK-SAKNR,
*        VENCI   LIKE BSAK-ZFBDT,
*        XCONTA  LIKE EKKN-SAKTO,
*        XTOT    LIKE EKPO-NETWR,
*        XTOP    LIKE EKPO-NETWR,
*        XVLRR   LIKE EKPO-NETWR,
*        XVLR_R  LIKE EKPO-NETWR,
*        XVLR_U  LIKE EKPO-NETWR,
*      END OF TG_BSAK,
*
*      BEGIN OF TG_BSAK_BSD OCCURS 0,
*        BUKRS   LIKE BSAK-BUKRS,
*        LIFNR   LIKE BSAK-LIFNR,
*        BUDAT   LIKE BSAK-BUDAT,
*        GJAHR   LIKE BSAK-GJAHR,
*        BELNR   LIKE BSAK-BELNR,
*        BUZEI   LIKE BSAK-BUZEI,
*        BLDAT   LIKE BSAK-BLDAT,
*        WAERS   LIKE BSAK-WAERS,
*        XBLNR   LIKE BSAK-XBLNR,
*        BLART   LIKE BSAK-BLART,
*        GSBER   LIKE BSAK-GSBER,
*        EBELN   LIKE BSAK-EBELN,
*        EBELP   LIKE BSAK-EBELP,
*        BSCHL   LIKE BSAK-BSCHL,
*        SHKZG   LIKE BSAK-SHKZG,
*        ZFBDT   LIKE BSAK-ZFBDT,
*        ZBD1T   LIKE BSAK-ZBD1T,
*        AUGBL   LIKE BSAK-AUGBL,
*        AUGDT   LIKE BSAK-AUGDT,
*        DMBTR   LIKE BSAK-DMBTR,
*        DMBE2   LIKE BSAK-DMBE2,
*        KIDNO   LIKE BSAK-KIDNO,
*        ZUONR   LIKE BSAK-ZUONR,
*        HKONT   LIKE BSAK-HKONT,
*        SAKNR   LIKE BSAK-SAKNR,
*        VENCI   LIKE BSAK-ZFBDT,
*        XCONTA  LIKE EKKN-SAKTO,
*        XTOT    LIKE EKPO-NETWR,
*        XTOP    LIKE EKPO-NETWR,
*        XVLRR   LIKE EKPO-NETWR,
*        XVLR_R  LIKE EKPO-NETWR,
*        XVLR_U  LIKE EKPO-NETWR,
*      END OF TG_BSAK_BSD,

DATA: BEGIN OF tg_bsad OCCURS 0,
        bukrs LIKE bsad-bukrs,
        kunnr LIKE bsad-kunnr,
        budat LIKE bsad-budat,
        gjahr LIKE bsad-gjahr,
        belnr LIKE bsad-belnr,
        buzei LIKE bsad-buzei,
        bldat LIKE bsad-bldat,
        waers LIKE bsad-waers,
        xblnr LIKE bsad-xblnr,
        blart LIKE bsad-blart,
        gsber LIKE bsad-gsber,
        vbeln LIKE bsad-vbeln,
        bschl LIKE bsad-bschl,
        shkzg LIKE bsad-shkzg,
        zfbdt LIKE bsad-zfbdt,
        zbd1t LIKE bsad-zbd1t,
        augbl LIKE bsad-augbl,
        augdt LIKE bsad-augdt,
        dmbtr LIKE bsad-dmbtr,
        dmbe2 LIKE bsad-dmbe2,
        wrbtr TYPE bsad-wrbtr,
        kidno LIKE bsad-kidno,
        zuonr LIKE bsad-zuonr,
        hkont LIKE bsad-hkont,
        saknr LIKE bsad-saknr,
        vbel2 LIKE bsad-vbel2,
        posn2 LIKE bsad-posn2,
        xref3 LIKE bsad-xref3,
        rebzg LIKE bsad-rebzg,
        "LCTO_COMP TYPE ZFIT0078-LCTO_COMP,
        venci LIKE bsad-zfbdt,
      END OF tg_bsad,

      BEGIN OF tg_bsid OCCURS 0,
        bukrs LIKE bsid-bukrs,
        kunnr LIKE bsid-kunnr,
        budat LIKE bsid-budat,
        gjahr LIKE bsid-gjahr,
        belnr LIKE bsid-belnr,
        buzei LIKE bsid-buzei,
        bldat LIKE bsid-bldat,
        waers LIKE bsid-waers,
        xblnr LIKE bsid-xblnr,
        blart LIKE bsid-blart,
        gsber LIKE bsid-gsber,
        vbeln LIKE bsid-vbeln,
        bschl LIKE bsid-bschl,
        shkzg LIKE bsid-shkzg,
        zfbdt LIKE bsid-zfbdt,
        zbd1t LIKE bsid-zbd1t,
        augbl LIKE bsid-augbl,
        augdt LIKE bsid-augdt,
        dmbtr LIKE bsid-dmbtr,
        dmbe2 LIKE bsid-dmbe2,
        kidno LIKE bsid-kidno,
        zuonr LIKE bsid-zuonr,
        hkont LIKE bsid-hkont,
        saknr LIKE bsid-saknr,
        vbel2 LIKE bsid-vbel2,
        posn2 LIKE bsid-posn2,
        venci LIKE bsid-zfbdt,
      END OF tg_bsid,

      BEGIN OF tg_vbap OCCURS 0,
        vbeln LIKE vbap-vbeln,
        posnr LIKE vbap-posnr,
        matkl LIKE vbap-matkl,
      END OF tg_vbap,

      BEGIN OF tg_bsis OCCURS 0,
        bukrs LIKE bsis-bukrs,
        budat LIKE bsis-budat,
        belnr LIKE bsis-belnr,
        gjahr LIKE bsis-gjahr,
        buzei LIKE bsis-buzei,
        hkont LIKE bsis-hkont,
        bldat LIKE bsis-bldat,
        waers LIKE bsis-waers,
        xblnr LIKE bsis-xblnr,
        blart LIKE bsis-blart,
        gsber LIKE bsis-gsber,
        bschl LIKE bsis-bschl,
        shkzg LIKE bsis-shkzg,
        zfbdt LIKE bsis-zfbdt,
        augbl LIKE bsis-augbl,
        augdt LIKE bsis-augdt,
        dmbtr LIKE bsis-dmbtr,
        dmbe2 LIKE bsis-dmbe2,
        kidno LIKE bsis-kidno,
        zuonr LIKE bsis-zuonr,
      END OF tg_bsis,

      BEGIN OF tg_skb1 OCCURS 0,
        bukrs LIKE skb1-bukrs,
        saknr LIKE skb1-saknr,
        fdlev LIKE skb1-fdlev,
      END OF tg_skb1,

      BEGIN OF tg_fagl OCCURS 0,
        ryear  LIKE faglflexa-ryear,
        rldnr  LIKE faglflexa-rldnr,
        docnr  LIKE faglflexa-docnr,
        rbukrs LIKE faglflexa-rbukrs,
        bschl  LIKE faglflexa-bschl,
        racct  LIKE faglflexa-racct,
        hsl    LIKE faglflexa-hsl,
        ksl    LIKE faglflexa-ksl,
        drcrk  LIKE faglflexa-drcrk,
        buzei  LIKE faglflexa-buzei,
        usnam  LIKE faglflexa-usnam,
        rmvct  LIKE faglflexa-rmvct,
      END OF tg_fagl,

      BEGIN OF tg_ekkn OCCURS 0,
        ebeln LIKE ekkn-ebeln,
        ebelp LIKE ekkn-ebelp,
        sakto LIKE ekkn-sakto,
        menge LIKE ekkn-menge,
      END OF tg_ekkn,

      BEGIN OF tg_ekpo OCCURS 0,
        ebeln LIKE ekpo-ebeln,
        ebelp LIKE ekpo-ebelp,
        netwr LIKE ekpo-netwr,
        brtwr LIKE ekpo-brtwr,
        knttp LIKE ekpo-knttp,
        matkl LIKE ekpo-matkl,
      END OF tg_ekpo,

      BEGIN OF tg_0036 OCCURS 0,
        obj_key         LIKE zfit0036-obj_key,
        id_tipo_invoice LIKE zfit0036-id_tipo_invoice,
        matnr           LIKE zfit0036-matnr,
        matkl           LIKE mara-matkl,
        bukrs           TYPE bkpf-bukrs,
        belnr           TYPE bkpf-belnr,
        gjahr           TYPE bkpf-gjahr,
      END OF tg_0036,

      BEGIN OF tg_mara OCCURS 0,
        matnr LIKE mara-matnr,
        matkl LIKE mara-matkl,
      END OF tg_mara.

DATA: it_bsid TYPE TABLE OF bsid,
      wa_bsid TYPE bsid,
      it_bsik TYPE TABLE OF bsik,
      wa_bsik TYPE bsik,
      wl_bsis TYPE bsis.

DATA: tg_0080        LIKE zfit0080 OCCURS 0 WITH HEADER LINE,
      tg_0078        LIKE zfit0078 OCCURS 0 WITH HEADER LINE,
      tg_0078_bsad   TYPE ty_0078_bsad OCCURS 0 WITH HEADER LINE,
      tg_0078_bsak   TYPE ty_0078_bsak OCCURS 0 WITH HEADER LINE,
      tg_0078_r      LIKE zfit0078 OCCURS 0 WITH HEADER LINE,
      tg_0078_l      LIKE zfit0078 OCCURS 0 WITH HEADER LINE,
      tg_sakto       TYPE ty_sakto OCCURS 0 WITH HEADER LINE,
      tg_matkl       TYPE ty_matkl OCCURS 0 WITH HEADER LINE,
      tg_matkl_aux   TYPE ty_matkl OCCURS 0 WITH HEADER LINE,
      tg_bkpf        TYPE ty_bkpf  OCCURS 0 WITH HEADER LINE,
      tg_bkpf_comp   TYPE ty_bkpf  OCCURS 0 WITH HEADER LINE,
      tg_bsak        TYPE ty_bsak  OCCURS 0 WITH HEADER LINE,
      tg_rseg        TYPE ty_rseg  OCCURS 0 WITH HEADER LINE,
      tg_bsak_bsd    TYPE ty_bsak  OCCURS 0 WITH HEADER LINE,
      tg_bsad_rsd    LIKE TABLE OF tg_bsad WITH HEADER LINE,
      tg_bsad_aux    LIKE TABLE OF tg_bsad WITH HEADER LINE,
      tg_bsak_rsd    LIKE TABLE OF tg_bsak WITH HEADER LINE,
      tg_bsak_aux    LIKE TABLE OF tg_bsak WITH HEADER LINE,
      tg_bsis_cbanco TYPE TABLE OF ty_bsis WITH HEADER LINE,
      "TG_BSIS_RAZAO  TYPE TABLE OF TY_BSIS WITH HEADER LINE,
      "TG_BSAS_RAZAO  TYPE TABLE OF TY_BSAS WITH HEADER LINE,
      tg_bsis_set    TYPE TABLE OF ty_bsis WITH HEADER LINE,
      tg_bsas_set    TYPE TABLE OF ty_bsas WITH HEADER LINE.

DATA: wa_flx_par TYPE ty_flx_par,
      wl_cls_ped TYPE ty_cls_ped.

DATA: vl_data    TYPE sy-datum,
      vl_hora    TYPE sy-uzeit,
      vg_tx_conv TYPE p DECIMALS 10.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: v_cx TYPE c VALUE 'X'.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
* Tela de Seleção
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR bsak-bukrs OBLIGATORY,
                  "S_LIFNR FOR BSAK-LIFNR,
                  s_budat FOR bsak-budat OBLIGATORY NO-EXTENSION,
**<<<------"187070 - NMS - INI------>>>
                  s_belnr FOR bsak-belnr OBLIGATORY NO INTERVALS MODIF ID grl.
**<<<------"187070 - NMS - FIM------>>>

  PARAMETERS: p_clc_ds TYPE c AS CHECKBOX.
**<<<------"187070 - NMS - INI------>>>
  SELECTION-SCREEN SKIP.
* Tipo de execução
  SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: rb_geral RADIOBUTTON GROUP gp1 USER-COMMAND grl DEFAULT 'X',
                rb_indiv RADIOBUTTON GROUP gp1.

  SELECTION-SCREEN: END OF BLOCK b2.
**<<<------"187070 - NMS - FIM------>>>
SELECTION-SCREEN: END OF BLOCK b1.
**<<<------"187070 - NMS - INI------>>>
*----------------------------------------------------------------------*
* I N I T I A L I Z A T I O N
*----------------------------------------------------------------------*
INITIALIZATION.
* Restringe as opções da tela de seleção
  PERFORM zf_limit_select_option.
*--------------------------------------------------------------------*
* A T - S E L E C T I O N  S C R E E N                               *
*--------------------------------------------------------------------*
* O evento é executado para processar telas antes de serem exibidas.
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-required EQ 1.
      screen-required = 2.
      MODIFY SCREEN.

    ENDIF.

    IF screen-name CS 'S_BUDAT-HIGH'.
      screen-required = 2.
      MODIFY SCREEN.

    ENDIF.

    CASE abap_on.
      WHEN rb_geral. "Execução Gerarl
        IF screen-group1 EQ 'GRL'.
          screen-invisible = 1.
          screen-input     = 0.
          MODIFY SCREEN.
          CLEAR: s_belnr, s_belnr[].

        ENDIF.
      WHEN rb_indiv. "Execução Individual
        IF screen-group1 EQ 'GRL'.
          screen-invisible = 0.
          screen-input     = 1.
          MODIFY SCREEN.

        ENDIF.
      WHEN OTHERS.
*     Do nothing
    ENDCASE.

  ENDLOOP.
* O evento é executado quando passa pelo respectivo campo.
AT SELECTION-SCREEN ON s_bukrs.
* Configuração e validação do respectivo campo acionado.
  PERFORM zf_setting_verify_field USING s_bukrs-low.
* O evento é executado quando passa pelo respectivo campo.
AT SELECTION-SCREEN ON s_budat.
* Configuração e validação do respectivo campo acionado.
  PERFORM zf_setting_verify_field USING s_budat-low.
* Configuração e validação do respectivo campo acionado.
  PERFORM zf_setting_verify_field USING s_budat-high.
**<<<------"187070 - NMS - FIM------>>>
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM seleciona_dados.
  PERFORM processa_dados.
  PERFORM grava_dados.


FORM seleciona_dados .

  DATA: vl_data_high TYPE bsak-budat.

  PERFORM iniciar_variaveis.

*-----------------------------------------------------------------*
*  Busca Parametros Cta. Partida Razão
*-----------------------------------------------------------------*
  SELECT *
    FROM zfit0078 INTO TABLE tg_0078_r
   WHERE cta_part = 'R'.

*-----------------------------------------------------------------*
*  Busca Parametros. Cta. que utilizam Data de Lançamento como Referencia.
*-----------------------------------------------------------------*
  SELECT *
    FROM zfit0078 INTO TABLE tg_0078_l
   WHERE lcto_comp = 'L'.

*-----------------------------------------------------------------*
*  Busca Parametros
*-----------------------------------------------------------------*
  SELECT *
    FROM zfit0078 INTO TABLE tg_0078.

*-----------------------------------------------------------------*
*  Busca Contas Par. SET
*-----------------------------------------------------------------*

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZFI0059_CTAS'
    TABLES
      set_values    = t_conta
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  SORT t_conta BY from.

  LOOP AT t_conta.
    IF ( t_conta-from IS NOT INITIAL ).
      wa_contas-hkont = t_conta-from(10).
      APPEND wa_contas TO it_contas.
    ENDIF.
    CLEAR: wa_contas.
  ENDLOOP.

  it_gjahr-sign   = 'I'.
  it_gjahr-option = 'EQ'.
  it_gjahr-low    = s_budat-low(4).
  it_gjahr-high   = s_budat-low(4).

  IF s_budat-high IS NOT INITIAL..
    it_gjahr-option = 'BT'.
    it_gjahr-high   = s_budat-high(4).
  ENDIF.

  APPEND it_gjahr.

  "Monta Periodo por Ano.
  it_date-sign   = 'I'.
  it_date-option = 'BT'.

  CONCATENATE s_budat-low(4)  '0101' INTO it_date-low.

  IF s_budat-high IS NOT INITIAL.
    CONCATENATE s_budat-high(4) '1201' INTO vl_data_high.
  ELSE.
    CONCATENATE s_budat-low(4)  '1201' INTO vl_data_high.
  ENDIF.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = vl_data_high
    IMPORTING
      last_day_of_month = it_date-high.

  APPEND it_date.

  it_fdlev-sign   = 'I'.
  it_fdlev-option = 'EQ'.
  it_fdlev-low    = 'F0'.
  it_fdlev-high   = 'F0'.
  APPEND it_fdlev.

  it_fdlev-sign   = 'I'.
  it_fdlev-option = 'EQ'.
  it_fdlev-low    = 'B2'.
  it_fdlev-high   = 'B2'.
  APPEND it_fdlev.

*-----------------------------------------------------------------*
*  Busca Partidas Fornecedor Liquidadas
*-----------------------------------------------------------------*

  IF it_contas[] IS NOT INITIAL.

    SELECT *
      FROM bsid INTO TABLE it_bsid
       FOR ALL ENTRIES IN it_contas
     WHERE bukrs IN s_bukrs
       AND umsks NE 'W'
       AND gjahr IN it_gjahr
       AND belnr IN s_belnr          "<<<------"187070 - NMS ------->>>
       AND budat IN s_budat
       AND hkont EQ it_contas-hkont.

    IF it_bsid[] IS NOT INITIAL.

      SELECT a~bukrs a~lifnr a~budat a~gjahr a~belnr a~buzei a~bldat
             a~waers a~xblnr a~blart a~gsber a~ebeln a~ebelp
             a~bschl a~shkzg a~zfbdt a~zbd1t a~augbl a~augdt
             a~dmbtr a~dmbe2 a~kidno a~zuonr a~hkont a~saknr a~xref3
             a~wrbtr
        INTO CORRESPONDING FIELDS OF TABLE tg_bsak_bsd
        FROM bsak AS a INNER JOIN bkpf AS b ON b~bukrs = a~bukrs
                                           AND b~belnr = a~augbl
         FOR ALL ENTRIES IN it_bsid
       WHERE a~bukrs IN s_bukrs
         "AND A~LIFNR IN S_LIFNR
         "AND A~AUGDT IN S_BUDAT
         AND a~augdt IN it_date
         AND a~bukrs EQ it_bsid-bukrs
         AND a~belnr EQ it_bsid-belnr
         AND a~gjahr EQ it_bsid-gjahr
         AND a~belnr NE a~augbl
         AND b~gjahr IN it_gjahr
         AND b~budat IN s_budat.

    ENDIF.

  ENDIF.

*-----------------------------------------------------------------*
*  Busca Partidas Fornecedor - em Aberto
*-----------------------------------------------------------------*
  IF tg_0078_l[] IS NOT INITIAL.
    SELECT *
      FROM bsik INTO TABLE it_bsik
       FOR ALL ENTRIES IN tg_0078_l
     WHERE bukrs IN s_bukrs
       "AND LIFNR IN S_LIFNR
       AND gjahr IN it_gjahr
       AND belnr IN s_belnr          "<<<------"187070 - NMS ------->>>
       AND budat IN s_budat
       AND hkont EQ tg_0078_l-saknr.
  ENDIF.

*-----------------------------------------------------------------*
*  Busca de Contrapartida de Conta Razão
*-----------------------------------------------------------------*
  IF tg_0078_r[] IS NOT INITIAL.
    SELECT *
      FROM bsis INTO CORRESPONDING FIELDS OF TABLE tg_bsis
      FOR ALL ENTRIES IN tg_0078_r
     WHERE bukrs IN s_bukrs
       AND hkont EQ tg_0078_r-saknr
       AND gjahr IN it_gjahr
       AND belnr IN s_belnr          "<<<------"187070 - NMS ------->>>
       AND budat IN s_budat.
  ENDIF.
*-----------------------------------------------------------------*
*  Busca Partidas Clientes Liquidadas
*-----------------------------------------------------------------*

  "Busca Partidas (Utilizam Data Compensação como Referência)
  SELECT a~bukrs a~kunnr a~budat a~gjahr a~belnr a~buzei a~bldat a~waers a~xblnr a~blart a~gsber
         a~vbeln a~bschl a~shkzg a~zfbdt a~zbd1t a~augbl a~augdt a~dmbtr a~dmbe2 a~kidno a~zuonr
         a~hkont a~saknr a~vbel2 a~posn2 a~xref3 a~rebzg
         a~wrbtr
    INTO CORRESPONDING FIELDS OF TABLE tg_bsad
    FROM bsad AS a INNER JOIN bkpf AS b ON b~bukrs = a~bukrs
                                       AND b~belnr = a~augbl
   WHERE a~bukrs IN s_bukrs
     "AND A~KUNNR IN S_LIFNR
     AND a~umsks NE 'W'
     "AND A~AUGDT IN IT_DATE
     AND a~belnr NE a~augbl
     "AND B~GJAHR IN IT_GJAHR
     AND a~belnr IN s_belnr          "<<<------"187070 - NMS ------->>>
     AND ( ( b~budat IN s_budat ) OR
           ( a~budat IN s_budat ) ).

  IF tg_bsad[] IS NOT INITIAL.

    "Partidas Residuais Liquidadas
    SELECT a~bukrs a~kunnr a~budat a~gjahr a~belnr a~buzei a~bldat a~waers a~xblnr a~blart a~gsber
           a~vbeln a~bschl a~shkzg a~zfbdt a~zbd1t a~augbl a~augdt a~dmbtr a~dmbe2 a~kidno a~zuonr
           a~hkont a~saknr a~vbel2 a~posn2 a~xref3 a~rebzg
           a~wrbtr
      INTO CORRESPONDING FIELDS OF TABLE tg_bsad_rsd
      FROM bsad AS a
      FOR ALL ENTRIES IN tg_bsad
     WHERE a~bukrs EQ tg_bsad-bukrs
       AND a~kunnr EQ tg_bsad-kunnr
       AND a~umsks NE 'W'
       AND a~belnr EQ tg_bsad-augbl
       AND a~belnr NE a~augbl
       AND a~waers EQ tg_bsad-waers
       AND a~shkzg EQ tg_bsad-shkzg
       AND ( ( a~rebzg EQ tg_bsad-rebzg ) OR
             ( a~rebzg EQ tg_bsad-belnr ) ).

    "Partidas Residuais em Aberto
    SELECT a~bukrs a~kunnr a~budat a~gjahr a~belnr a~buzei a~bldat a~waers a~xblnr a~blart a~gsber
           a~vbeln a~bschl a~shkzg a~zfbdt a~zbd1t a~dmbtr a~dmbe2 a~kidno a~zuonr
           a~hkont a~saknr a~vbel2 a~posn2 a~xref3 a~rebzg
      APPENDING CORRESPONDING FIELDS OF TABLE tg_bsad_rsd
      FROM bsid AS a
      FOR ALL ENTRIES IN tg_bsad
     WHERE a~bukrs EQ tg_bsad-bukrs
       AND a~kunnr EQ tg_bsad-kunnr
       AND a~umsks NE 'W'
       AND a~belnr EQ tg_bsad-augbl
       AND a~waers EQ tg_bsad-waers
       AND a~shkzg EQ tg_bsad-shkzg
       AND ( ( a~rebzg EQ tg_bsad-rebzg ) OR
             ( a~rebzg EQ tg_bsad-belnr ) ).

  ENDIF.

  SORT tg_bsad_rsd BY bukrs kunnr belnr waers shkzg rebzg.

  "Busca Partidas (Utilizam Data Lcto como Referência)
*  SELECT A~BUKRS A~KUNNR A~BUDAT A~GJAHR A~BELNR A~BUZEI A~BLDAT A~WAERS A~XBLNR A~BLART A~GSBER
*         A~VBELN A~BSCHL A~SHKZG A~ZFBDT A~ZBD1T A~AUGBL A~AUGDT A~DMBTR A~DMBE2 A~KIDNO A~ZUONR
*         A~HKONT A~SAKNR A~VBEL2 A~POSN2 A~XREF3
*    FROM BSAD AS A INTO CORRESPONDING FIELDS OF TABLE TG_BSAD_AUX
*   WHERE A~BUKRS IN S_BUKRS
*     AND A~KUNNR IN S_LIFNR
*     AND A~BUDAT IN S_BUDAT
*     AND A~BELNR NE A~AUGBL.

*     AND (
*            EXISTS ( SELECT *
*                       FROM BSIS AS B INNER JOIN ZFIT0078 AS C ON B~HKONT = C~SAKNR
*                      WHERE B~BUKRS     EQ A~BUKRS
*                        AND B~GJAHR     EQ A~GJAHR
*                        AND B~BELNR     EQ A~BELNR
*                        AND C~LCTO_COMP EQ 'L' )
*            OR
*
*            EXISTS ( SELECT *
*                       FROM BSAS AS B INNER JOIN ZFIT0078 AS C ON B~HKONT = C~SAKNR
*                      WHERE B~BUKRS     EQ A~BUKRS
*                        AND B~GJAHR     EQ A~GJAHR
*                        AND B~BELNR     EQ A~BELNR
*                        AND C~LCTO_COMP EQ 'L' )
*         ).


*  LOOP AT TG_BSAD_AUX.
*    TG_BSAD_AUX-LCTO_COMP = 'L'. "Data de Lançamento
*    APPEND TG_BSAD_AUX TO TG_BSAD.
*  ENDLOOP.

*  SORT TG_BSAD BY BUKRS GJAHR BELNR  BUZEI.
*  DELETE ADJACENT DUPLICATES FROM TG_BSAD COMPARING BUKRS GJAHR BELNR  BUZEI.

*-----------------------------------------------------------------*
*  Busca Partidas Clientes - Em aberto
*-----------------------------------------------------------------*
  IF tg_0078_l[] IS NOT INITIAL.
    SELECT *
      FROM bsid INTO CORRESPONDING FIELDS OF TABLE tg_bsid
      FOR ALL ENTRIES IN tg_0078_l
     WHERE bukrs IN s_bukrs
       "AND KUNNR IN S_LIFNR
       AND umsks NE 'W'
       AND gjahr IN it_gjahr
       AND belnr IN s_belnr          "<<<------"187070 - NMS ------->>>
       AND budat IN s_budat
       AND hkont EQ tg_0078_l-saknr.
  ENDIF.

*-----------------------------------------------------------------*
*  Busca Partidas Fornecedor - Liquidadas
*-----------------------------------------------------------------*

  "Busca Partidas (Utilizam Data Compensação como Referência)
  SELECT a~bukrs a~lifnr a~budat a~gjahr a~belnr a~buzei a~bldat
         a~waers a~xblnr a~blart a~gsber a~ebeln a~ebelp
         a~bschl a~shkzg a~zfbdt a~zbd1t a~augbl a~augdt
         a~dmbtr a~dmbe2 a~kidno a~zuonr a~hkont a~saknr a~xref3 a~rebzg
         a~wrbtr
    INTO CORRESPONDING FIELDS OF TABLE tg_bsak
    FROM bsak AS a INNER JOIN bkpf AS b ON b~bukrs = a~bukrs
                                       AND b~belnr = a~augbl
   WHERE a~bukrs IN s_bukrs
     "AND A~LIFNR IN S_LIFNR
     "AND A~AUGDT IN S_BUDAT
     "AND A~AUGDT IN IT_DATE
     AND a~belnr NE a~augbl
     "AND B~GJAHR IN IT_GJAHR
     AND a~belnr IN s_belnr          "<<<------"187070 - NMS ------->>>
     AND ( ( b~budat IN s_budat ) OR
           ( a~budat IN s_budat ) ).

  IF tg_bsak[] IS NOT INITIAL.

    "Partidas Residuais Liquidadas
    SELECT a~bukrs a~lifnr a~budat a~gjahr a~belnr a~buzei a~bldat
           a~waers a~xblnr a~blart a~gsber a~ebeln a~ebelp
           a~bschl a~shkzg a~zfbdt a~zbd1t a~augbl a~augdt
           a~dmbtr a~dmbe2 a~kidno a~zuonr a~hkont a~saknr a~xref3 a~rebzg
           a~wrbtr
      INTO CORRESPONDING FIELDS OF TABLE tg_bsak_rsd
      FROM bsak AS a
      FOR ALL ENTRIES IN tg_bsak
     WHERE a~bukrs EQ tg_bsak-bukrs
       AND a~lifnr EQ tg_bsak-lifnr
       AND a~belnr EQ tg_bsak-augbl
       AND a~belnr NE a~augbl
       AND a~waers EQ tg_bsak-waers
       AND a~shkzg EQ tg_bsak-shkzg
       AND ( ( a~rebzg EQ tg_bsak-rebzg ) OR
             ( a~rebzg EQ tg_bsak-belnr ) ).

    "Partidas Residuais Aberto
    SELECT a~bukrs a~lifnr a~budat a~gjahr a~belnr a~buzei a~bldat
           a~waers a~xblnr a~blart a~gsber a~ebeln a~ebelp
           a~bschl a~shkzg a~zfbdt a~zbd1t
           a~dmbtr a~dmbe2 a~kidno a~zuonr a~hkont a~saknr a~xref3 a~rebzg
      APPENDING CORRESPONDING FIELDS OF TABLE tg_bsak_rsd
      FROM bsik AS a
      FOR ALL ENTRIES IN tg_bsak
     WHERE a~bukrs EQ tg_bsak-bukrs
       AND a~lifnr EQ tg_bsak-lifnr
       AND a~belnr EQ tg_bsak-augbl
       AND a~waers EQ tg_bsak-waers
       AND a~shkzg EQ tg_bsak-shkzg
       AND ( ( a~rebzg EQ tg_bsak-rebzg ) OR
             ( a~rebzg EQ tg_bsak-belnr ) ).

  ENDIF.

  SORT tg_bsak_rsd BY bukrs lifnr belnr waers shkzg rebzg.



  "Busca Partidas (Utilizam Data Lançamento como Referência)
*  SELECT A~BUKRS A~LIFNR A~BUDAT A~GJAHR A~BELNR A~BUZEI A~BLDAT
*         A~WAERS A~XBLNR A~BLART A~GSBER A~EBELN A~EBELP
*         A~BSCHL A~SHKZG A~ZFBDT A~ZBD1T A~AUGBL A~AUGDT
*         A~DMBTR A~DMBE2 A~KIDNO A~ZUONR A~HKONT A~SAKNR A~XREF3
*    FROM BSAK AS A INTO CORRESPONDING FIELDS OF TABLE TG_BSAK_AUX
*   WHERE A~BUKRS IN S_BUKRS
*     AND A~LIFNR IN S_LIFNR
*     AND A~BUDAT IN S_BUDAT
*     AND A~BELNR NE A~AUGBL.

*     AND (
*            EXISTS ( SELECT *
*                       FROM BSIS AS B INNER JOIN ZFIT0078 AS C ON B~HKONT = C~SAKNR
*                      WHERE B~BUKRS     EQ A~BUKRS
*                        AND B~GJAHR     EQ A~GJAHR
*                        AND B~BELNR     EQ A~BELNR
*                        AND C~LCTO_COMP EQ 'L' )
*            OR
*
*            EXISTS ( SELECT *
*                       FROM BSAS AS B INNER JOIN ZFIT0078 AS C ON B~HKONT = C~SAKNR
*                      WHERE B~BUKRS     EQ A~BUKRS
*                        AND B~GJAHR     EQ A~GJAHR
*                        AND B~BELNR     EQ A~BELNR
*                        AND C~LCTO_COMP EQ 'L' )
*         ).

*  LOOP AT TG_BSAK_AUX.
*    TG_BSAK_AUX-LCTO_COMP = 'L'. "Data de Lançamento
*    APPEND TG_BSAK_AUX TO TG_BSAK.
*  ENDLOOP.
*
*  SORT TG_BSAK BY BUKRS GJAHR BELNR  BUZEI.
*  DELETE ADJACENT DUPLICATES FROM TG_BSAK COMPARING BUKRS GJAHR BELNR  BUZEI.

*-----------------------------------------------------------------*
*  Busca dados Tabelas Auxiliares
*-----------------------------------------------------------------*
  IF tg_bsak[] IS NOT INITIAL.

    "Busca dados BKPF
*    SELECT BUKRS BELNR GJAHR STBLG AWKEY
*      FROM BKPF APPENDING TABLE TG_BKPF
*      FOR ALL ENTRIES IN TG_BSAK
*     WHERE BUKRS = TG_BSAK-BUKRS
*       AND BELNR = TG_BSAK-BELNR
*       AND GJAHR = TG_BSAK-GJAHR.

    SELECT ryear rldnr docnr rbukrs bschl
           racct hsl ksl drcrk buzei usnam rmvct
      FROM faglflexa APPENDING CORRESPONDING FIELDS OF TABLE tg_fagl
      FOR ALL ENTRIES IN tg_bsak
     WHERE ryear   EQ tg_bsak-gjahr
       AND docnr   EQ tg_bsak-belnr
       AND rldnr   EQ '0L'
       AND rbukrs  EQ tg_bsak-bukrs
       AND buzei   EQ tg_bsak-buzei.

    SELECT ebeln ebelp sakto menge
      FROM ekkn APPENDING CORRESPONDING FIELDS OF TABLE tg_ekkn
       FOR ALL ENTRIES IN tg_bsak
     WHERE ebeln EQ tg_bsak-ebeln.

    SELECT ebeln ebelp netwr brtwr knttp matkl
      FROM ekpo APPENDING CORRESPONDING FIELDS OF TABLE tg_ekpo
       FOR ALL ENTRIES IN tg_bsak
     WHERE ebeln EQ tg_bsak-ebeln
       AND loekz = ''.

    SELECT a~obj_key a~id_tipo_invoice a~matnr b~bukrs b~belnr b~gjahr
      APPENDING CORRESPONDING FIELDS OF TABLE tg_0036
      FROM zfit0036 AS a INNER JOIN bkpf AS b ON a~obj_key = b~awkey
      FOR ALL ENTRIES IN tg_bsak
     WHERE b~bukrs = tg_bsak-bukrs
       AND b~belnr = tg_bsak-belnr
       AND b~gjahr = tg_bsak-gjahr.

    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
           bsis~waers bsis~wrbtr
           APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_cbanco
      FROM bsis INNER JOIN skb1 ON bsis~bukrs = skb1~bukrs "#EC CI_DB_OPERATION_OK[2431747]
                               AND bsis~hkont = skb1~saknr
       FOR ALL ENTRIES IN tg_bsak
     WHERE bsis~bukrs EQ tg_bsak-bukrs
       AND bsis~gjahr EQ tg_bsak-augdt(4)
       AND bsis~belnr EQ tg_bsak-augbl
       AND skb1~fdlev IN it_fdlev.

    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
           bsis~waers bsis~wrbtr
           APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_cbanco
      FROM bsis INNER JOIN skb1 ON bsis~bukrs = skb1~bukrs "#EC CI_DB_OPERATION_OK[2431747]
                               AND bsis~hkont = skb1~saknr
       FOR ALL ENTRIES IN tg_bsak
     WHERE bsis~bukrs EQ tg_bsak-bukrs
       AND bsis~gjahr EQ tg_bsak-gjahr
       AND bsis~belnr EQ tg_bsak-belnr
       AND skb1~fdlev IN it_fdlev.

    SELECT bukrs belnr gjahr hkont dmbtr dmbe2
           bsis~waers bsis~wrbtr
      FROM bsis APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_set
      FOR ALL ENTRIES IN tg_bsak
     WHERE bukrs  EQ tg_bsak-bukrs
       AND gjahr  EQ tg_bsak-augdt(4)
       AND belnr  EQ tg_bsak-augbl.

    SELECT bukrs belnr gjahr hkont dmbtr dmbe2
           bsas~waers bsas~wrbtr
      FROM bsas APPENDING CORRESPONDING FIELDS OF TABLE tg_bsas_set
      FOR ALL ENTRIES IN tg_bsak
     WHERE bukrs  EQ tg_bsak-bukrs
       AND gjahr  EQ tg_bsak-augdt(4)
       AND belnr  EQ tg_bsak-augbl.

    "Check Documento Lcto no Set.
    SELECT bukrs belnr gjahr hkont dmbtr dmbe2
           bsis~waers bsis~wrbtr
      FROM bsis APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_set
      FOR ALL ENTRIES IN tg_bsak
     WHERE bukrs  EQ tg_bsak-bukrs
       AND gjahr  EQ tg_bsak-gjahr
       AND belnr  EQ tg_bsak-belnr.

    SELECT bukrs belnr gjahr hkont dmbtr dmbe2
           bsas~waers bsas~wrbtr
      FROM bsas APPENDING CORRESPONDING FIELDS OF TABLE tg_bsas_set
      FOR ALL ENTRIES IN tg_bsak
     WHERE bukrs  EQ tg_bsak-bukrs
       AND gjahr  EQ tg_bsak-gjahr
       AND belnr  EQ tg_bsak-belnr.
    "Fim

  ENDIF.

  IF tg_bsak_bsd[] IS NOT INITIAL.

    "Busca dados BKPF
*    SELECT BUKRS BELNR GJAHR STBLG AWKEY
*      FROM BKPF APPENDING TABLE TG_BKPF
*       FOR ALL ENTRIES IN TG_BSAK_BSD
*     WHERE BUKRS = TG_BSAK_BSD-BUKRS
*       AND BELNR = TG_BSAK_BSD-BELNR
*       AND GJAHR = TG_BSAK_BSD-GJAHR.

    SELECT ryear rldnr docnr rbukrs bschl
           racct hsl ksl drcrk buzei usnam rmvct
      FROM faglflexa APPENDING CORRESPONDING FIELDS OF TABLE tg_fagl
      FOR ALL ENTRIES IN tg_bsak_bsd
     WHERE ryear   EQ tg_bsak_bsd-gjahr
       AND docnr   EQ tg_bsak_bsd-belnr
       AND rldnr   EQ '0L'
       AND rbukrs  EQ tg_bsak_bsd-bukrs
       AND buzei   EQ tg_bsak_bsd-buzei.

    SELECT ebeln ebelp sakto menge
      FROM ekkn APPENDING TABLE tg_ekkn
       FOR ALL ENTRIES IN tg_bsak_bsd
     WHERE ebeln EQ tg_bsak_bsd-ebeln.

    SELECT ebeln ebelp netwr brtwr knttp matkl
      FROM ekpo APPENDING CORRESPONDING FIELDS OF TABLE tg_ekpo
       FOR ALL ENTRIES IN tg_bsak_bsd
     WHERE ebeln EQ tg_bsak_bsd-ebeln
       AND loekz = ''.

    SELECT a~obj_key a~id_tipo_invoice a~matnr b~bukrs b~belnr b~gjahr
      APPENDING CORRESPONDING FIELDS OF TABLE tg_0036
      FROM zfit0036 AS a INNER JOIN bkpf AS b ON a~obj_key = b~awkey
      FOR ALL ENTRIES IN tg_bsak_bsd
     WHERE b~bukrs = tg_bsak_bsd-bukrs
       AND b~belnr = tg_bsak_bsd-belnr
       AND b~gjahr = tg_bsak_bsd-gjahr.

  ENDIF.

  IF it_bsik[] IS NOT INITIAL.

    SELECT ebeln ebelp sakto menge
      FROM ekkn APPENDING TABLE tg_ekkn
       FOR ALL ENTRIES IN it_bsik
     WHERE ebeln EQ it_bsik-ebeln.

    SELECT ebeln ebelp netwr brtwr knttp matkl
      FROM ekpo APPENDING CORRESPONDING FIELDS OF TABLE tg_ekpo
       FOR ALL ENTRIES IN it_bsik
     WHERE ebeln EQ it_bsik-ebeln
       AND loekz = ''.

    SELECT a~obj_key a~id_tipo_invoice a~matnr b~bukrs b~belnr b~gjahr
      APPENDING CORRESPONDING FIELDS OF TABLE tg_0036
      FROM zfit0036 AS a INNER JOIN bkpf AS b ON a~obj_key = b~awkey
      FOR ALL ENTRIES IN it_bsik
     WHERE b~bukrs = it_bsik-bukrs
       AND b~belnr = it_bsik-belnr
       AND b~gjahr = it_bsik-gjahr.

    "Busca dados BKPF
*    SELECT BUKRS BELNR GJAHR STBLG AWKEY
*      FROM BKPF APPENDING TABLE TG_BKPF
*       FOR ALL ENTRIES IN IT_BSIK
*     WHERE BUKRS = IT_BSIK-BUKRS
*       AND BELNR = IT_BSIK-BELNR
*       AND GJAHR = IT_BSIK-GJAHR.

    "Busca dados FAGLFLEXA
    SELECT ryear rldnr docnr rbukrs bschl
           racct hsl ksl drcrk buzei usnam rmvct
      FROM faglflexa APPENDING CORRESPONDING FIELDS OF TABLE tg_fagl
      FOR ALL ENTRIES IN it_bsik
    WHERE ryear   EQ it_bsik-gjahr
      AND docnr   EQ it_bsik-belnr
      AND rldnr   EQ '0L'
      AND rbukrs  EQ it_bsik-bukrs
      AND buzei   EQ it_bsik-buzei.

    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
           bsis~waers bsis~wrbtr
           APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_cbanco
      FROM bsis INNER JOIN skb1 ON bsis~bukrs = skb1~bukrs "#EC CI_DB_OPERATION_OK[2431747]
                               AND bsis~hkont = skb1~saknr
       FOR ALL ENTRIES IN it_bsik
     WHERE bsis~bukrs EQ it_bsik-bukrs
       AND bsis~gjahr EQ it_bsik-gjahr
       AND bsis~belnr EQ it_bsik-belnr
       AND skb1~fdlev IN it_fdlev.

  ENDIF.

  IF tg_bsad[] IS NOT INITIAL.
    "Busca dados BKPF
*    SELECT BUKRS BELNR GJAHR STBLG AWKEY
*      FROM BKPF APPENDING TABLE TG_BKPF
*       FOR ALL ENTRIES IN TG_BSAD
*     WHERE BUKRS = TG_BSAD-BUKRS
*       AND BELNR = TG_BSAD-BELNR
*       AND GJAHR = TG_BSAD-GJAHR.

    SELECT ryear rldnr docnr rbukrs bschl
           racct hsl ksl drcrk buzei usnam rmvct
      FROM faglflexa APPENDING CORRESPONDING FIELDS OF TABLE tg_fagl
      FOR ALL ENTRIES IN tg_bsad
    WHERE ryear   EQ tg_bsad-gjahr
      AND docnr   EQ tg_bsad-belnr
      AND rldnr   EQ '0L'
      AND rbukrs  EQ tg_bsad-bukrs
      AND buzei   EQ tg_bsad-buzei.

    SELECT *
      FROM vbap APPENDING CORRESPONDING FIELDS OF TABLE tg_vbap
      FOR ALL ENTRIES IN tg_bsad
     WHERE vbeln = tg_bsad-vbel2
       AND posnr = tg_bsad-posn2.

    SELECT a~obj_key a~id_tipo_invoice a~matnr b~bukrs b~belnr b~gjahr
      APPENDING CORRESPONDING FIELDS OF TABLE tg_0036
      FROM zfit0036 AS a INNER JOIN bkpf AS b ON a~obj_key = b~awkey
      FOR ALL ENTRIES IN tg_bsad
     WHERE b~bukrs = tg_bsad-bukrs
       AND b~belnr = tg_bsad-belnr
       AND b~gjahr = tg_bsad-gjahr.

    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
           bsis~waers bsis~wrbtr
           APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_cbanco
      FROM bsis INNER JOIN skb1 ON bsis~bukrs = skb1~bukrs "#EC CI_DB_OPERATION_OK[2431747]
                               AND bsis~hkont = skb1~saknr
       FOR ALL ENTRIES IN tg_bsad
     WHERE bsis~bukrs EQ tg_bsad-bukrs
       AND bsis~gjahr EQ tg_bsad-augdt(4)
       AND bsis~belnr EQ tg_bsad-augbl
       AND skb1~fdlev IN it_fdlev.

    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
           bsis~waers bsis~wrbtr
           APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_cbanco
      FROM bsis INNER JOIN skb1 ON bsis~bukrs = skb1~bukrs "#EC CI_DB_OPERATION_OK[2431747]
                               AND bsis~hkont = skb1~saknr
       FOR ALL ENTRIES IN tg_bsad
     WHERE bsis~bukrs EQ tg_bsad-bukrs
       AND bsis~gjahr EQ tg_bsad-gjahr
       AND bsis~belnr EQ tg_bsad-belnr
       AND skb1~fdlev IN it_fdlev.

    SELECT bukrs belnr gjahr hkont dmbtr dmbe2
           bsis~waers bsis~wrbtr
      FROM bsis APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_set
      FOR ALL ENTRIES IN tg_bsad
     WHERE bukrs  EQ tg_bsad-bukrs
       AND gjahr  EQ tg_bsad-augdt(4)
       AND belnr  EQ tg_bsad-augbl.

    SELECT bukrs belnr gjahr hkont dmbtr dmbe2
           bsas~waers bsas~wrbtr
      FROM bsas APPENDING CORRESPONDING FIELDS OF TABLE tg_bsas_set
      FOR ALL ENTRIES IN tg_bsad
     WHERE bukrs  EQ tg_bsad-bukrs
       AND gjahr  EQ tg_bsad-augdt(4)
       AND belnr  EQ tg_bsad-augbl.

    "Check Documento Lcto no Set.
    SELECT bukrs belnr gjahr hkont dmbtr dmbe2
           bsis~waers bsis~wrbtr
      FROM bsis APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_set
      FOR ALL ENTRIES IN tg_bsad
     WHERE bukrs  EQ tg_bsad-bukrs
       AND gjahr  EQ tg_bsad-gjahr
       AND belnr  EQ tg_bsad-belnr.

    SELECT bukrs belnr gjahr hkont dmbtr dmbe2
           bsas~waers bsas~wrbtr
      FROM bsas APPENDING CORRESPONDING FIELDS OF TABLE tg_bsas_set
      FOR ALL ENTRIES IN tg_bsad
     WHERE bukrs  EQ tg_bsad-bukrs
       AND gjahr  EQ tg_bsad-gjahr
       AND belnr  EQ tg_bsad-belnr.
    "Fim

  ENDIF.

  IF tg_bsid[] IS NOT INITIAL.

    SELECT *
     FROM vbap APPENDING CORRESPONDING FIELDS OF TABLE tg_vbap
     FOR ALL ENTRIES IN tg_bsid
    WHERE vbeln = tg_bsid-vbel2
      AND posnr = tg_bsid-posn2.

    SELECT a~obj_key a~id_tipo_invoice a~matnr b~bukrs b~belnr b~gjahr
      APPENDING CORRESPONDING FIELDS OF TABLE tg_0036
      FROM zfit0036 AS a INNER JOIN bkpf AS b ON a~obj_key = b~awkey
      FOR ALL ENTRIES IN tg_bsid
     WHERE b~bukrs = tg_bsid-bukrs
       AND b~belnr = tg_bsid-belnr
       AND b~gjahr = tg_bsid-gjahr.

    SELECT ryear rldnr docnr rbukrs bschl
           racct hsl ksl drcrk buzei usnam rmvct
      FROM faglflexa APPENDING CORRESPONDING FIELDS OF TABLE tg_fagl
      FOR ALL ENTRIES IN tg_bsid
    WHERE ryear   EQ tg_bsid-gjahr
      AND docnr   EQ tg_bsid-belnr
      AND rldnr   EQ '0L'
      AND rbukrs  EQ tg_bsid-bukrs
      AND buzei   EQ tg_bsid-buzei.

    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
           bsis~waers bsis~wrbtr
           APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_cbanco
      FROM bsis INNER JOIN skb1 ON bsis~bukrs = skb1~bukrs "#EC CI_DB_OPERATION_OK[2431747]
                               AND bsis~hkont = skb1~saknr
       FOR ALL ENTRIES IN tg_bsid
     WHERE bsis~bukrs EQ tg_bsid-bukrs
       AND bsis~gjahr EQ tg_bsid-gjahr
       AND bsis~belnr EQ tg_bsid-belnr
       AND skb1~fdlev IN it_fdlev.

  ENDIF.

  IF tg_bsis[] IS NOT INITIAL.

    SELECT a~obj_key a~id_tipo_invoice a~matnr b~bukrs b~belnr b~gjahr
      APPENDING CORRESPONDING FIELDS OF TABLE tg_0036
      FROM zfit0036 AS a INNER JOIN bkpf AS b ON a~obj_key = b~awkey
      FOR ALL ENTRIES IN tg_bsis
     WHERE b~bukrs = tg_bsis-bukrs
       AND b~belnr = tg_bsis-belnr
       AND b~gjahr = tg_bsis-gjahr.

    "Busca dados BKPF
*    SELECT BUKRS BELNR GJAHR STBLG AWKEY
*      FROM BKPF APPENDING TABLE TG_BKPF
*       FOR ALL ENTRIES IN TG_BSIS
*     WHERE BUKRS = TG_BSIS-BUKRS
*       AND BELNR = TG_BSIS-BELNR
*       AND GJAHR = TG_BSIS-GJAHR.

    SELECT ryear rldnr docnr rbukrs bschl
           racct hsl ksl drcrk buzei usnam rmvct
      FROM faglflexa APPENDING CORRESPONDING FIELDS OF TABLE tg_fagl
      FOR ALL ENTRIES IN tg_bsis
    WHERE ryear   EQ tg_bsis-gjahr
      AND docnr   EQ tg_bsis-belnr
      AND rldnr   EQ '0L'
      AND rbukrs  EQ tg_bsis-bukrs
      AND buzei   EQ tg_bsis-buzei.

    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~hkont bsis~dmbtr bsis~dmbe2
           bsis~waers bsis~wrbtr
           APPENDING CORRESPONDING FIELDS OF TABLE tg_bsis_cbanco
      FROM bsis INNER JOIN skb1 ON bsis~bukrs = skb1~bukrs "#EC CI_DB_OPERATION_OK[2431747]
                               AND bsis~hkont = skb1~saknr
       FOR ALL ENTRIES IN tg_bsis
     WHERE bsis~bukrs EQ tg_bsis-bukrs
       AND bsis~gjahr EQ tg_bsis-gjahr
       AND bsis~belnr EQ tg_bsis-belnr
       AND skb1~fdlev IN it_fdlev.

  ENDIF.

*-----------------------------------------------------------------*
*  Busca Item do documento de compras
*-----------------------------------------------------------------*
  IF tg_ekpo[] IS NOT INITIAL.
    SORT tg_ekpo BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM tg_ekpo COMPARING ebeln ebelp.

    LOOP AT tg_ekpo.
      IF ( tg_ekpo-netwr IS INITIAL ) AND ( tg_ekpo-brtwr IS NOT INITIAL ).
        tg_ekpo-netwr  = tg_ekpo-brtwr.
        MODIFY tg_ekpo.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF tg_0036[] IS NOT INITIAL.

    LOOP AT tg_0036.
      IF tg_0036-matnr IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = tg_0036-matnr
          IMPORTING
            output = tg_0036-matnr.

        MODIFY tg_0036.
      ENDIF.
    ENDLOOP.

    SELECT *
      FROM mara INTO CORRESPONDING FIELDS OF TABLE tg_mara
      FOR ALL ENTRIES IN tg_0036
     WHERE matnr = tg_0036-matnr.

    LOOP AT tg_0036.
      READ TABLE tg_mara WITH KEY matnr = tg_0036-matnr.
      IF sy-subrc = 0.
        tg_0036-matkl = tg_mara-matkl.
        MODIFY tg_0036.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Delete Contas que não estão no Set.
  LOOP AT tg_bsis_set.
    READ TABLE it_contas INTO wa_contas WITH KEY hkont = tg_bsis_set-hkont.
    IF sy-subrc NE 0.
      DELETE tg_bsis_set.
    ENDIF.
  ENDLOOP.

  LOOP AT tg_bsas_set.
    READ TABLE it_contas INTO wa_contas WITH KEY hkont = tg_bsas_set-hkont.
    IF sy-subrc NE 0.
      DELETE tg_bsas_set.
    ENDIF.
  ENDLOOP.

  "Sorts

  SORT tg_fagl BY ryear docnr rldnr rbukrs buzei.
  SORT tg_bsis_cbanco BY bukrs gjahr belnr.
  SORT tg_bsis_set BY bukrs gjahr belnr.
  SORT tg_bsas_set BY bukrs gjahr belnr.

ENDFORM.                    " SELECIONA_DADOS

FORM processa_dados .

  DATA: wl_bsak_bsik TYPE ty_bsak,
        wl_0078      LIKE tg_0078.

  DATA: vl_liq_cbanco TYPE c,
        vl_ret        TYPE c,
        vl_kursf      LIKE vg_tx_conv,
        vl_kurs2      LIKE vg_tx_conv.

  DATA: vl_venci_bsid TYPE bsid-zfbdt,
        vl_venci_bsik TYPE bsik-zfbdt.

  DATA: xconta_dif TYPE ekkn-sakto.

  vl_hora = sy-uzeit.
  vl_data = sy-datum.

*-----------------------------------------------------------------*
*  Processamento Partidas Fornecedores - Compensadas
*-----------------------------------------------------------------*
  LOOP AT tg_bsak.
    PERFORM processar_bsak.
  ENDLOOP.

*-----------------------------------------------------------------*
*  Processamento Partidas Fornecedores - Liquidadas (BSID)
*-----------------------------------------------------------------*
  LOOP AT tg_bsak_bsd.

    CLEAR: vl_liq_cbanco, tg_0080, tg_fagl, tg_bkpf, tg_skb1, tg_0078, tg_bkpf.

    PERFORM get_bkpf USING tg_bsak_bsd-bukrs tg_bsak_bsd-augbl tg_bsak_bsd-augdt(4)
                  CHANGING tg_bkpf_comp.
    CHECK ( sy-subrc EQ 0 ) AND ( tg_bkpf_comp-stblg IS INITIAL ).

    tg_bsak_bsd-augdt = tg_bkpf_comp-budat.

    PERFORM get_bkpf USING tg_bsak_bsd-bukrs tg_bsak_bsd-belnr tg_bsak_bsd-gjahr
                  CHANGING tg_bkpf.
    CHECK ( sy-subrc EQ 0 ) AND ( tg_bkpf-stblg IS INITIAL ). "Não lista documentos Estornados.

    PERFORM conv_taxa USING tg_bkpf_comp-waers
                            tg_bkpf_comp-kursf
                            tg_bkpf_comp-kurs2
                   CHANGING tg_bsak_bsd-dmbtr
                            tg_bsak_bsd-dmbe2
                            tg_bsak_bsd-wrbtr.

    IF tg_bsak_bsd-shkzg = 'H'.
      tg_bsak_bsd-dmbtr  = abs( tg_bsak_bsd-dmbtr ) * -1.
      tg_bsak_bsd-dmbe2  = abs( tg_bsak_bsd-dmbe2 ) * -1.
    ELSE.
      tg_bsak_bsd-dmbtr  = abs( tg_bsak_bsd-dmbtr ).
      tg_bsak_bsd-dmbe2  = abs( tg_bsak_bsd-dmbe2 ).
    ENDIF.

    PERFORM get_flexa USING tg_bsak_bsd-bukrs tg_bsak_bsd-belnr tg_bsak_bsd-gjahr tg_bsak_bsd-buzei
                   CHANGING tg_fagl.

    IF sy-subrc = 0.
      MOVE: tg_fagl-rmvct  TO tg_0080-rmvct.
    ENDIF.

    " Monta Estrutura de parâmetros para busca do código de Fluxo.---------------*
    CLEAR: wa_flx_par.
    MOVE: '2'                    TO wa_flx_par-orig,
          tg_bsak_bsd-hkont      TO wa_flx_par-hkont,
          tg_bsak_bsd-kidno      TO wa_flx_par-kidno,
          tg_bsak_bsd-zuonr      TO wa_flx_par-zuonr,
          tg_bsak_bsd-blart      TO wa_flx_par-blart,
          tg_bsak_bsd-xblnr      TO wa_flx_par-xblnr,
          tg_0080-rmvct          TO wa_flx_par-rmvct,
          tg_bsak_bsd-bschl      TO wa_flx_par-bschl,
          tg_bsak_bsd-lifnr      TO wa_flx_par-lifnr,
          tg_bsak_bsd-xref3      TO wa_flx_par-xref3,
          tg_bsak_bsd-bukrs      TO wa_flx_par-bukrs,
          tg_bsak_bsd-belnr      TO wa_flx_par-belnr,
          tg_bsak_bsd-gjahr      TO wa_flx_par-gjahr,
          tg_bsak_bsd-buzei      TO wa_flx_par-buzei.


    PERFORM atrib_cod_imposto USING tg_bkpf
                           CHANGING wa_flx_par-cod_imposto.

    PERFORM busca_cod_flx USING wa_flx_par
                       CHANGING tg_0080-cod_flx
                                wl_0078.

    IF wl_0078-shkzg IS NOT INITIAL.
      IF wl_0078-shkzg = 'S'.
        tg_bsak_bsd-dmbtr  = abs( tg_bsak_bsd-dmbtr ) * -1.
        tg_bsak_bsd-dmbe2  = abs( tg_bsak_bsd-dmbe2 ) * -1.
      ELSEIF wl_0078-shkzg = 'H'.
        tg_bsak_bsd-dmbtr  = abs( tg_bsak_bsd-dmbtr ).
        tg_bsak_bsd-dmbe2  = abs( tg_bsak_bsd-dmbe2 ).
      ENDIF.
    ENDIF.

    CLEAR: wl_cls_ped.
    MOVE-CORRESPONDING tg_bsak_bsd TO wl_cls_ped.

    PERFORM get_contas_ped USING wl_cls_ped tg_bkpf.

    PERFORM append_bsk USING tg_bsak_bsd tg_bkpf tg_0080.

  ENDLOOP.

*-----------------------------------------------------------------*
*  Processamento Partidas Fornecedores - Em aberto
*-----------------------------------------------------------------*

  LOOP AT it_bsik INTO wa_bsik.

    CLEAR: vl_liq_cbanco,tg_0080, tg_skb1, tg_0078,vl_venci_bsik, tg_fagl, tg_bkpf.

    PERFORM get_bkpf USING wa_bsik-bukrs wa_bsik-belnr wa_bsik-gjahr
                  CHANGING tg_bkpf.
    CHECK ( sy-subrc EQ 0 ) AND ( tg_bkpf-stblg IS INITIAL ). "Não lista documentos Estornados.

    IF wa_bsik-shkzg = 'H'.
      wa_bsik-dmbtr  = abs( wa_bsik-dmbtr ) * -1.
      wa_bsik-dmbe2  = abs( wa_bsik-dmbe2 ) * -1.
    ELSE.
      wa_bsik-dmbtr  = abs( wa_bsik-dmbtr ).
      wa_bsik-dmbe2  = abs( wa_bsik-dmbe2 ).
    ENDIF.

    vl_venci_bsik = wa_bsik-zfbdt + wa_bsik-zbd1t.

    "---------------------------------------------------*
    "Verificar se partida foi liquidada contra banco
    "---------------------------------------------------*
    PERFORM get_bsis_cbanco USING wa_bsik-bukrs wa_bsik-belnr wa_bsik-gjahr
                         CHANGING vl_kursf
                                  vl_kurs2.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    PERFORM get_flexa USING wa_bsik-bukrs wa_bsik-belnr wa_bsik-gjahr wa_bsik-buzei
                   CHANGING tg_fagl.

    IF sy-subrc = 0.
      MOVE: tg_fagl-rmvct  TO tg_0080-rmvct.
    ENDIF.

    " Monta Estrutura de parâmetros para busca do código de Fluxo.---------------*
    CLEAR: wa_flx_par.
    MOVE: '3'                TO wa_flx_par-orig,
          wa_bsik-hkont      TO wa_flx_par-hkont,
          wa_bsik-kidno      TO wa_flx_par-kidno,
          wa_bsik-zuonr      TO wa_flx_par-zuonr,
          wa_bsik-blart      TO wa_flx_par-blart,
          wa_bsik-xblnr      TO wa_flx_par-xblnr,
          tg_0080-rmvct      TO wa_flx_par-rmvct,
          wa_bsik-bschl      TO wa_flx_par-bschl,
          wa_bsik-lifnr      TO wa_flx_par-lifnr,
          wa_bsik-xref3      TO wa_flx_par-xref3,
          wa_bsik-bukrs      TO wa_flx_par-bukrs,
          wa_bsik-belnr      TO wa_flx_par-belnr,
          wa_bsik-gjahr      TO wa_flx_par-gjahr,
          wa_bsik-buzei      TO wa_flx_par-buzei.

    PERFORM atrib_cod_imposto USING tg_bkpf
                           CHANGING wa_flx_par-cod_imposto.

    PERFORM busca_cod_flx USING wa_flx_par
                       CHANGING tg_0080-cod_flx
                                wl_0078.

    IF wl_0078-shkzg IS NOT INITIAL.
      IF wl_0078-shkzg = 'S'.
        wa_bsik-dmbtr  = abs( wa_bsik-dmbtr ) * -1.
        wa_bsik-dmbe2  = abs( wa_bsik-dmbe2 ) * -1.
      ELSEIF wl_0078-shkzg = 'H'.
        wa_bsik-dmbtr  = abs( wa_bsik-dmbtr ).
        wa_bsik-dmbe2  = abs( wa_bsik-dmbe2 ).
      ENDIF.
    ENDIF.

    CLEAR: wl_cls_ped, wl_bsak_bsik.
    MOVE-CORRESPONDING wa_bsik TO wl_cls_ped.
    PERFORM get_contas_ped USING wl_cls_ped tg_bkpf.

    MOVE-CORRESPONDING wa_bsik TO wl_bsak_bsik.
    wl_bsak_bsik-venci = wa_bsik-zfbdt + wa_bsik-zbd1t.
    wl_bsak_bsik-augbl = wa_bsik-belnr.
    wl_bsak_bsik-augdt = wa_bsik-budat.

    PERFORM append_bsk USING wl_bsak_bsik tg_bkpf tg_0080.

  ENDLOOP.

*-----------------------------------------------------------------*
*  Processamento Partidas Clientes Compensadas
*-----------------------------------------------------------------*

  LOOP AT tg_bsad.
    PERFORM processar_bsad.
  ENDLOOP.

*-----------------------------------------------------------------*
*  Processamento Partidas Razão - Em aberto
*-----------------------------------------------------------------*
  LOOP AT tg_bsis.

    CLEAR: vl_liq_cbanco,tg_0080, tg_skb1, tg_0078, tg_fagl, tg_bkpf.

    PERFORM get_bkpf USING tg_bsis-bukrs tg_bsis-belnr tg_bsis-gjahr
                  CHANGING tg_bkpf.

    CHECK ( sy-subrc EQ 0 ) AND ( tg_bkpf-stblg IS INITIAL ). "Não lista documentos Estornados.

    IF tg_bsis-shkzg = 'H'.
      tg_bsis-dmbtr  = abs( tg_bsis-dmbtr ) * -1.
      tg_bsis-dmbe2  = abs( tg_bsis-dmbe2 ) * -1.
    ELSE.
      tg_bsis-dmbtr  = abs( tg_bsis-dmbtr ).
      tg_bsis-dmbe2  = abs( tg_bsis-dmbe2 ).
    ENDIF.

    "---------------------------------------------------*
    "Verificar se partida foi liquidada contra banco
    "---------------------------------------------------*
    PERFORM get_bsis_cbanco USING tg_bsis-bukrs tg_bsis-belnr tg_bsis-gjahr
                         CHANGING vl_kursf
                                  vl_kurs2.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    PERFORM get_flexa USING tg_bsis-bukrs tg_bsis-belnr tg_bsis-gjahr tg_bsis-buzei
                   CHANGING tg_fagl.

    IF sy-subrc = 0.
      MOVE: tg_fagl-rmvct  TO tg_0080-rmvct.
    ENDIF.

    " Monta Estrutura de parâmetros para busca do código de Fluxo.---------------*
    CLEAR: wa_flx_par.
    MOVE: '3'            TO wa_flx_par-orig,
          tg_bsis-hkont  TO wa_flx_par-hkont,
          tg_bsis-kidno  TO wa_flx_par-kidno,
          tg_bsis-zuonr  TO wa_flx_par-zuonr,
          tg_bsis-blart  TO wa_flx_par-blart,
          tg_bsis-xblnr  TO wa_flx_par-xblnr,
          tg_0080-rmvct  TO wa_flx_par-rmvct,
          tg_bsis-bschl  TO wa_flx_par-bschl,
          tg_bsis-bukrs  TO wa_flx_par-bukrs,
          tg_bsis-belnr  TO wa_flx_par-belnr,
          tg_bsis-gjahr  TO wa_flx_par-gjahr,
          tg_bsis-buzei  TO wa_flx_par-buzei.

    PERFORM atrib_cod_imposto USING tg_bkpf
                           CHANGING wa_flx_par-cod_imposto.

    PERFORM busca_cod_flx USING wa_flx_par
                       CHANGING tg_0080-cod_flx
                                wl_0078.

    IF wl_0078-shkzg IS NOT INITIAL.
      IF wl_0078-shkzg = 'S'.
        tg_bsis-dmbtr  = abs( tg_bsis-dmbtr ) * -1.
        tg_bsis-dmbe2  = abs( tg_bsis-dmbe2 ) * -1.
      ELSEIF wl_0078-shkzg = 'H'.
        tg_bsis-dmbtr  = abs( tg_bsis-dmbtr ).
        tg_bsis-dmbe2  = abs( tg_bsis-dmbe2 ).
      ENDIF.
    ENDIF.

    MOVE: tg_bsis-bukrs   TO tg_0080-bukrs,
          tg_bsis-hkont   TO tg_0080-hkont,
          tg_bsis-belnr   TO tg_0080-belnr,
          tg_bsis-gjahr   TO tg_0080-gjahr,
          tg_bsis-buzei   TO tg_0080-buzei,
          tg_bsis-budat   TO tg_0080-budat,
          tg_bsis-zfbdt   TO tg_0080-zfbdt,
          tg_bsis-bldat   TO tg_0080-bldat,
          tg_bsis-waers   TO tg_0080-waers,
          tg_bsis-xblnr   TO tg_0080-xblnr,
          tg_bsis-blart   TO tg_0080-blart,
          tg_bsis-gsber   TO tg_0080-gsber,
          tg_bsis-bschl   TO tg_0080-bschl,
          tg_bsis-shkzg   TO tg_0080-shkzg,
          tg_bsis-dmbtr   TO tg_0080-dmbtr,
          tg_bsis-dmbe2   TO tg_0080-dmbe2,
          vl_data         TO tg_0080-dt_atual,
          vl_hora         TO tg_0080-hr_atual,
          tg_bsis-belnr   TO tg_0080-augbl,
          tg_bsis-budat   TO tg_0080-augdt.
    "WA_SKB1-SAKNR   TO TG_0080-SAKNR.

    APPEND tg_0080.

  ENDLOOP.


*-----------------------------------------------------------------*
*  Processamento Partidas Cliente - Em aberto
*-----------------------------------------------------------------*
  LOOP AT tg_bsid.

    CLEAR: vl_liq_cbanco,tg_0080, tg_skb1, tg_0078, tg_fagl, tg_bkpf.

    PERFORM get_bkpf USING tg_bsid-bukrs tg_bsid-belnr tg_bsid-gjahr
                  CHANGING tg_bkpf.

    CHECK ( sy-subrc EQ 0 ) AND ( tg_bkpf-stblg IS INITIAL ). "Não lista documentos Estornados.

    IF tg_bsid-shkzg = 'S'.
      tg_bsid-dmbtr  = abs( tg_bsid-dmbtr ) * -1.
      tg_bsid-dmbe2  = abs( tg_bsid-dmbe2 ) * -1.
    ELSE.
      tg_bsid-dmbtr  = abs( tg_bsid-dmbtr ).
      tg_bsid-dmbe2  = abs( tg_bsid-dmbe2 ).
    ENDIF.

    tg_bsid-venci = tg_bsid-zfbdt + tg_bsid-zbd1t.

    "---------------------------------------------------*
    "Verificar se partida foi liquidada contra banco
    "---------------------------------------------------*
    PERFORM get_bsis_cbanco USING tg_bsid-bukrs tg_bsid-belnr tg_bsid-gjahr
                         CHANGING vl_kursf
                                  vl_kurs2.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    PERFORM get_flexa USING tg_bsid-bukrs tg_bsid-belnr tg_bsid-gjahr tg_bsid-buzei
                   CHANGING tg_fagl.

    IF sy-subrc = 0.
      MOVE: tg_fagl-rmvct  TO tg_0080-rmvct.
    ENDIF.

    CLEAR: tg_vbap.
    READ TABLE tg_vbap WITH KEY vbeln = tg_bsid-vbel2 posnr = tg_bsid-posn2.

    " Monta Estrutura de parâmetros para busca do código de Fluxo.---------------*
    CLEAR: wa_flx_par.
    MOVE: '3'            TO wa_flx_par-orig,
          tg_bsid-hkont  TO wa_flx_par-hkont,
          tg_bsid-kidno  TO wa_flx_par-kidno,
          tg_bsid-zuonr  TO wa_flx_par-zuonr,
          tg_bsid-blart  TO wa_flx_par-blart,
          tg_bsid-xblnr  TO wa_flx_par-xblnr,
          tg_vbap-matkl  TO wa_flx_par-matkl,
          tg_0080-rmvct  TO wa_flx_par-rmvct,
          tg_bsid-bschl  TO wa_flx_par-bschl,
          tg_bsid-bukrs  TO wa_flx_par-bukrs,
          tg_bsid-belnr  TO wa_flx_par-belnr,
          tg_bsid-gjahr  TO wa_flx_par-gjahr,
          tg_bsid-buzei  TO wa_flx_par-buzei.

    PERFORM atrib_cod_imposto USING tg_bkpf
                           CHANGING wa_flx_par-cod_imposto.

    PERFORM busca_cod_flx USING wa_flx_par
                       CHANGING tg_0080-cod_flx
                                wl_0078.

    IF wl_0078-shkzg IS NOT INITIAL.
      IF wl_0078-shkzg = 'S'.
        tg_bsid-dmbtr  = abs( tg_bsid-dmbtr ) * -1.
        tg_bsid-dmbe2  = abs( tg_bsid-dmbe2 ) * -1.
      ELSEIF wl_0078-shkzg = 'H'.
        tg_bsid-dmbtr  = abs( tg_bsid-dmbtr ).
        tg_bsid-dmbe2  = abs( tg_bsid-dmbe2 ).
      ENDIF.
    ENDIF.

    MOVE: tg_bsid-bukrs   TO tg_0080-bukrs,
          tg_bsid-kunnr   TO tg_0080-kunnr,
          tg_bsid-belnr   TO tg_0080-belnr,
          tg_bsid-gjahr   TO tg_0080-gjahr,
          tg_bsid-buzei   TO tg_0080-buzei,
          tg_bsid-budat   TO tg_0080-budat,
          tg_bsid-venci   TO tg_0080-zfbdt,
          tg_bsid-hkont   TO tg_0080-hkont,
          tg_bsid-bldat   TO tg_0080-bldat,
          tg_bsid-waers   TO tg_0080-waers,
          tg_bsid-xblnr   TO tg_0080-xblnr,
          tg_bsid-blart   TO tg_0080-blart,
          tg_bsid-gsber   TO tg_0080-gsber,
          tg_bsid-vbeln   TO tg_0080-vbeln,
          tg_bsid-bschl   TO tg_0080-bschl,
          tg_bsid-shkzg   TO tg_0080-shkzg,
          tg_bsid-dmbtr   TO tg_0080-dmbtr,
          tg_bsid-dmbe2   TO tg_0080-dmbe2,
          vl_data         TO tg_0080-dt_atual,
          vl_hora         TO tg_0080-hr_atual,
          tg_bsid-belnr   TO tg_0080-augbl,
          tg_bsid-budat   TO tg_0080-augdt.
    "WA_SKB1-SAKNR   TO TG_0080-SAKNR.


    APPEND tg_0080.
*    CLEAR: tg_vbap, tg_bsid,  tg_0080, wa_flx_par.

  ENDLOOP.

ENDFORM.                    " PROCESSA_DADOS

FORM grava_dados .
  DATA: tg_0121 TYPE TABLE OF zfit0121 WITH HEADER LINE.

  DATA: wl_lines TYPE i.

**<<<------"187070 - NMS - INI------>>>
  IF rb_geral IS NOT INITIAL.
**<<<------"187070 - NMS - FIM------>>>
    DELETE FROM zfit0080 WHERE bukrs IN s_bukrs
                           AND augdt EQ '00000000'.

    DELETE FROM zfit0080 WHERE bukrs IN s_bukrs
                           AND augdt IN s_budat.
    COMMIT WORK.
**<<<------"187070 - NMS - INI------>>>
  ENDIF.
**<<<------"187070 - NMS - FIM------>>>
  IF tg_0080[] IS NOT INITIAL.

    SORT tg_0080 BY bukrs lifnr belnr gjahr buzei budat hkont cod_flx zfbdt DESCENDING.
    DELETE ADJACENT DUPLICATES FROM tg_0080 COMPARING bukrs lifnr belnr gjahr buzei budat hkont cod_flx.

    SORT tg_0080 BY bukrs belnr gjahr buzei budat hkont cod_flx zfbdt lifnr DESCENDING.
    DELETE ADJACENT DUPLICATES FROM tg_0080 COMPARING bukrs belnr gjahr buzei budat hkont cod_flx zfbdt.

    MODIFY zfit0080 FROM TABLE tg_0080.

    DESCRIBE TABLE tg_0080 LINES wl_lines.
    MESSAGE s000(z01) DISPLAY LIKE 'S' WITH 'Foram gravados' wl_lines 'registros na tabela ZFIT0080.'.

    COMMIT WORK.

    "Gravar Lançamentos Manuais
    CLEAR: tg_0121[], tg_0080[].
    SELECT *
      FROM zfit0121 AS a INTO TABLE tg_0121
     WHERE a~bukrs    IN s_bukrs
       AND a~augdt    IN s_budat
       AND a~manual   EQ 'X'
       AND NOT EXISTS ( SELECT *
                          FROM zfit0080 AS b
                         WHERE b~bukrs = a~bukrs
                           AND b~belnr = a~belnr
                           AND b~gjahr = a~gjahr
                           AND b~monat = a~monat
                           AND b~buzei = a~buzei ).
    IF tg_0121[] IS NOT INITIAL.
      LOOP AT tg_0121.
        CLEAR: tg_0080.
        MOVE-CORRESPONDING tg_0121 TO tg_0080.
        APPEND tg_0080.
      ENDLOOP.

      MODIFY zfit0080 FROM TABLE tg_0080.
    ENDIF.

  ELSE.
    MESSAGE s000(z01) DISPLAY LIKE 'E' WITH 'Não foi possível gravar registros na tabela ZFIT0080.'.
  ENDIF.

  CALL FUNCTION 'ZFI_PROC_FLX_REAL'
    EXPORTING
      i_data_ini       = s_budat-low
      i_bukrs          = s_bukrs-low
      i_ref_saldo      = 'X'
      i_calc_sem_class = p_clc_ds.

ENDFORM.                    " GRAVA_DADOS

FORM busca_cod_flx USING p_flx_par TYPE ty_flx_par
                CHANGING p_cod_flx TYPE zfit0080-cod_flx
                         p_0078    LIKE tg_0078.

  DATA: wa_0121 TYPE zfit0121.

  DATA: vl_found_par TYPE c.

  CLEAR: tg_0078, p_cod_flx, wa_0121, p_0078.

  CHECK ( p_flx_par-hkont IS NOT INITIAL ) OR
        ( p_flx_par-kidno IS NOT INITIAL ) OR
        ( p_flx_par-zuonr IS NOT INITIAL ) OR
        ( p_flx_par-blart IS NOT INITIAL ) OR
        ( p_flx_par-xblnr IS NOT INITIAL ) OR
        ( p_flx_par-rmvct IS NOT INITIAL ).

  IF ( p_flx_par-kidno IS INITIAL ) AND
     ( p_flx_par-xblnr IS NOT INITIAL ).
    p_flx_par-kidno = p_flx_par-xblnr.
  ENDIF.

  "Verificar se Doc. Possui Tp. Invoice.
  READ TABLE tg_0036 WITH KEY bukrs = p_flx_par-bukrs
                              belnr = p_flx_par-belnr
                              gjahr = p_flx_par-gjahr.
  IF sy-subrc = 0.
    p_flx_par-id_tipo_invoice = tg_0036-id_tipo_invoice.
    p_flx_par-matkl           = tg_0036-matkl.
  ENDIF.

*----------------------------------------------------------------------*
* Verifica se possui classificação manual.
*----------------------------------------------------------------------*
  CLEAR: wa_0121.
  SELECT SINGLE *
    FROM zfit0121 INTO wa_0121
   WHERE bukrs  = p_flx_par-bukrs
     AND belnr  = p_flx_par-belnr
     AND gjahr  = p_flx_par-gjahr
     AND buzei  = p_flx_par-buzei
     AND manual = ''.

  IF ( sy-subrc = 0 ) AND ( wa_0121-cod_flx IS NOT INITIAL ).
    READ TABLE tg_0078 WITH KEY cod_flx = wa_0121-cod_flx.
    IF sy-subrc = 0.
      MOVE wa_0121-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Prioridades por Operação
*----------------------------------------------------------------------*

  "----------------------------------*
  "   Impostos
  "----------------------------------*
  IF ( p_flx_par-hkont       IS NOT INITIAL ) AND
     ( p_flx_par-cod_imposto IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               cod_imposto     = p_flx_par-cod_imposto
               kidno           = ''
               zuonr           = ''
               blart           = ''
               rmvct           = ''
               bschl           = ''
               matkl           = ''
               prctr           = ''
               lifnr           = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  "----------------------------------*
  "   Fretes
  "----------------------------------*
  IF ( p_flx_par-hkont  IS NOT INITIAL ) AND
     ( p_flx_par-lifnr  IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               lifnr           = p_flx_par-lifnr
               cod_imposto     = ''
               kidno           = ''
               zuonr           = ''
               blart           = ''
               rmvct           = ''
               bschl           = ''
               matkl           = ''
               prctr           = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.


*----------------------------------------------------------------------*
* 6º Nível
*----------------------------------------------------------------------*

  READ TABLE tg_0078
    WITH KEY saknr           = p_flx_par-hkont
             kidno           = p_flx_par-kidno
             zuonr           = p_flx_par-zuonr
             blart           = p_flx_par-blart
             rmvct           = p_flx_par-rmvct
             bschl           = p_flx_par-bschl
             matkl           = ''
             prctr           = ''
             lifnr           = ''
             cod_imposto     = ''
             bukrs           = ''
             id_tipo_invoice = ''.

  IF sy-subrc EQ 0.
    MOVE tg_0078-cod_flx TO p_cod_flx.
    MOVE-CORRESPONDING tg_0078 TO p_0078.
    EXIT.
  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-kidno IS NOT INITIAL ) AND
     ( p_flx_par-blart IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ) AND
     ( p_flx_par-rmvct IS NOT INITIAL ) AND
     ( p_flx_par-lifnr IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               kidno           = p_flx_par-kidno
               blart           = p_flx_par-blart
               bschl           = p_flx_par-bschl
               rmvct           = p_flx_par-rmvct
               lifnr           = p_flx_par-lifnr
               zuonr           = ''
               matkl           = ''
               prctr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

*----------------------------------------------------------------------*
* 5º Nível
*----------------------------------------------------------------------*

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-kidno IS NOT INITIAL ) AND
     ( p_flx_par-zuonr IS NOT INITIAL ) AND
     ( p_flx_par-blart IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               kidno           = p_flx_par-kidno
               zuonr           = p_flx_par-zuonr
               blart           = p_flx_par-blart
               bschl           = p_flx_par-bschl
               matkl           = ''
               prctr           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-kidno IS NOT INITIAL ) AND
     ( p_flx_par-blart IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ) AND
     ( p_flx_par-rmvct IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               kidno           = p_flx_par-kidno
               blart           = p_flx_par-blart
               bschl           = p_flx_par-bschl
               rmvct           = p_flx_par-rmvct
               zuonr           = ''
               matkl           = ''
               prctr           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

*----------------------------------------------------------------------*
* 4º Nível
*----------------------------------------------------------------------*

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-kidno IS NOT INITIAL ) AND
     ( p_flx_par-zuonr IS NOT INITIAL ) AND
     ( p_flx_par-blart IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               kidno           = p_flx_par-kidno
               zuonr           = p_flx_par-zuonr
               blart           = p_flx_par-blart
               bschl           = ''
               matkl           = ''
               prctr           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-kidno IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ) AND
     ( p_flx_par-rmvct IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               kidno           = p_flx_par-kidno
               bschl           = p_flx_par-bschl
               rmvct           = p_flx_par-rmvct
               matkl           = ''
               prctr           = ''
               zuonr           = ''
               blart           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-kidno IS NOT INITIAL ) AND
     ( p_flx_par-zuonr IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               kidno           = p_flx_par-kidno
               zuonr           = p_flx_par-zuonr
               bschl           = p_flx_par-bschl
               rmvct           = ''
               matkl           = ''
               prctr           = ''
               blart           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.


  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-zuonr IS NOT INITIAL ) AND
     ( p_flx_par-blart IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               zuonr           = p_flx_par-zuonr
               blart           = p_flx_par-blart
               bschl           = p_flx_par-bschl
               matkl           = ''
               prctr           = ''
               kidno           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-kidno IS NOT INITIAL ) AND
     ( p_flx_par-blart IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               kidno           = p_flx_par-kidno
               blart           = p_flx_par-blart
               bschl           = p_flx_par-bschl
               matkl           = ''
               prctr           = ''
               zuonr           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-zuonr IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ) AND
     ( p_flx_par-bukrs IS NOT INITIAL ) .

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               zuonr           = p_flx_par-zuonr
               bschl           = p_flx_par-bschl
               bukrs           = p_flx_par-bukrs
               matkl           = ''
               prctr           = ''
               kidno           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               blart           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

*----------------------------------------------------------------------*
* 3º Nível
*----------------------------------------------------------------------*

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-id_tipo_invoice IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               id_tipo_invoice = p_flx_par-id_tipo_invoice
               bschl           = p_flx_par-bschl
               rmvct           = ''
               matkl           = ''
               prctr           = ''
               zuonr           = ''
               blart           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               kidno           = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.


  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-kidno IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               kidno           = p_flx_par-kidno
               bschl           = p_flx_par-bschl
               rmvct           = ''
               matkl           = ''
               prctr           = ''
               zuonr           = ''
               blart           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-xref3 IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               kidno           = p_flx_par-xref3
               bschl           = p_flx_par-bschl
               rmvct           = ''
               matkl           = ''
               prctr           = ''
               zuonr           = ''
               blart           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-zuonr IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               zuonr           = p_flx_par-zuonr
               bschl           = p_flx_par-bschl
               rmvct           = ''
               matkl           = ''
               prctr           = ''
               kidno           = ''
               blart           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.


  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-matkl IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               matkl           = p_flx_par-matkl
               bschl           = p_flx_par-bschl
               rmvct           = ''
               kidno           = ''
               prctr           = ''
               zuonr           = ''
               blart           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-blart IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               blart           = p_flx_par-blart
               bschl           = p_flx_par-bschl
               rmvct           = ''
               kidno           = ''
               prctr           = ''
               zuonr           = ''
               matkl           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-bukrs IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               bukrs           = p_flx_par-bukrs
               bschl           = p_flx_par-bschl
               rmvct           = ''
               matkl           = ''
               prctr           = ''
               zuonr           = ''
               blart           = ''
               lifnr           = ''
               cod_imposto     = ''
               id_tipo_invoice = ''
               kidno           = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-blart IS NOT INITIAL ) AND
     ( p_flx_par-bukrs IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               blart           = p_flx_par-blart
               bukrs           = p_flx_par-bukrs
               rmvct           = ''
               kidno           = ''
               prctr           = ''
               zuonr           = ''
               matkl           = ''
               lifnr           = ''
               cod_imposto     = ''
               bschl           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.


*----------------------------------------------------------------------*
* 2º Nível
*----------------------------------------------------------------------*

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-id_tipo_invoice IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               id_tipo_invoice = p_flx_par-id_tipo_invoice
               bschl           = ''
               matkl           = ''
               prctr           = ''
               kidno           = ''
               blart           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               zuonr           = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.


  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-zuonr IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               zuonr           = p_flx_par-zuonr
               bschl           = ''
               matkl           = ''
               prctr           = ''
               kidno           = ''
               blart           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.


  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-blart IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               blart           = p_flx_par-blart
               bschl           = ''
               matkl           = ''
               prctr           = ''
               kidno           = ''
               zuonr           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-kidno IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               kidno           = p_flx_par-kidno
               bschl           = ''
               matkl           = ''
               prctr           = ''
               zuonr           = ''
               blart           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc EQ 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-xblnr IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               kidno           = p_flx_par-xblnr
               bschl           = ''
               matkl           = ''
               prctr           = ''
               zuonr           = ''
               blart           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF ( sy-subrc = 0 ).
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-rmvct IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               rmvct           = p_flx_par-rmvct
               bschl           = ''
               matkl           = ''
               prctr           = ''
               kidno           = ''
               zuonr           = ''
               blart           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc = 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-matkl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               matkl           = p_flx_par-matkl
               bschl           = ''
               prctr           = ''
               kidno           = ''
               zuonr           = ''
               blart           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc = 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

  IF ( p_flx_par-hkont IS NOT INITIAL ) AND
     ( p_flx_par-bschl IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               bschl           = p_flx_par-bschl
               matkl           = ''
               prctr           = ''
               kidno           = ''
               zuonr           = ''
               blart           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc = 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

*----------------------------------------------------------------------*
* 1º Nível
*----------------------------------------------------------------------*

  IF ( p_flx_par-hkont IS NOT INITIAL ).

    READ TABLE tg_0078
      WITH KEY saknr           = p_flx_par-hkont
               bschl           = ''
               matkl           = ''
               prctr           = ''
               kidno           = ''
               zuonr           = ''
               blart           = ''
               rmvct           = ''
               lifnr           = ''
               cod_imposto     = ''
               bukrs           = ''
               id_tipo_invoice = ''.

    IF sy-subrc = 0.
      MOVE tg_0078-cod_flx TO p_cod_flx.
      MOVE-CORRESPONDING tg_0078 TO p_0078.
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.

FORM atrib_doc_estorno CHANGING p_0080 TYPE zfit0080.


  PERFORM get_bkpf USING p_0080-bukrs
                         p_0080-belnr
                         p_0080-gjahr
                CHANGING tg_bkpf.

  IF ( sy-subrc = 0 ) AND ( tg_bkpf-stblg IS NOT INITIAL ).
    p_0080-stblg =  tg_bkpf-stblg.
  ENDIF.

ENDFORM.

FORM atrib_bsak USING p_bsak TYPE ty_bsak
                      p_0080 TYPE zfit0080.

  IF p_bsak-xvlr_r NE 0 AND p_bsak-xvlr_u NE 0.

    MOVE: p_bsak-xvlr_r  TO p_0080-dmbtr,
          p_bsak-xvlr_u  TO p_0080-dmbe2.
  ELSE.
    MOVE: p_bsak-dmbtr   TO p_0080-dmbtr,
          p_bsak-dmbe2   TO p_0080-dmbe2.
  ENDIF.


  MOVE: p_bsak-bukrs    TO p_0080-bukrs,
        p_bsak-lifnr    TO p_0080-lifnr,
        p_bsak-belnr    TO p_0080-belnr,
        p_bsak-gjahr    TO p_0080-gjahr,
        p_bsak-buzei    TO p_0080-buzei,
        p_bsak-budat    TO p_0080-budat,
        p_bsak-venci    TO p_0080-zfbdt,
        p_bsak-bldat    TO p_0080-bldat,
        p_bsak-waers    TO p_0080-waers,
        p_bsak-xblnr    TO p_0080-xblnr,
        p_bsak-blart    TO p_0080-blart,
        p_bsak-gsber    TO p_0080-gsber,
        p_bsak-ebeln    TO p_0080-ebeln,
        p_bsak-ebelp    TO p_0080-ebelp,
        p_bsak-bschl    TO p_0080-bschl,
        p_bsak-shkzg    TO p_0080-shkzg,
        vl_data         TO p_0080-dt_atual,
        vl_hora         TO p_0080-hr_atual,
        p_bsak-augbl    TO p_0080-augbl,
        p_bsak-augdt    TO p_0080-augdt.
  "WA_SKB1-SAKNR   TO P_0080-SAKNR.

ENDFORM.

FORM get_contas_ped USING p_cls_ped TYPE ty_cls_ped
                          p_bkpf    TYPE ty_bkpf.

  DATA: vl_error_conta TYPE c.

  REFRESH: tg_rseg, tg_sakto.

  "Verifica se conta está parametriza como fornecedor.
  CLEAR: tg_0078.
  READ TABLE tg_0078 WITH KEY saknr    = p_cls_ped-saknr
                              cta_part = 'F'.
  CHECK sy-subrc EQ 0.

  IF ( p_cls_ped-ebeln IS NOT INITIAL ) AND
     ( p_cls_ped-ebelp IS INITIAL ) AND
     ( strlen( p_bkpf-awkey ) = 14 ).

    SELECT ebeln ebelp
      FROM rseg INTO TABLE tg_rseg
     WHERE belnr = p_bkpf-awkey(10)
       AND gjahr = p_bkpf-awkey+10(4).

    LOOP AT tg_rseg.
      READ TABLE tg_ekpo WITH KEY ebeln = tg_rseg-ebeln
                                  ebelp = tg_rseg-ebelp.

      IF sy-subrc NE 0.
        vl_error_conta = 'X'.
        EXIT.
      ENDIF.

      IF ( tg_ekpo-knttp IS INITIAL ).
        vl_error_conta = 'X'.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDIF.

  CHECK vl_error_conta IS INITIAL.

  "Busca Contas e Valores para Rateio
  LOOP AT tg_ekkn WHERE ebeln = p_cls_ped-ebeln.

    IF p_cls_ped-ebelp IS NOT INITIAL.
      IF p_cls_ped-ebelp NE tg_ekkn-ebelp.
        CONTINUE.
      ENDIF.
    ELSE.
      IF strlen( p_bkpf-awkey ) = 14.

        READ TABLE tg_rseg WITH KEY ebelp = tg_ekkn-ebelp.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        READ TABLE tg_ekpo WITH KEY ebeln = tg_ekkn-ebeln
                                    ebelp = tg_ekkn-ebelp.

        IF ( sy-subrc NE 0 ).
          REFRESH: tg_sakto.
          vl_error_conta = 'X'.
          EXIT.
        ENDIF.

        IF ( tg_ekpo-knttp IS INITIAL ) .
          REFRESH: tg_sakto.
          vl_error_conta = 'X'.
          EXIT.
        ENDIF.

      ENDIF.

    ENDIF.

    ADD tg_ekkn-menge TO p_cls_ped-xtot.

    READ TABLE tg_sakto WITH KEY sakto = tg_ekkn-sakto.
    IF sy-subrc = 0.
      LOOP AT tg_sakto WHERE sakto = tg_ekkn-sakto.
        ADD tg_ekkn-menge TO tg_sakto-menge.
        MODIFY tg_sakto.
      ENDLOOP.
    ELSE.
      CLEAR: tg_sakto.
      tg_sakto-sakto = tg_ekkn-sakto.
      ADD tg_ekkn-menge TO tg_sakto-menge.
      APPEND tg_sakto.
    ENDIF.
  ENDLOOP.

  LOOP AT tg_sakto.
    IF ( tg_sakto-menge = 0 ) OR ( p_cls_ped-xtot = 0 ).
      DELETE tg_sakto.
    ELSE.
      tg_sakto-perc = tg_sakto-menge / p_cls_ped-xtot.
      MODIFY tg_sakto.
    ENDIF.
  ENDLOOP.

  SORT tg_sakto BY sakto.
  DELETE ADJACENT DUPLICATES FROM tg_sakto COMPARING sakto.

  IF vl_error_conta IS NOT INITIAL.
    REFRESH: tg_sakto.
  ENDIF.

ENDFORM.

FORM conv_taxa  USING  p_waers TYPE bkpf-waers
                       p_kursf TYPE bkpf-kursf
                       p_kurs2 TYPE bkpf-kurs2
              CHANGING p_dmbtr TYPE bsak-dmbtr
                       p_dmbe2 TYPE bsak-dmbe2
                       p_wrbtr TYPE bsak-wrbtr.


  p_kursf = abs( p_kursf ).
  p_kurs2 = abs( p_kurs2 ).

  IF ( p_waers = 'BRL' ).

    CHECK ( p_kurs2 NE 0 ).

    IF ( p_dmbtr NE 0 ).
      p_dmbe2 = p_dmbtr / p_kurs2.
    ENDIF.

  ELSEIF ( p_waers NE 'USD' ).

    IF ( p_kursf NE 0 ).
      p_dmbtr = p_wrbtr * p_kursf.
    ENDIF.

    IF ( p_kurs2 NE 0 ).
      p_dmbe2 = p_wrbtr * p_kurs2.
    ENDIF.

  ELSEIF ( p_dmbe2 NE 0 ).

    CHECK ( p_kursf NE 0 ).

    p_dmbtr = p_dmbe2 * p_kursf.
  ENDIF.

ENDFORM.

FORM conv_taxa_inf USING p_waers_doc TYPE bkpf-waers
                         p_kursf     LIKE vg_tx_conv
                         p_kurs2     LIKE vg_tx_conv
                CHANGING p_dmbtr     TYPE bsak-dmbtr
                         p_dmbe2     TYPE bsak-dmbe2
                         p_wrbtr     TYPE bsak-wrbtr.

  p_kursf = abs( p_kursf ).
  p_kurs2 = abs( p_kurs2 ).

  CHECK ( p_kursf NE 0 ) AND
        ( p_dmbtr NE 0 ) AND
        ( p_dmbe2 NE 0 ).

  IF ( p_waers_doc = 'BRL' ).
    p_dmbe2 = p_dmbtr / p_kursf.

  ELSEIF ( p_waers_doc NE 'USD' ).

    IF ( p_kursf NE 0 ).
      p_dmbtr = p_wrbtr * p_kursf.
    ENDIF.

    IF ( p_kurs2 NE 0 ).
      p_dmbe2 = p_wrbtr * p_kurs2.
    ENDIF.

  ELSE.
    p_dmbtr = p_dmbe2 * p_kursf.
  ENDIF.

ENDFORM.

FORM append_bsk USING p_bsak TYPE ty_bsak
                      p_bkpf TYPE ty_bkpf
                      p_0080 TYPE zfit0080 .

  DATA: vl_lines_sakto TYPE i,
        vl_lines_matkl TYPE i,
        vl_idx_sakto   TYPE sy-tabix,
        vl_idx_matkl   TYPE sy-tabix,
        vl_rateio_r    TYPE bsak-dmbtr,
        vl_rateio_u    TYPE bsak-dmbtr,
        vl_dif_r       TYPE bsak-dmbtr,
        vl_dif_u       TYPE bsak-dmbtr.

  MOVE: p_bsak-hkont TO p_0080-hkont.

  vl_lines_sakto = lines( tg_sakto ).

  CLEAR: tg_0078, vl_idx_sakto, vl_idx_matkl, vl_rateio_r, vl_rateio_u.
  READ TABLE tg_0078 WITH KEY saknr    = p_bsak-saknr
                              cta_part = 'F'.
  IF ( sy-subrc = 0 ) AND
     ( vl_lines_sakto > 0 ).

    LOOP AT tg_sakto.

      ADD 1 TO vl_idx_sakto.

      p_bsak-xvlr_r = p_bsak-dmbtr * tg_sakto-perc.
      p_bsak-xvlr_u = p_bsak-dmbe2 * tg_sakto-perc.

      ADD p_bsak-xvlr_r TO vl_rateio_r.
      ADD p_bsak-xvlr_u TO vl_rateio_u.

      READ TABLE tg_0078 WITH KEY saknr = tg_sakto-sakto.

      IF ( sy-subrc = 0 ).
        MOVE tg_0078-cod_flx TO p_0080-cod_flx.
        MOVE tg_sakto-sakto  TO p_0080-hkont.
      ELSE.
        CLEAR: p_0080-cod_flx, p_0080-hkont.
      ENDIF.

      "Verifica se teve diferença no rateio dos valores das contas,
      "Caso encontre, joga a diferença na ultima conta.
      IF ( vl_idx_sakto = vl_lines_sakto ) AND ( vl_lines_sakto > 1 ).

        vl_dif_r = p_bsak-dmbtr - vl_rateio_r.
        vl_dif_u = p_bsak-dmbe2 - vl_rateio_u.

        IF vl_dif_r <> 0.
          p_bsak-xvlr_r = p_bsak-xvlr_r + vl_dif_r.
        ENDIF.

        IF vl_dif_u <> 0.
          p_bsak-xvlr_u = p_bsak-xvlr_u + vl_dif_u.
        ENDIF.

      ENDIF.

      IF p_0080-cod_flx IS NOT INITIAL.
        IF tg_0078-shkzg = 'S'.
          p_bsak-xvlr_r  = abs( p_bsak-xvlr_r ) * -1.
          p_bsak-xvlr_u  = abs( p_bsak-xvlr_u ) * -1.
        ELSEIF tg_0078-shkzg = 'H'.
          p_bsak-xvlr_r  = abs( p_bsak-xvlr_r ).
          p_bsak-xvlr_u  = abs( p_bsak-xvlr_u ).
        ENDIF.
      ENDIF.

      PERFORM atrib_bsak USING p_bsak p_0080.

      APPEND p_0080 TO tg_0080.

    ENDLOOP.

  ELSEIF p_bsak-ebeln IS NOT INITIAL.
    "Não tem classificação contabil no pedido, rateia pela grupo de Mercadoria.
    PERFORM get_matkl_ped USING p_bsak p_bkpf.

    IF tg_matkl[] IS NOT INITIAL.

      vl_lines_matkl = lines( tg_matkl ).

      LOOP AT tg_matkl.

        ADD 1 TO vl_idx_matkl.

        p_bsak-xvlr_r = p_bsak-dmbtr * tg_matkl-perc.
        p_bsak-xvlr_u = p_bsak-dmbe2 * tg_matkl-perc.

        ADD p_bsak-xvlr_r TO vl_rateio_r.
        ADD p_bsak-xvlr_u TO vl_rateio_u.

        MOVE tg_matkl-cod_flx TO p_0080-cod_flx.

        "Verifica se teve diferença no rateio dos valores das contas,
        "Caso encontre, joga a diferença na ultima conta.
        IF ( vl_idx_matkl = vl_lines_matkl ) AND ( vl_lines_matkl > 1 ).
          vl_dif_r = p_bsak-dmbtr - vl_rateio_r.
          vl_dif_u = p_bsak-dmbe2 - vl_rateio_u.

          IF vl_dif_r <> 0.
            p_bsak-xvlr_r = p_bsak-xvlr_r + vl_dif_r.
          ENDIF.

          IF vl_dif_u <> 0.
            p_bsak-xvlr_u = p_bsak-xvlr_u + vl_dif_u.
          ENDIF.
        ENDIF.

        IF tg_matkl-shkzg = 'S'.
          p_bsak-xvlr_r  = abs( p_bsak-xvlr_r ) * -1.
          p_bsak-xvlr_u  = abs( p_bsak-xvlr_u ) * -1.
        ELSEIF tg_matkl-shkzg = 'H'.
          p_bsak-xvlr_r  = abs( p_bsak-xvlr_r ).
          p_bsak-xvlr_u  = abs( p_bsak-xvlr_u ).
        ENDIF.

        PERFORM atrib_bsak USING p_bsak p_0080.

        APPEND p_0080 TO tg_0080.

      ENDLOOP.

    ELSE.
      PERFORM atrib_bsak USING p_bsak p_0080.
      APPEND p_0080 TO tg_0080.
    ENDIF.
  ELSE.
    PERFORM atrib_bsak USING p_bsak p_0080.
    APPEND p_0080 TO tg_0080.
  ENDIF.


ENDFORM.



FORM processar_bsad. " USING P_LCTO_COMP TYPE ZFIT0078-LCTO_COMP.

  DATA: vl_liq_cbanco  TYPE c,
        vl_cbanco_comp TYPE c,
        vl_cbanco_lcto TYPE c,
        vl_hkont_set   TYPE c,
        vl_ret         TYPE c,
        vl_kursf_lcto  LIKE vg_tx_conv,
        vl_kurs2_lcto  LIKE vg_tx_conv,
        vl_kursf_comp  LIKE vg_tx_conv,
        vl_kurs2_comp  LIKE vg_tx_conv,
        vl_kursf_set   LIKE vg_tx_conv,
        vl_kurs2_set   LIKE vg_tx_conv,
        vl_saknr_set   LIKE bsad-hkont,
        wl_0078        LIKE tg_0078,
        vl_shkzg       TYPE zfit0078-shkzg.

  CLEAR: vl_liq_cbanco, tg_0080, tg_skb1, tg_0078, tg_fagl, tg_vbap,
         tg_bsis_cbanco, vl_kursf_comp, vl_kurs2_comp, vl_kursf_set, vl_kurs2_set, vl_kursf_lcto, vl_kurs2_lcto, vl_shkzg,
         vl_cbanco_comp, vl_cbanco_lcto, vl_hkont_set, vl_saknr_set.

  "---------------------------------------------------*
  " "Não lista documentos Estornados.
  "---------------------------------------------------*

  "Verifica Doc. Compensação
  PERFORM get_bkpf USING tg_bsad-bukrs tg_bsad-augbl tg_bsad-augdt(4)
                CHANGING tg_bkpf_comp.
  CHECK ( sy-subrc EQ 0 ) AND ( tg_bkpf_comp-stblg IS INITIAL ).

  "Verifica Doc. Lcto
  PERFORM get_bkpf USING tg_bsad-bukrs tg_bsad-belnr tg_bsad-gjahr
                CHANGING tg_bkpf.
  CHECK ( sy-subrc EQ 0 ) AND ( tg_bkpf-stblg IS INITIAL ).

  "---------------------------------------------------*
  "Verificar se partida foi liquidada contra banco
  "---------------------------------------------------*

  "Verifica partida contra Banco no Doc. de Compensação
  PERFORM get_bsis_cbanco USING tg_bsad-bukrs tg_bsad-augbl tg_bsad-augdt(4)
                       CHANGING vl_kursf_comp
                                vl_kurs2_comp.
  IF sy-subrc = 0.
    vl_cbanco_comp = 'X'.
  ELSE.

    "Verifica partida contra Banco no Doc. de Lançamento
    PERFORM get_bsis_cbanco USING tg_bsad-bukrs tg_bsad-belnr tg_bsad-gjahr
                         CHANGING vl_kursf_lcto
                                  vl_kurs2_lcto.
    IF sy-subrc = 0.
      vl_cbanco_lcto = 'X'.
    ELSE.
      PERFORM get_hkont_set USING tg_bsad-bukrs tg_bsad-augbl tg_bsad-augdt(4)
                         CHANGING vl_kursf_set
                                  vl_kurs2_set
                                  vl_saknr_set.
      IF sy-subrc = 0.
        vl_hkont_set  = 'X'.
      ELSE.
        PERFORM get_hkont_set USING tg_bsad-bukrs tg_bsad-belnr tg_bsad-gjahr
                         CHANGING vl_kursf_set
                                  vl_kurs2_set
                                  vl_saknr_set.
        IF sy-subrc = 0.
          vl_hkont_set  = 'X'.
        ELSE.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

  IF ( vl_hkont_set  IS NOT INITIAL  ) AND
     ( tg_bsad-hkont EQ vl_saknr_set ).
    EXIT.
  ENDIF.

  READ TABLE tg_vbap WITH KEY vbeln = tg_bsad-vbel2
                              posnr = tg_bsad-posn2.

  PERFORM get_flexa USING tg_bsad-bukrs tg_bsad-belnr tg_bsad-gjahr tg_bsad-buzei
                 CHANGING tg_fagl.

  IF sy-subrc = 0.
    MOVE: tg_fagl-rmvct  TO tg_0080-rmvct.
  ENDIF.

  "---------------------------------------------------------------------*
  " Verifica se Lançamento já foi classificado durante o processamento.
  "---------------------------------------------------------------------*
*  READ TABLE TG_0078_BSAD WITH KEY BUKRS_L = TG_BSAD-BUKRS
*                                   BELNR_L = TG_BSAD-BELNR
*                                   GJAHR_L = TG_BSAD-GJAHR
*                                   BUZEI_L = TG_BSAD-BUZEI.
*  IF SY-SUBRC = 0.
*    CLEAR: WL_0078.
*    MOVE-CORRESPONDING TG_0078_BSAD TO WL_0078.
*    MOVE WL_0078-COD_FLX TO TG_0080-COD_FLX.
*  ELSE.

  "--------------------------------------------------------------*
  " Monta Estrutura de parâmetros para busca do código de Fluxo
  "--------------------------------------------------------------*
  CLEAR: wa_flx_par.
  MOVE: '4'                TO wa_flx_par-orig,
        tg_bsad-hkont      TO wa_flx_par-hkont,
        tg_bsad-kidno      TO wa_flx_par-kidno,
        tg_bsad-zuonr      TO wa_flx_par-zuonr,
        tg_bsad-blart      TO wa_flx_par-blart,
        tg_bsad-xblnr      TO wa_flx_par-xblnr,
        tg_0080-rmvct      TO wa_flx_par-rmvct,
        tg_vbap-matkl      TO wa_flx_par-matkl,
        tg_bsad-bschl      TO wa_flx_par-bschl,
        tg_bsad-bukrs      TO wa_flx_par-bukrs,
        tg_bsad-belnr      TO wa_flx_par-belnr,
        tg_bsad-gjahr      TO wa_flx_par-gjahr,
        tg_bsad-buzei      TO wa_flx_par-buzei.

  PERFORM atrib_cod_imposto USING tg_bkpf
                         CHANGING wa_flx_par-cod_imposto.

  PERFORM busca_cod_flx USING wa_flx_par
                     CHANGING tg_0080-cod_flx
                              wl_0078.

*    IF WL_0078 IS NOT INITIAL.
*      CLEAR: TG_0078_BSAD.
*      MOVE-CORRESPONDING WL_0078 TO TG_0078_BSAD.
*      TG_0078_BSAD-BUKRS_L = TG_BSAD-BUKRS.
*      TG_0078_BSAD-BELNR_L = TG_BSAD-BELNR.
*      TG_0078_BSAD-GJAHR_L = TG_BSAD-GJAHR.
*      TG_0078_BSAD-BUZEI_L = TG_BSAD-BUZEI.
*      APPEND TG_0078_BSAD.
*    ENDIF.
*
*  ENDIF.

  "---------------------------------------------------*
  "Verificar se partida foi liquidada contra banco
  "---------------------------------------------------*
*  IF ( TG_BSAD-AUGBL(2) EQ '20' ) OR
*     ( TG_BSAD-AUGBL(2) EQ '15' ).
*    VL_LIQ_CBANCO = 'X'.
*  ENDIF.

*  IF VL_LIQ_CBANCO IS INITIAL.

  "Caso documento tem conta parametrizada como data de Lançamento,
  "somente verificar se doc. contabil(Belnr) tem liq. contra contra banco.
*    IF P_LCTO_COMP NE 'L'.
*      PERFORM GET_BSIS USING TG_BSAD-BUKRS TG_BSAD-BELNR TG_BSAD-GJAHR
*                             'R' ''
*                    CHANGING VL_RET.
*
*      IF ( VL_RET IS INITIAL ).
*        PERFORM GET_BSAS USING TG_BSAD-BUKRS TG_BSAD-BELNR TG_BSAD-GJAHR
*                               'R' ''
*                      CHANGING VL_RET.
*      ENDIF.
*    ENDIF.

*    IF ( VL_RET IS NOT INITIAL ) OR ( P_LCTO_COMP EQ 'L' ).
*      PERFORM GET_BSIS_CBANCO USING TG_BSAD-BUKRS TG_BSAD-BELNR TG_BSAD-GJAHR
*                           CHANGING VL_KURSF.
*      IF SY-SUBRC = 0.
*        VL_LIQ_CBANCO = 'X'.
*      ENDIF.
*    ENDIF.
*  ENDIF.

  CASE wl_0078-lcto_comp.
    WHEN 'L'.
      tg_bsad-augdt = tg_bsad-budat.
    WHEN OTHERS.
      tg_bsad-augdt = tg_bkpf_comp-budat.
  ENDCASE.

  CHECK tg_bsad-augdt IN s_budat.

  IF ( vl_hkont_set IS INITIAL ) AND ( vl_cbanco_comp IS INITIAL ).
    CHECK ( wl_0078-lcto_comp = 'L' ) AND ( vl_cbanco_lcto IS NOT INITIAL ).
  ENDIF.

  IF ( vl_cbanco_comp IS NOT INITIAL ) OR ( vl_hkont_set IS NOT INITIAL ).

    IF ( vl_hkont_set IS NOT INITIAL ).
      vl_kursf_comp = vl_kursf_set.
      vl_kurs2_comp = vl_kurs2_set.
    ENDIF.

    IF ( vl_kursf_comp IS NOT INITIAL ).
      PERFORM conv_taxa_inf USING tg_bkpf_comp-waers
                                  vl_kursf_comp
                                  vl_kurs2_comp
                         CHANGING tg_bsad-dmbtr
                                  tg_bsad-dmbe2
                                  tg_bsad-wrbtr.
    ELSE.
      PERFORM conv_taxa USING tg_bkpf_comp-waers
                              tg_bkpf_comp-kursf
                              tg_bkpf_comp-kurs2
                     CHANGING tg_bsad-dmbtr
                              tg_bsad-dmbe2
                              tg_bsad-wrbtr.
    ENDIF.

    "Verifica se possui residual na compensação
    READ TABLE tg_bsad_rsd WITH KEY bukrs  = tg_bsad-bukrs
                                    kunnr  = tg_bsad-kunnr
                                    belnr  = tg_bsad-augbl
                                    waers  = tg_bsad-waers
                                    shkzg  = tg_bsad-shkzg
                                    rebzg  = tg_bsad-rebzg
                                    BINARY SEARCH.

    IF sy-subrc NE 0.
      READ TABLE tg_bsad_rsd WITH KEY bukrs  = tg_bsad-bukrs
                                      kunnr  = tg_bsad-kunnr
                                      belnr  = tg_bsad-augbl
                                      waers  = tg_bsad-waers
                                      shkzg  = tg_bsad-shkzg
                                      rebzg  = tg_bsad-belnr
                                      BINARY SEARCH.
    ENDIF.

    IF sy-subrc = 0.
      IF ( tg_bsad-dmbtr - tg_bsad_rsd-dmbtr ) > 0.
        tg_bsad-dmbtr = tg_bsad-dmbtr - tg_bsad_rsd-dmbtr.
        tg_bsad-dmbe2 = tg_bsad-dmbe2 - tg_bsad_rsd-dmbe2.
      ENDIF.
    ENDIF.

  ENDIF.

  IF wl_0078-shkzg IS NOT INITIAL.
    vl_shkzg = wl_0078-shkzg.
  ELSE.
    vl_shkzg = tg_bsad-shkzg.
  ENDIF.

  IF vl_shkzg = 'S'.
    tg_bsad-dmbtr  = abs( tg_bsad-dmbtr ) * -1.
    tg_bsad-dmbe2  = abs( tg_bsad-dmbe2 ) * -1.
  ELSEIF vl_shkzg = 'H'.
    tg_bsad-dmbtr  = abs( tg_bsad-dmbtr ).
    tg_bsad-dmbe2  = abs( tg_bsad-dmbe2 ).
  ENDIF.

  tg_bsad-venci = tg_bsad-zfbdt + tg_bsad-zbd1t.

  MOVE: tg_bsad-bukrs   TO tg_0080-bukrs,
        tg_bsad-kunnr   TO tg_0080-kunnr,
        tg_bsad-belnr   TO tg_0080-belnr,
        tg_bsad-gjahr   TO tg_0080-gjahr,
        tg_bsad-buzei   TO tg_0080-buzei,
        tg_bsad-budat   TO tg_0080-budat,
        tg_bsad-venci   TO tg_0080-zfbdt,
        tg_bsad-hkont   TO tg_0080-hkont,
        tg_bsad-bldat   TO tg_0080-bldat,
        tg_bsad-waers   TO tg_0080-waers,
        tg_bsad-xblnr   TO tg_0080-xblnr,
        tg_bsad-blart   TO tg_0080-blart,
        tg_bsad-gsber   TO tg_0080-gsber,
        tg_bsad-vbeln   TO tg_0080-vbeln,
        tg_bsad-bschl   TO tg_0080-bschl,
        tg_bsad-shkzg   TO tg_0080-shkzg,
        tg_bsad-dmbtr   TO tg_0080-dmbtr,
        tg_bsad-dmbe2   TO tg_0080-dmbe2,
        vl_data         TO tg_0080-dt_atual,
        vl_hora         TO tg_0080-hr_atual,
        tg_bsad-augbl   TO tg_0080-augbl,
        tg_bsad-augdt   TO tg_0080-augdt.
  "WA_SKB1-SAKNR   TO TG_0080-SAKNR.

  APPEND tg_0080.

ENDFORM.

FORM processar_bsak. " USING P_LCTO_COMP TYPE ZFIT0078-LCTO_COMP.

  DATA: vl_liq_cbanco  TYPE c,
        vl_cbanco_comp TYPE c,
        vl_cbanco_lcto TYPE c,
        vl_hkont_set   TYPE c,
        vl_ret         TYPE c,
        vl_kursf_lcto  LIKE vg_tx_conv,
        vl_kurs2_lcto  LIKE vg_tx_conv,
        vl_kursf_comp  LIKE vg_tx_conv,
        vl_kurs2_comp  LIKE vg_tx_conv,
        vl_kursf_set   LIKE vg_tx_conv,
        vl_kurs2_set   LIKE vg_tx_conv,
        vl_saknr_set   LIKE bsak-hkont,
        wl_0078        LIKE tg_0078,
        vl_shkzg       TYPE zfit0078-shkzg.

  CLEAR: vl_liq_cbanco, tg_0080, tg_fagl,tg_bkpf, tg_skb1, tg_0078,
         tg_bkpf, tg_bsis_cbanco,  vl_kursf_comp, vl_kurs2_comp, vl_kursf_lcto, vl_kurs2_lcto, vl_kursf_set,vl_kurs2_set, vl_shkzg,
         vl_cbanco_comp, vl_cbanco_lcto, vl_hkont_set, vl_saknr_set.

  "---------------------------------------------------*
  " "Não lista documentos Estornados.
  "---------------------------------------------------*

  "Verifica Doc. Compensação
  PERFORM get_bkpf USING tg_bsak-bukrs tg_bsak-augbl tg_bsak-augdt(4)
                CHANGING tg_bkpf_comp.
  CHECK ( sy-subrc EQ 0 ) AND ( tg_bkpf_comp-stblg IS INITIAL ).

  "Verifica Doc. Lcto
  PERFORM get_bkpf USING tg_bsak-bukrs tg_bsak-belnr tg_bsak-gjahr
                CHANGING tg_bkpf.
  CHECK ( sy-subrc EQ 0 ) AND ( tg_bkpf-stblg IS INITIAL ).


  "---------------------------------------------------*
  "Verificar se partida foi liquidada contra banco
  "---------------------------------------------------*

  "Verifica partida contra Banco no Doc. de Compensação
  PERFORM get_bsis_cbanco USING tg_bsak-bukrs tg_bsak-augbl tg_bsak-augdt(4)
                       CHANGING vl_kursf_comp
                                vl_kurs2_comp.
  IF sy-subrc = 0.
    vl_cbanco_comp = 'X'.
  ELSE.

    "Verifica partida contra Banco no Doc. de Lançamento
    PERFORM get_bsis_cbanco USING tg_bsak-bukrs tg_bsak-belnr tg_bsak-gjahr
                         CHANGING vl_kursf_lcto
                                  vl_kurs2_lcto.
    IF sy-subrc = 0.
      vl_cbanco_lcto = 'X'.
    ELSE.
      PERFORM get_hkont_set USING tg_bsak-bukrs tg_bsak-augbl tg_bsak-augdt(4)
                         CHANGING vl_kursf_set
                                  vl_kurs2_set
                                  vl_saknr_set.
      IF sy-subrc = 0.
        vl_hkont_set  = 'X'.
      ELSE.
        PERFORM get_hkont_set USING tg_bsak-bukrs tg_bsak-belnr tg_bsak-gjahr
                         CHANGING vl_kursf_set
                                  vl_kurs2_set
                                  vl_saknr_set.
        IF sy-subrc = 0.
          vl_hkont_set  = 'X'.
        ELSE.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

  IF ( vl_hkont_set  IS NOT INITIAL  ) AND
     ( tg_bsak-hkont EQ vl_saknr_set ).
    EXIT.
  ENDIF.

  PERFORM get_flexa USING tg_bsak-bukrs tg_bsak-belnr tg_bsak-gjahr tg_bsak-buzei
                 CHANGING tg_fagl.

  IF sy-subrc = 0.
    MOVE: tg_fagl-rmvct TO tg_0080-rmvct.
  ENDIF.

  "---------------------------------------------------------------------*
  " Verifica se Lançamento já foi classificado durante o processamento.
  "---------------------------------------------------------------------*
*  READ TABLE TG_0078_BSAK WITH KEY BUKRS_L = TG_BSAK-BUKRS
*                                   BELNR_L = TG_BSAK-BELNR
*                                   GJAHR_L = TG_BSAK-GJAHR
*                                   BUZEI_L = TG_BSAK-BUZEI.
*  IF SY-SUBRC = 0.
*    CLEAR: WL_0078.
*    MOVE-CORRESPONDING TG_0078_BSAK TO WL_0078.
*    MOVE WL_0078-COD_FLX TO TG_0080-COD_FLX.
*  ELSE.

  "--------------------------------------------------------------*
  " Monta Estrutura de parâmetros para busca do código de Fluxo
  "--------------------------------------------------------------*
  CLEAR: wa_flx_par.
  MOVE: '1'                TO wa_flx_par-orig,
        tg_bsak-hkont      TO wa_flx_par-hkont,
        tg_bsak-kidno      TO wa_flx_par-kidno,
        tg_bsak-zuonr      TO wa_flx_par-zuonr,
        tg_bsak-blart      TO wa_flx_par-blart,
        tg_bsak-xblnr      TO wa_flx_par-xblnr,
        tg_0080-rmvct      TO wa_flx_par-rmvct,
        tg_bsak-bschl      TO wa_flx_par-bschl,
        tg_bsak-lifnr      TO wa_flx_par-lifnr,
        tg_bsak-xref3      TO wa_flx_par-xref3,
        tg_bsak-bukrs      TO wa_flx_par-bukrs,
        tg_bsak-belnr      TO wa_flx_par-belnr,
        tg_bsak-gjahr      TO wa_flx_par-gjahr,
        tg_bsak-buzei      TO wa_flx_par-buzei.

  PERFORM atrib_cod_imposto USING tg_bkpf
                         CHANGING wa_flx_par-cod_imposto.

  PERFORM busca_cod_flx USING wa_flx_par
                     CHANGING tg_0080-cod_flx
                              wl_0078.

*    IF WL_0078 IS NOT INITIAL.
*      CLEAR: TG_0078_BSAK.
*      MOVE-CORRESPONDING WL_0078 TO TG_0078_BSAK.
*      TG_0078_BSAK-BUKRS_L = TG_BSAK-BUKRS.
*      TG_0078_BSAK-BELNR_L = TG_BSAK-BELNR.
*      TG_0078_BSAK-GJAHR_L = TG_BSAK-GJAHR.
*      TG_0078_BSAK-BUZEI_L = TG_BSAK-BUZEI.
*      APPEND TG_0078_BSAK.
*    ENDIF.
*
*  ENDIF.

  "---------------------------------------------------*
  "Verificar se partida foi liquidada contra banco
  "---------------------------------------------------*
*  IF ( TG_BSAK-AUGBL(2) EQ '20' ) OR
*     ( TG_BSAK-AUGBL(2) EQ '15' ).
*    VL_LIQ_CBANCO = 'X'.
*  ENDIF.
*
*  IF VL_LIQ_CBANCO IS INITIAL.
*
*    "Caso documento tem conta parametrizada como data de Lançamento,
*    "somente verificar se doc. contabil(Belnr) tem liq. contra contra banco.
*    IF P_LCTO_COMP NE 'L'.
*      PERFORM GET_BSIS USING TG_BSAK-BUKRS TG_BSAK-BELNR TG_BSAK-GJAHR
*                             'R' ''
*                    CHANGING VL_RET.
*
*      IF ( VL_RET IS INITIAL ).
*        PERFORM GET_BSAS USING TG_BSAK-BUKRS TG_BSAK-BELNR TG_BSAK-GJAHR
*                               'R' ''
*                      CHANGING VL_RET.
*      ENDIF.
*    ENDIF.
*
*    IF ( VL_RET IS NOT INITIAL ) OR ( P_LCTO_COMP EQ 'L' ).
*      PERFORM GET_BSIS_CBANCO USING TG_BSAK-BUKRS TG_BSAK-BELNR TG_BSAK-GJAHR
*                           CHANGING VL_KURSF.
*
*      IF SY-SUBRC = 0.
*        VL_LIQ_CBANCO = 'X'.
*      ENDIF.
*    ENDIF.
*  ENDIF.

  CASE wl_0078-lcto_comp.
    WHEN 'L'.
      tg_bsak-augdt = tg_bsak-budat.
    WHEN OTHERS.
      tg_bsak-augdt = tg_bkpf_comp-budat.
  ENDCASE.

  CHECK tg_bsak-augdt IN s_budat.

  IF ( vl_hkont_set IS INITIAL ) AND ( vl_cbanco_comp IS INITIAL ).
    CHECK ( wl_0078-lcto_comp = 'L' ) AND ( vl_cbanco_lcto IS NOT INITIAL ).
  ENDIF.

  IF ( vl_cbanco_comp IS NOT INITIAL ) OR ( vl_hkont_set IS NOT INITIAL ).

    IF ( vl_hkont_set IS NOT INITIAL ).
      vl_kursf_comp = vl_kursf_set.
      vl_kurs2_comp = vl_kurs2_set.
    ENDIF.

    IF vl_kursf_comp IS NOT INITIAL.
      PERFORM conv_taxa_inf USING tg_bkpf_comp-waers
                                  vl_kursf_comp
                                  vl_kurs2_comp
                         CHANGING tg_bsak-dmbtr
                                  tg_bsak-dmbe2
                                  tg_bsak-wrbtr.
    ELSE.
      PERFORM conv_taxa USING tg_bkpf_comp-waers
                              tg_bkpf_comp-kursf
                              tg_bkpf_comp-kurs2
                     CHANGING tg_bsak-dmbtr
                              tg_bsak-dmbe2
                              tg_bsak-wrbtr.
    ENDIF.

    "Verifica se possui residual na compensação
    READ TABLE tg_bsak_rsd WITH KEY bukrs  = tg_bsak-bukrs
                                    lifnr  = tg_bsak-lifnr
                                    belnr  = tg_bsak-augbl
                                    waers  = tg_bsak-waers
                                    shkzg  = tg_bsak-shkzg
                                    rebzg  = tg_bsak-rebzg
                                    BINARY SEARCH.

    IF sy-subrc NE 0.
      READ TABLE tg_bsak_rsd WITH KEY bukrs  = tg_bsak-bukrs
                                      lifnr  = tg_bsak-lifnr
                                      belnr  = tg_bsak-augbl
                                      waers  = tg_bsak-waers
                                      shkzg  = tg_bsak-shkzg
                                      rebzg  = tg_bsak-belnr
                                      BINARY SEARCH.
    ENDIF.

    IF sy-subrc = 0.
      IF ( tg_bsak-dmbtr - tg_bsak_rsd-dmbtr ) > 0.
        tg_bsak-dmbtr = tg_bsak-dmbtr - tg_bsak_rsd-dmbtr.
        tg_bsak-dmbe2 = tg_bsak-dmbe2 - tg_bsak_rsd-dmbe2.
      ENDIF.
    ENDIF.

  ENDIF.


  IF wl_0078-shkzg IS NOT INITIAL.
    IF wl_0078-shkzg = 'S'.
      tg_bsak-dmbtr  = abs( tg_bsak-dmbtr ) * -1.
      tg_bsak-dmbe2  = abs( tg_bsak-dmbe2 ) * -1.
    ELSEIF wl_0078-shkzg = 'H'.
      tg_bsak-dmbtr  = abs( tg_bsak-dmbtr ).
      tg_bsak-dmbe2  = abs( tg_bsak-dmbe2 ).
    ENDIF.
  ELSE.
    IF tg_bsak-shkzg = 'H'.
      tg_bsak-dmbtr  = abs( tg_bsak-dmbtr ) * -1.
      tg_bsak-dmbe2  = abs( tg_bsak-dmbe2 ) * -1.
    ELSE.
      tg_bsak-dmbtr  = abs( tg_bsak-dmbtr ).
      tg_bsak-dmbe2  = abs( tg_bsak-dmbe2 ).
    ENDIF.
  ENDIF.

  tg_bsak-venci = tg_bsak-zfbdt + tg_bsak-zbd1t.

  CLEAR: wl_cls_ped.
  MOVE-CORRESPONDING tg_bsak TO wl_cls_ped.

  PERFORM get_contas_ped USING wl_cls_ped tg_bkpf.

  PERFORM append_bsk USING tg_bsak tg_bkpf tg_0080.

ENDFORM.

FORM iniciar_variaveis.

  REFRESH: tg_0078_r,
           tg_0078_l,
           tg_0078,
           it_contas,
           it_gjahr,
           it_date,
           tg_bsak_bsd,
           it_bsik,
           tg_bsis,
           tg_bsad,
           tg_bsad_aux,
           tg_bsak_aux,
           tg_bsak,
           tg_bkpf,
           tg_fagl,
           "TG_BSIS_RAZAO,
           "TG_BSAS_RAZAO,
           tg_bsis_cbanco,
           tg_vbap,
           tg_ekkn,
           tg_ekpo,
           tg_0036,
           tg_0080,
           tg_0078_bsad,
           tg_0078_bsak.

ENDFORM.

FORM get_bkpf USING p_bukrs TYPE bkpf-bukrs
                    p_belnr TYPE bkpf-belnr
                    p_gjahr TYPE bkpf-gjahr
           CHANGING p_bkpf  TYPE ty_bkpf.

  CLEAR: p_bkpf.

  SELECT SINGLE bukrs belnr gjahr stblg awkey blart budat kursf kurs2 waers
    FROM bkpf INTO CORRESPONDING FIELDS OF p_bkpf
   WHERE bukrs = p_bukrs
     AND belnr = p_belnr
     AND gjahr = p_gjahr.

ENDFORM.

FORM get_flexa USING p_bukrs TYPE bsak-bukrs
                     p_belnr TYPE bsak-belnr
                     p_gjahr TYPE bsak-gjahr
                     p_buzei TYPE bsak-buzei
            CHANGING p_flexa LIKE tg_fagl.

  CLEAR: p_flexa.

  READ TABLE tg_fagl INTO p_flexa WITH KEY ryear   = p_gjahr
                                           docnr   = p_belnr
                                           rldnr   = '0L'
                                           rbukrs  = p_bukrs
                                           buzei   = p_buzei BINARY SEARCH.


*  SELECT SINGLE RYEAR RLDNR DOCNR RBUKRS BSCHL
*         RACCT HSL KSL DRCRK BUZEI USNAM RMVCT
*    FROM FAGLFLEXA INTO P_FLEXA
*   WHERE RYEAR   EQ P_GJAHR
*     AND DOCNR   EQ P_BELNR
*     AND RLDNR   EQ '0L'
*     AND RBUKRS  EQ P_BUKRS
*     AND BUZEI   EQ P_BUZEI.

ENDFORM.

*FORM GET_BSIS USING P_BUKRS     TYPE BSIS-BUKRS
*                    P_BELNR     TYPE BSIS-BELNR
*                    P_GJAHR     TYPE BSIS-GJAHR
*                    P_CTA_PART  TYPE ZFIT0078-CTA_PART
*                    P_LCTO_COMP TYPE ZFIT0078-LCTO_COMP
*           CHANGING P_RET       TYPE C.
*
*  CLEAR: TG_BSIS_RAZAO, P_RET.
*
*  IF ( P_CTA_PART  IS NOT INITIAL ) AND
*     ( P_LCTO_COMP IS NOT INITIAL ).
*
*    SELECT SINGLE A~BUKRS A~BELNR A~GJAHR A~HKONT B~CTA_PART B~LCTO_COMP
*           INTO TG_BSIS_RAZAO
*      FROM BSIS AS A INNER JOIN ZFIT0078 AS B ON A~HKONT = B~SAKNR
*     WHERE A~BUKRS     EQ P_BUKRS
*       AND A~GJAHR     EQ P_GJAHR
*       AND A~BELNR     EQ P_BELNR
*       AND B~CTA_PART  EQ P_CTA_PART
*       AND B~LCTO_COMP EQ P_LCTO_COMP.
*
*    IF SY-SUBRC = 0.
*      P_RET = 'X'.
*    ENDIF.
*
*  ELSEIF ( P_CTA_PART  IS NOT INITIAL ).
*
*    SELECT SINGLE A~BUKRS A~BELNR A~GJAHR A~HKONT B~CTA_PART B~LCTO_COMP
*           INTO TG_BSIS_RAZAO
*      FROM BSIS AS A INNER JOIN ZFIT0078 AS B ON A~HKONT = B~SAKNR
*     WHERE A~BUKRS     EQ P_BUKRS
*       AND A~GJAHR     EQ P_GJAHR
*       AND A~BELNR     EQ P_BELNR
*       AND B~CTA_PART  EQ P_CTA_PART.
*
*    IF SY-SUBRC = 0.
*      P_RET = 'X'.
*    ENDIF.
*
*  ELSEIF ( P_LCTO_COMP IS NOT INITIAL ).
*
*    SELECT SINGLE A~BUKRS A~BELNR A~GJAHR A~HKONT B~CTA_PART B~LCTO_COMP
*           INTO TG_BSIS_RAZAO
*      FROM BSIS AS A INNER JOIN ZFIT0078 AS B ON A~HKONT = B~SAKNR
*     WHERE A~BUKRS     EQ P_BUKRS
*       AND A~GJAHR     EQ P_GJAHR
*       AND A~BELNR     EQ P_BELNR
*       AND B~LCTO_COMP EQ P_LCTO_COMP.
*
*    IF SY-SUBRC = 0.
*      P_RET = 'X'.
*    ENDIF.
*
*  ENDIF.
*
*ENDFORM.

*FORM GET_BSAS USING P_BUKRS     TYPE BSAS-BUKRS
*                    P_BELNR     TYPE BSAS-BELNR
*                    P_GJAHR     TYPE BSAS-GJAHR
*                    P_CTA_PART  TYPE ZFIT0078-CTA_PART
*                    P_LCTO_COMP TYPE ZFIT0078-LCTO_COMP
*           CHANGING P_RET       TYPE C.
*
*  CLEAR: TG_BSAS_RAZAO, P_RET.
*
*  IF ( P_CTA_PART  IS NOT INITIAL ) AND
*     ( P_LCTO_COMP IS NOT INITIAL ).
*
*    SELECT SINGLE A~BUKRS A~BELNR A~GJAHR A~HKONT B~CTA_PART B~LCTO_COMP
*           INTO TG_BSAS_RAZAO
*      FROM BSAS AS A INNER JOIN ZFIT0078 AS B ON A~HKONT = B~SAKNR
*     WHERE A~BUKRS     EQ P_BUKRS
*       AND A~GJAHR     EQ P_GJAHR
*       AND A~BELNR     EQ P_BELNR
*       AND B~CTA_PART  EQ P_CTA_PART
*       AND B~LCTO_COMP EQ P_LCTO_COMP.
*
*    IF SY-SUBRC = 0.
*      P_RET = 'X'.
*    ENDIF.
*
*  ELSEIF ( P_CTA_PART  IS NOT INITIAL ).
*
*    SELECT SINGLE A~BUKRS A~BELNR A~GJAHR A~HKONT B~CTA_PART B~LCTO_COMP
*           INTO TG_BSAS_RAZAO
*      FROM BSAS AS A INNER JOIN ZFIT0078 AS B ON A~HKONT = B~SAKNR
*     WHERE A~BUKRS     EQ P_BUKRS
*       AND A~GJAHR     EQ P_GJAHR
*       AND A~BELNR     EQ P_BELNR
*       AND B~CTA_PART  EQ P_CTA_PART.
*
*    IF SY-SUBRC = 0.
*      P_RET = 'X'.
*    ENDIF.
*
*  ELSEIF ( P_LCTO_COMP IS NOT INITIAL ).
*
*    SELECT SINGLE A~BUKRS A~BELNR A~GJAHR A~HKONT B~CTA_PART B~LCTO_COMP
*           INTO TG_BSAS_RAZAO
*      FROM BSAS AS A INNER JOIN ZFIT0078 AS B ON A~HKONT = B~SAKNR
*     WHERE A~BUKRS     EQ P_BUKRS
*       AND A~GJAHR     EQ P_GJAHR
*       AND A~BELNR     EQ P_BELNR
*       AND B~LCTO_COMP EQ P_LCTO_COMP.
*
*    IF SY-SUBRC = 0.
*      P_RET = 'X'.
*    ENDIF.
*
*  ENDIF.
*
*ENDFORM.


FORM get_bsis_cbanco USING p_bukrs     TYPE bsas-bukrs
                           p_belnr     TYPE bsas-belnr
                           p_gjahr     TYPE bsas-gjahr
                  CHANGING p_kursf     LIKE vg_tx_conv
                           p_kurs2     LIKE vg_tx_conv.

  CLEAR: tg_bsis_cbanco, p_kursf, p_kurs2.

*  SELECT SINGLE BSIS~BUKRS BSIS~BELNR BSIS~GJAHR BSIS~HKONT BSIS~DMBTR BSIS~DMBE2
*    INTO CORRESPONDING FIELDS OF TG_BSIS_CBANCO
*    FROM BSIS INNER JOIN SKB1 ON BSIS~BUKRS = SKB1~BUKRS
*                             AND BSIS~HKONT = SKB1~SAKNR
*  WHERE BSIS~BUKRS EQ P_BUKRS
*    AND BSIS~GJAHR EQ P_GJAHR
*    AND BSIS~BELNR EQ P_BELNR
*    AND SKB1~FDLEV IN ( 'F0' , 'B2' ).

  READ TABLE tg_bsis_cbanco WITH KEY bukrs = p_bukrs
                                     gjahr = p_gjahr
                                     belnr = p_belnr BINARY SEARCH.
  IF ( sy-subrc = 0 ) AND
     ( tg_bsis_cbanco-dmbtr > 0 ) AND
     ( tg_bsis_cbanco-dmbe2 > 0 ).

    TRY.
        p_kursf = tg_bsis_cbanco-dmbtr / tg_bsis_cbanco-dmbe2.
      CATCH cx_sy_arithmetic_overflow.
    ENDTRY.

    IF ( tg_bsis_cbanco-waers IS NOT INITIAL ) AND
       ( tg_bsis_cbanco-waers NE 'BRL'       ) AND
       ( tg_bsis_cbanco-waers NE 'USD'       ) AND
       ( tg_bsis_cbanco-wrbtr > 0            ).

      TRY.
          p_kursf = tg_bsis_cbanco-dmbtr / tg_bsis_cbanco-wrbtr.
          p_kurs2 = tg_bsis_cbanco-dmbe2 / tg_bsis_cbanco-wrbtr.
        CATCH cx_sy_arithmetic_overflow.
      ENDTRY.
    ENDIF.

  ENDIF.


ENDFORM.

FORM get_hkont_set USING p_bukrs     TYPE bsis-bukrs
                         p_belnr     TYPE bsis-belnr
                         p_gjahr     TYPE bsis-gjahr
                CHANGING p_kursf     LIKE vg_tx_conv
                         p_kurs2     LIKE vg_tx_conv
                         p_hkont     LIKE bsis-hkont.

  CLEAR: tg_bsis_set, tg_bsas_set, p_kursf, p_kurs2, p_hkont.

  READ TABLE tg_bsis_set WITH KEY bukrs = p_bukrs
                                  gjahr = p_gjahr
                                  belnr = p_belnr BINARY SEARCH.
  IF ( sy-subrc = 0 ) AND
     ( tg_bsis_set-dmbtr > 0 ) AND
     ( tg_bsis_set-dmbe2 > 0 ).

    TRY.
        p_kursf = tg_bsis_set-dmbtr / tg_bsis_set-dmbe2.
      CATCH cx_sy_arithmetic_overflow.
    ENDTRY.

    p_hkont = tg_bsis_set-hkont.

  ELSE.
    READ TABLE tg_bsas_set WITH KEY bukrs = p_bukrs
                                    gjahr = p_gjahr
                                    belnr = p_belnr BINARY SEARCH.

    IF ( sy-subrc = 0 ) AND
       ( tg_bsas_set-dmbtr > 0 ) AND
       ( tg_bsas_set-dmbe2 > 0 ).

      TRY.
          p_kursf = tg_bsas_set-dmbtr / tg_bsas_set-dmbe2.
        CATCH cx_sy_arithmetic_overflow.
      ENDTRY.

      p_hkont = tg_bsas_set-hkont.
    ENDIF.
  ENDIF.

*  SELECT BUKRS BELNR GJAHR HKONT DMBTR DMBE2
*    FROM BSIS INTO CORRESPONDING FIELDS OF TABLE TG_BSIS_SET
*    FOR ALL ENTRIES IN IT_CONTAS
*   WHERE BUKRS  EQ P_BUKRS
*     AND GJAHR  EQ P_GJAHR
*     AND BELNR  EQ P_BELNR
*     AND HKONT  EQ IT_CONTAS-HKONT.
*
*  IF ( SY-SUBRC = 0 ) AND
*     ( TG_BSIS_SET-DMBTR > 0 ) AND
*     ( TG_BSIS_SET-DMBE2 > 0 ).
*    P_KURSF = TG_BSIS_SET-DMBTR / TG_BSIS_SET-DMBE2.
*  ELSE.
*    SELECT BUKRS BELNR GJAHR HKONT DMBTR DMBE2
*      FROM BSAS INTO CORRESPONDING FIELDS OF TABLE TG_BSAS_SET
*      FOR ALL ENTRIES IN IT_CONTAS
*     WHERE BUKRS  EQ P_BUKRS
*       AND GJAHR  EQ P_GJAHR
*       AND BELNR  EQ P_BELNR
*       AND HKONT  EQ IT_CONTAS-HKONT.
*
*    IF ( SY-SUBRC = 0 ) AND
*       ( TG_BSAS_SET-DMBTR > 0 ) AND
*       ( TG_BSAS_SET-DMBE2 > 0 ).
*      P_KURSF = TG_BSAS_SET-DMBTR / TG_BSAS_SET-DMBE2.
*    ENDIF.
*  ENDIF.

ENDFORM.

FORM atrib_cod_imposto  USING    p_bkpf        TYPE ty_bkpf
                        CHANGING p_cod_imposto TYPE zfit0078-cod_imposto.

  DATA: vl_doc_imposto TYPE zimp_lanc_impost-doc_imposto.

  CLEAR: p_cod_imposto.

  CHECK   p_bkpf-blart = 'TB' AND
        ( p_bkpf-awkey(02) = 'ZP' OR p_bkpf-awkey(04) = 'ZIMP' ).

  IF p_bkpf-awkey(02) = 'ZP'.
    vl_doc_imposto = p_bkpf-awkey+06(10).
  ELSEIF p_bkpf-awkey(04) = 'ZIMP'.
    vl_doc_imposto = p_bkpf-awkey+04(10).
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vl_doc_imposto
    IMPORTING
      output = vl_doc_imposto.

  SELECT SINGLE cod_imposto
    FROM zimp_lanc_impost INTO p_cod_imposto
   WHERE doc_imposto = vl_doc_imposto
     AND bukrs       = p_bkpf-bukrs.

ENDFORM.

FORM get_matkl_ped USING p_bsak TYPE ty_bsak
                         p_bkpf TYPE ty_bkpf.



  CLEAR: tg_matkl[], tg_matkl_aux[], tg_rseg[], p_bsak-xtot.

  IF ( p_bsak-ebeln IS NOT INITIAL ) AND
     ( p_bsak-ebelp IS INITIAL ) AND
     ( strlen( p_bkpf-awkey ) = 14 ).

    SELECT ebeln ebelp
      FROM rseg INTO TABLE tg_rseg
     WHERE belnr = p_bkpf-awkey(10)
       AND gjahr = p_bkpf-awkey+10(4).

  ENDIF.

  "Busca Contas e Valores para Rateio
  LOOP AT tg_ekpo WHERE ebeln = p_bsak-ebeln.

    IF p_bsak-ebelp IS NOT INITIAL .
      CHECK p_bsak-ebelp = tg_ekpo-ebelp.
    ELSE.
      IF ( ( strlen( p_bkpf-awkey ) = 14 ) AND ( tg_rseg[] IS NOT INITIAL ) ).
        READ TABLE tg_rseg WITH KEY ebelp = tg_ekpo-ebelp.
        CHECK sy-subrc EQ 0.
      ENDIF.
    ENDIF.

    CHECK tg_ekpo-netwr > 0.
    ADD tg_ekpo-netwr TO p_bsak-xtot.

    READ TABLE tg_matkl WITH KEY matkl = tg_ekpo-matkl.
    IF sy-subrc = 0.
      LOOP AT tg_matkl WHERE matkl = tg_ekpo-matkl.
        ADD tg_ekpo-netwr TO tg_matkl-netwr.
        MODIFY tg_matkl.
      ENDLOOP.
    ELSE.
      CLEAR: tg_matkl.
      tg_matkl-matkl = tg_ekpo-matkl.
      ADD tg_ekpo-netwr TO tg_matkl-netwr.
      APPEND tg_matkl.
    ENDIF.
  ENDLOOP.

  IF ( p_bsak-xtot = 0 ).
    REFRESH tg_matkl.
    EXIT.
  ENDIF.

  "Calcula Proporcional.
  LOOP AT tg_matkl.
    "Busca Classificação por Grupo de Mercadoria
    READ TABLE tg_0078 WITH KEY matkl = tg_matkl-matkl
                                saknr = p_bsak-hkont.
    IF ( sy-subrc = 0 ).
      tg_matkl-cod_flx = tg_0078-cod_flx.
      tg_matkl-shkzg   = tg_0078-shkzg.
    ELSE.
      REFRESH tg_matkl.
      RETURN.
    ENDIF.

    tg_matkl-perc = tg_matkl-netwr / p_bsak-xtot.
    MODIFY tg_matkl.
  ENDLOOP.

  SORT tg_matkl BY matkl.
  DELETE ADJACENT DUPLICATES FROM tg_matkl COMPARING matkl.

  tg_matkl_aux[] = tg_matkl[].

  SORT tg_matkl_aux BY cod_flx.
  DELETE ADJACENT DUPLICATES FROM tg_matkl_aux COMPARING cod_flx.

  "Agrupa Cod. Fluxo Iguais
  IF lines( tg_matkl_aux[] ) NE lines( tg_matkl[] ).

    LOOP AT tg_matkl_aux.

      CLEAR: tg_matkl_aux-perc, tg_matkl_aux-netwr.

      LOOP AT tg_matkl WHERE cod_flx = tg_matkl_aux-cod_flx.
        ADD tg_matkl-perc  TO tg_matkl_aux-perc.
        ADD tg_matkl-netwr TO tg_matkl_aux-netwr.
      ENDLOOP.

      MODIFY tg_matkl_aux.

    ENDLOOP.

    tg_matkl[] = tg_matkl_aux[].

  ENDIF.


ENDFORM.
**<<<------"187070 - NMS - INI------>>>
*&----------------------------------------------------------------------------------*
*&      Form  ZF_LIMIT_SELECT_OPTION
*&----------------------------------------------------------------------------------*
*       Restringe as opções da tela de seleção
*-----------------------------------------------------------------------------------*
FORM zf_limit_select_option.

  TYPE-POOLS sscr. "Tipo da tela de selação
* Restringe os dados do parâmetro da tela de seleção
  DATA: tl_screen TYPE sscr_restrict. "Tabelda tela de seleção
* Estruturas para preencher a tab. t_screen
  DATA: el_opts  TYPE sscr_opt_list, "Estrutura da restrição da lista de opções
        el_assoc TYPE sscr_ass.      "Estrutura da lista do nome da variável restringida
  CONSTANTS: cl_objectkey1(10) TYPE c VALUE 'OBJECTKEY1'.

* Restringe o campo "Modificado em"  selection para somente EQ.
* Documento Contábil
  el_opts-name       = cl_objectkey1.
  el_opts-options-eq = sy-abcde+23(1). "X
  APPEND el_opts TO tl_screen-opt_list_tab.
  el_assoc-kind      = sy-abcde+18(1). "S
  el_assoc-name      = 'S_BELNR'.
*  el_assoc-sg_main   = sy-abcde+8(1).  "I
  el_assoc-sg_main   = '*'.
  el_assoc-sg_addy   = space.
  el_assoc-op_main   = cl_objectkey1.
  APPEND el_assoc TO tl_screen-ass_tab.
* Função para restringir Selection  Option
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = tl_screen
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.
* Verifica de função executou com erro.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SETTING_VERIFY_FIELD
*&---------------------------------------------------------------------*
*       Configuração e validação do respectivo campo acionado
*----------------------------------------------------------------------*
*    --> P_VALUE Valor do respectivo campo para validar o preenchimento
*----------------------------------------------------------------------*
FORM zf_setting_verify_field USING p_value.

  CONSTANTS: cl_onli TYPE char4 VALUE 'ONLI'.

  CHECK sy-ucomm         EQ cl_onli OR
        sscrfields-ucomm EQ cl_onli.

  IF p_value IS INITIAL.
    IF   s_budat-high IS INITIAL     AND
         s_budat-low  IS NOT INITIAL AND
       ( s_bukrs[]    IS NOT INITIAL OR
         s_bukrs      IS NOT INITIAL ).
      SET CURSOR FIELD 'S_BUDAT-HIGH'.

    ENDIF.
* Preencher todos os campos obrigatórios
    MESSAGE e055(00).

  ENDIF.

ENDFORM.
**<<<------"187070 - NMS - FIM------>>>
