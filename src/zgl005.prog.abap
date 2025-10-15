************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda.                *
* Data desenv ...: 06.08.2008                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Relatório para conferencia de lançamentos indevidos *
*                  nas contas com relevância para classificação de     *
*                  custos originadas em MM                             *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 06.08.2008    Marcus Barbara       Inicio               DEVK904634   *
* 14.07.2008    Marcus Barbara       Alteração            DEVK904684   *
* 20.07.2008    Marcus Barbara       Alteração            DEVK904708   *
* 20.07.2008    Marcus Barbara       Alteração            DEVK904716   *
* 04.03.2011    Victor Hugo          Alteração            DEVK914269   *
************************************************************************

REPORT  ZGL005  MESSAGE-ID Z01.
*----------------------------------------------------------------------*
* Tabelas                                                              *
*----------------------------------------------------------------------*
TABLES: T001, BSIS.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO, STREE.

*----------------------------------------------------------------------*
* Tabelas Internas (ALV)                                               *
*----------------------------------------------------------------------*
DATA: IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,                   "Estrutura de saida
      IT_EVENT    TYPE SLIS_T_EVENT       WITH HEADER LINE,   "Eventos
      IT_HEADER   TYPE KKBLO_T_LISTHEADER WITH HEADER LINE,   "Cabeçalho
      VG_LAYOUT   TYPE SLIS_LAYOUT_ALV,   "Layout do alv
      VG_UCOMM    TYPE STREE_UCOMM.

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
DATA: BEGIN OF WA_BSIS,
        HKONT LIKE BSIS-HKONT,
        BUKRS LIKE BSIS-BUKRS,
        GJAHR LIKE BSIS-GJAHR,
        BELNR LIKE BSIS-BELNR,
        BUZEI LIKE BSIS-BUZEI,
        GSBER LIKE BSIS-GSBER,
        KOSTL LIKE BSIS-KOSTL,
        BUDAT LIKE BSIS-BUDAT,
        AUFNR LIKE BSIS-AUFNR,
      END OF WA_BSIS.

DATA: BEGIN OF WA_BSEG,
        BUKRS LIKE BSEG-BUKRS,
        BELNR LIKE BSEG-BELNR,
        GJAHR LIKE BSEG-GJAHR,
        BUZEI LIKE BSEG-BUZEI,
        SHKZG LIKE BSEG-SHKZG,
        DMBTR LIKE BSEG-DMBTR,
        DMBE2 LIKE BSEG-DMBE2,
        EBELN LIKE BSEG-EBELN,
        LIFNR LIKE BSEG-LIFNR,
        EBELP LIKE BSEG-EBELP,
        MATNR LIKE BSEG-MATNR,
      END OF WA_BSEG.

DATA: BEGIN OF WA_EKPO,
        EBELN LIKE EKPO-EBELN,
        EBELP LIKE EKPO-EBELP,
        MATNR LIKE EKPO-MATNR,
        MATKL LIKE EKPO-MATKL,
        BANFN LIKE EKPO-BANFN,
        TXZ01 LIKE EKPO-TXZ01,
      END OF WA_EKPO.

DATA: BEGIN OF WA_EBAN,
        BANFN LIKE EBAN-BANFN,
        ERNAM LIKE EBAN-ERNAM,
      END OF WA_EBAN.

DATA: BEGIN OF WA_MAKT,
        MATNR LIKE MAKT-MATNR,
        MAKTX LIKE MAKT-MAKTX,
      END OF WA_MAKT.

DATA: BEGIN OF WA_LFA1,
        LIFNR LIKE LFA1-LIFNR,
        NAME1 LIKE LFA1-NAME1,
      END OF WA_LFA1.

DATA: BEGIN OF WA_CSKT,
        KOSTL LIKE CSKT-KOSTL,
        KTEXT LIKE CSKT-KTEXT,
      END OF WA_CSKT.

DATA: BEGIN OF WA_T001,
        BUKRS LIKE T001-BUKRS,
        BUTXT LIKE T001-BUTXT,
      END OF WA_T001.

DATA: BEGIN OF WA_T001W,
        WERKS LIKE T001W-WERKS,
        NAME1 LIKE T001W-NAME1,
      END OF WA_T001W.

DATA: BEGIN OF WA_SKAT,
        SAKNR LIKE SKAT-SAKNR,
        TXT20 LIKE SKAT-TXT20,
      END OF WA_SKAT.

DATA: BEGIN OF WA_T023T,
        MATKL   LIKE T023T-MATKL,
        WGBEZ60 LIKE T023T-WGBEZ60,
      END OF WA_T023T.

DATA: BEGIN OF WA_ALV_REQ,
        STATUS(20) TYPE C,     "Status
        BUKRS     LIKE BSIS-BUKRS,          "Empresa
        BUTXT     LIKE T001-BUTXT,          "Nome Empresa
        GSBER     LIKE BSIS-GSBER,          "Centro
        NAME1     LIKE T001W-NAME1,         "Nome Centro
        BUDAT     LIKE BSIS-BUDAT,          "Data Lcto
        BELNR     LIKE BSIS-BELNR,          "Doc.Contab.
        KOSTL     LIKE BSIS-KOSTL,          "Centro Custo
        AUFNR     LIKE BSIS-AUFNR,          "Ordem
        KTEXT     LIKE CSKT-KTEXT,          "Descrição
        HKONT     LIKE BSIS-HKONT,          "Conta
        TXT20     LIKE SKAT-TXT20,          "Descrição
        DMBTR     LIKE BSEG-DMBTR,          "Valor R$
        DMBE2     LIKE BSEG-DMBE2,          "Valor US$
        EBELN     LIKE BSEG-EBELN,          "Pedido
        BANFN     LIKE EBAN-BANFN,          "Requisição
        ERNAM     LIKE EBAN-ERNAM,          "Us.Req.
        MATNR     LIKE EKPO-MATNR,          "Material
        MAKTX     LIKE MAKT-MAKTX,          "Descrição
        TXZ01     LIKE EKPO-TXZ01,          "Texto
        MATKL     LIKE EKPO-MATKL,          "Gpo.Merc.
        WGBEZ60   LIKE T023T-WGBEZ60,       "Descrição
        LIFNR     LIKE BSEG-LIFNR,          "Fornecedor
        FORNE     LIKE LFA1-NAME1,          "Nome
        MATNR9(9),                        "Material com 9 dígitos
        HKONT6(6),                       "Conta com 6 dígitos
      END OF WA_ALV_REQ.

DATA: BEGIN OF WA_BKPF,
        BUKRS LIKE BKPF-BUKRS,
        BELNR LIKE BKPF-BELNR,
        GJAHR LIKE BKPF-GJAHR,
        AWKEY LIKE BKPF-AWKEY,
        AWK10 LIKE MSEG-MBLNR,
        MJAHR LIKE MSEG-MJAHR,
      END OF WA_BKPF.

DATA: BEGIN OF WA_MSEG,
        MBLNR LIKE MSEG-MBLNR,
        MJAHR LIKE MSEG-MJAHR,
        MATNR LIKE MSEG-MATNR,
        RSNUM LIKE MSEG-RSNUM,
      END OF WA_MSEG.

DATA: BEGIN OF WA_RESB,
        RSNUM LIKE RESB-RSNUM,
        MATKL LIKE RESB-MATKL,
      END OF WA_RESB.

DATA: BEGIN OF WA_RKPF,
        RSNUM LIKE RKPF-RSNUM,
        USNAM LIKE RKPF-USNAM,
      END OF WA_RKPF.

DATA: BEGIN OF WA_EKKO,
        EBELN LIKE EKKO-EBELN,
        LIFNR LIKE EKKO-LIFNR,
      END OF WA_EKKO.

DATA: BEGIN OF WA_ALV_RES,
        STATUS(20) TYPE C,                  "Status
        BUKRS     LIKE BSIS-BUKRS,          "Empresa
        BUTXT     LIKE T001-BUTXT,          "Nome Empresa
        GSBER     LIKE BSIS-GSBER,          "Centro
        NAME1     LIKE T001W-NAME1,         "Nome Centro
        BUDAT     LIKE BSIS-BUDAT,          "Data Lcto
        BELNR     LIKE BSIS-BELNR,          "Doc.Contab.
        KOSTL     LIKE BSIS-KOSTL,          "Centro Custo
        KTEXT     LIKE CSKT-KTEXT,          "Descrição
        HKONT     LIKE BSIS-HKONT,          "Conta
        TXT20     LIKE SKAT-TXT20,          "Descrição
        DMBTR     LIKE BSEG-DMBTR,          "Valor R$
        DMBE2     LIKE BSEG-DMBE2,          "Valor US$
        AWK10     LIKE MSEG-MBLNR,          "Doc. Material
        RSNUM     LIKE MSEG-RSNUM,          "Reserva
        USNAM     LIKE RKPF-USNAM,          "Us. Reserva
        MATNR     LIKE MSEG-MATNR,          "Material
        MAKTX     LIKE MAKT-MAKTX,          "Descrição
        MATKL     LIKE EKPO-MATKL,          "Gpo.Merc.
        WGBEZ60   LIKE T023T-WGBEZ60,       "Descrição
        MATNR9(9),                        "Material com 9 dígitos
        HKONT6(6),                        "Conta com 6 dígitos
      END OF WA_ALV_RES.

DATA: IT_BSIS  LIKE STANDARD TABLE OF WA_BSIS,
      IT_BSEG  LIKE STANDARD TABLE OF WA_BSEG,
      IT_MSEG  LIKE STANDARD TABLE OF WA_MSEG,
      IT_EKPO  LIKE STANDARD TABLE OF WA_EKPO,
      IT_EBAN  LIKE STANDARD TABLE OF WA_EBAN,
      IT_MAKT  LIKE STANDARD TABLE OF WA_MAKT,
      IT_LFA1  LIKE STANDARD TABLE OF WA_LFA1,
      IT_CSKT  LIKE STANDARD TABLE OF WA_CSKT,
      IT_T001  LIKE STANDARD TABLE OF WA_T001,
      IT_T001W LIKE STANDARD TABLE OF WA_T001W,
      IT_SKAT  LIKE STANDARD TABLE OF WA_SKAT,
      IT_T023T LIKE STANDARD TABLE OF WA_T023T,
      IT_BKPF  LIKE STANDARD TABLE OF WA_BKPF,
      IT_BKPF2 LIKE STANDARD TABLE OF WA_BKPF,
      IT_RESB  LIKE STANDARD TABLE OF WA_RESB,
      IT_RKPF  LIKE STANDARD TABLE OF WA_RKPF,
      IT_EKKO  LIKE STANDARD TABLE OF WA_EKKO.

DATA : IT_ALV_REQ LIKE STANDARD TABLE OF WA_ALV_REQ,
       IT_ALV_RES LIKE STANDARD TABLE OF WA_ALV_RES.

DATA: E_EVENT  TYPE  SLIS_ALV_EVENT.



*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*
INCLUDE ZGL005_CONSULTAS.

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  SET TITLEBAR 'TITULO'.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*

  SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S02.
  PARAMETER: R_REQ       RADIOBUTTON GROUP TP DEFAULT 'X',
             R_RES       RADIOBUTTON GROUP TP.
  SELECTION-SCREEN END   OF BLOCK B1.

  SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S01.

  SELECT-OPTIONS:
              PA_BUKRS      FOR T001-BUKRS OBLIGATORY. "Empresa"

  PARAMETERS:
    PB_WERKS LIKE T001W-WERKS,           "Centro"
    PD_KOSTL LIKE CSKSZ-KOSTL.           "Centro de custo"

  SELECT-OPTIONS:
              PC_HKONT      FOR BSIS-HKONT DEFAULT '0000412000' TO '0000422999' OBLIGATORY, "Conta do Razão da contabilidade geral"
              PE_BUDAT      FOR BSIS-BUDAT OBLIGATORY.  "Data de lançamento no documento"
  SELECTION-SCREEN END   OF BLOCK B2.

*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF PA_BUKRS IS INITIAL.
    LEAVE TO SCREEN 1000.
  ENDIF.

  IF PC_HKONT-HIGH IS INITIAL.
    MESSAGE I000 WITH 'Deve ser informado um intervalo de conta razão!'.
    LEAVE TO SCREEN 1000.
  ENDIF.

  IF PE_BUDAT-HIGH IS INITIAL.
    MESSAGE I000 WITH 'Deve ser informado um período de lançamento!'.
    LEAVE TO SCREEN 1000.
  ENDIF.

  DATA: P_DATA1(6),
        P_DATA2(6).

  CONCATENATE PE_BUDAT-LOW(4) PE_BUDAT-LOW+4(2) INTO P_DATA1.
  CONCATENATE PE_BUDAT-HIGH(4) PE_BUDAT-HIGH+4(2) INTO P_DATA2.

  "IF p_data1 NE p_data2.
  " MESSAGE i000 WITH 'Deve ser informado um período dentro de um mês!'.
  " LEAVE TO SCREEN 1000.
  " ENDIF.

*----------------------------------------------------------------------*
* Event Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF R_REQ IS NOT INITIAL.
    PERFORM F_SELECIONA_DADOS USING 'WE' 'RE'.
  ELSE.
    PERFORM F_SELECIONA_DADOS USING 'WA' ' '.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona dados de cabeçalho dos lançamentos
*----------------------------------------------------------------------*
*      -->P_0134   text
*      -->P_0135   text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS  USING P_0134 TYPE C
                              P_0135 TYPE C.

  DATA: DATAINI(10) TYPE C.
  DATA: DATAFIM(10) TYPE C.

  DATAINI = |{ PE_BUDAT-LOW+6(2) }.{ PE_BUDAT-LOW+4(2) }.{ PE_BUDAT-LOW+0(4) }|.
  DATAFIM = |{ PE_BUDAT-HIGH+6(2) }.{ PE_BUDAT-HIGH+4(2) }.{ PE_BUDAT-HIGH+0(4) }|.


  PERFORM F_MENSAGEM USING 'Contabilidade financ.: índice secundário p/contas do Razão'.

  IF P_0135 NE ' '.
    "Se for Empresa + Conta + Data Lançamento"
    IF ( PC_HKONT IS NOT INITIAL ) AND ( PE_BUDAT IS NOT INITIAL )
       AND ( PB_WERKS IS INITIAL ) AND ( PD_KOSTL IS INITIAL ).
      SELECT HKONT BUKRS GJAHR BELNR BUZEI GSBER KOSTL BUDAT AUFNR
        FROM BSIS
        INTO TABLE IT_BSIS
       WHERE  BUKRS IN PA_BUKRS
          AND ( BLART EQ P_0134 OR BLART EQ P_0135 )
          "AND HKONT BETWEEN PC_HKONT-LOW AND PC_HKONT-HIGH
          AND HKONT IN PC_HKONT
          AND BUDAT IN PE_BUDAT.
      "AND BUDAT BETWEEN DATAINI AND DATAFIM.
      "Se for Empresa + Conta + Data Lançamento + Centro"
    ELSEIF ( PC_HKONT IS NOT INITIAL ) AND ( PE_BUDAT IS NOT INITIAL )
       AND ( PB_WERKS IS NOT INITIAL ) AND ( PD_KOSTL IS INITIAL ).
      SELECT HKONT BUKRS GJAHR BELNR BUZEI GSBER KOSTL BUDAT AUFNR
        FROM BSIS
        INTO TABLE IT_BSIS
       WHERE  BUKRS IN PA_BUKRS
          AND WERKS EQ PB_WERKS
          AND ( BLART EQ P_0134 OR BLART EQ P_0135 )
          AND HKONT IN PC_HKONT
          AND BUDAT IN PE_BUDAT.
      "Se for Empresa + Conta + Data Lançamento + Centro Custo"
    ELSEIF ( PC_HKONT IS NOT INITIAL ) AND ( PE_BUDAT IS NOT INITIAL )
       AND ( PB_WERKS IS INITIAL ) AND ( PD_KOSTL IS NOT INITIAL ).
      SELECT HKONT BUKRS GJAHR BELNR BUZEI GSBER KOSTL BUDAT AUFNR
        FROM BSIS
        INTO TABLE IT_BSIS
       WHERE BUKRS IN PA_BUKRS
          AND KOSTL EQ PD_KOSTL
          AND ( BLART EQ P_0134 OR BLART EQ P_0135 )
          AND HKONT IN PC_HKONT
          AND BUDAT IN PE_BUDAT.
      "Se for Empresa + Conta + Data Lançamento + Centro + Centro de Custo"
    ELSEIF ( PC_HKONT IS NOT INITIAL ) AND ( PE_BUDAT IS NOT INITIAL )
       AND ( PB_WERKS IS NOT INITIAL ) AND ( PD_KOSTL IS NOT INITIAL ).
      SELECT HKONT BUKRS GJAHR BELNR BUZEI GSBER KOSTL BUDAT AUFNR
        FROM BSIS
        INTO TABLE IT_BSIS
       WHERE BUKRS IN PA_BUKRS
          AND WERKS EQ PB_WERKS
          AND KOSTL EQ PD_KOSTL
          AND ( BLART EQ P_0134 OR BLART EQ P_0135 )
          AND HKONT IN PC_HKONT
          AND BUDAT IN PE_BUDAT.
    ENDIF.
  ELSE.
    "Se for Empresa + Conta + Data Lançamento"
    IF ( PC_HKONT IS NOT INITIAL ) AND ( PE_BUDAT IS NOT INITIAL )
       AND ( PB_WERKS IS INITIAL ) AND ( PD_KOSTL IS INITIAL ).
      SELECT HKONT BUKRS GJAHR BELNR BUZEI GSBER KOSTL BUDAT AUFNR
        FROM BSIS
        INTO TABLE IT_BSIS
       WHERE BUKRS IN PA_BUKRS
         AND BLART EQ P_0134
          AND HKONT IN PC_HKONT
          AND BUDAT IN PE_BUDAT.
      "Se for Empresa + Conta + Data Lançamento + Centro"
    ELSEIF ( PC_HKONT IS NOT INITIAL ) AND ( PE_BUDAT IS NOT INITIAL )
       AND ( PB_WERKS IS NOT INITIAL ) AND ( PD_KOSTL IS INITIAL ).
      SELECT HKONT BUKRS GJAHR BELNR BUZEI GSBER KOSTL BUDAT AUFNR
        FROM BSIS
        INTO TABLE IT_BSIS
       WHERE BUKRS IN PA_BUKRS
         AND WERKS EQ PB_WERKS
         AND BLART EQ P_0134
          AND HKONT IN PC_HKONT
          AND BUDAT IN PE_BUDAT.
      "Se for Empresa + Conta + Data Lançamento + Centro Custo"
    ELSEIF ( PC_HKONT IS NOT INITIAL ) AND ( PE_BUDAT IS NOT INITIAL )
       AND ( PB_WERKS IS INITIAL ) AND ( PD_KOSTL IS NOT INITIAL ).
      SELECT HKONT BUKRS GJAHR BELNR BUZEI GSBER KOSTL BUDAT AUFNR
        FROM BSIS
        INTO TABLE IT_BSIS
       WHERE BUKRS IN PA_BUKRS
         AND BLART EQ P_0134
         AND KOSTL EQ PD_KOSTL
          AND HKONT IN PC_HKONT
          AND BUDAT IN PE_BUDAT.
      "Se for Empresa + Conta + Data Lançamento + Centro + Centro de Custo"
    ELSEIF ( PC_HKONT IS NOT INITIAL ) AND ( PE_BUDAT IS NOT INITIAL )
       AND ( PB_WERKS IS NOT INITIAL ) AND ( PD_KOSTL IS NOT INITIAL ).
      SELECT HKONT BUKRS GJAHR BELNR BUZEI GSBER KOSTL BUDAT AUFNR
        FROM BSIS
        INTO TABLE IT_BSIS
       WHERE BUKRS IN PA_BUKRS
         AND WERKS EQ PB_WERKS
         AND KOSTL EQ PD_KOSTL
         AND BLART EQ P_0134
          AND HKONT IN PC_HKONT
          AND BUDAT IN PE_BUDAT.
    ENDIF.
  ENDIF.

  SORT IT_BSIS BY HKONT BUKRS GJAHR BELNR BUZEI GSBER KOSTL BUDAT.

*  IF it_bsis IS NOT INITIAL.
  IF P_0135 NE ' '.
    PERFORM F_CONSULTA_REQUISICAO.
  ELSE.
    PERFORM F_CONSULTA_RESERVA.
  ENDIF.
*  ELSE.
*    MESSAGE 'Não existem índices secundários p/contas do Razão para este filtro.' TYPE 'I'.
*    EXIT.
*  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_requisicao
*&---------------------------------------------------------------------*
*       Gera consulta de requisicao para ALV                           *
*----------------------------------------------------------------------*
*      -->P_IT_BSIS  text
*----------------------------------------------------------------------*
FORM F_CONSULTA_REQUISICAO.
  PERFORM F_CONSULTA_BSEG.
  PERFORM F_CONSULTA_EKKO.
  PERFORM F_CONSULTA_EKPO.
  PERFORM F_CONSULTA_EBAN.
  PERFORM F_CONSULTA_MAKT_EKPO.
  PERFORM F_CONSULTA_LFA1.
  PERFORM F_CONSULTA_CSKT.
  PERFORM F_CONSULTA_T001.
  PERFORM F_CONSULTA_T001W.
  PERFORM F_CONSULTA_SKAT.
  PERFORM F_CONSULTA_T023T_EKPO.
  PERFORM GERA_TABELA_INTERNA_REQUISICAO.
ENDFORM.                    " F_CONSULTA

*&---------------------------------------------------------------------*
*&      Form  GERA_TABELA_INTERNA_requisicao
*&---------------------------------------------------------------------*
*       Gerar tabela interna com informações de analise da plano de    *
*       classe de custo                                                *
*----------------------------------------------------------------------*
FORM GERA_TABELA_INTERNA_REQUISICAO .

  PERFORM F_MENSAGEM USING 'Gerando Analise de Classe de Custo - Requisição'.

  LOOP AT IT_BSIS INTO WA_BSIS.

    CLEAR: WA_ALV_REQ.

    WA_ALV_REQ-BUKRS = WA_BSIS-BUKRS.
    WA_ALV_REQ-GSBER = WA_BSIS-GSBER.
    WA_ALV_REQ-BUDAT = WA_BSIS-BUDAT.
    WA_ALV_REQ-BELNR = WA_BSIS-BELNR.
    WA_ALV_REQ-KOSTL = WA_BSIS-KOSTL.
    WA_ALV_REQ-HKONT = WA_BSIS-HKONT.
    WA_ALV_REQ-HKONT6 = WA_BSIS-HKONT+4(6).
    WA_ALV_REQ-AUFNR = WA_BSIS-AUFNR.

    READ TABLE IT_T001 INTO WA_T001 WITH KEY BUKRS = WA_BSIS-BUKRS BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_ALV_REQ-BUTXT = WA_T001-BUTXT.
    ELSE.
      WA_ALV_REQ-BUTXT = ''.
    ENDIF.

    READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_BSIS-GSBER BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_ALV_REQ-NAME1 = WA_T001W-NAME1.
    ELSE.
      WA_ALV_REQ-NAME1 = ''.
    ENDIF.

    READ TABLE IT_CSKT INTO WA_CSKT WITH KEY KOSTL = WA_BSIS-KOSTL BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_ALV_REQ-KTEXT = WA_CSKT-KTEXT.
    ELSE.
      WA_ALV_REQ-KTEXT = ''.
    ENDIF.

    READ TABLE IT_SKAT INTO WA_SKAT WITH KEY SAKNR = WA_BSIS-HKONT BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_ALV_REQ-TXT20 = WA_SKAT-TXT20.
    ELSE.
      WA_ALV_REQ-TXT20 = ''.
    ENDIF.

    LOOP AT IT_BSEG INTO WA_BSEG WHERE BUKRS EQ WA_BSIS-BUKRS
                                   AND BELNR EQ WA_BSIS-BELNR
                                   AND GJAHR EQ WA_BSIS-GJAHR
                                   AND BUZEI EQ WA_BSIS-BUZEI.

      CLEAR: WA_ALV_REQ-EBELN,
             WA_ALV_REQ-LIFNR,
             WA_ALV_REQ-DMBTR,
             WA_ALV_REQ-DMBE2,
             WA_ALV_REQ-FORNE,
             WA_ALV_REQ-MATNR,
             WA_ALV_REQ-TXZ01,
             WA_ALV_REQ-MATKL,
             WA_ALV_REQ-BANFN,
             WA_ALV_REQ-ERNAM,
             WA_ALV_REQ-MAKTX,
             WA_ALV_REQ-WGBEZ60,
             WA_ALV_REQ-MATNR9.

      WA_ALV_REQ-EBELN = WA_BSEG-EBELN.

      IF WA_BSEG-SHKZG = 'H'.
        WA_ALV_REQ-DMBTR = WA_BSEG-DMBTR * -1.
        WA_ALV_REQ-DMBE2 = WA_BSEG-DMBE2 * -1.
      ELSE.
        WA_ALV_REQ-DMBTR = WA_BSEG-DMBTR.
        WA_ALV_REQ-DMBE2 = WA_BSEG-DMBE2.
      ENDIF.

      READ TABLE IT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_BSEG-EBELN BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        WA_ALV_REQ-LIFNR = WA_EKKO-LIFNR.

        READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_ALV_REQ-LIFNR BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          WA_ALV_REQ-FORNE = WA_LFA1-NAME1.
        ENDIF.
      ENDIF.

      LOOP AT IT_EKPO INTO WA_EKPO WHERE EBELN EQ WA_BSEG-EBELN
                                     AND EBELP EQ WA_BSEG-EBELP.

        CLEAR: WA_ALV_REQ-TXZ01,
               WA_ALV_REQ-MATKL,
               WA_ALV_REQ-BANFN,
               WA_ALV_REQ-ERNAM,
               WA_ALV_REQ-MAKTX,
               WA_ALV_REQ-WGBEZ60,
               WA_ALV_REQ-MATNR,
               WA_ALV_REQ-MATNR9.

        WA_ALV_REQ-MATNR = WA_EKPO-MATNR.
        WA_ALV_REQ-MATNR9 = WA_EKPO-MATNR+9(9).
        WA_ALV_REQ-TXZ01 = WA_EKPO-TXZ01.
        WA_ALV_REQ-MATKL = WA_EKPO-MATKL.

        READ TABLE IT_EBAN INTO WA_EBAN WITH KEY BANFN = WA_EKPO-BANFN BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          WA_ALV_REQ-BANFN = WA_EBAN-BANFN.
          WA_ALV_REQ-ERNAM = WA_EBAN-ERNAM.
        ENDIF.

        READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO-MATNR BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          WA_ALV_REQ-MAKTX = WA_MAKT-MAKTX.
        ENDIF.

        READ TABLE IT_T023T INTO WA_T023T WITH KEY MATKL = WA_EKPO-MATKL BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          WA_ALV_REQ-WGBEZ60 = WA_T023T-WGBEZ60.
        ENDIF.



        SELECT SINGLE STATUS
        FROM ZMMT0091  INTO WA_ALV_REQ-STATUS
         WHERE  BUKRS EQ WA_ALV_REQ-BUKRS
          AND   BELNR EQ WA_ALV_REQ-BELNR.

        IF WA_ALV_REQ-STATUS IS NOT INITIAL.
          WA_ALV_REQ-STATUS = ICON_CHECKED.
        ELSE.
          WA_ALV_REQ-STATUS = ICON_GENERATE.
        ENDIF.

        APPEND WA_ALV_REQ TO IT_ALV_REQ.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  PERFORM F_ALV_ESTRUTURA_REQ .

ENDFORM.                    " GERA_TABELA_INTERNA

*&---------------------------------------------------------------------*
*&      Form  F_ALV_ESTRUTURA_REQ
*&---------------------------------------------------------------------*
*       ALV de apresentação da análise de classe de custo              *
*----------------------------------------------------------------------*
FORM F_ALV_ESTRUTURA_REQ .
* Montar estruduta de ALV
  PERFORM F_ALV_EST.
* Monta cabeçalho
  PERFORM F_MONTA_CABECALHO USING 'RQ'.
* Executa ALV
  PERFORM F_ALV_EXECUTA.
ENDFORM.                    " F_ALV_ESTRUTURA_REQ

*&---------------------------------------------------------------------*
*&      Form  F_ALV_EST
*&---------------------------------------------------------------------*
*       Montagem da estrutura do ALV                                   *
*----------------------------------------------------------------------*
FORM F_ALV_EST .
  PERFORM F_FIELDCAT USING:
       '00' '' 'IT_ALV_RQ'  'STATUS' 'Apontar'
       05   '' ''            '' 'X'  CHANGING IT_FIELDCAT,
       '01' '' 'IT_ALV_REQ' 'BUKRS' 'Empresa'
       07  ''  ''             '' ''  CHANGING IT_FIELDCAT,
       '02' '' 'IT_ALV_REQ' 'BUTXT' 'Nome Empresa'
       20  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '03' '' 'IT_ALV_REQ' 'GSBER' 'Centro'
       07  ''  ''             '' ''  CHANGING IT_FIELDCAT,
       '04' '' 'IT_ALV_REQ' 'NAME1' 'Nome Centro'
       20  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '05' '' 'IT_ALV_REQ' 'BUDAT' 'Data Lcto'
       10  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '06' '' 'IT_ALV_REQ' 'BELNR' 'Doc.Contab.'
       10  ''  ''             '' 'X' CHANGING IT_FIELDCAT,
       '07' '' 'IT_ALV_REQ' 'KOSTL' 'Centro Custo'
       10  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '08' '' 'IT_ALV_REQ' 'AUFNR' 'Ordem'
       10  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '09' '' 'IT_ALV_REQ' 'KTEXT' 'Descrição'
       20  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '10' '' 'IT_ALV_REQ' 'HKONT6' 'Conta'
       10  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '11' '' 'IT_ALV_REQ' 'TXT20' 'Descrição'
       20  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '12' '' 'IT_ALV_REQ' 'DMBTR' 'Valor R$'
       15  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '13' '' 'IT_ALV_REQ' 'DMBE2' 'Valor US$'
       15  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '14' '' 'IT_ALV_REQ' 'EBELN' 'Pedido'
       10  ''  ''             '' 'X' CHANGING IT_FIELDCAT,
       '15' '' 'IT_ALV_REQ' 'BANFN' 'Requisição'
       10  ''  ''             '' 'X' CHANGING IT_FIELDCAT,
       '16' '' 'IT_ALV_REQ' 'ERNAM' 'Us.Req.'
       10  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '17' '' 'IT_ALV_REQ' 'MATNR9' 'Material'
       10  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '18' '' 'IT_ALV_REQ' 'MAKTX' 'Descrição'
       20  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '19' '' 'IT_ALV_REQ' 'TXZ01' 'Texto'
       30  ''  ''             '' '' CHANGING IT_FIELDCAT,
       '20' '' 'IT_ALV_REQ' 'MATKL' 'Gpo.Merc.'
        8  ''  ''             '' ''  CHANGING IT_FIELDCAT,
       '21' '' 'IT_ALV_REQ' 'WGBEZ60' 'Descrição'
       20  ''  ''             '' ''  CHANGING IT_FIELDCAT,
       '22' '' 'IT_ALV_REQ' 'LIFNR' 'Fornecedor'
       10  ''  ''             '' ''  CHANGING IT_FIELDCAT,
       '23' '' 'IT_ALV_REQ' 'FORNE' 'Nome'
       20  ''  ''             '' ''  CHANGING IT_FIELDCAT.
ENDFORM.                    " F_ALV_EST

*&---------------------------------------------------------------------*                                                                      *
*&      Form  f_fieldcat                                               *
*&---------------------------------------------------------------------*
* Preenche a tabela fieldcat                                           *
*----------------------------------------------------------------------*
* p_cont   -> Posição do campo                                         *
* p_key    -> campo chave                                              *
* p_tab    -> tabela interna                                           *
* p_field  -> campo da tabela interna                                  *
* p_desc   -> Descrição do campo                                       *
* p_tam    -> Tamanho do campo de saída                                *
* p_qtde   -> É um campo de to tipo QUAN                               *
* p_fix    -> Congelar a coluna                                        *
* p_just-> -> Alinhamento (R)ight (L)eft (C)ent                        *
*----------------------------------------------------------------------*
FORM F_FIELDCAT USING P_CONT P_KEY  P_TAB  P_FIELD P_DESC
                      P_TAM  P_QTDE P_FIX  P_JUST P_HOT
             CHANGING P_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

* Tabela interna local
  DATA: TL_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

  TL_FIELDCAT-COL_POS    = P_CONT. "Posição
  TL_FIELDCAT-KEY        = P_KEY.  "
  TL_FIELDCAT-TABNAME    = P_TAB.  "Tabela interna
  TL_FIELDCAT-FIELDNAME  = P_FIELD."Campo
  TL_FIELDCAT-SELTEXT_L  = P_DESC. "Descrição longa
  TL_FIELDCAT-SELTEXT_M  = P_DESC. "Descrição media
  TL_FIELDCAT-SELTEXT_S  = P_DESC. "Descrição pequena
  TL_FIELDCAT-OUTPUTLEN  = P_TAM.  "Tamanho
  TL_FIELDCAT-QUANTITY   = P_QTDE. "Campo quantidade
  TL_FIELDCAT-FIX_COLUMN = P_FIX.  "Fixar coluna
  TL_FIELDCAT-JUST       = P_JUST. "Alinhar
  TL_FIELDCAT-HOTSPOT    = P_HOT.  "Clique chama evento
  APPEND TL_FIELDCAT TO P_FIELDCAT.

ENDFORM.                    " f_fieldcatJ1BNFDOC
*&---------------------------------------------------------------------*
*&      Form  F_ALV_EXECUTA
*&---------------------------------------------------------------------*
*       Executa a ALV com as informações da it_avl                     *
*----------------------------------------------------------------------*
FORM F_ALV_EXECUTA .

* Variavel Local
  DATA: VL_REPID LIKE SY-REPID.
  DATA: YT_EVENT       TYPE  SLIS_T_EVENT.

*  E_EVENT-NAME =  SLIS_EV_DATA_CHANGED.
*  E_EVENT-FORM =  'DATA_CHANGED_FINISHED'.
*
*  APPEND E_EVENT TO IT_EVENT[].


  VL_REPID = SY-REPID.

  IT_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  IT_EVENT-FORM = SLIS_EV_TOP_OF_PAGE.
  APPEND IT_EVENT.

* Determinar a tabela de cores
  VG_LAYOUT-ZEBRA               = 'X'.

* Função para exibir o ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = VL_REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IS_LAYOUT                = VG_LAYOUT
      IT_FIELDCAT              = IT_FIELDCAT[]
      I_DEFAULT                = 'A'
      I_SAVE                   = 'A'
      IT_EVENTS                = IT_EVENT[]
    TABLES
      T_OUTTAB                 = IT_ALV_REQ
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_ALV_EXECUTA


*&--------------------------------------------------------------------*
*&      Form  set_pf_status
*&--------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'PF_STATUS_ALV'.
ENDFORM.                    "set_pf_status

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Ao clicar na estrutura vai listar todos os niveis
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING P_UCOMM LIKE SY-UCOMM
      P_FIELD TYPE SLIS_SELFIELD.


  DATA: T_ZMMT0091 TYPE TABLE OF ZMMT0091.
  DATA: W_ZMMT0091 TYPE ZMMT0091.


  IF P_FIELD-FIELDNAME = 'STATUS'.
     READ TABLE IT_ALV_REQ  INTO WA_ALV_REQ INDEX P_FIELD-TABINDEX.

     IF WA_ALV_REQ-STATUS = ICON_CHECKED.
        WA_ALV_REQ-STATUS = ICON_GENERATE.
     ELSE.
        WA_ALV_REQ-STATUS = ICON_CHECKED.
      ENDIF.
      MODIFY IT_ALV_REQ FROM WA_ALV_REQ INDEX  P_FIELD-TABINDEX.

     IF WA_ALV_REQ-STATUS = ICON_GENERATE.
        W_ZMMT0091-BUKRS  = WA_ALV_REQ-BUKRS.
        W_ZMMT0091-BELNR  = WA_ALV_REQ-BELNR.

        DELETE  ZMMT0091  FROM W_ZMMT0091.

     ELSE.
        W_ZMMT0091-BUKRS  = WA_ALV_REQ-BUKRS.
        W_ZMMT0091-BELNR  = WA_ALV_REQ-BELNR.
        W_ZMMT0091-STATUS = 'X'.
        MODIFY ZMMT0091 FROM W_ZMMT0091.
     ENDIF.

    MESSAGE TEXT-001 TYPE 'S'.
    PERFORM F_ALV_EXECUTA.
  ENDIF.



 " IF P_UCOMM = 'SALVAR'.
*    IF R_REQ IS NOT INITIAL.
*      LOOP AT IT_ALV_REQ INTO WA_ALV_REQ.
*        W_ZMMT0091-BUKRS  = WA_ALV_REQ-BUKRS.
*        W_ZMMT0091-BELNR  = WA_ALV_REQ-BELNR.
*        W_ZMMT0091-STATUS = 'X'.
*        MODIFY ZMMT0091 FROM W_ZMMT0091.
*      ENDLOOP.
*    ELSE.
*      LOOP AT IT_ALV_RES INTO WA_ALV_RES.
*        W_ZMMT0091-BUKRS  = WA_ALV_RES-BUKRS.
*        W_ZMMT0091-BELNR  = WA_ALV_RES-BELNR.
*        W_ZMMT0091-STATUS = 'X'
*        MODIFY ZMMT0091 FROM W_ZMMT0091.
*      ENDLOOP.
*    ENDIF.
*    MESSAGE TEXT-001 TYPE 'S'.
 " ENDIF.

  IF P_FIELD-FIELDNAME = 'BELNR'.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        TCODE  = 'FB03'
      EXCEPTIONS
        OK     = 1
        NOT_OK = 2.
    IF SY-SUBRC = 2.
      MESSAGE E077(S#) WITH 'FB03'.
    ENDIF.

    READ TABLE IT_ALV_REQ INTO WA_ALV_REQ INDEX P_FIELD-TABINDEX.

    IF WA_ALV_REQ-BELNR IS NOT INITIAL
      AND WA_ALV_REQ-BUKRS IS NOT INITIAL
      AND WA_ALV_REQ-BUDAT IS NOT INITIAL.

      SET PARAMETER ID: 'BLN' FIELD WA_ALV_REQ-BELNR,
                        'BUK' FIELD WA_ALV_REQ-BUKRS,
                        'GJR' FIELD WA_ALV_REQ-BUDAT(4).
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.

  IF P_FIELD-FIELDNAME = 'EBELN'.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        TCODE  = 'ME23N'
      EXCEPTIONS
        OK     = 1
        NOT_OK = 2.
    IF SY-SUBRC = 2.
      MESSAGE E077(S#) WITH 'ME23N'.
    ENDIF.

    READ TABLE IT_ALV_REQ INTO WA_ALV_REQ INDEX P_FIELD-TABINDEX.

    IF WA_ALV_REQ-EBELN IS NOT INITIAL.
      SET PARAMETER ID 'BES' FIELD WA_ALV_REQ-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.
  IF P_FIELD-FIELDNAME = 'BANFN'.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        TCODE  = 'ME53N'
      EXCEPTIONS
        OK     = 1
        NOT_OK = 2.
    IF SY-SUBRC = 2.
      MESSAGE E077(S#) WITH 'ME53N'.
    ENDIF.

    READ TABLE IT_ALV_REQ INTO WA_ALV_REQ INDEX P_FIELD-TABINDEX.
    IF WA_ALV_REQ-BANFN IS NOT INITIAL.
      SET PARAMETER ID 'BAN' FIELD WA_ALV_REQ-BANFN.
      CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.



ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM
*&---------------------------------------------------------------------*
*       Mensagem de acompanhamento de processo
*----------------------------------------------------------------------*
FORM F_MENSAGEM  USING P_MSG.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = P_MSG.
ENDFORM.                    " F_MENSAGEM

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_RESERVA
*&---------------------------------------------------------------------*
*       Gera consulta de reserva para ALV                              *
*----------------------------------------------------------------------*
FORM F_CONSULTA_RESERVA .
  PERFORM F_CONSULTA_BSEG.
  PERFORM F_CONSULTA_BKPF.
  PERFORM F_CONSULTA_MSEG.
  PERFORM F_CONSULTA_RESB.
  PERFORM F_CONSULTA_RKPF.
  PERFORM F_CONSULTA_MAKT_MSEG.
  PERFORM F_CONSULTA_CSKT.
  PERFORM F_CONSULTA_T001.
  PERFORM F_CONSULTA_T001W.
  PERFORM F_CONSULTA_SKAT.
  PERFORM F_CONSULTA_T023T_RESB.
  PERFORM GERA_TABELA_INTERNA_RESERVA.
ENDFORM.                    " F_CONSULTA_RESERVA

*&---------------------------------------------------------------------*
*&      Form  GERA_TABELA_INTERNA_RESERVA
*&---------------------------------------------------------------------*
*       Gerandor de Analise de Classe de Custo - Reserva
*----------------------------------------------------------------------*
FORM GERA_TABELA_INTERNA_RESERVA .

  PERFORM F_MENSAGEM USING 'Gerando Analise de Classe de Custo - Reserva'.

  LOOP AT IT_BSIS INTO WA_BSIS.

    CLEAR: WA_ALV_RES.
    WA_ALV_RES-BUKRS = WA_BSIS-BUKRS.
    WA_ALV_RES-GSBER = WA_BSIS-GSBER.
    WA_ALV_RES-BUDAT = WA_BSIS-BUDAT.
    WA_ALV_RES-BELNR = WA_BSIS-BELNR.
    WA_ALV_RES-KOSTL = WA_BSIS-KOSTL.
    WA_ALV_RES-HKONT = WA_BSIS-HKONT.
    WA_ALV_RES-HKONT6 = WA_BSIS-HKONT+4(6).

    READ TABLE IT_T001 INTO WA_T001 WITH KEY BUKRS = WA_BSIS-BUKRS BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_ALV_RES-BUTXT = WA_T001-BUTXT.
    ENDIF.

    READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_BSIS-GSBER BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_ALV_RES-NAME1 = WA_T001W-NAME1.
    ENDIF.

    READ TABLE IT_CSKT INTO WA_CSKT WITH KEY KOSTL = WA_BSIS-KOSTL BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_ALV_RES-KTEXT = WA_CSKT-KTEXT.
    ENDIF.

    READ TABLE IT_SKAT INTO WA_SKAT WITH KEY SAKNR = WA_BSIS-HKONT BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_ALV_RES-TXT20 = WA_SKAT-TXT20.
    ENDIF.

    LOOP AT IT_BSEG INTO WA_BSEG WHERE BUKRS EQ WA_BSIS-BUKRS
                                       AND BELNR EQ WA_BSIS-BELNR
                                       AND GJAHR EQ WA_BSIS-GJAHR
                                       AND BUZEI EQ WA_BSIS-BUZEI.

      CLEAR: WA_ALV_RES-DMBTR,
             WA_ALV_RES-DMBE2,
             WA_ALV_RES-MATNR,
             WA_ALV_RES-MATNR9,
             WA_ALV_RES-MATKL,
             WA_ALV_RES-MAKTX,
             WA_ALV_RES-RSNUM,
             WA_ALV_RES-AWK10,
             WA_ALV_RES-USNAM,
             WA_ALV_RES-WGBEZ60.

      IF WA_BSEG-SHKZG = 'H'.
        WA_ALV_RES-DMBTR = WA_BSEG-DMBTR * -1.
        WA_ALV_RES-DMBE2 = WA_BSEG-DMBE2 * -1.
      ELSE.
        WA_ALV_RES-DMBTR = WA_BSEG-DMBTR.
        WA_ALV_RES-DMBE2 = WA_BSEG-DMBE2.
      ENDIF.

      READ TABLE IT_BKPF INTO WA_BKPF WITH KEY BELNR = WA_BSIS-BELNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        WA_ALV_RES-AWK10 = WA_BKPF-AWK10.

        READ TABLE IT_MSEG INTO WA_MSEG WITH KEY MBLNR = WA_BKPF-AWK10
                                                 MJAHR = WA_BKPF-MJAHR
                                                 MATNR = WA_BSEG-MATNR BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          WA_ALV_RES-MATNR = WA_MSEG-MATNR.
          WA_ALV_RES-MATNR9 = WA_MSEG-MATNR+9(9).
          WA_ALV_RES-RSNUM = WA_MSEG-RSNUM.

          READ TABLE IT_RKPF INTO WA_RKPF WITH KEY RSNUM = WA_MSEG-RSNUM BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            WA_ALV_RES-USNAM = WA_RKPF-USNAM.
          ENDIF.

          READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_MSEG-MATNR BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            WA_ALV_RES-MAKTX = WA_MAKT-MAKTX.
          ENDIF.

          READ TABLE IT_RESB INTO WA_RESB WITH KEY RSNUM = WA_MSEG-RSNUM BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            WA_ALV_RES-MATKL = WA_RESB-MATKL.

            READ TABLE IT_T023T INTO WA_T023T WITH KEY MATKL = WA_RESB-MATKL BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              WA_ALV_RES-WGBEZ60 = WA_T023T-WGBEZ60.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

      SELECT SINGLE STATUS
        FROM ZMMT0091 INTO WA_ALV_RES-STATUS
        WHERE BUKRS EQ WA_ALV_RES-BUKRS
        AND   BELNR EQ  WA_ALV_RES-BELNR.


        IF WA_ALV_REQ-STATUS IS NOT INITIAL.
          WA_ALV_REQ-STATUS = ICON_CHECKED.
        ELSE.
          WA_ALV_REQ-STATUS = ICON_GENERATE.
        ENDIF.


      APPEND WA_ALV_RES TO IT_ALV_RES.

    ENDLOOP.

  ENDLOOP.

  PERFORM F_ALV_ESTRUTURA_RES .

ENDFORM.                    " GERA_TABELA_INTERNA_RESERVA
*&---------------------------------------------------------------------*
*&      Form  F_ALV_ESTRUTURA_RES
*&---------------------------------------------------------------------*
*       ALV de apresentação da análise de classe de custo              *
*----------------------------------------------------------------------*
FORM F_ALV_ESTRUTURA_RES .
* Montar estruduta de ALV
  PERFORM F_ALV_EST_RES.
* Monta cabeçalho
  PERFORM F_MONTA_CABECALHO USING 'RS'.
* Executa ALV
  PERFORM F_ALV_EXECUTA_RES.
ENDFORM.                    " F_ALV_ESTRUTURA_RES
*&---------------------------------------------------------------------*
*&      Form  F_ALV_EST_RES
*&---------------------------------------------------------------------*
*       Montagem da estrutura do ALV                                   *
*----------------------------------------------------------------------*
FORM F_ALV_EST_RES .
  PERFORM F_FIELDCAT USING:
         '00' '' 'IT_ALV_RES' 'STATUS' 'Apontar'
         05   ''  ''            '' 'X' CHANGING IT_FIELDCAT,
         '01' '' 'IT_ALV_RES' 'BUKRS' 'Empresa'
         07  ''  ''             '' ''  CHANGING IT_FIELDCAT,
         '02' '' 'IT_ALV_RES' 'BUTXT' 'Nome Empresa'
         20  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '03' '' 'IT_ALV_RES' 'GSBER' 'Centro'
         07  ''  ''             '' ''  CHANGING IT_FIELDCAT,
         '04' '' 'IT_ALV_RES' 'NAME1' 'Nome Centro'
         20  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '05' '' 'IT_ALV_RES' 'BUDAT' 'Data Lcto'
         10  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '06' '' 'IT_ALV_RES' 'BELNR' 'Doc.Contab.'
         10  ''  ''             '' 'X' CHANGING IT_FIELDCAT,
         '07' '' 'IT_ALV_RES' 'KOSTL' 'Centro Custo'
         10  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '08' '' 'IT_ALV_RES' 'KTEXT' 'Descrição'
         20  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '09' '' 'IT_ALV_RES' 'HKONT6' 'Conta'
         10  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '10' '' 'IT_ALV_RES' 'TXT20' 'Descrição'
         20  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '11' '' 'IT_ALV_RES' 'DMBTR' 'Valor R$'
         15  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '12' '' 'IT_ALV_RES' 'DMBE2' 'Valor US$'
         15  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '13' '' 'IT_ALV_RES' 'AWK10' 'Doc.Material'
         10  ''  ''             '' 'X' CHANGING IT_FIELDCAT,
         '14' '' 'IT_ALV_RES' 'RSNUM' 'Reserva'
         10  ''  ''             '' 'X' CHANGING IT_FIELDCAT,
         '15' '' 'IT_ALV_RES' 'USNAM' 'Us.Reserva'
         10  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '16' '' 'IT_ALV_RES' 'MATNR9' 'Material'
         10  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '17' '' 'IT_ALV_RES' 'MAKTX' 'Descrição'
         20  ''  ''             '' '' CHANGING IT_FIELDCAT,
         '18' '' 'IT_ALV_RES' 'MATKL' 'Gpo.Merc.'
          8  ''  ''             '' ''  CHANGING IT_FIELDCAT,
         '19' '' 'IT_ALV_RES' 'WGBEZ60' 'Descrição'
         20  ''  ''             '' ''  CHANGING IT_FIELDCAT.

ENDFORM.                    " F_ALV_EST_RES
*&---------------------------------------------------------------------*
*&      Form  F_ALV_EXECUTA_RES
*&---------------------------------------------------------------------*
*       Executa a ALV com as informações da it_avl                     *
*----------------------------------------------------------------------*
FORM F_ALV_EXECUTA_RES .

* Variavel Local
  DATA: VL_REPID LIKE SY-REPID.

  VL_REPID = SY-REPID.

  IT_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  IT_EVENT-FORM = SLIS_EV_TOP_OF_PAGE.
  APPEND IT_EVENT.

* Determinar a tabela de cores
  VG_LAYOUT-ZEBRA               = 'X'.

* Função para exibir o ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = VL_REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND_RES'
      IS_LAYOUT                = VG_LAYOUT
      IT_FIELDCAT              = IT_FIELDCAT[]
      I_DEFAULT                = 'A'
      I_SAVE                   = 'A'
      IT_EVENTS                = IT_EVENT[]
    TABLES
      T_OUTTAB                 = IT_ALV_RES
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_ALV_EXECUTA_RES

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_RES
*&---------------------------------------------------------------------*
*       Ao clicar na estrutura vai listar todos os niveis
*----------------------------------------------------------------------*
FORM USER_COMMAND_RES  USING P_UCOMM LIKE SY-UCOMM
      P_FIELD TYPE SLIS_SELFIELD.

  DATA: T_ZMMT0091 TYPE TABLE OF ZMMT0091.
  DATA: W_ZMMT0091 TYPE ZMMT0091.



  IF P_FIELD-FIELDNAME = 'STATUS'.
     READ TABLE IT_ALV_RES  INTO WA_ALV_RES INDEX P_FIELD-TABINDEX.

     IF WA_ALV_RES-STATUS = ICON_CHECKED.
        WA_ALV_RES-STATUS = ICON_GENERATE.
     ELSE.
        WA_ALV_RES-STATUS = ICON_CHECKED.
      ENDIF.
      MODIFY IT_ALV_RES FROM WA_ALV_RES INDEX  P_FIELD-TABINDEX.

     IF WA_ALV_RES-STATUS = ICON_GENERATE.
        W_ZMMT0091-BUKRS  = WA_ALV_RES-BUKRS.
        W_ZMMT0091-BELNR  = WA_ALV_RES-BELNR.

        DELETE  ZMMT0091  FROM W_ZMMT0091.

     ELSE.
        W_ZMMT0091-BUKRS  = WA_ALV_RES-BUKRS.
        W_ZMMT0091-BELNR  = WA_ALV_RES-BELNR.
        W_ZMMT0091-STATUS = 'X'.
        MODIFY ZMMT0091 FROM W_ZMMT0091.
     ENDIF.

    MESSAGE TEXT-001 TYPE 'S'.
    PERFORM F_ALV_EXECUTA_RES.
  ENDIF.




  IF P_FIELD-FIELDNAME = 'BELNR'.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        TCODE  = 'FB03'
      EXCEPTIONS
        OK     = 1
        NOT_OK = 2.
    IF SY-SUBRC = 2.
      MESSAGE E077(S#) WITH 'FB03'.
    ENDIF.

    READ TABLE IT_ALV_RES INTO WA_ALV_RES INDEX P_FIELD-TABINDEX.

    IF WA_ALV_RES-BELNR IS NOT INITIAL
      AND WA_ALV_RES-BUKRS IS NOT INITIAL
      AND WA_ALV_RES-BUDAT IS NOT INITIAL.

      SET PARAMETER ID: 'BLN' FIELD WA_ALV_RES-BELNR,
                        'BUK' FIELD WA_ALV_RES-BUKRS,
                        'GJR' FIELD WA_ALV_RES-BUDAT(4).
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDIF.

  IF P_FIELD-FIELDNAME = 'AWK10'.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        TCODE  = 'MIGO'
      EXCEPTIONS
        OK     = 1
        NOT_OK = 2.
    IF SY-SUBRC = 2.
      MESSAGE E077(S#) WITH 'MIGO'.
    ENDIF.

    READ TABLE IT_ALV_RES INTO WA_ALV_RES INDEX P_FIELD-TABINDEX.

    IF WA_ALV_RES-AWK10 IS NOT INITIAL.
      SET PARAMETER ID 'MBN' FIELD WA_ALV_RES-AWK10.
      CALL TRANSACTION 'MIGO' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDIF.

  IF P_FIELD-FIELDNAME = 'RSNUM'.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        TCODE  = 'MB23'
      EXCEPTIONS
        OK     = 1
        NOT_OK = 2.
    IF SY-SUBRC = 2.
      MESSAGE E077(S#) WITH 'MB23'.
    ENDIF.

    READ TABLE IT_ALV_RES INTO WA_ALV_RES INDEX P_FIELD-TABINDEX.
    IF WA_ALV_RES-RSNUM IS NOT INITIAL.
      SET PARAMETER ID 'RES' FIELD WA_ALV_RES-RSNUM.
      CALL TRANSACTION 'MB23' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    "user_command_res
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_CABECALHO
*&---------------------------------------------------------------------*
*       Cabeçalho do relatório de requisição
*----------------------------------------------------------------------*
FORM F_MONTA_CABECALHO USING TIPO TYPE C.

  DATA: VL_BUTXT        LIKE T001-BUTXT,  "Nome da empresa
        VL_FILIAL       LIKE T001W-NAME1, "Nome da filial
        VL_DATA1(10)    TYPE C,
        VL_DATA2(10)    TYPE C,
        VL_DATA(25)     TYPE C,
        VL_FILIAL2(100) TYPE C,
        VL_HKONT(25).

  CLEAR IT_HEADER.
  IT_HEADER-TYP  = 'H'.
  IF TIPO EQ 'RQ'.
    IT_HEADER-INFO = 'Rel. de Analise de Classe de Custo via Requisição (MM)'.
  ELSE.
    IT_HEADER-INFO = 'Rel. de Analise de Classe de Custo via Reserva'.
  ENDIF.
  APPEND  IT_HEADER.

  SELECT SINGLE BUTXT
   FROM T001
   INTO VL_BUTXT
  WHERE BUKRS = PA_BUKRS.
  CONCATENATE PA_BUKRS
              VL_BUTXT INTO VL_BUTXT
              SEPARATED BY SPACE.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Empresa'.
  IT_HEADER-INFO = VL_BUTXT.
  APPEND  IT_HEADER.

  IF PB_WERKS IS NOT INITIAL.

    SELECT SINGLE NAME1
      FROM T001W
      INTO VL_FILIAL
     WHERE WERKS EQ PB_WERKS.

    CONCATENATE PB_WERKS
                  VL_FILIAL INTO VL_FILIAL2
                  SEPARATED BY SPACE.

    IT_HEADER-TYP  = 'S'.
    IT_HEADER-KEY  = 'Filial'.
    IT_HEADER-INFO = VL_FILIAL2.
    APPEND  IT_HEADER.

  ENDIF.

  CONCATENATE PE_BUDAT-LOW+6(2) '.'
              PE_BUDAT-LOW+4(2) '.'
              PE_BUDAT-LOW(4)
              INTO VL_DATA1.

  CONCATENATE PE_BUDAT-HIGH+6(2) '.'
              PE_BUDAT-HIGH+4(2) '.'
              PE_BUDAT-HIGH(4)
              INTO VL_DATA2.

  CONCATENATE VL_DATA1
              'a'
              VL_DATA2 INTO VL_DATA
              SEPARATED BY SPACE.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Periodo'.
  IT_HEADER-INFO = VL_DATA.
  APPEND  IT_HEADER.

  CONCATENATE PC_HKONT-LOW+4(6)
              'a'
              PC_HKONT-HIGH+4(6) INTO VL_HKONT
              SEPARATED BY SPACE.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Conta do Razão'.
  IT_HEADER-INFO = VL_HKONT.
  APPEND  IT_HEADER.

ENDFORM.                    " F_MONTA_CABECALHO

*&---------------------------------------------------------------------*
*&      Form  top_of_page                                              *
*&---------------------------------------------------------------------*
*     Form Para Fazer o cabeçalho   no ALV                             *
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
* Cabeçalho
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = c_logo
      IT_LIST_COMMENTARY = IT_HEADER[].
  SET TITLEBAR 'INI'.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Module  COMANDO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE COMANDO INPUT.

*  DATA: T_ZMMT0091 TYPE TABLE OF ZMMT0091.
*  DATA: W_ZMMT0091 TYPE ZMMT0091.
*
*
*  CASE SY-UCOMM.
*    WHEN 'SALVAR'.
*      IF R_REQ IS NOT INITIAL.
*        LOOP AT IT_ALV_REQ INTO WA_ALV_REQ.
*          W_ZMMT0091-BUKRS = WA_ALV_REQ-BUKRS.
*          W_ZMMT0091-BELNR = WA_ALV_REQ-BELNR.
*          W_ZMMT0091-STATUS = WA_ALV_REQ-STATUS.
*          MODIFY ZMMT0091 FROM W_ZMMT0091.
*        ENDLOOP.
*      ELSE.
*        LOOP AT IT_ALV_RES INTO WA_ALV_RES.
*          W_ZMMT0091-BUKRS  = WA_ALV_RES-BUKRS.
*          W_ZMMT0091-BELNR  = WA_ALV_RES-BELNR.
*          W_ZMMT0091-STATUS = WA_ALV_RES-STATUS.
*          MODIFY ZMMT0091 FROM W_ZMMT0091.
*        ENDLOOP.
*      ENDIF.
*
*      MESSAGE TEXT-001 TYPE 'S'.
*
*  ENDCASE.

ENDMODULE.


*CLASS LCL_EVENT_HANDLER DEFINITION.
*  PUBLIC SECTION.
*   CLASS-METHODS:
*  DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
*  IMPORTING E_MODIFIED ET_GOOD_CELLS.
*
*
*ENDCLASS.

*CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
*  METHOD DATA_CHANGED_FINISHED.
*
*  DATA: WA_GOOD_CELLS   TYPE LVC_S_MODI.
*
*  LOOP AT ET_GOOD_CELLS INTO WA_GOOD_CELLS.
*    IF WA_GOOD_CELLS-FIELDNAME = 'STATUS'.
*      IF R_REQ IS NOT INITIAL.
*        READ TABLE IT_ALV_REQ INTO WA_ALV_REQ INDEX WA_GOOD_CELLS-ROW_ID.
*           WA_ALV_REQ-STATUS = 'X'.
*           MODIFY IT_ALV_REQ FROM WA_ALV_REQ TRANSPORTING STATUS.
*       ELSE.
*        READ TABLE IT_ALV_RES INTO WA_ALV_RES INDEX WA_GOOD_CELLS-ROW_ID.
*           WA_ALV_RES-STATUS = 'X'.
*           MODIFY IT_ALV_RES FROM WA_ALV_RES TRANSPORTING STATUS.
*       ENDIF.
*      ENDIF.
*  ENDLOOP.
*
*ENDMETHOD.
*
*ENDCLASS.


FORM DATA_CHANGED_FINISHED.
  DATA: WA_GOOD_CELLS  TYPE LVC_S_MODI.
  DATA: ET_GOOD_CELLS TYPE  LVC_T_MODI.

  LOOP AT ET_GOOD_CELLS INTO WA_GOOD_CELLS.
    IF WA_GOOD_CELLS-FIELDNAME = 'STATUS'.
      IF R_REQ IS NOT INITIAL.
        READ TABLE IT_ALV_REQ INTO WA_ALV_REQ INDEX WA_GOOD_CELLS-ROW_ID.
        WA_ALV_REQ-STATUS = 'X'.
        MODIFY IT_ALV_REQ FROM WA_ALV_REQ TRANSPORTING STATUS.
      ELSE.
        READ TABLE IT_ALV_RES INTO WA_ALV_RES INDEX WA_GOOD_CELLS-ROW_ID.
        WA_ALV_RES-STATUS = 'X'.
        MODIFY IT_ALV_RES FROM WA_ALV_RES TRANSPORTING STATUS.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
