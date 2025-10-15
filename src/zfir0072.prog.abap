*&---------------------------------------------------------------------*
*& Report  ZFIR0072 - Aging List                                       *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT ZFIR0072.

*----------------------------------------------------------------------*
* TYPES                                                                *
*----------------------------------------------------------------------*
TABLES: BSIK.

TYPES: BEGIN OF LI_DADOS,
         GJAHR    TYPE BSIK-GJAHR,
         LIFNR    TYPE BSIK-LIFNR,
         KUNNR    TYPE BSID-KUNNR,
         HKONT    TYPE BSIK-HKONT,
         BUDAT    TYPE BSIK-BUDAT,
         BLDAT    TYPE BSIK-BLDAT,
         ZFBDT    TYPE BSIK-ZFBDT,
         ZBD1T    TYPE BSIK-ZBD1T,
         BLART    TYPE BSIK-BLART,
         BELNR    TYPE BSIK-BELNR,
         WAERS    TYPE BSIK-WAERS,
         BUKRS    TYPE BSIK-BUKRS,
         DMBTR    TYPE BSIK-DMBTR,
         DMBE2    TYPE BSIK-DMBE2,
         GSBER    TYPE BSIK-GSBER,
         SGTXT    TYPE BSIK-SGTXT,
         XBLNR    TYPE BSIK-XBLNR,
         EBELN    TYPE BSIK-EBELN,
         VBEL2    TYPE BSID-VBEL2,
         VBELN    TYPE BSID-VBELN,
         UMSKZ    TYPE BSIK-UMSKZ,
         NAME1    TYPE T001W-NAME1,
         AWKEY    TYPE J_1BNFLIN-REFKEY,
         USNAM    TYPE BKPF-USNAM,
         CPUDT    TYPE BKPF-CPUDT,
         CPUTM    TYPE BKPF-CPUTM,
         TCODE    TYPE BKPF-TCODE,
         TXT50    TYPE SKAT-TXT50,
         WAERS2   TYPE T001-WAERS,
         BUTXT    TYPE T001-BUTXT,
         DOCNUM   TYPE J_1BNFLIN-DOCNUM,
         NFE      TYPE J_1BNFDOC-NFE,
         NFNUM    TYPE J_1BNFDOC-NFNUM,
         NFENUM   TYPE J_1BNFDOC-NFENUM,
         BELNR2   TYPE RBKP-BELNR,
         XBLNR2   TYPE RBKP-XBLNR,
         NAME2    TYPE LFA1-NAME1,
         ICON     TYPE STRING,
         XDTVCTO  TYPE D,
         XDTVCTO2 TYPE I,
         CAMBIO   TYPE P DECIMALS 4,
         TPLCTO   TYPE STRING,
         NRNOTA   TYPE J_1BNFDOC-NFENUM,
         CLASS    TYPE I,
         AWKEY2   TYPE RBKP-BELNR,
         SHKZG    TYPE BSIK-SHKZG.
TYPES: END OF LI_DADOS.

TYPES: BEGIN OF LI_CLASS,
         CLASS_I  TYPE I,
         CLASS_F  TYPE I,
         CLASS_T  TYPE STRING,
         CLASS    TYPE I,
         XTOTVR   TYPE BSIK-DMBTR,
         XTOTVUS  TYPE BSIK-DMBE2,
         XTOTNVR  TYPE BSIK-DMBTR,
         XTOTNVUS TYPE BSIK-DMBE2,
         TOTRS    TYPE BSIK-DMBTR,
         TOTUS    TYPE BSIK-DMBE2,
         BUKRS    TYPE BSIK-BUKRS.
TYPES: END OF LI_CLASS.

TYPE-POOLS: ICON.

*----------------------------------------------------------------------*
* DECLARAÇÕES                                                            *
*----------------------------------------------------------------------*

DATA: IT_DADOS TYPE TABLE OF LI_DADOS,
      IT_TEMP  TYPE TABLE OF LI_DADOS,
      V_TABELA TYPE TABLE OF LI_DADOS,
      IT_CLASS TYPE TABLE OF LI_CLASS,
      WA_CLASS TYPE LI_CLASS,
      WA_TEMP TYPE LI_DADOS.

FIELD-SYMBOLS: <WA_DADOS> TYPE LI_DADOS.

DATA: IT_T001W     TYPE TABLE OF T001W,
      WA_T001W     TYPE T001W,
      IT_BKPF      TYPE TABLE OF BKPF,
      WA_BKPF      TYPE BKPF,
      IT_SKAT      TYPE TABLE OF SKAT,
      WA_SKAT      TYPE SKAT,
      IT_T001      TYPE TABLE OF T001,
      WA_T001      TYPE T001,
      IT_RBKP      TYPE TABLE OF RBKP,
      WA_RBKP      TYPE RBKP,
      IT_J_1BNFLIN TYPE TABLE OF J_1BNFLIN,
      WA_J_1BNFLIN TYPE J_1BNFLIN,
      IT_J_1BNFDOC TYPE TABLE OF J_1BNFDOC,
      WA_J_1BNFDOC TYPE J_1BNFDOC,
      IT_LFA1      TYPE TABLE OF LFA1,
      WA_LFA1      TYPE LFA1,
      IT_KNA1      TYPE TABLE OF KNA1,
      WA_KNA1      TYPE KNA1.

DATA: P_TCO TYPE CHAR1.

RANGES: R_COD FOR LFB1-AKONT,
        R_CON FOR KNB1-AKONT .

DATA: IT_FCAT TYPE SLIS_T_FIELDCAT_ALV.

TABLES: LFB1, KNB1.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO                                                      *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN SKIP.


SELECT-OPTIONS: P_EMP FOR BSIK-BUKRS.

PARAMETERS: P_PAR TYPE DATUM.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (14) TEXT-008.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_TCO_K RADIOBUTTON GROUP G4 USER-COMMAND SEL DEFAULT 'X'.
SELECTION-SCREEN COMMENT (10) TEXT-009.
PARAMETERS: P_TCO_D RADIOBUTTON GROUP G4.
SELECTION-SCREEN COMMENT (10) TEXT-010.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS:
                P_COD_K FOR LFB1-LIFNR MODIF ID M1 MATCHCODE OBJECT KREDA,
                P_COD_D FOR KNB1-KUNNR MODIF ID M2 MATCHCODE OBJECT DEBIA,
                P_CON_K FOR LFB1-AKONT MODIF ID M1,
                P_CON_D FOR KNB1-AKONT MODIF ID M2.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

SELECTION-SCREEN SKIP.

PARAMETERS: PP_IND RADIOBUTTON GROUP G1 DEFAULT 'X'.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 4.
PARAMETERS: PP_IND_V AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (8) TEXT-003.
PARAMETERS: PP_IND_A AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (8) TEXT-004.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

PARAMETERS: PP_AGR   RADIOBUTTON GROUP G1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 4.
PARAMETERS: PP_AGR_V AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (8) TEXT-003.
PARAMETERS: PP_AGR_A AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (8) TEXT-004.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 4.
SELECTION-SCREEN COMMENT (18) TEXT-005.
PARAMETERS: P1 TYPE CHAR5 DEFAULT '30'.
SELECTION-SCREEN COMMENT (10) TEXT-006.
PARAMETERS: P2 TYPE CHAR5 DEFAULT '360'.
SELECTION-SCREEN COMMENT (5) TEXT-007.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B2.
SELECTION-SCREEN END OF BLOCK B1.


AT SELECTION-SCREEN OUTPUT.

  IF P_TCO_K = 'X'.
    PERFORM HIDE_M1_OPTIONS.
    P_TCO = 'K'.
  ELSE.
    PERFORM HIDE_M2_OPTIONS.
    P_TCO = 'D'.
  ENDIF.

*----------------------------------------------------------------------*
* SELEÇÃO                                                              *
*----------------------------------------------------------------------*

START-OF-SELECTION.

  IF  P_EMP IS INITIAL OR P_PAR IS INITIAL.
    MESSAGE I000(Z01) WITH 'É obrigatório informar a Empresa e Data para' 'Partidas em Aberto.'.
    STOP.
  ENDIF.

  IF P_TCO_K = 'X'.
    LOOP AT P_COD_K.
      R_COD = P_COD_K.
      APPEND R_COD.
    ENDLOOP.
    LOOP AT P_CON_K.
      R_CON = P_CON_K.
      APPEND R_CON.
    ENDLOOP.
  ELSE.
    LOOP AT P_COD_D.
      R_COD = P_COD_D.
      APPEND R_COD.
    ENDLOOP.
    LOOP AT P_CON_D.
      R_CON = P_CON_D.
      APPEND R_CON.
    ENDLOOP.
  ENDIF.

  IF R_COD  IS INITIAL AND R_CON  IS INITIAL.
    MESSAGE I000(Z01) WITH 'Informe um Código ou uma Conta.'.
    STOP.
  ENDIF.

  PERFORM: SELECIONA_IT_DADOS.

  IF  IT_DADOS IS INITIAL.
    MESSAGE I000(Z01) WITH 'Nenhum dado de Partida encontrado.'.
    STOP.
  ENDIF.

  PERFORM: COMPLETE_IT_DADOS,
           DELETE_DADOS.

  IF  IT_DADOS IS INITIAL.
    MESSAGE I000(Z01) WITH 'Nenhum dado de Partida encontrado.'.
    STOP.
  ENDIF.

  IF PP_AGR = 'X'.
    PERFORM: TABELA_CLASSE,
             COMPLETE_IT_DADOS_CLASSE.
  ENDIF.

*----------------------------------------------------------------------*
* IMPRESSÃO                                                            *
*----------------------------------------------------------------------*

  IF P_TCO_K = 'X'.
    SET TITLEBAR 'K'.
  ELSE.
    SET TITLEBAR 'D'.
  ENDIF.

  IF PP_IND = 'X'.
    V_TABELA = IT_DADOS.
    PERFORM PRINT_ALV.
  ELSEIF PP_AGR = 'X'.
    PERFORM PRINT_ALV_AGR.
  ENDIF.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*-FORMS----------------------------------------------------------------*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  HIDE_M1_OPTIONS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM HIDE_M1_OPTIONS .
  LOOP AT SCREEN.
    CASE SCREEN-GROUP1.
      WHEN 'M1'.
        SCREEN-ACTIVE = 1.
        CLEAR: P_COD_K.
        MODIFY SCREEN.
      WHEN 'M2'.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HIDE_M2_OPTIONS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM HIDE_M2_OPTIONS .
  LOOP AT SCREEN.
    CASE SCREEN-GROUP1.
      WHEN 'M2'.
        SCREEN-ACTIVE = 1.
        CLEAR: P_COD_D.
        MODIFY SCREEN.
      WHEN 'M1'.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM SELECIONA_IT_DADOS.

  IF P_TCO = 'K'.

    IF R_COD IS INITIAL.

      SELECT * FROM BSIK INTO CORRESPONDING FIELDS OF TABLE IT_DADOS
        WHERE BUKRS IN P_EMP
        AND HKONT IN R_CON
        AND BUDAT <= P_PAR
"        AND BLART <> 'VC'
        AND UMSKZ <> 'F'.

      SELECT * FROM BSAK APPENDING CORRESPONDING FIELDS OF TABLE IT_DADOS
       WHERE BUKRS IN P_EMP
       AND HKONT IN R_CON
       AND AUGDT > P_PAR
       AND BUDAT <= P_PAR
       AND UMSKZ <> 'F'.

    ELSEIF R_CON IS INITIAL.

      SELECT * FROM BSIK INTO CORRESPONDING FIELDS OF TABLE IT_DADOS
        WHERE BUKRS IN P_EMP
        AND LIFNR IN R_COD
        AND BUDAT <= P_PAR
"        AND BLART <> 'VC'
        AND UMSKZ <> 'F'.

      SELECT * FROM BSAK APPENDING CORRESPONDING FIELDS OF TABLE IT_DADOS
       WHERE BUKRS IN P_EMP
       AND LIFNR IN R_COD
       AND AUGDT > P_PAR
       AND BUDAT <= P_PAR
       AND UMSKZ <> 'F'.

    ELSE.

      SELECT * FROM BSIK INTO CORRESPONDING FIELDS OF TABLE IT_DADOS
        WHERE BUKRS IN P_EMP
        AND LIFNR IN R_COD
        AND HKONT IN R_CON
        AND BUDAT <= P_PAR
"        AND BLART <> 'VC'
        AND UMSKZ <> 'F'.

      SELECT * FROM BSAK APPENDING CORRESPONDING FIELDS OF TABLE IT_DADOS
        WHERE BUKRS IN P_EMP
        AND LIFNR IN R_COD
        AND HKONT IN R_CON
        AND AUGDT > P_PAR
        AND BUDAT <= P_PAR
        AND UMSKZ <> 'F'.

    ENDIF.

  ELSEIF P_TCO = 'D'.

    IF R_COD IS INITIAL.

      SELECT * FROM BSID INTO CORRESPONDING FIELDS OF TABLE IT_DADOS
        WHERE BUKRS IN P_EMP
        AND HKONT IN R_CON
        AND BUDAT <= P_PAR
"        AND BLART <> 'VC'
        AND UMSKZ <> 'F'
        AND UMSKS <> 'W'.

      SELECT * FROM BSAD APPENDING CORRESPONDING FIELDS OF TABLE IT_DADOS
        WHERE BUKRS IN P_EMP
        AND HKONT IN R_CON
        AND AUGDT > P_PAR
        AND BUDAT <= P_PAR
        AND UMSKZ <> 'F'
        AND UMSKS <> 'W'
        AND UMSKZ <> 'R'.

    ELSEIF R_CON IS INITIAL.

      SELECT * FROM BSID INTO CORRESPONDING FIELDS OF TABLE IT_DADOS
        WHERE BUKRS IN P_EMP
        AND KUNNR IN R_COD
        AND BUDAT <= P_PAR
"        AND BLART <> 'VC'
        AND UMSKZ <> 'F'
        AND UMSKS <> 'W'.

      SELECT * FROM BSAD APPENDING CORRESPONDING FIELDS OF TABLE IT_DADOS
        WHERE BUKRS IN P_EMP
        AND KUNNR IN R_COD
        AND AUGDT > P_PAR
        AND BUDAT <= P_PAR
        AND UMSKZ <> 'F'
        AND UMSKS <> 'W'
        AND UMSKZ <> 'R'.

    ELSE.

      SELECT * FROM BSID INTO CORRESPONDING FIELDS OF TABLE IT_DADOS
        WHERE BUKRS IN P_EMP
        AND KUNNR IN R_COD
        AND HKONT IN R_CON
        AND BUDAT <= P_PAR
"        AND BLART <> 'VC'
        AND UMSKZ <> 'F'
        AND UMSKS <> 'W'.

      SELECT * FROM BSAD APPENDING CORRESPONDING FIELDS OF TABLE IT_DADOS
        WHERE BUKRS IN P_EMP
        AND KUNNR IN R_COD
        AND HKONT IN R_CON
        AND AUGDT > P_PAR
        AND BUDAT <= P_PAR
        AND UMSKZ <> 'F'
        AND UMSKS <> 'W'
        AND UMSKZ <> 'R'.

    ENDIF.

  ENDIF.

  IF IT_DADOS IS INITIAL.
    EXIT.
  ENDIF.

  IT_TEMP = IT_DADOS.
  SORT IT_TEMP ASCENDING.
  DELETE ADJACENT DUPLICATES FROM IT_TEMP COMPARING GSBER.

  SELECT * FROM T001W INTO TABLE IT_T001W
    FOR ALL ENTRIES IN IT_TEMP
    WHERE WERKS = IT_TEMP-GSBER.

  CLEAR IT_TEMP.
  IT_TEMP = IT_DADOS.
  SORT IT_TEMP ASCENDING.
  DELETE ADJACENT DUPLICATES FROM IT_TEMP COMPARING BUKRS BELNR GJAHR.

  SELECT * FROM BKPF INTO TABLE IT_BKPF
    FOR ALL ENTRIES IN IT_TEMP
    WHERE BUKRS = IT_TEMP-BUKRS
    AND BELNR = IT_TEMP-BELNR
    AND GJAHR = IT_TEMP-GJAHR.

  CLEAR IT_TEMP.
  IT_TEMP = IT_DADOS.
  SORT IT_TEMP ASCENDING.
  DELETE ADJACENT DUPLICATES FROM IT_TEMP COMPARING HKONT.

  SELECT * FROM SKAT INTO TABLE IT_SKAT
    FOR ALL ENTRIES IN IT_TEMP
    WHERE SPRAS = SY-LANGU
    AND KTOPL = '0050'
    AND SAKNR = IT_TEMP-HKONT.

  CLEAR IT_TEMP.
  IT_TEMP = IT_DADOS.
  SORT IT_TEMP ASCENDING.
  DELETE ADJACENT DUPLICATES FROM IT_TEMP COMPARING BUKRS.

  SELECT * FROM T001 INTO TABLE IT_T001
    FOR ALL ENTRIES IN IT_TEMP
    WHERE BUKRS = IT_TEMP-BUKRS.

  IF P_TCO = 'K'.

    CLEAR IT_TEMP.
    IT_TEMP = IT_DADOS.
    SORT IT_TEMP ASCENDING.
    DELETE ADJACENT DUPLICATES FROM IT_TEMP COMPARING LIFNR.

    SELECT * FROM LFA1 INTO TABLE IT_LFA1
      FOR ALL ENTRIES IN IT_TEMP
      WHERE LIFNR = IT_TEMP-LIFNR.

    LOOP AT IT_DADOS ASSIGNING <WA_DADOS>.
      READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = <WA_DADOS>-GSBER.
      READ TABLE IT_SKAT INTO WA_SKAT WITH KEY SAKNR = <WA_DADOS>-HKONT.
      READ TABLE IT_T001 INTO WA_T001 WITH KEY BUKRS = <WA_DADOS>-BUKRS.
      READ TABLE IT_BKPF INTO WA_BKPF WITH KEY BUKRS = <WA_DADOS>-BUKRS BELNR = <WA_DADOS>-BELNR GJAHR = <WA_DADOS>-GJAHR.
      IF <WA_DADOS>-BLART = 'WR'.
        <WA_DADOS>-AWKEY = WA_BKPF-AWKEY+4(10).
      ELSE.
        <WA_DADOS>-AWKEY = WA_BKPF-AWKEY(10).
        <WA_DADOS>-AWKEY2 = WA_BKPF-AWKEY(10).
      ENDIF.
      READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = <WA_DADOS>-LIFNR.
      <WA_DADOS>-NAME1 = WA_T001W-NAME1.
      <WA_DADOS>-USNAM = WA_BKPF-USNAM.
      <WA_DADOS>-CPUDT = WA_BKPF-CPUDT.
      <WA_DADOS>-CPUTM = WA_BKPF-CPUTM.
      <WA_DADOS>-TCODE = WA_BKPF-TCODE.
      <WA_DADOS>-TXT50 = WA_SKAT-TXT50.
      <WA_DADOS>-WAERS2 = WA_T001-WAERS.
      <WA_DADOS>-BUTXT = WA_T001-BUTXT.
      <WA_DADOS>-NAME2 = WA_LFA1-NAME1.
      <WA_DADOS>-BUKRS  = WA_BKPF-BUKRS.

      CLEAR: WA_T001W,
             WA_SKAT,
             WA_T001,
             WA_BKPF,
             WA_LFA1,
             WA_RBKP.

    ENDLOOP.

  ELSEIF P_TCO = 'D'.

    CLEAR IT_TEMP.
    IT_TEMP = IT_DADOS.
    SORT IT_TEMP ASCENDING.
*Inicio de alteração - 21/10/2022 - CS1031594 - FMARTINS
*    DELETE ADJACENT DUPLICATES FROM IT_TEMP COMPARING LIFNR.
     DELETE ADJACENT DUPLICATES FROM IT_TEMP COMPARING KUNNR.
*Fim de alteração - 21/10/2022 - CS1031594 - FMARTINS

    SELECT * FROM KNA1 INTO TABLE IT_KNA1
      FOR ALL ENTRIES IN IT_TEMP
      WHERE KUNNR = IT_TEMP-KUNNR.

    LOOP AT IT_DADOS ASSIGNING <WA_DADOS>.
      READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = <WA_DADOS>-GSBER.
      READ TABLE IT_SKAT INTO WA_SKAT WITH KEY SAKNR = <WA_DADOS>-HKONT.
      READ TABLE IT_T001 INTO WA_T001 WITH KEY BUKRS = <WA_DADOS>-BUKRS.
      READ TABLE IT_BKPF INTO WA_BKPF WITH KEY BUKRS = <WA_DADOS>-BUKRS BELNR = <WA_DADOS>-BELNR GJAHR = <WA_DADOS>-GJAHR.
      IF <WA_DADOS>-BLART = 'WR'.
        <WA_DADOS>-AWKEY = WA_BKPF-AWKEY+4(10).
      ELSE.
        <WA_DADOS>-AWKEY = WA_BKPF-AWKEY(10).
      ENDIF.
      READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = <WA_DADOS>-KUNNR.
      READ TABLE IT_RBKP INTO WA_RBKP WITH KEY GJAHR = <WA_DADOS>-GJAHR BELNR = <WA_DADOS>-AWKEY.
      <WA_DADOS>-NAME1 = WA_T001W-NAME1.
      <WA_DADOS>-USNAM = WA_BKPF-USNAM.
      <WA_DADOS>-CPUDT = WA_BKPF-CPUDT.
      <WA_DADOS>-CPUTM = WA_BKPF-CPUTM.
      <WA_DADOS>-TCODE = WA_BKPF-TCODE.
      <WA_DADOS>-TXT50 = WA_SKAT-TXT50.
      <WA_DADOS>-WAERS2 = WA_T001-WAERS.
      <WA_DADOS>-BUTXT = WA_T001-BUTXT.
      <WA_DADOS>-NAME2 = WA_KNA1-NAME1.
      <WA_DADOS>-BELNR2 = WA_RBKP-BELNR.
      <WA_DADOS>-XBLNR2 = WA_RBKP-XBLNR.
      <WA_DADOS>-BUKRS  = WA_BKPF-BUKRS.

      CLEAR: WA_T001W,
             WA_SKAT,
             WA_T001,
             WA_BKPF,
             WA_KNA1,
             WA_RBKP.

    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETE_IT_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM COMPLETE_IT_DADOS .

  CLEAR IT_TEMP.
  IT_TEMP = IT_DADOS.

  LOOP AT IT_DADOS ASSIGNING <WA_DADOS>.
    <WA_DADOS>-XDTVCTO = <WA_DADOS>-ZFBDT + <WA_DADOS>-ZBD1T.
    <WA_DADOS>-XDTVCTO2 = <WA_DADOS>-XDTVCTO - P_PAR.
    IF <WA_DADOS>-DMBE2 = 0.
      <WA_DADOS>-CAMBIO = 0.
    ELSE.
      <WA_DADOS>-CAMBIO = <WA_DADOS>-DMBTR / <WA_DADOS>-DMBE2.
    ENDIF.
    IF <WA_DADOS>-XDTVCTO2 < 0.
      <WA_DADOS>-ICON = ICON_ALERT.
    ELSE.
      <WA_DADOS>-ICON = ICON_RESUBMISSION.
    ENDIF.
    IF P2 MOD P1 = 0.
      IF ABS( <WA_DADOS>-XDTVCTO2 ) DIV P1 + 1 > P2 DIV P1.
        <WA_DADOS>-CLASS = P2 DIV P1.
      ELSE.
        <WA_DADOS>-CLASS = ABS( <WA_DADOS>-XDTVCTO2 ) DIV P1 + 1.
      ENDIF.
    ELSE.
      IF ABS( <WA_DADOS>-XDTVCTO2 ) DIV P1 + 1 > P2 DIV P1 + 1.
        <WA_DADOS>-CLASS = P2 DIV P1 + 1.
      ELSE.
        <WA_DADOS>-CLASS = ABS( <WA_DADOS>-XDTVCTO2 ) DIV P1 + 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF P_TCO = 'K'.

    SORT IT_TEMP ASCENDING.
    DELETE ADJACENT DUPLICATES FROM IT_TEMP COMPARING GJAHR AWKEY.

    SELECT * FROM RBKP INTO TABLE IT_RBKP
      FOR ALL ENTRIES IN IT_TEMP
      WHERE GJAHR = IT_TEMP-GJAHR
      AND BELNR = IT_TEMP-AWKEY2.

    SORT IT_TEMP ASCENDING.
    DELETE ADJACENT DUPLICATES FROM IT_TEMP COMPARING AWKEY.

    SELECT * FROM J_1BNFLIN INTO TABLE IT_J_1BNFLIN
      FOR ALL ENTRIES IN IT_TEMP
      WHERE REFKEY = IT_TEMP-AWKEY.

    IF IT_J_1BNFLIN IS NOT INITIAL.
      SELECT * FROM J_1BNFDOC INTO TABLE IT_J_1BNFDOC
        FOR ALL ENTRIES IN IT_J_1BNFLIN
        WHERE DOCNUM = IT_J_1BNFLIN-DOCNUM.
    ENDIF.

    LOOP AT IT_DADOS ASSIGNING <WA_DADOS>.
      IF <WA_DADOS>-SHKZG = 'H'.
        <WA_DADOS>-DMBTR = <WA_DADOS>-DMBTR * ( - 1 ).
        <WA_DADOS>-DMBE2 = <WA_DADOS>-DMBE2 * ( - 1 ).
      ENDIF.
      READ TABLE IT_RBKP INTO WA_RBKP WITH KEY GJAHR = <WA_DADOS>-GJAHR BELNR = <WA_DADOS>-AWKEY2.
      <WA_DADOS>-BELNR2 = WA_RBKP-BELNR.
      <WA_DADOS>-XBLNR2 = WA_RBKP-XBLNR.
      IF <WA_DADOS>-BELNR2 IS NOT INITIAL.
        <WA_DADOS>-TPLCTO = 'Fatura'.
      ELSEIF <WA_DADOS>-UMSKZ IS NOT INITIAL.
        <WA_DADOS>-TPLCTO = 'Adiantamento'.
      ELSE.
        <WA_DADOS>-TPLCTO = 'Lctos. Contabilidade'.
      ENDIF.

      IF <WA_DADOS>-BLART = 'WR'.
        READ TABLE IT_J_1BNFLIN INTO WA_J_1BNFLIN WITH KEY REFKEY = <WA_DADOS>-AWKEY.
        IF SY-SUBRC = 0.
          <WA_DADOS>-DOCNUM = WA_J_1BNFLIN-DOCNUM.
          READ TABLE IT_J_1BNFDOC INTO WA_J_1BNFDOC WITH KEY DOCNUM = <WA_DADOS>-DOCNUM.
          IF SY-SUBRC = 0.
            <WA_DADOS>-NFE = WA_J_1BNFDOC-NFE.
            IF <WA_DADOS>-NFE = 'X'.
              <WA_DADOS>-NRNOTA =  WA_J_1BNFDOC-NFENUM.
            ELSE.
              <WA_DADOS>-NRNOTA =  WA_J_1BNFDOC-NFNUM.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        IF <WA_DADOS>-BELNR2 <> ''.
          <WA_DADOS>-NRNOTA = <WA_DADOS>-XBLNR2.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-LIFNR
        IMPORTING
          OUTPUT = <WA_DADOS>-LIFNR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-HKONT
        IMPORTING
          OUTPUT = <WA_DADOS>-HKONT.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-BELNR
        IMPORTING
          OUTPUT = <WA_DADOS>-BELNR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-BELNR2
        IMPORTING
          OUTPUT = <WA_DADOS>-BELNR2.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-XBLNR
        IMPORTING
          OUTPUT = <WA_DADOS>-XBLNR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-EBELN
        IMPORTING
          OUTPUT = <WA_DADOS>-EBELN.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-NRNOTA
        IMPORTING
          OUTPUT = <WA_DADOS>-NRNOTA.

    ENDLOOP.

  ELSEIF P_TCO = 'D'.

    RANGES: R_STBLG FOR RBKP-STBLG.

    SORT IT_TEMP ASCENDING.
    DELETE ADJACENT DUPLICATES FROM IT_TEMP COMPARING VBELN.

    LOOP AT IT_DADOS ASSIGNING <WA_DADOS>.
      IF <WA_DADOS>-VBELN <> ' '.
        CLEAR: R_STBLG.
        R_STBLG-SIGN   = 'I'.
        R_STBLG-OPTION = 'EQ'.
        R_STBLG-LOW    = <WA_DADOS>-VBELN.
        R_STBLG-HIGH   = ' '.
        APPEND R_STBLG.
      ENDIF.
    ENDLOOP.

    IF R_STBLG IS NOT INITIAL.
      SELECT * FROM J_1BNFLIN INTO TABLE IT_J_1BNFLIN
        WHERE REFKEY IN R_STBLG.
    ENDIF.

    IF IT_J_1BNFLIN IS NOT INITIAL.
      SELECT * FROM J_1BNFDOC INTO TABLE IT_J_1BNFDOC
        FOR ALL ENTRIES IN IT_J_1BNFLIN
        WHERE DOCNUM = IT_J_1BNFLIN-DOCNUM.
    ENDIF.

    LOOP AT IT_DADOS ASSIGNING <WA_DADOS>.

      IF <WA_DADOS>-SHKZG = 'H'.
        <WA_DADOS>-DMBTR = <WA_DADOS>-DMBTR * ( - 1 ).
        <WA_DADOS>-DMBE2 = <WA_DADOS>-DMBE2 * ( - 1 ).
      ENDIF.

      IF <WA_DADOS>-VBELN IS NOT INITIAL.
        <WA_DADOS>-TPLCTO = 'Fatura'.
      ELSEIF <WA_DADOS>-UMSKZ IS NOT INITIAL.
        <WA_DADOS>-TPLCTO = 'Adiantamento'.
      ELSE.
        <WA_DADOS>-TPLCTO = 'Lctos. Contabilidade'.
      ENDIF.
      IF <WA_DADOS>-BLART <> 'WR'.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = <WA_DADOS>-VBELN
          IMPORTING
            OUTPUT = <WA_DADOS>-VBELN.

        READ TABLE IT_J_1BNFLIN INTO WA_J_1BNFLIN WITH KEY REFKEY = <WA_DADOS>-VBELN.
      ELSE.
        READ TABLE IT_J_1BNFLIN INTO WA_J_1BNFLIN WITH KEY REFKEY = <WA_DADOS>-AWKEY.
      ENDIF.

      IF SY-SUBRC = 0.
        <WA_DADOS>-DOCNUM = WA_J_1BNFLIN-DOCNUM.
        READ TABLE IT_J_1BNFDOC INTO WA_J_1BNFDOC WITH KEY DOCNUM = <WA_DADOS>-DOCNUM.
        IF SY-SUBRC = 0.
          <WA_DADOS>-NFE = WA_J_1BNFDOC-NFE.
          IF <WA_DADOS>-NFE = 'X'.
            <WA_DADOS>-NRNOTA =  WA_J_1BNFDOC-NFENUM.
          ELSE.
            <WA_DADOS>-NRNOTA =  WA_J_1BNFDOC-NFNUM.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-KUNNR
        IMPORTING
          OUTPUT = <WA_DADOS>-KUNNR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-HKONT
        IMPORTING
          OUTPUT = <WA_DADOS>-HKONT.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-BELNR
        IMPORTING
          OUTPUT = <WA_DADOS>-BELNR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-VBELN
        IMPORTING
          OUTPUT = <WA_DADOS>-VBELN.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-XBLNR
        IMPORTING
          OUTPUT = <WA_DADOS>-XBLNR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-VBEL2
        IMPORTING
          OUTPUT = <WA_DADOS>-VBEL2.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = <WA_DADOS>-NRNOTA
        IMPORTING
          OUTPUT = <WA_DADOS>-NRNOTA.

    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETE
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM DELETE_DADOS.
  IF PP_IND = 'X'.
    IF PP_IND_A = ''.
      DELETE IT_DADOS
        WHERE XDTVCTO2 >= 0.
    ENDIF.
    IF PP_IND_V = ''.
      DELETE IT_DADOS
        WHERE XDTVCTO2 < 0.
    ENDIF.
  ELSE.
    IF PP_AGR_A = ''.
      DELETE IT_DADOS
        WHERE XDTVCTO2 >= 0.
    ENDIF.
    IF PP_AGR_V = ''.
      DELETE IT_DADOS
        WHERE XDTVCTO2 < 0.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TABELA_CLASSE
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM TABELA_CLASSE .
  CLEAR IT_TEMP.
  IT_TEMP = IT_DADOS.
  SORT IT_TEMP BY BUKRS.
  DELETE ADJACENT DUPLICATES FROM IT_TEMP COMPARING BUKRS.
  loop at IT_TEMP INTO WA_TEMP.
  IF P2 MOD P1 <> 0.

    WHILE SY-INDEX <= ( P2 DIV P1 ) + 1.
      WA_CLASS-CLASS = SY-INDEX.
      IF SY-INDEX = 1.
        WA_CLASS-CLASS_I = ( SY-INDEX - 1 ) * P1.
      ELSE.
        WA_CLASS-CLASS_I = ( SY-INDEX - 1 ) * P1 + 1.
      ENDIF.
      IF SY-INDEX = ( P2 DIV P1 ) + 1.
        WA_CLASS-CLASS_F = 9999.
      ELSE.
        WA_CLASS-CLASS_F = SY-INDEX * P1.
      ENDIF.
      WA_CLASS-CLASS_T = WA_CLASS-CLASS_I && '-' && WA_CLASS-CLASS_F.
       WA_CLASS-BUKRS = WA_TEMP-BUKRS.
      APPEND WA_CLASS TO IT_CLASS.
      CLEAR WA_CLASS.
    ENDWHILE.
  ELSE.
    WHILE SY-INDEX <= P2 DIV P1.
      WA_CLASS-CLASS = SY-INDEX.
      IF SY-INDEX = 1.
        WA_CLASS-CLASS_I = ( SY-INDEX - 1 ) * P1.
      ELSE.
        WA_CLASS-CLASS_I = ( SY-INDEX - 1 ) * P1 + 1.
      ENDIF.
      IF SY-INDEX = ( P2 DIV P1 ).
        WA_CLASS-CLASS_F = 9999.
      ELSE.
        WA_CLASS-CLASS_F = SY-INDEX * P1.
      ENDIF.
      WA_CLASS-BUKRS = WA_TEMP-BUKRS.

      WA_CLASS-CLASS_T = WA_CLASS-CLASS_I && '-' && WA_CLASS-CLASS_F.
      APPEND WA_CLASS TO IT_CLASS.
      CLEAR WA_CLASS.
    ENDWHILE.
  ENDIF.
 ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETE_IT_DADOS_CLASSE
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM COMPLETE_IT_DADOS_CLASSE .

  DATA: TOTAL_R TYPE BSIK-DMBTR,
        TOTAL_D TYPE BSIK-DMBE2.

  LOOP AT IT_DADOS ASSIGNING <WA_DADOS>.
    READ TABLE IT_CLASS INTO WA_CLASS WITH KEY CLASS = <WA_DADOS>-CLASS BUKRS = <WA_DADOS>-BUKRS.
    IF <WA_DADOS>-XDTVCTO2 < 0.
      WA_CLASS-XTOTVR = WA_CLASS-XTOTVR + <WA_DADOS>-DMBTR.
      WA_CLASS-XTOTVUS = WA_CLASS-XTOTVUS + <WA_DADOS>-DMBE2.
    ELSE.
      WA_CLASS-XTOTNVR = WA_CLASS-XTOTNVR + <WA_DADOS>-DMBTR.
      WA_CLASS-XTOTNVUS = WA_CLASS-XTOTNVUS + <WA_DADOS>-DMBE2.
    ENDIF.
    WA_CLASS-TOTRS =  WA_CLASS-XTOTVR + WA_CLASS-XTOTNVR.
    WA_CLASS-TOTUS =  WA_CLASS-XTOTVUS + WA_CLASS-XTOTNVUS.

    MODIFY IT_CLASS FROM WA_CLASS  TRANSPORTING XTOTVR XTOTVUS XTOTNVR XTOTNVUS TOTRS TOTUS
    WHERE CLASS EQ <WA_DADOS>-CLASS
    AND   BUKRS EQ <WA_DADOS>-BUKRS.
  ENDLOOP.

ENDFORM.

*FORM COMPLETE_IT_DADOS_CLASSE .
*
*  DATA: TOTAL_R TYPE BSIK-DMBTR,
*        TOTAL_D TYPE BSIK-DMBE2.
*
*  LOOP AT IT_DADOS ASSIGNING <WA_DADOS>.
*    READ TABLE IT_CLASS INTO WA_CLASS WITH KEY CLASS = <WA_DADOS>-CLASS.
*    IF <WA_DADOS>-XDTVCTO2 < 0.
*      WA_CLASS-XTOTVR = WA_CLASS-XTOTVR + <WA_DADOS>-DMBTR.
*      WA_CLASS-XTOTVUS = WA_CLASS-XTOTVUS + <WA_DADOS>-DMBE2.
*    ELSE.
*      WA_CLASS-XTOTNVR = WA_CLASS-XTOTNVR + <WA_DADOS>-DMBTR.
*      WA_CLASS-XTOTNVUS = WA_CLASS-XTOTNVUS + <WA_DADOS>-DMBE2.
*    ENDIF.
*
*
*    WA_CLASS-TOTRS =  WA_CLASS-XTOTVR + WA_CLASS-XTOTNVR.
*    WA_CLASS-TOTUS =  WA_CLASS-XTOTVUS + WA_CLASS-XTOTNVUS.
*    WA_CLASS-BUKRS =  <WA_DADOS>-BUKRS.
*    MODIFY IT_CLASS FROM WA_CLASS INDEX SY-TABIX TRANSPORTING XTOTVR XTOTVUS XTOTNVR XTOTNVUS TOTRS TOTUS BUKRS.
*  ENDLOOP.
*
*ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  PRINT_ALV
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM PRINT_ALV .

  DATA: WA_LAY TYPE SLIS_LAYOUT_ALV,
        VALOR  TYPE STRING.

  WA_LAY-COLWIDTH_OPTIMIZE = 'X'. "Auto-ajuste das colunas do fieldcat
  READ TABLE IT_T001 INTO WA_T001 WITH KEY BUKRS = P_EMP.
  CONCATENATE 'Valor(' WA_T001-WAERS ')' INTO VALOR.
  REFRESH IT_FCAT.

  IF P_TCO = 'K'.

    PERFORM BUILD_FIELDCAT USING:
      'BUKRS' 'Empresa',
      'LIFNR' 'Código',
      'NAME2' 'Nome do Forncedor',
      'HKONT' 'Conta',
      'TXT50' 'Descrição Conta',
      'TPLCTO' 'Tp. Lançamento',
      'BELNR' 'Docto. Contábil',
      'BLART' 'Tp. Docto.',
      'BELNR2' 'Nro. MIRO',
      'XBLNR' 'Referência',
      'NRNOTA' 'Nro. Nota',
      'EBELN' 'Doc. Compra',
      'GJAHR' 'Ano',
      'BUDAT' 'Dt. Lcto',
      'BLDAT' 'Dt. Docto',
      'XDTVCTO' 'Dt. Vencimento',
      'XDTVCTO2' 'Qte. Dias Vcto.',
      'ICON' 'Simb. Vcto.',
      'DMBTR' VALOR,
      'DMBE2' 'Valor(US$)',
      'CAMBIO' 'Tx. Câmbio',
      'SGTXT' 'Texto Contábil',
      'GSBER' 'Filial',
      'NAME1' 'Nome Filial',
      'USNAM' 'Usuário',
      'CPUDT' 'Dt. Criação',
      'CPUTM' 'Hora Criação',
      'TCODE' 'Transação'.

  ELSEIF P_TCO = 'D'.

    PERFORM BUILD_FIELDCAT USING:
          'BUKRS' 'Empresa',
          'KUNNR' 'Código',
          'NAME2' 'Nome do Cliente',
          'HKONT' 'Conta',
          'TXT50' 'Descrição Conta',
          'TPLCTO' 'Tp. Lançamento',
          'BELNR' 'Docto. Contábil',
          'BLART' 'Tp. Docto.',
          'VBELN' 'Fatura SD',
          'XBLNR' 'Referência',
          'VBEL2' 'Ordem de Venda',
          'NRNOTA' 'Nro. Nota',
          'GJAHR' 'Ano',
          'BUDAT' 'Dt. Lcto',
          'BLDAT' 'Dt. Docto',
          'XDTVCTO' 'Dt. Vencimento',
          'XDTVCTO2' 'Qte. Dias Vcto.',
          'ICON' 'Simb. Vcto.',
          'DMBTR' VALOR,
          'DMBE2' 'Valor(US$)',
          'CAMBIO' 'Tx. Câmbio',
          'SGTXT' 'Texto Contábil',
          'GSBER' 'Filial',
          'NAME1' 'Nome Filial',
          'USNAM' 'Usuário',
          'CPUDT' 'Dt. Criação',
          'CPUTM' 'Hora Criação',
          'TCODE' 'Transação'.

  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = SY-REPID
      I_CALLBACK_TOP_OF_PAGE  = 'TOP_OF_PAGE'
"     I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      IT_FIELDCAT             = IT_FCAT
      IS_LAYOUT               = WA_LAY
    TABLES
      T_OUTTAB                = V_TABELA "IT_DADOS
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT  USING    P_CAMPO
                              P_DESC.

  DATA: WA_FCAT TYPE SLIS_FIELDCAT_ALV.

  CASE P_CAMPO.
    WHEN 'BELNR'.
      WA_FCAT-HOTSPOT = 'X'.
    WHEN 'BELNR2'.
      WA_FCAT-HOTSPOT = 'X'.
    WHEN 'EBELN'.
      WA_FCAT-HOTSPOT = 'X'.
    WHEN 'VBELN'.
      WA_FCAT-HOTSPOT = 'X'.
    WHEN 'VBEL2'.
      WA_FCAT-HOTSPOT = 'X'.
    WHEN 'XTOTVR'.
      WA_FCAT-HOTSPOT = 'X'.
      WA_FCAT-DO_SUM = 'X'.
    WHEN 'XTOTVUS'.
      WA_FCAT-HOTSPOT = 'X'.
      WA_FCAT-DO_SUM = 'X'.
    WHEN 'XTOTNVR'.
      WA_FCAT-HOTSPOT = 'X'.
      WA_FCAT-DO_SUM = 'X'.
    WHEN 'XTOTNVUS'.
      WA_FCAT-HOTSPOT = 'X'.
      WA_FCAT-DO_SUM = 'X'.
    WHEN 'TOTRS'.
      WA_FCAT-HOTSPOT = 'X'.
      WA_FCAT-DO_SUM = 'X'.
      WA_FCAT-EMPHASIZE = 'X'.
    WHEN 'TOTUS'.
      WA_FCAT-HOTSPOT = 'X'.
      WA_FCAT-DO_SUM = 'X'.
      WA_FCAT-EMPHASIZE = 'X'.
    WHEN 'DMBTR'.
      WA_FCAT-DO_SUM = 'X'.
    WHEN 'DMBE2'.
      WA_FCAT-DO_SUM = 'X'.
  ENDCASE.

  WA_FCAT-FIELDNAME = P_CAMPO.
  WA_FCAT-SELTEXT_M = P_DESC.

  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING UCOMM LIKE SY-UCOMM
                        SELFIELD TYPE KKBLO_SELFIELD. "Formulário para captura de evento de usuário

  DATA: WA_SDADOS TYPE LI_DADOS.

  CASE UCOMM.
    WHEN '&IC1'.
      READ TABLE V_TABELA INTO WA_SDADOS INDEX SELFIELD-TABINDEX.
      IF SELFIELD-FIELDNAME = 'BELNR'.
        SET PARAMETER ID 'BLN' FIELD WA_SDADOS-BELNR.
        SET PARAMETER ID 'BUK' FIELD WA_SDADOS-BUKRS.
        SET PARAMETER ID 'GJR' FIELD WA_SDADOS-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSEIF SELFIELD-FIELDNAME = 'BELNR2'.
        SET PARAMETER ID 'RBN' FIELD WA_SDADOS-BELNR2.
        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
      ELSEIF SELFIELD-FIELDNAME = 'EBELN'.
        SET PARAMETER ID 'BES' FIELD WA_SDADOS-EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ELSEIF SELFIELD-FIELDNAME = 'VBELN'.
        SET PARAMETER ID 'VF ' FIELD WA_SDADOS-VBELN.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ELSEIF SELFIELD-FIELDNAME = 'VBEL2'.
        SET PARAMETER ID 'AUN' FIELD WA_SDADOS-VBEL2.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.

  " Declarações locais do cabeçalho do ALV

  DATA: IT_HEADER TYPE SLIS_T_LISTHEADER,
        WA_HEADER TYPE SLIS_LISTHEADER,
        T_LINE    LIKE WA_HEADER-INFO.

  "  Titulo ( H )
  " Vai ser a linha que mais chama a atenção dentro do Cabeçalho, pois será
  " maior e em negrito.

  WA_HEADER-TYP  = 'H'.
  IF P_TCO_K = 'X'.
    WA_HEADER-INFO = 'Aging List - Contas a Pagar - Partidas Individuais'.
  ELSE.
    WA_HEADER-INFO = 'Aging List - Contas a Receber - Partidas Individuais'.
  ENDIF.
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR WA_HEADER.

  " Informações ( S )
  "  Como se fosse o texto para comentários, descrição de algo, somente no
  " tipo 'S' o campo 'KEY' faz algo, ele serve como inicio da sua frase e fica
  " em negrito se diferenciando do resto da linha.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Empresa: '.
  WA_HEADER-INFO = P_EMP." User name
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  DATA: ANO  TYPE STRING,
        MES  TYPE STRING,
        DIA  TYPE STRING,
        DATA TYPE STRING.

  ANO = SUBSTRING( VAL = P_PAR OFF = 0 LEN = 4 ).
  MES = SUBSTRING( VAL = P_PAR OFF = 4 LEN = 2 ).
  DIA = SUBSTRING( VAL = P_PAR OFF = 6 LEN = 2 ).
  CONCATENATE DIA '/' MES '/' ANO INTO DATA.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Ptdas em aberto até: '.
  WA_HEADER-INFO = DATA." User name
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

*  WA_HEADER-TYP  = 'S'.
*  WA_HEADER-KEY = 'Moeda: '.
*  WA_HEADER-INFO = P_MOE." User name
*  APPEND WA_HEADER TO IT_HEADER.
*  CLEAR: WA_HEADER.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Tipo de Conta: '.
  WA_HEADER-INFO = P_TCO." User name
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  DATA: LIN    TYPE I,
        CODIGO TYPE STRING,
        CONTA  TYPE STRING.
  DESCRIBE TABLE R_COD LINES LIN.

  IF LIN = 1.
    READ TABLE R_COD WITH KEY OPTION = 'EQ'.
    IF SY-SUBRC = 0.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = R_COD-LOW
        IMPORTING
          OUTPUT = CODIGO.
    ELSE.
      CODIGO = 'Múltiplas Seleções'.
    ENDIF.
  ELSEIF LIN = 0.
    CODIGO = ''.
  ELSE.
    CODIGO = 'Múltiplas Seleções'.
  ENDIF.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Código: '.
  WA_HEADER-INFO = CODIGO." User name
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  DESCRIBE TABLE R_CON LINES LIN.

  IF LIN = 1.
    READ TABLE R_CON WITH KEY OPTION = 'EQ'.
    IF SY-SUBRC = 0.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = R_CON-LOW
        IMPORTING
          OUTPUT = CONTA.
    ELSE.
      CONTA = 'Múltiplas Seleções'.
    ENDIF.
  ELSEIF LIN = 0.
    CONTA = ''.
  ELSE.
    CONTA = 'Múltiplas Seleções'.
  ENDIF.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Conta Razão: '.
  WA_HEADER-INFO = CONTA." User name
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER
*     I_LOGO             =
*     I_END_OF_LIST_GRID =
*     i_alv_form         = 'X'
    .

  "  Ai está a função que vai junta tudo isso e mostra no inicio da página,
  " podemos até adicionar uma imagem que esteja no banco SAP ( OAER ), Mas
  " isso é assunto para um próximo Post,
  " Qualquer dúvida estamos aih.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PF_STATUS_SET .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PRINT_ALV_AGR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM PRINT_ALV_AGR .

  DATA: WA_LAY TYPE SLIS_LAYOUT_ALV.

  WA_LAY-COLWIDTH_OPTIMIZE = 'X'. "Auto-ajuste das colunas do fieldcat
  REFRESH IT_FCAT.

  IF PP_AGR_V = 'X' AND PP_AGR_A = ''.
    PERFORM BUILD_FIELDCAT USING:
      'BUKRS'   'Empresa',
      'CLASS_T' 'Divisão em Dias',
      'XTOTVR' 'Vencidas (R$)',
      'XTOTVUS' 'Vencidas(US$)'.
  ELSEIF PP_AGR_A = 'X' AND PP_AGR_V = ''.
    PERFORM BUILD_FIELDCAT USING:
      'BUKRS'   'Empresa',
      'CLASS_T' 'Divisão em Dias',
      'XTOTNVR' 'A Vencer (R$)',
      'XTOTNVUS' 'A Vencer (US$)'.
  ELSE.
    PERFORM BUILD_FIELDCAT USING:
      'BUKRS'   'Empresa',
      'CLASS_T' 'Divisão em Dias',
      'XTOTVR' 'Vencidas (R$)',
      'XTOTNVR' 'A Vencer (R$)',
      'TOTRS' 'Total Partidas (R$)',
      'XTOTVUS' 'Vencidas (US$)',
      'XTOTNVUS' 'A Vencer (US$)',
      'TOTUS' 'Total Partidas (US$)'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = SY-REPID
      I_CALLBACK_TOP_OF_PAGE  = 'TOP_OF_PAGE_AGR'
"     I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND_AGR'
      IT_FIELDCAT             = IT_FCAT
      IS_LAYOUT               = WA_LAY
    TABLES
      T_OUTTAB                = IT_CLASS
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_AGR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM USER_COMMAND_AGR USING UCOMM LIKE SY-UCOMM
                            SELFIELD TYPE KKBLO_SELFIELD."Formulário para captura de evento de usuário

  DATA: WA_SDADOS TYPE LI_DADOS.

  CASE UCOMM.
    WHEN '&IC1'.
      READ TABLE IT_CLASS INTO WA_CLASS INDEX SELFIELD-TABINDEX.
      CLEAR V_TABELA.
      V_TABELA = IT_DADOS.

      IF SELFIELD-FIELDNAME = 'XTOTVR' OR SELFIELD-FIELDNAME = 'XTOTVUS'.
        DELETE V_TABELA
          WHERE CLASS <> WA_CLASS-CLASS
          OR XDTVCTO2 >= 0
          OR BUKRS <> WA_CLASS-BUKRS.
      ELSEIF SELFIELD-FIELDNAME = 'XTOTNVR' OR SELFIELD-FIELDNAME = 'XTOTNVUS'.
        DELETE V_TABELA
          WHERE CLASS <> WA_CLASS-CLASS
          OR XDTVCTO2 < 0
          OR BUKRS <> WA_CLASS-BUKRS.
      ELSEIF SELFIELD-FIELDNAME = 'TOTRS' OR SELFIELD-FIELDNAME = 'TOTUS'.
        DELETE V_TABELA
          WHERE CLASS <> WA_CLASS-CLASS
          OR BUKRS <> WA_CLASS-BUKRS.
      ENDIF.

  ENDCASE.
  PERFORM PRINT_ALV.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE_AGR.

  " Declarações locais do cabeçalho do ALV

  DATA: IT_HEADER     TYPE SLIS_T_LISTHEADER,
        WA_HEADER     TYPE SLIS_LISTHEADER,
        T_LINE        LIKE WA_HEADER-INFO,
        LD_LINES      TYPE I,
        LD_LINESC(10) TYPE C.

  "  Titulo ( H )
  " Vai ser a linha que mais chama a atenção dentro do Cabeçalho, pois será
  " maior e em negrito.

  WA_HEADER-TYP  = 'H'.
  IF P_TCO_K = 'X'.
    WA_HEADER-INFO = 'Aging List - Conta a Pagar Abertura Partis - Dias Venc.'.
  ELSE.
    WA_HEADER-INFO = 'Aging List - Conta a Receber Abertura Partis - Dias Venc.'.
  ENDIF.
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR WA_HEADER.

  " Informações ( S )
  "  Como se fosse o texto para comentários, descrição de algo, somente no
  " tipo 'S' o campo 'KEY' faz algo, ele serve como inicio da sua frase e fica
  " em negrito se diferenciando do resto da linha.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Empresa: '.
  WA_HEADER-INFO = P_EMP.
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  DATA: ANO  TYPE STRING,
        MES  TYPE STRING,
        DIA  TYPE STRING,
        DATA TYPE STRING.

  ANO = SUBSTRING( VAL = P_PAR OFF = 0 LEN = 4 ).
  MES = SUBSTRING( VAL = P_PAR OFF = 4 LEN = 2 ).
  DIA = SUBSTRING( VAL = P_PAR OFF = 6 LEN = 2 ).
  CONCATENATE DIA '/' MES '/' ANO INTO DATA.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Ptdas em aberto até: '.
  WA_HEADER-INFO = DATA.
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

*  WA_HEADER-TYP  = 'S'.
*  WA_HEADER-KEY = 'Moeda: '.
*  WA_HEADER-INFO = P_MOE.
*  APPEND WA_HEADER TO IT_HEADER.
*  CLEAR: WA_HEADER.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Tipo de Conta: '.
  WA_HEADER-INFO = P_TCO.
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  DATA: LIN    TYPE I,
        CODIGO TYPE STRING,
        CONTA  TYPE STRING.
  DESCRIBE TABLE R_COD LINES LIN.

  IF LIN = 1.
    READ TABLE R_COD WITH KEY OPTION = 'EQ'.
    IF SY-SUBRC = 0.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = R_COD-LOW
        IMPORTING
          OUTPUT = CODIGO.
    ELSE.
      CODIGO = 'Múltiplas Seleções'.
    ENDIF.
  ELSEIF LIN = 0.
    CODIGO = ''.
  ELSE.
    CODIGO = 'Múltiplas Seleções'.
  ENDIF.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Código: '.
  WA_HEADER-INFO = CODIGO.
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  DESCRIBE TABLE R_CON LINES LIN.

  IF LIN = 1.
    READ TABLE R_CON WITH KEY OPTION = 'EQ'.
    IF SY-SUBRC = 0.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = R_CON-LOW
        IMPORTING
          OUTPUT = CONTA.
    ELSE.
      CONTA = 'Múltiplas Seleções'.
    ENDIF.
  ELSEIF LIN = 0.
    CONTA = ''.
  ELSE.
    CONTA = 'Múltiplas Seleções'.
  ENDIF.

  WA_HEADER-TYP  = 'S'.
  WA_HEADER-KEY = 'Conta Razão: '.
  WA_HEADER-INFO = CONTA.
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER
*     I_LOGO             =
*     I_END_OF_LIST_GRID =
*     i_alv_form         = 'X'
    .

  "  Ai está a função que vai junta tudo isso e mostra no inicio da página,
  " podemos até adicionar uma imagem que esteja no banco SAP ( OAER ), Mas
  " isso é assunto para um próximo Post,
  " Qualquer dúvida estamos aih.

ENDFORM.
