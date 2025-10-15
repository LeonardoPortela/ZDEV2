*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo R Tavares                                       &*
*& Data.....: 15/07/2014                                              &*
*& Descrição: Atualização de Partidas Moeda Funcional / Estrangeira   &*
*& Transação: ZFI0063                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         14.07.2014                            &*
*&--------------------------------------------------------------------&*

REPORT  ZFIR051.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.
TABLES: ZFIT0084.
INCLUDE <ICON>.

TYPES: BEGIN OF TY_SETLEAF.
         INCLUDE TYPE SETLEAF.
TYPES:   KTOKS TYPE SKA1-KTOKS,
       END OF TY_SETLEAF,

       BEGIN OF TY_BKPF,
         BUKRS TYPE BKPF-BUKRS,
         BELNR TYPE BKPF-BELNR,
         GJAHR TYPE BKPF-GJAHR,
         BUDAT TYPE BKPF-BUDAT,
         STBLG TYPE BKPF-STBLG,
         STJAH TYPE BKPF-STJAH,
       END OF TY_BKPF,

       BEGIN OF TY_BSIK_BSID,
         BUKRS TYPE BSIK-BUKRS,
         BELNR TYPE BSIK-BELNR,
         BUZEI TYPE BSIK-BUZEI,
         GJAHR TYPE BSIK-GJAHR,
         BUDAT TYPE BSIK-BUDAT,
         HKONT TYPE BSIK-HKONT,
         WAERS TYPE BSIK-WAERS,
         MONAT TYPE BSIK-MONAT,
         DMBTR TYPE BSIK-DMBTR,
         DMBE2 TYPE BSIK-DMBE2,
         DMBE3 TYPE BSIK-DMBE3,
         LIFNR TYPE BSIK-LIFNR,
         XBLNR TYPE BSIK-XBLNR,
         BLART TYPE BSIK-BLART,
         DOC   TYPE BSIK-EBELN,
         ITEM  TYPE BSIK-EBELP,
         UMSKS TYPE BSIK-UMSKS,
         UMSKZ TYPE BSIK-UMSKZ,
         SHKZG TYPE BSIK-SHKZG,
         BSCHL TYPE BSIK-BSCHL,
         AUGBL TYPE BSIK-AUGBL,
       END OF TY_BSIK_BSID,

       BEGIN OF TY_BSIK_BSID_AUX,
         BUKRS TYPE BSIK-BUKRS,
         BELNR TYPE BSIK-BELNR,
         BUZEI TYPE BSIK-BUZEI,
         GJAHR TYPE BSIK-GJAHR,
         BUDAT TYPE BSIK-BUDAT,
         HKONT TYPE BSIK-HKONT,
         WAERS TYPE BSIK-WAERS,
         MONAT TYPE BSIK-MONAT,
         DMBTR TYPE BSIK-BDIF2,
         DMBE2 TYPE BSIK-BDIF2,
         DMBE3 TYPE BSIK-BDIF2,
         LIFNR TYPE BSIK-LIFNR,
         XBLNR TYPE BSIK-XBLNR,
         BLART TYPE BSIK-BLART,
         DOC   TYPE BSIK-EBELN,
         ITEM  TYPE BSIK-EBELP,
         UMSKS TYPE BSIK-UMSKS,
         UMSKZ TYPE BSIK-UMSKZ,
         SHKZG TYPE BSIK-SHKZG,
         BSCHL TYPE BSIK-BSCHL,
       END OF TY_BSIK_BSID_AUX,

       BEGIN OF TY_SAIDA,
         MARK,
         KUNNR           TYPE BSID-KUNNR,
         NAME1           TYPE KNA1-NAME1,
         HKONT           TYPE BSID-HKONT,
         TXT50           TYPE SKAT-TXT50,
         BELNR2          TYPE BSIK-BELNR,
         BUZEI           TYPE BSIK-BUZEI,
         GJAHR           TYPE BSIK-GJAHR,
         WAERS           TYPE BSIK-WAERS,
         BUDAT           TYPE BSIK-BUDAT,
         BLART           TYPE BSIK-BLART,
         XBLNR           TYPE BSIK-XBLNR,
         DOC             TYPE BSIK-EBELN,
         ITEM            TYPE BSIK-EBELP,
         DMBTR           TYPE BSIK-BDIF2,
         DMBE2           TYPE BSIK-DMBE2,
         DMBE3           TYPE BSIK-DMBE3,
         TX_USD          TYPE ZFIT0084-TX_USD,
         TX_BRL          TYPE ZFIT0084-TX_BRL,
         SALDO_CORR      TYPE BSIK-DMBTR,
         SALDO_CORR2     TYPE BSIK-DMBE2,
         VLR_AJUST       TYPE BSIK-DMBTR,
         VLR_AJUST2      TYPE BSIK-DMBE2,
         BSCHL           TYPE BSIK-BSCHL,
         UMSKZ           TYPE BSIK-UMSKZ,
         BELNR           TYPE ZIB_CONTABIL_CHV-BELNR,
         BELNR_EST       TYPE ZIB_CONTABIL_CHV-BELNR,
         OBJ_KEY         TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
         OBJ_KEY_EST     TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
         BELNR_INV       TYPE ZIB_CONTABIL_CHV-BELNR,
         BELNR_INV_EST   TYPE ZIB_CONTABIL_CHV-BELNR,
         OBJ_KEY_INV     TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
         OBJ_KEY_INV_EST TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
         VBUND           TYPE BSEG-VBUND,
         BEWAR           TYPE BSEG-BEWAR,
         STBLG           TYPE BKPF-STBLG, " USER STORY 155163 // MMSILVA - 15.10.2024
         LOG(4),
         LOG_INV(4),
         LINE_COLOR(4)   TYPE C,
       END OF TY_SAIDA,

       BEGIN OF TY_SAIDAR,
         MARK,
         RACCT         TYPE FAGLFLEXT-RACCT,
         RASSC         TYPE FAGLFLEXT-RASSC,
         TXT50         TYPE SKAT-TXT50,
         KTOKS         TYPE SKA1-KTOKS,
         CURR1         TYPE FAGLFLEXT-HSLVT,
         CURR2         TYPE FAGLFLEXT-KSLVT,
         CURR3         TYPE FAGLFLEXT-OSLVT,
         TX_USD        TYPE ZFIT0082-TX_USD,
         TX_BRL        TYPE ZFIT0082-TX_BRL,
         SALDO_CORR    TYPE FAGLFLEXT-HSLVT,
         SALDO_CORR2   TYPE FAGLFLEXT-HSLVT,
         VLR_AJUST     TYPE FAGLFLEXT-KSLVT,
         VLR_AJUST2    TYPE FAGLFLEXT-KSLVT,
         BELNR         TYPE ZIB_CONTABIL_CHV-BELNR,
         BELNR_EST     TYPE ZIB_CONTABIL_CHV-BELNR,
         OBJ_KEY       TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
         OBJ_KEY_EST   TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
         STBLG         TYPE BKPF-STBLG, " USER STORY 155163 // MMSILVA - 15.10.2024
         LOG(4),
         LINE_COLOR(4) TYPE C,
       END OF TY_SAIDAR,

       BEGIN OF TY_SAIDA_EXEC,
         ICON(4),
         TIPO    TYPE C LENGTH 30,
         LIFNR   TYPE LFA1-LIFNR,
         NAME1   TYPE LFA1-NAME1,
         HKONT   TYPE BSIK-HKONT,
         RACCT   TYPE FAGLFLEXT-RACCT,
         TXT50   TYPE SKAT-TXT50,
         MSG(80),
       END OF TY_SAIDA_EXEC,

       BEGIN OF TY_SAKNR,
         SAKNR TYPE SKA1-SAKNR,
       END OF TY_SAKNR.

TYPES: BEGIN OF TY_ESTRUTURA.
         INCLUDE TYPE SLIS_FIELDCAT_MAIN.
         INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

TYPES: BEGIN OF TY_FAGLFLEXT, "       PSA - DEVK9A2CGB
         RASSC   TYPE RASSC,
         RACCT   TYPE RACCT,
         RBUKRS  TYPE BUKRS,
         SOCPARC TYPE CHAR1,
       END OF TY_FAGLFLEXT,

       BEGIN OF TY_acdoca, "       PSA - DEVK9A2CGB
         "RASSC   TYPE RASSC,
         RACCT   TYPE RACCT,
         RBUKRS  TYPE BUKRS,
         SOCPARC TYPE CHAR1,
       END OF TY_acdoca.



*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: TG_SETLEAF       TYPE TABLE OF TY_SETLEAF,
      TG_SETLEAF_02    TYPE TABLE OF TY_SETLEAF,
      TG_T882G         TYPE TABLE OF T882G,
      TG_T001          TYPE TABLE OF T001,
      TG_SKA1          TYPE TABLE OF SKA1,
      TG_SKAT          TYPE TABLE OF SKAT,
      TG_TCURR         TYPE TABLE OF TCURR,
      TG_ZFIT0185      TYPE TABLE OF ZFIT0185,
      TG_TCURR_LDAY    TYPE TABLE OF TCURR,
      TG_ZFIT0185_LDAY TYPE TABLE OF ZFIT0185,
      TG_0084          TYPE TABLE OF ZFIT0084,
      TG_0084_AUX      TYPE TABLE OF ZFIT0084,
      TG_0081          TYPE TABLE OF ZFIT0081,
      TG_LFA1          TYPE TABLE OF LFA1,
      TG_KNA1          TYPE TABLE OF KNA1,
      TG_BSIK_BSID     TYPE TABLE OF TY_BSIK_BSID,
      TG_BSIK_BSID_AUX TYPE TABLE OF TY_BSIK_BSID,
      TG_BKPF_LANC     TYPE TABLE OF BKPF,               " Lançamento
      TG_BKPF_EST      TYPE TABLE OF BKPF,               " Estorno
      TG_ZIB           TYPE TABLE OF ZIB_CONTABIL,
      TG_ZIB_CHV       TYPE TABLE OF ZIB_CONTABIL_CHV,
      TG_ZIB_ERR       TYPE TABLE OF ZIB_CONTABIL_ERR,
      T_HKONT          TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
      TG_BSEG          TYPE TABLE OF BSEG,
      TG_SAIDA         TYPE TABLE OF TY_SAIDA,
      TG_SAIDA_AUX     TYPE TABLE OF TY_SAIDA,
      TG_SAIDA_RES     TYPE TABLE OF TY_SAIDA,
      TG_SAIDA_EXEC    TYPE TABLE OF TY_SAIDA_EXEC,
      IT_SAKNR         TYPE TABLE OF TY_SAKNR,
      WA_SAKNR         TYPE TY_SAKNR.

DATA:
  TG_SKB1       TYPE TABLE OF SKB1,
  WG_SKB1       TYPE SKB1, "
  TG_0082       TYPE TABLE OF ZFIT0082,
  TG_0082_AUX   TYPE TABLE OF ZFIT0082,
  TG_FAGLFLEXT  TYPE TABLE OF TY_FAGLFLEXT, "faglflext, PSA
  TG_FAGLFLEXTG LIKE TG_FAGLFLEXT, "faglflext, PSA
  WG_FAGLFLEXT  TYPE TY_FAGLFLEXT,
  WG_ACDOCA     TYPE TY_ACDOCA,
  TG_SAIDAR     TYPE TABLE OF TY_SAIDAR,
  TG_SAIDAR_AUX TYPE TABLE OF TY_SAIDAR,
  TG_ACDOCA     TYPE TABLE OF TY_acdoca,
  WG_SAIDAR     TYPE TY_SAIDAR,
  WG_0082       TYPE ZFIT0082,
  WL_INPUT_0082 TYPE ZFIT0082,
  GS_VARIANT_C  TYPE DISVARIANT,
  LR_WAERS      TYPE WAERS,
  V_TITLE       TYPE STRING.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WG_SETLEAF       TYPE TY_SETLEAF,
      WG_SETLEAF_02    TYPE TY_SETLEAF,
      WG_T882G         TYPE T882G,
      WG_T001          TYPE T001,
      WG_SKA1          TYPE SKA1,
      WG_SKAT          TYPE SKAT,
      WG_0084          TYPE ZFIT0084,
      WG_TCURR         TYPE TCURR,
      WG_ZFIT0185      TYPE ZFIT0185,
      WG_TCURR_LDAY    TYPE TCURR,
      WG_ZFIT0185_LDAY TYPE ZFIT0185,
      WG_LFA1          TYPE LFA1,
      WG_KNA1          TYPE KNA1,
      WG_0081          TYPE ZFIT0081,
      WG_ZIB_CHV       TYPE ZIB_CONTABIL_CHV,
      WG_ZIB_ERR       TYPE ZIB_CONTABIL_ERR,
      WG_BSIK_BSID     TYPE TY_BSIK_BSID,
      WG_BKPF          TYPE BKPF,
      WG_BSEG          TYPE BSEG,
      WG_BKPF_FB08     TYPE TY_BKPF,
      WG_BKPF_FB08_E   TYPE TY_BKPF,
      WG_BKPFE         TYPE BKPF,
      WG_BSIK_BSID_AUX TYPE TY_BSIK_BSID_AUX,
      WG_SAIDA         TYPE TY_SAIDA,
      WG_SAIDA_EXEC    TYPE TY_SAIDA_EXEC.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      V_CARGA(1),
      T_TOP        TYPE SLIS_T_LISTHEADER,
      LT_SORT      TYPE SLIS_T_SORTINFO_ALV,
      LS_SORT      TYPE SLIS_SORTINFO_ALV,
      TABIX        TYPE SY-TABIX,
      TABIX2       TYPE SY-TABIX.
*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
DATA: VG_RYEAR            TYPE FAGLFLEXT-RYEAR,
      VG_LAST_DAY         TYPE SY-DATUM,
      VG_FIRST_DAY        TYPE SY-DATUM,
      W_ANSWER(1),
      VG_LAST_DAY_AUX(8),
      VG_LAST_DAY_AUX2(8),
      V_VBUND             TYPE BSEG-VBUND,
      V_BEWAR             TYPE BSEG-BEWAR,
      V_SAKNR             TYPE ZFIT0081-SAKNR,
      E_STATUS(1),
      E_MESSA(64).


DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.

DATA: TI_BDCDATA       TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      WA_BDCDATA       LIKE LINE OF TI_BDCDATA,
      T_MESSTAB        TYPE TABLE OF BDCMSGCOLL,
      VOBJ_KEY         TYPE ZIB_CONTABIL_ERR-OBJ_KEY,
      WL_MESSAGE       TYPE PMST_RAW_MESSAGE,
      WG_DOCUMENTO(10),
      WL_MODE(1),
      IT_SELECTION     TYPE TABLE OF RSPARAMS,
      WA_SELECTION     LIKE LINE OF IT_SELECTION.
*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: C_X(1) TYPE C VALUE 'X'.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE ACAO.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      FUNCTIONCODE           = &1
    EXCEPTIONS
      FUNCTION_NOT_SUPPORTED = 1.

END-OF-DEFINITION.
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS
      ON_BUTTON_CLICK FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
        IMPORTING ES_COL_ID ES_ROW_NO.
  PRIVATE SECTION.
ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
* TELA DE SELECAO.
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-H01.


  SELECT-OPTIONS: S_BUKRS FOR ZFIT0084-BUKRS OBLIGATORY NO INTERVALS NO-EXTENSION,
                  S_MES   FOR ZFIT0084-MES_ANO NO INTERVALS NO-EXTENSION OBLIGATORY.
  PARAMETERS:
    R_AV_F RADIOBUTTON GROUP RAD1 USER-COMMAND ACT DEFAULT 'X' MODIF ID B,
    R_AV_E RADIOBUTTON GROUP RAD1 MODIF ID B.
  SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-H02.



    PARAMETERS: P_CLIE RADIOBUTTON GROUP G1 USER-COMMAND ACT.
    SELECT-OPTIONS S_SAKNRC FOR ZFIT0084-SAKNR.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(1) TEXT-SU3.
    SELECTION-SCREEN END OF LINE.

    PARAMETERS: P_FORN RADIOBUTTON GROUP G1.
    SELECT-OPTIONS S_SAKNR FOR ZFIT0084-SAKNR.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(1) TEXT-SU3.
    SELECTION-SCREEN END OF LINE.

    PARAMETERS: P_RAZ RADIOBUTTON GROUP G1.
    SELECT-OPTIONS S_SAKNRS FOR ZFIT0084-SAKNR.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(1) TEXT-SU3.
    SELECTION-SCREEN END OF LINE.

    PARAMETERS: P_HKONT2 RADIOBUTTON GROUP G1 MODIF ID A.
    SELECT-OPTIONS: S_HKONT2 FOR ZFIT0084-SAKNR MODIF ID A.

  SELECTION-SCREEN: END OF BLOCK B2.
  SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-H02.


    PARAMETERS: P_PROC RADIOBUTTON GROUP G2,
                P_REVE RADIOBUTTON GROUP G2,
                P_VISU RADIOBUTTON GROUP G2.

  SELECTION-SCREEN: END OF BLOCK B3.
SELECTION-SCREEN: END OF BLOCK B1.

INITIALIZATION.
  SELECT SINGLE *
     FROM USR05
     INTO @DATA(_USR05)
     WHERE BNAME = @SY-UNAME
     AND PARID   = 'BUK'.
  IF SY-SUBRC = 0.
    S_BUKRS-SIGN    = 'I'.
    S_BUKRS-OPTION  = 'EQ'.
    S_BUKRS-LOW = _USR05-PARVA+0(4).
    APPEND S_BUKRS.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SY-TCODE NE 'ZFI0108'.
      IF SCREEN-GROUP1 = 'B'.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ENDIF.
    IF R_AV_E = 'X'.
      IF SCREEN-GROUP1 = 'A'.
        SCREEN-ACTIVE = 1.
      ENDIF.
    ELSE.
      IF SCREEN-GROUP1 = 'A'.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
  IF P_FORN = 'X'.
    REFRESH:  S_SAKNRC, S_SAKNRS, S_HKONT2.
  ELSEIF P_CLIE = 'X'.
    REFRESH: S_SAKNR, S_SAKNRS, S_HKONT2.
  ELSEIF P_RAZ = 'X'.
    REFRESH: S_SAKNR, S_SAKNRC, S_HKONT2.
  ELSE.
    REFRESH: S_SAKNR, S_SAKNRC,S_SAKNRS.
  ENDIF.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD ON_BUTTON_CLICK.
    DATA: TL_TEXTO    TYPE CATSXT_LONGTEXT_ITAB,
          WL_TEXTO    TYPE LINE OF CATSXT_LONGTEXT_ITAB,
          IT_ZIB_ERR  TYPE TABLE OF ZIB_CONTABIL_ERR,
          WL_TEXT     TYPE SYTITLE,
          WL_TEXT_EST TYPE SYTITLE.

    REFRESH: TL_TEXTO.
    CLEAR: WL_TEXTO.
    IF ES_COL_ID EQ 'LOG'.
      IF TG_SAIDAR[] IS NOT INITIAL.
        READ TABLE TG_SAIDAR INTO WG_SAIDAR INDEX ES_ROW_NO-ROW_ID.
        WG_SAIDA-OBJ_KEY = WG_SAIDAR-OBJ_KEY.
      ELSE.
        READ TABLE TG_SAIDA INTO WG_SAIDA INDEX ES_ROW_NO-ROW_ID.
      ENDIF.
**       Contabilização
      IF WG_SAIDA-OBJ_KEY IS NOT INITIAL.
        SELECT *
          FROM ZIB_CONTABIL_ERR
          INTO TABLE IT_ZIB_ERR
          WHERE OBJ_KEY EQ WG_SAIDA-OBJ_KEY.

        LOOP AT IT_ZIB_ERR INTO WG_ZIB_ERR  WHERE OBJ_KEY EQ WG_SAIDA-OBJ_KEY.

          WL_TEXTO = WG_ZIB_ERR-MESSAGE.

          APPEND WL_TEXTO TO TL_TEXTO.
          CLEAR: WL_TEXTO.
        ENDLOOP.
        IF TL_TEXTO[] IS NOT INITIAL.
          WL_TEXT = TEXT-001.
          CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
            EXPORTING
              IM_TITLE        = WL_TEXT           "" Título
              IM_DISPLAY_MODE = C_X
            CHANGING
              CH_TEXT         = TL_TEXTO.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDMETHOD.                    "ON_BUTTON_CLICK
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

INITIALIZATION.       "to set titlebar on selection screen
  IF SY-TCODE = 'ZFI0108'.
    SY-TITLE = TEXT-064.
  ELSE.
    SY-TITLE = TEXT-065.
  ENDIF.

START-OF-SELECTION.


  IF R_AV_E = 'X'.
    VG_RYEAR  = S_MES-LOW+2(4).
    CONCATENATE VG_RYEAR S_MES-LOW(2) '01' INTO VG_LAST_DAY_AUX.
    VG_LAST_DAY = VG_LAST_DAY_AUX.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        I_DATE = VG_LAST_DAY
      IMPORTING
        E_DATE = VG_LAST_DAY.
    "
    REFRESH IT_SELECTION.
    "P_RAZ
    LOOP AT S_SAKNRS.
      WA_SELECTION-SELNAME = 'S_HKONT'.
      WA_SELECTION-KIND    = 'S'.
      WA_SELECTION-SIGN    = S_SAKNRS-SIGN.
      WA_SELECTION-OPTION  = S_SAKNRS-OPTION.
      WA_SELECTION-LOW     = S_SAKNRS-LOW.
      WA_SELECTION-HIGH    = S_SAKNRS-HIGH.
      APPEND WA_SELECTION TO IT_SELECTION.
    ENDLOOP.
    "
    "P_FORN
    LOOP AT S_SAKNR.
      WA_SELECTION-SELNAME = 'S_KONT'.
      WA_SELECTION-KIND    = 'S'.
      WA_SELECTION-SIGN    = S_SAKNR-SIGN.
      WA_SELECTION-OPTION  = S_SAKNR-OPTION.
      WA_SELECTION-LOW     = S_SAKNR-LOW.
      WA_SELECTION-HIGH    = S_SAKNR-HIGH.
      APPEND WA_SELECTION TO IT_SELECTION.
    ENDLOOP.

    "P_CLIE
    LOOP AT S_SAKNRC.
      WA_SELECTION-SELNAME = 'S_DONT'.
      WA_SELECTION-KIND    = 'S'.
      WA_SELECTION-SIGN    = S_SAKNRC-SIGN.
      WA_SELECTION-OPTION  = S_SAKNRC-OPTION.
      WA_SELECTION-LOW     = S_SAKNRC-LOW.
      WA_SELECTION-HIGH    = S_SAKNRC-HIGH.
      APPEND WA_SELECTION TO IT_SELECTION.
    ENDLOOP.

    "P_HKONT2
    LOOP AT S_HKONT2.
      WA_SELECTION-SELNAME = 'S_DONT'.
      WA_SELECTION-KIND    = 'S'.
      WA_SELECTION-SIGN    = S_HKONT2-SIGN.
      WA_SELECTION-OPTION  = S_HKONT2-OPTION.
      WA_SELECTION-LOW     = S_HKONT2-LOW.
      WA_SELECTION-HIGH    = S_HKONT2-HIGH.
      APPEND WA_SELECTION TO IT_SELECTION.
    ENDLOOP.

    SUBMIT ZGL027 WITH SELECTION-TABLE IT_SELECTION
                  WITH P_BUKRS  = S_BUKRS-LOW
                  WITH P_BUDAT  = VG_LAST_DAY
                  WITH P_C_LANC = P_PROC
                  WITH P_E_LANC = P_REVE
                  WITH P_V_LANC = P_VISU
                  WITH P_HKONT  = P_RAZ
                  WITH P_LIFNR  = P_FORN
                  WITH P_KUNNR  = P_CLIE
                  WITH P_HKONT2 = P_HKONT2
    AND RETURN.

  ELSE.
    IF P_RAZ = 'X'.
      PERFORM SELECIONAR_DADOS_R.
    ELSE.
      PERFORM SELECIONAR_DADOS.
    ENDIF.


    PERFORM INICIAR_VARIAVEIS.
    IF P_RAZ IS INITIAL.
      PERFORM ORGANIZACAO_DADOS.
    ELSE.
      PERFORM ORGANIZACAO_DADOS_R.
    ENDIF.
    PERFORM IMPRIMIR_DADOS.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS.

  CLEAR: WG_T001,V_TITLE,LR_WAERS.
  READ TABLE TG_T001 INTO WG_T001 INDEX 1.

  IF WG_T001-BUKRS = '0004'.
    LR_WAERS = 'USD'.
  ELSE.
    LR_WAERS = WG_T001-WAERS.
  ENDIF.

  IF P_FORN IS NOT INITIAL.
    V_TITLE = TEXT-003. "PSA DEVK9A2CGB 124919
    "PERFORM f_construir_cabecalho USING 'H' TEXT-003.
  ELSEIF P_CLIE IS NOT INITIAL.
    V_TITLE = TEXT-004. "PSA DEVK9A2CGB 124919
    "PERFORM f_construir_cabecalho USING 'H' TEXT-004.
  ELSE.
    V_TITLE = TEXT-056. "PSA DEVK9A2CGB 124919
    "PERFORM f_construir_cabecalho USING 'H' TEXT-056.
  ENDIF.
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' V_TITLE.
ENDFORM.                    " INICIAR_VARIAVES                  " INICIAR_VARIAVES

FORM XUSER_COMMANDR USING UCOMM LIKE SY-UCOMM
                         SELFIELD TYPE KKBLO_SELFIELD.
  DATA: TL_ZIB      TYPE TABLE OF ZIB_CONTABIL WITH HEADER LINE,
        TL_ZIB_CHV  TYPE TABLE OF ZIB_CONTABIL_CHV WITH HEADER LINE,
        TL_ZIB_ERR  TYPE TABLE OF ZIB_CONTABIL_ERR WITH HEADER LINE,
        TL_SAIDA    TYPE TABLE OF TY_SAIDAR,
        WL_OBJ_KEY  TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
        WA_ZFIT0082 TYPE ZFIT0082,
        LV_SEQITEM  TYPE ZIB_CONTABIL-SEQITEM.

  REFRESH: TG_SAIDA_EXEC, TL_ZIB, TL_ZIB_CHV, TL_ZIB_ERR, TL_SAIDA.
  CLEAR:WL_OBJ_KEY.

  TL_SAIDA[]  = TG_SAIDAR[].
  DELETE TL_SAIDA WHERE OBJ_KEY IS INITIAL.
  IF TL_SAIDA[] IS NOT INITIAL.
    SELECT *
      FROM ZIB_CONTABIL
      INTO TABLE TL_ZIB
       FOR ALL ENTRIES IN TL_SAIDA
       WHERE OBJ_KEY EQ TL_SAIDA-OBJ_KEY.
  ENDIF.


  IF TL_ZIB[] IS NOT INITIAL.
    SELECT *
     FROM ZIB_CONTABIL_CHV
     INTO TABLE TL_ZIB_CHV
      FOR ALL ENTRIES IN TL_ZIB
      WHERE OBJ_KEY EQ TL_ZIB-OBJ_KEY.

    SELECT *
     FROM ZIB_CONTABIL_ERR
     INTO TABLE TL_ZIB_ERR
      FOR ALL ENTRIES IN TL_ZIB
      WHERE OBJ_KEY EQ TL_ZIB-OBJ_KEY.

  ENDIF.

  SORT: TL_ZIB     BY OBJ_KEY,
        TL_ZIB_CHV BY OBJ_KEY,
        TL_ZIB_ERR BY OBJ_KEY.

  CASE UCOMM.
    WHEN 'GERA'.

*      sort tg_saidar by  racct rassc.
*      free: tg_saidar_aux.
*      tg_saidar_aux = tg_saidar.
*      delete adjacent duplicates from tg_saidar_aux comparing racct.
*** Inicio - Rubenilson - 21.01.25 - 124919
      DATA(LT_SAIDAR) = TG_SAIDAR.
      SORT LT_SAIDAR BY RACCT.

      DATA(LT_SAIDAR2) = TG_SAIDAR.
      DELETE LT_SAIDAR2 WHERE MARK IS INITIAL.
      SORT LT_SAIDAR2 BY RACCT.
      DELETE ADJACENT DUPLICATES FROM LT_SAIDAR2 COMPARING RACCT.

*      LOOP AT tg_saidar INTO wg_saidar WHERE mark IS NOT INITIAL.
      LOOP AT LT_SAIDAR2 ASSIGNING FIELD-SYMBOL(<FS_SAIDAR>).

        READ TABLE LT_SAIDAR TRANSPORTING NO FIELDS
        WITH KEY RACCT = <FS_SAIDAR>-RACCT
        BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          LOOP AT LT_SAIDAR INTO WG_SAIDAR FROM SY-TABIX.
            IF <FS_SAIDAR>-RACCT <> WG_SAIDAR-RACCT.
              CLEAR LV_SEQITEM.
              EXIT.
            ENDIF.
*** Fim - Rubenilson - 21.01.25 - 124919

            IF WG_SAIDAR-OBJ_KEY IS NOT INITIAL AND WG_SAIDAR-BELNR IS INITIAL.
              READ TABLE TL_ZIB WITH KEY OBJ_KEY = WG_SAIDAR-OBJ_KEY BINARY SEARCH.
              IF TL_ZIB-RG_ATUALIZADO EQ 'N'.
                CLEAR: WG_SAIDA_EXEC.
                WG_SAIDA_EXEC-ICON   = ICON_YELLOW_LIGHT.
                WG_SAIDA_EXEC-RACCT  = WG_SAIDAR-RACCT.
                WG_SAIDA_EXEC-TXT50  = WG_SAIDAR-TXT50.
                WG_SAIDA_EXEC-MSG    = TEXT-M01 .
                APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
                CONTINUE.
              ELSEIF TL_ZIB-RG_ATUALIZADO EQ 'S'.
                READ TABLE TL_ZIB_CHV WITH KEY OBJ_KEY = WG_SAIDAR-OBJ_KEY BINARY SEARCH.
                IF SY-SUBRC IS INITIAL.
                  CLEAR: WG_SAIDA_EXEC.
                  WG_SAIDA_EXEC-ICON   = ICON_GREEN_LIGHT.
                  WG_SAIDA_EXEC-RACCT  = WG_SAIDAR-RACCT.
                  WG_SAIDA_EXEC-TXT50  = WG_SAIDAR-TXT50.
                  WG_SAIDA_EXEC-MSG    = TEXT-M02 .
                  APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
                  ACAO '&ATUAL'.
                  CONTINUE.
                ELSE.
                  READ TABLE TL_ZIB_ERR  WITH KEY OBJ_KEY = WG_SAIDAR-OBJ_KEY BINARY SEARCH.
                  IF SY-SUBRC IS INITIAL.
                    CLEAR: WG_SAIDA_EXEC.
                    WG_SAIDA_EXEC-ICON   = ICON_RED_LIGHT.
                    WG_SAIDA_EXEC-RACCT  = WG_SAIDAR-RACCT.
                    WG_SAIDA_EXEC-TXT50  = WG_SAIDAR-TXT50.
                    WG_SAIDA_EXEC-MSG    = TEXT-M03 .
                    APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.

                    CALL FUNCTION 'POPUP_TO_CONFIRM'
                      EXPORTING
                        TEXT_QUESTION         = TEXT-M04
*                       TEXT_BUTTON_1         = 'Sim'(100)
                        TEXT_BUTTON_1         = TEXT-B01
                        ICON_BUTTON_1         = 'ICON_OKAY'
*                       TEXT_BUTTON_2         = 'Não'(101)
                        TEXT_BUTTON_2         = TEXT-B02
                        ICON_BUTTON_2         = 'ICON_CANCEL'
                        DEFAULT_BUTTON        = '1'
                        DISPLAY_CANCEL_BUTTON = ' '
                        START_COLUMN          = 25
                        START_ROW             = 6
                      IMPORTING
                        ANSWER                = W_ANSWER
                      EXCEPTIONS
                        TEXT_NOT_FOUND        = 1
                        OTHERS                = 2.

                    IF SY-SUBRC <> 0.
                      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                    ENDIF.
                    IF W_ANSWER = '1'.
                      DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY = WG_SAIDAR-OBJ_KEY.
                      DELETE FROM ZIB_CONTABIL     WHERE OBJ_KEY = WG_SAIDAR-OBJ_KEY.
                      DELETE FROM ZFIT0082         WHERE OBJ_KEY = WG_SAIDAR-OBJ_KEY.
                    ELSE.
                      ACAO '&ATUAL'.
                      CONTINUE.
                    ENDIF.
                  ELSE.
                    CLEAR: WG_SAIDA_EXEC.
                    WG_SAIDA_EXEC-ICON   = ICON_YELLOW_LIGHT.
                    WG_SAIDA_EXEC-RACCT  = WG_SAIDAR-RACCT.
                    WG_SAIDA_EXEC-TXT50  = WG_SAIDAR-TXT50.
                    WG_SAIDA_EXEC-MSG    = TEXT-M01 .
                    APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
                    CONTINUE.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

            IF WG_SAIDAR-OBJ_KEY IS INITIAL.
              SELECT SINGLE *
                FROM ZFIT0082
                INTO WA_ZFIT0082
                WHERE BUKRS     = S_BUKRS-LOW
                AND   MES_ANO   = S_MES-LOW
                AND   SAKNR     = WG_SAIDAR-RACCT
                AND   VBUND     = WG_SAIDAR-RASSC
                AND   OBJ_KEY   NE ''.
              IF SY-SUBRC = 0.
                CONTINUE.
              ENDIF.

            ENDIF.

            IF WG_SAIDAR-VLR_AJUST = 0.
              CLEAR: WG_SAIDA_EXEC.
              WG_SAIDA_EXEC-ICON   = ICON_RED_LIGHT.
              WG_SAIDA_EXEC-RACCT  = WG_SAIDAR-RACCT.
              WG_SAIDA_EXEC-TXT50  = WG_SAIDAR-TXT50.
              WG_SAIDA_EXEC-MSG    = TEXT-M11 .
              APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
              CONTINUE.
            ENDIF.

            IF WG_SAIDAR-BELNR IS NOT INITIAL.
              CLEAR: WG_SAIDA_EXEC.
              WG_SAIDA_EXEC-ICON   = ICON_RED_LIGHT.
              WG_SAIDA_EXEC-RACCT  = WG_SAIDAR-RACCT.
              WG_SAIDA_EXEC-TXT50  = WG_SAIDAR-TXT50.
              WG_SAIDA_EXEC-MSG    = TEXT-M05 .
              APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
              CONTINUE.
            ENDIF.

            ADD 1 TO LV_SEQITEM." Rubenilson - 21.01.25 - 124919
            PERFORM GERA_CONTABILR USING WG_SAIDAR SPACE CHANGING WL_OBJ_KEY LV_SEQITEM." Rubenilson - 21.01.25 - 124919

          ENDLOOP.

          CLEAR WG_SAIDAR." Rubenilson - 21.01.25 - 124919
          WG_SAIDAR-OBJ_KEY = WL_OBJ_KEY.
*          CLEAR: wg_saidar-obj_key_est, wg_saidar-belnr_est." Rubenilson - 21.01.25 - 124919
          MODIFY TG_SAIDAR FROM WG_SAIDAR TRANSPORTING OBJ_KEY OBJ_KEY_EST BELNR_EST WHERE RACCT = <FS_SAIDAR>-RACCT." Rubenilson - 21.01.25 - 124919
          CLEAR: WL_OBJ_KEY." Rubenilson - 21.01.25 - 124919
        ENDIF.

      ENDLOOP.

      SELFIELD-COL_STABLE = C_X.
      SELFIELD-ROW_STABLE = C_X.
      SELFIELD-REFRESH = C_X.

      PERFORM IMPRIMIR_EXEC.
      ACAO '&ATUAL'.
    WHEN 'ESTORNO'.
      PERFORM ESTORNA_DOCUMENTOS USING 'EST'.
      SELFIELD-COL_STABLE = C_X.
      SELFIELD-ROW_STABLE = C_X.
      SELFIELD-REFRESH    = C_X.
      PERFORM IMPRIMIR_EXEC.
      ACAO '&ATUAL'." Rubenilson - 22.01.25 - 124919
    WHEN 'REVERTE'.
      PERFORM ESTORNA_DOCUMENTOS USING 'REV'.
      SELFIELD-COL_STABLE = C_X.
      SELFIELD-ROW_STABLE = C_X.
      SELFIELD-REFRESH    = C_X.
      PERFORM IMPRIMIR_EXEC.
    WHEN '&IC1'.
* Lê na tabela de saída
      READ TABLE TG_SAIDAR INTO WG_SAIDAR INDEX SELFIELD-TABINDEX.

      IF SY-SUBRC EQ 0.

        IF SELFIELD-FIELDNAME = 'BELNR'.
          IF WG_SAIDAR-BELNR IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD WG_SAIDAR-BELNR.
            SET PARAMETER ID 'GJR' FIELD WG_SAIDAR-OBJ_KEY+16(4).
            SET PARAMETER ID 'BUK' FIELD S_BUKRS-LOW.

            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.

        IF SELFIELD-FIELDNAME = 'STBLG'.
          IF WG_SAIDAR-STBLG IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD WG_SAIDAR-STBLG.
            SET PARAMETER ID 'GJR' FIELD WG_SAIDAR-OBJ_KEY+16(4).
            SET PARAMETER ID 'BUK' FIELD S_BUKRS-LOW.

            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '&ATUAL'.
      PERFORM ATUALIZA_SAIDAR TABLES TG_SAIDAR
                                    TL_ZIB
                                    TL_ZIB_CHV
                                    TL_ZIB_ERR.
  ENDCASE.

  SELFIELD-COL_STABLE = C_X.
  SELFIELD-ROW_STABLE = C_X.
  SELFIELD-REFRESH = C_X.
ENDFORM. "XUSER_COMMAND

*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XUSER_COMMAND USING UCOMM LIKE SY-UCOMM
                         SELFIELD TYPE KKBLO_SELFIELD.

  TYPES: BEGIN OF TY_DOCUMENTO,
           BUKRS TYPE BUKRS,
           BELNR TYPE BELNR_D,
           GJAHR TYPE GJAHR.
  TYPES: END OF TY_DOCUMENTO.

  DATA: TL_ZIB     TYPE TABLE OF ZIB_CONTABIL WITH HEADER LINE,
        TL_ZIB_CHV TYPE TABLE OF ZIB_CONTABIL_CHV WITH HEADER LINE,
        TL_ZIB_ERR TYPE TABLE OF ZIB_CONTABIL_ERR WITH HEADER LINE,
        TL_SAIDA   TYPE TABLE OF TY_SAIDA,
        VG_BELNR   TYPE TY_DOCUMENTO.

  REFRESH: TG_SAIDA_EXEC, TL_ZIB, TL_ZIB_CHV, TL_ZIB_ERR, TL_SAIDA.

  "Ajusta Documento de Variação """"""""""""""""""""""""""""""""""""""""""""""""""
  "*******************************************************************************
  TL_SAIDA[]  = TG_SAIDA[].
  DELETE TL_SAIDA WHERE OBJ_KEY IS INITIAL.
  IF TL_SAIDA[] IS NOT INITIAL.
    SELECT *
      FROM ZIB_CONTABIL
      INTO TABLE TL_ZIB
       FOR ALL ENTRIES IN TL_SAIDA
     WHERE OBJ_KEY EQ TL_SAIDA-OBJ_KEY.
  ENDIF.

  IF TL_ZIB[] IS NOT INITIAL.
    SELECT *
     FROM ZIB_CONTABIL_CHV
     INTO TABLE TL_ZIB_CHV
      FOR ALL ENTRIES IN TL_ZIB
      WHERE OBJ_KEY EQ TL_ZIB-OBJ_KEY.

    SELECT *
     FROM ZIB_CONTABIL_ERR
     INTO TABLE TL_ZIB_ERR
      FOR ALL ENTRIES IN TL_ZIB
      WHERE OBJ_KEY EQ TL_ZIB-OBJ_KEY.
  ENDIF.

  SORT: TL_ZIB     BY OBJ_KEY,
        TL_ZIB_CHV BY OBJ_KEY,
        TL_ZIB_ERR BY OBJ_KEY.

  CASE UCOMM.
    WHEN 'GERA'.
      IF P_PROC IS NOT INITIAL.
        PERFORM GERA_DOCUMENTOS TABLES TL_ZIB TL_ZIB_CHV TL_ZIB_ERR.
        SELFIELD-COL_STABLE = C_X.
        SELFIELD-ROW_STABLE = C_X.
        SELFIELD-REFRESH    = C_X.
        PERFORM IMPRIMIR_EXEC.
      ENDIF.

    WHEN 'ESTORNO'.
      PERFORM ESTORNA_DOCUMENTOS USING 'EST'.
      SELFIELD-COL_STABLE = C_X.
      SELFIELD-ROW_STABLE = C_X.
      SELFIELD-REFRESH    = C_X.
      PERFORM IMPRIMIR_EXEC.

    WHEN 'REVERTE'.
      PERFORM ESTORNA_DOCUMENTOS USING 'REV'.
      SELFIELD-COL_STABLE = C_X.
      SELFIELD-ROW_STABLE = C_X.
      SELFIELD-REFRESH    = C_X.
      PERFORM IMPRIMIR_EXEC.
    WHEN '&IC1'.
* Lê na tabela de saída
      READ TABLE TG_SAIDA INTO WG_SAIDA INDEX SELFIELD-TABINDEX.
      IF SY-SUBRC EQ 0.
        CLEAR: VG_BELNR.
        IF SELFIELD-FIELDNAME = 'BELNR'.
          IF WG_SAIDA-BELNR IS NOT INITIAL AND WG_SAIDA-BELNR+0(1) NE '@'.
            VG_BELNR-BUKRS = S_BUKRS-LOW.
            VG_BELNR-BELNR = WG_SAIDA-BELNR.
            VG_BELNR-GJAHR = WG_SAIDA-OBJ_KEY+16(4).
          ENDIF.
        ELSEIF SELFIELD-FIELDNAME = 'BELNR2'.
          IF WG_SAIDA-BELNR2 IS NOT INITIAL AND WG_SAIDA-BELNR2+0(1) NE '@'.
            VG_BELNR-BUKRS = S_BUKRS-LOW.
            VG_BELNR-BELNR = WG_SAIDA-BELNR2.
            VG_BELNR-GJAHR = WG_SAIDA-GJAHR.
          ENDIF.
        ELSEIF SELFIELD-FIELDNAME = 'BELNR_EST'.
          IF WG_SAIDA-BELNR_EST IS NOT INITIAL AND WG_SAIDA-BELNR_EST+0(1) NE '@'.
            VG_BELNR-BUKRS = S_BUKRS-LOW.
            VG_BELNR-BELNR = WG_SAIDA-BELNR_EST.
            VG_BELNR-GJAHR = WG_SAIDA-OBJ_KEY_EST+16(4).
          ENDIF.
        ELSEIF SELFIELD-FIELDNAME = 'BELNR_INV'.
          IF WG_SAIDA-BELNR_INV IS NOT INITIAL AND WG_SAIDA-BELNR_INV+0(1) NE '@'.
            VG_BELNR-BUKRS = S_BUKRS-LOW.
            VG_BELNR-BELNR = WG_SAIDA-BELNR_INV.
            VG_BELNR-GJAHR = WG_SAIDA-OBJ_KEY_INV+16(4).
          ENDIF.
        ELSEIF SELFIELD-FIELDNAME = 'BELNR_INV_EST'.
          IF WG_SAIDA-BELNR_INV_EST IS NOT INITIAL AND WG_SAIDA-BELNR_INV_EST+0(1) NE '@'.
            VG_BELNR-BUKRS = S_BUKRS-LOW.
            VG_BELNR-BELNR = WG_SAIDA-BELNR_INV_EST.
            VG_BELNR-GJAHR = WG_SAIDA-OBJ_KEY_INV_EST+16(4).
          ENDIF.
        ENDIF.

        IF VG_BELNR IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD VG_BELNR-BELNR.
          SET PARAMETER ID 'GJR' FIELD VG_BELNR-GJAHR.
          SET PARAMETER ID 'BUK' FIELD VG_BELNR-BUKRS.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          "USER STORY 155163 // MMSILVA - 24.10.2024 - Inicio
        ELSEIF SELFIELD-FIELDNAME = 'STBLG'.
          IF WG_SAIDA-STBLG IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD WG_SAIDA-STBLG.
            SET PARAMETER ID 'GJR' FIELD WG_SAIDA-OBJ_KEY+16(4).
            SET PARAMETER ID 'BUK' FIELD S_BUKRS-LOW.

            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
          "USER STORY 155163 // MMSILVA - 24.10.2024 - Fim
        ENDIF.

      ENDIF.

    WHEN '&ATUAL'.
      PERFORM ATUALIZA_SAIDA TABLES TG_SAIDA
                                    TL_ZIB
                                    TL_ZIB_CHV
                                    TL_ZIB_ERR.
  ENDCASE.

  SELFIELD-COL_STABLE = C_X.
  SELFIELD-ROW_STABLE = C_X.
  SELFIELD-REFRESH = C_X.
ENDFORM. "XUSER_COMMAND
*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XPF_STATUS_SET USING E_COMM.                           "#EC CALLED
  DATA: GR_EVENTS      TYPE REF TO LCL_EVENT_RECEIVER,
        CK_ATUALIZA(1).
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.
  DATA : LS_SEL_HIDE            TYPE SLIS_SEL_HIDE_ALV.
  DATA: REF1             TYPE REF TO CL_GUI_ALV_GRID,
        TL_FIELDCATALOG  TYPE LVC_T_FCAT,
        TL_FIELDCATALOG2 TYPE LVC_T_FCAT,
        WL_FIELDCATALOG  TYPE LVC_S_FCAT,
        WL_FIELDCATALOG2 TYPE LVC_S_FCAT,
        IS_TABLE         TYPE LVC_S_STBL.

  IF V_CARGA IS INITIAL.
    V_CARGA = 'X'.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        ES_SEL_HIDE = LS_SEL_HIDE
        E_GRID      = REF1.

    CREATE OBJECT GR_EVENTS.
    CALL METHOD REF1->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = TL_FIELDCATALOG.

    LOOP AT TL_FIELDCATALOG INTO WL_FIELDCATALOG.
      TABIX = SY-TABIX.
      READ TABLE ESTRUTURA INTO WA_ESTRUTURA WITH KEY FIELDNAME = WL_FIELDCATALOG-FIELDNAME.
      IF SY-SUBRC = 0.
        WL_FIELDCATALOG-COL_POS = WA_ESTRUTURA-COL_POS.
        MODIFY TL_FIELDCATALOG FROM WL_FIELDCATALOG INDEX TABIX.
      ENDIF.
    ENDLOOP.
    SORT TL_FIELDCATALOG BY COL_POS.

    CK_ATUALIZA  = ABAP_FALSE.

    READ TABLE TL_FIELDCATALOG INTO WL_FIELDCATALOG WITH KEY FIELDNAME = 'LOG'.
    IF SY-SUBRC IS INITIAL.
      WL_FIELDCATALOG-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
      MODIFY TL_FIELDCATALOG FROM WL_FIELDCATALOG INDEX SY-TABIX TRANSPORTING STYLE EDIT.
      CALL METHOD REF1->SET_FRONTEND_FIELDCATALOG
        EXPORTING
          IT_FIELDCATALOG = TL_FIELDCATALOG.
      IS_TABLE-ROW = 'X'.
      IS_TABLE-COL = 'X'.
      CK_ATUALIZA  = ABAP_TRUE.
    ENDIF.

    READ TABLE TL_FIELDCATALOG INTO WL_FIELDCATALOG WITH KEY FIELDNAME = 'LOG_INV'.
    IF SY-SUBRC IS INITIAL.
      WL_FIELDCATALOG-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
      MODIFY TL_FIELDCATALOG FROM WL_FIELDCATALOG INDEX SY-TABIX TRANSPORTING STYLE EDIT.
      CALL METHOD REF1->SET_FRONTEND_FIELDCATALOG
        EXPORTING
          IT_FIELDCATALOG = TL_FIELDCATALOG.
      IS_TABLE-ROW = 'X'.
      IS_TABLE-COL = 'X'.
      CK_ATUALIZA  = ABAP_TRUE.
    ENDIF.

    IF CK_ATUALIZA EQ ABAP_TRUE.
      CALL METHOD REF1->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE      = IS_TABLE
          I_SOFT_REFRESH = 'X'.
    ENDIF.

    SET HANDLER LCL_EVENT_RECEIVER=>ON_BUTTON_CLICK FOR REF1.
  ENDIF.

  IF P_VISU IS NOT INITIAL.
    APPEND 'GERA' TO FCODE.
    APPEND 'ESTORNO' TO FCODE.
    APPEND 'REVERTE' TO FCODE.
  ELSE.
    IF P_PROC IS INITIAL.
      APPEND 'GERA' TO FCODE.
    ENDIF.

    IF P_REVE IS INITIAL.
      APPEND 'REVERTE' TO FCODE.
    ELSE.
      APPEND 'ESTORNO' TO FCODE.
    ENDIF.
  ENDIF.



  SET PF-STATUS 'STATUS_UNI'  EXCLUDING FCODE.

ENDFORM. "XPF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.
  DATA: CABEC TYPE STRING.
  IF P_PROC = 'X'.
    CONCATENATE TEXT-066 TEXT INTO CABEC SEPARATED BY SPACE.
  ELSEIF P_REVE = 'X'.
    CONCATENATE TEXT-067 TEXT INTO CABEC SEPARATED BY SPACE.
  ELSE.
    CONCATENATE TEXT-068 TEXT INTO CABEC SEPARATED BY SPACE.
  ENDIF.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = CABEC.
  APPEND LS_LINE TO T_TOP.


  LS_LINE-TYP = 'S'.
  IF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR  WG_T001-LAND1 EQ 'CH'.
*    LS_LINE-KEY = 'Empresa:'.
    LS_LINE-KEY = TEXT-008.
    CONCATENATE  WG_T001-BUKRS '-' WG_T001-BUTXT INTO LS_LINE-INFO SEPARATED BY SPACE.
  ELSEIF WG_T001-LAND1 EQ 'AR'
      OR WG_T001-LAND1 EQ 'PY'.
*    LS_LINE-KEY = 'Sociedad:'.
    LS_LINE-KEY = TEXT-009.
    CONCATENATE  WG_T001-BUKRS '-' WG_T001-BUTXT INTO LS_LINE-INFO SEPARATED BY SPACE.
  ENDIF.
  APPEND LS_LINE TO T_TOP.

  IF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR  WG_T001-LAND1 EQ 'CH'.
*    LS_LINE-KEY = 'Mês/Ano:'.
    LS_LINE-KEY = TEXT-010.
    CONCATENATE  S_MES-LOW(2) '/' S_MES-LOW+2(4)  INTO LS_LINE-INFO SEPARATED BY SPACE.
  ELSEIF WG_T001-LAND1 EQ 'AR'
      OR WG_T001-LAND1 EQ 'PY'.
*    LS_LINE-KEY = 'Ejercicio:'.
    LS_LINE-KEY = TEXT-011.
    CONCATENATE  S_MES-LOW(2) '/' S_MES-LOW+2(4)  INTO LS_LINE-INFO SEPARATED BY SPACE.
  ENDIF.
  APPEND LS_LINE TO T_TOP.

  IF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR  WG_T001-LAND1 EQ 'CH'. "PSA - DEVK9A2CGB
*    LS_LINE-KEY = 'Moeda:'.
    LS_LINE-KEY = TEXT-069.
    LS_LINE-INFO = LR_WAERS.
  ELSEIF WG_T001-LAND1 EQ 'AR'
      OR WG_T001-LAND1 EQ 'PY'.
*    LS_LINE-KEY = 'Moneda:'.
    LS_LINE-KEY = TEXT-070.
    LS_LINE-INFO = LR_WAERS.
  ENDIF.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV,
        W_REPID   TYPE SY-REPID.

  PERFORM DEFINIR_EVENTOS.
  IF P_RAZ IS INITIAL.
    PERFORM MONTAR_LAYOUT USING 'TG_SAIDA'.
*
    CLEAR       V_CARGA.
    IF P_PROC IS NOT INITIAL OR P_REVE IS NOT INITIAL.
      WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
    ENDIF.
    WL_LAYOUT-INFO_FIELDNAME    = 'LINE_COLOR'.

*    WL_LAYOUT-COL_STABLE = 'X'.
*    WL_LAYOUT-ROW_STABLE = 'X'.

    GS_VARIANT_C-REPORT      = SY-REPID.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        IS_VARIANT         = GS_VARIANT_C
        I_CALLBACK_PROGRAM = SY-REPID
        IT_FIELDCAT        = ESTRUTURA[]
        IS_LAYOUT          = WL_LAYOUT
        I_SAVE             = 'X'
        IT_EVENTS          = EVENTS
        IS_PRINT           = T_PRINT
        IT_SORT            = LT_SORT
      TABLES
        T_OUTTAB           = TG_SAIDA.
  ELSE.

    PERFORM MONTAR_LAYOUT USING 'TG_SAIDAR'.

*
    CLEAR       V_CARGA.
    IF P_PROC IS NOT INITIAL OR P_REVE IS NOT INITIAL.
      WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
    ENDIF.
    WL_LAYOUT-INFO_FIELDNAME    = 'LINE_COLOR'.
    CONCATENATE SY-REPID 'R' INTO W_REPID.
    GS_VARIANT_C-REPORT      = W_REPID.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        IS_VARIANT         = GS_VARIANT_C
        I_CALLBACK_PROGRAM = SY-REPID
        IT_FIELDCAT        = ESTRUTURA[]
        IS_LAYOUT          = WL_LAYOUT
        I_SAVE             = 'X'
        IT_EVENTS          = EVENTS
        IS_PRINT           = T_PRINT
        IT_SORT            = LT_SORT
      TABLES
        T_OUTTAB           = TG_SAIDAR.
  ENDIF.


ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.

  IF P_PROC IS NOT INITIAL OR P_REVE IS NOT INITIAL  OR P_RAZ IS NOT INITIAL.
    IF P_RAZ IS INITIAL.
      PERFORM F_CARREGAR_EVENTOS USING:
                                       SLIS_EV_USER_COMMAND  'XUSER_COMMAND', "para tira duplo click
                                       SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                       SLIS_EV_TOP_OF_PAGE   'XTOP_OF_PAGE'.
    ELSE.
      PERFORM F_CARREGAR_EVENTOS USING:
                                 SLIS_EV_USER_COMMAND  'XUSER_COMMANDR', "para tira duplo click
                                 SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                 SLIS_EV_TOP_OF_PAGE   'XTOP_OF_PAGE'.
    ENDIF.
  ELSE.
    PERFORM F_CARREGAR_EVENTOS USING:
                                     SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                     SLIS_EV_TOP_OF_PAGE   'XTOP_OF_PAGE'.
  ENDIF.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT USING P_TABNAME.
  DATA: WL_CURR1_AUX  TYPE DD03P-SCRTEXT_L,
        WL_CURR2_AUX  TYPE DD03P-SCRTEXT_L,
        WL_CURR3_AUX  TYPE DD03P-SCRTEXT_L,
        WL_SALDO_COR  TYPE DD03P-SCRTEXT_L,
        WL_SALDO_COR2 TYPE DD03P-SCRTEXT_L,
        WL_VLR_AJST   TYPE DD03P-SCRTEXT_L,
        WL_VLR_AJST2  TYPE DD03P-SCRTEXT_L.


  REFRESH ESTRUTURA.
  CLEAR: WG_T001.
  READ TABLE TG_T001 INTO WG_T001 INDEX 1.

  IF P_TABNAME EQ 'TG_SAIDAR'.
    READ TABLE TG_T882G INTO WG_T882G
      WITH KEY RBUKRS =  S_BUKRS-LOW
               BINARY SEARCH.

    IF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR WG_T001-LAND1 EQ 'CH' OR WG_T001-LAND1 EQ 'LU'.
      CONCATENATE TEXT-060 WG_T882G-CURR1 INTO WL_CURR1_AUX SEPARATED BY SPACE.
      CONCATENATE TEXT-060 WG_T882G-CURR2 INTO WL_CURR2_AUX SEPARATED BY SPACE.
      IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW = '0037'.
        CONCATENATE TEXT-061 WG_T882G-CURR1 INTO WL_SALDO_COR SEPARATED BY SPACE.
        CONCATENATE TEXT-062 WG_T882G-CURR1 INTO WL_VLR_AJST SEPARATED BY SPACE.
      ELSE.
        CONCATENATE TEXT-061 WG_T882G-CURR2 INTO WL_SALDO_COR SEPARATED BY SPACE.
        CONCATENATE TEXT-062 WG_T882G-CURR2 INTO WL_VLR_AJST SEPARATED BY SPACE.
      ENDIF.

      PERFORM MONTAR_ESTRUTURA USING:
                1  'FAGLFLEXT'         'RACCT'          'TG_SAIDAR' 'RACCT'                   TEXT-A25                   ' ' ,
                1  'FAGLFLEXT'         'RASSC'          'TG_SAIDAR' 'RASSC'                   TEXT-A26                   ' ' ,
                2  'SKAT'              'TXT50'          'TG_SAIDAR' 'TXT50'                   TEXT-A27                   ' ' ,
                3  'SKA1'              'KTOKS'          'TG_SAIDAR' 'KTOKS'                   TEXT-A28                   ' ' ,
                4  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'CURR1'                   WL_CURR1_AUX              '15' ,
                5  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'CURR2'                   WL_CURR2_AUX              '15' ,
                6  'ZFIT0082'          'TX_USD'         'TG_SAIDAR' 'TX_USD'                  TEXT-A29                  '10' ,
                7  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'SALDO_CORR'              WL_SALDO_COR              '15' ,
                8  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'VLR_AJUST'               WL_VLR_AJST               '15' ,
                9  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDAR' 'BELNR'                   TEXT-A30                  ' ' ,
               11  ' '                 ' '              'TG_SAIDAR' 'LOG'                     TEXT-A14                  ' ' ,
               12  'BKPF'              'STBLG'          'TG_SAIDAR' 'STBLG'                   TEXT-A33             ' ' . " USER STORY 155163 // MMSILVA - 15.10.2024
    ELSEIF WG_T001-LAND1 EQ 'AR' OR WG_T001-LAND1 EQ 'PY'.
      CONCATENATE TEXT-060 WG_T882G-CURR1 INTO WL_CURR1_AUX  SEPARATED BY SPACE.
      CONCATENATE TEXT-060 WG_T882G-CURR2 INTO WL_CURR2_AUX  SEPARATED BY SPACE.
      CONCATENATE TEXT-060 WG_T882G-CURR3 INTO WL_CURR3_AUX  SEPARATED BY SPACE.
      CONCATENATE TEXT-063 WG_T882G-CURR2 INTO WL_SALDO_COR  SEPARATED BY SPACE.
      CONCATENATE TEXT-063 WG_T882G-CURR3 INTO WL_SALDO_COR2 SEPARATED BY SPACE.
      CONCATENATE TEXT-061 WG_T882G-CURR2 INTO WL_VLR_AJST   SEPARATED BY SPACE.
      CONCATENATE TEXT-061 WG_T882G-CURR3 INTO WL_VLR_AJST2  SEPARATED BY SPACE.

      PERFORM MONTAR_ESTRUTURA USING:
            1  'FAGLFLEXT'         'RACCT'          'TG_SAIDAR' 'RACCT'                   TEXT-A31             ' ' ,
            2  'SKAT'              'TXT50'          'TG_SAIDAR' 'TXT50'                   TEXT-A32             ' ' ,
            3  'SKA1'              'KTOKS'          'TG_SAIDAR' 'KTOKS'                   TEXT-A13             ' ' ,
            4  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'CURR1'                   WL_CURR1_AUX         '15' ,
            5  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'CURR2'                   WL_CURR2_AUX         '15' ,
            6  'FAGLFLEXT'         'OSLVT'          'TG_SAIDAR' 'CURR3'                   WL_CURR3_AUX         '15' ,
            7  'ZFIT0082'          'TX_USD'         'TG_SAIDAR' 'TX_USD'                  TEXT-A29             '10' ,
            8  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'VLR_AJUST'               WL_VLR_AJST          '15' ,
            9  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'SALDO_CORR'              WL_SALDO_COR         '15' ,

           10  'ZFIT0082'          'TX_BRL'         'TG_SAIDAR' 'TX_BRL'                  TEXT-A09             '10' ,
           11  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'VLR_AJUST2'              WL_VLR_AJST2         '15' ,
           12  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'SALDO_CORR2'             WL_SALDO_COR2        '15' ,

           13  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDAR' 'BELNR'                   TEXT-A30             ' ' ,
           15  ' '                 ' '              'TG_SAIDAR' 'LOG'                     TEXT-A14             ' ' .
      "
    ENDIF.
  ELSEIF P_TABNAME EQ 'TG_SAIDA'.
    READ TABLE TG_T882G INTO WG_T882G
      WITH KEY RBUKRS =  S_BUKRS-LOW
               BINARY SEARCH.
    IF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR WG_T001-LAND1 EQ 'CH' OR WG_T001-LAND1 EQ 'LU'.
      CONCATENATE TEXT-012 WG_T882G-CURR1 INTO WL_CURR1_AUX SEPARATED BY SPACE.
      CONCATENATE TEXT-012 WG_T882G-CURR2 INTO WL_CURR2_AUX SEPARATED BY SPACE.
      IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW = '0037'.
        CONCATENATE TEXT-013 WG_T882G-CURR1 INTO WL_SALDO_COR SEPARATED BY SPACE.
        CONCATENATE TEXT-014 WG_T882G-CURR1 INTO WL_VLR_AJST SEPARATED BY SPACE.
      ELSE.
        CONCATENATE TEXT-013 WG_T882G-CURR2 INTO WL_SALDO_COR SEPARATED BY SPACE.
        CONCATENATE TEXT-014 WG_T882G-CURR2 INTO WL_VLR_AJST SEPARATED BY SPACE.
      ENDIF.

      IF P_CLIE IS NOT INITIAL.
        PERFORM MONTAR_ESTRUTURA USING:
                  1  'BSID'              'KUNNR'          'TG_SAIDA' 'KUNNR'                   TEXT-A01             '06' ,
                  2  'KNA1'              'NAME1'          'TG_SAIDA' 'NAME1'                   TEXT-A02             '05' ,
                  3  'BSID'              'HKONT'          'TG_SAIDA' 'HKONT'                   TEXT-A03             '08' ,
                  4  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   TEXT-A04             '05' ,
                  5  ' '                 ' '              'TG_SAIDA' 'VBUND'                   TEXT-A05             '05' ,
                  6  ' '                 ' '              'TG_SAIDA' 'BEWAR'                   TEXT-A06             '05' ,
                  7  'BSIK'              'BELNR'          'TG_SAIDA' 'BELNR2'                  ' '                  ' ' ,
                  8  'BSIK'              'BUZEI'          'TG_SAIDA' 'BUZEI'                   ' '                  '03' ,
                  9  'BSIK'              'WAERS'          'TG_SAIDA' 'WAERS'                   ' '                  '05' ,
                 10  'BSIK'              'BUDAT'          'TG_SAIDA' 'BUDAT'                   ' '                  '08' ,
                 11  'BSIK'              'BLART'          'TG_SAIDA' 'BLART'                   ' '                  '03' ,
                 12  'BSIK'              'XBLNR'          'TG_SAIDA' 'XBLNR'                   TEXT-A07             '03',
                 13  'BSID'              'VBEL2'          'TG_SAIDA' 'DOC'                     TEXT-A08             '03',
                 14  'BSID'              'VPOS2'          'TG_SAIDA' 'ITEM'                    TEXT-A10             '03' ,
                 15  'BSIK'              'DMBTR'          'TG_SAIDA' 'DMBTR'                   WL_CURR1_AUX         ' ' ,
                 16  'BSIK'              'DMBE2'          'TG_SAIDA' 'DMBE2'                   WL_CURR2_AUX         ' ' ,
                 17  'ZFIT0084'          'TX_USD'         'TG_SAIDA' 'TX_USD'                  TEXT-A11             ' ' ,
                 18  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR'              WL_SALDO_COR         ' ' ,
                 19  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST'               WL_VLR_AJST          ' ' ,
                 20  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDA' 'BELNR'                   TEXT-A12             ' ' ,
                 22  ' '                 ' '              'TG_SAIDA' 'LOG'                     TEXT-A14             ' ' ,
                 23  'BKPF'              'STBLG'          'TG_SAIDA' 'STBLG'                   TEXT-A33             ' ' . " USER STORY 155163 // MMSILVA - 15.10.2024

      ELSE.
        PERFORM MONTAR_ESTRUTURA USING:
                       1  'BSIK'              'LIFNR'          'TG_SAIDA' 'KUNNR'                   TEXT-A18                 '06' ,
                       2  'LFA1'              'NAME1'          'TG_SAIDA' 'NAME1'                   TEXT-A19                 '05' ,
                       3  'BSID'              'HKONT'          'TG_SAIDA' 'HKONT'                   TEXT-A03                 '08' ,
                       4  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   TEXT-A04                 '05' ,
                       5  ' '                 ' '              'TG_SAIDA' 'VBUND'                   TEXT-A05                 '05' ,
                       6  ' '                 ' '              'TG_SAIDA' 'BEWAR'                   TEXT-A06                 '05' ,
                       7  'BSIK'              'BELNR'          'TG_SAIDA' 'BELNR2'                  ' '                      ' ' ,
                       8  'BSIK'              'BUZEI'          'TG_SAIDA' 'BUZEI'                   ' '                      '03' ,
                       9  'BSIK'              'WAERS'          'TG_SAIDA' 'WAERS'                   ' '                      '05' ,
                      10  'BSIK'              'BUDAT'          'TG_SAIDA' 'BUDAT'                   ' '                      '08' ,
                      11  'BSIK'              'BLART'          'TG_SAIDA' 'BLART'                   ' '                      '03' ,
                      12  'BSIK'              'XBLNR'          'TG_SAIDA' 'XBLNR'                   TEXT-A07                 '03' ,
                      13  'BSIK'              'EBELN'          'TG_SAIDA' 'DOC'                     TEXT-A20                 '03' ,
                      14  'BSIK'              'EBELP'          'TG_SAIDA' 'ITEM'                    TEXT-A10                 '03' ,
                      15  'BSIK'              'DMBTR'          'TG_SAIDA' 'DMBTR'                   WL_CURR1_AUX             ' ' ,
                      16  'BSIK'              'DMBE2'          'TG_SAIDA' 'DMBE2'                   WL_CURR2_AUX             ' ' ,
                      17  'ZFIT0084'          'TX_USD'         'TG_SAIDA' 'TX_USD'                  TEXT-A11                 ' ' ,
                      18  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR'              WL_SALDO_COR             ' ' ,
                      19  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST'               WL_VLR_AJST              ' ' ,
                      20  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDA' 'BELNR'                   TEXT-A12                 ' ' ,
                      22  ' '                 ' '              'TG_SAIDA' 'LOG'                     TEXT-A14                 ' ' ,
                      23  'BKPF'              'STBLG'          'TG_SAIDA' 'STBLG'                   TEXT-A33                 ' ' . " USER STORY 155163 // MMSILVA - 15.10.2024

      ENDIF.
    ELSEIF WG_T001-LAND1 EQ 'AR' OR WG_T001-LAND1 EQ 'PY' OR WG_T001-LAND1 EQ 'LU'..

      CONCATENATE TEXT-012 WG_T882G-CURR1 INTO WL_CURR1_AUX SEPARATED BY SPACE.
      CONCATENATE TEXT-012 WG_T882G-CURR2 INTO WL_CURR2_AUX SEPARATED BY SPACE.
      CONCATENATE TEXT-012 WG_T882G-CURR3 INTO WL_CURR3_AUX SEPARATED BY SPACE.
      CONCATENATE TEXT-A21 WG_T882G-CURR2 INTO WL_SALDO_COR SEPARATED BY SPACE.
      CONCATENATE TEXT-A21 WG_T882G-CURR3 INTO WL_SALDO_COR2 SEPARATED BY SPACE.
      CONCATENATE TEXT-A22 WG_T882G-CURR2 INTO WL_VLR_AJST SEPARATED BY SPACE.
      CONCATENATE TEXT-A22 WG_T882G-CURR3 INTO WL_VLR_AJST2 SEPARATED BY SPACE.
      IF P_CLIE IS NOT INITIAL.
        PERFORM MONTAR_ESTRUTURA USING:
                  1  'BSID'              'KUNNR'          'TG_SAIDA' 'KUNNR'                   'Cliente'          ' ' ,
                  2  'KNA1'              'NAME1'          'TG_SAIDA' 'NAME1'                   'Nombre cliente'   ' ' ,
                  3  'BSID'              'HKONT'          'TG_SAIDA' 'HKONT'                   'Cuenta '          ' ' ,
                  4  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   'Descripción'      ' ' ,
                  5  ' '                 ' '              'TG_SAIDA' 'VBUND'                   'Soc.Parc.'        ' ' ,
                  6  ' '                 ' '              'TG_SAIDA' 'BEWAR'                   'Tp.Movto'         ' ' ,
                  7  'BSIK'              'BELNR'          'TG_SAIDA' 'BELNR2'                  ' '         ' ' ,
                  8  'BSIK'              'WAERS'          'TG_SAIDA' 'WAERS'                   ' '         ' ' ,
                  9  'BSIK'              'BUDAT'          'TG_SAIDA' 'BUDAT'                   'Dt.Fecha'         ' ' ,
                 10  'BSIK'              'BLART'          'TG_SAIDA' 'BLART'                   'Tp.Dcto.'         ' ' ,
                 11  'BSIK'              'XBLNR'          'TG_SAIDA' 'XBLNR'                   'Referência'       ' ' ,
                 12  'BSID'              'VBEL2'          'TG_SAIDA' 'DOC'                     'Nro.OV'           ' ' ,
                 13  'BSID'              'VPOS2'          'TG_SAIDA' 'ITEM'                    'Item'             ' ' ,
                 14  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBTR'                   WL_CURR1_AUX       ' ' ,
                 15  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBE2'                   WL_CURR2_AUX       ' ' ,
                 16  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBE3'                   WL_CURR3_AUX       ' ' ,
                 17  'ZFIT0084'          'TX_USD'         'TG_SAIDA' 'TX_USD'                  'Taxa USD'         ' ' ,
                 18  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR'              WL_SALDO_COR       ' ' ,
                 19  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST'               WL_VLR_AJST        ' ' ,
                 20  'ZFIT0084'          'TX_BRL'         'TG_SAIDA' 'TX_BRL'                  'Taxa BRL'         ' ' ,
                 21  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR2'             WL_SALDO_COR2      ' ' ,
                 22  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST2'              WL_VLR_AJST2       ' ' ,
                 23  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDA' 'BELNR'                   'Nro.documento'    ' ' ,
                 25  ' '                 ' '              'TG_SAIDA' 'LOG'                     'Log'              ' ' .

      ELSE.
        PERFORM MONTAR_ESTRUTURA USING:
                       1  'BSIK'              'LIFNR'          'TG_SAIDA' 'KUNNR'                   'Proveedor'            ' ' ,
                       2  'LFA1'              'NAME1'          'TG_SAIDA' 'NAME1'                   'Nombre Proveedor'     ' ' ,
                       3  'BSID'              'HKONT'          'TG_SAIDA' 'HKONT'                   'Cuenta'               ' ' ,
                       4  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   'Descripción'          ' ' ,
                       5  ' '                 ' '              'TG_SAIDA' 'VBUND'                   'Soc.Parc.'          ' ' ,
                       6  ' '                 ' '              'TG_SAIDA' 'BEWAR'                   'Tp.Movto'           ' ' ,
                       7  'BSIK'              'BELNR'          'TG_SAIDA' 'BELNR2'                  ' '             ' ' ,
                       8  'BSIK'              'WAERS'          'TG_SAIDA' 'WAERS'                   ' '             ' ' ,
                       9  'BSIK'              'BUDAT'          'TG_SAIDA' 'BUDAT'                   'Dt.Fecha'             ' ' ,
                      10  'BSIK'              'BLART'          'TG_SAIDA' 'BLART'                   'Tp.Dcto.'             ' ' ,
                      11  'BSIK'              'XBLNR'          'TG_SAIDA' 'XBLNR'                   'Referência'           ' ' ,
                      12  'BSIK'              'EBELN'          'TG_SAIDA' 'DOC'                     'Solicitud'            ' ' ,
                      13  'BSIK'              'EBELP'          'TG_SAIDA' 'ITEM'                    'Item'                 ' ' ,
                      14  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBTR'                   WL_CURR1_AUX           ' ' ,
                      15  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBE2'                   WL_CURR2_AUX           ' ' ,
                      16  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBE3'                   WL_CURR3_AUX           ' ' ,
                      17  'ZFIT0084'          'TX_USD'         'TG_SAIDA' 'TX_USD'                  'Taxa USD'             ' ' ,
                      18  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR'              WL_SALDO_COR           ' ' ,
                      19  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST'               WL_VLR_AJST            ' ' ,
                      20  'ZFIT0084'          'TX_BRL'         'TG_SAIDA' 'TX_BRL'                  'Taxa BRL'             ' ' ,
                      21  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR2'             WL_SALDO_COR2          ' ' ,
                      22  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST2'              WL_VLR_AJST2           ' ' ,
                      23  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDA' 'BELNR'                   'Nro.documento'        ' ' ,
                      25  ' '                 ' '              'TG_SAIDA' 'LOG'                     'Log'                  ' ' .
      ENDIF.
    ENDIF.
  ELSEIF P_TABNAME EQ 'TG_SAIDA_EXEC'.

    IF P_CLIE IS NOT INITIAL.
      PERFORM MONTAR_ESTRUTURA USING:
          1  ' '          ' '                'TG_SAIDA_EXEC' 'ICON'      TEXT-A23             ' ',
          2  ' '          ' '                'TG_SAIDA_EXEC' 'MSG'       'Msg'                '80',
          3  ' '          ' '                'TG_SAIDA_EXEC' 'TIPO'      TEXT-A24             '30',
          4  'KNA1'       'KUNNR'            'TG_SAIDA_EXEC' 'LIFNR'     TEXT-A01             ' ',
          5  'KNA1'       'NAME1'            'TG_SAIDA_EXEC' 'NAME1'     TEXT-A02             ' ',
          6  'BSIK'       'HKONT'            'TG_SAIDA_EXEC' 'HKONT'     TEXT-A03             ' ',
          7  'SKAT'       'TXT50'            'TG_SAIDA_EXEC' 'TXT50'     TEXT-A04             ' '.

    ELSEIF  P_FORN IS NOT INITIAL..
      PERFORM MONTAR_ESTRUTURA USING:
        1  ' '          ' '                'TG_SAIDA_EXEC' 'ICON'       TEXT-A23               ' ',
        2  ' '          ' '                'TG_SAIDA_EXEC' 'MSG'       'Msg'                   '80',
        3  ' '          ' '                'TG_SAIDA_EXEC' 'TIPO'       TEXT-A24               '30',
        4  'LFA1'       'LIFNR'            'TG_SAIDA_EXEC' 'LIFNR'      TEXT-A18               ' ',
        5  'LFA1'       'NAME1'            'TG_SAIDA_EXEC' 'NAME1'      TEXT-A19               ' ',
        6  'BSIK'       'HKONT'            'TG_SAIDA_EXEC' 'HKONT'      TEXT-A03               ' ',
        7  'SKAT'       'TXT50'            'TG_SAIDA_EXEC' 'TXT50'      TEXT-A04               ' '.
    ELSE.
      PERFORM MONTAR_ESTRUTURA USING:
          1  ' '          ' '                'TG_SAIDA_EXEC' 'ICON'      TEXT-A23           ' ',
          2  ' '          ' '                'TG_SAIDA_EXEC' 'MSG'       TEXT-A04           '80',
          3  'FAGLFLEXT'  'RACCT'            'TG_SAIDA_EXEC' 'RACCT'     TEXT-A25           ' ',
          4  'SKAT'       'TXT50'            'TG_SAIDA_EXEC' 'TXT50'     TEXT-A27           ' '.


    ENDIF.

  ENDIF.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            P_SCRTEXT_L
*                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  CLEAR WA_ESTRUTURA.

  IF P_FIELD EQ 'BELNR'
  OR P_FIELD EQ 'BELNR_EST'
  OR P_FIELD EQ 'BELNR_INV'
  OR P_FIELD EQ 'BELNR_INV_EST'
  OR P_FIELD EQ 'BELNR2'
  OR P_FIELD EQ 'STBLG'. "USER STORY 155163 // MMSILVA - 24.10.2024
    WA_ESTRUTURA-JUST = 'C'.
    WA_ESTRUTURA-HOTSPOT = C_X.
  ENDIF.


  WA_ESTRUTURA-OUTPUTLEN     = P_OUTPUTLEN.
  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .
  DATA: WL_TABIX     TYPE SY-TABIX,
        WL_BSIK_BSID TYPE TY_BSIK_BSID.

  SELECT *                             "#EC CI_DB_OPERATION_OK[2431747]
        FROM T882G
        INTO TABLE TG_T882G
         WHERE RBUKRS IN S_BUKRS.

  IF SY-SUBRC IS INITIAL.
    SELECT *
      FROM T001
      INTO TABLE TG_T001
       FOR ALL ENTRIES IN TG_T882G
       WHERE BUKRS EQ TG_T882G-RBUKRS.
  ELSE.
    EXIT.
  ENDIF.

  RANGES: RL_BUDAT FOR BSIK-BUDAT.

  REFRESH: RL_BUDAT.
** Processamento
  IF P_PROC IS NOT INITIAL OR P_REVE IS NOT INITIAL.
    VG_RYEAR  = S_MES-LOW+2(4).
    CONCATENATE VG_RYEAR S_MES-LOW(2) '01' INTO VG_LAST_DAY_AUX.
    VG_LAST_DAY = VG_LAST_DAY_AUX.
    RL_BUDAT-SIGN = 'I'.
    RL_BUDAT-OPTION = 'BT'.

    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        I_DATE = VG_LAST_DAY
      IMPORTING
        E_DATE = VG_LAST_DAY.

    CONVERT INVERTED-DATE VG_LAST_DAY INTO DATE VG_LAST_DAY_AUX2.

    VG_FIRST_DAY = VG_LAST_DAY.

    ADD 1 TO VG_FIRST_DAY.


    RL_BUDAT-HIGH      = VG_LAST_DAY.
    APPEND RL_BUDAT.
    CLEAR: RL_BUDAT.


    CONVERT INVERTED-DATE VG_FIRST_DAY INTO DATE VG_LAST_DAY_AUX.
*
    IF P_FORN IS NOT INITIAL.
      READ TABLE TG_T882G INTO WG_T882G INDEX 1.
      IF S_BUKRS-LOW EQ '0004' OR S_BUKRS-LOW = '0037'.
        WG_T882G-CURR1 = 'USD'.
      ENDIF.
      SELECT BUKRS BELNR BUZEI GJAHR BUDAT HKONT WAERS MONAT DMBTR DMBE2
             DMBE3 LIFNR XBLNR BLART EBELN EBELP UMSKS UMSKZ SHKZG BSCHL
        FROM BSIK
        INTO TABLE TG_BSIK_BSID
         WHERE BUKRS  IN S_BUKRS
           AND BUDAT  IN RL_BUDAT
           AND HKONT  IN S_SAKNR
           AND BLART  NE 'VC'
           AND WAERS  EQ WG_T882G-CURR1
           AND ANLN1  EQ ''.

      "ALRS 13.11.2014
      SELECT BUKRS BELNR BUZEI GJAHR BUDAT HKONT WAERS MONAT DMBTR DMBE2
       DMBE3 LIFNR XBLNR BLART EBELN EBELP UMSKS UMSKZ SHKZG BSCHL
        FROM BSAK
        APPENDING TABLE TG_BSIK_BSID
         WHERE BUKRS  IN S_BUKRS
           AND AUGDT  GT VG_LAST_DAY
           AND BUDAT  IN RL_BUDAT
           AND HKONT  IN S_SAKNR
           AND BLART  NE 'VC'
           AND WAERS  EQ WG_T882G-CURR1
           AND ANLN1  EQ ''.

      IF TG_BSIK_BSID[] IS NOT INITIAL.
        SELECT *
          FROM LFA1
          INTO TABLE TG_LFA1
           FOR ALL ENTRIES IN TG_BSIK_BSID
            WHERE LIFNR EQ TG_BSIK_BSID-LIFNR.
      ENDIF.
    ELSE.
      READ TABLE TG_T882G INTO WG_T882G INDEX 1.
      IF S_BUKRS-LOW EQ '0004' OR S_BUKRS-LOW EQ '0037' .
        WG_T882G-CURR1 = 'USD'.
      ENDIF.
      SELECT BUKRS BELNR BUZEI GJAHR BUDAT HKONT WAERS MONAT DMBTR DMBE2
             DMBE3 KUNNR XBLNR BLART VBEL2 VPOS2 UMSKS UMSKZ SHKZG BSCHL AUGBL
        FROM BSID
        INTO TABLE TG_BSIK_BSID
         WHERE BUKRS  IN S_BUKRS
           AND BUDAT  IN RL_BUDAT
           AND HKONT  IN S_SAKNRC
           AND BLART  NE 'VC'
           AND WAERS  EQ WG_T882G-CURR1.

      "ALRS 13.11.2014
      SELECT BUKRS BELNR BUZEI GJAHR BUDAT HKONT WAERS MONAT DMBTR DMBE2
             DMBE3 KUNNR XBLNR BLART VBEL2 VPOS2 UMSKS UMSKZ SHKZG BSCHL AUGBL
        FROM BSAD
        APPENDING TABLE TG_BSIK_BSID
         WHERE BUKRS  IN S_BUKRS
           AND AUGDT  GT VG_LAST_DAY
           AND BUDAT  IN RL_BUDAT
           AND HKONT  IN S_SAKNRC
           AND BLART  NE 'VC'
           AND WAERS  EQ WG_T882G-CURR1.

      IF TG_BSIK_BSID[] IS NOT  INITIAL.
        SELECT *
          FROM KNA1
          INTO TABLE TG_KNA1
           FOR ALL ENTRIES IN TG_BSIK_BSID
            WHERE KUNNR EQ TG_BSIK_BSID-LIFNR.

      ENDIF.
    ENDIF.

* Visualização
  ELSE.
    SELECT *
      FROM ZFIT0084
      INTO TABLE TG_0084
       WHERE BUKRS   IN S_BUKRS
         AND MES_ANO IN S_MES
         AND SAKNR   IN S_SAKNR
         AND SAKNR   IN S_SAKNRC.

    IF TG_0084 IS NOT INITIAL.
      SELECT *
          FROM ZFIT0081
          INTO TABLE TG_0081
          WHERE BUKRS  IN S_BUKRS.

      IF P_FORN IS NOT INITIAL.
        READ TABLE TG_T882G INTO WG_T882G INDEX 1.
        SELECT BUKRS BELNR BUZEI GJAHR BUDAT HKONT WAERS MONAT DMBTR DMBE2
               DMBE3 LIFNR XBLNR BLART EBELN EBELP UMSKS UMSKZ SHKZG BSCHL AUGBL
          FROM BSIK
          INTO TABLE TG_BSIK_BSID
          FOR ALL ENTRIES IN TG_0084
           WHERE BUKRS  EQ TG_0084-BUKRS
             AND BELNR  EQ TG_0084-BELNR
             AND BUZEI  EQ TG_0084-BUZEI.

        SELECT BUKRS BELNR BUZEI GJAHR BUDAT HKONT WAERS MONAT DMBTR DMBE2
               DMBE3 LIFNR XBLNR BLART EBELN EBELP UMSKS UMSKZ SHKZG BSCHL AUGBL
          FROM BSAK
          APPENDING TABLE TG_BSIK_BSID
          FOR ALL ENTRIES IN TG_0084
           WHERE BUKRS  EQ TG_0084-BUKRS
             AND LIFNR  EQ TG_0084-LIFNR
             AND BELNR  EQ TG_0084-BELNR
             AND BUZEI  EQ TG_0084-BUZEI
             AND AUGDT  GT VG_LAST_DAY
             AND BUDAT  IN RL_BUDAT.


        IF TG_BSIK_BSID[] IS NOT INITIAL.
          SELECT *
            FROM LFA1
            INTO TABLE TG_LFA1
             FOR ALL ENTRIES IN TG_BSIK_BSID
              WHERE LIFNR EQ TG_BSIK_BSID-LIFNR.
        ENDIF.

      ELSE.
        READ TABLE TG_T882G INTO WG_T882G INDEX 1.
        SELECT BUKRS BELNR BUZEI GJAHR BUDAT HKONT WAERS MONAT DMBTR DMBE2
               DMBE3 KUNNR XBLNR BLART VBEL2 VPOS2 UMSKS UMSKZ SHKZG BSCHL AUGBL
          FROM BSID
          INTO TABLE TG_BSIK_BSID
           FOR ALL ENTRIES IN TG_0084
           WHERE BUKRS  EQ TG_0084-BUKRS
             AND KUNNR  EQ TG_0084-LIFNR
             AND BELNR  EQ TG_0084-BELNR
             AND BUZEI  EQ TG_0084-BUZEI.

        SELECT BUKRS BELNR BUZEI GJAHR BUDAT HKONT WAERS MONAT DMBTR DMBE2
              DMBE3 KUNNR XBLNR BLART VBEL2 VPOS2 UMSKS UMSKZ SHKZG BSCHL AUGBL
         FROM BSAD
         APPENDING TABLE TG_BSIK_BSID
          FOR ALL ENTRIES IN TG_0084
          WHERE BUKRS  EQ TG_0084-BUKRS
            AND KUNNR  EQ TG_0084-LIFNR
            AND BELNR  EQ TG_0084-BELNR
            AND BUZEI  EQ TG_0084-BUZEI
            AND AUGDT  GT VG_LAST_DAY
            AND BUDAT  IN RL_BUDAT.

        IF TG_BSIK_BSID[] IS NOT INITIAL.
          SELECT *
            FROM KNA1
            INTO TABLE TG_KNA1
             FOR ALL ENTRIES IN TG_BSIK_BSID
              WHERE KUNNR EQ TG_BSIK_BSID-LIFNR.

        ENDIF.
      ENDIF.

      IF TG_BSIK_BSID[] IS NOT INITIAL.
        SELECT *                       "#EC CI_DB_OPERATION_OK[2389136]
          FROM SKA1                    "#EC CI_DB_OPERATION_OK[2431747]
          INTO TABLE TG_SKA1
           FOR ALL ENTRIES IN TG_BSIK_BSID
           WHERE SAKNR EQ TG_BSIK_BSID-HKONT
           AND KTOPL = '0050'.

        IF SY-SUBRC IS INITIAL.
          SELECT *
           FROM SKAT
           INTO TABLE TG_SKAT
            FOR ALL ENTRIES IN TG_SKA1
            WHERE SAKNR EQ TG_SKA1-SAKNR
              AND KTOPL EQ '0050'
              AND SPRAS EQ SY-LANGU.
        ENDIF.
      ENDIF.

      SELECT *
        FROM ZIB_CONTABIL
        INTO TABLE TG_ZIB
         FOR ALL ENTRIES IN TG_0084
         WHERE OBJ_KEY EQ TG_0084-OBJ_KEY.

      SELECT *
        FROM ZIB_CONTABIL_CHV
        INTO TABLE TG_ZIB_CHV
         FOR ALL ENTRIES IN TG_0084
         WHERE OBJ_KEY EQ TG_0084-OBJ_KEY.

      SELECT *
        FROM ZIB_CONTABIL_ERR
        INTO TABLE TG_ZIB_ERR
         FOR ALL ENTRIES IN TG_0084
         WHERE OBJ_KEY EQ TG_0084-OBJ_KEY.

      READ TABLE TG_T882G INTO WG_T882G INDEX 1.

    ENDIF.
    EXIT.
  ENDIF.

** Documentos estornados
  READ TABLE TG_T882G INTO WG_T882G INDEX 1.

*  SELECT *
*    FROM BKPF
*    INTO CORRESPONDING FIELDS OF TABLE TG_BKPF_LANC
*    WHERE BUKRS  IN S_BUKRS
*     AND  BUDAT  IN RL_BUDAT
*     AND  STBLG  NE ''
*     AND  BLART  EQ 'LM'
*     AND  WAERS  EQ WG_T882G-CURR1.
*
*  IF TG_BKPF_LANC[] IS NOT INITIAL.
*    SELECT *
*      FROM BKPF
*      INTO CORRESPONDING FIELDS OF TABLE TG_BKPF_EST
*      FOR ALL ENTRIES IN TG_BKPF_LANC
*      WHERE BUKRS EQ TG_BKPF_LANC-BUKRS
*       AND  GJAHR EQ TG_BKPF_LANC-STJAH
*       AND  STBLG EQ TG_BKPF_LANC-BELNR
*       AND  ( ( MONAT NE TG_BKPF_LANC-MONAT AND GJAHR EQ TG_BKPF_LANC-GJAHR ) OR
*              ( MONAT EQ TG_BKPF_LANC-MONAT AND GJAHR NE TG_BKPF_LANC-GJAHR ) ).
*
*    IF SY-SUBRC IS INITIAL.
*      FIELD-SYMBOLS : <FS_BKPF> TYPE BKPF.
*
*      LOOP AT TG_BKPF_LANC ASSIGNING <FS_BKPF>.
*        READ TABLE TG_BKPF_EST INTO WG_BKPF WITH KEY BELNR = <FS_BKPF>-STBLG.
*        IF SY-SUBRC IS NOT INITIAL.
*          DELETE TG_BKPF_LANC WHERE STBLG = <FS_BKPF>-STBLG.
*        ENDIF.
*      ENDLOOP.
*
*      SELECT *
*        FROM BSEG
*        INTO CORRESPONDING FIELDS OF TABLE TG_BSEG
*        FOR ALL ENTRIES IN TG_BKPF_LANC
*        WHERE BELNR EQ TG_BKPF_LANC-BELNR
*         AND  BUKRS EQ TG_BKPF_LANC-BUKRS
*         AND  HKONT IN S_SAKNR
*         AND  GJAHR EQ TG_BKPF_LANC-GJAHR.
*
*      IF SY-SUBRC IS INITIAL.
*        IF P_FORN IS NOT INITIAL.
*          DELETE TG_BSEG WHERE KOART NE 'K'.
*        ELSE.
*          DELETE TG_BSEG WHERE KOART NE 'D'.
*        ENDIF.
*        DELETE TG_BSEG WHERE KUNNR IS INITIAL.
*
*        LOOP AT TG_BSEG INTO WG_BSEG.
*          CLEAR WL_BSIK_BSID.
*          READ TABLE TG_BKPF_LANC INTO WG_BKPF WITH KEY BELNR = WG_BSEG-BELNR.
*          IF SY-SUBRC IS INITIAL.
*            READ TABLE TG_BSIK_BSID INTO WL_BSIK_BSID  WITH KEY BELNR = WG_BSEG-BELNR
*                                                                BUZEI = WG_BSEG-BUZEI.
*            IF SY-SUBRC NE 0.
*              CLEAR WL_BSIK_BSID.
*              MOVE-CORRESPONDING WG_BKPF TO WL_BSIK_BSID.
*
*              WL_BSIK_BSID-BUZEI = WG_BSEG-BUZEI.
*              WL_BSIK_BSID-HKONT = WG_BSEG-HKONT.
*              WL_BSIK_BSID-DMBTR = WG_BSEG-DMBTR.
*              WL_BSIK_BSID-DMBE2 = WG_BSEG-DMBE2.
*              WL_BSIK_BSID-DMBE3 = WG_BSEG-DMBE3.
*              WL_BSIK_BSID-LIFNR = WG_BSEG-KUNNR.
*              WL_BSIK_BSID-DOC   = WG_BSEG-EBELN.
*              WL_BSIK_BSID-ITEM  = WG_BSEG-EBELP.
*              WL_BSIK_BSID-UMSKS = WG_BSEG-UMSKS.
*              WL_BSIK_BSID-UMSKZ = WG_BSEG-UMSKZ.
*              WL_BSIK_BSID-SHKZG = WG_BSEG-SHKZG.
*              WL_BSIK_BSID-BSCHL = WG_BSEG-BSCHL.
*
*              APPEND WL_BSIK_BSID TO TG_BSIK_BSID.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*
*    ENDIF.
*  ENDIF.

  "Eliminar documentos estornados  até a data BASE  - ALRS
  IF TG_BSIK_BSID[] IS NOT INITIAL.
    SELECT *
       FROM BKPF INTO TABLE TG_BKPF_LANC
     FOR ALL ENTRIES IN  TG_BSIK_BSID
     WHERE BUKRS = TG_BSIK_BSID-BUKRS
     AND   BELNR = TG_BSIK_BSID-BELNR
     AND   GJAHR = TG_BSIK_BSID-GJAHR
     AND   ( STGRD NE SPACE OR BSTAT = 'S' ).
    IF SY-SUBRC = 0.
      "ALRS 13.11.2014 Estorno dentro do mês não leva
      SELECT *
         FROM BKPF
         INTO TABLE TG_BKPF_EST
          FOR ALL ENTRIES IN TG_BKPF_LANC
           WHERE BUKRS EQ TG_BKPF_LANC-BUKRS
             AND BELNR EQ TG_BKPF_LANC-STBLG
             AND GJAHR EQ TG_BKPF_LANC-STJAH.

      LOOP AT TG_BKPF_LANC INTO WG_BKPF.
        READ TABLE TG_BKPF_EST INTO WG_BKPFE WITH KEY BUKRS = WG_BKPF-BUKRS
                                                      BELNR = WG_BKPF-STBLG
                                                      GJAHR = WG_BKPF-STJAH.
        IF SY-SUBRC = 0 AND WG_BKPF-BSTAT NE 'S'. "07.03.2017 ALRS
          IF  WG_BKPFE-BUDAT LE VG_LAST_DAY.
            DELETE TG_BSIK_BSID WHERE BUKRS = WG_BKPF-BUKRS AND
                                  BELNR = WG_BKPF-BELNR AND
                                  GJAHR = WG_BKPF-GJAHR.

          ENDIF.
        ELSE.
          DELETE TG_BSIK_BSID WHERE BUKRS = WG_BKPF-BUKRS AND
                                BELNR = WG_BKPF-BELNR AND
                                GJAHR = WG_BKPF-GJAHR.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF TG_BSIK_BSID[] IS NOT INITIAL.
*    IF P_VISU = 'X'.
*      SELECT *
*       FROM ZFIT0084
*       INTO TABLE TG_0084
*       FOR ALL ENTRIES IN TG_BSIK_BSID
*        WHERE BUKRS   EQ TG_BSIK_BSID-BUKRS
*          AND MES_ANO IN S_MES
*          AND SAKNR   EQ TG_BSIK_BSID-HKONT
*          AND BELNR   EQ TG_BSIK_BSID-BELNR.
*
*    ENDIF.

    SELECT *                           "#EC CI_DB_OPERATION_OK[2389136]
     FROM SKA1                         "#EC CI_DB_OPERATION_OK[2431747]
     INTO TABLE TG_SKA1
      FOR ALL ENTRIES IN TG_BSIK_BSID
      WHERE SAKNR EQ TG_BSIK_BSID-HKONT
      AND KTOPL = '0050'.

    IF SY-SUBRC IS INITIAL.
      SELECT *
      FROM SKAT
      INTO TABLE TG_SKAT
       FOR ALL ENTRIES IN TG_SKA1
       WHERE SAKNR EQ TG_SKA1-SAKNR
         AND KTOPL EQ '0050'
         AND SPRAS EQ SY-LANGU.
    ENDIF.

**ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg US 100617 - BG - Inicio

    DATA: V_MES TYPE ZDE_MES,
          V_ANO TYPE ZDE_ANO.

    V_MES = S_MES-LOW(2).
    V_ANO = S_MES-LOW+2(4).

    SELECT *
    FROM ZFIT0185 INTO TABLE TG_ZFIT0185
    WHERE MES EQ V_MES
      AND ANO EQ V_ANO
      AND EMPRESA EQ S_BUKRS-LOW
      AND  CATEGORIA EQ 'B'
      AND MOEDA_ORIGEM IN ('USD', 'BRL')
      AND MOEDA_DESTINO IN ('BRL', 'ARS', 'PYG', 'EUR', 'CHF').

    SELECT *
 FROM ZFIT0185 INTO TABLE TG_ZFIT0185_LDAY
 WHERE MES EQ V_MES
   AND ANO EQ V_ANO
   AND EMPRESA EQ S_BUKRS-LOW
     AND CATEGORIA EQ 'B'
      AND MOEDA_ORIGEM IN ('USD', 'BRL')
      AND MOEDA_DESTINO IN ('BRL', 'ARS', 'PYG', 'EUR', 'CHF').

    SELECT *
      FROM TCURR
      INTO TABLE TG_TCURR
       WHERE GDATU EQ VG_LAST_DAY_AUX
         AND KURST EQ 'B'
         AND FCURR IN ('USD', 'BRL')
         AND TCURR IN ('BRL', 'ARS', 'PYG', 'EUR', 'CHF').

    SELECT *
      FROM TCURR
      INTO TABLE TG_TCURR_LDAY
       WHERE GDATU EQ VG_LAST_DAY_AUX2
         AND KURST EQ 'B'
         AND FCURR IN ('USD', 'BRL')
         AND TCURR IN ('BRL', 'ARS', 'PYG', 'EUR', 'CHF').

    SELECT *
      FROM ZFIT0081
      INTO TABLE TG_0081
      WHERE BUKRS  IN S_BUKRS.

    IF P_VISU NE 'X'.
      TG_BSIK_BSID_AUX[] = TG_BSIK_BSID[].
      SORT TG_BSIK_BSID_AUX BY HKONT.
      DELETE ADJACENT DUPLICATES FROM TG_BSIK_BSID_AUX COMPARING HKONT.

      SELECT *
         FROM ZFIT0084
         INTO TABLE TG_0084
         FOR ALL ENTRIES IN  TG_BSIK_BSID_AUX
          WHERE BUKRS   IN S_BUKRS
            AND MES_ANO IN S_MES
            AND SAKNR   EQ TG_BSIK_BSID_AUX-HKONT .
      REFRESH TG_BSIK_BSID_AUX.
    ENDIF.

    IF SY-SUBRC IS INITIAL.
      REFRESH: TG_0084_AUX.
      TG_0084_AUX[] = TG_0084[].
      DELETE TG_0084_AUX WHERE OBJ_KEY IS INITIAL.
      IF TG_0084_AUX[] IS NOT INITIAL.
        SELECT *
          FROM ZIB_CONTABIL
          INTO TABLE TG_ZIB
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY.

        SELECT *
          FROM ZIB_CONTABIL_CHV
          INTO TABLE TG_ZIB_CHV
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY.

        SELECT *
          FROM ZIB_CONTABIL_ERR
          INTO TABLE TG_ZIB_ERR
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY.
      ENDIF.

      REFRESH: TG_0084_AUX.
      TG_0084_AUX[] = TG_0084[].
      DELETE TG_0084_AUX WHERE OBJ_KEY_INV IS INITIAL.
      IF TG_0084_AUX[] IS NOT INITIAL.
        SELECT *
          FROM ZIB_CONTABIL
           APPENDING TABLE TG_ZIB
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY_INV.

        SELECT *
          FROM ZIB_CONTABIL_CHV
          APPENDING TABLE TG_ZIB_CHV
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY_INV.

        SELECT *
          FROM ZIB_CONTABIL_ERR
          APPENDING TABLE TG_ZIB_ERR
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY_INV.
      ENDIF.

**             Estorno
      REFRESH: TG_0084_AUX.
      TG_0084_AUX[] = TG_0084[].
      DELETE TG_0084_AUX WHERE OBJ_KEY_EST IS INITIAL.
      IF TG_0084_AUX[] IS NOT INITIAL.
        SELECT *
          FROM ZIB_CONTABIL
          APPENDING TABLE TG_ZIB
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY_EST.

        SELECT *
          FROM ZIB_CONTABIL_CHV
          APPENDING TABLE TG_ZIB_CHV
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY_EST.

        SELECT *
          FROM ZIB_CONTABIL_ERR
          APPENDING TABLE TG_ZIB_ERR
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY_EST.
      ENDIF.

      REFRESH: TG_0084_AUX.
      TG_0084_AUX[] = TG_0084[].
      DELETE TG_0084_AUX WHERE OBJ_KEY_INV_EST IS INITIAL.
      IF TG_0084_AUX[] IS NOT INITIAL.
        SELECT *
          FROM ZIB_CONTABIL
          APPENDING TABLE TG_ZIB
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY_INV_EST.

        SELECT *
          FROM ZIB_CONTABIL_CHV
          APPENDING TABLE TG_ZIB_CHV
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY_INV_EST.

        SELECT *
          FROM ZIB_CONTABIL_ERR
          APPENDING TABLE TG_ZIB_ERR
           FOR ALL ENTRIES IN TG_0084_AUX
           WHERE OBJ_KEY EQ TG_0084_AUX-OBJ_KEY_INV_EST.
      ENDIF.

    ENDIF.
  ELSE.
*    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Para essa seleção não foram encontrados'
*                                           'dados para visualização.'.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-015
                                           TEXT-016.
    STOP.
  ENDIF.


ENDFORM.                    " SELECIONAR_DADOS
"
"alrs
FORM SELECIONAR_DADOS_R.
  DATA: WL_TABIX TYPE SY-TABIX.
  DATA: SO_DATA TYPE RANGE OF MKPF-BUDAT,
        WA_DATA LIKE LINE OF SO_DATA.

  SELECT *
       FROM ZFIT0081
       INTO TABLE TG_0081
        WHERE CTA_MONET EQ 'S'
          AND CONTA_DE NE ''
          AND BUKRS IN S_BUKRS.

** Processamento
  IF P_PROC IS NOT INITIAL OR P_REVE IS NOT INITIAL.
    VG_RYEAR  = S_MES-LOW+2(4).
    CONCATENATE VG_RYEAR S_MES-LOW(2) '01' INTO VG_LAST_DAY_AUX.
    VG_LAST_DAY = VG_LAST_DAY_AUX.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        I_DATE = VG_LAST_DAY
      IMPORTING
        E_DATE = VG_LAST_DAY.

    CONVERT INVERTED-DATE VG_LAST_DAY INTO DATE VG_LAST_DAY_AUX2.

*    IF  NOT '0101_0200' CS S_BUKRS-LOW.
    ADD 1 TO VG_LAST_DAY.

    CONVERT INVERTED-DATE VG_LAST_DAY INTO DATE VG_LAST_DAY_AUX.

*    ENDIF.

*    IF  NOT '0101_0200' CS S_BUKRS-LOW.
    SUBTRACT 1 FROM VG_LAST_DAY.
*    ENDIF.

    VG_FIRST_DAY = VG_LAST_DAY.

    ADD 1 TO VG_FIRST_DAY.

    SELECT *
      FROM SETLEAF
      INTO TABLE TG_SETLEAF
       WHERE SETNAME EQ 'MAGGI_ZFI061'.

*    SELECT *
*      FROM SETLEAF
*      INTO TABLE TG_SETLEAF_02
*       WHERE SETNAME EQ 'MAGGI_ZFI0063'.

    SELECT *
          FROM SETLEAF
          INTO TABLE TG_SETLEAF_02
           WHERE SETNAME IN ( 'MAGGI_ZFI0063' , 'CONTAS_EC-CS' ).


    IF SY-SUBRC NE 0.
      SELECT *                         "#EC CI_DB_OPERATION_OK[2431747]
           FROM T882G
           INTO TABLE TG_T882G
            WHERE RBUKRS IN S_BUKRS.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM T001
          INTO TABLE TG_T001
           FOR ALL ENTRIES IN TG_T882G
           WHERE BUKRS EQ TG_T882G-RBUKRS.
      ENDIF.
      EXIT.
    ENDIF.

    IF SY-SUBRC IS INITIAL.
      LOOP AT TG_SETLEAF INTO WG_SETLEAF.
        MOVE WG_SETLEAF-VALFROM TO WG_SETLEAF-KTOKS.
        MODIFY TG_SETLEAF FROM WG_SETLEAF.
        "
        WA_DATA-SIGN = 'I'.
        WA_DATA-OPTION = 'EQ'.
        WA_DATA-LOW = WG_SETLEAF-KTOKS.
        APPEND WA_DATA  TO SO_DATA.
      ENDLOOP.

      LOOP AT TG_SETLEAF_02 INTO WG_SETLEAF_02.
*        MOVE WG_SETLEAF_02-VALFROM TO WG_SETLEAF_02-KTOKS.
*        MODIFY TG_SETLEAF_02 FROM WG_SETLEAF_02.
        "
        WA_SAKNR-SAKNR = WG_SETLEAF_02-VALFROM.
        APPEND WA_SAKNR  TO IT_SAKNR.
      ENDLOOP.


      SELECT *                         "#EC CI_DB_OPERATION_OK[2431747]
            FROM T882G
            INTO TABLE TG_T882G
             WHERE RBUKRS IN S_BUKRS.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM T001
          INTO TABLE TG_T001
           FOR ALL ENTRIES IN TG_T882G
           WHERE BUKRS EQ TG_T882G-RBUKRS.

        IF SY-SUBRC IS INITIAL.
*          SELECT *
*            FROM SKA1
*            INTO TABLE TG_SKA1
*             FOR ALL ENTRIES IN TG_SETLEAF
*             WHERE KTOPL EQ '0050'
*               AND KTOKS EQ TG_SETLEAF-KTOKS
*               AND SAKNR IN S_SAKNRS.
          SELECT *                     "#EC CI_DB_OPERATION_OK[2389136]
          FROM SKA1                    "#EC CI_DB_OPERATION_OK[2431747]
          INTO TABLE TG_SKA1
           FOR ALL ENTRIES IN TG_0081
           WHERE KTOPL EQ '0050'
             AND SAKNR EQ TG_0081-CONTA_DE
             AND SAKNR IN S_SAKNRS
              AND KTOKS IN SO_DATA.

          IF SY-SUBRC IS INITIAL.
            SELECT *
              FROM SKAT
              INTO TABLE TG_SKAT
               FOR ALL ENTRIES IN TG_SKA1
               WHERE SAKNR EQ TG_SKA1-SAKNR
               AND KTOPL EQ '0050'
               AND SPRAS EQ SY-LANGU.

            SELECT *                   "#EC CI_DB_OPERATION_OK[2431747]
              FROM SKB1
              INTO TABLE TG_SKB1
               FOR ALL ENTRIES IN TG_SKA1
               WHERE SAKNR EQ TG_SKA1-SAKNR
                 AND BUKRS IN S_BUKRS.

            SELECT DISTINCT Racct RBUKRS
                         FROM ACDOCA
                         INTO TABLE TG_acdoca
                          FOR ALL ENTRIES IN TG_SKB1
                              WHERE RYEAR  EQ VG_RYEAR
                                AND RBUKRS IN S_BUKRS
                                AND RACCT  EQ TG_SKB1-SAKNR
                                AND RLDNR  EQ '0L'
                                AND AUGBL                  =  ''.
            "and substring( belnr,1,1 ) <> 'B'.
            "and substring( belnr,1,2 ) <> 'DG'.

            IF SY-SUBRC IS INITIAL.
              SELECT *
                FROM ZFIT0082
                INTO TABLE TG_0082
                 FOR ALL ENTRIES IN TG_SKB1
                 WHERE BUKRS   IN S_BUKRS
                   AND MES_ANO IN S_MES
                   AND SAKNR   EQ TG_SKB1-SAKNR.


              IF SY-SUBRC IS INITIAL.
                REFRESH: TG_0082_AUX.
                TG_0082_AUX[] = TG_0082[].
                DELETE TG_0082_AUX WHERE OBJ_KEY IS INITIAL.
                IF TG_0082_AUX[] IS NOT INITIAL.
                  SELECT *
                    FROM ZIB_CONTABIL
                    INTO TABLE TG_ZIB
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.

                  SELECT *
                    FROM ZIB_CONTABIL_CHV
                    INTO TABLE TG_ZIB_CHV
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.

                  SELECT *
                    FROM ZIB_CONTABIL_ERR
                    INTO TABLE TG_ZIB_ERR
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.
                ENDIF.
**             Estorno
                REFRESH: TG_0082_AUX.
                TG_0082_AUX[] = TG_0082[].
                DELETE TG_0082_AUX WHERE OBJ_KEY_EST IS INITIAL.
                IF TG_0082_AUX[] IS NOT INITIAL.
                  SELECT *
                    FROM ZIB_CONTABIL
                    APPENDING TABLE TG_ZIB
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.

                  SELECT *
                    FROM ZIB_CONTABIL_CHV
                    APPENDING TABLE TG_ZIB_CHV
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.

                  SELECT *
                    FROM ZIB_CONTABIL_ERR
                    APPENDING TABLE TG_ZIB_ERR
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.
                ENDIF.
              ENDIF.

              IF TG_SKB1[] IS NOT INITIAL.
                SELECT
                  DISTINCT RASSC,RACCT,RBUKRS
                  FROM FAGLFLEXT
                  INTO TABLE @TG_FAGLFLEXT
                   FOR ALL ENTRIES IN @TG_SKB1
                   WHERE RYEAR  EQ @VG_RYEAR
                     AND RBUKRS IN @S_BUKRS
                     AND RACCT  EQ @TG_SKB1-SAKNR
                     AND RLDNR  EQ '0L'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG

                DATA: V_MES TYPE ZDE_MES,
                      V_ANO TYPE ZDE_ANO.

                V_MES = S_MES-LOW(2).
                V_ANO = S_MES-LOW+2(4).

                SELECT *
                FROM ZFIT0185 INTO TABLE TG_ZFIT0185
                WHERE MES EQ V_MES
                   AND ANO EQ V_ANO
                   AND EMPRESA EQ S_BUKRS-LOW
                   AND CATEGORIA EQ 'B'
                   AND MOEDA_ORIGEM IN ('USD', 'BRL')
                   AND MOEDA_DESTINO IN ('BRL', 'ARS', 'PYG','EUR','CHF').

                SELECT *
             FROM ZFIT0185
             INTO TABLE TG_ZFIT0185_LDAY
             WHERE MES EQ V_MES
                AND ANO EQ V_ANO
                AND EMPRESA EQ S_BUKRS-LOW
                AND CATEGORIA EQ 'B'
                AND MOEDA_ORIGEM IN ('USD', 'BRL')
                AND MOEDA_DESTINO IN ('BRL', 'ARS', 'PYG','EUR','CHF').

                SELECT *
           FROM ZFIT0185 INTO TABLE TG_ZFIT0185
           WHERE MES EQ V_MES
              AND ANO EQ V_ANO
              AND EMPRESA EQ S_BUKRS-LOW
              AND CATEGORIA EQ 'B'
              AND MOEDA_ORIGEM IN ('USD', 'BRL')
              AND MOEDA_DESTINO IN ('BRL', 'ARS', 'PYG','EUR','CHF').

                SELECT *
                FROM ZFIT0185 INTO TABLE TG_ZFIT0185_LDAY
                  WHERE MES EQ V_MES
                     AND ANO EQ V_ANO
                     AND EMPRESA EQ S_BUKRS-LOW
                     AND CATEGORIA EQ 'B'
                     AND MOEDA_ORIGEM IN ('USD', 'BRL')
                     AND MOEDA_DESTINO IN ('BRL', 'ARS', 'PYG','EUR','CHF').

                "----------------------------------------------



                SELECT *
               FROM TCURR
               INTO TABLE TG_TCURR
                WHERE GDATU EQ VG_LAST_DAY_AUX
                  AND KURST EQ 'B'
                  AND FCURR IN ('USD', 'BRL')
                  AND TCURR IN ('BRL', 'ARS', 'PYG','EUR','CHF').

                SELECT *
                  FROM TCURR
                  INTO TABLE TG_TCURR_LDAY
                   WHERE GDATU EQ VG_LAST_DAY_AUX2
                     AND KURST EQ 'B'
                     AND FCURR IN ('USD', 'BRL')
                     AND TCURR IN ('BRL', 'ARS', 'PYG','EUR','CHF').

                SELECT *
                  FROM TCURR
                  INTO TABLE TG_TCURR
                   WHERE GDATU EQ VG_LAST_DAY_AUX
                     AND KURST EQ 'B'
                     AND FCURR IN ('USD', 'BRL')
                     AND TCURR IN ('BRL', 'ARS', 'PYG','EUR','CHF').

                SELECT *
                  FROM TCURR
                  INTO TABLE TG_TCURR_LDAY
                   WHERE GDATU EQ VG_LAST_DAY_AUX2
                     AND KURST EQ 'B'
                     AND FCURR IN ('USD', 'BRL')
                     AND TCURR IN ('BRL', 'ARS', 'PYG','EUR','CHF').



              ENDIF.

            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

** Visualização
  ELSE.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE TG_0082
      FROM ZFIT0082
      INNER JOIN SKB1 ON SKB1~BUKRS = ZFIT0082~BUKRS "#EC CI_DB_OPERATION_OK[2431747]
                     AND SKB1~SAKNR = ZFIT0082~SAKNR
                     AND SKB1~MITKZ = ''

       WHERE ZFIT0082~BUKRS   IN S_BUKRS
         AND ZFIT0082~MES_ANO IN S_MES
         AND ZFIT0082~SAKNR   IN S_SAKNRS.

    IF SY-SUBRC IS INITIAL.
      SELECT *                         "#EC CI_DB_OPERATION_OK[2431747]
        FROM T882G
        INTO TABLE TG_T882G
         WHERE RBUKRS IN S_BUKRS.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM T001
          INTO TABLE TG_T001
           FOR ALL ENTRIES IN TG_T882G
           WHERE BUKRS EQ TG_T882G-RBUKRS.
      ENDIF.
      SELECT *                         "#EC CI_DB_OPERATION_OK[2431747]
        FROM SKA1                      "#EC CI_DB_OPERATION_OK[2389136]
        INTO TABLE TG_SKA1
         FOR ALL ENTRIES IN TG_0082
         WHERE KTOPL EQ '0050'
           AND SAKNR EQ TG_0082-SAKNR.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM SKAT
          INTO TABLE TG_SKAT
           FOR ALL ENTRIES IN TG_SKA1
           WHERE SAKNR EQ TG_SKA1-SAKNR
           AND KTOPL EQ '0050'
           AND SPRAS EQ SY-LANGU.
      ENDIF.

      SELECT *
          FROM T001
          INTO TABLE TG_T001
           FOR ALL ENTRIES IN TG_0082
           WHERE BUKRS EQ TG_0082-BUKRS.

      REFRESH: TG_0082_AUX.
      TG_0082_AUX[] = TG_0082[].
      DELETE TG_0082_AUX WHERE OBJ_KEY IS INITIAL.
      IF TG_0082_AUX[] IS NOT INITIAL.
        SELECT *
          FROM ZIB_CONTABIL
          INTO TABLE TG_ZIB
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.

        SELECT *
          FROM ZIB_CONTABIL_CHV
          INTO TABLE TG_ZIB_CHV
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.

        SELECT *
          FROM ZIB_CONTABIL_ERR
          INTO TABLE TG_ZIB_ERR
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.
      ENDIF.
**             Estorno
      REFRESH: TG_0082_AUX.
      TG_0082_AUX[] = TG_0082[].
      DELETE TG_0082_AUX WHERE OBJ_KEY_EST IS INITIAL.
      IF TG_0082_AUX[] IS NOT INITIAL.
        SELECT *
          FROM ZIB_CONTABIL
          APPENDING TABLE TG_ZIB
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.

        SELECT *
          FROM ZIB_CONTABIL_CHV
          APPENDING TABLE TG_ZIB_CHV
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.

        SELECT *
          FROM ZIB_CONTABIL_ERR
          APPENDING TABLE TG_ZIB_ERR
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.
      ENDIF.

    ELSE.
*                                           'dados para visualização.'.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-015
                                             TEXT-016.
      STOP.
    ENDIF.
  ENDIF.


ENDFORM.                    " SELECIONAR_DADOS

FORM ORGANIZACAO_DADOS_R.

  DATA: WL_SALDO_MI  TYPE FAGLFLEXT-HSLVT,
        WL_SALDO_MI2 TYPE FAGLFLEXT-KSLVT,
        WL_SALDO_MI3 TYPE FAGLFLEXT-OSLVT,
        WL_SAIDAR    LIKE WG_SAIDAR,
        WL_UPDATE    TYPE SY-TABIX,
        VG_RACCT     TYPE FAGLFLEXT-RACCT,
        WL_FLAG.

  "Alerações
  DATA: IT_CONTAS         TYPE ZCT_EMP_CONTAS,
        WA_CONTAS         TYPE ZLC_EMP_CONTAS,
        IT_SALDO_CONTAS   TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
        WA_SALDO_CONTAS   TYPE ZDE_FI_GL_SALDO_FAGLFLEXT,
        IT_SALDO_CONTAS_2 TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
        IT_SALDO_CONTAS_3 TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
        REFE1	            TYPE HSLXX12,
        VMES              TYPE MONAT.

** Processamento
  IF P_PROC IS NOT INITIAL.
    SORT: TG_SKAT          BY SAKNR,
          TG_SKA1          BY SAKNR,
          TG_TCURR         BY FCURR TCURR,
          TG_TCURR_LDAY    BY FCURR TCURR,
          TG_ZFIT0185      BY MOEDA_ORIGEM MOEDA_DESTINO,
          TG_ZFIT0185_LDAY BY MOEDA_ORIGEM MOEDA_DESTINO,
          TG_T001          BY BUKRS,
          TG_T882G         BY RBUKRS,
          TG_0082          BY SAKNR VBUND,
          TG_ZIB_CHV       BY OBJ_KEY,
          TG_ZIB_ERR       BY OBJ_KEY.

*-US 156673-12-11-2024-#156673-RJF-Inicio
    TG_FAGLFLEXTG[] = TG_FAGLFLEXT[].
    SORT TG_ACDOCA[] BY RBUKRS RACCT.
    DATA: P_FUNC_SET TYPE CHAR1.
    CLEAR:P_FUNC_SET.
**********************************************************************
    "Verifica o SET para agrupar e calcular, se encontrar a conta limpa a empresa parceira para que seja agrupado no calculo da função
*    LOOP AT TG_ACDOCA ASSIGNING FIELD-SYMBOL(<FS_acdoca_AUX>).
*      SELECT SINGLE * FROM SETLEAF WHERE SETNAME in ( 'CONTAS_EC-CS', 'MAGGI_ZFI0063' ) AND VALFROM = @<FS_acdoca_AUX>-RACCT INTO @DATA(LR_SET).
*      IF SY-SUBRC = 0.
*        <FS_acdoca_AUX>-SOCPARC = 'S'.
*      ELSE.
*        <FS_acdoca_AUX>-SOCPARC = 'N'.
*      ENDIF.
*    ENDLOOP.
***********************************************************************
    " DELETE TG_ACDOCA WHERE SOCPARC = 'N' AND RASSC <> ''.
    DELETE ADJACENT DUPLICATES FROM TG_ACDOCA.

    CLEAR: WG_FAGLFLEXT, WG_ACDOCA.

    LOOP AT TG_ACDOCA INTO WG_ACDOCA.

      REFRESH: IT_SALDO_CONTAS,  IT_SALDO_CONTAS_2, IT_SALDO_CONTAS_3, IT_CONTAS.

      "Documento Processado não deve mais consultar pois vai alterar novamente
      "com base diferente, a variação ou valor em moeda forte irá mudar
      "IR56206 ALRS 13.03.2015
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WG_ACDOCA-RACCT
        IMPORTING
          OUTPUT = VG_RACCT.

      WA_CONTAS-BUKRS = S_BUKRS-LOW.
      WA_CONTAS-SAKNR = WG_ACDOCA-RACCT.
      APPEND WA_CONTAS TO IT_CONTAS.

      VG_RYEAR  = S_MES-LOW+2(4).

      READ TABLE IT_SAKNR INTO DATA(WA_SAKNR) WITH KEY SAKNR = WG_ACDOCA-RACCT.

      IF SY-SUBRC IS INITIAL. "possui sociedade PARCEIRA
        CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
          EXPORTING
            RYEAR                = VG_RYEAR
            CONTAS               = IT_CONTAS
            P_GERAR_SOC_PARCEIRA = 'X'
            P_GERAR_TODAS        = 'X'
          TABLES
            IT_SALDOS            = IT_SALDO_CONTAS
            IT_SALDOS_2          = IT_SALDO_CONTAS_2
            IT_SALDOS_3          = IT_SALDO_CONTAS_3
          EXCEPTIONS
            MOEDA_NAO_ADM        = 1
            ERRO_LEDGER          = 2
            OTHERS               = 3.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.


      ELSE.
        CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
          EXPORTING
            RYEAR         = VG_RYEAR
            CONTAS        = IT_CONTAS
            P_GERAR_TODAS = 'X'
          TABLES
            IT_SALDOS     = IT_SALDO_CONTAS
            IT_SALDOS_2   = IT_SALDO_CONTAS_2
            IT_SALDOS_3   = IT_SALDO_CONTAS_3
          EXCEPTIONS
            MOEDA_NAO_ADM = 1
            ERRO_LEDGER   = 2
            OTHERS        = 3.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ENDIF.

      LOOP AT IT_SALDO_CONTAS INTO WA_SALDO_CONTAS.
        CLEAR: WL_SALDO_MI, WL_SALDO_MI2, WL_SALDO_MI3,
      WL_UPDATE, WL_SAIDAR, WL_FLAG.

        READ TABLE IT_SAKNR INTO WA_SAKNR WITH KEY SAKNR =  WA_SALDO_CONTAS-RACCT.
        IF SY-SUBRC = 0.
          READ TABLE TG_0082 INTO WG_0082 WITH KEY SAKNR = WA_SALDO_CONTAS-RACCT
                                                   VBUND = WA_SALDO_CONTAS-RASSC.
        ELSE.
          READ TABLE TG_0082 INTO WG_0082 WITH KEY SAKNR = WA_SALDO_CONTAS-RACCT.
        ENDIF.
        IF ( SY-SUBRC IS INITIAL ) AND ( WG_0082-OBJ_KEY IS NOT INITIAL ) AND ( WG_0082-OBJ_KEY_EST IS INITIAL ).
          CONTINUE.
        ENDIF.
        CLEAR: WG_0082.

        READ TABLE IT_SAKNR INTO WA_SAKNR WITH KEY SAKNR =  WA_SALDO_CONTAS-RACCT.
        IF SY-SUBRC = 0.
          READ TABLE TG_SAIDAR INTO WL_SAIDAR WITH KEY RACCT = WA_SALDO_CONTAS-RACCT
                                                     RASSC = WA_SALDO_CONTAS-RASSC.
        ELSE.
          READ TABLE TG_SAIDAR INTO WL_SAIDAR WITH KEY RACCT = WA_SALDO_CONTAS-RACCT
                                                     RASSC = WA_SALDO_CONTAS-RASSC. " RJF "-US 156673-12-11-2024-#156673-RJF
        ENDIF.
        IF SY-SUBRC IS INITIAL.
          WL_UPDATE = SY-TABIX.
          ADD WL_SAIDAR-CURR1 TO WL_SALDO_MI.
          ADD WL_SAIDAR-CURR2 TO WL_SALDO_MI2.
          ADD WL_SAIDAR-CURR3 TO WL_SALDO_MI3.
        ENDIF.

        VMES = S_MES-LOW(2).
        IF VMES = 12.
          VMES = 15.

        ENDIF.

        READ TABLE IT_SALDO_CONTAS WITH KEY RACCT  = WA_SALDO_CONTAS-RACCT
                                            RASSC = WA_SALDO_CONTAS-RASSC.
        IF SY-SUBRC IS INITIAL.
          ADD WA_SALDO_CONTAS-SLVT TO WL_SALDO_MI.
          DO VMES TIMES VARYING REFE1 FROM WA_SALDO_CONTAS-SL01 NEXT WA_SALDO_CONTAS-SL02.
            ADD REFE1 TO WL_SALDO_MI.
          ENDDO.
        ENDIF.
        READ TABLE IT_SALDO_CONTAS_2 WITH KEY RACCT  = WA_SALDO_CONTAS-RACCT
                                              RASSC = WA_SALDO_CONTAS-RASSC.
        IF SY-SUBRC IS INITIAL.
          ADD IT_SALDO_CONTAS_2-SLVT TO WL_SALDO_MI2.
          DO VMES TIMES VARYING REFE1 FROM IT_SALDO_CONTAS_2-SL01 NEXT IT_SALDO_CONTAS_2-SL02.
            ADD REFE1 TO WL_SALDO_MI2.
          ENDDO.
        ENDIF.
        READ TABLE IT_SALDO_CONTAS_3 WITH KEY RACCT  = WA_SALDO_CONTAS-RACCT
                                              RASSC = WA_SALDO_CONTAS-RASSC.
        IF SY-SUBRC IS INITIAL.
          ADD IT_SALDO_CONTAS_3-SLVT TO WL_SALDO_MI3.
          DO VMES TIMES VARYING REFE1 FROM IT_SALDO_CONTAS_3-SL01 NEXT IT_SALDO_CONTAS_3-SL02.
            ADD REFE1 TO WL_SALDO_MI3.
          ENDDO.
        ENDIF.

        READ TABLE TG_T001 INTO WG_T001 WITH KEY BUKRS = WA_SALDO_CONTAS-rBUKRS BINARY SEARCH.

        IF WL_UPDATE IS INITIAL.
          READ TABLE TG_SKAT INTO WG_SKAT WITH KEY SAKNR = WA_SALDO_CONTAS-RACCT BINARY SEARCH.
          READ TABLE TG_SKA1 INTO WG_SKA1 WITH KEY SAKNR = WA_SALDO_CONTAS-RACCT BINARY SEARCH.
          "IR56206 ALRS 13.03.2015
          READ TABLE IT_SAKNR INTO WA_SAKNR WITH KEY SAKNR =  WA_SALDO_CONTAS-RACCT.
          IF SY-SUBRC = 0.

            "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
            READ TABLE TG_0082 INTO WG_0082 WITH KEY SAKNR = WA_SALDO_CONTAS-RACCT
                                                     VBUND = WA_SALDO_CONTAS-RASSC BINARY SEARCH.
          ELSE.
*            READ TABLE tg_0082 INTO wg_0082 WITH KEY saknr = WA_SALDO_CONTAS-RACCT BINARY SEARCH. "-US 156673-12-11-2024-#156673-RJF
            READ TABLE TG_0082 INTO WG_0082 WITH KEY SAKNR = WA_SALDO_CONTAS-RACCT "RJF "-US 156673-12-11-2024-#156673-RJF
                                                     VBUND = WA_SALDO_CONTAS-RASSC BINARY SEARCH.
          ENDIF.
          IF SY-SUBRC IS INITIAL.
            CLEAR: WG_ZIB_CHV.
            READ TABLE TG_ZIB_CHV INTO WG_ZIB_CHV WITH KEY OBJ_KEY = WG_0082-OBJ_KEY BINARY SEARCH.
            IF SY-SUBRC IS NOT INITIAL.
              READ TABLE TG_ZIB_ERR INTO WG_ZIB_ERR WITH KEY OBJ_KEY = WG_0082-OBJ_KEY BINARY SEARCH.
            ELSEIF WG_0082-OBJ_KEY_EST IS NOT INITIAL.   " estorno
              CLEAR: WG_ZIB_CHV.
              READ TABLE TG_ZIB_CHV  INTO WG_ZIB_CHV WITH KEY OBJ_KEY = WG_0082-OBJ_KEY_EST BINARY SEARCH.
              IF SY-SUBRC IS NOT INITIAL.
                READ TABLE TG_ZIB_ERR  INTO WG_ZIB_ERR WITH KEY OBJ_KEY = WG_0082-OBJ_KEY_EST BINARY SEARCH.
              ELSE.
                WG_SAIDAR-BELNR_EST = WG_ZIB_CHV-BELNR.
              ENDIF.
            ELSEIF WG_0082-OBJ_KEY_EST IS INITIAL.
              WG_SAIDAR-BELNR = WG_ZIB_CHV-BELNR.
            ENDIF.
          ENDIF.
          WG_SAIDAR-OBJ_KEY     = WG_0082-OBJ_KEY.
          WG_SAIDAR-OBJ_KEY_EST = WG_0082-OBJ_KEY_EST.
          WG_SAIDAR-RACCT       = WA_SALDO_CONTAS-RACCT.
          WG_SAIDAR-TXT50       = WG_SKAT-TXT50.
          WG_SAIDAR-KTOKS       = WG_SKA1-KTOKS.
        ENDIF.
        WG_SAIDAR-CURR1       = WL_SALDO_MI.
        WG_SAIDAR-CURR2       = WL_SALDO_MI2.

        CASE WG_T001-LAND1.
          WHEN 'BR'.
            "ajuste   CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - BG US 100617
            "Recupera
            CLEAR: WG_TCURR, WG_ZFIT0185.

            READ TABLE TG_ZFIT0185 INTO WG_ZFIT0185
              WITH KEY MOEDA_ORIGEM  = 'USD'
                       MOEDA_DESTINO = 'BRL'.
            "BINARY SEARCH.


            IF SY-SUBRC IS INITIAL.
              WG_SAIDAR-TX_USD = WG_ZFIT0185-TAXA.
              TRY.
                  IF WG_T001-BUKRS EQ '0101'.
                    WG_SAIDAR-SALDO_CORR  = ( ( WG_SAIDAR-CURR1 * 100 ) / WG_SAIDAR-TX_USD ).
                  ELSE.
                    WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_USD ).
                  ENDIF.
                CATCH CX_SY_ZERODIVIDE .
              ENDTRY.
              WG_SAIDAR-VLR_AJUST   = ( WG_SAIDAR-SALDO_CORR - WG_SAIDAR-CURR2 ).
            ELSE.

              READ TABLE TG_TCURR INTO WG_TCURR
                WITH KEY FCURR = 'USD'
                         TCURR = 'BRL'.
              "BINARY SEARCH.

              IF SY-SUBRC IS INITIAL.
                WG_SAIDAR-TX_USD = WG_TCURR-UKURS.
                TRY.
                    IF WG_T001-BUKRS EQ '0101'.
                      WG_SAIDAR-SALDO_CORR  = ( ( WG_SAIDAR-CURR1 * 100 ) / WG_SAIDAR-TX_USD ).
                    ELSE.
                      WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_USD ).
                    ENDIF.
                  CATCH CX_SY_ZERODIVIDE .
                ENDTRY.
                WG_SAIDAR-VLR_AJUST   = ( WG_SAIDAR-SALDO_CORR - WG_SAIDAR-CURR2 ).
              ENDIF.

            ENDIF.


          WHEN 'NL'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
            "Recupera
            CLEAR: WG_TCURR_LDAY.

            READ TABLE TG_ZFIT0185_LDAY INTO WG_ZFIT0185_LDAY
              WITH KEY MOEDA_ORIGEM = 'USD'
                       MOEDA_DESTINO = 'EUR'.
            "BINARY SEARCH.

            IF SY-SUBRC IS INITIAL.
              WG_SAIDAR-TX_USD = WG_ZFIT0185_LDAY-TAXA.
              IF WG_SAIDAR-TX_USD LT 0.
                MULTIPLY WG_SAIDAR-TX_USD BY -1.
                TRY.
                    WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 * WG_SAIDAR-TX_USD ).
                  CATCH CX_SY_ZERODIVIDE .
                ENDTRY.
              ELSE.
                TRY.
                    WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_USD ).
                  CATCH CX_SY_ZERODIVIDE .
                ENDTRY.
              ENDIF.

            ELSE.
              READ TABLE TG_TCURR_LDAY INTO WG_TCURR_LDAY
              WITH KEY FCURR = 'USD'
                       TCURR = 'EUR'.
              "BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                WG_SAIDAR-TX_USD = WG_TCURR_LDAY-UKURS.
                IF WG_SAIDAR-TX_USD LT 0.
                  MULTIPLY WG_SAIDAR-TX_USD BY -1.
                  TRY.
                      WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 * WG_SAIDAR-TX_USD ).
                    CATCH CX_SY_ZERODIVIDE .
                  ENDTRY.
                ELSE.
                  TRY.
                      WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_USD ).
                    CATCH CX_SY_ZERODIVIDE .
                  ENDTRY.
                ENDIF.

              ENDIF.
            ENDIF.


            WG_SAIDAR-VLR_AJUST   = ( WG_SAIDAR-SALDO_CORR - WG_SAIDAR-CURR2 ).


            "============================= INICIO DEVK9A0R37 - FI - [IR053183] Ajuste ZFI0063 - AOENNING
          WHEN 'LU'.
*          CLEAR: wg_tcurr_lday.
*          READ TABLE tg_tcurr_lday INTO wg_tcurr_lday

*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
            CLEAR: WG_TCURR, WG_ZFIT0185.

            READ TABLE TG_ZFIT0185 INTO WG_ZFIT0185
              WITH KEY MOEDA_ORIGEM = 'USD'
                       MOEDA_DESTINO = 'EUR'.
            "BINARY SEARCH.

            IF SY-SUBRC IS INITIAL.

              WG_SAIDAR-TX_USD = WG_ZFIT0185-TAXA.
              IF WG_SAIDAR-TX_USD LT 0.
                MULTIPLY WG_SAIDAR-TX_USD BY -1.
                TRY.
                    WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 * WG_SAIDAR-TX_USD ).
                  CATCH CX_SY_ZERODIVIDE .
                ENDTRY.
              ELSE.
                TRY.
                    WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_USD ).
                  CATCH CX_SY_ZERODIVIDE .
                ENDTRY.
              ENDIF.
              WG_SAIDAR-VLR_AJUST   = ( WG_SAIDAR-SALDO_CORR - WG_SAIDAR-CURR2 ).

            ELSE.

              READ TABLE TG_TCURR INTO WG_TCURR
              WITH KEY FCURR = 'USD'
                       TCURR = 'EUR'.
              "BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                WG_SAIDAR-TX_USD = WG_TCURR-UKURS.
                IF WG_SAIDAR-TX_USD LT 0.
                  MULTIPLY WG_SAIDAR-TX_USD BY -1.
                  TRY.
                      WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 * WG_SAIDAR-TX_USD ).
                    CATCH CX_SY_ZERODIVIDE .
                  ENDTRY.
                ELSE.
                  TRY.
                      WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_USD ).
                    CATCH CX_SY_ZERODIVIDE .
                  ENDTRY.
                ENDIF.
                WG_SAIDAR-VLR_AJUST   = ( WG_SAIDAR-SALDO_CORR - WG_SAIDAR-CURR2 ).
              ENDIF.

            ENDIF.


            "============================= INICIO DEVK9A0R37 - FI - [IR053183] Ajuste ZFI0063 - AOENNING
          WHEN 'CH'.
            "Recupera
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
            CLEAR: WG_TCURR_LDAY, WG_ZFIT0185_LDAY.

            READ TABLE TG_ZFIT0185_LDAY INTO WG_ZFIT0185_LDAY
              WITH KEY MOEDA_ORIGEM = 'USD'
                       MOEDA_DESTINO = 'CHF'.
            "BINARY SEARCH.

            IF SY-SUBRC IS INITIAL.
              WG_SAIDAR-TX_USD = WG_ZFIT0185_LDAY-TAXA.
              IF WG_SAIDAR-TX_USD LT 0.
                MULTIPLY WG_SAIDAR-TX_USD BY -1.
                TRY.
                    WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 * WG_SAIDAR-TX_USD ).
                  CATCH CX_SY_ZERODIVIDE .
                ENDTRY.
              ELSE.
                TRY.
                    WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_USD ).
                  CATCH CX_SY_ZERODIVIDE .
                ENDTRY.
              ENDIF.
            ELSE.
              READ TABLE TG_TCURR_LDAY INTO WG_TCURR_LDAY
              WITH KEY FCURR = 'USD'
                       TCURR = 'CHF'.
              "BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                WG_SAIDAR-TX_USD = WG_TCURR_LDAY-UKURS.
                IF WG_SAIDAR-TX_USD LT 0.
                  MULTIPLY WG_SAIDAR-TX_USD BY -1.
                  TRY.
                      WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 * WG_SAIDAR-TX_USD ).
                    CATCH CX_SY_ZERODIVIDE .
                  ENDTRY.
                ELSE.
                  TRY.
                      WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_USD ).
                    CATCH CX_SY_ZERODIVIDE .
                  ENDTRY.
                ENDIF.
              ENDIF.

            ENDIF.
            WG_SAIDAR-VLR_AJUST   = ( WG_SAIDAR-SALDO_CORR - WG_SAIDAR-CURR2 ).
          WHEN 'AR' OR 'PY'.

            WG_SAIDAR-CURR3 = WL_SALDO_MI3.
            IF WA_SALDO_CONTAS-rBUKRS EQ '0100'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
              CLEAR: WG_TCURR, WG_ZFIT0185.

              READ TABLE TG_ZFIT0185 INTO WG_ZFIT0185
              WITH KEY MOEDA_ORIGEM = 'USD'
                       MOEDA_DESTINO = 'ARS'.
              "BINARY SEARCH.

              IF SY-SUBRC IS INITIAL.
                WG_SAIDAR-TX_USD = WG_ZFIT0185-TAXA.
                TRY.
                    WG_SAIDAR-SALDO_CORR = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_USD ).
                  CATCH CX_SY_ZERODIVIDE.
                ENDTRY.
                WG_SAIDAR-VLR_AJUST = ( WG_SAIDAR-SALDO_CORR - WG_SAIDAR-CURR2 ).

              ELSE.
                READ TABLE TG_TCURR INTO WG_TCURR
                WITH KEY FCURR = 'USD'
                         TCURR = 'ARS'.
                "BINARY SEARCH.
                IF SY-SUBRC IS INITIAL.
                  WG_SAIDAR-TX_USD = WG_TCURR-UKURS.
                  TRY.
                      WG_SAIDAR-SALDO_CORR = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_USD ).
                    CATCH CX_SY_ZERODIVIDE.
                  ENDTRY.
                  WG_SAIDAR-VLR_AJUST = ( WG_SAIDAR-SALDO_CORR - WG_SAIDAR-CURR2 ).
                ENDIF.

              ENDIF.


              CLEAR: WG_TCURR, WG_ZFIT0185.

              READ TABLE TG_ZFIT0185 INTO WG_ZFIT0185
                WITH KEY MOEDA_ORIGEM = 'BRL'
                         MOEDA_DESTINO = 'ARS'.
              "BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                WG_SAIDAR-TX_BRL      = WG_ZFIT0185-TAXA.
                TRY.
                    WG_SAIDAR-SALDO_CORR2 = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_BRL ).
                  CATCH CX_SY_ZERODIVIDE.
                ENDTRY.
                WG_SAIDAR-VLR_AJUST2 = ( WG_SAIDAR-SALDO_CORR2 - WG_SAIDAR-CURR3 ).
              ELSE.
                READ TABLE TG_TCURR INTO WG_TCURR
                WITH KEY FCURR = 'BRL'
                         TCURR = 'ARS'.
                "BINARY SEARCH.
                IF SY-SUBRC IS INITIAL.
                  WG_SAIDAR-TX_BRL      = WG_TCURR-UKURS.
                  TRY.
                      WG_SAIDAR-SALDO_CORR2 = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_BRL ).
                    CATCH CX_SY_ZERODIVIDE.
                  ENDTRY.
                  WG_SAIDAR-VLR_AJUST2 = ( WG_SAIDAR-SALDO_CORR2 - WG_SAIDAR-CURR3 ).
                ENDIF.
              ENDIF.

            ELSEIF WA_SALDO_CONTAS-RBUKRS EQ '0101'.

              CLEAR: WG_TCURR, WG_ZFIT0185.
              READ TABLE TG_TCURR INTO WG_TCURR
                WITH KEY FCURR = 'USD'
                         TCURR = 'PYG'.
              "BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                WG_SAIDAR-TX_USD = WG_TCURR-UKURS.
                TRY.
                    IF WG_T001-BUKRS EQ '0101'.
                      WG_SAIDAR-SALDO_CORR  = ( ( WG_SAIDAR-CURR1 * 100 ) / WG_SAIDAR-TX_USD ).
                    ELSE.
                      WG_SAIDAR-SALDO_CORR  = ( WG_SAIDAR-CURR1 / WG_SAIDAR-TX_USD ).
                    ENDIF.
                  CATCH CX_SY_ZERODIVIDE.
                ENDTRY.
                WG_SAIDAR-VLR_AJUST = ( WG_SAIDAR-SALDO_CORR - WG_SAIDAR-CURR2 ).
              ENDIF.

              CLEAR: WG_TCURR, WG_ZFIT0185.

              READ TABLE TG_ZFIT0185 INTO WG_ZFIT0185
                WITH KEY MOEDA_ORIGEM = 'BRL'
                         MOEDA_DESTINO = 'PYG'.
              "BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.

                WG_SAIDAR-TX_BRL = WG_ZFIT0185-TAXA.
                TRY.
                    WG_SAIDAR-SALDO_CORR2 = ( ( WG_SAIDAR-CURR1 * 100 )  / WG_SAIDAR-TX_BRL ).
                  CATCH CX_SY_ZERODIVIDE.
                ENDTRY.
                WG_SAIDAR-VLR_AJUST2 = ( WG_SAIDAR-SALDO_CORR2 - WG_SAIDAR-CURR3 ).

              ELSE.
                READ TABLE TG_TCURR INTO WG_TCURR
                WITH KEY FCURR = 'BRL'
                         TCURR = 'PYG'.
                "BINARY SEARCH.
                IF SY-SUBRC IS INITIAL.
                  WG_SAIDAR-TX_BRL = WG_TCURR-UKURS.
                  TRY.
                      WG_SAIDAR-SALDO_CORR2 = ( ( WG_SAIDAR-CURR1 * 100 )  / WG_SAIDAR-TX_BRL ).
                    CATCH CX_SY_ZERODIVIDE.
                  ENDTRY.
                  WG_SAIDAR-VLR_AJUST2 = ( WG_SAIDAR-SALDO_CORR2 - WG_SAIDAR-CURR3 ).
                ENDIF.
              ENDIF.
            ENDIF.
        ENDCASE.

*        LOOP AT tg_0081 INTO wg_0081.
*          IF wg_0081-conta_de IS NOT INITIAL.
*            IF WA_SALDO_CONTAS-RACCT EQ wg_0081-conta_de.
*              wl_flag = c_x.
*              EXIT.
*            ENDIF.
*          ENDIF.
*          CLEAR:wg_0081.
*        ENDLOOP.

        READ TABLE TG_0081 INTO WG_0081 WITH KEY CONTA_DE = WA_SALDO_CONTAS-RACCT.
        IF SY-SUBRC = 0.
          WL_FLAG = C_X.
          "EXIT.
        ENDIF.

        IF WG_0081-CTA_MONET NE 'S'.
          CONTINUE.
        ENDIF.

        "IR56206 ALRS 13.03.2015
        "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
        READ TABLE IT_SAKNR INTO WA_SAKNR WITH KEY SAKNR =  WA_SALDO_CONTAS-RACCT.
        IF SY-SUBRC = 0.

          READ TABLE TG_0082 INTO WG_0082
           WITH KEY SAKNR = WA_SALDO_CONTAS-RACCT
                    VBUND = WG_FAGLFLEXT-RASSC BINARY SEARCH.
        ELSE.
*          READ TABLE tg_0082 INTO wg_0082 "-US 156673-12-11-2024-#156673-RJF
*            WITH KEY saknr = WA_SALDO_CONTAS-RACCT BINARY SEARCH. "-US 156673-12-11-2024-#156673-RJF

          READ TABLE TG_0082 INTO WG_0082   "RJF "-US 156673-12-11-2024-#156673-RJF
           WITH KEY SAKNR = WA_SALDO_CONTAS-RACCT
                    VBUND = WG_FAGLFLEXT-RASSC BINARY SEARCH.
        ENDIF.
        " Se for estorno recupera valor de ajuste
        IF SY-SUBRC = 0.
          IF WG_0082-OBJ_KEY_EST IS INITIAL AND WG_0082-OBJ_KEY IS NOT INITIAL.
            WG_SAIDAR-VLR_AJUST  = WG_0082-VLR_CORR_MI2.
            WG_SAIDAR-VLR_AJUST2 = WG_0082-VLR_CORR_MI3.
          ENDIF.
        ENDIF.

        "-US 156673-12-11-2024-#156673-RJF- Inicio
        "IR56206 ALRS 13.03.2015
        "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
*        READ TABLE it_saknr INTO wa_saknr WITH KEY saknr =  WA_SALDO_CONTAS-RACCT.
*        IF sy-subrc = 0.
*
*          wg_saidar-rassc = wg_faglflext-rassc.
*        ENDIF.

        WG_SAIDAR-RASSC = WA_SALDO_CONTAS-RASSC. "RJF "-US 156673-12-11-2024-#156673-RJF
        "-US 156673-12-11-2024-#156673-RJF-fim
        IF WL_FLAG IS NOT INITIAL.
          IF WL_UPDATE IS INITIAL.
            APPEND WG_SAIDAR TO TG_SAIDAR.
          ELSE.
            MODIFY TG_SAIDAR FROM WG_SAIDAR INDEX WL_UPDATE TRANSPORTING RASSC CURR1 CURR2 CURR3 TX_USD TX_BRL SALDO_CORR SALDO_CORR2 VLR_AJUST VLR_AJUST2.
          ENDIF.
        ENDIF.
        CLEAR: WG_SAIDAR, WG_T001, WG_SKA1, WG_SKAT, WG_0082, WL_FLAG,WG_FAGLFLEXT, WG_ACDOCA, WA_SALDO_CONTAS.
      ENDLOOP.
    ENDLOOP.

*------------ FIM LOOP --------------------------------------------------------------------------------------------------------------------------------------
*-------------------------------------------------------------------------------------------------------------------------------------------------------------

    "ENDLOOP. "-US 156673-12-11-2024-#156673-RJF
    "10.09.2015 ALRS
    IF S_BUKRS-LOW EQ '0101'.
      LOOP AT  TG_SAIDAR INTO WG_SAIDAR.
        MULTIPLY WG_SAIDAR-CURR1 BY 100.
        MODIFY TG_SAIDAR FROM WG_SAIDAR INDEX SY-TABIX TRANSPORTING CURR1.
      ENDLOOP.
    ENDIF.

    "**********************************************************************************************
    " Ajusta Saida de Documento Processado """"""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT TG_0082 INTO WG_0082.
      "IR56206 ALRS 13.03.2015
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WG_0082-SAKNR
        IMPORTING
          OUTPUT = VG_RACCT.
      "IF '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
      READ TABLE IT_SAKNR INTO WA_SAKNR WITH KEY SAKNR = WG_0082-SAKNR.
      IF SY-SUBRC = 0.

        READ TABLE TG_SAIDAR INTO WL_SAIDAR WITH KEY RACCT = WG_0082-SAKNR
                                                   RASSC = WG_0082-VBUND.
      ELSE.
        READ TABLE TG_SAIDAR INTO WL_SAIDAR WITH KEY RACCT = WG_0082-SAKNR
                                                   RASSC = WG_0082-VBUND. "RJF "-US 156673-12-11-2024-#156673-RJF
      ENDIF.
      IF SY-SUBRC IS NOT INITIAL.
        READ TABLE TG_SKAT INTO WG_SKAT WITH KEY SAKNR = WG_0082-SAKNR BINARY SEARCH.
        READ TABLE TG_SKA1 INTO WG_SKA1 WITH KEY SAKNR = WG_0082-SAKNR BINARY SEARCH.
        WL_SAIDAR-RACCT        = WG_0082-SAKNR.
        "IR56206 ALRS 13.03.2015
        WL_SAIDAR-RASSC        = WG_0082-VBUND.
        WL_SAIDAR-TXT50        = WG_SKAT-TXT50.
        WL_SAIDAR-KTOKS        = WG_SKA1-KTOKS.

        WL_SAIDAR-CURR1        = WG_0082-DMBTR.


        WL_SAIDAR-CURR2        = WG_0082-DMBE2.
        WL_SAIDAR-CURR3        = WG_0082-DMBE3.
        WL_SAIDAR-TX_USD       = WG_0082-TX_USD.
        WL_SAIDAR-TX_BRL       = WG_0082-TX_BRL.
        WL_SAIDAR-SALDO_CORR   = WG_0082-SDO_CORR_MI2.
        WL_SAIDAR-SALDO_CORR2  = WG_0082-SDO_CORR_MI3.
        WL_SAIDAR-VLR_AJUST    = WG_0082-VLR_CORR_MI2.
        WL_SAIDAR-VLR_AJUST2   = WG_0082-VLR_CORR_MI3.
        WL_SAIDAR-OBJ_KEY      = WG_0082-OBJ_KEY.
        WL_SAIDAR-OBJ_KEY_EST  = WG_0082-OBJ_KEY_EST.
        READ TABLE TG_ZIB_CHV INTO WG_ZIB_CHV WITH KEY OBJ_KEY = WG_0082-OBJ_KEY BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WL_SAIDAR-BELNR = WG_ZIB_CHV-BELNR.
        ENDIF.
        APPEND WL_SAIDAR TO TG_SAIDAR.
      ENDIF.
    ENDLOOP.

    SORT TG_SAIDAR BY RACCT.


    PERFORM ATUALIZA_SAIDAR TABLES TG_SAIDAR
                                   TG_ZIB
                                   TG_ZIB_CHV
                                   TG_ZIB_ERR.

    IF TG_SAIDAR[] IS INITIAL.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-015
                                             TEXT-016.
      STOP.
    ENDIF.

** Visualização
  ELSE.
    SORT: TG_SKAT    BY SAKNR,
          TG_SKA1    BY SAKNR,
          TG_T001    BY BUKRS,
          TG_ZIB_CHV BY OBJ_KEY,
          TG_ZIB_ERR BY OBJ_KEY.

    LOOP AT TG_0082 INTO WG_0082.
      CLEAR: WL_FLAG.

      READ TABLE TG_SKAT INTO WG_SKAT WITH KEY SAKNR = WG_0082-SAKNR BINARY SEARCH.
      READ TABLE TG_SKA1 INTO WG_SKA1 WITH KEY SAKNR = WG_0082-SAKNR BINARY SEARCH.

      WG_SAIDAR-RACCT        = WG_0082-SAKNR.
      WG_SAIDAR-TXT50        = WG_SKAT-TXT50.
      WG_SAIDAR-KTOKS        = WG_SKA1-KTOKS.
      WG_SAIDAR-CURR1        = WG_0082-DMBTR.
      WG_SAIDAR-CURR2        = WG_0082-DMBE2.
      WG_SAIDAR-CURR3        = WG_0082-DMBE3.
      WG_SAIDAR-TX_USD       = WG_0082-TX_USD.
      WG_SAIDAR-SALDO_CORR   = WG_0082-SDO_CORR_MI2.
      WG_SAIDAR-VLR_AJUST    = WG_0082-VLR_CORR_MI2.
      WG_SAIDAR-TX_BRL       = WG_0082-TX_BRL.
      WG_SAIDAR-SALDO_CORR2  = WG_0082-SDO_CORR_MI3.
      WG_SAIDAR-VLR_AJUST2   = WG_0082-VLR_CORR_MI3.
      WG_SAIDAR-OBJ_KEY      = WG_0082-OBJ_KEY.
      WG_SAIDAR-OBJ_KEY_EST  = WG_0082-OBJ_KEY_EST.

      LOOP AT TG_0081 INTO WG_0081.

        IF WG_0081-CONTA_DE IS NOT INITIAL.
          IF WG_SAIDAR-RACCT EQ WG_0081-CONTA_DE.
            WL_FLAG = C_X.
            EXIT.
          ENDIF.
        ENDIF.
        CLEAR:WG_0081.
      ENDLOOP.
      IF WL_FLAG IS NOT INITIAL.
        APPEND WG_SAIDAR TO TG_SAIDAR.
      ENDIF.
      CLEAR: WG_SAIDAR, WG_SKA1, WG_SKAT, WL_FLAG.
    ENDLOOP.

    PERFORM ATUALIZA_SAIDAR TABLES TG_SAIDAR
                                  TG_ZIB
                                  TG_ZIB_CHV
                                  TG_ZIB_ERR.

  ENDIF.



** Processamento
  IF P_PROC IS NOT INITIAL.
    IF S_BUKRS-LOW EQ '0101'.
*      LOOP AT  TG_SAIDA INTO WG_SAIDA.
*        MULTIPLY WG_SAIDA-CURR1 BY 100.
*        MODIFY TG_SAIDA FROM WG_SAIDA INDEX SY-TABIX TRANSPORTING CURR1.
*      ENDLOOP.
    ELSEIF S_BUKRS-LOW EQ '0004' OR S_BUKRS-LOW EQ '0037'. "recalcula ajuste
      LOOP AT  TG_SAIDAR INTO WG_SAIDAR.
        TABIX = SY-TABIX.
        CLEAR WG_0082.
        READ TABLE TG_0082 INTO WG_0082
              WITH KEY SAKNR = WG_SAIDAR-RACCT  BINARY SEARCH.
        " Se for estorno recupera valor de ajuste
        IF SY-SUBRC NE 0
          OR  ( WG_0082-OBJ_KEY_EST IS NOT INITIAL OR  WG_0082-OBJ_KEY IS NOT INITIAL ).
          WG_SAIDAR-SALDO_CORR = WG_SAIDAR-CURR2 * WG_SAIDAR-TX_USD.
          WG_SAIDAR-VLR_AJUST  = WG_SAIDAR-SALDO_CORR - WG_SAIDAR-CURR1.
          MODIFY TG_SAIDAR FROM WG_SAIDAR INDEX TABIX TRANSPORTING VLR_AJUST SALDO_CORR.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " ORGANIZACAO_DADOS

*form organizacao_dados_r.
*  data: wl_saldo_mi  type faglflext-hslvt,
*        wl_saldo_mi2 type faglflext-kslvt,
*        wl_saldo_mi3 type faglflext-oslvt,
*        wl_saidar    like wg_saidar,
*        wl_update    type sy-tabix,
*        vg_racct     type faglflext-racct,
*        wl_flag.
*
*** Processamento
*  if p_proc is not initial.
*    sort: tg_skat          by saknr,
*          tg_ska1          by saknr,
*          tg_tcurr         by fcurr tcurr,
*          tg_tcurr_lday    by fcurr tcurr,
*          tg_zfit0185      by moeda_origem moeda_destino,
*          tg_zfit0185_lday by moeda_origem moeda_destino,
*          tg_t001          by bukrs,
*          tg_t882g         by rbukrs,
*          tg_0082          by saknr vbund,
*          tg_zib_chv       by obj_key,
*          tg_zib_err       by obj_key.
*
*    loop at tg_faglflext into wg_faglflext.
*      clear: wl_saldo_mi, wl_saldo_mi2, wl_saldo_mi3,
*      wl_update, wl_saidar, wl_flag.
*
*      "Documento Processado não deve mais consultar pois vai alterar novamente
*      "com base diferente, a variação ou valor em moeda forte irá mudar
*      "IR56206 ALRS 13.03.2015
*      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        exporting
*          input  = wg_faglflext-racct
*        importing
*          output = vg_racct.
*
*      "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
*      read table it_saknr into wa_saknr with key saknr =  wg_faglflext-racct.
*      if sy-subrc = 0.
*        read table tg_0082 into wg_0082 with key saknr = wg_faglflext-racct
*                                                 vbund = wg_faglflext-rassc.
*      else.
*        read table tg_0082 into wg_0082 with key saknr = wg_faglflext-racct.
*      endif.
*      if ( sy-subrc is initial ) and ( wg_0082-obj_key is not initial ) and ( wg_0082-obj_key_est is initial ).
*        continue.
*      endif.
*      clear: wg_0082.
*
*      "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
*      read table it_saknr into wa_saknr with key saknr =  wg_faglflext-racct.
*      if sy-subrc = 0.
*        read table tg_saidar into wl_saidar with key racct = wg_faglflext-racct
*                                                   rassc = wg_faglflext-rassc.
*      else.
*        read table tg_saidar into wl_saidar with key racct = wg_faglflext-racct.
*      endif.
*      if sy-subrc is initial.
*        wl_update = sy-tabix.
*        add wl_saidar-curr1 to wl_saldo_mi.
*        add wl_saidar-curr2 to wl_saldo_mi2.
*        add wl_saidar-curr3 to wl_saldo_mi3.
*      endif.
*
***  Saldo Mi1 ***************************************************************
*      add wg_faglflext-hslvt to wl_saldo_mi.
*      if s_mes-low(2) ge '01'.
*        add wg_faglflext-hsl01 to wl_saldo_mi.
*      endif.
*      if s_mes-low(2) ge '02'.
*        add wg_faglflext-hsl02 to wl_saldo_mi.
*      endif.
*      if s_mes-low(2) ge '03'.
*        add wg_faglflext-hsl03 to wl_saldo_mi.
*      endif.
*      if s_mes-low(2) ge '04'.
*        add wg_faglflext-hsl04 to wl_saldo_mi.
*      endif.
*      if s_mes-low(2) ge '05'.
*        add wg_faglflext-hsl05 to wl_saldo_mi.
*      endif.
*      if s_mes-low(2) ge '06'.
*        add wg_faglflext-hsl06 to wl_saldo_mi.
*      endif.
*      if s_mes-low(2) ge '07'.
*        add wg_faglflext-hsl07 to wl_saldo_mi.
*      endif.
*      if s_mes-low(2) ge '08'.
*        add wg_faglflext-hsl08 to wl_saldo_mi.
*      endif.
*      if s_mes-low(2) ge '09'.
*        add wg_faglflext-hsl09 to wl_saldo_mi.
*      endif.
*      if s_mes-low(2) ge '10'.
*        add wg_faglflext-hsl10 to wl_saldo_mi.
*      endif.
*      if s_mes-low(2) ge '11'.
*        add wg_faglflext-hsl11 to wl_saldo_mi.
*      endif.
*      if s_mes-low(2) ge '12'.
*        add wg_faglflext-hsl12 to wl_saldo_mi.
*        add wg_faglflext-hsl13 to wl_saldo_mi.
*        add wg_faglflext-hsl14 to wl_saldo_mi.
*        add wg_faglflext-hsl15 to wl_saldo_mi.
*        add wg_faglflext-hsl16 to wl_saldo_mi.
*      endif.
*
***  Saldo Mi2 *****************************************************************
*      add wg_faglflext-kslvt to wl_saldo_mi2.
*      if s_mes-low(2) ge '01'.
*        add wg_faglflext-ksl01 to wl_saldo_mi2.
*      endif.
*      if s_mes-low(2) ge '02'.
*        add wg_faglflext-ksl02 to wl_saldo_mi2.
*      endif.
*      if s_mes-low(2) ge '03'.
*        add wg_faglflext-ksl03 to wl_saldo_mi2.
*      endif.
*      if s_mes-low(2) ge '04'.
*        add wg_faglflext-ksl04 to wl_saldo_mi2.
*      endif.
*      if s_mes-low(2) ge '05'.
*        add wg_faglflext-ksl05 to wl_saldo_mi2.
*      endif.
*      if s_mes-low(2) ge '06'.
*        add wg_faglflext-ksl06 to wl_saldo_mi2.
*      endif.
*      if s_mes-low(2) ge '07'.
*        add wg_faglflext-ksl07 to wl_saldo_mi2.
*      endif.
*      if s_mes-low(2) ge '08'.
*        add wg_faglflext-ksl08 to wl_saldo_mi2.
*      endif.
*      if s_mes-low(2) ge '09'.
*        add wg_faglflext-ksl09 to wl_saldo_mi2.
*      endif.
*      if s_mes-low(2) ge '10'.
*        add wg_faglflext-ksl10 to wl_saldo_mi2.
*      endif.
*      if s_mes-low(2) ge '11'.
*        add wg_faglflext-ksl11 to wl_saldo_mi2.
*      endif.
*      if s_mes-low(2) ge '12'.
*        add wg_faglflext-ksl12 to wl_saldo_mi2.
*        add wg_faglflext-ksl13 to wl_saldo_mi2.
*        add wg_faglflext-ksl14 to wl_saldo_mi2.
*        add wg_faglflext-ksl15 to wl_saldo_mi2.
*        add wg_faglflext-ksl16 to wl_saldo_mi2.
*      endif.
*
*      read table tg_t001 into wg_t001 with key bukrs = wg_faglflext-rbukrs binary search.
*
*      if wl_update is initial.
*        read table tg_skat into wg_skat with key saknr = wg_faglflext-racct binary search.
*        read table tg_ska1 into wg_ska1 with key saknr = wg_faglflext-racct binary search.
*        "IR56206 ALRS 13.03.2015
*        read table it_saknr into wa_saknr with key saknr =  wg_faglflext-racct.
*        if sy-subrc = 0.
*
*          "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
*          read table tg_0082 into wg_0082 with key saknr = wg_faglflext-racct
*                                                   vbund = wg_faglflext-rassc binary search.
*        else.
*          read table tg_0082 into wg_0082 with key saknr = wg_faglflext-racct binary search.
*        endif.
*        if sy-subrc is initial.
*          clear: wg_zib_chv.
*          read table tg_zib_chv into wg_zib_chv with key obj_key = wg_0082-obj_key binary search.
*          if sy-subrc is not initial.
*            read table tg_zib_err into wg_zib_err with key obj_key = wg_0082-obj_key binary search.
*          elseif wg_0082-obj_key_est is not initial.   " estorno
*            clear: wg_zib_chv.
*            read table tg_zib_chv  into wg_zib_chv with key obj_key = wg_0082-obj_key_est binary search.
*            if sy-subrc is not initial.
*              read table tg_zib_err  into wg_zib_err with key obj_key = wg_0082-obj_key_est binary search.
*            else.
*              wg_saidar-belnr_est = wg_zib_chv-belnr.
*            endif.
*          elseif wg_0082-obj_key_est is initial.
*            wg_saidar-belnr = wg_zib_chv-belnr.
*          endif.
*        endif.
*        wg_saidar-obj_key     = wg_0082-obj_key.
*        wg_saidar-obj_key_est = wg_0082-obj_key_est.
*        wg_saidar-racct       = wg_faglflext-racct.
*        wg_saidar-txt50       = wg_skat-txt50.
*        wg_saidar-ktoks       = wg_ska1-ktoks.
*      endif.
*      wg_saidar-curr1       = wl_saldo_mi.
*      wg_saidar-curr2       = wl_saldo_mi2.
*
*      case wg_t001-land1.
*        when 'BR'.
*          "ajuste   CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - BG US 100617
*          "Recupera
*          clear: wg_tcurr, wg_zfit0185.
*
*          read table tg_zfit0185 into wg_zfit0185
*            with key moeda_origem  = 'USD'
*                     moeda_destino = 'BRL'.
*          "BINARY SEARCH.
*
*
*          if sy-subrc is initial.
*            wg_saidar-tx_usd = wg_zfit0185-taxa.
*            try.
*                if wg_t001-bukrs eq '0101'.
*                  wg_saidar-saldo_corr  = ( ( wg_saidar-curr1 * 100 ) / wg_saidar-tx_usd ).
*                else.
*                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
*                endif.
*              catch cx_sy_zerodivide .
*            endtry.
*            wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
*          else.
*
*            read table tg_tcurr into wg_tcurr
*              with key fcurr = 'USD'
*                       tcurr = 'BRL'.
*            "BINARY SEARCH.
*
*            if sy-subrc is initial.
*              wg_saidar-tx_usd = wg_tcurr-ukurs.
*              try.
*                  if wg_t001-bukrs eq '0101'.
*                    wg_saidar-saldo_corr  = ( ( wg_saidar-curr1 * 100 ) / wg_saidar-tx_usd ).
*                  else.
*                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
*                  endif.
*                catch cx_sy_zerodivide .
*              endtry.
*              wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
*            endif.
*
*          endif.
*
*
*        when 'NL'.
**ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
*          "Recupera
*          clear: wg_tcurr_lday.
*
*          read table tg_zfit0185_lday into wg_zfit0185_lday
*            with key moeda_origem = 'USD'
*                     moeda_destino = 'EUR'.
*          "BINARY SEARCH.
*
*          if sy-subrc is initial.
*            wg_saidar-tx_usd = wg_zfit0185_lday-taxa.
*            if wg_saidar-tx_usd lt 0.
*              multiply wg_saidar-tx_usd by -1.
*              try.
*                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
*                catch cx_sy_zerodivide .
*              endtry.
*            else.
*              try.
*                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
*                catch cx_sy_zerodivide .
*              endtry.
*            endif.
*
*          else.
*            read table tg_tcurr_lday into wg_tcurr_lday
*            with key fcurr = 'USD'
*                     tcurr = 'EUR'.
*            "BINARY SEARCH.
*            if sy-subrc is initial.
*              wg_saidar-tx_usd = wg_tcurr_lday-ukurs.
*              if wg_saidar-tx_usd lt 0.
*                multiply wg_saidar-tx_usd by -1.
*                try.
*                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
*                  catch cx_sy_zerodivide .
*                endtry.
*              else.
*                try.
*                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
*                  catch cx_sy_zerodivide .
*                endtry.
*              endif.
*
*            endif.
*          endif.
*
*
*          wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
*
*
*          "============================= INICIO DEVK9A0R37 - FI - [IR053183] Ajuste ZFI0063 - AOENNING
*        when 'LU'.
**          CLEAR: wg_tcurr_lday.
**          READ TABLE tg_tcurr_lday INTO wg_tcurr_lday
*
**ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
*          clear: wg_tcurr, wg_zfit0185.
*
*          read table tg_zfit0185 into wg_zfit0185
*            with key moeda_origem = 'USD'
*                     moeda_destino = 'EUR'.
*          "BINARY SEARCH.
*
*          if sy-subrc is initial.
*
*            wg_saidar-tx_usd = wg_zfit0185-taxa.
*            if wg_saidar-tx_usd lt 0.
*              multiply wg_saidar-tx_usd by -1.
*              try.
*                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
*                catch cx_sy_zerodivide .
*              endtry.
*            else.
*              try.
*                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
*                catch cx_sy_zerodivide .
*              endtry.
*            endif.
*            wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
*
*          else.
*
*            read table tg_tcurr into wg_tcurr
*            with key fcurr = 'USD'
*                     tcurr = 'EUR'.
*            "BINARY SEARCH.
*            if sy-subrc is initial.
*              wg_saidar-tx_usd = wg_tcurr-ukurs.
*              if wg_saidar-tx_usd lt 0.
*                multiply wg_saidar-tx_usd by -1.
*                try.
*                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
*                  catch cx_sy_zerodivide .
*                endtry.
*              else.
*                try.
*                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
*                  catch cx_sy_zerodivide .
*                endtry.
*              endif.
*              wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
*            endif.
*
*          endif.
*
*
*          "============================= INICIO DEVK9A0R37 - FI - [IR053183] Ajuste ZFI0063 - AOENNING
*        when 'CH'.
*          "Recupera
**ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
*          clear: wg_tcurr_lday, wg_zfit0185_lday.
*
*          read table tg_zfit0185_lday into wg_zfit0185_lday
*            with key moeda_origem = 'USD'
*                     moeda_destino = 'CHF'.
*          "BINARY SEARCH.
*
*          if sy-subrc is initial.
*            wg_saidar-tx_usd = wg_zfit0185_lday-taxa.
*            if wg_saidar-tx_usd lt 0.
*              multiply wg_saidar-tx_usd by -1.
*              try.
*                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
*                catch cx_sy_zerodivide .
*              endtry.
*            else.
*              try.
*                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
*                catch cx_sy_zerodivide .
*              endtry.
*            endif.
*          else.
*            read table tg_tcurr_lday into wg_tcurr_lday
*            with key fcurr = 'USD'
*                     tcurr = 'CHF'.
*            "BINARY SEARCH.
*            if sy-subrc is initial.
*              wg_saidar-tx_usd = wg_tcurr_lday-ukurs.
*              if wg_saidar-tx_usd lt 0.
*                multiply wg_saidar-tx_usd by -1.
*                try.
*                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
*                  catch cx_sy_zerodivide .
*                endtry.
*              else.
*                try.
*                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
*                  catch cx_sy_zerodivide .
*                endtry.
*              endif.
*            endif.
*
*          endif.
*          wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
*        when 'AR' or 'PY'.
*
***  Saldo Mi3 ****************************************************************************
*          add wg_faglflext-oslvt to wl_saldo_mi3.
*          if s_mes-low(2) ge '01'.
*            add wg_faglflext-osl01 to wl_saldo_mi3.
*          endif.
*          if s_mes-low(2) ge '02'.
*            add wg_faglflext-osl02 to wl_saldo_mi3.
*          endif.
*          if s_mes-low(2) ge '03'.
*            add wg_faglflext-osl03 to wl_saldo_mi3.
*          endif.
*          if s_mes-low(2) ge '04'.
*            add wg_faglflext-osl04 to wl_saldo_mi3.
*          endif.
*          if s_mes-low(2) ge '05'.
*            add wg_faglflext-osl05 to wl_saldo_mi3.
*          endif.
*          if s_mes-low(2) ge '06'.
*            add wg_faglflext-osl06 to wl_saldo_mi3.
*          endif.
*          if s_mes-low(2) ge '07'.
*            add wg_faglflext-osl07 to wl_saldo_mi3.
*          endif.
*          if s_mes-low(2) ge '08'.
*            add wg_faglflext-osl08 to wl_saldo_mi3.
*          endif.
*          if s_mes-low(2) ge '09'.
*            add wg_faglflext-osl09 to wl_saldo_mi3.
*          endif.
*          if s_mes-low(2) ge '10'.
*            add wg_faglflext-osl10 to wl_saldo_mi3.
*          endif.
*          if s_mes-low(2) ge '11'.
*            add wg_faglflext-osl11 to wl_saldo_mi3.
*          endif.
*          if s_mes-low(2) ge '12'.
*            add wg_faglflext-osl12 to wl_saldo_mi3.
*            add wg_faglflext-osl13 to wl_saldo_mi3.
*            add wg_faglflext-osl14 to wl_saldo_mi3.
*            add wg_faglflext-osl15 to wl_saldo_mi3.
*            add wg_faglflext-osl16 to wl_saldo_mi3.
*          endif.
*
*
*          wg_saidar-curr3 = wl_saldo_mi3.
*          if wg_faglflext-rbukrs eq '0100'.
**ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
*            clear: wg_tcurr, wg_zfit0185.
*
*            read table tg_zfit0185 into wg_zfit0185
*            with key moeda_origem = 'USD'
*                     moeda_destino = 'ARS'.
*            "BINARY SEARCH.
*
*            if sy-subrc is initial.
*              wg_saidar-tx_usd = wg_zfit0185-taxa.
*              try.
*                  wg_saidar-saldo_corr = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
*                catch cx_sy_zerodivide.
*              endtry.
*              wg_saidar-vlr_ajust = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
*
*            else.
*              read table tg_tcurr into wg_tcurr
*              with key fcurr = 'USD'
*                       tcurr = 'ARS'.
*              "BINARY SEARCH.
*              if sy-subrc is initial.
*                wg_saidar-tx_usd = wg_tcurr-ukurs.
*                try.
*                    wg_saidar-saldo_corr = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
*                  catch cx_sy_zerodivide.
*                endtry.
*                wg_saidar-vlr_ajust = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
*              endif.
*
*            endif.
*
*
*            clear: wg_tcurr, wg_zfit0185.
*
*            read table tg_zfit0185 into wg_zfit0185
*              with key moeda_origem = 'BRL'
*                       moeda_destino = 'ARS'.
*            "BINARY SEARCH.
*            if sy-subrc is initial.
*              wg_saidar-tx_brl      = wg_zfit0185-taxa.
*              try.
*                  wg_saidar-saldo_corr2 = ( wg_saidar-curr1 / wg_saidar-tx_brl ).
*                catch cx_sy_zerodivide.
*              endtry.
*              wg_saidar-vlr_ajust2 = ( wg_saidar-saldo_corr2 - wg_saidar-curr3 ).
*            else.
*              read table tg_tcurr into wg_tcurr
*              with key fcurr = 'BRL'
*                       tcurr = 'ARS'.
*              "BINARY SEARCH.
*              if sy-subrc is initial.
*                wg_saidar-tx_brl      = wg_tcurr-ukurs.
*                try.
*                    wg_saidar-saldo_corr2 = ( wg_saidar-curr1 / wg_saidar-tx_brl ).
*                  catch cx_sy_zerodivide.
*                endtry.
*                wg_saidar-vlr_ajust2 = ( wg_saidar-saldo_corr2 - wg_saidar-curr3 ).
*              endif.
*            endif.
*
*          elseif wg_faglflext-rbukrs eq '0101'.
*
*            clear: wg_tcurr, wg_zfit0185.
*            read table tg_tcurr into wg_tcurr
*              with key fcurr = 'USD'
*                       tcurr = 'PYG'.
*            "BINARY SEARCH.
*            if sy-subrc is initial.
*              wg_saidar-tx_usd = wg_tcurr-ukurs.
*              try.
*                  if wg_t001-bukrs eq '0101'.
*                    wg_saidar-saldo_corr  = ( ( wg_saidar-curr1 * 100 ) / wg_saidar-tx_usd ).
*                  else.
*                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
*                  endif.
*                catch cx_sy_zerodivide.
*              endtry.
*              wg_saidar-vlr_ajust = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
*            endif.
*
*            clear: wg_tcurr, wg_zfit0185.
*
*            read table tg_zfit0185 into wg_zfit0185
*              with key moeda_origem = 'BRL'
*                       moeda_destino = 'PYG'.
*            "BINARY SEARCH.
*            if sy-subrc is initial.
*
*              wg_saidar-tx_brl = wg_zfit0185-taxa.
*              try.
*                  wg_saidar-saldo_corr2 = ( ( wg_saidar-curr1 * 100 )  / wg_saidar-tx_brl ).
*                catch cx_sy_zerodivide.
*              endtry.
*              wg_saidar-vlr_ajust2 = ( wg_saidar-saldo_corr2 - wg_saidar-curr3 ).
*
*            else.
*              read table tg_tcurr into wg_tcurr
*              with key fcurr = 'BRL'
*                       tcurr = 'PYG'.
*              "BINARY SEARCH.
*              if sy-subrc is initial.
*                wg_saidar-tx_brl = wg_tcurr-ukurs.
*                try.
*                    wg_saidar-saldo_corr2 = ( ( wg_saidar-curr1 * 100 )  / wg_saidar-tx_brl ).
*                  catch cx_sy_zerodivide.
*                endtry.
*                wg_saidar-vlr_ajust2 = ( wg_saidar-saldo_corr2 - wg_saidar-curr3 ).
*              endif.
*            endif.
*          endif.
*      endcase.
*
*      loop at tg_0081 into wg_0081.
*        if wg_0081-conta_de is not initial.
*          if wg_faglflext-racct eq wg_0081-conta_de.
*            wl_flag = c_x.
*            exit.
*          endif.
*        endif.
*        clear:wg_0081.
*      endloop.
*
*      if wg_0081-cta_monet ne 'S'.
*        continue.
*      endif.
*
*      "IR56206 ALRS 13.03.2015
*      "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
*      read table it_saknr into wa_saknr with key saknr =  wg_faglflext-racct.
*      if sy-subrc = 0.
*
*        read table tg_0082 into wg_0082
*         with key saknr = wg_faglflext-racct
*                  vbund = wg_faglflext-rassc binary search.
*      else.
*        read table tg_0082 into wg_0082
*          with key saknr = wg_faglflext-racct binary search.
*      endif.
*      " Se for estorno recupera valor de ajuste
*      if sy-subrc = 0.
*        if wg_0082-obj_key_est is initial and wg_0082-obj_key is not initial.
*          wg_saidar-vlr_ajust  = wg_0082-vlr_corr_mi2.
*          wg_saidar-vlr_ajust2 = wg_0082-vlr_corr_mi3.
*        endif.
*      endif.
*
*      "IR56206 ALRS 13.03.2015
*      "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
*      read table it_saknr into wa_saknr with key saknr =  wg_faglflext-racct.
*      if sy-subrc = 0.
*
*        wg_saidar-rassc = wg_faglflext-rassc.
*      endif.
*
*
*      if wl_flag is not initial.
*        if wl_update is initial.
*          append wg_saidar to tg_saidar.
*        else.
*          modify tg_saidar from wg_saidar index wl_update transporting curr1 curr2 curr3 tx_usd tx_brl saldo_corr saldo_corr2 vlr_ajust vlr_ajust2.
*        endif.
*      endif.
*      clear: wg_saidar, wg_t001, wg_ska1, wg_skat, wg_0082, wl_flag.
*    endloop.
*
*    "10.09.2015 ALRS
*    if s_bukrs-low eq '0101'.
*      loop at  tg_saidar into wg_saidar.
*        multiply wg_saidar-curr1 by 100.
*        modify tg_saidar from wg_saidar index sy-tabix transporting curr1.
*      endloop.
*    endif.
*
*    "**********************************************************************************************
*    " Ajusta Saida de Documento Processado """"""""""""""""""""""""""""""""""""""""""""""""""""""""
*    loop at tg_0082 into wg_0082.
*      "IR56206 ALRS 13.03.2015
*      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        exporting
*          input  = wg_0082-saknr
*        importing
*          output = vg_racct.
*      "IF '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
*      read table it_saknr into wa_saknr with key saknr = wg_0082-saknr.
*      if sy-subrc = 0.
*
*        read table tg_saidar into wl_saidar with key racct = wg_0082-saknr
*                                                   rassc = wg_0082-vbund.
*      else.
*        read table tg_saidar into wl_saidar with key racct = wg_0082-saknr.
*      endif.
*      if sy-subrc is not initial.
*        read table tg_skat into wg_skat with key saknr = wg_0082-saknr binary search.
*        read table tg_ska1 into wg_ska1 with key saknr = wg_0082-saknr binary search.
*        wl_saidar-racct        = wg_0082-saknr.
*        "IR56206 ALRS 13.03.2015
*        wl_saidar-rassc        = wg_0082-vbund.
*        wl_saidar-txt50        = wg_skat-txt50.
*        wl_saidar-ktoks        = wg_ska1-ktoks.
*
*        wl_saidar-curr1        = wg_0082-dmbtr.
*
*
*        wl_saidar-curr2        = wg_0082-dmbe2.
*        wl_saidar-curr3        = wg_0082-dmbe3.
*        wl_saidar-tx_usd       = wg_0082-tx_usd.
*        wl_saidar-tx_brl       = wg_0082-tx_brl.
*        wl_saidar-saldo_corr   = wg_0082-sdo_corr_mi2.
*        wl_saidar-saldo_corr2  = wg_0082-sdo_corr_mi3.
*        wl_saidar-vlr_ajust    = wg_0082-vlr_corr_mi2.
*        wl_saidar-vlr_ajust2   = wg_0082-vlr_corr_mi3.
*        wl_saidar-obj_key      = wg_0082-obj_key.
*        wl_saidar-obj_key_est  = wg_0082-obj_key_est.
*        read table tg_zib_chv into wg_zib_chv with key obj_key = wg_0082-obj_key binary search.
*        if sy-subrc is initial.
*          wl_saidar-belnr = wg_zib_chv-belnr.
*        endif.
*        append wl_saidar to tg_saidar.
*      endif.
*    endloop.
*    "**********************************************************************************************
*
*    sort tg_saidar by racct.
*
*
*    perform atualiza_saidar tables tg_saidar
*                                   tg_zib
*                                   tg_zib_chv
*                                   tg_zib_err.
*
*    if tg_saidar[] is initial.
*      message s836(sd) display like 'E' with text-015
*                                             text-016.
*      stop.
*    endif.
*
*** Visualização
*  else.
*    sort: tg_skat    by saknr,
*          tg_ska1    by saknr,
*          tg_t001    by bukrs,
*          tg_zib_chv by obj_key,
*          tg_zib_err by obj_key.
*
*    loop at tg_0082 into wg_0082.
*      clear: wl_flag.
*
*      read table tg_skat into wg_skat with key saknr = wg_0082-saknr binary search.
*      read table tg_ska1 into wg_ska1 with key saknr = wg_0082-saknr binary search.
*
*      wg_saidar-racct        = wg_0082-saknr.
*      wg_saidar-txt50        = wg_skat-txt50.
*      wg_saidar-ktoks        = wg_ska1-ktoks.
*      wg_saidar-curr1        = wg_0082-dmbtr.
*      wg_saidar-curr2        = wg_0082-dmbe2.
*      wg_saidar-curr3        = wg_0082-dmbe3.
*      wg_saidar-tx_usd       = wg_0082-tx_usd.
*      wg_saidar-saldo_corr   = wg_0082-sdo_corr_mi2.
*      wg_saidar-vlr_ajust    = wg_0082-vlr_corr_mi2.
*      wg_saidar-tx_brl       = wg_0082-tx_brl.
*      wg_saidar-saldo_corr2  = wg_0082-sdo_corr_mi3.
*      wg_saidar-vlr_ajust2   = wg_0082-vlr_corr_mi3.
*      wg_saidar-obj_key      = wg_0082-obj_key.
*      wg_saidar-obj_key_est  = wg_0082-obj_key_est.
*
*      loop at tg_0081 into wg_0081.
*
*        if wg_0081-conta_de is not initial.
*          if wg_saidar-racct eq wg_0081-conta_de.
*            wl_flag = c_x.
*            exit.
*          endif.
*        endif.
*        clear:wg_0081.
*      endloop.
*      if wl_flag is not initial.
*        append wg_saidar to tg_saidar.
*      endif.
*      clear: wg_saidar, wg_ska1, wg_skat, wl_flag.
*    endloop.
*
*    perform atualiza_saidar tables tg_saidar
*                                  tg_zib
*                                  tg_zib_chv
*                                  tg_zib_err.
*
*
*  endif.
*
*
*
*** Processamento
*  if p_proc is not initial.
*    if s_bukrs-low eq '0101'.
**      LOOP AT  TG_SAIDA INTO WG_SAIDA.
**        MULTIPLY WG_SAIDA-CURR1 BY 100.
**        MODIFY TG_SAIDA FROM WG_SAIDA INDEX SY-TABIX TRANSPORTING CURR1.
**      ENDLOOP.
*    elseif s_bukrs-low eq '0004' or s_bukrs-low eq '0037'. "recalcula ajuste
*      loop at  tg_saidar into wg_saidar.
*        tabix = sy-tabix.
*        clear wg_0082.
*        read table tg_0082 into wg_0082
*              with key saknr = wg_saidar-racct  binary search.
*        " Se for estorno recupera valor de ajuste
*        if sy-subrc ne 0
*          or  ( wg_0082-obj_key_est is not initial or  wg_0082-obj_key is not initial ).
*          wg_saidar-saldo_corr = wg_saidar-curr2 * wg_saidar-tx_usd.
*          wg_saidar-vlr_ajust  = wg_saidar-saldo_corr - wg_saidar-curr1.
*          modify tg_saidar from wg_saidar index tabix transporting vlr_ajust saldo_corr.
*        endif.
*      endloop.
*    endif.
*  endif.
*
*endform.                    " ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZACAO_DADOS .
  DATA: WL_SAIDA  LIKE WG_SAIDA,
        WL_UPDATE TYPE SY-TABIX.


  SORT: TG_SKAT     BY SAKNR,
        TG_SKA1     BY SAKNR,
        TG_TCURR    BY FCURR TCURR,
        TG_ZFIT0185 BY MOEDA_ORIGEM MOEDA_DESTINO,
        TG_T001     BY BUKRS,
        TG_T882G    BY RBUKRS,
        TG_0084     BY BELNR BUZEI,
        TG_LFA1     BY LIFNR,
        TG_KNA1     BY KUNNR,
        TG_ZIB      BY OBJ_KEY,
        TG_ZIB_CHV  BY OBJ_KEY,
        TG_ZIB_ERR  BY OBJ_KEY.

  IF P_VISU EQ 'X'.
    SORT: TG_0084 BY SAKNR,
          TG_BSIK_BSID BY BUKRS BELNR BUZEI.
    REFRESH TG_SAIDA.
    LOOP AT TG_0084 INTO WG_0084.
      CLEAR:  WL_SAIDA.
      WG_SAIDA-OBJ_KEY         = WG_0084-OBJ_KEY.

      READ TABLE TG_BSIK_BSID INTO WG_BSIK_BSID_AUX WITH KEY BUKRS = WG_0084-BUKRS
                                                             BELNR = WG_0084-BELNR
                                                             BUZEI = WG_0084-BUZEI BINARY SEARCH.
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE TG_T001 INTO WG_T001
             WITH KEY BUKRS = WG_0084-BUKRS
                      BINARY SEARCH.

      READ TABLE TG_SKAT INTO WG_SKAT
        WITH KEY SAKNR = WG_0084-SAKNR
                 BINARY SEARCH.
      IF SY-SUBRC = 0.
        WG_SAIDA-TXT50       = WG_SKAT-TXT50.
      ENDIF.
      WG_SAIDA-HKONT       = WG_0084-SAKNR.
      WG_SAIDA-KUNNR       = WG_0084-LIFNR.

      READ TABLE TG_SKA1 INTO WG_SKA1
        WITH KEY SAKNR = WG_0084-SAKNR
                 BINARY SEARCH.

      CLEAR: WG_ZIB_CHV.
      IF WG_0084-OBJ_KEY IS NOT INITIAL.
        READ TABLE TG_ZIB_CHV  INTO WG_ZIB_CHV
          WITH KEY OBJ_KEY = WG_0084-OBJ_KEY
                     BINARY SEARCH.
        IF SY-SUBRC = 0.
          WG_SAIDA-BELNR       = WG_ZIB_CHV-BELNR.
        ENDIF.
      ENDIF.

      IF P_FORN IS NOT INITIAL.
        READ TABLE TG_LFA1 INTO WG_LFA1
          WITH KEY LIFNR = WG_0084-LIFNR
                   BINARY SEARCH.

        WG_SAIDA-NAME1       = WG_LFA1-NAME1.
      ELSE.
        READ TABLE TG_KNA1 INTO WG_KNA1
         WITH KEY KUNNR = WG_0084-LIFNR
                  BINARY SEARCH.

        WG_SAIDA-NAME1       = WG_KNA1-NAME1.
      ENDIF.

      WG_SAIDA-BSCHL       = WG_BSIK_BSID_AUX-BSCHL.
      WG_SAIDA-BELNR2      = WG_BSIK_BSID_AUX-BELNR.
      WG_SAIDA-BUZEI       = WG_BSIK_BSID_AUX-BUZEI.
      WG_SAIDA-GJAHR       = WG_BSIK_BSID_AUX-GJAHR.
      WG_SAIDA-WAERS       = WG_BSIK_BSID_AUX-WAERS.
      WG_SAIDA-BUDAT       = WG_BSIK_BSID_AUX-BUDAT.
      WG_SAIDA-BLART       = WG_BSIK_BSID_AUX-BLART.
      WG_SAIDA-XBLNR       = WG_BSIK_BSID_AUX-XBLNR.
      WG_SAIDA-DOC         = WG_BSIK_BSID_AUX-DOC.
      WG_SAIDA-ITEM        = WG_BSIK_BSID_AUX-ITEM.

      WG_SAIDA-UMSKZ       = WG_BSIK_BSID_AUX-UMSKZ.
      "calculos
      WG_SAIDA-DMBTR       = WG_0084-DMBTR.
      WG_SAIDA-DMBE2       = WG_0084-DMBE2.
      WG_SAIDA-DMBE3       = WG_0084-DMBE3.
      WG_SAIDA-VLR_AJUST   = WG_0084-VLR_CORR_MI2.
      WG_SAIDA-VLR_AJUST2  = WG_0084-VLR_CORR_MI3.
      WG_SAIDA-SALDO_CORR  = WG_0084-SDO_CORR_MI2.


      LOOP AT TG_0081 INTO WG_0081.
        IF WG_0081-CONTA_DE IS NOT INITIAL.
          IF WG_SAIDA-HKONT EQ WG_0081-CONTA_DE.
            EXIT.
          ENDIF.
        ENDIF.
        CLEAR:WG_0081.
      ENDLOOP.

      IF WG_0081 IS NOT INITIAL .
        IF WG_T001-LAND1 EQ 'AR' OR WG_T001-LAND1 EQ 'PY'.
          V_SAKNR      =  WG_0081-SAKNR.
        ELSEIF WG_0081-SAKNR IS NOT INITIAL.
          V_SAKNR      =  WG_0081-SAKNR.
        ELSEIF WG_0081-HKONT IS NOT INITIAL.
          V_SAKNR      =  WG_0081-HKONT.
        ENDIF.

        CLEAR V_VBUND.
        READ TABLE T_HKONT WITH KEY FROM = V_SAKNR.
        IF SY-SUBRC = 0.
          IF P_FORN = 'X'.
            SELECT SINGLE VBUND INTO V_VBUND FROM LFA1 WHERE LIFNR = WG_SAIDA-KUNNR.
          ELSE.
            SELECT SINGLE VBUND INTO V_VBUND FROM KNA1 WHERE KUNNR = WG_SAIDA-KUNNR.
          ENDIF.
        ENDIF.
        WG_SAIDA-VBUND = V_VBUND.

        CLEAR V_BEWAR.
        SELECT SINGLE BEWAR
          INTO V_BEWAR
          FROM ZFIT0030
         WHERE HKONT EQ V_SAKNR
           AND COND  EQ ''.
        WG_SAIDA-BEWAR      = V_BEWAR.
      ENDIF.

      APPEND WG_SAIDA TO TG_SAIDA.
      CLEAR: WG_SAIDA, WG_T001, WG_SKA1, WG_SKAT, WG_0084.
    ENDLOOP.


    PERFORM ATUALIZA_SAIDA TABLES TG_SAIDA
                                  TG_ZIB
                                  TG_ZIB_CHV
                                  TG_ZIB_ERR.

    EXIT. "visualização

  ENDIF.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'CONTAS_EC-CS'
    TABLES
      SET_VALUES    = T_HKONT
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.

  REFRESH: TG_BSIK_BSID_AUX.
  LOOP AT TG_BSIK_BSID INTO WG_BSIK_BSID_AUX.
    IF WG_BSIK_BSID_AUX-SHKZG EQ 'H'.
      MULTIPLY WG_BSIK_BSID_AUX-DMBTR BY -1.
      MULTIPLY WG_BSIK_BSID_AUX-DMBE2 BY -1.
      MULTIPLY WG_BSIK_BSID_AUX-DMBE3 BY -1.
    ENDIF.

    APPEND  WG_BSIK_BSID_AUX TO TG_BSIK_BSID_AUX.
    CLEAR: WG_BSIK_BSID_AUX.
  ENDLOOP.

*---> 05/07/2023 - Migração S4 - DL
  SORT TG_TCURR_LDAY BY KURST FCURR.
  SORT TG_T001 BY BUKRS.
  SORT TG_SKAT BY SAKNR.
  SORT TG_SKA1 BY SAKNR.
  SORT TG_0084 BY BELNR BUZEI.
  SORT TG_ZIB_CHV BY OBJ_KEY.
  SORT TG_LFA1 BY LIFNR.
  SORT TG_KNA1 BY KUNNR.
  SORT TG_ZFIT0185 BY MOEDA_ORIGEM MOEDA_DESTINO.
  SORT TG_TCURR BY FCURR TCURR.
  SORT TG_ZFIT0185_LDAY BY MOEDA_ORIGEM MOEDA_DESTINO.
  SORT TG_0084 BY BELNR BUZEI.
*<--- 05/07/2023 - Migração S4 - DL


  LOOP AT TG_BSIK_BSID_AUX INTO WG_BSIK_BSID_AUX.
    CLEAR:  WL_SAIDA.

    READ TABLE TG_T001 INTO WG_T001
           WITH KEY BUKRS = WG_BSIK_BSID_AUX-BUKRS
                    BINARY SEARCH.

    READ TABLE TG_SKAT INTO WG_SKAT
      WITH KEY SAKNR = WG_BSIK_BSID_AUX-HKONT
               BINARY SEARCH.
    IF SY-SUBRC = 0.
      WG_SAIDA-TXT50       = WG_SKAT-TXT50.
    ENDIF.
    WG_SAIDA-HKONT       = WG_BSIK_BSID_AUX-HKONT.
    WG_SAIDA-KUNNR       = WG_BSIK_BSID_AUX-LIFNR.

    READ TABLE TG_SKA1 INTO WG_SKA1
      WITH KEY SAKNR = WG_BSIK_BSID_AUX-HKONT
               BINARY SEARCH.

    CLEAR WG_0084.
    " Testar com BUZEI ANTES
    READ TABLE TG_0084 INTO WG_0084
    WITH KEY BELNR = WG_BSIK_BSID_AUX-BELNR
             BUZEI = WG_BSIK_BSID_AUX-BUZEI BINARY SEARCH.
    IF SY-SUBRC NE 0.
      READ TABLE TG_0084 INTO WG_0084
      WITH KEY BELNR = WG_BSIK_BSID_AUX-BELNR BINARY SEARCH.
      IF WG_0084-BUZEI IS NOT INITIAL.
        SY-SUBRC = 4.
      ENDIF.
    ENDIF.

    IF SY-SUBRC IS INITIAL.
      WG_SAIDA-OBJ_KEY         = WG_0084-OBJ_KEY.
      "Documento da Variação
      CLEAR: WG_ZIB_CHV.
      IF WG_0084-OBJ_KEY IS NOT INITIAL.
        READ TABLE TG_ZIB_CHV  INTO WG_ZIB_CHV
          WITH KEY OBJ_KEY = WG_0084-OBJ_KEY
                     BINARY SEARCH.
        IF SY-SUBRC = 0.
          WG_SAIDA-BELNR       = WG_ZIB_CHV-BELNR.
        ENDIF.
      ENDIF.

    ENDIF.

    IF P_FORN IS NOT INITIAL.
      READ TABLE TG_LFA1 INTO WG_LFA1
        WITH KEY LIFNR = WG_BSIK_BSID_AUX-LIFNR
                 BINARY SEARCH.

      WG_SAIDA-NAME1       = WG_LFA1-NAME1.
    ELSE.
      READ TABLE TG_KNA1 INTO WG_KNA1
       WITH KEY KUNNR = WG_BSIK_BSID_AUX-LIFNR
                BINARY SEARCH.

      WG_SAIDA-NAME1       = WG_KNA1-NAME1.
    ENDIF.

    WG_SAIDA-BSCHL       = WG_BSIK_BSID_AUX-BSCHL.
    WG_SAIDA-BELNR2      = WG_BSIK_BSID_AUX-BELNR.
    WG_SAIDA-BUZEI       = WG_BSIK_BSID_AUX-BUZEI.
    WG_SAIDA-GJAHR       = WG_BSIK_BSID_AUX-GJAHR.
    WG_SAIDA-WAERS       = WG_BSIK_BSID_AUX-WAERS.
    WG_SAIDA-BUDAT       = WG_BSIK_BSID_AUX-BUDAT.
    WG_SAIDA-BLART       = WG_BSIK_BSID_AUX-BLART.
    WG_SAIDA-XBLNR       = WG_BSIK_BSID_AUX-XBLNR.
    WG_SAIDA-DOC         = WG_BSIK_BSID_AUX-DOC.
    WG_SAIDA-ITEM        = WG_BSIK_BSID_AUX-ITEM.
    WG_SAIDA-DMBTR       = WG_BSIK_BSID_AUX-DMBTR.
    WG_SAIDA-DMBE2       = WG_BSIK_BSID_AUX-DMBE2.
    WG_SAIDA-UMSKZ       = WG_BSIK_BSID_AUX-UMSKZ.

    CASE WG_T001-LAND1.
      WHEN 'BR'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
        CLEAR: WG_TCURR, WG_ZFIT0185.

        READ TABLE TG_ZFIT0185 INTO WG_ZFIT0185
         WITH KEY MOEDA_ORIGEM = 'USD'
                  MOEDA_DESTINO = 'BRL'.
        "BINARY SEARCH.

        IF SY-SUBRC IS INITIAL.
          WG_SAIDA-TX_USD = WG_ZFIT0185-TAXA.
          TRY.
              WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
            CATCH CX_SY_ZERODIVIDE .
          ENDTRY.
          WG_SAIDA-VLR_AJUST   = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).
        ELSE.
          READ TABLE TG_TCURR INTO WG_TCURR
          WITH KEY FCURR = 'USD'
                   TCURR = 'BRL'.
          "BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WG_SAIDA-TX_USD = WG_TCURR-UKURS.
            TRY.
                WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
              CATCH CX_SY_ZERODIVIDE .
            ENDTRY.
            WG_SAIDA-VLR_AJUST   = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).
          ENDIF.
        ENDIF.



      WHEN 'NL'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
        CLEAR: WG_TCURR_LDAY, WG_ZFIT0185_LDAY.

        READ TABLE TG_ZFIT0185_LDAY INTO WG_ZFIT0185_LDAY
         WITH KEY  MOEDA_ORIGEM = 'USD'
                   MOEDA_DESTINO = 'EUR'.
        "BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.


          WG_SAIDA-TX_USD = WG_ZFIT0185_LDAY-TAXA.
          IF WG_SAIDA-TX_USD LT 0.
            MULTIPLY WG_SAIDA-TX_USD BY -1.
            TRY.
                WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR * WG_SAIDA-TX_USD ).
              CATCH CX_SY_ZERODIVIDE .
            ENDTRY.
          ELSE.
            TRY.
                WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
              CATCH CX_SY_ZERODIVIDE .
            ENDTRY.
          ENDIF.
          WG_SAIDA-VLR_AJUST   = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).


        ELSE.

          READ TABLE TG_TCURR_LDAY INTO WG_TCURR_LDAY
          WITH KEY FCURR = 'USD'
                   TCURR = 'EUR'.
          "BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WG_SAIDA-TX_USD = WG_TCURR_LDAY-UKURS.
            IF WG_SAIDA-TX_USD LT 0.
              MULTIPLY WG_SAIDA-TX_USD BY -1.
              TRY.
                  WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR * WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE .
              ENDTRY.
            ELSE.
              TRY.
                  WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE .
              ENDTRY.
            ENDIF.
            WG_SAIDA-VLR_AJUST   = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).
          ENDIF.
        ENDIF.

        "============================= INICIO DEVK9A0R37 - FI - [IR053183] Ajuste ZFI0063 - AOENNING
      WHEN 'LU'.
*        CLEAR: wg_tcurr_lday.
*        READ TABLE tg_tcurr_lday INTO wg_tcurr_lday
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
        CLEAR: WG_TCURR, WG_ZFIT0185.
        READ TABLE TG_ZFIT0185 INTO WG_ZFIT0185
        WITH KEY MOEDA_ORIGEM = 'USD'
                 MOEDA_DESTINO = 'EUR'.
        "BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WG_SAIDA-TX_USD = WG_ZFIT0185-TAXA.
          IF WG_SAIDA-TX_USD LT 0.
            MULTIPLY WG_SAIDA-TX_USD BY -1.
            TRY.
                WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR * WG_SAIDA-TX_USD ).
              CATCH CX_SY_ZERODIVIDE .
            ENDTRY.
          ELSE.
            TRY.
                WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
              CATCH CX_SY_ZERODIVIDE .
            ENDTRY.
          ENDIF.
          WG_SAIDA-VLR_AJUST   = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).
        ELSE.
          READ TABLE TG_TCURR INTO WG_TCURR
          WITH KEY FCURR = 'USD'
                   TCURR = 'EUR'.
          "BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WG_SAIDA-TX_USD = WG_TCURR-UKURS.
            IF WG_SAIDA-TX_USD LT 0.
              MULTIPLY WG_SAIDA-TX_USD BY -1.
              TRY.
                  WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR * WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE .
              ENDTRY.
            ELSE.
              TRY.
                  WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE .
              ENDTRY.
            ENDIF.
            WG_SAIDA-VLR_AJUST   = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).
          ENDIF.
        ENDIF.



        "=============================FIM DEVK9A0R37 - FI - [IR053183] Ajuste ZFI0063 - AOENNING
      WHEN 'CH'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
        CLEAR: WG_TCURR_LDAY, WG_ZFIT0185_LDAY .

        READ TABLE TG_ZFIT0185_LDAY INTO WG_ZFIT0185_LDAY
        WITH KEY MOEDA_ORIGEM = 'USD'
                 MOEDA_DESTINO = 'CHF'.
        "BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WG_SAIDA-TX_USD = WG_ZFIT0185-TAXA.
          IF WG_SAIDA-TX_USD LT 0.
            MULTIPLY WG_SAIDA-TX_USD BY -1.
            TRY.
                WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR * WG_SAIDA-TX_USD ).
              CATCH CX_SY_ZERODIVIDE .
            ENDTRY.
          ELSE.
            TRY.
                WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
              CATCH CX_SY_ZERODIVIDE .
            ENDTRY.
          ENDIF.
          WG_SAIDA-VLR_AJUST   = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).
        ELSE.
          READ TABLE TG_TCURR_LDAY INTO WG_TCURR_LDAY
          WITH KEY FCURR = 'USD'
                   TCURR = 'CHF'.
          "BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WG_SAIDA-TX_USD = WG_TCURR_LDAY-UKURS.
            IF WG_SAIDA-TX_USD LT 0.
              MULTIPLY WG_SAIDA-TX_USD BY -1.
              TRY.
                  WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR * WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE .
              ENDTRY.
            ELSE.
              TRY.
                  WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE .
              ENDTRY.
            ENDIF.
            WG_SAIDA-VLR_AJUST   = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).
          ENDIF.
        ENDIF.


      WHEN 'AR' OR 'PY'.
        WG_SAIDA-DMBE3 = WG_BSIK_BSID_AUX-DMBE3.
        IF WG_BSIK_BSID_AUX-BUKRS EQ '0100'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
          CLEAR: WG_TCURR, WG_ZFIT0185.

          READ TABLE TG_ZFIT0185 INTO WG_ZFIT0185
            WITH KEY MOEDA_ORIGEM = 'USD'
                     MOEDA_DESTINO = 'ARS'.
          "BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WG_SAIDA-TX_USD = WG_ZFIT0185-TAXA.
            TRY.
                WG_SAIDA-SALDO_CORR = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
              CATCH CX_SY_ZERODIVIDE.
            ENDTRY.
            WG_SAIDA-VLR_AJUST = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).
          ELSE.
            READ TABLE TG_TCURR INTO WG_TCURR
            WITH KEY FCURR = 'USD'
                     TCURR = 'ARS'.
            "BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              WG_SAIDA-TX_USD = WG_TCURR-UKURS.
              TRY.
                  WG_SAIDA-SALDO_CORR = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE.
              ENDTRY.
              WG_SAIDA-VLR_AJUST = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).
            ENDIF.
          ENDIF.

          CLEAR: WG_TCURR, WG_ZFIT0185.


          READ TABLE TG_ZFIT0185 INTO WG_ZFIT0185
            WITH KEY MOEDA_ORIGEM = 'BRL'
                     MOEDA_DESTINO = 'ARS'
                     BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WG_SAIDA-TX_BRL      = WG_ZFIT0185-TAXA.
            TRY.
                WG_SAIDA-SALDO_CORR2 = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_BRL ).
              CATCH CX_SY_ZERODIVIDE.
            ENDTRY.
            WG_SAIDA-VLR_AJUST2 = ( WG_SAIDA-SALDO_CORR2 - WG_SAIDA-DMBE3 ).
          ELSE.
            READ TABLE TG_TCURR INTO WG_TCURR
            WITH KEY FCURR = 'BRL'
                     TCURR = 'ARS'
                     BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              WG_SAIDA-TX_BRL      = WG_TCURR-UKURS.
              TRY.
                  WG_SAIDA-SALDO_CORR2 = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_BRL ).
                CATCH CX_SY_ZERODIVIDE.
              ENDTRY.
              WG_SAIDA-VLR_AJUST2 = ( WG_SAIDA-SALDO_CORR2 - WG_SAIDA-DMBE3 ).
            ENDIF.
          ENDIF.

        ELSEIF WG_BSIK_BSID_AUX-BUKRS EQ '0101'.
          CLEAR: WG_TCURR, WG_ZFIT0185.
          MULTIPLY WG_SAIDA-DMBTR BY 100. "Ajusta decimal Paraguay

          READ TABLE TG_ZFIT0185 INTO WG_ZFIT0185
          WITH KEY MOEDA_ORIGEM = 'USD'
                   MOEDA_DESTINO = 'PYG'.
          "BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WG_SAIDA-TX_USD = WG_ZFIT0185-TAXA.
            TRY.
                WG_SAIDA-SALDO_CORR = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
              CATCH CX_SY_ZERODIVIDE.
            ENDTRY.
            WG_SAIDA-VLR_AJUST = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).
          ELSE.
            READ TABLE TG_TCURR INTO WG_TCURR
            WITH KEY FCURR = 'USD'
                     TCURR = 'PYG'.
            "BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              WG_SAIDA-TX_USD = WG_TCURR-UKURS.
              TRY.
                  WG_SAIDA-SALDO_CORR = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE.
              ENDTRY.
              WG_SAIDA-VLR_AJUST = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBE2 ).
            ENDIF.
          ENDIF.



          CLEAR: WG_TCURR, WG_ZFIT0185.

          READ TABLE TG_ZFIT0185 INTO WG_ZFIT0185
            WITH KEY MOEDA_ORIGEM = 'BRL'
                     MOEDA_DESTINO = 'PYG'
                     BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WG_SAIDA-TX_BRL = WG_ZFIT0185-TAXA.
            TRY.
                WG_SAIDA-SALDO_CORR2 = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_BRL ).
              CATCH CX_SY_ZERODIVIDE.
            ENDTRY.
            WG_SAIDA-VLR_AJUST2 = ( WG_SAIDA-SALDO_CORR2 - WG_SAIDA-DMBE3 ).
          ELSE.
            READ TABLE TG_TCURR INTO WG_TCURR
            WITH KEY FCURR = 'BRL'
                     TCURR = 'PYG'
                     BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              WG_SAIDA-TX_BRL = WG_TCURR-UKURS.
              TRY.
                  WG_SAIDA-SALDO_CORR2 = ( WG_SAIDA-DMBTR / WG_SAIDA-TX_BRL ).
                CATCH CX_SY_ZERODIVIDE.
              ENDTRY.
              WG_SAIDA-VLR_AJUST2 = ( WG_SAIDA-SALDO_CORR2 - WG_SAIDA-DMBE3 ).
            ENDIF.
          ENDIF.

        ENDIF.
    ENDCASE.

    READ TABLE TG_0084 INTO WG_0084
    WITH KEY BELNR = WG_BSIK_BSID_AUX-BELNR
             BUZEI = WG_BSIK_BSID_AUX-BUZEI BINARY SEARCH.

    IF SY-SUBRC NE 0.
      READ TABLE TG_0084 INTO WG_0084
      WITH KEY BELNR = WG_BSIK_BSID_AUX-BELNR BINARY SEARCH.
      IF WG_0084-BUZEI IS NOT INITIAL.
        SY-SUBRC = 4.
      ENDIF.
    ENDIF.

    " Se for estorno recupera valor de ajuste
    IF SY-SUBRC = 0.
      IF WG_0084-OBJ_KEY_EST IS INITIAL AND WG_0084-OBJ_KEY IS NOT INITIAL.
        WG_SAIDA-VLR_AJUST  = WG_0084-VLR_CORR_MI2.
        WG_SAIDA-VLR_AJUST2 = WG_0084-VLR_CORR_MI3.
      ENDIF.
    ENDIF.

    LOOP AT TG_0081 INTO WG_0081.
      IF WG_0081-CONTA_DE IS NOT INITIAL.
        IF WG_SAIDA-HKONT EQ WG_0081-CONTA_DE.
          EXIT.
        ENDIF.
      ENDIF.
      CLEAR:WG_0081.
    ENDLOOP.

    IF WG_0081 IS NOT INITIAL .
      IF WG_T001-LAND1 EQ 'AR' OR WG_T001-LAND1 EQ 'PY'.
        V_SAKNR      =  WG_0081-SAKNR.
      ELSEIF WG_0081-SAKNR IS NOT INITIAL.
        V_SAKNR      =  WG_0081-SAKNR.
      ELSEIF WG_0081-HKONT IS NOT INITIAL.
        V_SAKNR      =  WG_0081-HKONT.
      ENDIF.

      CLEAR V_VBUND.
      READ TABLE T_HKONT WITH KEY FROM = V_SAKNR.
      IF SY-SUBRC = 0.
        IF P_FORN = 'X'.
          SELECT SINGLE VBUND INTO V_VBUND FROM LFA1 WHERE LIFNR = WG_SAIDA-KUNNR.
        ELSE.
          SELECT SINGLE VBUND INTO V_VBUND FROM KNA1 WHERE KUNNR = WG_SAIDA-KUNNR.
        ENDIF.
      ENDIF.
      WG_SAIDA-VBUND = V_VBUND.

      CLEAR V_BEWAR.
      SELECT SINGLE BEWAR
        INTO V_BEWAR
        FROM ZFIT0030
       WHERE HKONT EQ V_SAKNR
         AND COND  EQ ''.
      WG_SAIDA-BEWAR      = V_BEWAR.
    ENDIF.


    APPEND WG_SAIDA TO TG_SAIDA.
    CLEAR: WG_SAIDA, WG_T001, WG_SKA1, WG_SKAT, WG_0084.
  ENDLOOP.

  IF S_BUKRS-LOW EQ '0004' OR S_BUKRS-LOW EQ '0037'. " recalcula ajuste
    LOOP AT  TG_SAIDA INTO WG_SAIDA.
      WG_SAIDA-SALDO_CORR = WG_SAIDA-DMBE2 * WG_SAIDA-TX_USD.
      WG_SAIDA-VLR_AJUST  = WG_SAIDA-SALDO_CORR - WG_SAIDA-DMBTR.
      MODIFY TG_SAIDA FROM WG_SAIDA INDEX SY-TABIX TRANSPORTING VLR_AJUST SALDO_CORR.
    ENDLOOP.
  ENDIF.

  PERFORM ATUALIZA_SAIDA TABLES TG_SAIDA
                                TG_ZIB
                                TG_ZIB_CHV
                                TG_ZIB_ERR.

ENDFORM.                    " ORGANIZACAO_DADOS

FORM GERA_CONTABILR  USING   WL_SAIDA TYPE TY_SAIDAR
                             WL_ESTORNO
                   CHANGING  P_OBJ_KEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY
                             WL_SEQITEM TYPE ZIB_CONTABIL-SEQITEM." Rubenilson - 21.01.25 - 124919

  DATA: WL_INPUT_ZIB  TYPE ZIB_CONTABIL,
        TL_INPUT_ZIB  TYPE TABLE OF ZIB_CONTABIL,
        WL_OBJ_KEY(9),
        V_SAKNR       TYPE ZFIT0081-SAKNR,
        V_BEWAR       TYPE BSEG-BEWAR,
        VANOMES(7).

  CONCATENATE S_MES-LOW(2) '/' S_MES-LOW+2(4) INTO VANOMES.

  CALL FUNCTION 'Z_CONTROLE_FECHAMES'
    EXPORTING
      I_BUKRS  = S_BUKRS-LOW
      I_DATA   = VG_LAST_DAY
    IMPORTING
      E_STATUS = E_STATUS
      E_MESSA  = E_MESSA
    EXCEPTIONS
      ERROR    = 1
      OTHERS   = 2.
  IF E_STATUS = 'E'.
    MESSAGE E398(00) WITH E_MESSA.
    EXIT.
  ENDIF.

  REFRESH: TL_INPUT_ZIB.
  CLEAR: WL_INPUT_ZIB, WL_INPUT_0082.
  READ TABLE TG_T001 INTO WG_T001
      WITH KEY BUKRS =  S_BUKRS-LOW
               BINARY SEARCH.

  READ TABLE TG_T882G INTO WG_T882G
    WITH KEY RBUKRS =  S_BUKRS-LOW
             BINARY SEARCH.

  LOOP AT TG_0081 INTO WG_0081.
    IF WG_0081-CONTA_DE IS NOT INITIAL.
      IF WL_SAIDA-RACCT EQ WG_0081-CONTA_DE.
        EXIT.
      ENDIF.
    ENDIF.
    CLEAR:WG_0081.
  ENDLOOP.
  IF WG_0081 IS NOT INITIAL .
    IF WG_T001-LAND1 EQ 'AR' OR WG_T001-LAND1 EQ 'PY'.
      V_SAKNR      =  WG_0081-SAKNR.
    ELSEIF WG_0081-SAKNR IS NOT INITIAL.
      V_SAKNR      =  WG_0081-SAKNR.
    ELSEIF WG_0081-HKONT IS NOT INITIAL.
      V_SAKNR      =  WG_0081-HKONT.
    ENDIF.

    IF P_OBJ_KEY IS INITIAL.
      PERFORM GET_NEXT_NUMBER  USING  'Z_SALDOMO2'
                                      '01'
                             CHANGING WL_OBJ_KEY.
    ELSE.
      WL_OBJ_KEY = P_OBJ_KEY+7(9).
    ENDIF.
** 1 Partida
    WL_INPUT_ZIB-MANDT    = SY-MANDT.
    CONCATENATE 'ZFI0061' WL_OBJ_KEY VG_RYEAR INTO  WL_INPUT_ZIB-OBJ_KEY.
*    wl_input_zib-seqitem    = '0001'." Rubenilson - 21.01.25 - 124919
    WL_INPUT_ZIB-SEQITEM    = WL_SEQITEM." Rubenilson - 21.01.25 - 124919
    CONCATENATE S_MES-LOW(2) '/' S_MES-LOW+2(4) INTO WL_INPUT_ZIB-XBLNR.
    IF WL_ESTORNO IS INITIAL.
      IF WL_SAIDA-VLR_AJUST LT 0.
        WL_INPUT_ZIB-BSCHL      = '50'.
      ELSE.
        WL_INPUT_ZIB-BSCHL      = '40'.
      ENDIF.
    ELSE.
      IF WL_SAIDA-VLR_AJUST LT 0.
        WL_INPUT_ZIB-BSCHL      = '40'.
      ELSE.
        WL_INPUT_ZIB-BSCHL      = '50'.
      ENDIF.
    ENDIF.
    IF WG_T001-LAND1 EQ 'BR'.
      CONCATENATE S_BUKRS-LOW+2(2) '01' INTO WL_INPUT_ZIB-GSBER.
    ELSEIF WG_T001-LAND1 EQ 'NL'.
      IF S_BUKRS-LOW EQ '0201'.
        WL_INPUT_ZIB-GSBER = 'H201'.
      ELSE.
        WL_INPUT_ZIB-GSBER = 'H202'.
      ENDIF.
    ELSEIF WG_T001-LAND1 EQ 'LU'.
      IF S_BUKRS-LOW EQ '0203'.
        WL_INPUT_ZIB-GSBER = 'L203'.
      ENDIF.
    ELSEIF WG_T001-LAND1 EQ 'CH'.
      WL_INPUT_ZIB-GSBER = 'S201'.
    ELSEIF WG_T001-LAND1 EQ 'AR'
        OR WG_T001-LAND1 EQ 'PY'.
      IF S_BUKRS-LOW EQ '0100'.
        WL_INPUT_ZIB-GSBER = 'T001'.
      ELSE.
        WL_INPUT_ZIB-GSBER = 'P001'.
      ENDIF.
    ENDIF.
    WL_INPUT_ZIB-BUKRS      = S_BUKRS-LOW.
    WL_INPUT_ZIB-INTERFACE    = '35'.
    WL_INPUT_ZIB-BKTXT      = TEXT-050.
    WRITE VG_LAST_DAY TO  WL_INPUT_ZIB-BLDAT.
    WRITE VG_LAST_DAY TO  WL_INPUT_ZIB-BUDAT.
*    WL_INPUT_ZIB-BUDAT      = VG_LAST_DAY.
    WL_INPUT_ZIB-GJAHR      = VG_RYEAR.
    WL_INPUT_ZIB-MONAT      = S_MES-LOW(2).
    WL_INPUT_ZIB-BLART      = 'VC'.
    WL_INPUT_ZIB-HKONT      = WL_SAIDA-RACCT.
    WL_INPUT_ZIB-WRBTR      = 0.
    WL_INPUT_ZIB-VBUND      = WL_SAIDA-RASSC.

    IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-WAERS      = 'USD'.
    ELSE.
      WL_INPUT_ZIB-WAERS      = WG_T882G-CURR1.
    ENDIF.

    WL_INPUT_ZIB-BUPLA      = SPACE.

    IF WG_T001-LAND1 EQ 'AR'
      OR WG_T001-LAND1 EQ 'PY'.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      CONCATENATE TEXT-051 VANOMES
            INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ELSEIF WG_0081-SAKNR IS NOT INITIAL.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      CONCATENATE TEXT-051 VANOMES
          INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ELSEIF WG_0081-HKONT IS NOT INITIAL.
*      CONCATENATE 'Ajuste moeda apresentação ' VANOMES
      CONCATENATE TEXT-052 VANOMES
          INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ENDIF.

    CLEAR V_BEWAR.
    SELECT SINGLE BEWAR
      INTO V_BEWAR
      FROM ZFIT0030
      WHERE HKONT EQ WL_INPUT_ZIB-HKONT
      AND   COND  EQ ''.

    WL_INPUT_ZIB-BEWAR      = V_BEWAR.

    WL_INPUT_ZIB-WAERS_I    = 'X'.

    IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-DMBTR      = WL_SAIDA-VLR_AJUST.
      IF WL_INPUT_ZIB-DMBTR LT 0.
        MULTIPLY WL_INPUT_ZIB-DMBTR BY -1.
      ENDIF.
    ELSE.
      WL_INPUT_ZIB-DMBTR      = 0.
    ENDIF.

    WL_INPUT_ZIB-WAERS_F    = WG_T882G-CURR2.

    IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-DMBE2     = 0.
    ELSE.
      WL_INPUT_ZIB-DMBE2      = WL_SAIDA-VLR_AJUST.
      IF WL_INPUT_ZIB-DMBE2 LT 0.
        MULTIPLY WL_INPUT_ZIB-DMBE2 BY -1.
      ENDIF.
    ENDIF.

    WL_INPUT_ZIB-RG_ATUALIZADO  = 'N'.

    APPEND WL_INPUT_ZIB TO TL_INPUT_ZIB.
    CLEAR: WL_INPUT_ZIB.

** 2 Partida
    WL_INPUT_ZIB-MANDT    = SY-MANDT.
    CONCATENATE 'ZFI0061' WL_OBJ_KEY VG_RYEAR INTO  WL_INPUT_ZIB-OBJ_KEY.
*    wl_input_zib-seqitem    = '0002'." Rubenilson - 21.01.25 - 124919
    WL_SEQITEM = WL_SEQITEM + 1." Rubenilson - 21.01.25 - 124919
    WL_INPUT_ZIB-SEQITEM    = WL_SEQITEM.
    CONCATENATE S_MES-LOW(2) '/' S_MES-LOW+2(4) INTO WL_INPUT_ZIB-XBLNR.
    IF WL_ESTORNO IS INITIAL.
      IF WL_SAIDA-VLR_AJUST LT 0.
        WL_INPUT_ZIB-BSCHL      = '40'.
      ELSE.
        WL_INPUT_ZIB-BSCHL      = '50'.
      ENDIF.
    ELSE.
      IF WL_SAIDA-VLR_AJUST LT 0.
        WL_INPUT_ZIB-BSCHL      = '50'.
      ELSE.
        WL_INPUT_ZIB-BSCHL      = '40'.
      ENDIF.
    ENDIF.
    IF WG_T001-LAND1 EQ 'BR'.
      CONCATENATE S_BUKRS-LOW+2(2) '01' INTO WL_INPUT_ZIB-GSBER.
    ELSEIF WG_T001-LAND1 EQ 'NL'.
      IF S_BUKRS-LOW EQ '0201'.
        WL_INPUT_ZIB-GSBER = 'H201'.
      ELSE.
        WL_INPUT_ZIB-GSBER = 'H202'.
      ENDIF.
    ELSEIF WG_T001-LAND1 EQ 'LU'.
      IF S_BUKRS-LOW EQ '0203'.
        WL_INPUT_ZIB-GSBER = 'L203'.
      ENDIF.
    ELSEIF WG_T001-LAND1 EQ 'CH'.
      WL_INPUT_ZIB-GSBER = 'S201'.
    ELSEIF WG_T001-LAND1 EQ 'AR'
        OR WG_T001-LAND1 EQ 'PY'.
      IF S_BUKRS-LOW EQ '0100'.
        WL_INPUT_ZIB-GSBER = 'T001'.
      ELSE.
        WL_INPUT_ZIB-GSBER = 'P001'.
      ENDIF.
    ENDIF.
    WL_INPUT_ZIB-BUKRS      = S_BUKRS-LOW.
    WL_INPUT_ZIB-INTERFACE    = '35'.
    WL_INPUT_ZIB-BKTXT      = TEXT-050.
    WRITE VG_LAST_DAY TO  WL_INPUT_ZIB-BLDAT.
*    WL_INPUT_ZIB-BLDAT      = VG_LAST_DAY.
    WRITE VG_LAST_DAY TO  WL_INPUT_ZIB-BUDAT.
*    WL_INPUT_ZIB-BUDAT      = VG_LAST_DAY.
    WL_INPUT_ZIB-GJAHR      = VG_RYEAR.
    WL_INPUT_ZIB-MONAT      = S_MES-LOW(2).
    WL_INPUT_ZIB-BLART      = 'VC'.

    IF WG_T001-LAND1 EQ 'AR'
        OR WG_T001-LAND1 EQ 'PY'.
      WL_INPUT_ZIB-HKONT      =  WG_0081-SAKNR.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      CONCATENATE TEXT-051 VANOMES
         INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ELSEIF WG_0081-SAKNR IS NOT INITIAL.
      WL_INPUT_ZIB-HKONT      =  WG_0081-SAKNR.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      CONCATENATE TEXT-051 VANOMES
      INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ELSEIF WG_0081-HKONT IS NOT INITIAL.
      WL_INPUT_ZIB-HKONT      =  WG_0081-HKONT.
*      CONCATENATE 'Ajuste moeda apresentação ' VANOMES
      CONCATENATE TEXT-052 VANOMES
      INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ENDIF.

    CLEAR V_BEWAR.
    SELECT SINGLE BEWAR
      INTO V_BEWAR
      FROM ZFIT0030
      WHERE HKONT EQ WL_INPUT_ZIB-HKONT
      AND   COND  EQ ''.

    WL_INPUT_ZIB-BEWAR      = V_BEWAR.
    WL_INPUT_ZIB-VBUND      = WL_SAIDA-RASSC.

    WL_INPUT_ZIB-WRBTR      = 0.

    IF S_BUKRS-LOW = '0004' OR  S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-WAERS      = 'USD'.
    ELSE.
      WL_INPUT_ZIB-WAERS      = WG_T882G-CURR1.
    ENDIF.


    WL_INPUT_ZIB-BUPLA      = SPACE.

    WL_INPUT_ZIB-WAERS_I    = 'X'.

    IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-DMBTR      = WL_SAIDA-VLR_AJUST.
      IF WL_INPUT_ZIB-DMBTR LT 0.
        MULTIPLY WL_INPUT_ZIB-DMBTR BY -1.
      ENDIF.
    ELSE.
      WL_INPUT_ZIB-DMBTR      = 0.
    ENDIF.

    WL_INPUT_ZIB-WAERS_F    = WG_T882G-CURR2.
    IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-DMBE2     = 0.
    ELSE.
      WL_INPUT_ZIB-DMBE2      = WL_SAIDA-VLR_AJUST.
      IF WL_INPUT_ZIB-DMBE2 LT 0.
        MULTIPLY WL_INPUT_ZIB-DMBE2 BY -1.
      ENDIF.
    ENDIF.

    WL_INPUT_ZIB-RG_ATUALIZADO  = 'N'.

    APPEND WL_INPUT_ZIB TO TL_INPUT_ZIB.
    CLEAR: WL_INPUT_ZIB.


    MODIFY ZIB_CONTABIL FROM TABLE TL_INPUT_ZIB.
    COMMIT WORK.
    IF WL_ESTORNO IS INITIAL.
      WL_INPUT_0082-MANDT = SY-MANDT.
      WL_INPUT_0082-BUKRS = S_BUKRS-LOW.
      WL_INPUT_0082-MES_ANO = S_MES-LOW.
      WL_INPUT_0082-SAKNR = WL_SAIDA-RACCT.
      WL_INPUT_0082-VBUND = WL_SAIDA-RASSC.
      WL_INPUT_0082-DMBTR = WL_SAIDA-CURR1.
      WL_INPUT_0082-DMBE2 = WL_SAIDA-CURR2.
      WL_INPUT_0082-DMBE3 = WL_SAIDA-CURR3.
      WL_INPUT_0082-TX_USD = WL_SAIDA-TX_USD.
      WL_INPUT_0082-TX_BRL = WL_SAIDA-TX_BRL.
      WL_INPUT_0082-SDO_CORR_MI2 = WL_SAIDA-SALDO_CORR.
      WL_INPUT_0082-VLR_CORR_MI2 = WL_SAIDA-VLR_AJUST.
      IF WG_T001-LAND1 EQ 'AR'
      OR WG_T001-LAND1 EQ 'PY'.
        WL_INPUT_0082-SDO_CORR_MI3 = WL_SAIDA-SALDO_CORR2.
        WL_INPUT_0082-VLR_CORR_MI3 = WL_SAIDA-VLR_AJUST2.
      ELSEIF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR WG_T001-LAND1 EQ 'CH'.
        CLEAR: WL_INPUT_0082-SDO_CORR_MI3, WL_INPUT_0082-VLR_CORR_MI3.
      ENDIF.
      CONCATENATE 'ZFI0061' WL_OBJ_KEY VG_RYEAR INTO WL_INPUT_0082-OBJ_KEY.
      P_OBJ_KEY = WL_INPUT_0082-OBJ_KEY.

      WL_INPUT_0082-OBJ_KEY_EST  = SPACE.
      WL_INPUT_0082-USNAM        = SY-UNAME.
      WL_INPUT_0082-AEDAT        = SY-DATUM.
      WL_INPUT_0082-CPUTM        = SY-UZEIT.

      MODIFY  ZFIT0082 FROM WL_INPUT_0082.
      COMMIT WORK.
      WG_SAIDA_EXEC-ICON   = ICON_GREEN_LIGHT.
      WG_SAIDA_EXEC-RACCT = WL_SAIDA-RACCT.
      WG_SAIDA_EXEC-TXT50 = WL_SAIDA-TXT50.
*      WG_SAIDA_EXEC-MSG   = 'Documento foi importado na ZIB_CONTABIL com sucesso.' .
      WG_SAIDA_EXEC-MSG   = TEXT-053 .
      APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
      CLEAR: WL_INPUT_0082, WG_SAIDA_EXEC.
    ELSE.
      CONCATENATE 'ZFI0061' WL_OBJ_KEY VG_RYEAR INTO WL_INPUT_0082-OBJ_KEY_EST.
      P_OBJ_KEY = WL_INPUT_0082-OBJ_KEY_EST.

      UPDATE ZFIT0082 SET OBJ_KEY_EST = WL_INPUT_0082-OBJ_KEY_EST
                      WHERE BUKRS   EQ S_BUKRS-LOW
                        AND MES_ANO EQ S_MES-LOW
                        AND SAKNR   EQ WL_SAIDA-RACCT
                        AND VBUND   EQ WL_SAIDA-RASSC.
      COMMIT WORK.

      WG_SAIDA_EXEC-ICON  = ICON_GREEN_LIGHT.
      WG_SAIDA_EXEC-RACCT = WL_SAIDA-RACCT.
      WG_SAIDA_EXEC-TXT50 = WL_SAIDA-TXT50.
      WG_SAIDA_EXEC-MSG   = TEXT-054 .
      APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.

    ENDIF.
  ELSE.
    CLEAR: WG_SAIDA_EXEC.
    WG_SAIDA_EXEC-ICON   = ICON_RED_LIGHT.
    WG_SAIDA_EXEC-RACCT  = WL_SAIDA-RACCT.
    WG_SAIDA_EXEC-TXT50  = WL_SAIDA-TXT50.
    WG_SAIDA_EXEC-MSG    = TEXT-055.
    APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
  ENDIF.
ENDFORM.                    " GERA_CONTABIL

*&---------------------------------------------------------------------*
*&      Form  GERA_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_SAIDA  text
*----------------------------------------------------------------------*
FORM GERA_CONTABIL  USING    WL_SAIDA TYPE TY_SAIDA
                             WL_ESTORNO
                             WL_VARICAO
                   CHANGING  P_OBJ_KEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY.

  DATA: WL_INPUT_ZIB  TYPE ZIB_CONTABIL,
        TL_INPUT_ZIB  TYPE TABLE OF ZIB_CONTABIL,
        WL_INPUT_0084 TYPE ZFIT0084,
        WA_ZFIT0084   TYPE ZFIT0084,
        WA_ZFIT0086   TYPE ZFIT0086,
        WA_SAIDA_AUX  TYPE TY_SAIDA,
        V_TIPO        TYPE C LENGTH 30,
        WL_OBJ_KEY(9),
        VANOMES(7).

  SELECT SINGLE *
    FROM ZFIT0086
    INTO WA_ZFIT0086
    WHERE SAKNR = WL_SAIDA-HKONT.

  IF SY-SUBRC NE 0.
*    MESSAGE E836(SD) WITH 'Cadastrar fornecedor/cliente'
*                          'para esta conta'.
    MESSAGE E836(SD) WITH TEXT-017
                          TEXT-018.
    EXIT.
  ENDIF.

  CONCATENATE S_MES-LOW(2) '/' S_MES-LOW+2(4) INTO VANOMES.
  REFRESH: TL_INPUT_ZIB.
  CLEAR: WL_INPUT_ZIB, WL_INPUT_0084.
  READ TABLE TG_T001 INTO WG_T001 WITH KEY BUKRS =  S_BUKRS-LOW BINARY SEARCH.
  READ TABLE TG_T882G INTO WG_T882G WITH KEY RBUKRS = S_BUKRS-LOW BINARY SEARCH.

  LOOP AT TG_0081 INTO WG_0081.
    IF WG_0081-CONTA_DE IS NOT INITIAL.
      IF WL_SAIDA-HKONT EQ WG_0081-CONTA_DE.
        EXIT.
      ENDIF.
    ENDIF.
    CLEAR:WG_0081.
  ENDLOOP.

  IF WG_0081 IS NOT INITIAL .
    IF WG_T001-LAND1 EQ 'AR' OR WG_T001-LAND1 EQ 'PY'.
      V_SAKNR      =  WG_0081-SAKNR.
    ELSEIF WG_0081-SAKNR IS NOT INITIAL.
      V_SAKNR      =  WG_0081-SAKNR.
    ELSEIF WG_0081-HKONT IS NOT INITIAL.
      V_SAKNR      =  WG_0081-HKONT.
    ENDIF.

    "Sumariza conta por VBUND e BEWAR
    IF P_OBJ_KEY IS NOT INITIAL .
      IF WL_VARICAO IS NOT  INITIAL.
        TG_SAIDA_AUX[]  = TG_SAIDA[].
        DELETE TG_SAIDA_AUX WHERE OBJ_KEY     NE P_OBJ_KEY.
        SORT TG_SAIDA_AUX BY VBUND BEWAR.
        DELETE ADJACENT DUPLICATES FROM TG_SAIDA_AUX  COMPARING VBUND BEWAR.
        LOOP AT TG_SAIDA_AUX INTO WA_SAIDA_AUX.
          WA_SAIDA_AUX-VLR_AJUST  = 0.
          WA_SAIDA_AUX-VLR_AJUST2 = 0.
          LOOP AT TG_SAIDA INTO WG_SAIDA WHERE HKONT = WA_SAIDA_AUX-HKONT
                                         AND   VBUND = WA_SAIDA_AUX-VBUND
                                         AND   BEWAR = WA_SAIDA_AUX-BEWAR.
            IF WL_ESTORNO IS NOT INITIAL.
              SELECT SINGLE *
                FROM ZFIT0084
                   INTO WA_ZFIT0084
                   WHERE BUKRS   EQ S_BUKRS-LOW
                   AND MES_ANO EQ S_MES-LOW
                   AND SAKNR   EQ WG_SAIDA-HKONT
                   AND BELNR   EQ WG_SAIDA-BELNR2
                   AND BUZEI   EQ WG_SAIDA-BUZEI.
              IF SY-SUBRC = 0.
                ADD WA_ZFIT0084-VLR_CORR_MI2 TO WA_SAIDA_AUX-VLR_AJUST.
                ADD WA_ZFIT0084-VLR_CORR_MI3 TO WA_SAIDA_AUX-VLR_AJUST2.
              ENDIF.
            ELSE.
              ADD WG_SAIDA-VLR_AJUST  TO WA_SAIDA_AUX-VLR_AJUST.
              ADD WG_SAIDA-VLR_AJUST2 TO WA_SAIDA_AUX-VLR_AJUST2.
            ENDIF.
          ENDLOOP.
          WA_SAIDA_AUX-KUNNR = WA_ZFIT0086-CD_CLI_FOR.
          APPEND WA_SAIDA_AUX TO TG_SAIDA_RES.
        ENDLOOP.
      ENDIF.
    ELSE.
      IF WL_VARICAO IS NOT  INITIAL.
        TG_SAIDA_AUX[]  = TG_SAIDA[].
        DELETE TG_SAIDA_AUX WHERE HKONT       NE WL_SAIDA-HKONT.
        DELETE TG_SAIDA_AUX WHERE OBJ_KEY     IS NOT INITIAL
                            AND   OBJ_KEY_EST IS INITIAL.
        SORT TG_SAIDA_AUX BY VBUND BEWAR.
        DELETE ADJACENT DUPLICATES FROM TG_SAIDA_AUX  COMPARING VBUND BEWAR.
        LOOP AT TG_SAIDA_AUX INTO WA_SAIDA_AUX.
          WA_SAIDA_AUX-VLR_AJUST  = 0.
          WA_SAIDA_AUX-VLR_AJUST2 = 0.
          LOOP AT TG_SAIDA INTO WG_SAIDA WHERE HKONT = WA_SAIDA_AUX-HKONT
                                         AND   VBUND = WA_SAIDA_AUX-VBUND
                                         AND   BEWAR = WA_SAIDA_AUX-BEWAR.
            ADD WG_SAIDA-VLR_AJUST  TO WA_SAIDA_AUX-VLR_AJUST.
            ADD WG_SAIDA-VLR_AJUST2 TO WA_SAIDA_AUX-VLR_AJUST2.
          ENDLOOP.
          WA_SAIDA_AUX-KUNNR = WA_ZFIT0086-CD_CLI_FOR.
          APPEND WA_SAIDA_AUX TO TG_SAIDA_RES.
        ENDLOOP.
      ENDIF.
    ENDIF.
    "
    IF TG_SAIDA_RES[] IS INITIAL.
      MESSAGE TEXT-019 TYPE 'I'.
      EXIT.
    ENDIF.

    LOOP AT TG_SAIDA_RES INTO WL_SAIDA. "ALRS
      TABIX = SY-TABIX.
      V_VBUND = WL_SAIDA-VBUND.

      PERFORM GET_NEXT_NUMBER USING 'Z_SALDOMO2' '01' CHANGING WL_OBJ_KEY.

** 1 Partida *************************************************************************************************************
      WL_INPUT_ZIB-MANDT    = SY-MANDT.
      CONCATENATE 'ZFI0063' WL_OBJ_KEY VG_RYEAR INTO  WL_INPUT_ZIB-OBJ_KEY.
      WL_INPUT_ZIB-SEQITEM    = '0001'.
      CONCATENATE S_MES-LOW(2) '/' S_MES-LOW+2(4) INTO WL_INPUT_ZIB-XBLNR.

      PERFORM BUSCA_CHAVE USING P_FORN WL_ESTORNO WL_VARICAO WL_SAIDA-VLR_AJUST WL_SAIDA-UMSKZ CHANGING WL_INPUT_ZIB-BSCHL.

      IF WG_T001-LAND1 EQ 'BR'.
        CONCATENATE S_BUKRS-LOW+2(2) '01' INTO WL_INPUT_ZIB-GSBER.
      ELSEIF WG_T001-LAND1 EQ 'NL'.
        IF S_BUKRS-LOW EQ '0201'.
          WL_INPUT_ZIB-GSBER = 'H201'.
        ELSE.
          WL_INPUT_ZIB-GSBER = 'H202'.
        ENDIF.

      ELSEIF WG_T001-LAND1 EQ 'LU'.
        IF S_BUKRS-LOW EQ '0203'.
          WL_INPUT_ZIB-GSBER = 'L203'.
        ENDIF.
      ELSEIF WG_T001-LAND1 EQ 'CH'.
        WL_INPUT_ZIB-GSBER = 'S201'.
      ELSEIF WG_T001-LAND1 EQ 'AR' OR WG_T001-LAND1 EQ 'PY'.
        IF S_BUKRS-LOW EQ '0100'.
          WL_INPUT_ZIB-GSBER = 'T001'.
        ELSE.
          WL_INPUT_ZIB-GSBER = 'P001'.
        ENDIF.
      ENDIF.
      WL_INPUT_ZIB-BUKRS      = S_BUKRS-LOW.
      WL_INPUT_ZIB-INTERFACE  = '35'.
*      WL_INPUT_ZIB-BKTXT      = 'VAR.CTAS MONETÁRIAS'.
      WL_INPUT_ZIB-BKTXT      = TEXT-020.
      WRITE VG_LAST_DAY TO WL_INPUT_ZIB-BLDAT.
      WRITE VG_LAST_DAY TO WL_INPUT_ZIB-BUDAT.
      WL_INPUT_ZIB-GJAHR      = VG_RYEAR.
      WL_INPUT_ZIB-MONAT      = S_MES-LOW(2).
      WL_INPUT_ZIB-BLART      = 'VC'.
      WL_INPUT_ZIB-HKONT      = WL_SAIDA-KUNNR.

      WL_INPUT_ZIB-WRBTR      =  0.


      IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
        WL_INPUT_ZIB-WAERS    = 'USD'.
      ELSE.
        WL_INPUT_ZIB-WAERS    = WG_T882G-CURR1.
      ENDIF.

      WL_INPUT_ZIB-BUPLA      = SPACE.
      IF WG_T001-LAND1 EQ 'AR'
         OR WG_T001-LAND1 EQ 'PY'.
*        CONCATENATE 'Ajuste moeda funcional ' VANOMES
        CONCATENATE TEXT-021 VANOMES
              INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
      ELSEIF WG_0081-SAKNR IS NOT INITIAL.
*        CONCATENATE 'Ajuste moeda funcional ' VANOMES
        CONCATENATE TEXT-021 VANOMES
            INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
      ELSEIF WG_0081-HKONT IS NOT INITIAL.
        CONCATENATE TEXT-022 VANOMES
            INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
      ENDIF.

      WL_INPUT_ZIB-VBUND      = V_VBUND.

      WL_INPUT_ZIB-BEWAR      = WL_SAIDA-BEWAR.

      WL_INPUT_ZIB-WAERS_I    = 'X'.
      IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
        WL_INPUT_ZIB-DMBTR      = WL_SAIDA-VLR_AJUST.
        IF WL_INPUT_ZIB-DMBTR LT 0.
          MULTIPLY WL_INPUT_ZIB-DMBTR BY -1.
        ENDIF.
      ELSE.
        WL_INPUT_ZIB-DMBTR      = 0.
      ENDIF.

      WL_INPUT_ZIB-WAERS_F    = WG_T882G-CURR2.
      IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
        WL_INPUT_ZIB-DMBE2     = 0.
      ELSE.
        IF WL_SAIDA-VLR_AJUST GT 0.
          WL_INPUT_ZIB-DMBE2 = WL_SAIDA-VLR_AJUST.
        ELSE.
          WL_INPUT_ZIB-DMBE2 = WL_SAIDA-VLR_AJUST * -1.
        ENDIF.
      ENDIF.

      WL_INPUT_ZIB-ZUONR         = WL_SAIDA-BELNR2.
      WL_INPUT_ZIB-UMSKZ         = WL_SAIDA-UMSKZ.
      WL_INPUT_ZIB-RG_ATUALIZADO = 'N'.

      APPEND WL_INPUT_ZIB TO TL_INPUT_ZIB.
      CLEAR: WL_INPUT_ZIB.

** 2 Partida *************************************************************************************************************************
      WL_INPUT_ZIB-MANDT    = SY-MANDT.
      CONCATENATE 'ZFI0063' WL_OBJ_KEY VG_RYEAR INTO  WL_INPUT_ZIB-OBJ_KEY.
      WL_INPUT_ZIB-SEQITEM    = '0002'.
      CONCATENATE S_MES-LOW(2) '/' S_MES-LOW+2(4) INTO WL_INPUT_ZIB-XBLNR.
      IF WL_ESTORNO IS INITIAL.
        IF WL_SAIDA-VLR_AJUST LT 0.
          WL_INPUT_ZIB-BSCHL      = '40'.
        ELSE.
          WL_INPUT_ZIB-BSCHL      = '50'.
        ENDIF.
      ELSE.
        IF WL_SAIDA-VLR_AJUST LT 0.
          WL_INPUT_ZIB-BSCHL      = '50'.
        ELSE.
          WL_INPUT_ZIB-BSCHL      = '40'.
        ENDIF.
      ENDIF.
      IF WG_T001-LAND1 EQ 'BR'.
        CONCATENATE S_BUKRS-LOW+2(2) '01' INTO WL_INPUT_ZIB-GSBER.
      ELSEIF WG_T001-LAND1 EQ 'NL'.
        IF S_BUKRS-LOW EQ '0201'.
          WL_INPUT_ZIB-GSBER = 'H201'.
        ELSE.
          WL_INPUT_ZIB-GSBER = 'H202'.
        ENDIF.
      ELSEIF WG_T001-LAND1 EQ 'LU'.
        IF S_BUKRS-LOW EQ '0203'.
          WL_INPUT_ZIB-GSBER = 'L203'.
        ENDIF.
      ELSEIF WG_T001-LAND1 EQ 'CH'.
        WL_INPUT_ZIB-GSBER = 'S201'.
      ELSEIF WG_T001-LAND1 EQ 'AR'
          OR WG_T001-LAND1 EQ 'PY'.
        IF S_BUKRS-LOW EQ '0100'.
          WL_INPUT_ZIB-GSBER = 'T001'.
        ELSE.
          WL_INPUT_ZIB-GSBER = 'P001'.
        ENDIF.
      ENDIF.
      WL_INPUT_ZIB-BUKRS      = S_BUKRS-LOW.
      WL_INPUT_ZIB-INTERFACE  = '35'.
*      WL_INPUT_ZIB-BKTXT      = 'VAR.CTAS MONETÁRIAS'.
      WL_INPUT_ZIB-BKTXT      = TEXT-020.
      WRITE VG_LAST_DAY TO WL_INPUT_ZIB-BLDAT.
      WRITE VG_LAST_DAY TO WL_INPUT_ZIB-BUDAT.
      WL_INPUT_ZIB-GJAHR      = VG_RYEAR.
      WL_INPUT_ZIB-MONAT      = S_MES-LOW(2).
      WL_INPUT_ZIB-BLART      = 'VC'.

      IF WG_T001-LAND1 EQ 'AR' OR WG_T001-LAND1 EQ 'PY'.
        WL_INPUT_ZIB-HKONT      =  WG_0081-SAKNR.
        CONCATENATE TEXT-021 VANOMES INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
      ELSEIF WG_0081-SAKNR IS NOT INITIAL.
        WL_INPUT_ZIB-HKONT      =  WG_0081-SAKNR.
        CONCATENATE TEXT-021 VANOMES INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
      ELSEIF WG_0081-HKONT IS NOT INITIAL.
        WL_INPUT_ZIB-HKONT      =  WG_0081-HKONT.
        CONCATENATE TEXT-022 VANOMES INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
      ENDIF.
      WL_INPUT_ZIB-VBUND      = V_VBUND.
      WL_INPUT_ZIB-BEWAR      = WL_SAIDA-BEWAR.

      WL_INPUT_ZIB-WRBTR      = 0.

      IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
        WL_INPUT_ZIB-WAERS      = 'USD'.
      ELSE.
        WL_INPUT_ZIB-WAERS      = WG_T882G-CURR1.
      ENDIF.

      WL_INPUT_ZIB-BUPLA      = SPACE.

      WL_INPUT_ZIB-WAERS_I    = 'X'.

      IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
        WL_INPUT_ZIB-DMBTR      = WL_SAIDA-VLR_AJUST.
        IF WL_INPUT_ZIB-DMBTR LT 0.
          MULTIPLY WL_INPUT_ZIB-DMBTR BY -1.
        ENDIF.
      ELSE.
        WL_INPUT_ZIB-DMBTR      = 0.
      ENDIF.

      WL_INPUT_ZIB-WAERS_F    = WG_T882G-CURR2.

      IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
        WL_INPUT_ZIB-DMBE2     = 0.
      ELSE.
        IF WL_SAIDA-VLR_AJUST GT 0.
          WL_INPUT_ZIB-DMBE2      = WL_SAIDA-VLR_AJUST.
        ELSE.
          WL_INPUT_ZIB-DMBE2      = WL_SAIDA-VLR_AJUST * -1.
        ENDIF.
      ENDIF.

      WL_INPUT_ZIB-ZUONR          = WL_SAIDA-BELNR2.
      WL_INPUT_ZIB-RG_ATUALIZADO  = 'N'.

      APPEND WL_INPUT_ZIB TO TL_INPUT_ZIB.
      CLEAR: WL_INPUT_ZIB.


      "Grava Chave
      IF WL_ESTORNO IS INITIAL.
        IF WL_VARICAO IS NOT INITIAL.
          CONCATENATE 'ZFI0063' WL_OBJ_KEY VG_RYEAR INTO WL_SAIDA-OBJ_KEY.
          WL_SAIDA-OBJ_KEY_EST = SPACE.
        ELSE.
          CONCATENATE 'ZFI0063' WL_OBJ_KEY VG_RYEAR INTO WL_SAIDA-OBJ_KEY_INV.
          WL_SAIDA-OBJ_KEY_INV_EST = SPACE.
        ENDIF.
      ELSE.
        IF WL_VARICAO IS NOT INITIAL.
          CONCATENATE 'ZFI0063' WL_OBJ_KEY VG_RYEAR INTO WL_SAIDA-OBJ_KEY_EST.
        ELSE.
          CONCATENATE 'ZFI0063' WL_OBJ_KEY VG_RYEAR INTO WL_SAIDA-OBJ_KEY_INV_EST.
        ENDIF.
      ENDIF.
      MODIFY TG_SAIDA_RES FROM WL_SAIDA INDEX TABIX TRANSPORTING OBJ_KEY OBJ_KEY_EST OBJ_KEY_INV OBJ_KEY_INV_EST.
    ENDLOOP. "ALRS

    "inverte todas de uma vez só 09.03.2015
    CLEAR: WG_SAIDA_EXEC.
    IF WL_VARICAO IS INITIAL.
      V_TIPO = 'Inverso'.
      LOOP AT TL_INPUT_ZIB INTO WL_INPUT_ZIB.
        TABIX2 = SY-TABIX.
        PERFORM INVERTE_CHAVE USING WG_SAIDA_EXEC CHANGING WL_INPUT_ZIB.
        IF WG_SAIDA_EXEC IS NOT INITIAL.
          WG_SAIDA_EXEC-TIPO  = V_TIPO.
          WG_SAIDA_EXEC-LIFNR = WL_SAIDA-KUNNR.
          WG_SAIDA_EXEC-NAME1 = WL_SAIDA-NAME1.
          WG_SAIDA_EXEC-HKONT = WL_SAIDA-HKONT.
          WG_SAIDA_EXEC-TXT50 = WL_SAIDA-TXT50.
          APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
        ENDIF.
        MODIFY TL_INPUT_ZIB INDEX TABIX2 FROM WL_INPUT_ZIB TRANSPORTING BLDAT BUDAT BSCHL GJAHR MONAT.
      ENDLOOP.
    ELSE.
      V_TIPO = TEXT-M12. "'Variação'.
    ENDIF.

    "CHECK WG_SAIDA_EXEC IS INITIAL. "ALRS 10.03.2015
    IF TL_INPUT_ZIB[] IS INITIAL.
      MESSAGE TEXT-023 TYPE 'I'.
      EXIT.
    ENDIF.
    MODIFY ZIB_CONTABIL FROM TABLE TL_INPUT_ZIB.
    IF SY-SUBRC NE 0.
      MESSAGE TEXT-024 TYPE 'I'.
      EXIT.
    ENDIF.
    COMMIT WORK.

    " Atualiza ALV (tela)
    LOOP AT TG_SAIDA_RES INTO WA_SAIDA_AUX.
      LOOP AT TG_SAIDA INTO WL_SAIDA WHERE HKONT = WA_SAIDA_AUX-HKONT
                                     AND   VBUND = WA_SAIDA_AUX-VBUND
                                     AND   BEWAR = WA_SAIDA_AUX-BEWAR.
        TABIX = SY-TABIX.
        IF WL_ESTORNO IS INITIAL.
          WL_INPUT_0084-MANDT        = SY-MANDT.
          WL_INPUT_0084-BUKRS        = S_BUKRS-LOW.
          WL_INPUT_0084-MES_ANO      = S_MES-LOW.
          WL_INPUT_0084-SAKNR        = WL_SAIDA-HKONT.
          WL_INPUT_0084-BELNR        = WL_SAIDA-BELNR2.
          WL_INPUT_0084-BUZEI        = WL_SAIDA-BUZEI.
          WL_INPUT_0084-LIFNR        = WL_SAIDA-KUNNR.
          WL_INPUT_0084-DMBTR        = WL_SAIDA-DMBTR.
          WL_INPUT_0084-DMBE2        = WL_SAIDA-DMBE2.
          WL_INPUT_0084-DMBE3        = WL_SAIDA-DMBE3.
          WL_INPUT_0084-TX_USD       = WL_SAIDA-TX_USD.
          WL_INPUT_0084-TX_BRL       = WL_SAIDA-TX_BRL.
          WL_INPUT_0084-SDO_CORR_MI2 = WL_SAIDA-SALDO_CORR.
          WL_INPUT_0084-VLR_CORR_MI2 = WL_SAIDA-VLR_AJUST.

          WL_INPUT_0084-OBJ_KEY      = WA_SAIDA_AUX-OBJ_KEY.
          WL_INPUT_0084-OBJ_KEY_INV  = WA_SAIDA_AUX-OBJ_KEY_INV.
          "
          IF WG_T001-LAND1 EQ 'AR' OR WG_T001-LAND1 EQ 'PY'.
            WL_INPUT_0084-SDO_CORR_MI3 = WL_SAIDA-SALDO_CORR2.
            WL_INPUT_0084-VLR_CORR_MI3 = WL_SAIDA-VLR_AJUST2.
          ELSEIF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL'  OR WG_T001-LAND1 EQ 'CH'.
            CLEAR: WL_INPUT_0084-SDO_CORR_MI3, WL_INPUT_0084-VLR_CORR_MI3.
          ENDIF.

          WA_SAIDA_AUX-OBJ_KEY      = WA_SAIDA_AUX-OBJ_KEY.
          WA_SAIDA_AUX-OBJ_KEY_INV  = WA_SAIDA_AUX-OBJ_KEY_INV.

          IF WL_VARICAO IS NOT INITIAL.
            WL_INPUT_0084-OBJ_KEY_EST = SPACE.
            WA_SAIDA_AUX-OBJ_KEY_EST = SPACE.
            MODIFY TG_SAIDA FROM WA_SAIDA_AUX INDEX TABIX TRANSPORTING OBJ_KEY OBJ_KEY_EST.
          ELSE.
            WL_INPUT_0084-OBJ_KEY_INV_EST = SPACE.
            WA_SAIDA_AUX-OBJ_KEY_INV_EST = SPACE.
            MODIFY TG_SAIDA FROM WA_SAIDA_AUX INDEX TABIX TRANSPORTING OBJ_KEY_INV OBJ_KEY_INV_EST.
          ENDIF.
          WL_INPUT_0084-USNAM        = SY-UNAME.
          WL_INPUT_0084-AEDAT        = SY-DATUM.
          WL_INPUT_0084-CPUTM        = SY-UZEIT.

          MODIFY ZFIT0084 FROM WL_INPUT_0084.
          COMMIT WORK.

          CLEAR: WL_INPUT_0084.
        ELSE.
          IF WL_VARICAO IS NOT INITIAL.
            UPDATE ZFIT0084
               SET OBJ_KEY_EST = WA_SAIDA_AUX-OBJ_KEY_EST
             WHERE BUKRS   EQ S_BUKRS-LOW
               AND MES_ANO EQ S_MES-LOW
               AND SAKNR   EQ WL_SAIDA-HKONT
               AND BELNR   EQ WL_SAIDA-BELNR2
               AND BUZEI   EQ WL_SAIDA-BUZEI.
            IF SY-SUBRC NE 0.
              UPDATE ZFIT0084
                SET OBJ_KEY_EST = WA_SAIDA_AUX-OBJ_KEY_EST
              WHERE BUKRS   EQ S_BUKRS-LOW
                AND MES_ANO EQ S_MES-LOW
                AND SAKNR   EQ WL_SAIDA-HKONT
                AND BELNR   EQ WL_SAIDA-BELNR2.
            ENDIF.
            MODIFY TG_SAIDA FROM WA_SAIDA_AUX INDEX TABIX TRANSPORTING OBJ_KEY_EST.
          ELSE.
            UPDATE ZFIT0084
               SET OBJ_KEY_INV_EST = WA_SAIDA_AUX-OBJ_KEY_INV_EST
             WHERE BUKRS   EQ S_BUKRS-LOW
               AND MES_ANO EQ S_MES-LOW
               AND SAKNR   EQ WL_SAIDA-HKONT
               AND BELNR   EQ WL_SAIDA-BELNR2
               AND BUZEI   EQ WL_SAIDA-BUZEI.
            IF SY-SUBRC NE 0.
              UPDATE ZFIT0084
               SET OBJ_KEY_INV_EST = WA_SAIDA_AUX-OBJ_KEY_INV_EST
             WHERE BUKRS   EQ S_BUKRS-LOW
               AND MES_ANO EQ S_MES-LOW
               AND SAKNR   EQ WL_SAIDA-HKONT
               AND BELNR   EQ WL_SAIDA-BELNR2.
            ENDIF.
            MODIFY TG_SAIDA FROM WA_SAIDA_AUX INDEX TABIX TRANSPORTING OBJ_KEY_INV_EST.
          ENDIF.
          COMMIT WORK.
        ENDIF.
      ENDLOOP.
      IF WL_ESTORNO IS INITIAL.
        WG_SAIDA_EXEC-ICON  = ICON_GREEN_LIGHT.
        WG_SAIDA_EXEC-TIPO  = V_TIPO.
        WG_SAIDA_EXEC-LIFNR = WA_SAIDA_AUX-KUNNR.
        WG_SAIDA_EXEC-NAME1 = WA_SAIDA_AUX-NAME1.
        WG_SAIDA_EXEC-HKONT = WA_SAIDA_AUX-HKONT.
        WG_SAIDA_EXEC-TXT50 = WA_SAIDA_AUX-TXT50.
        WG_SAIDA_EXEC-MSG   = TEXT-025 .
        APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
        CLEAR WG_SAIDA_EXEC.
      ELSE.
        WG_SAIDA_EXEC-ICON  = ICON_GREEN_LIGHT.
        WG_SAIDA_EXEC-TIPO  = V_TIPO.
        WG_SAIDA_EXEC-LIFNR = WA_SAIDA_AUX-KUNNR.
        WG_SAIDA_EXEC-NAME1 = WA_SAIDA_AUX-NAME1.
        WG_SAIDA_EXEC-HKONT = WA_SAIDA_AUX-HKONT.
        WG_SAIDA_EXEC-TXT50 = WA_SAIDA_AUX-TXT50.
        WG_SAIDA_EXEC-MSG   = TEXT-026 .
        APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
        CLEAR WG_SAIDA_EXEC.
      ENDIF.

    ENDLOOP.
  ELSE.
    CLEAR: WG_SAIDA_EXEC.
    WG_SAIDA_EXEC-ICON   = ICON_RED_LIGHT.
    WG_SAIDA_EXEC-TIPO   = V_TIPO.
    WG_SAIDA_EXEC-LIFNR  = WL_SAIDA-KUNNR.
    WG_SAIDA_EXEC-NAME1  = WL_SAIDA-NAME1.
    WG_SAIDA_EXEC-HKONT  = WL_SAIDA-HKONT.
    WG_SAIDA_EXEC-TXT50  = WL_SAIDA-TXT50.
    WG_SAIDA_EXEC-MSG    = TEXT-027.
    APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
  ENDIF.
ENDFORM.                    " GERA_CONTABIL
*&---------------------------------------------------------------------*
*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
FORM GET_NEXT_NUMBER  USING    P_OBJECT   "TYPE nrobj
                               P_NR_RANGE "TYPE nrnr
*                               P_COMMIT
                      CHANGING P_NUMBER.

*  IF P_COMMIT IS NOT INITIAL.
  CLEAR P_NUMBER.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = P_NR_RANGE
      OBJECT                  = P_OBJECT
    IMPORTING
      NUMBER                  = P_NUMBER
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CLEAR: P_NUMBER.
    MESSAGE E836(SD) WITH TEXT-028
                          TEXT-029.

  ENDIF.
*  ELSE.
*    P_NUMBER = '$00000001'.
*    WG_FLAG = C_X.
*  ENDIF.

ENDFORM.                    " get_next_number
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_EXEC .
  IF TG_SAIDA_EXEC[] IS NOT INITIAL.
    PERFORM MONTAR_LAYOUT USING 'TG_SAIDA_EXEC'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       i_callback_program    = v_report
*       I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
        IT_FIELDCAT           = ESTRUTURA[]
*       IT_SORT               = T_SORT[]
        I_SAVE                = 'A'
        I_SCREEN_START_COLUMN = 3
        I_SCREEN_START_LINE   = 3
        I_SCREEN_END_COLUMN   = 100
        I_SCREEN_END_LINE     = 13
      TABLES
        T_OUTTAB              = TG_SAIDA_EXEC.
  ENDIF.
ENDFORM.                    " IMPRIMIR_EXEC

FORM ATUALIZA_SAIDAR  TABLES   TL_SAIDA   LIKE TG_SAIDAR
                              TL_ZIB     STRUCTURE ZIB_CONTABIL
                              TL_ZIB_CHV STRUCTURE ZIB_CONTABIL_CHV
                              TL_ZIB_ERR STRUCTURE ZIB_CONTABIL_ERR.

  SORT: TL_ZIB     BY OBJ_KEY RG_ATUALIZADO,
        TL_ZIB_CHV BY OBJ_KEY,
        TL_ZIB_ERR BY OBJ_KEY.

  LOOP AT TL_SAIDA.
    IF TL_SAIDA-OBJ_KEY IS NOT INITIAL.
      CLEAR: TL_ZIB, TL_SAIDA-BELNR, WG_BKPF_FB08, WG_BKPF_FB08_E, TL_SAIDA-STBLG.
      READ TABLE TL_ZIB
        WITH KEY OBJ_KEY       = TL_SAIDA-OBJ_KEY
                 RG_ATUALIZADO = 'S'
                         BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        CLEAR: TL_ZIB_CHV.
        READ TABLE TL_ZIB_CHV
        WITH KEY OBJ_KEY       = TL_SAIDA-OBJ_KEY
                         BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          TL_SAIDA-BELNR = TL_ZIB_CHV-BELNR.

          IF SY-SUBRC = 0.
            SELECT SINGLE   BUKRS BELNR GJAHR BUDAT
               FROM BKPF
               INTO WG_BKPF_FB08_E
               WHERE BUKRS EQ WG_BKPF_FB08-BUKRS
               AND   BELNR EQ WG_BKPF_FB08-STBLG
               AND   GJAHR EQ WG_BKPF_FB08-STJAH.
            IF WG_BKPF_FB08_E-BUDAT+0(6) = WG_BKPF_FB08-BUDAT+0(6). "estorno e lançamento mesmo mês, limpa pra gerar outro documento
              DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY  = TL_SAIDA-OBJ_KEY.
*              clear: tl_saida-belnr, tl_saida-obj_key.
            ELSE.
              TL_SAIDA-LINE_COLOR = 'C601'.
              MODIFY TL_SAIDA TRANSPORTING LINE_COLOR.
            ENDIF.
          ENDIF.

          "FB08
          CLEAR: WG_BKPF_FB08.
          SELECT SINGLE   BUKRS BELNR  GJAHR  BUDAT STBLG STJAH
            FROM BKPF
            INTO WG_BKPF_FB08
            WHERE BUKRS EQ TL_ZIB_CHV-BUKRS
            AND   BELNR EQ TL_ZIB_CHV-BELNR
            AND   GJAHR EQ TL_ZIB_CHV-GJAHR
            AND   STBLG NE ''.
*          " US 155163 // MMSILVA - 16.10.2024
          IF P_REVE IS NOT INITIAL.
            TL_SAIDA-STBLG = WG_BKPF_FB08-STBLG.
          ENDIF.
*          " US 155163 // MMSILVA - 16.10.2024

          TL_SAIDA-LOG   = ICON_ENTER_MORE.
          MODIFY TL_SAIDA TRANSPORTING BELNR  OBJ_KEY  LOG STBLG.
          CLEAR: TL_SAIDA-STBLG, WG_BKPF_FB08-STBLG.
        ELSE.
          CLEAR: TL_ZIB_ERR.
          READ TABLE TL_ZIB_ERR
            WITH KEY OBJ_KEY       = TL_SAIDA-OBJ_KEY
                             BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            TL_SAIDA-BELNR = SPACE.
            TL_SAIDA-LOG   = ICON_DISPLAY_MORE.
            MODIFY TL_SAIDA TRANSPORTING BELNR LOG.
          ENDIF.
        ENDIF.
      ELSE.
        TL_SAIDA-BELNR = ICON_OPERATION.
        MODIFY TL_SAIDA TRANSPORTING BELNR.
      ENDIF.
    ELSE.
      TL_SAIDA-OBJ_KEY = SPACE.
      TL_SAIDA-BELNR = SPACE.
      TL_SAIDA-LOG   = ICON_ENTER_MORE.
      MODIFY TL_SAIDA TRANSPORTING LOG BELNR OBJ_KEY.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " ATUALIZA_SAIDA

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TG_SAIDA  text
*      -->P_TL_ZIB  text
*      -->P_TL_ZIB_CHV  text
*      -->P_TL_ZIB_ERR  text
*----------------------------------------------------------------------*
FORM ATUALIZA_SAIDA  TABLES   TL_SAIDA   LIKE TG_SAIDA
                              TL_ZIB     STRUCTURE ZIB_CONTABIL
                              TL_ZIB_CHV STRUCTURE ZIB_CONTABIL_CHV
                              TL_ZIB_ERR STRUCTURE ZIB_CONTABIL_ERR.

  SORT: TL_ZIB     BY OBJ_KEY RG_ATUALIZADO,
        TL_ZIB_CHV BY OBJ_KEY,
        TL_ZIB_ERR BY OBJ_KEY.

*  IF P_VISU EQ 'X'.
*    EXIT.
*  ENDIF.

  LOOP AT TL_SAIDA.

    " Ajusta Visualização de Lançamento da Variação Cambial """""""""""""""""""""""""""
    "**********************************************************************************
    IF TL_SAIDA-OBJ_KEY IS NOT INITIAL.
      CLEAR: TL_ZIB.
      READ TABLE TL_ZIB WITH KEY OBJ_KEY = TL_SAIDA-OBJ_KEY RG_ATUALIZADO = 'S' BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        CLEAR: TL_ZIB_CHV.
        READ TABLE TL_ZIB_CHV WITH KEY OBJ_KEY = TL_SAIDA-OBJ_KEY BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          TL_SAIDA-BELNR = TL_ZIB_CHV-BELNR.
          IF SY-SUBRC = 0.
            SELECT SINGLE   BUKRS BELNR GJAHR BUDAT
               FROM BKPF
               INTO WG_BKPF_FB08_E
               WHERE BUKRS EQ WG_BKPF_FB08-BUKRS
               AND   BELNR EQ WG_BKPF_FB08-STBLG
               AND   GJAHR EQ WG_BKPF_FB08-STJAH.
            IF WG_BKPF_FB08_E-BUDAT+0(6) = WG_BKPF_FB08-BUDAT+0(6). "estorno e lançamento mesmo mês, limpa pra gerar outro documento
              DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY  = TL_SAIDA-OBJ_KEY.
*              clear: tl_saida-belnr, tl_saida-obj_key.
            ELSE.
              TL_SAIDA-LINE_COLOR = 'C601'.
              MODIFY TL_SAIDA TRANSPORTING LINE_COLOR.
            ENDIF.
          ENDIF.

          "FB08
          CLEAR: WG_BKPF_FB08.
          SELECT SINGLE   BUKRS BELNR  GJAHR  BUDAT STBLG STJAH
            FROM BKPF
            INTO WG_BKPF_FB08
            WHERE BUKRS EQ TL_ZIB_CHV-BUKRS
            AND   BELNR EQ TL_ZIB_CHV-BELNR
            AND   GJAHR EQ TL_ZIB_CHV-GJAHR
            AND   STBLG NE ''.
*          " US 155163 // MMSILVA - 16.10.2024
          IF P_REVE IS NOT INITIAL.
            TL_SAIDA-STBLG = WG_BKPF_FB08-STBLG.
          ENDIF.
*            " US 155163 // MMSILVA - 16.10.2024
          TL_SAIDA-LOG   = ICON_ENTER_MORE.
          MODIFY TL_SAIDA TRANSPORTING BELNR OBJ_KEY LOG STBLG.
        ELSE.
          CLEAR: TL_ZIB_ERR.
          READ TABLE TL_ZIB_ERR WITH KEY OBJ_KEY = TL_SAIDA-OBJ_KEY BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
*            TL_SAIDA-OBJ_KEY = SPACE.
            TL_SAIDA-BELNR   = SPACE.
            TL_SAIDA-LOG    = ICON_DISPLAY_MORE.
            MODIFY TL_SAIDA TRANSPORTING BELNR  LOG.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE TL_ZIB WITH KEY OBJ_KEY = TL_SAIDA-OBJ_KEY BINARY SEARCH.
        IF SY-SUBRC = 0.
          TL_SAIDA-BELNR = ICON_OPERATION.
          MODIFY TL_SAIDA TRANSPORTING BELNR.
        ELSE.
          TL_SAIDA-OBJ_KEY = SPACE.
          TL_SAIDA-BELNR = SPACE.
          TL_SAIDA-LOG   = ICON_SYSTEM_CANCEL.
          MODIFY TL_SAIDA TRANSPORTING LOG BELNR OBJ_KEY.
        ENDIF.
      ENDIF.
    ELSE.
      TL_SAIDA-OBJ_KEY = SPACE.
      TL_SAIDA-BELNR   = SPACE.
      TL_SAIDA-LOG     = ICON_ENTER_MORE.
      MODIFY TL_SAIDA TRANSPORTING LOG OBJ_KEY BELNR.
    ENDIF.
    CLEAR: TL_SAIDA.
  ENDLOOP.

ENDFORM.                    " ATUALIZA_SAIDA


*&---------------------------------------------------------------------*
*&      Form  GERA_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GERA_DOCUMENTOS TABLES TL_ZIB     STRUCTURE ZIB_CONTABIL
                            TL_ZIB_CHV STRUCTURE ZIB_CONTABIL_CHV
                            TL_ZIB_ERR STRUCTURE ZIB_CONTABIL_ERR.

  DATA: WL_OBJ_KEY  TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
        WA_ZFIT0084 TYPE ZFIT0084,
        W_CONT      TYPE I.

  REFRESH TG_SAIDA_AUX.
  REFRESH TG_SAIDA_RES.
  W_CONT = 0.
  LOOP AT TG_SAIDA INTO WG_SAIDA WHERE MARK IS NOT INITIAL.
    ADD 1 TO W_CONT.
  ENDLOOP.
  IF W_CONT NE 1.
    MESSAGE TEXT-034 TYPE 'I'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'Z_CONTROLE_FECHAMES'
    EXPORTING
      I_BUKRS  = S_BUKRS-LOW
      I_DATA   = VG_LAST_DAY
    IMPORTING
      E_STATUS = E_STATUS
      E_MESSA  = E_MESSA
    EXCEPTIONS
      ERROR    = 1
      OTHERS   = 2.
  IF E_STATUS = 'E'.
    MESSAGE E398(00) WITH E_MESSA.
    EXIT.
  ENDIF.

  LOOP AT TG_SAIDA INTO WG_SAIDA WHERE MARK IS NOT INITIAL.
    IF WG_SAIDA-OBJ_KEY IS NOT INITIAL AND WG_SAIDA-BELNR IS INITIAL.
      READ TABLE TL_ZIB WITH KEY OBJ_KEY = WG_SAIDA-OBJ_KEY BINARY SEARCH.
      IF TL_ZIB-RG_ATUALIZADO EQ 'N'.
        CLEAR: WG_SAIDA_EXEC.
        WG_SAIDA_EXEC-ICON   = ICON_YELLOW_LIGHT.
        WG_SAIDA_EXEC-TIPO   = TEXT-M12. "'Variação'.
        WG_SAIDA_EXEC-LIFNR  = WG_SAIDA-KUNNR.
        WG_SAIDA_EXEC-NAME1  = WG_SAIDA-NAME1.
        WG_SAIDA_EXEC-HKONT  = WG_SAIDA-HKONT.
        WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
        WG_SAIDA_EXEC-MSG    = TEXT-030 .
        APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
        CONTINUE.
      ELSEIF TL_ZIB-RG_ATUALIZADO EQ 'S'.
        READ TABLE TL_ZIB_CHV WITH KEY OBJ_KEY = WG_SAIDA-OBJ_KEY BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          CLEAR: WG_SAIDA_EXEC.
          WG_SAIDA_EXEC-ICON   = ICON_GREEN_LIGHT.
          WG_SAIDA_EXEC-TIPO   = TEXT-M12. "'Variação'.
          WG_SAIDA_EXEC-LIFNR  = WG_SAIDA-KUNNR.
          WG_SAIDA_EXEC-NAME1  = WG_SAIDA-NAME1.
          WG_SAIDA_EXEC-HKONT  = WG_SAIDA-HKONT.
          WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
          WG_SAIDA_EXEC-MSG    = TEXT-031 .
          APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
          ACAO '&REFRESH'.
          CONTINUE.
        ELSE.
          READ TABLE TL_ZIB_ERR WITH KEY OBJ_KEY = WG_SAIDA-OBJ_KEY BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            CLEAR: WG_SAIDA_EXEC.
            WG_SAIDA_EXEC-ICON   = ICON_RED_LIGHT.
            WG_SAIDA_EXEC-TIPO   = TEXT-M12. "'Variação'.
            WG_SAIDA_EXEC-LIFNR  = WG_SAIDA-KUNNR.
            WG_SAIDA_EXEC-NAME1  = WG_SAIDA-NAME1.
            WG_SAIDA_EXEC-HKONT  = WG_SAIDA-HKONT.
            WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
            WG_SAIDA_EXEC-MSG    = TEXT-032 .
            APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
*               TEXT_QUESTION         = '“Contabilização com erro, reprocessa, sim ou não?'
                TEXT_QUESTION         = TEXT-035
                TEXT_BUTTON_1         = TEXT-036
                ICON_BUTTON_1         = 'ICON_OKAY'
                TEXT_BUTTON_2         = TEXT-037
                ICON_BUTTON_2         = 'ICON_CANCEL'
                DEFAULT_BUTTON        = '1'
                DISPLAY_CANCEL_BUTTON = ' '
                START_COLUMN          = 25
                START_ROW             = 6
              IMPORTING
                ANSWER                = W_ANSWER
              EXCEPTIONS
                TEXT_NOT_FOUND        = 1
                OTHERS                = 2.

            IF SY-SUBRC <> 0.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ENDIF.
            IF W_ANSWER = '1'.
              DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY = WG_SAIDA-OBJ_KEY.
              DELETE FROM ZIB_CONTABIL     WHERE OBJ_KEY = WG_SAIDA-OBJ_KEY.
              DELETE FROM ZFIT0084         WHERE OBJ_KEY = WG_SAIDA-OBJ_KEY.
            ELSE.
              ACAO '&REFRESH'.
              CONTINUE.
            ENDIF.
          ELSE.
            CLEAR: WG_SAIDA_EXEC.
            WG_SAIDA_EXEC-ICON   = ICON_YELLOW_LIGHT.
            WG_SAIDA_EXEC-TIPO   = TEXT-M12. "'Variação'.
            WG_SAIDA_EXEC-LIFNR  = WG_SAIDA-KUNNR.
            WG_SAIDA_EXEC-NAME1  = WG_SAIDA-NAME1.
            WG_SAIDA_EXEC-HKONT  = WG_SAIDA-HKONT.
            WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
            WG_SAIDA_EXEC-MSG    = TEXT-030 .
            APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF WG_SAIDA-OBJ_KEY IS INITIAL.
      SELECT SINGLE *
        FROM ZFIT0084
        INTO WA_ZFIT0084
        WHERE BUKRS     = S_BUKRS-LOW
        AND   MES_ANO   = S_MES-LOW
        AND   SAKNR     = WG_SAIDA-HKONT
        AND   BELNR     = WG_SAIDA-BELNR2
        AND   BUZEI     = WG_SAIDA-BUZEI
        AND   OBJ_KEY   NE ''.
      IF SY-SUBRC = 0.
        CONTINUE.
      ENDIF.

    ENDIF.

    IF WG_SAIDA-VLR_AJUST = 0.
      CLEAR: WG_SAIDA_EXEC.
      WG_SAIDA_EXEC-ICON   = ICON_RED_LIGHT.
      WG_SAIDA_EXEC-TIPO   = TEXT-M12. "'Variação'.
      WG_SAIDA_EXEC-LIFNR  = WG_SAIDA-KUNNR.
      WG_SAIDA_EXEC-NAME1  = WG_SAIDA-NAME1.
      WG_SAIDA_EXEC-HKONT  = WG_SAIDA-HKONT.
      WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
      WG_SAIDA_EXEC-MSG    = TEXT-M11.
      APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
      CONTINUE.
    ENDIF.

    IF WG_SAIDA-BELNR IS NOT INITIAL.
      CLEAR: WG_SAIDA_EXEC.
      WG_SAIDA_EXEC-ICON   = ICON_RED_LIGHT.
      WG_SAIDA_EXEC-TIPO   = TEXT-M12. "'Variação'.
      WG_SAIDA_EXEC-LIFNR  = WG_SAIDA-KUNNR.
      WG_SAIDA_EXEC-NAME1  = WG_SAIDA-NAME1.
      WG_SAIDA_EXEC-HKONT  = WG_SAIDA-HKONT.
      WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
      WG_SAIDA_EXEC-MSG    = TEXT-038 .
      APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
      CONTINUE.
    ENDIF.
    CLEAR WL_OBJ_KEY.
    PERFORM GERA_CONTABIL USING WG_SAIDA SPACE C_X CHANGING WL_OBJ_KEY.

  ENDLOOP.

ENDFORM.                    " GERA_DOCUMENTOS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_CHAVE
*&---------------------------------------------------------------------*
*       Busca Chave de Lançamento
*----------------------------------------------------------------------*
FORM BUSCA_CHAVE  USING    PFORN         TYPE CHAR1
                           WL_ESTORNO    TYPE CHAR1
                           WL_VARICAO    TYPE CHAR1
                           VLR_AJUST     LIKE BSIK-DMBTR
                           UMSKZ         LIKE BSIK-UMSKZ
                  CHANGING ZIB_BSCHL     LIKE BSIK-BSCHL.

  IF PFORN = 'X'.
    IF VLR_AJUST > 0 AND  UMSKZ IS INITIAL.  ZIB_BSCHL =  '21'. ENDIF.
    IF VLR_AJUST < 0 AND  UMSKZ IS INITIAL.  ZIB_BSCHL =  '31'. ENDIF.

    IF VLR_AJUST > 0 AND  UMSKZ IS NOT INITIAL.  ZIB_BSCHL =  '29'. ENDIF.
    IF VLR_AJUST < 0 AND  UMSKZ IS NOT INITIAL.  ZIB_BSCHL =  '39'. ENDIF.

    IF WL_ESTORNO IS NOT INITIAL.
      IF ZIB_BSCHL = '21'.
        ZIB_BSCHL = '31'.
      ELSEIF ZIB_BSCHL = '31'.
        ZIB_BSCHL = '21'.
      ENDIF.

      IF ZIB_BSCHL = '29'.
        ZIB_BSCHL = '39'.
      ELSEIF ZIB_BSCHL = '39'.
        ZIB_BSCHL = '29'.
      ENDIF.
    ENDIF.

  ELSE.
    IF VLR_AJUST > 0 AND  UMSKZ IS INITIAL.  ZIB_BSCHL =  '01'. ENDIF.
    IF VLR_AJUST < 0 AND  UMSKZ IS INITIAL.  ZIB_BSCHL =  '11'. ENDIF.

    IF VLR_AJUST > 0 AND  UMSKZ IS NOT INITIAL.  ZIB_BSCHL =  '09'. ENDIF.
    IF VLR_AJUST < 0 AND  UMSKZ IS NOT INITIAL.  ZIB_BSCHL =  '19'. ENDIF.

    IF WL_ESTORNO IS NOT INITIAL.
      IF ZIB_BSCHL = '01'.
        ZIB_BSCHL = '11'.
      ELSEIF ZIB_BSCHL = '11'.
        ZIB_BSCHL = '01'.
      ENDIF.

      IF ZIB_BSCHL = '09'.
        ZIB_BSCHL = '19'.
      ELSEIF ZIB_BSCHL = '19'.
        ZIB_BSCHL = '09'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " BUSCA_CHAVE

*&---------------------------------------------------------------------*
*&      Form  BUSCA_CHAVE2
*&---------------------------------------------------------------------*
*       Busca Chave de Lançamento
*----------------------------------------------------------------------*
FORM BUSCA_CHAVE2 USING    PFORN         TYPE CHAR1
                           WL_ESTORNO    TYPE CHAR1
                           WL_VARICAO    TYPE CHAR1
                           VLR_AJUST     LIKE BSIK-DMBTR
                           UMSKZ         LIKE BSIK-UMSKZ
                  CHANGING ZIB_BSCHL     LIKE BSIK-BSCHL.

  IF PFORN = 'X'.
    IF VLR_AJUST < 0 AND  UMSKZ IS INITIAL.  ZIB_BSCHL =  '21'. ENDIF.
    IF VLR_AJUST > 0 AND  UMSKZ IS INITIAL.  ZIB_BSCHL =  '31'. ENDIF.

    IF VLR_AJUST < 0 AND  UMSKZ IS NOT INITIAL.  ZIB_BSCHL =  '29'. ENDIF.
    IF VLR_AJUST > 0 AND  UMSKZ IS NOT INITIAL.  ZIB_BSCHL =  '39'. ENDIF.

    IF WL_ESTORNO IS NOT INITIAL.
      IF ZIB_BSCHL = '21'.
        ZIB_BSCHL = '31'.
      ELSEIF ZIB_BSCHL = '31'.
        ZIB_BSCHL = '21'.
      ENDIF.

      IF ZIB_BSCHL = '27'.
        ZIB_BSCHL = '34'.
      ELSEIF ZIB_BSCHL = '34'.
        ZIB_BSCHL = '27'.
      ENDIF.

      IF ZIB_BSCHL = '29'.
        ZIB_BSCHL = '39'.
      ELSEIF ZIB_BSCHL = '39'.
        ZIB_BSCHL = '29'.
      ENDIF.
    ENDIF.

  ELSE.
    IF VLR_AJUST < 0 AND  UMSKZ IS INITIAL.  ZIB_BSCHL =  '01'. ENDIF.
    IF VLR_AJUST > 0 AND  UMSKZ IS INITIAL.  ZIB_BSCHL =  '11'. ENDIF.

    IF VLR_AJUST < 0 AND  UMSKZ IS NOT INITIAL.  ZIB_BSCHL =  '09'. ENDIF.
    IF VLR_AJUST > 0 AND  UMSKZ IS NOT INITIAL.  ZIB_BSCHL =  '19'. ENDIF.

    IF WL_ESTORNO IS NOT INITIAL.
      IF ZIB_BSCHL = '01'.
        ZIB_BSCHL = '11'.
      ELSEIF ZIB_BSCHL = '11'.
        ZIB_BSCHL = '01'.
      ENDIF.

      IF ZIB_BSCHL = '09'.
        ZIB_BSCHL = '19'.
      ELSEIF ZIB_BSCHL = '19'.
        ZIB_BSCHL = '09'.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " BUSCA_CHAVE2

*&---------------------------------------------------------------------*
*&      Form  INVERTE_CHAVE
*&---------------------------------------------------------------------*
*       Inverter chave de Lançamento para Inverso de Variação Cambial
*----------------------------------------------------------------------*
*      <--ZIB  - Registro de Geração de Documentos
*----------------------------------------------------------------------*
FORM INVERTE_CHAVE USING WA_SAIDA TYPE TY_SAIDA_EXEC CHANGING ZIB TYPE ZIB_CONTABIL.

  DATA: V_ENTRADA TYPE BSCHL.

  WRITE VG_FIRST_DAY TO ZIB-BLDAT.
  WRITE VG_FIRST_DAY TO ZIB-BUDAT.
  ZIB-GJAHR = VG_FIRST_DAY(4).
  ZIB-MONAT = VG_FIRST_DAY+4(2).

  MOVE ZIB-BSCHL TO V_ENTRADA.

  CASE ZIB-BSCHL.
      "Conta Razão """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    WHEN '50'.
      ZIB-BSCHL = '40'.
    WHEN '40'.
      ZIB-BSCHL = '50'.
      "Fornecedor """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    WHEN '21'.
      ZIB-BSCHL = '31'.
    WHEN '31'.
      ZIB-BSCHL = '21'.
    WHEN '27'.
      ZIB-BSCHL = '34'.
    WHEN '34'.
      ZIB-BSCHL = '27'.
    WHEN '29'.
      ZIB-BSCHL = '39'.
    WHEN '39'.
      ZIB-BSCHL = '29'.
      "Cliente """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    WHEN '01'.
      ZIB-BSCHL = '11'.
    WHEN '11'.
      ZIB-BSCHL = '01'.
    WHEN '09'.
      ZIB-BSCHL = '19'.
    WHEN '19'.
      ZIB-BSCHL = '09'.
    WHEN: '06'.
      ZIB-BSCHL = '01'.
    WHEN: '15'.
      ZIB-BSCHL = '11'.
  ENDCASE.



  IF V_ENTRADA EQ ZIB-BSCHL.
    WA_SAIDA-ICON = ICON_RED_LIGHT.
    CONCATENATE TEXT-039 ZIB-BSCHL INTO WA_SAIDA-MSG SEPARATED BY SPACE.
  ENDIF.

ENDFORM.                    " INVERTE_CHAVE
*&---------------------------------------------------------------------*
*&      Form  REVERTE_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ESTORNA_DOCUMENTOS USING P_TIPO.

  DATA: W_CONT     TYPE I,
        VDATA(10),
        P_ERRO(1),
        V_ESTORNO  TYPE UF05A-STGRD,
        WA_ZIB_CHV TYPE ZIB_CONTABIL_CHV,
        V_MSG(60),
        LW_SAIDAR  TYPE TY_SAIDAR. "Rubenilson - 22.01.25 - BUG163439

  VG_RYEAR  = S_MES-LOW+2(4).
  CONCATENATE VG_RYEAR S_MES-LOW(2) '01' INTO VG_LAST_DAY_AUX.
  VG_LAST_DAY = VG_LAST_DAY_AUX.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      I_DATE = VG_LAST_DAY
    IMPORTING
      E_DATE = VG_LAST_DAY.

  IF P_TIPO = 'REV'.
    IF VG_LAST_DAY_AUX+0(6) = SY-DATUM+0(6).
      MESSAGE TEXT-041 TYPE 'I'.
      EXIT.
    ENDIF.
    ADD 1 TO VG_LAST_DAY.
    V_ESTORNO = '02'.
  ELSE.
    V_ESTORNO = '01'.
  ENDIF.

  CALL FUNCTION 'Z_CONTROLE_FECHAMES'
    EXPORTING
      I_BUKRS  = S_BUKRS-LOW
      I_DATA   = VG_LAST_DAY
    IMPORTING
      E_STATUS = E_STATUS
      E_MESSA  = E_MESSA
    EXCEPTIONS
      ERROR    = 1
      OTHERS   = 2.
  IF E_STATUS = 'E'.
    MESSAGE E398(00) WITH E_MESSA.
    EXIT.
  ENDIF.

  IF P_TIPO = 'REV'.
    V_MSG = TEXT-045.
  ELSE.
    V_MSG = TEXT-042.
  ENDIF.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TEXT_QUESTION         = V_MSG
      TEXT_BUTTON_1         = TEXT-036
      ICON_BUTTON_1         = 'ICON_OKAY'
      TEXT_BUTTON_2         = TEXT-037
      ICON_BUTTON_2         = 'ICON_CANCEL'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ' '
      START_COLUMN          = 25
      START_ROW             = 6
    IMPORTING
      ANSWER                = W_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF W_ANSWER NE '1'.
    EXIT.
  ENDIF.

  IF P_RAZ IS NOT INITIAL.
    REFRESH TG_SAIDA.
    LOOP AT TG_SAIDAR INTO WG_SAIDAR WHERE MARK IS NOT INITIAL.

      MOVE-CORRESPONDING WG_SAIDAR TO WG_SAIDA.

      WG_SAIDA-HKONT = WG_SAIDAR-RACCT.
      WG_SAIDA-VBUND = WG_SAIDAR-RASSC.
      APPEND WG_SAIDA TO TG_SAIDA.
    ENDLOOP.
  ENDIF.


  LOOP AT TG_SAIDA INTO WG_SAIDA WHERE MARK IS NOT INITIAL.
    IF WG_SAIDA-BELNR IS INITIAL.
      CLEAR: WG_SAIDA_EXEC.
      WG_SAIDA_EXEC-ICON   = ICON_YELLOW_LIGHT.
      WG_SAIDA_EXEC-TIPO   = TEXT-M12. "'Variação'.
      WG_SAIDA_EXEC-LIFNR  = WG_SAIDA-KUNNR.
      WG_SAIDA_EXEC-NAME1  = WG_SAIDA-NAME1.
      WG_SAIDA_EXEC-HKONT  = WG_SAIDA-HKONT.
      WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
      WG_SAIDA_EXEC-MSG    = TEXT-030 .
      APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
    ELSE.
      SELECT SINGLE *
        FROM ZIB_CONTABIL_CHV
        INTO  WA_ZIB_CHV
         WHERE OBJ_KEY EQ WG_SAIDA-OBJ_KEY.
      "FB08
      SELECT SINGLE   BUKRS BELNR  GJAHR  BUDAT STBLG STJAH
        FROM BKPF
        INTO WG_BKPF_FB08
        WHERE BUKRS EQ WA_ZIB_CHV-BUKRS
        AND   BELNR EQ WA_ZIB_CHV-BELNR
        AND   GJAHR EQ WA_ZIB_CHV-GJAHR
        AND   STBLG NE ''.
      IF SY-SUBRC = 0.
        CLEAR: WG_SAIDA_EXEC.
        WG_SAIDA_EXEC-ICON   = ICON_YELLOW_LIGHT.
        WG_SAIDA_EXEC-TIPO   = TEXT-M12. "'Variação'.
        WG_SAIDA_EXEC-LIFNR  = WG_SAIDA-KUNNR.
        WG_SAIDA_EXEC-NAME1  = WG_SAIDA-NAME1.
        WG_SAIDA_EXEC-HKONT  = WG_SAIDA-HKONT.
        WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
        WG_SAIDA_EXEC-MSG    = TEXT-044 .
        APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
      ENDIF.
    ENDIF.

  ENDLOOP.

  "
  IF TG_SAIDA_EXEC[] IS NOT INITIAL.
    EXIT.
  ENDIF.

  LOOP AT TG_SAIDA INTO WG_SAIDA WHERE MARK IS NOT INITIAL.
    SELECT SINGLE *
        FROM ZIB_CONTABIL_CHV
        INTO  WA_ZIB_CHV
         WHERE OBJ_KEY EQ WG_SAIDA-OBJ_KEY.
    CONCATENATE  VG_LAST_DAY+6(2) '.' VG_LAST_DAY+4(2) '.' VG_LAST_DAY+0(4) INTO VDATA.

    REFRESH TI_BDCDATA.
    PERFORM F_BDC_DATA USING:
          'SAPMF05A'  '0105'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'	      '/00',
          ''          ''      ''   'RF05A-BELNS'      WA_ZIB_CHV-BELNR,
          ''          ''      ''   'BKPF-BUKRS'       WA_ZIB_CHV-BUKRS,
          ''          ''      ''   'RF05A-GJAHS'      WA_ZIB_CHV-GJAHR,
          ''          ''      ''   'UF05A-STGRD'      V_ESTORNO,
          ''          ''      ''   'BSIS-BUDAT'       VDATA,
          'SAPMF05A'  '0105'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'	      '=BU'.

    CLEAR P_ERRO.
    VOBJ_KEY = WG_SAIDA-OBJ_KEY.
    PERFORM ZF_CALL_TRANSACTION USING 'FB08' CHANGING P_ERRO.

    IF P_ERRO IS NOT INITIAL.
      MESSAGE TEXT-043 TYPE 'I'.
    ELSEIF P_RAZ IS NOT INITIAL AND P_TIPO = 'EST'.
      DELETE FROM ZFIT0082
      WHERE BUKRS   = S_BUKRS-LOW
      AND   MES_ANO = S_MES-LOW
      AND   SAKNR   = WG_SAIDA-HKONT.
*      AND   vbund   = wg_saida-vbund. "Rubenilson - 22.01.25 - BUG163439

      MODIFY TG_SAIDAR FROM LW_SAIDAR TRANSPORTING OBJ_KEY WHERE RACCT = WG_SAIDA-HKONT."Rubenilson - 22.01.25 - BUG163439

    ELSEIF P_TIPO = 'EST'.
      DELETE FROM ZFIT0084
      WHERE OBJ_KEY    = WG_SAIDA-OBJ_KEY.
      COMMIT WORK.
      EXIT.
    ENDIF.

  ENDLOOP.
ENDFORM.

FORM F_BDC_DATA  USING P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR WA_BDCDATA.
  WA_BDCDATA-PROGRAM   = P_PROGRAM.
  WA_BDCDATA-DYNPRO    = P_DYNPRO.
  WA_BDCDATA-DYNBEGIN  = P_START.
  WA_BDCDATA-FNAM      = P_FNAM.
  WA_BDCDATA-FVAL      = P_FVAL.
  APPEND WA_BDCDATA TO TI_BDCDATA.

ENDFORM.                    " F_BDC_DATA

FORM ZF_CALL_TRANSACTION USING P_TRANS CHANGING P_ERRO.
  CONSTANTS: C_MSGID LIKE IT_MSG-MSGID VALUE 'F5',
             C_MSGNR LIKE IT_MSG-MSGNR VALUE '312',
             C_MSGNE LIKE IT_MSG-MSGNR VALUE '539'.

  DATA: WL_CONT     TYPE SY-TABIX.

  REFRESH: IT_MSG, TG_ZIB_ERR.
  CLEAR TG_ZIB_ERR.

  WL_MODE = 'E'.

  CALL TRANSACTION P_TRANS USING TI_BDCDATA
        MODE WL_MODE
        MESSAGES INTO IT_MSG.
  CLEAR: WL_CONT.

  LOOP AT IT_MSG WHERE MSGTYP EQ 'E'.
    ADD 1 TO WL_CONT.
  ENDLOOP.
  IF WL_CONT  GT 0.
    CLEAR WL_CONT.
    DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY  = VOBJ_KEY.
    LOOP AT IT_MSG WHERE MSGTYP EQ 'E'.
      ADD 1 TO WL_CONT.
      CLEAR: WL_MESSAGE.
      CALL FUNCTION 'CUTC_GET_MESSAGE'
        EXPORTING
          MSG_TYPE       = IT_MSG-MSGTYP
          MSG_ID         = IT_MSG-MSGID
          MSG_NO         = SY-MSGNO
          MSG_ARG1       = SY-MSGV1
          MSG_ARG2       = SY-MSGV2
          MSG_ARG3       = SY-MSGV3
          MSG_ARG4       = SY-MSGV4
        IMPORTING
          RAW_MESSAGE    = WL_MESSAGE
        EXCEPTIONS
          MSG_NOT_FOUND  = 1
          INTERNAL_ERROR = 2
          OTHERS         = 3.

      IF ( SY-SUBRC NE 0 ).
        WL_MESSAGE = 'Erro na mensagem do BATCH-INPUT'.
      ENDIF.

      WG_ZIB_ERR-OBJ_KEY            = VOBJ_KEY.
      WG_ZIB_ERR-NR_ITEM            = WL_CONT.
      WG_ZIB_ERR-INTERFACE          = ''.
      WG_ZIB_ERR-DT_ATUALIZACAO     = SY-DATUM.
      WG_ZIB_ERR-HR_ATUALIZACAO     = SY-UZEIT.
      WG_ZIB_ERR-TYPE               = IT_MSG-MSGTYP.
      WG_ZIB_ERR-ID                 = IT_MSG-MSGID.
      WG_ZIB_ERR-NUM                = SY-MSGNO.
      WG_ZIB_ERR-MESSAGE            = WL_MESSAGE.
      WG_ZIB_ERR-MESSAGE_V1         = IT_MSG-MSGV1.
      WG_ZIB_ERR-MESSAGE_V2         = IT_MSG-MSGV2.
      WG_ZIB_ERR-MESSAGE_V3         = IT_MSG-MSGV3.
      WG_ZIB_ERR-MESSAGE_V4         = IT_MSG-MSGV4.

      APPEND WG_ZIB_ERR TO TG_ZIB_ERR.
      CLEAR WG_ZIB_ERR.

    ENDLOOP.

    MODIFY ZIB_CONTABIL_ERR FROM TABLE TG_ZIB_ERR.
  ENDIF.

  READ TABLE IT_MSG WITH KEY MSGTYP = 'A'.
  IF SY-SUBRC = 0.
    P_ERRO = 'X'.
  ELSE.
    READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      P_ERRO = 'X'.
    ENDIF.
  ENDIF.

  CLEAR WG_DOCUMENTO.

  READ TABLE IT_MSG WITH KEY MSGID = C_MSGID
                             MSGNR = C_MSGNR
                             MSGTYP = 'S'.

  IF SY-SUBRC = 0.
    MOVE IT_MSG-MSGV1 TO WG_DOCUMENTO.
  ENDIF.

  IF  WG_DOCUMENTO IS INITIAL.
    P_ERRO = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_DOCUMENTO
      IMPORTING
        OUTPUT = WG_DOCUMENTO.
  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION
