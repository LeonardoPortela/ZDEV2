************************************************************************
* TÍTULO     : Avaliação de Moeda Estrangeira                          *
* TIPO PROG  : REPORT - ZGL027                                         *
* FUNCIONAL  : Cleandra Piovesan                                       *
* AUTOR(A)   : Marcos Faneli                                           *
* DATA       : 03.03.2015                                              *
* TRANSAÇÃO  : ZGL042                                                  *
* DESCRIÇÃO  : Avaliação de Moeda Estrangeira                          *
************************************************************************
REPORT  ZGL027 MESSAGE-ID Z01 NO STANDARD PAGE HEADING.
INCLUDE <ICON>.

*CONTROLS MY  TABSTRIP TYPE TABSTRIP.
CONSTANTS: "C_USD  TYPE TCURR_CURR VALUE 'USD',
           C_0050 TYPE KTOPL      VALUE '0050'.

DATA: C_USD        TYPE TCURR_CURR VALUE 'USD',
      OK_CODE      TYPE SY-UCOMM,
      SAVE_OK      TYPE SY-UCOMM,
      WG_WAERS     TYPE C LENGTH 3 VALUE 'USD',
      WG_INDICADOR TYPE C,
      VSEQITEM     TYPE ZIB_CONTABIL-SEQITEM,
      VNUM(10)     TYPE C,
      VSEQ(10)     TYPE P,
      VOBJ_KEY     TYPE ZGL012_AVM-OBJ_KEY,
      VG_LAST_DAY  TYPE SY-DATUM.

DATA  NUMBER TYPE SY-DYNNR.
*TABELAS STANDARD
TABLES: BSIK,     "Cont.: índice secund. para fornecedores
        BSID,     "Cont.: índice secund. para clientes
        BSIS.     "Cont. financ.: índice secund. p/contas do Razão

TYPES: BEGIN OF TY_ZIB_CONTABIL.
         INCLUDE STRUCTURE ZIB_CONTABIL.
TYPES:   MARK TYPE C,
       END OF TY_ZIB_CONTABIL.

TYPES: BEGIN OF TY_ZIB_CONTABIL_ERR.
         INCLUDE STRUCTURE ZIB_CONTABIL_ERR.
TYPES:   MARK TYPE C,
       END OF TY_ZIB_CONTABIL_ERR.

TYPES: BEGIN OF TY_ZIB_CONTABIL_CHV.
         INCLUDE STRUCTURE ZIB_CONTABIL_CHV.
TYPES:   BLDAT TYPE ZIB_CONTABIL-BLDAT,
       END OF TY_ZIB_CONTABIL_CHV.

*TIPOS DE REFERÊNCIA
TYPES: BEGIN OF TY_BSIK,
         BUKRS     TYPE BSIK-BUKRS,
         LIFNR     TYPE BSIK-LIFNR,
         UMSKZ     TYPE BSIK-UMSKZ,
         BELNR     TYPE BSIK-BELNR,
         BUZEI     TYPE BSIK-BUZEI,
         BUDAT     TYPE BSIK-BUDAT,
         WAERS     TYPE BSIK-WAERS,
         BLART     TYPE BLART,
         BSCHL     TYPE BSCHL,
         SHKZG     TYPE SHKZG,
         GSBER     TYPE GSBER,
         DMBTR     TYPE BSIK-DMBTR,
         WRBTR     TYPE BSIK-WRBTR,
         HKONT     TYPE BSIK-HKONT,
         DMBE2     TYPE BSIK-DMBE2,
         GJAHR     TYPE BSIK-GJAHR,
         AUGBL     TYPE BSIK-AUGBL,
         AUGDT     TYPE BSIK-AUGDT,
         ST_REV(1),
       END OF TY_BSIK,

       BEGIN OF TY_BSID,
         BUKRS     TYPE BSID-BUKRS,
         KUNNR     TYPE BSID-KUNNR,
         UMSKZ     TYPE BSID-UMSKZ,
         BELNR     TYPE BSID-BELNR,
         BUZEI     TYPE BSID-BUZEI,
         BUDAT     TYPE BSID-BUDAT,
         WAERS     TYPE BSID-WAERS,
         BLART     TYPE BLART,
         BSCHL     TYPE BSCHL,
         SHKZG     TYPE SHKZG,
         GSBER     TYPE GSBER,
         DMBTR     TYPE BSID-DMBTR,
         WRBTR     TYPE BSID-WRBTR,
         HKONT     TYPE BSID-HKONT,
         DMBE2     TYPE BSID-DMBE2,
         GJAHR     TYPE BSID-GJAHR,
         AUGBL     TYPE BSID-AUGBL,
         AUGDT     TYPE BSID-AUGDT,
         ST_REV(1),
       END OF TY_BSID,

       BEGIN OF TY_BSIS,
         BUKRS     TYPE BSIS-BUKRS,
         HKONT     TYPE BSIS-HKONT,
         BELNR     TYPE BSIS-BELNR,
         BUZEI     TYPE BSIS-BUZEI,
         BUDAT     TYPE BSIS-BUDAT,
         WAERS     TYPE BSIS-WAERS,
         BLART     TYPE BLART,
         BSCHL     TYPE BSCHL,
         SHKZG     TYPE SHKZG,
         GSBER     TYPE GSBER,
         DMBTR     TYPE BSIS-DMBTR,
         WRBTR     TYPE BSIS-WRBTR,
         DMBE2     TYPE BSIS-DMBE2,
         GJAHR     TYPE BSIS-GJAHR,
         AUGBL     TYPE BSIS-AUGBL,
         AUGDT     TYPE BSIS-AUGDT,
         ST_REV(1),
       END OF TY_BSIS,

       BEGIN OF TY_BKPF,
         BUKRS TYPE BKPF-BUKRS,
         BELNR TYPE BKPF-BELNR,
         GJAHR TYPE BKPF-GJAHR,
         BUDAT TYPE BKPF-BUDAT,
         STBLG TYPE BKPF-STBLG,
         STJAH TYPE BKPF-STJAH,
       END OF TY_BKPF,

       BEGIN OF TY_T030H,
         KTOPL TYPE T030H-KTOPL,
         HKONT TYPE T030H-HKONT,
         WAEES TYPE T030H-WAERS,
         CURTP TYPE T030H-CURTP,
         LSBEW TYPE T030H-LSBEW,
         LHBEW TYPE T030H-LHBEW,
       END OF TY_T030H,

       BEGIN OF TY_TCURR,
         KURST TYPE TCURR-KURST,
         FCURR TYPE TCURR-FCURR,
         TCURR TYPE TCURR-TCURR,
         GDATU TYPE TCURR-GDATU,
         UKURS TYPE TCURR-UKURS,
       END OF TY_TCURR,

       BEGIN OF TY_T001B,
         FRPE1 TYPE T001B-FRPE1,
         FRYE1 TYPE T001B-FRYE1,
       END OF   TY_T001B,


       BEGIN OF TY_T001,
         BUKRS TYPE T001-BUKRS,
         LAND1 TYPE T001-LAND1,
       END OF   TY_T001,

       BEGIN OF TY_T005,
         LAND1 TYPE T005-LAND1,
         WAERS TYPE T005-WAERS,
         CURIN TYPE T005-CURIN,
         CURHA TYPE T005-CURHA,
       END OF   TY_T005,

       BEGIN OF TY_SKB1,
         BUKRS TYPE SKB1-BUKRS,
         SAKNR TYPE SKB1-SAKNR,
         XOPVW TYPE SKB1-XOPVW,
       END OF  TY_SKB1.


DATA: BEGIN OF T_ZGL012_AVM OCCURS 0.
        INCLUDE STRUCTURE ZGL012_AVM.
DATA: DELE(1).
DATA: END OF   T_ZGL012_AVM.

DATA: BEGIN OF T_ZGL012_ATUAL OCCURS 0.
        INCLUDE STRUCTURE ZGL012_AVM.
DATA: DELE(1).
DATA: END OF   T_ZGL012_ATUAL.

DATA: BEGIN OF T_ZGL012_AUX OCCURS 0.
        INCLUDE STRUCTURE ZGL012_AVM.
DATA: DELE(1).
DATA: END OF   T_ZGL012_AUX.
*

*DECLARAÇÃO DE TABELA INTERNA
DATA: T_BSIK              TYPE TABLE OF TY_BSIK,
      IT_BKPF             TYPE TABLE OF BKPF WITH HEADER LINE,
      IT_BKPF_EST         TYPE TABLE OF BKPF WITH HEADER LINE,

      T_BSID              TYPE TABLE OF TY_BSID,
      T_BSIS              TYPE TABLE OF TY_BSIS,
      T_T030H             TYPE TABLE OF TY_T030H,
      T_TCURR             TYPE TABLE OF TY_TCURR,
      T_TCURR_A           TYPE TABLE OF TY_TCURR,
      T_TCURR_G           TYPE TABLE OF TY_TCURR,
      T_T001B             TYPE TABLE OF TY_T001B,
      T_T001              TYPE TABLE OF TY_T001,
      T_T005              TYPE TABLE OF TY_T005,
      T_SKB1              TYPE TABLE OF TY_SKB1,
      T_0025              TYPE TABLE OF ZFIT0025,
      TG_T882G            TYPE TABLE OF T882G,
      IT_ZIB_CONTABIL     TYPE TABLE OF TY_ZIB_CONTABIL,
      IT_ZIB_CONTABIL_ERR TYPE TABLE OF TY_ZIB_CONTABIL_ERR,
      IT_ZIB_CONTABIL_CHV TYPE TABLE OF TY_ZIB_CONTABIL_CHV.

*DECLARAÇÃO DE WORK AREA
DATA: WA_BSIK             TYPE TY_BSIK,
      WA_ZGL012_AVM       LIKE T_ZGL012_AVM,
      WA_ZGL012_2         LIKE T_ZGL012_AVM,
      WA_ZGL012_AUX       LIKE T_ZGL012_AVM,
      WA_BSID             TYPE TY_BSID,
      WA_BSIS             TYPE TY_BSIS,
      WA_T030H            TYPE TY_T030H,
      WA_TCURR            TYPE TY_TCURR,
      WA_T001B            TYPE TY_T001B,
      WA_T001             TYPE TY_T001,
      WA_T005             TYPE TY_T005,
      WA_SKB1             TYPE TY_SKB1,
      WA_0025             TYPE ZFIT0025,
      WG_T882G            TYPE T882G,
      WA_ZIB_CONTABIL     TYPE TY_ZIB_CONTABIL,
      WA_ZIB_CONTABIL_ERR TYPE TY_ZIB_CONTABIL_ERR,
      WA_ZIB_CONTABIL_CHV TYPE TY_ZIB_CONTABIL_CHV,
      WG_BKPF_FB08        TYPE TY_BKPF,
      WG_BKPF_FB08_E      TYPE TY_BKPF.


*DECLARAÇÃO DE VARIÁVEIS DE TAXA DE CÂMBIO
DATA: XTX_USD      TYPE TCURR-UKURS,
      XTX_USD_AUX  TYPE TCURR-UKURS,
      XTX_USD_A    TYPE TCURR-UKURS,
      WG_DMBT2     TYPE BSIK-DMBT2 VALUE '0.01',
      WG_CUR_P     TYPE WAERS, " Variável que armazena a moeda principal
      WL_ST_REV(1),
      WG_ACAO(30).

DATA: IT_BDCDATA TYPE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF O_ALV2 OCCURS 0, "TABELA DE SAIDA DO ALV
        MARK,
        RACCT       TYPE FAGLFLEXT-RACCT,
        RASSC       TYPE FAGLFLEXT-RASSC,
        TXT50       TYPE SKAT-TXT50,
        WAERS       TYPE ZGL012_AVM-WAERS,
        KTOKS       TYPE SKA1-KTOKS,
        MOEDA_ATU   TYPE Z_CHAR12,
        WRBTR       TYPE ZGL012_AVM-WRBTR,
        CURR1       TYPE FAGLFLEXT-HSLVT,
        CURR2       TYPE FAGLFLEXT-KSLVT,
        CURR3       TYPE FAGLFLEXT-OSLVT,
        TX_USD      TYPE ZFIT0082-TX_USD,
        TX_BRL      TYPE ZFIT0082-TX_BRL,
        SALDO_CORR  TYPE FAGLFLEXT-HSLVT,
        SALDO_CORR2 TYPE FAGLFLEXT-HSLVT,
        VLR_AJUST   TYPE FAGLFLEXT-KSLVT,
        VLR_AJUST2  TYPE FAGLFLEXT-KSLVT,
        BELNR       TYPE ZIB_CONTABIL_CHV-BELNR,
        BELNR_EST   TYPE ZIB_CONTABIL_CHV-BELNR,
        OBJ_KEY     TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
        OBJ_KEY_EST TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
        LOG(4),
      END OF O_ALV2.


DATA : E_STATUS(1),
       E_MESSA(64).

DATA: BEGIN OF O_ALV OCCURS 0, "TABELA DE SAIDA DO ALV
        MARK(1),
        MANDT            TYPE ZGL012_AVM-MANDT,
        BUKRS            TYPE ZGL012_AVM-BUKRS,
        DT_AVAL          TYPE ZGL012_AVM-DT_AVAL,
        KUNNR            TYPE ZGL012_AVM-KUNNR,
        BELNR            TYPE ZGL012_AVM-BELNR,
        BUZEI            TYPE ZGL012_AVM-BUZEI,
        MOEDA_ATU        TYPE Z_CHAR12,
        BUDAT            TYPE ZGL012_AVM-BUDAT,
        WAERS            TYPE ZGL012_AVM-WAERS,
        GSBER            TYPE ZGL012_AVM-GSBER,
        DMBTR            TYPE ZGL012_AVM-DMBTR,
        DMBE2            TYPE ZGL012_AVM-DMBE2,
        WRBTR            TYPE ZGL012_AVM-WRBTR,
        KURSF            TYPE ZGL012_AVM-KURSF,
        VLR_ATUALIZADO   TYPE ZGL012_AVM-VLR_ATUALIZADO,
        VLR_ACUM_MES_ATU TYPE ZGL012_AVM-VLR_ACUM_MES_ATU,
        VLR_ACUM_MES_ANT TYPE ZGL012_AVM-VLR_ACUM_MES_ANT,
        VLR_VARIACAO     TYPE ZGL012_AVM-VLR_VARIACAO,
        RESULTADO        TYPE ZGL012_AVM-RESULTADO,
        TX_FECH          TYPE ZGL012_AVM-TX_FECH,
        HKONT            TYPE ZGL012_AVM-HKONT,
        UMSKZ            TYPE ZGL012_AVM-UMSKZ,
        LIFNR            TYPE ZGL012_AVM-LIFNR,
        DT_LCTO          TYPE ZGL012_AVM-DT_LCTO,
        DOC_LCTO         TYPE ZGL012_AVM-DOC_LCTO,
        CPUDT            TYPE ZGL012_AVM-CPUDT,
        CPUTM            TYPE ZGL012_AVM-CPUTM,
        USNAM            TYPE ZGL012_AVM-USNAM,
        AUGDT            TYPE ZGL012_AVM-AUGDT,
        BSCHL            TYPE ZGL012_AVM-BSCHL,
        AUGBL            TYPE ZGL012_AVM-AUGBL,
        OBJ_KEY          TYPE ZGL012_AVM-OBJ_KEY,
        ST_REV           TYPE ZGL012_AVM-ST_REV,
        DOC_REV          TYPE ZGL012_AVM-DOC_REV,
        DT_REV           TYPE ZGL012_AVM-DT_REV,
        STATUS(4),
        TIPO(10),
        CODIGO(10),
        DESCRICAO(50),
        RESULTADO_C(7),
        ESTORNO(1),
        DELE(1),
      END OF O_ALV.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: P_BUKRS TYPE ZGL012_AVM-BUKRS OBLIGATORY,
              P_BUDAT TYPE ZGL012_AVM-BUDAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

"100
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: P_C_LANC RADIOBUTTON GROUP R1 DEFAULT 'X'."AS CHECKBOX.
    SELECTION-SCREEN COMMENT 4(20) TEXT-C02 FOR FIELD P_C_LANC. "P_EX_TES.
    SELECTION-SCREEN POSITION 24.
    PARAMETERS: P_E_LANC RADIOBUTTON GROUP R1."AS CHECKBOX.
    SELECTION-SCREEN COMMENT 26(20) TEXT-C03 FOR FIELD P_E_LANC. "P_C_LANC.
    SELECTION-SCREEN POSITION 50.
    PARAMETERS: P_V_LANC RADIOBUTTON GROUP R1."AS CHECKBOX.
    SELECTION-SCREEN COMMENT 65(20) TEXT-C06 FOR FIELD P_V_LANC. "P_V_LANC.
    SELECTION-SCREEN POSITION 54.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS: P_BUDAT2 TYPE ZGL012_AVM-BUDAT,
              P_AUGDT  TYPE ZGL012_AVM-AUGDT,
              P_SPMON  TYPE SPMON,
              P_BLART  TYPE BSAS-BLART DEFAULT TEXT-004,
              P_SGTXT  TYPE BSIK-SGTXT.

SELECTION-SCREEN END OF BLOCK B2.
"200
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
  PARAMETERS: P_HKONT RADIOBUTTON GROUP RAD1 DEFAULT 'X' .
  SELECT-OPTIONS: S_HKONT FOR BSIK-HKONT.
  PARAMETERS: P_LIFNR RADIOBUTTON GROUP RAD1.
  SELECT-OPTIONS: S_LIFNR FOR BSIK-LIFNR.
  SELECT-OPTIONS: S_KONT  FOR BSIK-HKONT.
  PARAMETERS: P_KUNNR RADIOBUTTON GROUP RAD1.
  SELECT-OPTIONS: S_KUNNR FOR BSID-KUNNR.
  SELECT-OPTIONS: S_DONT  FOR BSID-HKONT.
  SELECT-OPTIONS: S_BELNR FOR BSIK-BELNR MODIF ID SC1.
  PARAMETERS: P_HKONT2 RADIOBUTTON GROUP RAD1.
  SELECT-OPTIONS: S_HKONT2 FOR BSIK-HKONT.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-001.
  PARAMETERS: P_VARI LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK B4.


**********Declarações ALV*****************************
*Pool de declarações do ALV.
TYPE-POOLS: KKBLO,
            SLIS,
            SHLP,
            VRM.

*Estrutura
DATA: ST_SELFIELD TYPE KKBLO_SELFIELD.

************************************************************************
* C O N T R O L E   A L V
************************************************************************
DATA: EDITCONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      EDITOR           TYPE REF TO CL_GUI_TEXTEDIT,
      CL_CONTAINER_95  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      CL_CONTAINER_05  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID    TYPE REF TO CL_DD_DOCUMENT,
      CL_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      WA_AFIELD        TYPE LVC_S_FCAT,
      IT_FIELDCAT      TYPE LVC_T_FCAT,
      I_SORT           TYPE LVC_T_SORT,
      WA_LAYOUT        TYPE LVC_S_LAYO,
      IS_STABLE        TYPE LVC_S_STBL VALUE 'XX',
      WG_REPNAME       LIKE SY-REPID,
      WG_X_VARIANT     LIKE DISVARIANT,
      WG_EXIT(1)       TYPE C,
      WG_SAVE(1)       TYPE C,
      WG_DOCUMENTO(10),
      WG_VARIANT       LIKE DISVARIANT.

DATA: BEGIN OF SRC OCCURS 5,
        LINE(100),
      END OF SRC.

*Tabelas internas do ALV.
DATA: IT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
      IT_HEADER TYPE KKBLO_T_LISTHEADER,
      IT_SORT   TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.


************************************************************************
* D E F I N I T I O N
************************************************************************
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      CATCH_HOTSPOT
        FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING
          E_ROW_ID
          E_COLUMN_ID
          ES_ROW_NO.

    METHODS:
      HANDLE_TOP_OF_PAGE
        FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
        IMPORTING
          E_DYNDOC_ID
          TABLE_INDEX.

ENDCLASS.                    "lcl_event_receiver DEFINITION

************************************************************************
* I M P L E M E N T A T I O N
************************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD CATCH_HOTSPOT.
    DATA: WL_ANO(4).

    TYPES: BEGIN OF TY_ITAB ,
             NAME(80) TYPE C,
           END OF TY_ITAB.

    DATA: MSG_ALV  TYPE CHAR80,
          ITAB_MSG TYPE TABLE OF TY_ITAB,
          WTAB_MSG TYPE  TY_ITAB.

    IF P_HKONT2 = 'X'.
      READ TABLE O_ALV2 INTO O_ALV2 INDEX E_ROW_ID-INDEX.
      IF SY-SUBRC = 0.
        IF E_COLUMN_ID = 'BELNR' AND O_ALV2-BELNR IS NOT INITIAL.
          WL_ANO = P_BUDAT(4).
          SET PARAMETER ID 'BLN' FIELD O_ALV2-BELNR.
          SET PARAMETER ID 'BUK' FIELD P_BUKRS.
          SET PARAMETER ID 'GJR' FIELD P_BUDAT(4).
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ELSEIF E_COLUMN_ID = 'BELNR_EST' AND O_ALV2-BELNR_EST IS NOT INITIAL.
          WL_ANO = P_BUDAT(4).
          SET PARAMETER ID 'BLN' FIELD O_ALV2-BELNR_EST.
          SET PARAMETER ID 'BUK' FIELD P_BUKRS.
          SET PARAMETER ID 'GJR' FIELD P_BUDAT(4).
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ELSEIF O_ALV2-LOG = ICON_INCOMPLETE.

          SELECT *
             FROM ZIB_CONTABIL_ERR
             INTO TABLE IT_ZIB_CONTABIL_ERR
             WHERE OBJ_KEY  = O_ALV2-OBJ_KEY.

          WTAB_MSG-NAME    = '------------------------MENSAGEM ERRO---------------------------------------'.
          APPEND WTAB_MSG TO ITAB_MSG .
          CLEAR WTAB_MSG.
          LOOP AT IT_ZIB_CONTABIL_ERR INTO WA_ZIB_CONTABIL_ERR.
            WTAB_MSG-NAME = WA_ZIB_CONTABIL_ERR-MESSAGE.
            APPEND WTAB_MSG TO ITAB_MSG .
            CLEAR WTAB_MSG.
          ENDLOOP.

          CONCATENATE 'DOCUMENTO ' O_ALV-BELNR INTO MSG_ALV SEPARATED BY SPACE.
          CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
            EXPORTING
              ENDPOS_COL   = 140
              ENDPOS_ROW   = 20
              STARTPOS_COL = 60
              STARTPOS_ROW = 15
              TITLETEXT    = MSG_ALV
            TABLES
              VALUETAB     = ITAB_MSG
            EXCEPTIONS
              BREAK_OFF    = 1
              OTHERS       = 2.
        ENDIF.
      ENDIF  .
    ELSE.
      READ TABLE O_ALV INTO O_ALV INDEX E_ROW_ID-INDEX.
      IF SY-SUBRC = 0.
        IF E_COLUMN_ID = 'BELNR'.
          WL_ANO = P_BUDAT(4).
          SET PARAMETER ID 'BLN' FIELD O_ALV-BELNR.
          SET PARAMETER ID 'BUK' FIELD P_BUKRS.
          SET PARAMETER ID 'GJR' FIELD O_ALV-BUDAT(4).
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ELSEIF E_COLUMN_ID = 'DOC_LCTO' AND O_ALV-DOC_LCTO IS NOT INITIAL.
          SELECT SINGLE  ZIB_CONTABIL_CHV~MANDT
                ZIB_CONTABIL_CHV~OBJ_KEY
                ZIB_CONTABIL_CHV~BELNR
                ZIB_CONTABIL_CHV~BUKRS
                ZIB_CONTABIL_CHV~GJAHR
           FROM ZIB_CONTABIL_CHV
           INTO WA_ZIB_CONTABIL_CHV
           WHERE ZIB_CONTABIL_CHV~OBJ_KEY = O_ALV-OBJ_KEY.
          SET PARAMETER ID 'BLN' FIELD WA_ZIB_CONTABIL_CHV-BELNR.
          SET PARAMETER ID 'BUK' FIELD WA_ZIB_CONTABIL_CHV-BUKRS.
          SET PARAMETER ID 'GJR' FIELD WA_ZIB_CONTABIL_CHV-GJAHR.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ELSEIF O_ALV-STATUS = ICON_INCOMPLETE.
          SELECT *
             FROM ZIB_CONTABIL_ERR
             INTO TABLE IT_ZIB_CONTABIL_ERR
             WHERE OBJ_KEY  = O_ALV-OBJ_KEY.

          WTAB_MSG-NAME    = '------------------------MENSAGEM ERRO---------------------------------------'.
          APPEND WTAB_MSG TO ITAB_MSG .
          CLEAR WTAB_MSG.
          LOOP AT IT_ZIB_CONTABIL_ERR INTO WA_ZIB_CONTABIL_ERR.
            WTAB_MSG-NAME = WA_ZIB_CONTABIL_ERR-MESSAGE.
            APPEND WTAB_MSG TO ITAB_MSG .
            CLEAR WTAB_MSG.
          ENDLOOP.

          CONCATENATE 'DOCUMENTO ' O_ALV-BELNR INTO MSG_ALV SEPARATED BY SPACE.
          CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
            EXPORTING
              ENDPOS_COL   = 140
              ENDPOS_ROW   = 20
              STARTPOS_COL = 60
              STARTPOS_ROW = 15
              TITLETEXT    = MSG_ALV
            TABLES
              VALUETAB     = ITAB_MSG
            EXCEPTIONS
              BREAK_OFF    = 1
              OTHERS       = 2.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "catch_hotspot

  METHOD HANDLE_TOP_OF_PAGE .
    PERFORM EVENT_TOP_OF_PAGE USING E_DYNDOC_ID
    TABLE_INDEX .
  ENDMETHOD.                    "HANDLE_TOP_OF_PAGE


ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


DATA:       EVENT_RECEIVER   TYPE REF TO LCL_EVENT_RECEIVER.


**************************************************************************
INITIALIZATION.
**************************************************************************

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = SY-DATUM
    IMPORTING
      LAST_DAY_OF_MONTH = P_BUDAT2
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E398(00) WITH TEXT-017 SPACE SPACE SPACE. "'Erro ao determinar dia!'
  ENDIF.
  P_AUGDT = P_BUDAT2.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
  DATA: VARIANTE     LIKE DISVARIANT,
        DEF_VARIANTE LIKE DISVARIANT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.

  WG_SAVE = 'X'.
  WG_VARIANT-REPORT = WG_REPNAME.
  WG_X_VARIANT = WG_VARIANT.

  IF ( NOT P_VARI IS INITIAL ).
    WG_VARIANT-VARIANT = P_VARI.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = WG_VARIANT
      I_SAVE        = WG_SAVE
    IMPORTING
      ES_VARIANT    = WG_X_VARIANT
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.

  IF ( SY-SUBRC NE 0 ).
    MESSAGE S000(Z01) WITH TEXT-018. "Não existe variante
*    STOP.
  ELSE.
    MOVE WG_X_VARIANT-VARIANT TO P_VARI.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_HKONT-LOW.
  PERFORM F_BUSCAR_CONTA.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_HKONT-HIGH.
  PERFORM F_BUSCAR_CONTA.


AT SELECTION-SCREEN.

  P_BUDAT2 = P_BUDAT.
  P_AUGDT = P_BUDAT.
  P_SPMON = P_BUDAT.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'SC1'.
      SCREEN-ACTIVE = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.

*************Processamento****************************
START-OF-SELECTION.
  DATA: WG_INDEX TYPE SY-TABIX.
*CONSISTÊNCIAS

  IF P_V_LANC EQ 'X' AND P_HKONT2 NE 'X'.
    PERFORM BUSCA_ZGL12_AVM.
    EXIT.
  ENDIF.
  "ELSEIF P_HKONT2 EQ 'X'.
  "  EXIT.
  "ENDIF.

  PERFORM ZF_VALIDA_CAMPOS CHANGING SY-SUBRC.
  CHECK SY-SUBRC IS INITIAL.

  SELECT SINGLE BUKRS LAND1
    FROM T001
    INTO WA_T001
   WHERE BUKRS = P_BUKRS.

  "'US'
*  IF P_BUKRS = '0004'.
*    WA_T001-LAND1 = 'US'.
*  ENDIF.

  SELECT SINGLE LAND1 WAERS
    FROM T005
    INTO WA_T005
   WHERE LAND1 = WA_T001-LAND1.

*  Atribui Moeda Principal
  WG_CUR_P = WA_T005-WAERS.


  IF P_V_LANC <> 'X'.
*Verifica campos da T001B
*Verifica se o período esta aberto
    SELECT FRYE1 FRPE1 FROM T001B
      INTO TABLE T_T001B
     WHERE BUKRS EQ P_BUKRS
       AND FRYE1 EQ P_BUDAT(4)
       AND FRPE1 EQ P_BUDAT+4(2).

  ENDIF.

**********************************************************************
*Verifica Data Archive - 115495 IR133267 - ZGL042 saldo bancário - Archive - PSA
**********************************************************************

  DATA: DT_TVARVC TYPE TVARVC-LOW.
  SELECT SINGLE MAX( LOW ) AS DT
        FROM TVARVC
       INTO DT_TVARVC
        WHERE NAME ='MAGGI_DATA_ARCHIVE'.

  DATA(DT_ARQUIVE) = | { DT_TVARVC+6(2) }/{ DT_TVARVC+4(2) }/{ DT_TVARVC+0(4) }|.

  DATA(MSG_DT_TVARVC) = 'Os documentos até'  && DT_ARQUIVE && ' foram arquivados.  Para consulta-los usar a transação FBL3N'.

  IF P_BUDAT <= DT_TVARVC .
    MESSAGE MSG_DT_TVARVC TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE TO TRANSACTION 'ZGL042'.
  ENDIF.

**********************************************************************

*  IF SY-SUBRC <> 0.
*Se não encontrou o mês está fechado!
*    MESSAGE I398(00) WITH TEXT-015  "Mês referente a Data da avaliação está
*                          TEXT-016. "fechado,é possível somente a visualização '.
*
**    LEAVE TO TRANSACTION 'ZGL042'.
*  ELSE.
*Trata Taxa de Câmbio
  PERFORM BUSCA_TX_CAMBIO.
*Trata Partidas em Aberto
  CASE 'X'.
    WHEN P_LIFNR.
      PERFORM F_FORNECEDOR.

      SELECT *
        FROM ZGL012_AVM
        INTO TABLE T_ZGL012_ATUAL
       WHERE BUKRS   EQ P_BUKRS
         AND DT_AVAL EQ P_BUDAT
         AND LIFNR   IN S_LIFNR
         AND HKONT   IN S_KONT
         AND LIFNR   NE ' '
         AND ESTORNO NE 'X'.

      DELETE T_ZGL012_ATUAL WHERE LIFNR = SPACE.

    WHEN P_KUNNR.
      PERFORM F_CLIENTE.

      SELECT *
        FROM ZGL012_AVM
        INTO TABLE T_ZGL012_ATUAL
       WHERE BUKRS EQ P_BUKRS
         AND DT_AVAL EQ P_BUDAT
         AND KUNNR   IN S_KUNNR
         AND HKONT   IN S_DONT
         AND KUNNR   NE ' '
         AND ESTORNO NE 'X'.

      DELETE T_ZGL012_ATUAL WHERE KUNNR = SPACE.

    WHEN P_HKONT.
      PERFORM F_CONTA.

      SELECT *
        FROM ZGL012_AVM
        INTO TABLE T_ZGL012_ATUAL
       WHERE BUKRS EQ P_BUKRS
         AND DT_AVAL EQ P_BUDAT
         AND HKONT   IN S_HKONT
         AND LIFNR   EQ ' '
         AND KUNNR   EQ ' '
         AND ESTORNO NE 'X'.

      DELETE T_ZGL012_ATUAL WHERE HKONT = SPACE.
      DELETE T_ZGL012_ATUAL WHERE ( KUNNR NE SPACE  OR  LIFNR NE SPACE ).
    WHEN P_HKONT2.
      IF P_E_LANC IS INITIAL.
        PERFORM F_CONTA2.
      ENDIF.
  ENDCASE.

  IF P_BUKRS = '0201' OR P_BUKRS = '0202'.
    DELETE T_ZGL012_ATUAL WHERE WAERS = 'USD'.
  ENDIF.

  IF NOT T_ZGL012_ATUAL[] IS INITIAL AND P_HKONT2 NE 'X'.
    "
    "substitui valores tabelas standard pelos valores ja criados
    LOOP AT T_ZGL012_ATUAL INTO WA_ZGL012_AUX.
      DELETE T_ZGL012_AVM WHERE BUKRS     = WA_ZGL012_AUX-BUKRS   AND
                                DT_AVAL   = WA_ZGL012_AUX-DT_AVAL AND
                                BELNR     = WA_ZGL012_AUX-BELNR   AND
                                BUZEI     = WA_ZGL012_AUX-BUZEI   AND
                                MOEDA_ATU = WA_ZGL012_AUX-MOEDA_ATU.

    ENDLOOP.

    APPEND LINES OF T_ZGL012_ATUAL TO T_ZGL012_AVM.

  ENDIF.

  IF T_ZGL012_AVM[] IS NOT INITIAL  AND P_HKONT2 NE 'X'.
    SELECT *
      FROM ZIB_CONTABIL_ERR
      INTO TABLE IT_ZIB_CONTABIL_ERR
       FOR ALL ENTRIES IN T_ZGL012_AVM
     WHERE OBJ_KEY = T_ZGL012_AVM-OBJ_KEY.

    SORT IT_ZIB_CONTABIL_ERR BY OBJ_KEY.

    SELECT ZIB_CONTABIL_CHV~MANDT
           ZIB_CONTABIL_CHV~OBJ_KEY
           ZIB_CONTABIL_CHV~BELNR
           ZIB_CONTABIL_CHV~BUKRS
           ZIB_CONTABIL_CHV~GJAHR
           ZIB_CONTABIL~BLDAT
      FROM ZIB_CONTABIL_CHV
     INNER JOIN ZIB_CONTABIL ON ZIB_CONTABIL~OBJ_KEY EQ ZIB_CONTABIL_CHV~OBJ_KEY
      INTO TABLE IT_ZIB_CONTABIL_CHV
       FOR ALL ENTRIES IN T_ZGL012_AVM
     WHERE ZIB_CONTABIL_CHV~OBJ_KEY = T_ZGL012_AVM-OBJ_KEY.

    SORT IT_ZIB_CONTABIL_CHV BY OBJ_KEY.

  ENDIF.

  PERFORM F_ALV.

*  ENDIF.
*************Subrotinas*******************************
*&---------------------------------------------------------------------*
*&      Form  BUSCA_ZGL12_AVM
*&---------------------------------------------------------------------*
*     Trata Fechamento Mês
*----------------------------------------------------------------------*
FORM BUSCA_ZGL12_AVM .

  CASE 'X'.
    WHEN P_LIFNR.
      SELECT *
        FROM ZGL012_AVM
        INTO TABLE T_ZGL012_AVM
       WHERE BUKRS   EQ P_BUKRS
         AND DT_AVAL EQ P_BUDAT
         AND LIFNR   IN S_LIFNR
         AND HKONT   IN S_KONT
         AND LIFNR   NE ''.
    WHEN P_KUNNR.
      SELECT *
        FROM ZGL012_AVM
        INTO TABLE T_ZGL012_AVM
       WHERE BUKRS   EQ P_BUKRS
         AND DT_AVAL EQ P_BUDAT
         AND KUNNR   IN S_KUNNR
         AND HKONT   IN S_DONT
         AND KUNNR   NE ''.
    WHEN P_HKONT.
      SELECT *
        FROM ZGL012_AVM
        INTO TABLE T_ZGL012_AVM
       WHERE BUKRS   EQ P_BUKRS
         AND DT_AVAL EQ P_BUDAT
         AND HKONT   IN S_HKONT
         AND HKONT   NE ''
         AND LIFNR   EQ ''
         AND KUNNR   EQ ''.
  ENDCASE.

  IF SY-SUBRC = 0.
    PERFORM: F_ALV,
             ATUALIZA_TELA.

  ELSE.
    MESSAGE I000(SU) WITH TEXT-019. "Registros não encontrados
*    STOP.
  ENDIF.

ENDFORM.                    " BUSCA_ZGL12_AVM
*&---------------------------------------------------------------------*
*&      Form  BUSCA_TX_CAMBIO
*&---------------------------------------------------------------------*
*       Trata Taxa de Câmbio
*----------------------------------------------------------------------*
FORM BUSCA_TX_CAMBIO .
  RANGES: R_GDATU FOR TCURR-GDATU,
          R_FCURR FOR TCURR-FCURR.

  DATA: WL_DATE_AUX  TYPE DATUM,
        WL_DATE_V1   TYPE DATUM,
        WL_INPUT(10).

  MOVE 'IEQ' TO R_GDATU.

  WL_DATE_AUX = P_BUDAT.

  IF ( P_BUKRS NE '0200' ) AND
     ( P_BUKRS NE '0201' ) AND
     ( P_BUKRS NE '0202' ).
    ADD 1 TO WL_DATE_AUX.
  ENDIF.

  WRITE WL_DATE_AUX TO WL_INPUT.

*** PBI - 64926 - Inicio - CSB
  IF ( P_BUKRS EQ '0203' ).
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Início
    DATA: LV_LASTDAY TYPE SY-DATUM,
          LT_DAY_ATT TYPE STANDARD TABLE OF CASDAYATTR.

    SELECT *
      FROM TVARVC
      INTO TABLE @DATA(T_VARVC)
      WHERE NAME LIKE 'Z_WEEK_%'.

    IF SY-SUBRC IS INITIAL.
      LOOP AT T_VARVC INTO DATA(LW_VARVC).
        IF LW_VARVC CA '5'.
          DATA(LV_DAYS_5) = LW_VARVC-LOW.
        ELSEIF LW_VARVC CA '6'.
          DATA(LV_DAYS_6) = LW_VARVC-LOW.
        ELSEIF LW_VARVC CA '7'.
          DATA(LV_DAYS_7) = LW_VARVC-LOW.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN            = P_BUDAT
      IMPORTING
        LAST_DAY_OF_MONTH = LV_LASTDAY
      EXCEPTIONS
        DAY_IN_NO_DATE    = 1
        OTHERS            = 2.

    IF SY-SUBRC EQ 0.

      REFRESH LT_DAY_ATT[].
      CALL FUNCTION 'DAY_ATTRIBUTES_GET'
        EXPORTING
          FACTORY_CALENDAR           = 'BR'
          HOLIDAY_CALENDAR           = 'BR'
          DATE_FROM                  = LV_LASTDAY
          DATE_TO                    = LV_LASTDAY
          LANGUAGE                   = SY-LANGU
        TABLES
          DAY_ATTRIBUTES             = LT_DAY_ATT
        EXCEPTIONS
          FACTORY_CALENDAR_NOT_FOUND = 1
          HOLIDAY_CALENDAR_NOT_FOUND = 2
          DATE_HAS_INVALID_FORMAT    = 3
          DATE_INCONSISTENCY         = 4
          OTHERS                     = 5.

      IF SY-SUBRC EQ 0.
        READ TABLE LT_DAY_ATT INTO DATA(LW_DAY_ATT) INDEX 1.
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Fim
        CLEAR: WL_DATE_AUX, WL_INPUT.
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Início
        CASE LW_DAY_ATT-WEEKDAY.
          WHEN 5.
            WL_DATE_V1 = P_BUDAT + LV_DAYS_5.
          WHEN 6.
            WL_DATE_V1 = P_BUDAT + LV_DAYS_6.
          WHEN 7.
            WL_DATE_V1 = P_BUDAT + LV_DAYS_7.
          WHEN OTHERS.
            WL_DATE_V1 = P_BUDAT + 1.
        ENDCASE.
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Fim
        ZCL_MIRO=>GET_PROXIMO_DIA_UTIL(
          EXPORTING
            I_DATA_BASE        = CONV #( WL_DATE_V1 )
            I_SIGNUM           = '+'
            I_CK_DATA_ZLES0145 = ABAP_TRUE
          RECEIVING
            R_DATA             = WL_DATE_AUX  "Retorno.
          EXCEPTIONS
            ERRO               = 1 ).

        WRITE WL_DATE_AUX TO WL_INPUT.
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Início
      ENDIF.
    ENDIF.
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Fim
  ENDIF.
*** PBI - 64926 - Fim - CSB


  IF WL_INPUT IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = WL_INPUT
      IMPORTING
        OUTPUT = R_GDATU-LOW.

    APPEND R_GDATU.
  ENDIF.

*BUSCA TAXA EM EUR
  SELECT KURST FCURR TCURR GDATU UKURS
    FROM TCURR
    INTO TABLE T_TCURR
   WHERE KURST = 'B'
     AND TCURR EQ WA_T005-WAERS
     AND GDATU IN R_GDATU.

  IF SY-SUBRC = 0.
*Como o campo é de data invertida para pegar oa mais atual a data GDATU tem que ser ordenada do menor pro maior
    SORT T_TCURR BY GDATU ASCENDING FCURR ASCENDING .
*    IF P_BUKRS EQ '0004'.
*      C_USD = 'BRL'.
*    ELSE.
*      C_USD = 'USD'.
*    ENDIF.
    READ TABLE T_TCURR INTO WA_TCURR WITH KEY FCURR = C_USD BINARY SEARCH.
    IF SY-SUBRC = 0.
      XTX_USD = WA_TCURR-UKURS.
    ELSE.
      MESSAGE E398(00) WITH TEXT-020. "Erro ao encontrar taxa de conversão do Dólar
    ENDIF.

  ELSE.
    MESSAGE E398(00) WITH TEXT-020."Erro ao encontrar taxa de conversão do Dólar'
  ENDIF.

  IF P_BUKRS = '0101'.
    SELECT KURST FCURR TCURR GDATU UKURS
      FROM TCURR
      INTO TABLE T_TCURR_G
     WHERE KURST = 'G'
       AND TCURR EQ WA_T005-WAERS
       AND GDATU IN R_GDATU.

    IF SY-SUBRC = 0.
*Como o campo é de data invertida para pegar oa mais atual a data GDATU tem que ser ordenada do menor pro maior
      SORT T_TCURR_G BY GDATU ASCENDING FCURR ASCENDING .

      READ TABLE T_TCURR_G INTO WA_TCURR WITH KEY FCURR = C_USD BINARY SEARCH.
      IF SY-SUBRC = 0.
        XTX_USD = WA_TCURR-UKURS.
      ELSE.
        MESSAGE E398(00) WITH TEXT-020. "Erro ao encontrar taxa de conversão do Dólar
      ENDIF.
    ELSE.
      MESSAGE E398(00) WITH TEXT-020."Erro ao encontrar taxa de conversão do Dólar'
    ENDIF.
  ENDIF.

*  IF P_BUKRS = '0004' OR P_BUKRS = '0201' OR P_BUKRS = '0200'.
  SELECT KURST FCURR TCURR GDATU UKURS
     FROM TCURR
     INTO TABLE T_TCURR_A
    WHERE KURST = 'B'
      AND TCURR EQ 'USD'
      AND GDATU IN R_GDATU.

  SORT T_TCURR_A BY GDATU ASCENDING FCURR ASCENDING.
*  ENDIF.

ENDFORM.                    " BUSCA_TX_CAMBIO
*&---------------------------------------------------------------------*
*&      Form  F_FORNECEDOR
*&---------------------------------------------------------------------*
*       Partidas em Aberto fornecedor
*----------------------------------------------------------------------*
FORM F_FORNECEDOR .

  DATA: BEGIN OF T_BKPF0 OCCURS 0.
          INCLUDE STRUCTURE BKPF.
  DATA: END OF T_BKPF0.

  DATA: BEGIN OF T_BKPF0E OCCURS 0.
          INCLUDE STRUCTURE BKPF.
  DATA: END OF T_BKPF0E.

  DATA: VG_FIRST TYPE DATUM.

  CONCATENATE P_BUDAT+0(6) '01' INTO VG_FIRST.


  DATA: BEGIN OF ITL_BSIK OCCURS 0,
          BUKRS TYPE BSIK-BUKRS,
          LIFNR TYPE BSIK-LIFNR,
          UMSKZ TYPE BSIK-UMSKZ,
          AUGBL TYPE BSIK-AUGBL,
          GJAHR TYPE BSIK-GJAHR,
          BELNR TYPE BSIK-BELNR,
          BUZEI TYPE BSIK-BUZEI,
          BUDAT TYPE BSIK-BUDAT,
          WAERS TYPE BSIK-WAERS,
          BLART TYPE BLART,
          BSCHL TYPE BSCHL,
          SHKZG TYPE SHKZG,
          GSBER TYPE GSBER,
          DMBTR TYPE BSIK-DMBTR,
          WRBTR TYPE BSIK-WRBTR,
          HKONT TYPE BSIK-HKONT,
          DMBE2 TYPE BSIK-DMBE2,
          AUGDT TYPE BSIK-AUGDT,
        END OF ITL_BSIK.

  DATA: BEGIN OF ITL_BKPF OCCURS 0,
          BUKRS TYPE BKPF-BUKRS,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          STGRD TYPE BKPF-STGRD,
          STBLG TYPE BKPF-STBLG,
          STJAH TYPE BKPF-STJAH,
        END OF ITL_BKPF.

  IF P_BUKRS = '0004'.
    WA_T005-WAERS = 'XXX'. "pega todas as moedas, inclusive BRL
  ENDIF.
  "Em Aberto
  SELECT BUKRS LIFNR UMSKZ BELNR BUZEI BUDAT WAERS BLART BSCHL SHKZG GSBER DMBTR WRBTR HKONT DMBE2 GJAHR
    FROM BSIK
    INTO TABLE T_BSIK
    WHERE BUKRS EQ P_BUKRS
      AND BUDAT LE P_BUDAT
      AND LIFNR IN S_LIFNR
      AND HKONT IN S_KONT
      AND WAERS NE WA_T005-WAERS
      AND BLART NE 'VC'
      AND ANLN1 EQ ''
      AND BELNR IN S_BELNR
      AND DMBE2 GE WG_DMBT2.

  IF SY-SUBRC IS INITIAL.
    SELECT * FROM BKPF INTO TABLE T_BKPF0
      FOR ALL ENTRIES IN T_BSIK
      WHERE BUKRS = T_BSIK-BUKRS AND
            BELNR = T_BSIK-BELNR AND
            GJAHR = T_BSIK-GJAHR AND
            BSTAT = 'S'.

    IF SY-SUBRC = 0.
      "ALRS 13.11.2014 Estorno dentro do mês não leva
      SELECT *
         FROM BKPF
         INTO TABLE T_BKPF0E
          FOR ALL ENTRIES IN T_BKPF0
           WHERE BUKRS EQ T_BKPF0-BUKRS
             AND BELNR EQ T_BKPF0-STBLG
             AND GJAHR EQ T_BKPF0-STJAH.

      LOOP AT T_BKPF0.
        READ TABLE T_BKPF0E WITH KEY BUKRS = T_BKPF0-BUKRS
                                     BELNR = T_BKPF0-STBLG
                                     GJAHR = T_BKPF0-STJAH.
        IF SY-SUBRC = 0.
          IF  T_BKPF0E-BUDAT LE P_BUDAT.
            READ TABLE T_BSIK INTO WA_BSIK WITH KEY BUKRS = T_BKPF0-BUKRS
                                                             BELNR = T_BKPF0-BELNR
                                                             GJAHR = T_BKPF0-GJAHR.
            IF SY-SUBRC = 0.
              DELETE T_BSIK INDEX SY-TABIX.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE T_BSIK INTO WA_BSIK WITH KEY BUKRS = T_BKPF0-BUKRS
                                                  BELNR = T_BKPF0-BELNR
                                                  GJAHR = T_BKPF0-GJAHR.
          IF SY-SUBRC = 0.
            DELETE T_BSIK INDEX SY-TABIX.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

*  "compensados depois
  SELECT BUKRS LIFNR UMSKZ AUGBL GJAHR BELNR BUZEI BUDAT
       WAERS BLART BSCHL SHKZG GSBER DMBTR WRBTR HKONT DMBE2 AUGDT
  FROM BSAK
  INTO TABLE ITL_BSIK
  WHERE BUKRS EQ P_BUKRS
    AND BUDAT LE P_BUDAT
    AND AUGDT GE VG_FIRST "P_BUDAT
    AND LIFNR IN S_LIFNR
    AND HKONT IN S_KONT
    AND WAERS NE WA_T005-WAERS
    AND BLART NE 'VC'
    AND ANLN1 EQ ''
    AND BELNR IN S_BELNR
    AND DMBE2 GE WG_DMBT2.

  IF SY-SUBRC = 0.
    SELECT *
      FROM BKPF INTO TABLE T_BKPF0
      FOR ALL ENTRIES IN ITL_BSIK
      WHERE BUKRS = ITL_BSIK-BUKRS AND
            BELNR = ITL_BSIK-BELNR AND
            GJAHR = ITL_BSIK-GJAHR AND
            BSTAT = 'S'. "MEMO

    IF SY-SUBRC = 0.
      "ALRS 13.11.2014 Estorno dentro do mês não leva
      SELECT *
         FROM BKPF
         INTO TABLE T_BKPF0E
          FOR ALL ENTRIES IN T_BKPF0
           WHERE BUKRS EQ T_BKPF0-BUKRS
             AND BELNR EQ T_BKPF0-STBLG
             AND GJAHR EQ T_BKPF0-STJAH.

      LOOP AT T_BKPF0.
        READ TABLE T_BKPF0E WITH KEY BUKRS = T_BKPF0-BUKRS
                                 BELNR = T_BKPF0-STBLG
                                 GJAHR = T_BKPF0-STJAH.
        IF SY-SUBRC = 0.
          IF  T_BKPF0E-BUDAT LE P_BUDAT.
            READ TABLE ITL_BSIK WITH KEY BUKRS = T_BKPF0-BUKRS
                                       BELNR = T_BKPF0-BELNR
                                       GJAHR = T_BKPF0-GJAHR.
            IF SY-SUBRC = 0.
              DELETE ITL_BSIK INDEX SY-TABIX.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE ITL_BSIK WITH KEY BUKRS = T_BKPF0-BUKRS
                                       BELNR = T_BKPF0-BELNR
                                       GJAHR = T_BKPF0-GJAHR.
          IF SY-SUBRC = 0.
            DELETE ITL_BSIK INDEX SY-TABIX.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT ITL_BSIK.

      IF ITL_BSIK-BELNR = ITL_BSIK-AUGBL.
        DELETE ITL_BSIK INDEX SY-TABIX.
        CONTINUE.
      ENDIF.

      IF ITL_BSIK-BUDAT(6) = ITL_BSIK-AUGDT(6).
        DELETE ITL_BSIK INDEX SY-TABIX.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING ITL_BSIK TO WA_BSIK.
      IF ITL_BSIK-AUGDT LE P_BUDAT.
        WA_BSIK-ST_REV = 'S'.
      ENDIF.

      APPEND WA_BSIK TO T_BSIK.
      CLEAR WA_BSIK.

    ENDLOOP.
  ENDIF.

  DELETE T_BSIK WHERE BLART = 'VC'.

  IF P_BUKRS = '0004'.
    WA_T005-WAERS = 'BRL'. "pega todas as moedas, inclusive BRL
  ENDIF.
  PERFORM: CARREGA_T_ZGL_FOR.     "Carrega tabela do ALV

ENDFORM.                    " F_FORNECEDOR
*&---------------------------------------------------------------------*
*&      Form  CARREGA_T_ZGL_FOR
*&---------------------------------------------------------------------*
*       Carrega tabela aux. fornecedor
*----------------------------------------------------------------------*
FORM CARREGA_T_ZGL_FOR .
  DATA: W_TB    TYPE SY-TABIX,
        W_LOOP  TYPE I,
        W_DMBTR TYPE ZGL012_AVM-DMBTR,
        W_MOEDA TYPE ZGL012_AVM-WAERS.

  DATA TABIX TYPE SY-TABIX.

  PERFORM ULTIMA_AVALIACAO_FOR.  "Trata ültima avaliação

  DELETE T_BSIK WHERE DMBE2 EQ '0.01'
                  OR  UMSKZ  EQ 'F'.

  LOOP AT T_BSIK INTO WA_BSIK.
    WA_ZGL012_AVM-MANDT   = SY-MANDT.
    WA_ZGL012_AVM-BUKRS   = WA_BSIK-BUKRS.
    WA_ZGL012_AVM-DT_AVAL = P_BUDAT.
    WA_ZGL012_AVM-LIFNR   = WA_BSIK-LIFNR.
    WA_ZGL012_AVM-BELNR   = WA_BSIK-BELNR.
    WA_ZGL012_AVM-BUZEI   = WA_BSIK-BUZEI.
    WA_ZGL012_AVM-BUDAT   = WA_BSIK-BUDAT.
    WA_ZGL012_AVM-BSCHL   = WA_BSIK-BSCHL.
    WA_ZGL012_AVM-WAERS   = WA_BSIK-WAERS.
    WA_ZGL012_AVM-GSBER   = WA_BSIK-GSBER.
    WA_ZGL012_AVM-ST_REV  = WA_BSIK-ST_REV.
    WA_ZGL012_AVM-AUGBL   = WA_BSIK-AUGBL.
    WA_ZGL012_AVM-AUGDT   = WA_BSIK-AUGDT.

    IF WA_BSIK-SHKZG = 'S'.
      WA_ZGL012_AVM-DMBTR   = WA_BSIK-DMBTR.
      WA_ZGL012_AVM-DMBE2   = WA_BSIK-DMBE2.
      WA_ZGL012_AVM-WRBTR   = WA_BSIK-WRBTR.
    ELSE.
      WA_ZGL012_AVM-DMBTR   = WA_BSIK-DMBTR * ( -1 ).
      WA_ZGL012_AVM-DMBE2   = WA_BSIK-DMBE2 * ( -1 ).
      WA_ZGL012_AVM-WRBTR   = WA_BSIK-WRBTR * ( -1 ).
    ENDIF.

    IF P_BUKRS = '0004' OR P_BUKRS = '0201' OR P_BUKRS = '0202' OR P_BUKRS = '0037'. "Elimina moeda dolar
      IF WA_ZGL012_AVM-WAERS = C_USD.
        CONTINUE.
      ENDIF.
    ENDIF.


    IF WG_CUR_P EQ 'PYG'.
      WA_ZGL012_AVM-DMBTR = WA_ZGL012_AVM-DMBTR * 100.
    ENDIF.

    IF WA_ZGL012_AVM-WAERS = C_USD.
      W_LOOP = 1.
    ELSE.
      W_LOOP = 2.
    ENDIF.

    CLEAR: W_DMBTR.
    DO W_LOOP TIMES.
      XTX_USD = 0.
      IF SY-INDEX = 1.
        W_MOEDA = ''.
        READ TABLE  T_TCURR INTO WA_TCURR WITH KEY FCURR = WA_ZGL012_AVM-WAERS.
        IF P_BUKRS = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WA_BSIK-HKONT
            IMPORTING
              OUTPUT = WA_BSIK-HKONT.
          IF WA_BSIK-HKONT+0(1) = '1'. "ativos (compra)
            READ TABLE  T_TCURR_G INTO WA_TCURR WITH KEY FCURR = WA_ZGL012_AVM-WAERS.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_BSIK-HKONT
            IMPORTING
              OUTPUT = WA_BSIK-HKONT.
        ENDIF.
        IF SY-SUBRC = 0.
          XTX_USD = WA_TCURR-UKURS.
          WA_ZGL012_AVM-TX_FECH  = XTX_USD.
        ENDIF.

        IF XTX_USD LT 0.
          IF WA_BSIK-DMBTR NE 0.
            WA_ZGL012_AVM-KURSF   = WA_BSIK-WRBTR / WA_BSIK-DMBTR.
          ENDIF.
        ELSE.
          IF WA_BSIK-WRBTR NE 0.
            WA_ZGL012_AVM-KURSF   = WA_BSIK-DMBTR / WA_BSIK-WRBTR.
          ENDIF.
        ENDIF.
      ELSE. "ler o USD
        W_MOEDA = C_USD.
        READ TABLE  T_TCURR INTO WA_TCURR WITH KEY FCURR = C_USD.
        IF P_BUKRS = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WA_BSIK-HKONT
            IMPORTING
              OUTPUT = WA_BSIK-HKONT.
          IF WA_BSIK-HKONT+0(1) = '1'. "ativos (compra)
            READ TABLE  T_TCURR_G INTO WA_TCURR WITH KEY FCURR = C_USD.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_BSIK-HKONT
            IMPORTING
              OUTPUT = WA_BSIK-HKONT.
        ENDIF.
        IF SY-SUBRC = 0.
          XTX_USD = WA_TCURR-UKURS.
          WA_ZGL012_AVM-TX_FECH  = XTX_USD.
        ENDIF.

        IF XTX_USD LT 0.
          IF WA_BSIK-WRBTR NE 0.
            WA_ZGL012_AVM-KURSF   = WA_BSIK-DMBE2 / WA_BSIK-WRBTR.
          ENDIF.
        ELSE.
          IF WA_BSIK-DMBE2 NE 0.
            WA_ZGL012_AVM-KURSF  = WA_BSIK-WRBTR / WA_BSIK-DMBE2.
          ENDIF.
        ENDIF.
      ENDIF.


      IF WA_ZGL012_AVM-TX_FECH LT 0.
        MULTIPLY WA_ZGL012_AVM-TX_FECH BY -1.
      ENDIF.

      IF SY-INDEX = 1.
        IF XTX_USD LT 0.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBTR - ( WA_ZGL012_AVM-WRBTR / WA_ZGL012_AVM-TX_FECH ) .
          WA_ZGL012_AVM-VLR_ATUALIZADO    = WA_ZGL012_AVM-WRBTR / WA_ZGL012_AVM-TX_FECH.
        ELSE.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBTR - ( WA_ZGL012_AVM-WRBTR * WA_ZGL012_AVM-TX_FECH ) .
          WA_ZGL012_AVM-VLR_ATUALIZADO    = WA_ZGL012_AVM-WRBTR * WA_ZGL012_AVM-TX_FECH.
        ENDIF.
        W_DMBTR = WA_ZGL012_AVM-VLR_ATUALIZADO.
        IF P_BUKRS = '0201' OR P_BUKRS = '0202' OR ( P_BUKRS = '0004' AND WA_BSIK-WAERS = 'BRL' ).
          IF P_BUKRS = '0004'.
            W_DMBTR = WA_ZGL012_AVM-DMBTR. " valor em BRL para atualizar USD Somente neste caso
          ENDIF.
          CONTINUE.
        ENDIF.
      ELSE. "USD
*        IF wa_zgl012_avm-bukrs = '0201'.
*          MULTIPLY xtx_usd BY -1.
*        ENDIF.
        IF XTX_USD LT 0.
          WA_ZGL012_AVM-VLR_ATUALIZADO    =  W_DMBTR  * WA_ZGL012_AVM-TX_FECH.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBE2 - WA_ZGL012_AVM-VLR_ATUALIZADO .
        ELSE.
          WA_ZGL012_AVM-VLR_ATUALIZADO    =  W_DMBTR / WA_ZGL012_AVM-TX_FECH.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBE2 -   WA_ZGL012_AVM-VLR_ATUALIZADO.
        ENDIF.
      ENDIF.

      WA_ZGL012_AVM-HKONT   = WA_BSIK-HKONT.
      WA_ZGL012_AVM-UMSKZ   = WA_BSIK-UMSKZ.
*Grava valor acumulado do mês anterior.

      READ TABLE T_ZGL012_AUX INTO WA_ZGL012_AUX WITH KEY
                                   BUKRS     =  WA_ZGL012_AVM-BUKRS
                                   LIFNR     =  WA_ZGL012_AVM-LIFNR
                                   BELNR     =  WA_ZGL012_AVM-BELNR
                                   BUZEI     =  WA_ZGL012_AVM-BUZEI
                                   BUDAT     =  WA_ZGL012_AVM-BUDAT
                                   MOEDA_ATU =  W_MOEDA.

*NAO UTILIZAR O BINARY SEARCH POIS COM O COMANDO DELETE ABAIXO PODE DESORDENAR
      IF SY-SUBRC = 0 .
        IF WA_ZGL012_AUX-DOC_REV IS INITIAL.
          WA_ZGL012_AVM-VLR_ACUM_MES_ANT = WA_ZGL012_AUX-VLR_ACUM_MES_ATU.
        ELSE.
          WA_ZGL012_AVM-VLR_ACUM_MES_ANT = 0.
        ENDIF.

      ELSE.
        WA_ZGL012_AVM-VLR_ACUM_MES_ANT = 0.
      ENDIF.

      WA_ZGL012_AVM-VLR_VARIACAO = WA_ZGL012_AVM-VLR_ACUM_MES_ATU - WA_ZGL012_AVM-VLR_ACUM_MES_ANT.
      WA_ZGL012_AVM-MOEDA_ATU = W_MOEDA.
*Grava documentos compensados
      APPEND WA_ZGL012_AVM TO T_ZGL012_AVM.
    ENDDO.
  ENDLOOP.

ENDFORM.                    " CARREGA_T_ZGL_FOR

*&---------------------------------------------------------------------*
*&      Form  ULTIMA_AVALIACAO_FOR
*&---------------------------------------------------------------------*
*       Última data gravada_
*----------------------------------------------------------------------*
FORM ULTIMA_AVALIACAO_FOR.
*Define última data de avaliação gravada.
  DATA: L_UDATA TYPE DATUM,
        L_PDATA TYPE DATUM.

  DATA: TL_BKPF  TYPE TABLE OF BKPF WITH HEADER LINE,
        TL_BKPFE TYPE TABLE OF BKPF WITH HEADER LINE.

  L_UDATA = P_BUDAT.
  L_UDATA+6(2) = '01'.   "tranforma em primeiro dia do mês corrente
  L_UDATA = L_UDATA - 1. "transforma no último dia do mês anterior

  L_PDATA = L_UDATA - 120.

  CHECK T_BSIK[] IS NOT INITIAL.

  SELECT * INTO TABLE T_ZGL012_AUX FROM ZGL012_AVM
    FOR ALL ENTRIES IN T_BSIK
    WHERE
      BUKRS   EQ T_BSIK-BUKRS  AND
      BELNR   EQ T_BSIK-BELNR  AND
      BUZEI   EQ T_BSIK-BUZEI  AND
      DT_AVAL GE L_PDATA       AND
      DT_AVAL LE L_UDATA       AND
      ESTORNO NE 'X'           AND
      LIFNR   IN  S_LIFNR      AND
      HKONT   IN  S_KONT.

  IF SY-SUBRC = 0.
    SELECT *
      FROM BKPF
      INTO TABLE TL_BKPF
       FOR ALL ENTRIES IN T_ZGL012_AUX
        WHERE BUKRS EQ T_ZGL012_AUX-BUKRS
          AND BELNR EQ T_ZGL012_AUX-BELNR
          AND STBLG NE SPACE.

    "ALRS 13.11.2014 Estorno dentro do mês não leva
    IF TL_BKPF[] IS NOT INITIAL.
      SELECT *
         FROM BKPF
         INTO TABLE TL_BKPFE
          FOR ALL ENTRIES IN TL_BKPF
           WHERE BUKRS EQ TL_BKPF-BUKRS
             AND BELNR EQ TL_BKPF-STBLG
             AND GJAHR EQ TL_BKPF-STJAH .
    ENDIF.

    SORT T_ZGL012_AUX BY BUKRS BELNR.
    LOOP AT TL_BKPF.
      READ TABLE TL_BKPFE WITH KEY BUKRS = TL_BKPF-BUKRS
                                  BELNR = TL_BKPF-STBLG
                                  GJAHR = TL_BKPF-STJAH.
      IF SY-SUBRC = 0.
        IF  TL_BKPFE-BUDAT LE P_BUDAT.
          LOOP AT T_ZGL012_AUX WHERE BELNR EQ TL_BKPF-BELNR
                               AND   BUKRS EQ TL_BKPF-BUKRS.
            T_ZGL012_AUX-DELE = 'X'.
            MODIFY T_ZGL012_AUX INDEX SY-TABIX TRANSPORTING DELE.
          ENDLOOP.
        ENDIF.
      ELSE.
        DELETE T_ZGL012_AUX WHERE BELNR EQ TL_BKPF-BELNR
                              AND BUKRS EQ TL_BKPF-BUKRS.
      ENDIF.

    ENDLOOP.

    SORT T_ZGL012_AUX BY BUKRS BELNR BUZEI DT_AVAL MOEDA_ATU.
    DELETE ADJACENT DUPLICATES FROM T_ZGL012_AUX COMPARING BUKRS BELNR BUZEI DT_AVAL MOEDA_ATU.
    SORT T_ZGL012_AUX BY BUKRS LIFNR BELNR BUZEI BUDAT MOEDA_ATU ASCENDING   DT_AVAL  DESCENDING.
  ENDIF.
  CLEAR L_UDATA.
ENDFORM.                    " ULTIMA_AVALIACAO_FOR

*&---------------------------------------------------------------------*
*&      Form  F_CLIENTE
*&---------------------------------------------------------------------*
*       Partidas em Aberto Cliente
*----------------------------------------------------------------------*
FORM F_CLIENTE .

  DATA: BEGIN OF T_BKPF00 OCCURS 0.
          INCLUDE STRUCTURE BKPF.
  DATA: END OF T_BKPF00.

  DATA: BEGIN OF T_BKPF0E OCCURS 0.
          INCLUDE STRUCTURE BKPF.
  DATA: END OF T_BKPF0E.


  DATA: BEGIN OF ITL_BSID OCCURS 0,
          BUKRS TYPE BSID-BUKRS,
          KUNNR TYPE BSID-KUNNR,
          UMSKZ TYPE BSID-UMSKZ,
          AUGBL TYPE BSID-AUGBL,
          GJAHR TYPE BSID-GJAHR,
          BELNR TYPE BSID-BELNR,
          BUZEI TYPE BSID-BUZEI,
          BUDAT TYPE BSID-BUDAT,
          WAERS TYPE BSID-WAERS,
          BLART TYPE BLART,
          BSCHL TYPE BSCHL,
          SHKZG TYPE SHKZG,
          GSBER TYPE GSBER,
          DMBTR TYPE BSID-DMBTR,
          WRBTR TYPE BSID-WRBTR,
          HKONT TYPE BSID-HKONT,
          DMBE2 TYPE BSID-DMBE2,
          AUGDT TYPE BSID-AUGDT,
        END OF ITL_BSID.

  DATA: BEGIN OF ITL_BKPF OCCURS 0,
          BUKRS TYPE BKPF-BUKRS,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          STGRD TYPE BKPF-STGRD,
          STBLG TYPE BKPF-STBLG,
          STJAH TYPE BKPF-STJAH,
        END OF ITL_BKPF.


  DATA: VG_FIRST TYPE DATUM.

  CONCATENATE P_BUDAT+0(6) '01' INTO VG_FIRST.


  "Abertos
  SELECT BUKRS KUNNR UMSKZ BELNR BUZEI BUDAT WAERS BLART BSCHL SHKZG GSBER DMBTR WRBTR HKONT DMBE2 GJAHR
      FROM BSID
      INTO TABLE T_BSID
      WHERE BUKRS EQ P_BUKRS
        AND BUDAT LE P_BUDAT
        AND KUNNR IN S_KUNNR
        AND HKONT IN S_DONT
        AND WAERS NE WA_T005-WAERS
        AND BLART NE 'VC'
        AND BELNR IN S_BELNR.

  IF SY-SUBRC = 0.

    SELECT * FROM BKPF INTO TABLE T_BKPF00
      FOR ALL ENTRIES IN T_BSID
      WHERE BUKRS = T_BSID-BUKRS AND
            BELNR = T_BSID-BELNR AND
            GJAHR = T_BSID-GJAHR AND
            BSTAT = 'S'.

    IF SY-SUBRC = 0.
      "ALRS 13.11.2014 Estorno dentro do mês não leva
      SELECT *
         FROM BKPF
         INTO TABLE T_BKPF0E
          FOR ALL ENTRIES IN T_BKPF00
           WHERE BUKRS EQ T_BKPF00-BUKRS
             AND BELNR EQ T_BKPF00-STBLG
             AND GJAHR EQ T_BKPF00-STJAH.

      LOOP AT T_BKPF00.
        READ TABLE T_BKPF0E WITH KEY BUKRS = T_BKPF00-BUKRS
                                   BELNR = T_BKPF00-STBLG
                                   GJAHR = T_BKPF00-STJAH.
        IF SY-SUBRC = 0.
          IF  T_BKPF0E-BUDAT LE P_BUDAT.
            READ TABLE T_BSID INTO WA_BSID WITH KEY BUKRS = T_BKPF00-BUKRS
                                     BELNR = T_BKPF00-BELNR
                                     GJAHR = T_BKPF00-GJAHR.
            IF SY-SUBRC = 0.
              DELETE T_BSID INDEX SY-TABIX.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE T_BSID INTO WA_BSID WITH KEY BUKRS = T_BKPF00-BUKRS
                                       BELNR = T_BKPF00-BELNR
                                       GJAHR = T_BKPF00-GJAHR.
          IF SY-SUBRC = 0.
            DELETE T_BSID INDEX SY-TABIX.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  "compensados depois da data
  SELECT BUKRS KUNNR UMSKZ AUGBL GJAHR BELNR BUZEI BUDAT WAERS BLART BSCHL SHKZG GSBER DMBTR WRBTR HKONT DMBE2 AUGDT
    FROM BSAD
    INTO TABLE  ITL_BSID
   WHERE BUKRS EQ P_BUKRS
     AND BUDAT LE P_BUDAT
     AND AUGDT GE VG_FIRST
     AND KUNNR IN S_KUNNR
     AND HKONT IN S_DONT
     AND WAERS NE WA_T005-WAERS
     AND BLART NE 'VC'
     AND BELNR IN S_BELNR
     AND DMBE2 GE WG_DMBT2.

  IF SY-SUBRC = 0.
    SELECT * FROM BKPF
      INTO TABLE T_BKPF00
       FOR ALL ENTRIES IN ITL_BSID
     WHERE BUKRS = ITL_BSID-BUKRS AND
           BELNR = ITL_BSID-BELNR AND
           GJAHR = ITL_BSID-GJAHR AND
           BSTAT = 'S'.

    IF SY-SUBRC = 0.
      "ALRS 13.11.2014 Estorno dentro do mês não leva
      SELECT *
         FROM BKPF
         INTO TABLE T_BKPF0E
          FOR ALL ENTRIES IN T_BKPF00
        WHERE BUKRS EQ T_BKPF00-BUKRS
          AND BELNR EQ T_BKPF00-STBLG
          AND GJAHR EQ T_BKPF00-STJAH.

      LOOP AT T_BKPF00.
        READ TABLE T_BKPF0E WITH KEY BUKRS = T_BKPF00-BUKRS
                                     BELNR = T_BKPF00-STBLG
                                     GJAHR = T_BKPF00-STJAH.
        IF SY-SUBRC = 0.
          IF  T_BKPF0E-BUDAT LE P_BUDAT.
            READ TABLE ITL_BSID WITH KEY BUKRS = T_BKPF00-BUKRS
                               BELNR = T_BKPF00-BELNR
                               GJAHR = T_BKPF00-GJAHR.
            IF SY-SUBRC = 0.
              DELETE ITL_BSID INDEX SY-TABIX.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE ITL_BSID WITH KEY BUKRS = T_BKPF00-BUKRS
                                       BELNR = T_BKPF00-BELNR
                                       GJAHR = T_BKPF00-GJAHR.
          IF SY-SUBRC = 0.
            DELETE ITL_BSID INDEX SY-TABIX.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.


  LOOP AT ITL_BSID.
    IF ITL_BSID-BELNR = ITL_BSID-AUGBL.
      DELETE ITL_BSID INDEX SY-TABIX.
      CONTINUE.
    ENDIF.

    IF ITL_BSID-BUDAT(6) = ITL_BSID-AUGDT(6).
      DELETE ITL_BSID INDEX SY-TABIX.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING ITL_BSID TO WA_BSID.
    IF ITL_BSID-AUGDT LE P_BUDAT.
      WA_BSID-ST_REV = 'S'.
    ENDIF.
    APPEND WA_BSID TO T_BSID.
    CLEAR WA_BSID.
  ENDLOOP.
  DELETE T_BSID WHERE BLART = 'VC'.

  PERFORM: CARREGA_T_ZGL_CLI.     "Carrega tabela do ALV

ENDFORM.                    " F_CLIENTE
*&---------------------------------------------------------------------*
*&      Form  CARREGA_T_ZGL_CLI
*&---------------------------------------------------------------------*
*       Carrega tabela aux. cliente
*----------------------------------------------------------------------*
FORM CARREGA_T_ZGL_CLI .

  DATA: W_LOOP  TYPE I,
        W_DMBTR TYPE ZGL012_AVM-DMBTR,
        W_MOEDA TYPE ZGL012_AVM-WAERS.

  PERFORM: ULTIMA_AVALIACAO_CLI.  "Trata ültima avaliação

  DELETE T_BSID WHERE DMBE2 EQ '0.01'
                   OR UMSKZ  EQ 'F'.

  LOOP AT T_BSID INTO WA_BSID.
    WA_ZGL012_AVM-MANDT   = SY-MANDT.
    WA_ZGL012_AVM-BUKRS   = WA_BSID-BUKRS.
    WA_ZGL012_AVM-DT_AVAL = P_BUDAT.
    WA_ZGL012_AVM-KUNNR   = WA_BSID-KUNNR.
    WA_ZGL012_AVM-BELNR   = WA_BSID-BELNR.
    WA_ZGL012_AVM-BUZEI   = WA_BSID-BUZEI.
    WA_ZGL012_AVM-BUDAT   = WA_BSID-BUDAT.
    WA_ZGL012_AVM-BSCHL   = WA_BSID-BSCHL.
    WA_ZGL012_AVM-WAERS   = WA_BSID-WAERS.
    WA_ZGL012_AVM-GSBER   = WA_BSID-GSBER.
    WA_ZGL012_AVM-ST_REV  = WA_BSID-ST_REV.
    WA_ZGL012_AVM-AUGBL   = WA_BSID-AUGBL.
    WA_ZGL012_AVM-AUGDT   = WA_BSID-AUGDT.

    IF WA_BSID-SHKZG = 'S'.
      WA_ZGL012_AVM-DMBTR   = WA_BSID-DMBTR.
      WA_ZGL012_AVM-DMBE2   = WA_BSID-DMBE2.
      WA_ZGL012_AVM-WRBTR   = WA_BSID-WRBTR.
    ELSE.
      WA_ZGL012_AVM-DMBTR   = WA_BSID-DMBTR * ( -1 ).
      WA_ZGL012_AVM-DMBE2   = WA_BSID-DMBE2 * ( -1 ).
      WA_ZGL012_AVM-WRBTR   = WA_BSID-WRBTR * ( -1 ).
    ENDIF.

    IF P_BUKRS = '0004' OR P_BUKRS = '0201' OR P_BUKRS = '0202' OR P_BUKRS = '0037'. "Elimina moeda dolar
      IF WA_ZGL012_AVM-WAERS = C_USD.
        CONTINUE.
      ENDIF.
    ENDIF.


    IF WG_CUR_P EQ 'PYG'.
      WA_ZGL012_AVM-DMBTR = WA_ZGL012_AVM-DMBTR * 100.
    ENDIF.

    IF WA_ZGL012_AVM-WAERS = C_USD.
      W_LOOP = 1.
    ELSE.
      W_LOOP = 2.
    ENDIF.

    DO W_LOOP TIMES.
      XTX_USD = 0.
      IF SY-INDEX = 1.
        W_MOEDA = ''.
        READ TABLE  T_TCURR INTO WA_TCURR WITH KEY FCURR = WA_ZGL012_AVM-WAERS.
        IF P_BUKRS = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WA_BSID-HKONT
            IMPORTING
              OUTPUT = WA_BSID-HKONT.
          IF WA_BSID-HKONT+0(1) = '1'. "ativos (compra)
            READ TABLE  T_TCURR_G INTO WA_TCURR WITH KEY FCURR = WA_ZGL012_AVM-WAERS.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_BSID-HKONT
            IMPORTING
              OUTPUT = WA_BSID-HKONT.
        ENDIF.
        IF SY-SUBRC = 0.
          XTX_USD = WA_TCURR-UKURS.
          WA_ZGL012_AVM-TX_FECH  = XTX_USD.
        ENDIF.

        IF XTX_USD LT 0.
          IF WA_BSID-DMBTR NE 0.
            WA_ZGL012_AVM-KURSF   = WA_BSID-WRBTR / WA_BSID-DMBTR.
          ENDIF.
        ELSE.
          IF WA_BSID-WRBTR NE 0.
            WA_ZGL012_AVM-KURSF   = WA_BSID-DMBTR / WA_BSID-WRBTR.
          ENDIF.
        ENDIF.
      ELSE. "ler o USD
        W_MOEDA = C_USD.
        READ TABLE  T_TCURR INTO WA_TCURR WITH KEY FCURR = C_USD.
        IF P_BUKRS = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WA_BSID-HKONT
            IMPORTING
              OUTPUT = WA_BSID-HKONT.
          IF WA_BSID-HKONT+0(1) = '1'. "ativos (compra)
            READ TABLE  T_TCURR_G INTO WA_TCURR WITH KEY FCURR = C_USD.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_BSID-HKONT
            IMPORTING
              OUTPUT = WA_BSID-HKONT.
        ENDIF.
        IF SY-SUBRC = 0.
          XTX_USD = WA_TCURR-UKURS.
          WA_ZGL012_AVM-TX_FECH  = XTX_USD.
        ENDIF.

        IF XTX_USD LT 0.
          IF WA_BSID-WRBTR NE 0.
            WA_ZGL012_AVM-KURSF   = WA_BSID-DMBE2 / WA_BSID-WRBTR.
          ENDIF.
        ELSE.
          IF WA_BSID-DMBE2 NE 0.
            WA_ZGL012_AVM-KURSF  = WA_BSID-WRBTR / WA_BSID-DMBE2.
          ENDIF.
        ENDIF.
      ENDIF.

      IF WA_ZGL012_AVM-TX_FECH LT 0.
        MULTIPLY WA_ZGL012_AVM-TX_FECH BY -1.
      ENDIF.

      IF SY-INDEX = 1.
        IF XTX_USD LT 0.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBTR - ( WA_ZGL012_AVM-WRBTR / WA_ZGL012_AVM-TX_FECH ) .
          WA_ZGL012_AVM-VLR_ATUALIZADO    = WA_ZGL012_AVM-WRBTR / WA_ZGL012_AVM-TX_FECH.
        ELSE.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBTR - ( WA_ZGL012_AVM-WRBTR * WA_ZGL012_AVM-TX_FECH ) .
          WA_ZGL012_AVM-VLR_ATUALIZADO    = WA_ZGL012_AVM-WRBTR * WA_ZGL012_AVM-TX_FECH.
        ENDIF.
        W_DMBTR = WA_ZGL012_AVM-VLR_ATUALIZADO.
        IF P_BUKRS = '0201' OR P_BUKRS = '0202'.
          CONTINUE.
        ENDIF.
      ELSE. "USD
*        IF wa_zgl012_avm-bukrs = '0201'.
*          MULTIPLY xtx_usd BY -1.
*        ENDIF.
        IF XTX_USD LT 0.
          WA_ZGL012_AVM-VLR_ATUALIZADO    =  W_DMBTR  * WA_ZGL012_AVM-TX_FECH.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBE2 - WA_ZGL012_AVM-VLR_ATUALIZADO .
        ELSE.
          WA_ZGL012_AVM-VLR_ATUALIZADO    =  W_DMBTR / WA_ZGL012_AVM-TX_FECH.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBE2 -   WA_ZGL012_AVM-VLR_ATUALIZADO.
        ENDIF.
      ENDIF.

      WA_ZGL012_AVM-HKONT   = WA_BSID-HKONT.
      WA_ZGL012_AVM-UMSKZ   = WA_BSID-UMSKZ.
*Grava valor acumulado do mês anterior.
      READ TABLE T_ZGL012_AUX INTO WA_ZGL012_AUX WITH KEY BUKRS   =  WA_ZGL012_AVM-BUKRS
                                                          KUNNR   =  WA_ZGL012_AVM-KUNNR
                                                          BELNR   =  WA_ZGL012_AVM-BELNR
                                                          BUZEI   =  WA_ZGL012_AVM-BUZEI
                                                          BUDAT   =  WA_ZGL012_AVM-BUDAT
                                                          MOEDA_ATU =  W_MOEDA.
      IF SY-SUBRC = 0 .
        IF WA_ZGL012_AUX-DOC_REV IS INITIAL.
          WA_ZGL012_AVM-VLR_ACUM_MES_ANT = WA_ZGL012_AUX-VLR_ACUM_MES_ATU.
        ELSE.
          WA_ZGL012_AVM-VLR_ACUM_MES_ANT = 0.
        ENDIF.
      ELSE.
        WA_ZGL012_AVM-VLR_ACUM_MES_ANT = 0.
      ENDIF.

      WA_ZGL012_AVM-VLR_VARIACAO = WA_ZGL012_AVM-VLR_ACUM_MES_ATU - WA_ZGL012_AVM-VLR_ACUM_MES_ANT.
      WA_ZGL012_AVM-MOEDA_ATU = W_MOEDA.
*Grava documentos compensados
      APPEND WA_ZGL012_AVM TO T_ZGL012_AVM.
    ENDDO.
  ENDLOOP.
ENDFORM.                    " CARREGA_T_ZGL_CLI
*&---------------------------------------------------------------------*
*&      Form  ULTIMA_AVALIACAO_CLI
*&---------------------------------------------------------------------*
*       Última data gravada
*----------------------------------------------------------------------*
FORM ULTIMA_AVALIACAO_CLI .
*Define última data de avaliação gravada.
  DATA: L_UDATA TYPE DATUM,
        L_PDATA TYPE DATUM.

  DATA: TL_BKPF  TYPE TABLE OF BKPF WITH HEADER LINE,
        TL_BKPFE TYPE TABLE OF BKPF WITH HEADER LINE.

  L_UDATA = P_BUDAT.
  L_UDATA+6(2) = '01'.   "tranforma em primeiro dia do mês corrente
  L_UDATA = L_UDATA - 1. "transforma no último dia do mês anterior

  L_PDATA = L_UDATA - 120.


  CHECK T_BSID[] IS NOT INITIAL.

  SELECT * INTO TABLE T_ZGL012_AUX
    FROM ZGL012_AVM
    FOR ALL ENTRIES IN T_BSID
   WHERE
      BUKRS   EQ T_BSID-BUKRS  AND
      BELNR   EQ T_BSID-BELNR  AND
      BUZEI   EQ T_BSID-BUZEI  AND
      DT_AVAL GE L_PDATA       AND
      DT_AVAL LE  L_UDATA      AND
      ESTORNO NE 'X'           AND
      KUNNR   IN S_KUNNR       AND
      HKONT   IN S_DONT.

  IF SY-SUBRC = 0.
    SELECT *
      FROM BKPF
      INTO TABLE TL_BKPF
       FOR ALL ENTRIES IN T_ZGL012_AUX
     WHERE BUKRS EQ T_ZGL012_AUX-BUKRS
       AND BELNR EQ T_ZGL012_AUX-BELNR
       AND STBLG NE SPACE.

    "ALRS 13.11.2014 Estorno dentro do mês não leva
    IF TL_BKPF[] IS NOT INITIAL.
      SELECT *
        FROM BKPF
        INTO TABLE TL_BKPFE
         FOR ALL ENTRIES IN TL_BKPF
       WHERE BUKRS EQ TL_BKPF-BUKRS
         AND BELNR EQ TL_BKPF-STBLG
         AND GJAHR EQ TL_BKPF-STJAH .

    ENDIF.

    LOOP AT TL_BKPF.
      READ TABLE TL_BKPFE WITH KEY BUKRS = TL_BKPF-BUKRS
                                   BELNR = TL_BKPF-STBLG
                                   GJAHR = TL_BKPF-STJAH.
      IF SY-SUBRC = 0.
        IF  TL_BKPFE-BUDAT LE P_BUDAT.
          LOOP AT T_ZGL012_AUX WHERE BELNR EQ TL_BKPF-BELNR
                               AND   BUKRS EQ TL_BKPF-BUKRS.
            T_ZGL012_AUX-DELE = 'X'.
            MODIFY T_ZGL012_AUX INDEX SY-TABIX TRANSPORTING DELE.

          ENDLOOP.
        ENDIF.
      ELSE.
        DELETE T_ZGL012_AUX WHERE BELNR EQ TL_BKPF-BELNR
                              AND BUKRS EQ TL_BKPF-BUKRS.

      ENDIF.

    ENDLOOP.

    SORT T_ZGL012_AUX BY BUKRS BELNR BUZEI DT_AVAL MOEDA_ATU .
    DELETE ADJACENT DUPLICATES FROM T_ZGL012_AUX COMPARING BUKRS BELNR BUZEI DT_AVAL MOEDA_ATU.
    SORT T_ZGL012_AUX BY BUKRS KUNNR BELNR BUZEI BUDAT MOEDA_ATU ASCENDING   DT_AVAL  DESCENDING.

  ENDIF.

  CLEAR L_UDATA.

ENDFORM.                    " ULTIMA_AVALIACAO_CLI

*&---------------------------------------------------------------------*
*&      Form  F_CONTA
*&---------------------------------------------------------------------*
*       Partidas em Aberto Razão
*----------------------------------------------------------------------*
FORM F_CONTA .

  DATA: BEGIN OF T_BKPF000 OCCURS 0.
          INCLUDE STRUCTURE BKPF.
  DATA: END OF T_BKPF000.

  DATA: BEGIN OF T_BKPF0E OCCURS 0.
          INCLUDE STRUCTURE BKPF.
  DATA: END OF T_BKPF0E.

  DATA: BEGIN OF ITL_BSIS OCCURS 0,
          BUKRS TYPE BSIS-BUKRS,
          HKONT TYPE BSIS-HKONT,
          AUGBL TYPE BSIS-AUGBL,
          GJAHR TYPE BSIS-GJAHR,
          BELNR TYPE BSIS-BELNR,
          BUZEI TYPE BSIS-BUZEI,
          BUDAT TYPE BSIS-BUDAT,
          WAERS TYPE BSIS-WAERS,
          BLART TYPE BLART,
          BSCHL TYPE BSCHL,
          SHKZG TYPE SHKZG,
          GSBER TYPE GSBER,
          DMBTR TYPE BSIS-DMBTR,
          WRBTR TYPE BSIS-WRBTR,
          DMBE2 TYPE BSIS-DMBE2,
          AUGDT LIKE BSIS-AUGDT,
        END OF ITL_BSIS.


  DATA: BEGIN OF ITL_BKPF OCCURS 0,
          BUKRS TYPE BKPF-BUKRS,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          STGRD TYPE BKPF-STGRD,
          STBLG TYPE BKPF-STBLG,
          STJAH TYPE BKPF-STJAH,
        END OF ITL_BKPF.


  DATA: VG_FIRST TYPE DATUM.

  CONCATENATE P_BUDAT+0(6) '01' INTO VG_FIRST.

  "Abertos
  SELECT BUKRS SAKNR XOPVW             "#EC CI_DB_OPERATION_OK[2431747]
    FROM SKB1
    INTO TABLE T_SKB1
   WHERE BUKRS = P_BUKRS
     AND SAKNR IN S_HKONT
     AND XOPVW EQ 'X'.
  IF SY-SUBRC = 0.
    SELECT BUKRS HKONT BELNR BUZEI BUDAT WAERS BLART BSCHL SHKZG GSBER DMBTR WRBTR DMBE2 GJAHR
      INTO TABLE T_BSIS
      FROM BSIS
       FOR ALL ENTRIES IN T_SKB1
      WHERE BUKRS EQ T_SKB1-BUKRS
        AND HKONT EQ T_SKB1-SAKNR
        AND BUDAT LE P_BUDAT
        AND WAERS NE WA_T005-WAERS
        AND BLART NE 'VC'
        AND BELNR IN S_BELNR.

    IF SY-SUBRC = 0.
      SELECT *
        FROM BKPF
        INTO TABLE T_BKPF000
         FOR ALL ENTRIES IN T_BSIS
       WHERE BUKRS = T_BSIS-BUKRS AND
             BELNR = T_BSIS-BELNR AND
             GJAHR = T_BSIS-GJAHR AND
             BSTAT = 'S'.

      IF SY-SUBRC = 0.
        "ALRS 13.11.2014 Estorno dentro do mês não leva
        SELECT *
           FROM BKPF
           INTO TABLE T_BKPF0E
            FOR ALL ENTRIES IN T_BKPF000
             WHERE BUKRS EQ T_BKPF000-BUKRS
               AND BELNR EQ T_BKPF000-STBLG
               AND GJAHR EQ T_BKPF000-STJAH.

        LOOP AT T_BKPF000.
          READ TABLE T_BKPF0E WITH KEY BUKRS = T_BKPF000-BUKRS
                                       BELNR = T_BKPF000-STBLG
                                       GJAHR = T_BKPF000-STJAH.
          IF SY-SUBRC = 0.
            IF  T_BKPF0E-BUDAT LE P_BUDAT.
              READ TABLE T_BSIS INTO WA_BSIS WITH KEY BUKRS = T_BKPF000-BUKRS
                                                    BELNR = T_BKPF000-BELNR
                                                    GJAHR = T_BKPF000-GJAHR.
              IF SY-SUBRC = 0.
                DELETE T_BSIS INDEX SY-TABIX.
              ENDIF.
            ENDIF.
          ELSE.
            READ TABLE T_BSIS INTO WA_BSIS WITH KEY BUKRS = T_BKPF000-BUKRS
                                                    BELNR = T_BKPF000-BELNR
                                                    GJAHR = T_BKPF000-GJAHR.
            IF SY-SUBRC = 0.
              DELETE T_BSIS INDEX SY-TABIX.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    "compensados no mês
    IF T_SKB1[] IS NOT INITIAL.
      SELECT BUKRS HKONT AUGBL GJAHR BELNR BUZEI BUDAT WAERS
             BLART BSCHL SHKZG GSBER DMBTR WRBTR DMBE2 AUGDT
        FROM BSAS
        INTO TABLE ITL_BSIS
        FOR ALL ENTRIES IN T_SKB1
        WHERE BUKRS EQ T_SKB1-BUKRS
          AND HKONT EQ T_SKB1-SAKNR
          AND BUDAT LE P_BUDAT
          AND WAERS NE WA_T005-WAERS
          AND BLART NE 'VC'
          AND AUGDT GE VG_FIRST
          AND BELNR IN S_BELNR.


    ENDIF.

    IF NOT ITL_BSIS[] IS INITIAL.
      SELECT * FROM BKPF INTO TABLE T_BKPF000
        FOR ALL ENTRIES IN ITL_BSIS
        WHERE BUKRS = ITL_BSIS-BUKRS AND
              BELNR = ITL_BSIS-BELNR AND
              GJAHR = ITL_BSIS-GJAHR AND
              BSTAT = 'S'.

      IF SY-SUBRC = 0.
        "ALRS 13.11.2014 Estorno dentro do mês não leva
        SELECT *
           FROM BKPF
           INTO TABLE T_BKPF0E
            FOR ALL ENTRIES IN T_BKPF000
             WHERE BUKRS EQ T_BKPF000-BUKRS
               AND BELNR EQ T_BKPF000-STBLG
               AND GJAHR EQ T_BKPF000-STJAH.

        LOOP AT T_BKPF000.
          READ TABLE T_BKPF0E WITH KEY BUKRS = T_BKPF000-BUKRS
                                       BELNR = T_BKPF000-STBLG
                                       GJAHR = T_BKPF000-STJAH.
          IF SY-SUBRC = 0.
            IF  T_BKPF0E-BUDAT LE P_BUDAT.
              READ TABLE ITL_BSIS WITH KEY BUKRS = T_BKPF000-BUKRS
                                           BELNR = T_BKPF000-BELNR
                                           GJAHR = T_BKPF000-GJAHR.
              IF SY-SUBRC = 0.
                DELETE ITL_BSIS INDEX SY-TABIX.
              ENDIF.
            ENDIF.
          ELSE.
            READ TABLE ITL_BSIS WITH KEY BUKRS = T_BKPF000-BUKRS
                                         BELNR = T_BKPF000-BELNR
                                         GJAHR = T_BKPF000-GJAHR.
            IF SY-SUBRC = 0.
              DELETE ITL_BSIS INDEX SY-TABIX.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

    LOOP AT ITL_BSIS.
      IF ITL_BSIS-BUDAT(6) = ITL_BSIS-AUGDT(6).
        DELETE ITL_BSIS INDEX SY-TABIX.
        CONTINUE.
      ENDIF.

      IF ITL_BSIS-BELNR = ITL_BSIS-AUGBL.
        DELETE ITL_BSIS INDEX SY-TABIX.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING ITL_BSIS TO WA_BSIS.
      IF ITL_BSIS-AUGDT LE P_BUDAT.
        WA_BSIS-ST_REV = 'S'.
      ENDIF.
      APPEND WA_BSIS TO T_BSIS.
      CLEAR WA_BSIS.

    ENDLOOP.

    DELETE T_BSIS WHERE BLART = 'VC'.

    PERFORM: CARREGA_T_ZGL_RAZ.     "Carrega tabela do ALV

  ENDIF.
ENDFORM.                    " F_CONTA
*&---------------------------------------------------------------------*
*&      Form  CARREGA_T_ZGL_RAZ
*&---------------------------------------------------------------------*
*       Carrega tabela aux. Razão
*----------------------------------------------------------------------*
FORM CARREGA_T_ZGL_RAZ .

  DATA: W_LOOP  TYPE I,
        W_DMBTR TYPE ZGL012_AVM-DMBTR,
        W_MOEDA TYPE ZGL012_AVM-WAERS.


  PERFORM: ULTIMA_AVALIACAO_RAZ.

  DELETE T_BSIS WHERE DMBE2 EQ '0.01'.

  LOOP AT T_BSIS INTO WA_BSIS.
    WA_ZGL012_AVM-MANDT   = SY-MANDT.
    WA_ZGL012_AVM-BUKRS   = WA_BSIS-BUKRS.
    WA_ZGL012_AVM-DT_AVAL = P_BUDAT.
    WA_ZGL012_AVM-HKONT   = WA_BSIS-HKONT.
    WA_ZGL012_AVM-BELNR   = WA_BSIS-BELNR.
    WA_ZGL012_AVM-BUZEI   = WA_BSIS-BUZEI.
    WA_ZGL012_AVM-BUDAT   = WA_BSIS-BUDAT.
    WA_ZGL012_AVM-BSCHL   = WA_BSIS-BSCHL.
    WA_ZGL012_AVM-WAERS   = WA_BSIS-WAERS.
    WA_ZGL012_AVM-GSBER   = WA_BSIS-GSBER.
    WA_ZGL012_AVM-ST_REV  = WA_BSIS-ST_REV.
    WA_ZGL012_AVM-AUGBL   = WA_BSIS-AUGBL.
    WA_ZGL012_AVM-AUGDT   = WA_BSIS-AUGDT.

    IF WA_BSIS-SHKZG = 'S'.
      WA_ZGL012_AVM-DMBTR   = WA_BSIS-DMBTR.
      WA_ZGL012_AVM-DMBE2   = WA_BSIS-DMBE2.
      WA_ZGL012_AVM-WRBTR   = WA_BSIS-WRBTR.
    ELSE.
      WA_ZGL012_AVM-DMBTR   = WA_BSIS-DMBTR * ( -1 ).
      WA_ZGL012_AVM-DMBE2   = WA_BSIS-DMBE2 * ( -1 ).
      WA_ZGL012_AVM-WRBTR   = WA_BSIS-WRBTR * ( -1 ).
    ENDIF.

    IF P_BUKRS = '0004' OR P_BUKRS = '0201' OR P_BUKRS = '0202' OR P_BUKRS = '0037'.. "Elimina moeda dolar
      IF WA_ZGL012_AVM-WAERS = C_USD.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF WG_CUR_P EQ 'PYG'.
      WA_ZGL012_AVM-DMBTR = WA_ZGL012_AVM-DMBTR * 100.
    ENDIF.

    IF WA_ZGL012_AVM-WAERS = C_USD.
      W_LOOP = 1.
    ELSE.
      W_LOOP = 2.
    ENDIF.

    DO W_LOOP TIMES.
      XTX_USD = 0.
      IF SY-INDEX = 1.
        W_MOEDA = ''.
        READ TABLE  T_TCURR INTO WA_TCURR WITH KEY FCURR = WA_ZGL012_AVM-WAERS.
        IF P_BUKRS = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WA_BSIS-HKONT
            IMPORTING
              OUTPUT = WA_BSIS-HKONT.
          IF WA_BSIS-HKONT+0(1) = '1'. "ativos (compra)
            READ TABLE  T_TCURR_G INTO WA_TCURR WITH KEY FCURR = WA_ZGL012_AVM-WAERS.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_BSIS-HKONT
            IMPORTING
              OUTPUT = WA_BSIS-HKONT.
        ENDIF.
        IF SY-SUBRC = 0.
          XTX_USD = WA_TCURR-UKURS.
          WA_ZGL012_AVM-TX_FECH  = XTX_USD.
        ENDIF.

        IF XTX_USD LT 0.
          IF WA_BSIS-DMBTR NE 0.
            WA_ZGL012_AVM-KURSF   = WA_BSIS-WRBTR / WA_BSIS-DMBTR.
          ENDIF.
        ELSE.
          IF WA_BSIS-WRBTR NE 0.
            WA_ZGL012_AVM-KURSF   = WA_BSIS-DMBTR / WA_BSIS-WRBTR.
          ENDIF.
        ENDIF.
      ELSE. "ler o USD
        W_MOEDA = C_USD.
        READ TABLE  T_TCURR INTO WA_TCURR WITH KEY FCURR = C_USD.
        IF P_BUKRS = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WA_BSIS-HKONT
            IMPORTING
              OUTPUT = WA_BSIS-HKONT.
          IF WA_BSIS-HKONT+0(1) = '1'. "ativos (compra)
            READ TABLE  T_TCURR_G INTO WA_TCURR WITH KEY FCURR = C_USD.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_BSIS-HKONT
            IMPORTING
              OUTPUT = WA_BSIS-HKONT.
        ENDIF.
        IF SY-SUBRC = 0.
          XTX_USD = WA_TCURR-UKURS.
          WA_ZGL012_AVM-TX_FECH  = XTX_USD.
        ENDIF.

        IF XTX_USD LT 0.
          IF WA_BSIS-WRBTR NE 0.
            WA_ZGL012_AVM-KURSF   = WA_BSIS-DMBE2 / WA_BSIS-WRBTR.
          ENDIF.
        ELSE.
          IF WA_BSIS-DMBE2 NE 0.
            WA_ZGL012_AVM-KURSF  = WA_BSIS-WRBTR / WA_BSIS-DMBE2.
          ENDIF.
        ENDIF.
      ENDIF.
      IF XTX_USD LT 0.
        MULTIPLY WA_ZGL012_AVM-TX_FECH BY -1.
      ENDIF.

      IF SY-INDEX = 1.
        IF XTX_USD LT 0.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBTR - ( WA_ZGL012_AVM-WRBTR / WA_ZGL012_AVM-TX_FECH ) .
          WA_ZGL012_AVM-VLR_ATUALIZADO    = WA_ZGL012_AVM-WRBTR / WA_ZGL012_AVM-TX_FECH.
        ELSE.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBTR - ( WA_ZGL012_AVM-WRBTR * WA_ZGL012_AVM-TX_FECH ) .
          WA_ZGL012_AVM-VLR_ATUALIZADO    = WA_ZGL012_AVM-WRBTR * WA_ZGL012_AVM-TX_FECH.
        ENDIF.
        W_DMBTR = WA_ZGL012_AVM-VLR_ATUALIZADO.
        IF P_BUKRS = '0201' OR P_BUKRS = '0202'.
          CONTINUE.
        ENDIF.
      ELSE. "USD
*        IF wa_zgl012_avm-bukrs = '0201'.
*          MULTIPLY xtx_usd BY -1.
*        ENDIF.
        IF XTX_USD LT 0.
          WA_ZGL012_AVM-VLR_ATUALIZADO    =  W_DMBTR  * WA_ZGL012_AVM-TX_FECH.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBE2 - WA_ZGL012_AVM-VLR_ATUALIZADO .
        ELSE.
          WA_ZGL012_AVM-VLR_ATUALIZADO    =  W_DMBTR / WA_ZGL012_AVM-TX_FECH.
          WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = WA_ZGL012_AVM-DMBE2 -   WA_ZGL012_AVM-VLR_ATUALIZADO.
        ENDIF.
      ENDIF.

*Grava valor acumulado do mês anterior.
      READ TABLE T_ZGL012_AUX INTO WA_ZGL012_AUX WITH KEY BUKRS   =  WA_ZGL012_AVM-BUKRS
                                                          HKONT   =  WA_ZGL012_AVM-HKONT
                                                          BELNR   =  WA_ZGL012_AVM-BELNR
                                                          BUZEI   =  WA_ZGL012_AVM-BUZEI
                                                          BUDAT   =  WA_ZGL012_AVM-BUDAT
                                                          MOEDA_ATU =  W_MOEDA.
      IF SY-SUBRC = 0 .
        IF WA_ZGL012_AUX-DOC_REV IS INITIAL.
          WA_ZGL012_AVM-VLR_ACUM_MES_ANT = WA_ZGL012_AUX-VLR_ACUM_MES_ATU.
        ELSE.
          WA_ZGL012_AVM-VLR_ACUM_MES_ANT = 0.
        ENDIF.
      ELSE.
        WA_ZGL012_AVM-VLR_ACUM_MES_ANT = 0.
      ENDIF.

      WA_ZGL012_AVM-VLR_VARIACAO = WA_ZGL012_AVM-VLR_ACUM_MES_ATU - WA_ZGL012_AVM-VLR_ACUM_MES_ANT.
      WA_ZGL012_AVM-MOEDA_ATU = W_MOEDA.
*Grava documentos compensados
      IF NOT WA_ZGL012_AVM-BUDAT+4(4) = WA_ZGL012_AVM-AUGDT+4(4).
        APPEND WA_ZGL012_AVM TO T_ZGL012_AVM.
      ENDIF.
    ENDDO.
  ENDLOOP.


ENDFORM.                    " CARREGA_T_ZGL_RAZ
*&---------------------------------------------------------------------*
*&      Form  ULTIMA_AVALIACAO_RAZ
*&---------------------------------------------------------------------*
*       Última data gravada
*----------------------------------------------------------------------*
FORM ULTIMA_AVALIACAO_RAZ .
*Define última data de avaliação gravada.
  DATA: L_UDATA TYPE DATUM,
        L_PDATA TYPE DATUM.

  DATA: TL_BKPF  TYPE TABLE OF BKPF WITH HEADER LINE,
        TL_BKPFE TYPE TABLE OF BKPF WITH HEADER LINE.

  L_UDATA = P_BUDAT.
  L_UDATA+6(2) = '01'.   "tranforma em primeiro dia do mês corrente
  L_UDATA = L_UDATA - 1. "transforma no último dia do mês anterior

  L_PDATA = L_UDATA - 120.

  CHECK T_BSIS[] IS NOT INITIAL.

  SELECT * INTO TABLE T_ZGL012_AUX  FROM ZGL012_AVM
    FOR ALL ENTRIES IN T_BSIS
    WHERE
      BUKRS   EQ T_BSIS-BUKRS  AND
      BELNR   EQ T_BSIS-BELNR  AND
      BUZEI   EQ T_BSIS-BUZEI  AND
      DT_AVAL GE L_PDATA       AND
      DT_AVAL LE L_UDATA       AND
      ESTORNO NE 'X'           AND
      HKONT   IN S_HKONT .

  IF SY-SUBRC = 0.
    SELECT *
      FROM BKPF
      INTO TABLE TL_BKPF
       FOR ALL ENTRIES IN T_ZGL012_AUX
     WHERE BUKRS EQ T_ZGL012_AUX-BUKRS
       AND BELNR EQ T_ZGL012_AUX-BELNR
       AND STBLG NE SPACE.

    "ALRS 13.11.2014 Estorno dentro do mês não leva
    IF TL_BKPF[] IS NOT INITIAL.
      SELECT *
        FROM BKPF
        INTO TABLE TL_BKPFE
         FOR ALL ENTRIES IN TL_BKPF
       WHERE BUKRS EQ TL_BKPF-BUKRS
         AND BELNR EQ TL_BKPF-STBLG
         AND GJAHR EQ TL_BKPF-STJAH .

    ENDIF.

    LOOP AT TL_BKPF.
      READ TABLE TL_BKPFE WITH KEY BUKRS = TL_BKPF-BUKRS
                                   BELNR = TL_BKPF-STBLG
                                   GJAHR = TL_BKPF-STJAH.
      IF SY-SUBRC = 0.
        IF  TL_BKPFE-BUDAT LE P_BUDAT.
          LOOP AT T_ZGL012_AUX WHERE BELNR EQ TL_BKPF-BELNR
                               AND   BUKRS EQ TL_BKPF-BUKRS.
            T_ZGL012_AUX-DELE = 'X'.

            MODIFY T_ZGL012_AUX INDEX SY-TABIX TRANSPORTING DELE.

          ENDLOOP.

        ENDIF.

      ELSE.
        DELETE T_ZGL012_AUX WHERE BELNR EQ TL_BKPF-BELNR
                              AND BUKRS EQ TL_BKPF-BUKRS.
      ENDIF.

    ENDLOOP.

    SORT T_ZGL012_AUX BY BUKRS BELNR BUZEI DT_AVAL MOEDA_ATU.
    DELETE ADJACENT DUPLICATES FROM T_ZGL012_AUX COMPARING BUKRS BELNR BUZEI DT_AVAL MOEDA_ATU.

    SORT T_ZGL012_AUX BY BUKRS HKONT BELNR BUZEI BUDAT MOEDA_ATU ASCENDING DT_AVAL DESCENDING.
  ENDIF.

  CLEAR L_UDATA.

ENDFORM.                    " ULTIMA_AVALIACAO_RAZ

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*      Apresentação do Relatório
*----------------------------------------------------------------------*
FORM F_ALV .
  IF P_HKONT2 = 'X'.
    PERFORM:
             F_ALV_FIELDCAT2.
    CALL SCREEN 9002.
  ELSE.
    PERFORM:
             F_ALV_FIELDCAT,
             F_ALV_TRANSF,
             F_ALV_IMPRIME,
             ATUALIZA_TELA .
  ENDIF.
ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  F_ALV_HEADER
*&---------------------------------------------------------------------*
*       Cabeçalho
*----------------------------------------------------------------------*
FORM ZF_ALV_HEADER  USING P_TIPO.
  DATA: WL_DATA(10),
        WL_HORA(8),
        WL_LINHA(80),
        WL_TEXT      TYPE SDYDO_TEXT_ELEMENT,
        WL_DOLAR(15),
        WL_EURO(15),
        WL_BUTXT     TYPE BUTXT,
        WL_CONT      TYPE SY-TABIX,
        WL_LINE      TYPE SY-TABIX,
        WL_DIV       TYPE SY-TABIX.

  IF P_TIPO = '1'.
    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = TEXT-010
        SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

    SELECT SINGLE BUTXT FROM T001
      INTO WL_BUTXT
      WHERE BUKRS = P_BUKRS.

    IF SY-SUBRC = 0.
      MOVE WL_BUTXT TO WL_LINHA.
    ELSE.
      MOVE 'N/a' TO WL_LINHA.
    ENDIF.

    CONCATENATE  TEXT-009 P_BUKRS ' - ' WL_LINHA INTO WL_LINHA SEPARATED BY SPACE.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.

    WRITE SY-UZEIT TO WL_HORA.
    WRITE SY-DATUM TO WL_DATA.

    CONCATENATE TEXT-011 WL_DATA TEXT-012 WL_HORA INTO WL_LINHA SEPARATED BY SPACE.

    WL_TEXT = WL_LINHA.

    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.

    WRITE P_BUDAT TO WL_DATA.
    CONCATENATE TEXT-013 WL_DATA INTO WL_LINHA.
    WL_TEXT = WL_LINHA.

    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.


    WL_CONT = 0.
    CLEAR  WL_LINHA.
    IF P_BUKRS = '0004' OR P_BUKRS = '0201' OR P_BUKRS = '0202' OR P_BUKRS = '0037'.
      WL_LINE = LINES( T_TCURR_A ).
      SORT T_TCURR_A BY GDATU ASCENDING FCURR DESCENDING .
      LOOP AT T_TCURR_A INTO WA_TCURR.
        WL_CONT = SY-TABIX.
        XTX_USD = WA_TCURR-UKURS.
        WRITE XTX_USD  TO WL_DOLAR.
        CONDENSE WL_DOLAR NO-GAPS.

        CONCATENATE  WL_LINHA TEXT-014 WA_TCURR-FCURR ':' WL_DOLAR INTO  WL_LINHA SEPARATED BY SPACE.

        WL_DIV  = WL_CONT MOD 3.
        IF WL_DIV EQ 0 OR WL_LINE EQ WL_CONT.
          WL_TEXT = WL_LINHA.

          CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

          CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
            EXPORTING
              TEXT         = WL_TEXT
              SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.

          CLEAR  WL_LINHA.
        ENDIF.
      ENDLOOP.
    ELSE.
      WL_LINE = LINES( T_TCURR ).
      SORT T_TCURR BY GDATU ASCENDING FCURR DESCENDING .
      LOOP AT T_TCURR INTO WA_TCURR.
        WL_CONT = SY-TABIX.
        XTX_USD = WA_TCURR-UKURS.
        WRITE XTX_USD  TO WL_DOLAR.
        CONDENSE WL_DOLAR NO-GAPS.

        CONCATENATE  WL_LINHA TEXT-014 WA_TCURR-FCURR ':' WL_DOLAR INTO  WL_LINHA SEPARATED BY SPACE.

        WL_DIV  = WL_CONT MOD 3.
        IF WL_DIV EQ 0 OR WL_LINE EQ WL_CONT.
          WL_TEXT = WL_LINHA.

          CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

          CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
            EXPORTING
              TEXT         = WL_TEXT
              SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.

          CLEAR  WL_LINHA.
        ENDIF.
      ENDLOOP.
    ENDIF.
    SORT T_TCURR BY GDATU ASCENDING FCURR ASCENDING .
  ELSE.
    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.

  ENDIF.

ENDFORM.                    " F_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       Características dos Campos
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .
  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'O_ALV'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TIPO'.
  WA_AFIELD-SCRTEXT_S = TEXT-A01.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 10.
  WA_AFIELD-KEY           = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*STATUS
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'STATUS'.
  WA_AFIELD-ICON          = 'X'.
  WA_AFIELD-HOTSPOT  = 'X'.
  WA_AFIELD-SCRTEXT_S = TEXT-A02.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 06.
  WA_AFIELD-KEY           = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.
  CLEAR WA_AFIELD-ICON.

*Código
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CODIGO'.
  WA_AFIELD-SCRTEXT_S = TEXT-A03.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-REF_FIELD = 'KUNNR'.
  WA_AFIELD-REF_TABLE = 'BSID'.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-KEY           = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.
  CLEAR:   WA_AFIELD-REF_FIELD ,  WA_AFIELD-REF_TABLE.

*Descrição
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DESCRICAO'.
  WA_AFIELD-SCRTEXT_S = TEXT-A04.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 25.
  WA_AFIELD-KEY           = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*NR DOC
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BELNR'.
  WA_AFIELD-SCRTEXT_S = TEXT-A05.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-HOTSPOT   = 'X'.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 12.
  WA_AFIELD-KEY           = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.
  CLEAR WA_AFIELD-HOTSPOT.

*Chave de lançamento
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BSCHL'.
  WA_AFIELD-SCRTEXT_S = TEXT-A06.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 08.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*CONTA
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'HKONT'.
  WA_AFIELD-SCRTEXT_S = TEXT-A07.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-REF_FIELD = 'HKONT'.
  WA_AFIELD-REF_TABLE = 'BSIS'.
  WA_AFIELD-KEY           = ''.

  APPEND WA_AFIELD TO IT_FIELDCAT.
  CLEAR: WA_AFIELD-REF_FIELD, WA_AFIELD-REF_TABLE.
*NRO. DOC.
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'UMSKZ'.
  WA_AFIELD-SCRTEXT_S =  TEXT-A08.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 08.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*DT lANCAMENTO
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BUDAT'.
  WA_AFIELD-SCRTEXT_S = TEXT-A09.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*Moeda documento
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'WAERS'.
  WA_AFIELD-SCRTEXT_S = TEXT-A10.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-DO_SUM        = ''.
  WA_AFIELD-OUTPUTLEN     = 08.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*moeda de atualização
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MOEDA_ATU'.
  WA_AFIELD-SCRTEXT_S = TEXT-A38.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 15.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*VALOR DOC
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'WRBTR'.
  WA_AFIELD-SCRTEXT_M = TEXT-A16.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-DO_SUM        = ''.
  WA_AFIELD-OUTPUTLEN     = 18.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*TX. CÂMBIO
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'KURSF'.
  WA_AFIELD-SCRTEXT_S = TEXT-A17.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
*  WA_AFIELD-REF_FIELDNAME = ''.
*  WA_AFIELD-REF_TABNAME   = ''.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-DO_SUM        = ''.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*Valor interno
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DMBTR'.
  WA_AFIELD-SCRTEXT_M = TEXT-A14.

  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY       = ' '.
  WA_AFIELD-DO_SUM    = ''.
  WA_AFIELD-OUTPUTLEN = 18.
  APPEND WA_AFIELD TO IT_FIELDCAT.


*VAlor dolar
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'DMBE2'.
  WA_AFIELD-SCRTEXT_M = TEXT-A15.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY       = ' '.
  WA_AFIELD-DO_SUM    = ''.
  WA_AFIELD-OUTPUTLEN = 18.
  APPEND WA_AFIELD TO IT_FIELDCAT.



*VALOR ATUALIZADO.
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_ATUALIZADO'.
  WA_AFIELD-SCRTEXT_M = TEXT-A18.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-OUTPUTLEN     = 18.
  WA_AFIELD-KEY           = ' '.
  APPEND WA_AFIELD TO IT_FIELDCAT.


*ACUM. MÊS ANT.
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_ACUM_MES_ANT'.
  WA_AFIELD-SCRTEXT_M = TEXT-A19.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-OUTPUTLEN     = 18.
  WA_AFIELD-KEY           = ' '.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*ACUM. MÊS ATUAL
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_ACUM_MES_ATU'.
  WA_AFIELD-SCRTEXT_M = TEXT-A20.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-OUTPUTLEN     = 18.
  WA_AFIELD-KEY           = ' '.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*VLR. VARIAÇÃO
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_VARIACAO'.
  WA_AFIELD-SCRTEXT_M = TEXT-A21.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-OUTPUTLEN     = 18.
  WA_AFIELD-KEY           = ' '.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*RESULTADO
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'RESULTADO_C'.
  WA_AFIELD-SCRTEXT_M = TEXT-A22.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-OUTPUTLEN     = 12.
  WA_AFIELD-KEY           = ' '.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*DT.LCTO.VAR.
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_LCTO'.
  WA_AFIELD-SCRTEXT_M = TEXT-A23.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*DOC.VAR.
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DOC_LCTO'.
  WA_AFIELD-SCRTEXT_M = TEXT-A24.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-REF_FIELD = 'BELNR'.
  WA_AFIELD-REF_TABLE   = 'BSEG'.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-HOTSPOT       = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*FLAG ESTORNO
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ESTORNO'.
  WA_AFIELD-SCRTEXT_M = TEXT-A25.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-OUTPUTLEN     = 08.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'AUGDT'.
  WA_AFIELD-SCRTEXT_M = TEXT-A29.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'AUGBL'.
  WA_AFIELD-SCRTEXT_M = TEXT-A30.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_REV'.
  WA_AFIELD-SCRTEXT_M = TEXT-A46.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DOC_REV'.
  WA_AFIELD-SCRTEXT_M = TEXT-A47.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*USUÁRIO
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'USNAM'.
  WA_AFIELD-SCRTEXT_M = TEXT-A26.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY           = ' '.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*DATA
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CPUDT'.
  WA_AFIELD-SCRTEXT_M = TEXT-A27.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*HORA
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CPUTM'.
  WA_AFIELD-SCRTEXT_M = TEXT-A28.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_ALV_TRANSF
*&---------------------------------------------------------------------*
*       Configura informações
*----------------------------------------------------------------------*
FORM F_ALV_TRANSF .
  DATA: W_AUGDT TYPE DATUM.

  DATA: TL_ZGL012 LIKE T_ZGL012_AVM OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF TL_SKAT OCCURS 0,
          SAKNR TYPE SAKNR,
          TXT50 TYPE TXT50_SKAT,
        END OF TL_SKAT.

  DATA: BEGIN OF TL_KNA1 OCCURS 0,
          KUNNR TYPE KUNNR,
          NAME1 TYPE NAME1_GP,
          VBUND TYPE KNA1-VBUND,
        END OF TL_KNA1.

  DATA: BEGIN OF TL_LFA1 OCCURS 0,
          LIFNR TYPE LIFNR,
          NAME1 TYPE NAME1_GP,
          VBUND TYPE LFA1-VBUND,
        END OF TL_LFA1.

  CLEAR: WA_LAYOUT.

  WA_LAYOUT-ZEBRA      = 'X'.
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.
  CLEAR WA_LAYOUT-GRID_TITLE .

  IF P_E_LANC = 'X'. "Se for reversão
    WA_LAYOUT-GRID_TITLE = TEXT-C03. "'Estorno de lançamentos'.
  ELSEIF P_C_LANC = 'X'.
    WA_LAYOUT-GRID_TITLE = TEXT-C02. "'Criação de lançamentos'.
  ELSEIF P_V_LANC = 'X'.
    WA_LAYOUT-GRID_TITLE = TEXT-C01. "'Visualização de lançamentos'.
  ENDIF.

  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT = ' '.
  WA_LAYOUT-BOX_FNAME  = 'MARK'.

  DO 3 TIMES.
    TL_ZGL012[] = T_ZGL012_AVM[].

    CASE SY-INDEX.
      WHEN 1.
        DELETE TL_ZGL012 WHERE HKONT = SPACE.

        IF NOT TL_ZGL012[] IS INITIAL.
          SELECT SAKNR TXT50 FROM SKAT
            INTO TABLE TL_SKAT
             FOR ALL ENTRIES IN TL_ZGL012
           WHERE SPRAS = SY-LANGU
             AND SAKNR = TL_ZGL012-HKONT.

          IF SY-SUBRC <> 0.
            MESSAGE S398(00) WITH TEXT-021 SPACE SPACE SPACE. "Erro ao obter descrição de contas
          ELSE.
            SORT TL_SKAT BY SAKNR.
          ENDIF.

        ENDIF.

      WHEN 2.
        DELETE TL_ZGL012 WHERE KUNNR = SPACE.

        IF NOT TL_ZGL012[] IS INITIAL.
          SELECT KUNNR NAME1 VBUND FROM KNA1
            INTO TABLE TL_KNA1
             FOR ALL ENTRIES IN TL_ZGL012
           WHERE KUNNR = TL_ZGL012-KUNNR.

          IF SY-SUBRC <> 0.
            MESSAGE S398(00) WITH TEXT-022 SPACE SPACE SPACE. "Erro ao obter descrição de clientes
          ELSE.
            SORT TL_KNA1 BY KUNNR.
          ENDIF.

        ENDIF.

      WHEN 3.
        DELETE TL_ZGL012 WHERE LIFNR = SPACE.

        IF NOT TL_ZGL012[] IS INITIAL.
          SELECT LIFNR NAME1 VBUND
            FROM LFA1
            INTO TABLE TL_LFA1
             FOR ALL ENTRIES IN TL_ZGL012
           WHERE LIFNR = TL_ZGL012-LIFNR.

          IF SY-SUBRC <> 0.
            MESSAGE S398(00) WITH TEXT-023 SPACE SPACE SPACE."Erro ao obter descrição de fornecedores
          ELSE.
            SORT TL_LFA1 BY LIFNR.
          ENDIF.

        ENDIF.

    ENDCASE.

  ENDDO.

  LOOP AT T_ZGL012_AVM INTO WA_ZGL012_AUX.

    IF P_BUKRS EQ '0101'.
      WA_ZGL012_AUX-VLR_ATUALIZADO   = TRUNC( WA_ZGL012_AUX-VLR_ATUALIZADO ).
      WA_ZGL012_AUX-VLR_ACUM_MES_ATU = TRUNC( WA_ZGL012_AUX-VLR_ACUM_MES_ATU ).
      WA_ZGL012_AUX-VLR_ACUM_MES_ANT = TRUNC( WA_ZGL012_AUX-VLR_ACUM_MES_ANT ).
      WA_ZGL012_AUX-VLR_VARIACAO     = TRUNC( WA_ZGL012_AUX-VLR_VARIACAO ).
    ENDIF.

    MOVE-CORRESPONDING WA_ZGL012_AUX TO O_ALV.

    IF WA_ZGL012_AUX-MOEDA_ATU = ''.
      O_ALV-MOEDA_ATU =  TEXT-A39.
    ELSE.
      O_ALV-MOEDA_ATU =  TEXT-A40.
    ENDIF.

    IF O_ALV-VLR_VARIACAO < 0.
      O_ALV-RESULTADO_C = TEXT-A41.
    ELSE.
      O_ALV-RESULTADO_C = TEXT-A42.
    ENDIF.

    IF ( NOT WA_ZGL012_AUX-HKONT IS INITIAL ) AND
       ( WA_ZGL012_AUX-KUNNR IS INITIAL )     AND
       ( WA_ZGL012_AUX-LIFNR IS INITIAL ).
      O_ALV-TIPO =  TEXT-A43.
      O_ALV-CODIGO = WA_ZGL012_AUX-HKONT .
      READ TABLE TL_SKAT WITH KEY SAKNR = WA_ZGL012_AUX-HKONT BINARY SEARCH.
      IF SY-SUBRC = 0.
        O_ALV-DESCRICAO = TL_SKAT-TXT50.
      ENDIF.
    ENDIF.

    IF NOT WA_ZGL012_AUX-KUNNR IS INITIAL.
      O_ALV-TIPO = TEXT-A44.
      O_ALV-CODIGO = WA_ZGL012_AUX-KUNNR.
      READ TABLE TL_KNA1 WITH KEY KUNNR = WA_ZGL012_AUX-KUNNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        O_ALV-DESCRICAO = TL_KNA1-NAME1.
      ENDIF.

    ENDIF.

    IF NOT WA_ZGL012_AUX-LIFNR IS INITIAL.
      O_ALV-TIPO = TEXT-A45.
      O_ALV-CODIGO = WA_ZGL012_AUX-LIFNR.
      READ TABLE TL_LFA1 WITH KEY LIFNR = WA_ZGL012_AUX-LIFNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        O_ALV-DESCRICAO = TL_LFA1-NAME1.
      ENDIF.
    ENDIF.

    O_ALV-STATUS = ICON_LED_RED.

    "verificar aqui status Fb08

    IF WA_ZGL012_AUX-ST_REV = 'S'.
      REFRESH T_ZGL012_AUX.
      SELECT * INTO TABLE T_ZGL012_AUX FROM ZGL012_AVM
       WHERE
         BUKRS      EQ  WA_ZGL012_AUX-BUKRS  AND
         DT_AVAL    LT  P_BUDAT              AND
         BELNR      EQ  WA_ZGL012_AUX-BELNR  AND
         BUZEI      EQ  WA_ZGL012_AUX-BUZEI  AND
         MOEDA_ATU  EQ  WA_ZGL012_AUX-MOEDA_ATU AND
         ESTORNO    NE 'X'           AND
         DOC_REV    EQ ''. "
      IF T_ZGL012_AUX[] IS NOT INITIAL. "não reverteu
        O_ALV-STATUS = ICON_RED_LIGHT.
      ELSE.
        O_ALV-STATUS = ICON_CHECKED. "ICON_LED_GREEN.
      ENDIF.

      O_ALV-VLR_VARIACAO = O_ALV-VLR_ACUM_MES_ANT.
      IF  O_ALV-VLR_VARIACAO = 0.
        O_ALV-STATUS = ICON_CHECKED. "ICON_LED_GREEN.
      ENDIF.
      "Reversão nova
    ELSEIF  NOT WA_ZGL012_AUX-DOC_LCTO IS INITIAL OR O_ALV-VLR_VARIACAO = 0.
      O_ALV-STATUS = ICON_LED_GREEN.
    ELSEIF WA_ZGL012_AUX-OBJ_KEY IS NOT INITIAL.
      READ TABLE IT_ZIB_CONTABIL_ERR INTO WA_ZIB_CONTABIL_ERR WITH KEY OBJ_KEY = WA_ZGL012_AUX-OBJ_KEY BINARY SEARCH.
      IF SY-SUBRC = 0.
        O_ALV-STATUS = ICON_INCOMPLETE.
      ENDIF.
    ENDIF.

    O_ALV-DELE   = WA_ZGL012_AUX-DELE.

    APPEND O_ALV.

    CLEAR O_ALV.

  ENDLOOP.


  DELETE O_ALV WHERE BUDAT GT P_BUDAT.

  IF P_E_LANC = 'X'. "Somente reversão
    DELETE O_ALV WHERE STATUS  NE ICON_RED_LIGHT AND STATUS NE ICON_CHECKED. "Reversão
  ELSE.
    DELETE O_ALV WHERE STATUS  EQ ICON_RED_LIGHT OR STATUS EQ ICON_CHECKED. "Reversão
  ENDIF.


ENDFORM.                    " F_ALV_TRANSF

*&---------------------------------------------------------------------*
*&      Form  F_ALV_IMPRIME
*&---------------------------------------------------------------------*
*       Apresenta o Relatório
*----------------------------------------------------------------------*
FORM F_ALV_IMPRIME .
  CALL SCREEN 9001.

ENDFORM.                    " F_ALV_IMPRIME

*&---------------------------------------------------------------------*
*&      Form  F_PF_STATUS_NOVO
*&---------------------------------------------------------------------*
*     Força um PF Status com o botão para salvar layout
*----------------------------------------------------------------------*
FORM F_PF_STATUS_NOVO ."USING RT_EXTAB TYPE SLIS_T_EXTAB.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  IF P_E_LANC = 'X' . "reversão
    APPEND 'ESTORNO' TO FCODE. "elimina o botao estorno do menu
  ENDIF.
  IF  P_V_LANC = 'X'.
    APPEND 'ESTORNO' TO FCODE.
    APPEND 'LANCAR' TO FCODE.
    APPEND '&REFRESH' TO FCODE.
  ENDIF.
  SET PF-STATUS 'F_SET_PF' EXCLUDING FCODE.
  SET TITLEBAR  'ZFTITLE'.

ENDFORM.                    " F_PF_STATUS_NOVO

*&---------------------------------------------------------------------*
*&      Form  F_AT_USER_COMMAND
*&---------------------------------------------------------------------*
*     Tratamento de comandos do usuário - Salvar e Duplo clique
*----------------------------------------------------------------------*
FORM F_AT_USER_COMMAND USING UCOMM LIKE SY-UCOMM
                             SELFIELD TYPE KKBLO_SELFIELD.

  ST_SELFIELD = SELFIELD.

  SELFIELD-REFRESH = 'X'.

  CASE SY-UCOMM.
    WHEN 'UP' OR 'BACK' .
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " F_AT_USER_COMMAND
*---------------------------------------------------------------------*
*      Form  f_top_of_page
*---------------------------------------------------------------------*
FORM F_TOP_OF_PAGE .
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER.

ENDFORM.                    " f_top_of_page


*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_VALIDA_CAMPOS CHANGING C_ERRO.
  DATA: WL_DATE       TYPE DATUM,
        WL_DATA_C(10).


*valida data da avaliação. Tem que ser o último dia do mes
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = P_BUDAT
    IMPORTING
      LAST_DAY_OF_MONTH = WL_DATE
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.

  IF SY-SUBRC <> 0.
    MESSAGE E398(00) WITH TEXT-017 SPACE SPACE SPACE. "Erro ao determinar dia!
  ELSE.
    IF WL_DATE <> P_BUDAT.
      WRITE WL_DATE TO WL_DATA_C.
      MESSAGE I398(00) WITH TEXT-024 " Favor entrar último dia do
                            TEXT-034 " mês como parâmetro. No caso:
                            WL_DATA_C
                            SPACE.

*      ADD 1 TO C_ERRO.
    ENDIF.
  ENDIF.

*pelo menos um dos checkbox esteja marcado:
  IF P_HKONT = SPACE AND P_KUNNR = SPACE AND P_LIFNR = SPACE AND P_HKONT2 = SPACE.
    MESSAGE I398(00) WITH TEXT-025 "Para partidas em aberto pelo menos
                          TEXT-035 "um dos checkbox deve ser marcado.
                          TEXT-036 " (Razão, Fornecedor ou Cliente)
                          SPACE.
    ADD 1 TO C_ERRO.

  ENDIF.

*Valida período de acordo com a data da avaliação
  IF P_SPMON <> P_BUDAT(6).
    MESSAGE I398(00) WITH TEXT-026 "Período difere da data de avaliação
                          SPACE
                          SPACE
                          SPACE.
    ADD 1 TO C_ERRO.

  ENDIF.

*valida data do documento e data do lançamento iguais a data de avaliacao
  IF P_BUDAT <> P_BUDAT2 OR P_BUDAT <> P_AUGDT.
    MESSAGE I398(00) WITH TEXT-027 "Data do documento e data do lançamento
                          TEXT-037 "devem ser iguais a data fixada da avaliação
                          SPACE.
    ADD 1 TO C_ERRO.

  ENDIF.

*  CALL FUNCTION 'Z_CONTROLE_FECHAMES'
*   EXPORTING
*     I_BUKRS    = P_BUKRS
*     I_DATA     = P_BUDAT
**      I_DEP_RESP = VG_DEPTO
*   IMPORTING
*     E_STATUS   = E_STATUS
*     E_MESSA    = E_MESSA
*   EXCEPTIONS
*     ERROR      = 1
*     OTHERS     = 2.
*  IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*  IF  E_STATUS = 'E'.
*    MESSAGE E398(00) WITH E_MESSA.
*  ENDIF.

ENDFORM.                    "ZF_VALIDA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  ZF_LANCAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALV      text
*----------------------------------------------------------------------*
FORM ZF_LANCAR CHANGING P_ALV LIKE O_ALV
                        P_ERRO.

  DATA: V_STBLG     TYPE BKPF-STBLG,
        V_DAT       TYPE ZIB_CONTABIL-BUDAT,
        V_MOEDA_ATU TYPE ZGL012_AVM-MOEDA_ATU.
  "ALRS
  CLEAR VOBJ_KEY.
  IF P_C_LANC = 'X'.
    IF WG_ACAO = 'ESTORNO'.
      CLEAR: V_STBLG, P_ERRO.
      VOBJ_KEY = P_ALV-OBJ_KEY.

      SUBMIT Z_FB08_ZGL042 WITH P_OBJ = P_ALV-OBJ_KEY
      AND RETURN.


      SELECT SINGLE STBLG
        FROM ZIB_CONTABIL_CHV
        INNER JOIN BKPF
        ON  BKPF~BUKRS = ZIB_CONTABIL_CHV~BUKRS
        AND BKPF~BELNR = ZIB_CONTABIL_CHV~BELNR
        AND BKPF~GJAHR = ZIB_CONTABIL_CHV~GJAHR
        INTO V_STBLG
        WHERE ZIB_CONTABIL_CHV~OBJ_KEY = P_ALV-OBJ_KEY.

      IF V_STBLG IS INITIAL. "Não estornou
        P_ERRO = 'X'.
      ENDIF.
    ELSE.
      IF P_ALV-LIFNR <> SPACE.
        IF P_ALV-UMSKZ = SPACE.
          PERFORM GRAVA_ZIB USING '1' CHANGING P_ALV P_ERRO.
        ELSE.
          PERFORM GRAVA_ZIB USING '2' CHANGING P_ALV P_ERRO.
        ENDIF.
      ENDIF.

      IF P_ALV-KUNNR <> SPACE.
        IF P_ALV-UMSKZ = SPACE.
          PERFORM GRAVA_ZIB USING '3' CHANGING P_ALV P_ERRO.
        ELSE.
          PERFORM GRAVA_ZIB  USING '4' CHANGING P_ALV P_ERRO.
        ENDIF.
      ENDIF.

      IF P_ALV-KUNNR IS INITIAL AND P_ALV-LIFNR IS INITIAL AND
         P_ALV-HKONT <> SPACE.
        PERFORM GRAVA_ZIB USING '5' CHANGING  P_ALV P_ERRO .
      ENDIF.
    ENDIF.
  ELSEIF P_E_LANC = 'X'."Reversão
    IF O_ALV-MOEDA_ATU+0(2) = '01'.
      CLEAR V_MOEDA_ATU.
    ELSE.
      V_MOEDA_ATU = C_USD.
    ENDIF.
    SELECT * INTO TABLE T_ZGL012_AUX FROM ZGL012_AVM
         WHERE
           BUKRS      EQ  P_ALV-BUKRS  AND
           DT_AVAL    LT  P_BUDAT      AND
           BELNR      EQ  P_ALV-BELNR  AND
           BUZEI      EQ  P_ALV-BUZEI  AND
           MOEDA_ATU  EQ  V_MOEDA_ATU  AND
           ESTORNO    NE 'X'           AND
           DOC_REV    EQ  ''
    ORDER BY DT_AVAL.

    CLEAR: V_STBLG, P_ERRO.
    LOOP AT T_ZGL012_AUX .
      CONCATENATE P_ALV-AUGDT+6(2) '.' P_ALV-AUGDT+4(2) '.' P_ALV-AUGDT+0(4) INTO V_DAT.
      SUBMIT Z_FB08_ZGL042 WITH P_OBJ = T_ZGL012_AUX-OBJ_KEY
                           WITH P_DAT = V_DAT
                           WITH P_ST  = '04'
      AND RETURN.

      WAIT UP TO 1 SECONDS.

      SELECT SINGLE STBLG
        FROM ZIB_CONTABIL_CHV
        INNER JOIN BKPF
        ON  BKPF~BUKRS = ZIB_CONTABIL_CHV~BUKRS
        AND BKPF~BELNR = ZIB_CONTABIL_CHV~BELNR
        AND BKPF~GJAHR = ZIB_CONTABIL_CHV~GJAHR
        INTO V_STBLG
        WHERE ZIB_CONTABIL_CHV~OBJ_KEY = T_ZGL012_AUX-OBJ_KEY.

      IF V_STBLG IS INITIAL. "Não estornou
        P_ERRO = 'X'.
      ELSE.
        UPDATE ZGL012_AVM SET DOC_REV = V_STBLG
                              DT_REV  = P_ALV-AUGDT
        WHERE  BUKRS  EQ  T_ZGL012_AUX-BUKRS    AND
           DT_AVAL    EQ  T_ZGL012_AUX-DT_AVAL  AND
           BELNR      EQ  T_ZGL012_AUX-BELNR    AND
           BUZEI      EQ  T_ZGL012_AUX-BUZEI    AND
           MOEDA_ATU  EQ  T_ZGL012_AUX-MOEDA_ATU.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    "ZF_LANCAR



*&---------------------------------------------------------------------*
*&      Module  status_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE STATUS_9001 OUTPUT.
  DATA: W_DT    TYPE SY-DATUM,
        TABIX   TYPE SY-TABIX,
        W_DTZIB TYPE ZGL012_AVM-DT_AVAL.

  PERFORM F_PF_STATUS_NOVO .

  IF CL_CONTAINER_95 IS INITIAL.
    CREATE OBJECT CL_CONTAINER_95
      EXPORTING
        SIDE  = '4'
        RATIO = '80'.

  ENDIF.

  IF NOT CL_GRID IS INITIAL.
    PERFORM ZF_ALV_HEADER USING '2'.
    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.

    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
        NO_MARGINS = 'X'.

    PERFORM ZF_ALV_HEADER USING '1'.

    IF EDITCONTAINER IS INITIAL .
      CREATE OBJECT EDITCONTAINER
        EXPORTING
          CONTAINER_NAME = 'HEADER'.

    ENDIF .

    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.

    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT      = CL_CONTAINER_95
        I_APPL_EVENTS = 'X'.

    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

*Limpa os duplicados comparando BELNR e BSCHL caso iguais manter o registro que possuir AUGBL preenchido
    SORT O_ALV BY BELNR BUZEI BSCHL DELE ASCENDING AUGBL DT_LCTO DOC_LCTO DESCENDING.

    WG_X_VARIANT-REPORT = SY-REPID.
    WG_SAVE             = 'X'.
    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT      = WG_X_VARIANT
        I_SAVE          = WG_SAVE        "IS_VARIANT = WG_VARIANT
        IS_LAYOUT       = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT[]
*       IT_SORT         = I_SORT[]
        IT_OUTTAB       = O_ALV[].

    CREATE OBJECT EVENT_RECEIVER.
    SET HANDLER EVENT_RECEIVER->CATCH_HOTSPOT      FOR CL_GRID.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOP_OF_PAGE FOR CL_GRID.

  ENDIF.

ENDMODULE.                 " status_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.

  DATA: V_VALID(1) TYPE C,
        INDROW     TYPE LVC_T_ROW,
        W_IND      TYPE LVC_T_ROW WITH HEADER LINE,
        V_MOD(1)   TYPE C,
        WL_TABIX   TYPE SY-TABIX,
        WL_ERRO(1),
        "E_STATUS(1),
        "E_MESSA(64),
        WL_CONT    TYPE I.


  IF NOT CL_GRID IS INITIAL.
    CALL METHOD CL_GRID->DISPATCH
      EXPORTING
        CARGO         = SY-UCOMM
        EVENTID       = 19
        IS_SHELLEVENT = ' '.

    IF SY-UCOMM IS INITIAL.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = IS_STABLE.
    ENDIF.
  ENDIF.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH O_ALV.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN '&REFRESH'.
      PERFORM ATUALIZA_TELA.
    WHEN 'LANCAR' OR 'ESTORNO'.
      WG_ACAO = SY-UCOMM.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          I_BUKRS  = P_BUKRS
          I_DATA   = P_BUDAT
        IMPORTING
          E_STATUS = E_STATUS
          E_MESSA  = E_MESSA
        EXCEPTIONS
          ERROR    = 1
          OTHERS   = 2.
      IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF  E_STATUS = 'E'.
        MESSAGE E398(00) WITH E_MESSA.
      ENDIF.

      CHECK P_V_LANC IS INITIAL.
      IF T_T030H[] IS INITIAL.
        SELECT KTOPL HKONT WAERS CURTP LSBEW LHBEW FROM T030H
          INTO TABLE T_T030H
           FOR ALL ENTRIES IN O_ALV
         WHERE KTOPL = C_0050
           AND HKONT = O_ALV-HKONT.
      ENDIF.

      IF T_0025[] IS INITIAL
      AND T_T030H[] IS NOT INITIAL.
        SELECT *
          FROM ZFIT0025
          INTO TABLE T_0025
           FOR ALL ENTRIES IN T_T030H
         WHERE SAKNR_P EQ T_T030H-LSBEW.

        SELECT *
          FROM ZFIT0025
     APPENDING TABLE T_0025
           FOR ALL ENTRIES IN T_T030H
         WHERE SAKNR_P EQ T_T030H-LHBEW.

        SORT: T_0025 BY SAKNR_P.

      ENDIF.

      REFRESH INDROW.

      CALL METHOD CL_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = INDROW.

      IF NOT INDROW IS INITIAL.
        DELETE INDROW WHERE NOT ROWTYPE IS INITIAL.
      ENDIF.


      LOOP AT O_ALV.
        O_ALV-MARK = ' '.
        MODIFY O_ALV.
      ENDLOOP.

      LOOP AT INDROW INTO W_IND.
        READ TABLE O_ALV INDEX W_IND-INDEX.
        O_ALV-MARK = 'X'.
        MODIFY O_ALV INDEX W_IND-INDEX.

      ENDLOOP.

      READ TABLE O_ALV WITH KEY MARK   = 'X'
                                  ST_REV = 'X'.

      IF P_E_LANC = 'X' OR SY-SUBRC = 0 OR WG_ACAO = 'ESTORNO'.
        CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
          EXPORTING
            TCODE  = 'FB08'
          EXCEPTIONS
            OK     = 0
            NOT_OK = 2
            OTHERS = 3.
        IF SY-SUBRC <> 0.
          MESSAGE TEXT-E01 TYPE 'I'.
          EXIT.
        ENDIF.
      ENDIF.

      CLEAR: WA_ZGL012_AVM, WL_CONT.
      READ TABLE O_ALV WITH KEY MARK   = 'X'
                                STATUS = ICON_OKAY.
      IF SY-SUBRC = 0.
        MESSAGE W398(00) WITH TEXT-028 "Existe registro com valor de variação
                              TEXT-038 "igual a zero. Reveja a seleção linha:
                              SY-TABIX.
      ELSE.

        LOOP AT O_ALV WHERE MARK = 'X'.
          CHECK ( O_ALV-STATUS = ICON_LED_GREEN AND WG_ACAO = 'ESTORNO' ) OR
                ( ( O_ALV-STATUS = ICON_LED_RED OR O_ALV-STATUS = ICON_RED_LIGHT OR O_ALV-STATUS = ICON_INCOMPLETE ) AND
                WG_ACAO = 'LANCAR' ).

          IF WG_ACAO = 'LANCAR'.

            IF O_ALV-MOEDA_ATU+0(2) = '01'.
              WA_ZGL012_AVM-MOEDA_ATU = ''.
            ELSE.
              WA_ZGL012_AVM-MOEDA_ATU =  C_USD.
            ENDIF.

            IF O_ALV-STATUS NE ICON_INCOMPLETE.
              SELECT SINGLE *
                FROM  ZGL012_AVM
                INTO  WA_ZGL012_AVM
                WHERE BUKRS     EQ O_ALV-BUKRS
                AND   DT_AVAL   EQ O_ALV-DT_AVAL
                AND   BELNR     EQ O_ALV-BELNR
                AND   BUZEI     EQ O_ALV-BUZEI
                AND   MOEDA_ATU EQ WA_ZGL012_AVM-MOEDA_ATU
                AND   OBJ_KEY   NE ''.

              IF SY-SUBRC = 0.
                CONTINUE.
              ENDIF.
            ENDIF.

          ENDIF.
          IF O_ALV-STATUS = ICON_LED_GREEN AND WG_ACAO = 'ESTORNO' AND O_ALV-VLR_VARIACAO = 0.
            CONTINUE.
          ENDIF.

          WL_TABIX = SY-TABIX.

          CLEAR WL_ST_REV.

          IF O_ALV-STATUS = ICON_RED_LIGHT. "Reversão
            WL_ST_REV = 'X'.
          ENDIF.

          CLEAR WL_ERRO.
          PERFORM ZF_LANCAR CHANGING O_ALV WL_ERRO.

          IF WL_ST_REV = 'X'.
            IF WL_ERRO IS INITIAL.
              O_ALV-STATUS   = ICON_LED_GREEN.
            ELSE.
              O_ALV-STATUS   = ICON_INCOMPLETE.
            ENDIF.
            MODIFY O_ALV INDEX WL_TABIX TRANSPORTING STATUS.
            COMMIT WORK AND WAIT .
            WL_CONT = WL_CONT + 1.
            CONTINUE.
          ENDIF.

          IF WL_ERRO IS INITIAL.
            IF WG_ACAO = 'ESTORNO'.
              CLEAR: O_ALV-DT_LCTO, O_ALV-DOC_LCTO.
              O_ALV-ESTORNO = 'X'.
              O_ALV-STATUS = ICON_LED_RED.
              WL_CONT = WL_CONT + 1.
              DELETE FROM ZGL012_AVM WHERE OBJ_KEY = O_ALV-OBJ_KEY.
              COMMIT WORK.
              CLEAR VOBJ_KEY.
            ELSE.
              CLEAR: O_ALV-ESTORNO.
              O_ALV-STATUS   = ICON_LED_GREEN.
              CLEAR O_ALV-DOC_LCTO.
            ENDIF.

            O_ALV-STATUS = ICON_GENERATE .
            O_ALV-OBJ_KEY = VOBJ_KEY.

            MOVE: SY-UNAME TO O_ALV-USNAM,
                  SY-UZEIT TO O_ALV-CPUTM,
                  SY-DATUM TO O_ALV-CPUDT.
            MODIFY O_ALV INDEX WL_TABIX TRANSPORTING DT_LCTO DOC_LCTO STATUS ESTORNO OBJ_KEY ST_REV USNAM CPUTM CPUDT.

            MOVE-CORRESPONDING O_ALV TO WA_ZGL012_AVM .

            IF O_ALV-MOEDA_ATU+0(2) = '01'.
              WA_ZGL012_AVM-MOEDA_ATU = ''.
            ELSE.
              WA_ZGL012_AVM-MOEDA_ATU =  C_USD.
            ENDIF.

            MOVE: SY-UNAME TO WA_ZGL012_AVM-USNAM,
                  SY-UZEIT TO WA_ZGL012_AVM-CPUTM,
                  SY-DATUM TO WA_ZGL012_AVM-CPUDT,
                  VOBJ_KEY TO WA_ZGL012_AVM-OBJ_KEY.

            IF WL_ST_REV = 'X'.
              WA_ZGL012_AVM-ST_REV = 'X'. "Marcar como reversão OK
            ENDIF.
            IF WG_ACAO NE 'ESTORNO'.
              MODIFY ZGL012_AVM FROM WA_ZGL012_AVM.
              IF SY-SUBRC = 0.
                COMMIT WORK AND WAIT .
                WL_CONT = WL_CONT + 1.
              ENDIF.
            ENDIF.
          ELSE.
            O_ALV-STATUS = ICON_LED_RED.
            MODIFY O_ALV INDEX WL_TABIX TRANSPORTING STATUS.
          ENDIF.

          CLEAR WA_ZGL012_AVM.

        ENDLOOP.

        IF WL_CONT > 0.
          MESSAGE I398(00) WITH WL_CONT TEXT-029  "registro(s) executado(s) com sucesso.
                                        TEXT-041. " (gravados na ZTGL012_AVM).
        ELSE.
          MESSAGE I398(00) WITH TEXT-030. "'Nenhum registro executado com sucesso.'
        ENDIF.

      ENDIF.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " user_command_9001  INPUT

*&---------------------------------------------------------------------*
*&      Form  ZF_INICIALIZA_VARIANTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_INICIALIZA_VARIANTE.
  CLEAR WG_VARIANT.

  WG_SAVE = 'X'.
  WG_VARIANT-REPORT = WG_REPNAME.
  WG_X_VARIANT = WG_VARIANT.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = WG_SAVE
    CHANGING
      CS_VARIANT = WG_X_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.

  IF SY-SUBRC = 0.
    P_VARI = WG_X_VARIANT-VARIANT.
  ENDIF.

ENDFORM. " ZF_inicializa_variante
*&---------------------------------------------------------------------*
*&      Form  ZF_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_AT_SELECTION_SCREEN .
* ALV Layout variant
  IF NOT P_VARI IS INITIAL.
    MOVE WG_VARIANT TO WG_X_VARIANT.
    MOVE P_VARI TO WG_X_VARIANT-VARIANT.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        I_SAVE     = WG_SAVE
      CHANGING
        CS_VARIANT = WG_X_VARIANT.

    WG_VARIANT = WG_X_VARIANT.

  ELSE.
    PERFORM ZF_INICIALIZA_VARIANTE.
  ENDIF.

ENDFORM.                    "zf_at_selection_screen

*&---------------------------------------------------------------------*
*&      Form  free_objects
*&---------------------------------------------------------------------*
*       Free Objects
*----------------------------------------------------------------------*
FORM FREE_OBJECTS .
  CALL METHOD CL_GRID->FREE
    EXCEPTIONS
      CNTL_ERROR        = 1
      CNTL_SYSTEM_ERROR = 2
      OTHERS            = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD CL_CONTAINER_95->FREE
    EXCEPTIONS
      CNTL_ERROR        = 1
      CNTL_SYSTEM_ERROR = 2
      OTHERS            = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD CL_CONTAINER_05->FREE
    EXCEPTIONS
      CNTL_ERROR        = 1
      CNTL_SYSTEM_ERROR = 2
      OTHERS            = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD EDITCONTAINER->FREE
    EXCEPTIONS
      CNTL_ERROR        = 1
      CNTL_SYSTEM_ERROR = 2
      OTHERS            = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " free_objects



*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IM_OBJ_DYNDOC_ID  text
*      -->IM_TABLE_INDEX    text
*----------------------------------------------------------------------*
FORM EVENT_TOP_OF_PAGE USING IM_OBJ_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT
                             IM_TABLE_INDEX   TYPE SYINDEX .

  PERFORM ZF_ALV_HEADER  USING '2'.

ENDFORM.                    "EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  GRAVA_ZIB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVA_ZIB USING    P_TIPO
               CHANGING P_ALV LIKE O_ALV
                        P_ERRO.

  DATA: WL_DATA(10),
        WL_DESCRICAO(1),
        WL_DATA_AVAL(10),
        WL_BKTXT(7),
        WL_NEWBS(2),
        WL_NEWBS2(2),
        WL_NEWKO2(10),
        WL_VLR_VARIACAO  TYPE ZGL012_AVM-VLR_VARIACAO,

        WL_DIVISAO(4),
        WL_BUPLA(4),
        WL_BEWAR         TYPE ZFIT0030-BEWAR,
        XPROV_DOLAR      TYPE I.

  DATA: BEGIN OF TL_AUX OCCURS 0,
          BELNR   TYPE BELNR_D,
          DT_AVAL TYPE BUDAT,
        END OF TL_AUX.

  IF P_ALV-GSBER IS INITIAL.
    IF P_ALV-BUKRS = '0200' .
      WL_DIVISAO = 'S201'.
    ELSEIF P_ALV-BUKRS = '0201' .
      WL_DIVISAO = 'H201'.
    ELSEIF P_ALV-BUKRS = '0202' .
      WL_DIVISAO = 'H202'.
    ELSEIF P_ALV-BUKRS = '0101' .
      WL_DIVISAO = 'F101'.
    ELSE.
      CONCATENATE P_ALV-BUKRS+2(2) '01'
      INTO WL_DIVISAO.
    ENDIF.
  ELSE.
    MOVE P_ALV-GSBER TO WL_DIVISAO.
  ENDIF.

  WL_BUPLA = WL_DIVISAO.
  IF P_ALV-BUKRS = '0004'.
    WL_BUPLA = '0401'.
  ENDIF.


  IF P_ALV-BUKRS = '0100'.
    CLEAR: WL_BUPLA, WL_DIVISAO.
  ENDIF.

  WRITE P_ALV-DT_AVAL TO WL_DATA.

  READ TABLE T_T030H INTO WA_T030H WITH KEY HKONT = P_ALV-HKONT.
  CLEAR WL_DESCRICAO.

  IF P_ALV-VLR_VARIACAO GE 0.
    WL_VLR_VARIACAO = P_ALV-VLR_VARIACAO.
  ELSE.
    WL_VLR_VARIACAO = P_ALV-VLR_VARIACAO * -1.
  ENDIF.

  SELECT SINGLE BEWAR
    FROM ZFIT0030
    INTO WL_BEWAR
   WHERE HKONT  = P_ALV-HKONT
     AND COND  IN ('C','CV').

  IF SY-SUBRC NE 0.
    SELECT SINGLE BEWAR
      FROM ZFIT0030
      INTO WL_BEWAR
     WHERE HKONT = P_ALV-HKONT.

  ENDIF.
  "alrs
  CASE P_TIPO.
    WHEN '1'.
      IF ( P_ALV-VLR_VARIACAO > 0 AND ( P_ALV-BSCHL = '31' OR
                                        P_ALV-BSCHL = '34' ) )
      OR (  P_ALV-VLR_VARIACAO > 0 AND ( ( P_ALV-BSCHL GE '21'
                                           AND P_ALV-BSCHL LE '39' ) AND
                                         ( P_ALV-BSCHL NE '23' AND
                                           P_ALV-BSCHL NE '30' AND
                                           P_ALV-BSCHL NE '33' ) ) ).
        WL_NEWBS  = '31'.
        WL_NEWBS2 = '40'.
        WL_NEWKO2 = WA_T030H-LSBEW.

      ENDIF.
      IF ( P_ALV-VLR_VARIACAO < 0 AND ( P_ALV-BSCHL = '31' OR
                                        P_ALV-BSCHL = '34' ) )
      OR ( P_ALV-VLR_VARIACAO < 0 AND ( ( P_ALV-BSCHL GE '21'
                                          AND P_ALV-BSCHL LE '39' ) AND
                                        ( P_ALV-BSCHL NE '23' AND
                                          P_ALV-BSCHL NE '30' AND
                                          P_ALV-BSCHL NE '33' ) ) ).
        WL_NEWBS = '21'.
        WL_NEWBS2 = '50'.
        WL_NEWKO2 = WA_T030H-LHBEW.

      ENDIF.
      IF P_ALV-STATUS = ICON_RED_LIGHT.
        IF P_ALV-VLR_VARIACAO > 0 AND P_ALV-UMSKZ = SPACE.
          WL_NEWBS = '31'.                                "'21'.
          WL_NEWBS2 = '40'.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LSBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO > 0.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LSBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO < 0 AND P_ALV-UMSKZ = SPACE.
          WL_NEWBS  = '21'.                               "'31'.
          WL_NEWBS2 = '50'.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LHBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO < 0.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LHBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN '2'.
      IF ( P_ALV-VLR_VARIACAO < 0 AND P_ALV-BSCHL = '39' )
      OR ( P_ALV-VLR_VARIACAO < 0 AND P_ALV-BSCHL = '29' ).
        WL_NEWBS  = '29'.
        WL_NEWBS2 = '50'.
        WL_NEWKO2 = WA_T030H-LHBEW.

      ENDIF.
      IF ( P_ALV-VLR_VARIACAO > 0 AND P_ALV-BSCHL = '39' )
      OR ( P_ALV-VLR_VARIACAO > 0 AND P_ALV-BSCHL = '29' ).
        WL_NEWBS = '39'.
        WL_NEWBS2 = '40'.
        WL_NEWKO2 = WA_T030H-LSBEW.

      ENDIF.
      IF P_ALV-STATUS = ICON_RED_LIGHT.
        IF P_ALV-VLR_VARIACAO > 0 AND P_ALV-UMSKZ <> SPACE.
          WL_NEWBS = '39'.                                "'29'.
          WL_NEWBS2 = '40'.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LSBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO > 0.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LSBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO < 0 AND P_ALV-UMSKZ <> SPACE..
          WL_NEWBS  = '29'.                               "'39'.
          WL_NEWBS2 = '50'.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LHBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO < 0.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LHBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '3'.
      IF ( P_ALV-VLR_VARIACAO < 0 AND (  P_ALV-BSCHL = '11' OR
                                         P_ALV-BSCHL = '17' ) )
      OR ( P_ALV-VLR_VARIACAO < 0 AND ( ( P_ALV-BSCHL GE '01' AND
                                          P_ALV-BSCHL LE '19' ) AND
                                          P_ALV-BSCHL NE '10' ) ).
        WL_NEWBS  = '01'.
        WL_NEWBS2 = '50'.
        WL_NEWKO2 = WA_T030H-LHBEW.

      ENDIF.
      IF ( P_ALV-VLR_VARIACAO > 0 AND ( P_ALV-BSCHL = '11' OR
                                        P_ALV-BSCHL = '17' ) )
      OR ( P_ALV-VLR_VARIACAO > 0 AND ( ( P_ALV-BSCHL GE '01' AND
                                          P_ALV-BSCHL LE '19' ) AND
                                          P_ALV-BSCHL NE '10' ) ).
        WL_NEWBS = '11'.
        WL_NEWBS2 = '40'.
        WL_NEWKO2 = WA_T030H-LSBEW.

      ENDIF.
      IF P_ALV-STATUS = ICON_RED_LIGHT.
        IF P_ALV-VLR_VARIACAO > 0 AND P_ALV-UMSKZ = SPACE.
          WL_NEWBS = '11'.                                "'01'.
          WL_NEWBS2 = '40'.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LSBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO > 0.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LSBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO < 0 AND P_ALV-UMSKZ = SPACE.
          WL_NEWBS  = '01'.                               "'11'.
          WL_NEWBS2 = '50'.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LHBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO < 0.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LHBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '4'.

      IF ( P_ALV-VLR_VARIACAO < 0 AND P_ALV-BSCHL = '19' )
         OR
         ( P_ALV-VLR_VARIACAO < 0 AND P_ALV-BSCHL = '09' ).
        WL_NEWBS = '09'.
        WL_NEWBS2 = '50'.
        WL_NEWKO2 = WA_T030H-LHBEW.

      ENDIF.
      IF ( P_ALV-VLR_VARIACAO > 0 AND P_ALV-BSCHL = '19' )
      OR ( P_ALV-VLR_VARIACAO > 0 AND P_ALV-BSCHL = '09' ).
        WL_NEWBS  = '19'.
        WL_NEWBS2 = '40'.
        WL_NEWKO2 = WA_T030H-LSBEW.

      ENDIF.
      IF P_ALV-STATUS = ICON_RED_LIGHT.
        IF P_ALV-VLR_VARIACAO > 0 AND P_ALV-UMSKZ <> SPACE.
          WL_NEWBS = '19'.                                "'09'.
          WL_NEWBS2 = '40'.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LSBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO > 0.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LSBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO < 0 AND P_ALV-UMSKZ <> SPACE.
          WL_NEWBS  = '09'.                               "'19'.
          WL_NEWBS2 = '50'.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LHBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO < 0.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LHBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '5'.
      IF ( P_ALV-VLR_VARIACAO > 0 AND ( P_ALV-BSCHL = '40' OR
                                        P_ALV-BSCHL = '86' ) )
      OR ( P_ALV-VLR_VARIACAO > 0 AND ( P_ALV-BSCHL = '50' OR
                                        P_ALV-BSCHL = '96' ) ).
        WL_NEWBS  = '50'.
        WL_NEWBS2 = '40'.
        WL_NEWKO2 = WA_T030H-LSBEW.

      ENDIF.

      IF ( P_ALV-VLR_VARIACAO < 0 AND ( P_ALV-BSCHL = '50' OR
                                        P_ALV-BSCHL = '96' ) )
      OR ( P_ALV-VLR_VARIACAO < 0 AND ( P_ALV-BSCHL = '40' OR
                                        P_ALV-BSCHL = '86' ) ).
        WL_NEWBS = '40'.
        WL_NEWBS2 = '50'.
        WL_NEWKO2 = WA_T030H-LHBEW.

      ENDIF.
      IF P_ALV-STATUS = ICON_RED_LIGHT.
        IF P_ALV-VLR_VARIACAO > 0 AND P_ALV-UMSKZ <> SPACE.
          WL_NEWBS = '50'.                                "'40'.
          WL_NEWBS2 = '40'.                               "'50'.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LSBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO > 0.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LSBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO < 0 AND P_ALV-UMSKZ <> SPACE.
          WL_NEWBS  = '40'.                               "'50'.
          WL_NEWBS2 = '50'.                               "'40'.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LHBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ELSEIF P_ALV-VLR_VARIACAO < 0.
**        Caso seja uma reversão
          CLEAR: WA_0025.
          READ TABLE T_0025 INTO WA_0025
            WITH KEY SAKNR_P = WA_T030H-LHBEW
                       BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_0025-SAKNR_R TO WL_NEWKO2.
          ELSE.
            CLEAR: WL_NEWKO2.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.

  "alrs
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR = '01'
      OBJECT      = 'ZID_GL'
    IMPORTING
      NUMBER      = VSEQ.

  VNUM = VSEQ .

  VSEQITEM = 0.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = VNUM
    IMPORTING
      OUTPUT = VNUM.

  SELECT SINGLE BUKRS LAND1
    FROM T001
    INTO WA_T001
   WHERE BUKRS = P_BUKRS.

  "'US'
*  IF P_BUKRS = '0004'.
*    WA_T001-LAND1 = 'US'.
*  ENDIF.

  SELECT SINGLE LAND1 WAERS
    FROM T005
    INTO WA_T005
   WHERE LAND1 = WA_T001-LAND1.

  " Primeira Perna lançamento contábil
  CONCATENATE 'ZGL042' VNUM  P_SPMON+0(4) INTO WA_ZIB_CONTABIL-OBJ_KEY.
  VOBJ_KEY = WA_ZIB_CONTABIL-OBJ_KEY.
  VSEQITEM = 1.
  WA_ZIB_CONTABIL-SEQITEM   = VSEQITEM.
  WA_ZIB_CONTABIL-XBLNR     = P_ALV-RESULTADO_C.
  WA_ZIB_CONTABIL-BSCHL     = WL_NEWBS. " P_ALV-BSCHL.
  WA_ZIB_CONTABIL-GSBER     = WL_DIVISAO.
  WA_ZIB_CONTABIL-BUKRS     = P_BUKRS.
  WA_ZIB_CONTABIL-INTERFACE = '35'.
  CONCATENATE P_SPMON+4(2) P_SPMON+0(4) INTO WA_ZIB_CONTABIL-BKTXT SEPARATED BY '.'.
  WA_ZIB_CONTABIL-BLDAT     = WL_DATA.
  WA_ZIB_CONTABIL-BUDAT     = WL_DATA.
  WA_ZIB_CONTABIL-GJAHR     = P_SPMON+0(4).
  WA_ZIB_CONTABIL-MONAT     = P_SPMON+4(2).
  WA_ZIB_CONTABIL-BLART     = 'VC'.

  CLEAR WA_ZIB_CONTABIL-HKONT2.

  IF O_ALV-TIPO = TEXT-A43. "Razao
    WA_ZIB_CONTABIL-HKONT     = P_ALV-HKONT.
    CLEAR WA_ZIB_CONTABIL-HKONT2.
  ELSE.
    WA_ZIB_CONTABIL-HKONT     = P_ALV-CODIGO.
*    WA_ZIB_CONTABIL-HKONT2    = P_ALV-HKONT.
  ENDIF.

  WA_ZIB_CONTABIL-UMSKZ     = P_ALV-UMSKZ.
  WA_ZIB_CONTABIL-WRBTR     = 0.
  WA_ZIB_CONTABIL-WAERS     = P_ALV-WAERS.
  WA_ZIB_CONTABIL-BUPLA     = WL_BUPLA.
  WA_ZIB_CONTABIL-ZUONR     = P_ALV-BELNR.

  IF P_TIPO = 'E'.
*    CONCATENATE 'Estorno VC-' O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
    CONCATENATE TEXT-A53 O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
  ELSE.
    IF P_ALV-STATUS EQ '@0A@'.
*      CONCATENATE 'Reversão Provisão VC-' O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
      CONCATENATE TEXT-A54 O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
    ELSE.
*      CONCATENATE 'Provisão VC-' O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
      CONCATENATE TEXT-A55 O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
    ENDIF.
  ENDIF.

  WA_ZIB_CONTABIL-WAERS_I = 'X'.
  IF O_ALV-MOEDA_ATU+0(2) = '01'. "Interna
    WA_ZIB_CONTABIL-DMBTR     = WL_VLR_VARIACAO.
    WA_ZIB_CONTABIL-WAERS_F   = 'USD'.
    WA_ZIB_CONTABIL-DMBE2     = 0.
  ELSE.
    WA_ZIB_CONTABIL-DMBTR     = 0.
    WA_ZIB_CONTABIL-WAERS_F   = 'USD'.
    WA_ZIB_CONTABIL-DMBE2     = WL_VLR_VARIACAO.
  ENDIF.

  WA_ZIB_CONTABIL-RG_ATUALIZADO  = 'N'.
  WA_ZIB_CONTABIL-BEWAR     = WL_BEWAR.

  INSERT INTO  ZIB_CONTABIL VALUES WA_ZIB_CONTABIL.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    P_ERRO = 'X'.
    CLEAR VOBJ_KEY.
  ELSE.
    COMMIT WORK.
  ENDIF.

  CLEAR  WA_ZIB_CONTABIL.

  " segunda perna lançamento contábil
  WA_ZIB_CONTABIL-OBJ_KEY = VOBJ_KEY .
  VSEQITEM = 2.
  WA_ZIB_CONTABIL-SEQITEM   = VSEQITEM.
  WA_ZIB_CONTABIL-XBLNR     = P_ALV-RESULTADO_C.
  WA_ZIB_CONTABIL-BSCHL     = WL_NEWBS2.
  WA_ZIB_CONTABIL-GSBER     = WL_DIVISAO.
  WA_ZIB_CONTABIL-BUKRS     = P_BUKRS.
  WA_ZIB_CONTABIL-INTERFACE = '35'.
  CONCATENATE P_SPMON+4(2) P_SPMON+0(4) INTO WA_ZIB_CONTABIL-BKTXT SEPARATED BY '.'.
  WA_ZIB_CONTABIL-BLDAT     = WL_DATA.
  WA_ZIB_CONTABIL-BUDAT     = WL_DATA.
  WA_ZIB_CONTABIL-GJAHR     = P_SPMON+0(4).
  WA_ZIB_CONTABIL-MONAT     = P_SPMON+4(2).
  WA_ZIB_CONTABIL-BLART     = 'VC'.
  WA_ZIB_CONTABIL-HKONT     = WL_NEWKO2.
  WA_ZIB_CONTABIL-UMSKZ     = P_ALV-UMSKZ.
  WA_ZIB_CONTABIL-WRBTR     = 0.
  WA_ZIB_CONTABIL-WAERS     = P_ALV-WAERS.
  WA_ZIB_CONTABIL-BUPLA     = WL_BUPLA.
  WA_ZIB_CONTABIL-ZUONR     = P_ALV-BELNR.

  IF P_TIPO = 'E'.
    CONCATENATE TEXT-A53 O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
  ELSE.
    IF P_ALV-STATUS EQ '@0A@'.
      CONCATENATE TEXT-A54  O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
    ELSE.
      CONCATENATE TEXT-A55  O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
    ENDIF.
  ENDIF.
  WA_ZIB_CONTABIL-WAERS_I   = 'X'.
  IF O_ALV-MOEDA_ATU+0(2) = '01'. "Interna
    WA_ZIB_CONTABIL-DMBTR     = WL_VLR_VARIACAO.
    WA_ZIB_CONTABIL-WAERS_F   = 'USD'.
    WA_ZIB_CONTABIL-DMBE2     = 0.
  ELSE.
    WA_ZIB_CONTABIL-DMBTR     = 0.
    WA_ZIB_CONTABIL-WAERS_F   = 'USD'.
    WA_ZIB_CONTABIL-DMBE2     = WL_VLR_VARIACAO.
  ENDIF.
  WA_ZIB_CONTABIL-RG_ATUALIZADO  = 'N'.
  WA_ZIB_CONTABIL-BEWAR     = WL_BEWAR.

  INSERT INTO  ZIB_CONTABIL VALUES WA_ZIB_CONTABIL.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    P_ERRO = 'X'.
    CLEAR VOBJ_KEY.
  ELSE.
    COMMIT WORK.
  ENDIF.
  CLEAR  WA_ZIB_CONTABIL.

ENDFORM.                    " GRAVA_ZIB
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZA_TELA .
  DATA: FLAG_MOD(1), TABIX TYPE SY-TABIX.

  LOOP AT O_ALV.

    IF O_ALV-OBJ_KEY IS INITIAL AND  O_ALV-ESTORNO NE 'X'.
      CONTINUE.
    ENDIF.

    TABIX = SY-TABIX.
    FLAG_MOD = 'N'.

    IF O_ALV-ESTORNO = 'X'.
      CLEAR: O_ALV-DT_LCTO, O_ALV-DOC_LCTO.
      O_ALV-STATUS = ICON_LED_RED.
      FLAG_MOD = 'S'.
      MODIFY O_ALV INDEX TABIX TRANSPORTING DT_LCTO DOC_LCTO STATUS.
      CONTINUE.
    ELSE.
      IF O_ALV-VLR_VARIACAO = 0.
        O_ALV-STATUS = ICON_LED_GREEN.
      ENDIF.

      IF O_ALV-DOC_LCTO IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      SELECT SINGLE  ZIB_CONTABIL_CHV~MANDT
             ZIB_CONTABIL_CHV~OBJ_KEY
             ZIB_CONTABIL_CHV~BELNR
             ZIB_CONTABIL_CHV~BUKRS
             ZIB_CONTABIL_CHV~GJAHR
             ZIB_CONTABIL~BLDAT
        FROM ZIB_CONTABIL_CHV INNER JOIN ZIB_CONTABIL
        ON ZIB_CONTABIL~OBJ_KEY EQ ZIB_CONTABIL_CHV~OBJ_KEY
        INTO WA_ZIB_CONTABIL_CHV
        WHERE ZIB_CONTABIL_CHV~OBJ_KEY = O_ALV-OBJ_KEY.

      IF SY-SUBRC = 0.
        CONCATENATE WA_ZIB_CONTABIL_CHV-BLDAT+6(4) WA_ZIB_CONTABIL_CHV-BLDAT+3(2) WA_ZIB_CONTABIL_CHV-BLDAT+0(2) INTO W_DTZIB.
        IF W_DTZIB+0(6) = O_ALV-DT_AVAL+0(6).
          O_ALV-STATUS = ICON_LED_GREEN.
          O_ALV-DOC_LCTO = WA_ZIB_CONTABIL_CHV-BELNR.
          O_ALV-DT_LCTO  = W_DTZIB.
          FLAG_MOD = 'S'.
        ENDIF.
        "FB08
        SELECT SINGLE   BUKRS BELNR  GJAHR  BUDAT STBLG STJAH
          FROM BKPF
          INTO WG_BKPF_FB08
          WHERE BUKRS EQ WA_ZIB_CONTABIL_CHV-BUKRS
          AND   BELNR EQ WA_ZIB_CONTABIL_CHV-BELNR
          AND   GJAHR EQ WA_ZIB_CONTABIL_CHV-GJAHR
          AND   STBLG NE ''.
        IF SY-SUBRC = 0.
          SELECT SINGLE   BUKRS BELNR GJAHR BUDAT
             FROM BKPF
             INTO WG_BKPF_FB08_E
             WHERE BUKRS EQ WG_BKPF_FB08-BUKRS
             AND   BELNR EQ WG_BKPF_FB08-STBLG
             AND   GJAHR EQ WG_BKPF_FB08-STJAH.
          IF WG_BKPF_FB08_E-BUDAT+0(6) = WG_BKPF_FB08-BUDAT+0(6). "estorno e lançamento mesmo mês, limpa pra gerar outro documento
            CLEAR: O_ALV-DT_LCTO, O_ALV-DOC_LCTO, O_ALV-OBJ_KEY.
            O_ALV-STATUS = ICON_LED_RED. "Refazer variação
          ELSE.
            O_ALV-STATUS = ICON_CHECKED. "Reversão com sucesso
          ENDIF.
          FLAG_MOD = 'S'.
        ENDIF.
      ELSEIF O_ALV-OBJ_KEY IS NOT INITIAL.
        SELECT SINGLE *
         FROM ZIB_CONTABIL_ERR
         INTO WA_ZIB_CONTABIL_ERR
         WHERE OBJ_KEY = O_ALV-OBJ_KEY.
        IF SY-SUBRC = 0.
          O_ALV-STATUS = ICON_INCOMPLETE.
          FLAG_MOD = 'S'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF FLAG_MOD = 'S'.
      MODIFY O_ALV INDEX TABIX TRANSPORTING DT_LCTO DOC_LCTO STATUS.
      MOVE-CORRESPONDING O_ALV TO WA_ZGL012_AVM .
      IF O_ALV-MOEDA_ATU+0(2) = '01'.
        CLEAR WA_ZGL012_AVM-MOEDA_ATU.
      ELSE.
        WA_ZGL012_AVM-MOEDA_ATU = 'USD'.
      ENDIF.

      IF WA_ZGL012_AVM-USNAM IS INITIAL.
        MOVE: SY-UNAME TO WA_ZGL012_AVM-USNAM,
              SY-UZEIT TO WA_ZGL012_AVM-CPUTM,
              SY-DATUM TO WA_ZGL012_AVM-CPUDT.
      ENDIF.

      MODIFY ZGL012_AVM FROM WA_ZGL012_AVM.
      IF SY-SUBRC = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.
    CLEAR: WA_ZGL012_AVM.

  ENDLOOP.

ENDFORM.                    " ATUALIZA_TELA

*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_CONTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUSCAR_CONTA .
  DATA: BEGIN OF TL_TEMP OCCURS 0,
          SAKNR TYPE SKA1-SAKNR,
          TXT50 TYPE SKAT-TXT50,
        END OF TL_TEMP.

  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  SELECT A~SAKNR TXT50
    INTO TABLE TL_TEMP
    FROM SKA1 AS A
   INNER JOIN GL_ACCT_CC AS B ON B~SAKNR EQ A~SAKNR
   INNER JOIN SKAT AS C ON C~SAKNR EQ A~SAKNR AND C~KTOPL EQ B~KTOPL AND C~SPRAS EQ SY-LANGU
   WHERE KTOKS IN ('YB01','YB04')
     AND BUKRS EQ P_BUKRS.

  IF SY-SUBRC IS INITIAL.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        RETFIELD        = 'SAKNR'
        DYNPPROG        = SY-REPID
        DYNPNR          = SY-DYNNR
        DYNPROFIELD     = 'SAKNR'
        VALUE_ORG       = 'S'
      TABLES
        VALUE_TAB       = TL_TEMP
        RETURN_TAB      = TL_RETURN_TAB
        DYNPFLD_MAPPING = TL_DSELC.

  ENDIF.

ENDFORM.                    " F_BUSCAR_CONTA
*&---------------------------------------------------------------------*
*&      Form  F_CONTA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CONTA2 .
  DATA: TL_SKA1             TYPE TABLE OF SKA1 WITH HEADER LINE,
        TL_SKAT             TYPE TABLE OF SKAT WITH HEADER LINE,
        TL_SKB1             TYPE TABLE OF SKB1 WITH HEADER LINE,
        TL_ZSALDO_CTA_MOEDA TYPE TABLE OF ZSALDO_CTA_MOEDA WITH HEADER LINE,
        WA_SKB1             TYPE SKB1.

  DATA: IT_CONTAS         TYPE ZCT_EMP_CONTAS,
        WA_CONTAS         TYPE ZLC_EMP_CONTAS,

        IT_CONTAS2        TYPE ZCT_EMP_CONTAS,
        WA_CONTAS2        TYPE ZLC_EMP_CONTAS,

        TG_TCURR          TYPE TABLE OF TCURR,
        WG_TCURR          TYPE TCURR,
        IT_BSXS           TYPE TABLE OF BSIS WITH HEADER LINE,
        IT_BSXS_G         TYPE TABLE OF BSIS WITH HEADER LINE,
        IT_SALDO_CONTAS   TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
        IT_SALDO_CONTAS_2 TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
        IT_SALDO_CONTAS_3 TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE.

  DATA: WL_SALDO_MI        TYPE FAGLFLEXT-HSLVT,
        WL_SALDO_MI2       TYPE FAGLFLEXT-KSLVT,
        WL_SALDO_MI3       TYPE FAGLFLEXT-OSLVT,
        WL_SALDO_DOC       TYPE BSIS-WRBTR,
        VG_LAST_DAY_AUX(8),
        W_LOOP             TYPE I,
        VL_MOEDA_OK        TYPE C,
        W_MOEDA            TYPE ZGL012_AVM-WAERS.

  DATA: REFE1	TYPE HSLXX12,
        REFE2	TYPE HSLXX12,
        VMES  TYPE MONAT,
        TABIX TYPE SY-TABIX.

  CONCATENATE P_BUDAT+0(6) '01' INTO VG_LAST_DAY_AUX.
  VG_LAST_DAY = VG_LAST_DAY_AUX.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      I_DATE = VG_LAST_DAY
    IMPORTING
      E_DATE = VG_LAST_DAY.

  SELECT *
    FROM ZSALDO_CTA_MOEDA
    INTO TABLE TL_ZSALDO_CTA_MOEDA
    WHERE BUKRS = P_BUKRS.

  SORT TL_ZSALDO_CTA_MOEDA  BY SAKNR.

  RANGES: R_KTOKS FOR SKA1-KTOKS.

  SELECT SINGLE *                      "#EC CI_DB_OPERATION_OK[2431747]
      FROM T882G
      INTO WG_T882G
       WHERE RBUKRS = P_BUKRS.

  CLEAR: R_KTOKS.
  R_KTOKS-SIGN   = 'I'.
  R_KTOKS-OPTION = 'EQ'.
  R_KTOKS-LOW    = 'YB01'.
  R_KTOKS-HIGH   = 'YB01'.
  APPEND R_KTOKS.

  R_KTOKS-LOW    = 'YB02'.
  R_KTOKS-HIGH   = 'YB02'.
  APPEND R_KTOKS.

  R_KTOKS-LOW    = 'YB04'.
  R_KTOKS-HIGH   = 'YB04'.
  APPEND R_KTOKS.

  SELECT *                             "#EC CI_DB_OPERATION_OK[2389136]
      FROM SKA1                        "#EC CI_DB_OPERATION_OK[2431747]
      INTO TABLE TL_SKA1
      WHERE KTOPL	=	'0050'
      AND   KTOKS	IN R_KTOKS
      AND   SAKNR IN S_HKONT2.

  IF TL_SKA1[] IS INITIAL.
    MESSAGE I398(00) WITH TEXT-033."Não Encontrado contas do Razão p/ a Seleção
    EXIT.
  ENDIF.

  SELECT *                             "#EC CI_DB_OPERATION_OK[2431747]
     FROM SKB1
     INTO TABLE TL_SKB1
     FOR ALL ENTRIES IN TL_SKA1
     WHERE BUKRS  = P_BUKRS
     AND   SAKNR  = TL_SKA1-SAKNR
     AND   XOPVW  = ''.

  IF TL_SKB1[] IS INITIAL.
    MESSAGE I398(00) WITH TEXT-031 "Não Encontrado contas do Razão
                          TEXT-039 "p/ a Seleção (empresa)
                          TEXT-040. "administração por partidas em aberto
    EXIT.
  ENDIF.

  SELECT KTOPL HKONT WAERS CURTP LSBEW LHBEW FROM T030H
      INTO TABLE T_T030H
      FOR ALL ENTRIES IN TL_SKB1
      WHERE KTOPL = C_0050 AND
            HKONT = TL_SKB1-SAKNR.

  SORT T_T030H BY HKONT.
  LOOP AT TL_SKB1 INTO WA_SKB1.
    TABIX = SY-TABIX.
    READ TABLE T_T030H INTO WA_T030H WITH KEY HKONT = WA_SKB1-SAKNR BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR WA_SKB1-SAKNR.
      MODIFY TL_SKB1 FROM WA_SKB1 INDEX TABIX TRANSPORTING SAKNR.
    ENDIF.
  ENDLOOP.
  DELETE TL_SKB1 WHERE SAKNR  = ''.

  SORT TL_ZSALDO_CTA_MOEDA BY SAKNR.

  SELECT SINGLE BUKRS LAND1
   FROM T001
   INTO WA_T001
   WHERE BUKRS = P_BUKRS.

*  "'US'
*  IF P_BUKRS = '0004'.
*    WA_T001-LAND1 = 'US'.
*  ENDIF.

  SELECT SINGLE LAND1 WAERS CURIN CURHA
    FROM T005
    INTO WA_T005
    WHERE LAND1 = WA_T001-LAND1.

  REFRESH IT_CONTAS.
*---> 05/07/2023 - Migração S4 - DL
  SORT TL_SKA1 BY SAKNR.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT TL_SKB1.
    CLEAR: TL_ZSALDO_CTA_MOEDA, TL_SKA1.
    READ TABLE TL_SKA1 WITH KEY SAKNR = TL_SKB1-SAKNR BINARY SEARCH.
    IF ( TL_SKA1-KTOKS = 'YB04' ).
      READ TABLE TL_ZSALDO_CTA_MOEDA WITH KEY SAKNR = TL_SKB1-SAKNR BINARY SEARCH.
      IF SY-SUBRC = 0 AND
        ( TL_ZSALDO_CTA_MOEDA-WAERS = WA_T005-CURIN OR
          TL_ZSALDO_CTA_MOEDA-WAERS = WA_T005-CURHA ).
        WA_CONTAS-BUKRS = TL_SKB1-BUKRS.
        WA_CONTAS-SAKNR = TL_SKB1-SAKNR.
        APPEND WA_CONTAS TO IT_CONTAS.
      ENDIF.
    ELSEIF  ( TL_ZSALDO_CTA_MOEDA-WAERS = WA_T005-CURIN OR
              TL_ZSALDO_CTA_MOEDA-WAERS = WA_T005-CURHA ).
      WA_CONTAS-BUKRS = TL_SKB1-BUKRS.
      WA_CONTAS-SAKNR = TL_SKB1-SAKNR.
      APPEND WA_CONTAS TO IT_CONTAS.
**-US 156673-12-11-2024-#156673-RJF-Inicio
*    ELSEIF ( tl_ska1-ktoks = 'YB01' ).
*      wa_contas-bukrs = tl_skb1-bukrs.
*      wa_contas-saknr = tl_skb1-saknr.
*      APPEND wa_contas TO it_contas.
**-US 156673-12-11-2024-#156673-RJF-Fim
    ENDIF.

  ENDLOOP.

  SELECT *
    FROM SKAT
    INTO TABLE TL_SKAT
    FOR ALL ENTRIES IN TL_SKB1
    WHERE SPRAS = SY-LANGU
    AND   KTOPL	=	'0050'
    AND   SAKNR = TL_SKB1-SAKNR.

  CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
    EXPORTING
      RYEAR         = P_BUDAT+0(4)
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

  REFRESH IT_CONTAS2.
*---> 05/07/2023 - Migração S4 - DL
  SORT TL_SKA1 BY SAKNR.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT TL_SKB1.
    CLEAR: TL_ZSALDO_CTA_MOEDA, TL_SKA1.
    READ TABLE TL_SKA1 WITH KEY SAKNR = TL_SKB1-SAKNR BINARY SEARCH.
    IF ( TL_SKA1-KTOKS = 'YB04' ).
      READ TABLE TL_ZSALDO_CTA_MOEDA WITH KEY SAKNR = TL_SKB1-SAKNR BINARY SEARCH.
      IF SY-SUBRC = 0 AND
        ( TL_ZSALDO_CTA_MOEDA-WAERS NE WA_T005-CURIN AND
          TL_ZSALDO_CTA_MOEDA-WAERS NE WA_T005-CURHA AND
          TL_ZSALDO_CTA_MOEDA-WAERS NE WA_T005-WAERS ).
        WA_CONTAS2-BUKRS = TL_SKB1-BUKRS.
        WA_CONTAS2-SAKNR = TL_SKB1-SAKNR.
        APPEND WA_CONTAS2 TO IT_CONTAS2.
      ENDIF.
    ELSEIF  ( TL_ZSALDO_CTA_MOEDA-WAERS NE WA_T005-CURIN AND
              TL_ZSALDO_CTA_MOEDA-WAERS NE WA_T005-CURHA AND
              TL_ZSALDO_CTA_MOEDA-WAERS NE WA_T005-WAERS ).
      WA_CONTAS2-BUKRS = TL_SKB1-BUKRS.
      WA_CONTAS2-SAKNR = TL_SKB1-SAKNR.
      APPEND WA_CONTAS2 TO IT_CONTAS2.
**-US 156673-12-11-2024-#156673-RJF-Inicio
*    ELSEIF ( tl_ska1-ktoks = 'YB01' ).
*      wa_contas2-bukrs = tl_skb1-bukrs.
*      wa_contas2-saknr = tl_skb1-saknr.
*      APPEND wa_contas2 TO it_contas2.
**-US 156673-12-11-2024-#156673-RJF-Fim
    ENDIF.

  ENDLOOP.

  IF IT_CONTAS[] IS INITIAL AND IT_CONTAS2[] IS INITIAL.
    MESSAGE TEXT-032 TYPE 'I'. "Não foram encontradas contas configuradas
    EXIT.
  ENDIF.

  IF IT_CONTAS2[] IS NOT INITIAL.
    CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
      EXPORTING
        P_DT_POSICAO = VG_LAST_DAY
        CONTAS       = IT_CONTAS2
      TABLES
        IT_BSXS      = IT_BSXS.
  ENDIF.
*---> 05/07/2023 - Migração S4 - DL
  SORT IT_BSXS BY HKONT WAERS.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT TL_SKB1.

    READ TABLE TL_SKA1 WITH KEY SAKNR = TL_SKB1-SAKNR BINARY SEARCH.
    IF ( TL_SKA1-KTOKS NE 'YB04' ).
*    IF ( tl_ska1-ktoks NE 'YB04' AND tl_ska1-ktoks NE 'YB01' ). "RJF -US 156673-12-11-2024-#156673-RJF
      DELETE IT_BSXS WHERE HKONT  = TL_SKB1-SAKNR AND WAERS EQ  WA_T005-WAERS.
    ENDIF.
  ENDLOOP.

  SELECT *
    INTO TABLE T_ZGL012_AUX
   FROM ZGL012_AVM
  FOR ALL ENTRIES IN TL_SKB1
  WHERE BUKRS   EQ  TL_SKB1-BUKRS
    AND ST_REV  EQ 'S'
    AND DT_AVAL EQ P_BUDAT
    AND BELNR   EQ TL_SKB1-SAKNR.

  REFRESH: IT_ZIB_CONTABIL_ERR, IT_ZIB_CONTABIL_CHV.
  IF T_ZGL012_AUX[] IS NOT INITIAL.
    SELECT *
            FROM ZIB_CONTABIL_ERR
            INTO TABLE IT_ZIB_CONTABIL_ERR
            FOR ALL ENTRIES IN T_ZGL012_AUX
            WHERE OBJ_KEY = T_ZGL012_AUX-OBJ_KEY.
    SORT IT_ZIB_CONTABIL_ERR BY OBJ_KEY.

    SELECT ZIB_CONTABIL_CHV~MANDT
              ZIB_CONTABIL_CHV~OBJ_KEY
              ZIB_CONTABIL_CHV~BELNR
              ZIB_CONTABIL_CHV~BUKRS
              ZIB_CONTABIL_CHV~GJAHR
              ZIB_CONTABIL~BLDAT
         FROM ZIB_CONTABIL_CHV INNER JOIN ZIB_CONTABIL
         ON ZIB_CONTABIL~OBJ_KEY EQ ZIB_CONTABIL_CHV~OBJ_KEY
         INTO TABLE IT_ZIB_CONTABIL_CHV
         FOR ALL ENTRIES IN T_ZGL012_AUX
         WHERE ZIB_CONTABIL_CHV~OBJ_KEY = T_ZGL012_AUX-OBJ_KEY.

    SORT IT_ZIB_CONTABIL_CHV BY OBJ_KEY.
  ENDIF.

  SORT: TL_SKA1           BY SAKNR,
        TL_SKAT           BY SAKNR,
        TL_SKB1           BY SAKNR,
        IT_SALDO_CONTAS   BY RACCT,
        IT_SALDO_CONTAS_2 BY RACCT,
        IT_BSXS           BY HKONT,
        T_ZGL012_AUX      BY BELNR MOEDA_ATU.

  VMES = P_BUDAT+4(2).
  IF VMES = 12.
    VMES = 15.
  ENDIF.

  REFRESH O_ALV2.
  LOOP AT TL_SKB1.
    READ TABLE TL_SKA1 WITH KEY SAKNR = TL_SKB1-SAKNR BINARY SEARCH.
    "

*-US 156673-12-11-2024-#156673-RJF-Inicio
    IT_BSXS_G[] = IT_BSXS[].
    SORT IT_BSXS_G[] BY HKONT VBUND.
    DELETE ADJACENT DUPLICATES FROM IT_BSXS_G COMPARING HKONT VBUND.
    LOOP AT IT_BSXS_G ASSIGNING FIELD-SYMBOL(<FS_BSXS_G>) WHERE HKONT EQ TL_SKB1-SAKNR.
*-US 156673-12-11-2024-#156673-RJF-fim

      WL_SALDO_MI = 0.
      READ TABLE IT_SALDO_CONTAS WITH KEY RACCT  = TL_SKB1-SAKNR.
      IF SY-SUBRC IS INITIAL.
        ADD IT_SALDO_CONTAS-SLVT TO WL_SALDO_MI.
        DO VMES TIMES VARYING REFE1 FROM IT_SALDO_CONTAS-SL01 NEXT IT_SALDO_CONTAS-SL02.
          ADD REFE1 TO WL_SALDO_MI.
        ENDDO.
      ENDIF.

      "
      WL_SALDO_MI2 = 0.
      READ TABLE IT_SALDO_CONTAS_2 WITH KEY   RACCT  = TL_SKB1-SAKNR.
      IF SY-SUBRC IS INITIAL.
        ADD IT_SALDO_CONTAS_2-SLVT TO WL_SALDO_MI2.
        DO VMES TIMES VARYING REFE1 FROM IT_SALDO_CONTAS_2-SL01 NEXT IT_SALDO_CONTAS_2-SL02.
          ADD REFE1 TO WL_SALDO_MI2.
        ENDDO.
      ENDIF.


      WL_SALDO_MI3 = 0.
      READ TABLE IT_SALDO_CONTAS_3 WITH KEY   RACCT  = TL_SKB1-SAKNR.
      IF SY-SUBRC IS INITIAL.
        ADD IT_SALDO_CONTAS_3-SLVT TO WL_SALDO_MI3.
        DO VMES TIMES VARYING REFE1 FROM IT_SALDO_CONTAS_3-SL01 NEXT IT_SALDO_CONTAS_3-SL02.
          ADD REFE1 TO WL_SALDO_MI3.
        ENDDO.
      ENDIF.

      WL_SALDO_DOC = 0.
*    LOOP AT it_bsxs WHERE hkont  = tl_skb1-saknr. " -US 156673-12-11-2024-#156673-RJF
      LOOP AT IT_BSXS WHERE HKONT  = TL_SKB1-SAKNR AND VBUND = <FS_BSXS_G>-VBUND. " -US 156673-12-11-2024-#156673-RJF
        IF IT_BSXS-SHKZG = 'S'. "debito
          ADD IT_BSXS-DMBTR TO WL_SALDO_MI.
          ADD IT_BSXS-DMBE2 TO WL_SALDO_MI2.
          ADD IT_BSXS-DMBE3 TO WL_SALDO_MI3.
          ADD IT_BSXS-WRBTR TO WL_SALDO_DOC.
        ELSE.
          SUBTRACT IT_BSXS-DMBTR FROM WL_SALDO_MI.
          SUBTRACT IT_BSXS-DMBE2 FROM WL_SALDO_MI2.
          SUBTRACT IT_BSXS-DMBE3 FROM WL_SALDO_MI3.
          SUBTRACT IT_BSXS-WRBTR FROM WL_SALDO_DOC.
        ENDIF.
      ENDLOOP.

      IF P_BUKRS = '0101'. "PYG
        MULTIPLY WL_SALDO_MI BY 100.
      ENDIF.

      "Sem saldo a corrigir
      IF WL_SALDO_DOC = 0 AND WL_SALDO_MI = 0 AND WL_SALDO_MI2 = 0.
        CONTINUE.
      ENDIF.

      READ TABLE TL_ZSALDO_CTA_MOEDA WITH KEY SAKNR = TL_SKB1-SAKNR BINARY SEARCH.

      O_ALV2-RACCT            = TL_SKB1-SAKNR.
*    o_alv2-rassc            = ''.  " -US 156673-12-11-2024-#156673-RJF
      O_ALV2-RASSC            = <FS_BSXS_G>-VBUND.  " -US 156673-12-11-2024-#156673-RJF

      READ TABLE TL_SKAT WITH KEY SAKNR = TL_SKB1-SAKNR BINARY SEARCH.
      O_ALV2-TXT50            = TL_SKAT-TXT50.

      IF ( TL_SKA1-KTOKS = 'YB04' ).
*    IF ( tl_ska1-ktoks = 'YB04' AND tl_ska1-ktoks = 'YB01' ). "RJF
        O_ALV2-WAERS            = TL_ZSALDO_CTA_MOEDA-WAERS.
      ELSE.
        O_ALV2-WAERS            = WA_T005-CURHA.
      ENDIF.

      O_ALV2-KTOKS            = TL_SKA1-KTOKS.
      O_ALV2-CURR1            = WL_SALDO_MI.
      O_ALV2-CURR2            = WL_SALDO_MI2.
      O_ALV2-CURR3            = WL_SALDO_MI3.
      O_ALV2-WRBTR            = WL_SALDO_DOC.

      IF P_BUKRS = '0004' OR P_BUKRS = '0201' OR P_BUKRS = '0202' OR P_BUKRS = '0037'. "Elimina moeda dolar
        IF O_ALV2-WAERS  = C_USD.
          CONTINUE.
        ENDIF.
      ENDIF.


      IF O_ALV2-WAERS = C_USD.
        W_LOOP = 1.
      ELSE.
        W_LOOP = 2.
      ENDIF.
      CLEAR: WL_SALDO_MI,WL_SALDO_DOC.
      DO W_LOOP TIMES.
        IF SY-INDEX = 1.
          W_MOEDA = ''.
          CLEAR XTX_USD_AUX.
          READ TABLE  T_TCURR INTO WA_TCURR WITH KEY FCURR = O_ALV2-WAERS.
          IF P_BUKRS = '0101'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = TL_SKB1-SAKNR
              IMPORTING
                OUTPUT = TL_SKB1-SAKNR.
            IF TL_SKB1-SAKNR+0(1) = '1'. "ativos (compra)
              READ TABLE  T_TCURR_G INTO WA_TCURR WITH KEY FCURR = O_ALV2-WAERS.
            ENDIF.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = TL_SKB1-SAKNR
              IMPORTING
                OUTPUT = TL_SKB1-SAKNR.
          ENDIF.
          IF SY-SUBRC = 0.
            O_ALV2-TX_USD = WA_TCURR-UKURS.
            XTX_USD_AUX = WA_TCURR-UKURS.
          ENDIF.
        ELSE. "ler o USD
          READ TABLE  T_TCURR INTO WA_TCURR WITH KEY FCURR = O_ALV2-WAERS. "alrs2
*        READ TABLE  T_TCURR_A INTO WA_TCURR WITH KEY FCURR =  WA_T005-WAERS. "alrs
          IF P_BUKRS = '0101'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = TL_SKB1-SAKNR
              IMPORTING
                OUTPUT = TL_SKB1-SAKNR.
            IF TL_SKB1-SAKNR+0(1) = '1'. "ativos (compra)
              READ TABLE  T_TCURR_G INTO WA_TCURR WITH KEY FCURR = O_ALV2-WAERS.
            ENDIF.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = TL_SKB1-SAKNR
              IMPORTING
                OUTPUT = TL_SKB1-SAKNR.
          ENDIF.
          IF SY-SUBRC = 0.
            XTX_USD = WA_TCURR-UKURS.
          ENDIF.

          W_MOEDA = C_USD.
*        READ TABLE  T_TCURR INTO WA_TCURR WITH KEY FCURR = C_USD.
          READ TABLE  T_TCURR_A INTO WA_TCURR WITH KEY FCURR =  WA_T005-WAERS. "alrs
          IF P_BUKRS = '0101'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = TL_SKB1-SAKNR
              IMPORTING
                OUTPUT = TL_SKB1-SAKNR.
            IF TL_SKB1-SAKNR+0(1) = '1'. "ativos (compra)
              READ TABLE  T_TCURR_G INTO WA_TCURR WITH KEY FCURR = C_USD.
            ENDIF.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = TL_SKB1-SAKNR
              IMPORTING
                OUTPUT = TL_SKB1-SAKNR.
          ENDIF.
          IF SY-SUBRC = 0.
            O_ALV2-TX_USD = WA_TCURR-UKURS.
          ENDIF.
        ENDIF.

        IF WA_ZGL012_AVM-TX_FECH LT 0.
          MULTIPLY WA_ZGL012_AVM-TX_FECH BY -1.
        ENDIF.

*     "// Verifica se a Moeda do banco tl_zsaldo_cta_moeda é Igual a moeda da Empresa wa_t005
*     "// se for Igual não usamos o saldo da moeda do documento
*     "// caso Suiça IR058012
        CLEAR VL_MOEDA_OK.
        IF WA_T005-WAERS    EQ TL_ZSALDO_CTA_MOEDA-WAERS
           OR WA_T005-CURIN EQ TL_ZSALDO_CTA_MOEDA-WAERS
          OR WA_T005-CURHA  EQ TL_ZSALDO_CTA_MOEDA-WAERS.
          VL_MOEDA_OK = ABAP_TRUE.
        ENDIF.
*     "// caso Suiça IR058012

        O_ALV2-BELNR            = ''.
        O_ALV2-BELNR_EST        = ''.

        READ TABLE T_ZGL012_AUX WITH KEY BELNR     = TL_SKB1-SAKNR
                                         MOEDA_ATU = W_MOEDA BINARY SEARCH.
        IF SY-SUBRC = 0.
          IF T_ZGL012_AUX-ESTORNO = 'X'.
            O_ALV2-OBJ_KEY_EST  = T_ZGL012_AUX-OBJ_KEY.
          ELSE.
            O_ALV2-OBJ_KEY = T_ZGL012_AUX-OBJ_KEY.
          ENDIF.
          CLEAR O_ALV2-LOG .
          IF O_ALV2-OBJ_KEY IS NOT INITIAL.
            O_ALV2-LOG     = ICON_MESSAGE_WARNING_SMALL.
          ENDIF.
          READ TABLE IT_ZIB_CONTABIL_CHV INTO WA_ZIB_CONTABIL_CHV WITH KEY OBJ_KEY = T_ZGL012_AUX-OBJ_KEY BINARY SEARCH.
          IF SY-SUBRC = 0.
            IF T_ZGL012_AUX-ESTORNO = 'X'.
              O_ALV2-BELNR_EST  = WA_ZIB_CONTABIL_CHV-BELNR.
              O_ALV2-LOG        = ICON_SYSTEM_UNDO.
            ELSE.
              O_ALV2-BELNR     = WA_ZIB_CONTABIL_CHV-BELNR.
              O_ALV2-LOG       = ICON_LED_GREEN.
            ENDIF.

          ELSE.
            READ TABLE IT_ZIB_CONTABIL_ERR INTO WA_ZIB_CONTABIL_ERR WITH KEY OBJ_KEY = T_ZGL012_AUX-OBJ_KEY BINARY SEARCH.
            IF SY-SUBRC = 0.
              O_ALV2-LOG = ICON_INCOMPLETE.
            ENDIF.
          ENDIF.
        ELSE.
          O_ALV2-OBJ_KEY          = ''.
          O_ALV2-LOG              = ''.
        ENDIF.
        "
        IF W_MOEDA = ''.
          O_ALV2-MOEDA_ATU =  TEXT-A39.
        ELSE.
          O_ALV2-MOEDA_ATU =  TEXT-A40.
        ENDIF.

**********************************************************************

        IF SY-INDEX = 1. "Moeda Interna
          IF O_ALV2-WRBTR  EQ 0 AND VL_MOEDA_OK IS NOT INITIAL.
            TRY.
                IF O_ALV2-TX_USD LT 0.
                  MULTIPLY O_ALV2-TX_USD BY -1.
                  O_ALV2-SALDO_CORR       = ( O_ALV2-CURR2 / O_ALV2-TX_USD ).
                ELSE.
                  O_ALV2-SALDO_CORR       = ( O_ALV2-CURR2 * O_ALV2-TX_USD ).
                ENDIF.
              CATCH CX_SY_ZERODIVIDE.
            ENDTRY.
          ELSE.
            TRY.
                IF O_ALV2-TX_USD LT 0.
                  MULTIPLY O_ALV2-TX_USD BY -1.
                  O_ALV2-SALDO_CORR       = ( O_ALV2-WRBTR / O_ALV2-TX_USD ). "Saldo moeda documento
                ELSE.
                  O_ALV2-SALDO_CORR       = ( O_ALV2-WRBTR * O_ALV2-TX_USD ).
                ENDIF.
              CATCH CX_SY_ZERODIVIDE.
            ENDTRY.
          ENDIF.
          WL_SALDO_MI = O_ALV2-SALDO_CORR.
          O_ALV2-VLR_AJUST = ( O_ALV2-SALDO_CORR - O_ALV2-CURR1 ).
          IF P_BUKRS = '0201' OR P_BUKRS = '0202'.
            CONTINUE.
          ENDIF.
        ELSE. "USD
          O_ALV2-CURR1 = WL_SALDO_MI.
          TRY.
              IF XTX_USD LT 0.
*              MULTIPLY O_ALV2-TX_USD BY -1.
                WL_SALDO_DOC        = ( O_ALV2-WRBTR / XTX_USD ). "converte do documento para a moeda interna
              ELSE.
                WL_SALDO_DOC        = ( O_ALV2-WRBTR * XTX_USD ).
              ENDIF.
            CATCH CX_SY_ZERODIVIDE.
          ENDTRY.

          " 06.03.2018 - Não troca sinal
*        IF WL_SALDO_DOC LT 0.
*          MULTIPLY WL_SALDO_DOC BY -1.
*        ENDIF.

          XTX_USD_A = O_ALV2-TX_USD.
          TRY.
              IF O_ALV2-TX_USD LT 0.
                MULTIPLY O_ALV2-TX_USD BY -1. "ALRS CAROL
*              IF WL_SALDO_DOC LT 0.
*                MULTIPLY WL_SALDO_DOC BY -1. "ALRS CAROL
*              ENDIF.
                O_ALV2-SALDO_CORR       = ( WL_SALDO_DOC / O_ALV2-TX_USD ). "converte moeda interna para USD

              ELSE.
                O_ALV2-SALDO_CORR       = ( WL_SALDO_DOC * O_ALV2-TX_USD ).
              ENDIF.
            CATCH CX_SY_ZERODIVIDE.
          ENDTRY.
          IF O_ALV2-SALDO_CORR LT 0 AND O_ALV2-CURR2 GT 0.
            O_ALV2-VLR_AJUST = ( O_ALV2-SALDO_CORR - ( O_ALV2-CURR2 * -1  ) ).
          ELSE.
            O_ALV2-VLR_AJUST = ( O_ALV2-SALDO_CORR - O_ALV2-CURR2 ).
          ENDIF.

        ENDIF.

        "Pega valores da Saldo ZGLT101_SALDO, trata valores positivos e negativos e soma com o saldo total da alv - PSA


        TYPES: BEGIN OF TY_ZGLT101_SALDO,
                 VLR_MI    TYPE ZGLT101_SALDO-DMBTR,
                 VLR_ME    TYPE ZGLT101_SALDO-DMBE2,
                 VLR_DOC   TYPE ZGLT101_SALDO-WRBTR,
                 SINAL_DOC TYPE ZGLT101_SALDO-SHKZG,
                 SINAL_MI  TYPE ZGLT101_SALDO-SHKZG_MI,
                 SINAL_ME  TYPE ZGLT101_SALDO-SHKZG_ME,
               END OF TY_ZGLT101_SALDO.

        DATA: VLR_AUX        TYPE STANDARD TABLE OF TY_ZGLT101_SALDO WITH HEADER LINE,
              DT_INICIO      TYPE SY-DATUM,
              DT_FIM         TYPE SY-DATUM,
              O_ALV2_AUX_MI  TYPE FAGLFLEXT-HSLVT,
              O_ALV2_AUX_ME  TYPE FAGLFLEXT-HSLVT,
              O_ALV2_AUX_DOC TYPE FAGLFLEXT-HSLVT,
              SALDO_AUX_MI   TYPE DECFLOAT34,
              SALDO_AUX_ME   TYPE DECFLOAT34,
              SALDO_AUX_DOC  TYPE DECFLOAT34. "

        DT_INICIO = P_BUDAT2+0(6) && '01'.
        DT_FIM = P_BUDAT2.

        "Verifica Data Archive - 115495 IR133267 - ZGL042 saldo bancário - Archive - PSA

        DATA(VLR_AUX_MI) = O_ALV2-CURR1.
        DATA(VLR_AUX_ME) = O_ALV2-CURR2.
        DATA(VLR_AUX_DOC) = O_ALV2-WRBTR.

        FREE: O_ALV2_AUX_MI,O_ALV2_AUX_ME,O_ALV2_AUX_DOC.

        IF SY-INDEX = 1."Somnente executar regra na linha 1 psa


          FREE: SALDO_AUX_DOC,SALDO_AUX_MI,SALDO_AUX_ME,VLR_AUX.
          CLEAR: VLR_AUX-SINAL_DOC,VLR_AUX-SINAL_ME,VLR_AUX-SINAL_MI, VLR_AUX-VLR_DOC,VLR_AUX-VLR_ME,VLR_AUX-VLR_MI.

          SELECT SHKZG_MI AS SINAL_MI, DMBTR AS VLR_MI,
              SHKZG_ME AS SINAL_ME, DMBE2 AS VLR_ME,
           SHKZG AS SINAL_DOC, WRBTR AS VLR_DOC
            FROM ZGLT101_SALDO
            INTO CORRESPONDING FIELDS OF @VLR_AUX
                WHERE BUKRS = @TL_SKB1-BUKRS AND WAERS = @O_ALV2-WAERS AND SAKNR = @TL_SKB1-SAKNR.
          ENDSELECT.


          IF VLR_AUX IS NOT INITIAL.

            FREE: O_ALV2-CURR1, O_ALV2-CURR2, O_ALV2-WRBTR.

            IF VLR_AUX-SINAL_DOC = 'H'.
              SALDO_AUX_DOC = ( VLR_AUX-VLR_DOC ) * ( -1 ).
            ELSE.
              SALDO_AUX_DOC = ( VLR_AUX-VLR_DOC ) * ( 1 ).

            ENDIF.

            IF VLR_AUX-SINAL_MI = 'H'.
              SALDO_AUX_MI = ( VLR_AUX-VLR_MI ) * ( -1 ).
            ELSE.
              SALDO_AUX_MI = ( VLR_AUX-VLR_MI ) * ( 1 ).
            ENDIF.

            IF VLR_AUX-SINAL_ME = 'H'.
              SALDO_AUX_ME = ( VLR_AUX-VLR_ME ) * ( -1 ).
            ELSE.
              SALDO_AUX_ME = ( VLR_AUX-VLR_ME ) * ( 1 ).
            ENDIF.


            O_ALV2_AUX_MI = VLR_AUX_MI + SALDO_AUX_MI.
            O_ALV2_AUX_ME = VLR_AUX_ME + SALDO_AUX_ME .
            O_ALV2_AUX_DOC = VLR_AUX_DOC + SALDO_AUX_DOC .

            O_ALV2-CURR1 = O_ALV2_AUX_MI.
            O_ALV2-CURR2 = O_ALV2_AUX_ME.
            O_ALV2-WRBTR = O_ALV2_AUX_DOC.

          ELSE.

            O_ALV2-CURR1 = VLR_AUX_MI.
            O_ALV2-CURR2 = VLR_AUX_ME.
            O_ALV2-WRBTR = VLR_AUX_DOC.

          ENDIF.

        ELSE.

          O_ALV2-CURR1 = VLR_AUX_MI.
          O_ALV2-CURR2 = VLR_AUX_ME.
          O_ALV2-WRBTR = VLR_AUX_DOC.

        ENDIF.

        FREE: VLR_AUX-SINAL_DOC,VLR_AUX-SINAL_ME,VLR_AUX-SINAL_MI.

        "Verifica Data Archive - 115495 IR133267 - ZGL042 saldo bancário - Archive - PSA
        "Movido o bloco a baixo da 5151 para que os valores já estejam atualizado!

        IF SY-INDEX = 1. "Moeda Interna
          IF O_ALV2-WRBTR  EQ 0 AND VL_MOEDA_OK IS NOT INITIAL.
            TRY.
                O_ALV2-TX_USD = XTX_USD_AUX. "ALRS 02.01.2024
                IF O_ALV2-TX_USD LT 0.
                  MULTIPLY O_ALV2-TX_USD BY -1.
                  O_ALV2-SALDO_CORR       = ( O_ALV2-CURR2 / O_ALV2-TX_USD ).
                ELSE.
                  O_ALV2-SALDO_CORR       = ( O_ALV2-CURR2 * O_ALV2-TX_USD ).
                ENDIF.
              CATCH CX_SY_ZERODIVIDE.
            ENDTRY.
          ELSE.
            TRY.
                O_ALV2-TX_USD = XTX_USD_AUX. "ALRS 02.01.2024
                IF O_ALV2-TX_USD LT 0.
                  MULTIPLY O_ALV2-TX_USD BY -1.
                  O_ALV2-SALDO_CORR       = ( O_ALV2-WRBTR / O_ALV2-TX_USD ). "Saldo moeda documento
                ELSE.
                  O_ALV2-SALDO_CORR       = ( O_ALV2-WRBTR * O_ALV2-TX_USD ).
                ENDIF.
              CATCH CX_SY_ZERODIVIDE.
            ENDTRY.
          ENDIF.
          WL_SALDO_MI = O_ALV2-SALDO_CORR.
          O_ALV2-VLR_AJUST = ( O_ALV2-SALDO_CORR - O_ALV2-CURR1 ).
          IF P_BUKRS = '0201' OR P_BUKRS = '0202'.
            CONTINUE.
          ENDIF.
        ELSE. "USD
          O_ALV2-CURR1 = WL_SALDO_MI.
          TRY.
              IF XTX_USD LT 0.
*              MULTIPLY O_ALV2-TX_USD BY -1.
                WL_SALDO_DOC        = ( O_ALV2-WRBTR / XTX_USD ). "converte do documento para a moeda interna
              ELSE.
                WL_SALDO_DOC        = ( O_ALV2-WRBTR * XTX_USD ).
              ENDIF.
            CATCH CX_SY_ZERODIVIDE.
          ENDTRY.

          " 06.03.2018 - Não troca sinal
*        IF WL_SALDO_DOC LT 0.
*          MULTIPLY WL_SALDO_DOC BY -1.
*        ENDIF.

          TRY.
              O_ALV2-TX_USD = XTX_USD_A.
              IF O_ALV2-TX_USD LT 0.
                MULTIPLY O_ALV2-TX_USD BY -1. "ALRS CAROL
*              IF WL_SALDO_DOC LT 0.
*                MULTIPLY WL_SALDO_DOC BY -1. "ALRS CAROL
*              ENDIF.
                O_ALV2-SALDO_CORR       = ( WL_SALDO_DOC / O_ALV2-TX_USD ). "converte moeda interna para USD

              ELSE.
                O_ALV2-SALDO_CORR       = ( WL_SALDO_DOC * O_ALV2-TX_USD ).
              ENDIF.
            CATCH CX_SY_ZERODIVIDE.
          ENDTRY.
          IF O_ALV2-SALDO_CORR LT 0 AND O_ALV2-CURR2 GT 0.
            O_ALV2-VLR_AJUST = ( O_ALV2-SALDO_CORR - ( O_ALV2-CURR2 * -1  ) ).
          ELSE.
            O_ALV2-VLR_AJUST = ( O_ALV2-SALDO_CORR - O_ALV2-CURR2 ).
          ENDIF.

        ENDIF.


**********************************************************************

        APPEND O_ALV2.
        CLEAR O_ALV2. " -US 156673-12-11-2024-#156673-RJF
      ENDDO.
*    CLEAR o_alv2.    " -US 156673-12-11-2024-#156673-RJF
    ENDLOOP.          " -US 156673-12-11-2024-#156673-RJF
  ENDLOOP.

  SORT O_ALV2 BY RACCT MOEDA_ATU.

  " DELETE O_ALV2 WHERE VLR_AJUST  = 0 AND LOG     NE ICON_LED_GREEN.
ENDFORM.                    " F_CONTA2
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT2 .
  DATA: WL_CURR1_AUX  TYPE DD03P-SCRTEXT_L,
        WL_CURR2_AUX  TYPE DD03P-SCRTEXT_L,
        WL_CURR3_AUX  TYPE DD03P-SCRTEXT_L,
        WL_SALDO_COR  TYPE DD03P-SCRTEXT_L,
        WL_SALDO_COR2 TYPE DD03P-SCRTEXT_L,
        WL_VLR_AJST   TYPE DD03P-SCRTEXT_L,
        WL_VLR_AJST2  TYPE DD03P-SCRTEXT_L.


  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'O_ALV2'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

*STATUS
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'LOG'.
  WA_AFIELD-ICON          = 'X'.
  WA_AFIELD-HOTSPOT  = 'X'.
  WA_AFIELD-SCRTEXT_S = 'St.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = 'X'.
  WA_AFIELD-OUTPUTLEN = 06.
  APPEND WA_AFIELD TO IT_FIELDCAT.
  CLEAR WA_AFIELD-ICON.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'RACCT'.
  WA_AFIELD-SCRTEXT_S = TEXT-A07.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = 'X'.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'RASSC'.
  WA_AFIELD-SCRTEXT_S = TEXT-A34.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = 'X'.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.
  CLEAR WA_AFIELD-ICON.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TXT50'.
  WA_AFIELD-SCRTEXT_S = TEXT-A04.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 25.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.
  CLEAR:   WA_AFIELD-REF_FIELD ,  WA_AFIELD-REF_TABLE.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'WAERS'.
  WA_AFIELD-SCRTEXT_S = TEXT-A10.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-OUTPUTLEN = 10.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.
  CLEAR:   WA_AFIELD-REF_FIELD ,  WA_AFIELD-REF_TABLE.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'KTOKS'.
  WA_AFIELD-SCRTEXT_S = TEXT-A35.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*moeda de atualização
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MOEDA_ATU'.
  WA_AFIELD-SCRTEXT_S = TEXT-A38.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 15.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*Saldo DOC
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'WRBTR'.
  WA_AFIELD-SCRTEXT_M = TEXT-A48.
  WA_AFIELD-SCRTEXT_L = TEXT-A48.
  WA_AFIELD-SCRTEXT_S = TEXT-A48.
  WA_AFIELD-KEY           = ' '.
  WA_AFIELD-DO_SUM        = ''.
  WA_AFIELD-OUTPUTLEN     = 18.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  " Saldo Moeda Interna
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CURR1'.
  WA_AFIELD-SCRTEXT_S = TEXT-A49.
  WA_AFIELD-SCRTEXT_L = TEXT-A49.
  WA_AFIELD-SCRTEXT_M = TEXT-A49.
  WA_AFIELD-OUTPUTLEN = 18.
  WA_AFIELD-KEY       = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  " Saldo moeda forte
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CURR2'.
  WA_AFIELD-SCRTEXT_S = TEXT-A50.
  WA_AFIELD-SCRTEXT_L = TEXT-A50.
  WA_AFIELD-SCRTEXT_M = TEXT-A50.
  WA_AFIELD-KEY       = ''.
  WA_AFIELD-OUTPUTLEN = 18.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  " taxa
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TX_USD'.
  WA_AFIELD-SCRTEXT_S = TEXT-A17.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY       = ''.
  WA_AFIELD-OUTPUTLEN = 12.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  " Saldo Corrigido
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SALDO_CORR'.
  WA_AFIELD-SCRTEXT_S = TEXT-A51.
  WA_AFIELD-SCRTEXT_L = TEXT-A51.
  WA_AFIELD-SCRTEXT_M = TEXT-A51.
  WA_AFIELD-KEY       = ''.
  WA_AFIELD-OUTPUTLEN = 18.
  APPEND WA_AFIELD TO IT_FIELDCAT.
  CLEAR: WA_AFIELD-REF_FIELD, WA_AFIELD-REF_TABLE.

  "Valor do Ajuste
  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_AJUST'.
  WA_AFIELD-SCRTEXT_S = TEXT-A52.
  WA_AFIELD-SCRTEXT_L = TEXT-A52.
  WA_AFIELD-SCRTEXT_M = TEXT-A52.
  WA_AFIELD-KEY       = ''.
  WA_AFIELD-OUTPUTLEN = 18.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BELNR'.
  WA_AFIELD-SCRTEXT_S = TEXT-A05.
  WA_AFIELD-SCRTEXT_L = TEXT-A05.
  WA_AFIELD-SCRTEXT_M = TEXT-A05.
  WA_AFIELD-KEY       = ''.
  WA_AFIELD-HOTSPOT   = 'X'.
  WA_AFIELD-OUTPUTLEN = 12.
  APPEND WA_AFIELD TO IT_FIELDCAT.
*
*  I = I + 1.
*  CLEAR WA_AFIELD.
*  WA_AFIELD-COL_POS       = I.
*  WA_AFIELD-FIELDNAME     = 'BELNR_EST'.
*  WA_AFIELD-SCRTEXT_S = TEXT-A37.
*  WA_AFIELD-SCRTEXT_L = TEXT-A37.
*  WA_AFIELD-SCRTEXT_M = TEXT-A37.
*  WA_AFIELD-KEY           = ' '.
*  WA_AFIELD-DO_SUM        = ''.
*  WA_AFIELD-HOTSPOT  = 'X'.
*  WA_AFIELD-OUTPUTLEN = 12.
*  APPEND WA_AFIELD TO IT_FIELDCAT.
ENDFORM.                    " F_ALV_FIELDCAT2
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  IF P_E_LANC = 'X' .
    APPEND 'ESTORNO' TO FCODE.
  ENDIF.
  IF  P_V_LANC = 'X'.
    APPEND 'ESTORNO' TO FCODE.
    APPEND 'LANCAR' TO FCODE.
    APPEND '&REFRESH' TO FCODE.
  ENDIF.
  SET PF-STATUS 'F_SET_PF' EXCLUDING FCODE.

  SET TITLEBAR  'ZFTITLE'.

  IF CL_CONTAINER_95 IS INITIAL.
    CREATE OBJECT CL_CONTAINER_95
      EXPORTING
        SIDE  = '4'
        RATIO = '80'.

  ENDIF.

  IF NOT CL_GRID IS INITIAL.
    PERFORM ZF_ALV_HEADER USING '2'.
    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE      =
*        I_SOFT_REFRESH =
*      EXCEPTIONS
*        FINISHED       = 1
*        OTHERS         = 2
      .
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
        NO_MARGINS = 'X'.


    PERFORM ZF_ALV_HEADER USING '1'.


    IF EDITCONTAINER IS INITIAL .
      CREATE OBJECT EDITCONTAINER
        EXPORTING
          CONTAINER_NAME = 'HEADER'.
    ENDIF .

    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.

    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT      = CL_CONTAINER_95
*       I_PARENT      = CL_CONTAINER
        I_APPL_EVENTS = 'X'.


    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    WA_LAYOUT-ZEBRA      = 'X'.
    WA_LAYOUT-NO_ROWMOVE = 'X'.
    WA_LAYOUT-NO_ROWINS  = 'X'.
    WA_LAYOUT-NO_ROWMARK = SPACE.
    IF P_E_LANC = 'X'. "Se for estorno
      WA_LAYOUT-GRID_TITLE = 'Estorno de lançamentos'.
    ELSEIF P_C_LANC = 'X'.
      WA_LAYOUT-GRID_TITLE = 'Criação de lançamentos'.
    ELSE.
      CLEAR WA_LAYOUT-GRID_TITLE .
    ENDIF.
    WA_LAYOUT-SEL_MODE   = 'A'.
    WA_LAYOUT-CWIDTH_OPT = ''.
    WA_LAYOUT-BOX_FNAME  = 'MARK'.

    "WG_SAVE             = 'X'.
*    WG_X_VARIANT-REPORT = SY-REPID.
    CLEAR WG_X_VARIANT-REPORT .

    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT      = WG_X_VARIANT
*       I_SAVE          = WG_SAVE
        IS_LAYOUT       = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT[]
*       IT_SORT         = I_SORT[]
        IT_OUTTAB       = O_ALV2[].


    CREATE OBJECT EVENT_RECEIVER.
    SET HANDLER EVENT_RECEIVER->CATCH_HOTSPOT      FOR CL_GRID.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOP_OF_PAGE FOR CL_GRID.

  ENDIF.

ENDMODULE.                 " STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.

  IF NOT CL_GRID IS INITIAL.
    CALL METHOD CL_GRID->DISPATCH
      EXPORTING
        CARGO         = SY-UCOMM
        EVENTID       = 19
        IS_SHELLEVENT = ' '.

    IF SY-UCOMM IS INITIAL.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = IS_STABLE.
    ENDIF.
  ENDIF.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH O_ALV2.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN '&REFRESH'.
      PERFORM F_CONTA2.
    WHEN 'LANCAR' OR 'ESTORNO'.
      WG_ACAO = SY-UCOMM.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          I_BUKRS  = P_BUKRS
          I_DATA   = P_BUDAT
*         I_DEP_RESP = VG_DEPTO
        IMPORTING
          E_STATUS = E_STATUS
          E_MESSA  = E_MESSA
        EXCEPTIONS
          ERROR    = 1
          OTHERS   = 2.
      IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF  E_STATUS = 'E'.
        MESSAGE E398(00) WITH E_MESSA.
      ENDIF.


      IF T_T030H[] IS INITIAL.
        SELECT KTOPL HKONT WAERS CURTP LSBEW LHBEW FROM T030H
          INTO TABLE T_T030H
          FOR ALL ENTRIES IN O_ALV2
          WHERE KTOPL = C_0050 AND
                HKONT = O_ALV2-RACCT.
      ENDIF.

      REFRESH INDROW.
      CALL METHOD CL_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = INDROW.

      LOOP AT O_ALV2.
        O_ALV2-MARK = ' '.
        MODIFY O_ALV2.
      ENDLOOP.

      LOOP AT INDROW INTO W_IND.
        READ TABLE O_ALV2 INDEX W_IND-INDEX.
        O_ALV2-MARK = 'X'.
        MODIFY O_ALV2 INDEX W_IND-INDEX.
      ENDLOOP.
      " BUG 160280 - BG - INICIOO
*      LOOP AT o_alv2 WHERE mark = 'X'.
*
*        DATA(lv_lines) = REDUCE i( INIT x = 0 FOR wa IN o_alv2[]
*                    WHERE ( racct = o_alv2-racct ) NEXT x = x + 1 ).
*
*        wl_tabix = sy-tabix.
*        IF o_alv2-log   IS INITIAL          "primeiro processamento
*          OR o_alv2-log = icon_incomplete   "Erro contabilização  - reprocessa
*          OR ( o_alv2-log = icon_system_undo AND  wg_acao = 'ESTORNO' ) "Feito o estorno - reprocessa
*          OR ( o_alv2-log = icon_led_green AND wg_acao = 'ESTORNO' ). "Se lançamento OK permite estorno
*
*          IF wg_acao = 'LANCAR' AND o_alv2-log NE icon_incomplete.
*            IF o_alv2-moeda_atu+0(2) = '01'.
*              wa_zgl012_avm-moeda_atu = ''.
*            ELSE.
*              wa_zgl012_avm-moeda_atu =  c_usd.
*            ENDIF.
*            SELECT SINGLE *
*              FROM  zgl012_avm
*              INTO  wa_zgl012_avm
*              WHERE bukrs     EQ  p_bukrs
*              AND   dt_aval   EQ p_budat
*              AND   belnr     EQ  o_alv2-racct
*              "and   VBUND      eq o_alv2-RASSC " BUG 160280 - BG
*              AND   moeda_atu EQ wa_zgl012_avm-moeda_atu
*              AND   obj_key   NE ''.
*            IF sy-subrc = 0.
*              CONTINUE.
*            ENDIF.
*          ENDIF.
*
*          PERFORM zf_lancar2 CHANGING o_alv2 wl_erro.
*          CLEAR: o_alv2-obj_key, o_alv2-obj_key_est.
*          IF wg_acao = 'ESTORNO'.
**            O_ALV2-OBJ_KEY_EST = VOBJ_KEY.
*            CLEAR o_alv2-obj_key.
*          ELSE.
*            o_alv2-obj_key     = vobj_key.
*          ENDIF.
*          o_alv2-log     = icon_message_warning_small.
*          MODIFY o_alv2 INDEX wl_tabix TRANSPORTING obj_key obj_key_est log.
*        ENDIF.
*      ENDLOOP.
      " BUG 160280 - BG - FIMM

      LOOP AT O_ALV2 INTO DATA(WA_ALV2) WHERE MARK = 'X'.

        DATA(LV_LINES) = REDUCE I( INIT X = 0 FOR WA IN O_ALV2[]
                    WHERE ( RACCT = WA_ALV2-RACCT ) NEXT X = X + 1 ).

        WL_TABIX = SY-TABIX.
        IF WA_ALV2-LOG   IS INITIAL          "primeiro processamento
          OR WA_ALV2-LOG = ICON_INCOMPLETE   "Erro contabilização  - reprocessa
          OR ( WA_ALV2-LOG = ICON_SYSTEM_UNDO AND  WG_ACAO = 'ESTORNO' ) "Feito o estorno - reprocessa
          OR ( WA_ALV2-LOG = ICON_LED_GREEN AND WG_ACAO = 'ESTORNO' ). "Se lançamento OK permite estorno

          IF WG_ACAO = 'LANCAR' AND WA_ALV2-LOG NE ICON_INCOMPLETE.
            IF WA_ALV2-MOEDA_ATU+0(2) = '01'.
              WA_ZGL012_AVM-MOEDA_ATU = ''.
            ELSE.
              WA_ZGL012_AVM-MOEDA_ATU =  C_USD.
            ENDIF.
            SELECT SINGLE *
              FROM  ZGL012_AVM
              INTO  WA_ZGL012_AVM
              WHERE BUKRS     EQ  P_BUKRS
              AND   DT_AVAL   EQ P_BUDAT
              AND   BELNR     EQ  WA_ALV2-RACCT
              AND   MOEDA_ATU EQ WA_ZGL012_AVM-MOEDA_ATU
              AND   OBJ_KEY   NE ''.
            IF SY-SUBRC = 0.
              CONTINUE.
            ENDIF.
          ENDIF.

          PERFORM ZF_LANCAR2 CHANGING WA_ALV2 WL_ERRO.
          CLEAR: WA_ALV2-OBJ_KEY, WA_ALV2-OBJ_KEY_EST.
          IF WG_ACAO = 'ESTORNO'.
*            O_ALV2-OBJ_KEY_EST = VOBJ_KEY.
            CLEAR WA_ALV2-OBJ_KEY.
          ELSE.
            WA_ALV2-OBJ_KEY     = VOBJ_KEY.
          ENDIF.
          WA_ALV2-LOG     = ICON_MESSAGE_WARNING_SMALL.
          MODIFY O_ALV2 INDEX WL_TABIX TRANSPORTING OBJ_KEY OBJ_KEY_EST LOG.
        ENDIF.
      ENDLOOP.

    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_LANCAR2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_O_ALV2  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM ZF_LANCAR2 CHANGING P_ALV LIKE O_ALV2
                         P_ERRO.

  DATA: WL_DATA(10),
        V_STBLG     TYPE BKPF-STBLG,
        TABIX       TYPE SY-TABIX,
        V_MOEDA_ATU TYPE ZGL012_AVM-MOEDA_ATU,
        V_CONTADOR  TYPE I.

  WRITE VG_LAST_DAY TO WL_DATA.

  SELECT SINGLE BUKRS LAND1
    FROM T001
    INTO WA_T001
    WHERE BUKRS = P_BUKRS.

  "'US'
*  IF P_BUKRS = '0004'.
*    WA_T001-LAND1 = 'US'.
*  ENDIF.


  SELECT SINGLE LAND1 WAERS
    FROM T005
    INTO WA_T005
    WHERE LAND1 = WA_T001-LAND1.

  IF O_ALV2-MOEDA_ATU+0(2) = '01'.
    CLEAR V_MOEDA_ATU.
  ELSE.
    V_MOEDA_ATU = C_USD.
  ENDIF.

  REFRESH  IT_ZIB_CONTABIL.
  IF WG_ACAO = 'ESTORNO'.
    CLEAR: V_STBLG, P_ERRO.
    VOBJ_KEY = P_ALV-OBJ_KEY.

    SUBMIT Z_FB08_ZGL042 WITH P_OBJ = P_ALV-OBJ_KEY
    AND RETURN.


    SELECT SINGLE STBLG
      FROM ZIB_CONTABIL_CHV
      INNER JOIN BKPF
      ON  BKPF~BUKRS = ZIB_CONTABIL_CHV~BUKRS
      AND BKPF~BELNR = ZIB_CONTABIL_CHV~BELNR
      AND BKPF~GJAHR = ZIB_CONTABIL_CHV~GJAHR
      INTO V_STBLG
      WHERE ZIB_CONTABIL_CHV~OBJ_KEY = P_ALV-OBJ_KEY.

    IF V_STBLG IS INITIAL. "Não estornou
      P_ERRO = 'X'.
      EXIT.
    ENDIF.
    DELETE FROM ZGL012_AVM WHERE OBJ_KEY = P_ALV-OBJ_KEY.
    COMMIT WORK.
    EXIT.
  ELSE.
    "BUG 160280 - BG - INICIO
    "do LV_LINES TIMES.
*    LOOP AT o_alv2[] into data(w_alv2) WHERE RACCT eq o_alv2-RACCT.
*

*    DO 2 TIMES.
*      IF sy-index = 1.
*        CALL FUNCTION 'NUMBER_GET_NEXT'
*          EXPORTING
*            nr_range_nr = '01'
*            object      = 'ZID_GL'
*          IMPORTING
*            number      = vseq.
*        vnum = vseq .
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = vnum
*          IMPORTING
*            output = vnum.
*        CONCATENATE 'ZGL012' vnum  p_spmon+0(4) INTO wa_zib_contabil-obj_key.
*        vobj_key = wa_zib_contabil-obj_key.
*      ENDIF.
*
*      vseqitem = sy-index.
*      wa_zib_contabil-seqitem   = vseqitem.
*      IF p_alv-vlr_ajust  < 0.
*        wa_zib_contabil-xblnr = TEXT-a58. "'Receita'.
*      ELSE.
*        wa_zib_contabil-xblnr = TEXT-a59. "'Despesa'.
*      ENDIF.
*
*      IF sy-index = 1.
*        IF p_alv-vlr_ajust LT 0.
*          wa_zib_contabil-bschl      = '50'.
*        ELSE.
*          wa_zib_contabil-bschl      = '40'.
*        ENDIF.
*      ELSE.
*        IF p_alv-vlr_ajust LT 0.
*          wa_zib_contabil-bschl      = '40'.
*        ELSE.
*          wa_zib_contabil-bschl      = '50'.
*        ENDIF.
*      ENDIF.
*
*      CLEAR wa_zib_contabil-bupla.
*      IF p_bukrs = '0100'.
*        wa_zib_contabil-gsber = 'T001'.
*      ELSEIF p_bukrs = '0101'.
*        wa_zib_contabil-gsber = 'F101'.
*      ELSEIF p_bukrs = '0200'.
*        wa_zib_contabil-gsber = 'S201'.
*      ELSEIF p_bukrs = '0201'.
*        wa_zib_contabil-gsber = 'H201'.
*      ELSEIF p_bukrs = '0202'.
*        wa_zib_contabil-gsber = 'H202'.
*      ELSEIF p_bukrs = '0203'.
*        wa_zib_contabil-gsber = 'L203'.
*      ELSE.
*        CONCATENATE p_bukrs+2(2) '01' INTO wa_zib_contabil-gsber.
*        CONCATENATE p_bukrs+2(2) '01' INTO wa_zib_contabil-bupla.
*      ENDIF.
*
*      wa_zib_contabil-bukrs     = p_bukrs.
*      wa_zib_contabil-interface = '35'.
*      CONCATENATE p_spmon+4(2) p_spmon+0(4) INTO wa_zib_contabil-bktxt SEPARATED BY '.'.
*      wa_zib_contabil-bldat     = wl_data.
*      wa_zib_contabil-budat     = wl_data.
*      wa_zib_contabil-gjahr     = p_spmon+0(4).
*      wa_zib_contabil-monat     = p_spmon+4(2).
*      wa_zib_contabil-blart     = 'VC'.
*      IF sy-index = 1.
*        wa_zib_contabil-hkont     = p_alv-racct.
*      ELSE.
*        READ TABLE t_t030h INTO wa_t030h
*        WITH KEY hkont = p_alv-racct.
*        IF p_alv-vlr_ajust LT 0.
*          wa_zib_contabil-hkont     = wa_t030h-lsbew. "despesa
*        ELSE.
*          wa_zib_contabil-hkont     = wa_t030h-lhbew. "receita
*        ENDIF.
*      ENDIF.
*
*      wa_zib_contabil-VBUND = p_alv-RASSC. " BUG 160280 - BG
*
**      CONCATENATE 'Var. Monetária-'  P_BUDAT+4(2) '/' P_BUDAT+0(4) INTO WA_ZIB_CONTABIL-SGTXT.
*      CONCATENATE TEXT-a56  p_budat+4(2) '/' p_budat+0(4) INTO wa_zib_contabil-sgtxt.
*
*      wa_zib_contabil-wrbtr   = 0.
*      wa_zib_contabil-waers   = p_alv-waers.
*      wa_zib_contabil-waers_i = 'X'.
*
*      IF o_alv2-moeda_atu+0(2) = '01'. "Interna
*        wa_zib_contabil-dmbtr     = p_alv-vlr_ajust.
*        IF wa_zib_contabil-dmbtr LT 0.
*          MULTIPLY wa_zib_contabil-dmbtr BY -1.
*        ENDIF.
*        wa_zib_contabil-waers_f   = 'USD'.
*        wa_zib_contabil-dmbe2     = 0.
*      ELSE.
*        wa_zib_contabil-dmbtr     = 0.
*        wa_zib_contabil-waers_f   = 'USD'.
*        wa_zib_contabil-dmbe2     = p_alv-vlr_ajust.
*        IF wa_zib_contabil-dmbe2 LT 0.
*          MULTIPLY wa_zib_contabil-dmbe2 BY -1.
*        ENDIF.
*      ENDIF.
*
*      wa_zib_contabil-rg_atualizado  = 'N'.
*      wa_zib_contabil-bewar     = ''. "WL_BEWAR.
*      wa_zib_contabil-bktxt     = sy-uname.
*
*      APPEND wa_zib_contabil TO it_zib_contabil.
*    ENDDO.
*      ENDLOOP.
    "

    V_CONTADOR = 1.
    LOOP AT O_ALV2[] INTO DATA(W_ALV2) WHERE RACCT EQ O_ALV2-RACCT.
      DO 2 TIMES.
        IF V_CONTADOR = 1.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              NR_RANGE_NR = '01'
              OBJECT      = 'ZID_GL'
            IMPORTING
              NUMBER      = VSEQ.
          VNUM = VSEQ .

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = VNUM
            IMPORTING
              OUTPUT = VNUM.
          CONCATENATE 'ZGL012' VNUM  P_SPMON+0(4) INTO WA_ZIB_CONTABIL-OBJ_KEY.
          VOBJ_KEY = WA_ZIB_CONTABIL-OBJ_KEY.
        ENDIF.

        VSEQITEM = V_CONTADOR.
        WA_ZIB_CONTABIL-SEQITEM   = VSEQITEM.
        IF W_ALV2-VLR_AJUST  < 0.
          WA_ZIB_CONTABIL-XBLNR = TEXT-A58. "'Receita'.
        ELSE.
          WA_ZIB_CONTABIL-XBLNR = TEXT-A59. "'Despesa'.
        ENDIF.

        IF SY-INDEX = 1.
          IF W_ALV2-VLR_AJUST LT 0.
            WA_ZIB_CONTABIL-BSCHL      = '50'.
          ELSE.
            WA_ZIB_CONTABIL-BSCHL      = '40'.
          ENDIF.
        ELSE.
          IF W_ALV2-VLR_AJUST LT 0.
            WA_ZIB_CONTABIL-BSCHL      = '40'.
          ELSE.
            WA_ZIB_CONTABIL-BSCHL      = '50'.
          ENDIF.
        ENDIF.

        CLEAR WA_ZIB_CONTABIL-BUPLA.
        IF P_BUKRS = '0100'.
          WA_ZIB_CONTABIL-GSBER = 'T001'.
        ELSEIF P_BUKRS = '0101'.
          WA_ZIB_CONTABIL-GSBER = 'F101'.
        ELSEIF P_BUKRS = '0200'.
          WA_ZIB_CONTABIL-GSBER = 'S201'.
        ELSEIF P_BUKRS = '0201'.
          WA_ZIB_CONTABIL-GSBER = 'H201'.
        ELSEIF P_BUKRS = '0202'.
          WA_ZIB_CONTABIL-GSBER = 'H202'.
        ELSEIF P_BUKRS = '0203'.
          WA_ZIB_CONTABIL-GSBER = 'L203'.
        ELSE.
          CONCATENATE P_BUKRS+2(2) '01' INTO WA_ZIB_CONTABIL-GSBER.
          CONCATENATE P_BUKRS+2(2) '01' INTO WA_ZIB_CONTABIL-BUPLA.
        ENDIF.

        WA_ZIB_CONTABIL-BUKRS     = P_BUKRS.
        WA_ZIB_CONTABIL-INTERFACE = '35'.
        CONCATENATE P_SPMON+4(2) P_SPMON+0(4) INTO WA_ZIB_CONTABIL-BKTXT SEPARATED BY '.'.
        WA_ZIB_CONTABIL-BLDAT     = WL_DATA.
        WA_ZIB_CONTABIL-BUDAT     = WL_DATA.
        WA_ZIB_CONTABIL-GJAHR     = P_SPMON+0(4).
        WA_ZIB_CONTABIL-MONAT     = P_SPMON+4(2).
        WA_ZIB_CONTABIL-BLART     = 'VC'.
        IF SY-INDEX = 1.
          WA_ZIB_CONTABIL-HKONT     = W_ALV2-RACCT.
        ELSE.
          READ TABLE T_T030H INTO WA_T030H
          WITH KEY HKONT = W_ALV2-RACCT.
          IF W_ALV2-VLR_AJUST LT 0.
            WA_ZIB_CONTABIL-HKONT     = WA_T030H-LSBEW. "despesa
          ELSE.
            WA_ZIB_CONTABIL-HKONT     = WA_T030H-LHBEW. "receita
          ENDIF.
        ENDIF.

        WA_ZIB_CONTABIL-VBUND = W_ALV2-RASSC. " BUG 160280 - BG

*      CONCATENATE 'Var. Monetária-'  P_BUDAT+4(2) '/' P_BUDAT+0(4) INTO WA_ZIB_CONTABIL-SGTXT.
        CONCATENATE TEXT-A56  P_BUDAT+4(2) '/' P_BUDAT+0(4) INTO WA_ZIB_CONTABIL-SGTXT.

        WA_ZIB_CONTABIL-WRBTR   = 0.
        WA_ZIB_CONTABIL-WAERS   = W_ALV2-WAERS.
        WA_ZIB_CONTABIL-WAERS_I = 'X'.

        IF O_ALV2-MOEDA_ATU+0(2) = '01'. "Interna
          WA_ZIB_CONTABIL-DMBTR     = W_ALV2-VLR_AJUST.
          IF WA_ZIB_CONTABIL-DMBTR LT 0.
            MULTIPLY WA_ZIB_CONTABIL-DMBTR BY -1.
          ENDIF.
          WA_ZIB_CONTABIL-WAERS_F   = 'USD'.
          WA_ZIB_CONTABIL-DMBE2     = 0.
        ELSE.
          WA_ZIB_CONTABIL-DMBTR     = 0.
          WA_ZIB_CONTABIL-WAERS_F   = 'USD'.
          WA_ZIB_CONTABIL-DMBE2     = W_ALV2-VLR_AJUST.
          IF WA_ZIB_CONTABIL-DMBE2 LT 0.
            MULTIPLY WA_ZIB_CONTABIL-DMBE2 BY -1.
          ENDIF.
        ENDIF.

        WA_ZIB_CONTABIL-RG_ATUALIZADO  = 'N'.
        WA_ZIB_CONTABIL-BEWAR     = ''. "WL_BEWAR.
        WA_ZIB_CONTABIL-BKTXT     = SY-UNAME.

        APPEND WA_ZIB_CONTABIL TO IT_ZIB_CONTABIL.
        V_CONTADOR = V_CONTADOR + 1.
      ENDDO.
    ENDLOOP.
    " BUG 160280 - BG - FIM
    MODIFY ZIB_CONTABIL FROM TABLE IT_ZIB_CONTABIL.
    REFRESH IT_ZIB_CONTABIL.

  ENDIF.

  "Grava Z
  IF P_ALV-VLR_AJUST NE 0.
    WA_ZGL012_AVM-BUKRS             = P_BUKRS.
    WA_ZGL012_AVM-DT_AVAL           = P_BUDAT.
    WA_ZGL012_AVM-BELNR             = P_ALV-RACCT.
    WA_ZGL012_AVM-MOEDA_ATU         = V_MOEDA_ATU.
    WA_ZGL012_AVM-BUZEI             = 0.
    WA_ZGL012_AVM-BSCHL             = ''.
    WA_ZGL012_AVM-KUNNR             = ''.
    WA_ZGL012_AVM-LIFNR             = ''.
    WA_ZGL012_AVM-HKONT             = ''.
    WA_ZGL012_AVM-VBUND             = P_ALV-RASSC. " BUG 160280 - BG
    WA_ZGL012_AVM-UMSKZ             = ''.
    WA_ZGL012_AVM-BUDAT             = VG_LAST_DAY.
    WA_ZGL012_AVM-WAERS             = P_ALV-WAERS.
    WA_ZGL012_AVM-GSBER             = ''.
    WA_ZGL012_AVM-DMBTR             = 0.
    WA_ZGL012_AVM-WRBTR             = 0.
    WA_ZGL012_AVM-DMBE2             = 0.
    WA_ZGL012_AVM-KURSF             = 0.
    WA_ZGL012_AVM-AUGDT             = ''.
    WA_ZGL012_AVM-AUGBL             = ''.
    WA_ZGL012_AVM-TX_FECH           = 0.
    WA_ZGL012_AVM-VLR_ATUALIZADO    = 0.
    WA_ZGL012_AVM-VLR_ACUM_MES_ANT  = 0.
    WA_ZGL012_AVM-VLR_ACUM_MES_ATU  = 0.
    WA_ZGL012_AVM-VLR_VARIACAO      = P_ALV-VLR_AJUST.
    WA_ZGL012_AVM-RESULTADO         = ''.
    WA_ZGL012_AVM-DT_LCTO           = ''.
    WA_ZGL012_AVM-DOC_LCTO          = ''.

    IF WG_ACAO = 'ESTORNO'.
      WA_ZGL012_AVM-ESTORNO           = ''.
*      WA_ZGL012_AVM-ESTORNO           = 'X'.
      CLEAR VOBJ_KEY.
    ELSE.
      WA_ZGL012_AVM-ESTORNO           = ''.
    ENDIF.
    WA_ZGL012_AVM-ST_REV            = 'S'. "Marcar como "S" saldo conta

    MOVE: SY-UNAME TO WA_ZGL012_AVM-USNAM,
          SY-UZEIT TO WA_ZGL012_AVM-CPUTM,
          SY-DATUM TO WA_ZGL012_AVM-CPUDT,
          VOBJ_KEY TO WA_ZGL012_AVM-OBJ_KEY.

    MODIFY ZGL012_AVM FROM WA_ZGL012_AVM.
    IF SY-SUBRC = 0.
      COMMIT WORK AND WAIT .
    ENDIF.
  ELSE.
    IF WG_ACAO = 'ESTORNO'.
*      WA_ZGL012_AVM-ESTORNO           = 'X'.
      WA_ZGL012_AVM-ESTORNO           = ''.
      CLEAR VOBJ_KEY.
    ELSE.
      WA_ZGL012_AVM-ESTORNO           = ''.
    ENDIF.
    UPDATE ZGL012_AVM SET OBJ_KEY = VOBJ_KEY
                          ESTORNO = WA_ZGL012_AVM-ESTORNO
                          USNAM   = SY-UNAME
                          CPUTM   = SY-UZEIT
                          CPUDT   = SY-DATUM
    WHERE  BUKRS             = P_BUKRS
    AND    DT_AVAL           = P_BUDAT
    AND    BELNR             = P_ALV-RACCT
    AND    MOEDA_ATU         = V_MOEDA_ATU.
    IF SY-SUBRC = 0.
      COMMIT WORK AND WAIT .
    ENDIF.
  ENDIF.

ENDFORM.                    "ZF_LANCAR2
*&---------------------------------------------------------------------*
*&      Form  EXEC_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ALV_OBJ_KEY  text
*----------------------------------------------------------------------*
*FORM EXEC_SHDB  USING    P_OBJ_KEY.
*  DATA: WL_SETLEAF  TYPE SETLEAF,
*          I_HEAD      TYPE TBTCJOB.
*
*  DATA:   WL_JOB_ID   LIKE TBTCJOB-JOBCOUNT.
*  DATA:   WL_JOBN(32).
*
*  DATA: BEGIN OF I_STEPLIST OCCURS 10.
*          INCLUDE STRUCTURE TBTCSTEP.
*  DATA: END OF I_STEPLIST.
*  DATA : C_NO(1) TYPE C . "value 'N', " Criação do job
*
*  DATA: WL_TBTCJOB  TYPE  TBTCJOB,
*        WL_TBTCSTRT TYPE  TBTCSTRT.
*
*  DATA: LV_REPNAME LIKE  RSVAR-REPORT.           " for variant handling
*  DATA: IV_VARNAME LIKE  RALDB-VARIANT VALUE 'SAP_UPGRADE'.
*  DATA: IV_VARIANTTEXT  LIKE  VARIT-VTEXT VALUE 'Upgrade variant'.
*  DATA: WL_SUBRC TYPE SY-SUBRC.
*  DATA: TT_REPORTPARAM TYPE TABLE OF  RSPARAMS WITH HEADER LINE.
*
*  SELECT SINGLE *
*   FROM SETLEAF
*   INTO WL_SETLEAF
*    WHERE SETNAME EQ 'MAGGI_JOB_USER'.
*
*  IF SY-SUBRC NE 0.
*    MESSAGE TEXT-E01 TYPE 'E'.
*    EXIT.
*  ENDIF.
*  CONCATENATE 'Z_FB08_ZGL042' SY-TCODE  INTO WL_JOBN SEPARATED BY '|'.
*
*  I_HEAD-JOBNAME = WL_JOBN. " Nome do JOBi_head-sdlstrtdt = sy-datum. " Dia
*  I_HEAD-SDLSTRTTM = SY-UZEIT + 20. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
*  I_HEAD-STEPCOUNT = 1.
*
*  TT_REPORTPARAM-SELNAME = 'P_OBJ'.
*  TT_REPORTPARAM-KIND =  'P'.
*  TT_REPORTPARAM-SIGN = 'I'.
*  TT_REPORTPARAM-OPTION = 'EQ'.
*  TT_REPORTPARAM-LOW = P_OBJ_KEY.
*  APPEND TT_REPORTPARAM.
*  CLEAR TT_REPORTPARAM.
*
*  LV_REPNAME = 'Z_FB08_ZGL042'.
**    Write the variant first (Insert or Update)
*  CALL FUNCTION 'SUBST_WRITE_UPGRADE_VARIANT'
*    EXPORTING
*      IV_REPORTNAME         = LV_REPNAME
*      IV_VARIANTNAME        = IV_VARNAME
*      IV_VARIANTTEXT        = IV_VARIANTTEXT
*    IMPORTING
*      EV_FUNCRC             = WL_SUBRC
*    TABLES
*      TT_REPORTPARAM        = TT_REPORTPARAM
*    EXCEPTIONS
*      EXIST_CHECK_FAILED    = 1
*      UPDATE_FAILED         = 2
*      UPDATE_NOT_AUTHORIZED = 3
*      UPDATE_NO_REPORT      = 4
*      UPDATE_NO_VARIANT     = 5
*      UPDATE_VARIANT_LOCKED = 6
*      INSERT_FAILED         = 7
*      INSERT_NOT_AUTHORIZED = 8
*      INSERT_NO_REPORT      = 9
*      INSERT_VARIANT_EXISTS = 10
*      INSERT_VARIANT_LOCKED = 11
*      OTHERS                = 12.
*
*  I_STEPLIST-PARAMETER = IV_VARNAME. " Nome da variante
*  I_STEPLIST-PROGRAM = 'Z_FB08_ZGL042'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
*  I_STEPLIST-TYP = 'A'. " Tipo de Job
*  I_STEPLIST-AUTHCKNAM = WL_SETLEAF-VALFROM.
*  I_STEPLIST-LANGUAGE = SY-LANGU.
*  I_STEPLIST-ARCUSER = WL_SETLEAF-VALFROM.
*
*  APPEND I_STEPLIST.
*
*
*  C_NO = 'N'.
*  CALL FUNCTION 'BP_JOB_CREATE'
*    EXPORTING
*      JOB_CR_DIALOG       = C_NO " Coloque 'Y' se quiser ver
*      JOB_CR_HEAD_INP     = I_HEAD " os valores atribuidos
*    IMPORTING
*      JOB_CR_HEAD_OUT     = WL_TBTCJOB
*      JOB_CR_STDT_OUT     = WL_TBTCSTRT
*    TABLES
*      JOB_CR_STEPLIST     = I_STEPLIST
*    EXCEPTIONS
*      CANT_CREATE_JOB     = 1
*      INVALID_DIALOG_TYPE = 2
*      INVALID_JOB_DATA    = 3
*      JOB_CREATE_CANCELED = 4
*      OTHERS              = 5.
*
*  CALL FUNCTION 'JOB_CLOSE'
*    EXPORTING
*      JOBNAME   = WL_JOBN
*      JOBCOUNT  = WL_TBTCJOB-JOBCOUNT
*      STRTIMMED = 'X'.
*ENDFORM.                    " EXEC_SHDB
