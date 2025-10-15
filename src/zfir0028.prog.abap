
*&---------------------------------------------------------------------*
*& Report  ZFIR0028
*&
*&---------------------------------------------------------------------*
*&TITULO: INVOICE - CONTAS A PAGAR
*&Liquidação Operação – Recebimento (Maggi International)
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 04.06.2013
*TRANSACAO: ZFI0021
*&---------------------------------------------------------------------*
REPORT  ZFIR0028.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: BSID.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.
DATA: WL_MODE(1).


TYPES:
  BEGIN OF TY_LOTE,
    LOTE    TYPE ZFIT0036-LOTE,
    DT_PGTO TYPE ZFIT0036-DT_PGTO,
  END OF TY_LOTE,

  BEGIN OF TY_VBRP,
    VBELN TYPE VBRP-VBELN,
    GSBER TYPE VBRP-GSBER,
    LIFNR TYPE LFA1-LIFNR,
  END OF TY_VBRP,

  BEGIN OF TY_CADLIQ,
    HKONT TYPE ZIMP_CAD_IMP_CON-HKONT,
    TXT50 TYPE SKAT-TXT50,
    BLDAT TYPE BKPF-BLDAT,
  END OF TY_CADLIQ,

  BEGIN OF TY_ZFIT0042,
    BUKRS       TYPE  ZFIT0042-BUKRS,
    BELNR2      TYPE  ZFIT0042-BELNR2,
    BELNR       TYPE  ZFIT0042-BELNR,
    KUNNR       TYPE  ZFIT0042-KUNNR,
    TP_OPERACAO TYPE  ZFIT0042-TP_OPERACAO,
    LOTE        TYPE  ZFIT0042-LOTE,
    DT_VENC     TYPE  ZFIT0042-DT_VENC,
    DMBE2       TYPE  ZFIT0042-DMBE2,
    DMBTR       TYPE  ZFIT0042-DMBTR,
    NRO_OV      TYPE  ZFIT0042-NRO_OV,
    NRO_FATURA  TYPE  ZFIT0042-NRO_FATURA,
    TX_CAMBIO   TYPE  ZFIT0042-TX_CAMBIO,
    BUZEI2      TYPE  ZFIT0042-BUZEI2,
    GJAHR2      TYPE  ZFIT0042-GJAHR2,
    GJAHR       TYPE  ZFIT0042-GJAHR,
    VLR_SLD     TYPE  ZFIT0042-DMBE2,
    FLAG_OK(1),
  END OF TY_ZFIT0042,


  BEGIN OF TY_BSID,
    CHECKBOX(1),
    BUKRS       TYPE BSID-BUKRS,
    GJAHR       TYPE BSID-GJAHR,
    BLART       TYPE BSID-BLART,
    XBLNR       TYPE BSID-XBLNR,
    BSCHL       TYPE BSID-BSCHL,
    KUNNR       TYPE BSID-KUNNR,
    DMBE2       TYPE BSID-DMBE2,
    DMBTR       TYPE BSID-DMBTR,
    BELNR       TYPE BSID-BELNR,
    BUZEI       TYPE BSID-BUZEI,
    UMSKZ       TYPE BSID-UMSKZ,
    NAME1       TYPE KNA1-NAME1,
  END OF TY_BSID,

  BEGIN OF TY_BSAD,
    BELNR   TYPE BSAD-BELNR,
    BUKRS   TYPE BSAD-BUKRS,
    KUNNR   TYPE BSAD-KUNNR,
    AUGDT   TYPE BSAD-AUGDT,
    AUGBL   TYPE BSAD-AUGBL,
    DELE(1),
  END OF TY_BSAD,

  BEGIN OF TY_KNA1,
    KUNNR TYPE KNA1-KUNNR,
    NAME1 TYPE KNA1-NAME1,
  END OF TY_KNA1,


  BEGIN OF TY_BKPF,
    KURSF TYPE BKPF-KURSF,
  END OF TY_BKPF,

  " Tipos  exclusivos RE Banco
  BEGIN OF TY_ZFIT0042_RE,
    TP_OPERACAO TYPE  ZFIT0042-TP_OPERACAO,
    DT_VENC     TYPE  ZFIT0042-DT_VENC,
    NR_RE       TYPE  ZFIT0042-NR_RE,
    NUMERO_DUE  TYPE  ZFIT0042-NUMERO_DUE,
    NR_INVOICE  TYPE  ZFIT0042-NR_INVOICE,
    LOTE        TYPE  ZFIT0042-LOTE,
    DMBE2       TYPE  ZFIT0042-DMBE2,
    NRO_FATURA  TYPE  ZFIT0042-NRO_FATURA,
    TX_CAMBIO   TYPE  ZFIT0042-TX_CAMBIO,
  END OF TY_ZFIT0042_RE,

  BEGIN OF TY_ZFIT0036,
    LOTE       TYPE ZFIT0036-LOTE,
    INVOICE    TYPE ZFIT0036-INVOICE,
    OBJ_KEY    TYPE ZFIT0036-OBJ_KEY,
    NAVIO      TYPE ZFIT0036-NAVIO,
    BVTYP      TYPE ZFIT0036-BVTYP,
    DT_PGTO    TYPE ZFIT0036-DT_PGTO,
    VLR_PGTO   TYPE ZFIT0036-VLR_PGTO,
    MOEDA_PGTO TYPE ZFIT0036-MOEDA_PGTO,
  END OF TY_ZFIT0036,

  BEGIN OF TY_LFA1,
    LIFNR TYPE LFA1-LIFNR,
    STCD1 TYPE LFA1-STCD1,
    NAME1 TYPE LFA1-NAME1,
  END OF TY_LFA1,

  BEGIN OF TY_ZIB_CONTABIL,
    OBJ_KEY TYPE ZFIT0036-OBJ_KEY,
    HKONT   TYPE ZIB_CONTABIL-HKONT,
    BVTYP   TYPE ZFIT0036-BVTYP,
  END OF TY_ZIB_CONTABIL,

  BEGIN OF TY_ZDOC_EXP,
    NUMERO_DUE       TYPE ZDOC_EXP-NUMERO_DUE,
    NR_REGISTRO_EXPO TYPE ZDOC_EXP-NR_REGISTRO_EXPO,
    ID_NOMEACAO_TRAN TYPE ZDOC_EXP-ID_NOMEACAO_TRAN,
    VBELN            TYPE ZDOC_EXP-VBELN,
    ID_DUE           TYPE ZDOC_EXP-ID_DUE,
  END OF TY_ZDOC_EXP,

  BEGIN OF TY_ZNOM_TRANSPORTE,
    ID_NOMEACAO_TRAN TYPE ZNOM_TRANSPORTE-ID_NOMEACAO_TRAN,
    DS_NOME_TRANSPOR TYPE ZNOM_TRANSPORTE-DS_NOME_TRANSPOR,
  END OF TY_ZNOM_TRANSPORTE,

  BEGIN OF TY_ZNOM_CONHEC,
    ID_NOMEACAO_TRAN TYPE ZDOC_EXP-ID_NOMEACAO_TRAN,
    DT_DATA          TYPE ZNOM_CONHEC-DT_DATA,
  END OF TY_ZNOM_CONHEC,

  BEGIN OF TY_LIPS,
    VBELN TYPE LIPS-VBELN,
    MATNR TYPE LIPS-MATNR,
    ARKTX TYPE LIPS-ARKTX,
  END OF TY_LIPS,

  BEGIN OF TY_LFBK,
    LIFNR TYPE LFBK-LIFNR,
    BVTYP TYPE LFBK-BVTYP,
    BANKS TYPE LFBK-BANKS,
    BANKL TYPE LFBK-BANKL,
  END OF TY_LFBK,

  BEGIN OF TY_BNKA,
    BANKS TYPE BNKA-BANKS,
    BANKL TYPE BNKA-BANKL,
    BANKA TYPE BNKA-BANKA,
  END OF TY_BNKA,

  BEGIN OF TY_SAIDA_RE,
    CHECKBOX(1),
    LOTE         TYPE ZFIT0036-LOTE,
    HKONT        TYPE ZIB_CONTABIL-HKONT,
    NAME1        TYPE LFA1-NAME1,
    STCD1        TYPE LFA1-STCD1,
    BANKA        TYPE BNKA-BANKA,
    INVOICE(150), "     TYPE ZFIT0036-INVOICE,
    DT_PGTO      TYPE ZFIT0036-DT_PGTO,
    MOEDA_PGTO   TYPE ZFIT0036-MOEDA_PGTO,
    VLR_PGTO     TYPE ZFIT0036-VLR_PGTO,
    NR_RE        TYPE ZFIT0042-NR_RE,
    NUMERO_DUE   TYPE ZFIT0042-NUMERO_DUE,
    CHAVE_ACESSO TYPE ZSDT0170-CHAVE_ACESSO,
    DT_DATA      TYPE ZNOM_CONHEC-DT_DATA,
    NAVIO        TYPE ZFIT0036-NAVIO,
    ARKTX        TYPE LIPS-ARKTX,
  END OF TY_SAIDA_RE,


  BEGIN OF TY_SAIDA,
    ICON(4),
    CHECKBOX(1),
    LOTE            TYPE ZFIT0042-LOTE,
    INVOICE         TYPE ZFIT0036-INVOICE,
    TP_OPERACAO(20) ,
    DT_VENC         TYPE ZFIT0042-DT_VENC,
    KUNNR           TYPE ZFIT0042-KUNNR,
    NAME1           TYPE KNA1-NAME1,
    XVLR_DMBE2      TYPE BSID-DMBE2,
    XVLR_DMBTR      TYPE BSID-DMBTR,
    AVLR_DMBTR      TYPE BSID-DMBTR,
    DOC_ACC(4),
    NRO_OV          TYPE ZFIT0042-NRO_OV,
    NRO_FATURA      TYPE ZFIT0042-NRO_FATURA,
    BELNR           TYPE ZFIT0042-BELNR,
    BELNR2          TYPE ZFIT0042-BELNR2,
    TX_CAMBIO       TYPE ZFIT0042-TX_CAMBIO,
    DMBE2           TYPE ZFIT0042-DMBE2,
    DMBTR           TYPE ZFIT0042-DMBTR,
    AUGBL           TYPE BSAD-AUGBL,
    AUGDT           TYPE BSAD-AUGDT,
    BUKRS           TYPE ZFIT0042-BUKRS,
    CANC(4),
    ABER(4),
  END OF TY_SAIDA.


TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: TI_BDCDATA         TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      T_MESSTAB          TYPE TABLE OF BDCMSGCOLL,

      IT_LOTE            TYPE TABLE OF TY_LOTE,
      IT_VBRP            TYPE TABLE OF TY_VBRP,
      IT_ZFIT0042        TYPE TABLE OF TY_ZFIT0042,
      IT_ZFIT0042_COP    TYPE TABLE OF TY_ZFIT0042,
      IT_ZFIT0042_LOT    TYPE TABLE OF TY_ZFIT0042,
      IT_ZFIT0042_TOT    TYPE TABLE OF TY_ZFIT0042,
      IT_BSID            TYPE TABLE OF TY_BSID,
      IT_BSID_AUX        TYPE TABLE OF TY_BSID,
      IT_BSAD            TYPE TABLE OF TY_BSAD,
      IT_KNA1            TYPE TABLE OF TY_KNA1,
      IT_KNA1_BSID       TYPE TABLE OF TY_KNA1,
      IT_SAIDA           TYPE TABLE OF TY_SAIDA,
      IT_SAIDA_RE        TYPE TABLE OF TY_SAIDA_RE,

      IT_ZFIT0042_RE     TYPE TABLE OF TY_ZFIT0042_RE,
      IT_ZFIT0036        TYPE TABLE OF TY_ZFIT0036,
      IT_LFA1            TYPE TABLE OF TY_LFA1,
      IT_ZIB_CONTABIL    TYPE TABLE OF TY_ZIB_CONTABIL,
      IT_ZDOC_EXP        TYPE TABLE OF TY_ZDOC_EXP,
      IT_ZNOM_CONHEC     TYPE TABLE OF TY_ZNOM_CONHEC,
      IT_ZNOM_TRANSPORTE TYPE TABLE OF TY_ZNOM_TRANSPORTE,
      IT_LIPS            TYPE TABLE OF TY_LIPS,
      IT_LFBK            TYPE TABLE OF TY_LFBK,
      IT_BNKA            TYPE TABLE OF TY_BNKA,


      IT_COLOR           TYPE TABLE OF LVC_S_SCOL.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  WA_CONT            TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_BDCDATA         LIKE LINE OF TI_BDCDATA,

  WA_LOTE            TYPE TY_LOTE,
  WA_VBRP            TYPE TY_VBRP,
  WL_ZFIT0042        TYPE ZFIT0042,
  WA_ZFIT0042        TYPE TY_ZFIT0042,
  WA_ZFIT0042_LOT    TYPE TY_ZFIT0042,
  WA_ZFIT0042_TOT    TYPE TY_ZFIT0042,
  WA_BSID            TYPE TY_BSID,
  WA_BSID_AUX        TYPE TY_BSID,
  WA_BSAD            TYPE TY_BSAD,
  WA_BKPF            TYPE TY_BKPF,
  WA_KNA1            TYPE TY_KNA1,
  WA_KNA1_BSID       TYPE TY_KNA1,
  WA_SAIDA           TYPE TY_SAIDA,
  WA_SAIDA_RE        TYPE TY_SAIDA_RE,

  WA_ZFIT0042_RE     TYPE TY_ZFIT0042_RE,
  WA_ZFIT0036        TYPE TY_ZFIT0036,
  WA_LFA1            TYPE TY_LFA1,
  WA_ZIB_CONTABIL    TYPE TY_ZIB_CONTABIL,
  WA_ZDOC_EXP        TYPE TY_ZDOC_EXP,
  WA_ZNOM_TRANSPORTE TYPE TY_ZNOM_TRANSPORTE,
  WA_ZNOM_CONHEC     TYPE TY_ZNOM_CONHEC,
  WA_LIPS            TYPE TY_LIPS,
  WA_LFBK            TYPE TY_LFBK,
  WA_BNKA            TYPE TY_BNKA,

  WG_CADLIQ          TYPE TY_CADLIQ.

*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
CONTROLS:  TAB_STRIP_IMP TYPE TABSTRIP.
DATA:      BEGIN OF G_TAB_STRIP_IMP,
             SUBSCREEN   LIKE SY-DYNNR,
             PROG        LIKE SY-REPID VALUE 'ZFIR0028',
             PRESSED_TAB LIKE SY-UCOMM VALUE '',
           END OF G_TAB_STRIP_IMP.

CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  IT_FCAT    TYPE TABLE OF TY_ESTRUTURA,
  S_VARIANT  TYPE DISVARIANT           , " Tabela Estrutura co
  T_TOP      TYPE SLIS_T_LISTHEADER,
  XS_EVENTS  TYPE SLIS_ALV_EVENT,
  EVENTS     TYPE SLIS_T_EVENT,
  GD_LAYOUT  TYPE SLIS_LAYOUT_ALV,
  T_PRINT    TYPE SLIS_PRINT_ALV,
  V_REPORT   LIKE SY-REPID,
  T_SORT     TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
  IT_SETLEAF LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
  ESTRUTURA  TYPE TABLE OF TY_ESTRUTURA,
  VG_I       TYPE I.

DATA: OK-CODE          TYPE SY-UCOMM,
      VNUM(10)         TYPE C,
      VSEQ(10)         TYPE P,
      VNUM_SEQ         TYPE I,
      VNUM_DIV         TYPE I,
      VNUM_RES         TYPE I,
      TABIX            TYPE SY-TABIX,
      WL_ERRO(1),
      W_ANSWER(1),
      W_BUKRS          TYPE BSID-BUKRS,
      W_OPER(02),
      W_LIB(1),
      WG_DOCUMENTO(10),
      VSTATUS(1),
      INDROW           TYPE LVC_T_ROW,
      W_IND            TYPE LVC_T_ROW WITH HEADER LINE,
      W_CONT           TYPE I,
      W_CONT2          TYPE I,
      VL_FORM          TYPE TDSFNAME,
      VL_NAME          TYPE RS38L_FNAM.

CONSTANTS:         C_X           TYPE C VALUE 'X',
                   C_ADD(3)      TYPE C VALUE 'ADD',
                   C_DEL(3)      TYPE C VALUE 'DEL',
                   C_CLOS_MSG(8) TYPE C VALUE 'CLOS_MSG'.

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
DATA: EDITCONTAINER        TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      CONTAINER_2          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 2
      SPLITTER             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,

      EDITOR               TYPE REF TO CL_GUI_TEXTEDIT,
      CL_CONTAINER_95      TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      CL_CONTAINER_05      TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID        TYPE REF TO CL_DD_DOCUMENT,
      CL_GRID              TYPE REF TO CL_GUI_ALV_GRID,
      WA_STABLE            TYPE LVC_S_STBL,
      WA_AFIELD            TYPE LVC_S_FCAT,
      IT_FIELDCAT          TYPE LVC_T_FCAT,
      W_FIELDCAT           TYPE LVC_S_FCAT,
      I_SORT               TYPE LVC_T_SORT,
      WA_LAYOUT            TYPE LVC_S_LAYO,
      IS_STABLE            TYPE LVC_S_STBL VALUE 'XX',
      WG_REPNAME           LIKE SY-REPID,
      WG_X_VARIANT         LIKE DISVARIANT,
      WG_EXIT(1)           TYPE C,
      WG_SAVE(1)           TYPE C,

      GRID1                TYPE REF TO CL_GUI_ALV_GRID,
      GRID2                TYPE REF TO CL_GUI_ALV_GRID,
      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      TG_SELECTEDCELL      TYPE LVC_T_CELL,
      WG_SELECTEDCELL      TYPE LVC_S_CELL,
      OBG_CONTEINER_EXT    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CC_EXT             TYPE SCRFNAME VALUE 'CC_BSIDSEL'.

*Declaration for toolbar buttons
DATA : TY_TOOLBAR TYPE STB_BUTTON.

** Criação de tabela dinamica
DATA: T_FIELDCATALOG TYPE LVC_T_FCAT,
      W_FIELDCATALOG TYPE LVC_S_FCAT.


************************************************************************
* D E F I N I T I O N
************************************************************************
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      CATCH_HOTSPOT
                    FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID
                    E_COLUMN_ID
                    ES_ROW_NO.
    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.


  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION


************************************************************************
* I M P L E M E N T A T I O N
************************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD ON_DATA_CHANGED.

    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          VL_VALUE TYPE LVC_VALUE.

    DATA: V_AVLR_DMBTR TYPE BSID-DMBTR,
          C_AVLR_DMBTR TYPE BSID-DMBTR.

    C_AVLR_DMBTR = 1 / 10.
    LOOP AT IT_SAIDA INTO WA_SAIDA WHERE CHECKBOX = 'X'.
    ENDLOOP.
    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                            INTO LS_GOOD
                            WHERE FIELDNAME = 'AVLR_DMBTR'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      V_AVLR_DMBTR = LV_VALUE.
      IF V_AVLR_DMBTR LT 0.
        MULTIPLY V_AVLR_DMBTR BY -1.
      ENDIF.
      IF V_AVLR_DMBTR GT C_AVLR_DMBTR.
        CLEAR LV_VALUE.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'AVLR_DMBTR'
            I_VALUE     = LV_VALUE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Ajustes no máximo em R$ 0,10'.
      ENDIF.
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'BELNR'.

      READ TABLE IT_BSID INTO WA_BSID INDEX LS_GOOD-ROW_ID.
      IF WA_BSID-BUZEI IS INITIAL.
        EXIT.
      ENDIF.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE UMSKZ BUKRS GJAHR BLART XBLNR BSCHL KUNNR DMBE2 DMBTR BELNR BUZEI UMSKZ
             FROM BSID
             INTO  WA_BSID
             WHERE BUKRS = WA_SAIDA-BUKRS
             AND   BELNR = LV_VALUE
             AND   BUZEI = WA_BSID-BUZEI.

      IF SY-SUBRC = 0.
        SELECT SINGLE KUNNR NAME1
          FROM KNA1
          INTO WA_KNA1_BSID
          WHERE KUNNR = WA_BSID-KUNNR.

        LV_VALUE =   WA_BSID-GJAHR.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'GJAHR'
            I_VALUE     = LV_VALUE.


        LV_VALUE =   WA_BSID-BLART.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'BLART'
            I_VALUE     = LV_VALUE.

        LV_VALUE =   WA_BSID-KUNNR.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'KUNNR'
            I_VALUE     = LV_VALUE.

        LV_VALUE =   WA_KNA1_BSID-NAME1.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'NAME1'
            I_VALUE     = LV_VALUE.

        LV_VALUE =   WA_BSID-DMBE2.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'DMBE2'
            I_VALUE     = LV_VALUE.

        LV_VALUE =   WA_BSID-UMSKZ.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'UMSKZ'
            I_VALUE     = LV_VALUE.
      ELSE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'GJAHR'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'BLART'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'KUNNR'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'NAME1'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'DMBE2'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'UMSKZ'
            I_VALUE     = LV_VALUE.
      ENDIF.
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'BUZEI'.

      READ TABLE IT_BSID INTO WA_BSID INDEX LS_GOOD-ROW_ID.
      IF WA_BSID-BELNR IS INITIAL.
        EXIT.
      ENDIF.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE UMSKZ BUKRS GJAHR BLART XBLNR BSCHL KUNNR DMBE2 DMBTR BELNR BUZEI UMSKZ
             FROM BSID
             INTO  WA_BSID
             WHERE BUKRS = WA_SAIDA-BUKRS
             AND   BELNR = WA_BSID-BELNR
             AND   BUZEI = LV_VALUE.

      IF SY-SUBRC = 0.
        SELECT SINGLE KUNNR NAME1
          FROM KNA1
          INTO WA_KNA1_BSID
          WHERE KUNNR = WA_BSID-KUNNR.

        LV_VALUE =   WA_BSID-GJAHR.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'GJAHR'
            I_VALUE     = LV_VALUE.

        LV_VALUE =   WA_BSID-BLART.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'BLART'
            I_VALUE     = LV_VALUE.

        LV_VALUE =   WA_BSID-KUNNR.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'KUNNR'
            I_VALUE     = LV_VALUE.

        LV_VALUE =   WA_KNA1_BSID-NAME1.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'NAME1'
            I_VALUE     = LV_VALUE.

        LV_VALUE =   WA_BSID-DMBE2.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'DMBE2'
            I_VALUE     = LV_VALUE.

        LV_VALUE =   WA_BSID-UMSKZ.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'UMSKZ'
            I_VALUE     = LV_VALUE.
      ELSE.
        CLEAR LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'GJAHR'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'BLART'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'KUNNR'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'NAME1'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'DMBE2'
            I_VALUE     = LV_VALUE.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'UMSKZ'
            I_VALUE     = LV_VALUE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED
*  METHOD on_data_changed4.
*
*  ENDMETHOD.                    "ON_DATA_CHANGED4
  METHOD ON_DATA_CHANGED_FINISHED.

*    DELETE IT_BSID WHERE BELNR IS INITIAL OR CHECKBOX IS INITIAL.
*** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.


  ENDMETHOD.                    "on_data_changed_finisheD

  METHOD CATCH_HOTSPOT.
    DATA: W_ERRO(1),
          VLOTE TYPE ZFIT0036-LOTE.
    CLEAR W_ERRO.

    READ TABLE IT_SAIDA INTO WA_SAIDA INDEX E_ROW_ID-INDEX.
    IF SY-SUBRC = 0.
      IF E_COLUMN_ID = 'CANC'.
        IF WA_SAIDA-CANC = ICON_CANCEL.
          IF  WA_SAIDA-AUGBL IS NOT INITIAL .
            MESSAGE 'Lançamento não pode ser cancelado, pois já foi compensado' TYPE 'I'.
            EXIT.
          ENDIF.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              TEXT_QUESTION         = 'Confirma cancelamento?'
              TEXT_BUTTON_1         = 'Sim'(100)
              ICON_BUTTON_1         = 'ICON_OKAY '
              TEXT_BUTTON_2         = 'Não'(101)
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
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.

          IF W_LIB = 'X'.
            LOOP AT IT_ZFIT0042 INTO WA_ZFIT0042  WHERE DT_VENC = WA_SAIDA-DT_VENC
                                                  AND   LOTE    = WA_SAIDA-LOTE.

              READ TABLE IT_BSAD INTO WA_BSAD WITH KEY BELNR = WA_ZFIT0042-BELNR2
                                                       KUNNR = WA_ZFIT0042-KUNNR
                                                       BUKRS = WA_ZFIT0042-BUKRS BINARY SEARCH.
              IF SY-SUBRC = 0.
                MESSAGE 'Existe documento compensado para este lote!' TYPE 'I'.
                W_ERRO = 'X'.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF W_ERRO = 'X'.
            EXIT.
          ENDIF.

          IF W_ANSWER = '1'.
            IF W_LIB NE 'X'.
              DELETE FROM ZFIT0042
               WHERE   STATUS_LIQ  NE 'S'
                 AND   TP_OPERACAO EQ W_OPER
                 AND   LOTE        EQ WA_SAIDA-LOTE
                 AND   DT_VENC     EQ WA_SAIDA-DT_VENC
                 AND   TX_CAMBIO   EQ WA_SAIDA-TX_CAMBIO
                 AND   BUKRS       EQ W_BUKRS.

              COMMIT WORK.

              WA_SAIDA-CANC = ICON_CHECKED.
              MODIFY IT_SAIDA FROM WA_SAIDA INDEX E_ROW_ID-INDEX TRANSPORTING CANC.
            ELSE.
              DELETE FROM ZFIT0042
               WHERE   STATUS_LIQ  EQ 'S'
                 AND   TP_OPERACAO EQ W_OPER
                 AND   LOTE        EQ WA_SAIDA-LOTE
                 AND   DT_VENC     EQ WA_SAIDA-DT_VENC
                 AND   TX_CAMBIO   EQ WA_SAIDA-TX_CAMBIO
                 AND   BUKRS       EQ W_BUKRS.
              COMMIT WORK.
              VLOTE = WA_SAIDA-LOTE.
              LOOP AT IT_SAIDA INTO WA_SAIDA WHERE LOTE = VLOTE.
                WA_SAIDA-CANC = ICON_CHECKED.
                MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX TRANSPORTING CANC.
              ENDLOOP.
            ENDIF.
            UPDATE ZFIT0036 SET RG_ATUALIZADO = 'T'
               WHERE LOTE = WA_SAIDA-LOTE
               AND   STATUS EQ 'P'.
            COMMIT WORK.

            CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
            IF SY-SUBRC <> 0.

            ENDIF.
          ENDIF.
        ENDIF.
      ELSEIF E_COLUMN_ID = 'ABER'.
        IF WA_SAIDA-ABER = ICON_OPEN.
          IF  WA_SAIDA-AUGBL IS NOT INITIAL .
            MESSAGE 'Lançamento não pode voltar para Aberto, pois já foi compensado' TYPE 'I'.
            EXIT.
          ENDIF.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              TEXT_QUESTION         = 'Confirma voltar para Abertos?'
              TEXT_BUTTON_1         = 'Sim'(100)
              ICON_BUTTON_1         = 'ICON_OKAY '
              TEXT_BUTTON_2         = 'Não'(101)
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
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.

          IF W_LIB = 'X'.
            LOOP AT IT_ZFIT0042 INTO WA_ZFIT0042  WHERE DT_VENC = WA_SAIDA-DT_VENC
                                                  AND   LOTE    = WA_SAIDA-LOTE.

              READ TABLE IT_BSAD INTO WA_BSAD WITH KEY BELNR = WA_ZFIT0042-BELNR2
                                                       KUNNR = WA_ZFIT0042-KUNNR
                                                       BUKRS = WA_ZFIT0042-BUKRS BINARY SEARCH.
              IF SY-SUBRC = 0.
                MESSAGE 'Existe documento compensado para este lote!' TYPE 'I'.
                W_ERRO = 'X'.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF W_ERRO = 'X'.
            EXIT.
          ENDIF.

          IF W_ANSWER = '1'.
            UPDATE ZFIT0042 SET STATUS_LIQ = ' '
            WHERE   STATUS_LIQ  EQ 'S'
              AND   TP_OPERACAO EQ W_OPER
              AND   LOTE        EQ WA_SAIDA-LOTE
              AND   DT_VENC     EQ WA_SAIDA-DT_VENC
              AND   TX_CAMBIO   EQ WA_SAIDA-TX_CAMBIO
              AND   BUKRS       EQ W_BUKRS.
            COMMIT WORK.
            VLOTE = WA_SAIDA-LOTE.
            LOOP AT IT_SAIDA INTO WA_SAIDA WHERE LOTE = VLOTE.
              WA_SAIDA-ABER = ICON_CHECKED.
              MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX TRANSPORTING ABER.
            ENDLOOP.

            CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
            IF SY-SUBRC <> 0.

            ENDIF.
          ENDIF.
        ENDIF.

      ELSEIF E_COLUMN_ID = 'DOC_ACC'.
        TYPES: BEGIN OF TY_ITAB ,
                 NAME(80) TYPE C,
               END OF TY_ITAB.

        DATA: MSG_ALV  TYPE CHAR80,
              ITAB_MSG TYPE TABLE OF TY_ITAB,
              WTAB_MSG TYPE  TY_ITAB.

        REFRESH ITAB_MSG.
        MSG_ALV = 'Partidas de ACC '.
        WTAB_MSG-NAME    = WA_SAIDA-NAME1.
        APPEND WTAB_MSG TO ITAB_MSG .
        CLEAR WTAB_MSG.
        "                   0000000000|0000000000|XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX|
        WTAB_MSG-NAME    = 'Nro.doc.  |Cliente   |Nome                               |Valor US$'.
        APPEND WTAB_MSG TO ITAB_MSG .
        CLEAR WTAB_MSG.
        WTAB_MSG-NAME    = '-----------------------------------------------'.
        APPEND WTAB_MSG TO ITAB_MSG .
        CLEAR WTAB_MSG.
        LOOP AT IT_BSID INTO WA_BSID.
          READ TABLE IT_KNA1_BSID INTO WA_KNA1_BSID WITH KEY KUNNR = WA_BSID-KUNNR BINARY SEARCH.
          WTAB_MSG-NAME+0 = WA_BSID-BELNR.
          WTAB_MSG-NAME+10 = '|'.
          WTAB_MSG-NAME+11 = WA_BSID-KUNNR.
          WTAB_MSG-NAME+21 = '|'.
          WTAB_MSG-NAME+22 = WA_KNA1_BSID-NAME1.
          WTAB_MSG-NAME+57 = '|'.
          WTAB_MSG-NAME+58 = WA_BSID-DMBE2.

          APPEND WTAB_MSG TO ITAB_MSG .
          CLEAR WTAB_MSG.
        ENDLOOP.
        CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
          EXPORTING
            ENDPOS_COL   = 220
            ENDPOS_ROW   = 16
            STARTPOS_COL = 165
            STARTPOS_ROW = 12
            TITLETEXT    = MSG_ALV
          TABLES
            VALUETAB     = ITAB_MSG
          EXCEPTIONS
            BREAK_OFF    = 1
            OTHERS       = 2.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "CATCH_HOTSPOT



ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR
      IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: WL_DESACTIVE.

    TY_TOOLBAR-ICON      =  ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  =  C_ADD.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.


    TY_TOOLBAR-ICON      =  ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  =  C_DEL.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
*   variable for Toolbar Button
    TY_TOOLBAR-ICON      =  ICON_VIEW_CLOSE.
    TY_TOOLBAR-FUNCTION  =  C_CLOS_MSG.
    TY_TOOLBAR-DISABLED  = SPACE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar
  METHOD HANDLE_USER_COMMAND.
    DATA: TL_ITENS_AUX TYPE TABLE OF TY_BSID,
          WL_ITENS     LIKE LINE OF IT_BSID,
          WL_LINES     TYPE SY-TABIX.
    REFRESH: TL_ITENS_AUX.

    CASE E_UCOMM.
      WHEN C_ADD.
        TL_ITENS_AUX[] = IT_BSID[].
        REFRESH: IT_BSID.
        LOOP AT TL_ITENS_AUX INTO WL_ITENS.
          APPEND WL_ITENS TO IT_BSID.
        ENDLOOP.
        CLEAR: WL_ITENS.
        APPEND WL_ITENS TO IT_BSID.

        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
      WHEN C_DEL.
        CALL METHOD GRID1->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.

        LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
          DELETE IT_BSID INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
        ENDLOOP.

        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
    ENDCASE.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar

DATA:       EVENT_RECEIVER   TYPE REF TO LCL_EVENT_RECEIVER.

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS    : P_BUKRS   TYPE BSID-BUKRS OBLIGATORY,
                P_OPER(2) TYPE C OBLIGATORY,
                P_TAXA    TYPE ZFIT0042-TX_CAMBIO.
SELECT-OPTIONS: P_DATA     FOR SY-DATUM.

SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: R_ST_A RADIOBUTTON GROUP RAD1 DEFAULT 'X',
            R_ST_L RADIOBUTTON GROUP RAD1,
            R_RE_B RADIOBUTTON GROUP RAD1.

SELECTION-SCREEN: END OF BLOCK B2.

INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_OPER.

  DATA: TL_RETURN_TAB_O TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC_O      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_OPERA OCCURS 0,
          OPERA TYPE ZFIT0042-TP_OPERACAO,
          TEXT  TYPE T012T-TEXT1,
        END OF TL_OPERA.

  REFRESH TL_OPERA.
  TL_OPERA-OPERA = '01'.
  TL_OPERA-TEXT  = 'Cambio'.
  APPEND TL_OPERA.

  TL_OPERA-OPERA = '02'.
  TL_OPERA-TEXT  = 'Captações'.
  APPEND TL_OPERA.

  TL_OPERA-OPERA = '03'.
  TL_OPERA-TEXT  = 'Baixa PA'.
  APPEND TL_OPERA.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'OPERA'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'P_OPER'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_OPERA
      RETURN_TAB      = TL_RETURN_TAB_O
      DYNPFLD_MAPPING = TL_DSELC_O.
*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
            F_SELECIONA_DADOS, " Form seleciona dados
            F_SAIDA, " Form de saida
            F_IMPRIME_DADOS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .
  W_OPER  = P_OPER.
  W_BUKRS = P_BUKRS.
  CLEAR W_LIB.
  IF R_ST_A = 'X'.
    SELECT  BUKRS BELNR2 BELNR KUNNR TP_OPERACAO LOTE DT_VENC DMBE2 DMBTR NRO_OV NRO_FATURA TX_CAMBIO BUZEI2 GJAHR2 GJAHR
      FROM ZFIT0042
      INTO TABLE IT_ZFIT0042
      WHERE STATUS_LIQ    = ''
      AND   TP_OPERACAO = P_OPER
      AND   DT_VENC IN P_DATA
    AND   BUKRS   EQ P_BUKRS.
  ELSEIF R_ST_L = 'X'.
    W_LIB = 'X'.
    SELECT  BUKRS BELNR2 BELNR KUNNR TP_OPERACAO LOTE DT_VENC DMBE2 DMBTR NRO_OV NRO_FATURA TX_CAMBIO BUZEI2 GJAHR2 GJAHR
      FROM ZFIT0042
      INTO TABLE IT_ZFIT0042
      WHERE STATUS_LIQ  = 'S'
      AND   TP_OPERACAO = P_OPER
      AND   DT_VENC IN P_DATA
    AND   BUKRS   EQ P_BUKRS.
  ELSEIF R_RE_B = 'X'.
    PERFORM F_SELECIONA_RE.
    PERFORM F_SAIDA_RE.
    EXIT.
  ENDIF.

  CHECK IT_ZFIT0042[] IS NOT INITIAL.

  SELECT LOTE INVOICE OBJ_KEY NAVIO BVTYP DT_PGTO VLR_PGTO  MOEDA_PGTO
    FROM ZFIT0036
    INTO TABLE IT_ZFIT0036
    FOR ALL ENTRIES IN IT_ZFIT0042
  WHERE LOTE   = IT_ZFIT0042-LOTE.


  SELECT KUNNR NAME1
    FROM KNA1
    INTO TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_ZFIT0042
  WHERE KUNNR = IT_ZFIT0042-KUNNR.

  SELECT  BELNR BUKRS KUNNR AUGDT AUGBL
    FROM BSAD
    INTO TABLE IT_BSAD
    FOR ALL ENTRIES IN IT_ZFIT0042
    WHERE BELNR = IT_ZFIT0042-BELNR2
    AND KUNNR   = IT_ZFIT0042-KUNNR
  AND BUKRS	  =	IT_ZFIT0042-BUKRS.

  LOOP AT IT_BSAD INTO WA_BSAD.
    IF WA_BSAD-BELNR = WA_BSAD-AUGBL OR WA_BSAD-AUGBL+0(2) NE '14'.
      WA_BSAD-DELE = 'X'.
      MODIFY IT_BSAD FROM WA_BSAD INDEX SY-TABIX TRANSPORTING DELE.
    ENDIF.
  ENDLOOP.
  DELETE IT_BSAD WHERE DELE = 'X'.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .

  IT_ZFIT0042_TOT[] = IT_ZFIT0042[].
  IT_ZFIT0042_LOT[] = IT_ZFIT0042[].
  SORT: IT_KNA1         BY KUNNR,
        IT_ZFIT0036     BY LOTE,
        IT_ZFIT0042     BY DT_VENC TX_CAMBIO BELNR2,
        IT_ZFIT0042_TOT BY DT_VENC TX_CAMBIO LOTE,
        IT_ZFIT0042_LOT BY DT_VENC TX_CAMBIO LOTE,
        IT_BSAD         BY BELNR KUNNR BUKRS.

  DELETE ADJACENT DUPLICATES FROM IT_ZFIT0042_LOT COMPARING DT_VENC TX_CAMBIO LOTE.
  LOOP AT IT_ZFIT0042_LOT INTO WA_ZFIT0042_LOT.
    READ TABLE IT_ZFIT0042 INTO WA_ZFIT0042  WITH KEY LOTE = WA_ZFIT0042_LOT-LOTE.
    WA_SAIDA-LOTE          = WA_ZFIT0042-LOTE.
    IF WA_ZFIT0042-TP_OPERACAO = '01'.
      WA_SAIDA-TP_OPERACAO   = 'Câmbio'.
    ELSEIF WA_ZFIT0042-TP_OPERACAO = '02'.
      WA_SAIDA-TP_OPERACAO   = 'Captações'.
    ELSEIF WA_ZFIT0042-TP_OPERACAO = '03'.
      WA_SAIDA-TP_OPERACAO   = 'Baixa PA'.
    ENDIF.
    WA_SAIDA-DT_VENC       = WA_ZFIT0042_LOT-DT_VENC.
    WA_SAIDA-TX_CAMBIO     = WA_ZFIT0042_LOT-TX_CAMBIO.
    WA_SAIDA-KUNNR         = WA_ZFIT0042-KUNNR.
    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_ZFIT0042-KUNNR BINARY SEARCH.
    WA_SAIDA-NAME1         = WA_KNA1-NAME1.
    WA_SAIDA-DOC_ACC       = ICON_COLUMN_RIGHT.
    WA_SAIDA-XVLR_DMBE2    = 0.
    WA_SAIDA-XVLR_DMBTR    = 0.
    WA_SAIDA-BUKRS         = WA_ZFIT0042-BUKRS.
    WA_SAIDA-CANC          = ICON_CANCEL.
    IF R_ST_L = 'X'.
      WA_SAIDA-ABER          = ICON_OPEN.
    ELSE.
      CLEAR WA_SAIDA-ABER.
    ENDIF.
    LOOP AT IT_ZFIT0042_TOT INTO WA_ZFIT0042_TOT WHERE DT_VENC    = WA_ZFIT0042_LOT-DT_VENC
                                                 AND   TX_CAMBIO  = WA_ZFIT0042_LOT-TX_CAMBIO
                                                 AND   LOTE       = WA_ZFIT0042_LOT-LOTE.
      ADD WA_ZFIT0042_TOT-DMBE2  TO WA_SAIDA-XVLR_DMBE2.
      ADD WA_ZFIT0042_TOT-DMBTR  TO WA_SAIDA-XVLR_DMBTR.
    ENDLOOP.
    IF R_ST_A = 'X'.
      APPEND WA_SAIDA TO IT_SAIDA.
      CLEAR WA_SAIDA.
    ELSEIF R_ST_L = 'X'.
      LOOP AT IT_ZFIT0042 INTO WA_ZFIT0042  WHERE DT_VENC = WA_ZFIT0042_LOT-DT_VENC.
        READ TABLE IT_ZFIT0036 INTO WA_ZFIT0036 WITH KEY LOTE  = WA_ZFIT0042-LOTE BINARY SEARCH.
        WA_SAIDA-LOTE          = WA_ZFIT0042-LOTE.
        IF WA_ZFIT0042-TP_OPERACAO = '01'.
          WA_SAIDA-TP_OPERACAO   = 'INVOICE'.
        ELSE.
          WA_SAIDA-TP_OPERACAO   = 'Fech.Câmbio'.
        ENDIF.
        WA_SAIDA-DT_VENC       = WA_ZFIT0042-DT_VENC.
        WA_SAIDA-KUNNR         = WA_ZFIT0042-KUNNR.
        READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_ZFIT0042-KUNNR BINARY SEARCH.
        WA_SAIDA-NAME1         = WA_KNA1-NAME1.
        WA_SAIDA-DOC_ACC       = ICON_COLUMN_RIGHT.
        WA_SAIDA-NRO_OV        = WA_ZFIT0042-NRO_OV.
        WA_SAIDA-INVOICE       = WA_ZFIT0036-INVOICE.
        WA_SAIDA-NRO_FATURA    = WA_ZFIT0042-NRO_FATURA.
        WA_SAIDA-BELNR         = WA_ZFIT0042-BELNR.
        WA_SAIDA-BELNR2        = WA_ZFIT0042-BELNR2.
        WA_SAIDA-TX_CAMBIO     = WA_ZFIT0042-TX_CAMBIO.
        WA_SAIDA-DMBE2         = WA_ZFIT0042-DMBE2.
        WA_SAIDA-DMBTR         = WA_ZFIT0042-DMBTR.
        WA_SAIDA-BUKRS         = WA_ZFIT0042-BUKRS.
        WA_SAIDA-CANC          = ICON_CANCEL.
        WA_SAIDA-ABER          = ICON_OPEN.

        READ TABLE IT_BSAD INTO WA_BSAD WITH KEY BELNR = WA_ZFIT0042-BELNR2
                                                 KUNNR = WA_ZFIT0042-KUNNR
                                                 BUKRS = WA_ZFIT0042-BUKRS BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-AUGBL         = WA_BSAD-AUGBL.
          WA_SAIDA-AUGDT         = WA_BSAD-AUGDT.
        ENDIF.
        APPEND WA_SAIDA TO IT_SAIDA.
        CLEAR WA_SAIDA.
      ENDLOOP.
      SORT IT_SAIDA BY LOTE.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .
  PERFORM F_ALV_FIELDCAT.
  WA_LAYOUT-ZEBRA      = 'X'.
*  wa_layout-cell_merge          = 'X'.    "Desneccessário
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.
  IF R_ST_A = 'X'.
    WA_LAYOUT-GRID_TITLE = 'Abertas'.
  ELSE.
    WA_LAYOUT-GRID_TITLE = 'Liquidadas'.
  ENDIF.
  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT   = 'X'.
  "wa_layout-box_fname       = 'MARK'.
  CALL SCREEN 0100.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .
  REFRESH IT_FIELDCAT.

  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_SAIDA'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.


  IF R_ST_A = 'X' .
    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'ICON'.
    WA_AFIELD-ICON          = 'X'.
    WA_AFIELD-SCRTEXT_S = 'St.Proc'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.
    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'CHECKBOX'.
    WA_AFIELD-CHECKBOX      = 'X'.
    WA_AFIELD-SCRTEXT_S = 'Chk'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = 'X'.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.
  ENDIF.

  IF R_ST_L = 'X'.
    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'LOTE'.
    WA_AFIELD-SCRTEXT_S = 'Lote'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'INVOICE'.
    WA_AFIELD-SCRTEXT_S = 'Invoice'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.
  ENDIF.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TP_OPERACAO'.
  WA_AFIELD-SCRTEXT_S = 'Tp.Operação'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_VENC'.
  WA_AFIELD-SCRTEXT_S = 'Dt.Vencimento'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'KUNNR'.
  WA_AFIELD-SCRTEXT_S = 'Cliente'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NAME1'.
  WA_AFIELD-SCRTEXT_S = 'Nome'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  IF R_ST_A = 'X'.
    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'XVLR_DMBE2'.
    WA_AFIELD-SCRTEXT_S = 'Valor US$'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-DO_SUM        = 'X'.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'XVLR_DMBTR'.
    WA_AFIELD-SCRTEXT_S = 'Valor R$'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-DO_SUM        = 'X'.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'TX_CAMBIO'.
    WA_AFIELD-SCRTEXT_S = 'Tx. Câmbio'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'AVLR_DMBTR'.
    WA_AFIELD-SCRTEXT_S = 'Ajuste R$'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = 'X'.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'CANC'.
    WA_AFIELD-SCRTEXT_S = 'Cancelar'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-ICON          = 'X'.
    WA_AFIELD-HOTSPOT       = 'X'.
    APPEND WA_AFIELD TO IT_FIELDCAT.

  ENDIF.

  IF R_ST_L = 'X'.
    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'NRO_OV'.
    WA_AFIELD-SCRTEXT_S = 'Ordem Venda'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'NRO_FATURA'.
    WA_AFIELD-SCRTEXT_S = 'Fatura'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'BELNR2'.
    WA_AFIELD-SCRTEXT_S = 'Doc.contabil'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'TX_CAMBIO'.
    WA_AFIELD-SCRTEXT_S = 'Tx.Câmbio'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'DMBE2'.
    WA_AFIELD-SCRTEXT_S = 'Valor US$'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'DMBTR'.
    WA_AFIELD-SCRTEXT_S = 'Valor R$'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'AUGBL'.
    WA_AFIELD-SCRTEXT_S = 'Doc.Compensação'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'AUGDT'.
    WA_AFIELD-SCRTEXT_S = 'Dt.Compensação'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'CANC'.
    WA_AFIELD-SCRTEXT_S = 'Cancelar'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-ICON          = 'X'.
    WA_AFIELD-HOTSPOT       = 'X'.
    APPEND WA_AFIELD TO IT_FIELDCAT.

    I = I + 1.
    CLEAR WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'ABER'.
    WA_AFIELD-SCRTEXT_S = 'Aberto'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-ICON          = 'X'.
    WA_AFIELD-HOTSPOT       = 'X'.
    APPEND WA_AFIELD TO IT_FIELDCAT.

  ENDIF.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.

  IF R_ST_L = 'X' .
    APPEND '&LIQP' TO FCODE.
    APPEND '&REL_RE' TO FCODE.
  ELSEIF R_ST_A = 'X'.
    APPEND '&REL_RE' TO FCODE.
  ELSE.
    APPEND '&LIQP' TO FCODE.
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

    PERFORM ZF_ALV_HEADER.
    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
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

    PERFORM ZF_ALV_HEADER .


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
        I_PARENT = CL_CONTAINER_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    WG_SAVE = 'X'.
    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.


    WA_STABLE-ROW        = 'X'.
    IF R_RE_B = 'X'.
      CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING        "IS_VARIANT = WG_X_VARIANT
          IS_LAYOUT       = WA_LAYOUT
        CHANGING
          IT_FIELDCATALOG = IT_FIELDCAT[]
          IT_SORT         = I_SORT[]
          IT_OUTTAB       = IT_SAIDA_RE[].
    ELSE.
      CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING        "IS_VARIANT = WG_X_VARIANT
          IS_LAYOUT       = WA_LAYOUT
        CHANGING
          IT_FIELDCATALOG = IT_FIELDCAT[]
          IT_SORT         = I_SORT[]
          IT_OUTTAB       = IT_SAIDA[].
    ENDIF.

    CREATE OBJECT EVENT_RECEIVER.
    SET HANDLER EVENT_RECEIVER->CATCH_HOTSPOT              FOR CL_GRID.
    SET HANDLER EVENT_RECEIVER->ON_DATA_CHANGED            FOR CL_GRID.
*    set handler event_receiver->on_data_changed_finished   for cl_grid.


  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ALV_HEADER .
  DATA:   WL_DATA(10),
           WL_HORA(8),
           WL_LINHA(60),
  WL_TEXT TYPE SDYDO_TEXT_ELEMENT.

  IF R_ST_A = 'X'.
    WL_TEXT = 'Em Aberto'.
  ELSEIF R_ST_L = 'X'.
    WL_TEXT = 'Liquidadas'.
  ELSE.
    WL_TEXT = 'Relatório de RE'.
  ENDIF.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT
      SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  CONCATENATE  'Empresa:' P_BUKRS
           INTO WL_LINHA SEPARATED BY SPACE.
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  IF P_OPER = '01'.
    CONCATENATE  'Tipo Operação : ' 'INVOICE'
            INTO WL_LINHA SEPARATED BY SPACE.
  ELSE.
    CONCATENATE  'Tipo Operação : ' 'Fech.Câmbio'
          INTO WL_LINHA SEPARATED BY SPACE.
  ENDIF.
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  IF P_DATA IS NOT INITIAL.
    CONCATENATE P_DATA-LOW+6(2) P_DATA-LOW+4(2) P_DATA-LOW+0(4) INTO  WL_DATA SEPARATED BY '.'.
    CONCATENATE 'Data Vencimento  :' WL_DATA
      INTO WL_LINHA SEPARATED BY SPACE.
    IF P_DATA-HIGH IS NOT INITIAL.
      CONCATENATE P_DATA-HIGH+6(2) P_DATA-HIGH+4(2) P_DATA-HIGH+0(4) INTO  WL_DATA SEPARATED BY '.'.
      CONCATENATE WL_LINHA 'a' WL_DATA
        INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.


ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

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
      REFRESH IT_SAIDA.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN '&AJUST'.
      LOOP AT IT_SAIDA INTO WA_SAIDA WHERE CHECKBOX = 'X'.
        CLEAR WL_ZFIT0042.
        IF WA_SAIDA-AVLR_DMBTR NE 0.
          "atualiza ajuste em R$ no primeiro registro encontrado
          SELECT SINGLE *
           FROM ZFIT0042
            INTO WL_ZFIT0042
            WHERE STATUS_LIQ    EQ ''
              AND   TP_OPERACAO EQ W_OPER
              AND   DT_VENC     EQ WA_SAIDA-DT_VENC
              AND   TX_CAMBIO   EQ WA_SAIDA-TX_CAMBIO
          AND   BUKRS       EQ W_BUKRS.

          UPDATE ZFIT0042 SET DMBTR = DMBTR + WA_SAIDA-AVLR_DMBTR
          WHERE BUKRS       = WL_ZFIT0042-BUKRS
          AND   NR_RE       = WL_ZFIT0042-NR_RE
          AND   NUMERO_DUE  = WL_ZFIT0042-NUMERO_DUE
          AND   NR_INVOICE  = WL_ZFIT0042-NR_INVOICE
          AND   BELNR       = WL_ZFIT0042-BELNR
          AND   BUZEI       = WL_ZFIT0042-BUZEI
          AND   BELNR2      = WL_ZFIT0042-BELNR2
          AND   BUZEI2      = WL_ZFIT0042-BUZEI2.
        ENDIF.
      ENDLOOP.
      "
      REFRESH IT_SAIDA.
      PERFORM:
                F_SELECIONA_DADOS, " Form seleciona dados
                F_SAIDA, " Form de saida
                F_IMPRIME_DADOS.

    WHEN '&REL_RE'.
      W_CONT = 0.
      DATA VDT_PGTO TYPE ZFIT0036-DT_PGTO.

      REFRESH IT_LOTE.
      LOOP AT IT_SAIDA_RE INTO WA_SAIDA_RE WHERE CHECKBOX = 'X'.
        WA_LOTE-LOTE    = WA_SAIDA_RE-LOTE.
        WA_LOTE-DT_PGTO = WA_SAIDA_RE-DT_PGTO.
        APPEND WA_LOTE TO IT_LOTE.
      ENDLOOP.

      SORT IT_LOTE BY LOTE.
      DELETE ADJACENT DUPLICATES FROM IT_LOTE COMPARING ALL FIELDS.
      IF IT_LOTE[] IS NOT INITIAL.
        READ TABLE IT_LOTE INTO WA_LOTE INDEX 1.
        VDT_PGTO = WA_LOTE-DT_PGTO.
        LOOP AT IT_LOTE INTO WA_LOTE.
          IF WA_LOTE-DT_PGTO NE VDT_PGTO.
            W_CONT = 1.
          ENDIF.
        ENDLOOP.
        IF W_CONT = 1.
          MESSAGE 'Selecione somente uma data' TYPE 'I'.
        ELSE.
          LOOP AT IT_SAIDA_RE INTO WA_SAIDA_RE WHERE CHECKBOX = 'X'.
            PERFORM F_IMPRIME_SMART USING WA_SAIDA_RE .
          ENDLOOP.
        ENDIF.
      ENDIF.
    WHEN '&LIQP'.
      CLEAR WG_CADLIQ.
      W_CONT = 0.
      LOOP AT IT_SAIDA INTO WA_SAIDA WHERE CHECKBOX = 'X'.
        IF WA_SAIDA-ICON NE ICON_OKAY.
          WG_CADLIQ-BLDAT = WA_SAIDA-DT_VENC.
          ADD 1 TO W_CONT.
          IF WA_SAIDA-AVLR_DMBTR NE 0.
            MESSAGE 'Existem valores de ajuste para atualizar' TYPE 'I'.
            W_CONT = 9.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF W_CONT = 9.
        EXIT.
      ENDIF.
      IF W_CONT = 1.
        IF P_OPER = '02' OR P_OPER = '03'.
          REFRESH  IT_BSID.
          CALL SCREEN 2000 STARTING AT 050 3
                 ENDING   AT 187 22.
        ELSE.
          CALL SCREEN 2000 STARTING AT 050 3
                 ENDING   AT 135 6.
        ENDIF.
      ELSE.
        MESSAGE 'Selecione somente linha' TYPE 'I'.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_2000 OUTPUT.
  DATA WK_SKAT      TYPE SKAT.

  SET PF-STATUS 'Z001'.
  SET TITLEBAR '2000'.
  LOOP AT SCREEN.
    IF P_OPER  = '02' OR P_OPER = '03'.
      IF SCREEN-NAME = 'WG_CADLIQ-TXT50' OR
         SCREEN-NAME = 'WG_CADLIQ-HKONT' OR
         SCREEN-NAME = 'TXTCONTA'.
        SCREEN-INPUT     = 0.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF SCREEN-NAME = 'WG_CADLIQ-TXT50'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WG_CADLIQ-HKONT
          IMPORTING
            OUTPUT = WG_CADLIQ-HKONT.
        SELECT SINGLE *
          FROM SKAT
          INTO WK_SKAT
          WHERE SPRAS = 'PT'
          AND KTOPL   = '0050'
        AND SAKNR   = WG_CADLIQ-HKONT.
        IF SY-SUBRC = 0.
          WG_CADLIQ-TXT50 = WK_SKAT-TXT50.
        ELSE.
          CLEAR  WG_CADLIQ-TXT50.
        ENDIF.
        MODIFY SCREEN.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2000 INPUT.
  CASE OK-CODE.
    WHEN 'GERAR'.
      CLEAR WL_ERRO.
      W_CONT = 0.
      IF P_OPER = '01'.
        IF  WG_CADLIQ-TXT50 IS INITIAL.
          MESSAGE 'Informe a conta banco' TYPE 'I'.
          EXIT.
        ENDIF.
      ELSEIF P_OPER = '02' OR P_OPER = '03'.
        IT_BSID_AUX[] = IT_BSID[].
        SORT IT_BSID_AUX BY BELNR.
        LOOP AT IT_BSID INTO WA_BSID WHERE CHECKBOX = 'X'.
          ADD 1 TO W_CONT.
          W_CONT2 = 0.
          LOOP AT IT_BSID_AUX INTO WA_BSID_AUX WHERE BELNR = WA_BSID-BELNR.
            ADD 1 TO W_CONT2.
          ENDLOOP.
          IF W_CONT2 GT 1.
            MESSAGE 'Documento digitado mais de uma vez' TYPE 'I'.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF W_CONT2 GT 1.
          EXIT.
        ENDIF.
        IF W_CONT = 0.
          MESSAGE 'Selecione um documento, para Fech.Câmbio' TYPE 'I'.
          EXIT.
        ENDIF.
      ENDIF.

      DATA TABIXSAIDA TYPE SY-TABIX.
      LOOP AT IT_SAIDA INTO WA_SAIDA WHERE CHECKBOX = 'X'.
        IF WA_SAIDA-ICON NE ICON_OKAY.
          TABIXSAIDA = SY-TABIX.

          CLEAR WL_ERRO.
          IF WA_SAIDA-TP_OPERACAO   = 'Câmbio'.
            PERFORM BAPI_F28 CHANGING WA_SAIDA WL_ERRO.
          ELSE.
            PERFORM BAPI_F51 CHANGING WA_SAIDA WL_ERRO.
          ENDIF.
          IF WL_ERRO = 'X'.
            WA_SAIDA-ICON = ICON_INCOMPLETE.
          ELSE.
            WA_SAIDA-ICON = ICON_OKAY.
            UPDATE ZFIT0042 SET STATUS_LIQ  = 'S'
             WHERE LOTE = WA_SAIDA-LOTE
             AND   DT_VENC  = WA_SAIDA-DT_VENC
             AND   TX_CAMBIO = WA_SAIDA-TX_CAMBIO.
          ENDIF.
          MODIFY IT_SAIDA FROM WA_SAIDA INDEX TABIXSAIDA TRANSPORTING ICON.
        ENDIF.
      ENDLOOP.
      IF WL_ERRO NE 'X'.
        SET SCREEN 0.
      ELSE.
        MESSAGE 'Erro de execução' TYPE 'I'.
      ENDIF.

    WHEN 'SAIR'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT INPUT.
  CASE OK-CODE.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'CANCEL'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_OPER INPUT.
  DATA: TL_RETURN_TAB_O TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC_O      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_OPERA OCCURS 0,
          OPERA TYPE ZFIT0042-TP_OPERACAO,
          TEXT  TYPE T012T-TEXT1,
        END OF TL_OPERA.

  REFRESH TL_OPERA.
  TL_OPERA-OPERA = '01'.
  TL_OPERA-TEXT  = 'Invoice'.
  APPEND TL_OPERA.

  TL_OPERA-OPERA = '02'.
  TL_OPERA-TEXT  = 'Fech.Câmbio'.
  APPEND TL_OPERA.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'OPERA'
      DYNPPROG        = SY-REPID                            "'ZFINR018'
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'P_OPERA'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_OPERA
      RETURN_TAB      = TL_RETURN_TAB_O
      DYNPFLD_MAPPING = TL_DSELC_O.
ENDMODULE.                 " SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  DATA: EVENT       TYPE CNTL_SIMPLE_EVENT,
        "EVENTS TYPE CNTL_SIMPLE_EVENTS,
        TL_FILTER   TYPE LVC_T_FILT,
        WL_FILTER   TYPE LVC_S_FILT,
        TL_FUNCTION TYPE UI_FUNCTIONS,
        WL_FUNCTION LIKE TL_FUNCTION WITH HEADER LINE.

  CLEAR WA_LAYOUT.
  WA_LAYOUT-CWIDTH_OPT  = C_X.
  WA_LAYOUT-ZEBRA      = 'X'.
  WA_LAYOUT-NO_ROWMARK = ' '.
  WA_LAYOUT-NO_TOOLBAR = ' '.
  WA_LAYOUT-GRID_TITLE = ' '.

  WA_STABLE-ROW        = 'X'.
  WA_STABLE-COL        = 'X'.
  "GRID1
  IF OBG_CONTEINER_EXT IS INITIAL.
    CREATE OBJECT OBG_CONTEINER_EXT
      EXPORTING
        CONTAINER_NAME = G_CC_EXT.


    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = OBG_CONTEINER_EXT.


    PERFORM MONTAR_LAYOUT_BSID.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID1.

*      * Register event handler
    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.

    REFRESH: TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    WA_LAYOUT-GRID_TITLE = ''.
    WA_LAYOUT-NO_TOOLBAR = ''.

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = IT_BSID[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER:
              LCL_EVENT_RECEIVER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
              LCL_EVENT_RECEIVER=>ON_DATA_CHANGED FOR GRID1.

  ELSE.
    PERFORM MONTAR_LAYOUT_BSID.
    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG[].

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_BSID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_BSID .

  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        1 ' '     ' '         'IT_BSID' 'CHECKBOX'    'Chk'              '05' 'X' ' ' ' ',
        2 'BSID'  'BELNR'     'IT_BSID' 'BELNR'       'Documento'        '15' 'X' ' ' ' ',
        3 'BSID'  'BUZEI'     'IT_BSID' 'BUZEI'       'Item'             '08' 'X' ' ' ' ',
        4 'BSID'  'GJAHR'     'IT_BSID' 'GJAHR'       'Ano'              '06' ' ' ' ' ' ',
        5 'BSID'  'KUNNR'     'IT_BSID' 'KUNNR'       'Cliente'          '15' ' ' ' ' ' ',
        6 'KNA1'  'NAME1'     'IT_BSID' 'NAME1'       'Nome'             '40' ' ' ' ' ' ',
        7 'BSID'  'DMBE2'     'IT_BSID' 'DMBE2'       'Valor U$'         '20' ' ' 'X' ' '.


ENDFORM.                    " MONTAR_LAYOUT_BSID
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT)
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE).

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY           = ' '.
  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS         = P_COL_POS.
  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  ENDIF.

  IF P_FIELD = 'CHECKBOX'.
    W_FIELDCATALOG-CHECKBOX      = 'X'.
  ENDIF.

  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_RE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_RE .
  DATA TABIX TYPE SY-TABIX.

  SELECT TP_OPERACAO DT_VENC NR_RE NUMERO_DUE NR_INVOICE LOTE DMBE2 NRO_FATURA TX_CAMBIO
    FROM ZFIT0042
    INTO TABLE IT_ZFIT0042_RE
    WHERE TP_OPERACAO = P_OPER
    AND   DT_VENC IN P_DATA
  AND   BUKRS   EQ P_BUKRS.

  CHECK SY-SUBRC = 0.

  SELECT LOTE INVOICE OBJ_KEY NAVIO BVTYP DT_PGTO VLR_PGTO  MOEDA_PGTO
    FROM ZFIT0036
    INTO TABLE IT_ZFIT0036
    FOR ALL ENTRIES IN IT_ZFIT0042_RE
  WHERE LOTE      = IT_ZFIT0042_RE-LOTE.

  CHECK SY-SUBRC = 0.

  SELECT VBELN GSBER
  FROM VBRP
  INTO TABLE IT_VBRP
  FOR ALL ENTRIES IN IT_ZFIT0042_RE
  WHERE VBELN = IT_ZFIT0042_RE-NRO_FATURA.

  LOOP AT IT_VBRP INTO WA_VBRP.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_VBRP-GSBER
      IMPORTING
        OUTPUT = WA_VBRP-LIFNR.
    MODIFY IT_VBRP FROM WA_VBRP INDEX SY-TABIX TRANSPORTING LIFNR.
  ENDLOOP.

  SELECT  OBJ_KEY HKONT
    FROM ZIB_CONTABIL
    INTO TABLE IT_ZIB_CONTABIL
    FOR ALL ENTRIES IN IT_ZFIT0036
    WHERE OBJ_KEY = IT_ZFIT0036-OBJ_KEY
  AND BSCHL	 NOT IN ('40','50').

  CHECK SY-SUBRC = 0.

  SORT IT_ZFIT0036 BY OBJ_KEY.
  LOOP AT IT_ZIB_CONTABIL  INTO WA_ZIB_CONTABIL.
    TABIX = SY-TABIX.
    READ TABLE IT_ZFIT0036 INTO WA_ZFIT0036 WITH KEY OBJ_KEY = WA_ZIB_CONTABIL-OBJ_KEY BINARY SEARCH.
    WA_ZIB_CONTABIL-BVTYP = WA_ZFIT0036-BVTYP.
    MODIFY IT_ZIB_CONTABIL FROM WA_ZIB_CONTABIL INDEX TABIX TRANSPORTING BVTYP.
  ENDLOOP.


  SELECT  LIFNR STCD1 NAME1
    FROM LFA1
    INTO TABLE IT_LFA1
    FOR ALL ENTRIES IN IT_ZIB_CONTABIL
  WHERE LIFNR = IT_ZIB_CONTABIL-HKONT.

  IF  IT_VBRP[] IS NOT INITIAL.
    SELECT  LIFNR STCD1 NAME1
      FROM LFA1
      APPENDING TABLE IT_LFA1
      FOR ALL ENTRIES IN IT_VBRP
      WHERE LIFNR = IT_VBRP-LIFNR.
  ENDIF.


  SELECT  NUMERO_DUE NR_REGISTRO_EXPO ID_NOMEACAO_TRAN VBELN ID_DUE
    FROM ZDOC_EXP
    INTO TABLE IT_ZDOC_EXP
    FOR ALL ENTRIES IN IT_ZFIT0042_RE
  WHERE NR_REGISTRO_EXPO  = IT_ZFIT0042_RE-NR_RE
  AND   NUMERO_DUE        = IT_ZFIT0042_RE-NUMERO_DUE.

  CHECK SY-SUBRC = 0.

  SELECT  ID_NOMEACAO_TRAN DS_NOME_TRANSPOR
    FROM ZNOM_TRANSPORTE
    INTO TABLE IT_ZNOM_TRANSPORTE
    FOR ALL ENTRIES IN IT_ZDOC_EXP
  WHERE ID_NOMEACAO_TRAN  = IT_ZDOC_EXP-ID_NOMEACAO_TRAN.

  SELECT  ID_NOMEACAO_TRAN DT_DATA
    FROM ZNOM_CONHEC
    INTO TABLE IT_ZNOM_CONHEC
    FOR ALL ENTRIES IN IT_ZDOC_EXP
  WHERE ID_NOMEACAO_TRAN  = IT_ZDOC_EXP-ID_NOMEACAO_TRAN.


  SELECT  VBELN MATNR ARKTX
    FROM LIPS
    INTO TABLE IT_LIPS
    FOR ALL ENTRIES IN IT_ZDOC_EXP
  WHERE  VBELN    = IT_ZDOC_EXP-VBELN.

  IF IT_ZIB_CONTABIL[] IS NOT INITIAL.
    SELECT LIFNR BVTYP BANKS BANKL
      FROM LFBK
      INTO TABLE IT_LFBK
      FOR ALL ENTRIES IN IT_ZIB_CONTABIL
      WHERE LIFNR	=	IT_ZIB_CONTABIL-HKONT
    AND   BVTYP	=	IT_ZIB_CONTABIL-BVTYP.
  ENDIF.

  IF IT_LFBK[] IS NOT INITIAL.
    SELECT BANKS BANKL BANKA
      FROM BNKA
      INTO TABLE IT_BNKA
      FOR ALL ENTRIES IN IT_LFBK
      WHERE BANKS	=	IT_LFBK-BANKS
    AND   BANKL	=	IT_LFBK-BANKL.
  ENDIF.



ENDFORM.                    " F_SELECIONA_RE
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_RE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA_RE .
  SORT:  IT_ZFIT0036     BY LOTE,
         IT_ZIB_CONTABIL BY OBJ_KEY,
         IT_LFA1         BY LIFNR,
         IT_ZDOC_EXP     BY NUMERO_DUE NR_REGISTRO_EXPO,
         IT_ZNOM_CONHEC  BY ID_NOMEACAO_TRAN,
         IT_ZNOM_TRANSPORTE  BY ID_NOMEACAO_TRAN,
         IT_LIPS         BY VBELN,
         IT_LFBK         BY LIFNR BVTYP,
         IT_BNKA         BY BANKS	BANKL,
         IT_VBRP         BY VBELN.

  DATA: WA_ZSDT0170 TYPE ZSDT0170.

  DELETE IT_ZFIT0036 WHERE MOEDA_PGTO IS INITIAL.
  DATA: IT_ZFIT0036_INV        TYPE TABLE OF TY_ZFIT0036.
  IT_ZFIT0036_INV[] = IT_ZFIT0036[].

  SORT IT_ZFIT0036_INV BY LOTE INVOICE.
  DELETE ADJACENT DUPLICATES FROM IT_ZFIT0036_INV COMPARING LOTE INVOICE.

  LOOP AT IT_ZFIT0042_RE INTO WA_ZFIT0042_RE.
    READ TABLE IT_ZFIT0036 INTO WA_ZFIT0036 WITH KEY LOTE  = WA_ZFIT0042_RE-LOTE BINARY SEARCH.
    READ TABLE IT_ZIB_CONTABIL INTO WA_ZIB_CONTABIL WITH KEY OBJ_KEY = WA_ZFIT0036-OBJ_KEY BINARY SEARCH.
    WA_SAIDA_RE-HKONT         =  WA_ZIB_CONTABIL-HKONT.
    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR =  WA_ZIB_CONTABIL-HKONT BINARY SEARCH.
    WA_SAIDA_RE-NAME1         = WA_LFA1-NAME1.

    READ TABLE  IT_LFBK INTO WA_LFBK WITH KEY  LIFNR  = WA_ZIB_CONTABIL-HKONT
                                               BVTYP  = WA_ZIB_CONTABIL-BVTYP BINARY SEARCH.
    READ TABLE IT_BNKA INTO WA_BNKA WITH KEY BANKS  = WA_LFBK-BANKS
                                             BANKL  = WA_LFBK-BANKL BINARY SEARCH.
    WA_SAIDA_RE-BANKA         = WA_BNKA-BANKA.

    WA_SAIDA_RE-LOTE          = WA_ZFIT0036-LOTE.
    WA_SAIDA_RE-INVOICE       = ''.
    LOOP AT IT_ZFIT0036_INV INTO WA_ZFIT0036 WHERE LOTE  = WA_ZFIT0042_RE-LOTE.
      CONCATENATE  WA_ZFIT0036-INVOICE '|' WA_SAIDA_RE-INVOICE  INTO WA_SAIDA_RE-INVOICE.
      CONDENSE WA_SAIDA_RE-INVOICE NO-GAPS.
    ENDLOOP.
    WA_SAIDA_RE-DT_PGTO       = WA_ZFIT0042_RE-DT_VENC.
    WA_SAIDA_RE-MOEDA_PGTO    = WA_ZFIT0036-MOEDA_PGTO.
*---> 08/06/2023 - Migração S4 - JS
*             WA_SAIDA_RE-VLR_PGTO      = WA_ZFIT0042_RE-DMBE2.
    WA_SAIDA_RE-VLR_PGTO = CONV #( WA_ZFIT0042_RE-DMBE2 ).
*<--- 08/06/2023 - Migração S4 - JS

    WA_SAIDA_RE-NR_RE         = WA_ZFIT0042_RE-NR_RE.
    WA_SAIDA_RE-NUMERO_DUE    = WA_ZFIT0042_RE-NUMERO_DUE.
    "
    READ TABLE IT_VBRP INTO WA_VBRP WITH KEY VBELN = WA_ZFIT0042_RE-NRO_FATURA BINARY SEARCH.
    IF SY-SUBRC = 0.
      READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR =  WA_VBRP-LIFNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SAIDA_RE-STCD1         = WA_LFA1-STCD1.
      ENDIF.
    ENDIF.

    CLEAR WA_ZDOC_EXP.
    READ TABLE IT_ZDOC_EXP INTO WA_ZDOC_EXP WITH KEY  NUMERO_DUE        = WA_ZFIT0042_RE-NUMERO_DUE
                                                      NR_REGISTRO_EXPO  = WA_ZFIT0042_RE-NR_RE BINARY SEARCH.

    CLEAR WA_ZNOM_CONHEC.
    READ TABLE IT_ZNOM_CONHEC INTO WA_ZNOM_CONHEC WITH KEY ID_NOMEACAO_TRAN  = WA_ZDOC_EXP-ID_NOMEACAO_TRAN BINARY SEARCH.
    WA_SAIDA_RE-DT_DATA       = WA_ZNOM_CONHEC-DT_DATA.

    READ TABLE IT_ZNOM_TRANSPORTE INTO WA_ZNOM_TRANSPORTE WITH KEY ID_NOMEACAO_TRAN  = WA_ZDOC_EXP-ID_NOMEACAO_TRAN BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SAIDA_RE-NAVIO         = WA_ZNOM_TRANSPORTE-DS_NOME_TRANSPOR.
    ENDIF.

    SELECT SINGLE *
    FROM ZSDT0170
    INTO WA_ZSDT0170
    WHERE ID_DUE     = WA_ZDOC_EXP-ID_DUE.
    IF SY-SUBRC = 0.
      WA_SAIDA_RE-CHAVE_ACESSO = WA_ZSDT0170-CHAVE_ACESSO.
    ENDIF.

    READ TABLE IT_LIPS INTO WA_LIPS WITH KEY VBELN    = WA_ZDOC_EXP-VBELN BINARY SEARCH.
    WA_SAIDA_RE-ARKTX         = WA_LIPS-ARKTX.

    APPEND WA_SAIDA_RE TO IT_SAIDA_RE.
    CLEAR WA_SAIDA_RE.
  ENDLOOP.


  "Imprime
  PERFORM F_ALV_FIELDCAT_RE.
  WA_LAYOUT-ZEBRA      = 'X'.
*    wa_layout-cell_merge          = 'X'.    "Desneccessário
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.
  IF R_ST_A = 'X'.
    WA_LAYOUT-GRID_TITLE = 'Abertas'.
  ELSE.
    WA_LAYOUT-GRID_TITLE = 'Liquidadas'.
  ENDIF.
  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT   = 'X'.

  CALL SCREEN 0100.


ENDFORM.                    " F_SAIDA_RE
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT_RE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT_RE .
  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_SAIDA_RE'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CHECKBOX'.
  WA_AFIELD-CHECKBOX      = 'X'.
  WA_AFIELD-SCRTEXT_S = 'Chk'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = 'X'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'LOTE'.
  WA_AFIELD-SCRTEXT_S = 'Lote'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'HKONT'.
  WA_AFIELD-SCRTEXT_S = 'Fornecedor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NAME1'.
  WA_AFIELD-SCRTEXT_S = 'Nome'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'STCD1'.
  WA_AFIELD-SCRTEXT_S = 'CNPJ'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BANKA'.
  WA_AFIELD-SCRTEXT_S = 'Banco'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'INVOICE'.
  WA_AFIELD-SCRTEXT_S = 'Invoice'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_PGTO'.
  WA_AFIELD-SCRTEXT_S = 'Dt.Pgto'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MOEDA_PGTO'.
  WA_AFIELD-SCRTEXT_S = 'Moeda'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-DO_SUM        = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_PGTO'.
  WA_AFIELD-SCRTEXT_S = 'Valor US$'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-DO_SUM        = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NR_RE'.
  WA_AFIELD-SCRTEXT_S = 'RE'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NUMERO_DUE'.
  WA_AFIELD-SCRTEXT_S = 'DUE'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CHAVE_ACESSO'.
  WA_AFIELD-SCRTEXT_S = 'CHAVE ACESSO'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_DATA'.
  WA_AFIELD-SCRTEXT_S = 'Data BL'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NAVIO'.
  WA_AFIELD-SCRTEXT_S = 'Navio'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ARKTX'.
  WA_AFIELD-SCRTEXT_S = 'Produto'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.
ENDFORM.                    " F_ALV_FIELDCAT_RE
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_SMART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_SMART USING P_ALV LIKE WA_SAIDA_RE.
  DATA: LS_CONTROL        TYPE SSFCTRLOP,
        LS_OPTIONS        TYPE SSFCOMPOP,
        JOB_OUTPUT_INFO   TYPE SSFCRESCL,
        LS_XSFPARAM_LINE  TYPE SSFXSFP,
        V_BIN_FILESIZE    TYPE I,
        IT_DOCS           TYPE STANDARD TABLE OF DOCS,
        IT_LINES          TYPE STANDARD TABLE OF TLINE,
        LV_FNAME          TYPE RS38L_FNAM,
        LV_MAIL_RECIPIENT TYPE SWOTOBJID,
        LV_MAIL_SENDER    TYPE SWOTOBJID,
        LV_CONTROL        TYPE SSFCTRLOP,
        LV_NAME           TYPE SO_NAME,
        LV_OUTPUT         TYPE SSFCOMPOP,
        WL_ZMENG(20),
        WL_DMBTR(20),
        WL_VLRTOT(20),
        WL_LOTE           TYPE ZFIT0036-OBSERVACAO.

  DATA: I_OTF       TYPE ITCOO OCCURS 0 WITH HEADER LINE,
        I_TLINE     TYPE TABLE OF TLINE WITH HEADER LINE,
        I_RECEIVERS TYPE TABLE OF SOMLRECI1 WITH HEADER LINE,
        I_RECORD    LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
        I_OBJPACK   LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
        I_OBJTXT    LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        I_OBJBIN    LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        I_RECLIST   LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
* Work Area declarations
        WA_OBJHEAD  TYPE SOLI_TAB,
        W_CTRLOP    TYPE SSFCTRLOP,
        W_COMPOP    TYPE SSFCOMPOP,
        W_RETURN    TYPE SSFCRESCL,
        WA_DOC_CHNG TYPE SODOCCHGI1,
        W_DATA      TYPE SODOCCHGI1,
        WA_BUFFER   TYPE STRING, "To convert from 132 to 255
* Variables declarations
        V_FORM_NAME TYPE RS38L_FNAM,
        V_LEN_IN    LIKE SOOD-OBJLEN,
        V_LEN_OUT   LIKE SOOD-OBJLEN,
        V_LEN_OUTN  TYPE I,
        V_LINES_TXT TYPE I,
        V_LINES_BIN TYPE I.

  VL_FORM = 'ZFIR0002'.
*
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = VL_FORM
    IMPORTING
      FM_NAME            = VL_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  Impresora
  LS_CONTROL-NO_DIALOG = ' '. "Evita la pantalla de opciones de salida del formulario
  LS_OPTIONS-TDDEST   = 'LOCL'.
  LS_OPTIONS-TDIMMED  = C_X.
  LS_OPTIONS-TDNEWID  = C_X.
  LS_OPTIONS-TDNOARCH = C_X.

  LS_CONTROL-PREVIEW = SPACE.
  LS_CONTROL-DEVICE  = 'PRINTER'.
  LS_CONTROL-GETOTF  = ' '.

  CLEAR:JOB_OUTPUT_INFO.
  IF P_DATA-HIGH IS INITIAL.
    P_DATA-HIGH = P_DATA-LOW.
  ENDIF.

  CLEAR WL_LOTE .
  LOOP AT IT_LOTE INTO WA_LOTE.
    CONCATENATE WL_LOTE '<' WA_LOTE-LOTE '>' INTO WL_LOTE.
  ENDLOOP.

  CALL FUNCTION VL_NAME
    EXPORTING
      USER_SETTINGS      = ' '
      CONTROL_PARAMETERS = LS_CONTROL
      OUTPUT_OPTIONS     = LS_OPTIONS
      I_OPER             = P_OPER
      I_VENCI            = WA_LOTE-DT_PGTO
      I_VENCF            = WA_LOTE-DT_PGTO
      I_LOTE             = WL_LOTE
      I_TAXA             = P_TAXA
    IMPORTING
      JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " F_IMPRIME_SMART
*&---------------------------------------------------------------------*
*&      Form  BAPI_F51
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_SAIDA  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM BAPI_F51   CHANGING P_ALV LIKE WA_SAIDA P_ERRO.
  DATA: VDATA(10),
        CNUM_SEQ(2),
        WL_VLR(16),
        WL_TAXA(16),
        VCAMPO(15),
        V_KUR       TYPE BKPF-KURSF,
        V_GJAHR     TYPE BKPF-GJAHR,

        VVALOR_BAX  TYPE ZFIT0042-DMBE2,

        MSG_NO      TYPE T100-MSGNR,
        MSG_TEXT    TYPE STRING,
        P_MODE      LIKE RFPDO-ALLGAZMD.

  "pega documentos originais e calculo de juros
  P_MODE = 'N'.
  "
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      I_CLIENT           = SY-MANDT
      I_FUNCTION         = 'C'
      I_MODE             = P_MODE
      I_UPDATE           = 'S'
      I_USER             = SY-UNAME
    EXCEPTIONS
      CLIENT_INCORRECT   = 1
      FUNCTION_INVALID   = 2
      GROUP_NAME_MISSING = 3
      MODE_INVALID       = 4
      UPDATE_INVALID     = 5
      OTHERS             = 6.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

  DATA: L_AUGLV   TYPE T041A-AUGLV   VALUE 'UMBUCHNG', "Posting with Clearing
        L_TCODE   TYPE SY-TCODE      VALUE 'FB05',     "You get an error with any other value
        L_SGFUNCT TYPE RFIPI-SGFUNCT VALUE 'C'.        "Post immediately

  DATA: LT_BLNTAB  TYPE STANDARD TABLE OF BLNTAB  WITH HEADER LINE,
        LT_FTCLEAR TYPE STANDARD TABLE OF FTCLEAR WITH HEADER LINE,
        LT_FTPOST  TYPE STANDARD TABLE OF FTPOST  WITH HEADER LINE,
        LT_FTTAX   TYPE STANDARD TABLE OF FTTAX   WITH HEADER LINE,
        LDS_RETURN TYPE BAPIRET2.

  CONCATENATE  WG_CADLIQ-BLDAT+6(2) WG_CADLIQ-BLDAT+4(2) WG_CADLIQ-BLDAT(4) INTO VDATA  SEPARATED BY '.'.

  WRITE: P_ALV-XVLR_DMBE2  TO WL_VLR.

  WRITE: P_ALV-TX_CAMBIO TO WL_TAXA.
  CONDENSE WL_TAXA NO-GAPS.

  REFRESH: LT_BLNTAB,LT_FTCLEAR, LT_FTPOST,LT_FTTAX.
  CLEAR: LT_BLNTAB,LT_FTCLEAR, LT_FTPOST,LT_FTTAX,LDS_RETURN.
  "
  LT_FTPOST-STYPE = 'K'."Header
  LT_FTPOST-COUNT = 1.  "number of Dynpro

  LT_FTPOST-FNAM = 'BKPF-BUKRS'.
  LT_FTPOST-FVAL = P_ALV-BUKRS.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-WAERS'.
  LT_FTPOST-FVAL = 'USD'.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-KURSF'.
  LT_FTPOST-FVAL = WL_TAXA.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-BLDAT'.
  LT_FTPOST-FVAL = VDATA.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-BUDAT'.
  LT_FTPOST-FVAL = VDATA.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-MONAT'.
  LT_FTPOST-FVAL =  P_ALV-DT_VENC+4(2).
  APPEND LT_FTPOST.

  LOOP AT IT_BSID INTO WA_BSID WHERE CHECKBOX = 'X'.
  ENDLOOP.
  LT_FTPOST-FNAM = 'BKPF-BLART'.
  LT_FTPOST-FVAL = 'AB'.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-XBLNR'.
  LT_FTPOST-FVAL = P_ALV-LOTE.
  APPEND LT_FTPOST.
  "

  IT_ZFIT0042_COP[] = IT_ZFIT0042[].
  SORT IT_ZFIT0042_COP BY BELNR2 BUZEI2.
  DELETE ADJACENT DUPLICATES FROM IT_ZFIT0042_COP COMPARING BELNR2 BUZEI2.
  SORT IT_ZFIT0042_COP BY DT_VENC TX_CAMBIO.

  LOOP AT IT_ZFIT0042_COP INTO WA_ZFIT0042 WHERE DT_VENC   = P_ALV-DT_VENC
                                           AND   TX_CAMBIO = P_ALV-TX_CAMBIO
                                           AND   LOTE      = P_ALV-LOTE.
    LT_FTCLEAR-AGKOA = 'D'.
    LT_FTCLEAR-AGKON = P_ALV-KUNNR.
    LT_FTCLEAR-AGUMS = ''.
    LT_FTCLEAR-AGBUK  = P_ALV-BUKRS.
    LT_FTCLEAR-XNOPS  = 'X'.
    LT_FTCLEAR-SELFD  = 'BELNR'.
    LT_FTCLEAR-SELVON = WA_ZFIT0042-BELNR2.
    IF WA_ZFIT0042-GJAHR2 IS NOT INITIAL.
      V_GJAHR = WA_ZFIT0042-GJAHR2.
    ELSE.
      V_GJAHR = P_ALV-DT_VENC+0(4).
    ENDIF.
    IF WA_ZFIT0042-BUZEI2 IS NOT INITIAL.
      CONCATENATE LT_FTCLEAR-SELVON V_GJAHR WA_ZFIT0042-BUZEI2 INTO LT_FTCLEAR-SELVON.
    ENDIF.
    APPEND LT_FTCLEAR.
  ENDLOOP.


  LOOP AT IT_BSID INTO WA_BSID WHERE CHECKBOX = 'X'.
    LT_FTCLEAR-AGKOA  = 'D'.
    LT_FTCLEAR-AGKON  = WA_BSID-KUNNR.
    LT_FTCLEAR-AGUMS  = WA_BSID-UMSKZ.
    LT_FTCLEAR-AGBUK  = P_ALV-BUKRS.
    LT_FTCLEAR-XNOPS  = 'X'.
    LT_FTCLEAR-SELFD  = 'BELNR'.
    LT_FTCLEAR-SELVON = WA_BSID-BELNR.
    IF WA_BSID-BUZEI IS NOT INITIAL.
      CONCATENATE LT_FTCLEAR-SELVON WA_BSID-GJAHR WA_BSID-BUZEI INTO LT_FTCLEAR-SELVON.
    ENDIF.
    APPEND LT_FTCLEAR.
  ENDLOOP.

  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      I_AUGLV                    = L_AUGLV
      I_TCODE                    = L_TCODE
      I_SGFUNCT                  = L_SGFUNCT
      I_NO_AUTH                  = 'X'
    IMPORTING
      E_MSGID                    = LDS_RETURN-ID
      E_MSGNO                    = LDS_RETURN-NUMBER
      E_MSGTY                    = LDS_RETURN-TYPE
      E_MSGV1                    = LDS_RETURN-MESSAGE_V1
      E_MSGV2                    = LDS_RETURN-MESSAGE_V2
      E_MSGV3                    = LDS_RETURN-MESSAGE_V3
      E_MSGV4                    = LDS_RETURN-MESSAGE_V4
    TABLES
      T_BLNTAB                   = LT_BLNTAB
      T_FTCLEAR                  = LT_FTCLEAR
      T_FTPOST                   = LT_FTPOST
      T_FTTAX                    = LT_FTTAX
    EXCEPTIONS
      CLEARING_PROCEDURE_INVALID = 1
      CLEARING_PROCEDURE_MISSING = 2
      TABLE_T041A_EMPTY          = 3
      TRANSACTION_CODE_INVALID   = 4
      AMOUNT_FORMAT_ERROR        = 5
      TOO_MANY_LINE_ITEMS        = 6
      COMPANY_CODE_INVALID       = 7
      SCREEN_NOT_FOUND           = 8
      NO_AUTHORIZATION           = 9
      OTHERS                     = 10.

  CLEAR P_ERRO.
  IF LT_BLNTAB[] IS INITIAL.
    P_ERRO = 'X'.
    WRITE LDS_RETURN-NUMBER TO MSG_NO.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        MSG_ID                 = LDS_RETURN-ID
        MSG_NO                 = MSG_NO
        MSG_VAR1               = LDS_RETURN-MESSAGE_V1
        MSG_VAR2               = LDS_RETURN-MESSAGE_V2
        MSG_VAR3               = LDS_RETURN-MESSAGE_V3
        MSG_VAR4               = LDS_RETURN-MESSAGE_V4
      IMPORTING
        MSG_TEXT               = MSG_TEXT
      EXCEPTIONS
        FUNCTION_NOT_COMPLETED = 1
        MESSAGE_NOT_FOUND      = 2
        OTHERS                 = 3.
    MESSAGE MSG_TEXT TYPE 'I'.
  ENDIF.

  "fim
  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      I_BDCIMMED              = 'X'
    EXCEPTIONS
      SESSION_NOT_PROCESSABLE = 1
      OTHERS                  = 2.

  IF SY-SUBRC <> 0.
    EXIT.
*      RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BAPI_F28
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_SAIDA  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM BAPI_F28   CHANGING P_ALV LIKE WA_SAIDA P_ERRO.
  DATA: VDATA(10),
        VGSBER       TYPE BSEG-GSBER,
        WL_VLR(16),
        WL_VLR_I(16),
        WL_TAXA(16),
        CNUM_SEQ(2),
        VNUM_SEQ     TYPE I,
        VNUM_SUB     TYPE I,
        VCAMPO(15),
        V_KUR        TYPE BKPF-KURSF,
        V_GJAHR      TYPE BKPF-GJAHR,

        MSG_NO       TYPE T100-MSGNR,
        MSG_TEXT     TYPE STRING,
        P_MODE       LIKE RFPDO-ALLGAZMD.

  CONCATENATE  WG_CADLIQ-BLDAT+6(2) WG_CADLIQ-BLDAT+4(2) WG_CADLIQ-BLDAT(4) INTO VDATA  SEPARATED BY '.'.

  WRITE: P_ALV-XVLR_DMBE2  TO WL_VLR.
  WRITE: P_ALV-XVLR_DMBTR  TO WL_VLR_I.
  V_KUR = P_ALV-TX_CAMBIO.
  WRITE: V_KUR  TO WL_TAXA.
  CONDENSE WL_TAXA NO-GAPS.

  IF P_ALV-BUKRS = '0001'.
    VGSBER = '0101'.
  ELSEIF P_ALV-BUKRS = '0015'.
    VGSBER = '1501'.
  ELSEIF P_ALV-BUKRS = '0018'.
    VGSBER = '1801'.
  ENDIF.

  "pega documentos originais e calculo de juros
  P_MODE = 'N'.
  "
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      I_CLIENT           = SY-MANDT
      I_FUNCTION         = 'C'
      I_MODE             = P_MODE
      I_UPDATE           = 'S'
      I_USER             = SY-UNAME
    EXCEPTIONS
      CLIENT_INCORRECT   = 1
      FUNCTION_INVALID   = 2
      GROUP_NAME_MISSING = 3
      MODE_INVALID       = 4
      UPDATE_INVALID     = 5
      OTHERS             = 6.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

  DATA: L_AUGLV   TYPE T041A-AUGLV   VALUE 'EINGZAHL', "Entrada de pagamento
        L_TCODE   TYPE SY-TCODE      VALUE 'FB05',     "You get an error with any other value
        L_SGFUNCT TYPE RFIPI-SGFUNCT VALUE 'C'.        "Post immediately

  DATA: LT_BLNTAB  TYPE STANDARD TABLE OF BLNTAB  WITH HEADER LINE,
        LT_FTCLEAR TYPE STANDARD TABLE OF FTCLEAR WITH HEADER LINE,
        LT_FTPOST  TYPE STANDARD TABLE OF FTPOST  WITH HEADER LINE,
        LT_FTTAX   TYPE STANDARD TABLE OF FTTAX   WITH HEADER LINE,
        LDS_RETURN TYPE BAPIRET2.


  REFRESH: LT_BLNTAB,LT_FTCLEAR, LT_FTPOST,LT_FTTAX.
  CLEAR: LT_BLNTAB,LT_FTCLEAR, LT_FTPOST,LT_FTTAX,LDS_RETURN.
  "
  LT_FTPOST-STYPE = 'K'."Header
  LT_FTPOST-COUNT = 1.  "number of Dynpro

  LT_FTPOST-FNAM = 'BKPF-BUKRS'.
  LT_FTPOST-FVAL = P_ALV-BUKRS.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-WAERS'.
  LT_FTPOST-FVAL = 'USD'.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-KURSF'.
  LT_FTPOST-FVAL = WL_TAXA.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-BLDAT'.
  LT_FTPOST-FVAL = VDATA.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-BUDAT'.
  LT_FTPOST-FVAL = VDATA.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-MONAT'.
  LT_FTPOST-FVAL =  P_ALV-DT_VENC+4(2).
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-BLART'.
  LT_FTPOST-FVAL = 'DZ'.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BKPF-XBLNR'.
  LT_FTPOST-FVAL = P_ALV-LOTE.
  APPEND LT_FTPOST.

  " Valor recebimento/pagamento
  LT_FTPOST-STYPE = 'P'.
  LT_FTPOST-COUNT =  2 .

  LT_FTPOST-FNAM = 'RF05A-NEWBS'.
  LT_FTPOST-FVAL =  '40'.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BSEG-HKONT'.
  LT_FTPOST-FVAL = WG_CADLIQ-HKONT.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BSEG-WRBTR'.
  LT_FTPOST-FVAL = WL_VLR.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BSEG-DMBTR'.
  LT_FTPOST-FVAL = WL_VLR_I.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'BSEG-BUPLA'.
  LT_FTPOST-FVAL = VGSBER.
  APPEND LT_FTPOST.

  LT_FTPOST-FNAM = 'COBL-GSBER'.
  LT_FTPOST-FVAL = VGSBER.
  APPEND LT_FTPOST.

  IT_ZFIT0042_COP[] = IT_ZFIT0042[].
  SORT IT_ZFIT0042_COP BY BELNR2 BUZEI2.
  DELETE ADJACENT DUPLICATES FROM IT_ZFIT0042_COP COMPARING BELNR2 BUZEI2.
  SORT IT_ZFIT0042_COP BY DT_VENC TX_CAMBIO.

  LOOP AT IT_ZFIT0042_COP INTO WA_ZFIT0042 WHERE DT_VENC   = P_ALV-DT_VENC
                                           AND   TX_CAMBIO = P_ALV-TX_CAMBIO
                                           AND   LOTE      = P_ALV-LOTE.

    LT_FTCLEAR-AGKOA  = 'D'.
    LT_FTCLEAR-AGKON  = P_ALV-KUNNR.
    LT_FTCLEAR-AGBUK  = P_ALV-BUKRS.
    LT_FTCLEAR-XNOPS  = 'X'.
    LT_FTCLEAR-SELFD  = 'BELNR'.
    LT_FTCLEAR-SELVON = WA_ZFIT0042-BELNR2.
    IF WA_ZFIT0042-GJAHR2 IS NOT INITIAL.
      V_GJAHR = WA_ZFIT0042-GJAHR2.
    ELSE.
      V_GJAHR = P_ALV-DT_VENC+0(4).
    ENDIF.
    IF WA_ZFIT0042-BUZEI2 IS NOT INITIAL.
      CONCATENATE LT_FTCLEAR-SELVON V_GJAHR WA_ZFIT0042-BUZEI2 INTO LT_FTCLEAR-SELVON.
    ENDIF.
    APPEND LT_FTCLEAR.
  ENDLOOP.

  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      I_AUGLV                    = L_AUGLV
      I_TCODE                    = L_TCODE
      I_SGFUNCT                  = L_SGFUNCT
      I_NO_AUTH                  = 'X'
    IMPORTING
      E_MSGID                    = LDS_RETURN-ID
      E_MSGNO                    = LDS_RETURN-NUMBER
      E_MSGTY                    = LDS_RETURN-TYPE
      E_MSGV1                    = LDS_RETURN-MESSAGE_V1
      E_MSGV2                    = LDS_RETURN-MESSAGE_V2
      E_MSGV3                    = LDS_RETURN-MESSAGE_V3
      E_MSGV4                    = LDS_RETURN-MESSAGE_V4
    TABLES
      T_BLNTAB                   = LT_BLNTAB
      T_FTCLEAR                  = LT_FTCLEAR
      T_FTPOST                   = LT_FTPOST
      T_FTTAX                    = LT_FTTAX
    EXCEPTIONS
      CLEARING_PROCEDURE_INVALID = 1
      CLEARING_PROCEDURE_MISSING = 2
      TABLE_T041A_EMPTY          = 3
      TRANSACTION_CODE_INVALID   = 4
      AMOUNT_FORMAT_ERROR        = 5
      TOO_MANY_LINE_ITEMS        = 6
      COMPANY_CODE_INVALID       = 7
      SCREEN_NOT_FOUND           = 8
      NO_AUTHORIZATION           = 9
      OTHERS                     = 10.

  CLEAR P_ERRO.
  IF LT_BLNTAB[] IS INITIAL.
    P_ERRO = 'X'.
    WRITE LDS_RETURN-NUMBER TO MSG_NO.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        MSG_ID                 = LDS_RETURN-ID
        MSG_NO                 = MSG_NO
        MSG_VAR1               = LDS_RETURN-MESSAGE_V1
        MSG_VAR2               = LDS_RETURN-MESSAGE_V2
        MSG_VAR3               = LDS_RETURN-MESSAGE_V3
        MSG_VAR4               = LDS_RETURN-MESSAGE_V4
      IMPORTING
        MSG_TEXT               = MSG_TEXT
      EXCEPTIONS
        FUNCTION_NOT_COMPLETED = 1
        MESSAGE_NOT_FOUND      = 2
        OTHERS                 = 3.
    MESSAGE MSG_TEXT TYPE 'I'.
  ENDIF.

  "fim
  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      I_BDCIMMED              = 'X'
    EXCEPTIONS
      SESSION_NOT_PROCESSABLE = 1
      OTHERS                  = 2.

  IF SY-SUBRC <> 0.
    EXIT.
*      RETURN.
  ENDIF.


ENDFORM.
