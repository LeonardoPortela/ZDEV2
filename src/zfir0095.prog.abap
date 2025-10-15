*&---------------------------------------------------------------------*
*& Report  ZFIR0095
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZFIR0095.

TYPE-POOLS ICON.

*----------------------------------------------------------------------*
***Tabelas
*----------------------------------------------------------------------*
TABLES: T001, BSIK, ZFIT0111, MAKT.


*---------------------------------------------------------------------*
*  Types
*---------------------------------------------------------------------*

TYPES: BEGIN OF TY_SAIDA,
         BUKRS           TYPE ZFIT0111-BUKRS,
         COD_ESTRUTURA   TYPE ZFIT181-COD_ESTRUTURA,
         CODIGO_FLUXO    TYPE ZFIT183-CODIGO_FLUXO,
         DESCRICAO       TYPE ZFIT0109-DESCRICAO,
         CLAS_FLX        TYPE ZFIT0111-CLAS_FLX,
         TP_PREV         TYPE ZFIT0109-TP_PREV,
         ST_CALC_SDO     TYPE ZFIT0109-ST_CALC_SDO,
         PROCESSO_ESP    TYPE ZFIT0109-PROCESSO_ESP,
         SISTEMA_ORIG    TYPE ZFIT0109-SISTEMA_ORIG,
         SEQ             TYPE ZFIT0111-SEQ,
         DT_VCTO         TYPE ZFIT0111-DT_VCTO,
         NIVEL           TYPE C,
         VARICACAO       TYPE C,
         PGTO_BLOQ       TYPE C,
         TXT_VRS1(60)    TYPE C,
         TXT_VRS2(60)    TYPE C,
         COLOR           TYPE   KKBLO_SPECIALCOL OCCURS 0,
         "Versão em Exibção---------------------------------*
         DT_VERSAO       TYPE ZFIT0111-DT_BASE_VERSAO,
         VERSAO          TYPE ZFIT0111-VERSAO,
         HORA_VERSAO     TYPE ZFIT0111-HORA_VERSAO,
         "Versão Variação-----------------------------------*
         DT_VERSAO_VAR   TYPE ZFIT0111-DT_BASE_VERSAO,
         VERSAO_VAR      TYPE ZFIT0111-VERSAO,
         HORA_VERSAO_VAR TYPE ZFIT0111-HORA_VERSAO,
         "--------------------------------------------------*
         DAY_01          TYPE ZFIT0111-DMBTR,
         DAY_02          TYPE ZFIT0111-DMBTR,
         DAY_03          TYPE ZFIT0111-DMBTR,
         DAY_04          TYPE ZFIT0111-DMBTR,
         DAY_05          TYPE ZFIT0111-DMBTR,
         DAY_06          TYPE ZFIT0111-DMBTR,
         DAY_07          TYPE ZFIT0111-DMBTR,
         DAY_08          TYPE ZFIT0111-DMBTR,
         DAY_09          TYPE ZFIT0111-DMBTR,
         DAY_10          TYPE ZFIT0111-DMBTR,
         DAY_11          TYPE ZFIT0111-DMBTR,
         DAY_12          TYPE ZFIT0111-DMBTR,
         DAY_13          TYPE ZFIT0111-DMBTR,
         DAY_14          TYPE ZFIT0111-DMBTR,
         DAY_15          TYPE ZFIT0111-DMBTR,
         DAY_16          TYPE ZFIT0111-DMBTR,
         DAY_17          TYPE ZFIT0111-DMBTR,
         DAY_18          TYPE ZFIT0111-DMBTR,
         DAY_19          TYPE ZFIT0111-DMBTR,
         DAY_20          TYPE ZFIT0111-DMBTR,
         DAY_21          TYPE ZFIT0111-DMBTR,
         DAY_22          TYPE ZFIT0111-DMBTR,
         DAY_23          TYPE ZFIT0111-DMBTR,
         DAY_24          TYPE ZFIT0111-DMBTR,
         DAY_25          TYPE ZFIT0111-DMBTR,
         DAY_26          TYPE ZFIT0111-DMBTR,
         DAY_27          TYPE ZFIT0111-DMBTR,
         DAY_28          TYPE ZFIT0111-DMBTR,
         DAY_29          TYPE ZFIT0111-DMBTR,
         DAY_30          TYPE ZFIT0111-DMBTR,
         DAY_31          TYPE ZFIT0111-DMBTR,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_DAYS_MOV,
         COLUNA(06) TYPE C,
         DT_VCTO    TYPE ZFIT0079-ZFBDT,
       END OF TY_DAYS_MOV.


*
TYPES: BEGIN OF TY_LIST_VRS,
         DT_BASE_VERSAO TYPE ZFIT0111-DT_BASE_VERSAO,
         BUKRS          TYPE ZFIT0111-BUKRS,
         BUTXT          TYPE T001-BUTXT,
         VERSAO         TYPE ZFIT0111-VERSAO,
         HORA_VERSAO    TYPE ZFIT0111-HORA_VERSAO,
       END OF TY_LIST_VRS.

DATA: BEGIN OF tg_last_versao OCCURS 0,
        bukrs          TYPE t001-bukrs,
        dt_base_versao TYPE zfit0111-dt_base_versao,
        versao         TYPE zfit0111-versao,
      END OF tg_last_versao.
*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
DATA: VG_NO_VALID        TYPE C,
      WA_BUKRS_VERSAO    TYPE ZFIT0111-BUKRS,
      WA_DT_VERSAO       TYPE ZFIT0111-DT_BASE_VERSAO,
      WA_HORA_VERSAO     TYPE ZFIT0111-HORA_VERSAO,
      WA_VERSAO          TYPE ZFIT0111-VERSAO,
      WA_DT_VERSAO_VAR   TYPE ZFIT0111-DT_BASE_VERSAO,
      WA_VERSAO_VAR      TYPE ZFIT0111-VERSAO,
      WA_HORA_VERSAO_VAR TYPE ZFIT0111-HORA_VERSAO,
      VG_TX_USD_BRL      TYPE UKURS_CURR,
      VG_TX_USD_ARS      TYPE UKURS_CURR,
      VG_TX_EUR_BRL      TYPE UKURS_CURR,
      VG_TX_EUR_USD      TYPE UKURS_CURR,
      WG_COMENT_AJUSTE   TYPE ZFIT0149,
      VG_MOEDA_INT       TYPE C,
      VG_OK_0110         TYPE SY-UCOMM,
      VG_KEY_NODE        TYPE LVC_NKEY,
      VG_SALDO_INICIAL   TYPE DMBTR.

*---------------------------------------------------------------------*
* Constantes
*---------------------------------------------------------------------*

CONSTANTS: C_ARS VALUE 'ARS' TYPE C LENGTH 3,
           C_BRL VALUE 'BRL' TYPE C LENGTH 3,
           C_USD VALUE 'USD' TYPE C LENGTH 3.

*-------------------------------------------------------------------
* Variaveis Colunns
*-------------------------------------------------------------------
DATA: DAY_01_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_02_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_03_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_04_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_05_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_06_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_07_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_08_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_09_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_10_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_11_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_12_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_13_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_14_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_15_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_16_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_17_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_18_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_19_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_20_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_21_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_22_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_23_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_24_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_25_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_26_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_27_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_28_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_29_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_30_MOV TYPE ZFIT0111-DT_VCTO,
      DAY_31_MOV TYPE ZFIT0111-DT_VCTO.

DATA: DAY_01_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_02_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_03_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_04_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_05_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_06_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_07_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_08_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_09_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_10_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_11_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_12_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_13_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_14_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_15_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_16_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_17_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_18_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_19_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_20_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_21_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_22_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_23_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_24_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_25_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_26_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_27_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_28_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_29_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_30_AJUSTE TYPE ZFIT0111-DT_VCTO,
      DAY_31_AJUSTE TYPE ZFIT0111-DT_VCTO.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*

TYPES: BEGIN OF TY_181.
         INCLUDE STRUCTURE ZFIT181.
TYPES  DESCRICAO TYPE T001-BUTXT.
TYPES: END OF TY_181.


*----------------------------------------------------------------------*
* Tabelas Interna e Work Areas
*----------------------------------------------------------------------*
DATA: TG_0109            TYPE TABLE OF ZFIT0109 WITH HEADER LINE,
      tg_0111            TYPE TABLE OF zfit0111 WITH HEADER LINE, "ZFI Visualizar datas retroativas - ZFI0140 #124930 - BG
      GT_0111            TYPE TABLE OF ZFIT0111 WITH HEADER LINE,
      GT_0112            TYPE TABLE OF ZFIT0112 WITH HEADER LINE,
      "gt_0112            TYPE TABLE OF zfit0118 WITH HEADER LINE,
      GT_0112_AUX        TYPE TABLE OF ZFIT0112 WITH HEADER LINE,
      GT_0113_AUX        TYPE TABLE OF ZFIT0113 WITH HEADER LINE,
      GT_0184_AUX        TYPE TABLE OF ZFIT184 WITH HEADER LINE,
      GT_0113            TYPE TABLE OF ZFIT0113 WITH HEADER LINE,
      GT_181             TYPE TABLE OF TY_181 WITH HEADER LINE,
      GT_182             TYPE TABLE OF ZFIT182 WITH HEADER LINE,
      GT_182_ESTRUTURA   TYPE TABLE OF ZFIT182 WITH HEADER LINE,
      GT_183             TYPE TABLE OF ZFIT183 WITH HEADER LINE,
      GT_184             TYPE TABLE OF ZFIT184 WITH HEADER LINE,
      TG_0117            TYPE TABLE OF ZFIT0117 WITH HEADER LINE,
      TG_0118            TYPE TABLE OF ZFIT0118 WITH HEADER LINE,
      TG_0118_GROUP      TYPE TABLE OF ZFIT0118 WITH HEADER LINE,
      IT_SAIDA           TYPE TABLE OF TY_SAIDA,
      WA_SAIDA           TYPE TY_SAIDA,
      IT_DIF_SALDO       TYPE TABLE OF ZFI_DIF_SALDO WITH HEADER LINE,
      IT_SALDO_INI       TYPE TABLE OF TY_SAIDA,
      IT_TOTAL_VC        TYPE TABLE OF TY_SAIDA,
      IT_SAL_TOT_FIN     TYPE TABLE OF TY_SAIDA, " RJF - 115538
      IT_SAL_TOT         TYPE TABLE OF TY_SAIDA, " RJF - 115538
      IT_SCALC_SALDO     TYPE TABLE OF TY_SAIDA,
      WA_SCALC_SALDO     TYPE TY_SAIDA,
      IT_ALV_TREE        TYPE TABLE OF TY_SAIDA,
      WA_ALV_TREE        TYPE TY_SAIDA,
      IT_SAIDA_MOV_FLX   TYPE TABLE OF TY_SAIDA,
      WA_SAIDA_MOV_FLX   TYPE TY_SAIDA,
      GV_NODE_EMPRESA    TYPE LVC_NKEY,
      GV_NODE_XX         TYPE LVC_NKEY,
      IT_SAI_AJUSTE      TYPE TABLE OF TY_SAIDA,
      WA_SAI_AJUSTE      TYPE TY_SAIDA,
      WA_SAI_AJUSTE_COPY TYPE TY_SAIDA,
      IT_MOV_AJUSTE      TYPE TABLE OF TY_SAIDA,
      WA_MOV_AJUSTE      TYPE TY_SAIDA,
      TG_DAYS_MOV_AJUSTE TYPE TABLE OF TY_DAYS_MOV WITH HEADER LINE,
      IT_RSPARAMS        TYPE TABLE OF RSPARAMS,
      WA_RSPARAMS        TYPE RSPARAMS.


DATA: BEGIN OF TG_T001 OCCURS 0,
        BUKRS TYPE T001-BUKRS,
        BUTXT TYPE T001-BUTXT,
      END OF TG_T001.

DATA: BEGIN OF TG_0079 OCCURS 0.
        INCLUDE STRUCTURE ZFIT0079.
DATA: END OF TG_0079.

DATA: BEGIN OF TG_0119 OCCURS 0.
        INCLUDE STRUCTURE ZFIT0119.
DATA: END OF TG_0119.

DATA: BEGIN OF TG_LFA1 OCCURS 0,
        LIFNR LIKE LFA1-LIFNR,
        NAME1 LIKE LFA1-NAME1,
      END OF TG_LFA1.

DATA: BEGIN OF TG_KNA1 OCCURS 0,
        KUNNR LIKE KNA1-KUNNR,
        NAME1 LIKE KNA1-NAME1,
      END OF TG_KNA1.

DATA: BEGIN OF TG_SKAT OCCURS 0.
        INCLUDE STRUCTURE SKAT.
DATA: END OF TG_SKAT.

DATA: T_LIST_VRS TYPE TABLE OF TY_LIST_VRS.
*----------------------------------------------------------------------*
* Estrutura
*----------------------------------------------------------------------*
DATA: VG_NODE_KEY       TYPE LVC_T_NKEY,
      IT_FCAT           TYPE LVC_T_FCAT,
      WA_FCAT           TYPE LVC_S_FCAT,
      H_HEADER          TYPE TREEV_HHDR,
      WA_VARIANT        TYPE DISVARIANT,
      VG_NO_EXPAND_NODE TYPE C,
      TREE              TYPE REF TO CL_GUI_ALV_TREE.

DATA: G_VARIANT       LIKE DISVARIANT,
      G_LAYOUT        TYPE LVC_S_LAYO,
      G_TREE          TYPE REF TO CL_GUI_ALV_TREE,
      DG_HTML_CNTRL   TYPE REF TO CL_GUI_HTML_VIEWER,
      DG_DYNDOC_ID    TYPE REF TO CL_DD_DOCUMENT,
      DG_SPLITTER     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_SPLITTER_2   TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_TREE  TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_HTML  TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_HTML1 TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_HTML2 TYPE REF TO CL_GUI_CONTAINER,
      PICTURE         TYPE REF TO CL_GUI_PICTURE,
      CONTAINER       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OK_CODE         TYPE SY-UCOMM.

DATA: OBJ_ALV_MOV_FLX       TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_MOV_FLX TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_ALV_VAR_V1        TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_VAR_V1  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_ALV_VAR_V2        TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_VAR_V2  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_ALV_VPROC         TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_VPROC   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_ALV_BMOV          TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_BMOV    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_ALV_AJUSTE        TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_AJUSTE  TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

*----------------------------------------------------------------------*
* Ranges
*----------------------------------------------------------------------*
RANGES: R_TP_PREV     FOR ZFIT0109-TP_PREV,
        S_DT_VERSAO   FOR ZFIT0079-DT_BASE_VERSAO,
        S_VERSAO      FOR ZFIT0079-VERSAO,
        S_BUKRS_REP   FOR T001-BUKRS,
        S_DT_REP      FOR ZFIT0079-ZFBDT.

* Objetos
DATA: C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      TY_TOOLBAR           TYPE STB_BUTTON.

DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
      WA_SELECTED_ROWS TYPE LVC_S_ROW,

      IT_SELECTEDCELL  TYPE LVC_T_CELL,
      WA_SELECTEDCELL  TYPE LVC_S_CELL.

* ALV layout variant
DATA: GS_VARIANT       TYPE DISVARIANT.

* ALV layout
DATA: GS_LAYOUT        TYPE LVC_S_LAYO.

* ALV Stable
DATA: WA_STABLE        TYPE LVC_S_STBL.

* ALV excluded functions
DATA: IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE LIKE LINE OF IT_EXCLUDE_FCODE.

*DATA: obj_toolbar_bmov   TYPE REF TO lcl_alv_toolbar_bloq_mov,
*      obj_toolbar_ajuste TYPE REF TO lcl_alv_toolbar_mov_ajuste.

DATA: GT_BDC TYPE TABLE OF BDCDATA,
      GW_BDC TYPE BDCDATA.

*----------------------------------------------------------------------*
* Group Box
*----------------------------------------------------------------------*
DATA: QD_VERSAO_1(60),
      QD_VERSAO_2(60).

*----------------------------------------------------------------------*
* Grid
*----------------------------------------------------------------------*
DATA: V_GRID_110 TYPE REF TO CL_GUI_ALV_GRID.

*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: S_BUKRS FOR T001-BUKRS OBLIGATORY, "Empresa
                  S_ZFBDT FOR BSIK-ZFBDT OBLIGATORY. "Datas
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: P_INTER RADIOBUTTON GROUP RB1 , "Moeda Interna
              P_FORTE RADIOBUTTON GROUP RB1. "Moeda Forte
SELECTION-SCREEN: END OF BLOCK B2.

*=============================================================================*
*Subscreen para listar versões                                                *
*=============================================================================*
SELECTION-SCREEN BEGIN OF SCREEN 109 AS SUBSCREEN.
  SELECTION-SCREEN: BEGIN OF BLOCK B109 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: S_BUKVRS FOR T001-BUKRS,
                    S_DTVRS FOR ZFIT0111-DT_BASE_VERSAO.
  SELECTION-SCREEN: END OF BLOCK B109.
SELECTION-SCREEN END   OF SCREEN 109.

*=============================================================================*
* Initialization
*=============================================================================*
INITIALIZATION.

  REFRESH S_ZFBDT.
  S_ZFBDT-SIGN   = 'I'.
  S_ZFBDT-OPTION = 'BT'.
  S_ZFBDT-LOW    = SY-DATUM.
  S_ZFBDT-HIGH   = SY-DATUM + 30.
  APPEND S_ZFBDT.


*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
START-OF-SELECTION.

  CLEAR: VG_MOEDA_INT.

  VG_NO_EXPAND_NODE = 'X'.

  CALL SCREEN 0100.

  INCLUDE ZFIR0095_FORM.
  INCLUDE ZFIR0095_PBO.
  INCLUDE ZFIR0095_PAI.

MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN: 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN: 'VERIF_VSR'.
      CALL SCREEN 0102 STARTING AT 02 02 ENDING AT 90 10.
  ENDCASE.

ENDMODULE.
