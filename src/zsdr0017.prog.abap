************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 21.03.2012                                          *
* Objetivo    ...: Relatorio Comprovação de Exportação                 *
* Transação   ...: ZSDT0050                                            *
************************************************************************

REPORT  ZSDR0017.

INCLUDE <ICON>.
TYPE-POOLS: SLIS, KKBLO.

"&---------------------------------------------
"& Tabelas
"&---------------------------------------------
TABLES: ZSDT_RETLOTE.

"&---------------------------------------------
"& Estrutura
"&---------------------------------------------

TYPES:
      BEGIN OF TY_ZSDT_RETLOTE,
        DOCNUM       TYPE ZSDT_RETLOTE-DOCNUM,
        NFENUM       TYPE ZSDT_RETLOTE-NFENUM,
        WERKS        TYPE ZSDT_RETLOTE-WERKS,
        NF_RETORNO   TYPE ZSDT_RETLOTE-NF_RETORNO,
        DOCNUM_RET   TYPE ZSDT_RETLOTE-DOCNUM_RET,
        DATA_CRIACAO TYPE ZSDT_RETLOTE-DATA_CRIACAO,
        DATA_SAIDA   TYPE ZSDT_RETLOTE-DATA_SAIDA,
        SAFRA        TYPE ZSDT_RETLOTE-SAFRA,
        MATNR        TYPE ZSDT_RETLOTE-MATNR,
        QUANT_VINC   TYPE ZSDT_RETLOTE-QUANT_VINC,
        VLR_TOTAL    TYPE ZSDT_RETLOTE-VLR_TOTAL,
      END OF TY_ZSDT_RETLOTE,

      BEGIN OF TY_J_1BNFLIN,
        DOCNUM TYPE J_1BNFLIN-DOCNUM,
        MENGE  TYPE J_1BNFLIN-MENGE,
        MEINS  TYPE J_1BNFLIN-MEINS,
        REFKEY TYPE J_1BNFLIN-REFKEY,
        REFTYP TYPE J_1BNFLIN-REFTYP,
        CFOP   TYPE J_1BNFLIN-CFOP,
        TOTAL  TYPE J_1BNFLIN-MENGE,
      END OF TY_J_1BNFLIN,

      BEGIN OF TY_J_1BNFLIN_VLR,
        DOCNUM TYPE J_1BNFLIN-DOCNUM,
        TOTAL  TYPE J_1BNFLIN-MENGE,
        CFOP   TYPE J_1BNFLIN-CFOP,
      END OF TY_J_1BNFLIN_VLR,

      BEGIN OF TY_MAKT,
        MATNR TYPE MAKT-MATNR,
        MAKTX TYPE MAKT-MAKTX,
      END OF TY_MAKT,

      BEGIN OF TY_ZSDT_EXPORT,
       DOCNUM       TYPE ZSDT_EXPORT-DOCNUM,
       WERKS        TYPE ZSDT_EXPORT-WERKS,
       NF_RETORNO   TYPE ZSDT_EXPORT-NF_RETORNO,
       ORDEM        TYPE ZSDT_EXPORT-ORDEM,
       MATNR        TYPE ZSDT_EXPORT-MATNR,
       DATA_CRIACAO TYPE ZSDT_EXPORT-DATA_CRIACAO,
       QUANT        TYPE ZSDT_EXPORT-QUANT,
       VALOR_TOTAL  TYPE ZSDT_EXPORT-VALOR_TOTAL,
       EXPORT       TYPE ZSDT_EXPORT-EXPORT,
       STATUS       TYPE ZSDT_EXPORT-STATUS,
       FINALIDADE   TYPE ZSDT_EXPORT-FINALIDADE,
      END OF TY_ZSDT_EXPORT,

      BEGIN OF TY_VBFA,
        VBELV        TYPE VBFA-VBELV,
        VBTYP_V      TYPE VBFA-VBTYP_V,
        VBTYP_N      TYPE VBFA-VBTYP_N,
        VBELN        TYPE VBFA-VBELN,
        VBELN_REFKEY TYPE J_1BNFLIN-REFKEY,
      END OF TY_VBFA,

      BEGIN OF TY_J_1BNFDOC,
        DOCNUM TYPE J_1BNFDOC-DOCNUM,
        DOCTYP TYPE J_1BNFDOC-DOCTYP,
        DIRECT TYPE J_1BNFDOC-DIRECT,
        CREDAT TYPE J_1BNFDOC-CREDAT,
        NFENUM TYPE J_1BNFDOC-NFENUM,
        NTGEW  TYPE J_1BNFDOC-NTGEW,
      END OF TY_J_1BNFDOC,

      BEGIN OF TY_VBRP,
        VBELN TYPE VBRP-VBELN,
        VGBEL TYPE VBRP-VGBEL,
        VGTYP TYPE VBRP-VGTYP,
      END OF TY_VBRP,

      BEGIN OF TY_ZDOC_EXP,
       VBELN             TYPE ZDOC_EXP-VBELN,
       ID_REGISTRO_EXPO  TYPE ZDOC_EXP-ID_REGISTRO_EXPO,
       ID_DDE            TYPE ZDOC_EXP-ID_DDE,
      END OF TY_ZDOC_EXP,

    BEGIN OF TY_ZREG_EXPORTACAO,
      ID_REGISTRO_EXPO TYPE ZREG_EXPORTACAO-ID_REGISTRO_EXPO,
      NR_REGISTRO_EXPO TYPE ZREG_EXPORTACAO-NR_REGISTRO_EXPO,
    END OF TY_ZREG_EXPORTACAO,

    BEGIN OF TY_ZDDE,
      ID_DDE TYPE ZDDE-ID_DDE,
      NR_DDE TYPE ZDDE-NR_DDE,
    END OF TY_ZDDE,

    BEGIN OF TY_SAIDA_CC_ALV,

      DATA_CRIACAO     TYPE ZSDT_RETLOTE-DATA_CRIACAO,
      DATA_SAIDA       TYPE ZSDT_RETLOTE-DATA_CRIACAO,
      NFENUM_REM       TYPE ZSDT_RETLOTE-NFENUM,
      NTGEW_REM        TYPE J_1BNFDOC-NTGEW,
      MATNR            TYPE MAKT-MATNR,
      MAKTX            TYPE MAKT-MAKTX,
      QUANT            TYPE ZSDT_EXPORT-QUANT,
      NF_RETORNO       TYPE ZSDT_EXPORT-NF_RETORNO,
      CREDAT           TYPE J_1BNFDOC-CREDAT,
      NFENUM_EXP       TYPE J_1BNFDOC-NFENUM,
      NTGEW_EXP        TYPE J_1BNFDOC-NTGEW,
      NR_REGISTRO_EXPO TYPE ZREG_EXPORTACAO-NR_REGISTRO_EXPO,
      NR_DDE           TYPE ZDDE-NR_DDE,
      CFOP_REM         TYPE J_1BNFLIN-CFOP,
      CFOP_EXP         TYPE J_1BNFLIN-CFOP,
      DOCNUM_RET       TYPE ZSDT_RETLOTE-DOCNUM_RET,

   END OF TY_SAIDA_CC_ALV,

    BEGIN OF TY_SAIDA_CC_ALV_REM,
            DOCNUM       TYPE ZSDT_RETLOTE-DOCNUM,
            NFENUM       TYPE ZSDT_RETLOTE-NFENUM,
            WERKS        TYPE ZSDT_RETLOTE-WERKS,
            NF_RETORNO   TYPE ZSDT_RETLOTE-NF_RETORNO,
            DOCNUM_RET   TYPE ZSDT_RETLOTE-DOCNUM_RET,
            DATA_CRIACAO TYPE ZSDT_RETLOTE-DATA_CRIACAO,
            DATA_SAIDA   TYPE ZSDT_RETLOTE-DATA_SAIDA,
            SAFRA        TYPE ZSDT_RETLOTE-SAFRA,
            MATNR        TYPE ZSDT_RETLOTE-MATNR,
            QUANT_VINC   TYPE ZSDT_RETLOTE-QUANT_VINC,
            VLR_TOTAL    TYPE ZSDT_RETLOTE-VLR_TOTAL,
    END OF TY_SAIDA_CC_ALV_REM.

"&---------------------------------------------
"& Internal Table
"&---------------------------------------------
DATA: IT_ZSDT_RETLOTE     TYPE TABLE OF TY_ZSDT_RETLOTE,
      IT_J_1BNFLIN        TYPE TABLE OF TY_J_1BNFLIN,
      IT_MAKT             TYPE TABLE OF TY_MAKT,
      IT_ZSDT_EXPORT      TYPE TABLE OF TY_ZSDT_EXPORT,
      IT_VBFA             TYPE TABLE OF TY_VBFA,
      IT_J_1BNFDOC        TYPE TABLE OF TY_J_1BNFDOC,
      IT_VBRP             TYPE TABLE OF TY_VBRP,
      IT_ZDOC_EXP         TYPE TABLE OF TY_ZDOC_EXP,
      IT_ZREG_EXPORTACAO  TYPE TABLE OF TY_ZREG_EXPORTACAO,
      IT_ZDDE             TYPE TABLE OF TY_ZDDE,
      IT_SAIDA_CC_ALV     TYPE TABLE OF TY_SAIDA_CC_ALV,
      IT_SAIDA_CC_ALV_REM TYPE TABLE OF TY_SAIDA_CC_ALV_REM.

"&---------------------------------------------
"& Internal Table Aux
"&---------------------------------------------
DATA: IT_J_1BNFLIN_AUX     TYPE TABLE OF TY_J_1BNFLIN,
      IT_J_1BNFLIN_SUM     TYPE TABLE OF TY_J_1BNFLIN,
      IT_J_1BNFLIN_SUM_VLR TYPE TABLE OF TY_J_1BNFLIN_VLR,
      IT_VBFA_AUX          TYPE TABLE OF TY_VBFA,
      IT_ZSDT_RETLOTE_REM  TYPE TABLE OF TY_ZSDT_RETLOTE,
      IT_ZSDT_RETLOTE_RET  TYPE TABLE OF TY_ZSDT_RETLOTE.

"&---------------------------------------------
"& Work Area
"&---------------------------------------------
DATA: WA_ZSDT_RETLOTE     TYPE TY_ZSDT_RETLOTE,
      WA_J_1BNFLIN        TYPE TY_J_1BNFLIN,
      WA_MAKT             TYPE TY_MAKT,
      WA_ZSDT_EXPORT      TYPE TY_ZSDT_EXPORT,
      WA_VBFA             TYPE TY_VBFA,
      WA_J_1BNFDOC        TYPE TY_J_1BNFDOC,
      WA_VBRP             TYPE TY_VBRP,
      WA_ZDOC_EXP         TYPE TY_ZDOC_EXP,
      WA_ZREG_EXPORTACAO  TYPE TY_ZREG_EXPORTACAO,
      WA_ZDDE             TYPE TY_ZDDE,
      WA_SAIDA_CC_ALV     TYPE TY_SAIDA_CC_ALV,
      WA_SAIDA_CC_ALV_REM TYPE TY_SAIDA_CC_ALV_REM,
      WA_ZSDT_RETLOTE_RET TYPE TY_ZSDT_RETLOTE.

"&---------------------------------------------
"& Work Area Aux
"&---------------------------------------------
DATA: WA_J_1BNFLIN_AUX     TYPE TY_J_1BNFLIN,
      WA_J_1BNFLIN_SUM     TYPE TY_J_1BNFLIN,
      WA_J_1BNFLIN_SUM_VLR TYPE TY_J_1BNFLIN_VLR,
      WA_VBFA_AUX          TYPE TY_VBFA,
      WA_ZSDT_RETLOTE_REM  TYPE TY_ZSDT_RETLOTE.


"&---------------------------------------------
"& Variable
"&---------------------------------------------
DATA: C_ALV_TOOLBARMANGER    TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      GR_ALVGRID             TYPE REF TO CL_GUI_ALV_GRID,
      GR_CONTAINER           TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

      GR_ALVGRID_REM         TYPE REF TO CL_GUI_ALV_GRID,
      GR_CONTAINER_REM       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,


      GC_CONTAINER_NAME      TYPE SCRFNAME VALUE 'CC_ALV',
      GC_CONTAINER_NAME_LOTE TYPE SCRFNAME VALUE 'CC_ALV_LOTE'.

"&---------------------------------------------
"& LAYOUT
"&---------------------------------------------
DATA: WA_LAYOUT           TYPE LVC_S_LAYO,
      WA_LAYOUT_REM       TYPE LVC_S_LAYO,

      WA_STABLE           TYPE LVC_S_STBL,
      IT_FIELDCATALOG     TYPE LVC_T_FCAT,
      WA_FIELDCATALOG     TYPE LVC_S_FCAT,
      IT_FIELDCATALOG_REM TYPE LVC_T_FCAT,
      WA_FIELDCATALOG_REM TYPE LVC_S_FCAT.

"&---------------------------------------------
"& Constants
"&---------------------------------------------
CONSTANTS: C_X                 TYPE C VALUE 'X',
           TELA_CC_ALV         TYPE C LENGTH 4   VALUE '0100',
           TELA_CC_ALV_LOTE    TYPE C LENGTH 4   VALUE '0200',

           " Colunas da CC ALV
           C_NR_RE             TYPE C LENGTH 12  VALUE 'Número da RE',
           C_NR_DDE            TYPE C LENGTH 13  VALUE 'Número da DDE',
           C_KG_EXP            TYPE C LENGTH 9   VALUE 'Volume KG',
           C_NF_EXP            TYPE C LENGTH 13  VALUE 'NF Exportação',
           C_CFOP              TYPE C LENGTH 18  VALUE 'CFOP de Exportação',
           C_DATA_EXP          TYPE C LENGTH 17  VALUE 'Data Emissão Exp.',
           C_NFE_SIMB          TYPE C LENGTH 20  VALUE 'NF Retorno Simbólico',
           C_DATA_SIMB         TYPE C LENGTH 22  VALUE 'Data Emissão Simbólico',
           C_KG_SIMB           TYPE C LENGTH 10  VALUE 'Volume KG',
           C_NF_REM            TYPE C LENGTH 26  VALUE 'NF Remessa Formção de Lote',
           C_CFOP_REM          TYPE C LENGTH 15  VALUE 'CFOP de Remessa',
           C_DATA_REM          TYPE C LENGTH 15  VALUE 'Data da Remessa',
           C_KG_REM            TYPE C LENGTH 10  VALUE 'Volume KG',
           C_MATNR             TYPE C LENGTH 13  VALUE 'Cod. Material',
           C_MAT_DESC          TYPE C LENGTH 16  VALUE 'Desc. Material',

           C_DOCNUM_REM         TYPE C LENGTH 16  VALUE 'Docnum',
           C_NFENUM_REM         TYPE C LENGTH 16  VALUE 'NF',
           C_WERKS_REM          TYPE C LENGTH 16  VALUE 'Filial',
           C_NF_RETORNO_REM     TYPE C LENGTH 16  VALUE 'NF Retorno',
           C_DOCNUM_RET_REM     TYPE C LENGTH 16  VALUE 'Docnum Retorno',
           C_DATA_CRIACAO_REM   TYPE C LENGTH 20  VALUE 'Data de Criação',
           C_DATA_SAIDA_REM     TYPE C LENGTH 20  VALUE 'Data de Saida',
           C_SAFRA_REM          TYPE C LENGTH 16  VALUE 'Safra',
           C_MATNR_REM          TYPE C LENGTH 16  VALUE 'Material',
           C_QUANT_VINC_REM     TYPE C LENGTH 16  VALUE 'Qtd. Vinculada',
           C_VLR_TOTAL_REM      TYPE C LENGTH 16  VALUE 'Valor Total'.

"&---------------------------------------------
"& SELECTION SCREEEN
"&---------------------------------------------
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_NFE   FOR ZSDT_RETLOTE-NFENUM NO INTERVALS," obligatory,
                P_WERKS FOR ZSDT_RETLOTE-WERKS NO INTERVALS,
                P_BUKRS FOR ZSDT_RETLOTE-BUKRS NO INTERVALS,
                P_DATA  FOR ZSDT_RETLOTE-DATA_CRIACAO.
SELECTION-SCREEN: END OF BLOCK B1.

"&---------------------------------------------
"& DATA SELECT
"&---------------------------------------------
PERFORM: DATA_SELECT_CC_ALV.
IF IT_SAIDA_CC_ALV[] IS INITIAL.
  MESSAGE E836(SD) WITH 'Não existem dados para os parâmetros de seleção'.
ENDIF.
"&---------------------------------------------
"& CALL SCREEN
"&---------------------------------------------
CALL SCREEN TELA_CC_ALV.
*----------------------------------------------------------------------*
*  MODULE status_0100 OUTPUT
*----------------------------------------------------------------------*

MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*----------------------------------------------------------------------*
*  MODULE user_command_0100 INPUT
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  IF ( SY-DYNNR EQ TELA_CC_ALV ).
    CASE SY-UCOMM.
      WHEN: 'BACK'.
        LEAVE TO SCREEN 0.
      WHEN: 'CANC'.
        LEAVE TO SCREEN 0.
      WHEN: 'EXIT'.
        LEAVE PROGRAM.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
DATA WA_EVENT TYPE REF TO LCL_EVENT_RECEIVER.
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:

    ZM_HANDLE_HOTSPOT FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
    IMPORTING E_ROW_ID
              E_COLUMN_ID
              ES_ROW_NO,

    ZM_HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
    IMPORTING
        E_OBJECT E_INTERACTIVE,

    ZM_HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
    IMPORTING
         E_UCOMM.
ENDCLASS.                    "l
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT.
    PERFORM Z_HANDLE_HOTSPOT USING    E_ROW_ID
                                      E_COLUMN_ID
                                      ES_ROW_NO.
  ENDMETHOD.                    "zm_handle_hotspot

  METHOD ZM_HANDLE_TOOLBAR.
*   Incluindo Botão ALV
    PERFORM Z_HANDLE_TOOLBAR USING E_OBJECT
                                   E_INTERACTIVE.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD ZM_HANDLE_USER_COMMAND.
*   User Command Botões Incluidos
    PERFORM Z_HANDLE_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
FORM Z_HANDLE_HOTSPOT  USING    P_E_ROW_ID
                                P_E_COLUMN_ID
                                P_ES_ROW_NO.

  READ TABLE IT_SAIDA_CC_ALV INTO WA_SAIDA_CC_ALV INDEX P_E_ROW_ID.

  IF ( SY-SUBRC EQ 0 ).

    CASE P_E_COLUMN_ID.
      WHEN 'NF_RETORNO'.
        PERFORM: DATA_SELECT_REMESSA_FORM_LOTE USING WA_SAIDA_CC_ALV-DOCNUM_RET
                                                     WA_SAIDA_CC_ALV-MATNR.


    ENDCASE.
  ENDIF.

ENDFORM.                    " Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM Z_HANDLE_TOOLBAR  USING    P_E_OBJECT
                                P_E_INTERACTIVE.

ENDFORM.                    " Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
FORM Z_HANDLE_COMMAND  USING    P_E_UCOMM.

ENDFORM.                    " Z_HANDLE_COMMAND


*----------------------------------------------------------------------*
*  MODULE create_object OUTPUT
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT OUTPUT.

  IF ( GR_ALVGRID IS INITIAL ).

    CREATE OBJECT GR_CONTAINER
      EXPORTING
        CONTAINER_NAME              = GC_CONTAINER_NAME
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT GR_ALVGRID
      EXPORTING
        I_PARENT          = GR_CONTAINER
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    IF WA_EVENT IS INITIAL.

      CREATE OBJECT WA_EVENT.
      SET HANDLER: WA_EVENT->ZM_HANDLE_HOTSPOT      FOR GR_ALVGRID.
      SET HANDLER: WA_EVENT->ZM_HANDLE_TOOLBAR      FOR GR_ALVGRID.
      SET HANDLER: WA_EVENT->ZM_HANDLE_USER_COMMAND FOR GR_ALVGRID.

    ENDIF.

    WA_STABLE-ROW        = 'X'.
    WA_LAYOUT-SEL_MODE   = 'A'.

    PERFORM: PREPARE_FIELDCATALOG.

    CALL METHOD GR_ALVGRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT
      CHANGING
        IT_OUTTAB                     = IT_SAIDA_CC_ALV[]
        IT_FIELDCATALOG               = IT_FIELDCATALOG
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.


    CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY
      EXCEPTIONS
        FINISHED = 1
        OTHERS   = 2.

  ENDIF.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  prepare_fieldcatalog
*&---------------------------------------------------------------------*
FORM PREPARE_FIELDCATALOG .

  REFRESH: IT_FIELDCATALOG.

  PERFORM FIELDCATALOG USING:
      'NR_REGISTRO_EXPO' '14'  C_NR_RE     ''  '',
      'NR_DDE'           '12'  C_NR_DDE    ''  '',
      'NTGEW_EXP'        '15'  C_KG_EXP    ''  '',
      'NFENUM_EXP'       '9'   C_NF_EXP    'X' '',
      'CFOP_EXP'         '10'  C_CFOP      ''  '',
      'CREDAT'           '10'  C_DATA_EXP  ''  '',
      'NF_RETORNO'       '9'   C_NFE_SIMB  'X' 'X',
      'DATA_CRIACAO'     '10'  C_DATA_SIMB ''  '',
      'QUANT'            '13'  C_KG_SIMB   ''  '',
      'NFENUM_REM'       '10'  C_NF_REM    ''  '',
      'DATA_SAIDA'       '10'  C_DATA_REM  ''  '',
      'NTGEW_REM'        '15'  C_KG_REM    ''  '',
      'CFOP_REM'         '10'  C_CFOP_REM  ''  '',
      'MATNR'            '10'  C_MATNR     'X' '',
      'MAKTX'            '35'  C_MAT_DESC  ''  ''.

ENDFORM.                    " PREPARE_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  fieldcatalog
*&---------------------------------------------------------------------*

FORM FIELDCATALOG  USING    VALUE(P_FIELDNAME)
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_COLTEXT)
                            VALUE(P_NO_ZERO)
                            VALUE(P_HOTSPOT).

  CLEAR: WA_FIELDCATALOG.

  WA_FIELDCATALOG-FIELDNAME = P_FIELDNAME.
  WA_FIELDCATALOG-OUTPUTLEN = P_OUTPUTLEN.
  WA_FIELDCATALOG-COLTEXT   = P_COLTEXT.
  WA_FIELDCATALOG-SELTEXT   = P_COLTEXT.
  WA_FIELDCATALOG-NO_ZERO   = P_NO_ZERO.
  WA_FIELDCATALOG-HOTSPOT   = P_HOTSPOT.

  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

ENDFORM.                    " FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT_CC_ALV
*&---------------------------------------------------------------------*
FORM DATA_SELECT_CC_ALV .

  DATA: TOTAL TYPE J_1BNFLIN-MENGE.

  " Table - Retorno de formação de lote
  SELECT DOCNUM NFENUM WERKS NF_RETORNO DOCNUM_RET DATA_CRIACAO DATA_SAIDA SAFRA MATNR QUANT_VINC VLR_TOTAL
    FROM ZSDT_RETLOTE
    INTO TABLE IT_ZSDT_RETLOTE
  WHERE NFENUM       IN P_NFE
    AND WERKS        IN P_WERKS
    AND DATA_SAIDA   IN P_DATA
    AND BUKRS        IN P_BUKRS.

  CHECK NOT IT_ZSDT_RETLOTE[] IS INITIAL.

  " Notas de remessa do retorno
  SELECT DOCNUM NFENUM WERKS NF_RETORNO DOCNUM_RET DATA_CRIACAO DATA_SAIDA SAFRA MATNR QUANT_VINC VLR_TOTAL
      FROM ZSDT_RETLOTE
      INTO TABLE IT_ZSDT_RETLOTE_RET
      FOR ALL ENTRIES IN IT_ZSDT_RETLOTE
      WHERE DOCNUM_RET EQ IT_ZSDT_RETLOTE-DOCNUM_RET
      AND MATNR EQ IT_ZSDT_RETLOTE-MATNR.

  " Table - Partidas individuais da nota fiscal
  SELECT DOCNUM MENGE MEINS REFKEY REFTYP CFOP
     FROM J_1BNFLIN
     INTO TABLE IT_J_1BNFLIN_AUX
     FOR ALL ENTRIES IN IT_ZSDT_RETLOTE
  WHERE DOCNUM EQ IT_ZSDT_RETLOTE-DOCNUM.

  REFRESH: IT_J_1BNFLIN_SUM[].

  IT_J_1BNFLIN_SUM[] = IT_J_1BNFLIN_AUX[].

  " Soma os itens

  SORT: IT_J_1BNFLIN_AUX     BY DOCNUM,
        IT_J_1BNFLIN_SUM     BY DOCNUM.

  LOOP AT IT_J_1BNFLIN_AUX INTO WA_J_1BNFLIN_AUX.

    CLEAR: TOTAL.

    LOOP AT IT_J_1BNFLIN_SUM INTO WA_J_1BNFLIN_SUM WHERE DOCNUM EQ WA_J_1BNFLIN_AUX-DOCNUM.
      TOTAL = TOTAL + WA_J_1BNFLIN_SUM-MENGE.
    ENDLOOP.

    IF NOT ( TOTAL IS INITIAL ).

      WA_J_1BNFLIN_SUM_VLR-TOTAL  = TOTAL.
      WA_J_1BNFLIN_SUM_VLR-DOCNUM = WA_J_1BNFLIN_AUX-DOCNUM.
      WA_J_1BNFLIN_SUM_VLR-CFOP   = WA_J_1BNFLIN_AUX-CFOP.

      APPEND WA_J_1BNFLIN_SUM_VLR TO IT_J_1BNFLIN_SUM_VLR.

      CLEAR: WA_J_1BNFLIN_SUM, WA_J_1BNFLIN_SUM_VLR, TOTAL.

    ENDIF.

    CLEAR: WA_J_1BNFLIN_AUX, WA_J_1BNFLIN_SUM_VLR.

  ENDLOOP.

  " Textos breves de material
  SELECT MATNR MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_ZSDT_RETLOTE
  WHERE MATNR EQ IT_ZSDT_RETLOTE-MATNR.

  " Validar a Quantidade Exportada
  SELECT DOCNUM WERKS NF_RETORNO ORDEM MATNR DATA_CRIACAO QUANT VALOR_TOTAL EXPORT STATUS FINALIDADE
    FROM ZSDT_EXPORT
    INTO TABLE IT_ZSDT_EXPORT
    FOR ALL ENTRIES IN IT_ZSDT_RETLOTE
  WHERE DOCNUM     EQ IT_ZSDT_RETLOTE-DOCNUM_RET
    AND MATNR      EQ IT_ZSDT_RETLOTE-MATNR
    AND EXPORT     EQ 'X'
    AND STATUS     EQ 'X'
    AND FINALIDADE EQ 'E'.

  IF SY-SUBRC IS INITIAL.
    " Fluxo de documentos de vendas e distribuição
    SELECT VBELV VBTYP_V VBTYP_N VBELN
      FROM VBFA
      INTO TABLE IT_VBFA_AUX
      FOR ALL ENTRIES IN IT_ZSDT_EXPORT
    WHERE VBELV = IT_ZSDT_EXPORT-ORDEM
      AND VBTYP_N = 'M'
      AND VBTYP_V = 'C'.

    IF SY-SUBRC IS INITIAL.
      " Conversion
      LOOP AT IT_VBFA_AUX INTO WA_VBFA_AUX.
        WA_VBFA-VBELV        = WA_VBFA_AUX-VBELV.
        WA_VBFA-VBTYP_V      = WA_VBFA_AUX-VBTYP_V.
        WA_VBFA-VBTYP_N      = WA_VBFA_AUX-VBTYP_N.
        WA_VBFA-VBELN        = WA_VBFA_AUX-VBELN.
        WA_VBFA-VBELN_REFKEY = WA_VBFA_AUX-VBELN.
        APPEND WA_VBFA TO IT_VBFA.
      ENDLOOP.

      " Partidas individuais da nota fiscal
      SELECT DOCNUM MENGE MEINS REFKEY REFTYP CFOP
        FROM J_1BNFLIN
        INTO TABLE IT_J_1BNFLIN
        FOR ALL ENTRIES IN IT_VBFA
      WHERE REFKEY EQ IT_VBFA-VBELN_REFKEY
        AND REFTYP EQ 'BI'.

      IF SY-SUBRC IS INITIAL.
        " Cabeçalho da nota fiscal
        SELECT DOCNUM DOCTYP DIRECT CREDAT NFENUM NTGEW
          FROM J_1BNFDOC
          INTO TABLE IT_J_1BNFDOC
          FOR ALL ENTRIES IN IT_J_1BNFLIN
        WHERE DOCNUM EQ IT_J_1BNFLIN-DOCNUM
          AND DOCTYP EQ '1'
          AND DIRECT EQ '2'.
      ENDIF.

      " Buscar a Remessa
      SELECT VBELN VGBEL VGTYP
        FROM VBRP
        INTO TABLE IT_VBRP
        FOR ALL ENTRIES IN IT_VBFA
      WHERE VBELN EQ IT_VBFA-VBELN.

      " Documento Exportação
      SELECT VBELN ID_REGISTRO_EXPO ID_DDE
        FROM ZDOC_EXP
        INTO TABLE IT_ZDOC_EXP
        FOR ALL ENTRIES IN IT_VBRP
      WHERE VBELN EQ IT_VBRP-VGBEL.
    ENDIF.
    " Tabela de Regsitros de Exportação
    SELECT ID_REGISTRO_EXPO NR_REGISTRO_EXPO
      FROM ZREG_EXPORTACAO
      INTO TABLE IT_ZREG_EXPORTACAO
      FOR ALL ENTRIES IN IT_ZDOC_EXP
    WHERE ID_REGISTRO_EXPO EQ IT_ZDOC_EXP-ID_REGISTRO_EXPO.

    " Tabela InBound de Declaração Desp. de Exportação
    SELECT ID_DDE NR_DDE
      FROM ZDDE
      INTO TABLE IT_ZDDE
      FOR ALL ENTRIES IN IT_ZDOC_EXP
    WHERE ID_DDE EQ IT_ZDOC_EXP-ID_DDE.

  ENDIF.
  PERFORM: DATA_SELECT_EXIT.

ENDFORM.                    " DATA_SELECT_CC_ALV
*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT_EXIT
*&---------------------------------------------------------------------*
FORM DATA_SELECT_EXIT .

  SORT: IT_J_1BNFLIN_SUM_VLR BY DOCNUM,
        IT_MAKT              BY MATNR,
        IT_ZSDT_EXPORT       BY DOCNUM MATNR,
        IT_VBFA              BY VBELV,
        IT_J_1BNFLIN         BY REFKEY,
        IT_J_1BNFDOC         BY DOCNUM,
        IT_VBRP              BY VBELN,
        IT_ZDOC_EXP          BY VBELN,
        IT_ZREG_EXPORTACAO   BY ID_REGISTRO_EXPO,
        IT_ZDDE              BY ID_DDE,
        IT_ZSDT_RETLOTE      BY DOCNUM_RET MATNR,
        IT_J_1BNFLIN_AUX     BY DOCNUM.

  LOOP AT IT_ZSDT_RETLOTE INTO WA_ZSDT_RETLOTE.

    READ TABLE IT_J_1BNFLIN_SUM_VLR INTO WA_J_1BNFLIN_SUM_VLR WITH KEY DOCNUM = WA_ZSDT_RETLOTE-DOCNUM BINARY SEARCH.
    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_ZSDT_RETLOTE-MATNR BINARY SEARCH.
    READ TABLE IT_ZSDT_EXPORT INTO WA_ZSDT_EXPORT WITH KEY DOCNUM = WA_ZSDT_RETLOTE-DOCNUM_RET
                                                           MATNR  = WA_ZSDT_RETLOTE-MATNR BINARY SEARCH.


    READ TABLE IT_VBFA INTO WA_VBFA WITH KEY VBELV = WA_ZSDT_EXPORT-ORDEM BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR :  WA_SAIDA_CC_ALV-DATA_SAIDA ,
               WA_SAIDA_CC_ALV-NFENUM_REM  ,
               WA_SAIDA_CC_ALV-DOCNUM_RET  ,
               WA_SAIDA_CC_ALV-NTGEW_REM   ,
               WA_SAIDA_CC_ALV-CFOP_REM     .

      WA_SAIDA_CC_ALV-DATA_SAIDA   = WA_ZSDT_RETLOTE-DATA_SAIDA.
      WA_SAIDA_CC_ALV-NFENUM_REM   = WA_ZSDT_RETLOTE-NFENUM.
      WA_SAIDA_CC_ALV-DOCNUM_RET   = WA_ZSDT_RETLOTE-DOCNUM_RET.

      WA_SAIDA_CC_ALV-MATNR        = WA_MAKT-MATNR.
      WA_SAIDA_CC_ALV-MAKTX        = WA_MAKT-MAKTX.

      WA_SAIDA_CC_ALV-QUANT        = 0.
      LOOP AT IT_ZSDT_RETLOTE_RET INTO WA_ZSDT_RETLOTE_RET WHERE DOCNUM_RET = WA_ZSDT_RETLOTE-DOCNUM_RET
                                                           AND   MATNR      = WA_ZSDT_RETLOTE-MATNR.
        ADD WA_ZSDT_RETLOTE_RET-QUANT_VINC TO WA_SAIDA_CC_ALV-QUANT .
      ENDLOOP.

      WA_SAIDA_CC_ALV-DATA_CRIACAO = WA_ZSDT_RETLOTE-DATA_CRIACAO.
      WA_SAIDA_CC_ALV-NF_RETORNO   = WA_ZSDT_RETLOTE-NF_RETORNO.

      CLEAR WA_SAIDA_CC_ALV-CFOP_EXP.

      READ TABLE IT_J_1BNFLIN_AUX INTO WA_J_1BNFLIN_AUX WITH KEY DOCNUM = WA_ZSDT_RETLOTE-DOCNUM BINARY SEARCH.
      WA_SAIDA_CC_ALV-CFOP_REM = WA_J_1BNFLIN_AUX-CFOP.


      CLEAR : WA_SAIDA_CC_ALV-CREDAT     ,
              WA_SAIDA_CC_ALV-NFENUM_EXP ,
              WA_SAIDA_CC_ALV-NTGEW_EXP ,
              WA_SAIDA_CC_ALV-NR_REGISTRO_EXPO ,
              WA_SAIDA_CC_ALV-NR_DDE .

      WA_SAIDA_CC_ALV-NTGEW_REM      = WA_ZSDT_RETLOTE-QUANT_VINC.

      APPEND WA_SAIDA_CC_ALV TO IT_SAIDA_CC_ALV.

      CLEAR: WA_SAIDA_CC_ALV,
             WA_VBFA,
             WA_J_1BNFLIN,
             WA_J_1BNFDOC,
             WA_VBRP,
             WA_ZDOC_EXP,
             WA_ZREG_EXPORTACAO,
             WA_ZDDE.
    ELSE.
      LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV EQ WA_ZSDT_EXPORT-ORDEM.

        WA_SAIDA_CC_ALV-DATA_SAIDA   = WA_ZSDT_RETLOTE-DATA_SAIDA.
        WA_SAIDA_CC_ALV-NFENUM_REM   = WA_ZSDT_RETLOTE-NFENUM.
        WA_SAIDA_CC_ALV-DOCNUM_RET   = WA_ZSDT_RETLOTE-DOCNUM_RET.

        WA_SAIDA_CC_ALV-NTGEW_REM    = WA_J_1BNFLIN_SUM_VLR-TOTAL.
        WA_SAIDA_CC_ALV-CFOP_REM     = WA_J_1BNFLIN_SUM_VLR-CFOP.

        WA_SAIDA_CC_ALV-MATNR        = WA_MAKT-MATNR.
        WA_SAIDA_CC_ALV-MAKTX        = WA_MAKT-MAKTX.

        WA_SAIDA_CC_ALV-QUANT        = WA_ZSDT_EXPORT-QUANT.
        WA_SAIDA_CC_ALV-DATA_CRIACAO = WA_ZSDT_EXPORT-DATA_CRIACAO.
        WA_SAIDA_CC_ALV-NF_RETORNO   = WA_ZSDT_EXPORT-NF_RETORNO.

        READ TABLE IT_J_1BNFLIN INTO WA_J_1BNFLIN WITH KEY REFKEY = WA_VBFA-VBELN_REFKEY BINARY SEARCH.

        WA_SAIDA_CC_ALV-CFOP_EXP = WA_J_1BNFLIN-CFOP.

        IF ( SY-SUBRC EQ 0 ).

          READ TABLE IT_J_1BNFDOC INTO WA_J_1BNFDOC WITH KEY DOCNUM = WA_J_1BNFLIN-DOCNUM BINARY SEARCH.
          WA_SAIDA_CC_ALV-CREDAT     = WA_J_1BNFDOC-CREDAT.
          WA_SAIDA_CC_ALV-NFENUM_EXP = WA_J_1BNFDOC-NFENUM.
          WA_SAIDA_CC_ALV-NTGEW_EXP  = WA_J_1BNFDOC-NTGEW.

        ENDIF.

        READ TABLE IT_VBRP INTO WA_VBRP WITH KEY VBELN = WA_VBFA-VBELN BINARY SEARCH.

        IF ( SY-SUBRC EQ 0 ).

          READ TABLE IT_ZDOC_EXP INTO WA_ZDOC_EXP WITH KEY VBELN = WA_VBRP-VGBEL BINARY SEARCH.

          IF ( SY-SUBRC EQ 0 ).

            READ TABLE IT_ZREG_EXPORTACAO INTO WA_ZREG_EXPORTACAO WITH KEY ID_REGISTRO_EXPO = WA_ZDOC_EXP-ID_REGISTRO_EXPO BINARY SEARCH.
            WA_SAIDA_CC_ALV-NR_REGISTRO_EXPO = WA_ZREG_EXPORTACAO-NR_REGISTRO_EXPO.

            READ TABLE IT_ZDDE INTO WA_ZDDE WITH KEY ID_DDE = WA_ZDOC_EXP-ID_DDE BINARY SEARCH.
            WA_SAIDA_CC_ALV-NR_DDE = WA_ZDDE-NR_DDE.


          ENDIF.
        ENDIF.

        APPEND WA_SAIDA_CC_ALV TO IT_SAIDA_CC_ALV.

        CLEAR: WA_SAIDA_CC_ALV,
               WA_VBFA,
               WA_J_1BNFLIN,
               WA_J_1BNFDOC,
               WA_VBRP,
               WA_ZDOC_EXP,
               WA_ZREG_EXPORTACAO,
               WA_ZDDE.
      ENDLOOP.
    ENDIF.
    CLEAR:
            WA_ZSDT_RETLOTE,
            WA_J_1BNFLIN_SUM_VLR,
            WA_MAKT,
            WA_ZSDT_EXPORT.

  ENDLOOP.

ENDFORM.                    " DATA_SELECT_EXIT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR  'FB0200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  IF ( SY-DYNNR EQ TELA_CC_ALV_LOTE ).
    CASE SY-UCOMM.
      WHEN: 'BACK'.
        LEAVE TO SCREEN 0.
      WHEN: 'CANC'.
        LEAVE TO SCREEN 0.
      WHEN: 'EXIT'.
        LEAVE PROGRAM.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

*----------------------------------------------------------------------*
*  MODULE create_object OUTPUT
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT_REM OUTPUT.

  IF ( GR_ALVGRID_REM IS INITIAL ).

    CREATE OBJECT GR_CONTAINER_REM
      EXPORTING
        CONTAINER_NAME              = GC_CONTAINER_NAME_LOTE
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT GR_ALVGRID_REM
      EXPORTING
        I_PARENT          = GR_CONTAINER_REM
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    PERFORM: PREPARE_FIELDCATALOG_REM.

    CALL METHOD GR_ALVGRID_REM->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT_REM
      CHANGING
        IT_OUTTAB                     = IT_SAIDA_CC_ALV_REM[]
        IT_FIELDCATALOG               = IT_FIELDCATALOG_REM
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.


    CALL METHOD GR_ALVGRID_REM->REFRESH_TABLE_DISPLAY
      EXCEPTIONS
        FINISHED = 1
        OTHERS   = 2.

  ELSE.
    CALL METHOD GR_ALVGRID_REM->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDIF.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  prepare_fieldcatalog
*&---------------------------------------------------------------------*
FORM PREPARE_FIELDCATALOG_REM .

  REFRESH: IT_FIELDCATALOG_REM.

  PERFORM FIELDCATALOG_REM USING:

      'DOCNUM'       '14'  C_DOCNUM_REM       'X' '',
      'NFENUM'       '12'  C_NFENUM_REM       'X' '',
      'WERKS'        '15'  C_WERKS_REM        'X' '',
      'NF_RETORNO'   '9'   C_NF_RETORNO_REM   'X' '',
      'DOCNUM_RET'   '10'  C_DOCNUM_RET_REM   'X' '',
      'DATA_CRIACAO' '10'  C_DATA_CRIACAO_REM ''  '',
      'DATA_SAIDA'   '9'   C_DATA_SAIDA_REM   ''  '',
      'SAFRA'        '10'  C_SAFRA_REM        ''  '',
      'MATNR'        '13'  C_MATNR_REM        'X' '',
      'QUANT_VINC'   '10'  C_QUANT_VINC_REM   ''  '',
      'VLR_TOTAL'    '15'  C_VLR_TOTAL_REM    ''  ''.

ENDFORM.                    " PREPARE_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  fieldcatalog
*&---------------------------------------------------------------------*
FORM FIELDCATALOG_REM  USING VALUE(P_FIELDNAME)
                             VALUE(P_OUTPUTLEN)
                             VALUE(P_COLTEXT)
                             VALUE(P_NO_ZERO)
                             VALUE(P_HOTSPOT).

  CLEAR: WA_FIELDCATALOG_REM.

  WA_FIELDCATALOG_REM-FIELDNAME = P_FIELDNAME.
  WA_FIELDCATALOG_REM-OUTPUTLEN = P_OUTPUTLEN.
  WA_FIELDCATALOG_REM-COLTEXT   = P_COLTEXT.
  WA_FIELDCATALOG_REM-SELTEXT   = P_COLTEXT.
  WA_FIELDCATALOG_REM-NO_ZERO   = P_NO_ZERO.
  WA_FIELDCATALOG_REM-HOTSPOT   = P_HOTSPOT.

  APPEND WA_FIELDCATALOG_REM TO IT_FIELDCATALOG_REM.

ENDFORM.                    " FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT_REMESSA_FORM_LOTE
*&---------------------------------------------------------------------*
FORM DATA_SELECT_REMESSA_FORM_LOTE  USING P_DOCNUM_RET
                                          P_MATNR.

  SELECT DOCNUM NFENUM WERKS NF_RETORNO DOCNUM_RET DATA_CRIACAO DATA_SAIDA SAFRA MATNR QUANT_VINC VLR_TOTAL
      FROM ZSDT_RETLOTE
      INTO TABLE IT_ZSDT_RETLOTE_REM
    WHERE DOCNUM_RET EQ P_DOCNUM_RET
      AND MATNR EQ P_MATNR.

  REFRESH IT_SAIDA_CC_ALV_REM.
  LOOP AT IT_ZSDT_RETLOTE_REM INTO WA_ZSDT_RETLOTE_REM.


    WA_SAIDA_CC_ALV_REM-DOCNUM       = WA_ZSDT_RETLOTE_REM-DOCNUM.
    WA_SAIDA_CC_ALV_REM-NFENUM       = WA_ZSDT_RETLOTE_REM-NFENUM.
    WA_SAIDA_CC_ALV_REM-WERKS        = WA_ZSDT_RETLOTE_REM-WERKS.
    WA_SAIDA_CC_ALV_REM-NF_RETORNO   = WA_ZSDT_RETLOTE_REM-NF_RETORNO.
    WA_SAIDA_CC_ALV_REM-DOCNUM_RET   = WA_ZSDT_RETLOTE_REM-DOCNUM_RET.
    WA_SAIDA_CC_ALV_REM-DATA_CRIACAO = WA_ZSDT_RETLOTE_REM-DATA_CRIACAO.
    WA_SAIDA_CC_ALV_REM-DATA_SAIDA   = WA_ZSDT_RETLOTE_REM-DATA_SAIDA.
    WA_SAIDA_CC_ALV_REM-SAFRA        = WA_ZSDT_RETLOTE_REM-SAFRA.
    WA_SAIDA_CC_ALV_REM-MATNR        = WA_ZSDT_RETLOTE_REM-MATNR.
    WA_SAIDA_CC_ALV_REM-QUANT_VINC   = WA_ZSDT_RETLOTE_REM-QUANT_VINC.
    WA_SAIDA_CC_ALV_REM-VLR_TOTAL    = WA_ZSDT_RETLOTE_REM-VLR_TOTAL.

    APPEND WA_SAIDA_CC_ALV_REM TO IT_SAIDA_CC_ALV_REM.

    CLEAR: WA_ZSDT_RETLOTE_REM, WA_SAIDA_CC_ALV_REM.

  ENDLOOP.

  IF NOT ( IT_SAIDA_CC_ALV_REM IS INITIAL ).
    CALL SCREEN TELA_CC_ALV_LOTE.
  ENDIF.
ENDFORM.                    " DATA_SELECT_REMESSA_FORM_LOTE
