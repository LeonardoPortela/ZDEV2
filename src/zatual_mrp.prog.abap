*&---------------------------------------------------------------------*
*& Report  ZATUAL_USUARIO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZATUAL_MRP.

TYPES: BEGIN OF TY_MRP,
         WERKS     TYPE RMMG1-WERKS,
         MATNR     TYPE RMMG1-MATNR,
         DISMM     TYPE MARC-DISMM,
         BSTMI(10), " TYPE MARC-BSTMI,
         PLIFZ(10), " TYPE MARC-PLIFZ,
         SHZET     TYPE MARC-SHZET,
         MTVFP     TYPE MARC-MTVFP,
       END OF TY_MRP,

       BEGIN OF TY_ERRO,
         MSG(100),
       END OF TY_ERRO.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: TI_BDCDATA TYPE STANDARD TABLE OF BDCDATA,   "Guarda o mapeamento
      IT_ERRO    TYPE TABLE OF TY_ERRO.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_BDCDATA      LIKE LINE OF TI_BDCDATA,
      WA_MRP          TYPE TY_MRP,
      WA_MARA         TYPE MARA,
      WA_ZTWF_MAT_SEQ TYPE ZTWF_MAT_SEQ,
      WA_ERRO         TYPE TY_ERRO.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.
DATA: WL_MODE(1),
      WG_DOCUMENTO(10),
* ---> S4 Migração - 19/06/2023 - FC - Inicio
      "VMSG(50),
      VMSG(62),
* <--- S4 Migração - 19/06/2023 - FC - Fim
      WL_ERRO(1).




PARAMETER P_FILE TYPE RLGRAP-FILENAME DEFAULT 'C:\maggi\MRP.xlsx'.

DATA: T_EXCEL  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE,
      T_EXCEL2 LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.


CLEAR T_EXCEL.
REFRESH T_EXCEL.

CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  EXPORTING
    FILENAME                = P_FILE
    I_BEGIN_COL             = 1
    I_BEGIN_ROW             = 2
    I_END_COL               = 7
    I_END_ROW               = 10000
  TABLES
    INTERN                  = T_EXCEL
  EXCEPTIONS
    INCONSISTENT_PARAMETERS = 1
    UPLOAD_OLE              = 2
    OTHERS                  = 3.

CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
  EXPORTING
    TEXT = 'Atualizando Dados'.

T_EXCEL2[] = T_EXCEL[].
SORT T_EXCEL2 BY ROW COL.
CLEAR T_EXCEL2.
LOOP AT T_EXCEL.
  IF T_EXCEL-ROW = T_EXCEL2-ROW.
    CONTINUE.
  ENDIF.
  LOOP AT T_EXCEL2 WHERE ROW = T_EXCEL-ROW.
    CASE T_EXCEL2-COL.
      WHEN 1.
        WA_MRP-WERKS  = T_EXCEL2-VALUE.
      WHEN 2.
        WA_MRP-MATNR  = T_EXCEL2-VALUE.
      WHEN 3.
        WA_MRP-DISMM  = T_EXCEL2-VALUE.
      WHEN 4.
        WA_MRP-BSTMI  = T_EXCEL2-VALUE.
      WHEN 5.
        WA_MRP-PLIFZ  = T_EXCEL2-VALUE.
      WHEN 6.
        WA_MRP-SHZET  = T_EXCEL2-VALUE.
      WHEN 7.
        WA_MRP-MTVFP  = T_EXCEL2-VALUE.
    ENDCASE.
  ENDLOOP.
  CONCATENATE 'Linha ' T_EXCEL-ROW 'MATERIAL '  WA_MRP-MATNR INTO VMSG SEPARATED BY SPACE.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = VMSG.

*  SELECT SINGLE *
*    FROM MARA
*    INTO WA_MARA
*  WHERE MATNR = WA_MRP-MATNR.

  REFRESH TI_BDCDATA.
  PERFORM F_BDC_DATA USING:
        'SAPLMGMM'  '0060'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=ENTR',
        ''          ''      ''   'RMMG1-MATNR'        WA_MRP-MATNR,

        'SAPLMGMM'  '0070'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=ENTR',
        ''          ''      ''   'MSICHTAUSW-KZSEL(12)'       'X',
        ''          ''      ''   'MSICHTAUSW-KZSEL(13)'       'X',
        ''          ''      ''   'MSICHTAUSW-KZSEL(14)'       'X',

        'SAPLMGMM'  '0080'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=ENTR',
        ''          ''      ''   'RMMG1-WERKS'       WA_MRP-WERKS,

        'SAPLMGMM'  '4000'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=SP13',
        ''          ''      ''   'MARC-DISMM'       WA_MRP-DISMM,
        ''          ''      ''   'MARC-BSTMI'       WA_MRP-BSTMI,

        'SAPLMGMM'  '4000'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=SP14',
        ''          ''      ''   'MARC-PLIFZ'       WA_MRP-PLIFZ,
        ''          ''      ''   'MARC-SHZET'       WA_MRP-SHZET,

        'SAPLMGMM'  '4000'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=BU',
        ''          ''      ''   'MARC-MTVFP'       WA_MRP-MTVFP.

  CLEAR WL_ERRO.
  PERFORM ZF_CALL_TRANSACTION USING 'MM01' CHANGING WL_ERRO.


ENDLOOP.
MESSAGE 'Fim atualização' TYPE 'I'.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_program   programa
*      -->P_dynpro    tela
*      -->P_start     define a tela
*      -->P_fnam      nome do campo ou comando
*      -->P_fval      conteúdo do campo ou comando
*----------------------------------------------------------------------*
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

*&---------------------------------------------------------------------*
*&      Form  zf_call_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*      -->P_ERRO     text
*----------------------------------------------------------------------*
FORM ZF_CALL_TRANSACTION USING P_TRANS CHANGING P_ERRO.
  CONSTANTS: C_MSGID LIKE IT_MSG-MSGID VALUE 'F5',
             C_MSGNR LIKE IT_MSG-MSGNR VALUE '312',
             C_MSGNE LIKE IT_MSG-MSGNR VALUE '539'.

  REFRESH IT_MSG.

  WL_MODE = 'E'.

  CALL TRANSACTION P_TRANS USING TI_BDCDATA
        MODE WL_MODE
        MESSAGES INTO IT_MSG.

  READ TABLE IT_MSG WITH KEY MSGTYP = 'A'.
  IF SY-SUBRC = 0.
    P_ERRO = 'X'.
  ELSE.
    READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      P_ERRO = 'X'.
    ELSE.
      READ TABLE IT_MSG WITH KEY MSGTYP = 'S'.
      IF SY-SUBRC NE 0.
        P_ERRO = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.






ENDFORM.                    "ZF_CALL_TRANSACTION
