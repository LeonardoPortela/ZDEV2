*&---------------------------------------------------------------------*
*& Report  ZATUAL_CLIENTE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZATUAL_CLIENTE.

TYPES: BEGIN OF TY_CLIENTE,
       CODESYS(20),
       KTOKD         TYPE RF02D-KTOKD,
       BUKRS         TYPE RF02D-BUKRS,
       TITLE_MEDI    TYPE SZA1_D0100-TITLE_MEDI,
       NAME1         TYPE ADDR1_DATA-NAME1,
       NAME2         TYPE ADDR1_DATA-NAME2,
       SORT1         TYPE ADDR1_DATA-SORT1,
       STREET        TYPE ADDR1_DATA-STREET,
       HOUSE_NUM1    TYPE ADDR1_DATA-HOUSE_NUM1,
       POST_CODE1    TYPE ADDR1_DATA-POST_CODE1,
       CITY1         TYPE ADDR1_DATA-CITY1,
       COUNTRY       TYPE ADDR1_DATA-COUNTRY,
       TEL_NUMBER    TYPE SZA1_D0100-TEL_NUMBER,
       MOB_NUMBER    TYPE SZA1_D0100-MOB_NUMBER,
       FAX_NUMBER    TYPE SZA1_D0100-FAX_NUMBER,
       SMTP_ADDR     TYPE SZA1_D0100-SMTP_ADDR,
       STCD1         TYPE KNA1-STCD1,
       Q_VATCODE(15),
       AKONT         TYPE KNB1-AKONT,
       FDGRV         TYPE KNB1-FDGRV,
       ALTKN         TYPE KNB1-ALTKN,
       ZTERM         TYPE KNB1-ZTERM,
       XZVER         TYPE KNB1-XZVER,
       ZWELS         TYPE KNB1-ZWELS,
       SEQ           TYPE I,
   END OF TY_CLIENTE,

   BEGIN OF TY_FORNECEDOR,
      CODESYS(20),
      BUKRS         TYPE RF02K-BUKRS,
      EKORG         TYPE RF02K-EKORG,
      KTOKK         TYPE RF02K-KTOKK,
      TITLE_MEDI    TYPE SZA1_D0100-TITLE_MEDI,
      NAME1         TYPE ADDR1_DATA-NAME1,
      NAME2         TYPE ADDR1_DATA-NAME2,
      SORT1         TYPE ADDR1_DATA-SORT1,
      STREET        TYPE ADDR1_DATA-STREET,
      HOUSE_NUM1    TYPE ADDR1_DATA-HOUSE_NUM1,
      POST_CODE1    TYPE ADDR1_DATA-POST_CODE1,
      CITY1         TYPE ADDR1_DATA-CITY1,
      COUNTRY       TYPE ADDR1_DATA-COUNTRY,
      TEL_NUMBER    TYPE SZA1_D0100-TEL_NUMBER,
      MOB_NUMBER    TYPE SZA1_D0100-MOB_NUMBER,
      FAX_NUMBER    TYPE SZA1_D0100-FAX_NUMBER,
      SMTP_ADDR     TYPE SZA1_D0100-SMTP_ADDR,
      STCD1         TYPE KNA1-STCD1,
      R_VATCODE(15),
      AKONT         TYPE KNB1-AKONT,
      FDGRV         TYPE KNB1-FDGRV,
      ALTKN         TYPE KNB1-ALTKN,
      ZTERM         TYPE KNB1-ZTERM,
      ZWELS         TYPE KNB1-ZWELS,
      WAERS         TYPE LFM1-WAERS,
   END OF TY_FORNECEDOR,


  BEGIN OF TY_ERRO,
      MSG(100),
  END OF TY_ERRO.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: TI_BDCDATA          TYPE STANDARD TABLE OF BDCDATA,   "Guarda o mapeamento
      IT_ERRO             TYPE TABLE OF TY_ERRO.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_BDCDATA    LIKE LINE OF TI_BDCDATA ,
      WA_CLIENTE    TYPE TY_CLIENTE,
      WA_FORNECEDOR TYPE TY_FORNECEDOR,
      WA_ERRO       TYPE TY_ERRO.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.
DATA: WL_MODE(1),
      WG_DOCUMENTO(10),
      VMSG(50),
      WL_ERRO(1),
      WL_SEQ TYPE I,
      WL_SEQ1(2),
      WL_SEQ2(5).



PARAMETER P_FILE TYPE RLGRAP-FILENAME DEFAULT ''.
SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: R_CLI   RADIOBUTTON GROUP RAD1,
            R_FOR   RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN: END OF BLOCK B2.

SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS: R_MOD   LIKE BSID-UMSKZ AS CHECKBOX  DEFAULT ' '.
SELECTION-SCREEN: END OF BLOCK B3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_FILENAME     = ' '
      DEF_PATH         = P_FILE
      MASK             = ',*.xlsx.'
      MODE             = 'O'
      TITLE            = 'Arquivo a importar !'
    IMPORTING
      FILENAME         = P_FILE
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

START-OF-SELECTION.

  DATA: T_EXCEL   LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE ,
        T_EXCEL2  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE .


  CLEAR T_EXCEL.
  REFRESH T_EXCEL.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 3
      I_END_COL               = 25
      I_END_ROW               = 1000
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
  IF R_CLI = 'X'.
    PERFORM F_CLIENTE.
  ELSE.
    PERFORM F_FORNECEDOR.
  ENDIF.

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
  IF R_MOD = 'X'.
    WL_MODE = 'A'.
  ENDIF.

  CALL TRANSACTION P_TRANS USING TI_BDCDATA
        MODE WL_MODE
        UPDATE 'S'
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
*&---------------------------------------------------------------------*
*&      Form  F_CLIENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CLIENTE .
  LOOP AT T_EXCEL.
    IF T_EXCEL-ROW = T_EXCEL2-ROW.
      CONTINUE.
    ENDIF.
    CLEAR WA_CLIENTE.
    LOOP AT T_EXCEL2 WHERE ROW = T_EXCEL-ROW.
      CASE T_EXCEL2-COL.
        WHEN 2.
          WA_CLIENTE-KTOKD        = T_EXCEL2-VALUE.
        WHEN 3.
          WA_CLIENTE-BUKRS        = T_EXCEL2-VALUE.
        WHEN 4.
          WA_CLIENTE-TITLE_MEDI   = T_EXCEL2-VALUE.
        WHEN 5.
          WA_CLIENTE-NAME1        = T_EXCEL2-VALUE.
        WHEN 6.
          WA_CLIENTE-NAME2        = T_EXCEL2-VALUE.
        WHEN 7.
          WA_CLIENTE-SORT1        = T_EXCEL2-VALUE.
        WHEN 8.
          WA_CLIENTE-STREET       = T_EXCEL2-VALUE.
        WHEN 9.
          WA_CLIENTE-HOUSE_NUM1   = T_EXCEL2-VALUE.
        WHEN 10.
          WA_CLIENTE-POST_CODE1   = T_EXCEL2-VALUE.
        WHEN 11.
          WA_CLIENTE-CITY1        = T_EXCEL2-VALUE.
        WHEN 12.
          WA_CLIENTE-COUNTRY      = T_EXCEL2-VALUE.
        WHEN 13.
          WA_CLIENTE-TEL_NUMBER   = T_EXCEL2-VALUE.
        WHEN 14.
          WA_CLIENTE-MOB_NUMBER   = T_EXCEL2-VALUE.
        WHEN 15.
          WA_CLIENTE-FAX_NUMBER   = T_EXCEL2-VALUE.
        WHEN 16.
          WA_CLIENTE-SMTP_ADDR    = T_EXCEL2-VALUE.
        WHEN 17.
          WA_CLIENTE-STCD1        = T_EXCEL2-VALUE.
        WHEN 19.
          WA_CLIENTE-AKONT        = T_EXCEL2-VALUE.
        WHEN 20.
          WA_CLIENTE-FDGRV        = T_EXCEL2-VALUE.
        WHEN 21.
          WA_CLIENTE-ALTKN        = T_EXCEL2-VALUE.
        WHEN 22.
          WA_CLIENTE-ZTERM        = T_EXCEL2-VALUE.
        WHEN 23.
          WA_CLIENTE-XZVER        = T_EXCEL2-VALUE.
        WHEN 24.
          WA_CLIENTE-ZWELS        = T_EXCEL2-VALUE.
      ENDCASE.
    ENDLOOP.
    CONCATENATE 'Linha ' T_EXCEL-ROW 'Nome '  WA_CLIENTE-NAME1 INTO VMSG SEPARATED BY SPACE.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = VMSG.

    REFRESH TI_BDCDATA.
    PERFORM F_BDC_DATA USING:
          'SAPMF02D'  '0105'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '/00',
          ''          ''      ''   'RF02D-KTOKD'        WA_CLIENTE-KTOKD,
          ''          ''      ''   'RF02D-BUKRS'        WA_CLIENTE-BUKRS,
          ''          ''      ''   'USE_ZAV'            'X',
          'SAPMF02D'  '0111'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',
          ''          ''      ''   'SZA1_D0100-TITLE_MEDI'       WA_CLIENTE-TITLE_MEDI,
          ''          ''      ''   'ADDR1_DATA-NAME1'            WA_CLIENTE-NAME1,
          ''          ''      ''   'ADDR1_DATA-NAME2'            WA_CLIENTE-NAME2,
          ''          ''      ''   'ADDR1_DATA-SORT1'            WA_CLIENTE-SORT1,
          ''          ''      ''   'ADDR1_DATA-STREET'           WA_CLIENTE-STREET,
          ''          ''      ''   'ADDR1_DATA-HOUSE_NUM1'       WA_CLIENTE-HOUSE_NUM1,
          ''          ''      ''   'ADDR1_DATA-POST_CODE1'       WA_CLIENTE-POST_CODE1,
          ''          ''      ''   'ADDR1_DATA-CITY1'            WA_CLIENTE-CITY1,
          ''          ''      ''   'ADDR1_DATA-COUNTRY'          WA_CLIENTE-COUNTRY,
          ''          ''      ''   'ADDR1_DATA-REGION'           '',
          ''          ''      ''   'ADDR1_DATA-LANGU'            'EN',
          ''          ''      ''   'SZA1_D0100-TEL_NUMBER'       WA_CLIENTE-TEL_NUMBER,
          ''          ''      ''   'SZA1_D0100-MOB_NUMBER'       WA_CLIENTE-MOB_NUMBER,
          ''          ''      ''   'SZA1_D0100-FAX_NUMBER'       WA_CLIENTE-FAX_NUMBER,
          ''          ''      ''   'SZA1_D0100-SMTP_ADDR'        WA_CLIENTE-SMTP_ADDR,

          'SAPMF02D'  '0120'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',
          ''          ''      ''   'KNA1-STCD1'                 WA_CLIENTE-STCD1,

          'SAPMF02D'  '0130'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',

          'SAPMF02D'  '0210'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',
          ''          ''      ''   'KNB1-AKONT'                WA_CLIENTE-AKONT,
          ''          ''      ''   'KNB1-FDGRV'                WA_CLIENTE-FDGRV,
          ''          ''      ''   'KNB1-ALTKN'                WA_CLIENTE-ALTKN,

          'SAPMF02D'  '0215'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',
          ''          ''      ''   'KNB1-ZTERM'                WA_CLIENTE-ZTERM,
          ''          ''      ''   'KNB1-ZWELS'                WA_CLIENTE-ZWELS,

          'SAPMF02D'  '0220'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',

          'SAPMF02D'  '0230'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW'.

    CLEAR WL_ERRO.
    PERFORM ZF_CALL_TRANSACTION USING 'FD01' CHANGING WL_ERRO.


  ENDLOOP.
ENDFORM.                    " F_CLIENTE
*&---------------------------------------------------------------------*
*&      Form  F_FORNECEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_FORNECEDOR .
  LOOP AT T_EXCEL.
    IF T_EXCEL-ROW = T_EXCEL2-ROW.
      CONTINUE.
    ENDIF.
    CLEAR WA_FORNECEDOR.
    LOOP AT T_EXCEL2 WHERE ROW = T_EXCEL-ROW.
      CASE T_EXCEL2-COL.
        WHEN 2.
          WA_FORNECEDOR-BUKRS        = T_EXCEL2-VALUE.
        WHEN 3.
          WA_FORNECEDOR-EKORG        = T_EXCEL2-VALUE.
        WHEN 4.
          WA_FORNECEDOR-KTOKK        = T_EXCEL2-VALUE.
        WHEN 5.
          WA_FORNECEDOR-TITLE_MEDI   = T_EXCEL2-VALUE.
        WHEN 6.
          WA_FORNECEDOR-NAME1        = T_EXCEL2-VALUE.
        WHEN 7.
          WA_FORNECEDOR-NAME2        = T_EXCEL2-VALUE.
        WHEN 8.
          WA_FORNECEDOR-SORT1        = T_EXCEL2-VALUE.
        WHEN 9.
          WA_FORNECEDOR-STREET       = T_EXCEL2-VALUE.
        WHEN 10.
          WA_FORNECEDOR-HOUSE_NUM1   = T_EXCEL2-VALUE.
        WHEN 11.
          WA_FORNECEDOR-POST_CODE1   = T_EXCEL2-VALUE.
        WHEN 12.
          WA_FORNECEDOR-CITY1        = T_EXCEL2-VALUE.
        WHEN 13.
          WA_FORNECEDOR-COUNTRY      = T_EXCEL2-VALUE.
        WHEN 14.
          WA_FORNECEDOR-TEL_NUMBER   = T_EXCEL2-VALUE.
        WHEN 15.
          WA_FORNECEDOR-MOB_NUMBER   = T_EXCEL2-VALUE.
        WHEN 16.
          WA_FORNECEDOR-FAX_NUMBER   = T_EXCEL2-VALUE.
        WHEN 17.
          WA_FORNECEDOR-SMTP_ADDR    = T_EXCEL2-VALUE.
        WHEN 18.
          WA_FORNECEDOR-STCD1        = T_EXCEL2-VALUE.
        WHEN 20.
          WA_FORNECEDOR-AKONT        = T_EXCEL2-VALUE.
        WHEN 21.
          WA_FORNECEDOR-FDGRV        = T_EXCEL2-VALUE.
        WHEN 22.
          WA_FORNECEDOR-ALTKN        = T_EXCEL2-VALUE.
        WHEN 23.
          WA_FORNECEDOR-ZTERM        = T_EXCEL2-VALUE.
        WHEN 24.
          WA_FORNECEDOR-ZWELS        = T_EXCEL2-VALUE.
        WHEN 25.
          WA_FORNECEDOR-WAERS        = T_EXCEL2-VALUE.
      ENDCASE.
    ENDLOOP.
    CONCATENATE 'Linha ' T_EXCEL-ROW 'Nome '  WA_FORNECEDOR-NAME1 INTO VMSG SEPARATED BY SPACE.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = VMSG.

    REFRESH TI_BDCDATA.
    PERFORM F_BDC_DATA USING:
          'SAPMF02K'  '0100'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '/00',
          ''          ''      ''   'BDC_CURSOR'        'RF02K-KTOKK',
          ''          ''      ''   'RF02K-BUKRS'        WA_FORNECEDOR-BUKRS,
          ''          ''      ''   'RF02K-EKORG'        WA_FORNECEDOR-EKORG,
          ''          ''      ''   'RF02K-KTOKK'        WA_FORNECEDOR-KTOKK,
          ''          ''      ''   'USE_ZAV'            'X',

          'SAPMF02K'  '0111'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',
          ''          ''      ''   'BDC_SUBSCR'        'SAPLSZA1                                0300ADDRESS',
          ''          ''      ''   'BDC_SUBSCR'        'SAPLSZA1                                0301COUNTRY_SCREEN',
          ''          ''      ''   'SZA1_D0100-TITLE_MEDI'       WA_FORNECEDOR-TITLE_MEDI,
          ''          ''      ''   'ADDR1_DATA-NAME1'            WA_FORNECEDOR-NAME1,
          ''          ''      ''   'ADDR1_DATA-NAME2'            WA_FORNECEDOR-NAME2,
          ''          ''      ''   'ADDR1_DATA-SORT1'            WA_FORNECEDOR-SORT1,
          ''          ''      ''   'ADDR1_DATA-STREET'           WA_FORNECEDOR-STREET,
          ''          ''      ''   'ADDR1_DATA-HOUSE_NUM1'       WA_FORNECEDOR-HOUSE_NUM1,
          ''          ''      ''   'ADDR1_DATA-POST_CODE1'       WA_FORNECEDOR-POST_CODE1,
          ''          ''      ''   'ADDR1_DATA-CITY1'            WA_FORNECEDOR-CITY1,
          ''          ''      ''   'ADDR1_DATA-COUNTRY'          WA_FORNECEDOR-COUNTRY,
          "''          ''      ''   'ADDR1_DATA-REGION'           '001',
          ''          ''      ''   'ADDR1_DATA-LANGU'            'EN',
          ''          ''      ''   'SZA1_D0100-TEL_NUMBER'       WA_FORNECEDOR-TEL_NUMBER,
          ''          ''      ''   'SZA1_D0100-MOB_NUMBER'       WA_FORNECEDOR-MOB_NUMBER,
          ''          ''      ''   'SZA1_D0100-FAX_NUMBER'       WA_FORNECEDOR-FAX_NUMBER,
          ''          ''      ''   'SZA1_D0100-SMTP_ADDR'        WA_FORNECEDOR-SMTP_ADDR,
          ''          ''      ''   'BDC_SUBSCR'        'SAPLSZA1                                1300APPL_SUB_T',
          ''          ''      ''   'BDC_SUBSCR'        'SAPLSEXM                                0200APPL_SUB',

          'SAPMF02K'  '0120'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',
          ''          ''      ''   'LFA1-STCD1'                 WA_FORNECEDOR-STCD1,

          'SAPMF02K'  '0130'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',

          'SAPMF02K'  '0380'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',

          'SAPMF02K'  '0210'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',
          ''          ''      ''   'LFB1-AKONT'                WA_FORNECEDOR-AKONT,
          ''          ''      ''   'LFB1-FDGRV'                WA_FORNECEDOR-FDGRV,
          ''          ''      ''   'LFB1-ALTKN'                WA_FORNECEDOR-ALTKN,

          'SAPMF02K'  '0215'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',
          ''          ''      ''   'LFB1-ZTERM'                WA_FORNECEDOR-ZTERM,
          ''          ''      ''   'LFB1-ZWELS'                WA_FORNECEDOR-ZWELS,

          'SAPMF02K'  '0220'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=VW',

          'SAPMF02K'  '0310'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'        '=UPDA',
          ''          ''      ''   'LFM1-WAERS'                WA_FORNECEDOR-WAERS.

    CLEAR WL_ERRO.
    PERFORM ZF_CALL_TRANSACTION USING 'XK01' CHANGING WL_ERRO.


  ENDLOOP.
ENDFORM.                    " F_FORNECEDOR
