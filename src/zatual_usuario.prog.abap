*&---------------------------------------------------------------------*
*& Report  ZATUAL_USUARIO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZATUAL_USUARIO.

TYPES: BEGIN OF TY_USUARIO,
       USUARIOSAP  	 TYPE USR02-BNAME,
       CARGO         TYPE ADDR3_DATA-FUNCTION,
       FILIAL	       TYPE ADDR3_DATA-DEPARTMENT,
       CPF           TYPE SZA5_D0700-FAX_NUMBER,
       EMAIL         TYPE SZA5_D0700-SMTP_ADDR,
       COLIGADA      TYPE ADDR3_DATA-DEFLT_COMM,
       USERREDE      TYPE USREFUS-USERALIAS,
       CODCCUSTO     TYPE ADDR3_DATA-ROOMNUM_C,
       TIPO          TYPE US930-USERTYP,
       SEQ           TYPE I,
   END OF TY_USUARIO,

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
      WA_USUARIO    TYPE TY_USUARIO,
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
      WL_SEQ type i,
      WL_SEQ1(2),
      WL_SEQ2(5).



PARAMETER P_FILE TYPE RLGRAP-FILENAME DEFAULT 'C:\maggi\us_todas.xlsx'.

DATA: T_EXCEL   LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE ,
      T_EXCEL2  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE .


CLEAR T_EXCEL.
REFRESH T_EXCEL.

CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  EXPORTING
    FILENAME                = P_FILE
    I_BEGIN_COL             = 1
    I_BEGIN_ROW             = 2
    I_END_COL               = 11
    I_END_ROW               = 806
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
        WA_USUARIO-USUARIOSAP  = T_EXCEL2-VALUE.
      WHEN 2.
        WA_USUARIO-CARGO       = T_EXCEL2-VALUE.
      WHEN 3.
        WA_USUARIO-FILIAL      = T_EXCEL2-VALUE.
      WHEN 4.
        WA_USUARIO-CPF         = T_EXCEL2-VALUE.
      WHEN 5.
        WA_USUARIO-EMAIL       = T_EXCEL2-VALUE.
      WHEN 6.
        WA_USUARIO-COLIGADA    = T_EXCEL2-VALUE.
      WHEN 7.
        WA_USUARIO-USERREDE    = T_EXCEL2-VALUE.
      WHEN 8.
        WA_USUARIO-CODCCUSTO   = T_EXCEL2-VALUE.
      WHEN 9.
        WA_USUARIO-TIPO  = T_EXCEL2-VALUE.
      WHEN 10.
        WA_USUARIO-SEQ  = T_EXCEL2-VALUE.
    ENDCASE.
  ENDLOOP.
  CONCATENATE 'Linha ' T_EXCEL-ROW 'User '  WA_USUARIO-USUARIOSAP INTO VMSG SEPARATED BY SPACE.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = VMSG.
  WL_SEQ = WA_USUARIO-SEQ + 3.
  WL_SEQ1 = WL_SEQ .
   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WL_SEQ1
            IMPORTING
              OUTPUT = WL_SEQ1.

  CONCATENATE  WL_SEQ1 '/03' into WL_SEQ2.

  REFRESH TI_BDCDATA.
  PERFORM F_BDC_DATA USING:
        'SAPLSUU5'  '0050'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=CHAN',
        ''          ''      ''   'USR02-BNAME'        WA_USUARIO-USUARIOSAP,
        'SAPLSUU5'  '0100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'       '=$OCM',
        ''          ''      ''   'BDC_SUBSCR'       'SAPLSZA5                                0900MAINAREA',
        ''          ''      ''   'BDC_CURSOR'       'SZA5_D0700-SMTP_ADDR',
        ''          ''      ''   'ADDR3_DATA-FUNCTION'         WA_USUARIO-CARGO,
        ''          ''      ''   'ADDR3_DATA-DEPARTMENT'       WA_USUARIO-FILIAL,
        ''          ''      ''   'SZA5_D0700-FAX_NUMBER'       WA_USUARIO-CPF,
        ''          ''      ''   'SZA5_D0700-SMTP_ADDR'        WA_USUARIO-EMAIL,
        ''          ''      ''   'ADDR3_DATA-ROOMNUM_C'        WA_USUARIO-CODCCUSTO ,
        "''          ''      ''   'ADDR3_DATA-DEFLT_COMM'       'RML', "WA_USUARIO-COLIGADA,
        'SAPMSSY0'  '0120'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_CURSOR'       WL_SEQ2,
        ''          ''      ''   'BDC_OKCODE'       '=PICK',
        'SAPLSUU5'  '0100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'       '=LOGO',
        ''          ''      ''   'BDC_SUBSCR'       'SAPLSZA5                                0900MAINAREA',
        ''          ''      ''   'ADDR3_DATA-FUNCTION'         WA_USUARIO-CARGO,
        ''          ''      ''   'ADDR3_DATA-DEPARTMENT'       WA_USUARIO-FILIAL,
        ''          ''      ''   'SZA5_D0700-FAX_NUMBER'       WA_USUARIO-CPF,
        ''          ''      ''   'SZA5_D0700-SMTP_ADDR'        WA_USUARIO-EMAIL,
        "''          ''      ''   'ADDR3_DATA-DEFLT_COMM'       'RML', "WA_USUARIO-COLIGADA,
        'SAPLSUU5'  '0100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'       '=LAW',
        ''          ''      ''   'BDC_SUBSCR'       'SAPLSUU5                                0101MAINAREA',
        ''          ''      ''   'USREFUS-USERALIAS' WA_USUARIO-USERREDE   ,
        ''          ''      ''   'USLOGOND-USTYP' 'A',
        "''          ''      ''   'USDEFAULTS-KOSTL' WA_USUARIO-CODCCUSTO ,
        'SAPLSUU5'  '0100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'       '=UPD',
        ''          ''      ''   'BDC_SUBSCR'       'SAPLLAW_SUU5                            0112MAINAREA',
        ''          ''      ''   'US930-USERTYP'    WA_USUARIO-TIPO ,
        'SAPLSUU5'  '0050'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'       '/EBACK',
        ''          ''      ''   'USR02-BNAME'      WA_USUARIO-USUARIOSAP.

  CLEAR WL_ERRO.
  PERFORM ZF_CALL_TRANSACTION USING 'SU01' CHANGING WL_ERRO.


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
