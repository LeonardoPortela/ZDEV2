*&---------------------------------------------------------------------*
*& Report  ZATUAL_ESTRATEGIA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZATUAL_ESTRATEGIA.


TYPES: BEGIN OF TY_RM,
      KLART	 TYPE RMCLF-KLART,
      FRGGR	 TYPE RMCLF-FRGGR,
      FRGSX	 TYPE RMCBC-FRGSX,
      CLASS(10),
      ATWRT  TYPE RCTMS-ATWRT,
      ATWRT2 TYPE RCTMS-ATWRT,
   END OF TY_RM,

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
      WA_RM         TYPE TY_RM     ,
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
      WL_LINHA TYPE SY-TABIX,
      WL_FRGSX TYPE RMCBC-FRGSX.



PARAMETER: P_FILE TYPE RLGRAP-FILENAME DEFAULT 'C:\maggi\lsmwresreq.xlsx',
           "P_OPCAO LIKE BSID-UMSKZ AS CHECKBOX.
           R_ST_R  RADIOBUTTON GROUP RAD1 DEFAULT 'X',
           R_ST_P  RADIOBUTTON GROUP RAD1,
           R_ST_E  RADIOBUTTON GROUP RAD1.

DATA: T_EXCEL   LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE ,
      T_EXCEL2  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE .


CLEAR T_EXCEL.
REFRESH T_EXCEL.
IF R_ST_E = 'X'.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 3
      I_END_ROW               = 3000
    TABLES
      INTERN                  = T_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
ELSEIF R_ST_R = 'X'.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 5
      I_END_ROW               = 3000
    TABLES
      INTERN                  = T_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
ELSEIF R_ST_P = 'X'.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 6
      I_END_ROW               = 10000
    TABLES
      INTERN                  = T_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
ENDIF.
CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
  EXPORTING
    TEXT = 'Atualizando Dados'.

T_EXCEL2[] = T_EXCEL[].
SORT T_EXCEL2 BY ROW COL.
CLEAR T_EXCEL2.
WL_LINHA = 0.
CLEAR WL_FRGSX.
LOOP AT T_EXCEL.
  IF T_EXCEL-ROW = T_EXCEL2-ROW.
    CONTINUE.
  ENDIF.

  LOOP AT T_EXCEL2 WHERE ROW = T_EXCEL-ROW.
    CASE T_EXCEL2-COL.
      WHEN 1.
        WA_RM-KLART  = T_EXCEL2-VALUE.
      WHEN 2.
        WA_RM-FRGGR       = T_EXCEL2-VALUE.
      WHEN 3.
        WA_RM-FRGSX     = T_EXCEL2-VALUE.
      WHEN 4.
        WA_RM-CLASS        = T_EXCEL2-VALUE.
      WHEN 5.
        WA_RM-ATWRT       = T_EXCEL2-VALUE.
      WHEN 6.
        WA_RM-ATWRT2       = T_EXCEL2-VALUE.
    ENDCASE.
  ENDLOOP.
  IF WL_FRGSX NE WA_RM-FRGSX.
    WL_LINHA = 0.
  ENDIF.
  WL_FRGSX = WA_RM-FRGSX.
  ADD 1 TO WL_LINHA.

  CONCATENATE 'Linha ' T_EXCEL-ROW 'C.Custo '  WA_RM-ATWRT INTO VMSG SEPARATED BY SPACE.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = VMSG.

  REFRESH TI_BDCDATA.
  IF R_ST_P = 'X'.
    IF WL_LINHA = 1.
      PERFORM F_BDC_DATA USING:
         'SAPLCLFM'  '1100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '/00',
         ''          ''      ''   'RMCLF-KLART'        '032',
         'SAPLCLFM'  '1100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '/00',
         ''          ''      ''   'RMCLF-KLART'        '032',
         ''          ''      ''   'RMCLF-FRGGR'        WA_RM-FRGGR,
         ''          ''      ''   'RMCBC-FRGSX'        WA_RM-FRGSX.
      PERFORM F_BDC_DATA USING:
         'SAPLCLFM'  '1100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '/00',
         ''          ''      ''   'RMCLF-CLASS(01)'   WA_RM-CLASS.
      PERFORM F_BDC_DATA USING:
         'SAPLCLFM'  '1100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '/00',
         ''          ''      ''   'RMCLF-PAGPOS'      '1',
         ''          ''      ''   'RCTMS-MWERT(03)'   ''.

      PERFORM F_BDC_DATA USING:
        'SAPLCLFM'  '1100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=AUSW',
        ''          ''      ''   'RMCLF-PAGPOS'      '1',
        ''          ''      ''   'BDC_CURSOR'        'RCTMS-MWERT(09)'.
      PERFORM F_BDC_DATA USING:
       'SAPLCTMS'  '1900'  'X'  ''                 ' ',
       ''          ''      ''   'BDC_CURSOR'        'RCTMS-ATWRT(02)',
       ''          ''      ''   'BDC_OKCODE'        '=XEOT',
       ''          ''      ''   'RCTMS-ATWRT(02)'    WA_RM-ATWRT2.
      PERFORM F_BDC_DATA USING:
        'SAPLCLFM'  '1100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=AUSW',
        ''          ''      ''   'RMCLF-PAGPOS'      '1',
        ''          ''      ''   'BDC_CURSOR'        'RCTMS-MWERT(08)'.
      PERFORM F_BDC_DATA USING:
        'SAPLCTMS'  '1901'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_CURSOR'        'RCTMS-ATWRT(01)',
        ''          ''      ''   'BDC_OKCODE'        '=XEOT',
        ''          ''      ''   'RCTMS-ATWRT(01)'    WA_RM-ATWRT.
*    PERFORM F_BDC_DATA USING:
*      'SAPLCTMS'  '1901'  'X'  ''                 ' ',
*      ''          ''      ''   'BDC_CURSOR'        'RCTMS-ATWRT(01)',
*      ''          ''      ''   'BDC_OKCODE'        '=XEOT'.
      PERFORM F_BDC_DATA USING:
         'SAPLCLFM'  '1100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=SAVE',
         ''          ''      ''   'RMCLF-PAGPOS'      '1'.
    ELSE.
      PERFORM F_BDC_DATA USING:
             'SAPLCLFM'  '1100'  'X'  ''                 ' ',
             ''          ''      ''   'BDC_OKCODE'        '/00',
             ''          ''      ''   'RMCLF-KLART'        '032',
             'SAPLCLFM'  '1100'  'X'  ''                 ' ',
             ''          ''      ''   'BDC_OKCODE'        '/00',
             ''          ''      ''   'RMCLF-KLART'        '032',
             ''          ''      ''   'RMCLF-FRGGR'        WA_RM-FRGGR,
             ''          ''      ''   'RMCBC-FRGSX'        WA_RM-FRGSX.

      PERFORM F_BDC_DATA USING:
        'SAPLCLFM'  '1100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=AUSW',
        ''          ''      ''   'RMCLF-PAGPOS'      '1',
        ''          ''      ''   'BDC_CURSOR'        'RMCLF-CLASS(01)'.
      PERFORM F_BDC_DATA USING:
        'SAPLCLFM'  '1100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=AUSW',
        ''          ''      ''   'RMCLF-PAGPOS'      '1',
        ''          ''      ''   'BDC_CURSOR'        'RCTMS-MWERT(08)'.
      PERFORM F_BDC_DATA USING:
        'SAPLCTMS'  '1901'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_CURSOR'        'RCTMS-ATWRT(01)',
        ''          ''      ''   'BDC_OKCODE'        '=XEOT',
        ''          ''      ''   'RCTMS-ATWRT(01)'    WA_RM-ATWRT.
      PERFORM F_BDC_DATA USING:
         'SAPLCLFM'  '1100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=SAVE',
         ''          ''      ''   'RMCLF-PAGPOS'      '1'.
    ENDIF.


  ELSEIF R_ST_E = 'X'.
    PERFORM F_BDC_DATA USING:
         'SAPLCLFM'  '1100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '/00',
         ''          ''      ''   'RMCLF-KLART'        '032',
         'SAPLCLFM'  '1100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '/00',
         ''          ''      ''   'RMCLF-KLART'        '032',
         ''          ''      ''   'RMCLF-FRGGR'        WA_RM-FRGGR,
         ''          ''      ''   'RMCBC-FRGSX'        WA_RM-FRGSX.
    PERFORM F_BDC_DATA USING:
        'SAPLCLFM'  '1100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=AUSW',
        ''          ''      ''   'RMCLF-PAGPOS'      '1',
        ''          ''      ''   'BDC_CURSOR'        'RMCLF-CLASS(01)'.
    PERFORM F_BDC_DATA USING:
        'SAPLCLFM'  '1100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=AMRK',
        ''          ''      ''   'RMCLF-PAGPOS'      '1',
        ''          ''      ''   'BDC_CURSOR'        'RCTMS-MWERT(01)'.
    PERFORM F_BDC_DATA USING:
        'SAPLCLFM'  '1100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=DELE',
        ''          ''      ''   'RMCLF-PAGPOS'      '1',
        ''          ''      ''   'BDC_CURSOR'        'RCTMS-MWERT(01)'.
    PERFORM F_BDC_DATA USING:
        'SAPLCLFM'  '1100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=LOES',
        ''          ''      ''   'RMCLF-PAGPOS'      '1',
        ''          ''      ''   'BDC_CURSOR'        'RMCLF-CLASS(01)',
        ''          ''      ''   'RMCLF-KREUZ(01)'   'X'.
    PERFORM F_BDC_DATA USING:
        'SAPLSPO1'  '0100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=YES'.
    PERFORM F_BDC_DATA USING:
       'SAPLCLFM'  '1100'  'X'  ''                 ' ',
       ''          ''      ''   'BDC_OKCODE'        '=SAVE'.
  ELSEIF R_ST_R = 'X'.
    IF  WL_LINHA = 1.
      PERFORM F_BDC_DATA USING:
            'SAPLCLFM'  '1100'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'        '/00',
            ''          ''      ''   'RMCLF-KLART'        '032',
            'SAPLCLFM'  '1100'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'        '/00',
            ''          ''      ''   'RMCLF-KLART'        '032',
            ''          ''      ''   'RMCLF-FRGGR'        WA_RM-FRGGR,
            ''          ''      ''   'RMCBC-FRGSX'        WA_RM-FRGSX.
      PERFORM F_BDC_DATA USING:
         'SAPLCLFM'  '1100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '/00',
         ''          ''      ''   'RMCLF-CLASS(01)'   WA_RM-CLASS.

      PERFORM F_BDC_DATA USING:
            'SAPLCLFM'  '1100'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'        '=AUSW',
            ''          ''      ''   'RMCLF-PAGPOS'      '1',
            ''          ''      ''   'BDC_CURSOR'        'RCTMS-MWERT(01)'.
      PERFORM F_BDC_DATA USING:
           'SAPLCTMS'  '1901'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_CURSOR'        'RCTMS-ATWRT(01)',
           ''          ''      ''   'BDC_OKCODE'        '=XEOT',
           ''          ''      ''   'RCTMS-ATWRT(01)'    WA_RM-ATWRT.
      PERFORM F_BDC_DATA USING:
           'SAPLCLFM'  '1100'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'        '=SAVE',
           ''          ''      ''   'RMCLF-PAGPOS'      '1'.
    ELSE.
      PERFORM F_BDC_DATA USING:
           'SAPLCLFM'  '1100'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'        '/00',
           ''          ''      ''   'RMCLF-KLART'        '032',
           'SAPLCLFM'  '1100'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'        '/00',
           ''          ''      ''   'RMCLF-KLART'        '032',
           ''          ''      ''   'RMCLF-FRGGR'        WA_RM-FRGGR,
           ''          ''      ''   'RMCBC-FRGSX'        WA_RM-FRGSX.

      PERFORM F_BDC_DATA USING:
            'SAPLCLFM'  '1100'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'        '=AUSW',
            ''          ''      ''   'RMCLF-PAGPOS'      '1',
            ''          ''      ''   'BDC_CURSOR'        'RMCLF-CLASS(01)'.
      PERFORM F_BDC_DATA USING:
            'SAPLCLFM'  '1100'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'        '=AUSW',
            ''          ''      ''   'RMCLF-PAGPOS'      '1',
            ''          ''      ''   'BDC_CURSOR'        'RCTMS-MWERT(01)'.
      PERFORM F_BDC_DATA USING:
           'SAPLCTMS'  '1901'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_CURSOR'        'RCTMS-ATWRT(01)',
           ''          ''      ''   'BDC_OKCODE'        '=XEOT',
           ''          ''      ''   'RCTMS-ATWRT(01)'    WA_RM-ATWRT,

           'SAPLCLFM'  '1100'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'        '=SAVE',
           ''          ''      ''   'RMCLF-PAGPOS'      '1'.
    ENDIF.
  ENDIF.
  CLEAR WL_ERRO.
  PERFORM ZF_CALL_TRANSACTION USING 'CL20N' CHANGING WL_ERRO.


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
