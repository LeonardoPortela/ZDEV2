*&---------------------------------------------------------------------*
*& Report  ZATUAL_DIGITO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZATUAL_DIGITO.
TYPES: BEGIN OF TY_BANCO,
         BANKL TYPE BNKA-BANKL,
         BGRUP TYPE BNKA-BGRUP,
       END  OF TY_BANCO,

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
DATA: WA_BDCDATA LIKE LINE OF TI_BDCDATA,
      WA_BANCO   TYPE TY_BANCO,
      WA_ERRO    TYPE TY_ERRO.

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
      WL_SEQ           TYPE I,
      WL_SEQ1(2),
      WL_SEQ2(5).



PARAMETER P_FILE TYPE RLGRAP-FILENAME DEFAULT 'C:\'.

DATA: T_EXCEL  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE,
      T_EXCEL2 LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.


CLEAR T_EXCEL.
REFRESH T_EXCEL.

CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  EXPORTING
    FILENAME                = P_FILE
    I_BEGIN_COL             = 1
    I_BEGIN_ROW             = 2
    I_END_COL               = 2
    I_END_ROW               = 5000
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
        WA_BANCO-BANKL         = T_EXCEL2-VALUE.
      WHEN 2.
        WA_BANCO-BGRUP         = T_EXCEL2-VALUE.

    ENDCASE.
  ENDLOOP.
  CONCATENATE 'Linha ' T_EXCEL-ROW 'Banco '  WA_BANCO-BANKL INTO VMSG SEPARATED BY SPACE.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = VMSG.

  REFRESH TI_BDCDATA.
  PERFORM F_BDC_DATA USING:
        'SAPMF02B'  '0100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '/00',
        ''          ''      ''   'BNKA-BANKS'        'BR',
        ''          ''      ''   'BNKA-BANKL'        WA_BANCO-BANKL,

        'SAPMF02B'  '0110'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=UPDA',
        ''          ''      ''   'BNKA-BGRUP'        WA_BANCO-BGRUP.

  CLEAR WL_ERRO.
  PERFORM ZF_CALL_TRANSACTION USING 'FI02' CHANGING WL_ERRO.


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
