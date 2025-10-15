*&---------------------------------------------------------------------*
*& Report  ZATUAL_ESTRATEGIA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZATUAL_ESTRATEGIA_C.


TYPES: BEGIN OF TY_RM,
      OBJEK  TYPE AUSP-OBJEK,
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
      IT_RM         TYPE TABLE OF TY_RM,
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
      WL_LINHA(5),
      VMSG(50),
      WL_ERRO(1),

      WL_FRGSX TYPE RMCBC-FRGSX.


PARAMETER: P_BSART TYPE EKKO-BSART.
SELECT OBJEK
  INTO TABLE IT_RM
  FROM AUSP
  WHERE KLART = '032'
  AND   ATWRT = 'ZNB'.

LOOP AT IT_RM INTO WA_RM.
  WL_LINHA = SY-TABIX.
  WL_FRGSX = WA_RM-OBJEK+2(2).

  CONCATENATE 'Linha ' WL_LINHA  INTO VMSG SEPARATED BY SPACE.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = VMSG.

  REFRESH TI_BDCDATA.
  PERFORM F_BDC_DATA USING:
     'SAPLCLFM'  '1100'  'X'  ''                 ' ',
     ''          ''      ''   'BDC_OKCODE'        '/00',
     ''          ''      ''   'RMCLF-KLART'        '032'.

   PERFORM F_BDC_DATA USING:
     'SAPLCLFM'  '1100'  'X'  ''                 ' ',
     ''          ''      ''   'BDC_OKCODE'        '/00',
     ''          ''      ''   'RMCLF-KLART'        '032',
     ''          ''      ''   'RMCLF-FRGGR'        '01',
     ''          ''      ''   'RMCBC-FRGSX'        WL_FRGSX.


  PERFORM F_BDC_DATA USING:
     'SAPLCLFM'  '1100'  'X'  ''                  ' ',
     ''          ''      ''   'BDC_OKCODE'        '=AUSW',
     ''          ''      ''   'RMCLF-PAGPOS'      '1',
     ''          ''      ''   'BDC_CURSOR'      'RMCLF-CLASS(01)'.

  PERFORM F_BDC_DATA USING:
     'SAPLCLFM'  '1100'  'X'  ''                  ' ',
     ''          ''      ''   'BDC_OKCODE'        '=AUSW',
     ''          ''      ''   'RMCLF-PAGPOS'      '1',
     ''          ''      ''   'BDC_CURSOR'      'RCTMS-MNAME(01)'.

  PERFORM F_BDC_DATA USING:
     'SAPLCTMS'  '1901'  'X'  ''                  ' ',
     ''          ''      ''   'BDC_OKCODE'      '=XEOT',
     ''          ''      ''   'RCTMS-SEL01(02)' 'X',
     ''          ''      ''   'RCTMS-SEL01(03)' 'X'.


  PERFORM F_BDC_DATA USING:
     'SAPLCLFM'  '1100'  'X'  ''                 ' ',
     ''          ''      ''   'BDC_OKCODE'        '=SAVE',
     ''          ''      ''   'RMCLF-PAGPOS'      '1'.

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
