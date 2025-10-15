*&---------------------------------------------------------------------*
*& Report  Z_MR8M
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT Z_MR8M.

TABLES RBKP.

DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.
DATA: WL_MODE(1).

DATA: TI_BDCDATA TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      T_MESSTAB  TYPE TABLE OF BDCMSGCOLL.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  WA_CONT          TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_ALV           TYPE REF TO CL_GUI_ALV_GRID,
  WA_BDCDATA       LIKE LINE OF TI_BDCDATA,
  TABIX            TYPE SY-TABIX,
  TABIX2           TYPE SY-TABIX,
  VNUM(10)         TYPE C,
  VSEQ(10)         TYPE P,
  WL_ERRO(1),
  WG_DOCUMENTO(10).

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BELNR          FOR RBKP-BELNR NO INTERVALS.
PARAMETERS    : P_GJAHR TYPE RBKP-GJAHR OBLIGATORY,
                P_STGRD TYPE UF05A-STGRD OBLIGATORY,
                P_BUDAT TYPE RBKP-BUDAT.
SELECTION-SCREEN:END OF BLOCK B1.

START-OF-SELECTION.


  PERFORM F_SHDB.
*&---------------------------------------------------------------------*
*&      Form  F_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_EXTERNO  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM F_SHDB.
  DATA: VDATA(10),
        WL_LINHA(5),
        VMSG(50),
        P_ERRO(1),
        VLINES        TYPE SY-TABIX.


  LOOP AT S_BELNR.
    WL_LINHA = SY-TABIX.

    CONCATENATE 'Linha ' WL_LINHA 'doc' S_BELNR-LOW INTO VMSG SEPARATED BY SPACE.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = VMSG.

    REFRESH TI_BDCDATA.
    CONCATENATE  P_BUDAT+6(2) P_BUDAT+4(2) P_BUDAT+0(4) INTO VDATA SEPARATED BY '.'.

    PERFORM F_BDC_DATA USING:
      'SAPLMR1M'  '0300'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'        '=CANC',
      ''          ''      ''   'RBKPV-BELNR'       S_BELNR-LOW,
      ''          ''      ''   'RBKPV-GJAHR'       P_GJAHR,
      ''          ''      ''   'UF05A-STGRD'       P_STGRD.

    IF P_BUDAT IS NOT INITIAL.
      PERFORM F_BDC_DATA USING:
        ''          ''      ''   'G_BUDAT'            VDATA.
    ENDIF.


    CLEAR P_ERRO.
    "
    PERFORM ZF_CALL_TRANSACTION USING 'MR8M' CHANGING P_ERRO.
    IF P_ERRO = 'X'.
      ROLLBACK WORK.
      WRITE / S_BELNR-LOW.
    ELSE.
      COMMIT WORK.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " F_SHDB
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2296   text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM ZF_CALL_TRANSACTION  USING P_TRANS CHANGING P_ERRO.
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
    ENDIF.
  ENDIF.

  CLEAR WG_DOCUMENTO.

  READ TABLE IT_MSG WITH KEY MSGID = C_MSGID
                             MSGNR = C_MSGNR
                             MSGTYP = 'S'.

  IF SY-SUBRC = 0.
    MOVE IT_MSG-MSGV1 TO WG_DOCUMENTO.
  ENDIF.

  IF  WG_DOCUMENTO IS INITIAL.
    P_ERRO = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_DOCUMENTO
      IMPORTING
        OUTPUT = WG_DOCUMENTO.
  ENDIF.



ENDFORM.                    " ZF_CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      -->P_DYNPRO   text
*      -->P_START    text
*      -->P_FNAM     text
*      -->P_FVAL     text
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
