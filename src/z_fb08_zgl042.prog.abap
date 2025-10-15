*&---------------------------------------------------------------------*
*& Report  Z_GRAVA_ZIB_ZGL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_FB08_ZGL042.

TYPE-POOLS: PMST.

DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.

DATA: WL_MODE(1),
       VDATA(10),
       P_ERRO(1).


DATA: TI_BDCDATA          TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      WA_BDCDATA          LIKE LINE OF TI_BDCDATA ,
      T_MESSTAB           TYPE TABLE OF BDCMSGCOLL,
      WA_ZIB_CONTABIL_CHV TYPE ZIB_CONTABIL_CHV,
      WA_ZIB_CONTABIL_ERR TYPE ZIB_CONTABIL_ERR,
      IT_ZIB_CONTABIL_ERR TYPE TABLE OF ZIB_CONTABIL_ERR WITH HEADER LINE,
      WL_MESSAGE          TYPE PMST_RAW_MESSAGE,
      WG_DOCUMENTO(10).

DATA:
      TG_MSG   TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.


SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_OBJ  TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
            P_DAT  TYPE ZIB_CONTABIL-BUDAT,
            P_ST(2).
SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.
  PERFORM ORGANIZA_DADOS.

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZA_DADOS .
  " SHDB
  "WRITE / P_OBJ.

  SELECT SINGLE *
    FROM ZIB_CONTABIL_CHV
    INTO WA_ZIB_CONTABIL_CHV
    WHERE OBJ_KEY = P_OBJ.

  IF P_DAT IS INITIAL.
    SELECT SINGLE BUDAT
      FROM ZIB_CONTABIL
       INTO VDATA
        WHERE OBJ_KEY = P_OBJ.
    P_ST = '01'.
  ELSE.
    VDATA = P_DAT.
    P_ST  = '04'.
  ENDIF.

  REFRESH TI_BDCDATA.
  PERFORM F_BDC_DATA USING:
        'SAPMF05A'  '0105'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '/00',
        ''          ''      ''   'RF05A-BELNS'      WA_ZIB_CONTABIL_CHV-BELNR,
        ''          ''      ''   'BKPF-BUKRS'       WA_ZIB_CONTABIL_CHV-BUKRS,
        ''          ''      ''   'RF05A-GJAHS'      WA_ZIB_CONTABIL_CHV-GJAHR,
        ''          ''      ''   'UF05A-STGRD'      P_ST,
        ''          ''      ''   'BSIS-BUDAT'       VDATA,
        'SAPMF05A'  '0105'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '=BU'.

  CLEAR P_ERRO.

  PERFORM ZF_CALL_TRANSACTION USING 'FB08' CHANGING P_ERRO.

  IF P_ERRO NE 'X'.
    COMMIT WORK.
  ENDIF.




ENDFORM.                    " ORGANIZA_DADOS


*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
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
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*----------------------------------------------------------------------*
FORM ZF_CALL_TRANSACTION USING P_TRANS CHANGING P_ERRO.
  CONSTANTS: C_MSGID LIKE IT_MSG-MSGID VALUE 'F5',
             C_MSGNR LIKE IT_MSG-MSGNR VALUE '312',
             C_MSGNE LIKE IT_MSG-MSGNR VALUE '539'.

  DATA: WL_CONT     TYPE SY-TABIX.

  REFRESH: IT_MSG, IT_ZIB_CONTABIL_ERR.
  CLEAR IT_ZIB_CONTABIL_ERR.

  WL_MODE = 'E'.
  CALL TRANSACTION P_TRANS USING TI_BDCDATA
        MODE WL_MODE
        MESSAGES INTO IT_MSG.
  CLEAR: WL_CONT.

  LOOP AT IT_MSG WHERE MSGTYP EQ 'E'.
    ADD 1 TO WL_CONT.
  ENDLOOP.
  IF WL_CONT  GT 0.
    CLEAR WL_CONT.
    DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY  = P_OBJ.
    LOOP AT IT_MSG WHERE MSGTYP EQ 'E'.
      ADD 1 TO WL_CONT.
      CLEAR: WL_MESSAGE.
      CALL FUNCTION 'CUTC_GET_MESSAGE'
        EXPORTING
          MSG_TYPE       = IT_MSG-MSGTYP
          MSG_ID         = IT_MSG-MSGID
          MSG_NO         = SY-MSGNO
          MSG_ARG1       = SY-MSGV1
          MSG_ARG2       = SY-MSGV2
          MSG_ARG3       = SY-MSGV3
          MSG_ARG4       = SY-MSGV4
        IMPORTING
          RAW_MESSAGE    = WL_MESSAGE
        EXCEPTIONS
          MSG_NOT_FOUND  = 1
          INTERNAL_ERROR = 2
          OTHERS         = 3.

      IF ( SY-SUBRC NE 0 ).
        WL_MESSAGE = 'Erro na mensagem do BATCH-INPUT'.
      ENDIF.

      IT_ZIB_CONTABIL_ERR-OBJ_KEY            = P_OBJ.
      IT_ZIB_CONTABIL_ERR-NR_ITEM            = WL_CONT.
      IT_ZIB_CONTABIL_ERR-INTERFACE          = ''.
      IT_ZIB_CONTABIL_ERR-DT_ATUALIZACAO     = SY-DATUM.
      IT_ZIB_CONTABIL_ERR-HR_ATUALIZACAO     = SY-UZEIT.
      IT_ZIB_CONTABIL_ERR-TYPE               = IT_MSG-MSGTYP.
      IT_ZIB_CONTABIL_ERR-ID                 = IT_MSG-MSGID.
      IT_ZIB_CONTABIL_ERR-NUM                = SY-MSGNO.
      IT_ZIB_CONTABIL_ERR-MESSAGE            = WL_MESSAGE.
      IT_ZIB_CONTABIL_ERR-MESSAGE_V1         = IT_MSG-MSGV1.
      IT_ZIB_CONTABIL_ERR-MESSAGE_V2         = IT_MSG-MSGV2.
      IT_ZIB_CONTABIL_ERR-MESSAGE_V3         = IT_MSG-MSGV3.
      IT_ZIB_CONTABIL_ERR-MESSAGE_V4         = IT_MSG-MSGV4.

      APPEND IT_ZIB_CONTABIL_ERR.
      CLEAR IT_ZIB_CONTABIL_ERR.

    ENDLOOP.

    MODIFY ZIB_CONTABIL_ERR FROM TABLE IT_ZIB_CONTABIL_ERR.
  ENDIF.

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
  IF P_TRANS = 'FBRA'.
    READ TABLE IT_MSG WITH KEY MSGID = C_MSGID
                           MSGNR = C_MSGNE
                           MSGTYP = 'S'.
  ELSE.
    READ TABLE IT_MSG WITH KEY MSGID = C_MSGID
                               MSGNR = C_MSGNR
                               MSGTYP = 'S'.
  ENDIF.
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


ENDFORM.                    "ZF_CALL_TRANSACTION
