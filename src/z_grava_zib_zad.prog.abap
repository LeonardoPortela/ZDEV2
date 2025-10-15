*&---------------------------------------------------------------------*
*& Report  Z_GRAVA_ZIB_ZAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_GRAVA_ZIB_ZAD.

DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.
DATA: WL_MODE(1).

TYPES:
      BEGIN OF TY_EKPO,
        EBELN      TYPE EKPO-EBELN,
        EBELP      TYPE EKPO-EBELP,
        WERKS      TYPE EKPO-WERKS,
      END OF TY_EKPO,

     BEGIN OF TY_ZFIT0046,
      NRO_SOL   TYPE ZFIT0046-NRO_SOL,
      EBELN     TYPE ZFIT0046-EBELN,
      EBELP     TYPE ZFIT0046-EBELP,
      ANLN1     TYPE ZFIT0046-ANLN1,
      ANLN2     TYPE ZFIT0046-ANLN2,
      VLR_ADIANTAMENTO TYPE ZFIT0046-VLR_ADIANTAMENTO,
   END OF TY_ZFIT0046.

DATA: TI_BDCDATA          TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      T_MESSTAB           TYPE TABLE OF BDCMSGCOLL.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
      WA_CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV    TYPE REF TO CL_GUI_ALV_GRID ,
      WA_BDCDATA    LIKE LINE OF TI_BDCDATA ,
      TABIX         TYPE SY-TABIX,
      TABIX2        TYPE SY-TABIX,
      VNUM(10)    TYPE C,
      VSEQ(10)    TYPE P,
      WL_ERRO(1),
      WG_DOCUMENTO(10).

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_SOL  TYPE ZFIT0045-NRO_SOL.

SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.


  PERFORM F_SHDB USING P_SOL CHANGING WL_ERRO.
*&---------------------------------------------------------------------*
*&      Form  F_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_EXTERNO  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM F_SHDB  USING  P_NRO_SOL  CHANGING P_ERRO.
  DATA: VDATA(10),
        VDATA_V(10),
        WL_VLR(16),
        V_XBLNR   TYPE BKPF-XBLNR,
        V_NRO_SOL(10),
        VLINES TYPE SY-TABIX,
        IT_ZFIT0046         TYPE TABLE OF TY_ZFIT0046,
        WA_ZFIT0046         TYPE TY_ZFIT0046,
        IT_EKPO             TYPE TABLE OF TY_EKPO,
        WA_EKPO             TYPE TY_EKPO,
        P_ALV    TYPE ZFIT0045.

  SELECT SINGLE *
    FROM ZFIT0045
    INTO P_ALV
    WHERE NRO_SOL EQ P_NRO_SOL.

  SELECT  NRO_SOL EBELN  EBELP ANLN1 ANLN2 VLR_ADIANTAMENTO
  FROM ZFIT0046
  INTO TABLE IT_ZFIT0046
  WHERE NRO_SOL EQ P_NRO_SOL.

  CHECK IT_ZFIT0046[] IS NOT INITIAL.

  SELECT EBELN EBELP WERKS
    FROM EKPO
    INTO TABLE IT_EKPO
    FOR ALL ENTRIES IN IT_ZFIT0046
    WHERE EBELN EQ IT_ZFIT0046-EBELN
    AND   EBELP EQ IT_ZFIT0046-EBELP ORDER BY PRIMARY KEY .


  VLINES = 0.
  LOOP AT IT_ZFIT0046 INTO WA_ZFIT0046 WHERE NRO_SOL = P_ALV-NRO_SOL
                                       AND   EBELN   = P_ALV-EBELN.
    ADD 1 TO VLINES.
  ENDLOOP.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_ALV-NRO_SOL
    IMPORTING
      OUTPUT = V_NRO_SOL.

  CONCATENATE 'SOL.' V_NRO_SOL INTO V_XBLNR.
  REFRESH TI_BDCDATA.
  CONCATENATE  SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM+0(4) INTO VDATA SEPARATED BY '.'.
  CONCATENATE  P_ALV-DT_PGTO+6(2) P_ALV-DT_PGTO+4(2) P_ALV-DT_PGTO(4) INTO VDATA_V SEPARATED BY '.'.
  TABIX2 = 0.

  PERFORM F_BDC_DATA USING:
    'SAPMF05A'  '0112'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'        '/00',
    ''          ''      ''   'BKPF-BLDAT'       VDATA,
    ''          ''      ''   'BKPF-BLART'       'KA',
    ''          ''      ''   'BKPF-BUKRS'       P_ALV-BUKRS,
    ''          ''      ''   'BKPF-BUDAT'       VDATA,
    ''          ''      ''   'BKPF-MONAT'       P_ALV-DT_PGTO+4(2),
    ''          ''      ''   'BKPF-WAERS'       P_ALV-MOEDA_PGTO,
    ''          ''      ''   'BKPF-XBLNR'       V_XBLNR,
    ''          ''      ''   'RF05A-NEWKO'      P_ALV-LIFNR,
    ''          ''      ''   'RF05A-ZUMSK'      'A'.
  LOOP AT IT_ZFIT0046 INTO WA_ZFIT0046 WHERE NRO_SOL = P_ALV-NRO_SOL
                                       AND   EBELN   = P_ALV-EBELN.
    ADD 1 TO TABIX2.
    WRITE: WA_ZFIT0046-VLR_ADIANTAMENTO                TO WL_VLR.
    TRANSLATE WL_VLR USING '. ,'.
    CONDENSE WL_VLR NO-GAPS.
    READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_ZFIT0046-EBELN
                                             EBELP = WA_ZFIT0046-EBELP BINARY SEARCH.


    PERFORM F_BDC_DATA USING:
            'SAPMF05A'  '0304'  'X'  ''                  ' ',
            ''          ''      ''   'BDC_OKCODE'        '=ZK'.

    PERFORM F_BDC_DATA USING:
    ''          ''      ''   'BSEG-WRBTR'        WL_VLR,
    ''          ''      ''   'BSEG-GSBER'        WA_EKPO-WERKS,
    ''          ''      ''   'BSEG-ZFBDT'        VDATA_V,
    ''          ''      ''   'BSEG-ZLSCH'        P_ALV-ZLSCH,
    ''          ''      ''   'BSEG-KIDNO'        P_ALV-IDENTIFICADOR,
    ''          ''      ''   'BSEG-ANLN1'        WA_ZFIT0046-ANLN1,
    ''          ''      ''   'BSEG-ANLN2'        WA_ZFIT0046-ANLN2,
    ''          ''      ''   'BSEG-EBELN'        WA_ZFIT0046-EBELN,
    ''          ''      ''   'BSEG-EBELP'        WA_ZFIT0046-EBELP,
    ''          ''      ''   'BSEG-SGTXT'        P_ALV-SGTXT.
    IF TABIX2 = VLINES.
      PERFORM F_BDC_DATA USING:
      'SAPMF05A'  '0332'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'        '/00'.
    ELSE.
      PERFORM F_BDC_DATA USING:
      'SAPMF05A'  '0332'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'        '=NP'.
    ENDIF.

    PERFORM F_BDC_DATA USING:
    ''          ''      ''   'BSEG-BVTYP'        P_ALV-BVTYP,
    ''          ''      ''   'BSEG-HBKID'        P_ALV-HBKID.
  ENDLOOP.

  PERFORM F_BDC_DATA USING:
    'SAPMF05A'  '0332'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'        '=BU'.


  CLEAR P_ERRO.
  PERFORM ZF_CALL_TRANSACTION USING 'F-47' CHANGING P_ERRO.
  IF P_ERRO = 'X'.
    ROLLBACK WORK.
  ELSE.
    UPDATE ZFIT0045 SET BELNR = WG_DOCUMENTO
                    STATUS = 'A'
                    WHERE NRO_SOL =  P_NRO_SOL.

    COMMIT WORK.
  ENDIF.

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
