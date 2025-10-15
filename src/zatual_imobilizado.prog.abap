*&---------------------------------------------------------------------*
*& Report  ZATUAL_USUARIO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZATUAL_IMOBILIZADO.

TYPES: BEGIN OF TY_IMOBIL,
      ANLN1	    TYPE ANLA-ANLN1,
      ANLN2	    TYPE ANLA-ANLN2,
      BUKRS	    TYPE ANLA-BUKRS,
      AKTIV(10),
      BZDAT(10),
      BWASL     TYPE RALT_DYNP_STRUC-BWASL,
     	ANBTR01(16),
     	ANBTR03(16),
     	ANBTR04(16),
     	ANBTR05(16),
     	ANBTR06(16),
     	ANBTR07(16),
     	ANBTR08(16),
     	ANBTR09(16),
     	ANBTR10(16),
     	ANBTR11(16),
     	ANBTR12(16),

   END OF TY_IMOBIL,

   BEGIN OF TY_NUMERO,
      ANLN1	    TYPE ANLA-ANLN1,
      ANLN2	    TYPE ANLA-ANLN2,
      BUKRS	    TYPE ANLA-BUKRS,
      SEQ       TYPE I,
   END OF TY_NUMERO,

  BEGIN OF TY_ERRO,
      MSG(100),
  END OF TY_ERRO.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: TI_BDCDATA          TYPE STANDARD TABLE OF BDCDATA,   "Guarda o mapeamento
      IT_ERRO             TYPE TABLE OF TY_ERRO,
      IT_NUMERO           TYPE TABLE OF TY_NUMERO,
      IT_ANEP             TYPE TABLE OF ANEP.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_BDCDATA    LIKE LINE OF TI_BDCDATA ,
      WA_IMOBIL    TYPE TY_IMOBIL,
      WA_ERRO       TYPE TY_ERRO,
      WA_NUMERO     TYPE TY_NUMERO,
      WA_ANEP       TYPE ANEP,
      VDATA1(10),
      VDATA2(10).

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
      VANBTR09  TYPE RALT_DYNP_STRUC-ANBTR09,
      VGJAHR    TYPE ANEP-GJAHR,
      WL_VLR(16),
      VDATA TYPE SY-DATUM.

PARAMETER P_FILE TYPE RLGRAP-FILENAME DEFAULT 'C:\maggi\upload_area_41.xlsx'.

DATA: T_EXCEL   LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE ,
      T_EXCEL2  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE .


CLEAR T_EXCEL.
REFRESH T_EXCEL.

CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  EXPORTING
    FILENAME                = P_FILE
    I_BEGIN_COL             = 1
    I_BEGIN_ROW             = 2
    I_END_COL               = 17
    I_END_ROW               = 5400
  TABLES
    INTERN                  = T_EXCEL
  EXCEPTIONS
    INCONSISTENT_PARAMETERS = 1
    UPLOAD_OLE              = 2
    OTHERS                  = 3.

CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
  EXPORTING
    TEXT = 'Atualizando Dados'.

VANBTR09 = 0.
WRITE: VANBTR09 TO WL_VLR.
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
        WA_IMOBIL-ANLN1	      = T_EXCEL2-VALUE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_IMOBIL-ANLN1
          IMPORTING
            OUTPUT = WA_IMOBIL-ANLN1.
      WHEN 2.
        WA_IMOBIL-ANLN2	     = T_EXCEL2-VALUE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_IMOBIL-ANLN2
          IMPORTING
            OUTPUT = WA_IMOBIL-ANLN2.
      WHEN 3.
        WA_IMOBIL-BUKRS      = T_EXCEL2-VALUE.
      WHEN 4.
        WA_IMOBIL-AKTIV = T_EXCEL2-VALUE.
        "CONCATENATE T_EXCEL2-VALUE+6(4) T_EXCEL2-VALUE+3(2) T_EXCEL2-VALUE+0(2) INTO  WA_IMOBIL-AKTIV.
      WHEN 5.
        WA_IMOBIL-BZDAT = T_EXCEL2-VALUE.
        "CONCATENATE T_EXCEL2-VALUE+6(4) T_EXCEL2-VALUE+3(2) T_EXCEL2-VALUE+0(2) INTO  WA_IMOBIL-BZDAT.
        VGJAHR = WA_IMOBIL-BZDAT+0(4).
      WHEN 6.
        WA_IMOBIL-BWASL      = T_EXCEL2-VALUE.
      WHEN 7.
        WA_IMOBIL-ANBTR01    = T_EXCEL2-VALUE.
      WHEN 8.
        WA_IMOBIL-ANBTR03    = T_EXCEL2-VALUE.
      WHEN 9.
        WA_IMOBIL-ANBTR04    = T_EXCEL2-VALUE.
      WHEN 10.
        WA_IMOBIL-ANBTR05    = T_EXCEL2-VALUE.
      WHEN 11.
        WA_IMOBIL-ANBTR06    = T_EXCEL2-VALUE.
      WHEN 12.
        WA_IMOBIL-ANBTR07    = T_EXCEL2-VALUE.
      WHEN 13.
        WA_IMOBIL-ANBTR08    = T_EXCEL2-VALUE.
      WHEN 14.
        WA_IMOBIL-ANBTR09    = T_EXCEL2-VALUE.
      WHEN 15.
        WA_IMOBIL-ANBTR10    = T_EXCEL2-VALUE.
      WHEN 16.
        WA_IMOBIL-ANBTR11    = T_EXCEL2-VALUE.
      WHEN 17.
        WA_IMOBIL-ANBTR12    = T_EXCEL2-VALUE.
    ENDCASE.
  ENDLOOP.
  WA_NUMERO-ANLN1 = WA_IMOBIL-ANLN1.
  WA_NUMERO-ANLN2 = WA_IMOBIL-ANLN2.
  WA_NUMERO-BUKRS = WA_IMOBIL-BUKRS.
  APPEND WA_NUMERO TO IT_NUMERO.
  CLEAR WA_NUMERO.
ENDLOOP.

"VGJAHR = "'2013'.
SORT IT_NUMERO BY  ANLN1 ANLN2 BUKRS.
DELETE ADJACENT DUPLICATES FROM       IT_NUMERO COMPARING ANLN1 ANLN2 BUKRS.
SELECT *
  FROM ANEP
  INTO TABLE IT_ANEP
  FOR ALL ENTRIES IN IT_NUMERO
  WHERE ANLN1 = IT_NUMERO-ANLN1
  AND   ANLN2 = IT_NUMERO-ANLN2
  AND   BUKRS = IT_NUMERO-BUKRS
  AND   GJAHR = VGJAHR.

DATA: TABIX TYPE SY-TABIX,
      VSEQ  TYPE I,
      CSEQ(2),
      VCAMPO1(30),
      VCAMPO2(30),
      VCAMPO3(30),
      VCAMPO4(30),
      VCAMPO5(30),
      VCAMPO6(30),
      VCAMPO7(30),
      VCAMPO8(30),
      VCAMPO9(30),
      VCAMP10(30),
      VCAMP11(30),
      VCAMP12(30),
      VCAMP13(30).

SORT: IT_ANEP BY ANLN1 ANLN2 BUKRS LNRAN,
      IT_NUMERO BY ANLN1 ANLN2 BUKRS.
DELETE ADJACENT DUPLICATES FROM IT_ANEP COMPARING ANLN1 ANLN2 BUKRS LNRAN.
LOOP AT IT_NUMERO INTO WA_NUMERO.
  TABIX = SY-TABIX.
  VSEQ  = 0.
  LOOP AT IT_ANEP INTO WA_ANEP WHERE ANLN1 = WA_NUMERO-ANLN1
                               AND   ANLN2 = WA_NUMERO-ANLN2
                               AND   BUKRS = WA_NUMERO-BUKRS.
    ADD 1 TO VSEQ .
  ENDLOOP.
  ADD 1 TO VSEQ.
  WA_NUMERO-SEQ = VSEQ.
  MODIFY IT_NUMERO FROM WA_NUMERO INDEX TABIX TRANSPORTING SEQ.
ENDLOOP.

SORT T_EXCEL2 BY ROW COL.
CLEAR T_EXCEL2.
LOOP AT T_EXCEL.
  IF T_EXCEL-ROW = T_EXCEL2-ROW.
    CONTINUE.
  ENDIF.
  LOOP AT T_EXCEL2 WHERE ROW = T_EXCEL-ROW.
    CASE T_EXCEL2-COL.
      WHEN 1.
        WA_IMOBIL-ANLN1	      = T_EXCEL2-VALUE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_IMOBIL-ANLN1
          IMPORTING
            OUTPUT = WA_IMOBIL-ANLN1.
      WHEN 2.
        WA_IMOBIL-ANLN2	     = T_EXCEL2-VALUE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_IMOBIL-ANLN2
          IMPORTING
            OUTPUT = WA_IMOBIL-ANLN2.
      WHEN 3.
        WA_IMOBIL-BUKRS      = T_EXCEL2-VALUE.
      WHEN 4.
        WA_IMOBIL-AKTIV = T_EXCEL2-VALUE.
        "CONCATENATE T_EXCEL2-VALUE+6(4) T_EXCEL2-VALUE+3(2) T_EXCEL2-VALUE+0(2) INTO  WA_IMOBIL-AKTIV.
      WHEN 5.
        WA_IMOBIL-BZDAT = T_EXCEL2-VALUE.
        "CONCATENATE T_EXCEL2-VALUE+6(4) T_EXCEL2-VALUE+3(2) T_EXCEL2-VALUE+0(2) INTO  WA_IMOBIL-BZDAT.
      WHEN 6.
        WA_IMOBIL-BWASL      = T_EXCEL2-VALUE.
      WHEN 7.
        WA_IMOBIL-ANBTR01    = T_EXCEL2-VALUE.
      WHEN 8.
        WA_IMOBIL-ANBTR03    = T_EXCEL2-VALUE.
      WHEN 9.
        WA_IMOBIL-ANBTR04    = T_EXCEL2-VALUE.
      WHEN 10.
        WA_IMOBIL-ANBTR05    = T_EXCEL2-VALUE.
      WHEN 11.
        WA_IMOBIL-ANBTR06    = T_EXCEL2-VALUE.
      WHEN 12.
        WA_IMOBIL-ANBTR07    = T_EXCEL2-VALUE.
      WHEN 13.
        WA_IMOBIL-ANBTR08    = T_EXCEL2-VALUE.
      WHEN 14.
        WA_IMOBIL-ANBTR09    = T_EXCEL2-VALUE.
      WHEN 15.
        WA_IMOBIL-ANBTR10    = T_EXCEL2-VALUE.
      WHEN 16.
        WA_IMOBIL-ANBTR11    = T_EXCEL2-VALUE.
      WHEN 17.
        WA_IMOBIL-ANBTR12    = T_EXCEL2-VALUE.
    ENDCASE.
  ENDLOOP.
  CONCATENATE 'Linha ' T_EXCEL-ROW 'IMOBIL'  WA_IMOBIL-ANLN1 INTO VMSG SEPARATED BY SPACE.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = VMSG.

  READ TABLE IT_NUMERO INTO WA_NUMERO WITH KEY ANLN1 = WA_IMOBIL-ANLN1
                                               ANLN2 = WA_IMOBIL-ANLN2
                                               BUKRS = WA_IMOBIL-BUKRS BINARY SEARCH.
  IF SY-SUBRC = 0.
    CSEQ = WA_NUMERO-SEQ.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = CSEQ
      IMPORTING
        OUTPUT = CSEQ.
  ELSE.
    CSEQ = '01'.
  ENDIF.

  CONCATENATE 'RALT_DYNP_STRUC-BZDAT(' CSEQ ')' INTO VCAMPO1.
  CONCATENATE 'RALT_DYNP_STRUC-BWASL(' CSEQ ')' INTO VCAMPO2.
  CONCATENATE 'RALT_DYNP_STRUC-ANBTR01(' CSEQ ')' INTO VCAMPO3.
  CONCATENATE 'RALT_DYNP_STRUC-ANBTR03(' CSEQ ')' INTO VCAMPO4.
  CONCATENATE 'RALT_DYNP_STRUC-ANBTR04(' CSEQ ')' INTO VCAMPO5.
  CONCATENATE 'RALT_DYNP_STRUC-ANBTR05(' CSEQ ')' INTO VCAMPO6.
  CONCATENATE 'RALT_DYNP_STRUC-ANBTR06(' CSEQ ')' INTO VCAMPO7.
  CONCATENATE 'RALT_DYNP_STRUC-ANBTR07(' CSEQ ')' INTO VCAMPO8.
  CONCATENATE 'RALT_DYNP_STRUC-ANBTR08(' CSEQ ')' INTO VCAMPO9.
  CONCATENATE 'RALT_DYNP_STRUC-ANBTR09(' CSEQ ')' INTO VCAMP10.
  CONCATENATE 'RALT_DYNP_STRUC-ANBTR10(' CSEQ ')' INTO VCAMP11.
  CONCATENATE 'RALT_DYNP_STRUC-ANBTR11(' CSEQ ')' INTO VCAMP12.
  CONCATENATE 'RALT_DYNP_STRUC-ANBTR12(' CSEQ ')' INTO VCAMP13.

  REFRESH TI_BDCDATA.
  PERFORM F_BDC_DATA USING:
        'SAPLAIST'  '0100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '/00',
        ''          ''      ''   'ANLA-ANLN1'      WA_IMOBIL-ANLN1,
        ''          ''      ''   'ANLA-ANLN2'      WA_IMOBIL-ANLN2,
        ''          ''      ''   'ANLA-BUKRS'      WA_IMOBIL-BUKRS,
        'SAPLAIST'  '1000'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=ALTD',
        ''          ''      ''   'ANLA-AKTIV'        WA_IMOBIL-AKTIV,
        'SAPLALTD'  '1100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=ALTB',
        'SAPLALTD'  '1200'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'        '=BUCH',
        ''          ''      ''   'BDC_CURSOR'        VCAMPO3,
        ''          ''      ''   VCAMPO1                          WA_IMOBIL-BZDAT,
        ''          ''      ''   VCAMPO2                          WA_IMOBIL-BWASL,
        ''          ''      ''   VCAMPO3                          WA_IMOBIL-ANBTR01,
        ''          ''      ''   VCAMPO4                          WA_IMOBIL-ANBTR03,
        ''          ''      ''   VCAMPO5                          WA_IMOBIL-ANBTR04,
        ''          ''      ''   VCAMPO6                          WA_IMOBIL-ANBTR05,
        ''          ''      ''   VCAMPO7                          WA_IMOBIL-ANBTR06,
        ''          ''      ''   VCAMPO8                          WA_IMOBIL-ANBTR07,
        ''          ''      ''   VCAMPO9                          WA_IMOBIL-ANBTR08,
        ''          ''      ''   VCAMP10                          WA_IMOBIL-ANBTR09,
        ''          ''      ''   VCAMP11                          WA_IMOBIL-ANBTR10,
        ''          ''      ''   VCAMP12                          WA_IMOBIL-ANBTR11,
        ''          ''      ''   VCAMP13                          WA_IMOBIL-ANBTR12.

  CLEAR WL_ERRO.
  PERFORM ZF_CALL_TRANSACTION USING 'AS92' CHANGING WL_ERRO.


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
