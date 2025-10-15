*&---------------------------------------------------------------------*
*& Report  ZMMR147
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR147.


PARAMETERS: FP_VBELN TYPE LIKP-VBELN NO-DISPLAY,
            FP_BUDAT TYPE SY-DATLO   NO-DISPLAY DEFAULT SY-DATLO.

DATA: FP_VBTYP    TYPE LIKP-VBTYP,
      IT_MESG     TYPE TABLE OF MESG,
      LC_ES_EMKPF TYPE EMKPF,
      FP_TCODE TYPE SY-TCODE   VALUE 'VL09'.

DATA: TI_BDCDATA       TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      WA_BDCDATA       LIKE LINE OF TI_BDCDATA,
      TL_BDC           TYPE TABLE OF BDCDATA,
      WL_BDC           TYPE BDCDATA,
      OPT              TYPE CTU_PARAMS,
      WG_DOCUMENTO(10),
      WL_MODE(1),
      WL_ERRO(1).

DATA:  BEGIN OF IT_MSG OCCURS 0.
         INCLUDE STRUCTURE BDCMSGCOLL.
       DATA:  END OF IT_MSG.

START-OF-SELECTION.

  SELECT SINGLE * INTO @DATA(WA_LIKP)
    FROM LIKP
   WHERE VBELN EQ @FP_VBELN.

  IF SY-SUBRC IS NOT INITIAL.
    STOP.
  ENDIF.

  FP_VBTYP = WA_LIKP-VBTYP.

END-OF-SELECTION.

  CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE'
    EXPORTING
      I_VBELN                   = FP_VBELN
      I_BUDAT                   = FP_BUDAT
      I_TCODE                   = FP_TCODE
      I_VBTYP                   = FP_VBTYP
    IMPORTING
      ES_EMKPF                  = LC_ES_EMKPF
    TABLES
      T_MESG                    = IT_MESG
    EXCEPTIONS
      ERROR_REVERSE_GOODS_ISSUE = 1
      OTHERS                    = 2.

  IF SY-SUBRC IS NOT INITIAL OR LC_ES_EMKPF-MBLNR IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT IT_MESG INTO DATA(WA_MESG).
      SY-MSGID = WA_MESG-ARBGB.
      SY-MSGTY = WA_MESG-MSGTY.
      SY-MSGNO = WA_MESG-TXTNR.
      SY-MSGTY = WA_MESG-MSGV1.
      SY-MSGTY = WA_MESG-MSGV2.
      SY-MSGTY = WA_MESG-MSGV3.
      SY-MSGTY = WA_MESG-MSGV4.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE SY-MSGTY.
    ENDLOOP.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    "****************************************************
    REFRESH TI_BDCDATA.
    PERFORM F_BDC_DATA USING:
           'SAPMV50A'  '4104'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'       '/00',
           ''          ''      ''   'LIKP-VBELN'       FP_VBELN,

           'SAPMV50A'  '1000'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'       '/ELOES_T'.

    CLEAR WL_ERRO.
    PERFORM F_CALL_TRANSACTION USING 'VL32N'
                            CHANGING WL_ERRO.
    "***********************************************************************
    MESSAGE S625(VL) WITH FP_VBELN.
  ENDIF.

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

FORM F_CALL_TRANSACTION USING P_TRANS
                   CHANGING P_ERRO.

  CONSTANTS: C_MSGID LIKE IT_MSG-MSGID VALUE 'F5',
             C_MSGNR LIKE IT_MSG-MSGNR VALUE '312',
             C_MSGNE LIKE IT_MSG-MSGNR VALUE '539'.

  CLEAR: IT_MSG[], WG_DOCUMENTO, P_ERRO.

  WL_MODE = 'E'.

  CALL TRANSACTION P_TRANS USING TI_BDCDATA
              MODE WL_MODE
     MESSAGES INTO IT_MSG.

  READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC = 0.
    P_ERRO = 'X'.
  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION
