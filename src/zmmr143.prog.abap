*&---------------------------------------------------------------------*
*& Report  ZMMR143
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR143.

TABLES: ZMMT0095.


TYPES: BEGIN OF TY_SAIDA,
         RESWK        TYPE EKPO-WERKS,
         XCF          TYPE T001W-NAME1,
         WERKS        TYPE EKPO-WERKS,
         XCR          TYPE T001W-NAME1,
         EBELN        TYPE EKBE-EBELN,
         NR_ROMANEIO  TYPE ZSDT0001-NR_ROMANEIO,
         PLACA_CAV    TYPE ZSDT0001-PLACA_CAV,
         MATNR        TYPE EKBE-MATNR,
         NFENUM       TYPE J_1BNFDOC-NFENUM,
         XDTSAIDA(10) TYPE C,
         XQTSD        TYPE STRING,
         XDIAS        TYPE STRING,
       END OF TY_SAIDA.

DATA: IT_ZMMR140  TYPE  ZDE_OUT_ZMM0141_T,
      WA_ZMMR140  TYPE  ZDE_OUT_ZMM0141,

      IT_ZMMT0095 TYPE TABLE OF ZMMT0095,
      WA_ZMMT0095 TYPE ZMMT0095,

      IT_SAIDA    TYPE TABLE OF TY_SAIDA,
      WA_SAIDA    TYPE TY_SAIDA.

DATA: LT_MAILSUBJECT     TYPE SODOCCHGI1,
      LT_MAILRECIPIENTES TYPE STANDARD TABLE OF SOMLREC90 WITH HEADER LINE,
      LT_MAILTXT         TYPE STANDARD TABLE OF SOLI WITH HEADER LINE.

INITIALIZATION.

START-OF-SELECTION.

  PERFORM SELECIONA_DADOS.

END-OF-SELECTION.

FORM SELECIONA_DADOS.

  DATA: LVA_DATA_INI TYPE EKBE-BUDAT.

  DATA:  IT_IT001 TYPE IT001,
         IT_WERKS TYPE CVP_TT_T001W.

  SELECT *
    FROM ZMMT0095 INTO TABLE IT_ZMMT0095.

  LOOP AT IT_ZMMT0095 INTO WA_ZMMT0095.

    CLEAR: IT_IT001[],
           IT_WERKS[],
           IT_ZMMR140[].

    SELECT *  FROM T001 INTO TABLE IT_IT001
    WHERE BUKRS EQ WA_ZMMT0095-BUKRS.

    IF  WA_ZMMT0095-WERKS IS NOT INITIAL.
      SELECT *  FROM T001W  INTO TABLE IT_WERKS
      WHERE WERKS EQ WA_ZMMT0095-WERKS.
    ENDIF.

    LVA_DATA_INI = SY-DATUM - 360.

    CALL FUNCTION 'ZMM_OUT_ZMM0141'
      EXPORTING
        T_T001         = IT_IT001
        T_WERKS        = IT_WERKS
        T_BUDAT_L      = LVA_DATA_INI
        T_BUDAT_H      = SY-DATUM
        I_SIT_CARGA2   = '1'
      IMPORTING
        E_OUT_ZMMM0141 = IT_ZMMR140.

"US152646
    delete IT_ZMMR140 where xdocmatet is not INITIAL.
"US152646
    IF IT_ZMMR140[] IS NOT INITIAL.

      CLEAR:  LT_MAILRECIPIENTES[],
              LT_MAILTXT[].

      IF WA_ZMMT0095-EMAIL_01 IS NOT INITIAL.
        LT_MAILRECIPIENTES-REC_TYPE = 'U'.
        LT_MAILRECIPIENTES-RECEIVER = WA_ZMMT0095-EMAIL_01.
        APPEND LT_MAILRECIPIENTES.
      ENDIF.

      IF WA_ZMMT0095-EMAIL_02 IS NOT INITIAL.
        LT_MAILRECIPIENTES-REC_TYPE = 'U'.
        LT_MAILRECIPIENTES-RECEIVER = WA_ZMMT0095-EMAIL_02.
        APPEND LT_MAILRECIPIENTES.
      ENDIF.

      IF WA_ZMMT0095-EMAIL_03 IS NOT INITIAL.
        LT_MAILRECIPIENTES-REC_TYPE = 'U'.
        LT_MAILRECIPIENTES-RECEIVER = WA_ZMMT0095-EMAIL_03.
        APPEND LT_MAILRECIPIENTES.
      ENDIF.

      IF WA_ZMMT0095-EMAIL_04 IS NOT INITIAL.
        LT_MAILRECIPIENTES-REC_TYPE = 'U'.
        LT_MAILRECIPIENTES-RECEIVER = WA_ZMMT0095-EMAIL_04.
        APPEND LT_MAILRECIPIENTES.
      ENDIF.

      IF WA_ZMMT0095-EMAIL_05 IS NOT INITIAL.
        LT_MAILRECIPIENTES-REC_TYPE = 'U'.
        LT_MAILRECIPIENTES-RECEIVER = WA_ZMMT0095-EMAIL_05.
        APPEND LT_MAILRECIPIENTES.
      ENDIF.

      PERFORM MONTAR_EMAIL.

    ENDIF.

    CLEAR: WA_ZMMT0095.

  ENDLOOP.

ENDFORM.

FORM MONTAR_EMAIL.

  LT_MAILSUBJECT-OBJ_LANGU = SY-LANGU.
  LT_MAILSUBJECT-OBJ_DESCR = 'Transferências de Cargas - Pendentes'.

  LT_MAILTXT = '<!DOCTYPE HTML><html><body><STYLE>'.
  APPEND LT_MAILTXT.

  LT_MAILTXT = 'TABLE, TH, TD { BORDER: 1PX SOLID BLACK; BORDER-COLLAPSE: COLLAPSE; }'.
  APPEND LT_MAILTXT.

  LT_MAILTXT = ' TH, TD { PADDING: 1PX; }'.
  APPEND LT_MAILTXT.

  LT_MAILTXT = ' TH { TEXT-ALIGN: LEFT; }'.
  APPEND LT_MAILTXT.

  LT_MAILTXT = ' TABLE#T01 TH { BACKGROUND-COLOR:#c9daf9; COLOR: black; } </STYLE>'.
  APPEND LT_MAILTXT.

  LT_MAILTXT = '<b>Existem cargas remetidas em transferências que ultrapassaram o tempo limite'.
  APPEND LT_MAILTXT.

  LT_MAILTXT = ' para registro na sua filial e precisam ser regularizadas com urgência.</b><br /><br>'.
  APPEND LT_MAILTXT.

  LT_MAILTXT = '<TABLE BORDER="1" STYLE="WIDTH:100%" ID="T01">'.
  APPEND LT_MAILTXT.

  CONCATENATE '<tr> <th align=center><font size="2"> Filial Origem</font></th>'
              '<th align=center><font size="2">Nome Filial Origem</font></th>' INTO LT_MAILTXT.
  APPEND LT_MAILTXT.

  CONCATENATE '<th><font size="2"> Filial Destino </font></th>'
              '<th><font size="2"> Nome Filial Destino </font></th>' INTO LT_MAILTXT.
  APPEND LT_MAILTXT.

  CONCATENATE '<th><font size="2"> Nº Pedido </font></th>'
              '<th><font size="2"> Romaneio </font></th>' INTO LT_MAILTXT.
  APPEND LT_MAILTXT.

  CONCATENATE '<th><font size="2"> Placa </font></th>'
              '<th><font size="2"> Material </font></th>' INTO LT_MAILTXT.
  APPEND LT_MAILTXT.

  CONCATENATE '<th><font size="2"> Nota Fiscal </font></th>'
              '<th><font size="2"> Data Saida  </font></th>'  INTO LT_MAILTXT.
  APPEND LT_MAILTXT.

  CONCATENATE '<th><font size="2"> Peso Saida </font></th>'
              '<th><font size="2"> Dias em Transito </font></th></tr>' INTO LT_MAILTXT.
  APPEND LT_MAILTXT.

  CLEAR IT_SAIDA[].

  SORT IT_ZMMR140 BY XDIAS DESCENDING.

  LOOP  AT IT_ZMMR140 INTO WA_ZMMR140.

    WA_SAIDA-RESWK        =  WA_ZMMR140-RESWK.
    WA_SAIDA-XCF          =  WA_ZMMR140-XCF.
    WA_SAIDA-WERKS        =  WA_ZMMR140-WERKS.
    WA_SAIDA-XCR          =  WA_ZMMR140-XCR.
    WA_SAIDA-EBELN        =  WA_ZMMR140-EBELN.
    WA_SAIDA-NR_ROMANEIO  =  WA_ZMMR140-NR_ROMANEIO.
    WA_SAIDA-PLACA_CAV    =  WA_ZMMR140-PLACA_CAV.
    WA_SAIDA-MATNR        =  WA_ZMMR140-MATNR.
    WA_SAIDA-NFENUM       =  WA_ZMMR140-NFENUM.
    WA_SAIDA-XQTSD        =  WA_ZMMR140-XQTSD.
    WA_SAIDA-XDIAS        =  WA_ZMMR140-XDIAS.

    IF WA_SAIDA-XDIAS > WA_ZMMT0095-ATAGE.

      CONCATENATE WA_ZMMR140-XDTSAIDA+6(2) '.' WA_ZMMR140-XDTSAIDA+4(2) '.' WA_ZMMR140-XDTSAIDA+0(4) INTO WA_SAIDA-XDTSAIDA.

      CONCATENATE '<tr><td><font size="2">' WA_SAIDA-RESWK '</font></td>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      CONCATENATE '<td><font size="2">' WA_SAIDA-XCF '</font></td>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      CONCATENATE '<td><font size="2">' WA_SAIDA-WERKS '</font></td>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      CONCATENATE '<td><font size="2">' WA_SAIDA-XCR '</font></td>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      CONCATENATE '<td><font size="2">' WA_SAIDA-EBELN'</font></td>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      CONCATENATE '<td><font size="2">'  WA_SAIDA-NR_ROMANEIO '</font></td>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      CONCATENATE '<td><font size="2">' WA_SAIDA-PLACA_CAV'</font></td>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      CONCATENATE '<td><font size="2">' WA_SAIDA-MATNR '</font></td>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      CONCATENATE '<td><font size="2">' WA_SAIDA-NFENUM '</font></td>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      CONCATENATE '<td><font size="2">'  WA_SAIDA-XDTSAIDA '</font></td>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      CONCATENATE '<td><font size="2">' WA_SAIDA-XQTSD '</font></td>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      CONCATENATE '<td><font size="2">'  WA_SAIDA-XDIAS '</font></td></tr>' INTO LT_MAILTXT.
      APPEND LT_MAILTXT.

      APPEND WA_SAIDA TO IT_SAIDA.

    ENDIF.

    CLEAR WA_SAIDA.
  ENDLOOP.

  LT_MAILTXT = '</table></body></html>'.
  APPEND LT_MAILTXT.

  IF IT_SAIDA[] IS NOT INITIAL.
    PERFORM ENVIAR.
  ENDIF.

ENDFORM.

FORM ENVIAR.

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = LT_MAILSUBJECT
      DOCUMENT_TYPE              = 'HTM'
    TABLES
      OBJECT_CONTENT             = LT_MAILTXT
      RECEIVERS                  = LT_MAILRECIPIENTES
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      DOCUMENT_TYPE_NOT_EXIST    = 3
      OPERATION_NO_AUTHORIZATION = 4
      PARAMETER_ERROR            = 5
      X_ERROR                    = 6
      ENQUEUE_ERROR              = 7
      OTHERS                     = 8.

  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ENDIF.

ENDFORM.
