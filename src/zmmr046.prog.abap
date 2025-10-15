*&---------------------------------------------------------------------*
*& Report  ZMMR046
*&---------------------------------------------------------------------*
*&TITULO: Rotina Mensal de Atualização de MRP
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 16.05.2014
*TRANSACAO: ZMM0067
*&---------------------------------------------------------------------*

REPORT  ZMMR046.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.


*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: MARC.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

DATA: BEGIN OF IT_RETURN OCCURS 0.
        INCLUDE STRUCTURE BAPIRET2.
DATA:END OF IT_RETURN.

TYPES:
  BEGIN OF TY_MARC,
    MATNR     TYPE MARC-MATNR,
    WERKS     TYPE MARC-WERKS,
  END OF TY_MARC,

  BEGIN OF TY_EKKO,
    EBELN     TYPE EKKO-EBELN,
    AEDAT     TYPE EKKO-AEDAT,
    XDTAPROV  TYPE EKKO-AEDAT,
  END OF TY_EKKO,

  BEGIN OF TY_EKPO,
    EBELN     TYPE EKPO-EBELN,
    EBELP     TYPE EKPO-EBELP,
    MATNR     TYPE EKPO-MATNR,
    WERKS     TYPE EKPO-WERKS,
  END OF TY_EKPO,

  BEGIN OF TY_EKBE,
    EBELN     TYPE EKBE-EBELN,
    EBELP     TYPE EKBE-EBELP,
    BUDAT     TYPE EKBE-BUDAT,
    BWART     TYPE EKBE-BWART,
    LFBNR     TYPE EKBE-LFBNR,
    BELNR     TYPE EKBE-BELNR,
    DEL(1),
  END OF TY_EKBE,

  BEGIN OF TY_MAPR,
    MATNR TYPE MAPR-MATNR,
    WERKS TYPE MAPR-WERKS,
    PNUM1 TYPE MAPR-PNUM1,
   END OF TY_MAPR,

   BEGIN OF TY_PROP,
    PNUM1 TYPE PROP-PNUM1,
    PNUM2 TYPE PROP-PNUM2,
    GWERT TYPE PROP-GWERT,
    PRDAT TYPE PROP-PRDAT,
   END OF TY_PROP.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: IT_MARC       TYPE TABLE OF TY_MARC,
      IT_EKKO       TYPE TABLE OF TY_EKKO,
      IT_EKPO       TYPE TABLE OF TY_EKPO,
      IT_EKBE       TYPE TABLE OF TY_EKBE,
      IT_EKBE_102   TYPE TABLE OF TY_EKBE,
      IT_MARC_FULL  TYPE TABLE OF MARC,
      T_MAPR        TYPE TABLE OF TY_MAPR,
      T_PROP        TYPE TABLE OF TY_PROP,
      IT_MARA       TYPE TABLE OF MARA.

DATA: BAPI_HEAD       LIKE BAPIMATHEAD,
      BAPI_PLANTDATA  LIKE BAPI_MARC,
      BAPI_PLANTDATAX LIKE BAPI_MARCX,
      BAPI_MPOP       LIKE BAPI_MPOP,
      BAPI_MPOPX      LIKE BAPI_MPOPX.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_MARC       TYPE TY_MARC,
      WA_EKKO       TYPE TY_EKKO,
      WA_EKPO       TYPE TY_EKPO,
      WA_EKBE       TYPE TY_EKBE,
      WA_EKBE_102   TYPE TY_EKBE,
      WA_MARC_FULL  TYPE MARC,
      WA_MAPR       TYPE TY_MAPR,
      WA_PROP       TYPE TY_PROP,
      WA_MARA       TYPE MARA.
* ---> S4 Migration - 19/06/2023 - MA
*DATA: VMSG(50).
DATA: VMSG(70).
* <--- S4 Migration - 19/06/2023 - MA
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS    : P_BUKRS TYPE T001K-BUKRS OBLIGATORY.
SELECT-OPTIONS: P_WERKS FOR MARC-WERKS,
                P_MATNR FOR MARC-MATNR.
SELECTION-SCREEN: END OF BLOCK B1.

*---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
          F_SELECIONA_DADOS,
          F_PROCESSA.


END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  DATA: VDATA TYPE SY-DATUM.
  VDATA = SY-DATUM - 360.

  SELECT MARC~MATNR MARC~WERKS
    FROM MARC
    INNER JOIN T001K ON T001K~BWKEY = MARC~WERKS
    AND  T001K~BUKRS = P_BUKRS
    INTO TABLE IT_MARC
    WHERE MARC~DISPO NE ''
    AND   MARC~WERKS IN P_WERKS
    AND   MARC~MATNR IN P_MATNR.

  CHECK IT_MARC[] IS NOT INITIAL.

  SELECT EKPO~EBELN EKPO~EBELP EKPO~MATNR EKPO~WERKS
    FROM EKPO
    INNER JOIN EKKO ON EKKO~EBELN = EKPO~EBELN
                     AND EKKO~AEDAT	GT VDATA
                     AND EKKO~BSART	NE  'ZGR'
                     AND EKKO~BSART	NE  'NB'
    INTO TABLE IT_EKPO
    FOR ALL ENTRIES IN IT_MARC
    WHERE EKPO~EBELN  = EKKO~EBELN
    AND   EKPO~MATNR  = IT_MARC-MATNR
    AND   EKPO~WERKS  = IT_MARC-WERKS.

  IF SY-SUBRC EQ 0.
    SELECT EBELN EBELP BUDAT BWART LFBNR BELNR
      FROM EKBE
      INTO TABLE IT_EKBE
      FOR ALL ENTRIES IN IT_EKPO
      WHERE EBELN	=	IT_EKPO-EBELN
      AND   EBELP	=	IT_EKPO-EBELP
      AND   VGABE	=	1
      AND   BWART EQ 101.

    SELECT EBELN EBELP BUDAT BWART LFBNR BELNR
      FROM EKBE
      INTO TABLE IT_EKBE_102
      FOR ALL ENTRIES IN IT_EKPO
      WHERE EBELN	=	IT_EKPO-EBELN
      AND   EBELP	=	IT_EKPO-EBELP
      AND   VGABE	=	1
      AND   BWART EQ  102.

    SORT IT_EKBE BY BELNR.
    LOOP AT IT_EKBE_102 INTO WA_EKBE_102.
      READ TABLE IT_EKBE INTO WA_EKBE WITH KEY BELNR = WA_EKBE_102-LFBNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_EKBE-DEL = 'X'.
        MODIFY IT_EKBE FROM WA_EKBE INDEX SY-TABIX TRANSPORTING DEL.
      ENDIF.
    ENDLOOP.
    DELETE IT_EKBE WHERE DEL = 'X'.

    SELECT EBELN AEDAT
      FROM EKKO
      INTO TABLE IT_EKKO
      FOR ALL ENTRIES IN IT_EKPO
      WHERE EBELN EQ IT_EKPO-EBELN.
  ENDIF.

  SELECT *
    FROM MARC
    INTO TABLE IT_MARC_FULL
    FOR ALL ENTRIES IN IT_MARC
    WHERE MATNR = IT_MARC-MATNR
    AND   WERKS = IT_MARC-WERKS.

  SELECT *
    FROM MARA
    INTO TABLE IT_MARA
    FOR ALL ENTRIES IN IT_MARC
    WHERE MATNR EQ IT_MARC-MATNR.

  SELECT MATNR WERKS PNUM1
      FROM MAPR
        INTO  TABLE T_MAPR
        FOR ALL ENTRIES IN IT_MARC
          WHERE MATNR EQ IT_MARC-MATNR
            AND WERKS EQ IT_MARC-WERKS.

  IF  SY-SUBRC IS INITIAL.
    SELECT PNUM1 PNUM2 GWERT PRDAT
      FROM PROP
        INTO TABLE T_PROP
        FOR ALL ENTRIES IN T_MAPR
          WHERE PNUM1 EQ T_MAPR-PNUM1.

  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_PROCESSA .

  SORT: IT_MARA      BY MATNR,
        IT_MARC_FULL BY WERKS MATNR,
        IT_EKKO      BY EBELN,
        IT_EKPO      BY WERKS MATNR,
        IT_EKBE      BY EBELN EBELP,
        T_PROP       BY PRDAT DESCENDING,
        T_MAPR       BY MATNR WERKS.

  DATA: XDIAS     TYPE I,
        XTDIAS    TYPE I,
        XRDIAS    TYPE I,
        XOCORR    TYPE I,
        XSTK      TYPE I.


  LOOP AT IT_MARC_FULL INTO WA_MARC_FULL.
    CONCATENATE ' Centro ' WA_MARC_FULL-WERKS 'Material ' WA_MARC_FULL-MATNR INTO VMSG SEPARATED BY SPACE.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = VMSG.

    XRDIAS = 0.
    XTDIAS = 0.
    XOCORR = 0.
    LOOP AT IT_EKPO INTO WA_EKPO WHERE WERKS = WA_MARC_FULL-WERKS
                                 AND   MATNR = WA_MARC_FULL-MATNR.
      READ TABLE IT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_EKPO-EBELN BINARY SEARCH.
      WA_EKKO-XDTAPROV = WA_EKKO-AEDAT + 3.
      LOOP AT IT_EKBE INTO WA_EKBE WHERE EBELN = WA_EKPO-EBELN
                                   AND   EBELP = WA_EKPO-EBELP.
        IF WA_EKBE-BUDAT GT WA_EKKO-XDTAPROV.
          XDIAS = WA_EKBE-BUDAT - WA_EKKO-XDTAPROV.
          ADD XDIAS TO XTDIAS.
          ADD 1     TO XOCORR.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    IF XOCORR GT 0.
      XRDIAS = XTDIAS / XOCORR.
    ENDIF.

    READ TABLE IT_MARA INTO WA_MARA WITH KEY MATNR = WA_MARC_FULL-MATNR BINARY SEARCH.

* > 05/07/2023 - Migração S4 - LM
*    BAPI_HEAD-MATERIAL      = WA_MARA-MATNR.

    IF STRLEN( WA_MARA-MATNR ) > 18.
      BAPI_HEAD-MATERIAL_LONG = WA_MARA-MATNR.
    ELSE.
      BAPI_HEAD-MATERIAL      = WA_MARA-MATNR.
    ENDIF.
* > 05/07/2023 - Migração S4 - LM

    BAPI_HEAD-IND_SECTOR    = WA_MARA-MBRSH.
    BAPI_HEAD-MATL_TYPE     = WA_MARA-MTART.
    BAPI_HEAD-MRP_VIEW      = 'X'.
    BAPI_HEAD-FORECAST_VIEW = 'X'.


    BAPI_PLANTDATA-PLANT = WA_MARC_FULL-WERKS.
    BAPI_PLANTDATA-PUR_GROUP = WA_MARC_FULL-EKGRP.

    BAPI_PLANTDATAX-PLANT = WA_MARC_FULL-WERKS.
    BAPI_PLANTDATAX-PUR_GROUP = WA_MARC_FULL-EKGRP.


    BAPI_PLANTDATA-REORDER_PT = ''.
    BAPI_PLANTDATA-SAFETY_STK = ''.
    BAPI_PLANTDATA-PLND_DELRY = XRDIAS.

    BAPI_PLANTDATAX-REORDER_PT = 'X'.
    BAPI_PLANTDATAX-SAFETY_STK = 'X'.
    BAPI_PLANTDATAX-PLND_DELRY = 'X'.

    READ TABLE T_MAPR INTO WA_MAPR
       WITH KEY MATNR = WA_MARC_FULL-MATNR
                WERKS = WA_MARC_FULL-WERKS
                BINARY SEARCH.


    IF SY-SUBRC IS INITIAL.
      READ TABLE T_PROP INTO WA_PROP
        WITH KEY PNUM1 = WA_MAPR-PNUM1.
      IF SY-SUBRC IS INITIAL.
        XSTK = WA_PROP-GWERT.
        BAPI_PLANTDATA-SAFETY_STK  = XSTK.
      ENDIF.
    ENDIF.

    CLEAR IT_RETURN.
    REFRESH IT_RETURN.

    BAPI_MPOP-PLANT  = WA_MARC_FULL-WERKS.
    BAPI_MPOPX-PLANT = WA_MARC_FULL-WERKS.

    BAPI_MPOP-INITIALIZE  = 'X'.
    BAPI_MPOPX-INITIALIZE  = 'X'.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        HEADDATA            = BAPI_HEAD
        PLANTDATA           = BAPI_PLANTDATA
        PLANTDATAX          = BAPI_PLANTDATAX
        FORECASTPARAMETERS  = BAPI_MPOP    " Forecast Parameters
        FORECASTPARAMETERSX = BAPI_MPOPX "Information on update for FORECASTDATA
      IMPORTING
        RETURN              = IT_RETURN.


*    READ TABLE IT_RETURN WITH KEY TYPE = 'S'.
*
*    IF SY-SUBRC EQ 0.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*    ELSE.
*
*    ENDIF.

  ENDLOOP.

  MESSAGE 'Fim de atualização' TYPE 'I'.
ENDFORM.                    " F_PROCESSA
