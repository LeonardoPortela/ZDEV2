*--------------------------------------------------------------------------------------------------------*
*&                          AMAGGI
*--------------------------------------------------------------------------------------------------------*
*& Criado por: Anderson Oenning
*& Data      : 29/06/2017
*& Especificado: Anderson Oenning
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&             |DEVK972712  |Anderson Oe    |                                                            *
*&             |DEVK973178  |Anderson Oe    |                                                            *
*&             |DEVK973615  |Anderson Oe    |                                                            *
*&             |DEVK973621  |Anderson Oe    |                                                            *
*&             |DEVK973702  |Anderson Oe    |                                                            *
*&             |DEVK974161  |Anderson Oe    |                                                            *
*& 01/09/2017  |DEVK975728  |Anderson Oe    |                                                            *
*--------------------------------------------------------------------------------------------------------*


REPORT ZPMR0025.

TABLES: VIQMEL, T357, EQKT, EQUI, IFLO, ITOB.



TYPES: BEGIN OF TY_VIQMEL,
         BEBER TYPE VIQMEL-BEBER,
         IWERK TYPE VIQMEL-IWERK,
         QMNUM TYPE VIQMEL-QMNUM,
         QMART TYPE VIQMEL-QMART,
         QMGRP TYPE VIQMEL-QMGRP,
         QMCOD TYPE VIQMEL-QMCOD,
         DATA  TYPE VIQMEL-ERDAT,
         OBJNR TYPE VIQMEL-OBJNR,
         QMDAB TYPE VIQMEL-QMDAB,
         EQUNR TYPE VIQMEL-EQUNR,
         TPLNR TYPE  VIQMEL-TPLNR.
TYPES: END OF TY_VIQMEL.

TYPES: BEGIN OF TY_SAIDA,
         IWERK TYPE VIQMEL-IWERK,
         QMNUM TYPE VIQMEL-QMNUM,
         QMART TYPE VIQMEL-QMART,
         BEBER TYPE VIQMEL-BEBER,
         FING  TYPE T357-FING,
         TXT04 TYPE TJ02T-TXT04,
         DATA  TYPE VIQMEL-ERDAT,
         QMGRP TYPE VIQMEL-QMGRP,
         QMCOD TYPE VIQMEL-QMCOD,
         EQUNR TYPE VIQMEL-EQUNR,
         TPLNR TYPE  VIQMEL-TPLNR,
         QNEME TYPE P DECIMALS 2,   "0050 - Emergência
         QNURG TYPE P DECIMALS 2,   "0040 - Urgência
         QNOPO TYPE P DECIMALS 2,   "0030 - Oportunidade
         QNNOR TYPE P DECIMALS 2,   "0020 - Normal
         QNIND TYPE P DECIMALS 2,   "0010 - Indisponível
         TNOTA TYPE P DECIMALS 2.
TYPES:     END OF TY_SAIDA.

TYPES: BEGIN OF TY_COR,       " Dados do relatorio
         QNEME      TYPE P DECIMALS 2,
         QNURG      TYPE P DECIMALS 2,
         QNOPO      TYPE P DECIMALS 2,
         QNNOR      TYPE P DECIMALS 2,
         QNIND      TYPE P DECIMALS 2,
         TNOTA      TYPE P DECIMALS 2,
         CELL_COLOR TYPE LVC_T_SCOL. " Cor da Célula
TYPES: END OF TY_COR .

TYPES: BEGIN OF TY_T357,
         WERKS TYPE T357-WERKS,
         BEBER TYPE T357-BEBER,
         FING  TYPE T357-FING.
TYPES: END OF TY_T357.

TYPES: BEGIN OF TY_JEST,
         OBJNR TYPE JEST-OBJNR,
         STAT  TYPE JEST-STAT,
         INACT TYPE JEST-INACT.
TYPES: END OF TY_JEST.

TYPES: BEGIN OF TY_TJ02T,
         ISTAT TYPE TJ02T-ISTAT,
         TXT04 TYPE TJ02T-TXT04.
TYPES: END OF TY_TJ02T.

TYPES: BEGIN OF TY_EQUI,
         EQUNR TYPE EQUI-EQUNR.
TYPES: END OF TY_EQUI.

TYPES: BEGIN OF TY_IFLO,
         TPLNR TYPE IFLO-TPLNR,
         PLTXT TYPE IFLO-PLTXT.
TYPES: END OF TY_IFLO.

TYPES: BEGIN OF TY_EQKT,
         EQUNR TYPE EQKT-EQUNR,
         EQKTX TYPE EQKT-EQKTX.
TYPES: END OF TY_EQKT.

TYPES: BEGIN OF TY_PARAM,
         PARAM TYPE ZTPARAM-PARAM,
         ZVAL  TYPE ZTPARAM-ZVAL,
         CONST TYPE ZTPARAM-CONST,
       END OF TY_PARAM.

TYPES: BEGIN OF TY_ZVAL,
         PARAM TYPE ZTPARAM-PARAM,
         ZVAL  TYPE VIQMEL-QMART,
         CONST TYPE VIQMEL-IWERK,
       END OF TY_ZVAL.



TYPES: BEGIN OF TY_POPUP,
         BEBER    TYPE VIQMEL-BEBER,
         FING     TYPE T357-FING,
         QMNUM    TYPE VIQMEL-QMNUM,
         IWERK    TYPE VIQMEL-IWERK,
         QMART    TYPE VIQMEL-QMART,
         EQUNR    TYPE VIQMEL-EQUNR,
         TPLNR    TYPE  VIQMEL-TPLNR,
         EQKTX    TYPE EQKT-EQKTX,
         PLTXT    TYPE IFLO-PLTXT,
         QMDAB    TYPE VIQMEL-QMDAB,
         DATA(11) TYPE C.
TYPES: END OF TY_POPUP.

*DATA:BEGIN OF IT_POPUP OCCURS 10,
*       QMNUM TYPE VIQMEL-QMNUM,
*       IWERK TYPE VIQMEL-IWERK,
*       QMART TYPE VIQMEL-QMART,
*     END OF IT_POPUP.

DATA:
  IT_VIQMEL TYPE TABLE OF TY_VIQMEL WITH HEADER LINE,
  IT_SAIDA  TYPE TABLE OF TY_SAIDA,
  IT_JEST   TYPE TABLE OF TY_JEST,
  IT_TJ02T  TYPE TABLE OF TY_TJ02T,
  IT_T357   TYPE TABLE OF TY_T357 WITH HEADER LINE,
  IT_COLOR  TYPE TABLE OF LVC_S_SCOL,  " Cor para célula
  IT_RETURN TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
  IT_PARAM  TYPE TABLE OF TY_PARAM WITH HEADER LINE,
  IT_ZVAL   TYPE TABLE OF TY_ZVAL   WITH HEADER LINE,
  IT_POPUP  TYPE TABLE OF TY_POPUP,
  IT_EQUI   TYPE TABLE OF TY_EQUI WITH HEADER LINE,
  IT_IFLO   TYPE TABLE OF TY_IFLO WITH HEADER LINE,
  IT_EQKT   TYPE TABLE OF TY_EQKT WITH HEADER LINE.

DATA:
  WA_VIQMEL TYPE TY_VIQMEL,
  WA_SAIDA  TYPE TY_SAIDA,
  WA_JEST   TYPE TY_JEST,
  WA_TJ02T  TYPE TY_TJ02T,
  WA_T357   TYPE TY_T357,
  WA_COLOR  TYPE LVC_S_SCOL,  " Cor para célula
  WA_RETURN LIKE IT_RETURN,
  WA_POPUP  TYPE TY_POPUP,
  WA_EQUI   TYPE TY_EQUI,
  WA_IFLO   TYPE TY_IFLO,
  WA_EQKT   TYPE TY_EQKT.


CONSTANTS:
  L_PPREDIT(8)   VALUE 'P-PREDIT',
  L_NO_PREDIT(9) VALUE 'NO_PREDIT'.


SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETER: P_ABERT AS CHECKBOX.
SELECTION-SCREEN COMMENT 2(7) TEXT-005 FOR FIELD P_ABERT.
SELECTION-SCREEN POSITION 11.

PARAMETER: P_ENC AS CHECKBOX.
SELECTION-SCREEN COMMENT 14(10) TEXT-006 FOR FIELD P_ENC.
SELECTION-SCREEN POSITION 25.


PARAMETER: P_PEN AS CHECKBOX.
SELECTION-SCREEN COMMENT 32(15) TEXT-007 FOR FIELD P_PEN.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK B2.


SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS: P_QMART FOR VIQMEL-QMART. "Tipo de Nota.
SELECT-OPTIONS: P_IWERK FOR ITOB-SWERK NO-EXTENSION.  "Centro.
SELECT-OPTIONS: P_BEBER FOR ITOB-BEBER. "Area operacional.
SELECT-OPTIONS: P_DATA  FOR VIQMEL-ERDAT DEFAULT SY-DATUM TO SY-DATUM. "Data da nota.
SELECTION-SCREEN: END OF BLOCK B1.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_BEBER.
*  PERFORM ZF_SELECT_BEBER.

*-----------------------------------------------------------------------------*
*   Processamento
*-----------------------------------------------------------------------------*
START-OF-SELECTION.

* Seleção.

  IF P_ENC IS INITIAL AND P_PEN IS INITIAL  AND P_ABERT IS INITIAL.
    MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF P_PEN IS NOT INITIAL  AND P_ABERT IS NOT INITIAL  AND P_ENC IS NOT INITIAL.
    MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ELSEIF
    P_PEN IS NOT INITIAL  AND P_ABERT IS NOT INITIAL.
    MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF
    P_PEN IS NOT INITIAL AND P_ENC IS NOT INITIAL.
    MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF P_PEN IS NOT INITIAL.
    PERFORM SEL_NOTPEND.
    PERFORM P_DADOS.
  ELSEIF
    P_ABERT IS NOT INITIAL.
    PERFORM SEL_ABERT.
    PERFORM P_DADOS.
  ELSE.
    PERFORM SEL_NOTENC.
    PERFORM P_DADOS.
  ENDIF.


  IF IT_SAIDA IS NOT INITIAL.
    PERFORM Z_CATALOGO.

  ELSE.
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  INCLUDE ZPMR0025_0100.
*&---------------------------------------------------------------------*
*&      Seleção de notas pendentes
*&---------------------------------------------------------------------*
*       text - Selecionando notas de manutenção.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEL_NOTPEND.

  FREE: IT_ZVAL[].
  SELECT PARAM ZVAL CONST
    FROM ZTPARAM
    INTO CORRESPONDING FIELDS OF TABLE IT_PARAM
    WHERE PARAM EQ L_NO_PREDIT.

  IF NOT IT_PARAM[] IS INITIAL.
    LOOP AT IT_PARAM.
      IT_ZVAL-PARAM = IT_PARAM-PARAM.
      IT_ZVAL-ZVAL = IT_PARAM-ZVAL.
      IT_ZVAL-CONST = IT_PARAM-CONST.
      APPEND IT_ZVAL.
    ENDLOOP.
  ENDIF.

  IF IT_ZVAL[] IS NOT INITIAL.
    SELECT BEBER IWERK QMNUM QMART QMGRP QMCOD ERDAT OBJNR QMDAB  EQUNR TPLNR
      FROM VIQMEL
      INTO TABLE IT_VIQMEL
      FOR ALL ENTRIES IN IT_ZVAL
      WHERE IWERK IN P_IWERK
      AND BEBER IN P_BEBER
      AND   QMART EQ IT_ZVAL-ZVAL
      AND   IWERK EQ IT_ZVAL-CONST
      AND   QMGRP EQ 'F0000050'
      AND   QMCOD IN ('0010', '0020', '0030', '0040', '0050')
      AND   QMDAB EQ 0.
  ENDIF.

  SORT IT_VIQMEL ASCENDING BY QMNUM.

  IF  IT_VIQMEL[] IS NOT INITIAL.

    SELECT EQUNR EQKTX
      FROM EQKT
      INTO TABLE IT_EQKT
        FOR ALL ENTRIES IN IT_VIQMEL
        WHERE EQUNR EQ IT_VIQMEL-EQUNR.

    SELECT TPLNR PLTXT
      FROM IFLO
      INTO TABLE IT_IFLO
        FOR ALL ENTRIES IN IT_VIQMEL
        WHERE TPLNR EQ IT_VIQMEL-TPLNR.

    SELECT WERKS BEBER FING
      FROM T357
      INTO TABLE IT_T357
        FOR ALL ENTRIES IN IT_VIQMEL
          WHERE BEBER EQ IT_VIQMEL-BEBER
            AND WERKS EQ IT_VIQMEL-IWERK.

    SORT IT_T357 ASCENDING BY WERKS BEBER.

    SELECT OBJNR STAT INACT
      FROM JEST
      INTO TABLE IT_JEST
        FOR ALL ENTRIES IN IT_VIQMEL
        WHERE OBJNR EQ IT_VIQMEL-OBJNR
          AND  INACT NE ABAP_TRUE.

  ENDIF.

  IF  IT_JEST[] IS NOT INITIAL.
    SELECT ISTAT TXT04
      INTO TABLE IT_TJ02T
     FROM TJ02T
      FOR ALL ENTRIES IN IT_JEST
      WHERE ISTAT EQ IT_JEST-STAT
       AND TXT04 IN ('MSPN', 'MSPR')
      AND SPRAS EQ SY-LANGU.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  seleção de notas encerradas.
*&---------------------------------------------------------------------*
*       text - Selecionando notas de manutenção.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


FORM SEL_NOTENC.

  FREE: IT_ZVAL[].
  SELECT PARAM ZVAL CONST
    FROM ZTPARAM
    INTO CORRESPONDING FIELDS OF TABLE IT_PARAM
    WHERE PARAM EQ L_NO_PREDIT.
  IF NOT IT_PARAM[] IS INITIAL.
    LOOP AT IT_PARAM.
      IT_ZVAL-PARAM = IT_PARAM-PARAM.
      IT_ZVAL-ZVAL = IT_PARAM-ZVAL.
      IT_ZVAL-CONST = IT_PARAM-CONST.
      APPEND IT_ZVAL.
    ENDLOOP.
  ENDIF.

  IF IT_ZVAL[] IS NOT INITIAL.
    SELECT BEBER IWERK QMNUM QMART QMGRP QMCOD ERDAT OBJNR QMDAB  EQUNR TPLNR
     FROM VIQMEL
     INTO TABLE IT_VIQMEL
     FOR ALL ENTRIES IN IT_ZVAL
     WHERE IWERK IN P_IWERK
     AND BEBER IN P_BEBER
    AND   ERDAT IN  P_DATA
    AND   QMART EQ IT_ZVAL-ZVAL
    AND   IWERK EQ IT_ZVAL-CONST
    AND   QMGRP EQ 'F0000050'
    AND   QMCOD IN ('0010', '0020', '0030', '0040', '0050')
    AND   QMDAB NE 0.
    SORT IT_VIQMEL ASCENDING BY QMNUM.
  ENDIF.

  IF IT_VIQMEL[] IS NOT INITIAL.

    SELECT EQUNR EQKTX
     FROM EQKT
     INTO TABLE IT_EQKT
     FOR ALL ENTRIES IN IT_VIQMEL
     WHERE EQUNR EQ IT_VIQMEL-EQUNR.

    SELECT TPLNR PLTXT
      FROM IFLO
      INTO TABLE IT_IFLO
      FOR ALL ENTRIES IN IT_VIQMEL
      WHERE TPLNR EQ IT_VIQMEL-TPLNR.


    SELECT WERKS BEBER FING
     FROM T357
     INTO TABLE IT_T357
      FOR ALL ENTRIES IN IT_VIQMEL
      WHERE BEBER EQ IT_VIQMEL-BEBER
      AND WERKS EQ IT_VIQMEL-IWERK.

    SORT IT_T357 ASCENDING BY WERKS BEBER.

    SELECT OBJNR STAT INACT
      FROM JEST
      INTO TABLE IT_JEST
      FOR ALL ENTRIES IN IT_VIQMEL
      WHERE OBJNR EQ IT_VIQMEL-OBJNR
       AND  INACT NE ABAP_TRUE.
  ENDIF.

  IF IT_JEST[] IS NOT INITIAL.
    SELECT ISTAT TXT04
     FROM TJ02T
     INTO TABLE IT_TJ02T
       FOR ALL ENTRIES IN IT_JEST
       WHERE ISTAT EQ IT_JEST-STAT
        AND TXT04 IN ('MSPN', 'MSPR', 'MSEN')
       AND SPRAS EQ SY-LANGU.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  seleção de notas abetas.
*&---------------------------------------------------------------------*
*       text - Selecionando notas de manutenção.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEL_ABERT.

  FREE: IT_ZVAL[].
  SELECT PARAM ZVAL CONST
    FROM ZTPARAM
    INTO CORRESPONDING FIELDS OF TABLE IT_PARAM
    WHERE PARAM EQ L_NO_PREDIT.
  IF NOT IT_PARAM[] IS INITIAL.
    LOOP AT IT_PARAM.
      IT_ZVAL-PARAM = IT_PARAM-PARAM.
      IT_ZVAL-ZVAL = IT_PARAM-ZVAL.
      IT_ZVAL-CONST = IT_PARAM-CONST.
      APPEND IT_ZVAL.
    ENDLOOP.
  ENDIF.

  IF IT_ZVAL IS NOT INITIAL.
    SELECT BEBER IWERK QMNUM QMART QMGRP QMCOD ERDAT OBJNR QMDAB  EQUNR TPLNR
      FROM VIQMEL
      INTO TABLE IT_VIQMEL
      FOR ALL ENTRIES IN IT_ZVAL
      WHERE IWERK IN P_IWERK
       AND BEBER IN P_BEBER
       AND   ERDAT IN P_DATA
       AND   QMART EQ IT_ZVAL-ZVAL
       AND   IWERK EQ IT_ZVAL-CONST
       AND   QMGRP EQ 'F0000050'
       AND   QMCOD IN ('0010', '0020', '0030', '0040', '0050').

    SORT IT_VIQMEL ASCENDING BY QMNUM.

  ENDIF.

  IF IT_VIQMEL[] IS NOT INITIAL.

    SELECT EQUNR EQKTX
      FROM EQKT
      INTO TABLE IT_EQKT
      FOR ALL ENTRIES IN IT_VIQMEL
      WHERE EQUNR EQ IT_VIQMEL-EQUNR.

    SORT IT_EQKT ASCENDING BY EQUNR.

    SELECT TPLNR PLTXT
      FROM IFLO
      INTO TABLE IT_IFLO
      FOR ALL ENTRIES IN IT_VIQMEL
      WHERE TPLNR EQ IT_VIQMEL-TPLNR.

    SELECT WERKS BEBER FING
      FROM T357
      INTO TABLE IT_T357
       FOR ALL ENTRIES IN IT_VIQMEL
       WHERE BEBER EQ IT_VIQMEL-BEBER
         AND WERKS EQ IT_VIQMEL-IWERK.

    SORT IT_T357 ASCENDING BY WERKS BEBER.

    SELECT OBJNR STAT INACT
      FROM JEST
      INTO TABLE IT_JEST
      FOR ALL ENTRIES IN IT_VIQMEL
      WHERE OBJNR EQ IT_VIQMEL-OBJNR
        AND INACT NE ABAP_TRUE.
  ENDIF.

  IF IT_JEST IS NOT INITIAL .
    SELECT ISTAT TXT04
     FROM TJ02T
      INTO TABLE IT_TJ02T
      FOR ALL ENTRIES IN IT_JEST
      WHERE ISTAT EQ IT_JEST-STAT
      AND TXT04 IN ('MSPN', 'MSPR', 'MSEN')
      AND SPRAS EQ SY-LANGU.
  ENDIF.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  P_DADOS
*&---------------------------------------------------------------------*
*       text - Preparando as informações para saida.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM P_DADOS.

*  DATA LS_CELLCOLOR TYPE LVC_S_SCOL .

  LOOP AT IT_VIQMEL INTO WA_VIQMEL.
    WA_SAIDA-QMART = WA_VIQMEL-QMART.
    WA_SAIDA-IWERK = WA_VIQMEL-IWERK.
    WA_SAIDA-BEBER = WA_VIQMEL-BEBER.


*   Quantidade de notas Indisponivel
    IF WA_VIQMEL-QMGRP IS NOT INITIAL
     AND WA_VIQMEL-QMCOD = '0010'.
      ADD 1 TO WA_SAIDA-QNIND.
    ENDIF.

*   Quantidade de notas Normais
    IF WA_VIQMEL-QMGRP IS NOT INITIAL
      AND WA_VIQMEL-QMCOD = '0020'.
      ADD 1 TO WA_SAIDA-QNNOR.
    ENDIF.

*   Quantidade de notas opertunidades
    IF WA_VIQMEL-QMGRP IS NOT INITIAL
     AND WA_VIQMEL-QMCOD = '0030'.
      ADD 1 TO WA_SAIDA-QNOPO.
    ENDIF.

*   Quantidade de notas Urgencia.
    IF WA_VIQMEL-QMGRP IS NOT INITIAL
   AND WA_VIQMEL-QMCOD = '0040'.
      ADD 1 TO WA_SAIDA-QNURG.
    ENDIF.

*  Quantidade de notas Emergencia.
    IF WA_VIQMEL-QMGRP IS NOT INITIAL
   AND WA_VIQMEL-QMCOD = '0050'.
      ADD 1 TO WA_SAIDA-QNEME.
    ENDIF.

    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_T357 INTO WA_T357 WITH KEY BEBER = WA_VIQMEL-BEBER
                                               WERKS = WA_VIQMEL-IWERK.
      WA_SAIDA-FING  = WA_T357-FING.
    ENDIF.

*    APPEND WA_SAIDA TO IT_SAIDA.
    COLLECT WA_SAIDA INTO IT_SAIDA.

    CLEAR WA_VIQMEL.
    CLEAR WA_T357.
    CLEAR WA_SAIDA.
  ENDLOOP.

  LOOP AT IT_SAIDA INTO WA_SAIDA.
    WA_SAIDA-QMNUM = WA_VIQMEL-QMNUM.
    WA_SAIDA-TNOTA = ( WA_SAIDA-QNIND + WA_SAIDA-QNNOR + WA_SAIDA-QNOPO + WA_SAIDA-QNURG + WA_SAIDA-QNEME ).
    MODIFY IT_SAIDA FROM WA_SAIDA.
  ENDLOOP.
  SORT IT_SAIDA ASCENDING BY IWERK BEBER.

ENDFORM.

*FORM ZF_SELECT_BEBER .
**
*  CHECK NOT P_IWERK IS INITIAL.
*
*  SELECT WERKS BEBER FING INTO TABLE IT_T357
*    FROM T357
*    WHERE WERKS IN P_IWERK.
**
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
**     DDIC_STRUCTURE  = ' '
*      RETFIELD        = 'BEBER'
*      VALUE_ORG       = 'S'
*    TABLES
*      VALUE_TAB       = IT_T357
*      RETURN_TAB      = IT_RETURN
*    EXCEPTIONS
*      PARAMETER_ERROR = 1
*      NO_VALUES_FOUND = 2
*      OTHERS          = 3.
*  LOOP AT IT_RETURN INTO WA_RETURN.
*    READ TABLE IT_T357 WITH KEY BEBER = WA_RETURN-FIELDVAL.
*    IF SY-SUBRC IS INITIAL.
*      P_BEBER = IT_T357-BEBER.
*    ENDIF.
*  ENDLOOP.
*ENDFORM.

* Preparando a ALV.

FORM Z_CATALOGO.
  CALL SCREEN 100.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE

  USER_COMMAND_0200 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

MODULE STATUS_0200 OUTPUT.

  SET PF-STATUS 'T3'.
  SET TITLEBAR 'T4'.


  IF P_ENC IS INITIAL.
    P_TEXT = TEXT-011.
    PERFORM FILL_IT_FIELDCATALOG_POPUP  USING:

            01 'IWERK '      'VIQMEL '    '10'  ' '     ' '    ' '   'Centro Plan         ' ' ' ' ' ,
            03 'QMART '      'VIQMEL '    '10'  ' '     ' '    ' '   'Tipo de Nota        ' ' ' ' ' ,
            04 'QMNUM '      'VIQMEL '    '10'  ' '     ' '    ' '   'Nº Nota             ' ' ' 'X' ,
            02 'FING  '      'T357   '    '20'  ' '     ' '    ' '   'Area Operacional    ' ' ' ' ' ,
            05 'EQUNR '      'VIQMEL '    '12'  ' '     ' '    ' '   'Equipamento         ' ' ' 'X' ,
            06 'EQKTX '      'EQKT   '    '30'  ' '     ' '    ' '   'Desc Equipamento    ' ' ' ' ' ,
            07 'TPLNR '      'VIQMEL '    '20'  ' '     ' '    ' '   'Local Instalação    ' ' ' ' ' ,
            08 'PLTXT '      'IFLO   '    '25'  ' '     ' '    ' '   'Desc Local          ' ' ' ' ' ,
            09 'DATA  '      'VIQMEL '    '15'  ' '     ' '    ' '   'Data de Criação     ' ' ' ' ' .

  ELSE .
    P_TEXT = TEXT-011.
    PERFORM FILL_IT_FIELDCATALOG_POPUP  USING:

            01 'IWERK '      'VIQMEL '    '10'  ' '     ' '    ' '   'Centro Plan         ' ' ' ' ' ,
            03 'QMART '      'VIQMEL '    '10'  ' '     ' '    ' '   'Tipo de Nota        ' ' ' ' ' ,
            04 'QMNUM '      'VIQMEL '    '10'  ' '     ' '    ' '   'Nº Nota             ' ' ' 'X' ,
            02 'FING  '      'T357   '    '17'  ' '     ' '    ' '   'Area Operacional    ' ' ' ' ' ,
            05 'EQUNR '      'VIQMEL '    '12'  ' '     ' '    ' '   'Equipamento         ' ' ' 'X' ,
            06 'EQKTX '      'EQKT   '    '30'  ' '     ' '    ' '   'Desc Equipamento    ' ' ' ' ' ,
            07 'TPLNR '      'VIQMEL '    '20'  ' '     ' '    ' '   'Local Instalação    ' ' ' ' ' ,
            08 'PLTXT '      'IFLO   '    '25'  ' '     ' '    ' '   'Desc Local          ' ' ' ' ' ,
            09 'DATA  '      'VIQMEL '    '12'  ' '     ' '    ' '   'Data de Criação     ' ' ' ' ' ,
            10 'QMDAB '      'VIQMEL '    '20'  ' '     ' '    ' '   'Data de Enceramento ' ' ' ' ' .

  ENDIF.

  IF G_CONTAINER IS INITIAL.

    CREATE OBJECT G_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_2'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    CREATE OBJECT CTL_ALV_2
      EXPORTING
        I_PARENT = G_CONTAINER.

    WA_EXCLUDE_FCODE_2 = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
    APPEND WA_EXCLUDE_FCODE_2 TO IT_EXCLUDE_FCODE_2.


    CALL METHOD CTL_ALV_2->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT_2
        IS_VARIANT           = GS_VARIANT_2
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE_2
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG_2
        IT_OUTTAB            = IT_POPUP
        IT_SORT              = IT_SORT.


    SET HANDLER: LCL_EVENTOS_2=>ON_HOTSPOT_CLICK FOR CTL_ALV_2.

  ELSE.
    CALL METHOD CTL_ALV_2->REFRESH_TABLE_DISPLAY.
  ENDIF.

* ______________________________________________________________


ENDMODULE.
