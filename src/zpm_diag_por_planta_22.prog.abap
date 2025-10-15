*-----------------------------------------------------------------------------*
*&                 AMAGGI - Projeto Abaco
*-----------------------------------------------------------------------------*
*& Criado por:  José Godoy ( JAP ) - Ábaco Consultores
*& Data      : 08/06/2017
*& Pedido por: Cleudo Ferreira
*& Chamado/Descrição :xxxx Porto Itacoatiara
*& Request: DEVK971787 PM - 08.06.2017 - ALV Rel.Diagn.p/Planta [  ] JAP
*-----------------------------------------------------------------------------*
*& Histórico de Alterações:                                                   *
*-----------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                       *
*&----------------------------------------------------------------------------*
*&             |            |               |                                 *
*-----------------------------------------------------------------------------*
REPORT ZPM_DIAG_POR_PLANTA_22.
*
TABLES: VIQMEL.
*
*-----------------------------------------------------------------------------*
*-> Tipos internos
*-----------------------------------------------------------------------------*
*
TYPES: BEGIN OF Y_PARAM,
         ZVAL  TYPE ZTPARAM-ZVAL,
         CONST TYPE ZTPARAM-CONST,
       END OF Y_PARAM.
*
TYPES: BEGIN OF Y_VIQMEL,
         QMNUM TYPE VIQMEL-QMNUM,
         IWERK TYPE VIQMEL-IWERK,
         ILOAN TYPE VIQMEL-ILOAN,
         EQUNR TYPE VIQMEL-EQUNR,
         QMART TYPE VIQMEL-QMART,
         BEBER TYPE VIQMEL-BEBER,
         KOSTL TYPE VIQMEL-KOSTL,
       END OF Y_VIQMEL.
*
TYPES: BEGIN OF Y_ZVAL,
         ZVAL TYPE VIQMEL-QMART,
       END OF Y_ZVAL.
*
TYPES: BEGIN OF Y_QPCT,
         CODE     TYPE QPCT-CODE,
         KURZTEXT TYPE QPCT-KURZTEXT,
       END OF Y_QPCT.
*
TYPES: BEGIN OF Y_T357,
         WERKS TYPE T357-WERKS,
         BEBER TYPE T357-BEBER,
         FING  TYPE T357-FING,
       END OF Y_T357.
*
TYPES: BEGIN OF Y_VIQMFE,
         QMNUM TYPE VIQMFE-QMNUM,
         FECOD TYPE VIQMFE-FECOD,
       END OF Y_VIQMFE.
*
DATA:
  BEGIN OF ITAB_OPTIONS OCCURS 0,
    OPTION(20),
  END OF ITAB_OPTIONS.
*
*-----------------------------------------------------------------------------*
** DECLARAÇÃO DE TABELAS alv                                                  **
*-----------------------------------------------------------------------------*
DATA:
  T_LAYOUT     TYPE SLIS_LAYOUT_ALV     OCCURS 0 WITH HEADER LINE,
  T_LISTHEADER TYPE SLIS_T_LISTHEADER,
  T_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.
*-----------------------------------------------------------------------------*
*-> Tabelas internas
*-----------------------------------------------------------------------------*
DATA: IT_REG    TYPE TABLE OF ZSPM_DIAG_POR_PLANTA WITH HEADER LINE,
      IT_VIQMEL TYPE TABLE OF Y_VIQMEL WITH HEADER LINE,
      IT_VIQMFE TYPE TABLE OF Y_VIQMFE WITH HEADER LINE,
      IT_PARAM  TYPE TABLE OF Y_PARAM  WITH HEADER LINE,
      IT_QPCT   TYPE TABLE OF Y_QPCT  WITH HEADER LINE,
      IT_T357   TYPE TABLE OF Y_T357  WITH HEADER LINE,
      IT_RETURN TYPE TABLE OF DDSHRETVAL,
      WA_RETURN LIKE LINE OF  IT_RETURN,
      IT_ZVAL   TYPE TABLE OF Y_ZVAL   WITH HEADER LINE.

*-----------------------------------------------------------------------------*
** DECLARAÇÃO DE VARIÁVEIS                                                    **
*-----------------------------------------------------------------------------*
DATA: LV_CONTNF(4) TYPE P,
      LV_COL(2)    TYPE C,
      LV_ROW(2)    TYPE C,
      LV_TABIX     TYPE SY-TABIX.

DATA :
  T_OUT_1   TYPE TABLE OF ZSPM_DIAG_POR_PLANTA,
  T_FCAT_1  TYPE TABLE OF LVC_S_FCAT,
  W_FCAT_1  TYPE LVC_S_FCAT,
  T_OUT_2   TYPE TABLE OF ZSPM_DIAG_POR_PLANTA,
  W_OUT_2   TYPE ZSPM_DIAG_POR_PLANTA,
  T_FCAT_2  TYPE TABLE OF LVC_S_FCAT,
  W_FCAT_2  TYPE LVC_S_FCAT,
*
  ST_LAYOUT TYPE LVC_S_LAYO.

CONSTANTS: LC_U(1)         VALUE 'U',
           LC_1(1)         VALUE '1',
           LC_NOPS(4)      VALUE 'NOPS',
           LC_QTDDIAG(7)   VALUE 'QTDDIAG',
           LC_QTDPERC(7)   VALUE 'QTDPERC',
           LC_PPREDIT(8)   VALUE 'P-PREDIT',
           LC_NO_PREDIT(9) VALUE 'NO_PREDIT'.

* Objetos
*&---------------------------------------------------------------------*
DATA :
  O_DOCKING  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
  O_SPLITTER TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
DATA :
  O_CONTAINER_1 TYPE REF TO CL_GUI_CONTAINER,
  O_CONTAINER_2 TYPE REF TO CL_GUI_CONTAINER,
  O_ALV_1       TYPE REF TO CL_GUI_ALV_GRID,
  O_ALV_2       TYPE REF TO CL_GUI_ALV_GRID.

*-----------------------------------------------------------------------------*
*  Tela de seleção
*-----------------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_IWERK FOR VIQMEL-IWERK NO-EXTENSION.  " CENTRO PLANEJAMENTO”
PARAMETER       P_BEBER TYPE VIQMEL-BEBER.  " CENTRO PLANEJAMENTO”
SELECT-OPTIONS: S_KOSTL FOR VIQMEL-KOSTL, "CENTRO DE CUSTO”
                S_ERDAT FOR VIQMEL-ERDAT NO-EXTENSION OBLIGATORY DEFAULT SY-DATUM TO SY-DATUM.
SELECTION-SCREEN:END OF BLOCK B1.
*
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_BEBER.
  PERFORM ZF_SELECT_BEBER.
*-----------------------------------------------------------------------------*
*   Processamento
*-----------------------------------------------------------------------------*
START-OF-SELECTION.
* Seleção
  PERFORM ZF_SELECIONA.
  PERFORM ZF_EXIBE_ALV.
*-----------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_SELECIONA .
*
  FREE: IT_ZVAL[], IT_REG[].
  SELECT ZVAL CONST
    FROM ZTPARAM
    INTO CORRESPONDING FIELDS OF TABLE IT_PARAM
    WHERE PARAM EQ LC_NO_PREDIT.
  IF NOT IT_PARAM[] IS INITIAL.
    LOOP AT IT_PARAM.
      IT_ZVAL-ZVAL = IT_PARAM-ZVAL.
      APPEND IT_ZVAL.
    ENDLOOP.
  ENDIF.
  DELETE ADJACENT DUPLICATES FROM IT_ZVAL.
*
  IF P_BEBER IS INITIAL.
    SELECT QMNUM IWERK ILOAN EQUNR QMART BEBER KOSTL INTO TABLE IT_VIQMEL
      FROM VIQMEL FOR ALL ENTRIES IN IT_ZVAL
      WHERE IWERK IN S_IWERK
        AND ERDAT IN S_ERDAT
        AND KOSTL IN S_KOSTL
        AND QMART EQ IT_ZVAL-ZVAL.
    SORT IT_VIQMEL ASCENDING BY QMNUM.
  ELSE.
    SELECT QMNUM IWERK ILOAN EQUNR QMART BEBER KOSTL INTO TABLE IT_VIQMEL
      FROM VIQMEL FOR ALL ENTRIES IN IT_ZVAL
      WHERE IWERK IN S_IWERK
        AND ERDAT IN S_ERDAT
        AND BEBER EQ P_BEBER
        AND KOSTL IN S_KOSTL
        AND QMART EQ IT_ZVAL-ZVAL.
    SORT IT_VIQMEL ASCENDING BY QMNUM.
  ENDIF.
*
  SELECT QMNUM FECOD INTO TABLE IT_VIQMFE
    FROM VIQMFE FOR ALL ENTRIES IN IT_VIQMEL
    WHERE QMNUM EQ IT_VIQMEL-QMNUM.
  SORT IT_VIQMFE ASCENDING BY QMNUM.
*
  SELECT CODE KURZTEXT INTO TABLE IT_QPCT
    FROM QPCT FOR ALL ENTRIES IN IT_VIQMFE
    WHERE KATALOGART EQ LC_U
      AND CODEGRUPPE EQ LC_PPREDIT
      AND CODE EQ IT_VIQMFE-FECOD.
  SORT IT_QPCT ASCENDING BY CODE.
*
  SELECT WERKS BEBER FING INTO TABLE IT_T357
    FROM T357 FOR ALL ENTRIES IN IT_VIQMEL
    WHERE WERKS EQ IT_VIQMEL-IWERK
      AND BEBER EQ IT_VIQMEL-BEBER.
  SORT IT_T357 ASCENDING BY WERKS BEBER.
*
  CLEAR LV_CONTNF.      "Qtd de Notas
  LOOP AT IT_VIQMEL.
    READ TABLE IT_VIQMFE WITH KEY QMNUM = IT_VIQMEL-QMNUM
     BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      CLEAR IT_REG.
      READ TABLE IT_QPCT WITH KEY CODE = IT_VIQMFE-FECOD
      BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_T357 WITH KEY WERKS = IT_VIQMEL-IWERK
                                    BEBER = IT_VIQMEL-BEBER
            BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          LV_CONTNF = LV_CONTNF + 1.
*
          IT_REG-FECOD    = IT_VIQMFE-FECOD.
          IT_REG-BTXTCDFE = IT_QPCT-KURZTEXT.
          IT_REG-IWERK    = IT_VIQMEL-IWERK.
          IT_REG-QTDDIAG  = LC_1.
          IT_REG-FING     = IT_T357-FING.
          COLLECT IT_REG.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
* Calcula percentual de cada Defeito
  LOOP AT IT_REG.
    IT_REG-QTDPERC = ( IT_REG-QTDDIAG * 100 ) / LV_CONTNF.
    MODIFY IT_REG.
  ENDLOOP.
*
  SORT IT_REG ASCENDING BY FECOD.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_EXIBE_ALV .
  CALL SCREEN 100.
*

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_BEBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_SELECT_BEBER .
*
  CHECK NOT S_IWERK IS INITIAL.

  SELECT WERKS BEBER FING INTO TABLE IT_T357
    FROM T357
    WHERE WERKS IN S_IWERK.
*
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      RETFIELD        = 'BEBER'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_T357
      RETURN_TAB      = IT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  LOOP AT IT_RETURN INTO WA_RETURN.
    "The selected field needs to be passed to the screen field
*      z_equi_equnr = wa_return-fieldval.
    READ TABLE IT_T357 WITH KEY BEBER = WA_RETURN-FIELDVAL.
    IF SY-SUBRC IS INITIAL.
      P_BEBER = IT_T357-BEBER.
    ENDIF.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
INCLUDE ZPM_DIAG_POR_PLANTA_T_100_22.
