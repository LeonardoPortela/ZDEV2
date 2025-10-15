**-----------------------------------------------------------------------------*
*&                          AMAGGI - REPORT ZPMR0026.
*-----------------------------------------------------------------------------*
*& Criado por: Anderson Oenning
*& Data      : 06/07/2017
*& Especificado: Anderson Oenning
*-----------------------------------------------------------------------------*
*& Histórico de Alterações:                                                   *
*-----------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                       *
*&----------------------------------------------------------------------------*
*&             | DEVK972980 |               |                                 *
*-----------------------------------------------------------------------------*
REPORT ZPMR0026.

TABLES: ITOB, VIQMEL, T357, ILOA, IFLO, AFIH, AUFK, AFRU, CRHD.


TYPES: BEGIN OF TY_EQKT,
         EQUNR TYPE EQKT-EQUNR,
         EQKTX TYPE EQKT-EQKTX,
       END OF TY_EQKT.


TYPES: BEGIN OF TY_AFIH,
         AUFNR TYPE AFIH-AUFNR,
         EQUNR TYPE AFIH-EQUNR,
         IWERK TYPE AFIH-IWERK,
       END OF TY_AFIH.

TYPES: BEGIN OF TY_VIAUFKST,
         AUFNR TYPE VIAUFKST-AUFNR,
         EQUNR TYPE VIAUFKST-EQUNR,
         TPLNR TYPE VIAUFKST-TPLNR,
         IWERK TYPE VIAUFKST-IWERK,
         BEBER TYPE VIAUFKST-BEBER,
         ILOAN TYPE VIAUFKST-ILOAN,
       END OF TY_VIAUFKST.


TYPES: BEGIN OF TY_AUFK,
         AUFNR TYPE AUFK-AUFNR,
         WERKS TYPE AUFK-WERKS,
         KOSTL TYPE AUFK-KOSTL,
         AUART TYPE AUFK-AUART,
         OBJNR TYPE AUFK-OBJNR,
         VAPLZ TYPE AUFK-VAPLZ,
         AUTYP TYPE AUFK-AUTYP,
         ERDAT TYPE AUFK-ERDAT,
       END OF TY_AUFK.


TYPES: BEGIN OF TY_AFRU,
         AUFNR TYPE AFRU-AUFNR,
         VORNR TYPE AFRU-VORNR,
         ISMNW TYPE AFRU-ISMNW,
         ISMNE TYPE AFRU-ISMNE,
         ERSDA TYPE AFRU-ERSDA,
         ERZET TYPE AFRU-ERZET,
         WERKS TYPE AFRU-WERKS,
         STOKZ TYPE AFRU-STOKZ,
         STZHL TYPE AFRU-STZHL,
         BUDAT TYPE AFRU-BUDAT,
         LAEDA TYPE AFRU-LAEDA,
         ARBID TYPE AFRU-ARBID,
       END OF TY_AFRU.

TYPES: BEGIN OF TY_CSKT,
         KOSTL TYPE CSKT-KOSTL,
         KTEXT TYPE CSKT-KTEXT,
       END OF TY_CSKT.


TYPES: BEGIN OF TY_ILOA,
         BEBER TYPE ILOA-BEBER,
         TPLNR TYPE ILOA-TPLNR,
         SWERK TYPE ILOA-SWERK,
         ILOAN TYPE ILOA-ILOAN,
       END OF TY_ILOA.


TYPES: BEGIN OF TY_T357,
         BEBER TYPE T357-BEBER,
         FING  TYPE T357-FING,
         WERKS TYPE T357-WERKS,
       END OF TY_T357.

TYPES: BEGIN OF TY_IFLO,
         TPLNR TYPE IFLO-TPLNR,
         PLTXT TYPE IFLO-PLTXT,
         IWERK TYPE IFLO-IWERK,
       END OF TY_IFLO.


TYPES: BEGIN OF TY_PARAM,
         PARAM TYPE ZTPARAM-PARAM,
         ZVAL  TYPE ZTPARAM-ZVAL,
         CONST TYPE ZTPARAM-CONST,
       END OF TY_PARAM.

TYPES: BEGIN OF TY_ZVAL,
         PARAM TYPE ZTPARAM-PARAM,
         ZVAL  TYPE AUFK-AUART,
         CONST TYPE AUFK-WERKS,
       END OF TY_ZVAL.

TYPES: BEGIN OF TY_ITOB,
         AUFNR TYPE ITOB-AUFNR,
         TPLNR TYPE ITOB-TPLNR,
         EQUNR TYPE ITOB-EQUNR,
         IWERK TYPE ITOB-IWERK,
         BEBER TYPE ITOB-BEBER,
       END OF TY_ITOB.

TYPES: BEGIN OF TY_CRHD,
         OBJID TYPE CRHD-OBJID,
         ARBPL TYPE CRHD-ARBPL,
         WERKS TYPE CRHD-WERKS,
       END OF TY_CRHD.

TYPES: BEGIN OF TY_SAIDA,
         BEBER TYPE ILOA-BEBER,
         FING  TYPE T357-FING,
         KOSTL TYPE AUFK-KOSTL,
         KTEXT TYPE CSKT-KTEXT,
         VAPLZ TYPE AUFK-VAPLZ,
         AUFNR TYPE AFRU-AUFNR,
         ISMNW TYPE AFRU-ISMNW,
         ERSDA TYPE AFRU-ERSDA,
         WERKS TYPE AFRU-WERKS,
         ARBPL TYPE CRHD-ARBPL,
         QORPR TYPE P,
         QOCOR TYPE P,
         QOPRV TYPE P,
         QOMEL TYPE P,
         QOPRO TYPE P,
         QOLUB TYPE P,
         QOTOT TYPE P,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_VIAU,
         AUFNR TYPE VIAUFKST-AUFNR,
         EQUNR TYPE VIAUFKST-EQUNR,
         TPLNR TYPE VIAUFKST-TPLNR,
         IWERK TYPE VIAUFKST-IWERK,
         BEBER TYPE VIAUFKST-BEBER,
       END OF TY_VIAU.


DATA:
  IT_SAIDA    TYPE TABLE OF TY_SAIDA,
  IT_AUFK     TYPE TABLE OF TY_AUFK,
  IT_AFRU     TYPE TABLE OF TY_AFRU,
  IT_IFLO     TYPE TABLE OF TY_IFLO,
  IT_AFIH     TYPE TABLE OF TY_AFIH,
  IT_CSKT     TYPE TABLE OF TY_CSKT,
  IT_ILOA     TYPE TABLE OF TY_ILOA,
  IT_T357     TYPE TABLE OF TY_T357,
  IT_PARAM    TYPE TABLE OF TY_PARAM WITH HEADER LINE,
  IT_ZVAL     TYPE TABLE OF TY_ZVAL  WITH HEADER LINE,
  IT_CRHD     TYPE TABLE OF TY_CRHD,
  IT_VIAUFKST TYPE TABLE OF TY_VIAUFKST.



DATA:
  WA_SAIDA    TYPE TY_SAIDA,
  WA_AUFK     TYPE TY_AUFK,
  WA_AFRU     TYPE TY_AFRU,
  WA_IFLO     TYPE TY_IFLO,
  WA_AFIH     TYPE TY_AFIH,
  WA_CSKT     TYPE TY_CSKT,
  WA_ILOA     TYPE TY_ILOA,
  WA_ITOB     TYPE TY_ITOB,
  WA_T357     TYPE TY_T357,
  WA_ZVAL     TYPE TY_ZVAL,
  WA_CRHD     TYPE TY_CRHD,
  WA_VIAUFKST TYPE TY_VIAUFKST.

DATA:
  "IT_SELECT           TYPE STANDARD TABLE OF TY_DADOS_IMOB,
  G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  DG_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
  DG_PARENT_1        TYPE REF TO CL_GUI_CONTAINER,
  DG_SPLITTER_2      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
  DG_PARENT_2        TYPE REF TO CL_GUI_CONTAINER,
  DG_PARENT_2A       TYPE REF TO CL_GUI_CONTAINER,
  DG_PARENT_ALV      TYPE REF TO CL_GUI_CONTAINER,
  PICTURE            TYPE REF TO CL_GUI_PICTURE,
  CTL_ALV            TYPE REF TO CL_GUI_ALV_GRID,
  DG_DYNDOC_ID       TYPE REF TO CL_DD_DOCUMENT,
  TABLE_ELEMENT      TYPE REF TO CL_DD_TABLE_ELEMENT,
  COLUMN             TYPE REF TO CL_DD_AREA,
  TABLE_ELEMENT2     TYPE REF TO CL_DD_TABLE_ELEMENT,
  COLUMN_1           TYPE REF TO CL_DD_AREA,
  DG_HTML_CNTRL      TYPE REF TO CL_GUI_HTML_VIEWER,
  IT_EXCLUDE_FCODE   TYPE UI_FUNCTIONS,
  WA_EXCLUDE_FCODE   LIKE LINE OF IT_EXCLUDE_FCODE,
  GS_LAYOUT          TYPE LVC_S_LAYO,
  GS_VARIANT         TYPE DISVARIANT,
  IT_FIELDCATALOG    TYPE LVC_T_FCAT,
  WA_FIELDCATALOG    TYPE LVC_S_FCAT,
  IT_SORT            TYPE LVC_T_SORT,
  "GS_SCROLL_COL       TYPE LVC_S_COL,
  "GS_SCROLL_ROW       TYPE LVC_S_ROID,
  "GS_STABLE           TYPE LVC_S_STBL,
  "IT_SELECTED_ROWS    TYPE LVC_T_ROW,
  "WA_SELECTED_ROWS    TYPE LVC_S_ROW,
  LS_STABLE          TYPE LVC_S_STBL,
  T_SORT             TYPE LVC_T_SORT,
  W_SORT             TYPE LVC_T_SORT WITH HEADER LINE.

CONSTANTS:
  LC_OR_PREDIT(15)  VALUE 'OR_PREDIT',
  LC_OR_CORRET(15)  VALUE 'OR_CORRET',
  LC_OR_PREVENT(15) VALUE 'OR_PREVENT',
  LC_OR_LUBRIF(15)  VALUE 'OR_LUBRIF',
  LC_OR_PROGRA(15)  VALUE 'OR_PROGRA',
  LC_OR_MELHOR(15)  VALUE 'OR_MELHOR'.



*-----------------------------------------------------------------------------*
*   Parametro de seleção.
*-----------------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: "P_AUART FOR AUFK-AUART,
                P_WERKS  FOR ITOB-SWERK,
                P_BEBER  FOR ITOB-BEBER,
                P_ARBPL  FOR CRHD-ARBPL,
                P_BUDAT FOR  AFRU-BUDAT OBLIGATORY NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK B1.


*-----------------------------------------------------------------------------*
*   Processamento.
*-----------------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM BUSCAR_DADOS.
  PERFORM ZPREPARA_SAIDA.

  IF IT_SAIDA IS NOT INITIAL.
    PERFORM ZF_EXIBE_ALV.
  ELSE.
    MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

*-----------------------------------------------------------------------------*
*   Seleção de dados.
*-----------------------------------------------------------------------------*

FORM BUSCAR_DADOS.

  FREE: IT_ZVAL[].
  SELECT PARAM ZVAL CONST
    FROM ZTPARAM
    INTO CORRESPONDING FIELDS OF TABLE IT_PARAM
    WHERE PARAM IN (LC_OR_PREDIT, LC_OR_CORRET, LC_OR_PREVENT, LC_OR_PROGRA, LC_OR_MELHOR, LC_OR_LUBRIF).
  IF NOT IT_PARAM[] IS INITIAL.
    LOOP AT IT_PARAM.
      IT_ZVAL-PARAM = IT_PARAM-PARAM.
      IT_ZVAL-ZVAL = IT_PARAM-ZVAL.
      IT_ZVAL-CONST = IT_PARAM-CONST.
      APPEND IT_ZVAL.
    ENDLOOP.
  ENDIF.

  SELECT AUFNR WERKS KOSTL AUART OBJNR VAPLZ AUTYP ERDAT
FROM AUFK
INTO TABLE IT_AUFK
 FOR ALL ENTRIES IN IT_ZVAL
 WHERE AUART EQ IT_ZVAL-ZVAL
   AND WERKS EQ IT_ZVAL-CONST
   AND ERDAT IN P_BUDAT
   AND WERKS IN P_WERKS.


  SELECT AUFNR VORNR ISMNW ISMNE ERSDA ERZET WERKS STOKZ STZHL BUDAT LAEDA ARBID
 FROM AFRU
 INTO TABLE IT_AFRU
 FOR ALL ENTRIES IN IT_AUFK
 WHERE AUFNR EQ IT_AUFK-AUFNR
   AND BUDAT IN P_BUDAT
   AND WERKS IN P_WERKS
   AND STOKZ EQ ' '
   AND STZHL EQ 0.
  SORT IT_AFRU ASCENDING BY AUFNR.

*  DELETE IT_AFRU WHERE STOKZ EQ 'X'.

  SELECT OBJID ARBPL WERKS
    FROM CRHD
    INTO TABLE IT_CRHD
    FOR ALL ENTRIES IN IT_AFRU
    WHERE OBJID EQ IT_AFRU-ARBID
      AND ARBPL IN P_ARBPL
      AND WERKS IN P_WERKS
      AND WERKS EQ IT_AFRU-WERKS.

  SELECT AUFNR EQUNR TPLNR IWERK BEBER ILOAN
  FROM VIAUFKST
  INTO TABLE IT_VIAUFKST
  FOR ALL ENTRIES IN IT_AFRU
   WHERE AUFNR EQ IT_AFRU-AUFNR
     AND IWERK EQ IT_AFRU-WERKS
     AND BEBER IN P_BEBER
     AND IWERK IN P_WERKS
     AND TPLNR NE ' '.
  SORT IT_VIAUFKST ASCENDING BY AUFNR.

  SELECT BEBER TPLNR SWERK ILOAN
    FROM ILOA
    INTO TABLE IT_ILOA
    FOR ALL ENTRIES IN IT_VIAUFKST
    WHERE TPLNR EQ IT_VIAUFKST-TPLNR
      AND SWERK EQ IT_VIAUFKST-IWERK
*      AND ILOAN EQ IT_VIAUFKST-ILOAN
      AND BEBER IN P_BEBER
      AND SWERK IN P_WERKS
      AND TPLNR NE ' '
      AND BEBER NE ' '.
  SORT IT_ILOA ASCENDING BY BEBER SWERK.

  SELECT BEBER FING WERKS
  FROM T357
  INTO TABLE IT_T357
    FOR ALL ENTRIES IN IT_ILOA
     WHERE BEBER EQ IT_ILOA-BEBER
     AND  WERKS EQ IT_ILOA-SWERK.

ENDFORM.

FORM ZPREPARA_SAIDA.

* Preparando informações.
  LOOP AT IT_AFRU INTO WA_AFRU.
    WA_SAIDA-WERKS = WA_AFRU-WERKS.

    READ TABLE IT_AUFK INTO WA_AUFK WITH KEY AUFNR = WA_AFRU-AUFNR.

    IF SY-SUBRC = 0.
    READ TABLE IT_ZVAL INTO WA_ZVAL WITH KEY ZVAL = WA_AUFK-AUART
                                             CONST = WA_AUFK-WERKS.

        IF SY-SUBRC = 0.
        READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_AFRU-AUFNR
                                                         IWERK = WA_AFRU-WERKS.
          IF SY-SUBRC = 0.
          READ TABLE IT_CRHD INTO WA_CRHD WITH KEY OBJID = WA_AFRU-ARBID.
          IF SY-SUBRC = 0.
            WA_SAIDA-ARBPL = WA_CRHD-ARBPL.

            READ TABLE IT_ILOA INTO WA_ILOA WITH KEY TPLNR = WA_VIAUFKST-TPLNR
                                                     SWERK = WA_VIAUFKST-IWERK.

            IF SY-SUBRC = 0.
              WA_SAIDA-BEBER = WA_ILOA-BEBER.

              READ TABLE IT_T357 INTO WA_T357 WITH KEY BEBER = WA_ILOA-BEBER.
              IF SY-SUBRC = 0.
                WA_SAIDA-FING = WA_T357-FING.

                IF WA_AFRU-AUFNR IS NOT INITIAL
                AND WA_AFRU-WERKS IS NOT INITIAL
                AND WA_ILOA-BEBER IS NOT INITIAL
                AND WA_CRHD-ARBPL IS NOT INITIAL
                AND WA_ZVAL-PARAM   = LC_OR_PREDIT
                AND WA_ZVAL-ZVAL    = WA_AUFK-AUART
                AND WA_ZVAL-CONST   = WA_AFRU-WERKS.
                  WA_SAIDA-QORPR = WA_AFRU-ISMNW.
                  ENDIF.


                IF  WA_AFRU-AUFNR IS NOT INITIAL
                   AND WA_AFRU-WERKS IS NOT INITIAL
                  AND WA_ILOA-BEBER IS NOT INITIAL
                   AND WA_CRHD-ARBPL IS NOT INITIAL
                AND WA_ZVAL-PARAM   = LC_OR_CORRET
                AND WA_ZVAL-ZVAL   = WA_AUFK-AUART
                AND WA_ZVAL-CONST  = WA_AFRU-WERKS.
                  WA_SAIDA-QOCOR = WA_AFRU-ISMNW.
                  ENDIF.



                IF  WA_AFRU-AUFNR IS NOT INITIAL
                   AND WA_AFRU-WERKS IS NOT INITIAL
                  AND WA_ILOA-BEBER IS NOT INITIAL
                   AND WA_CRHD-ARBPL IS NOT INITIAL
                AND WA_ZVAL-PARAM   = LC_OR_PREVENT
                  AND WA_ZVAL-ZVAL   = WA_AUFK-AUART
                 AND WA_ZVAL-CONST  = WA_AFRU-WERKS.
                  WA_SAIDA-QOPRV = WA_AFRU-ISMNW.
                  ENDIF.



                  IF  WA_AFRU-AUFNR IS NOT INITIAL
                     AND WA_AFRU-WERKS IS NOT INITIAL
                    AND WA_ILOA-BEBER IS NOT INITIAL
                     AND WA_CRHD-ARBPL IS NOT INITIAL
                  AND WA_ZVAL-PARAM   = LC_OR_MELHOR
                 AND WA_ZVAL-ZVAL   = WA_AUFK-AUART
                 AND WA_ZVAL-CONST  = WA_AFRU-WERKS.
                    WA_SAIDA-QOMEL = WA_AFRU-ISMNW.
                    ENDIF.



                  IF  WA_AFRU-AUFNR IS NOT INITIAL
                     AND WA_AFRU-WERKS IS NOT INITIAL
                    AND WA_ILOA-BEBER IS NOT INITIAL
                     AND WA_CRHD-ARBPL IS NOT INITIAL
                  AND WA_ZVAL-PARAM   = LC_OR_PROGRA
                 AND WA_ZVAL-ZVAL   = WA_AUFK-AUART
                 AND WA_ZVAL-CONST  = WA_AFRU-WERKS.
                    WA_SAIDA-QOPRO = WA_AFRU-ISMNW.
                 ENDIF.



                  IF  WA_AFRU-AUFNR IS NOT INITIAL
                     AND WA_AFRU-WERKS IS NOT INITIAL
                     AND WA_AFRU-WERKS IS NOT INITIAL
                    AND WA_ILOA-BEBER IS NOT INITIAL
                     AND WA_CRHD-ARBPL IS NOT INITIAL
                  AND WA_ZVAL-PARAM   = LC_OR_LUBRIF
                  AND WA_ZVAL-ZVAL   = WA_AUFK-AUART
                  AND WA_ZVAL-CONST  = WA_AFRU-WERKS.
                    WA_SAIDA-QOLUB  = WA_AFRU-ISMNW.
                  ENDIF.


                  COLLECT WA_SAIDA INTO IT_SAIDA.

                  CLEAR WA_VIAUFKST.
                  CLEAR WA_T357.
                  CLEAR WA_CRHD.
                  CLEAR WA_ILOA.
                  CLEAR WA_ZVAL.
                  CLEAR WA_AUFK.
                  CLEAR WA_AFIH.
                  CLEAR WA_AFRU.
                  CLEAR WA_SAIDA.
                ENDIF.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDLOOP.

  SORT IT_SAIDA ASCENDING BY WERKS BEBER.

  LOOP AT IT_SAIDA INTO WA_SAIDA.
    IF IT_SAIDA IS NOT INITIAL.
      WA_SAIDA-QOTOT = ( WA_SAIDA-QORPR + WA_SAIDA-QOCOR + WA_SAIDA-QOPRV + WA_SAIDA-QOMEL + WA_SAIDA-QOPRO + WA_SAIDA-QOLUB ).
      MODIFY IT_SAIDA FROM WA_SAIDA.
    ENDIF.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA:
    LST_LAYOUT TYPE LVC_S_LAYO.

  DATA: URL(255)                TYPE C,
        P_TEXT                  TYPE SDYDO_TEXT_ELEMENT,
        SDYDO_TEXT_ELEMENT(255),
        P_TEXT_TABLE            TYPE SDYDO_TEXT_TABLE,
        VL_CONT                 TYPE I,
        VL_BUTXT                TYPE T001-BUTXT,
        VL_DATES1               TYPE CHAR10,
        VL_DATES2               TYPE CHAR10.


  SET PF-STATUS 'T001'.
  SET TITLEBAR 'T002'.

  IF G_CUSTOM_CONTAINER IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC <> 0.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.

    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_ALV.

    CREATE OBJECT DG_SPLITTER_2
      EXPORTING
        PARENT  = DG_PARENT_1
        ROWS    = 1
        COLUMNS = 2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 2
      RECEIVING
        CONTAINER = DG_PARENT_2A.

    CALL METHOD DG_SPLITTER_1->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 15.

    CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
      EXPORTING
        ID    = 1
        WIDTH = 65.

    CREATE OBJECT PICTURE
      EXPORTING
        PARENT = DG_PARENT_2A.

    PERFORM F_PEGA_IMAGEM USING 'LOGO_NOVO' CHANGING URL.

    CALL METHOD PICTURE->LOAD_PICTURE_FROM_URL
      EXPORTING
        URL = URL.

    CALL METHOD PICTURE->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE->DISPLAY_MODE_FIT_CENTER.

    IF IT_SAIDA IS NOT INITIAL.
      P_TEXT = TEXT-004.
      PERFORM FILL_IT_FIELDCATALOG USING:
               02 'BEBER '      'ILOA   '    '04'  ' '     ' '    ' '   'Cod',
               03 'FING  '      'T357   '    '15'  ' '     ' '    ' '   'Area Operacional     ',
               01 'WERKS '      'AFRU   '    '06'  ' '     ' '    ' '   'Centro               ',
*               04 'AUFNR '      'AFRU   '    '07'  ' '     ' '    ' '   'Ordem               ',
*               05 'KOSTL '      'AUFK   '    '15'  ' '     ' '    ' '   'Centro de Custo     ',
*               06 'KTEXT '      'CSKT   '    '18'  ' '     ' '    ' '   'Desc. Cent Custo    ',
               07 'ARBPL '      'CRHD   '    '10'  ' '     ' '    ' '   'Centro Trab           ',
               08 'QORPR '      '       '    '10'  ' '     ' '    'X'   'Ordem Preditiva       ',
               09 'QOCOR '      '       '    '10'  ' '     ' '    'X'   'Ordem Corretiva       ',
               10 'QOPRV '      '       '    '13'  ' '     ' '    'X'   'Ordem Preventiva      ',
               11 'QOMEL'       '       '    '12'  ' '     ' '    'X'   'Ordem Melhoria        ',
               12 'QOPRO'       '       '    '13'  ' '     ' '    'X'   'Ordem Programada      ',
               13 'QOLUB'       '       '    '13'  ' '     ' '    'X'   'Ordem Lubrificação    ',
               14 'QOTOT'       '       '    '13'  ' '     ' '    'X'   'Total Horas           '.
*               14 'ERSDA'       'AFRU   '    '15'  ' '     ' '    ' '   'Data Apontamento     '.

      PERFORM FILL_IT_SORT.


      GS_LAYOUT-SEL_MODE   = 'A'.
      GS_LAYOUT-CWIDTH_OPT = 'X'.
*      CLEAR: IT_EXCLUDE_FCODE, IT_EXCLUDE_FCODE[].

      CREATE OBJECT CTL_ALV
        EXPORTING
          I_PARENT = DG_PARENT_ALV.


      CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_LAYOUT       = GS_LAYOUT
*         IS_VARIANT      = GS_VARIANT
*         IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
          I_SAVE          = 'A'
        CHANGING
          IT_FIELDCATALOG = IT_FIELDCATALOG
          IT_OUTTAB       = IT_SAIDA
          IT_SORT         = IT_SORT.


      CREATE OBJECT DG_DYNDOC_ID
        EXPORTING
          STYLE = 'ALV_GRID'.

      CALL METHOD DG_DYNDOC_ID->INITIALIZE_DOCUMENT.

      CALL METHOD DG_DYNDOC_ID->ADD_TABLE
        EXPORTING
          NO_OF_COLUMNS = 1
          BORDER        = '0'
          WIDTH         = '100%'
        IMPORTING
          TABLE         = TABLE_ELEMENT.

      CALL METHOD TABLE_ELEMENT->ADD_COLUMN
        IMPORTING
          COLUMN = COLUMN.

      CALL METHOD TABLE_ELEMENT->SET_COLUMN_STYLE
        EXPORTING
          COL_NO    = 1
          "SAP_ALIGN = 'CENTER'
          SAP_STYLE = CL_DD_DOCUMENT=>HEADING.

      CALL METHOD COLUMN->ADD_TEXT
        EXPORTING
          TEXT      = P_TEXT
          SAP_STYLE = 'HEADING'.

      CALL METHOD DG_DYNDOC_ID->ADD_TABLE
        EXPORTING
          NO_OF_COLUMNS = 2
          BORDER        = '0'
          WIDTH         = '100%'
        IMPORTING
          TABLE         = TABLE_ELEMENT2.

      CALL METHOD TABLE_ELEMENT2->ADD_COLUMN
        EXPORTING
          SAP_STYLE   = 'SAP_BOLD'
          STYLE_CLASS = 'SAP_BOLD'
        IMPORTING
          COLUMN      = COLUMN_1.


* Mosta o cabeçario com o periodo pesquisado.
      LOOP AT P_BUDAT.
        IF P_BUDAT-OPTION EQ 'BT'.
          CONCATENATE P_BUDAT-LOW+6(2) '.' P_BUDAT-LOW+4(2) '.' P_BUDAT-LOW(4) INTO VL_DATES1.
          CONCATENATE P_BUDAT-HIGH+6(2) '.' P_BUDAT-HIGH+4(2) '.' P_BUDAT-HIGH(4) INTO VL_DATES2.
          CONCATENATE 'Período:' VL_DATES1 '-' VL_DATES2 INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
          EXIT.
        ELSE.
          CONCATENATE P_BUDAT-LOW+6(2) '.' P_BUDAT-LOW+4(2) '.' P_BUDAT-LOW(4) INTO VL_DATES1.
          CONCATENATE 'Período:' VL_DATES1 INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
        ENDIF.
      ENDLOOP.
      APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
      CLEAR: SDYDO_TEXT_ELEMENT, VL_DATES1, VL_DATES2.


      "------------------
      CALL METHOD COLUMN_1->ADD_TEXT
        EXPORTING
          TEXT_TABLE = P_TEXT_TABLE
          FIX_LINES  = 'X'.

      CALL METHOD DG_DYNDOC_ID->MERGE_DOCUMENT.

      CREATE OBJECT DG_HTML_CNTRL
        EXPORTING
          PARENT = DG_PARENT_2.

      DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.

      CALL METHOD DG_DYNDOC_ID->DISPLAY_DOCUMENT
        EXPORTING
          REUSE_CONTROL      = 'X'
          PARENT             = DG_PARENT_2
        EXCEPTIONS
          HTML_DISPLAY_ERROR = 1.

    ELSE.

      LS_STABLE-ROW = 'X'.
      LS_STABLE-COL = 'X'.

      CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = LS_STABLE
        EXCEPTIONS
          FINISHED  = 1
          OTHERS    = 2.

      IF SY-SUBRC <> 0.
      ENDIF.

    ENDIF.
  ENDIF.


ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

FORM FILL_IT_SORT .

  DATA: WA_SORT TYPE LVC_S_SORT.

  WA_SORT-SPOS = '1'.
  WA_SORT-FIELDNAME = 'WERKS'.
*  WA_SORT-DOWN = 'X'.
  WA_SORT-GROUP = '*'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO IT_SORT.

ENDFORM.



*  Busca a logo Marca e adiciona no cabeçario.
FORM F_PEGA_IMAGEM  USING    NOME_LOGO
                  CHANGING URL.

  DATA: BEGIN OF GRAPHIC_TABLE OCCURS 0,
          LINE(255) TYPE X,
        END OF GRAPHIC_TABLE.

  DATA: L_GRAPHIC_XSTR TYPE XSTRING.
  DATA: GRAPHIC_SIZE   TYPE I.
  DATA: L_GRAPHIC_CONV TYPE I.
  DATA: L_GRAPHIC_OFFS TYPE I.

  REFRESH GRAPHIC_TABLE.

  CALL METHOD CL_SSF_XSF_UTILITIES=>GET_BDS_GRAPHIC_AS_BMP
    EXPORTING
      P_OBJECT = 'GRAPHICS'
      P_NAME   = NOME_LOGO
      P_ID     = 'BMAP'
      P_BTYPE  = 'BCOL'
    RECEIVING
      P_BMP    = L_GRAPHIC_XSTR.

  GRAPHIC_SIZE = XSTRLEN( L_GRAPHIC_XSTR ).
  L_GRAPHIC_CONV = GRAPHIC_SIZE.
  L_GRAPHIC_OFFS = 0.

  WHILE L_GRAPHIC_CONV > 255.

    GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(255).
    APPEND GRAPHIC_TABLE.
    L_GRAPHIC_OFFS = L_GRAPHIC_OFFS + 255.
    L_GRAPHIC_CONV = L_GRAPHIC_CONV - 255.

  ENDWHILE.

  GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(L_GRAPHIC_CONV).
  APPEND GRAPHIC_TABLE.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE     = 'IMAGE'
      SUBTYPE  = 'X-UNKNOWN'
      SIZE     = GRAPHIC_SIZE
      LIFETIME = 'T'
    TABLES
      DATA     = GRAPHIC_TABLE
    CHANGING
      URL      = URL.

ENDFORM.                    " F_PEGA_IMAGEM

*Parametros da ALV.
FORM FILL_IT_FIELDCATALOG USING VALUE(P_COLNUM)
                                VALUE(P_FIELDNAME)
                                VALUE(P_TABNAME)
                                VALUE(P_LEN)
                                VALUE(P_EDIT)
                                VALUE(P_ICON)
                                VALUE(P_DO_SUM)
                                VALUE(P_HEADER).

  DATA:  WA_FIELDCATALOG  TYPE LVC_S_FCAT.

  WA_FIELDCATALOG-COL_POS     = P_COLNUM.
  WA_FIELDCATALOG-FIELDNAME   = P_FIELDNAME.
  WA_FIELDCATALOG-TABNAME     = P_TABNAME.
  WA_FIELDCATALOG-OUTPUTLEN   = P_LEN.
  WA_FIELDCATALOG-COLTEXT     = P_HEADER.
  WA_FIELDCATALOG-EDIT        = P_EDIT.
  WA_FIELDCATALOG-ICON        = P_ICON.
  WA_FIELDCATALOG-REF_TABLE   = P_TABNAME.
  WA_FIELDCATALOG-CHECKTABLE  = P_TABNAME.
  WA_FIELDCATALOG-DO_SUM      = P_DO_SUM.

  GS_LAYOUT-EXCP_CONDS    = 'X'.
  GS_LAYOUT-ZEBRA         = 'X'.
  GS_LAYOUT-SEL_MODE      = 'A'.
  GS_LAYOUT-CWIDTH_OPT    = 'X'.     "  Otimizar colunas na tela
  GS_LAYOUT-TOTALS_BEF     = ' '.

  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.
ENDFORM.

FORM ZF_EXIBE_ALV .
  CALL SCREEN 100.
ENDFORM.
