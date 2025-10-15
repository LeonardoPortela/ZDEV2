*--------------------------------------------------------------------------------------------------------*
*&                          AMAGGI
*--------------------------------------------------------------------------------------------------------*
*& REPORT ZPMR0030.                                                                                      *
*& Data           : 03/12/2017                                                                           *
*& Especificado   : Anderson Oenning                                                                     *
*& Desenvolvimento: Anderson Oenning                                                                     *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*--------------------------------------------------------------------------------------------------------*
REPORT ZPMR0031.


*----------------------------------------------------*
*               Tables                               *
*----------------------------------------------------*

*** Macros
***********************************************************

CONSTANTS: CC_A        TYPE C VALUE 'A',
           CC_X        TYPE C VALUE 'X',
           CC_I        TYPE C VALUE 'I',
           CC_1        TYPE C VALUE '1',
           CC_2        TYPE C VALUE '2',
           CC_SPRAS(2) TYPE C VALUE 'PT',
           CC_M        TYPE C VALUE 'M'.
* Definições para ALV
TYPE-POOLS: KKBLO.  "Tipos globais para ALV

*----------------------------------------------------*
*               Variaveis Globais                    *
*----------------------------------------------------*
DATA:
  G_CONTAINER      TYPE REF TO CL_GUI_CUSTOM_CONTAINER, "Conteiner ALV
  CTL_ALV          TYPE REF TO CL_GUI_ALV_GRID,
  GS_VARIANT       TYPE DISVARIANT,
  IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
  WA_EXCLUDE_FCODE LIKE LINE OF IT_EXCLUDE_FCODE,
  GS_LAYOUT        TYPE LVC_S_LAYO, "Layout da ALV
  IT_FIELDCATALOG  TYPE LVC_T_FCAT, "Catálogo de campos para controle visor de listas
  WA_FIELDCATALOG  TYPE LVC_S_FCAT, "Controle VLA: catálogo de campos
  IT_SORT          TYPE LVC_T_SORT.

DATA:
  DG_SPLITTER_1  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
  DG_PARENT_1    TYPE REF TO CL_GUI_CONTAINER,
  DG_SPLITTER_2  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
  DG_PARENT_2    TYPE REF TO CL_GUI_CONTAINER,
  DG_PARENT_2A   TYPE REF TO CL_GUI_CONTAINER,
  DG_PARENT_ALV  TYPE REF TO CL_GUI_CONTAINER,
  PICTURE        TYPE REF TO CL_GUI_PICTURE,
  DG_DYNDOC_ID   TYPE REF TO CL_DD_DOCUMENT,
  TABLE_ELEMENT  TYPE REF TO CL_DD_TABLE_ELEMENT,
  COLUMN         TYPE REF TO CL_DD_AREA,
  TABLE_ELEMENT2 TYPE REF TO CL_DD_TABLE_ELEMENT,
  COLUMN_1       TYPE REF TO CL_DD_AREA,
  DG_HTML_CNTRL  TYPE REF TO CL_GUI_HTML_VIEWER.

DATA: URL(255)                TYPE C,
      P_TEXT                  TYPE SDYDO_TEXT_ELEMENT,
      SDYDO_TEXT_ELEMENT(255),
      P_TEXT_TABLE            TYPE SDYDO_TEXT_TABLE,
      VL_CONT                 TYPE I,
      VL_BUTXT                TYPE T001-BUTXT.
*----------------------------------------------------*
*                ALV GRID                            *
*----------------------------------------------------*
DATA:                   VL_NIVEL    TYPE NUMC10.

*----------------------------------------------------*
*               Tables                               *
*----------------------------------------------------*
TABLES: ZPMR0006, AUFK, EQUI, EQKT.

*----------------------------------------------------*
*               Types                                *
*----------------------------------------------------*
TYPES: BEGIN OF TY_AUFK,
         BUKRS TYPE AUFK-BUKRS,
         AUFNR TYPE AUFK-AUFNR,
         USER4 TYPE AUFK-USER4,
         TPLNR TYPE VIAUFKST-TPLNR,
         PLTXT TYPE IFLO-PLTXT,
         EQUNR TYPE VIAUFKST-EQUNR,
         EQKTX TYPE EQKT-EQKTX,
         WERKS TYPE AUFK-WERKS,
       END OF TY_AUFK,


       " Para pegar a descrição
       BEGIN OF TY_EQKT,
         EQUNR TYPE EQKT-EQUNR, " Equipamento
         EQKTX TYPE EQKT-EQKTX, " Descrição
       END OF TY_EQKT,

       BEGIN OF TY_ORDENS,
         AUFNR          TYPE ZPMR0006-AUFNR,
         SOLICITANTE    TYPE ZPMR0006-SOLICITANTE,
         DT_SOLICITACAO TYPE ZPMR0006-DT_SOLICITACAO,
         EQUIPMENT      TYPE ZPMR0006-EQUIPMENT,
         PLTXT          TYPE IFLO-PLTXT,
         WERKS          TYPE ZPMR0006-WERKS,
         STATUS         TYPE ZPMR0006-STATUS,
         V_ESTIMADO     TYPE ZPMR0006-VLR_ESTIMADO,
         OBSERVACAO     TYPE ZPMR0006-OBSERVACAO,
         DESC_EQUIP     TYPE ZPMR0006-EQUIPMENT_DESC,
         NIVEL_APROVADO TYPE ZPMR0006-NIVEL_APROVADO,
         RESPONSAVEL    TYPE ZPMR0006-RESPONSAVEL,
         RESPONSAVEIS   TYPE C LENGTH 255,
         FUNCAO         TYPE C LENGTH 255, "Permit de aprovação.
         P_APROV        TYPE C LENGTH 255, "Nome da pemit do aprovador.
         P_NIVEL        TYPE C LENGTH 255,  "Nivel pendente aprovação.
         USER4          TYPE AUFK-USER4,
*         SUPLEMENTACAO  TYPE ZPMR0006-VLR_ESTIMADO,
         NAPROVADOR     TYPE C LENGTH 255, "Nome do aprovador pendente.
         ICONE          TYPE ICON-ID,      "Icone.
       END OF TY_ORDENS.

TYPES : BEGIN OF TY_SUPLEMENTOS.
        INCLUDE TYPE ZPMR0006.
TYPES : APROVADOR TYPE USNAM,
       PERMIT    TYPE PMSOG,
       CHECK     TYPE CHAR1,
       END OF TY_SUPLEMENTOS.

DATA: SUPLEMENTOS TYPE TABLE OF TY_SUPLEMENTOS,
      T_EQKT      TYPE TABLE OF TY_EQKT WITH HEADER LINE,
      T_ZPMR0006  TYPE TABLE OF ZPMR0006 WITH HEADER LINE,
      T_ZPMR0002  TYPE TABLE OF ZPMR0002 WITH HEADER LINE,
      T_AUFK      TYPE TABLE OF TY_AUFK  WITH HEADER LINE,
      T_ORDENS    TYPE TABLE OF TY_ORDENS WITH HEADER LINE,
      WA_ZPMR0002 TYPE ZPMR0002,
      VIR         TYPE CHAR1 VALUE ','.


*----------------------------------------------------*
*                EVENT                               *
*----------------------------------------------------*
CLASS LCL_EVENTOS DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID.

ENDCLASS.                    "LCL_EVENT DEFINITION


*----------------------------------------------------*
*                Parâmetros de Seleção               *
*----------------------------------------------------*

*Status autorização.
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE .

SELECTION-SCREEN COMMENT 1(08) TEXT-002.
SELECTION-SCREEN POSITION 11.
PARAMETERS: P_NOTCO AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN POSITION 10.
SELECTION-SCREEN COMMENT 15(09) TEXT-003.
PARAMETERS: P_CONC AS CHECKBOX DEFAULT 'X'.

*SELECTION-SCREEN POSITION 26.
*SELECTION-SCREEN COMMENT 31(08) TEXT-007.
*PARAMETERS: P_REC AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK BLOCK1.

SELECTION-SCREEN: BEGIN OF BLOCK BLOCK2
                        WITH FRAME TITLE TEXT-004.
SELECT-OPTIONS: P_BUKRS FOR AUFK-BUKRS OBLIGATORY,
                P_WERKS FOR ZPMR0006-WERKS,
                P_AUFNR FOR ZPMR0006-AUFNR,
                P_SOLIC FOR ZPMR0006-SOLICITANTE,
                P_PERI  FOR ZPMR0006-DT_SOLICITACAO.
SELECTION-SCREEN: END OF BLOCK BLOCK2.


*---------------------------------------------------*
*               Evento de inicialização             *
*---------------------------------------------------*
INITIALIZATION.

START-OF-SELECTION.
  PERFORM F_SELECIONA_DADOS.
  PERFORM F_SAIDA.
  IF T_ORDENS[] IS NOT INITIAL.
    PERFORM F_EXIBIR_ALV.
  ELSE.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

END-OF-SELECTION.
*
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_ORDENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS.

  IF P_NOTCO IS INITIAL AND P_CONC IS INITIAL. "AND P_REC IS INITIAL.
    MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF P_NOTCO IS NOT INITIAL AND P_CONC IS NOT INITIAL.

    PERFORM F_LUPA USING 'Selecionando status das ordens' SPACE.

    SELECT *
    FROM ZPMR0006
    INTO CORRESPONDING FIELDS OF TABLE T_ZPMR0006
    WHERE AUFNR           IN P_AUFNR
    AND   SOLICITANTE     IN P_SOLIC
    AND   DT_SOLICITACAO  IN P_PERI.
*    AND   STATUS NE 'R'.

    CHECK T_ZPMR0006[] IS NOT INITIAL.
    SELECT DISTINCT A~BUKRS A~WERKS A~AUFNR A~USER4 C~TPLNR C~PLTXT B~EQUNR
    FROM AUFK AS A
    INNER JOIN VIAUFKST AS B ON B~AUFNR = A~AUFNR
    INNER JOIN IFLO     AS C ON C~TPLNR = B~TPLNR
*    INNER JOIN EQKT     AS D ON D~EQUNR = B~EQUNR
    INTO CORRESPONDING FIELDS OF TABLE T_AUFK
    FOR ALL ENTRIES IN T_ZPMR0006[]
    WHERE A~AUFNR EQ T_ZPMR0006-AUFNR
    AND   A~WERKS IN P_WERKS
    AND   A~BUKRS IN P_BUKRS
    AND   A~PHAS1  EQ ABAP_TRUE.

    SELECT *
    FROM ZPMR0002
    INTO CORRESPONDING FIELDS OF TABLE T_ZPMR0002
    FOR ALL ENTRIES IN T_ZPMR0006[]
    WHERE CENTRO_DESP EQ T_ZPMR0006-WERKS.
  ELSE.

    DATA R_STATUS TYPE RANGE OF ZPMR0006-STATUS WITH HEADER LINE.

    R_STATUS-OPTION = 'EQ'.
    R_STATUS-SIGN   = 'I'.

    IF P_NOTCO EQ ABAP_TRUE.
      R_STATUS-LOW = 'P'. APPEND R_STATUS.
      R_STATUS-LOW = 'R'. APPEND R_STATUS.
    ENDIF.

    IF P_CONC EQ ABAP_TRUE.
      R_STATUS-LOW = 'L'. APPEND R_STATUS.
    ENDIF.

**  IF P_REC EQ ABAP_TRUE.
*    R_STATUS-LOW = 'R'. APPEND R_STATUS.
*  ENDIF.

    PERFORM F_LUPA USING 'Selecionando status das ordens' SPACE.

    SELECT *
    FROM ZPMR0006
    INTO CORRESPONDING FIELDS OF TABLE T_ZPMR0006
    WHERE AUFNR           IN P_AUFNR
    AND   SOLICITANTE     IN P_SOLIC
    AND   DT_SOLICITACAO  IN P_PERI
    AND   STATUS          IN R_STATUS.

    CHECK T_ZPMR0006[] IS NOT INITIAL.
    SELECT DISTINCT A~BUKRS A~WERKS A~AUFNR A~USER4 C~TPLNR C~PLTXT B~EQUNR
    FROM AUFK AS A
    INNER JOIN VIAUFKST AS B ON B~AUFNR = A~AUFNR
    INNER JOIN IFLO     AS C ON C~TPLNR = B~TPLNR
*    INNER JOIN EQKT     AS D ON D~EQUNR = B~EQUNR
    INTO CORRESPONDING FIELDS OF TABLE T_AUFK
    FOR ALL ENTRIES IN T_ZPMR0006[]
    WHERE A~AUFNR EQ T_ZPMR0006-AUFNR
    AND   A~WERKS IN P_WERKS
    AND   A~BUKRS IN P_BUKRS
    AND   A~PHAS1  EQ ABAP_TRUE.

    SELECT *
    FROM ZPMR0002
    INTO CORRESPONDING FIELDS OF TABLE T_ZPMR0002
    FOR ALL ENTRIES IN T_ZPMR0006[]
    WHERE CENTRO_DESP EQ T_ZPMR0006-WERKS.
  ENDIF.


ENDFORM.
FORM F_SAIDA.

  LOOP AT T_ZPMR0006.
    READ TABLE T_AUFK WITH KEY AUFNR = T_ZPMR0006-AUFNR.

    IF SY-SUBRC = 0.
      T_ORDENS-AUFNR           = |{ T_ZPMR0006-AUFNR ALPHA = OUT }|.
      T_ORDENS-SOLICITANTE     = T_ZPMR0006-SOLICITANTE.
      T_ORDENS-DT_SOLICITACAO  = T_ZPMR0006-DT_SOLICITACAO.
      T_ORDENS-STATUS          = T_ZPMR0006-STATUS.
      T_ORDENS-V_ESTIMADO      = T_ZPMR0006-VLR_ESTIMADO.
      T_ORDENS-OBSERVACAO      = T_ZPMR0006-OBSERVACAO.
*    T_ORDENS-NIVEL_APROVADO  = T_ZPMR0006-NIVEL_APROVADO.
      T_ORDENS-RESPONSAVEL     = T_ZPMR0006-RESPONSAVEL.
      T_ORDENS-NIVEL_APROVADO  = |{ T_ZPMR0006-NIVEL_APROVADO ALPHA = OUT }|.
      T_ORDENS-ICONE           = SWITCH #( T_ZPMR0006-STATUS WHEN 'P' THEN ICON_RED_LIGHT
                                                             WHEN 'L' THEN ICON_GREEN_LIGHT
                                                             WHEN 'R' THEN ICON_RED_LIGHT ).
      IF T_ORDENS-STATUS EQ 'P'. "OR T_ORDENS-STATUS EQ 'R'.
        VL_NIVEL = T_ORDENS-NIVEL_APROVADO.
        ADD 1 TO VL_NIVEL.

      ELSEIF T_ORDENS-STATUS EQ 'R'. "OR T_ORDENS-STATUS EQ 'R'.
        VL_NIVEL = T_ORDENS-NIVEL_APROVADO.
        ADD 1 TO VL_NIVEL.
      ENDIF.

      T_ORDENS-WERKS          = T_AUFK-WERKS.
      T_ORDENS-PLTXT          = T_AUFK-PLTXT.
      T_ORDENS-EQUIPMENT      = |{ T_AUFK-EQUNR ALPHA = OUT }|.

      IF T_AUFK-EQUNR IS NOT INITIAL.
        SELECT SINGLE *
        FROM EQKT
        INTO @DATA(_EQKT)
          WHERE EQUNR EQ @T_AUFK-EQUNR.

        T_ORDENS-DESC_EQUIP     = _EQKT-EQKTX.
      ENDIF.

      T_ORDENS-USER4          = T_AUFK-USER4.

      READ TABLE T_ZPMR0002 WITH KEY CENTRO_DESP  = T_ZPMR0006-WERKS
                                            NIVEL = VL_NIVEL.

      IF SY-SUBRC = 0.
        T_ORDENS-P_NIVEL = |{ T_ZPMR0002-NIVEL ALPHA = OUT }|.
        T_ORDENS-FUNCAO  = T_ZPMR0002-PERMIT.


        LOOP AT T_ZPMR0002 INTO WA_ZPMR0002 WHERE CENTRO_DESP EQ T_ZPMR0006-WERKS AND NIVEL EQ VL_NIVEL.
          IF T_ORDENS-NAPROVADOR IS INITIAL.
            T_ORDENS-NAPROVADOR = WA_ZPMR0002-APROVADOR.
          ELSE.
            T_ORDENS-NAPROVADOR = |{ T_ORDENS-NAPROVADOR }, { WA_ZPMR0002-APROVADOR }|.
          ENDIF.
        ENDLOOP.
      ENDIF.

      APPEND T_ORDENS.
      CLEAR: T_ORDENS, T_ZPMR0006, T_ZPMR0002, VL_NIVEL, WA_ZPMR0002, T_AUFK.

    ENDIF.
  ENDLOOP.
  SORT T_ORDENS[] ASCENDING BY WERKS AUFNR DT_SOLICITACAO.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_F4_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

CLASS LCL_EVENTOS IMPLEMENTATION.
  METHOD ON_HOTSPOT_CLICK.

    DATA: WA_ORDENS TYPE TY_ORDENS.

    READ TABLE T_ORDENS INTO WA_ORDENS INDEX E_ROW_ID-INDEX.

    CASE E_COLUMN_ID-FIELDNAME.
      WHEN:'EQUIPMENT'.
        SET PARAMETER ID 'EQN' FIELD WA_ORDENS-EQUIPMENT.
        CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .

      WHEN:'AUFNR'.
        SET PARAMETER ID 'ANR' FIELD WA_ORDENS-AUFNR.
        CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN .
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


FORM F_LUPA USING P_MSG1 P_MSG2.
  DATA: VL_MESSAGE(150) TYPE C.
  CLEAR VL_MESSAGE.

  CONCATENATE P_MSG1 P_MSG2 INTO VL_MESSAGE SEPARATED BY SPACE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = 99
      TEXT       = VL_MESSAGE.
ENDFORM. "f_lupa
*&---------------------------------------------------------------------*
*&      Form  F_EXIBIR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_EXIBIR_ALV .
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'T001'.
  SET TITLEBAR 'T002'.

  PERFORM F_LUPA USING 'Preenchendo ALV' SPACE.

  PERFORM CATALOGO.

  PERFORM CRIA_CONTAINER.



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
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MC_PREENCHE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_01     text
*      -->P_1527   text
*      -->P_1528   text
*      -->P_1529   text
*      -->P_1530   text
*      -->P_1531   text
*      -->P_1532   text
*      -->P_1533   text
*      -->P_1534   text
*      -->P_1535   text
*----------------------------------------------------------------------*
FORM MC_PREENCHE_FIELDCAT  USING    VALUE(P_COLNUM)
                                    VALUE(P_FIELDNAME)
                                    VALUE(P_TABNAME)
                                    VALUE(P_LEN)
                                    VALUE(P_EDIT)
                                    VALUE(P_ICON)
                                    VALUE(P_DO_SUM)
                                    VALUE(P_HEADER)
                                    VALUE(P_EMPHASIZE)
                                    VALUE(P_HOTSPOT).

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
  WA_FIELDCATALOG-EMPHASIZE   = P_EMPHASIZE.
  WA_FIELDCATALOG-HOTSPOT     = P_HOTSPOT.

  GS_LAYOUT-CTAB_FNAME    = 'CELL_COLOR'.
  GS_LAYOUT-EXCP_CONDS    = 'X'.
  GS_LAYOUT-ZEBRA         = 'X'.
  GS_LAYOUT-SEL_MODE      = 'A'.
  GS_LAYOUT-CWIDTH_OPT    = 'X'.     "  Otimizar colunas na tela
  GS_LAYOUT-TOTALS_BEF    = ''.

  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CRIA_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIA_CONTAINER .
  IF G_CONTAINER IS INITIAL.

    CREATE OBJECT G_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    PERFORM CABECARIO_ALV USING G_CONTAINER.

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = T_ORDENS[]
        IT_SORT              = IT_SORT.

    PERFORM TEXT_CABECARIO.

    SET HANDLER: LCL_EVENTOS=>ON_HOTSPOT_CLICK FOR CTL_ALV.

  ELSE.
    CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CABECARIO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_CONTAINER  text
*----------------------------------------------------------------------*
FORM CABECARIO_ALV USING G_CONTAINER.

  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

  CREATE OBJECT DG_SPLITTER_1
    EXPORTING
      PARENT  = G_CONTAINER
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
      HEIGHT = 16.

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
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1489   text
*      <--P_URL  text
*----------------------------------------------------------------------*
FORM F_PEGA_IMAGEM  USING NOME_LOGO CHANGING URL.

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
*&---------------------------------------------------------------------*
*&      Form  CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CATALOGO .

  P_TEXT = TEXT-008.
  PERFORM MC_PREENCHE_FIELDCAT USING:
"                                                      Tam  Sum ID
  01 'ICONE            ' '' '30' '' ''  ' '  'Status               ' '' ' ',
  02 'WERKS            ' '' '30' '' ''  ' '  'Centro               ' '' ' ',
  03 'AUFNR            ' '' '30' '' ''  'X'  'Ordem                ' '' 'X',
  04 'PLTXT            ' '' '30' '' ''  'X'  'Local Instalação     ' '' ' ',
  05 'EQUIPMENT        ' '' '30' '' ''  'X'  'Equipamento          ' '' 'X',
  06 'DESC_EQUIP       ' '' '30' '' ''  ' '  'Desc Eqto            ' '' ' ',
  07 'SOLICITANTE      ' '' '30' '' ''  ' '  'Solicitante          ' '' ' ',
  08 'DT_SOLICITACAO   ' '' '30' '' ''  ' '  'Data Solicitação     ' '' ' ',
  09 'USER4            ' '' '30' '' ''  ' '  'Orçamento Inicial    ' '' ' ',
* 10 'Suplementação    ' '' '30' '' ''  ' '  'Suplementado aprovada' '' ' ',
  11 'V_ESTIMADO       ' '' '30' '' ''  ' '  'Suplementação        ' '' ' ',
  12 'OBSERVACAO       ' '' '30' '' ''  ' '  'Texto Breve          ' '' ' ',
  13 'NIVEL_APROVADO   ' '' '30' '' ''  'X'  'Nivel Aprovado       ' '' ' ',
* 14 'RESPONSAVEL      ' '' '30' '' ''  ' '  'Aprovador            ' '' ' ',
* 15 'RESPONSAVEIS     ' '' '30' '' ''  ' '  'Aprovadores          ' '' ' ',
  16 'P_NIVEL          ' '' '30' '' ''  'X'  'Nivel pendente       ' '' ' ',
  17 'FUNCAO           ' '' '30' '' ''  ' '  'Desc Nivel           ' '' ' ',
  18 'NAPROVADOR       ' '' '30' '' ''  ' '  'Aprovador pendente   ' '' ' '.
* 19 'P_P_APROV        ' '' '30' '' ''  ' '  'Aprovadores          ' '' ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TEXT_CABECARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TEXT_CABECARIO .

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

  CLEAR: P_TEXT_TABLE.
  "------------------
  LOOP AT P_BUKRS.
    IF P_BUKRS-OPTION NE 'EQ' AND P_BUKRS-OPTION NE 'BT'.
      SDYDO_TEXT_ELEMENT = 'Empresa: Multiplas Seleções'.
      EXIT.
    ELSEIF P_BUKRS-OPTION EQ 'BT'.

      SELECT SINGLE BUTXT
        FROM T001
        INTO VL_BUTXT
        WHERE BUKRS EQ P_BUKRS-LOW
        AND SPRAS EQ SY-LANGU.

      CONCATENATE 'Empresa:' P_BUKRS-LOW VL_BUTXT '-' INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
      CLEAR: VL_BUTXT.

      SELECT SINGLE BUTXT
       FROM T001
       INTO VL_BUTXT
       WHERE BUKRS EQ P_BUKRS-HIGH
       AND SPRAS EQ SY-LANGU.

      CONCATENATE SDYDO_TEXT_ELEMENT P_BUKRS-HIGH VL_BUTXT INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.

      EXIT.
    ELSE.
      VL_CONT = VL_CONT + 1.
      IF VL_CONT GT 1.
        SDYDO_TEXT_ELEMENT = 'Empresa: Multiplas Seleções'.
      ELSE.

        SELECT SINGLE BUTXT
          FROM T001
          INTO VL_BUTXT
          WHERE BUKRS EQ P_BUKRS-LOW
          AND SPRAS EQ SY-LANGU.

        CONCATENATE 'Empresa:' P_BUKRS-LOW VL_BUTXT INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.

      ENDIF.
    ENDIF.
  ENDLOOP.
  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  CLEAR: VL_CONT, VL_BUTXT, SDYDO_TEXT_ELEMENT.


  "------------------
  IF P_WERKS IS NOT INITIAL.
    LOOP AT P_WERKS.
      IF P_WERKS-OPTION NE 'EQ' AND P_WERKS-OPTION NE 'BT'.
        SDYDO_TEXT_ELEMENT = 'Centro: Multiplas Seleções'.
        EXIT.
      ELSEIF P_WERKS-OPTION EQ 'BT'.
        CONCATENATE 'Centro:' P_WERKS-LOW '-' P_WERKS-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
        EXIT.
      ELSE.
        VL_CONT = VL_CONT + 1.
        IF VL_CONT GT 1.
          SDYDO_TEXT_ELEMENT = 'Centro: Multiplas Seleções'.
        ELSE.
          CONCATENATE 'Centro:' P_WERKS-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.
  ELSE.
    SDYDO_TEXT_ELEMENT = 'Centro:'.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  ENDIF.
  CLEAR: VL_CONT, SDYDO_TEXT_ELEMENT.


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

ENDFORM.
