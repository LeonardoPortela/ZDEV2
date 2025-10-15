*----------------------------------------------------------------------*
***INCLUDE LZLES_AVERBACAO_SEGUROO02.
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0002 OUTPUT.

  SET PF-STATUS 'PF0002'.
  SET TITLEBAR 'TL0002'.

  IF DG_SPLITTER IS INITIAL.

    CREATE OBJECT DG_SPLITTER
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_HTML.

    CREATE OBJECT DG_SPLITTER_2
      EXPORTING
        PARENT  = DG_PARENT_HTML
        ROWS    = 1
        COLUMNS = 2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_HTML1.

    CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
      EXPORTING
        ID    = 1
        WIDTH = 40.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 2
      RECEIVING
        CONTAINER = DG_PARENT_HTML2.

    CREATE OBJECT PICTURE
      EXPORTING
        PARENT = DG_PARENT_HTML2.

    PERFORM F_PEGA_IMAGEM USING 'LOGO_NOVO' CHANGING URL.

    CALL METHOD PICTURE->LOAD_PICTURE_FROM_URL
      EXPORTING
        URL = URL.

    CALL METHOD PICTURE->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE->DISPLAY_MODE_FIT_CENTER.

    CALL METHOD DG_SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_ALV.

    CALL METHOD DG_SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 16.

    CREATE OBJECT G_ALV
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    PERFORM FILL_IT_FIELDCATALOG_0002.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT_0002.

*   Set layout parameters for ALV grid
    "GS_LAYOUT-GRID_TITLE = TEXT-100.
    GS_LAYOUT-SEL_MODE   = 'A'.

    CALL METHOD G_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_ZLEST0145[].

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
        SAP_ALIGN = 'CENTER'
        SAP_STYLE = CL_DD_DOCUMENT=>HEADING.

    P_TEXT = TEXT-008.

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

    "SDYDO_TEXT_ELEMENT = ''.
    "APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

    CALL METHOD COLUMN_1->ADD_TEXT
      EXPORTING
        TEXT_TABLE = P_TEXT_TABLE
        FIX_LINES  = 'X'.

    CALL METHOD TABLE_ELEMENT2->ADD_COLUMN
      IMPORTING
        COLUMN = COLUMN_2.

    CALL METHOD TABLE_ELEMENT2->SET_COLUMN_STYLE
      EXPORTING
        COL_NO       = 2
        SAP_ALIGN    = 'LEFT'
        SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM.

    CLEAR: P_TEXT_TABLE.

*PBUKRS	Sociedad
    "SDYDO_TEXT_ELEMENT = 'Departamentos NF-e Inbound'.
    "APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

    CALL METHOD COLUMN_2->ADD_TEXT
      EXPORTING
        TEXT_TABLE = P_TEXT_TABLE
        FIX_LINES  = 'X'.

    CALL METHOD DG_DYNDOC_ID->MERGE_DOCUMENT.

    CREATE OBJECT DG_HTML_CNTRL
      EXPORTING
        PARENT = DG_PARENT_HTML1.

    DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.

    CALL METHOD DG_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = DG_PARENT_HTML1
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.

  ENDIF.

  CALL METHOD G_ALV->REFRESH_TABLE_DISPLAY.

  CALL METHOD G_ALV->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL
      ES_ROW_NO   = GS_SCROLL_ROW.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0002
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0002 .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZLEST0145'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

ENDFORM.                    " FILL_IT_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0002
*&---------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0002 .

  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = '0002'.
  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT-USERNAME    = ABAP_FALSE.
  GS_VARIANT-VARIANT     = ABAP_FALSE.
  GS_VARIANT-TEXT        = ABAP_FALSE.
  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Form  CADASTRO_TOKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CADASTRO_TOKEN .

  CK_NOVO_TOKEN = ABAP_FALSE.
  CK_ALTEROU_TOKEN = ABAP_FALSE.
  OBJ_TOKEN->GET_REGISTRO( IMPORTING E_REGISTRO = WA_ZLEST0145 ).

  IF WA_ZLEST0145-CD_TOKEN IS INITIAL.
    CK_NOVO_TOKEN = ABAP_TRUE.
  ELSE.
    MOVE WA_ZLEST0145 TO ZLEST0145.
  ENDIF.

  CALL SCREEN 2000 STARTING AT 50 01.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_TOKEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_TOKEN INPUT.
  CK_ALTEROU_TOKEN = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2000_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_2000 OUTPUT.

  CLEAR: IT_UCOMM.

  IF CK_CONSULTA EQ ABAP_TRUE.
    APPEND OK_GRAVAR TO IT_UCOMM.
  ENDIF.

  SET PF-STATUS 'PF2000' EXCLUDING IT_UCOMM.

  IF CK_CONSULTA = ABAP_TRUE.
    SET TITLEBAR 'TL2000' WITH TEXT-001.
  ELSE.
    DATA(LC_TOKEN) = OBJ_TOKEN->GET_TOKEN( ).

    IF LC_TOKEN-CD_TOKEN IS INITIAL.
      SET TITLEBAR 'TL2000' WITH TEXT-003.
    ELSE.
      SET TITLEBAR 'TL2000' WITH TEXT-002.
    ENDIF.
  ENDIF.

  IF CK_ALTEROU_TOKEN EQ ABAP_TRUE.
    CK_ALTEROU_TOKEN = ABAP_FALSE.
  ENDIF.

  IF CK_CONSULTA EQ ABAP_TRUE.
    LOOP AT SCREEN .
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-NAME(09) EQ 'ZLEST0145'.
        SPLIT SCREEN-NAME AT '-' INTO DATA(STR1) DATA(STR2).
        I_CAMPO = STR2.
        IF OBJ_TOKEN->VALIDA_ATRIBUTO_ALTERAVEL( EXPORTING I_CAMPO = I_CAMPO ) EQ ABAP_TRUE.
          SCREEN-INPUT = 1.
        ELSE.
          SCREEN-INPUT = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2000 INPUT.

  CHECK CK_ALTEROU_TOKEN IS INITIAL.

  CASE OK_CODE.
    WHEN 'GRAVAR'.
      CLEAR: OK_CODE.
      IF OBJ_TOKEN->GRAVAR_REGISTRO( ) EQ ABAP_TRUE.
        CK_GRAVADO = ABAP_TRUE.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_ZLEST0145_BUKRS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATRIBUIR_ZLEST0145_BUKRS INPUT.
  OBJ_TOKEN->SET_BUKRS( I_BUKRS = ZLEST0145-BUKRS ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_ZLEST0145_BRANCH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATRIBUIR_ZLEST0145_BRANCH INPUT.
  OBJ_TOKEN->SET_BRANCH( I_BRANCH = ZLEST0145-BRANCH ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_ZLEST0145_NR_TOKEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATRIBUIR_ZLEST0145_NR_TOKEN INPUT.
  OBJ_TOKEN->SET_NR_TOKEN( I_NR_TOKEN = ZLEST0145-NR_TOKEN ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_ZLEST0145_DT_VALIDADE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATRIBUIR_ZLEST0145_DT_VALIDADE INPUT.
  OBJ_TOKEN->SET_DT_VALIDADE( I_DT_VALIDADE = ZLEST0145-DT_VALIDADE ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002 INPUT.

  CASE OK_CODE.
    WHEN 'ATUALIZAR'.
      CLEAR: OK_CODE.
      PERFORM ATUALIZAR_TOKEN.
    WHEN 'PNOVO'.
      CLEAR: OK_CODE.
      PERFORM NOVO_TOKEN.
    WHEN 'PABRIR'.
      CLEAR: OK_CODE.
      PERFORM ABRIR_TOKEN.
    WHEN 'PEDITAR'.
      CLEAR: OK_CODE.
      PERFORM EDITAR_TOKEN.
    WHEN 'PEXCLUIR'.
      CLEAR: OK_CODE.
      PERFORM EXCLUIR_TOKEN.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_TOKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZAR_TOKEN .

  DATA: OBJETO TYPE REF TO ZCL_AVERBACAO_SEGURO_TOKEN.

  CREATE OBJECT OBJETO.

  OBJETO->ZIF_PESQUISA~PESQUISAR( EXPORTING I_FILTROS = LC_FILTRO_45 IMPORTING E_REGISTROS = IT_ZLEST0145 ).

  CLEAR: OBJETO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  NOVO_TOKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM NOVO_TOKEN .

  DATA: I_GRAVOU TYPE CHAR01.

  CALL FUNCTION 'ZLES_CADASTRO_AVSEG_TOKEN_CAD'
    EXPORTING
      I_CONSULTA = ABAP_FALSE
    IMPORTING
      I_GRAVOU   = I_GRAVOU.

  IF I_GRAVOU EQ ABAP_TRUE.
    PERFORM ATUALIZAR_TOKEN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ABRIR_TOKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ABRIR_TOKEN .

  IF IT_ZLEST0145_SEL[] IS INITIAL.
    MESSAGE S015.
    RETURN.
  ENDIF.

  READ TABLE IT_ZLEST0145_SEL INDEX 1 INTO DATA(WA_ZLEST0145_SEL).

  CALL FUNCTION 'ZLES_CADASTRO_AVSEG_TOKEN_CAD'
    EXPORTING
      I_CONSULTA = ABAP_TRUE
      I_CD_TOKEN = WA_ZLEST0145_SEL-CD_TOKEN.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_TOKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EDITAR_TOKEN .

  DATA: I_GRAVOU TYPE CHAR01.

  IF IT_ZLEST0145_SEL[] IS INITIAL.
    MESSAGE S015.
    RETURN.
  ENDIF.

  READ TABLE IT_ZLEST0145_SEL INDEX 1 INTO DATA(WA_ZLEST0145_SEL).

  CALL FUNCTION 'ZLES_CADASTRO_AVSEG_TOKEN_CAD'
    EXPORTING
      I_CONSULTA = ABAP_FALSE
      I_CD_TOKEN = WA_ZLEST0145_SEL-CD_TOKEN
    IMPORTING
      I_GRAVOU   = I_GRAVOU.

  IF I_GRAVOU EQ ABAP_TRUE.
    PERFORM ATUALIZAR_TOKEN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_TOKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUIR_TOKEN .

  DATA: OBJ_EXCLUIR TYPE REF TO ZCL_AVERBACAO_SEGURO_TOKEN,
        P_EXCLUIU   TYPE C LENGTH 1.

  DATA: ANSWER TYPE C LENGTH 1.

  IF IT_ZLEST0145_SEL[] IS INITIAL.
    MESSAGE S015.
    RETURN.
  ENDIF.

  READ TABLE IT_ZLEST0145_SEL INDEX 1 INTO DATA(WA_ZLEST0145_SEL).

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      TITEL     = TEXT-009
      TEXTLINE1 = TEXT-010
      TEXTLINE2 = TEXT-011
    IMPORTING
      ANSWER    = ANSWER.

  CASE ANSWER.
    WHEN 'J'.
      CREATE OBJECT OBJ_EXCLUIR.
      OBJ_EXCLUIR->SET_REGISTRO( I_ID_REGISTRO = WA_ZLEST0145_SEL-CD_TOKEN ).
      P_EXCLUIU = OBJ_EXCLUIR->EXCLUIR_REGISTRO( ).

      CLEAR: OBJ_EXCLUIR.

      IF P_EXCLUIU EQ ABAP_TRUE.
        PERFORM ATUALIZAR_TOKEN.
      ENDIF.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_0002 INPUT.


  CALL METHOD G_ALV->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL
      ES_ROW_NO   = GS_SCROLL_ROW.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_0002 INPUT.

  CLEAR IT_SELECTED_ROWS.

  CALL METHOD G_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  CLEAR IT_ZLEST0145_SEL[].

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_ZLEST0145 INTO WA_ZLEST0145 INDEX WA_SELECTED_ROWS-INDEX.
    APPEND WA_ZLEST0145 TO IT_ZLEST0145_SEL.
  ENDLOOP.

ENDMODULE.
