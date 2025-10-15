*&---------------------------------------------------------------------*
*& Report  ZMEMO13                                                    &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Izyan Nascimento (Faneli)                               &*
*& Data.....: 12/03/2015                                              &*
*& Descrição: Relatório de notas planejadas                           &*
*& Transação: ZMEMO                                                   &*
*&--------------------------------------------------------------------&*

REPORT ZMEMO13.

TYPE-POOLS VRM.
*=============================================================================*
*TABELAS                                                                      *
*=============================================================================*
TABLES:ZNOM_REME_NOTAS.

*=============================================================================*
*Estrutura                                                                    *
*=============================================================================*
TYPES: BEGIN OF TY_SAIDA,
        DOCNUM        TYPE ZNOM_REME_NOTAS-DOCNUM,
        ITEM          TYPE ZNOM_REME_NOTAS-ITMNUM,
        QTD_NOTA      TYPE J_1BNFLIN-MENGE,
        UNI_M         TYPE J_1BNFLIN-MEINS,
        NOMEACAO      TYPE ZNOM_REME_NOTAS-ID_NOMEACAO_TRAN,
        NAVIO         TYPE ZNOM_TRANSPORTE-DS_NOME_TRANSPOR,
        ANO	          TYPE ZNOM_TRANSPORTE-NR_ANO,
        MES	          TYPE ZNOM_TRANSPORTE-NR_MES,
        PORTO	        TYPE ZNOM_TRANSPORTE-DS_PORTO,
        FILIAL        TYPE ZNOM_REME_NOTAS-ID_FILIAL,
        QTD_VINCULADA	TYPE ZNOM_REME_NOTAS-NR_QUANTIDADE,
        TOTAL         TYPE ZNOM_REME_NOTAS-NR_QUANTIDADE,

       END OF TY_SAIDA.


*=============================================================================*
*TABELA INTERNA                                                               *
*=============================================================================*
DATA: IT_ZNOM_REME_NOTAS TYPE TABLE OF ZNOM_REME_NOTAS,
      IT_ZNOM_TRANSPORTE TYPE TABLE OF ZNOM_TRANSPORTE,
      IT_J_1BNFLIN       TYPE TABLE OF J_1BNFLIN,
      IT_SAIDA           TYPE TABLE OF TY_SAIDA,
      IT_SAIDA_AUX       TYPE TABLE OF TY_SAIDA.
*=============================================================================*
*WORK AREA                                                                    *
*=============================================================================*
DATA: WA_ZNOM_REME_NOTAS TYPE ZNOM_REME_NOTAS,
      WA_ZNOM_TRANSPORTE TYPE ZNOM_TRANSPORTE,
      WA_J_1BNFLIN       TYPE J_1BNFLIN,
      WA_SAIDA           TYPE TY_SAIDA,
      WA_SAIDA_AUX       TYPE TY_SAIDA.

*=============================================================================*
*WORK AREA  TELA                                                              *
*=============================================================================*
DATA: WA_CONT        TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV         TYPE REF TO  CL_GUI_ALV_GRID,
      WA_LAYOUT      TYPE LVC_S_LAYO.

DATA: DG_DYNDOC_ID  TYPE REF TO CL_DD_DOCUMENT,
      WG_POS        TYPE I.

DATA: BEGIN OF GRAPHIC_TABLE OCCURS 0,
        LINE(255) TYPE X,
      END OF GRAPHIC_TABLE.

DATA: L_GRAPHIC_XSTR TYPE XSTRING,
      GRAPHIC_SIZE   TYPE I,
      L_GRAPHIC_CONV TYPE I,
      L_GRAPHIC_OFFS TYPE I.

*=============================================================================*
*Estrutura cabeçalho Alv                                                      *
*=============================================================================*
DATA: PICTURE          TYPE REF TO CL_GUI_PICTURE,
      GF_FIRST_DISPLAY TYPE C VALUE 'X',
      CTL_CCCONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DG_SPLITTER      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_SPLITTER_2    TYPE REF TO CL_GUI_SPLITTER_CONTAINER,

      DG_PARENT_HTML   TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_HTML1  TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_HTML2  TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_GRID   TYPE REF TO CL_GUI_CONTAINER,
      DG_HTML_CNTRL    TYPE REF TO CL_GUI_HTML_VIEWER,
      CTL_ALV_RESUMO   TYPE REF TO CL_GUI_ALV_GRID,

      IT_SORT          TYPE LVC_T_SORT,
      WA_SORT          TYPE LVC_S_SORT,

      GS_SCROLL_COL    TYPE LVC_S_COL,
      GS_SCROLL_ROW    TYPE LVC_S_ROID,
      GS_LAYOUT        TYPE LVC_S_LAYO,

      GS_VARIANT       TYPE DISVARIANT,
      IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS.


*=============================================================================*
*Estrutura Alv                                                                *
*=============================================================================*
DATA: IT_FCAT    TYPE TABLE OF LVC_S_FCAT,
      IT_LIST    TYPE VRM_VALUES,
      LIST_VALUE TYPE VRM_VALUES.

*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_EMP    FOR ZNOM_REME_NOTAS-ID_EMPRESA OBLIGATORY,
                S_FILIAL FOR ZNOM_REME_NOTAS-ID_FILIAL  OBLIGATORY.
PARAMETERS: P_DIF AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN: END OF BLOCK B1.

*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
START-OF-SELECTION.

  GF_FIRST_DISPLAY = 'X'.

  PERFORM: F_SELECIONAR_DADOS,               " Form selecionar dado
           F_ORGANIZAR_DADOS,                " ORGANIZAR DADOS
           F_ALV.                            "Saida ALV

  CALL SCREEN 0100.

*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*
FORM F_SELECIONAR_DADOS.

  SELECT *
    FROM ZNOM_REME_NOTAS
    INTO TABLE IT_ZNOM_REME_NOTAS
   WHERE ID_EMPRESA IN S_EMP
     AND ID_FILIAL  IN S_FILIAL
     AND DOCNUM NE ''
     AND ITMNUM NE ''.

  IF SY-SUBRC IS INITIAL.
    SELECT *
      INTO TABLE IT_ZNOM_TRANSPORTE
      FROM ZNOM_TRANSPORTE
      FOR ALL ENTRIES IN IT_ZNOM_REME_NOTAS
     WHERE ID_NOMEACAO_TRAN EQ IT_ZNOM_REME_NOTAS-ID_NOMEACAO_TRAN.

    SELECT *
      INTO TABLE IT_J_1BNFLIN
      FROM J_1BNFLIN
       FOR ALL ENTRIES IN IT_ZNOM_REME_NOTAS
     WHERE DOCNUM EQ IT_ZNOM_REME_NOTAS-DOCNUM
       AND ITMNUM EQ IT_ZNOM_REME_NOTAS-ITMNUM.

  ENDIF.

ENDFORM.                    "F_SELECIONAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS                                        *
*&---------------------------------------------------------------------*
FORM F_ORGANIZAR_DADOS.
  DATA: LV_MARCADOR TYPE C,
        LV_DOCNUM   TYPE ZNOM_REME_NOTAS-DOCNUM,
        LV_ITMNUM   TYPE ZNOM_REME_NOTAS-ITMNUM,
        LV_QTD_VIN  TYPE ZNOM_REME_NOTAS-NR_QUANTIDADE,
        LV_QTD_NOT  TYPE J_1BNFLIN-MENGE,
        wl_saida    like line of it_saida.

  SORT IT_ZNOM_REME_NOTAS BY DOCNUM ITMNUM NR_QUANTIDADE.

  LOOP AT IT_ZNOM_REME_NOTAS INTO WA_ZNOM_REME_NOTAS.

    WA_SAIDA-DOCNUM        = WA_ZNOM_REME_NOTAS-DOCNUM.
    WA_SAIDA-ITEM          = WA_ZNOM_REME_NOTAS-ITMNUM.
    WA_SAIDA-FILIAL        = WA_ZNOM_REME_NOTAS-ID_FILIAL.
    WA_SAIDA-QTD_VINCULADA = WA_ZNOM_REME_NOTAS-NR_QUANTIDADE.
    WA_SAIDA-NOMEACAO      = WA_ZNOM_REME_NOTAS-ID_NOMEACAO_TRAN.

    READ TABLE IT_ZNOM_TRANSPORTE INTO WA_ZNOM_TRANSPORTE
      WITH KEY ID_NOMEACAO_TRAN = WA_ZNOM_REME_NOTAS-ID_NOMEACAO_TRAN.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-ANO   = WA_ZNOM_TRANSPORTE-NR_ANO.
      WA_SAIDA-MES   = WA_ZNOM_TRANSPORTE-NR_MES.
      WA_SAIDA-PORTO = WA_ZNOM_TRANSPORTE-DS_PORTO.
      WA_SAIDA-NAVIO = WA_ZNOM_TRANSPORTE-DS_NOME_TRANSPOR.

    ENDIF.

    READ TABLE IT_J_1BNFLIN INTO WA_J_1BNFLIN
      WITH KEY DOCNUM = WA_ZNOM_REME_NOTAS-DOCNUM
               ITMNUM = WA_ZNOM_REME_NOTAS-ITMNUM.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-QTD_NOTA = WA_J_1BNFLIN-MENGE.
      WA_SAIDA-UNI_M    = WA_J_1BNFLIN-MEINS.

      CLEAR WL_SAIDA.

      READ TABLE IT_SAIDA INTO WL_SAIDA
        WITH KEY DOCNUM = WA_ZNOM_REME_NOTAS-DOCNUM
                 ITEM   = WA_ZNOM_REME_NOTAS-ITMNUM.
      IF SY-SUBRC IS INITIAL.
        ADD WA_SAIDA-QTD_VINCULADA TO WL_SAIDA-TOTAL.

        CLEAR: WA_SAIDA-QTD_NOTA.

        MODIFY IT_SAIDA FROM WL_SAIDA TRANSPORTING TOTAL WHERE DOCNUM = WA_ZNOM_REME_NOTAS-DOCNUM
                                                           AND ITEM   = WA_ZNOM_REME_NOTAS-ITMNUM.

      ELSE.
        WA_SAIDA-TOTAL = WA_SAIDA-QTD_VINCULADA.

      ENDIF.

    ENDIF.

    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR WA_SAIDA.

  ENDLOOP.

  IF P_DIF IS NOT INITIAL.
    LOOP AT IT_SAIDA INTO WA_SAIDA WHERE QTD_NOTA IS NOT INITIAL.
      IF WA_SAIDA-QTD_NOTA EQ WA_SAIDA-TOTAL.
        DELETE IT_SAIDA WHERE DOCNUM = WA_SAIDA-DOCNUM
                          AND ITEM   = WA_SAIDA-ITEM.

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.                    "F_ORGANIZAR_DADOS
*=============================================================================*
*Form F_Alv                                                                   *
*=============================================================================*
FORM F_ALV.
  PERFORM ALV_PREENCHE_CAT USING:
        'DOCNUM'         'Nº Documento'           '15'  ''  ''   ''   ''  ,
        'ITEM'           'Item'                   '08'  ''  ''   ''   ''  ,
        'QTD_NOTA'       'Qtd. Nota'              '15'  ''  ''   'x'  ''  ,
        'UNI_M'          'Um.'                    '04'  ''  ''   ''   ''  ,
        'NOMEACAO'       'Nomeação'               '12'  ''  ''   ''   ''  ,
        'NAVIO'          'Navio'                  '30'  ''  ''   ''   ''  ,
        'ANO'            'Ano'                    '06'  ''  ''   ''   ''  ,
        'MES'            'Mês'                    '04'  ''  ''   ''   ''  ,
        'PORTO'          'Porto'                  '30'  ''  ''   ''   ''  ,
        'FILIAL'         'Filial'                 '06'  ''  ''   ''   ''  ,
        'QTD_VINCULADA'  'Qtd. Vinculada'         '15'  ''  ''   'x'  ''  .


ENDFORM.    "F_ALV

"&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT   USING   P_CAMPO  TYPE C
                                P_DESC   TYPE C
                                P_TAM    TYPE C
                                P_HOT    TYPE C
                                P_ZERO   TYPE C
                                P_SUM    TYPE C
                                P_COR    TYPE C .
  DATA: WL_FCAT TYPE LVC_S_FCAT.

  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SCRTEXT_L = P_DESC.
  WL_FCAT-SCRTEXT_M = P_DESC.
  WL_FCAT-SCRTEXT_S = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-DO_SUM    = P_SUM.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-EMPHASIZE = P_COR.

  APPEND WL_FCAT TO IT_FCAT.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT .

  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = SPACE.
  GS_VARIANT-LOG_GROUP   = SPACE.
  GS_VARIANT-USERNAME    = SPACE.
  GS_VARIANT-VARIANT     = SPACE.
  GS_VARIANT-TEXT        = SPACE.
  GS_VARIANT-DEPENDVARS  = SPACE.

ENDFORM.                    " FILL_GS_VARIANT

"&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0100 INPUT.
  IF SY-DYNNR EQ '0100'.
    CASE SY-UCOMM.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " PAI_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CONTAINER_HTML .

  DATA : DL_LENGTH  TYPE I,                           " Length
         DL_BACKGROUND_ID TYPE SDYDO_KEY VALUE SPACE. " Background_id

  IF DG_HTML_CNTRL IS INITIAL.
    CREATE OBJECT DG_HTML_CNTRL
      EXPORTING
        PARENT = DG_PARENT_HTML1.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      DOCUMENT = DG_DYNDOC_ID
      BOTTOM   = SPACE
    IMPORTING
      LENGTH   = DL_LENGTH.

  CALL METHOD DG_DYNDOC_ID->MERGE_DOCUMENT.

  CALL METHOD DG_DYNDOC_ID->SET_DOCUMENT_BACKGROUND
    EXPORTING
      PICTURE_ID = DL_BACKGROUND_ID.

  DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.

  CALL METHOD DG_DYNDOC_ID->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = DG_PARENT_HTML1
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.

ENDFORM.                    " CONTAINER_HTML

*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       To add Text
*----------------------------------------------------------------------*
FORM ADD_TEXT USING P_TEXT  TYPE SDYDO_TEXT_ELEMENT
                    P_STYLE TYPE SDYDO_ATTRIBUTE
                    P_SIZE  TYPE SDYDO_ATTRIBUTE
                    P_COLOR TYPE SDYDO_ATTRIBUTE.

  CALL METHOD DG_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT          = P_TEXT
      SAP_STYLE     = P_STYLE
      SAP_FONTSIZE  = P_SIZE
      SAP_COLOR     = P_COLOR
      SAP_FONTSTYLE = CL_DD_AREA=>SANS_SERIF.

ENDFORM.                    " ADD_TEXT


*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECTS OUTPUT.
  DATA: URL(255) TYPE C.

  IF GF_FIRST_DISPLAY = 'X'.
    CREATE OBJECT CTL_CCCONTAINER
      EXPORTING
        CONTAINER_NAME = 'TELA_0100'.

    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.

    CREATE OBJECT DG_SPLITTER
      EXPORTING
        PARENT  = CTL_CCCONTAINER
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
        CONTAINER = DG_PARENT_GRID.

    CALL METHOD DG_SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 15.

    CREATE OBJECT CTL_ALV_RESUMO
      EXPORTING
        I_PARENT = DG_PARENT_GRID.

    PERFORM FILL_GS_VARIANT.

    "GS_LAYOUT-SEL_MODE = 'A'.
    GS_LAYOUT-ZEBRA      = 'X'.
    "GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.

    PERFORM F_SORT.

**   Send data to ALV grid
    CALL METHOD CTL_ALV_RESUMO->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = WA_LAYOUT
        IS_VARIANT           = GS_VARIANT
        I_SAVE               = 'A'
*        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
      CHANGING
        IT_FIELDCATALOG      = IT_FCAT
        IT_OUTTAB            = IT_SAIDA
        IT_SORT              = IT_SORT.

    PERFORM CRIA_HTML_CAB.

    CALL METHOD CTL_ALV_RESUMO->LIST_PROCESSING_EVENTS
      EXPORTING
        I_EVENT_NAME = 'TOP_OF_PAGE'
        I_DYNDOC_ID  = DG_DYNDOC_ID.

    CLEAR: GF_FIRST_DISPLAY.

  ENDIF.

  CALL METHOD CTL_ALV_RESUMO->REFRESH_TABLE_DISPLAY.

  CALL METHOD CTL_ALV_RESUMO->SET_SCROLL_INFO_VIA_ID
    EXPORTING
      IS_COL_INFO = GS_SCROLL_COL
      IS_ROW_NO   = GS_SCROLL_ROW.

ENDMODULE.                 " CREATE_OBJECTS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_HTML_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIA_HTML_CAB .

  DATA: COLUMN         TYPE REF TO CL_DD_AREA,
        COLUMN_1       TYPE REF TO CL_DD_AREA,
        COLUMN_2       TYPE REF TO CL_DD_AREA,
        TABLE_ELEMENT  TYPE REF TO CL_DD_TABLE_ELEMENT,
        TABLE_ELEMENT2 TYPE REF TO CL_DD_TABLE_ELEMENT,
        P_TEXT         TYPE SDYDO_TEXT_ELEMENT,
        P_TEXT_TABLE   TYPE SDYDO_TEXT_TABLE,
        SDYDO_TEXT_ELEMENT(255),
        VG_MES(2), VG_ANO(4),
        QTD TYPE I.

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

  P_TEXT = 'Relatório de notas planejadas'.
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
    IMPORTING
      COLUMN = COLUMN_1.

  CALL METHOD TABLE_ELEMENT2->ADD_COLUMN
    IMPORTING
      COLUMN = COLUMN_2.

  CALL METHOD TABLE_ELEMENT2->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 1
      SAP_ALIGN    = 'LEFT'
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM.

  CALL METHOD TABLE_ELEMENT2->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 2
      SAP_ALIGN    = 'LEFT'
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM.

  IF S_EMP IS NOT INITIAL.
    SDYDO_TEXT_ELEMENT = 'Empresa:'.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  ENDIF.

  IF S_EMP IS NOT INITIAL.
    SDYDO_TEXT_ELEMENT = 'Filial: '.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  ENDIF.

  CALL METHOD COLUMN_1->ADD_TEXT
    EXPORTING
      TEXT_TABLE = P_TEXT_TABLE
      FIX_LINES  = 'X'.

  CLEAR: P_TEXT_TABLE, SDYDO_TEXT_ELEMENT.

  "********

  "EMPRESA *********
  SDYDO_TEXT_ELEMENT = S_EMP-LOW.
  IF S_EMP-HIGH IS NOT INITIAL.
    CONCATENATE SDYDO_TEXT_ELEMENT ' - '  INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
    CONCATENATE SDYDO_TEXT_ELEMENT '  ' S_EMP-HIGH INTO SDYDO_TEXT_ELEMENT.
  ENDIF.

  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  "FILIAL *********
  SDYDO_TEXT_ELEMENT = S_FILIAL-LOW.
  IF S_FILIAL-HIGH IS NOT INITIAL.
    CONCATENATE SDYDO_TEXT_ELEMENT ' - ' INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
    CONCATENATE SDYDO_TEXT_ELEMENT S_FILIAL-HIGH INTO SDYDO_TEXT_ELEMENT.
  ENDIF.

  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

  CALL METHOD COLUMN_2->ADD_TEXT
    EXPORTING
      TEXT_TABLE = P_TEXT_TABLE
      FIX_LINES  = 'X'.

  PERFORM CONTAINER_HTML.

ENDFORM.                    " CRIA_HTML_CAB

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
FORM F_PEGA_IMAGEM  USING    NOME_LOGO
                    CHANGING URL.

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
*&      Form  f_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

DEFINE MC_PREENCHE_CLASS.
  ADD 1 TO WG_POS.

  CLEAR WA_SORT.
  WA_SORT-SPOS      = WG_POS.
  WA_SORT-FIELDNAME = &1.
  WA_SORT-GROUP     = &2.
  IF &3 = 'D'.
    WA_SORT-DOWN    = 'X'.
  ELSE.
    WA_SORT-UP      = &3.
  ENDIF.
  WA_SORT-SUBTOT    = &4.
  APPEND WA_SORT TO IT_SORT.

END-OF-DEFINITION.


*&---------------------------------------------------------------------*
*&      Form  F_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_SORT.
  FREE IT_SORT.
  MC_PREENCHE_CLASS:  'DOCNUM'          ''   'X'   'X'.
*                      'ITEM'            ''   'X'   'X',
*                      'QTD_NOTA'        ''   'D'   'X',
*                      'UNI_M'           ''   'X'   '',
*                      'NOMEACAO'        ''   'X'   '',
*                      'NAVIO'           ''   'X'   '',
*                      'ANO'             ''   'X'   '',
*                      'MES'             ''   'X'   '',
*                      'PORTO'           ''   'X'   '',
*                      'FILIAL'          ''   'X'   '',
*                      'QTD_VINCULADA'   ''   'X'   ''.

ENDFORM.                    "f_sort
