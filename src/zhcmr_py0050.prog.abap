*&---------------------------------------------------------------------*
*& Report  ZHCMR_PY0050
*&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Relatório status áreas folha de pagamento               *
* Transação..: ZLES                                                    *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data    | Nome      | Request | Descrição                            *
*----------------------------------------------------------------------*
* 10.11.20|JALEXANDRE |         | Relatório status áreas folha de pagto*
*----------------------------------------------------------------------*
REPORT ZHCMR_PY0050.

TABLES: T569V, T549U, T549Q, DD07L.

*----------------------------------------------------------------------*
* Declaração detipos
*----------------------------------------------------------------------*
TYPES: BEGIN OF Y_SAIDA,
         ABKRS          TYPE T569V-ABKRS,
         ATEXT          TYPE T549T-ATEXT,
         STATE_TXT      TYPE DD07T-DDTEXT,
         PABRP_PABRJ(7) TYPE C, "Juntar os dois campos
         BEGDA(10)      TYPE C, "t549q-begda,
         ENDDA(10)      TYPE C, "t549q-endda,
         SRTFD          TYPE T569V-SRTFD,
         UABRP_UABRJ(7) TYPE C, "Juntar os dois campos
         UNAME          TYPE T569U-UNAME,
         AEDAT(10)      TYPE C, "t569u-aedat,
         UZEIT(10)      TYPE C, "t569u-uzeit,
         STATE_U        TYPE DD07T-DDTEXT,
       END OF Y_SAIDA.

TYPES: BEGIN OF Y_INTERVALO,
         DATA TYPE SY-DATUM,
         HORA TYPE SY-UZEIT,
       END OF Y_INTERVALO.

TYPES: BEGIN OF Y_FILTROS,
         PARAMETRO  TYPE STRING,
         VALOR      TYPE STRING,
         DIREITA    TYPE STRING,
         PARAMETRO2 TYPE STRING,
         VALOR2     TYPE STRING,
       END OF Y_FILTROS.

*----------------------------------------------------------------------*
* Declaração de tabela Interna
*----------------------------------------------------------------------*
DATA: T_SAIDA          TYPE TABLE OF Y_SAIDA,
      T_INTERVALO      TYPE TABLE OF Y_INTERVALO,
      T_FCAT           TYPE LVC_T_FCAT,
      T_FILTRO         TYPE TABLE OF Y_FILTROS,
      GOB_GUI_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.
*----------------------------------------------------------------------*
* CLASS DEFINITION
*----------------------------------------------------------------------*
CLASS       LCL_EVENT_RECEIVER DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS : HANDLE_HOTSPOT_CLICK
                FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID E_COLUMN_ID.

    METHODS : HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
      IMPORTING E_OBJECT
                E_INTERACTIVE.


ENDCLASS .                    "LCL_EVENT_RECEIVER DEFINITION

*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.


*-----Logic to handle the HOTSPOT click
  METHOD HANDLE_HOTSPOT_CLICK.
*---To handel hotspot in the firstlist
*    PERFORM zf_handle_hotspot_click USING e_row_id-index e_column_id.
  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD HANDLE_TOOLBAR.
    PERFORM ZF_ELIMINA_BOTOES_HEADER  USING E_OBJECT.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
*&---------------------------------------------------------------------*
* Declaração de Variáveis
*&---------------------------------------------------------------------*
DATA: GO_ALV           TYPE REF TO CL_SALV_TABLE,
      V_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.      "#EC NEEDED

*----------------------------------------------------------------------*
* Parâmetros de seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_ABKRS FOR T569V-ABKRS.
SELECTION-SCREEN: END OF BLOCK BL1.

*----------------------------------------------------------------------*
*START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM ZF_BUSCAR_REGISTROS.

*----------------------------------------------------------------------*
* Mostrar tabela de Saída                                              *
*----------------------------------------------------------------------*

  PERFORM ZF_ALV.


*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_REGISTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_BUSCAR_REGISTROS .

  DATA: R_SRTFD   TYPE RANGE OF  T569V-SRTFD,
        R_PERMO   TYPE RANGE OF T549Q-PERMO,
        R_DOMNAME TYPE RANGE OF DD07T-DOMNAME.

  REFRESH: R_SRTFD, R_PERMO, R_DOMNAME.
  "Regs.de controle
  R_SRTFD = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = '01' ) ).

  SELECT * FROM T549A
    INTO TABLE @DATA(T_T549A)
    WHERE ABKRS IN @S_ABKRS.

  IF T_T549A[] IS NOT INITIAL.

    SELECT * FROM T569V
      INTO TABLE @DATA(T_T569V)
      FOR ALL ENTRIES IN @T_T549A
      WHERE ABKRS = @T_T549A-ABKRS.

  ENDIF.

  IF T_T569V[] IS NOT INITIAL.

    "Áreas de processamento da folha de pagamento
    SELECT * FROM T549T
      INTO TABLE @DATA(T_T549T)
      FOR ALL ENTRIES IN @T_T569V
      WHERE SPRSL = 'P'
        AND ABKRS = @T_T569V-ABKRS.

    "Parâmetros período
    R_PERMO = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = '01' ) ).

    "Períods.proc.FlhPag.
    SELECT * FROM T549Q
      INTO TABLE @DATA(T_T549Q)
      FOR ALL ENTRIES IN @T_T569V
      WHERE PERMO IN @R_PERMO
        AND PABRJ = @T_T569V-PABRJ
        AND PABRP = @T_T569V-PABRP.

    "Logs registros de controle
    SELECT * FROM T569U
      INTO TABLE @DATA(T_T569U)
      FOR ALL ENTRIES IN @T_T569V
       WHERE ABKRS = @T_T569V-ABKRS.

    SORT T_T569U BY ABKRS AEDAT UZEIT SRTFD  DESCENDING.

  ENDIF.

  R_DOMNAME = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = 'VWSTA' ) ).

  SELECT * FROM DD07T
    INTO TABLE @DATA(T_DD07L)
    WHERE DOMNAME IN @R_DOMNAME
      AND DDLANGUAGE = 'P'.

*----------------------------------------------------------------------*
* Montar tabela de saída
*----------------------------------------------------------------------*
  DATA: W_SAIDA LIKE LINE OF T_SAIDA.

  LOOP AT T_T569V INTO DATA(W_T569V).

    MOVE-CORRESPONDING W_T569V TO W_SAIDA.

    CONCATENATE W_T569V-UABRP W_T569V-UABRJ INTO W_SAIDA-UABRP_UABRJ "Juntar os dois campos
      SEPARATED BY '/'.

    CASE W_T569V-STATE.
      WHEN 1.
        W_SAIDA-STATE_TXT = 'Lvr.p/CálcFlhsPgto.' .
      WHEN 2.
        W_SAIDA-STATE_TXT = 'Livre p/ correção' .
      WHEN 3.
        W_SAIDA-STATE_TXT = 'Fim cálc.flhs.pgto.' .
      WHEN 4.
        W_SAIDA-STATE_TXT = 'Verif.results.cálc.flh.pgto' .
      WHEN OTHERS.
    ENDCASE.

    READ TABLE T_T549T INTO DATA(W_T549T) WITH KEY ABKRS = W_T569V-ABKRS.
    IF SY-SUBRC IS INITIAL.
      W_SAIDA-ATEXT  =  W_T549T-ATEXT.
    ENDIF.

    READ TABLE T_T549Q INTO DATA(W_T549Q) WITH KEY PABRJ = W_T569V-PABRJ
                                                   PABRP = W_T569V-PABRP.

    IF SY-SUBRC IS INITIAL.

      CONCATENATE W_T549Q-PABRP W_T549Q-PABRJ INTO W_SAIDA-PABRP_PABRJ "Juntar os dois campos
        SEPARATED BY '/'.

      WRITE W_T549Q-BEGDA TO W_SAIDA-BEGDA USING EDIT MASK '__/__/____'.
      WRITE W_T549Q-ENDDA TO W_SAIDA-ENDDA USING EDIT MASK '__/__/____'.

    ENDIF.

    READ TABLE T_T569U INTO DATA(W_T569U) WITH KEY ABKRS = W_T569V-ABKRS.
    IF SY-SUBRC IS INITIAL.
      W_SAIDA-UNAME      = W_T569U-UNAME.

      CONVERT INVERTED-DATE W_T569U-AEDAT INTO DATE W_T569U-AEDAT.
      CONVERT INVERTED-DATE W_T569U-UZEIT INTO DATE W_T569U-UZEIT.

      WRITE W_T569U-AEDAT TO W_SAIDA-AEDAT USING EDIT MASK '__/__/____'.
      WRITE W_T569U-UZEIT TO W_SAIDA-UZEIT USING EDIT MASK '__:__:____'.

      READ TABLE T_DD07L INTO DATA(W_DD07L) WITH KEY DOMVALUE_L    = W_T569U-STATE.
      IF SY-SUBRC IS INITIAL.
        W_SAIDA-STATE_U = W_DD07L-DDTEXT.
      ENDIF.

    ENDIF.

    APPEND W_SAIDA TO T_SAIDA.
    CLEAR: W_SAIDA, W_T569V, W_T549Q, W_T569U, W_DD07L.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ALV .

  IF T_SAIDA IS NOT INITIAL.
    PERFORM ZF_FILTROS.
    CALL SCREEN 0100.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  PERFORM ZF_CRIAR_OBJETOS .
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_CRIAR_OBJETOS .

  DATA: W_VARIANT TYPE DISVARIANT,
        W_LAYOUT  TYPE LVC_S_LAYO.

  W_LAYOUT-CWIDTH_OPT = 'X'.
  W_LAYOUT-ZEBRA      = 'X'.
  W_LAYOUT-SEL_MODE   = 'A'.

  W_LAYOUT-INFO_FNAME = 'COLOR_LINE'.
  W_LAYOUT-CTAB_FNAME = 'COLOR_CELL'.

  W_VARIANT-REPORT   = SY-REPID.
*  w_variant-variant  = p_layout.

  IF GOB_GUI_ALV_GRID IS INITIAL.

    PERFORM ZF_SPLIT_SCREEN         CHANGING SY-TITLE GOB_GUI_ALV_GRID.
    PERFORM FM_CRIA_FIELDCAT        CHANGING T_SAIDA T_FCAT.
    PERFORM ZF_AJUSTAR_DESCR_CAMPOS CHANGING T_FCAT.

    W_LAYOUT-CWIDTH_OPT = 'X'.
    W_LAYOUT-ZEBRA      = 'X'.

*----------------------------------------------------------------------*
*** Define eventos
*----------------------------------------------------------------------*
    CREATE OBJECT V_EVENT_RECEIVER.
    SET HANDLER V_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR GOB_GUI_ALV_GRID.

    SET HANDLER V_EVENT_RECEIVER->HANDLE_TOOLBAR FOR GOB_GUI_ALV_GRID.

    CALL METHOD GOB_GUI_ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT                    = W_VARIANT
        I_SAVE                        = 'A'
        IS_LAYOUT                     = W_LAYOUT
      CHANGING
        IT_OUTTAB                     = T_SAIDA
        IT_FIELDCATALOG               = T_FCAT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SAIDA  text
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM FM_CRIA_FIELDCAT CHANGING PT_TABELA   TYPE ANY TABLE
                                  PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA:
    L_COLUMNS      TYPE REF TO CL_SALV_COLUMNS_TABLE,
    L_AGGREGATIONS TYPE REF TO CL_SALV_AGGREGATIONS,
    L_SALV_TABLE   TYPE REF TO CL_SALV_TABLE,
    L_DATA         TYPE REF TO DATA.
  FIELD-SYMBOLS:
    <F_TABLE>      TYPE STANDARD TABLE.

* Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA L_DATA LIKE PT_TABELA.
  ASSIGN L_DATA->* TO <F_TABLE>.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

* Monta a estrutura dinâmica no objeto l_salv_table
  TRY.
      CL_SALV_TABLE=>FACTORY(
        EXPORTING
          LIST_DISPLAY = ABAP_FALSE
        IMPORTING
          R_SALV_TABLE = L_SALV_TABLE
        CHANGING
          T_TABLE      = <F_TABLE> ).
    CATCH CX_SALV_MSG.                                  "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* Recupera as colunas e dados internos
  L_COLUMNS      = L_SALV_TABLE->GET_COLUMNS( ).
  L_AGGREGATIONS = L_SALV_TABLE->GET_AGGREGATIONS( ).

* Monta o fieldcat
  PT_FIELDCAT = CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG( R_COLUMNS      = L_COLUMNS
                                                                   R_AGGREGATIONS = L_AGGREGATIONS ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_FILTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_FILTROS .

  FREE: T_FILTRO.

  LOOP AT SCREEN.
    T_FILTRO = VALUE #(
      ( PARAMETRO = ' ' VALOR = '' )
    ).
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINA_BOTOES_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM ZF_ELIMINA_BOTOES_HEADER    USING  E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET.

*    elimina itens desnecessarios da barra do container
  DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION = '&LOCAL&APPEND'
                                 OR FUNCTION = '&LOCAL&INSERT_ROW'
                                 OR FUNCTION = '&LOCAL&DELETE_ROW'
                                 OR FUNCTION = '&LOCAL&COPY_ROW'
                                 OR FUNCTION = '&LOCAL&CUT'
                                 OR FUNCTION = '&LOCAL&COPY'
                                 OR FUNCTION = '&LOCAL&PASTE'
                                 OR FUNCTION = '&REFRESH'
                                 OR FUNCTION = '&CHECK'
                                 OR FUNCTION = '&GRAPH'
                                 OR FUNCTION = '&INFO'
                                 OR FUNCTION = '&LOCAL&UNDO'
                                 OR FUNCTION = '&MB_VIEW'
*                                 OR function = '&MB_VARIANT'
*                                 OR function =  '&MB_EXPORT'
                                 OR FUNCTION =  '&MB_SUM'
                                 OR FUNCTION =  '&MB_SUBTOT'
                                 OR FUNCTION =  '&PRINT_BACK'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTAR_DESCR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FCAT  text
*----------------------------------------------------------------------*
FORM ZF_AJUSTAR_DESCR_CAMPOS CHANGING PT_FCAT TYPE LVC_T_FCAT.

  LOOP AT PT_FCAT ASSIGNING FIELD-SYMBOL(<FS_FCAT>).

    CLEAR: <FS_FCAT>-SCRTEXT_L, <FS_FCAT>-SCRTEXT_M, <FS_FCAT>-SCRTEXT_S.

    CASE <FS_FCAT>-FIELDNAME.
      WHEN 'ABKRS'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'Área proc. FlhPagto.'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN 'ATEXT'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'Descrição'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN 'STATE'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'Status'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN 'PABRP_PABRJ'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'Per.proc.flh.pgto.'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN 'BEGDA'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'De'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN 'ENDDA'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'Até'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN 'SRTFD'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'Execução'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN 'UABRP_UABRJ'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'Período mais antigo'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN 'UNAME'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'Usuário'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN 'AEDAT'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'data'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN 'UZEIT'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'Hora'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN 'STATE_U'.
        <FS_FCAT>-SCRTEXT_S = <FS_FCAT>-SCRTEXT_M = <FS_FCAT>-SCRTEXT_L = 'Modif'.
        <FS_FCAT>-REPTEXT = <FS_FCAT>-COLTEXT = <FS_FCAT>-SCRTEXT_L.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SPLIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_V_GRID  text
*----------------------------------------------------------------------*
FORM ZF_SPLIT_SCREEN USING I_TITULO CHANGING P_ALV TYPE REF TO CL_GUI_ALV_GRID.

  DATA: L_HEADER       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
        L_PICTURE      TYPE REF TO CL_GUI_PICTURE,
        L_DG_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT,
        L_HTML         TYPE REF TO  CL_GUI_HTML_VIEWER,
        L_SPLIT        TYPE REF TO  CL_GUI_SPLITTER_CONTAINER.

  DATA: T_TEXT_TABLE_F  TYPE SDYDO_TEXT_TABLE,
        T_TEXT_TABLE_V  TYPE SDYDO_TEXT_TABLE,
        T_TEXT_TABLE_F2 TYPE SDYDO_TEXT_TABLE,
        T_TEXT_TABLE_V2 TYPE SDYDO_TEXT_TABLE.

  CHECK L_SPLIT IS INITIAL.

  L_SPLIT = NEW #( PARENT = CL_GUI_CONTAINER=>SCREEN0 ROWS = 2 COLUMNS = 1 ).

  DATA(V_CONTAINER_HTML) = L_SPLIT->GET_CONTAINER( EXPORTING ROW = 1 COLUMN = 1 ).

*----------------------------------------------------------------------*
* Header
*----------------------------------------------------------------------*
  L_HEADER = NEW #( PARENT = V_CONTAINER_HTML ROWS = 1 COLUMNS = 2 ).

  DATA(L_HEADER_TEXTO) = L_HEADER->GET_CONTAINER( EXPORTING ROW = 1 COLUMN = 1 ).

  L_HEADER->SET_COLUMN_WIDTH( EXPORTING ID = 1 WIDTH = 40 ).

  "Logo
  DATA(L_HEADER_LOGO) = L_HEADER->GET_CONTAINER( EXPORTING ROW = 1 COLUMN = 2 ).

  L_PICTURE = NEW #( PARENT = L_HEADER_LOGO ).

  DATA: L_URL TYPE CHAR255.

  PERFORM ZF_BUSCAR_IMAGEM_URL USING 'LOGO_NOVO' CHANGING L_URL.

  L_PICTURE->LOAD_PICTURE_FROM_URL( EXPORTING URL = L_URL ).

  L_PICTURE->SET_DISPLAY_MODE( EXPORTING DISPLAY_MODE = L_PICTURE->DISPLAY_MODE_FIT_CENTER ).

*----------------------------------------------------------------------*
* Item
*----------------------------------------------------------------------*

  DATA(V_ITEM_GRID) = L_SPLIT->GET_CONTAINER( EXPORTING ROW = 2 COLUMN = 1 ).

  L_SPLIT->SET_ROW_HEIGHT( EXPORTING ID = 1 HEIGHT = 15 ).

  P_ALV = NEW #( I_PARENT = V_ITEM_GRID ).

  L_DG_DYNDOC_ID = NEW #( STYLE = 'ALV_TO_HTML' BACKGROUND_COLOR = 7 ).
  L_DG_DYNDOC_ID->INITIALIZE_DOCUMENT( ).

  L_DG_DYNDOC_ID->ADD_TABLE( EXPORTING NO_OF_COLUMNS = 1 BORDER = '0' WIDTH = '100%' IMPORTING TABLE = DATA(TABLE_ELEMENT) ).

*----------------------------------------------------------------------*
* Preencher Titulo
*----------------------------------------------------------------------*
  IF  I_TITULO IS NOT INITIAL.
    TABLE_ELEMENT->ADD_COLUMN( IMPORTING COLUMN = DATA(COLUMN) ).
    TABLE_ELEMENT->SET_COLUMN_STYLE( EXPORTING COL_NO = 1 SAP_STYLE = CL_DD_DOCUMENT=>HEADING SAP_ALIGN = 'CENTER' ).
    COLUMN->ADD_TEXT( EXPORTING TEXT = CONV #( I_TITULO ) SAP_STYLE = 'HEADING' ).
  ENDIF.

*----------------------------------------------------------------------*
* Mostra dados adicionais
*----------------------------------------------------------------------*
  IF T_FILTRO[] IS NOT INITIAL.

    L_DG_DYNDOC_ID->ADD_TABLE( EXPORTING NO_OF_COLUMNS = 4 BORDER = '0' WIDTH = '100%' IMPORTING TABLE = DATA(TABLE_ELEMENT_LINHAS) ).
    TABLE_ELEMENT_LINHAS->ADD_COLUMN( IMPORTING COLUMN = DATA(COLUMN_1) ).
    TABLE_ELEMENT_LINHAS->ADD_COLUMN( IMPORTING COLUMN = DATA(COLUMN_2) ).
    TABLE_ELEMENT_LINHAS->ADD_COLUMN( IMPORTING COLUMN = DATA(COLUMN_3) ).
    TABLE_ELEMENT_LINHAS->ADD_COLUMN( IMPORTING COLUMN = DATA(COLUMN_4) ).

    TABLE_ELEMENT_LINHAS->SET_COLUMN_STYLE( EXPORTING COL_NO = 1 SAP_ALIGN = 'LEFT' SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
    TABLE_ELEMENT_LINHAS->SET_COLUMN_STYLE( EXPORTING COL_NO = 2 SAP_ALIGN = 'LEFT' SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL ).
    TABLE_ELEMENT_LINHAS->SET_COLUMN_STYLE( EXPORTING COL_NO = 3 SAP_ALIGN = 'LEFT' SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
    TABLE_ELEMENT_LINHAS->SET_COLUMN_STYLE( EXPORTING COL_NO = 4 SAP_ALIGN = 'LEFT' SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL ).

    LOOP AT T_FILTRO INTO DATA(W_FILTRO).

      APPEND W_FILTRO-PARAMETRO TO T_TEXT_TABLE_F.
      APPEND W_FILTRO-VALOR TO T_TEXT_TABLE_V.

      APPEND W_FILTRO-PARAMETRO2 TO T_TEXT_TABLE_F2.
      APPEND W_FILTRO-VALOR2 TO T_TEXT_TABLE_V2.

      COLUMN_1->ADD_TEXT( EXPORTING TEXT_TABLE = T_TEXT_TABLE_F  FIX_LINES = ABAP_TRUE ).
      COLUMN_2->ADD_TEXT( EXPORTING TEXT_TABLE = T_TEXT_TABLE_V  FIX_LINES = ABAP_TRUE ).
      COLUMN_3->ADD_TEXT( EXPORTING TEXT_TABLE = T_TEXT_TABLE_F2 FIX_LINES = ABAP_TRUE ).
      COLUMN_4->ADD_TEXT( EXPORTING TEXT_TABLE = T_TEXT_TABLE_V2 FIX_LINES = ABAP_TRUE ).

      CLEAR: T_TEXT_TABLE_F[], T_TEXT_TABLE_V[], T_TEXT_TABLE_F2[], T_TEXT_TABLE_V2[].

    ENDLOOP.

  ENDIF.

  L_HTML = NEW #( PARENT = L_HEADER_TEXTO ).

  L_DG_DYNDOC_ID->MERGE_DOCUMENT( ).

  L_DG_DYNDOC_ID->HTML_CONTROL = L_HTML.

  L_DG_DYNDOC_ID->DISPLAY_DOCUMENT( EXPORTING REUSE_CONTROL = 'X' PARENT = L_HEADER_TEXTO ).


  CALL METHOD L_SPLIT->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 20.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_IMAGEM_URL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1099   text
*      <--P_L_URL  text
*----------------------------------------------------------------------*
FORM ZF_BUSCAR_IMAGEM_URL   USING    I_NOME_LOGO
                         CHANGING R_URL.

  TYPES: BEGIN OF TY_GRAPHIC_TABLE,
           LINE(255) TYPE X,
         END OF TY_GRAPHIC_TABLE.

  DATA: GRAPHIC_TABLE TYPE TABLE OF TY_GRAPHIC_TABLE.

  DATA: L_GRAPHIC_XSTR TYPE XSTRING.

  CALL METHOD CL_SSF_XSF_UTILITIES=>GET_BDS_GRAPHIC_AS_BMP
    EXPORTING
      P_OBJECT = 'GRAPHICS'
      P_NAME   = I_NOME_LOGO
      P_ID     = 'BMAP'
      P_BTYPE  = 'BCOL'
    RECEIVING
      P_BMP    = L_GRAPHIC_XSTR.

  DATA(GRAPHIC_SIZE) = XSTRLEN( L_GRAPHIC_XSTR ).
  DATA(L_GRAPHIC_CONV) = GRAPHIC_SIZE.
  DATA(L_GRAPHIC_OFFS) = 0.
  WHILE L_GRAPHIC_CONV > 255.
    APPEND VALUE #( LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(255) ) TO GRAPHIC_TABLE.
    L_GRAPHIC_OFFS = L_GRAPHIC_OFFS + 255.
    L_GRAPHIC_CONV = L_GRAPHIC_CONV - 255.
  ENDWHILE.
  APPEND VALUE #( LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(L_GRAPHIC_CONV) ) TO GRAPHIC_TABLE.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE     = 'IMAGE'
      SUBTYPE  = 'X-UNKNOWN'
      SIZE     = GRAPHIC_SIZE
      LIFETIME = 'T'
    TABLES
      DATA     = GRAPHIC_TABLE
    CHANGING
      URL      = R_URL.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
