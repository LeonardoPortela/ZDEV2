*&---------------------------------------------------------------------*
*& Report ZPMR_TESTE_GC_2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPMR_TESTE_GC_2.
TABLES: EKKO, EKPO, MAKT.

TYPES:

*TABELA PRINCIPAL /ESTRUTURA DE SAIDA
  BEGIN OF TY_SAIDA,
    EBELN TYPE EKKO-EBELN,
    BUKRS TYPE EKKO-BUKRS,
    STATU TYPE EKKO-STATU,
    AEDAT TYPE EKKO-AEDAT,
    ERNAM TYPE EKKO-ERNAM,
    MATNR TYPE EKPO-MATNR,
    WERKS TYPE EKPO-WERKS,
    LGORT TYPE EKPO-LGORT,
    MENGE TYPE EKPO-MENGE,
    NETWR TYPE EKPO-NETWR,
    MAKTX TYPE MAKT-MAKTX,
  END OF TY_SAIDA,

*ESTRUTURA DE SELEÇÃO DE DADOS
  BEGIN OF TY_EKKO,
    EBELN TYPE EKKO-EBELN,
    BUKRS TYPE EKKO-BUKRS,
    STATU TYPE EKKO-STATU,
    AEDAT TYPE EKKO-AEDAT,
    ERNAM TYPE EKKO-ERNAM,
  END OF TY_EKKO,

  BEGIN OF TY_EKPO,
    EBELN TYPE EKPO-EBELN,
    MATNR TYPE EKPO-MATNR,
    WERKS TYPE EKPO-WERKS,
    LGORT TYPE EKPO-LGORT,
    MENGE TYPE EKPO-MENGE,
    NETWR TYPE EKPO-NETWR,
  END OF TY_EKPO,

  BEGIN OF TY_MAKT,
    MATNR TYPE MAKT-MATNR,
    MAKTX TYPE MAKT-MAKTX,
  END OF TY_MAKT.

*DECLARAÇÃO DE TABELA
DATA: IT_EKKO  TYPE TABLE OF TY_EKKO,
      WA_EKKO  TYPE TY_EKKO,
      IT_EKPO  TYPE TABLE OF TY_EKPO,
      WA_EKPO  TYPE TY_EKPO,
      IT_MAKT  TYPE TABLE OF TY_MAKT,
      WA_MAKT  TYPE TY_MAKT,
      IT_SAIDA TYPE TABLE OF TY_SAIDA,
      WA_SAIDA TYPE TY_SAIDA.

DATA: CHECK_INFO TYPE CHAR01.

*DECLARAÇÕES DO ALV
DATA: DG_SPLITTER_1        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      G_GRID               TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,
      CL_CONTAINER_95      TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID        TYPE REF TO CL_DD_DOCUMENT,
      TL_FUNCTION          TYPE UI_FUNCTIONS,
      WL_FUNCTION          TYPE UI_FUNC,
*
      T_FIELDCAT           TYPE LVC_T_FCAT,
      W_FIELDCAT           TYPE LVC_S_FCAT,
      T_COLORCELL          TYPE TABLE OF LVC_S_SCOL,
      W_COLORCELL          TYPE LVC_S_SCOL,
      T_EXCTAB             TYPE SLIS_T_EXTAB,
      W_EXCTAB             TYPE SLIS_EXTAB,
      W_LAYOUT             TYPE LVC_S_LAYO,
      W_STABLE             TYPE LVC_S_STBL,
      T_STYLE              TYPE LVC_T_STYL,
      W_STYLE              TYPE LVC_S_STYL,
      T_ROWS               TYPE LVC_T_ROW,
      W_ROWS               TYPE LVC_S_ROW,
      OK_CODE              TYPE SY-UCOMM.

DATA: VARIANTE         LIKE DISVARIANT.

*----------------------------------------------------------------------*

* TELA DE SELEÇÃO

*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:     P_BUKRS FOR EKKO-BUKRS,
                      P_WERKS FOR EKPO-WERKS,
                      P_EBELN FOR EKKO-EBELN,
                      P_AEDAT FOR EKKO-AEDAT,
                      P_STATU FOR EKKO-STATU,
                      P_ERNAM FOR EKKO-ERNAM.

SELECTION-SCREEN: END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM PF_SELECAO_DE_DADOS.

  PERFORM PF_TRATA_DADOS.

  PERFORM PF_EXIBIR_DADOS.

FORM PF_SELECAO_DE_DADOS .
  SELECT * FROM EKKO INTO CORRESPONDING FIELDS OF TABLE IT_EKKO
    WHERE EBELN IN P_EBELN
      AND BUKRS IN P_BUKRS
      AND STATU IN P_STATU
      AND AEDAT IN P_AEDAT
      AND ERNAM IN P_ERNAM.

  IF IT_EKKO IS NOT INITIAL.
    SELECT * FROM EKPO INTO CORRESPONDING FIELDS OF TABLE IT_EKPO
      FOR ALL ENTRIES IN IT_EKKO
      WHERE EBELN = IT_EKKO-EBELN.
  ENDIF.

  IF IT_EKPO IS NOT INITIAL.
    SELECT * FROM MAKT INTO CORRESPONDING FIELDS OF TABLE IT_MAKT
      FOR ALL ENTRIES IN IT_EKPO
      WHERE MATNR = IT_EKPO-MATNR.
  ENDIF.
ENDFORM.

FORM PF_TRATA_DADOS .
  FREE IT_SAIDA.
  LOOP AT IT_EKKO INTO WA_EKKO.
    CLEAR: CHECK_INFO, WA_SAIDA, WA_MAKT.

    WA_SAIDA-EBELN = WA_EKKO-EBELN.
    WA_SAIDA-BUKRS = WA_EKKO-BUKRS.
    WA_SAIDA-STATU = WA_EKKO-STATU.
    WA_SAIDA-AEDAT = WA_EKKO-AEDAT.
    WA_SAIDA-ERNAM = WA_EKKO-ERNAM.

    LOOP AT IT_EKPO INTO WA_EKPO WHERE EBELN = WA_EKKO-EBELN.
      WA_SAIDA-MATNR = WA_EKPO-MATNR.
      WA_SAIDA-WERKS = WA_EKPO-WERKS.
      WA_SAIDA-LGORT = WA_EKPO-LGORT.
      WA_SAIDA-MENGE = WA_EKPO-MENGE.
      WA_SAIDA-NETWR = WA_EKPO-NETWR.

*       READ AT IT_MAKT INTO WA_MAKT WITH  MATNR = WA_EKPO-MATNR.
      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO-MATNR.
      IF SY-SUBRC EQ 0.
        WA_SAIDA-MAKTX = WA_MAKT-MAKTX.
      ENDIF.
      APPEND WA_SAIDA TO IT_SAIDA.
      CHECK_INFO = ABAP_TRUE.
    ENDLOOP.

    IF CHECK_INFO <> ABAP_TRUE.
      APPEND WA_SAIDA TO IT_SAIDA.
    ENDIF.
  ENDLOOP.
ENDFORM.


FORM PF_EXIBIR_DADOS .
  IF IT_SAIDA IS NOT INITIAL.
    CALL SCREEN 100.

  ENDIF.
ENDFORM.

MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TITLE_0100'.

  PERFORM PF_ALV.
ENDMODULE.

MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM. "Chamada do botão
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

FORM PF_ALV.
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.

  DATA:
    P_TEXT      TYPE SDYDO_TEXT_ELEMENT,
    FILTROS     TYPE ZIF_SCREEN_LINHA_FILTRO,
    I_FILTROS   TYPE ZIF_SCREEN_LINHA_FILTRO_T,
    V_VALOR(60),
    V_DATUM(10) TYPE C,
    V_UZEIT(10) TYPE C.

  PERFORM PF_FIELDCATALOG.

  VARIANTE = VALUE #( REPORT = SY-REPID ).

  IF G_GRID IS INITIAL.

    CLEAR: I_FILTROS.
    CONCATENATE SY-DATUM+06(02) '/' SY-DATUM+04(02) '/' SY-DATUM(04) INTO V_DATUM.
    CONCATENATE SY-UZEIT(02) ':' SY-UZEIT+02(02) ':' SY-UZEIT+04(02) INTO V_UZEIT.
    DESCRIBE TABLE IT_SAIDA LINES DATA(V_LINES).
    APPEND VALUE #( PARAMETRO = 'DATA:' VALOR = V_DATUM ) TO I_FILTROS.
    APPEND VALUE #( PARAMETRO = 'HORA:' VALOR = V_UZEIT ) TO I_FILTROS.
    APPEND VALUE #( PARAMETRO = 'REGISTROS:' VALOR = V_LINES ) TO I_FILTROS.

  ENDIF.

  IF ZCL_SCREEN=>ZIF_SCREEN~SET_CRIAR_TELA_PADRAO_REPORT(
     EXPORTING
       I_TITULO  = CONV #( P_TEXT )
       I_FILTROS = I_FILTROS
     CHANGING
       SPLIT     = DG_SPLITTER_1
       ALV       = G_GRID ) = ABAP_TRUE.


    W_LAYOUT-SEL_MODE = 'A'.
    W_LAYOUT-COL_OPT  = ABAP_TRUE.

    W_STABLE-ROW          = ABAP_TRUE.
    W_STABLE-COL          = ABAP_TRUE.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = W_LAYOUT
        I_SAVE                        = 'A'
        IT_TOOLBAR_EXCLUDING          = TL_FUNCTION
        IS_VARIANT                    = VARIANTE
      CHANGING
        IT_OUTTAB                     = IT_SAIDA[]
        IT_FIELDCATALOG               = T_FIELDCAT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.


    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.


    IF LINES( T_ROWS ) > 0.
      CALL METHOD G_GRID->SET_SELECTED_ROWS
        EXPORTING
          IT_INDEX_ROWS = T_ROWS.
    ENDIF.

  ELSE.
    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = W_STABLE ).
  ENDIF.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
ENDFORM.

FORM PF_FIELDCATALOG.
  FREE T_FIELDCAT[].

  PERFORM PF_ESTRUTURA_ALV USING:
 01  ''   ''   'IT_SAIDA'   'BUKRS  '            'Empresa          '       '4  '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'IT_SAIDA'   'WERKS  '            'Centro           '       '4  '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'IT_SAIDA'   'EBELN  '            'Nº doc. compras  '       '10 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'IT_SAIDA'   'MATNR  '            'Nº do material   '       '40 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'IT_SAIDA'   'MAKTX  '            'Desv. material   '       '40 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''   ''   'IT_SAIDA'   'MENGE  '            'Qtd. pedido      '       '13 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  ''   ''   'IT_SAIDA'   'LGORT  '            'Depósito         '       '4  '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''   ''   'IT_SAIDA'   'NETWR  '            'Vlr pedido       '       '13 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''   ''   'IT_SAIDA'   'STATU  '            'Status           '       '1  '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 10  ''   ''   'IT_SAIDA'   'ERNAM  '            'Usuário          '       '12 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 11  ''   ''   'IT_SAIDA'   'AEDAT  '            'Data             '       '8  '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

ENDFORM.

FORM PF_ESTRUTURA_ALV  USING VALUE(P_COL_POS)       TYPE I                    "1
                           VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME        "2
                           VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME      "3
                           VALUE(P_TABNAME)       LIKE DD02D-TABNAME        "4
                           VALUE(P_FIELD)         LIKE DD03D-FIELDNAME      "5
                           VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L      "6
                           VALUE(P_OUTPUTLEN)                               "7
                           VALUE(P_EDIT)                                    "8
                           VALUE(P_SUM)                                     "9
                           VALUE(P_JUST)                                    "10
                           VALUE(P_HOTSPOT)                                 "11
                           VALUE(P_F4)                                      "12
                           VALUE(P_CHECKBOX)                                "13
                           VALUE(P_STYLE)                                   "14
                           VALUE(P_NO_OUT)                                  "15
                           VALUE(P_ICON)                                    "16
                           VALUE(P_FIX).                                    "17

  CLEAR W_FIELDCAT.
  W_FIELDCAT-FIELDNAME   = P_FIELD.
  W_FIELDCAT-TABNAME     = P_TABNAME.
  W_FIELDCAT-REF_TABLE   = P_REF_TABNAME.
  W_FIELDCAT-REF_FIELD   = P_REF_FIELDNAME.
  W_FIELDCAT-KEY         = ' '.
  W_FIELDCAT-EDIT        = P_EDIT.
  W_FIELDCAT-COL_POS     = P_COL_POS.
  W_FIELDCAT-OUTPUTLEN   = P_OUTPUTLEN.
  W_FIELDCAT-NO_OUT      = P_NO_OUT.
  W_FIELDCAT-DO_SUM      = P_SUM.
  W_FIELDCAT-REPTEXT     = P_SCRTEXT_L.
  W_FIELDCAT-SCRTEXT_S   = P_SCRTEXT_L.
  W_FIELDCAT-SCRTEXT_M   = P_SCRTEXT_L.
  W_FIELDCAT-SCRTEXT_L   = P_SCRTEXT_L.
  W_FIELDCAT-STYLE       = P_STYLE.
  W_FIELDCAT-JUST        = P_JUST.
  W_FIELDCAT-HOTSPOT     = P_HOTSPOT.
  W_FIELDCAT-F4AVAILABL  = P_F4.
  W_FIELDCAT-CHECKBOX    = P_CHECKBOX.
  W_FIELDCAT-ICON        = P_ICON.
  W_FIELDCAT-COLDDICTXT  = 'M'.
  W_FIELDCAT-SELDDICTXT  = 'M'.
  W_FIELDCAT-TIPDDICTXT  = 'M'.
  W_FIELDCAT-FIX_COLUMN  = P_FIX.
  W_FIELDCAT-COL_OPT     = 'X'.

  IF P_FIELD = 'MATNR'.
    W_FIELDCAT-COL_OPT     = 'X'.
  ENDIF.

  IF W_FIELDCAT-FIELDNAME = 'MATNR'.
    W_FIELDCAT-NO_ZERO = ABAP_TRUE.
  ENDIF.


  APPEND W_FIELDCAT TO T_FIELDCAT.

ENDFORM.
