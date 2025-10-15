
DATA: G_CUSTOM_CONTAINER1  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CUSTOM_CONTAINER2  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DG_SPLITTER_1        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_SPLITTER_2        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_SPLITTER_3        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_SPLITTER_4        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_1          TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_2          TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_3          TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_4          TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_2A         TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_2A2        TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_ALV        TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_ALV2       TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_ALV3       TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_ALV4       TYPE REF TO CL_GUI_CONTAINER,
      PICTURE              TYPE REF TO CL_GUI_PICTURE,
      PICTURE2             TYPE REF TO CL_GUI_PICTURE,
      CTL_ALV              TYPE REF TO CL_GUI_ALV_GRID,
      CTL_ALV2             TYPE REF TO CL_GUI_ALV_GRID,
      DG_DYNDOC_ID         TYPE REF TO CL_DD_DOCUMENT,
      DG_DYNDOC_ID2        TYPE REF TO CL_DD_DOCUMENT,
      TABLE_ELEMENT        TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN               TYPE REF TO CL_DD_AREA,
      TABLE_ELEMENT2       TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN_1             TYPE REF TO CL_DD_AREA,
      TABLE_ELEMENT3       TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN3              TYPE REF TO CL_DD_AREA,
      TABLE_ELEMENT4       TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN_4             TYPE REF TO CL_DD_AREA,
      DG_HTML_CNTRL        TYPE REF TO CL_GUI_HTML_VIEWER,
      DG_HTML_CNTRL2       TYPE REF TO CL_GUI_HTML_VIEWER,
      IT_EXCLUDE_FCODE     TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE     LIKE LINE OF IT_EXCLUDE_FCODE,
      IT_EXCLUDE_FCODE_2   TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE_2   LIKE LINE OF IT_EXCLUDE_FCODE,
      IT_EXCLUDE_FCODE_3   TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE_3   LIKE LINE OF IT_EXCLUDE_FCODE,
      GS_LAYOUT            TYPE LVC_S_LAYO,
      GS_LAYOUT_2          TYPE LVC_S_LAYO,
      GS_LAYOUT_3          TYPE LVC_S_LAYO,
      GS_VARIANT           TYPE DISVARIANT,
      GS_VARIANT_2         TYPE DISVARIANT,
      GS_VARIANT_3         TYPE DISVARIANT,
      IT_FIELDCATALOG      TYPE LVC_T_FCAT,
      IT_FIELDCATALOG2     TYPE LVC_T_FCAT,
      IT_FIELDCATALOG_AUX  TYPE LVC_T_FCAT,
      IT_FIELDCATALOG_AUX2 TYPE LVC_T_FCAT,
      IT_SORT              TYPE LVC_T_SORT,
      IT_SORT2             TYPE LVC_T_SORT,
      LS_STABLE            TYPE LVC_S_STBL,
      LS_STABLE_2          TYPE LVC_S_STBL.

DATA: URL(255)                 TYPE C,
      URL2(255)                TYPE C,
      P_TEXT                   TYPE SDYDO_TEXT_ELEMENT,
      P_TEXT2                  TYPE SDYDO_TEXT_ELEMENT,
      SDYDO_TEXT_ELEMENT(255),
      SDYDO_TEXT_ELEMENT2(255),
      P_TEXT_TABLE             TYPE SDYDO_TEXT_TABLE,
      P_TEXT_TABLE2            TYPE SDYDO_TEXT_TABLE,
      VL_CONT                  TYPE I,
      VL_BUTXT                 TYPE T001-BUTXT,
      VL_CONT2                 TYPE I,
      VL_BUTXT2                TYPE T001-BUTXT,
      VL_DATES1                TYPE CHAR10,
      VL_DATES2                TYPE CHAR10,
      VL_ANO                   TYPE CHAR10.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF G_CUSTOM_CONTAINER1 IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER1
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

    PERFORM SET_TOPO USING G_CUSTOM_CONTAINER1 ABAP_FALSE.

    FREE: IT_FIELDCATALOG_AUX, IT_FIELDCATALOG.

    PERFORM FILL_IT_SORT.

    P_TEXT = TEXT-008.
    PERFORM FILL_IT_FIELDCATALOG USING:
       01 'BUKRS'      ''          '05'  ' '     ' '    ' '   'Empresa',
       02 'SWERK'      ''          '05'  ' '     ' '    ' '   'Filial',
       03 'TPLMA'      ''          '20'  ' '     ' '    ' '   'Local Superior',
       04 'PLTX2'      ''          '30'  ' '     ' '    ' '   'Desc. Local Superior',
       04 'PLTX1'      ''          '30'  ' '     ' '    ' '   'Desc. Local Inferior',
       05 'TXTQM'      ''          '60'  ' '     ' '    ' '   'Tipo Parada',
       08 'PAR01'      ''          '16'  ' '     ' '    'X'   '%Jan',
       09 'PAR02'      ''          '16'  ' '     ' '    'X'   '%Fev',
       10 'PAR03'      ''          '16'  ' '     ' '    'X'   '%Mar',
       11 'PAR04'      ''          '16'  ' '     ' '    'X'   '%Abr',
       12 'PAR05'      ''          '16'  ' '     ' '    'X'   '%Mai',
       13 'PAR06'      ''          '16'  ' '     ' '    'X'   '%Jun',
       14 'PAR07'      ''          '16'  ' '     ' '    'X'   '%Jul',
       15 'PAR08'      ''          '16'  ' '     ' '    'X'   '%Ago',
       16 'PAR09'      ''          '16'  ' '     ' '    'X'   '%Set',
       17 'PAR10'      ''          '16'  ' '     ' '    'X'   '%Out',
       18 'PAR11'      ''          '16'  ' '     ' '    'X'   '%Nov',
       19 'PAR12'      ''          '16'  ' '     ' '    'X'   '%Dez',
       20 'PAR13'      ''          '16'  ' '     ' '    'X'   '%Total'.

    APPEND LINES OF IT_FIELDCATALOG_AUX TO IT_FIELDCATALOG.
    FREE IT_FIELDCATALOG_AUX.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT.

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    CREATE OBJECT OBJ_EVEN.

    SET HANDLER: OBJ_EVEN->HANDLE_DOUBLE_CLICK FOR CTL_ALV,
                 OBJ_EVEN->ON_HOTSPOT_CLICK    FOR CTL_ALV.

    CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY(
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_SAIDA
        IT_SORT              = IT_SORT ).

    PERFORM MONTA_TOPO.

    PERFORM ADD_ITENS_TOPO.

    PERFORM AJUSTA_TOTAIS.

  ELSE.

    LS_STABLE = VALUE #(
                         ROW = ABAP_TRUE
                         COL = ABAP_TRUE
                       ).

    CTL_ALV->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = LS_STABLE EXCEPTIONS FINISHED  = 1 OTHERS    = 2 ).

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  LEAVE TO CURRENT TRANSACTION.
*  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'PRINT'.

      PERFORM IMPRIMIR_SMARTFORM.
    WHEN 'EXEC'.
      PERFORM INICIA.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.

  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TL0200'.

*  CLEAR G_CUSTOM_CONTAINER2.

  IF G_CUSTOM_CONTAINER2 IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER2
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER2'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

*    PERFORM SET_TOPO USING G_CUSTOM_CONTAINER1 ABAP_TRUE.
*    PERFORM SET_TOPO USING G_CUSTOM_CONTAINER2 ABAP_FALSE.
*    PERFORM SET_TOPO_DETALHE. "USING G_CUSTOM_CONTAINER2 ABAP_TRUE.

    FREE GS_VARIANT_2.
    FREE: IT_FIELDCATALOG_AUX2, IT_FIELDCATALOG2.


    P_TEXT2 = TEXT-011.
    PERFORM FILL_IT_FIELDCATALOG2 USING:
       01 'SWERK'  'VIAUFKST' '10'  ' '  ' '  ' '  'Centro',
       02 'QMART'  'VIQMEL'  '10'  ' '  ' '  ' '  'Tipo nota',
       03 'QMNUM'  'VIQMEL'  '10'  ' '  ' '  ' '  'Nota',
*      04 'TPLNR_'  'VIQMEL'  '10'  ' '  ' '  ' '  'Local Afetado',
       05 'PLTXT_' '      '  '40'  ' '  ' '  ' '  'Txt loc afetado',
       06 'AUSZT'  'VIQMEL'  '10'  ' '  ' '  ' '  'Dur da parada',
       07 'AUFNR'  'VIQMEL'  '10'  ' '  ' '  ' '  'OS',
       08 'AUART'  'AUFK'    '04'  ' '  ' '  ' '  'Tipo ordem',
       09 'TXT'    'V_AUART' '30'  ' '  ' '  ' '  'Txt tipo',
       10 'VORNR'  'AFVC'    '04'  ' '  ' '  ' '  'Operação',
       11 'LTXA1'  'AFVC'    '20'  ' '  ' '  ' '  'Serv executado',
       12 'VAPLZ'  'AUFK'    '20'  ' '  ' '  ' '  'Exect',
       13 'EQUNR'  'VIQMEL'  '18'  ' '  ' '  ' '  'Equipamento',
       14 'PLTXT'  'IFLO'    '15'  ' '  ' '  ' '  'Local',
       15 'AUSVN'  'VIQMEL'  '10'  ' '  ' '  ' '  'Data'.

    APPEND LINES OF IT_FIELDCATALOG_AUX2 TO IT_FIELDCATALOG2.
    FREE IT_FIELDCATALOG_AUX2.

    PERFORM FILL_GS_VARIANT_2.

    CREATE OBJECT CTL_ALV2
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER2.

*    PERFORM EXCLUDE.

    CTL_ALV2->SET_TABLE_FOR_FIRST_DISPLAY(
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT_2
        IS_VARIANT           = GS_VARIANT_2
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE_2
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG2
        IT_OUTTAB            = IT_VIQSAIDA[] ).

    CREATE OBJECT OBJ_EVEN.
    SET HANDLER: OBJ_EVEN->ON_HOTSPOT_CLICK  FOR CTL_ALV2.

*    PERFORM MONTA_TOPO_DETALHE.

*    PERFORM ADD_ITENS_TOPO_DETALHE.

  ELSE.

    LS_STABLE_2 = VALUE #(
                         ROW = ABAP_TRUE
                         COL = ABAP_TRUE

                       ).


    FREE GS_VARIANT_2.
    PERFORM FILL_GS_VARIANT_2.

    CTL_ALV2->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = LS_STABLE_2 EXCEPTIONS FINISHED  = 1 OTHERS = 2 ).

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
*      CALL SCREEN 0100.
      LEAVE TO SCREEN 0.

*      DATA: CONTADOR TYPE SY-TABIX.
*
*      DO.
*
*        ADD 1 TO CONTADOR.
*
*        IF CONTADOR <= CLICKS.
*          LEAVE TO SCREEN 0.
*        ELSE.
*          CLEAR: CLICKS, CONTADOR.
*          EXIT.
*        ENDIF.
*
*      ENDDO.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0190   text
*      <--P_URL  text
*----------------------------------------------------------------------*
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

  CL_SSF_XSF_UTILITIES=>GET_BDS_GRAPHIC_AS_BMP(
    EXPORTING
      P_OBJECT = 'GRAPHICS'
      P_NAME   = NOME_LOGO
      P_ID     = 'BMAP'
      P_BTYPE  = 'BCOL'
    RECEIVING
      P_BMP    = L_GRAPHIC_XSTR ).

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
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM FILL_GS_VARIANT.

  GS_VARIANT = VALUE #(
                        REPORT      = SY-REPID
                        HANDLE      = '0100'
                        LOG_GROUP   = ABAP_FALSE
                        USERNAME    = ABAP_FALSE
                        VARIANT     = ABAP_FALSE
                        TEXT        = ABAP_FALSE
                        DEPENDVARS  = ABAP_FALSE
                      ).

  GS_LAYOUT = VALUE #(
                        SEL_MODE   = 'A'
                        CWIDTH_OPT = 'X'
                     ).

ENDFORM.                    " FILL_GS_VARIANT


FORM FILL_GS_VARIANT_2.

  GS_VARIANT_2 = VALUE #(
                        REPORT      = SY-REPID
                        HANDLE      = '0200'
                        LOG_GROUP   = ABAP_FALSE
                        USERNAME    = ABAP_FALSE
                        VARIANT     = ABAP_FALSE
                        TEXT        = ABAP_FALSE
                        DEPENDVARS  = ABAP_FALSE
                      ).

  GS_LAYOUT_2 = VALUE #(
                        EXCP_CONDS    = 'X'
                        ZEBRA         = 'X'
                        SEL_MODE      = 'A'
                        CWIDTH_OPT    = 'X'     "  Otimizar colunas na tela
                        TOTALS_BEF    = ' '
                     ).




ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE .

*   Excluir Buttons Toolbar
  FREE: IT_EXCLUDE_FCODE.

  IT_EXCLUDE_FCODE = VALUE #(
                             ( CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL )
                            ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG USING VALUE(P_COLNUM)
                                VALUE(P_FIELDNAME)
                                VALUE(P_TABNAME)
                                VALUE(P_LEN)
                                VALUE(P_EDIT)
                                VALUE(P_ICON)
                                VALUE(P_DO_SUM)
                                VALUE(P_HEADER).

  APPEND VALUE #(
                  COL_POS     = P_COLNUM
                  FIELDNAME   = P_FIELDNAME
                  TABNAME     = P_TABNAME
                  OUTPUTLEN   = P_LEN
                  COLTEXT     = P_HEADER
                  EDIT        = P_EDIT
                  ICON        = P_ICON
                  REF_TABLE   = P_TABNAME
                  CHECKTABLE  = P_TABNAME
                  DO_SUM      = P_DO_SUM
                  HOTSPOT     = COND #( WHEN P_FIELDNAME EQ 'AUFNR' OR  P_FIELDNAME EQ 'QMNUM' THEN ABAP_TRUE ELSE ABAP_FALSE )
                ) TO IT_FIELDCATALOG_AUX.

ENDFORM.                    " FILL_IT_FIELDCATALOG


FORM FILL_IT_FIELDCATALOG2 USING VALUE(P_COLNUM)
                                  VALUE(P_FIELDNAME)
                                  VALUE(P_TABNAME)
                                  VALUE(P_LEN)
                                  VALUE(P_EDIT)
                                  VALUE(P_ICON)
                                  VALUE(P_DO_SUM)
                                  VALUE(P_HEADER).

  APPEND VALUE #(
                  COL_POS     = P_COLNUM
                  FIELDNAME   = P_FIELDNAME
                  TABNAME     = P_TABNAME
                  OUTPUTLEN   = P_LEN
                  COLTEXT     = P_HEADER
                  EDIT        = P_EDIT
                  ICON        = P_ICON
                  REF_TABLE   = P_TABNAME
                  CHECKTABLE  = P_TABNAME
                  DO_SUM      = P_DO_SUM
                  HOTSPOT     = COND #( WHEN P_FIELDNAME EQ 'AUFNR' OR  P_FIELDNAME EQ 'QMNUM' THEN ABAP_TRUE ELSE ABAP_FALSE )
                ) TO IT_FIELDCATALOG_AUX2.

  GS_LAYOUT_2-EXCP_CONDS    = 'X'.
  GS_LAYOUT_2-ZEBRA         = 'X'.
  GS_LAYOUT_2-SEL_MODE      = 'A'.
  GS_LAYOUT_2-CWIDTH_OPT    = 'X'.     "  Otimizar colunas na tela
  GS_LAYOUT_2-TOTALS_BEF    = ''.

ENDFORM.                    " FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_SORT .

  FREE IT_SORT.

*  IT_SORT = VALUE #(
*                     ( SPOS = '2'
*                  FIELDNAME = 'SWERK'
*                      GROUP = '*'
**                       DOWN = 'X'
*                     SUBTOT = 'X' )
*                   ).

  DATA: WA_SORT TYPE LVC_S_SORT.

  WA_SORT-SPOS = '1'.
  WA_SORT-FIELDNAME = 'SWERK'.
  "WA_SORT-DOWN = 'X'.
  WA_SORT-GROUP = '*'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO IT_SORT.

  IF P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL.
    WA_SORT-SPOS = '2'.
    WA_SORT-FIELDNAME = 'TPLMA'.
    "WA_SORT-DOWN = 'X'.
    WA_SORT-GROUP = '*'.
    WA_SORT-SUBTOT = 'X'.
    APPEND WA_SORT TO IT_SORT.

  ENDIF.


ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_SMARTFORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_SMARTFORM.

  DATA: TOTAL        TYPE ZPME_INDISPON,
        TOTALAUX     TYPE ZPME_INDISPON,
        TEMPRESA     TYPE ZPME_INDISPON,
        TEMPRESA_LOC TYPE ZPME_INDISPON.

  FREE IT_TEMPRESA        .
  FREE IT_TOT_EMPRESA     .
  FREE IT_TOT_EMPRESA_AUX .
  FREE IT_TOT_EMPRESA_LOC .
  FREE IT_TOT_CENTRO.

  FREE IT_SAIDA_SMARTAUX.
  IT_SAIDA_SMARTAUX[] = IT_SAIDA_SMARTFORM.

  SORT IT_SAIDA_SMARTAUX[] ASCENDING BY BUKRS TPLMA.

  LOOP AT IT_SAIDA INTO WA_SAIDA.
    ADD WA_SAIDA-PAR01 TO TOTAL-PAR01.
    ADD WA_SAIDA-PAR02 TO TOTAL-PAR02.
    ADD WA_SAIDA-PAR03 TO TOTAL-PAR03.
    ADD WA_SAIDA-PAR04 TO TOTAL-PAR04.
    ADD WA_SAIDA-PAR05 TO TOTAL-PAR05.
    ADD WA_SAIDA-PAR06 TO TOTAL-PAR06.
    ADD WA_SAIDA-PAR07 TO TOTAL-PAR07.
    ADD WA_SAIDA-PAR08 TO TOTAL-PAR08.
    ADD WA_SAIDA-PAR09 TO TOTAL-PAR09.
    ADD WA_SAIDA-PAR10 TO TOTAL-PAR10.
    ADD WA_SAIDA-PAR11 TO TOTAL-PAR11.
    ADD WA_SAIDA-PAR12 TO TOTAL-PAR12.
    ADD WA_SAIDA-PAR13 TO TOTAL-PAR13.
  ENDLOOP.

  APPEND TOTAL TO IT_SAIDA_SMARTAUX.
  CLEAR TOTAL.


  LOOP AT IT_SAIDA INTO WA_SAIDA.
    IF WA_SAIDA-BUKRS IS NOT INITIAL.
      IT_TEMPRESA-BUKRS = WA_SAIDA-BUKRS.
      IT_TEMPRESA-TPLMA = WA_SAIDA-TPLMA.
    ENDIF.
    APPEND IT_TEMPRESA.
    CLEAR IT_TEMPRESA.
    CLEAR WA_SAIDA.
  ENDLOOP.

  SORT IT_TEMPRESA[] BY BUKRS TPLMA.
  DELETE ADJACENT DUPLICATES FROM IT_TEMPRESA[] COMPARING BUKRS TPLMA.

  LOOP AT IT_TEMPRESA.
    LOOP AT IT_SAIDA INTO WA_SAIDA WHERE BUKRS = IT_TEMPRESA-BUKRS
                                     AND TPLMA = IT_TEMPRESA-TPLMA.

      ADD WA_SAIDA-PAR01 TO TEMPRESA_LOC-PAR01.
      ADD WA_SAIDA-PAR02 TO TEMPRESA_LOC-PAR02.
      ADD WA_SAIDA-PAR03 TO TEMPRESA_LOC-PAR03.
      ADD WA_SAIDA-PAR04 TO TEMPRESA_LOC-PAR04.
      ADD WA_SAIDA-PAR05 TO TEMPRESA_LOC-PAR05.
      ADD WA_SAIDA-PAR06 TO TEMPRESA_LOC-PAR06.
      ADD WA_SAIDA-PAR07 TO TEMPRESA_LOC-PAR07.
      ADD WA_SAIDA-PAR08 TO TEMPRESA_LOC-PAR08.
      ADD WA_SAIDA-PAR09 TO TEMPRESA_LOC-PAR09.
      ADD WA_SAIDA-PAR10 TO TEMPRESA_LOC-PAR10.
      ADD WA_SAIDA-PAR11 TO TEMPRESA_LOC-PAR11.
      ADD WA_SAIDA-PAR12 TO TEMPRESA_LOC-PAR12.
      ADD WA_SAIDA-PAR13 TO TEMPRESA_LOC-PAR13.
    ENDLOOP.

    TEMPRESA_LOC-BUKRS = WA_SAIDA-BUKRS.
    TEMPRESA_LOC-TPLMA = WA_SAIDA-TPLMA.

    APPEND TEMPRESA_LOC TO IT_TOT_EMPRESA_LOC.
    CLEAR TEMPRESA_LOC.
    CLEAR IT_TEMPRESA.
    CLEAR WA_SAIDA.
  ENDLOOP.

  LOOP AT IT_TOT_EMPRESA_LOC.
    LOOP AT IT_SAIDA_SMARTAUX WHERE BUKRS = IT_TOT_EMPRESA_LOC-BUKRS
                                AND TPLMA = IT_TOT_EMPRESA_LOC-TPLMA.

      MOVE-CORRESPONDING IT_SAIDA_SMARTAUX TO IT_TOT_EMPRESA_AUX.
      APPEND IT_TOT_EMPRESA_AUX.
      CLEAR IT_TOT_EMPRESA_AUX.
    ENDLOOP.


    MOVE-CORRESPONDING IT_TOT_EMPRESA_LOC TO IT_TOT_EMPRESA_AUX.
    APPEND IT_TOT_EMPRESA_AUX.

    CLEAR IT_TOT_EMPRESA_AUX.
    CLEAR IT_TOT_EMPRESA_LOC.
    CLEAR IT_TOT_EMPRESA_AUX.
    CLEAR IT_SAIDA_SMARTAUX.
  ENDLOOP.

  CLEAR IT_TOT_CENTRO_AUX.
  IT_TOT_CENTRO_AUX[] = IT_TOT_EMPRESA_AUX[].

*  SORT IT_TOT_CENTRO_AUX ASCENDING BY SWERK.

  SORT IT_TEMPRESA[] BY BUKRS.
  DELETE ADJACENT DUPLICATES FROM IT_TEMPRESA[] COMPARING BUKRS.

  LOOP AT IT_TEMPRESA.
    LOOP AT IT_SAIDA INTO WA_SAIDA WHERE BUKRS = IT_TEMPRESA-BUKRS.
*                                     AND TPLMA = IT_TEMPRESA-TPLMA.

      ADD WA_SAIDA-PAR01 TO TEMPRESA-PAR01.
      ADD WA_SAIDA-PAR02 TO TEMPRESA-PAR02.
      ADD WA_SAIDA-PAR03 TO TEMPRESA-PAR03.
      ADD WA_SAIDA-PAR04 TO TEMPRESA-PAR04.
      ADD WA_SAIDA-PAR05 TO TEMPRESA-PAR05.
      ADD WA_SAIDA-PAR06 TO TEMPRESA-PAR06.
      ADD WA_SAIDA-PAR07 TO TEMPRESA-PAR07.
      ADD WA_SAIDA-PAR08 TO TEMPRESA-PAR08.
      ADD WA_SAIDA-PAR09 TO TEMPRESA-PAR09.
      ADD WA_SAIDA-PAR10 TO TEMPRESA-PAR10.
      ADD WA_SAIDA-PAR11 TO TEMPRESA-PAR11.
      ADD WA_SAIDA-PAR12 TO TEMPRESA-PAR12.
      ADD WA_SAIDA-PAR13 TO TEMPRESA-PAR13.
    ENDLOOP.

    TEMPRESA-BUKRS = WA_SAIDA-BUKRS.

    APPEND TEMPRESA TO IT_TOT_EMPRESA.
    CLEAR TEMPRESA.
    CLEAR IT_TEMPRESA.
    CLEAR WA_SAIDA.
  ENDLOOP.


  LOOP AT IT_TOT_EMPRESA.
    LOOP AT IT_TOT_CENTRO_AUX WHERE BUKRS = IT_TOT_EMPRESA-BUKRS.
*                                AND TPLMA = IT_TOT_EMPRESA-TPLMA.

      MOVE-CORRESPONDING IT_TOT_CENTRO_AUX TO IT_TOT_CENTRO.
      APPEND IT_TOT_CENTRO.
      CLEAR IT_TOT_CENTRO_AUX.
      CLEAR IT_TOT_CENTRO.
    ENDLOOP.

    MOVE-CORRESPONDING IT_TOT_EMPRESA TO IT_TOT_CENTRO.
    APPEND IT_TOT_CENTRO.

    CLEAR IT_TOT_CENTRO.
    CLEAR IT_TOT_EMPRESA.
    CLEAR IT_TOT_CENTRO_AUX.
  ENDLOOP.


  FREE IT_TOT_GERAL_SMART.
  IT_TOT_GERAL_SMART[] = IT_TOT_CENTRO[].

  LOOP AT IT_TOT_CENTRO.
    IF IT_TOT_CENTRO-SWERK IS NOT INITIAL.
      ADD IT_TOT_CENTRO-PAR01 TO TOTALAUX-PAR01.
      ADD IT_TOT_CENTRO-PAR02 TO TOTALAUX-PAR02.
      ADD IT_TOT_CENTRO-PAR03 TO TOTALAUX-PAR03.
      ADD IT_TOT_CENTRO-PAR04 TO TOTALAUX-PAR04.
      ADD IT_TOT_CENTRO-PAR05 TO TOTALAUX-PAR05.
      ADD IT_TOT_CENTRO-PAR06 TO TOTALAUX-PAR06.
      ADD IT_TOT_CENTRO-PAR07 TO TOTALAUX-PAR07.
      ADD IT_TOT_CENTRO-PAR08 TO TOTALAUX-PAR08.
      ADD IT_TOT_CENTRO-PAR09 TO TOTALAUX-PAR09.
      ADD IT_TOT_CENTRO-PAR10 TO TOTALAUX-PAR10.
      ADD IT_TOT_CENTRO-PAR11 TO TOTALAUX-PAR11.
      ADD IT_TOT_CENTRO-PAR12 TO TOTALAUX-PAR12.
      ADD IT_TOT_CENTRO-PAR13 TO TOTALAUX-PAR13.
    ENDIF.
  ENDLOOP.

  APPEND TOTALAUX TO IT_TOT_GERAL_SMART.
  CLEAR TOTALAUX.
  CLEAR IT_TOT_CENTRO.


  FREE IT_TOT_GERAL.
  IT_TOT_GERAL[] = IT_TOT_GERAL_SMART[].



  DATA: VL_FM_NAME TYPE RS38L_FNAM.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = C_FORM
    IMPORTING
      FM_NAME            = VL_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  CALL FUNCTION VL_FM_NAME
    EXPORTING
      P_DATES_LOW      = DTINI
      P_DATES_HIGH     = DTFIM
    TABLES
      IT_TOT_EMPRESA   = IT_TOT_EMPRESA[]
      IT_SAIDA         = IT_TOT_GERAL[]
      IT_DETALHE       = IT_DETALHE_SMART[]
      IT_ORDERNA       = IT_ORDERNA
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      USER_CANCELED    = 4
      OTHERS           = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_TOPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_CUSTOM_CONTAINER  text
*----------------------------------------------------------------------*
FORM SET_TOPO USING G_CUSTOM_CONTAINER P_DIR.

*  CLEAR G_CUSTOM_CONTAINER.
*  FREE DG_SPLITTER_1.
*  FREE DG_PARENT_1.
*  FREE DG_PARENT_ALV.
*  FREE DG_PARENT_ALV2.
*  FREE DG_SPLITTER_2.
*  FREE DG_PARENT_2.
*  FREE DG_PARENT_2A.
*  FREE PICTURE.
*  FREE DG_PARENT_2A.
*  FREE URL.

  IF P_DIR IS NOT INITIAL.
    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.
  ELSE.
    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.
  ENDIF.

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

  IF P_DIR IS NOT INITIAL.
    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_ALV2.
  ENDIF.

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
      HEIGHT = 20.

  CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
    EXPORTING
      ID    = 1
      WIDTH = 90.

*  ---------------------------------------------------------------------

*  IF P_DIR IS NOT INITIAL.
*    CALL METHOD DG_SPLITTER_1->SET_ROW_HEIGHT
*      EXPORTING
*        ID     = 2
*        HEIGHT = 16.
*
*    CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
*      EXPORTING
*        ID    = 2
*        WIDTH = 50.
*  ENDIF.
*----------------------------------------------------------
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


FORM SET_TOPO_DETALHE. "USING G_CUSTOM_CONTAINER2 P_DIRE.

*  IF P_DIRE IS NOT INITIAL.
  CREATE OBJECT DG_SPLITTER_3
    EXPORTING
      PARENT  = G_CUSTOM_CONTAINER2
      ROWS    = 2
      COLUMNS = 1.
*  ELSE.
*    CREATE OBJECT DG_SPLITTER_3
*      EXPORTING
*        PARENT  = G_CUSTOM_CONTAINER2
*        ROWS    = 2
*        COLUMNS = 1.
*  ENDIF.

  CALL METHOD DG_SPLITTER_3->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = DG_PARENT_3.

  CALL METHOD DG_SPLITTER_3->GET_CONTAINER
    EXPORTING
      ROW       = 2
      COLUMN    = 1
    RECEIVING
      CONTAINER = DG_PARENT_ALV3.

*  IF P_DIRE IS NOT INITIAL.
*    CALL METHOD DG_SPLITTER_3->GET_CONTAINER
*      EXPORTING
*        ROW       = 2
*        COLUMN    = 1
*      RECEIVING
*        CONTAINER = DG_PARENT_ALV4.
*  ENDIF.

  CREATE OBJECT DG_SPLITTER_4
    EXPORTING
      PARENT  = DG_PARENT_3
      ROWS    = 1
      COLUMNS = 2.

  CALL METHOD DG_SPLITTER_4->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = DG_PARENT_4.

  CALL METHOD DG_SPLITTER_4->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 2
    RECEIVING
      CONTAINER = DG_PARENT_2A2.

  CALL METHOD DG_SPLITTER_3->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 20.

  CALL METHOD DG_SPLITTER_4->SET_COLUMN_WIDTH
    EXPORTING
      ID    = 1
      WIDTH = 90.

*  ---------------------------------------------------------------------

*  IF P_DIR IS NOT INITIAL.
*    CALL METHOD DG_SPLITTER_1->SET_ROW_HEIGHT
*      EXPORTING
*        ID     = 2
*        HEIGHT = 16.
*
*    CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
*      EXPORTING
*        ID    = 2
*        WIDTH = 50.
*  ENDIF.
*----------------------------------------------------------
  CREATE OBJECT PICTURE2
    EXPORTING
      PARENT = DG_PARENT_2A2.

  PERFORM F_PEGA_IMAGEM USING 'LOGO_NOVO' CHANGING URL2.

  CALL METHOD PICTURE2->LOAD_PICTURE_FROM_URL
    EXPORTING
      URL = URL2.

  CALL METHOD PICTURE2->SET_DISPLAY_MODE
    EXPORTING
      DISPLAY_MODE = PICTURE2->DISPLAY_MODE_FIT_CENTER.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTA_TOPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTA_TOPO .

*  FREE DG_DYNDOC_ID.
*  FREE COLUMN.
*  FREE TABLE_ELEMENT.
*  FREE TABLE_ELEMENT2.
*  FREE COLUMN_1.



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

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_ITENS_TOPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_ITENS_TOPO.

  CLEAR: P_TEXT_TABLE.

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

  IF P_SWERK IS NOT INITIAL.
    LOOP AT P_SWERK.
      IF P_SWERK-OPTION NE 'EQ' AND P_SWERK-OPTION NE 'BT'.
        SDYDO_TEXT_ELEMENT = 'Centro: Multiplas Seleções'.
        EXIT.
      ELSEIF P_SWERK-OPTION EQ 'BT'.
        CONCATENATE 'Centro:' P_SWERK-LOW '-' P_SWERK-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
        EXIT.
      ELSE.
        VL_CONT = VL_CONT + 1.
        IF VL_CONT GT 1.
          SDYDO_TEXT_ELEMENT = 'Centro: Multiplas Seleções'.
        ELSE.
          CONCATENATE 'Centro:' P_SWERK-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
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

  CONCATENATE DTINI+6(2) '.' DTINI+4(2) '.' DTINI(4) INTO VL_DATES1.
  CONCATENATE DTFIM+6(2) '.' DTFIM+4(2) '.' DTFIM(4) INTO VL_DATES2.
  CONCATENATE 'Período:' VL_DATES1 '-' VL_DATES2 INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.

  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
  CLEAR: SDYDO_TEXT_ELEMENT, VL_DATES1, VL_DATES2.

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

FORM ADD_ITENS_TOPO_DETALHE.

  CLEAR: P_TEXT_TABLE2.
  CLEAR: IT_VIQSAIDA.

  DATA: P_COLUMN TYPE CHAR30.


  LOOP AT IT_VIQSAIDA INTO WA_VIQSAIDA.
    IF WA_VIQSAIDA-BUKRS IS NOT INITIAL.
      SELECT SINGLE BUTXT
        FROM T001
        INTO VL_BUTXT2
        WHERE BUKRS EQ WA_VIQSAIDA-BUKRS
          AND SPRAS EQ SY-LANGU.

      CONCATENATE 'Empresa:' WA_VIQSAIDA-BUKRS VL_BUTXT2 INTO SDYDO_TEXT_ELEMENT2 SEPARATED BY SPACE.
      CLEAR: VL_BUTXT2.
      EXIT.
    ENDIF.
  ENDLOOP.

  APPEND SDYDO_TEXT_ELEMENT2 TO P_TEXT_TABLE2.
  CLEAR: VL_CONT2, VL_BUTXT2, SDYDO_TEXT_ELEMENT2.

  LOOP AT IT_VIQSAIDA INTO WA_VIQSAIDA..
    IF WA_VIQSAIDA-SWERK IS NOT INITIAL.
      CONCATENATE 'Centro:' WA_VIQSAIDA-SWERK INTO SDYDO_TEXT_ELEMENT2 SEPARATED BY SPACE.
      EXIT.
    ENDIF.
  ENDLOOP.

  APPEND SDYDO_TEXT_ELEMENT2 TO P_TEXT_TABLE2.
  CLEAR: VL_CONT2, SDYDO_TEXT_ELEMENT2.


  CALL METHOD COLUMN_4->ADD_TEXT
    EXPORTING
      TEXT_TABLE = P_TEXT_TABLE2
      FIX_LINES  = 'X'.

  CALL METHOD DG_DYNDOC_ID2->MERGE_DOCUMENT.

  CREATE OBJECT DG_HTML_CNTRL2
    EXPORTING
      PARENT = DG_PARENT_4.

  DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL2.

  CALL METHOD DG_DYNDOC_ID2->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = DG_PARENT_4
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALC_MES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_MES USING V_MES.

  DATA: _DAY   TYPE I,
        _MONTH TYPE I,
        _YEAR  TYPE I,
        _MES   TYPE N LENGTH 2.

  DATA: _DTINI TYPE SY-DATUM,
        _DTFIM TYPE SY-DATUM,
        PARADA TYPE ZPME_INDISPON-PAR01.

  FIELD-SYMBOLS: <FS_CAMPO> TYPE ANY.

  _MONTH = V_MES.
  _YEAR = P_ANO-LOW.

  _MES = |{ V_MES ALPHA = IN }|.

  CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
    EXPORTING
      I_DATE_MONTH = _MONTH
      I_DATE_YEAR  = _YEAR
    IMPORTING
      E_MAX_DAYS   = _DAY.

  DATA(FIELD) = |PAR{ V_MES }|.

  ASSIGN COMPONENT FIELD OF STRUCTURE <WA_SAIDA_AUX> TO <FS_CAMPO>.
  PARADA = <FS_CAMPO>.

  DATA(OPERACAO) = ( _DAY * 24 ) * 60.
  <FS_CAMPO> = ( PARADA / OPERACAO ) * 100.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AJUSTA_TOTAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AJUSTA_TOTAIS .

  DATA: LREF_DATA TYPE REF TO DATA.
  DATA: VL_CONTSUM TYPE P DECIMALS 2.
  FIELD-SYMBOLS: <L_SUM_TAB> TYPE TABLE,
                 <L_SUM>     TYPE TY_SAIDA.

*  "Ajuste de valores da linha de TOTAIS
  CALL METHOD CTL_ALV->GET_SUBTOTALS
    IMPORTING
      EP_COLLECT00 = LREF_DATA.

  ASSIGN LREF_DATA->* TO <L_SUM_TAB>.
  IF <L_SUM_TAB> IS ASSIGNED.
    READ TABLE <L_SUM_TAB> ASSIGNING <L_SUM> INDEX 1.
    IF SY-SUBRC EQ 0.
      CLEAR: <L_SUM>.
      LOOP AT IT_SAIDA INTO WA_SAIDA.
        VL_CONTSUM = VL_CONTSUM + 1.
        <L_SUM>-PAR01 = <L_SUM>-PAR01 + WA_SAIDA-PAR01.
        <L_SUM>-PAR02 = <L_SUM>-PAR02 + WA_SAIDA-PAR02.
        <L_SUM>-PAR03 = <L_SUM>-PAR03 + WA_SAIDA-PAR03.
        <L_SUM>-PAR04 = <L_SUM>-PAR04 + WA_SAIDA-PAR04.
        <L_SUM>-PAR05 = <L_SUM>-PAR05 + WA_SAIDA-PAR05.
        <L_SUM>-PAR06 = <L_SUM>-PAR06 + WA_SAIDA-PAR06.
        <L_SUM>-PAR07 = <L_SUM>-PAR07 + WA_SAIDA-PAR07.
        <L_SUM>-PAR08 = <L_SUM>-PAR08 + WA_SAIDA-PAR08.
        <L_SUM>-PAR09 = <L_SUM>-PAR09 + WA_SAIDA-PAR09.
        <L_SUM>-PAR10 = <L_SUM>-PAR10 + WA_SAIDA-PAR10.
        <L_SUM>-PAR11 = <L_SUM>-PAR11 + WA_SAIDA-PAR11.
        <L_SUM>-PAR12 = <L_SUM>-PAR12 + WA_SAIDA-PAR12.
        <L_SUM>-PAR13 = <L_SUM>-PAR13 + WA_SAIDA-PAR13.
      ENDLOOP.

*      <L_SUM>-PAR01 = <L_SUM>-PAR01 / VL_CONTSUM.
*      <L_SUM>-PAR02 = <L_SUM>-PAR02 / VL_CONTSUM.
*      <L_SUM>-PAR03 = <L_SUM>-PAR03 / VL_CONTSUM.
*      <L_SUM>-PAR04 = <L_SUM>-PAR04 / VL_CONTSUM.
*      <L_SUM>-PAR05 = <L_SUM>-PAR05 / VL_CONTSUM.
*      <L_SUM>-PAR06 = <L_SUM>-PAR06 / VL_CONTSUM.
*      <L_SUM>-PAR07 = <L_SUM>-PAR07 / VL_CONTSUM.
*      <L_SUM>-PAR08 = <L_SUM>-PAR08 / VL_CONTSUM.
*      <L_SUM>-PAR09 = <L_SUM>-PAR09 / VL_CONTSUM.
*      <L_SUM>-PAR10 = <L_SUM>-PAR10 / VL_CONTSUM.
*      <L_SUM>-PAR11 = <L_SUM>-PAR11 / VL_CONTSUM.
*      <L_SUM>-PAR12 = <L_SUM>-PAR12 / VL_CONTSUM.
*      <L_SUM>-PAR13 = <L_SUM>-PAR13 / VL_CONTSUM.
*      CLEAR: VL_CONTSUM.


      CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
        EXPORTING
          I_SOFT_REFRESH = 'X'.

    ENDIF.
  ENDIF.

*  CLEAR: LREF_DATA.
*  "Ajuste de valores da linha de SUB-TOTAIS Nível 01
*  CALL METHOD CTL_ALV->GET_SUBTOTALS
*    IMPORTING
*      EP_COLLECT01 = LREF_DATA.
*
*  ASSIGN LREF_DATA->* TO <L_SUM_TAB>.
*  IF <L_SUM_TAB> IS ASSIGNED.
*    LOOP AT <L_SUM_TAB> ASSIGNING <L_SUM>.
*      IF <L_SUM> IS ASSIGNED.
*
*        CLEAR: <L_SUM>-PAR01, <L_SUM>-PAR02, <L_SUM>-PAR03, <L_SUM>-PAR04, <L_SUM>-PAR05, <L_SUM>-PAR06, <L_SUM>-PAR07.
*        CLEAR: <L_SUM>-PAR08, <L_SUM>-PAR09, <L_SUM>-PAR10, <L_SUM>-PAR11, <L_SUM>-PAR12, <L_SUM>-PAR13.
*
*        LOOP AT IT_SAIDA INTO WA_SAIDA
*               WHERE SWERK EQ <L_SUM>-SWERK.
*          VL_CONTSUM = VL_CONTSUM + 1.
*          <L_SUM>-PAR01 = <L_SUM>-PAR01 + WA_SAIDA-PAR01.
*          <L_SUM>-PAR02 = <L_SUM>-PAR02 + WA_SAIDA-PAR02.
*          <L_SUM>-PAR03 = <L_SUM>-PAR03 + WA_SAIDA-PAR03.
*          <L_SUM>-PAR04 = <L_SUM>-PAR04 + WA_SAIDA-PAR04.
*          <L_SUM>-PAR05 = <L_SUM>-PAR05 + WA_SAIDA-PAR05.
*          <L_SUM>-PAR06 = <L_SUM>-PAR06 + WA_SAIDA-PAR06.
*          <L_SUM>-PAR07 = <L_SUM>-PAR07 + WA_SAIDA-PAR07.
*          <L_SUM>-PAR08 = <L_SUM>-PAR08 + WA_SAIDA-PAR08.
*          <L_SUM>-PAR09 = <L_SUM>-PAR09 + WA_SAIDA-PAR09.
*          <L_SUM>-PAR10 = <L_SUM>-PAR10 + WA_SAIDA-PAR10.
*          <L_SUM>-PAR11 = <L_SUM>-PAR11 + WA_SAIDA-PAR11.
*          <L_SUM>-PAR12 = <L_SUM>-PAR12 + WA_SAIDA-PAR12.
*          <L_SUM>-PAR13 = <L_SUM>-PAR13 + WA_SAIDA-PAR13.
*        ENDLOOP.
*
**        <L_SUM>-PAR01 = <L_SUM>-PAR01 / VL_CONTSUM.
**        <L_SUM>-PAR02 = <L_SUM>-PAR02 / VL_CONTSUM.
**        <L_SUM>-PAR03 = <L_SUM>-PAR03 / VL_CONTSUM.
**        <L_SUM>-PAR04 = <L_SUM>-PAR04 / VL_CONTSUM.
**        <L_SUM>-PAR05 = <L_SUM>-PAR05 / VL_CONTSUM.
**        <L_SUM>-PAR06 = <L_SUM>-PAR06 / VL_CONTSUM.
**        <L_SUM>-PAR07 = <L_SUM>-PAR07 / VL_CONTSUM.
**        <L_SUM>-PAR08 = <L_SUM>-PAR08 / VL_CONTSUM.
**        <L_SUM>-PAR09 = <L_SUM>-PAR09 / VL_CONTSUM.
**        <L_SUM>-PAR10 = <L_SUM>-PAR10 / VL_CONTSUM.
**        <L_SUM>-PAR11 = <L_SUM>-PAR11 / VL_CONTSUM.
**        <L_SUM>-PAR12 = <L_SUM>-PAR12 / VL_CONTSUM.
**        <L_SUM>-PAR13 = <L_SUM>-PAR13 / VL_CONTSUM.
*        CLEAR: VL_CONTSUM.
*      ENDIF.
*    ENDLOOP.
*
*  ENDIF.
*
*  CLEAR: LREF_DATA.
*  "Ajuste de valores da linha de SUB-TOTAIS Nível 02
*  CALL METHOD CTL_ALV->GET_SUBTOTALS
*    IMPORTING
*      EP_COLLECT02 = LREF_DATA.
*
*  ASSIGN LREF_DATA->* TO <L_SUM_TAB>.
*  IF <L_SUM_TAB> IS ASSIGNED.
*    LOOP AT <L_SUM_TAB> ASSIGNING <L_SUM>.
*      IF <L_SUM> IS ASSIGNED.
*        CLEAR: <L_SUM>-PAR01, <L_SUM>-PAR02, <L_SUM>-PAR03, <L_SUM>-PAR04, <L_SUM>-PAR05, <L_SUM>-PAR06, <L_SUM>-PAR07.
*        CLEAR: <L_SUM>-PAR08, <L_SUM>-PAR09, <L_SUM>-PAR10, <L_SUM>-PAR11, <L_SUM>-PAR12, <L_SUM>-PAR13.
*        LOOP AT IT_SAIDA INTO WA_SAIDA
*          WHERE SWERK EQ <L_SUM>-SWERK
*          AND TPLMA EQ <L_SUM>-TPLMA.
*
*          VL_CONTSUM = VL_CONTSUM + 1.
*          <L_SUM>-PAR01 = <L_SUM>-PAR01 + WA_SAIDA-PAR01.
*          <L_SUM>-PAR02 = <L_SUM>-PAR02 + WA_SAIDA-PAR02.
*          <L_SUM>-PAR03 = <L_SUM>-PAR03 + WA_SAIDA-PAR03.
*          <L_SUM>-PAR04 = <L_SUM>-PAR04 + WA_SAIDA-PAR04.
*          <L_SUM>-PAR05 = <L_SUM>-PAR05 + WA_SAIDA-PAR05.
*          <L_SUM>-PAR06 = <L_SUM>-PAR06 + WA_SAIDA-PAR06.
*          <L_SUM>-PAR07 = <L_SUM>-PAR07 + WA_SAIDA-PAR07.
*          <L_SUM>-PAR08 = <L_SUM>-PAR08 + WA_SAIDA-PAR08.
*          <L_SUM>-PAR09 = <L_SUM>-PAR09 + WA_SAIDA-PAR09.
*          <L_SUM>-PAR10 = <L_SUM>-PAR10 + WA_SAIDA-PAR10.
*          <L_SUM>-PAR11 = <L_SUM>-PAR11 + WA_SAIDA-PAR11.
*          <L_SUM>-PAR12 = <L_SUM>-PAR12 + WA_SAIDA-PAR12.
*          <L_SUM>-PAR13 = <L_SUM>-PAR13 + WA_SAIDA-PAR13.
*        ENDLOOP.
*
**        <L_SUM>-PAR01 = <L_SUM>-PAR01 / VL_CONTSUM.
**        <L_SUM>-PAR02 = <L_SUM>-PAR02 / VL_CONTSUM.
**        <L_SUM>-PAR03 = <L_SUM>-PAR03 / VL_CONTSUM.
**        <L_SUM>-PAR04 = <L_SUM>-PAR04 / VL_CONTSUM.
**        <L_SUM>-PAR05 = <L_SUM>-PAR05 / VL_CONTSUM.
**        <L_SUM>-PAR06 = <L_SUM>-PAR06 / VL_CONTSUM.
**        <L_SUM>-PAR07 = <L_SUM>-PAR07 / VL_CONTSUM.
**        <L_SUM>-PAR08 = <L_SUM>-PAR08 / VL_CONTSUM.
**        <L_SUM>-PAR09 = <L_SUM>-PAR09 / VL_CONTSUM.
**        <L_SUM>-PAR10 = <L_SUM>-PAR10 / VL_CONTSUM.
**        <L_SUM>-PAR11 = <L_SUM>-PAR11 / VL_CONTSUM.
**        <L_SUM>-PAR12 = <L_SUM>-PAR12 / VL_CONTSUM.
**        <L_SUM>-PAR13 = <L_SUM>-PAR13 / VL_CONTSUM.
*        CLEAR: VL_CONTSUM.
*      ENDIF.
*    ENDLOOP.
*
*  ENDIF.

  CALL METHOD CTL_ALV->GET_FRONTEND_LAYOUT
    IMPORTING
      ES_LAYOUT = GS_LAYOUT.

  GS_LAYOUT-CWIDTH_OPT = 'X'.

  CALL METHOD CTL_ALV->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = GS_LAYOUT.

  CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
    EXPORTING
      I_SOFT_REFRESH = 'X'.
ENDFORM.
