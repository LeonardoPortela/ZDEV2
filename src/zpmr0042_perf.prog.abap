*&---------------------------------------------------------------------*
*&  Include           ZPMR0042_PERF
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'SET_PFST001'.
  SET TITLEBAR 'SET_TIT001'.

  PERFORM FILL_IT_FIELDCATALOG USING:
      01 'IWERK       '     ' '    '15'  ' '     ' '   ' '   'Centro                    '  ''  '' ,
      02 'KOSTL       '     ' '    '15'  ' '     ' '   ' '   'Centro de Custo           '  ''  '' ,
      03 'GJAHR       '     ' '    '15'  ' '     ' '   ' '   'Ano                       '  ''  '' ,
      04 'PERIO       '     ' '    '15'  ' '     ' '   ' '   'Período                   '  ''  '' ,

      05 'MATNR       '     ' '    '15'  ' '     ' '   ' '   'Nº do material            '  ''  '' ,
      06 'MAKTX       '     ' '    '40'  ' '     ' '   ' '   'Texto breve de material   '  ''  '' ,
      07 'MEINS       '     ' '    '08'  ' '     ' '   ' '   'Und medida                '  ''  '' ,
      08 'QUAN_VEIC   '     ' '    '08'  ' '     ' '   ' '   'Quantidade/veiculo        '  ''  '' ,
      09 'MENGE       '     ' '    '08'  ' '     ' '   ' '   'Volume                    '  ''  '' ,
*      08 'PORC_ULT    '     ' '    '08'  ' '     ' '   ' '   'Média geral               '  ''  '' ,
      10 'QUAN_HR     '     ' '    '23'  ' '     ' '   ' '   'Hr/Km                     '  ''  '' ,
      11 'CONS_MEDIO  '     ' '    '08'  ' '     ' '   ' '   'Consumo médio             '  ''  '' .


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

    PERFORM CABECARIO USING G_CONTAINER.

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
        IT_OUTTAB            = ZCL_ORDEM->GT_ORDEM.
    IT_SORT              = IT_SORT.

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

    PERFORM CABECARIO_ALV.

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
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_01     text
*      -->P_0014   text
*      -->P_0015   text
*      -->P_0016   text
*      -->P_0017   text
*      -->P_0018   text
*      -->P_0019   text
*      -->P_0020   text
*      -->P_0021   text
*      -->P_0022   text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG  USING    VALUE(P_COLNUM)
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
*&      Form  TOT_HORAS_KM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<_ORDEM>_EQUNR  text
*      <--P_DATA(TOT_HORA)  text
*----------------------------------------------------------------------*
FORM TOT_HORAS_KM  USING    P_EQUNR
                   CHANGING P_HORA T_CONTADOR.
  DATA: IDAT1 TYPE IDATE.
  DATA: IDAT2 TYPE IDATE.
  CLEAR: P_HORA, FLTP_CHAR.

  DATA(ANO) = P_GJAHR-LOW.
  DATA(DATA_LOW)  = P_PERIO-LOW+1(2).
  DATA(DATA_HIGH) = P_PERIO-HIGH+1(2).

  IDAT1 =  |{ ANO }{ DATA_LOW }01|.
  IDAT2 =  |{ ANO }{ DATA_HIGH }31|.

  FREE GS_IMRG.
  SELECT *
  FROM IMRG AS A
  INNER JOIN IMPTT AS B ON B~POINT EQ A~POINT
  INNER JOIN EQUI AS C ON C~OBJNR EQ B~MPOBJ AND C~EQUNR EQ P_EQUNR
    INTO CORRESPONDING FIELDS OF TABLE GS_IMRG
    WHERE B~MPTYP EQ 'V'
      AND A~CANCL EQ ABAP_FALSE
      AND B~INDTR EQ ABAP_FALSE
      AND A~IDATE BETWEEN IDAT1 AND IDAT2.

  IF GS_IMRG IS NOT INITIAL.
    LOOP AT GS_IMRG INTO DATA(W_IMRG).
      IF SY-SUBRC = 0.
        CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
          EXPORTING
            CHAR_UNIT       = W_IMRG-RECDU
            DECIMALS        = 0
            EXPONENT        = 0
            FLTP_VALUE_SI   = W_IMRG-CDIFF
            INDICATOR_VALUE = 'X'
            MASC_SYMBOL     = ' '
          IMPORTING
            CHAR_VALUE      = FLTP_CHAR
          EXCEPTIONS
            NO_UNIT_GIVEN   = 01.
        CONDENSE FLTP_CHAR.
      ENDIF.
      ADD FLTP_CHAR TO P_HORA.
      T_CONTADOR =  W_IMRG-RECDU.
      CONDENSE P_HORA.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CABECARIO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CABECARIO_ALV .

  IF P_IWERK[] IS NOT INITIAL.
    CONCATENATE 'CENTRO:' P_IWERK-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: SDYDO_TEXT_ELEMENT.
  ENDIF.

  IF P_KOSTL[] IS NOT INITIAL.

    SELECT SINGLE *
    FROM CSKT
    INTO @DATA(S_CSKT)
      WHERE KOSTL IN @P_KOSTL.
    DATA(TXT_KOSTL) = S_CSKT-KTEXT.


    CONCATENATE 'CENTRO DE CUSTO:' P_KOSTL-LOW '-' TXT_KOSTL INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: SDYDO_TEXT_ELEMENT.

  ENDIF.

  IF P_GJAHR[] IS NOT INITIAL.
    CONCATENATE 'ANO:' P_GJAHR-LOW INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: SDYDO_TEXT_ELEMENT.
  ENDIF.


  IF P_PERIO IS NOT INITIAL.

    P_PERIO-LOW = |{ P_PERIO-LOW ALPHA = OUT }|.
    P_PERIO-HIGH = |{ P_PERIO-HIGH ALPHA = OUT }|.

    CONCATENATE 'MÊS:' P_PERIO-LOW 'a' P_PERIO-HIGH INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: SDYDO_TEXT_ELEMENT.

    EXIT.
  ELSE.

    CONCATENATE 'MÊS:' P_PERIO-LOW  INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR: SDYDO_TEXT_ELEMENT.
    EXIT.
  ENDIF.





ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CABECARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_CONTAINER  text
*      -->P_CREATE  text
*      -->P_OBJECT  text
*      -->P_CTL_ALV  text
*      -->P_EXPORTING  text
*      -->P_I_PARENT  text
*      -->P_=  text
*      -->P_G_CONTAINER  text
*----------------------------------------------------------------------*
FORM CABECARIO  USING CTL_ALV.


  CREATE OBJECT DG_SPLITTER_1
    EXPORTING
      PARENT  = CTL_ALV
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
      HEIGHT = 20.

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
*      -->P_0844   text
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
ENDFORM.
