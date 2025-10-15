*----------------------------------------------------------------------*
***INCLUDE ZPM_DIAG_POR_PLANTA_TELA_100.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'. LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANC'. LEAVE PROGRAM.
    WHEN 'GRAF'. PERFORM ZF_GRAFICO.
  ENDCASE.
ENDMODULE.

"fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  INITIAL_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_0100 OUTPUT.

  IF O_DOCKING IS NOT BOUND.

    CREATE OBJECT O_DOCKING
      EXPORTING
        REPID                       = SY-REPID
        DYNNR                       = SY-DYNNR
        EXTENSION                   = 2000 " Tela Full " Tamanho da Tela
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

*&---------------------------------------------------------------------*
    CREATE OBJECT O_SPLITTER
      EXPORTING
*       link_dynnr        =     " Screen Number
*       link_repid        =     " Report Name
*       shellstyle        =     " Window Style
*       left              =     " Left-aligned
*       top               =     " top
*       width             =     " NPlWidth
*       height            =     " Hght
*       metric            = CNTL_METRIC_DYNPRO    " Metric
*       align             = 15    " Alignment
        PARENT            = O_DOCKING    " Parent Container
        ROWS              = 3   " Number of Rows to be displayed
        COLUMNS           = 1  " Number of Columns to be Displayed
*       no_autodef_progid_dynnr =     " Don't Autodefined Progid and Dynnr?
*       name              =     " Name
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3.
  ENDIF.

* Definição das medidas dos containers
  PERFORM F_DEFINE_CONTAINER_EXTENSION
*         Row column Height Width
    USING '1' '1'    '40'   '70'.

*&---------------------------------------------------------------------*
* Container Header ( 1,1 )
*&---------------------------------------------------------------------*
  PERFORM F_CREATE_CONTAINER
*         Row Column  Container
    USING '1' '1'     O_CONTAINER_1.
  PERFORM F_DEFINE_CONTAINER_HEADER.

*&---------------------------------------------------------------------*
* Container Item ( 1,2 )
*&---------------------------------------------------------------------*
  PERFORM F_CREATE_CONTAINER
*         Row Column  Container
    USING '3' '1'     O_CONTAINER_2.
  PERFORM F_DEFINE_CONTAINER_ITEM.

ENDMODULE.                 " USER_COMMAND_9001  INPUT
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_DEFINE_CONTAINER_EXTENSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0093   text
*      -->P_0094   text
*      -->P_0095   text
*      -->P_0096   text
*----------------------------------------------------------------------*
FORM F_DEFINE_CONTAINER_EXTENSION USING ROW    TYPE I
                                        COLUMN TYPE I
                                        HEIGHT TYPE I
                                        WIDTH  TYPE I.
* Altura
  IF ROW IS NOT INITIAL
    AND HEIGHT IS NOT INITIAL.
    O_SPLITTER->SET_ROW_HEIGHT(
      EXPORTING
        ID     = ROW
        HEIGHT = HEIGHT
        ).
  ENDIF.

* Largura
  IF COLUMN IS NOT INITIAL
    AND WIDTH IS NOT INITIAL.
    O_SPLITTER->SET_COLUMN_WIDTH(
      EXPORTING
        ID    = COLUMN
        WIDTH = WIDTH
    ).
  ENDIF.

ENDFORM.                    " F_DEFINE_CONTAINER_EXTENSION
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0100   text
*      -->P_0101   text
*      -->P_O_CONTAINER_1  text
*----------------------------------------------------------------------*
FORM F_CREATE_CONTAINER  USING ROW       TYPE I
                               COL       TYPE I
                               CONTAINER TYPE REF TO CL_GUI_CONTAINER.

* Aqui é feito a criação do container com referencia a uma das partições.
  O_SPLITTER->GET_CONTAINER(
    EXPORTING
      ROW    = ROW
      COLUMN = COL
    RECEIVING
      CONTAINER = CONTAINER
      ).


ENDFORM.                    " F_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  F_DEFINE_CONTAINER_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DEFINE_CONTAINER_HEADER .

  DATA:
    LST_LAYOUT TYPE LVC_S_LAYO.

  IF O_DOCKING IS BOUND
    AND O_ALV_1 IS NOT BOUND.

* Cria o ALV OO
    CREATE OBJECT O_ALV_1
      EXPORTING
        I_PARENT          = O_CONTAINER_1   " Parent Container
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    PERFORM F_FIELDCAT
      TABLES  T_FCAT_1
      USING : 'IWERK'    'IT_REG' 'IWERK'    'ZSPM_DIAG_POR_PLANTA',
              'FING'     'IT_REG' 'FING'     'ZSPM_DIAG_POR_PLANTA',
              'FECOD'    'IT_REG' ''    '',
              'BTXTCDFE' 'IT_REG' 'BTXTCDFE' 'ZSPM_DIAG_POR_PLANTA',
              'QTDDIAG'  'IT_REG' 'QTDDIAG'  'ZSPM_DIAG_POR_PLANTA',
              'QTDPERC'  'IT_REG' ''  ''.
*
    LOOP AT T_FCAT_1 INTO W_FCAT_1.
      IF W_FCAT_1-FIELDNAME = LC_QTDPERC.
        W_FCAT_1-REPTEXT = '% Quant.'.
        MODIFY T_FCAT_1 FROM W_FCAT_1.
      ELSEIF W_FCAT_1-FIELDNAME = 'FECOD'.
        W_FCAT_1-REPTEXT = 'Código'.
        MODIFY T_FCAT_1 FROM W_FCAT_1.
      ENDIF.
    ENDLOOP.

    LST_LAYOUT-ZEBRA      = 'X'.
    LST_LAYOUT-CWIDTH_OPT = 'X'.

    SORT IT_REG ASCENDING BY IWERK FING FECOD.
    T_OUT_1[] = IT_REG[].
* Exibe o ALV OO
    O_ALV_1->SET_TABLE_FOR_FIRST_DISPLAY(
      EXPORTING
        IS_LAYOUT                     =  LST_LAYOUT   " Layout
     CHANGING
        IT_OUTTAB                     =  T_OUT_1        " Output Table
        IT_FIELDCATALOG               =  T_FCAT_1   " Field Catalog
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4
    ).

  ELSE.

* Atualiza o ALV OO
    O_ALV_1->REFRESH_TABLE_DISPLAY(
      EXCEPTIONS
        FINISHED       = 1
        OTHERS         = 2
    ).

  ENDIF.

ENDFORM.                    " F_DEFINE_CONTAINER_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_DEFINE_CONTAINER_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DEFINE_CONTAINER_ITEM .

  DATA:
    LST_LAYOUT TYPE LVC_S_LAYO.

  IF O_DOCKING IS BOUND
    AND O_ALV_2 IS NOT BOUND.

    CREATE OBJECT O_ALV_2
      EXPORTING
        I_PARENT          = O_CONTAINER_2   " Parent Container
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    PERFORM F_FIELDCAT
      TABLES  T_FCAT_2
      USING : 'FECOD'    'IT_REG' ''    '',
              'BTXTCDFE' 'IT_REG' 'BTXTCDFE' 'ZSPM_DIAG_POR_PLANTA',
              'QTDDIAG'  'IT_REG' 'QTDDIAG'  'ZSPM_DIAG_POR_PLANTA',
              'QTDPERC'  'IT_REG' ''  ''.
*
    LOOP AT T_FCAT_2 INTO W_FCAT_2.
      IF W_FCAT_2-FIELDNAME = LC_QTDDIAG.
        W_FCAT_2-REPTEXT = LC_NOPS.
        MODIFY T_FCAT_2 FROM W_FCAT_2.
      ELSEIF W_FCAT_2-FIELDNAME = 'FECOD'.
        W_FCAT_2-REPTEXT = 'Código'.
        MODIFY T_FCAT_2 FROM W_FCAT_2.
      ELSEIF W_FCAT_2-FIELDNAME = LC_QTDPERC.
        W_FCAT_2-REPTEXT = '% Quant.'.
        MODIFY T_FCAT_2 FROM W_FCAT_2.
      ENDIF.
    ENDLOOP.

    LST_LAYOUT-ZEBRA      = 'X'.
    LST_LAYOUT-CWIDTH_OPT = 'X'.

    CLEAR T_OUT_2[].
    LOOP AT IT_REG.
      CLEAR W_OUT_2.
      W_OUT_2-FECOD    = IT_REG-FECOD.
      W_OUT_2-BTXTCDFE = IT_REG-BTXTCDFE.
      W_OUT_2-QTDDIAG  = IT_REG-QTDDIAG.
      W_OUT_2-QTDPERC  = IT_REG-QTDPERC.
      COLLECT W_OUT_2 INTO T_OUT_2.
    ENDLOOP.

    SORT T_OUT_2 DESCENDING BY QTDDIAG.

    O_ALV_2->SET_TABLE_FOR_FIRST_DISPLAY(
      EXPORTING
        IS_LAYOUT                     =  LST_LAYOUT   " Layout
      CHANGING
        IT_OUTTAB                     = T_OUT_2    " Output Table
        IT_FIELDCATALOG               = T_FCAT_2    " Field Catalog
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4
    ).

  ELSE.
    O_ALV_2->REFRESH_TABLE_DISPLAY(
  EXCEPTIONS
    FINISHED       = 1
    OTHERS         = 2
).
  ENDIF.

ENDFORM.                    " F_DEFINE_CONTAINER_ITEM
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_FCAT_2  text
*      -->P_0241   text
*      -->P_0242   text
*      -->P_0243   text
*      -->P_0244   text
*----------------------------------------------------------------------*
FORM F_FIELDCAT TABLES FIELDCAT
  USING FIELDNAME TABNAME REF_FIELD REF_TABLE .

* Criação do Fieldcat
  DATA :
    S_FCAT TYPE LVC_S_FCAT.

  S_FCAT-FIELDNAME  = FIELDNAME.
  S_FCAT-TABNAME    = TABNAME.
  S_FCAT-REF_FIELD  = REF_FIELD.
  S_FCAT-REF_TABLE  = REF_TABLE.

  APPEND S_FCAT TO FIELDCAT.

ENDFORM.                    " F_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAFICO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_GRAFICO .
*
  IF T_OUT_2[] IS INITIAL.
    LEAVE TO SCREEN 0.
  ENDIF.

*  ITAB_OPTIONS-OPTION = 'FIFRST = 3D'. APPEND ITAB_OPTIONS. "// Grafik-Typ
*  ITAB_OPTIONS-OPTION = 'P3TYPE = TO'. APPEND ITAB_OPTIONS. "// Objektart
*  ITAB_OPTIONS-OPTION = 'P3CTYP = RO'. APPEND ITAB_OPTIONS. "// Farben der Objekte
*  ITAB_OPTIONS-OPTION = 'TISIZE = 2'. APPEND ITAB_OPTIONS. "// Haupttitelgr?
*  ITAB_OPTIONS-OPTION = 'CLBACK = X'. APPEND ITAB_OPTIONS. "// Background Color

  DATA: BEGIN OF TAB OCCURS 5,
          CLASS(5) TYPE C,
          VAL1     TYPE I,
        END OF TAB.
*
  LOOP AT T_OUT_2 INTO W_OUT_2.
    CLEAR TAB.
    TAB-CLASS = W_OUT_2-FECOD.
    TAB-VAL1  = W_OUT_2-QTDDIAG.
    APPEND TAB.
  ENDLOOP.
  SORT TAB DESCENDING BY VAL1.

  LV_COL = '45'.
  LV_ROW = '55'.

  CALL FUNCTION 'GRAPH_MATRIX_3D'
    EXPORTING
*     AUTO_CMD_1        = ' '
*     AUTO_CMD_2        = ' '
      COL1    = 'Defeitos'
*     COL2    = 'Centro '
*     DIM1    = 'Defeitos'
*     DIM2    = 'YEAR'
      TITL    = 'Relação de Diagnóstico por Planta'
*     VALT    = ' '
*     WDID    = ' '
*     WINID   = ' '
      WINPOS  = '30'
      WINSZX  = '50'
      WINSZY  = '50'
*     X_OPT   = ' '
    IMPORTING
*"              B_KEY
*"              B_TYP
*"              CUA_ID
      MOD_COL = LV_COL
      MOD_ROW = LV_ROW
    TABLES
      DATA    = TAB
      OPTS    = ITAB_OPTIONS
    EXCEPTIONS
      OTHERS  = 1.
*
  LEAVE TO SCREEN 0.
ENDFORM.
