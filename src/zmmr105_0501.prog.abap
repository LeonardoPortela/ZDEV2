*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0501 .
*----------------------------------------------------------------------*
DATA: IT_EXCLUDE_0501  TYPE UI_FUNCTIONS,
      WA_EXCLUDE_0501  LIKE LINE OF IT_EXCLUDE_FCODE,
      CK_VT_CRIADA     TYPE C LENGTH 1,
      CK_TIPO_DIF_TELA TYPE C LENGTH 1,
      CK_EXISTE_REGI   TYPE C LENGTH 1. "C - Calculado de Inicio / A - Ajustado / O - Confirmado

DATA: WA_PESO_LIBERADO    TYPE ZIB_CTE_DIST_LBP,
      WA_CTE_0501         TYPE ZIB_CTE_DIST_TER,
      IT_ZIB_CTE_DIST_LBP TYPE TABLE OF ZIB_CTE_DIST_LBP WITH HEADER LINE.

DATA: IT_0501_N55 TYPE TABLE OF ZIB_CTE_DIST_N55 WITH HEADER LINE,
      IT_0501_N01 TYPE TABLE OF ZIB_CTE_DIST_N01 WITH HEADER LINE.

DATA: CTL_ALV_0501A       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0501A       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0501A        TYPE LVC_S_LAYO,
      GS_VAR_0501A        TYPE DISVARIANT,
      GS_SCROLL_COL_0501A TYPE LVC_S_COL,
      GS_SCROLL_ROW_0501A TYPE LVC_S_ROID,
      IT_CATALOG_0501A    TYPE LVC_T_FCAT.

DATA: CTL_ALV_0501B       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0501B       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0501B        TYPE LVC_S_LAYO,
      GS_VAR_0501B        TYPE DISVARIANT,
      GS_SCROLL_COL_0501B TYPE LVC_S_COL,
      GS_SCROLL_ROW_0501B TYPE LVC_S_ROID,
      IT_CATALOG_0501B    TYPE LVC_T_FCAT.

DATA: CTL_ALV_0501C       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0501C       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0501C        TYPE LVC_S_LAYO,
      GS_VAR_0501C        TYPE DISVARIANT,
      GS_SCROLL_COL_0501C TYPE LVC_S_COL,
      GS_SCROLL_ROW_0501C TYPE LVC_S_ROID,
      IT_CATALOG_0501C    TYPE LVC_T_FCAT.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0501_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0501_EXIT INPUT.
  CLEAR: WA_PESO_LIBERADO.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0501_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0501  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0501 OUTPUT.

  CLEAR: IT_EXCLUDE_0501, IT_EXCLUDE_0501[].

  PERFORM POPULA_INFO_DIFERENCA.

  IF ( WA_CTE_0501-CK_FINALIZADO IS NOT INITIAL ) OR ( CK_VT_CRIADA IS NOT INITIAL ).
    WA_EXCLUDE_0501 = OK_SALVAR.
    APPEND WA_EXCLUDE_0501 TO IT_EXCLUDE_0501.
    WA_EXCLUDE_0501 = OK_DELETE.
    APPEND WA_EXCLUDE_0501 TO IT_EXCLUDE_0501.

    "Bloquear Grupo
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'A1'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF CK_EXISTE_REGI EQ ABAP_FALSE.
    WA_EXCLUDE_0501 = OK_DELETE.
    APPEND WA_EXCLUDE_0501 TO IT_EXCLUDE_0501.
  ENDIF.

  SET PF-STATUS 'PF0501' EXCLUDING IT_EXCLUDE_0501.
  SET TITLEBAR 'TL0501'.

  "Montar Grids """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "(1)
  IF CTL_CON_0501A IS INITIAL.

    CREATE OBJECT CTL_CON_0501A
      EXPORTING
        CONTAINER_NAME = 'ALV_NOTAS_55'.

    CREATE OBJECT CTL_ALV_0501A
      EXPORTING
        I_PARENT = CTL_CON_0501A.

    PERFORM FILL_IT_FIELDCATALOG_0501A.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0501A.

    GS_LAY_0501A-SEL_MODE   = 'A'.
    GS_LAY_0501A-ZEBRA      = ABAP_TRUE.
    GS_LAY_0501A-NO_TOOLBAR = ABAP_TRUE.
    GS_LAY_0501A-GRID_TITLE = 'Rateio de NF-e (Id.Transporte)'.

    CALL METHOD CTL_ALV_0501A->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0501A
        IS_VARIANT      = GS_VAR_0501A
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0501A
        IT_OUTTAB       = IT_CTE_D55[].

  ELSE.
    CALL METHOD CTL_ALV_0501A->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0501A->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0501A
      ES_ROW_NO   = GS_SCROLL_ROW_0501A.

  "(2)
  IF CTL_CON_0501B IS INITIAL.

    CREATE OBJECT CTL_CON_0501B
      EXPORTING
        CONTAINER_NAME = 'ALV_NOTAS_01'.

    CREATE OBJECT CTL_ALV_0501B
      EXPORTING
        I_PARENT = CTL_CON_0501B.

    PERFORM FILL_IT_FIELDCATALOG_0501B.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0501B.

    GS_LAY_0501B-SEL_MODE   = 'A'.
    GS_LAY_0501B-ZEBRA      = ABAP_TRUE.
    GS_LAY_0501B-NO_TOOLBAR = ABAP_TRUE.
    GS_LAY_0501B-GRID_TITLE = 'Rateio de Nota (Id.Transporte)'.

    CALL METHOD CTL_ALV_0501B->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0501B
        IS_VARIANT      = GS_VAR_0501B
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0501B
        IT_OUTTAB       = IT_CTE_D01[].

  ELSE.
    CALL METHOD CTL_ALV_0501B->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0501B->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0501B
      ES_ROW_NO   = GS_SCROLL_ROW_0501B.

  "(3)
  IF CTL_CON_0501C IS INITIAL.

    CREATE OBJECT CTL_CON_0501C
      EXPORTING
        CONTAINER_NAME = 'ALV_VAGAO'.

    CREATE OBJECT CTL_ALV_0501C
      EXPORTING
        I_PARENT = CTL_CON_0501C.

    PERFORM FILL_IT_FIELDCATALOG_0501C.

    PERFORM FILL_GS_VARIANT_0501C.

    GS_LAY_0501C-SEL_MODE   = SPACE.
    GS_LAY_0501C-ZEBRA      = ABAP_TRUE.
    GS_LAY_0501C-NO_TOOLBAR = ABAP_TRUE.
    GS_LAY_0501C-GRID_TITLE = 'Vagões da CT-e'.

    CALL METHOD CTL_ALV_0501C->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0501C
        IS_VARIANT      = GS_VAR_0501C
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0501C
        IT_OUTTAB       = IT_CTE_VGA[].

  ELSE.
    CALL METHOD CTL_ALV_0501C->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0501C->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0501C
      ES_ROW_NO   = GS_SCROLL_ROW_0501C.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

ENDMODULE.                 " STATUS_0501  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  POPULA_INFO_DIFERENCA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POPULA_INFO_DIFERENCA .

  DATA: LC_QT_DIFERENCA	TYPE ZDE_QT_CARGA_CTE_SINAL.

  LC_QT_DIFERENCA = 0.

  IF WA_PESO_LIBERADO IS INITIAL.

    CLEAR: CK_VT_CRIADA.

    SELECT SINGLE * INTO WA_CTE_0501
      FROM ZIB_CTE_DIST_TER
     WHERE CD_CHAVE_CTE EQ WA_CTE_SELECT-CD_CHAVE_CTE.

    CK_TIPO_DIF_TELA = 'C'.

    SELECT * INTO TABLE IT_0501_N55
      FROM ZIB_CTE_DIST_N55
     WHERE CD_CHAVE_CTE EQ WA_CTE_SELECT-CD_CHAVE_CTE.

    LOOP AT IT_0501_N55.
      IF IT_0501_N55-TKNUM IS NOT INITIAL.
        CK_VT_CRIADA = ABAP_TRUE.
      ENDIF.
    ENDLOOP.

    IF CK_VT_CRIADA IS INITIAL.
      SELECT * INTO TABLE IT_0501_N01
        FROM ZIB_CTE_DIST_N01
       WHERE CD_CHAVE_CTE EQ WA_CTE_SELECT-CD_CHAVE_CTE.

      LOOP AT IT_0501_N01.
        IF IT_0501_N01-TKNUM IS NOT INITIAL.
          CK_VT_CRIADA = ABAP_TRUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SELECT * INTO TABLE IT_CTE_D55
      FROM ZIB_CTE_DIST_D55
     WHERE CD_CHAVE_CTE EQ WA_CTE_SELECT-CD_CHAVE_CTE.

    SELECT * INTO TABLE IT_CTE_D01
      FROM ZIB_CTE_DIST_D01
     WHERE CD_CHAVE_CTE EQ WA_CTE_SELECT-CD_CHAVE_CTE.

    SELECT * INTO TABLE IT_CTE_VGA
      FROM ZIB_CTE_DIST_VGA
     WHERE CD_CHAVE_CTE EQ WA_CTE_SELECT-CD_CHAVE_CTE.

    SELECT SINGLE * INTO WA_PESO_LIBERADO
      FROM ZIB_CTE_DIST_LBP
     WHERE CD_CHAVE_CTE EQ WA_CTE_SELECT-CD_CHAVE_CTE.

    IF NOT SY-SUBRC IS INITIAL.

      WA_PESO_LIBERADO-CD_CHAVE_CTE    = WA_CTE_0501-CD_CHAVE_CTE.
      WA_PESO_LIBERADO-QT_CARGA_CTE    = WA_CTE_0501-QT_CARGA_CTE.
      WA_PESO_LIBERADO-QT_CARGA_CTE_LB = WA_CTE_0501-QT_CARGA_CTE.

      "Quantidade dos Vagões
      WA_PESO_LIBERADO-QT_VAGOES = 0.
      LOOP AT IT_CTE_VGA.
        ADD IT_CTE_VGA-VALR_PESO_REAL TO WA_PESO_LIBERADO-QT_VAGOES.
      ENDLOOP.
      WA_PESO_LIBERADO-QT_VAGOES = WA_PESO_LIBERADO-QT_VAGOES * 1000.

      "Quantidade de Nota Fiscal Eletrônica
      WA_PESO_LIBERADO-QT_NFE = 0.
      LOOP AT IT_CTE_D55.
        ADD IT_CTE_D55-VALR_PESO_RATE TO WA_PESO_LIBERADO-QT_NFE.
      ENDLOOP.
      WA_PESO_LIBERADO-QT_NFE = WA_PESO_LIBERADO-QT_NFE * 1000.

      "Quantidade de Nota Fiscal Papel
      WA_PESO_LIBERADO-QT_NF = 0.
      LOOP AT IT_CTE_D01.
        ADD IT_CTE_D01-VALR_PESO_RATE TO  WA_PESO_LIBERADO-QT_NF.
      ENDLOOP.
      WA_PESO_LIBERADO-QT_NF = WA_PESO_LIBERADO-QT_NF * 1000.

      LC_QT_DIFERENCA = WA_PESO_LIBERADO-QT_NFE + WA_PESO_LIBERADO-QT_NF.

      WA_PESO_LIBERADO-QT_DIFERENCA = WA_PESO_LIBERADO-QT_CARGA_CTE - LC_QT_DIFERENCA.
      CK_EXISTE_REGI = ABAP_FALSE.
    ELSE.
      CK_EXISTE_REGI  = ABAP_TRUE.
      LC_QT_DIFERENCA = WA_PESO_LIBERADO-QT_DIFERENCA.
    ENDIF.
  ENDIF.

  IF LC_QT_DIFERENCA EQ 0.
    WA_PESO_LIBERADO-QT_CARGA_CTE_LB = WA_PESO_LIBERADO-QT_CARGA_CTE.
  ELSE.
    WA_PESO_LIBERADO-QT_CARGA_CTE_LB = WA_PESO_LIBERADO-QT_CARGA_CTE + WA_PESO_LIBERADO-QT_DIFERENCA.
  ENDIF.

ENDFORM.                    " POPULA_INFO_DIFERENCA

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0501  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0501 INPUT.
  CASE OK_CODE.
    WHEN OK_RATEIO.

      CALL SCREEN 0502 STARTING AT 40 10.

    WHEN OK_SALVAR.

      IF IT_ZIB_CTE_DIST_LBP[] IS INITIAL.

        IF IT_CTE_D55_SEL[] IS INITIAL.
          MESSAGE S022.
          RETURN.
        ENDIF.

        CHECK CK_TIPO_DIF_TELA NE 'C'.

        IF CK_TIPO_DIF_TELA EQ 'A'.
          CK_TIPO_DIF_TELA = 'O'.
          EXIT.
        ENDIF.

        IF WA_PESO_LIBERADO-QT_DIFERENCA EQ 0.
          MESSAGE S152.
        ENDIF.
        CHECK WA_PESO_LIBERADO-QT_DIFERENCA NE 0.

        WA_PESO_LIBERADO-DT_AUTORIZACAO   = SY-DATUM.
        WA_PESO_LIBERADO-DS_NAME_USUARIO  = SY-UNAME.
        WA_PESO_LIBERADO-HR_AUTORIZACAO   = SY-UZEIT.

        READ TABLE IT_CTE_D55_SEL INDEX 1.
        WA_PESO_LIBERADO-N55_CHAVE_ACESSO = IT_CTE_D55_SEL-N55_CHAVE_ACESSO.

        DELETE FROM ZIB_CTE_DIST_LBP WHERE CD_CHAVE_CTE EQ WA_CTE_SELECT-CD_CHAVE_CTE.
        MODIFY ZIB_CTE_DIST_LBP FROM WA_PESO_LIBERADO.
        COMMIT WORK.
        CLEAR: WA_PESO_LIBERADO.
        MESSAGE S059.
        LEAVE TO SCREEN 0.

      ELSE.

        DELETE IT_ZIB_CTE_DIST_LBP WHERE QT_DIFERENCA EQ 0.
        LOOP AT IT_ZIB_CTE_DIST_LBP ASSIGNING FIELD-SYMBOL(<FS_LPB>).
          <FS_LPB>-DT_AUTORIZACAO   = SY-DATUM.
          <FS_LPB>-DS_NAME_USUARIO  = SY-UNAME.
          <FS_LPB>-HR_AUTORIZACAO   = SY-UZEIT.
          <FS_LPB>-CD_CHAVE_CTE     = WA_PESO_LIBERADO-CD_CHAVE_CTE.
          <FS_LPB>-QT_CARGA_CTE     = WA_PESO_LIBERADO-QT_CARGA_CTE.
          <FS_LPB>-QT_CARGA_CTE_LB  = WA_PESO_LIBERADO-QT_CARGA_CTE_LB.
          <FS_LPB>-QT_VAGOES        = WA_PESO_LIBERADO-QT_VAGOES.
          <FS_LPB>-QT_NFE           = WA_PESO_LIBERADO-QT_NFE.
          <FS_LPB>-QT_NF            = WA_PESO_LIBERADO-QT_NF.
        ENDLOOP.
        DELETE FROM ZIB_CTE_DIST_LBP WHERE CD_CHAVE_CTE EQ WA_CTE_SELECT-CD_CHAVE_CTE.
        MODIFY ZIB_CTE_DIST_LBP FROM TABLE IT_ZIB_CTE_DIST_LBP.
        COMMIT WORK.
        CLEAR: WA_PESO_LIBERADO.
        MESSAGE S059.
        LEAVE TO SCREEN 0.

      ENDIF.

    WHEN OK_DELETE.

      IF WA_CTE_0501-CD_CHAVE_CTE IS NOT INITIAL.
        DELETE FROM ZIB_CTE_DIST_LBP WHERE CD_CHAVE_CTE EQ WA_CTE_0501-CD_CHAVE_CTE.
        COMMIT WORK.
        CLEAR: WA_PESO_LIBERADO.
        MESSAGE S151.
        LEAVE TO SCREEN 0.
      ENDIF.

  ENDCASE.
ENDMODULE. " USER_COMMAND_0501  INPUT

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_DIFERENCA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_DIFERENCA INPUT.
  CK_TIPO_DIF_TELA = 'A'.
ENDMODULE.                 " ALTEROU_DIFERENCA  INPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0501A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0501A .

  FIELD-SYMBOLS: <FS_CAT_0501A> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_D55'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0501A.

  LOOP AT IT_CATALOG_0501A ASSIGNING <FS_CAT_0501A>.
    CASE <FS_CAT_0501A>-FIELDNAME.
      WHEN 'MANDT' OR 'CD_CHAVE_CTE'.
        <FS_CAT_0501A>-NO_OUT = ABAP_TRUE.
    ENDCASE.
    CASE <FS_CAT_0501A>-DATATYPE.
      WHEN 'QUAN' OR 'CURR' OR 'DEC'.
        <FS_CAT_0501A>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0501A>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0501A

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0501A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0501A .

  GS_VAR_0501A-REPORT      = SY-REPID.
  GS_VAR_0501A-HANDLE      = '501A'.
  GS_VAR_0501A-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0501A-USERNAME    = ABAP_FALSE.
  GS_VAR_0501A-VARIANT     = ABAP_FALSE.
  GS_VAR_0501A-TEXT        = ABAP_FALSE.
  GS_VAR_0501A-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0501A

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0501b
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0501B .

  FIELD-SYMBOLS: <FS_CAT_0501B> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_D01'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0501B.

  LOOP AT IT_CATALOG_0501B ASSIGNING <FS_CAT_0501B>.
    CASE <FS_CAT_0501B>-FIELDNAME.
      WHEN 'MANDT' OR 'CD_CHAVE_CTE'.
        <FS_CAT_0501B>-NO_OUT = ABAP_TRUE.
    ENDCASE.
    CASE <FS_CAT_0501B>-DATATYPE.
      WHEN 'QUAN' OR 'CURR' OR 'DEC'.
        <FS_CAT_0501B>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0501B>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0501b

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0501b
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0501B .

  GS_VAR_0501B-REPORT      = SY-REPID.
  GS_VAR_0501B-HANDLE      = '501B'.
  GS_VAR_0501B-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0501B-USERNAME    = ABAP_FALSE.
  GS_VAR_0501B-VARIANT     = ABAP_FALSE.
  GS_VAR_0501B-TEXT        = ABAP_FALSE.
  GS_VAR_0501B-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0501A

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0501C
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0501C .

  FIELD-SYMBOLS: <FS_CAT_0501C> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_VGA'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0501C.

  LOOP AT IT_CATALOG_0501C ASSIGNING <FS_CAT_0501C>.
    CASE <FS_CAT_0501C>-FIELDNAME.
      WHEN 'MANDT' OR 'CD_CHAVE_CTE'.
        <FS_CAT_0501C>-NO_OUT = ABAP_TRUE.
    ENDCASE.
    CASE <FS_CAT_0501C>-DATATYPE.
      WHEN 'QUAN' OR 'CURR' OR 'DEC'.
        <FS_CAT_0501C>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0501C>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0501C

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0501C
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0501C.

  GS_VAR_0501C-REPORT      = SY-REPID.
  GS_VAR_0501C-HANDLE      = '501C'.
  GS_VAR_0501C-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0501C-USERNAME    = ABAP_FALSE.
  GS_VAR_0501C-VARIANT     = ABAP_FALSE.
  GS_VAR_0501C-TEXT        = ABAP_FALSE.
  GS_VAR_0501C-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0501C

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_0501  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_0501 INPUT.

  CALL METHOD CTL_ALV_0501A->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0501A
      ES_ROW_NO   = GS_SCROLL_ROW_0501A.

  CALL METHOD CTL_ALV_0501B->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0501B
      ES_ROW_NO   = GS_SCROLL_ROW_0501B.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_0501  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_0501 INPUT.

  CLEAR: IT_CTE_D55_SEL[], IT_CTE_D01_SEL[].

  CLEAR IT_SELECTED_ROWS.

  CALL METHOD CTL_ALV_0501A->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  CLEAR IT_CTE_SELECT[].

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_CTE_D55 INTO IT_CTE_D55_SEL INDEX WA_SELECTED_ROWS-INDEX.
    APPEND IT_CTE_D55_SEL.
  ENDLOOP.

  CLEAR IT_SELECTED_ROWS.

  CALL METHOD CTL_ALV_0501B->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  CLEAR IT_CTE_SELECT[].

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_CTE_D01 INTO IT_CTE_D01_SEL INDEX WA_SELECTED_ROWS-INDEX.
    APPEND IT_CTE_D01_SEL.
  ENDLOOP.

ENDMODULE.
