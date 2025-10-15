*&---------------------------------------------------------------------*
*&  Include           ZAA10_0100
*&---------------------------------------------------------------------*

CONSTANTS: OK_EDITAR_DATA TYPE SY-UCOMM VALUE 'EDITAR',
           OK_SALVAR      TYPE SY-UCOMM VALUE 'SAVE'.

DATA: G_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DG_SPLITTER_1       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_1         TYPE REF TO CL_GUI_CONTAINER,
      DG_SPLITTER_2       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      DG_PARENT_2         TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_2A        TYPE REF TO CL_GUI_CONTAINER,
      DG_PARENT_ALV       TYPE REF TO CL_GUI_CONTAINER,
      PICTURE             TYPE REF TO CL_GUI_PICTURE,
      GS_LAYOUT           TYPE LVC_S_LAYO,
      GS_VARIANT          TYPE DISVARIANT,
      IT_FIELDCATALOG     TYPE LVC_T_FCAT,
      WA_FIELDCATALOG     TYPE LVC_S_FCAT,
      CTL_ALV             TYPE REF TO CL_GUI_ALV_GRID,
      GS_SCROLL_COL       TYPE LVC_S_COL,
      GS_SCROLL_ROW       TYPE LVC_S_ROID,
      IT_EXCLUDE_FCODE    TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE    LIKE LINE OF IT_EXCLUDE_FCODE,
      DG_DYNDOC_ID        TYPE REF TO CL_DD_DOCUMENT,
      TABLE_ELEMENT       TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN              TYPE REF TO CL_DD_AREA,
      TABLE_ELEMENT2      TYPE REF TO CL_DD_TABLE_ELEMENT,
      COLUMN_1            TYPE REF TO CL_DD_AREA,
      COLUMN_2            TYPE REF TO CL_DD_AREA,
      DG_HTML_CNTRL       TYPE REF TO CL_GUI_HTML_VIEWER,
      IT_SELECT           TYPE STANDARD TABLE OF ZAA004,
      WA_SELECT           TYPE ZAA004,
      CK_GRAVOU           TYPE C LENGTH 1,
      CK_PRIMEIRA_ENTRADA TYPE C LENGTH 1.

DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
      WA_SELECTED_ROWS TYPE LVC_S_ROW.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CONTROLS:  MAIN_TAB TYPE TABSTRIP.

  CTL_ALV->CHECK_CHANGED_DATA( ).

  CASE SY-UCOMM.
    WHEN OK_EDITAR_DATA.
      PERFORM EDITAR_DATA.
      "CLEAR: OK_CODE.
    WHEN OK_SALVAR.
      CASE I_MAIN_TAB-PRESSED_TAB.
        WHEN C_MAIN_TAB-TAB1.
          PERFORM SALVAR.
        WHEN C_MAIN_TAB-TAB2.
          PERFORM SALVA_BAIXA.
      ENDCASE.

  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  "  CLEAR OK_CODE.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA: URL(255) TYPE C,
        P_TEXT   TYPE SDYDO_TEXT_ELEMENT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  PERFORM SELECIONAR_REGISTROS.

  MAIN_TAB-ACTIVETAB = I_MAIN_TAB-PRESSED_TAB.

  CASE I_MAIN_TAB-PRESSED_TAB.
    WHEN C_MAIN_TAB-TAB1.
      PERFORM CREATE_OBJECT USING 'CONTAINER'.            "Creating Object
      PERFORM FILL_IT_FIELDCATALOG.                       "Building the field catalog
      I_MAIN_TAB-SUBSCREEN = '0101'.
      PERFORM DISPLAY_OUTPUT USING IT_ZAA004.
    WHEN C_MAIN_TAB-TAB2.
      PERFORM CREATE_OBJECT USING 'CONTAINER_2'.          "Creating Object
      PERFORM FILL_IT_FIELDCATALOG.                       "Building the field catalog
      I_MAIN_TAB-SUBSCREEN = '0101'.
      PERFORM DISPLAY_OUTPUT USING IT_ZAA004_BAIXA.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG .
  DATA: LC_COL_POS  TYPE LVC_COLPOS.
  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CLEAR: IT_FIELDCATALOG[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZAA004'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

  LOOP AT IT_FIELDCATALOG ASSIGNING <FS_CAT>.
    CASE <FS_CAT>-FIELDNAME .
      WHEN 'BUKRS'.
        <FS_CAT>-CHECKTABLE = 'T001'.
        <FS_CAT>-COLTEXT    = 'Empresa'.
        "<FS_CAT>-F4AVAILABL = 'C_T001'.
        <FS_CAT>-OUTPUTLEN  = 07.
      WHEN 'USUAR_AA'.
        <FS_CAT>-CHECKTABLE = 'USR01'.
        <FS_CAT>-COLTEXT    = 'Usuário'.
        <FS_CAT>-F4AVAILABL = 'USER_ADDR'.
        <FS_CAT>-OUTPUTLEN  = 12.
      WHEN 'GSBER'.
        <FS_CAT>-CHECKTABLE = 'TGSB'.
        <FS_CAT>-COLTEXT    = 'Filial'.
        <FS_CAT>-F4AVAILABL = 'H_TGSB'.
        <FS_CAT>-OUTPUTLEN  = 6.
      WHEN 'KOSTL'.
        <FS_CAT>-COLTEXT    = 'C. Custo'.
        <FS_CAT>-F4AVAILABL = 'KOSTN'.
        <FS_CAT>-OUTPUTLEN  = 12.
      WHEN 'NIVEL_AA'.
        <FS_CAT>-COLTEXT    = 'Nível'.
        <FS_CAT>-OUTPUTLEN  = 6.
      WHEN 'GJAHR'.
        IF ( I_MAIN_TAB-PRESSED_TAB =  C_MAIN_TAB-TAB1 ).
          <FS_CAT>-CHECKTABLE = 'BKPF'.
          <FS_CAT>-COLTEXT    = 'Ano Inv.'.
          <FS_CAT>-OUTPUTLEN  = 8.
        ELSE.
          <FS_CAT>-NO_OUT  = 'X'.
        ENDIF.
      WHEN 'DT_INI'.
        <FS_CAT>-COLTEXT    = 'Data Início'.
      WHEN 'DT_FIM'.
        <FS_CAT>-COLTEXT    = 'Data Fim'.
      WHEN 'FLUXO_BAIXA'.
        <FS_CAT>-NO_OUT  = 'X'.
    ENDCASE.
  ENDLOOP.

  PERFORM FILL_GS_VARIANT.

ENDFORM.                    " FILL_IT_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM FILL_GS_VARIANT .
  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = '0100'.
  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT-USERNAME    = ABAP_FALSE.
  GS_VARIANT-VARIANT     = ABAP_FALSE.
  GS_VARIANT-TEXT        = ABAP_FALSE.
  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.
ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
MODULE GET_SCROLL_INFO INPUT.

  CALL METHOD CTL_ALV->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL
      ES_ROW_NO   = GS_SCROLL_ROW.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS INPUT.

  CLEAR IT_SELECTED_ROWS.

  CALL METHOD CTL_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  CLEAR IT_SELECT.

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_ZAA004 INTO WA_ZAA004 INDEX WA_SELECTED_ROWS-INDEX.
    IF SY-SUBRC IS INITIAL.
      APPEND WA_ZAA004 TO IT_SELECT.
    ENDIF.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_DATA
*&---------------------------------------------------------------------*
FORM EDITAR_DATA .

  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.

  GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
  GS_ALV_REFRES_COND-COL = ABAP_TRUE.

  IF IT_SELECT IS INITIAL.
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0100.
    RETURN.
  ENDIF.

  CLEAR: CK_GRAVOU.

  "CK_PRIMEIRA_ENTRADA = ABAP_TRUE.
  CALL SCREEN 0002 STARTING AT 05 05.

  CLEAR: IT_SELECT.

  IF CK_GRAVOU = ABAP_TRUE.

    CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = GS_ALV_REFRES_COND
        I_SOFT_REFRESH = ABAP_TRUE.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SALVAR
*&---------------------------------------------------------------------*
FORM SALVAR .

  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.
  DATA: IT_ZAA004_AUX TYPE STANDARD TABLE OF ZAA004,
        WA_ZAA004_AUX TYPE ZAA004.

  DATA: VL_CONT TYPE I.

  GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
  GS_ALV_REFRES_COND-COL = ABAP_TRUE.

  IT_ZAA004_AUX = IT_ZAA004.

  SORT IT_ZAA004_AUX BY GSBER KOSTL GJAHR NIVEL_AA.

  "Consistências
  LOOP AT IT_ZAA004 INTO WA_ZAA004.
    "Não pode linhas com campos em branco
    IF WA_ZAA004-BUKRS    IS INITIAL OR
       WA_ZAA004-GSBER    IS INITIAL OR
       WA_ZAA004-KOSTL    IS INITIAL OR
       WA_ZAA004-GJAHR    IS INITIAL OR
       WA_ZAA004-NIVEL_AA IS INITIAL OR
       WA_ZAA004-USUAR_AA IS INITIAL.
      MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE TO SCREEN 0100.
    ENDIF.
    "Não pode 'estratégias' de aprovação pulando níveis
    VL_CONT = 0.
    LOOP AT IT_ZAA004_AUX INTO WA_ZAA004_AUX
      WHERE BUKRS EQ WA_ZAA004-BUKRS
        AND GSBER EQ WA_ZAA004-GSBER
        AND KOSTL EQ WA_ZAA004-KOSTL
        AND GJAHR EQ WA_ZAA004-GJAHR.

      "Não pode 'estratégias' de aprovação pulando níveis
      VL_CONT = VL_CONT + 1.
      IF WA_ZAA004_AUX-NIVEL_AA NE VL_CONT.
        MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0100.
      ENDIF.
      "Não pode 'estratégias' de aprovação com niveis acima de 5
      IF VL_CONT > 4.
        MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0100.
      ENDIF.
    ENDLOOP.
    "Não pode 'estratégias' de aprovação sem níveis de 1 a 4
    IF VL_CONT NE 4.
      MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE TO SCREEN 0100.
    ENDIF.
  ENDLOOP.

  SELECT *
    FROM ZAA004
    INTO TABLE IT_ZAA004_BASE
    WHERE FLUXO_BAIXA EQ ''.

  LOOP AT IT_ZAA004_BASE INTO WA_ZAA004.

    READ TABLE IT_ZAA004 WITH KEY BUKRS    = WA_ZAA004-BUKRS
                                  GSBER    = WA_ZAA004-GSBER
                                  KOSTL    = WA_ZAA004-KOSTL
                                  GJAHR    = WA_ZAA004-GJAHR
                                  NIVEL_AA = WA_ZAA004-NIVEL_AA
                                  USUAR_AA = WA_ZAA004-USUAR_AA
                                  DT_INI   = WA_ZAA004-DT_INI
                                  DT_FIM   = WA_ZAA004-DT_FIM TRANSPORTING NO FIELDS.

    IF SY-SUBRC IS NOT INITIAL.
      DELETE ZAA004 FROM WA_ZAA004.
    ENDIF.
  ENDLOOP.

  SELECT *
    FROM ZAA004
    INTO TABLE IT_ZAA004_BASE
    WHERE FLUXO_BAIXA EQ ''.

  LOOP AT IT_ZAA004 INTO WA_ZAA004.

    READ TABLE IT_ZAA004_BASE WITH KEY BUKRS    = WA_ZAA004-BUKRS
                                       GSBER    = WA_ZAA004-GSBER
                                       KOSTL    = WA_ZAA004-KOSTL
                                       GJAHR    = WA_ZAA004-GJAHR
                                       NIVEL_AA = WA_ZAA004-NIVEL_AA
                                       USUAR_AA = WA_ZAA004-USUAR_AA
                                       DT_INI   = WA_ZAA004-DT_INI
                                       DT_FIM   = WA_ZAA004-DT_FIM TRANSPORTING NO FIELDS.

    IF SY-SUBRC IS NOT INITIAL.
      INSERT ZAA004 FROM WA_ZAA004.
    ENDIF.
  ENDLOOP.

  COMMIT WORK.

  PERFORM: LIMPAR_TABELAS,
           SELECIONAR_REGISTROS.

  CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_ALV_REFRES_COND
      I_SOFT_REFRESH = ABAP_TRUE.

  MESSAGE TEXT-004 TYPE 'S'.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
FORM CREATE_OBJECT USING CUSTOM.

  IF ( G_CUSTOM_CONTAINER IS INITIAL ).
    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = CUSTOM.
  ELSE.
    CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY.
  ENDIF.

  IF ( CTL_ALV IS INITIAL ).
    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER.
  ENDIF.



ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
FORM DISPLAY_OUTPUT USING ITAB.

  GS_LAYOUT-SEL_MODE = 'A'.
  GS_LAYOUT-EDIT     = ABAP_TRUE.

  CLEAR: IT_EXCLUDE_FCODE, IT_EXCLUDE_FCODE[].

  SET HANDLER:
    LCL_EVENTHANDLER=>HANDLE_DATA_CHANGED          FOR CTL_ALV,
    LCL_EVENTHANDLER=>HANDLE_DATA_CHANGED_FINISHED FOR CTL_ALV.

  CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_VARIANT
      IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
      I_SAVE               = 'A'
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCATALOG
      IT_OUTTAB            = ITAB.

  CALL METHOD CTL_ALV->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

  CALL METHOD CTL_ALV->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED
    EXCEPTIONS
      ERROR      = 1
      OTHERS     = 2.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  MAIN_TAB_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
MODULE MAIN_TAB_ACTIVE_TAB_GET INPUT.

  CASE SY-UCOMM.
    WHEN C_MAIN_TAB-TAB1.
      I_MAIN_TAB-PRESSED_TAB = C_MAIN_TAB-TAB1.
      I_MAIN_TAB-SUBSCREEN = '0101'.
    WHEN C_MAIN_TAB-TAB2.
      I_MAIN_TAB-PRESSED_TAB = C_MAIN_TAB-TAB2.
      I_MAIN_TAB-SUBSCREEN = '0102'.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  SALVA_BAIXA
*&---------------------------------------------------------------------*
FORM SALVA_BAIXA .


  DATA: GS_ALV_REFRES_COND  TYPE LVC_S_STBL.
  DATA: IT_ZAA004_TEMP      TYPE STANDARD TABLE OF ZAA004.
  DATA: VL_CONT TYPE I.

  GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
  GS_ALV_REFRES_COND-COL = ABAP_TRUE.

  IT_ZAA004_TEMP = IT_ZAA004_BAIXA.
  SORT IT_ZAA004_TEMP BY BUKRS GSBER KOSTL NIVEL_AA ASCENDING.

*&-----------Consistências
  LOOP AT IT_ZAA004_BAIXA INTO DATA(WL_ZAA004_BAIXA).

    IF  ( WL_ZAA004_BAIXA-BUKRS IS INITIAL ) OR
        ( WL_ZAA004_BAIXA-GSBER IS INITIAL ) OR
        ( WL_ZAA004_BAIXA-KOSTL IS INITIAL ) OR
        ( WL_ZAA004_BAIXA-NIVEL_AA IS INITIAL ) OR
        ( WL_ZAA004_BAIXA-USUAR_AA IS INITIAL ).

      MESSAGE 'Preencher todos os campos obrigatórios!' TYPE 'I'.

    ENDIF.

    VL_CONT = 0.

    LOOP AT IT_ZAA004_TEMP INTO DATA(WL_ZAA004_TEMP) WHERE BUKRS EQ WL_ZAA004_BAIXA-BUKRS
                                                       AND GSBER EQ WL_ZAA004_BAIXA-GSBER
                                                       AND KOSTL EQ WL_ZAA004_BAIXA-KOSTL.

      VL_CONT = VL_CONT + 1.
      IF ( WL_ZAA004_TEMP-NIVEL_AA NE VL_CONT ).
        MESSAGE 'Nível Inconsistente.' TYPE 'I'.
        LEAVE TO SCREEN 0100.
      ENDIF.
*      IF ( WL_ZAA004_TEMP-NIVEL_AA > 4 ).
*        MESSAGE 'Nível de aprovação maior que 4.' TYPE 'I'.
*        LEAVE TO SCREEN 0100.
*      ENDIF.

    ENDLOOP.

*    IF ( VL_CONT NE 4 ).
*      MESSAGE 'Estratégia de aprovação deve possuir níveis de 1 à 4' TYPE 'I'.
*      LEAVE TO SCREEN 0100.
*    ENDIF.

  ENDLOOP.

  SELECT * FROM ZAA004
    INTO TABLE @DATA(TL_ZAA004)
    WHERE FLUXO_BAIXA EQ 'X'.

  LOOP AT TL_ZAA004 INTO WL_ZAA004_TEMP.

    READ TABLE IT_ZAA004_BAIXA WITH KEY BUKRS    = WL_ZAA004_TEMP-BUKRS
                                        GSBER    = WL_ZAA004_TEMP-GSBER
                                        KOSTL    = WL_ZAA004_TEMP-KOSTL
                                        NIVEL_AA = WL_ZAA004_TEMP-NIVEL_AA
                                        USUAR_AA = WL_ZAA004_TEMP-USUAR_AA
                                        DT_INI   = WL_ZAA004_TEMP-DT_INI
                                        DT_FIM   = WL_ZAA004_TEMP-DT_FIM TRANSPORTING NO FIELDS.

    IF SY-SUBRC IS NOT INITIAL.
      DELETE ZAA004 FROM WL_ZAA004_TEMP.
    ENDIF.


  ENDLOOP.

    CLEAR: TL_ZAA004[], WL_ZAA004_TEMP.

    SELECT *
    FROM ZAA004
    INTO TABLE TL_ZAA004
    WHERE FLUXO_BAIXA EQ 'X'.

  LOOP AT IT_ZAA004_BAIXA INTO WL_ZAA004_BAIXA.

    READ TABLE TL_ZAA004 WITH KEY BUKRS    = WL_ZAA004_BAIXA-BUKRS
                                  GSBER    = WL_ZAA004_BAIXA-GSBER
                                  KOSTL    = WL_ZAA004_BAIXA-KOSTL
                                  NIVEL_AA = WL_ZAA004_BAIXA-NIVEL_AA
                                  USUAR_AA = WL_ZAA004_BAIXA-USUAR_AA
                                  DT_INI   = WL_ZAA004_BAIXA-DT_INI
                                  DT_FIM   = WL_ZAA004_BAIXA-DT_FIM TRANSPORTING NO FIELDS.

    IF SY-SUBRC IS NOT INITIAL.
      WL_ZAA004_BAIXA-FLUXO_BAIXA = 'X'.
      WL_ZAA004_BAIXA-GJAHR = SY-DATUM+0(4).
      INSERT ZAA004 FROM WL_ZAA004_BAIXA.
      COMMIT WORK.
    ENDIF.

    CLEAR: WL_ZAA004_BAIXA.
  ENDLOOP.

  PERFORM: LIMPAR_TABELAS,
           SELECIONAR_REGISTROS.

  CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_ALV_REFRES_COND
      I_SOFT_REFRESH = ABAP_TRUE.

  MESSAGE TEXT-004 TYPE 'S'.
ENDFORM.
