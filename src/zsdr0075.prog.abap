************************************************************************
** Report  ZSDR0075
**
************************************************************************
**
**
************************************************************************
REPORT ZSDR0075.
************************************************************************
* TABLES
************************************************************************
TABLES: ZSDT0051.
************************************************************************
* TYPES
************************************************************************
TYPES: BEGIN OF TY_SAIDA,
         ICONE            TYPE CHAR6,
         NRO_SOL_OV       TYPE ZSDT0051-NRO_SOL_OV,
         VKORG            TYPE ZSDT0051-VKORG,
         AUART            TYPE ZSDT0051-AUART,
         KUNNR            TYPE ZSDT0051-KUNNR,
         NAME1            TYPE KNA1-NAME1,
         DTDE_LOGIST      TYPE ZSDT0051-DTDE_LOGIST,
         DTATE_LOGIST     TYPE ZSDT0051-DTATE_LOGIST,
         WAERK            TYPE ZSDT0051-WAERK,
         "
         VAL_TOT          TYPE ZSDT0053-VLRTOT,
         VAL_TOT_APLICADO TYPE ZSDT0053-VLRTOT,
         VAL_TOT_AAPLICAR TYPE ZSDT0053-VLRTOT,
         KURSF            TYPE ZSDT0054-KURSF,
         """""""""""""""
         VBELN            TYPE ZSDT0053-VBELN,
         WERKS            TYPE ZSDT0053-WERKS,
         MATNR            TYPE ZSDT0053-MATNR,
         MAKTX            TYPE MAKT-MAKTX,
         ZMENG            TYPE ZSDT0053-ZMENG,
         ZIEME            TYPE ZSDT0053-ZIEME,
         TOT_OV           TYPE ZSDT0053-VLRTOT,
         TOT_VBFA         TYPE VBFA-RFMNG,
         DMBTR            TYPE ZSDT0053-DMBTR,
         PMEIN            TYPE ZSDT0053-PMEIN,
         NAVIO            TYPE ZSDT0053-NAVIO,
         PORTO            TYPE ZSDT0053-PORTO.
TYPES: END OF TY_SAIDA.

TYPES: BEGIN OF TY_EXCEL,
         NRO_SOL_OV TYPE ZSDT0051-NRO_SOL_OV,
         WERKS      TYPE ZSDT0053-WERKS,
         WAERK      TYPE ZSDT0051-WAERK,
         KURSF      TYPE ZSDT0054-KURSF,
         C_KURSF    TYPE CHAR20,
         NAME1      TYPE KNA1-NAME1,
         STCDX      TYPE CHAR18,
         MAKTX      TYPE MAKT-MAKTX,
         ZMENG      TYPE ZSDT0053-ZMENG,
         ZIEME      TYPE ZSDT0053-ZIEME,
         VLT_PORTO  TYPE ZSDT0053-VLT_PORTO,
         P_PORTO    TYPE ZSDT0053-P_PORTO,
         C_P_PORTO  TYPE CHAR30,
         PMEIN      TYPE ZSDT0053-PMEIN,
         NAVIO      TYPE ZSDT0053-NAVIO,
         PORTO      TYPE ZSDT0053-PORTO.
TYPES: END OF TY_EXCEL.

************************************************************************
* INTERNAL TABLES
************************************************************************
DATA: IT_ZSDT0051         TYPE STANDARD TABLE OF ZSDT0051,
      IT_KNA1             TYPE STANDARD TABLE OF KNA1,
      IT_ZSDT0053         TYPE STANDARD TABLE OF ZSDT0053,
      IT_ZSDT0054         TYPE STANDARD TABLE OF ZSDT0054,
      IT_ZSDT0054_COMPENS TYPE STANDARD TABLE OF ZSDT0054,
      IT_MAKT             TYPE STANDARD TABLE OF MAKT,
      IT_VBFA             TYPE STANDARD TABLE OF VBFA,
      IT_SAIDA            TYPE STANDARD TABLE OF TY_SAIDA,
      IT_SAIDA_EMAIL      TYPE STANDARD TABLE OF TY_SAIDA,
      IT_ANEXO_1          TYPE STANDARD TABLE OF TY_EXCEL,
      IT_ANEXO_2          TYPE STANDARD TABLE OF TY_EXCEL,
      IT_BDCDATA          TYPE STANDARD TABLE OF BDCDATA.

DATA SEND_REQUEST   TYPE REF TO CL_BCS.
DATA DOCUMENT       TYPE REF TO CL_DOCUMENT_BCS.
DATA RECIPIENT      TYPE REF TO IF_RECIPIENT_BCS.
DATA BCS_EXCEPTION  TYPE REF TO CX_BCS.

DATA MAIN_TEXT      TYPE BCSY_TEXT.
DATA BINARY_CONTENT TYPE SOLIX_TAB.
DATA SIZE           TYPE SO_OBJ_LEN.
DATA SENT_TO_ALL    TYPE OS_BOOLEAN.

************************************************************************
* ALV
************************************************************************
DATA:  G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       CTL_ALV1           TYPE REF TO CL_GUI_ALV_GRID,
       GS_LAYOUT          TYPE LVC_S_LAYO,
       IT_FIELDCATALOG    TYPE LVC_T_FCAT.

************************************************************************
* SELECTION SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: S_VKORG FOR ZSDT0051-VKORG OBLIGATORY,
                S_KUNNR FOR ZSDT0051-KUNNR,
                S_NRSOL FOR ZSDT0051-NRO_SOL_OV,
                S_TPVEN FOR ZSDT0051-TP_VENDA,
                S_DTVEN FOR ZSDT0051-DATA_VENDA.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

PARAMETERS: P_AAPLI RADIOBUTTON GROUP G1 DEFAULT 'X',
            P_APLIC RADIOBUTTON GROUP G1.

SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      HANDLE_HOTSPOT_CLICK  FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID,

      TOOLBAR FOR EVENT TOOLBAR OF  CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD HANDLE_HOTSPOT_CLICK.

    DATA: WA_SAIDA TYPE TY_SAIDA,
          VL_ERRO  TYPE CHAR1.

    IF E_COLUMN_ID-FIELDNAME EQ 'NRO_SOL_OV'.

      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX E_ROW_ID-INDEX.
      IF SY-SUBRC IS INITIAL.

        CLEAR: IT_BDCDATA.

        IF P_AAPLI IS NOT INITIAL.

          PERFORM BDC_DATA USING:
                        ''            ''      'T'   'ZSDT0062'                '',
                        'ZSDR0022'    '0050'  'X'   ''                        '',
                        ''            ''      ''    'BDC_OKCODE'              '=ATUAL',
                        ''            ''      ''    'BDC_CURSOR'              'WG_HEADER-NRO_SOL_OV',
                        ''            ''      ''    'WG_HEADER-NRO_SOL_OV'    WA_SAIDA-NRO_SOL_OV,
                        ''            ''      ''    'BDC_SUBSCR'              'ZSDR0022                                0052SUB01',
                        ''            ''      ''    'BDC_SUBSCR'              'ZSDR0022                                0101TAB_STRIP_SCA',
                        'ZSDR0022'    '0050'  'X'   ''                        '',
                        ''            ''      ''    'BDC_OKCODE'              '=MODIF',
                        ''            ''      ''    'BDC_CURSOR'              'WG_HEADER-NRO_SOL_OV',
                        ''            ''      ''    'WG_HEADER-NRO_SOL_OV'    WA_SAIDA-NRO_SOL_OV,
                        ''            ''      ''    'BDC_SUBSCR'              'ZSDR0022                                0052SUB01',
                        ''            ''      ''    'BDC_SUBSCR'              'ZSDR0022                                0101TAB_STRIP_SCA',
                        'ZSDR0022'    '0050'  'X'   ''                        '',
                        ''            ''      ''    'BDC_OKCODE'              '=TAB_STRIP_FC2'.

        ELSEIF P_APLIC IS NOT INITIAL.

          PERFORM BDC_DATA USING:
                        ''            ''      'T'   'ZSDT0062'                '',
                        'ZSDR0022'    '0050'  'X'   ''                        '',
                        ''            ''      ''    'BDC_OKCODE'              '=ATUAL',
                        ''            ''      ''    'BDC_CURSOR'              'WG_HEADER-NRO_SOL_OV',
                        ''            ''      ''    'WG_HEADER-NRO_SOL_OV'    WA_SAIDA-NRO_SOL_OV,
                        ''            ''      ''    'BDC_SUBSCR'              'ZSDR0022                                0052SUB01',
                        ''            ''      ''    'BDC_SUBSCR'              'ZSDR0022                                0101TAB_STRIP_SCA',
                        'ZSDR0022'    '0050'  'X'   ''                        '',
                        ''            ''      ''    'BDC_OKCODE'              '=TAB_STRIP_FC2'.

        ENDIF.

        PERFORM CALL_TRANSACTION USING 'ZSDT0062' CHANGING VL_ERRO.

*        IF NOT VL_ERRO IS INITIAL.
*          MESSAGE 'Ação não executada' TYPE 'S' DISPLAY LIKE 'E'.
*        ELSE.

      ENDIF.

    ELSEIF E_COLUMN_ID-FIELDNAME EQ 'VBELN'.

      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX E_ROW_ID-INDEX.
      IF SY-SUBRC IS INITIAL.
        SET PARAMETER ID 'AUN' FIELD WA_SAIDA-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD TOOLBAR.

    DATA WA_TOOL TYPE STB_BUTTON.

    MOVE 3 TO WA_TOOL-BUTN_TYPE.
    APPEND WA_TOOL TO E_OBJECT->MT_TOOLBAR.
    CLEAR WA_TOOL.

    WA_TOOL-FUNCTION = 'EMAIL'.
    WA_TOOL-ICON     = '@1S@'.
    WA_TOOL-QUICKINFO = 'Informativo RE'.
    APPEND WA_TOOL TO E_OBJECT->MT_TOOLBAR.
    CLEAR WA_TOOL.

  ENDMETHOD.

  METHOD USER_COMMAND.

    IF E_UCOMM = 'EMAIL'.

      DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
            WA_SELECTED_ROWS TYPE LVC_S_ROW,
            WA_SAIDA         TYPE TY_SAIDA,
            VL_LINES         TYPE I.


      CLEAR: IT_SAIDA_EMAIL, VL_LINES.

      CALL METHOD CTL_ALV1->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SELECTED_ROWS.

      DESCRIBE TABLE IT_SELECTED_ROWS LINES VL_LINES.

      IF VL_LINES IS INITIAL.
        MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.

        LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WA_SELECTED_ROWS-INDEX.
          IF SY-SUBRC IS INITIAL.
            APPEND WA_SAIDA TO IT_SAIDA_EMAIL.
          ENDIF.
        ENDLOOP.

        PERFORM PREPARA_EMAIL.

      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.

  PERFORM SELECIONA_DADOS.
  PERFORM MONTA_SAIDA.

  IF IT_SAIDA IS NOT INITIAL.
    CALL SCREEN 5000.
  ELSE.
    MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  SELECT *
    FROM ZSDT0051
    INTO TABLE IT_ZSDT0051
   WHERE VKORG      IN S_VKORG
     AND KUNNR      IN S_KUNNR
     AND NRO_SOL_OV IN S_NRSOL
     AND TP_VENDA   IN S_TPVEN
     AND DATA_VENDA IN S_DTVEN.

  CHECK IT_ZSDT0051 IS NOT INITIAL.

  SELECT *
    FROM KNA1
    INTO TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_ZSDT0051
    WHERE KUNNR EQ IT_ZSDT0051-KUNNR.

  SELECT *
    FROM ZSDT0053
    INTO TABLE IT_ZSDT0053
    FOR ALL ENTRIES IN IT_ZSDT0051
    WHERE NRO_SOL_OV EQ IT_ZSDT0051-NRO_SOL_OV
      AND STATUS NE 'C'.

  SELECT *
    FROM ZSDT0054
    INTO TABLE IT_ZSDT0054
    FOR ALL ENTRIES IN IT_ZSDT0051
    WHERE NRO_SOL_OV EQ IT_ZSDT0051-NRO_SOL_OV
      AND ADIANT NE SPACE.

  SELECT *
     FROM ZSDT0054
     INTO TABLE IT_ZSDT0054_COMPENS
     FOR ALL ENTRIES IN IT_ZSDT0051
     WHERE NRO_SOL_OV EQ IT_ZSDT0051-NRO_SOL_OV
       AND ADIANT NE SPACE
       AND EXISTS ( SELECT *
                      FROM BSAD
                     WHERE BUKRS EQ IT_ZSDT0051-VKORG
                       AND BELNR EQ ZSDT0054~ADIANT
                       AND KUNNR EQ IT_ZSDT0051-KUNNR
                       AND AUGBL NE SPACE ).

  IF P_APLIC EQ ABAP_TRUE.

    IF IT_ZSDT0053 IS NOT INITIAL.

      SELECT *
          FROM MAKT
          INTO TABLE IT_MAKT
          FOR ALL ENTRIES IN IT_ZSDT0053
          WHERE MATNR EQ IT_ZSDT0053-MATNR
            AND SPRAS EQ SY-LANGU.

      SELECT *
        FROM VBFA
        INTO TABLE IT_VBFA
        FOR ALL ENTRIES IN IT_ZSDT0053
        WHERE VBELV EQ IT_ZSDT0053-VBELN
          AND VBTYP_N EQ 'J'
          AND VBTYP_V EQ 'C'.

    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTA_SAIDA
*&---------------------------------------------------------------------*
FORM MONTA_SAIDA .

  DATA: WA_SAIDA        TYPE TY_SAIDA,
        WA_ZSDT0051     TYPE ZSDT0051,
        WA_ZSDT0054     TYPE ZSDT0054,
        WA_ZSDT0053     TYPE ZSDT0053,
        WA_KNA1         TYPE KNA1,
        WA_MAKT         TYPE MAKT,
        WA_VBFA         TYPE VBFA,
        VL_TOT_APLICADO TYPE ZSDT0053-VLRTOT,
        VL_TOTAL_VBFA   TYPE VBFA-RFMNG,
        VL_TOT          TYPE VBFA-RFMNG,
        VL_SEM_ORDEM    TYPE CHAR1.

  CLEAR: IT_SAIDA.

  IF P_AAPLI EQ ABAP_TRUE.

    LOOP AT IT_ZSDT0051 INTO WA_ZSDT0051.

      WA_SAIDA-NRO_SOL_OV       = WA_ZSDT0051-NRO_SOL_OV.
      WA_SAIDA-VKORG            = WA_ZSDT0051-VKORG.
      WA_SAIDA-AUART            = WA_ZSDT0051-AUART.
      WA_SAIDA-KUNNR            = WA_ZSDT0051-KUNNR.
      WA_SAIDA-DTDE_LOGIST      = WA_ZSDT0051-DTDE_LOGIST.
      WA_SAIDA-DTATE_LOGIST     = WA_ZSDT0051-DTATE_LOGIST.
      WA_SAIDA-WAERK            = WA_ZSDT0051-WAERK.

      READ TABLE IT_ZSDT0054 INTO WA_ZSDT0054 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV.
      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-KURSF          = WA_ZSDT0054-KURSF.
      ENDIF.

      READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_ZSDT0051-KUNNR.
      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-NAME1          = WA_KNA1-NAME1.
      ENDIF.

      LOOP AT IT_ZSDT0053 INTO WA_ZSDT0053 WHERE NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV.
        VL_TOT_APLICADO         = VL_TOT_APLICADO + WA_ZSDT0053-VLRTOT.
        IF WA_ZSDT0053-VBELN IS INITIAL.
          VL_SEM_ORDEM = ABAP_TRUE.
        ENDIF.
      ENDLOOP.

      IF VL_SEM_ORDEM IS NOT INITIAL.
        WA_SAIDA-ICONE = '@5D@'.
      ELSE.
        WA_SAIDA-ICONE = '@5B@'.
      ENDIF.

      READ TABLE IT_ZSDT0053 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        WA_SAIDA-ICONE = '@5C@'.
      ENDIF.

      WA_SAIDA-VAL_TOT_APLICADO = VL_TOT_APLICADO.

      LOOP AT IT_ZSDT0054_COMPENS INTO WA_ZSDT0054 WHERE NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV.
        VL_TOT                  = VL_TOT + WA_ZSDT0054-DMBTR.
      ENDLOOP.

      WA_SAIDA-VAL_TOT = VL_TOT.

      WA_SAIDA-VAL_TOT_AAPLICAR = WA_SAIDA-VAL_TOT - WA_SAIDA-VAL_TOT_APLICADO.

      IF WA_SAIDA-VAL_TOT_AAPLICAR EQ 0.
        READ TABLE IT_ZSDT0053 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV
                                              VBELN = SPACE TRANSPORTING NO FIELDS.
        IF SY-SUBRC IS INITIAL.
          APPEND WA_SAIDA TO IT_SAIDA.
        ENDIF.
      ELSE.
        APPEND WA_SAIDA TO IT_SAIDA.
      ENDIF.

      CLEAR: VL_TOT_APLICADO, VL_TOT, VL_SEM_ORDEM.

    ENDLOOP.

  ELSEIF P_APLIC EQ ABAP_TRUE.

    LOOP AT IT_ZSDT0051 INTO WA_ZSDT0051.

      WA_SAIDA-NRO_SOL_OV           = WA_ZSDT0051-NRO_SOL_OV.
      WA_SAIDA-KUNNR                = WA_ZSDT0051-KUNNR.
      WA_SAIDA-WAERK                = WA_ZSDT0051-WAERK.
      WA_SAIDA-DTDE_LOGIST          = WA_ZSDT0051-DTDE_LOGIST.
      WA_SAIDA-DTATE_LOGIST         = WA_ZSDT0051-DTATE_LOGIST.

      READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_ZSDT0051-KUNNR.
      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-NAME1              = WA_KNA1-NAME1.
      ENDIF.

      READ TABLE IT_ZSDT0054 INTO WA_ZSDT0054 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV.
      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-KURSF              = WA_ZSDT0054-KURSF.
      ENDIF.

      LOOP AT IT_ZSDT0053 INTO WA_ZSDT0053 WHERE NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV.

        CHECK WA_ZSDT0053-VBELN IS NOT INITIAL.

        WA_SAIDA-VBELN              = WA_ZSDT0053-VBELN.
        WA_SAIDA-WERKS              = WA_ZSDT0053-WERKS.
        WA_SAIDA-MATNR              = WA_ZSDT0053-MATNR.
        WA_SAIDA-ZMENG              = WA_ZSDT0053-ZMENG.
        WA_SAIDA-ZIEME              = WA_ZSDT0053-ZIEME.
        WA_SAIDA-TOT_OV             = WA_ZSDT0053-VLRTOT.
        WA_SAIDA-DMBTR              = WA_ZSDT0053-DMBTR.
        WA_SAIDA-PMEIN              = WA_ZSDT0053-PMEIN.
        WA_SAIDA-VAL_TOT_APLICADO   = WA_ZSDT0053-VLRTOT.
        WA_SAIDA-NAVIO              = WA_ZSDT0053-NAVIO.
        WA_SAIDA-PORTO              = WA_ZSDT0053-PORTO.

        READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_ZSDT0053-MATNR.
        IF SY-SUBRC IS INITIAL.
          WA_SAIDA-MAKTX            = WA_MAKT-MAKTX.
        ENDIF.

        LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV = WA_ZSDT0053-VBELN.
          VL_TOTAL_VBFA             = VL_TOTAL_VBFA + WA_VBFA-RFMNG.
        ENDLOOP.

        WA_SAIDA-TOT_VBFA           = VL_TOTAL_VBFA.

        APPEND WA_SAIDA TO IT_SAIDA.
        CLEAR: VL_TOTAL_VBFA.

      ENDLOOP.

    ENDLOOP.

  ENDIF.

  SORT IT_SAIDA BY NRO_SOL_OV ASCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_5000 OUTPUT.

  SET PF-STATUS 'STATUS5000'.
  SET TITLEBAR 'T0001'.

  PERFORM CARREGA_RELATORIO.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_RELATORIO
*&---------------------------------------------------------------------*
FORM CARREGA_RELATORIO .

  IF G_CUSTOM_CONTAINER IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER5000'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC <> 0.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.

    CREATE OBJECT CTL_ALV1
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER.

    PERFORM FILL_IT_FIELDCATALOG TABLES IT_FIELDCATALOG USING P_AAPLI P_APLIC.

    GS_LAYOUT-CWIDTH_OPT = 'X'.
    GS_LAYOUT-SEL_MODE = 'A'.

    SET HANDLER:
      LCL_EVENT_HANDLER=>HANDLE_HOTSPOT_CLICK FOR CTL_ALV1,
      LCL_EVENT_HANDLER=>TOOLBAR FOR CTL_ALV1,
      LCL_EVENT_HANDLER=>USER_COMMAND FOR CTL_ALV1.

    CALL METHOD CTL_ALV1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT
        I_SAVE          = 'A'
*       IT_TOOLBAR_EXCLUDING = IT_EXCLUDE
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG
        IT_OUTTAB       = IT_SAIDA.
*        IT_SORT              = IT_SORT.

  ELSE.

    CALL METHOD CTL_ALV1->REFRESH_TABLE_DISPLAY.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG  TABLES   P_IT_FIELDCATALOG STRUCTURE LVC_S_FCAT
                           USING    VALUE(P_P_AAPLI)
                                    VALUE(P_P_APLIC).

  DATA: WA_FIELDCATALOG TYPE LVC_S_FCAT.

  IF P_P_AAPLI IS NOT INITIAL.

    PERFORM COMPLETA_TABELA_CATALOG TABLES P_IT_FIELDCATALOG USING:
          01 'ICONE'              'ZSDT0051'  ' '      ' '  'X'  'C'  'Status',
          02 'NRO_SOL_OV'         'ZSDT0051'  ' '      'X'  'X'  ' '  'Nr. Sol. OV',
          03 'VKORG'              'ZSDT0051'  ' '      ' '  'X'  ' '  'Org. Vendas',
          04 'AUART'              'ZSDT0051'  ' '      ' '  'X'  ' '  'Tipo OV',
          05 'KUNNR'              'ZSDT0051'  ' '      ' '  'X'  ' '  'Cód. Cliente',
          06 'NAME1'              'KNA1'      ' '      ' '  'X'  ' '  'Nome Cliente',
          07 'DTDE_LOGIST'        'ZSDT0051'  ' '      ' '  'X'  ' '  'Prog. Log. Inicial',
          08 'DTATE_LOGIST'       'ZSDT0051'  ' '      ' '  'X'  ' '  'Prog. Log. Final',
          09 'WAERK'              'ZSDT0051'  ' '      ' '  'X'  ' '  'Moeda',
          10 'VAL_TOT'            ''          'DMBTR'  ' '  'X'  ' '  'Valor Total',
          11 'VAL_TOT_APLICADO'   ''          'DMBTR'  ' '  'X'  ' '  'Valor Aplicado',
          12 'VAL_TOT_AAPLICAR'   ''          'DMBTR'  ' '  'X'  ' '  'Valor a Aplicar',
          13 'KURSF'              'ZSDT0054'  ' '      ' '  'X'  ' '  'Tx. Dolar'.

  ELSEIF P_P_APLIC IS NOT INITIAL.

    PERFORM COMPLETA_TABELA_CATALOG TABLES P_IT_FIELDCATALOG USING:
          01 'NRO_SOL_OV'         'ZSDT0051'  ' '      'X'  'X'  ' '  'Nr. Sol. OV',
          02 'VBELN'              'ZSDT0053'  ' '      'X'  'X'  ' '  'Ordem Venda',
          03 'WERKS'              'ZSDT0053'  ' '      ' '  'X'  ' '  'Centro Forn.',
          04 'KUNNR'              'ZSDT0051'  ' '      ' '  'X'  ' '  'Cód. Cliente',
          05 'NAME1'              'KNA1'      ' '      ' '  'X'  ' '  'Nome Cliente',
          06 'DTDE_LOGIST'        'ZSDT0051'  ' '      ' '  'X'  ' '  'Prog. Log. Inicial',
          07 'DTATE_LOGIST'       'ZSDT0051'  ' '      ' '  'X'  ' '  'Prog. Log. Final',
          08 'MATNR'              'ZSDT0053'  ' '      ' '  'X'  ' '  'Material',
          09 'MAKTX'              'MAKT'      ' '      ' '  'X'  ' '  'Desc. Material',
          10 'ZMENG'              'ZSDT0053'  ' '      ' '  'X'  ' '  'Quant. Prevista',
          11 'TOT_VBFA'           ''          'RFMNG'  ' '  'X'  ' '  'Quant. Remessa',
          12 'ZIEME'              'ZSDT0053'  ' '      ' '  'X'  ' '  'UM',
          13 'WAERK'              'ZSDT0051'  ' '      ' '  'X'  ' '  'Moeda',
          14 'TOT_OV'             ''          'DMBTR'  ' '  'X'  ' '  'Valor Total OV',
          15 'DMBTR'              'ZSDT0053'  ' '      ' '  'X'  ' '  'Preço',
          16 'PMEIN'              'ZSDT0053'  ' '      ' '  'X'  ' '  'UM',
          17 'VAL_TOT_APLICADO'   ''          'DMBTR'  ' '  'X'  ' '  'Valor Aplicado',
          18 'KURSF'              'ZSDT0054'  ' '      ' '  'X'  ' '  'Tx. Dolar',
          19 'NAVIO'              'ZSDT0053'  ' '      ' '  'X'  ' '  'Navio',
          20 'PORTO'              'ZSDT0053'  ' '      ' '  'X'  ' '  'Porto'.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_TABELA_CATALOG
*&---------------------------------------------------------------------*
FORM COMPLETA_TABELA_CATALOG  TABLES P_IT_FIELDACATALOG STRUCTURE LVC_S_FCAT
                               USING VALUE(P_COLNUM)
                                     VALUE(P_FIELDNAME)
                                     VALUE(P_TABNAME)
                                     VALUE(P_DOMNAME)
                                     VALUE(P_HOTSPOT)
                                     VALUE(P_OPT)
                                     VALUE(P_JUST)
                                     VALUE(P_HEADER).

  DATA: WA_FIELDCATALOG TYPE LVC_S_FCAT.

  WA_FIELDCATALOG-COL_POS    = P_COLNUM.
  WA_FIELDCATALOG-FIELDNAME  = P_FIELDNAME.
  WA_FIELDCATALOG-TABNAME    = P_TABNAME.
  WA_FIELDCATALOG-COLTEXT    = P_HEADER.
  WA_FIELDCATALOG-REF_TABLE  = P_TABNAME.
  WA_FIELDCATALOG-DOMNAME    = P_DOMNAME.
  WA_FIELDCATALOG-HOTSPOT    = P_HOTSPOT.
  WA_FIELDCATALOG-COL_OPT    = P_OPT.
  WA_FIELDCATALOG-JUST       = P_JUST.
  APPEND WA_FIELDCATALOG TO P_IT_FIELDACATALOG.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5000_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form BDC_DATA
*&---------------------------------------------------------------------*
FORM BDC_DATA  USING P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.

  DATA: WA_BDCDATA TYPE BDCDATA.

  WA_BDCDATA-PROGRAM = P_PROGRAM.
  WA_BDCDATA-DYNPRO = P_DYNPRO.
  WA_BDCDATA-DYNBEGIN = P_START.
  WA_BDCDATA-FNAM = P_FNAM.
  WA_BDCDATA-FVAL = P_FVAL.
  APPEND WA_BDCDATA TO IT_BDCDATA.
  CLEAR: WA_BDCDATA.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION USING P_TRANS CHANGING VL_ERRO.

  DATA: IT_MSG  TYPE STANDARD TABLE OF BDCMSGCOLL,
        WL_MODE TYPE CHAR1.

  FREE IT_MSG.

  WL_MODE = 'E'.

  CALL TRANSACTION P_TRANS USING IT_BDCDATA
                           MODE WL_MODE.

  PERFORM SELECIONA_DADOS.
  PERFORM MONTA_SAIDA.
  PERFORM CARREGA_RELATORIO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
FORM PREPARA_EMAIL .

  DATA: IT_KNA1            TYPE STANDARD TABLE OF KNA1,
        IT_MAKT            TYPE STANDARD TABLE OF MAKT,
        IT_EXCEL_KEY_NAVIO TYPE STANDARD TABLE OF TY_EXCEL,
        IT_EXCEL           TYPE STANDARD TABLE OF TY_EXCEL,
        IT_EXCEL_AUX       TYPE STANDARD TABLE OF TY_EXCEL,
        WA_EXCEL_AUX       TYPE TY_EXCEL,
        WA_EXCEL_KEY_NAVIO TYPE TY_EXCEL,
        WA_KNA1            TYPE KNA1,
        WA_MAKT            TYPE MAKT,
        WA_ZSDT0054        TYPE ZSDT0054,
        WA_ZSDT0053        TYPE ZSDT0053,
        WA_SAIDA_EMAIL     TYPE TY_SAIDA,
        WA_EXCEL           TYPE TY_EXCEL.

  CLEAR: IT_ANEXO_1, IT_ANEXO_2.

  IF IT_ZSDT0053 IS NOT INITIAL.

    SELECT *
      FROM KNA1
      INTO TABLE IT_KNA1
      FOR ALL ENTRIES IN IT_ZSDT0051
      WHERE KUNNR EQ IT_ZSDT0051-KUNNR.

    SELECT *
      FROM MAKT
      INTO TABLE IT_MAKT
      FOR ALL ENTRIES IN IT_ZSDT0053
      WHERE MATNR EQ IT_ZSDT0053-MATNR
        AND SPRAS EQ SY-LANGU.

  ENDIF.

  LOOP AT IT_SAIDA_EMAIL INTO WA_SAIDA_EMAIL.

    LOOP AT IT_ZSDT0053 INTO WA_ZSDT0053 WHERE NRO_SOL_OV EQ WA_SAIDA_EMAIL-NRO_SOL_OV
                                           AND VBELN      EQ SPACE.

      WA_EXCEL-NRO_SOL_OV       = WA_SAIDA_EMAIL-NRO_SOL_OV.
      WA_EXCEL-WAERK            = WA_SAIDA_EMAIL-WAERK.

      READ TABLE IT_ZSDT0054 INTO WA_ZSDT0054 WITH KEY NRO_SOL_OV = WA_SAIDA_EMAIL-NRO_SOL_OV.
      IF SY-SUBRC IS INITIAL.
        WA_EXCEL-KURSF          = WA_ZSDT0054-KURSF.
      ENDIF.

      READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_SAIDA_EMAIL-KUNNR.
      IF SY-SUBRC IS INITIAL.
        WA_EXCEL-NAME1        = WA_KNA1-NAME1.
        IF WA_KNA1-STKZN IS NOT INITIAL.
          WA_EXCEL-STCDX      = WA_KNA1-STCD2.
        ELSE.

          CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
            EXPORTING
              INPUT  = WA_KNA1-STCD1
            IMPORTING
              OUTPUT = WA_EXCEL-STCDX.

*          WA_EXCEL-STCDX      = WA_KNA1-STCD1.

        ENDIF.
      ENDIF.

      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_ZSDT0053-MATNR.
      IF SY-SUBRC IS INITIAL.
        WA_EXCEL-MAKTX        = WA_MAKT-MAKTX.
      ENDIF.

      WA_EXCEL-ZMENG          = WA_ZSDT0053-ZMENG.
      WA_EXCEL-ZIEME          = WA_ZSDT0053-ZIEME.
      WA_EXCEL-VLT_PORTO      = WA_ZSDT0053-VLT_PORTO.
      WA_EXCEL-P_PORTO        = WA_ZSDT0053-P_PORTO.
      WA_EXCEL-PMEIN          = WA_ZSDT0053-PMEIN.
      WA_EXCEL-NAVIO          = WA_ZSDT0053-NAVIO.
      WA_EXCEL-PORTO          = WA_ZSDT0053-PORTO.
      WA_EXCEL-WERKS          = WA_ZSDT0053-WERKS.

      APPEND WA_EXCEL TO IT_EXCEL.
      CLEAR: WA_EXCEL.

    ENDLOOP.

  ENDLOOP.

  IT_EXCEL_KEY_NAVIO = IT_EXCEL.
  SORT IT_EXCEL_KEY_NAVIO BY NAVIO.
  DELETE ADJACENT DUPLICATES FROM IT_EXCEL_KEY_NAVIO COMPARING NAVIO.

  LOOP AT IT_EXCEL_KEY_NAVIO INTO WA_EXCEL_KEY_NAVIO.

    IT_EXCEL_AUX = IT_EXCEL.
    DELETE IT_EXCEL_AUX WHERE NAVIO NE WA_EXCEL_KEY_NAVIO-NAVIO.

    LOOP AT IT_EXCEL_AUX INTO WA_EXCEL_AUX.
      WA_EXCEL_AUX-C_KURSF      = WA_EXCEL_AUX-KURSF.
      WA_EXCEL_AUX-WERKS        = WA_EXCEL_AUX-WERKS.
      WA_EXCEL_AUX-C_P_PORTO    = WA_EXCEL_AUX-P_PORTO.
      WA_EXCEL_AUX-KURSF        = SPACE.
      WA_EXCEL_AUX-P_PORTO      = SPACE.
      WA_EXCEL_AUX-NRO_SOL_OV   = SPACE.
      COLLECT WA_EXCEL_AUX INTO IT_ANEXO_1.
    ENDLOOP.

    IT_ANEXO_2 = IT_EXCEL_AUX.
    SORT IT_ANEXO_2 BY NRO_SOL_OV.
    DELETE ADJACENT DUPLICATES FROM IT_ANEXO_2 COMPARING NRO_SOL_OV.

    IF IT_ANEXO_1 IS NOT INITIAL.
      PERFORM CREATE_CONTENT TABLES IT_ANEXO_1
                                    IT_ANEXO_2
                             USING WA_EXCEL_KEY_NAVIO-NAVIO.
      PERFORM SEND USING WA_EXCEL_KEY_NAVIO-NAVIO.
      CLEAR: IT_ANEXO_1, IT_ANEXO_2.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTENT
*&---------------------------------------------------------------------*
FORM CREATE_CONTENT TABLES IT_ANEXO_1
                           IT_ANEXO_2
                    USING VALUE(P_NAVIO) TYPE ZSDT0053-NAVIO.

  DATA: LV_STRING    TYPE STRING,
        WA_ANEXO_1   TYPE TY_EXCEL,
        WA_ANEXO_2   TYPE TY_EXCEL,
        VL_ZMENG     TYPE STRING,
        VL_VLT_PORTO TYPE STRING,
        VL_P_PORTO   TYPE STRING,
        VL_KURSF     TYPE STRING.
* --------------------------------------------------------------
* as example content we use some system messages out of t100
* get them for all installed languages from db
* and write one line for each language into the spread sheet

* columns are separated by TAB and each line ends with CRLF

  CONSTANTS:
    GC_TAB  TYPE C VALUE CL_BCS_CONVERT=>GC_TAB,
    GC_CRLF TYPE C VALUE CL_BCS_CONVERT=>GC_CRLF.

* header line
  CONCATENATE
    'CNPJ/CPF'            GC_TAB
    'Nome Cliente'        GC_TAB
    'Centro'              GC_TAB
    'Desc. Material'      GC_TAB
    'Quant. Prevista'     GC_TAB
    'UM'                  GC_TAB
    'Valor porto'         GC_TAB
    'Moeda'               GC_TAB
    'Preço porto'         GC_TAB
    'UM'                  GC_TAB
    'Tx. Dolar'           GC_TAB
    'Navio'               GC_TAB
    'Porto'               GC_CRLF
  INTO LV_STRING.

  LOOP AT IT_ANEXO_1 INTO WA_ANEXO_1.

*    VL_ZMENG      = WA_ANEXO_1-ZMENG.
*    VL_VLT_PORTO  = WA_ANEXO_1-VLT_PORTO.
*    VL_P_PORTO    = WA_ANEXO_1-C_P_PORTO.
    VL_KURSF      = WA_ANEXO_1-C_KURSF.

    WA_ANEXO_1-P_PORTO = WA_ANEXO_1-C_P_PORTO .

*    REPLACE ALL OCCURRENCES OF '.' IN VL_ZMENG      WITH ','.
*    REPLACE ALL OCCURRENCES OF '.' IN VL_VLT_PORTO  WITH ','.
*    REPLACE ALL OCCURRENCES OF '.' IN VL_P_PORTO    WITH ','.
    REPLACE ALL OCCURRENCES OF '.' IN VL_KURSF      WITH ','.

    VL_ZMENG      = |{ WA_ANEXO_1-ZMENG NUMBER = USER }|.
    VL_VLT_PORTO  = |{ WA_ANEXO_1-VLT_PORTO CURRENCY = 'BRL' NUMBER = USER }|.
    VL_P_PORTO    = |{ WA_ANEXO_1-P_PORTO CURRENCY = 'BRL' NUMBER = USER }|.
*    VL_KURSF      = |{ WA_ANEXO_1-KURSF CURRENCY = 'BRL' NUMBER = USER }|.

    CONCATENATE
      LV_STRING
      WA_ANEXO_1-STCDX      GC_TAB
      WA_ANEXO_1-NAME1      GC_TAB
      WA_ANEXO_1-WERKS      GC_TAB
      WA_ANEXO_1-MAKTX      GC_TAB
*      WA_anexo-ZMENG       GC_TAB
      VL_ZMENG              GC_TAB
      WA_ANEXO_1-ZIEME      GC_TAB
*      WA_anexo-VLT_PORTO   GC_TAB
      VL_VLT_PORTO          GC_TAB
      WA_ANEXO_1-WAERK      GC_TAB
*      WA_anexo-P_PORTO     GC_TAB
      VL_P_PORTO            GC_TAB
      WA_ANEXO_1-PMEIN      GC_TAB
*      WA_anexo-KURSF       GC_TAB
      VL_KURSF              GC_TAB
      WA_ANEXO_1-NAVIO      GC_TAB
      WA_ANEXO_1-PORTO      GC_CRLF
    INTO LV_STRING.

    CLEAR: VL_ZMENG, VL_VLT_PORTO, VL_P_PORTO, VL_KURSF.

  ENDLOOP.

* header line
  CONCATENATE
    LV_STRING               GC_CRLF
                            GC_CRLF
    'Nro. Sol.'             GC_CRLF
  INTO LV_STRING.

  LOOP AT IT_ANEXO_2 INTO WA_ANEXO_2.

    CONCATENATE
      LV_STRING
      WA_ANEXO_2-NRO_SOL_OV GC_CRLF
    INTO LV_STRING.

  ENDLOOP.

* --------------------------------------------------------------
* convert the text string into UTF-16LE binary data including
* byte-order-mark. Mircosoft Excel prefers these settings
* all this is done by new class cl_bcs_convert (see note 1151257)

  CLEAR: SIZE, BINARY_CONTENT.

  TRY.
      CL_BCS_CONVERT=>STRING_TO_SOLIX(
        EXPORTING
          IV_STRING   = LV_STRING
          IV_CODEPAGE = '4103'  "suitable for MS Excel, leave empty
          IV_ADD_BOM  = 'X'     "for other doc types
        IMPORTING
          ET_SOLIX  = BINARY_CONTENT
          EV_SIZE   = SIZE ).
    CATCH CX_BCS.
      MESSAGE E445(SO).
  ENDTRY.

ENDFORM.                    "create_content

*&---------------------------------------------------------------------*
*&      Form  SEND
*&---------------------------------------------------------------------*
FORM SEND USING VALUE(P_NAVIO) TYPE ZSDT0053-NAVIO.

  DATA: MAILTO  TYPE AD_SMTPADR,                            "#EC *
        SUBJECT TYPE SO_OBJ_DES.                            "#EC *

  DATA: IT_ZMAIL TYPE STANDARD TABLE OF ZMAIL,
        WA_ZMAIL TYPE ZMAIL.

  DATA: VUSER   TYPE SY-UNAME.

  CLEAR: MAIN_TEXT, SUBJECT, IT_ZMAIL, VUSER.

  SELECT *
    FROM ZMAIL
    INTO TABLE IT_ZMAIL
    WHERE TCODE EQ 'ZSDT0122'.

  CONCATENATE P_NAVIO '-' SY-DATUM+6(2) INTO SUBJECT SEPARATED BY SPACE.
  CONCATENATE SUBJECT '.' SY-DATUM+4(2) '.' SY-DATUM(4) INTO SUBJECT.

  TRY.

*     -------- create persistent send request ------------------------
      SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).
*     -------- create and set document with attachment ---------------
*     create document object from internal table with text
      APPEND TEXT-004 TO MAIN_TEXT.                         "#EC NOTEXT
      APPEND ' '      TO MAIN_TEXT.                         "#EC NOTEXT
      APPEND TEXT-005 TO MAIN_TEXT.                         "#EC NOTEXT
      APPEND P_NAVIO  TO MAIN_TEXT.                         "#EC NOTEXT
      APPEND ' '      TO MAIN_TEXT.                         "#EC NOTEXT
      APPEND ' '      TO MAIN_TEXT.                         "#EC NOTEXT
      APPEND ' '      TO MAIN_TEXT.                         "#EC NOTEXT
      APPEND TEXT-007 TO MAIN_TEXT.                         "#EC NOTEXT
      APPEND TEXT-006 TO MAIN_TEXT.                         "#EC NOTEXT

      DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
        I_TYPE    = 'RAW'
        I_TEXT    = MAIN_TEXT
        I_SUBJECT = SUBJECT ).                              "#EC NOTEXT

*     add the spread sheet as attachment to document object
      DOCUMENT->ADD_ATTACHMENT(
        I_ATTACHMENT_TYPE    = 'xls'                        "#EC NOTEXT
        I_ATTACHMENT_SUBJECT = P_NAVIO                      "#EC NOTEXT
        I_ATTACHMENT_SIZE    = SIZE
        I_ATT_CONTENT_HEX    = BINARY_CONTENT ).

*     add document object to send request
      SEND_REQUEST->SET_DOCUMENT( DOCUMENT ).

      LOOP AT IT_ZMAIL INTO WA_ZMAIL.
        IF WA_ZMAIL IS NOT INITIAL.
          CLEAR: MAILTO.
          MAILTO = WA_ZMAIL-EMAIL.
*     --------- add recipient (e-mail address) -----------------------
*     create recipient object
          RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( MAILTO ).

*     add recipient object to send request
          SEND_REQUEST->ADD_RECIPIENT( RECIPIENT ).
        ENDIF.
      ENDLOOP.

*     ---------- send document ---------------------------------------
      VUSER = SY-UNAME.
      SY-UNAME = 'JOBADM'.
      SENT_TO_ALL = SEND_REQUEST->SEND( I_WITH_ERROR_SCREEN = 'X' ).

      COMMIT WORK.
      SY-UNAME = VUSER.

      IF SENT_TO_ALL IS INITIAL.
*        MESSAGE I500(SBCOMS) WITH MAILTO.
        MESSAGE S026(SO).
      ELSE.
        MESSAGE S022(SO).
      ENDIF.

*   ------------ exception handling ----------------------------------
*   replace this rudimentary exception handling with your own one !!!
    CATCH CX_BCS INTO BCS_EXCEPTION.
      MESSAGE I865(SO) WITH BCS_EXCEPTION->ERROR_TYPE.
  ENDTRY.

ENDFORM.                    "send
