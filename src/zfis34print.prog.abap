*&---------------------------------------------------------------------*
*&  Include           ZFIS34PRINT
*&---------------------------------------------------------------------*

MODULE CRIAR_ALV_0001 OUTPUT.

    IF IT_SAIDA_PRIN[] IS INITIAL AND SY-UCOMM IS NOT INITIAL.
      MESSAGE TEXT-002 TYPE 'I'.
    ENDIF.
      PERFORM CRIAR_CATALOG_0001.
      PERFORM CRIAR_ALV_0001.

ENDMODULE.

MODULE CRIAR_ALV_0002 OUTPUT.

  PERFORM CRIAR_CATALOG_0002.
  PERFORM CRIAR_ALV_0002.

ENDMODULE.


CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ZM_HANDLE_HOTSPOT_REPORT
                    FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

ENDCLASS.                    "lcl_event_compras DEFINITION

CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT_REPORT.

    PERFORM Z_HOTSPOT_REPORT USING  E_ROW_ID E_COLUMN_ID ES_ROW_NO.

  ENDMETHOD.
ENDCLASS.

FORM Z_HOTSPOT_REPORT USING    P_E_ROW_ID    TYPE  LVC_S_ROW
                                P_E_COLUMN_ID TYPE  LVC_S_COL
                                P_ES_ROW_NO   TYPE  LVC_S_ROID.

  CLEAR V_TPMERC. CLEAR V_FILIAL. CLEAR WA_SAIDA_PRIN.
  FREE IT_SAIDA_ITEM.

  IF P_E_ROW_ID-ROWTYPE IS INITIAL." ( SY-SUBRC EQ 0 ).
    CASE P_E_COLUMN_ID.
      WHEN: 'TOTAL_ME'.

        READ TABLE IT_SAIDA_PRIN INTO WA_SAIDA_PRIN INDEX P_E_ROW_ID.
        IF WA_SAIDA_PRIN-TOTAL_ME IS NOT INITIAL.
          V_TPMERC = 'ME'.
          V_FILIAL = WA_SAIDA_PRIN-FILIAL.
          PERFORM SELECIONA_ITEM USING V_TPMERC V_FILIAL.

          IF NOT ( IT_SAIDA_ITEM[] IS INITIAL ).
            CALL SCREEN 0103.
          ENDIF.
        ELSE.
          MESSAGE TEXT-007 TYPE 'I'.
        ENDIF.


      WHEN: 'TOTAL_MI'.

        READ TABLE IT_SAIDA_PRIN INTO WA_SAIDA_PRIN INDEX P_E_ROW_ID.
        IF WA_SAIDA_PRIN-TOTAL_MI IS NOT INITIAL.
          V_TPMERC = 'MI'.
          V_FILIAL = WA_SAIDA_PRIN-FILIAL.
          PERFORM SELECIONA_ITEM USING V_TPMERC V_FILIAL.

          IF NOT ( IT_SAIDA_ITEM[] IS INITIAL ).
            CALL SCREEN 0103.
          ENDIF.
        ELSE.
          MESSAGE TEXT-007 TYPE 'I'.
        ENDIF.


      WHEN: 'CRED_ICMS'.

        READ TABLE IT_SAIDA_PRIN INTO WA_SAIDA_PRIN INDEX P_E_ROW_ID.
        IF WA_SAIDA_PRIN-CRED_ICMS IS NOT INITIAL .
          V_TPMERC = 'EN'.
          V_FILIAL = WA_SAIDA_PRIN-FILIAL.
          PERFORM SELECIONA_ITEM USING V_TPMERC V_FILIAL.

          IF NOT ( IT_SAIDA_ITEM[] IS INITIAL ).
            CALL SCREEN 0103.
          ENDIF.
        ELSE.
          MESSAGE TEXT-007 TYPE 'I'.
        ENDIF.

      WHEN: 'NR_DOCUMENTO'.

        READ TABLE IT_SAIDA_PRIN INTO WA_SAIDA_PRIN INDEX P_E_ROW_ID.
        IF WA_SAIDA_PRIN-NR_DOCUMENTO CS 'r'.
          CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
            EXPORTING
              TEXTLINE1 = WA_SAIDA_PRIN-NR_DOCUMENTO.
        ELSE.
          SET PARAMETER ID 'BLN'   FIELD WA_SAIDA_PRIN-NR_DOCUMENTO.
          SET PARAMETER ID 'BUK'   FIELD P_BUKRS-LOW.
          SET PARAMETER ID 'GJAHR' FIELD P_ANO-LOW.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.

      WHEN: 'NR_DOC_ESTORNO'.

        READ TABLE IT_SAIDA_PRIN INTO WA_SAIDA_PRIN INDEX P_E_ROW_ID.
        IF WA_SAIDA_PRIN-NR_DOC_ESTORNO CS 'r'.
          CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
            EXPORTING
              TEXTLINE1 = WA_SAIDA_PRIN-NR_DOC_ESTORNO.
        ELSE.
          SET PARAMETER ID 'BLN'   FIELD WA_SAIDA_PRIN-NR_DOC_ESTORNO.
          SET PARAMETER ID 'BUK'   FIELD P_BUKRS-LOW.
          SET PARAMETER ID 'GJAHR' FIELD P_ANO-LOW.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.

    ENDCASE.
  ELSE.
    MESSAGE TEXT-010 TYPE 'I'.
  ENDIF.

ENDFORM.



FORM CRIAR_CATALOG_0001.
  REFRESH: IT_FCAT[].

  PERFORM ALV_CATALOG_0001  USING :

'IT_SAIDA_PRIN'        'STATUS'               'Status'                      '08'    'X' ''  ''      'MATN1' 'C' ,
'IT_SAIDA_PRIN'        'FILIAL'               'Filial'                      '08'    ''  ''  ''      ''      '' ,
'IT_SAIDA_PRIN'        'NOME_FILIAL'          'Nome da Filial'              '28'    ''  ''  ''      ''      '' ,
'IT_SAIDA_PRIN'        'CENTRO_CUSTO'         'Centro de Custo'             '13'    ''  ''  ''      ''      '' ,
'IT_SAIDA_PRIN'        'TOTAL_ME'             'Vlr.Total-ME'                '11'    ''  'X' 'C500'  ''      '' ,
'IT_SAIDA_PRIN'        'TOTAL_MI'             'Vlr.Total-MI'                '12'    ''  'X' 'C500'  ''      '' ,
'IT_SAIDA_PRIN'        'TOTAL_NF'             'Vlr.Total-NF(ME+MI)'         '14'    ''  'X' 'C500'  ''      '' ,
'IT_SAIDA_PRIN'        'P_ESTORNO'            '% Estorno'                   '09'    ''  ''  ''      ''      '' ,
'IT_SAIDA_PRIN'        'CRED_ICMS'            'Vlr.Crédito ICMS'            '14'    ''  'X' 'C500'  ''      '' ,
'IT_SAIDA_PRIN'        'V_ESTORNO'            'Vlr.Estorno'                 '16'    ''  'X' 'C500'  ''      '' ,
'IT_SAIDA_PRIN'        'NR_DOCUMENTO'         'Nro.Documento'               '15'    ''  ''  ''      ''      'C',
'IT_SAIDA_PRIN'        'NR_DOC_ESTORNO'       'Doc.Estorno'                 '15'    ''  ''  ''      ''      'C' .


ENDFORM.


FORM CRIAR_CATALOG_0002.
  REFRESH: IT_FCAT2[].

  PERFORM ALV_CATALOG_0001  USING:

'IT_SAIDA_ITEM'        'DOCNUM'               'Doc.Num.'                    '08'       'X' ''  ''      'MATN1' 'C' ,
'IT_SAIDA_ITEM'        'NOTA_FISCAL'          'NF'                          '14'       ''  ''  ''      ''      '' ,
'IT_SAIDA_ITEM'        'DT_LCTO'              'Dt.Lcto'                     '12'       ''  ''  ''      ''      '' ,
'IT_SAIDA_ITEM'        'DT_DCTO'              'Dt.Dcto'                     '12'       ''  ''  ''      ''      '' ,
'IT_SAIDA_ITEM'        'FILIAL'               'Filial'                      '08'       ''  ''  ''      ''      '' ,
'IT_SAIDA_ITEM'        'FILIAL_NAME'          'Nome Filial'                 '25'       ''  ''  ''      ''      '' ,
'IT_SAIDA_ITEM'        'MATNR'                'Material'                    '08'       'X' ''  ''      ''      '' ,
'IT_SAIDA_ITEM'        'MAKTX'                'Desc.Material'               '30'       ''  ''  ''      ''      '' ,
'IT_SAIDA_ITEM'        'QUANTIDADE'           'Quant'                       '09'       ''  ''  ''      ''      '' ,
'IT_SAIDA_ITEM'        'PRECO'                'Preço'                       '14'       ''  'X'  'C500'  ''      '' ,
'IT_SAIDA_ITEM'        'VALOR_NOTA'           'Valor NF'                    '14'       ''  'X'  'C500'  ''      '' ,
'IT_SAIDA_ITEM'        'CFOP'                 'CFOP'                        '10'       ''  ''  ''      ''      ''.

ENDFORM.


FORM ALV_CATALOG_0001  USING   P_TABLE    TYPE C
                               P_CAMPO    TYPE C
                               P_DESC     TYPE C
                               P_TAM      TYPE C
                               P_ZERO     TYPE C
                               P_SUM      TYPE C
                               P_COR      TYPE C
                               P_CONVEXIT TYPE C
                               P_JUST     TYPE C.


  DATA: P_HOT   TYPE C,
        WL_FCAT TYPE LVC_S_FCAT,
        MARCA   TYPE C VALUE 'X'.

  IF P_TABLE EQ 'IT_SAIDA_PRIN'.
    CASE P_CAMPO.
      WHEN 'TOTAL_ME' OR 'TOTAL_MI' OR 'CRED_ICMS' OR 'NR_DOCUMENTO' OR 'NR_DOC_ESTORNO'.
        P_HOT = MARCA.
    ENDCASE.
  ENDIF.

  WL_FCAT-TABNAME   = P_TABLE.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SCRTEXT_L = P_DESC.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-DO_SUM   =  P_SUM.
  WL_FCAT-EMPHASIZE = P_COR.
  WL_FCAT-CONVEXIT  = P_CONVEXIT.
  WL_FCAT-JUST      = P_JUST.

  IF P_TABLE EQ 'IT_SAIDA_PRIN'.
    APPEND WL_FCAT TO IT_FCAT.
  ELSE.
    APPEND WL_FCAT TO IT_FCAT2.
  ENDIF.


ENDFORM.


FORM CRIAR_ALV_0001.

  DATA: WA_EVENT_0001    TYPE REF TO LCL_EVENT_RECEIVER.

  DATA:   G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
          DG_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
          DG_PARENT_1        TYPE REF TO CL_GUI_CONTAINER,
          DG_SPLITTER_2      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
          DG_PARENT_2        TYPE REF TO CL_GUI_CONTAINER,
          DG_PARENT_2A       TYPE REF TO CL_GUI_CONTAINER,
          DG_PARENT_ALV      TYPE REF TO CL_GUI_CONTAINER,
          PICTURE            TYPE REF TO CL_GUI_PICTURE,
          DG_DYNDOC_ID       TYPE REF TO CL_DD_DOCUMENT,
          TABLE_ELEMENT      TYPE REF TO CL_DD_TABLE_ELEMENT,
          COLUMN             TYPE REF TO CL_DD_AREA,
          TABLE_ELEMENT2     TYPE REF TO CL_DD_TABLE_ELEMENT,
          COLUMN_1           TYPE REF TO CL_DD_AREA,
          COLUMN_2           TYPE REF TO CL_DD_AREA,
          DG_HTML_CNTRL      TYPE REF TO CL_GUI_HTML_VIEWER.

  IF WA_CONTAINER_0001 IS INITIAL.

    CREATE OBJECT WA_CONTAINER_0001
      EXPORTING
        CONTAINER_NAME              = 'CC_PRINCIPAL'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = WA_CONTAINER_0001
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_ALV.

    CALL METHOD DG_SPLITTER_1->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 13.

    CREATE OBJECT WA_ALV_0001
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    IF WA_EVENT_0001 IS INITIAL.
      CREATE OBJECT WA_EVENT_0001.
      SET HANDLER: WA_EVENT_0001->ZM_HANDLE_HOTSPOT_REPORT FOR WA_ALV_0001.
    ENDIF.

    WA_LAYOUT-CWIDTH_OPT = ' '.
    WA_LAYOUT-SEL_MODE   = 'A'.

    CALL METHOD WA_ALV_0001->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = WA_LAYOUT
        IS_VARIANT      = GS_VARIANT_C
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FCAT
        IT_OUTTAB       = IT_SAIDA_PRIN[].

    CALL METHOD WA_ALV_0001->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER: LCL_EVENT_RECEIVER=>ZM_HANDLE_HOTSPOT_REPORT FOR WA_ALV_0001.

    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.

  ELSE.
    CALL METHOD WA_ALV_0001->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDFORM.                    " CRIAR_ALV_0001


FORM CRIAR_ALV_0002.

  DATA: WA_EVENT_0002    TYPE REF TO LCL_EVENT_RECEIVER.

  DATA:   G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
          DG_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
          DG_PARENT_1        TYPE REF TO CL_GUI_CONTAINER,
          DG_SPLITTER_2      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
          DG_PARENT_2        TYPE REF TO CL_GUI_CONTAINER,
          DG_PARENT_2A       TYPE REF TO CL_GUI_CONTAINER,
          DG_PARENT_ALV      TYPE REF TO CL_GUI_CONTAINER,
          PICTURE            TYPE REF TO CL_GUI_PICTURE,
          DG_DYNDOC_ID       TYPE REF TO CL_DD_DOCUMENT,
          TABLE_ELEMENT      TYPE REF TO CL_DD_TABLE_ELEMENT,
          COLUMN             TYPE REF TO CL_DD_AREA,
          TABLE_ELEMENT2     TYPE REF TO CL_DD_TABLE_ELEMENT,
          COLUMN_1           TYPE REF TO CL_DD_AREA,
          COLUMN_2           TYPE REF TO CL_DD_AREA,
          DG_HTML_CNTRL      TYPE REF TO CL_GUI_HTML_VIEWER.

  IF WA_CONTAINER_0002 IS INITIAL.

    CREATE OBJECT WA_CONTAINER_0002
      EXPORTING
        CONTAINER_NAME              = 'CC_ITEM'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = WA_CONTAINER_0002
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_ALV.

    CALL METHOD DG_SPLITTER_1->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 30.

    CREATE OBJECT WA_ALV_0002
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    IF WA_EVENT_0002 IS INITIAL.
      CREATE OBJECT WA_EVENT_0002.
      SET HANDLER: WA_EVENT_0002->ZM_HANDLE_HOTSPOT_REPORT FOR WA_ALV_0002.
    ENDIF.

    WA_LAYOUT-CWIDTH_OPT = ' '.
    WA_LAYOUT-SEL_MODE   = 'A'.
*
    CALL METHOD WA_ALV_0002->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = WA_LAYOUT
        IS_VARIANT      = GS_VARIANT_C2
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FCAT2
        IT_OUTTAB       = IT_SAIDA_ITEM[].

    CALL METHOD WA_ALV_0002->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER: LCL_EVENT_RECEIVER=>ZM_HANDLE_HOTSPOT_REPORT FOR WA_ALV_0002.

    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.
*

  ELSE.
    CALL METHOD WA_ALV_0002->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDFORM.                    " CRIAR_ALV_0001
