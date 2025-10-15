*&---------------------------------------------------------------------*
*&      Module  SET_SELECTED_ROWS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SELECTED_ROWS OUTPUT.
  CHECK ( WL_CELL IS NOT INITIAL ).

  REFRESH TL_SELECTED_ROWS.

  WL_SELECTED_ROWS = WL_CELL-ROW_ID.
  APPEND WL_SELECTED_ROWS TO TL_SELECTED_ROWS.

  CALL METHOD OBJ_ALV_0140->SET_SELECTED_ROWS
    EXPORTING
      IT_INDEX_ROWS = TL_SELECTED_ROWS.

ENDMODULE.                 " SET_SELECTED_ROWS  OUTPUT

*----------------------------------------------------------------------*
*  MODULE PBO_0140 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE PBO_0140 OUTPUT.
  DATA OBJ_TOOLBAR_0140 TYPE REF TO LCL_EVENT_HANDLER.
  CLEAR: GT_FCAT_0140, WL_LAYOUT.

  PERFORM ALV_PREENCHE_CAT USING:
'CHECK'        ''              '2'  ''  '' '' 'X' 'X' '' '' '' '' '' '' '',
'STATUS'       'Status'        '5'  ''  '' '' '' '' 'X' '' '' '' '' 'X' '',
'BUKRS'        'Empresa'       '7'  ''  '' '' '' '' '' '' '' '' '' '' '',
'WERKS'        'Filial'        '5'  ''  '' '' '' '' '' '' '' '' '' '' '',
'ANLN1'        'Imobilizado'   '10' '' 'X' '' '' '' '' '' '' '' '' '' '',
'ANLN2'        'Sub Nr'        '6'  '' 'X' '' '' '' '' '' '' '' '' '' '',
'KFZKZ'        'Placa Veículo' '13' ''  '' '' '' '' '' '' '' '' '' '' '',
'TXT50'        'Descrição do Veículo' '50' ''  '' '' '' '' '' '' '' '' '' '' '',
'COD_REGI'     'UF'            '3'  ''  '' '' '' '' '' '' '' '' '' '' '',
'COD_REGISTRO' 'Renavam'       '7'  ''  '' '' '' '' '' '' '' '' '' '' '',
'TP_OBRIG'     'Tp Obrigação' '12'  ''  '' '' '' '' '' '' '' '' '' '' '',
'MES_VCTO'     'Mês Vcto'      '8'  ''  '' '' '' '' '' '' '' '' '' '' '',
'ANO_VCTO'     'Exercício'     '9'  ''  '' '' '' 'X' '' '' '' '' '' '' '',
'DT_VENC'      'Vencimento'    '11' ''  '' '' '' 'X' '' 'ZAA003' 'DT_VENC' 'GT_SAIDA_0140' '' '' '',
'DEP_RESP'     'Dpto Resp'     '9'  ''  '' '' '' 'X' '' ''       ''        ''              'X' '' '',
'CONV_BANC'    'Conv Bancário' '12' ''  '' '' 'X' 'X' '' ''  '' '' '' '' '',
'COD_IMPOSTO'  'Tp Imposto'   '12'  ''  '' '' '' 'X' '' '' '' '' '' '' '',
'WAERS'        'Moeda'         '5'  ''  '' '' '' 'X' '' '' '' 'GT_SAIDA_0140' 'X' '' '',
'FILIAL_PG'    'Filial Pgnto'  '10'  ''  '' '' '' 'X' '' 'T001W' 'WERKS' '' '' '' '',
'KOSTL_PG'     'Centro Custo'  '10'  ''  '' '' '' 'X' '' 'CSKS' 'KOSTL' '' '' '' '',
'VLR_PRINC'    'Vlr Principal' '10' ''  '' '' '' '' '' 'ZAA003' 'VLR_PRINC' 'GT_SAIDA_0140' '' '' '',
'VLR_CORRE'    'Vlr Correção'  '10' ''  '' '' '' '' '' 'ZAA003' 'VLR_CORRE' '' '' '' '',
'VLR_MULTA'    'Vlr Multa'     '7'  ''  '' '' '' '' '' 'ZAA003' 'VLR_MULTA' '' '' '' '',
'VLR_JUROS'    'Vlr Juros'     '7'  ''  '' '' '' '' '' 'ZAA003' 'VLR_JUROS' '' '' '' '',
'VLR_TSE'      'Vlr TSE'       '6'  ''  '' '' '' '' '' 'ZAA003' 'VLR_TSE'   '' '' '' '',
'VLR_TOTAL'    'Vlr Total'     '7'  ''  '' '' '' '' '' 'ZAA003' 'VLR_TOTAL' '' '' '' '',
'COD_BARRAS'   'Cód Barras'    '48' ''  '' '' '' 'X' '' '' '' '' '' '' '',
'LOTE'         'Lote'          '4'  ''  '' '' '' '' '' '' '' '' '' 'X' '',
'DOC_IMPOSTO'  'Doc Imposto'   '12' '' 'X' '' '' '' '' '' '' '' '' 'X' '',
'DOC_CONTABIL' 'Doc Contábil'  '12' '' 'X' '' '' '' '' '' '' '' '' 'X' ''.

  IF ( OBJ_CUSTOM_0140 IS INITIAL ).
    CREATE OBJECT OBJ_CUSTOM_0140
      EXPORTING
        CONTAINER_NAME              = 'CUSTOM_0140'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT OBJ_ALV_0140
      EXPORTING
        I_PARENT          = OBJ_CUSTOM_0140
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    CREATE OBJECT OBJ_TOOLBAR_0140
      EXPORTING
        IO_ALV_GRID = OBJ_ALV_0140.

    PERFORM EXCLUDE_TB_FUNCTIONS CHANGING WL_EXCLUDE.
    PERFORM REGISTER_F4_FOR_FIELDS.
  ENDIF.

  GT_VARIANT-REPORT    = SY-REPID.
  WL_LAYOUT-NO_ROWMARK = 'X'.
  WL_LAYOUT-STYLEFNAME = 'ESTILO'.
  WL_LAYOUT-ZEBRA      = 'X'.
  WL_STABLE-ROW        = 'X'.
  WL_STABLE-COL        = 'X'.

  SET HANDLER:
    LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR OBJ_ALV_0140,
    LCL_EVENT_HANDLER=>ON_CLICK        FOR OBJ_ALV_0140,
    LCL_EVENT_HANDLER=>ON_ONF4         FOR OBJ_ALV_0140,
    LCL_EVENT_HANDLER=>SET_TOOLBAR     FOR OBJ_ALV_0140,
    LCL_EVENT_HANDLER=>GET_UCOMM       FOR OBJ_ALV_0140,
    LCL_EVENT_HANDLER=>HANDLE_USER_COMMAND FOR OBJ_ALV_0140.

  CALL METHOD OBJ_ALV_0140->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WL_LAYOUT
      IT_TOOLBAR_EXCLUDING          = WL_EXCLUDE
      IS_VARIANT                    = GT_VARIANT
      I_SAVE                        = 'A'
    CHANGING
      IT_OUTTAB                     = GT_SAIDA_0140
      IT_FIELDCATALOG               = GT_FCAT_0140
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

  CALL METHOD OBJ_ALV_0140->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD OBJ_ALV_0140->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CALL METHOD OBJ_ALV_0140->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WL_STABLE.

ENDMODULE.                    "PBO_0140 OUTPUT
