*&---------------------------------------------------------------------*
*&  Include           ZFI0029_PBO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
*  SET TITLEBAR ''.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.
  DATA: OBJ_TOOLBAR_0120    TYPE REF TO LCL_EVENT_TOOLBAR.

  CLEAR GT_FCAT.
  PERFORM ALV_PREENCHE_CAT USING:
  'CHECK'     ''               '3'  '' ''  '' '' 'X' 'X' ''  ''  ''  '' '',
  'STATUS'    'Status'         '6'  '' ''  '' 'X' '' ''  'X'  ''  ''  '' '',
  'BELNR'     'Documento'      '11' '' 'X' '' '' ''  ''  'X' ''  ''  '' '',
  'EBELN'     'Pedido'         '4' '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'EBELP'     'Item'           '4' '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'BLART'     'Tp Doc'         '6'  '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'STBLG'     'Doc Estorno'    '11' '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'GJAHR'     'Ano'            '6'  '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'BUDAT'     'Dt Lcto'        '10' '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'HKONT'     'Conta'          '8' '' 'X' '' '' ''  ''  ''  ''  ''  '' '',
  'BSCHL'     'Chave'          '4'  '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'UMSKZ'     'Raz. Esp'       '2'  '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'SHKZG'     'D/C'            '3'  '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'WAERS'     'Moeda'          '5'  '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'GSBER'     'Divisão'        '5'  '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'DMBTR_BRL' 'Valor R$'       ''   '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'DMBTR_USD' 'Valor US$'      ''   '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'TX_CAMBIO' 'Tx. Câmbio'     ''   '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'TX_AJUSTE' 'Tx. Ajuste'     ''   '' ''  '' '' 'X' ''  ''  'ZGLT067'  'WKURS'  'GT_SAIDA' '',
  'VLR_CALC'  'Vlr Calculado'  ''   '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'VLR_AJUS'  'Vlr Ajuste US$' '20'   '' ''  '' '' 'X' ''  ''  '' ''  '' '',
*  'CREDITO'    'Crédito'          ''   '' ''  '' '' 'X' 'X' ''  ''  ''  '',
  'KUNNR'     'Cliente'        ''   '' 'X' '' '' ''  ''  ''  ''  ''  '' '',
  'LIFNR'     'Fornecedor'     ''   '' 'X' '' '' ''  ''  ''  ''  ''  '' '',
  'KOSTL'     'Centro Custo'   '12' '' 'X' '' '' ''  ''  ''  ''  ''  '' '',
  'MATNR'     'Material'       '8'  '' 'X' '' '' ''  ''  ''  ''  ''  '' '',
  'AUFNR'     'Ordem Interna'  '13' '' 'X' '' '' ''  ''  ''  ''  ''  '' '',
  'BUZID'     'Ident. Lcto'    '11' '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'KOART'     'Tp Conta'       '8'  '' ''  '' '' ''  ''  ''  ''  ''  '' '',
  'OBJKEY'    'Chave Ref.'     '15' '' ''  '' '' ''  ''  'X' ''  ''  '' '',
  'TCODE'     'Origem'         '6'  '' ''  '' '' ''  ''  ''  ''  ''  '' ''.

  IF ( OBJ_CUSTOM_0100 IS INITIAL ).
    CREATE OBJECT OBJ_CUSTOM_0100
      EXPORTING
        CONTAINER_NAME              = 'CUSTOM_0100'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT OBJ_ALV_0100
      EXPORTING
        I_PARENT          = OBJ_CUSTOM_0100
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    CREATE OBJECT OBJ_TOOLBAR_0120
      EXPORTING
        IO_ALV_GRID = OBJ_ALV_0100.

    PERFORM EXCLUDE_TB_FUNCTIONS CHANGING WL_EXCLUDE.
    PERFORM REGISTER_F4_FOR_FIELDS.

    WL_LAYOUT-NO_ROWMARK = 'X'.
    WL_LAYOUT-ZEBRA      = 'X'.
    WL_LAYOUT-STYLEFNAME = 'ESTILO'.
  ENDIF.

  WL_STABLE-ROW        = 'X'.
  WL_STABLE-COL        = 'X'.
  WL_STABLE_AUX-ROW    = 'X'.

  SET HANDLER:
  LCL_EVENT_TOOLBAR=>SET_TOOLBAR     FOR OBJ_ALV_0100,
  LCL_EVENT_TOOLBAR=>GET_UCOMM       FOR OBJ_ALV_0100,
  LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR OBJ_ALV_0100,
  LCL_EVENT_HANDLER=>ON_CLICK        FOR OBJ_ALV_0100.

  CALL METHOD OBJ_ALV_0100->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WL_LAYOUT
      IT_TOOLBAR_EXCLUDING          = WL_EXCLUDE
      I_DEFAULT                     = 'X'
      I_SAVE                        = 'A'
    CHANGING
      IT_OUTTAB                     = GT_SAIDA
      IT_FIELDCATALOG               = GT_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

  CALL METHOD OBJ_ALV_0100->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD OBJ_ALV_0100->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
