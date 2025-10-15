*&---------------------------------------------------------------------*
*&  Include           Z_FORM_MONTA_ALV_TELA_0100
*&---------------------------------------------------------------------*

REFRESH IT_FCAT.
  PERFORM ALV_PREENCHE_CAT USING:
        'CHECK'      'Selecione'           '08'    ''    ' '    ''    'X' 'X',
        'EQUNR'      'Nº Equipamento'      '10'    ''    'X'    ''    ' ' ' ',
        'IWERK'      'Centro'              '06'    ''    ''     ''     ''  '',
        'EQKTX'      'Desc. Equipamento'   '86'    ''    ' '    ''    ' ' ' '.

  IF ( OBJ_CUSTOM_0100 IS INITIAL ).
    CREATE OBJECT OBJ_CUSTOM_0100
      EXPORTING
        CONTAINER_NAME              = 'CUSTOM_EQUI_DISPONIVEIS'
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
  ENDIF.

  "Registra os eventos a serem utilizados
  SET HANDLER:
      LCL_EVENT_HANDLER=>ON_DATA_CHANGED_CHECKBOX FOR OBJ_ALV_0100,
      LCL_EVENT_HANDLER=>SET_TOOLBAR              FOR OBJ_ALV_0100,
      LCL_EVENT_HANDLER=>GET_UCOMM                FOR OBJ_ALV_0100.
  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL       TO GT_EXC_BUTTON.

  CALL METHOD OBJ_ALV_0100->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      IT_TOOLBAR_EXCLUDING          = GT_EXC_BUTTON
    CHANGING
      IT_OUTTAB                     = IT_SAIDA
      IT_FIELDCATALOG               = IT_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

  CALL METHOD OBJ_ALV_0100->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.
