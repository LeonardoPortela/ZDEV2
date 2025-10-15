*&---------------------------------------------------------------------*
*& FORM MONTA_ALV_TELA_0200                                            *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*
REFRESH: IT_FCAT.
PERFORM ALV_PREENCHE_CAT USING:
      'TXT_STATUS'      'Status'             '6'    ''    ' '    ''    ''  ' ' ' ' '' ''.

IF ( OBJ_CUSTOM_STATUS_BAPI IS INITIAL ).
  CREATE OBJECT OBJ_CUSTOM_STATUS_BAPI
    EXPORTING
      CONTAINER_NAME              = 'CUSTOM_STATUS_BAPI'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  CREATE OBJECT OBJ_ALV_0200
    EXPORTING
      I_PARENT          = OBJ_CUSTOM_STATUS_BAPI
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.

ENDIF.

  CALL METHOD OBJ_ALV_0200->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      IT_TOOLBAR_EXCLUDING          = GT_EXC_BUTTON
    CHANGING
      IT_OUTTAB                     = IT_STATUS_BAPIS
      IT_FIELDCATALOG               = IT_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

*  CREATE OBJECT O_TIMER
*    EXPORTING
*      PARENT = OBJ_CUSTOM_STATUS_BAPI.
*
*  DATA: O_ACTIO TYPE REF TO ZUTEIS.
*
*  CREATE OBJECT O_ACTIO.
*
*  SET HANDLER O_ACTIO->HANDLE_FINISHED FOR O_TIMER. "
*
*  O_TIMER->INTERVAL = 10.
*
*  CALL METHOD OBJ_ALV_0200->REFRESH_TABLE_DISPLAY
*    EXPORTING
*      IS_STABLE = WA_STABLE.
