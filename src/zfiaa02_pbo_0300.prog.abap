*&---------------------------------------------------------------------*
*&  Include           ZFIAA02_PBO_0300
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS '0300'.
  SET TITLEBAR  '0300'.

ENDMODULE.                 " STATUS_0300  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0300 OUTPUT.
  CLEAR: GT_FCAT_0300.

  PERFORM ALV_PREENCHE_CAT USING:
  'STATUS'   ''                         '2'   ''  '' '' '' '' 'X' '' '' '' '' '' '',
  'MSG_ERRO' 'Mensagens de Erro'        '70'  ''  '' '' '' '' ''  '' '' '' '' '' ''.

  IF ( OBJ_CUSTOM_0300 IS INITIAL ).
    CREATE OBJECT OBJ_CUSTOM_0300
      EXPORTING
        CONTAINER_NAME              = 'CUSTOM_0300'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT OBJ_ALV_0300
      EXPORTING
        I_PARENT          = OBJ_CUSTOM_0300
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    WL_LAYOUT-ZEBRA      = 'X'.
  ENDIF.

  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL TO WL_EXCLUDE.

  SET HANDLER:
    LCL_EVENT_HANDLER=>SET_TOOLBAR     FOR OBJ_ALV_0300,
    LCL_EVENT_HANDLER=>GET_UCOMM       FOR OBJ_ALV_0300.

  CALL METHOD OBJ_ALV_0300->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WL_LAYOUT
      IT_TOOLBAR_EXCLUDING          = WL_EXCLUDE
      IS_VARIANT                    = GT_VARIANT
      I_SAVE                        = 'A'
    CHANGING
      IT_OUTTAB                     = GT_SAIDA_0300
      IT_FIELDCATALOG               = GT_FCAT_0300
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
ENDMODULE.                 " PBO_0300  OUTPUT
