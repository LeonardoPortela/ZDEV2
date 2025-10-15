*&---------------------------------------------------------------------*
*&  Include           ZPPR0011_PBO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'T001'.
  SET TITLEBAR 'T002'.

  IF WA_CONT IS INITIAL.
    CREATE OBJECT WA_CONT
      EXPORTING
        CONTAINER_NAME              = 'T_0100'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.


    PERFORM Z_FIELDCAT.

    CREATE OBJECT WA_ALV
      EXPORTING
        I_PARENT          = WA_CONT
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    SET HANDLER: OBJ_EVEN->HANDLE_DOUBLE_CLICK FOR WA_ALV.

    CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT
      CHANGING
        IT_OUTTAB                     = IT_S_REPORT
        IT_FIELDCATALOG               = IT_FCAT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    IF SY-SUBRC NE 0 .
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DESCRICAO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
