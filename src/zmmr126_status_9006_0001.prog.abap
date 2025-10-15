*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_9006.
*----------------------------------------------------------------------*

DATA: CONTAINER_9006 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9006  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9006 OUTPUT.

  SET PF-STATUS 'PF9006'.
  SET TITLEBAR 'TL9006'.

  IF CONTAINER_9006 IS INITIAL.

    CREATE OBJECT CONTAINER_9006
      EXPORTING
        CONTAINER_NAME = 'HTML'
      EXCEPTIONS
        OTHERS         = 1.

    CL_ABAP_BROWSER=>SHOW_HTML(
     EXPORTING
       HTML_STRING = HTML_PAGINA
       MODAL       = ABAP_FALSE
       FORMAT      = CL_ABAP_BROWSER=>LANDSCAPE
       SIZE        = CL_ABAP_BROWSER=>MIDDLE
       CONTAINER   = CONTAINER_9006 ).

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9006_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9006_EXIT INPUT.

  CL_ABAP_BROWSER=>CLOSE_BROWSER( ).

  IF CONTAINER_9006 IS NOT INITIAL.
    CONTAINER_9006->FREE(
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: CONTAINER_9006.

  LEAVE TO SCREEN 0.

ENDMODULE.
