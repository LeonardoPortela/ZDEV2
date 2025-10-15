*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_9005.
*----------------------------------------------------------------------*

DATA: HTML_CONTROL     TYPE REF TO CL_GUI_HTML_VIEWER,
      SPLITTER_9005    TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CCCONTAINER_9005 TYPE REF TO CL_GUI_CONTAINER.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9005 OUTPUT.

  DATA: WA_REPORT           TYPE ZBIT0003,
        I_OTHERS_PARAMITERS TYPE STRING.

  SET PF-STATUS 'PF9005'.

  IF SPLITTER_9005 IS INITIAL.
    CREATE OBJECT SPLITTER_9005
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0 "CTL_CCCONTAINER
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD SPLITTER_9005->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CCCONTAINER_9005.

    CASE NM_REPORT.
      WHEN 'MM_0001'.
        CONCATENATE '?lsSEmpresa=' PEMPRE '?lsSFilial=' PFILIA '?lsSSafra=' PSAFRA INTO I_OTHERS_PARAMITERS.
        CASE NM_ATUAL.
          WHEN ABAP_FALSE.
            CONCATENATE I_OTHERS_PARAMITERS '?lsSDtMovimento=' SPACE INTO I_OTHERS_PARAMITERS.
          WHEN ABAP_TRUE.
            CONCATENATE I_OTHERS_PARAMITERS '?lsSDtMovimento=' SY-DATLO INTO I_OTHERS_PARAMITERS.
        ENDCASE.
    ENDCASE.

    CALL FUNCTION 'ZBI_CHAMA_REPORT'
      EXPORTING
        I_NM_TECNICO        = NM_REPORT
        I_RETORNAR_URI      = ABAP_TRUE
        I_OTHERS_PARAMITERS = I_OTHERS_PARAMITERS
        I_TYPE_REPORT       = '2'
        I_REFRESH           = ABAP_TRUE
      IMPORTING
        E_URI               = E_URI
        E_REPORT            = WA_REPORT
      EXCEPTIONS
        ERRO                = 1
        OTHERS              = 2.

    IF E_URI IS NOT INITIAL.
      CREATE OBJECT HTML_CONTROL
        EXPORTING
          PARENT = CCCONTAINER_9005.

      MOVE E_URI TO C_URI.

      CALL METHOD HTML_CONTROL->SHOW_URL
        EXPORTING
          URL = C_URI.
    ENDIF.

    CLEAR:NM_REPORT, NM_ATUAL.

  ENDIF.

  SET TITLEBAR 'TL9005' WITH WA_REPORT-DS_REPORT.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9005_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9005_EXIT INPUT.

  IF HTML_CONTROL IS NOT INITIAL.
    HTML_CONTROL->FREE( ).
  ENDIF.
  CLEAR: HTML_CONTROL.

  IF CCCONTAINER_9005 IS NOT INITIAL.
    CCCONTAINER_9005->FREE( ).
  ENDIF.
  CLEAR: CCCONTAINER_9005.

  IF SPLITTER_9005 IS NOT INITIAL.
    SPLITTER_9005->FREE( ).
  ENDIF.
  CLEAR: SPLITTER_9005.

  LEAVE TO SCREEN 0001.
ENDMODULE.
