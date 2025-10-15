*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1023 .
*----------------------------------------------------------------------*

DATA: GS_LAYOUT_1023  TYPE LVC_S_LAYO,
      IS_VARIANT_1023 TYPE DISVARIANT,
      ES_ROW_NO_1023  TYPE LVC_S_ROID,
      ES_ROW_INF_1023 TYPE LVC_S_ROW,
      ES_COL_INF_1023 TYPE LVC_S_COL.

*----------------------------------------------------------------------*
*       CLASS EVENT_HANDLER_1023 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS EVENT_HANDLER_1023 DEFINITION.
  PUBLIC SECTION.
    METHODS HOTSPOT_CLICK
      FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
    METHODS DOUBLE_CLICK
      FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "DEFINITION

*---------- Implementation -------------------------------------------*
CLASS EVENT_HANDLER_1023 IMPLEMENTATION.

  METHOD HOTSPOT_CLICK.
    PERFORM AJUSTA_CK_AGREGA_1023 USING E_ROW_ID-INDEX.
    ES_ROW_NO_1023  = ES_ROW_NO.
    ES_ROW_INF_1023 = E_ROW_ID.
    ES_COL_INF_1023 = E_COLUMN_ID.
    LEAVE TO SCREEN TL_1020.
  ENDMETHOD.                    "cockpit_hotspot_click_lotes

  METHOD DOUBLE_CLICK.
    PERFORM AJUSTA_CK_AGREGA_1023 USING E_ROW-INDEX.
    ES_ROW_NO_1023  = ES_ROW_NO.
    ES_ROW_INF_1023 = E_ROW.
    ES_COL_INF_1023 = E_COLUMN.
    LEAVE TO SCREEN TL_1020.
  ENDMETHOD.                    "ON_DOUBLE_CLICK

ENDCLASS.  "IMPLEMENTATION

DATA: EV_HANDLER_1023 TYPE REF TO EVENT_HANDLER_1023.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1023  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1023 OUTPUT.

  IF CONTAINER_1023 IS INITIAL.

    CREATE OBJECT CONTAINER_1023
      EXPORTING
        CONTAINER_NAME = 'ALV_1023'.

    CREATE OBJECT ALV_1023
      EXPORTING
        I_PARENT = CONTAINER_1023.

    CLEAR: GS_LAYOUT_1023.
    GS_LAYOUT_1023-ZEBRA      = ABAP_TRUE.
    GS_LAYOUT_1023-SEL_MODE   = 'A'.
    GS_LAYOUT_1023-NO_TOOLBAR = ABAP_TRUE.

    PERFORM Z_ESTRUTURA_FIELDCAT TABLES CATALOGO_1023 USING:
        'IT_1023_ALV' 'CK_AGREGA'   TEXT-050 'X' 01 04 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 'X' 'X',
        'IT_1023_ALV' 'MATKL'       TEXT-056 ' ' 02 09 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
        'IT_1023_ALV' 'WGBEZ60'     TEXT-057 ' ' 03 50 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' '.

    IS_VARIANT_1023-REPORT = SY-REPID.
    IS_VARIANT_1023-HANDLE = TL_1023.

    CALL METHOD ALV_1023->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_DEFAULT       = SPACE
        IS_LAYOUT       = GS_LAYOUT_1023
        IS_VARIANT      = IS_VARIANT_1023
      CHANGING
        IT_FIELDCATALOG = CATALOGO_1023
        IT_OUTTAB       = IT_1023_ALV[].

    CREATE OBJECT EV_HANDLER_1023.
    SET HANDLER EV_HANDLER_1023->HOTSPOT_CLICK FOR ALV_1023.
    SET HANDLER EV_HANDLER_1023->DOUBLE_CLICK  FOR ALV_1023.

  ENDIF.

  CALL METHOD ALV_1023->REFRESH_TABLE_DISPLAY.

  CALL METHOD ALV_1023->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_ROW_NO   = ES_ROW_NO_1023
      ES_ROW_INFO = ES_ROW_INF_1023
      ES_COL_INFO = ES_COL_INF_1023.

ENDMODULE.                 " STATUS_1023  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_CK_AGREGA_1023
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID_INDEX  text
*----------------------------------------------------------------------*
FORM AJUSTA_CK_AGREGA_1023  USING ROW_ID TYPE LVC_INDEX.

  DATA: WA_1023_ALV TYPE TY_ZGLT049MN_ALV.

  READ TABLE IT_1023_ALV INTO WA_1023_ALV INDEX ROW_ID.
  IF SY-SUBRC IS INITIAL.
    IF WA_1023_ALV-CK_AGREGA EQ ABAP_TRUE.
      WA_1023_ALV-CK_AGREGA = ABAP_FALSE.
    ELSE.
      WA_1023_ALV-CK_AGREGA = ABAP_TRUE.
    ENDIF.
    MODIFY IT_1023_ALV FROM WA_1023_ALV INDEX SY-TABIX TRANSPORTING CK_AGREGA.
  ENDIF.

ENDFORM.                    " AJUSTA_CK_AGREGA_1023
