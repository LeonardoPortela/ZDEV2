*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1021 .
*----------------------------------------------------------------------*

DATA: GS_LAYOUT_1021  TYPE LVC_S_LAYO,
      IS_VARIANT_1021 TYPE DISVARIANT,
      ES_ROW_NO_1021  TYPE LVC_S_ROID,
      ES_ROW_INF_1021 TYPE LVC_S_ROW,
      ES_COL_INF_1021 TYPE LVC_S_COL.

*----------------------------------------------------------------------*
*       CLASS EVENT_HANDLER_1021 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS EVENT_HANDLER_1021 DEFINITION.
  PUBLIC SECTION.
    METHODS HOTSPOT_CLICK
      FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
    METHODS DOUBLE_CLICK
      FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "DEFINITION

*---------- Implementation -------------------------------------------*
CLASS EVENT_HANDLER_1021 IMPLEMENTATION.

  METHOD HOTSPOT_CLICK.
    PERFORM AJUSTA_CK_AGREGA_1021 USING E_ROW_ID-INDEX.
    ES_ROW_NO_1021  = ES_ROW_NO.
    ES_ROW_INF_1021 = E_ROW_ID.
    ES_COL_INF_1021 = E_COLUMN_ID.
    LEAVE TO SCREEN TL_1020.
  ENDMETHOD.                    "cockpit_hotspot_click_lotes

  METHOD DOUBLE_CLICK.
    PERFORM AJUSTA_CK_AGREGA_1021 USING E_ROW-INDEX.
    ES_ROW_NO_1021  = ES_ROW_NO.
    ES_ROW_INF_1021 = E_ROW.
    ES_COL_INF_1021 = E_COLUMN.
    LEAVE TO SCREEN TL_1020.
  ENDMETHOD.                    "ON_DOUBLE_CLICK

ENDCLASS.  "IMPLEMENTATION

DATA: EV_HANDLER_1021  TYPE REF TO EVENT_HANDLER_1021.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1021  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1021 OUTPUT.

  IF CONTAINER_1021 IS INITIAL.

    CREATE OBJECT CONTAINER_1021
      EXPORTING
        CONTAINER_NAME = 'ALV_1021'.

    CREATE OBJECT ALV_1021
      EXPORTING
        I_PARENT = CONTAINER_1021.

    CLEAR: GS_LAYOUT_1021.
    GS_LAYOUT_1021-ZEBRA      = ABAP_TRUE.
    GS_LAYOUT_1021-SEL_MODE   = 'A'.
    GS_LAYOUT_1021-NO_TOOLBAR = ABAP_TRUE.

    PERFORM Z_ESTRUTURA_FIELDCAT TABLES CATALOGO_1021 USING:
        'IT_1021_ALV' 'CK_AGREGA'   TEXT-050 'X' 01 04 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 'X' 'X',
        'IT_1021_ALV' 'KOSAR'       TEXT-051 ' ' 02 04 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
        'IT_1021_ALV' 'KTEXT'       TEXT-052 ' ' 03 40 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' '.

    IS_VARIANT_1021-REPORT = SY-REPID.
    IS_VARIANT_1021-HANDLE = TL_1021.

    CALL METHOD ALV_1021->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_DEFAULT       = SPACE
        IS_LAYOUT       = GS_LAYOUT_1021
        IS_VARIANT      = IS_VARIANT_1021
      CHANGING
        IT_FIELDCATALOG = CATALOGO_1021
        IT_OUTTAB       = IT_1021_ALV[].

    CREATE OBJECT EV_HANDLER_1021.
    SET HANDLER EV_HANDLER_1021->HOTSPOT_CLICK FOR ALV_1021.
    SET HANDLER EV_HANDLER_1021->DOUBLE_CLICK  FOR ALV_1021.

  ENDIF.

  CALL METHOD ALV_1021->REFRESH_TABLE_DISPLAY.

  CALL METHOD ALV_1021->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_ROW_NO   = ES_ROW_NO_1021
      ES_ROW_INFO = ES_ROW_INF_1021
      ES_COL_INFO = ES_COL_INF_1021.

ENDMODULE.                 " STATUS_1021  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  AJUSTA_CK_AGREGA_1021
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM AJUSTA_CK_AGREGA_1021  USING ROW_ID TYPE LVC_INDEX.

  DATA: WA_1021_ALV TYPE TY_ZGLT049CN_ALV.

  READ TABLE IT_1021_ALV INTO WA_1021_ALV INDEX ROW_ID.
  IF SY-SUBRC IS INITIAL.
    IF WA_1021_ALV-CK_AGREGA EQ ABAP_TRUE.
      WA_1021_ALV-CK_AGREGA = ABAP_FALSE.
    ELSE.
      WA_1021_ALV-CK_AGREGA = ABAP_TRUE.
    ENDIF.
    MODIFY IT_1021_ALV FROM WA_1021_ALV INDEX SY-TABIX TRANSPORTING CK_AGREGA.
  ENDIF.

ENDFORM.                    " AJUSTA_CK_AGREGA_1021
