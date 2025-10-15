*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1016 .
*----------------------------------------------------------------------*

DATA: GS_LAYOUT_1016  TYPE LVC_S_LAYO,
      IS_VARIANT_1016 TYPE DISVARIANT,
      ES_ROW_NO_1016  TYPE LVC_S_ROID,
      ES_ROW_INF_1016 TYPE LVC_S_ROW,
      ES_COL_INF_1016 TYPE LVC_S_COL.

*----------------------------------------------------------------------*
*       CLASS EVENT_HANDLER_1016 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS EVENT_HANDLER_1016 DEFINITION.
  PUBLIC SECTION.
    METHODS HOTSPOT_CLICK
      FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
    METHODS DOUBLE_CLICK
      FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "DEFINITION

*---------- Implementation -------------------------------------------*
CLASS EVENT_HANDLER_1016 IMPLEMENTATION.

  METHOD HOTSPOT_CLICK.
    PERFORM AJUSTA_CK_AGREGA USING E_ROW_ID-INDEX.
    ES_ROW_NO_1016  = ES_ROW_NO.
    ES_ROW_INF_1016 = E_ROW_ID.
    ES_COL_INF_1016 = E_COLUMN_ID.
    LEAVE TO SCREEN TL_1015.
  ENDMETHOD.                    "cockpit_hotspot_click_lotes

  METHOD DOUBLE_CLICK.
    PERFORM AJUSTA_CK_AGREGA USING E_ROW-INDEX.
    ES_ROW_NO_1016  = ES_ROW_NO.
    ES_ROW_INF_1016 = E_ROW.
    ES_COL_INF_1016 = E_COLUMN.
    LEAVE TO SCREEN TL_1015.
  ENDMETHOD.                    "ON_DOUBLE_CLICK

ENDCLASS.  "IMPLEMENTATION

DATA: EV_HANDLER_1016  TYPE REF TO EVENT_HANDLER_1016.

"ZGL015_DRE_EST04

*&---------------------------------------------------------------------*
*&      Module  STATUS_1016  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1016 OUTPUT.

  IF CONTAINER_1016 IS INITIAL.

    CREATE OBJECT CONTAINER_1016
      EXPORTING
        CONTAINER_NAME = 'ALV_1016'.

    CREATE OBJECT ALV_1016
      EXPORTING
        I_PARENT = CONTAINER_1016.

    CLEAR: GS_LAYOUT_1016.
    GS_LAYOUT_1016-ZEBRA      = ABAP_TRUE.
    GS_LAYOUT_1016-SEL_MODE   = 'A'.
    GS_LAYOUT_1016-NO_TOOLBAR = ABAP_TRUE.

    PERFORM Z_ESTRUTURA_FIELDCAT TABLES CATALOGO_1016 USING:
        'IT_1016_ALV' 'CK_AGREGA'   TEXT-050 'X' 01 04 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 'X' 'X',
        'IT_1016_ALV' 'KOSAR'       TEXT-051 ' ' 02 04 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
        'IT_1016_ALV' 'KTEXT'       TEXT-052 ' ' 03 40 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' '.

    IS_VARIANT_1016-REPORT = SY-REPID.
    IS_VARIANT_1016-HANDLE = TL_1016.

    CALL METHOD ALV_1016->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_DEFAULT       = SPACE
        IS_LAYOUT       = GS_LAYOUT_1016
        IS_VARIANT      = IS_VARIANT_1016
      CHANGING
        IT_FIELDCATALOG = CATALOGO_1016
        IT_OUTTAB       = IT_1016_ALV[].

    CREATE OBJECT EV_HANDLER_1016.
    SET HANDLER EV_HANDLER_1016->HOTSPOT_CLICK FOR ALV_1016.
    SET HANDLER EV_HANDLER_1016->DOUBLE_CLICK  FOR ALV_1016.

  ENDIF.

  CALL METHOD ALV_1016->REFRESH_TABLE_DISPLAY.

  CALL METHOD ALV_1016->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_ROW_NO   = ES_ROW_NO_1016
      ES_ROW_INFO = ES_ROW_INF_1016
      ES_COL_INFO = ES_COL_INF_1016.

ENDMODULE.                 " STATUS_1016  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_CK_AGREGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM AJUSTA_CK_AGREGA  USING ROW_ID TYPE LVC_INDEX.

  DATA: WA_1016_ALV TYPE TY_ZGLT049C_ALV.

  READ TABLE IT_1016_ALV INTO WA_1016_ALV INDEX ROW_ID.
  IF SY-SUBRC IS INITIAL.
    IF WA_1016_ALV-CK_AGREGA EQ ABAP_TRUE.
      WA_1016_ALV-CK_AGREGA = ABAP_FALSE.
    ELSE.
      WA_1016_ALV-CK_AGREGA = ABAP_TRUE.
    ENDIF.
    MODIFY IT_1016_ALV FROM WA_1016_ALV INDEX SY-TABIX TRANSPORTING CK_AGREGA.
  ENDIF.

ENDFORM.                    " AJUSTA_CK_AGREGA
