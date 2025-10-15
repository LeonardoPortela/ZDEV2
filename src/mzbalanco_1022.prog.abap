*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1022 .
*----------------------------------------------------------------------*

DATA: GS_LAYOUT_1022  TYPE LVC_S_LAYO,
      IS_VARIANT_1022 TYPE DISVARIANT,
      ES_ROW_NO_1022  TYPE LVC_S_ROID,
      ES_ROW_INF_1022 TYPE LVC_S_ROW,
      ES_COL_INF_1022 TYPE LVC_S_COL.

*----------------------------------------------------------------------*
*       CLASS EVENT_HANDLER_1022 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS EVENT_HANDLER_1022 DEFINITION.
  PUBLIC SECTION.
    METHODS HOTSPOT_CLICK
      FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
    METHODS DOUBLE_CLICK
      FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "DEFINITION

*---------- Implementation -------------------------------------------*
CLASS EVENT_HANDLER_1022 IMPLEMENTATION.

  METHOD HOTSPOT_CLICK.
    PERFORM AJUSTA_CK_AGREGA_1022 USING E_ROW_ID-INDEX.
    ES_ROW_NO_1022  = ES_ROW_NO.
    ES_ROW_INF_1022 = E_ROW_ID.
    ES_COL_INF_1022 = E_COLUMN_ID.
    LEAVE TO SCREEN TL_1020.
  ENDMETHOD.                    "cockpit_hotspot_click_lotes

  METHOD DOUBLE_CLICK.
    PERFORM AJUSTA_CK_AGREGA_1022 USING E_ROW-INDEX.
    ES_ROW_NO_1022  = ES_ROW_NO.
    ES_ROW_INF_1022 = E_ROW.
    ES_COL_INF_1022 = E_COLUMN.
    LEAVE TO SCREEN TL_1020.
  ENDMETHOD.                    "ON_DOUBLE_CLICK

ENDCLASS.  "IMPLEMENTATION

DATA: EV_HANDLER_1022 TYPE REF TO EVENT_HANDLER_1022.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1022  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1022 OUTPUT.

  IF CONTAINER_1022 IS INITIAL.

    CREATE OBJECT CONTAINER_1022
      EXPORTING
        CONTAINER_NAME = 'ALV_1022'.

    CREATE OBJECT ALV_1022
      EXPORTING
        I_PARENT = CONTAINER_1022.

    CLEAR: GS_LAYOUT_1022.
    GS_LAYOUT_1022-ZEBRA      = ABAP_TRUE.
    GS_LAYOUT_1022-SEL_MODE   = 'A'.
    GS_LAYOUT_1022-NO_TOOLBAR = ABAP_TRUE.

    PERFORM Z_ESTRUTURA_FIELDCAT TABLES CATALOGO_1022 USING:
        'IT_1022_ALV' 'CK_AGREGA'   TEXT-050 'X' 01 04 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 'X' 'X',
        'IT_1022_ALV' 'KOKRS'       TEXT-053 ' ' 02 04 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
        'IT_1022_ALV' 'PRCTR'       TEXT-054 ' ' 03 10 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
        'IT_1022_ALV' 'LTEXT'       TEXT-055 ' ' 04 40 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' '.

    IS_VARIANT_1022-REPORT = SY-REPID.
    IS_VARIANT_1022-HANDLE = TL_1022.

    CALL METHOD ALV_1022->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_DEFAULT       = SPACE
        IS_LAYOUT       = GS_LAYOUT_1022
        IS_VARIANT      = IS_VARIANT_1022
      CHANGING
        IT_FIELDCATALOG = CATALOGO_1022
        IT_OUTTAB       = IT_1022_ALV[].

    CREATE OBJECT EV_HANDLER_1022.
    SET HANDLER EV_HANDLER_1022->HOTSPOT_CLICK FOR ALV_1022.
    SET HANDLER EV_HANDLER_1022->DOUBLE_CLICK  FOR ALV_1022.

  ENDIF.

  CALL METHOD ALV_1022->REFRESH_TABLE_DISPLAY.

  CALL METHOD ALV_1022->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_ROW_NO   = ES_ROW_NO_1022
      ES_ROW_INFO = ES_ROW_INF_1022
      ES_COL_INFO = ES_COL_INF_1022.

ENDMODULE.                 " STATUS_1022  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_CK_AGREGA_1022
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID_INDEX  text
*----------------------------------------------------------------------*
FORM AJUSTA_CK_AGREGA_1022  USING ROW_ID TYPE LVC_INDEX.

  DATA: WA_1022_ALV TYPE TY_ZGLT049LN_ALV.

  READ TABLE IT_1022_ALV INTO WA_1022_ALV INDEX ROW_ID.
  IF SY-SUBRC IS INITIAL.
    IF WA_1022_ALV-CK_AGREGA EQ ABAP_TRUE.
      WA_1022_ALV-CK_AGREGA = ABAP_FALSE.
    ELSE.
      WA_1022_ALV-CK_AGREGA = ABAP_TRUE.
    ENDIF.
    MODIFY IT_1022_ALV FROM WA_1022_ALV INDEX SY-TABIX TRANSPORTING CK_AGREGA.
  ENDIF.

ENDFORM.                    " AJUSTA_CK_AGREGA_1022
