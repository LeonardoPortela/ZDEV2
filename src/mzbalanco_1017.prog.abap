*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1017 .
*----------------------------------------------------------------------*

DATA: GS_LAYOUT_1017  TYPE LVC_S_LAYO,
      IS_VARIANT_1017 TYPE DISVARIANT,
      ES_ROW_NO_1017  TYPE LVC_S_ROID,
      ES_ROW_INF_1017 TYPE LVC_S_ROW,
      ES_COL_INF_1017 TYPE LVC_S_COL.

*----------------------------------------------------------------------*
*       CLASS EVENT_HANDLER_1017 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS EVENT_HANDLER_1017 DEFINITION.
  PUBLIC SECTION.
    METHODS HOTSPOT_CLICK
      FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
    METHODS DOUBLE_CLICK
      FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "DEFINITION

*---------- Implementation -------------------------------------------*
CLASS EVENT_HANDLER_1017 IMPLEMENTATION.

  METHOD HOTSPOT_CLICK.
    PERFORM AJUSTA_CK_AGREGA_1017 USING E_ROW_ID-INDEX.
    ES_ROW_NO_1017  = ES_ROW_NO.
    ES_ROW_INF_1017 = E_ROW_ID.
    ES_COL_INF_1017 = E_COLUMN_ID.
    LEAVE TO SCREEN TL_1015.
  ENDMETHOD.                    "cockpit_hotspot_click_lotes

  METHOD DOUBLE_CLICK.
    PERFORM AJUSTA_CK_AGREGA_1017 USING E_ROW-INDEX.
    ES_ROW_NO_1017  = ES_ROW_NO.
    ES_ROW_INF_1017 = E_ROW.
    ES_COL_INF_1017 = E_COLUMN.
    LEAVE TO SCREEN TL_1015.
  ENDMETHOD.                    "ON_DOUBLE_CLICK

ENDCLASS.  "IMPLEMENTATION

DATA: EV_HANDLER_1017 TYPE REF TO EVENT_HANDLER_1017.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1017  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1017 OUTPUT.

  IF CONTAINER_1017 IS INITIAL.

    CREATE OBJECT CONTAINER_1017
      EXPORTING
        CONTAINER_NAME = 'ALV_1017'.

    CREATE OBJECT ALV_1017
      EXPORTING
        I_PARENT = CONTAINER_1017.

    CLEAR: GS_LAYOUT_1017.
    GS_LAYOUT_1017-ZEBRA      = ABAP_TRUE.
    GS_LAYOUT_1017-SEL_MODE   = 'A'.
    GS_LAYOUT_1017-NO_TOOLBAR = ABAP_TRUE.

    PERFORM Z_ESTRUTURA_FIELDCAT TABLES CATALOGO_1017 USING:
        'IT_1017_ALV' 'CK_AGREGA'   TEXT-050 'X' 01 04 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 'X' 'X',
        'IT_1017_ALV' 'KOKRS'       TEXT-053 ' ' 02 04 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
        'IT_1017_ALV' 'PRCTR'       TEXT-054 ' ' 03 10 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' ',
        'IT_1017_ALV' 'LTEXT'       TEXT-055 ' ' 04 40 'X' SPACE SPACE SPACE SPACE C_GRID_COLOR_C200 ' ' ' '.

    IS_VARIANT_1017-REPORT = SY-REPID.
    IS_VARIANT_1017-HANDLE = TL_1017.

    CALL METHOD ALV_1017->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_DEFAULT       = SPACE
        IS_LAYOUT       = GS_LAYOUT_1017
        IS_VARIANT      = IS_VARIANT_1017
      CHANGING
        IT_FIELDCATALOG = CATALOGO_1017
        IT_OUTTAB       = IT_1017_ALV[].

    CREATE OBJECT EV_HANDLER_1017.
    SET HANDLER EV_HANDLER_1017->HOTSPOT_CLICK FOR ALV_1017.
    SET HANDLER EV_HANDLER_1017->DOUBLE_CLICK  FOR ALV_1017.

  ENDIF.

  CALL METHOD ALV_1017->REFRESH_TABLE_DISPLAY.

  CALL METHOD ALV_1017->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_ROW_NO   = ES_ROW_NO_1017
      ES_ROW_INFO = ES_ROW_INF_1017
      ES_COL_INFO = ES_COL_INF_1017.

ENDMODULE.                 " STATUS_1017  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_CK_AGREGA_1017
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID_INDEX  text
*----------------------------------------------------------------------*
FORM AJUSTA_CK_AGREGA_1017  USING ROW_ID TYPE LVC_INDEX.

  DATA: WA_1017_ALV TYPE TY_ZGLT049L_ALV.

  READ TABLE IT_1017_ALV INTO WA_1017_ALV INDEX ROW_ID.
  IF SY-SUBRC IS INITIAL.
    IF WA_1017_ALV-CK_AGREGA EQ ABAP_TRUE.
      WA_1017_ALV-CK_AGREGA = ABAP_FALSE.
    ELSE.
      WA_1017_ALV-CK_AGREGA = ABAP_TRUE.
    ENDIF.
    MODIFY IT_1017_ALV FROM WA_1017_ALV INDEX SY-TABIX TRANSPORTING CK_AGREGA.
  ENDIF.

ENDFORM.                    " AJUSTA_CK_AGREGA_1017
