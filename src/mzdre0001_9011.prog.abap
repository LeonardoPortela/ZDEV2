*----------------------------------------------------------------------*
***INCLUDE MZDRE0001_9011.
*----------------------------------------------------------------------*

DATA: CONTAINER_9011  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      ALV_9011        TYPE REF TO CL_GUI_ALV_GRID,
      CATALOGO_9011   TYPE LVC_T_FCAT,
      LAYOUT_9011     TYPE LVC_S_LAYO,
      SCROLL_COL_9011 TYPE LVC_S_COL,
      SCROLL_ROW_9011 TYPE LVC_S_ROID.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9011  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9011 OUTPUT.

  DATA: TEXT_E001_9011 TYPE C LENGTH 50 VALUE 'Nível',
        TEXT_E002_9011 TYPE C LENGTH 50 VALUE 'Conta',
        TEXT_E003_9011 TYPE C LENGTH 50 VALUE 'Área de contabilidade de custos',
        TEXT_E004_9011 TYPE C LENGTH 50 VALUE 'Centro Lucro',
        TEXT_E005_9011 TYPE C LENGTH 50 VALUE 'Tipo de centro de custo',
        TEXT_E006_9011 TYPE C LENGTH 50 VALUE 'Grupo de mercadorias'.

  SET PF-STATUS 'PF9011'.
  SET TITLEBAR 'TL9011'.

  IF CONTAINER_9011 IS INITIAL.

    CREATE OBJECT CONTAINER_9011
      EXPORTING
        CONTAINER_NAME = 'ALV_9011'.

    CREATE OBJECT ALV_9011
      EXPORTING
        I_PARENT = CONTAINER_9011.

    CLEAR: CATALOGO_9011, CATALOGO_9011[].

    PERFORM Z_ESTRUTURA_FIELDCAT TABLES CATALOGO_9011 USING:
        'IT_MOTRA_ERRO' 'NIVEL'  TEXT_E001_9011 ' ' 01 10 SPACE SPACE SPACE SPACE SPACE SPACE SPACE,
        'IT_MOTRA_ERRO' 'SAKNR'  TEXT_E002_9011 ' ' 02 40 SPACE SPACE SPACE SPACE SPACE SPACE SPACE,
        'IT_MOTRA_ERRO' 'KOKRS'  TEXT_E003_9011 ' ' 03 06 SPACE SPACE SPACE SPACE SPACE SPACE SPACE,
        'IT_MOTRA_ERRO' 'PRCTR'  TEXT_E004_9011 ' ' 04 10 SPACE SPACE SPACE SPACE SPACE SPACE SPACE,
        'IT_MOTRA_ERRO' 'KOSAR'  TEXT_E004_9011 ' ' 05 04 SPACE SPACE SPACE SPACE SPACE SPACE SPACE,
        'IT_MOTRA_ERRO' 'MATKL'  TEXT_E004_9011 ' ' 06 10 SPACE SPACE SPACE SPACE SPACE SPACE SPACE.

    CLEAR: LAYOUT_9011.
    LAYOUT_9011-ZEBRA    = ABAP_TRUE.

    CALL METHOD ALV_9011->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_DEFAULT       = SPACE
        IS_LAYOUT       = LAYOUT_9011
      CHANGING
        IT_FIELDCATALOG = CATALOGO_9011
        IT_OUTTAB       = IT_MOTRA_ERRO[].

  ENDIF.

  CALL METHOD ALV_9011->REFRESH_TABLE_DISPLAY.

  CALL METHOD ALV_9011->SET_SCROLL_INFO_VIA_ID
    EXPORTING
      IS_COL_INFO = SCROLL_COL_9011
      IS_ROW_NO   = SCROLL_ROW_9011.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9011_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9011_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
