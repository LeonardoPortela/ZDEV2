*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5221
*&---------------------------------------------------------------------*

DATA:  G_CUSTOM_CONTAINER_POP_5221 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       CTL_ALV1_POP_5221           TYPE REF TO CL_GUI_ALV_GRID,
       GS_LAYOUT_POP_5221          TYPE LVC_S_LAYO,
       IT_FIELDCATALOG_POP_5221    TYPE LVC_T_FCAT.

DATA: IT_ZSDT0132_POP_5221 TYPE STANDARD TABLE OF ZSDT0132.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5221_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5221_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5221  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_5221 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5221'.

  PERFORM PESQUISA_POP_5221.
  PERFORM MOSTRA_POP_5221.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5131  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5221 INPUT.

  CASE SY-UCOMM.
    WHEN 'SALVAR'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP_5221
*&---------------------------------------------------------------------*
FORM MOSTRA_POP_5221 .

  IF G_CUSTOM_CONTAINER_POP_5221 IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER_POP_5221
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER5221'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    PERFORM FILL_IT_FIELDCATALOG TABLES IT_FIELDCATALOG_POP_5221 USING:
          01 'ROT_DESC'          ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Local de Embarque'.

    GS_LAYOUT_POP_5221-SEL_MODE   = 'A'.
    GS_LAYOUT_POP_5221-CWIDTH_OPT = 'X'.

    CREATE OBJECT CTL_ALV1_POP_5221
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER_POP_5221.           "ALV Lote

    CALL METHOD CTL_ALV1_POP_5221->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT_POP_5221
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG_POP_5221
        IT_OUTTAB       = IT_ZSDT0132_POP_5221.

  ELSE.
    CALL METHOD CTL_ALV1_POP_5221->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = _STABLE.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_5221
*&---------------------------------------------------------------------*
FORM PESQUISA_POP_5221 .

  DATA: IT_ZSDT0131 TYPE STANDARD TABLE OF ZSDT0131.

  SELECT *
    FROM ZSDT0131
    INNER JOIN ZSDT0129 ON ZSDT0131~NRO_LOTE = ZSDT0129~NRO_LOTE
    INNER JOIN ZSDT0133 ON ZSDT0129~NRO_CG = ZSDT0133~NRO_CG
    INTO CORRESPONDING FIELDS OF TABLE IT_ZSDT0131
    WHERE ZSDT0133~NRO_CG EQ VG_HOTSPOT_CARGA_5220
      AND ZSDT0133~STATUS NE 'X'
      AND ZSDT0129~STATUS NE 'X'
      AND ZSDT0131~STATUS NE 'X'.

  SELECT *
    FROM ZSDT0132
    INTO TABLE IT_ZSDT0132_POP_5221
    FOR ALL ENTRIES IN IT_ZSDT0131
    WHERE NR_ROT EQ IT_ZSDT0131-COD_LOC_EMB
      AND STATUS EQ 'A'.

ENDFORM.
