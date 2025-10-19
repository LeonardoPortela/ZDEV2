*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5322
*&---------------------------------------------------------------------*

DATA:  G_CUSTOM_CONTAINER_POP_5322 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       CTL_ALV1_POP_5322           TYPE REF TO CL_GUI_ALV_GRID,
       GS_LAYOUT_POP_5322          TYPE LVC_S_LAYO,
       IT_FIELDCATALOG_POP_5322    TYPE LVC_T_FCAT.

DATA: IT_ZSDT0062_5321 TYPE STANDARD TABLE OF ZSDT0062.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5322_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5322_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5321  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_5322 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5321'.

  PERFORM PESQUISA_POP_5322.
  PERFORM MOSTRA_POP_5322.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5322  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5322 INPUT.

  CASE SY-UCOMM.
    WHEN 'SALVAR'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP_5321
*&---------------------------------------------------------------------*
FORM MOSTRA_POP_5322 .

  IF G_CUSTOM_CONTAINER_POP_5322 IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER_POP_5322
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER5322'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    PERFORM FILL_IT_FIELDCATALOG TABLES IT_FIELDCATALOG_POP_5322 USING:
          01 'NRO_CG'          ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Carga',
          02 'VBELN'           ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Ordem',
          03 'POSNR'           ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Item Ordem',
          04 'EBELN'           ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Pedido',
          05 'EBELP'           ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Item Pedido'.

    GS_LAYOUT_POP_5322-SEL_MODE   = 'A'.
    GS_LAYOUT_POP_5322-CWIDTH_OPT = 'X'.

    CREATE OBJECT CTL_ALV1_POP_5322
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER_POP_5322.           "ALV Lote

    CALL METHOD CTL_ALV1_POP_5322->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT_POP_5322
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG_POP_5322
        IT_OUTTAB       = IT_ZSDT0062_5321.

  ELSE.
    CALL METHOD CTL_ALV1_POP_5322->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = _STABLE.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_5321
*&---------------------------------------------------------------------*
FORM PESQUISA_POP_5322.

  CLEAR: IT_ZSDT0062_5321.

  SELECT *
    FROM ZSDT0062
    INTO TABLE IT_ZSDT0062_5321
    FOR ALL ENTRIES IN IT_ORDEM_5320
    WHERE NRO_CG EQ IT_ORDEM_5320-NRO_CG
      AND VBELN  EQ IT_ORDEM_5320-VBELN
      AND POSNR  EQ IT_ORDEM_5320-POSNR.

ENDFORM.
