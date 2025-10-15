*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5521
*&---------------------------------------------------------------------*

DATA:  G_CUSTOM_CONTAINER_POP_5221 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       CTL_ALV1_POP_5221           TYPE REF TO CL_GUI_ALV_GRID,
       GS_LAYOUT_POP_5221          TYPE LVC_S_LAYO,
       IT_FIELDCATALOG_POP_5221    TYPE LVC_T_FCAT.

DATA: IT_ZSDT0132_POP_5221 TYPE STANDARD TABLE OF ZSDT0132.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5621_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5521_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5621  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_5521 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5131'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5131  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5521 INPUT.

  CASE SY-UCOMM.
    WHEN 'SALVAR'.
      IF WA_HEADER_OVS-PTO_COL IS INITIAL OR
         WA_HEADER_OVS-PTO_EMB IS INITIAL OR
         WA_HEADER_OVS-PRC_FRT IS INITIAL.
        message text-075 type 'S' DISPLAY LIKE 'E'.
      ELSE.

        "gera ov de serviço
        LEAVE TO SCREEN 0.

        CLEAR: WA_HEADER_OVS.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
