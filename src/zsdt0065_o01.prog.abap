*----------------------------------------------------------------------*
***INCLUDE ZSDT0065_O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.

  REFRESH: FCODE.

  SET PF-STATUS 'Z001' EXCLUDING FCODE.
  CALL METHOD CL_GUI_CFW=>DISPATCH.
  SET TITLEBAR 'Z001'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.

  IF G_CUSTOM_INSTR IS INITIAL.
    CREATE OBJECT G_CUSTOM_INSTR
      EXPORTING
        CONTAINER_NAME = G_DESCBOX.

    IF G_CUSTOM_INSTR IS NOT INITIAL.
      CREATE OBJECT OBG_DESCBOX
        EXPORTING
          PARENT            = G_CUSTOM_INSTR
          WORDWRAP_MODE     = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
          WORDWRAP_POSITION = 79
          MAX_NUMBER_CHARS  = 450.

      CALL METHOD OBG_DESCBOX->SET_TOOLBAR_MODE
        EXPORTING
          TOOLBAR_MODE = '0'.

      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 1.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.
  LOOP AT TG_FIELDS.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ TG_FIELDS-CAMPO
      OR SCREEN-GROUP1 EQ TG_FIELDS-GROUP1.
        SCREEN-INPUT     = TG_FIELDS-VALUE.
        SCREEN-INVISIBLE = TG_FIELDS-INVISIBLE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD."'WG_DESC_OPERACAO'.
  ENDIF.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_TELA100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIALIZA_TELA100 OUTPUT.
  IF WG_ACAO IS INITIAL.
    REFRESH: TG_FIELDS.
    PERFORM TRATA_CAMPOS USING SPACE
                              'GR1'
                               C_0       "INPUT 1     NO INPUT 0
                               C_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.
ENDMODULE.                 " INICIALIZA_TELA100  OUTPUT
