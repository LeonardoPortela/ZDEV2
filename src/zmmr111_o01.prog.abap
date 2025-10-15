*&---------------------------------------------------------------------*
*&  Include           ZMMR111_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'Z001'.
  SET TITLEBAR 'Z001'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.
  LOOP AT TG_FIELDS.
    LOOP AT SCREEN.
      IF    SCREEN-NAME   EQ TG_FIELDS-CAMPO
        OR  SCREEN-GROUP1 EQ TG_FIELDS-GROUP1.

        SCREEN-INPUT     = TG_FIELDS-VALUE.
        SCREEN-INVISIBLE = TG_FIELDS-INVISIBLE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  IF G_CUSTOM_CONT_DESC IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONT_DESC
      EXPORTING
        CONTAINER_NAME = G_DESCBOX.

    IF G_CUSTOM_CONT_DESC IS NOT INITIAL.
      CREATE OBJECT OBG_DESCBOX
        EXPORTING
          PARENT            = G_CUSTOM_CONT_DESC
          WORDWRAP_MODE     = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
          WORDWRAP_POSITION = 72
          MAX_NUMBER_CHARS  = 936.

      CALL METHOD OBG_DESCBOX->SET_TOOLBAR_MODE
        EXPORTING
          TOOLBAR_MODE = '0'.

      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 1.
    ENDIF.
  ENDIF.
  "
  IF G_CUSTOM_CONT_DESCR IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONT_DESCR
      EXPORTING
        CONTAINER_NAME = G_DESCBOXR.

    IF G_CUSTOM_CONT_DESCR IS NOT INITIAL.
      CREATE OBJECT OBG_DESCBOXR
        EXPORTING
          PARENT            = G_CUSTOM_CONT_DESCR
          WORDWRAP_MODE     = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
          WORDWRAP_POSITION = 72
          MAX_NUMBER_CHARS  = 936.

      CALL METHOD OBG_DESCBOXR->SET_TOOLBAR_MODE
        EXPORTING
          TOOLBAR_MODE = '0'.

      CALL METHOD OBG_DESCBOXR->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 1.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INICIALIZA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIALIZA OUTPUT.
  IF WG_ACAO IS INITIAL.
    REFRESH TG_MSG_RET.
    CLEAR WG_MENSAGEM.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN    = '100'
        I_SHOW      = SPACE   "c_x
        I_REPID     = SY-REPID
        I_SET_FIELD = 'X_FIELD'
      IMPORTING
        E_MESSAGEM  = WG_MENSAGEM
      TABLES
        IT_MSGS     = TG_MSG_RET.
    CLEAR WG_CADMAT.
    REFRESH: TG_FIELDS.
    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR1'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0

    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR2'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0

    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR3'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR4'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.

ENDMODULE.
