*----------------------------------------------------------------------*
***INCLUDE ZFIR0044_O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.


  IF WG_ACAO IS INITIAL.
    APPEND C_SAVE TO FCODE.
    APPEND C_DELDOC TO FCODE.
    IF XMODIF = 'X' OR WG_ACAO IS INITIAL.
      APPEND C_MODIF TO FCODE.
    ENDIF.
  ELSEIF XMODIF = 'X' .
    APPEND C_MODIF TO FCODE.
  ENDIF.

  IF  XEXISTE = 'X'.
    APPEND C_ADD TO FCODE.
  ENDIF.

  SET PF-STATUS 'Z001' EXCLUDING FCODE.
  CALL METHOD CL_GUI_CFW=>DISPATCH.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
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

  IF WG_CADCTO-COD_OPER IS NOT INITIAL AND WG_ACAO NE C_DISPLA.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'WG_CADCTO-TX_JROS_ATIVA'.
        IF 'H_S' CS WG_CADCTO-COD_OPER.
          SCREEN-INPUT     = 1.
          SCREEN-INVISIBLE = 0.
        ELSEIF WG_CADCTO-COD_OPER IS NOT INITIAL.
          CLEAR WG_CADCTO-TX_JROS_ATIVA.
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-NAME EQ 'WG_CADCTO-TX_ACIMA_IND' OR
         SCREEN-NAME EQ 'WG_CADCTO-INDEX_ATIVO' OR
         SCREEN-NAME EQ 'WG_CADCTO-TX_INDEX_ATIVO'.
        IF 'V' CS WG_CADCTO-COD_OPER.
          SCREEN-INPUT     = 1.
          SCREEN-INVISIBLE = 0.
        ELSEIF WG_CADCTO-COD_OPER IS NOT INITIAL.
          CLEAR: WG_CADCTO-TX_ACIMA_IND, WG_CADCTO-INDEX_ATIVO,WG_CADCTO-TX_INDEX_ATIVO.
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-NAME EQ 'WG_CADCTO-TX_JROS_PASSIVA'.
        IF 'V_S' CS WG_CADCTO-COD_OPER.
          SCREEN-INPUT     = 1.
          SCREEN-INVISIBLE = 0.
        ELSEIF WG_CADCTO-COD_OPER IS NOT INITIAL.
          CLEAR WG_CADCTO-TX_JROS_PASSIVA.
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-NAME EQ 'WG_CADCTO-TX_AC_INDEX_PASS' OR
         SCREEN-NAME EQ 'WG_CADCTO-INDEX_PASSIVO' OR
         SCREEN-NAME EQ 'WG_CADCTO-TX_INDEX_PASSIVO'.
        IF 'H' CS WG_CADCTO-COD_OPER.
          SCREEN-INPUT     = 1.
          SCREEN-INVISIBLE = 0.
        ELSEIF WG_CADCTO-COD_OPER IS NOT INITIAL.
          CLEAR: WG_CADCTO-TX_AC_INDEX_PASS,WG_CADCTO-INDEX_PASSIVO,WG_CADCTO-TX_INDEX_PASSIVO.
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDIF.


  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD.
  ENDIF.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIALIZA_TELA OUTPUT.
  IF WG_ACAO IS INITIAL.
    REFRESH: TG_FIELDS.

    PERFORM TRATA_CAMPOS USING SPACE
                               'GR2'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM TRATA_CAMPOS USING SPACE
                              'GR1'
                               C_1       "INPUT 1     NO INPUT 0
                               C_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.
ENDMODULE.                 " INICIALIZA_TELA  OUTPUT
