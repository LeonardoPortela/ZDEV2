*&---------------------------------------------------------------------*
*&  Include           ZPM_APONT_MEDIC_EQUIPAM_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CLEAR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CLEAR OUTPUT.
*
  IF SY-UCOMM EQ LC_SAVEP.
    REFRESH IT_REG.
    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN 'MHIO-WARPL'.
          SCREEN-INPUT = 0.
        WHEN 'MHIO-QMNUM'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
    CLEAR: MHIO-WARPL, MHIO-QMNUM, V_QMNUM,
           V_EQUNR, V_DESCRSAP, V_TIPOBJ.
    SET PARAMETER ID: 'MPL' FIELD MHIO-WARPL,
                'IQM' FIELD MHIO-QMNUM.

    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN 'MHIO-WARPL'.
          SCREEN-INPUT = 1.
        WHEN 'MHIO-QMNUM'.
          SCREEN-INPUT = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF SY-UCOMM EQ LC_CRIA.
    IF MHIO-QMNUM IS INITIAL.
      EXIT.
    ENDIF.
    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN 'MHIO-WARPL'.
          SCREEN-INPUT = 0.
        WHEN 'MHIO-QMNUM'.
          SCREEN-INPUT = 0.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
    SET SCREEN 100.
  ELSEIF SY-UCOMM EQ 'BTN_CANCELAR'.

    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN 'MHIO-WARPL'.
          SCREEN-INPUT = 0.
        WHEN 'MHIO-QMNUM'.
          SCREEN-INPUT = 0.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
* Limpar campos.
    REFRESH: IT_REG, IT_NOTAS.
    CLEAR: MHIO-WARPL, MHIO-QMNUM, V_QMNUM,
           V_EQUNR, V_DESCRSAP, V_TIPOBJ.

    SET SCREEN 100.

    CLEAR: MHIO-WARPL, MHIO-QMNUM, V_QMNUM,
       V_EQUNR, V_DESCRSAP, V_TIPOBJ.

    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN 'MHIO-WARPL'.
          SCREEN-INPUT = 1.
        WHEN 'MHIO-QMNUM'.
          SCREEN-INPUT = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDMODULE.
