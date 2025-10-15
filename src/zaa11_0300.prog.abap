*&---------------------------------------------------------------------*
*&  Include           ZAA11_0300
*&---------------------------------------------------------------------*

DATA: P_301,          "Radiobutton Plaquena S
      P_302.          "Radiobutton Plaquena N

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.

  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TL0300'.

  IF VG_1PTELA IS NOT INITIAL.

    READ TABLE IT_DADOS_IMOB INTO WA_DADOS_IMOB INDEX VG_ROW.
    CLEAR: P_301, P_302.

    IF WA_DADOS_IMOB-ZIMOB_P EQ 'S'.
      MOVE ABAP_TRUE TO P_301.
    ELSE.
      MOVE ABAP_TRUE TO P_302.
    ENDIF.

    CLEAR: WA_DADOS_IMOB, VG_1PTELA.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300_EXIT INPUT.
  CLEAR OK_CODE.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.

  DATA: WA_ZAA005_PL TYPE ZAA005.

  CASE SY-UCOMM.
    WHEN 'SALVAR'.

      READ TABLE IT_DADOS_IMOB INTO WA_DADOS_IMOB INDEX VG_ROW.

      WA_ZAA005_MOD-ANLN1   = WA_DADOS_IMOB-ANLN1.
      WA_ZAA005_MOD-ANLN2   = WA_DADOS_IMOB-ANLN2.
      WA_ZAA005_MOD-BUKRS   = WA_DADOS_IMOB-BUKRS.
      WA_ZAA005_MOD-GSBER   = WA_DADOS_IMOB-GSBER.
      WA_ZAA005_MOD-KOSTL   = WA_DADOS_IMOB-KOSTL.
      WA_ZAA005_MOD-GJAHR   = P_GJAHR-LOW.
      WA_ZAA005_MOD-ZIMOB_V = WA_DADOS_IMOB-ZIMOB_V.
      WA_ZAA005_MOD-ZIMOB_A = WA_DADOS_IMOB-ZIMOB_A.

      IF P_301 IS NOT INITIAL.
        WA_ZAA005_MOD-ZIMOB_P = 'S'.
        WA_DADOS_IMOB-ZIMOB_P = 'S'.
      ELSE.
        WA_ZAA005_MOD-ZIMOB_P = 'N'.
        WA_DADOS_IMOB-ZIMOB_P = 'N'.
      ENDIF.

      IF ( WA_DADOS_IMOB-ZIMOB_A EQ 0 OR WA_DADOS_IMOB-ZIMOB_A EQ 1 ) AND ( WA_DADOS_IMOB-USUAR_AA EQ SY-UNAME ).
        MODIFY ZAA005 FROM WA_ZAA005_MOD.
        MODIFY IT_DADOS_IMOB FROM WA_DADOS_IMOB INDEX VG_ROW.
      ENDIF.

      "MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'S'.
      CLEAR: WA_ZAA005_MOD, VG_ROW, WA_DADOS_IMOB.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
