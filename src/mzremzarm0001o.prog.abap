*----------------------------------------------------------------------*
***INCLUDE MZREMZARM0001O .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  CLEAR: it_fcode.

  IF vg_dynnr_000 IS INITIAL.
    vg_dynnr_000 = c_1000.
  ENDIF.

  CASE vg_dynnr_000.
    WHEN c_1000.
      IF it_pedidos[] IS INITIAL.
        wa_fcode = c_psromaneio.
        APPEND wa_fcode TO it_fcode.
        wa_fcode = c_psromaneie.
        APPEND wa_fcode TO it_fcode.
      ENDIF.
      SET PF-STATUS 'PFCONS' EXCLUDING it_fcode.
      SET TITLEBAR  'TLCONS'.
    WHEN c_2000.
      IF it_messagem[] IS INITIAL.
        wa_fcode = c_rologsa.
        APPEND wa_fcode TO it_fcode.
      ENDIF.
      SET PF-STATUS 'PFROMA' EXCLUDING it_fcode.
      IF vg_retorno IS INITIAL.
        SET TITLEBAR  'TLROMA'.
      ELSE.
        SET TITLEBAR  'TLROME'.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " STATUS_0001  OUTPUT
