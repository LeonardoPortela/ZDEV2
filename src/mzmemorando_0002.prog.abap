*----------------------------------------------------------------------*
***INCLUDE MZMEMORANDO_0002 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       PAI da Tela de Acompanhamento
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.

  IF vg_dynnr_000 IS INITIAL.
    vg_dynnr_000  = c_5000.
  ENDIF.

  CASE vg_dynnr_000.
    WHEN c_5000.
      SET PF-STATUS 'PFACOMP'.
      SET TITLEBAR 'TLACOMP'.
    WHEN c_2000.
      CLEAR: it_fcode.
      IF vg_consul_memo IS INITIAL.
        IF zdoc_memorando IS INITIAL.
          SET TITLEBAR 'TLLANC'.
        ELSE.
          SET TITLEBAR 'TLALTE'.
        ENDIF.
        wa_fcode = c_fcode_csnfms.
        APPEND wa_fcode TO it_fcode.
        wa_fcode = c_fcode_csnfmm.
        APPEND wa_fcode TO it_fcode.
      ELSE.
        SET TITLEBAR 'TLCONS'.
      ENDIF.
      IF zdoc_memo_nf_exp-proprio IS INITIAL.
        wa_fcode = c_fcode_nfpropria.
        APPEND wa_fcode TO it_fcode.
      ENDIF.
      IF vg_alterou_memorando IS INITIAL.
        wa_fcode = c_fcode_save.
        APPEND wa_fcode TO it_fcode.
      ENDIF.
      SET PF-STATUS 'PFLANC' EXCLUDING it_fcode.
    WHEN c_4000.
      CLEAR: it_fcode.
      IF vg_alterou_notas IS INITIAL.
        wa_fcode = c_fcode_save.
        APPEND wa_fcode TO it_fcode.
      ENDIF.
      SET PF-STATUS 'PFVINC' EXCLUDING it_fcode.
      SET TITLEBAR 'TLVINC'.
    WHEN c_6000.
      CLEAR: it_fcode.
      IF vg_alterou_notas IS INITIAL.
        wa_fcode = c_fcode_save.
        APPEND wa_fcode TO it_fcode.
      ENDIF.
      SET PF-STATUS 'PFVINCS' EXCLUDING it_fcode.
      SET TITLEBAR 'TLVINCS'.
    WHEN c_7000.
      SET PF-STATUS 'PFNFS' EXCLUDING it_fcode.
      SET TITLEBAR 'TLNFS'.
  ENDCASE.

ENDMODULE.                 " STATUS_0002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_APLICATIVO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_aplicativo INPUT.

  CASE ok_code.
    WHEN c_back OR c_exit OR c_cancel.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " EXIT_APLICATIVO  INPUT
