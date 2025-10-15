*----------------------------------------------------------------------*
***INCLUDE MZMEMORANDO_0003 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0003 OUTPUT.

  IF vg_dynnr_000 IS INITIAL.
    vg_dynnr_000  = c_5050.
  ENDIF.

  CASE vg_dynnr_000.
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
    WHEN c_5050.
      SET PF-STATUS 'PFACOME'.
      SET TITLEBAR 'TLACOME'.
    WHEN c_7000.
      SET PF-STATUS 'PFNFS' EXCLUDING it_fcode.
      SET TITLEBAR 'TLNFE'.
  ENDCASE.

ENDMODULE.                 " STATUS_0003  OUTPUT
