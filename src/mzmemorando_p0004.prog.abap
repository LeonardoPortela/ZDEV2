*----------------------------------------------------------------------*
***INCLUDE MZMEMORANDO_P0004 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0004 OUTPUT.

  IF vg_dynnr_000 IS INITIAL.
    vg_dynnr_000  = c_8000.
    GET PARAMETER ID 'ZMEMO' FIELD t_memop.
  ENDIF.

  CASE vg_dynnr_000.
    WHEN c_8000.
      SET PF-STATUS 'PFPROTO'.
      SET TITLEBAR 'TLPROTO'.
    WHEN c_8020.
      CLEAR: it_fcode.
      IF vg_alterou_protocolo IS INITIAL.
        wa_fcode = c_fcode_save.
        APPEND wa_fcode TO it_fcode.
      ENDIF.
      SET PF-STATUS 'PFLANCP' EXCLUDING it_fcode.

      IF vg_consul_prot IS INITIAL.
        IF zdoc_memo_protoc IS INITIAL.
          SET TITLEBAR 'TLPROTON'.
        ELSE.
          SET TITLEBAR 'TLPROTOE'.
        ENDIF.
      ELSE.
        SET TITLEBAR 'TLPROTOC'.
      ENDIF.
    WHEN c_8050.
      CLEAR: it_fcode.
      IF vg_alterou_protocolo IS INITIAL.
        wa_fcode = c_fcode_save.
        APPEND wa_fcode TO it_fcode.
      ENDIF.
      SET PF-STATUS 'PFLANCP' EXCLUDING it_fcode.
      SET TITLEBAR 'TLPROTOV'.
  ENDCASE.

ENDMODULE.                 " STATUS_0004  OUTPUT
