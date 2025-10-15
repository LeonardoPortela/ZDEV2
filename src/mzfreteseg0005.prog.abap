*----------------------------------------------------------------------*
***INCLUDE MZFRETESEG0005 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0005 OUTPUT.
  SET PF-STATUS 'POPUP_TX'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0005  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0005 INPUT.

  CASE ok_code.
    WHEN 'CAN_NO'.
      CLEAR gf_cancel.
    WHEN 'CAN_YES'.
      PERFORM alterar_lancamentos.
      gf_cancel = c_x.
      LEAVE TO SCREEN 0.
    WHEN 'QUIT'.
      CLEAR gf_cancel.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.                 " USER_COMMAND_0005  INPUT

*&---------------------------------------------------------------------*
*&      Form  ALTERAR_LANCAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alterar_lancamentos .

  DATA: empty TYPE sy-datum.

  MOVE-CORRESPONDING zvalor_seg_taxa TO wa_valor_taxa.

  MODIFY zvalor_seg_taxa.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_seg_terc
    FROM zvalor_seg_terc
   WHERE dt_final EQ empty.

  LOOP AT it_seg_terc INTO wa_seg_terc.
    wa_seg_terc-vr_imp_ton = ( wa_seg_terc-vr_mer_ton / wa_valor_taxa-vr_taxa ) - wa_seg_terc-vr_mer_ton.
    wa_seg_terc-vr_tot_ton = wa_seg_terc-vr_fre_ton + wa_seg_terc-vr_mer_ton + wa_seg_terc-vr_imp_ton.
    move-corresponding wa_seg_terc to zvalor_seg_terc.
    MODIFY zvalor_seg_terc.
    COMMIT WORK.
  ENDLOOP.

ENDFORM.                    " ALTERAR_LANCAMENTOS
