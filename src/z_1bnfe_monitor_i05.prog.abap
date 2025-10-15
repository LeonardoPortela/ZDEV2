*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I05
*&---------------------------------------------------------------------*
module user_command_0101 input.

  case ok_code.
    when 'CAN_NO'.
      clear gf_cancel.
    when 'CAN_YES'.
      gf_cancel = c_x.
    when 'QUIT'.
      clear gf_cancel.
  endcase.

  clear ok_code.
  leave to screen 0.

endmodule.                 " user_command_0101  INPUT
