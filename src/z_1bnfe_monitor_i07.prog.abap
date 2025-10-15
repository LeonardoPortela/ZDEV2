*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I07
*&---------------------------------------------------------------------*
module exit_command_0102 input.

  case ok_code.
    when 'BACK'.
      clear ok_code.
      gf_first_display_0102 = 'R'.
      leave to screen 100.
    when 'EXIT'.
      leave program.
    when 'CANCEL'.
      leave program.
  endcase.

endmodule.                 " EXIT_COMMAND_0102  INPUT
