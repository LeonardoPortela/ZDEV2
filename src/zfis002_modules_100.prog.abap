*----------------------------------------------------------------------*
***INCLUDE ZFIS002_MODULES_100 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'STAT_0100'.
  set titlebar 'TIT_0100'.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  md_exit  INPUT
*----------------------------------------------------------------------*
module md_exit input.
  case sy-ucomm .
    when 'EXIT'.  leave to screen 0.
    when 'CANC'.  leave to screen 0.
    when 'BACK'.  leave to screen 0.
  endcase.
endmodule.                 " md_exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*----------------------------------------------------------------------*
module user_command_0100 input.
  case sy-ucomm .
    when 'CALL'. perform f_call_form.
  endcase.
endmodule.                 " user_command_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  md_create_text  OUTPUT
module md_create_text output.
  PERFORM f_create_text USING  'CN_TEXT'
                                it_text
                                CHANGING wg_text.
endmodule.                 " md_create_text  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  md_get_text  INPUT
*----------------------------------------------------------------------*
module md_get_text input.
  FREE: it_lines.
  PERFORM f_get_text USING it_text
                           wg_text.
  LOOP AT it_text INTO it_lines-tdline.
    it_lines-tdformat = '*'.
    APPEND it_lines.
  ENDLOOP.

endmodule.                 " md_get_text  INPUT
*&---------------------------------------------------------------------*
*&      Module  md_company  INPUT
*----------------------------------------------------------------------*
module md_company input.
  perform f_get_bukrs.
endmodule.                 " md_company  INPUT
*&---------------------------------------------------------------------*
*&      Module  md_period  INPUT
*----------------------------------------------------------------------*
module md_period input.
  perform f_get_month.
endmodule.                 " md_period  INPUT
