*----------------------------------------------------------------------*
***INCLUDE LZSAPMZSETUP_AUXI01 .
*----------------------------------------------------------------------*
data: ok_code type sy-ucomm.

constants: ok_salvar   type sy-ucomm value 'SALVAR',
           ok_cancelar type sy-ucomm value 'CANCELAR'.

data: tx_bezei  type bezei20,
      tx_text40 type text40.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0001 input.
  if ok_code eq ok_salvar.
    if zmmt_ee_zgr_imp-cd_param is initial.
      select single max( cd_param ) into zmmt_ee_zgr_imp-cd_param from zmmt_ee_zgr_imp.
      if zmmt_ee_zgr_imp-cd_param is initial.
        zmmt_ee_zgr_imp-cd_param = 1.
      else.
        zmmt_ee_zgr_imp-cd_param = zmmt_ee_zgr_imp-cd_param + 1.
      endif.
    endif.
    modify zmmt_ee_zgr_imp.
    commit work.
    leave to screen 0.
  endif.
endmodule.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0001 output.
  set pf-status 'PF0001'.
  set titlebar 'TL0001'.

  if zmmt_ee_zgr_imp-land1 is initial.
    zmmt_ee_zgr_imp-land1 = 'BR'.
  endif.

  clear: tx_bezei, tx_text40.

  if not zmmt_ee_zgr_imp-regio is initial.
    select single bezei into tx_bezei
      from t005u
     where spras eq sy-langu
       and land1 eq zmmt_ee_zgr_imp-land1
       and bland eq zmmt_ee_zgr_imp-regio.
  endif.

  if not zmmt_ee_zgr_imp-witht is initial.
    select single text40 into tx_text40
      from t059u
     where spras eq sy-langu
       and land1 eq zmmt_ee_zgr_imp-land1
       and witht eq zmmt_ee_zgr_imp-witht.
  endif.

endmodule.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
module user_command_0001_exit input.
  leave to screen 0.
endmodule.                 " USER_COMMAND_0001_EXIT  INPUT
