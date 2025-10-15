*----------------------------------------------------------------------*
***INCLUDE MZDRE0001_9010 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9010 output.

  set pf-status 'PF9003'.
  set titlebar 'TL9010'.

  clear: wa_zgl015_dre_est02-nitxt.

  if not wa_zgl015_dre_est02-nivel is initial.
    select single nitxt into wa_zgl015_dre_est02-nitxt
      from zgl015_dre_est02
     where bukrs eq wa_zgl015_dre_est02-bukrs
       and versn eq wa_zgl015_dre_est02-versn
       and nivel eq wa_zgl015_dre_est02-nivel.
  endif.

endmodule.                 " STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9010_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9010_exit input.
  leave to screen 0.
endmodule.                 " USER_COMMAND_9010_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9010 input.

  data: vg_cga type zgl015_dre_est07.

  if ok_code_9010 eq c_conf.

    select single * into vg_cga
      from zgl015_dre_est07
     where bukrs      eq zgl015_dre_est02-bukrs
       and versn      eq zgl015_dre_est02-versn
       and nivel      eq zgl015_dre_est02-nivel
       and nivel_agpd eq wa_zgl015_dre_est02-nivel.

    if sy-subrc is initial.
      message 'Nível Agrupado já cadastrado!' type 'E'.
    endif.
    clear: zgl015_dre_est07.
    zgl015_dre_est07-bukrs      = zgl015_dre_est02-bukrs.
    zgl015_dre_est07-versn      = zgl015_dre_est02-versn.
    zgl015_dre_est07-nivel      = zgl015_dre_est02-nivel.
    zgl015_dre_est07-nivel_agpd = wa_zgl015_dre_est02-nivel.
    modify zgl015_dre_est07.

    prim_dre_nivel_re = c_x.
    move-corresponding zgl015_dre_est07 to zgl015_dre_est02.
    call method g_tree->delete_all_nodes.
    leave to screen 0.
  endif.

endmodule.                 " USER_COMMAND_9010  INPUT
