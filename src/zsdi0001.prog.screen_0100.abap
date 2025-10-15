
process before output.

  module zm_status.

  loop at t_avinc into s_avinc with control tc_avinc
    cursor tc_avinc-current_line.
*    module  z_screeen_avinc.
  endloop.

  loop at t_vinc into s_vinc with control tc_vinc
    cursor tc_vinc-current_line.
  endloop.

*  module  zm_screeen.

process after input.

  loop at t_avinc.
    field s_avinc-marc module zm_avinc_marc on request.
  endloop.

  loop at t_vinc.
    field s_vinc-marc module zm_vinc_marc on request.
  endloop.

*  chain.
*    field s_dco.
*    module set_pesq_dco on chain-request.
*  endchain.

  module zm_user_command.
