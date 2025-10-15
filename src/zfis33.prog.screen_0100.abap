process before output.

  module status_0100.
  call subscreen subscreen_cadastro including sy-repid screen_cadastro.


process after input.

  call subscreen subscreen_cadastro.

  chain.
    field p_bukrs.
  endchain.


  field p_bukrs module atribui_info_empresa on request.


  module user_command_0100.
  module exit_screen at exit-command.
