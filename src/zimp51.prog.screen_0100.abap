
process before output.
  module inicializa_tela200.
  module tab_strip_imp_active_tab_set.
  module trata_fields.
  call subscreen tab_strip_imp_sca
    including g_tab_strip_imp-prog g_tab_strip_imp-subscreen.
  module status_0100.
  module cria_objetos.

process after input.

  call subscreen tab_strip_imp_sca.
  module tab_strip_imp_active_tab_get.

  module user_command_0100.
  module user_command_exit at exit-command.

