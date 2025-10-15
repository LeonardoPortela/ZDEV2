
process before output.
  module md_create_text.
  module status_0100.
*
process after input.
  module md_exit at exit-command.
  chain.
    field pm_bukrs.
    field pm_bupla.
    module md_company.
  endchain.
  field pm_spmon module md_period.

  module md_get_text.
  module user_command_0100.
