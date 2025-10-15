
process before output.

  module cte_status_0100.

  call subscreen: sub0001 including sy-repid cte_c_0001,
                  sub0100 including sy-repid cte_dynnr_000.

process after input.

  module cte_command_0100.

  module cte_aplicativo_exit at exit-command.

  call subscreen: sub0001,
                  sub0100.
