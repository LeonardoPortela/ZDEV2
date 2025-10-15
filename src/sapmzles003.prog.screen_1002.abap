
process before output.

  module cria_itens_lote_confe.

  call subscreen: sub0006 including sy-cprog c_0006,
                  sub2001 including sy-cprog c_2001.
*
process after input.

  call subscreen: sub0006,
                  sub2001.

  module user_command_1002.
