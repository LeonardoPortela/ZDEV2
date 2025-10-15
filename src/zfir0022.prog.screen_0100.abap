
process before output.

  module status_0100.

  call subscreen: sub_01 including sy-repid '0200',
                  sub_02 including sy-repid '0300',
                  sub_03 including sy-repid '0400'.

  module create_object.

process after input.

  call subscreen: sub_01,
                  sub_02,
                  sub_03.

  module user_command_0100.
