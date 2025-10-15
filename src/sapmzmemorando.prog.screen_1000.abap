
process before output.

  module status_1000.
*
  call subscreen: sub1001 including sy-cprog vg_dynnr_flt,
                  sub1002 including sy-cprog vg_dynnr_res.

process after input.

  call subscreen: sub1001,
                  sub1002.

  module user_command_1000.
