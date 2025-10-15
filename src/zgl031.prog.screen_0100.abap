
process before output.
  module:status_0100,
         pbo_0100.

  call subscreen: sub_0100 including sy-repid screen_principal.

process after input.
  call subscreen sub_0100.

  module pai_0100.
