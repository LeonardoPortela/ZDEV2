
process before output.
  module: pbo_0140,
          set_selected_rows.

  call subscreen: sub_0140 including sy-repid c_screen_0150.

process after input.
  call subscreen sub_0140.

  module pai_0140.
