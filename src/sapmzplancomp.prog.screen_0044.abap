
process before output.
* MODULE STATUS_0044.
*
  call subscreen: sub0041 including sy-cprog vg_dynnr_0041,
                  sub0043 including sy-cprog vg_dynnr_0043.
*                  sub0045 including sy-cprog vg_dynnr_0045.

process after input.
* MODULE USER_COMMAND_0044.

  call subscreen: sub0041,
                  sub0043.
*                  sub0045.
