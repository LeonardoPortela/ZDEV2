
process before output.

  call subscreen: sub0011 including sy-cprog vg_dynnr_0011,
                  sub0012 including sy-cprog vg_dynnr_0012.
*                  sub0003 including sy-cprog vg_dynnr_0003.

process after input.

  call subscreen: sub0011,
                  sub0012.
*                  sub0003.
