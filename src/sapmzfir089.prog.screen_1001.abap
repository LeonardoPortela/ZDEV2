
process before output.
* MODULE STATUS_1001.
*
  call subscreen: sub1101 including sy-cprog tl_1101,
                  sub1102 including sy-cprog tl_1102.

process after input.
* MODULE USER_COMMAND_1001.

  call subscreen: sub1101,
                  sub1102.
