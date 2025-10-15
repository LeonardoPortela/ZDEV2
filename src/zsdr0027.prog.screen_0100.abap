process before output.
 module pbo_0100.

  call subscreen: sub_01 including sy-repid '0200',
                  sub_02 including sy-repid '0300',
                  sub_03 including sy-repid '0400',
                  sub_04 INCLUDING sy-repid '0600'.

module criar_alv_0200.


process after input.

  call subscreen: sub_01,
                  sub_02,
                  sub_03,
                  sub_04.


 module pai_0100.
