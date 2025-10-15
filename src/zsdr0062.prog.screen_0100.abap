PROCESS BEFORE OUTPUT.
  MODULE pbo_0100.

  CALL SUBSCREEN: sub_01 INCLUDING sy-repid '0200',
                  sub_02 INCLUDING sy-repid '0300',
                  sub_03 INCLUDING sy-repid '0400',
                  sub_04 INCLUDING sy-repid '0600'.

* CALL SUBSCREEN: sub_01 INCLUDING sy-repid gv_screen1,
*                 sub_02 INCLUDING sy-repid gv_screen2,
*                 sub_03 INCLUDING sy-repid gv_screen3,
*                 sub_04 INCLUDING sy-repid gv_screen4.

  MODULE criar_alv_0200.


PROCESS AFTER INPUT.

  CALL SUBSCREEN: sub_01,
                  sub_02,
                  sub_03,
                  sub_04.


  MODULE pai_0100.
