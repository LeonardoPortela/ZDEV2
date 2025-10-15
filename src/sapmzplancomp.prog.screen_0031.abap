
PROCESS BEFORE OUTPUT.

  MODULE STATUS_0031.
*
  CALL SUBSCREEN: SUB0034 INCLUDING SY-CPROG VG_DYNNR_30XX,
                  sub0036 including sy-cprog vg_dynnr_36xx.
                  "sub0036 including sy-cprog vg_dynnr_0036.

PROCESS AFTER INPUT.

  MODULE USER_COMMAND_0031.

  CALL SUBSCREEN: SUB0034,
                  SUB0036.
                  "SUB0035.
