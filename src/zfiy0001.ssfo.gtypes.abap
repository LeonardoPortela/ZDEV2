TYPES: BEGIN OF ty_curr,
kurst TYPE kurst_curr,
fcurr TYPE fcurr_curr,
tcurr TYPE tcurr_curr,
gdatu TYPE gdatu_inv ,
ukurs TYPE ukurs_curr,
ffact TYPE ffact_curr,
tfact TYPE tfact_curr,
datum TYPE datum     ,
END OF ty_curr.

TYPES tyt_curr TYPE STANDARD TABLE OF ty_curr.
