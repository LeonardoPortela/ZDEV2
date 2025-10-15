*----------------------------------------------------------------------*
***INCLUDE ZXMRMTOP .
*----------------------------------------------------------------------*
DATA: GF_INIT,
      IT_ZPPT0002 TYPE TABLE OF ZPPT0002,
      WA_ZPPT0002 TYPE ZPPT0002,
      IT_OUTRETURN    TYPE TABLE OF ZFIE_RET_DOCUMENT,
      WA_OUTRETURN    TYPE ZFIE_RET_DOCUMENT,
      VG_OBJ_KEY      TYPE ZMMT_EE_ZGR-OBJ_KEY,
      VG_INTERFACE(2).
