*&---------------------------------------------------------------------*
*& Report  ZLESR0069
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZLESR0069.

TABLES: ZLEST0062.

DATA: TL_NR_LOTE_ADM TYPE LXHME_RANGE_C10_T,
      TL_NR_LOTE     TYPE LXHME_RANGE_C10_T,
      TL_CHVID       TYPE LXHME_RANGE_C2_T,
      TL_NUCONTRATO  TYPE LXHME_RANGE_C12_T,
      TL_ID_TIPO     TYPE LXHME_RANGE_C1_T.


SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:   S_NR_L_A     FOR ZLEST0062-NR_LOTE_ADM,
                  S_NR_L       FOR ZLEST0062-NR_LOTE,
                  S_CHVID      FOR ZLEST0062-CHVID,
                  S_NUCONT     FOR ZLEST0062-NUCONTRATO,
                  S_ID_TIP     FOR ZLEST0062-ID_TIPO.
SELECTION-SCREEN: END OF BLOCK B1.

TL_NR_LOTE_ADM[] = S_NR_L_A[].
TL_NR_LOTE[]     = S_NR_L[].
TL_CHVID[]       = S_CHVID[].
TL_NUCONTRATO[]  = S_NUCONT[].
TL_ID_TIPO[]     = S_ID_TIP[].

START-OF-SELECTION.

  CALL FUNCTION 'Z_PFE_LOG_PROC_ARQ'
    EXPORTING
      I_NR_LOTE_ADM = TL_NR_LOTE_ADM
      I_NR_LOTE     = TL_NR_LOTE
      I_CHVID       = TL_CHVID
      I_NUCONTRATO  = TL_NUCONTRATO
      I_ID_TIPO     = TL_ID_TIPO
      I_POPUP       = '0'
      I_SEARCH      = 'X'.



*                    TABLES
*                      IT_LOG              =
