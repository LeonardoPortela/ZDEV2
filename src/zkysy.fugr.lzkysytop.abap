FUNCTION-POOL zkysy.                    "MESSAGE-ID ..
*--------- GENERAL DEFINITIONS ----------------------------------------*


DATA: hline          LIKE sy-tabix,
      ok_code        LIKE sy-ucomm,
      title_text     TYPE char80,
      p_tamanho_area TYPE int4.


*--------- POPUP_WITH_TABLE_DISPLAY -----------------------------------*

INCLUDE lkysyd01.


*--------- RKC_TRANSLATE_TEXTS ----------------------------------------*

INCLUDE lkysyd02.

DATA: BEGIN OF wa_listtab OCCURS 1,
        field(200),
      END OF wa_listtab.
