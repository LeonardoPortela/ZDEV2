REPORT zfir0094.

INCLUDE: zfir0094_t01,
         zfir0094_s01,
         zfir0094_cd01,
         zfir0094_ci01.

START-OF-SELECTION.

  CREATE OBJECT go_document_selector.

  CREATE OBJECT go_document_changer
    EXPORTING
      iv_document_selector = go_document_selector.

  go_document_changer->change_values( ).

  go_document_changer->save_values( ).

  MESSAGE text-m01 TYPE 'S'.
