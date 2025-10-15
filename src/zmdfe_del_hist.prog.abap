*&---------------------------------------------------------------------*
*& Report ZMDFE_AJUSTE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmdfe_del_hist.

TABLES: zsdt0102.


DATA: t_0102           TYPE TABLE OF zsdt0102,
      w_0102           TYPE zsdt0102,
      w_j_1bnfdoc      TYPE j_1bnfdoc,
      w_j_1bnfe_active TYPE j_1bnfe_active,
      l_chave          TYPE zde_chave_doc_e,
      l_acckey         TYPE j_1b_nfe_access_key,
      zcl_util         TYPE REF TO zcl_util.

SELECTION-SCREEN BEGIN OF BLOCK 02 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_docnum   FOR zsdt0102-docnum.
SELECTION-SCREEN   END OF BLOCK 02.

START-OF-SELECTION.

  PERFORM f_del_hist.

  IF t_0102[] IS INITIAL.
    MESSAGE s024(sd) WITH TEXT-002.
    EXIT.
  ENDIF.



FORM f_del_hist.

DELETE FROM J_1BNFE_HISTORY
WHERE  DOCNUM IN s_docnum.

ENDFORM.
