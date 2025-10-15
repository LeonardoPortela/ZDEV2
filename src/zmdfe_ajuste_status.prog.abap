*&---------------------------------------------------------------------*
*& Report ZMDFE_AJUSTE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmdfe_ajuste_status.

TABLES: zsdt0102.

CONSTANTS: c_yf_user_command   TYPE slis_formname   VALUE 'F_USER_COMMAND'.

DATA: t_0102           TYPE TABLE OF zsdt0102,
      w_0102           TYPE zsdt0102,
      w_j_1bnfdoc      TYPE j_1bnfdoc,
      w_j_1bnfe_active TYPE j_1bnfe_active,
      l_chave          TYPE zde_chave_doc_e,
      l_acckey         TYPE j_1b_nfe_access_key,
      zcl_util         TYPE REF TO zcl_util.

SELECTION-SCREEN BEGIN OF BLOCK 02 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_docnum   FOR zsdt0102-docnum,
                  s_dtemi    FOR zsdt0102-data_emi.
SELECTION-SCREEN   END OF BLOCK 02.

START-OF-SELECTION.

  PERFORM f_selecao.

  IF t_0102[] IS INITIAL.
    MESSAGE s024(sd) WITH TEXT-002.
    EXIT.
  ENDIF.

  PERFORM f_salvar.

FORM f_selecao.

  SELECT *
    FROM zsdt0102
    INTO TABLE t_0102
   WHERE docnum      IN s_docnum
     AND data_emi    IN s_dtemi
     AND contingencia = abap_true.

ENDFORM.

FORM f_salvar.

  CREATE OBJECT zcl_util.

  LOOP AT t_0102 INTO w_0102.

    CLEAR: w_j_1bnfdoc, w_j_1bnfe_active.

    SELECT SINGLE *
      INTO w_j_1bnfdoc
      FROM j_1bnfdoc
     WHERE docnum = w_0102-docnum.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      INTO w_j_1bnfe_active
      FROM j_1bnfe_active
     WHERE docnum = w_0102-docnum.

    CHECK sy-subrc = 0.

*   l_chave  = zcl_util->get_chave_nfe( w_0102-docnum ).
*   l_acckey = l_chave.
*
*   CALL FUNCTION 'J_1B_NFE_CREATE_CHECK_DIGIT'
*     CHANGING
*       c_acckey = l_acckey.
*
    w_j_1bnfdoc-conting      = abap_true.
    MODIFY j_1bnfdoc      FROM w_j_1bnfdoc.

*   IF  w_j_1bnfe_active-nfnum9 IS NOT INITIAL.
*     w_j_1bnfe_active-cdv   = l_acckey+43(1).
*   ENDIF.
    w_j_1bnfe_active-conting = abap_true.
*   w_j_1bnfe_active-tpemis  = '2'.
    MODIFY j_1bnfe_active FROM w_j_1bnfe_active.

    COMMIT WORK AND WAIT.
  ENDLOOP.

ENDFORM.
