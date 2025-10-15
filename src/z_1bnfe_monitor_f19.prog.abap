*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F19
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  fill_nfe_history
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GF_DOCNUM  text
*----------------------------------------------------------------------*
FORM fill_nfe_history USING p_gf_docnum TYPE j_1bnfe_active-docnum.

* Get Entries for selected NF-e from J_1BNFE_HISTORY
  REFRESH it_nfe_alv2.
  CLEAR   wa_nfe_alv2.
  SELECT * FROM j_1bnfe_history
    INTO wa_nfe_history
    WHERE docnum = p_gf_docnum.

    MOVE-CORRESPONDING wa_nfe_history TO wa_nfe_alv2.
    APPEND wa_nfe_alv2 TO it_nfe_alv2.
  ENDSELECT.

* Add Entry for selected NF-e with actual status (J_1BNFE_ACTIVE)
  CLEAR wa_nfe_alv2.
  READ TABLE it_nfe_alv INTO wa_nfe_alv                     "1090279
    WITH TABLE KEY docnum = p_gf_docnum.                    "1090279
  MOVE-CORRESPONDING wa_nfe_alv TO wa_nfe_alv2.
  APPEND wa_nfe_alv2 TO it_nfe_alv2.

* Sort ALV list
  SORT it_nfe_alv2 DESCENDING BY action_date action_time.

* Add status texts to ALV list
  LOOP AT it_nfe_alv2 INTO wa_nfe_alv2.
    index = sy-tabix.
    READ TABLE it_scs_text INTO wa_scs_text
      WITH KEY domvalue_l = wa_nfe_alv2-scssta.
    MOVE wa_scs_text-ddtext TO wa_nfe_alv2-scsstat.
    READ TABLE it_doc_text INTO wa_doc_text
      WITH KEY domvalue_l = wa_nfe_alv2-docsta.
    MOVE wa_doc_text-ddtext TO wa_nfe_alv2-docstat.
    IF NOT wa_nfe_alv2-code IS INITIAL.
      READ TABLE it_code_text INTO wa_code_text
        WITH KEY code       = wa_nfe_alv2-code.
      IF sy-subrc IS INITIAL AND wa_nfe_alv2-code NE '778'. "1149787
        MOVE wa_code_text-text TO wa_nfe_alv2-codet.
      ELSEIF NOT wa_nfe_alv2-code_description IS INITIAL. "DEVK9A1YX8 -SD - Ajuste projeto DRC ZNFE #130434 RSA
        MOVE wa_nfe_alv2-code_description TO wa_nfe_alv2-codet.
      ENDIF.                                                "1149787
    ENDIF.
    MODIFY it_nfe_alv2 FROM wa_nfe_alv2 INDEX index
      TRANSPORTING docstat scsstat codet.
  ENDLOOP.

* Update ALV distplay
  CALL METHOD ctl_alv_nfe_hist->refresh_table_display.

ENDFORM.                    " fill_nfe_history
