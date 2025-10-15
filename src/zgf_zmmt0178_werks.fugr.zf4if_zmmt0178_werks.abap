FUNCTION zf4if_zmmt0178_werks.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCR_TAB_T
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR_T
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA: l_shselopt   LIKE ddshselopt,
* EINGEGEBENE DATEN IN DER SUCHHILFE
        l_read_tabix LIKE sy-tabix.
* ZEIGER AUF TABELLENEINTRAG

  RANGES: l_bukrs FOR t001-bukrs,
          l_name1 FOR t001w-name1,
          l_werks FOR t001w-werks,
          l_pstlz FOR t001w-pstlz,
          l_vlfkz FOR t001w-vlfkz.

  DATA: BEGIN OF l_t001k OCCURS 0,
          bukrs LIKE t001k-bukrs,
          bwkey LIKE t001w-bwkey.
  DATA: END OF l_t001k.
* BEWERTUNGSKREISE ZUM BUCHUNGSKREIS

  DATA: BEGIN OF l_t001w OCCURS 0,
          bwkey LIKE t001w-bwkey,
          werks LIKE t001w-werks,
          name1 LIKE t001w-name1,
          pstlz LIKE t001w-pstlz,
          vlfkz LIKE t001w-vlfkz.
  DATA: END   OF l_t001w.
* WERKE ZUM BUCHUNGSKREIS

  DATA: BEGIN OF l_output OCCURS 0,
          werks LIKE t001w-werks,
          name1 LIKE t001w-name1,
          pstlz LIKE t001w-pstlz,
          vlfkz LIKE t001w-vlfkz,
          bukrs LIKE t001k-bukrs.
  DATA: END   OF l_output.

  DATA: v_bukrs TYPE bukrs.
  IMPORT v_bukrs TO v_bukrs FROM MEMORY ID 'ZMM_BUKRS'.


* EXIT immediately, if you do not want to handle this step
  IF callcontrol-step <> 'SELECT'.
    callcontrol-no_maxdisp = 'X'.
* SUPRESS MAX_LINES
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP SELECT    (Select values)
*"----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard seletion, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step
  IF callcontrol-step = 'SELECT'.
    LOOP AT shlp-selopt INTO l_shselopt.
      CASE l_shselopt-shlpfield.
        WHEN 'BUKRS'.
*          MOVE-CORRESPONDING L_SHSELOPT TO L_BUKRS.
*          APPEND L_BUKRS.
        WHEN 'NAME1'.
          MOVE-CORRESPONDING l_shselopt TO l_name1.
          APPEND l_name1.
        WHEN 'WERKS'.
          MOVE-CORRESPONDING l_shselopt TO l_werks.
          APPEND l_werks.
        WHEN 'VLFKZ'.
          MOVE-CORRESPONDING l_shselopt TO l_vlfkz.
          APPEND l_vlfkz.
        WHEN 'PSTLZ'.
          MOVE-CORRESPONDING l_shselopt TO l_pstlz.
          APPEND l_pstlz.
      ENDCASE.
    ENDLOOP.

    SELECT * FROM t001k
      INTO CORRESPONDING FIELDS OF TABLE l_t001k
      WHERE bukrs = v_bukrs
      ORDER BY PRIMARY KEY.

    SELECT * FROM t001w
      INTO CORRESPONDING FIELDS OF TABLE l_t001w
      FOR ALL ENTRIES IN l_t001k
      WHERE werks IN l_werks
        AND bwkey = l_t001k-bwkey
        AND name1 IN l_name1
      ORDER BY PRIMARY KEY.

    l_read_tabix = 1.

    LOOP AT l_t001k.
      MOVE-CORRESPONDING l_t001k TO l_output.
      LOOP AT l_t001w
        FROM l_read_tabix
        WHERE bwkey = l_t001k-bwkey.
        l_read_tabix = sy-tabix.
        MOVE-CORRESPONDING l_t001w TO l_output.
        APPEND l_output.
      ENDLOOP.
    ENDLOOP.

    record_tab[] = l_output[].

    IF NOT l_output[] IS INITIAL.
      callcontrol-step = 'DISP'.
    ENDIF.
  ENDIF.

ENDFUNCTION.
