FUNCTION zmmaj007_exit.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) TYPE  DDSHF4CTRL
*"--------------------------------------------------------------------

  DATA: ls_fielddescr TYPE dfies, "loc str for shlp-fielddescr
        ls_selopt     TYPE ddshselopt.  "loc str for shlp-selopt

*Local structure for itab record_tab
  DATA: BEGIN OF ls_record.
          INCLUDE STRUCTURE seahlpres.
  DATA: END OF ls_record.

  DATA: ls_name1 TYPE name1_gp,
        ls_start TYPE string,
        ls_end   TYPE string,
        v_lifnr  TYPE lifnr.

*Internal table to store Customer Name
  DATA: BEGIN OF GT_lfa1 OCCURS 0,
          lifnr LIKE lfa1-lifnr,
          name1 LIKE lfa1-name1,
        END OF GT_lfa1.

  RANGES: lr_lifnr FOR lfa1-lifnr.  "Ranges for customer number

  CHECK callcontrol-step = 'DISP'.

*Build range for customer number
  LOOP AT shlp-selopt INTO ls_selopt WHERE shlpfield = 'LIFNR'.
    lr_lifnr-sign = ls_selopt-sign.
    lr_lifnr-option = ls_selopt-option.
    lr_lifnr-low = ls_selopt-low.
    lr_lifnr-high = ls_selopt-high.
    APPEND lr_lifnr.
    CLEAR: lr_lifnr.
  ENDLOOP.

  IF lr_lifnr[] IS INITIAL.
    FREE record_tab[].
    EXIT.
  ENDIF.

*Select Customer name
  SELECT lifnr name1
    FROM lfa1
    INTO TABLE GT_lfa1
    WHERE lifnr IN lr_lifnr.

*Modify record_tab to append Customer name to customer number
  LOOP AT record_tab INTO ls_record.
    v_lifnr = ls_record-string+23(10).
    READ TABLE GT_lfa1 WITH KEY lifnr = v_lifnr.
    IF sy-subrc = 0.
      ls_record-string+93(35) = GT_lfa1-name1.
      MODIFY record_tab FROM ls_record.
      CLEAR: ls_record,ls_start,ls_end,GT_lfa1,v_lifnr.
    ENDIF.
  ENDLOOP.

  DELETE record_tab WHERE string+23(1) EQ space.

ENDFUNCTION.
