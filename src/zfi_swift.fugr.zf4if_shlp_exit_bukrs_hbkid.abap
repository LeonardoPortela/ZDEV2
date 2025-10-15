FUNCTION ZF4IF_SHLP_EXIT_BUKRS_HBKID.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCR_TAB_T
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------
  DATA: rc       TYPE sy-subrc,
        lc_tabix TYPE sy-tabix.

  DATA: it_tbsl  TYPE TABLE OF tbsl WITH HEADER LINE,
        lit_t012 TYPE TABLE OF t012 WITH HEADER LINE.

  DATA: lva_shlp LIKE LINE OF shlp_tab.


* EXIT immediately, if you do not want to handle this step
  IF callcontrol-step <> 'SELONE' AND
     callcontrol-step <> 'SELECT' AND
     " AND SO ON
     callcontrol-step <> 'DISP'.
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP SELONE  (Select one of the elementary searchhelps)
*"----------------------------------------------------------------------
* This step is only called for collective searchhelps. It may be used
* to reduce the amount of elementary searchhelps given in SHLP_TAB.
* The compound searchhelp is given in SHLP.
* If you do not change CALLCONTROL-STEP, the next step is the
* dialog, to select one of the elementary searchhelps.
* If you want to skip this dialog, you have to return the selected
* elementary searchhelp in SHLP and to change CALLCONTROL-STEP to
* either to 'PRESEL' or to 'SELECT'.
  IF callcontrol-step = 'SELONE'.
*   PERFORM SELONE .........
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP PRESEL  (Enter selection conditions)
*"----------------------------------------------------------------------
* This step allows you, to influence the selection conditions either
* before they are displayed or in order to skip the dialog completely.
* If you want to skip the dialog, you should change CALLCONTROL-STEP
* to 'SELECT'.
* Normaly only SHLP-SELOPT should be changed in this step.
  IF callcontrol-step = 'PRESEL'.
*   PERFORM PRESEL ..........
    EXIT.
  ENDIF.
*"----------------------------------------------------------------------
* STEP SELECT    (Select values)
*"----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard seletion, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step.
* Standard function module F4UT_RESULTS_MAP may be very helpfull in this
* step.
  IF callcontrol-step = 'SELECT'.
    "PERFORM STEP_SELECT TABLES RECORD_TAB SHLP_TAB CHANGING SHLP CALLCONTROL RC.
*    IF RC = 0.
*      CALLCONTROL-STEP = 'DISP'.
*    ELSE.
*      CALLCONTROL-STEP = 'EXIT'.
*    ENDIF.
    EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.
*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
  IF callcontrol-step = 'DISP'.

    DATA lva_bukrs  TYPE t012-bukrs.

    DATA: lva_dyname LIKE d020s-prog VALUE 'ZREGISTER_DATA',
          lva_dynumb LIKE d020s-dnum VALUE '0118'.

    DATA: BEGIN OF lit_dynpfields OCCURS 3.
            INCLUDE STRUCTURE dynpread.
          DATA: END OF lit_dynpfields.

    MOVE '<FS_WA_REGISTRO_MANTER>-BUKRS' TO lit_dynpfields-fieldname.
    APPEND lit_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = lva_dyname
        dynumb               = lva_dynumb
      TABLES
        dynpfields           = lit_dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 01
        invalid_dynprofield  = 02
        invalid_dynproname   = 03
        invalid_dynpronummer = 04
        invalid_request      = 05
        no_fielddescription  = 06
        undefind_error       = 07.

    READ TABLE lit_dynpfields INDEX 1.
    IF sy-subrc EQ 0.
      lva_bukrs = lit_dynpfields-fieldvalue. "assign value from screen field to variable

      SELECT *
        FROM t012 APPENDING TABLE lit_t012
       WHERE bukrs EQ lva_bukrs.
         "AND spras EQ sy-langu.

      IF sy-subrc IS INITIAL.
        SORT lit_t012 BY bukrs hbkid.
        LOOP AT record_tab.
          lc_tabix = sy-tabix.
          READ TABLE lit_t012 WITH KEY bukrs = record_tab+03(04).
                                       "spras = record_tab+22(01) BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            DELETE record_tab INDEX lc_tabix.
          ENDIF.
        ENDLOOP.
      ELSE.
        CLEAR: record_tab[].
      ENDIF.
    ELSE.
      lva_bukrs = ' '.
    ENDIF.
  ENDIF.

ENDFUNCTION.
