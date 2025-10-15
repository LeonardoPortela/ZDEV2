FUNCTION f4if_zlesh0002.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA: tl_dynpread TYPE TABLE OF dynpread    ,
        sl_dynpread TYPE dynpread             ,
        vl_region   TYPE j_1btreg_city-region ,
        vl_country  TYPE j_1btreg_city-country,
        sl_record   TYPE seahlpres            ,
        sl_shlp     TYPE LINE OF shlp_desct   ,
        sl_inter    TYPE LINE OF ddshifaces   ,
        vl_campo1   TYPE char12               ,
        vl_campo2   TYPE char12               ,
        vl_index    TYPE syindex              .

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
*   PERFORM STEP_SELECT TABLES RECORD_TAB SHLP_TAB
*                       CHANGING SHLP CALLCONTROL RC.
*   IF RC = 0.
*     CALLCONTROL-STEP = 'DISP'.
*   ELSE.
*     CALLCONTROL-STEP = 'EXIT'.
*   ENDIF.
    EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.

*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
* This step is called, before the selected data is displayed.
* You can e.g. modify or reduce the data in RECORD_TAB
* according to the users authority.
* If you want to get the standard display dialog afterwards, you
* should not change CALLCONTROL-STEP.
* If you want to overtake the dialog on you own, you must return
* the following values in CALLCONTROL-STEP:
* - "RETURN" if one line was selected. The selected line must be
*   the only record left in RECORD_TAB. The corresponding fields of
*   this line are entered into the screen.
* - "EXIT" if the values request should be aborted
* - "PRESEL" if you want to return to the selection dialog
* Standard function modules F4UT_PARAMETER_VALUE_GET and
* F4UT_PARAMETER_RESULTS_PUT may be very helpfull in this step.
  IF callcontrol-step = 'DISP'.
*   PERFORM AUTHORITY_CHECK TABLES RECORD_TAB SHLP_TAB
*                           CHANGING SHLP CALLCONTROL.
    IF NOT record_tab[]  IS INITIAL AND
           shlp-selopt[] IS INITIAL.
      READ TABLE: shlp_tab          INTO sl_shlp  INDEX 1,
                  sl_shlp-interface INTO sl_inter INDEX 1.
      CASE sl_inter-valfield.
        WHEN 'V_CIDADE1' OR
             'V_CIDADE2' OR
             'V_CIDADE3'.
          vl_campo1 = 'V_COUNTRY'.
          vl_campo2 = 'V_REGION'.
      ENDCASE.
*     Busca País
      PERFORM z_busca_country USING vl_campo1
                           CHANGING vl_country.
*     Busca Região
      PERFORM z_busca_region USING vl_campo2
                          CHANGING vl_region.
      IF NOT vl_country IS INITIAL.
        LOOP AT record_tab INTO sl_record.
          vl_index = sy-tabix.
          IF sl_record-string+04(02) NE vl_country.
            DELETE record_tab INDEX vl_index.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF NOT vl_region IS INITIAL.
        LOOP AT record_tab INTO sl_record.
          vl_index = sy-tabix.
          IF sl_record-string+07(02) NE vl_region.
            DELETE record_tab INDEX vl_index.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFUNCTION.
