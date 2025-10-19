FUNCTION zf4if_shlp_exit_zlesh0003.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------

  DATA: vc_tipo_chvid TYPE zlctochvid,
        vw_selopt     TYPE ddshselopt.

* EXIT immediately, if you do not want to handle this step
  IF callcontrol-step  <> 'SELONE'  AND
     callcontrol-step  <> 'SELECT'  AND
      callcontrol-step <> 'PRESEL1' AND
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
  IF callcontrol-step = 'PRESEL1'.

    GET PARAMETER ID 'ZTH' FIELD vc_tipo_chvid.

    IF NOT vc_tipo_chvid IS INITIAL.
      REFRESH shlp-selopt.
      CLEAR vw_selopt.
      vw_selopt-shlpname  = 'LANCTO_MANUAL'.
      vw_selopt-shlpfield = 'LCTOCHVID'.
      vw_selopt-sign      = 'I'.
      vw_selopt-option    = 'EQ'.
      vw_selopt-low       = vc_tipo_chvid.
      APPEND vw_selopt TO shlp-selopt.
      CLEAR vc_tipo_chvid.
      SET PARAMETER ID 'ZTH' FIELD vc_tipo_chvid.
    ENDIF.

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
    EXIT.
  ENDIF.
ENDFUNCTION.
