FUNCTION ZF4IF_SHLP_EXIT_LOCAL_ENTREGA.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------

* EXIT immediately, if you do not want to handle this step
  IF CALLCONTROL-STEP <> 'SELONE' AND
     CALLCONTROL-STEP <> 'SELECT' AND
     " AND SO ON
     CALLCONTROL-STEP <> 'DISP'.
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
  IF CALLCONTROL-STEP = 'SELONE'.
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
  IF CALLCONTROL-STEP = 'PRESEL'.
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
  IF CALLCONTROL-STEP = 'SELECT'.
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
  IF CALLCONTROL-STEP = 'DISP'.

    DELETE ADJACENT DUPLICATES FROM RECORD_TAB[] COMPARING ALL FIELDS.

*    DATA: lt_tnch12     TYPE TABLE OF tnch12,
*          lt_interface  TYPE ddshifaces,
*          ls_interface  TYPE ddshiface,
*          l_tarcde      TYPE nch_tarcde,
*          l_tabix       TYPE sy-tabix,
*          lt_record_tab TYPE TABLE OF seahlpres.
*
*    DATA: BEGIN OF lt_results_tab OCCURS 0,      " parallel table to
*            einri     TYPE tnch12-einri,
*            tarcde    TYPE tnch12-tarcde,     " record_tab
*          END OF lt_results_tab.
*
*    FIELD-SYMBOLS: <lt_results_tab> LIKE LINE OF lt_results_tab,
*                   <record_tab>     LIKE LINE OF record_tab,
*                   <lt_record_tab>  LIKE LINE OF lt_record_tab.
*
**   Get inactive values
*    SELECT * FROM tnch12 INTO TABLE lt_tnch12
*        WHERE inact = 'X'.
*
*    IF lt_tnch12 IS NOT INITIAL.
**     Get current field value
*      CLEAR: l_tarcde.
*      lt_interface = shlp-interface.
*      READ TABLE lt_interface INDEX 1 INTO ls_interface.
*      l_tarcde = ls_interface-value.
*
**     Get current F4 help list values
*      CALL FUNCTION 'F4UT_PARAMETER_VALUE_GET'
*        EXPORTING
*          parameter           = 'EINRI'
**         OFF_RESULT          = 0
**         LEN_RESULT          = 0
*          fieldname           = 'EINRI'
**       IMPORTING
**         VALUE               =
*        TABLES
*          shlp_tab            = shlp_tab
*          record_tab          = record_tab
**         SELOPT_TAB          =
*          results_tab         = lt_results_tab
*        CHANGING
*          shlp                = shlp
*          callcontrol         = callcontrol
*        EXCEPTIONS
*          parameter_unknown   = 1
*          OTHERS              = 2.
*
*      CALL FUNCTION 'F4UT_PARAMETER_VALUE_GET'
*        EXPORTING
*          parameter           = 'TARCDE'
**         OFF_RESULT          = 0
**         LEN_RESULT          = 0
*          fieldname           = 'TARCDE'
**       IMPORTING
**         VALUE               =
*        TABLES
*          shlp_tab            = shlp_tab
*          record_tab          = record_tab
**         SELOPT_TAB          =
*          results_tab         = lt_results_tab
*        CHANGING
*          shlp                = shlp
*          callcontrol         = callcontrol
*        EXCEPTIONS
*          parameter_unknown   = 1
*          OTHERS              = 2.
*
*      CLEAR: lt_record_tab.
**     Check on inactive values
*      LOOP AT lt_results_tab ASSIGNING <lt_results_tab>.
*        CLEAR l_tabix.
*        l_tabix = sy-tabix.
*        READ TABLE lt_tnch12 WITH KEY einri     = <lt_results_tab>-einri
*                                      tarcde    = <lt_results_tab>-tarcde
*                                      TRANSPORTING NO FIELDS.
*        IF sy-subrc = 0.      " AND <lt_results_tab>-tarcde <> l_tarcde.
*          READ TABLE record_tab INDEX l_tabix ASSIGNING <record_tab>.
*          APPEND <record_tab> TO lt_record_tab.
*        ENDIF.
*      ENDLOOP.
*
**     Remove inactive values from F4 help list XXXexcept the current valueXXX
*      LOOP AT lt_record_tab ASSIGNING <lt_record_tab>.
*        READ TABLE record_tab WITH KEY string = <lt_record_tab>-string.
*        IF sy-subrc = 0.
*          DELETE TABLE record_tab.
*        ENDIF.
*      ENDLOOP.
*
*    ENDIF.

    EXIT.
  ENDIF.
ENDFUNCTION.
