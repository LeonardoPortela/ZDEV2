*"* components of interface ZIF_SAPMVC_CONTROLLER
interface ZIF_SAPMVC_CONTROLLER
  public .


  data PROGRAM type SYREPID .
  data DYNPRO type SYDYNNR .
  data CONTROLLER_ERROR type ref to ZCX_SAPMVC .

  methods CREATE_MODEL .
  methods AT_EXIT_COMMAND
    for event AT_EXIT_COMMAND of ZIF_SAPMVC_VIEW
    importing
      !UCOMM .
  methods USER_COMMAND
    for event USER_COMMAND of ZIF_SAPMVC_VIEW
    importing
      !UCOMM .
  methods PROCESS_AFTER_INPUT
    for event PROCESS_AFTER_INPUT of ZIF_SAPMVC_VIEW
    importing
      !DYNNR
      !REPID .
  methods PROCESS_BEFORE_OUTPUT
    for event PROCESS_BEFORE_OUTPUT of ZIF_SAPMVC_VIEW
    importing
      !DYNNR
      !REPID .
  methods PF_STATUS
    for event PF_STATUS of ZIF_SAPMVC_VIEW
    importing
      !DYNNR
      !REPID .
  methods TITLEBAR
    for event TITLEBAR of ZIF_SAPMVC_VIEW
    importing
      !DYNNR
      !REPID .
  methods PROCESS_ON_VALUE_REQUEST
    for event PROCESS_ON_VALUE_REQUEST of ZIF_SAPMVC_VIEW
    importing
      !FIELD .
  methods PROCESS_ON_HELP_REQUEST
    for event PROCESS_ON_HELP_REQUEST of ZIF_SAPMVC_VIEW
    importing
      !FIELD .
  methods TABSTRIP_ACTIVE_TAB_GET
    for event TABSTRIP_ACTIVE_TAB_GET of ZIF_SAPMVC_VIEW
    importing
      !UCOMM .
  methods TABSTRIP_ACTIVE_TAB_SET
    for event TABSTRIP_ACTIVE_TAB_SET of ZIF_SAPMVC_VIEW
    importing
      !UCOMM .
  methods CALL_SCREEN
    importing
      !DYNPRO type DYNNR optional
      !STARTING_COLUMN type SCRHUSEC optional
      !STARTING_LINE type SCRHUSEL optional
      !ENDING_COLUMN type SCRHSETC optional
      !ENDING_LINE type SCRHSETL optional .
  methods SET_DYNPRO
    importing
      !REPID type SYREPID
      !DYNNR type SYDYNNR .
endinterface.
