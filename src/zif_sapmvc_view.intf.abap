*"* components of interface ZIF_SAPMVC_VIEW
interface ZIF_SAPMVC_VIEW
  public .


  events USER_COMMAND
    exporting
      value(UCOMM) type SYUCOMM .
  events PROCESS_AFTER_INPUT
    exporting
      value(DYNNR) type SYDYNNR
      value(REPID) type SYREPID .
  events PROCESS_BEFORE_OUTPUT
    exporting
      value(DYNNR) type SYDYNNR
      value(REPID) type SYREPID .
  events PF_STATUS
    exporting
      value(DYNNR) type SYDYNNR
      value(REPID) type SYREPID .
  events TITLEBAR
    exporting
      value(DYNNR) type SYDYNNR
      value(REPID) type SYREPID .
  events PROCESS_ON_VALUE_REQUEST
    exporting
      value(FIELD) type DYNFNAM .
  events PROCESS_ON_HELP_REQUEST
    exporting
      value(FIELD) type DYNFNAM .
  events TABSTRIP_ACTIVE_TAB_GET
    exporting
      value(UCOMM) type SYUCOMM .
  events TABSTRIP_ACTIVE_TAB_SET
    exporting
      value(UCOMM) type SYUCOMM .
  events AT_EXIT_COMMAND
    exporting
      value(UCOMM) type SYUCOMM .

  methods TRIGGER_USER_COMMAND
    importing
      !UCOMM type SYUCOMM .
  methods TRIGGER_PAI .
  methods TRIGGER_PBO .
  methods TRIGGER_STATUS .
  methods TRIGGER_TITLEBAR .
  methods TRIGGER_POV
    importing
      !FIELD type DYNFNAM .
  methods TRIGGER_POH
    importing
      !FIELD type DYNFNAM .
  methods TRIGGER_TABSTRIP_ACTIVE_GET
    importing
      !UCOMM type SYUCOMM .
  methods TRIGGER_TABSTRIP_ACTIVE_SET
    importing
      !UCOMM type SYUCOMM .
  methods TRIGGER_AT_EXIT_COMMAND
    importing
      !UCOMM type SYUCOMM .
  methods CHANGE_NOTIFICATION
    for event CHANGE_NOTIFICATION of ZSAPMVC_MODEL
    importing
      !FIELD .
endinterface.
