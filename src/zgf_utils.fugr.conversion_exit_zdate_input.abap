FUNCTION CONVERSION_EXIT_ZDATE_INPUT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"  EXCEPTIONS
*"      INVALID_DATE
*"----------------------------------------------------------------------
* { begin MELN1059013
  DATA: DATE_OUT   TYPE D,
        LEN        TYPE I,
        LV_CENTURY TYPE I.

  LEN =  STRLEN( INPUT ).
  IF LEN = 0.
    EXIT.
  ENDIF.

* call basis functionality to convert date from external to internal format
  TRY.
      REPLACE ALL OCCURRENCES OF '/' IN INPUT WITH '.'.

      CALL METHOD CL_ABAP_DATFM=>CONV_DATE_EXT_TO_INT
        EXPORTING
          IM_DATEXT = INPUT
        IMPORTING
          EX_DATINT = DATE_OUT.
* catch all possible exceptions
    CATCH CX_ABAP_DATFM_NO_DATE CX_ABAP_DATFM_INVALID_DATE CX_ABAP_DATFM_FORMAT_UNKNOWN CX_ABAP_DATFM_AMBIGUOUS .
      MESSAGE E022 RAISING INVALID_DATE.              "Ungültiges Datum
  ENDTRY.

* handle the interval boundaries for a non-unique year specification
* (e.g. '010145' -> '20450101') and adjust accordingly ('20450101'
*  -> '19450101')
*  IF DATE_OUT > SY-DATUM AND LEN = 6.                      "MELN1504588
*    LV_CENTURY = DATE_OUT+0(2).
*    LV_CENTURY = LV_CENTURY - 1.
*    DATE_OUT+0(2) = LV_CENTURY.
*  ENDIF.

*  PERFORM CHECK_DATE USING DATE_OUT.
  OUTPUT = DATE_OUT.
ENDFUNCTION.
