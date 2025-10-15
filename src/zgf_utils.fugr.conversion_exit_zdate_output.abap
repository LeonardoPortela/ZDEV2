FUNCTION CONVERSION_EXIT_ZDATE_OUTPUT.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"--------------------------------------------------------------------
  DATA: LV_DATE      TYPE D,
        LR_ELEMDESCR TYPE REF TO CL_ABAP_ELEMDESCR,        "MELN1508874
        LV_OUTPUT    TYPE REF TO DATA.

  FIELD-SYMBOLS <LV_OUTPUT> TYPE ANY.

  IF NOT INPUT IS INITIAL.
    LV_DATE = INPUT.

    LR_ELEMDESCR ?= CL_ABAP_ELEMDESCR=>DESCRIBE_BY_DATA( OUTPUT ).
    IF LR_ELEMDESCR->OUTPUT_LENGTH < 10.
      CREATE DATA LV_OUTPUT TYPE CHAR8.
    ELSE.
      CREATE DATA LV_OUTPUT TYPE CHAR10.
    ENDIF.
    ASSIGN LV_OUTPUT->* TO <LV_OUTPUT>.

*  call basis functionality to convert date from internal to external format
    TRY.
        CALL METHOD CL_ABAP_DATFM=>CONV_DATE_INT_TO_EXT
          EXPORTING
            IM_DATINT = LV_DATE
          IMPORTING
            EX_DATEXT = <LV_OUTPUT>.

        REPLACE ALL OCCURRENCES OF '.' IN <LV_OUTPUT> WITH '/'.

      CATCH CX_ABAP_DATFM_FORMAT_UNKNOWN.
        CLEAR <LV_OUTPUT>.
    ENDTRY.
    OUTPUT = <LV_OUTPUT>.
  ENDIF.
ENDFUNCTION.
