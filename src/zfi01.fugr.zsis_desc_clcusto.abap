FUNCTION zsis_desc_clcusto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA wa_shlp_selopt LIKE LINE OF shlp-selopt.
  DATA it_cskt TYPE STANDARD TABLE OF cskt INITIAL SIZE 0.

  SELECT DISTINCT ktext FROM cskt INTO TABLE @it_cskt.


  IF callcontrol-step = 'SELECT'.
    DATA: t_fields LIKE TABLE OF shlp_tab-fielddescr.
    DATA: w_fields LIKE LINE OF shlp_tab-fielddescr.

    LOOP AT shlp_tab.
      LOOP AT shlp_tab-fielddescr INTO w_fields.

        DATA: l_fname TYPE dfies-lfieldname.
        l_fname = w_fields-fieldname.

        CALL FUNCTION 'F4UT_PARAMETER_RESULTS_PUT'
          EXPORTING
            parameter         = w_fields-fieldname
            off_source        = 0
            len_source        = 0
            "value =
            fieldname         = l_fname
          TABLES
            shlp_tab          = shlp_tab
            record_tab        = record_tab
            source_tab        = it_cskt
          CHANGING
            shlp              = shlp
            callcontrol       = callcontrol
          EXCEPTIONS
            parameter_unknown = 1
            OTHERS            = 2.

      ENDLOOP.
    ENDLOOP.

    IF sy-subrc EQ 0.
      callcontrol-step = 'DISP'.
    ELSE.
      callcontrol-step = 'EXIT'.

    ENDIF.
    EXIT.

  ENDIF.


ENDFUNCTION.
