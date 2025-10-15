FUNCTION zpmsearchhelp_exit_event .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  SORT record_tab.
  DELETE ADJACENT DUPLICATES FROM record_tab.

  TYPES:
    BEGIN OF ty_itob,
      herst      TYPE itob-herst,
      class_oper TYPE itob-eqart,
    END OF ty_itob.

  DATA: lw_itob  TYPE zpmr0001,
        lv_tabix TYPE sy-tabix.

  FIELD-SYMBOLS: <fs_itob> TYPE itob.

  ASSIGN ('(SAPMIEQ0)ITOB') TO <fs_itob>.
  SORT record_tab BY string.
  DELETE ADJACENT DUPLICATES FROM record_tab COMPARING string.
  CHECK <fs_itob> IS ASSIGNED.

  LOOP AT record_tab ASSIGNING FIELD-SYMBOL(<fs_record_tab>).

    lv_tabix = sy-tabix.

    CALL METHOD cl_abap_container_utilities=>read_container_c
      EXPORTING
        im_container           = <fs_record_tab>-string
      IMPORTING
        ex_value               = lw_itob
      EXCEPTIONS
        illegal_parameter_type = 1
        OTHERS                 = 2.

    IF ( lw_itob-class_oper NE <fs_itob>-eqart ).
      DELETE record_tab[] INDEX lv_tabix.
    ENDIF.

  ENDLOOP.
*  DELETE record_tab WHERE eqart <> <fs_itbo>-eqart.

ENDFUNCTION.
