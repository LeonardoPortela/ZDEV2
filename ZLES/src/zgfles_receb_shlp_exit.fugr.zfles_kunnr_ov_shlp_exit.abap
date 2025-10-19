FUNCTION ZFLES_KUNNR_OV_SHLP_EXIT.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------

  IF CALLCONTROL-STEP = 'DISP'.

* Busca Cliente e Descrição
    SELECT KUNNR, NAME1
          FROM KNA1 AS K INNER JOIN ZLEST0194 AS Z
      ON K~KUNNR = Z~KUNNR_OV
           INTO TABLE @DATA(T_RESULT)
              WHERE K~KUNNR = Z~KUNNR_OV.

    SORT T_RESULT BY KUNNR.
    DELETE ADJACENT DUPLICATES FROM T_RESULT COMPARING KUNNR.

    CALL FUNCTION 'F4UT_RESULTS_MAP'
      EXPORTING
        APPLY_RESTRICTIONS = 'X'
      TABLES
        SHLP_TAB           = SHLP_TAB
        RECORD_TAB         = RECORD_TAB
        SOURCE_TAB         = T_RESULT
      CHANGING
        SHLP               = SHLP
        CALLCONTROL        = CALLCONTROL
      EXCEPTIONS
        ILLEGAL_STRUCTURE  = 1
        OTHERS             = 2.

  ENDIF.

ENDFUNCTION.
