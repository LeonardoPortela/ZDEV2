"Name: \TY:CL_FAA_CFG_DB_ACCESS_ERP\IN:IF_FAA_CFG_ACCESS_ERP\ME:T093B_READ_MULTIPLE\SE:END\EI
ENHANCEMENT 0 Z_AFAB_BRL.
*
  IF sy-tcode = 'AFAB' OR sy-tcode = 'AFABN' or sy-tcode is INITIAL.
    SELECT SINGLE *
      FROM t001
      INTO @DATA(_t001)
      WHERE bukrs = @iv_bukrs.
    IF _t001-land1 = 'BR'.
      LOOP AT et_t093b ASSIGNING FIELD-SYMBOL(<w_t093b>).
        IF '11_16_42' CS <w_t093b>-afabe.
          <w_t093b>-waers = 'BRL'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
