FUNCTION zsdmf_autoriz_dados_g_zsdt0087.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_BUKRS) TYPE  BUKRS
*"     REFERENCE(IV_VKBUR) TYPE  VKBUR
*"     REFERENCE(IV_USER) TYPE  UNAME DEFAULT SY-UNAME
*"  EXPORTING
*"     REFERENCE(EV_01_X) TYPE  FLAG
*"     REFERENCE(EV_02_X) TYPE  FLAG
*"     REFERENCE(EV_03_X) TYPE  FLAG
*"     REFERENCE(EV_04_X) TYPE  FLAG
*"----------------------------------------------------------------------

  DATA lt_values TYPE vlc_dd07v_t.

  DATA lv_fieldname(7) TYPE c.
  DATA lv_count TYPE c LENGTH 2.

  DATA lv_value1 LIKE ust12-von.
  DATA lv_value2 LIKE ust12-von.
  DATA lv_value3 LIKE ust12-von.

  PERFORM f_domain_text USING 'ZDD_AUT_0087_DADOS_GERAIS' CHANGING lt_values.

  LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<fs_values>).

    lv_value1 = iv_bukrs.
    lv_value2 = iv_vkbur.
    lv_value3 = <fs_values>-domvalue_l.

    CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
        user                = iv_user
        object              = 'ZSDT0087'
        field1              = 'BUKRS'
        value1              = lv_value1
        field2              = 'VKBUR'
        value2              = lv_value2
        field3              = 'ZSD_87_DG'
        value3              = lv_value3
      EXCEPTIONS
        user_dont_exist     = 1
        user_is_authorized  = 2
        user_not_authorized = 3
        user_is_locked      = 4
        OTHERS              = 5.

    IF sy-subrc = 2.

      lv_fieldname = 'EV_' && <fs_values>-domvalue_l && '_X'.

      ASSIGN (lv_fieldname) TO FIELD-SYMBOL(<fs_field>).

      CHECK sy-subrc EQ 0.

      <fs_field> = 'X'.

    ENDIF.

  ENDLOOP.

ENDFUNCTION.
