FUNCTION zsdmf_autorizacao_zsdt0087.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_BUKRS) TYPE  BUKRS
*"     REFERENCE(IV_VKBUR) TYPE  VKBUR
*"     REFERENCE(IV_USER) TYPE  UNAME DEFAULT SY-UNAME
*"     REFERENCE(IV_FIELD) TYPE  FIELDNAME OPTIONAL
*"     REFERENCE(IV_VALUE) TYPE  FIELDVALUE OPTIONAL
*"  EXCEPTIONS
*"      SEM_AUTORIZACAO
*"----------------------------------------------------------------------

  DATA lv_value1 LIKE ust12-von.
  DATA lv_value2 LIKE ust12-von.
  DATA lv_value3 LIKE ust12-von.

  DATA lv_field3 TYPE ust12-field.

  lv_value1 = iv_bukrs.
  lv_value2 = iv_vkbur.
  lv_value3 = iv_value.

  lv_field3 = iv_field.

  CALL FUNCTION 'AUTHORITY_CHECK'
    EXPORTING
      user                = iv_user
      object              = 'ZSDT0087'
      field1              = 'BUKRS'
      value1              = lv_value1
      field2              = 'VKBUR'
      value2              = lv_value2
      field3              = lv_field3
      value3              = lv_value3
    EXCEPTIONS
      user_dont_exist     = 1
      user_is_authorized  = 2
      user_not_authorized = 3
      user_is_locked      = 4
      OTHERS              = 5.

*  AUTHORITY-CHECK OBJECT 'ZSD_087'
*           ID 'BUKRS' FIELD iv_bukrs
*           ID 'VKBUR' FIELD iv_vkbur
*           ID iv_field FIELD iv_value.

  IF sy-subrc <> 2.

    SELECT SINGLE scrtext_l FROM dd04v
      INNER JOIN authx ON authx~rollname = dd04v~rollname
        INTO @DATA(lv_descr)
          WHERE  authx~fieldname = @iv_field.

    IF iv_field = 'ZTPSIM'.

      MESSAGE ID 'DS' TYPE 'E' NUMBER '016'
        WITH 'Sem autorização para' 'Cond. de Pgto' iv_value
          RAISING sem_autorizacao.

    ENDIF.

    IF iv_value = 'X'.

      MESSAGE ID 'DS' TYPE 'E' NUMBER '016'
        WITH 'Sem autorização para' lv_descr
          RAISING sem_autorizacao.

    ELSE.

      MESSAGE ID 'DS' TYPE 'E' NUMBER '016'
        WITH 'Sem autorização para' lv_descr iv_value
          RAISING sem_autorizacao.

    ENDIF.

  ENDIF.

ENDFUNCTION.
