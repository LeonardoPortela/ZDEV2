FUNCTION zsdmf_autoriz_botoes_zsdt0087.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_BUKRS) TYPE  BUKRS
*"     REFERENCE(IV_VKBUR) TYPE  VKBUR
*"     REFERENCE(IV_USER) TYPE  SYUNAME DEFAULT SY-UNAME
*"  TABLES
*"      CT_UCOMM STRUCTURE  ZSDS083
*"----------------------------------------------------------------------

  DATA lt_values TYPE vlc_dd07v_t.
  DATA lv_ucomm TYPE sy-ucomm.

  DATA lv_value1 LIKE ust12-von.
  DATA lv_value2 LIKE ust12-von.
  DATA lv_value3 TYPE fieldvalue.

  PERFORM f_domain_text USING 'ZDD_AUT_0087_BUTTON' CHANGING lt_values.

  LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<fs_values>).

    lv_value3 = <fs_values>-domvalue_l.

    CALL FUNCTION 'ZSDMF_AUTORIZACAO_ZSDT0087'
      EXPORTING
        iv_bukrs        = iv_bukrs
        iv_vkbur        = iv_vkbur
        iv_user         = iv_user
        iv_field        = 'ZSD_87_BUT'
        iv_value        = lv_value3
      EXCEPTIONS
        sem_autorizacao = 1
        OTHERS          = 2.

    IF sy-subrc EQ 1.

      CASE <fs_values>-domvalue_l.
        WHEN '01'. lv_ucomm = 'TRANSF'.
        WHEN '02'. lv_ucomm = 'GERAR'.
        WHEN '03'. lv_ucomm = 'ZTRI'.
        WHEN '04'. lv_ucomm = 'ESTORNAR'.
        WHEN '05'. lv_ucomm = 'ALTERAR'.
        WHEN '06'. lv_ucomm = 'CANCELAR'.
        WHEN '07'. lv_ucomm = 'DESMEMBRAR'.
        WHEN '08'. lv_ucomm = 'DESBLOQ'.
        WHEN '09'. lv_ucomm = 'TROCA'.
        WHEN '10'. lv_ucomm = 'ENCERRAR'.
        WHEN '11'. lv_ucomm = 'PRICE'.
        WHEN '12'. lv_ucomm = 'PRICE_NEW'.
        WHEN '13'. lv_ucomm = 'REDIST'.
        WHEN '14'. lv_ucomm = 'MDF_VENC'.
        WHEN '15'. lv_ucomm = 'ALT_GERAIS'.
        WHEN '16'. lv_ucomm = 'DEP_LOTE'.
        WHEN '17'. lv_ucomm = 'ALT_DT_ENT'.
        WHEN '18'. lv_ucomm = 'AGRUP_OV'.
        WHEN '19'. lv_ucomm = 'ATU_IMP'.
      ENDCASE.

      APPEND lv_ucomm TO ct_ucomm.

    ENDIF.

  ENDLOOP.

ENDFUNCTION.
