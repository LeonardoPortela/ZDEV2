FUNCTION z_sample_interface_00001321 .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_KNA1) LIKE  KNA1 STRUCTURE  KNA1 OPTIONAL
*"     VALUE(I_KNA1_OLD) LIKE  KNA1 STRUCTURE  KNA1 OPTIONAL
*"     VALUE(I_KNB1) LIKE  KNB1 STRUCTURE  KNB1 OPTIONAL
*"     VALUE(I_KNB1_OLD) LIKE  KNB1 STRUCTURE  KNB1 OPTIONAL
*"     VALUE(I_KNVV) LIKE  KNVV STRUCTURE  KNVV OPTIONAL
*"     VALUE(I_KNVV_OLD) LIKE  KNVV STRUCTURE  KNVV OPTIONAL
*"     VALUE(UPD_KNA1) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNAS) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNAT) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNB1) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNB5) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNBK) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNBK_IBAN) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNBW) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNEX) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNVA) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNVD) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNVI) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNVK) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNVL) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNVP) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNVS) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNVV) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_KNZA) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_ADD_ON_DATA) LIKE  CUST_ADD_ON_DATA STRUCTURE
*"        CUST_ADD_ON_DATA OPTIONAL
*"     VALUE(UPD_VCKUN) LIKE  CDPOS-CHNGIND OPTIONAL
*"  TABLES
*"      T_XKNAS STRUCTURE  FKNAS OPTIONAL
*"      T_YKNAS STRUCTURE  FKNAS OPTIONAL
*"      T_XKNAT STRUCTURE  FKNAT OPTIONAL
*"      T_YKNAT STRUCTURE  FKNAT OPTIONAL
*"      T_XKNB5 STRUCTURE  FKNB5 OPTIONAL
*"      T_YKNB5 STRUCTURE  FKNB5 OPTIONAL
*"      T_XKNBK STRUCTURE  FKNBK OPTIONAL
*"      T_YKNBK STRUCTURE  FKNBK OPTIONAL
*"      T_XKNBK_IBAN STRUCTURE  FKNBK_IBAN OPTIONAL
*"      T_YKNBK_IBAN STRUCTURE  FKNBK_IBAN OPTIONAL
*"      T_XKNBW STRUCTURE  FKNBW OPTIONAL
*"      T_YKNBW STRUCTURE  FKNBW OPTIONAL
*"      T_XKNEX STRUCTURE  FKNEX OPTIONAL
*"      T_YKNEX STRUCTURE  FKNEX OPTIONAL
*"      T_XKNVA STRUCTURE  FKNVA OPTIONAL
*"      T_YKNVA STRUCTURE  FKNVA OPTIONAL
*"      T_XKNVD STRUCTURE  FKNVD OPTIONAL
*"      T_YKNVD STRUCTURE  FKNVD OPTIONAL
*"      T_XKNVI STRUCTURE  FKNVI OPTIONAL
*"      T_YKNVI STRUCTURE  FKNVI OPTIONAL
*"      T_XKNVK STRUCTURE  FKNVK OPTIONAL
*"      T_YKNVK STRUCTURE  FKNVK OPTIONAL
*"      T_XKNVL STRUCTURE  FKNVL OPTIONAL
*"      T_YKNVL STRUCTURE  FKNVL OPTIONAL
*"      T_XKNVP STRUCTURE  FKNVP OPTIONAL
*"      T_YKNVP STRUCTURE  FKNVP OPTIONAL
*"      T_XKNVS STRUCTURE  FKNVS OPTIONAL
*"      T_YKNVS STRUCTURE  FKNVS OPTIONAL
*"      T_XKNZA STRUCTURE  FKNZA OPTIONAL
*"      T_YKNZA STRUCTURE  FKNZA OPTIONAL
*"      T_UPD_TXT STRUCTURE  FKUNTXT OPTIONAL
*"      T_XVCNUM STRUCTURE  VCNUMVB OPTIONAL
*"      T_YVCNUM STRUCTURE  VCNUMVB OPTIONAL
*"      T_XVCKUN STRUCTURE  VCKUNVB OPTIONAL
*"      T_YVCKUN STRUCTURE  VCKUNVB OPTIONAL
*"----------------------------------------------------------------------

*> A chamada em background serve para aguardar o momento de commit da
*> transação, pois a BTE está inserida no contexto do código.
*> Desta forma será possível enviar os dados de endereçamento que ainda
*> não foram gravados no BD (somente no insert).
  CLEAR vg_testeabap_debug.

  IF ( vg_testeabap_debug IS INITIAL ).
    CALL FUNCTION 'Z_FI_OUTCUSTOMER_BACKGROUND' IN BACKGROUND TASK
      DESTINATION 'NONE'
      EXPORTING
        i_kna1   = i_kna1
        i_knb1   = i_knb1
        upd_kna1 = upd_kna1
        upd_knb1 = upd_knb1
        upd_knvk = upd_knvk
      TABLES
        t_xknbk  = t_xknbk
        t_yknbk  = t_yknbk
        t_xknvk  = t_xknvk
        t_yknvk  = t_yknvk.


    IF upd_knas IS NOT INITIAL.
      DATA : t_knas      TYPE TABLE OF zknas_out,
             wa_knas_out TYPE zknas_out,
             wa_xknas    TYPE fknas,
             wa_yknas    TYPE fknas.

      REFRESH t_knas.

      LOOP AT t_xknas INTO wa_xknas.

        CLEAR : wa_knas_out, wa_yknas.

        MOVE-CORRESPONDING wa_xknas TO wa_knas_out.

        wa_knas_out-tp_atualizacao  = upd_knas.
        wa_knas_out-dt_atualizacao = sy-datum.

        READ TABLE t_yknas INTO wa_yknas  INDEX  sy-tabix.

        IF sy-subrc IS INITIAL.
          wa_knas_out-land1_old = wa_yknas-land1.
          wa_knas_out-stceg_old = wa_yknas-stceg.
        ENDIF.

        APPEND wa_knas_out TO t_knas.

      ENDLOOP.


      LOOP AT t_yknas INTO wa_yknas WHERE kz EQ ''.

        CLEAR wa_knas_out.

        MOVE-CORRESPONDING wa_yknas TO wa_knas_out.

        wa_knas_out-tp_atualizacao  = 'D'.
        wa_knas_out-dt_atualizacao = sy-datum.

        APPEND wa_knas_out TO t_knas.

      ENDLOOP.

*--> 23.08.2023 17:06:53 - Migração S4 – ML - Início
*      CALL FUNCTION 'Z_FI_OUTBOUND_CUSTOMER_VAT' IN BACKGROUND TASK
*        DESTINATION 'XI_CUSTOMER_VAT'
*        TABLES
*          T_KNAS = T_KNAS.

      DATA: lv_rfc TYPE rfcdest.

      CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_CUSTOMER_VAT'.

      CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
        EXPORTING
          i_fm          = c_fm
        IMPORTING
          e_rfc         = lv_rfc
        EXCEPTIONS
          no_rfc        = 1
          no_rfc_config = 2
          OTHERS        = 3.

      IF sy-subrc EQ 0.
        CALL FUNCTION c_fm IN BACKGROUND TASK
          DESTINATION lv_rfc
          TABLES
            t_knas = t_knas.
      ELSE.
        CALL FUNCTION c_fm IN BACKGROUND TASK
          TABLES
            t_knas = t_knas.
      ENDIF.
*<-- 23.08.2023 17:06:53 - Migração S4 – ML – Fim

      COMMIT WORK.

    ENDIF.


  ELSE.
    CALL FUNCTION 'Z_FI_OUTCUSTOMER_BACKGROUND' STARTING NEW TASK 'OUTCUS'
      EXPORTING
        i_kna1   = i_kna1
        i_knb1   = i_knb1
        upd_kna1 = upd_kna1
        upd_knb1 = upd_knb1
        upd_knvk = upd_knvk
      TABLES
        t_xknbk  = t_xknbk
        t_yknbk  = t_yknbk
        t_xknvk  = t_xknvk
        t_yknvk  = t_yknvk.

  ENDIF.

ENDFUNCTION.
