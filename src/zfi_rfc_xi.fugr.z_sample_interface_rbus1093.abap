FUNCTION z_sample_interface_rbus1093.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      TCURR_INS STRUCTURE  TCURR OPTIONAL
*"      TCURR_UPD STRUCTURE  TCURR OPTIONAL
*"      TCURR_DEL STRUCTURE  TCURR OPTIONAL
*"----------------------------------------------------------------------

* Sample interfcace for P/S Event RBUS1093
* Never change this function, please !!!
* Use as template only !
*
*
*  TCURR_INS:  record inserted, UKURS holds exchange rate
*  TCURR_UPD:  existing record updated, UKURS holds new exchange rate
*  TCURR_DEL:  existing record deleted, UKURS will be initial
*
*  In all three parameters fields FFACT and TFACT  for exchange rate
*  ratios will be empty since these are not used in database table
*  TCURR either, but picked up from TCURF instead
*-----------------------------------------------------------------------

* Armazenar dados gerados na atualizacao de taxas de cambio

  CHECK sy-mandt = '160'OR
        sy-mandt = '300'.

  CLEAR: it_tcurr_aux, it_tcurr_aux[].

  DO 3 TIMES.
    CASE sy-index.
*   Inclusão de registros
      WHEN 1.
        ASSIGN ('TCURR_INS[]') TO <fs_table>.
        vg_ativ = 'I'.
* Modificação de registros
      WHEN 2.
        ASSIGN ('TCURR_UPD[]') TO <fs_table>.
        vg_ativ = 'A'.
* Exclusao de registros
      WHEN 3.
        ASSIGN ('TCURR_DEL[]') TO <fs_table>.
        vg_ativ = 'E'.
    ENDCASE.
    IF ( <fs_table> IS ASSIGNED ).
      LOOP AT <fs_table> INTO wa_tcurr.
        PERFORM f_append_aux USING wa_tcurr vg_ativ.
      ENDLOOP.
    ENDIF.

  ENDDO.

*--> 25.08.2023 16:45:09 - Migração S4 – ML - Início
* Exportar dados para o XI
*  CALL FUNCTION 'Z_FI_OUTBOUND_CAMBIO' IN BACKGROUND TASK
*    DESTINATION 'XI_SIGAM_INDICE'
*    TABLES
*      t_tcurr = it_tcurr_aux.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_CAMBIO'.

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
        t_tcurr = it_tcurr_aux.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
     TABLES
        t_tcurr = it_tcurr_aux.
  ENDIF.
*<-- 25.08.2023 16:45:09 - Migração S4 – ML – Fim

  COMMIT WORK.

ENDFUNCTION.
