FUNCTION zsd_bloqueio_romaneio.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(CD_REFERENCIA) TYPE  ZCH_REF
*"     VALUE(TP_BLOQUEIO) TYPE  CHAR1 DEFAULT 'X'
*"----------------------------------------------------------------------

  DATA: wa_romaneio TYPE zsdt0001.
  DATA: wa_bloqueio TYPE zsdt0001_bloq,
        it_bloqueio TYPE TABLE OF zsdt0001_bloq.

  SELECT SINGLE *
    INTO wa_romaneio
    FROM zsdt0001
   WHERE ch_referencia EQ cd_referencia.

  IF sy-subrc EQ 0.

    CALL FUNCTION 'ZSD_STATUS_BLOQUEIO_ROMANEIO'
      EXPORTING
        ch_referencia = wa_romaneio-ch_referencia
      TABLES
        it_bloqueio   = it_bloqueio.

    CHECK it_bloqueio[] IS NOT INITIAL.

    IF tp_bloqueio IS INITIAL. "Solicitação de Desbloqueio Romaneio no OPUS
      READ TABLE it_bloqueio INTO DATA(lwa_bloqueio) INDEX 1.
      IF sy-subrc EQ 0 AND lwa_bloqueio-bloquear = 'S'. "Sem tem faturamento gerado, aborta missão
        RETURN.
      ENDIF.
    ELSE.  "Solicitação Bloquei Romaneio OPUS
      READ TABLE it_bloqueio ASSIGNING FIELD-SYMBOL(<fs_bloqueio>) INDEX 1.
      IF sy-subrc eq 0.
        <fs_bloqueio>-bloquear = 'S'.
      ENDIF.
    ENDIF.

*    wa_bloqueio-ch_referencia    = wa_romaneio-ch_referencia.
*    wa_bloqueio-tp_movimento     = wa_romaneio-tp_movimento.
*    wa_bloqueio-doc_rem          = wa_romaneio-doc_rem.
*    wa_bloqueio-st_cct           = wa_romaneio-st_cct.
*    wa_bloqueio-ct_aquav         = wa_romaneio-ct_aquav.
*    wa_bloqueio-doc_material_arm = wa_romaneio-doc_material.
*
*    IF tp_bloqueio IS INITIAL.
*
*      if ( wa_romaneio-doc_rem      is INITIAL ) and
*         ( wa_romaneio-st_cct       is INITIAL or wa_romaneio-st_cct eq '01' ) and
*         ( wa_romaneio-ct_aquav     is INITIAL ) AND
*         ( wa_romaneio-doc_material is INITIAL ).
*        wa_bloqueio-bloquear    = 'N'.
*      else.
*        return.
*      endif.
*    ELSE.
*      wa_bloqueio-bloquear    = 'S'.
*    ENDIF.
*    APPEND wa_bloqueio TO it_bloqueio.

*--> 25.08.2023 16:05:43 - Migração S4 – ML - Início
*    CALL FUNCTION 'ZSD_OUTBOUND_REMESSA_BLOQ' IN BACKGROUND TASK
*      DESTINATION 'XI_ROMANEIO'
*      AS SEPARATE UNIT
*      TABLES
*        it_zsdt0001_bloq = it_bloqueio.
*
*    COMMIT WORK.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'ZSD_OUTBOUND_REMESSA_BLOQ'.

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
        AS SEPARATE UNIT
        TABLES
          it_zsdt0001_bloq = it_bloqueio.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          it_zsdt0001_bloq = it_bloqueio.
    ENDIF.

    COMMIT WORK.
*<-- 25.08.2023 16:05:43 - Migração S4 – ML – Fim

  ENDIF.

ENDFUNCTION.
