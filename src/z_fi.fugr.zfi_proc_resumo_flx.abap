FUNCTION zfi_proc_resumo_flx.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DATA_INI) TYPE  DZFBDT
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_DT_BASE_VERSAO) TYPE  RRSELDATE
*"     REFERENCE(I_HORA_VERSAO) TYPE  RRSELTIME
*"     REFERENCE(I_VERSAO) TYPE  NUM4
*"     REFERENCE(I_REF_SALDO) TYPE  CHAR01 OPTIONAL
*"  EXCEPTIONS
*"      M_ERROR
*"----------------------------------------------------------------------


  DATA: BEGIN OF tg_0119_resumo OCCURS 0,
          bukrs    TYPE zfit0119-bukrs,
          codigo   TYPE zfit0119-codigo,
          clas_flx TYPE zfit0119-clas_flx,
          tp_prev  TYPE zfit0119-tp_prev,
          zfbdt    TYPE zfit0119-zfbdt,
          dmbtr    TYPE zfit0119-dmbtr,
          dmbe2    TYPE zfit0119-dmbe2,
        END OF tg_0119_resumo.

  DATA: BEGIN OF tg_0079_resumo OCCURS 0,
          bukrs            TYPE zfit0079-bukrs,
          codigo           TYPE zfit0079-codigo,
          clas_flx         TYPE zfit0079-clas_flx,
          tp_prev          TYPE zfit0079-tp_prev,
          zfbdt            TYPE zfit0079-zfbdt,
          cx_internacional TYPE zfit0079-cx_internacional,
          dmbtr            TYPE zfit0079-dmbtr,
          dmbe2            TYPE zfit0079-dmbe2,
        END OF tg_0079_resumo,

        BEGIN OF tg_0111_resumo OCCURS 0,
          bukrs            TYPE zfit0111-bukrs,
          dt_vcto          TYPE zfit0111-dt_vcto,
          cx_internacional TYPE zfit0111-cx_internacional,
        END OF tg_0111_resumo.

  DATA: tg_0079     LIKE zfit0079 OCCURS 0 WITH HEADER LINE,
        tg_0079_aux LIKE zfit0079 OCCURS 0 WITH HEADER LINE,
        tg_0109     LIKE zfit0109 OCCURS 0 WITH HEADER LINE,
        tg_0109_aux LIKE zfit0109 OCCURS 0 WITH HEADER LINE,
        tg_0111_det LIKE zfit0111 OCCURS 0 WITH HEADER LINE,
        tg_0113     LIKE zfit0113 OCCURS 0 WITH HEADER LINE,
        tg_0113_aux LIKE zfit0113 OCCURS 0 WITH HEADER LINE,
        tg_0115_aux LIKE zfit0115 OCCURS 0 WITH HEADER LINE,
        tg_0118_det LIKE zfit0118 OCCURS 0 WITH HEADER LINE.

  DATA: vl_max_dt_vcto  TYPE zfit0113-dt_vcto,
        vl_saldo_ini_r  TYPE zfit0113-sdo_inicial_r,
        vl_saldo_ini_us TYPE zfit0113-sdo_inicial_us,
        vl_saldo_dia_r  TYPE zfit0113-dmbtr,
        vl_saldo_dia_us TYPE zfit0113-dmbe2,
        vl_saldo_fim_r  TYPE zfit0113-sdo_final_r,
        vl_saldo_fim_us TYPE zfit0113-sdo_final_us,
        vl_min_dt_clone TYPE sydatum,
        vl_idx_resumo   TYPE i,
        vl_max_dt_base  TYPE zfit0079-dt_base_versao,
        vl_dt_base      TYPE zfit0079-dt_base_versao,
        vl_max_versao   TYPE zfit0079-versao,
        vl_versao       TYPE zfit0079-versao,
        vl_hora_versao  TYPE rrseltime.

  IF i_hora_versao IS NOT INITIAL.
    vl_hora_versao = i_hora_versao.
  ELSE.
    vl_hora_versao = sy-uzeit.
  ENDIF.

  IF i_ref_saldo IS NOT INITIAL.
    "Limpa Tabela de Resumo Detalhe
    DELETE FROM zfit0111 WHERE bukrs          =  i_bukrs
                           AND dt_vcto        >= i_data_ini
                           AND dt_base_versao = i_dt_base_versao
                           AND versao         = i_versao.

    "Limpa Tabela de Resumo Detalhe - Sem Calculo de Saldo
    DELETE FROM zfit0118 WHERE bukrs          =  i_bukrs
                           AND dt_vcto        >= i_data_ini
                           AND dt_base_versao = i_dt_base_versao
                           AND versao         = i_versao.

    "Limpa Tabela de Resumo
    DELETE FROM zfit0113 WHERE bukrs          =  i_bukrs
                           AND dt_vcto        >= i_data_ini
                           AND dt_base_versao = i_dt_base_versao
                           AND versao         = i_versao.
  ENDIF.


  "Buscar Registros p/ Atualização do Detalhe do Resumo (ZFIT0079)
  SELECT bukrs codigo clas_flx tp_prev zfbdt cx_internacional SUM( dmbtr ) SUM( dmbe2 )
    FROM zfit0079
    INTO TABLE tg_0079_resumo
   WHERE bukrs            EQ i_bukrs
     AND zfbdt            >= i_data_ini
     AND dt_base_versao   = i_dt_base_versao
     AND versao           = i_versao
   GROUP BY bukrs codigo clas_flx tp_prev zfbdt cx_internacional.

  IF ( tg_0079_resumo[] IS NOT INITIAL ).

    SELECT *
      FROM zfit0109
      INTO TABLE tg_0109
      FOR ALL ENTRIES IN tg_0079_resumo
     WHERE codigo  = tg_0079_resumo-codigo.

  ENDIF.

  "Buscar Registros p/ Atualização do Detalhe do Resumo (XRT)
  SELECT bukrs codigo clas_flx tp_prev zfbdt SUM( dmbtr ) SUM( dmbe2 )
    FROM zfit0119
    INTO TABLE tg_0119_resumo
   WHERE bukrs            EQ i_bukrs
     AND zfbdt            >= i_data_ini
     AND dt_base_versao   = i_dt_base_versao
     AND versao           = i_versao
   GROUP BY bukrs codigo clas_flx tp_prev zfbdt.

  IF ( tg_0119_resumo[] IS NOT INITIAL ).

    SELECT *
      FROM zfit0109
      APPENDING TABLE tg_0109
      FOR ALL ENTRIES IN tg_0119_resumo
     WHERE codigo  = tg_0119_resumo-codigo.

  ENDIF.

  SORT tg_0109 BY codigo.
  DELETE ADJACENT DUPLICATES FROM tg_0109 COMPARING codigo.

  "Grava dados detalhados para compor Tabela de Resumo - ( ZFIT0079 )
  LOOP AT tg_0079_resumo.

    CLEAR: tg_0111_det, tg_0109, tg_0118_det.

    READ TABLE tg_0109 WITH KEY codigo = tg_0079_resumo-codigo.

    IF ( sy-subrc NE 0 ).
      CONTINUE.
    ENDIF.

    IF ( tg_0109-st_calc_sdo IS NOT INITIAL ). "Codigos de Fluxos que influenciam no Saldo

      tg_0111_det-bukrs           = tg_0079_resumo-bukrs.
      tg_0111_det-codigo          = tg_0079_resumo-codigo.
      tg_0111_det-clas_flx        =	tg_0079_resumo-clas_flx.
      tg_0111_det-tp_prev         =	tg_0079_resumo-tp_prev.
      tg_0111_det-seq             = tg_0109-seq.
      tg_0111_det-dt_vcto         =	tg_0079_resumo-zfbdt.
      tg_0111_det-cx_internacional = tg_0079_resumo-cx_internacional.
*      tg_0111_det-cx_internacional = tg_0079_resumo-cx_internacional.
*---> 15/06/2023 - Migração S4 - JS
*      TG_0111_DET-DMBTR   = TG_0079_RESUMO-DMBTR.
*      TG_0111_DET-DMBE2   = TG_0079_RESUMO-DMBE2.
      tg_0111_det-dmbtr = CONV #( tg_0079_resumo-dmbtr ).
      tg_0111_det-dmbe2 = CONV #( tg_0079_resumo-dmbe2 ).
*<--- 15/06/2023 - Migração S4 - JS
      tg_0111_det-dt_atual        = sy-datum.
      tg_0111_det-hr_atual        = sy-uzeit.

      IF ( tg_0079_resumo-dmbtr > 0 ) AND ( tg_0079_resumo-dmbe2 > 0 ).
        TRY.
            tg_0111_det-kursf         = ( tg_0079_resumo-dmbtr / tg_0079_resumo-dmbe2 ).
          CATCH cx_sy_arithmetic_overflow.
        ENDTRY.
      ENDIF.

      tg_0111_det-dt_base_versao  = i_dt_base_versao.
      tg_0111_det-hora_versao     = vl_hora_versao.
      tg_0111_det-versao          = i_versao.

      MODIFY zfit0111 FROM tg_0111_det.

      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao gerar o Resumo' TYPE 'S' RAISING m_error.
        RETURN.
      ENDIF.


    ELSE. "Codigos de Fluxos que não influenciam no Saldo

      IF tg_0109-tp_prev EQ 'S'. "Saldo Inicial
        CONTINUE.
      ENDIF.

      tg_0118_det-bukrs           = tg_0079_resumo-bukrs.
      tg_0118_det-codigo          = tg_0079_resumo-codigo.
      tg_0118_det-clas_flx        =	tg_0079_resumo-clas_flx.
      tg_0118_det-tp_prev         =	tg_0079_resumo-tp_prev.
      tg_0118_det-seq             = tg_0109-seq.
      tg_0118_det-dt_vcto         =	tg_0079_resumo-zfbdt.
      tg_0118_det-cx_internacional = tg_0079_resumo-cx_internacional.
*---> 15/06/2023 - Migração S4 - JS
*      TG_0118_DET-DMBTR = TG_0079_RESUMO-DMBTR.
*      TG_0118_DET-DMBE2 = TG_0079_RESUMO-DMBE2.
      tg_0118_det-dmbtr = CONV #( tg_0079_resumo-dmbtr ).
      tg_0118_det-dmbe2 = CONV #( tg_0079_resumo-dmbe2 ).
*<--- 15/06/2023 - Migração S4 - JS
      tg_0118_det-dt_atual        = sy-datum.
      tg_0118_det-hr_atual        = sy-uzeit.

      IF ( tg_0079_resumo-dmbtr > 0 ) AND ( tg_0079_resumo-dmbe2 > 0 ).
        TRY.
            tg_0118_det-kursf         = ( tg_0079_resumo-dmbtr / tg_0079_resumo-dmbe2 ).
          CATCH cx_sy_arithmetic_overflow.
        ENDTRY.
      ENDIF.

      tg_0118_det-dt_base_versao  = i_dt_base_versao.
      tg_0118_det-hora_versao     = vl_hora_versao.
      tg_0118_det-versao          = i_versao.

      MODIFY zfit0118 FROM tg_0118_det.

      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao gerar o Resumo' TYPE 'S' RAISING m_error.
        RETURN.
      ENDIF.

    ENDIF.

  ENDLOOP.

  "Grava dados detalhados para compor Tabela de Resumo - ( XRT )
  LOOP AT tg_0119_resumo.

    CLEAR: tg_0111_det, tg_0109, tg_0118_det.

    READ TABLE tg_0109 WITH KEY codigo = tg_0119_resumo-codigo.

    IF ( sy-subrc NE 0 ).
      CONTINUE.
    ENDIF.

    IF ( tg_0109-st_calc_sdo IS NOT INITIAL ). "Codigos de Fluxos que influenciam no Saldo

      tg_0111_det-bukrs           = tg_0119_resumo-bukrs.
      tg_0111_det-codigo          = tg_0119_resumo-codigo.
      tg_0111_det-clas_flx        =	tg_0109-clas_flx.
      tg_0111_det-tp_prev         =	tg_0109-tp_prev.
      tg_0111_det-seq             = tg_0109-seq.
      tg_0111_det-dt_vcto         =	tg_0119_resumo-zfbdt.
*---> 15/06/2023 - Migração S4 - JS
*      TG_0111_DET-DMBTR   = TG_0119_RESUMO-DMBTR.
*      TG_0111_DET-DMBE2   = TG_0119_RESUMO-DMBE2.
      tg_0111_det-dmbtr = CONV #( tg_0119_resumo-dmbtr ).
      tg_0111_det-dmbe2 = CONV #( tg_0119_resumo-dmbe2 ).
*<--- 15/06/2023 - Migração S4 - JS
      tg_0111_det-dt_atual        = sy-datum.
      tg_0111_det-hr_atual        = sy-uzeit.

      IF ( tg_0111_det-dmbtr > 0 ) AND ( tg_0111_det-dmbe2 > 0 ).
        TRY.
            tg_0111_det-kursf           = ( tg_0111_det-dmbtr / tg_0111_det-dmbe2 ).
          CATCH cx_sy_arithmetic_overflow.
        ENDTRY.
      ENDIF.

      tg_0111_det-dt_base_versao  = i_dt_base_versao.
      tg_0111_det-hora_versao     = vl_hora_versao.
      tg_0111_det-versao          = i_versao.

      MODIFY zfit0111 FROM tg_0111_det.

      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao gerar o Resumo' TYPE 'S' RAISING m_error.
        RETURN.
      ENDIF.

    ELSE. "Codigos de Fluxos que não influenciam no Saldo

      IF tg_0109-tp_prev EQ 'S'. "Saldo Inicial
        CONTINUE.
      ENDIF.

      tg_0118_det-bukrs           = tg_0119_resumo-bukrs.
      tg_0118_det-codigo          = tg_0119_resumo-codigo.
      tg_0118_det-clas_flx        =	tg_0109-clas_flx.
      tg_0118_det-tp_prev         =	tg_0109-tp_prev.
      tg_0118_det-seq             = tg_0109-seq.
      tg_0118_det-dt_vcto         =	tg_0119_resumo-zfbdt.
*---> 15/06/2023 - Migração S4 - JS
*      TG_0118_DET-DMBTR = TG_0119_RESUMO-DMBTR.
*      TG_0118_DET-DMBE2 = TG_0119_RESUMO-DMBE2.
      tg_0118_det-dmbtr = CONV #( tg_0119_resumo-dmbtr ).
      tg_0118_det-dmbe2 = CONV #( tg_0119_resumo-dmbe2 ).
*<--- 15/06/2023 - Migração S4 - JS
      tg_0118_det-dt_atual        = sy-datum.
      tg_0118_det-hr_atual        = sy-uzeit.

      IF ( tg_0118_det-dmbtr > 0 ) AND ( tg_0118_det-dmbe2 > 0 ).
        TRY.
            tg_0118_det-kursf         = ( tg_0118_det-dmbtr / tg_0118_det-dmbe2 ).
          CATCH cx_sy_arithmetic_overflow.
        ENDTRY.
      ENDIF.

      tg_0118_det-dt_base_versao  = i_dt_base_versao.
      tg_0118_det-hora_versao     = vl_hora_versao.
      tg_0118_det-versao          = i_versao.

      MODIFY zfit0118 FROM tg_0118_det.

      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao gerar o Resumo' TYPE 'S' RAISING m_error.
        RETURN.
      ENDIF.

    ENDIF.

  ENDLOOP.

*---------------------------------------------------------------------------------------*
*  Verifica se tem movimento na data atual para a empresa. Caso não tenha,
*  gerar apenas uma linha nos detalhes do resumo com valor zerado,
*  para atribuição do Saldo inicial do Dia.
*---------------------------------------------------------------------------------------*
  SELECT SINGLE *
    INTO tg_0111_det
    FROM zfit0111
   WHERE bukrs           = i_bukrs
     AND dt_vcto         = sy-datum
     AND dt_base_versao  = i_dt_base_versao
     AND versao          = i_versao.

  IF sy-subrc NE 0.

    CLEAR: tg_0111_det, tg_0109.

    "Busca Cod. Fluxo para Saldo Inicial.
    SELECT SINGLE *
      INTO tg_0109
      FROM zfit0109
     WHERE tp_prev = 'S'.

    IF sy-subrc EQ 0.

      tg_0111_det-bukrs           = i_bukrs.
      tg_0111_det-codigo          = tg_0109-codigo.
      tg_0111_det-clas_flx        =	tg_0109-clas_flx.
      tg_0111_det-tp_prev         =	tg_0109-tp_prev.
      tg_0111_det-seq             = tg_0109-seq.
      tg_0111_det-dt_vcto         =	sy-datum.
      tg_0111_det-dmbtr           = 0.
      tg_0111_det-dmbe2           = 0.
      tg_0111_det-dt_atual        = sy-datum.
      tg_0111_det-hr_atual        = sy-uzeit.
      tg_0111_det-kursf           = 0.
      tg_0111_det-dt_base_versao  = i_dt_base_versao.
      tg_0111_det-hora_versao     = vl_hora_versao.
      tg_0111_det-versao          = i_versao.

      MODIFY zfit0111 FROM tg_0111_det.

      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao gerar o Resumo' TYPE 'S' RAISING m_error.
        RETURN.
      ENDIF.

    ENDIF.

  ENDIF.

  REFRESH: tg_0111_resumo, tg_0111_det.

  "Buscar Registros para gerar Resumo
  SELECT bukrs dt_vcto cx_internacional
    FROM zfit0111
    INTO TABLE tg_0111_resumo
   WHERE bukrs            = i_bukrs
     AND dt_vcto          >= i_data_ini
     AND dt_base_versao   = i_dt_base_versao
     AND versao           = i_versao
   GROUP BY bukrs dt_vcto cx_internacional.

  SELECT *
    FROM zfit0111
    INTO TABLE tg_0111_det
   WHERE bukrs            EQ i_bukrs
     AND dt_vcto          >= i_data_ini
     AND dt_base_versao   = i_dt_base_versao
     AND versao           = i_versao.

  "Ordenar por data de Vencimento
  SORT: tg_0111_resumo BY dt_vcto,
        tg_0111_det    BY dt_vcto.

  vl_idx_resumo = 1.

  LOOP AT tg_0111_resumo.

    CLEAR: tg_0113, tg_0113_aux, vl_max_dt_vcto,
           vl_saldo_ini_r, vl_saldo_ini_us,
           vl_saldo_dia_r, vl_saldo_dia_us,
           vl_saldo_fim_r, vl_saldo_fim_us.

    REFRESH: tg_0113, tg_0113_aux.


    CLEAR: vl_max_versao, vl_max_dt_base, vl_max_dt_vcto, tg_0113_aux.

    vl_saldo_ini_r  = 0.
    vl_saldo_ini_us = 0.

    "Verifica se tem saldo Inicial implantado no dia.
    SELECT SINGLE *
      INTO tg_0115_aux
      FROM zfit0115
     WHERE dt_vcto = tg_0111_resumo-dt_vcto
       AND bukrs   = tg_0111_resumo-bukrs
       AND tp_prev = 'S'. "Saldo Inicial

    IF sy-subrc = 0.
      vl_saldo_ini_r  = tg_0115_aux-dmbtr.
      vl_saldo_ini_us = tg_0115_aux-dmbe2.
    ELSE.

      "Verifica se tem saldo anterior a data de Vencimento.
      SELECT MAX( dt_vcto )
        INTO vl_max_dt_vcto
        FROM zfit0113
       WHERE bukrs            = tg_0111_resumo-bukrs
         AND dt_vcto          < tg_0111_resumo-dt_vcto
         AND dt_base_versao   = i_dt_base_versao
         AND versao           = i_versao.

      IF ( sy-subrc = 0 ) AND ( vl_max_dt_vcto IS NOT INITIAL ).

        SELECT SINGLE *
          INTO tg_0113_aux
          FROM zfit0113
         WHERE bukrs            = tg_0111_resumo-bukrs
           AND dt_vcto          = vl_max_dt_vcto
           AND dt_base_versao   = i_dt_base_versao
           AND versao           = i_versao.

        IF sy-subrc = 0.
          vl_saldo_ini_r  = tg_0113_aux-sdo_final_r.
          vl_saldo_ini_us = tg_0113_aux-sdo_final_us.
        ENDIF.

      ENDIF.

    ENDIF.

    tg_0113-sdo_inicial_r   = vl_saldo_ini_r.
    tg_0113-sdo_inicial_us  = vl_saldo_ini_us.
    tg_0113-bukrs           = tg_0111_resumo-bukrs.
    tg_0113-dt_vcto         =	tg_0111_resumo-dt_vcto.

    "Buscar Saldo do dia do Vencimento.
    LOOP AT tg_0111_det WHERE bukrs   = tg_0111_resumo-bukrs
                          AND dt_vcto = tg_0111_resumo-dt_vcto
                          and cx_internacional = tg_0111_resumo-cx_internacional.

      tg_0111_det-dmbtr  = abs( tg_0111_det-dmbtr ).
      tg_0111_det-dmbe2  = abs( tg_0111_det-dmbe2 ).

      IF tg_0111_det-clas_flx = 'S'.
        vl_saldo_dia_r  = vl_saldo_dia_r   + ( tg_0111_det-dmbtr * -1 ).
        vl_saldo_dia_us = vl_saldo_dia_us  + ( tg_0111_det-dmbe2 * -1 ).
      ELSE.
        vl_saldo_dia_r  = vl_saldo_dia_r   + tg_0111_det-dmbtr.
        vl_saldo_dia_us = vl_saldo_dia_us  + tg_0111_det-dmbe2.
      ENDIF.

    ENDLOOP.

    tg_0113-dmbtr           = vl_saldo_dia_r.
    tg_0113-dmbe2           = vl_saldo_dia_us.
    tg_0113-dt_atual        = sy-datum.
    tg_0113-hr_atual        = sy-uzeit.
    tg_0113-sdo_final_r     = ( vl_saldo_ini_r  + vl_saldo_dia_r ) .
    tg_0113-sdo_final_us    = ( vl_saldo_ini_us + vl_saldo_dia_us ).
    tg_0113-cx_internacional = tg_0111_resumo-cx_internacional.
    IF ( vl_saldo_dia_r > 0 ) AND ( vl_saldo_dia_us > 0 ).
      TRY.
          tg_0113-kursf         = ( vl_saldo_dia_r / vl_saldo_dia_us ).
        CATCH cx_sy_arithmetic_overflow.
      ENDTRY.
    ENDIF.

    tg_0113-dt_base_versao  = i_dt_base_versao.
    tg_0113-hora_versao     = vl_hora_versao.
    tg_0113-versao          = i_versao.

    MODIFY zfit0113 FROM tg_0113.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gerar o Resumo' TYPE 'S' RAISING m_error.
      RETURN.
    ENDIF.

  ENDLOOP.


ENDFUNCTION.
