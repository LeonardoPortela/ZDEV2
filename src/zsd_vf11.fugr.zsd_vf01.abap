FUNCTION zsd_vf01.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"----------------------------------------------------------------------

  FREE: obj_auart, obj.
  CREATE OBJECT obj_auart.
  CREATE OBJECT obj.

  DATA: vg_check_dispara_hedge TYPE char01.

  CLEAR: vg_check_dispara_hedge.

  CHECK sy-tcode EQ 'VF01'.

  sair = 0.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 1
      text       = 'Aguardando Gravação'.

  DO.

    ADD 1 TO sair.

    SELECT SINGLE *
      FROM vbrk
      INTO @DATA(wa_vbrk)
      WHERE vbeln EQ @i_vbeln.

    IF sy-subrc IS NOT INITIAL.

      IF sair EQ 5 .
        EXIT.
      ENDIF.

      WAIT UP TO 2 SECONDS.

    ELSE.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sair * 20
        text       = 'Aguardando Gravação'.

  ENDDO.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 100
      text       = 'Aguardando Gravação'.

  CHECK wa_vbrk IS NOT INITIAL.

  SELECT SINGLE *
    FROM vbrp
    INTO @DATA(wa_vbrp)
    WHERE vbeln EQ @i_vbeln.

* //"Get SET de AUART de Complemento
  CALL METHOD obj_auart->get_auart
    EXPORTING
      set     = 'ZHEDGECOMP'
    RECEIVING
      r_range = r_comp.

* //"Get SET de AUART de Devolução/Recusa
  CALL METHOD obj_auart->get_auart
    EXPORTING
      set     = 'ZHEDGEDEVO/RECU'
    RECEIVING
      r_range = r_devo_recu.

* //"Get SET de AUART de Devolução/Recusa
  CALL METHOD obj_auart->get_auart
    EXPORTING
      set     = 'TODOS'
    RECEIVING
      r_range = r_auart.

  SELECT SINGLE *
    FROM vbak
    INTO @DATA(wa_vbak)
    WHERE vbeln EQ @wa_vbrp-aubel.

  IF wa_vbak-auart IN r_devo_recu.
    var_dir = 'Y'.
  ELSEIF wa_vbak-auart IN r_comp.
    var_dir = 'W'.
  ENDIF.

  CHECK wa_vbak-auart IN r_auart.

*  SELECT SINGLE *
*    FROM vbak
*    INTO @DATA(wa_vbak)
*    WHERE vbeln EQ @wa_vbrp-vgbel.

  SELECT *
    FROM vbap
    INTO TABLE it_vbap
      WHERE vbeln EQ wa_vbak-vbeln.

  SELECT * FROM vbfa
      INTO TABLE lt_vbfa_equ
    WHERE vbeln   EQ wa_vbak-vgbel
      AND vbtyp_n EQ 'M'
      AND vbtyp_v IN ('C', 'L').

  READ TABLE lt_vbfa_equ INTO lw_vbfa_equ WITH KEY vbeln   = wa_vbak-vgbel
                                                   vbtyp_n = 'M'
                                                   vbtyp_v = 'C'.

  IF sy-subrc IS NOT INITIAL.
*   "// se for Venda Futura
    READ TABLE lt_vbfa_equ INTO lw_vbfa_equ WITH KEY vbeln   = wa_vbak-vgbel
                                                     vbtyp_n = 'M'
                                                     vbtyp_v = 'L'.
  ENDIF.

  IF lt_vbfa_equ IS NOT INITIAL.

    SELECT * FROM zsdt0053
      INTO TABLE lt_zsdt0053_equ
    FOR ALL ENTRIES IN lt_vbfa_equ
    WHERE vbeln EQ lt_vbfa_equ-vbelv.

    IF lt_zsdt0053_equ IS NOT INITIAL.
      SELECT * FROM zsdt0051
        INTO TABLE @DATA(it_zsdt0051)
      FOR  ALL ENTRIES IN @lt_zsdt0053_equ
      WHERE nro_sol_ov EQ @lt_zsdt0053_equ-nro_sol_ov.
    ENDIF.

    IF  NOT lt_zsdt0053_equ IS INITIAL.
      DATA(_moeda) = it_zsdt0051[ 1 ]-waerk. "// Alterado para atender o CS2018001046 wbarbosa
      wa_vbap = it_vbap[ 1 ].
      dir_mi_in = 'MI'.
    ELSE.

      FREE it_insert.

      SELECT *
        FROM vbap
        INTO TABLE lt_vbap
          FOR ALL ENTRIES IN lt_vbfa_equ
          WHERE vbeln EQ lt_vbfa_equ-vbelv
            AND posnr EQ lt_vbfa_equ-posnv.

      IF NOT lt_vbap IS INITIAL.
        SELECT *
          FROM zsdt0041
          INTO TABLE lt_zsdt0041
            FOR ALL ENTRIES IN lt_vbap
            WHERE vbeln EQ lt_vbap-vbeln
              AND matnr EQ lt_vbap-matnr.

        IF NOT lt_zsdt0041 IS INITIAL.

          READ TABLE lt_zsdt0041 INTO lw_zsdt0041 WITH KEY vbeln = lw_vbfa_equ-vbelv.

          LOOP AT it_vbap INTO wa_vbap.

            wa_insert-doc    = lw_zsdt0041-doc_simulacao.
            wa_insert-vbeln  = wa_vbap-vbeln.
            wa_insert-vbelv  = lw_zsdt0041-vbeln.
            wa_insert-matnr  = wa_vbap-matnr.
            wa_insert-posnr  = lw_zsdt0041-posnr.
            wa_insert-charg  = wa_vbap-charg.

            APPEND wa_insert TO it_insert.

          ENDLOOP.

          dir_mi_in = 'IN'.
        ELSE.

          SELECT *
            FROM zsdt0090
            INTO TABLE lt_zsdt0090
              FOR ALL ENTRIES IN lt_vbap
              WHERE vbeln EQ lt_vbap-vbeln
                AND matnr EQ lt_vbap-matnr.

          IF NOT lt_zsdt0090 IS INITIAL.
            READ TABLE lt_zsdt0090 INTO lw_zsdt0090 WITH KEY vbeln = lw_vbfa_equ-vbelv.

            LOOP AT it_vbap INTO wa_vbap.

              wa_insert-doc    = lw_zsdt0090-doc_simulacao.
              wa_insert-vbeln  = wa_vbap-vbeln.
              wa_insert-vbelv  = lw_zsdt0090-vbeln.
              wa_insert-matnr  = wa_vbap-matnr.
              wa_insert-posnr  = lw_zsdt0090-posnn.
              wa_insert-charg  = wa_vbap-charg.

              APPEND wa_insert TO it_insert.
            ENDLOOP.

            dir_mi_in = 'IN'.
          ELSE.
            SELECT *
              FROM zlest0061
              INTO TABLE @DATA(lt_zlest0061)
                FOR ALL ENTRIES IN @lt_vbap
                WHERE nr_ov EQ @lt_vbap-vbeln.
            IF sy-subrc IS INITIAL.
              READ TABLE lt_zlest0061 INTO DATA(wa_zlest0061) INDEX 1.

              dir_mi_in = 'AQ'.

              SELECT SINGLE auart
                FROM vbak
                INTO vl_auart
                WHERE vbeln EQ wa_zlest0061-nr_ov.

            ENDIF.
*            SELECT * FROM sapsr3.zlest0061@sapecc WHERE nr_ov = '0012427576';
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*  READ TABLE lt_zsdt0053_equ INTO lw_zsdt0053_equ WITH KEY vbeln = lw_vbfa_equ-vbelv.
*  IF sy-subrc EQ 0.
  IF dir_mi_in EQ 'MI'.
    READ TABLE lt_zsdt0053_equ INTO lw_zsdt0053_equ WITH KEY vbeln = lw_vbfa_equ-vbelv.  "<<RIM-SKM-IR119065-29.11.22
    TRY.
        DATA(tp_venda) = it_zsdt0051[ nro_sol_ov = lw_zsdt0053_equ-nro_sol_ov ]-tp_venda .
      CATCH cx_sy_itab_line_not_found.
        CLEAR tp_venda.
    ENDTRY.

*&----Inicio ajuste Bug Solto 149379 / AOENNING& *
    CLEAR: vg_check_dispara_hedge.
    SELECT COUNT(*)
      FROM setleaf
      WHERE setname EQ 'MAGGI_ZSDT0062_HEDGE'
        AND valfrom EQ tp_venda.
    IF sy-subrc EQ 0.
      vg_check_dispara_hedge = abap_true.
    ENDIF.
*
*    CHECK sy-subrc IS INITIAL.
*    CHECK dir_mi_in EQ 'MI'.
*&----Fim ajuste Bug Solto 149379 / AOENNING& *

    CALL FUNCTION 'ENQUEUE_EZSDT0051'
      EXPORTING
        nro_sol_ov     = lw_zsdt0053_equ-nro_sol_ov
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF ( sy-ucomm EQ 'SICH' ).
      IF wa_vbak-auart IN r_auart. " Complemento e Devolução de Peso e Preço.

        CLEAR var_dir.
        IF wa_vbak-auart IN r_devo_recu.
          wa_vbap-brgew = wa_vbap-brgew * -1.
          var_dir = 'Y'.
        ELSEIF wa_vbak-auart IN r_comp.
          var_dir = 'W'.
        ENDIF.

        SELECT SINGLE * FROM zsdt0053 INTO lw_zsdt0053 WHERE vbeln EQ wa_vbak-vbeln AND status NE 'C'.
        IF ( sy-subrc NE 0 ).

          IF var_dir EQ 'Y'.
            SELECT * FROM vbfa
              INTO TABLE lt_vbfa
                WHERE vbeln   EQ wa_vbak-vgbel
                  AND vbtyp_n EQ 'M'
                  AND vbtyp_v IN ('C', 'L' ).

          ELSEIF var_dir EQ 'W'.
            SELECT * FROM vbfa
              INTO TABLE lt_vbfa
                WHERE vbeln   EQ wa_vbak-vbeln
                AND vbtyp_v IN ('C', 'L' )  "AJUSTE PARA CONTEMPLAR VENDA FUTURA Bug Solto 149379 / AOENNING& *
                AND vbtyp_n IN ('C', 'L' ).
          ENDIF.

          IF ( sy-subrc EQ 0 ).
            SELECT * FROM zsdt0053
              INTO TABLE lt_zsdt0053
              FOR ALL ENTRIES IN lt_vbfa
            WHERE vbeln EQ lt_vbfa-vbelv.

            IF ( sy-subrc EQ 0 ).
              READ TABLE lt_zsdt0053 INTO lw_zsdt0053 INDEX 1.

              SELECT * FROM zsdt0053
                INTO TABLE lt_zsdt0053_item
              WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov.

              SELECT * FROM zsdt0053
                INTO TABLE lt_zsdt0053_aux
              WHERE nro_sol_ov EQ lw_zsdt0053-nro_sol_ov
                AND fixacao    EQ lw_zsdt0053-fixacao.

*                   Venda Simples
              IF ( lw_zsdt0053-fixacao IS INITIAL ).
* Modificação - RIM-SKM-IR130621/IR134233 - 16.03.23 - Inicio
*                var_next_posnr = lines( lt_zsdt0053_item ).
**                READ TABLE lt_zsdt0053_item INTO lw_zsdt0053_item INDEX var_next_posnr.
*                var_next_posnr = ( var_next_posnr * 10 ) + 10.
                SORT lt_zsdt0053_item BY posnr DESCENDING.
                READ TABLE lt_zsdt0053_item INTO lw_zsdt0053_pos INDEX 1.
                var_next_posnr = lw_zsdt0053_pos-posnr + 10.
* Modificação - RIM-SKM-IR130621/IR134233 - 16.03.23 - Fim
              ELSE.
                READ TABLE lt_zsdt0053_item INTO lw_zsdt0053_item WITH KEY fixacao = lw_zsdt0053-fixacao
                                                                            status = var_dir
                                                                            charg  = lw_zsdt0053-charg.
                IF sy-subrc IS INITIAL.
                  var_next_posnr = lw_zsdt0053_item-posnr.
                ELSE.
* Modificação - RIM-SKM-IR130621/IR134233 - 16.03.23 - Inicio
*                  var_next_posnr = lines( lt_zsdt0053_item ).
**                  READ TABLE lt_zsdt0053_item INTO lw_zsdt0053_item INDEX var_next_posnr.
*                  var_next_posnr = ( var_next_posnr * 10 ) + 10.
                  SORT lt_zsdt0053_item BY posnr DESCENDING.
                  READ TABLE lt_zsdt0053_item INTO lw_zsdt0053_pos INDEX 1.
                  var_next_posnr = lw_zsdt0053_pos-posnr + 10.
* Modificação - RIM-SKM-IR130621/IR134233 - 16.03.23 - Fim
                  lw_zsdt0053_item-zmeng = 0.
                ENDIF.
              ENDIF.

              lw_zsdt0053-posnr           = var_next_posnr.
              lw_zsdt0053-doc_precedente  = lw_zsdt0053-vbeln.
              lw_zsdt0053-vbeln           = wa_vbak-vbeln.

              IF ( lw_zsdt0053-fixacao IS INITIAL ).
                lw_zsdt0053-zmeng           = wa_vbap-brgew.
              ELSE.
                lw_zsdt0053-zmeng           = lw_zsdt0053_item-zmeng + wa_vbap-brgew.
              ENDIF.

*&----Inicio ajuste Bug Solto 149379 / aoenning& / 22-08-2024 -----&*
              IF var_dir = 'Y'.
                IF ( lw_zsdt0053-zieme EQ 'KG' ) AND ( lw_zsdt0053-pmein EQ 'TO' ).
                  lw_zsdt0053-vlrtot = ( ( ( lw_zsdt0053-zmeng * lw_zsdt0053-dmbtr ) / 1000 ) ).
                ELSEIF ( lw_zsdt0053-zieme EQ 'TO' ) AND ( lw_zsdt0053-pmein EQ 'KG' ).
                  lw_zsdt0053-vlrtot = ( ( ( lw_zsdt0053-zmeng * lw_zsdt0053-dmbtr ) * 100 ) ).

                  " 29.08.2024 - 150168 - RAMON -->
                ELSEIF ( lw_zsdt0053-zieme EQ 'L' ) AND ( lw_zsdt0053-pmein EQ 'M3' ).
                  lw_zsdt0053-vlrtot = ( ( ( lw_zsdt0053-zmeng * lw_zsdt0053-dmbtr ) / 1000 ) ).
                ELSEIF ( lw_zsdt0053-zieme EQ 'M3' ) AND ( lw_zsdt0053-pmein EQ 'L' ).
                  lw_zsdt0053-vlrtot = ( ( ( lw_zsdt0053-zmeng * lw_zsdt0053-dmbtr ) * 100 ) ).
                  " 29.08.2024 - 150168 - RAMON <--

                ENDIF.
              ELSE.

              IF ( lw_zsdt0053-fixacao IS INITIAL ).
                lw_zsdt0053-vlrtot =  wa_vbap-netwr + wa_vbap-mwsbp.
              ELSE.
                lw_zsdt0053-vlrtot =  lw_zsdt0053-vlrtot + wa_vbap-netwr + wa_vbap-mwsbp. " para Atender os Complementos de Valor - Bug Solto 151247 - PQ
              ENDIF.

                lw_zsdt0053-vlrtot =  wa_vbap-netwr + wa_vbap-mwsbp. " para Atender os Complementos de Valor "ajuste Bug Solto 149379 / AOENNING ----&*
              ENDIF.
*&----Fim ajuste Bug Solto 149379 / aoenning& / 22-08-2024 -----&*

              lw_zsdt0053-fixacao  = lw_zsdt0053-fixacao.

*                   Venda Simples
              IF ( lw_zsdt0053-fixacao IS INITIAL ).
                SELECT * FROM zsdt0055
                  INTO TABLE lt_zsdt0055
                WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                  AND status NE 'C'.
                var_tabix = sy-dbcnt.
              ELSE.

*                    Venda Frame
                SELECT * FROM zsdt0055
                   INTO TABLE lt_zsdt0055
                 WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                   AND fixacao    = lw_zsdt0053-fixacao
                   AND status NE 'C'.
                var_tabix = sy-dbcnt.

                SELECT * FROM zsdt0073
                  INTO TABLE lt_zsdt0073
                 WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                   AND fixacao    = lw_zsdt0053-fixacao.

                IF ( sy-subrc EQ 0 ).
                  READ TABLE lt_zsdt0053_aux TRANSPORTING NO FIELDS WITH KEY fixacao = lw_zsdt0053-fixacao
                                                                             status = var_dir
                                                                             charg  = lw_zsdt0053-charg.
                  IF ( sy-subrc NE 0 ).
                    READ TABLE lt_zsdt0073 INTO lw_zsdt0073 INDEX 1.
                    lw_zsdt0073-qte_venc = lw_zsdt0073-qte_venc + 1.
                    UPDATE zsdt0073 SET qte_venc = lw_zsdt0073-qte_venc
                                  WHERE nro_sol_ov = lw_zsdt0073-nro_sol_ov
                                    AND fixacao    = lw_zsdt0073-fixacao.
                  ENDIF.
                ENDIF.
              ENDIF.

              READ TABLE lt_zsdt0055 INTO lw_zsdt0055 INDEX var_tabix.
              IF ( sy-subrc EQ 0 ).

                IF ( lw_zsdt0055-data_progr < sy-datum ).
                  lw_zsdt0055-data_progr = sy-datum + 30.
                ELSE.
                  lw_zsdt0055-data_progr = lw_zsdt0055-data_progr.
                ENDIF.

                IF vg_check_dispara_hedge EQ abap_true. "ajuste Bug Solto 149379 / AOENNING ----&*
                  lw_zsdt0055-status       = 'D'.
                ELSE.
                  lw_zsdt0055-status       = var_dir.  "ajuste Bug Solto 149379 / AOENNING ----&*
                ENDIF.
                lw_zsdt0055-cadencia_qte = wa_vbap-brgew.
                lw_zsdt0055-vbeln        = wa_vbap-vbeln.
                lw_zsdt0055-fixacao      = lw_zsdt0053-fixacao.

                CALL FUNCTION 'NUMBER_GET_NEXT'
                  EXPORTING
                    nr_range_nr = '01'
                    object      = 'ZSEQ_LOG'
                  IMPORTING
                    number      = lw_zsdt0055-id.

                INSERT INTO zsdt0055 VALUES lw_zsdt0055.

              ELSE.
                IF NOT ( lw_zsdt0053-valdt IS INITIAL ) AND ( lw_zsdt0053-valdt < sy-datum ).
                  lw_zsdt0053-valdt = sy-datum + 30.
                ELSE.
                  lw_zsdt0053-valdt = lw_zsdt0053-valdt.
                ENDIF.
              ENDIF.

              IF vg_check_dispara_hedge EQ abap_true.  "ajuste Bug Solto 149379 / AOENNING ----&*
                lw_zsdt0053-status          = 'D'.
              ELSE.
                lw_zsdt0053-status          = var_dir.  "ajuste Bug Solto 149379 / AOENNING ----&*
              ENDIF.

              lw_zsdt0053-job             = var_dir.
              lw_zsdt0053-data_atual      = sy-datum.
              lw_zsdt0053-hora_atual      = sy-uzeit.

              IF ( lw_zsdt0053-fixacao IS INITIAL ).
                INSERT INTO zsdt0053 VALUES lw_zsdt0053.
              ELSE.
                READ TABLE lt_zsdt0053_aux TRANSPORTING NO FIELDS WITH KEY fixacao = lw_zsdt0053-fixacao
                                                                           status = var_dir
                                                                           charg  = lw_zsdt0053-charg.
                IF ( sy-subrc EQ 0 ).
                  UPDATE zsdt0053 SET zmeng          = lw_zsdt0053-zmeng
                                      vlrtot         = lw_zsdt0053-vlrtot
                                      valdt          = lw_zsdt0053-valdt
                                      vbeln          = lw_zsdt0053-vbeln
                                      status         = lw_zsdt0053-status
                                      job            = lw_zsdt0053-job
                                      usnam          = sy-uname
                                      data_atual     = sy-datum
                                      hora_atual     = sy-uzeit
                                      doc_precedente = lw_zsdt0053-doc_precedente
                                      WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                                        AND posnr      = lw_zsdt0053-posnr
                                        AND fixacao    = lw_zsdt0053-fixacao.
                ELSE.
                  INSERT INTO zsdt0053 VALUES lw_zsdt0053.
                ENDIF.
              ENDIF.

              IF ( sy-subrc EQ 0 ).

                FREE: obj_zcl_webservice_taxa_curva.
                CREATE OBJECT obj_zcl_webservice_taxa_curva.

                TRY.
*                        Dispara o Hedge Vanda Simples
                    IF ( lw_zsdt0053-fixacao IS INITIAL ).
                      IF var_dir EQ 'Y'.

*&----Inicio ajuste Bug Solto 149379 / aoenning& / 22-08-2024 -----&*
*                        check sy-subrc is initial.
                        CHECK vg_check_dispara_hedge EQ abap_true.
*&----Fim ajuste Bug Solto 149379 / AOENNING ----&*

                        obj_zcl_webservice_taxa_curva->executar( i_numero = lw_zsdt0053-nro_sol_ov
                                                                 i_tipo   = 'ENC'
                                                                 "I_FIXACAO = LW_ZSDT0053-FIXACAO
                                                                 i_tcode  = 'VF01'
                                                                 i_status = 'Y'
                                                                 i_vbeln  = wa_vbap-vbeln
                                                                 ).

                      ELSEIF var_dir EQ 'W'.
                        obj_zcl_webservice_taxa_curva->executar( i_numero = lw_zsdt0053-nro_sol_ov
                                                                 i_tipo   = 'ENC'
                                                                 "I_FIXACAO = LW_ZSDT0053-FIXACAO
                                                                 i_tcode  = 'VF01'
                                                                 i_auart  = 'ZCPV'
                                                                 i_vbeln  = wa_vbap-vbeln
                                                                 i_vbap   = it_vbap "Inicio ajuste Bug Solto 149379 / aoenning& / 22-08-2024 -----&*
                                                                 ).

                      ENDIF.
                    ELSE.

*                         Dispara o Hedge Venda Frame
                      SELECT * FROM zsdt0056 INTO TABLE lt_zsdt0056.

                      IF ( sy-subrc EQ 0 ).

                        CLEAR: r_bezei_line.
                        r_bezei_line-sign   = 'I'.
                        r_bezei_line-option = 'EQ'.
                        r_bezei_line-low    = 'TAXA CAMBIO FRAME'.
                        r_bezei_line-high   = 'TAXA CAMBIO FRAME'.
                        APPEND r_bezei_line TO r_bezei.

                        CLEAR: c_bezei_line.
                        c_bezei_line-sign   = 'I'.
                        c_bezei_line-option = 'EQ'.
                        c_bezei_line-low    = 'CHICAGO FRAME'.
                        c_bezei_line-high   = 'CHICAGO FRAME'.
                        APPEND c_bezei_line TO c_bezei.

                        CLEAR: p_bezei_line.
                        p_bezei_line-sign   = 'I'.
                        p_bezei_line-option = 'EQ'.
                        p_bezei_line-low    = 'PREMIO FRAME'.
                        p_bezei_line-high   = 'PREMIO FRAME'.
                        APPEND p_bezei_line TO p_bezei.

                        " 29.10.2024 - 147331 - RAMON -->

                        CLEAR: b_bezei_line.
                        b_bezei_line-sign   = 'I'.
                        b_bezei_line-option = 'EQ'.
                        b_bezei_line-low    = 'BM&F FRAME'.
                        b_bezei_line-high   = 'BM&F FRAME'.
                        APPEND b_bezei_line TO b_bezei.

                        " 29.10.2024 - 147331 - RAMON --<

                        LOOP AT lt_zsdt0056 INTO lw_zsdt0056.
                          var_len  = strlen( lw_zsdt0056-bezei ).
                          CASE var_len.
                            WHEN: '2' OR '3'.

                              IF ( lw_zsdt0056-bezei(1) EQ 'T' ).
                                CLEAR: r_bezei_line.
                                r_bezei_line-sign   = 'I'.
                                r_bezei_line-option = 'EQ'.
                                r_bezei_line-low    = lw_zsdt0056-bezei.
                                r_bezei_line-high   = lw_zsdt0056-bezei.
                                APPEND r_bezei_line TO r_bezei.
                              ENDIF.

                              IF ( lw_zsdt0056-bezei(1) EQ 'C' ).
                                CLEAR: c_bezei_line.
                                c_bezei_line-sign   = 'I'.
                                c_bezei_line-option = 'EQ'.
                                c_bezei_line-low    = lw_zsdt0056-bezei.
                                c_bezei_line-high   = lw_zsdt0056-bezei.
                                APPEND c_bezei_line TO c_bezei.
                              ENDIF.

                              " 29.10.2024 - 147331 - RAMON -->
                              IF ( lw_zsdt0056-bezei(1) EQ 'B' ).
                                CLEAR: B_bezei_line.
                                B_bezei_line-sign   = 'I'.
                                B_bezei_line-option = 'EQ'.
                                B_bezei_line-low    = lw_zsdt0056-bezei.
                                B_bezei_line-high   = lw_zsdt0056-bezei.
                                APPEND B_bezei_line TO B_bezei.
                              ENDIF.

                              " 29.10.2024 - 147331 - RAMON --<


                              IF ( lw_zsdt0056-bezei(1) EQ 'P' ).
                                CLEAR: p_bezei_line.
                                p_bezei_line-sign   = 'I'.
                                p_bezei_line-option = 'EQ'.
                                p_bezei_line-low    = lw_zsdt0056-bezei.
                                p_bezei_line-high   = lw_zsdt0056-bezei.
                                APPEND p_bezei_line TO p_bezei.
                              ENDIF.

                            WHEN OTHERS.
                              CLEAR: lw_zsdt0056, var_len.
                              CONTINUE.
                          ENDCASE.
                          CLEAR: lw_zsdt0056, var_len.
                        ENDLOOP.
                      ENDIF.

*####  INICIO TAXA CAMBIO FRAME
*####  INICIO QTD FIXADA
                      SELECT * FROM zsdt0059
                         INTO TABLE lt_zsdt0059
                       WHERE nro_sol_ov  EQ lw_zsdt0053-nro_sol_ov
                         AND posnr       EQ lw_zsdt0053-fixacao
                         AND bezei       IN r_bezei.

*                          READ TABLE LT_ZSDT0053_AUX TRANSPORTING NO FIELDS WITH KEY STATUS = VAR_DIR.
*                          IF ( SY-SUBRC EQ 0 ).
                      LOOP AT lt_zsdt0059 INTO lw_zsdt0059 WHERE posnr1 EQ lw_zsdt0053-posnr.
                        var_bezei = lw_zsdt0059-bezei.
                      ENDLOOP.
*                          ELSE.
                      LOOP AT lt_zsdt0059 INTO lw_zsdt0059.
                        IF NOT ( lw_zsdt0059-valdt_hedge IS INITIAL ).
                          CONTINUE.
                        ELSEIF ( var_bezei IS INITIAL ).
                          var_bezei = lw_zsdt0059-bezei.
                        ENDIF.
                        CLEAR: lw_zsdt0059.
                      ENDLOOP.
*                          ENDIF.

                      var_formula = lw_zsdt0053-zmeng.
                      CONDENSE var_formula NO-GAPS.

                      UPDATE zsdt0059 SET formula      = var_formula
                                          formula2     = var_formula
                                          posnr1       = lw_zsdt0053-posnr
                                          monat        = lw_zsdt0059-monat
                                          valdt        = sy-datum
                                          usnam        = sy-uname
                                          data_atual   = sy-datum
                                          hora_atual   = sy-uzeit
                                          valdt_hedge  = '00000000'
                                      WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                                        AND posnr      = lw_zsdt0053-fixacao
                                        AND bezei      = var_bezei
                                        AND field      = 'QTDFIXADA'.

*                                REFRESH: LT_ZSDT0059[].
*####  FIM QTD FIXADA

*####  INICIO PRECO

                      READ TABLE lt_zsdt0059 INTO lw_zsdt0059 WITH KEY nro_sol_ov  = lw_zsdt0053-nro_sol_ov
                                                                       posnr       = lw_zsdt0053-fixacao
                                                                       bezei       = 'TAXA CAMBIO FRAME'
                                                                       field       = 'PRECO'.
                      CLEAR var_formula.
                      var_formula = lw_zsdt0059-formula2.
                      CONDENSE var_formula NO-GAPS.
                      CLEAR lw_zsdt0059.

                      READ TABLE lt_zsdt0059 INTO lw_zsdt0059 WITH KEY nro_sol_ov  = lw_zsdt0053-nro_sol_ov
                                                                       posnr       = lw_zsdt0053-fixacao
                                                                       bezei       = 'T1'
                                                                       field       = 'PRECO'.

                      IF ( sy-subrc EQ 0 ).
                        UPDATE zsdt0059 SET formula      = var_formula
                                            formula2     = var_formula
                                            posnr1       = lw_zsdt0053-posnr
                                            monat        = lw_zsdt0059-monat
                                            valdt        = sy-datum
                                            usnam        = sy-uname
                                            data_atual   = sy-datum
                                            hora_atual   = sy-uzeit
                                            valdt_hedge  = '00000000'
                                        WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                                          AND posnr      = lw_zsdt0053-fixacao
                                          AND bezei      = var_bezei
                                          AND field      = 'PRECO'.
                      ENDIF.

                      REFRESH: lt_zsdt0059[].
                      CLEAR: lw_zsdt0059.
*####  FIM PRECO
*####  FIM TAXA CAMBIO FRAME

*####  INICIO PREMIO FRAME
*####  INICIO QTD FIXADA
                      SELECT * FROM zsdt0059
                         INTO TABLE lt_zsdt0059
                       WHERE nro_sol_ov  EQ lw_zsdt0053-nro_sol_ov
                         AND posnr       EQ lw_zsdt0053-fixacao
                         AND bezei       IN p_bezei.

                      CLEAR: var_bezei.
                      LOOP AT lt_zsdt0059 INTO lw_zsdt0059 WHERE posnr1 EQ lw_zsdt0053-posnr.
                        var_bezei = lw_zsdt0059-bezei.
                      ENDLOOP.

                      LOOP AT lt_zsdt0059 INTO lw_zsdt0059.
                        IF NOT ( lw_zsdt0059-valdt IS INITIAL ).
                          CONTINUE.
                        ELSEIF ( var_bezei IS INITIAL ).
                          var_bezei = lw_zsdt0059-bezei.
                        ENDIF.
                        CLEAR: lw_zsdt0059.
                      ENDLOOP.

                      READ TABLE lt_zsdt0059 INTO lw_zsdt0059 WITH KEY bezei = 'P1'.
                      var_formula = lw_zsdt0053-zmeng.
                      CONDENSE var_formula NO-GAPS.

                      UPDATE zsdt0059 SET formula      = var_formula
                                          formula2     = var_formula
                                          posnr1       = lw_zsdt0053-posnr
                                          cbot         = lw_zsdt0059-cbot
                                          monat        = lw_zsdt0059-monat
                                          valdt        = sy-datum
                                          usnam        = sy-uname
                                          data_atual   = sy-datum
                                          hora_atual   = sy-uzeit
                                          valdt_hedge  = '00000000'
                                      WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                                        AND posnr      = lw_zsdt0053-fixacao
                                        AND bezei      = var_bezei
                                        AND field      = 'QTDFIXADA'.

*####  FIM QTD FIXADA

*####  INICIO PREÇO
                      READ TABLE lt_zsdt0059 INTO lw_zsdt0059 WITH KEY nro_sol_ov  = lw_zsdt0053-nro_sol_ov
                                                                       posnr       = lw_zsdt0053-fixacao
                                                                       bezei       = 'PREMIO FRAME'
                                                                       field       = 'PRECO'.
                      CLEAR var_formula.
                      var_formula = lw_zsdt0059-formula2.
                      CONDENSE var_formula NO-GAPS.
                      CLEAR lw_zsdt0059.

                      READ TABLE lt_zsdt0059 INTO lw_zsdt0059 WITH KEY nro_sol_ov  = lw_zsdt0053-nro_sol_ov
                                                                       posnr       = lw_zsdt0053-fixacao
                                                                       bezei       = 'P1'
                                                                       field       = 'PRECO'.

                      IF ( sy-subrc EQ 0 ).

                        UPDATE zsdt0059 SET formula      = var_formula
                                            formula2     = var_formula
                                            posnr1       = lw_zsdt0053-posnr
                                            cbot         = lw_zsdt0059-cbot
                                            monat        = lw_zsdt0059-monat
                                            valdt        = sy-datum
                                            usnam        = sy-uname
                                            data_atual   = sy-datum
                                            hora_atual   = sy-uzeit
                                            valdt_hedge  = '00000000'
                                        WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                                          AND posnr      = lw_zsdt0053-fixacao
                                          AND bezei      = var_bezei
                                          AND field      = 'PRECO'.
                      ENDIF.

                      REFRESH: lt_zsdt0059[].
                      CLEAR: lw_zsdt0059.
*####  FIM PRECO
*####  FIM PREMIO FRAME

*####  INICIO CHICAGO FRAME
*####  INICIO QTD FIXADA
                      SELECT * FROM zsdt0059
                         INTO TABLE lt_zsdt0059
                       WHERE nro_sol_ov  EQ lw_zsdt0053-nro_sol_ov
                         AND posnr       EQ lw_zsdt0053-fixacao
                         AND bezei       IN c_bezei.

                      CLEAR: var_bezei.
                      LOOP AT lt_zsdt0059 INTO lw_zsdt0059 WHERE posnr1 EQ lw_zsdt0053-posnr.
                        var_bezei = lw_zsdt0059-bezei.
                      ENDLOOP.

                      LOOP AT lt_zsdt0059 INTO lw_zsdt0059.
                        IF NOT ( lw_zsdt0059-valdt IS INITIAL ).
                          CONTINUE.
                        ELSEIF ( var_bezei IS INITIAL ).
                          var_bezei = lw_zsdt0059-bezei.
                        ENDIF.
                        CLEAR: lw_zsdt0059.
                      ENDLOOP.

                      READ TABLE lt_zsdt0059 INTO lw_zsdt0059 WITH KEY bezei = 'C1'.
                      var_formula = lw_zsdt0053-zmeng.
                      CONDENSE var_formula NO-GAPS.

                      UPDATE zsdt0059 SET formula      = var_formula
                                          formula2     = var_formula
                                          posnr1       = lw_zsdt0053-posnr
                                          cbot         = lw_zsdt0059-cbot
                                          monat        = lw_zsdt0059-monat
                                          valdt        = sy-datum
                                          usnam        = sy-uname
                                          data_atual   = sy-datum
                                          hora_atual   = sy-uzeit
                                          valdt_hedge  = '00000000'
                                      WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                                        AND posnr      = lw_zsdt0053-fixacao
                                        AND bezei      = var_bezei
                                        AND field      = 'QTDFIXADA'.

*####  FIM QTD FIXADA

*####  INICIO PRECO
                      READ TABLE lt_zsdt0059 INTO lw_zsdt0059 WITH KEY nro_sol_ov  = lw_zsdt0053-nro_sol_ov
                                                                       posnr       = lw_zsdt0053-fixacao
                                                                       bezei       = 'CHICAGO FRAME'
                                                                       field       = 'PRECO'.
                      CLEAR var_formula.
                      var_formula = lw_zsdt0059-formula2.
                      CONDENSE var_formula NO-GAPS.
                      CLEAR lw_zsdt0059.

                      READ TABLE lt_zsdt0059 INTO lw_zsdt0059 WITH KEY nro_sol_ov  = lw_zsdt0053-nro_sol_ov
                                                                       posnr       = lw_zsdt0053-fixacao
                                                                       bezei       = 'C1'
                                                                       field       = 'PRECO'.


                      IF ( sy-subrc EQ 0 ).

                        UPDATE zsdt0059 SET formula      = var_formula
                                            formula2     = var_formula
                                            posnr1       = lw_zsdt0053-posnr
                                            cbot         = lw_zsdt0059-cbot
                                            monat        = lw_zsdt0059-monat
                                            valdt        = sy-datum
                                            usnam        = sy-uname
                                            data_atual   = sy-datum
                                            hora_atual   = sy-uzeit
                                            valdt_hedge  = '00000000'
                                        WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                                          AND posnr      = lw_zsdt0053-fixacao
                                          AND bezei      = var_bezei
                                          AND field      = 'PRECO'.
                      ENDIF.

                      REFRESH: lt_zsdt0059[].
                      CLEAR: lw_zsdt0059.
*####  FIM PRECO
*####  FIM CHICAGO FRAME

" 29.10.2024 - 147331 - RAMON -->

*####  INICIO BM&F FRAME
*####  INICIO QTD FIXADA
                      SELECT * FROM zsdt0059
                         INTO TABLE lt_zsdt0059
                       WHERE nro_sol_ov  EQ lw_zsdt0053-nro_sol_ov
                         AND posnr       EQ lw_zsdt0053-fixacao
                         AND bezei       IN b_bezei.

                      CLEAR: var_bezei.
                      LOOP AT lt_zsdt0059 INTO lw_zsdt0059 WHERE posnr1 EQ lw_zsdt0053-posnr.
                        var_bezei = lw_zsdt0059-bezei.
                      ENDLOOP.

                      LOOP AT lt_zsdt0059 INTO lw_zsdt0059.
                        IF NOT ( lw_zsdt0059-valdt IS INITIAL ).
                          CONTINUE.
                        ELSEIF ( var_bezei IS INITIAL ).
                          var_bezei = lw_zsdt0059-bezei.
                        ENDIF.
                        CLEAR: lw_zsdt0059.
                      ENDLOOP.

                      READ TABLE lt_zsdt0059 INTO lw_zsdt0059 WITH KEY bezei = 'B1'.
                      var_formula = lw_zsdt0053-zmeng.
                      CONDENSE var_formula NO-GAPS.

                      UPDATE zsdt0059 SET formula      = var_formula
                                          formula2     = var_formula
                                          posnr1       = lw_zsdt0053-posnr
                                          cbot         = lw_zsdt0059-cbot
                                          monat        = lw_zsdt0059-monat
                                          valdt        = sy-datum
                                          usnam        = sy-uname
                                          data_atual   = sy-datum
                                          hora_atual   = sy-uzeit
                                          valdt_hedge  = '00000000'
                                      WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                                        AND posnr      = lw_zsdt0053-fixacao
                                        AND bezei      = var_bezei
                                        AND field      = 'QTDFIXADA'.

*####  FIM QTD FIXADA

*####  INICIO PRECO
                      READ TABLE lt_zsdt0059 INTO lw_zsdt0059 WITH KEY nro_sol_ov  = lw_zsdt0053-nro_sol_ov
                                                                       posnr       = lw_zsdt0053-fixacao
                                                                       bezei       = 'BM&F FRAME'
                                                                       field       = 'PRECO'.
                      CLEAR var_formula.
                      var_formula = lw_zsdt0059-formula2.
                      CONDENSE var_formula NO-GAPS.
                      CLEAR lw_zsdt0059.

                      READ TABLE lt_zsdt0059 INTO lw_zsdt0059 WITH KEY nro_sol_ov  = lw_zsdt0053-nro_sol_ov
                                                                       posnr       = lw_zsdt0053-fixacao
                                                                       bezei       = 'B1'
                                                                       field       = 'PRECO'.


                      IF ( sy-subrc EQ 0 ).

                        UPDATE zsdt0059 SET formula      = var_formula
                                            formula2     = var_formula
                                            posnr1       = lw_zsdt0053-posnr
                                            cbot         = lw_zsdt0059-cbot
                                            monat        = lw_zsdt0059-monat
                                            valdt        = sy-datum
                                            usnam        = sy-uname
                                            data_atual   = sy-datum
                                            hora_atual   = sy-uzeit
                                            valdt_hedge  = '00000000'
                                        WHERE nro_sol_ov = lw_zsdt0053-nro_sol_ov
                                          AND posnr      = lw_zsdt0053-fixacao
                                          AND bezei      = var_bezei
                                          AND field      = 'PRECO'.
                      ENDIF.

                      REFRESH: lt_zsdt0059[].
                      CLEAR: lw_zsdt0059.
*####  FIM PRECO
*####  FIM BM&F FRAME
" 29.10.2024 - 147331 - RAMON --<


*                         Insere na 0100 todas as alterações nos itens.
*######################## inicio
                      CALL FUNCTION 'NUMBER_GET_NEXT'
                        EXPORTING
                          nr_range_nr = '01'
                          object      = 'ZSEQ_HEDGE'
                        IMPORTING
                          number      = lw_zsdt0100-seq.

                      lw_zsdt0100-nro_sol_ov  = lw_zsdt0053-nro_sol_ov.
                      lw_zsdt0100-fixacao     = lw_zsdt0053-fixacao.
                      lw_zsdt0100-posnr       = lw_zsdt0053-posnr.
                      lw_zsdt0100-vbeln       = wa_vbak-vbeln.
                      lw_zsdt0100-auart       = wa_vbak-auart.
                      lw_zsdt0100-zmeng       = wa_vbap-brgew.
                      lw_zsdt0100-zieme       = wa_vbap-zieme.
                      lw_zsdt0100-vbelv       = wa_vbak-vgbel.
                      lw_zsdt0100-status      = var_dir.
                      lw_zsdt0100-usnam       = sy-uname.
                      lw_zsdt0100-data_atual  = sy-datum.
                      lw_zsdt0100-hora_atual  = sy-uzeit.

                      INSERT INTO zsdt0100 VALUES lw_zsdt0100.
*######################## Fim


*&----Inicio ajuste Bug Solto 149379 / aoenning& / 22-08-2024 -----&*

*                      check sy-subrc is initial.
                      CHECK vg_check_dispara_hedge EQ abap_true.
*&----Fim ajuste Bug Solto 149379 / AOENNING ----&*


                      obj_zcl_webservice_taxa_curva->executar( i_numero  = lw_zsdt0053-nro_sol_ov
                                                               i_tipo    = 'FRA'
                                                               i_fixacao = lw_zsdt0053-fixacao
                                                               i_tcode   = 'VF01'
                                                               i_vbeln   = lw_zsdt0053-vbeln
                                                               i_auart   = wa_vbak-auart
                                                               ).
                      IF _moeda EQ 'BRL'.
                        obj_zcl_webservice_taxa_curva->executar(  i_numero  = lw_zsdt0053-nro_sol_ov
                                                                  i_tipo    = 'FRE'
                                                                  i_fixacao = lw_zsdt0053-fixacao
*                                                                          I_TCODE   = 'ZSDT0062'
                                                                  i_tcode   = 'VF01'
                                                                  i_status  = 'X'
                                                                  i_vbeln   = lw_zsdt0053-vbeln
                                                                  i_auart   = wa_vbak-auart
                                                                  ).
                      ENDIF.
                    ENDIF.

                  CATCH zcx_webservice INTO cx_exception.
                    var_msg = cx_exception->get_text( ).
                    MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH var_msg.

                ENDTRY.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_EZSDT0051'
      EXPORTING
        nro_sol_ov = lw_zsdt0053_equ-nro_sol_ov.

  ELSEIF dir_mi_in EQ 'IN'.

    DATA mat_charg TYPE c LENGTH 50.

    CHECK dir_mi_in EQ 'IN'.

    IF ( sy-ucomm EQ 'SICH' ).
      CASE wa_vbak-auart.
        WHEN: 'ZROB' OR 'ZREB'
           OR 'ZRPF' OR 'ZRPF'.

          DATA: vl_trava_cambio TYPE c,
                wl_0040_aux     TYPE zsdt0040.

          CLEAR wa_insert.
          LOOP AT it_insert INTO wa_insert.

            SELECT SINGLE *
              FROM zsdt0040 INTO wl_0040_aux
             WHERE doc_simulacao EQ wa_insert-doc.

            CHECK sy-subrc = 0.

            SELECT COUNT(*) FROM zsdt0090
              WHERE vbeln EQ wa_insert-vbeln
              AND  matnr EQ wa_insert-matnr
              AND charg EQ wa_insert-charg
              AND categoria EQ var_dir
              AND estorno EQ abap_false.

            IF NOT sy-subrc IS INITIAL.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_insert-charg
                IMPORTING
                  output = wa_insert-charg.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_insert-matnr
                IMPORTING
                  output = wa_insert-matnr.

*                                   10 CARACTERES       18 CARACTERES
              mat_charg = |{ wa_insert-charg }#{ wa_insert-matnr }|.

              vcategoria = var_dir.

              CASE var_dir.
                WHEN 'Y' OR 'W'.
                  obj->insert_zsdt90(
                    EXPORTING
                      direcao   = vcategoria
                      simulador = wa_insert-doc    " Numero do documento de simulação de venda
                      ordem_old = wa_insert-vbelv   " Nº documento de vendas e distribuição
                      ordem_new = wa_insert-vbeln   " Nº documento de vendas e distribuição
                      material  = wa_insert-matnr  " Nº do material
                      charg     = wa_insert-charg   " Número do lote
                    RECEIVING
                      return    = p_0090
                  ).

                WHEN OTHERS.

                  PERFORM insert_zsdt0090(zsdr0042)
                   USING var_dir
                         wa_insert-doc      " DOC_SIMULACAO
                         wa_insert-vbeln    " NEW
                         wa_insert-vbelv    " OLD
                         mat_charg    " LOTE E MATERIAL
                   CHANGING p_0090.
              ENDCASE.


              MOVE-CORRESPONDING p_0090 TO im_0090.

              CLEAR: vl_trava_cambio.
              IF wl_0040_aux-waerk EQ 'USD'.
                CALL FUNCTION 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
                  EXPORTING
                    i_doc_simulacao = wa_insert-doc
                    i_vbeln         = wa_insert-vbelv
                  CHANGING
                    c_trava_cambio  = vl_trava_cambio.
              ENDIF.

              IF ( wl_0040_aux-waerk EQ 'BRL' ) OR

                 ( ( wl_0040_aux-waerk EQ 'USD' ) AND
                   ( vl_trava_cambio IS NOT INITIAL ) ).

                zcl_webservice_tx_curva=>hedge_insumos( i_0090 = im_0090
                                                        i_tipo = 'VDI'
                                                        i_dir  = var_dir
                                                        ).
              ENDIF.
              zcl_webservice_tx_curva=>hedge_insumos( i_0090 = im_0090
                                                      i_tipo = 'FRI'
                                                      i_dir  = var_dir
                                                      ).
              CLEAR: p_0090, im_0090.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDIF.

  ELSEIF dir_mi_in EQ 'AQ'.

    zcl_webservice_tx_curva=>hedge_aquaviario(
      _code  = sy-tcode
      _vbrk  = wa_vbrk
      _vbrp  = CONV #( wa_vbrp )
      _auart = vl_auart
    ).

  ENDIF.

ENDFUNCTION.
