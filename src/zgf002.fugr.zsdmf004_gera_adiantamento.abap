FUNCTION zsdmf004_gera_adiantamento.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      TI_NRO_SOL_OV STRUCTURE  ZSDS007
*"      TE_RETURN STRUCTURE  ZSDS008 OPTIONAL
*"----------------------------------------------------------------------
  DATA: tl_header       TYPE TABLE OF zsdt0051 WITH HEADER LINE,
        wl_header       TYPE zsdt0051,
        tl_pgt_ant      TYPE TABLE OF zsdt0054 WITH HEADER LINE,
        tl_adto_ext     TYPE TABLE OF zsdt0063 WITH HEADER LINE,
        tl_0036         TYPE TABLE OF zfit0036 WITH HEADER LINE,
        tl_itens        TYPE TABLE OF zsdt0053 WITH HEADER LINE,
        tl_cond_pgt     TYPE  TABLE OF zsdt0052 WITH HEADER LINE,
        tl_depara_cen   TYPE  TABLE OF zsdt_depara_cen WITH HEADER LINE,
        wl_0036         TYPE zfit0036,
        wl_data(10),
        wl_data_aux(10),
        wl_bktxt(30),
        wl_valor(16),
        wl_taxa(10),
        wl_zlsch        TYPE zsdt0052-zlsch,
        opt             TYPE ctu_params..

  IF ti_nro_sol_ov[] IS NOT INITIAL.
    SELECT  *
      FROM zsdt0051
      INTO TABLE tl_header
       FOR ALL ENTRIES IN ti_nro_sol_ov
       WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov
         AND status        EQ 'L'.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zsdt0052
        INTO TABLE tl_cond_pgt
        FOR ALL ENTRIES IN ti_nro_sol_ov
         WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.

      SELECT *
        FROM zsdt0054
        INTO TABLE tl_pgt_ant
        FOR ALL ENTRIES IN ti_nro_sol_ov
         WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov
           AND adiant     EQ space.

      SELECT *
        FROM zsdt0063
        INTO TABLE tl_adto_ext
        FOR ALL ENTRIES IN ti_nro_sol_ov
         WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov
           AND adiant     EQ space.

      SELECT *
        FROM zsdt0053
        INTO TABLE tl_itens
        FOR ALL ENTRIES IN ti_nro_sol_ov
         WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov
           AND posnr      EQ ti_nro_sol_ov-posnr
           AND vbeln      NE space.

      SELECT *
       FROM zsdt_depara_cen
       INTO TABLE tl_depara_cen
        FOR ALL ENTRIES IN tl_itens
         WHERE centrov_1 EQ tl_itens-werks.

    ENDIF.

    LOOP AT ti_nro_sol_ov.
      READ TABLE tl_header
        WITH KEY nro_sol_ov = ti_nro_sol_ov-nro_sol_ov.

      READ TABLE tl_cond_pgt
            WITH KEY nro_sol_ov = ti_nro_sol_ov-nro_sol_ov.

      IF tl_header-param_espec NE 'P'.
        LOOP AT tl_pgt_ant WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov
                             AND posnr      EQ ti_nro_sol_ov-posnr.

*          READ TABLE TL_HEADER
*            WITH KEY NRO_SOL_OV = TI_NRO_SOL_OV-NRO_SOL_OV.

*          READ TABLE TL_COND_PGT
*            WITH KEY NRO_SOL_OV = TL_PGT_ANT-NRO_SOL_OV.

          READ TABLE tl_itens
            WITH KEY nro_sol_ov = ti_nro_sol_ov-nro_sol_ov
                          posnr = ti_nro_sol_ov-posnr.

          IF sy-subrc IS INITIAL.
            READ TABLE tl_depara_cen
              WITH KEY centrov_1 = tl_itens-werks.

            IF sy-subrc IS INITIAL.
              tl_itens-werks = tl_depara_cen-centro_real.
            ENDIF.

            REFRESH: tg_bdc, tg_msg.
            CLEAR: wg_bdc.
            WRITE sy-datum TO wl_data.
            WRITE tl_pgt_ant-dmbtr TO wl_valor.
            CONDENSE wl_valor NO-GAPS.
            CONCATENATE 'Sol.' ti_nro_sol_ov-nro_sol_ov INTO wl_bktxt.
            IF tl_cond_pgt-pgto_ant EQ 'X'.
              wl_zlsch = 'D'.
            ELSEIF tl_cond_pgt-pgto_ant EQ 'N'.
              wl_zlsch = 'U'.
            ENDIF.


            PERFORM f_preencher_dynpro USING:
                   'X' 'SAPMF05A'                      '0113',
                   ' ' 'BKPF-BLDAT'                    wl_data,
                   ' ' 'BKPF-BLART'                    'DZ',
                   ' ' 'BKPF-BUKRS'                    tl_header-vkorg,
                   ' ' 'BKPF-BUDAT'                    wl_data,
                   ' ' 'BKPF-MONAT'                    wl_data+3(2),
                   ' ' 'BKPF-WAERS'                    tl_header-waerk,
                   ' ' 'BKPF-BKTXT'                    wl_bktxt,
                   ' ' 'BKPF-XBLNR'                    wl_bktxt,
                   ' ' 'RF05A-NEWKO'                   tl_header-kunnr,
                   ' ' 'RF05A-ZUMSK'                   'A',
                   ' ' 'BDC_OKCODE'                    '/00'.


            WRITE tl_pgt_ant-valdt TO  wl_data.
            PERFORM f_preencher_dynpro USING:
                   'X' 'SAPMF05A'                      '0304',
                   ' ' 'BSEG-WRBTR'                    wl_valor,
                   ' ' 'BSEG-GSBER'                    tl_itens-werks,
                   ' ' 'BSEG-ZFBDT'                    wl_data,
                   ' ' 'BSEG-ZLSCH'                    wl_zlsch, "'D', " tl_cond_pgt-ZLSCH,
                   ' ' 'BDC_OKCODE'                    '=ZK'.

            PERFORM f_preencher_dynpro USING:
                   'X' 'SAPMF05A'                      '0331',
                   ' ' 'BSEG-HBKID'                    tl_cond_pgt-hbkid,
                   ' ' 'BSEG-HZUON'                    tl_itens-vbeln,
                   ' ' 'BDC_OKCODE'                    '=BU'.

            opt-dismode = 'N'.
            opt-defsize = 'X'.
            CALL TRANSACTION 'F-37' USING tg_bdc
              OPTIONS FROM opt
              MESSAGES INTO tg_msg.

            te_return-nro_sol_ov = ti_nro_sol_ov-nro_sol_ov.
            te_return-posnr = ti_nro_sol_ov-posnr.
            te_return-zmeng = tl_itens-zmeng.
            te_return-valdt = tl_itens-valdt.
            te_return-vlrtot = tl_itens-vlrtot.
            te_return-vbeln = tl_itens-vbeln.

            PERFORM exibe_log TABLES te_return.

            READ TABLE tg_msg
              WITH KEY msgtyp = 'S'
                       msgnr  = '312'.
            IF  sy-subrc IS INITIAL.
              CONDENSE tg_msg-msgv1 NO-GAPS.
              tl_pgt_ant-adiant = tg_msg-msgv1.
              MODIFY zsdt0054 FROM tl_pgt_ant.

            ENDIF.
*                              USING TL_PGT_ANT.
          ENDIF.
        ENDLOOP.
*-CS2022000332-#78223-02.06.2022-JT-inicio
        IF tl_header-param_espec EQ 'A'.

          READ TABLE ti_nro_sol_ov WITH KEY nro_sol_ov = ti_nro_sol_ov-nro_sol_ov
                                            posnr      = 0.
          IF sy-subrc = 0.
            LOOP AT tl_adto_ext WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.
              CLEAR: tl_itens.
              READ TABLE tl_itens
              WITH KEY nro_sol_ov = tl_adto_ext-nro_sol_ov
                            posnr = tl_adto_ext-posnr.

              REFRESH: tg_bdc, tg_msg.
              CLEAR: wg_bdc.
              WRITE sy-datum TO wl_data.
              PERFORM f_preencher_dynpro USING:
                     'X' 'SAPMF05A'                      '0112',
                     ' ' 'BKPF-BLDAT'                    wl_data,
                     ' ' 'BKPF-BLART'                    'KA',
                     ' ' 'BKPF-BUKRS'                    tl_adto_ext-bukrs,
                     ' ' 'BKPF-BUDAT'                    wl_data,
                     ' ' 'BKPF-MONAT'                    wl_data+3(2),
                     ' ' 'BKPF-WAERS'                    tl_adto_ext-waers,
                     ' ' 'BKPF-XBLNR'                    wl_bktxt,
                     ' ' 'RF05A-NEWKO'                   tl_adto_ext-lifnr,
                     ' ' 'RF05A-ZUMSK'                   'A',
                     ' ' 'BDC_OKCODE'                    '/00'.

              WRITE tl_adto_ext-dmbtr TO wl_valor.
              CONDENSE wl_valor NO-GAPS.
              WRITE tl_adto_ext-valdt TO wl_data.
              CONCATENATE 'P.I.' tl_adto_ext-nr_provis_inv INTO DATA(l_zuonr).
              CONDENSE l_zuonr NO-GAPS.
              PERFORM f_preencher_dynpro USING:
                     'X' 'SAPMF05A'                      '0304',
                     ' ' 'BSEG-ZFBDT'                    wl_data,
                     ' ' 'BSEG-WRBTR'                    wl_valor,
                     ' ' 'BSEG-GSBER'                    '1501', "TL_HEADER-VKBUR,
                     ' ' 'BSEG-ZUONR'                    l_zuonr,
                     ' ' 'BDC_OKCODE'                    '=BU'.

              opt-dismode = 'N'.
              opt-defsize = 'X'.
              CALL TRANSACTION 'F-47' USING tg_bdc
                OPTIONS FROM opt
                MESSAGES INTO tg_msg.

              te_return-nro_sol_ov = ti_nro_sol_ov-nro_sol_ov.
              te_return-posnr = tl_adto_ext-posnr.
              te_return-zmeng = tl_itens-zmeng.
              te_return-valdt = tl_adto_ext-valdt.
              te_return-vlrtot = tl_adto_ext-dmbtr.
*          TE_RETURN-VBELN = TL_ITENS-VBELN.

              PERFORM exibe_log TABLES te_return.

              READ TABLE tg_msg
                WITH KEY msgtyp = 'S'
                         msgnr  = '312'.
              IF  sy-subrc IS INITIAL.
                CONDENSE tg_msg-msgv1 NO-GAPS.
                tl_adto_ext-adiant = tg_msg-msgv1.
                MODIFY tl_adto_ext FROM tl_adto_ext.
                MODIFY zsdt0063 FROM tl_adto_ext.
                IF sy-subrc IS INITIAL.
                  COMMIT WORK.
                ENDIF.

                REFRESH: tl_0036.
                CLEAR: tl_0036.

                CONCATENATE 'A' tl_adto_ext-adiant INTO tl_0036-obj_key.
                MOVE: tl_adto_ext-bukrs      TO tl_0036-bukrs,
                      tl_adto_ext-nro_sol_ov TO tl_0036-invoice,
                      tl_adto_ext-waers      TO tl_0036-moeda_pgto,
                      sy-datum               TO tl_0036-data_atual,
                      sy-uzeit               TO tl_0036-hora_atual,
                      sy-uname               TO tl_0036-usuario.

                SHIFT tl_0036-invoice LEFT DELETING LEADING '0'.
                APPEND tl_0036.
                CLEAR:tl_0036.

                IF tl_0036[] IS NOT INITIAL.
                  MODIFY zfit0036 FROM TABLE tl_0036.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ELSE.
            LOOP AT tl_adto_ext WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov
                                  AND posnr      EQ ti_nro_sol_ov-posnr.
              CLEAR: tl_itens.
              READ TABLE tl_itens
              WITH KEY nro_sol_ov = tl_adto_ext-nro_sol_ov
                            posnr = tl_adto_ext-posnr.

              REFRESH: tg_bdc, tg_msg.
              CLEAR: wg_bdc.
              WRITE sy-datum TO wl_data.
              PERFORM f_preencher_dynpro USING:
                     'X' 'SAPMF05A'                      '0112',
                     ' ' 'BKPF-BLDAT'                    wl_data,
                     ' ' 'BKPF-BLART'                    'KA',
                     ' ' 'BKPF-BUKRS'                    tl_adto_ext-bukrs,
                     ' ' 'BKPF-BUDAT'                    wl_data,
                     ' ' 'BKPF-MONAT'                    wl_data+3(2),
                     ' ' 'BKPF-WAERS'                    tl_adto_ext-waers,
                     ' ' 'BKPF-XBLNR'                    wl_bktxt,
                     ' ' 'RF05A-NEWKO'                   tl_adto_ext-lifnr,
                     ' ' 'RF05A-ZUMSK'                   'A',
                     ' ' 'BDC_OKCODE'                    '/00'.

              WRITE tl_adto_ext-dmbtr TO wl_valor.
              CONDENSE wl_valor NO-GAPS.
              WRITE tl_adto_ext-valdt TO wl_data.
              CONCATENATE 'P.I.' tl_adto_ext-nr_provis_inv INTO l_zuonr.
              CONDENSE l_zuonr NO-GAPS.
              PERFORM f_preencher_dynpro USING:
                     'X' 'SAPMF05A'                      '0304',
                     ' ' 'BSEG-ZFBDT'                    wl_data,
                     ' ' 'BSEG-WRBTR'                    wl_valor,
                     ' ' 'BSEG-GSBER'                    '1501', "TL_HEADER-VKBUR,
                     ' ' 'BSEG-ZUONR'                    l_zuonr,
                     ' ' 'BDC_OKCODE'                    '=BU'.

              opt-dismode = 'N'.
              opt-defsize = 'X'.
              CALL TRANSACTION 'F-47' USING tg_bdc
                OPTIONS FROM opt
                MESSAGES INTO tg_msg.

              te_return-nro_sol_ov = ti_nro_sol_ov-nro_sol_ov.
              te_return-posnr = tl_adto_ext-posnr.
              te_return-zmeng = tl_itens-zmeng.
              te_return-valdt = tl_adto_ext-valdt.
              te_return-vlrtot = tl_adto_ext-dmbtr.
*          TE_RETURN-VBELN = TL_ITENS-VBELN.

              PERFORM exibe_log TABLES te_return.

              READ TABLE tg_msg
                WITH KEY msgtyp = 'S'
                         msgnr  = '312'.
              IF  sy-subrc IS INITIAL.
                CONDENSE tg_msg-msgv1 NO-GAPS.
                tl_adto_ext-adiant = tg_msg-msgv1.
                MODIFY tl_adto_ext FROM tl_adto_ext.
                MODIFY zsdt0063 FROM tl_adto_ext.
                IF sy-subrc IS INITIAL.
                  COMMIT WORK.
                ENDIF.

                REFRESH: tl_0036.
                CLEAR: tl_0036.

                CONCATENATE 'A' tl_adto_ext-adiant INTO tl_0036-obj_key.
                MOVE: tl_adto_ext-bukrs      TO tl_0036-bukrs,
                      tl_adto_ext-nro_sol_ov TO tl_0036-invoice,
                      tl_adto_ext-waers      TO tl_0036-moeda_pgto,
                      sy-datum               TO tl_0036-data_atual,
                      sy-uzeit               TO tl_0036-hora_atual,
                      sy-uname               TO tl_0036-usuario.

                SHIFT tl_0036-invoice LEFT DELETING LEADING '0'.
                APPEND tl_0036.
                CLEAR:tl_0036.

                IF tl_0036[] IS NOT INITIAL.
                  MODIFY zfit0036 FROM TABLE tl_0036.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
*-CS2022000332-#78223-02.06.2022-JT-inicio
      ELSE.
        LOOP AT tl_pgt_ant
          WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.

          CLEAR: tl_itens.
          READ TABLE tl_itens
            WITH KEY nro_sol_ov = tl_pgt_ant-nro_sol_ov
                          posnr = tl_pgt_ant-posnr.

*        IF SY-SUBRC IS INITIAL.
          REFRESH: tg_bdc, tg_msg.
          CLEAR: wg_bdc, wl_taxa.
          WRITE sy-datum TO wl_data.
          WRITE tl_pgt_ant-dmbtr TO wl_valor.
          CONDENSE wl_valor NO-GAPS.
          CONCATENATE 'Sol.' ti_nro_sol_ov-nro_sol_ov INTO wl_bktxt.
          PERFORM f_preencher_dynpro USING:
                 'X' 'SAPMF05A'                      '0113',
                 ' ' 'BKPF-BLDAT'                    wl_data,
                 ' ' 'BKPF-BLART'                    'DZ',
                 ' ' 'BKPF-BUKRS'                    tl_header-vkorg,
                 ' ' 'BKPF-BUDAT'                    wl_data,
                 ' ' 'BKPF-MONAT'                    wl_data+3(2),
                 ' ' 'BKPF-WAERS'                    tl_header-waerk,
                 ' ' 'BKPF-BKTXT'                    wl_bktxt,
                 ' ' 'BKPF-XBLNR'                    wl_bktxt,
                 ' ' 'RF05A-NEWKO'                   tl_header-kunnr,
                 ' ' 'RF05A-ZUMSK'                   'A',
                 ' ' 'BDC_OKCODE'                    '/00'.

          IF tl_cond_pgt-pgto_ant EQ 'X'.
            WRITE tl_pgt_ant-kursf TO wl_taxa.
            CONDENSE wl_taxa NO-GAPS.
            PERFORM f_preencher_dynpro USING:
                  ' ' 'BKPF-KURSF'                    wl_taxa.
          ENDIF.

          WRITE tl_pgt_ant-valdt TO  wl_data.
          PERFORM f_preencher_dynpro USING:
                 'X' 'SAPMF05A'                      '0304',
                 ' ' 'BSEG-WRBTR'                    wl_valor,
                 ' ' 'BSEG-GSBER'                    tl_header-vkbur,
                 ' ' 'BSEG-ZUONR'                    'PERFORMANCE',
                 ' ' 'BSEG-ZFBDT'                    wl_data,
                 ' ' 'BDC_OKCODE'                    '=ZK'.

          PERFORM f_preencher_dynpro USING:
                 'X' 'SAPMF05A'                      '0331',
                 ' ' 'BSEG-HBKID'                    tl_cond_pgt-hbkid,
                 ' ' 'BDC_OKCODE'                    '=BU'.

          opt-dismode = 'N'.
          opt-defsize = 'X'.
          CALL TRANSACTION 'F-37' USING tg_bdc
            OPTIONS FROM opt
            MESSAGES INTO tg_msg.

          te_return-nro_sol_ov = ti_nro_sol_ov-nro_sol_ov.
          te_return-posnr = tl_pgt_ant-posnr.
          te_return-zmeng = tl_itens-zmeng.
          te_return-valdt = tl_pgt_ant-valdt.
          te_return-vlrtot = tl_pgt_ant-dmbtr.
*          TE_RETURN-VBELN = TL_ITENS-VBELN.

          PERFORM exibe_log TABLES te_return.

          READ TABLE tg_msg
            WITH KEY msgtyp = 'S'
                     msgnr  = '312'.
          IF  sy-subrc IS INITIAL.
            CONDENSE tg_msg-msgv1 NO-GAPS.
            tl_pgt_ant-adiant = tg_msg-msgv1.
            MODIFY zsdt0054 FROM tl_pgt_ant.

            CONCATENATE 'PB' tl_pgt_ant-adiant INTO wl_0036-obj_key.
            wl_0036-bukrs     = tl_header-vkorg.
            wl_0036-invoice   = tl_header-nro_sol_ov.
            SHIFT wl_0036-invoice LEFT DELETING LEADING '0'.
            wl_0036-moeda_pgto = tl_header-waerk.
            wl_0036-data_atual = sy-datum.
            wl_0036-hora_atual = sy-uzeit.
            wl_0036-usuario    = sy-uname.

            MODIFY zfit0036 FROM wl_0036.
          ENDIF.
*        ENDIF.
        ENDLOOP.

        LOOP AT tl_adto_ext
           WHERE nro_sol_ov EQ ti_nro_sol_ov-nro_sol_ov.

          CLEAR: tl_itens.
          READ TABLE tl_itens
          WITH KEY nro_sol_ov = tl_adto_ext-nro_sol_ov
                        posnr = tl_adto_ext-posnr.

          REFRESH: tg_bdc, tg_msg.
          CLEAR: wg_bdc.
          WRITE sy-datum TO wl_data.
          PERFORM f_preencher_dynpro USING:
                 'X' 'SAPMF05A'                      '0112',
                 ' ' 'BKPF-BLDAT'                    wl_data,
                 ' ' 'BKPF-BLART'                    'KA',
                 ' ' 'BKPF-BUKRS'                    tl_adto_ext-bukrs,
                 ' ' 'BKPF-BUDAT'                    wl_data,
                 ' ' 'BKPF-MONAT'                    wl_data+3(2),
                 ' ' 'BKPF-WAERS'                    tl_adto_ext-waers,
                 ' ' 'BKPF-XBLNR'                    wl_bktxt,
                 ' ' 'RF05A-NEWKO'                   tl_adto_ext-lifnr,
                 ' ' 'RF05A-ZUMSK'                   'A',
                 ' ' 'BDC_OKCODE'                    '/00'.

          WRITE tl_adto_ext-dmbtr TO wl_valor.
          CONDENSE wl_valor NO-GAPS.
          WRITE tl_adto_ext-valdt TO wl_data.
          PERFORM f_preencher_dynpro USING:
                 'X' 'SAPMF05A'                      '0304',
                 ' ' 'BSEG-ZFBDT'                    wl_data,
                 ' ' 'BSEG-WRBTR'                    wl_valor,
                 ' ' 'BSEG-GSBER'                    '0401', "TL_HEADER-VKBUR,
                 ' ' 'BSEG-ZUONR'                    'PERFORMANCE',
                 ' ' 'BDC_OKCODE'                    '=BU'.

          opt-dismode = 'N'.
          opt-defsize = 'X'.
          CALL TRANSACTION 'F-47' USING tg_bdc
            OPTIONS FROM opt
            MESSAGES INTO tg_msg.

          te_return-nro_sol_ov = ti_nro_sol_ov-nro_sol_ov.
          te_return-posnr = tl_adto_ext-posnr.
          te_return-zmeng = tl_itens-zmeng.
          te_return-valdt = tl_adto_ext-valdt.
          te_return-vlrtot = tl_adto_ext-dmbtr.
*          TE_RETURN-VBELN = TL_ITENS-VBELN.

          PERFORM exibe_log TABLES te_return.

          READ TABLE tg_msg
            WITH KEY msgtyp = 'S'
                     msgnr  = '312'.
          IF  sy-subrc IS INITIAL.
            CONDENSE tg_msg-msgv1 NO-GAPS.
            tl_adto_ext-adiant = tg_msg-msgv1.
            MODIFY zsdt0063 FROM tl_adto_ext.

            REFRESH: tl_0036.
            CLEAR: tl_0036.

            CONCATENATE 'P' tl_adto_ext-adiant INTO tl_0036-obj_key.
            MOVE: tl_adto_ext-bukrs      TO tl_0036-bukrs,
                  tl_adto_ext-nro_sol_ov TO tl_0036-invoice,
                  tl_adto_ext-waers      TO tl_0036-moeda_pgto,
                  sy-datum               TO tl_0036-data_atual,
                  sy-uzeit               TO tl_0036-hora_atual,
                  sy-uname               TO tl_0036-usuario.

            SHIFT tl_0036-invoice LEFT DELETING LEADING '0'.
            APPEND tl_0036.
            CLEAR:tl_0036.

            IF tl_0036[] IS NOT INITIAL.
              MODIFY zfit0036 FROM TABLE tl_0036.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFUNCTION.
