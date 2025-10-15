FUNCTION zhcmf_return_batidas_new.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(PERNR_SOLICITANTE) LIKE  P0002-PERNR OPTIONAL
*"     VALUE(PERNR_FUNC) LIKE  P0002-PERNR OPTIONAL
*"     VALUE(CNAME) LIKE  P0002-CNAME OPTIONAL
*"     VALUE(STAT2) LIKE  P0000-STAT2 OPTIONAL
*"     VALUE(BEGDA) LIKE  P0002-BEGDA
*"     VALUE(ENDDA) LIKE  P0002-ENDDA
*"     VALUE(TIPO) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      T_SAIDA STRUCTURE  ZHCMS_BATIDA_LIST
*"      T_OBJID STRUCTURE  ZHCMS_OBJID
*"      T_AREARH STRUCTURE  ZHCMS_WERKS OPTIONAL
*"      T_KOSTL STRUCTURE  ZHCMS_KOSTL OPTIONAL
*"----------------------------------------------------------------------

  RANGES: rg_uniorg FOR zhcmt_pa_0031-cod_uniorg,
          rg_abkrs  FOR pa0001-abkrs.
  RANGES: rg_datum FOR pc2ba-datum,
          rg_pernr_aux FOR p0001-pernr.

  DATA: l_it_0001      TYPE TABLE OF p0001,
        l_0001         LIKE LINE OF l_it_0001,
        it_0007        TYPE TABLE OF p0007,
        wa_0007        LIKE LINE OF it_0007,
        it_0008        TYPE TABLE OF p0008,
        wa_0008        LIKE LINE OF it_0008,
        it_2003        TYPE TABLE OF p2003,
        wa_2003        LIKE LINE OF it_2003,
        wa_batida_list LIKE t_saida,
        wa_saida_tab   LIKE t_saida,
        v_day          TYPE c LENGTH 2,
        l_anzhl_12x36  TYPE pc2b6-anzhl,
        it_func_list   TYPE TABLE OF zhcms_func_list,
        wl_func_list   LIKE LINE OF it_func_list.

  DATA: t_abkrs TYPE TABLE OF  zhcms_abkrs,
        w_abkrs TYPE zhcms_abkrs.

  TYPES: BEGIN OF ty_zes,
           reday TYPE pc2b6-reday,
           ztart TYPE pc2b6-ztart,
           anzhl TYPE pc2b6-anzhl,
           datum TYPE datum,
         END OF ty_zes,

         ty_rg_pernr TYPE RANGE OF pa0001-pernr.

  DATA: time_results    TYPE ptm_time_results OCCURS 0 WITH HEADER LINE,
        time_results_wa LIKE LINE OF time_results,
        zes             TYPE TABLE OF ty_zes,
        zes_ini         LIKE pc2b6 OCCURS 0 WITH HEADER LINE,
        psp             LIKE pc2ba OCCURS 0 WITH HEADER LINE,
        pt              LIKE pdcpt OCCURS 0 WITH HEADER LINE,
        ab              LIKE pc20i OCCURS 0 WITH HEADER LINE,
        zl              TYPE TABLE OF pc2bf,
        wa_psp          LIKE LINE OF psp,
        wa_zes          LIKE LINE OF zes,
        wa_zes_ini      LIKE LINE OF zes_ini,
        wa_pt           LIKE LINE OF pt,
        wa_ab           LIKE LINE OF ab,
        wa_zl           TYPE pc2bf,
        count           TYPE i,
        w001p           TYPE t001p,
        v_text          TYPE tholt-ktext,
        v_ltext         TYPE tholt-ltext,
        v_daysem        TYPE scal-indicator,
        v_atraso_banco  TYPE i,
        v_base          TYPE i,
        v_hora          TYPE c LENGTH 8,
        v_hora_12x36    TYPE c LENGTH 8,
        v_time          TYPE c LENGTH 8,
        v_time_ent      TYPE c LENGTH 8,
        v_time_sai      TYPE c LENGTH 8,
        v_hora_ent      TYPE c LENGTH 8,
        v_hora_sai      TYPE c LENGTH 8,
        v_hora_e        TYPE pc2b6-anzhl,
        v_hora_s        TYPE pc2b6-anzhl,
        v_hora_not(2)   TYPE n,
        v_hora_not_c(2) TYPE c,
        v_mofid         TYPE p2003-mofid,
        v_tjornada      TYPE dbnum,
        v_anzhl         TYPE pc2b6-anzhl.

  DATA: it_pernr     TYPE TABLE OF zhcms_ret_pernr.

  CHECK tipo IS NOT INITIAL.

  IF pernr_func IS NOT INITIAL.
    APPEND
    VALUE #(
             sign   = 'I'
             option = 'EQ'
             low    = pernr_func
           ) TO rg_pernr_aux.
  ENDIF.

  CASE tipo.
    WHEN 1.

      CALL FUNCTION 'ZHCMF_RETURN_FUNC'
        EXPORTING
          stat2    = stat2
          pernr    = pernr_func
          cname    = cname
        TABLES
          t_saida  = it_func_list
          t_objid  = t_objid[]
          t_arearh = t_arearh[]
          t_kostl  = t_kostl[]
          t_pernr  = it_pernr[].

    WHEN 2.

      SELECT 'I' AS sign,
             'EQ' AS option,
             cod_uniorg AS low
        FROM zhcmt_pa_0031
          INTO TABLE @rg_uniorg
          WHERE matricula EQ @pernr_solicitante.

      CHECK rg_uniorg[] IS NOT INITIAL.

      SELECT pernr
        FROM pa0001
        INTO TABLE @it_pernr
        WHERE endda >= @sy-datum
          AND orgeh IN @rg_uniorg
          AND pernr IN @rg_pernr_aux
          AND plans <> '99999999'.

      CALL FUNCTION 'ZHCMF_RETURN_FUNC'
        EXPORTING
          stat2    = stat2
          pernr    = pernr_func
          cname    = cname
        TABLES
          t_saida  = it_func_list
          t_objid  = t_objid[]
          t_arearh = t_arearh[]
          t_kostl  = t_kostl[]
          t_pernr  = it_pernr[].

    WHEN 3.

      SELECT SINGLE abkrs
        FROM pa0001
        INTO @DATA(vl_abkrs)
        WHERE pernr EQ @pernr_solicitante
        AND endda >= @sy-datum.

      CHECK sy-subrc IS INITIAL.


* BUG - 101495 - CBRAND - Inicio
      rg_abkrs-sign   = 'I'.
      rg_abkrs-option = 'EQ'.
      rg_abkrs-low    = vl_abkrs.
      APPEND rg_abkrs.
      CLEAR: rg_abkrs.

      w_abkrs-abkrs    = vl_abkrs.
      APPEND w_abkrs  TO t_abkrs.

      CASE vl_abkrs.
        WHEN '01' .
          rg_abkrs-sign = 'I'.
          rg_abkrs-option = 'EQ'.
          rg_abkrs-low = 'A2'.
          APPEND rg_abkrs.
          CLEAR: rg_abkrs.

          w_abkrs-abkrs    = 'A2'.
          APPEND w_abkrs  TO t_abkrs.

        WHEN 'A2'.
          rg_abkrs-sign = 'I'.
          rg_abkrs-option = 'EQ'.
          rg_abkrs-low = '01'.
          APPEND rg_abkrs.
          CLEAR: rg_abkrs.

          w_abkrs-abkrs    = '01'.
          APPEND w_abkrs  TO t_abkrs.
        WHEN '10'.
          rg_abkrs-sign = 'I'.
          rg_abkrs-option = 'EQ'.
          rg_abkrs-low = '11'.
          APPEND rg_abkrs.
          CLEAR: rg_abkrs.

          w_abkrs-abkrs    = '11'.
          APPEND w_abkrs  TO t_abkrs.
        WHEN '11'.
          rg_abkrs-sign = 'I'.
          rg_abkrs-option = 'EQ'.
          rg_abkrs-low = '10'.
          APPEND rg_abkrs.
          CLEAR: rg_abkrs.

          w_abkrs-abkrs    = '10'.
          APPEND w_abkrs  TO t_abkrs.
      ENDCASE.

* BUG - 101495 - CBRAND - Inicio

      SELECT 'I'   AS sign,
             'EQ'  AS option,
             orgeh AS low
        FROM pa0001
        INTO TABLE @rg_uniorg
        WHERE abkrs IN @rg_abkrs "= @vl_abkrs
          AND endda >= @sy-datum
          AND plans <> '99999999'.


      CHECK rg_uniorg[] IS NOT INITIAL.

      SELECT pernr
        FROM pa0001
        INTO TABLE @it_pernr
        WHERE endda >= @sy-datum
          AND orgeh IN @rg_uniorg
          AND pernr IN @rg_pernr_aux
          AND plans <> '99999999'.

      CALL FUNCTION 'ZHCMF_RETURN_FUNC'
        EXPORTING
          stat2    = stat2
          pernr    = pernr_func
          cname    = cname
        TABLES
          t_saida  = it_func_list
          t_objid  = t_objid[]
          t_arearh = t_arearh[]
          t_kostl  = t_kostl[]
          t_pernr  = it_pernr[]
          t_abkrs  = t_abkrs[].


  ENDCASE.

  CHECK  it_func_list[] IS NOT INITIAL.

  DATA(rg_pernr) = VALUE ty_rg_pernr(
    FOR w_atual IN it_func_list[] ( sign = 'I'
                                    option = 'EQ'
                                    low = w_atual-pernr )  ).
  SORT rg_pernr BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM rg_pernr COMPARING low.

  SELECT p1~pernr, p1~stell
    FROM pa0001 AS p1
    INTO TABLE @DATA(it_0001)
    WHERE p1~pernr IN @rg_pernr AND
          p1~endda >= @endda AND
          p1~begda <= @endda
  ORDER BY pernr ASCENDING.

  SELECT * FROM pa0007
      INTO TABLE @DATA(lt_pa0007)
      WHERE pernr IN @rg_pernr[]
        AND endda >= @endda
        AND begda <= @endda
        AND zterf = 1
   ORDER BY pernr ASCENDING.

  SELECT * FROM pa0008
     INTO TABLE @DATA(lt_pa0008)
     WHERE pernr IN @rg_pernr[]
       AND endda >= @endda
       AND begda <= @endda
       AND subty <> 'BR01'
  ORDER BY pernr ASCENDING.

  SELECT * FROM pa2003
     INTO TABLE @DATA(lt_pa2003)
     WHERE pernr IN @rg_pernr[]
       AND endda >= @endda
       AND begda <= @endda
  ORDER BY pernr ASCENDING.

  SELECT schkz, rtext FROM t508s
     INTO TABLE @DATA(lt_t508s)
          WHERE zeity = 1
            AND mofid = '2A'
            AND sprsl = @sy-langu
            AND mosid = 37
ORDER BY schkz ASCENDING.


*======================================================================*
*** Loop por pernr - Aqui começa todo o processo de batidas
*======================================================================*

  LOOP AT it_func_list INTO wl_func_list.

    CLEAR: wa_saida_tab, l_it_0001.

    READ TABLE lt_pa0007[] ASSIGNING FIELD-SYMBOL(<lfs_pa0007>) WITH KEY pernr = wl_func_list-pernr BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_saida_tab-jornada  = <lfs_pa0007>-mostd.
    ELSE.
      CONTINUE.
    ENDIF.

    wa_saida_tab-cname       = wl_func_list-cname.
    wa_saida_tab-cpf_nr      = wl_func_list-cpf_nr.
    wa_saida_tab-email       = wl_func_list-email.
    wa_saida_tab-pernr       = wl_func_list-pernr.
    wa_saida_tab-persk       = wl_func_list-persk .
    wa_saida_tab-ptext       = wl_func_list-ptext.
    wa_saida_tab-bukrs       = wl_func_list-bukrs.
    wa_saida_tab-empresa     = wl_func_list-empresa.
    wa_saida_tab-area_folha  = wl_func_list-abkrs.
    wa_saida_tab-werks       = wl_func_list-werks.
    wa_saida_tab-arearh      = wl_func_list-arearh.
    wa_saida_tab-stat2       = wl_func_list-stat2.
    wa_saida_tab-situacao    = wl_func_list-situacao.
    wa_saida_tab-orgeh       = wl_func_list-orgeh.
    wa_saida_tab-uniorg      = wl_func_list-uniorg.

    READ TABLE it_0001[] ASSIGNING FIELD-SYMBOL(<lfs_0001>) WITH KEY pernr = wl_func_list-pernr BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_saida_tab-cargo = <lfs_0001>-stell.
    ENDIF.

    wa_saida_tab-plans    = wl_func_list-plans.
    wa_saida_tab-posicao  = wl_func_list-posicao.
    wa_saida_tab-kostl    = wl_func_list-kostl.
    wa_saida_tab-ccusto   = wl_func_list-ccusto.
    wa_saida_tab-dat01    = wl_func_list-dat01.

    READ TABLE lt_pa0008[] ASSIGNING FIELD-SYMBOL(<lfs_0008>) WITH KEY pernr = wl_func_list-pernr BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_saida_tab-salario  = <lfs_0008>-bet01.
    ENDIF.

    READ TABLE lt_pa2003[] ASSIGNING FIELD-SYMBOL(<lfs_2003>) WITH KEY pernr = wl_func_list-pernr BINARY SEARCH.
    IF ( sy-subrc = 0 ). " AND ( wa_2003-endda >= begda AND wa_2003-endda >= endda ).

      wa_saida_tab-schkz    = <lfs_2003>-schkz.
      READ TABLE lt_t508s[] ASSIGNING FIELD-SYMBOL(<lfs_t508s>) WITH KEY schkz = <lfs_2003>-schkz BINARY SEARCH.
      IF ( sy-subrc = 0 ).
        wa_saida_tab-horario = <lfs_t508s>-rtext.
      ENDIF.

    ELSE.
      wa_saida_tab-schkz    = wl_func_list-schkz.
      wa_saida_tab-horario =  wl_func_list-horario.
    ENDIF.

*======================================================================*
*** Calculo de batidas
*======================================================================*

    CLEAR: time_results[].
    CALL FUNCTION 'HR_TIME_RESULTS_IN_INTERVAL'
      EXPORTING
        int_pernr             = wl_func_list-pernr
        int_begda             = begda "PARAMETRO
        int_endda             = endda "PARAMETRO
      TABLES
        int_time_results      = time_results
      EXCEPTIONS
        no_period_specified   = 1
        wrong_cluster_version = 2
        no_read_authority     = 3
        cluster_archived      = 4
        technical_error       = 5
        OTHERS                = 6.

    REFRESH: zes,
             zes_ini,
             psp,
             pt,
             ab,
             zl.

    CLEAR:   time_results_wa.

*======================================================================*
*** LOOP FINAL COM OS RESULTADOS DO CLUSTER
*======================================================================*

    LOOP AT time_results INTO time_results_wa.
      REFRESH zes_ini.

      APPEND LINES OF time_results_wa-zes   TO zes_ini.
      APPEND LINES OF time_results_wa-psp   TO psp.
      APPEND LINES OF time_results_wa-pt    TO pt.
      APPEND LINES OF time_results_wa-ab    TO ab.
      APPEND LINES OF time_results_wa-zl    TO zl.

      LOOP AT zes_ini INTO wa_zes_ini.

        wa_zes-reday = wa_zes_ini-reday.
        wa_zes-ztart = wa_zes_ini-ztart.
        wa_zes-anzhl = wa_zes_ini-anzhl.

        CONCATENATE time_results_wa-pabrj time_results_wa-pabrp wa_zes_ini-reday INTO wa_zes-datum.

        APPEND  wa_zes TO zes.

      ENDLOOP.
      CLEAR: time_results_wa, zes_ini, wa_zes_ini, wa_zes .

    ENDLOOP.

    "Pegar somente o range de datas informado na seleção
    rg_datum-sign   = 'I'.
    rg_datum-option = 'BT'.
    rg_datum-low   = begda.
    rg_datum-high  = endda.
    APPEND rg_datum.
    CLEAR: rg_datum.

    SORT psp BY datum.
    DELETE ADJACENT DUPLICATES FROM psp COMPARING datum.
    DELETE psp WHERE datum NOT IN rg_datum.

    SORT ab BY awart endda begda beguz enduz.
    DELETE ADJACENT DUPLICATES FROM ab COMPARING awart endda begda beguz enduz.

    SORT: pt BY ldate begtm,
          ab BY begda.

    CLEAR: wa_psp.

    LOOP AT psp INTO wa_psp.

      wa_batida_list-cname        = wa_saida_tab-cname.
      wa_batida_list-cpf_nr       = wa_saida_tab-cpf_nr.
      wa_batida_list-email        = wa_saida_tab-email.
      wa_batida_list-pernr        = wa_saida_tab-pernr.
      wa_batida_list-bukrs        = wa_saida_tab-bukrs.
      wa_batida_list-empresa      = wa_saida_tab-empresa.
      wa_batida_list-persk        = wa_saida_tab-persk.
      wa_batida_list-ptext        = wa_saida_tab-ptext.
      wa_batida_list-werks        = wa_saida_tab-werks.
      wa_batida_list-arearh       = wa_saida_tab-arearh.
      wa_batida_list-stat2        = wa_saida_tab-stat2.
      wa_batida_list-situacao     = wa_saida_tab-situacao .
      wa_batida_list-orgeh        = wa_saida_tab-orgeh.
      wa_batida_list-uniorg       = wa_saida_tab-uniorg.
      wa_batida_list-cargo        = wa_saida_tab-cargo.
      wa_batida_list-plans        = wa_saida_tab-plans.
      wa_batida_list-posicao      = wa_saida_tab-posicao.
      wa_batida_list-kostl        = wa_saida_tab-kostl.
      wa_batida_list-ccusto       = wa_saida_tab-ccusto.
      wa_batida_list-dat01        = wa_saida_tab-dat01.
      wa_batida_list-jornada      = wa_saida_tab-jornada.
      wa_batida_list-salario      = wa_saida_tab-salario.
      wa_batida_list-schkz        = wa_saida_tab-schkz.
      wa_batida_list-horario      = wa_saida_tab-horario.

      wa_batida_list-area_folha = wa_saida_tab-area_folha. "BUG - 101068  - CBRAND

      wa_batida_list-data_batida  = wa_psp-datum.

      CLEAR: v_hora, v_time, v_tjornada.

      v_tjornada = wa_batida_list-jornada.

      CALL FUNCTION 'ZHCMF_CONVERT_NUM_HORA'
        EXPORTING
          v_hora     = v_tjornada
        IMPORTING
          v_ret_hora = v_hora
          v_ret_time = v_time.

      wa_batida_list-tjornada  = v_time.

      wa_batida_list-tprog        = wa_psp-tprog.

      CLEAR v_daysem.

      CALL FUNCTION 'DATE_COMPUTE_DAY'
        EXPORTING
          date = wa_psp-datum
        IMPORTING
          day  = v_daysem.

      CASE v_daysem.
        WHEN 1.
          wa_batida_list-dia_semana = 'SEG'.
        WHEN 2.
          wa_batida_list-dia_semana = 'TER'.
        WHEN 3.
          wa_batida_list-dia_semana = 'QUA'.
        WHEN 4.
          wa_batida_list-dia_semana = 'QUI'.
        WHEN 5.
          wa_batida_list-dia_semana = 'SEX'.
        WHEN 6.
          wa_batida_list-dia_semana = 'SAB'.
        WHEN 7.
          wa_batida_list-dia_semana = 'DOM'.
      ENDCASE.

      "Verificar os registros ponto
      count = 0.
      CLEAR wa_pt.
      LOOP AT pt INTO wa_pt WHERE ldate EQ wa_psp-datum.
        count = count + 1.
        IF wa_pt-ldate GT sy-datum. " NÃO LÊ O TEÓRICO data maior que o dia de processamento
          CONTINUE.
        ELSE.

          CLEAR: v_time_ent, v_time_sai.

          v_time_ent =  (  wa_pt-begtm(2) * 60 ) + wa_pt-begtm+2(2).

          "Situações em que o colaborador trabalha no periodo noturno.
          IF wa_pt-endtm(2) >= 24.
            v_hora_not = 0 .
            v_hora_not = wa_pt-endtm(2) - 24.
            v_time_sai =  ( v_hora_not * 60 ) + wa_pt-endtm+2(2).

          ELSE.
            v_time_sai =  ( wa_pt-endtm(2) * 60 ) + wa_pt-endtm+2(2).
          ENDIF.

          CONDENSE v_time_ent.
          CONDENSE v_time_sai.

          CLEAR: v_hora_not, v_hora_not_c.

          IF count = 1.
            CONCATENATE wa_pt-begtm(2)  ':' wa_pt-begtm+2(2)  INTO wa_batida_list-ent1.

            IF wa_pt-endtm(2) >= 24.

              v_hora_not = wa_pt-endtm(2) - 24.
              v_hora_not_c = v_hora_not.

              TRANSLATE v_hora_not_c USING ' 0'.
              CONCATENATE v_hora_not_c  ':' wa_pt-endtm+2(2)  INTO wa_batida_list-sai1.

            ELSE.
              CONCATENATE wa_pt-endtm(2)  ':' wa_pt-endtm+2(2)  INTO wa_batida_list-sai1.
            ENDIF.

            wa_batida_list-tent1 = v_time_ent.
            wa_batida_list-tsai1 = v_time_sai.

          ELSE.
            IF count = 2.

              IF wa_pt-begtm(2) >= 24.

                v_hora_not = wa_pt-begtm(2) - 24.
                v_hora_not_c = v_hora_not.

                TRANSLATE v_hora_not_c USING ' 0'.
                CONCATENATE v_hora_not_c  ':' wa_pt-begtm+2(2)  INTO wa_batida_list-ent2.
              ELSE.
                CONCATENATE wa_pt-begtm(2)  ':' wa_pt-begtm+2(2)  INTO wa_batida_list-ent2.
              ENDIF.

              IF wa_pt-endtm(2) >= 24.

                v_hora_not = wa_pt-endtm(2) - 24.
                v_hora_not_c = v_hora_not.

                TRANSLATE v_hora_not_c USING ' 0'.
                CONCATENATE v_hora_not_c  ':' wa_pt-endtm+2(2)  INTO wa_batida_list-sai2.
              ELSE.
                CONCATENATE wa_pt-endtm(2)  ':' wa_pt-endtm+2(2)  INTO wa_batida_list-sai2.
              ENDIF.

              wa_batida_list-tent2 = v_time_ent.
              wa_batida_list-tsai2 = v_time_sai.

            ELSE.
              IF count = 3.

                IF wa_pt-begtm(2) >= 24.

                  v_hora_not = wa_pt-begtm(2) - 24.
                  v_hora_not_c = v_hora_not.

                  TRANSLATE v_hora_not_c USING ' 0'.
                  CONCATENATE v_hora_not_c  ':' wa_pt-begtm+2(2)  INTO wa_batida_list-ent3.
                ELSE.
                  CONCATENATE wa_pt-begtm(2)  ':' wa_pt-begtm+2(2)  INTO wa_batida_list-ent3.
                ENDIF.

                IF wa_pt-endtm(2) > 24.

                  v_hora_not = wa_pt-endtm(2) - 24.
                  v_hora_not_c = v_hora_not.

                  TRANSLATE v_hora_not_c USING ' 0'.
                  CONCATENATE v_hora_not_c  ':' wa_pt-endtm+2(2)  INTO wa_batida_list-sai3.

                ELSE.
                  CONCATENATE wa_pt-endtm(2)  ':' wa_pt-endtm+2(2)  INTO wa_batida_list-sai3.
                ENDIF.

                wa_batida_list-tent3 = v_time_ent.
                wa_batida_list-tsai3 = v_time_sai.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.


      CLEAR wa_zes.

      DATA: r_chck_hr_d TYPE RANGE OF char05.
      DATA: hr_d TYPE pthours VALUE 11.
      DATA: tl_saida TYPE TABLE OF zhcms_horarios_excecao.

      CALL FUNCTION 'ZHCMF_RETURN_HORARIOS_FUNC'
        EXPORTING
*         pernr   = pernr_func "pernr " Nº pessoal
          pernr   = wl_func_list-pernr "pernr " Nº pessoal
          begda   = begda " Início da validade
          endda   = endda " Fim da validade
        TABLES
          t_saida = tl_saida.    " Tabela Horários Calendário - Ponto Exceção


      r_chck_hr_d = VALUE
      #( FOR ls1 IN tl_saida WHERE ( hora_diario EQ hr_d )
       ( sign = 'I' option = 'EQ' low = abap_true  ) ).

      CLEAR l_anzhl_12x36.

      LOOP AT  zes INTO wa_zes  WHERE datum = wa_psp-datum.

        CLEAR: v_hora, v_time.

        wa_zes-anzhl = abs( wa_zes-anzhl ).
        CALL FUNCTION 'ZHCMF_CONVERT_NUM_HORA'
          EXPORTING
            v_hora     = wa_zes-anzhl
          IMPORTING
            v_ret_hora = v_hora
            v_ret_time = v_time.

        CASE wa_zes-ztart.
          WHEN 'WSHA'.
            wa_batida_list-atraso  = v_hora.
            wa_batida_list-tatraso = v_time.
          WHEN 'WSHF'.
            wa_batida_list-falta  = v_hora.
            wa_batida_list-tfalta = v_time.
          WHEN 'WSHT'.
            wa_batida_list-htrab   = v_hora.
            wa_batida_list-thtrab  = v_time.
          WHEN 'WSEE'.
            wa_batida_list-extraexecutado  = v_hora.
            wa_batida_list-textraexecutado = v_time.

            IF r_chck_hr_d IS NOT INITIAL.
              l_anzhl_12x36 = l_anzhl_12x36 + wa_zes-anzhl.
              CALL FUNCTION 'ZHCMF_CONVERT_NUM_HORA'
                EXPORTING
                  v_hora     = l_anzhl_12x36
                IMPORTING
                  v_ret_hora = v_hora_12x36.

              wa_batida_list-he_12x36 = v_hora_12x36.
            ENDIF.

          WHEN 'WSAD'.
            wa_batida_list-adicional  = v_hora.
            wa_batida_list-tadicional = v_time.
          WHEN 'WSAB'.
            wa_batida_list-abono   = v_hora.
            wa_batida_list-tabono  = v_time.
          WHEN 'WSBS'.
            wa_batida_list-base   = v_hora.
            wa_batida_list-tbase  = v_time.
          WHEN 'WSDE'.
            wa_batida_list-descanso  = v_hora.
            wa_batida_list-tdescanso = v_time.
          WHEN 'WSFE'.
            wa_batida_list-feriado  = v_hora.
            wa_batida_list-tferiado = v_time.
          WHEN 'WSBF'.
            wa_batida_list-faltabanco  = v_hora.
            wa_batida_list-tfaltabanco = v_time.
          WHEN 'WSBA'.
            wa_batida_list-atrasobanco   = v_hora.
            wa_batida_list-tatrasobanco  = v_time.

            IF r_chck_hr_d IS NOT INITIAL.
              l_anzhl_12x36 = l_anzhl_12x36 + wa_zes-anzhl.
              CALL FUNCTION 'ZHCMF_CONVERT_NUM_HORA'
                EXPORTING
                  v_hora     = l_anzhl_12x36
                IMPORTING
                  v_ret_hora = v_hora_12x36.

              wa_batida_list-he_12x36 = v_hora_12x36.
            ENDIF.

        ENDCASE.

      ENDLOOP.

      CLEAR: wa_ab.

      LOOP AT ab INTO wa_ab WHERE ( begda <= wa_psp-datum ) AND ( endda >= wa_psp-datum ).

        IF wa_ab-awart IS NOT INITIAL.

          "Código Abono
          wa_batida_list-cod_abono = wa_ab-awart.

          "Nome Abono
          SELECT SINGLE atext
            FROM t554t
              INTO wa_batida_list-nomeabonos
            WHERE sprsl = sy-langu
              AND moabw = '37'
              AND awart =  wa_ab-awart.
*       Tipo de Abono
          CASE wa_ab-awart.
            WHEN '0360'.                      "FALTA JUSTIFICADA
              wa_batida_list-tipofalta = 1.
            WHEN '0350' OR '0943'.            "FALTA INJUSTIFICADA
              wa_batida_list-tipofalta = 2.
            WHEN '0290' OR '0291'.            "Lic.Maternida
              wa_batida_list-nomeabonos = 'Lic.Maternidade'.

          ENDCASE.

        ENDIF.
      ENDLOOP.

      "Feriado
      CLEAR: v_ltext,
             w001p.

      IF wa_psp-ftkla <> 0.

        CLEAR: v_mofid.
        SELECT SINGLE mofid
        FROM pa2003
          INTO v_mofid
        WHERE pernr = wa_batida_list-pernr
          AND begda LE wa_psp-datum
          AND endda GE wa_psp-datum.

        IF v_mofid IS INITIAL.

          CALL FUNCTION 'HR_READ_INFOTYPE'
            EXPORTING
              tclas     = 'A'
              pernr     = wa_batida_list-pernr
              infty     = '0001'
            TABLES
              infty_tab = l_it_0001.

          SORT: l_it_0001 BY endda DESCENDING.

          READ TABLE l_it_0001 INTO l_0001 INDEX 1.

          CALL FUNCTION 'HR_TMW_READ_T001P'
            EXPORTING
              persa          = l_0001-werks
              btrtl          = l_0001-btrtl
            IMPORTING
              w001p          = w001p
            EXCEPTIONS
              no_entry_found = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            CLEAR w001p.
          ENDIF.

        ELSE.
          w001p-mofid =  v_mofid.
        ENDIF.

        CALL FUNCTION 'HR_FORMS_READ_HOLIDAYS'
          EXPORTING
            sprsl      = sy-langu
            begda      = wa_psp-datum
            endda      = wa_psp-datum
            mofid      = w001p-mofid
            date_value = wa_psp-datum
          CHANGING
            ltext      = v_ltext
            stext      = v_text.

        wa_batida_list-nomeferiado = v_ltext.

      ENDIF.

*     Caso exista, sempre preencher campos de falta
      IF NOT wa_batida_list-faltabanco IS INITIAL.
        wa_batida_list-falta  = wa_batida_list-faltabanco.
        wa_batida_list-tfalta = wa_batida_list-tfaltabanco.
      ENDIF.

*     TIPO DE FALTA = ATRASO
      IF NOT wa_batida_list-tatraso IS INITIAL.
        wa_batida_list-tipofalta = 3.
      ENDIF.

*     Atitude 0 - descontado /  1 - Compensado pelo BH
      READ TABLE zl INTO wa_zl WITH KEY datum = wa_psp-datum
                                        lgart = '8111'.
      IF sy-subrc EQ 0.
        wa_batida_list-atitude = 1.
      ELSE.
        wa_batida_list-atitude = 0.
      ENDIF.

      APPEND wa_batida_list TO t_saida.
      CLEAR: wa_batida_list.

    ENDLOOP.

    CLEAR: wa_saida_tab.

    CLEAR: it_0008[], it_0007[], it_2003[], wa_0008. ", wa_0007.

  ENDLOOP.

ENDFUNCTION.
