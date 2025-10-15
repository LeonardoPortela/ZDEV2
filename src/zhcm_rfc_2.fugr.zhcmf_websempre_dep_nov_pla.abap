FUNCTION zhcmf_websempre_dep_nov_pla.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(BEGDA) TYPE  BEGDA
*"     VALUE(ENDDA) TYPE  ENDDA
*"  TABLES
*"      IT_SAIDA STRUCTURE  ZHCMS_WEBSEMPRE_DEP_NOV_PLA_S
*"----------------------------------------------------------------------

  DATA: it_pa0167 TYPE STANDARD TABLE OF pa0167,
        it_p0465  TYPE STANDARD TABLE OF p0465,
        it_p0021  TYPE STANDARD TABLE OF p0021,
        it_p0398  TYPE STANDARD TABLE OF p0398,
        it_p0105  TYPE STANDARD TABLE OF p0105,
        it_p0002  TYPE STANDARD TABLE OF p0002,
        it_p0001  TYPE STANDARD TABLE OF p0001,
        it_p0006 TYPE STANDARD TABLE OF p0006,
        wa_pa0167 TYPE pa0167,
        wa_p0465  TYPE p0465,
        wa_p0021  TYPE p0021,
        wa_p0006 TYPE p0006,
        wa_p0398  TYPE p0398,
        wa_p0105  TYPE p0105,
        wa_p0002  TYPE p0002,
        wa_p0001  TYPE p0001,
        wa_saida  TYPE zhcms_websempre_dep_nov_pla_s.

  DATA: v_condicao_dep  TYPE string,
        t_error_table   TYPE STANDARD TABLE OF rpbenerr,
        t_dependentes_1 TYPE STANDARD TABLE OF rpbenddp,
        t_dependentes_2 TYPE STANDARD TABLE OF rpbenddp.


*** BUG - 126211 - Inicio - CBRAND
  RANGES: rg_bukrs  FOR p0001-bukrs.
  DATA: t_tvarvc   TYPE TABLE OF tvarvc.

  DATA: r_bplan TYPE RANGE OF ben_plan.

  r_bplan = VALUE #( sign = 'I' option = 'EQ' ( low = 'MED7' )
                                              ( low = 'MED8' ) ).


  SELECT *
    FROM tvarvc
    INTO TABLE t_tvarvc
   WHERE name LIKE 'ZHCM_BUKRS_ADM_UNIMED'.

  IF t_tvarvc[] IS NOT INITIAL.
    REFRESH: rg_bukrs .
    LOOP AT t_tvarvc INTO DATA(ls_tvarvc).
      rg_bukrs-sign   = 'I'.
      rg_bukrs-option = 'EQ'.
      rg_bukrs-low = ls_tvarvc-low.
      rg_bukrs-high = space.
      APPEND rg_bukrs.
      CLEAR: rg_bukrs.
    ENDLOOP.
  ELSE.
    CLEAR t_tvarvc.
  ENDIF.
*** BUG - 126211 - Fim - CBRAND

  "Pego todos os planos com período selecionado no filtro
  CLEAR: it_pa0167.
  SELECT *
    FROM pa0167
    INTO TABLE it_pa0167
    WHERE subty LIKE 'MED%'
      AND bplan NOT IN r_bplan
      AND ( begda LE endda AND
            begda GE begda ).

  SORT it_pa0167 BY pernr subty begda ASCENDING.

**** BUG - 129688 - Inicio - CBRAND
*** Somente Inclusão.

  DATA(it_pa0167_aux) = it_pa0167[].

  LOOP AT it_pa0167_aux[] ASSIGNING FIELD-SYMBOL(<w_pa0167_aux>).
    <w_pa0167_aux>-endda = <w_pa0167_aux>-begda - 1.
  ENDLOOP.

  SELECT
    p167~pernr,
    p167~subty,
    p167~objps,
    p167~sprps,
    p167~endda,
    p167~begda,
    p167~seqnr,
    p167~barea,
    p167~pltyp,
    p167~bplan,
    p167~bengr,
    p167~bstat
  FROM pa0167 AS p167
  INTO TABLE @DATA(it_pa0167_alterados)
  FOR ALL ENTRIES IN @it_pa0167_aux
  WHERE
    pernr = @it_pa0167_aux-pernr AND
    subty LIKE 'MED%'           AND
    endda = @it_pa0167_aux-endda
  ORDER BY PRIMARY KEY.

  LOOP AT it_pa0167 INTO wa_pa0167.

* 1 - Busco a lista de dependentes.
    DATA(w_benefit_data) = VALUE rpbeneedat( pernr = wa_pa0167-pernr
                                             barea = wa_pa0167-barea ).

    "Busca dependentes do mês consultado
    CLEAR: t_dependentes_1[],
       t_error_table[].

    CALL FUNCTION 'HR_BEN_READ_DEPENDENTS'
      EXPORTING
        ee_benefit_data = w_benefit_data
        bpcat           = 'A'
        pltyp           = wa_pa0167-pltyp
        bplan           = wa_pa0167-bplan
        begda           = wa_pa0167-begda
        endda           = wa_pa0167-endda
        logicview       = 'X'
        reaction        = 'N'
      TABLES
        existing_dep    = t_dependentes_1[]
        error_table     = t_error_table[].


    READ TABLE it_pa0167_alterados[] INTO DATA(wa_pa0167_alterados) WITH KEY pernr = wa_pa0167-pernr
                                                                             subty = wa_pa0167-subty BINARY SEARCH.
    IF sy-subrc = 0.
      "Busca dependentes da data anterior
      CLEAR: t_dependentes_2[],
             t_error_table[].
      CALL FUNCTION 'HR_BEN_READ_DEPENDENTS'
        EXPORTING
          ee_benefit_data = w_benefit_data
          bpcat           = 'A'
          pltyp           = wa_pa0167_alterados-pltyp
          bplan           = wa_pa0167_alterados-bplan
          begda           = wa_pa0167_alterados-begda
          endda           = wa_pa0167_alterados-endda
          logicview       = 'X'
          reaction        = 'N'
        TABLES
          existing_dep    = t_dependentes_2[]
          error_table     = t_error_table[].


      " Aqui preciso comprar os dependentes.

      LOOP AT t_dependentes_1 INTO DATA(l_dependentes_1).
        READ TABLE t_dependentes_2 INTO DATA(l_dependentes_2)  WITH KEY  dep_type = l_dependentes_1-dep_type  dep_id = l_dependentes_1-dep_id
                                                                         dep_name = l_dependentes_1-dep_name.
        IF sy-subrc = 0.
          CONTINUE.
        ELSE.
* Se for nova inclusão - envia.

          CLEAR: it_p0001.
          CALL FUNCTION 'HR_READ_INFOTYPE'
            EXPORTING
              tclas     = 'A'
              pernr     = wa_pa0167-pernr
              infty     = '0001'
            TABLES
              infty_tab = it_p0001.

          SORT it_p0001 BY endda DESCENDING.
          CLEAR:wa_p0001 .
          READ TABLE it_p0001 INTO wa_p0001 INDEX 1.

          IF  wa_p0001-bukrs NOT IN rg_bukrs. "*** BUG - 126211

            wa_saida-werks  = wa_p0001-werks.
            wa_saida-bukrs  = wa_p0001-bukrs. "DEVK9A20QS - #140481 RSA

            CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
              EXPORTING
                werks      = wa_p0001-werks
              IMPORTING
                werks_text = wa_saida-arearh.


            wa_saida-kostl   =  wa_p0001-kostl.


            SELECT stat2 UP TO 1 ROWS "DEVK9A20QS - #140481 RSA
                   FROM pa0000
                   INTO @DATA(vl_stat2)
                   WHERE pernr EQ @wa_p0001-pernr
                   AND   begda <= @endda
                   AND   endda >= @endda.
            ENDSELECT.

            IF NOT vl_stat2 IS INITIAL. "DEVK9A20QS - #140481 RSA
              SELECT SINGLE text1
                     FROM t529u
                     INTO wa_saida-stat2
                     WHERE sprsl EQ 'PT'
                     AND   statn EQ '2'
                     AND   statv EQ vl_stat2.
            ENDIF.


            SELECT SINGLE ktext  FROM cskt
              INTO wa_saida-ccusto
            WHERE kokrs = wa_p0001-kokrs
              AND kostl = wa_p0001-kostl
              AND spras = sy-langu
              AND datbi >= sy-datum.

            wa_saida-orgeh   = wa_p0001-orgeh.

            SELECT SINGLE stext FROM hrp1000
             INTO wa_saida-uniorg
           WHERE plvar = '01'
            AND  objid = wa_p0001-orgeh
            AND  otype = 'O'
            AND  begda <= endda
            AND  endda >= endda
            AND  langu = sy-langu.


            CLEAR: it_p0002.
            CALL FUNCTION 'HR_READ_INFOTYPE'
              EXPORTING
                tclas     = 'A'
                pernr     = wa_pa0167-pernr
                infty     = '0002'
              TABLES
                infty_tab = it_p0002.

            IF ( l_dependentes_1-dep_type = '1') OR ( l_dependentes_1-dep_type = '13' ) OR ( l_dependentes_1-dep_type = '15').
              SELECT SINGLE
                fgbdt,
                znm_paicom, famsa , fanat , fasex , znumerocom ,
                zufcom , zorgao_expcom , zdata_emissaocom
              FROM pa0021
              INTO @DATA(w_pa0021)
              WHERE
                pernr = @wa_pa0167-pernr  AND
                subty = @l_dependentes_1-dep_type AND
                endda >= @sy-datum.

              SELECT SINGLE
                icnum,
                mothe, lbcnr,
                nhcnr "AJUSTE BUG #168966 - MMSILVA - 05.03.2025
              FROM pa0397
              INTO @DATA(w_pa0397)
              WHERE
                pernr = @wa_pa0167-pernr  AND
                subty = @l_dependentes_1-dep_type AND
                endda >= @sy-datum..

            ELSE.

              SELECT SINGLE
                fgbdt znm_paicom famsa fanat fasex znumerocom
                zufcom zorgao_expcom zdata_emissaocom
              FROM pa0021
              INTO w_pa0021
              WHERE
                pernr = wa_pa0167-pernr AND
                objps = l_dependentes_1-dep_id AND
                subty = l_dependentes_1-dep_type.


              SELECT SINGLE
                icnum mothe lbcnr
                nhcnr "AJUSTE BUG #168966 - MMSILVA - 05.03.2025
              FROM pa0397
              INTO w_pa0397
              WHERE
                pernr = wa_pa0167-pernr AND
                objps = l_dependentes_1-dep_id AND
                subty = l_dependentes_1-dep_type.

            ENDIF.

            SORT it_p0002 BY endda DESCENDING.
            READ TABLE it_p0002 INTO wa_p0002 INDEX 1.

            wa_saida-pernr      = wa_p0002-pernr.
            wa_saida-fcnam_p    = w_pa0021-znm_paicom.
            wa_saida-gbdat      = w_pa0021-fgbdt.
            wa_saida-famsa      = w_pa0021-famsa.
            wa_saida-natio      = w_pa0021-fanat.
            wa_saida-gesch      = w_pa0021-fasex.
            wa_saida-ident_nr   = w_pa0021-znumerocom.
            wa_saida-es_emis    = w_pa0021-zufcom.
            wa_saida-doc_issuer = w_pa0021-zorgao_expcom.
            wa_saida-dt_emis    = w_pa0021-zdata_emissaocom.

            wa_saida-famst    = wa_p0002-famst. "Estado Civil
            "wa_saida-gesch    = wa_p0002-gesch. "Sexo
            "wa_saida-natio    = wa_p0002-natio.

            wa_saida-fcnam_m  = w_pa0397-mothe.
            wa_saida-icnum    = w_pa0397-icnum.
            wa_saida-cname = l_dependentes_1-dep_name.
            wa_saida-declnvivo = w_pa0397-lbcnr.
            wa_saida-ident_nr_s = w_pa0397-nhcnr. "AJUSTE BUG #168966 - MMSILVA - 05.03.2025

            CLEAR: it_p0398.
            CALL FUNCTION 'HR_READ_INFOTYPE'
              EXPORTING
                tclas     = 'A'
                pernr     = wa_p0002-pernr
                infty     = '0398'
              TABLES
                infty_tab = it_p0398.

            SORT it_p0398 BY endda DESCENDING.

            CLEAR:wa_p0398.
            READ TABLE it_p0398 INTO wa_p0398 INDEX 1.
            IF sy-subrc IS INITIAL.
              wa_saida-escol = wa_p0398-escol.
            ENDIF.


            CLEAR: it_p0465.
            CALL FUNCTION 'HR_READ_INFOTYPE'
              EXPORTING
                tclas     = 'A'
                pernr     = wa_p0002-pernr
                infty     = '0465'
              TABLES
                infty_tab = it_p0465.

            SORT it_p0465 BY endda DESCENDING.
            CLEAR:wa_p0465.
            READ TABLE it_p0465 INTO wa_p0465 WITH KEY subty = '0001'.
            IF sy-subrc IS INITIAL.
              wa_saida-cpf_nr = wa_p0465-cpf_nr.
            ENDIF.

*            AJUSTE BUG #168966 - MMSILVA - 05.03.2025 - Inicio
*            CLEAR: wa_p0465.
*            READ TABLE it_p0465 INTO wa_p0465 WITH KEY subty = '0015'.
*            IF sy-subrc IS INITIAL.
*              wa_saida-ident_nr_s = wa_p0465-ident_nr.
*            ENDIF.
*            AJUSTE BUG #168966 - MMSILVA - 05.03.2025 - Fim

*            CLEAR: wa_p0465.
*            READ TABLE it_p0465 INTO wa_p0465 WITH KEY subty = '0002'.
*            IF sy-subrc IS INITIAL.
*              wa_saida-ident_nr   = wa_p0465-ident_nr.
*              wa_saida-es_emis    = wa_p0465-es_emis.
*              wa_saida-doc_issuer = wa_p0465-doc_issuer.
*              wa_saida-dt_emis    = wa_p0465-dt_emis.
*            ENDIF.

            CLEAR:it_p0105.
            CALL FUNCTION 'HR_READ_INFOTYPE'
              EXPORTING
                tclas     = 'A'
                pernr     = wa_p0002-pernr
                infty     = '0105'
              TABLES
                infty_tab = it_p0105.

            SORT it_p0105 BY endda DESCENDING.

            CLEAR:wa_p0105.
            READ TABLE it_p0105 INTO wa_p0105 INDEX 1.
            IF sy-subrc IS INITIAL.
              wa_saida-usrid_long = wa_p0105-usrid_long.
            ENDIF.

      CLEAR: it_p0006.
        CALL FUNCTION 'HR_READ_INFOTYPE'
          EXPORTING
            tclas     = 'A'
            pernr     = wa_pa0167-pernr
            infty     = '0006'
          TABLES
            infty_tab = it_p0006.

      SORT it_p0006 BY endda DESCENDING.
      SORT it_p0006 BY subty ASCENDING
                       endda DESCENDING.
*Inicío Alteração "US 186771# - ROBPDIAS
      READ TABLE it_p0006 INTO wa_p0006 WITH KEY subty = '1'.

      IF sy-subrc IS INITIAL.

        SELECT SINGLE * FROM t591s INTO @DATA(lwa_t591s)
             WHERE sprsl EQ @sy-langu
             AND   infty EQ '0006'
             AND   subty EQ @wa_p0006-anssa.

        IF wa_p0006-com01 = 'CON1' OR wa_p0006-com01 = 'CON2' OR wa_p0006-com01 = 'CON3' . "US 186771# - ROBPDIAS

          wa_saida-num01  = wa_p0006-num01. "US 186771# - ROBPDIAS

        ENDIF. "US 186771# - ROBPDIAS
        IF wa_p0006-com02 = 'CON1' OR wa_p0006-com02 = 'CON2' OR wa_p0006-com02 = 'CON3' .
          wa_saida-num02  = wa_p0006-num02.
        ENDIF.
        IF wa_p0006-com03 = 'CON1' OR wa_p0006-com03 = 'CON2' OR wa_p0006-com03 = 'CON3'.
          wa_saida-num03  = wa_p0006-num03.
        ENDIF.
        CLEAR:lwa_t591s.
      ENDIF.
*FIM ALTERAÇÃO "US 186771# - ROBPDIAS

            wa_saida-bopti = wa_pa0167-bopti.


            IF wa_saida IS NOT INITIAL.
              APPEND wa_saida TO it_saida.
            ENDIF.

            CLEAR: wa_saida, w_pa0021, w_pa0397, wa_p0002, wa_p0465, it_p0002, it_p0105, it_p0465, it_p0398, it_p0002,wa_p0006.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
