CLASS lcl_hierarquia_organizacional  IMPLEMENTATION. "Responsabilidade -> Receber um aprovador e buscar todos os gestores acima do mesmo, percorrendo toda estrutura organizacional.

  METHOD constructor.
    me->gt_pernr = it_pernr.
  ENDMETHOD.

  METHOD build_data.

    DATA: lt_aprovadores TYPE zsdados_aprovadores_tt.

    WHILE me->gt_pernr IS NOT INITIAL.

      SELECT pernr stell orgeh
        FROM pa0001
        INTO TABLE lt_aprovadores
        FOR ALL ENTRIES IN me->gt_pernr
        WHERE pernr =  me->gt_pernr-pernr
        AND   endda GE sy-datum.

      IF lt_aprovadores IS NOT INITIAL. " BUG - 90416 - CBRAND

        APPEND LINES OF lt_aprovadores TO gs_dados_hierarquia-aprovadores.

        SELECT pernr samaccountname
          FROM zhcmt0007
          APPENDING TABLE gs_dados_hierarquia-nomes_aprovadores
          FOR ALL ENTRIES IN me->gt_pernr
          WHERE pernr = me->gt_pernr-pernr.

        SORT lt_aprovadores BY stell.
        DELETE ADJACENT DUPLICATES FROM lt_aprovadores COMPARING stell.

        SELECT objid hilfm
          FROM hrp1010
          APPENDING TABLE gs_dados_hierarquia-niveis_aprovadores
          FOR ALL ENTRIES IN lt_aprovadores
          WHERE plvar = '01'
          AND   otype = 'C'
          AND   objid = lt_aprovadores-stell
          AND   subty = '0008'
          AND   endda GE sy-datum.

        SORT lt_aprovadores BY orgeh.
        DELETE ADJACENT DUPLICATES FROM lt_aprovadores COMPARING orgeh.

        SELECT objid, hilfm
          FROM hrp1010
          INTO TABLE @DATA(lt_tipo_operacao)
          FOR ALL ENTRIES IN @lt_aprovadores
          WHERE plvar = '01'
          AND   otype = 'O'
          AND   objid = @lt_aprovadores-orgeh
          AND   subty = '0003'
          AND   endda GE @sy-datum.

        APPEND LINES OF lt_tipo_operacao TO gs_dados_hierarquia-tipo_operacao_chave.

        IF sy-subrc IS INITIAL.
          SORT lt_tipo_operacao BY hilfm.
          DELETE ADJACENT DUPLICATES FROM lt_tipo_operacao COMPARING hilfm.

          SELECT hilfm htext
            FROM t777w
            APPENDING TABLE gs_dados_hierarquia-tipo_operacao
            FOR ALL ENTRIES IN lt_tipo_operacao
            WHERE langu = sy-langu
            AND   subty = '0003'
            AND   hilfm = lt_tipo_operacao-hilfm.
        ENDIF.

        IF me->gt_pernr IS NOT INITIAL.
          SELECT pernr, pernimed
            FROM pa9002
            INTO TABLE @DATA(lt_gestores_imediatos)
            FOR ALL ENTRIES IN @me->gt_pernr
            WHERE pernr = @me->gt_pernr-pernr
            AND   endda GE @sy-datum.
          IF sy-subrc IS INITIAL.
*** BUG -  139997 - Inicio
*** Pernr com mesma matricula de gestor imediato
            LOOP AT lt_gestores_imediatos INTO DATA(lwa_gestores_imediatos).
              DATA(lva_tabix) = sy-tabix.
              IF lwa_gestores_imediatos-pernimed = lwa_gestores_imediatos-pernr.

                SELECT SINGLE *
                  FROM pa9002
                INTO @DATA(lwa_gestores_mediatos)
                    WHERE pernr = @lwa_gestores_imediatos-pernr
                    AND   endda GE @sy-datum.

                lwa_gestores_imediatos-pernimed = lwa_gestores_mediatos-pernmed.

                MODIFY lt_gestores_imediatos FROM lwa_gestores_imediatos INDEX lva_tabix TRANSPORTING pernimed.
                CLEAR: lwa_gestores_imediatos, lwa_gestores_mediatos.
              ENDIF.
            ENDLOOP.
*** BUG -  139997 - Fim

            APPEND LINES OF lt_gestores_imediatos TO gs_dados_hierarquia-gestores_imediatos.

            LOOP AT lt_gestores_imediatos INTO DATA(ls_gestores_imediatos).
              READ TABLE me->gt_pernr INTO DATA(ls_pernr) WITH KEY pernr = ls_gestores_imediatos-pernr BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                DELETE me->gt_pernr INDEX sy-tabix.
                ls_pernr-pernr = ls_gestores_imediatos-pernimed. "Altera o número pessoal para o número pessoal do gestor e busca todas as informações novamente.
                INSERT ls_pernr INTO TABLE me->gt_pernr.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

        DELETE me->gt_pernr WHERE pernr = '00000000'.
      ELSE.
        " Não existe matricula na PA0001. - BUG - 90416 - CBRAND
        DELETE me->gt_pernr WHERE pernr <> '00000000'.

      ENDIF.

    ENDWHILE.

    ev_dados_hierarquia = me->gs_dados_hierarquia.

    FREE: lt_gestores_imediatos, ls_pernr, ls_gestores_imediatos, lt_tipo_operacao, lt_aprovadores, me->gs_dados_hierarquia.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_load_approver_k IMPLEMENTATION. "Responsabilidade -> Efetuar carga de aprovadores centro de custo
  METHOD constructor.
*** CSB - 01.07.2022 - Inicio - US82203
    IF iv_begin_date IS INITIAL.
      APPEND VALUE #( sign = 'I' option = 'GE' low = sy-datum ) TO me->gt_range_date.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'GE' low = iv_begin_date ) TO me->gt_range_date.
    ENDIF.
*** CSB - 01.07.2022 - Fim

  ENDMETHOD.

  METHOD if_load_approver~load.

    DATA: lt_pernr            TYPE ztt_pernr,
          ls_pernr            TYPE zpernr,
          ls_dados_hierarquia TYPE zsdados_hierarquia_hcm,
          ls_aprovadores      TYPE zprovcoupa01.

    DATA: l_pernr                    TYPE zpernr,
          l_niveis                   TYPE i,
          l_qtd_niveis               TYPE i,
          l_niveis_aprovador_nivel_1 TYPE i,
          l_proximo_nivel            TYPE i.

    DATA: lc_nivel TYPE hrp1010-hilfm VALUE '6'.

    SELECT csks~kostl, csks~gsber, csks~abtei, cskt~ltext
      FROM csks INNER JOIN cskt
      ON  csks~kokrs = cskt~kokrs
      AND csks~kostl = cskt~kostl
      INTO TABLE @DATA(lt_centros_custos)
      "WHERE csks~kokrs = 'MAGI'
      WHERE csks~kokrs IN ( 'MAGI', 'MGBG', 'MTGG' )
      AND   csks~datbi IN @me->gt_range_date
      AND   cskt~datbi IN @me->gt_range_date "CSB - 01.07.2022
      AND   cskt~spras = @sy-langu
      AND   EXISTS ( SELECT COUNT(*)
                      FROM pa0001
                      WHERE pernr = csks~abtei ).

    CHECK sy-subrc IS INITIAL.

    DELETE lt_centros_custos WHERE abtei IS INITIAL.

    DATA(lt_aprovador_centro_custo) = lt_centros_custos[].

    SORT lt_aprovador_centro_custo BY abtei.
    DELETE ADJACENT DUPLICATES FROM lt_aprovador_centro_custo COMPARING abtei.

    LOOP AT lt_aprovador_centro_custo INTO DATA(ls_aprovador_centro_custo).
      ls_pernr-pernr = ls_aprovador_centro_custo-abtei.
      INSERT ls_pernr INTO TABLE lt_pernr.
    ENDLOOP.

    DELETE lt_pernr WHERE pernr = '00000000'.

    CREATE OBJECT if_load_approver~go_hierarquia_organizacional
      EXPORTING
        it_pernr = lt_pernr.

    FREE: lt_pernr, ls_pernr, lt_aprovador_centro_custo, ls_aprovador_centro_custo, ls_dados_hierarquia.

    if_load_approver~go_hierarquia_organizacional->build_data( IMPORTING ev_dados_hierarquia = ls_dados_hierarquia ).

    IF lt_centros_custos IS NOT INITIAL.
      SELECT werks, name1
        FROM t001w
        INTO TABLE @DATA(lt_centros)
        FOR ALL ENTRIES IN @lt_centros_custos
        WHERE werks = @lt_centros_custos-gsber.
      IF sy-subrc IS INITIAL.
        SORT lt_centros BY werks.
      ENDIF.
    ENDIF.

    LOOP AT lt_centros_custos INTO ls_aprovador_centro_custo.
      CLEAR: ls_aprovadores, l_niveis.

      ls_aprovadores-tp_estrat = 'K'.
      ls_aprovadores-kostl     = ls_aprovador_centro_custo-kostl.
      ls_aprovadores-werks     = ls_aprovador_centro_custo-gsber.
      ls_aprovadores-ltext     = ls_aprovador_centro_custo-ltext.

      READ TABLE lt_centros INTO DATA(ls_centros) WITH KEY werks = ls_aprovador_centro_custo-gsber BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_aprovadores-name1    = ls_centros-name1.
      ENDIF.

      ls_aprovadores-dt_atual = sy-datum.
      ls_aprovadores-hr_atual = sy-uzeit.
      ls_aprovadores-usnam    = sy-uname.

      READ TABLE ls_dados_hierarquia-aprovadores INTO DATA(ls_aprovador) WITH KEY pernr = ls_aprovador_centro_custo-abtei.
      IF sy-subrc IS INITIAL.
        ls_aprovadores-nivel = '1'. "Primeiro nível é o responsável do centro de custo(CSKS-ABTEI).
        READ TABLE ls_dados_hierarquia-nomes_aprovadores INTO DATA(ls_nome_aprovadores) WITH KEY pernr = ls_aprovador-pernr.
        IF sy-subrc IS INITIAL.
          READ TABLE ls_dados_hierarquia-tipo_operacao_chave INTO DATA(ls_tipo_operacao_chave) WITH KEY objid = ls_aprovador-orgeh.
          IF sy-subrc IS INITIAL.
            READ TABLE ls_dados_hierarquia-tipo_operacao INTO DATA(ls_tipo_operacao) WITH KEY hilfm = ls_tipo_operacao_chave-hilfm.
            IF sy-subrc IS INITIAL.
              ls_aprovadores-tp_oper = ls_tipo_operacao-htext.
            ENDIF.
          ENDIF.
        ENDIF.
        ls_aprovadores-aprovador = ls_nome_aprovadores-samaccountname.
        APPEND ls_aprovadores TO me->if_load_approver~gt_aprovadores.
        ADD 1 TO l_niveis.
      ELSE.
        CONTINUE.
      ENDIF.

      "Primeiro aprovador pode ir além de apenas o primeiro nível, percorre até o nível do seu cargo.
      READ TABLE ls_dados_hierarquia-niveis_aprovadores INTO DATA(ls_niveis_aprovadores) WITH KEY objid = ls_aprovador-stell.
      IF sy-subrc IS INITIAL.
        l_niveis_aprovador_nivel_1 = ls_niveis_aprovadores-hilfm+1(1).
        l_niveis_aprovador_nivel_1 = l_niveis_aprovador_nivel_1 - 1. "Subtrai primeiro nível já adicionado.
        DO l_niveis_aprovador_nivel_1 TIMES.
          ls_aprovadores-nivel = l_niveis + 1.
          APPEND ls_aprovadores TO me->if_load_approver~gt_aprovadores.
          ADD 1 TO l_niveis.
        ENDDO.
      ENDIF.

      l_pernr = ls_aprovador-pernr.
      "Percorrer toda estrutura organizacional
      DO.
        READ TABLE ls_dados_hierarquia-gestores_imediatos INTO DATA(ls_gestores_imediatos) WITH KEY pernr = l_pernr.
        IF sy-subrc IS INITIAL.
          READ TABLE ls_dados_hierarquia-nomes_aprovadores INTO ls_nome_aprovadores WITH KEY pernr = ls_gestores_imediatos-pernimed.
          IF sy-subrc IS INITIAL.
            ls_aprovadores-aprovador = ls_nome_aprovadores-samaccountname.
            READ TABLE ls_dados_hierarquia-aprovadores INTO ls_aprovador WITH KEY pernr = ls_nome_aprovadores-pernr.
            IF sy-subrc IS INITIAL.
              READ TABLE ls_dados_hierarquia-tipo_operacao_chave INTO ls_tipo_operacao_chave WITH KEY objid = ls_aprovador-orgeh.
              IF sy-subrc IS INITIAL.
                READ TABLE ls_dados_hierarquia-tipo_operacao INTO ls_tipo_operacao WITH KEY hilfm = ls_tipo_operacao_chave-hilfm.
                IF sy-subrc IS INITIAL.
                  ls_aprovadores-tp_oper = ls_tipo_operacao-htext.
                ENDIF.
              ENDIF.
              READ TABLE ls_dados_hierarquia-niveis_aprovadores INTO ls_niveis_aprovadores WITH KEY objid = ls_aprovador-stell.
              IF sy-subrc IS INITIAL.

                l_proximo_nivel = ls_niveis_aprovadores-hilfm+1(1).
                l_qtd_niveis    = l_proximo_nivel - l_niveis.

                DO l_qtd_niveis TIMES.
                  ls_aprovadores-nivel = l_niveis + 1.
                  APPEND ls_aprovadores TO me->if_load_approver~gt_aprovadores.
                  ADD 1 TO l_niveis.
                ENDDO.

                IF l_niveis EQ lc_nivel OR ls_niveis_aprovadores-hilfm+1(1) EQ lc_nivel. "Nível completa estrutura organizacional..
                  EXIT.
                ENDIF.

                l_pernr = ls_gestores_imediatos-pernimed. "Altera o número pessoal para o número pessoal do gestor e percorre novamente.
                IF l_pernr = '00000000'.
                  EXIT.
                ENDIF.
              ELSE.
                EXIT. " TESTE CAMILA BRAND 14.07.2022 - Sair do loop infinito conforme BUG - 83252
              ENDIF.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

    ENDLOOP.

    SORT me->if_load_approver~gt_aprovadores[] BY kostl nivel.

    LOOP AT me->if_load_approver~gt_aprovadores ASSIGNING FIELD-SYMBOL(<aprovadores>).
      READ TABLE me->if_load_approver~gt_aprovadores INTO DATA(ls_aprovadores_nivel_5) WITH KEY kostl = <aprovadores>-kostl
                                                                                                nivel = 5 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE ls_dados_hierarquia-nomes_aprovadores INTO ls_nome_aprovadores WITH KEY samaccountname = ls_aprovadores_nivel_5-aprovador.
        IF sy-subrc IS INITIAL.
          READ TABLE ls_dados_hierarquia-aprovadores INTO ls_aprovador WITH KEY pernr = ls_nome_aprovadores-pernr.
          IF sy-subrc IS INITIAL.
            READ TABLE ls_dados_hierarquia-tipo_operacao_chave INTO ls_tipo_operacao_chave WITH KEY objid = ls_aprovador-orgeh.
            IF sy-subrc IS INITIAL.
              READ TABLE ls_dados_hierarquia-tipo_operacao INTO ls_tipo_operacao WITH KEY hilfm = ls_tipo_operacao_chave-hilfm.
              IF sy-subrc IS INITIAL.
                <aprovadores>-tp_oper = ls_tipo_operacao-htext.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD if_load_approver~get_aprovadores.
    rt_aprovadores[] = me->if_load_approver~gt_aprovadores[].
  ENDMETHOD.

ENDCLASS.

CLASS lcl_load_approver_e IMPLEMENTATION. "Responsabilidade -> Efetuar carga de aprovadores centro/estoque.
  METHOD constructor.
*** CSB - 01.07.2022 - inicio - us82203
    IF iv_begin_date IS INITIAL.
      APPEND VALUE #( sign = 'I' option = 'GE' low = sy-datum ) TO me->gt_range_date.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'GE' low = iv_begin_date ) TO me->gt_range_date.
    ENDIF.
  ENDMETHOD.

  METHOD if_load_approver~load.

    DATA: lt_pernr            TYPE ztt_pernr,
          ls_pernr            TYPE zpernr,
          ls_dados_hierarquia TYPE zsdados_hierarquia_hcm,
          ls_aprovadores      TYPE zprovcoupa01.

    DATA: l_pernr                    TYPE zpernr,
          l_niveis                   TYPE i,
          l_qtd_niveis               TYPE i,
          l_niveis_aprovador_nivel_1 TYPE i,
          l_proximo_nivel            TYPE i.

    DATA: lc_nivel TYPE hrp1010-hilfm VALUE '6'.

    SELECT *
      FROM zprovcoupa02
      INTO TABLE @DATA(lt_centros).

    CHECK sy-subrc IS INITIAL.

    DELETE lt_centros WHERE pernr_aprovador IS INITIAL.

    DATA(lt_aprovador_centro) = lt_centros[].
    SORT lt_aprovador_centro BY pernr_aprovador.
    DELETE ADJACENT DUPLICATES FROM lt_aprovador_centro COMPARING pernr_aprovador.

    DELETE lt_aprovador_centro WHERE pernr_aprovador IS INITIAL.

    LOOP AT lt_aprovador_centro INTO DATA(ls_aprovador_centro).
      ls_pernr-pernr = ls_aprovador_centro-pernr_aprovador.
      INSERT ls_pernr INTO TABLE lt_pernr.
    ENDLOOP.

    DELETE lt_pernr WHERE pernr = '00000000'.

    CREATE OBJECT if_load_approver~go_hierarquia_organizacional
      EXPORTING
        it_pernr = lt_pernr.

    if_load_approver~go_hierarquia_organizacional->build_data( IMPORTING ev_dados_hierarquia = ls_dados_hierarquia ).

    FREE: lt_pernr, ls_pernr, lt_aprovador_centro.

    IF lt_centros IS NOT INITIAL.
      SELECT werks, name1
        FROM t001w
        INTO TABLE @DATA(lt_centros_nomes)
        FOR ALL ENTRIES IN @lt_centros
        WHERE werks = @lt_centros-werks.
      IF sy-subrc IS INITIAL.
        SORT lt_centros_nomes BY werks.
      ENDIF.
    ENDIF.

    LOOP AT lt_centros INTO DATA(ls_centros).
      CLEAR: ls_aprovadores, l_niveis.

      ls_aprovadores-tp_estrat = 'E'.
      ls_aprovadores-werks     = ls_centros-werks.

      READ TABLE lt_centros_nomes INTO DATA(ls_centros_nomes) WITH KEY werks = ls_centros-werks BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_aprovadores-name1  = ls_centros_nomes-name1.
      ENDIF.

      ls_aprovadores-dt_atual = sy-datum.
      ls_aprovadores-hr_atual = sy-uzeit.
      ls_aprovadores-usnam    = sy-uname.

      READ TABLE ls_dados_hierarquia-aprovadores INTO DATA(ls_aprovador) WITH KEY pernr = ls_centros-pernr_aprovador.
      IF sy-subrc IS INITIAL.
        ls_aprovadores-nivel = '1'. "Primeiro nível é o responsável do centro de custo(CSKS-ABTEI).
        READ TABLE ls_dados_hierarquia-nomes_aprovadores INTO DATA(ls_nome_aprovadores) WITH KEY pernr = ls_aprovador-pernr.
        IF sy-subrc IS INITIAL.
          READ TABLE ls_dados_hierarquia-tipo_operacao_chave INTO DATA(ls_tipo_operacao_chave) WITH KEY objid = ls_aprovador-orgeh.
          IF sy-subrc IS INITIAL.
            READ TABLE ls_dados_hierarquia-tipo_operacao INTO DATA(ls_tipo_operacao) WITH KEY hilfm = ls_tipo_operacao_chave-hilfm.
            IF sy-subrc IS INITIAL.
              ls_aprovadores-tp_oper = ls_tipo_operacao-htext.
            ENDIF.
          ENDIF.
        ENDIF.
        ls_aprovadores-aprovador = ls_nome_aprovadores-samaccountname.
        APPEND ls_aprovadores TO me->if_load_approver~gt_aprovadores.
        ADD 1 TO l_niveis.
      ELSE.
        CONTINUE.
      ENDIF.

      "Primeiro aprovador pode ir além de apenas o primeiro nível, percorre até o nível do seu cargo.
      READ TABLE ls_dados_hierarquia-niveis_aprovadores INTO DATA(ls_niveis_aprovadores) WITH KEY objid = ls_aprovador-stell.
      IF sy-subrc IS INITIAL.
        l_niveis_aprovador_nivel_1 = ls_niveis_aprovadores-hilfm+1(1).
        l_niveis_aprovador_nivel_1 = l_niveis_aprovador_nivel_1 - 1. "Subtrai primeiro nível já adicionado.
        DO l_niveis_aprovador_nivel_1 TIMES.
          ls_aprovadores-nivel = l_niveis + 1.
          APPEND ls_aprovadores TO me->if_load_approver~gt_aprovadores.
          ADD 1 TO l_niveis.
        ENDDO.
      ENDIF.

      l_pernr = ls_aprovador-pernr.
      "Percorrer toda estrutura organizacional
      DO.
        READ TABLE ls_dados_hierarquia-gestores_imediatos INTO DATA(ls_gestores_imediatos) WITH KEY pernr = l_pernr.
        IF sy-subrc IS INITIAL.
          READ TABLE ls_dados_hierarquia-nomes_aprovadores INTO ls_nome_aprovadores WITH KEY pernr = ls_gestores_imediatos-pernimed.
          IF sy-subrc IS INITIAL.
            ls_aprovadores-aprovador = ls_nome_aprovadores-samaccountname.
            READ TABLE ls_dados_hierarquia-aprovadores INTO ls_aprovador WITH KEY pernr = ls_nome_aprovadores-pernr.
            IF sy-subrc IS INITIAL.
              READ TABLE ls_dados_hierarquia-tipo_operacao_chave INTO ls_tipo_operacao_chave WITH KEY objid = ls_aprovador-orgeh.
              IF sy-subrc IS INITIAL.
                READ TABLE ls_dados_hierarquia-tipo_operacao INTO ls_tipo_operacao WITH KEY hilfm = ls_tipo_operacao_chave-hilfm.
                IF sy-subrc IS INITIAL.
                  ls_aprovadores-tp_oper = ls_tipo_operacao-htext.
                ENDIF.
              ENDIF.
              READ TABLE ls_dados_hierarquia-niveis_aprovadores INTO ls_niveis_aprovadores WITH KEY objid = ls_aprovador-stell.
              IF sy-subrc IS INITIAL.

                l_proximo_nivel = ls_niveis_aprovadores-hilfm+1(1).
                l_qtd_niveis    = l_proximo_nivel - l_niveis.

                DO l_qtd_niveis TIMES.
                  ls_aprovadores-nivel = l_niveis + 1.
                  APPEND ls_aprovadores TO me->if_load_approver~gt_aprovadores.
                  ADD 1 TO l_niveis.
                ENDDO.

                IF l_niveis EQ lc_nivel OR ls_niveis_aprovadores-hilfm+1(1) EQ lc_nivel. "Nível completa estrutura organizacional..
                  EXIT.
                ENDIF.

                l_pernr = ls_gestores_imediatos-pernimed. "Altera o número pessoal para o número pessoal do gestor e percorre novamente.
              ELSE.
                EXIT. " TESTE CAMILA BRAND 03.08.2022 - Saqir do loop infinito conforme BUG - 83758
              ENDIF.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDLOOP.

    SORT me->if_load_approver~gt_aprovadores[] BY werks nivel.

    LOOP AT me->if_load_approver~gt_aprovadores ASSIGNING FIELD-SYMBOL(<aprovadores>).
      READ TABLE me->if_load_approver~gt_aprovadores INTO DATA(ls_aprovadores_nivel_5) WITH KEY werks = <aprovadores>-werks
                                                                                                nivel = 5 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE ls_dados_hierarquia-nomes_aprovadores INTO ls_nome_aprovadores WITH KEY samaccountname = ls_aprovadores_nivel_5-aprovador.
        IF sy-subrc IS INITIAL.
          READ TABLE ls_dados_hierarquia-aprovadores INTO ls_aprovador WITH KEY pernr = ls_nome_aprovadores-pernr.
          IF sy-subrc IS INITIAL.
            READ TABLE ls_dados_hierarquia-tipo_operacao_chave INTO ls_tipo_operacao_chave WITH KEY objid = ls_aprovador-orgeh.
            IF sy-subrc IS INITIAL.
              READ TABLE ls_dados_hierarquia-tipo_operacao INTO ls_tipo_operacao WITH KEY hilfm = ls_tipo_operacao_chave-hilfm.
              IF sy-subrc IS INITIAL.
                <aprovadores>-tp_oper = ls_tipo_operacao-htext.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD if_load_approver~get_aprovadores.
    rt_aprovadores[] = me->if_load_approver~gt_aprovadores[].

  ENDMETHOD.

ENDCLASS.

CLASS lcl_approver_table_handler IMPLEMENTATION. "Responsabilidade -> Gerenciar log e armazenar registros na tabela de aprovadores.

  METHOD apply_changes.

    DATA: lt_zprovcoupa01 TYPE STANDARD TABLE OF zprovcoupa01.
    DATA: ls_zprovcoupa01_new LIKE LINE OF lt_zprovcoupa01,
          ls_zprovcoupa01_t   LIKE LINE OF lt_zprovcoupa01,
          ls_zprovcoupa01_s   LIKE LINE OF lt_zprovcoupa01,
          ls_zprovcoupa01_old LIKE LINE OF lt_zprovcoupa01,
          ls_zprovcoupa01_aux LIKE LINE OF lt_zprovcoupa01,
          it_zprovcoupa01_new TYPE zprovcoupa01_tt,
          it_zprovcoupa01_aux TYPE zprovcoupa01_tt.

    IF it_zprovcoupa01 IS INITIAL.
      RETURN.
    ENDIF.

*** CSB - Faço a busca na tabela de exceção.
    SELECT *
      FROM zprovcoupa03
        INTO TABLE @DATA(lt_zprovcoupa03)
          FOR ALL ENTRIES IN @it_zprovcoupa01
            WHERE tp_estrat = @it_zprovcoupa01-tp_estrat
              AND   kostl   = @it_zprovcoupa01-kostl
              AND   werks   = @it_zprovcoupa01-werks.
    "AND   nivel   = @it_zprovcoupa01-nivel. Teste CSB 08082022

    IF lt_zprovcoupa03[] IS NOT INITIAL.
      SORT lt_zprovcoupa03 BY tp_estrat kostl werks nivel.
    ENDIF.

*** CSB - 28.06.2022 - Verifico o que mudou, pego toda a informação da zprovcoupa01 para comparar com a it_zprovcoupa01
    SELECT *
      FROM zprovcoupa01
      INTO TABLE @DATA(lt_zprovcoupa01_old)
      WHERE tp_estrat EQ @p_type.


    SORT lt_zprovcoupa01_old BY  tp_estrat
                                 kostl
                                 werks
                                 nivel.

    LOOP AT it_zprovcoupa01 INTO ls_zprovcoupa01_new .

*** Não pode esxistir na tabela de exceção.
      READ TABLE lt_zprovcoupa03 INTO DATA(ls_zprovcoupa03)  WITH KEY  tp_estrat = ls_zprovcoupa01_new-tp_estrat
                                                                       kostl     = ls_zprovcoupa01_new-kostl
                                                                       werks     = ls_zprovcoupa01_new-werks
                                                                       nivel     = ls_zprovcoupa01_new-nivel BINARY SEARCH.

      IF sy-subrc <> 0.

        READ TABLE lt_zprovcoupa01_old INTO ls_zprovcoupa01_old  WITH KEY tp_estrat = ls_zprovcoupa01_new-tp_estrat
                                                                          kostl     = ls_zprovcoupa01_new-kostl
                                                                          werks     = ls_zprovcoupa01_new-werks
                                                                          nivel     = ls_zprovcoupa01_new-nivel BINARY SEARCH.
        IF  sy-subrc = 0 .

          CLEAR: ls_zprovcoupa01_new-status.

          IF ls_zprovcoupa01_new-ltext      <>  ls_zprovcoupa01_old-ltext.
            ls_zprovcoupa01_new-status = 'S'.
          ENDIF.
          IF ls_zprovcoupa01_new-name1      <>  ls_zprovcoupa01_old-name1.
            ls_zprovcoupa01_new-status = 'S'.
          ENDIF.
          IF ls_zprovcoupa01_new-tp_oper    <>  ls_zprovcoupa01_old-tp_oper.
            ls_zprovcoupa01_new-status = 'S'.
          ENDIF.
          IF ls_zprovcoupa01_new-aprovador  <>  ls_zprovcoupa01_old-aprovador.
            ls_zprovcoupa01_new-status = 'S'.
          ENDIF.

          IF ls_zprovcoupa01_new-status = 'S'.
            APPEND ls_zprovcoupa01_new TO it_zprovcoupa01_aux.
          ENDIF.

        ELSE.
          ls_zprovcoupa01_new-status = 'I'.
          APPEND ls_zprovcoupa01_new TO it_zprovcoupa01_aux.
        ENDIF.
      ENDIF.
      CLEAR: ls_zprovcoupa03,  ls_zprovcoupa01_new, ls_zprovcoupa01_old.
    ENDLOOP.

    IF it_zprovcoupa01_aux[] IS NOT INITIAL.
      CLEAR: ls_zprovcoupa01_new, ls_zprovcoupa01_aux.


      SORT it_zprovcoupa01_aux BY tp_estrat
                                  kostl
                                  werks
                                  nivel.

      DELETE ADJACENT DUPLICATES FROM it_zprovcoupa01_aux COMPARING  tp_estrat
                                                                     kostl
                                                                     werks.
      " Append dos registros modificados na nova tabela.
      LOOP AT it_zprovcoupa01_aux INTO ls_zprovcoupa01_aux WHERE status =  'S'.

        LOOP AT it_zprovcoupa01 INTO ls_zprovcoupa01_new WHERE  tp_estrat =  ls_zprovcoupa01_aux-tp_estrat
                                                            AND kostl     =  ls_zprovcoupa01_aux-kostl
                                                            AND werks     =  ls_zprovcoupa01_aux-werks.

          ls_zprovcoupa01_new-status = 'S'.

          APPEND ls_zprovcoupa01_new TO it_zprovcoupa01_new.

        ENDLOOP.
        CLEAR: ls_zprovcoupa01_new, ls_zprovcoupa01_aux.
      ENDLOOP.


      " Append nos registros novos na nova tabela.
      LOOP AT it_zprovcoupa01_aux INTO ls_zprovcoupa01_aux WHERE status =  'I'.
        LOOP AT it_zprovcoupa01 INTO ls_zprovcoupa01_new WHERE  tp_estrat =  ls_zprovcoupa01_aux-tp_estrat
                                                            AND kostl     =  ls_zprovcoupa01_aux-kostl
                                                            AND werks     =  ls_zprovcoupa01_aux-werks.

          ls_zprovcoupa01_new-status = 'S'.

          APPEND ls_zprovcoupa01_new TO it_zprovcoupa01_new.
        ENDLOOP.
        CLEAR: ls_zprovcoupa01_new, ls_zprovcoupa01_aux.
      ENDLOOP.
    ENDIF.

* Código antigo
*    SELECT *
*      FROM zprovcoupa03
*      INTO TABLE @DATA(lt_zprovcoupa03)
*      FOR ALL ENTRIES IN @it_zprovcoupa01
*      WHERE tp_estrat = @it_zprovcoupa01-tp_estrat
*      AND   kostl     = @it_zprovcoupa01-kostl
*      AND   werks     = @it_zprovcoupa01-werks
*      AND   nivel     = @it_zprovcoupa01-nivel.
*    IF sy-subrc IS INITIAL.
*      SORT lt_zprovcoupa03 BY tp_estrat kostl werks nivel.
*    ENDIF.
*
*    lt_zprovcoupa01[] = it_zprovcoupa01[].
*
*    LOOP AT lt_zprovcoupa01 ASSIGNING FIELD-SYMBOL(<zprovcoupa01>).
*      READ TABLE lt_zprovcoupa03 INTO DATA(ls_zprovcoupa03) WITH KEY tp_estrat = <zprovcoupa01>-tp_estrat
*                                                                     kostl     = <zprovcoupa01>-kostl
*                                                                     werks     = <zprovcoupa01>-werks
*                                                                     nivel     = <zprovcoupa01>-nivel BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        MOVE-CORRESPONDING ls_zprovcoupa03 TO <zprovcoupa01>.
*      ENDIF.
*    ENDLOOP.
*
*    MODIFY zprovcoupa01 FROM TABLE lt_zprovcoupa01.


*** Aqui ele trata exceção - Feita pela transação de exceção.
    IF lt_zprovcoupa03[] IS NOT INITIAL.

      CLEAR: it_zprovcoupa01_aux[], ls_zprovcoupa01_new,
             ls_zprovcoupa03, ls_zprovcoupa01_old.

      LOOP AT lt_zprovcoupa03 INTO ls_zprovcoupa03.
        READ TABLE lt_zprovcoupa01_old INTO ls_zprovcoupa01_old  WITH KEY tp_estrat = ls_zprovcoupa03-tp_estrat
                                                                          kostl     = ls_zprovcoupa03-kostl
                                                                          werks     = ls_zprovcoupa03-werks
                                                                          nivel     = ls_zprovcoupa03-nivel BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          IF ls_zprovcoupa03-ltext      <>  ls_zprovcoupa01_old-ltext.
            ls_zprovcoupa01_new-status = 'S'.
          ENDIF.
          IF ls_zprovcoupa03-name1      <>  ls_zprovcoupa01_old-name1.
            ls_zprovcoupa01_new-status = 'S'.
          ENDIF.
          IF ls_zprovcoupa03-tp_oper    <>  ls_zprovcoupa01_old-tp_oper.
            ls_zprovcoupa01_new-status = 'S'.
          ENDIF.
          IF ls_zprovcoupa03-aprovador  <>  ls_zprovcoupa01_old-aprovador.
            ls_zprovcoupa01_new-status = 'S'.
          ENDIF.

          IF ls_zprovcoupa01_new-status = 'S'.
            MOVE-CORRESPONDING ls_zprovcoupa03 TO ls_zprovcoupa01_new.
            APPEND ls_zprovcoupa01_new TO it_zprovcoupa01_aux.
          ENDIF.
        ELSE.
          ls_zprovcoupa01_new-status = 'I'.
          MOVE-CORRESPONDING ls_zprovcoupa03 TO ls_zprovcoupa01_new.
          APPEND ls_zprovcoupa01_new TO it_zprovcoupa01_aux.
        ENDIF.
        CLEAR: ls_zprovcoupa01_new, ls_zprovcoupa01_old , ls_zprovcoupa03.
      ENDLOOP.

      " Tratar novos e antigos registros.

      IF it_zprovcoupa01_aux[] IS NOT INITIAL.

        CLEAR: ls_zprovcoupa01_aux, ls_zprovcoupa01_new.

        SORT it_zprovcoupa01_aux BY tp_estrat
                                    kostl
                                    werks
                                    nivel.

        DELETE ADJACENT DUPLICATES FROM it_zprovcoupa01_aux COMPARING  tp_estrat
                                                                       kostl
                                                                       werks.
        " Append dos registros modificados na nova tabela.
        LOOP AT it_zprovcoupa01_aux INTO ls_zprovcoupa01_aux WHERE status =  'S'.
          LOOP AT lt_zprovcoupa03 INTO ls_zprovcoupa01_new WHERE  tp_estrat =  ls_zprovcoupa01_aux-tp_estrat
                                                              AND kostl     =  ls_zprovcoupa01_aux-kostl
                                                              AND werks     =  ls_zprovcoupa01_aux-werks.

            ls_zprovcoupa01_new-status = 'S'.

            APPEND ls_zprovcoupa01_new TO it_zprovcoupa01_new.
          ENDLOOP.
          CLEAR: ls_zprovcoupa01_aux, ls_zprovcoupa01_new.
        ENDLOOP.


        " Append nos registros novos na nova tabela.
        LOOP AT it_zprovcoupa01_aux INTO ls_zprovcoupa01_aux WHERE status =  'I'.
          LOOP AT lt_zprovcoupa03 INTO ls_zprovcoupa01_new WHERE  tp_estrat =  ls_zprovcoupa01_aux-tp_estrat
                                                              AND kostl     =  ls_zprovcoupa01_aux-kostl
                                                              AND werks     =  ls_zprovcoupa01_aux-werks.

            ls_zprovcoupa01_new-status = 'S'.

            APPEND ls_zprovcoupa01_new TO it_zprovcoupa01_new.
          ENDLOOP.
          CLEAR: ls_zprovcoupa01_aux, ls_zprovcoupa01_new.
        ENDLOOP.
      ENDIF.

    ENDIF.

*** CSB - 28.06.2022 - Verifico se tem registo na tabela antes de salvar o LOG
    IF it_zprovcoupa01_new[] IS NOT INITIAL.
      SORT it_zprovcoupa01_new BY tp_estrat
                                  kostl
                                  werks
                                  nivel.

      me->save_log( ). " Salvo o LOG.
    ENDIF.

*** CSB - 28.06.2022 - O que não sofreu alteração eu mudo o status para nulo
    " Pego tudo o que esta na tabela com 'S' vejo se esta vindo com alteração.
    "Senão tiver alteração mudo para nulo o status.

    SELECT *
      FROM zprovcoupa01
      INTO TABLE @DATA(lt_zprovcoupa01_s)
      WHERE status = 'S'
        AND tp_estrat EQ @p_type.


    IF lt_zprovcoupa01_s[] IS NOT INITIAL AND it_zprovcoupa01_new[] IS NOT INITIAL.

      SORT lt_zprovcoupa01_s BY tp_estrat
                                kostl
                                werks
                                nivel.

      CLEAR: ls_zprovcoupa01_s.
      LOOP AT lt_zprovcoupa01_s INTO ls_zprovcoupa01_s.
        READ TABLE it_zprovcoupa01_new INTO DATA(ls_zprovcoupa01) WITH KEY tp_estrat = ls_zprovcoupa01_s-tp_estrat
                                                                           kostl     = ls_zprovcoupa01_s-kostl
                                                                           werks     = ls_zprovcoupa01_s-werks
                                                                           nivel     = ls_zprovcoupa01_s-nivel BINARY SEARCH.
        IF sy-subrc <> 0.
          CLEAR: ls_zprovcoupa01_s-status.
          MODIFY zprovcoupa01 FROM ls_zprovcoupa01_s.
        ENDIF.
        CLEAR: ls_zprovcoupa01_s, ls_zprovcoupa01.
      ENDLOOP.
    ENDIF.
*** CSB - 28.06.2022 - O que não sofreu alteração eu mudo o status para nulo

**** NOVA REGRA: 14.07.2022 - CBRAND
*Comentei 08.08.2022 - CBRAND
*    CLEAR: lt_zprovcoupa03.
*
*    SELECT *
*    FROM zprovcoupa03
*      INTO TABLE lt_zprovcoupa03
*        FOR ALL ENTRIES IN it_zprovcoupa01
*          WHERE tp_estrat = it_zprovcoupa01-tp_estrat
*            AND   kostl   = it_zprovcoupa01-kostl
*            AND   werks   = it_zprovcoupa01-werks.
*
*
*    IF lt_zprovcoupa03[] IS NOT INITIAL.
*
*      CLEAR: it_zprovcoupa01_aux[], ls_zprovcoupa01_new,
*             ls_zprovcoupa03, ls_zprovcoupa01_old.
*
*      LOOP AT lt_zprovcoupa03 INTO ls_zprovcoupa03.
*
*        READ TABLE lt_zprovcoupa01_old INTO ls_zprovcoupa01_old  WITH KEY tp_estrat = ls_zprovcoupa03-tp_estrat
*                                                                          kostl     = ls_zprovcoupa03-kostl
*                                                                          werks     = ls_zprovcoupa03-werks
*                                                                          nivel     = ls_zprovcoupa03-nivel BINARY SEARCH.
*        IF sy-subrc IS NOT INITIAL.
*          ls_zprovcoupa01_new-status = 'S'.
*          MOVE-CORRESPONDING ls_zprovcoupa03 TO ls_zprovcoupa01_new.
*          APPEND ls_zprovcoupa01_new TO it_zprovcoupa01_new.
*        ENDIF.
*        CLEAR: ls_zprovcoupa01_new, ls_zprovcoupa01_old , ls_zprovcoupa03.
*      ENDLOOP.
*    ENDIF.
**** NOVA REGRA TESTE : 14.07.2022 - CBRAND
*Comentei 08.08.2022 - CBRAND

    IF it_zprovcoupa01_new IS NOT INITIAL.
      MODIFY zprovcoupa01 FROM TABLE it_zprovcoupa01_new."lt_zprovcoupa01.
    ENDIF.
    CHECK sy-subrc IS INITIAL.

    MESSAGE TEXT-m03 TYPE 'I'. "text-m03 Aprovadores inseridos com êxito!!

  ENDMETHOD.

  METHOD save_log.

    DATA: lt_zprovcoupa01     TYPE TABLE OF zprovcoupa01,
          lt_zprovcoupa01_log TYPE TABLE OF zprovcoupa01_log,
          lw_zprovcoupa01_log LIKE LINE OF lt_zprovcoupa01_log.

    DELETE FROM zprovcoupa01_log.

    SELECT *
      FROM zprovcoupa01
      INTO TABLE lt_zprovcoupa01.

    CHECK sy-subrc IS INITIAL.

    "INSERT zprovcoupa01_log FROM TABLE lt_zprovcoupa01. "CSB - 28.06.2022

    SORT lt_zprovcoupa01 BY  tp_estrat
                             kostl
                             werks
                             nivel.

    LOOP AT lt_zprovcoupa01 INTO DATA(lw_zprovcoupa01).

      MOVE-CORRESPONDING lw_zprovcoupa01 TO lw_zprovcoupa01_log.
      lw_zprovcoupa01_log-dt_atual =  sy-datum.
      lw_zprovcoupa01_log-hr_atual = sy-uzeit.
      lw_zprovcoupa01_log-usnam    = sy-uname.

      MODIFY zprovcoupa01_log FROM lw_zprovcoupa01_log.
      CLEAR: lw_zprovcoupa01_log.
    ENDLOOP.

    MESSAGE TEXT-m02 TYPE 'I'. "text-m02 Registros inseridos no log com êxito!!

  ENDMETHOD.

  METHOD constructor.
  ENDMETHOD.

ENDCLASS.
