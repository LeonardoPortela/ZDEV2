FUNCTION z_sol_baixa_imob_lista.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) TYPE  SY-UNAME
*"     VALUE(ANLN1) TYPE  ANLA-ANLN1 OPTIONAL
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_SOLICITACOES STRUCTURE  ZAAS0001
*"      T_ESTRA STRUCTURE  ZAAS0002
*"      T_DETALHES STRUCTURE  ZAAS0003
*"----------------------------------------------------------------------
  TYPE-POOLS: icon.
*"----------------------------------------------------------------------
*"  DATA
*"----------------------------------------------------------------------
  DATA: vflg_ico(1),
        vflag(1),
        e_row_id       TYPE sy-tabix,
        v_ic_set_state TYPE c,
        v_append_ordem TYPE c.

  FIELD-SYMBOLS: <wl_007> TYPE zaa007.

*"----------------------------------------------------------------------
*"  START-OF-SELECTION
*"----------------------------------------------------------------------
  SELECT * FROM zaa007
    INTO TABLE @DATA(tg_zaa007)
    WHERE status EQ ''.

  LOOP AT tg_zaa007[] ASSIGNING <wl_007>.
    IF ( <wl_007>-anln2 EQ '0000' ).
      CLEAR: <wl_007>-anln2.
    ENDIF.
  ENDLOOP.

  IF ( tg_zaa007 IS NOT INITIAL ).

    SELECT * FROM zaa004
      INTO TABLE @DATA(tg_zaa004)
      FOR ALL ENTRIES IN @tg_zaa007
      WHERE bukrs EQ @tg_zaa007-bukrs
        AND gsber EQ @tg_zaa007-werks
        AND kostl EQ @tg_zaa007-kostl
        AND fluxo_baixa EQ 'X'.

    SELECT * FROM zaa008
      INTO TABLE @DATA(tg_zaa008)
      FOR ALL ENTRIES IN @tg_zaa007
      WHERE bukrs EQ @tg_zaa007-bukrs
        AND werks EQ @tg_zaa007-werks
        AND anln1 EQ @tg_zaa007-anln1
        AND anln2 EQ @tg_zaa007-anln2.

    DELETE tg_zaa008 WHERE dt_estorno IS NOT INITIAL.

    SELECT werks, name1
      FROM t001w
      INTO TABLE @DATA(tl_t001w)
      FOR ALL ENTRIES IN @tg_zaa007
      WHERE werks EQ @tg_zaa007-werks.

    SORT tg_zaa004[] BY bukrs gsber kostl nivel_aa  ASCENDING.
    SORT tg_zaa008[] BY bukrs werks anln1 nivel     ASCENDING.
    SORT tl_t001w[]  BY werks ASCENDING.

    IF ( tg_zaa004 IS NOT INITIAL ).

      LOOP AT tg_zaa007[] INTO DATA(wl_zaa007).

        CLEAR: v_append_ordem, vflg_ico.

        READ TABLE tl_t001w INTO DATA(wl_t001w) WITH KEY werks = wl_zaa007-werks.

        t_solicitacoes-empresa      = wl_zaa007-bukrs.
        t_solicitacoes-filial       =  wl_t001w-werks .
*        T_SOLICITACOES-FILIAL       = |{ WL_T001W-WERKS } - { WL_T001W-NAME1 }|.
        t_solicitacoes-imobilizado  = wl_zaa007-anln1.
        t_solicitacoes-txt50        = wl_zaa007-txt50.
        t_solicitacoes-usuario      = wl_zaa007-solicitante.
        t_solicitacoes-dt_solicita  = wl_zaa007-dt_solicitacao.

        t_solicitacoes-kostl        = wl_zaa007-kostl.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = t_solicitacoes-kostl
          IMPORTING
            output = t_solicitacoes-kostl.

        LOOP AT tg_zaa004[] INTO DATA(wl_zaa004).

          "|Valida empresa/filial/centro de custo.
          IF ( wl_zaa007-bukrs NE wl_zaa004-bukrs ).
            CONTINUE.
          ELSEIF ( wl_zaa007-werks NE wl_zaa004-gsber ).
            CONTINUE.
          ELSEIF ( wl_zaa007-kostl NE wl_zaa004-kostl ).
            CONTINUE.
          ENDIF.

          "|Valida Data de validade da estratégia.
          IF  ( wl_zaa004-dt_ini LT sy-datum ) AND
              ( wl_zaa004-dt_fim GT sy-datum )

            OR
              ( wl_zaa004-dt_ini EQ sy-datum ) AND
              ( wl_zaa004-dt_fim GT sy-datum )
            OR
              ( wl_zaa004-dt_ini LT sy-datum ) AND
              ( wl_zaa004-dt_fim EQ sy-datum ).

            "Estratégia Válida.

          ELSE.

            " Estratégia inválida.
            CONTINUE.
          ENDIF.

          t_estra-bukrs     = wl_zaa004-bukrs.
          t_estra-gsber     = wl_zaa004-gsber.
          t_estra-kostl     = wl_zaa004-kostl.
          t_estra-anln1     = wl_zaa007-anln1.
          t_estra-anln2     = wl_zaa007-anln2.
          t_estra-aprovador = wl_zaa004-usuar_aa.
          t_estra-nivel     = wl_zaa004-nivel_aa.

          READ TABLE tg_zaa008[] INTO DATA(wl_zaa008) WITH KEY bukrs  = wl_zaa007-bukrs
                                                               werks  = wl_zaa007-werks
                                                               anln1  = wl_zaa007-anln1
                                                               anln2  = wl_zaa007-anln2
                                                               nivel  = wl_zaa004-nivel_aa.

          IF ( wl_zaa008 IS NOT INITIAL ).
            t_estra-estado    = icon_checked.
            t_estra-opcoes    = icon_system_undo.
            vflg_ico          = 'N'.
            t_estra-aprovador = wl_zaa008-aprovador.
          ELSEIF vflg_ico EQ 'S'.
            t_estra-estado    = icon_led_yellow.
            t_estra-opcoes    = ''.
            t_estra-aprovador = wl_zaa004-usuar_aa.
          ELSE.
            IF ( v_usuario NE wl_zaa004-usuar_aa ).
              t_estra-estado  = icon_led_yellow.
              t_estra-opcoes  = ''.
            ELSE.
              t_estra-estado  = icon_led_yellow.
              t_estra-opcoes  = icon_set_state.
            ENDIF.
            vflg_ico          = 'X'.
            t_estra-aprovador = wl_zaa004-usuar_aa.
          ENDIF.

          IF ( vflg_ico EQ 'X' ).
            vflg_ico = 'S'.
          ENDIF.

          APPEND t_estra.
          CLEAR: wl_zaa008.

        ENDLOOP.

        IF ( t_estra IS INITIAL ).
          CONTINUE.
        ENDIF.

        DATA(count_nivel) = 0.

        LOOP AT t_estra[] INTO DATA(wl_estra) WHERE bukrs     EQ wl_zaa007-bukrs
                                                AND gsber     EQ wl_zaa007-werks
                                                AND kostl     EQ wl_zaa007-kostl
                                                AND anln1     EQ wl_zaa007-anln1
                                                AND aprovador EQ v_usuario.

          IF ( wl_estra-aprovador EQ v_usuario ).
            ADD 1 TO count_nivel.
          ENDIF.

          IF ( e_row_id IS NOT INITIAL ) AND ( sy-tabix NE e_row_id + 1 ).
            EXIT.
          ELSEIF ( e_row_id IS NOT INITIAL ) AND ( sy-tabix EQ e_row_id + 1 ).
            IF ( t_estra-opcoes NE icon_system_undo ).
              v_ic_set_state = 'X'.
            ENDIF.

            e_row_id = sy-tabix.
          ELSE.
            e_row_id = sy-tabix.
          ENDIF.

          LOOP AT t_estra[] INTO wl_estra WHERE bukrs EQ wl_zaa007-bukrs
                                            AND anln1 EQ wl_zaa007-anln1.

            IF ( wl_estra-estado NE icon_checked ) AND ( sy-tabix LT e_row_id ).
              v_append_ordem = 'N'.
            ENDIF.

          ENDLOOP.

        ENDLOOP.

        IF ( count_nivel > 1 ).
          CLEAR v_append_ordem.
        ELSEIF ( count_nivel EQ 0 ).
          v_append_ordem = 'N'.
        ENDIF.

        IF ( v_append_ordem EQ 'N' ).
          CONTINUE.
        ENDIF.

        APPEND t_solicitacoes.
        CLEAR: vflg_ico.

      ENDLOOP.

      IF ( t_solicitacoes[] IS NOT INITIAL ).

        SORT t_estra[] BY bukrs gsber nivel.
        LOOP AT t_solicitacoes[] INTO DATA(wl_solicitacoes).
          CLEAR:vflag.
          LOOP AT t_estra[] INTO wl_estra WHERE bukrs EQ wl_solicitacoes-empresa
                                            AND anln1 EQ wl_solicitacoes-imobilizado.
            vflag = 'X'.
            EXIT.

          ENDLOOP.

          SORT t_estra[] BY bukrs gsber kostl anln1.

          IF ( vflag EQ 'X' ).
            LOOP AT tg_zaa007 INTO wl_zaa007 WHERE anln1 EQ wl_solicitacoes-imobilizado.
              MOVE-CORRESPONDING wl_zaa007 TO t_detalhes.
              APPEND t_detalhes.
            ENDLOOP.
          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDIF.

  IF t_solicitacoes[] IS NOT INITIAL.

**********************************************************************
    "128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA

    IF sy-cprog = 'ZAA15'.

      DATA: tmp_ZAAS0002 TYPE STANDARD TABLE OF zaas0002 INITIAL SIZE 0.

*      LOOP AT t_estra[] ASSIGNING FIELD-SYMBOL(<get_del>) WHERE estado = '@01@'.
*        APPEND <get_del> TO tmp_ZAAS0002.
*      ENDLOOP.

      SORT t_estra[] BY aprovador."aprovador estado opcoes ASCENDING.
      DELETE t_estra[] WHERE aprovador <> sy-uname.
      DELETE t_estra[] WHERE estado = '@01@' and estado = '@5D@'.

*      SORT t_solicitacoes[] BY usuario.
*      SORT tmp_ZAAS0002[] BY aprovador.
*
*      DELETE t_solicitacoes[] WHERE usuario <> sy-uname.
*      DELETE tmp_ZAAS0002[] WHERE aprovador <> sy-uname.

*      LOOP AT t_solicitacoes[] INTO DATA(_get).
*        LOOP AT tmp_ZAAS0002 INTO DATA(_del) WHERE bukrs = _get-empresa AND anln1 = _get-imobilizado AND gsber = _get-filial AND aprovador = _get-usuario.
*          IF ( _del-bukrs IS NOT INITIAL AND _get-empresa IS NOT INITIAL AND  _del-anln1 IS NOT INITIAL AND  _get-imobilizado IS NOT INITIAL AND  _del-gsber IS NOT INITIAL AND  _get-filial IS NOT INITIAL                  AND _del-aprovador
*IS NOT INITIAL AND  _get-usuario IS NOT INITIAL )
*            AND
*            ( _del-bukrs = _get-empresa AND _del-anln1 = _get-imobilizado AND _del-gsber = _get-filial AND _del-aprovador = _get-usuario ).
*            DELETE t_solicitacoes[] WHERE empresa = _get-empresa AND filial = _get-filial AND imobilizado = _get-imobilizado AND usuario = _get-usuario AND kostl = _get-kostl.
*          ENDIF.
*        ENDLOOP.
*      ENDLOOP.


*      CLEAR: tmp_ZAAS0002, tmp_ZAAS0002[].
    ENDIF.

**********************************************************************

    msg = 'Sucesso'.
  ELSE.
    msg = 'Não há solicitações à aprovar.'.
  ENDIF.

ENDFUNCTION.
