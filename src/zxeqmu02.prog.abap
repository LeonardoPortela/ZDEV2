*&---------------------------------------------------------------------*
*&  Include           ZXEQMU02
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*----------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 24/09/2024  |DEVK9A2791  |NSEGANTIN      |Eqpt Tp 01, Obrig Imobiliz*
*&---------------------------------------------------------------------*
IF 'IE02_IE01_IE31' CS sy-tcode.

  DATA: "TL_ZPMR0001 TYPE TABLE OF ZPMR0001,
    wl_zequi_emprestimo TYPE zequi_emprestimo,
    wl_mpos             TYPE mpos,
    ls_adrc             TYPE adrc,
    ls_addr1_data       TYPE addr1_data,
    ws_dados            TYPE char50,
    rg_mptyp            TYPE RANGE OF mptyp,
    ztipo_contador      TYPE dzeieh,
    p_erro              TYPE char01,
    p_check_od          TYPE char01 VALUE abap_false,
    p_check_vida_ut     TYPE char01 VALUE abap_false,
    p_messagem          TYPE char255,
    p_messagem1         TYPE char255,
    p_messagem2         TYPE char255,
    it_zpmt0074         TYPE TABLE OF zpmt0074,
    it_zpmt0074_aux     TYPE TABLE OF zpmt0074,
    rg_atnam            TYPE RANGE OF zpmt0074-atnam,
    wl_zpmr0001         TYPE zpmr0001.

  DATA: lv_soma_combust TYPE fleet-tq_combustivel_1.

  FIELD-SYMBOLS: <fs_diadr>     TYPE diadr,
                 <fs_diadr_aux> TYPE diadr,

**  Begin of   "FF #107313
                 <fs_fleet>     TYPE fleet.
  IF  data_equi-eqtyp EQ '1'
      OR data_equi-eqtyp EQ '2'
      OR data_equi-eqtyp EQ '3'
      OR data_equi-eqtyp EQ '4'.

    ASSIGN ('(SAPMIEQ0)FLEET') TO <fs_fleet>.

    CLEAR: lv_soma_combust.
    IF <fs_fleet>-tq_combustivel_1 > 0.
      ADD <fs_fleet>-tq_combustivel_1 TO lv_soma_combust.
    ENDIF.

    IF <fs_fleet>-tq_combustivel_2 > 0.
      ADD <fs_fleet>-tq_combustivel_2 TO lv_soma_combust.
    ENDIF.

    IF <fs_fleet>-tq_combustivel_3 > 0.
      ADD <fs_fleet>-tq_combustivel_3 TO lv_soma_combust.
    ENDIF.

    IF lv_soma_combust IS NOT INITIAL.
      <fs_fleet>-key_num = lv_soma_combust.
      CONDENSE <fs_fleet>-key_num NO-GAPS.
    ENDIF.

  ENDIF.
** End of FF

*** Inicio - Rubenilson - 14.01.24 - BUG163330
  IF  data_equi-eqtyp EQ '1'
      OR data_equi-eqtyp EQ '2'
      OR data_equi-eqtyp EQ '3'
      OR data_equi-eqtyp EQ '4'.
*      OR data_equi-eqtyp EQ 'A'. "FF #190850
    ASSIGN ('(SAPMIEQ0)FLEET') TO <fs_fleet>.
    IF <fs_fleet> IS ASSIGNED.

*** Inicio - Rubenilson - 06.08.2025 - BUG186983
      SELECT *
        FROM ztparam
        INTO @DATA(ls_param)
        UP TO 1 ROWS
        WHERE param = 'TP_IMPLEM'
          AND zval  = @<fs_fleet>-fleet_cat.
        ENDSELECT.
        IF sy-subrc IS NOT INITIAL.
*** Fim - Rubenilson - 06.08.2025 - BUG186983

          IF <fs_fleet>-key_num IS INITIAL OR <fs_fleet>-key_num EQ 0.
            CLEAR: sy-msgv1,
                   sy-msgv2.

            sy-msgid = 'SD'.
            sy-msgno = '024'.
            sy-msgty = 'E'.
            sy-msgv1 = 'Favor preencher valor do Tq Comb'.
            RAISE posting_not_allowed_ext.
          ELSE.

            SELECT tq_comb,tolerancia
              FROM zpmr0001
              INTO @DATA(ls_zpmr0001)
              UP TO 1 ROWS
              WHERE herst = @data_equi-herst
                AND typbz = @data_equi-typbz
                AND class_oper = @data_equi-eqart. "Ajustar validação do tanque no cadastro da ZPM0017 - BG #184192
            ENDSELECT.
            IF sy-subrc IS INITIAL.

              IF ls_zpmr0001-tolerancia > 0.
                DATA(lv_total) = ls_zpmr0001-tq_comb + ( ls_zpmr0001-tq_comb * ( ls_zpmr0001-tolerancia / 100 ) ).
              ELSE.
                lv_total = ls_zpmr0001-tq_comb.
              ENDIF.

              IF lv_total < <fs_fleet>-key_num.
                sy-msgid = 'SD'.
                sy-msgno = '024'.
                sy-msgty = 'E'.
                sy-msgv1 = 'Valor de Tq Comb informado'.
                sy-msgv2 = 'maior que capacidade cadastrada!'.

                RAISE posting_not_allowed_ext.
              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.
*** Fim - Rubenilson - 14.01.24 - BUG163330

*-US 158036-26-11-2024-#158036-RJF-Início
    IF  data_equi-eqtyp IS NOT INITIAL
    AND data_equi-eqart IS NOT INITIAL
    AND data_iloa-kostl IS NOT INITIAL.

      IF  data_equi-eqtyp EQ '1'
          OR data_equi-eqtyp EQ '2'
          OR data_equi-eqtyp EQ '3'
          OR data_equi-eqtyp EQ '4'.
*          OR data_equi-eqtyp EQ 'A'. "FF #190614

        DATA: ln_kostl(10)  TYPE n,
              ln_kostlm(10) TYPE n.

        ln_kostlm = data_iloa-kostl.

        SELECT * FROM zpmt0001
        INTO TABLE @DATA(it_ZPMT0001)
        WHERE eqtyp  EQ @data_equi-eqtyp
          AND eqart  EQ @data_equi-eqart
          AND kostlg EQ @ln_kostlm+6(4).

          IF sy-subrc IS INITIAL.
            SORT it_ZPMT0001 BY eqtyp eqart kostlg.
            LOOP AT it_ZPMT0001 INTO DATA(wa_PMT0001) WHERE eqtyp = data_equi-eqtyp
                                                        AND eqart = data_equi-eqart.
              ln_kostl  = wa_PMT0001-kostlg.
              IF sy-subrc IS NOT INITIAL OR ln_kostl NE ln_kostlm+6(4).
                MESSAGE 'Verificar categoria informada x centro de custos tabela ZPM0010.' TYPE 'I'.
                RAISE posting_not_allowed.
              ENDIF.
            ENDLOOP.

          ELSE.
            MESSAGE 'Verificar categoria informada x centro de custos tabela ZPM0010.' TYPE 'I'.
            RAISE posting_not_allowed.
          ENDIF.
        ENDIF.
      ENDIF.
*-US 158036-26-11-2024-#158036-RJF-Fim


*  ws_dados = '(SAPLSZA1)DIADR'.
*  ASSIGN (ws_dados) TO  <fs_diadr>.

      ASSIGN ('(SAPLIPAR)DIADR') TO  <fs_diadr>.

      IF  data_equi-eqtyp EQ '1'
          OR data_equi-eqtyp EQ '2'
          OR data_equi-eqtyp EQ '3'
          OR data_equi-eqtyp EQ '4'.

        IF data_equi-herst IS INITIAL
        OR data_equi-typbz IS INITIAL.
          MESSAGE 'Os campos "Fabricante" e "Denomin. tipo" são de preenchimento obrigatório.' TYPE 'I'.
          RAISE posting_not_allowed.
        ELSE.
          IF data_equi-eqtyp EQ '1'
            OR data_equi-eqtyp EQ '2'
            OR data_equi-eqtyp EQ '3'
            OR data_equi-eqtyp EQ '4'.

            SELECT SINGLE *
              FROM zpmr0001
              INTO wl_zpmr0001
              WHERE herst = data_equi-herst
               AND  typbz = data_equi-typbz.

              IF sy-subrc IS NOT INITIAL.
                MESSAGE 'Os campos "Fabricante" e "Denomin. tipo" devem estar cadastrados na transação ZPM0017.' TYPE 'I'.
                RAISE posting_not_allowed.
              ENDIF.

              IF data_equi-eqtyp EQ '1'
                OR data_equi-eqtyp EQ '2'
                OR data_equi-eqtyp EQ '3'
                OR data_equi-eqtyp EQ '4'.


                "Check enderenço.
                IF <fs_diadr> IS ASSIGNED.
*          verificar o cadastro endereço.
                  IF <fs_diadr>-name1 IS INITIAL.
                    MESSAGE 'Informe o nome do endereço' TYPE 'I'.
                    RAISE posting_not_allowed.
                  ENDIF.
                ENDIF.

                "Check centro de trabalho.
                IF data_equz-gewrk IS INITIAL.
                  MESSAGE 'Os campos "CenTrab respon" são de preenchimento obrigatório.' TYPE 'I'.
                  RAISE posting_not_allowed.
                ENDIF.

                "Check grupo.
                IF data_equz-ingrp IS INITIAL.
                  MESSAGE 'Os campos "Grp.plnj.PM" são de preenchimento obrigatório.' TYPE 'I'.
                  RAISE posting_not_allowed.
                ENDIF.


                "Verificar se esta cadastrado os parametro ponto de medição.
                APPEND VALUE #( sign = 'I' option = 'EQ' low = 'ODOMETRO' ) TO rg_atnam.
                APPEND VALUE #( sign = 'I' option = 'EQ' low = 'HORIMETRO' ) TO rg_atnam.
                FREE: it_zpmt0074.
                SELECT * FROM zpmt0074 INTO TABLE it_zpmt0074
                  WHERE fleet_cat EQ data_equi-eqart
                    AND herst EQ data_equi-herst
                    AND eqtyp EQ data_equi-eqtyp
                    AND typbz EQ data_equi-typbz
                    AND klasse EQ  main_class.


                  IF sy-subrc NE 0.
                    MESSAGE i024(sd) WITH 'Não existe parametro ponto medição cadastrado '
                                          'Odometro/Horimetro, realizar '
                                          'cadastro transação ZPM0093'.
                    RAISE posting_not_allowed.
                  ELSE.
                    CLEAR:  p_erro, p_messagem, p_messagem1, ztipo_contador.
                    FREE: it_zpmt0074_aux.
                    it_zpmt0074_aux = it_zpmt0074.

                    SORT it_zpmt0074_aux BY atnam.
                    DELETE it_zpmt0074_aux WHERE atnam  NOT IN rg_atnam.

                    IF it_zpmt0074_aux IS INITIAL.
                      MESSAGE i024(sd) WITH 'Não existe parametro ponto medição cadastrado '
                                            'Odometro/Horimetro, realizar '
                                            'cadastro transação ZPM0093'.
                      RAISE posting_not_allowed.
                    ENDIF.

                    LOOP AT it_zpmt0074_aux ASSIGNING FIELD-SYMBOL(<lw_zpmt0074>).

                      IF <lw_zpmt0074>-atnam EQ 'ODOMETRO' OR <lw_zpmt0074>-atnam EQ 'HORIMETRO'.
                        IF <lw_zpmt0074>-atnam EQ 'ODOMETRO'.
                          ztipo_contador = 'KM'.
                        ELSE.
                          ztipo_contador = 'H'.
                        ENDIF.


                        CASE <lw_zpmt0074>-pttxt.
                          WHEN 'ODOMETRO' OR 'HORIMETRO' OR 'HORÍMETRO'.
                            p_check_od = abap_true.
                          WHEN 'VIDA UTIL' OR 'VIDA ÚTIL'.
                            p_check_vida_ut = abap_true.
                          WHEN OTHERS.
                        ENDCASE.

                      ELSE.
                        p_messagem = 'Não existe parametro ponto medição cadastrado'.
                        p_messagem1 = 'ODOMETRO/HORIMETRO, realizar cadastro '.
                        p_messagem2 = 'transação ZPM0093'.
                        p_erro = abap_true.
                        EXIT.
                      ENDIF.
                    ENDLOOP.

                    IF p_check_od IS INITIAL.
                      p_messagem = 'Não existe parametro ponto medição cadastrado'.
                      p_messagem1 = 'ODOMETRO/HORIMETRO, realizar cadastro '.
                      p_messagem2 = 'transação ZPM0093'.

                      MESSAGE i024(sd) WITH p_messagem p_messagem1 p_messagem2.
                      RAISE posting_not_allowed.
                    ENDIF.

                    IF p_check_vida_ut IS INITIAL.
                      p_messagem = 'Não existe parametro ponto medição cadastrado'.
                      p_messagem1 = 'VIDA UTIL, realizar cadastro '.
                      p_messagem2 = 'transação ZPM0093'.

                      MESSAGE i024(sd) WITH p_messagem p_messagem1 p_messagem2.
                      RAISE posting_not_allowed.
                    ENDIF.

                    "Verifica se existe erro.
                    IF p_erro IS NOT INITIAL.
                      MESSAGE i024(sd) WITH p_messagem p_messagem1 p_messagem2.
                      RAISE posting_not_allowed.
                    ENDIF.


                    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'H' ) TO rg_mptyp.
                    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'KM' ) TO rg_mptyp.


                    SORT it_zpmt0074 BY locas.
                    DELETE it_zpmt0074 WHERE mptyp NOT IN rg_mptyp.
                    IF it_zpmt0074 IS INITIAL.
                      MESSAGE i024(sd) WITH 'Não existe parametro ponto medição '
                                            'cadastrado para o tipo F e H'.
                      RAISE posting_not_allowed.
                    ENDIF.

                    LOOP AT it_zpmt0074 ASSIGNING FIELD-SYMBOL(<ls_zpmt0074>).
                      CLEAR: p_erro, p_messagem, p_messagem1.

                      "Verificar se esta cadastrado os parametro ponto de medição.
                      SELECT SINGLE * FROM zpmt0075 INTO @DATA(ls_zpmt0075)
                      WHERE locas EQ @<ls_zpmt0074>-locas AND zeieh EQ @ztipo_contador AND typbz EQ @<ls_zpmt0074>-typbz.

                        IF sy-subrc NE 0.
                          p_messagem = 'Não existe cadastro de parametro '.
                          p_messagem1 = |plano de manutenção para o conjunto { <ls_zpmt0074>-locas }|.
                          p_messagem2 = |realizar cadastro ZPM0094|.
                          p_erro = abap_true.
                          EXIT.
                        ENDIF.
                        CLEAR: ls_zpmt0075.
                      ENDLOOP.

                      "Verifica se existe erro.
                      IF p_erro IS NOT INITIAL.
                        MESSAGE i024(sd) WITH p_messagem p_messagem1 p_messagem2.
                        RAISE posting_not_allowed.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

              "Inicio da USER STORY 96087 / Anderson Oenning
              "Verifica se o centro, centro de manutenção e Divisão são todos igual, caso seja diferente, informar ao usuario.
              IF data_iloa-swerk NE data_iloa-gsber.
                MESSAGE 'O centro localização esta diferente da divisão.' TYPE 'W'.
                RAISE posting_not_allowed.
              ENDIF.

              IF data_equz-iwerk IS NOT INITIAL.
                IF data_iloa-swerk NE data_equz-iwerk.
                  MESSAGE 'O centro localização esta diferente do centro de manutenção' TYPE 'W'.
                  RAISE posting_not_allowed.
                ENDIF.
              ENDIF.

              "Verifica se o centro de custo pertence ao centro e a empresa.
              SELECT SINGLE * FROM csks INTO @DATA(ws_csks) WHERE kostl EQ @data_iloa-kostl.
                IF ws_csks-bukrs NE data_iloa-bukrs.
                  MESSAGE 'O centro de custo não pertence a empresa->.' &&  data_iloa-bukrs TYPE 'W'.
                  RAISE posting_not_allowed.
                ELSE.
                  IF ws_csks-gsber NE data_iloa-swerk.
                    MESSAGE 'O centro de custo não pertence ao centro->.' &&  data_iloa-swerk TYPE 'W'.
                    RAISE posting_not_allowed.
                  ENDIF.
                ENDIF.
                "Fim da USER STORY 96087 / Anderson Oenning
**<<<------"149356 - NMS - INI------>>>
                IF  data_equi-eqtyp          EQ '1'    AND "1-Agro Próprio
                    <fs_fleet>-zzimobilizado IS INITIAL.
                  MESSAGE i024(sd) WITH 'Preencher Campo Imobilizado Agro para Equp. Tp = 1' DISPLAY LIKE 'W'.
                  RAISE posting_not_allowed.

                ENDIF.
**<<<------"149356 - NMS - FIM------>>>
                SELECT SINGLE *
                  FROM zequi_emprestimo
                  INTO wl_zequi_emprestimo
                 WHERE equnr = data_equz-equnr.

                  CHECK sy-subrc IS INITIAL.

                  SELECT SINGLE *
                    FROM mpos
                    INTO wl_mpos
                   WHERE iwerk = data_equz-iwerk
                     AND gewrk <> ''.

                    IF wl_zequi_emprestimo-iwerk NE data_equz-iwerk OR
                       wl_mpos-gewrk             NE data_equz-gewrk.

                      MESSAGE 'Não é possível alterar as responsabilidades do eqpto, pois o mesmo está emprestado.' TYPE 'W'.
                      RAISE posting_not_allowed.
                    ENDIF.

                  ENDIF.
                  "Verifica se o enderenço esta preenchido.


                ENDIF.
