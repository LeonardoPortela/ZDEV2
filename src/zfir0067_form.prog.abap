*----------------------------------------------------------------------*
***INCLUDE ZFIR0067_FORM.
*----------------------------------------------------------------------*

FORM f_seleciona_dados.

  DATA: vl_interval_days   TYPE i,
        vl_max_versao      TYPE zfit0079-versao,
        vl_max_dt_versao   TYPE zfit0079-dt_base_versao,
        vl_max_hora_versao TYPE zfit0079-hora_versao.

  CLEAR: vg_no_valid.

  vl_interval_days = 0.

  PERFORM limpa_dados.

  IF ( s_zfbdt-low  IS NOT INITIAL ) AND
     ( s_zfbdt-high IS NOT INITIAL ).
    vl_interval_days = s_zfbdt-high - s_zfbdt-low.
  ENDIF.

  IF vl_interval_days > 31.
    MESSAGE 'Intervalo de dias não pode ser superior a 31 dias!' TYPE 'S'.
    vg_no_valid = 'X'.
    EXIT.
  ENDIF.

  SELECT a~bukrs a~butxt
    INTO TABLE tg_t001
    FROM t001 AS a
   WHERE a~bukrs IN s_bukrs
     AND EXISTS ( SELECT *
                    FROM zfit0111 AS b
                   WHERE b~bukrs = a~bukrs ).

  "Se não informou versão nos parâmetros, Busca ultima Versão do Fluxo p/ cada Empresa.
  IF ( wa_dt_versao IS INITIAL ) OR
     ( wa_versao    IS INITIAL ).

    LOOP AT tg_t001.

      PERFORM get_last_versao USING tg_t001-bukrs ''
                           CHANGING vl_max_dt_versao
                                    vl_max_hora_versao
                                    vl_max_versao.

      IF vl_max_versao IS INITIAL.
        CONTINUE.
      ENDIF.

      tg_last_versao-bukrs           = tg_t001-bukrs.
      tg_last_versao-dt_base_versao  = vl_max_dt_versao.
      tg_last_versao-versao          = vl_max_versao.
      APPEND tg_last_versao.

    ENDLOOP.

    IF tg_last_versao[] IS INITIAL.
      MESSAGE 'Nenhuma versão encontrada!' TYPE 'S'.
      vg_no_valid = 'X'.
      EXIT.
    ENDIF.

  ENDIF.

  "Se não informou versão nos parâmetros...Busca ultima Versão p/ cada Empresa.
  IF ( wa_dt_versao IS INITIAL ) OR
     ( wa_versao    IS INITIAL ).

    "Buscar Registros de Resumo de Fluxo com Calculo de Saldo
    SELECT *
      FROM zfit0111
      INTO TABLE tg_0111
      FOR ALL ENTRIES IN tg_last_versao
     WHERE bukrs          IN s_bukrs
       AND dt_vcto        IN s_zfbdt
       AND bukrs          EQ tg_last_versao-bukrs
       AND dt_base_versao EQ tg_last_versao-dt_base_versao
       AND versao         EQ tg_last_versao-versao
       AND cx_internacional EQ p_inter.

    "Buscar Registros de Resumo de Fluxo sem Calculo de Saldo
    SELECT *
      INTO TABLE tg_0118
      FROM zfit0118
      FOR ALL ENTRIES IN tg_last_versao
     WHERE bukrs          IN s_bukrs
       AND dt_vcto        IN s_zfbdt
       AND bukrs          EQ tg_last_versao-bukrs
       AND dt_base_versao EQ tg_last_versao-dt_base_versao
       AND versao         EQ tg_last_versao-versao
       AND cx_internacional EQ  p_inter.

  ELSE.

    "Buscar Registros de Resumo de Fluxo com Calculo de Saldo
    SELECT *
      FROM zfit0111
      INTO TABLE tg_0111
     WHERE bukrs          IN s_bukrs
       AND dt_vcto        IN s_zfbdt
       AND dt_base_versao EQ wa_dt_versao
       AND versao         EQ wa_versao
       AND cx_internacional EQ p_inter.

    "Buscar Registros de Resumo de Fluxo sem Calculo de Saldo
    SELECT *
      INTO TABLE tg_0118
      FROM zfit0118 AS a
     WHERE bukrs          IN s_bukrs
       AND dt_vcto        IN s_zfbdt
       AND dt_base_versao EQ wa_dt_versao
       AND versao         EQ wa_versao.

  ENDIF.

  IF tg_0111[] IS NOT INITIAL.

    SELECT *
      FROM zfit0109
      INTO TABLE tg_0109
      FOR ALL ENTRIES IN tg_0111
     WHERE codigo    = tg_0111-codigo.

    "Busca Todos Parâmetros da Tesouraria independente se houve Movimentação.
    SELECT *
      APPENDING TABLE tg_0109
      FROM zfit0109
     WHERE tp_prev =  'T'.

    SELECT *
      APPENDING TABLE tg_0109
      FROM zfit0109
      FOR ALL ENTRIES IN tg_0118
     WHERE codigo = tg_0118-codigo.

    IF tg_0109[] IS NOT INITIAL.
      SORT tg_0109 BY codigo.
      DELETE ADJACENT DUPLICATES FROM tg_0109 COMPARING codigo.
    ENDIF.

    tg_0111_group[] = tg_0111[].
    SORT tg_0111_group BY bukrs codigo.
    DELETE ADJACENT DUPLICATES FROM tg_0111_group COMPARING bukrs codigo.

    tg_0118_group[] = tg_0118[].
    SORT tg_0118_group BY bukrs codigo.
    DELETE ADJACENT DUPLICATES FROM tg_0118_group COMPARING bukrs codigo.

    "Atribui Versão de Processamento das Empresas.
    REFRESH: it_saida_vproc.
    CLEAR: wa_saida_vproc.
    LOOP AT tg_0111_group.
      MOVE-CORRESPONDING tg_0111_group TO wa_saida_vproc.
      APPEND wa_saida_vproc TO it_saida_vproc.
    ENDLOOP.

    SORT it_saida_vproc BY bukrs.
    DELETE ADJACENT DUPLICATES FROM it_saida_vproc COMPARING bukrs.

  ELSE.
    MESSAGE 'Não encontrado registros para a consulta!' TYPE 'S'.
    vg_no_valid = 'X'.
    EXIT.
  ENDIF.


ENDFORM.                    "F_SELECIONA_DADOS

FORM f_processar_dados.

  DATA: vl_vlr_dia  TYPE zfit0111-dmbtr.

  LOOP AT tg_0111_group.

    CLEAR: wa_saida, wa_mov_ajuste, tg_0109.

    READ TABLE tg_0109 WITH KEY codigo = tg_0111_group-codigo.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    wa_saida-bukrs         = tg_0111_group-bukrs.
    wa_saida-codigo        = tg_0111_group-codigo.
    wa_saida-clas_flx      = tg_0111_group-clas_flx.
    wa_saida-dt_versao     = tg_0111_group-dt_base_versao.
    wa_saida-versao        = tg_0111_group-versao.
    wa_saida-hora_versao   = tg_0111_group-hora_versao.
    wa_saida-descricao     = tg_0109-descricao.
    wa_saida-tp_prev       = tg_0109-tp_prev.
    wa_saida-st_calc_sdo   = tg_0109-st_calc_sdo.
    wa_saida-processo_esp  = tg_0109-processo_esp.
    wa_saida-sistema_orig  = tg_0109-sistema_orig.

    MOVE-CORRESPONDING wa_saida TO wa_mov_ajuste.

    LOOP AT tg_0111 WHERE bukrs  = wa_saida-bukrs
                      AND codigo = wa_saida-codigo.

      IF vg_moeda_int IS NOT INITIAL.
        vl_vlr_dia = tg_0111-dmbtr.
      ELSE.
        vl_vlr_dia = tg_0111-dmbe2.
      ENDIF.

      IF ( ( tg_0111-tp_prev = 'T' ) AND ( tg_0111-clas_flx = 'S' ) ). " Tesouraria
        vl_vlr_dia = abs( vl_vlr_dia ) * -1.
      ENDIF.

*---> 26/05/2023 - Migração S4 - JS
      DATA: vlr_vlr_dia TYPE zfit0079-dmbtr.

      vlr_vlr_dia = CONV #( vl_vlr_dia ).
*<--- 26/05/2023 - Migração S4 - JS
      PERFORM atrib_vlr_dia USING tg_0111-dt_vcto
*---> 26/05/2023 - Migração S4 - JS
                                  "vl_vlr_dia
                                  vlr_vlr_dia
*<--- 26/05/2023 - Migração S4 - JS
                                  wa_saida.

      IF tg_0111_group-tp_prev = 'T'. " Tesouraria

*---> 26/05/2023 - Migração S4 - JS
        vlr_vlr_dia = CONV #( vl_vlr_dia ).
*<--- 26/05/2023 - Migração S4 - JS
        PERFORM atrib_vlr_dia_ajuste USING tg_0111-dt_vcto
*---> 26/05/2023 - Migração S4 - JS
                                  "vl_vlr_dia
                                  vlr_vlr_dia
*<--- 26/05/2023 - Migração S4 - JS
                                           wa_mov_ajuste.
      ENDIF.

    ENDLOOP.

    APPEND wa_saida TO it_saida.

    IF tg_0111_group-tp_prev = 'T'. " Tesouraria
      APPEND wa_mov_ajuste TO it_mov_ajuste.
    ENDIF.

  ENDLOOP.

  "Saida Fluxo sem Calculo de Saldo
  LOOP AT tg_0118_group.

    CLEAR: wa_scalc_saldo, tg_0109.

    READ TABLE tg_0109 WITH KEY codigo = tg_0118_group-codigo.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    wa_scalc_saldo-bukrs         = tg_0118_group-bukrs.
    wa_scalc_saldo-codigo        = tg_0118_group-codigo.
    wa_scalc_saldo-clas_flx      = tg_0118_group-clas_flx.
    wa_scalc_saldo-dt_versao     = tg_0118_group-dt_base_versao.
    wa_scalc_saldo-versao        = tg_0118_group-versao.
    wa_scalc_saldo-hora_versao   = tg_0118_group-hora_versao.
    wa_scalc_saldo-descricao     = tg_0109-descricao.
    wa_scalc_saldo-tp_prev       = tg_0109-tp_prev.
    wa_scalc_saldo-st_calc_sdo   = tg_0109-st_calc_sdo.
    wa_scalc_saldo-processo_esp  = tg_0109-processo_esp.
    wa_scalc_saldo-sistema_orig  = tg_0109-sistema_orig.

    IF ( tg_0109-bloq_pgto IS NOT INITIAL ).
      wa_scalc_saldo-pgto_bloq = 'X'.
    ENDIF.

    LOOP AT tg_0118 WHERE bukrs  = wa_scalc_saldo-bukrs
                      AND codigo = wa_scalc_saldo-codigo.

      IF vg_moeda_int IS NOT INITIAL.
        vl_vlr_dia = tg_0118-dmbtr.
      ELSE.
        vl_vlr_dia = tg_0118-dmbe2.
      ENDIF.

      IF ( ( tg_0109-bloq_pgto IS NOT INITIAL ) AND ( tg_0118-clas_flx = 'S' ) ).
        vl_vlr_dia = abs( vl_vlr_dia ) * -1.
      ENDIF.

      IF ( tg_0109-processo_esp = 'XRT-S'). "Saldo Aplic.- Sobra Caixa.
        vl_vlr_dia = abs( vl_vlr_dia ).
      ENDIF.

*---> 26/05/2023 - Migração S4 - JS
      vlr_vlr_dia = CONV #( vl_vlr_dia ).
*<--- 26/05/2023 - Migração S4 - JS
      PERFORM atrib_vlr_dia USING tg_0118-dt_vcto
*---> 26/05/2023 - Migração S4 - JS
                                  "vl_vlr_dia
                                  vlr_vlr_dia
*<--- 26/05/2023 - Migração S4 - JS
                                  wa_scalc_saldo.

    ENDLOOP.

    APPEND wa_scalc_saldo TO it_scalc_saldo.

  ENDLOOP.

ENDFORM.                    "F_PROCESSAR_DADOS


*=============================================================================*
*Form F_GET_DIAS_MOV                                                       *
*=============================================================================*
FORM f_get_dias_mov USING p_mov_avulso.

  DATA: tg_0111_vcto  TYPE TABLE OF zfit0111 WITH HEADER LINE.

  DATA: vl_dt_vcto_aux TYPE zfit0111-dt_vcto.

  IF p_mov_avulso IS NOT INITIAL.

    IF wa_saida_mov_flx-day_01 = 0.
      CLEAR: day_01_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_02 = 0.
      CLEAR: day_02_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_03 = 0.
      CLEAR: day_03_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_04 = 0.
      CLEAR: day_04_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_05 = 0.
      CLEAR: day_05_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_06 = 0.
      CLEAR: day_06_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_07 = 0.
      CLEAR: day_07_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_08 = 0.
      CLEAR: day_08_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_09 = 0.
      CLEAR: day_09_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_10 = 0.
      CLEAR: day_10_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_11 = 0.
      CLEAR: day_11_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_12 = 0.
      CLEAR: day_12_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_13 = 0.
      CLEAR: day_13_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_14 = 0.
      CLEAR: day_14_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_15 = 0.
      CLEAR: day_15_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_16 = 0.
      CLEAR: day_16_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_17 = 0.
      CLEAR: day_17_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_18 = 0.
      CLEAR: day_18_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_19 = 0.
      CLEAR: day_19_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_20 = 0.
      CLEAR: day_20_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_21 = 0.
      CLEAR: day_21_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_22 = 0.
      CLEAR: day_22_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_23 = 0.
      CLEAR: day_23_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_24 = 0.
      CLEAR: day_24_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_25 = 0.
      CLEAR: day_25_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_26 = 0.
      CLEAR: day_26_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_27 = 0.
      CLEAR: day_27_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_28 = 0.
      CLEAR: day_28_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_29 = 0.
      CLEAR: day_29_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_30 = 0.
      CLEAR: day_30_mov.
    ENDIF.

    IF wa_saida_mov_flx-day_31 = 0.
      CLEAR: day_31_mov.
    ENDIF.

  ELSE.

    PERFORM limpa_dias_mov.

    tg_0111_vcto[] = tg_0111[].

    SORT tg_0111_vcto BY dt_vcto.

    DELETE ADJACENT DUPLICATES FROM tg_0111_vcto COMPARING dt_vcto.

    LOOP AT tg_0111_vcto.
      PERFORM add_dia_mov USING tg_0111_vcto-dt_vcto.
    ENDLOOP.

    vl_dt_vcto_aux = s_zfbdt-low.

    IF ( s_zfbdt-high IS NOT INITIAL ) AND ( s_zfbdt-low NE s_zfbdt-high  ).

      WHILE vl_dt_vcto_aux <> s_zfbdt-high.
        PERFORM add_dia_mov_ajuste USING vl_dt_vcto_aux.
        ADD 1 TO vl_dt_vcto_aux.
      ENDWHILE.

      PERFORM add_dia_mov_ajuste USING s_zfbdt-high.

    ELSE.
      PERFORM add_dia_mov_ajuste USING vl_dt_vcto_aux.
    ENDIF.

  ENDIF.

ENDFORM.                    "F_GET_DIAS_MOV

FORM add_dia_mov USING p_dt_vcto TYPE zfit0111-dt_vcto.

  CHECK p_dt_vcto IS NOT INITIAL.

  IF day_01_mov IS INITIAL.

    day_01_mov = p_dt_vcto.

  ELSEIF day_02_mov IS INITIAL.

    day_02_mov = p_dt_vcto.

  ELSEIF day_03_mov IS INITIAL.

    day_03_mov = p_dt_vcto.

  ELSEIF day_04_mov IS INITIAL.

    day_04_mov = p_dt_vcto.

  ELSEIF day_05_mov IS INITIAL.

    day_05_mov = p_dt_vcto.

  ELSEIF day_06_mov IS INITIAL.

    day_06_mov = p_dt_vcto.

  ELSEIF day_07_mov IS INITIAL.

    day_07_mov = p_dt_vcto.

  ELSEIF day_08_mov IS INITIAL.

    day_08_mov = p_dt_vcto.

  ELSEIF day_09_mov IS INITIAL.

    day_09_mov = p_dt_vcto.

  ELSEIF day_10_mov IS INITIAL.

    day_10_mov = p_dt_vcto.

  ELSEIF day_11_mov IS INITIAL.

    day_11_mov = p_dt_vcto.

  ELSEIF day_12_mov IS INITIAL.

    day_12_mov = p_dt_vcto.

  ELSEIF day_13_mov IS INITIAL.

    day_13_mov = p_dt_vcto.

  ELSEIF day_14_mov IS INITIAL.

    day_14_mov = p_dt_vcto.

  ELSEIF day_15_mov IS INITIAL.

    day_15_mov = p_dt_vcto.

  ELSEIF day_16_mov IS INITIAL.

    day_16_mov = p_dt_vcto.

  ELSEIF day_17_mov IS INITIAL.

    day_17_mov = p_dt_vcto.

  ELSEIF day_18_mov IS INITIAL.

    day_18_mov = p_dt_vcto.

  ELSEIF day_19_mov IS INITIAL.

    day_19_mov = p_dt_vcto.

  ELSEIF day_20_mov IS INITIAL.

    day_20_mov = p_dt_vcto.

  ELSEIF day_21_mov IS INITIAL.

    day_21_mov = p_dt_vcto.

  ELSEIF day_22_mov IS INITIAL.

    day_22_mov = p_dt_vcto.

  ELSEIF day_23_mov IS INITIAL.

    day_23_mov = p_dt_vcto.

  ELSEIF day_24_mov IS INITIAL.

    day_24_mov = p_dt_vcto.

  ELSEIF day_25_mov IS INITIAL.

    day_25_mov = p_dt_vcto.

  ELSEIF day_26_mov IS INITIAL.

    day_26_mov = p_dt_vcto.

  ELSEIF day_27_mov IS INITIAL.

    day_27_mov = p_dt_vcto.

  ELSEIF day_28_mov IS INITIAL.

    day_28_mov = p_dt_vcto.

  ELSEIF day_29_mov IS INITIAL.

    day_29_mov = p_dt_vcto.

  ELSEIF day_30_mov IS INITIAL.

    day_30_mov = p_dt_vcto.

  ELSEIF day_31_mov IS INITIAL.

    day_31_mov = p_dt_vcto.

  ENDIF.

ENDFORM.


FORM add_dia_mov_ajuste USING p_dt_vcto TYPE zfit0111-dt_vcto.

  CHECK p_dt_vcto IS NOT INITIAL.

  IF p_dt_vcto < sy-datum. "Elimina Dias Retroativos
    EXIT.
  ENDIF.

  CLEAR: tg_days_mov_ajuste.

  IF day_01_ajuste IS INITIAL.

    day_01_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_01'.

  ELSEIF day_02_ajuste IS INITIAL.

    day_02_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_02'.

  ELSEIF day_03_ajuste IS INITIAL.

    day_03_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_03'.

  ELSEIF day_04_ajuste IS INITIAL.

    day_04_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_04'.

  ELSEIF day_05_ajuste IS INITIAL.

    day_05_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_05'.

  ELSEIF day_06_ajuste IS INITIAL.

    day_06_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_06'.

  ELSEIF day_07_ajuste IS INITIAL.

    day_07_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_07'.

  ELSEIF day_08_ajuste IS INITIAL.

    day_08_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_08'.

  ELSEIF day_09_ajuste IS INITIAL.

    day_09_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_09'.

  ELSEIF day_10_ajuste IS INITIAL.

    day_10_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_10'.

  ELSEIF day_11_ajuste IS INITIAL.

    day_11_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_11'.

  ELSEIF day_12_ajuste IS INITIAL.

    day_12_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_12'.

  ELSEIF day_13_ajuste IS INITIAL.

    day_13_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_13'.

  ELSEIF day_14_ajuste IS INITIAL.

    day_14_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_14'.

  ELSEIF day_15_ajuste IS INITIAL.

    day_15_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_15'.

  ELSEIF day_16_ajuste IS INITIAL.

    day_16_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_16'.

  ELSEIF day_17_ajuste IS INITIAL.

    day_17_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_17'.

  ELSEIF day_18_ajuste IS INITIAL.

    day_18_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_18'.

  ELSEIF day_19_ajuste IS INITIAL.

    day_19_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_19'.

  ELSEIF day_20_ajuste IS INITIAL.

    day_20_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_20'.

  ELSEIF day_21_ajuste IS INITIAL.

    day_21_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_21'.

  ELSEIF day_22_ajuste IS INITIAL.

    day_22_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_22'.

  ELSEIF day_23_ajuste IS INITIAL.

    day_23_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_23'.

  ELSEIF day_24_ajuste IS INITIAL.

    day_24_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_24'.

  ELSEIF day_25_ajuste IS INITIAL.

    day_25_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_25'.

  ELSEIF day_26_ajuste IS INITIAL.

    day_26_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_26'.

  ELSEIF day_27_ajuste IS INITIAL.

    day_27_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_27'.

  ELSEIF day_28_ajuste IS INITIAL.

    day_28_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_28'.

  ELSEIF day_29_ajuste IS INITIAL.

    day_29_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_29'.

  ELSEIF day_30_ajuste IS INITIAL.

    day_30_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_30'.

  ELSEIF day_31_ajuste IS INITIAL.

    day_31_ajuste = p_dt_vcto.
    tg_days_mov_ajuste-coluna = 'DAY_31'.

  ENDIF.

  IF tg_days_mov_ajuste-coluna IS NOT INITIAL.
    tg_days_mov_ajuste-dt_vcto = p_dt_vcto.
    APPEND tg_days_mov_ajuste.
  ENDIF.

ENDFORM.

FORM create_container_alv_tree .

  DATA: url(255)        TYPE c,
        it_fieldcatalog TYPE lvc_t_fcat.

  CHECK container IS INITIAL.

* create a container for the tree control
  CREATE OBJECT container
    EXPORTING
      container_name              = 'C_TREE'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.

  CREATE OBJECT dg_dyndoc_id
    EXPORTING
      style = 'ALV_GRID'.

  CREATE OBJECT dg_splitter
    EXPORTING
      parent  = container
      rows    = 2
      columns = 1.

  CALL METHOD dg_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = dg_parent_html.

  CREATE OBJECT dg_splitter_2
    EXPORTING
      parent  = dg_parent_html
      rows    = 1
      columns = 2.

  CALL METHOD dg_splitter_2->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = dg_parent_html1.

  CALL METHOD dg_splitter_2->set_column_width
    EXPORTING
      id    = 1
      width = 40.

  CALL METHOD dg_splitter_2->get_container
    EXPORTING
      row       = 1
      column    = 2
    RECEIVING
      container = dg_parent_html2.

  CREATE OBJECT picture
    EXPORTING
      parent = dg_parent_html2.

  PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

  CALL METHOD picture->load_picture_from_url
    EXPORTING
      url = url.

  CALL METHOD picture->set_display_mode
    EXPORTING
      display_mode = picture->display_mode_fit_center.

  CALL METHOD dg_splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = dg_parent_tree.

  CALL METHOD dg_splitter->set_row_height
    EXPORTING
      id     = 1
      height = 16.

  PERFORM cria_html_cab_tree.


ENDFORM.                    " CREATE_CONTAINER_ALV_TREE

FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.
  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM

FORM cria_html_cab_tree.

  DATA: texto(40), vg_mes(2), vg_ano(4),
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        p_text                  TYPE sdydo_text_element,
        position                TYPE i,
        vl_data_aux             TYPE string,
        vl_data_aux02           TYPE string.

  DATA: column         TYPE REF TO cl_dd_area,
        column_1       TYPE REF TO cl_dd_area,
        column_2       TYPE REF TO cl_dd_area,
        table_element  TYPE REF TO cl_dd_table_element,
        table_element2 TYPE REF TO cl_dd_table_element.

  CALL METHOD dg_dyndoc_id->initialize_document.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 1
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element.

  CALL METHOD table_element->add_column
    IMPORTING
      column = column.

  CALL METHOD table_element->set_column_style
    EXPORTING
      col_no    = 1
      sap_align = 'CENTER'
      sap_style = cl_dd_document=>heading.

*  P_TEXT = 'Fluxo Caixa Previsto'.
*  CALL METHOD COLUMN->ADD_TEXT
*    EXPORTING
*      TEXT      = P_TEXT
*      SAP_STYLE = 'HEADING'.
*
*  CALL METHOD TABLE_ELEMENT->NEW_ROW.

  CONCATENATE 'Moeda' '-' vg_waers INTO p_text SEPARATED BY space.

  IF s_zfbdt-high IS NOT INITIAL.

    PERFORM formata_data USING s_zfbdt-low  vl_data_aux.
    PERFORM formata_data USING s_zfbdt-high vl_data_aux02.

    CONCATENATE p_text '/' 'Período:' vl_data_aux 'até' vl_data_aux02
           INTO p_text SEPARATED BY space.
  ELSE.

    PERFORM formata_data USING s_zfbdt-low vl_data_aux.

    CONCATENATE p_text '/' 'Dia:' vl_data_aux
           INTO p_text SEPARATED BY space.

  ENDIF.


  CALL METHOD column->add_text
    EXPORTING
      text         = p_text
      "SAP_STYLE = 'HEADING'.
      sap_fontsize = 'MEDIUM'.


  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element2.

  CALL METHOD table_element2->add_column
    EXPORTING
      sap_style   = 'SAP_BOLD'
      style_class = 'SAP_BOLD'
    IMPORTING
      column      = column_1.

  CALL METHOD table_element2->add_column
    IMPORTING
      column = column_2.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 2
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.


  CALL METHOD column_1->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  CLEAR: p_text_table.


  CALL METHOD column_2->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  PERFORM container_html.


ENDFORM.                    " CRIA_HTML_CAB_DRE

FORM container_html .

  DATA : dl_length        TYPE i,
         dl_background_id TYPE sdydo_key VALUE space.

  IF dg_html_cntrl IS INITIAL.
    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html1.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = dg_dyndoc_id
      bottom   = space
    IMPORTING
      length   = dl_length.

  CALL METHOD dg_dyndoc_id->merge_document.

  CALL METHOD dg_dyndoc_id->set_document_background
    EXPORTING
      picture_id = dl_background_id.

  dg_dyndoc_id->html_control = dg_html_cntrl.

  CALL METHOD dg_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_html1
    EXCEPTIONS
      html_display_error = 1.


ENDFORM.                    " CONTAINER_HTML

FORM criar_field_catalog_tree.

  FREE: wa_fcat, it_fcat.
  REFRESH: it_fcat.

  PERFORM estrutura_alv USING:

      03  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_01'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      04  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_02'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      05  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_03'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      06  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_04'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      07  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_05'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      08  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_06'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      09  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_07'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      10  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_08'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      11  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_09'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      12  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_10'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      13  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_11'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      14  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_12'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      15  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_13'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      16  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_14'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      17  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_15'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      18  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_16'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      19  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_17'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      20  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_18'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      21  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_19'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      22  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_20'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      23  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_21'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      24  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_22'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      25  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_23'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      26  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_24'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      27  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_25'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      28  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_26'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      29  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_27'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      30  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_28'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      31  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_29'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      32  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_30'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X',
      33  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_31'           'Dia 1'            '20'  ' '    '' ' ' 'R' ' ' 'X'.

ENDFORM.                    " CRIAR_FIELD_CATALOG

FORM criar_field_catalog_0101.

  FREE: wa_fcat, it_fcat.
  REFRESH: it_fcat.

  PERFORM estrutura_alv USING:

      03  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_01'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      04  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_02'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      05  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_03'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      06  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_04'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      07  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_05'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      08  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_06'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      09  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_07'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      10  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_08'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      11  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_09'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      12  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_10'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      13  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_11'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      14  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_12'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      15  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_13'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      16  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_14'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      17  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_15'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      18  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_16'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      19  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_17'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      20  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_18'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      21  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_19'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      22  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_20'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      23  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_21'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      24  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_22'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      25  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_23'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      26  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_24'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      27  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_25'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      28  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_26'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      29  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_27'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      30  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_28'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      31  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_29'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      32  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_30'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X',
      33  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_31'           'Dia 1'            '20'  ' '    '' ' ' 'R' 'X' 'X'.

ENDFORM.                    " CRIAR_FIELD_CATALOG

FORM criar_field_catalog_0103 USING p_var.

  FREE: wa_fcat, it_fcat.

  PERFORM estrutura_alv USING:

      01  ''  ''   'TG_SAIDA_VAR1' 'BUKRS'          'Empresa'                         '07'   ' '    '' ' ' ' ' ' ' ' ' ,
      02  ''  ''   'TG_SAIDA_VAR1' 'LIFNR'          'Fornecedor'                      '09'   ' '    '' ' ' ' ' ' ' ' ' ,
      03  ''  ''   'TG_SAIDA_VAR1' 'KUNNR'          'Cliente'                         '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      04  ''  ''   'TG_SAIDA_VAR1' 'NAME1'          'Nome Forn./Cliente'              '35'   ' '    '' ' ' ' ' ' ' ' ' ,
      05  ''  ''   'TG_SAIDA_VAR1' 'CODIGO'         'Cód.Flx.'                        '08'   ' '    '' ' ' ' ' ' ' ' ' ,
      06  ''  ''   'TG_SAIDA_VAR1' 'DESC_FLX'       'Descrição'                       '14'   ' '    '' ' ' ' ' ' ' ' ' ,
      07  ''  ''   'TG_SAIDA_VAR1' 'CLAS_FLX'       'Clas.Flx.'                       '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      08  ''  ''   'TG_SAIDA_VAR1' 'TP_PREV'        'Tp.Prev.'                        '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      09  ''  ''   'TG_SAIDA_VAR1' 'BLART'          'Tp.Docto'                        '04'   ' '    '' ' ' ' ' ' ' ' ' ,
      10  ''  ''   'TG_SAIDA_VAR1' 'BSART'          'Tp.Ped.'                         '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      11  ''  ''   'TG_SAIDA_VAR1' 'AUART'          'Tp.OV.'                          '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      12  ''  ''   'TG_SAIDA_VAR1' 'ZLSPR'          'Bloq.Pgto'                       '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      13  ''  ''   'TG_SAIDA_VAR1' 'ZLSCH'          'Forma.Pgto'                      '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      14  ''  ''   'TG_SAIDA_VAR1' 'HBKID'          'Bco.Empresa'                     '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      15  ''  ''   'TG_SAIDA_VAR1' 'SGTXT'          'Ref.'                            '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      15  ''  ''   'TG_SAIDA_VAR1' 'SGTXT2'         'Ref.2'                           '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      16  ''  ''   'TG_SAIDA_VAR1' 'DEP_RESP'       'Dep.Resp.'                       '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      18  ''  ''   'TG_SAIDA_VAR1' 'ZFBDT'          'Dt. Vcto'                        '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      19  ''  ''   'TG_SAIDA_VAR1' 'HKONT'          'Conta contábil'                  '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      21  ''  ''   'TG_SAIDA_VAR1' 'TXT50'          'Descrição da conta'              '50'   ' '    '' ' ' ' ' ' ' ' ' ,
      22  ''  ''   'TG_SAIDA_VAR1' 'PLANILHA'       'Planilha'                        '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      22  ''  ''   'TG_SAIDA_VAR1' 'PLANILHA_ITM'   'Plan.Item'                       '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      22  ''  ''   'TG_SAIDA_VAR1' 'ID_INVOICE'     'Id.Invoice'                      '25'   ' '    '' ' ' ' ' ' ' ' ' ,
      22  ''  ''   'TG_SAIDA_VAR1' 'DS_PORTO'       'Porto'                           '30'   ' '    '' ' ' ' ' ' ' ' ' ,
      23  ''  ''   'TG_SAIDA_VAR1' 'TRADE_ID'       'Chav.Ident.'                     '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      24  ''  ''   'TG_SAIDA_VAR1' 'DOC_IMPOSTO'    'Doc. Imposto'                    '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      25  ''  ''   'TG_SAIDA_VAR1' 'SEQITEM'        'Item'                            '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      25  ''  ''   'TG_SAIDA_VAR1' 'OBJ_KEY_PREV'   'Chave.Lcto.Prev'                 '20'   ' '    '' ' ' ' ' ' ' ' ' ,
      26  ''  ''   'TG_SAIDA_VAR1' 'CD_PREV'        'Cd.Prev.Avulsa'                  '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      27  ''  ''   'TG_SAIDA_VAR1' 'BELNR'          'Doc. Contábil'                   '10'   ' '    '' ' ' ' ' 'X' ' ' ,
      28  ''  ''   'TG_SAIDA_VAR1' 'BUDAT'          'Dt. Lcto'                        '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      29  ''  ''   'TG_SAIDA_VAR1' 'BLDAT'          'Dt. Dcto'                        '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      29  ''  ''   'TG_SAIDA_VAR1' 'AUGBL'          'Doc. Comp.'                      '10'   ' '    '' ' ' ' ' 'X' ' ' ,
      29  ''  ''   'TG_SAIDA_VAR1' 'AUGDT'          'Dt. Comp.'                       '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      30  ''  ''   'TG_SAIDA_VAR1' 'WAERS'          'Moeda'                           '5'    ' '    '' ' ' ' ' ' ' ' ' ,
      31  ''  ''   'TG_SAIDA_VAR1' 'DMBTR'          'Valor R$'                        '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      32  ''  ''   'TG_SAIDA_VAR1' 'DMBE2'          'Valor US$'                       '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      33  ''  ''   'TG_SAIDA_VAR1' 'XBLNR'          'Referência'                      '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      34  ''  ''   'TG_SAIDA_VAR1' 'NRO_SOL'        'Nro.Sol.'                        '10'   ' '    '' ' ' ' ' 'X' ' ' ,
      34  ''  ''   'TG_SAIDA_VAR1' 'EBELN'          'Doc.compra'                      '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      35  ''  ''   'TG_SAIDA_VAR1' 'EBELP'          'Item'                            '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      36  ''  ''   'TG_SAIDA_VAR1' 'BSCHL'          'Chv lc'                          '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      37  ''  ''   'TG_SAIDA_VAR1' 'OPR_NUMERO'     'Nro. Operação'                   '13'   ' '    '' ' ' ' ' ' ' ' ' ,
      38  ''  ''   'TG_SAIDA_VAR1' 'CON_CODIGO'     'Contrato'                        '08'   ' '    '' ' ' ' ' ' ' ' ' ,
      39  ''  ''   'TG_SAIDA_VAR1' 'MDO_CODIGO'     'Modalidade'                      '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      40  ''  ''   'TG_SAIDA_VAR1' 'PAR_TIPO'       'Tp.Parcela'                      '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      41  ''  ''   'TG_SAIDA_VAR1' 'MDO_TIPO'       'Tp.Mdo.'                         '07'   ' '    '' ' ' ' ' ' ' ' ' ,
      41  ''  ''   'TG_SAIDA_VAR1' 'BUKRS_OPR'      'Emp.Opr.'                        '09'   ' '    '' ' ' ' ' ' ' ' ' ,
      41  ''  ''   'TG_SAIDA_VAR1' 'AGENTE'         'Agente'                          '15'   ' '    '' ' ' ' ' ' ' ' ' ,
      41  ''  ''   'TG_SAIDA_VAR1' 'REGRA_VAL'      'Regra Vlr.'                      '20'   ' '    '' ' ' ' ' ' ' ' ' ,
      42  ''  ''   'TG_SAIDA_VAR1' 'PROCESSO_ESP'   'Proc. Espec.'                    '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      43  ''  ''   'TG_SAIDA_VAR1' 'SISTEMA_ORIG'   'Sistema Origem'                  '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      44  ''  ''   'TG_SAIDA_VAR1' 'USNAM'          'Usuário'                         '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      45  ''  ''   'TG_SAIDA_VAR1' 'US_PROC'        'Usuario Proc.'                   '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      46  ''  ''   'TG_SAIDA_VAR1' 'DT_ATUAL'       'Dt atual'                        '10'   ' '    '' ' ' ' ' ' ' ' ' ,
      47  ''  ''   'TG_SAIDA_VAR1' 'HR_ATUAL'       'Hr atual'                        '10'   ' '    '' ' ' ' ' ' ' ' ' .


  PERFORM remove_colunas_null USING p_var.


ENDFORM.                    " CRIAR_FIELD_CATALOG_XML


FORM estrutura_alv USING VALUE(p_col_pos)       TYPE i
                         VALUE(p_ref_tabname)   LIKE dd02d-tabname
                         VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                         VALUE(p_tabname)       LIKE dd02d-tabname
                         VALUE(p_field)         LIKE dd03d-fieldname
                         VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                         VALUE(p_outputlen)
                         VALUE(p_edit)
                         VALUE(p_sum)
                         VALUE(p_emphasize)
                         VALUE(p_just)
                         VALUE(p_hotspot)
                         VALUE(p_com_mov).

  DATA: vl_scrtext_aux TYPE dd03p-scrtext_l,
        vl_scrtext     TYPE dd03p-scrtext_l.

  CASE p_field.
    WHEN 'DAY_01'.
      IF ( day_01_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_01_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_02'.
      IF ( day_02_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_02_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_03'.
      IF ( day_03_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_03_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_04'.
      IF ( day_04_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_04_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_05'.
      IF ( day_05_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_05_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_06'.
      IF ( day_06_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_06_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_07'.
      IF ( day_07_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_07_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_08'.
      IF ( day_08_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_08_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_09'.
      IF ( day_09_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_09_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_10'.
      IF ( day_10_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_10_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_11'.
      IF ( day_11_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_11_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_12'.
      IF ( day_12_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_12_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_13'.
      IF ( day_13_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_13_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_14'.
      IF ( day_14_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_14_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_15'.
      IF ( day_15_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_15_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_16'.
      IF ( day_16_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_16_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_17'.
      IF ( day_17_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_17_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_18'.
      IF ( day_18_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_18_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_19'.
      IF ( day_19_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_19_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_20'.
      IF ( day_20_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_20_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_21'.
      IF ( day_21_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_21_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_22'.
      IF ( day_22_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_22_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_23'.
      IF ( day_23_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_23_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_24'.
      IF ( day_24_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_24_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_25'.
      IF ( day_25_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_25_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_26'.
      IF ( day_26_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_26_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_27'.
      IF ( day_27_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_27_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_28'.
      IF ( day_28_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_28_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_29'.
      IF ( day_29_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_29_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_30'.
      IF ( day_30_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_30_mov CHANGING vl_scrtext_aux.
    WHEN 'DAY_31'.
      IF ( day_31_mov IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_31_mov CHANGING vl_scrtext_aux.
  ENDCASE.

  IF vl_scrtext_aux IS NOT INITIAL.
    "TRANSLATE VL_SCRTEXT_AUX+6 TO LOWER CASE.
    vl_scrtext = vl_scrtext_aux.
  ELSE.
    vl_scrtext = p_scrtext_l.
  ENDIF.

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-colddictxt = 'L'.
  wa_fcat-selddictxt = 'L'.
  wa_fcat-tipddictxt = 'L'.
  wa_fcat-seltext     = vl_scrtext.
  wa_fcat-reptext     = vl_scrtext.
  wa_fcat-scrtext_s   = vl_scrtext.
  wa_fcat-scrtext_m   = vl_scrtext.
  wa_fcat-scrtext_l   = vl_scrtext.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-do_sum      = p_sum.
  wa_fcat-hotspot     = p_hotspot.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM estrutura_alv_ajuste USING VALUE(p_col_pos)       TYPE i
                                VALUE(p_ref_tabname)   LIKE dd02d-tabname
                                VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                                VALUE(p_tabname)       LIKE dd02d-tabname
                                VALUE(p_field)         LIKE dd03d-fieldname
                                VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                                VALUE(p_outputlen)
                                VALUE(p_edit)
                                VALUE(p_sum)
                                VALUE(p_emphasize)
                                VALUE(p_just)
                                VALUE(p_hotspot).

  DATA: vl_scrtext_aux TYPE dd03p-scrtext_l,
        vl_scrtext     TYPE dd03p-scrtext_l.

  CASE p_field.
    WHEN 'DAY_01'.
      IF ( day_01_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_01_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_02'.
      IF ( day_02_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_02_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_03'.
      IF ( day_03_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_03_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_04'.
      IF ( day_04_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_04_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_05'.
      IF ( day_05_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_05_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_06'.
      IF ( day_06_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_06_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_07'.
      IF ( day_07_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_07_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_08'.
      IF ( day_08_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_08_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_09'.
      IF ( day_09_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_09_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_10'.
      IF ( day_10_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_10_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_11'.
      IF ( day_11_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_11_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_12'.
      IF ( day_12_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_12_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_13'.
      IF ( day_13_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_13_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_14'.
      IF ( day_14_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_14_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_15'.
      IF ( day_15_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_15_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_16'.
      IF ( day_16_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_16_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_17'.
      IF ( day_17_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_17_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_18'.
      IF ( day_18_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_18_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_19'.
      IF ( day_19_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_19_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_20'.
      IF ( day_20_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_20_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_21'.
      IF ( day_21_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_21_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_22'.
      IF ( day_22_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_22_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_23'.
      IF ( day_23_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_23_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_24'.
      IF ( day_24_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_24_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_25'.
      IF ( day_25_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_25_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_26'.
      IF ( day_26_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_26_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_27'.
      IF ( day_27_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_27_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_28'.
      IF ( day_28_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_28_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_29'.
      IF ( day_29_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_29_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_30'.
      IF ( day_30_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_30_ajuste CHANGING vl_scrtext_aux.
    WHEN 'DAY_31'.
      IF ( day_31_ajuste IS INITIAL ).
        EXIT.
      ENDIF.
      PERFORM formata_data USING day_31_ajuste CHANGING vl_scrtext_aux.
  ENDCASE.

  IF vl_scrtext_aux IS NOT INITIAL.
    vl_scrtext = vl_scrtext_aux.
  ELSE.
    vl_scrtext = p_scrtext_l.
  ENDIF.

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-reptext     = vl_scrtext.
  wa_fcat-scrtext_s   = vl_scrtext.
  wa_fcat-scrtext_m   = vl_scrtext.
  wa_fcat-scrtext_l   = vl_scrtext.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-do_sum      = p_sum.
  wa_fcat-hotspot     = p_hotspot.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV


FORM iniciar_tree .

  DATA: l_hierarchy_header TYPE treev_hhdr,
        lt_list_commentary TYPE slis_t_listheader,
        l_logo             TYPE sdydo_value,
        ls_line            TYPE slis_listheader,
        it_fieldcatalog    TYPE lvc_t_fcat,
        vg_mes(2),
        vg_ano(4),
        lt_events          TYPE cntl_simple_events,
        l_event            TYPE cntl_simple_event,
        i_default          TYPE char01,
        it_node_key        TYPE lvc_t_nkey.

  IF g_tree IS INITIAL.

    CREATE OBJECT g_tree
      EXPORTING
        parent                      = dg_parent_tree
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
        item_selection              = space
        no_html_header              = 'X'
        no_toolbar                  = ''
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.

*    CLEAR L_EVENT.
*    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.

    CALL METHOD g_tree->set_registered_events
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

*  Definição de tamanhos do cabeçalho do avl tree list
    l_hierarchy_header-t_image = 'LOGO_NOVO'.
    l_hierarchy_header-heading = 'Empresa'.
    l_hierarchy_header-width   = 50.

    "CREATE OBJECT ALV_TREE_VIEW_EVENT.
    "SET HANDLER ALV_TREE_VIEW_EVENT->ON_NODE_DOUBLE_CLICK FOR G_TREE.

    l_logo = 'LOGO_NOVO'.
    g_variant-report = sy-repid.

    PERFORM: f_seleciona_dados.

    CHECK vg_no_valid IS INITIAL.

    PERFORM: f_get_dias_mov USING '',
             criar_field_catalog_tree.

    IF it_fcat[] IS NOT INITIAL.

      CALL METHOD g_tree->set_table_for_first_display
        EXPORTING
          is_hierarchy_header = l_hierarchy_header
          it_list_commentary  = lt_list_commentary
          i_logo              = l_logo
          i_background_id     = 'ALV_BACKGROUND'
          i_save              = 'A'
          is_variant          = g_variant
        CHANGING
          it_outtab           = it_alv_tree
          it_fieldcatalog     = it_fcat.

      PERFORM: f_processar_dados,
               criar_hierarchy TABLES it_node_key.

*     Register events for alv tree
      PERFORM fm_register_event.

*    * calculate totals
      CALL METHOD g_tree->update_calculations.
*
*    * this method must be called to send the data to the frontend
      CALL METHOD g_tree->frontend_update.

      IF ( vg_no_expand_node IS INITIAL ) AND ( vg_node_key IS NOT INITIAL ).

        CALL METHOD g_tree->expand_nodes
          EXPORTING
            it_node_key = vg_node_key.

      ELSE.
        CALL METHOD g_tree->collapse_all_nodes.
        REFRESH: vg_node_key.
        CLEAR: vg_node_key, vg_no_expand_node.
      ENDIF.

    ELSE.
      MESSAGE 'Nenhum movimento Encontrado!' TYPE 'S'.
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.                    " CREATE_TREE_ALV_TREE3_TREE

*&---------------------------------------------------------------------*
*&      Form  ESTRUTURA_SAIDA_ALV_TREE_VIEW
*&---------------------------------------------------------------------*

FORM criar_hierarchy TABLES it_node_key TYPE lvc_t_nkey.

  DATA: vl_node_text   TYPE lvc_value,
        l_root_key     TYPE lvc_nkey,
        l_nvl1_key     TYPE lvc_nkey,
        wa_saldo_final TYPE ty_saida.

  DATA: it_saida_bukrs  TYPE TABLE OF ty_saida.

  it_saida_bukrs[] = it_saida[].

  SORT it_saida_bukrs BY bukrs.

  DELETE ADJACENT DUPLICATES FROM it_saida_bukrs COMPARING bukrs.

  LOOP AT it_saida_bukrs INTO wa_saida.

    READ TABLE tg_t001 WITH KEY bukrs = wa_saida-bukrs.

    CONCATENATE wa_saida-bukrs '-' tg_t001-butxt
           INTO vl_node_text SEPARATED BY space.


    PERFORM get_saldo_final USING wa_saida
                         CHANGING wa_saldo_final.

    "Adiciona Nó Empresa.
    PERFORM add_line_tree USING l_root_key
                                vl_node_text
                                'X'
                                wa_saldo_final
                                '@A8@'
                       CHANGING l_nvl1_key.

    it_node_key = l_nvl1_key.
    APPEND it_node_key.

    PERFORM: add_saldo_inicial   TABLES it_node_key
                                  USING wa_saida
                                        l_nvl1_key,

             add_entradas        TABLES it_node_key
                                  USING wa_saida
                                        l_nvl1_key,

             add_saidas          TABLES it_node_key
                                  USING wa_saida
                                        l_nvl1_key,

             add_flx_tesouraria  TABLES it_node_key
                                  USING wa_saida
                                        l_nvl1_key,

             add_saldo_final     TABLES it_node_key
                                  USING wa_saldo_final
                                        l_nvl1_key,

             add_variacao        TABLES it_node_key
                                  USING wa_saldo_final
                                        l_nvl1_key,

             add_sld_aplic       TABLES it_node_key
                                  USING wa_saida
                                        l_nvl1_key,

             add_pgto_bloq       TABLES it_node_key
                                  USING wa_saida
                                        l_nvl1_key.

  ENDLOOP.

ENDFORM.


FORM formata_data  USING    p_data
                   CHANGING p_value.

  DATA: vl_day_of_week_num TYPE c,
        vl_ds_day_week     TYPE c LENGTH 10.

  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date   = p_data
    IMPORTING
      day    = vl_day_of_week_num
    EXCEPTIONS
      OTHERS = 8.

  CASE vl_day_of_week_num.
    WHEN 1.
      vl_ds_day_week = '- Seg.'.
    WHEN 2.
      vl_ds_day_week = '- Ter.'.
    WHEN 3.
      vl_ds_day_week = '- Qua.'.
    WHEN 4.
      vl_ds_day_week = '- Qui.'.
    WHEN 5.
      vl_ds_day_week = '- Sex.'.
    WHEN 6.
      vl_ds_day_week = '- Sáb.'.
    WHEN 7.
      vl_ds_day_week = '- Dom.'.
  ENDCASE.

  CONCATENATE p_data+06(2) '.' p_data+04(2) '.' p_data+2(2) INTO p_value.
  CONCATENATE p_value vl_ds_day_week INTO p_value SEPARATED BY space.

ENDFORM.

FORM formata_hora  USING    p_hora
                   CHANGING p_value.

  CONCATENATE p_hora(2) ':' p_hora+02(2) INTO p_value.

ENDFORM.

FORM add_saldo_inicial  TABLES it_node_key
                        USING  p_saida TYPE     ty_saida
                               p_relat_key TYPE lvc_nkey.

  DATA: vl_lvc_nkey TYPE lvc_nkey.

  DATA: tg_0113  TYPE TABLE OF zfit0113 WITH HEADER LINE,
        wl_saida TYPE ty_saida.

  DATA: vl_saldo_ini TYPE zfit0113-dmbtr.

  SELECT *
    FROM zfit0113
    INTO TABLE tg_0113
   WHERE bukrs          = p_saida-bukrs
     AND dt_vcto        IN s_zfbdt
     AND dt_base_versao = p_saida-dt_versao
     AND versao         = p_saida-versao
     AND cx_internacional = p_inter.

  wl_saida-bukrs = p_saida-bukrs.

  LOOP AT tg_0113.

    IF vg_moeda_int IS NOT INITIAL.
      vl_saldo_ini = tg_0113-sdo_inicial_r.
    ELSE.
      vl_saldo_ini = tg_0113-sdo_inicial_us.
    ENDIF.

    PERFORM atrib_vlr_dia USING tg_0113-dt_vcto
                                vl_saldo_ini
                                wl_saida.

  ENDLOOP.

  PERFORM add_line_tree USING p_relat_key
                              'SALDO INICIAL'
                              ''
                              wl_saida
                              '@3Z@'
                     CHANGING vl_lvc_nkey.


ENDFORM.

FORM add_entradas  TABLES it_node_key
                   USING  p_saida       TYPE ty_saida
                          p_relat_key TYPE lvc_nkey.

  DATA: wa_tot_entradas TYPE ty_saida,
        l_nvl2_key      TYPE lvc_nkey.

  REFRESH: r_tp_prev.

  r_tp_prev-sign   = 'I'.
  r_tp_prev-option = 'EQ'.
  r_tp_prev-low    = 'A'.
  r_tp_prev-high   = 'A'.
  APPEND r_tp_prev.

  r_tp_prev-low    = 'M'.
  r_tp_prev-high   = 'M'.
  APPEND r_tp_prev.

  PERFORM get_total_entradas TABLES r_tp_prev
                              USING p_saida
                           CHANGING wa_tot_entradas.

  PERFORM add_line_tree USING p_relat_key
                              'Entradas'
                              'X'
                              wa_tot_entradas
                              '@93@'
                     CHANGING l_nvl2_key.

  it_node_key = l_nvl2_key.
  APPEND it_node_key.

  PERFORM add_entradas_lcto TABLES r_tp_prev
                             USING p_saida
                                   l_nvl2_key.


ENDFORM.


FORM add_saidas  TABLES it_node_key
                 USING  p_saida      TYPE ty_saida
                        p_relat_key  TYPE lvc_nkey.

  DATA: wa_tot_saidas TYPE ty_saida,
        l_nvl2_key    TYPE lvc_nkey.

  REFRESH: r_tp_prev.

  r_tp_prev-sign   = 'I'.
  r_tp_prev-option = 'EQ'.
  r_tp_prev-low    = 'A'.
  r_tp_prev-high   = 'A'.
  APPEND r_tp_prev.

  r_tp_prev-low    = 'M'.
  r_tp_prev-high   = 'M'.
  APPEND r_tp_prev.

  PERFORM get_total_saidas TABLES r_tp_prev
                            USING p_saida
                         CHANGING wa_tot_saidas.

  PERFORM add_line_tree USING p_relat_key
                              'Saídas'
                              'X'
                              wa_tot_saidas
                              '@94@'
                     CHANGING l_nvl2_key.

  it_node_key = l_nvl2_key.
  APPEND it_node_key.

  PERFORM add_saidas_lcto TABLES r_tp_prev
                           USING p_saida
                                 l_nvl2_key.

ENDFORM.


FORM add_flx_tesouraria  TABLES it_node_key
                         USING  p_saida      TYPE ty_saida
                                p_relat_key  TYPE lvc_nkey.

  DATA: vl_lvc_nkey    TYPE lvc_nkey,
        wa_tot_ajustes TYPE ty_saida.

  PERFORM get_total_ajustes   USING p_saida
                           CHANGING wa_tot_ajustes.

  PERFORM add_line_tree USING p_relat_key
                              'Ajuste(Tesouraria)'
                              'X'
                              wa_tot_ajustes
                              '@0M@'
                     CHANGING vl_lvc_nkey.

  it_node_key = vl_lvc_nkey.
  APPEND it_node_key.

  PERFORM add_lctos_ajuste USING p_saida
                                 vl_lvc_nkey.

ENDFORM.

FORM add_lctos_ajuste USING  p_saida     TYPE ty_saida
                             p_relat_key TYPE lvc_nkey.

  DATA: vl_lvc_nkey     TYPE lvc_nkey,
        vl_node_text    TYPE lvc_value,
        wl_saida_aux    TYPE ty_saida,
        wl_saida_ajuste TYPE ty_saida.

*-CS2022000133-#74201-05.05.2022-JT-inicio
  SORT tg_0109 BY tp_prev codigo.
*-CS2022000133-#74201-05.05.2022-JT-fim

  LOOP AT tg_0109 WHERE tp_prev = 'T'.

    CLEAR: wl_saida_aux, wl_saida_ajuste.

    READ TABLE it_saida INTO wl_saida_aux WITH KEY bukrs      = p_saida-bukrs
                                                   codigo     = tg_0109-codigo.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING wl_saida_aux TO wl_saida_ajuste.
    ENDIF.

    MOVE: 'X'                       TO wl_saida_ajuste-nivel,
          p_saida-bukrs             TO wl_saida_ajuste-bukrs,
          tg_0109-codigo            TO wl_saida_ajuste-codigo,
          tg_0109-descricao         TO wl_saida_ajuste-descricao,
          tg_0109-tp_prev           TO wl_saida_ajuste-tp_prev,
          p_saida-dt_versao         TO wl_saida_ajuste-dt_versao,
          p_saida-versao            TO wl_saida_ajuste-versao,
          p_saida-hora_versao       TO wl_saida_ajuste-hora_versao,
          p_saida-dt_versao_var     TO wl_saida_ajuste-dt_versao_var,
          p_saida-versao_var        TO wl_saida_ajuste-versao_var,
          p_saida-hora_versao_var   TO wl_saida_ajuste-hora_versao_var.

    CLEAR: wl_saida_ajuste-processo_esp ,
           wl_saida_ajuste-sistema_orig ,
           wl_saida_ajuste-seq          ,
           wl_saida_ajuste-dt_vcto      ,
           wl_saida_ajuste-varicacao    ,
           wl_saida_ajuste-txt_vrs1     ,
           wl_saida_ajuste-txt_vrs2     .

    CONCATENATE wl_saida_ajuste-codigo '-' wl_saida_ajuste-descricao
           INTO vl_node_text SEPARATED BY space.

    PERFORM add_line_tree USING p_relat_key
                                vl_node_text
                                ''
                                wl_saida_ajuste
                                ''
                       CHANGING vl_lvc_nkey.


  ENDLOOP.


ENDFORM.

FORM add_entradas_lcto TABLES p_tp_prev
                       USING  p_saida     TYPE ty_saida
                              p_relat_key TYPE lvc_nkey.

  DATA: vl_lvc_nkey  TYPE lvc_nkey,
        vl_node_text TYPE lvc_value,
        wl_saida_aux TYPE ty_saida.

  LOOP AT it_saida INTO wl_saida_aux WHERE bukrs        = p_saida-bukrs
                                       AND clas_flx     =  'E'
                                       AND tp_prev      IN p_tp_prev
                                       AND dt_versao    = p_saida-dt_versao
                                       AND versao       = p_saida-versao.

    wl_saida_aux-nivel = 'X'.

    CONCATENATE wl_saida_aux-codigo '-' wl_saida_aux-descricao
           INTO vl_node_text SEPARATED BY space.

    PERFORM add_line_tree USING p_relat_key
                                vl_node_text
                                ''
                                wl_saida_aux
                                ''
                       CHANGING vl_lvc_nkey.

  ENDLOOP.


ENDFORM.


FORM get_total_entradas  TABLES p_tp_prev
                          USING p_saida     TYPE ty_saida
                       CHANGING p_saida_ret TYPE ty_saida.

  DATA: BEGIN OF tg_0111_resumo OCCURS 0,
          bukrs   TYPE zfit0111-bukrs,
          dt_vcto TYPE zfit0111-dt_vcto,
          dmbtr   TYPE zfit0111-dmbtr,
          dmbe2   TYPE zfit0111-dmbe2,
        END OF tg_0111_resumo.

  DATA: l_node_text       TYPE lvc_value,
        lt_item_layout    TYPE lvc_t_layi,
        ls_item_layout    TYPE lvc_s_layi,
        ls_node           TYPE lvc_s_layn,
        vl_node_key_teste TYPE lvc_nkey.

  DATA: tg_0111_ent TYPE TABLE OF zfit0111 WITH HEADER LINE,
        wl_saida    TYPE ty_saida.

  DATA: vl_tot_entradas TYPE zfit0113-dmbtr.

  SELECT bukrs dt_vcto SUM( dmbtr ) SUM( dmbe2 )
    FROM zfit0111
    INTO TABLE tg_0111_resumo
   WHERE bukrs    EQ p_saida-bukrs
     AND dt_vcto  IN s_zfbdt
     AND clas_flx EQ 'E'
     AND tp_prev  IN p_tp_prev
     AND dt_base_versao = p_saida-dt_versao
     AND versao         = p_saida-versao
     AND cx_internacional = p_inter
  GROUP BY bukrs dt_vcto.

  wl_saida-bukrs = p_saida-bukrs.

  LOOP AT tg_0111_resumo.

    IF vg_moeda_int IS NOT INITIAL.
      vl_tot_entradas = tg_0111_resumo-dmbtr.
    ELSE.
      vl_tot_entradas = tg_0111_resumo-dmbe2.
    ENDIF.

    PERFORM atrib_vlr_dia USING tg_0111_resumo-dt_vcto
                                vl_tot_entradas
                                wl_saida.

  ENDLOOP.

  p_saida_ret = wl_saida.


ENDFORM.


FORM add_saidas_lcto  TABLES p_tp_prev
                       USING p_saida     TYPE ty_saida
                             p_relat_key TYPE lvc_nkey.

  DATA: vl_lvc_nkey  TYPE lvc_nkey,
        vl_node_text TYPE lvc_value,
        wl_saida_aux TYPE ty_saida.

*-CS2022000133-#74201-05.05.2022-JT-inicio
  SORT it_saida BY bukrs codigo.
*-CS2022000133-#74201-05.05.2022-JT-fim

  LOOP AT it_saida INTO wl_saida_aux WHERE bukrs      = p_saida-bukrs
                                       AND clas_flx   =  'S'
                                       AND tp_prev    IN p_tp_prev
                                       AND dt_versao  = p_saida-dt_versao
                                       AND versao     = p_saida-versao.

    wl_saida_aux-nivel = 'X'.

    CONCATENATE wl_saida_aux-codigo '-' wl_saida_aux-descricao
           INTO vl_node_text SEPARATED BY space.

    PERFORM add_line_tree USING p_relat_key
                                vl_node_text
                                ''
                                wl_saida_aux
                                ''
                       CHANGING vl_lvc_nkey.

  ENDLOOP.



ENDFORM.

FORM get_total_saidas  TABLES p_tp_prev
                       USING  p_saida      TYPE ty_saida
                     CHANGING p_saida_ret  TYPE ty_saida.

  DATA: BEGIN OF tg_0111_resumo OCCURS 0,
          bukrs   TYPE zfit0111-bukrs,
          dt_vcto TYPE zfit0111-dt_vcto,
          dmbtr   TYPE zfit0111-dmbtr,
          dmbe2   TYPE zfit0111-dmbe2,
        END OF tg_0111_resumo.

  DATA: wl_saida TYPE ty_saida.

  DATA: vl_tot_saidas TYPE zfit0113-dmbtr.

  SELECT bukrs dt_vcto SUM( dmbtr ) SUM( dmbe2 )
    FROM zfit0111
    INTO TABLE tg_0111_resumo
   WHERE bukrs    EQ p_saida-bukrs
     AND dt_vcto  IN s_zfbdt
     AND clas_flx EQ 'S'
     AND tp_prev  IN p_tp_prev
     AND dt_base_versao = p_saida-dt_versao
     AND versao         = p_saida-versao
     AND cx_internacional = p_inter
  GROUP BY bukrs dt_vcto.

  wl_saida-bukrs = p_saida-bukrs.

  LOOP AT tg_0111_resumo.

    IF vg_moeda_int IS NOT INITIAL.
      vl_tot_saidas = tg_0111_resumo-dmbtr.
    ELSE.
      vl_tot_saidas = tg_0111_resumo-dmbe2.
    ENDIF.

    PERFORM atrib_vlr_dia USING tg_0111_resumo-dt_vcto
                                vl_tot_saidas
                                wl_saida.

  ENDLOOP.

  p_saida_ret = wl_saida.


ENDFORM.

FORM get_saldo_final  USING    p_saida     TYPE ty_saida
                      CHANGING p_saida_ret TYPE ty_saida.

  DATA: tg_0113  TYPE TABLE OF zfit0113 WITH HEADER LINE,
        wl_saida TYPE ty_saida.

  DATA: vl_saldo_fim       TYPE zfit0113-dmbtr,
        vl_max_versao      TYPE zfit0079-versao,
        vl_max_dt_versao   TYPE zfit0079-dt_base_versao,
        vl_max_hora_versao TYPE zfit0079-hora_versao.

  SELECT *
    FROM zfit0113
    INTO TABLE tg_0113
   WHERE bukrs          = p_saida-bukrs
     AND dt_vcto        IN s_zfbdt
     AND dt_base_versao = p_saida-dt_versao
     AND versao         = p_saida-versao
     AND cx_internacional = p_inter.

  wl_saida-bukrs       = p_saida-bukrs.
  wl_saida-dt_versao   = p_saida-dt_versao.
  wl_saida-versao      = p_saida-versao.
  wl_saida-hora_versao = p_saida-hora_versao.

  "Se não informou versão nos parâmetros...Busca Versão anterior a versão atual.
  IF ( wa_dt_versao_var IS INITIAL ) OR
     ( wa_versao_var    IS INITIAL ).

    "Get Versão para gerar Variação.
    CLEAR: vl_max_versao , vl_max_dt_versao, vl_max_hora_versao.

    "Busca Versão Anterior no mesmo dia.
    SELECT MAX( versao )
      INTO (vl_max_versao )
       FROM zfit0079
      WHERE bukrs          =  p_saida-bukrs
        AND dt_base_versao = p_saida-dt_versao
        AND versao         <  p_saida-versao
        AND cx_internacional = p_inter.

    IF ( sy-subrc EQ 0 ) AND
       ( vl_max_versao IS NOT INITIAL ).

      SELECT MAX( hora_versao )
        INTO (vl_max_hora_versao )
        FROM zfit0079
        WHERE bukrs          =  p_saida-bukrs
          AND dt_base_versao =  p_saida-dt_versao
          AND versao         =  vl_max_versao
          AND cx_internacional = p_inter.

      wl_saida-dt_versao_var   = p_saida-dt_versao.
      wl_saida-versao_var      = vl_max_versao.
      wl_saida-hora_versao_var = vl_max_hora_versao.

    ELSE. "Busca Ultima Versão nos dias anteriores.

      CLEAR: vl_max_versao , vl_max_dt_versao, vl_max_hora_versao.

      SELECT MAX( dt_base_versao )
       INTO (vl_max_dt_versao)
        FROM zfit0079
       WHERE bukrs          =  p_saida-bukrs
         AND dt_base_versao <  p_saida-dt_versao.

*      SELECT SINGLE dt_base_versao
*        INTO (@vl_max_dt_versao)
*        FROM zi_fi_dt_base_zfi0101
*          WHERE bukrs          = @p_saida-bukrs
*           AND dt_base_versao < @p_saida-dt_versao.


      IF ( sy-subrc EQ 0 ) AND
         ( vl_max_dt_versao IS NOT INITIAL ).

        SELECT MAX( versao )
         INTO (vl_max_versao )
          FROM zfit0079
         WHERE bukrs          =  p_saida-bukrs
           AND dt_base_versao =  vl_max_dt_versao.

        IF ( sy-subrc EQ 0 ) AND
           ( vl_max_versao  IS NOT INITIAL ).

          SELECT MAX( hora_versao )
           INTO (vl_max_hora_versao )
            FROM zfit0079
           WHERE bukrs          =  p_saida-bukrs
             AND dt_base_versao =  vl_max_dt_versao
             AND versao         =  vl_max_versao.

          wl_saida-dt_versao_var    = vl_max_dt_versao.
          wl_saida-versao_var       = vl_max_versao.
          wl_saida-hora_versao_var  = vl_max_hora_versao.

        ENDIF.

      ENDIF.

    ENDIF.

  ELSE.

    wl_saida-dt_versao_var    = wa_dt_versao_var.
    wl_saida-versao_var       = wa_versao_var.
    wl_saida-hora_versao_var  = wa_hora_versao_var.

  ENDIF.


  LOOP AT tg_0113.

    IF vg_moeda_int IS NOT INITIAL.
      vl_saldo_fim = tg_0113-sdo_final_r.
    ELSE.
      vl_saldo_fim = tg_0113-sdo_final_us.
    ENDIF.

    PERFORM atrib_vlr_dia USING tg_0113-dt_vcto
                                vl_saldo_fim
                                wl_saida.

  ENDLOOP.

  p_saida_ret = wl_saida.

ENDFORM.

FORM add_saldo_final    TABLES it_node_key
                        USING  p_saldo_final TYPE ty_saida
                               p_relat_key   TYPE lvc_nkey.

  DATA: vl_lvc_nkey  TYPE lvc_nkey.

  PERFORM add_line_tree USING p_relat_key
                              'SALDO FINAL'
                              ''
                              p_saldo_final
                              '@3Z@'
                     CHANGING vl_lvc_nkey.

ENDFORM.


FORM add_pgto_bloq  TABLES it_node_key
                    USING  p_saida      TYPE ty_saida
                           p_relat_key  TYPE lvc_nkey.

  DATA: wa_tot_bloq_pgto TYPE ty_saida,
        l_nvl2_key       TYPE lvc_nkey.

  REFRESH: r_tp_prev.

  PERFORM get_total_bloq_pgto USING p_saida
                           CHANGING wa_tot_bloq_pgto.

  PERFORM add_line_tree USING p_relat_key
                              'Pgtos. Bloqueados'
                              'X'
                              wa_tot_bloq_pgto
                              '@06@'
                     CHANGING l_nvl2_key.

  it_node_key = l_nvl2_key.
  APPEND it_node_key.

  PERFORM add_pgto_bloq_lcto USING p_saida
                                   l_nvl2_key.

ENDFORM.


FORM add_sld_aplic  TABLES it_node_key
                    USING  p_saida      TYPE ty_saida
                           p_relat_key  TYPE lvc_nkey.

  DATA: wa_tot_sld_aplic TYPE ty_saida,
        l_nvl2_key       TYPE lvc_nkey.


  PERFORM get_total_sld_aplic USING p_saida
                           CHANGING wa_tot_sld_aplic.

  wa_tot_sld_aplic-nivel = 'X'.

  PERFORM add_line_tree USING p_relat_key
                              'Resgate Aplicação Finan. - Sobra de Caixa'
                              ''
                              wa_tot_sld_aplic
                              '@6F@'
                     CHANGING l_nvl2_key.


ENDFORM.



FORM get_total_bloq_pgto  USING  p_saida      TYPE ty_saida
                        CHANGING p_saida_ret  TYPE ty_saida.

  DATA: BEGIN OF tg_0118_resumo OCCURS 0,
          bukrs   TYPE zfit0118-bukrs,
          dt_vcto TYPE zfit0118-dt_vcto,
        END OF tg_0118_resumo.

  DATA: BEGIN OF tg_0118_det OCCURS 0,
          bukrs    TYPE zfit0118-bukrs,
          dt_vcto  TYPE zfit0118-dt_vcto,
          clas_flx TYPE zfit0118-clas_flx,
          dmbtr    TYPE zfit0118-dmbtr,
          dmbe2    TYPE zfit0118-dmbe2,
        END OF tg_0118_det.

  DATA: wl_saida TYPE ty_saida.

  DATA: vl_tot_bloq_pgto TYPE zfit0113-dmbtr.

  SELECT a~bukrs a~dt_vcto
    INTO TABLE tg_0118_resumo
    FROM zfit0118 AS a
    INNER JOIN zfit0109 AS b ON a~codigo = b~codigo
   WHERE bukrs            EQ p_saida-bukrs
     AND a~dt_vcto        IN s_zfbdt
     AND a~dt_base_versao = p_saida-dt_versao
     AND a~versao         = p_saida-versao
     AND b~bloq_pgto      NE ''
   GROUP BY a~bukrs a~dt_vcto.

  SELECT a~bukrs a~dt_vcto a~clas_flx a~dmbtr a~dmbe2
    INTO TABLE tg_0118_det
    FROM zfit0118 AS a
    INNER JOIN zfit0109 AS b ON a~codigo = b~codigo
   WHERE bukrs            EQ p_saida-bukrs
     AND a~dt_vcto        IN s_zfbdt
     AND a~dt_base_versao = p_saida-dt_versao
     AND a~versao         = p_saida-versao
     AND b~bloq_pgto      NE ''.

  wl_saida-bukrs = p_saida-bukrs.

  LOOP AT tg_0118_resumo.

    CLEAR: vl_tot_bloq_pgto.

    "Buscar Saldo do dia do Vencimento.
    LOOP AT tg_0118_det WHERE bukrs   = tg_0118_resumo-bukrs
                          AND dt_vcto = tg_0118_resumo-dt_vcto.

      IF vg_moeda_int IS NOT INITIAL.

        IF tg_0118_det-clas_flx = 'S'.
          vl_tot_bloq_pgto  = vl_tot_bloq_pgto   + ( abs( tg_0118_det-dmbtr ) * -1 ).
        ELSE.
          vl_tot_bloq_pgto  = vl_tot_bloq_pgto   + abs( tg_0118_det-dmbtr ).
        ENDIF.

      ELSE.

        IF tg_0118_det-clas_flx = 'S'.
          vl_tot_bloq_pgto = vl_tot_bloq_pgto  + ( abs( tg_0118_det-dmbe2 ) * -1 ).
        ELSE.
          vl_tot_bloq_pgto = vl_tot_bloq_pgto  + abs( tg_0118_det-dmbe2 ).
        ENDIF.

      ENDIF.

    ENDLOOP.

    PERFORM atrib_vlr_dia USING tg_0118_resumo-dt_vcto
                                vl_tot_bloq_pgto
                                wl_saida.

  ENDLOOP.

  p_saida_ret = wl_saida.


ENDFORM.

FORM get_total_sld_aplic  USING  p_saida      TYPE ty_saida
                        CHANGING p_saida_ret  TYPE ty_saida.

  DATA: BEGIN OF tg_0118_resumo OCCURS 0,
          bukrs   TYPE zfit0118-bukrs,
          dt_vcto TYPE zfit0118-dt_vcto,
        END OF tg_0118_resumo.

  DATA: BEGIN OF tg_0118_det OCCURS 0,
          bukrs    TYPE zfit0118-bukrs,
          dt_vcto  TYPE zfit0118-dt_vcto,
          codigo   TYPE zfit0118-codigo,
          tp_prev  TYPE zfit0118-tp_prev,
          clas_flx TYPE zfit0118-clas_flx,
          dmbtr    TYPE zfit0118-dmbtr,
          dmbe2    TYPE zfit0118-dmbe2,
        END OF tg_0118_det.

  DATA: wl_saida TYPE ty_saida.

  DATA: vl_tot_sld_aplic TYPE zfit0113-dmbtr.

  "Seleciona Lançamentos Sobra Caixa Resumo por Empresa/Dt.Vcto.
  SELECT a~bukrs a~dt_vcto
    INTO TABLE tg_0118_resumo
    FROM zfit0118 AS a
    INNER JOIN zfit0109 AS b ON a~codigo = b~codigo
   WHERE bukrs            EQ p_saida-bukrs
     AND a~dt_vcto        IN s_zfbdt
     AND a~dt_base_versao = p_saida-dt_versao
     AND a~versao         = p_saida-versao
     AND a~cx_internacional = p_inter
     AND b~processo_esp   = 'XRT-S'
   GROUP BY a~bukrs a~dt_vcto.

  "Seleciona Lançamentos Sobra Caixa Detalhes
  SELECT a~bukrs a~dt_vcto a~codigo a~tp_prev a~clas_flx a~dmbtr a~dmbe2
    INTO TABLE tg_0118_det
    FROM zfit0118 AS a
    INNER JOIN zfit0109 AS b ON a~codigo = b~codigo
   WHERE bukrs            EQ p_saida-bukrs
     AND a~dt_vcto        IN s_zfbdt
     AND a~dt_base_versao = p_saida-dt_versao
     AND a~versao         = p_saida-versao
     AND a~cx_internacional = p_inter
     AND b~processo_esp   = 'XRT-S'.

  wl_saida-bukrs         = p_saida-bukrs.
  wl_saida-dt_versao     = p_saida-dt_versao.
  wl_saida-versao        = p_saida-versao.
  wl_saida-hora_versao   = p_saida-hora_versao.

  LOOP AT tg_0118_resumo.

    CLEAR: vl_tot_sld_aplic.

    "Buscar Saldo do dia do Vencimento.
    LOOP AT tg_0118_det WHERE bukrs   = tg_0118_resumo-bukrs
                          AND dt_vcto = tg_0118_resumo-dt_vcto.

      wl_saida-codigo   = tg_0118_det-codigo.
      wl_saida-tp_prev  = tg_0118_det-tp_prev.
      wl_saida-clas_flx = tg_0118_det-clas_flx.

      IF vg_moeda_int IS NOT INITIAL.

        IF tg_0118_det-clas_flx = 'S'.
          vl_tot_sld_aplic  = vl_tot_sld_aplic   + ( abs( tg_0118_det-dmbtr ) * -1 ).
        ELSE.
          vl_tot_sld_aplic  = vl_tot_sld_aplic   + abs( tg_0118_det-dmbtr ).
        ENDIF.

      ELSE.

        IF tg_0118_det-clas_flx = 'S'.
          vl_tot_sld_aplic = vl_tot_sld_aplic  + ( abs( tg_0118_det-dmbe2 ) * -1 ).
        ELSE.
          vl_tot_sld_aplic = vl_tot_sld_aplic  + abs( tg_0118_det-dmbe2 ).
        ENDIF.

      ENDIF.

    ENDLOOP.

    PERFORM atrib_vlr_dia USING tg_0118_resumo-dt_vcto
                                vl_tot_sld_aplic
                                wl_saida.
  ENDLOOP.

  " Montar Previsão com os Ajustes da Tesouraria em cima do
  " Saldo de Aplicação Sobra de Caixa.
  PERFORM aplicar_ajustes_sobra_cxa USING wl_saida
                                          vl_tot_sld_aplic.

  p_saida_ret = wl_saida.


ENDFORM.

FORM get_total_ajustes  USING  p_saida      TYPE ty_saida
                      CHANGING p_saida_ret  TYPE ty_saida.

  DATA: BEGIN OF tg_0111_resumo OCCURS 0,
          bukrs   TYPE zfit0111-bukrs,
          dt_vcto TYPE zfit0111-dt_vcto,
        END OF tg_0111_resumo.

  DATA: BEGIN OF tg_0111_det OCCURS 0,
          bukrs    TYPE zfit0111-bukrs,
          dt_vcto  TYPE zfit0111-dt_vcto,
          clas_flx TYPE zfit0111-clas_flx,
          dmbtr    TYPE zfit0111-dmbtr,
          dmbe2    TYPE zfit0111-dmbe2,
        END OF tg_0111_det.

  DATA: wl_saida TYPE ty_saida.

  DATA: vl_tot_bloq_pgto TYPE zfit0113-dmbtr.

  SELECT bukrs dt_vcto
    INTO TABLE tg_0111_resumo
    FROM zfit0111 AS a
   WHERE bukrs          EQ p_saida-bukrs
     AND dt_vcto        IN s_zfbdt
     AND dt_base_versao EQ p_saida-dt_versao
     AND versao         EQ p_saida-versao
     AND tp_prev        EQ 'T'
     AND cx_internacional EQ p_inter
   GROUP BY bukrs dt_vcto.

  SELECT bukrs dt_vcto clas_flx dmbtr dmbe2
    INTO TABLE tg_0111_det
    FROM zfit0111 AS a
   WHERE bukrs            EQ p_saida-bukrs
     AND dt_vcto          IN s_zfbdt
     AND dt_base_versao   EQ p_saida-dt_versao
     AND versao           EQ p_saida-versao
     AND tp_prev          EQ 'T'
     AND cx_internacional EQ p_inter.

  wl_saida-bukrs = p_saida-bukrs.

  LOOP AT tg_0111_resumo.

    CLEAR: vl_tot_bloq_pgto.

    "Buscar Saldo do dia do Vencimento.
    LOOP AT tg_0111_det WHERE bukrs   = tg_0111_resumo-bukrs
                          AND dt_vcto = tg_0111_resumo-dt_vcto.

      IF vg_moeda_int IS NOT INITIAL.

        IF tg_0111_det-clas_flx = 'S'.
          vl_tot_bloq_pgto  = vl_tot_bloq_pgto   + ( abs( tg_0111_det-dmbtr ) * -1 ).
        ELSE.
          vl_tot_bloq_pgto  = vl_tot_bloq_pgto   + abs( tg_0111_det-dmbtr ).
        ENDIF.

      ELSE.

        IF tg_0111_det-clas_flx = 'S'.
          vl_tot_bloq_pgto = vl_tot_bloq_pgto  + ( abs( tg_0111_det-dmbe2 ) * -1 ).
        ELSE.
          vl_tot_bloq_pgto = vl_tot_bloq_pgto  + abs( tg_0111_det-dmbe2 ).
        ENDIF.

      ENDIF.

    ENDLOOP.

    PERFORM atrib_vlr_dia USING tg_0111_resumo-dt_vcto
                                vl_tot_bloq_pgto
                                wl_saida.

  ENDLOOP.

  p_saida_ret = wl_saida.


ENDFORM.

FORM add_line_tree USING p_relat_key     TYPE lvc_nkey
                         p_text          TYPE lvc_value
                         p_folder        TYPE c
                         p_saida         TYPE ty_saida
                         p_image         TYPE string
                CHANGING p_new_node_key  TYPE lvc_nkey.

  DATA: l_node_text    TYPE lvc_value,
        lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi,
        ls_node        TYPE lvc_s_layn.

  DATA: vl_style TYPE i.

  ls_item_layout-fieldname = g_tree->c_hierarchy_column_name.

  IF ( p_text EQ 'SALDO INICIAL' ) OR ( p_text EQ 'SALDO FINAL' ).
    ls_item_layout-style     = cl_gui_column_tree=>style_emphasized.
  ELSEIF ( p_text(8) EQ 'VARIAÇÃO' ).
    ls_item_layout-style     = cl_gui_column_tree=>style_emphasized_negative.
  ELSE.
    ls_item_layout-style     = cl_gui_column_tree=>style_default.
  ENDIF.

  "LS_ITEM_LAYOUT-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.

  IF p_image IS NOT INITIAL.
    ls_item_layout-t_image = p_image.
  ENDIF.
  APPEND ls_item_layout TO lt_item_layout.

  l_node_text       = p_text.
  ls_node-exp_image = space.

  IF ( p_text EQ 'SALDO INICIAL' ) OR ( p_text EQ 'SALDO FINAL' ).
    ls_node-style  = cl_gui_column_tree=>style_emphasized.
  ELSEIF ( p_text(8) EQ 'VARIAÇÃO' ).
    ls_node-style  = cl_gui_column_tree=>style_emphasized_negative.
  ELSE.
    ls_node-style  = cl_gui_column_tree=>style_intensified.
  ENDIF.


  IF p_folder IS NOT INITIAL.
    ls_node-isfolder = 'X'.
  ENDIF.

  CALL METHOD g_tree->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = p_saida
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_new_node_key.

ENDFORM.

FORM lcto_ajuste.

  SUBMIT zfir0068 AND RETURN.

ENDFORM.

FORM limpa_dados .

  REFRESH: tg_0109,
           tg_0111,
           tg_0111_group,
           tg_0118,
           tg_0118_group,
           tg_t001,
           it_saida,
           it_scalc_saldo,
           it_alv_tree,
           tg_last_versao,
           it_mov_ajuste,
           tg_days_mov_ajuste.

ENDFORM.

FORM limpa_dias_mov .

  CLEAR:  day_01_mov, day_02_mov, day_03_mov, day_04_mov, day_05_mov,
          day_06_mov, day_07_mov, day_08_mov, day_09_mov, day_10_mov,
          day_11_mov, day_12_mov, day_13_mov, day_14_mov, day_15_mov,
          day_16_mov, day_17_mov, day_18_mov, day_19_mov, day_20_mov,
          day_21_mov, day_22_mov, day_23_mov, day_24_mov, day_25_mov,
          day_26_mov, day_27_mov, day_28_mov, day_29_mov, day_30_mov, day_31_mov,

          day_01_ajuste, day_02_ajuste, day_03_ajuste, day_04_ajuste, day_05_ajuste,
          day_06_ajuste, day_07_ajuste, day_08_ajuste, day_09_ajuste, day_10_ajuste,
          day_11_ajuste, day_12_ajuste, day_13_ajuste, day_14_ajuste, day_15_ajuste,
          day_16_ajuste, day_17_ajuste, day_18_ajuste, day_19_ajuste, day_20_ajuste,
          day_21_ajuste, day_22_ajuste, day_23_ajuste, day_24_ajuste, day_25_ajuste,
          day_26_ajuste, day_27_ajuste, day_28_ajuste, day_29_ajuste, day_30_ajuste, day_31_ajuste.

ENDFORM.


FORM fm_register_event.

  DATA: lt_events        TYPE cntl_simple_events,
        l_event          TYPE cntl_simple_event,
        l_event_receiver TYPE REF TO lcl_tree_event_receiver.

*Get register events
  CALL METHOD g_tree->get_registered_events
    IMPORTING
      events = lt_events.

*Frontend registration: add additional event ids
  l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  APPEND l_event TO lt_events.

  "L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_LINK_CLICK.
  "APPEND L_EVENT TO LT_EVENTS.

*Frontend registration:provide new event table to alv tree
  CALL METHOD g_tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  CREATE OBJECT l_event_receiver.
  SET HANDLER: l_event_receiver->handle_node_double_click FOR g_tree.
  "SET HANDLER: L_EVENT_RECEIVER->HANDLE_LINK_CLICK        FOR G_TREE.

ENDFORM.

FORM refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM handle_hotspot_click  USING    i_row_id     TYPE lvc_s_row
                                    i_column_id  TYPE lvc_s_col
                                    is_row_no    TYPE lvc_s_roid.

  DATA opt TYPE ctu_params.

  DATA: vl_dt_vcto TYPE zfit0079-zfbdt,
        vl_dt_c(8) TYPE c,
        vl_zfbdt   TYPE sy-datum,
        vl_cd_flx  TYPE zfit0079-codigo.

  CLEAR: vl_dt_c,vl_zfbdt, vl_cd_flx.

  CASE i_column_id.
    WHEN 'DAY_01'.
      vl_dt_vcto = day_01_mov.
    WHEN 'DAY_02'.
      vl_dt_vcto = day_02_mov.
    WHEN 'DAY_03'.
      vl_dt_vcto = day_03_mov.
    WHEN 'DAY_04'.
      vl_dt_vcto = day_04_mov.
    WHEN 'DAY_05'.
      vl_dt_vcto = day_05_mov.
    WHEN 'DAY_06'.
      vl_dt_vcto = day_06_mov.
    WHEN 'DAY_07'.
      vl_dt_vcto = day_07_mov.
    WHEN 'DAY_08'.
      vl_dt_vcto = day_08_mov.
    WHEN 'DAY_09'.
      vl_dt_vcto = day_09_mov.
    WHEN 'DAY_10'.
      vl_dt_vcto = day_10_mov.
    WHEN 'DAY_11'.
      vl_dt_vcto = day_11_mov.
    WHEN 'DAY_12'.
      vl_dt_vcto = day_12_mov.
    WHEN 'DAY_13'.
      vl_dt_vcto = day_13_mov.
    WHEN 'DAY_14'.
      vl_dt_vcto = day_14_mov.
    WHEN 'DAY_15'.
      vl_dt_vcto = day_15_mov.
    WHEN 'DAY_16'.
      vl_dt_vcto = day_16_mov.
    WHEN 'DAY_17'.
      vl_dt_vcto = day_17_mov.
    WHEN 'DAY_18'.
      vl_dt_vcto = day_18_mov.
    WHEN 'DAY_19'.
      vl_dt_vcto = day_19_mov.
    WHEN 'DAY_20'.
      vl_dt_vcto = day_20_mov.
    WHEN 'DAY_21'.
      vl_dt_vcto = day_21_mov.
    WHEN 'DAY_22'.
      vl_dt_vcto = day_22_mov.
    WHEN 'DAY_23'.
      vl_dt_vcto = day_23_mov.
    WHEN 'DAY_24'.
      vl_dt_vcto = day_24_mov.
    WHEN 'DAY_25'.
      vl_dt_vcto = day_25_mov.
    WHEN 'DAY_26'.
      vl_dt_vcto = day_26_mov.
    WHEN 'DAY_27'.
      vl_dt_vcto = day_27_mov.
    WHEN 'DAY_28'.
      vl_dt_vcto = day_28_mov.
    WHEN 'DAY_29'.
      vl_dt_vcto = day_29_mov.
    WHEN 'DAY_30'.
      vl_dt_vcto = day_30_mov.
    WHEN 'DAY_31'.
      vl_dt_vcto = day_31_mov.
  ENDCASE.

  IF vl_dt_vcto IS NOT INITIAL.

    IF wa_saida_mov_flx-varicacao IS NOT INITIAL. "Exibe Lançamentos de Variação

      PERFORM seleciona_dados_varicacao USING vl_dt_vcto.

      CALL SCREEN 0103 STARTING AT 02 02 ENDING AT 165 27.

    ELSE.

      vl_cd_flx = wa_saida_mov_flx-codigo.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_cd_flx
        IMPORTING
          output = vl_cd_flx.

      SUBMIT zfir0053 WITH s_bukrs         = wa_saida_mov_flx-bukrs
                      WITH s_zfbdt         = vl_dt_vcto
                      WITH s_cod_f         = vl_cd_flx
                      WITH s_prev          = wa_saida_mov_flx-tp_prev
                      WITH s_cls_f         = wa_saida_mov_flx-clas_flx
                      WITH s_dt_vrs        = wa_saida_mov_flx-dt_versao
                      WITH s_vrs           = wa_saida_mov_flx-versao
                      WITH p_inter         = p_inter
                      AND RETURN.
    ENDIF.

  ENDIF.

ENDFORM.                    "HANDLE_HOTSPOT_CLICK

FORM add_variacao  TABLES it_node_key
                   USING  p_saida     TYPE ty_saida
                          p_relat_key TYPE lvc_nkey.

  DATA: vl_lvc_nkey TYPE lvc_nkey.

  DATA: tg_0113     TYPE TABLE OF zfit0113 WITH HEADER LINE,
        tg_0113_2   TYPE TABLE OF zfit0113 WITH HEADER LINE,
        tg_0113_aux TYPE TABLE OF zfit0113 WITH HEADER LINE,
        wl_saida    TYPE ty_saida.

  DATA: vl_saldo   TYPE zfit0113-dmbtr,
        vl_valor_1 TYPE zfit0113-dmbtr,
        vl_valor_2 TYPE zfit0113-dmbtr.

  DATA: vl_versao_1 TYPE string,
        vl_versao_2 TYPE string,
        vl_variacao TYPE lvc_value,
        vl_hora_vs1 TYPE string,
        vl_hora_vs2 TYPE string.

  DATA: vl_max_dt_vcto TYPE zfit0113-dt_vcto.

  SELECT *
    FROM zfit0113
    INTO TABLE tg_0113
   WHERE bukrs          = p_saida-bukrs
     AND dt_vcto        IN s_zfbdt
     AND dt_base_versao = p_saida-dt_versao
     AND versao         = p_saida-versao
     AND cx_internacional EQ p_inter.

  wl_saida-bukrs         = p_saida-bukrs.
  wl_saida-dt_versao     = p_saida-dt_versao.
  wl_saida-versao        = p_saida-versao.
  wl_saida-hora_versao   = p_saida-hora_versao.

  wl_saida-dt_versao_var   = p_saida-dt_versao_var.
  wl_saida-versao_var      = p_saida-versao_var.
  wl_saida-hora_versao_var = p_saida-hora_versao_var.

  wl_saida-nivel = 'X'.
  wl_saida-varicacao = 'X'.

  CHECK p_saida-dt_versao_var IS NOT INITIAL AND
        p_saida-versao_var IS NOT INITIAL.

** performance 131155 - inicio

*  SELECT *
*  INTO TABLE @DATA(lt_itens_1)
*  FROM zfit0079 AS a
*    WHERE bukrs       EQ @p_saida-bukrs
*   AND zfbdt          IN @s_zfbdt
*   AND dt_base_versao EQ @p_saida-dt_versao
*   AND versao         EQ @p_saida-versao.
*
*  SELECT *
*   INTO TABLE @DATA(lt_itens_2)
*   FROM zfit0079 AS a
*     WHERE bukrs       EQ @p_saida-bukrs
*    AND zfbdt          IN @s_zfbdt
*    AND dt_base_versao EQ @p_saida-dt_versao_var
*    AND versao         EQ @p_saida-versao_var.
*
*  DATA(lv_qtd_version) = lines( lt_itens_1 ).
*  DATA(lv_qtd_oldversion) = lines( lt_itens_2 ).
*
*  IF lv_qtd_version <> lv_qtd_oldversion .
*
*    SORT: lt_itens_1 BY bukrs belnr gjahr buzei,
*          lt_itens_2 BY bukrs belnr gjahr buzei.
*
*    IF lv_qtd_version > lv_qtd_oldversion.
*
*      LOOP AT lt_itens_1 ASSIGNING FIELD-SYMBOL(<fs_item1>).
*
*        READ TABLE lt_itens_2 ASSIGNING FIELD-SYMBOL(<fs_item2>)
*                                WITH KEY bukrs = <fs_item1>-bukrs
*                                         belnr = <fs_item1>-belnr
*                                         gjahr = <fs_item1>-gjahr
*                                         buzei = <fs_item1>-buzei
*                                         BINARY SEARCH.
*        IF sy-subrc <> 0.
*
*          CLEAR: tg_0113_aux, vl_saldo.
*          READ TABLE tg_0113 INTO tg_0113
*                                  WITH KEY bukrs = p_saida-bukrs
*                                           dt_vcto = <fs_item1>-zfbdt
*                                           dt_base_versao = p_saida-dt_versao
*                                           versao         = p_saida-versao.
*          IF sy-subrc = 0.
*            IF vg_moeda_int IS NOT INITIAL.
*              vl_saldo =  0  - ( tg_0113-dmbtr ).
*            ELSE.
*              vl_saldo =  0  - ( tg_0113-dmbe2 ) .
*            ENDIF.
*          ENDIF.
*
*          PERFORM atrib_vlr_dia USING tg_0113-dt_vcto
*                                      vl_saldo
*                                      wl_saida.
*
*        ENDIF.
*
*      ENDLOOP.
*
*    ELSEIF lv_qtd_oldversion > lv_qtd_version.
*
*      LOOP AT lt_itens_2 ASSIGNING <fs_item2>.
*
*        READ TABLE lt_itens_1 ASSIGNING <fs_item1>
*                                WITH KEY bukrs = <fs_item2>-bukrs
*                                         belnr = <fs_item2>-belnr
*                                         gjahr = <fs_item2>-gjahr
*                                         buzei = <fs_item2>-buzei
*                                         BINARY SEARCH.
*        IF sy-subrc <> 0.
*
*          CLEAR: tg_0113_aux, vl_saldo.
*
*          READ TABLE tg_0113 INTO tg_0113
*                                  WITH KEY bukrs = p_saida-bukrs
*                                           dt_vcto = <fs_item2>-zfbdt
*                                           dt_base_versao = p_saida-dt_versao
*                                           versao         = p_saida-versao.
*
*          IF sy-subrc = 0.
*
*            SELECT SINGLE *
*              INTO tg_0113_aux
*              FROM zfit0113
*             WHERE bukrs          = p_saida-bukrs
*               AND dt_vcto        = <fs_item2>-zfbdt
*               AND dt_base_versao = p_saida-dt_versao_var
*               AND versao         = p_saida-versao_var.
*
*            IF sy-subrc = 0.
*              IF vg_moeda_int IS NOT INITIAL.
*                vl_saldo = ( tg_0113_aux-dmbtr )  - ( tg_0113-dmbtr ).
*              ELSE.
*                vl_saldo = ( tg_0113_aux-dmbe2 ) - ( tg_0113-dmbe2 ) .
*              ENDIF.
*            ENDIF.
*
*          ENDIF.
*
*          PERFORM atrib_vlr_dia USING tg_0113-dt_vcto
*                                      vl_saldo
*                                      wl_saida.
*
*        ENDIF.
*
*      ENDLOOP.
*
*    ENDIF.
*
*
*  ENDIF.

** performance 131155 - fim

  DATA(_check_variacao) = abap_true.
  SELECT SINGLE *
    FROM tvarvc INTO @DATA(stvarv)
   WHERE name = 'NO_CHECK_VAR_LCTOS_ZFI0101'.

  IF sy-subrc EQ 0.
    _check_variacao = abap_false.
  ENDIF.

  LOOP AT tg_0113.

*-----------------------------------------------------------------------------------------------
*   Verifica se possui divergencia de Lançamentos no dia do Vencimento entre as duas versões.
*-----------------------------------------------------------------------------------------------

    IF _check_variacao EQ abap_true.
      "Lançamentos da primeira versão que não existe na segunda versão.
      PERFORM verifica_variacao_lctos TABLES tg_0079 tg_0119
                                       USING p_saida-bukrs
                                             tg_0113-dt_vcto
                                             p_saida-dt_versao
                                             p_saida-versao
                                             p_saida-dt_versao_var
                                             p_saida-versao_var.

      IF ( tg_0079[] IS INITIAL ) AND ( tg_0119[] IS INITIAL ).

        "Lançamentos da primeira versão que não existe na segunda versão.
        PERFORM verifica_variacao_lctos TABLES tg_0079 tg_0119
                                         USING p_saida-bukrs
                                               tg_0113-dt_vcto
                                               p_saida-dt_versao_var
                                               p_saida-versao_var
                                               p_saida-dt_versao
                                               p_saida-versao.

        IF ( tg_0079[] IS INITIAL ) AND ( tg_0119[] IS INITIAL ).
          CONTINUE.
        ENDIF.

      ENDIF.
    ENDIF.

    CLEAR: tg_0113_aux, vl_saldo.
    SELECT SINGLE *
      INTO tg_0113_aux
      FROM zfit0113
     WHERE bukrs          = p_saida-bukrs
       AND dt_vcto        = tg_0113-dt_vcto
       AND dt_base_versao = p_saida-dt_versao_var
       AND versao         = p_saida-versao_var
       AND cx_internacional = p_inter.

    IF sy-subrc = 0.
      IF vg_moeda_int IS NOT INITIAL.
        vl_saldo = ( tg_0113_aux-dmbtr )  - ( tg_0113-dmbtr ).
      ELSE.
        vl_saldo = ( tg_0113_aux-dmbe2 ) - ( tg_0113-dmbe2 ) .
      ENDIF.
    ELSE.
      IF vg_moeda_int IS NOT INITIAL.
        vl_saldo =  0  - ( tg_0113-dmbtr ).
      ELSE.
        vl_saldo =  0  - ( tg_0113-dmbe2 ) .
      ENDIF.
    ENDIF.

    PERFORM atrib_vlr_dia USING tg_0113-dt_vcto
                                vl_saldo
                                wl_saida.

  ENDLOOP.

  CLEAR: vl_variacao, vl_versao_1, vl_versao_2.

  IF ( p_saida-dt_versao IS NOT INITIAL ) AND
     ( p_saida-versao IS NOT INITIAL ) AND
     ( p_saida-dt_versao_var IS NOT INITIAL ) AND
     ( p_saida-versao_var IS NOT INITIAL ).

    CLEAR: vl_hora_vs1 , vl_hora_vs2.

    PERFORM formata_hora USING p_saida-hora_versao     CHANGING vl_hora_vs1.
    PERFORM formata_hora USING p_saida-hora_versao_var CHANGING vl_hora_vs2.

    PERFORM formata_data USING p_saida-dt_versao CHANGING vl_versao_1.

    CONCATENATE 'Vrs.:' p_saida-versao '-' vl_versao_1 '-' vl_hora_vs1
           INTO vl_versao_1 SEPARATED BY space.


    PERFORM formata_data USING p_saida-dt_versao_var  CHANGING vl_versao_2.

    CONCATENATE 'Vrs.:' p_saida-versao_var '-' vl_versao_2 '-' vl_hora_vs2
           INTO vl_versao_2 SEPARATED BY space.

    CONCATENATE 'VARIAÇÃO -' vl_versao_1 '/' vl_versao_2
           INTO vl_variacao SEPARATED BY space.

    wl_saida-txt_vrs1 = vl_versao_1.
    wl_saida-txt_vrs2 = vl_versao_2.

  ELSE.
    vl_variacao = 'VARIAÇÃO'.
  ENDIF.

  PERFORM add_line_tree USING p_relat_key
                              vl_variacao
                              ''
                              wl_saida
                              '@2B@'
                     CHANGING vl_lvc_nkey.


ENDFORM.


FORM pesq_versao  USING    VALUE(p_versao).

  "TYPES
  TYPES: BEGIN OF ty_versao,
           "BUKRS           TYPE ZFIT0079-BUKRS,
           "DT_BASE_VERSAO  TYPE ZFIT0079-DT_BASE_VERSAO,
           versao      TYPE zfit0111-versao,
           hora_versao TYPE zfit0111-hora_versao,
         END OF ty_versao.

  DATA: lt_versao TYPE TABLE OF ty_versao,
        ls_versao TYPE ty_versao,
        lt_map    TYPE TABLE OF dselc,
        ls_map    TYPE dselc,
        lt_return TYPE TABLE OF ddshretval,
        ls_return TYPE ddshretval,
        ls_stable TYPE lvc_s_stbl.

  IF wa_bukrs_versao IS INITIAL.
    MESSAGE 'Selecione a Empresa!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF p_versao EQ '1'.

    CLEAR: wa_hora_versao, wa_versao.

    IF wa_dt_versao IS INITIAL.
      MESSAGE 'Selecione a data da Versão!' TYPE 'S'.
      EXIT.
    ENDIF.

    "LOAD F4 DATA
    SELECT versao hora_versao
      INTO TABLE lt_versao
      FROM zfit0111
     WHERE bukrs          = wa_bukrs_versao
       AND dt_base_versao = wa_dt_versao.

  ELSE.
    CLEAR: wa_hora_versao_var, wa_versao_var.

    IF wa_dt_versao_var IS INITIAL.
      MESSAGE 'Selecione a data da Versão!' TYPE 'S'.
      EXIT.
    ENDIF.

    "LOAD F4 DATA
    SELECT versao hora_versao
      INTO TABLE lt_versao
      FROM zfit0111
     WHERE bukrs          = wa_bukrs_versao
       AND dt_base_versao = wa_dt_versao_var.

  ENDIF.

  SORT lt_versao BY versao.
  DELETE ADJACENT DUPLICATES FROM lt_versao COMPARING versao.

  "SET RETURN FIELD
  CLEAR ls_map.
  ls_map-fldname = 'F0001'.
  ls_map-dyfldname = 'VERSAO'.
  APPEND ls_map TO lt_map.

  "SET RETURN FIELD
  CLEAR ls_map.
  ls_map-fldname = 'F0002'.
  ls_map-dyfldname = 'HORA_VERSAO'.
  APPEND ls_map TO lt_map.


  " CALL SEARCH HELP POPUP FUNCTION
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VERSAO'
      value_org       = 'S'
    TABLES
      value_tab       = lt_versao
      dynpfld_mapping = lt_map
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.


  " READ SELECTED F4 VALUE
  READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0001'.
  IF ls_return IS NOT INITIAL.

    IF p_versao EQ '1'.
      wa_versao     = ls_return-fieldval.
    ELSEIF p_versao EQ '2'.
      wa_versao_var = ls_return-fieldval.
    ENDIF.

  ENDIF.

  " READ SELECTED F4 VALUE
  READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0002'.
  IF ls_return IS NOT INITIAL.

    IF p_versao EQ '1'.

      wa_hora_versao     = ls_return-fieldval.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_hora_versao
        IMPORTING
          output = wa_hora_versao.

    ELSEIF p_versao EQ '2'.
      wa_hora_versao_var = ls_return-fieldval.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_hora_versao_var
        IMPORTING
          output = wa_hora_versao_var.

    ENDIF.

  ENDIF.


ENDFORM.

FORM seleciona_dados_varicacao USING p_dt_vcto TYPE zfit0079-zfbdt .

  DATA: tg_0109_aux  TYPE TABLE OF zfit0109 WITH HEADER LINE.

*=====================================================================*
* Lançamentos primeira versão que não existem na segunda versão
*=====================================================================*
  REFRESH: tg_0079, tg_0119, it_saida_var1, tg_kna1, tg_lfa1.

  PERFORM verifica_variacao_lctos TABLES tg_0079 tg_0119
                                   USING wa_saida_mov_flx-bukrs
                                         p_dt_vcto
                                         wa_saida_mov_flx-dt_versao
                                         wa_saida_mov_flx-versao
                                         wa_saida_mov_flx-dt_versao_var
                                         wa_saida_mov_flx-versao_var.

  IF tg_0079[] IS NOT INITIAL.

    SELECT lifnr name1
      FROM lfa1 INTO TABLE tg_lfa1
      FOR ALL ENTRIES IN tg_0079
      WHERE lifnr = tg_0079-lifnr.

    SELECT kunnr name1
      FROM kna1 INTO TABLE tg_kna1
      FOR ALL ENTRIES IN tg_0079
      WHERE kunnr = tg_0079-kunnr.

    SELECT * FROM skat INTO TABLE tg_skat
     FOR ALL ENTRIES IN tg_0079
     WHERE spras EQ sy-langu
       AND ktopl EQ '0050'
       AND saknr EQ tg_0079-hkont.

  ENDIF.

  "Processa Dados ------------------
  CLEAR wa_saida_var1.

*---> 05/07/2023 - Migração S4 - DL
  SORT tg_kna1 BY kunnr.
  SORT tg_lfa1 BY lifnr.
  SORT tg_skat BY saknr.
*<--- 05/07/2023 - Migração S4 - DL

  LOOP AT tg_0079.

    IF tg_0079-lifnr IS NOT INITIAL.

      READ TABLE tg_lfa1 WITH KEY lifnr = tg_0079-lifnr
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_saida_var1-name1 = tg_lfa1-name1.
      ENDIF.

    ENDIF.

    IF tg_0079-kunnr IS NOT INITIAL.

      READ TABLE tg_kna1 WITH KEY kunnr = tg_0079-kunnr
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_saida_var1-name1 = tg_kna1-name1.
      ENDIF.

    ENDIF.

    IF tg_0079-hkont IS NOT INITIAL.

      READ TABLE tg_skat WITH KEY saknr = tg_0079-hkont
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_saida_var1-txt50 = tg_skat-txt50.
      ENDIF.

    ENDIF.


    CLEAR: tg_0109_aux.
    SELECT SINGLE *
      INTO tg_0109_aux
      FROM zfit0109
     WHERE codigo = tg_0079-codigo.

    IF sy-subrc = 0.
      wa_saida_var1-desc_flx =  tg_0109_aux-descricao.
    ENDIF.

    MOVE-CORRESPONDING: tg_0079 TO wa_saida_var1.

    APPEND wa_saida_var1 TO it_saida_var1.
    CLEAR wa_saida_var1.
  ENDLOOP.

  "Lctos XRT
  LOOP AT tg_0119.

    CLEAR: tg_0109_aux.
    SELECT SINGLE *
      INTO tg_0109_aux
      FROM zfit0109
     WHERE codigo = tg_0119-codigo.

    IF sy-subrc = 0.
      wa_saida_var1-desc_flx =  tg_0109_aux-descricao.
    ENDIF.

    MOVE-CORRESPONDING: tg_0119   TO wa_saida_var1.

    APPEND wa_saida_var1 TO it_saida_var1.
    CLEAR wa_saida_var1.
  ENDLOOP.

*=====================================================================*
* Lançamentos segunda versão que não existem na primeira versão
*=====================================================================*
  REFRESH: tg_0079, tg_0119, it_saida_var2, tg_kna1, tg_lfa1.

  PERFORM verifica_variacao_lctos TABLES tg_0079 tg_0119
                                   USING wa_saida_mov_flx-bukrs
                                         p_dt_vcto
                                         wa_saida_mov_flx-dt_versao_var
                                         wa_saida_mov_flx-versao_var
                                         wa_saida_mov_flx-dt_versao
                                         wa_saida_mov_flx-versao.

  IF tg_0079[] IS NOT INITIAL.

    SELECT lifnr name1
      FROM lfa1 INTO TABLE tg_lfa1
      FOR ALL ENTRIES IN tg_0079
      WHERE lifnr = tg_0079-lifnr.

    SELECT kunnr name1
      FROM kna1 INTO TABLE tg_kna1
      FOR ALL ENTRIES IN tg_0079
      WHERE kunnr = tg_0079-kunnr.

    SELECT * FROM skat INTO TABLE tg_skat
     FOR ALL ENTRIES IN tg_0079
     WHERE spras EQ sy-langu
       AND ktopl EQ '0050'
       AND saknr EQ tg_0079-hkont.

  ENDIF.

  "Processa Dados ------------------
  CLEAR wa_saida_var2.

*---> 05/07/2023 - Migração S4 - DL
  SORT tg_lfa1 BY lifnr.
  SORT tg_kna1 BY kunnr.
  SORT tg_skat BY saknr.
*<--- 05/07/2023 - Migração S4 - DL

  LOOP AT tg_0079.

    IF tg_0079-lifnr IS NOT INITIAL.

      READ TABLE tg_lfa1 WITH KEY lifnr = tg_0079-lifnr
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_saida_var2-name1 = tg_lfa1-name1.
      ENDIF.

    ENDIF.

    IF tg_0079-kunnr IS NOT INITIAL.

      READ TABLE tg_kna1 WITH KEY kunnr = tg_0079-kunnr
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_saida_var2-name1 = tg_kna1-name1.
      ENDIF.

    ENDIF.

    IF tg_0079-hkont IS NOT INITIAL.

      READ TABLE tg_skat WITH KEY saknr = tg_0079-hkont
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_saida_var1-txt50 = tg_skat-txt50.
      ENDIF.

    ENDIF.

    CLEAR: tg_0109_aux.
    SELECT SINGLE *
      INTO tg_0109_aux
      FROM zfit0109
     WHERE codigo = tg_0079-codigo.

    IF sy-subrc = 0.
      wa_saida_var2-desc_flx =  tg_0109_aux-descricao.
    ENDIF.

    MOVE-CORRESPONDING: tg_0079 TO wa_saida_var2.

    APPEND wa_saida_var2 TO it_saida_var2.
    CLEAR wa_saida_var2.
  ENDLOOP.

  "XRT
  LOOP AT tg_0119.

    CLEAR: tg_0109_aux.
    SELECT SINGLE *
      INTO tg_0109_aux
      FROM zfit0109
     WHERE codigo = tg_0119-codigo.

    IF sy-subrc = 0.
      wa_saida_var2-desc_flx =  tg_0109_aux-descricao.
    ENDIF.

    MOVE-CORRESPONDING: tg_0119 TO wa_saida_var2.

    APPEND wa_saida_var2 TO it_saida_var2.
    CLEAR wa_saida_var2.
  ENDLOOP.



ENDFORM.


FORM verifica_variacao_lctos TABLES p_0079 p_0119
                              USING p_bukrs
                                    p_dt_vcto
                                    p_dt_versao_1
                                    p_versao_1
                                    p_dt_versao_2
                                    p_versao_2.

  REFRESH p_0079.
  SELECT *
    INTO TABLE p_0079
    FROM zfit0079 AS a
   WHERE bukrs          EQ p_bukrs
     AND zfbdt          EQ p_dt_vcto
     AND dt_base_versao EQ p_dt_versao_1
     AND versao         EQ p_versao_1
     AND cx_internacional EQ p_inter
     AND NOT EXISTS ( SELECT *
                        FROM zfit0079 AS b
                       WHERE b~bukrs          EQ  p_bukrs
                         AND b~zfbdt          EQ  p_dt_vcto
                         AND b~dt_base_versao EQ  p_dt_versao_2
                         AND b~versao         EQ  p_versao_2
                         AND b~belnr          EQ  a~belnr
                         AND b~gjahr          EQ  a~gjahr
                         AND b~buzei          EQ  a~buzei
                         AND b~budat          EQ  a~budat
                         AND b~hkont          EQ  a~hkont
                         AND b~cod_flx        EQ  a~cod_flx
                         AND b~planilha       EQ  a~planilha
                         AND b~planilha_itm   EQ  a~planilha_itm
                         AND b~trade_id       EQ  a~trade_id
                         AND b~doc_imposto    EQ  a~doc_imposto
                         AND b~seqitem        EQ  a~seqitem
                         AND b~cd_prev        EQ  a~cd_prev
                         AND b~obj_key_prev   EQ  a~obj_key_prev
                         AND b~dmbtr          EQ  a~dmbtr
                         AND b~dmbe2          EQ  a~dmbe2
                         ).

  "Lctos XRT
  REFRESH p_0119.
  SELECT *
    INTO TABLE p_0119
    FROM zfit0119 AS a
   WHERE bukrs           EQ p_bukrs
     AND zfbdt           EQ p_dt_vcto
     AND dt_base_versao  EQ p_dt_versao_1
     AND versao          EQ p_versao_1
     AND NOT EXISTS ( SELECT *
                        FROM zfit0119 AS b
                       WHERE b~bukrs           EQ  p_bukrs
                         AND b~zfbdt           EQ  p_dt_vcto
                         AND b~dt_base_versao  EQ  p_dt_versao_2
                         AND b~versao          EQ  p_versao_2
                         AND b~opr_numero      EQ  a~opr_numero
                         AND b~con_codigo      EQ  a~con_codigo
                         AND b~mdo_codigo      EQ  a~mdo_codigo
                         AND b~par_tipo        EQ  a~par_tipo
                         AND b~dmbtr           EQ  a~dmbtr
                         AND b~dmbe2           EQ  a~dmbe2
                         ).


ENDFORM.

FORM criar_field_catalog_0104 .

  FREE: wa_fcat, it_fcat.
  REFRESH: it_fcat.

  PERFORM estrutura_alv USING:

      03  ''  ''   'IT_SAIDA_VPROC' 'BUKRS'           'Empresa'          '07'  ' '    '' ' ' '' '' '',
      04  ''  ''   'IT_SAIDA_VPROC' 'DT_BASE_VERSAO'  'Dt. Versão'       '12'  ' '    '' ' ' '' '' '',
      05  ''  ''   'IT_SAIDA_VPROC' 'HORA_VERSAO'     'Hr. Versão'       '10'  ' '    '' ' ' '' '' '',
      06  ''  ''   'IT_SAIDA_VPROC' 'VERSAO'          'Versão'           '06'  ' '    '' ' ' '' '' '',
      07  ''  ''   'IT_SAIDA_VPROC' 'KURSF'           'Tx.Câmbio'        '10'  ' '    '' ' ' '' '' ''.

ENDFORM.

FORM criar_field_catalog_0105.

  FREE: wa_fcat, it_fcat.
  REFRESH: it_fcat.

  PERFORM estrutura_alv USING:

      03  'ZFIT0117'  'BUKRS'     'IT_SAIDA_BMOV' 'BUKRS'      'Empresa'          '07'  'X'    '' ' ' ' '  '' '',
      04  'ZFIT0117'  'DT_VCTO'   'IT_SAIDA_BMOV' 'DT_VCTO'    'Dt. Vcto'         '12'  'X'    '' ' ' ' '  '' '',
      05  'ZFIT0117'  ''          'IT_SAIDA_BMOV' 'STATUS'     'Status'           '06'  ' '    '' ' ' 'C'  '' '',
      06  'ZFIT0117'  ''          'IT_SAIDA_BMOV' 'USNAM'      'Usuário'          '08'  ' '    '' ' ' ' '  '' '',
      07  'ZFIT0117'  ''          'IT_SAIDA_BMOV' 'DT_ATUAL'   'Dt. Atualização'  '15'  ' '    '' ' ' ' '  '' '',
      08  'ZFIT0117'  ''          'IT_SAIDA_BMOV' 'HR_ATUAL'   'Hr. Atualização'  '15'  ' '    '' ' ' ' '  '' ''.



ENDFORM.

FORM lctos_bloq_mov .

  REFRESH: it_saida_bmov, tg_0117.
  CLEAR: tg_0117.

  SELECT *
    INTO TABLE tg_0117
    FROM zfit0117
   WHERE bukrs   IN s_bukrs
     AND dt_vcto IN s_zfbdt.

  LOOP AT tg_0117.

    CLEAR: wa_saida_bmov.

    MOVE-CORRESPONDING tg_0117 TO wa_saida_bmov.
    APPEND wa_saida_bmov TO it_saida_bmov.

  ENDLOOP.

  CALL SCREEN 0105 STARTING AT 02 02 ENDING AT 90 18.


ENDFORM.

FORM save_lctos_bmov .


  LOOP AT it_saida_bmov INTO wa_saida_bmov.

    CLEAR: tg_0117.

    IF wa_saida_bmov-status IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Existem Lançamentos sem Status!' TYPE 'S'.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING wa_saida_bmov TO tg_0117.

    MODIFY zfit0117 FROM tg_0117.

  ENDLOOP.

  CALL METHOD obj_alv_bmov->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  COMMIT WORK.
  MESSAGE 'Registro(s) gravado(s) com sucesso!' TYPE 'S'.


ENDFORM.

FORM muda_status_bloqueio USING p_status.

  CALL METHOD obj_alv_bmov->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CHECK it_selected_rows[] IS NOT INITIAL.

  FIELD-SYMBOLS: <saida_bmov> TYPE ty_saida_bmov.

  LOOP AT it_selected_rows INTO wa_selected_rows.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE it_saida_bmov ASSIGNING <saida_bmov> INDEX wa_selected_rows-index.

    <saida_bmov>-status   = p_status.
    <saida_bmov>-usnam    = sy-uname.
    <saida_bmov>-dt_atual = sy-datum.
    <saida_bmov>-hr_atual = sy-uzeit.

  ENDLOOP.

  CALL METHOD obj_alv_bmov->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

FORM del_bloq_mov .

  DATA: var_answer TYPE c.

  FIELD-SYMBOLS: <saida_bmov> TYPE ty_saida_bmov.

  CALL METHOD obj_alv_bmov->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CHECK it_selected_rows[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente excluir o registro?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  LOOP AT it_selected_rows INTO wa_selected_rows.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE it_saida_bmov ASSIGNING <saida_bmov> INDEX wa_selected_rows-index.

    DELETE FROM zfit0117 WHERE bukrs   = <saida_bmov>-bukrs
                           AND dt_vcto = <saida_bmov>-dt_vcto.

    DELETE it_saida_bmov INDEX wa_selected_rows-index.

  ENDLOOP.

  COMMIT WORK.
  MESSAGE 'Registro(s) excluído(s) com sucesso!' TYPE 'S'.


  CALL METHOD obj_alv_bmov->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

FORM processar_fluxo .

  DATA: vg_dt_high TYPE zfit0079-zfbdt.

  IF s_bukrs_rep-low IS INITIAL.
    MESSAGE 'Empresa é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  vg_dt_high = sy-datum.
  ADD 30 TO vg_dt_high.


  IF s_bukrs_rep-high IS NOT INITIAL.

    wa_rsparams-selname = 'S_BUKRS'.
    wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
    wa_rsparams-sign = 'I'.
    wa_rsparams-option = 'BT'.
    wa_rsparams-low    = s_bukrs_rep-low.
    wa_rsparams-high   = s_bukrs_rep-high.
    APPEND wa_rsparams TO it_rsparams.

  ELSE.
    wa_rsparams-selname = 'S_BUKRS'.
    wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
    wa_rsparams-sign = 'I'.
    wa_rsparams-option = 'EQ'.
    wa_rsparams-low    = s_bukrs_rep-low.
    wa_rsparams-high   = s_bukrs_rep-low.
    APPEND wa_rsparams TO it_rsparams.
  ENDIF.

*  WA_RSPARAMS-SELNAME = 'S_ZFBDT'.
*  WA_RSPARAMS-KIND = 'S'.  "SELECT OPTIONS TO BE PASSED
*  WA_RSPARAMS-SIGN = 'I'.
*  WA_RSPARAMS-OPTION = 'BT'.
*  WA_RSPARAMS-LOW    = SY-DATUM.
*  WA_RSPARAMS-HIGH   = VG_DT_HIGH.
*  APPEND WA_RSPARAMS TO IT_RSPARAMS.

  SUBMIT zfir0066 WITH SELECTION-TABLE it_rsparams
              AND RETURN.

  LEAVE TO SCREEN 0.

ENDFORM.

FORM atrib_vlr_dia USING p_dt_vcto_mov TYPE zfit0079-zfbdt
                         p_valor_mov   TYPE zfit0079-dmbtr
                         p_wa_saida    TYPE ty_saida.

  CASE p_dt_vcto_mov.
*---> 08/06/2023 - Migração S4 - JS
*         WHEN day_01_mov.
*             p_wa_saida-day_01 = p_valor_mov.
*         WHEN day_02_mov.
*             p_wa_saida-day_02 = p_valor_mov.
*         when DAY_03_MOV.
*             p_wa_saida-day_03 = p_valor_mov.
*         when DAY_04_MOV.
*             P_WA_SAIDA-DAY_04 = P_VALOR_MOV.
*         when DAY_05_MOV.
*             p_wa_saida-day_05 = p_valor_mov.
*         when DAY_06_MOV.
*             P_WA_SAIDA-DAY_06 = P_VALOR_MOV.
*         when DAY_07_MOV.
*             P_WA_SAIDA-DAY_07 = P_VALOR_MOV.
*         when DAY_08_MOV.
*             p_wa_saida-day_08 = p_valor_mov.
*         when DAY_09_MOV.
*             p_wa_saida-day_09 = p_valor_mov.
*         when DAY_10_MOV.
*             p_wa_saida-day_10 = p_valor_mov.
*         when DAY_11_MOV.
*             p_wa_saida-day_11 = p_valor_mov.
*         when DAY_12_MOV.
*             p_wa_saida-day_12 = p_valor_mov.
*         when DAY_13_MOV.
*             P_WA_SAIDA-DAY_13 = P_VALOR_MOV.
*         when DAY_14_MOV.
*             p_wa_saida-day_14 = p_valor_mov.
*         when DAY_15_MOV.
*             P_WA_SAIDA-DAY_15 = P_VALOR_MOV.
*         when DAY_16_MOV.
*             p_wa_saida-day_16 = p_valor_mov.
*         when DAY_17_MOV.
*             P_WA_SAIDA-DAY_17 = P_VALOR_MOV.
*         when DAY_18_MOV.
*             P_WA_SAIDA-DAY_18 = P_VALOR_MOV.
*         when DAY_19_MOV.
*             p_wa_saida-day_19 = p_valor_mov.
*         when DAY_20_MOV.
*             p_wa_saida-day_20 = p_valor_mov.
*         when DAY_21_MOV.
*             p_wa_saida-day_21 = p_valor_mov.
*         when DAY_22_MOV.
*             p_wa_saida-day_22 = p_valor_mov.
*         when DAY_23_MOV.
*             p_wa_saida-day_23 = p_valor_mov.
*         when DAY_24_MOV.
*             p_wa_saida-day_24 = p_valor_mov.
*         when DAY_25_MOV.
*             P_WA_SAIDA-DAY_25 = P_VALOR_MOV.
*         when DAY_26_MOV.
*             p_wa_saida-day_26 = p_valor_mov.
*         when DAY_27_MOV.
*             P_WA_SAIDA-DAY_27 = P_VALOR_MOV.
*         when DAY_28_MOV.
*             P_WA_SAIDA-DAY_28 = P_VALOR_MOV.
*         when DAY_29_MOV.
*             P_WA_SAIDA-DAY_29 = P_VALOR_MOV.
*         when DAY_30_MOV.
*             p_wa_saida-day_30 = p_valor_mov.
*         when DAY_31_MOV.
*             P_WA_SAIDA-DAY_31 = P_VALOR_MOV.
    WHEN day_01_mov.
      p_wa_saida-day_01 = CONV #( p_valor_mov ).
    WHEN day_02_mov.
      p_wa_saida-day_02 = CONV #( p_valor_mov ).
    WHEN day_03_mov.
      p_wa_saida-day_03 = CONV #( p_valor_mov ).
    WHEN day_04_mov.
      p_wa_saida-day_04 = CONV #( p_valor_mov ).
    WHEN day_05_mov.
      p_wa_saida-day_05 = CONV #( p_valor_mov ).
    WHEN day_06_mov.
      p_wa_saida-day_06 = CONV #( p_valor_mov ).
    WHEN day_07_mov.
      p_wa_saida-day_07 = CONV #( p_valor_mov ).
    WHEN day_08_mov.
      p_wa_saida-day_08 = CONV #( p_valor_mov ).
    WHEN day_09_mov.
      p_wa_saida-day_09 = CONV #( p_valor_mov ).
    WHEN day_10_mov.
      p_wa_saida-day_10 = CONV #( p_valor_mov ).
    WHEN day_11_mov.
      p_wa_saida-day_11 = CONV #( p_valor_mov ).
    WHEN day_12_mov.
      p_wa_saida-day_12 = CONV #( p_valor_mov ).
    WHEN day_13_mov.
      p_wa_saida-day_13 = CONV #( p_valor_mov ).
    WHEN day_14_mov.
      p_wa_saida-day_14 = CONV #( p_valor_mov ).
    WHEN day_15_mov.
      p_wa_saida-day_15 = CONV #( p_valor_mov ).
    WHEN day_16_mov.
      p_wa_saida-day_16 = CONV #( p_valor_mov ).
    WHEN day_17_mov.
      p_wa_saida-day_17 = CONV #( p_valor_mov ).
    WHEN day_18_mov.
      p_wa_saida-day_18 = CONV #( p_valor_mov ).
    WHEN day_19_mov.
      p_wa_saida-day_19 = CONV #( p_valor_mov ).
    WHEN day_20_mov.
      p_wa_saida-day_20 = CONV #( p_valor_mov ).
    WHEN day_21_mov.
      p_wa_saida-day_21 = CONV #( p_valor_mov ).
    WHEN day_22_mov.
      p_wa_saida-day_22 = CONV #( p_valor_mov ).
    WHEN day_23_mov.
      p_wa_saida-day_23 = CONV #( p_valor_mov ).
    WHEN day_24_mov.
      p_wa_saida-day_24 = CONV #( p_valor_mov ).
    WHEN day_25_mov.
      p_wa_saida-day_25 = CONV #( p_valor_mov ).
    WHEN day_26_mov.
      p_wa_saida-day_26 = CONV #( p_valor_mov ).
    WHEN day_27_mov.
      p_wa_saida-day_27 = CONV #( p_valor_mov ).
    WHEN day_28_mov.
      p_wa_saida-day_28 = CONV #( p_valor_mov ).
    WHEN day_29_mov.
      p_wa_saida-day_29 = CONV #( p_valor_mov ).
    WHEN day_30_mov.
      p_wa_saida-day_30 = CONV #( p_valor_mov ).
    WHEN day_31_mov.
      p_wa_saida-day_31 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS
  ENDCASE.

ENDFORM.


FORM atrib_vlr_dia_ajuste USING p_dt_vcto_mov TYPE zfit0079-zfbdt
                                p_valor_mov   TYPE zfit0079-dmbtr
                                p_wa_saida    TYPE ty_saida.

  CASE p_dt_vcto_mov.
    WHEN day_01_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*              P_WA_SAIDA-DAY_01 = P_VALOR_MOV.
      p_wa_saida-day_01  = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS


      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_01'
                                         CHANGING p_wa_saida.

    WHEN day_02_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*            p_wa_saida-day_02 = p_valor_mov.
      p_wa_saida-day_02 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_02'
                                         CHANGING p_wa_saida.

    WHEN day_03_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*            p_wa_saida-day_03 = p_valor_mov.
      p_wa_saida-day_03 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_03'
                                         CHANGING p_wa_saida.

    WHEN day_04_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*            p_wa_saida-day_04 = p_valor_mov.
      p_wa_saida-day_04 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_04'
                                         CHANGING p_wa_saida.

    WHEN day_05_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*            p_wa_saida-day_05 = p_valor_mov.
      p_wa_saida-day_05 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_05'
                                         CHANGING p_wa_saida.

    WHEN day_06_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*            p_wa_saida-day_06 = p_valor_mov.
      p_wa_saida-day_06 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS


      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_06'
                                         CHANGING p_wa_saida.

    WHEN day_07_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*            P_WA_SAIDA-DAY_07 = P_VALOR_MOV.
      p_wa_saida-day_07 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS


      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_07'
                                         CHANGING p_wa_saida.

    WHEN day_08_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*             P_WA_SAIDA-DAY_08 = P_VALOR_MOV.
      p_wa_saida-day_08 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_08'
                                         CHANGING p_wa_saida.

    WHEN day_09_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*           p_wa_saida-day_09 = p_valor_mov.
      p_wa_saida-day_09 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS
      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_09'
                                         CHANGING p_wa_saida.

    WHEN day_10_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*           p_wa_saida-day_10 = p_valor_mov.
      p_wa_saida-day_10 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS


      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_10'
                                         CHANGING p_wa_saida.

    WHEN day_11_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_11 = p_valor_mov.
      p_wa_saida-day_11 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS


      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_11'
                                         CHANGING p_wa_saida.

    WHEN day_12_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_12 = p_valor_mov.
      p_wa_saida-day_12 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_12'
                                         CHANGING p_wa_saida.

    WHEN day_13_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*              p_wa_saida-day_13 = p_valor_mov.
      p_wa_saida-day_13 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_13'
                                         CHANGING p_wa_saida.

    WHEN day_14_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_14 = p_valor_mov.
      p_wa_saida-day_14 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_14'
                                         CHANGING p_wa_saida.

    WHEN day_15_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*            p_wa_saida-day_15 = p_valor_mov.
      p_wa_saida-day_15 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_15'
                                         CHANGING p_wa_saida.

    WHEN day_16_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*            p_wa_saida-day_16 = p_valor_mov.
      p_wa_saida-day_16 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_16'
                                         CHANGING p_wa_saida.

    WHEN day_17_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_17 = p_valor_mov.
      p_wa_saida-day_17 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_17'
                                         CHANGING p_wa_saida.

    WHEN day_18_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_18 = p_valor_mov.
      p_wa_saida-day_18 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_18'
                                         CHANGING p_wa_saida.

    WHEN day_19_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*           p_wa_saida-day_19 = p_valor_mov.
      p_wa_saida-day_19 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_19'
                                         CHANGING p_wa_saida.

    WHEN day_20_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*     p_wa_saida-day_20 = p_valor_mov.
      p_wa_saida-day_20 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_20'
                                         CHANGING p_wa_saida.

    WHEN day_21_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*            p_wa_saida-day_21 = p_valor_mov.
      p_wa_saida-day_21 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_21'
                                         CHANGING p_wa_saida.

    WHEN day_22_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_22 = p_valor_mov.
      p_wa_saida-day_22 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_22'
                                         CHANGING p_wa_saida.

    WHEN day_23_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_23 = p_valor_mov.
      p_wa_saida-day_23 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_23'
                                         CHANGING p_wa_saida.

    WHEN day_24_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*            p_wa_saida-day_24 = p_valor_mov.
      p_wa_saida-day_24 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_24'
                                         CHANGING p_wa_saida.

    WHEN day_25_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_25 = p_valor_mov.
      p_wa_saida-day_25 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_25'
                                         CHANGING p_wa_saida.

    WHEN day_26_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_26 = p_valor_mov.
      p_wa_saida-day_26 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_26'
                                         CHANGING p_wa_saida.

    WHEN day_27_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_27 = p_valor_mov.
      p_wa_saida-day_27 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_27'
                                         CHANGING p_wa_saida.

    WHEN day_28_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*           p_wa_saida-day_28 = p_valor_mov.
      p_wa_saida-day_28 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_28'
                                         CHANGING p_wa_saida.

    WHEN day_29_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_29 = p_valor_mov.
      p_wa_saida-day_29 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_29'
                                         CHANGING p_wa_saida.

    WHEN day_30_ajuste.
*---> 08/06/2023 - Migração S4 - JS
*            p_wa_saida-day_30 = p_valor_mov.
      p_wa_saida-day_30 = CONV #( p_valor_mov ).
*<--- 08/06/2023 - Migração S4 - JS


      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_30'
                                         CHANGING p_wa_saida.

    WHEN day_31_ajuste.
*---> 09/06/2023 - Migração S4 - JS
*           p_wa_saida-day_31 = p_valor_mov.
      p_wa_saida-day_31 = CONV #( p_valor_mov ).
*<--- 09/06/2023 - Migração S4 - JS

      PERFORM f_set_color_coment_dia_ajuste USING p_dt_vcto_mov
                                                  'DAY_31'
                                         CHANGING p_wa_saida.
  ENDCASE.

ENDFORM.


FORM f_get_dia_ajuste USING i_column_id
                   CHANGING p_dia_ajuste.

  CLEAR: p_dia_ajuste.

  CHECK i_column_id IS NOT INITIAL.

  CASE i_column_id.
    WHEN 'DAY_01'.
      p_dia_ajuste = day_01_ajuste.
    WHEN 'DAY_02'.
      p_dia_ajuste = day_02_ajuste.
    WHEN 'DAY_03'.
      p_dia_ajuste = day_03_ajuste.
    WHEN 'DAY_04'.
      p_dia_ajuste = day_04_ajuste.
    WHEN 'DAY_05'.
      p_dia_ajuste = day_05_ajuste.
    WHEN 'DAY_06'.
      p_dia_ajuste = day_06_ajuste.
    WHEN 'DAY_07'.
      p_dia_ajuste = day_07_ajuste.
    WHEN 'DAY_08'.
      p_dia_ajuste = day_08_ajuste.
    WHEN 'DAY_09'.
      p_dia_ajuste = day_09_ajuste.
    WHEN 'DAY_10'.
      p_dia_ajuste = day_10_ajuste.
    WHEN 'DAY_11'.
      p_dia_ajuste = day_11_ajuste.
    WHEN 'DAY_12'.
      p_dia_ajuste = day_12_ajuste.
    WHEN 'DAY_13'.
      p_dia_ajuste = day_13_ajuste.
    WHEN 'DAY_14'.
      p_dia_ajuste = day_14_ajuste.
    WHEN 'DAY_15'.
      p_dia_ajuste = day_15_ajuste.
    WHEN 'DAY_16'.
      p_dia_ajuste = day_16_ajuste.
    WHEN 'DAY_17'.
      p_dia_ajuste = day_17_ajuste.
    WHEN 'DAY_18'.
      p_dia_ajuste = day_18_ajuste.
    WHEN 'DAY_19'.
      p_dia_ajuste = day_19_ajuste.
    WHEN 'DAY_20'.
      p_dia_ajuste = day_20_ajuste.
    WHEN 'DAY_21'.
      p_dia_ajuste = day_21_ajuste.
    WHEN 'DAY_22'.
      p_dia_ajuste = day_22_ajuste.
    WHEN 'DAY_23'.
      p_dia_ajuste = day_23_ajuste.
    WHEN 'DAY_24'.
      p_dia_ajuste = day_24_ajuste.
    WHEN 'DAY_25'.
      p_dia_ajuste = day_25_ajuste.
    WHEN 'DAY_26'.
      p_dia_ajuste = day_26_ajuste.
    WHEN 'DAY_27'.
      p_dia_ajuste = day_27_ajuste.
    WHEN 'DAY_28'.
      p_dia_ajuste = day_28_ajuste.
    WHEN 'DAY_29'.
      p_dia_ajuste = day_29_ajuste.
    WHEN 'DAY_30'.
      p_dia_ajuste = day_30_ajuste.
    WHEN 'DAY_31'.
      p_dia_ajuste = day_31_ajuste.
  ENDCASE.


ENDFORM.


FORM criar_field_catalog_0107.

  FREE: wa_fcat, it_fcat.
  REFRESH: it_fcat.

  PERFORM estrutura_alv_ajuste USING:

      03  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_01'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      04  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_02'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      05  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_03'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      06  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_04'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      07  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_05'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      08  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_06'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      09  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_07'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      10  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_08'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      11  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_09'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      12  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_10'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      13  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_11'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      14  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_12'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      15  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_13'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      16  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_14'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      17  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_15'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      18  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_16'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      19  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_17'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      20  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_18'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      21  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_19'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      22  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_20'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      23  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_21'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      24  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_22'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      25  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_23'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      26  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_24'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      27  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_25'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      28  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_26'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      29  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_27'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      30  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_28'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      31  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_29'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      32  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_30'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' ,
      33  'ZFIT0111'  'DMBTR'   'IT_SAIDA' 'DAY_31'           'Dia 1'            '20'  'X'    '' ' ' 'R' '' .

ENDFORM.

FORM gravar_ajustes.

  DATA: var_answer TYPE c,
        vl_gdatu   TYPE gdatu_inv.

  DATA: vl_dt_ini_rep TYPE zfit0079-zfbdt,
        vl_error      TYPE c.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  CALL METHOD obj_alv_ajuste->check_changed_data.

  READ TABLE it_sai_ajuste INTO wa_sai_ajuste INDEX 1.

  CHECK sy-subrc EQ 0.

  CLEAR: vl_error, vl_dt_ini_rep.

* Busca Taxa Cambio ----------------------------------------------------*
  CREATE OBJECT obj_zcl_util_sd.

  MOVE sy-datum TO vl_gdatu.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('USD').
  obj_zcl_util_sd->set_tcurr('BRL').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_usd_brl = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_usd_brl = 0.
    MESSAGE 'Taxa de Câmbio não encontrada(USD/BRL)!' TYPE 'S'.
    EXIT.
  ENDIF.

  vg_tx_usd_brl = abs( vg_tx_usd_brl ).

  FREE obj_zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('USD').
  obj_zcl_util_sd->set_tcurr('ARS').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_usd_ars = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_usd_ars = 0.
    MESSAGE 'Taxa de Câmbio não encontrada(USD/ARS)!' TYPE 'S'.
    EXIT.
  ENDIF.

  FREE: obj_zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  obj_zcl_util_sd->set_kurst('EURX').
  obj_zcl_util_sd->set_waerk('EUR').
  obj_zcl_util_sd->set_tcurr('BRL').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_eur_brl = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_eur_brl = 0.
    MESSAGE 'Taxa de Câmbio não encontrada(EUR/BRL)!' TYPE 'S'.
    EXIT.
  ENDIF.

  FREE: obj_zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('EUR').
  obj_zcl_util_sd->set_tcurr('USD').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_eur_usd = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_eur_usd = 0.
    MESSAGE 'Taxa de Câmbio não encontrada(EUR/USD)!' TYPE 'S'.
    EXIT.
  ENDIF.

  LOOP AT tg_days_mov_ajuste.

    CASE tg_days_mov_ajuste-coluna..
      WHEN 'DAY_01'.
        IF wa_sai_ajuste-day_01 <>  wa_sai_ajuste_copy-day_01.

*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_01    TYPE zfit0079-dmbtr.

          vlr_day_01 = CONV #( wa_sai_ajuste-day_01 ).
*<--- 26/05/2023 - Migração S4 - JS

          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_01
                                          vlr_day_01
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_02'.
        IF wa_sai_ajuste-day_02 <>  wa_sai_ajuste_copy-day_02.

*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_02    TYPE zfit0079-dmbtr.

          vlr_day_02 = CONV #( wa_sai_ajuste-day_02 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_02
                                          vlr_day_02
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_03'.
        IF wa_sai_ajuste-day_03 <>  wa_sai_ajuste_copy-day_03.

*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_03 TYPE zfit0079-dmbtr.
          vlr_day_03 = CONV #( wa_sai_ajuste-day_03 ).
*<--- 26/05/2023 - Migração S4 - JS

          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_03
                                          vlr_day_03
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.

        ENDIF.
      WHEN 'DAY_04'.
        IF wa_sai_ajuste-day_04 <>  wa_sai_ajuste_copy-day_04.

*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_04 TYPE zfit0079-dmbtr.
          vlr_day_04 = CONV #( wa_sai_ajuste-day_04 ).
*<--- 26/05/2023 - Migração S4 - JS

          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_04
                                          vlr_day_04
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_05'.
        IF wa_sai_ajuste-day_05 <>  wa_sai_ajuste_copy-day_05.

*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_05 TYPE zfit0079-dmbtr.
          vlr_day_05 = CONV #( wa_sai_ajuste-day_05 ).
*<--- 26/05/2023 - Migração S4 - JS

          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_05
                                          vlr_day_05
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_06'.
        IF wa_sai_ajuste-day_06 <>  wa_sai_ajuste_copy-day_06.

*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_06 TYPE zfit0079-dmbtr.
          vlr_day_06     = CONV #( wa_sai_ajuste-day_06 ).
*<--- 26/05/2023 - Migração S4 - JS

          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_06
                                          vlr_day_06
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_07'.
        IF wa_sai_ajuste-day_07 <>  wa_sai_ajuste_copy-day_07.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_07 TYPE zfit0079-dmbtr.
          vlr_day_07 = CONV #( wa_sai_ajuste-day_07 ).
*<--- 26/05/2023 - Migração S4 - JS

          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_07
                                          vlr_day_07
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_08'.
        IF wa_sai_ajuste-day_08 <>  wa_sai_ajuste_copy-day_08.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_08 TYPE zfit0079-dmbtr.
          vlr_day_08 = CONV #( wa_sai_ajuste-day_08 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_08
                                          vlr_day_08
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_09'.
        IF wa_sai_ajuste-day_09 <>  wa_sai_ajuste_copy-day_09.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_09 TYPE zfit0079-dmbtr.
          vlr_day_09 = CONV #( wa_sai_ajuste-day_09 ).
*<--- 26/05/2023 - Migração S4 - JS

          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_09
                                          vlr_day_09
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_10'.
        IF wa_sai_ajuste-day_10 <>  wa_sai_ajuste_copy-day_10.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_10 TYPE zfit0079-dmbtr.
          vlr_day_10 = CONV #( wa_sai_ajuste-day_10 ).
*<--- 26/05/2023 - Migração S4 - JS

          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_10
                                          vlr_day_10
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_11'.
        IF wa_sai_ajuste-day_11 <>  wa_sai_ajuste_copy-day_11.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_11 TYPE zfit0079-dmbtr.
          vlr_day_11 = CONV #( wa_sai_ajuste-day_11 ).
*<--- 26/05/2023 - Migração S4 - JS

          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_11
                                          vlr_day_11
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_12'.
        IF wa_sai_ajuste-day_12 <>  wa_sai_ajuste_copy-day_12.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_12 TYPE zfit0079-dmbtr.
          vlr_day_12 = CONV #( wa_sai_ajuste-day_12 ).
*<--- 26/05/2023 - Migração S4 - JS

          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_12
                                          vlr_day_12
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_13'.
        IF wa_sai_ajuste-day_13 <>  wa_sai_ajuste_copy-day_13.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_13 TYPE zfit0079-dmbtr.
          vlr_day_13 = CONV #( wa_sai_ajuste-day_13 ).
*<--- 26/05/2023 - Migração S4 - JS

          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_13
                                          vlr_day_13
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_14'.
        IF wa_sai_ajuste-day_14 <>  wa_sai_ajuste_copy-day_14.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_14 TYPE zfit0079-dmbtr.
          vlr_day_14 =     CONV #( wa_sai_ajuste-day_14 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_14
                                          vlr_day_14
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_15'.
        IF wa_sai_ajuste-day_15 <>  wa_sai_ajuste_copy-day_15.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_15 TYPE zfit0079-dmbtr.
          vlr_day_15 =     CONV #( wa_sai_ajuste-day_15 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_15
                                          vlr_day_15
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_16'.
        IF wa_sai_ajuste-day_16 <>  wa_sai_ajuste_copy-day_16.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_16 TYPE zfit0079-dmbtr.
          vlr_day_16 =     CONV #( wa_sai_ajuste-day_16 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_16
                                          vlr_day_16
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_17'.
        IF wa_sai_ajuste-day_17 <>  wa_sai_ajuste_copy-day_17.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_17 TYPE zfit0079-dmbtr.
          vlr_day_17 =     CONV #( wa_sai_ajuste-day_17 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_17
                                          vlr_day_17
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_18'.
        IF wa_sai_ajuste-day_18 <>  wa_sai_ajuste_copy-day_18.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_18 TYPE zfit0079-dmbtr.
          vlr_day_18 =     CONV #( wa_sai_ajuste-day_18 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_18
                                          vlr_day_18
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_19'.
        IF wa_sai_ajuste-day_19 <>  wa_sai_ajuste_copy-day_19.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_19 TYPE zfit0079-dmbtr.
          vlr_day_19 =     CONV #( wa_sai_ajuste-day_19 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_19
                                          vlr_day_19
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_20'.
        IF wa_sai_ajuste-day_20 <>  wa_sai_ajuste_copy-day_20.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_20 TYPE zfit0079-dmbtr.
          vlr_day_20 =     CONV #( wa_sai_ajuste-day_20 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_20
                                          vlr_day_20
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_21'.
        IF wa_sai_ajuste-day_21 <>  wa_sai_ajuste_copy-day_21.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_21 TYPE zfit0079-dmbtr.
          vlr_day_21 =     CONV #( wa_sai_ajuste-day_21 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_21
                                          vlr_day_21
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_22'.
        IF wa_sai_ajuste-day_22 <>  wa_sai_ajuste_copy-day_22.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_22 TYPE zfit0079-dmbtr.
          vlr_day_22 =     CONV #( wa_sai_ajuste-day_22 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_22
                                          vlr_day_22
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_23'.
        IF wa_sai_ajuste-day_23 <>  wa_sai_ajuste_copy-day_23.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_23 TYPE zfit0079-dmbtr.
          vlr_day_23 =     CONV #( wa_sai_ajuste-day_23 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_23
                                          vlr_day_23
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_24'.
        IF wa_sai_ajuste-day_24 <>  wa_sai_ajuste_copy-day_24.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_24 TYPE zfit0079-dmbtr.
          vlr_day_24 =     CONV #( wa_sai_ajuste-day_24 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_24
                                          vlr_day_24
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_25'.
        IF wa_sai_ajuste-day_25 <>  wa_sai_ajuste_copy-day_25.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_25 TYPE zfit0079-dmbtr.
          vlr_day_25 =     CONV #( wa_sai_ajuste-day_25 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_25
                                          vlr_day_25
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_26'.
        IF wa_sai_ajuste-day_26 <>  wa_sai_ajuste_copy-day_26.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_26 TYPE zfit0079-dmbtr.
          vlr_day_26 =     CONV #( wa_sai_ajuste-day_26 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_26
                                          vlr_day_26
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_27'.
        IF wa_sai_ajuste-day_27 <>  wa_sai_ajuste_copy-day_27.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_27 TYPE zfit0079-dmbtr.
          vlr_day_27 =     CONV #( wa_sai_ajuste-day_27 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_27
                                          vlr_day_27
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_28'.
        IF wa_sai_ajuste-day_28 <>  wa_sai_ajuste_copy-day_28.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_28 TYPE zfit0079-dmbtr.
          vlr_day_28 =     CONV #( wa_sai_ajuste-day_28 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_28
                                          vlr_day_28
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_29'.
        IF wa_sai_ajuste-day_29 <>  wa_sai_ajuste_copy-day_29.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_29 TYPE zfit0079-dmbtr.
          vlr_day_29 =     CONV #( wa_sai_ajuste-day_29 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_29
                                          vlr_day_29
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_30'.
        IF wa_sai_ajuste-day_30 <>  wa_sai_ajuste_copy-day_30.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_30 TYPE zfit0079-dmbtr.
          vlr_day_30 =     CONV #( wa_sai_ajuste-day_30 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_30
                                          vlr_day_30
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.
      WHEN 'DAY_31'.
        IF wa_sai_ajuste-day_31 <>  wa_sai_ajuste_copy-day_31.
*---> 26/05/2023 - Migração S4 - JS
          DATA: vlr_day_31 TYPE zfit0079-dmbtr.
          vlr_day_31 =     CONV #( wa_sai_ajuste-day_31 ).
*<--- 26/05/2023 - Migração S4 - JS
          PERFORM gravar_mov_ajuste USING
*---> 26/05/2023 - Migração S4 - JS
                                         "wa_sai_ajuste-day_31
                                          vlr_day_31
*<--- 26/05/2023 - Migração S4 - JS
                                          tg_days_mov_ajuste-dt_vcto
                                          wa_sai_ajuste
                                 CHANGING vl_dt_ini_rep
                                          vl_error.
        ENDIF.

    ENDCASE.

    IF vl_error IS NOT INITIAL.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

  ENDLOOP.

  CALL FUNCTION 'ZFI_PROC_RESUMO_FLX'
    EXPORTING
      i_data_ini       = vl_dt_ini_rep
      i_bukrs          = wa_sai_ajuste-bukrs
      i_dt_base_versao = wa_sai_ajuste-dt_versao
      i_hora_versao    = wa_sai_ajuste-hora_versao
      i_versao         = wa_sai_ajuste-versao
      i_ref_saldo      = 'X'
    EXCEPTIONS
      m_error          = 1
      OTHERS           = 2.

  IF sy-subrc NE 0.
    MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'S'.
    ROLLBACK WORK.
    RETURN.
  ENDIF.

  COMMIT WORK.
  MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.

  LEAVE TO SCREEN 0.

ENDFORM.

FORM gravar_mov_ajuste USING p_valor         TYPE zfit0079-dmbtr
                             p_dt_vcto       TYPE zfit0079-zfbdt
                             p_wa_sai_ajuste TYPE ty_saida
                    CHANGING p_dt_ini_rep    TYPE zfit0079-zfbdt "Data Inicial de Reprocessamento
                             p_error         TYPE c.

  DATA: it_0079_aux     TYPE TABLE OF zfit0079,
        wa_0079_aux     TYPE zfit0079,
        wa_0115_aux     TYPE zfit0115,
        vl_valor_ajuste TYPE zfit0079-dmbtr,
        vl_clas_flx     TYPE zfit0079-clas_flx.

  CLEAR: vl_valor_ajuste, vl_clas_flx.

  vl_valor_ajuste = p_valor.

  vl_clas_flx = 'E'.
  IF vl_valor_ajuste < 0.
    vl_clas_flx = 'S'.
    vl_valor_ajuste = abs( vl_valor_ajuste ).
  ENDIF.

  IF p_dt_ini_rep IS INITIAL.
    p_dt_ini_rep = p_dt_vcto.
  ENDIF.

  IF p_dt_vcto  < p_dt_ini_rep.
    p_dt_ini_rep = p_dt_vcto.
  ENDIF.

*  REFRESH IT_0079_AUX.
*  SELECT *
*    INTO TABLE IT_0079_AUX
*    FROM ZFIT0079
*   WHERE BUKRS          = P_WA_SAI_AJUSTE-BUKRS
*     AND CODIGO         = P_WA_SAI_AJUSTE-CODIGO
*     AND ZFBDT          = P_DT_VCTO
*     AND DT_BASE_VERSAO = P_WA_SAI_AJUSTE-DT_VERSAO
*     AND VERSAO         = P_WA_SAI_AJUSTE-VERSAO.
*
*  IF ( IT_0079_AUX[] IS NOT INITIAL ) AND ( LINES( IT_0079_AUX ) > 1 ).
*
*    DELETE FROM ZFIT0079 WHERE BUKRS          = P_WA_SAI_AJUSTE-BUKRS
*                           AND CODIGO         = P_WA_SAI_AJUSTE-CODIGO
*                           AND ZFBDT          = P_DT_VCTO
*                           AND DT_BASE_VERSAO = P_WA_SAI_AJUSTE-DT_VERSAO
*                           AND VERSAO         = P_WA_SAI_AJUSTE-VERSAO.
*  ENDIF.

  DELETE FROM zfit0079 WHERE bukrs          = p_wa_sai_ajuste-bukrs
                         AND zfbdt          = p_dt_vcto
                         AND dt_base_versao = p_wa_sai_ajuste-dt_versao
                         AND versao         = p_wa_sai_ajuste-versao
                         AND codigo         = p_wa_sai_ajuste-codigo.

  DELETE FROM zfit0115 WHERE bukrs          = p_wa_sai_ajuste-bukrs
                         AND codigo_flx     = p_wa_sai_ajuste-codigo
                         AND dt_vcto        = p_dt_vcto.

*  CLEAR WA_0079_AUX.
*  SELECT SINGLE *
*    INTO WA_0079_AUX
*    FROM ZFIT0079
*   WHERE BUKRS          = P_WA_SAI_AJUSTE-BUKRS
*     AND CODIGO         = P_WA_SAI_AJUSTE-CODIGO
*     AND ZFBDT          = P_DT_VCTO
*     AND DT_BASE_VERSAO = P_WA_SAI_AJUSTE-DT_VERSAO
*     AND VERSAO         = P_WA_SAI_AJUSTE-VERSAO.
*
*  IF SY-SUBRC = 0. "Modifica Registro Previsão
*
*    IF 'BRL' IN S_WAERS.
*      MOVE VL_VALOR_AJUSTE TO WA_0079_AUX-DMBTR.
*      WA_0079_AUX-DMBE2 = WA_0079_AUX-DMBTR / VG_TX_CAMBIO.
*    ELSE.
*      MOVE VL_VALOR_AJUSTE TO WA_0079_AUX-DMBE2.
*      WA_0079_AUX-DMBTR = WA_0079_AUX-DMBE2 * VG_TX_CAMBIO.
*    ENDIF.
*
**    IF ( WA_0079_AUX-DMBTR = 0 ) OR ( WA_0079_AUX-DMBE2 = 0 ).
**      P_ERROR = 'X'.
**      ROLLBACK WORK.
**      MESSAGE 'Houve um erro ao gerar os valores da Previsão!' TYPE 'S'.
**      RETURN.
**    ENDIF.
*
*    WA_0079_AUX-CLAS_FLX = VL_CLAS_FLX.
*    WA_0079_AUX-US_PROC  = SY-UNAME.
*    WA_0079_AUX-DT_ATUAL = SY-DATUM.
*    WA_0079_AUX-HR_ATUAL = SY-UZEIT.
*
*    MODIFY ZFIT0079 FROM WA_0079_AUX.
*
*    IF ( SY-SUBRC NE 0 ).
*      P_ERROR = 'X'.
*      ROLLBACK WORK.
*      MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'S'.
*      RETURN.
*    ENDIF.
*
*    SELECT SINGLE *
*      INTO WA_0115_AUX
*      FROM ZFIT0115
*     WHERE CD_PREV = WA_0079_AUX-CD_PREV.
*
*    IF ( SY-SUBRC NE 0 ).
*      P_ERROR = 'X'.
*      ROLLBACK WORK.
*      MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'S'.
*      RETURN.
*    ENDIF.
*
*    MOVE: WA_0079_AUX-CLAS_FLX   TO WA_0115_AUX-CLAS_FLX,
*          WA_0079_AUX-ZFBDT      TO WA_0115_AUX-DT_VCTO,
*          WA_0079_AUX-DMBTR      TO WA_0115_AUX-DMBTR,
*          WA_0079_AUX-DMBE2      TO WA_0115_AUX-DMBE2,
*          SY-UNAME               TO WA_0115_AUX-USNAM,
*          SY-DATUM               TO WA_0115_AUX-DT_ATUAL,
*          SY-UZEIT               TO WA_0115_AUX-HR_ATUAL.
*
*    MODIFY ZFIT0115 FROM WA_0115_AUX.
*
*    IF ( SY-SUBRC NE 0 ).
*      P_ERROR = 'X'.
*      ROLLBACK WORK.
*      MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'S'.
*      RETURN.
*    ENDIF.


  "ELSE.

  "Incluir novo Registro de Previsão

  CLEAR: wa_0079_aux.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZFLX_PREV'
    IMPORTING
      number      = wa_0079_aux-cd_prev.

  IF ( sy-subrc <> 0 ) OR ( wa_0079_aux-cd_prev IS INITIAL ).
    p_error = 'X'.
    ROLLBACK WORK.
    MESSAGE 'Houve um erro ao gerar o Numero da Previsão!' TYPE 'S'.
    RETURN.
  ENDIF.

  "Ini CS2017001994
  CALL FUNCTION 'ZFI_CONV_MOEDA_FLX'
    EXPORTING
      i_bukrs         = p_wa_sai_ajuste-bukrs
      i_waers_doc     = vg_waers
      i_dmbtr         = vl_valor_ajuste
      i_dmbe2         = vl_valor_ajuste
      i_tx_usd_brl    = vg_tx_usd_brl
      i_tx_usd_ars    = vg_tx_usd_ars
      i_tx_eur_brl    = vg_tx_eur_brl
      i_tx_eur_usd    = vg_tx_eur_usd
    CHANGING
      c_dmbtr         = wa_0079_aux-dmbtr
      c_dmbe2         = wa_0079_aux-dmbe2
    EXCEPTIONS
      data_inconplete = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    p_error = 'X'.
    ROLLBACK WORK.
    RETURN.
  ENDIF.
  "Fim CS2017001994

*    IF ( WA_0079_AUX-DMBTR = 0 ) OR ( WA_0079_AUX-DMBE2 = 0 ).
*      P_ERROR = 'X'.
*      ROLLBACK WORK.
*      MESSAGE 'Houve um erro ao gerar os valores da Previsão!' TYPE 'S'.
*      RETURN.
*    ENDIF.

  MOVE: p_wa_sai_ajuste-bukrs         TO wa_0079_aux-bukrs,
        "WA_0115-DEP_RESP           TO WA_0079_AUX-DEP_RESP,
        p_wa_sai_ajuste-codigo        TO wa_0079_aux-codigo,
        p_wa_sai_ajuste-tp_prev       TO wa_0079_aux-tp_prev,
        vl_clas_flx                   TO wa_0079_aux-clas_flx,
        p_wa_sai_ajuste-st_calc_sdo   TO wa_0079_aux-st_calc_sdo,
        p_dt_vcto                     TO wa_0079_aux-zfbdt,
        vg_waers                      TO wa_0079_aux-waers,
        sy-uname                      TO wa_0079_aux-us_proc,
        sy-uname                      TO wa_0079_aux-usnam,
        p_wa_sai_ajuste-dt_versao     TO wa_0079_aux-dt_base_versao,
        p_wa_sai_ajuste-versao        TO wa_0079_aux-versao,
        p_wa_sai_ajuste-hora_versao   TO wa_0079_aux-hora_versao,
        sy-datum                      TO wa_0079_aux-dt_atual,
        sy-uzeit                      TO wa_0079_aux-hr_atual.

  IF p_inter IS NOT INITIAL.
    wa_0079_aux-cx_internacional = abap_true.
  ENDIF.

  MODIFY zfit0079 FROM wa_0079_aux.

  IF ( sy-subrc NE 0 ).
    p_error = 'X'.
    ROLLBACK WORK.
    MESSAGE 'Houve um erro ao gravar os registros!' TYPE 'S'.
    RETURN.
  ENDIF.

  MOVE: wa_0079_aux-bukrs        TO wa_0115_aux-bukrs,
        wa_0079_aux-dep_resp     TO wa_0115_aux-dep_resp,
        wa_0079_aux-codigo       TO wa_0115_aux-codigo_flx,
        wa_0079_aux-cd_prev      TO wa_0115_aux-cd_prev,
        wa_0079_aux-tp_prev      TO wa_0115_aux-tp_prev,
        wa_0079_aux-clas_flx     TO wa_0115_aux-clas_flx,
        wa_0079_aux-zfbdt        TO wa_0115_aux-dt_vcto,
        wa_0079_aux-waers        TO wa_0115_aux-waers,
        wa_0079_aux-zfbdt        TO wa_0115_aux-dt_vcto,
        wa_0079_aux-dmbtr        TO wa_0115_aux-dmbtr,
        wa_0079_aux-dmbe2        TO wa_0115_aux-dmbe2,
        sy-uname                 TO wa_0115_aux-usnam,
        sy-datum                 TO wa_0115_aux-dt_atual,
        sy-uzeit                 TO wa_0115_aux-hr_atual.

  IF p_inter IS NOT INITIAL.
    wa_0115_aux-cx_internacional = abap_true.
  ENDIF.

  MODIFY zfit0115 FROM wa_0115_aux.

  IF ( sy-subrc NE 0 ).
    p_error = 'X'.
    ROLLBACK WORK.
    MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'S'.
    RETURN.
  ENDIF.

  "ENDIF.



ENDFORM.

FORM add_pgto_bloq_lcto  USING    p_saida      TYPE ty_saida
                                  p_relat_key  TYPE lvc_nkey.

  DATA: vl_lvc_nkey  TYPE lvc_nkey,
        vl_node_text TYPE lvc_value,
        wl_saida_aux TYPE ty_saida.

  LOOP AT it_scalc_saldo INTO wl_saida_aux WHERE bukrs = p_saida-bukrs
                                             AND pgto_bloq IS NOT INITIAL.

    wl_saida_aux-nivel = 'X'.

    CONCATENATE wl_saida_aux-codigo '-' wl_saida_aux-descricao
           INTO vl_node_text SEPARATED BY space.

    PERFORM add_line_tree USING p_relat_key
                                vl_node_text
                                ''
                                wl_saida_aux
                                ''
                       CHANGING vl_lvc_nkey.

  ENDLOOP.


ENDFORM.


FORM get_last_versao USING p_bukrs p_msg
                  CHANGING p_dt_versao
                           p_hr_versao
                           p_versao.

  DATA: vl_msg_1      TYPE string,
        vl_msg_2      TYPE string,
        vl_msg_exibir TYPE string.

  CLEAR: p_dt_versao, p_hr_versao, p_versao.

  SELECT MAX( dt_base_versao )
    INTO (p_dt_versao)
    FROM zfit0111
   WHERE bukrs = p_bukrs
     AND cx_internacional = p_inter.

  IF ( sy-subrc NE 0 ) OR
     ( p_dt_versao IS INITIAL ).

    IF p_msg IS NOT INITIAL.
      vl_msg_1 = p_bukrs.
      CONCATENATE 'Nenhuma versão de Processamento encontrada para a Empresa('vl_msg_1')!'
           INTO vl_msg_exibir SEPARATED BY space.
      MESSAGE vl_msg_exibir TYPE 'S'.
    ENDIF.

    RETURN.
  ENDIF.

  SELECT MAX( versao )
    INTO (p_versao)
    FROM zfit0111
   WHERE bukrs          = p_bukrs
     AND dt_base_versao = p_dt_versao
     AND cx_internacional = p_inter.

  IF ( sy-subrc NE 0 ) OR
     ( p_versao    IS INITIAL ).

    IF p_msg IS NOT INITIAL.
      vl_msg_1 = p_bukrs.
      CONCATENATE 'Nenhuma versão de Processamento encontrada para a Empresa('vl_msg_1')!'
             INTO vl_msg_exibir SEPARATED BY space.
      MESSAGE vl_msg_exibir TYPE 'S'.
    ENDIF.

    RETURN.
  ENDIF.

  SELECT SINGLE hora_versao
    INTO p_hr_versao
    FROM zfit0111
   WHERE bukrs          = p_bukrs
     AND dt_base_versao = p_dt_versao
     AND versao         = p_versao
     AND cx_internacional = p_inter.

  IF ( sy-subrc NE 0 ) OR
     ( p_hr_versao  IS INITIAL ).

    IF p_msg IS NOT INITIAL.
      vl_msg_1 = p_bukrs.
      CONCATENATE 'Nenhuma versão de Processamento encontrada para a Empresa('vl_msg_1')!'
             INTO vl_msg_exibir SEPARATED BY space.
      MESSAGE vl_msg_exibir TYPE 'S'.
    ENDIF.

    RETURN.
  ENDIF.

  IF ( p_dt_versao IS INITIAL ) OR
     ( p_hr_versao IS INITIAL ) OR
     ( p_versao    IS INITIAL ).
    CLEAR: p_dt_versao, p_hr_versao, p_versao.
  ENDIF.

ENDFORM.

FORM remove_colunas_null USING p_var.

  DATA: wa_saida_var_aux TYPE ty_saida_var,
        it_saida_var_aux TYPE TABLE OF ty_saida_var.

  CASE p_var.
    WHEN '1'.
      it_saida_var_aux[] = it_saida_var1[].
    WHEN '2'.
      it_saida_var_aux[] = it_saida_var2[].
  ENDCASE.

  LOOP AT it_fcat INTO wa_fcat.

    wa_fcat-no_out = 'X'.

    CASE wa_fcat-fieldname.
      WHEN 'BUKRS'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE bukrs IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'LIFNR'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE lifnr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'KUNNR'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE kunnr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'NAME1'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE name1 IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'CODIGO'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE codigo IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DESC_FLX'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE desc_flx IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'CLAS_FLX'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE clas_flx IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'TP_PREV'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE tp_prev IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BLART'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE blart IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BSART'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE bsart IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'AUART'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE auart IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'ZLSPR'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE zlspr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'ZLSCH'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE zlsch IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'HBKID'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE hbkid IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'SGTXT'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE sgtxt IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'SGTXT2'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE sgtxt2 IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DEP_RESP'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE dep_resp IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BUDAT'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE budat IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'ZFBDT'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE zfbdt IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'HKONT'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE hkont IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'TXT50'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE txt50 IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'PLANILHA'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE planilha IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'PLANILHA_ITM'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE planilha_itm IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'ID_INVOICE'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE id_invoice IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DS_PORTO'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE ds_porto IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'TRADE_ID'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE trade_id IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DOC_IMPOSTO'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE doc_imposto IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'SEQITEM'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE seqitem IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'OBJ_KEY_PREV'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE obj_key_prev IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'CD_PREV'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE cd_prev IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BELNR'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE belnr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BLDAT'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE bldat IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'AUGBL'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE augbl IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'AUGDT'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE augdt IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'WAERS'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE waers IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DMBTR'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE dmbtr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DMBE2'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE dmbe2 IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'XBLNR'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE xblnr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'NRO_SOL'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE nro_sol IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'EBELN'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE ebeln IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'EBELP'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE ebelp IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BSCHL'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE bschl IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'OPR_NUMERO'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE opr_numero IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'CON_CODIGO'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE con_codigo IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'MDO_CODIGO'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE mdo_codigo IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'PAR_TIPO'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE par_tipo IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'MDO_TIPO'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE mdo_tipo IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'BUKRS_OPR'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE bukrs_opr IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'AGENTE'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE agente IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'REGRA_VAL'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE regra_val IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'PROCESSO_ESP'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE processo_esp IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'SISTEMA_ORIG'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE sistema_orig IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'USNAM'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE usnam IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'US_PROC'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE us_proc IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'DT_ATUAL'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE dt_atual IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
      WHEN 'HR_ATUAL'.
        LOOP AT it_saida_var_aux INTO wa_saida_var_aux WHERE hr_atual IS NOT INITIAL.
          wa_fcat-no_out = space.
          EXIT.
        ENDLOOP.
    ENDCASE.

    MODIFY it_fcat FROM wa_fcat.

  ENDLOOP.


ENDFORM.

FORM aplicar_ajustes_sobra_cxa  USING  p_saida         TYPE ty_saida
                                       p_tot_sld_aplic TYPE zfit0113-dmbtr.

  DATA: BEGIN OF tg_0111_resumo OCCURS 0,
          bukrs   TYPE zfit0111-bukrs,
          dt_vcto TYPE zfit0111-dt_vcto,
        END OF tg_0111_resumo.

  DATA: BEGIN OF tg_0111_det OCCURS 0,
          bukrs    TYPE zfit0111-bukrs,
          dt_vcto  TYPE zfit0111-dt_vcto,
          codigo   TYPE zfit0111-codigo,
          clas_flx TYPE zfit0111-clas_flx,
          dmbtr    TYPE zfit0111-dmbtr,
          dmbe2    TYPE zfit0111-dmbe2,
        END OF tg_0111_det.


  "Seleciona Lançamentos Resumo por Empresa/Vcto Ajustes Tesouraria
  SELECT bukrs dt_vcto
    INTO TABLE tg_0111_resumo
    FROM zfit0111 AS a
   WHERE bukrs          EQ p_saida-bukrs
     AND dt_vcto        IN s_zfbdt
     AND dt_base_versao EQ p_saida-dt_versao
     AND versao         EQ p_saida-versao
     AND tp_prev        EQ 'T'
     AND cx_internacional EQ p_inter
   GROUP BY bukrs dt_vcto.

*  "Seleciona Lançamentos Ajustes Tesouraria
  SELECT bukrs dt_vcto codigo clas_flx dmbtr dmbe2
    INTO TABLE tg_0111_det
    FROM zfit0111 AS a
   WHERE bukrs            EQ p_saida-bukrs
     AND dt_vcto          IN s_zfbdt
     AND dt_base_versao   EQ p_saida-dt_versao
     AND versao           EQ p_saida-versao
     AND tp_prev          EQ 'T'
     AND cx_internacional EQ p_inter.

  SORT tg_0111_resumo BY dt_vcto.
  LOOP AT tg_0111_resumo.

    LOOP AT tg_0111_det WHERE dt_vcto = tg_0111_resumo-dt_vcto.

      READ TABLE tg_0109 WITH KEY codigo = tg_0111_det-codigo.
      CHECK ( sy-subrc EQ 0 ) AND ( tg_0109-opr_sobra_cxa IS NOT INITIAL ).

      IF vg_moeda_int IS NOT INITIAL.
        IF tg_0109-opr_sobra_cxa = 'C'. "Creditar
          p_tot_sld_aplic = p_tot_sld_aplic + abs( tg_0111_det-dmbtr ).
        ELSE.
          p_tot_sld_aplic = p_tot_sld_aplic - abs( tg_0111_det-dmbtr ).
        ENDIF.
      ELSE.
        IF tg_0109-opr_sobra_cxa = 'C'. "Creditar
          p_tot_sld_aplic = p_tot_sld_aplic + abs( tg_0111_det-dmbe2 ).
        ELSE.
          p_tot_sld_aplic = p_tot_sld_aplic - abs( tg_0111_det-dmbe2 ).
        ENDIF.
      ENDIF.

    ENDLOOP.

    PERFORM atrib_vlr_dia USING tg_0111_resumo-dt_vcto
                                p_tot_sld_aplic
                                p_saida.
  ENDLOOP.



ENDFORM.

FORM f_set_color_coment_dia_ajuste USING p_data_ajuste  TYPE zfit0149-data
                                         p_dia_ajuste
                                CHANGING p_saida        TYPE ty_saida.

  DATA: wl_color  TYPE kkblo_specialcol.

  CLEAR: wl_color.
  wl_color-color-col = 1.
  wl_color-color-int = 0.
  wl_color-color-inv = 0.
  wl_color-fieldname = p_dia_ajuste.

  CHECK p_saida-tp_prev EQ 'T'.

  SELECT SINGLE *
    FROM zfit0149 INTO @DATA(_wl_0149)
   WHERE bukrs  EQ @p_saida-bukrs
     AND codigo EQ @p_saida-codigo
     AND data   EQ @p_data_ajuste.

  IF ( sy-subrc EQ 0 ) AND ( _wl_0149-comentario IS NOT INITIAL ).
    APPEND wl_color TO p_saida-color.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LISTA_LOG_VERSOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lista_log_versoes .

  DATA:
    l_returncode TYPE string,
    t_fields     TYPE STANDARD TABLE OF sval,
    w_fields     LIKE LINE OF t_fields.


  DATA(l_no_value_check) = 'some text here'.

  DATA(l_popup_title) = 'Listar Versões'.

  DATA(l_start_column) = '02'.

  DATA(l_start_row) = '02'.

  w_fields-tabname     ='T001'.
  w_fields-fieldname   ='BUKRS'.
  w_fields-value       =''.
  w_fields-field_attr  =''.
  w_fields-field_obl   ='X'.
*  w_fields-comp_code   =''.
*  w_fields-fieldtext   =''.
*  w_fields-comp_tab    =''.
*  w_fields-comp_field  =''.
*  w_fields-novaluehlp  =''.

  "populate fields of struture and append to itab
  APPEND w_fields TO t_fields.
  .

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
*     no_value_check  = ld_no_value_check
      popup_title     = l_popup_title
      start_column    = l_start_column
      start_row       = l_start_row
    IMPORTING
      returncode      = l_returncode
    TABLES
      fields          = t_fields
    EXCEPTIONS
      error_in_fields = 1.

  IF sy-subrc EQ 0.
    "All OK
  ELSEIF sy-subrc EQ 1. "Exception
    "Add code for exception here
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_ALV_LIST_VSR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preparar_alv_list_vsr .

*&---------------------------------------------------------------------*
*&  Declaração de Tabela Interna
*&---------------------------------------------------------------------*
  DATA: t_sort_110 TYPE lvc_t_sort,
        t_fcat_110 TYPE lvc_t_fcat.

*&---------------------------------------------------------------------*
*&  Declaração de Work Área
*&---------------------------------------------------------------------*
  DATA: w_variant_110 TYPE disvariant,
        w_layout_110  TYPE lvc_s_layo.

*&---------------------------------------------------------------------*
*&  Declaração de GRID - 0109
*&---------------------------------------------------------------------*
  DATA:
    v_docking  TYPE REF TO cl_gui_docking_container,
    v_splitter TYPE REF TO cl_gui_splitter_container.

*&---------------------------------------------------------------------*
*&  Declaração de Container
*&---------------------------------------------------------------------*
  DATA: v_container_110 TYPE REF TO cl_gui_custom_container.

  w_layout_110-cwidth_opt = 'X'.
  w_layout_110-sel_mode   = 'D'.
  w_layout_110-sel_mode   = 'D'.

  w_variant_110-report    = sy-repid.

  CREATE OBJECT v_container_110
    EXPORTING
      container_name              = 'CONTAINER'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF v_grid_110 IS INITIAL.

    PERFORM zf_montar_fieldcat  CHANGING t_list_vrs t_fcat_110.
    PERFORM zf_descricao_campos CHANGING t_fcat_110.

    CREATE OBJECT v_grid_110
      EXPORTING
        i_parent          = v_container_110
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT v_event_receiver_110.

*----------------------------------------------------------------------*
*** Define eventos
*----------------------------------------------------------------------*
    SET HANDLER v_event_receiver_110->handle_toolbar FOR v_grid_110.

    w_layout_110-sel_mode = 'A'.

    CALL METHOD v_grid_110->set_table_for_first_display
      EXPORTING
        is_variant                    = w_variant_110
        i_save                        = 'A'
        is_layout                     = w_layout_110
      CHANGING
        it_fieldcatalog               = t_fcat_110
        it_outtab                     = t_list_vrs
        it_sort                       = t_sort_110
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD v_grid_110->set_toolbar_interactive.

  ELSE.
    CALL METHOD v_grid_110->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_LIST_VRS  text
*      <--P_T_FCAT_VRS  text
*----------------------------------------------------------------------*
FORM zf_montar_fieldcat   CHANGING pt_tabela   TYPE ANY TABLE
                                  pt_fieldcat TYPE lvc_t_fcat.

  DATA:
    l_columns      TYPE REF TO cl_salv_columns_table,
    l_aggregations TYPE REF TO cl_salv_aggregations,
    l_salv_table   TYPE REF TO cl_salv_table,
    l_data         TYPE REF TO data.
  FIELD-SYMBOLS:
    <f_table>      TYPE STANDARD TABLE.

* Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA l_data LIKE pt_tabela.
  ASSIGN l_data->* TO <f_table>.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Monta a estrutura dinâmica no objeto l_salv_table
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = l_salv_table
        CHANGING
          t_table      = <f_table> ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* Recupera as colunas e dados internos
  l_columns      = l_salv_table->get_columns( ).
  l_aggregations = l_salv_table->get_aggregations( ).

* Monta o fieldcat
  pt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = l_columns
                                                                   r_aggregations = l_aggregations ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ELIMINA_BOTOES_LIST_VRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM elimina_botoes_list_vrs    USING  e_object TYPE REF TO cl_alv_event_toolbar_set.

*    elimina itens desnecessarios da barra do container
  DELETE e_object->mt_toolbar WHERE function = '&LOCAL&APPEND'
                                 OR function = '&LOCAL&INSERT_ROW'
                                 OR function = '&LOCAL&DELETE_ROW'
                                 OR function = '&LOCAL&COPY_ROW'
                                 OR function = '&LOCAL&CUT'
                                 OR function = '&LOCAL&COPY'
                                 OR function = '&LOCAL&PASTE'
                                 OR function = '&REFRESH'
                                 OR function = '&CHECK'
                                 OR function = '&GRAPH'
                                 OR function = '&INFO'
                                 OR function = '&LOCAL&UNDO'
                                 OR function = '&MB_VIEW'
*                                 OR function = '&MB_VARIANT'
*                                 OR function =  '&MB_EXPORT'
                                 OR function =  '&MB_SUM'
                                 OR function =  '&MB_SUBTOT'
                                 OR function =  '&PRINT_BACK'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_DESCRICAO_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FCAT_110  text
*----------------------------------------------------------------------*
FORM zf_descricao_campos CHANGING pt_fcat TYPE lvc_t_fcat.

  LOOP AT pt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN 'DT_BASE_VERSAO'.
        <fs_fcat>-scrtext_l = 'Data Base Versão'.
      WHEN 'BUTXT'.
        <fs_fcat>-scrtext_l = 'Descr. Empresa'.
      WHEN 'VERSAO'.
        <fs_fcat>-scrtext_l = 'Versão'.
      WHEN 'HORA_VERSAO'.
        <fs_fcat>-scrtext_l = 'Hora Versão'.
      WHEN OTHERS.
    ENDCASE.

    <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
    <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-reptext.

  ENDLOOP.

ENDFORM.
