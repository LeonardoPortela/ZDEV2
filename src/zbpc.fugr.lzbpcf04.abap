*----------------------------------------------------------------------*
***INCLUDE LZBPCF04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZSELECIONA_DADOS_VIDA_UTIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zseleciona_dados_vida_util .
  LOOP AT it_vida_util_imobilizado INTO wa_vida_util_imobilizado.
    IF wa_vida_util_imobilizado-campo EQ 'BUKRS'.
      p_bukrs-low    = wa_vida_util_imobilizado-valor_de.
      p_bukrs-high   = wa_vida_util_imobilizado-valor_ate.
      p_bukrs-sign   = wa_vida_util_imobilizado-sign.
      p_bukrs-option = wa_vida_util_imobilizado-option.
      APPEND p_bukrs.
    ENDIF.

    IF wa_vida_util_imobilizado-campo EQ 'GJAHR'.
      p_gjahr-low    = wa_vida_util_imobilizado-valor_de.
      p_gjahr-high   = wa_vida_util_imobilizado-valor_ate.
      p_gjahr-sign   = wa_vida_util_imobilizado-sign.
      p_gjahr-option = wa_vida_util_imobilizado-option.
      APPEND p_gjahr.
    ENDIF.

    IF wa_vida_util_imobilizado-campo EQ 'AFABE'.
      p_afabe-low    = wa_vida_util_imobilizado-valor_de.
      p_afabe-high   = wa_vida_util_imobilizado-valor_ate.
      p_afabe-sign   = wa_vida_util_imobilizado-sign.
      p_afabe-option = wa_vida_util_imobilizado-option.
      APPEND p_afabe.
    ENDIF.

    IF wa_vida_util_imobilizado-campo EQ 'SAKNR'.
      p_saknr-low    = wa_vida_util_imobilizado-valor_de.
      p_saknr-high   = wa_vida_util_imobilizado-valor_ate.
      p_saknr-sign   = wa_vida_util_imobilizado-sign.
      p_saknr-option = wa_vida_util_imobilizado-option.
      APPEND p_saknr.
    ENDIF.

  ENDLOOP.

  DATA: year(4)       TYPE c,
        v_mes_proc(2) TYPE c,
        v_data_i      TYPE sy-datum,
        v_data_f      TYPE sy-datum.


  CLEAR:year.



  " Pego o inicio do mes
  "CONCATENATE sy-datum(4) sy-datum+4(2) '01' INTO v_data_i .


  IF p_dt_exec[] IS NOT INITIAL.
    " Pego o inicio do mes
    CONCATENATE p_dt_exec-low(4) p_dt_exec-low+4(2) '01' INTO v_data_i .
  ELSE.
    CONCATENATE sy-datum(4) sy-datum+4(2) '01' INTO v_data_i .
  ENDIF.


  " Pego o Final do mes
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = v_data_i
    IMPORTING
      last_day_of_month = v_data_f.


  "Pego todos os dados de imobilizado.
  SELECT *
    FROM anla
    INTO TABLE it_anla
    WHERE bukrs IN p_bukrs.


  "DELETE it_anla WHERE deakt IS NOT INITIAL.
  DELETE it_anla WHERE zugdt IS INITIAL.

  " Se a data de aquisição for maior que o mes que estou rodando.
  DELETE it_anla WHERE zugdt >  v_data_f.


  it_anla_aux[] = it_anla[].

  DELETE it_anla WHERE deakt IS NOT INITIAL.
  DELETE it_anla_aux WHERE deakt IS INITIAL.

  IF it_anla_aux[] IS NOT INITIAL.
    LOOP AT it_anla_aux INTO wa_anla_aux.
      IF wa_anla_aux-deakt > v_data_i .

        wa_anla-bukrs = wa_anla_aux-bukrs.
        wa_anla-anln1 = wa_anla_aux-anln1.
        wa_anla-anln2 = wa_anla_aux-anln2.
        wa_anla-ktogr = wa_anla_aux-ktogr.
        wa_anla-anlkl = wa_anla_aux-anlkl.

        APPEND wa_anla TO it_anla.

      ENDIF.
    ENDLOOP.
  ENDIF.

  "Pega o ano corrente.
  year = sy-datum(4) .

  "Pega o mes que esta rodando.
  v_mes_proc = v_data_i+4(2).


  CHECK it_anla[] IS NOT INITIAL.

  " Pega o periodo.
  SELECT *
      FROM anlb
      INTO TABLE it_anlb
      FOR ALL ENTRIES IN it_anla
      WHERE bukrs = it_anla-bukrs
      AND   anln1 = it_anla-anln1
      AND   anln2 = it_anla-anln2
      AND   afabe EQ '01'.

  " Pega os valores em Reais
  SELECT *
      FROM anlc
      INTO TABLE it_anlc
      FOR ALL ENTRIES IN it_anla
      WHERE bukrs = it_anla-bukrs
      AND   anln1 = it_anla-anln1
      AND   anln2 = it_anla-anln2
      AND   afabe = '01'
      AND   gjahr = year.

  "Pego as baixas
  SELECT *
      FROM anlp
      INTO TABLE it_anlp
      FOR ALL ENTRIES IN it_anlc
      WHERE bukrs   =  it_anlc-bukrs
      AND   gjahr   =  it_anlc-gjahr
      AND   anln1   =  it_anlc-anln1
      AND   anln2   =  it_anlc-anln2
      AND   afaber  =  it_anlc-afabe        " Area de avaliação
      AND   peraf   =  v_mes_proc.         " Mes de processamento


  SELECT *
    FROM t095
    INTO TABLE it_t095
    FOR ALL ENTRIES IN it_anla
    WHERE ktogr = it_anla-ktogr
    AND   afabe = '01'.


ENDFORM.                    " ZSELECIONA_DADOS_VIDA_UTIL
*&---------------------------------------------------------------------*
*&      Form  ZPROCESSA_RETORNO_VIDA_UTIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zprocessa_retorno_vida_util .
  SORT :   it_anla BY bukrs anln1 anln2,
           it_anlb BY bukrs anln1 anln2,
           it_anlc BY bukrs anln1 anln2,
           it_t095 BY ktogr.



  DATA: v_data_proc TYPE sy-datum,
        v_tot_answl TYPE anlp-answl.

  "CONCATENATE sy-datum(4) sy-datum+4(2) '01' INTO v_data_proc .

  IF p_dt_exec[] IS NOT INITIAL.
    " Pego o inicio do mes
    CONCATENATE p_dt_exec-low(4) p_dt_exec-low+4(2) '01' INTO v_data_proc .
  ELSE.
    CONCATENATE sy-datum(4) sy-datum+4(2) '01' INTO v_data_proc .
  ENDIF.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = v_data_proc
    IMPORTING
      last_day_of_month = v_data_proc.




  LOOP AT it_anla INTO wa_anla.

    CLEAR: wa_zfit0044.

    READ TABLE it_anlb INTO wa_anlb WITH KEY bukrs = wa_anla-bukrs
                                             anln1 = wa_anla-anln1
                                             anln2 = wa_anla-anln2 BINARY SEARCH.
    READ TABLE it_anlc INTO wa_anlc WITH KEY bukrs = wa_anla-bukrs
                                             anln1 = wa_anla-anln1
                                             anln2 = wa_anla-anln2 BINARY SEARCH.
    READ TABLE it_t095 INTO wa_t095 WITH KEY ktogr = wa_anla-ktogr BINARY SEARCH.

    IF ( p_afabe IS NOT INITIAL ) AND ( wa_anlc-afabe NOT IN p_afabe ) .
      CONTINUE.
    ENDIF.

    IF ( p_gjahr IS NOT INITIAL ) AND ( wa_anlc-gjahr NOT IN p_gjahr ) .
      CONTINUE.
    ENDIF.

    IF ( p_saknr IS NOT INITIAL ) AND ( wa_t095-ktansw NOT IN p_saknr ) .
      CONTINUE.
    ENDIF.

*    IF WA_ANLC-ANSWL EQ 0.
*      CONTINUE.
*    ENDIF.

*    WA_RETVIDA_UTIL_IMOBILIZADO-EMPRESA              = WA_ANLA-BUKRS.
*    WA_RETVIDA_UTIL_IMOBILIZADO-NU_IMOBILIZADO       = WA_ANLA-ANLN1.
*    WA_RETVIDA_UTIL_IMOBILIZADO-SU_IMOBILIZADO       = WA_ANLA-ANLN2.
*    WA_RETVIDA_UTIL_IMOBILIZADO-CLASSE_IMOB          = WA_ANLA-ANLKL.
*    WA_RETVIDA_UTIL_IMOBILIZADO-DET_CONTA            = WA_ANLA-KTOGR.
*    WA_RETVIDA_UTIL_IMOBILIZADO-AREA_AVAL            = WA_ANLC-AFABE.
*    WA_RETVIDA_UTIL_IMOBILIZADO-VIDA_PLAN_PER        = WA_ANLB-NDPER.
*    WA_RETVIDA_UTIL_IMOBILIZADO-VIDA_PLAN_ANOS       = WA_ANLB-NDJAR.
*    WA_RETVIDA_UTIL_IMOBILIZADO-VIDA_PLAN_ANOS_F     = ( ( WA_ANLB-NDJAR * 12 ) + WA_ANLB-NDPER ) / 12.
*    WA_RETVIDA_UTIL_IMOBILIZADO-VALOR_AQUI_ACU       = WA_ANLC-KANSW .
*    WA_RETVIDA_UTIL_IMOBILIZADO_AX-VALOR_AQUI_PER    = WA_ANLC-KANSW * WA_RETVIDA_UTIL_IMOBILIZADO_AX-VIDA_PLAN_ANOS_F.
*    WA_RETVIDA_UTIL_IMOBILIZADO_AX-CONTA_RAZAO       = WA_T095-KTANSW.
*    WA_RETVIDA_UTIL_IMOBILIZADO_AX-EXERCICIO         = WA_ANLC-GJAHR.
*    WA_RETVIDA_UTIL_IMOBILIZADO_AX-DATA_AQUIS        = V_DATA.
*
*    APPEND WA_RETVIDA_UTIL_IMOBILIZADO_AX TO IT_RETVIDA_UTIL_IMOBILIZADO_AX.


    wa_zfit0044-empresa           = wa_anla-bukrs.
    wa_zfit0044-nu_imobilizado    = wa_anla-anln1.
    wa_zfit0044-su_imobilizado    = wa_anla-anln2.
    wa_zfit0044-classe_imob       = wa_anla-anlkl.
    wa_zfit0044-det_conta         = wa_anla-ktogr.
    wa_zfit0044-area_aval         = wa_anlc-afabe.
    wa_zfit0044-vida_plan_per     = wa_anlb-ndper.
    wa_zfit0044-vida_plan_anos    = wa_anlb-ndjar.
    wa_zfit0044-vida_plan_anos_f  = ( ( wa_anlb-ndjar * 12 ) + wa_anlb-ndper ) / 12.

    CLEAR: v_tot_answl.

    LOOP AT it_anlp INTO wa_anlp WHERE bukrs = wa_anlc-bukrs AND gjahr = wa_anlc-gjahr AND  anln1 = wa_anlc-anln1 AND  anln2 =  wa_anlc-anln2.

      v_tot_answl =  v_tot_answl + wa_anlp-answl.


    ENDLOOP.

    wa_zfit0044-valor_aqui_acu    = wa_anlc-kansw + v_tot_answl.  " Se teve transferencia ou baixa soma com o WA_ANLC-ANSWL
    wa_zfit0044-valor_aqui_per    = wa_zfit0044-valor_aqui_acu * wa_zfit0044-vida_plan_anos_f.
    wa_zfit0044-conta_razao       = wa_t095-ktansw.
    wa_zfit0044-exercicio         = wa_anlc-gjahr.
    wa_zfit0044-data_aqui         = v_data_proc.

    APPEND wa_zfit0044  TO it_zfit0044.

  ENDLOOP.


  " Salvo as informações na tabela.
  MODIFY zfit0044 FROM TABLE it_zfit0044.
  COMMIT WORK.

  "it_retvida_util_imobilizado[] = it_zfit0044[].

*  "Faco a media ponderada por conta de imobilizado e salvo na tabela Z.
*
*  DATA: YEAR(4)          TYPE C,
*        V_VALOR_AQUI_PER TYPE ANLC-KANSW,
*        V_VALOR_AQUI_ACU TYPE ANLC-KANSW,
*        V_VALOR_AQUI_TOT TYPE ANLC-KANSW.
*
*  CLEAR:YEAR,
*        V_VALOR_AQUI_PER,
*        V_VALOR_AQUI_ACU,
*        V_VALOR_AQUI_TOT,
*        IT_RETVIDA_UTIL_IMOBILIZADO[].
*
*  IT_RETVIDA_UTIL_IMOBILIZADO[] = IT_RETVIDA_UTIL_IMOBILIZADO_AX[].
*
*  SORT :   IT_RETVIDA_UTIL_IMOBILIZADO BY EMPRESA CONTA_RAZAO.
*
*  DELETE ADJACENT DUPLICATES FROM IT_RETVIDA_UTIL_IMOBILIZADO COMPARING EMPRESA CONTA_RAZAO.
*
*  LOOP AT IT_RETVIDA_UTIL_IMOBILIZADO INTO WA_RETVIDA_UTIL_IMOBILIZADO.
*    CLEAR:YEAR,
*      V_VALOR_AQUI_PER,
*      V_VALOR_AQUI_ACU,
*      V_VALOR_AQUI_TOT.
*
*    LOOP AT IT_RETVIDA_UTIL_IMOBILIZADO_AX INTO WA_RETVIDA_UTIL_IMOBILIZADO_AX WHERE EMPRESA = WA_RETVIDA_UTIL_IMOBILIZADO-EMPRESA AND CONTA_RAZAO = WA_RETVIDA_UTIL_IMOBILIZADO-CONTA_RAZAO.
*       V_VALOR_AQUI_PER = V_VALOR_AQUI_PER +  WA_RETVIDA_UTIL_IMOBILIZADO_AX-VALOR_AQUI_PER .
*       V_VALOR_AQUI_ACU = V_VALOR_AQUI_ACU +  WA_RETVIDA_UTIL_IMOBILIZADO_AX-VALOR_AQUI_ACU.
*    ENDLOOP.
*
*    V_VALOR_AQUI_TOT = V_VALOR_AQUI_ACU / V_VALOR_AQUI_PER. " BALANCE e TURNOVER
*
*
*    " Inserir Valores na tabela
*
*
*    WA_ZFIT0044-EMPRESA          =  WA_RETVIDA_UTIL_IMOBILIZADO-EMPRESA.
*    WA_ZFIT0044-AREA_AVAL        =  WA_RETVIDA_UTIL_IMOBILIZADO-AREA_AVAL.
*    WA_ZFIT0044-VIDA_PLAN_PER    =  WA_RETVIDA_UTIL_IMOBILIZADO-PLAN_PER.
*    WA_ZFIT0044-VIDA_PLAN_ANOS   =  WA_RETVIDA_UTIL_IMOBILIZADO-VIDA_PLAN_ANOS.
*    WA_ZFIT0044-VIDA_PLAN_ANOS_F =  WA_RETVIDA_UTIL_IMOBILIZADO-VIDA_PLAN_ANOS_F.
*    WA_ZFIT0044-VALOR_AQUI_ACU   =  V_VALOR_AQUI_TOT.
*    WA_ZFIT0044-VALOR_AQUI_PER   =  V_VALOR_AQUI_PER.
*    WA_ZFIT0044-CONTA_RAZAO      =  WA_RETVIDA_UTIL_IMOBILIZADO-CONTA_RAZAO.
*    WA_ZFIT0044-EXERCICIO        =  WA_RETVIDA_UTIL_IMOBILIZADO-EXERCICIO.
*    WA_ZFIT0044-DATA_AQUI        =  V_DATA.
*
*    APPEND WA_ZFIT0044  TO IT_ZFIT0044.
*
*  ENDLOOP.






ENDFORM.                    " ZPROCESSA_RETORNO_VIDA_UTIL
