*&---------------------------------------------------------------------*
*& Report  ZHCMR_PA0120_TGG
*&
*& -> Integração SAP x RSdata -> 5.2.3 - insertEmpregados()
*&    Insere ou atualiza dados de empregados e atividades conforme dados
*&    recebidos. Se houver necessidade os dados de Setor e Cargo podem ser
*&    incluídos ou atualizados, de acordo com o campo TPVerSetorCargo.
*&    A empresa do empregado é localizada pelo seu CNPJ e o empregado
*&    obedece ao campo TPVerEmpregado.
*&---------------------------------------------------------------------*

REPORT zhcmr_pa0120.

TABLES: pa0001, sscrfields.

TYPES: ty_rg_pernr TYPE RANGE OF pa0001-pernr.

DATA: zcl_rsdata        TYPE REF TO zcl_hcm_rsdata_tgg_cpi,
      lt_func           TYPE TABLE OF zhcm_funcionarios_list,
      i_prog            TYPE char20,
      r_bukrs           TYPE RANGE OF pa0001-bukrs,
      l_data_del        TYPE sy-datum,
      t_pernr_alterados TYPE TABLE OF pernr_list,
      v_limite_envio    TYPE sy-tabix VALUE '200'.

DATA: lit_delete TYPE zhcms_rsdata_empreg_res_get.

DATA: v_opt(2)   TYPE c. "PBI 63026  controle de opções vindo do programa ZHCMR_PA0068

*--------------------------------------------------------------------*
*SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
    p_bukrs   FOR pa0001-bukrs,
    p_werks   FOR pa0001-werks,
    p_pernr   FOR pa0001-pernr,
    p_date    FOR pa0001-begda NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_ausen AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK 1.

SELECTION-SCREEN: FUNCTION KEY 1.  "// FC01 - Envio Ausência

INITIALIZATION.

  sscrfields =
  VALUE #(
            functxt_01 = |{ icon_next_hierarchy_level }     { TEXT-021 }|
          ).

  CREATE OBJECT zcl_rsdata.


AT SELECTION-SCREEN.
* "// Excução do Botão Envia Ausência
  IF sy-ucomm EQ 'FC01' AND p_ausen IS NOT INITIAL.
    PERFORM fm_executar_ausencia_api.
  ENDIF.

START-OF-SELECTION.

  SELECT valsign AS sign
         valoption AS option
         valfrom AS low
    FROM setleaf
    INTO TABLE r_bukrs
    WHERE setname = 'ZHCM_RSDATA_TGGBUKRS'.

  zcl_rsdata->at_bukrs = r_bukrs.

  IMPORT v_opt TO v_opt FROM MEMORY ID 'v_opt'.

  IF ( p_date[] IS INITIAL ).
    "-> Data base para integração
    APPEND INITIAL LINE TO p_date[] ASSIGNING FIELD-SYMBOL(<lfs_date>).
    <lfs_date>-sign = 'I'.
    <lfs_date>-option = 'EQ'.
    <lfs_date>-low = sy-datum.
  ENDIF.

  LOOP AT p_date[] INTO DATA(w_date).

    "-> Data base para integração
    zcl_rsdata->at_data_base = COND #(
        WHEN w_date IS NOT INITIAL THEN w_date-low
                                   ELSE sy-datum ).

* Execução em background:
    IF ( p_bukrs[] IS INITIAL AND p_werks[] IS INITIAL AND p_pernr[] IS INITIAL ).

*  busca todos os colaboradores para enviar os serviços abaixo:
      DATA(request) = zcl_rsdata->get_dados_empregados( zcl_rsdata->get_lista_pernr( ) ).

      DELETE zcl_rsdata->at_it_dados_empregado[] WHERE bukrs NOT IN r_bukrs.

      IF ( zcl_rsdata->at_it_dados_empregado[] IS NOT INITIAL ).

        zcl_rsdata->insert_empresa( ).

        "-> Chama método para enviar férias cadastradas ao RSData:
        zcl_rsdata->insert_ferias( ).

        "-> Chama método para enviar exclusões ausências/absenteismos cadastradas ao RSData incorretamente
        zcl_rsdata->delete_absenteismo( ).

        "-> Chama método para enviar ausências/absenteismos cadastradas ao RSData:
        zcl_rsdata->insert_absenteismo( ).

        "-> Chama método para enviar cadastro de horários do funcionário:
        zcl_rsdata->insert_empregados_turnos( ).

        "-> Chama método para enviar transferências de empregados:
        zcl_rsdata->transfer_empregados( IMPORTING  e_pernr = DATA(rg_pernr_t) ).

      ENDIF.

*   Buscar matrículas alteradas no dia para envio no Insert_empregados:
      i_prog = 'ZHCMR_PA0062'.
      CALL FUNCTION 'ZHCMF_SFSF_0001'
        EXPORTING
          i_data            = zcl_rsdata->at_data_base
          i_retornar_lista  = 'X'
          i_prog            = i_prog
        TABLES
          t_pernr_alterados = t_pernr_alterados.

      CHECK  t_pernr_alterados[] IS NOT INITIAL.

      IF rg_pernr_t IS NOT INITIAL.
*** " São matriculas que tiverãoalgum tipo de erro. Então ficam fora do processamento
        DELETE t_pernr_alterados WHERE pernr IN rg_pernr_t[].
      ENDIF.

      lt_func[] = CORRESPONDING #( t_pernr_alterados[] ).

*      DELETE lt_func[] WHERE bukrs IN r_bukrs_remover.

*  Execução manual quando algum parâmetro for preenchido (Empresa, Filial, Matrícula):
    ELSE.
      PERFORM f_seleciona_matriculas.
    ENDIF.

    IF v_opt NE 'TR'. "PBI 63026 Verificar se é uma transferencia.

* InsertEmpregados:
      IF ( lt_func[] IS NOT INITIAL ).

        DATA(lrg_pernr) = VALUE ty_rg_pernr( FOR lws_func IN lt_func[] (
          sign   = 'I'
          option = 'EQ'
          low    = lws_func-pernr
          high   = lws_func-pernr ) ).
        SORT lrg_pernr[] BY low ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lrg_pernr[] COMPARING low.

        request = zcl_rsdata->get_dados_empregados(
          i_insert_empregados = abap_true
          it_funcionarios     = zcl_rsdata->get_lista_pernr( r_pernr = lrg_pernr[] ) ).

        IF ( request-rsdata-empregados-empregado[] IS NOT INITIAL ).

          SORT: request-rsdata-empregados-empregado BY nr_matricula.

          DELETE ADJACENT DUPLICATES FROM request-rsdata-empregados-empregado[] COMPARING nr_matricula.

          "-> Metodo para atualização de data de admissão, cargo e setor.
          zcl_rsdata->get_out_empregados( EXPORTING i_request = request IMPORTING i_request_cpf = DATA(i_request_cpf) i_request_ins = DATA(i_request_ins)  ).

          "-> Chama método para enviar os empregados:
          zcl_rsdata->insert_empregados( EXPORTING i_request = request i_request_cpf = i_request_cpf i_request_ins = i_request_ins ).
        ENDIF.

        "-> Chama método para enviar os empregados: DELETE - No caso de exclusão pela PU00
        zcl_rsdata->delete_empregados( EXPORTING i_delete = lit_delete IMPORTING  e_pernr = DATA(rg_pernr) ).

      ENDIF.

    ELSE. "PBI 63026 Caso tenha selecionado transferir usuário em ZHCMR_PA0068
      IF ( lt_func[] IS NOT INITIAL ).
        CLEAR:lrg_pernr.

        lrg_pernr = VALUE ty_rg_pernr( FOR lws_func IN lt_func[] (
            sign   = 'I'
            option = 'EQ'
            low    = lws_func-pernr
            high   = lws_func-pernr ) ).
        SORT lrg_pernr[] BY low ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lrg_pernr[] COMPARING low.

        IF lrg_pernr IS NOT INITIAL.
          zcl_rsdata->transfer_empregados( r_pernr = lrg_pernr[] ).
        ENDIF.

      ENDIF.
    ENDIF.
  ENDLOOP.

  IF ( sy-batch = abap_true ).
*  eliminar da tabela de logs os registros com mais de 6 meses:
    l_data_del = sy-datum - 180.
    DELETE FROM zhcmt_pa_0023 WHERE data_envio < l_data_del.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_MATRICULAS
*&---------------------------------------------------------------------*
*       - Seleciona as matrículas informadas no filtro para envio ao RSData
*       - Válido somente para o método InsertEmpregado *
*----------------------------------------------------------------------*
FORM f_seleciona_matriculas.

  DATA: v_endda       TYPE endda,
        v_dt_demissao TYPE endda,
        v_dt_antes    TYPE endda,
        v_tabix       TYPE sy-tabix.

  SELECT
    p1~pernr,
    p1~endda,
    p1~begda,
    p1~bukrs,
    p1~werks,
    p1~stell,
    p1~persk,
    p1~abkrs,
    p1~orgeh,
    p1~plans,
    p0~begda AS demissao_date,
    p0~stat2,
    p2~cname,
    p2~nachn,
    p2~vorna,
    p2~gesch,
    p2~famst,
    p2~gbdat
  FROM pa0001 AS p1
  LEFT JOIN pa0000 AS p0
  ON p1~pernr = p0~pernr
  LEFT JOIN pa0002 AS p2
  ON p1~pernr = p2~pernr
  INTO TABLE @DATA(it_func)
  WHERE
    p1~pernr IN @p_pernr[]        AND
    p1~endda >= @zcl_rsdata->at_data_base AND
    p1~begda <= @zcl_rsdata->at_data_base AND
    p1~bukrs IN @p_bukrs[]        AND
    p1~werks IN @p_werks[]        AND
    p1~bukrs IN @r_bukrs          AND
    p1~abkrs <> 'BA'              AND
    p0~begda <= @zcl_rsdata->at_data_base AND
    p0~endda >= @zcl_rsdata->at_data_base AND
    p0~stat2 IN ( '3', '0' )      AND
    p2~endda > @zcl_rsdata->at_data_base.
  SORT it_func[] BY pernr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_func[] COMPARING pernr.

  IF ( it_func[] IS NOT INITIAL ).

*-PBI 69293 - 02.12.2021 - JT - inicio
    LOOP AT it_func INTO DATA(wa_func).
      v_tabix = sy-tabix.

      IF wa_func-stat2 EQ 0.
        CLEAR: v_dt_demissao, v_dt_antes.

        CALL FUNCTION 'RP_GET_FIRE_DATE'
          EXPORTING
            persnr   = wa_func-pernr
          IMPORTING
            firedate = v_dt_demissao.

        v_dt_antes = |{ zcl_rsdata->at_data_base(4) }{ zcl_rsdata->at_data_base+4(2) }01|. "Considerar o mês para envio
        v_dt_antes = v_dt_antes - 1.

        IF v_dt_demissao <  v_dt_antes.
          DELETE it_func INDEX v_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.
*   DELETE it_func[] WHERE stat2 = '0' AND demissao_date  <= zcl_rsdata->at_data_base.
  ENDIF.
*-PBI 69293 - 02.12.2021 - JT - fim

  "-> Se existir demitidos à serem enviados, buscar posição anterior.
  IF ( line_exists( it_func[ plans = '99999999' ] ) ).

    LOOP AT it_func[]
      ASSIGNING FIELD-SYMBOL(<w_empregado>)  WHERE plans = '99999999'.

      v_endda = <w_empregado>-begda - 1.

      SELECT SINGLE pernr, begda, endda, stell, plans, orgeh
          FROM pa0001
          WHERE pernr = @<w_empregado>-pernr
              AND endda = @v_endda
              AND plans <> '99999999'
           INTO @DATA(w_posicao_demitido) .

      IF ( sy-subrc = 0 ).
        <w_empregado>-begda = w_posicao_demitido-begda.
        <w_empregado>-stell = w_posicao_demitido-stell.
        <w_empregado>-plans = w_posicao_demitido-plans.
        <w_empregado>-orgeh = w_posicao_demitido-orgeh.
      ENDIF.

      CLEAR: v_endda.
    ENDLOOP.
  ENDIF.

  IF ( it_func[] IS NOT INITIAL ).
    lt_func[] = CORRESPONDING #( it_func[] ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_EXECUTAR_AUSENCIA_API
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_executar_ausencia_api .

  SELECT valsign AS sign
         valoption AS option
         valfrom AS low
    FROM setleaf
    INTO TABLE r_bukrs
    WHERE setname = 'ZHCM_RSDATA_TGGBUKRS'.

  zcl_rsdata->at_bukrs = r_bukrs.

  zcl_rsdata->at_data_base = COND #( WHEN p_date IS INITIAL THEN sy-datum ELSE p_date-low ).

  zcl_rsdata->at_rg_pernr[] =
  VALUE #( FOR _line IN p_pernr[]
            (
              sign   = _line-sign
              option = _line-option
              low    = _line-low
            )
         ).
  DATA(request) = zcl_rsdata->get_dados_empregados( zcl_rsdata->get_lista_pernr_retroativo( ) ).

  CHECK zcl_rsdata->at_it_dados_empregado[] IS NOT INITIAL.

  zcl_rsdata->insert_empresa( ).
  zcl_rsdata->insert_ferias_retroativo( ).
  zcl_rsdata->insert_absenteismo_retroativo( ).

ENDFORM.
