*&---------------------------------------------------------------------*
*& Report  ZLESR0128
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0130.

DATA: zcl_ferro_saida_ws TYPE REF TO zcl_ferroviario_saida_ws.

*----------------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------------*

DATA: vg_dt_saida TYPE zlest0177-dt_carregamento.
DATA:vg_inter TYPE zlest0174-srv_integracao.

TABLES: zlest0177.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_dtsai  TYPE zlest0177-dt_carregamento,
            p_tpproc TYPE c NO-DISPLAY. " 1 - Consulta - 2 Processa
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.

  DATA: vg_job      TYPE i.

  SELECT SINGLE COUNT( * ) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'MAGGI_ZLES0170_DAY'
     AND status EQ 'R'.

  CHECK ( vg_job   EQ 1           ) OR
        ( p_dtsai  IS NOT INITIAL ) OR
        ( p_tpproc EQ '2'         ). "Processamento

  PERFORM: f_define_dt_saida,
           f_consulta_dados_carregamento,
           f_processa_dados_carregamento.

FORM f_consulta_dados_carregamento.

  CHECK ( p_tpproc EQ '1'     ) OR "Consulta
        ( p_tpproc IS INITIAL ).

  IMPORT vg_inter FROM MEMORY ID 'VG_INTER'.

  IF NOT  vg_inter IS INITIAL.

    IF  vg_inter = '01'.
      PERFORM f_consulta_dados_car_01. "Terminais VLI
    ELSEIF  vg_inter = '04'.
      PERFORM f_consulta_dados_car_04. "Terminais Rumo
    ENDIF.
  ELSE.
    PERFORM f_consulta_dados_car_01. "Terminais VLI

    PERFORM f_consulta_dados_car_04. "Terminais Rumo
  ENDIF.

ENDFORM.

FORM f_processa_dados_carregamento.

  DATA: tg_agrupa         TYPE TABLE OF zlest0177 WITH HEADER LINE,
        tg_zlest0177      TYPE TABLE OF zlest0177 WITH HEADER LINE,
        tg_zlest0178      TYPE TABLE OF zlest0178 WITH HEADER LINE,
        tg_zlest0178_aux  TYPE TABLE OF zlest0178 WITH HEADER LINE,
        tg_lfa1           TYPE TABLE OF lfa1      WITH HEADER LINE,
        wl_nota           TYPE zlest0019_l2_30,
        tg_notas_processo TYPE zde_zlest0019_l2_30_t,
        tg_notas          TYPE zde_zlest0019_l2_30_t,
        tg_logs           TYPE zlest0176_t,
        tg_logs_processo  TYPE zlest0176_t,
        v_id_proc         TYPE zlest0176-cont.

  DATA: v_dcl         TYPE zdcl,
        v_serie       TYPE zseriedcl,
        v_id_vagao    TYPE zidvagao,
        v_id_vagao_c7 TYPE c LENGTH 7,
        v_cnpj        TYPE zcnpjferro.

  DATA: zcl_ferroviario_saida TYPE REF TO zcl_ferroviario_saida.

  CLEAR: tg_zlest0177[], tg_zlest0178[], tg_notas[], tg_logs[].

  CHECK ( p_tpproc EQ '2'     ) OR "Processar
        ( p_tpproc IS INITIAL ).

  "Dados Carregamento Ferroviario - Integração WS
  SELECT *
    FROM zlest0177 INTO TABLE tg_zlest0177
   WHERE processado EQ abap_false.

  CHECK tg_zlest0177[] IS NOT INITIAL.

  "Tabela de dados Carregamento Ferroviario - Notas - Integração WS
  SELECT *
    FROM zlest0178 INTO TABLE tg_zlest0178
     FOR ALL ENTRIES IN tg_zlest0177
   WHERE id_registro EQ tg_zlest0177-id_registro.

  CHECK tg_zlest0178[] IS NOT INITIAL.

  tg_zlest0178_aux[] = tg_zlest0178[].
  DELETE tg_zlest0178_aux WHERE stcd1 IS INITIAL.
  SORT tg_zlest0178_aux BY stcd1.
  DELETE ADJACENT DUPLICATES FROM tg_zlest0178_aux COMPARING stcd1.

  IF tg_zlest0178_aux[] IS NOT INITIAL.
    SELECT *
      FROM lfa1 INTO TABLE tg_lfa1
       FOR ALL ENTRIES IN tg_zlest0178_aux
     WHERE stcd1 EQ tg_zlest0178_aux-stcd1.

    PERFORM f_elimina_lfa1_bloq TABLES tg_lfa1.
  ENDIF.

  SORT tg_zlest0177 BY id_registro.

  LOOP AT tg_zlest0177.

    CLEAR: v_dcl, v_serie, v_id_vagao, v_cnpj, tg_notas_processo[], tg_notas_processo.

    v_dcl           = tg_zlest0177-nr_cte.
    v_serie         = tg_zlest0177-serie_cte.
    v_id_vagao_c7   = tg_zlest0177-idvagao.
    v_id_vagao_c7   = |{ v_id_vagao_c7 ALPHA = IN } |.
    v_id_vagao      = tg_zlest0177-serie_vagao && v_id_vagao_c7 &&  'T'.
    v_cnpj          = tg_zlest0177-cnpj_terminal_transb.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_dcl
      IMPORTING
        output = v_dcl.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_serie
      IMPORTING
        output = v_serie.

    LOOP AT tg_zlest0178 WHERE id_registro EQ tg_zlest0177-id_registro.

      CLEAR: wl_nota.

      CHECK tg_zlest0178-stcd1 IS NOT INITIAL.

      wl_nota-nfenum          = tg_zlest0178-nfenum.
      wl_nota-cnpjcliente     = tg_zlest0178-stcd1.

      LOOP AT tg_lfa1 WHERE stcd1 = tg_zlest0178-stcd1.
        wl_nota-cod_fornecedor = tg_lfa1-lifnr.

        IF tg_lfa1-ktokk = 'ZFIC'.
          SELECT SINGLE *
            FROM j_1bbranch INTO @DATA(_wl_branch)
           WHERE branch = @tg_lfa1-lifnr+6(4).

          IF sy-subrc EQ 0.
            wl_nota-bukrs           = _wl_branch-bukrs.
            wl_nota-branch          = _wl_branch-branch.
          ENDIF.
        ENDIF.
      ENDLOOP.

      wl_nota-dtadecarga      = tg_zlest0177-dt_carregamento.
      wl_nota-dtachegada      = tg_zlest0177-dt_carregamento.
      wl_nota-pesodvagao      = tg_zlest0178-peso_carregado.

      APPEND wl_nota TO tg_notas_processo.
    ENDLOOP.

    FREE zcl_ferroviario_saida.
    CREATE OBJECT zcl_ferroviario_saida.

    zcl_ferroviario_saida->monta_processo( i_dcl        = v_dcl
                                           i_serie      = v_serie
                                           i_id_vagao   = v_id_vagao
                                           i_cnpj       = v_cnpj
                                           i_notas      = tg_notas_processo ).


    READ TABLE tg_notas_processo INTO DATA(_nota_processo) INDEX 1.
    IF ( sy-subrc EQ 0 ) AND ( _nota_processo-dtachegada IS NOT INITIAL ).
      zcl_ferroviario_saida->set_dt_saida( i_dt_saida = _nota_processo-dtachegada ).
    ENDIF.

    IF zcl_ferroviario_saida->ck_alterou EQ abap_true.
      IF zcl_ferroviario_saida->get_id_refkey( ) IS INITIAL.
        zcl_ferroviario_saida->set_id_refkey( i_id_refkey = zcl_ferroviario=>get_new_id_refkey( ) ).
      ENDIF.
      IF zcl_ferroviario_saida->validar_registro( ) EQ abap_true.
        zcl_ferroviario_saida->gravar_registro( ).
      ELSE.
        PERFORM f_gravar_log CHANGING v_id_proc.

        UPDATE zlest0177 SET processado = abap_true
                             erro_proc  = abap_true
                             id_proc    = v_id_proc
         WHERE id_registro EQ tg_zlest0177-id_registro.
      ENDIF.
    ENDIF.

  ENDLOOP.

  UPDATE zlest0177 SET processado = abap_true
   WHERE processado EQ abap_false.

ENDFORM.

FORM f_define_dt_saida.

  CLEAR: vg_dt_saida.

  vg_dt_saida = sy-datum.

  IF p_dtsai IS NOT INITIAL.
    vg_dt_saida = p_dtsai.
  ENDIF.

ENDFORM.


FORM f_elimina_lfa1_bloq TABLES p_lfa1 STRUCTURE lfa1.

  LOOP AT p_lfa1 INTO DATA(wl_lfa1).
    DATA(_delete) = ''.
    DATA(_tabix)  = sy-tabix.
    TRY.
        zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = wl_lfa1-lifnr
        )->ck_ativo( ).
      CATCH zcx_parceiros INTO DATA(ex_parceiros_k).
        _delete = 'X'.
    ENDTRY.
    IF _delete IS NOT INITIAL.
      DELETE p_lfa1 INDEX _tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM f_gravar_log CHANGING c_count TYPE zlest0176-cont.

  DATA: v_cont       TYPE zlest0176-cont,
        v_cont_item  TYPE zlest0176-cont_item,
        wl_zlest0176 TYPE zlest0176.

  CLEAR: v_cont, v_cont_item, c_count.

  SELECT MAX( cont )
    FROM zlest0176 INTO v_cont.

  ADD 1 TO v_cont.

  CLEAR: wl_zlest0176.

  ADD 1 TO v_cont_item.

  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(_msg).

  wl_zlest0176-cont         = v_cont.
  wl_zlest0176-cont_item    = v_cont_item.
  wl_zlest0176-msgtyp       = sy-msgty.
  wl_zlest0176-msgnr        = sy-msgno.
  wl_zlest0176-msgv1        = _msg.
  wl_zlest0176-dt_registro  = sy-datum.
  wl_zlest0176-hr_registro  = sy-uzeit.
  wl_zlest0176-us_registro  = sy-uname.

  MODIFY zlest0176 FROM wl_zlest0176.

  c_count = v_cont.

ENDFORM.

FORM f_consulta_dados_car_01 .

  DATA: tg_terminais     TYPE TABLE OF setleaf WITH HEADER LINE.

  DATA: v_sigla_terminal TYPE zde_sigla_terminal.

  CLEAR: tg_terminais[].

  SELECT *
    FROM setleaf INTO TABLE tg_terminais
   WHERE setname = 'TERMINAIS_VLI_L2_WS'.

  LOOP AT tg_terminais.

    v_sigla_terminal = tg_terminais-valfrom.

    FREE zcl_ferro_saida_ws.
    CREATE OBJECT zcl_ferro_saida_ws.

    zcl_ferro_saida_ws->set_srv_integracao( i_srv_integracao =  '01' ). "Terminais VLI
    zcl_ferro_saida_ws->set_dt_saida( i_dt_saida = vg_dt_saida ).
    zcl_ferro_saida_ws->set_sigla_terminal( i_sigla_terminal =  v_sigla_terminal ).
    IF zcl_ferro_saida_ws->consultar( ) EQ abap_true.
      zcl_ferro_saida_ws->gravar_dados( ).
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_consulta_dados_car_04 .

  DATA: tg_terminais TYPE TABLE OF setleaf WITH HEADER LINE,
        tg_cnpj      TYPE TABLE OF setleaf WITH HEADER LINE,
        tg_estacao   TYPE TABLE OF setleaf WITH HEADER LINE.

  DATA: v_sigla_terminal TYPE zde_sigla_terminal,
        v_sigla_estacao  TYPE zde_estacao_rumo,
        v_cnpj           TYPE zde_cnpj_term_rumo,
        v_data(10)       TYPE c.


  CLEAR: tg_terminais[],
         tg_cnpj[].

  SELECT *
    FROM setleaf INTO TABLE tg_terminais
   WHERE setname = 'TERMINAIS_RUMO_L2_WS'.


  SELECT *
  FROM setleaf INTO TABLE tg_cnpj
 WHERE setname = 'CNPJ_GRP_NEG_RUMO_L2_WS'.


  LOOP AT tg_terminais.

    v_sigla_estacao = tg_terminais-valfrom.

    READ TABLE tg_cnpj INDEX 1.
    v_cnpj = tg_cnpj-valfrom.

    v_sigla_estacao = tg_terminais-valfrom.

    CONCATENATE sy-datum+0(4) '-' sy-datum+4(2) '-' sy-datum+6(2) INTO v_data.

    FREE zcl_ferro_saida_ws.
    CREATE OBJECT zcl_ferro_saida_ws.

    zcl_ferro_saida_ws->set_srv_integracao( i_srv_integracao =  '04' ). "Terminais VLI
    zcl_ferro_saida_ws->set_dt_saida( i_dt_saida = vg_dt_saida ).
    " zcl_ferro_saida_ws->set_sigla_terminal( i_sigla_terminal =  v_sigla_terminal ).
    zcl_ferro_saida_ws->set_operacao( i_operacao = 'INSERIR' ).
    zcl_ferro_saida_ws->set_terminal( i_terminal = v_cnpj ).
    zcl_ferro_saida_ws->set_data( i_data = v_data ).
    zcl_ferro_saida_ws->set_hora( i_hora = '').
    zcl_ferro_saida_ws->set_estacao( i_sigla_estacao =   v_sigla_estacao ).
    zcl_ferro_saida_ws->set_documentacao( i_documentacao = 'false' ).
    zcl_ferro_saida_ws->set_gp_negociador( i_gp_negociador = 'S' ).


    IF zcl_ferro_saida_ws->consultar( ) EQ abap_true.
      zcl_ferro_saida_ws->gravar_dados( ).
    ENDIF.

  ENDLOOP.

ENDFORM.
