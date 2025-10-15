*&---------------------------------------------------------------------*
*& Report  ZLESR0128
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0128.

TYPES: BEGIN OF y_check,
         v1(02) TYPE c,
       END OF y_check.


DATA: zcl_rodo_chegada_ws TYPE REF TO zcl_rodoviario_chegada_ws.

*----------------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------------*

DATA: vg_dt_chegada TYPE zlest0174-dt_chegada.
DATA:lt_check TYPE TABLE OF y_check,
     ls_check TYPE y_check.

DATA:vg_inter TYPE zlest0174-srv_integracao.

TABLES: zlest0174.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS: p_dtche  TYPE zlest0174-dt_chegada,
              p_tpproc TYPE c NO-DISPLAY. " 1 - Consulta - 2 Processa
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.

  DATA: vg_job      TYPE i.

  SELECT SINGLE COUNT( * ) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'MAGGI_ZLES0170'
     AND status EQ 'R'.

*  CHECK ( vg_job EQ 1 ) OR
*        ( p_dtche IS NOT INITIAL ) OR
*        ( p_tpproc EQ '2' ). "Processamento

  CHECK
        ( p_dtche IS NOT INITIAL ) OR
        ( p_tpproc EQ '2' ). "Processamento

  PERFORM: f_define_dt_chegada,
           f_consulta_dados_descarga,
           f_processa_dados_descarga.

FORM f_consulta_dados_descarga.



  ls_check-v1  = '01'.
  APPEND ls_check TO lt_check.
  ls_check-v1  = '02'.
  APPEND ls_check TO lt_check.
  ls_check-v1  = '03'.
  APPEND ls_check TO lt_check.
  ls_check-v1  = '04'.
  APPEND ls_check TO lt_check.
  ls_check-v1  = '05'.
  APPEND ls_check TO lt_check.
  ls_check-v1  = '06'.
  APPEND ls_check TO lt_check.

  CHECK ( p_tpproc EQ '1'     ) OR "Consulta
        ( p_tpproc IS INITIAL ).

  CLEAR:vg_inter.
  IMPORT vg_inter FROM MEMORY ID 'VG_INTER'.


  SELECT SINGLE COUNT( * ) INTO vg_job
     FROM tbtco
    WHERE jobname EQ 'MAGGI_ZLES0170_MINUTE'
      AND status EQ 'R'.

  IF vg_job = '1'.

    SELECT * FROM tvarvc
      INTO TABLE @DATA(lt_tvarvc)
      WHERE name = 'ZLES0170_INT_MINUTE'.

    SELECT * FROM tvarvc
      INTO TABLE @DATA(lt_tvarvc_day)
      WHERE name = 'MAGGI_ZLES0170_DAY'.

  ENDIF.

  IF NOT vg_inter IS INITIAL.

    IF vg_inter = '01'.
      PERFORM: f_consulta_dados_descarga_01. "Terminais VLI
    ELSEIF  vg_inter = '02'.
      PERFORM: f_consulta_dados_descarga_02. "Terminais BUNGE
    ELSEIF  vg_inter = '03'.
      PERFORM: f_consulta_dados_descarga_03. "Terminais TGG
    ELSEIF  vg_inter = '04'.
      PERFORM: f_consulta_dados_descarga_04. "Terminais RUMO
*Inicio Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
    ELSEIF  vg_inter = '05'.
      PERFORM: f_consulta_dados_descarga_05. "Terminais BRADO
*Fim Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
    ELSEIF  vg_inter = '06'.
      PERFORM: f_consulta_dados_descarga_06. "Terminais ROCHA
    ENDIF.

  ELSEIF NOT lt_tvarvc IS INITIAL.

    LOOP AT lt_tvarvc INTO DATA(ls_tvarvc).

      IF ls_tvarvc-low = '01'.
        PERFORM: f_consulta_dados_descarga_01. "Terminais VLI
      ELSEIF  ls_tvarvc-low = '02'.
        PERFORM: f_consulta_dados_descarga_02. "Terminais BUNGE
      ELSEIF  ls_tvarvc-low = '03'.
        PERFORM: f_consulta_dados_descarga_03. "Terminais TGG
      ELSEIF  ls_tvarvc-low = '04'.
        PERFORM: f_consulta_dados_descarga_04. "Terminais RUMO
*Inicio Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
      ELSEIF  ls_tvarvc-low = '05'.
        PERFORM: f_consulta_dados_descarga_05. "Terminais BRADO
*Fim Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
      ELSEIF  ls_tvarvc-low = '06'.
        PERFORM: f_consulta_dados_descarga_06. "Terminais ROCHA
      ENDIF.

    ENDLOOP.

  ELSEIF NOT lt_tvarvc_day IS INITIAL.

    LOOP AT lt_check INTO ls_check.

      READ TABLE lt_tvarvc_day INTO DATA(ls_tvarvc_day) WITH KEY  low = ls_check-v1.


      IF sy-subrc <> 0.

        IF ls_check-v1 = '01'.
          PERFORM: f_consulta_dados_descarga_01. "Terminais VLI
        ELSEIF  ls_check-v1 = '02'.
          PERFORM: f_consulta_dados_descarga_02. "Terminais BUNGE
        ELSEIF  ls_check-v1 = '03'.
          PERFORM: f_consulta_dados_descarga_03. "Terminais TGG
        ELSEIF  ls_check-v1 = '04'.
          PERFORM: f_consulta_dados_descarga_04. "Terminais RUMO
*Inicio Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
        ELSEIF  ls_check-v1 = '05'.
          PERFORM: f_consulta_dados_descarga_05. "Terminais BRADO
*Fim Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
        ELSEIF  ls_check-v1 = '06'.
          PERFORM: f_consulta_dados_descarga_06. "Terminais ROCHA
        ENDIF.


      ENDIF.


    ENDLOOP.


  ELSE.

    PERFORM: f_consulta_dados_descarga_01. "Terminais VLI
    PERFORM: f_consulta_dados_descarga_02. "Terminais BUNGE
    PERFORM: f_consulta_dados_descarga_03. "Terminais TGG
    PERFORM: f_consulta_dados_descarga_04. "Terminais RUMO
*Inicio Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
    PERFORM: f_consulta_dados_descarga_05. "Terminais BRADO
*Fim Alteração - Leandro Valentim Ferreria - 02.08.2023 - #70575
    PERFORM: f_consulta_dados_descarga_06. "Terminais ROCHA   "CS2022000810   #84550 FF   14.02.2023

  ENDIF.
ENDFORM.

FORM f_processa_dados_descarga.

  DATA: tg_terminais      TYPE TABLE OF zlest0174 WITH HEADER LINE,
        tg_zlest0174      TYPE TABLE OF zlest0174 WITH HEADER LINE,
        tg_zlest0175      TYPE TABLE OF zlest0175 WITH HEADER LINE,
        wl_nota           TYPE zlest0019_l1_30,
        tg_logs           TYPE zlest0176_t,
        tg_logs_terminal  TYPE zlest0176_t,
        tg_notas_terminal TYPE zde_zlest0019_l1_30_t,
        tg_notas          TYPE zde_zlest0019_l1_30_t.

  DATA: zcl_rodoviario_chegada TYPE REF TO zcl_rodoviario_chegada.

  CLEAR: tg_zlest0174[], tg_zlest0175[], tg_notas[], tg_logs[].

  CHECK ( p_tpproc EQ '2'     ) OR "Processar
        ( p_tpproc IS INITIAL ).

  "Dados descarga Rodoviário - Integração WS
  SELECT *
    FROM zlest0174 INTO TABLE tg_zlest0174
   WHERE processado EQ abap_false.

  CHECK tg_zlest0174[] IS NOT INITIAL.

  "Tabela de dados descarga Rodoviário - Notas - Integração WS
  SELECT *
    FROM zlest0175 INTO TABLE tg_zlest0175
     FOR ALL ENTRIES IN tg_zlest0174
   WHERE id_registro EQ tg_zlest0174-id_registro.

  CHECK tg_zlest0175[] IS NOT INITIAL.

  LOOP AT tg_zlest0175.

    READ TABLE tg_zlest0174 WITH KEY id_registro = tg_zlest0175-id_registro.

    CHECK sy-subrc EQ 0.

    CLEAR: wl_nota.

    wl_nota-cnpjferro   = tg_zlest0174-cnpj_terminal_transb.
    wl_nota-dtaenvio    = tg_zlest0174-dt_registro.
    wl_nota-horaenvio   = tg_zlest0174-hr_registro.
    wl_nota-nfenum      = tg_zlest0175-nfenum.
    wl_nota-pesonf      = tg_zlest0175-peso_declarado.
    wl_nota-pesodvagao  = tg_zlest0175-peso_descarga.
    wl_nota-dtachegada  = tg_zlest0174-dt_chegada.
    wl_nota-TP_TRANSGENIA  = tg_zlest0174-TP_TRANSGENIA.

    IF tg_zlest0175-model = '55'.
      wl_nota-cnpjcliente = tg_zlest0175-chave_nf+06(14).
    ENDIF.

    APPEND wl_nota TO tg_notas.

  ENDLOOP.

  IF tg_notas[] IS INITIAL.
    MESSAGE 'Nenhum registro pendente de processamento!' TYPE 'S'.
    EXIT.
  ENDIF.

  tg_terminais[] = tg_zlest0174[].
  SORT tg_terminais BY cnpj_terminal_transb.
  DELETE ADJACENT DUPLICATES FROM tg_terminais COMPARING cnpj_terminal_transb.

  LOOP AT tg_terminais.

    CLEAR: tg_notas_terminal[].

    LOOP AT tg_notas INTO wl_nota WHERE cnpjferro = tg_terminais-cnpj_terminal_transb.
      APPEND wl_nota TO tg_notas_terminal.
    ENDLOOP.

    FREE zcl_rodoviario_chegada.
    CREATE OBJECT zcl_rodoviario_chegada.
    zcl_rodoviario_chegada->monta_processo( EXPORTING i_notas = tg_notas_terminal ).
    zcl_rodoviario_chegada->processar_dados( IMPORTING e_logs = tg_logs_terminal  ).

    LOOP AT tg_logs_terminal INTO DATA(_wl_log_terminal).
      APPEND _wl_log_terminal TO tg_logs.
    ENDLOOP.

    READ TABLE tg_logs_terminal INTO _wl_log_terminal INDEX 1.
    IF ( sy-subrc EQ 0 ) AND ( tg_logs_terminal[] IS NOT INITIAL ).
      LOOP AT tg_zlest0174 WHERE cnpj_terminal_transb EQ tg_terminais-cnpj_terminal_transb.

        tg_zlest0174-id_proc    = _wl_log_terminal-cont.
        tg_zlest0174-processado = 'X'.
        MODIFY zlest0174 FROM tg_zlest0174.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

  UPDATE zlest0174 SET processado = abap_true
   WHERE processado EQ abap_false.

ENDFORM.

FORM f_define_dt_chegada.

  CLEAR: vg_dt_chegada.

  vg_dt_chegada = sy-datum.

  vg_dt_chegada = '20180109'.

  IF p_dtche IS NOT INITIAL.
    vg_dt_chegada = p_dtche.
  ENDIF.

ENDFORM.

FORM f_consulta_dados_descarga_01.

  DATA: tg_terminais     TYPE TABLE OF setleaf WITH HEADER LINE.

  DATA: v_sigla_terminal TYPE zde_sigla_terminal.

  CLEAR: tg_terminais[].

  SELECT *
    FROM setleaf INTO TABLE tg_terminais
   WHERE setname = 'TERMINAIS_VLI_L1_WS'.

  LOOP AT tg_terminais.
    v_sigla_terminal = tg_terminais-valfrom.

    FREE zcl_rodo_chegada_ws.
    CREATE OBJECT zcl_rodo_chegada_ws.

    zcl_rodo_chegada_ws->set_srv_integracao( i_srv_integracao =  '01' ). "Terminais VLI
    zcl_rodo_chegada_ws->set_dt_chegada( i_dt_chegada = vg_dt_chegada ).
    zcl_rodo_chegada_ws->set_sigla_terminal( i_sigla_terminal =  v_sigla_terminal ).
    IF zcl_rodo_chegada_ws->consultar( ) EQ abap_true.
      zcl_rodo_chegada_ws->gravar_dados( ).
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM f_consulta_dados_descarga_02.

  DATA: tg_terminais     TYPE TABLE OF setleaf WITH HEADER LINE.
  DATA: wg_dias          TYPE  setleaf.

  DATA: v_sigla_terminal TYPE zde_sigla_terminal,
        v_qt_d           TYPE i,
        v_dias           TYPE i,
        v_dt_chegada     TYPE sy-datum.

  CLEAR: tg_terminais[], wg_dias.


  SELECT *
    FROM setleaf INTO TABLE tg_terminais
   WHERE setname = 'TERMINAIS_BUN_L1_WS'.

  SELECT SINGLE *
    FROM setleaf INTO wg_dias
   WHERE setname = 'MAGGI_ZLESR0149'.

  v_qt_d   = wg_dias-valfrom.
  v_dias   = wg_dias-valfrom.

  LOOP AT tg_terminais.
    v_sigla_terminal = tg_terminais-valfrom.

    FREE zcl_rodo_chegada_ws.
    CREATE OBJECT zcl_rodo_chegada_ws.

    IF vg_job EQ 1.
      DO v_qt_d  TIMES.
        CLEAR v_dt_chegada.
        v_dt_chegada = sy-datum - v_dias.

        zcl_rodo_chegada_ws->set_srv_integracao( i_srv_integracao =  '02' ). "Terminais BUNGE
        zcl_rodo_chegada_ws->set_dt_chegada( i_dt_chegada = v_dt_chegada ).
        zcl_rodo_chegada_ws->set_sigla_terminal( i_sigla_terminal =  v_sigla_terminal ).
        IF zcl_rodo_chegada_ws->consultar( ) EQ abap_true.
          zcl_rodo_chegada_ws->gravar_dados( ).
        ENDIF.

        v_dias = v_dias - 1.
      ENDDO.
    ELSE.
      zcl_rodo_chegada_ws->set_srv_integracao( i_srv_integracao =  '02' ). "Terminais BUNGE
      zcl_rodo_chegada_ws->set_dt_chegada( i_dt_chegada = vg_dt_chegada ).
      zcl_rodo_chegada_ws->set_sigla_terminal( i_sigla_terminal =  v_sigla_terminal ).
      IF zcl_rodo_chegada_ws->consultar( ) EQ abap_true.
        zcl_rodo_chegada_ws->gravar_dados( ).
      ENDIF.

    ENDIF.

  ENDLOOP.

  CLEAR: v_qt_d, v_dias, v_dt_chegada .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_DADOS_DESCARGA_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_consulta_dados_descarga_03.
  DATA: integ_dados TYPE REF TO zcl_integ_rodv.
  DATA: e_dados_modal_t TYPE zde_dados_modal_t.
  DATA: s_sig_terminal TYPE zde_sigla_terminal.
  DATA zcl_integ_rodv TYPE REF TO zcl_integ_rodv.
  CREATE OBJECT zcl_integ_rodv.
  CREATE OBJECT integ_dados.
  FREE zcl_rodo_chegada_ws.
  CREATE OBJECT zcl_rodo_chegada_ws.


  DATA: tg_terminais TYPE TABLE OF setleaf WITH HEADER LINE,
        t_term       TYPE STANDARD TABLE OF  rgsb4.
  DATA: v_sigla_terminal TYPE zde_sigla_terminal,
        txt_terminal     TYPE char40.
  CLEAR:           tg_terminais[].

*  SELECT *
*    FROM setleaf INTO TABLE tg_terminais
*   WHERE setname = 'TERMINAIS_LOGONE_L1_WS'.
*
*  IF tg_terminais IS NOT INITIAL.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class           = '0000'
      setnr           = 'TERMINAIS_LOGONE_L1_WS'
      no_descriptions = ' '
    TABLES
      set_values      = t_term
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

*-#137148-01.04.2024-JT-inicio
*--------------------------------
*-set dados para pesquisa
*--------------------------------
  s_sig_terminal = 'R'.
  zcl_integ_rodv->zif_integracao_rodov~set_srv_integracao( i_srv_integracao = '03' ).
  zcl_integ_rodv->zif_integracao_rodov~set_sig_terminal( i_sig_terminal = s_sig_terminal ).
  zcl_integ_rodv->zif_integracao_rodov~set_data_chegada( i_data_chegada = vg_dt_chegada ).

*--------------------------------
*-pesquisa terminal TGG
*--------------------------------
  zcl_integ_rodv->zif_integracao_rodov~get_int_rod(
    EXPORTING
      i_servico             =  'RV'   " IS-H: campo geral de comprimento 2 para módulos de função
    IMPORTING
      e_data                = e_dados_modal_t ).

  CHECK e_dados_modal_t[] IS NOT INITIAL.

*--------------------------------
*-tratar CNPJ nao encontrado
*--------------------------------
  LOOP AT e_dados_modal_t INTO DATA(w_dados_modal).
    READ TABLE t_term   INTO DATA(w_term) WITH KEY title = w_dados_modal-cnpjempresa.
    IF sy-subrc <> 0.
      READ TABLE t_term INTO w_term WITH KEY from = 'TGGROD'.
      w_dados_modal-cnpjempresa = w_term-title.
      MODIFY e_dados_modal_t FROM w_dados_modal TRANSPORTING cnpjempresa.
    ENDIF.
  ENDLOOP.
*-#137148-01.04.2024-JT-fim

  LOOP AT t_term INTO DATA(wa_term).
    CLEAR: txt_terminal, v_sigla_terminal.
    v_sigla_terminal = wa_term-from.
    txt_terminal     = wa_term-title.

    zcl_rodo_chegada_ws->set_srv_integracao( i_srv_integracao =  '03' ). "Terminais TGG
    zcl_rodo_chegada_ws->set_dt_chegada( i_dt_chegada = vg_dt_chegada ).
    zcl_rodo_chegada_ws->set_sigla_terminal( i_sigla_terminal =  v_sigla_terminal ). "'TGGROD' ).
    CLEAR: s_sig_terminal.
    s_sig_terminal = 'R'.

*  Inicio USER STORY 74070 - Anderson Oenning / 02/05/2022IF zcl_rodo_chegada_ws->consultar( ) EQ abap_true.
    zcl_integ_rodv->zif_integracao_rodov~set_srv_integracao( i_srv_integracao = '03' ).
    zcl_integ_rodv->zif_integracao_rodov~set_sig_terminal( i_sig_terminal = s_sig_terminal ).
    zcl_integ_rodv->zif_integracao_rodov~set_sigla_terminal( i_sigla_terminal = v_sigla_terminal ).
    zcl_integ_rodv->zif_integracao_rodov~set_desc_terminal( i_text = txt_terminal ).
    zcl_integ_rodv->zif_integracao_rodov~set_data_chegada( i_data_chegada = vg_dt_chegada ).
*-#137148-01.04.2024-JT-inicio
    zcl_integ_rodv->zif_integracao_rodov~set_cnpj_terminal( i_cnpj_terminal = CONV #( txt_terminal ) ).
*-#137148-01.04.2024-JT-fim

*-#137148-01.04.2024-JT-inicio
*   zcl_integ_rodv->zif_integracao_rodov~get_int_rod(
*     EXPORTING
*       i_servico             =  'RV'   " IS-H: campo geral de comprimento 2 para módulos de função
*     IMPORTING
*       e_data                = e_dados_modal_t ).
*-#137148-01.04.2024-JT-fim

    TRY.
        zcl_integ_rodv->zif_integracao_rodov~trata_retorno( i_dados = e_dados_modal_t ).
        zcl_integ_rodv->zif_integracao_rodov~salvar_dados( ).
      CATCH zcx_integracao INTO DATA(ws_integracao).

      CATCH cx_sy_range_out_of_bounds INTO DATA(ws_cx_sy_range).
    ENDTRY.

  ENDLOOP.
  "Inicio USER STORY 74070 - Anderson Oenning / 02/05/2022

*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_DADOS_DESCARGA_04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_consulta_dados_descarga_04.

  DATA: tg_terminais    TYPE TABLE OF setleaf WITH HEADER LINE,
        tg_cnpj_grp_neg TYPE TABLE OF setleaf WITH HEADER LINE.

  DATA: v_sigla_terminal TYPE zde_sigla_terminal,
        v_cnpj_grupo_neg TYPE lfa1-stcd1.

  CLEAR: tg_terminais[],tg_cnpj_grp_neg[].

  SELECT *
    FROM setleaf INTO TABLE tg_terminais
   WHERE setname = 'TERMINAIS_RUMO_L1_WS'.

  SELECT *
    FROM setleaf INTO TABLE tg_cnpj_grp_neg
   WHERE setname = 'CNPJ_GRP_NEG_RUMO_L1_WS'.

  LOOP AT tg_terminais.
    v_sigla_terminal = tg_terminais-valfrom.

    LOOP AT tg_cnpj_grp_neg.

      v_cnpj_grupo_neg = tg_cnpj_grp_neg-valfrom.

      FREE zcl_rodo_chegada_ws.
      CREATE OBJECT zcl_rodo_chegada_ws.

      zcl_rodo_chegada_ws->set_srv_integracao( i_srv_integracao =  '04' ). "Terminais Rumo
      zcl_rodo_chegada_ws->set_dt_chegada( i_dt_chegada = vg_dt_chegada ).
      zcl_rodo_chegada_ws->set_sigla_terminal( i_sigla_terminal = v_sigla_terminal ).
      zcl_rodo_chegada_ws->set_cnpj_grupo_neg( i_cnpj_grupo_neg = v_cnpj_grupo_neg ).

      IF zcl_rodo_chegada_ws->consultar( ) EQ abap_true.
        zcl_rodo_chegada_ws->gravar_dados( ).
      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
FORM f_consulta_dados_descarga_06.

  DATA: tg_terminais     TYPE TABLE OF setleaf WITH HEADER LINE,
        v_sigla_terminal TYPE zde_sigla_terminal.

  CLEAR: tg_terminais[],v_sigla_terminal.

  SELECT *
    FROM setleaf INTO TABLE tg_terminais
   WHERE setname = 'TERMINAIS_ROCHA_L1_WS'.
  IF sy-subrc <> 0.
    CLEAR tg_terminais[].
  ENDIF.

  DATA i_dt_inicial TYPE zde_dt_inicial.
  DATA i_dt_final   TYPE zde_dt_final.

  i_dt_inicial = i_dt_final = vg_dt_chegada.

  LOOP AT tg_terminais.
    v_sigla_terminal = tg_terminais-valfrom.

    FREE zcl_rodo_chegada_ws.
    CREATE OBJECT zcl_rodo_chegada_ws.

    zcl_rodo_chegada_ws->set_srv_integracao( i_srv_integracao =  '06' ). "Terminais Rocha
    zcl_rodo_chegada_ws->set_dt_inicial( i_dt_inicial ).
    zcl_rodo_chegada_ws->set_dt_final( i_dt_final ).
    zcl_rodo_chegada_ws->set_sigla_terminal( i_sigla_terminal = v_sigla_terminal ).

    IF zcl_rodo_chegada_ws->consultar( ) = abap_true.
      zcl_rodo_chegada_ws->gravar_dados( ).
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_DADOS_DESCARGA_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_consulta_dados_descarga_05 .
  DATA: tg_terminais    TYPE TABLE OF setleaf WITH HEADER LINE,
        tg_cnpj_grp_neg TYPE TABLE OF setleaf WITH HEADER LINE.

  DATA: v_sigla_terminal TYPE zde_sigla_terminal,
        v_cnpj_grupo_neg TYPE lfa1-stcd1.

  CLEAR: tg_terminais[],tg_cnpj_grp_neg[].

  SELECT *
    FROM setleaf INTO TABLE tg_terminais
   WHERE setname = 'TERMINAIS_BRADO_L1_WS'.

  SELECT *
    FROM setleaf INTO TABLE tg_cnpj_grp_neg
   WHERE setname = 'CNPJ_GRP_NEG_BRADO_L1_WS'.

  LOOP AT tg_terminais.
    v_sigla_terminal = tg_terminais-valfrom.

    LOOP AT tg_cnpj_grp_neg.

      v_cnpj_grupo_neg = tg_cnpj_grp_neg-valfrom.

      FREE zcl_rodo_chegada_ws.
      CREATE OBJECT zcl_rodo_chegada_ws.

      zcl_rodo_chegada_ws->set_srv_integracao( i_srv_integracao =  '05' ). "Terminais Brado
      zcl_rodo_chegada_ws->set_dt_chegada( i_dt_chegada = vg_dt_chegada ).
      zcl_rodo_chegada_ws->set_sigla_terminal( i_sigla_terminal = v_sigla_terminal ).
      zcl_rodo_chegada_ws->set_cnpj_grupo_neg( i_cnpj_grupo_neg = v_cnpj_grupo_neg ).

      IF zcl_rodo_chegada_ws->consultar( ) EQ abap_true.
        zcl_rodo_chegada_ws->gravar_dados( ).
      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
