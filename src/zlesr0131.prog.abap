*&---------------------------------------------------------------------*
*& Report  ZLESR0128
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0131.

DATA: zcl_ferro_chegada_ws TYPE REF TO zcl_ferroviario_chegada_ws.

*----------------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------------*

DATA: vg_dt_chegada TYPE zlest0179-dt_chegada.

TABLES: zlest0179.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_dtche  TYPE zlest0179-dt_chegada,
            p_tpproc TYPE c NO-DISPLAY. " 1 - Consulta - 2 Processa
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.

  DATA: vg_job      TYPE i.

  DATA:vg_inter TYPE zlest0174-srv_integracao.

  SELECT SINGLE COUNT( * ) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'MAGGI_ZLES0170_DAY'
     AND status EQ 'R'.

  CHECK ( vg_job EQ 1 ) OR
        ( p_dtche IS NOT INITIAL ) OR
        ( p_tpproc EQ '2' ). "Processamento

  PERFORM: f_define_dt_chegada,
           f_consulta_dados_descarga,
           f_processa_dados_descarga.

FORM f_consulta_dados_descarga.

  CHECK ( p_tpproc EQ '1'     ) OR "Consulta
        ( p_tpproc IS INITIAL ).

  IMPORT vg_inter FROM MEMORY ID 'VG_INTER'.

  IF NOT vg_inter IS INITIAL.

    IF vg_inter = '01'.
      PERFORM: f_consulta_dados_descarga_01. "Terminais VLI
    ELSEIF vg_inter = '02'.
      PERFORM: f_consulta_dados_descarga_02. "Terminais BUNGE
    ELSEIF vg_inter = '03'.
      PERFORM: f_consulta_dados_descarga_03. "Terminais TGG
    ENDIF.

  ELSE.
    PERFORM: f_consulta_dados_descarga_01. "Terminais VLI
    PERFORM: f_consulta_dados_descarga_02. "Terminais BUNGE
    PERFORM: f_consulta_dados_descarga_03. "Terminais TGG
  ENDIF.
ENDFORM.

FORM f_processa_dados_descarga.

  TYPES: BEGIN OF ty_notas,
           nr_nf_propria TYPE j_1bnfnum9,
           serie_propria TYPE j_1bseries.
           INCLUDE STRUCTURE zlest0019_l3_30.
         TYPES: END OF ty_notas.

  DATA: tg_zlest0179      TYPE TABLE OF zlest0179 WITH HEADER LINE,
        tg_zlest0180      TYPE TABLE OF zlest0180 WITH HEADER LINE,
        tg_zlest0180_aux  TYPE TABLE OF zlest0180 WITH HEADER LINE,
        tg_lfa1           TYPE TABLE OF lfa1      WITH HEADER LINE,
        wl_nota           TYPE ty_notas,
        tg_notas_processo TYPE TABLE OF ty_notas WITH HEADER LINE,
        tg_notas          TYPE TABLE OF ty_notas WITH HEADER LINE,
        tg_logs           TYPE zlest0176_t,
        tg_logs_processo  TYPE zlest0176_t,
        v_id_proc         TYPE zlest0176-cont.

  DATA: v_dcl         TYPE zdcl,
        v_branch      TYPE j_1bnfdoc-branch,
        v_nfe         TYPE j_1bnfdoc-nfenum,
        v_serie       TYPE zseriedcl,
        v_id_vagao    TYPE zidvagao,
        v_id_vagao_c7 TYPE c LENGTH 7,
        v_cnpj        TYPE zcnpjferro.

  DATA: zcl_ferroviario_chegada TYPE REF TO zcl_ferroviario_chegada.

  CLEAR: tg_zlest0179[], tg_zlest0180[], tg_notas[], tg_logs[].

  CHECK ( p_tpproc EQ '2'     ) OR "Processar
        ( p_tpproc IS INITIAL ).

  "Dados descarga Ferroviario - Integração WS
  SELECT *
    FROM zlest0179 INTO TABLE tg_zlest0179
   WHERE processado EQ abap_false.

  CHECK tg_zlest0179[] IS NOT INITIAL.

  "Tabela de dados descarga Ferroviario - Notas - Integração WS
  SELECT *
    FROM zlest0180 INTO TABLE tg_zlest0180
     FOR ALL ENTRIES IN tg_zlest0179
   WHERE id_registro EQ tg_zlest0179-id_registro.

  CHECK tg_zlest0180[] IS NOT INITIAL.

  tg_zlest0180_aux[] = tg_zlest0180[].
  DELETE tg_zlest0180_aux WHERE stcd1 IS INITIAL.
  SORT tg_zlest0180_aux BY stcd1.
  DELETE ADJACENT DUPLICATES FROM tg_zlest0180_aux COMPARING stcd1.

  IF tg_zlest0180_aux[] IS NOT INITIAL.
    SELECT *
      FROM lfa1 INTO TABLE tg_lfa1
       FOR ALL ENTRIES IN tg_zlest0180_aux
     WHERE stcd1 EQ tg_zlest0180_aux-stcd1.

    PERFORM f_elimina_lfa1_bloq TABLES tg_lfa1.
  ENDIF.

  SORT tg_zlest0179 BY id_registro.

  LOOP AT tg_zlest0179.

    CLEAR: v_dcl, v_serie, v_id_vagao, v_cnpj, tg_notas_processo[], tg_notas_processo.
    CLEAR: v_branch, v_nfe.


    v_dcl          = tg_zlest0179-nr_cte.
    v_serie        = tg_zlest0179-serie_cte.

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

    v_id_vagao_c7  = tg_zlest0179-idvagao.
    v_id_vagao_c7  = |{ v_id_vagao_c7 ALPHA = IN } |.
    v_id_vagao     = tg_zlest0179-serie_vagao && v_id_vagao_c7 && 'T'.

    CONCATENATE v_id_vagao 'T' INTO v_id_vagao.

    v_cnpj         = tg_zlest0179-cnpj_terminal_destino.


    LOOP AT tg_zlest0180 WHERE id_registro EQ tg_zlest0179-id_registro.

      CLEAR: wl_nota.

      CHECK tg_zlest0180-stcd1 IS NOT INITIAL.

      wl_nota-nfenum          = tg_zlest0180-nfenum.
      wl_nota-cnpjcliente     = tg_zlest0180-stcd1.

      v_nfe = tg_zlest0180-nfenum.


      LOOP AT tg_lfa1 WHERE stcd1 = tg_zlest0180-stcd1.
        wl_nota-cod_fornecedor = tg_lfa1-lifnr.

        IF tg_lfa1-ktokk = 'ZFIC'.
          SELECT SINGLE *
            FROM j_1bbranch INTO @DATA(_wl_branch)
           WHERE branch = @tg_lfa1-lifnr+6(4).

          IF sy-subrc EQ 0.
            v_branch                = _wl_branch-branch.
            wl_nota-bukrs           = _wl_branch-bukrs.
            wl_nota-branch          = _wl_branch-branch.
          ENDIF.
        ENDIF.
      ENDLOOP.

      wl_nota-dtadecarga      = tg_zlest0179-dt_chegada.
      wl_nota-dtachegada      = tg_zlest0179-dt_chegada.
      wl_nota-pesodvagao      = tg_zlest0180-peso_rateado.
      wl_nota-nr_nf_propria   = tg_zlest0180-nfenum.
      wl_nota-serie_propria   = tg_zlest0180-series.

      APPEND wl_nota TO tg_notas_processo.
    ENDLOOP.

    FREE zcl_ferroviario_chegada.
    CREATE OBJECT zcl_ferroviario_chegada.

    zcl_ferroviario_chegada->monta_processo( i_dcl        = v_dcl
                                             i_serie      = v_serie
                                             i_id_vagao   = v_id_vagao
                                             i_cnpj       = v_cnpj
                                             i_branch     = v_branch
                                             i_nfe        = v_nfe
                                              ).

    LOOP AT tg_notas_processo INTO DATA(_wl_nota_processo).

      TRY.
          zcl_ferroviario_chegada->set_qt_chegada(
                    EXPORTING
                      i_bukrs         =  _wl_nota_processo-bukrs
                      i_branch        =  _wl_nota_processo-branch
                      i_nr_nf_propria =  _wl_nota_processo-nr_nf_propria
                      i_serie_propria =  _wl_nota_processo-serie_propria
                      i_qt_chegada    =  _wl_nota_processo-pesodvagao ).

        CATCH zcx_ferroviario_chegada INTO DATA(cx_ferroviario_chegada).

          PERFORM f_gravar_log CHANGING v_id_proc.

          sy-msgid = cx_ferroviario_chegada->msgid.
          sy-msgno = cx_ferroviario_chegada->msgno.
          sy-msgty = cx_ferroviario_chegada->msgty.
          sy-msgv1 = cx_ferroviario_chegada->msgv1.
          sy-msgv2 = cx_ferroviario_chegada->msgv2.
          sy-msgv3 = cx_ferroviario_chegada->msgv3.
          sy-msgv4 = cx_ferroviario_chegada->msgv4.

          UPDATE zlest0179 SET processado = abap_true
                               id_proc    = v_id_proc
           WHERE id_registro EQ tg_zlest0179-id_registro.

          CONTINUE.
      ENDTRY.

    ENDLOOP.

    READ TABLE tg_notas_processo INTO DATA(_nota_processo) INDEX 1.
    IF ( sy-subrc EQ 0 ) AND ( _nota_processo-dtachegada IS NOT INITIAL ).
      zcl_ferroviario_chegada->set_dt_chegada( i_dt_chegada = _nota_processo-dtachegada ).
    ENDIF.

    IF zcl_ferroviario_chegada->ck_alterou EQ abap_true.
      IF zcl_ferroviario_chegada->get_id_refkey( ) IS INITIAL.
        zcl_ferroviario_chegada->set_id_refkey( i_id_refkey = zcl_ferroviario=>get_new_id_refkey( ) ).
      ENDIF.
      IF zcl_ferroviario_chegada->validar_registro( ) EQ abap_true.
        zcl_ferroviario_chegada->gravar_registro( ).
      ELSE.
        PERFORM f_gravar_log CHANGING v_id_proc.

        UPDATE zlest0179 SET processado = abap_true
                             erro_proc  = abap_true
                             id_proc    = v_id_proc
         WHERE id_registro EQ tg_zlest0179-id_registro.
      ENDIF.
    ENDIF.

  ENDLOOP.

  UPDATE zlest0179 SET processado = abap_true
   WHERE processado EQ abap_false.


ENDFORM.

FORM f_define_dt_chegada.

  CLEAR: vg_dt_chegada.

  vg_dt_chegada = sy-datum.

  vg_dt_chegada = '20180909'.

  IF p_dtche IS NOT INITIAL.
    vg_dt_chegada = p_dtche.
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

FORM f_consulta_dados_descarga_01.

  DATA: tg_terminais     TYPE TABLE OF setleaf WITH HEADER LINE.

  DATA: v_sigla_terminal TYPE zde_sigla_terminal.

  CLEAR: tg_terminais[].

  SELECT *
    FROM setleaf INTO TABLE tg_terminais
   WHERE setname = 'TERMINAIS_VLI_L3_WS'.

  LOOP AT tg_terminais.
    v_sigla_terminal = tg_terminais-valfrom.

    FREE zcl_ferro_chegada_ws.
    CREATE OBJECT zcl_ferro_chegada_ws.

    zcl_ferro_chegada_ws->set_srv_integracao( i_srv_integracao =  '01' ). "Terminais VLI
    zcl_ferro_chegada_ws->set_dt_chegada( i_dt_chegada = vg_dt_chegada ).
    zcl_ferro_chegada_ws->set_sigla_terminal( i_sigla_terminal = v_sigla_terminal ).
    IF zcl_ferro_chegada_ws->consultar( ) EQ abap_true.
      zcl_ferro_chegada_ws->gravar_dados( ).
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM f_consulta_dados_descarga_02.

  DATA: tg_terminais     TYPE TABLE OF setleaf WITH HEADER LINE.

  DATA: v_sigla_terminal TYPE zde_sigla_terminal.

  CLEAR: tg_terminais[].

  SELECT *
    FROM setleaf INTO TABLE tg_terminais
   WHERE setname = 'TERMINAIS_BUN_L3_WS'.

  LOOP AT tg_terminais.
    v_sigla_terminal = tg_terminais-valfrom.

    FREE zcl_ferro_chegada_ws.
    CREATE OBJECT zcl_ferro_chegada_ws.

    zcl_ferro_chegada_ws->set_srv_integracao( i_srv_integracao =  '02' ). "Terminais Bunge
    zcl_ferro_chegada_ws->set_dt_chegada( i_dt_chegada = vg_dt_chegada ).
    zcl_ferro_chegada_ws->set_sigla_terminal( i_sigla_terminal = v_sigla_terminal ).
    IF zcl_ferro_chegada_ws->consultar( ) EQ abap_true.
      zcl_ferro_chegada_ws->gravar_dados( ).
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONSULTA_DADOS_DESCARGA_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_consulta_dados_descarga_03 .
  DATA: tg_terminais     TYPE TABLE OF setleaf WITH HEADER LINE.
  DATA t_term       TYPE STANDARD TABLE OF  rgsb4.
  DATA: integ_dados TYPE REF TO zcl_integ_rodv.
  DATA: e_dados_modal_t TYPE zde_dados_modal_t.
  DATA txt_terminal     TYPE char40.
  CREATE OBJECT integ_dados.


  DATA: s_sig_terminal TYPE zde_sigla_terminal.
  DATA zcl_integ_rodv TYPE REF TO zcl_integ_rodv.
  CREATE OBJECT zcl_integ_rodv.

  DATA: v_sigla_terminal TYPE zde_sigla_terminal.
  CLEAR: tg_terminais[].

*  SELECT *
*    FROM setleaf INTO TABLE tg_terminais
*   WHERE setname = 'TERMINAIS_LOGONE_L3_WS'.

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

  LOOP AT t_term INTO DATA(wa_term).
    CLEAR: txt_terminal, v_sigla_terminal.
    v_sigla_terminal = wa_term-from.
    txt_terminal     = wa_term-title.

*    v_sigla_terminal = tg_terminais-valfrom.

    CLEAR: s_sig_terminal.
    s_sig_terminal = 'F'.

    TRY .

*  Inicio USER STORY 74070 - Anderson Oenning / 02/05/2022IF
        zcl_integ_rodv->zif_integracao_rodov~set_srv_integracao( i_srv_integracao = '03' ).

        zcl_integ_rodv->zif_integracao_rodov~set_sig_terminal( i_sig_terminal = s_sig_terminal ).
        zcl_integ_rodv->zif_integracao_rodov~set_sigla_terminal( i_sigla_terminal = v_sigla_terminal ).
        zcl_integ_rodv->zif_integracao_rodov~set_desc_terminal( i_text = txt_terminal ).
        zcl_integ_rodv->zif_integracao_rodov~set_data_chegada( i_data_chegada = vg_dt_chegada ).
        zcl_integ_rodv->zif_integracao_rodov~get_int_rod(
          EXPORTING
            i_servico             =  'RV'   " IS-H: campo geral de comprimento 2 para módulos de função
          IMPORTING
            e_data                = e_dados_modal_t
        ).zcl_integ_rodv->zif_integracao_rodov~trata_retorno( i_dados = e_dados_modal_t
        ).zcl_integ_rodv->zif_integracao_rodov~preparar_dados_gravacao(
        ).zcl_integ_rodv->zif_integracao_rodov~salvar_dados( ).
      CATCH zcx_integracao INTO DATA(ws_integracao).
      CATCH cx_sy_range_out_of_bounds INTO DATA(ws_sy_range_out_of_bounds).


    ENDTRY.
  ENDLOOP.
ENDFORM.
