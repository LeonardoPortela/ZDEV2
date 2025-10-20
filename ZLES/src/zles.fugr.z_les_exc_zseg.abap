FUNCTION Z_LES_EXC_ZSEG.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_PLACA) TYPE  ZPLACA
*"     VALUE(I_CK_CONSULTA) TYPE  ZDE_CK_RNTRC_CS OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_CONSULTAS) TYPE  ZLEST0135_T
*"     REFERENCE(E_ZLEST0135) TYPE  ZLEST0135
*"     REFERENCE(E_STATUS) TYPE  SYST_SUBRC
*"     REFERENCE(E_MSG_ERRO) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: it_zlest0002 TYPE TABLE OF zlest0002,
        consulta_api TYPE char01, "Consutar API.
        qtd_hora     TYPE p,
        qtde_min     TYPE p,
        l_ret_date   TYPE tvpod-rudat,
        l_ret_time   TYPE tvpod-rutim,
        lv_data_lim  TYPE char14,
        lv_data_cons TYPE char14,
        ws_zlest0002 TYPE zlest0002.

*===  "Legenda do status retorno=========================
*0 - Não deve validar zseg, e devemos zera-la
*1 - Erro na API
*2 - Tem que valizar zseg
*3 - Tip não retornou o resultado esperado na consulta
*  ======================================================



  DATA: lc_webservice TYPE REF TO zcl_webservice_tipcard.
  CREATE OBJECT lc_webservice.

  CLEAR: e_status, consulta_api, ws_zlest0002, e_zlest0135.

  SELECT SINGLE *
    FROM TVARVC INTO @DATA(LWA_CHECK_SIT_TRANSP)
   WHERE NAME EQ 'FAT_NO_CHECK_SITUACAO_TRANSP'
     AND LOW  EQ @ABAP_TRUE.

  IF SY-SUBRC EQ 0.
    e_status = 0.
    EXIT.
  ENDIF.


*--------------------------------------------------------------------------------------------------------*
*   "Verificar se existe dados do transportador.
*--------------------------------------------------------------------------------------------------------*


  SELECT SINGLE * FROM zlest0002
    INTO ws_zlest0002
    WHERE pc_veiculo EQ i_placa.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM zlest0135
      INTO e_zlest0135
      WHERE cd_transportador EQ ws_zlest0002-proprietario
        AND ds_placa EQ ws_zlest0002-pc_veiculo.

    IF e_zlest0135-tp_transportador IS NOT INITIAL.
      IF e_zlest0135-dt_atualizacao <> sy-datum. "Verificar se tem informação do dia atual.
        consulta_api = abap_true.
      ELSE.


        "Verificar se hora que esta na tabela é maior que 6 hora, se for maior devera consultar novamente a API.
        CALL FUNCTION 'TSTR_CALC_TIME'
          EXPORTING
            iv_begin_datelocal_req   = sy-datum
            iv_begin_timelocal_req   = sy-uzeit
            iv_duration_integer      = 3600 "Total de 1 horas a menor do que a data atual e hora.
            iv_direction             = '-'
          IMPORTING
            ev_end_datelocal         = l_ret_date
            ev_end_timelocal         = l_ret_time
          EXCEPTIONS
            fatal_error              = 1
            time_invalid             = 2
            time_missing             = 3
            tstream_not_loadable     = 4
            tstream_generation_error = 5
            parameter_error          = 6
            unspecified_error        = 7
            OTHERS                   = 8.

        lv_data_lim = |{ l_ret_date }{ l_ret_time }|.
        lv_data_cons  = |{ e_zlest0135-dt_atualizacao }{ e_zlest0135-hr_atualizacao }|.




        IF  lv_data_cons > lv_data_lim.
          consulta_api = abap_false.
        ELSE.
          consulta_api = abap_true.
        ENDIF.
      ENDIF.
    ELSE.
      consulta_api = abap_true.
    ENDIF.
  ENDIF.

  IF consulta_api EQ abap_true.
    "Consultar situação do transportador.
    CLEAR: e_zlest0135.
    FREE: e_consultas, e_zlest0135.
    lc_webservice->cons_situacao_transportador(
      EXPORTING
        i_placa          =  i_placa   " Placa veículo
        i_ck_consulta    = i_ck_consulta
      RECEIVING
        e_consultas      =  e_consultas " Tabela de Consultas Transportador
      EXCEPTIONS
        erro             = 1
        webservice       = 2
        OTHERS           = 3
    ).

    IF sy-subrc NE 0.
      e_status = 1.
      e_msg_erro = VALUE #(
      type = 'E'
       message_v1 = |Não foi possivel consultar situação transportador|
       message_v2 = |Placa : { i_placa }. Contate a Transportadora sua|
       message_v3 = |região!| ).
      EXIT.
    ENDIF.

    READ TABLE e_consultas INTO e_zlest0135 INDEX 1.
  ENDIF.

  IF e_zlest0135-tp_transportador IS NOT INITIAL.
    CASE e_zlest0135-tp_transportador.
      WHEN 1. "TAC
        e_status = 0.
      WHEN 2. "ETC
        IF e_zlest0135-ck_etc_equiparado EQ abap_true.
          e_status = 0.
        ELSE.
          e_status = 2.
        ENDIF.
      WHEN 3. "CTC
        e_status = 0.
      WHEN OTHERS.
        e_status = 2.
    ENDCASE.
  ELSE.

*--------------------------------------------------------------------------------------------------------*
*   "Neste ponto realizar a consulta na base da TIP.
*--------------------------------------------------------------------------------------------------------*

    "Consultar situação do transportador.
    CLEAR: e_zlest0135.
    FREE: e_consultas, e_zlest0135.
    lc_webservice->cons_status_parceiro(
      EXPORTING
        i_placa          =  i_placa   " Placa veículo
      RECEIVING
        e_consultas      =  e_consultas " Tabela de Consultas Transportador
      EXCEPTIONS
        erro             = 1
        webservice       = 2
        OTHERS           = 3
    ).

    IF sy-subrc NE 0.
      e_status = 1.
      e_msg_erro = VALUE #(
      type = 'E'
       message_v1 = |Não foi possivel consultar situação transportador|
       message_v2 = |Placa : { i_placa }. Contate a Transportadora sua|
       message_v3 = |região!| ).
      EXIT.
    ENDIF.

    READ TABLE e_consultas INTO e_zlest0135 INDEX 1.
    IF e_zlest0135-tp_transportador IS NOT INITIAL.
      CASE e_zlest0135-tp_transportador.
        WHEN 1. "TAC
          e_status = 0.
        WHEN 2. "ETC
          IF e_zlest0135-ck_etc_equiparado EQ abap_true.
            e_status = 0.
          ELSE.
            e_status = 2.
          ENDIF.
        WHEN 3. "CTC
          e_status = 0.
        WHEN OTHERS.
          e_status = 2.
      ENDCASE.

*--------------------------------------------------------------------------------------------------------*
*   "Check data hora atualizada envio email.
*--------------------------------------------------------------------------------------------------------*
      lc_webservice->check_data_envio_email( e_consultas = e_zlest0135 ).

    ELSE.
      e_status = 3.
      e_msg_erro = VALUE #(
      type = 'E'
       message_v1 = |Não foi possivel consultar situação transportador|
       message_v2 = |Placa : { i_placa }. Contate a Transportadora sua|
       message_v3 = |região!| ).
      EXIT.
    ENDIF.
  ENDIF.

ENDFUNCTION.
