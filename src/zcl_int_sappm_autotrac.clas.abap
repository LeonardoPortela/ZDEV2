class ZCL_INT_SAPPM_AUTOTRAC definition
  public
  final
  create public .

public section.

  class-methods I_CONS_CADASTRO_VEIC
    exporting
      !T_CADASTRO_VEIC type ZPME0056 .
  class-methods I_CONS_DADOS_ABAST
    importing
      value(T_DADOS_VEIC) type ZPME0056 optional
      value(I_DATA) type SY-DATUM optional
      value(I_HR_INIC) type SY-UZEIT optional
      value(I_HR_FIM) type SY-UZEIT
      value(I_ID) type ZCODE optional
      value(I_DATA_FIM) type SY-DATUM optional
    exporting
      value(T_DADOS_ABAST_VEIC) type ZPME0057 .
  class-methods I_CONS_LOGS
    importing
      value(I_FROTA) type ZRSDSSELOPTS optional
      value(I_EQUNR) type ZRSDSSELOPTS optional
      value(I_CENTRO) type ZRSDSSELOPTS optional
      value(I_PERIDO_INIC) type SY-DATUM optional
      value(I_PERIDO_FIM) type SY-DATUM optional
      value(I_STATUS) type ZRSDSSELOPTS
    exporting
      value(T_LOGS) type ZPME0060_T .
  class-methods M_REPROC_INFORM
    importing
      !W_ZPME0060 type ZPME0060
      !I_EQUNR type EQUNR
      !P_SUB_CONT type CHAR1 .
  class-methods M_REGIST_JUSTIF
    importing
      value(I_LINE) type CHAR72 optional
      value(W_ZPME0060) type ZPME0060 optional
      value(I_ODOMETRO) type IMRC_CNTRC optional .
  class-methods M_CLEAR_ID_ITEM
    importing
      !W_ZPME0060 type ZPME0060 .
  class-methods M_CHECK_PONT_MED
    importing
      !I_POINT type IMRC_POINT
      !I_DATE type SY-DATUM
      !I_TIME type SY-UZEIT
    exporting
      !E_VALUE type IMRC_CNTRC
      !E_DATA type IMRG .
  class-methods I_CONS_COMBUST
    importing
      value(I_PLACA) type ZRSDSSELOPTS optional
      value(I_EQUNR) type ZRSDSSELOPTS optional
      value(I_CENTRO) type ZRSDSSELOPTS optional
      value(I_PERIDO_INIC) type SY-DATUM optional
      value(I_PERIDO_FIM) type SY-DATUM optional
      value(I_COMB) type VRM_VALUE-TEXT optional
    exporting
      value(T_COMB) type ZPME0062_T .
  class-methods I_CONS_COMB_SINT
    importing
      value(I_PLACA) type ZRSDSSELOPTS optional
      value(I_EQUNR) type ZRSDSSELOPTS optional
      value(I_CENTRO) type ZRSDSSELOPTS optional
      value(I_PERIDO_INIC) type SY-DATUM optional
      value(I_PERIDO_FIM) type SY-DATUM optional
      value(I_COMB) type VRM_VALUE-TEXT optional
    exporting
      value(T_COMB) type ZPME0063_T .
  class-methods M_GET_DT_HR_PROC
    exporting
      value(I_HR_FIM) type SY-UZEIT
      value(I_HORA) type SY-UZEIT
      value(I_ID) type ZCODE
      value(I_DATA) type SY-DATUM
      !I_DATA_FIM type SY-DATUM .
  class-methods M_ESTORNO_DOC_MED
    importing
      value(E_DOC_MED) type IMRC_MDOCM
    exporting
      value(R_RETURN) type STRING
      value(E_MSGTY) type SYST_MSGTY .
  class-methods GET_DADOS_P_MIRIAN
    importing
      !I_PLACA type ZRSDSSELOPTS
      !I_PERIDO_INIC type SY-DATUM
      !I_PERIDO_FIM type SY-DATUM
    exporting
      !T_COMB type ZPME0065_T .
  class-methods GET_DADOS_LOCAL
    importing
      !I_CODE type CHAR15
      !I_DATA type SY-DATUM
      !I_HORA type SY-UZEIT
      value(I_DATEF) type SY-DATUM optional
      value(I_HORAF) type SY-UZEIT optional
    exporting
      !E_LOCALIZACAO type ZPME0068 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_SAPPM_AUTOTRAC IMPLEMENTATION.


  METHOD get_dados_local.
* Início - Processo Localização - CS2022000580 AJUSTAR ZPM0073 COM NOVA API DE LOCALIZAÇÃO DE ABASTECIMENTO ( SAP x AUTOTRAC ) #79559
* RJF - 2022.07.27
    DATA: _url               TYPE string,
          w_zciot_webservice TYPE zciot_webservice,
          json_retorno       TYPE string,
          e_reason           TYPE string,
          w_data             TYPE d,
          w_hora             TYPE t,
          w_rudat            TYPE tvpod-rudat,
          w_rutim            TYPE tvpod-rutim,
          wc_json            TYPE string,
          w_apont            TYPE zpme0058,
          dt                 TYPE char10,
          dt_fim             TYPE char10,
          hr_ini             TYPE char08,
          hr_fim             TYPE char08,
          lv_hri             TYPE sy-uzeit,
          lv_hrf             TYPE sy-uzeit,
          lv_dti             TYPE sy-datum,
          lv_dtf             TYPE sy-datum,
          ob_w_servicel      TYPE REF TO zcl_webservice.

    CREATE OBJECT ob_w_servicel.

    w_data = i_data.
    w_hora = i_hora.

    "Função para acrescentar tempo
    CALL FUNCTION 'TSTR_CALC_TIME'
      EXPORTING
        iv_begin_datelocal_req   = w_data
        iv_begin_timelocal_req   = w_hora
        iv_duration_integer      = 15540
        iv_direction             = '-'
      IMPORTING
        ev_end_datelocal         = w_rudat
        ev_end_timelocal         = w_rutim
      EXCEPTIONS
        fatal_error              = 1
        time_invalid             = 2
        time_missing             = 3
        tstream_not_loadable     = 4
        tstream_generation_error = 5
        parameter_error          = 6
        unspecified_error        = 7
        OTHERS                   = 8.


    MOVE: w_rudat TO lv_dti,
          w_rutim TO lv_hri.
    FREE: w_rudat, w_rutim.

*---

    w_data = i_datef.
    w_hora = i_horaf.

*   Função para acrescentar tempo
    CALL FUNCTION 'TSTR_CALC_TIME'
      EXPORTING
        iv_begin_datelocal_req   = i_data
        iv_begin_timelocal_req   = i_hora
        iv_duration_integer      = 13320
        iv_direction             = '-'
      IMPORTING
        ev_end_datelocal         = w_rudat
        ev_end_timelocal         = w_rutim
      EXCEPTIONS
        fatal_error              = 1
        time_invalid             = 2
        time_missing             = 3
        tstream_not_loadable     = 4
        tstream_generation_error = 5
        parameter_error          = 6
        unspecified_error        = 7
        OTHERS                   = 8.

    MOVE: w_rudat TO lv_dtf,
          w_rutim TO lv_hrf.
    FREE: w_rudat, w_rutim.

*   Passando os dados para variavel principal.
    CLEAR: dt, dt_fim, hr_ini, hr_fim.
    dt     = |{ lv_dti(4) }-{ lv_dti+4(2) }-{ lv_dti+6(2) }|.
    dt_fim = |{ lv_dtf(4) }-{ lv_dtf+4(2) }-{ lv_dtf+6(2) }|.
    hr_ini = |{ lv_hri(2) }:{ lv_hri+2(2) }:{ lv_hri+4(2) }|.
    hr_fim = |{ lv_hrf(2) }:{ lv_hrf+2(2) }:{ lv_hrf+4(2) }|.

    "Alterar o link na tabela com base no veiculo.
    CLEAR:_url, w_zciot_webservice.
    SELECT SINGLE *
    FROM zciot_webservice
    INTO w_zciot_webservice
      WHERE tipo          = '7'
        AND url           IS NOT NULL
        AND servico       = '22'
         OR servico       = '21'.

    IF w_zciot_webservice-url IS NOT INITIAL.
      _url = w_zciot_webservice-url.

*    Url
      _url = _url && i_code
      && '/positions?_dateTimeFrom='
      && dt
      && 'T'
      && hr_ini
      && '&_dateTimeTo='
      && dt_fim
      && 'T'
      && hr_fim
      && '-04:00&_LAST=TRUE'.
      "_dateTimeFrom=2021-04-21T07:00&_dateTimeTo=2021-04-22T10:29

      CALL METHOD cl_http_client=>create_by_url
        EXPORTING
          url                = _url
        IMPORTING
          client             = DATA(v_http_get)
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4.

      CALL METHOD v_http_get->request->set_header_field
        EXPORTING
          name  = 'Ocp-Apim-Subscription-Key'
          value = 'fdb79bbd3a3447d7b1c4fdba77cfd453'.  "'9b1b268b3394477ab9c6b04382010234'.


      CALL METHOD v_http_get->request->set_header_field
        EXPORTING
          name  = 'Authorization'
          value = 'Basic aticapiti@amaggi:Transportes@123'.


*     Documentação: http://www.w3schools.com/tags/ref_httpmethods.asp
      CALL METHOD v_http_get->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'GET'.

      CALL METHOD v_http_get->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'application/json'.

*    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).
      CLEAR json_retorno.
      ob_w_servicel->zif_webservice~consultar(
          EXPORTING
            i_http                     = v_http_get
            i_xml                      = wc_json
          IMPORTING
            e_reason                   = e_reason
          RECEIVING
            e_resultado                = json_retorno
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5
        ).

      IF sy-subrc IS INITIAL.

        CLEAR e_localizacao.
        FREE e_localizacao-data.
        /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = e_localizacao ).
      ENDIF.
    ENDIF.
* Fim - Processo Localização - CS2022000580 AJUSTAR ZPM0073 COM NOVA API DE LOCALIZAÇÃO DE ABASTECIMENTO ( SAP x AUTOTRAC ) #79559
* RJF - 2022.07.27
  ENDMETHOD.


  METHOD get_dados_p_mirian.

    DATA: t_zpmt0032 TYPE TABLE OF zpmt0032,
          t_zpmt0024 TYPE TABLE OF zpmt0024,
          t_zpmt0026 TYPE TABLE OF zpmt0026,
          w_zpme0045 TYPE zpme0065.

    FREE: t_zpmt0026, t_zpmt0024, t_zpmt0032.

    "Seleciona dados abastecimento combutivél.
    SELECT * FROM zpmt0024 AS a INTO TABLE t_zpmt0024
    WHERE a~placa IN i_placa
      AND a~dt_cupom_fisc BETWEEN i_perido_inic AND i_perido_fim
      AND ( EXISTS ( SELECT * FROM zpmt0032 WHERE fatura EQ a~fatura AND cnpj EQ a~cnpj AND cod_status NE '999' ) ).


    IF t_zpmt0024 IS NOT INITIAL.
      "Seleciona material.
      SELECT * FROM zpmt0026 INTO TABLE t_zpmt0026
      FOR ALL ENTRIES IN t_zpmt0024
        WHERE fatura EQ t_zpmt0024-fatura
          AND cnpj   EQ t_zpmt0024-cnpj
          AND cupom_fisc EQ t_zpmt0024-cupom_fisc.


      "Seleciona dados da fatura.
      SELECT * FROM zpmt0032 INTO TABLE t_zpmt0032
      FOR ALL ENTRIES IN t_zpmt0024
        WHERE fatura EQ t_zpmt0024-fatura
          AND cnpj   EQ t_zpmt0024-cnpj.

      LOOP AT t_zpmt0026 ASSIGNING FIELD-SYMBOL(<ws_zpmt0026>).

        w_zpme0045-cod_material    = <ws_zpmt0026>-cod_material.
        w_zpme0045-desc_material   = <ws_zpmt0026>-desc_material.
        w_zpme0045-und             = <ws_zpmt0026>-und.
        w_zpme0045-qtde            = <ws_zpmt0026>-qtde.
        w_zpme0045-vlr_unt         = <ws_zpmt0026>-vlr_unt.
        w_zpme0045-vlr_total       = <ws_zpmt0026>-vlr_total.

        READ TABLE t_zpmt0024 INTO DATA(ws_zpmt0024) WITH KEY fatura = <ws_zpmt0026>-fatura
                                                              cnpj   = <ws_zpmt0026>-cnpj
                                                          cupom_fisc = <ws_zpmt0026>-cupom_fisc.

        IF sy-subrc EQ 0.
          w_zpme0045-chave_nfe     = ws_zpmt0024-chave_nfe     .
          w_zpme0045-cupom_fisc    = ws_zpmt0024-cupom_fisc    .
          w_zpme0045-serie_cupom   = ws_zpmt0024-serie_cupom   .
          w_zpme0045-dt_cupom_fisc = ws_zpmt0024-dt_cupom_fisc .
          w_zpme0045-hr_cupom_fisc = ws_zpmt0024-hr_cupom_fisc .
          w_zpme0045-ordem         = ws_zpmt0024-ordem         .
          w_zpme0045-placa         = ws_zpmt0024-placa         .
          w_zpme0045-odometro      = ws_zpmt0024-odometro      .
        ENDIF.


        READ TABLE t_zpmt0032 INTO DATA(ws_zpmt0032) WITH KEY fatura = ws_zpmt0024-fatura
                                                              cnpj   = ws_zpmt0024-cnpj.
        IF sy-subrc EQ 0.
          w_zpme0045-fatura  = ws_zpmt0032-fatura .
          w_zpme0045-cnpj    = ws_zpmt0032-cnpj   .
          w_zpme0045-lifnr   = ws_zpmt0032-lifnr  .
          w_zpme0045-empresa = ws_zpmt0032-empresa.
          w_zpme0045-pedido  = ws_zpmt0032-pedido .
        ENDIF.

        APPEND w_zpme0045 TO t_comb.
        CLEAR: w_zpme0045.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD I_CONS_CADASTRO_VEIC.

    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.
    CREATE OBJECT OB_WEB_SERVICE.
    DATA: E_REASON           TYPE STRING.
    DATA WC_JSON TYPE STRING.
    DATA:  JSON_RETORNO       TYPE STRING.

*   ACESSANDO A CLASSE WEBSERVICE PARA VALIDAR O TIPO DE SERVIÇO CADASTRADO ZLES0096
    TRY .
        OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = '20' ).
      CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
    ENDTRY.
    OB_WEB_SERVICE->SET_TIPO( I_TIPO = '7' ).

    TRY .
        DATA(VAR_HTTP_GET) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
        DATA(LC_URI) = OB_WEB_SERVICE->GET_URI(  ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
    ENDTRY.

*       Construindo bearer
*        CONCATENATE '' '9b1b268b3394477ab9c6b04382010234'   INTO DATA(BEARER_TOKEN) SEPARATED BY SPACE.
    CALL METHOD VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Ocp-Apim-Subscription-Key'
        VALUE = 'fdb79bbd3a3447d7b1c4fdba77cfd453'.


    CALL METHOD VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Authorization'
        VALUE = 'Basic aticapiti@amaggi:Transportes@123'.


    "Documentação: http://www.w3schools.com/tags/ref_httpmethods.asp
    CALL METHOD VAR_HTTP_GET->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = '~request_method'
        VALUE = 'GET'.

*    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).
    CLEAR JSON_RETORNO.
    OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
        EXPORTING
          I_HTTP                     = VAR_HTTP_GET
          I_XML                      = WC_JSON
        IMPORTING
*          E_CODE                     = E_CODE
          E_REASON                   = E_REASON
        RECEIVING
          E_RESULTADO                = JSON_RETORNO
        EXCEPTIONS
          HTTP_COMMUNICATION_FAILURE = 1
          HTTP_INVALID_STATE         = 2
          HTTP_PROCESSING_FAILED     = 3
          HTTP_INVALID_TIMEOUT       = 4
          OTHERS                     = 5
      ).

    IF SY-SUBRC IS INITIAL.
      CLEAR T_CADASTRO_VEIC.
      /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = T_CADASTRO_VEIC ).
    ENDIF.


  ENDMETHOD.


  METHOD i_cons_combust.

    DATA: t_zpmt0026 TYPE TABLE OF zpmt0026.
    DATA: zpme0061 TYPE zpme0061.
    DATA: gt_abastecimentos TYPE TABLE OF zpme0047.
    DATA  t_combt TYPE zpme0062_t.

    DATA: tot_quand_diesel TYPE bstmg.
    DATA: tot_quand_arla TYPE bstmg.
    DATA t_veiculo TYPE TABLE OF fleet.
    DATA: r_placa TYPE zrsdsselopts.
    DATA: dt_inic TYPE sy-datum,
          dt_fim  TYPE sy-datum,
*---> 20.06.2023 - Migração S4 - DG
"          item    TYPE char02.
          ITEM    TYPE ZCHAR02.
*<--- 20.06.2023 - Migração S4 - DG

    "selecionar abastecimentos da frota. Posto Mirian
    IF i_equnr IS NOT INITIAL.
      SELECT *
      FROM fleet AS c
      INNER JOIN equi AS b ON b~objnr EQ c~objnr
      INNER JOIN equz AS a ON a~equnr EQ b~equnr
        INTO CORRESPONDING FIELDS OF TABLE t_veiculo
          WHERE b~equnr  IN i_equnr
            AND a~datbi EQ '99991231'.

      r_placa  =  VALUE #( FOR s IN  t_veiculo   ( sign = 'I' option = 'EQ' low = s-license_num ) ).

      SELECT  * FROM zpmt0026 AS a
      INNER JOIN zpmt0024 AS b ON b~fatura EQ a~fatura AND b~cnpj EQ b~cnpj AND b~cupom_fisc EQ a~cupom_fisc
      INTO CORRESPONDING FIELDS OF TABLE gt_abastecimentos
        WHERE b~placa IN r_placa
          AND b~dt_cupom_fisc BETWEEN i_perido_inic AND i_perido_fim
        AND ( EXISTS ( SELECT * FROM zpmt0032 WHERE fatura EQ a~fatura AND cnpj EQ a~cnpj AND cod_status NE '999' ) ).

    ELSE.
      SELECT  * FROM zpmt0026 AS a
   INNER JOIN zpmt0024 AS b ON b~fatura EQ a~fatura AND b~cnpj EQ b~cnpj AND b~cupom_fisc EQ a~cupom_fisc
   INTO CORRESPONDING FIELDS OF TABLE gt_abastecimentos
     WHERE b~placa IN i_placa
       AND b~dt_cupom_fisc BETWEEN i_perido_inic AND i_perido_fim
        AND ( EXISTS ( SELECT * FROM zpmt0032 WHERE fatura EQ a~fatura AND cnpj EQ a~cnpj AND cod_status NE '999' ) ).
    ENDIF.

    IF gt_abastecimentos IS NOT INITIAL.

      SELECT * FROM zpmt0032 INTO TABLE @DATA(t_fatura)
        FOR ALL ENTRIES IN @gt_abastecimentos
        WHERE fatura EQ @gt_abastecimentos-fatura
          AND cnpj   EQ @gt_abastecimentos-cnpj
          AND cod_status NE '999'.
    ENDIF.


    "Selecionado combustivel da Autotrac com base nas condições.
    SELECT * FROM zpmt0035 INTO TABLE @DATA(t_zpmt0035)
      WHERE dt_cupom_fisc BETWEEN @i_perido_inic AND @i_perido_fim
       AND  equnr IN @i_equnr
       AND  placa IN @i_placa
       AND  item  EQ '20'.

    DATA(tl_zpmt0035) = t_zpmt0035.
    SORT tl_zpmt0035 BY gru dt_cupom_fisc.
    DELETE ADJACENT DUPLICATES FROM tl_zpmt0035 COMPARING gru.


    SORT t_fatura BY cnpj fatura .
    SORT gt_abastecimentos BY cnpj fatura dt_cupom_fisc .

    "Totalizado quantidade de combustivel por cnpj, data, fatura. Posto Mirian.
    LOOP AT t_fatura ASSIGNING FIELD-SYMBOL(<l_fatura>).
      LOOP AT gt_abastecimentos ASSIGNING FIELD-SYMBOL(<l_abast>) WHERE cnpj EQ <l_fatura>-cnpj AND fatura EQ <l_fatura>-fatura.
        CASE <l_abast>-cod_material.
          WHEN '000000000000000007'.
            ADD <l_abast>-qtde TO tot_quand_diesel.
          WHEN '000000000000002238' OR '000000000000004177'.
            IF <l_abast>-cod_material EQ '000000000000002238'.
              <l_abast>-qtde = ( <l_abast>-qtde * 20 ).
            ENDIF.
            ADD <l_abast>-qtde TO tot_quand_arla.
        ENDCASE.

        IF dt_inic IS INITIAL.
          dt_inic = <l_abast>-dt_cupom_fisc.
        ENDIF.

        dt_fim  = <l_abast>-dt_cupom_fisc.
      ENDLOOP.

      ADD 1 TO item.



      APPEND VALUE #(     item         = item
                          data_ini     = dt_inic
                          data_fim     = dt_fim
                          empresa      = <l_fatura>-empresa
                          fatura       = <l_fatura>-fatura
                          pedido       = <l_fatura>-pedido
                          nfe          = <l_fatura>-nfe
                          desc_mat     = 'Diesel'
                          qtde         = tot_quand_diesel
                          gru          = ''
                          qtde_aut     = ''
                          dif          = '' ) TO t_comb.

      APPEND VALUE #(     item         = item
                          data_ini     = dt_inic
                          data_fim     = dt_fim
                          empresa      = <l_fatura>-empresa
                          fatura       = <l_fatura>-fatura
                          pedido       = <l_fatura>-pedido
                          nfe          = <l_fatura>-nfe
                          desc_mat     = 'Arla'
                          qtde         = tot_quand_arla
                          gru          = ''
                          qtde_aut     = ''
                          dif          = '' ) TO t_comb.

      CLEAR: dt_inic, dt_fim, tot_quand_arla, tot_quand_diesel.
    ENDLOOP.


    CLEAR: item.
    "Totalizar quantidade de combustivel Autotrac com base Grupo e Data.
    LOOP AT tl_zpmt0035 ASSIGNING FIELD-SYMBOL(<l_zpmt0035>).
      LOOP AT t_zpmt0035 ASSIGNING FIELD-SYMBOL(<w_zpmt0035>) WHERE gru EQ <l_zpmt0035>-gru.

        CASE <l_zpmt0035>-item.
          WHEN '20'.
            ADD <w_zpmt0035>-qtde TO tot_quand_diesel.
            ADD <w_zpmt0035>-lt_arla TO tot_quand_arla.
            IF dt_inic IS INITIAL.
              dt_inic = <w_zpmt0035>-dt_cupom_fisc.
            ENDIF.
            dt_fim  = <w_zpmt0035>-dt_cupom_fisc.
        ENDCASE.
      ENDLOOP.

      ADD 1 TO item.


      APPEND VALUE #(   item         = item
                        data_ini     = dt_inic
                        data_fim     = dt_fim
                        desc_mat     = 'Diesel'
                        gru          = <l_zpmt0035>-gru
                        qtde_aut     = tot_quand_diesel
                        dif          = '' ) TO t_combt.

      APPEND VALUE #(   item         = item
                        data_ini     = dt_inic
                        data_fim     = dt_fim
                        desc_mat     = 'Arla'
                        gru          = <l_zpmt0035>-gru
                        qtde_aut     = tot_quand_arla
                        dif          = '' ) TO t_combt.

      CLEAR: dt_inic, dt_fim, tot_quand_arla, tot_quand_diesel.
    ENDLOOP.

    IF i_comb IS NOT INITIAL.
      SORT t_comb BY desc_mat.
      DELETE t_comb WHERE desc_mat NE i_comb.
    ENDIF.

    SORT t_comb BY data_ini.


    "Adicionando o diesel Autotrac na saida principal.
    LOOP AT t_comb ASSIGNING FIELD-SYMBOL(<w_comb>).
      READ TABLE t_combt INTO DATA(l_comb) WITH KEY item = <w_comb>-item
                                                desc_mat = <w_comb>-desc_mat.

      IF sy-subrc EQ 0.

        CASE <w_comb>-desc_mat.
          WHEN 'Diesel'.
            <w_comb>-gru      = l_comb-gru.
            <w_comb>-qtde_aut = l_comb-qtde_aut.
            <w_comb>-dif      = ( l_comb-qtde_aut - <w_comb>-qtde ).

          WHEN 'Arla'.
            <w_comb>-gru      = l_comb-gru.
            <w_comb>-qtde_aut = l_comb-qtde_aut.
            <w_comb>-dif      = ( l_comb-qtde_aut - <w_comb>-qtde ).
          WHEN OTHERS.
        ENDCASE.
      ENDIF.

    ENDLOOP.
    CLEAR: item.

  ENDMETHOD.


  METHOD i_cons_comb_sint.

    DATA: t_zpmt0026 TYPE TABLE OF zpmt0026.
    DATA: zpme0061 TYPE zpme0061.
    DATA: gt_abastecimentos TYPE TABLE OF zpme0047.
    DATA  t_combt TYPE zpme0063_t.

    DATA: tot_quand_diesel TYPE bstmg.
    DATA: tot_quand_arla TYPE bstmg.
    DATA t_veiculo TYPE TABLE OF fleet.
    DATA: r_placa TYPE zrsdsselopts.
    DATA: dt_inic TYPE sy-datum,
          dt_fim  TYPE sy-datum,
*---> 20.06.2023 - Migração S4 - DG
"          item    TYPE char02.
          ITEM    TYPE ZCHAR02.
*<--- 20.06.2023 - Migração S4 - DG

    DATA: total_diesel TYPE p DECIMALS 3.
    DATA: total_arla TYPE p DECIMALS 3.

    "selecionar abastecimentos da frota. Posto Mirian
    IF i_equnr IS NOT INITIAL.
      SELECT *
      FROM fleet AS c
      INNER JOIN equi AS b ON b~objnr EQ c~objnr
      INNER JOIN equz AS a ON a~equnr EQ b~equnr
        INTO CORRESPONDING FIELDS OF TABLE t_veiculo
          WHERE b~equnr  IN i_equnr
            AND a~datbi EQ '99991231'.

      r_placa  =  VALUE #( FOR s IN  t_veiculo   ( sign = 'I' option = 'EQ' low = s-license_num ) ).

      SELECT  * FROM zpmt0026 AS a
      INNER JOIN zpmt0024 AS b ON b~fatura EQ a~fatura AND b~cnpj EQ b~cnpj AND b~cupom_fisc EQ a~cupom_fisc
      INTO CORRESPONDING FIELDS OF TABLE gt_abastecimentos
        WHERE b~placa IN r_placa
          AND b~dt_cupom_fisc BETWEEN i_perido_inic AND i_perido_fim
          AND ( EXISTS ( SELECT * FROM zpmt0032 WHERE fatura EQ a~fatura AND cnpj EQ a~cnpj AND cod_status NE '999' ) ).

    ELSE.
      SELECT  * FROM zpmt0026 AS a
   INNER JOIN zpmt0024 AS b ON b~fatura EQ a~fatura AND b~cnpj EQ b~cnpj AND b~cupom_fisc EQ a~cupom_fisc
   INTO CORRESPONDING FIELDS OF TABLE gt_abastecimentos
     WHERE b~placa IN i_placa
       AND b~dt_cupom_fisc BETWEEN i_perido_inic AND i_perido_fim
       AND ( EXISTS ( SELECT * FROM zpmt0032 WHERE fatura EQ a~fatura AND cnpj EQ a~cnpj AND cod_status NE '999' ) ).

    ENDIF.

    IF gt_abastecimentos IS NOT INITIAL.

      SELECT * FROM zpmt0032 INTO TABLE @DATA(t_fatura)
        FOR ALL ENTRIES IN @gt_abastecimentos
        WHERE fatura EQ @gt_abastecimentos-fatura
          AND cnpj   EQ @gt_abastecimentos-cnpj
          AND cod_status NE '999'.
    ENDIF.

    "Selecionado combustivel da Autotrac com base nas condições.
    IF i_equnr IS NOT INITIAL.
      FREE: r_placa.
      SELECT *
      FROM fleet AS c
      INNER JOIN equi AS b ON b~objnr EQ c~objnr
      INNER JOIN equz AS a ON a~equnr EQ b~equnr
        INTO CORRESPONDING FIELDS OF TABLE t_veiculo
          WHERE b~equnr  IN i_equnr
            AND a~datbi EQ '99991231'.

      r_placa  =  VALUE #( FOR s IN  t_veiculo   ( sign = 'I' option = 'EQ' low = s-license_num ) ).

      SELECT * FROM zpmt0035 INTO TABLE @DATA(t_zpmt0035)
      WHERE dt_cupom_fisc BETWEEN @i_perido_inic AND @i_perido_fim
       AND  placa IN @r_placa
       AND  item  EQ '20'.
    ELSE.

      SELECT * FROM zpmt0035 INTO TABLE t_zpmt0035
     WHERE dt_cupom_fisc BETWEEN i_perido_inic AND i_perido_fim
      AND  placa IN i_placa
        AND  item  EQ '20'.
    ENDIF.


    DATA(tl_comb) = gt_abastecimentos.
    SORT tl_comb BY placa.
    DELETE ADJACENT DUPLICATES FROM tl_comb COMPARING placa.


    SORT t_zpmt0035 BY placa .
    SORT gt_abastecimentos BY placa .

    "Totalizado quantidade de combustivel por cnpj, data, fatura. Posto Mirian.
    LOOP AT tl_comb ASSIGNING FIELD-SYMBOL(<l_comb>).
      LOOP AT gt_abastecimentos ASSIGNING FIELD-SYMBOL(<l_abast>) WHERE placa EQ <l_comb>-placa. "AND COD_MATERIAL EQ <L_COMB>-COD_MATERIAL.
        CASE <l_abast>-cod_material.
          WHEN '000000000000000007'.
            IF <l_abast>-qtde > 0.
              ADD <l_abast>-qtde TO tot_quand_diesel.
            ENDIF.
          WHEN '000000000000002238' OR '000000000000004177'.
            IF <l_abast>-cod_material EQ '000000000000002238'.
              IF <l_abast>-qtde > 0.
                <l_abast>-qtde = ( <l_abast>-qtde * 20 ).
              ENDIF.
            ENDIF.
            IF <l_abast>-qtde > 0.
              ADD <l_abast>-qtde TO tot_quand_arla.
            ENDIF.
        ENDCASE.

        IF dt_inic IS INITIAL.
          dt_inic = <l_abast>-dt_cupom_fisc.
        ENDIF.

        dt_fim  = <l_abast>-dt_cupom_fisc.
      ENDLOOP.

      IF tot_quand_diesel IS NOT  INITIAL.
        ADD tot_quand_diesel TO total_diesel.

        APPEND VALUE #(
                            data_ini     = dt_inic
                            data_fim     = dt_fim
                            placa        = <l_comb>-placa
                            desc_mat     = 'Diesel'
                            qtde         = tot_quand_diesel
                            ) TO t_comb.
      ENDIF.


      IF tot_quand_arla IS NOT INITIAL.
        ADD tot_quand_arla TO total_arla.
        APPEND VALUE #(
                            data_ini     = dt_inic
                            data_fim     = dt_fim
                            placa        = <l_comb>-placa
                            desc_mat     = 'Arla'
                            qtde         = tot_quand_arla
                            ) TO t_comb.
      ENDIF.


      CLEAR: dt_inic, dt_fim, tot_quand_arla, tot_quand_diesel.
    ENDLOOP.


    CLEAR: item.
    "Totalizar quantidade de combustivel Autotrac com base Grupo e Data.
    LOOP AT tl_comb ASSIGNING <l_comb>.
      LOOP AT t_zpmt0035 ASSIGNING FIELD-SYMBOL(<w_zpmt0035>) WHERE placa EQ <l_comb>-placa.

        IF ( <w_zpmt0035>-qtde IS NOT INITIAL ).
          ADD <w_zpmt0035>-qtde TO tot_quand_diesel.
        ENDIF.

        IF ( <w_zpmt0035>-lt_arla  IS NOT INITIAL ).
          ADD <w_zpmt0035>-lt_arla TO tot_quand_arla.
        ENDIF.

        IF dt_inic IS INITIAL.
          dt_inic = <w_zpmt0035>-dt_cupom_fisc.
        ENDIF.
        dt_fim  = <w_zpmt0035>-dt_cupom_fisc.
      ENDLOOP.

*      ADD 1 TO ITEM.

      APPEND VALUE #(
                          data_ini     = dt_inic
                          data_fim     = dt_fim
                          placa        = <l_comb>-placa
                          desc_mat     = 'Diesel'
                          qtde         = tot_quand_diesel
                          ) TO  t_combt.

      APPEND VALUE #(
                          data_ini     = dt_inic
                          data_fim     = dt_fim
                          placa        = <l_comb>-placa
                          desc_mat     = 'Arla'
                          qtde         = tot_quand_arla
                          ) TO t_combt.

      CLEAR: dt_inic, dt_fim, tot_quand_arla, tot_quand_diesel.
    ENDLOOP.

    IF i_comb IS NOT INITIAL.
      SORT t_comb BY desc_mat.
      DELETE t_comb WHERE desc_mat NE i_comb.
    ENDIF.

    SORT t_comb BY data_ini.


    "Adicionando o diesel Autotrac na saida principal.
    LOOP AT t_comb ASSIGNING FIELD-SYMBOL(<w_comb>).
      READ TABLE t_combt INTO DATA(l_comb) WITH KEY placa = <w_comb>-placa desc_mat = <w_comb>-desc_mat.

      IF sy-subrc EQ 0.
        CASE <w_comb>-desc_mat.
          WHEN 'Diesel'.
            <w_comb>-qtde_aut = l_comb-qtde.
            <w_comb>-dif      = ( <w_comb>-qtde - l_comb-qtde ).

            IF <w_comb>-dif > 0 AND l_comb-qtde > 0.
              <w_comb>-porc     = ( <w_comb>-dif / l_comb-qtde ) * 100.
            ENDIF.

          WHEN 'Arla'.
            <w_comb>-qtde_aut = l_comb-qtde.
            <w_comb>-dif      = ( <w_comb>-qtde - l_comb-qtde ).

            IF <w_comb>-dif > 0 AND l_comb-qtde > 0.
              <w_comb>-porc     = ( <w_comb>-dif / l_comb-qtde ) * 100.
            ENDIF.

          WHEN OTHERS.
        ENDCASE.
      ENDIF.

    ENDLOOP.
    CLEAR: item, total_diesel, total_arla.

  ENDMETHOD.


  METHOD i_cons_dados_abast.

    DATA: ob_w_service TYPE REF TO zcl_webservice.
    CREATE OBJECT ob_w_service.
    DATA: e_reason           TYPE string.
    DATA wc_json TYPE string.
    DATA:  json_retorno       TYPE string.
    DATA: w_zciot_webservic TYPE zciot_webservice.
    DATA: w_apont TYPE zpme0058.
    DATA: i_dt     TYPE sy-datum.
    DATA: w_dt TYPE zpmt0038.
    DATA: w_zpme0064 TYPE zpme0064.
    DATA: data       TYPE char10,
          hora       TYPE char08,
          t_doc      TYPE TABLE OF imrg_mdocm,
          i_equnr    TYPE equnr,
          p_erro,
          w_zpmt0035 TYPE zpmt0035,
          cont       TYPE p.

    DATA:  placa TYPE license_num.
    DATA: gru    TYPE char7.
    DATA: frota  TYPE char7.
    DATA: w_zciot_webservice   TYPE zciot_webservice.
    DATA: _url TYPE string.

    DATA: dt     TYPE char10,
          dt_fim TYPE char10.
    DATA: hr_ini TYPE char08.
    DATA: hr_fim TYPE char08.

    DATA:n_envelope TYPE char20,
         n_cupum    TYPE zpmt0035-cupom_fisc,
         lts_diesel TYPE char13,
         lts_arla   TYPE char13,
         vlr_diesel TYPE char13,
         matricula  TYPE zpmt0035-motorista,
         odometro   TYPE zpmt0035-odometro,
         w_combust  TYPE zpmt0037.

    DATA: gw_veiculo TYPE equz.
    CLEAR: p_erro, cont.


    "Passando os dados para variavel principal.
    CLEAR: dt, dt_fim, hr_ini, hr_fim.
    dt     = |{ i_data(4) }-{ i_data+4(2) }-{ i_data+6(2) }|.
    dt_fim = |{ i_data_fim(4) }-{ i_data_fim+4(2) }-{ i_data_fim+6(2) }|.
    hr_ini = |{ i_hr_inic(2) }:{ i_hr_inic+2(2) }:{ i_hr_inic+4(2) }|.
    hr_fim = |{ i_hr_fim(2) }:{ i_hr_fim+2(2) }:{ i_hr_fim+4(2) }|.

**================================================================================
*
*    DT        = '2020-06-02'.
*    HR_INI    = '00:00:00'.
*    HR_FIM    = '23:59:00'.

*    CHECK DT IS NOT INITIAL.


    CLEAR: p_erro. FREE: t_doc.
    LOOP AT t_dados_veic-data ASSIGNING FIELD-SYMBOL(<w_abast_veic>).

      "Alterar o link na tabela com base no veiculo.
      CLEAR:_url, w_zciot_webservice.
      SELECT SINGLE *
      FROM zciot_webservice
      INTO w_zciot_webservice
        WHERE tipo          = '7'
          AND servico       = '21'.

      IF w_zciot_webservice-url IS NOT INITIAL.
        _url = w_zciot_webservice-url.

*        _URL = _URL && <W_ABAST_VEIC>-CODE && '/returnmessages?_dateTimeFrom=' && DT && 'T' && HR_INI && '&_dateTimeTo=' && DT && 'T' && HR_FIM && '-04:00&_LIMIT=300'.

        _url = _url && <w_abast_veic>-code
        && '/returnmessages?_dateTimeFrom='
        && dt
        && 'T'
        && hr_ini
        && '&_dateTimeTo='
        && dt_fim
        && 'T'
        && hr_fim
        && '-04:00&_LIMIT=300'.
        "_dateTimeFrom=2021-04-21T07:00&_dateTimeTo=2021-04-22T10:29

        CALL METHOD cl_http_client=>create_by_url
          EXPORTING
            url                = _url
          IMPORTING
            client             = DATA(v_http_get)
          EXCEPTIONS
            argument_not_found = 1
            plugin_not_active  = 2
            internal_error     = 3
            OTHERS             = 4.

        CALL METHOD v_http_get->request->set_header_field
          EXPORTING
            name  = 'Ocp-Apim-Subscription-Key'
            value = 'fdb79bbd3a3447d7b1c4fdba77cfd453'.  "'9b1b268b3394477ab9c6b04382010234'.


        CALL METHOD v_http_get->request->set_header_field
          EXPORTING
            name  = 'Authorization'
            value = 'Basic aticapiti@amaggi:Transportes@123'.


        "Documentação: http://www.w3schools.com/tags/ref_httpmethods.asp
        CALL METHOD v_http_get->request->set_header_field
          EXPORTING
            name  = '~request_method'
            value = 'GET'.

        CALL METHOD v_http_get->request->set_header_field
          EXPORTING
            name  = 'Content-Type'
            value = 'application/json'.

*    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP_GET ).
        CLEAR json_retorno.
        ob_w_service->zif_webservice~consultar(
            EXPORTING
              i_http                     = v_http_get
              i_xml                      = wc_json
            IMPORTING
*          E_CODE                     = E_CODE
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = json_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5
          ).

        IF sy-subrc IS INITIAL.

          CLEAR t_dados_abast_veic.
          FREE t_dados_abast_veic-data.
          /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = t_dados_abast_veic ).

          IF t_dados_abast_veic-data IS NOT INITIAL.
            SORT t_dados_abast_veic-data BY macronumber.
            DELETE t_dados_abast_veic-data WHERE macronumber NE '14'.

            LOOP AT t_dados_abast_veic-data ASSIGNING FIELD-SYMBOL(<_dados>).
              IF <_dados>-macronumber EQ '14'.

                "Verificar se macro msg ja foi importada com sucesso.
                CLEAR: w_zpmt0035.
                SELECT SINGLE *
                FROM zpmt0035
                INTO w_zpmt0035
                  WHERE id EQ <_dados>-id.

                "Se ja existe na tabela, retorna o loop para processar o proximo.
                IF w_zpmt0035 IS NOT INITIAL.
                  CONTINUE.
                ENDIF.

                DATA: e_dados_loc_veic TYPE zpme0068,
                      lv_found         TYPE char01.

                CLEAR: placa, frota, gru.
                SPLIT <w_abast_veic>-name AT '-' INTO: frota placa DATA(data3).
                SPLIT <w_abast_veic>-name AT '/' INTO: DATA(data2) gru.
                placa = |{ placa(8) }|.
                CONDENSE placa.
                CONDENSE frota.
                CONDENSE gru.

                "Dados da mensagem.
                CLEAR: n_envelope, n_cupum, lts_diesel, lts_arla, vlr_diesel,  matricula, odometro.
                SPLIT <_dados>-messagetext AT '_' INTO: DATA(vazio) n_envelope n_cupum lts_diesel lts_arla vlr_diesel  matricula odometro.
                CONDENSE: n_envelope, n_cupum, matricula, lts_diesel, lts_arla, vlr_diesel, odometro.
                REPLACE '-' IN lts_diesel WITH '0'.
                REPLACE '-' IN lts_arla WITH '0'.

                "Data
*                DATA: L_DATA TYPE d L_HORA TYPE LIKPUKWA-LFUHR.
                DATA: l_data TYPE d, l_hora TYPE t.
                DATA: l_rudat TYPE tvpod-rudat, l_rutim TYPE tvpod-rutim.
                CLEAR: l_rudat, l_rutim, l_data, l_hora.

                SPLIT <_dados>-messagetime AT 'T' INTO: data hora.
                l_data = |{ DATA(4) }{ data+5(2) }{ data+8(2) }|.
                l_hora = |{ hora(2) }{ hora+3(2) }{ hora+6(2) }|.


* Início - Processo Localização - CS2022000580 AJUSTAR ZPM0073 COM NOVA API DE LOCALIZAÇÃO DE ABASTECIMENTO ( SAP x AUTOTRAC ) #79559
* RJF - 2022.07.27
                FREE: e_dados_loc_veic.
                zcl_int_sappm_autotrac=>get_dados_local( EXPORTING
                                                             i_code  = <w_abast_veic>-code
                                                             i_data  = l_data
                                                             i_hora  = l_hora
*                                                             i_datef = i_data_fim
*                                                             i_horaf = i_hr_fim
                                                         IMPORTING
                                                             e_localizacao = e_dados_loc_veic ). " Retorno Json objetivo County
* Retorno json county = vilhena... continue. Resolvendo problema específico de duplicidade
                CLEAR: lv_found.
                LOOP AT e_dados_loc_veic-data ASSIGNING FIELD-SYMBOL(<w_loc_veic>).
                  IF <w_loc_veic>-county EQ 'Vilhena'(001).
                    lv_found = abap_true.
                    EXIT.
                  ENDIF.
                ENDLOOP.

                IF lv_found IS NOT INITIAL.
                  CONTINUE.
                ENDIF.
* Fim - Processo Localização - CS2022000580 AJUSTAR ZPM0073 COM NOVA API DE LOCALIZAÇÃO DE ABASTECIMENTO ( SAP x AUTOTRAC ) #79559

                CALL FUNCTION 'TSTR_CALC_TIME'
                  EXPORTING
*                   IV_CALENDARID            = '99'
                    iv_begin_datelocal_req   = l_data
                    iv_begin_timelocal_req   = l_hora
                    iv_duration_integer      = 14400
                    iv_direction             = '-'
                  IMPORTING
                    ev_end_datelocal         = l_rudat
                    ev_end_timelocal         = l_rutim
                  EXCEPTIONS
                    fatal_error              = 1
                    time_invalid             = 2
                    time_missing             = 3
                    tstream_not_loadable     = 4
                    tstream_generation_error = 5
                    parameter_error          = 6
                    unspecified_error        = 7
                    OTHERS                   = 8.

                "Seleciona equipamento.
                CLEAR: i_equnr.
                zcl_exc_apont_med=>get_equipamento( EXPORTING i_placa = placa  IMPORTING e_equnr = i_equnr ).

                TRY .
                    "Passando informações para estrutura zpme0058.
                    CLEAR: w_apont.
                    w_apont = VALUE #( id            = <_dados>-id
                                       address       = <w_abast_veic>-address
                                       code          = <w_abast_veic>-code
                                       equnr         = i_equnr
                                       motorista     = matricula
                                       dt_cupom_fisc = l_rudat
                                       hr_cupom_fisc = l_rutim
                                       placa         = placa
                                       odometro      = odometro
                                       cupom_fisc    = n_cupum
*                                   COD_MATERIAL  = ' '
                                       frota         = frota
                                       gru           = gru
                                       usuario       = sy-uname
                                       qtde          = lts_diesel
                                       vlr_total     = vlr_diesel
                                       lt_arla       = lts_arla ).

                    "Grava os dados na tabela de bkp dados combustivel.
                    MOVE-CORRESPONDING w_apont TO w_combust.
                    MODIFY zpmt0037 FROM w_combust.
                    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                      EXPORTING
                        wait = 'X'.

                  CATCH cx_root INTO DATA(ol_salv_msg).
                    DATA(vl_msg) = ol_salv_msg->get_longtext( ).

                    "Processa log.
                    zcl_exc_apont_med=>set_doc(
                            EXPORTING
                         w_zpme0058 =  w_apont   " Estrutra de dados para input de apontamento medição frota pr
                         i_msgty    =  'E'   " Campo do sistema: tipo de mensagem
                         i_msgv1    =  CONV #( vl_msg && '-> Erro Gerenrico ao processar dados' )  " Campo do sistema ABAP: variável da mensagem
                         i_item     = '99'
                     ).
                ENDTRY.

                "Seleciona ponto de medição do equpamento.
                IF i_equnr IS NOT INITIAL.
                  zcl_exc_apont_med=>select_pont_medicao(
                         EXPORTING
                     i_equnr =   i_equnr
                        RECEIVING
                    t_dimpt = DATA(t_dimpt) ).

                  "Processa documento medição.
                  IF t_dimpt IS NOT INITIAL.
                    zcl_exc_apont_med=>processa_documento(
                           EXPORTING
                      t_dimpt    =  t_dimpt
                      i_equnr    = i_equnr
                      w_zpme0058 = w_apont
                      p_sub_cont = ' '
                           IMPORTING
                        p_err = p_erro
                        t_log = t_doc ).
                  ELSE.
                    "Processa log.
                    zcl_exc_apont_med=>set_doc(
                      EXPORTING
                        w_zpme0058 =  w_apont   " Estrutra de dados para input de apontamento medição frota pr
                        i_msgty    =  'E'   " Campo do sistema: tipo de mensagem
                        i_msgv1    =  'Veiculo não possuem pontos de medição cadastrado'   " Campo do sistema ABAP: variável da mensagem
                        i_item     = '50'
                    ).
                  ENDIF.
                ELSE.
                  "Processa log.
                  zcl_exc_apont_med=>set_doc(
                          EXPORTING
                       w_zpme0058 =  w_apont   " Estrutra de dados para input de apontamento medição frota pr
                       i_msgty    =  'E'   " Campo do sistema: tipo de mensagem
                       i_msgv1    =  'Veiculo não cadastrado'   " Campo do sistema ABAP: variável da mensagem
                       i_item     = '60'
                   ).
                ENDIF.


              ENDIF.

*********
              IF p_erro IS INITIAL.
*      UPDATE ZPMT0024 SET CHECK_APONT = ABAP_TRUE
*      WHERE  FATURA EQ W_ZPMT0024-FATURA
*        AND  CNPJ   EQ W_ZPMT0024-CNPJ
*        AND  CUPOM_FISC EQ W_ZPMT0024-CUPOM_FISC.
*      COMMIT WORK.
              ELSE.

                "Caso tenha erro, estornar todos os documentos criados.
                IF t_doc IS NOT INITIAL.
                  CALL FUNCTION 'MEASUREM_DOCUM_CANCEL_ARRAY'
                    EXPORTING
                      messages_allowed       = abap_false
                    TABLES
                      cancel_requests        = t_doc
                    EXCEPTIONS
                      no_authority           = 1
                      foreign_lock_occured   = 2
                      system_failure_occured = 3
                      recursiveness_found    = 4
                      error_message          = 5
                      OTHERS                 = 6.

                  CHECK sy-subrc IS INITIAL.

                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = abap_true.
                ENDIF.
              ENDIF.

              CLEAR: p_erro.
            ENDLOOP.

          ELSE.
            CLEAR: w_zpme0064.
            /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = w_zpme0064 ).

            IF w_zpme0064 IS NOT INITIAL.
              CLEAR: w_apont.
              i_dt     = |{ dt_fim(4) }{ dt_fim+5(2) }{ dt_fim+8(2) }|.
              i_hr_fim = |{ hr_fim(2) }{ hr_fim+3(2) }{ hr_fim+6(2) }|.
              w_apont = VALUE #( address = <w_abast_veic>-address code  = <w_abast_veic>-code dt_cupom_fisc = i_dt hr_cupom_fisc = i_hr_fim msgv1 = _url ).

              "Processa log.
              zcl_exc_apont_med=>set_doc(
              EXPORTING
              w_zpme0058 =  w_apont   " Estrutra de dados para input de apontamento medição frota pr
              i_msgty    =  'E'   " Campo do sistema: tipo de mensagem
              i_msgv1    =  'Falha na comunicação com Autotrac '   && <w_abast_veic>-name" Campo do sistema ABAP: variável da mensagem
              i_item     = '99'
              ).
            ENDIF.

          ENDIF.
        ELSE.
          CLEAR: w_apont.
          i_dt     = |{ dt_fim(4) }{ dt_fim+5(2) }{ dt_fim+8(2) }|.
          i_hr_fim = |{ hr_fim(2) }{ hr_fim+3(2) }{ hr_fim+6(2) }|.
          w_apont = VALUE #( address = <w_abast_veic>-address code  = <w_abast_veic>-code dt_cupom_fisc = i_dt hr_cupom_fisc = i_hr_fim msgv1 = _url ).

          "Processa log.
          zcl_exc_apont_med=>set_doc(
          EXPORTING
          w_zpme0058 =  w_apont   " Estrutra de dados para input de apontamento medição frota pr
          i_msgty    =  'E'   " Campo do sistema: tipo de mensagem
          i_msgv1    =  'Dados não carregados'   " Campo do sistema ABAP: variável da mensagem
          i_item     = '99'
          ).

        ENDIF.

        FREE: t_dimpt.
      ELSE.

        CLEAR: w_apont.
*        i_dt     = |{ dt_fim(4) }{ dt_fim+5(2) }{ dt_fim+8(2) }|.
*        i_hr_fim = |{ hr_fim(2) }{ hr_fim+3(2) }{ hr_fim+6(2) }|.
        w_apont = VALUE #( address = <w_abast_veic>-address code  = <w_abast_veic>-code dt_cupom_fisc = i_dt hr_cupom_fisc = i_hr_fim msgv1 = _url ).

        "Processa log.
        zcl_exc_apont_med=>set_doc(
        EXPORTING
        w_zpme0058 =  w_apont   " Estrutra de dados para input de apontamento medição frota pr
        i_msgty    =  'E'   " Campo do sistema: tipo de mensagem
        i_msgv1    =  'URL Não esta cadastrada'   " Campo do sistema ABAP: variável da mensagem
        i_item     = '99'
        ).

      ENDIF.
    ENDLOOP.


*    "Passando para estrutura a data e hora da ultima executação.
    w_dt = VALUE #( id = i_id data = i_data_fim hora = i_hr_fim ).

    "Gravando a ultima executação na tabela.
    MODIFY zpmt0038 FROM w_dt.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDMETHOD.


  METHOD i_cons_logs.

    DATA: w_zpme0060 TYPE zpme0060.
    DATA: t_comb TYPE zpme0056.

    DATA: lva_equipment TYPE zpmt0044-equnr,
          lwa_eq_header TYPE alm_me_tob_header,
          lit_ret       TYPE bapiret2_tab.

    "Reprocessar mensagem pendentes.
    SELECT * FROM zpmt0035 INTO TABLE @DATA(it_zpmt0035)
    WHERE item EQ '99' AND estorno NE 'X'
    AND equnr IN @i_frota
    AND dt_cupom_fisc BETWEEN @i_perido_inic AND @i_perido_fim

*      i_frota
*      i_equnr
*      i_centro
*      i_perido_inic
*      i_perido_fim

      .
    SORT it_zpmt0035 BY code.
    DELETE zpmt0035 FROM TABLE it_zpmt0035.
    COMMIT WORK.

    t_comb-data = VALUE #( FOR l IN it_zpmt0035 ( address = |{ l-address ALPHA = OUT }| code = |{ l-code ALPHA = OUT }| ) ).

    SORT it_zpmt0035 BY dt_cupom_fisc.
    DELETE ADJACENT DUPLICATES FROM it_zpmt0035 COMPARING dt_cupom_fisc.


    LOOP AT it_zpmt0035 ASSIGNING FIELD-SYMBOL(<i_dados>).
      zcl_int_sappm_autotrac=>i_cons_dados_abast(
        EXPORTING
          t_dados_veic       =   t_comb  " Dados de abastecimento de combustivel
          i_data             = <i_dados>-dt_cupom_fisc
          i_hr_inic          = '000000'
          i_hr_fim           = '235900'
        IMPORTING
          t_dados_abast_veic =     DATA(t_dados) " Estrutura de dados consumo de combustivel SAP x Autotrac
      ).
    ENDLOOP.



    "Selecionar logs.
    SELECT * FROM zpmt0035 INTO TABLE @DATA(t_zpmt0035)
    WHERE frota IN @i_frota
    AND   equnr IN @i_equnr
    AND   estorno NE 'X'
    AND  dt_cupom_fisc BETWEEN @i_perido_inic AND @i_perido_fim
    AND  msgty IN @i_status.

    CHECK t_zpmt0035 IS NOT INITIAL.

    "Selecionando descrição frota.
    SELECT * FROM eqkt INTO TABLE @DATA(t_eqkt) FOR ALL ENTRIES IN @t_zpmt0035 WHERE equnr EQ @t_zpmt0035-equnr.

    "Selecionando descrição frota.
    MOVE-CORRESPONDING t_zpmt0035 TO t_logs.

    LOOP AT t_logs ASSIGNING FIELD-SYMBOL(<w_logs>).

      "Iserindo o nome do item.
      CASE <w_logs>-item.
        WHEN '10'.
          <w_logs>-desc_item = 'Odometro'.

        WHEN '20'.
          <w_logs>-desc_item = 'Combustível'.
        WHEN OTHERS.
      ENDCASE.

      READ TABLE t_eqkt ASSIGNING FIELD-SYMBOL(<w_eqkt>) WITH KEY equnr = <w_logs>-equnr.
      IF sy-subrc EQ 0.
        <w_logs>-equtx = <w_eqkt>-eqktx.
      ENDIF.

      DATA(icon_erro)   = icon_red_light.
      DATA(icon_exibir) = icon_display_text.
      DATA(icon_modif)  = icon_change.


      IF <w_logs>-msgty EQ 'E'.
        IF <w_logs>-item < '50'.
          <w_logs>-status = |{ icon_modif }|.
        ELSE.

          <w_logs>-status = |{ icon_exibir }|.
        ENDIF.
      ELSE.
        <w_logs>-status = icon_green_light.
      ENDIF.
* ---> CS1086697 / IR135761
*      CALL FUNCTION 'ALM_ME_EQUIPMENT_GETDETAIL'
*        EXPORTING
*          i_equipment    = <w_logs>-equnr
*        IMPORTING
*          e_equi_header  = lwa_eq_header
*        TABLES
*          return         = lit_ret
*        EXCEPTIONS
*          not_successful = 1
*          OTHERS         = 2.
* <--- CS1086697 / IR135761

    ENDLOOP.
  ENDMETHOD.


  METHOD m_check_pont_med.

    DATA:
*          wa_value TYPE imrg,
      v_medano TYPE dec_16_02_s,
      v_impt   TYPE impt,

      vl_cont  TYPE rihimrg-pyeac.

    CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST'
      EXPORTING
        buffer_bypass  = ' '
        dyfield        = ' '
        offset_date    = i_date
        offset_time    = i_time
        point          = i_point
      IMPORTING
        imrg_wa        = e_data
        impt_wa        = v_impt
      EXCEPTIONS
        imrg_not_found = 1
        OTHERS         = 2.

    IF sy-subrc IS INITIAL.


      "Converter. "USING wa_value-recdu wa_value-readg 'X' vl_cont.
*      f_unit
*      f_input
*      f_indik
*      f_flstr

      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          char_unit       = e_data-recdu "f_unit
          decimals        = 2
          exponent        = 0
          fltp_value_si   = e_data-readg "f_input
          indicator_value = 'X' "f_indik
        IMPORTING
          char_value      = vl_cont "f_flstr
        EXCEPTIONS
          no_unit_given   = 01.

      REPLACE ALL OCCURRENCES OF ',' IN vl_cont WITH '.' .
      MOVE vl_cont TO e_value.

    ENDIF.
  ENDMETHOD."


  METHOD M_CLEAR_ID_ITEM.

    "Delete o item da tabela para ser reprocessado novamente.
    DELETE FROM ZPMT0035 WHERE ID EQ W_ZPME0060-ID AND ITEM EQ W_ZPME0060-ITEM.
    COMMIT WORK.
  ENDMETHOD.


  METHOD m_estorno_doc_med.

    DATA: t_canc_req TYPE TABLE OF imrg_mdocm.
    IF e_doc_med IS NOT INITIAL.
      APPEND VALUE #( mdocm = e_doc_med ) TO t_canc_req.
    ENDIF.

    IF t_canc_req IS NOT INITIAL.
      CALL FUNCTION 'MEASUREM_DOCUM_CANCEL_ARRAY'
        EXPORTING
          messages_allowed       = abap_false
        TABLES
          cancel_requests        = t_canc_req
        EXCEPTIONS
          no_authority           = 1
          foreign_lock_occured   = 2
          system_failure_occured = 3
          recursiveness_found    = 4
          error_message          = 5
          OTHERS                 = 6.

      IF sy-subrc EQ 0.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        e_msgty = 'S'.
        r_return = 'Documento -' && e_doc_med && ' estornado com sucesso -' && e_doc_med.
      ELSE.
        e_msgty = 'E'.
        r_return = 'Erro ao estornar o documento - ' && e_doc_med.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD M_GET_DT_HR_PROC.
    SELECT MAX( ID ) FROM ZPMT0038 INTO I_ID.

    IF I_ID IS NOT INITIAL.
      SELECT SINGLE *
      FROM ZPMT0038
        INTO @DATA(L_ZPMT0038)
        WHERE ID EQ @I_ID.
    ENDIF.

    IF L_ZPMT0038 IS NOT INITIAL.
      I_HORA  = L_ZPMT0038-HORA.
      I_DATA  = L_ZPMT0038-DATA.
    ENDIF.

    ADD 1 TO I_ID.

    IF I_DATA IS NOT INITIAL. "Se achar a data e hora da ultima executação, acrescenta mais um minuto, na proxima executação.
      DATA: W_DATA TYPE D, W_HORA TYPE T.
      DATA: W_RUDAT TYPE TVPOD-RUDAT, W_RUTIM TYPE TVPOD-RUTIM.
      W_DATA = I_DATA.
      W_HORA = I_HORA.

      "Função para acrescentar + 1 minuto.
      CALL FUNCTION 'TSTR_CALC_TIME'
        EXPORTING
*         IV_CALENDARID            = '99'
          IV_BEGIN_DATELOCAL_REQ   = W_DATA
          IV_BEGIN_TIMELOCAL_REQ   = W_HORA
          IV_DURATION_INTEGER      = 1
          IV_DIRECTION             = '+'
        IMPORTING
          EV_END_DATELOCAL         = W_RUDAT
          EV_END_TIMELOCAL         = W_RUTIM
        EXCEPTIONS
          FATAL_ERROR              = 1
          TIME_INVALID             = 2
          TIME_MISSING             = 3
          TSTREAM_NOT_LOADABLE     = 4
          TSTREAM_GENERATION_ERROR = 5
          PARAMETER_ERROR          = 6
          UNSPECIFIED_ERROR        = 7
          OTHERS                   = 8.


      I_DATA     = W_RUDAT. "Data Inicio
      I_HORA     = W_RUTIM. "Hora Inicio
      i_data_fim = sy-datum. "Data Fim
      I_HR_FIM   = SY-UZEIT. "Hora Fim

    ELSE.
      I_DATA     = SY-DATUM.
      I_HORA     = '000000'.
      I_HR_FIM   = SY-UZEIT.
    ENDIF.
    CLEAR: W_DATA, W_HORA, W_RUDAT, W_RUTIM.
**================================================================================
  ENDMETHOD.


  METHOD M_REGIST_JUSTIF.

    DATA: W_ZPMT0036 TYPE ZPMT0036.


    W_ZPMT0036 = VALUE #( ID              = W_ZPME0060-ID
                          EQUNR           = W_ZPME0060-EQUNR
                          PLACA           = W_ZPME0060-PLACA
                          FROTA           = W_ZPME0060-FROTA
                          USUARIO         = W_ZPME0060-USUARIO
                          ODOMETRO        = I_ODOMETRO
                          ODOMETRO_ATUAL  = W_ZPME0060-ODOMETRO
                          MSGV1           = I_LINE
                          ).


    MODIFY ZPMT0036 FROM W_ZPMT0036.
    COMMIT WORK.
  ENDMETHOD.


  METHOD M_REPROC_INFORM.

    DATA: W_APONT TYPE ZPME0058,
          T_DOC   TYPE TABLE OF IMRG_MDOCM,
          P_ERRO.

    CLEAR: W_APONT.
    W_APONT = VALUE #(
                                     ID            = W_ZPME0060-ID
                                     ADDRESS       = W_ZPME0060-ADDRESS
                                     CODE          = W_ZPME0060-CODE
                                     ITEM          = W_ZPME0060-ITEM
                                     EQUNR         = I_EQUNR
                                     DT_CUPOM_FISC = W_ZPME0060-DT_CUPOM_FISC
                                     HR_CUPOM_FISC = W_ZPME0060-HR_CUPOM_FISC
                                     PLACA         = W_ZPME0060-PLACA
                                     ODOMETRO      = W_ZPME0060-ODOMETRO
                                     CUPOM_FISC    = W_ZPME0060-CUPOM_FISC
                                     QTDE          = W_ZPME0060-QTDE
                                     FROTA         = W_ZPME0060-FROTA
                                     GRU           = W_ZPME0060-GRU
                                     USUARIO       = SY-UNAME ).


    "Seleciona ponto de medição do equpamento.
    IF I_EQUNR IS NOT INITIAL.
      ZCL_EXC_APONT_MED=>SELECT_PONT_MEDICAO(
             EXPORTING
         I_EQUNR =   I_EQUNR
            RECEIVING
        T_DIMPT = DATA(T_DIMPT) ).

      "Processa documento medição.
      IF T_DIMPT IS NOT INITIAL.
        ZCL_EXC_APONT_MED=>PROCESSA_DOCUMENTO(
               EXPORTING
          T_DIMPT =  T_DIMPT
          I_EQUNR = W_ZPME0060-EQUNR
          W_ZPME0058 = W_APONT
          P_SUB_CONT = P_SUB_CONT
               IMPORTING
            P_ERR = P_ERRO
            T_LOG = T_DOC ).
      ELSE.
        "Processa log.
        ZCL_EXC_APONT_MED=>SET_DOC(
          EXPORTING
            W_ZPME0058 =  W_APONT   " Estrutra de dados para input de apontamento medição frota pr
            I_MSGTY    =  'E'   " Campo do sistema: tipo de mensagem
            I_MSGV1    =  'Veiculo não possuem pontos de medição cadastrado'   " Campo do sistema ABAP: variável da mensagem
            I_ITEM     = '50'
        ).
      ENDIF.
    ELSE.
      "Processa log.
      ZCL_EXC_APONT_MED=>SET_DOC(
              EXPORTING
           W_ZPME0058 =  W_APONT   " Estrutra de dados para input de apontamento medição frota pr
           I_MSGTY    =  'E'   " Campo do sistema: tipo de mensagem
           I_MSGV1    =  'Veiculo não cadastrado'   " Campo do sistema ABAP: variável da mensagem
           I_ITEM     = '60'
       ).

    ENDIF.


    IF P_ERRO IS INITIAL.
*      UPDATE ZPMT0024 SET CHECK_APONT = ABAP_TRUE
*      WHERE  FATURA EQ W_ZPMT0024-FATURA
*        AND  CNPJ   EQ W_ZPMT0024-CNPJ
*        AND  CUPOM_FISC EQ W_ZPMT0024-CUPOM_FISC.
*      COMMIT WORK.
    ELSE.

      "Caso tenha erro, estornar todos os documentos criados.
      IF T_DOC IS NOT INITIAL.
        CALL FUNCTION 'MEASUREM_DOCUM_CANCEL_ARRAY'
          EXPORTING
            MESSAGES_ALLOWED       = ABAP_FALSE
          TABLES
            CANCEL_REQUESTS        = T_DOC
          EXCEPTIONS
            NO_AUTHORITY           = 1
            FOREIGN_LOCK_OCCURED   = 2
            SYSTEM_FAILURE_OCCURED = 3
            RECURSIVENESS_FOUND    = 4
            ERROR_MESSAGE          = 5
            OTHERS                 = 6.

        CHECK SY-SUBRC IS INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = ABAP_TRUE.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
