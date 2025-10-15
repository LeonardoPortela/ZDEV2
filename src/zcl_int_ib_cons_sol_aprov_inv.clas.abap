CLASS zcl_int_ib_cons_sol_aprov_inv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_zim01,
        bukrs        TYPE zim01_sol_ap_inv-bukrs,
        gsber        TYPE zim01_sol_ap_inv-gsber,
        gtext        TYPE tgsbt-gtext,
        ano          TYPE zim01_sol_ap_inv-ano,
        safra        TYPE zim01_sol_ap_inv-safra,
        kostl        TYPE zim01_sol_ap_inv-kostl,
        izwek        TYPE zim01_sol_ap_inv-izwek,
        txt50        TYPE zim01_sol_ap_inv-txt50,
        finalidade   TYPE zim01_sol_ap_inv-finalidade,
        objetivo     TYPE zim01_sol_ap_inv-objetivo,
        descr_item   TYPE zim01_sol_ap_inv-descr_item,
        menge        TYPE zim01_sol_ap_inv-menge,
        vlr_unitario TYPE zim01_sol_ap_inv-vlr_unitario,
        vlr_total    TYPE zim01_sol_ap_inv-vlr_total,
        vl_usd       TYPE zim01_sol_ap_inv-vl_usd,
        tx_usd       TYPE zim01_sol_ap_inv-tx_usd,
        vl_eur       TYPE zim01_sol_ap_inv-vl_eur,
        tx_eur       TYPE zim01_sol_ap_inv-tx_eur,
        dt_inicio    TYPE zim01_sol_ap_inv-dt_inicio,
        dt_fim       TYPE zim01_sol_ap_inv-dt_fim,
        ano_fim_exec TYPE zim01_sol_ap_inv-ano_fim_exec,
        knttp        TYPE zim01_sol_ap_inv-knttp,
        knttx        TYPE zim01_sol_ap_inv-knttx,
        observacoes  TYPE zim01_sol_ap_inv-observacoes,
        saknr        TYPE zim01_sol_ap_inv-saknr,
        txt20        TYPE zim01_sol_ap_inv-txt20,
        posnr        TYPE zim01_sol_ap_inv-posnr,
        status_aprov TYPE zim01_sol_ap_inv-status_aprov,
        dt_aprovacao TYPE zim01_sol_ap_inv-dt_aprovacao,
        aprovador    TYPE zim01_sol_ap_inv-aprovador,
        usuario      TYPE zim01_sol_ap_inv-usuario,
        data_entr    TYPE zim01_sol_ap_inv-data_entr,
        hora_entr    TYPE zim01_sol_ap_inv-hora_entr,
        data_mod     TYPE zim01_sol_ap_inv-data_mod,
        hora_mod     TYPE zim01_sol_ap_inv-hora_mod,
        usuario_im   TYPE zim01_sol_ap_inv-usuario_im,
        data_entr_im TYPE zim01_sol_ap_inv-data_entr_im,
      END OF ty_zim01,

      BEGIN OF ty_zim02,
        bukrs       TYPE zim02_sol_ap_ctl-bukrs,
        ano         TYPE zim02_sol_ap_ctl-ano,
        safra       TYPE zim02_sol_ap_ctl-safra,
        safra2      TYPE zim02_sol_ap_ctl-safra2,
        kostl       TYPE zim02_sol_ap_ctl-kostl,
        aprovador   TYPE zim02_sol_ap_ctl-aprovador,
        atividade   TYPE zim02_sol_ap_ctl-atividade,
        responsavel TYPE zim02_sol_ap_ctl-responsavel,
      END OF ty_zim02,

      BEGIN OF ty_cskt,
        spras TYPE cskt-spras,
        kokrs TYPE cskt-kokrs,
        kostl TYPE cskt-kostl,
        datbi TYPE cskt-datbi,
        ktext TYPE cskt-ktext,
        ltext TYPE cskt-ltext,
        mctxt TYPE cskt-mctxt,
      END OF ty_cskt,

      BEGIN OF ty_usr21,
        bname      TYPE usr21-bname,
        persnumber TYPE usr21-persnumber,
        name_text  TYPE adrp-name_text,
        smtp_addr  TYPE adr6-smtp_addr,
      END OF ty_usr21.

    DATA:
      BEGIN OF z_gen_table,
        cd_empresa                   TYPE zim01_sol_ap_inv-bukrs,
        cd_filial                    TYPE zim01_sol_ap_inv-gsber,
        ds_filial                    TYPE tgsbt-gtext,
        ano                          TYPE zim01_sol_ap_inv-ano,
        safra                        TYPE zim01_sol_ap_inv-safra,
        cd_centro_custo              TYPE zim01_sol_ap_inv-kostl,
        ds_centro_custo              TYPE cskt-ltext,
        atividade                    TYPE zim02_sol_ap_ctl-atividade,
        finalidade(30)               TYPE c,
        ds_cat_investimento(100)     TYPE c,
        objetivo                     TYPE zim01_sol_ap_inv-objetivo,
        ds_item                      TYPE zim01_sol_ap_inv-descr_item,
        qt_item                      TYPE zim01_sol_ap_inv-menge,
        vl_unitario_rs               TYPE zim01_sol_ap_inv-vlr_unitario,
        vl_total_rs                  TYPE zim01_sol_ap_inv-vlr_total,
        vl_total_usd                 TYPE zim01_sol_ap_inv-vl_usd,
        vl_taxa_usd                  TYPE zim01_sol_ap_inv-tx_usd,
        vl_total_eur                 TYPE zim01_sol_ap_inv-vl_eur,
        vl_taxa_eur                  TYPE zim01_sol_ap_inv-tx_eur,
        dt_inicio                    TYPE zim01_sol_ap_inv-dt_inicio,
        dt_fim                       TYPE zim01_sol_ap_inv-dt_fim,
        ano_fim_exec                 TYPE zim01_sol_ap_inv-ano_fim_exec,
        ds_class_contabil(100)       TYPE c,
        obs_contabil                 TYPE zim01_sol_ap_inv-observacoes,
        nr_conta_contabil(20)        TYPE c,
        ds_conta_contabil            TYPE zim01_sol_ap_inv-txt20,
        nr_solicitacao               TYPE zim01_sol_ap_inv-posnr,
        ds_status_aprov(20)          TYPE c,
        dt_aprovacao                 TYPE zim01_sol_ap_inv-dt_aprovacao,
        aprovador(12)                TYPE c,
        usuario                      TYPE zim01_sol_ap_inv-usuario,
        nome                         TYPE adrp-name_text,
        email                        TYPE adr6-smtp_addr,
        dt_entrada                   TYPE zim01_sol_ap_inv-data_entr,
        hr_entrada                   TYPE zim01_sol_ap_inv-hora_entr,
        dt_modificacao               TYPE zim01_sol_ap_inv-data_mod,
        hr_modificacao               TYPE zim01_sol_ap_inv-hora_mod,
        ds_usuario_contabil          TYPE zim01_sol_ap_inv-usuario_im,
        dt_contabil                  TYPE zim01_sol_ap_inv-data_entr_im,
        responsavel_centro_custo(20) TYPE c,


*        bukrs        TYPE zim01_sol_ap_inv-bukrs,
*        gsber        TYPE zim01_sol_ap_inv-gsber,
*        gtext        TYPE tgsbt-gtext,
*        ano          TYPE zim01_sol_ap_inv-ano,
*        safra        TYPE zim01_sol_ap_inv-safra,
*        kostl        TYPE zim01_sol_ap_inv-kostl,
*        izwek        TYPE zim01_sol_ap_inv-izwek,
*        txt50        TYPE zim01_sol_ap_inv-txt50,
*        objetivo     TYPE zim01_sol_ap_inv-objetivo,
*        descr_item   TYPE zim01_sol_ap_inv-descr_item,
*        menge        TYPE zim01_sol_ap_inv-menge,
*        vlr_unitario TYPE zim01_sol_ap_inv-vlr_unitario,
*        vlr_total    TYPE zim01_sol_ap_inv-vlr_total,
*        vl_usd       TYPE zim01_sol_ap_inv-vl_usd,
*        tx_usd       TYPE zim01_sol_ap_inv-tx_usd,
*        vl_eur       TYPE zim01_sol_ap_inv-vl_eur,
*        tx_eur       TYPE zim01_sol_ap_inv-tx_eur,
*        dt_inicio    TYPE zim01_sol_ap_inv-dt_inicio,
*        dt_fim       TYPE zim01_sol_ap_inv-dt_fim,
*        ano_fim_exec TYPE zim01_sol_ap_inv-ano_fim_exec,
*        knttp        TYPE zim01_sol_ap_inv-knttp,
*        knttx        TYPE zim01_sol_ap_inv-knttx,
*        observacoes  TYPE zim01_sol_ap_inv-observacoes,
*        txt20        TYPE zim01_sol_ap_inv-txt20,
*        posnr        TYPE zim01_sol_ap_inv-posnr,
*        dt_aprovacao TYPE zim01_sol_ap_inv-dt_aprovacao,
*        usuario      TYPE zim01_sol_ap_inv-usuario,
*        data_entr    TYPE zim01_sol_ap_inv-data_entr,
*        hora_entr    TYPE zim01_sol_ap_inv-hora_entr,
*        data_mod     TYPE zim01_sol_ap_inv-data_mod,
*        hora_mod     TYPE zim01_sol_ap_inv-hora_mod,
*        usuario_im   TYPE zim01_sol_ap_inv-usuario_im,
*        data_entr_im TYPE zim01_sol_ap_inv-data_entr_im,
*        spras        TYPE cskt-spras,
*        kokrs        TYPE cskt-kokrs,
*        kostl1       TYPE cskt-kostl,
*        datbi        TYPE cskt-datbi,
*        ktext        TYPE cskt-ktext,
*        ltext        TYPE cskt-ltext,
*        mctxt        TYPE cskt-mctxt,
      END OF z_gen_table.
    DATA: it_zim01 TYPE STANDARD TABLE OF ty_zim01,
          it_zim02 TYPE STANDARD TABLE OF ty_zim02,
          it_cskt  TYPE STANDARD TABLE OF ty_cskt,
          it_usr21 TYPE STANDARD TABLE OF ty_usr21,
          gs_zim01 TYPE ty_zim01.
    DATA:
      tb_ano            TYPE TABLE OF gjahr,
      tb_gsber          TYPE TABLE OF gsber,
      tb_kostl          TYPE TABLE OF kostl,
      tb_status_aprov   TYPE TABLE OF zim01_sol_ap_inv-status_aprov,
      tb_nr_solicitacao TYPE TABLE OF zim01_sol_ap_inv-posnr.

    DATA:
      BEGIN OF zde_data_request,
        ano            LIKE tb_ano,
        gsber          LIKE tb_gsber,
        kostl          LIKE tb_kostl,
        status_aprov   LIKE tb_status_aprov,
        nr_solicitacao LIKE tb_nr_solicitacao,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        solicitacoes LIKE TABLE OF z_gen_table,

      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '160' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_SOL_APROV_INV IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    DATA: lva_reason TYPE STRING,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code is NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          OUTPUT        = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code         = lva_code
        IMPORTING
          E_DESC_STATUS  = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = conv #( lva_code )
          reason = conv #( lva_reason )
       ).

    endif.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_inbound~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno( IMPORTING e_data_retorno = DATA(e_data_retorno) e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    me->zif_integracao_inbound~at_zintegracao_log = e_zintegracao_log.

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND.
*
    DATA: lwa_data_request LIKE zde_data_request.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_post.
      r_msg_erro     = 'Metodo informado não previsto!'.
      e_status_code  = '405'. "Method Not Allowed
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lwa_data_request ).

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*
    IF lwa_data_request IS INITIAL.
      r_msg_erro = 'Nenhum filtro foi informado!'.
      RETURN.
    ENDIF.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response LIKE zde_data_response.

    TYPES: lr_kokrs_t TYPE RANGE OF cskt-kokrs.
    DATA: lr_kokrs   TYPE RANGE OF cskt-kokrs,
          lra_nr_sol TYPE RANGE OF zim01_sol_ap_inv-posnr.

    lr_kokrs = VALUE lr_kokrs_t(
              LET s = 'I'
                  o = 'EQ'
              IN sign   = s
                 option = o
                 ( low = 'MAGI' )
                 ( low = 'MGBG' )
                 ( low = 'MGLD' )
                 ( low = 'MGTF' )
    ).

    DATA: lr_ano          TYPE RANGE OF gjahr,
          lr_gsber        TYPE RANGE OF gsber,
          lr_kostl        TYPE RANGE OF kostl,
          lr_status_aprov TYPE RANGE OF zim01_sol_ap_inv-status_aprov.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, lwa_data_response.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_request ).
    ENDIF.

    me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound =  i_msg_inbound IMPORTING e_status_code  =  DATA(_status_code)  RECEIVING r_msg_erro = e_msg_erro ).

    IF e_msg_erro IS NOT INITIAL.

      IF _status_code IS INITIAL .
        _status_code = '400'. "Bad Request
      ENDIF.

      e_sucesso      = abap_true.
      e_nm_code      = _status_code.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.
    ENDIF.

    "----->> Begin of change


    LOOP AT lwa_data_request-ano INTO DATA(wa_ano).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_ano ) TO lr_ano.
    ENDLOOP.
    LOOP AT lwa_data_request-gsber INTO DATA(wa_gsber).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_gsber ) TO lr_gsber.
    ENDLOOP.
    LOOP AT lwa_data_request-kostl INTO DATA(wa_kostl).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_kostl
        IMPORTING
          output = wa_kostl.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_kostl ) TO lr_kostl.
    ENDLOOP.

    LOOP AT lwa_data_request-status_aprov INTO DATA(wa_status_aprov).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_status_aprov ) TO lr_status_aprov.
    ENDLOOP.
    LOOP AT lwa_data_request-nr_solicitacao INTO DATA(wa_nr_sol).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_nr_sol ) TO lra_nr_sol.
    ENDLOOP.

    SELECT z01~bukrs
           z01~gsber
           z02~gtext
           z01~ano
           z01~safra
           z01~kostl
           z01~izwek
           z01~txt50
           z01~finalidade
           z01~objetivo
           z01~descr_item
           z01~menge
           z01~vlr_unitario
           z01~vlr_total
           z01~vl_usd
           z01~tx_usd
           z01~vl_eur
           z01~tx_eur
           z01~dt_inicio
           z01~dt_fim
           z01~ano_fim_exec
           z01~knttp
           z01~knttx
           z01~observacoes
           z01~saknr
           z01~txt20
           z01~posnr
           z01~status_aprov
           z01~dt_aprovacao
           z01~aprovador
           z01~usuario
           z01~data_entr
           z01~hora_entr
           z01~data_mod
           z01~hora_mod
           z01~usuario_im
           z01~data_entr_im
      INTO CORRESPONDING FIELDS OF TABLE it_zim01
      FROM zim01_sol_ap_inv   AS      z01
      INNER JOIN  tgsbt       AS      z02
       ON z02~gsber EQ z01~gsber
       AND z02~spras EQ 'P'
      WHERE z01~ano   IN lr_ano
        AND z01~gsber IN lr_gsber
        AND z01~kostl IN lr_kostl
        AND z01~status_aprov IN lr_status_aprov
        AND z01~posnr IN lra_nr_sol.

    IF sy-subrc IS INITIAL.
      SORT it_zim01 BY bukrs ano safra kostl.

      SELECT bukrs
             ano
             safra
             safra2
             kostl
             aprovador
             atividade
             responsavel
           FROM zim02_sol_ap_ctl
           INTO TABLE it_zim02
           FOR ALL ENTRIES IN it_zim01
           WHERE bukrs = it_zim01-bukrs
             AND ano   = it_zim01-ano
             AND safra = it_zim01-safra
             AND kostl = it_zim01-kostl.
      IF sy-subrc IS INITIAL.
        SORT it_zim02 BY bukrs ano safra kostl.

      ENDIF.

      SELECT us~bname
             ad~persnumber
             ed~addrnumber
             ad~name_text
             ed~smtp_addr
         FROM usr21 AS us
         INNER JOIN adrp AS ad
         ON ad~persnumber EQ us~persnumber
         INNER JOIN adr6 AS ed
         ON ed~persnumber EQ us~persnumber
         AND ed~addrnumber EQ us~addrnumber
         INTO CORRESPONDING FIELDS OF TABLE it_usr21
         FOR ALL ENTRIES IN it_zim01
         WHERE bname EQ it_zim01-usuario.
      IF sy-subrc IS INITIAL.
        SORT it_usr21 BY bname.
      ENDIF.

      SELECT spras
             kokrs
             kostl
             datbi
             ktext
             ltext
             mctxt
               FROM cskt AS cc
        INTO TABLE it_cskt
        FOR ALL ENTRIES IN it_zim01
              WHERE kostl EQ it_zim01-kostl
                AND cc~spras = 'P'
                AND cc~kokrs IN lr_kokrs
                AND cc~datbi <= '99991231'
                AND cc~datbi = ( SELECT MAX( datbi )
                                   FROM cskt AS cc2
                                  WHERE cc2~mandt = cc~mandt
                                    AND cc2~spras = cc~spras
                                    AND cc2~kokrs = cc~kokrs
                                    AND cc2~kostl = cc~kostl ).
      IF sy-subrc IS INITIAL.
        SORT it_cskt BY kostl.
      ENDIF.


    ENDIF.


    LOOP AT it_zim01 INTO DATA(ls_zim01).
      APPEND INITIAL LINE TO lwa_data_response-solicitacoes ASSIGNING FIELD-SYMBOL(<fs_response>).

      READ TABLE it_zim02 INTO DATA(ls_zim02) WITH KEY bukrs = ls_zim01-bukrs
                                                       ano   = ls_zim01-ano
                                                       safra = ls_zim01-safra
                                                       kostl = ls_zim01-kostl BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_response>-atividade                =  ls_zim02-atividade.
        <fs_response>-responsavel_centro_custo =  ls_zim02-responsavel.
      ENDIF.


      <fs_response>-cd_empresa                 =  ls_zim01-bukrs.
      <fs_response>-cd_filial                  =  ls_zim01-gsber.
      <fs_response>-ds_filial                  =  ls_zim01-gtext.
      <fs_response>-ano                        =  ls_zim01-ano.
      <fs_response>-safra                      =  ls_zim01-safra.
      <fs_response>-cd_centro_custo            =  ls_zim01-kostl.
      READ TABLE it_cskt INTO DATA(ls_cskt) WITH KEY kostl = ls_zim01-kostl BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_response>-ds_centro_custo          =  ls_cskt-ltext.
      ENDIF.

      CASE ls_zim01-finalidade.
        WHEN '01'.
          <fs_response>-finalidade             = '01 - Meio ambiente'.
        WHEN '02'.
          <fs_response>-finalidade             = '02 - Obrigacao Legal'.
        WHEN '03'.
          <fs_response>-finalidade             = '03 - Sinistro'.
        WHEN '04'.
          <fs_response>-finalidade             = '04 - Reserva'.
        WHEN '05'.
          <fs_response>-finalidade             = '05 - Outros'.
      ENDCASE.
      CONCATENATE ls_zim01-izwek ls_zim01-txt50 INTO <fs_response>-ds_cat_investimento SEPARATED BY '-'.

      <fs_response>-objetivo                   =  ls_zim01-objetivo.
      <fs_response>-ds_item                    =  ls_zim01-descr_item.
      <fs_response>-qt_item                    =  ls_zim01-menge.
      <fs_response>-vl_unitario_rs             =  ls_zim01-vlr_unitario.
      <fs_response>-vl_total_rs                =  ls_zim01-vlr_total.
      <fs_response>-vl_total_usd               =  ls_zim01-vl_usd.
      <fs_response>-vl_taxa_usd                =  ls_zim01-tx_usd.
      <fs_response>-vl_total_eur               =  ls_zim01-vl_eur.
      <fs_response>-vl_taxa_eur                =  ls_zim01-tx_eur.
      <fs_response>-dt_inicio                  =  ls_zim01-dt_inicio.
      <fs_response>-dt_fim                     =  ls_zim01-dt_fim.
      <fs_response>-ano_fim_exec               =  ls_zim01-ano_fim_exec.
      CONCATENATE ls_zim01-knttp ls_zim01-knttx INTO <fs_response>-ds_class_contabil SEPARATED BY '-'.

      <fs_response>-obs_contabil               =  ls_zim01-observacoes.
      <fs_response>-nr_conta_contabil          =  ls_zim01-saknr.
      <fs_response>-ds_conta_contabil          =  ls_zim01-txt20.
      <fs_response>-nr_solicitacao             =  ls_zim01-posnr.
      CASE ls_zim01-status_aprov.
        WHEN '1'.
          <fs_response>-ds_status_aprov        = 'Aprovado'.
        WHEN '2'.
          <fs_response>-ds_status_aprov        = 'Reprovado'.
        WHEN '3'.
          <fs_response>-ds_status_aprov        = 'Bloqueado'.
      ENDCASE.
*
      <fs_response>-dt_aprovacao               =  ls_zim01-dt_aprovacao.
      IF ls_zim01-aprovador EQ space.
        <fs_response>-aprovador                = ls_zim02-aprovador.
      ELSE.
        <fs_response>-aprovador                = ls_zim01-aprovador.
      ENDIF.
      <fs_response>-usuario                    =  ls_zim01-usuario.
      <fs_response>-dt_entrada                 =  ls_zim01-data_entr.
      <fs_response>-hr_entrada                 =  ls_zim01-hora_entr.
      <fs_response>-dt_modificacao             =  ls_zim01-data_mod.
      <fs_response>-hr_modificacao             =  ls_zim01-hora_mod.
      <fs_response>-ds_usuario_contabil        =  ls_zim01-usuario_im.
      <fs_response>-dt_contabil                =  ls_zim01-data_entr_im.


      READ TABLE it_usr21 INTO DATA(ls_usr21) WITH KEY bname = ls_zim01-usuario BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_response>-nome                     = ls_usr21-name_text.
        <fs_response>-email                    = ls_usr21-smtp_addr.
      ENDIF.

    ENDLOOP.


    "ENDIF.
    "----->> End of change
    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.
ENDCLASS.
