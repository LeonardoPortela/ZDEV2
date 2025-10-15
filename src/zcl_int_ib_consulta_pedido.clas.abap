CLASS zcl_int_ib_consulta_pedido DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject.
    INTERFACES zif_integracao_inbound .

    TYPES: BEGIN OF ty_aedat,
             inicio TYPE dats,
             fim    TYPE dats,
           END OF ty_aedat,

           BEGIN OF ty_bedat,
             inicio TYPE dats,
             fim    TYPE dats,
           END OF ty_bedat.



    DATA:
      tb_ebeln    TYPE TABLE OF ebeln,
      tb_lifnr    TYPE TABLE OF elifn,
      tb_ebelp    TYPE TABLE OF ebelp,
      tb_werks    TYPE TABLE OF ewerk,
      tb_bewtp    TYPE TABLE OF bewtp,
      tb_vgabe    TYPE TABLE OF vgabe,
      tb_lifn2    TYPE TABLE OF lifn2,
      tb_parvw    TYPE TABLE OF parvw,
      tb_bukrs    TYPE TABLE OF bukrs,
      tb_bsart    TYPE TABLE OF bsart,
      tb_loekz    TYPE TABLE OF loekz,
      tb_shkzg    TYPE TABLE OF shkzg,
      tb_matnr    TYPE TABLE OF matnr,
      tb_no_loekz TYPE TABLE OF loekz.


    TYPES:

      BEGIN OF ty_dados_fornecedor,
        nome               TYPE adrc-name1,
        endereco           TYPE adrc-street,
        municipio          TYPE adrc-city1,
        estado             TYPE adrc-region,
        bairro             TYPE adrc-city2,
        end_numero         TYPE adrc-house_num1,
        cnpj               TYPE lfa1-stcd1,
        cpf                TYPE lfa1-stcd2,
        inscricao_estadual TYPE lfa1-stcd3,
        pessoa_fisica      TYPE lfa1-stkzn,
      END OF ty_dados_fornecedor,

      BEGIN OF ty_user_lan,
        cpf  TYPE adr3-fax_number,
        nome TYPE user_addr-name_textc,
      END OF ty_user_lan,

      BEGIN OF tl_cabecalho,
        ekko                        TYPE ekko,
        dados_fornecedor            TYPE ty_dados_fornecedor,
        ds_empresa                  TYPE t001-butxt,
        usuario_lancamento          TYPE ty_user_lan,
        ds_usuario_aprovador_ultimo TYPE user_addr-name_textc,
      END OF tl_cabecalho,

      BEGIN OF ty_dados_mat,
        tp_material         TYPE mtart,
        ds_material         TYPE maktx,
        ds_grupo_mercadoria TYPE wgbez,
      END OF ty_dados_mat,

      BEGIN OF ty_dados_centro,
        endereco    TYPE t001w-stras,
        cidade      TYPE t001w-ort01,
        uf          TYPE t001w-regio,
        bairro      TYPE adrc-city2,
        endereco_nr TYPE adrc-house_num1,
        endereco_2  TYPE kna1-stras,
        cnpj        TYPE kna1-stcd1,
      END OF ty_dados_centro,

      BEGIN OF it_itens,
        ekpo           TYPE EKPO,
        dados_material TYPE ty_dados_mat,
        dados_centro   TYPE ty_dados_centro,
      END OF it_itens,

      BEGIN OF ty_historico,
        ekbe                  TYPE ekbe,
        doc_mat_estornado(1),
        doc_miro_estornado(1),
      END OF ty_historico,

      BEGIN OF ty_class_contabil,
        ebelp                       TYPE  ekkn-ebelp,
        sakto                       TYPE  ekkn-sakto,
        gsber                       TYPE  ekkn-gsber,
        kostl                       TYPE  ekkn-kostl,
        kokrs                       TYPE  ekkn-kokrs,
        aufnr                       TYPE  ekkn-aufnr,
        anln1                       TYPE  ekkn-anln1,
        anln2                       TYPE  ekkn-anln2,
        ds_conta_contabil           TYPE skat-txt50,
        ds_divisao                  TYPE tgsbt-gtext,
        ds_centro_custo             TYPE cskt-ltext,
        ds_centro_custo_responsavel TYPE csks-verak,
      END OF ty_class_contabil,

      BEGIN OF ty_cabecalho_miro,
        belnr TYPE rbkp-belnr,
        gjahr TYPE rbkp-gjahr,
        budat TYPE rbkp-budat,
        stblg TYPE rbkp-stblg,
      END OF ty_cabecalho_miro,

      BEGIN OF ty_itens_miro,
        belnr TYPE rseg-belnr,
        gjahr TYPE rseg-gjahr,
        xblnr TYPE rseg-xblnr,
      END OF ty_itens_miro.


    DATA:
      BEGIN OF ty_filter_header,
        ebeln    LIKE tb_ebeln,
        lifnr    LIKE tb_lifnr,
        bukrs    LIKE tb_bukrs,
        bsart    LIKE tb_bsart,
        aedat TYPE  ty_aedat,
        bedat TYPE ty_bedat,
      END OF ty_filter_header,

      BEGIN OF ty_items_filters,
        ebelp    LIKE tb_ebelp,
        werks    LIKE tb_werks,
        matnr    LIKE tb_matnr,
        loekz    LIKE tb_loekz,
        no_loekz LIKE tb_no_loekz,
      END OF ty_items_filters,

      BEGIN OF ty_historic_filters,
        bewtp LIKE tb_bewtp,
        vgabe LIKE tb_vgabe,
        shkzg LIKE tb_shkzg,
      END OF ty_historic_filters,

      BEGIN OF ty_partners_filters,
        lifn2 LIKE tb_lifn2,
        parvw LIKE tb_parvw,
      END OF ty_partners_filters.


    DATA:  tl_itens_miro    TYPE TABLE OF ty_itens_miro.

    TYPES:
      BEGIN OF ty_dados_miro,
        cabecalho TYPE ty_cabecalho_miro,
        itens     LIKE tl_itens_miro,
      END OF  ty_dados_miro.


    DATA:
      tl_itens TYPE TABLE OF it_itens,
      tl_historico TYPE TABLE OF ty_historico,
      tl_class_contabil TYPE TABLE OF ty_class_contabil,


      BEGIN OF ty_pedidos,
        cabecalho      TYPE ZDE_SAIDA_EKKO,
        itens          type TABLE OF ZDE_SAIDA_ekpo,"tl_itens,
        historico      TYPE TABLE OF ZDE_SAIDA_historico,"ty_historico,
        parceiros      type ZEKPA_TAB,
        class_contabil TYPE TABLE OF ty_class_contabil,
        dados_miro     TYPE TABLE OF ty_dados_miro,
      END OF ty_pedidos,

      BEGIN OF zde_data_request,
        header_filters       LIKE ty_filter_header,
        items_filters        LIKE ty_items_filters,
        historic_filters     LIKE ty_historic_filters,
        partners_filters     LIKE ty_partners_filters,
        dados_cabecalho      TYPE char1,
        dados_itens          TYPE char1,
        dados_historico      TYPE char1,
        dados_parceiro       TYPE char1,
        dados_class_contabil TYPE char1,
        dados_miro           TYPE char1,
        dados_complementares TYPE char1,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        pedidos LIKE TABLE OF ty_pedidos,
      END OF zde_data_response .


    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '138' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONSULTA_PEDIDO IMPLEMENTATION.


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

    IF lwa_data_request-header_filters IS INITIAL and lwa_data_request-historic_filters IS INITIAL and
      lwa_data_request-items_filters IS INITIAL  and lwa_data_request-partners_filters IS INITIAL.
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
          lwa_data_response LIKE zde_data_response,
          wa_ekko           TYPE ekko.

    TYPES: BEGIN OF ty_datas,
             inicio TYPE dats,
             fim    TYPE dats,
           END OF ty_datas.

    TYPES: BEGIN OF ty_pedido_filter,
             ebeln TYPE cdhdr-objectid,
           END OF ty_pedido_filter.


    DATA:
      lra_header_ebeln   TYPE RANGE OF ebeln,
      lra_header_lifnr   TYPE RANGE OF elifn,
      lra_header_bukrs   TYPE RANGE OF bukrs,
      lra_header_bsart   TYPE RANGE OF bsart,
      lra_header_eadat   TYPE RANGE OF dats,
      lra_header_bedat   TYPE RANGE OF dats,
      lra_itens_ebelp    TYPE RANGE OF ebelp,
      lra_itens_werks    TYPE RANGE OF ewerk,
      lra_itens_matnr    TYPE RANGE OF matnr,
      lra_itens_loekz    TYPE RANGE OF loekz,
      lra_itens_no_loekz TYPE RANGE OF loekz,
      lra_historic_bewtp TYPE RANGE OF bewtp,
      lra_historic_vgabe TYPE RANGE OF vgabe,
      lra_historic_shkzg TYPE RANGE OF shkzg,
      lra_partners_lifn2 TYPE RANGE OF lifn2,
      lra_partners_parvw TYPE RANGE OF parvw.

    DATA: lit_filter_pedido TYPE TABLE OF ty_pedido_filter.

    DATA: lt_aedat    TYPE TABLE OF ty_datas.

    DATA: v_belnr TYPE  rseg-belnr,
          v_gjahr TYPE  rseg-gjahr.

    DATA: vg_bukrs  TYPE t001w-werks,
          vg_cnpj   TYPE j_1bcgc,
          vg_branch TYPE t001w-j_1bbranch.

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


*--------------------------------------------------------------------------------------------------------------------------------------*
*    Construção Filtros
*--------------------------------------------------------------------------------------------------------------------------------------*

    DATA(lva_dados_complementares) = abap_false.
    IF lwa_data_request-dados_complementares IS NOT INITIAL.
      lva_dados_complementares = abap_true.
    ENDIF.

    "Campos Header
    LOOP AT lwa_data_request-header_filters-ebeln INTO DATA(wa_filtro1).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro1 ) TO lra_header_ebeln.
    ENDLOOP.

    LOOP AT lwa_data_request-header_filters-lifnr INTO DATA(wa_filtro2).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro2 ) TO lra_header_lifnr.
    ENDLOOP.

    LOOP AT lwa_data_request-header_filters-bukrs INTO DATA(wa_filtro9).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro9 ) TO lra_header_bukrs.
    ENDLOOP.

    LOOP AT lwa_data_request-header_filters-bsart INTO DATA(wa_filtro10).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro10 ) TO lra_header_bsart.
    ENDLOOP.

    "lt_AEDAT = .

    IF lwa_data_request-header_filters-aedat IS NOT INITIAL.

      IF lwa_data_request-header_filters-aedat-inicio IS NOT INITIAL AND lwa_data_request-header_filters-aedat-fim IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'BT' low = lwa_data_request-header_filters-aedat-inicio  high = lwa_data_request-header_filters-aedat-fim ) TO lra_header_eadat.
      ELSE.
        IF lwa_data_request-header_filters-aedat-inicio IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'GE' low = lwa_data_request-header_filters-aedat-inicio  ) TO lra_header_eadat.
        ENDIF.

        IF lwa_data_request-header_filters-aedat-fim IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'LE' low = lwa_data_request-header_filters-aedat-fim ) TO lra_header_eadat.
        ENDIF.

      ENDIF.
    ENDIF.

    IF lwa_data_request-header_filters-bedat IS NOT INITIAL.

      IF lwa_data_request-header_filters-bedat-inicio IS NOT INITIAL AND lwa_data_request-header_filters-bedat-fim IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'BT' low = lwa_data_request-header_filters-bedat-inicio  high = lwa_data_request-header_filters-bedat-fim ) TO lra_header_bedat.
      ELSE.
        IF lwa_data_request-header_filters-bedat-inicio IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'GE' low = lwa_data_request-header_filters-bedat-inicio  ) TO lra_header_bedat.
        ENDIF.

        IF lwa_data_request-header_filters-bedat-fim IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'LE' low = lwa_data_request-header_filters-bedat-fim ) TO lra_header_bedat.
        ENDIF.

      ENDIF.

    ENDIF.

*
*    "Campos Itens
    LOOP AT lwa_data_request-items_filters-ebelp INTO DATA(wa_filtro3).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro3 ) TO lra_itens_ebelp.
    ENDLOOP.

    LOOP AT lwa_data_request-items_filters-werks INTO DATA(wa_filtro4).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro4 ) TO lra_itens_werks.
    ENDLOOP.

    LOOP AT lwa_data_request-items_filters-matnr INTO DATA(wa_filtro11).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro11 ) TO lra_itens_matnr.
    ENDLOOP.

    LOOP AT lwa_data_request-items_filters-loekz INTO DATA(wa_filtro12).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro12 ) TO lra_itens_loekz.
    ENDLOOP.

    LOOP AT lwa_data_request-items_filters-no_loekz INTO DATA(wa_filtro13).
      APPEND VALUE #( sign = 'E' option = 'EQ' low = wa_filtro13 ) TO lra_itens_loekz.
    ENDLOOP.


    "Campos de histórico
    LOOP AT lwa_data_request-historic_filters-bewtp INTO DATA(wa_filtro5).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro5 ) TO lra_historic_bewtp.
    ENDLOOP.

    LOOP AT lwa_data_request-historic_filters-vgabe INTO DATA(wa_filtro6).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro6 ) TO lra_historic_vgabe.
    ENDLOOP.

    LOOP AT lwa_data_request-historic_filters-shkzg INTO DATA(wa_filtro14).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro14 ) TO lra_historic_shkzg.
    ENDLOOP.

    "Campos de Parceiro
    LOOP AT lwa_data_request-partners_filters-lifn2 INTO DATA(wa_filtro7).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro7 ) TO lra_partners_lifn2.
    ENDLOOP.

    LOOP AT lwa_data_request-partners_filters-parvw INTO DATA(wa_filtro8).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro8 ) TO lra_partners_parvw.
    ENDLOOP.





    DATA: v_where      TYPE string,
          v_maktx_like TYPE maktx,
          v_language   TYPE makt-spras.

    CONCATENATE 'cb~ebeln IN @lra_header_EBELN'
            'AND cb~lifnr IN @lra_header_LIFNR'
            'and cb~BSART in @lra_header_bsart'
            'and cb~AEDAT in @lra_header_eadat'
            'and cb~bedat in @lra_header_bedat'
            'AND cb~bukrs in @lra_header_bukrs'
            'AND it~ebelp IN @lra_itens_ebelp'
            'AND it~werks IN @lra_itens_werks'
            'AND it~MATNR in @lra_itens_MATNR'
            'AND it~loekz in @lra_itens_loekz' INTO v_where SEPARATED BY space.


    IF lwa_data_request-partners_filters IS NOT INITIAL.

      CONCATENATE v_where ' AND EXISTS ( SELECT pc~ebeln '
                             'FROM ekpa AS pc'
                          'WHERE  pc~ebeln = cb~ebeln'
                             'AND pc~lifn2 IN @lra_partners_lifn2'
                             'AND pc~parvw IN @lra_partners_PARVW )' INTO v_where SEPARATED BY space.
    ENDIF.


    IF  lwa_data_request-historic_filters IS NOT INITIAL.

      CONCATENATE v_where 'AND EXISTS ( SELECT hi~ebeln'
                           ' FROM ekbe AS hi'
                          ' WHERE hi~ebeln = cb~ebeln'
                             'AND hi~bewtp IN @lra_Historic_BEWTP'
                             'AND hi~vgabe IN @lra_Historic_VGABE '
                             'and hi~SHKZG in @lra_historic_SHKZG )' INTO v_where SEPARATED BY space.
    ENDIF.



*--------------------------------------------------------------------------------------------------------------------------------------*
*   Seleção Dados
*--------------------------------------------------------------------------------------------------------------------------------------*

    SELECT cb~*
      FROM ekko AS cb
      INNER JOIN ekpo AS it ON cb~ebeln = it~ebeln
      INTO TABLE @DATA(tl_ekko)
     WHERE (v_where).

    SORT tl_ekko BY ebeln.
    DELETE ADJACENT DUPLICATES FROM tl_ekko COMPARING ebeln.

    IF tl_ekko[] IS NOT INITIAL .

      LOOP AT tl_ekko INTO DATA(wa_ekko1).
        APPEND VALUE #( ebeln = wa_ekko1-ebeln ) TO lit_filter_pedido.
      ENDLOOP.

      IF lva_dados_complementares IS NOT INITIAL.

        "DADOS_FORNECEDOR
        SELECT     l1~name1      AS nome,
                   d1~street     AS endereco,
                   d1~city1      AS municipio,
                   d1~region     AS estado,
                   d1~city2      AS bairro,
                   d1~house_num1 AS end_numero,
                   l1~stcd1      AS cnpj,
                   l1~stcd2      AS cpf,
                   l1~stcd3      AS inscricao_estadual,
                   l1~stkzn     AS pessoa_fisica,
                   l1~lifnr     AS cod_for
          INTO TABLE @DATA(tl_dados_fornecedor)
              FROM lfa1 AS l1
              INNER JOIN adrc AS d1 ON  l1~adrnr = d1~addrnumber
          FOR ALL ENTRIES IN @tl_ekko
              WHERE l1~lifnr = @tl_ekko-lifnr.

        "DS_EMPRESA
        SELECT butxt, bukrs
       INTO TABLE @DATA(tl_desc_empresa)
       FROM t001
       FOR ALL ENTRIES IN @tl_ekko
       WHERE bukrs = @tl_ekko-bukrs.

        "USUARIO_LANCAMENTO
        SELECT a1~bname,
               b1~fax_number AS cpf,
               c1~name_textc AS nome
        INTO TABLE @DATA(tl_user_lanc)
        FROM usr21 AS a1 INNER JOIN adr3 AS b1 ON a1~addrnumber = b1~addrnumber AND a1~persnumber = b1~persnumber
             INNER JOIN user_addr AS c1 ON a1~bname  = c1~bname
        FOR ALL ENTRIES IN @tl_ekko
       WHERE  a1~bname = @tl_ekko-ernam.

        "DS_USUARIO_APROVADOR_ULTIMO
        SELECT tc~objectid,
               tc~objectclas,
               tc~udate,
               tc~utime,
               us~name_textc
        INTO TABLE @DATA(tl_ultimo_user_aprov)
        FROM cdhdr  AS   tc INNER JOIN user_addr AS us
        ON tc~username = us~bname
          FOR ALL ENTRIES IN @lit_filter_pedido
       WHERE tc~objectid EQ @lit_filter_pedido-ebeln
       AND tc~objectclas  = 'EINKBELEG'
       AND tc~tcode   IN ('ME29N','ME28').

        "       SORT tl_ultimo_user_aprov BY udate utime DESCENDING.

*        IF tl_ultimo_user_aprov[] IS NOT INITIAL.
*          SELECT  tc~udate, tc~utime, tc~objectclas, tc~objectid FROM cdhdr AS tc
*            INTO TABLE @DATA(it_cdhdr)
*            FOR ALL ENTRIES IN @tl_ultimo_user_aprov
*          WHERE tc~objectclas = @tl_ultimo_user_aprov-objectclas
*          AND tc~objectid  = @tl_ultimo_user_aprov-objectid
*          AND tc~tcode      IN ( 'ME29N', 'ME28' ) .
*
*          SORT it_cdhdr BY udate utime.
*        ENDIF.

      ENDIF.

      "DADOS_ITENS
      IF lwa_data_request-dados_itens IS NOT INITIAL.
        "ITENS
        SELECT * FROM ekpo
          INTO TABLE @DATA(it_ekpo)
          FOR ALL ENTRIES IN @tl_ekko
          WHERE ebeln = @tl_ekko-ebeln
          AND ebelp IN @lra_itens_ebelp
          AND werks IN @lra_itens_werks
          AND matnr IN @lra_itens_matnr
          AND loekz IN @lra_itens_loekz.

        IF it_ekpo[] IS NOT INITIAL.

          IF lva_dados_complementares IS NOT INITIAL.

            "DADOS_MATERIAL
            SELECT ma~matnr AS material,
              mtart AS tp_material,
                  maktx AS ds_material,
                  wgbez AS ds_grupo_mercadoria
            INTO TABLE @DATA(tl_dados_material)
            FROM mara AS ma INNER JOIN makt AS tm
                ON ma~matnr = tm~matnr
                INNER JOIN t023t AS tg ON ma~matkl = tg~matkl
            FOR ALL ENTRIES IN @it_ekpo
            WHERE tg~spras = 'P'
                 AND tm~spras = 'P'
                 AND ma~matnr = @it_ekpo-matnr.

            "DADOS_CENTRO
            SELECT t1~werks      AS centro,
                   t1~stras      AS endereco,
                   t1~ort01      AS cidade,
                   t1~regio      AS uf,
                   d1~city2      AS bairro,
                   d1~house_num1 AS endereco_nr,
                   k1~stras      AS endereco_2                ,
                   k1~stcd1      AS cnpj
            FROM t001w AS t1
            LEFT JOIN kna1 AS k1 ON t1~kunnr = k1~kunnr
            INNER JOIN adrc AS d1 ON t1~adrnr = d1~addrnumber
            INTO TABLE @DATA(tl_dados_centro)
            FOR ALL ENTRIES IN @it_ekpo
            WHERE  t1~werks = @it_ekpo-werks.


            "DADOS_MIRO
            IF lwa_data_request-dados_miro IS NOT INITIAL.

              " ITENS
              SELECT ebeln, ebelp, belnr, gjahr, xblnr FROM  rseg
              INTO TABLE @DATA(it_rseg)
               FOR ALL ENTRIES IN @it_ekpo
                WHERE ebeln = @it_ekpo-ebeln AND
                      ebelp = @it_ekpo-ebelp.


              IF it_rseg[] IS NOT INITIAL.
                " CABECALHO
                SELECT belnr,
                       gjahr,
                       budat,
                       stblg
                FROM rbkp
                INTO TABLE @DATA(it_cab_miro)
                FOR ALL ENTRIES IN @it_rseg
                WHERE belnr =  @it_rseg-belnr
                      AND gjahr =  @it_rseg-gjahr.

              ENDIF.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.

      "DADOS_HISTORICO
      IF lwa_data_request-dados_historico IS NOT INITIAL.

        " HISTORICO
        SELECT * FROM ekbe
       INTO TABLE @DATA(it_ekbe)
       FOR ALL ENTRIES IN @tl_ekko
       WHERE ebeln = @tl_ekko-ebeln
             AND bewtp IN @lra_historic_bewtp
             AND vgabe IN @lra_historic_vgabe
            AND shkzg IN @lra_historic_shkzg.



      ENDIF.

      "PARCEIROS
      IF lwa_data_request-dados_parceiro IS NOT INITIAL.

        SELECT * FROM ekpa
       INTO TABLE @DATA(it_ekpa)
       FOR ALL ENTRIES IN @tl_ekko
       WHERE ebeln = @tl_ekko-ebeln
             AND lifn2 IN @lra_partners_lifn2
             AND parvw IN @lra_partners_parvw .

      ENDIF.

      "DADOS_CLASS_CONTABIL
      IF lwa_data_request-dados_class_contabil IS NOT INITIAL.

        SELECT ebeln,
               ebelp,
               sakto,
               gsber,
               kostl,
               kokrs ,
               aufnr,
               anln1,
               anln2
         FROM  ekkn
         INTO TABLE @DATA(tl_ekkn)
         FOR ALL ENTRIES IN @tl_ekko
         WHERE ebeln = @tl_ekko-ebeln.

        IF lva_dados_complementares IS NOT INITIAL AND tl_ekkn[] IS NOT INITIAL.

          "DS_CONTA_CONTABIL
          SELECT em~bukrs,
                   tc~saknr,
                   tc~txt50
          FROM skat AS tc INNER JOIN t001 AS em
                 ON tc~spras = em~spras
                 AND tc~ktopl = em~ktopl
          INTO TABLE @DATA(tl_ds_conta_contabil)
          FOR ALL ENTRIES IN @tl_ekkn
          WHERE tc~spras = em~spras
            AND tc~ktopl = em~ktopl
            AND tc~saknr   =  @tl_ekkn-sakto.
          "and em~bukrs   =  @tl_EKKN-bukrs .

          "DS_DIVISAO
          SELECT fi~gsber,
            em~bukrs,
            fi~gtext
          FROM tgsbt AS fi INNER JOIN t001 AS em
               ON fi~spras =  em~spras
          INTO TABLE @DATA(tl_ds_divisao)
          FOR ALL ENTRIES IN @tl_ekkn
          WHERE fi~gsber    =  @tl_ekkn-gsber.
          "and EM~bukrs    =  EKKO-bukrs

          "DS_CENTRO_CUSTO e DS_CENTRO_CUSTO_RESPONSAVEL
          SELECT em~bukrs,
            tc~kokrs,
            tc~kostl,
            tc~ltext AS centro_custo,
                 ct~verak AS centro_custo_responsavel
          FROM cskt AS tc INNER JOIN t001 AS em
                          ON tc~spras = em~spras
                          INNER JOIN csks AS ct
                          ON tc~kostl = ct~kostl  AND tc~datbi = ct~datbi
          INTO TABLE @DATA(it_dados_c_custo)
            FOR ALL ENTRIES IN @tl_ekkn
          WHERE tc~datbi >= @sy-datum
                AND tc~kokrs = ct~kokrs
                "AND EM~BUKRS = EKKO-bukrs
                AND tc~kokrs =  @tl_ekkn-kokrs
                AND tc~kostl =  @tl_ekkn-kostl.

        ENDIF.

      ENDIF.
      SORT: tl_ultimo_user_aprov BY udate DESCENDING
                                    utime DESCENDING.

*--------------------------------------------------------------------------------------------------------------------------------------*
*     Construção Response
*--------------------------------------------------------------------------------------------------------------------------------------*

      LOOP AT tl_ekko ASSIGNING FIELD-SYMBOL(<fs_ekko>).
        APPEND INITIAL LINE TO lwa_data_response-pedidos ASSIGNING FIELD-SYMBOL(<fs_response>).

        "DADOS_CABECALHO
        IF lwa_data_request-dados_cabecalho IS NOT INITIAL.
          MOVE-CORRESPONDING <fs_ekko> TO <fs_response>-cabecalho.

          IF lva_dados_complementares IS NOT INITIAL.
            READ TABLE tl_dados_fornecedor INTO DATA(w_dados_fornecedor) WITH KEY cod_for = <fs_ekko>-lifnr.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING w_dados_fornecedor TO <fs_response>-cabecalho-dados_fornecedor.
            ENDIF.

            READ TABLE tl_desc_empresa INTO DATA(w_desc_empresa) WITH KEY bukrs = <fs_ekko>-bukrs.
            IF sy-subrc IS INITIAL.
              <fs_response>-cabecalho-ds_empresa = w_desc_empresa-butxt.
            ENDIF.

            READ TABLE tl_user_lanc INTO DATA(w_user_lanc) WITH KEY bname = <fs_ekko>-ernam.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING w_user_lanc TO <fs_response>-cabecalho-usuario_lancamento.
            ENDIF.

            READ TABLE tl_ultimo_user_aprov INTO DATA(w_ul_apro) WITH KEY objectid = <fs_ekko>-ebeln.
            IF sy-subrc IS INITIAL.
              <fs_response>-cabecalho-ds_usuario_aprovador_ultimo = w_ul_apro-name_textc.
            ENDIF.
          ENDIF.

        ENDIF.

        "DADOS_ITENS
        LOOP AT it_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>) WHERE ebeln   EQ <fs_ekko>-ebeln.
          "APPEND <fs_ekpo> TO <fs_response>-itens-.
          APPEND INITIAL LINE TO <fs_response>-itens ASSIGNING FIELD-SYMBOL(<fs_response_itens>).

          MOVE-CORRESPONDING <fs_ekpo> TO <fs_response_itens>.

          IF lva_dados_complementares IS NOT INITIAL.
            READ TABLE tl_dados_material INTO DATA(w_dados_material) WITH KEY material = <fs_ekpo>-matnr.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING w_dados_material TO <fs_response_itens>-dados_material.
              "APPEND w_dados_material TO <fs_response>-itens-.
            ENDIF.

            READ TABLE tl_dados_centro INTO DATA(w_dados_centro) WITH KEY centro = <fs_ekpo>-werks.
            IF sy-subrc IS INITIAL.
              IF w_dados_centro-cnpj IS INITIAL.

                CLEAR: vg_bukrs, vg_branch, vg_cnpj.
                vg_bukrs = <fs_ekko>-bukrs.
                vg_branch = <fs_ekpo>-werks.

                CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
                  EXPORTING
                    branch            = vg_branch
                    bukrs             = vg_bukrs
                  IMPORTING
                    cgc_number        = vg_cnpj       " CNPJ do Local de Neg./Filial
                  EXCEPTIONS
                    branch_not_found  = 1
                    address_not_found = 2
                    company_not_found = 3
                    OTHERS            = 4.

                IF vg_cnpj IS NOT INITIAL.
                  w_dados_centro-cnpj = vg_cnpj.
                  MOVE-CORRESPONDING w_dados_centro TO <fs_response_itens>-dados_centro.

                ELSE.
                  w_dados_centro-cnpj = 'Não cadastrado'.
                  MOVE-CORRESPONDING w_dados_centro TO <fs_response_itens>-dados_centro.
                ENDIF.
              ELSE.
                MOVE-CORRESPONDING w_dados_centro TO <fs_response_itens>-dados_centro.
              ENDIF.
            ENDIF.

          ENDIF.

        ENDLOOP.

        "DADOS_HISTORICO
        LOOP AT it_ekbe ASSIGNING FIELD-SYMBOL(<fs_ekbe>) WHERE ebeln   EQ <fs_ekko>-ebeln.
          APPEND INITIAL LINE TO <fs_response>-historico ASSIGNING FIELD-SYMBOL(<fs_response_historico>).
          MOVE-CORRESPONDING <fs_ekbe> TO <fs_response_historico>.
          "APPEND <fs_ekbe> TO <fs_response_historico>-ekbe.

          IF lva_dados_complementares IS NOT INITIAL.

            IF <fs_ekbe>-vgabe EQ '1'.

              SELECT SINGLE mblnr
                FROM mseg
                  INTO @DATA(v_mblnr)
                WHERE smbln = @<fs_ekbe>-belnr.

              IF sy-subrc IS INITIAL.
                <fs_response_historico>-doc_mat_estornado = 'X'.
              ENDIF.

            ELSEIF <fs_ekbe>-vgabe EQ '2'.

              SELECT SINGLE stblg
                FROM rbkp
                  INTO @DATA(v_stblg)
                WHERE belnr = @<fs_ekbe>-belnr.

              IF sy-subrc IS INITIAL AND v_stblg IS NOT INITIAL.
                <fs_response_historico>-doc_miro_estornado = 'X'.
              ENDIF.

            ENDIF.

          ENDIF.

        ENDLOOP.

        "DADOS_PARCEIRO
        LOOP AT it_ekpa ASSIGNING FIELD-SYMBOL(<fs_ekpa>) WHERE ebeln   EQ <fs_ekko>-ebeln.
          APPEND <fs_ekpa> TO <fs_response>-parceiros.
        ENDLOOP.

        "DADOS_CLASS_CONTABIL
        LOOP AT tl_ekkn ASSIGNING FIELD-SYMBOL(<fs_ekkn>) WHERE ebeln  EQ <fs_ekko>-ebeln.

          APPEND INITIAL LINE TO <fs_response>-class_contabil ASSIGNING FIELD-SYMBOL(<fs_response_class_contabil>).

          MOVE-CORRESPONDING <fs_ekkn> TO <fs_response_class_contabil>.

          IF lva_dados_complementares IS NOT INITIAL.
            READ TABLE tl_ds_conta_contabil INTO DATA(w_ds_c_cont) WITH KEY bukrs = <fs_ekko>-bukrs
                                                                            saknr = <fs_ekkn>-sakto.

            IF sy-subrc IS INITIAL.
              <fs_response_class_contabil>-ds_conta_contabil = w_ds_c_cont-txt50.
            ENDIF.

            READ TABLE tl_ds_divisao INTO DATA(w_ds_divisao) WITH KEY bukrs = <fs_ekko>-bukrs
                                                                      gsber = <fs_ekkn>-gsber.

            IF sy-subrc IS INITIAL.
              <fs_response_class_contabil>-ds_divisao = w_ds_divisao-gtext.
            ENDIF.

            READ TABLE it_dados_c_custo INTO DATA(w_dados_c_custo) WITH KEY bukrs = <fs_ekko>-bukrs
                                                                            kokrs = <fs_ekkn>-kokrs
                                                                            kostl = <fs_ekkn>-kostl.
            IF sy-subrc IS INITIAL.
              <fs_response_class_contabil>-ds_centro_custo = w_dados_c_custo-centro_custo.
              <fs_response_class_contabil>-ds_centro_custo_responsavel = w_dados_c_custo-centro_custo_responsavel.
            ENDIF.
          ENDIF.

        ENDLOOP.

        "DADOS_MIRO
        DATA(lit_rseg_aux) = it_rseg[].
        DELETE lit_rseg_aux WHERE  ebeln NE <fs_ekko>-ebeln.

        DATA(lit_rseg_cab) = lit_rseg_aux[].

        SORT lit_rseg_cab BY belnr gjahr.
        DELETE ADJACENT DUPLICATES FROM lit_rseg_cab COMPARING belnr gjahr.

        LOOP AT lit_rseg_cab INTO DATA(w_rseg).

          READ TABLE it_cab_miro INTO DATA(w_cab_miro) WITH KEY belnr =  w_rseg-belnr
                                                                gjahr =  w_rseg-gjahr.
          IF sy-subrc IS INITIAL.
            APPEND INITIAL LINE TO <fs_response>-dados_miro ASSIGNING FIELD-SYMBOL(<fs_response_miro>).

            MOVE-CORRESPONDING w_cab_miro TO <fs_response_miro>-cabecalho.

            LOOP AT lit_rseg_aux INTO DATA(lwa_rseg) WHERE belnr = w_rseg-belnr
                                                       AND gjahr = w_rseg-gjahr.

              APPEND INITIAL LINE TO <fs_response_miro>-itens ASSIGNING FIELD-SYMBOL(<fs_response_miro_item>).

              MOVE-CORRESPONDING lwa_rseg TO <fs_response_miro_item>.
            ENDLOOP.
          ENDIF.

        ENDLOOP.


      ENDLOOP.

    ENDIF.
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
