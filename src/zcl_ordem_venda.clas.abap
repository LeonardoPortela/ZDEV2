class ZCL_ORDEM_VENDA definition
  public
  final
  create public .

public section.

  interfaces ZIF_ORDEM_VENDA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ORDEM_VENDA IMPLEMENTATION.


  METHOD ZIF_ORDEM_VENDA~CK_FILIAL_EMISSORA_ROMANEIO.

    DATA: LC_BRANCH	TYPE ZDE_BRANCH_RECEB.

    R_INSTANCIA = ME.

    IF ME->ZIF_ORDEM_VENDA~AT_VBAK-AUART EQ 'ZRFL' OR ME->ZIF_ORDEM_VENDA~AT_VBAK-AUART EQ 'ZRDC'.
      ME->ZIF_ORDEM_VENDA~GET_PARTINER( EXPORTING I_FUNCAO_PARTINER = 'AG' IMPORTING E_PARTINER = DATA(E_PARTINER) ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = E_PARTINER-KUNNR
        IMPORTING
          OUTPUT = E_PARTINER-KUNNR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = I_BRANCH
        IMPORTING
          OUTPUT = LC_BRANCH.

      IF E_PARTINER-KUNNR NE LC_BRANCH.
        RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_FILIAL-MSGID
                              MSGNO = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_FILIAL-MSGNO )
            MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_FILIAL-MSGID
            MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_FILIAL-MSGNO
            MSGTY  = 'E'.
      ENDIF.

    ENDIF.

    IF ME->ZIF_ORDEM_VENDA~AT_VBAP-WERKS NE I_BRANCH.

      SELECT SINGLE * INTO @DATA(WA_CENTRO_REAL)
        FROM ZSDT_DEPARA_CEN
       WHERE CENTROV_1 = @ME->ZIF_ORDEM_VENDA~AT_VBAP-WERKS.

      IF SY-SUBRC IS NOT INITIAL OR WA_CENTRO_REAL-CENTRO_REAL NE I_BRANCH.
        RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_CENTRO_ERRO-MSGID
                              MSGNO = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_CENTRO_ERRO-MSGNO )
            MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_CENTRO_ERRO-MSGID
            MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_CENTRO_ERRO-MSGNO
            MSGTY  = 'E'.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_ordem_venda~ck_ordem_venda_convencional.

    DATA: lc_centro_real TYPE  werks_d,
          wa_j_1bbranch  TYPE  j_1bbranch,
          e_lgort        TYPE  lgort_d, "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
          wa_zmmt0017    TYPE  zmmt0017. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

    r_instancia = me.


    IF me->zif_ordem_venda~at_vbak-kvgr3 NE 'C'.

      RAISE EXCEPTION TYPE zcx_ordem_venda
        EXPORTING
          textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_ordem_venda_nao_co-msgid
                            msgno  = zcx_ordem_venda=>zcx_ordem_venda_nao_co-msgno
                            attr1  = CONV #( me->zif_ordem_venda~at_vbak-vbeln ) )
          msgty  = 'E'
          msgid  = zcx_ordem_venda=>zcx_ordem_venda_nao_co-msgid
          msgno  = zcx_ordem_venda=>zcx_ordem_venda_nao_co-msgno
          msgv1  = CONV #( me->zif_ordem_venda~at_vbak-vbeln ).
    ENDIF.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro               = me->zif_ordem_venda~at_vbap-gsber
      IMPORTING
        centro_real          = lc_centro_real
        wa_j_1bbranch        = wa_j_1bbranch
      EXCEPTIONS
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        informar_centro_v    = 4
        OTHERS               = 5.

    CHECK sy-subrc IS INITIAL.

    "Buscar Centro a Fixar
    SELECT SINGLE * INTO @DATA(wa_afixar)
      FROM zsdt_depara_cen
     WHERE vkorg             EQ @wa_j_1bbranch-bukrs
       AND centro_real       EQ @wa_j_1bbranch-branch
       AND tp_centro_virtual EQ @zcl_pedido_compra=>st_tp_centro_a_fixar.

    CHECK sy-subrc IS INITIAL.
*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO
*    SELECT SINGLE * INTO @DATA(WA_ZMMT0017)
*      FROM ZMMT0017
*     WHERE MATNR          EQ @ME->ZIF_ORDEM_VENDA~AT_VBAP-MATNR
*       AND CENTRO_FIXO    EQ @WA_J_1BBRANCH-BRANCH
*       AND CENTRO_A_FIXAR EQ @WA_AFIXAR-CENTROV_1
*       AND TP_PRODUTO     EQ 'CO'.

    zcl_deposito=>zif_deposito~get_instance(
                )->get_deposito_material_filial(
                EXPORTING
                  i_matnr          = me->zif_ordem_venda~at_vbap-matnr
                  i_tp_produto     = 'CO'
                  i_bukrs          = wa_j_1bbranch-bukrs
                  i_branch         = wa_j_1bbranch-branch
                  i_eudr           = I_EUDR
                IMPORTING
                  e_lgort          = e_lgort  ).

    IF e_lgort IS NOT INITIAL.
      "IF ZLGORT NE ME->ZIF_ORDEM_VENDA~AT_VBAP-LGORT	.
      IF e_lgort NE me->zif_ordem_venda~at_vbap-lgort	and me->zif_ordem_venda~at_vbap-lgort is not initial. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM.
        RAISE EXCEPTION TYPE zcx_ordem_venda
          EXPORTING
            textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_armazem_errado-msgid
                              msgno  = zcx_ordem_venda=>zcx_armazem_errado-msgno
                              attr1  = CONV #( me->zif_ordem_venda~at_vbap-lgort )
                              attr2  = CONV #( wa_zmmt0017-lgort ) )
            msgty  = 'E'
            msgid  = zcx_ordem_venda=>zcx_armazem_errado-msgid
            msgno  = zcx_ordem_venda=>zcx_armazem_errado-msgno
            msgv1  = CONV #( me->zif_ordem_venda~at_vbap-lgort )
            msgv2  = CONV #( wa_zmmt0017-lgort ).
      ENDIF.
    ENDIF.

*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - fim
  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~CK_ORDEM_VENDA_DCO.

    R_INSTANCIA = ME.

    IF ME->ZIF_ORDEM_VENDA~AT_VBAK-AUART EQ 'ZRDC'.

      SELECT SINGLE *
        FROM ZDCO_PRODUTOR
        INTO @DATA(WA_ZDCO_PRODUTOR)
       WHERE VBELN       = @ME->ZIF_ORDEM_VENDA~AT_VBAK-VBELN
         AND CD_MATERIAL = @I_MATNR
         AND CD_CENTRO   = @I_BRANCH.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
          EXPORTING
            TEXTID = VALUE #( MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_SEM_DCO-MSGID
                              MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_SEM_DCO-MSGNO )
            MSGTY  = 'E'
            MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_SEM_DCO-MSGID
            MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_SEM_DCO-MSGNO.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_ordem_venda~ck_ordem_venda_transgenica.

    DATA: lc_centro_real TYPE  werks_d,
          wa_j_1bbranch  TYPE  j_1bbranch,
          wa_zmmt0017    TYPE  zmmt0017, "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
          v_eudr(1). "BUG #185956  IR249979 - BG
    r_instancia = me.

    IF me->zif_ordem_venda~at_vbak-kvgr3 NE 'R'.
      RAISE EXCEPTION TYPE zcx_ordem_venda
        EXPORTING
          textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_ordem_venda_nao_rr-msgid
                            msgno  = zcx_ordem_venda=>zcx_ordem_venda_nao_rr-msgno
                            attr1  = CONV #( me->zif_ordem_venda~at_vbak-vbeln ) )
          msgty  = 'E'
          msgid  = zcx_ordem_venda=>zcx_ordem_venda_nao_rr-msgid
          msgno  = zcx_ordem_venda=>zcx_ordem_venda_nao_rr-msgno
          msgv1  = CONV #( me->zif_ordem_venda~at_vbak-vbeln ).
    ENDIF.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro               = me->zif_ordem_venda~at_vbap-gsber
      IMPORTING
        centro_real          = lc_centro_real
        wa_j_1bbranch        = wa_j_1bbranch
      EXCEPTIONS
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        informar_centro_v    = 4
        OTHERS               = 5.

    CHECK sy-subrc IS INITIAL.

    "Buscar Centro a Fixar
    SELECT SINGLE * INTO @DATA(wa_afixar)
      FROM zsdt_depara_cen
     WHERE vkorg             EQ @wa_j_1bbranch-bukrs
       AND centro_real       EQ @wa_j_1bbranch-branch
       AND tp_centro_virtual EQ @zcl_pedido_compra=>st_tp_centro_a_fixar.

    CHECK sy-subrc IS INITIAL.
*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO

*    SELECT SINGLE * INTO @DATA(WA_ZMMT0017)
*      FROM ZMMT0017
*     WHERE MATNR          EQ @ME->ZIF_ORDEM_VENDA~AT_VBAP-MATNR
*       AND CENTRO_FIXO    EQ @WA_J_1BBRANCH-BRANCH
*       AND CENTRO_A_FIXAR EQ @WA_AFIXAR-CENTROV_1
*       AND TP_PRODUTO     EQ 'RR'.

    zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
     EXPORTING
       i_material        = me->zif_ordem_venda~at_vbap-matnr
       i_centro_fixo     = wa_j_1bbranch-branch
       i_centro_afixar   = wa_afixar-centrov_1
       i_tipo_produto    = 'RR'
       i_eudr            = i_eudr
     IMPORTING
       e_single_depara   = wa_zmmt0017
            ).

*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM
    IF wa_zmmt0017 IS NOT INITIAL.
      IF wa_zmmt0017-lgort NE me->zif_ordem_venda~at_vbap-lgort AND me->zif_ordem_venda~at_vbap-lgort IS NOT INITIAL  . "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM
        RAISE EXCEPTION TYPE zcx_ordem_venda
          EXPORTING
            textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_armazem_errado-msgid
                              msgno  = zcx_ordem_venda=>zcx_armazem_errado-msgno
                              attr1  = CONV #( me->zif_ordem_venda~at_vbap-lgort )
                              attr2  = CONV #( wa_zmmt0017-lgort ) )
            msgty  = 'E'
            msgid  = zcx_ordem_venda=>zcx_armazem_errado-msgid
            msgno  = zcx_ordem_venda=>zcx_armazem_errado-msgno
            msgv1  = CONV #( me->zif_ordem_venda~at_vbap-lgort )
            msgv2  = CONV #( wa_zmmt0017-lgort ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~CK_SAFRA.

    R_INSTANCIA = ME.

    IF I_SAFRA NE ME->ZIF_ORDEM_VENDA~AT_VBAP-CHARG.
      RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_SAFRA-MSGID
                            MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_SAFRA-MSGNO
                            ATTR1  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBAK-VBELN )
                            ATTR2  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBAP-CHARG ) )
          MSGTY  = 'E'
          MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_SAFRA-MSGID
          MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_SAFRA-MSGNO
          MSGV1  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBAK-VBELN )
          MSGV2  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBAP-CHARG ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~CK_TIPO_FRETE.

    R_INSTANCIA = ME.

    IF I_TIPO_FRETE NE ME->ZIF_ORDEM_VENDA~AT_VBKD-INCO1.
      RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_TIPO_FRETE-MSGID
                            MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_TIPO_FRETE-MSGNO
                            ATTR1  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBAK-VBELN )
                            ATTR2  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBKD-INCO1 ) )
          MSGTY  = 'E'
          MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_TIPO_FRETE-MSGID
          MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_TIPO_FRETE-MSGNO
          MSGV1  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBAK-VBELN )
          MSGV2  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBKD-INCO1 ).
    ENDIF.

  ENDMETHOD.


  method ZIF_ORDEM_VENDA~GET_CENTRO.

    R_INSTANCIA = ME.

    CLEAR: E_WERKS, E_PARID_WERKS.

    E_WERKS       = ME->ZIF_ORDEM_VENDA~AT_VBAP-WERKS.
    E_PARID_WERKS = |{ E_WERKS ALPHA = IN }|.

  endmethod.


  METHOD zif_ordem_venda~get_ck_valor_frete.

    DATA: lc_frete       TYPE zde_info_frete,
          lc_frete_ent   TYPE zde_info_frete,
          lc_texto_valor TYPE c LENGTH 14.

    r_instancia = me.
    CLEAR: e_parametros_empresa,
           e_parametros_motorista.

    "Itinerário
    IF me->zif_ordem_venda~at_vbap-route IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ordem_venda
        EXPORTING
          textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_ordem_sem_itinerario-msgid
                            msgno  = zcx_ordem_venda=>zcx_ordem_sem_itinerario-msgno
                            attr1  = CONV #( me->zif_ordem_venda~at_vbak-vbeln ) )
          msgty  = 'E'
          msgid  = zcx_ordem_venda=>zcx_ordem_sem_itinerario-msgid
          msgno  = zcx_ordem_venda=>zcx_ordem_sem_itinerario-msgno
          msgv1  = CONV #( me->zif_ordem_venda~at_vbak-vbeln ).
    ELSE.
      lc_frete-route = me->zif_ordem_venda~at_vbap-route.
    ENDIF.

    " Tipo de transporte
    me->zif_ordem_venda~get_tipo_transporte(
       EXPORTING
         i_vsart = '01'
       IMPORTING
         e_tipo_transporte = lc_frete-shtyp
    " Zona de partida " Zona de chegada
      )->get_zonas_transporte( IMPORTING e_lzonea = lc_frete-lzonea e_lzonez = lc_frete-lzonez
      ).

    TRY .
        " Tipo do Contrato
        DATA(lc_veiculo_tracao) =
          zcl_veiculos=>zif_veiculos~get_instance(
            )->set_veiculo( i_placa = i_placa_veic_tracao
            )->get_tipo_contrato( IMPORTING e_tipo_contrato = lc_frete-add01
            )->at_veiculo.

      CATCH zcx_veiculos INTO DATA(ex_veiculo).    "
        ex_veiculo->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
        RAISE EXCEPTION TYPE zcx_ordem_venda
          EXPORTING
            textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_erro_geral-msgid
                              msgno  = zcx_ordem_venda=>zcx_erro_geral-msgno
                              attr1  = CONV #( sy-msgv1 ) attr2  = CONV #( sy-msgv2 ) attr3  = CONV #( sy-msgv3 ) attr4  = CONV #( sy-msgv4 ) )
            msgty  = 'E'
            msgid  = zcx_ordem_venda=>zcx_erro_geral-msgid
            msgno  = zcx_ordem_venda=>zcx_erro_geral-msgno
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4.
    ENDTRY.

    "Material
    lc_frete-matnr = me->zif_ordem_venda~at_vbap-matnr.

    "Veículo de Tração
    lc_frete-ds_placa_trator = i_placa_veic_tracao.

    "Somente CIF valida o preço do parceiro SP da OV.
    IF me->zif_ordem_venda~at_vbkd-inco1 NE zif_carga=>st_tp_frete_cpt AND
       me->zif_ordem_venda~at_vbkd-inco1 NE zif_carga=>st_tp_frete_cfr AND
       me->zif_ordem_venda~at_vbak-kvgr5 NE '002'.

      READ TABLE me->zif_ordem_venda~at_vbpa WITH KEY parvw = 'SP' INTO DATA(wa_empresa).

      IF sy-subrc IS NOT INITIAL AND i_zlest0181 IS NOT INITIAL.

        TRY .

*JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
            zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro( i_parceiro = i_zlest0181-id_parceiro_cole
              )->get_regio( IMPORTING e_regio = DATA(uf_coleta)
              ).

            "Procurar Por Local de Negócio
            SELECT SINGLE * INTO @DATA(wa_zlest0207)
              FROM zlest0207
             WHERE bukrs  EQ @i_zlest0181-bukrs
               AND branch EQ @i_zlest0181-branch
               AND regio  EQ @uf_coleta.

            IF sy-subrc IS NOT INITIAL.
              "Procurar Sem Local de Negócio
              SELECT SINGLE * INTO @wa_zlest0207
                FROM zlest0207
               WHERE bukrs  EQ @i_zlest0181-bukrs
                 AND regio  EQ @uf_coleta.
            ENDIF.

            IF sy-subrc IS INITIAL.
              APPEND VALUE #( parvw = 'SP' lifnr = wa_zlest0207-tdlnr ) TO me->zif_ordem_venda~at_vbpa.
            ENDIF.

            READ TABLE me->zif_ordem_venda~at_vbpa WITH KEY parvw = 'SP' INTO wa_empresa.

*-BUG 56712 - 23.04.2021 - JT - inicio
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_ordem_venda
                EXPORTING
                  textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_erro_geral-msgid
                                    msgno  = zcx_ordem_venda=>zcx_erro_geral-msgno
                                    attr1  = CONV #( zif_ordem_venda~at_vbak-vbeln ) )
                  msgty  = 'E'
                  msgid  = zcx_ordem_venda=>zcx_erro_geral-msgid
                  msgno  = zcx_ordem_venda=>zcx_erro_geral-msgno
                  msgv1  = CONV #( zif_ordem_venda~at_vbak-vbeln )
                  msgv2  = CONV #( 'Falta Param. ZLES0190 - Procurar Logística' ).
            ENDIF.
*-BUG 56712 - 23.04.2021 - JT - fim
*          CATCH zcx_parceiros.
*            sy-subrc = 1.
        ENDTRY.

      ENDIF.

      IF sy-subrc IS INITIAL AND ( me->zif_ordem_venda~at_vbak-kvgr1 EQ 'SIM' OR me->zif_ordem_venda~at_vbak-kvgr1 IS INITIAL ).

        " Nº do agente de frete
        lc_frete-id_agent_frete = wa_empresa-lifnr.

        TRY .

            "Valida Frete Empresa
            zcl_calc_frete=>get_valor_frete(
              EXPORTING
                i_route                = lc_frete-route           " Itinerário
                i_tdlnr                = lc_frete-id_agent_frete  " Nº do agente de frete
                i_shtyp                = lc_frete-shtyp           " Tipo de transporte
                i_lzonea               = lc_frete-lzonea    " Zona de partida
                i_lzonez               = lc_frete-lzonez    " Zona de chegada
                i_add01                = lc_frete-add01     " Suplem.1
                i_matnr                = lc_frete-matnr     " Nº do material
                i_ordem_venda          = me->zif_ordem_venda~at_vbak-vbeln  " Nº documento de vendas e distribuição
                i_placa_trator         = lc_frete-ds_placa_trator " Placa Veículo Tração
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
                i_id_carguero          = i_zlest0181-id_carguero
                i_viagem_id            = i_viagem_id
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - fim
              IMPORTING
                e_kbetr                = lc_frete-kbetr " Montante/porcentagem de condição no caso de não haver escala
                e_konwa                = lc_frete-konwa " Unidade de condição (moeda ou porcentagem)
                e_krech                = lc_frete-krech " Regra de cálculo de condição
                e_lzonea               = DATA(e_lzonea)
                e_lzonez               = DATA(e_lzonez)
                e_route                = DATA(e_route)
            ).

            CONCATENATE 'Itinerário: ' e_route
                        'Agente de Frete:' lc_frete-id_agent_frete
                        'Tipo de Transporte:' lc_frete-shtyp
                        'Zona de Partida:' e_lzonea
                        'Zona de Chegada:' e_lzonez
                        'Contrato:' lc_frete-add01
                        'Material:' lc_frete-matnr
                        'Ordem de Venda:' me->zif_ordem_venda~at_vbak-vbeln
                        'Placa Trator:' lc_frete-ds_placa_trator
                   INTO e_parametros_empresa SEPARATED BY space.


          CATCH zcx_calc_frete INTO DATA(ex_calc_frete).

            CONCATENATE 'Itinerário: ' e_route
                        'Agente de Frete:' lc_frete-id_agent_frete
                        'Tipo de Transporte:' lc_frete-shtyp
                        'Zona de Partida:' e_lzonea
                        'Zona de Chegada:' e_lzonez
                        'Contrato:' lc_frete-add01
                        'Material:' lc_frete-matnr
                        'Ordem de Venda:' me->zif_ordem_venda~at_vbak-vbeln
                        'Placa Trator:' lc_frete-ds_placa_trator
                   INTO e_parametros_empresa SEPARATED BY space.

            RAISE EXCEPTION TYPE zcx_calc_frete
              EXPORTING
                textid = VALUE #( msgid = ex_calc_frete->msgid
                                  msgno = ex_calc_frete->msgno
                                  attr1 = CONV #( ex_calc_frete->msgv1 )
                                  attr2 = CONV #( ex_calc_frete->msgv2 )
                                  attr3 = CONV #( ex_calc_frete->msgv3 )
                                  attr4 = CONV #( ex_calc_frete->msgv4 ) )
                msgid  = ex_calc_frete->msgid
                msgno  = ex_calc_frete->msgno
                msgty  = 'E'
                msgv1  = ex_calc_frete->msgv1
                msgv2  = ex_calc_frete->msgv2
                msgv3  = ex_calc_frete->msgv3
                msgv4  = ex_calc_frete->msgv4.

        ENDTRY.

        IF lc_frete-kbetr         NE i_valor_frete_empresa AND
           i_paga_trecho_terceiro IS INITIAL.

          WRITE i_valor_frete_empresa TO lc_texto_valor.
          CONDENSE lc_texto_valor NO-GAPS.
          RAISE EXCEPTION TYPE zcx_ordem_venda
            EXPORTING
              textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_preco_frete-msgid
                                msgno  = zcx_ordem_venda=>zcx_preco_frete-msgno
                                attr1  = lc_texto_valor )
              msgty  = 'E'
              msgid  = zcx_ordem_venda=>zcx_preco_frete-msgid
              msgno  = zcx_ordem_venda=>zcx_preco_frete-msgno
              msgv1  = CONV #( lc_texto_valor ).
        ENDIF.

      ELSEIF i_valor_frete_empresa IS NOT INITIAL.
        "Não pode ter valor de frete empresa
        RAISE EXCEPTION TYPE zcx_ordem_venda
          EXPORTING
            textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_preco_frete_empresa-msgid
                              msgno  = zcx_ordem_venda=>zcx_preco_frete_empresa-msgno )
            msgty  = 'E'
            msgid  = zcx_ordem_venda=>zcx_preco_frete_empresa-msgid
            msgno  = zcx_ordem_venda=>zcx_preco_frete_empresa-msgno.
      ENDIF.
    ENDIF.

    CLEAR: wa_empresa.
    READ TABLE me->zif_ordem_venda~at_vbpa WITH KEY parvw = 'SP' INTO wa_empresa.

    IF ( lc_veiculo_tracao-proprietario IS INITIAL AND i_valor_frete_motorista IS NOT INITIAL ) OR
       ( lc_veiculo_tracao-proprietario IS NOT INITIAL AND i_valor_frete_motorista IS INITIAL ) AND
       ( sy-subrc IS NOT INITIAL ).
      "Não pode ter valor de frete motorista
      RAISE EXCEPTION TYPE zcx_ordem_venda
        EXPORTING
          textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_preco_frete_motorista-msgid
                            msgno  = zcx_ordem_venda=>zcx_preco_frete_motorista-msgno )
          msgty  = 'E'
          msgid  = zcx_ordem_venda=>zcx_preco_frete_motorista-msgid
          msgno  = zcx_ordem_venda=>zcx_preco_frete_motorista-msgno.
    ELSE.

      IF sy-subrc IS NOT INITIAL.
        " Nº do agente de frete
        lc_frete-id_agent_frete = lc_veiculo_tracao-proprietario.
      ELSE.
        lc_frete-id_agent_frete = wa_empresa-lifnr.
      ENDIF.

      CONCATENATE 'Itinerário: ' lc_frete-route
                  'Agente de Frete:' lc_frete-id_agent_frete
                  'Tipo de Transporte:' lc_frete-shtyp
                  'Zona de Partida:' lc_frete-lzonea
                  'Zona de Chegada:' lc_frete-lzonez
                  'Contrato:' lc_frete-add01
                  'Material:' lc_frete-matnr
                  'Ordem de Venda:' me->zif_ordem_venda~at_vbak-vbeln
                  'Placa Trator:' lc_frete-ds_placa_trator
             INTO e_parametros_motorista SEPARATED BY space.

      TRY .

          "Valida Frete Motorista Saída
          zcl_calc_frete=>get_valor_frete(
            EXPORTING
              i_route        = lc_frete-route           " Itinerário
              i_tdlnr        = lc_frete-id_agent_frete  " Nº do agente de frete
              i_shtyp        = lc_frete-shtyp           " Tipo de transporte
              i_lzonea       = lc_frete-lzonea    " Zona de partida //PRODUTOR
              i_lzonez       = lc_frete-lzonez    " Zona de chegada // LOCAL ENTREGA
              i_add01        = lc_frete-add01     " Suplem.1
              i_matnr        = lc_frete-matnr     " Nº do material
              i_ordem_venda  = me->zif_ordem_venda~at_vbak-vbeln  " Nº documento de vendas e distribuição
              i_id_ordem     = i_id_ordem
              i_placa_trator = lc_frete-ds_placa_trator " Placa Veículo Tração
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
              i_id_carguero          = i_zlest0181-id_carguero
              i_viagem_id            = i_viagem_id
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - fim
            IMPORTING
              e_kbetr        = lc_frete-kbetr " Montante/porcentagem de condição no caso de não haver escala
              e_konwa        = lc_frete-konwa " Unidade de condição (moeda ou porcentagem)
              e_krech        = lc_frete-krech " Regra de cálculo de condição
              e_lzonea       = e_lzonea
              e_lzonez       = e_lzonez
              e_route        = e_route
          ).

          CONCATENATE 'Itinerário: ' e_route
                      'Agente de Frete:' lc_frete-id_agent_frete
                      'Tipo de Transporte:' lc_frete-shtyp
                      'Zona de Partida:' e_lzonea
                      'Zona de Chegada:' e_lzonez
                      'Contrato:' lc_frete-add01
                      'Material:' lc_frete-matnr
                      'Ordem de Venda:' me->zif_ordem_venda~at_vbak-vbeln
                      'Placa Trator:' lc_frete-ds_placa_trator
                 INTO e_parametros_motorista SEPARATED BY space.

        CATCH zcx_calc_frete INTO ex_calc_frete.

          CONCATENATE 'Itinerário: ' e_route
                      'Agente de Frete:' lc_frete-id_agent_frete
                      'Tipo de Transporte:' lc_frete-shtyp
                      'Zona de Partida:' e_lzonea
                      'Zona de Chegada:' e_lzonez
                      'Contrato:' lc_frete-add01
                      'Material:' lc_frete-matnr
                      'Ordem de Venda:' me->zif_ordem_venda~at_vbak-vbeln
                      'Placa Trator:' lc_frete-ds_placa_trator
                 INTO e_parametros_motorista SEPARATED BY space.

          RAISE EXCEPTION TYPE zcx_calc_frete
            EXPORTING
              textid = VALUE #( msgid = ex_calc_frete->msgid
                                msgno = ex_calc_frete->msgno
                                attr1 = CONV #( ex_calc_frete->msgv1 )
                                attr2 = CONV #( ex_calc_frete->msgv2 )
                                attr3 = CONV #( ex_calc_frete->msgv3 )
                                attr4 = CONV #( ex_calc_frete->msgv4 ) )
              msgid  = ex_calc_frete->msgid
              msgno  = ex_calc_frete->msgno
              msgty  = 'E'
              msgv1  = ex_calc_frete->msgv1
              msgv2  = ex_calc_frete->msgv2
              msgv3  = ex_calc_frete->msgv3
              msgv4  = ex_calc_frete->msgv4.

      ENDTRY.

      "Somar valor frete entrada
      IF me->zif_ordem_venda~at_vbak-kvgr5 EQ '002' OR
         i_paga_trecho_terceiro            EQ abap_true.

*------------------------------------
*-Ajuste busca parceiro erro Producao - 16.07.2021 - JT - inicio
*------------------------------------
        READ TABLE me->zif_ordem_venda~at_vbpa WITH KEY parvw = 'SP' INTO wa_empresa.

        IF sy-subrc <> 0.
          IF uf_coleta IS INITIAL.
            zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro( i_parceiro = i_zlest0181-id_parceiro_cole
              )->get_regio( IMPORTING e_regio = uf_coleta
              ).
          ENDIF.

          "Procurar Por Local de Negócio
          SELECT SINGLE * INTO @wa_zlest0207
            FROM zlest0207
           WHERE bukrs  EQ @i_zlest0181-bukrs
             AND branch EQ @i_zlest0181-branch
             AND regio  EQ @uf_coleta.

          IF sy-subrc IS NOT INITIAL.
            "Procurar Sem Local de Negócio
            SELECT SINGLE * INTO @wa_zlest0207
              FROM zlest0207
             WHERE bukrs  EQ @i_zlest0181-bukrs
               AND regio  EQ @uf_coleta.
          ENDIF.

          IF sy-subrc IS INITIAL.
            APPEND VALUE #( parvw = 'SP' lifnr = wa_zlest0207-tdlnr ) TO me->zif_ordem_venda~at_vbpa.
          ENDIF.
        ENDIF.
*------------------------------------
*-Ajuste busca parceiro erro Producao - 16.07.2021 - JT - fim
*------------------------------------

        READ TABLE me->zif_ordem_venda~at_vbpa WITH KEY parvw = 'SP' INTO wa_empresa.
        IF sy-subrc IS INITIAL.

          DATA: rgcnpj TYPE RANGE OF stcd1.

          SELECT SINGLE * INTO @DATA(wa_lfa1)
            FROM lfa1
           WHERE lifnr EQ '0000000116'.

          rgcnpj = VALUE #( sign = 'I' option = 'CP' ( low = wa_lfa1-stcd1(8) && '*' ) ).

          SELECT SINGLE * INTO @DATA(wa_zlest0181)
            FROM zlest0181
           WHERE vbeln EQ @me->zif_ordem_venda~at_vbak-vbeln.

          SELECT SINGLE * INTO @DATA(wa_cole)
            FROM lfa1
           WHERE lifnr EQ @wa_zlest0181-id_parceiro_cole.

          SELECT SINGLE * INTO @DATA(wa_lfa1_regio)
            FROM lfa1
           WHERE regio EQ @wa_cole-regio
             AND ktokk EQ 'ZFIC'
             AND bahns NE @space
             AND dlgrp EQ '0001'
             AND stcd1 IN @rgcnpj.

          lc_frete_ent-shtyp = 'Z021'.
          lc_frete_ent-add01 = lc_frete-add01.
          lc_frete_ent-id_agent_frete = wa_lfa1_regio-lifnr.
          lc_frete_ent-matnr = lc_frete-matnr.
          lc_frete_ent-ds_placa_trator = lc_frete-ds_placa_trator.

          me->zif_ordem_venda~get_zonas_transporte(
            EXPORTING
              i_frete_entrada = abap_true
            IMPORTING
              e_lzonea     = lc_frete_ent-lzonea
              e_lzonez     = lc_frete_ent-lzonez
              e_lc_coleta  = DATA(e_lc_coleta)
              e_lc_entrega = DATA(e_lc_entrega) ).

          TRY .

              zcl_itinerario=>zif_itinerario~get_instance(
                )->get_itinerario_relevante(
                EXPORTING
                  i_cod_loc_coleta  = e_lc_coleta    " Nº conta do fornecedor
                  i_cod_loc_entrega = e_lc_entrega    " Nº cliente
                IMPORTING
                  e_tvro            = DATA(e_tvro)    " Itinerários
              ).

              lc_frete_ent-route = e_tvro-route.

            CATCH zcx_itinerario INTO DATA(ex_itinerario).    "

              RAISE EXCEPTION TYPE zcx_ordem_venda
                EXPORTING
                  textid = VALUE #( msgid = ex_itinerario->msgid
                                    msgno = ex_itinerario->msgno
                                    attr1 = ex_itinerario->msgv1
                                    attr2 = ex_itinerario->msgv2
                                    attr3 = ex_itinerario->msgv3
                                    attr4 = ex_itinerario->msgv4 )
                  msgid  = ex_itinerario->msgid
                  msgno  = ex_itinerario->msgno
                  msgty  = 'E'
                  msgv1  = ex_itinerario->msgv1
                  msgv2  = ex_itinerario->msgv2
                  msgv3  = ex_itinerario->msgv3
                  msgv4  = ex_itinerario->msgv4.

          ENDTRY.

          TRY .

              zcl_calc_frete=>get_valor_frete(
                EXPORTING
                  i_route        = lc_frete_ent-route           " Itinerário
                  i_tdlnr        = lc_frete_ent-id_agent_frete  " Nº do agente de frete
                  i_shtyp        = lc_frete_ent-shtyp           " Tipo de transporte
                  i_lzonea       = lc_frete_ent-lzonea          " Zona de partida  // PRODUTOR
                  i_lzonez       = lc_frete_ent-lzonez      " Zona de chegada  // FILIAL
                  i_add01        = lc_frete_ent-add01       " Suplem.1
                  i_matnr        = lc_frete_ent-matnr       " Nº do material
                  i_ordem_venda  = me->zif_ordem_venda~at_vbak-vbeln  " Nº documento de vendas e distribuição
                  i_id_ordem     = i_id_ordem
                  i_placa_trator = lc_frete_ent-ds_placa_trator " Placa Veículo Tração
                  i_frete_entrada = abap_true
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
                  i_id_carguero          = i_zlest0181-id_carguero
                  i_viagem_id            = i_viagem_id
*---------------CS2019001158 - Jaime Tassoni - 16.11.2020 - fim
                IMPORTING
                  e_kbetr        = lc_frete_ent-kbetr " Montante/porcentagem de condição no caso de não haver escala
                  e_konwa        = lc_frete_ent-konwa " Unidade de condição (moeda ou porcentagem)
                  e_krech        = lc_frete_ent-krech " Regra de cálculo de condição
                  e_lzonea       = e_lzonea
                  e_lzonez       = e_lzonez
                  e_route        = e_route
                ).

              CONCATENATE 'Saída:' e_parametros_motorista
                          ' --> Entrada:'
                          'Itinerário: ' e_route
                          'Agente de Frete:' lc_frete_ent-id_agent_frete
                          'Tipo de Transporte:' lc_frete_ent-shtyp
                          'Zona de Partida:' e_lzonea
                          'Zona de Chegada:' e_lzonez
                          'Contrato:' lc_frete_ent-add01
                          'Material:' lc_frete_ent-matnr
                          'Ordem de Venda:' me->zif_ordem_venda~at_vbak-vbeln
                          'Placa Trator:' lc_frete_ent-ds_placa_trator
                     INTO e_parametros_motorista SEPARATED BY space.

            CATCH zcx_calc_frete INTO ex_calc_frete.

              CONCATENATE 'Saída:' e_parametros_motorista
                          ' --> Entrada:'
                          'Itinerário: ' e_route
                          'Agente de Frete:' lc_frete_ent-id_agent_frete
                          'Tipo de Transporte:' lc_frete_ent-shtyp
                          'Zona de Partida:' e_lzonea
                          'Zona de Chegada:' e_lzonez
                          'Contrato:' lc_frete_ent-add01
                          'Material:' lc_frete_ent-matnr
                          'Ordem de Venda:' me->zif_ordem_venda~at_vbak-vbeln
                          'Placa Trator:' lc_frete_ent-ds_placa_trator
                     INTO e_parametros_motorista SEPARATED BY space.

              RAISE EXCEPTION TYPE zcx_calc_frete
                EXPORTING
                  textid = VALUE #( msgid = ex_calc_frete->msgid
                                    msgno = ex_calc_frete->msgno
                                    attr1 = CONV #( ex_calc_frete->msgv1 )
                                    attr2 = CONV #( ex_calc_frete->msgv2 )
                                    attr3 = CONV #( ex_calc_frete->msgv3 )
                                    attr4 = CONV #( ex_calc_frete->msgv4 ) )
                  msgid  = ex_calc_frete->msgid
                  msgno  = ex_calc_frete->msgno
                  msgty  = 'E'
                  msgv1  = ex_calc_frete->msgv1
                  msgv2  = ex_calc_frete->msgv2
                  msgv3  = ex_calc_frete->msgv3
                  msgv4  = ex_calc_frete->msgv4.

          ENDTRY.

        ELSE.
          "Erro, não informado parceiro SP na Ordem de Venda/Pedido de Compra
          RAISE EXCEPTION TYPE zcx_ordem_venda
            EXPORTING
              textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_ordem_agente_frete-msgid
                                msgno  = zcx_ordem_venda=>zcx_ordem_agente_frete-msgno
                                attr1  = me->zif_ordem_venda~at_vbak-vbeln )
              msgty  = 'E'
              msgid  = zcx_ordem_venda=>zcx_ordem_agente_frete-msgid
              msgno  = zcx_ordem_venda=>zcx_ordem_agente_frete-msgno
              msgv1  = CONV #( me->zif_ordem_venda~at_vbak-vbeln ).
        ENDIF.
      ELSE.
        lc_frete_ent-kbetr = 0.
      ENDIF.

      IF ( lc_frete-kbetr + lc_frete_ent-kbetr ) NE i_valor_frete_motorista.

        WRITE i_valor_frete_motorista TO lc_texto_valor.
        CONDENSE lc_texto_valor NO-GAPS.
        RAISE EXCEPTION TYPE zcx_ordem_venda
          EXPORTING
            textid = VALUE #( msgid  = zcx_ordem_venda=>zcx_preco_frete-msgid
                              msgno  = zcx_ordem_venda=>zcx_preco_frete-msgno
                              attr1  = lc_texto_valor )
            msgty  = 'E'
            msgid  = zcx_ordem_venda=>zcx_preco_frete-msgid
            msgno  = zcx_ordem_venda=>zcx_preco_frete-msgno
            msgv1  = CONV #( lc_texto_valor ).
      ELSE.
        e_valor_frete_motorista = lc_frete-kbetr.
        e_valor_frete_entrada   = lc_frete_ent-kbetr.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  method ZIF_ORDEM_VENDA~GET_DADOS_COMERCIAIS.

    R_INSTANCIA = ME.

    E_VBKD = ME->ZIF_ORDEM_VENDA~AT_VBKD.

  endmethod.


  METHOD ZIF_ORDEM_VENDA~GET_INSTANCE.

    IF ZIF_ORDEM_VENDA~AT_ORDEM_VENDA IS NOT BOUND.
      CREATE OBJECT ZIF_ORDEM_VENDA~AT_ORDEM_VENDA TYPE ZCL_ORDEM_VENDA.
      R_INSTANCIA = ZIF_ORDEM_VENDA~AT_ORDEM_VENDA.
    ELSE.
      R_INSTANCIA = ZIF_ORDEM_VENDA~AT_ORDEM_VENDA.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~GET_ITEM.

    R_INSTANCIA = ME.

    E_VBAP = ME->ZIF_ORDEM_VENDA~AT_VBAP.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~GET_ORDEM_VENDA.

    R_INSTANCIA = ME.

    E_ORDEM_VENDA = ME->ZIF_ORDEM_VENDA~AT_VBAK.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~GET_ORDEM_VENDA_SALDO.

    E_SALDO   = 0.

    SELECT SUM( LFIMG ) INTO @DATA(E_REMESSA)
       FROM LIPS
      WHERE VGBEL EQ @I_VBELN.

    SELECT SUM( WMENG ) INTO @DATA(E_ITEM_ORDENS)
      FROM VBEP
     WHERE VBELN EQ @I_VBELN.

    E_SALDO = E_ITEM_ORDENS - E_REMESSA.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~GET_PARCEIROS.

    R_INSTANCIA = ME.

    E_VBPA = ME->ZIF_ORDEM_VENDA~AT_VBPA.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~GET_PARTINER.

    R_INSTANCIA = ME.

    SELECT SINGLE *
      INTO @E_PARTINER
      FROM VBPA
     WHERE VBELN EQ @ME->ZIF_ORDEM_VENDA~AT_VBAK-VBELN
       AND PARVW EQ @I_FUNCAO_PARTINER.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_PARCEIRO-MSGID
                            MSGNO = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_PARCEIRO-MSGNO
                            ATTR1 = CONV #( I_FUNCAO_PARTINER ) )
          MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_PARCEIRO-MSGID
          MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_PARCEIRO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_FUNCAO_PARTINER ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~GET_TEXT_CAB_1.

    DATA: LC_NAME  TYPE THEAD-TDNAME,
          IT_LINES TYPE TABLE OF TLINE.

    R_TEXTO = ''.

    "//Buscar Texto da ordem venda
    LC_NAME = I_VBELN.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                      = '0002'
        LANGUAGE                = SY-LANGU
        NAME                    = LC_NAME
        OBJECT                  = 'VBBK'
      TABLES
        LINES                   = IT_LINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCX_ORDEM_VENDA=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    R_TEXTO = ''.

    LOOP AT IT_LINES INTO DATA(WA_LINES).
      CONCATENATE R_TEXTO WA_LINES-TDLINE INTO R_TEXTO SEPARATED BY SPACE.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~GET_TEXT_CAB_FORMULARIO.

    DATA: LC_NAME  TYPE THEAD-TDNAME,
          IT_LINES TYPE TABLE OF TLINE.

    R_TEXTO = ''.

    "//Buscar Texto da ordem venda
    LC_NAME = I_VBELN.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                      = '0001'
        LANGUAGE                = SY-LANGU
        NAME                    = LC_NAME
        OBJECT                  = 'VBBK'
      TABLES
        LINES                   = IT_LINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCX_ORDEM_VENDA=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    R_TEXTO = ''.

    LOOP AT IT_LINES INTO DATA(WA_LINES).
      CONCATENATE R_TEXTO WA_LINES-TDLINE INTO R_TEXTO SEPARATED BY SPACE.
    ENDLOOP.


  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~GET_TIPO_FRETE.

    R_INSTANCIA = ME.

    CASE ME->ZIF_ORDEM_VENDA~AT_VBKD-INCO1.
      WHEN ZIF_CARGA=>ST_TP_FRETE_EXW.
      WHEN ZIF_CARGA=>ST_TP_FRETE_FCA.
      WHEN ZIF_CARGA=>ST_TP_FRETE_FAS.
      WHEN ZIF_CARGA=>ST_TP_FRETE_FOB.
      WHEN ZIF_CARGA=>ST_TP_FRETE_CFR.
      WHEN ZIF_CARGA=>ST_TP_FRETE_CIF.
      WHEN ZIF_CARGA=>ST_TP_FRETE_CPT.
      WHEN ZIF_CARGA=>ST_TP_FRETE_CIP.
      WHEN ZIF_CARGA=>ST_TP_FRETE_DAF.
      WHEN ZIF_CARGA=>ST_TP_FRETE_DES.
      WHEN ZIF_CARGA=>ST_TP_FRETE_DEQ.
      WHEN ZIF_CARGA=>ST_TP_FRETE_DDU.
      WHEN ZIF_CARGA=>ST_TP_FRETE_DDP.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
          EXPORTING
            TEXTID = VALUE #( MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_TIPO_FRETE_ERR-MSGID
                              MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_TIPO_FRETE_ERR-MSGNO )
            MSGTY  = 'E'
            MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_TIPO_FRETE_ERR-MSGID
            MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_TIPO_FRETE_ERR-MSGNO.
    ENDCASE.

    E_TIPO_FRETE = ME->ZIF_ORDEM_VENDA~AT_VBKD-INCO1.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~GET_TIPO_TRANSPORTE.

    R_INSTANCIA = ME.

* "// add Tipo de Frete ZGLO BUG-189811 05/09/2025 wbarbosa
    IF ( 'ZRDC_ZRFL_ZIND_ZGLO' CS ME->ZIF_ORDEM_VENDA~AT_VBAK-AUART ) AND ( ME->ZIF_ORDEM_VENDA~AT_VBAK-AUART IS NOT INITIAL ).

      READ TABLE ME->ZIF_ORDEM_VENDA~AT_VBPA INTO DATA(WA_VBPA) WITH KEY PARVW = 'LR'.
      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE * INTO @DATA(WA_KNA1_LR)
          FROM KNA1
         WHERE KUNNR EQ @WA_VBPA-KUNNR.
      ENDIF.

      READ TABLE ME->ZIF_ORDEM_VENDA~AT_VBPA INTO WA_VBPA WITH KEY PARVW = 'Z1'.
      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE * INTO @DATA(WA_LFA1_Z1)
          FROM LFA1
         WHERE LIFNR EQ @WA_VBPA-LIFNR.
      ENDIF.

      "Determinar Agente Frete
      READ TABLE ME->ZIF_ORDEM_VENDA~AT_VBPA INTO WA_VBPA  WITH KEY PARVW = 'SP'.
      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE * INTO @DATA(WA_LFA1_SP)
          FROM LFA1
         WHERE LIFNR EQ @WA_VBPA-LIFNR.
      ENDIF.

    ENDIF.

    SELECT T~SHTYP, K~VSART, T~BSART, T~AUART, K~LAUFK
      INTO TABLE @DATA(IT_ZSDT0011)
      FROM ZSDT0011 AS T
     INNER JOIN TVTK AS K ON K~SHTYP EQ T~SHTYP
     WHERE T~TP_MOVIMENTO EQ 'S'
       AND T~AUART        EQ @ME->ZIF_ORDEM_VENDA~AT_VBAK-AUART
       AND K~VSART        EQ @I_VSART.

    IF SY-SUBRC IS INITIAL.
      IF ( WA_KNA1_LR IS NOT INITIAL AND WA_LFA1_Z1 IS NOT INITIAL ) AND
         ( ( WA_KNA1_LR-STCD1 IS NOT INITIAL AND WA_KNA1_LR-STCD1 NE WA_LFA1_Z1-STCD1 ) OR
           ( WA_KNA1_LR-STCD2 IS NOT INITIAL AND WA_KNA1_LR-STCD2 NE WA_LFA1_Z1-STCD2 ) ).
        "Percurso com Transbordo
        READ TABLE IT_ZSDT0011 INTO DATA(WA_ZSDT0011) WITH KEY LAUFK = '1'.
        E_TIPO_TRANSPORTE = WA_ZSDT0011-SHTYP.
      ELSE.
        "Percurso Direto
        READ TABLE IT_ZSDT0011 INTO WA_ZSDT0011 WITH KEY LAUFK = '4'.
        E_TIPO_TRANSPORTE = WA_ZSDT0011-SHTYP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~GET_ZONAS_TRANSPORTE.

    R_INSTANCIA = ME.

    CLEAR: E_LZONEA,
           E_LZONEZ,
           E_LC_COLETA,
           E_LC_ENTREGA.

    READ TABLE ME->ZIF_ORDEM_VENDA~AT_VBPA INTO DATA(LC_PC) WITH KEY PARVW = 'PC'.
    IF SY-SUBRC IS INITIAL.
      E_LC_COLETA = LC_PC-LIFNR.
      SELECT SINGLE LZONE INTO @E_LZONEA
        FROM LFA1
       WHERE LIFNR EQ @LC_PC-LIFNR.
    ENDIF.

    CASE I_FRETE_ENTRADA.
      WHEN ABAP_FALSE.
        "Local de Entrega
        READ TABLE ME->ZIF_ORDEM_VENDA~AT_VBPA INTO DATA(LC_LR) WITH KEY PARVW = 'LR'.
        IF SY-SUBRC IS INITIAL.
          E_LC_ENTREGA = LC_LR-KUNNR.
          SELECT SINGLE LZONE INTO @E_LZONEZ
            FROM KNA1
           WHERE KUNNR EQ @LC_LR-KUNNR.
        ENDIF.
      WHEN ABAP_TRUE.
        "Filial de Entrega
        READ TABLE ME->ZIF_ORDEM_VENDA~AT_VBPA INTO LC_LR WITH KEY PARVW = 'AG'.
        IF SY-SUBRC IS INITIAL.
          E_LC_ENTREGA = LC_LR-KUNNR.
          SELECT SINGLE LZONE INTO @E_LZONEZ
            FROM KNA1
           WHERE KUNNR EQ @LC_LR-KUNNR.
        ENDIF.
    ENDCASE.

    IF E_LZONEA IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_ZONA_PARTIDA-MSGID
                            MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_ZONA_PARTIDA-MSGNO
                            ATTR1  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBAK-VBELN ) )
          MSGTY  = 'E'
          MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_ZONA_PARTIDA-MSGID
          MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_ZONA_PARTIDA-MSGNO
          MSGV1  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBAK-VBELN ).
    ENDIF.

    IF E_LZONEZ IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_ZONA_CHEGADA-MSGID
                            MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_ZONA_CHEGADA-MSGNO
                            ATTR1  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBAK-VBELN ) )
          MSGTY  = 'E'
          MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_ZONA_CHEGADA-MSGID
          MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_ZONA_CHEGADA-MSGNO
          MSGV1  = CONV #( ME->ZIF_ORDEM_VENDA~AT_VBAK-VBELN ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~OPEN.

    CHECK I_VBELN IS NOT INITIAL.

    SET PARAMETER ID 'AUN'  FIELD I_VBELN.
    CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

  ENDMETHOD.


  METHOD ZIF_ORDEM_VENDA~SET_ORDEM_VENDA.

    R_INSTANCIA = ME.

    SELECT SINGLE * INTO ME->ZIF_ORDEM_VENDA~AT_VBAK
      FROM VBAK
     WHERE VBELN EQ I_VBELN.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ORDEM_VENDA
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_NAO_EXISTE-MSGID
                            MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_NAO_EXISTE-MSGNO
                            ATTR1  = CONV #( I_VBELN ) )
          MSGTY  = 'E'
          MSGID  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_NAO_EXISTE-MSGID
          MSGNO  = ZCX_ORDEM_VENDA=>ZCX_ORDEM_VENDA_NAO_EXISTE-MSGNO
          MSGV1  = CONV #( I_VBELN ).
    ENDIF.

    SELECT SINGLE * INTO ME->ZIF_ORDEM_VENDA~AT_VBAP
      FROM VBAP
     WHERE VBELN EQ I_VBELN.

    SELECT SINGLE * INTO ME->ZIF_ORDEM_VENDA~AT_VBKD
      FROM VBKD
     WHERE VBELN EQ I_VBELN.

    SELECT * INTO TABLE ME->ZIF_ORDEM_VENDA~AT_VBPA
      FROM VBPA
     WHERE VBELN EQ I_VBELN.

  ENDMETHOD.


  METHOD zif_ordem_venda~ck_grupo_mercadoria.

    "Checa a Obrigatoriedade da Ordem de Carregamento para Carregamentos do TIpo CPT.

    CHECK i_tipo_frete = zif_carga=>st_tp_frete_cpt.

    SELECT SINGLE *
      FROM zlest0244
      INTO @DATA(_zlest0244)
     WHERE matkl = @me->zif_ordem_venda~at_vbap-matkl.

    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE zcx_ordem_venda
        EXPORTING
          textid = VALUE #( msgid = zcx_ordem_venda=>zcx_ordem_venda_cpt_od-msgid
                            msgno = zcx_ordem_venda=>zcx_ordem_venda_cpt_od-msgno
                            attr1 = CONV #( me->zif_ordem_venda~at_vbap-vbeln ) )
          msgid  = zcx_ordem_venda=>zcx_ordem_venda_cpt_od-msgid
          msgno  = zcx_ordem_venda=>zcx_ordem_venda_cpt_od-msgno
          msgty  = 'E'
          msgv1  = CONV #( me->zif_ordem_venda~at_vbap-vbeln ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
