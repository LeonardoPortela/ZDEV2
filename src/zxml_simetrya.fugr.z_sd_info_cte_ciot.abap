FUNCTION z_sd_info_cte_ciot.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_CTE_AVULSO) TYPE  J_1BDOCNUM
*"     REFERENCE(P_CTE_GERA) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_FATURAMENTO_AUTOM) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_CH_REFERENCIA) TYPE  ZCH_REF OPTIONAL
*"  TABLES
*"      IT_CTE_CIOT STRUCTURE  ZCTE_CIOT OPTIONAL
*"      IT_CTE_CIOT_PARCEIROS STRUCTURE  ZCTE_CIOT_PARCE OPTIONAL
*"      IT_ZCL_CIOT TYPE  ZCL_CIOT_T OPTIONAL
*"  CHANGING
*"     REFERENCE(P_ZCTE_IDENTIFICA) TYPE  ZCTE_IDENTIFICA OPTIONAL
*"  EXCEPTIONS
*"      INF_DOCNUM
*"      INF_PROPVEICULO
*"      NAO_DOCNUM
*"      NAO_RTRC
*"      NAO_CONTA_CORRENTE
*"      NAO_CIOT
*"      N_PLACA_CAD
*"      RESTRICOES_VEICULO
*"----------------------------------------------------------------------
  DATA: it_cte_trans    TYPE TABLE OF zcte_trans WITH HEADER LINE,
        wa_cte_trans    TYPE zcte_trans,
        wa_cte_ciot     TYPE zcte_ciot,
        wa_zcte_ret     TYPE zciot_ret,
        "IT_PROPRIERARIO TYPE TABLE OF LFA1 WITH HEADER LINE,
        wa_proprierario TYPE lfa1,
        wa_doc_transp   TYPE vttk,
        "VG_QTD_PROP     TYPE I,
        ob_ciot         TYPE REF TO zcl_ciot.

  IF p_faturamento_autom IS NOT INITIAL.        "*-#133089-21.02.2024-JT
    vg_faturamento_autom = p_faturamento_autom. "*-#133089-21.02.2024-JT
    vg_ch_referencia     = p_ch_referencia.     "*-#133089-21.02.2024-JT
  ENDIF.                                        "*-#133089-21.02.2024-JT

*-#133089-12.02.2024-JT-inicio
  IF vg_faturamento_autom = abap_true.
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.
*-#133089-12.02.2024-JT-fim

  IF p_cte_gera IS INITIAL.

    SELECT * INTO TABLE it_cte_ciot
      FROM zcte_ciot
     WHERE docnum EQ p_cte_avulso.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e008(zciot) WITH p_cte_avulso RAISING nao_ciot.
    ELSE.

      SELECT * INTO TABLE it_cte_ciot_parceiros
        FROM zcte_ciot_parce
        FOR ALL ENTRIES IN it_cte_ciot
       WHERE cd_ciot EQ it_cte_ciot-cd_ciot.

      CREATE OBJECT ob_ciot.
      LOOP AT it_cte_ciot INTO wa_cte_ciot.

        CALL METHOD ob_ciot->clear.

        CALL METHOD ob_ciot->get_info_ciot
          EXPORTING
            pcd_ciot = wa_cte_ciot-cd_ciot.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        APPEND ob_ciot TO it_zcl_ciot.
      ENDLOOP.

    ENDIF.

  ELSE.

    CALL FUNCTION 'Z_SD_INFO_CTE_TRANS'
      EXPORTING
        p_cte_avulso = p_cte_avulso
        p_cte_gera   = c_x
      IMPORTING
        p_doc_transp = wa_doc_transp
      TABLES
        it_cte_trans = it_cte_trans.

    CASE sy-subrc.
      WHEN 1.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE e026 WITH sy-msgv1 RAISING n_placa_cad.
          WHEN abap_true.
            MESSAGE e026 WITH sy-msgv1 INTO DATA(l_mesg).
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
            RAISE n_placa_cad.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      WHEN 2.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          WHEN abap_true.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDCASE.

    CHECK it_cte_trans[] IS NOT INITIAL.

    "Buca Veículo de Tração
    READ TABLE it_cte_trans INTO wa_cte_trans WITH KEY tp_veiculo = '0'.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e035(zciot) RAISING inf_propveiculo.
    ENDIF.

    "Busca Proprietário
    SELECT SINGLE * INTO wa_proprierario
      FROM lfa1
     WHERE lifnr EQ wa_cte_trans-proprietario.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e036(zciot) WITH wa_cte_trans-pc_veiculo RAISING inf_propveiculo.
    ENDIF.

    zcl_webservice_tipcard=>cons_situacao_transportador(
      EXPORTING
        i_placa          = wa_cte_trans-pc_veiculo    " Placa veículo
      RECEIVING
        e_consultas      =  DATA(e_consultas)   " Tabela de Consultas Transportador
      EXCEPTIONS
        erro             = 1
        webservice       = 2
        OTHERS           = 3 ).

    IF sy-subrc IS INITIAL.
      READ TABLE e_consultas INDEX 1 INTO DATA(wa_consultas).
      IF wa_consultas-ck_rntrc_ativo EQ abap_false.
        IF wa_consultas-ds_msg_transportador IS NOT INITIAL.
          DATA: lc_texto TYPE c LENGTH 200.
          MOVE wa_consultas-ds_msg_transportador TO lc_texto.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE e001(zciot) WITH lc_texto(50) lc_texto+50(50) lc_texto+100(50) lc_texto+150(50) RAISING inf_propveiculo.
            WHEN abap_true.
              MESSAGE e001(zciot) WITH lc_texto(50) lc_texto+50(50) lc_texto+100(50) lc_texto+150(50) INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
              RAISE inf_propveiculo.
          ENDCASE.
*-#133089-21.02.2024-JT-fim

        ELSEIF wa_consultas-ds_msg_veiculo IS NOT INITIAL.
          CLEAR: lc_texto.
          MOVE wa_consultas-ds_msg_veiculo TO lc_texto.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE e000(zciot) WITH lc_texto(50) lc_texto+50(50) lc_texto+100(50) lc_texto+150(50) RAISING restricoes_veiculo.
            WHEN abap_true.
              MESSAGE e000(zciot) WITH lc_texto(50) lc_texto+50(50) lc_texto+100(50) lc_texto+150(50) INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
              RAISE restricoes_veiculo.
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ELSE.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE e037(zciot) RAISING inf_propveiculo.
            WHEN abap_true.
              MESSAGE e037(zciot) RAISING inf_propveiculo INTO l_mesg..
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.
      ENDIF.
    ENDIF.

    CREATE OBJECT ob_ciot.

    CALL METHOD ob_ciot->clear.

    CALL METHOD ob_ciot->novo
      EXPORTING
        p_zcte_trans       = wa_cte_trans
      IMPORTING
        p_zcte_identifica  = p_zcte_identifica
      EXCEPTIONS
        inf_docnum         = 1
        inf_propveiculo    = 2
        nao_docnum         = 3
        nao_rtrc           = 4
        nao_conta_corrente = 5
        OTHERS             = 6.

*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_off.
        CASE sy-subrc.
          WHEN 1.
            MESSAGE e002(zciot) RAISING inf_docnum.
          WHEN 2.
            MESSAGE e006(zciot) RAISING inf_propveiculo.
          WHEN 3.
            MESSAGE e003(zciot) WITH sy-msgv1 RAISING nao_docnum.
          WHEN 4.
            MESSAGE e005(zciot) WITH sy-msgv1 RAISING nao_rtrc.
          WHEN 5.
            MESSAGE e007(zciot) WITH sy-msgv1 sy-msgv2 RAISING nao_conta_corrente.
          WHEN 6.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDCASE.
      WHEN abap_true.
        CASE sy-subrc.
          WHEN 1.
            MESSAGE e002(zciot) INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
            RAISE inf_docnum.
          WHEN 2.
            MESSAGE e006(zciot) INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
            RAISE inf_propveiculo.
          WHEN 3.
            MESSAGE e003(zciot) WITH sy-msgv1 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
            RAISE nao_docnum.
          WHEN 4.
            MESSAGE e005(zciot) WITH sy-msgv1 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
            RAISE nao_rtrc.
          WHEN 5.
            MESSAGE e007(zciot) WITH sy-msgv1 sy-msgv2 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
            RAISE nao_conta_corrente.
          WHEN 6.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
        ENDCASE.
    ENDCASE.
*-#133089-21.02.2024-JT-fim

    APPEND ob_ciot TO it_zcl_ciot.

    CALL METHOD ob_ciot->visualizar
      IMPORTING
        p_zcte_ciot      = wa_cte_ciot
        p_zcte_parceiros = it_cte_ciot_parceiros[].

    APPEND wa_cte_ciot TO it_cte_ciot.

  ENDIF.

ENDFUNCTION.
