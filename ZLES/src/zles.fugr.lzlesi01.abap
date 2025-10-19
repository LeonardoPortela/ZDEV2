*----------------------------------------------------------------------*
***INCLUDE LZLESI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  LEAVE_100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE leave_100 INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " LEAVE_100  INPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: wa_xvttk  TYPE vttkvb.
  DATA: e_consultas  TYPE zlest0135_t,
        wa_zlest0135 TYPE zlest0135.

  CHECK vg_ck_admim_ped EQ abap_false.

  CLEAR : vl_adto, vl_route, vl_tplst, vl_bukrs, wk_0026, vl_fknum, vl_chvadto, vl_tdlnr.

  CASE sy-ucomm.

    WHEN 'PEDAGIO'.

      "02.10.2018
      IF zlest0026-pedagio LE 0 AND
         ( ( vg_pedi_pedagio EQ abap_true AND vg_cartao_pedagio = abap_false ) OR
           ( vg_pedi_pedagio EQ abap_true AND vg_cartao_pedagio = abap_true AND zlest0026-ck_credita_ped EQ abap_true ) ).
        MESSAGE i070(zles) .
        EXIT.
      ENDIF.

      READ TABLE ti_xvttk INTO wa_xvttk INDEX 1.

      CALL FUNCTION 'Z_REPOM_MONITOR_PEDAGIO'
        EXPORTING
          i_incluir         = abap_true
          i_tknum           = wa_xvttk-tknum
          i_vttkvb          = wa_xvttk
          i_cd_cid_origem   = zlest0101-cd_cid_origem
          i_cd_cid_destino  = zlest0101-cd_cid_destino
          i_qtd_eixos       = i_veiculo_eixos
        IMPORTING
          e_id_proc_cliente = zlest0026-id_proc_cliente.

    WHEN 'CONF'.
*-#133089-12.02.2024-JT-inicio
      PERFORM f_confirma.
*-#133089-12.02.2024-JT-fim

*      READ TABLE ti_xvttk INTO wa_xvttk INDEX 1.
*
*      IF zlest0026-placa_cav IS INITIAL.
*        zlest0026-placa_cav = wa_xvttk-text1(7).
*      ENDIF.
*
*      IF zlest0026-placa_cav IS NOT INITIAL.
*        CALL METHOD zcl_webservice_tipcard=>cons_situacao_transportador
*          EXPORTING
*            i_placa     = zlest0026-placa_cav
*          RECEIVING
*            e_consultas = e_consultas
*          EXCEPTIONS
*            erro        = 1
*            webservice  = 2
*            OTHERS      = 3.
*
*        IF sy-subrc IS NOT INITIAL.
*          MESSAGE i000(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*          EXIT.
*        ENDIF.
*
*        READ TABLE e_consultas INDEX 1 INTO wa_zlest0135.
*        IF wa_zlest0135-ck_rntrc_ativo EQ abap_false.
*          sy-msgv1 = wa_zlest0135-ds_msg_transportador+000(50).
*          sy-msgv2 = wa_zlest0135-ds_msg_transportador+050(50).
*          sy-msgv3 = wa_zlest0135-ds_msg_transportador+100(50).
*          sy-msgv4 = wa_zlest0135-ds_msg_transportador+150(50)..
*          MESSAGE i000(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*          EXIT.
*        ENDIF.
*      ENDIF.
*
*      CHECK vg_ck_alterou_cidade EQ abap_false.
*
*      CHECK vg_ck_admim_ped EQ abap_false.
*
*      CHECK vg_ck_admim_frete EQ abap_false.
*
*      CHECK vg_ck_credito_pedt EQ abap_false.
*
*      IF zlest0026-ck_credita_ped EQ abap_true AND vg_pedi_mesmo_veicul EQ abap_true AND zlest0026-tx_obs_cred_mesm IS INITIAL.
*        MESSAGE i100(zles).
*        EXIT.
*      ENDIF.
*
*      IF vg_ck_ped_param = abap_false.
*        MESSAGE i092(zles).
*        EXIT.
*      ENDIF.
*
*      IF zlest0026-pedagio LE 0 AND
*         ( ( vg_pedi_pedagio EQ abap_true AND vg_cartao_pedagio = abap_false ) OR
*           ( vg_pedi_pedagio EQ abap_true AND vg_cartao_pedagio = abap_true AND zlest0026-ck_credita_ped EQ abap_true ) ).
*
*        IF ( vg_cartao_pedagio    EQ abap_false ) AND
*           ( vg_pedi_mesmo_veicul EQ abap_false ) AND
*           ( vg_pedi_mesma_carga  EQ abap_false ).
*          MESSAGE i070(zles) .
*          EXIT.
*        ENDIF.
*
*** Validação inserida fixa, pois o processo deve deixar de existir com a implementação da TIP
*        "===================================================USER STORY 61743 / Anderson Oenning
**      ELSEIF zlest0026-pedagio GT 2200 AND  vg_pedi_pedagio EQ abap_true.
**        MESSAGE i079(zles) .
**        EXIT.
*        "===================================================USER STORY 61743 / Anderson Oenning
*      ENDIF.
*
*      "Verificar Autorização de Viagem """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*      "Verificar Autorização de Viagem """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*      IF zlest0026-ck_credita_ped EQ abap_true AND vg_cartao_pedagio = abap_true.
*        CASE zlest0026-tp_admim_ped.
*          WHEN '03'. "REPOM S.A.
*
*            CALL METHOD zcl_repom_viagem_vpr=>get_id_proc_cliente_vt
*              EXPORTING
*                i_tknum           = vl_tknum
*              RECEIVING
*                e_id_proc_cliente = zlest0026-id_proc_cliente
*              EXCEPTIONS
*                nao_encontrado    = 1
*                OTHERS            = 2.
*
*            IF sy-subrc IS NOT INITIAL.
*              MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*              EXIT.
*            ENDIF.
*
*            IF zcl_repom_viagem_vpr=>get_autorizado( EXPORTING i_id_proc_cliente = zlest0026-id_proc_cliente ) NE abap_true.
*              CLEAR: zlest0026-pedagio.
*              MESSAGE i037(zrepom) .
*              EXIT.
*            ELSE.
*              CREATE OBJECT obj_pedagio EXPORTING i_id_proc_cliente = zlest0026-id_proc_cliente.
*              obj_pedagio->get_registro( IMPORTING e_registro = wa_zlest0123 ).
*              IF zlest0026-pedagio NE wa_zlest0123-vlr_total_pedagio.
*                zlest0026-pedagio = wa_zlest0123-vlr_total_pedagio.
*                CLEAR: obj_pedagio.
*                EXIT.
*              ELSE.
*                CLEAR: obj_pedagio.
*              ENDIF.
*            ENDIF.
*          WHEN '09'. "TipFrete
*            IF zlest0026-placa_cav IS NOT INITIAL.
*              IF wa_zlest0135-ck_sem_parar EQ abap_false AND zlest0026-tp_card_ped EQ 'S'. "Sem Para
*                sy-msgv1 = wa_zlest0135-ds_msg_veiculo+000(50).
*                sy-msgv2 = wa_zlest0135-ds_msg_veiculo+050(50).
*                sy-msgv3 = wa_zlest0135-ds_msg_veiculo+100(50).
*                sy-msgv4 = wa_zlest0135-ds_msg_veiculo+150(50)..
*                MESSAGE i000(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*                EXIT.
*              ENDIF.
*            ENDIF.
*        ENDCASE.
*      ENDIF.
*      "Verificar Autorização de Viagem """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*
*      "Tip Frete """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*      IF zlest0026-tp_admim_ped NE '03'.
*        IF vg_cartao_pedagio EQ abap_true AND zlest0026-tp_card_ped IS INITIAL.
*          MESSAGE i081(zles).
*          EXIT.
*        ELSEIF ( vg_cartao_pedagio     EQ abap_true   ) AND
*               ( zlest0026-tp_card_ped IS NOT INITIAL ) AND
*               ( zlest0026-tp_card_ped NE 'S'         ) AND "S = Sem Parar
*               ( zlest0026-tp_card_ped NE 'O'         ). "O = Visa Cargo
*
*          SELECT SINGLE * INTO @DATA(wa_zlest0002_card)
*            FROM zlest0002_card
*           WHERE pc_veiculo  EQ @zlest0026-placa_cav
*             AND tp_card_ped EQ @zlest0026-tp_card_ped.
*
*          IF sy-subrc IS INITIAL.
*            IF ( zlest0026-nr_card_ped IS INITIAL ) OR ( zlest0026-nr_card_ped NE wa_zlest0002_card-nr_card_ped ).
*              zlest0026-nr_card_ped = wa_zlest0002_card-nr_card_ped.
*              EXIT.
*            ELSE.
*              zlest0026-nr_card_ped = wa_zlest0002_card-nr_card_ped.
*              IF zlest0026-ck_credita_ped EQ abap_false.
*                CLEAR: zlest0026-pedagio.
*              ENDIF.
*            ENDIF.
*          ELSE.
*            MESSAGE i081(zles) .
*            EXIT.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
** Documento de custo de frete
*      SELECT SINGLE fknum
*      INTO   vl_fknum
*      FROM   vfkp
*      WHERE  refty = '8' AND
*             rebel = vl_tknum.
*
*      IF sy-subrc IS INITIAL.
*        MESSAGE w024(zles) WITH vl_fknum.
*        EXIT.
*      ELSE.
*
** Cabeçalho do documento de transporte
*        SELECT SINGLE tplst tdlnr
*          INTO  (vl_tplst,vl_tdlnr)
*          FROM vttk
*         WHERE tknum = vl_tknum.
*
** Local de organizaçao de transporte
*        SELECT SINGLE bukrs
*        INTO   vl_bukrs
*        FROM   ttds
*        WHERE  tplst = vl_tplst.
*
*        TRY .
*            DATA(r_margadto) = zcl_calc_frete=>get_valor_adiantamento(
*            EXPORTING
*              i_bukrs        = vl_bukrs
*              i_branch       = vl_tplst
*              i_lifnr        = wa_zlest0135-cd_transportador ).
*
*            vl_adto = ( wk_netwr_all * r_margadto ) / 100.
*            IF zlest0026-adto > vl_adto.
*              MESSAGE i022(zles) WITH r_margadto.
*              EXIT.
*            ENDIF.
*
*          CATCH zcx_calc_frete.
*        ENDTRY.
*
** Determinação do itinerário
*        SELECT SINGLE route
*        INTO   vl_route
*        FROM   vttk
*        WHERE  tknum = vl_tknum.
*
*        IF sy-subrc IS INITIAL.
*          IF ( NOT vg_pedi_pedagio IS INITIAL ) AND ( zlest0026-pedagio GT 0 ).
*            vg_pedi_informado = 'X'.
*          ELSE.
*            CLEAR: vg_pedi_informado.
*          ENDIF.
*        ENDIF.
*
*        DATA(ck_gravou) = abap_false.
*        PERFORM gravar_registro CHANGING ck_gravou.
*        IF ck_gravou EQ abap_true.
*          LEAVE TO SCREEN 0.
*        ENDIF.
*
*      ENDIF.

    WHEN 'CANC'.

      IF vg_ck_ped_param = abap_false.
        MESSAGE i092(zles).
        EXIT.
      ENDIF.

      CALL METHOD zcl_repom_viagem_vpr=>get_id_proc_cliente_vt
        EXPORTING
          i_tknum           = vl_tknum
        RECEIVING
          e_id_proc_cliente = zlest0026-id_proc_cliente
        EXCEPTIONS
          nao_encontrado    = 1
          OTHERS            = 2.

      IF sy-subrc IS INITIAL.
        MESSAGE i064(zrepom) WITH zlest0026-id_proc_cliente.
        EXIT.
      ENDIF.

      IF zlest0026-pedagio LE 0 AND vg_pedi_pedagio = 'X'.
        MESSAGE i070(zles) .
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'CLEAR'.

    WHEN OTHERS.

      CHECK vg_ck_admim_ped EQ abap_false.

      IF vg_ck_ped_param = abap_false.
        MESSAGE i092(zles).
        EXIT.
      ENDIF.

      CALL METHOD zcl_repom_viagem_vpr=>get_id_proc_cliente_vt
        EXPORTING
          i_tknum           = vl_tknum
        RECEIVING
          e_id_proc_cliente = zlest0026-id_proc_cliente
        EXCEPTIONS
          nao_encontrado    = 1
          OTHERS            = 2.

      IF sy-subrc IS INITIAL.
        MESSAGE i064(zrepom) WITH zlest0026-id_proc_cliente.
        EXIT.
      ENDIF.

      IF vg_cartao_pedagio EQ abap_true AND zlest0026-tp_card_ped IS NOT INITIAL.
        SELECT SINGLE * FROM zlest0002_card INTO @DATA(wa_zlest0002_card)  "*-#133089-21.02.2024-JT
         WHERE pc_veiculo  EQ @zlest0026-placa_cav
           AND tp_card_ped EQ @zlest0026-tp_card_ped.

        IF sy-subrc IS INITIAL.
          zlest0026-nr_card_ped = wa_zlest0002_card-nr_card_ped.
        ENDIF.
      ENDIF.

      IF zlest0026-pedagio LE 0  AND vg_pedi_pedagio = 'X'.
        MESSAGE i070(zles) .
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_ORG_DEST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_org_dest INPUT.
  vg_ck_alterou_cidade = abap_true.
  vg_ck_credito_ped    = abap_false.
ENDMODULE.                 " ALTEROU_ORG_DEST  INPUT

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_TP_ADMIM_PED  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_tp_admim_ped INPUT.
  vg_ck_admim_ped  = abap_true.
  "VG_CK_CREDITO_PED = ABAP_FALSE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_CK_CREDITO_PED  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_ck_credito_ped INPUT.
  vg_ck_credito_ped = abap_true.
  vg_ck_credito_pedt = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_TP_ADMIM_FRETE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_tp_admim_frete INPUT.
  vg_ck_admim_frete = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GRAVAR_REGISTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CK_GRAVOU  text
*----------------------------------------------------------------------*
FORM gravar_registro CHANGING p_ck_gravou TYPE char01.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZTRP01'
    IMPORTING
      number                  = vl_chvadto
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc IS INITIAL.

    EXPORT vl_chvadto TO MEMORY ID 'ZADTOPED'.

    "Se Administradora de Frete for a TipFrete e estiver sem versão XML pegar 1.17
    IF zlest0026-tp_admim_frete EQ '09' AND zlest0026-nr_vr_xml_tipf IS INITIAL.
      zlest0026-nr_vr_xml_tipf = '1.17'.
    ENDIF.

    MOVE : sy-mandt                   TO wk_0026-mandt,
           vl_chvadto                 TO wk_0026-tknum,
           vl_chvadto                 TO wk_0026-seq_controle,
           zlest0026-adto             TO wk_0026-adto,
           zlest0026-pedagio          TO wk_0026-pedagio,
           sy-datum                   TO wk_0026-erdat,
           sy-uzeit                   TO wk_0026-uzeit,
           sy-uname                   TO wk_0026-uname,
           zlest0026-tp_card_ped      TO wk_0026-tp_card_ped,
           zlest0026-nr_card_ped      TO wk_0026-nr_card_ped,
           zlest0026-id_rota          TO wk_0026-id_rota,
           zlest0026-qtd_eixo         TO wk_0026-qtd_eixo,
           zlest0026-nr_vr_xml_tipf   TO wk_0026-nr_vr_xml_tipf,
           zlest0026-ck_credita_ped   TO wk_0026-ck_credita_ped,
           zlest0026-tx_obs_cred_mesm TO wk_0026-tx_obs_cred_mesm,
           zlest0026-tp_admim_ped     TO wk_0026-tp_admim_ped,
           zlest0026-tp_admim_frete   TO wk_0026-tp_admim_frete,
           zlest0026-id_proc_cliente  TO wk_0026-id_proc_cliente,
           wa_xvttk-id_carga          TO wk_0026-id_carga,
           wa_xvttk-id_romaneio       TO wk_0026-id_romaneio,
           wa_xvttk-shtyp             TO wk_0026-shtyp,
           wa_xvttk-abfer             TO wk_0026-abfer.
    MODIFY zlest0026 FROM wk_0026.
    p_ck_gravou = abap_true.
  ENDIF.

ENDFORM.
