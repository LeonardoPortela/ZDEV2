class ZCL_DISTRIBUICAO_INSUMOS definition
  public
  final
  create public .

public section.

  constants C_CONCLUIDO type CHAR01 value 'C' ##NO_TEXT.
  constants C_PENDENTE type CHAR01 value 'P' ##NO_TEXT.
  constants C_PEND_APROV type CHAR01 value 'W' ##NO_TEXT.
  constants C_APROVADO type CHAR01 value 'A' ##NO_TEXT.
  constants C_OVEN type ZDE_ETAPA value 'OVEN' ##NO_TEXT.
  data AT_DISTRIBUICAO_INSUMOS type ref to ZCL_DISTRIBUICAO_INSUMOS .
  data AT_CH_REFERENCIA type ZID_INTEGRACAO .
  data AT_ORDEM_ETAPA type NUMC3 .
  data AT_ID_AGRUPAR type SYSUUID_X16 .
  data AT_REPROCESSAR type CHAR01 .
  data AT_ZSDT0082 type ZSDT0082 .
  data AT_ZSDS093 type ZSDS093 .
  data AT_T_ZSDS094 type ZSDS094_TT .
  data AT_ZSDS094_TT type ZSDS094_TT .
  data AT_T_ZSDT0414 type ZSDT0414_T .
  data AT_T_ZSDT0415 type ZSDT0415_T .
  data AT_ITEM_DISTRIB type ZITEM_DISTRIB .
  data AT_MARA type MARA .
  data AT_MAKT type MAKT .
  data AT_MARC type MARC .

  methods SET_LIBERAR_EMBARQUE
    importing
      !I_ZSDT0415 type ZSDT0415
    raising
      ZCX_ERROR .
  methods SET_APROVAR_EMBARQUE
    importing
      !I_ZSDT0415 type ZSDT0415
    raising
      ZCX_ERROR .
  methods SET_CRIAR_SOLICITACAO
    importing
      !I_ZSDT0414 type ZSDT0414
      !I_ZSDT0415 type ZSDT0415
    raising
      ZCX_ERROR .
  methods GET_UUID
    returning
      value(R_GUID) type SYSUUID_X16 .
  methods SET_QTDE_SOLICITADA_ZSDT0082
    importing
      !I_ZSDT0415 type ZSDT0415
    raising
      ZCX_ERROR .
  methods SET_VALIDAR_DISTRIBUICAO
    raising
      ZCX_ERROR .
  methods SET_EXECUTAR_DISTRIBUICAO
    importing
      !I_ZSDS093 type ZSDS093
      !I_ZSDS094 type ZSDS094_TT
      !I_SOMENTE_VALIDAR type CHAR01 optional
      !I_SOMENTE_MONTAR_EVENTOS type CHAR01 optional
      !I_REPROCESSAR type CHAR01 optional
    returning
      value(R_DISTRIBUICAO_INSUMOS) type ref to ZCL_DISTRIBUICAO_INSUMOS
    raising
      ZCX_ERROR .
  methods SET_TRATAR_PROCESSAMENTO
    returning
      value(R_PROSSEGUE) type CHAR01
    raising
      ZCX_ERROR .
  methods SET_AJUSTAR_DADOS .
  methods GET_ZSDT0082
    importing
      !I_NRO_SOL type ZDE_NRO_SOL
      !I_SEQ type NUMC3
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
    returning
      value(R_ZSDT0082) type ZSDT0082 .
  methods GET_ZSDT0411
    importing
      !I_NRO_SOL type ZDE_NRO_SOL
      !I_ID_DISTRIBUICAO type ZDE_ID_DISTRIB optional
      !I_TODOS type CHAR01 optional
    exporting
      !E_ZSDT0411 type ZSDT0411_T
    returning
      value(R_ZSDT0411) type ZSDT0411 .
  methods GET_ZSDT0414
    importing
      !I_CH_REFERENCIA type ZID_INTEGRACAO
    returning
      value(R_ZSDT0414) type ZSDT0414 .
  methods GET_ZSDT0415
    importing
      !I_CH_REFERENCIA type ZID_INTEGRACAO
      !I_SOMENTE_MAIS_ATUAL type CHAR01 optional
      !I_SOMENTE_SELECIONADOS type CHAR01 optional
      !I_SOMENTE_PENDENTES type CHAR01 optional
      !I_INCLUIR_CANCELADO type CHAR01 optional
      !I_ZSDT0415 type ZSDT0415_T optional
    returning
      value(R_ZSDT0415) type ZSDT0415_T .
  methods SET_TABLE_ZSDT0415
    importing
      !I_ZSDT0415 type ZSDT0415_T .
  methods GET_TABLE_ZSDT0415
    importing
      !I_ZSDS094 type ZSDS094
    returning
      value(R_ZSDT0415) type ZSDT0415 .
  methods GET_PENDENTE_APROVACAO
    importing
      !I_CH_REFERENCIA type ZID_INTEGRACAO
    returning
      value(R_PENDENTE) type CHAR01 .
  methods SET_CANCELAR_SOLICITACAO
    importing
      !I_CH_REFERENCIA type ZID_INTEGRACAO
      !I_ID_DISTRIBUICAO type ZDE_ID_DISTRIB .
  methods SET_ESTORNAR_SOLICITACAO
    importing
      !I_CH_REFERENCIA type ZID_INTEGRACAO
      !I_ITEM_DISTRIB type ZITEM_DISTRIB .
  methods SET_STATUS_AVALIACAO_SOLIC
    importing
      !I_NRO_SOL type ZDE_NRO_SOL
      !I_ID_DISTRIBUICAO type ZDE_ID_DISTRIB
      !I_STATUS type ZDE_STATUS_VE
      !I_USER_ACAO type ZDE_USER_CREATE optional
      !I_DATE_ACAO type ZDE_DATE_CREATE optional
      !I_TIME_ACAO type ZDE_TIME_CREATE optional
      !I_OBSERVACAO type CATSXT_LONGTEXT_ITAB optional .
  methods SET_AVALIAR_STATUS_SOLIC
    importing
      !I_NRO_SOL type ZDE_NRO_SOL
      !I_SEQ type NUMC3
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA .
  methods GET_NEXT_ITEM_DISTRIB
    returning
      value(R_ITEM_DISTRIB) type ZITEM_DISTRIB .
  methods SET_MONTAR_EVENTOS
    raising
      ZCX_ERROR .
  methods SET_CRIAR_TABELA_EVENTOS
    importing
      !I_ETAPA type ZDE_ETAPA
      !I_ZSDS094 type ZSDS094
    raising
      ZCX_ERROR .
  methods SET_TRATAR_VENDA_ESPECIAL .
  methods SET_MODIFICAR_STATUS
    importing
      !I_STATUS type ZDE_STATUS_SOLI
      !I_ITEM_DISTRIB type ZITEM_DISTRIB
      !I_ETAPA type ZDE_ETAPA
      !I_MENSAGEM type STRING optional .
  methods SET_TRANSFORM_TAB_RETURN
    importing
      !I_BAPIRET type BAPIRET2_T
    returning
      value(R_MENSAGEM) type STRING .
  methods GET_CH_REFERENCIA
    returning
      value(R_CH_REFERENCIA) type ZID_INTEGRACAO .
  methods SET_BLOQUEIO_SOLICITACAO
    raising
      ZCX_ERROR .
  methods SET_DESBOQUEIO_SOLICITACAO
    importing
      !I_ZSDT0415 type ZSDT0415
    raising
      ZCX_ERROR .
  methods SET_CRIAR_ORDEM_VENDA
    importing
      !I_ZSDT0414 type ZSDT0414
      !I_ZSDT0415 type ZSDT0415
    raising
      ZCX_ERROR .
  methods SET_TRATA_STRING
    importing
      !I_MESG type STRING
    exporting
      !E_MSGV1 type SYST_MSGV
      !E_MSGV2 type SYST_MSGV
      !E_MSGV3 type SYST_MSGV
      !E_MSGV4 type SYST_MSGV .
  methods SET_VALIDAR
    importing
      !I_ZSDS093 type ZSDS093
      !I_ZSDS094 type ZSDS094_TT
    raising
      ZCX_ERROR .
  methods SET_INTEGRAR_SAFRA_CONTROL
    importing
      !I_DADOS type ZMME_DADOS_SAFRA
    exporting
      !E_MSG_ERRO type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_STATUS_PROCESSAMENTO
    importing
      !I_NRO_SOL type ZDE_NRO_SOL
      !I_SEQ type NUMC3
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
      !I_STATUS type CHAR01
    returning
      value(R_DISTRIBUICAO_INSUMOS) type ref to ZCL_DISTRIBUICAO_INSUMOS .
  methods GET_VERIFICAR_CONSUMO
    importing
      !I_ZSDS094 type ZSDS094
    returning
      value(R_UTILIZADO) type DZMENG .
  methods GET_DADOS_ZSDT0415
    importing
      !I_ZSDS093 type ZSDS093
      !I_ZSDS094 type ZSDS094
    returning
      value(R_ZSDT0415) type ZSDT0415_T .
  methods GET_INSTANCE
    returning
      value(R_DISTRIBUICAO_INSUMOS) type ref to ZCL_DISTRIBUICAO_INSUMOS .
  methods SET_INTEGRAR_SAFRA_CONTROL_BK
    importing
      !I_DADOS type ZMME_DADOS_SAFRA
    exporting
      !E_MSG_ERRO type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ANALISAR_ESTOQUE
    importing
      !I_ZSDT0415 type ZSDT0415
    returning
      value(R_CONSUMO) type KWMENG .
  methods GET_PERMITE_CANCELAR_DISTR
    importing
      !I_CH_REFERENCIA type ZID_INTEGRACAO
    returning
      value(R_PERMITE_CANCELAR) type CHAR01 .
  methods CHECK_USO_ESTOQUE_SOL_ENT
    importing
      !I_ESTOQUE_PEDIDO type ZSDS094
    returning
      value(R_CONSUMO) type KWMENG .
  methods GET_CONSUMO_REAL
    importing
      !I_ZSDT0415 type ZSDT0415
    returning
      value(R_CONSUMO) type KWMENG .
  methods SET_CRIAR_JOB
    importing
      !I_ZSDS093 type ZSDS093
      !I_ZSDS094 type ZSDS094_TT
    raising
      ZCX_ERROR .
  methods SET_LIMPAR_MENSAGEM .
  methods GET_CK_JOB_CANCELADO
    importing
      !I_ZSDT0082 type ZSDT0082
    returning
      value(R_CANCELADO) type CHAR01 .
  methods GET_CK_ESTOQUE_FILIAL_ARMAZ
    raising
      ZCX_ERROR .
  class-methods CANCELAR_DISTRIBUICAO
    importing
      !I_NRO_SOL type ZSDT0082-NRO_SOL
      !I_SEQ type ZSDT0082-SEQ
      !I_VBELN type ZSDT0082-VBELN
      !I_POSNR type ZSDT0082-POSNR
    returning
      value(R_MSG_ERROR) type STRING .
  methods SET_SALVAR_COPIA_DISTRIB
    importing
      !I_ZSDS094 type ZSDS094_TT .
  PROTECTED SECTION.

private section.

  types:
    BEGIN OF TY_SAIDA_EXEC,
        INCO1    TYPE ZSDT0041-WERKS,
        SPART    TYPE ZSDT0041-SPART,
        AUART    TYPE ZSDT0041-AUART,
        WERKS    TYPE ZSDT0041-WERKS,
        VBELN    TYPE VBAK-VBELN,
        MSG(255),
      END OF TY_SAIDA_EXEC .
  types:
    BEGIN OF TY_FCAT.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
    TYPES: END OF TY_FCAT .

  class-data AT_SALESDOCUMENT type VBELN_VA .
  class-data AT_ORDER_HEADER_IN type BAPISDH1 .
  class-data AT_ORDER_HEADER_INX type BAPISDH1X .
  class-data AT_SALES_HEADER_IN type BAPISDHD1 .
  class-data AT_LOGIC_SWITCH type BAPISDLS .
  class-data AT_ORDER_ITEM_IN type OIL_BAPISDIM_T .
  class-data AT_ORDER_ITEM_INX type OIL_BAPISDIMX_T .
  class-data AT_CONDITIONS_IN type OIT_T_COND .
  class-data AT_CONDITIONS_INX type OIT_T_CONDX .
  class-data AT_SCHEDULE_LINES type OIJ_BAPISCHDL_T .
  class-data AT_SCHEDULE_LINESX type OIJ_BAPISCHDLX_T .
  class-data AT_PARTNERS type OIJ_BAPIPARNR_T .
  class-data AT_ORDER_TEXT type BAPISDTEXT_T .
  class-data AT_EXTENSIONIN type T_BAPIPAREX .
  class-data AT_RETURN type BAPIRET2_T .
  class-data AT_VBAK type VBAK .
  class-data AT_VBAP type VBAP_T .
  class-data AT_VBAK_NEW type VBAK .
  class-data AT_VBAP_NEW type VBAP_T .
  class-data AT_VBEP type VBEP_T .
  class-data AT_BEHAVE_WHEN_ERROR type CHAR1 .
  class-data AT_SELECAO_87 type ZDE_SELECAO_87 .
  class-data AT_DADOS_TRATADOS type ZSDS015_T .
  class-data AT_ADITIVOS type ZSDT0090 .
  class-data AT_VBELN_OLD type VBELN_VA .
  class-data AT_VBELN_NEW type VBELN_VA .
  class-data AT_SOLIICTACAO type ZSDED003 .
  class-data AT_MANUTENCAO type ZDE_MANUTENCAO_OV .
  class-data:
    AT_MENSAGENS TYPE TABLE OF TY_SAIDA_EXEC .
  class-data:
    AT_FCAT_MSG TYPE TABLE OF TY_FCAT .
  class-data AT_BACKGROUND type ABAP_BOOL .
  constants C_ZSDT0081 type TCODE value 'ZSDT0081' ##NO_TEXT.

  methods SET_ERROR
    importing
      !I_MESG type STRING
    raising
      ZCX_ERROR .
ENDCLASS.



CLASS ZCL_DISTRIBUICAO_INSUMOS IMPLEMENTATION.


  METHOD get_ch_referencia.

    DATA: l_seq TYPE char14.

    FREE: r_ch_referencia.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZSD_EMBARQ'
        toyear      = sy-datum(4)
      IMPORTING
        number      = l_seq.

    CHECK sy-subrc = 0.

    r_ch_referencia = 'CH' && sy-datum(4) && l_seq.

  ENDMETHOD.


  METHOD get_instance.

    IF at_distribuicao_insumos IS NOT BOUND.
      CREATE OBJECT at_distribuicao_insumos.
    ENDIF.

    r_distribuicao_insumos = at_distribuicao_insumos.

  ENDMETHOD.


  METHOD get_next_item_distrib.

    FREE: r_item_distrib.

    SELECT MAX( item_distrib )
      INTO    r_item_distrib
      FROM zsdt0415
     WHERE ch_referencia = me->at_ch_referencia.

  ENDMETHOD.


  METHOD get_uuid.

    FREE: r_guid.

    TRY.
        r_guid = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_verificar_consumo.

    FREE: r_utilizado.

    SELECT SUM( qte_sol )
      INTO r_utilizado
      FROM zsdt0415
     WHERE ordem_etapa = 1
       AND matnr       = i_zsds094-matnr
       AND mtart       = i_zsds094-mtart
       AND werks       = i_zsds094-werks
       AND lgort       = i_zsds094-lgort
       AND ebeln       = i_zsds094-ebeln
       AND ebelp       = i_zsds094-ebelp
*      AND marca       = i_zsds094-marca
       AND charg       = i_zsds094-charg
       AND licha       = i_zsds094-licha
*      AND lifnr       = i_zsds094-lifnr
       AND cancelado   = abap_false.

  ENDMETHOD.


  METHOD get_zsdt0082.

    FREE: r_zsdt0082.

    SELECT SINGLE *
      INTO r_zsdt0082
      FROM zsdt0082
     WHERE nro_sol = i_nro_sol
       AND vbeln   = i_vbeln
       AND posnr   = i_posnr
       AND seq     = i_seq.

  ENDMETHOD.


  METHOD get_zsdt0411.

    FREE: r_zsdt0411, e_zsdt0411.

    IF i_todos = abap_true.
      SELECT *
        INTO TABLE e_zsdt0411
        FROM zsdt0411
       WHERE nro_sol         = i_nro_sol.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      INTO r_zsdt0411
      FROM zsdt0411
     WHERE nro_sol         = i_nro_sol
       AND id_distribuicao = i_id_distribuicao.

  ENDMETHOD.


  METHOD get_zsdt0414.

    FREE: r_zsdt0414.

    SELECT SINGLE *
      INTO r_zsdt0414
      FROM zsdt0414
     WHERE ch_referencia = i_ch_referencia.

  ENDMETHOD.


  METHOD get_zsdt0415.

    DATA: t_0415 TYPE TABLE OF zsdt0415.

    FREE: r_zsdt0415.

    IF i_somente_selecionados = abap_true AND i_zsdt0415[] IS NOT INITIAL.
      t_0415[] = i_zsdt0415[].

      SORT t_0415 BY ch_referencia item_distrib.
      DELETE ADJACENT DUPLICATES FROM t_0415 COMPARING ch_referencia item_distrib.

      SELECT *
        INTO TABLE r_zsdt0415
        FROM zsdt0415
         FOR ALL ENTRIES IN t_0415
       WHERE ch_referencia = t_0415-ch_referencia
         AND item_distrib  = t_0415-item_distrib
         AND cancelado     = abap_off.
      RETURN.
    ENDIF.

    IF i_incluir_cancelado = abap_true.
      SELECT *
        INTO TABLE r_zsdt0415
        FROM zsdt0415
       WHERE ch_referencia = i_ch_referencia.
    ELSE.
      SELECT *
        INTO TABLE r_zsdt0415
        FROM zsdt0415
       WHERE ch_referencia = i_ch_referencia
         AND cancelado     = abap_off.
    ENDIF.

    IF i_somente_mais_atual = abap_true.
      t_0415[] = r_zsdt0415[].

      SORT t_0415 BY ch_referencia item_distrib DESCENDING.

      READ TABLE t_0415 INTO DATA(_0415) INDEX 1.

      SELECT *
        INTO TABLE r_zsdt0415
        FROM zsdt0415
       WHERE ch_referencia = _0415-ch_referencia
         AND item_distrib  = _0415-item_distrib
         AND cancelado     = abap_off.

    ELSEIF i_somente_pendentes = abap_true.
      t_0415[] = r_zsdt0415[].

      DELETE t_0415 WHERE status = c_concluido.

      IF t_0415[] IS NOT INITIAL.
        SELECT *
          INTO TABLE r_zsdt0415
          FROM zsdt0415
           FOR ALL ENTRIES IN t_0415
         WHERE ch_referencia = t_0415-ch_referencia
           AND item_distrib  = t_0415-item_distrib
           AND cancelado     = abap_off.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD set_aprovar_embarque.

    DATA: lc_vbeln  TYPE vbeln_va,
          lc_posnn  TYPE posnr_va,
          lc_vbelv  TYPE vbeln_va,
          lc_seq    TYPE zsdt0116-seq,  "*-CS2025000249-04.09.2025-#189804-JT-inicio
          lv_mesg   TYPE string,
          lv_return TYPE string,
          t_ret     TYPE bapiret2_t.

    SELECT SINGLE *
      INTO @DATA(_zsdt0415)
      FROM zsdt0415
     WHERE ch_referencia = @i_zsdt0415-ch_referencia
       AND item_distrib  = @i_zsdt0415-item_distrib
       AND etapa         = @c_oven
       AND cancelado     = @abap_off.

    IF sy-subrc <> 0 OR ( sy-subrc = 0 AND _zsdt0415-vbeln_new IS INITIAL ).
      lv_mesg = 'Não foi encontrada OV na tabela ZSDT0415'.
      me->set_error( lv_mesg ).
    ENDIF.

    lc_vbelv = me->at_zsdt0082-vbeln.
    lc_vbeln = _zsdt0415-vbeln_new.
    lc_posnn = _zsdt0415-posnr_new.

    SELECT SINGLE *
      INTO @DATA(_zsdt0116)
      FROM zsdt0116
     WHERE vbeln = @_zsdt0415-vbeln_new
       AND posnr = @_zsdt0415-posnr_new.

    IF sy-subrc <> 0.
      SELECT SINGLE *
        INTO @_zsdt0116
        FROM zsdt0116
       WHERE vbeln = @_zsdt0415-vbeln_new.
    ENDIF.

    CHECK sy-subrc <> 0.

*-----------------------------------
*-- liberacao embarque
*-----------------------------------
    zcl_manutencao_insumos=>send_aprovadores_embarque( EXPORTING i_vbeln      = lc_vbeln
                                                                 i_posnn      = lc_posnn
                                                                 i_vbelv      = lc_vbelv
                                                       IMPORTING e_sequencial = lc_seq   "*-CS2025000249-04.09.2025-#189804-JT-inicio
                                                                 r_return     = t_ret ).

    IF t_ret[] IS NOT INITIAL.
      READ TABLE t_ret INTO DATA(_ret) WITH KEY type = 'E'.
      IF sy-subrc = 0.
        lv_return = me->set_transform_tab_return( i_bapiret  = t_ret ).
        lv_mesg   = 'Erro ao Aprovar Embarque' && '|' && lv_return.
        me->set_error( lv_mesg ).
      ENDIF.
    ENDIF.

*-CS2025000249-04.09.2025-#189804-JT-inicio
    SELECT SINGLE *
      INTO @_zsdt0116
      FROM zsdt0116
     WHERE seq   = @lc_seq
       AND vbeln = @_zsdt0415-vbeln_new.

    IF sy-subrc <> 0.
      lv_mesg   = 'Erro ao Aprovar Embarque. OV:' && _zsdt0415-vbeln_new.
      me->set_error( lv_mesg ).
    ENDIF.
*-CS2025000249-04.09.2025-#189804-JT-fim

  ENDMETHOD.


  METHOD set_avaliar_status_solic.

    DATA: lv_bloqueio TYPE zsdt0082-bloqueio.

    lv_bloqueio = abap_false.

    SELECT SINGLE ch_referencia
      INTO @DATA(_ch_referencia)
      FROM zsdt0082
     WHERE nro_sol  = @i_nro_sol
       AND seq      = @i_seq
       AND vbeln    = @i_vbeln
       AND posnr    = @i_posnr.

    CHECK sy-subrc = 0 AND _ch_referencia IS NOT INITIAL.

    SELECT ch_referencia, item_distrib, etapa, status
      INTO TABLE @DATA(t_zsdt0415)
      FROM zsdt0415
     WHERE ch_referencia = @_ch_referencia
       AND nro_sol       = @i_nro_sol
       AND vbeln         = @i_vbeln
       AND posnr         = @i_posnr
       AND cancelado     = @abap_off.

    LOOP AT t_zsdt0415 INTO DATA(_zsdt0415) WHERE status <> c_concluido.
      lv_bloqueio = abap_true.
      EXIT.
    ENDLOOP.

    UPDATE zsdt0082 SET bloqueio  = lv_bloqueio
                  WHERE nro_sol   = i_nro_sol
                    AND seq       = i_seq
                    AND vbeln     = i_vbeln
                    AND posnr     = i_posnr.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_bloqueio_solicitacao.

    DATA: lv_mesg TYPE string.

*   CALL FUNCTION 'ENQUEUE_EZSDT0082'
*     EXPORTING
*       mode_zsdt0082  = 'E'
*       mandt          = sy-mandt
*       nro_sol        = me->at_zsdt0082-nro_sol
*       seq            = me->at_zsdt0082-seq
*       vbeln          = me->at_zsdt0082-vbeln
*       posnr          = me->at_zsdt0082-posnr
*     EXCEPTIONS
*       foreign_lock   = 1
*       system_failure = 2
*       OTHERS         = 3.

    UPDATE zsdt0082 SET bloqueio = abap_true
                  WHERE nro_sol  = me->at_zsdt0082-nro_sol
                    AND vbeln    = me->at_zsdt0082-vbeln
                    AND posnr    = me->at_zsdt0082-posnr
                    AND seq      = me->at_zsdt0082-seq.

    IF sy-subrc <> 0.
      lv_mesg = 'Erro ao Bloquear Registro!'.
      me->set_error( lv_mesg ).
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_cancelar_solicitacao.

    UPDATE zsdt0415 SET cancelado       = abap_true
                  WHERE ch_referencia   = i_ch_referencia
                    AND id_distribuicao = i_id_distribuicao.

    SELECT SINGLE nro_sol, seq, vbeln, posnr
       FROM zsdt0082
       INTO @DATA(_zsdt0082)
      WHERE ch_referencia = @i_ch_referencia
        AND seq           = 1.

    me->set_avaliar_status_solic( i_nro_sol = _zsdt0082-nro_sol i_seq = _zsdt0082-seq i_vbeln = _zsdt0082-vbeln i_posnr = _zsdt0082-posnr ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  method set_criar_ordem_venda.

    data: lc_manutencao type zde_manutencao,
          t_ret         type bapiret2_t,
          lc_vbeln      type vbeln_va,
          lc_posnr      type posnr_va,
          lv_mesg       type string,
          lv_return     type string.

    lc_manutencao-ch_referencia        = i_zsdt0415-ch_referencia.
    lc_manutencao-item_distrib         = i_zsdt0415-item_distrib.
    lc_manutencao-vbeln_new-werks      = i_zsdt0415-werks.
    lc_manutencao-vbeln_new-matnr      = i_zsdt0415-matnr.
    lc_manutencao-vbeln_new-lgort      = i_zsdt0415-lgort.
    lc_manutencao-vbeln_new-quantidade = i_zsdt0415-kwmeng.
    lc_manutencao-vbeln_new-nr_rot_lr  = me->at_zsdt0082-nr_rot.
    lc_manutencao-vbeln_new-nr_rot_pc  = i_zsdt0415-nr_rot_pc.
    lc_manutencao-vbeln_new-kunnr      = i_zsdt0414-kunnr.
    lc_manutencao-vbeln_old-vbeln      = i_zsdt0414-vbeln.
    lc_manutencao-vbeln_old-posnr      = i_zsdt0414-posnr.

*-----------------------------------
*-- cria OV / Item OV
*-----------------------------------
    sy-tcode = me->c_zsdt0081.

    data(obj_tcode) = zcl_memory_temp_single=>get_instance(  ).
    obj_tcode->set_data( conv #( me->c_zsdt0081 ) ).

    zcl_manutencao_insumos=>run( exporting i_manutencao  = lc_manutencao
                                           is_background = abap_true
                                 importing e_vbeln       = lc_vbeln
                                           e_posnr       = lc_posnr
                                           r_return      = t_ret ).

    if lc_vbeln is initial.
      lv_return = me->set_transform_tab_return( i_bapiret  = t_ret ).
      lv_mesg   = 'Erro ao Criar Ordem de Venda' && '|' && lv_return.
      me->set_error( lv_mesg ).
    else.
      update zsdt0415 set vbeln_new     = lc_vbeln
                          posnr_new     = lc_posnr
                    where ch_referencia = i_zsdt0415-ch_referencia
                      and item_distrib  = i_zsdt0415-item_distrib
                      and etapa         = i_zsdt0415-etapa.

      update zsdt0082 set nr_rot_pc     = i_zsdt0415-nr_rot_pc
                    where nro_sol       = me->at_zsdt0082-nro_sol
                      and seq           = me->at_zsdt0082-seq
                      and vbeln         = me->at_zsdt0082-vbeln
                      and posnr         = me->at_zsdt0082-posnr.

      commit work and wait.
    endif.

  endmethod.


  METHOD set_criar_solicitacao.

    DATA: t_0082            TYPE TABLE OF zsdt0082,
          w_zsdt0082        TYPE zsdt0082,
          w_vbap            TYPE vbap,
          lv_number         TYPE zde_nro_sol,
          lv_erro           TYPE char01,
          lv_mesg           TYPE string,
          lv_origem_estoque TYPE zde_origem_estoque,
          lv_tot_qte_sol    TYPE zsdt0082-qte_sol,
          lv_tot_qte_lib    TYPE zsdt0082-qte_lib,
          lv_qte_disponivel TYPE vbap-kwmeng.

    FREE: lv_number.

    w_zsdt0082 = me->at_zsdt0082.

    SELECT SINGLE *
      INTO @DATA(_zsdt0415)
      FROM zsdt0415
     WHERE ch_referencia = @i_zsdt0415-ch_referencia
       AND item_distrib  = @i_zsdt0415-item_distrib
       AND etapa         = @c_oven
       AND cancelado     = @abap_off.

    IF sy-subrc <> 0 OR ( sy-subrc = 0 AND _zsdt0415-vbeln_new IS INITIAL ).
      lv_mesg = 'Não foi encontrada OV na tabela ZSDT0415'.
      me->set_error( lv_mesg ).
    ENDIF.

* WBARBOSA US-169490 28/08/25
    IF _zsdt0415-vbeln_new IS NOT INITIAL.

      SELECT *
        FROM zsdt0116
      INTO TABLE @DATA(lt_0116)
        WHERE vbeln EQ @_zsdt0415-vbeln_new
        AND status NE @abap_true.

      DELETE lt_0116 WHERE status_workflow EQ 'L'.
      DELETE lt_0116 WHERE status_workflow EQ 'R'.

      IF lt_0116 IS INITIAL.
        lv_mesg = |OV: { _zsdt0415-vbeln_new } não esta Aprovada!|.
        me->set_error( lv_mesg ).
      ENDIF.

    ELSE.
      lv_mesg = |OV Nova esta em Branco!|.
      me->set_error( lv_mesg ).
    ENDIF.
* WBARBOSA US-169490 28/08/25

    SELECT *
      INTO TABLE t_0082
      FROM zsdt0082
     WHERE vbeln = _zsdt0415-vbeln_new
       AND posnr = _zsdt0415-posnr_new.

    SELECT SINGLE *
      INTO w_vbap
      FROM vbap
     WHERE vbeln = _zsdt0415-vbeln_new
       AND posnr = _zsdt0415-posnr_new.

    LOOP AT t_0082 INTO DATA(_0082).
      IF _0082-seq = 1  AND   _0082-status = '1'.
        lv_tot_qte_sol = lv_tot_qte_sol + _0082-qte_sol.
      ENDIF.
      IF _0082-seq >= 1 AND ( _0082-status = '1' OR _0082-status = '2' ).
        lv_tot_qte_lib = lv_tot_qte_lib + _0082-qte_lib.
      ENDIF.
    ENDLOOP.

    lv_qte_disponivel = w_vbap-kwmeng - lv_tot_qte_sol.

*-- check SISDEV
    zcl_manutencao_insumos=>check_cadastro_forn_sisdev( EXPORTING i_werks = i_zsdt0415-werks " Centro
                                                                  i_kunnr = i_zsdt0414-kunnr " Nº cliente
                                                        IMPORTING e_erro  = lv_erro ).
    IF lv_erro IS NOT INITIAL.
      lv_mesg = 'Cliente não possuem cadastro no SISDEV'.
      me->set_error( lv_mesg ).
    ENDIF.

*-- check liberada x disponivel
*   IF i_zsdt0415-qte_sol > lv_qte_disponivel.
*     lv_mesg = |Quantidade solicitada maior que a quantidade disponível para OV' { me->at_zsdt0082-vbeln }|.
*     me->set_error( lv_mesg ).
*   ENDIF.

*---------------------------------------------------
*-- checa se solicitacao ja foi criada
*---------------------------------------------------
    SELECT SINGLE *
      INTO @DATA(w_0082)
      FROM zsdt0082
     WHERE ch_referencia_ref = @_zsdt0415-ch_referencia
       AND item_distrib_ref  = @_zsdt0415-item_distrib.

    IF sy-subrc = 0.
      w_zsdt0082 = w_0082.
    ELSE.

*---------------------------------------------------
*---- obtem numeracao
*---------------------------------------------------
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'Z_NUM_SOL'
        IMPORTING
          number                  = lv_number
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF sy-subrc <> 0.
        lv_mesg = 'O intervalo de numeração, não foi encontrado!'.
        me->set_error( lv_mesg ).
      ENDIF.

*---------------------------------------------------
*---- criar solicitacao
*---------------------------------------------------
      CLEAR: w_zsdt0082-dt_liber,
             w_zsdt0082-qte_lib,
             w_zsdt0082-usuario_lib.

      IF     _zsdt0415-ebeln IS NOT INITIAL AND _zsdt0415-ebelp IS NOT INITIAL.
        lv_origem_estoque = '2'.
      ELSEIF _zsdt0415-lifnr IS NOT INITIAL.
        lv_origem_estoque = '3'.
      ELSE.
        lv_origem_estoque = '1'.
      ENDIF.

      MOVE:  lv_number                      TO w_zsdt0082-nro_sol,
             001                            TO w_zsdt0082-seq,
             _zsdt0415-vbeln_new            TO w_zsdt0082-vbeln,
             _zsdt0415-posnr_new            TO w_zsdt0082-posnr,
             _zsdt0415-werks                TO w_zsdt0082-werks,
             0                              TO w_zsdt0082-seq_lib,
             sy-datum                       TO w_zsdt0082-dt_sol,
             _zsdt0415-qte_sol              TO w_zsdt0082-qte_sol,
             sy-uname                       TO w_zsdt0082-usuario_sol,
             '1'                            TO w_zsdt0082-status,
             _zsdt0415-ebeln                TO w_zsdt0082-ebeln,
             _zsdt0415-ebelp                TO w_zsdt0082-ebelp,
             _zsdt0415-lifnr                TO w_zsdt0082-lifnr_arm,
             _zsdt0415-charg                TO w_zsdt0082-charg,
             _zsdt0415-marca                TO w_zsdt0082-marca,
             lv_origem_estoque              TO w_zsdt0082-origem_estoque,
             _zsdt0415-carga_auto           TO w_zsdt0082-carga_automatica,
             _zsdt0415-flexibilidade        TO w_zsdt0082-flexibilidade,
             _zsdt0415-prioridade           TO w_zsdt0082-prioridade,
             _zsdt0415-transf_no_fornecedor TO w_zsdt0082-transf_no_fornecedor,
             _zsdt0415-dt_entrega           TO w_zsdt0082-dt_entrega,
             _zsdt0415-nr_rot_pc            TO w_zsdt0082-nr_rot_pc,
             _zsdt0415-nro_sol              TO w_zsdt0082-nro_sol_origem,
             abap_off                       TO w_zsdt0082-bloqueio,
             abap_off                       TO w_zsdt0082-processando,
             abap_off                       TO w_zsdt0082-ch_referencia,
             _zsdt0415-ch_referencia        TO w_zsdt0082-ch_referencia_ref, "*-CS2025000249-08.09.2025-#189853-JT-inicio
             _zsdt0415-item_distrib         TO w_zsdt0082-item_distrib_ref.  "*-CS2025000249-08.09.2025-#189853-JT-inicio
      MODIFY zsdt0082                     FROM w_zsdt0082.
    ENDIF.

    UPDATE zsdt0415 SET nro_sol_new        = w_zsdt0082-nro_sol
                        vbeln_ref          = w_zsdt0082-vbeln
                        posnr_ref          = w_zsdt0082-posnr
                  WHERE ch_referencia      = i_zsdt0415-ch_referencia
                    AND item_distrib       = i_zsdt0415-item_distrib
                    AND etapa              = i_zsdt0415-etapa.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_criar_tabela_eventos.

    DATA: w_zsdt0414 TYPE zsdt0414,
          w_zsdt0415 TYPE zsdt0415,
          lv_mesg    TYPE string.

*-----------------------------------
* criar cabecalho
*-----------------------------------
    me->at_ordem_etapa                 = me->at_ordem_etapa + 1.

    MOVE-CORRESPONDING me->at_zsds093 TO w_zsdt0414.
    w_zsdt0414-ch_referencia           = me->at_ch_referencia.
    w_zsdt0414-user_create             = sy-uname.
    w_zsdt0414-date_create             = sy-datum.
    w_zsdt0414-time_create             = sy-uzeit.
    APPEND w_zsdt0414                 TO me->at_t_zsdt0414.

    MOVE-CORRESPONDING i_zsds094      TO w_zsdt0415.
    w_zsdt0415-ch_referencia           = me->at_ch_referencia.
    w_zsdt0415-item_distrib            = me->at_item_distrib.
    w_zsdt0415-id_agrupar              = me->at_id_agrupar.
    w_zsdt0415-etapa                   = i_etapa.
    w_zsdt0415-ordem_etapa             = me->at_ordem_etapa.
    w_zsdt0415-status                  = abap_off.
    w_zsdt0415-mensagem                = abap_off.
    w_zsdt0415-qte_sol                 = i_zsds094-kwmeng.
    w_zsdt0415-qte_sol_ori             = me->at_zsdt0082-qte_sol.
    w_zsdt0415-user_create             = sy-uname.
    w_zsdt0415-date_create             = sy-datum.
    w_zsdt0415-time_create             = sy-uzeit.

    IF w_zsdt0415-qte_sol > w_zsdt0415-qte_sol_ori.
      lv_mesg = 'Quantidade a liberar ultrapassa Quantidade Origem!'.
      me->set_error( lv_mesg ).
    ENDIF.

    APPEND w_zsdt0415                 TO me->at_t_zsdt0415.

  ENDMETHOD.


  METHOD set_desboqueio_solicitacao.

    DATA: lv_mesg TYPE string.

*   CALL FUNCTION 'DEQUEUE_EZSDT0082'
*     EXPORTING
*       mode_zsdt0082  = 'E'
*       mandt          = sy-mandt
*       nro_sol        = me->at_zsdt0082-nro_sol
*       seq            = me->at_zsdt0082-seq
*       vbeln          = me->at_zsdt0082-vbeln
*       posnr          = me->at_zsdt0082-posnr
*     EXCEPTIONS
*       foreign_lock   = 1
*       system_failure = 2
*       OTHERS         = 3.

*----------------------------------------
*-- validar quantidades Solic nova
*----------------------------------------
    SELECT SINGLE *
      INTO @DATA(_0415)
      FROM zsdt0415
     WHERE ch_referencia = @i_zsdt0415-ch_referencia
       AND item_distrib  = @i_zsdt0415-item_distrib
       AND etapa         = 'SOLI'
       AND cancelado     = @abap_off.

    IF sy-subrc = 0 AND _0415-nro_sol_new IS NOT INITIAL.
      SELECT SINGLE qte_sol
        INTO @DATA(_qte_sol)
        FROM zsdt0082
       WHERE nro_sol = @_0415-nro_sol_new
         AND seq     = '001'.

      IF sy-subrc = 0 AND _0415-qte_sol <> _qte_sol.
        lv_mesg = 'Quantidades Divergentes ZSDT0082 x ZSDT0415'.
        me->set_error( lv_mesg ).
      ENDIF.
    ENDIF.

*----------------------------------------
*-- validar quantidades Solic liberada
*----------------------------------------
    SELECT SINGLE *
      INTO @_0415
      FROM zsdt0415
     WHERE ch_referencia = @i_zsdt0415-ch_referencia
       AND item_distrib  = @i_zsdt0415-item_distrib
       AND etapa         = 'LIBE'
       AND cancelado     = @abap_off.

    IF sy-subrc = 0 AND _0415-nro_sol_ref IS NOT INITIAL.
      SELECT SINGLE qte_lib
        INTO @DATA(_qte_lib)
        FROM zsdt0082
       WHERE nro_sol = @_0415-nro_sol_ref
         AND seq     = @_0415-seq_ref.

      IF sy-subrc = 0 AND _0415-qte_sol <> _qte_lib.
        lv_mesg = 'Quantidades Divergentes ZSDT0082 x ZSDT0415'.
        me->set_error( lv_mesg ).
      ENDIF.
    ENDIF.

*-US191034-17.09.2025-#191034-JT-inicio
*-----------------------------------------------
*---Cancela solicitacao, se ela zerou saldo
*-----------------------------------------------
    SELECT SINGLE qte_sol
      INTO @DATA(_qte_sol_atu)
      FROM zsdt0082
     WHERE nro_sol = @me->at_zsdt0082-nro_sol
       AND vbeln   = @me->at_zsdt0082-vbeln
       AND posnr   = @me->at_zsdt0082-posnr
       AND seq     = '001'.

    IF sy-subrc <> 0.
      lv_mesg =  'Registro da solicitado não encontrado para atualização!'.
      me->set_error( lv_mesg ).
    ENDIF.

    IF _qte_sol_atu <= 0.

      lv_mesg = zcl_solicitacao_saida_insumos=>cancelar(
                         iv_nro_sol              = me->at_zsdt0082-nro_sol
                         iv_vbeln                = me->at_zsdt0082-vbeln
                         iv_posnr                = me->at_zsdt0082-posnr
                         iv_nao_validar_bloqueio = abap_true ).   "*-US192801-06.10.2025-#192801-JT
*
*      UPDATE zsdt0082 SET status  = '3'
*                    WHERE nro_sol = me->at_zsdt0082-nro_sol
*                      AND seq     = me->at_zsdt0082-seq
*                      AND vbeln   = me->at_zsdt0082-vbeln
*                      AND posnr   = me->at_zsdt0082-posnr.

      IF lv_mesg IS NOT INITIAL.
        me->set_error( lv_mesg ).
      ENDIF.
    ENDIF.
*-US191034-17.09.2025-#191034-JT-fim

*-----------------------------------------------
*---desbloquear solicitacao
*-----------------------------------------------
    UPDATE zsdt0082 SET bloqueio = abap_false
                  WHERE nro_sol  = me->at_zsdt0082-nro_sol
                    AND vbeln    = me->at_zsdt0082-vbeln
                    AND posnr    = me->at_zsdt0082-posnr
                    AND seq      = me->at_zsdt0082-seq.

    IF sy-subrc <> 0.
      lv_mesg = 'Erro ao Desbloquear Registro!'.
      me->set_error( lv_mesg ).
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_error.

    DATA: l_mesg1 TYPE syst_msgv,
          l_mesg2 TYPE syst_msgv,
          l_mesg3 TYPE syst_msgv,
          l_mesg4 TYPE syst_msgv.

    me->set_trata_string( EXPORTING i_mesg  = i_mesg IMPORTING e_msgv1 = l_mesg1 e_msgv2 = l_mesg2 e_msgv3 = l_mesg3 e_msgv4 = l_mesg4 ).

    RAISE EXCEPTION TYPE zcx_error
      EXPORTING
        textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                          msgno = zcx_error=>zcx_erro_geral-msgno
                          attr1 = CONV #( l_mesg1 )
                          attr2 = CONV #( l_mesg2 )
                          attr3 = CONV #( l_mesg3 )
                          attr4 = CONV #( l_mesg4 ) )
        msgid  = zcx_error=>zcx_erro_geral-msgid
        msgno  = zcx_error=>zcx_erro_geral-msgno
        msgty  = 'E'
        msgv1  = l_mesg1
        msgv2  = l_mesg2
        msgv3  = l_mesg3
        msgv4  = l_mesg4.

  ENDMETHOD.


  METHOD set_executar_distribuicao.

    DATA: lv_mesg       TYPE string,
          lc_manutencao TYPE zde_manutencao.

    r_distribuicao_insumos = me.

    me->at_zsds093     = i_zsds093.
    me->at_t_zsds094   = i_zsds094.
    me->at_reprocessar = i_reprocessar.

*-- obter zsdt0082
    me->at_zsdt0082    = me->get_zsdt0082( i_nro_sol = i_zsds093-nro_sol i_seq = i_zsds093-seq i_vbeln = i_zsds093-vbeln i_posnr = i_zsds093-posnr ).

*-- somente valida informacoes
    IF i_somente_validar = abap_true.
      TRY.
          me->set_validar( i_zsds093 = i_zsds093 i_zsds094 = i_zsds094 ).
          IF i_somente_montar_eventos = abap_false.
            RETURN.
          ENDIF.
        CATCH zcx_error INTO DATA(ex_error).
          MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                          ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
          me->set_error( lv_mesg ).
      ENDTRY.
    ENDIF.

*-- Tratar forma de processamento (Processar / Reprocessar / Venda especial)
    TRY.
        CHECK me->set_tratar_processamento( ) = abap_true.
      CATCH zcx_error INTO ex_error.
        MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                        ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
        me->set_error( lv_mesg ).
    ENDTRY.

    CHECK i_somente_montar_eventos = abap_false.

*-------------------------------------
*-- processar
*-------------------------------------
    me->set_limpar_mensagem( ).  "*-CS2025000249-08.09.2025-#189853-JT-inicio

    LOOP AT me->at_t_zsdt0414   INTO DATA(_zsdt0414).

      LOOP AT me->at_t_zsdt0415 INTO DATA(_zsdt0415) WHERE ch_referencia = _zsdt0414-ch_referencia GROUP BY ( item_distrib = _zsdt0415-item_distrib
                                                                                                              ordem_etapa  = _zsdt0415-ordem_etapa )
                                                                                                   ASCENDING.

        CHECK _zsdt0415-status = 'P' OR _zsdt0415-status = abap_off.

        me->at_zsdt0082 = me->get_zsdt0082( i_nro_sol = _zsdt0414-nro_sol i_seq = _zsdt0414-seq i_vbeln = _zsdt0414-vbeln i_posnr = _zsdt0414-posnr ).

        CASE _zsdt0415-etapa.

*-------- Boqueio Solicitacao Original
          WHEN 'BLOQ'.
            TRY.
                me->set_bloqueio_solicitacao( ).
                me->set_modificar_status( i_status = c_concluido i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa ).
              CATCH zcx_error INTO ex_error.
                MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                                ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
                me->set_modificar_status( i_status = c_pendente  i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa i_mensagem = lv_mesg ).
                RETURN.
            ENDTRY.

*-------- Transacao ZSDT0081
          WHEN 'LIBE'.
            TRY.
                me->set_liberar_embarque( EXPORTING i_zsdt0415 = _zsdt0415 ).
                me->set_modificar_status( i_status = c_concluido i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa ).
              CATCH zcx_error INTO ex_error.
                MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                                ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
                me->set_modificar_status( i_status = c_pendente  i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa i_mensagem = lv_mesg ).
                RETURN.
            ENDTRY.

*-------- Transacao ZSDT0081
          WHEN 'QUAN'.
            TRY.
                me->set_qtde_solicitada_zsdt0082( EXPORTING i_zsdt0415 = _zsdt0415 ).
                me->set_modificar_status( i_status = c_concluido i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa ).
              CATCH zcx_error INTO ex_error.
                MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                                ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
                me->set_modificar_status( i_status = c_pendente  i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa i_mensagem = lv_mesg ).
                RETURN.
            ENDTRY.

*-------- Transacao ZSDT0087
          WHEN 'OVEN'.
            TRY.
                me->set_criar_ordem_venda( EXPORTING i_zsdt0414 = _zsdt0414 i_zsdt0415 = _zsdt0415 ).
                me->set_modificar_status( i_status = c_concluido i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa ).
              CATCH zcx_error INTO ex_error.
                MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                                ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
                me->set_modificar_status( i_status = c_pendente  i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa i_mensagem = lv_mesg ).
                RETURN.
            ENDTRY.

*-------- Transacao ZSDT0100
          WHEN 'APRO'.
            TRY.
                me->set_aprovar_embarque( EXPORTING i_zsdt0415 = _zsdt0415 ).
                me->set_modificar_status( i_status = c_concluido i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa ).
              CATCH zcx_error INTO ex_error.
                MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                                ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
                me->set_modificar_status( i_status = c_pendente  i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa i_mensagem = lv_mesg ).
                RETURN.
            ENDTRY.

*-------- Transacao ZSDT0079
          WHEN 'SOLI'.
            TRY.
                me->set_criar_solicitacao( EXPORTING i_zsdt0414 = _zsdt0414 i_zsdt0415 = _zsdt0415 ).
                me->set_modificar_status( i_status = c_concluido i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa ).
              CATCH zcx_error INTO ex_error.
                MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                                ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
                me->set_modificar_status( i_status = c_pendente  i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa i_mensagem = lv_mesg ).
                RETURN.
            ENDTRY.

*-------- Desbloqueio Solicitacao Original
          WHEN 'DLOQ'.
            TRY.
                me->set_desboqueio_solicitacao( i_zsdt0415 = _zsdt0415 ).
                me->set_modificar_status( i_status = c_concluido i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa ).
              CATCH zcx_error INTO ex_error.
                MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                                ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
                me->set_modificar_status( i_status = c_pendente  i_item_distrib = _zsdt0415-item_distrib i_etapa = _zsdt0415-etapa i_mensagem = lv_mesg ).
                RETURN.
            ENDTRY.

        ENDCASE.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_integrar_safra_control.

    DATA: lo_order         TYPE REF TO zcl_int_ob_safra_crt_order,
          lo_order_itens   TYPE REF TO zcl_int_ob_safra_crt_ord_itens,
          ls_order         TYPE zde_safra_control_ordem,
          ls_order_itens   TYPE zde_safra_control_ordem_itens,
          ls_retorno       TYPE zintegracao,
          lv_external_id   TYPE string,
          lv_wgbez         TYPE t023t-wgbez,
          lv_peso          TYPE i,
          lv_matnr_out     TYPE mara-matnr,
          lv_kunnr         TYPE kunnr,
          lv_lifnr         TYPE lifnr,
          lv_vkbur         TYPE lifnr,
          lv_denom         TYPE dec10,
          lv_fator         TYPE dec10,
          lv_prioridade    TYPE string,
          lc_integra_safra TYPE REF TO zcl_int_ob_safra_crt_contact.

    FREE: e_msg_erro.

    CREATE OBJECT lc_integra_safra.

    SELECT SINGLE *
      FROM zsdt0082
      INTO @DATA(ls_0082)
     WHERE nro_sol = @i_dados-nro_sol
       AND seq     = @i_dados-seq
       AND vbeln   = @i_dados-vbeln
       AND posnr   = @i_dados-posnr.

    IF sy-subrc IS NOT INITIAL.
*     e_msg_erro = 'Nenhum registro encontrado'.
      RETURN.
    ENDIF.

*** Monta header - Inicio
    ls_order-externalid      = ls_0082-nro_sol && ls_0082-seq.
    ls_order-externalorderid = ls_0082-nro_sol && ls_0082-seq.
    ls_order-type            = 'normal'.

    SELECT inco1
      FROM vbkd
      INTO @DATA(lv_inco1)
        UP TO 1 ROWS
     WHERE vbeln = @ls_0082-vbeln.
    ENDSELECT.

    IF sy-subrc IS INITIAL.
      ls_order-freighttype = 'CIF'. "lv_inco1.
    ENDIF.

    ls_order-enabled = 'true'.

    SELECT doc_simulacao
      FROM zsdt0090
      INTO @DATA(lv_doc_simul)
        UP TO 1 ROWS
     WHERE vbeln = @ls_0082-vbeln.
    ENDSELECT.

    IF sy-subrc IS INITIAL  .
      SELECT SINGLE safra,tpcult,cultura
        FROM zsdt0040
        INTO @DATA(ls_0040)
       WHERE doc_simulacao = @lv_doc_simul.
    ELSE.
      SELECT doc_simulacao
        FROM zsdt0041
        INTO lv_doc_simul
          UP TO 1 ROWS
       WHERE vbeln = ls_0082-vbeln.
      ENDSELECT.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE safra tpcult
          FROM zsdt0040
          INTO ls_0040
         WHERE doc_simulacao = lv_doc_simul.
      ENDIF.
    ENDIF.

    ls_order-period-externalid = ls_0040-safra.
    ls_order-duedate = sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2).

    SELECT SINGLE kunnr
      FROM zsdt0132
      INTO @lv_kunnr
     WHERE nr_rot = @ls_0082-nr_rot.

    IF sy-subrc IS INITIAL AND lv_kunnr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_kunnr
        IMPORTING
          output = ls_order-customer-externalid.
    ELSE.
      SELECT SINGLE kunnr
        INTO lv_kunnr
        FROM vbak
       WHERE vbeln = ls_0082-vbeln.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_kunnr
        IMPORTING
          output = ls_order-customer-externalid.
    ENDIF.

    ls_order-customer-externalid  = lc_integra_safra->get_external_id( i_parceiro = lv_kunnr i_tipo_parceiro = 'C' ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_0082-vkbur
      IMPORTING
        output = lv_vkbur. "ls_order-branchoffice-externalid.

*   ls_order-branchoffice-externalid = ls_0082-vkbur.
    ls_order-branchoffice-externalid = lc_integra_safra->get_external_id( i_parceiro = lv_vkbur i_tipo_parceiro = 'F' ).

    SELECT SINGLE lifnr
      FROM zsdt0132
      INTO @lv_lifnr
     WHERE nr_rot = @ls_0082-nr_rot_pc.

    IF sy-subrc IS INITIAL AND lv_lifnr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_lifnr
        IMPORTING
          output = ls_order-withdrawalplace-externalid.
    ELSE.
      ls_order-withdrawalplace-externalid = ls_order-branchoffice-externalid.
    ENDIF.

    ls_order-withdrawalplace-externalid = lc_integra_safra->get_external_id( i_parceiro = lv_lifnr i_tipo_parceiro = 'F' i_nr_rot = ls_0082-nr_rot_pc ).

    if ls_0082-prioridade is NOT INITIAL.
      ls_order-specialattention = 'true'.
    else.
      ls_order-specialattention = 'false'.
    endif.


    ls_order-customfields     = '{"utilization": " "}'.
*** Monta header - Fim

    CONDENSE ls_order-customer-externalid NO-GAPS.
    CONDENSE ls_order-branchoffice-externalid NO-GAPS.
    CONDENSE ls_order-withdrawalplace-externalid NO-GAPS.

*** Monta itens - Inicio

    ls_order_itens-externalid              = ls_order-externalid.
    ls_order_itens-externalorderid         = ls_order-externalid.
    ls_order_itens-order-externalid        = ls_order-externalid.
    ls_order_itens-itemcode                = '1'.
    ls_order_itens-freighttype             = 'CIF'.
    ls_order_itens-quantity                = ls_0082-qte_lib. "ls_0082-qte_lib.
    ls_order_itens-enabled                 = 'true'.
    ls_order_itens-cancelad                = 'false'.
    ls_order_itens-schedulingdate          = ls_0082-dt_entrega(4) && '-' && ls_0082-dt_entrega+4(2) && '-' && ls_0082-dt_entrega+6(2).
    ls_order_itens-period-externalid       = ls_0040-safra.
    ls_order_itens-customer-externalid     = ls_order-customer-externalid.
    ls_order_itens-branchoffice-externalid = ls_order-branchoffice-externalid.

    SELECT SINGLE matnr
      FROM vbap
      INTO @DATA(lv_matnr)
     WHERE vbeln = @ls_0082-vbeln
       AND posnr = @ls_0082-posnr.

    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_matnr
        IMPORTING
          output = lv_matnr_out.

      SELECT SINGLE *
        INTO me->at_mara
        FROM mara
       WHERE matnr = lv_matnr.

      SELECT SINGLE *
        INTO me->at_makt
        FROM makt
       WHERE matnr = lv_matnr
         AND spras = sy-langu.

      SELECT SINGLE *
        INTO me->at_marc
        FROM marc
       WHERE matnr = lv_matnr.

      SELECT SINGLE wgbez
        INTO lv_wgbez
        FROM t023t
       WHERE spras = sy-langu
         AND matkl = me->at_mara-matkl.

      CALL FUNCTION 'CONVERSION_FACTOR_GET'
        EXPORTING
          no_type_check        = abap_true
          unit_in              = me->at_mara-gewei
          unit_out             = 'G'
        IMPORTING
          denominator          = lv_denom
          numerator            = lv_fator
        EXCEPTIONS
          conversion_not_found = 1
          overflow             = 2
          type_invalid         = 3
          units_missing        = 4
          unit_in_not_found    = 5
          unit_out_not_found   = 6
          OTHERS               = 7.

      IF sy-subrc <> 0.
        lv_fator = 1.
      ENDIF.

      lv_peso                                      = me->at_mara-brgew.
      ls_order_itens-product-externalid            = lv_matnr_out.
      ls_order_itens-product-name                  = me->at_makt-maktx.
      ls_order_itens-product-weight                = lv_peso.
      ls_order_itens-product-unit                  = me->at_mara-meins.
      ls_order_itens-product-conversionfactor      = lv_fator.
      ls_order_itens-product-multiple              = 1.
      ls_order_itens-product-ncm                   = me->at_marc-steuc.
      ls_order_itens-product-group-externalid      = me->at_mara-matkl.
      ls_order_itens-product-group-name            = lv_wgbez.
      ls_order_itens-product-group-enabled         = 'true'.
      ls_order_itens-product-variety-externalid    = lv_wgbez. "me->at_mara-wrkst.
      ls_order_itens-product-variety-name          = lv_wgbez. "me->at_mara-wrkst.
      ls_order_itens-product-supplier-externalid   = ls_order-branchoffice-externalid."me->at_mara-matnr.

      CONDENSE ls_order_itens-product-externalid NO-GAPS.
    ENDIF.

    ls_order_itens-withdrawalplace-externalid      = ls_order-withdrawalplace-externalid.
*   ls_order_itens-shippingaddress-externalid      = ls_order-customer-externalid. "ls_0082-nr_rot.
    ls_order_itens-shippingaddress-externalid      = lc_integra_safra->get_external_id( i_parceiro = lv_kunnr i_tipo_parceiro = 'C' i_nr_rot = ls_0082-nr_rot ).
    ls_order_itens-cultivation-externalid          = '1'. "ls_0040-cultura. "ls_0040-tpcult.

    if ls_0082-prioridade is NOT INITIAL.
      ls_order_itens-specialattention                = 'true'.
    else.
      ls_order_itens-specialattention                = 'false'.
    endif.

    IF ls_0082-prioridade = abap_true.
      lv_prioridade = 'true'.
    ELSE.
      lv_prioridade = 'false'.
    ENDIF.

*   ls_order_itens-customfields = '"{\"dataHoraPriorizacao\":"' && sy-datum && '",\"prioridade\":"' && lv_prioridade && '"}"'.
    ls_order_itens-customfields = ls_order-customfields.
*** Monta itens - Fim

    IF ls_0082-status = '4'. "Cancelada
      ls_order-enabled        = 'false'.
      ls_order_itens-enabled  = 'false'.
      ls_order_itens-cancelad = 'true'.
    ENDIF.

*--------------------------------------------------
*---- integrar orders ----------------------------
*--------------------------------------------------
    TRY .
        CREATE OBJECT lo_order.
        CALL METHOD lo_order->set_metodo_http
          EXPORTING
            i_metodo = 'POST'.

        CALL METHOD lo_order->zif_integracao_outbound~execute_request
          EXPORTING
            i_info_request           = ls_order
          IMPORTING
            e_id_integracao          = DATA(lv_id_integracao)
            e_integracao             = ls_retorno
          RECEIVING
            r_if_integracao_outbound = DATA(lo_outbound).

      CATCH zcx_integracao INTO DATA(lo_integracao).
        IF ls_retorno-nm_code <> '0409'.
          e_msg_erro = ls_retorno-ds_data_retorno. "lo_integracao->get_text( ).
          RETURN.
        ENDIF.
      CATCH zcx_error INTO DATA(lo_error).
        IF ls_retorno-nm_code <> '0409'.
          e_msg_erro = ls_retorno-ds_data_retorno. "lo_error->get_text( ).
          RETURN.
        ENDIF.
    ENDTRY.

    IF ls_retorno-nm_code = '0409'.
      TRY .
          CREATE OBJECT lo_order.
          CALL METHOD lo_order->set_metodo_http
            EXPORTING
              i_metodo = 'PUT'.

          CALL METHOD lo_order->zif_integracao_outbound~execute_request
            EXPORTING
              i_info_request           = ls_order
            IMPORTING
              e_id_integracao          = lv_id_integracao
              e_integracao             = ls_retorno
            RECEIVING
              r_if_integracao_outbound = lo_outbound.

        CATCH zcx_integracao INTO lo_integracao.
          e_msg_erro = ls_retorno-ds_data_retorno. "lo_integracao->get_text( ).
          RETURN.
        CATCH zcx_error INTO lo_error.
          e_msg_erro = ls_retorno-ds_data_retorno. "lo_error->get_text( ).
          RETURN.
      ENDTRY.
    ENDIF.

*--------------------------------------------------
*---- integrar orders itens ----------------------------
*--------------------------------------------------
    TRY .
        CREATE OBJECT lo_order_itens.
        CALL METHOD lo_order_itens->set_metodo_http
          EXPORTING
            i_metodo = 'POST'.

        CALL METHOD lo_order_itens->zif_integracao_outbound~execute_request
          EXPORTING
            i_info_request           = ls_order_itens
          IMPORTING
            e_id_integracao          = lv_id_integracao
            e_integracao             = ls_retorno
          RECEIVING
            r_if_integracao_outbound = lo_outbound.

      CATCH zcx_integracao INTO lo_integracao.
        IF ls_retorno-nm_code <> '0409'.
          e_msg_erro = ls_retorno-ds_data_retorno. "lo_integracao->get_text( ).
          RETURN.
        ENDIF.
      CATCH zcx_error INTO lo_error.
        IF ls_retorno-nm_code <> '0409'.
          e_msg_erro = ls_retorno-ds_data_retorno. "lo_error->get_text( ).
          RETURN.
        ENDIF.
    ENDTRY.

    IF ls_retorno-nm_code = '0409'.
      TRY .
          CREATE OBJECT lo_order_itens.
          CALL METHOD lo_order_itens->set_metodo_http
            EXPORTING
              i_metodo = 'PUT'.

          CALL METHOD lo_order_itens->zif_integracao_outbound~execute_request
            EXPORTING
              i_info_request           = ls_order_itens
            IMPORTING
              e_id_integracao          = lv_id_integracao
              e_integracao             = ls_retorno
            RECEIVING
              r_if_integracao_outbound = lo_outbound.

        CATCH zcx_integracao INTO lo_integracao.
          e_msg_erro = ls_retorno-ds_data_retorno. "lo_integracao->get_text( ).
        CATCH zcx_error INTO lo_error.
          e_msg_erro = ls_retorno-ds_data_retorno. "lo_error->get_text( ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD set_liberar_embarque.

    DATA: t_0082_lib        TYPE TABLE OF zsdt0082,
          w_0082_lib        TYPE zsdt0082,
          w_0082            TYPE zsdt0082,
          tl_0082           TYPE TABLE OF zsdt0082,
          tl_vbep           TYPE TABLE OF vbep,
          w_vbep            TYPE vbep,
          wl_0082           TYPE zsdt0082,
          w_schedule_lines  TYPE bapischdl,
          w_schedule_linesx TYPE bapischdlx,
          w_return          TYPE bapiret2,
          lv_origem_estoque TYPE zde_origem_estoque,
          lv_seq            TYPE zsdt0082-seq,
          lv_seq_lib        TYPE zsdt0082-seq_lib,
          lv_valida         TYPE char01,
          lv_return         TYPE string,
          lv_mesg           TYPE string,
          lc_dados          TYPE zmme_dados_safra.

    FREE: me->at_return,
          me->at_order_item_in,
          me->at_order_item_inx,
          me->at_schedule_lines,
          me->at_schedule_linesx,
          me->at_order_header_inx,
          lv_valida.

    w_0082 = me->at_zsdt0082.

    SELECT *
      FROM zsdt0082
      INTO TABLE t_0082_lib
     WHERE nro_sol  = w_0082-nro_sol
       AND vbeln    = w_0082-vbeln
       AND posnr    = w_0082-posnr.
*      AND status  <> 3.

    SORT t_0082_lib BY seq DESCENDING.

    READ TABLE t_0082_lib INTO w_0082_lib INDEX 1.
    IF sy-subrc = 0.
      lv_seq     = w_0082_lib-seq.
      lv_seq_lib = w_0082_lib-seq_lib.
    ENDIF.

    IF lv_seq = 0 .
      lv_seq     = w_0082-seq.     " Pega a sequencia da solicitação.
      lv_seq_lib = w_0082-seq_lib. " Pega a sequencia da solicitação.
    ENDIF.

    IF     i_zsdt0415-ebeln IS NOT INITIAL AND i_zsdt0415-ebelp IS NOT INITIAL.
      lv_origem_estoque     = '2'.
    ELSEIF i_zsdt0415-lifnr IS NOT INITIAL.
      lv_origem_estoque     = '3'.
    ELSE.
      lv_origem_estoque     = '1'.
    ENDIF.

    CLEAR w_0082-dt_sol.

    w_0082-seq              = lv_seq     + 1.
    w_0082-seq_lib          = lv_seq_lib + 1.
    w_0082-dt_liber         = sy-datum.
    w_0082-dt_entrega       = i_zsdt0415-dt_entrega.
    w_0082-nr_rot_pc        = i_zsdt0415-nr_rot_pc.
    w_0082-usuario_lib      = sy-uname.
    w_0082-qte_sol          = 0.
    w_0082-qte_lib          = i_zsdt0415-qte_sol.
    w_0082-status           = 2.
    w_0082-bloqueio         = abap_false.
    w_0082-ch_referencia    = abap_false.
    w_0082-ebeln            = i_zsdt0415-ebeln.
    w_0082-ebelp            = i_zsdt0415-ebelp.
    w_0082-lifnr_arm        = i_zsdt0415-lifnr.
    w_0082-charg            = i_zsdt0415-charg.
    w_0082-marca            = i_zsdt0415-marca.
    w_0082-origem_estoque   = lv_origem_estoque.
    w_0082-carga_automatica = i_zsdt0415-carga_auto.
    w_0082-flexibilidade    = i_zsdt0415-flexibilidade.
    w_0082-prioridade       = i_zsdt0415-prioridade.
    w_0082-transf_no_fornecedor = i_zsdt0415-transf_no_fornecedor.
    w_0082-processando      = abap_false.

*---------------------------------------------------
*-- mofifica OV
*---------------------------------------------------
    SELECT *
      FROM vbep
      INTO TABLE tl_vbep
     WHERE vbeln EQ w_0082-vbeln
       AND posnr EQ w_0082-posnr
       AND lifsp NE '12'.

    at_order_header_inx-updateflag = 'U'.

    LOOP AT tl_vbep INTO w_vbep.
      IF w_vbep-lifsp = '10'.
        w_schedule_lines-req_dlv_bl = abap_off.
        lv_valida = abap_true.
      ENDIF.

      w_schedule_lines-itm_number   = w_0082-posnr.
      w_schedule_lines-sched_line   = w_vbep-etenr.

      w_schedule_linesx-req_dlv_bl  = 'X'.
      w_schedule_linesx-itm_number  = w_0082-posnr.
      w_schedule_linesx-sched_line  = w_vbep-etenr.
      w_schedule_linesx-updateflag  = 'U'.

      APPEND: w_schedule_lines     TO me->at_schedule_lines,
              w_schedule_linesx    TO me->at_schedule_linesx.
    ENDLOOP.

    IF lv_valida = abap_false.
      MODIFY zsdt0082 FROM w_0082.
      COMMIT WORK AND WAIT.

      IF i_zsdt0415-carga_auto = abap_true.
        TRY.
            lc_dados-nro_sol = w_0082-nro_sol.
            lc_dados-seq     = w_0082-seq.
            lc_dados-vbeln   = w_0082-vbeln.
            lc_dados-posnr   = w_0082-posnr.
            me->set_integrar_safra_control( EXPORTING i_dados = lc_dados IMPORTING e_msg_erro = lv_mesg ).

            IF lv_mesg IS NOT INITIAL.
              DELETE zsdt0082 FROM w_0082.
              COMMIT WORK AND WAIT.
              me->set_error( lv_mesg ).
            ENDIF.

          CATCH zcx_error INTO DATA(ex_error).
            DELETE zsdt0082 FROM w_0082.
            COMMIT WORK AND WAIT.
            MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                            ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
            me->set_error( lv_mesg ).
        ENDTRY.
      ENDIF.

      UPDATE zsdt0415 SET nro_sol_ref   = w_0082-nro_sol
                          seq_ref       = w_0082-seq
                          vbeln_ref     = w_0082-vbeln
                          posnr_ref     = w_0082-posnr
                    WHERE ch_referencia = i_zsdt0415-ch_referencia
                      AND item_distrib  = i_zsdt0415-item_distrib
                      AND etapa         = i_zsdt0415-etapa.

      COMMIT WORK AND WAIT.
    ELSE.
*---------------------------------------------------
*---- bapi OV
*---------------------------------------------------
      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = w_0082-vbeln
          order_header_inx = me->at_order_header_inx
        TABLES
          return           = me->at_return
          order_item_in    = me->at_order_item_in
          order_item_inx   = me->at_order_item_inx
          schedule_lines   = me->at_schedule_lines
          schedule_linesx  = me->at_schedule_linesx.

      lv_return = me->set_transform_tab_return( i_bapiret = me->at_return ).

      READ TABLE me->at_return INTO w_return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        lv_mesg = 'Ocorreu erro ajuste na OV: ' &&  w_0082-vbeln && '|' && lv_return.
        me->set_error( lv_mesg ).
      ELSE.
        MODIFY zsdt0082 FROM w_0082.
        COMMIT WORK AND WAIT.

        IF i_zsdt0415-carga_auto = abap_true.
          TRY.
              lc_dados-nro_sol = w_0082-nro_sol.
              lc_dados-seq     = w_0082-seq.
              lc_dados-vbeln   = w_0082-vbeln.
              lc_dados-posnr   = w_0082-posnr.
              me->set_integrar_safra_control( EXPORTING i_dados = lc_dados IMPORTING e_msg_erro = lv_mesg ).

              IF lv_mesg IS NOT INITIAL.
                DELETE zsdt0082 FROM w_0082.
                COMMIT WORK AND WAIT.
                me->set_error( lv_mesg ).
              ENDIF.

            CATCH zcx_error INTO ex_error.
              DELETE zsdt0082 FROM w_0082.
              COMMIT WORK AND WAIT.
              MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                              ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
              me->set_error( lv_mesg ).
          ENDTRY.
        ENDIF.

        UPDATE zsdt0415 SET nro_sol_ref   = w_0082-nro_sol
                            seq_ref       = w_0082-seq
                            vbeln_ref     = w_0082-vbeln
                            posnr_ref     = w_0082-posnr
                      WHERE ch_referencia = i_zsdt0415-ch_referencia
                        AND item_distrib  = i_zsdt0415-item_distrib
                        AND etapa         = i_zsdt0415-etapa.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD set_modificar_status.

    READ TABLE me->at_t_zsdt0415 INTO DATA(_zsdt0415) WITH KEY item_distrib = i_item_distrib
                                                               etapa        = i_etapa.

    _zsdt0415-status                = i_status.
    _zsdt0415-mensagem              = i_mensagem.

    IF i_status = c_concluido.
      CLEAR _zsdt0415-mensagem.
    ENDIF.

    MODIFY me->at_t_zsdt0415     FROM _zsdt0415 INDEX sy-tabix.

    UPDATE zsdt0415               SET status        = _zsdt0415-status
                                      mensagem      = _zsdt0415-mensagem
                                WHERE ch_referencia = _zsdt0415-ch_referencia
                                  AND item_distrib  = _zsdt0415-item_distrib
                                  AND etapa         = _zsdt0415-etapa.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_montar_eventos.

    DATA: lv_liberar TYPE char01,
          lv_mesg    TYPE string.

    FREE: me->at_t_zsdt0414,
          me->at_t_zsdt0415,
          me->at_item_distrib.

    me->at_id_agrupar   = get_uuid( ).
    me->at_item_distrib = get_next_item_distrib( ).

    lv_liberar        = abap_true.

    LOOP AT me->at_t_zsds094 INTO DATA(_zsds094).
      IF _zsds094-matnr    <> me->at_zsds093-matnr OR
         _zsds094-werks    <> me->at_zsds093-werks OR
         _zsds094-lgort    <> me->at_zsds093-lgort OR
         _zsds094-lifnr_pc <> me->at_zsds093-lifnr_pc.
        lv_liberar    = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

    LOOP AT me->at_t_zsds094 INTO _zsds094.

      me->at_ordem_etapa  = 0.
      me->at_item_distrib = me->at_item_distrib + 1.

      TRY.
          IF lv_liberar = abap_true.
            me->set_criar_tabela_eventos( i_etapa  = 'BLOQ' i_zsds094 = _zsds094 ).
            me->set_criar_tabela_eventos( i_etapa  = 'LIBE' i_zsds094 = _zsds094 ).
            me->set_criar_tabela_eventos( i_etapa  = 'DLOQ' i_zsds094 = _zsds094 ).
          ELSE.
            me->set_criar_tabela_eventos( i_etapa  = 'BLOQ' i_zsds094 = _zsds094 ).
            me->set_criar_tabela_eventos( i_etapa  = 'QUAN' i_zsds094 = _zsds094 ).
            me->set_criar_tabela_eventos( i_etapa  = 'OVEN' i_zsds094 = _zsds094 ).
            me->set_criar_tabela_eventos( i_etapa  = 'APRO' i_zsds094 = _zsds094 ).
            me->set_criar_tabela_eventos( i_etapa  = 'SOLI' i_zsds094 = _zsds094 ).
            me->set_criar_tabela_eventos( i_etapa  = 'DLOQ' i_zsds094 = _zsds094 ).
          ENDIF.
        CATCH zcx_error INTO DATA(ex_error).
          MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                          ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
          me->set_error( lv_mesg ).
      ENDTRY.

    ENDLOOP.

    MODIFY zsdt0414 FROM TABLE me->at_t_zsdt0414.
    MODIFY zsdt0415 FROM TABLE me->at_t_zsdt0415.

    UPDATE zsdt0082 SET ch_referencia = me->at_ch_referencia
                  WHERE nro_sol       = me->at_zsdt0082-nro_sol
                    AND vbeln         = me->at_zsdt0082-vbeln
                    AND posnr         = me->at_zsdt0082-posnr
                    AND seq           = me->at_zsdt0082-seq.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_qtde_solicitada_zsdt0082.

    DATA: w_zsdt0150 TYPE zsdt0150,
          lv_qte_sol TYPE zsdt0415-kwmeng,
          lv_mesg    TYPE string,
          lv_status  TYPE zsdt0082-status,
          lc_dados   TYPE zmme_dados_safra.

*-----------------------------------------------
*-- Recupera registro da quantidade total da solicitação
*-----------------------------------------------
    SELECT SINGLE *
      FROM zsdt0082
      INTO @DATA(_zsdt0082)
     WHERE nro_sol = @me->at_zsdt0082-nro_sol
       AND vbeln   = @me->at_zsdt0082-vbeln
       AND posnr   = @me->at_zsdt0082-posnr
       AND seq     = '001'.

    IF sy-subrc <> 0.
      lv_mesg =  'Registro da solicitado não encontrado para atualização!'.
      me->set_error( lv_mesg ).
    ENDIF.

    SELECT SUM(   qte_lib )
      INTO @DATA(_qte_lib)
      FROM zsdt0082
     WHERE nro_sol  = @me->at_zsdt0082-nro_sol
       AND vbeln    = @me->at_zsdt0082-vbeln
       AND posnr    = @me->at_zsdt0082-posnr
       AND seq     <> '001'
       AND status  <> '4'
       AND qte_sol  = 0.

*   IF ( i_zsdt0415-kwmeng < _qte_lib ).
*     lv_mesg = 'Quantidade:' && i_zsdt0415-kwmeng && ' não pode ser inferior a quantidade já distribuída:' && _qte_lib.
*     me->set_error( lv_mesg ).
*   ENDIF.

    IF ( i_zsdt0415-kwmeng > _zsdt0082-qte_sol ).
      lv_mesg = 'Quantidade: ' && i_zsdt0415-kwmeng && 'deve ser menor que a quantidade da solicitação:' &&  _zsdt0082-qte_sol.
      me->set_error( lv_mesg ).
    ENDIF.

    lv_qte_sol                    = me->at_zsdt0082-qte_sol - i_zsdt0415-kwmeng.
*   lv_status                     = COND #( WHEN lv_qte_sol <= 0 THEN '3' ELSE me->at_zsdt0082-status ). "*-US191034-17.09.2025-#191034-JT
    lv_status                     = me->at_zsdt0082-status.                                              "*-US191034-17.09.2025-#191034-JT

*-------------------------------
*-- atualiza quantidade sol
*-------------------------------
    UPDATE zsdt0082 SET qte_sol   = lv_qte_sol
*                       status    = lv_status
                        nr_rot_pc = i_zsdt0415-nr_rot_pc
                  WHERE nro_sol   = me->at_zsdt0082-nro_sol
                    AND seq       = me->at_zsdt0082-seq
                    AND vbeln     = me->at_zsdt0082-vbeln
                    AND posnr     = me->at_zsdt0082-posnr.

    COMMIT WORK AND WAIT.

*-------------------------------
*-- historico
*-------------------------------
    w_zsdt0150-direcao          = 'Solicitado'.
    w_zsdt0150-nro_sol          = me->at_zsdt0082-nro_sol.
    w_zsdt0150-seq              = '001'.
    w_zsdt0150-vbeln            = me->at_zsdt0082-vbeln.
    w_zsdt0150-posnr            = me->at_zsdt0082-posnr.
    w_zsdt0150-dt_registro      = sy-datum.
    w_zsdt0150-hr_registro      = sy-uzeit.
    w_zsdt0150-us_registro      = sy-uname.
    w_zsdt0150-qtde_old         = me->at_zsdt0082-qte_sol.
    w_zsdt0150-qtde_new         = lv_qte_sol.
    MODIFY zsdt0150          FROM w_zsdt0150.

    UPDATE zsdt0415 SET nro_sol_ref   = me->at_zsdt0082-nro_sol
                        seq_ref       = me->at_zsdt0082-seq
                        vbeln_ref     = me->at_zsdt0082-vbeln
                        posnr_ref     = me->at_zsdt0082-posnr
                  WHERE ch_referencia = i_zsdt0415-ch_referencia
                    AND item_distrib  = i_zsdt0415-item_distrib
                    AND etapa         = i_zsdt0415-etapa.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_status_processamento.

    r_distribuicao_insumos = me.

    UPDATE zsdt0082 SET processando   = i_status
                  WHERE nro_sol       = i_nro_sol
                    AND seq           = i_seq
                    AND vbeln         = i_vbeln
                    AND posnr         = i_posnr.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_transform_tab_return.

    DATA: l_mesg TYPE char255.

    FREE: r_mensagem.

    LOOP AT i_bapiret INTO DATA(_bapiret) WHERE type = 'E'.
      IF _bapiret-message_v1 IS INITIAL AND _bapiret-message_v2 IS INITIAL AND
         _bapiret-message_v3 IS INITIAL AND _bapiret-message_v4 IS INITIAL.
        r_mensagem  = r_mensagem && _bapiret-message && '|'.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = _bapiret-id
          lang      = sy-langu
          no        = _bapiret-number
          v1        = _bapiret-message_v1
          v2        = _bapiret-message_v2
          v3        = _bapiret-message_v3
          v4        = _bapiret-message_v4
        IMPORTING
          msg       = l_mesg
        EXCEPTIONS
          not_found = 01
          OTHERS    = 02.

      CHECK l_mesg IS NOT INITIAL.

      r_mensagem = r_mensagem && l_mesg && '|'.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_tratar_processamento.

    DATA: lv_mesg       TYPE string.

    r_prossegue = abap_true.

    IF me->at_reprocessar = abap_false.
      me->at_ch_referencia = COND #( WHEN me->at_zsdt0082-ch_referencia IS INITIAL THEN me->get_ch_referencia( )
                                                                                   ELSE me->at_zsdt0082-ch_referencia ).
      TRY.
          me->set_montar_eventos( ).
        CATCH zcx_error INTO DATA(ex_error).
          MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                          ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
          me->set_error( lv_mesg ).
      ENDTRY.

      me->set_tratar_venda_especial( ).
    ELSE.
      me->at_ch_referencia = me->at_zsdt0082-ch_referencia.

      me->set_ajustar_dados( ).

      SELECT *
        INTO TABLE me->at_t_zsdt0414
        FROM zsdt0414
       WHERE ch_referencia = me->at_zsdt0082-ch_referencia.

      SELECT *
        INTO TABLE me->at_t_zsdt0415
        FROM zsdt0415
       WHERE ch_referencia = me->at_zsdt0082-ch_referencia
         AND status       <> c_concluido
         AND cancelado     = abap_off.

      me->set_tratar_venda_especial( ).
    ENDIF.

    READ TABLE  me->at_t_zsdt0415 INTO DATA(_zsdt0415) WITH KEY status = c_pend_aprov.
    IF sy-subrc = 0.
      r_prossegue = abap_false.
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_tratar_venda_especial.

    DATA: lv_id_distribuicao TYPE zde_id_distrib,
          lv_nro_sol         TYPE zsdt0082-nro_sol,
          lv_pai_aprovado    TYPE char01,
          w_zsdt0411         TYPE zsdt0411,
          w_zsdt0412         TYPE zsdt0412.

    CHECK me->at_zsds093-venda_esp = abap_true.

*-US190631-12.09.2025-#190631-JT-inicio
*----------------------------------------------------
*---verifica se solicitacoes pais ja foram aprovadas
*----------------------------------------------------
    lv_pai_aprovado = abap_off.
    lv_nro_sol      = me->at_zsdt0082-nro_sol.

    DO 10 TIMES.
      SELECT SINGLE nro_sol_origem
        INTO @DATA(_nro_sol_origem)
        FROM zsdt0082
       WHERE nro_sol = @lv_nro_sol.

      IF sy-subrc <> 0 OR _nro_sol_origem IS INITIAL.
        EXIT.
      ENDIF.

      SELECT SINGLE status
        INTO @DATA(_status)
        FROM zsdt0411
       WHERE nro_sol = @_nro_sol_origem
         AND status  = 'A'.

      IF sy-subrc = 0.
        lv_pai_aprovado = abap_true.
        EXIT.
      ENDIF.

      lv_nro_sol = _nro_sol_origem.
    ENDDO.

    CHECK lv_pai_aprovado = abap_off.
*-US190631-12.09.2025-#190631-JT-fim

    IF me->at_reprocessar = abap_false.
      SELECT MAX( id_distribuicao )
        INTO   lv_id_distribuicao
        FROM zsdt0411
       WHERE nro_sol = me->at_zsdt0082-nro_sol.

      lv_id_distribuicao                  = lv_id_distribuicao + 1.

      w_zsdt0411-nro_sol                  = me->at_zsdt0082-nro_sol.
      w_zsdt0411-id_distribuicao          = lv_id_distribuicao.
      w_zsdt0411-status                   = abap_off.
      w_zsdt0411-user_acao                = sy-uname.
      w_zsdt0411-date_acao                = sy-datum.
      w_zsdt0411-time_acao                = sy-uzeit.
      w_zsdt0411-user_create              = sy-uname.
      w_zsdt0411-date_create              = sy-datum.
      w_zsdt0411-time_create              = sy-uzeit.
      MODIFY zsdt0411                  FROM w_zsdt0411.

      MOVE-CORRESPONDING me->at_zsdt0082 TO w_zsdt0412.
      w_zsdt0412-id_distribuicao          = lv_id_distribuicao.
      MODIFY zsdt0412                  FROM w_zsdt0412.

      LOOP AT me->at_t_zsdt0415        INTO DATA(_zsdt0415).
        _zsdt0415-id_distribuicao         = lv_id_distribuicao.
        _zsdt0415-mensagem                = 'Aguardando Aprovação'.
        _zsdt0415-status                  = c_pend_aprov.
        MODIFY me->at_t_zsdt0415       FROM _zsdt0415 INDEX sy-tabix.
        MODIFY zsdt0415                FROM _zsdt0415.
      ENDLOOP.
    ELSE.
      LOOP AT me->at_t_zsdt0415        INTO _zsdt0415 WHERE status <> c_concluido.
        DATA(lv_tabix) = sy-tabix.

        IF me->get_zsdt0411( i_nro_sol    = _zsdt0415-nro_sol i_id_distribuicao = _zsdt0415-id_distribuicao )-status = c_aprovado.
          _zsdt0415-mensagem              = COND #( WHEN _zsdt0415-status = c_pend_aprov THEN abap_off ELSE _zsdt0415-mensagem ).
          _zsdt0415-status                = COND #( WHEN _zsdt0415-status = c_pend_aprov THEN abap_off ELSE _zsdt0415-status ).
          MODIFY me->at_t_zsdt0415     FROM _zsdt0415 INDEX lv_tabix.
          MODIFY zsdt0415              FROM _zsdt0415.
        ELSE.
          _zsdt0415-status                = c_pend_aprov.
          _zsdt0415-mensagem              = 'Aguardando Aprovação'.
          MODIFY me->at_t_zsdt0415     FROM _zsdt0415 INDEX lv_tabix.
          MODIFY zsdt0415              FROM _zsdt0415.
        ENDIF.
      ENDLOOP.
    ENDIF.

    TRY.
        me->set_bloqueio_solicitacao( ).
      CATCH zcx_error INTO DATA(ex_error).
    ENDTRY.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_trata_string.

    FREE: e_msgv1, e_msgv2, e_msgv3, e_msgv4.

    DATA: l_div1 TYPE i,
          l_div2 TYPE i,
          l_div3 TYPE i,
          l_div4 TYPE i,
          l_mod1 TYPE i,
          l_mod2 TYPE i,
          l_mod3 TYPE i,
          l_mod4 TYPE i,
          l_strl TYPE i,
          l_sub1 TYPE i,
          l_sub2 TYPE i,
          l_sub3 TYPE i,
          l_sub4 TYPE i.

    l_strl = strlen( i_mesg ).
    IF l_strl < 0.
      l_sub1 = 0.
    ELSE.
      l_div1 = l_strl DIV 50.
      l_mod1 = l_strl MOD 50.
      l_sub1 = COND #( WHEN l_div1 > 0 THEN 50 ELSE l_mod1 ).
    ENDIF.

    l_strl = l_strl - 50.
    IF l_strl < 0.
      l_sub2 = 0.
    ELSE.
      l_div2 = l_strl DIV 50.
      l_mod2 = l_strl MOD 50.
      l_sub2 = COND #( WHEN l_div2 > 0 THEN 50 ELSE l_mod2 ).
    ENDIF.

    l_strl = l_strl - 50.
    IF l_strl < 0.
      l_sub3 = 0.
    ELSE.
      l_div3 = l_strl DIV 50.
      l_mod3 = l_strl MOD 50.
      l_sub3 = COND #( WHEN l_div3 > 0 THEN 50 ELSE l_mod3 ).
    ENDIF.

    l_strl = l_strl - 50.
    IF l_strl < 0.
      l_sub4 = 0.
    ELSE.
      l_div4 = l_strl DIV 50.
      l_mod4 = l_strl MOD 50.
      l_sub4 = COND #( WHEN l_div4 > 0 THEN 50 ELSE l_mod4 ).
    ENDIF.

    e_msgv1 = COND #( WHEN l_sub1 > 0 THEN i_mesg+000(l_sub1) ELSE '' ).
    e_msgv2 = COND #( WHEN l_sub2 > 0 THEN i_mesg+050(l_sub2) ELSE '' ).
    e_msgv3 = COND #( WHEN l_sub3 > 0 THEN i_mesg+100(l_sub3) ELSE '' ).
    e_msgv4 = COND #( WHEN l_sub4 > 0 THEN i_mesg+150(l_sub4) ELSE '' ).

  ENDMETHOD.


  METHOD set_validar.

    DATA: w_zsdt0414 TYPE zsdt0414,
          w_zsdt0415 TYPE zsdt0415,
          lv_mesg    TYPE string.

    FREE: me->at_t_zsdt0414, me->at_t_zsdt0415.

    MOVE-CORRESPONDING i_zsds093  TO w_zsdt0414.
    APPEND w_zsdt0414             TO me->at_t_zsdt0414.

    LOOP AT i_zsds094           INTO DATA(_zsds094).
      MOVE-CORRESPONDING _zsds094 TO w_zsdt0415.
      APPEND w_zsdt0415           TO me->at_t_zsdt0415.
    ENDLOOP.

    TRY.
        me->set_validar_distribuicao( ).
      CATCH zcx_error INTO DATA(ex_error).
        MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                        ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
        me->set_error( lv_mesg ).
    ENDTRY.

  ENDMETHOD.


  METHOD set_validar_distribuicao.

    DATA: l_qtde        TYPE vbap-zmeng,
          l_qtde_lib    TYPE vbap-zmeng,
          l_ok          TYPE char01,
          lv_mesg       TYPE string,
          lc_manutencao TYPE zde_manutencao,
          lv_object     TYPE ausp-objek,
          lc_mm_util    TYPE REF TO zcl_mm_util,
          t_objectdata  TYPE tt_clobjdat.

    FREE: l_qtde, l_qtde_lib.

    CREATE OBJECT lc_mm_util.

    SELECT SUM( qte_lib )
      FROM zsdt0082
      INTO l_qtde_lib
     WHERE nro_sol  = me->at_zsdt0082-nro_sol
       AND vbeln    = me->at_zsdt0082-vbeln
       AND posnr    = me->at_zsdt0082-posnr
       AND seq     <> '001'
       AND status  <> '4'
       AND qte_sol  = 0.

    IF at_t_zsdt0415[] IS INITIAL.
      lv_mesg = 'Saldo a Liberar não Informado!'.
      me->set_error( lv_mesg ).
    ENDIF.

*-US191316-22.09.2025-#191316-JT-inicio
*--------------------------------------------------------
*----validar estoque filial/Fornec.Armazen --------------
*--------------------------------------------------------
    TRY.
        me->get_ck_estoque_filial_armaz( ).
      CATCH zcx_error INTO DATA(ex_error).
        MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                        ex_error->msgv3 ex_error->msgv4 INTO lv_mesg.
        me->set_error( lv_mesg ).
    ENDTRY.
*-US191316-22.09.2025-#191316-JT-inicio

    LOOP AT at_t_zsdt0415 INTO DATA(_zsdt0415).

      FREE: t_objectdata.

      l_qtde = l_qtde + _zsdt0415-kwmeng.

      IF _zsdt0415-kwmeng > _zsdt0415-clabs AND me->at_zsdt0082-bloqueio = abap_off.
        lv_mesg = 'Quantidade a liberar ultrapassa Saldo disponivel!'.
        me->set_error( lv_mesg ).
      ENDIF.

      lv_object = _zsdt0415-matnr.

      lc_mm_util->get_caracteristica_material( EXPORTING i_class      = 'SEMENTES_GERAL'
                                                         i_object     = lv_object
                                               IMPORTING t_objectdata = t_objectdata ).

      LOOP AT t_objectdata INTO DATA(_objectdata) WHERE atnam = 'ID_CULTIVAR_INDEA' OR
                                                        atnam = 'ID CULTIVAR INDEA'.
      ENDLOOP.

      IF ( sy-subrc <> 0 ) OR ( sy-subrc = 0 AND ( _objectdata-ausp1 IS INITIAL OR
                                                   _objectdata-ausp1  = '0'     OR
                                                   _objectdata-ausp1  = '00'    OR
                                                   _objectdata-ausp1  = '000' ) ).
        lv_mesg = 'ID Cultivar Indea não esta vinculado na característica do material'.
        me->set_error( lv_mesg ).
      ENDIF.

      zcl_manutencao_insumos=>verificar_itinerario_82( EXPORTING i_rota_lr     = me->at_zsdt0082-nr_rot
                                                                 i_rota_pc     = _zsdt0415-nr_rot_pc
                                                                 is_background = abap_true
                                                       IMPORTING is_ok         = l_ok ).
      IF l_ok = abap_false.
*       lv_mesg = |{ 'Não existe Itinerário para' } Rota LR: { me->at_zsdt0082-nr_rot }, PC:{ _zsdt0415-nr_rot_pc }|.
        lv_mesg = |{ 'Não existe Itinerário baseado no roteiro PC:' } { _zsdt0415-nr_rot_pc ALPHA = OUT } x roteiro LR: { me->at_zsdt0082-nr_rot ALPHA = OUT }|.
        me->set_error( lv_mesg ).
      ENDIF.

    ENDLOOP.

    IF l_qtde > ( at_zsdt0082-qte_sol - l_qtde_lib ) AND me->at_zsdt0082-bloqueio = abap_off.
      lv_mesg = 'Quantidade a liberar ultrapassa Quantidade Origem!'.
      me->set_error( lv_mesg ).
    ENDIF.

*   IF ( l_qtde < l_qtde_lib ).
*     lv_mesg = 'Quantidade:' && l_qtde && ' não pode ser inferior a quantidade já distribuída:' && l_qtde_lib.
*     me->set_error( lv_mesg ).
*   ENDIF.

    IF _zsdt0415-dt_entrega IS INITIAL.
      lv_mesg = 'Preencher Data de Entrega!'.
      me->set_error( lv_mesg ).
    ENDIF.

    IF _zsdt0415-nr_rot_pc IS INITIAL.
      lv_mesg = 'Preencher Roteiro Ponto Coleta!'.
      me->set_error( lv_mesg ).
    ENDIF.

    IF _zsdt0415-flexibilidade IS INITIAL.
      lv_mesg = 'Preencher a Flexibilidade!'.
      me->set_error( lv_mesg ).
    ENDIF.

  ENDMETHOD.


  METHOD set_ajustar_dados.

    LOOP AT me->at_t_zsds094 INTO DATA(_zsds094).

      UPDATE zsdt0415 SET nr_rot_pc            = _zsds094-nr_rot_pc
*                         qte_sol              = _zsds094-kwmeng
*                         kwmeng               = _zsds094-kwmeng
                          dt_entrega           = _zsds094-dt_entrega
                          prioridade           = _zsds094-prioridade
                          flexibilidade        = _zsds094-flexibilidade
                          carga_auto           = _zsds094-carga_auto
                          transf_no_fornecedor = _zsds094-transf_no_fornecedor
                          marca                = _zsds094-marca
                    WHERE matnr                = _zsds094-matnr
                      AND mtart                = _zsds094-mtart
                      AND werks                = _zsds094-werks
                      AND lgort                = _zsds094-lgort
                      AND lifnr_pc             = _zsds094-lifnr_pc "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
                      AND ebeln                = _zsds094-ebeln
                      AND ebelp                = _zsds094-ebelp
*                     AND marca                = _zsds094-marca
                      AND charg                = _zsds094-charg
                      AND licha                = _zsds094-licha
*                     AND lifnr                = _zsds094-lifnr
                      AND cancelado            = abap_false.
    ENDLOOP.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD get_pendente_aprovacao.

    FREE r_pendente.

    SELECT SINGLE status
      INTO @DATA(_status)
      FROM zsdt0415
     WHERE ch_referencia = @i_ch_referencia
       AND status        = @c_pend_aprov
       AND cancelado     = @abap_off.

    r_pendente = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.


  METHOD get_table_zsdt0415.

    FREE: r_zsdt0415.

    READ TABLE me->at_t_zsdt0415 INTO r_zsdt0415 WITH KEY matnr       = i_zsds094-matnr
                                                          mtart       = i_zsds094-mtart
                                                          werks       = i_zsds094-werks
                                                          lgort       = i_zsds094-lgort
*                                                         lifnr_pc    = i_zsds094-lifnr_pc "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
                                                          ebeln       = i_zsds094-ebeln
                                                          ebelp       = i_zsds094-ebelp
*                                                         marca       = i_zsds094-marca
                                                          charg       = i_zsds094-charg.
*                                                         licha       = i_zsds094-licha.
*                                                         lifnr       = i_zsds094-lifnr.

  endmethod.


  METHOD set_status_avaliacao_solic.

    DATA: l_txline TYPE txline.

    FREE: l_txline.

    LOOP AT i_observacao INTO DATA(_observacao).
      l_txline = l_txline  && | { _observacao } |.
    ENDLOOP.

    IF i_user_acao = abap_off.
      UPDATE zsdt0411 SET status          = i_status
                          observacao      = l_txline
                    WHERE nro_sol         = i_nro_sol
                      AND id_distribuicao = i_id_distribuicao.
    ELSE.
      UPDATE zsdt0411 SET status          = i_status
                          observacao      = l_txline
                          user_acao       = i_user_acao
                          date_acao       = i_date_acao
                          time_acao       = i_time_acao
                    WHERE nro_sol         = i_nro_sol
                      AND id_distribuicao = i_id_distribuicao.
    ENDIF.

    COMMIT WORK AND WAIT.

    CHECK i_status = 'R' OR i_status = 'C'.

    SELECT SINGLE ch_referencia
      INTO @DATA(_ch_referencia)
      FROM zsdt0082
     WHERE nro_sol = @i_nro_sol
       AND seq     = 1.

    CHECK _ch_referencia IS NOT INITIAL.

    me->set_cancelar_solicitacao( i_ch_referencia   = _ch_referencia
                                  i_id_distribuicao = i_id_distribuicao ).

  ENDMETHOD.


  METHOD set_table_zsdt0415.

    me->at_t_zsdt0415[] = i_zsdt0415[].

  ENDMETHOD.


  METHOD cancelar_distribuicao.

    DATA: lv_var_answer TYPE c.
    DATA: lc_dados                 TYPE zmme_dados_safra.
    DATA: ls_retorno_consulta_item TYPE zsds392.

    DATA: ls_order_itens TYPE zde_safra_control_ordem_itens,
          lo_order_itens TYPE REF TO zcl_int_ob_safra_crt_ord_itens,
          lc_insumos     TYPE REF TO zcl_distribuicao_insumos.

    CLEAR: r_msg_error.

    FREE:  lc_insumos, lo_order_itens.
    CREATE OBJECT: lc_insumos, lo_order_itens.

    SELECT SINGLE *
      FROM zsdt0082 INTO @DATA(_zsdt0082)
     WHERE nro_sol = @i_nro_sol
       AND seq     = @i_seq
       AND vbeln   = @i_vbeln
       AND posnr   = @i_posnr.

    IF sy-subrc NE 0.
      r_msg_error = |Distribuição { i_nro_sol } Seq: { i_seq } não encontrada!|.
      RETURN.
    ENDIF.

    IF _zsdt0082-seq = '1'.
      r_msg_error = |Registro { i_nro_sol } Seq: { i_seq } não é uma distribuição!|.
      RETURN.
    ENDIF.

    IF _zsdt0082-status EQ '4'.
      r_msg_error = |Distribuição já Cancelada!|.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0131 INTO @DATA(_zsdt0131_carga)
     WHERE nro_sol EQ @i_nro_sol
       AND seq     EQ @i_seq
       AND vbeln   EQ @i_vbeln
       AND posnr   EQ @i_posnr
       AND status  NE 'X'.

    IF _zsdt0082-status EQ '5' OR _zsdt0131_carga IS NOT INITIAL.
      r_msg_error = |Distribuição já se encontra em fase de Planejamento/Produção/Entrega!|.
      RETURN.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja Cancelar esta Distribuição?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
        start_column          = 40
        start_row             = 10
      IMPORTING
        answer                = lv_var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK lv_var_answer = '1'.

    DATA(_zsdt0082_rollback)   = _zsdt0082.

    IF _zsdt0082-carga_automatica EQ abap_true.

      TRY .
          CALL METHOD lo_order_itens->set_metodo_http
            EXPORTING
              i_metodo = 'GET'.

          ls_order_itens-externalid = _zsdt0082-nro_sol && _zsdt0082-seq.

          CALL METHOD lo_order_itens->zif_integracao_outbound~execute_request
            EXPORTING
              i_info_request  = ls_order_itens
            IMPORTING
              e_id_integracao = DATA(lv_id_integracao)
              e_integracao    = DATA(lwa_integracao).

          CLEAR: ls_retorno_consulta_item.
          /ui2/cl_json=>deserialize( EXPORTING json        = lwa_integracao-ds_data_retorno
                                               pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                      CHANGING data        = ls_retorno_consulta_item ).

          IF ls_retorno_consulta_item IS INITIAL.
            r_msg_error = |Não foi possivel consultar solicitação { ls_order_itens-externalid } no Safra Control! |.
            RETURN.
          ENDIF.

          IF ls_retorno_consulta_item-quantity_in_cargo > 0.
            r_msg_error = |Solicitação { ls_order_itens-externalid } já encontra-se em carga no Safra Control! |.
            RETURN.
          ENDIF.

        CATCH zcx_integracao INTO DATA(lo_integracao).
          r_msg_error = |Não foi possivel consultar solicitação { ls_order_itens-externalid } no Safra Control |.
          RETURN.
        CATCH zcx_error INTO DATA(lo_error).
          r_msg_error = |Não foi possivel consultar solicitação { ls_order_itens-externalid } no Safra Control |.
          RETURN.
      ENDTRY.

    ENDIF.

    UPDATE zsdt0082 SET status    = 4
                        user_canc = sy-uname
                        dt_canc   = sy-datum
                  WHERE nro_sol   = _zsdt0082-nro_sol
                    AND vbeln     = _zsdt0082-vbeln
                    AND posnr     = _zsdt0082-posnr
                    AND seq       = _zsdt0082-seq.
    COMMIT WORK AND WAIT .

    IF _zsdt0082-carga_automatica = abap_true.

      lc_dados-nro_sol = _zsdt0082-nro_sol.
      lc_dados-seq     = _zsdt0082-seq.
      lc_dados-vbeln   = _zsdt0082-vbeln.
      lc_dados-posnr   = _zsdt0082-posnr.
      lc_insumos->set_integrar_safra_control( EXPORTING i_dados = lc_dados IMPORTING e_msg_erro = DATA(l_msg_error) ).

      IF l_msg_error IS NOT INITIAL.
        r_msg_error = l_msg_error.

        UPDATE zsdt0082 SET status    = _zsdt0082_rollback-status
                            user_canc = _zsdt0082_rollback-user_canc
                            dt_canc   = _zsdt0082_rollback-dt_canc
                      WHERE nro_sol   = _zsdt0082-nro_sol
                        AND vbeln     = _zsdt0082-vbeln
                        AND posnr     = _zsdt0082-posnr
                        AND seq       = _zsdt0082-seq.
        COMMIT WORK AND WAIT.
        RETURN.

      ENDIF.

    ENDIF.

    MESSAGE s024(sd) WITH 'Distribuição Cancelada!'.

  ENDMETHOD.


  METHOD check_uso_estoque_sol_ent.

    CLEAR: r_consumo.

    CHECK i_estoque_pedido-ebeln IS NOT INITIAL AND i_estoque_pedido-ebelp IS NOT INITIAL.

*------------------------------------------------------------------------------------------------------*
*   Levantar Solicitações para Pedido/Item
*------------------------------------------------------------------------------------------------------*
    SELECT nro_sol, seq, ebeln, ebelp, cancel, solicitacao_qte
      FROM zmmt0196 INTO TABLE @DATA(lit_zmmt0196)
     WHERE ebeln EQ @i_estoque_pedido-ebeln
       AND ebelp EQ @i_estoque_pedido-ebelp.

    DELETE lit_zmmt0196 WHERE cancel = abap_true.

    LOOP AT lit_zmmt0196 ASSIGNING FIELD-SYMBOL(<fs_zmmt0196>).
      ADD <fs_zmmt0196>-solicitacao_qte TO r_consumo.
    ENDLOOP.

    CHECK lit_zmmt0196[] IS NOT INITIAL.

*------------------------------------------------------------------------------------------------------*
*   Verificar se solicitações para Pedido/Item que já foram faturadas nas cargas
*------------------------------------------------------------------------------------------------------*

    "Solicitações vinculadas em Item Carga
    SELECT nro_sol, seq, nro_cg, item_carga, cancel, qtd_vinc_carga
      FROM zmmt0202 INTO TABLE @DATA(lit_zmmt0202)
       FOR ALL ENTRIES IN @lit_zmmt0196
     WHERE nro_sol EQ @lit_zmmt0196-nro_sol
       AND seq     EQ @lit_zmmt0196-seq.

    DELETE lit_zmmt0202 WHERE cancel = abap_true.

    CHECK lit_zmmt0202[] IS NOT INITIAL.

    "Item Carga em Conferencia
    SELECT nro_cg, item_carga, chave_nfe, prod_item, quantidade
      FROM zmmt0218 INTO TABLE @DATA(lit_zmmt0218)
       FOR ALL ENTRIES IN @lit_zmmt0202
     WHERE nro_cg       EQ @lit_zmmt0202-nro_cg
       AND item_carga   EQ @lit_zmmt0202-item_carga.

    SELECT nro_cg, chave_nfe
      FROM zmmt0203 INTO TABLE @DATA(lit_zmmt0203)
       FOR ALL ENTRIES IN @lit_zmmt0202
     WHERE nro_cg      EQ @lit_zmmt0202-nro_cg
       AND cancel      EQ @abap_false.

    CHECK lit_zmmt0218[] IS NOT INITIAL OR  lit_zmmt0203[] IS NOT INITIAL.

    "Notas de Entrada com Migo Entrada
    IF lit_zmmt0218[] IS NOT INITIAL.
      SELECT chave_nfe, mblnr, belnr
        FROM zib_nfe_dist_ter INTO TABLE @DATA(lit_nfe_dist_with_migo)
         FOR ALL ENTRIES IN @lit_zmmt0218
       WHERE chave_nfe EQ @lit_zmmt0218-chave_nfe.
    ENDIF.

    IF lit_zmmt0203 IS NOT INITIAL.
      SELECT chave_nfe, mblnr, belnr
       FROM zib_nfe_dist_ter APPENDING TABLE @lit_nfe_dist_with_migo
        FOR ALL ENTRIES IN @lit_zmmt0203
      WHERE chave_nfe EQ @lit_zmmt0203-chave_nfe.
    ENDIF.

    CHECK lit_nfe_dist_with_migo[] IS NOT INITIAL.

    DELETE lit_nfe_dist_with_migo WHERE mblnr IS INITIAL OR belnr IS INITIAL.
    SORT lit_nfe_dist_with_migo BY chave_nfe.
    DELETE ADJACENT DUPLICATES FROM lit_nfe_dist_with_migo COMPARING chave_nfe.

    CHECK lit_nfe_dist_with_migo[] IS NOT INITIAL.

    LOOP AT lit_zmmt0196 ASSIGNING <fs_zmmt0196>.

      LOOP AT lit_zmmt0202 ASSIGNING FIELD-SYMBOL(<fs_zmmt0202>) WHERE nro_sol = <fs_zmmt0196>-nro_sol
                                                                   AND seq     = <fs_zmmt0196>-seq.

        READ TABLE lit_zmmt0218 ASSIGNING FIELD-SYMBOL(<fs_zmmt0218>) WITH KEY nro_cg     = <fs_zmmt0202>-nro_cg
                                                                               item_carga = <fs_zmmt0202>-item_carga.
        IF sy-subrc EQ 0.

          LOOP AT lit_zmmt0218 ASSIGNING <fs_zmmt0218> WHERE nro_cg     = <fs_zmmt0202>-nro_cg
                                                         AND item_carga = <fs_zmmt0202>-item_carga.

            READ TABLE lit_nfe_dist_with_migo WITH KEY chave_nfe = <fs_zmmt0218>-chave_nfe TRANSPORTING NO FIELDS BINARY SEARCH.
            CHECK sy-subrc EQ 0 AND <fs_zmmt0218>-quantidade IS NOT INITIAL.

            SUBTRACT <fs_zmmt0218>-quantidade FROM r_consumo.
          ENDLOOP.

        ELSE.

          DATA(_carga_com_migo_pendente) = abap_false.
          DATA(_carga_com_nota)          = abap_false.

          LOOP AT lit_zmmt0203 ASSIGNING FIELD-SYMBOL(<fs_zmmt0203>) WHERE nro_cg = <fs_zmmt0202>-nro_cg.
            READ TABLE lit_nfe_dist_with_migo WITH KEY chave_nfe = <fs_zmmt0203>-chave_nfe TRANSPORTING NO FIELDS BINARY SEARCH.
            IF sy-subrc NE 0.
              _carga_com_migo_pendente = abap_true.
              EXIT.
            ENDIF.

            _carga_com_nota = abap_true.
          ENDLOOP.

          IF _carga_com_nota = abap_true AND _carga_com_migo_pendente EQ abap_false.
            SUBTRACT <fs_zmmt0202>-qtd_vinc_carga FROM r_consumo.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDLOOP.



  ENDMETHOD.


  METHOD get_ck_estoque_filial_armaz.

    DATA: w_saldo    TYPE zsdt0415,
          t_saldo    TYPE TABLE OF zsdt0415,
          t_zsds094  TYPE zsds094_tt,
          w_zsdt0415 TYPE zsdt0415,
          t_zsdt0415 TYPE TABLE OF zsdt0415,
          lv_mesg    TYPE string.

*--------------------------
* validacao Ativa?
*--------------------------
    SELECT SINGLE *
      INTO @DATA(_tvarvc)
      FROM tvarvc
     WHERE name = 'ZSDT0081_VALIDA_SALDO_FILIAL'.

    CHECK sy-subrc = 0 AND _tvarvc-low = abap_true.

    CHECK me->at_zsdt0082-bloqueio = abap_off.
    CHECK me->at_zsds094_tt[]     IS NOT INITIAL.

    FREE: t_saldo, t_zsdt0415.

    LOOP AT at_t_zsdt0415  INTO DATA(_zsdt0415).
      IF     _zsdt0415-matnr IS NOT INITIAL AND
             _zsdt0415-werks IS NOT INITIAL AND
             _zsdt0415-lgort IS NOT INITIAL AND
             _zsdt0415-ebeln IS     INITIAL AND
             _zsdt0415-ebelp IS     INITIAL.
        CLEAR w_zsdt0415.
        w_zsdt0415-matnr      = _zsdt0415-matnr.
        w_zsdt0415-werks      = _zsdt0415-werks.
        w_zsdt0415-lgort      = _zsdt0415-lgort.
        w_zsdt0415-lifnr      = _zsdt0415-lifnr.
        w_zsdt0415-kwmeng     = _zsdt0415-kwmeng.
        COLLECT w_zsdt0415 INTO t_zsdt0415.
      ENDIF.
    ENDLOOP.

    CHECK t_zsdt0415[] IS NOT INITIAL.

    LOOP AT t_zsdt0415   INTO w_zsdt0415.
      FREE:  t_saldo.

      t_zsds094[] = me->at_zsds094_tt[].

      DELETE t_zsds094  WHERE matnr <> w_zsdt0415-matnr
                           OR werks <> w_zsdt0415-werks
                           OR lgort <> w_zsdt0415-lgort
                           OR lifnr <> w_zsdt0415-lifnr
                           OR ebeln <> abap_off
                           OR ebelp <> abap_off.

      LOOP AT t_zsds094  INTO DATA(_zsds094).
        CLEAR w_saldo.
        w_saldo-matnr       = _zsds094-matnr.
        w_saldo-werks       = _zsds094-werks.
        w_saldo-lgort       = _zsds094-lgort.
        w_saldo-lifnr       = _zsds094-lifnr.
        w_saldo-clabs       = _zsds094-clabs.
        COLLECT w_saldo  INTO t_saldo.
      ENDLOOP.

      READ TABLE t_saldo INTO w_saldo INDEX 1.

      IF w_zsdt0415-kwmeng > w_saldo-clabs.
        lv_mesg = 'Quantidade a liberar ultrapassa Saldo por Filial /Fornec.Armazen.!!'.
        me->set_error( lv_mesg ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_ck_job_cancelado.

    DATA: lv_jobname TYPE tbtcjob-jobname,
          lv_name    TYPE tbtcjob-jobname.

    FREE: r_cancelado.

    lv_jobname = |DISTRIB_INSUMOS|.
    lv_name    = lv_jobname && '_' && i_zsdt0082-nro_sol.

    SELECT *
      FROM tbtcp
      INTO TABLE @DATA(t_tbtcp)
     WHERE jobname = @lv_name.

    CHECK sy-subrc = 0.

    SORT t_tbtcp BY sdldate DESCENDING
                    sdltime DESCENDING.

    READ TABLE t_tbtcp INTO DATA(w_tbtcp) INDEX 1.

    IF sy-subrc = 0 AND w_tbtcp-status = 'A'.
      r_cancelado = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_consumo_real.

    DATA: lv_qte_lib TYPE zsdt0082-qte_lib.

    FREE: lv_qte_lib.

    r_consumo = i_zsdt0415-qte_sol.

    SELECT SINGLE qte_sol
      INTO @DATA(_qte_sol_atu)
      FROM zsdt0415
     WHERE ch_referencia = @i_zsdt0415-ch_referencia
       AND item_distrib  = @i_zsdt0415-item_distrib
       AND etapa         = 'DLOQ'
       AND cancelado     = @abap_off.

    IF sy-subrc = 0.
      r_consumo = _qte_sol_atu.
    ENDIF.

*---------------------------------------
*---recuperar saldo zsdt0082
*---------------------------------------
    SELECT SINGLE nro_sol_new, vbeln_new, posnr_new, nro_sol_ref, seq_ref
      INTO @DATA(_0415_soli)
      FROM zsdt0415
     WHERE ch_referencia = @i_zsdt0415-ch_referencia
       AND item_distrib  = @i_zsdt0415-item_distrib
       AND etapa         = 'SOLI'
       AND cancelado     = @abap_off.

    IF sy-subrc = 0 AND _0415_soli-nro_sol_new IS NOT INITIAL.
      SELECT SINGLE qte_sol
        INTO @DATA(_qte_sol)
        FROM zsdt0082
       WHERE nro_sol = @_0415_soli-nro_sol_new
         AND status  <> '3'
         AND seq     = '001'.

      IF sy-subrc = 0.
        SELECT SUM( qte_lib )
          INTO  @lv_qte_lib
          FROM zsdt0082
         WHERE nro_sol  = @_0415_soli-nro_sol_new
           AND seq     <> '001'
           AND status  <> '4'
           AND qte_sol  = 0.
      ENDIF.

      r_consumo = _qte_sol - lv_qte_lib.
      RETURN.
    ENDIF.

*----------------------------------------
*-- quantidades Solic liberada
*----------------------------------------
    SELECT SINGLE nro_sol_new, nro_sol_ref, seq_ref
      INTO @DATA(_0415_libe)
      FROM zsdt0415
     WHERE ch_referencia = @i_zsdt0415-ch_referencia
       AND item_distrib  = @i_zsdt0415-item_distrib
       AND etapa         = 'LIBE'
       AND cancelado     = @abap_off.

    IF sy-subrc = 0 AND _0415_libe-nro_sol_ref IS NOT INITIAL.
      SELECT SINGLE qte_lib
        INTO @DATA(_qte_lib)
        FROM zsdt0082
       WHERE nro_sol = @_0415_libe-nro_sol_ref
         AND seq     = @_0415_libe-seq_ref.

      IF sy-subrc = 0.
        r_consumo = _qte_lib.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_dados_zsdt0415.

    DATA: t_zsdt0415 TYPE TABLE OF zsdt0415.

    FREE: r_zsdt0415, t_zsdt0415.

    SELECT *
      INTO TABLE t_zsdt0415
      FROM zsdt0415
     WHERE etapa       = 'DLOQ'
       AND matnr       = i_zsds094-matnr
       "AND mtart       = i_zsds094-mtart "Não é necessário para checar o estoque consumido
       AND werks       = i_zsds094-werks
       AND lgort       = i_zsds094-lgort
       "AND lifnr_pc    = i_zsds094-lifnr_pc  "Não é necessário para checar o estoque consumido
       AND lifnr       = i_zsds094-lifnr      "Deve checar o estoque fornecedor terceiro
       AND ebeln       = i_zsds094-ebeln
       AND ebelp       = i_zsds094-ebelp
*      AND marca       = i_zsds094-marca
       AND charg       = i_zsds094-charg
       "AND licha       = i_zsds094-licha   "Não é necessário para checar o estoque consumido
*      AND lifnr       = i_zsds094-lifnr
       AND cancelado   = abap_false.

    LOOP AT t_zsdt0415  INTO DATA(_zsdt0415).
      CLEAR: _zsdt0415-mesma_solic, _zsdt0415-solic_pai, _zsdt0415-nro_sol_filha, _zsdt0415-solic_cancelada.
      MODIFY t_zsdt0415 FROM _zsdt0415 INDEX sy-tabix.
    ENDLOOP.

    LOOP AT t_zsdt0415 INTO _zsdt0415.
      DATA(lv_tabix) = sy-tabix.

      _zsdt0415-qte_sol = me->get_consumo_real( _zsdt0415 ).

      IF _zsdt0415-nro_sol = i_zsds093-nro_sol AND
         _zsdt0415-seq     = i_zsds093-seq     AND
         _zsdt0415-vbeln   = i_zsds093-vbeln   AND
         _zsdt0415-posnr   = i_zsds093-posnr   AND
         _zsdt0415-status <> 'C'.
        _zsdt0415-mesma_solic = abap_true.
      ENDIF.

      SELECT SINGLE nro_sol_origem, status
        INTO @DATA(_zsdt0082)
        FROM zsdt0082
       WHERE nro_sol = @_zsdt0415-nro_sol
         AND seq     = @_zsdt0415-seq
         AND vbeln   = @_zsdt0415-vbeln
         AND posnr   = @_zsdt0415-posnr.

      IF sy-subrc = 0.
        IF _zsdt0082-status = '3'.
          _zsdt0415-solic_cancelada = abap_true.
        ENDIF.

        READ TABLE t_zsdt0415 INTO DATA(_0415) WITH KEY nro_sol  = _zsdt0082-nro_sol_origem
                                                        matnr    = _zsdt0415-matnr
                                                        lgort    = _zsdt0415-lgort
                                                        werks    = _zsdt0415-werks
                                                        lifnr_pc = _zsdt0415-lifnr_pc."SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
        IF sy-subrc = 0.
          _0415-solic_pai        = abap_true.
          _0415-nro_sol_filha    = _zsdt0415-nro_sol.
          MODIFY t_zsdt0415   FROM _0415     INDEX sy-tabix.
        ENDIF.
      ENDIF.

      MODIFY t_zsdt0415       FROM _zsdt0415 INDEX lv_tabix.
    ENDLOOP.

    SELECT SINGLE nro_sol_origem
      INTO @DATA(_nro_sol_origem2)
      FROM zsdt0082
     WHERE nro_sol = @i_zsds094-nro_sol
       AND seq     = @i_zsds094-seq
       AND vbeln   = @i_zsds094-vbeln
       AND posnr   = @i_zsds094-posnr.

    IF sy-subrc = 0.
      READ TABLE t_zsdt0415 INTO _0415 WITH KEY nro_sol  = _nro_sol_origem2
                                                matnr    = i_zsds094-matnr
                                                lgort    = i_zsds094-lgort
                                                werks    = i_zsds094-werks
                                                lifnr_pc = i_zsds094-lifnr_pc. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>

      IF sy-subrc = 0.
        _0415-solic_pai        = abap_true.
        _0415-nro_sol_filha    = i_zsds094-nro_sol.
        MODIFY t_zsdt0415   FROM _0415     INDEX sy-tabix.
      ENDIF.
    ENDIF.

    r_zsdt0415[] =  t_zsdt0415[].

  ENDMETHOD.


  METHOD get_permite_cancelar_distr.

    r_permite_cancelar = abap_true.

    SELECT item_distrib
      INTO TABLE @DATA(t_0415)
      FROM zsdt0415
     WHERE ch_referencia = @i_ch_referencia
       AND status       <> @c_concluido
       AND cancelado     = @abap_off.

    IF sy-subrc <> 0.
      r_permite_cancelar = abap_false.
      RETURN.
    ENDIF.

    LOOP AT t_0415 INTO DATA(_0415).
      SELECT SINGLE status
        INTO @DATA(_status)
        FROM zsdt0415
       WHERE ch_referencia = @i_ch_referencia
         AND item_distrib  = @_0415-item_distrib
         AND status        = @c_concluido
         AND cancelado     = @abap_off.

      IF sy-subrc = 0.
        r_permite_cancelar = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_analisar_estoque.

*   r_consumo = i_zsdt0415-qte_sol.
    r_consumo = me->get_consumo_real( i_zsdt0415 ).

*---se esta pendente
    IF i_zsdt0415-status <> 'C'.  "Concluido
      RETURN.
    ENDIF.

*---verifica item que foi distribuido
    SELECT SINGLE *
      FROM zsdt0415 INTO @DATA(_zsdt0415)
     WHERE ch_referencia = @i_zsdt0415-ch_referencia
       AND item_distrib  = @i_zsdt0415-item_distrib
       AND etapa         = 'LIBE'
       AND cancelado     = @abap_off.

    CHECK sy-subrc = 0.

    SELECT SINGLE status, nr_rot
      INTO @DATA(lwa_zsdt0082_lib)
      FROM zsdt0082
     WHERE nro_sol = @_zsdt0415-nro_sol_ref
       AND seq     = @_zsdt0415-seq_ref
       AND vbeln   = @_zsdt0415-vbeln_ref
       AND posnr   = @_zsdt0415-posnr_ref.

    IF ( sy-subrc NE 0 ) OR
       ( lwa_zsdt0082_lib-status = '4' ).  "Cancelada
      r_consumo = 0.
      RETURN.
    ENDIF.

    SELECT vbeln, posnr, nro_sol, seq, nro_lote, status, qtd_vinc
      FROM zsdt0131 INTO TABLE @DATA(lit_zsdt0131)
     WHERE nro_sol = @_zsdt0415-nro_sol_ref
       AND seq     = @_zsdt0415-seq_ref
       AND vbeln   = @_zsdt0415-vbeln_ref
       AND posnr   = @_zsdt0415-posnr_ref.

    DELETE lit_zsdt0131 WHERE status = 'X'. "Eliminar Cancelados...

    CHECK lit_zsdt0131[] IS NOT INITIAL.

    SELECT nro_cg, vbeln, posnr, nr_rot, ch_referencia
      FROM zsdt0134 INTO TABLE @DATA(lit_zsdt0134)
       FOR ALL ENTRIES IN @lit_zsdt0131
     WHERE vbeln EQ @lit_zsdt0131-vbeln
       AND posnr EQ @lit_zsdt0131-posnr.

    IF lit_zsdt0134[] IS NOT INITIAL.
      SELECT ch_referencia, doc_rem
        FROM zsdt0001 INTO TABLE @DATA(lit_zsdt0001)
        FOR ALL ENTRIES IN @lit_zsdt0134
       WHERE ch_referencia EQ @lit_zsdt0134-ch_referencia.
    ENDIF.

    SELECT nro_lote, nro_cg
      FROM zsdt0129 INTO TABLE @DATA(lit_zsdt0129)
       FOR ALL ENTRIES IN @lit_zsdt0131
     WHERE nro_lote EQ @lit_zsdt0131-nro_lote.

    SORT: lit_zsdt0129 BY nro_lote,
          lit_zsdt0134 BY nro_cg vbeln posnr nr_rot,
          lit_zsdt0001 BY ch_referencia.

    LOOP AT lit_zsdt0131 INTO DATA(lwa_zsdt0131).

      READ TABLE lit_zsdt0129 INTO DATA(lwa_zsdt0129) WITH KEY nro_lote = lwa_zsdt0131-nro_lote BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      READ TABLE lit_zsdt0134 INTO DATA(lwa_zsdt0134) WITH KEY nro_cg = lwa_zsdt0129-nro_cg
                                                               vbeln  = lwa_zsdt0131-vbeln
                                                               posnr  = lwa_zsdt0131-posnr
                                                               nr_rot = lwa_zsdt0082_lib-nr_rot BINARY SEARCH.

      CHECK sy-subrc EQ 0 AND lwa_zsdt0134-ch_referencia IS NOT INITIAL.

      READ TABLE lit_zsdt0001 INTO DATA(lwa_zsdt0001) WITH KEY ch_referencia = lwa_zsdt0134-ch_referencia BINARY SEARCH.
      CHECK sy-subrc EQ 0 AND lwa_zsdt0001-doc_rem IS NOT INITIAL.

      SUBTRACT lwa_zsdt0131-qtd_vinc FROM r_consumo.
    ENDLOOP.


  ENDMETHOD.


  METHOD set_criar_job.

    DATA: lv_jobname      TYPE tbtcjob-jobname,
          lv_name         TYPE tbtcjob-jobname,
          lv_number       TYPE tbtcjob-jobcount,
          lv_mesg         TYPE string,
          lv_json_zsds093 TYPE string,
          lv_json_zsds094 TYPE string.

    lv_json_zsds093 = /ui2/cl_json=>serialize( data = i_zsds093 ).
    lv_json_zsds094 = /ui2/cl_json=>serialize( data = i_zsds094 ).

    IF 1 = 2.
      SUBMIT zsdr0038_job  WITH p_zsd093 = lv_json_zsds093
                           WITH p_zsd094 = lv_json_zsds094
                            AND RETURN.
    ENDIF.

    TRY.
        DATA(lv_user_job) = zcl_job=>get_user_job( ).
      CATCH zcx_job INTO DATA(ex_job).
        MESSAGE ID ex_job->msgid TYPE 'S' NUMBER ex_job->msgno WITH ex_job->msgv1 ex_job->msgv2
                                                                    ex_job->msgv3 ex_job->msgv4 INTO lv_mesg.
        me->set_error( lv_mesg ).
    ENDTRY.

    lv_jobname        = |DISTRIB_INSUMOS|.
    lv_name           = lv_jobname && '_' && i_zsds093-nro_sol.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_name
      IMPORTING
        jobcount         = lv_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      lv_mesg ='Houve um Erro ao Criar o JOB!'.
      me->set_error( lv_mesg ).
    ENDIF.

    SUBMIT zsdr0038_job  WITH p_zsd093 = lv_json_zsds093
                         WITH p_zsd094 = lv_json_zsds094
                      VIA JOB lv_name
                       NUMBER lv_number
                         USER lv_user_job
                          AND RETURN.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_number
        jobname              = lv_name
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.

  ENDMETHOD.


  METHOD set_estornar_solicitacao.

    UPDATE zsdt0415 SET cancelado     = abap_true
                  WHERE ch_referencia = i_ch_referencia
                    AND item_distrib  = i_item_distrib.

    SELECT SINGLE nro_sol, seq, vbeln, posnr
       FROM zsdt0082
       INTO @DATA(_zsdt0082)
      WHERE ch_referencia = @i_ch_referencia
        AND seq           = 1.

    me->set_avaliar_status_solic( i_nro_sol = _zsdt0082-nro_sol i_seq = _zsdt0082-seq i_vbeln = _zsdt0082-vbeln i_posnr = _zsdt0082-posnr ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD SET_INTEGRAR_SAFRA_CONTROL_BK.

    DATA: lo_order       TYPE REF TO zcl_int_ob_safra_crt_order,
          lo_order_itens TYPE REF TO zcl_int_ob_safra_crt_ord_itens,
          ls_order       TYPE zde_safra_control_ordem,
          ls_order_itens TYPE zde_safra_control_ordem_itens,
          ls_retorno     TYPE zintegracao,
          lv_external_id TYPE string,
          lv_wgbez       TYPE t023t-wgbez,
          lv_peso        TYPE i,
          lv_matnr_out   TYPE mara-matnr,
          lv_kunnr       TYPE kunnr,
          lv_lifnr       TYPE lifnr,
          lv_denom       TYPE dec10,
          lv_fator       TYPE dec10,
          lv_prioridade  TYPE string.

    SELECT SINGLE *
      FROM zsdt0082
      INTO @DATA(ls_0082)
     WHERE nro_sol = @i_dados-nro_sol
       AND seq     = @i_dados-seq
       AND vbeln   = @i_dados-vbeln
       AND posnr   = @i_dados-posnr
       AND status <> '4'.   "Diferente Cancelada

    IF sy-subrc IS NOT INITIAL.
*     e_msg_erro = 'Nenhum registro encontrado'.
      RETURN.
    ENDIF.

*** Monta header - Inicio
    ls_order-externalid      = ls_0082-nro_sol && ls_0082-seq.
    ls_order-externalorderid = ls_0082-nro_sol && ls_0082-seq.
    ls_order-type            = 'normal'.

    SELECT inco1
      FROM vbkd
      INTO @DATA(lv_inco1)
        UP TO 1 ROWS
     WHERE vbeln = @ls_0082-vbeln.
    ENDSELECT.

    IF sy-subrc IS INITIAL.
      ls_order-freighttype = 'CIF'. "lv_inco1.
    ENDIF.

    ls_order-enabled = 'true'.

    SELECT doc_simulacao
      FROM zsdt0090
      INTO @DATA(lv_doc_simul)
        UP TO 1 ROWS
     WHERE vbeln = @ls_0082-vbeln.
    ENDSELECT.

    IF sy-subrc IS INITIAL  .
      SELECT SINGLE safra,tpcult,cultura
        FROM zsdt0040
        INTO @DATA(ls_0040)
       WHERE doc_simulacao = @lv_doc_simul.
    ELSE.
      SELECT doc_simulacao
        FROM zsdt0041
        INTO lv_doc_simul
          UP TO 1 ROWS
       WHERE vbeln = ls_0082-vbeln.
      ENDSELECT.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE safra tpcult
          FROM zsdt0040
          INTO ls_0040
         WHERE doc_simulacao = lv_doc_simul.
      ENDIF.
    ENDIF.

    ls_order-period-externalid = ls_0040-safra.
    ls_order-duedate = sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2).

    SELECT SINGLE kunnr
      FROM zsdt0132
      INTO @lv_kunnr
     WHERE nr_rot = @ls_0082-nr_rot.

    IF sy-subrc IS INITIAL AND lv_kunnr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_kunnr
        IMPORTING
          output = ls_order-customer-externalid.
    ELSE.
      SELECT SINGLE kunnr
        INTO lv_kunnr
        FROM vbak
       WHERE vbeln = ls_0082-vbeln.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_kunnr
        IMPORTING
          output = ls_order-customer-externalid.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_0082-vkbur
      IMPORTING
        output = ls_order-branchoffice-externalid.

*   ls_order-branchoffice-externalid = ls_0082-vkbur.

    SELECT SINGLE lifnr
      FROM zsdt0132
      INTO @lv_lifnr
     WHERE nr_rot = @ls_0082-nr_rot_pc.

    IF sy-subrc IS INITIAL AND lv_lifnr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_lifnr
        IMPORTING
          output = ls_order-withdrawalplace-externalid.
    ELSE.
      ls_order-withdrawalplace-externalid = ls_order-branchoffice-externalid.
    ENDIF.

    if ls_0082-prioridade is NOT INITIAL.
      ls_order-specialattention = 'true'.
    else.
      ls_order-specialattention = 'false'.
    endif.

    ls_order-customfields     = '{"utilization": " "}'.
*** Monta header - Fim

    CONDENSE ls_order-customer-externalid NO-GAPS.
    CONDENSE ls_order-branchoffice-externalid NO-GAPS.
    CONDENSE ls_order-withdrawalplace-externalid NO-GAPS.

*** Monta itens - Inicio

    ls_order_itens-externalid              = ls_order-externalid.
    ls_order_itens-externalorderid         = ls_order-externalid.
    ls_order_itens-order-externalid        = ls_order-externalid.
    ls_order_itens-itemcode                = '1'.
    ls_order_itens-freighttype             = 'CIF'.
    ls_order_itens-quantity                = ls_0082-qte_lib. "ls_0082-qte_lib.
    ls_order_itens-enabled                 = 'true'.
    ls_order_itens-cancelad                = 'false'.
    ls_order_itens-schedulingdate          = ls_0082-dt_entrega(4) && '-' && ls_0082-dt_entrega+4(2) && '-' && ls_0082-dt_entrega+6(2).
    ls_order_itens-period-externalid       = ls_0040-safra.
    ls_order_itens-customer-externalid     = ls_order-customer-externalid.
    ls_order_itens-branchoffice-externalid = ls_order-branchoffice-externalid.

    SELECT SINGLE matnr
      FROM vbap
      INTO @DATA(lv_matnr)
     WHERE vbeln = @ls_0082-vbeln
       AND posnr = @ls_0082-posnr.

    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_matnr
        IMPORTING
          output = lv_matnr_out.

      SELECT SINGLE *
        INTO me->at_mara
        FROM mara
       WHERE matnr = lv_matnr.

      SELECT SINGLE *
        INTO me->at_makt
        FROM makt
       WHERE matnr = lv_matnr
         AND spras = sy-langu.

      SELECT SINGLE *
        INTO me->at_marc
        FROM marc
       WHERE matnr = lv_matnr.

      SELECT SINGLE wgbez
        INTO lv_wgbez
        FROM t023t
       WHERE spras = sy-langu
         AND matkl = me->at_mara-matkl.

      CALL FUNCTION 'CONVERSION_FACTOR_GET'
        EXPORTING
          no_type_check        = abap_true
          unit_in              = me->at_mara-gewei
          unit_out             = 'G'
        IMPORTING
          denominator          = lv_denom
          numerator            = lv_fator
        EXCEPTIONS
          conversion_not_found = 1
          overflow             = 2
          type_invalid         = 3
          units_missing        = 4
          unit_in_not_found    = 5
          unit_out_not_found   = 6
          OTHERS               = 7.

      IF sy-subrc <> 0.
        lv_fator = 1.
      ENDIF.

      lv_peso                                      = me->at_mara-brgew.
      ls_order_itens-product-externalid            = lv_matnr_out.
      ls_order_itens-product-name                  = me->at_makt-maktx.
      ls_order_itens-product-weight                = lv_peso.
      ls_order_itens-product-unit                  = me->at_mara-meins.
      ls_order_itens-product-conversionfactor      = lv_fator.
      ls_order_itens-product-multiple              = 1.
      ls_order_itens-product-ncm                   = me->at_marc-steuc.
      ls_order_itens-product-group-externalid      = me->at_mara-matkl.
      ls_order_itens-product-group-name            = lv_wgbez.
      ls_order_itens-product-group-enabled         = 'true'.
      ls_order_itens-product-variety-externalid    = lv_wgbez. "me->at_mara-wrkst.
      ls_order_itens-product-variety-name          = lv_wgbez. "me->at_mara-wrkst.
      ls_order_itens-product-supplier-externalid   = ls_order-branchoffice-externalid."me->at_mara-matnr.

      CONDENSE ls_order_itens-product-externalid NO-GAPS.
    ENDIF.

    ls_order_itens-withdrawalplace-externalid      = ls_order-withdrawalplace-externalid.
    ls_order_itens-shippingaddress-externalid      = ls_order-customer-externalid. "ls_0082-nr_rot.
    ls_order_itens-cultivation-externalid          = '1'. "ls_0040-cultura. "ls_0040-tpcult.


    if ls_0082-prioridade is NOT INITIAL.
      ls_order_itens-specialattention                = 'true'.
    else.
      ls_order_itens-specialattention                = 'false'.
    endif.


    IF ls_0082-prioridade = abap_true.
      lv_prioridade = 'true'.
    ELSE.
      lv_prioridade = 'false'.
    ENDIF.

*   ls_order_itens-customfields = '"{\"dataHoraPriorizacao\":"' && sy-datum && '",\"prioridade\":"' && lv_prioridade && '"}"'.
    ls_order_itens-customfields = ls_order-customfields.
*** Monta itens - Fim

    IF ls_0082-status <> '4'. "Diferente Cancelada

      TRY .
          CREATE OBJECT lo_order.

          CALL METHOD lo_order->set_metodo_http
            EXPORTING
              i_metodo = 'POST'.

          CALL METHOD lo_order->zif_integracao_outbound~execute_request
            EXPORTING
              i_info_request           = ls_order
            IMPORTING
              e_id_integracao          = DATA(lv_id_integracao)
              e_integracao             = ls_retorno
            RECEIVING
              r_if_integracao_outbound = DATA(lo_outbound).

        CATCH zcx_integracao INTO DATA(lo_integracao).
          e_msg_erro = lo_integracao->get_text( ).
          RETURN.
        CATCH zcx_error INTO DATA(lo_error).
          e_msg_erro = lo_error->get_text( ).
          RETURN.
      ENDTRY.

      IF ls_retorno-nm_code EQ '409'.

        TRY .
            CREATE OBJECT lo_order.

            CALL METHOD lo_order->set_metodo_http
              EXPORTING
                i_metodo = 'PUT'.

            CALL METHOD lo_order->zif_integracao_outbound~execute_request
              EXPORTING
                i_info_request           = ls_order
              IMPORTING
                e_id_integracao          = lv_id_integracao
                e_integracao             = ls_retorno
              RECEIVING
                r_if_integracao_outbound = lo_outbound.

          CATCH zcx_integracao INTO lo_integracao.
            e_msg_erro = lo_integracao->get_text( ).
            RETURN.
          CATCH zcx_error INTO lo_error.
            e_msg_erro = lo_error->get_text( ).
            RETURN.
        ENDTRY.

        TRY .
            CREATE OBJECT lo_order_itens.

            CALL METHOD lo_order_itens->set_metodo_http
              EXPORTING
                i_metodo = 'PUT'.

            CALL METHOD lo_order_itens->zif_integracao_outbound~execute_request
              EXPORTING
                i_info_request           = ls_order_itens
              IMPORTING
                e_id_integracao          = lv_id_integracao
                e_integracao             = ls_retorno
              RECEIVING
                r_if_integracao_outbound = lo_outbound.

          CATCH zcx_integracao INTO lo_integracao.
            e_msg_erro = lo_integracao->get_text( ).
            RETURN.
          CATCH zcx_error INTO lo_error.
            e_msg_erro = lo_error->get_text( ).
            RETURN.
        ENDTRY.

      ELSE.

        CLEAR e_msg_erro.

        TRY .
            CREATE OBJECT lo_order_itens.

            CALL METHOD lo_order_itens->set_metodo_http
              EXPORTING
                i_metodo = 'POST'.

            CALL METHOD lo_order_itens->zif_integracao_outbound~execute_request
              EXPORTING
                i_info_request           = ls_order_itens
              IMPORTING
                e_id_integracao          = lv_id_integracao
                e_integracao             = ls_retorno
              RECEIVING
                r_if_integracao_outbound = lo_outbound.

          CATCH zcx_integracao INTO lo_integracao.
            e_msg_erro = lo_integracao->get_text( ).
          CATCH zcx_error INTO lo_error.
            e_msg_erro = lo_error->get_text( ).
        ENDTRY.

        IF e_msg_erro IS NOT INITIAL.

          TRY .

              CALL METHOD lo_order->set_metodo_http
                EXPORTING
                  i_metodo = 'DELETE'.

              CALL METHOD lo_order->zif_integracao_outbound~execute_request
                EXPORTING
                  i_info_request           = ls_order-externalid
                IMPORTING
                  e_id_integracao          = lv_id_integracao
                  e_integracao             = ls_retorno
                RECEIVING
                  r_if_integracao_outbound = lo_outbound.

            CATCH zcx_integracao INTO lo_integracao.
              e_msg_erro = lo_integracao->get_text( ).
              RETURN.
            CATCH zcx_error INTO lo_error.
              e_msg_erro = lo_error->get_text( ).
              RETURN.
          ENDTRY.

        ENDIF.

      ENDIF.

    ELSE.

      ls_order_itens-enabled = 'false'.
      ls_order_itens-cancelad = 'true'.

      TRY .
          CREATE OBJECT lo_order_itens.

          CALL METHOD lo_order_itens->set_metodo_http
            EXPORTING
              i_metodo = 'PUT'.

          CALL METHOD lo_order_itens->zif_integracao_outbound~execute_request
            EXPORTING
              i_info_request           = ls_order_itens
            IMPORTING
              e_id_integracao          = lv_id_integracao
              e_integracao             = ls_retorno
            RECEIVING
              r_if_integracao_outbound = lo_outbound.

        CATCH zcx_integracao INTO lo_integracao.
          e_msg_erro = lo_integracao->get_text( ).
          RETURN.
        CATCH zcx_error INTO lo_error.
          e_msg_erro = lo_error->get_text( ).
          RETURN.
      ENDTRY.

    ENDIF.


  ENDMETHOD.


  METHOD set_limpar_mensagem.

    LOOP AT me->at_t_zsdt0415   INTO DATA(_zsdt0415) WHERE status <> c_concluido.
      _zsdt0415-mensagem           = abap_off.
      MODIFY me->at_t_zsdt0415  FROM _zsdt0415 INDEX sy-tabix.
      MODIFY zsdt0415           FROM _zsdt0415.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_salvar_copia_distrib.

    me->at_zsds094_tt[] = i_zsds094[].

  ENDMETHOD.
ENDCLASS.
