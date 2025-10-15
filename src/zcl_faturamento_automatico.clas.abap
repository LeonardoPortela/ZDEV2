class ZCL_FATURAMENTO_AUTOMATICO definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '203' ##NO_TEXT.
  data AT_DATA_REQUEST type ZLESE0200 .
  data AT_DATA_RESPONSE type ZLESE0201 .
  data AT_DATA_RESPONSE_AUTOM type ZLESE0241 .
  data AT_RETORNO_OPUS type ZLESE0230 .
  data AT_REQUEST_FATURAMENTO type ZLESE0240 .
  data AT_ZSDT0001 type ZDE_LES_ZSDT0001 .
  data AT_SAIDA type ZDE_LES_SAIDA_ZSDT0001 .
  data AT_T_SAIDA type ZDE_LES_SAIDA_ZSDT0001_T .
  data AT_EDITA_AGENTE_FRETE type CHAR01 .
  data AT_EDITA_TP_ADMIM_PED type CHAR01 .
  data AT_EDITA_TP_CARD_PED type CHAR01 .
  data AT_EDITA_PEDAGIO type CHAR01 .
  data AT_EDITA_CK_CREDITA_PED type CHAR01 .
  data AT_ZLEST0026 type ZLEST0026 .
  data AT_ZLEST0101 type ZLEST0101 .
  data:
    at_t_pracas TYPE TABLE OF zlest0102 .
  data AT_KBETR type KBETR_KOND .
  data AT_KONWA type KONWA .
  data AT_KRECH type KRECH .
  data AT_VALOR_FRETE type KBETR_KOND .
  data AT_MOEDA_FRETE type KONWA .
  data AT_ID_REFERENCIA type STRING .
  data AT_TP_REFERENCIA type STRING .
  data AT_WEBSERVICE type ZAUTH_WEBSERVICE .
  data AT_SET_TOKEN type STRING .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_JSON type STRING .
  data AT_METODO type STRING .
  data AT_ZLEST0240 type ZLEST0240 .
  data AT_ZLEST0242 type ZLEST0242_T .
  data AT_CH_REFERENCIA type ZCH_REF .
  data AT_ZLEST0241 type ZLEST0241_T .
  data AT_W_ZLEST0240 type ZLEST0240 .
  data AT_W_ZLEST0241 type ZLEST0241 .
  data AT_COCKPIT type ZCOCKPIT .
  data AT_ORIGEM type ZFATORIGEM .
  data AT_NR_PROTOCOLO type ZID_INTEGRACAO .
  data AT_CH_FATURAMENTO type ZID_INTEGRACAO .
  data AT_ITEM_FATURAMENTO type ZITEM_FATURAMENTO .
  data AT_T_STATUS type ZLEST0250_T .
  data AT_FATURAMENTO_INTERNO_SAP type CHAR01 .

  methods CONSTRUCTOR .
  methods SET_EXECUTAR_FATURAMENTO
    importing
      !I_MSG_INBOUND type STRING
      !I_MSG_COMPLETA type ZINTEGRACAO
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    exporting
      !E_MSG_OUTBOUND type STRING
      !E_SUCESSO type CHAR01
      !E_NM_CODE type CHAR03
      !E_MSG_ERRO type STRING
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_CONSULTA_FATURAMENTO
    importing
      !I_MSG_INBOUND type STRING
      !I_MSG_COMPLETA type ZINTEGRACAO
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    exporting
      !E_MSG_OUTBOUND type STRING
      !E_SUCESSO type CHAR01
      !E_NM_CODE type CHAR03
      !E_MSG_ERRO type STRING
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_RETORNO_OPUS
    importing
      !I_CH_FATURAMENTO type ZID_INTEGRACAO optional
      !I_CH_REFERENCIA type ZCH_REF optional
      !I_CONSULTA_OPUS type CHAR01 optional
      !I_PERMITE_CANCELAR type CHAR01 optional
    exporting
      !E_SUCESSO type CHAR01
      !E_NM_CODE type CHAR03
      !E_MSG_ERRO type STRING
    returning
      value(R_RETORNO) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_FATURAMENTO_AUTOMATICO
    importing
      !I_ORIGEM type ZFATORIGEM
      !I_INFO_FRETE type ZLESE0222 optional
      !I_INICIA_FATURAMENTO type CHAR01 optional
      !I_CONTINUA_FATURAMENTO type CHAR01 optional
      !I_CANCELA_FATURAMENTO type CHAR01 optional
      !I_PREENCH_INFO_AUTOMATICA type CHAR01 optional
    changing
      !E_STATUS_CODE type CHAR03
    returning
      value(R_MSG_ERRO) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_EMITIR_DOCUMENTOS
    importing
      !I_CH_FATURAMENTO type ZID_INTEGRACAO
      !I_ITEM_FATURAMENTO type ZITEM_FATURAMENTO
    raising
      ZCX_ERROR .
  methods SET_MENSAGEM
    importing
      !I_COD type CHAR03
      !I_MESG type STRING optional
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_GRAVAR_MENSAGEM
    importing
      !I_CH_REFERENCIA type ZCH_REF
      !I_TYPE type BAPI_MTYPE
      !I_NRO type SYMSGNO optional
      !I_MSG type STRING optional
      !I_STATUS type ZSTATUS_REG
      !I_ENVIA type CHAR01 optional
      !I_TAB_BAPIRET1 type TAB_BAPIRET1 optional
      !I_TAB_BAPIRET2 type BAPIRET2_T optional
      !I_TAB_ERRO type ZLESE0223_T optional
      !I_ERRO_ABANDONAR type ZERRO_ABANDONAR optional
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_CK_TIPO_SELECAO
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(R_COCKPIT) type CHAR02 .
  methods GET_CK_DESMEMBRAMENTO
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(E_STATUS) type ZLESE0257 .
  methods GET_AGUARDAR_DESMEMBRAMENTO
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(R_ERRO) type CHAR01 .
  methods SET_INICIAR_FATURAMENTO
    importing
      !I_CH_FATURAMENTO type ZID_INTEGRACAO optional
      !I_CH_REFERENCIA type ZCH_REF optional
    raising
      ZCX_ERROR .
  methods GET_VALOR_FRETE
    importing
      !I_ZSDT0001 type ZDE_LES_ZSDT0001
    exporting
      !E_VALOR_FRETE type KBETR_KOND
      !E_MOEDA_FRETE type KONWA
      !E_KRECH type KRECH
    raising
      ZCX_ERROR .
  methods SET_MONTAR_PROCESSAMENTO .
  methods GET_CK_PERMITE_CANCELAR
    importing
      !I_CH_FATURAMENTO type ZID_INTEGRACAO
      !I_ITEM_FATURAMENTO type ZITEM_FATURAMENTO optional
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_CK_JOB_CANCELADO
    importing
      !I_CH_FATURAMENTO type ZID_INTEGRACAO
      !I_ITEM_FATURAMENTO type ZITEM_FATURAMENTO
    returning
      value(R_STATUS_CANCELADO) type CHAR01 .
  methods GET_CK_ABANDONAR_PROCESSAMENTO
    importing
      !I_CH_FATURAMENTO type ZID_INTEGRACAO
      !I_ITEM_FATURAMENTO type ZITEM_FATURAMENTO
      !I_STATUS_REG type ZSTATUS_REG
    returning
      value(R_ERRO_ABANDONAR) type STRING .
  methods GET_CK_RETORNO_SEFAZ
    importing
      !I_CH_FATURAMENTO type ZID_INTEGRACAO
      !I_ITEM_FATURAMENTO type ZITEM_FATURAMENTO
      !I_STATUS_REG type ZSTATUS_REG
    changing
      value(E_MENSAGEM) type STRING
      value(E_CODE) type STRING
      value(E_DESCRICAO_CODE) type STRING .
protected section.
private section.

  methods SET_SELECAO_DADOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_GERAR_REMESSA
    raising
      ZCX_ERROR .
  methods SET_GERAR_FATURA
    raising
      ZCX_ERROR .
  methods SET_GERAR_DANFE
    raising
      ZCX_ERROR .
  methods SET_GERAR_TRANSPORTE
    raising
      ZCX_ERROR .
  methods SET_GERAR_DOCUMENTOS_CUSTOS
    raising
      ZCX_ERROR .
  methods SET_GERAR_DACTE
    raising
      ZCX_ERROR .
  methods SET_GERAR_VIAGEM
    raising
      ZCX_ERROR .
  methods SET_GERAR_MDFE
    raising
      ZCX_ERROR .
  methods SET_GERAR_CREDITO_VIAGEM
    raising
      ZCX_ERROR .
  methods SET_VALIDAR_MDFE_GERADA
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_DOCNUM_MDFE) type J_1BDOCNUM .
  methods SET_SOLICITAR_CREDITO
    importing
      !I_DOCNUM type J_1BDOCNUM
    raising
      ZCX_ERROR .
  methods SET_ATUALIZAR_STATUS
    importing
      value(I_STATUS) type ZSTATUS_REG .
  methods SET_FINALIZAR_ROMANEIO
    importing
      !I_CH_FATURAMENTO type ZID_INTEGRACAO
      !I_ITEM_FATURAMENTO type ZITEM_FATURAMENTO .
  methods GET_ROMANEIO_FINALIZADO
    importing
      !I_CH_FATURAMENTO type ZID_INTEGRACAO
      !I_ITEM_FATURAMENTO type ZITEM_FATURAMENTO
    returning
      value(R_FINALIZADO) type CHAR01 .
  methods GET_OBTER_DOCTOS_ROMANEIO
    importing
      !I_CH_FATURAMENTO type ZID_INTEGRACAO
      !I_ITEM_FATURAMENTO type ZITEM_FATURAMENTO
    returning
      value(R_DOCUMENTOS) type ZDE_DATA_XSTRING .
  methods GET_CK_DANFE_DACTE_AUTORIZADA
    importing
      !I_CH_REFERENCIA type ZCH_REF
      !I_TYPE type CHAR05
    returning
      value(R_AUTORIZADA) type CHAR01
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_EXEC_OPUS
    importing
      !I_METODO type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    importing
      !I_METODO type STRING
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INTEGRACAO type ZINTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_MENSAGEM type ZINTEGRACAO_LOG
      !E_HEADER_FIELDS type ZDE_HEADER_FIELD_T
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_REFERENCIA .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA .
  methods SET_SELECAO_DADOS_OLD .
  methods GET_DADOS_FRETE
    exporting
      !E_STATUS_CODE type CHAR03
    returning
      value(R_MSG_ERRO) type STRING .
  methods SET_PREPARAR_FATURAMENTO
    changing
      !E_STATUS_CODE type CHAR03
    returning
      value(R_MSG_ERRO) type STRING
    raising
      ZCX_ERROR .
  methods SET_RESPONSE
    importing
      !I_STATUS_CODE type CHAR03
      !I_MSG_ERRO type STRING
      !I_INFORMACOESFRETE type ZLESE0203 optional
      !I_INFORMACOESPEDAGIO type ZLESE0207 optional .
  methods SET_RESPONSE_AUTOMATICO
    importing
      !I_STATUS_CODE type CHAR03
      !I_MSG_ERRO type STRING
      !I_INFORMACOESFRETE type ZLESE0212 optional
      !I_INFORMACOESPEDAGIO type ZLESE0213 optional .
  methods SET_TROCA_AGENTE
    importing
      !I_PLACA_CAV type ZPLACA
      !I_BRANCH type J_1BBRANC_
    changing
      !C_LIFNR type LIFNR .
  methods SET_MONTAR_INFORMACOES_BASE
    exporting
      !E_STATUS_CODE type CHAR03
    returning
      value(R_MSG_ERRO) type STRING .
  methods SET_PREPARAR_DADOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_PREPARAR_DADOS_OLD
    exporting
      !E_STATUS_CODE type CHAR03
    returning
      value(R_MSG_ERRO) type STRING .
  methods SET_MONTAR_JSON
    changing
      !E_STATUS_CODE type CHAR03
    returning
      value(R_MSG_ERRO) type STRING .
  methods SET_MONTAR_ESTRUTURA
    importing
      !I_INFO_FRETE type ZLESE0222 optional .
  methods GET_PROTOCOLO
    returning
      value(R_NR_PROTOCOLO) type ZNR_PROTOCOLO
    raising
      ZCX_ERROR .
  methods GET_CH_FATURAMENTO
    returning
      value(R_CH_INTEGRACAO) type ZID_INTEGRACAO
    raising
      ZCX_ERROR .
  methods GET_CK_FAT_AUTOMATICO
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
ENDCLASS.



CLASS ZCL_FATURAMENTO_AUTOMATICO IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

*---Status Processamento
    SELECT * FROM zlest0250 INTO TABLE at_t_status.  "*-#157580-06.11.2024-JT-inicio

  ENDMETHOD.


  METHOD get_aguardar_desmembramento.

    DATA: l_chv_faturar	TYPE zch_ref,
          l_cockpit     TYPE zcockpit,
          t_saida       TYPE zde_les_saida_zsdt0001_t,
          w_saida       TYPE zde_les_saida_zsdt0001,
          w_zsdt0001    TYPE zde_les_zsdt0001,
          t_romaneios   TYPE zsdt0001_t.

    r_erro = abap_true.

*-----------------------------------------------------------
*-- busca romaneio q sera faturado
*-----------------------------------------------------------
    CALL METHOD zcl_romaneio=>get_ck_faturar
      EXPORTING
        i_ch_referencia_sai = i_ch_referencia
      IMPORTING
        e_romaneios         = t_romaneios
        e_chv_faturar       = l_chv_faturar.

    IF lines( t_romaneios[] ) = 0 OR
       lines( t_romaneios[] ) = 1.
      r_erro = abap_false.
      RETURN.
    ENDIF.

    IF i_ch_referencia <> l_chv_faturar.
      r_erro = abap_false.
      RETURN.
    ENDIF.

*--------------------------------------------------------------
*---Romaneio que esta sendo faturado, verificar se foi gerada remessa
*---dos romaneios pares
*--------------------------------------------------------------
    l_cockpit = me->get_ck_tipo_selecao( i_ch_referencia ).

    DATA(l_total) = 0.
    LOOP AT t_romaneios INTO DATA(w_romaneios) WHERE ch_referencia <> i_ch_referencia.
      l_total = l_total + 1.
    ENDLOOP.

    DO 5 TIMES.
      DATA(l_gerados) = 0.
      LOOP AT t_romaneios INTO w_romaneios  WHERE ch_referencia <> i_ch_referencia.
        DO 20 TIMES.
          PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING w_romaneios-ch_referencia
                                                                          l_cockpit.
          PERFORM f_saida                   IN PROGRAM zlesr0102.
          PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING t_saida
                                                                          w_zsdt0001
                                                                          w_saida.
          IF w_saida-danfe IS NOT INITIAL AND w_saida-danfe(1) <> '@'.
            l_gerados = l_gerados + 1.
            EXIT.
          ENDIF.
          WAIT UP TO 10 SECONDS.
        ENDDO.
      ENDLOOP.

      IF l_gerados = l_total.
        r_erro = abap_false.
        RETURN.
      ENDIF.
    ENDDO.

    r_erro = abap_false.

  ENDMETHOD.


  METHOD get_ch_faturamento.

    DATA: l_seq TYPE char14.

    FREE: r_ch_integracao.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZLES_CHFAT'
        toyear      = sy-datum(4)
      IMPORTING
        number      = l_seq.

    IF sy-subrc <> 0.
      set_mensagem( '18' ).
    ENDIF.

    r_ch_integracao = 'FT' && sy-datum(4) && l_seq.

  ENDMETHOD.


  METHOD get_ck_abandonar_processamento.

    DATA: t_idd07v TYPE TABLE OF dd07v,
          w_idd07v TYPE dd07v.

    FREE: r_erro_abandonar.

    SELECT *
      FROM zlest0242
      INTO TABLE @DATA(t_0242)
     WHERE ch_faturamento   = @i_ch_faturamento
       AND item_faturamento = @i_item_faturamento
       AND status_reg       = @i_status_reg.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZERRO_ABANDONAR'
        text           = abap_true
        langu          = sy-langu
      TABLES
        dd07v_tab      = t_idd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    SORT t_0242           BY seq DESCENDING.
    READ TABLE t_0242   INTO DATA(w_0242) INDEX 1.

    CHECK w_0242-erro_abandonar IS NOT INITIAL.

    READ TABLE t_idd07v INTO w_idd07v WITH KEY domvalue_l = w_0242-erro_abandonar.

    IF sy-subrc = 0.
      r_erro_abandonar = w_idd07v-ddtext.
    ENDIF.

  ENDMETHOD.


  METHOD get_ck_danfe_dacte_autorizada.

    DATA: l_docnum      TYPE j_1bnfdoc-docnum,
          l_vbeln       TYPE vbfa-vbeln,
          l_fksto       TYPE vbrk-fksto,
          l_mjahr       TYPE vbfa-mjahr,
          l_flag        TYPE char01,
          l_refkey      TYPE j_1bnflin-refkey,
          l_mesg        TYPE string,
          wa_active_mod TYPE          j_1bnfe_active.       "1090279

    r_autorizada = abap_off.

    SELECT SINGLE *
      INTO @DATA(_zsdt0001)
      FROM zsdt0001
     WHERE ch_referencia = @i_ch_referencia.

    CHECK sy-subrc = 0.

*-------------------------------------------
*-- danfe / dacte
*-------------------------------------------
    CASE i_type.
      WHEN 'DANFE'.
        SELECT SINGLE    vbeln   mjahr
                 INTO (l_vbeln,l_mjahr)
          FROM vbfa
         WHERE vbelv    = _zsdt0001-doc_rem
           AND vbtyp_n  = 'R'
           AND vbtyp_v  = 'J'.
        IF sy-subrc = 0.
          CONCATENATE l_vbeln l_mjahr INTO l_refkey.
          SELECT SINGLE docnum
            FROM j_1bnflin
            INTO l_docnum
           WHERE refkey = l_refkey.
          IF sy-subrc <> 0.
            SELECT SINGLE docnum
              FROM j_1bnflin
              INTO l_docnum
             WHERE refkey = _zsdt0001-fatura_prod.
          ENDIF.
        ELSE.
          SELECT SINGLE docnum
            FROM j_1bnflin
            INTO l_docnum
           WHERE refkey = _zsdt0001-fatura_prod.
        ENDIF.

      WHEN 'DACTE'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = _zsdt0001-fatura_frete
          IMPORTING
            output = _zsdt0001-fatura_frete.

        SELECT SINGLE j_1bnflin~docnum
                 INTO l_docnum
                 FROM j_1bnflin
           INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
                WHERE j_1bnflin~refkey = _zsdt0001-fatura_frete.

      WHEN 'MDFE'.
*-#158056-11.11.2024-JT-inicio
        IF _zsdt0001-fatura_frete IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = _zsdt0001-fatura_frete
            IMPORTING
              output = _zsdt0001-fatura_frete.

          SELECT SINGLE j_1bnflin~docnum
                   INTO l_docnum
                   FROM j_1bnflin
             INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
                  WHERE j_1bnflin~refkey = _zsdt0001-fatura_frete.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = _zsdt0001-nro_nf_prod
            IMPORTING
              output = _zsdt0001-nro_nf_prod.

          l_docnum = _zsdt0001-nro_nf_prod.
        ENDIF.
*-#158056-11.11.2024-JT-fim

        SELECT *
          FROM zsdt0105
          INTO TABLE @DATA(t_0105)
         WHERE docnum = @l_docnum.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        SELECT *
          FROM zsdt0102
          INTO TABLE @DATA(t_0102)
           FOR ALL ENTRIES IN @t_0105
         WHERE docnum = @t_0105-docnum_ref.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        DELETE t_0102 WHERE cancel    = abap_true
                         OR encerrado = abap_true
                         OR estornado = abap_true.

        SORT t_0102 BY docnum DESCENDING.

        LOOP AT t_0102 INTO DATA(w_0102).
          l_docnum = w_0102-docnum.
          EXIT.
        ENDLOOP.

        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        IF w_0102-autorizado = abap_true.
          r_autorizada = abap_true.
          RETURN.
        ENDIF.
    ENDCASE.

    DO 10 TIMES.
      SELECT SINGLE *
        FROM j_1bnfe_active
        INTO @DATA(w_active)
       WHERE docnum EQ @l_docnum.

      IF ( w_active-docsta EQ '1' AND w_active-cancel EQ abap_false ) OR
         ( w_active-docsta EQ '1' AND w_active-scssta EQ '2' ).
        r_autorizada = abap_true.

        CASE i_type.
          WHEN 'DANFE'.
            UPDATE zsdt0001 SET nro_nf_prod   = l_docnum
                          WHERE ch_referencia = i_ch_referencia.
          WHEN 'DACTE'.
            UPDATE zsdt0001 SET nro_nf_frete  = l_docnum
                          WHERE ch_referencia = i_ch_referencia.
        ENDCASE.

        RETURN.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.

*------------------------------------------
*---- danfe / dacte / mdfe
*------------------------------------------
      CASE i_type.
        WHEN 'DANFE' OR 'DACTE'.
          IF l_flag = abap_off.
            TRY.
                zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = CONV #( l_docnum )
                  )->set_registro(
                       EXPORTING
                         i_docnum = CONV #( l_docnum )
                         i_sem_bloqueio = abap_true
                  )->set_autorizar(
                         EXPORTING
                         i_aguardar = abap_true
                         i_ciclos   = 80
                         i_segundos = 10
                  )->get_registro(
                       IMPORTING
                         e_documento           = DATA(wl_doc)
                         e_info_doc_eletronico = DATA(wl_active)
                  ).
              CATCH zcx_doc_eletronico INTO DATA(ex_doc).
                MESSAGE ID ex_doc->msgid TYPE 'S' NUMBER ex_doc->msgno WITH ex_doc->msgv1 ex_doc->msgv2 ex_doc->msgv3 ex_doc->msgv4 INTO l_mesg.
                l_mesg = i_type && ':' &&  l_mesg.
                me->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_nro = ex_doc->msgno i_type = 'E' i_msg = l_mesg i_status = CONV #( i_type(4) ) ).
                RETURN.
              CATCH cx_root INTO DATA(ex_root).
            ENDTRY.
            l_flag = abap_true.
          ENDIF.

        WHEN 'MDFE'.
          IF l_flag = abap_off.
            TRY .
                DATA(obj_mdfe) = NEW zcl_mdfe( i_docnum = w_active-docnum i_nmdfe = w_active-nfnum9 ).
                obj_mdfe->enviar_mdfe( i_faturamento_autom = abap_true
                                       i_ch_referencia     = i_ch_referencia
                                       i_sem_confirmacao   = abap_true ).
                CLEAR: obj_mdfe.
              CATCH cx_root INTO ex_root.
            ENDTRY.
            l_flag = abap_true.
          ENDIF.
      ENDCASE.

    ENDDO.

  ENDMETHOD.


  METHOD get_ck_desmembramento.

    DATA: t_romaneios   TYPE zsdt0001_t,
          l_faturar	    TYPE char01,
          l_cockpit     TYPE zcockpit,
          l_mensagem    TYPE char255,
          l_chv_faturar	TYPE zch_ref,
          t_saida       TYPE zde_les_saida_zsdt0001_t,
          t_result      TYPE zde_les_saida_zsdt0001_t,
          w_saida       TYPE zde_les_saida_zsdt0001,
          w_zsdt0001    TYPE zde_les_zsdt0001.

    FREE: e_status, t_result.

    CALL METHOD zcl_romaneio=>get_ck_faturar
      EXPORTING
        i_ch_referencia_sai = i_ch_referencia
      IMPORTING
        e_romaneios         = t_romaneios
        e_faturar           = l_faturar
        e_chv_faturar       = l_chv_faturar
        e_mensagem          = l_mensagem.

    DESCRIBE TABLE t_romaneios LINES DATA(l_lines).

*-------------------------------------------------
*-- se nao tem desmembramento, status do proprio romaneio
*-------------------------------------------------
    IF l_lines = 0.
      l_cockpit = me->get_ck_tipo_selecao( i_ch_referencia ).

      PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING i_ch_referencia
                                                                      l_cockpit.
      PERFORM f_saida                   IN PROGRAM zlesr0102.
      PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING t_saida
                                                                      w_zsdt0001
                                                                      w_saida.

      e_status-gerar_transporte = COND #( WHEN ( (     w_saida-inco1 = 'FOB' OR w_saida-inco1 = 'CFR' ) AND
                                               ( NOT w_saida-enc_conhecimento = abap_true ) )            OR
                                               (     w_saida-operacao(4)      = 'ZPAR' )               THEN abap_false
                                                                                                       ELSE abap_true ).

      e_status-calcula_frete    = COND #( WHEN ( (     w_saida-inco1 = 'FOB' OR w_saida-inco1 = 'CFR' ) AND
                                               ( NOT w_saida-enc_conhecimento = abap_true ) )            OR
                                               (     w_saida-operacao(4)      = 'ZPAR' )               THEN abap_false
                                                                                                       ELSE abap_true ).
      e_status-gerar_dacte      = COND #( WHEN ( (     w_saida-inco1 = 'FOB' OR w_saida-inco1 = 'CFR' ) AND
                                               ( NOT w_saida-enc_conhecimento = abap_true ) )            OR
                                               (     w_saida-operacao(4)      = 'ZPAR' )               THEN abap_false
                                                                                                       ELSE abap_true ).
      RETURN.
    ENDIF.

    IF l_lines = 1.
      READ TABLE t_romaneios INTO DATA(w_romaneios) WITH KEY ch_referencia = i_ch_referencia.
      IF sy-subrc = 0.
        l_cockpit = me->get_ck_tipo_selecao( w_romaneios-ch_referencia ).

        PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING w_romaneios-ch_referencia
                                                                        l_cockpit.
        PERFORM f_saida                   IN PROGRAM zlesr0102.
        PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING t_saida
                                                                        w_zsdt0001
                                                                        w_saida.

        e_status-gerar_transporte = COND #( WHEN ( (     w_saida-inco1 = 'FOB' OR w_saida-inco1 = 'CFR' ) AND
                                                 ( NOT w_saida-enc_conhecimento = abap_true ) )            OR
                                                 (     w_saida-operacao(4)      = 'ZPAR' )               THEN abap_false
                                                                                                         ELSE abap_true ).

        e_status-calcula_frete    = COND #( WHEN ( (     w_saida-inco1 = 'FOB' OR w_saida-inco1 = 'CFR' ) AND
                                                 ( NOT w_saida-enc_conhecimento = abap_true ) )            OR
                                                 (     w_saida-operacao(4)      = 'ZPAR' )               THEN abap_false
                                                                                                         ELSE abap_true ).
        e_status-gerar_dacte      = COND #( WHEN ( (     w_saida-inco1 = 'FOB' OR w_saida-inco1 = 'CFR' ) AND
                                                 ( NOT w_saida-enc_conhecimento = abap_true ) )            OR
                                                 (     w_saida-operacao(4)      = 'ZPAR' )               THEN abap_false
                                                                                                         ELSE abap_true ).
      ENDIF.
      RETURN.
    ENDIF.

*-------------------------------------------------
*-- verificar o status dos romaneios desmembrados
*-------------------------------------------------
    LOOP AT t_romaneios INTO w_romaneios.
      l_cockpit = me->get_ck_tipo_selecao( w_romaneios-ch_referencia ).

      PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING w_romaneios-ch_referencia
                                                                      l_cockpit.
      PERFORM f_saida                   IN PROGRAM zlesr0102.
      PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING t_saida
                                                                      w_zsdt0001
                                                                      w_saida.
      APPEND w_saida      TO t_result.

      IF w_romaneios-ch_referencia = i_ch_referencia.
        e_status-gerar_transporte = COND #( WHEN ( (     w_saida-inco1 = 'FOB'   OR w_saida-inco1 = 'CFR' ) AND
                                                 ( NOT w_saida-enc_conhecimento   = abap_true ) )            OR
                                                 (     w_saida-operacao(4)        = 'ZPAR' )                 OR
                                                 (     w_romaneios-ch_referencia <> l_chv_faturar )        THEN abap_false
                                                                                                           ELSE abap_true ).
        e_status-gerar_dacte      = COND #( WHEN ( (     w_saida-inco1 = 'FOB'   OR w_saida-inco1 = 'CFR' ) AND
                                                 ( NOT w_saida-enc_conhecimento   = abap_true ) )            OR
                                                 (     w_saida-operacao(4)        = 'ZPAR' )                 OR
                                                 (     w_romaneios-ch_referencia <> l_chv_faturar )        THEN abap_false
                                                                                                           ELSE abap_true ).
      ENDIF.
    ENDLOOP.

    LOOP AT t_result    INTO DATA(w_result) WHERE transp <> icon_icon_list.
    ENDLOOP.

    e_status-calcula_frete = COND #( WHEN sy-subrc = 0 THEN abap_true
                                                       ELSE abap_false ).

  ENDMETHOD.


  METHOD get_ck_fat_automatico.

    DATA: l_mesg  TYPE string.

*-#143658-19.06.2024-JT-inicio
*---Tipo Cockpit
    me->at_cockpit = me->get_ck_tipo_selecao( me->at_zsdt0001-ch_referencia ).

    SELECT SINGLE *
      FROM setleaf
      INTO @DATA(w_setleaf)
     WHERE setname  = 'COKCPIT_FAT_AUT'
       AND valfrom  = @me->at_cockpit.

    IF sy-subrc <> 0.
      set_mensagem( '28' ).
    ENDIF.

*---Grupo Mercadoria
    SELECT matkl
      INTO @DATA(_matkl)
      FROM mara
        UP TO 1 ROWS
     WHERE matnr = @me->at_zsdt0001-matnr.
    ENDSELECT.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    IF sy-subrc <> 0 AND me->at_zsdt0001-ch_referencia IS NOT INITIAL.
      SELECT SINGLE ch_referencia, matnr
        FROM zsdt0001_item INTO @DATA(_zsdt0001_item)
       WHERE ch_referencia = @me->at_zsdt0001-ch_referencia.

      IF sy-subrc EQ 0 AND _zsdt0001_item-matnr IS NOT INITIAL.
        SELECT SINGLE matkl
          FROM mara INTO @_matkl
         WHERE matnr = @_zsdt0001_item-matnr.
      ENDIF.
    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    IF sy-subrc <> 0.
      set_mensagem( '27' ).
    ENDIF.

    SELECT SINGLE *
      FROM tvarvc
      INTO @DATA(w_tvarvc)
     WHERE name     = 'MAGGI_GR_GRAOS'
       AND low      = @_matkl.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    IF sy-subrc NE 0.
      SELECT SINGLE *
        FROM tvarvc INTO @w_tvarvc
       WHERE name     = 'MAGGI_GR_SEMENTES'
         AND low      = @_matkl.
    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    IF sy-subrc <> 0.
      l_mesg = |Grupo de Material { _matkl } não Previsto na Automação!|.
      set_mensagem( i_cod = '99' i_mesg = l_mesg ).
    ENDIF.
*-#143658-19.06.2024-JT-fim

*---Filial
    SELECT SINGLE *
      FROM setleaf
      INTO w_setleaf
     WHERE setname  = 'MAGGI_FAT_AUT_ZLES0136'
       AND valfrom <= me->at_zsdt0001-branch
       AND valto   >= me->at_zsdt0001-branch.

    IF sy-subrc <> 0.
      l_mesg = |Filial { me->at_zsdt0001-branch } não Previsto na Automação!|.
      set_mensagem( i_cod = '99' i_mesg = l_mesg ).
    ENDIF.

  ENDMETHOD.


  METHOD get_ck_job_cancelado.

    r_status_cancelado = abap_off.

    SELECT SINGLE nome_job
      INTO @DATA(_nome_job)
      FROM zlest0241
     WHERE ch_faturamento   = @i_ch_faturamento
       AND item_faturamento = @i_item_faturamento
       AND cancelado        = @abap_false.

    CHECK sy-subrc = 0.

    SELECT *
      FROM tbtcp
      INTO TABLE @DATA(t_tbtcp)
     WHERE jobname = @_nome_job.

    CHECK sy-subrc = 0.

    SORT t_tbtcp BY sdldate DESCENDING
                    sdltime DESCENDING.

    READ TABLE t_tbtcp INTO DATA(w_tbtcp) INDEX 1.

    IF sy-subrc = 0 AND w_tbtcp-status = 'A'.
      r_status_cancelado = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_ck_permite_cancelar.

    SELECT SINGLE *
      FROM zlest0240
      INTO @DATA(_zlest0240)
     WHERE ch_faturamento = @i_ch_faturamento
       AND cancelado      = @abap_false.

    IF sy-subrc <> 0.
      set_mensagem( '24' ).
    ENDIF.

    IF i_item_faturamento IS INITIAL.
      SELECT *
        FROM zlest0241
        INTO TABLE @DATA(t_zlest0241)
       WHERE ch_faturamento   = @i_ch_faturamento
         AND cancelado        = @abap_false.
    ELSE.
      SELECT *
        FROM zlest0241
        INTO TABLE t_zlest0241
       WHERE ch_faturamento   = i_ch_faturamento
         AND item_faturamento = i_item_faturamento
         AND cancelado        = abap_false.
    ENDIF.

    IF sy-subrc <> 0.
      set_mensagem( '24' ).
    ENDIF.

*---- busca documentos gerados
    LOOP AT t_zlest0241         INTO DATA(w_zlest0241).
      IF w_zlest0241-selecionado = abap_true.  "*-#157580-06.11.2024-JT
        set_mensagem( '19' ).
      ENDIF.

      PERFORM f_selecao_fat_autom IN PROGRAM zlesr0102    USING w_zlest0241-ch_referencia _zlest0240-cockpit.
      PERFORM f_saida             IN PROGRAM zlesr0102.
      PERFORM f_recuperar_dados   IN PROGRAM zlesr0102 CHANGING me->at_t_saida me->at_zsdt0001 me->at_saida.

      IF me->at_zsdt0001-doc_rem      IS NOT INITIAL OR
         me->at_zsdt0001-fatura_prod  IS NOT INITIAL OR
         me->at_zsdt0001-nro_nf_prod  IS NOT INITIAL OR
         me->at_zsdt0001-doc_transp   IS NOT INITIAL OR
         me->at_zsdt0001-fatura_frete IS NOT INITIAL OR
         me->at_zsdt0001-nro_nf_frete IS NOT INITIAL.
        set_mensagem( '25' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_ck_retorno_sefaz.

    DATA: t_value    TYPE TABLE OF rgsb4,
          l_vzeit    TYPE i,
          l_diftime  TYPE i,
          l_time_ret TYPE numc3.

    CHECK i_status_reg = 'DANF' OR
          i_status_reg = 'DACT' OR
          i_status_reg = 'MDFE'.

    SELECT *
      FROM zlest0242
      INTO TABLE @DATA(t_0242)
     WHERE ch_faturamento   = @i_ch_faturamento
       AND item_faturamento = @i_item_faturamento
       AND status_reg       = @i_status_reg.

    CHECK sy-subrc = 0.

*-------------------------------
*-- checa se status atual é de erro
*-------------------------------
    SORT t_0242           BY seq DESCENDING.
    READ TABLE t_0242   INTO DATA(w_0242) INDEX 1.

    CHECK w_0242-status_msg = 'E'.

*-------------------------------
*-- parametrizacao
*-------------------------------
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr           = 'PARAM_FATURA_AUTOMATICO'
        class           = '0000'
        no_descriptions = abap_off
      TABLES
        set_values      = t_value
      EXCEPTIONS
        set_not_found   = 1
        OTHERS          = 2.

    READ TABLE t_value INTO DATA(w_value) WITH KEY title = 'TEMPO_RETORNO_SEFAZ'.
    CHECK sy-subrc = 0.

    l_time_ret     = w_value-from.

*-------------------------------
*-- busca 1o tentativa de envio a SEFAZ
*-------------------------------
    SORT t_0242           BY seq.
    READ TABLE t_0242   INTO w_0242 INDEX 1.

    CHECK w_0242-data_reg  IS NOT INITIAL AND
          w_0242-hora_reg  IS NOT INITIAL.

*-------------------------------
*-- calcula tempo
*-------------------------------
    CALL FUNCTION 'SALP_SM_CALC_TIME_DIFFERENCE'
      EXPORTING
        date_1  = w_0242-data_reg
        time_1  = w_0242-hora_reg
        date_2  = sy-datum
        time_2  = sy-uzeit
      IMPORTING
        seconds = l_vzeit.

    l_diftime   = l_vzeit / 60.

    IF l_diftime < l_time_ret.
      e_code           = 'ANDAMENTO'.
      e_descricao_code = me->at_t_status[ code = e_code ]-descricao.
      e_mensagem       = w_0242-mensagem.
    ELSE.
      e_code           = 'PAUSADO'.
      e_descricao_code = me->at_t_status[ code = e_code ]-descricao.
      e_mensagem       = 'Excedido tempo médio de retorno de autorização da SEFAZ! Processo de faturamento deve ser concluído de forma manual no SAP.'.
    ENDIF.

  ENDMETHOD.


  METHOD get_ck_tipo_selecao.

    DATA: t_zlest0132       TYPE TABLE OF zlest0132,
          w_zlest0132       TYPE zlest0132,
          t_fatura_agrupada TYPE TABLE OF zsdt0121,
          w_vbak            TYPE vbak,
          w_zsdt0001        TYPE zsdt0001,
          w_vbap            TYPE vbap,
          w_mara            TYPE mara,
          w_fatura_agrupada TYPE zsdt0121.

    CLEAR: w_vbak, w_fatura_agrupada, w_zlest0132, r_cockpit.

    SELECT SINGLE *
      FROM zsdt0001
      INTO w_zsdt0001
     WHERE ch_referencia = i_ch_referencia.

    SELECT SINGLE *
      FROM vbak
      INTO w_vbak
     WHERE vbeln = at_zsdt0001-vbeln.

    SELECT SINGLE *
      FROM vbap
      INTO w_vbap
     WHERE vbeln = at_zsdt0001-vbeln.

    IF w_zsdt0001-matnr IS NOT INITIAL.
      SELECT SINGLE *
        FROM mara
        INTO w_mara
       WHERE matnr = w_zsdt0001-matnr.
    ELSE.
      SELECT SINGLE *
        FROM mara
        INTO w_mara
       WHERE matnr = w_vbap-matnr.
    ENDIF.

    SELECT *
      INTO TABLE t_zlest0132
      FROM zlest0132.

    SELECT *
      FROM zsdt0121
      INTO TABLE t_fatura_agrupada
     WHERE werks = at_zsdt0001-branch.

    SORT: t_zlest0132 BY branch parid.

    READ TABLE t_zlest0132       INTO w_zlest0132       WITH KEY branch = w_zsdt0001-branch
                                                                 parid  = w_zsdt0001-parid
                                                        BINARY SEARCH.

    READ TABLE t_fatura_agrupada INTO w_fatura_agrupada WITH KEY werks = w_zsdt0001-branch
                                                                 matnr = w_zsdt0001-matnr
                                                                 kunnr = w_zsdt0001-id_cli_dest
                                                        BINARY SEARCH.

    IF  ( w_zsdt0001-id_interface NE '48' ) AND
        ( w_zsdt0001-id_interface NE '49' ) AND
        ( w_zsdt0001-id_interface NE '50' ) AND
        ( w_zsdt0001-id_interface NE '51' ) AND
        ( w_zsdt0001-id_interface NE '52' ) AND
        ( w_vbak-spart NE '02' OR
          w_zlest0132  IS INITIAL ).
      r_cockpit = '01'.
      RETURN.
    ENDIF.

    IF w_fatura_agrupada-werks IS NOT INITIAL .
      r_cockpit = '03'.
      RETURN.
    ENDIF.

    IF  ( w_zsdt0001-id_interface NE '49'            ) AND
        ( w_zsdt0001-id_interface NE '51'            ) AND
        ( w_vbak-spart = '02'    OR
          w_mara-spart = '02' ) AND
        ( w_zlest0132 IS NOT INITIAL                    ).
      r_cockpit = '04'.
    ENDIF.

    IF ( w_zsdt0001-id_interface EQ '48' ).
      r_cockpit = '05'.
    ENDIF.

    IF ( w_zsdt0001-id_interface EQ '52' ).
      r_cockpit = '06'.
    ENDIF.

    IF ( w_zsdt0001-id_interface EQ '51' ).
      r_cockpit = '07'.
    ENDIF.

  ENDMETHOD.


  METHOD GET_DADOS_FRETE.

*    DATA: v_cont_ped  TYPE i.
*
*    TYPES: BEGIN OF ty_zsdt0001_aux.
*             INCLUDE TYPE zde_les_zsdt0001.
*    TYPES:   sdabw TYPE a942-sdabw.
*    TYPES: viagem_id TYPE zlest0185-viagem_id.
*    TYPES: END OF ty_zsdt0001_aux.
*
*    DATA: v_kunnr_lr       TYPE kunnr,
*          v_lifnr_z1       TYPE lifnr,
*          v_lifnr_sp       TYPE lifnr,
*          v_lifnr_ori      TYPE lifnr,
*          v_tp_veiculo     TYPE zde_tp_prop_veiculo,
*          v_cont_fre       TYPE i,
*          l_add01(4)       TYPE n,
*          wa_vbak          TYPE ty_vbak,
*          wa_mara          TYPE mara,
*          wa_tvakt         TYPE tvakt,
*          wa_t161t         TYPE t161t,
*          wa_vbkd          TYPE vbkd,
*          wa_tvtk          TYPE tvtk,
*          wa_ekko          TYPE ty_ekko,
*          wa_ekpo          TYPE ty_ekpo,
*          wa_vbpa          TYPE ty_vbpa,
*          wa_vbpa_2        TYPE ty_vbpa,
*          wa_zdco_produtor TYPE zdco_produtor,
*          wa_kna1          TYPE kna1,
*          wa_lfa1          TYPE lfa1,
*          w_lfa1           TYPE lfa1,
*          wa_t001w         TYPE t001w,
*          wa_makt          TYPE makt,
*          wa_zsdt0151      TYPE zsdt0151,
*          wa_t005s         TYPE t005s,
*          wa_vbpa_cr       TYPE vbpa, "Ponto de coleta  REMESSA
*          wa_vbpa_co       TYPE vbpa, "Ponto de coleta  ORDEM
*          wa_ekpa_pr       TYPE ekpa, "Ponto de coleta  Pedido
*          wa_vbap          TYPE vbap, "Itinerário  ORDE M
*          wa_ekpv          TYPE ekpv, "Itinerário  PEDIDO
*          wa_zsdt0001_aux  TYPE ty_zsdt0001_aux,
*          wa_a900          TYPE a900,
*          wa_a910          TYPE a910,
*          wa_a911          TYPE a911,
*          wa_a915          TYPE a915,
*          wa_a918          TYPE a918,
*          wa_a919          TYPE a919,
*          wa_a942          TYPE a942,
*          wa_konp          TYPE konp,
*          _vbeln           TYPE vbeln,
*          _placa_cav       TYPE zplaca,
*          _vlr_frete_neg   TYPE zvalor_frete,
*          _id_ordem        TYPE zde_id_ordem.
*
*    FREE: at_t_a900, at_t_a910, at_t_a911, at_t_a915, at_t_a918, at_t_a919, at_t_konp,
*          r_msg_erro, at_data_response.
*
*    e_status_code   = '200'.
*
*    at_zsdt0001_fre = at_zsdt0001.
*
*    CHECK at_zsdt0001_fre IS NOT INITIAL.
*
*    SORT at_t_ekpv BY ebeln.
*    SORT at_t_ekko BY ebeln.
*    SORT at_t_ekpa_pr BY ebeln.
*    SORT at_t_ekpo BY ebeln.
*    SORT at_t_likp BY vbeln.
*    SORT at_t_vbpa BY vbeln parvw.
*    SORT at_t_vbkd BY vbeln.
*    SORT at_t_vbap BY vbeln.
*    SORT at_t_kna1 BY kunnr.
*    SORT at_t_kna1 BY kunnr.
*    SORT at_t_vbkd BY vbeln.
*
*    CLEAR: v_kunnr_lr, v_lifnr_z1, v_lifnr_sp.
*
*    ASSIGN at_zsdt0001_fre TO FIELD-SYMBOL(<out_zsdt0001>).
*
*    READ TABLE at_t_vbak INTO wa_vbak WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Ordem
*
*    IF sy-subrc = 0.
*      CLEAR: wa_vbkd.
*
*      " Exclui Romaneio do SET
*      READ TABLE at_t_auart INTO DATA(wa_auart) WITH KEY from = wa_vbak-auart BINARY SEARCH.
*      IF sy-subrc = 0.
*        e_status_code = '400'.
*        r_msg_erro    = 'Tipo de Ordem do Romaneio nao esta Parametrizado!'.
*        set_response( i_status_code = e_status_code i_msg_erro = r_msg_erro ).
*        RETURN.
*      ENDIF.
*
*      "ZONA coleta ordem
*      READ TABLE at_t_vbpa_co INTO wa_vbpa_co WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Ordem
*      IF sy-subrc = 0.
*        READ TABLE at_t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa_co-lifnr BINARY SEARCH.
*        <out_zsdt0001>-lzonea = wa_lfa1-lzone.
*      ENDIF.
*
*      "Zona Local de entrega
*      READ TABLE at_t_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
*                                                  parvw = 'LR' BINARY SEARCH.
*      IF sy-subrc = 0.
*        READ TABLE at_t_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr BINARY SEARCH.
*        IF sy-subrc = 0.
*          <out_zsdt0001>-lzonez = wa_kna1-lzone.
*        ENDIF.
*      ENDIF.
**    ENDIF.
*
*      "Agregado
*      SELECT SINGLE agregado
*        FROM zlest0002 INTO @DATA(v_agregado)
*       WHERE pc_veiculo = @<out_zsdt0001>-placa_cav.
*
*      IF sy-subrc = 0.
*        IF v_agregado = 1.
*          <out_zsdt0001>-add01 = '0000000001'.
*        ELSE.
*          <out_zsdt0001>-add01 = '0000000002'.
*        ENDIF.
*      ENDIF.
*
*      "Itinerário
*      READ TABLE at_t_vbap INTO wa_vbap WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Ordem
*      IF sy-subrc = 0.
*        <out_zsdt0001>-route = wa_vbap-route.
*      ENDIF.
*
*      READ TABLE at_t_vbkd INTO wa_vbkd WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH.
*
*      IF ( 'ZRDC_ZRFL_ZIND' CS wa_vbak-auart ) AND ( wa_vbak-auart IS NOT INITIAL ).
*        "Determinar Local de Entrega
*        READ TABLE at_t_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
*                                                  parvw = 'LR' BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          v_kunnr_lr = wa_vbpa-kunnr.
*        ENDIF.
*
*        "Determinar Terminal Porto
*        READ TABLE at_t_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
*                                                  parvw = 'Z1' BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          v_lifnr_z1 = wa_vbpa-lifnr.
*        ENDIF.
*
*        "Determinar Agente Frete
*        READ TABLE at_t_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
*                                                    parvw = 'SP'
*                                                    dlgrp = '0007'. "Multimodal
*        IF sy-subrc = 0.
*          v_lifnr_sp = wa_vbpa-lifnr.
*        ELSEIF ( wa_vbkd-inco1 = 'CIF' ).
*          READ TABLE at_t_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
*                                                      parvw = 'SP' BINARY SEARCH.
*          IF sy-subrc = 0.
*            v_lifnr_sp = wa_vbpa-lifnr.
*            set_troca_agente( EXPORTING i_placa_cav = <out_zsdt0001>-placa_cav
*                                        i_branch    = <out_zsdt0001>-branch
*                              CHANGING  c_lifnr     = v_lifnr_sp ).
*          ENDIF.
*        ENDIF.
*
*        IF  ( v_lifnr_sp IS NOT INITIAL ).
*          <out_zsdt0001>-agente_frete = v_lifnr_sp.
*          set_troca_agente( EXPORTING i_placa_cav = <out_zsdt0001>-placa_cav
*                                      i_branch    = <out_zsdt0001>-branch
*                            CHANGING  c_lifnr     = <out_zsdt0001>-agente_frete ).
*        ENDIF.
*      ENDIF.
*
*      TRY.
*          zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
*            EXPORTING
*              i_tipo_mov       = CONV #( wa_vbak-tp_movimento )
*              i_vsart          = '01'  "Rodoviario
*              i_tipo_ov        = CONV #( wa_vbak-auart )
*              i_parid_lr       = CONV #( v_kunnr_lr )
*              i_parid_z1       = CONV #( v_lifnr_z1 )
*              i_parid_sp       = CONV #( v_lifnr_sp )
*            IMPORTING
*               e_shtyp         = DATA(_shtyp) ).
*
*          <out_zsdt0001>-shtyp = _shtyp.
*        CATCH zcx_faturamento.
*        CATCH zcx_error.
*      ENDTRY.
*
*    ELSE. " Pedido de Compra
*
*      "Itinerário
*      READ TABLE at_t_ekpv INTO wa_ekpv WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Pedido
*      IF sy-subrc = 0.
*        <out_zsdt0001>-route = wa_ekpv-route.
*      ENDIF.
*
*      READ TABLE at_t_ekko INTO wa_ekko WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Pedidos de transferencia
*      IF sy-subrc = 0 .
*        TRY.
*            zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
*              EXPORTING
*                i_tipo_mov       = CONV #( wa_ekko-tp_movimento )
*                i_vsart          = '01'  "Rodoviario
*                i_tipo_pedido    = CONV #( wa_ekko-bsart )
*              IMPORTING
*                 e_shtyp         = _shtyp ).
*
*            <out_zsdt0001>-shtyp = _shtyp.
*          CATCH zcx_faturamento.
*          CATCH zcx_error.
*        ENDTRY.
*        "Comentario Final Codigo - 0002 - 18.04.2019
*      ENDIF.
*
*      READ TABLE at_t_ekpo INTO wa_ekpo WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Pedidos de transferencia
*      IF sy-subrc = 0.
*        IF wa_ekpo-inco1 = 'CIF'.
*          "Agregado
*          SELECT SINGLE agregado
*            FROM zlest0002 INTO v_agregado
*           WHERE pc_veiculo = <out_zsdt0001>-placa_cav.
*          IF sy-subrc = 0.
*            IF v_agregado = 1.
*              <out_zsdt0001>-add01 = '0000000001'.
*            ELSE.
*              <out_zsdt0001>-add01 = '0000000002'.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    "Agente de Frete
*    IF <out_zsdt0001>-agente_frete IS INITIAL.
*      READ TABLE at_t_vbak INTO wa_vbak WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Ordem
*      IF sy-subrc = 0.
*        READ TABLE at_t_vbkd INTO wa_vbkd WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH.
*        IF sy-subrc = 0.
*          IF ( 'ZRFL_ZRDC_ZIND' CS wa_vbak-auart ) AND
*             ( wa_vbkd-inco1 = 'CIF'        ) AND
*             ( wa_vbak-auart IS NOT INITIAL ).
*
*            READ TABLE at_t_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
*                                                        parvw = 'SP' BINARY SEARCH.
*            <out_zsdt0001>-agente_frete           = wa_vbpa-lifnr.
*
*            set_troca_agente( EXPORTING i_placa_cav = <out_zsdt0001>-placa_cav
*                                        i_branch    = <out_zsdt0001>-branch
*                              CHANGING  c_lifnr     = <out_zsdt0001>-agente_frete ).
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    SELECT SINGLE MAX( id_cc )
*     FROM zcarta_correcao INTO @DATA(vl_id)
*    WHERE docnum        EQ @<out_zsdt0001>-nro_nf_prod
*      AND authcode      NE ''
*      AND novo_agente   NE ''.
*
*    IF vl_id GT 0.
*      SELECT SINGLE *
*        FROM zcarta_correcao INTO @DATA(wa_carta)
*       WHERE docnum        EQ @<out_zsdt0001>-nro_nf_prod
*         AND authcode      NE ''
*         AND novo_agente   NE ''
*         AND id_cc         EQ @vl_id.
*      IF sy-subrc = 0.
*        IF wa_carta-novo_agente NE <out_zsdt0001>-agente_frete.
*          <out_zsdt0001>-agente_frete = wa_carta-novo_agente.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
**----------------------------------------------
**-- Calculo do frete
**----------------------------------------------
*    REFRESH at_t_konp.
*
*    "Tp.transp./ForncServ./ItinTransp/Contrato/Agregado
*    SELECT *
*      FROM a900
*      INTO TABLE at_t_a900
*     WHERE kappl EQ 'F'
*       AND kschl EQ 'ZFRE'
*       AND shtyp EQ at_zsdt0001_fre-shtyp
*       AND tdlnr EQ at_zsdt0001_fre-agente_frete
*       AND route EQ at_zsdt0001_fre-route
*       AND add01 EQ at_zsdt0001_fre-add01
*       AND kfrst EQ ''
*       AND datab LE sy-datum
*       AND datbi GE sy-datum
*       AND EXISTS ( SELECT *
*                    FROM  konp
*                    WHERE knumh = a900~knumh
*                    AND   loevm_ko EQ '' ).
*
*    IF at_t_a900[] IS NOT INITIAL.
*      SELECT *
*        FROM konp
*        INTO TABLE at_t_konp
*         FOR ALL ENTRIES IN at_t_a900
*       WHERE knumh = at_t_a900-knumh
*         AND loevm_ko EQ ''.
*    ENDIF.
*
*    " Tp.transp./ForncServ./Zona part./Zona cheg.
*    SELECT *
*     FROM a910
*     INTO TABLE at_t_a910
*    WHERE kappl  EQ 'F'
*      AND kschl  EQ 'ZFRE'
*      AND shtyp  EQ at_zsdt0001_fre-shtyp
*      AND tdlnr  EQ at_zsdt0001_fre-agente_frete
*      AND lzonea EQ at_zsdt0001_fre-lzonea
*      AND lzonez EQ at_zsdt0001_fre-lzonez
*      AND kfrst  EQ ''
*      AND datab  LE sy-datum
*      AND datbi  GE sy-datum
*      AND EXISTS ( SELECT *
*                   FROM  konp
*                   WHERE knumh = a910~knumh
*                   AND   loevm_ko EQ '' ).
*
*    IF at_t_a910[] IS NOT INITIAL.
*      SELECT *
*        FROM konp APPENDING TABLE at_t_konp
*         FOR ALL ENTRIES IN at_t_a910
*       WHERE knumh     = at_t_a910-knumh
*         AND loevm_ko EQ ''.
*    ENDIF.
*
*    "Tp.transp./ForncServ./ItinTransp/Contrato
*    SELECT *
*      FROM a911
*      INTO TABLE at_t_a911
*     WHERE kappl EQ 'F'
*       AND kschl EQ 'ZFRE'
*       AND shtyp EQ at_zsdt0001_fre-shtyp
*       AND tdlnr EQ at_zsdt0001_fre-agente_frete
*       AND route EQ at_zsdt0001_fre-route
*       AND kfrst EQ ''
*       AND datab LE sy-datum
*       AND datbi GE sy-datum
*       AND EXISTS ( SELECT *
*                    FROM  konp
*                    WHERE knumh = a911~knumh
*                    AND   loevm_ko EQ '' ).
*
*    IF at_t_a911[] IS NOT INITIAL.
*      SELECT *
*        FROM konp APPENDING TABLE at_t_konp
*         FOR ALL ENTRIES IN at_t_a911
*       WHERE knumh     = at_t_a911-knumh
*         AND loevm_ko EQ ''.
*    ENDIF.
*
*    " Tp.transp./ForncServ./Zona part./Zona cheg./Agregado
*    SELECT *
*     FROM a915
*     INTO TABLE at_t_a915
*    WHERE kappl  EQ 'F'
*      AND kschl  EQ 'ZFRE'
*      AND shtyp  EQ at_zsdt0001_fre-shtyp
*      AND tdlnr  EQ at_zsdt0001_fre-agente_frete
*      AND lzonea EQ at_zsdt0001_fre-lzonea
*      AND lzonez EQ at_zsdt0001_fre-lzonez
*      AND add01  EQ at_zsdt0001_fre-add01
*      AND kfrst  EQ ''
*      AND datab  LE sy-datum
*      AND datbi  GE sy-datum
*      AND EXISTS ( SELECT *
*                   FROM  konp
*                   WHERE knumh = a915~knumh
*                   AND   loevm_ko EQ '' ).
*
*    IF at_t_a915[] IS NOT INITIAL.
*      SELECT *
*        FROM konp APPENDING TABLE at_t_konp
*         FOR ALL ENTRIES IN at_t_a915
*       WHERE knumh = at_t_a915-knumh
*         AND loevm_ko EQ ''.
*    ENDIF.
*
*    " Tp.transp./ForncServ./Material/Zona part./Zona cheg./Suplem.
*    SELECT *
*     FROM a918
*     INTO TABLE at_t_a918
*    WHERE kappl  EQ 'F'
*      AND kschl  EQ 'ZFRE'
*      AND shtyp  EQ at_zsdt0001_fre-shtyp
*      AND tdlnr  EQ at_zsdt0001_fre-agente_frete
*      AND matnr  EQ at_zsdt0001_fre-matnr
*      AND lzonea EQ at_zsdt0001_fre-lzonea
*      AND lzonez EQ at_zsdt0001_fre-lzonez
*      AND add01  EQ at_zsdt0001_fre-add01
*      AND kfrst  EQ ''
*      AND datab  LE sy-datum
*      AND datbi  GE sy-datum
*      AND  EXISTS ( SELECT *
*                    FROM  konp
*                    WHERE knumh = a918~knumh
*                    AND   loevm_ko EQ '' ).
*
*    IF at_t_a918[] IS NOT INITIAL.
*      SELECT *
*        FROM konp APPENDING TABLE at_t_konp
*         FOR ALL ENTRIES IN at_t_a918
*       WHERE knumh     = at_t_a918-knumh
*         AND loevm_ko EQ ''.
*    ENDIF.
*
*    "Tp.transp./ForncServ./Material/Zona part./Zona cheg.
*    SELECT *
*      FROM a919
*      INTO TABLE at_t_a919
*     WHERE kappl  EQ 'F'
*       AND kschl  EQ 'ZFRE'
*       AND shtyp  EQ at_zsdt0001_fre-shtyp
*       AND tdlnr  EQ at_zsdt0001_fre-agente_frete
*       AND matnr  EQ at_zsdt0001_fre-matnr
*       AND lzonea EQ at_zsdt0001_fre-lzonea
*       AND lzonez EQ at_zsdt0001_fre-lzonez
*       AND kfrst  EQ ''
*       AND datab  LE sy-datum
*       AND datbi  GE sy-datum
*       AND  EXISTS ( SELECT *
*                       FROM konp
*                      WHERE knumh = a919~knumh
*                        AND loevm_ko EQ '' ).
*
*    IF at_t_a919[] IS NOT INITIAL.
*      SELECT *
*        FROM konp APPENDING TABLE at_t_konp
*         FOR ALL ENTRIES IN at_t_a919
*       WHERE knumh     = at_t_a919-knumh
*         AND loevm_ko EQ ''.
*    ENDIF.
*
*    CLEAR wa_zsdt0001_aux.
*
*    MOVE-CORRESPONDING at_zsdt0001_fre TO wa_zsdt0001_aux.
*    MOVE at_zsdt0001_fre-add01         TO l_add01.
*    MOVE l_add01                       TO wa_zsdt0001_aux-sdabw.
*
*    SELECT SINGLE viagem_id
*      INTO wa_zsdt0001_aux-viagem_id
*      FROM zlest0185
*     WHERE id_ordem EQ at_zsdt0001_fre-id_ordem.
*
*    "Tp.transp./ForncServ./Material/Zona part./Zona cheg.
*    SELECT *
*      FROM a942
*      INTO TABLE at_t_a942
*     WHERE kappl     EQ 'F'
*       AND kschl     EQ 'ZFRE'
*       AND shtyp     EQ wa_zsdt0001_aux-shtyp
*       AND sdabw     EQ wa_zsdt0001_aux-sdabw
*       AND id_viagem EQ wa_zsdt0001_aux-viagem_id
*       AND kfrst     EQ ''
*       AND datab     LE sy-datum
*       AND datbi     GE sy-datum
*       AND EXISTS ( SELECT *
*                      FROM konp
*                     WHERE knumh = a942~knumh
*                       AND loevm_ko EQ '' ).
*
*    IF at_t_a942[] IS NOT INITIAL.
*      SELECT *
*        FROM konp APPENDING TABLE at_t_konp
*         FOR ALL ENTRIES IN at_t_a942
*       WHERE knumh     = at_t_a942-knumh
*         AND loevm_ko EQ ''.
*    ENDIF.
*
*    SORT: at_t_a900 BY shtyp tdlnr route add01,
*          at_t_a910 BY shtyp tdlnr lzonea lzonez,
*          at_t_a911 BY shtyp tdlnr route,
*          at_t_a915 BY shtyp tdlnr lzonea lzonez add01,
*          at_t_a918 BY shtyp tdlnr matnr lzonea lzonez add01,
*          at_t_a919 BY shtyp tdlnr matnr lzonea lzonez,
*          at_t_a942 BY shtyp sdabw id_viagem,
*          at_t_konp BY knumh.
*
**----------------------------------------------
**-- mover estrutura
**----------------------------------------------
*    at_zsdt0001 = at_zsdt0001_fre.
*
**----------------------------------------------
**-- Valor do Frete
**----------------------------------------------
*    LOOP AT at_t_a900 INTO wa_a900 WHERE shtyp = at_zsdt0001-shtyp
*                                     AND tdlnr = at_zsdt0001-agente_frete
*                                     AND route = at_zsdt0001-route
*                                     AND add01 = at_zsdt0001-add01.
*      READ TABLE at_t_konp INTO wa_konp WITH KEY knumh = wa_a900-knumh BINARY SEARCH.
*      IF sy-subrc = 0.
*        at_saida-kbetr = wa_konp-kbetr.
*        at_saida-konwa = wa_konp-konwa.
*        at_saida-krech = wa_konp-krech.
*        ADD 1 TO v_cont_fre.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT at_t_a910 INTO wa_a910 WHERE shtyp  = at_zsdt0001-shtyp
*                                     AND tdlnr  = at_zsdt0001-agente_frete
*                                     AND lzonea = at_zsdt0001-lzonea
*                                     AND lzonez = at_zsdt0001-lzonez.
*      READ TABLE at_t_konp INTO wa_konp WITH KEY knumh = wa_a910-knumh BINARY SEARCH.
*      IF sy-subrc = 0.
*        at_saida-kbetr = wa_konp-kbetr.
*        at_saida-konwa = wa_konp-konwa.
*        at_saida-krech = wa_konp-krech.
*        ADD 1 TO v_cont_fre.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT at_t_a911 INTO wa_a911 WHERE shtyp = at_zsdt0001-shtyp
*                                     AND tdlnr = at_zsdt0001-agente_frete
*                                     AND route = at_zsdt0001-route.
*      READ TABLE at_t_konp INTO wa_konp WITH KEY knumh = wa_a911-knumh BINARY SEARCH.
*      IF sy-subrc = 0.
*        at_saida-kbetr = wa_konp-kbetr.
*        at_saida-konwa = wa_konp-konwa.
*        at_saida-krech = wa_konp-krech.
*        ADD 1 TO v_cont_fre.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT at_t_a915 INTO wa_a915 WHERE shtyp  = at_zsdt0001-shtyp
*                                     AND tdlnr  = at_zsdt0001-agente_frete
*                                     AND lzonea = at_zsdt0001-lzonea
*                                     AND lzonez = at_zsdt0001-lzonez
*                                     AND add01  = at_zsdt0001-add01.
*      READ TABLE at_t_konp INTO wa_konp WITH KEY knumh = wa_a915-knumh BINARY SEARCH.
*      IF sy-subrc = 0.
*        at_saida-kbetr = wa_konp-kbetr.
*        at_saida-konwa = wa_konp-konwa.
*        at_saida-krech = wa_konp-krech.
*        ADD 1 TO v_cont_fre.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT at_t_a918 INTO wa_a918 WHERE shtyp  = at_zsdt0001-shtyp
*                                     AND tdlnr  = at_zsdt0001-agente_frete
*                                     AND matnr  = at_zsdt0001-matnr
*                                     AND lzonea = at_zsdt0001-lzonea
*                                     AND lzonez = at_zsdt0001-lzonez
*                                     AND add01  = at_zsdt0001-add01.
*      READ TABLE at_t_konp INTO wa_konp WITH KEY knumh = wa_a918-knumh BINARY SEARCH.
*      IF sy-subrc = 0.
*        at_saida-kbetr = wa_konp-kbetr.
*        at_saida-konwa = wa_konp-konwa.
*        at_saida-krech = wa_konp-krech.
*        ADD 1 TO v_cont_fre.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT at_t_a919 INTO wa_a919 WHERE shtyp  = at_zsdt0001-shtyp
*                                     AND tdlnr  = at_zsdt0001-agente_frete
*                                     AND matnr  = at_zsdt0001-matnr
*                                     AND lzonea = at_zsdt0001-lzonea
*                                     AND lzonez = at_zsdt0001-lzonez.
*      READ TABLE at_t_konp INTO wa_konp WITH KEY knumh = wa_a919-knumh BINARY SEARCH.
*      IF sy-subrc = 0.
*        at_saida-kbetr = wa_konp-kbetr.
*        at_saida-konwa = wa_konp-konwa.
*        at_saida-krech = wa_konp-krech.
*        ADD 1 TO v_cont_fre.
*      ENDIF.
*    ENDLOOP.
*
*    IF wa_konp-krech = 'A'. "Percentual
*      at_saida-kbetr = at_saida-kbetr / 10.
*    ENDIF.
*
*    LOOP AT at_t_a942 INTO wa_a942 WHERE shtyp     = wa_zsdt0001_aux-shtyp
*                                     AND sdabw     = wa_zsdt0001_aux-sdabw
*                                     AND id_viagem = wa_zsdt0001_aux-viagem_id.
*      READ TABLE at_t_konp INTO wa_konp WITH KEY knumh = wa_a942-knumh BINARY SEARCH.
*      IF sy-subrc = 0.
*        at_saida-kbetr = wa_konp-kbetr.
*        at_saida-konwa = wa_konp-konwa.
*        at_saida-krech = wa_konp-krech.
*        MOVE 1 TO v_cont_fre.
*      ENDIF.
*    ENDLOOP.
*
*    "Check Alteração Preço por Solicitação - Transação ZLES0153 - CS2016001693
*    IF ( at_saida-tipo = 'O' ) AND ( at_zsdt0001-vbeln IS NOT INITIAL ).
*      _vbeln         = at_zsdt0001-vbeln.
*      _placa_cav     = at_zsdt0001-placa_cav.
*      CALL FUNCTION 'ZLES_VALOR_FRETE_ORDEM_CAR'
*        EXPORTING
*          i_vbeln         = _vbeln
*          i_placa_cav     = _placa_cav
*          i_id_ordem      = at_zsdt0001-id_ordem
*          i_shtyp         = at_saida-shtyp
*        IMPORTING
*          e_vlr_frete_neg = _vlr_frete_neg
*          e_id_ordem      = _id_ordem.
*
*      IF _vlr_frete_neg > 0.
*        at_saida-kbetr    = _vlr_frete_neg. "Atribuir Valor de Frete Negociado
*        at_saida-id_ordem = _id_ordem.
*      ENDIF.
*    ENDIF.
*
*    at_saida-cont_fre = v_cont_fre.

  ENDMETHOD.


  METHOD GET_ID_REFERENCIA.

    e_referencia-tp_referencia = me->at_tp_referencia.
    e_referencia-id_referencia = me->at_id_referencia.

  ENDMETHOD.


  METHOD get_obter_doctos_romaneio.

    DATA: t_pdf_files        TYPE zsdt_pdf_files,
          w_pdf_files        TYPE zsde_pdf_files,
          t_doctos_faltantes TYPE zsdt_doctos_faltantes,
          w_doctos_faltantes TYPE zsde_doctos_faltantes,
          l_merged_document  TYPE xstring,
          l_naotem_doc       TYPE char01.

    FREE: r_documentos.

    SELECT SINGLE *
      INTO @DATA(_zlest0241)
      FROM zlest0241
     WHERE ch_faturamento   = @i_ch_faturamento
       AND item_faturamento = @i_item_faturamento
       AND cancelado        = @abap_off.

    CHECK sy-subrc = 0.

*-----------------------------------------
* obtem documentos faturamento
*-----------------------------------------
    TRY.
        t_pdf_files = zcl_faturamento=>zif_faturamento~get_instance(
                        )->get_documentos_faturamento( EXPORTING i_ch_referencia  = _zlest0241-ch_referencia
                        ).
      CATCH zcx_faturamento.
      CATCH zcx_error.
    ENDTRY.

*--------------------------------------------
* valida arquivos obrigatorios
*--------------------------------------------
    TRY.
        l_naotem_doc = zcl_faturamento=>zif_faturamento~get_instance(
                         )->get_documentos_obrigatorios( EXPORTING i_ch_referencia    = _zlest0241-ch_referencia
                                                                   t_pdf_files        = t_pdf_files
                                                         IMPORTING t_doctos_faltantes = t_doctos_faltantes
                         ).
      CATCH zcx_faturamento.
      CATCH zcx_error.
    ENDTRY.

    LOOP AT t_doctos_faltantes INTO w_doctos_faltantes.
      READ TABLE t_pdf_files INTO w_pdf_files WITH KEY tipo_doc = w_doctos_faltantes-tipo_doc.
      IF sy-subrc = 0.
        DELETE t_pdf_files INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

*-----------------------------------------
* agrupa documentos
*-----------------------------------------
    TRY.
        l_merged_document = zcl_faturamento=>zif_faturamento~get_instance(
                              )->get_merge_pdf( EXPORTING t_pdf_files = t_pdf_files
                              ).

      CATCH zcx_faturamento.
      CATCH zcx_error.
    ENDTRY.

    r_documentos = l_merged_document.

  ENDMETHOD.


  METHOD get_protocolo.

    DATA: l_seq TYPE char11.

    FREE: r_nr_protocolo.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZLES_FATUR'
        toyear      = sy-datum(4)
      IMPORTING
        number      = l_seq.

    IF sy-subrc <> 0.
      set_mensagem( '15' ).
    ENDIF.

    r_nr_protocolo = sy-datum(4) && l_seq.

  ENDMETHOD.


  METHOD get_romaneio_finalizado.

    FREE: r_finalizado.

    SELECT SINGLE finalizado
      INTO @DATA(_finalizado)
      FROM zlest0241
     WHERE ch_faturamento   = @i_ch_faturamento
       AND item_faturamento = @i_item_faturamento
       AND cancelado        = @abap_off.

    CHECK sy-subrc = 0.

    r_finalizado = _finalizado.

  ENDMETHOD.


  METHOD get_valor_frete.

    DATA: l_kbetr	      TYPE kbetr_kond,
          l_kbetr_zahd  TYPE kbetr_kond,
          l_kbetr_zhi1  TYPE kbetr_kond,
          l_konwa       TYPE konwa,
          l_krech       TYPE krech,
          l_auart       TYPE auart,
          l_werks       TYPE werks_ext,
          l_inco1       TYPE inco1,
          l_shtyp       TYPE shtyp,
          l_centro_real TYPE werks_d,
          l_empr_emite  TYPE vkorg,
          l_empr_cte    TYPE vkorg,
          l_viagem_id   TYPE zlest0185-viagem_id,     "*-BUG 155683-18.10.1024-JT-inicio
          l_doc_type    TYPE auart.

    FREE: e_valor_frete, e_krech, e_moeda_frete, l_viagem_id.  "*-BUG 155683-18.10.1024-JT-inicio

*-BUG 155683-18.10.1024-JT-inicio
*-- viagem
    IF i_zsdt0001-id_ordem IS NOT INITIAL.
      SELECT SINGLE viagem_id
        INTO l_viagem_id
        FROM zlest0185
       WHERE id_ordem = i_zsdt0001-id_ordem.
    ENDIF.
*-BUG 155683-18.10.1024-JT-fim

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    IF l_viagem_id IS INITIAL AND i_zsdt0001-id_interface EQ '48' AND i_zsdt0001-nro_cg IS NOT INITIAL.
      SELECT SINGLE viagem_id
        FROM zsdt0133 INTO l_viagem_id
       WHERE nro_cg = i_zsdt0001-nro_cg.
    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

*--------------------------
*-- valor do frete
*--------------------------
    TRY .
        zcl_calc_frete=>get_valor_frete(
          EXPORTING
            i_route        = i_zsdt0001-route        " Itinerário
            i_tdlnr        = i_zsdt0001-agente_frete " Nº do agente de frete
            i_shtyp        = i_zsdt0001-shtyp        " Tipo de transporte
            i_lzonea       = i_zsdt0001-lzonea       " Zona de partida
            i_lzonez       = i_zsdt0001-lzonez       " Zona de chegada
            i_add01        = i_zsdt0001-add01        " Suplem.1
            i_matnr        = i_zsdt0001-matnr        " Nº do material
            i_ordem_venda  = i_zsdt0001-vbeln        " Nº documento de vendas e distribuição
            i_placa_trator = i_zsdt0001-placa_cav    " Placa Veículo Tração
            i_viagem_id    = l_viagem_id             " Viagem     "*-BUG 155683-18.10.1024-JT-inicio
          IMPORTING
            e_kbetr        = l_kbetr                 " Montante/porcentagem de condição no caso de não haver escala
            e_konwa        = l_konwa                 " Unidade de condição (moeda ou porcentagem)
            e_krech        = l_krech ).              " Regra de cálculo de condição

      CATCH zcx_calc_frete INTO DATA(ex_calc_frete).    "
        MESSAGE ID ex_calc_frete->msgid TYPE 'S' NUMBER ex_calc_frete->msgno WITH ex_calc_frete->msgv1 ex_calc_frete->msgv2 ex_calc_frete->msgv3 ex_calc_frete->msgv4 INTO DATA(l_mesg).
        set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
*       set_mensagem( '13' ).
    ENDTRY.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    e_krech = l_krech.

    CASE l_krech.
      WHEN 'B'. "Montante Fixo
        e_valor_frete = l_kbetr.
      WHEN OTHERS.
        e_valor_frete = ( i_zsdt0001-peso_liq / 1000 ) * l_kbetr.
    ENDCASE.

    "e_valor_frete = ( i_zsdt0001-peso_liq / 1000 ) * l_kbetr.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----
    e_moeda_frete = l_konwa.

*--------------------------
*-- tipo de ordem
*--------------------------
    IF i_zsdt0001-vbeln IS NOT INITIAL.
      SELECT SINGLE auart
        INTO l_auart
        FROM vbak
       WHERE vbeln = i_zsdt0001-vbeln.

      SELECT SINGLE werks
        INTO l_werks
        FROM vbap
       WHERE vbeln = i_zsdt0001-vbeln.

      SELECT SINGLE inco1
        INTO l_inco1
        FROM vbkd
       WHERE vbeln = i_zsdt0001-vbeln.

      SELECT SINGLE shtyp
        INTO l_shtyp
        FROM zsdt0011
       WHERE auart = l_auart.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      IF i_zsdt0001-id_interface EQ '48' AND i_zsdt0001-nro_cg IS NOT INITIAL.
        CALL METHOD zcl_carga_saida_insumos=>get_agente_frete
          EXPORTING
            i_bukrs  = i_zsdt0001-bukrs
            i_branch = i_zsdt0001-branch
            i_nro_cg = i_zsdt0001-nro_cg
          IMPORTING
            e_inco1  = l_inco1.
      ENDIF.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----


    ELSEIF i_zsdt0001-ebeln IS NOT INITIAL.
      SELECT SINGLE bsart
        INTO l_auart
        FROM ekko
       WHERE ebeln = i_zsdt0001-ebeln.

      SELECT SINGLE werks inco1
        INTO (l_werks,l_inco1)
        FROM ekpo
       WHERE ebeln = i_zsdt0001-ebeln.

      SELECT SINGLE shtyp
        INTO l_shtyp
        FROM zsdt0011
       WHERE bsart = l_auart.
    ENDIF.

    CHECK l_auart IS NOT INITIAL AND
          l_werks IS NOT INITIAL AND
          l_inco1 IS NOT INITIAL AND
          l_shtyp IS NOT INITIAL.

*--------------------------
*-- determina centro real
*--------------------------
    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro               = l_werks
      IMPORTING
        centro_real          = l_centro_real
      EXCEPTIONS
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        informar_centro_v    = 4
        OTHERS               = 5.

*--------------------------
*-- tipo OV do frete
*--------------------------
    SELECT SINGLE vkorg
      FROM tvkwz
      INTO l_empr_emite
     WHERE werks = l_centro_real.

    SELECT SINGLE vkorg
      FROM tvkwz
      INTO l_empr_cte
     WHERE werks = i_zsdt0001-agente_frete+6(4).

    IF  l_empr_emite <> l_empr_cte.
      l_doc_type = 'ZTRH'.
    ELSEIF ( l_empr_emite = l_empr_cte ) AND ( l_shtyp <> 'Z026' ).
      l_doc_type = 'ZTRO'.
    ELSEIF ( l_empr_emite = l_empr_cte ) AND ( l_shtyp  = 'Z026' ).
      l_doc_type = 'ZTRT'.
    ENDIF.

    CHECK l_inco1 = 'CIF' AND l_doc_type = 'ZTRH'.

*--------------------------
*---- vaLor condicao ZADH
*--------------------------
    TRY .
        zcl_calc_frete=>get_condicao_adicional(
          EXPORTING
            i_kschl        = 'ZADH'                   " Tipo de condição
            i_tdlnr        = i_zsdt0001-agente_frete  " Nº do agente de frete
            i_shtyp        = l_shtyp                  " Tipo de transporte
            i_tplst        = l_centro_real            " Local de organização de transportes
          IMPORTING
            e_kbetr        = l_kbetr_zahd             " Montante/porcentagem de condição no caso de não haver escala
            e_konwa        = l_konwa                  " Unidade de condição (moeda ou porcentagem)
            e_krech        = l_krech ).               " Regra de cálculo de condição

      CATCH zcx_calc_frete INTO ex_calc_frete.
        MESSAGE ID ex_calc_frete->msgid TYPE 'S' NUMBER ex_calc_frete->msgno WITH ex_calc_frete->msgv1 ex_calc_frete->msgv2 ex_calc_frete->msgv3 ex_calc_frete->msgv4 INTO l_mesg.
        set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
    ENDTRY.

*--------------------------
*---- vaLor condicao ZHI1
*--------------------------
    TRY .
        zcl_calc_frete=>get_condicao_adicional(
          EXPORTING
            i_kschl        = 'ZHI1'                   " Tipo de condição
            i_tdlnr        = i_zsdt0001-agente_frete  " Nº do agente de frete
            i_shtyp        = l_shtyp                  " Tipo de transporte
            i_tplst        = l_centro_real            " Local de organização de transportes
          IMPORTING
            e_kbetr        = l_kbetr_zhi1             " Montante/porcentagem de condição no caso de não haver escala
            e_konwa        = l_konwa                  " Unidade de condição (moeda ou porcentagem)
            e_krech        = l_krech ).               " Regra de cálculo de condição

      CATCH zcx_calc_frete INTO ex_calc_frete.
        MESSAGE ID ex_calc_frete->msgid TYPE 'S' NUMBER ex_calc_frete->msgno WITH ex_calc_frete->msgv1 ex_calc_frete->msgv2 ex_calc_frete->msgv3 ex_calc_frete->msgv4 INTO l_mesg.
        set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
    ENDTRY.

    l_kbetr_zahd  = abs( l_kbetr_zahd / 10 ).
    l_kbetr_zhi1  =      l_kbetr_zhi1 / 10.

    l_kbetr_zahd  = (   e_valor_frete * l_kbetr_zahd ) / 100.
    l_kbetr_zhi1  = ( ( e_valor_frete - l_kbetr_zahd ) * l_kbetr_zhi1 ) / 100.

    e_valor_frete = e_valor_frete - l_kbetr_zahd + l_kbetr_zhi1.

  ENDMETHOD.


  METHOD set_atualizar_status.

    DATA(l_finalizado) = COND #( WHEN i_status = 'FINA' THEN abap_true
                                                        ELSE abap_false ).

    UPDATE zlest0241 SET finalizado            = l_finalizado
                         status_reg            = i_status
                   WHERE ch_faturamento        = me->at_w_zlest0241-ch_faturamento
                     AND item_faturamento      = me->at_w_zlest0241-item_faturamento
                     AND cancelado             = abap_off.

  ENDMETHOD.


  METHOD set_consulta_faturamento.

    DATA: _status_code  TYPE char03,
          _msg_erro     TYPE string,
          _msg_outbound TYPE string,
          t_status      TYPE zlese0232_t,
          w_status      TYPE zlese0232,
          t_romaneio    TYPE zlese0231_t,
          w_romaneio    TYPE zlese0231.

    r_if_integracao_inject = me.

    FREE: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, at_data_request, at_retorno_opus, t_status, t_romaneio.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = at_request_faturamento ).
    ENDIF.

*------------------------------------
*-- validar dados
*------------------------------------
    _status_code = '200'.
    _msg_erro    = abap_off.

    SELECT SINGLE *
      FROM zlest0241
      INTO @DATA(_zlest0241)
     WHERE ch_faturamento = @at_request_faturamento-protocolo
       AND cancelado      = @abap_false.

    IF sy-subrc <> 0.
      w_status-mensagem            = 'Faturamento não Localizado!'.
      APPEND w_status             TO t_status.
      w_romaneio-code              = 'ERRO'.  "'400'.                                  "*-#157580-06.11.2024-JT-inicio
      w_romaneio-descricao_code    = at_t_status[ code = w_romaneio-code ]-descricao.  "*-#157580-06.11.2024-JT-inicio
      w_romaneio-status            = t_status[].
      APPEND w_romaneio           TO t_romaneio.

      _status_code                 = w_romaneio-code.
      _msg_erro                    = w_status-mensagem.
      me->at_retorno_opus-romaneio = t_romaneio[].
      _msg_outbound                = /ui2/cl_json=>serialize( EXPORTING data        = me->at_retorno_opus
                                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    ELSE.
      me->set_retorno_opus( EXPORTING i_ch_faturamento   = _zlest0241-ch_faturamento
                                      i_consulta_opus    = abap_true
                                      i_permite_cancelar = at_request_faturamento-permite_cancelar "*-#166749-20.02.2025-JT-inicio
                            IMPORTING e_sucesso          = e_sucesso
                                      e_nm_code          = _status_code
                                      e_msg_erro         = _msg_erro
                            RECEIVING r_retorno          = _msg_outbound ).
    ENDIF.

*------------------------------------
*-- resposta
*------------------------------------
    e_sucesso      = abap_true.
    e_nm_code      = '200'.  "_status_code.
    e_msg_erro     = _msg_erro.
    e_msg_outbound = _msg_outbound.

  ENDMETHOD.


  METHOD SET_DS_DATA.

*---------------------------------------
*---types
*---------------------------------------
    TYPES BEGIN OF ty_retorno.
    TYPES: access_token TYPE string.
    TYPES: expires_in TYPE string.
    TYPES: token_type TYPE string.
    TYPES END OF ty_retorno.

*---------------------------------------
*---workarea
*---------------------------------------
    DATA: l_access_token TYPE string,
          l_token_type   TYPE string,
          l_expires_in   TYPE string,
          l_token        TYPE string,
          l_json         TYPE string,
          lc_retorno     TYPE ty_retorno.

    FREE: me->zif_integracao_inject~at_header_fields,
          me->zif_integracao_inject~at_form_fields.

    CASE me->at_metodo.
      WHEN 'TOKEN'.
*        l_json = 'grant_type='     && me->zif_trace_cotton~at_webservice-username &&
*                 '&client_id='     && me->zif_trace_cotton~at_webservice-password &&
*                 '&client_secret=' && me->zif_trace_cotton~at_webservice-add01    &&
*                 '&resource='      && me->zif_trace_cotton~at_webservice-add02.
*
        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/x-www-form-urlencoded'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = l_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.

      WHEN 'RETORNO_OPUS'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access_token.
        l_token_type   = lc_retorno-token_type.
        l_expires_in   = lc_retorno-expires_in.
        l_token        = |{ l_token_type } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )   TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->at_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'PUT'.

    ENDCASE.

  ENDMETHOD.


  METHOD SET_DS_URL.

    DATA: l_url     TYPE string,
          l_data    TYPE string,
          l_ativo   TYPE string,
          l_servico TYPE string.

    CASE i_metodo.
      WHEN 'TOKEN'.
        l_servico = 'FAT_AUTOMAT_TOKEN'.
      WHEN 'RETORNO_OPUS'.
        l_servico = 'FAT_AUTOMAT_RETORNO_OPUS'.
    ENDCASE.

    SELECT SINGLE *
             FROM zauth_webservice
             INTO @DATA(wa_webservice)
            WHERE service = @l_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'T'
                            attr2 = 'TC' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'T'
          msgv2  = 'TC'.
    ENDIF.

    me->at_webservice = wa_webservice.
    me->at_metodo     = i_metodo.

    CASE i_metodo.
      WHEN 'TOKEN'.
        l_url = wa_webservice-url.
      WHEN 'RETORNO_OPUS'.
        l_url = wa_webservice-url.
    ENDCASE.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
*   me->zif_integracao_inject~at_info_request_http-ds_content_type     = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token        = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url              = l_url.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->set_id_referencia( ).

  ENDMETHOD.


  METHOD set_emitir_documentos.

    DATA: l_mesg TYPE string.

    CLEAR: me->at_w_zlest0241.

*--------------------------
*-- buscar documento
*--------------------------
    SELECT SINGLE *
      FROM zlest0240
      INTO me->at_w_zlest0240
     WHERE ch_faturamento   = i_ch_faturamento
       AND cancelado        = abap_off.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM zlest0241
      INTO me->at_w_zlest0241
     WHERE ch_faturamento   = i_ch_faturamento
       AND item_faturamento = i_item_faturamento
       AND cancelado        = abap_off.

    CHECK sy-subrc = 0.

    me->at_ch_faturamento   = me->at_w_zlest0241-ch_faturamento.
    me->at_item_faturamento = me->at_w_zlest0241-item_faturamento.
    me->at_ch_referencia    = me->at_w_zlest0241-ch_referencia.

*--------------------------
*-- inicio faturamento
*--------------------------
    IF me->at_w_zlest0240-origem = '1' AND  ( me->at_w_zlest0240-cockpit = '01'  OR
                                              me->at_w_zlest0240-cockpit = '03'  OR
                                              me->at_w_zlest0240-cockpit = '05'  OR
                                              me->at_w_zlest0240-cockpit = '06'  OR
                                              me->at_w_zlest0240-cockpit = '07'  OR
                                              me->at_w_zlest0240-cockpit = '09'  OR
                                              me->at_w_zlest0240-cockpit = '10' ).
*--------------------------
*---- registra inicio
*--------------------------
      DATA(l_mensg) = 'Inicio Processamento'.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = CONV #( l_mensg ) i_status = 'AGUA' ).
      me->set_atualizar_status( 'AGUA' ).

*--------------------------
*---- gerar remessa
*--------------------------
      TRY.
          me->set_gerar_remessa( ).

        CATCH zcx_error INTO DATA(ex_error).
          l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
          me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_nro = ex_error->msgno i_type = 'E' i_msg = l_mesg i_status = 'REME' ).
          me->set_retorno_opus( i_ch_faturamento = me->at_w_zlest0241-ch_faturamento ).
          RETURN.
      ENDTRY.

*--------------------------
*---- gerar fatura
*--------------------------
      TRY.
          me->set_gerar_fatura( ).

        CATCH zcx_error INTO ex_error.
          l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
          me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_nro = ex_error->msgno i_type = 'E' i_msg = l_mesg i_status = 'FATU' ).
          me->set_retorno_opus( i_ch_faturamento = me->at_w_zlest0241-ch_faturamento ).
          RETURN.
      ENDTRY.

*--------------------------
*---- gerar danfe
*--------------------------
      TRY.
          me->set_gerar_danfe( ).

        CATCH zcx_error INTO ex_error.
          l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
          me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_nro = ex_error->msgno i_type = 'E' i_msg = l_mesg i_status = 'DANF' ).
          me->set_retorno_opus( i_ch_faturamento = me->at_w_zlest0241-ch_faturamento ).
          RETURN.
      ENDTRY.

*--------------------------
*-- gerar doc.transporte
*--------------------------
      TRY.
          me->set_gerar_transporte( ).

        CATCH zcx_error INTO ex_error.
          l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
          me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_nro = ex_error->msgno i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
          me->set_retorno_opus( i_ch_faturamento = me->at_w_zlest0241-ch_faturamento ).
          RETURN.
      ENDTRY.

*--------------------------
*---- gerar doc.custos
*--------------------------
      TRY.
          me->set_gerar_documentos_custos( ).

        CATCH zcx_error INTO ex_error.
          l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
          me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_nro = ex_error->msgno i_type = 'E' i_msg = l_mesg i_status = 'DCUS' ).
          me->set_retorno_opus( i_ch_faturamento = me->at_w_zlest0241-ch_faturamento ).
          RETURN.
      ENDTRY.

*--------------------------
*---- criar viagem
*--------------------------
      TRY.
          me->set_gerar_viagem( ).

        CATCH zcx_error INTO ex_error.
          l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
          me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_nro = ex_error->msgno i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
          me->set_retorno_opus( i_ch_faturamento = me->at_w_zlest0241-ch_faturamento ).
          RETURN.
      ENDTRY.

*--------------------------
*---- Autorizar dacte
*--------------------------
      TRY.
          me->set_gerar_dacte( ).

        CATCH zcx_error INTO ex_error.
          l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
          me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_nro = ex_error->msgno i_type = 'E' i_msg = l_mesg i_status = 'DACT' ).
          me->set_retorno_opus( i_ch_faturamento = me->at_w_zlest0241-ch_faturamento ).
          RETURN.
      ENDTRY.

*--------------------------
*---- GErar MDF-e
*--------------------------
      TRY.
          me->set_gerar_mdfe( ).

        CATCH zcx_error INTO ex_error.
          l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
          me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_nro = ex_error->msgno i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
          me->set_retorno_opus( i_ch_faturamento = me->at_w_zlest0241-ch_faturamento ).
          RETURN.
      ENDTRY.

*--------------------------
*---- Solicitar credito viagem
*--------------------------
      TRY.
          me->set_gerar_credito_viagem( ).

        CATCH zcx_error INTO ex_error.
          l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
          me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_nro = ex_error->msgno i_type = 'E' i_msg = l_mesg i_status = 'APRV' ).
          me->set_retorno_opus( i_ch_faturamento = me->at_w_zlest0241-ch_faturamento ).
          RETURN.
      ENDTRY.

*-------------------------
*---- verifica romaneio finalizado
*-------------------------
      me->set_finalizar_romaneio( i_ch_faturamento   = me->at_w_zlest0241-ch_faturamento
                                  i_item_faturamento = me->at_w_zlest0241-item_faturamento ).

*------------------------------------
*---- enviar mensagem ao OPUS
*------------------------------------
      TRY.
          me->set_retorno_opus(   i_ch_faturamento   = me->at_w_zlest0241-ch_faturamento ).
        CATCH zcx_integracao.
        CATCH zcx_error.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD set_executar_faturamento.

    DATA: _status_code TYPE char03,
          _msg_erro    TYPE string.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, at_data_request, at_data_response, at_data_response_autom.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = at_data_request ).
    ENDIF.

*------------------------------------
*-- validar dados
*------------------------------------
    _status_code = '200'.
    _msg_erro    = abap_off.

    TRY.
        me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound = i_msg_inbound ).

      CATCH zcx_integracao.
      CATCH zcx_error INTO DATA(ex_error).
        _status_code = '400'.
        _msg_erro    = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
        set_response( i_status_code = _status_code i_msg_erro = _msg_erro ).
    ENDTRY.

*----------------------------------
*-- inicio faturamento automatico
*----------------------------------
    me->set_faturamento_automatico( EXPORTING i_origem                   = '1'
                                              i_inicia_faturamento       = at_data_request-inicia_faturamento
                                              i_continua_faturamento     = at_data_request-continua_faturamento
                                              i_cancela_faturamento      = at_data_request-cancela_faturamento
                                              i_preench_info_automatica  = at_data_request-preenchimento_info_automatica "*-#143658-19.06.2024-JT
                                     CHANGING e_status_code              = _status_code
                                    RECEIVING r_msg_erro                 = _msg_erro ).

*------------------------------------
*-- resposta
*------------------------------------
    e_sucesso        = abap_true.
    e_nm_code        = '200'.  "_status_code.
    e_msg_erro       = _msg_erro.

    IF ( at_data_request-inicia_faturamento            = abap_true    OR
         at_data_request-preenchimento_info_automatica = abap_true ) AND _status_code = '200'.  "*-#143658-19.06.2024-JT
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data             = me->at_data_response_autom
                                                          pretty_name      = /ui2/cl_json=>pretty_mode-low_case ).
    ELSE.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data             = me->at_data_response
                                                          pretty_name      = /ui2/cl_json=>pretty_mode-low_case ).
    ENDIF.

  ENDMETHOD.


  METHOD set_exec_opus.

*---------------------------------------
* types
*---------------------------------------
    TYPES: BEGIN OF ty_metodo,
             metodo TYPE string.
    TYPES: END   OF ty_metodo.

*---------------------------------------
* workarea
*---------------------------------------
    DATA: lc_integracao  TYPE zintegracao,
          lc_mensagem    TYPE zintegracao_log,
          l_error        TYPE c,
          l_mesg         TYPE string,
          l_mesg_retorno TYPE string,
          t_metodo       TYPE TABLE OF ty_metodo,
          w_metodo       TYPE ty_metodo,
          w_zlest0242    TYPE zlest0242.

*---------------------------------------
* inicio processo
*---------------------------------------
    FREE: t_metodo,
          lc_integracao,
          l_error,
          me->zif_integracao_inject~at_header_fields,
          me->zif_integracao_inject~at_info_request_http.

    me->zif_integracao_inject~at_tp_integracao  = zif_integracao=>at_tp_integracao_outbound.

*---------------------------------------
* metodos chamada
*---------------------------------------
    w_metodo-metodo   = 'TOKEN'.
    APPEND w_metodo  TO t_metodo.
    w_metodo-metodo   = i_metodo.
    APPEND w_metodo  TO t_metodo.
*---------------------------------------

*---------------------------------------
*---buscar token / metodo
*---------------------------------------
    LOOP AT t_metodo INTO w_metodo.

      TRY.
          me->set_ds_url(   EXPORTING i_metodo        = w_metodo-metodo ).
          me->set_ds_data(  EXPORTING i_integracao    = lc_integracao ).
          me->set_send_msg( IMPORTING e_id_integracao = DATA(lc_id_integracao)
                                      e_integracao    = lc_integracao
                                      e_mensagem      = lc_mensagem ).

        CATCH zcx_integracao INTO DATA(ex_integra).
          l_error = abap_true.
          l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.

          CASE w_metodo-metodo.
            WHEN 'TOKEN' OR 'RETORNO_OPUS'.
              LOOP AT me->at_zlest0242           INTO w_zlest0242 WHERE ch_faturamento   = me->at_ch_faturamento
                                                                    AND item_faturamento = me->at_item_faturamento.
                w_zlest0242-selecionado             = abap_false.
                w_zlest0242-enviado_sistema_origem  = abap_false.
                w_zlest0242-mensagem_sistema_origem = l_mesg.
                w_zlest0242-tentar_envio            = w_zlest0242-tentar_envio + 1.
                w_zlest0242-data_envio              = sy-datum.
                w_zlest0242-hora_envio              = sy-uzeit.
                w_zlest0242-user_envio              = sy-uname.
                MODIFY zlest0242                 FROM w_zlest0242.
              ENDLOOP.
              COMMIT WORK.
          ENDCASE.

          RAISE EXCEPTION TYPE zcx_integracao
            EXPORTING
              textid = VALUE #( msgid = ex_integra->msgid
                                msgno = ex_integra->msgno
                                attr1 = CONV #( ex_integra->msgv1 )
                                attr2 = CONV #( ex_integra->msgv2 )
                                attr3 = CONV #( ex_integra->msgv3 )
                                attr4 = CONV #( ex_integra->msgv4 ) )
              msgid  = ex_integra->msgid
              msgno  = ex_integra->msgno
              msgty  = 'E'
              msgv1  = CONV #( ex_integra->msgv1 )
              msgv2  = CONV #( ex_integra->msgv2 )
              msgv3  = CONV #( ex_integra->msgv3 )
              msgv4  = CONV #( ex_integra->msgv4 ).

        CATCH zcx_error      INTO DATA(ex_error).    "  "
          l_error = abap_true.
          l_mesg  = ex_error->msgv1 && '-' && ex_error->msgv2 && '-' && ex_error->msgv3 && '-' && ex_error->msgv4.

          CASE w_metodo-metodo.
            WHEN 'TOKEN' OR 'RETORNO_OPUS'.
              LOOP AT me->at_zlest0242           INTO w_zlest0242 WHERE ch_faturamento   = me->at_ch_faturamento
                                                                    AND item_faturamento = me->at_item_faturamento.
                w_zlest0242-selecionado             = abap_false.
                w_zlest0242-enviado_sistema_origem  = abap_false.
                w_zlest0242-mensagem_sistema_origem = l_mesg.
                w_zlest0242-tentar_envio            = w_zlest0242-tentar_envio + 1.
                w_zlest0242-data_envio              = sy-datum.
                w_zlest0242-hora_envio              = sy-uzeit.
                w_zlest0242-user_envio              = sy-uname.
                MODIFY zlest0242                 FROM w_zlest0242.
              ENDLOOP.
              COMMIT WORK.
          ENDCASE.

          RAISE EXCEPTION TYPE zcx_error
            EXPORTING
              textid = VALUE #( msgid = ex_error->msgid
                                msgno = ex_error->msgno
                                attr1 = CONV #( ex_error->msgv1 )
                                attr2 = CONV #( ex_error->msgv2 )
                                attr3 = CONV #( ex_error->msgv3 )
                                attr4 = CONV #( ex_error->msgv4 ) )
              msgid  = ex_error->msgid
              msgno  = ex_error->msgno
              msgty  = 'E'
              msgv1  = CONV #( ex_error->msgv1 )
              msgv2  = CONV #( ex_error->msgv2 )
              msgv3  = CONV #( ex_error->msgv3 )
              msgv4  = CONV #( ex_error->msgv4 ).
      ENDTRY.

      CHECK l_error = abap_false.

*---------------------------------------
*---- avalia retorno JSON
*---------------------------------------
      CASE w_metodo-metodo.
        WHEN 'RETORNO_OPUS'.
          LOOP AT me->at_zlest0242           INTO w_zlest0242 WHERE ch_faturamento   = me->at_ch_faturamento
                                                                AND item_faturamento = me->at_item_faturamento.
            w_zlest0242-selecionado             = abap_true.
            w_zlest0242-enviado_sistema_origem  = abap_true.
            w_zlest0242-mensagem_sistema_origem = abap_off.
            w_zlest0242-tentar_envio            = w_zlest0242-tentar_envio + 1.
            w_zlest0242-data_envio              = sy-datum.
            w_zlest0242-hora_envio              = sy-uzeit.
            w_zlest0242-user_envio              = sy-uname.
            MODIFY zlest0242                 FROM w_zlest0242.
          ENDLOOP.
          COMMIT WORK.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_faturamento_automatico.

    me->at_origem = i_origem.

    CASE i_origem.

      WHEN '1'.  "Processamento OPUS
        CHECK e_status_code = '200'.

        IF i_inicia_faturamento      = abap_off AND
           i_continua_faturamento    = abap_off AND
           i_cancela_faturamento     = abap_off AND
           i_preench_info_automatica = abap_off.    "*-#143658-19.06.2024-JT
          TRY.
              set_selecao_dados( ).

            CATCH zcx_error INTO DATA(ex_error).
              e_status_code = '400'.
              r_msg_erro    = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
              set_response( i_status_code = e_status_code i_msg_erro = r_msg_erro ).
              RETURN.
          ENDTRY.

          TRY.
              set_preparar_dados( ).

            CATCH zcx_error INTO ex_error.
              e_status_code = '400'.
              r_msg_erro    = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
              set_response( i_status_code = e_status_code i_msg_erro = r_msg_erro ).
              RETURN.
          ENDTRY.

          set_montar_json(               CHANGING e_status_code = e_status_code
                                        RECEIVING r_msg_erro    = r_msg_erro ).
        ELSE.
          TRY.
              set_preparar_faturamento(  CHANGING e_status_code = e_status_code
                                        RECEIVING r_msg_erro    = r_msg_erro ).

            CATCH zcx_error INTO ex_error.
              e_status_code = '400'.
              r_msg_erro    = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
              set_response( i_status_code = e_status_code i_msg_erro = r_msg_erro ).
              RETURN.
          ENDTRY.

          TRY.
              set_iniciar_faturamento( i_ch_faturamento = me->at_ch_faturamento ).

            CATCH zcx_error INTO ex_error.
              e_status_code = '400'.
              r_msg_erro    = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
              set_response( i_status_code = e_status_code i_msg_erro = r_msg_erro ).
              RETURN.
          ENDTRY.

        ENDIF.

      WHEN '2'.  "Processamento por outros metodos
        set_montar_estrutura( i_info_frete ).

        TRY.
            set_preparar_faturamento(    CHANGING e_status_code = e_status_code
                                        RECEIVING r_msg_erro    = r_msg_erro ).

          CATCH zcx_error INTO ex_error.
            ex_error->zif_error~published_erro(  i_msgty = 'S' ).
            RETURN.
        ENDTRY.

        TRY.
            set_iniciar_faturamento( i_ch_faturamento = me->at_ch_faturamento ).

          CATCH zcx_error INTO ex_error.
            ex_error->zif_error~published_erro(  i_msgty = 'S' ).
            RETURN.
        ENDTRY.

    ENDCASE.

  ENDMETHOD.


  METHOD set_finalizar_romaneio.

    SELECT SINGLE *
      INTO @DATA(_zlest0240)
      FROM zlest0240
     WHERE ch_faturamento   = @i_ch_faturamento
       AND cancelado        = @abap_off.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      INTO @DATA(_zlest0241)
      FROM zlest0241
     WHERE ch_faturamento   = @i_ch_faturamento
       AND item_faturamento = @i_item_faturamento
       AND cancelado        = @abap_off.

    CHECK sy-subrc = 0.
    CHECK _zlest0241-finalizado = abap_false.

    PERFORM f_selecao_fat_autom         IN PROGRAM zlesr0102    USING _zlest0241-ch_referencia
                                                                      _zlest0240-cockpit.
    PERFORM f_saida                     IN PROGRAM zlesr0102.
    PERFORM f_repare_docs_romaneio      IN PROGRAM zlesr0102 CHANGING me->at_saida.

    IF _zlest0241-status_reg = 'APRV' OR _zlest0241-status_reg = 'MDFE'. "OR me->at_saida-st_proc = '98' OR me->at_saida-st_proc = '99' .  "*-#158056-11.11.2024-JT-inicio
      DATA(l_mesg) = 'Romaneio finalizado com Sucesso.'.
      me->set_gravar_mensagem( i_ch_referencia = _zlest0241-ch_referencia i_nro = '99' i_type = 'S' i_msg = CONV #( l_mesg ) i_status = 'FINA' ).
      me->set_atualizar_status( 'FINA' ).  "finalizado
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_gerar_credito_viagem.

    DATA: l_mesg TYPE string.

*--------------------------
*-- verifica desmembramento
*--------------------------
    IF me->get_ck_desmembramento( me->at_w_zlest0241-ch_referencia )-gerar_dacte = abap_false.
      me->set_atualizar_status( 'APRV' ).
      COMMIT WORK.
      RETURN.
    ENDIF.

*--------------------------
*-- selecao romaneios
*--------------------------
    PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING me->at_w_zlest0241-ch_referencia
                                                                    me->at_w_zlest0240-cockpit.
    PERFORM f_saida                   IN PROGRAM zlesr0102.

*--------------------------
*-- recuperar dados selecionados
*--------------------------
    PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING me->at_t_saida
                                                                    me->at_zsdt0001
                                                                    me->at_saida.

    CHECK me->at_zsdt0001-doc_rem      IS NOT INITIAL.
    CHECK me->at_zsdt0001-fatura_prod  IS NOT INITIAL.
    CHECK me->at_zsdt0001-nro_nf_prod  IS NOT INITIAL.
    CHECK me->at_zsdt0001-doc_transp   IS NOT INITIAL.
    CHECK me->at_zsdt0001-fatura_frete IS NOT INITIAL.
    CHECK me->at_zsdt0001-nro_nf_frete IS NOT INITIAL.

*------------------------------------
*-- verifica ciot para a CTE
*------------------------------------
    SELECT *
      FROM zcte_ciot
      INTO @DATA(_zcte_ciot)
        UP TO 1 ROWS
     WHERE docnum = @me->at_zsdt0001-nro_nf_frete.
    ENDSELECT.

    IF sy-subrc = 0 AND ( _zcte_ciot-st_ciot = '5' OR _zcte_ciot-st_ciot = '9' ).
      l_mesg = 'Credito Viagem ja estava Aprovada!'.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'APRV' ).
      me->set_atualizar_status( 'APRV' ).  "MDFE
      COMMIT WORK.
      RETURN.
    ENDIF.

*------------------------------------
*-- solicita credito viagem
*------------------------------------
    TRY.
        me->set_solicitar_credito( me->at_zsdt0001-nro_nf_frete ).
      CATCH zcx_error INTO DATA(ex_error).
        l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
        me->set_mensagem( i_cod = CONV #( ex_error->msgno ) i_mesg = CONV #( l_mesg ) ).
    ENDTRY.

*------------------------------------
*-- verifica ciot para a CTE
*------------------------------------
    SELECT *
      FROM zcte_ciot
      INTO _zcte_ciot
        UP TO 1 ROWS
     WHERE docnum = me->at_zsdt0001-nro_nf_frete.
    ENDSELECT.

    IF sy-subrc = 0 AND ( _zcte_ciot-st_ciot = '5' OR _zcte_ciot-st_ciot = '9' ).
      l_mesg = 'Aprovacao Credito Viagem feita com Sucesso.'.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'APRV' ).
      me->set_atualizar_status( 'APRV' ).  "APRV
    ELSE.
      me->set_atualizar_status( 'RPRV' ).  "RPRV  *-#158056-11.11.2024-JT-inicio
      COMMIT WORK.
      set_mensagem( i_cod = '99' i_mesg = 'Nao foi possivel aprovar Credito da Viagem!' ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD set_gerar_dacte.

    DATA: t_return     TYPE TABLE OF bapireturn1,
          l_emite_ciot TYPE char01,
          t_cte_ciot   TYPE TABLE OF zcte_ciot,
          w_cte_ciot   TYPE zcte_ciot.

*--------------------------
*-- verifica desmembramento
*--------------------------
    IF me->get_ck_desmembramento( me->at_w_zlest0241-ch_referencia )-gerar_dacte = abap_false.
      me->set_atualizar_status( 'DACT' ).  "Transporte
      COMMIT WORK.
      RETURN.
    ENDIF.

*--------------------------
*-- selecao romaneios
*--------------------------
    PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING me->at_w_zlest0241-ch_referencia
                                                                    me->at_w_zlest0240-cockpit.
    PERFORM f_saida                   IN PROGRAM zlesr0102.

*--------------------------
*-- recuperar dados selecionados
*--------------------------
    PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING me->at_t_saida
                                                                    me->at_zsdt0001
                                                                    me->at_saida.

*--------------------------
*-- buscar docnum
*--------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = me->at_zsdt0001-fatura_frete
      IMPORTING
        output = me->at_zsdt0001-fatura_frete.

    CHECK me->at_zsdt0001-doc_rem      IS NOT INITIAL.
    CHECK me->at_zsdt0001-fatura_prod  IS NOT INITIAL.
    CHECK me->at_zsdt0001-nro_nf_prod  IS NOT INITIAL.
    CHECK me->at_zsdt0001-doc_transp   IS NOT INITIAL.
    CHECK me->at_zsdt0001-fatura_frete IS NOT INITIAL.
    CHECK me->at_zsdt0001-nro_nf_frete IS     INITIAL.

*--------------------------
*-- verifica viagem realizada
*--------------------------
    SELECT SINGLE j_1bnfdoc~*
      INTO @DATA(_j_1bnfdoc)
      FROM j_1bnflin
     INNER JOIN j_1bnfdoc
             ON j_1bnfdoc~docnum = j_1bnflin~docnum
     WHERE j_1bnflin~refkey = @me->at_zsdt0001-fatura_frete.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'Z_CIOT_EMPRESA_PARCEIRO'
      EXPORTING
        p_empresa    = _j_1bnfdoc-bukrs
        p_partyp     = _j_1bnfdoc-partyp
        p_parid      = _j_1bnfdoc-parid
        p_dt_posicao = _j_1bnfdoc-docdat
        p_tknum      = me->at_zsdt0001-doc_transp
      IMPORTING
        p_emite      = l_emite_ciot.

    IF l_emite_ciot = abap_true.
      CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
        EXPORTING
          p_cte_avulso = _j_1bnfdoc-docnum
        TABLES
          it_cte_ciot  = t_cte_ciot
        EXCEPTIONS
          nao_ciot     = 1
          OTHERS       = 2.

      LOOP AT t_cte_ciot INTO w_cte_ciot.
        CASE w_cte_ciot-st_ciot.
          WHEN '0' OR space.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' w_cte_ciot-cd_ciot 'com estatus de "pendente"' INTO DATA(l_mesg).
            me->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          WHEN '1'.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' w_cte_ciot-cd_ciot 'com estatus de "enviado"' INTO l_mesg.
            me->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          WHEN '3'.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' w_cte_ciot-cd_ciot 'com estatus de "Rejeitado"' INTO l_mesg..
            me->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          WHEN '4'.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Nr. CIOT' w_cte_ciot-nr_ciot 'com estatus de "creditado"' INTO l_mesg..
            me->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          WHEN '5'.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' w_cte_ciot-cd_ciot 'com estatus de "fechado (pago cockpit)"' INTO l_mesg..
            me->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          WHEN '7'.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' w_cte_ciot-cd_ciot 'com estatus de "Enviado Cancelamento"' INTO l_mesg.
            me->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          WHEN '8'.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH 'Viagem Cd. CIOT' w_cte_ciot-cd_ciot 'com estatus de "Cancelado"' INTO l_mesg.
            me->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ENDCASE.
      ENDLOOP.
    ENDIF.

*--------------------------
*-- Autorizar DACTE
*--------------------------
    TRY.
        PERFORM f_action_user_dacte   IN PROGRAM zlesr0102    USING me->at_saida.

      CATCH zcx_error INTO DATA(ex_error).
        l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
        me->set_mensagem( i_cod = CONV #( ex_error->msgno ) i_mesg = l_mesg ).
    ENDTRY.

*--------------------------
*-- checa dacte autorizada
*--------------------------
    IF me->get_ck_danfe_dacte_autorizada( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'DACTE' ) = abap_off.
      me->set_mensagem( '21' ).
    ENDIF.

*--------------------------
*-- verifica documento gerado
*--------------------------
    DO 10 TIMES.
      PERFORM f_repare_docs_romaneio  IN PROGRAM zlesr0102 CHANGING me->at_saida.
      IF me->at_saida-dacte IS NOT INITIAL AND me->at_saida-dacte(1) <> '@'.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.
    ENDDO.

*------------------------------------
*-- retorno
*------------------------------------
    IF me->at_saida-dacte IS NOT INITIAL AND me->at_saida-dacte(1) <> '@'.
      l_mesg = 'DACTE autorizada com Sucesso! Docnum:' && me->at_saida-dacte.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'DACT' ).
      me->set_atualizar_status( 'DACT' ).  "Dacte
    ELSE.
      set_mensagem( i_cod = '99' i_mesg = 'CT-e Aguardando Autorização da SEFAZ!' ). "*-#166749-20.02.2025-JT
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD set_gerar_danfe.

    DATA: t_return TYPE TABLE OF bapireturn1,
          l_mesg   TYPE string.

*--------------------------
*-- selecao romaneios
*--------------------------
    PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING me->at_w_zlest0241-ch_referencia
                                                                    me->at_w_zlest0240-cockpit.
    PERFORM f_saida                   IN PROGRAM zlesr0102.

*--------------------------
*-- recuperar dados selecionados
*--------------------------
    PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING me->at_t_saida
                                                                    me->at_zsdt0001
                                                                    me->at_saida.

    CHECK me->at_zsdt0001-doc_rem     IS NOT INITIAL.
    CHECK me->at_zsdt0001-fatura_prod IS NOT INITIAL.
    CHECK me->at_zsdt0001-nro_nf_prod IS     INITIAL.

*--------------------------
*-- Autorizar DANFE
*--------------------------
    TRY.
        PERFORM f_action_user_danfe   IN PROGRAM zlesr0102    USING me->at_saida.

      CATCH zcx_error INTO DATA(ex_error).
        l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
        me->set_mensagem( i_cod = CONV #( ex_error->msgno ) i_mesg = l_mesg ).
    ENDTRY.

*-#167847-24.02.2025-JT-inicio
*--------------------------
*-- verifica se procesamento deve ser paralizado
*--------------------------
    l_mesg =  me->get_ck_abandonar_processamento( i_ch_faturamento   = me->at_w_zlest0241-ch_faturamento
                                                  i_item_faturamento = me->at_w_zlest0241-item_faturamento
                                                  i_status_reg       = 'TRAN' ).
    IF l_mesg IS NOT INITIAL.
      PERFORM f_repare_docs_romaneio  IN PROGRAM zlesr0102 CHANGING me->at_saida.
      PERFORM f_estorno_custo         IN PROGRAM zlesr0102 CHANGING me->at_saida.
      me->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
    ENDIF.
*-#167847-24.02.2025-JT-fim

*--------------------------
*-- checa danfe autorizada
*--------------------------
    IF me->get_ck_danfe_dacte_autorizada( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'DANFE' ) = abap_off.
      me->set_mensagem( '20' ).
    ENDIF.

*--------------------------
*-- verifica documento gerado
*--------------------------
    DO 10 TIMES.
      PERFORM f_repare_docs_romaneio  IN PROGRAM zlesr0102 CHANGING me->at_saida.
      IF me->at_saida-danfe IS NOT INITIAL AND me->at_saida-danfe(1) <> '@'.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.
    ENDDO.

*------------------------------------
*-- retorno
*------------------------------------
    IF me->at_saida-danfe IS NOT INITIAL AND me->at_saida-danfe(1) <> '@'.
      l_mesg = 'DANFE autorizada com Sucesso! Docnum:' && me->at_saida-danfe.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'DANF' ).
      me->set_atualizar_status( 'DANF' ).  "Danfe
    ELSE.
      set_mensagem( i_cod = '99' i_mesg = 'NF-e Aguardando Autorização da SEFAZ!' ). "*-#166749-20.02.2025-JT
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD set_gerar_documentos_custos.

    DATA: l_mesg TYPE string. "*-#158056-11.11.2024-JT-inicio

*--------------------------
*-- verifica desmembramento
*--------------------------
    IF me->get_ck_desmembramento( me->at_w_zlest0241-ch_referencia )-gerar_transporte = abap_false.
      me->set_atualizar_status( 'DCUS' ).  "Transporte
      COMMIT WORK.
      RETURN.
    ENDIF.

*--------------------------
*-- selecao romaneios
*--------------------------
    PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING me->at_w_zlest0241-ch_referencia
                                                                    me->at_w_zlest0240-cockpit.
    PERFORM f_saida                   IN PROGRAM zlesr0102.

*--------------------------
*-- recuperar dados selecionados
*--------------------------
    PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING me->at_t_saida
                                                                    me->at_zsdt0001
                                                                    me->at_saida.

    CHECK me->at_zsdt0001-doc_rem      IS NOT INITIAL.
    CHECK me->at_zsdt0001-fatura_prod  IS NOT INITIAL.
    CHECK me->at_zsdt0001-nro_nf_prod  IS NOT INITIAL.
    CHECK me->at_zsdt0001-doc_transp   IS NOT INITIAL.

*-#158056-11.11.2024-JT-inicio
*   CHECK me->at_zsdt0001-fknum        IS     INITIAL OR
*         me->at_zsdt0001-ov_frete     IS     INITIAL OR
*         me->at_zsdt0001-fatura_frete IS     INITIAL.
    IF me->at_zsdt0001-fknum           IS NOT   INITIAL.
      l_mesg = 'Documentos Custos ja foi gerados com Sucesso.'.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'DCUS' ).
      me->set_atualizar_status( 'DCUS' ).  "Doc.Custos
      RETURN.
    ENDIF.
*-#158056-11.11.2024-JT-fim

*--------------------------
*-- GErar Doc.Custos
*--------------------------
    TRY.
        PERFORM f_check_retorno_vt    IN PROGRAM zlesr0102    USING 'T' 'N'
                                                           CHANGING me->at_saida.

      CATCH zcx_error INTO DATA(ex_error).
        l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
        me->set_mensagem( i_cod = CONV #( ex_error->msgno ) i_mesg = l_mesg ).
    ENDTRY.

*--------------------------
*-- verifica documento gerado
*--------------------------
    DO 10 TIMES.
      PERFORM f_repare_docs_romaneio  IN PROGRAM zlesr0102 CHANGING me->at_saida.
      IF me->at_saida-doccus  IS NOT INITIAL AND me->at_saida-doccus(1)  <> '@'. " AND "*-#158056-11.11.2024-JT-inicio
*        me->at_saida-ovserv  IS NOT INITIAL AND me->at_saida-ovserv(1)  <> '@' AND
*        me->at_saida-fatserv IS NOT INITIAL AND me->at_saida-fatserv(1) <> '@'.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.
    ENDDO.

*------------------------------------
*-- retorno
*------------------------------------
    IF me->at_saida-doccus  IS NOT INITIAL AND me->at_saida-doccus(1)  <> '@'. " AND  "*-#158056-11.11.2024-JT-inicio
*      me->at_saida-ovserv  IS NOT INITIAL AND me->at_saida-ovserv(1)  <> '@' AND
*      me->at_saida-fatserv IS NOT INITIAL AND me->at_saida-fatserv(1) <> '@'.
      l_mesg = 'Documentos Custos gerados com Sucesso.'.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'DCUS' ).
      me->set_atualizar_status( 'DCUS' ).  "Doc.Custos
    ELSE.
      set_mensagem( i_cod = '99' i_mesg = 'Nao foi possivel gerar Doc.Custos!' ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD set_gerar_fatura.

    DATA: lc_ch_referencia TYPE zch_ref,
          t_return         TYPE TABLE OF bapireturn1.

*--------------------------
*-- selecao romaneios
*--------------------------
    PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING me->at_w_zlest0241-ch_referencia
                                                                    me->at_w_zlest0240-cockpit.
    PERFORM f_saida                   IN PROGRAM zlesr0102.

*--------------------------
*-- recuperar dados selecionados
*--------------------------
    PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING me->at_t_saida
                                                                    me->at_zsdt0001
                                                                    me->at_saida.

    CHECK me->at_zsdt0001-doc_rem     IS NOT INITIAL.
    CHECK me->at_zsdt0001-fatura_prod IS INITIAL.

*--------------------------
*-- Este ID sera usado na ENHANCEMENT: Z_INPUT_TAXS ao gerar a fatura
*--------------------------
    lc_ch_referencia        = me->at_w_zlest0241-ch_referencia.
    EXPORT lc_ch_referencia = lc_ch_referencia TO MEMORY ID 'FAT_AUTOMATICO'.

*--------------------------
*-- GErar fatura
*--------------------------
    TRY.
        PERFORM f_action_user_fatura  IN PROGRAM zlesr0102    USING me->at_saida 'L' CHANGING me->at_t_saida[] t_return[].

      CATCH zcx_error INTO DATA(ex_error).
        DATA(l_mesg) = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
        me->set_mensagem( i_cod = CONV #( ex_error->msgno ) i_mesg = l_mesg ).
    ENDTRY.

*--------------------------
*-- verifica documento gerado
*--------------------------
    DO 10 TIMES.
      PERFORM f_repare_docs_romaneio  IN PROGRAM zlesr0102 CHANGING me->at_saida.
      IF me->at_saida-fatura IS NOT INITIAL AND me->at_saida-fatura(1) <> '@'.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.
    ENDDO.

*------------------------------------
*-- retorno
*------------------------------------
    IF me->at_saida-fatura IS NOT INITIAL AND me->at_saida-fatura(1) <> '@'.
      l_mesg = 'Fatura gerada com Sucesso: ' && me->at_saida-fatura.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'FATU' ).
      me->set_atualizar_status( 'FATU' ).  "Fatura
    ELSE.
      set_mensagem( i_cod = '99' i_mesg = 'Nao foi possivel gerar Fatura!' ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  method set_gerar_mdfe.

    data: l_docnum      type j_1bnfdoc-docnum,
          l_docnum_mdfe type j_1bnfdoc-docnum,
          l_manifesto   type char01,
          lc_mdfe       type ref to zcl_mdfe,
          lc_erro       type char01,
          lc_tp_doc_ref type zdoc_ref_mdfe.  "*-#158056-11.11.2024-JT-inicio

    create object lc_mdfe.

*--------------------------
*-- verifica desmembramento
*--------------------------
    if me->get_ck_desmembramento( me->at_w_zlest0241-ch_referencia )-gerar_dacte = abap_false.
      me->set_atualizar_status( 'MDFE' ).
      commit work.
      return.
    endif.

*--------------------------
*-- selecao romaneios
*--------------------------
    perform f_selecao_fat_autom       in program zlesr0102    using me->at_w_zlest0241-ch_referencia
                                                                    me->at_w_zlest0240-cockpit.
    perform f_saida                   in program zlesr0102.

*--------------------------
*-- recuperar dados selecionados
*--------------------------
    perform f_recuperar_dados         in program zlesr0102 changing me->at_t_saida
                                                                    me->at_zsdt0001
                                                                    me->at_saida.


*-#158056-11.11.2024-JT-inicio
    if me->at_zsdt0001-fatura_frete is initial.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = me->at_zsdt0001-nro_nf_prod
        importing
          output = me->at_zsdt0001-nro_nf_prod.

      l_docnum = me->at_zsdt0001-nro_nf_prod.

*-----verifica emissao MDF-e
      zcl_faturamento=>zif_faturamento~get_instance(
        )->get_processo_emissao_docs( exporting i_docnum    = l_docnum
                                      importing e_manifesto = l_manifesto
        ).

      if l_manifesto = abap_false.
        me->set_atualizar_status( 'MDFE' ).
        commit work.
        return.
      endif.

*--------------------------
*---- Este ID sera usado NO INCLUDE Z_1BNFE_MONITOR_F34, FORM GRAVAR_MDFE
*--------------------------
      lc_tp_doc_ref = '2'.
      export lc_tp_doc_ref = lc_tp_doc_ref to memory id 'FAT_AUTOMATICO_TPDOC'.
    else.
*-#158056-11.11.2024-JT-fim
*--------------------------
*---- buscar docnum
*--------------------------
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = me->at_zsdt0001-fatura_frete
        importing
          output = me->at_zsdt0001-fatura_frete.

      select single j_1bnflin~docnum
               into l_docnum
               from j_1bnflin
         inner join j_1bnfdoc on j_1bnfdoc~docnum = j_1bnflin~docnum
              where j_1bnflin~refkey = me->at_zsdt0001-fatura_frete.

      check sy-subrc = 0.

      check me->at_zsdt0001-doc_rem      is not initial.
      check me->at_zsdt0001-fatura_prod  is not initial.
      check me->at_zsdt0001-nro_nf_prod  is not initial.
      check me->at_zsdt0001-doc_transp   is not initial.
      check me->at_zsdt0001-fatura_frete is not initial.
      check me->at_zsdt0001-nro_nf_frete is not initial.

*--------------------------
*---- Este ID sera usado NO INCLUDE Z_1BNFE_MONITOR_F34, FORM GRAVAR_MDFE
*--------------------------
      lc_tp_doc_ref = '1'.                                                       "*-#158056-11.11.2024-JT-fim
      export lc_tp_doc_ref = lc_tp_doc_ref to memory id 'FAT_AUTOMATICO_TPDOC'.  "*-#158056-11.11.2024-JT-fim
    endif.

    "ALRS
    l_docnum_mdfe = me->set_validar_mdfe_gerada( l_docnum ).
    if l_docnum_mdfe is not initial.
      select single *
       from zsdt0102
       into @data(w1_0102)
      where docnum = @l_docnum_mdfe
      and   status = '3'.
      if sy-subrc = 0.
        update zlest0241 set job_terminado = 'X'
        where ch_faturamento = me->at_w_zlest0241-ch_faturamento.
        commit work.
      endif.
    endif.
    "ALRS
*--------------------------
*---- checa se sera feito novo mdfe
*--------------------------
    lc_mdfe->add_documento( l_docnum ).

    select *
      from zsdt0105
      into table @data(t_0105)
     where docnum = @l_docnum.

    if sy-subrc = 0.
      loop at t_0105 into data(w_0105) where docnum_ref is not initial.
        select single *
          into @data(w_0102)
          from zsdt0102
         where docnum    eq @w_0105-docnum_ref
           and estornado ne 'X'
           and cancel    ne 'X'
           and encerrado ne 'X'.
        if sy-subrc = 0.
          data(l_mesmos_docs_vinc) = lc_mdfe->check_mesmos_docs_vinc( i_docnum = w_0102-docnum ).
          if l_mesmos_docs_vinc = abap_true.
            if me->get_ck_danfe_dacte_autorizada( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'MDFE' ) = abap_true.
              data(l_mesg) = 'MDF-e ja estava Autorizada! Docnum:' && w_0102-docnum.
              me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'MDFE' ).
              me->set_atualizar_status( 'MDFE' ).  "MDFE
              commit work.
              return.
            endif.
          endif.
        endif.
      endloop.
    endif.

*--------------------------
*-- seleecionar dados monitor CTE
*--------------------------
    perform f_selecao_fat_autom       in program z_1bnfe_monitor using me->at_w_zlest0241-ch_faturamento
                                                                       me->at_w_zlest0241-ch_referencia
                                                                       l_docnum.

*--------------------------
*-- Solicitar viagem
*--------------------------
    try.
        perform manifesto_eletronico  in program z_1bnfe_monitor.

      catch zcx_error into data(ex_error).
        l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
        me->set_mensagem( i_cod = conv #( ex_error->msgno ) i_mesg = l_mesg ).
    endtry.

*--------------------------
*-- autorizacao MDF-e
*--------------------------

    me->get_ck_danfe_dacte_autorizada( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'MDFE' ).



*------------------------------------
*-- verifica nro MDF-e autorizada
*------------------------------------
    l_docnum_mdfe = me->set_validar_mdfe_gerada( l_docnum ).


    if l_docnum_mdfe is initial.
*-----tenta 2o envio -------------------------------------------------------------
      lc_erro = abap_true.
      l_mesg  = 'MDF-e: Tentando nova autorização!'.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ). ""*-#167847-24.02.2025-JT

      do 5 times.
        if me->get_ck_danfe_dacte_autorizada( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'MDFE' ) = abap_true.
          lc_erro = abap_false.
          exit.
        endif.
      enddo.
      l_docnum_mdfe = me->set_validar_mdfe_gerada( l_docnum ).

      if lc_erro = abap_true.
        me->set_mensagem( '22' ).
      endif.
    endif.

    l_docnum_mdfe = me->set_validar_mdfe_gerada( l_docnum ).

    if l_docnum_mdfe is initial.
      me->set_mensagem( '22' ).
    endif.

    l_mesg = 'MDF-e autorizada com Sucesso! Docnum:' && l_docnum_mdfe.
    me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'MDFE' ).
    me->set_atualizar_status( 'MDFE' ).  "MDFE

    commit work.

  endmethod.


  METHOD set_gerar_remessa.

    DATA: l_index TYPE syst_index.

*--------------------------
*-- selecao romaneios
*--------------------------
    PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING me->at_w_zlest0241-ch_referencia
                                                                    me->at_w_zlest0240-cockpit.
    PERFORM f_saida                   IN PROGRAM zlesr0102.

*--------------------------
*-- recuperar dados selecionados
*--------------------------
    PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING me->at_t_saida
                                                                    me->at_zsdt0001
                                                                    me->at_saida.

    CHECK me->at_zsdt0001-doc_rem IS INITIAL.

*--------------------------
*-- GErar remessa
*--------------------------
    DO 4 TIMES.
      l_index = sy-index.

      TRY.
          PERFORM f_action_user_remessa IN PROGRAM zlesr0102    USING me->at_saida.
          EXIT.

        CATCH zcx_error INTO DATA(ex_error).
          IF l_index = 4.
            DATA(l_mesg) = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
            me->set_mensagem( i_cod = CONV #( ex_error->msgno ) i_mesg = l_mesg ).
          ELSE.
            WAIT UP TO 10 SECONDS.
            PERFORM f_selecao_fat_autom    IN PROGRAM zlesr0102    USING me->at_w_zlest0241-ch_referencia
                                                                         me->at_w_zlest0240-cockpit.
            PERFORM f_saida                IN PROGRAM zlesr0102.
            PERFORM f_recuperar_dados      IN PROGRAM zlesr0102 CHANGING me->at_t_saida
                                                                         me->at_zsdt0001
                                                                         me->at_saida.
            PERFORM f_repare_docs_romaneio IN PROGRAM zlesr0102 CHANGING me->at_saida.
            IF me->at_saida-remessa IS NOT INITIAL AND me->at_saida-remessa(1) <> '@'.
              EXIT.
            ENDIF.
          ENDIF.
      ENDTRY.
    ENDDO.

*--------------------------
*-- verifica documento gerado
*--------------------------
    DO 10 TIMES.
      PERFORM f_repare_docs_romaneio  IN PROGRAM zlesr0102 CHANGING me->at_saida.
      IF me->at_saida-remessa IS NOT INITIAL AND me->at_saida-remessa(1) <> '@'.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.
    ENDDO.

*------------------------------------
*-- retorno
*------------------------------------
    IF me->at_saida-remessa IS NOT INITIAL AND me->at_saida-remessa(1) <> '@'.
      l_mesg = 'Remessa gerada com Sucesso:' && me->at_saida-remessa.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'REME' ).
      me->set_atualizar_status( 'REME' ).  "Remessa
    ELSE.
      set_mensagem( i_cod = '99' i_mesg = 'Nao foi possivel gerar Remessa!' ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD set_gerar_transporte.

    DATA: t_itemdata     TYPE TABLE OF bapishipmentitem,
          w_itemdata     TYPE bapishipmentitem,
          l_tipo_chamada TYPE char1,
          l_mesg         TYPE string,   "*-#158056-11.11.2024-JT-inicio
          t_return       TYPE TABLE OF bapireturn1.

    FREE: t_itemdata.

*--------------------------
*-- verifica desmembramento
*--------------------------
    IF me->get_ck_desmembramento( me->at_w_zlest0241-ch_referencia )-gerar_transporte = abap_false.
      me->set_atualizar_status( 'TRAN' ).  "Transporte
      COMMIT WORK.
      RETURN.
    ENDIF.

*--------------------------
*-- no desmembramento, aguardar gerar remessa do romaneio irmao
*--------------------------
    CHECK me->get_aguardar_desmembramento(  i_ch_referencia       = me->at_w_zlest0241-ch_referencia ) = abap_false.

*--------------------------
*-- selecao romaneios
*--------------------------
    PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING me->at_w_zlest0241-ch_referencia
                                                                    me->at_w_zlest0240-cockpit.
    PERFORM f_saida                   IN PROGRAM zlesr0102.

*--------------------------
*-- recuperar dados selecionados
*--------------------------
    PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING me->at_t_saida
                                                                    me->at_zsdt0001
                                                                    me->at_saida.

    CHECK me->at_zsdt0001-doc_rem     IS NOT INITIAL.
    CHECK me->at_zsdt0001-fatura_prod IS NOT INITIAL.
    CHECK me->at_zsdt0001-nro_nf_prod IS NOT INITIAL.

*-#158056-11.11.2024-JT-inicio
*   CHECK me->at_zsdt0001-doc_transp  IS     INITIAL.
    IF me->at_zsdt0001-doc_transp     IS NOT INITIAL.
      l_mesg = 'Transporte ja foi gerado com Sucesso.'.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'TRAN' ).
      me->set_atualizar_status( 'TRAN' ).  "Transporte
      RETURN.
    ENDIF.
*-#158056-11.11.2024-JT-fim

*--------------------------
*-- GErar transporte
*--------------------------
    TRY.
        PERFORM f_action_user_transp  IN PROGRAM zlesr0102    USING me->at_saida 'L' CHANGING me->at_t_saida[] t_return[].

      CATCH zcx_error INTO DATA(ex_error).
        l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
        me->set_mensagem( i_cod = CONV #( ex_error->msgno ) i_mesg = l_mesg ).
    ENDTRY.

*--------------------------
*-- verifica documento gerado
*--------------------------
    DO 10 TIMES.
      PERFORM f_repare_docs_romaneio  IN PROGRAM zlesr0102 CHANGING me->at_saida.
      IF me->at_saida-transp IS NOT INITIAL AND me->at_saida-transp(1) <> '@'.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.
    ENDDO.

*-#167847-24.02.2025-JT-inicio
*--------------------------
*-- verifica se procesamento deve ser paralizado
*--------------------------
    l_mesg = me->get_ck_abandonar_processamento( i_ch_faturamento   = me->at_w_zlest0241-ch_faturamento
                                                 i_item_faturamento = me->at_w_zlest0241-item_faturamento
                                                 i_status_reg       = 'TRAN' ).
    IF l_mesg IS NOT INITIAL.
      PERFORM f_estorno_custo         IN PROGRAM zlesr0102 CHANGING me->at_saida.
      me->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
    ENDIF.
*-#167847-24.02.2025-JT-fim

*--------------------------
*-- se nao gerou ZLEST0026, elimina VT
*--------------------------
    IF me->at_saida-transp IS NOT INITIAL AND me->at_saida-transp(1) <> '@'.
      SELECT SINGLE *
        INTO @DATA(w_0026)
        FROM zlest0026
       WHERE tknum = @me->at_saida-transp.

      IF sy-subrc <> 0.
        PERFORM f_estorno_custo        IN PROGRAM zlesr0102 CHANGING me->at_saida.
        PERFORM f_repare_docs_romaneio IN PROGRAM zlesr0102 CHANGING me->at_saida.

        IF me->at_saida-transp IS NOT INITIAL AND me->at_saida-transp(1) <> '@'.
          me->set_mensagem( '26' ).
        ENDIF.
      ENDIF.
    ENDIF.

*------------------------------------
*-- retorno
*------------------------------------
    IF me->at_saida-transp IS NOT INITIAL AND me->at_saida-transp(1) <> '@'.
      l_mesg = 'Transporte gerado com Sucesso.'.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'TRAN' ).
      me->set_atualizar_status( 'TRAN' ).  "Transporte
    ELSE.
      set_mensagem( i_cod = '99' i_mesg = 'Nao foi possivel gerar Transporte!' ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD set_gerar_viagem.

    DATA: l_docnum TYPE j_1bnfdoc-docnum,
          l_mesg   TYPE string.  "*-US 172686-01.04.2025-JT

*--------------------------
*-- verifica desmembramento
*--------------------------
    IF me->get_ck_desmembramento(  me->at_w_zlest0241-ch_referencia )-gerar_dacte = abap_false.
      me->set_atualizar_status( 'SVIA' ).  "Transporte
      COMMIT WORK.
      RETURN.
    ENDIF.

*--------------------------
*-- selecao romaneios
*--------------------------
    PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102    USING me->at_w_zlest0241-ch_referencia
                                                                    me->at_w_zlest0240-cockpit.
    PERFORM f_saida                   IN PROGRAM zlesr0102.

*--------------------------
*-- recuperar dados selecionados
*--------------------------
    PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING me->at_t_saida
                                                                    me->at_zsdt0001
                                                                    me->at_saida.

*--------------------------
*-- buscar docnum
*--------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = me->at_zsdt0001-fatura_frete
      IMPORTING
        output = me->at_zsdt0001-fatura_frete.

    SELECT SINGLE j_1bnflin~docnum
             INTO l_docnum
             FROM j_1bnflin
       INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
            WHERE j_1bnflin~refkey = me->at_zsdt0001-fatura_frete.

    CHECK me->at_zsdt0001-doc_rem      IS NOT INITIAL.
    CHECK me->at_zsdt0001-fatura_prod  IS NOT INITIAL.
    CHECK me->at_zsdt0001-nro_nf_prod  IS NOT INITIAL.
    CHECK me->at_zsdt0001-doc_transp   IS NOT INITIAL.
    CHECK me->at_zsdt0001-fatura_frete IS NOT INITIAL.
    CHECK me->at_zsdt0001-nro_nf_frete IS     INITIAL.

*--------------------------
*-- verifica se criou viagem
*--------------------------
    SELECT * INTO @DATA(_zcte)
      FROM zcte_viagem
     UP TO 1 ROWS
     WHERE docnum    = @l_docnum
       AND st_ciot   = '1'
       AND st_ultimo = @abap_true.
    ENDSELECT.

    IF sy-subrc = 0 AND _zcte-ds_msg = 'OK'.
      me->set_atualizar_status( 'SVIA' ).  "Viagem
      COMMIT WORK.
      RETURN.
    ENDIF.

*--------------------------
*-- seleecionar dados monitor CTE
*--------------------------
    PERFORM f_selecao_fat_autom       IN PROGRAM z_1bnfe_monitor USING me->at_w_zlest0241-ch_faturamento
                                                                       me->at_w_zlest0241-ch_referencia
                                                                       l_docnum.

*--------------------------
*-- Solicitar viagem
*--------------------------
    TRY.
        PERFORM reset_rejected_nfe    IN PROGRAM z_1bnfe_monitor. "FF #170994
        PERFORM solicitar_ciot        IN PROGRAM z_1bnfe_monitor.

      CATCH zcx_error INTO DATA(ex_error).
        l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
        me->set_mensagem( i_cod = CONV #( ex_error->msgno ) i_mesg = l_mesg ).
    ENDTRY.

*------------------------------------
*-- retorno
*------------------------------------
*-US 172686-01.04.2025-JT-inicio
    SELECT * INTO _zcte
      FROM zcte_viagem
     UP TO 1 ROWS
     WHERE docnum    = l_docnum
       AND st_ciot   = '1'
       AND st_ultimo = abap_true.
    ENDSELECT.

    SELECT *
      INTO @DATA(_zcte_ciot)
      FROM zcte_ciot
     UP TO 1 ROWS
     WHERE docnum = @l_docnum.
    ENDSELECT.

    IF sy-subrc = 0 AND _zcte-ds_msg = 'OK'.
      l_mesg = 'Viagem criada com Sucesso! Nr.CIOT:' && _zcte_ciot-nr_ciot.
      me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'SVIA' ).
      me->set_atualizar_status( 'SVIA' ).  "Viagem
    ELSE.
      IF _zcte_ciot-nr_ciot IS INITIAL.
        IF     _zcte_ciot-st_ciot = '0' OR     "Pendente
               _zcte_ciot-st_ciot = '1' OR     "Enviado
               _zcte_ciot-st_ciot = '3'.       "Rejeitado
          l_mesg = 'Nao foi possivel solicitar Viagem. Status:' &&  _zcte_ciot-st_ciot.
          set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ELSEIF _zcte_ciot-st_ciot = '9'.       "Não gera contrato - frota Amaggi
          l_mesg = 'Não necessita Autorizacao Viagem. Status:'  && _zcte_ciot-st_ciot.
          me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'SVIA' ).
          me->set_atualizar_status( 'SVIA' ).  "Viagem
        ENDIF.
      ELSE.
        IF    _zcte_ciot-st_ciot = '2'.        "Autorizado
          l_mesg = 'Viagem criada com Sucesso! Nr.CIOT:' && _zcte_ciot-nr_ciot.
          me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'SVIA' ).
          me->set_atualizar_status( 'SVIA' ).  "Viagem
        ELSE.
          l_mesg = 'Nao foi possivel solicitar Viagem. Status:' &&  _zcte_ciot-st_ciot.
          set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ENDIF.
      ENDIF.
    ENDIF.

*     IF sy-subrc = 0 AND _zcte_ciot-nr_ciot IS NOT INITIAL..
*       l_mesg = 'Viagem criada com Sucesso! Nr.CIOT:' && _zcte_ciot-nr_ciot.
*       me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'SVIA' ).
*       me->set_atualizar_status( 'SVIA' ).  "Viagem
*     ELSE.
*       set_mensagem( i_cod = '99' i_mesg = 'Nao foi possivel solicitar Viagem!' ).
*     ENDIF.
*   ELSE.
*     set_mensagem( i_cod = '99' i_mesg = 'Nao foi possivel solicitar Viagem!' ).
*   ENDIF.
*-US 172686-01.04.2025-JT-fim
    COMMIT WORK.

  ENDMETHOD.


  METHOD set_gravar_mensagem.

    DATA: l_mensagem   TYPE string,
          l_mesg       TYPE char255,
          l_timestampl TYPE timestampl,
          t_bapiret    TYPE bapiret2_t,
          w_bapiret    TYPE bapiret2,
          w_zlest0242  TYPE zlest0242.

    CHECK i_nro <> '999'.

*------------------------------------
*-- chave de referencia
*------------------------------------
    SELECT SINGLE *
      FROM zlest0241
      INTO @DATA(w_zlest0241)
     WHERE doc_ref_origem = @i_ch_referencia
       AND cancelado      = @abap_false.

    CHECK sy-subrc = 0.

*------------------------------------
*-- formata mensagem
*------------------------------------
    IF i_tab_bapiret1[] IS NOT INITIAL.
      LOOP AT i_tab_bapiret1             INTO DATA(w_tab_bapiret1).
        MOVE-CORRESPONDING w_tab_bapiret1  TO w_bapiret.
        APPEND w_bapiret                   TO t_bapiret.
      ENDLOOP.
    ELSEIF i_tab_bapiret2[] IS NOT INITIAL.
      LOOP AT i_tab_bapiret2             INTO DATA(w_tab_bapiret2).
        MOVE-CORRESPONDING w_tab_bapiret2  TO w_bapiret.
        APPEND w_bapiret                   TO t_bapiret.
      ENDLOOP.
    ENDIF.

    IF i_msg IS NOT INITIAL.
      l_mensagem = i_msg.

    ELSEIF t_bapiret[] IS NOT INITIAL.
      READ TABLE t_bapiret INTO w_bapiret WITH KEY type = 'E'.
      CHECK sy-subrc = 0.

      LOOP AT t_bapiret INTO w_bapiret.
        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            id        = w_tab_bapiret1-id
            lang      = sy-langu
            no        = w_bapiret-number
            v1        = w_bapiret-message_v1
            v2        = w_bapiret-message_v2
            v3        = w_bapiret-message_v3
            v4        = w_bapiret-message_v4
          IMPORTING
            msg       = l_mesg
          EXCEPTIONS
            not_found = 01
            OTHERS    = 02.

        CHECK l_mesg IS NOT INITIAL.
        l_mensagem = l_mensagem && l_mesg && '|'.
      ENDLOOP.

    ELSEIF i_tab_erro[] IS NOT INITIAL.
      READ TABLE i_tab_erro INTO DATA(w_tab_erro) WITH KEY tp_msn = 'E'.
      CHECK sy-subrc = 0.

      LOOP AT i_tab_erro INTO w_tab_erro.
        CHECK w_tab_erro-messagem IS NOT INITIAL.
        l_mensagem = l_mensagem && w_tab_erro-messagem && '|'.
      ENDLOOP.
    ENDIF.

    CHECK l_mensagem IS NOT INITIAL.

*------------------------------------
*-- gravar mensagem
*------------------------------------
    CLEAR w_zlest0242.

    DO.
      GET TIME STAMP FIELD l_timestampl.

      SELECT SINGLE ch_faturamento
        INTO @DATA(_ch_faturamento)
        FROM zlest0242
       WHERE ch_faturamento   = @w_zlest0241-ch_faturamento
         AND item_faturamento = @w_zlest0241-item_faturamento
         AND seq              = @l_timestampl.

      IF sy-subrc = 0.
        WAIT UP TO 1 SECONDS.
        CONTINUE.
      ENDIF.

      w_zlest0242-mandt                   = sy-mandt.
      w_zlest0242-ch_faturamento          = w_zlest0241-ch_faturamento.
      w_zlest0242-item_faturamento        = w_zlest0241-item_faturamento.
      w_zlest0242-seq                     = l_timestampl.
      w_zlest0242-status_reg              = i_status.
      w_zlest0242-selecionado             = abap_false.
      w_zlest0242-enviado_sistema_origem  = abap_false.
      w_zlest0242-status_msg              = i_type.
      w_zlest0242-mensagem                = l_mensagem.
      w_zlest0242-mensagem_sistema_origem = abap_false.
      w_zlest0242-erro_abandonar          = i_erro_abandonar. "*-#167847-24.02.2025-JT-inicio
      w_zlest0242-data_reg                = sy-datum.
      w_zlest0242-hora_reg                = sy-uzeit.
      w_zlest0242-user_reg                = sy-uname.
      w_zlest0242-cancelado               = abap_false.
      MODIFY zlest0242                 FROM w_zlest0242.

      COMMIT WORK AND WAIT.
      EXIT.
    ENDDO.

*------------------------------------
*-- enviar mensagem ao OPUS
*------------------------------------
    IF i_envia = abap_true.
      TRY.
          me->set_retorno_opus( i_ch_referencia = i_ch_referencia ).
        CATCH zcx_integracao.
        CATCH zcx_error.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD SET_ID_REFERENCIA.

    me->get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD set_iniciar_faturamento.

    DATA: l_jobname   TYPE tbtcjob-jobname,
          l_name      TYPE tbtcjob-jobname,
          l_number    TYPE tbtcjob-jobcount,
          w_zlest0241 TYPE zlest0241.

*------------------------------------
*-- dados faturamento
*------------------------------------
    IF     i_ch_faturamento IS NOT INITIAL.
      SELECT SINGLE *
        FROM zlest0240
        INTO me->at_zlest0240
       WHERE ch_faturamento = i_ch_faturamento
         AND cancelado      = abap_off.

      SELECT *
        FROM zlest0241
        INTO TABLE me->at_zlest0241
       WHERE ch_faturamento = i_ch_faturamento
         AND selecionado    = abap_off
         AND cancelado      = abap_off.

    ELSEIF i_ch_referencia  IS NOT INITIAL.
      SELECT *
        FROM zlest0241
        INTO TABLE me->at_zlest0241
       WHERE ch_referencia  = i_ch_referencia
         AND selecionado    = abap_off
         AND cancelado      = abap_off.
    ENDIF.

*------------------------------------
*-- gerar JOBS
*------------------------------------
    LOOP AT me->at_zlest0241 INTO w_zlest0241.

*---- para DEBUG --------------------
      IF 1 = 2.
        SUBMIT zlesr0180_job WITH p_ch_fat = w_zlest0241-ch_faturamento
                             WITH p_it_fat = w_zlest0241-item_faturamento
                              AND RETURN.
        RETURN.
      ENDIF.

*------------------------------------
*---- criar job execucao
*------------------------------------
      l_jobname = |FATURA_AUT|.
      l_name    = l_jobname && '_' && w_zlest0241-doc_ref_origem.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          jobname          = l_name
        IMPORTING
          jobcount         = l_number
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.

      IF sy-subrc <> 0.
        set_mensagem( '17' ).
      ENDIF.

      SUBMIT zlesr0180_job WITH p_ch_fat = w_zlest0241-ch_faturamento
                           WITH p_it_fat = w_zlest0241-item_faturamento
                        VIA JOB l_name
                         NUMBER l_number
                            AND RETURN.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = l_number
          jobname              = l_name
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

*------------------------------------
*-- atualiza tabela com job criado
*------------------------------------
      w_zlest0241-nome_job = l_name.
      MODIFY zlest0241  FROM w_zlest0241.

      COMMIT WORK.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_mensagem.

    DATA: l_mesg1 TYPE syst_msgv,
          l_mesg2 TYPE syst_msgv,
          l_mesg3 TYPE syst_msgv,
          l_mesg4 TYPE syst_msgv.

*------------------------------
*-- separar string
*------------------------------
    me->set_trata_string( EXPORTING i_mesg  = i_mesg
                          IMPORTING e_msgv1 = l_mesg1
                                    e_msgv2 = l_mesg2
                                    e_msgv3 = l_mesg3
                                    e_msgv4 = l_mesg4 ).

    CASE i_cod.

      WHEN '01'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Metodo informado não previsto!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Metodo informado não previsto!' ).

      WHEN '02'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Payload Requisição não pode ser vazio!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Payload Requisição não pode ser vazio!' ).

      WHEN '03'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Romaneio nao foi Localizado!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Romaneio nao foi Localizado!' ).

      WHEN '04'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Somente previsto automação para tipo de frete CIF!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Somente previsto automação para tipo de frete CIF!' ). "*-#143658-19.06.2024-JT

      WHEN '05'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Agente de Frete nao cadastrado!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Agente de Frete nao cadastrado!' ).

      WHEN '06'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Administrador de Frete incorreto!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Administrador de Frete incorreto!' ).

      WHEN '07'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'ID Rota nao cadastrada!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'ID Rota nao cadastrada!' ).

      WHEN '08'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Administrador de Pedagio incorreto!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Administrador de Pedagio incorreto!' ).

      WHEN '09'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Tipo Cartao Pedagio incorreto!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Tipo Cartao Pedagio incorreto!' ).

      WHEN '10'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Codigo do Domicílio Fiscal Origem nao cadastrado!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Codigo do Domicílio Fiscal Origem nao cadastrado!' ).

      WHEN '11'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Codigo do Domicílio Fiscal Destino nao cadastrado!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Codigo do Domicílio Fiscal Destino nao cadastrado!' ).

      WHEN '12'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'UF Percurso nao cadastrada!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'UF Percurso nao cadastrada!' ).

      WHEN '13'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Nao foi possivel calcular o valor do FRETE!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Nao foi possivel calcular o valor do FRETE!' ).

      WHEN '14'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Nao foi possivel calcular custos do PEDAGIO!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Nao foi possivel calcular custos do PEDAGIO!' ).

      WHEN '15'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Nao foi possivel recuperar Protocolo!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Nao foi possivel recuperar Protocolo!' ).

      WHEN '16'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Filial nao utiliza Faturamento Automatico!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Filial nao utiliza Faturamento Automatico!' ).

      WHEN '17'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'JOB nao pode ser criado!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'JOB nao pode ser criado!' ).

      WHEN '18'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Nao foi possivel gerar Chave Integracao!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Nao foi possivel gerar Chave Integracao!' ).

      WHEN '19'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'ATENCAO: Faturamento esta em andamento!!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'ATENCAO: Faturamento esta em andamento!!' ).

      WHEN '20'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'DANFE Aguardando Autorização da SEFAZ!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'DANFE Aguardando Autorização da SEFAZ!' ).

      WHEN '21'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'DACTE Aguardando Autorização da SEFAZ!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'DACTE Aguardando Autorização da SEFAZ!' ).

      WHEN '22'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'MDF-e Aguardando Autorização da SEFAZ!' ) ) "*-#166749-20.02.2025-JT
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'MDF-e Aguardando Autorização da SEFAZ!' ).                   "*-#166749-20.02.2025-JT

      WHEN '23'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Faltam informar os desmembramentos do Romaneio!!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Faltam informar os desmembramentos do Romaneio!!' ).

      WHEN '24'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Protocolo não Localizado!!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Protocolo não Localizado!!' ).

      WHEN '25'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Não pode Cancelar! Ha documentos emitidos!!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Não pode Cancelar! Ha documentos emitidos!!' ).

      WHEN '26'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Erro ao Eliminar Doc.Transporte!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Erro ao Eliminar Doc.Transporte!' ).

      WHEN '27'.  "*-#143658-19.06.2024-JT
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Grp.Mercadoria nao Localizado!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Grp.Mercadoria nao Localizado!' ).

      WHEN '28'.  "*-#143658-19.06.2024-JT
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Cockpit não Suportado p/ Faturamento Automatico!' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Cockpit não Suportado p/ Faturamento Automatico!' ).

      WHEN '99' OR '099'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = '99' "zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( l_mesg1 )
                              attr2 = CONV #( l_mesg2 )
                              attr3 = CONV #( l_mesg3 )
                              attr4 = CONV #( l_mesg4 ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = '99' "zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( l_mesg1 )
            msgv2  = CONV #( l_mesg2 )
            msgv3  = CONV #( l_mesg3 )
            msgv4  = CONV #( l_mesg4 ).

*-------------------------------
*---- para tratamento de retornos de erros de BAPIs
*-------------------------------
      WHEN '999'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = '999' "zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( l_mesg1 )
                              attr2 = CONV #( l_mesg2 )
                              attr3 = CONV #( l_mesg3 )
                              attr4 = CONV #( l_mesg4 ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = '999' "zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( l_mesg1 )
            msgv2  = CONV #( l_mesg2 )
            msgv3  = CONV #( l_mesg3 )
            msgv4  = CONV #( l_mesg4 ).

    ENDCASE.

  ENDMETHOD.


  METHOD set_montar_estrutura.

    DATA: t_ufs TYPE zlese0214_t,
          w_ufs TYPE zlese0214.

    MOVE: i_info_frete-id_agent_frete      TO at_data_request-informacoesfrete-agentefrete-codigo,
          i_info_frete-tp_admim_frete      TO at_data_request-informacoesfrete-administradorfrete-codigo,
          i_info_frete-vl_adiantamento     TO at_data_request-informacoesfrete-adiantamento-valor,
          i_info_frete-ck_credita_ped      TO at_data_request-informacoespedagio-creditapedagio-credita,
          i_info_frete-id_rota_tip_frete   TO at_data_request-informacoespedagio-idrotaadministradora,
          i_info_frete-nm_qtd_eixos        TO at_data_request-informacoespedagio-qtdeeixos,
          i_info_frete-vl_pedagio          TO at_data_request-informacoespedagio-valor,
          i_info_frete-tp_admim_ped        TO at_data_request-informacoespedagio-administradorpedagio-codigo,
          i_info_frete-tp_card_ped         TO at_data_request-informacoespedagio-cartaopedagio-codigo,
          i_info_frete-cd_cid_origem(2)    TO at_data_request-informacoespedagio-municipios-origem-uf,
          i_info_frete-cd_cid_origem+3(7)  TO at_data_request-informacoespedagio-municipios-origem-codigoibge,
          i_info_frete-cd_cid_destino(2)   TO at_data_request-informacoespedagio-municipios-destino-uf,
          i_info_frete-cd_cid_destino+3(7) TO at_data_request-informacoespedagio-municipios-destino-codigoibge.

    LOOP AT i_info_frete-ufspercurso INTO DATA(w_ufsper).
      w_ufs-ordem     = w_ufsper-ordem.
      w_ufs-uf        = w_ufsper-uf.
      APPEND w_ufs   TO t_ufs.
    ENDLOOP.

    at_data_request-informacoespedagio-ufspercurso[] = t_ufs[].

  ENDMETHOD.


  METHOD SET_MONTAR_INFORMACOES_BASE.

*    DATA: v_cd_uf           TYPE zlest0002-cd_uf,
*          v_cont_fre        TYPE i,
*          wa_mara           TYPE mara,
*          wa_vbak           TYPE ty_vbak,
*          wa_tvakt          TYPE tvakt,
*          wa_t161t          TYPE t161t,
*          wa_vbkd           TYPE vbkd,
*          wa_tvtk           TYPE tvtk,
*          wa_ekko           TYPE ty_ekko,
*          wa_ekpo           TYPE ty_ekpo,
*          wa_vbpa           TYPE ty_vbpa,
*          wa_vbpa_2         TYPE ty_vbpa,
*          wa_kna1           TYPE kna1,
*          wa_lfa1           TYPE lfa1,
*          w_lfa1            TYPE lfa1,
*          wa_t001w          TYPE t001w,
*          wa_makt           TYPE makt,
*          wa_vbpa_cr        TYPE vbpa, "Ponto de coleta  REMESSA
*          wa_vbpa_co        TYPE vbpa, "Ponto de coleta  ORDEM
*          wa_ekpa_pr        TYPE ekpa, "Ponto de coleta  Pedido
*          wa_vbap           TYPE vbap, "Itinerário  ORDE M
*          wa_ekpv           TYPE ekpv, "Itinerário  PEDIDO
*          t_fatura_agrupada TYPE TABLE OF zsdt0121,
*          w_fatura_agrupada TYPE zsdt0121.
*
*    CLEAR: at_saida, at_edita_agente_frete.
*
*    e_status_code = '200'.
*
**----------------------------
**-- obter dados frete
**----------------------------
*    get_dados_frete( IMPORTING e_status_code = e_status_code
*                     RECEIVING r_msg_erro    = r_msg_erro ).
*
*    CHECK e_status_code = '200'.
*
**----------------------------
**-- prepara dados saida
**----------------------------
*    SELECT *
*      FROM zsdt0121
*      INTO TABLE t_fatura_agrupada.
*
*    at_saida-bukrs           = at_zsdt0001-bukrs.
*    at_saida-branch          = at_zsdt0001-branch.
*    at_saida-ch_referencia   = at_zsdt0001-ch_referencia.
*    at_saida-tp_movimento    = at_zsdt0001-tp_movimento.
*    at_saida-dt_movimento    = at_zsdt0001-dt_movimento.
*    at_saida-nr_romaneio     = at_zsdt0001-nr_romaneio.
*    at_saida-nro_cg          = at_zsdt0001-nro_cg.
*    at_saida-placa_cav       = at_zsdt0001-placa_cav.
*    at_saida-qtde_remessa    = at_zsdt0001-qtde_remessa.
*    at_saida-um_remessa      = at_zsdt0001-um_remessa.
*
*    IF at_zsdt0001-region IS NOT INITIAL.
*      at_saida-region        = at_zsdt0001-region.
*    ELSE.
*      SELECT SINGLE cd_uf
*        FROM zlest0002
*        INTO v_cd_uf
*        WHERE pc_veiculo = at_zsdt0001-placa_cav.
*      IF sy-subrc = 0.
*        at_saida-region      = v_cd_uf.
*      ENDIF.
*    ENDIF.
*
*    at_saida-route           = at_zsdt0001-route.
*    at_saida-st_proc         = at_zsdt0001-st_proc.
*    at_saida-shtyp           = at_zsdt0001-shtyp.
*
*    CLEAR wa_vbak.
*    READ TABLE at_t_vbak INTO wa_vbak WITH KEY vbeln = at_zsdt0001-vbeln BINARY SEARCH. " Ordem
*    IF sy-subrc = 0.
*      at_saida-tipo = 'O'.
*      "Ponto de coleta ordem
*      READ TABLE at_t_vbpa_co INTO wa_vbpa_co WITH KEY vbeln = at_zsdt0001-vbeln BINARY SEARCH. " Ordem
*      IF sy-subrc = 0.
*        at_saida-lifnr_c = wa_vbpa_co-lifnr.
*        READ TABLE at_t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa_co-lifnr BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          at_saida-name1_c = wa_lfa1-name1.
*          at_saida-regio_c = wa_lfa1-regio.
*        ENDIF.
*      ENDIF.
*
*      "Zona Local de entrega ordem
*      READ TABLE at_t_vbpa INTO wa_vbpa  WITH KEY vbeln = at_zsdt0001-vbeln
*                                                  parvw = 'LR' BINARY SEARCH.
*      IF sy-subrc = 0.
*        at_saida-local_entrega = wa_vbpa-kunnr.
*      ENDIF.
*
*      READ TABLE at_t_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak-auart BINARY SEARCH.
*      CONCATENATE wa_vbak-auart '-' wa_tvakt-bezei INTO at_saida-operacao.
*      CLEAR wa_kna1.
*      READ TABLE at_t_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
*      at_saida-kunnr           = wa_kna1-kunnr.
*      at_saida-name1           = wa_kna1-name1.
*
*      READ TABLE at_t_vbkd INTO wa_vbkd WITH KEY vbeln = at_zsdt0001-vbeln BINARY SEARCH.
*      IF sy-subrc = 0 .
*        at_saida-inco1           = wa_vbkd-inco1.
*      ENDIF.
*
*    ELSE.
*      at_saida-tipo = 'P'.
*      "Ponto de coleta pedido
*      READ TABLE at_t_ekpa_pr INTO wa_ekpa_pr WITH KEY ebeln = at_zsdt0001-vbeln BINARY SEARCH. " Ordem
*      IF sy-subrc = 0.
*        at_saida-lifnr_c = wa_ekpa_pr-lifn2.
*        READ TABLE at_t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekpa_pr-lifn2 BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          at_saida-name1_c = wa_lfa1-name1.
*          at_saida-regio_c = wa_lfa1-regio.
*        ENDIF.
*      ENDIF.
*
*      READ TABLE at_t_ekko INTO wa_ekko WITH KEY ebeln = at_zsdt0001-vbeln BINARY SEARCH. " Pedidos de transferencia
*      IF sy-subrc = 0.
*        at_saida-bsart = wa_ekko-bsart.
*
*        READ TABLE at_t_t161t INTO wa_t161t WITH KEY bsart = wa_ekko-bsart BINARY SEARCH.
*        CONCATENATE wa_ekko-bsart '-' wa_t161t-batxt  INTO at_saida-operacao.
*
*        READ TABLE at_t_ekpo INTO wa_ekpo WITH KEY ebeln = at_zsdt0001-vbeln BINARY SEARCH. " Pedidos de transferencia
*        IF sy-subrc = 0.
*          at_saida-inco1           = wa_ekpo-inco1.
*        ENDIF.
*
*        CLEAR wa_lfa1.
*        READ TABLE at_t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekpo-lifnr BINARY SEARCH.
*        at_saida-kunnr           = wa_lfa1-lifnr.
*        at_saida-name1           = wa_lfa1-name1.
*
*        "Zona Local de entrega Pedido
*        READ TABLE at_t_vbpa INTO wa_vbpa WITH KEY vbeln = at_zsdt0001-doc_rem
*                                                   parvw = 'LR'.  "Local de entrega
*
*        IF ( sy-subrc EQ 0 ) AND ( at_zsdt0001-doc_rem IS NOT INITIAL ).
*          at_saida-local_entrega = wa_vbpa-kunnr.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    IF at_zsdt0001-kbetr GT 0.
*      at_saida-kbetr = at_zsdt0001-kbetr.
*      at_saida-konwa = at_zsdt0001-konwa.
*      v_cont_fre = 1.
*    ENDIF.
*
*    at_saida-vbeln           = at_zsdt0001-vbeln.
*    at_saida-peso_liq        = at_zsdt0001-peso_liq.
*
*    IF at_zsdt0001-agente_frete IS NOT INITIAL.
*      at_saida-lifnr = at_zsdt0001-agente_frete.
*      READ TABLE at_t_vbpa INTO wa_vbpa  WITH KEY vbeln = at_zsdt0001-vbeln
*                                                  parvw = 'LR' BINARY SEARCH.
*      IF sy-subrc = 0.
*        READ TABLE at_t_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr BINARY SEARCH.
*        IF sy-subrc = 0.
*          at_saida-kunnr           = wa_kna1-kunnr.
*          at_saida-name1           = wa_kna1-name1.
*        ENDIF.
*      ENDIF.
*    ELSEIF at_saida-tipo = 'O'.
*      IF ( 'ZRFL_ZRDC_ZIND' CS wa_vbak-auart ) AND
*         ( wa_vbak-auart IS NOT INITIAL ) AND
*         ( wa_vbkd-inco1 = 'CIF'        ).
*        READ TABLE at_t_vbpa INTO wa_vbpa  WITH KEY vbeln = at_zsdt0001-vbeln
*                                                    parvw = 'SP' BINARY SEARCH.
*        IF sy-subrc = 0.
*          at_saida-lifnr = wa_vbpa-lifnr.
*          set_troca_agente( EXPORTING i_placa_cav = at_zsdt0001-placa_cav
*                                      i_branch    = at_zsdt0001-branch
*                            CHANGING  c_lifnr     = at_saida-lifnr ).
*        ENDIF.
*      ELSE.
*        READ TABLE at_t_vbpa INTO wa_vbpa  WITH KEY vbeln = at_zsdt0001-vbeln
*                                                    parvw = 'LR' BINARY SEARCH.
*        IF sy-subrc = 0.
*          READ TABLE at_t_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr BINARY SEARCH.
*          IF sy-subrc = 0.
*            at_saida-kunnr = wa_kna1-kunnr.
*            at_saida-name1 = wa_kna1-name1.
*          ENDIF.
*        ENDIF.
*
*        IF ( t_fatura_agrupada IS NOT INITIAL ) AND
*           ( 'CFR_FOB' CS at_saida-inco1      ) AND
*           ( at_saida-inco1 IS NOT INITIAL    ).
*          IF at_saida-lifnr IS NOT INITIAL.
*            at_edita_agente_frete = abap_false.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    IF at_saida-name1 IS INITIAL .
*      CLEAR wa_kna1.
*      READ TABLE at_t_t001w INTO wa_t001w WITH KEY werks = at_zsdt0001-branch  BINARY SEARCH.
*      at_saida-name1           = wa_t001w-name1.
*    ENDIF.
*
*    IF at_saida-lifnr IS INITIAL. "Sugere Agente Frete
*      TRY .
*          zcl_faturamento=>zif_faturamento~get_instance( )->get_agente_frete(
*          EXPORTING
*            i_bukrs                =    CONV #( at_zsdt0001-bukrs ) "CS2022000236 - 25.02.2022 - JT - inicio
*            i_placa                =    CONV #( at_zsdt0001-placa_cav )
*            i_uf_origem_mercadoria =    CONV #( at_saida-regio_c  )
*           IMPORTING
*             e_agente_frete         =   at_saida-lifnr ).
*
*        CATCH zcx_faturamento INTO DATA(_cx_fat).
*        CATCH zcx_error INTO DATA(_cx_error).
*      ENDTRY.
*    ENDIF.
*
*    IF at_saida-lifnr IS NOT INITIAL.
*      at_edita_agente_frete = abap_false.
*    ELSE.
*      at_edita_agente_frete = abap_true.
*    ENDIF.
*
**----------------------------
**-- obter dados frete
**----------------------------
*    set_preparar_dados( IMPORTING e_status_code = e_status_code
*                        RECEIVING r_msg_erro    = r_msg_erro ).

  ENDMETHOD.


  METHOD set_montar_json.

    DATA: t_values_adm_ped   TYPE crm_isu_wael_ddicdomavalues,
          t_values_card_ped  TYPE crm_isu_wael_ddicdomavalues,
          t_values_adm_frete TYPE crm_isu_wael_ddicdomavalues,
          t_adm_frete        TYPE zlese0215,
          w_adm_frete        TYPE zlese0205,
          t_adm_ped          TYPE zlese0215,
          w_adm_ped          TYPE zlese0205,
          t_card_ped         TYPE zlese0255_t,
          w_card_ped         TYPE zlese0255,
          t_pracas_ped       TYPE zlese0216_t,
          w_pracas_ped       TYPE zlese0216,
          l_eixo             TYPE numc1,
          l_campo            TYPE char30,
          l_json             TYPE zlese0201,
          l_frete_cod        TYPE string,  "*-BUG160728-13.12.2024-#160728-JT
          l_pedag_cod        TYPE string.  "*-BUG160728-13.12.2024-#160728-JT

    FIELD-SYMBOLS: <f_val>  TYPE any.

    CHECK e_status_code = '200'.

    FREE l_json.

*----------------------------
*- obter valores dominios
*----------------------------
    t_values_adm_ped   = cl_crm_eewa_ddicinfo=>getdomainvalues( par_name = 'ZDM_ADM_PEDAGIO' ).
    t_values_card_ped  = cl_crm_eewa_ddicinfo=>getdomainvalues( par_name = 'ZDM_TP_CARD_PED' ).
    t_values_adm_frete = cl_crm_eewa_ddicinfo=>getdomainvalues( par_name = 'ZDM_ADM_PEDAGIO' ).

*----------------------------
*- Informacoes frete
*----------------------------
    l_json-informacoesfrete-agentefrete-codigo           = at_saida-lifnr.
    l_json-informacoesfrete-agentefrete-permitealteracao = at_edita_agente_frete.

    LOOP AT t_values_adm_frete INTO DATA(w_value_adm_frete).
      l_frete_cod                  = w_value_adm_frete-domvalue_l. ""*-BUG160728-13.12.2024-#160728-JT

      CHECK l_frete_cod            = at_zlest0026-tp_admim_frete.  ""*-BUG160728-13.12.2024-#160728-JT

      w_adm_frete-codigo           = w_value_adm_frete-domvalue_l. "*-BUG160728-13.12.2024-#160728-JT
      w_adm_frete-descricao        = w_value_adm_frete-ddtext.
*     w_adm_frete-padrao           = COND #( WHEN w_adm_frete-codigo = at_zlest0026-tp_admim_frete THEN abap_true
*                                                                                                  ELSE abap_off ).
*     w_adm_frete-permitealteracao = abap_false.
      APPEND w_adm_frete          TO t_adm_frete.
    ENDLOOP.

*   l_json-informacoesfrete-administradorfrete[] = t_adm_frete[].
    l_json-informacoesfrete-administradorfrete   = w_adm_frete.
    at_zlest0026-adto                            = trunc( at_zlest0026-adto ).
    l_json-informacoesfrete-adiantamento-valor   = at_zlest0026-adto.

*----------------------------
*- Informacoes pedagio
*----------------------------
    l_json-informacoespedagio-creditapedagio-credita          = at_zlest0026-ck_credita_ped.
    l_json-informacoespedagio-creditapedagio-permitealteracao = at_edita_ck_credita_ped.
    l_json-informacoespedagio-idrotaadministradora            = at_zlest0026-id_rota.
    l_json-informacoespedagio-qtdeeixos                       = at_zlest0026-qtd_eixo.
    l_json-informacoespedagio-valor                           = at_zlest0026-pedagio.
*   l_json-informacoespedagio-permitealteracao                = at_edita_pedagio.

    LOOP AT t_values_adm_ped    INTO DATA(w_value_adm_ped).
      l_pedag_cod                  = w_value_adm_ped-domvalue_l.  ""*-BUG160728-13.12.2024-#160728-JT

      CHECK l_pedag_cod            = at_zlest0026-tp_admim_ped.   ""*-BUG160728-13.12.2024-#160728-JT

      w_adm_ped-codigo             = w_value_adm_ped-domvalue_l.  ""*-BUG160728-13.12.2024-#160728-JT
      w_adm_ped-descricao          = w_value_adm_ped-ddtext.
*     w_adm_ped-padrao             = COND #( WHEN w_adm_ped-codigo = at_zlest0026-tp_admim_ped THEN abap_true
*                                                                                              ELSE abap_off ).
*     w_adm_ped-permitealteracao   = at_edita_tp_admim_ped.
      APPEND w_adm_ped            TO t_adm_ped.
    ENDLOOP.

    l_json-informacoespedagio-administradorpedagio   = w_adm_ped.
*   l_json-informacoespedagio-administradorpedagio[] = t_adm_ped[].

*----------------------------
*- Informacoes cartao pedagio
*----------------------------
    LOOP AT t_values_card_ped   INTO DATA(w_value_card_ped).
      w_card_ped-codigo             = w_value_card_ped-domvalue_l.
      w_card_ped-descricao          = w_value_card_ped-ddtext.
      w_card_ped-padrao             = COND #( WHEN w_card_ped-codigo = at_zlest0026-tp_card_ped THEN abap_true
                                                                                                ELSE abap_off ).
*     w_card_ped-permitealteracao   = at_edita_tp_card_ped.
      APPEND w_card_ped            TO t_card_ped.
    ENDLOOP.

    l_json-informacoespedagio-cartaopedagio-permitealteracao = at_edita_tp_card_ped.
    l_json-informacoespedagio-cartaopedagio-valores[]        = t_card_ped[].

*----------------------------
*- municipios
*----------------------------
    l_json-informacoespedagio-municipios-origem-uf          = at_zlest0101-cd_cid_origem(2).
    l_json-informacoespedagio-municipios-origem-codigoibge  = at_zlest0101-cd_cid_origem+3(10).
    l_json-informacoespedagio-municipios-destino-uf         = at_zlest0101-cd_cid_destino(2).
    l_json-informacoespedagio-municipios-destino-codigoibge = at_zlest0101-cd_cid_destino+3(10).

*----------------------------
*- pracas pedagio
*----------------------------
    l_eixo  = at_zlest0026-qtd_eixo.
    l_campo = 'W_PRACAS-VL_EIXO' && l_eixo.

    LOOP AT at_t_pracas      INTO DATA(w_pracas).
      w_pracas_ped-empresa      = w_pracas-bukrs.
      w_pracas_ped-localnegocio = w_pracas-branch.
      w_pracas_ped-nomepraca    = w_pracas-nm_praca.
      w_pracas_ped-num          = w_pracas-vl_precisao.

      ASSIGN (l_campo)         TO <f_val>.
      IF sy-subrc = 0.
        w_pracas_ped-qtdeeixo   = <f_val>.
      ENDIF.

      APPEND w_pracas_ped      TO t_pracas_ped.
    ENDLOOP.

    l_json-informacoespedagio-pracaspedagio[]        = t_pracas_ped.

    e_status_code  = '200'.
    r_msg_erro     = abap_off.

    set_response( i_status_code        = e_status_code
                  i_msg_erro           = r_msg_erro
                  i_informacoesfrete   = l_json-informacoesfrete
                  i_informacoespedagio = l_json-informacoespedagio ).

  ENDMETHOD.


  METHOD set_montar_processamento.

    DATA: t_vbpa             TYPE tt_vbpa,
          t_ekpa             TYPE zekpa_tab,
          l_ekko             TYPE ekko,
          l_lifnr            TYPE lifnr,
          l_uf_ini           TYPE regio,
          l_uf_fim           TYPE regio,
          t_ufs              TYPE zlese0214_t,
          w_ufs              TYPE zlese0214,
          t_values_adm_ped   TYPE crm_isu_wael_ddicdomavalues,
          t_values_card_ped  TYPE crm_isu_wael_ddicdomavalues,
          t_values_adm_frete TYPE crm_isu_wael_ddicdomavalues.

    FREE: at_data_request-informacoesfrete,
          at_data_request-informacoespedagio.

*----------------------------
*- obter valores dominios
*----------------------------
    t_values_adm_ped   = cl_crm_eewa_ddicinfo=>getdomainvalues( par_name = 'ZDM_ADM_PEDAGIO' ).
    t_values_card_ped  = cl_crm_eewa_ddicinfo=>getdomainvalues( par_name = 'ZDM_TP_CARD_PED' ).
    t_values_adm_frete = cl_crm_eewa_ddicinfo=>getdomainvalues( par_name = 'ZDM_ADM_PEDAGIO' ).

    READ TABLE t_values_adm_frete INTO DATA(w_value_adm_frete) WITH KEY domvalue_l = at_zlest0026-tp_admim_frete.
    READ TABLE t_values_adm_ped   INTO DATA(w_value_adm_ped)   WITH KEY domvalue_l = at_zlest0026-tp_admim_ped.
    READ TABLE t_values_card_ped  INTO DATA(w_value_card_ped)  WITH KEY domvalue_l = at_zlest0026-tp_card_ped.

*---frete
    at_data_request-informacoesfrete-agentefrete-codigo               = me->at_saida-lifnr.
    at_data_request-informacoesfrete-administradorfrete-codigo        = me->at_zlest0026-tp_admim_frete.
    at_data_request-informacoesfrete-administradorfrete-descricao     = w_value_adm_frete-ddtext.
    at_zlest0026-adto                                                 = trunc( me->at_zlest0026-adto ).
    at_data_request-informacoesfrete-adiantamento-valor               = me->at_zlest0026-adto.

*---pedagio
    at_data_request-informacoespedagio-creditapedagio-credita         = me->at_zlest0026-ck_credita_ped.
    at_data_request-informacoespedagio-idrotaadministradora           = me->at_zlest0026-id_rota.
    at_data_request-informacoespedagio-qtdeeixos                      = me->at_zlest0026-qtd_eixo.
    at_data_request-informacoespedagio-valor                          = me->at_zlest0026-pedagio.
    at_data_request-informacoespedagio-administradorpedagio-codigo    = me->at_zlest0026-tp_admim_ped.
    at_data_request-informacoespedagio-administradorpedagio-descricao = w_value_adm_ped-ddtext.
    at_data_request-informacoespedagio-cartaopedagio-codigo           = me->at_zlest0026-tp_card_ped.
    at_data_request-informacoespedagio-cartaopedagio-descricao        = w_value_card_ped-ddtext.
    at_data_request-informacoespedagio-municipios-origem-uf           = me->at_zlest0101-cd_cid_origem(2).
    at_data_request-informacoespedagio-municipios-origem-codigoibge   = me->at_zlest0101-cd_cid_origem+3(10).
    at_data_request-informacoespedagio-municipios-destino-uf          = me->at_zlest0101-cd_cid_destino(2).
    at_data_request-informacoespedagio-municipios-destino-codigoibge  = me->at_zlest0101-cd_cid_destino+3(10).

*-------------------------
*-- obter parceiros
*-------------------------
    TRY.
        zcl_ordem_venda=>zif_ordem_venda~get_instance(
          )->set_ordem_venda(     EXPORTING i_vbeln = me->at_zsdt0001-vbeln
          )->get_parceiros(       IMPORTING e_vbpa  = t_vbpa
          ).
      CATCH zcx_ordem_venda INTO DATA(ex_ordem).
        "Se não Achou a Ordem de Venda procura o Pedido de Compra
        IF zcx_ordem_venda=>zcx_ordem_venda_nao_existe-msgid EQ ex_ordem->msgid AND
           zcx_ordem_venda=>zcx_ordem_venda_nao_existe-msgno EQ ex_ordem->msgno.

          TRY .
              zcl_pedido_compra=>get_instance(
                )->set_pedido_pc( EXPORTING i_ebeln         = me->at_zsdt0001-vbeln
                )->get_pedido_pc( IMPORTING e_pedido_compra = l_ekko
                )->get_parceiros( IMPORTING e_ekpa          = t_ekpa
                ).
            CATCH zcx_pedido_compra INTO DATA(ex_pedido_compra).
          ENDTRY.
        ENDIF.
    ENDTRY.

    IF t_vbpa[] IS NOT INITIAL.
      READ TABLE t_vbpa INTO DATA(w_vbpa) WITH KEY parvw = 'PC'.
      IF sy-subrc = 0.
        SELECT SINGLE region
          INTO l_uf_ini
          FROM adrc
         WHERE addrnumber = w_vbpa-adrnr.
      ENDIF.

      READ TABLE t_vbpa INTO w_vbpa       WITH KEY parvw = 'LR'.
      IF sy-subrc = 0.
        SELECT SINGLE region
          INTO l_uf_fim
          FROM adrc
         WHERE addrnumber = w_vbpa-adrnr.
      ENDIF.

    ELSEIF t_ekpa[] IS NOT INITIAL.
      IF l_ekko-bsart = 'ZUB'.
        READ TABLE t_ekpa INTO DATA(w_ekpa) WITH KEY parvw = 'PR'.
        IF sy-subrc = 0.
          SELECT SINGLE regio
            INTO l_uf_ini
            FROM lfa1
           WHERE lifnr = w_ekpa-lifn2.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = me->at_zsdt0001-branch
          IMPORTING
            output = l_lifnr.

        SELECT SINGLE regio
          INTO l_uf_fim
          FROM lfa1
         WHERE lifnr = l_lifnr.
      ENDIF.
    ENDIF.

*-------------------------
*-- montar UFs intermediarias
*-------------------------
    SELECT *
      FROM zlest0245
      INTO TABLE @DATA(t_0245)
     WHERE uf_ini = @l_uf_ini
       AND uf_fim = @l_uf_fim.

    IF sy-subrc = 0.
      LOOP AT t_0245 INTO DATA(w_0245).
        w_ufs-ordem     = w_0245-ordem.
        w_ufs-uf        = w_0245-uf_intermediaria.
        APPEND w_ufs   TO t_ufs.
      ENDLOOP.

      at_data_request-informacoespedagio-ufspercurso[] = t_ufs[].
    ENDIF.

  ENDMETHOD.


  METHOD set_preparar_dados.

    DATA: t_xvttk        TYPE TABLE OF vttkvb,
          t_xvtts        TYPE TABLE OF vttsvb,
          w_xvttk        TYPE vttkvb,
          w_xvtts        TYPE vttsvb,
          w_zlest0240    TYPE zlest0240,
          w_faturaut_in  TYPE zlese0220,
          w_faturaut_out TYPE zlese0221.

    FREE: t_xvttk, t_xvtts, w_faturaut_in, w_zlest0240.

    at_edita_agente_frete = COND #( WHEN at_zsdt0001-agente_frete IS INITIAL THEN abap_true
                                                                             ELSE abap_off ).

    CHECK me->get_ck_desmembramento( at_zsdt0001-ch_referencia )-calcula_frete = abap_true.

*------------------------
*-- preencher estrutura
*------------------------
    w_xvttk-route                 = at_zsdt0001-route.
    w_xvttk-shtyp                 = at_zsdt0001-shtyp.
    w_xvttk-tplst                 = at_zsdt0001-branch.
    w_xvttk-tdlnr                 = at_zsdt0001-agente_frete.
    w_xvttk-tplst                 = at_zsdt0001-branch.
    w_xvttk-text1                 = at_zsdt0001-placa_cav.
    w_xvttk-text2                 = at_zsdt0001-placa_car1.
    w_xvttk-text3                 = at_zsdt0001-placa_car2.
    w_xvttk-text4                 = at_zsdt0001-placa_car3.
    w_xvttk-dalen                 = at_zsdt0001-dt_criacao.
    APPEND w_xvttk               TO t_xvttk.

    w_xvtts-vsart                 = '1'.
    w_xvtts-knota                 = at_zsdt0001-lzonea.
    w_xvtts-knotz                 = at_zsdt0001-lzonez.
    APPEND w_xvtts               TO t_xvtts.

    w_faturaut_in-route           = at_zsdt0001-route.
    w_faturaut_in-agente_frete    = at_zsdt0001-agente_frete.
    w_faturaut_in-kunnr           = at_saida-kunnr.
    w_faturaut_in-parid           = at_zsdt0001-parid.
    w_faturaut_in-shtyp           = at_zsdt0001-shtyp.
    w_faturaut_in-branch          = at_zsdt0001-branch.
    w_faturaut_in-vsart           = '1'.
    w_faturaut_in-lzonez          = at_zsdt0001-lzonez.
    w_faturaut_in-lzonea          = at_zsdt0001-lzonea.
    w_faturaut_in-placa_cav       = at_zsdt0001-placa_cav.
    w_faturaut_in-placa_car1      = at_zsdt0001-placa_car1.
    w_faturaut_in-placa_car2      = at_zsdt0001-placa_car2.
    w_faturaut_in-placa_car3      = at_zsdt0001-placa_car3.
    w_faturaut_in-valor_frete     = at_valor_frete.
    w_faturaut_in-moeda_frete     = at_moeda_frete.

    w_zlest0240-tp_admim_frete    = at_data_request-informacoesfrete-administradorfrete-codigo.
    w_zlest0240-tp_admim_ped      = at_data_request-informacoespedagio-administradorpedagio-codigo.
    w_zlest0240-vl_adiantamento   = at_data_request-informacoesfrete-adiantamento-valor.
    w_zlest0240-id_rota_tip_frete = at_data_request-informacoespedagio-idrotaadministradora.
    w_zlest0240-nm_qtd_eixos      = at_data_request-informacoespedagio-qtdeeixos.
    w_zlest0240-ck_credita_ped    = at_data_request-informacoespedagio-creditapedagio-credita.
    w_zlest0240-cd_cid_origem     = |{ at_data_request-informacoespedagio-municipios-origem-uf } { at_data_request-informacoespedagio-municipios-origem-codigoibge }|.
    w_zlest0240-cd_cid_destino    = |{ at_data_request-informacoespedagio-municipios-destino-uf } { at_data_request-informacoespedagio-municipios-destino-codigoibge }|.
    w_zlest0240-tx_obs_cred_mesm  = 'Credita Pedagio'.

*------------------------
*-- informacoes pedagio
*------------------------
    CALL FUNCTION 'Z_LES_ADTO_PEDAGIO'
      EXPORTING
        start_column   = 80
        start_row      = 02
        end_column     = 130
        end_row        = 20
        i_fatura_aut   = w_faturaut_in
        i_zlest0240_in = w_zlest0240
      IMPORTING
        e_fatura_aut   = w_faturaut_out
      TABLES
        c_xvttk        = t_xvttk
        c_xvtts        = t_xvtts
      EXCEPTIONS
        erro           = 1
        pedagio        = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      set_mensagem( '14' ).
    ENDIF.

*------------------------
*-- montar estruturas
*------------------------
    MOVE-CORRESPONDING w_faturaut_out        TO at_zlest0026.
    MOVE-CORRESPONDING w_faturaut_out        TO at_zlest0101.
    MOVE-CORRESPONDING w_faturaut_out-pracas TO at_t_pracas.
    MOVE w_faturaut_out-edita_tp_admim_ped   TO at_edita_tp_admim_ped.
    MOVE w_faturaut_out-edita_tp_card_ped    TO at_edita_tp_card_ped.
    MOVE w_faturaut_out-edita_pedagio        TO at_edita_pedagio.
    MOVE w_faturaut_out-edita_ck_credita_ped TO at_edita_ck_credita_ped.

  ENDMETHOD.


  METHOD SET_PREPARAR_DADOS_OLD.

*    DATA: repom_roteiro        TYPE REF TO zcl_repom_roteiro_vlr_vpr,
*          i_veiculo_eixos      TYPE zqt_eixo,
*          pck_open_pedagio     TYPE char01,
*          wk_xvttk             TYPE vttkvb,
*          wa_zlest0027         TYPE zlest0027,
*          vg_pedi_mesmo_veicul TYPE char01,
*          vg_pedi_mesma_carga  TYPE char01,
*          vg_pedi_pedagio      TYPE char01,
*          vg_pedi_informado    TYPE char01,
*          vg_ck_adiantamento   TYPE char01,
*          vg_ck_alterou_cidade TYPE char01,
*          vg_ck_credito_ped    TYPE char01,
*          vg_ck_admim_frete    TYPE char01,
*          vg_ck_admim_ped      TYPE char01,
*          vg_ck_ped_param      TYPE char01,
*          vg_solicita_pedagio  TYPE char01,
*          vl_route             TYPE vttk-route,
*          vl_tplst             TYPE vttk-tplst,
*          vl_tdlnr             TYPE vttk-tdlnr,
*          vl_bukrs             TYPE ttds-bukrs,
*          vl_adto              TYPE zlest0026-adto,
*          vg_cartao_pedagio    TYPE char01,
*          vl_laufk             TYPE tvtk-laufk,
*          vl_shtyp             TYPE vttk-shtyp,
*          vl_ctrl              TYPE c,
*          vl_ped_e             TYPE c,
*          vl_bezkz             TYPE tvknt-bezkz,
*          vg_branch            TYPE j_1bbranc_,
*          vg_tdlnr             TYPE tdlnr,
*          wk_netwr_all         TYPE netwr_all,
*          wk_waers_all         TYPE waers_all,
*          wk_zlest0090         TYPE zlest0090,
*          it_zlest0090         TYPE TABLE OF zlest0090,
*          qt_zlest0090         TYPE i,
*          lt_zlest0002         TYPE TABLE OF zlest0002,
*          wa_zlest0002_card    TYPE zlest0002_card,
*          wk_zlest0002         TYPE zlest0002,
*          var_qtd_eixo         TYPE i,
*          wk_trolz             TYPE trolz,
*          wk_lfa1              TYPE lfa1,
*          wk_kna1              TYPE kna1,
*          wk_vtpa              TYPE vtpa,
*          lt_zlest0084         TYPE TABLE OF zlest0084,
*          wk_zlest0084         TYPE zlest0084,
*          wk_zlest0091         TYPE zlest0091,
*          lc_branch            TYPE j_1bbranc_,
*          lc_ck_ped            TYPE zde_ck_pegagio,
*          lc_ok                TYPE char01,
*          lc_percurso          TYPE zlest0122,
*          lc_erro              TYPE zde_repom_erros,
*          p_retornou           TYPE c LENGTH 1,
*          vg_tp_card_ped       TYPE zde_tp_card_ped,
*          vg_tp_admim_ped      TYPE zde_adm_pedagio,
*          vg_tp_admim_frete    TYPE zde_adm_pedagio,
*          vg_credito_ped       TYPE zde_cred_pedagio,
*          wa_zlest0026_aux     TYPE zlest0026,
*          it_pracas            TYPE TABLE OF zlest0102.
*
*    CLEAR: at_zlest0026, at_zlest0101.
*
*    e_status_code         = '200'.
*
*    pck_open_pedagio      = abap_true.
*
*    vg_pedi_pedagio       = space.
*    vg_pedi_informado     = space.
*    vg_ck_adiantamento    = abap_false.
*    vg_pedi_mesmo_veicul  = abap_false.
*    vg_ck_alterou_cidade  = abap_false.
*
*    SELECT SINGLE *
*      INTO wa_zlest0027
*      FROM zlest0027
*     WHERE route = at_saida-route.
*
*    IF sy-subrc <> 0.
*      e_status_code = '400'.
*      r_msg_erro    = 'Itinerário sem determinação p/ pedágio Sim/Não (ZLES0027)!'.
*      set_response( i_status_code = e_status_code i_msg_erro = r_msg_erro ).
*      RETURN.
*    ENDIF.
*
*    DATA(lc_ck_pedagio) = wa_zlest0027-ck_pedagio.
*
**-----------------------------------------------
**-inicio montar dados
**-----------------------------------------------
*    IF vg_ck_credito_ped EQ abap_true.
*      vg_credito_ped = at_zlest0026-ck_credita_ped.
*    ENDIF.
*
*    IF vg_ck_admim_frete EQ abap_true.
*      CLEAR: at_zlest0026-nr_vr_xml_tipf.
*    ENDIF.
*
*    IF vg_ck_admim_ped EQ abap_true.
*      CLEAR: at_zlest0026-tp_card_ped,
*             at_zlest0026-nr_card_ped,
*             at_zlest0026-id_rota,
*             at_zlest0026-qtd_eixo,
*             at_zlest0026-ck_credita_ped,
*             at_zlest0026-tx_obs_cred_mesm,
*             vg_ck_admim_ped,
*             vg_ck_admim_frete,
*             vg_ck_adiantamento.
*    ENDIF.
*
*    vg_tp_card_ped     = at_zlest0026-tp_card_ped.
*    vg_tp_admim_ped    = at_zlest0026-tp_admim_ped.
*    vg_tp_admim_frete  = at_zlest0026-tp_admim_frete.
*
*    CLEAR : vl_route, vl_tplst ,vl_bukrs, at_zlest0026, vl_ctrl, vl_ped_e, vl_laufk, vl_shtyp,
*            vg_pedi_pedagio, vg_cartao_pedagio.
*
*    at_zlest0026-tp_card_ped     = vg_tp_card_ped.
*    at_zlest0026-tp_admim_ped    = vg_tp_admim_ped.
*    at_zlest0026-tp_admim_frete  = vg_tp_admim_frete.
*
*    " Origem e Destino """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    IF at_zlest0101-cd_cid_origem IS INITIAL.
*      CLEAR: wk_vtpa.
*      SELECT SINGLE * FROM lfa1 INTO wk_lfa1 WHERE lifnr EQ at_saida-lifnr_c.
*      at_zlest0101-cd_cid_origem = wk_lfa1-txjcd.
*    ENDIF.
*
*    IF at_zlest0101-cd_cid_destino IS INITIAL.
*      SELECT SINGLE * FROM kna1 INTO wk_kna1 WHERE kunnr EQ at_saida-kunnr.
*      at_zlest0101-cd_cid_destino = wk_kna1-txjcd.
*    ENDIF.
*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*
** Validação do código de percurso
*    SELECT SINGLE laufk INTO vl_laufk FROM tvtk WHERE shtyp = at_saida-shtyp.
*
*    vg_ck_ped_param = abap_true.
*
*    CASE vl_laufk.
**---- Percurso preliminar / Percurso Direto
*      WHEN 1 OR 4.
*        SELECT SINGLE ck_pedagio INTO lc_ck_ped FROM zlest0027 WHERE route = at_saida-route.
*        IF sy-subrc IS INITIAL.
*          CASE lc_ck_ped.
*            WHEN abap_true.
*              vl_ctrl = abap_false.
*            WHEN abap_false.
*              vl_ctrl = abap_true.
*          ENDCASE.
*        ELSE.
*          vg_ck_ped_param = abap_false.
*        ENDIF.
*
**---- Principal, subsequente
*      WHEN OTHERS.
*        SELECT SINGLE bezkz INTO vl_bezkz FROM tvknt WHERE knote = at_zsdt0001-lzonez.
*        IF NOT sy-subrc IS INITIAL OR vl_bezkz <> 'X'.
*          vl_ctrl = 'X'.
*        ENDIF.
*    ENDCASE.
*
*    vg_branch = at_saida-branch.
*    vg_tdlnr  = at_saida-lifnr.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = vg_tdlnr
*      IMPORTING
*        output = vg_tdlnr.
*
*    lc_branch = vg_tdlnr.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lc_branch
*      IMPORTING
*        output = lc_branch.
*
*    at_zlest0101-branch = lc_branch.
*
*    IF NOT at_valor_frete IS INITIAL.
*
**---- Local de organizaçao de transporte
*      SELECT SINGLE bukrs INTO vl_bukrs FROM ttds WHERE tplst = at_saida-branch.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = vg_branch
*        IMPORTING
*          output = vg_branch.
*
*      CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
*        EXPORTING
*          centro               = vg_branch
*        IMPORTING
*          centro_out           = vg_branch
*        EXCEPTIONS
*          informar_centro      = 1
*          nao_centro_r_virtual = 2
*          informar_centro_out  = 3
*          informar_centro_v    = 4
*          OTHERS               = 5.
*      IF sy-subrc <> 0.
*        e_status_code = '400'.
*        r_msg_erro    = sy-msgv1 && sy-msgv2 && sy-msgv3 && sy-msgv4.
*        set_response( i_status_code = e_status_code i_msg_erro = r_msg_erro ).
*        RETURN.
*      ENDIF.
*
*      CLEAR: wk_zlest0002.
*
*      SELECT SINGLE * INTO wk_zlest0002
*        FROM zlest0002
*       WHERE pc_veiculo EQ at_zsdt0001-placa_cav.
*      IF sy-subrc IS INITIAL.
*        DATA(vl_proprietario) = wk_zlest0002-proprietario.
*      ENDIF.
*
*      TRY .
*          DATA(r_margadto) =
*          zcl_calc_frete=>get_valor_adiantamento(
*            EXPORTING
*              i_bukrs        = vl_bukrs
*              i_branch       = vg_branch
*              i_lifnr        = vl_proprietario ).
*
*          IF r_margadto IS NOT INITIAL.
*            vg_ck_adiantamento = abap_true.
*            at_zlest0026-adto = ( at_valor_frete * r_margadto ) / 100.
*          ELSE.
*            at_zlest0026-adto = 0.
*          ENDIF.
*
*        CATCH zcx_calc_frete INTO DATA(ex_calc_frete).    "
*          at_zlest0026-adto = 0.
*          e_status_code = '400'.
*          r_msg_erro    = sy-msgv1 && sy-msgv2 && sy-msgv3 && sy-msgv4.
*          set_response( i_status_code = e_status_code i_msg_erro = r_msg_erro ).
*          RETURN.
*      ENDTRY.
*    ENDIF.
*
**-- Validação se o valor do pedagio precisa ser informado manualmente ou eletronico.
*    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
*      EXPORTING
*        centro               = at_saida-branch
*      IMPORTING
*        centro_real          = wk_zlest0090-werks
*      EXCEPTIONS
*        informar_centro      = 1
*        nao_centro_r_virtual = 2
*        informar_centro_out  = 3
*        informar_centro_v    = 4
*        OTHERS               = 5.
*
*    IF sy-subrc <> 0.
*      e_status_code = '400'.
*      r_msg_erro    = sy-msgv1 && sy-msgv2 && sy-msgv3 && sy-msgv4.
*      set_response( i_status_code = e_status_code i_msg_erro = r_msg_erro ).
*      RETURN.
*    ENDIF.
*
*    "Administradora de Frete """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    IF at_zlest0026-tp_admim_frete IS INITIAL.
*      SELECT SINGLE * FROM zlest0090 INTO wk_zlest0090 WHERE werks      EQ wk_zlest0090-werks
*                                                         AND tp_servico EQ '0'
*                                                         AND ck_default EQ abap_true.
*      IF sy-subrc IS INITIAL.
*        at_zlest0026-tp_admim_frete = wk_zlest0090-tp_adm.
*      ENDIF.
*    ELSE.
*      SELECT SINGLE * FROM zlest0090 INTO wk_zlest0090 WHERE werks      EQ wk_zlest0090-werks
*                                                         AND tp_servico EQ '0'
*                                                         AND tp_adm     EQ at_zlest0026-tp_admim_frete.
*    ENDIF.
*
*    IF sy-subrc IS INITIAL.
*      at_zlest0026-nr_vr_xml_tipf = wk_zlest0090-nr_vr_xml_tipf.
*    ELSEIF at_zlest0026-tp_admim_frete IS INITIAL.
*      at_zlest0026-tp_admim_frete = '09'.
*      at_zlest0026-nr_vr_xml_tipf = '1.17'.
*    ENDIF.
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*
*    "Administradora de Pedágio
*    IF vl_ctrl IS INITIAL AND at_zlest0026-id_proc_cliente IS INITIAL.
*
*      vg_pedi_pedagio = abap_true.
*
*      "Administradora de Pedágio """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*      SELECT * INTO TABLE it_zlest0090 FROM zlest0090 WHERE werks      EQ at_saida-branch
*                                                        AND tp_servico EQ '1'.
*      IF sy-subrc IS INITIAL.
*        IF at_zlest0026-tp_admim_ped IS INITIAL.
*          READ TABLE it_zlest0090 INTO wk_zlest0090 WITH KEY ck_default = abap_true.
*          IF sy-subrc IS INITIAL.
*            at_zlest0026-tp_admim_ped = wk_zlest0090-tp_adm.
*          ELSE.
*            at_zlest0026-tp_admim_ped = '09'.
*          ENDIF.
*        ELSE.
*          SELECT SINGLE * FROM zlest0090 INTO wk_zlest0090 WHERE werks      EQ wk_zlest0090-werks
*                                                             AND tp_servico EQ '1'
*                                                             AND tp_adm     EQ at_zlest0026-tp_admim_ped.
*        ENDIF.
*      ELSE.
*        at_zlest0026-tp_admim_ped = '09'.
*      ENDIF.
*
*      DESCRIBE TABLE it_zlest0090 LINES qt_zlest0090.
*      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*
*      IF ( sy-subrc EQ 0 ).
*
*        CASE wk_zlest0090-tp_op.
*          WHEN 'E'. "Eletronico
*
*            vg_cartao_pedagio = abap_true.
*
*            CASE wk_zlest0090-tp_adm.
*              WHEN '03'. "REPOM S.A.
*
*                "Solicitar Pedágio REPOM
*                vg_solicita_pedagio = abap_true.
*
*                CLEAR: at_zlest0026-tp_card_ped,
*                       at_zlest0026-nr_card_ped,
*                       at_zlest0026-id_rota,
*                       at_zlest0026-qtd_eixo.
*
*                IF vg_ck_credito_ped NE abap_true.
*                  at_zlest0026-ck_credita_ped = abap_true.
*                ELSE.
*                  at_zlest0026-ck_credita_ped = vg_credito_ped.
*                ENDIF.
*
*                SELECT *
*                  FROM zlest0002
*                  INTO TABLE lt_zlest0002
*                 WHERE pc_veiculo IN (at_zsdt0001-placa_cav,at_zsdt0001-placa_car1,at_zsdt0001-placa_car2,at_zsdt0001-placa_car3 ).
*
*                IF ( sy-subrc EQ 0 ).
*                  at_zlest0026-placa_cav = at_zsdt0001-placa_cav.
*
*                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*                    EXPORTING
*                      input  = vg_tdlnr
*                    IMPORTING
*                      output = vg_tdlnr.
*
*                  lc_branch = vg_tdlnr.
*
*                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                    EXPORTING
*                      input  = lc_branch
*                    IMPORTING
*                      output = lc_branch.
*
*                  i_veiculo_eixos = 0.
*                  LOOP AT lt_zlest0002 INTO wk_zlest0002.
*                    i_veiculo_eixos = i_veiculo_eixos + wk_zlest0002-qt_eixo.
*                  ENDLOOP.
*
*                  IF pck_open_pedagio EQ abap_true.
*
*                    SELECT SINGLE * INTO @DATA(wa_j1_bbranch)
*                      FROM j_1bbranch
*                     WHERE branch EQ @lc_branch.
*
*                    CALL FUNCTION 'Z_REPOM_INFORMA_PERCURSO'
*                      EXPORTING
*                        i_branch         = lc_branch
*                        i_bukrs          = wa_j1_bbranch-bukrs
*                        i_cd_cid_origem  = at_zlest0101-cd_cid_origem
*                        i_cd_cid_destino = at_zlest0101-cd_cid_destino
**                       I_QTD_EIXOS      = I_VEICULO_EIXOS
*                      IMPORTING
*                        e_informado      = lc_ok
*                        e_percurso       = lc_percurso
*                      EXCEPTIONS
*                        sem_percurso     = 1
*                        OTHERS           = 2.
*
*                    IF lc_ok EQ abap_true.
*                      CREATE OBJECT repom_roteiro.
*                      repom_roteiro->set_bukrs( EXPORTING i_bukrs = wa_j1_bbranch-bukrs ).
*                      repom_roteiro->set_branch( EXPORTING i_branch = lc_branch ).
*                      repom_roteiro->set_id_rota_repom( EXPORTING i_id_rota_repom = lc_percurso-id_rota_repom ).
*                      repom_roteiro->set_id_percurso_repom( EXPORTING i_id_percurso_repom = lc_percurso-id_percurso_repom ).
*                      repom_roteiro->set_id_rota( EXPORTING i_id_rota = lc_percurso-id_rota ).
*                      repom_roteiro->set_veiculo_eixos( EXPORTING i_veiculo_eixos = i_veiculo_eixos ).
*                      repom_roteiro->set_qtd_eixos_suspensos_ida( EXPORTING i_qtd_eixos_suspensos_ida = 0 ).
*                      repom_roteiro->set_qtd_eixos_suspensos_volta( EXPORTING i_qtd_eixos_suspensos_volta = 0 ).
*
*                      CALL METHOD repom_roteiro->consultar_valor
*                        IMPORTING
*                          e_erros                    = DATA(lc_erros)
*                        RECEIVING
*                          i_retornou                 = p_retornou
*                        EXCEPTIONS
*                          servico_nao_encontrado     = 1
*                          http_communication_failure = 2
*                          http_invalid_state         = 3
*                          http_processing_failed     = 4
*                          http_invalid_timeout       = 5
*                          erro                       = 6
*                          OTHERS                     = 7.
*
*                      IF sy-subrc IS NOT INITIAL.
*                        e_status_code = '400'.
*                        r_msg_erro    = sy-msgv1 && sy-msgv2 && sy-msgv3 && sy-msgv4.
*                        set_response( i_status_code = e_status_code i_msg_erro = r_msg_erro ).
*                        RETURN.
*                      ENDIF.
*
*                      IF p_retornou EQ abap_false.
**                       LOOP AT LC_ERROS INTO LC_ERRO.
**                         MESSAGE W017(ZREPOM) WITH LC_ERRO-ERRO_CODIGO LC_ERRO-ERRO_DESCRICAO.
**                       ENDLOOP.
*                      ELSE.
*                        at_zlest0026-pedagio = repom_roteiro->get_valor_total_vpr( ).
*
*                        CALL FUNCTION 'Z_CK_PEDAGIO_MESMO_VEICULO'
*                          EXPORTING
*                            i_zlest0090     = wk_zlest0090
*                            i_zlest0026     = at_zlest0026
*                            i_zlest0122     = lc_percurso
*                            i_tknum         = wk_xvttk-tknum
*                          IMPORTING
*                            e_mesmo_veiculo = vg_pedi_mesmo_veicul
*                            e_mesma_carga   = vg_pedi_mesma_carga.
*
*                        IF vg_pedi_mesma_carga EQ abap_true.
*                          at_zlest0026-ck_credita_ped = abap_false.
*                          CLEAR: at_zlest0026-pedagio.
*                        ENDIF.
*
*                      ENDIF.
*                      CLEAR: repom_roteiro.
*                    ELSE.
**                     MESSAGE W040(ZREPOM) WITH ZLEST0101-CD_CID_ORIGEM ZLEST0101-CD_CID_DESTINO.
*                    ENDIF.
*                  ENDIF.
*                ENDIF.
*
*              WHEN '09'. "(TipFrete) UNIK S.A.
*
*                SELECT *
*                  FROM zlest0002
*                  INTO TABLE lt_zlest0002
*                 WHERE pc_veiculo IN (at_zsdt0001-placa_cav,at_zsdt0001-placa_car1,at_zsdt0001-placa_car2,at_zsdt0001-placa_car3 ).
*
*                IF ( sy-subrc EQ 0 ).
*
*                  CLEAR: var_qtd_eixo, wk_zlest0091.
*
*                  LOOP AT lt_zlest0002 INTO wk_zlest0002.
*                    var_qtd_eixo = var_qtd_eixo + wk_zlest0002-qt_eixo.
*                    CLEAR: wk_zlest0002.
*                  ENDLOOP.
*
*                  SELECT SINGLE * FROM zlest0091
*                    INTO wk_zlest0091
*                   WHERE qtd_eixo EQ var_qtd_eixo.
*
*                  at_zlest0026-placa_cav = at_zsdt0001-placa_cav.
*
*                  IF at_zlest0026-tp_card_ped IS INITIAL.
*                    "Busca Padrão
*                    SELECT SINGLE *
*                      FROM zlest0002_card
*                      INTO wa_zlest0002_card
*                     WHERE pc_veiculo     EQ at_zlest0026-placa_cav
*                       AND ck_card_padrao EQ 'X'.
*                  ELSE.
*                    "Busca Selecionado
*                    IF at_zlest0026-tp_card_ped NE 'S'.
*                      SELECT SINGLE *
*                        FROM zlest0002_card
*                        INTO wa_zlest0002_card
*                       WHERE pc_veiculo  EQ at_zlest0026-placa_cav
*                         AND tp_card_ped EQ at_zlest0026-tp_card_ped.
*                    ENDIF.
*                  ENDIF.
*
*                  IF sy-subrc IS INITIAL AND wa_zlest0002_card IS NOT INITIAL.
*                    at_zlest0026-tp_card_ped = wa_zlest0002_card-tp_card_ped.
*                    at_zlest0026-nr_card_ped = wa_zlest0002_card-nr_card_ped.
*                  ELSE.
*                    IF at_zlest0026-tp_card_ped IS INITIAL.
*                      IF wk_zlest0090-tp_card_ped_default IS NOT INITIAL.
*                        at_zlest0026-tp_card_ped = wk_zlest0090-tp_card_ped_default.
*                      ELSE.
*                        at_zlest0026-tp_card_ped = 'S'.
*                      ENDIF.
*                      CLEAR: at_zlest0026-nr_card_ped.
*                    ENDIF.
*                  ENDIF.
*
*                  SELECT *
*                    FROM zlest0084
*                    INTO TABLE lt_zlest0084
*                   WHERE branch         EQ at_zlest0101-branch
*                     AND munic_origem   EQ at_zlest0101-cd_cid_origem+3(7)
*                     AND munic_destino  EQ at_zlest0101-cd_cid_destino+3(7)
*                     AND cat_veiculo    EQ wk_zlest0091-categoria
*                     AND prioridade     EQ 'X'.
*
*                  IF ( sy-subrc EQ 0 ).
*                    SORT lt_zlest0084 BY vlr_pedagio ASCENDING.
*                    READ TABLE lt_zlest0084 INTO wk_zlest0084 INDEX 1.
*                    IF ( at_zsdt0001-dt_criacao >= wk_zlest0084-dt_vigencia ).
*                      IF NOT ( wk_zlest0084-vlr_pedagio  IS INITIAL ).
*
*                        IF vg_ck_credito_ped NE abap_true.
*                          at_zlest0026-ck_credita_ped = abap_true.
*                        ELSE.
*                          at_zlest0026-ck_credita_ped = vg_credito_ped.
*                        ENDIF.
*
*                        IF at_zlest0026-ck_credita_ped EQ abap_true.
*                          at_zlest0026-pedagio      = wk_zlest0084-vlr_pedagio.
*                        ENDIF.
*                        at_zlest0026-id_rota        = wk_zlest0084-id_rota.
*                        at_zlest0026-qtd_eixo       = wk_zlest0091-qtd_eixo.
*
*                        CALL FUNCTION 'Z_CK_PEDAGIO_MESMO_VEICULO'
*                          EXPORTING
*                            i_zlest0090     = wk_zlest0090
*                            i_zlest0026     = at_zlest0026
*                            i_tknum         = wk_xvttk-tknum
*                          IMPORTING
*                            e_mesmo_veiculo = vg_pedi_mesmo_veicul
*                            e_mesma_carga   = vg_pedi_mesma_carga.
*
*                        IF vg_pedi_mesma_carga EQ abap_true.
*                          at_zlest0026-ck_credita_ped = abap_false.
*                          CLEAR: at_zlest0026-pedagio.
*                        ENDIF.
*
*                      ENDIF.
*                    ELSE.
*                      e_status_code = '400'.
*                      r_msg_erro    = 'Data de Vigência menor que a do transporte'.
*                      set_response( i_status_code = e_status_code i_msg_erro = r_msg_erro ).
*                      RETURN.
*                    ENDIF.
*                  ENDIF.
*                ENDIF.
*            ENDCASE.
*
*          WHEN 'M'. "Manual
*            SELECT *
*              FROM zlest0002
*              INTO TABLE lt_zlest0002
*             WHERE pc_veiculo IN (at_zsdt0001-placa_cav,at_zsdt0001-placa_car1,at_zsdt0001-placa_car2,at_zsdt0001-placa_car3 ).
*
*            IF ( sy-subrc EQ 0 ).
*
*              at_zlest0026-placa_cav = at_zsdt0001-placa_cav.
*
*              CALL FUNCTION 'Z_CK_PEDAGIO_MESMO_VEICULO'
*                EXPORTING
*                  i_zlest0090     = wk_zlest0090
*                  i_zlest0026     = at_zlest0026
*                  i_tknum         = wk_xvttk-tknum
*                IMPORTING
*                  e_mesmo_veiculo = vg_pedi_mesmo_veicul
*                  e_mesma_carga   = vg_pedi_mesma_carga.
*
*              IF vg_pedi_mesma_carga EQ abap_true.
*                CLEAR: at_zlest0026-pedagio.
*              ENDIF.
*            ENDIF.
*        ENDCASE.
*      ENDIF.
*    ELSE.
*      qt_zlest0090 = 0.
*    ENDIF.
*
*    IF at_zlest0026-id_rota IS NOT INITIAL.
*      SELECT *
*        INTO TABLE at_t_pracas
*        FROM zlest0102
*       WHERE id_rota_adm EQ at_zlest0026-id_rota
*         AND branch      EQ at_zlest0101-branch.
*
*      DELETE it_pracas WHERE st_praca = abap_false.
*    ELSEIF at_zlest0026-id_rota IS INITIAL.
*      FREE: at_t_pracas[].
*    ENDIF.
*
**---------------------------------------
**-- exibir / editar campos
**---------------------------------------
*    "Valor do Pedágio (Mostrar/Informar)
*    "Não pode alterar quando: Sem Pedágio ou Pedágio Eletrônico
*    IF vg_pedi_pedagio EQ abap_false OR vg_cartao_pedagio EQ abap_true OR vg_pedi_mesma_carga EQ abap_true.
*      at_edita_pedagio = abap_false.
*    ELSE.
*      at_edita_pedagio = abap_true.
*    ENDIF.
*
*    "Cartão de Pagamento (Mostrar)
*    IF vg_cartao_pedagio            EQ abap_false OR
*       at_zlest0026-tp_admim_ped    EQ '03'       OR
*       at_zlest0026-id_proc_cliente IS NOT INITIAL. ""Processo de Pedágio REPOM Emitido (Bloqueia)
*      at_edita_tp_card_ped = abap_false.
*    ELSE.
*      at_edita_tp_card_ped = abap_true.
*    ENDIF.
*
*    "Credita Pedágio (Mostrar)
*    IF vg_pedi_mesmo_veicul EQ abap_false OR vg_cartao_pedagio EQ abap_false OR
*       at_zlest0026-id_proc_cliente IS NOT INITIAL OR vg_pedi_mesma_carga IS NOT INITIAL.
*      IF at_zlest0026-id_proc_cliente IS NOT INITIAL AND vg_pedi_mesmo_veicul EQ abap_true.
*        at_edita_ck_credita_ped = abap_true.
*      ELSE.
*        at_edita_ck_credita_ped = abap_false.
*      ENDIF.
*    ELSE.
*      at_edita_ck_credita_ped = abap_true.
*    ENDIF.
*
*    "Escolher Administradora Pedágio
*    IF vg_pedi_pedagio EQ abap_false OR at_zlest0026-id_proc_cliente IS NOT INITIAL OR qt_zlest0090 LE 1.
*      at_edita_tp_admim_ped = abap_false.
*    ELSE.
*      at_edita_tp_admim_ped = abap_true.
*    ENDIF.

  ENDMETHOD.


  METHOD set_preparar_faturamento.

    DATA: w_zlest0240 TYPE zlest0240,
          w_zlest0241 TYPE zlest0241,
          w_zlest0243 TYPE zlest0243,
          l_lifnr     TYPE lfa1-lifnr,
          l_item      TYPE zitem_faturamento.

    FREE: w_zlest0240, me->at_zlest0240, me->at_zlest0241, l_item.

    CASE abap_true.

      WHEN at_data_request-inicia_faturamento OR
           at_data_request-preenchimento_info_automatica.

*-------Somente preenchimento automatico, busca informacoes romaneio
*-------e comeca a processar, caso nao haja erro
        IF at_data_request-preenchimento_info_automatica = abap_true.
          TRY.
              set_selecao_dados( ).

            CATCH zcx_error INTO DATA(ex_error).
              DATA(l_mesg) = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
              set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          ENDTRY.

          TRY.
              set_preparar_dados( ).

            CATCH zcx_error INTO ex_error.
              l_mesg = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
              set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          ENDTRY.

          set_montar_processamento( ).
        ENDIF.

*--------------------------
*------ chave faturamento
*--------------------------
        TRY.
            me->at_ch_faturamento = me->get_ch_faturamento( ).
          CATCH zcx_error.
            set_mensagem( '18' ).
        ENDTRY.

        me->at_nr_protocolo       = me->at_ch_faturamento.

*--------------------------
*------ tipo cockpit
*--------------------------
        READ TABLE at_data_request-romaneio INTO DATA(w_romaneio) INDEX 1.
        me->at_cockpit            = me->get_ck_tipo_selecao( w_romaneio-ch_referencia_romaneio ).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = at_data_request-informacoesfrete-agentefrete-codigo
          IMPORTING
            output = l_lifnr.

*---------------------------------------------
*------ Informações de Frete para Faturamento Automatico OPUS
*---------------------------------------------
        w_zlest0240-mandt              = sy-mandt.
        w_zlest0240-ch_faturamento     = me->at_ch_faturamento.
        w_zlest0240-id_agent_frete     = l_lifnr.
        w_zlest0240-tp_admim_frete     = at_data_request-informacoesfrete-administradorfrete-codigo.
        w_zlest0240-vl_adiantamento    = at_data_request-informacoesfrete-adiantamento-valor.
        w_zlest0240-ck_credita_ped     = at_data_request-informacoespedagio-creditapedagio-credita.
        w_zlest0240-id_rota_tip_frete  = at_data_request-informacoespedagio-idrotaadministradora.
        w_zlest0240-nm_qtd_eixos       = at_data_request-informacoespedagio-qtdeeixos.
        w_zlest0240-vl_pedagio         = at_data_request-informacoespedagio-valor.
        w_zlest0240-tx_obs_cred_mesm   = 'Credita Pedagio'.
        w_zlest0240-tp_admim_ped       = at_data_request-informacoespedagio-administradorpedagio-codigo.
        w_zlest0240-tp_card_ped        = at_data_request-informacoespedagio-cartaopedagio-codigo.
        w_zlest0240-cd_cid_origem      = |{ at_data_request-informacoespedagio-municipios-origem-uf } { at_data_request-informacoespedagio-municipios-origem-codigoibge }|.
        w_zlest0240-cd_cid_destino     = |{ at_data_request-informacoespedagio-municipios-destino-uf } { at_data_request-informacoespedagio-municipios-destino-codigoibge }|.
        w_zlest0240-origem             = me->at_origem.
        w_zlest0240-cockpit            = me->at_cockpit.
        w_zlest0240-data_modif         = sy-datum.
        w_zlest0240-hora_modif         = sy-uzeit.
        w_zlest0240-user_modif         = sy-uname.
        MODIFY zlest0240            FROM w_zlest0240.
        me->at_zlest0240               = w_zlest0240.

*---------------------------------------------
*------ Itens faturamento
*---------------------------------------------
        LOOP AT at_data_request-romaneio INTO w_romaneio.
          l_item                       = l_item + 1.

          w_zlest0241-mandt            = sy-mandt.
          w_zlest0241-ch_faturamento   = me->at_ch_faturamento.
          w_zlest0241-item_faturamento = l_item.
          w_zlest0241-ch_referencia    = w_romaneio-ch_referencia_romaneio.
          w_zlest0241-texto_fatura     = w_romaneio-textofaturamento.
          w_zlest0241-doc_ref_origem   = w_romaneio-ch_referencia_romaneio.
          w_zlest0241-selecionado      = abap_off.
          w_zlest0241-job_terminado    = abap_off.  "*-#157580-06.11.2024-JT-inicio
*         w_zlest0241-nr_protocolo     = me->at_nr_protocolo.
          w_zlest0241-status_reg       = 'AGUA'.
          w_zlest0241-data_modif       = sy-datum.
          w_zlest0241-hora_modif       = sy-uzeit.
          w_zlest0241-user_modif       = sy-uname.
          MODIFY zlest0241          FROM w_zlest0241.
        ENDLOOP.
*---------------------------------------------
*------ Informações de Frete - Trajeto
*---------------------------------------------
        LOOP AT at_data_request-informacoespedagio-ufspercurso INTO DATA(w_ufs).
          CLEAR: w_zlest0243.
          w_zlest0243-mandt            = sy-mandt.
          w_zlest0243-ch_faturamento   = me->at_ch_faturamento.
          w_zlest0243-land1            = 'BR'.
          w_zlest0243-bland            = w_ufs-uf.
          w_zlest0243-nm_sequencia     = w_ufs-ordem.
*         w_zlest0243-route            = at_data_request-informacoespedagio-idrotaadministradora.
*         w_zlest0243-cd_cid_origem    = |{ at_data_request-informacoespedagio-municipios-origem-uf } { at_data_request-informacoespedagio-municipios-origem-codigoibge }|.
*         w_zlest0243-cd_cid_destino   = |{ at_data_request-informacoespedagio-municipios-destino-uf } { at_data_request-informacoespedagio-municipios-destino-codigoibge }|.
          MODIFY zlest0243          FROM w_zlest0243.
        ENDLOOP.

        e_status_code  = '200'.
        r_msg_erro     = 'Solicitação recebida! Faturamento iniciado...'.

      WHEN at_data_request-continua_faturamento.

        me->at_ch_faturamento                    = at_data_request-protocolo.

*---------------------------------------------
*------ atualiza informacoes frete
*---------------------------------------------
        IF at_data_request-informacoesfrete IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = at_data_request-informacoesfrete-agentefrete-codigo
            IMPORTING
              output = l_lifnr.

          UPDATE zlest0240 SET id_agent_frete    = l_lifnr
                               tp_admim_frete    = at_data_request-informacoesfrete-administradorfrete-codigo
                               vl_adiantamento   = at_data_request-informacoesfrete-adiantamento-valor
                               data_modif        = sy-datum
                               hora_modif        = sy-uzeit
                               user_modif        = sy-uname
                         WHERE ch_faturamento    = me->at_ch_faturamento
                           AND cancelado         = abap_off.
        ENDIF.

*---------------------------------------------
*------ atualiza informacoes pedagio
*---------------------------------------------
        IF at_data_request-informacoespedagio IS NOT INITIAL.
          w_zlest0240-cd_cid_origem              = |{ at_data_request-informacoespedagio-municipios-origem-uf } { at_data_request-informacoespedagio-municipios-origem-codigoibge }|.
          w_zlest0240-cd_cid_destino             = |{ at_data_request-informacoespedagio-municipios-destino-uf } { at_data_request-informacoespedagio-municipios-destino-codigoibge }|.

          UPDATE zlest0240 SET ck_credita_ped    = at_data_request-informacoespedagio-creditapedagio-credita
                               id_rota_tip_frete = at_data_request-informacoespedagio-idrotaadministradora
                               nm_qtd_eixos      = at_data_request-informacoespedagio-qtdeeixos
                               vl_pedagio        = at_data_request-informacoespedagio-valor
                               tx_obs_cred_mesm  = 'Credita Pedagio'
                               tp_admim_ped      = at_data_request-informacoespedagio-administradorpedagio-codigo
                               tp_card_ped       = at_data_request-informacoespedagio-cartaopedagio-codigo
                               cd_cid_origem     = w_zlest0240-cd_cid_origem
                               cd_cid_destino    = w_zlest0240-cd_cid_destino
                               data_modif        = sy-datum
                               hora_modif        = sy-uzeit
                               user_modif        = sy-uname
                         WHERE ch_faturamento    = me->at_ch_faturamento
                           AND cancelado         = abap_off.
        ENDIF.

        SELECT SINGLE *
          FROM zlest0240
          INTO me->at_zlest0240
         WHERE ch_faturamento = me->at_ch_faturamento
           AND cancelado      = abap_off.

*---------------------------------------------
*------ atualia status registro
*---------------------------------------------
        UPDATE zlest0241 SET selecionado    = abap_off
                             job_terminado  = abap_off  "*-#157580-06.11.2024-JT-inicio
                       WHERE ch_faturamento = me->at_ch_faturamento
                         AND cancelado      = abap_off.

        e_status_code  = '200'.
        r_msg_erro     = 'Solicitação recebida! Faturamento Reiniciado...'.

      WHEN at_data_request-cancela_faturamento.

        me->at_ch_faturamento               = at_data_request-protocolo.

        UPDATE zlest0240 SET cancelado      = abap_true
                       WHERE ch_faturamento = at_data_request-protocolo
                         AND cancelado      = abap_off.

        UPDATE zlest0241 SET cancelado      = abap_true
                       WHERE ch_faturamento = at_data_request-protocolo
                         AND cancelado      = abap_off.

        e_status_code  = '200'.
        r_msg_erro     = 'Cancelamento Efetuato com Sucesso!'.

    ENDCASE.

    COMMIT WORK AND WAIT.

*---------------------------------------------
*-- retorna resposta ao opus
*---------------------------------------------
    IF at_data_request-inicia_faturamento            = abap_true OR
       at_data_request-preenchimento_info_automatica = abap_true.  "*-#143658-19.06.2024-JT-inicio
      set_response_automatico( i_status_code        = e_status_code
                               i_msg_erro           = r_msg_erro
                               i_informacoesfrete   = me->at_data_request-informacoesfrete
                               i_informacoespedagio = me->at_data_request-informacoespedagio ).
    ELSE.
      set_response(            i_status_code        = e_status_code
                               i_msg_erro           = r_msg_erro ).
    ENDIF.

  ENDMETHOD.


  METHOD set_response.

    at_data_response-status-codigo          = COND #( WHEN i_status_code = '200' THEN 'SUCESSO'
                                                                                 ELSE 'PENDENTE' ).
    at_data_response-romaneio[]             = me->at_data_request-romaneio[].
    at_data_response-status-mensagem        = i_msg_erro.
    at_data_response-status-protocolo       = me->at_nr_protocolo.
    at_data_response-informacoesfrete       = i_informacoesfrete.
    at_data_response-informacoespedagio     = i_informacoespedagio.

  ENDMETHOD.


  METHOD set_response_automatico.

    at_data_response_autom-status-codigo      = COND #( WHEN i_status_code = '200' THEN 'SUCESSO'
                                                                                   ELSE 'PENDENTE' ).
    at_data_response_autom-romaneio[]         = me->at_data_request-romaneio[].
    at_data_response_autom-status-mensagem    = i_msg_erro.
    at_data_response_autom-status-protocolo   = me->at_nr_protocolo.
    at_data_response_autom-informacoesfrete   = i_informacoesfrete.
    at_data_response_autom-informacoespedagio = i_informacoespedagio.

  ENDMETHOD.


  METHOD set_retorno_opus.

    DATA: t_zlest0242  TYPE TABLE OF zlest0242,
          w_zlest0242  TYPE zlest0242,
          t_romaneio   TYPE zlese0231_t,
          w_romaneio   TYPE zlese0231,
          t_status     TYPE zlese0232_t,
          w_status     TYPE zlese0232,
          t_idd07v     TYPE TABLE OF dd07v,
          w_idd07v     TYPE dd07v,
          l_metodo     TYPE string,
          r_ch_ref     TYPE RANGE OF zlest0241-ch_referencia,
          l_ch_ref     LIKE LINE OF r_ch_ref,
          r_ch_fat     TYPE RANGE OF zlest0241-ch_faturamento,
          l_ch_fat     LIKE LINE OF r_ch_fat,
          l_time_ret   TYPE numc3,
          l_tentativas TYPE numc3,
          l_tabix      TYPE sy-tabix,
          l_vzeit      TYPE i,
          l_diftime    TYPE i,
          t_value      TYPE TABLE OF rgsb4.

    me->at_tp_referencia = 'FATURA_AUTOM-RETORNO_OPUS'.

    FREE: me->at_zlest0242, me->at_json, me->at_retorno_opus, r_ch_ref, l_time_ret, l_tentativas.

*-------------------------------
*-- parametrizacao
*-------------------------------
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr           = 'PARAM_FATURA_AUTOMATICO'
        class           = '0000'
        no_descriptions = abap_off
      TABLES
        set_values      = t_value
      EXCEPTIONS
        set_not_found   = 1
        OTHERS          = 2.

    LOOP AT t_value INTO DATA(w_value).
      IF     w_value-title = 'TEMPO_RETORNO_MINUTOS'.
        l_time_ret     = w_value-from.
      ELSEIF w_value-title = 'TENTATIVAS'.
        l_tentativas   = w_value-from.
      ENDIF.
    ENDLOOP.

*-------------------------------
*-- montar range
*-------------------------------
    IF i_ch_referencia IS NOT INITIAL.
      l_ch_ref-sign     = 'I'.
      l_ch_ref-option   = 'EQ'.
      l_ch_ref-low      = i_ch_referencia.
      APPEND l_ch_ref  TO r_ch_ref.
    ENDIF.

    IF i_ch_faturamento IS NOT INITIAL.
      l_ch_fat-sign     = 'I'.
      l_ch_fat-option   = 'EQ'.
      l_ch_fat-low      = i_ch_faturamento.
      APPEND l_ch_fat  TO r_ch_fat.
    ENDIF.

*-------------------------------
*-- selecao mensagens a enviar OPUS
*-------------------------------
    IF i_ch_faturamento IS NOT INITIAL.
      SELECT *
        FROM zlest0241
        INTO TABLE @DATA(t_zlest0241)
       WHERE ch_faturamento IN @r_ch_fat
         AND cancelado       = @abap_false.
    ELSE.
      SELECT *
        FROM zlest0241
        INTO TABLE t_zlest0241
       WHERE ch_referencia  IN r_ch_ref
         AND cancelado       = abap_false.
    ENDIF.

    CHECK sy-subrc = 0.

    IF i_consulta_opus = abap_true.
      SELECT *
        FROM zlest0242
        INTO TABLE me->at_zlest0242
         FOR ALL ENTRIES IN t_zlest0241
       WHERE ch_faturamento = t_zlest0241-ch_faturamento
         AND cancelado      = abap_false.

*     CHECK sy-subrc = 0.

    ELSE.
      SELECT *
        FROM zlest0242
        INTO TABLE me->at_zlest0242
         FOR ALL ENTRIES IN t_zlest0241
       WHERE ch_faturamento         = t_zlest0241-ch_faturamento
         AND selecionado            = abap_false
         AND enviado_sistema_origem = abap_false
         AND cancelado              = abap_false.

*     CHECK sy-subrc = 0.

*-------------------------------
*---- verifica nro de tentativas
*-------------------------------
      LOOP AT me->at_zlest0242 INTO w_zlest0242.
        w_zlest0242-tentar_envio = w_zlest0242-tentar_envio + 1.
        IF w_zlest0242-tentar_envio > l_tentativas.
          DELETE me->at_zlest0242 INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

*-------------------------------
*---- verifica tempo de reenvio
*-------------------------------
      LOOP AT me->at_zlest0242 INTO w_zlest0242.
        l_tabix     = sy-tabix.

        IF w_zlest0242-data_envio IS INITIAL OR
           w_zlest0242-hora_envio IS INITIAL.
          CONTINUE.
        ENDIF.

        CALL FUNCTION 'SALP_SM_CALC_TIME_DIFFERENCE'
          EXPORTING
            date_1  = w_zlest0242-data_envio
            time_1  = w_zlest0242-hora_envio
            date_2  = sy-datum
            time_2  = sy-uzeit
          IMPORTING
            seconds = l_vzeit.

        l_diftime   = l_vzeit / 60.

        IF l_diftime < l_time_ret.
          DELETE me->at_zlest0242 INDEX l_tabix.
        ENDIF.
      ENDLOOP.

*     CHECK me->at_zlest0242[] IS NOT INITIAL.
    ENDIF.

*-------------------------------
*-- status selecionado
*-------------------------------
    LOOP AT me->at_zlest0242 INTO w_zlest0242.
      w_zlest0242-selecionado   = abap_true.
      MODIFY zlest0242       FROM w_zlest0242.
    ENDLOOP.

    COMMIT WORK.

    SORT me->at_zlest0242 BY ch_faturamento item_faturamento seq DESCENDING.

*-------------------------------
*-- enviar mensagens q nao foram integradas
*-------------------------------
    LOOP AT t_zlest0241 INTO DATA(w_zlest0241).

      FREE: t_romaneio, w_romaneio, t_status, w_status.

      t_zlest0242[]                     = me->at_zlest0242[].

      me->at_id_referencia              = w_zlest0241-ch_faturamento.
      me->at_ch_faturamento             = w_zlest0241-ch_faturamento.
      me->at_item_faturamento           = w_zlest0241-item_faturamento.
      me->at_retorno_opus-protocolo     = w_zlest0241-ch_faturamento.

      w_romaneio-ch_referencia_romaneio = w_zlest0241-ch_referencia.

*-#157580-06.11.2024-JT-inicio
*     w_romaneio-code                   = COND #( WHEN w_zlest0241-finalizado = abap_true THEN 'SUCESSO'
*                                                                                         ELSE 'PENDENTE' ).

*-#166749-20.02.2025-JT-inicio
      IF i_permite_cancelar = abap_true.
        TRY.
            me->get_ck_permite_cancelar( i_ch_faturamento   = w_zlest0241-ch_faturamento
                                         i_item_faturamento = w_zlest0241-item_faturamento ).

            w_romaneio-code             = 'SUCESSO'.
            w_romaneio-permite_cancelar = abap_true.
            w_romaneio-descricao_code   = 'O Romaneio pode ser Cacelado!'.

          CATCH zcx_integracao.
          CATCH zcx_error INTO DATA(ex_error_c).
            w_romaneio-code             = 'ERRO'.
            w_romaneio-permite_cancelar = abap_false.
            MESSAGE ID ex_error_c->msgid TYPE 'S' NUMBER ex_error_c->msgno WITH ex_error_c->msgv1 ex_error_c->msgv2 ex_error_c->msgv3 ex_error_c->msgv4
               INTO w_romaneio-descricao_code.
        ENDTRY.
      ELSE.
*-#166749-20.02.2025-JT-fim
        TRY.
            me->get_ck_permite_cancelar( i_ch_faturamento   = w_zlest0241-ch_faturamento
                                         i_item_faturamento = w_zlest0241-item_faturamento ).
            w_romaneio-permite_cancelar = abap_true.
          CATCH zcx_integracao.
          CATCH zcx_error INTO ex_error_c.
            w_romaneio-permite_cancelar = abap_false.
        ENDTRY.

        IF w_zlest0241-selecionado     = abap_false AND
           w_zlest0241-job_terminado   = abap_false AND
           w_zlest0241-finalizado      = abap_false.
          w_romaneio-code   = 'PENDENTE'.
        ELSE.
          IF w_zlest0241-selecionado   = abap_true.
            w_romaneio-code = 'ANDAMENTO'.
          ENDIF.
          IF w_zlest0241-job_terminado = abap_true.
            w_romaneio-code = 'PAUSADO'.
          ENDIF.
          IF w_zlest0241-finalizado    = abap_true.
            w_romaneio-code = 'SUCESSO'.
          ENDIF.
        ENDIF.

*-#166749-20.02.2025-JT-inicio
        IF w_romaneio-code = 'ANDAMENTO' AND ( me->get_ck_job_cancelado( i_ch_faturamento   = w_zlest0241-ch_faturamento
                                                                         i_item_faturamento = w_zlest0241-item_faturamento ) = abap_true ).
          w_romaneio-code            = 'PAUSADO'.
          w_romaneio-descricao_code  = 'Processamento Interrompido! Aguarde alguns minutos, pois ele será Retomado Automaticamente!'.
*         w_romaneio-descricao_code  = 'Processamento Interrompido! Favor verificar andamento do Faturamenro na ZLES0136!'.
        ELSE.
*-#166749-20.02.2025-JT-fim
          w_romaneio-descricao_code  = at_t_status[ code = w_romaneio-code ]-descricao.  "*-#157580-06.11.2024-JT-inicio
        ENDIF.
      ENDIF.
*-#157580-06.11.2024-JT-fim

      IF w_zlest0241-finalizado = abap_true.
        SELECT SINGLE nro_nf_frete
          FROM zsdt0001
          INTO @DATA(_nro_nf_frete)
         WHERE ch_referencia = @w_zlest0241-ch_referencia.

        IF sy-subrc = 0.
          SELECT link_carga_pedagio
            FROM zcte_ciot
            INTO @DATA(_link_carga_pedagio)
           UP TO 1 ROWS
           WHERE docnum = @_nro_nf_frete.
          ENDSELECT.

          IF sy-subrc = 0.
            w_romaneio-link_carga_pedagio = _link_carga_pedagio.
          ENDIF.
        ENDIF.

        w_romaneio-documentos             = me->get_obter_doctos_romaneio( i_ch_faturamento   = w_zlest0241-ch_faturamento
                                                                           i_item_faturamento = w_zlest0241-item_faturamento ).

        SORT t_zlest0242 BY ch_faturamento item_faturamento seq DESCENDING.
        DELETE ADJACENT DUPLICATES FROM t_zlest0242 COMPARING ch_faturamento item_faturamento.

*       LOOP AT me->at_zlest0242 INTO w_zlest0242 WHERE ch_faturamento   = w_zlest0241-ch_faturamento
*                                                   AND item_faturamento = w_zlest0241-item_faturamento
*                                                   AND status_reg       = '08'
*                                                   AND status_msg       = 'E'.
*         APPEND w_zlest0242       TO t_zlest0242.
*       ENDLOOP.
      ELSE.
        IF w_zlest0241-job_terminado = abap_false.  "*-#157580-06.11.2024-JT-inicio
          IF i_permite_cancelar = abap_true.                   "*-#166749-20.02.2025-JT-inicio
            FREE t_zlest0242.                                  "*-#166749-20.02.2025-JT-inicio
          ELSE.                                                "*-#166749-20.02.2025-JT-inicio
            DELETE t_zlest0242 WHERE status_reg <> 'DANF' AND  "*-#166749-20.02.2025-JT-inicio
                                     status_reg <> 'DACT' AND  "*-#166749-20.02.2025-JT-inicio
                                     status_reg <> 'MDFE'.     "*-#166749-20.02.2025-JT-inicio
          ENDIF.                                               "*-#166749-20.02.2025-JT-inicio
        ELSE.                                                  "*-#166749-20.02.2025-JT-inicio
          DELETE t_zlest0242 WHERE status_msg <> 'E'.          "*-#166749-20.02.2025-JT-inicio
        ENDIF.                                                 "*-#166749-20.02.2025-JT-inicio

        SORT t_zlest0242 BY ch_faturamento item_faturamento mensagem seq DESCENDING.
        DELETE ADJACENT DUPLICATES FROM t_zlest0242 COMPARING ch_faturamento item_faturamento mensagem.

        SORT t_zlest0242 BY ch_faturamento item_faturamento seq DESCENDING.
      ENDIF.

      LOOP AT t_zlest0242 INTO w_zlest0242 WHERE ch_faturamento   = w_zlest0241-ch_faturamento
                                             AND item_faturamento = w_zlest0241-item_faturamento.
        CLEAR w_status.

*-#166749-20.02.2025-JT-inicio
        IF w_zlest0242-mensagem(4) = '0000' OR
           w_zlest0242-mensagem    = 'OK'   OR
           w_zlest0242-mensagem    = 'Nao foi possivel solicitar Viagem!'.
          CONTINUE.
        ENDIF.

        IF w_zlest0242-status_reg = 'DANF' OR
           w_zlest0242-status_reg = 'DACT' OR
           w_zlest0242-status_reg = 'MDFE'.
          me->get_ck_retorno_sefaz( EXPORTING i_ch_faturamento   = w_zlest0242-ch_faturamento
                                              i_item_faturamento = w_zlest0242-item_faturamento
                                              i_status_reg       = w_zlest0242-status_reg
                                     CHANGING e_code             = w_romaneio-code
                                              e_descricao_code   = w_romaneio-descricao_code
                                              e_mensagem         = w_zlest0242-mensagem ).
        ENDIF.
*-#166749-20.02.2025-JT-fim

        CALL FUNCTION 'DD_DOMVALUES_GET'
          EXPORTING
            domname        = 'ZSTATUS_REG'
            text           = abap_true
            langu          = sy-langu
          TABLES
            dd07v_tab      = t_idd07v
          EXCEPTIONS
            wrong_textflag = 1
            OTHERS         = 2.

        CLEAR w_idd07v.
        READ TABLE t_idd07v      INTO w_idd07v WITH KEY domvalue_l = w_zlest0242-status_reg.

        w_status-etapa              = w_idd07v-ddtext.
        w_status-mensagem           = w_zlest0242-mensagem.
        APPEND w_status            TO t_status.

        EXIT. "Exibir somente a ultima mensagem *-#166749-20.02.2025-JT-inicio
      ENDLOOP.

      w_romaneio-status             = t_status[].
      APPEND w_romaneio            TO t_romaneio.

      APPEND LINES OF t_romaneio[] TO me->at_retorno_opus-romaneio[].
    ENDLOOP.

*-----------------------------------
*-- monta JSON
*-----------------------------------
    me->at_json = /ui2/cl_json=>serialize( data        = me->at_retorno_opus
                                           pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

*-----------------------------------
*-- retorno
*-----------------------------------
    CASE i_consulta_opus.

      WHEN abap_false.
        l_metodo = 'RETORNO_OPUS'.

*------ Executa API
*       TRY .                                                     "*-#166749-20.02.2025-JT-comentado
*           me->set_exec_opus( EXPORTING i_metodo = l_metodo ).   "*-#166749-20.02.2025-JT-comentado
**          CATCH zcx_integracao INTO DATA(ex_integra).           "*-#166749-20.02.2025-JT-comentado
*         CATCH zcx_error INTO DATA(ex_error).                    "*-#166749-20.02.2025-JT-comentado
*       ENDTRY.                                                   "*-#166749-20.02.2025-JT-comentado

      WHEN abap_true.
        LOOP AT me->at_zlest0242           INTO w_zlest0242 WHERE ch_faturamento   = me->at_ch_faturamento
                                                              AND item_faturamento = me->at_item_faturamento.
          w_zlest0242-selecionado             = abap_true.
          w_zlest0242-enviado_sistema_origem  = abap_true.
          w_zlest0242-mensagem_sistema_origem = abap_off.
          w_zlest0242-tentar_envio            = w_zlest0242-tentar_envio + 1.
          w_zlest0242-data_envio              = sy-datum.
          w_zlest0242-hora_envio              = sy-uzeit.
          w_zlest0242-user_envio              = sy-uname.
          MODIFY zlest0242                 FROM w_zlest0242.
        ENDLOOP.

        COMMIT WORK.

*------ retorno api
        e_sucesso    = abap_true.
        e_nm_code    = w_romaneio-code.
        e_msg_erro   = abap_false.
        r_retorno    = me->at_json.

    ENDCASE.

  ENDMETHOD.


  METHOD set_selecao_dados.

    DATA: l_valor_frete TYPE kbetr_kond.

    FREE: me->at_valor_frete.

    LOOP AT at_data_request-romaneio INTO DATA(w_romaneio).

*--------------------------
*---- tipo cockpit
*--------------------------
      me->at_cockpit = me->get_ck_tipo_selecao( w_romaneio-ch_referencia_romaneio ).

*--------------------------
*---- selecao romaneios
*--------------------------
      PERFORM f_selecao_fat_autom IN PROGRAM zlesr0102    USING w_romaneio-ch_referencia_romaneio
                                                                me->at_cockpit.
      PERFORM f_saida             IN PROGRAM zlesr0102.

*--------------------------
*---- recuperar dados selecionados
*--------------------------
      PERFORM f_recuperar_dados   IN PROGRAM zlesr0102 CHANGING me->at_t_saida
                                                                me->at_zsdt0001
                                                                me->at_saida.

      CHECK me->get_ck_desmembramento( at_zsdt0001-ch_referencia )-calcula_frete = abap_true.

*--------------------------
*---- recuperar agente frete
*--------------------------
*     IF me->at_saida-lifnr IS INITIAL.
*       SELECT SINGLE agente_frete
*         INTO @DATA(_agente_frete)
*         FROM zsdt0001od
*        WHERE id_ordem = @me->at_zsdt0001-id_ordem.
*
*       IF sy-subrc = 0 AND _agente_frete IS NOT INITIAL.
*         me->at_saida-lifnr                = _agente_frete.
*         me->at_zsdt0001-agente_frete      = _agente_frete.
*
*         UPDATE zsdt0001 SET agente_frete  = me->at_zsdt0001-agente_frete
*                       WHERE ch_referencia = me->at_zsdt0001-ch_referencia.
*         COMMIT WORK.
*       ENDIF.
*     ENDIF.

*--------------------------
*---- valor do frete
*--------------------------
      TRY .
          me->get_valor_frete( EXPORTING i_zsdt0001    = me->at_zsdt0001
                               IMPORTING e_valor_frete = l_valor_frete
                                         e_moeda_frete = me->at_moeda_frete
                                         e_krech       = DATA(e_krech) ). "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP

        CATCH zcx_error INTO DATA(ex_error).
          MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 INTO DATA(l_mesg).
          set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
      ENDTRY.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      CASE e_krech.
        WHEN 'B'. "Montante Fixo
          me->at_valor_frete = l_valor_frete.
        WHEN OTHERS.
          me->at_valor_frete = me->at_valor_frete + l_valor_frete.
      ENDCASE.
      "me->at_valor_frete = me->at_valor_frete + l_valor_frete.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

      me->at_moeda_frete = me->at_konwa.

    ENDLOOP.

  ENDMETHOD.


  METHOD SET_SELECAO_DADOS_OLD.

*    DATA: lra_bsart_cockpit_01 TYPE RANGE OF ekko-bsart.
*
*    FREE: at_t_lfa1, at_t_kna1, at_t_mara, at_t_makt, at_t_vbpa.
*
**---------------------------------------------------
**-- TVARV
**---------------------------------------------------
*    SELECT *
*      FROM tvarvc
*      INTO TABLE @DATA(lit_tvarvc_bsart_01)
*     WHERE name = 'ZLES0136_BSART_COCKPIT_01'.
*
*    IF lit_tvarvc_bsart_01[] IS INITIAL.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'ZUB' ) TO lra_bsart_cockpit_01.
*    ELSE.
*      LOOP AT lit_tvarvc_bsart_01 INTO DATA(lwa_tvarvc_bsart_01).
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_tvarvc_bsart_01-low ) TO lra_bsart_cockpit_01.
*      ENDLOOP.
*    ENDIF.
*
**---------------------------------------------------
**-- SET
**---------------------------------------------------
*    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*      EXPORTING
*        class         = '0000'
*        setnr         = 'MAGGI_ARMAZENAGEM_VA01'
*      TABLES
*        set_values    = at_t_auart
*      EXCEPTIONS
*        set_not_found = 1
*        OTHERS        = 2.
*
*    SORT at_t_auart BY from.
*
**---------------------------------------------------
**-- Romaneios
**---------------------------------------------------
*    SELECT SINGLE *
*      FROM zsdt0001
*      INTO at_zsdt0001
*     WHERE ch_referencia = at_data_request-ch_referencia_romaneio.
*
*    CHECK sy-subrc = 0.
*
*    SELECT *
*      FROM mara
*      INTO TABLE at_t_mara
*     WHERE matnr EQ at_zsdt0001-matnr.
*
*    SELECT *
*      FROM zsdt0062
*      INTO TABLE at_t_zsdt0062
*     WHERE vbeln  = at_zsdt0001-vbeln.
*
*    DELETE at_t_zsdt0062 WHERE status = 'E'.
*
*    DATA(it_zsdt0062_aux) = at_t_zsdt0062[].
*
*    SORT at_t_zsdt0062 BY vbeln ebeln ebelp.
*    DELETE ADJACENT DUPLICATES FROM at_t_zsdt0062 COMPARING vbeln ebeln ebelp.
*
*    LOOP AT at_t_zsdt0062 ASSIGNING FIELD-SYMBOL(<fs_zsdt0062>).
*      CLEAR: <fs_zsdt0062>-qtd_vinc.
*      LOOP AT it_zsdt0062_aux INTO DATA(lwa_zsdt0062_aux) WHERE vbeln = <fs_zsdt0062>-vbeln
*                                                            AND ebeln = <fs_zsdt0062>-ebeln
*                                                            AND ebelp = <fs_zsdt0062>-ebelp.
*        ADD lwa_zsdt0062_aux-qtd_vinc TO <fs_zsdt0062>-qtd_vinc.
*      ENDLOOP.
*    ENDLOOP.
**-----------------------------------------------------------------------------*
**-  Seleção de Pontos de Coleta
**-----------------------------------------------------------------------------*
*    "Ordem Venda
*    SELECT *
*      FROM vbpa
*      INTO TABLE at_t_vbpa_co
*     WHERE vbeln = at_zsdt0001-vbeln
*       AND parvw = 'PC'.
*
*    IF at_t_vbpa_co[] IS NOT INITIAL.
*      SELECT *
*        FROM lfa1
*   APPENDING TABLE at_t_lfa1
*         FOR ALL ENTRIES IN at_t_vbpa_co
*       WHERE lifnr  = at_t_vbpa_co-lifnr.
*    ENDIF.
*
*    "Pedido
*    SELECT *
*      FROM ekpa
*      INTO TABLE at_t_ekpa_pr
*     WHERE ebeln = at_zsdt0001-vbeln
*       AND parvw = 'PR'.
*
*    IF at_t_ekpa_pr[] IS NOT INITIAL.
*      SELECT *
*        FROM lfa1
*   APPENDING TABLE at_t_lfa1
*         FOR ALL ENTRIES IN at_t_ekpa_pr
*       WHERE lifnr  = at_t_ekpa_pr-lifn2.
*    ENDIF.
*
**-----------------------------------------------------------------------------*
**-  Seleção de Itinerário
**-----------------------------------------------------------------------------*
*    "Ordem Venda
*    SELECT *
*      FROM vbap
*      INTO TABLE at_t_vbap
*     WHERE vbeln = at_zsdt0001-vbeln.
*
*    "Dados gerais de material
*    IF at_t_vbap[] IS NOT INITIAL.
*      SELECT *
*        FROM mara
*   APPENDING TABLE at_t_mara
*         FOR ALL ENTRIES IN at_t_vbap
*       WHERE matnr = at_t_vbap-matnr.
*
*      SELECT *
*        FROM makt
*        INTO TABLE at_t_makt
*         FOR ALL ENTRIES IN at_t_vbap
*       WHERE matnr = at_t_vbap-matnr
*         AND spras = sy-langu.
*    ENDIF.
*
*    "Pedido
*    SELECT *
*      FROM ekpv
*      INTO TABLE at_t_ekpv
*     WHERE ebeln = at_zsdt0001-vbeln.
*
*    SELECT *
*      FROM makt
* APPENDING TABLE at_t_makt
*     WHERE matnr = at_zsdt0001-matnr
*       AND spras = sy-langu.
*
*    SELECT *
*      FROM t001w
*      INTO TABLE at_t_t001w
*     WHERE werks = at_zsdt0001-branch.
*
*    SELECT *
*      FROM vbak
*      INTO TABLE at_t_vbak
*     WHERE vbeln  = at_zsdt0001-vbeln.
*
*    LOOP AT at_t_vbak INTO DATA(wa_vbak).
*      DATA(tabix) = sy-tabix .
*      wa_vbak-tp_movimento = at_zsdt0001-tp_movimento.
*      MODIFY at_t_vbak FROM wa_vbak INDEX tabix TRANSPORTING tp_movimento.
*    ENDLOOP.
*
*    IF at_t_vbak[] IS NOT INITIAL.
*      SELECT *
*        FROM zsdt0011
*        INTO TABLE at_t_zsdt0011_o
*         FOR ALL ENTRIES IN at_t_vbak
*       WHERE tp_movimento = at_t_vbak-tp_movimento
*         AND auart        = at_t_vbak-auart.
*
*      SELECT *
*        FROM kna1
*   APPENDING TABLE at_t_kna1
*         FOR ALL ENTRIES IN at_t_vbak
*       WHERE kunnr  = at_t_vbak-kunnr.
*
*      SELECT *
*        FROM tvakt
*        INTO TABLE at_t_tvakt
*         FOR ALL ENTRIES IN at_t_vbak
*       WHERE auart = at_t_vbak-auart
*         AND spras = sy-langu.
*
*      SELECT *
*        FROM vbkd
*        INTO TABLE at_t_vbkd
*       WHERE vbeln  = at_zsdt0001-vbeln
*         AND posnr  = '000000'.
*    ENDIF.
*
*    SELECT *
*      FROM ekko
*      INTO TABLE at_t_ekko
*     WHERE ebeln  = at_zsdt0001-vbeln
*       AND bsart IN lra_bsart_cockpit_01.
*
*    LOOP AT at_t_ekko INTO DATA(wa_ekko).
*      tabix = sy-tabix.
*      wa_ekko-tp_movimento = at_zsdt0001-tp_movimento.
*      MODIFY at_t_ekko FROM wa_ekko INDEX tabix TRANSPORTING tp_movimento.
*    ENDLOOP.
*
*    IF at_t_ekko[] IS NOT INITIAL.
*      SELECT  *
*        FROM zsdt0011
*        INTO TABLE at_t_zsdt0011_p
*         FOR ALL ENTRIES IN at_t_ekko
*       WHERE tp_movimento =  at_t_ekko-tp_movimento
*         AND bsart        =  at_t_ekko-bsart.
*
*      SELECT *
*        FROM ekpo
*        INTO TABLE at_t_ekpo
*         FOR ALL ENTRIES IN at_t_ekko
*       WHERE ebeln = at_t_ekko-ebeln.
*
*      LOOP AT at_t_ekpo INTO DATA(wa_ekpo).
*        tabix = sy-tabix .
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = wa_ekpo-werks
*          IMPORTING
*            output = wa_ekpo-lifnr.
*        MODIFY at_t_ekpo FROM wa_ekpo INDEX tabix TRANSPORTING lifnr.
*      ENDLOOP.
*
*      SELECT lifnr name1 dlgrp lzone
*        FROM lfa1
*   APPENDING TABLE at_t_lfa1
*         FOR ALL ENTRIES IN at_t_ekpo
*       WHERE lifnr  = at_t_ekpo-lifnr.
*
*      SELECT bsart batxt
*        FROM t161t
*        INTO TABLE at_t_t161t
*         FOR ALL ENTRIES IN at_t_ekko
*       WHERE bsart = at_t_ekko-bsart
*         AND spras = sy-langu.
*    ENDIF.
*
*    SELECT *
*      FROM vbpa APPENDING TABLE at_t_vbpa
*     WHERE vbeln = at_zsdt0001-vbeln
*       AND parvw  IN ('LR','SP','Z1').
*
*    SELECT *
*      FROM vbpa APPENDING TABLE at_t_vbpa
*     WHERE vbeln = at_zsdt0001-doc_aviso
*       AND parvw  IN ('LR','SP').
*
*    SELECT *
*      FROM vbpa APPENDING TABLE at_t_vbpa
*     WHERE vbeln = at_zsdt0001-doc_rem
*       AND parvw  IN ('LR','SP').
*
*    IF at_t_vbpa[] IS NOT INITIAL.
*      SELECT *
*        FROM kna1 APPENDING TABLE at_t_kna1
*         FOR ALL ENTRIES IN at_t_vbpa
*       WHERE kunnr  = at_t_vbpa-kunnr.
*    ENDIF.
*
*    LOOP AT at_t_vbpa ASSIGNING FIELD-SYMBOL(<fs_vbpa>) WHERE parvw EQ 'SP'
*                                                          AND lifnr IS NOT INITIAL.
*      SELECT SINGLE *
*        FROM lfa1
*        INTO @DATA(_wl_lfa1_sp)
*       WHERE lifnr EQ @<fs_vbpa>-lifnr.
*      IF sy-subrc EQ 0.
*        <fs_vbpa>-dlgrp = _wl_lfa1_sp-dlgrp.
*      ENDIF.
*    ENDLOOP.
*
*    "Itens Romaneio
*    SELECT *
*      FROM zsdt0001_item
*      INTO CORRESPONDING FIELDS OF TABLE at_t_zsdt0001_item
*     WHERE ch_referencia EQ at_zsdt0001-ch_referencia.
*
*    SORT: at_t_mara     BY matnr,
*          at_t_lfa1     BY lifnr,
*          at_t_vbpa_co  BY vbeln,  "Ponto de coleta  ORDEM
*          at_t_ekpa_pr  BY ebeln.  "Ponto de coleta  Pedido

  ENDMETHOD.


  METHOD SET_SEND_MSG.

    TYPES BEGIN OF ty_retorno.
    TYPES: access_token TYPE string.
    TYPES: expires_in TYPE string.
    TYPES: token_type TYPE string.
    TYPES END OF ty_retorno.

    DATA: lc_integrar    TYPE REF TO zcl_integracao,
          lc_retorno     TYPE ty_retorno,
          l_access_token TYPE string,
          l_token_type   TYPE string,
          l_expires_in   TYPE string,
          l_force        TYPE char01.

    FREE: e_mensagem.

    CREATE OBJECT lc_integrar.

    lc_integrar->zif_integracao~at_form_fields = me->zif_integracao_inject~at_form_fields.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg(      IMPORTING e_id_integracao  = e_id_integracao
      )->set_outbound_msg( IMPORTING e_mensagem       = e_mensagem
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro(     IMPORTING e_integracao     = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.

  ENDMETHOD.


  METHOD set_solicitar_credito.

    DATA: it_cte_ciot TYPE TABLE OF zcte_ciot,
          wa_cte_ciot TYPE zcte_ciot,
          p_protocolo TYPE zprotocolo.

    DO 10 TIMES.

      CALL FUNCTION 'Z_SD_CREDITA_CIOT'
        EXPORTING
          p_cte_avulso        = i_docnum
          p_faturamento_autom = abap_true
          p_ch_referencia     = me->at_w_zlest0241-ch_referencia
        IMPORTING
          p_protocolo         = p_protocolo
        EXCEPTIONS
          cte_nao_autorizado  = 1
          sem_dados_ciot      = 2
          erro_status_cred    = 3
          erro_web_service    = 4
          OTHERS              = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_mesg).
        me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'APRV' ).
      ENDIF.

      FREE: it_cte_ciot[].

      CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
        EXPORTING
          p_cte_avulso       = i_docnum
        TABLES
          it_cte_ciot        = it_cte_ciot
        EXCEPTIONS
          inf_docnum         = 1
          inf_propveiculo    = 2
          nao_docnum         = 3
          nao_rtrc           = 4
          nao_conta_corrente = 5
          nao_ciot           = 6
          n_placa_cad        = 7
          restricoes_veiculo = 8
          OTHERS             = 9.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
        me->set_gravar_mensagem( i_ch_referencia = me->at_w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'APRV' ).
      ENDIF.

*------------------------------------
*---- verifica ciot para a CTE
*------------------------------------
      SELECT *
        FROM  zcte_ciot
        INTO @DATA(_zcte_ciot)
          UP TO 1 ROWS
       WHERE docnum = @i_docnum.
      ENDSELECT.

      IF sy-subrc = 0 AND ( _zcte_ciot-st_ciot = '5' OR _zcte_ciot-st_ciot = '9' ).
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.

    ENDDO.

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


  METHOD SET_TROCA_AGENTE.

    DATA: t_tvarvc    TYPE TABLE OF tvarvc,
          w_tvarvc    TYPE tvarvc,
          v_lifnr_ori TYPE lifnr.

    DATA: r_bukrs TYPE RANGE OF t001-bukrs.

    SELECT SINGLE bukrs
      INTO @DATA(v_bukrs)
      FROM j_1bbranch
      WHERE branch = @i_branch.

    FREE: r_bukrs.

*---------------------------------
* ler TVARVset
*---------------------------------
    SELECT *
      FROM tvarvc
      INTO TABLE t_tvarvc
     WHERE name = 'ZLES0136_BUKRS_CHANGE_AGENTE'.

    LOOP AT t_tvarvc INTO w_tvarvc.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = w_tvarvc-low ) TO r_bukrs.
    ENDLOOP.

    CHECK v_bukrs IN r_bukrs[] AND r_bukrs[] IS NOT INITIAL.

    TRY.
        zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_veiculo(
           EXPORTING i_placa        = i_placa_cav
           IMPORTING e_tipo         = DATA(e_tipo)
                     e_proprietario = DATA(e_proprietario) ).
      CATCH zcx_faturamento .
      CATCH zcx_error .
    ENDTRY.

    IF e_tipo = 'P'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = i_branch
        IMPORTING
          output = v_lifnr_ori.
      "
      SELECT SINGLE regio
        FROM lfa1
        INTO @DATA(_regio)
        WHERE lifnr = @v_lifnr_ori.
      "
      TRY.
          zcl_faturamento=>zif_faturamento~get_instance( )->get_agente_frete(
            EXPORTING
              i_tipo_agente           = '2'               "CS2022000236 - 25.02.2022 - JT - inicio
              i_bukrs                 = CONV #( v_bukrs ) "CS2022000236 - 25.02.2022 - JT - inicio
              i_placa                 = CONV #( i_placa_cav )
              i_uf_origem_mercadoria  = CONV #( _regio )
             IMPORTING
               e_agente_frete         = DATA(_agente) ).

          c_lifnr = _agente.
        CATCH zcx_faturamento.
        CATCH zcx_error.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  method set_validar_mdfe_gerada.

    free: r_docnum_mdfe.

    do 5 times.
      select *
        from zsdt0105
        into table @data(t_0105)
       where docnum = @i_docnum.

      if sy-subrc <> 0.
        wait up to 10 seconds.
        continue.
      endif.

      sort t_0105 by docnum_ref descending.

      select *
        from zsdt0102
        into table @data(t_0102)
         for all entries in @t_0105
       where docnum = @t_0105-docnum_ref.

      if sy-subrc <> 0.
        wait up to 10 seconds.
        continue.
      endif.

      delete t_0102 where cancel    = abap_true
                       or encerrado = abap_true
                       or estornado = abap_true.

      sort t_0102 by docnum descending.

      loop at t_0102 into data(w_0102).
        if w_0102-autorizado = abap_true or w_0102-status = '3'. "ALRS
          r_docnum_mdfe = w_0102-docnum.
        endif.
      endloop.

      if r_docnum_mdfe is initial.
        wait up to 10 seconds.
      else.
        return.
      endif.
    enddo.

  endmethod.


  METHOD ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    DATA: lva_reason TYPE string,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          output = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code        = lva_code
        IMPORTING
          e_desc_status = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = CONV #( lva_code )
          reason = CONV #( lva_reason )
       ).

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

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

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  ENDMETHOD.


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA: l_lifnr            TYPE lfa1-lifnr,
          l_taxjurcode       TYPE j_1btxjcd,
          l_tp_frete         TYPE zde_tp_frete,
          t_romaneios        TYPE zsdt0001_t,
          t_desmembra        TYPE zsdt0001_t,
          t_values_adm_ped   TYPE crm_isu_wael_ddicdomavalues,
          t_values_card_ped  TYPE crm_isu_wael_ddicdomavalues,
          t_values_adm_frete TYPE crm_isu_wael_ddicdomavalues.

    FREE: r_msg_erro, at_data_response, t_romaneios.

    e_status_code   = '200'.

*----------------------------
*- obter valores dominios
*----------------------------
    t_values_adm_ped   = cl_crm_eewa_ddicinfo=>getdomainvalues( par_name = 'ZDM_ADM_PEDAGIO' ).
    t_values_card_ped  = cl_crm_eewa_ddicinfo=>getdomainvalues( par_name = 'ZDM_TP_CARD_PED' ).
    t_values_adm_frete = cl_crm_eewa_ddicinfo=>getdomainvalues( par_name = 'ZDM_ADM_PEDAGIO' ).

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_post.
      set_mensagem( '01' ).
    ENDIF.

    IF i_data_inbound IS INITIAL.
      set_mensagem( '02' ).
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = at_data_request ).

    LOOP AT at_data_request-romaneio INTO DATA(w_romaneio).
      CONDENSE w_romaneio-ch_referencia_romaneio.
      MODIFY at_data_request-romaneio FROM w_romaneio INDEX sy-tabix.
    ENDLOOP.

    IF sy-subrc <> 0 AND at_data_request-protocolo IS INITIAL.
      set_mensagem( '02' ).
    ENDIF.

    CONDENSE: at_data_request-informacoesfrete-administradorfrete-codigo,
              at_data_request-informacoesfrete-adiantamento-valor,
              at_data_request-informacoespedagio-creditapedagio-credita,
              at_data_request-informacoespedagio-idrotaadministradora,
              at_data_request-informacoespedagio-qtdeeixos,
              at_data_request-informacoespedagio-valor,
              at_data_request-informacoespedagio-administradorpedagio-codigo,
              at_data_request-informacoespedagio-cartaopedagio-codigo,
              at_data_request-informacoespedagio-municipios-origem-uf,
              at_data_request-informacoespedagio-municipios-origem-codigoibge,
              at_data_request-informacoespedagio-municipios-destino-uf,
              at_data_request-informacoespedagio-municipios-destino-codigoibge.

*------------------------------------
*-- valida se faturamento esta em andamento
*------------------------------------
    IF at_data_request-continua_faturamento          = abap_off OR
       at_data_request-inicia_faturamento            = abap_on  OR
       at_data_request-preenchimento_info_automatica = abap_on.     "*-#143658-19.06.2024-JT
      LOOP AT at_data_request-romaneio INTO w_romaneio.
        SELECT SINGLE *
          FROM zlest0241
          INTO @DATA(zlest0241)
         WHERE ch_referencia = @w_romaneio-ch_referencia_romaneio
           AND cancelado     = @abap_false.

        IF sy-subrc = 0.
          set_mensagem( '19' ).
        ENDIF.
      ENDLOOP.
    ENDIF.

*------------------------------------
*-- romaneio existe
*------------------------------------
    LOOP AT at_data_request-romaneio INTO w_romaneio.
      SELECT SINGLE *
        FROM zsdt0001
        INTO me->at_zsdt0001
       WHERE ch_referencia = w_romaneio-ch_referencia_romaneio.

      IF sy-subrc <> 0.
        set_mensagem( '03' ).
      ENDIF.

*-#143658-19.06.2024-JT-inicio
*------------------------------------
*---- validar tipo Frete
*------------------------------------
      TRY.
          zcl_ordem_venda=>zif_ordem_venda~get_instance(
            )->set_ordem_venda( EXPORTING i_vbeln      = me->at_zsdt0001-vbeln
            )->get_tipo_frete(  IMPORTING e_tipo_frete = l_tp_frete
            ).
        CATCH zcx_ordem_venda INTO DATA(ex_ordem).
          "Se não Achou a Ordem de Venda procura o Pedido de Compra
          IF zcx_ordem_venda=>zcx_ordem_venda_nao_existe-msgid EQ ex_ordem->msgid AND
             zcx_ordem_venda=>zcx_ordem_venda_nao_existe-msgno EQ ex_ordem->msgno.

            TRY .
                zcl_pedido_compra=>get_instance(
                  )->set_pedido(     EXPORTING i_ebeln      = me->at_zsdt0001-vbeln
                  )->get_tipo_frete( IMPORTING e_tipo_frete = l_tp_frete
                  ).
              CATCH zcx_pedido_compra INTO DATA(ex_pedido_compra).
            ENDTRY.
          ENDIF.
      ENDTRY.

      IF me->at_faturamento_interno_sap EQ abap_false. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
        IF l_tp_frete <> 'CIF'. " AND l_tp_frete <> 'FOB'.
          set_mensagem( '04' ).
        ENDIF.
      ENDIF.

*     IF me->at_zsdt0001-tp_frete <> 'C' AND me->at_zsdt0001-tp_frete <> 'F'. "*-#143658-19.06.2024-JT
*       set_mensagem( '04' ).
*     ENDIF.
*-#143658-19.06.2024-JT-fim

*------------------------------------
*---- autoriado por filial / grupo material / Tipo Cockpit
*------------------------------------
      TRY.
          me->get_ck_fat_automatico( ).
        CATCH zcx_error INTO DATA(ex_error).
          DATA(l_mesg) = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
          set_mensagem( i_cod = '99' i_mesg = l_mesg ).
      ENDTRY.
    ENDLOOP.

*------------------------------------
*-- desmembramento
*------------------------------------
    LOOP AT at_data_request-romaneio INTO w_romaneio.

      CALL METHOD zcl_romaneio=>get_ck_faturar
        EXPORTING
          i_ch_referencia_sai = w_romaneio-ch_referencia_romaneio
        IMPORTING
          e_romaneios         = t_romaneios.

      APPEND LINES OF t_romaneios[]    TO t_desmembra[].
    ENDLOOP.

    LOOP AT t_desmembra INTO DATA(w_desmembra).
      READ TABLE at_data_request-romaneio INTO w_romaneio WITH KEY ch_referencia_romaneio = w_desmembra-ch_referencia.
      IF sy-subrc <> 0.
        set_mensagem( '23' ).
      ENDIF.
    ENDLOOP.

*   CHECK at_data_request-inicia_faturamento = abap_true.

*------------------------------------
*-- continuacao do faturamento, informar protocolo
*------------------------------------
    IF at_data_request-continua_faturamento = abap_true.
      SELECT SINGLE *
        FROM zlest0240
        INTO @DATA(_zlest0240)
       WHERE ch_faturamento = @at_data_request-protocolo
         AND cancelado      = @abap_false.

      IF sy-subrc <> 0.
        set_mensagem( '24' ).
      ENDIF.

*-#157580-06.11.2024-JT-inicio
      SELECT SINGLE *
        FROM zlest0241
        INTO @DATA(_zlest0241)
       WHERE ch_faturamento = @at_data_request-protocolo
         AND cancelado      = @abap_false.

      IF sy-subrc = 0 AND _zlest0241-selecionado = abap_true.
        set_mensagem( '19' ).
      ENDIF.
*-#157580-06.11.2024-JT-fim
    ENDIF.

*------------------------------------
*-- valida cancelamento faturamento
*------------------------------------
    IF at_data_request-cancela_faturamento = abap_true.
*-#166749-20.02.2025-JT-inicio
      TRY.
          me->get_ck_permite_cancelar( i_ch_faturamento = CONV #( at_data_request-protocolo ) ).
        CATCH zcx_integracao.
        CATCH zcx_error INTO DATA(ex_error_c).
          set_mensagem( '25' ).
      ENDTRY.
*-#166749-20.02.2025-JT-fim
    ENDIF.

*------------------------------------
*-- validar informacoes para faturamento
*------------------------------------
    IF at_data_request-informacoesfrete-agentefrete-codigo IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = at_data_request-informacoesfrete-agentefrete-codigo
        IMPORTING
          output = l_lifnr.

      SELECT SINGLE lifnr
        FROM lfa1
        INTO @DATA(_lfa1)
       WHERE lifnr = @l_lifnr.

      IF sy-subrc <> 0.
        set_mensagem( '05' ).
      ENDIF.
    ENDIF.

    IF at_data_request-informacoesfrete-administradorfrete-codigo IS NOT INITIAL.
      READ TABLE t_values_adm_frete INTO DATA(w_values_adm_frete) WITH KEY domvalue_l = at_data_request-informacoesfrete-administradorfrete-codigo.

      IF sy-subrc <> 0.
        set_mensagem( '06' ).
      ENDIF.
    ENDIF.

    IF at_data_request-informacoespedagio-idrotaadministradora IS NOT INITIAL.
      SELECT SINGLE id_rota
        FROM zlest0084
        INTO @DATA(_zlest0084)
       WHERE id_rota = @at_data_request-informacoespedagio-idrotaadministradora.

      IF sy-subrc <> 0.
        set_mensagem( '07' ).
      ENDIF.
    ENDIF.

    IF at_data_request-informacoespedagio-administradorpedagio-codigo IS NOT INITIAL.
      READ TABLE t_values_adm_ped INTO DATA(w_values_adm_ped) WITH KEY domvalue_l = at_data_request-informacoespedagio-administradorpedagio-codigo.

      IF sy-subrc <> 0.
        set_mensagem( '08' ).
      ENDIF.
    ENDIF.

    IF  at_data_request-informacoespedagio-cartaopedagio-codigo IS NOT INITIAL.
      READ TABLE t_values_card_ped INTO DATA(w_values_card_ped) WITH KEY domvalue_l = at_data_request-informacoespedagio-cartaopedagio-codigo.

      IF sy-subrc <> 0.
        set_mensagem( '09' ).
      ENDIF.
    ENDIF.

    IF at_data_request-informacoespedagio-municipios-origem-codigoibge IS NOT INITIAL.
      l_taxjurcode =  |{ at_data_request-informacoespedagio-municipios-origem-uf } { at_data_request-informacoespedagio-municipios-origem-codigoibge }|.

      SELECT SINGLE *
        INTO @DATA(_j_1btxjur)
        FROM j_1btxjur
       WHERE country    = 'BR'
         AND taxjurcode = @l_taxjurcode.

      IF sy-subrc <> 0.
        set_mensagem( '10' ).
      ENDIF.
    ENDIF.

    IF  at_data_request-informacoespedagio-municipios-destino-codigoibge IS NOT INITIAL.
      l_taxjurcode =  |{ at_data_request-informacoespedagio-municipios-destino-uf } { at_data_request-informacoespedagio-municipios-destino-codigoibge }|.

      SELECT SINGLE *
        INTO @DATA(_j_1btxjur2)
        FROM j_1btxjur
       WHERE country    = 'BR'
         AND taxjurcode = @l_taxjurcode.

      IF sy-subrc <> 0.
        set_mensagem( '11' ).
      ENDIF.
    ENDIF.

    IF at_data_request-informacoespedagio-ufspercurso[] IS NOT INITIAL.
      LOOP AT at_data_request-informacoespedagio-ufspercurso INTO DATA(w_ufs).
        SELECT SINGLE *
          INTO @DATA(_t005s)
          FROM t005s
         WHERE land1 = 'BR'
           AND bland = @w_ufs-uf.

        IF sy-subrc <> 0.
          set_mensagem( '12' ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.

    e_sucesso = abap_false.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    r_if_integracao_inject = me.

    IF     me->zif_integracao_inject~at_info_request_http-ds_url CS 'automatico'.
*-----------------------------
* --- Executar o Faturamento automatico
*-----------------------------
      me->set_executar_faturamento( EXPORTING i_msg_inbound          = i_msg_inbound
                                              i_msg_completa         = i_msg_completa
                                              i_id_integracao        = i_id_integracao
                                    IMPORTING e_msg_outbound         = e_msg_outbound
                                              e_sucesso              = e_sucesso
                                              e_nm_code              = e_nm_code
                                              e_msg_erro             = e_msg_erro  ).

    ELSEIF me->zif_integracao_inject~at_info_request_http-ds_url CS 'consulta'.
*-----------------------------
* --- Efetuar consulta no faturamento
*-----------------------------
      me->set_consulta_faturamento( EXPORTING i_msg_inbound          = i_msg_inbound
                                              i_msg_completa         = i_msg_completa
                                              i_id_integracao        = i_id_integracao
                                    IMPORTING e_msg_outbound         = e_msg_outbound
                                              e_sucesso              = e_sucesso
                                              e_nm_code              = e_nm_code
                                              e_msg_erro             = e_msg_erro  ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.
ENDCLASS.
