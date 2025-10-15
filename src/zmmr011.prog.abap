*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZMMR011                                                 *
* Descrição  : Rotina para ajustar o valor do custo dos materiais dos  *
*              centros fixo e a fixar, está rotina será mensal         *
*              executando A transação MR22. O valor do ajuste será     *
*              efetuado nos dois centros.                              *
* Módulo     : MM                               Transação: NA          *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Marcus Scauri Santos                   Data: 28/07/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT  zmmr011.

*----------------------------------------------------------------------*
* Tabelas globais
*----------------------------------------------------------------------*
DATA: t_zmmt0006 TYPE TABLE OF zmmt0006,
      t_return   TYPE TABLE OF bdcmsgcoll,
      t_bdc      TYPE TABLE OF bdcdata,
      t_log      TYPE TABLE OF zfie_ret_document.

*----------------------------------------------------------------------*
* Estruturas globais
*----------------------------------------------------------------------*
DATA: wa_zmmt0006      TYPE zmmt0006,
      wa_zmmt0006_docs TYPE zmmt0006_docs,
      wa_return        TYPE bdcmsgcoll,
      wa_options       TYPE ctu_params,
      wa_log           TYPE zfie_ret_document.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: c_n       TYPE c VALUE 'N',
           c_s       TYPE c VALUE 'S',
           c_e       TYPE c VALUE 'E',
           c_x       TYPE c VALUE 'X',
           c_mr22(4) TYPE c VALUE 'MR22'.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM zf_seleciona_dados.
  IF t_zmmt0006[] IS NOT INITIAL.
    PERFORM zf_executa_batch.
    IF t_log[] IS NOT INITIAL.
      PERFORM zf_envia_msg_legado.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona dados para o batch input
*----------------------------------------------------------------------*
FORM zf_seleciona_dados .

  DATA: vg_job TYPE i.

  REFRESH t_zmmt0006.

  SELECT SINGLE COUNT(*) INTO vg_job
     FROM tbtco
    WHERE jobname EQ 'AJUSTE_ESTOQUE_MR22'
      AND status EQ 'R'.

  IF ( vg_job EQ 1 ).
    SELECT *
      FROM zmmt0006
      INTO TABLE t_zmmt0006
      WHERE tcode         = c_mr22
        AND rg_atualizado = c_n.
  ENDIF.

ENDFORM.                    " ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_EXECUTA_BATCH
*&---------------------------------------------------------------------*
*       Executa batch input
*----------------------------------------------------------------------*
FORM zf_executa_batch .
  DATA: vl_data     TYPE sy-datum,
        vl_exbwr    TYPE ckmpc_zuumb,
        vl_mat_usd  TYPE ckmpc_zuumb,
        vl_mat_ufir TYPE ckmpc_zuumb.

  REFRESH: t_bdc,
           t_return.

  CLEAR wa_zmmt0006.

  LOOP AT t_zmmt0006 INTO wa_zmmt0006.

    wa_zmmt0006-rg_atualizado = c_s.
    MODIFY zmmt0006 FROM wa_zmmt0006.
    COMMIT WORK.

    "Verifica Duplicidade
    SELECT SINGLE *
      FROM zmmt0006_docs
      INTO wa_zmmt0006_docs
     WHERE ch_referencia = wa_zmmt0006-ch_referencia.

    IF sy-subrc IS INITIAL.
      CLEAR wa_log.

      CONCATENATE 'Chave de Referência:' wa_zmmt0006-ch_referencia 'gerado anteriormente!' INTO wa_log-message SEPARATED BY space.

      wa_log-obj_key        = wa_zmmt0006-ch_referencia.
      wa_log-interface      = wa_zmmt0006-id_interface.
      wa_log-dt_atualizacao = sy-datum.
      wa_log-hr_atualizacao = sy-uzeit.
      wa_log-type           = 'S'.
      wa_log-id             = 'MM'.
      wa_log-num            = '899'.
      wa_log-message_v1     = wa_zmmt0006_docs-mat_doc.
      wa_log-message_v2     = space.
      wa_log-message_v3     = space.
      wa_log-message_v4     = space.

      APPEND wa_log TO t_log.

      CONTINUE.
    ENDIF.
    "Verificação Duplicidade Fim


    CLEAR: vl_data, vl_exbwr, vl_mat_usd.

    WRITE wa_zmmt0006-budat TO vl_data.
    vl_exbwr = wa_zmmt0006-exbwr.
*---> 14/06/2023 - Migração S4 - JS
*    vl_mat_usd  = wa_zmmt0006-vr_material_usd.
*    vl_mat_ufir = wa_zmmt0006-vr_material_ufir.
    vl_mat_usd  = CONV #( wa_zmmt0006-vr_material_usd ).
    vl_mat_ufir = CONV #( wa_zmmt0006-vr_material_ufir ).
*<--- 14/06/2023 - Migração S4 - JS

* Mapeia tela inicial
    PERFORM zf_insere_bdc USING: 'X' 'SAPRCKM_MR22'           '0201',
                                 ' ' 'BDC_CURSOR'             'MR21HEAD-WERKS',
                                 ' ' 'BDC_OKCODE'             '=ENTR',
                                 ' ' 'MR21HEAD-BUDAT'         vl_data,
                                 ' ' 'MR21HEAD-BUKRS'         wa_zmmt0006-bukrs,
                                 ' ' 'MR21HEAD-WERKS'         wa_zmmt0006-werks.

    PERFORM zf_insere_bdc USING: 'X' 'SAPRCKM_MR22'           '0201',
                                 ' ' 'BDC_CURSOR'             'CKI_MR22_0250-ZUUMB(01)',
                                 ' ' 'BDC_OKCODE'             '=ENTR',
                                 ' ' 'CKI_MR22_0250-MATNR(01)' wa_zmmt0006-matnr,
                                 ' ' 'CKI_MR22_0250-ZUUMB(01)' vl_exbwr.

    PERFORM zf_insere_bdc USING: 'X' 'SAPRCKM_MR22'            '0201',
                                 ' ' 'BDC_CURSOR'              'CKI_MR22_0250-MATNR(02)',
                                 ' ' 'BDC_OKCODE'              '=TAB2'.

    PERFORM zf_insere_bdc USING: 'X' 'SAPRCKM_MR22'           '0201',
                                 ' ' 'BDC_CURSOR'             'CKI_MR22_0250-ZUUMB(01)',
                                 ' ' 'BDC_OKCODE'             '=ENTR',
                                 ' ' 'CKI_MR22_0250-ZUUMB(01)' vl_mat_usd.

    PERFORM zf_insere_bdc USING: 'X' 'SAPRCKM_MR22'            '0201',
                                 ' ' 'BDC_CURSOR'              'CKI_MR22_0250-MATNR(02)',
                                 ' ' 'BDC_OKCODE'              '=TAB3'.

    PERFORM zf_insere_bdc USING: 'X' 'SAPRCKM_MR22'           '0201',
                                 ' ' 'BDC_CURSOR'             'CKI_MR22_0250-ZUUMB(01)',
                                 ' ' 'BDC_OKCODE'             '=ENTR',
                                 ' ' 'CKI_MR22_0250-ZUUMB(01)' vl_mat_ufir.

    PERFORM zf_insere_bdc USING: 'X' 'SAPRCKM_MR22'             '0201',
                                 ' ' 'BDC_CURSOR'               'CKI_MR22_0250-MATNR(02)',
                                 ' ' 'BDC_OKCODE'               '=SAVE'.

    wa_options-dismode = c_n.
    CALL TRANSACTION c_mr22 USING t_bdc OPTIONS FROM wa_options
                            MESSAGES INTO t_return .

    PERFORM zf_trata_mensagens USING wa_zmmt0006-ch_referencia
                                     wa_zmmt0006-id_interface.

    LOOP AT t_return ASSIGNING FIELD-SYMBOL(<fs_return>) WHERE msgtyp = 'S' AND msgid = 'CKPRCH' AND msgnr = '019' AND msgv1 IS NOT INITIAL.

      UPDATE zmmt0006 SET doc_material =  <fs_return>-msgv1
        WHERE ch_referencia = wa_zmmt0006-ch_referencia..

      "Registra Vinculo
      CLEAR wa_zmmt0006_docs.
      wa_zmmt0006_docs-ch_referencia = wa_zmmt0006-ch_referencia.
      wa_zmmt0006_docs-bukrs         = wa_zmmt0006-bukrs.
      wa_zmmt0006_docs-mat_doc       = <fs_return>-msgv1.
      MODIFY zmmt0006_docs FROM wa_zmmt0006_docs.
      COMMIT WORK.

    ENDLOOP.


    CLEAR   wa_zmmt0006.
    REFRESH: t_bdc, t_return.

  ENDLOOP.

ENDFORM.                    " ZF_EXECUTA_BATCH
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERE_BDC
*&---------------------------------------------------------------------*
*       Insere dados na tabela BDC
*----------------------------------------------------------------------*
FORM zf_insere_bdc  USING p_dynbegin
                          p_field
                          p_value.

  DATA: wa_bdc TYPE bdcdata.

  CLEAR wa_bdc.

  IF p_dynbegin = c_x.
    wa_bdc-dynbegin = c_x.
    wa_bdc-program  = p_field.
    wa_bdc-dynpro   = p_value.
  ELSE.
    wa_bdc-fnam = p_field.
    IF p_field = 'CKI_MR22_0250-ZUUMB(01)'.
      WRITE p_value TO wa_bdc-fval.
      SHIFT wa_bdc-fval LEFT DELETING LEADING space.
    ELSE.
      wa_bdc-fval = p_value.
    ENDIF.
  ENDIF.

  APPEND wa_bdc TO t_bdc..

ENDFORM.                    " ZF_INSERE_BDC
*&---------------------------------------------------------------------*
*&      Form  ZF_TRATA_MENSAGENS
*&---------------------------------------------------------------------*
*       Trata mensagem de erro
*----------------------------------------------------------------------*
FORM zf_trata_mensagens USING p_ch_referencia TYPE any
                              p_id_interface  TYPE any.

  DATA: vl_language TYPE  t100-sprsl,
        vl_msg_id   TYPE  t100-arbgb,
        vl_msg_no   TYPE  t100-msgnr,
        vl_msg_var1 TYPE  balm-msgv1,
        vl_msg_var2 TYPE  balm-msgv2,
        vl_msg_var3 TYPE  balm-msgv3,
        vl_msg_var4 TYPE  balm-msgv4,
        vl_msg_text TYPE  string.

  vl_language = sy-langu.

  IF NOT t_return[] IS INITIAL.
    CLEAR wa_return.

    LOOP AT t_return INTO wa_return.

*Inicializa variáveis
      CLEAR: vl_msg_id,
             vl_msg_no,
             vl_msg_var1,
             vl_msg_var2,
             vl_msg_var3,
             vl_msg_var4,
             vl_msg_text.

*Atribui valor aos parametros
      vl_msg_id = wa_return-msgid.
      vl_msg_no = wa_return-msgnr.
      vl_msg_var1 = wa_return-msgv1.
      vl_msg_var2 = wa_return-msgv2.
      vl_msg_var3 = wa_return-msgv3.
      vl_msg_var4 = wa_return-msgv4.

*Monta mensagem
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          language = vl_language
          msg_id   = vl_msg_id
          msg_no   = vl_msg_no
          msg_var1 = vl_msg_var1
          msg_var2 = vl_msg_var2
          msg_var3 = vl_msg_var3
          msg_var4 = vl_msg_var4
        IMPORTING
          msg_text = vl_msg_text.

*Preenche campos para enviar msg
      wa_log-obj_key        = p_ch_referencia.
      wa_log-interface      = p_id_interface.
      wa_log-dt_atualizacao = sy-datum.
      wa_log-hr_atualizacao = sy-uzeit.
      wa_log-type           = wa_return-msgtyp.
      wa_log-id             = wa_return-msgid.
      wa_log-num            = wa_return-msgnr.
      wa_log-message        = vl_msg_text.
      wa_log-message_v1     = wa_return-msgv1.
      wa_log-message_v2     = wa_return-msgv2.
      wa_log-message_v3     = wa_return-msgv3.
      wa_log-message_v4     = wa_return-msgv4.

      APPEND wa_log TO t_log.
      CLEAR wa_log.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " ZF_TRATA_MENSAGENS
*&---------------------------------------------------------------------*
*&      Form  ZF_ENVIA_MSG_LEGADO
*&---------------------------------------------------------------------*
*       Envia msg para legado
*----------------------------------------------------------------------*
FORM zf_envia_msg_legado .

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*  CALL FUNCTION 'Z_FI_OUTBOUND_RETURN'
*    IN BACKGROUND TASK
*    DESTINATION 'XI_SIGAM_RETURN'
*    TABLES
*      outreturn = t_log.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = c_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      AS SEPARATE UNIT
      TABLES
        outreturn = t_log.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        outreturn = t_log.
  ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
  COMMIT WORK.

ENDFORM.                    " ZF_ENVIA_MSG_LEGADO
