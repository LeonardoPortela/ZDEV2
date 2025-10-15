class ZCL_CARGA_SAIDA_INSUMOS definition
  public
  final
  create public .

public section.

  class-methods ACEITE_FISCAL
    importing
      !I_NRO_CG type ZNRO_CG
      !I_NOTAS type ZSDS389_T
    exporting
      !E_MSG_ERRO type STRING .
  class-methods ANEXAR_DOCUMENTOS_CARGA
    importing
      !I_NRO_CARGA type ZDE_NRO_CG
      !I_ARQUIVOS type ZMMT_ARQUIVOS
    exporting
      !E_MSG_ERRO type STRING .
  class-methods ANULAR_CONFERENCIA
    importing
      !I_NRO_CG type ZNRO_CG optional
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods ATUALIZAR_DADOS_LOGISTICO
    importing
      !I_HEADER type ZSDS382
      !I_REMOVE_DATA_AUT_EMB type CHAR01 optional
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods ATUALIZA_SOL_SAFRA_CONTROL
    importing
      !I_SOLICITACOES_CURRENT type ZSDT0131_T
      !I_HEADER_CARGA type ZSDT0133
      !I_SOLICITACOES_SAFRA type ZSDS392_T
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods BLOQUEIO_DESBLOQUEIO_CARGA
    importing
      !I_NRO_CG type ZMMT0201-NRO_CG
      !I_BLOQUEIO type FLAG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods BUSCA_DADOS_CARGA
    importing
      !I_NR_CARGA type ZSDT_RANGE_NRO_CG optional
      !I_NR_CARGA_SINGLE type ZNRO_CG optional
      !I_ID_CARGA_SAFRA_CONTROL type ZDE_ID_CARGA_SAFRA_CONTROL optional
      !I_VKORG type SD_VKORG_RANGES optional
      !I_VKBUR type SHP_VKBUR_RANGE_T optional
      !I_INCO1 type SHP_INCO1_RANGE_T optional
      !I_SPART type SPART optional
      !I_KUNNR type JITO_KUNNR_RANGE_TT optional
      !I_NRO_SOL type ZSDT_RANGE_NRO_SOL optional
      !I_ID_VIAGEM type ZSDT_RANGE_ID_VIAGEM optional
      !I_ID_CARGA_SAFRA_CTRL type ZSDT_RANGE_ID_CARGA_SAFRA_CTRL optional
      !I_ORDEM_VENDA type SD_VBELN_RANGES optional
      !I_DT_CARGA type DATUM_RANGE_TAB optional
      !I_DADOS_CONFERENCIA type CHAR01 optional
      !I_SET_STATUS_CARGA type CHAR01 default 'X'
    exporting
      !E_CARGAS type ZSDS382_T
      !E_SOLICITACOES type ZSDS381_T
      !E_NOTAS_VENDA type ZSDT0410_T
      !E_NOTAS_TRANSF type ZSDT0410_T
      !E_NOTAS_CONFERENCIA type ZSDS390_T
      !E_OV_LOTES type ZSDS381_T
      !E_LOTES type ZSDT0134_T
      !E_ROMANEIOS type ZLESS0006_T
      !E_BORDERO type ZSDS387_T
      !E_DADOS_CONFERENCIA type ZSDS388_T .
  class-methods BUSCA_DADOS_MONTAR_CARGA
    importing
      !I_VKBUR type SHP_VKBUR_RANGE_T optional
      !I_VKORG type SD_VKORG_RANGES optional
      !I_SPART type SPART optional
      !I_KUNNR type JITO_KUNNR_RANGE_TT optional
      !I_INCO1 type INCO1 optional
      !I_NRO_SOL type ZSDT_RANGE_NRO_SOL optional
      !I_DT_LIB_SOL type DATUM_RANGE_TAB optional
      !I_ORDEM_VENDA type SD_VBELN_RANGES optional
      !I_ROTEIRO_PC type ZRANGE_NR_ROT optional
      !I_TIPO_SALDO type CHAR01
      !I_ORIGEM_ESTOQUE type ZDE_ORIGEM_ESTOQUE optional
      !I_NRO_CG_NO_CHECK_SALDO type ZNRO_CG optional
    exporting
      !E_TABELA_MONTA_CARGA type ZSDS381_T
      !E_MSG_ERRO type STRING
    returning
      value(SUCESSO) type CHAR1 .
  class-methods CALC_CATEGORIA_VEICULO
    importing
      !I_QUANTIDADE_KG type MENGE_D
    changing
      !C_CTG_TRANSP type CHAR01 optional
      !C_CTG_TRANSP_D type CHAR10 optional .
  class-methods CANCELAR_ACEITE
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
      !I_NRO_CG type ZNRO_CG
    exporting
      !E_MSG_ERRO type STRING .
  class-methods CANCELA_CARGA
    importing
      !I_NRO_CARGA type ZNRO_CG
      !I_MOTIVO type STRING optional
      !I_ORIGEM_CANCELAMENTO type CHAR01 optional
    exporting
      !E_MSG_ERRO type STRING
    returning
      value(SUCESSO) type CHAR01 .
  class-methods CHANGE_DELIVERY
    importing
      !I_VBELN type VBELN_VL
      !I_INCO1 type INCO1
    exporting
      value(E_RETURN) type BAPIRET2_T .
  class-methods CHECK_AND_SET_CARGA_CONFERIDA
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_CONFERIDA_TOTAL) type CHAR01 .
  class-methods CHECK_DISPARO_CARGUERO
    importing
      !I_MATKL type MATKL
    returning
      value(R_OK) type CHAR01 .
  class-methods CHECK_EMBARQUE_ARMAZEM
    importing
      !I_NRO_CG type ZNRO_CG optional
      !I_ROTEIRO_PC type Z_NR_ROT optional
    returning
      value(R_EMBARQUE_ARMAZEM) type CHAR01 .
  class-methods CHECK_PERMISSAO_CARGA
    importing
      !I_NRO_CARGA type ZNRO_CG
      !I_ATIVIDADE type CHAR02
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods CHECK_PERMISSAO_CARGA_CORE
    importing
      !I_BUKRS type BUKRS
      !I_ATIVIDADE type CHAR02
      !I_SPART type SPART
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods CHECK_REINICIALIZACAO_AUT_EMB
    importing
      !I_NRO_CARGA type ZNRO_CG
      !I_BACKGROUND type CHAR01 optional
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods CONFERIR_CARGA
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods CONFERIR_CARGA_COM_NF
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods CONFERIR_CARGA_SEM_NF
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods DEL_LOTE_PREENCHIMENTO_AUTO
    importing
      !I_NRO_CG type ZNRO_CG .
  class-methods DEL_SOL_CARGA_SAFRA_CONTROL
    importing
      !I_HEADER_CARGA type ZSDT0133
      !I_ZSDT0131_T type ZSDT0131_T
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods DESVINCULA_CONFERENCIA
    importing
      !I_CONFERENCIA type ZSDT0420_T
      !I_NRO_CG type ZNRO_CG
    exporting
      !E_MSG_ERRO type STRING .
  class-methods ENVIA_PATH_AUTORIZA_EMBARQUE
    importing
      !I_CARGA type ZNRO_CG
      !I_VIAGEM_ID type ZDE_VIAGEM_ID
      !I_PDF_XSTRING type XSTRING optional
    returning
      value(R_MSG_ERROR) type STRING
    exceptions
      ERRO_UPLOAD_NAO_AUTORIZADO
      ERRO_UPLOAD .
  class-methods ESTORNAR_ROMANEIOS
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods EXECUTAR_TROCA_NOTA
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods EXEC_FATURAMENTO_SAIDA_AUTO
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GERAR_AUTORIZACAO_EMBARQUE
    importing
      !I_NRO_CG type ZNRO_CG
      !I_BACKGROUND type CHAR01 optional
      !I_ENVIA_AUTORIZACAO_CARGUERO type CHAR01 optional
      !I_VIAGEM_ID type ZDE_VIAGEM_ID optional
      !I_WITH_LOTE type CHAR01 optional
    exporting
      !E_MSG_ERROR type STRING
    returning
      value(R_SUCESSO) type CHAR01 .
  class-methods GERAR_COTACAO
    importing
      !IT_NRO_CG type ZSDT_RANGE_NRO_CG
    exporting
      !E_NRO_CG_SUCESS type ZSDT_RANGE_NRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GERAR_LOTE_EMBARCADOR_CARGUERO
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GERAR_ROMANEIOS
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GET_AGENTE_FRETE
    importing
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
      !I_NRO_CG type ZNRO_CG
    exporting
      !E_INCO1 type INCO1
      !E_TDLNR type TDLNR .
  class-methods GET_DADOS_SOLITACOES
    importing
      !I_SOLICITACOES type ZSDS384_T
      !I_NRO_CG_NO_CHECK_SALDO type ZNRO_CG optional
    returning
      value(R_SOLICITACOES) type ZSDS381_T .
  class-methods GET_EMBARQUE_LUFT
    importing
      !I_NRO_CG type ZNRO_CG optional
      !I_ROTEIRO_PC type Z_NR_ROT optional
    exporting
      !E_LFA1_EMBARQUE type LFA1
    returning
      value(R_EMBARQUE_LUFT) type CHAR01 .
  class-methods GET_OBSERVACOES_LOTE_CARGUERO
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_OBSERVACOES) type STRING .
  class-methods GET_PREENCHIMENTO_LOTE_MANUAL
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_PREENCHE_MANUAL) type CHAR01 .
  class-methods GET_SOLICITACOES_SAFRA_CONTROL
    importing
      !I_ZSDT0131_T type ZSDT0131_T
    exporting
      !E_MSG_ERROR type STRING
    returning
      value(R_SOLICITACOES) type ZSDS392_T .
  class-methods GET_SPART_CARGA
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_SPART) type SPART .
  class-methods GET_TEXTO_ROTEITO
    importing
      value(I_NAME) type TDOBNAME
      !I_TP_EXEC type C default 'V'
    returning
      value(R_VALOR) type ICON_D .
  class-methods GRAVAR_CARGA
    importing
      !I_HEADER type ZSDS382
      !I_SOLICITACOES type ZSDS381_T optional
      !I_NOTAS_VENDA type ZSDT0410_T optional
      !I_NOTAS_TRANSFERENCIA type ZSDT0410_T optional
      !I_LOTES type ZSDT0134_T optional
      !I_LOTE_OV_SAVE type ZSDT0134 optional
      !I_ATUALIZA_DADOS_LOGISTICO type CHAR01 optional
      !I_CRIACAO_CARGA_SAFRA_CTRL type CHAR01 optional
    exporting
      !E_CARGA type ZNRO_CG
      !E_MSG_ERRO type STRING .
  class-methods GRAVAR_CARGA_CORE
    importing
      !I_LOTE_OV_SAVE type ZSDT0134 optional
      !I_ATUALIZA_DADOS_LOGISTICO type CHAR01 optional
      !I_CRIACAO_CARGA_SAFRA_CTRL type CHAR01 optional
    exporting
      !E_NRO_CARGA type ZNRO_CG
    changing
      !I_HEADER_LOTE type ZSDT0129 optional
      !I_CLIENTES_LOTE type ZSDT0130_T optional
      !I_ORDENS_LOTE type ZSDT0131_T optional
      !I_HEADER_CARGA type ZSDT0133 optional
      !I_NOTAS_VENDA type ZSDT0410_T optional
      !I_NOTAS_TRANSFERENCIA type ZSDT0410_T optional
      !I_LOTES type ZSDT0134_T optional
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods HABILITAR_EDICAO_CHAVES
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods HABILITAR_EDICAO_DADOS_LOGIST
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods HABILITAR_EDICAO_LOTES
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods HABILITAR_EDICAO_SOLICITACOES
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods HABILITAR_EDICAO_FRETE
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GRAVAR_AJUSTE_FRETE
    importing
      !I_HEADER type ZSDS382
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods INFORMA_QTD_CONFERENCIA
    importing
      !I_DE_PARA type ZSDS388
      !I_NRO_CARGA type ZNRO_CG
    exporting
      !E_MSG_ERRO type STRING
    changing
      !C_BORDERO type ZSDS387 optional
      !C_ITEM_CARGA type ZSDS381 optional
      !C_NOTA type ZSDS390 optional .
  class-methods LISTA_STATUS_CARGA
    importing
      !I_SHOW_STATUS type CHAR01 default 'X'
    returning
      value(R_STATUS_CARGA) type ZSDS386_T .
  class-methods MONTA_DADOS_ACEITE_FISCAL
    importing
      !I_NOTAS type ZSDS390_T
      !I_ITENS_CARGA type ZSDS381_T
      !I_BORDERO type ZSDS387_T
      !I_DE_PARA type ZSDS388_T
      !I_CANCELAR_ACEITE type CHAR1 optional
    exporting
      !E_DADOS_ACEITE_FISCAL type ZSDS391_T
      !E_MSG_ERRO type STRING .
  class-methods POST_SOL_CARGA_SAFRA_CONTROL
    importing
      !I_HEADER_CARGA type ZSDT0133
      !I_ZSDT0131_T type ZSDT0131_T
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods PREENCHER_LOTES_CARGA
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods PREPARAR_GRAVACAO
    changing
      !I_ORDENS_LOTE type ZSDT0131_T optional
      !I_HEADER_CARGA type ZSDT0133 optional
      !I_NOTAS_VENDA type ZSDT0410_T optional
      !I_NOTAS_TRANSFERENCIA type ZSDT0410_T optional
      !I_LOTES type ZSDT0134_T optional
      !I_HEADER_LOTE type ZSDT0129 optional
      !I_CLIENTES_LOTE type ZSDT0130_T optional
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods SET_STATUS_CARGA
    changing
      !I_ZSDT0133 type ZSDT0133_T .
  class-methods VALIDAR_GERACAO_ROMANEIOS
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods VAL_QTDE_SOL_SAFRA_CONTROL
    importing
      !I_SOLICITACOES_CURRENT type ZSDT0131_T
      !I_SOLICITACOES_OLD type ZSDT0131_T
      !I_HEADER_CARGA type ZSDT0133
    exporting
      !E_SOLICITACOES_SAFRA type ZSDS392_T
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods VINCULAR_CONFERENCIA
    importing
      !I_NRO_CARGA type ZNRO_CG
      !I_BORDERO type ZSDS387 optional
      !I_ITEM_CARGA type ZSDS381
      !I_NOTA type ZSDS390 optional
    exporting
      !E_ZSDT0420 type ZSDT0420
      !E_MSG_ERRO type STRING .
  class-methods ENVIAR_DOCUMENTOS_CARGUERO
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GET_ORIGEM_ESTOQUE_CARGA
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_ORIGEM_ESTOQUE) type ZSDT0082-ORIGEM_ESTOQUE .
  class-methods CHECK_CARGA_TROCA_NOTA
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_TROCA_NOTA) type CHAR01 .
  class-methods CHECK_CONFERENCIA_COM_DEPARA
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_CONFERENCIA_COM_DEPARA) type CHAR01 .
  class-methods GERAR_SEQ_CARREGAMENTO_LUFT
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_MSG_ERROR) type CHAR01 .
  class-methods CONSOLIDA_STATUS_SOLICITACAO
    importing
      !I_NRO_SOL type ZDE_NRO_SOL
      !I_SEQ type NUMC3
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA .
  class-methods CHECK_ENTRADA_NFE_PENDENTE
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  PROTECTED SECTION.
private section.

  class-methods CRIAR_PEDIDO_LUFT_V1
    importing
      !I_NRO_CG type ZNRO_CG
      !I_NRO_PEDIDO_LUFT type ZDE_NRO_PEDIDO_LUFT
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GERAR_AUTORIZACAO_EMBARQUE_SM
    importing
      !I_CARGA type ZNRO_CG
      !I_BINARY type CHAR1 optional
      !I_PREVIEW type CHAR1 optional
      !I_WITH_LOTE type CHAR01 optional
    exporting
      !E_PDF_XSTRING type XSTRING .
  class-methods GERAR_AUTORIZACAO_EMB_CORE
    importing
      !I_NRO_CG type ZNRO_CG
      !I_ENVIA_AUTORIZACAO_CARGUERO type CHAR01 optional
      !I_BACKGROUND type CHAR01 optional
      !I_WITH_LOTE type CHAR01 optional
      !I_VIAGEM_ID type ZDE_VIAGEM_ID optional
    exporting
      !E_MSG_ERROR type STRING
    returning
      value(R_SUCESSO) type CHAR01 .
  class-methods GERAR_ROMANEIOS_CORE
    importing
      !I_NRO_CG type ZNRO_CG
    exporting
      !E_ROMANEIOS_GERADOS type CHAR01
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods ROLLBACK_CARGA
    importing
      !I_NRO_CG type ZNRO_CG
      !I_NRO_LOTE type ZNRO_LOTE
      !I_ZSDT0133_OLD type ZSDT0133
      !I_ZSDT0129_OLD type ZSDT0129
      !I_ZSDT0130_T_OLD type ZSDT0130_T
      !I_ZSDT0131_T_OLD type ZSDT0131_T
      !I_ZSDT0410_T_OLD type ZSDT0410_T
      !I_ZSDT0134_T_OLD type ZSDT0134_T
      !I_LOTES type ZSDT0134_T
      !I_REGISTRO_NEW type CHAR01 .
ENDCLASS.



CLASS ZCL_CARGA_SAIDA_INSUMOS IMPLEMENTATION.


  METHOD anexar_documentos_carga.

    DATA: lo_attach    TYPE REF TO cl_gos_document_service,
          ls_object    TYPE borident,
          ls_retorno   TYPE borident-objkey,
          ls_atta_list TYPE sap_bool,
          ls_msg       TYPE symsg.

    DATA: lt_solix          TYPE solix_tab,
          lv_fol_id         TYPE soodk,
          ls_doc_data       TYPE sodocchgi1,
          lt_header         TYPE STANDARD TABLE OF solisti1,
          lt_return         TYPE bapiret2_tab,
          ls_object2        TYPE sibflporb,
          ls_objtgt         TYPE sibflporb,
          ls_file           TYPE zmms_bol_comp_anexo,
          ls_doc_info       TYPE sofolenti1,
          lv_idboletacompra TYPE zde_id_boleta_compra,
          ld_doc_type       TYPE so_obj_tp,
          ls_doc_id         TYPE soodk,
          ls_header         TYPE sood1,
          lt_head           TYPE TABLE OF soli,
          lv_xstring        TYPE xstring.

    IF i_arquivos IS INITIAL.

      CREATE OBJECT lo_attach.

      ls_object-objkey  = i_nro_carga.
      ls_object-objtype = 'ZSDT0112'.

      CALL FUNCTION 'GOS_EXECUTE_SERVICE'
        EXPORTING
          ip_service         = 'PCATTA_CREA'
          is_object          = ls_object
          ip_no_commit       = abap_false
          ip_popup           = abap_false
          ip_rwmod           = 'E'
        IMPORTING
          ep_message         = ls_msg
          ep_check_atta_list = ls_atta_list
        EXCEPTIONS
          execution_failed   = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.

        MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO e_msg_erro.

      ENDIF.

    ELSE.

      CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
        EXPORTING
          region                = 'B'
        IMPORTING
          folder_id             = lv_fol_id
        EXCEPTIONS
          communication_failure = 1
          owner_not_exist       = 2
          system_failure        = 3
          x_error               = 4
          OTHERS                = 5.

      LOOP AT i_arquivos ASSIGNING FIELD-SYMBOL(<fs_arquivos>).

        lt_solix = cl_bcs_convert=>xstring_to_solix( iv_xstring = <fs_arquivos>-xstring ).

        ls_doc_data-doc_size  = xstrlen( <fs_arquivos>-xstring ).
        ls_doc_data-obj_descr = <fs_arquivos>-descricao.
        ls_doc_data-obj_name  = <fs_arquivos>-descricao.
        MOVE <fs_arquivos>-tipo TO ld_doc_type.

        lt_header = VALUE #( ( |&SO_FILENAME={ <fs_arquivos>-descricao }| )
                             ( |&SO_FORMAT=BIN | ) ).


        TRY.

            CALL FUNCTION 'SO_DOCUMENT_INSERT_API1'
              EXPORTING
                folder_id                  = lv_fol_id
                document_data              = ls_doc_data
                document_type              = ld_doc_type
              IMPORTING
                document_info              = ls_doc_info
              TABLES
                object_header              = lt_header
                contents_hex               = lt_solix
              EXCEPTIONS
                folder_not_exist           = 1
                document_type_not_exist    = 2
                operation_no_authorization = 3
                parameter_error            = 4
                x_error                    = 5
                enqueue_error              = 6
                OTHERS                     = 7.
          CATCH cx_root.

        ENDTRY.


        IF sy-subrc = 0.

          ls_object2-instid  = CONV #( i_nro_carga ).
          ls_object2-typeid  = 'ZSDT0112'.
          ls_object2-catid   = 'BO'.
          ls_objtgt-instid  = ls_doc_info-doc_id.
          ls_objtgt-typeid  = 'MESSAGE'.
          ls_objtgt-catid   = 'BO'.

          TRY.
              cl_binary_relation=>create_link( EXPORTING is_object_a = ls_object2
                                                         is_object_b = ls_objtgt
                                                         ip_reltype  = 'ATTA' ).
              COMMIT WORK AND WAIT.
            CATCH cx_obl_parameter_error cx_obl_model_error cx_obl_internal_error.
          ENDTRY.
        ELSE.
          e_msg_erro = 'Não foi possivel anexar os arquivos na carga!'.
        ENDIF.


      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD atualizar_dados_logistico.

    DATA: lv_dt_null TYPE zsdt0133-dt_autorizacao_embarque.

    IF i_header-nro_cg IS INITIAL.
      r_msg_error = 'Numero Carga não informada!'.
      RETURN.
    ENDIF.

    DATA(lra_nr_carga) = VALUE zsdt_range_nro_cg( ( sign = 'I' option = 'EQ' low = i_header-nro_cg ) ).

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga     = lra_nr_carga
      IMPORTING
        e_cargas       = DATA(lit_carga)
        e_romaneios    = DATA(lit_romaneios)
         ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      r_msg_error = 'Não foi possivel localizar a Carga!'.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS NOT INITIAL .
      r_msg_error = |Carga com romaneios gerados! Operação não permitida!|.
      RETURN.
    ENDIF.

*------------------------------------------------------------------------------------------------------------*
*   Determinar Campos Atualização
*------------------------------------------------------------------------------------------------------------*

    lwa_carga-cod_transportadora   = i_header-cod_transportadora.
    lwa_carga-preco_frete          = i_header-preco_frete.
    lwa_carga-inco1                = i_header-inco1.
    lwa_carga-frete_por            = i_header-frete_por.
    lwa_carga-frete_por_t          = i_header-frete_por_t.
    lwa_carga-frete_por_v          = i_header-frete_por_v.
    lwa_carga-viagem_id            = i_header-viagem_id.
    lwa_carga-placa_cav            = i_header-placa_cav.
    lwa_carga-placa_car1           = i_header-placa_car1.
    lwa_carga-placa_car2           = i_header-placa_car2.
    lwa_carga-placa_car3           = i_header-placa_car3.
    lwa_carga-motorista            = i_header-motorista.
    lwa_carga-ds_conjunto_transp   = i_header-ds_conjunto_transp.

    zcl_carga_saida_insumos=>gravar_carga(
      EXPORTING
        i_header                   = lwa_carga
        i_atualiza_dados_logistico = abap_true
      IMPORTING
        e_carga               = DATA(lva_nro_carga)
        e_msg_erro            = r_msg_error
    ).

    IF i_remove_data_aut_emb EQ abap_true.
      zcl_carga_saida_insumos=>check_reinicializacao_aut_emb( EXPORTING i_nro_carga  = lwa_carga-nro_cg
                                                                        i_background = abap_true ).
    ENDIF.


  ENDMETHOD.


  METHOD bloqueio_desbloqueio_carga.

    DATA: lva_chave_sol  TYPE zde_chave_sol.

    CLEAR: r_msg_error.

    "Seleciona Cabeçalho Carga
    SELECT SINGLE *
      FROM zsdt0133 INTO @DATA(lw_0133)
      WHERE nro_cg EQ @i_nro_cg.

    CHECK sy-subrc EQ 0.

    "Seleciona Cabeçalho Lote
    SELECT SINGLE *
      FROM zsdt0129 INTO @DATA(lw_0129)
     WHERE nro_cg EQ @i_nro_cg.

    "Seleciona Solicitações Carga
    SELECT *
      FROM zsdt0131 INTO TABLE @DATA(lt_0131)
     WHERE nro_lote EQ @lw_0129-nro_lote.

    CASE i_bloqueio.
      WHEN abap_true. "Bloquear

        "Bloquear Carga
        CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
          EXPORTING
            chave          = i_nro_cg
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO r_msg_error.
          RETURN.
        ENDIF.

        "Bloquear Solicitações
        LOOP AT lt_0131 ASSIGNING FIELD-SYMBOL(<fs_0131>).

          CONCATENATE <fs_0131>-nro_sol <fs_0131>-seq <fs_0131>-vbeln <fs_0131>-posnr INTO lva_chave_sol.

          CALL FUNCTION 'ZENQUEUE_SD_SOL_INSUMOS'
            EXPORTING
              chave          = lva_chave_sol
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc NE 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO r_msg_error.
            RETURN.
          ENDIF.

        ENDLOOP.

      WHEN abap_false.

        "Desbloquear Carga
        CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
          EXPORTING
            chave = i_nro_cg.

        "Desbloquear Solicitações
        LOOP AT lt_0131 ASSIGNING <fs_0131>.

          CONCATENATE <fs_0131>-nro_sol <fs_0131>-seq <fs_0131>-vbeln <fs_0131>-posnr INTO lva_chave_sol.

          CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
            EXPORTING
              chave = lva_chave_sol.
        ENDLOOP.



    ENDCASE.


  ENDMETHOD.


  METHOD busca_dados_carga.

    TYPES: BEGIN OF ty_active,
             docnum    TYPE j_1bnfe_active-docnum,
             regio     TYPE j_1bnfe_active-regio,
             nfyear    TYPE j_1bnfe_active-nfyear,
             nfmonth   TYPE j_1bnfe_active-nfmonth,
             stcd1     TYPE j_1bnfe_active-stcd1,
             model     TYPE j_1bnfe_active-model,
             serie     TYPE j_1bnfe_active-serie,
             nfnum9    TYPE j_1bnfe_active-nfnum9,
             docnum9   TYPE j_1bnfe_active-docnum9,
             cdv       TYPE j_1bnfe_active-cdv,
             chave_nfe TYPE zde_chave_doc_e,
           END OF ty_active.

    DATA: lit_zsdt0133 TYPE zsdt0133_t.

    DATA: lva_preco_frete_rom TYPE zsdt0133-preco_frete.
    DATA: lva_preco_frete_itm TYPE zsdt0133-preco_frete.
    DATA: lva_dif_rat         TYPE zsdt0133-preco_frete.
    DATA: lva_total_frete_rat TYPE zsdt0133-preco_frete.

    DATA: lva_preco_frete_total TYPE zsdt0133-preco_frete.

    DATA: lit_active TYPE TABLE OF ty_active.

    DATA: lt_celltab TYPE TABLE OF lvc_s_styl,
          ls_celltab TYPE lvc_s_styl.

    DATA: lv_qtd_conf  TYPE menge_d.

    DATA: lit_solicitacoes_carga TYPE zsds384_t.
    DATA: lra_spart TYPE wtysc_spart_ranges_tab.
    DATA: lra_nro_carga TYPE zsdt_range_nro_cg.
    DATA: lra_id_carga_safra_control TYPE RANGE OF zsdt0133-id_carga_safra_control.

    CLEAR: e_cargas[],e_solicitacoes[],e_notas_venda[],e_notas_transf[],e_ov_lotes[],e_lotes[],e_romaneios[].

    IF i_spart IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_spart ) TO lra_spart.
    ENDIF.

    IF i_id_carga_safra_control IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_id_carga_safra_control ) TO lra_id_carga_safra_control.
    ENDIF.

    IF i_id_carga_safra_ctrl[] IS NOT INITIAL.
      APPEND LINES OF i_id_carga_safra_ctrl TO lra_id_carga_safra_control.
    ENDIF.

    lra_nro_carga = i_nr_carga.

    IF i_nr_carga_single IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_nr_carga_single ) TO lra_nro_carga.
    ENDIF.

*------------------------------------------------------------------------------------------------------------*
*   Seleção Cargas Cabealho
*------------------------------------------------------------------------------------------------------------*

    CLEAR: e_cargas[].

    SELECT *
      FROM zsdt0133 INTO TABLE lit_zsdt0133
     WHERE data_atual IN i_dt_carga
       AND nro_cg     IN lra_nro_carga
       AND id_carga_safra_control IN lra_id_carga_safra_control
       AND viagem_id  IN i_id_viagem
       AND EXISTS ( SELECT *
                     FROM zsdt0129 INNER JOIN zsdt0131 ON zsdt0129~nro_lote EQ zsdt0131~nro_lote
                    WHERE zsdt0129~nro_cg   EQ zsdt0133~nro_cg
                      AND zsdt0129~inco1    IN i_inco1
                      AND zsdt0129~status   NE abap_true
                      AND zsdt0131~vkorg    IN i_vkorg
                      AND zsdt0131~vkbur    IN i_vkbur
                      AND zsdt0131~spart    IN lra_spart
                      AND zsdt0131~kunnr    IN i_kunnr
                      AND zsdt0131~vbeln    IN i_ordem_venda
                      AND zsdt0131~nro_sol  IN i_nro_sol
                      AND zsdt0131~status   NE abap_true ).

    CHECK lit_zsdt0133[] IS NOT INITIAL.

*------------------------------------------------------------------------------------------------------------*
*   Seleção Tabelas Complemantares Carga
*------------------------------------------------------------------------------------------------------------*

    "Seleção Cabeçalho Lote Carga
    SELECT *
      FROM zsdt0129 INTO TABLE @DATA(lit_zsdt0129)
      FOR ALL ENTRIES IN @lit_zsdt0133
      WHERE nro_cg EQ @lit_zsdt0133-nro_cg
        AND status NE 'X'.

    "Notas da Carga
    SELECT *
      FROM zsdt0410 INTO TABLE @DATA(lit_zsdt0410)
       FOR ALL ENTRIES IN @lit_zsdt0133
     WHERE nro_cg EQ @lit_zsdt0133-nro_cg.

    DELETE lit_zsdt0410 WHERE cancel EQ abap_true.

    "Seleção Clientes dos Lote Carga
    SELECT *
      FROM zsdt0130 INTO TABLE @DATA(lit_zsdt0130)
       FOR ALL ENTRIES IN @lit_zsdt0129
     WHERE nro_lote EQ @lit_zsdt0129-nro_lote
       AND status NE 'X'.

    "Ordens do Lote
    SELECT *
      FROM zsdt0131  INTO TABLE @DATA(lit_zsdt0131)
       FOR ALL ENTRIES IN @lit_zsdt0129
     WHERE nro_lote EQ @lit_zsdt0129-nro_lote
       AND status   NE 'X'.

    IF lit_zsdt0131[] IS NOT INITIAL.
      MOVE-CORRESPONDING lit_zsdt0131[] TO lit_solicitacoes_carga[].

      "Recuperar Dados Complementares Solicitação
      zcl_carga_saida_insumos=>get_dados_solitacoes(
        EXPORTING
          i_solicitacoes          = lit_solicitacoes_carga
        RECEIVING
          r_solicitacoes = DATA(lit_dados_comp_sol) ).

      "Lotes Carga
      SELECT *
        FROM zsdt0134 INTO TABLE @DATA(lit_zsdt0134)
        FOR ALL ENTRIES IN @lit_zsdt0133
       WHERE nro_cg EQ @lit_zsdt0133-nro_cg
         AND status NE 'X'.

    ENDIF.

    "Romaneios Carga
    IF lit_zsdt0131[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0001 INTO TABLE @DATA(lit_zsdt0001)
        FOR ALL ENTRIES IN @lit_zsdt0131
       WHERE vbeln EQ @lit_zsdt0131-vbeln.

      LOOP AT lit_zsdt0001 ASSIGNING FIELD-SYMBOL(<fs_zsdt0001>).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_zsdt0001>-nro_nf_prod
          IMPORTING
            output = <fs_zsdt0001>-nro_nf_prod.
      ENDLOOP.

      IF lit_zsdt0001[] IS NOT INITIAL.

        SELECT docnum regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
          FROM j_1bnfe_active INTO CORRESPONDING FIELDS OF TABLE lit_active
           FOR ALL ENTRIES IN lit_zsdt0001
          WHERE docnum = lit_zsdt0001-nro_nf_prod.

        LOOP AT  lit_active ASSIGNING FIELD-SYMBOL(<fs_active>).
          <fs_active>-chave_nfe = <fs_active>-regio &&
                                  <fs_active>-nfyear &&
                                  <fs_active>-nfmonth &&
                                  <fs_active>-stcd1 &&
                                  <fs_active>-model &&
                                  <fs_active>-serie &&
                                  <fs_active>-nfnum9 &&
                                  <fs_active>-docnum9 &&
                                  <fs_active>-cdv.
        ENDLOOP.

        IF lit_active[] IS NOT INITIAL.
          SELECT *
            FROM zsdt0375 INTO TABLE @DATA(lit_zsdt0375)
             FOR ALL ENTRIES IN @lit_active
           WHERE chave_nfe EQ @lit_active-chave_nfe.
        ENDIF.

        SELECT *
          FROM zsdt0001_item INTO TABLE @DATA(lit_zsdt0001_item)
           FOR ALL ENTRIES IN @lit_zsdt0001
         WHERE ch_referencia EQ @lit_zsdt0001-ch_referencia.

      ENDIF.

    ENDIF.

    "Dados Conferencia
    IF i_dados_conferencia EQ abap_true.
      SELECT *
        FROM zsdt0420 INTO CORRESPONDING FIELDS OF TABLE e_dados_conferencia
         FOR ALL ENTRIES IN lit_zsdt0133
       WHERE nro_cg EQ lit_zsdt0133-nro_cg
         AND cancel EQ abap_false.
    ENDIF.

    "Bordero Saida
    SELECT *
      FROM zsdt0376 INTO CORRESPONDING FIELDS OF TABLE e_bordero
       FOR ALL ENTRIES IN lit_zsdt0133
     WHERE id_autorizacao_embarque EQ lit_zsdt0133-nro_cg.

    IF e_bordero[] IS NOT INITIAL.

      SELECT a~matnr , a~maktx, b~meins
        FROM makt AS a INNER JOIN mara AS b ON a~matnr = b~matnr INTO TABLE @DATA(lit_makt_bordero)
         FOR ALL ENTRIES IN @e_bordero
       WHERE a~matnr EQ @e_bordero-matnr
         AND a~spras  EQ @sy-langu.

      SORT lit_makt_bordero BY matnr.

      LOOP AT e_bordero ASSIGNING FIELD-SYMBOL(<fs_bordero>).

        READ TABLE lit_makt_bordero INTO DATA(lwa_makt_bordero) WITH KEY matnr = <fs_bordero>-matnr BINARY SEARCH.
        CHECK sy-subrc EQ 0.

        <fs_bordero>-ds_produto  = lwa_makt_bordero-maktx.
        <fs_bordero>-embalagem   = lwa_makt_bordero-meins.
        <fs_bordero>-saldo_conf  = <fs_bordero>-quantidade.

        LOOP AT e_dados_conferencia INTO DATA(lwa_conf) WHERE nro_cg               = <fs_bordero>-id_autorizacao_embarque
                                                          AND id_item_carregamento = <fs_bordero>-id_item
                                                          AND lote_carregamento    = <fs_bordero>-lote.
          SUBTRACT lwa_conf-quantidade FROM <fs_bordero>-saldo_conf.
        ENDLOOP.

      ENDLOOP.

    ENDIF.



*------------------------------------------------------------------------------------------------------------*
*   Seleção Tabelas Auxiliares
*------------------------------------------------------------------------------------------------------------*
    SELECT *
      FROM lfa1 INTO TABLE @DATA(lit_lfa1)
      FOR ALL ENTRIES IN @lit_zsdt0133
      WHERE lifnr EQ @lit_zsdt0133-cod_transportadora.

    SELECT *
      FROM zlest0181 INTO TABLE @DATA(lit_zlest0181)
      FOR ALL ENTRIES IN @lit_zsdt0133
     WHERE id_lote_frete EQ @lit_zsdt0133-id_lote_frete.

    IF lit_zsdt0129 IS NOT INITIAL.
      SELECT *
        FROM lfa1 APPENDING TABLE @lit_lfa1
         FOR ALL ENTRIES IN @lit_zsdt0129
       WHERE lifnr EQ @lit_zsdt0129-motorista.
    ENDIF.

    IF lit_zsdt0131[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0132 INTO TABLE @DATA(it_zsdt0132)
         FOR ALL ENTRIES IN @lit_zsdt0131
       WHERE nr_rot EQ @lit_zsdt0131-cod_loc_emb.

      IF it_zsdt0132[] IS NOT INITIAL.
        SELECT *
          FROM lfa1 APPENDING TABLE @DATA(it_lfa1)
           FOR ALL ENTRIES IN @it_zsdt0132
         WHERE lifnr EQ @it_zsdt0132-lifnr.
      ENDIF.
    ENDIF.

*------------------------------------------------------------------------------------------------------------*
*   Montagem Dados Saida
*------------------------------------------------------------------------------------------------------------*

    "Ordens da Carga
    LOOP AT lit_zsdt0131 ASSIGNING FIELD-SYMBOL(<fs_zsdt0131>).

      APPEND INITIAL LINE TO e_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao_carga>).

      READ TABLE lit_dados_comp_sol INTO DATA(lwa_dados_comp_sol) WITH KEY nro_sol = <fs_zsdt0131>-nro_sol
                                                                           seq     = <fs_zsdt0131>-seq
                                                                           vbeln   = <fs_zsdt0131>-vbeln
                                                                           posnr   = <fs_zsdt0131>-posnr.
      CHECK sy-subrc EQ 0.

      READ TABLE lit_zsdt0129 INTO DATA(lwa_zsdt0129) WITH KEY nro_lote = <fs_zsdt0131>-nro_lote.
      CHECK sy-subrc EQ 0.

      MOVE-CORRESPONDING lwa_dados_comp_sol TO <fs_solicitacao_carga>.

      <fs_solicitacao_carga>-nro_lote               =    <fs_zsdt0131>-nro_lote.
      <fs_solicitacao_carga>-nro_sol                =    <fs_zsdt0131>-nro_sol.
      <fs_solicitacao_carga>-seq                    =    <fs_zsdt0131>-seq.
      <fs_solicitacao_carga>-kunnr                  =    <fs_zsdt0131>-kunnr.
      <fs_solicitacao_carga>-vbeln                  =    <fs_zsdt0131>-vbeln.
      <fs_solicitacao_carga>-posnr                  =    <fs_zsdt0131>-posnr.
      <fs_solicitacao_carga>-auart                  =    <fs_zsdt0131>-auart.
      <fs_solicitacao_carga>-spart                  =    <fs_zsdt0131>-spart.
      <fs_solicitacao_carga>-vkorg                  =    <fs_zsdt0131>-vkorg.
      <fs_solicitacao_carga>-vkbur                  =    <fs_zsdt0131>-vkbur.
      <fs_solicitacao_carga>-werks                  =    <fs_zsdt0131>-werks.
      <fs_solicitacao_carga>-matnr                  =    <fs_zsdt0131>-matnr.
      <fs_solicitacao_carga>-item_carga             =    <fs_zsdt0131>-item_carga.
      <fs_solicitacao_carga>-seq_carregamento_luft  =    <fs_zsdt0131>-seq_carregamento_luft.
      <fs_solicitacao_carga>-dt_entrega_efetiva     =    <fs_zsdt0131>-dt_entrega_efetiva.
      <fs_solicitacao_carga>-qtd_vinc               =    <fs_zsdt0131>-qtd_vinc.
      <fs_solicitacao_carga>-meins                  =    <fs_zsdt0131>-um.
      <fs_solicitacao_carga>-brgew                  =    <fs_zsdt0131>-brgew.
      <fs_solicitacao_carga>-qtd_vinc_kg            =    <fs_zsdt0131>-qtd_emkg.
      <fs_solicitacao_carga>-cod_loc_emb            =    <fs_zsdt0131>-cod_loc_emb.
      <fs_solicitacao_carga>-local_embarq           =    <fs_zsdt0131>-local_embarq.
      <fs_solicitacao_carga>-status                 =    <fs_zsdt0131>-status.
      <fs_solicitacao_carga>-saldo                  =    <fs_solicitacao_carga>-saldo + <fs_zsdt0131>-qtd_vinc.

      READ TABLE lit_zsdt0130 INTO DATA(lwa_zsdt0130) WITH KEY nro_lote  = <fs_zsdt0131>-nro_lote
                                                               nro_sol   = <fs_zsdt0131>-nro_sol
                                                               seq       = <fs_zsdt0131>-seq
                                                               kunnr     = <fs_zsdt0131>-kunnr.
      IF sy-subrc EQ 0.
        <fs_solicitacao_carga>-seq_entrega  = lwa_zsdt0130-seq_ent_cg.
      ENDIF.

      CASE lwa_dados_comp_sol-konv_kmein.
        WHEN 'TO'.
          <fs_solicitacao_carga>-vlr_solicitacao = <fs_zsdt0131>-qtd_vinc * lwa_dados_comp_sol-brgew * lwa_dados_comp_sol-konv_kbetr / 1000 * lwa_dados_comp_sol-konv_kkurs.
        WHEN 'KG'.
          <fs_solicitacao_carga>-vlr_solicitacao = <fs_zsdt0131>-qtd_vinc * lwa_dados_comp_sol-brgew * lwa_dados_comp_sol-konv_kbetr / 1 * lwa_dados_comp_sol-konv_kkurs.
        WHEN OTHERS.
          <fs_solicitacao_carga>-vlr_solicitacao = <fs_zsdt0131>-qtd_vinc * lwa_dados_comp_sol-konv_kbetr / 1 * lwa_dados_comp_sol-konv_kkurs.
      ENDCASE.

      "Conferencia
      <fs_solicitacao_carga>-saldo_conf = <fs_zsdt0131>-qtd_vinc.

      LOOP AT e_dados_conferencia INTO lwa_conf WHERE nro_cg    = lwa_zsdt0129-nro_cg
                                                  AND nro_sol   = <fs_solicitacao_carga>-nro_sol
                                                  AND seq       = <fs_solicitacao_carga>-seq.
        SUBTRACT lwa_conf-quantidade FROM <fs_solicitacao_carga>-saldo_conf.
      ENDLOOP.

    ENDLOOP.



    "Notas Carga
    LOOP AT lit_zsdt0410 ASSIGNING FIELD-SYMBOL(<fs_zsdt0410>).

      CASE <fs_zsdt0410>-processo.
        WHEN '1'. "NF Transf.
          APPEND INITIAL LINE TO e_notas_transf ASSIGNING FIELD-SYMBOL(<fs_nf_transf>).
          MOVE-CORRESPONDING <fs_zsdt0410> TO <fs_nf_transf>.
        WHEN '2'. "NF Compra
          APPEND INITIAL LINE TO e_notas_venda ASSIGNING FIELD-SYMBOL(<fs_nf_venda>).
          MOVE-CORRESPONDING <fs_zsdt0410> TO <fs_nf_venda>.
      ENDCASE.

    ENDLOOP.
**<<<------"169665 - NMS - INI------>>>
    IF e_notas_venda[] IS NOT INITIAL.
      SELECT *
        FROM zib_nfe_dist_itm INTO TABLE @DATA(lit_zib_nfe_dist_itm)
         FOR ALL ENTRIES IN @e_notas_venda
       WHERE chave_nfe EQ @e_notas_venda-chave_nfe.

      SELECT *
        FROM zib_nfe_dist_ter INTO TABLE @DATA(lit_zib_nfe_dist_ter)
         FOR ALL ENTRIES IN @e_notas_venda
      WHERE chave_nfe EQ @e_notas_venda-chave_nfe.

    ENDIF.
**<<<------"169665 - NMS - FIM------>>>
    "Notas Conferencia
    IF i_dados_conferencia EQ abap_true.

      IF e_notas_venda[] IS NOT INITIAL.
**<<<------"169665 - NMS - INI------>>>
*        SELECT *
*          FROM zib_nfe_dist_itm INTO TABLE @DATA(lit_zib_nfe_dist_itm)
*           FOR ALL ENTRIES IN @e_notas_venda
*         WHERE chave_nfe EQ @e_notas_venda-chave_nfe.
*
*        SELECT *
*          FROM zib_nfe_dist_ter INTO TABLE @DATA(lit_zib_nfe_dist_ter)
*           FOR ALL ENTRIES IN @e_notas_venda
*        WHERE chave_nfe EQ @e_notas_venda-chave_nfe.
**<<<------"169665 - NMS - FIM------>>>
        LOOP AT e_notas_venda ASSIGNING FIELD-SYMBOL(<fs_nota_venda>).

          READ TABLE lit_zib_nfe_dist_ter INTO DATA(lwa_zib_nfe_dist_ter) WITH KEY chave_nfe = <fs_nota_venda>-chave_nfe.
          CHECK sy-subrc EQ 0.

          LOOP AT lit_zib_nfe_dist_itm INTO DATA(lwa_zib_nfe_dist_itm) WHERE chave_nfe = <fs_nota_venda>-chave_nfe.

            APPEND INITIAL LINE TO e_notas_conferencia ASSIGNING FIELD-SYMBOL(<fs_nota_conf>).

            <fs_nota_conf>-chave_nfe       = lwa_zib_nfe_dist_ter-chave_nfe.
            <fs_nota_conf>-numero_nfe      = lwa_zib_nfe_dist_ter-numero.
            <fs_nota_conf>-ck_fiscal       = lwa_zib_nfe_dist_ter-ck_fiscal.
            <fs_nota_conf>-prod_item       = lwa_zib_nfe_dist_itm-prod_item.
            <fs_nota_conf>-codigo_produto  = lwa_zib_nfe_dist_itm-prod_codigo.
            <fs_nota_conf>-desc_produto    = lwa_zib_nfe_dist_itm-prod_descricao.
            <fs_nota_conf>-cod_controle    = lwa_zib_nfe_dist_itm-prod_ncm.
            <fs_nota_conf>-quantidade      = lwa_zib_nfe_dist_itm-prod_qtd_comerci.
            <fs_nota_conf>-unidade         = lwa_zib_nfe_dist_itm-prod_und_comerci.
            <fs_nota_conf>-preco_liquido   = lwa_zib_nfe_dist_itm-prod_vlr_und_com.
            <fs_nota_conf>-montante        = lwa_zib_nfe_dist_itm-prod_vlr_total_b.
            <fs_nota_conf>-icms            = lwa_zib_nfe_dist_itm-icms_base.

            CLEAR: lv_qtd_conf.
            LOOP AT e_dados_conferencia ASSIGNING FIELD-SYMBOL(<fs_conferencia>) WHERE chave_nfe  = lwa_zib_nfe_dist_itm-chave_nfe
                                                                                   AND prod_item  = lwa_zib_nfe_dist_itm-prod_item.

              <fs_nota_conf>-peso_conv  = <fs_conferencia>-peso_conv_nfe.
              "<fs_nota_conf>-saldo_conf = <fs_conferencia>-qtd_conv_nfe.

              ADD <fs_conferencia>-quantidade TO lv_qtd_conf.

            ENDLOOP.

            IF <fs_nota_conf>-peso_conv > 0.
              <fs_nota_conf>-qtd_conv   = <fs_nota_conf>-quantidade / <fs_nota_conf>-peso_conv.
              <fs_nota_conf>-saldo_conf = <fs_nota_conf>-qtd_conv.
            ENDIF.

            IF <fs_nota_conf>-saldo_conf > 0.
              <fs_nota_conf>-saldo_conf = <fs_nota_conf>-saldo_conf - lv_qtd_conf.
            ELSE.
              <fs_nota_conf>-saldo_conf = <fs_nota_conf>-qtd_conv.
            ENDIF.

          ENDLOOP.

        ENDLOOP.

      ENDIF.

    ENDIF.

    "Lotes da Carga
    LOOP AT lit_zsdt0134 ASSIGNING FIELD-SYMBOL(<fs_zsdt0134>).

      READ TABLE lit_zsdt0129 INTO lwa_zsdt0129 WITH KEY nro_cg = <fs_zsdt0134>-nro_cg.
      CHECK sy-subrc EQ 0.

      READ TABLE lit_zsdt0131 WITH KEY nro_lote = lwa_zsdt0129-nro_lote
                                       vbeln    = <fs_zsdt0134>-vbeln
                                       posnr    = <fs_zsdt0134>-posnr TRANSPORTING NO FIELDS.
      CHECK sy-subrc EQ 0.

      APPEND INITIAL LINE TO e_lotes ASSIGNING FIELD-SYMBOL(<fs_zsdt0134_out>).
      MOVE-CORRESPONDING <fs_zsdt0134> TO <fs_zsdt0134_out>.
    ENDLOOP.

    "Romaneios Carga
    LOOP AT lit_zsdt0001 ASSIGNING <fs_zsdt0001>.

      READ TABLE lit_zsdt0133 INTO DATA(lwa_zsdt0133) WITH KEY nro_cg = <fs_zsdt0001>-nro_cg.
      CHECK sy-subrc EQ 0.

      READ TABLE lit_zsdt0129 INTO lwa_zsdt0129 WITH KEY nro_cg  = lwa_zsdt0133-nro_cg.
      CHECK sy-subrc EQ 0.

      READ TABLE lit_zsdt0131 INTO DATA(lwa_zsdt0131) WITH KEY vbeln    = <fs_zsdt0001>-vbeln
                                                               nro_lote = lwa_zsdt0129-nro_lote.
      CHECK sy-subrc EQ 0.

      APPEND INITIAL LINE TO e_romaneios ASSIGNING FIELD-SYMBOL(<fs_zsdt0001_out>).
      MOVE-CORRESPONDING <fs_zsdt0001> TO <fs_zsdt0001_out>.

      IF <fs_zsdt0001_out>-st_proc EQ '99'.
        <fs_zsdt0001_out>-icon_status = '@01@'.
      ENDIF.

      READ TABLE lit_active INTO DATA(lwa_active) WITH KEY docnum = <fs_zsdt0001>-nro_nf_prod.
      IF sy-subrc EQ 0.
        READ TABLE lit_zsdt0375 INTO DATA(lwa_zsdt0375) WITH KEY chave_nfe = lwa_active-chave_nfe.
        IF sy-subrc EQ 0.
          <fs_zsdt0001_out>-dt_saida_cd = lwa_zsdt0375-data_saida.
          <fs_zsdt0001_out>-hr_saida_cd = lwa_zsdt0375-hora_saida.
        ENDIF.
      ENDIF.

      LOOP AT lit_zsdt0001_item INTO DATA(lwa_zsdt0001_item) WHERE ch_referencia = <fs_zsdt0001>-ch_referencia.
        APPEND INITIAL LINE TO <fs_zsdt0001_out>-itens ASSIGNING FIELD-SYMBOL(<fs_item_romaneio>).
        MOVE-CORRESPONDING lwa_zsdt0001_item TO <fs_item_romaneio>.
      ENDLOOP.

    ENDLOOP.

*    IF i_set_status_carga EQ abap_true.
*      zcl_carga_saida_insumos=>set_status_carga( CHANGING i_zsdt0133 = lit_zsdt0133 ).
*    ENDIF.

    DATA(lit_status_carga) = zcl_carga_saida_insumos=>lista_status_carga( EXPORTING i_show_status = abap_false ).

    IF lit_zsdt0133[] IS NOT INITIAL.
      SELECT *
        FROM zi_sd_st_cg_sai_insumos INTO TABLE @DATA(lit_st_carga)
         FOR ALL ENTRIES IN @lit_zsdt0133
       WHERE nro_cg EQ @lit_zsdt0133-nro_cg.

      SORT lit_st_carga BY nro_cg.

      LOOP AT lit_zsdt0133 ASSIGNING FIELD-SYMBOL(<fs_zsdt0133>).
        READ TABLE lit_st_carga ASSIGNING FIELD-SYMBOL(<fs_st_carga>) WITH KEY nro_cg = <fs_zsdt0133>-nro_cg BINARY SEARCH.
        CHECK sy-subrc EQ 0.

        <fs_zsdt0133>-status = <fs_st_carga>-status.
      ENDLOOP.
    ENDIF.

    "Cabeçalho Carga
    LOOP AT lit_zsdt0133 INTO DATA(lwa_carga).

      READ TABLE lit_zsdt0129 INTO lwa_zsdt0129 WITH KEY nro_cg = lwa_carga-nro_cg.

      CHECK sy-subrc EQ 0.

      APPEND INITIAL LINE TO e_cargas ASSIGNING FIELD-SYMBOL(<fs_carga>).

      MOVE-CORRESPONDING lwa_carga TO <fs_carga>.

      CASE <fs_carga>-frete_por.
        WHEN '1'.  "Tonelada
          <fs_carga>-frete_por_t = abap_true.
        WHEN '2'.  "Viagem
          <fs_carga>-frete_por_v = abap_true.
        WHEN OTHERS.
          <fs_carga>-frete_por_v = abap_true.
      ENDCASE.

      READ TABLE lit_status_carga INTO DATA(lwa_status_carga) WITH KEY status = lwa_carga-status.
      IF sy-subrc EQ 0.
        <fs_carga>-icon_status = lwa_status_carga-id.
        <fs_carga>-ds_status   = lwa_status_carga-desc.
      ENDIF.

      READ TABLE lit_zlest0181 INTO DATA(lwa_zlest0181) WITH KEY id_lote_frete = lwa_carga-id_lote_frete.
      IF sy-subrc EQ 0 AND lwa_zlest0181-id_carguero IS NOT INITIAL AND lwa_carga-id_lote_frete IS NOT INITIAL.
        <fs_carga>-integrado_carguero = icon_led_green.
      ELSE.
        <fs_carga>-integrado_carguero = icon_led_yellow.
      ENDIF.


      READ TABLE lit_lfa1 INTO DATA(lwa_lfa1) WITH KEY lifnr = lwa_carga-cod_transportadora.
      IF sy-subrc IS INITIAL.
        <fs_carga>-desc_transportadora = lwa_lfa1-name1.
        <fs_carga>-cnpj_transportadora = lwa_lfa1-stcd1.
      ENDIF.

      <fs_carga>-dt_entrega  = lwa_zsdt0129-dt_entrega.
      <fs_carga>-inco1       = lwa_zsdt0129-inco1.
      <fs_carga>-placa_cav   = lwa_zsdt0129-placa_cav.
      <fs_carga>-placa_car1  = lwa_zsdt0129-placa_car1.
      <fs_carga>-placa_car2  = lwa_zsdt0129-placa_car2.
      <fs_carga>-placa_car3  = lwa_zsdt0129-placa_car3.
      <fs_carga>-motorista   = lwa_zsdt0129-motorista.
      <fs_carga>-nro_lote    = lwa_zsdt0129-nro_lote.
      READ TABLE lit_lfa1 INTO lwa_lfa1 WITH KEY lifnr = <fs_carga>-motorista.
      IF sy-subrc IS INITIAL.
        <fs_carga>-nome_motorista = lwa_lfa1-name1.
      ENDIF.


      LOOP AT e_solicitacoes INTO DATA(lwa_sol) WHERE nro_lote EQ lwa_zsdt0129-nro_lote.
        ADD lwa_sol-vlr_solicitacao TO <fs_carga>-vlr_carga.
      ENDLOOP.

      "Dados Ponto Coleta
      READ TABLE lit_zsdt0131 INTO lwa_zsdt0131 WITH KEY nro_lote = lwa_zsdt0129-nro_lote.
      IF sy-subrc EQ 0.
        READ TABLE it_zsdt0132 INTO DATA(lwa_zsdt0132) WITH KEY nr_rot = lwa_zsdt0131-cod_loc_emb.
        IF sy-subrc EQ 0.
          <fs_carga>-codigo_pc        = lwa_zsdt0132-lifnr.
          READ TABLE it_lfa1 INTO lwa_lfa1 WITH KEY lifnr = lwa_zsdt0132-lifnr.
          IF sy-subrc EQ 0.
            <fs_carga>-descricao_pc = lwa_lfa1-name1.
          ENDIF.

          <fs_carga>-roteiro_pc  = lwa_zsdt0132-nr_rot.
          <fs_carga>-rot_desc = lwa_zsdt0132-rot_desc.
        ENDIF.
        <fs_carga>-local_embarq = lwa_zsdt0131-local_embarq.   "<<<------"169665 - NMS ------->>>
      ENDIF.

      zcl_carga_saida_insumos=>calc_categoria_veiculo(
        EXPORTING
          i_quantidade_kg = CONV #( <fs_carga>-qtd_total_kg )
        CHANGING
          c_ctg_transp    = <fs_carga>-ctg_transp
          c_ctg_transp_d  = <fs_carga>-ctg_transp_d  ).

      "FF #169665 - inicio
* Verifica o Frete é por Tonelada ou Viagem.
      CASE abap_on.
        WHEN <fs_carga>-frete_por_t.
          <fs_carga>-ds_frete_por = 'Tonelada'.
        WHEN <fs_carga>-frete_por_v.
          <fs_carga>-ds_frete_por = 'Viagem'.
      ENDCASE.


      IF lwa_carga-integrar_carguero IS NOT INITIAL.
        <fs_carga>-dt_envio_cotacao = lwa_carga-data_atual.
      ELSE.
        <fs_carga>-dt_envio_cotacao = lwa_carga-dt_envio_cotacao.
      ENDIF.

      <fs_carga>-dt_frete_contratado = lwa_carga-dt_frete_contratado.
      <fs_carga>-dt_autorizacao_embarque = lwa_carga-dt_autorizacao_embarque.

      READ TABLE lit_zsdt0410 INTO DATA(wa_410_transf) WITH KEY processo = '1'
                                                         nro_cg = lwa_carga-nro_cg.

      IF sy-subrc = 0.
        <fs_carga>-dt_troca_nf_fonec = wa_410_transf-date_create.
      ELSE.
        CLEAR <fs_carga>-dt_troca_nf_fonec.
      ENDIF.

      READ TABLE lit_zsdt0410 INTO DATA(wa_410_venda) WITH KEY processo = '2'
                                                             nro_cg = lwa_carga-nro_cg.
      IF sy-subrc = 0.

        READ TABLE lit_zib_nfe_dist_ter INTO DATA(wa_nfe) WITH KEY chave_nfe = wa_410_venda-chave_nfe.
        IF sy-subrc = 0.

          <fs_carga>-dt_carrega_fornec = wa_nfe-dt_emissao.

        ELSE.
          CLEAR <fs_carga>-dt_carrega_fornec.
        ENDIF.

      ELSE.
        CLEAR <fs_carga>-dt_carrega_fornec.
      ENDIF.

      READ TABLE e_bordero INTO DATA(wa_bordero) WITH KEY id_autorizacao_embarque = lwa_carga-nro_cg.
      IF sy-subrc = 0.
        <fs_carga>-dt_carrega_cd = wa_bordero-date_create.
      ELSE.
        CLEAR <fs_carga>-dt_carrega_cd.
      ENDIF.
**<<<------"169665 - NMS - INI------>>>
      IF <fs_carga>-status = '6' OR
         <fs_carga>-status = '7'.
        <fs_carga>-status_entrega = 'Entregue'.

      ELSE.
**<<<------"169665 - NMS - FIM------>>>
        READ TABLE lit_zsdt0129 INTO DATA(wa_129) WITH KEY nro_cg = lwa_carga-nro_cg.
        IF sy-subrc = 0.

          IF wa_129-dt_entrega < sy-datum.
            <fs_carga>-status_entrega = 'Atraso'.
          ELSEIF wa_129-dt_entrega = sy-datum.
            <fs_carga>-status_entrega = 'Para hoje'.
          ELSEIF wa_129-dt_entrega > sy-datum.
            <fs_carga>-status_entrega = 'No prazo'.
          ENDIF.

          LOOP AT lit_zsdt0131 INTO DATA(wa_131) WHERE nro_lote = wa_129-nro_lote.

            <fs_carga>-qtd_bags = <fs_carga>-qtd_bags + wa_131-qtd_vinc.

          ENDLOOP.

        ELSE.
          CLEAR <fs_carga>-status_entrega.
        ENDIF.
**<<<------"169665 - NMS - INI------>>>
      ENDIF.
**<<<------"169665 - NMS - FIM------>>>
      "FF #169665 - fim


    ENDLOOP.

    "Complementa Dados Conferencia
    CLEAR: lt_celltab.
    ls_celltab-fieldname = 'QUANTIDADE'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO lt_celltab INDEX 1.

    LOOP AT e_dados_conferencia ASSIGNING <fs_conferencia>.

      READ TABLE e_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_sol_carga>)
       WITH KEY nro_sol   = <fs_conferencia>-nro_sol
                seq       = <fs_conferencia>-seq.

      IF sy-subrc IS INITIAL.
        <fs_conferencia>-cod_prod_ov  = <fs_sol_carga>-matnr.
        <fs_conferencia>-desc_prod_ov = <fs_sol_carga>-maktx.
        <fs_conferencia>-unidade_ov   = <fs_sol_carga>-meins.
      ENDIF.

      READ TABLE e_notas_conferencia ASSIGNING FIELD-SYMBOL(<fs_nfe>)
      WITH KEY chave_nfe = <fs_conferencia>-chave_nfe
               prod_item = <fs_conferencia>-prod_item.

      IF sy-subrc IS INITIAL.
        <fs_conferencia>-cod_prod_nfe  = <fs_nfe>-codigo_produto.
        <fs_conferencia>-desc_prod_nfe = <fs_nfe>-desc_produto.
        <fs_conferencia>-unidade_nfe   = <fs_nfe>-unidade.
        <fs_conferencia>-numero_nfe    = <fs_nfe>-numero_nfe.

        IF <fs_nfe>-ck_fiscal IS NOT INITIAL.
          <fs_conferencia>-celltab = lt_celltab.
        ENDIF.
      ENDIF.

      READ TABLE e_bordero ASSIGNING <fs_bordero>
      WITH KEY id_autorizacao_embarque = <fs_conferencia>-nro_cg
               id_item                 = <fs_conferencia>-id_item_carregamento
               lote                    = <fs_conferencia>-lote_carregamento.
      IF sy-subrc IS INITIAL.
        <fs_conferencia>-cod_prod_bordero   = <fs_bordero>-matnr.
        <fs_conferencia>-desc_prod_bordero  = <fs_bordero>-ds_produto.
        <fs_conferencia>-lote_bordero       = <fs_bordero>-lote.
        <fs_conferencia>-embalagem          = <fs_bordero>-embalagem.
      ENDIF.

    ENDLOOP.

    "Totalizadores
    DATA(lit_solicitacoes_aux) = e_solicitacoes.

    LOOP AT e_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao>).

      CLEAR: <fs_solicitacao>-tot_vinc_ov,
             <fs_solicitacao>-tot_vinc_ov_kg,
             <fs_solicitacao>-qtde_lote_vinc_ov.

      LOOP AT lit_solicitacoes_aux INTO DATA(lwa_solicitacao_aux) WHERE vbeln  EQ <fs_solicitacao>-vbeln
                                                                    AND posnr  EQ <fs_solicitacao>-posnr
                                                                    AND nr_rot EQ <fs_solicitacao>-nr_rot.
        ADD lwa_solicitacao_aux-qtd_vinc    TO <fs_solicitacao>-tot_vinc_ov.
        ADD lwa_solicitacao_aux-qtd_vinc_kg TO <fs_solicitacao>-tot_vinc_ov_kg.
      ENDLOOP.

      LOOP AT e_lotes INTO DATA(lwa_zsdt0134) WHERE vbeln  EQ <fs_solicitacao>-vbeln
                                                AND posnr  EQ <fs_solicitacao>-posnr
                                                AND nr_rot EQ <fs_solicitacao>-nr_rot.
        ADD lwa_zsdt0134-lfimg TO <fs_solicitacao>-qtde_lote_vinc_ov.
      ENDLOOP.

    ENDLOOP.

    IF i_nr_carga_single IS NOT INITIAL AND e_romaneios[] IS NOT INITIAL. "Rateio Preço Frete para os romaneios

      SORT e_romaneios BY nr_romaneio.
      READ TABLE lit_zsdt0133 INTO lwa_zsdt0133 INDEX 1.
      IF lwa_zsdt0133-frete_por = '2' AND lwa_zsdt0133-viagem_id IS NOT INITIAL.

        lva_preco_frete_total = lwa_zsdt0133-preco_frete.

        IF lva_preco_frete_total IS NOT INITIAL.

          DATA(_qtde_romaneios) = lines( e_romaneios ).

          lva_preco_frete_rom = lva_preco_frete_total / _qtde_romaneios.

          CLEAR: lva_total_frete_rat.
          LOOP AT e_romaneios ASSIGNING FIELD-SYMBOL(<fs_rom>).
            <fs_rom>-preco_zfre = lva_preco_frete_rom.
            ADD lva_preco_frete_rom TO lva_total_frete_rat.
          ENDLOOP.

          lva_dif_rat = lva_preco_frete_total - lva_total_frete_rat.
          IF lva_dif_rat NE 0.
            READ TABLE e_romaneios ASSIGNING FIELD-SYMBOL(<fs_romaneio_rat>) INDEX 1.
            IF sy-subrc EQ 0.
              ADD lva_dif_rat TO <fs_romaneio_rat>-preco_zfre.
            ENDIF.
          ENDIF.

          "Rateio Itens dos romaneios
          LOOP AT e_romaneios ASSIGNING FIELD-SYMBOL(<fs_romaneio>).

            lva_preco_frete_total = <fs_romaneio>-preco_zfre.

            CHECK lva_preco_frete_total IS NOT INITIAL.

            SORT <fs_romaneio>-itens BY cd_item.

            DATA(_qtde_itens) = lines( <fs_romaneio>-itens ).

            lva_preco_frete_itm = lva_preco_frete_total / _qtde_itens.

            CLEAR: lva_total_frete_rat.
            LOOP AT <fs_romaneio>-itens ASSIGNING FIELD-SYMBOL(<fs_rom_item>).
              <fs_rom_item>-preco_zfre = lva_preco_frete_itm.
              ADD lva_preco_frete_itm TO lva_total_frete_rat.
            ENDLOOP.

            lva_dif_rat = lva_preco_frete_total - lva_total_frete_rat.
            IF lva_dif_rat NE 0.
              READ TABLE <fs_romaneio>-itens ASSIGNING FIELD-SYMBOL(<fs_romaneio_rat_itm>) INDEX 1.
              IF sy-subrc EQ 0.
                ADD lva_dif_rat TO <fs_romaneio_rat_itm>-preco_zfre.
              ENDIF.
            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.

    e_ov_lotes = e_solicitacoes[].

    SORT e_ov_lotes BY vbeln posnr nr_rot.
    DELETE ADJACENT DUPLICATES FROM e_ov_lotes COMPARING vbeln posnr nr_rot.

  ENDMETHOD.


  METHOD busca_dados_montar_carga.

    DATA: lit_solicitacoes_lib TYPE zsds384_t.
    DATA: lra_carga_automatica TYPE RANGE OF zsdt0133-carga_automatica.
    DATA: lra_origem_estoque TYPE RANGE OF zsdt0082-origem_estoque.
    DATA: lra_spart          TYPE RANGE OF zsdt0131-spart.
    DATA: lra_inco1          TYPE RANGE OF vbkd-inco1.

    CLEAR: e_tabela_monta_carga[].

    CASE i_tipo_saldo.
      WHEN 'A'. "Carga Automatica
        APPEND VALUE #( sign = 'I' option = 'EQ' low = abap_true ) TO lra_carga_automatica.
      WHEN 'M'. "Manual
        APPEND VALUE #( sign = 'I' option = 'EQ' low = abap_false ) TO lra_carga_automatica.
      WHEN 'T'. "Todos
        APPEND VALUE #( sign = 'I' option = 'EQ' low = abap_true ) TO lra_carga_automatica.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = abap_false ) TO lra_carga_automatica.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    IF i_origem_estoque IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_origem_estoque ) TO lra_origem_estoque.
    ENDIF.

    IF i_spart IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_spart ) TO lra_spart.
    ENDIF.

    IF i_inco1 IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_inco1 ) TO lra_inco1.
    ENDIF.

    SELECT zsdt0082~nro_sol
           zsdt0082~seq
           zsdt0082~vbeln
           zsdt0082~posnr
           zsdt0082~seq_lib  INTO CORRESPONDING FIELDS OF TABLE lit_solicitacoes_lib
      FROM zsdt0082 INNER JOIN vbap ON zsdt0082~vbeln = vbap~vbeln AND
                                       zsdt0082~posnr = vbap~posnr
                    INNER JOIN mara ON mara~matnr = vbap~matnr
                    INNER JOIN vbak ON vbak~vbeln = vbap~vbeln
     WHERE zsdt0082~vkbur      IN i_vkbur
       AND zsdt0082~vkorg      IN i_vkorg
       AND zsdt0082~spart      IN lra_spart
       AND vbak~kunnr          IN i_kunnr
       AND zsdt0082~nro_sol    IN i_nro_sol
       AND zsdt0082~nr_rot_pc  IN i_roteiro_pc
       "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New --->>>
       AND zsdt0082~origem_estoque  IN lra_origem_estoque
       "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New <<----
       AND zsdt0082~dt_liber   IN i_dt_lib_sol
       AND zsdt0082~vbeln      IN i_ordem_venda
       AND zsdt0082~seq        NE 1
       AND zsdt0082~carga_automatica IN lra_carga_automatica
       AND ( zsdt0082~status EQ 2 OR
             zsdt0082~status EQ 5 ).

    CHECK lit_solicitacoes_lib[] IS NOT INITIAL.

    "Recuperar Dados Complementares Solicitação
    zcl_carga_saida_insumos=>get_dados_solitacoes(
      EXPORTING
        i_solicitacoes = lit_solicitacoes_lib
        i_nro_cg_no_check_saldo = i_nro_cg_no_check_saldo
      RECEIVING
        r_solicitacoes = DATA(lit_dados_comp_sol)
    ).

    LOOP AT lit_solicitacoes_lib INTO DATA(lwa_solicitacao_lib).

      READ TABLE lit_dados_comp_sol INTO DATA(lwa_dados_comp_sol) WITH KEY nro_sol = lwa_solicitacao_lib-nro_sol
                                                                           seq     = lwa_solicitacao_lib-seq
                                                                           vbeln   = lwa_solicitacao_lib-vbeln
                                                                           posnr   = lwa_solicitacao_lib-posnr.
      CHECK sy-subrc EQ 0.

      CHECK lwa_dados_comp_sol-inco1 IN lra_inco1.

      CHECK lwa_dados_comp_sol-saldo GT 0.

      APPEND INITIAL LINE TO e_tabela_monta_carga ASSIGNING FIELD-SYMBOL(<fs_saida_solicitacao>).

      MOVE-CORRESPONDING lwa_dados_comp_sol TO <fs_saida_solicitacao>.

      <fs_saida_solicitacao>-qtd_vinc     = <fs_saida_solicitacao>-saldo.
      <fs_saida_solicitacao>-qtd_vinc_kg  = <fs_saida_solicitacao>-saldo * <fs_saida_solicitacao>-brgew.

    ENDLOOP.

  ENDMETHOD.


  METHOD calc_categoria_veiculo.

    CLEAR: c_ctg_transp, c_ctg_transp_d.

    IF i_quantidade_kg GT 0 AND i_quantidade_kg LE 23000.
      c_ctg_transp_d = 'AVULSO'.
      c_ctg_transp = 'A'.
    ELSEIF i_quantidade_kg GT 23000 AND i_quantidade_kg LE 27000.
      c_ctg_transp_d = 'VEÍCULO 27'.
      c_ctg_transp = 'B'.
    ELSEIF i_quantidade_kg GT 27000 AND i_quantidade_kg LE 32000.
      c_ctg_transp_d = 'VEÍCULO 32'.
      c_ctg_transp = 'C'.
    ELSEIF i_quantidade_kg GT 32000 AND i_quantidade_kg LE 37000.
      c_ctg_transp_d = 'VEÍCULO 37'.
      c_ctg_transp = 'D'.
    ELSEIF i_quantidade_kg GT 37000 AND i_quantidade_kg LE 50000.
      c_ctg_transp_d = 'VEÍCULO 50'.
      c_ctg_transp = 'E'.
    ELSE.
      c_ctg_transp_d = '----------'.
    ENDIF.

  ENDMETHOD.


  METHOD cancela_carga.

    DATA: tl_texto TYPE catsxt_longtext_itab.

    DATA: var_answer.

    DATA: lva_msg TYPE string.

    DATA: lva_motivo TYPE string.

    DATA: lv_erro TYPE c.

    CLEAR: e_msg_erro.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single =  i_nro_carga
      IMPORTING
        e_cargas          =  DATA(lit_carga)
        e_romaneios       =  DATA(lit_romaneios)
        e_lotes           =  DATA(lit_lotes)
    ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      e_msg_erro = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_carga-status = 'X'.
      e_msg_erro = |Carga já foi cancelada!|.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS NOT INITIAL .
      e_msg_erro = |Carga com romaneios gerados! Operação não permitida!|.
      RETURN.
    ENDIF.

    CASE i_origem_cancelamento.
      WHEN 'S'. "Cancelamento pelo Sistema Safra Control

        IF lwa_carga-viagem_id IS NOT INITIAL.
          e_msg_erro = |Carga com viagem no Carguero! Operação não permitida!|.
          RETURN.
        ENDIF.

      WHEN OTHERS.

        IF lwa_carga-viagem_id IS NOT INITIAL OR lwa_carga-id_carga_safra_control IS NOT INITIAL.

          IF lwa_carga-viagem_id IS NOT INITIAL .
            lva_msg = 'Carga com Viagem no Carguero! Deseja realmente cancelar a mesma?'.
          ELSE.
            lva_msg = 'Carga com Origem no Safra Control! Deseja realmente cancelar a mesma?'.
          ENDIF.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Confirmação'
              text_question         = lva_msg
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              default_button        = '1'
              display_cancel_button = ''
            IMPORTING
              answer                = var_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.

          IF var_answer NE '1'.
            e_msg_erro = 'Operação abortada!'.
            RETURN.
          ENDIF.

          e_msg_erro = zcl_carga_saida_insumos=>check_permissao_carga(
              EXPORTING
                 i_nro_carga = lwa_carga-nro_cg
                 i_atividade = '10' ).

          CHECK e_msg_erro IS INITIAL.

        ELSE.
          e_msg_erro = zcl_carga_saida_insumos=>check_permissao_carga(
              EXPORTING
                 i_nro_carga = lwa_carga-nro_cg
                 i_atividade = '05' ).

          CHECK e_msg_erro IS INITIAL.
        ENDIF.

    ENDCASE.

    IF i_motivo IS NOT INITIAL.
      lva_motivo = i_motivo.
    ELSE.
      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title = 'Motivo do cancelamento'
        CHANGING
          ch_text  = tl_texto.

      LOOP AT tl_texto ASSIGNING FIELD-SYMBOL(<fs_texto>).
        lva_motivo = lva_motivo && <fs_texto>.
      ENDLOOP.
    ENDIF.

    IF lva_motivo IS INITIAL.
      e_msg_erro = 'Motivo cancelamento não informado!'.
      RETURN.
    ENDIF.

    e_msg_erro = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING
       i_nro_cg   = i_nro_carga
       i_bloqueio = abap_true ).

    IF e_msg_erro IS NOT INITIAL.
      MESSAGE e_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT *
      FROM zsdt0130 INTO TABLE @DATA(lit_zsdt130)
     WHERE nro_lote = @lwa_carga-nro_lote.

    SELECT *
      FROM zsdt0131
      INTO TABLE @DATA(lit_zsdt131)
      WHERE nro_lote = @lwa_carga-nro_lote.

    LOOP AT lit_zsdt130 ASSIGNING FIELD-SYMBOL(<fs_0130>).
      <fs_0130>-status      = 'X'.
      MODIFY zsdt0130 FROM <fs_0130>.
    ENDLOOP.

    LOOP AT lit_zsdt131 ASSIGNING FIELD-SYMBOL(<fs_0131>).

      <fs_0131>-status    = 'X'.
      <fs_0131>-user_canc = sy-uname.
      <fs_0131>-dt_canc   = sy-datum.
      <fs_0131>-hr_can    = sy-uzeit.

      SELECT SINGLE *
        FROM zsdt0082 INTO @DATA(lwa_zsdt0082)
      WHERE nro_sol EQ @<fs_0131>-nro_sol
        AND seq     EQ @<fs_0131>-seq
        AND vbeln   EQ @<fs_0131>-vbeln
        AND posnr   EQ @<fs_0131>-posnr.

      IF sy-subrc NE 0.
        ROLLBACK WORK.
        e_msg_erro = |Solicitação { <fs_0131>-nro_sol } Seq. { <fs_0131>-seq } não encontrada!|.
        zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga i_bloqueio = abap_false ).
        RETURN.
      ENDIF.

*     lwa_zsdt0082-status = '2'.          "*-US190744-16.09.2025-#190744-JT-inicio

      MODIFY zsdt0131 FROM <fs_0131>.
*     MODIFY zsdt0082 FROM lwa_zsdt0082.  "*-US190744-16.09.2025-#190744-JT-inicio

*-US190744-16.09.2025-#190744-JT-inicio
      zcl_carga_saida_insumos=>consolida_status_solicitacao( i_nro_sol = <fs_0131>-nro_sol
                                                             i_seq     = <fs_0131>-seq
                                                             i_vbeln   = <fs_0131>-vbeln
                                                             i_posnr   = <fs_0131>-posnr ).
*-US190744-16.09.2025-#190744-JT-fim
    ENDLOOP.

    LOOP AT lit_lotes INTO DATA(lwa_lote).
      UPDATE zsdt0134 SET status    = 'X'
                          user_canc = sy-uname
                          dt_canc   = sy-datum
                          hr_can    = sy-uzeit
       WHERE vbeln    = lwa_lote-vbeln
         AND posnr    = lwa_lote-posnr
         AND charg    = lwa_lote-charg
         AND nro_cg   = lwa_lote-nro_cg
         AND nr_fase  = lwa_lote-nr_fase
         AND nr_rot   = lwa_lote-nr_rot.
    ENDLOOP.

    UPDATE zsdt0129 SET status    = 'X'
                        user_canc = sy-uname
                        dt_canc   = sy-datum
                        hr_can    = sy-uzeit
     WHERE nro_cg = lwa_carga-nro_cg.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      e_msg_erro = |Não foi possivel cancelar a Lote!|.
      zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga i_bloqueio = abap_false ).
      RETURN.
    ENDIF.

    UPDATE zsdt0133 SET status    = 'X'
                        user_canc = sy-uname
                        dt_canc   = sy-datum
                        hr_can    = sy-uzeit
     WHERE nro_cg = lwa_carga-nro_cg.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      e_msg_erro = |Não foi possivel cancelar a carga!|.
      zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga i_bloqueio = abap_false ).
      RETURN.
    ENDIF.

    IF lwa_carga-id_lote_frete IS NOT INITIAL.
      DATA(lva_msg_error_carguero) = zcl_carga_saida_insumos=>gerar_lote_embarcador_carguero( i_nro_cg = i_nro_carga ).

      IF lva_msg_error_carguero IS NOT INITIAL.
        ROLLBACK WORK.
        e_msg_erro = |Não foi possivel cancelar o Lote Embarcador Carguero! Motivo: { lva_msg_error_carguero } |.
        zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga i_bloqueio = abap_false ).
        RETURN.
      ENDIF.
    ENDIF.

    zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga i_bloqueio = abap_false ).

  ENDMETHOD.


  METHOD check_disparo_carguero.

    DATA: r_spart TYPE RANGE OF spart .

    CLEAR: r_ok.

    CHECK i_matkl IS NOT INITIAL.

    SELECT valsign   AS sign,
           valoption AS option,
           valfrom   AS low
      FROM setleaf INTO TABLE @r_spart
     WHERE setname EQ 'ENVIO_SPART_CARGUERO'.

    CHECK r_spart IS NOT INITIAL.

    SELECT COUNT(*)
       FROM zmmt0200
      WHERE matkl EQ i_matkl
        AND spart IN r_spart.

    CHECK sy-subrc IS INITIAL.

    r_ok = abap_on.


  ENDMETHOD.


  METHOD check_embarque_armazem.

    CLEAR: r_embarque_armazem.

    IF i_nro_cg IS NOT INITIAL.

      SELECT c~*
        FROM zsdt0129 AS a  INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
                            INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
        INTO TABLE @DATA(t_0132)
      WHERE a~nro_cg EQ @i_nro_cg.

    ELSEIF i_roteiro_pc IS NOT INITIAL.

      SELECT c~*
        FROM zsdt0132 AS c INTO TABLE @t_0132
       WHERE c~nr_rot EQ @i_roteiro_pc.

    ELSE.
      EXIT.
    ENDIF.

    IF line_exists( t_0132[ armazem = abap_true ] ).
      r_embarque_armazem = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_permissao_carga.

    CLEAR: r_msg_error.

    "Seleciona Cabeçalho Carga
    SELECT SINGLE *
      FROM zsdt0133 INTO @DATA(lw_0133)
      WHERE nro_cg EQ @i_nro_carga.

    CHECK sy-subrc EQ 0.

    "Seleciona Cabeçalho Lote
    SELECT SINGLE *
      FROM zsdt0129 INTO @DATA(lwa_0129)
     WHERE nro_cg EQ @i_nro_carga.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zsdt0131 INTO @DATA(lwa_zsdt0131)
     WHERE nro_lote EQ @lwa_0129-nro_lote.

    CHECK sy-subrc EQ 0.

    r_msg_error = zcl_carga_saida_insumos=>check_permissao_carga_core(
                   i_bukrs     = lwa_zsdt0131-vkorg
                   i_atividade = i_atividade
                   i_spart     = lwa_zsdt0131-spart ).


  ENDMETHOD.


  METHOD check_permissao_carga_core.

    DATA: lv_answer.

    CLEAR: r_msg_error.

    AUTHORITY-CHECK OBJECT 'ZSDT0112V3'
    ID 'ZSDATIVID'   FIELD i_atividade
    ID 'BUKRS'       FIELD i_bukrs
    ID 'SPART'       FIELD i_spart
    ID 'DIRECAO'     FIELD 'S'.

    IF sy-subrc IS NOT INITIAL.
      r_msg_error = |Sem autorização para essa ação!|.
      r_msg_error = |{ r_msg_error } Procure o departamento de Insumos Corporativo para solicitar o acesso|.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD check_reinicializacao_aut_emb.
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2LTT |27/06/2025 |Implementação Status de processamento &*
*&                                    |da Carga - INSUMOS. Chamado: 169508.  &*
*&---------------------------------------------------------------------------&*

    DATA: lv_answer.
    DATA: lv_dt_null TYPE erdat.

    CLEAR: r_msg_error.

    IF i_nro_carga IS INITIAL.
      r_msg_error = 'Não foi possivel localizar a Carga!'.
      RETURN.
    ENDIF.

    DATA(lra_nr_carga) = VALUE zsdt_range_nro_cg( ( sign = 'I' option = 'EQ' low = i_nro_carga ) ).

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga     = lra_nr_carga
      IMPORTING
        e_cargas       = DATA(lit_carga) ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      r_msg_error = 'Não foi possivel localizar a Carga!'.
      RETURN.
    ENDIF.

    CHECK lwa_carga-dt_autorizacao_embarque IS NOT INITIAL.

    IF i_background EQ abap_false.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = 'Autorização de embarque já existe, será necessário gerar nova autorização de embarque. Deseja continuar?'
          text_button_1         = 'Sim'
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = 'Não'
          icon_button_2         = 'ICON_INCOMPLETE'
          display_cancel_button = space
        IMPORTING
          answer                = lv_answer.

      IF lv_answer NE '1'.
        r_msg_error = 'Operação abortada!'.
        RETURN.
      ENDIF.

    ENDIF.

    UPDATE zsdt0133 SET dt_autorizacao_embarque       = lv_dt_null
                        pedido_luft_criado            = abap_false
                        int_transp_safra_ctrl         = abap_false
                        int_mot_safra_ctrl            = abap_false
                        int_dados_transp_safra_ctrl   = abap_false
                        nro_pedido_luft               = abap_false
                        int_anexos_aut_emb_safra_ctrl = abap_false
                        int_anexos_nfe_safra_ctrl     = abap_false
     WHERE nro_cg EQ i_nro_carga.

    UPDATE zsdt0129 SET int_veiculo_safra_ctrl = abap_false
     WHERE nro_cg EQ i_nro_carga.

    "Remove Informações Bordero Recebido para carga, pois será recebido um novo bordero para a nova autorização
    DELETE FROM zsdt0376 WHERE id_autorizacao_embarque EQ i_nro_carga.

    "Remove Informações Conferencia Carga
    DELETE FROM zsdt0420 WHERE nro_cg EQ i_nro_carga.

    "Remove Anexos
    DELETE FROM zsdt0422 WHERE nro_cg EQ i_nro_carga.

  ENDMETHOD.


  METHOD conferir_carga.

    DATA: lv_answer.

    CLEAR: r_msg_error.

    zcl_carga_saida_insumos=>busca_dados_carga(
     EXPORTING
       i_nr_carga_single =  i_nro_carga
     IMPORTING
       e_cargas          =  DATA(lit_carga)
       e_romaneios       =  DATA(lit_romaneios)
       e_lotes           =  DATA(lit_lotes)
       e_notas_venda     =  DATA(lit_notas_venda)
   ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_carga-dt_autorizacao_embarque IS INITIAL.
      r_msg_error = |Carga não possui autorização de embarque gerada!|.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS NOT INITIAL.
      r_msg_error = 'Carga com romaneios gerados! Operação não permitida!'.
      RETURN.
    ENDIF.

    r_msg_error = zcl_carga_saida_insumos=>check_permissao_carga(
        EXPORTING
           i_nro_carga = i_nro_carga
           i_atividade = '09' ).

    CHECK r_msg_error IS INITIAL.

    DATA(_conferencia_com_depara) = zcl_carga_saida_insumos=>check_conferencia_com_depara( i_nro_cg = i_nro_carga ).

    IF _conferencia_com_depara EQ abap_true.

      r_msg_error = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING
                                                                            i_nro_cg   = i_nro_carga
                                                                            i_bloqueio = abap_true ).

      IF r_msg_error IS NOT INITIAL.
        zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga i_bloqueio = abap_false ).
        RETURN.
      ENDIF.

      CALL FUNCTION 'ZSDF_START_CONF_CARGA_SAI_IN'
        EXPORTING
          i_nro_cg = i_nro_carga.

      zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga i_bloqueio = abap_false ).

    ELSE.

      IF lwa_carga-carga_conferida IS NOT INITIAL.
        r_msg_error = |Carga já foi conferida!|.
        RETURN.
      ENDIF.

      IF lit_lotes[] IS INITIAL.
        r_msg_error = 'Carga sem lotes preenchidos! Operação não permitida!'.
        RETURN.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Conferência'
          text_question         = 'Confirma a conferência dessa Carga?'
          text_button_1         = 'Sim'
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = 'Não'
          icon_button_2         = 'ICON_INCOMPLETE'
          display_cancel_button = space
        IMPORTING
          answer                = lv_answer.

      IF lv_answer <> '1'.
        RETURN.
      ENDIF.

      r_msg_error = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING
                                                                            i_nro_cg   = i_nro_carga
                                                                            i_bloqueio = abap_true ).

      IF r_msg_error IS NOT INITIAL.
        zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga i_bloqueio = abap_false ).
        RETURN.
      ENDIF.

      UPDATE zsdt0133 SET carga_conferida = abap_true
                          dt_conferencia  = sy-datum
                          hr_conferencia  = sy-uzeit
                          us_conferencia  = sy-uname
       WHERE nro_cg EQ i_nro_carga.

      zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga i_bloqueio = abap_false ).

      MESSAGE 'Carga conferida com sucesso!' TYPE 'S'.

    ENDIF.

  ENDMETHOD.


  METHOD envia_path_autoriza_embarque.

    CLEAR: r_msg_error.

    "Seleciona Cabeçalho Carga
    SELECT SINGLE *
      FROM zsdt0133 INTO @DATA(lwa_0133)
      WHERE nro_cg EQ @i_carga.

    IF sy-subrc NE 0.
      r_msg_error = |Carga { i_carga } não encontrada!|.
      RETURN.
    ENDIF.


    "Seleciona Viagem
    SELECT SINGLE *
      FROM zlest0185 INTO @DATA(lwa_0185)
     WHERE viagem_id EQ @i_viagem_id.

    IF sy-subrc NE 0 OR i_viagem_id IS INITIAL.
      r_msg_error = |Viagem { i_viagem_id } não encontrada!|.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zlest0181 INTO @DATA(lwa_0181)
      WHERE id_lote_frete EQ @lwa_0133-id_lote_frete.

    IF sy-subrc NE 0 OR lwa_0133-id_lote_frete IS INITIAL.
      r_msg_error = |Lote Embarcador { lwa_0133-id_lote_frete } não encontrada!|.
      RETURN.
    ENDIF.


    TRY .
* Gera a URL do Documento de Viagem.
        DATA(vl_url) = lwa_0185-blob_path && '/AutorizacaoEmbarque_' && i_carga && '.pdf'.

* Carega(autentica) a URL do Documento de Viagem.
        zcl_integracao_upload_auth=>zif_integracao_upload_auth~get_instance(
          )->set_empresa(       EXPORTING i_bukrs         = lwa_0181-bukrs
          )->set_id_referencia( EXPORTING i_id_referencia = lwa_0185-viagem_id
          )->set_blob_path(     EXPORTING i_blob_path     = CONV #( vl_url )
          )->get_json(          IMPORTING e_json          = DATA(vl_json_tn)
          )->set_ds_data(       EXPORTING i_json          = vl_json_tn
          )->set_ds_url(
          )->set_send_msg(      IMPORTING e_id_integracao = DATA(vl_id_integracao)
                                          e_url_upload    = DATA(vl_url_upload)
          ).

      CATCH zcx_integracao INTO DATA(ex_integracao_tn).
        r_msg_error = ex_integracao_tn->get_text( ).
        RETURN.
      CATCH zcx_error INTO DATA(ex_error_tn).
        r_msg_error = ex_error_tn->get_text( ).
        RETURN.
    ENDTRY.
* Gera o PDF em binário.

    IF i_pdf_xstring IS INITIAL.
      gerar_autorizacao_embarque_sm( EXPORTING i_carga  = i_carga
                                               i_binary = abap_on
                                     IMPORTING e_pdf_xstring = DATA(vl_pdf_bin)
                                    ).
    ELSE.
      vl_pdf_bin = i_pdf_xstring.
    ENDIF.

    IF vl_pdf_bin IS INITIAL.
      r_msg_error = 'Não foi possivel gerar o PDF da autorização de Embarque'.
      RETURN.
    ENDIF.

* Efetua upload no carguero
    TRY .
        zcl_integracao_upload_exec=>zif_integracao_upload_exec~get_instance(
          )->set_empresa(       EXPORTING i_bukrs         = lwa_0181-bukrs
          )->set_id_referencia( EXPORTING i_id_referencia = lwa_0185-viagem_id
          )->get_json(          EXPORTING i_file_bin      = vl_pdf_bin
                                IMPORTING e_json_xstring  = DATA(vl_json_tn_xstring)
          )->set_ds_url(        EXPORTING i_url_upload    = vl_url_upload
          )->set_ds_data(       EXPORTING i_json_xstring  = vl_json_tn_xstring
          )->set_send_msg(      IMPORTING e_id_integracao = vl_id_integracao
          ).

      CATCH zcx_integracao INTO DATA(ex_integracao_tn_ret).
        r_msg_error = ex_integracao_tn_ret->get_text( ).
        RETURN.

      CATCH zcx_error INTO DATA(ex_error_tn_ret).
        r_msg_error = ex_error_tn_ret->get_text( ).
        RETURN.

    ENDTRY.

    UPDATE zsdt0133 SET ds_url_file_carguero = vl_url
     WHERE nro_cg EQ i_carga.

    COMMIT WORK.


  ENDMETHOD.


  METHOD estornar_romaneios.

    DATA: zcl_romaneio TYPE REF TO zcl_romaneio.
    DATA: lva_dt_null TYPE erdat.

    CLEAR: r_msg_error.

    r_msg_error = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg   = i_nro_cg i_bloqueio = abap_true ).

    CHECK r_msg_error IS INITIAL.

    zcl_carga_saida_insumos=>busca_dados_carga(
        EXPORTING
          i_nr_carga_single = i_nro_cg
        IMPORTING
          e_cargas          = DATA(lit_cargas)
          e_solicitacoes    = DATA(lit_solicitacoes)
          e_lotes           = DATA(lit_lotes)
          e_romaneios       = DATA(lit_romaneios)
      ).

    LOOP AT lit_romaneios INTO DATA(lwa_romaneio).
      IF lwa_romaneio-doc_rem IS NOT INITIAL.
        r_msg_error = |Remessa já gerada para o romaneio { lwa_romaneio-nr_romaneio }!|.
        zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg   = i_nro_cg i_bloqueio = abap_false ).
        RETURN.
      ENDIF.
    ENDLOOP.

    CREATE OBJECT zcl_romaneio.
    DATA(_error_exclusao_rom) = abap_false.

    LOOP AT lit_romaneios INTO DATA(wa_zsdt0001).

      TRY.
          CALL METHOD zcl_romaneio->set_registro
            EXPORTING
              i_id_registro = wa_zsdt0001-ch_referencia.

          DATA(_excluiu) = zcl_romaneio->excluir_registro( ).

          IF _excluiu EQ abap_true.
            UPDATE zsdt0134 SET ch_referencia = space WHERE ch_referencia = wa_zsdt0001-ch_referencia.
          ELSE.
            _error_exclusao_rom = abap_true.
          ENDIF.

        CATCH zcx_cadastro INTO DATA(zcx_cadastro).
          _error_exclusao_rom = abap_true.
          zcx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

    ENDLOOP.

    IF _error_exclusao_rom EQ abap_false.
      zcl_carga_saida_insumos=>del_lote_preenchimento_auto( EXPORTING i_nro_cg   = i_nro_cg ).
    ENDIF.

    DATA(_conferencia_com_depara) = zcl_carga_saida_insumos=>check_conferencia_com_depara( i_nro_cg = i_nro_cg ).

    IF _conferencia_com_depara EQ abap_false.

      UPDATE zsdt0133 SET carga_conferida = abap_false
                          dt_conferencia  = '00000000'
                          hr_conferencia  = space
                          us_conferencia  = space
       WHERE nro_cg EQ i_nro_cg.

    ENDIF.

    zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg   = i_nro_cg i_bloqueio = abap_false ).

  ENDMETHOD.


  METHOD gerar_autorizacao_embarque.

    CLEAR: r_sucesso, e_msg_error.

    e_msg_error = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg   = i_nro_cg i_bloqueio = abap_true ).

    CHECK e_msg_error IS INITIAL.

    zcl_carga_saida_insumos=>gerar_autorizacao_emb_core(
      EXPORTING
        i_nro_cg                       = i_nro_cg
        i_viagem_id                    = i_viagem_id
        i_envia_autorizacao_carguero   = i_envia_autorizacao_carguero
        i_background                   = i_background
        i_with_lote                    = i_with_lote
      IMPORTING
        e_msg_error                    = e_msg_error
      RECEIVING
        r_sucesso                      = r_sucesso ).

    zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_cg i_bloqueio = abap_false ).


  ENDMETHOD.


  METHOD gerar_autorizacao_embarque_sm.

    TYPES: BEGIN OF ty_vbak,
             vbeln TYPE vbak-vbeln,
             posnr TYPE vbap-posnr,
             knumv TYPE vbak-knumv,
           END OF ty_vbak,

           BEGIN OF ty_seq,
             nr_cg TYPE zsdt0129-nro_cg,
             ini   TYPE n LENGTH 2,
             fim   TYPE n LENGTH 2,
           END OF ty_seq.

    DATA: it_zsdt0129 TYPE TABLE OF zsdt0129,
          it_zsdt0131 TYPE TABLE OF zsdt0131,
          it_zsdt0132 TYPE TABLE OF zsdt0132,
          it_zsdt0134 TYPE TABLE OF zsdt0134,
          it_kna1     TYPE TABLE OF kna1,
          it_adr6     TYPE TABLE OF adr6,
          it_vbak     TYPE TABLE OF ty_vbak,
          it_exc      TYPE TABLE OF zsdt0135,
          lva_nro_cg  TYPE zsdt0133-nro_cg.


    "Work area
    DATA: wa_top TYPE zembarquetopo,
          wa_tra TYPE zembarquetrans.

    "Estruturas e tabelas para o smartforms
    DATA: lv_fm_name     TYPE rs38l_fnam,
          ls_ctrl_param  TYPE ssfctrlop,
          ls_out_options TYPE ssfcompop,
          ls_job_info    TYPE ssfcrescl,
          lt_otf         TYPE STANDARD TABLE OF itcoo,
          lt_lines       TYPE STANDARD TABLE OF tline,
          lv_pdf_xstring TYPE xstring.

    DATA: lva_lifnr_filial TYPE lfa1-lifnr.

    DATA: r_seq    TYPE TABLE OF ty_seq,
          vl_kunnr TYPE kna1-kunnr.

    "Tabelas
    DATA:it_fat TYPE TABLE OF zembarquefat,
         it_pro TYPE TABLE OF zembarquepro,
         it_com TYPE TABLE OF zembarquecom.

    "Variáveis
    DATA: vl_formname TYPE tdsfname,
          vl_name     TYPE rs38l_fnam.


    CLEAR: e_pdf_xstring.

    lva_nro_cg = i_carga.

*   Cabeçario de Carga
    SELECT *
      FROM zsdt0133 INTO TABLE @DATA(it_zsdt0133)
     WHERE nro_cg EQ @lva_nro_cg.

    CHECK it_zsdt0133[] IS NOT INITIAL.

* Cabeçalho de Lote
    SELECT *
      FROM zsdt0129 INTO TABLE it_zsdt0129
       FOR ALL ENTRIES IN it_zsdt0133
    WHERE nro_cg EQ it_zsdt0133-nro_cg.

    CHECK it_zsdt0129[] IS NOT INITIAL.

* Cliente do Lote
    SELECT *
      FROM zsdt0130 INTO TABLE @DATA(it_zsdt0130)
       FOR ALL ENTRIES IN @it_zsdt0129
     WHERE nro_lote EQ @it_zsdt0129-nro_lote
        AND status  NE 'X'.

* Ordem do Lote
    SELECT *
      FROM zsdt0131 INTO TABLE it_zsdt0131
       FOR ALL ENTRIES IN it_zsdt0129
      WHERE nro_lote EQ it_zsdt0129-nro_lote
        AND status NE 'X'.

    IF it_zsdt0131[] IS NOT INITIAL.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>> Processo new

      "Cabeçalho do Pedido
      SELECT *
        FROM ekko INTO TABLE @DATA(it_ekko)
         FOR ALL ENTRIES IN @it_zsdt0131
        WHERE ebeln EQ @it_zsdt0131-ebeln.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<---- Processo new

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>> Processo Old
*      SELECT * FROM zsdt0062
*       INTO TABLE @DATA(it_zsdt0062)
*       FOR ALL ENTRIES IN @it_zsdt0131
*       WHERE nro_cg EQ @lva_nro_cg
*         AND vbeln  EQ @it_zsdt0131-vbeln
*         AND status EQ 'L'.
*
*      IF it_zsdt0062[] IS NOT INITIAL.
*        "Cabeçalho do Pedido
*        SELECT *
*          FROM ekko INTO TABLE @DATA(it_ekko)
*          FOR ALL ENTRIES IN @it_zsdt0062
*          WHERE ebeln EQ @it_zsdt0062-ebeln.
*      ENDIF.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<---- Processo Old

      SELECT *
        FROM zsdt0132 INTO TABLE it_zsdt0132
         FOR ALL ENTRIES IN it_zsdt0131
       WHERE nr_rot EQ it_zsdt0131-cod_loc_emb.

      SELECT *
        FROM zsdt0134 INTO CORRESPONDING FIELDS OF TABLE it_zsdt0134
         FOR ALL ENTRIES IN it_zsdt0131
        WHERE vbeln EQ it_zsdt0131-vbeln " PEDIDO
          AND posnr EQ it_zsdt0131-posnr " ITEM
          AND nro_cg EQ lva_nro_cg " Carga
          AND status NE 'X'.  " Somente não excluidos

    ENDIF.

    "Dados do Fornecedor
    IF it_ekko[] IS NOT INITIAL.
      SELECT * FROM lfa1
        INTO TABLE @DATA(it_lfa1)
        FOR  ALL ENTRIES IN @it_ekko
        WHERE lifnr EQ @it_ekko-lifnr.
    ENDIF.

    IF it_zsdt0132[] IS NOT INITIAL.
      SELECT * FROM lfa1
        APPENDING TABLE it_lfa1
        FOR ALL ENTRIES IN it_zsdt0132
        WHERE lifnr EQ it_zsdt0132-lifnr.
    ENDIF.

    IF it_zsdt0133[] IS NOT INITIAL.
      SELECT * FROM lfa1
        APPENDING TABLE it_lfa1
        FOR ALL ENTRIES IN it_zsdt0133
        WHERE lifnr EQ it_zsdt0133-cod_transportadora.
    ENDIF.

    IF it_zsdt0129[] IS NOT INITIAL.
      SELECT * FROM lfa1
        APPENDING TABLE it_lfa1
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE lifnr EQ it_zsdt0129-motorista.
    ENDIF.

    IF it_zsdt0131[] IS NOT INITIAL.
      SELECT * FROM kna1
        INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_zsdt0131
        WHERE kunnr EQ it_zsdt0131-kunnr.

      SELECT * FROM makt
        INTO TABLE @DATA(it_makt)
        FOR ALL ENTRIES IN @it_zsdt0131
        WHERE matnr EQ @it_zsdt0131-matnr.
    ENDIF.

    IF it_zsdt0129[] IS NOT INITIAL.
      SELECT zsdt0131~vbeln
             zsdt0131~posnr
             vbak~knumv
          FROM zsdt0131
          INNER JOIN vbak ON zsdt0131~vbeln = vbak~vbeln
          INTO CORRESPONDING FIELDS OF TABLE it_vbak
          FOR ALL ENTRIES IN it_zsdt0129
          WHERE nro_lote EQ it_zsdt0129-nro_lote
          AND status NE abap_true.

      IF it_vbak[] IS NOT INITIAL.
        SELECT FROM v_konv FIELDS *
          FOR ALL ENTRIES IN @it_vbak
          WHERE knumv EQ @it_vbak-knumv
            AND kposn EQ @it_vbak-posnr
            AND kschl EQ 'PR00' INTO TABLE @DATA(it_konv).
      ENDIF.
    ENDIF.

    IF it_kna1[] IS NOT INITIAL.
      SELECT *
        FROM adr6 INTO TABLE it_adr6
        FOR ALL ENTRIES IN it_kna1
        WHERE addrnumber EQ it_kna1-adrnr.

      SELECT *
        FROM adr2 INTO TABLE @DATA(it_adr2)
        FOR ALL ENTRIES IN @it_kna1
        WHERE addrnumber EQ @it_kna1-adrnr.
    ENDIF.

    DATA(it_0131) = it_zsdt0131[].

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
*    SORT it_0131 BY kunnr.
*    DELETE ADJACENT DUPLICATES FROM it_0131 COMPARING kunnr.
    SORT it_0131 BY werks.
    DELETE ADJACENT DUPLICATES FROM it_0131 COMPARING werks.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    LOOP AT it_0131 INTO DATA(wa_0131).

      APPEND INITIAL LINE TO it_fat ASSIGNING FIELD-SYMBOL(<fs_fat>).

      vl_kunnr = |{ wa_0131-werks ALPHA = IN }|.

      SELECT SINGLE *
        FROM kna1 INTO @DATA(wkna1)
       WHERE kunnr EQ @vl_kunnr.

      CHECK sy-subrc EQ 0.

      <fs_fat>-cliente  = wkna1-name1.
      <fs_fat>-endereco = wkna1-stras.
      <fs_fat>-cpf_cnpj = wkna1-stcd1.
      <fs_fat>-ins_est  = wkna1-stcd3.
      <fs_fat>-cidade   = wkna1-ort01.
      <fs_fat>-uf       = wkna1-regio.

      SELECT SINGLE smtp_addr
        FROM adr6 INTO <fs_fat>-email
      WHERE addrnumber EQ wkna1-adrnr.

      SELECT SINGLE tel_number
        FROM adr2 INTO <fs_fat>-contato
        WHERE addrnumber EQ wkna1-adrnr.

    ENDLOOP.

    SORT it_fat BY cpf_cnpj .
    DELETE ADJACENT DUPLICATES FROM it_fat COMPARING cpf_cnpj.

    LOOP AT it_zsdt0129 ASSIGNING FIELD-SYMBOL(<fs_zsdt0129>).

      wa_top-nro_cg     = <fs_zsdt0129>-nro_cg.
      wa_top-dt_emissao = sy-datum.
      wa_top-icon       = <fs_zsdt0129>-inco1.

      LOOP AT it_zsdt0131 INTO DATA(wa_zsdt0131) WHERE nro_lote EQ <fs_zsdt0129>-nro_lote.

        wa_top-spart = wa_zsdt0131-spart.

        CASE wa_top-spart.
          WHEN '04'.
            wa_top-desc_spart = 'SEMENTES'.
          WHEN '02'.
            wa_top-desc_spart = 'FERTILIZANTES'.
        ENDCASE.

        IF i_with_lote EQ abap_true.

          IF it_zsdt0134[] IS NOT INITIAL.

            LOOP AT it_zsdt0134 INTO DATA(wa_zsdt0134) WHERE vbeln EQ wa_zsdt0131-vbeln  " OV
                                                         AND posnr EQ wa_zsdt0131-posnr. " ITEM

              APPEND INITIAL LINE TO it_pro ASSIGNING FIELD-SYMBOL(<fs_pro>).
              <fs_pro>-nro_cg     = <fs_zsdt0129>-nro_cg.

              <fs_pro>-qtd_vinc   = wa_zsdt0134-lfimg.
              <fs_pro>-um         = wa_zsdt0131-um.
              <fs_pro>-lote     = wa_zsdt0134-charg.

              TRY .
                  DATA(wa_zsdt0132) = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
                  <fs_pro>-local      = wa_zsdt0132-rot_desc.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.

              TRY .
                  DATA(wa_zsdt0130) = it_zsdt0130[ nro_lote = <fs_zsdt0129>-nro_lote
                                                   nro_sol = wa_zsdt0131-nro_sol ].
                  <fs_pro>-seq_ent_cg = wa_zsdt0130-seq_ent_cg.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.

              TRY .
                  <fs_pro>-name1 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-name1.
                  <fs_pro>-mcod3 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-mcod3.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.

              TRY .
                  <fs_pro>-maktx = it_makt[ matnr = wa_zsdt0131-matnr ]-maktx.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.

              <fs_pro>-nr_rot = wa_zsdt0130-nr_rot.

              TRY .
                  DATA(wa_vbak) = it_vbak[ vbeln = wa_zsdt0131-vbeln
                                           posnr = wa_zsdt0131-posnr ].
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.

              TRY .
                  DATA(wa_konv) = it_konv[ knumv = wa_vbak-knumv
                                           kposn = wa_vbak-posnr ].
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.


              CASE wa_konv-kmein.
                WHEN 'TO'.
                  <fs_pro>-preco_frete = ( ( ( ( wa_zsdt0131-qtd_vinc * wa_zsdt0131-brgew ) * wa_konv-kbetr ) / 1000 ) * wa_konv-kkurs ).
                WHEN OTHERS.
                  <fs_pro>-preco_frete = ( ( wa_zsdt0131-qtd_vinc * wa_konv-kbetr ) * wa_konv-kkurs ).
              ENDCASE.

              APPEND INITIAL LINE TO r_seq ASSIGNING FIELD-SYMBOL(<fs_seq>).
              <fs_seq>-ini    = <fs_pro>-seq_ent_cg.
              <fs_seq>-nr_cg  = <fs_pro>-nro_cg.

            ENDLOOP.


          ELSE.


            APPEND INITIAL LINE TO it_pro ASSIGNING <fs_pro>.

            <fs_pro>-nro_cg     = <fs_zsdt0129>-nro_cg.

            <fs_pro>-qtd_vinc   = wa_zsdt0131-qtd_vinc.
            <fs_pro>-um         = wa_zsdt0131-um.

            <fs_pro>-lote     = ' '.

            "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>> Processo New
            select SINGLE *
              from zsdt0082 INTO @DATA(lwa_zsdt0082)
              WHERE nro_sol = @wa_zsdt0131-nro_sol
                and seq     = @wa_zsdt0131-seq
                and vbeln   = @wa_zsdt0131-vbeln
                and posnr   = @wa_zsdt0131-posnr.

            IF sy-subrc eq 0 AND lwa_zsdt0082-flexibilidade = '3'. "Manter Lote e Marca?
              <fs_pro>-lote     = lwa_zsdt0082-charg.
            ENDIF.
            "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

            TRY .
                wa_zsdt0132 = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
                <fs_pro>-local      = wa_zsdt0132-rot_desc.
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            TRY .
                wa_zsdt0130 = it_zsdt0130[ nro_lote = <fs_zsdt0129>-nro_lote
                                                 nro_sol = wa_zsdt0131-nro_sol ].
                <fs_pro>-seq_ent_cg = wa_zsdt0130-seq_ent_cg.
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            TRY .
                <fs_pro>-name1 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-name1.
                <fs_pro>-mcod3 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-mcod3.
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            TRY .
                <fs_pro>-maktx = it_makt[ matnr = wa_zsdt0131-matnr ]-maktx.
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            <fs_pro>-nr_rot = wa_zsdt0130-nr_rot.

            TRY .
                wa_vbak = it_vbak[ vbeln = wa_zsdt0131-vbeln
                                         posnr = wa_zsdt0131-posnr ].
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            TRY .
                wa_konv = it_konv[ knumv = wa_vbak-knumv
                                         kposn = wa_vbak-posnr ].
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.


            CASE wa_konv-kmein.
              WHEN 'TO'.
                <fs_pro>-preco_frete = ( ( ( ( wa_zsdt0131-qtd_vinc * wa_zsdt0131-brgew ) * wa_konv-kbetr ) / 1000 ) * wa_konv-kkurs ).
              WHEN OTHERS.
                <fs_pro>-preco_frete = ( ( wa_zsdt0131-qtd_vinc * wa_konv-kbetr ) * wa_konv-kkurs ).
            ENDCASE.

            APPEND INITIAL LINE TO r_seq ASSIGNING <fs_seq>.

            <fs_seq>-ini    = <fs_pro>-seq_ent_cg.
            <fs_seq>-nr_cg  = <fs_pro>-nro_cg.

          ENDIF.

          TRY.
              DATA(wa_0132) = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          APPEND INITIAL LINE TO it_com ASSIGNING FIELD-SYMBOL(<fs_com>).
          <fs_com>-local_embarque = wa_0132-rot_desc.
          <fs_com>-municipio = wa_0132-city1.
          <fs_com>-endereco = it_lfa1[ lifnr = wa_0132-lifnr ]-stras.
          <fs_com>-uf = wa_0132-uf.

        ELSE.

          APPEND INITIAL LINE TO it_pro ASSIGNING <fs_pro>.

          <fs_pro>-nro_cg     = <fs_zsdt0129>-nro_cg.

          <fs_pro>-vkbur      = wa_zsdt0131-vkbur.
          <fs_pro>-vbeln      = wa_zsdt0131-vbeln.

          <fs_pro>-qtd_vinc   = wa_zsdt0131-qtd_vinc.
          <fs_pro>-um         = wa_zsdt0131-um.
          <fs_pro>-qtd_emkg   = wa_zsdt0131-qtd_emkg.

          TRY .
              DATA(wa_zsdt0130_pro) = it_zsdt0130[ nro_lote = <fs_zsdt0129>-nro_lote
                                               nro_sol = wa_zsdt0131-nro_sol ].
              <fs_pro>-seq_ent_cg = wa_zsdt0130_pro-seq_ent_cg.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          TRY .
              <fs_pro>-name1 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-name1.
              <fs_pro>-mcod3 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-mcod3.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          TRY .
              <fs_pro>-maktx = it_makt[ matnr = wa_zsdt0131-matnr ]-maktx.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          TRY .
              wa_zsdt0132 = it_zsdt0132[ nr_rot = wa_zsdt0130-nr_rot ].
              <fs_pro>-fazenda    = wa_zsdt0132-rot_desc.
              <fs_pro>-tel_number = wa_zsdt0132-tel_number.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          <fs_pro>-nr_rot = wa_zsdt0130-nr_rot.

          TRY .
              DATA(wa_vbak_pro) = it_vbak[ vbeln = wa_zsdt0131-vbeln
                                       posnr = wa_zsdt0131-posnr ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          TRY .
              DATA(wa_konv_pro) = it_konv[ knumv = wa_vbak_pro-knumv
                                       kposn = wa_vbak_pro-posnr ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          CASE wa_konv-kmein.
            WHEN 'TO'.
              <fs_pro>-preco_frete = ( ( ( ( wa_zsdt0131-qtd_vinc * wa_zsdt0131-brgew ) * wa_konv_pro-kbetr ) / 1000 ) * wa_konv_pro-kkurs ).
            WHEN OTHERS.
              <fs_pro>-preco_frete = ( ( wa_zsdt0131-qtd_vinc * wa_konv_pro-kbetr ) * wa_konv_pro-kkurs ).
          ENDCASE.

          APPEND INITIAL LINE TO r_seq ASSIGNING <fs_seq>.
          <fs_seq>-ini    = <fs_pro>-seq_ent_cg.
          <fs_seq>-nr_cg  = <fs_pro>-nro_cg.

          TRY.
              DATA(wa_0132_pro) = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>> Processo New
          IF NOT line_exists( it_com[ ebeln = wa_zsdt0131-ebeln ] ) AND wa_zsdt0131-ebeln IS NOT INITIAL.

            READ TABLE it_ekko INTO DATA(lwa_ekko) WITH KEY ebeln = wa_zsdt0131-ebeln.
            IF sy-subrc EQ 0.

              APPEND INITIAL LINE TO it_com ASSIGNING <fs_com>.

              <fs_com>-local_embarque = wa_0132_pro-rot_desc.
              <fs_com>-municipio      = wa_0132_pro-city1.
              <fs_com>-endereco       = it_lfa1[ lifnr = wa_0132_pro-lifnr ]-stras.
              <fs_com>-uf             = wa_0132_pro-uf.

              TRY .
                  <fs_com>-fornecedor = it_lfa1[ lifnr = lwa_ekko-lifnr ]-name1.
                CATCH cx_sy_itab_line_not_found.
                  CLEAR <fs_com>-fornecedor.
              ENDTRY.

              <fs_com>-ihrez = lwa_ekko-ihrez.
              <fs_pro>-wrkst = <fs_com>-ihrez.
              <fs_com>-ebeln = wa_zsdt0131-ebeln.

            ENDIF.

          ENDIF.
          "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<---- Processo New

          "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>> Processo Old
*          LOOP AT it_zsdt0062 INTO DATA(wa_zsdt0062) WHERE vbeln EQ wa_zsdt0131-vbeln AND nro_cg EQ <fs_zsdt0129>-nro_cg.
*
*            CHECK NOT line_exists( it_com[ ebeln = wa_zsdt0062-ebeln ] ).
*
*            APPEND INITIAL LINE TO it_com ASSIGNING <fs_com>.
*
*            <fs_com>-local_embarque = wa_0132_pro-rot_desc.
*            <fs_com>-municipio = wa_0132_pro-city1.
*            <fs_com>-endereco = it_lfa1[ lifnr = wa_0132_pro-lifnr ]-stras.
*            <fs_com>-uf = wa_0132_pro-uf.
*
*            TRY .
*                <fs_com>-fornecedor = it_lfa1[ lifnr = it_ekko[ lifnr = wa_zsdt0062-lifnr ]-lifnr ]-name1.
*              CATCH cx_sy_itab_line_not_found.
*                CLEAR <fs_com>-fornecedor.
*            ENDTRY.
*
*            TRY .
*                <fs_com>-ihrez = it_ekko[ ebeln = wa_zsdt0062-ebeln ]-ihrez.
*              CATCH cx_sy_itab_line_not_found.
*                CLEAR <fs_com>-ihrez.
*            ENDTRY.
*
*            <fs_pro>-wrkst = <fs_com>-ihrez.
*            <fs_com>-ebeln = wa_zsdt0062-ebeln.
*
*          ENDLOOP.
          "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<---- Processo Old

        ENDIF.

      ENDLOOP.

    ENDLOOP.

    SORT r_seq BY nr_cg ini.
    DELETE ADJACENT DUPLICATES FROM r_seq COMPARING nr_cg ini.

    DATA cont TYPE n LENGTH 2.

    LOOP AT r_seq INTO DATA(lwa_seq).

      cont = 0.

      LOOP AT r_seq INTO DATA(lwa_seq_tmp) WHERE nr_cg EQ lwa_seq-nr_cg.
        ADD 1 TO cont.
      ENDLOOP.

      LOOP AT r_seq ASSIGNING FIELD-SYMBOL(<seq>) WHERE nr_cg EQ lwa_seq-nr_cg.
        <seq>-fim    = cont.
        SUBTRACT 1 FROM cont.
      ENDLOOP.

    ENDLOOP.

    LOOP AT it_pro  ASSIGNING FIELD-SYMBOL(<exc>).

      DATA(r_seq_sel) = r_seq[ nr_cg = <exc>-nro_cg
                               ini = <exc>-seq_ent_cg ].

      IF r_seq_sel-ini   EQ <exc>-seq_ent_cg AND
         r_seq_sel-nr_cg EQ <exc>-nro_cg.
        <exc>-seq_ent_cg = r_seq_sel-fim.
      ENDIF.

    ENDLOOP.

    SORT it_pro[] BY seq_ent_cg.

    LOOP AT it_zsdt0133 INTO DATA(wa_0133).

      TRY .
          wa_tra-transportadora = it_lfa1[ lifnr = wa_0133-cod_transportadora ]-name1.
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_tra-transportadora.
      ENDTRY.

      TRY .
          wa_tra-cnpj  = it_lfa1[ lifnr = wa_0133-cod_transportadora ]-stcd1.
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_tra-cnpj.
      ENDTRY.

      TRY .
          wa_tra-nome_motorista = it_lfa1[ lifnr = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-motorista ]-name1.
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_tra-nome_motorista.
      ENDTRY.

      TRY .
          wa_tra-cpf = it_lfa1[ lifnr = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-motorista ]-stcd2.
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_tra-cpf.
      ENDTRY.

      TRY .
          wa_tra-contato = it_lfa1[ lifnr = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-motorista ]-telf1.
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_tra-contato.
      ENDTRY.

      TRY .
          wa_tra-placa_cavalo = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-placa_cav.
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_tra-placa_cavalo.
      ENDTRY.

      TRY .
          wa_tra-placa_carreta1 = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-placa_car1.
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_tra-placa_carreta1.
      ENDTRY.

      TRY .
          wa_tra-placa_carreta2 = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-placa_car2.
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_tra-placa_carreta2.
      ENDTRY.


      TRY .
          wa_tra-placa_carreta3 = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-placa_car3.
        CATCH cx_sy_itab_line_not_found.
          CLEAR wa_tra-placa_carreta3.
      ENDTRY.

      "FF #184481 - inicio
      IF wa_0133-viagem_id IS NOT INITIAL.
        DATA(lv_viagem_id) = wa_0133-viagem_id.
      ENDIF.
      "FF #184481 - fim

    ENDLOOP.

    IF i_with_lote EQ abap_false.
      vl_formname = 'ZSDF0003'.
    ELSE.
      vl_formname = 'ZSDF0013'.
    ENDIF.


    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = vl_formname
      IMPORTING
        fm_name            = vl_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'. "SY-MSGTY
    ENDIF.


    IF i_preview EQ abap_false.
      ls_ctrl_param-no_dialog = abap_true.
    ENDIF.
    ls_ctrl_param-getotf    = i_binary. " Necessário para gerar o OTF
    ls_ctrl_param-preview   = abap_false.

    "FF #184481 - inicio

    DATA: lv_usuario_assinatura TYPE syuname.

    DATA: r_users TYPE RANGE OF sy-uname.

    r_users = VALUE #(
      ( sign = 'I' option = 'EQ' low = 'JECRUZ'      )
      ( sign = 'I' option = 'EQ' low = 'TCONCEICAO'  )
      ( sign = 'I' option = 'EQ' low = 'VMOREIRA'    )
      ( sign = 'I' option = 'EQ' low = 'APIMENTA'    )
      ( sign = 'I' option = 'EQ' low = 'TBOIN'       )
      ( sign = 'I' option = 'EQ' low = 'TCARVALHO'   )
      ( sign = 'I' option = 'EQ' low = 'JDENARDIN'   )
      ( sign = 'I' option = 'EQ' low = 'GDIAS'       )
      ( sign = 'I' option = 'EQ' low = 'GAMSILVA'    )
      ( sign = 'I' option = 'EQ' low = 'HREDIVO'     )
      ( sign = 'I' option = 'EQ' low = 'SFELIX'      )
    ).

    IF sy-uname IN  r_users.
      lv_usuario_assinatura = sy-uname.
    ELSE.
      lv_usuario_assinatura = 'TBOIN'.
    ENDIF.

    SELECT SINGLE id_localizador
      FROM zlest0185
        INTO @DATA(lv_id_localizador)
        WHERE viagem_id = @lv_viagem_id.

    IF sy-subrc <> 0.
      CLEAR lv_id_localizador.
    ENDIF.
    "FF #184481 - fim


    CALL FUNCTION vl_name
      EXPORTING
        control_parameters = ls_ctrl_param
        output_options     = ls_out_options
        user_settings      = abap_false
        wa_top             = wa_top
        wa_tra             = wa_tra
        usuario_assinatura = lv_usuario_assinatura          "FF #184481
        id_localizador     = lv_id_localizador              "FF #184481
      IMPORTING
        job_output_info    = ls_job_info
      TABLES
        it_fat             = it_fat
        it_pro             = it_pro
        it_com             = it_com
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'. "SY-MSGTY
    ENDIF.


    "Se solicitado, gerar o PDF em binário
    IF i_binary = abap_true AND ls_job_info-otfdata IS NOT INITIAL.
      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          format                = 'PDF'
          pdf_delete_otftab     = abap_on
        IMPORTING
          bin_file              = lv_pdf_xstring
        TABLES
          otf                   = ls_job_info-otfdata
          lines                 = lt_lines
        EXCEPTIONS
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          err_bad_otf           = 4
          OTHERS                = 5.

      IF sy-subrc IS INITIAL.
        e_pdf_xstring = lv_pdf_xstring.

      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD gerar_autorizacao_emb_core.
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2LTT |27/06/2025 |Implementação Status de processamento &*
*&                                    |da Carga - INSUMOS. Chamado: 169508.  &*
*&---------------------------------------------------------------------------&*

    DATA: lv_answer                TYPE c,
          lv_question              TYPE string,
          lv_xstring               TYPE xstring,
          lo_aprovar               TYPE REF TO zcl_integracao_viagem_aprovar,
          lv_id_integracao_aprovar TYPE zde_id_integracao,
          lv_msg2                  TYPE string.

    DATA: lva_name_file      TYPE string,
          lva_extension_file TYPE string,
          lt_arquivos        TYPE zmmt_arquivos.

    CLEAR: r_sucesso, e_msg_error.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single = i_nro_cg
      IMPORTING
        e_cargas          = DATA(lit_carga)
        e_romaneios       = DATA(lit_romaneios)
        e_lotes           = DATA(lit_lotes)
        e_solicitacoes    = DATA(lit_solicitacoes)
    ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      e_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_carga-integrar_carguero EQ abap_true AND lwa_carga-viagem_id IS INITIAL AND i_background EQ abap_false .
      e_msg_error = |Carga integrada ao Carguero! Autorização de Embarque é gerada na criação da Viagem e disponivel nos documentos anexos da Carga|.
      RETURN.
    ENDIF.

    IF lwa_carga-dt_autorizacao_embarque IS NOT INITIAL.
      e_msg_error = |Autorização já emitida, acessar os anexos da carga|.
      RETURN.
    ENDIF.

    IF lwa_carga-cod_transportadora IS INITIAL AND lwa_carga-inco1 NE 'FOB'.
      e_msg_error = |Carga sem transportadora informada! Operação permitida|.
      RETURN.
    ENDIF.

    DATA(_autorizacao_emb_com_lote) = abap_false.
    DATA(_embarque_armazem) = zcl_carga_saida_insumos=>check_embarque_armazem( i_nro_cg = i_nro_cg ).
    IF _embarque_armazem EQ  abap_true.
      "IF lit_lotes[] IS NOT INITIAL.
      _autorizacao_emb_com_lote = abap_true.
      "ENDIF.
    ELSE.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New --->>>
      LOOP AT lit_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_sol>).
*** DesComentado para liberar Versão da Autorização US-184481 22/07/2025
*        SELECT SINGLE *
*          FROM zsdt0062 INTO @DATA(lwa_zsdt0062)
*         WHERE nro_cg EQ @i_nro_cg
*           AND vbeln  EQ @<fs_sol>-vbeln
*           AND status EQ 'L'.
*
*        IF sy-subrc NE 0.
*          e_msg_error = |OV { <fs_sol>-vbeln } sem vinculação com pedido!|.
*          RETURN.
*        ENDIF.
*** DesComentado para liberar Versão da Autorização US-184481 22/07/2025
*** Comentado para liberar Versão da Autorização US-184481 22/07/2025
        IF <fs_sol>-ebeln IS INITIAL.
          e_msg_error = |OV { <fs_sol>-vbeln } sem pedido determinado!|.
          RETURN.
        ENDIF.
*** Comentado para liberar Versão da Autorização US-184481 22/07/2025
      ENDLOOP.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New <<----

    ENDIF.

    IF i_background EQ abap_false.
      e_msg_error = zcl_carga_saida_insumos=>check_permissao_carga( EXPORTING i_nro_carga = lwa_carga-nro_cg
                                                                              i_atividade = '08' ).
    ENDIF.

    CHECK e_msg_error IS INITIAL.

**---------------------------------------------------------------------------------------------------------*
**   Pré Visualização Carga
**---------------------------------------------------------------------------------------------------------*
    IF i_background EQ abap_false.
      TRY .
          zcl_carga_saida_insumos=>gerar_autorizacao_embarque_sm(
            EXPORTING
              i_carga     = i_nro_cg
              i_preview   = abap_true
              i_with_lote = _autorizacao_emb_com_lote
          ).

        CATCH zcx_integracao INTO DATA(lo_integra).
          MESSAGE ID lo_integra->msgid TYPE 'I'
              NUMBER lo_integra->msgno
                WITH lo_integra->msgv1
                     lo_integra->msgv2
                     lo_integra->msgv3
                     lo_integra->msgv4 INTO e_msg_error.
          RETURN.
        CATCH zcx_error INTO DATA(lo_error).
          MESSAGE ID lo_error->msgid TYPE 'I'
              NUMBER lo_error->msgno
                WITH lo_error->msgv1
                     lo_error->msgv2
                     lo_error->msgv3
                     lo_error->msgv4 INTO e_msg_error.
          RETURN.
      ENDTRY.
    ENDIF.

*    SELECT SINGLE transf_no_fornecedor
*      FROM zmmt0196 INTO @DATA(lv_transf)
*      WHERE nro_sol = @lwa_zmmt0201-nro_sol.
*
*    IF sy-subrc IS INITIAL.
*      IF lv_transf IS NOT INITIAL.
*        UPDATE zmmt0201 SET transf_no_fornecedor = lv_transf
*          WHERE nro_cg = lwa_zmmt0201-nro_cg.
*      ENDIF.
*    ENDIF.

**---------------------------------------------------------------------------------------------------------*
**   Gerar XString da Autorização Embarque
**---------------------------------------------------------------------------------------------------------*
    TRY.
        zcl_carga_saida_insumos=>gerar_autorizacao_embarque_sm(
          EXPORTING
            i_carga       = i_nro_cg
            i_binary      = abap_true
            i_preview     = abap_false
            i_with_lote   = _autorizacao_emb_com_lote
          IMPORTING
            e_pdf_xstring = DATA(lva_pdf_xstring) ).

      CATCH zcx_integracao INTO lo_integra.
        MESSAGE ID lo_integra->msgid TYPE 'I'
            NUMBER lo_integra->msgno
              WITH lo_integra->msgv1
                   lo_integra->msgv2
                   lo_integra->msgv3
                   lo_integra->msgv4 INTO e_msg_error.
        RETURN.
      CATCH zcx_error INTO lo_error.
        MESSAGE ID lo_error->msgid TYPE 'I'
            NUMBER lo_error->msgno
              WITH lo_error->msgv1
                   lo_error->msgv2
                   lo_error->msgv3
                   lo_error->msgv4 INTO e_msg_error.
        RETURN.
    ENDTRY.

    IF lva_pdf_xstring IS INITIAL.
      e_msg_error = |Erro ao gear o Arquivo de Autorização de embarque|.
      RETURN.
    ENDIF.

**---------------------------------------------------------------------------------------------------------*
**   Anexar Documentos na Carga
**---------------------------------------------------------------------------------------------------------*

    CLEAR: lt_arquivos[].

    APPEND INITIAL LINE TO lt_arquivos ASSIGNING FIELD-SYMBOL(<fs_arquivos>).
    <fs_arquivos>-descricao = |Autorização Embarque { sy-datum } - { sy-uzeit }.pdf|.
    <fs_arquivos>-xstring   = lva_pdf_xstring.
    <fs_arquivos>-tipo      = 'PDF'.

    CALL METHOD zcl_carga_saida_insumos=>anexar_documentos_carga(
      EXPORTING
        i_nro_carga = i_nro_cg
        i_arquivos  = lt_arquivos
      IMPORTING
        e_msg_erro  = e_msg_error ).

    CHECK e_msg_error IS INITIAL.

*----------------------------------------------------------------------------------------------------------*
*   Incrementa ID Autorização Embarque
*----------------------------------------------------------------------------------------------------------*

   ADD 1 TO lwa_carga-seq_autorizacao_embarque.

**---------------------------------------------------------------------------------------------------------*
** Integração Carguero/Strada
**---------------------------------------------------------------------------------------------------------*
    IF lwa_carga-integrar_carguero IS NOT INITIAL AND i_viagem_id IS NOT INITIAL.

      IF ( i_envia_autorizacao_carguero EQ abap_true ).

        TRY.
            zcl_carga_saida_insumos=>envia_path_autoriza_embarque(
              EXPORTING
                i_carga       = i_nro_cg
                i_viagem_id   = i_viagem_id
                i_pdf_xstring = lva_pdf_xstring
              EXCEPTIONS
                erro_upload_nao_autorizado = 1                " Nao autorizado upload
                erro_upload                = 2                " Erro ao gerar arquivos
                OTHERS                     = 3 ).

              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO  e_msg_error.
                e_msg_error = |Erro enviar Autorização para Carguero: Erro: { e_msg_error }|.
                RETURN.
              ENDIF.

          CATCH zcx_integracao INTO lo_integra.
            MESSAGE ID lo_integra->msgid TYPE 'I'
                NUMBER lo_integra->msgno
                  WITH lo_integra->msgv1
                       lo_integra->msgv2
                       lo_integra->msgv3
                       lo_integra->msgv4 INTO e_msg_error.
            RETURN.
          CATCH zcx_error INTO lo_error.
            MESSAGE ID lo_error->msgid TYPE 'I'
            NUMBER lo_error->msgno
              WITH lo_error->msgv1
                   lo_error->msgv2
                   lo_error->msgv3
                   lo_error->msgv4 INTO e_msg_error.
            RETURN.
        ENDTRY.
      ENDIF.

    ENDIF.

**---------------------------------------------------------------------------------------------------------*
** Integração Centro Distribuição
**---------------------------------------------------------------------------------------------------------*
    DATA(_criou_pedido_luft) = abap_false.
    IF ( zcl_carga_saida_insumos=>get_embarque_luft( i_nro_cg = i_nro_cg ) EQ abap_true ) AND
       ( zcl_util_sd=>ck_integration_luft_active( i_direcao = 'S' ) EQ abap_true ).

      lwa_carga-nro_pedido_luft = lwa_carga-nro_cg && '_' && lwa_carga-seq_autorizacao_embarque.

      DATA(_error_cria_pedido_luft) =
        zcl_carga_saida_insumos=>criar_pedido_luft_v1( EXPORTING i_nro_cg          = i_nro_cg
                                                                 i_nro_pedido_luft = lwa_carga-nro_pedido_luft ).

      IF _error_cria_pedido_luft IS NOT INITIAL.
        e_msg_error = |Não foi possivel criar Pedido na LUFT: Motivo: { _error_cria_pedido_luft }|.
      ENDIF.

      CHECK e_msg_error IS INITIAL.

      _criou_pedido_luft = abap_true.

    ENDIF.

    UPDATE zsdt0133 SET dt_autorizacao_embarque  = sy-datum
                        seq_autorizacao_embarque = lwa_carga-seq_autorizacao_embarque
                        pedido_luft_criado       = _criou_pedido_luft
                        nro_pedido_luft          = lwa_carga-nro_pedido_luft
     WHERE nro_cg = i_nro_cg.

    r_sucesso = abap_true.


  ENDMETHOD.


  METHOD gerar_cotacao.
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2LTT |27/06/2025 |Implementação Status de processamento &*
*&                                    |da Carga - INSUMOS. Chamado: 169508.  &*
*&---------------------------------------------------------------------------&*

    CLEAR: e_nro_cg_sucess[].

    CHECK it_nro_cg[] IS NOT INITIAL.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga        = it_nro_cg
      IMPORTING
        e_cargas          = DATA(lit_cargas)
    ).

    LOOP AT lit_cargas INTO DATA(el_zsdt0133).

      IF NOT ( el_zsdt0133-status EQ '1' OR el_zsdt0133-status EQ '2' ).
        r_msg_error = |O Status não permite esta ação.|.
        RETURN.
      ENDIF.

      r_msg_error = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg   = el_zsdt0133-nro_cg
                                                                                   i_bloqueio = abap_true ).

      CHECK r_msg_error IS INITIAL.

*Atualiza para Carga em Cotação.
      UPDATE zsdt0133 SET carga_em_cotacao = abap_true
                          dt_envio_cotacao = sy-datum
       WHERE nro_cg EQ el_zsdt0133-nro_cg.
      COMMIT WORK.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = el_zsdt0133-nro_cg ) TO e_nro_cg_sucess.

      r_msg_error = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg   = el_zsdt0133-nro_cg
                                                                                   i_bloqueio = abap_false
                                                                        ).

    ENDLOOP.


  ENDMETHOD.


  METHOD gerar_lote_embarcador_carguero.

    CLEAR: r_msg_error.

*    IF sy-sysid EQ 'QAS' AND sy-uzeit >= '1700'.
*      EXIT.
*    ENDIF.

    "Seleciona Cabeçalho Carga
    SELECT SINGLE *
      FROM zsdt0133 INTO @DATA(lwa_0133)
      WHERE nro_cg EQ @i_nro_cg.

    CHECK sy-subrc EQ 0 AND lwa_0133-integrar_carguero EQ abap_true.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = 'Aguarde...Integrando Carga ao Carguero...'.

    TRY .
        zcl_integracao_lote_frete=>zif_integracao_lote_frete~set_gerencia_lote(
          EXPORTING
            i_nro_cg_sai    = i_nro_cg
            i_sincronia     = zif_integracao=>at_tp_sincronia_sincrona
          IMPORTING
            e_id_lote_frete = DATA(lv_id_lote_frete) ).

      CATCH zcx_integracao INTO DATA(ex_integracao).
        r_msg_error = ex_integracao->get_text( ).
        REPLACE ALL OCCURRENCES OF REGEX '&' IN r_msg_error WITH ''.
        RETURN.
      CATCH zcx_error INTO DATA(ex_error).
        r_msg_error = ex_error->get_text( ).
        REPLACE ALL OCCURRENCES OF REGEX '&' IN r_msg_error WITH ''.
        RETURN.
    ENDTRY.


  ENDMETHOD.


  METHOD gerar_romaneios.

    CLEAR: r_msg_error.

    r_msg_error = zcl_carga_saida_insumos=>check_permissao_carga(
      EXPORTING
         i_nro_carga = i_nro_cg
         i_atividade = '12' ).

    CHECK r_msg_error IS INITIAL.

    r_msg_error = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg   = i_nro_cg i_bloqueio = abap_true ).

    CHECK r_msg_error IS INITIAL.

    zcl_carga_saida_insumos=>gerar_romaneios_core(
      EXPORTING
        i_nro_cg             = i_nro_cg
       IMPORTING
         e_romaneios_gerados = DATA(_romaneios_gerados)
       RECEIVING
        r_msg_error         =  r_msg_error ).

    IF _romaneios_gerados IS INITIAL.
      zcl_carga_saida_insumos=>del_lote_preenchimento_auto( EXPORTING i_nro_cg   = i_nro_cg ).
    ENDIF.

    zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_cg i_bloqueio = abap_false ).


  ENDMETHOD.


  METHOD gerar_romaneios_core.

    DATA: zcl_romaneio TYPE REF TO zcl_romaneio.

    DATA: lit_zsdt0001_gravou TYPE STANDARD TABLE OF zsdt0001,
          lit_zsdt0134_aux    TYPE STANDARD TABLE OF zsdt0134,
          lit_vbpa            TYPE STANDARD TABLE OF vbpa,
          lit_vbap            TYPE STANDARD TABLE OF vbap,
          wa_zsdt0001         TYPE zsdt0001,
          wa_zsdt0001_item    TYPE zsdt0001_item,
          vl_brgew_tot        TYPE zsdt0001_item-brgew.

    CLEAR: r_msg_error, e_romaneios_gerados.

    r_msg_error = zcl_carga_saida_insumos=>preencher_lotes_carga( EXPORTING i_nro_cg    = i_nro_cg ).

    CHECK r_msg_error IS INITIAL.

    r_msg_error = zcl_carga_saida_insumos=>validar_geracao_romaneios( EXPORTING i_nro_cg    = i_nro_cg ).

    CHECK r_msg_error IS INITIAL.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single = i_nro_cg
      IMPORTING
        e_cargas          = DATA(lit_cargas)
        e_solicitacoes    = DATA(lit_solicitacoes)
        e_lotes           = DATA(lit_lotes)
    ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_cargas[] IS INITIAL.
      r_msg_error = 'Carga não encontrada!'.
      RETURN.
    ENDIF.

    IF lit_solicitacoes[] IS INITIAL.
      r_msg_error = 'Carga sem solicitações!'.
      RETURN.
    ENDIF.

    SELECT *
      FROM vbpa INTO TABLE lit_vbpa
      FOR ALL ENTRIES IN lit_solicitacoes
      WHERE vbeln EQ lit_solicitacoes-vbeln
      AND ( parvw EQ 'PC' OR parvw EQ 'AG' ).

    SORT lit_solicitacoes BY vbeln nr_rot.
    DELETE ADJACENT DUPLICATES FROM lit_solicitacoes COMPARING vbeln nr_rot.

    CLEAR: lit_zsdt0001_gravou.
    DATA(_error_geracao_romaneio) = abap_false.

    CREATE OBJECT zcl_romaneio.
    LOOP AT lit_solicitacoes INTO DATA(lwa_solicitacao).

      CLEAR: wa_zsdt0001.

      wa_zsdt0001-tp_movimento        = 'S'.
      wa_zsdt0001-vbeln               = lwa_solicitacao-vbeln.
      wa_zsdt0001-nr_rot              = lwa_solicitacao-nr_rot.
      wa_zsdt0001-dt_movimento        = sy-datum.
      wa_zsdt0001-peso_liq            = lwa_solicitacao-tot_vinc_ov_kg.
      wa_zsdt0001-peso_fiscal         = lwa_solicitacao-tot_vinc_ov_kg.
      wa_zsdt0001-bukrs               = lwa_solicitacao-vkorg.
      wa_zsdt0001-branch              = lwa_solicitacao-werks.
      wa_zsdt0001-placa_cav           = lwa_carga-placa_cav.
      wa_zsdt0001-placa_car1          = lwa_carga-placa_car1.
      wa_zsdt0001-placa_car2          = lwa_carga-placa_car2.
      wa_zsdt0001-placa_car3          = lwa_carga-placa_car3.
      wa_zsdt0001-motorista           = lwa_carga-motorista.
      wa_zsdt0001-nro_cg              = lwa_carga-nro_cg.
      wa_zsdt0001-agente_frete        = lwa_carga-cod_transportadora.
      wa_zsdt0001-id_interface        = '48'.

      SELECT SINGLE *
        FROM zi_sd_dados_compl_ov_info INTO @DATA(lwa_dados_comp_ov)
       WHERE vbeln EQ @lwa_solicitacao-vbeln.

      CHECK sy-subrc EQ 0.

      SELECT SINGLE *
        FROM zsdt0040 INTO @DATA(la_zsdt0040)
       WHERE doc_simulacao EQ @lwa_dados_comp_ov-nro_sol.

      CHECK sy-subrc EQ 0 AND la_zsdt0040-safra IS NOT INITIAL.

      wa_zsdt0001-nr_safra  = la_zsdt0040-safra.

      READ TABLE lit_vbpa INTO DATA(wa_vbpa) WITH KEY vbeln = lwa_solicitacao-vbeln
                                                      parvw = 'PC'.
      IF sy-subrc IS INITIAL.
        wa_zsdt0001-parid = wa_vbpa-lifnr.
      ENDIF.

      READ TABLE lit_vbpa INTO wa_vbpa WITH KEY vbeln = lwa_solicitacao-vbeln
                                                parvw = 'AG'.
      IF sy-subrc IS INITIAL.
        wa_zsdt0001-id_cli_dest = wa_vbpa-kunnr.
      ENDIF.

      IF lwa_carga-inco1 EQ 'FOB'.
        wa_zsdt0001-tp_frete = 'F'.
      ELSE.
        wa_zsdt0001-tp_frete = 'C'.
      ENDIF.

      zcl_romaneio->zif_cadastro~novo_registro( ).
      zcl_romaneio->set_cabecalho_romaneio( i_romaneio = wa_zsdt0001 ).

      "Salva Chave Referencia na ZSDT0134
      CLEAR: lit_zsdt0134_aux[], lit_vbap[].
      SELECT *
        FROM zsdt0134 INTO TABLE lit_zsdt0134_aux
        WHERE nro_cg EQ lwa_carga-nro_cg
          AND vbeln  EQ lwa_solicitacao-vbeln
          AND nr_rot EQ lwa_solicitacao-nr_rot
          AND status NE 'X'.

      IF lit_zsdt0134_aux[] IS NOT INITIAL.
        SELECT *
          FROM vbap INTO TABLE lit_vbap
          FOR ALL ENTRIES IN lit_zsdt0134_aux
          WHERE vbeln EQ lit_zsdt0134_aux-vbeln
            AND posnr EQ lit_zsdt0134_aux-posnr.
      ENDIF.

      CLEAR: vl_brgew_tot. "CS2017002830 - 12.12.2017

      LOOP AT lit_zsdt0134_aux INTO DATA(wa_zsdt0134).

        CLEAR: wa_zsdt0001_item.
        wa_zsdt0001_item-vbeln  = wa_zsdt0134-vbeln.
        wa_zsdt0001_item-posnr  = wa_zsdt0134-posnr.
        wa_zsdt0001_item-nr_rot = wa_zsdt0134-nr_rot.
        wa_zsdt0001_item-charg  = wa_zsdt0134-charg.
        wa_zsdt0001_item-lfimg  = wa_zsdt0134-lfimg.

        READ TABLE lit_vbap INTO DATA(wa_vbap) WITH KEY vbeln = wa_zsdt0134-vbeln
                                                        posnr = wa_zsdt0134-posnr.
        IF sy-subrc IS INITIAL.
          wa_zsdt0001_item-meins = wa_vbap-meins.
          wa_zsdt0001_item-matnr = wa_vbap-matnr.
          wa_zsdt0001_item-brgew = wa_zsdt0134-peso_liq_brt.
          wa_zsdt0001_item-ntgew = wa_zsdt0134-peso_liq_brt.
          ADD wa_zsdt0001_item-brgew TO vl_brgew_tot.
        ENDIF.

        zcl_romaneio->add_item( CHANGING i_item = wa_zsdt0001_item ).
      ENDLOOP.

      IF ( lit_zsdt0134_aux[] IS NOT INITIAL ) AND ( vl_brgew_tot <> 0 ).
        wa_zsdt0001-peso_liq      = vl_brgew_tot.
        wa_zsdt0001-peso_fiscal   = vl_brgew_tot.
        wa_zsdt0001-peso_subtotal = vl_brgew_tot.
        zcl_romaneio->set_cabecalho_romaneio( i_romaneio = wa_zsdt0001 ).
      ENDIF.

      TRY.
          zcl_romaneio->zif_cadastro~gravar_registro( RECEIVING i_gravou = DATA(vl_gravou) ).
          IF vl_gravou IS INITIAL.
            _error_geracao_romaneio = abap_true.
          ENDIF.
        CATCH zcx_cadastro INTO DATA(zcx_cadastro).
          _error_geracao_romaneio = abap_true.
          zcx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

      IF _error_geracao_romaneio IS NOT INITIAL.
        EXIT.
      ENDIF.

      zcl_romaneio->get_registro( IMPORTING e_registro = wa_zsdt0001 ).
      APPEND wa_zsdt0001 TO lit_zsdt0001_gravou.
    ENDLOOP.

    IF _error_geracao_romaneio IS INITIAL. "Se gravou todos os romaneios com sucesso

      "Gravar chave referencia nos lotes..
      CLEAR: lit_zsdt0134_aux.
      SELECT *
        FROM zsdt0134 INTO TABLE lit_zsdt0134_aux
         FOR ALL ENTRIES IN lit_zsdt0001_gravou
       WHERE nro_cg EQ lit_zsdt0001_gravou-nro_cg
         AND vbeln  EQ lit_zsdt0001_gravou-vbeln
         AND nr_rot EQ lit_zsdt0001_gravou-nr_rot
         AND status NE 'X'.

      LOOP AT lit_zsdt0134_aux INTO wa_zsdt0134.
        READ TABLE lit_zsdt0001_gravou INTO wa_zsdt0001 WITH KEY nro_cg = wa_zsdt0134-nro_cg
                                                                 vbeln  = wa_zsdt0134-vbeln
                                                                 nr_rot = wa_zsdt0134-nr_rot.
        IF sy-subrc IS INITIAL.
          wa_zsdt0134-ch_referencia = wa_zsdt0001-ch_referencia.
        ENDIF.
        MODIFY zsdt0134 FROM wa_zsdt0134.
      ENDLOOP.

    ELSE. "Se ocorreu erro em algum romaneio, apagar romaneios gerados...

      LOOP AT lit_zsdt0001_gravou INTO wa_zsdt0001.
        TRY.
            zcl_romaneio->set_registro( EXPORTING i_id_registro = wa_zsdt0001-ch_referencia ).
            zcl_romaneio->excluir_registro( ).
          CATCH zcx_cadastro INTO zcx_cadastro.
            zcx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        ENDTRY.

      ENDLOOP.

    ENDIF.

    CHECK _error_geracao_romaneio IS INITIAL.

    COMMIT WORK.

    e_romaneios_gerados = abap_true.

*----------------------------------------------------------------------------------------------------------*
*   Executar Movimentação Troca Nota
*----------------------------------------------------------------------------------------------------------*
    r_msg_error =  zcl_carga_saida_insumos=>executar_troca_nota( EXPORTING i_nro_cg = i_nro_cg ).

    IF r_msg_error IS NOT INITIAL.
      MESSAGE |Romaneios gerados, porém não foi possivel realizar a movimentação Troca nota! Erro: { r_msg_error } | TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    COMMIT WORK.

*----------------------------------------------------------------------------------------------------------*
*   Executar Faturamento Automatico
*----------------------------------------------------------------------------------------------------------*
    DATA(_preencher_lote_manual) = zcl_carga_saida_insumos=>get_preenchimento_lote_manual( i_nro_cg = i_nro_cg ).

    IF _preencher_lote_manual EQ abap_true.

      MESSAGE 'Romaneios gerados com sucesso!' TYPE 'S'.

    ELSE.

      r_msg_error =  zcl_carga_saida_insumos=>exec_faturamento_saida_auto( EXPORTING i_nro_cg = i_nro_cg ).

      IF r_msg_error IS NOT INITIAL.
        MESSAGE |Romaneios gerados, porém não foi possivel iniciar o faturamento automatico! Erro: { r_msg_error } | TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      MESSAGE 'Romaneios gerados com sucesso e faturamento iniciado!' TYPE 'S'.

    ENDIF.


  ENDMETHOD.


  METHOD get_dados_solitacoes.
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2LTT |27/06/2025 |Inserir os Campos Município e Texto do&*
*&                                    |Roteiro para Local de Embarque e Ponto&*
*&                                    |de Coleta Chamado: 169508.            &*
*&---------------------------------------------------------------------------&*

    DATA: lit_solicitacoes TYPE zsds381_t,
          lva_qtd_usada    TYPE zsdt0131-qtd_vinc.

    CLEAR: lit_solicitacoes[], r_solicitacoes[].

    SELECT vbap~matnr
           mara~meins
           mara~wrkst
           zsdt0082~mandt
           zsdt0082~nro_sol
           zsdt0082~seq
           zsdt0082~vbeln
           zsdt0082~posnr
           zsdt0082~seq_lib
           zsdt0082~vkorg
           zsdt0082~spart
           zsdt0082~vkgrp
           zsdt0082~vkbur
           zsdt0082~auart
           vbap~werks
           zsdt0082~qte_sol
           zsdt0082~dt_liber
           zsdt0082~usuario_lib
           zsdt0082~qte_lib
           zsdt0082~status
           zsdt0082~dt_entrega
           mara~brgew
           zsdt0082~nr_rot
           zsdt0082~nr_rot_pc
           zsdt0082~origem_estoque
           zsdt0082~ebeln
           zsdt0082~ebelp
           zsdt0082~lifnr_arm
           zsdt0082~charg
           zsdt0082~marca
           zsdt0082~carga_automatica
           zsdt0082~flexibilidade
      INTO CORRESPONDING FIELDS OF TABLE lit_solicitacoes
      FROM zsdt0082 INNER JOIN vbap ON zsdt0082~vbeln = vbap~vbeln AND
                                       zsdt0082~posnr = vbap~posnr
                    INNER JOIN mara ON mara~matnr = vbap~matnr
                    INNER JOIN vbak ON vbak~vbeln = vbap~vbeln
     FOR ALL ENTRIES IN i_solicitacoes
      WHERE zsdt0082~nro_sol = i_solicitacoes-nro_sol
        AND zsdt0082~seq     = i_solicitacoes-seq
        AND zsdt0082~vbeln   = i_solicitacoes-vbeln
        AND zsdt0082~posnr   = i_solicitacoes-posnr.

    CHECK lit_solicitacoes[] IS NOT INITIAL.

    SELECT *
      FROM vbkd INTO TABLE @DATA(it_vbkd)
      FOR ALL ENTRIES IN @lit_solicitacoes
      WHERE vbeln EQ @lit_solicitacoes-vbeln.

    SELECT *
      FROM tvkbt INTO TABLE @DATA(lit_tvkbt)
       FOR ALL ENTRIES IN @lit_solicitacoes
     WHERE spras EQ @sy-langu
       AND vkbur EQ @lit_solicitacoes-vkbur.

    SELECT vbap~vbeln,
           vbap~posnr,
           vbak~knumv
      FROM vbap INNER JOIN vbak ON vbap~vbeln = vbak~vbeln
       INTO TABLE @DATA(lit_price_ov)
       FOR ALL ENTRIES IN @lit_solicitacoes
     WHERE vbap~vbeln EQ @lit_solicitacoes-vbeln
       AND vbap~posnr EQ @lit_solicitacoes-posnr.

    IF lit_price_ov IS NOT INITIAL.
      SELECT
        FROM v_konv FIELDS * FOR ALL ENTRIES IN @lit_price_ov
       WHERE knumv EQ @lit_price_ov-knumv
         AND kposn EQ @lit_price_ov-posnr
         AND kschl EQ 'PR00'
        INTO TABLE @DATA(lit_konv).
    ENDIF.

    SELECT *
      FROM zsdt0132 INTO TABLE @DATA(it_zsdt0132)
      FOR ALL ENTRIES IN @lit_solicitacoes
      WHERE nr_rot EQ @lit_solicitacoes-nr_rot.

    SELECT *
      FROM zsdt0132 APPENDING TABLE @it_zsdt0132
      FOR ALL ENTRIES IN @lit_solicitacoes
      WHERE nr_rot EQ @lit_solicitacoes-nr_rot_pc.

    SELECT *
      FROM makt INTO TABLE @DATA(it_makt)
      FOR ALL ENTRIES IN @lit_solicitacoes
      WHERE matnr EQ @lit_solicitacoes-matnr.

    SELECT *
      FROM zsdt0131 INTO TABLE @DATA(it_zsdt0131)
      FOR ALL ENTRIES IN @lit_solicitacoes
      WHERE nro_sol EQ @lit_solicitacoes-nro_sol
        AND seq     EQ @lit_solicitacoes-seq
        AND status  NE 'X'.

    IF it_zsdt0131[] IS NOT INITIAL.
      SELECT *
       FROM zsdt0129 INTO TABLE @DATA(lit_zsdt0129)
       FOR ALL ENTRIES IN @it_zsdt0131
       WHERE nro_lote EQ @it_zsdt0131-nro_lote
         AND status  NE 'X'.
    ENDIF.

    SELECT *
      FROM vbpa INTO TABLE @DATA(it_vbpa)
      FOR ALL ENTRIES IN @lit_solicitacoes
      WHERE vbeln EQ @lit_solicitacoes-vbeln
        AND parvw EQ 'AG'.

    SELECT *
      FROM kna1 INTO TABLE @DATA(it_kna1)
      FOR ALL ENTRIES IN @it_vbpa
      WHERE kunnr EQ @it_vbpa-kunnr.

    IF it_zsdt0132[] IS NOT INITIAL.

      SELECT *
        FROM kna1 APPENDING TABLE @it_kna1
        FOR ALL ENTRIES IN @it_zsdt0132
        WHERE kunnr EQ @it_zsdt0132-kunnr.

      IF it_kna1[] IS NOT INITIAL.
        SELECT *
          FROM adrc INTO TABLE @DATA(lit_adrc)
          FOR ALL ENTRIES IN @it_kna1
         WHERE addrnumber EQ @it_kna1-adrnr.
      ENDIF.

      SELECT *
        FROM lfa1 INTO TABLE @DATA(it_lfa1)
        FOR ALL ENTRIES IN @it_zsdt0132
        WHERE lifnr EQ @it_zsdt0132-lifnr.

    ENDIF.

    LOOP AT lit_solicitacoes INTO DATA(lwa_solicitacao).

      CLEAR: lva_qtd_usada.

      APPEND INITIAL LINE TO r_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao_saida>).

      MOVE-CORRESPONDING lwa_solicitacao TO <fs_solicitacao_saida>.

      READ TABLE it_vbkd INTO DATA(wa_vbkd) WITH KEY vbeln = <fs_solicitacao_saida>-vbeln.
      IF sy-subrc IS INITIAL.
        <fs_solicitacao_saida>-inco1 = wa_vbkd-inco1.
      ENDIF.

      READ TABLE lit_tvkbt INTO DATA(lwa_tvkbt) WITH KEY vkbur = <fs_solicitacao_saida>-vkbur.
      IF sy-subrc IS INITIAL.
        <fs_solicitacao_saida>-vkbur_bezei = lwa_tvkbt-bezei.
      ENDIF.

      READ TABLE it_makt INTO DATA(wa_makt) WITH KEY matnr = <fs_solicitacao_saida>-matnr.
      IF sy-subrc IS INITIAL.
        <fs_solicitacao_saida>-maktx = wa_makt-maktx.
      ENDIF.

      LOOP AT it_zsdt0131 INTO DATA(wa_zsdt0131) WHERE nro_sol EQ <fs_solicitacao_saida>-nro_sol
                                                   AND seq     EQ <fs_solicitacao_saida>-seq.

        READ TABLE lit_zsdt0129 INTO DATA(lwa_zsdt0129) WITH KEY nro_lote = wa_zsdt0131-nro_lote.
        CHECK sy-subrc EQ 0.

        IF i_nro_cg_no_check_saldo IS NOT INITIAL.
          CHECK i_nro_cg_no_check_saldo NE lwa_zsdt0129-nro_cg.
        ENDIF.

        ADD wa_zsdt0131-qtd_vinc TO lva_qtd_usada.
      ENDLOOP.

      "Dados Ponto Coleta
      READ TABLE it_zsdt0132 INTO DATA(lwa_zsdt0132) WITH KEY nr_rot = <fs_solicitacao_saida>-nr_rot_pc.
      IF sy-subrc EQ 0.
        <fs_solicitacao_saida>-lifnr        = lwa_zsdt0132-lifnr.
        READ TABLE it_lfa1 INTO DATA(lwa_lfa1) WITH KEY lifnr = lwa_zsdt0132-lifnr.
        IF sy-subrc EQ 0.
          <fs_solicitacao_saida>-name1_lifnr = lwa_lfa1-name1.
        ENDIF.

        <fs_solicitacao_saida>-cod_loc_emb  = lwa_zsdt0132-nr_rot.
        <fs_solicitacao_saida>-local_embarq = lwa_zsdt0132-rot_desc.
**<<<------"169508 - NMS - INI------>>>
        <fs_solicitacao_saida>-city_loc_emb = lwa_zsdt0132-city1.
        <fs_solicitacao_saida>-txrot_loc_emb = get_texto_roteito( i_name = CONV #( <fs_solicitacao_saida>-nr_rot_pc ) ).
**<<<------"169508 - NMS - FIM------>>>
      ENDIF.

      "Dados Local Entrega
      READ TABLE it_zsdt0132 INTO lwa_zsdt0132 WITH KEY nr_rot = <fs_solicitacao_saida>-nr_rot.
      IF sy-subrc EQ 0.
        <fs_solicitacao_saida>-kunnr        = lwa_zsdt0132-kunnr.

        READ TABLE it_kna1 INTO DATA(lwa_kna1) WITH KEY kunnr = lwa_zsdt0132-kunnr.
        IF sy-subrc EQ 0.
          <fs_solicitacao_saida>-name1_kunnr = lwa_kna1-name1.
          <fs_solicitacao_saida>-cnpj_lr     = lwa_kna1-stcd1.
          <fs_solicitacao_saida>-cpf_lr      = lwa_kna1-stcd2.
          <fs_solicitacao_saida>-ie_lr       = lwa_kna1-stcd3.

          READ TABLE lit_adrc INTO DATA(lwa_adrc) WITH KEY addrnumber = lwa_kna1-adrnr.
          IF sy-subrc EQ 0.
            <fs_solicitacao_saida>-stras_lr         = lwa_adrc-street.
            <fs_solicitacao_saida>-house_num1_lr    = lwa_adrc-house_num1.
            <fs_solicitacao_saida>-pstlz_lr         = lwa_adrc-post_code1.
            <fs_solicitacao_saida>-ort02_lr         = lwa_adrc-city2.
            <fs_solicitacao_saida>-ort01_lr         = lwa_adrc-city1.
            <fs_solicitacao_saida>-uf_lr            = lwa_kna1-regio.
          ENDIF.

        ENDIF.

        <fs_solicitacao_saida>-ds_rot_lr   = lwa_zsdt0132-rot_desc.
        <fs_solicitacao_saida>-city_rot_lr = lwa_zsdt0132-city1.
**<<<------"169508 - NMS - INI------>>>
        <fs_solicitacao_saida>-txrot_rot_lr = get_texto_roteito( i_name = CONV #( <fs_solicitacao_saida>-nr_rot ) ).
**<<<------"169508 - NMS - FIM------>>>




      ENDIF.

      <fs_solicitacao_saida>-saldo        = <fs_solicitacao_saida>-qte_lib - lva_qtd_usada.
      <fs_solicitacao_saida>-qtd_vinc     = <fs_solicitacao_saida>-saldo.
      <fs_solicitacao_saida>-qtd_vinc_kg  = <fs_solicitacao_saida>-saldo * <fs_solicitacao_saida>-brgew.

      READ TABLE lit_price_ov INTO DATA(lwa_sol_val_price) WITH KEY vbeln = lwa_solicitacao-vbeln
                                                                    posnr = lwa_solicitacao-posnr.
      IF sy-subrc IS INITIAL.
        READ TABLE lit_konv INTO DATA(lwa_konv) WITH KEY knumv = lwa_sol_val_price-knumv
                                                         kposn = lwa_sol_val_price-posnr.
        IF sy-subrc IS INITIAL.
          <fs_solicitacao_saida>-konv_kmein = lwa_konv-kmein.
          <fs_solicitacao_saida>-konv_kbetr = lwa_konv-kbetr.
          <fs_solicitacao_saida>-konv_kkurs = lwa_konv-kkurs.
        ENDIF.
      ENDIF.


    ENDLOOP.


  ENDMETHOD.


  METHOD get_observacoes_lote_carguero.

    DATA: lva_data_entrega TYPE char50.

    DATA: wl_name_rot   TYPE thead-tdname,
          it_texto      TYPE STANDARD TABLE OF tline,
          lva_texto_rot TYPE string.

    CLEAR: r_observacoes.

    CHECK i_nro_cg IS NOT INITIAL.

    "Seleciona Cabeçalho Carga
    SELECT SINGLE *
      FROM zsdt0133 INTO @DATA(lwa_0133)
      WHERE nro_cg EQ @i_nro_cg.

    CHECK sy-subrc EQ 0.

    "Seleciona Cabeçalho Lote
    SELECT SINGLE *
      FROM zsdt0129 INTO @DATA(lwa_0129)
     WHERE nro_cg EQ @i_nro_cg.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zsdt0131 INTO @DATA(lwa_0131)
     WHERE nro_lote EQ @lwa_0129-nro_lote.

    CHECK sy-subrc EQ 0 AND lwa_0131-cod_loc_emb IS NOT INITIAL.

    "Seleciona Cabeçalho Lote
    SELECT *
      FROM zsdt0130 INTO TABLE @DATA(lit_0130)
     WHERE nro_lote EQ @lwa_0129-nro_lote.

    CHECK lit_0130[] IS NOT INITIAL.

    SORT lit_0130 BY seq_ent_cg.
    DELETE ADJACENT DUPLICATES FROM lit_0130 COMPARING seq_ent_cg.

    lva_data_entrega = lwa_0129-dt_entrega+6(2) && '/' &&  lwa_0129-dt_entrega+4(2) && '/' &&  lwa_0129-dt_entrega(4).

    r_observacoes = |Data Estimada Entrega Final: { lva_data_entrega } |.

    CONCATENATE r_observacoes 'ATENÇÃO ROTEIRO ::::::: PONTO DE COLETA:' INTO r_observacoes SEPARATED BY space.

    "Roteiro Ponto Coleta
    wl_name_rot = lwa_0131-cod_loc_emb.

    CLEAR: it_texto[], lva_texto_rot.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id        = 'ZROT'
        language  = sy-langu
        name      = wl_name_rot
        object    = 'ZSDROTEIRO'
      TABLES
        lines     = it_texto
      EXCEPTIONS
        id        = 1
        language  = 2
        name      = 3
        not_found = 4
        OTHERS    = 5.

    LOOP AT it_texto INTO DATA(wa_texto).
      IF lva_texto_rot IS INITIAL.
        lva_texto_rot = wa_texto-tdline.
      ELSE.
        CONCATENATE lva_texto_rot wa_texto-tdline INTO lva_texto_rot SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    IF lva_texto_rot IS NOT INITIAL.
      r_observacoes = |{ r_observacoes } { lva_texto_rot }|.
    ENDIF.

    "Roteiro Local Entrega
    CONCATENATE r_observacoes '::::::: SEGUIR COM A ORDEM DE ENTREGA A SEGUIR:' INTO r_observacoes SEPARATED BY space.

    LOOP AT lit_0130 ASSIGNING FIELD-SYMBOL(<fs_0130>).
      CLEAR: it_texto[], lva_texto_rot.

      r_observacoes = |{ r_observacoes } Entrega { <fs_0130>-seq_ent_cg }:|.

      wl_name_rot = <fs_0130>-nr_rot.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id        = 'ZROT'
          language  = sy-langu
          name      = wl_name_rot
          object    = 'ZSDROTEIRO'
        TABLES
          lines     = it_texto
        EXCEPTIONS
          id        = 1
          language  = 2
          name      = 3
          not_found = 4
          OTHERS    = 5.

      LOOP AT it_texto INTO wa_texto.
        IF lva_texto_rot IS INITIAL.
          lva_texto_rot = wa_texto-tdline.
        ELSE.
          CONCATENATE lva_texto_rot wa_texto-tdline INTO lva_texto_rot SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF lva_texto_rot IS NOT INITIAL.
        r_observacoes = |{ r_observacoes } { lva_texto_rot }|.
      ENDIF.

    ENDLOOP.

    r_observacoes = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( r_observacoes ) ) ).
    REPLACE ALL OCCURRENCES OF ';' IN r_observacoes WITH ','.

  ENDMETHOD.


  METHOD get_spart_carga.

    CLEAR: r_spart.

    SELECT SINGLE *
      FROM zsdt0129 INTO @DATA(lwa_zmmt0129)
      WHERE nro_cg EQ @i_nro_carga.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zsdt0131 INTO @DATA(lwa_zmmt0131)
     WHERE nro_lote EQ @lwa_zmmt0129-nro_lote.

    CHECK sy-subrc EQ 0.

    r_spart = lwa_zmmt0131-spart.

  ENDMETHOD.


  METHOD get_texto_roteito.
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2LTT |27/06/2025 |Inserir os Campos Município e Texto do&*
*&                                    |Roteiro para Local de Embarque e Ponto&*
*&                                    |de Coleta Chamado: 169508.            &*
*&---------------------------------------------------------------------------&*

    DATA: tl_texto  TYPE STANDARD TABLE OF tline,
          tl_texto2 TYPE                   catsxt_longtext_itab.

    DATA: le_texto2 TYPE                   txline.

    DATA: vl_title  TYPE                   sytitle.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ZROT'
        language                = sy-langu
        name                    = i_name
        object                  = 'ZSDROTEIRO'
      TABLES
        lines                   = tl_texto
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc IS INITIAL.
* Verifica o tipo da execução.
      CASE i_tp_exec.
        WHEN sy-abcde+21(1). "V - Verifica Texto
          IF tl_texto IS INITIAL.
            r_valor = '@1F@'.

          ELSE.
            r_valor = '@1E@'.

          ENDIF.

        WHEN sy-abcde+4(1).  "E - Exibe Texto
          LOOP AT tl_texto INTO DATA(el_texto).
            MOVE   el_texto-tdline TO le_texto2.
            APPEND le_texto2       TO tl_texto2.

          ENDLOOP.

          DATA(vl_len) = strlen( i_name ).
          vl_title     = |Texto do Roteiro Nº { i_name(vl_len) }|.
* Tela de editor de texto em modo exibição
          CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
            EXPORTING
              im_title        = vl_title
              im_display_mode = abap_on
            CHANGING
              ch_text         = tl_texto2.

        WHEN OTHERS.
* Do nothing
      ENDCASE.

    ELSE.
      r_valor = '@1F@'.

    ENDIF.

  ENDMETHOD.


  METHOD gravar_carga.

    DATA: lwa_header_lote	        TYPE zsdt0129,
          lit_clientes_lote  	    TYPE zsdt0130_t,
          lit_ordens_lote	        TYPE zsdt0131_t,
          lwa_header_carga        TYPE zsdt0133,
          lit_notas_venda	        TYPE zsdt0410_t,
          lit_notas_transferencia	TYPE zsdt0410_t,
          lit_lotes              	TYPE zsdt0134_t.

    CLEAR: e_msg_erro.

    CLEAR: lwa_header_lote,  lit_clientes_lote[],  lit_ordens_lote[], lwa_header_carga, lit_notas_venda[], lit_notas_transferencia[], lit_lotes[].

*------------------------------------------------------------------------------------------------------------*
*   Montar Cabeçalho da Carga
*------------------------------------------------------------------------------------------------------------*

    lwa_header_carga-nro_cg                       = i_header-nro_cg.
    lwa_header_carga-cod_transportadora           = i_header-cod_transportadora.
    lwa_header_carga-ds_conjunto_transp           = i_header-ds_conjunto_transp.
    lwa_header_carga-preco_frete                  = i_header-preco_frete.
    lwa_header_carga-ctg_transp                   = i_header-ctg_transp.
    lwa_header_carga-qtd_total_kg                 = i_header-qtd_total_kg.
    lwa_header_carga-status                       = i_header-status.
    lwa_header_carga-armazem_org                  = i_header-armazem_org.
    lwa_header_carga-dt_autorizacao_embarque      = i_header-dt_autorizacao_embarque.
    lwa_header_carga-seq_autorizacao_embarque     = i_header-seq_autorizacao_embarque.
    lwa_header_carga-ds_url_file_carguero         = i_header-ds_url_file_carguero.
    lwa_header_carga-integrar_carguero            = i_header-integrar_carguero.
    lwa_header_carga-viagem_id                    = i_header-viagem_id.
    lwa_header_carga-pedido_luft_criado           = i_header-pedido_luft_criado.
    lwa_header_carga-nro_pedido_luft              = i_header-nro_pedido_luft.
    lwa_header_carga-docs_env_carguero            = i_header-docs_env_carguero.
    lwa_header_carga-dt_env_doc_cg                = i_header-dt_env_doc_cg.
    lwa_header_carga-hr_env_doc_cg                = i_header-hr_env_doc_cg.
    lwa_header_carga-us_env_doc_cg                = i_header-us_env_doc_cg.
    lwa_header_carga-id_lote_frete                = i_header-id_lote_frete.
    lwa_header_carga-carga_conferida              = i_header-carga_conferida.
    lwa_header_carga-id_carga_safra_control       = i_header-id_carga_safra_control.
    lwa_header_carga-versao_processo              = i_header-versao_processo.
    lwa_header_carga-carga_em_cotacao             = i_header-carga_em_cotacao.
    lwa_header_carga-dt_envio_cotacao             = i_header-dt_envio_cotacao.
    lwa_header_carga-dt_frete_contratado          = i_header-dt_frete_contratado.
    lwa_header_carga-carga_automatica             = i_header-carga_automatica.

    CASE abap_true.
      WHEN i_header-frete_por_t.
        lwa_header_carga-frete_por  = '1'.
      WHEN i_header-frete_por_v.
        lwa_header_carga-frete_por  = '2'.
    ENDCASE.

    lwa_header_carga-usnam                        = i_header-usnam.
    lwa_header_carga-data_atual                   = i_header-data_atual.
    lwa_header_carga-hora_atual                   = i_header-hora_atual.
    lwa_header_carga-user_edit                    = i_header-user_edit.
    lwa_header_carga-data_edit                    = i_header-data_edit.
    lwa_header_carga-hora_edit                    = i_header-hora_edit.


*------------------------------------------------------------------------------------------------------------*
*   Montar Cabeçalho Lote
*------------------------------------------------------------------------------------------------------------*

    lwa_header_lote-nro_lote              = i_header-nro_lote.
    lwa_header_lote-nro_cg                = i_header-nro_cg.
    lwa_header_lote-inco1                 = i_header-inco1.
    lwa_header_lote-ctg_transp            = i_header-ctg_transp.
    lwa_header_lote-qtd_total_kg          = i_header-qtd_total_kg.
    lwa_header_lote-placa_cav             = i_header-placa_cav.
    lwa_header_lote-placa_car1            = i_header-placa_car1.
    lwa_header_lote-placa_car2            = i_header-placa_car2.
    lwa_header_lote-placa_car3            = i_header-placa_car3.
    lwa_header_lote-motorista             = i_header-motorista.
    lwa_header_lote-dt_entrega            = i_header-dt_entrega.
    lwa_header_lote-status                = i_header-status.
    lwa_header_lote-armazem_org           = i_header-armazem_org.

    IF i_solicitacoes[] IS NOT INITIAL.
      CLEAR: lwa_header_carga-qtd_total_kg, lwa_header_lote-qtd_total_kg.
    ENDIF.

    LOOP AT i_solicitacoes ASSIGNING FIELD-SYMBOL(<solicitacoes>).

      lwa_header_carga-qtd_total_kg = lwa_header_carga-qtd_total_kg + <solicitacoes>-qtd_vinc_kg.
      lwa_header_lote-qtd_total_kg  = lwa_header_lote-qtd_total_kg  + <solicitacoes>-qtd_vinc_kg.

*------------------------------------------------------------------------------------------------------------*
*     Montar Solicitaçoes/Ordens do Lote
*------------------------------------------------------------------------------------------------------------*
      APPEND INITIAL LINE TO lit_ordens_lote ASSIGNING FIELD-SYMBOL(<fs_ordem_lote>).

      <fs_ordem_lote>-nro_lote                    = lwa_header_lote-nro_lote.
      <fs_ordem_lote>-nro_sol                     = <solicitacoes>-nro_sol.
      <fs_ordem_lote>-seq                         = <solicitacoes>-seq.
      <fs_ordem_lote>-kunnr                       = <solicitacoes>-kunnr.
      <fs_ordem_lote>-vbeln                       = <solicitacoes>-vbeln.
      <fs_ordem_lote>-posnr                       = <solicitacoes>-posnr.
      <fs_ordem_lote>-ebeln                       = <solicitacoes>-ebeln.
      <fs_ordem_lote>-ebelp                       = <solicitacoes>-ebelp.
      <fs_ordem_lote>-auart                       = <solicitacoes>-auart.
      <fs_ordem_lote>-spart                       = <solicitacoes>-spart.
      <fs_ordem_lote>-vkorg                       = <solicitacoes>-vkorg.
      <fs_ordem_lote>-vkbur                       = <solicitacoes>-vkbur.
      <fs_ordem_lote>-werks                       = <solicitacoes>-werks.
      <fs_ordem_lote>-matnr                       = <solicitacoes>-matnr.
      <fs_ordem_lote>-qtd_vinc                    = <solicitacoes>-qtd_vinc.
      <fs_ordem_lote>-um                          = <solicitacoes>-meins.
      <fs_ordem_lote>-brgew                       = <solicitacoes>-brgew.
      <fs_ordem_lote>-qtd_emkg                    = <solicitacoes>-qtd_vinc_kg.
      <fs_ordem_lote>-item_carga                  = <solicitacoes>-item_carga.
      <fs_ordem_lote>-seq_carregamento_luft       = <solicitacoes>-seq_carregamento_luft.
      <fs_ordem_lote>-dt_entrega_efetiva          = <solicitacoes>-dt_entrega_efetiva.
      <fs_ordem_lote>-cod_loc_emb                 = i_header-roteiro_pc.
      <fs_ordem_lote>-local_embarq                = i_header-rot_desc.
      <fs_ordem_lote>-status                      = <solicitacoes>-status.

*------------------------------------------------------------------------------------------------------------*
*     Montar Cliente do Lote
*------------------------------------------------------------------------------------------------------------*
      READ TABLE lit_clientes_lote WITH KEY nro_sol = <solicitacoes>-nro_sol
                                            seq     = <solicitacoes>-seq
                                            kunnr   = <solicitacoes>-kunnr TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO lit_clientes_lote ASSIGNING FIELD-SYMBOL(<fs_cliente_lote>).

        <fs_cliente_lote>-nro_lote      = lwa_header_lote-nro_lote.
        <fs_cliente_lote>-nro_sol       = <solicitacoes>-nro_sol.
        <fs_cliente_lote>-seq           = <solicitacoes>-seq.
        <fs_cliente_lote>-kunnr         = <solicitacoes>-kunnr.
        <fs_cliente_lote>-nr_rot        = <solicitacoes>-nr_rot.
        <fs_cliente_lote>-seq_entrega   = <solicitacoes>-seq_entrega.
        <fs_cliente_lote>-seq_ent_cg    = <solicitacoes>-seq_entrega.
        <fs_cliente_lote>-status        = <solicitacoes>-status.

      ENDIF.

    ENDLOOP.

    IF lwa_header_carga-qtd_total_kg IS NOT INITIAL.
      zcl_carga_saida_insumos=>calc_categoria_veiculo(
        EXPORTING
          i_quantidade_kg = CONV #( lwa_header_carga-qtd_total_kg )
        CHANGING
          c_ctg_transp    =  lwa_header_carga-ctg_transp   ).

      lwa_header_lote-ctg_transp  = lwa_header_carga-ctg_transp.
    ENDIF.

*------------------------------------------------------------------------------------------------------------*
*   Montar Notas Venda
*------------------------------------------------------------------------------------------------------------*
    MOVE-CORRESPONDING i_notas_venda[] TO lit_notas_venda[].

*------------------------------------------------------------------------------------------------------------*
*   Montar Notas Transferencia
*------------------------------------------------------------------------------------------------------------*

    MOVE-CORRESPONDING i_notas_transferencia[] TO lit_notas_transferencia[].

*------------------------------------------------------------------------------------------------------------*
*   Lotes Carga
*------------------------------------------------------------------------------------------------------------*

    MOVE-CORRESPONDING i_lotes[] TO lit_lotes[].

*------------------------------------------------------------------------------------------------------------*
*   Montar Frete Carga
*------------------------------------------------------------------------------------------------------------*


*------------------------------------------------------------------------------------------------------------*
*   Montar Cotação Carga
*------------------------------------------------------------------------------------------------------------*



*------------------------------------------------------------------------------------------------------------*
*   Gravar Carga
*------------------------------------------------------------------------------------------------------------*

    zcl_carga_saida_insumos=>gravar_carga_core(
      EXPORTING
        i_lote_ov_save              = i_lote_ov_save
        i_atualiza_dados_logistico  = i_atualiza_dados_logistico
        i_criacao_carga_safra_ctrl  = i_criacao_carga_safra_ctrl
      IMPORTING
        e_nro_carga                 = DATA(lva_nro_carga)
      CHANGING
        i_header_lote               = lwa_header_lote
        i_clientes_lote             = lit_clientes_lote
        i_ordens_lote               = lit_ordens_lote
        i_lotes                     = lit_lotes
        i_header_carga              = lwa_header_carga
        i_notas_venda               = lit_notas_venda
        i_notas_transferencia       = lit_notas_transferencia

      RECEIVING
        r_msg_error           = e_msg_erro ).


    CHECK lva_nro_carga IS NOT INITIAL.

    e_carga =  lva_nro_carga.

    zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga(
      EXPORTING
        i_nro_cg    = lva_nro_carga
        i_bloqueio  = abap_false ).

  ENDMETHOD.


  METHOD gravar_carga_core.

    DATA: lit_notas_removidas  TYPE zsdt0410_t.
    DATA: lit_zsdt0131_safra_current TYPE zsdt0131_t.

    DATA: lit_zsdt0134_old TYPE zsdt0134_t.

    CLEAR: r_msg_error.

    r_msg_error = zcl_carga_saida_insumos=>preparar_gravacao(
                     CHANGING
                     i_header_lote         = i_header_lote
                     i_clientes_lote       = i_clientes_lote
                     i_ordens_lote         = i_ordens_lote
                     i_header_carga        = i_header_carga
                     i_notas_venda         = i_notas_venda
                     i_lotes               = i_lotes
                     i_notas_transferencia = i_notas_transferencia ).

    CHECK r_msg_error IS INITIAL.

    DATA(_registro_new) = abap_false.

    IF i_header_lote-nro_lote IS INITIAL.

      _registro_new = abap_true.

      "Numero Lote
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZSD_INS_LT'
        IMPORTING
          number                  = i_header_lote-nro_lote
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF i_header_lote-nro_lote IS INITIAL.
        r_msg_error = | Não foi possivel gerar a numeração do Lote! Objeto ZSD_INS_LT Range: 01|.
        RETURN.
      ENDIF.

      "Numero Carga
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZSD_INS_CG'
        IMPORTING
          number                  = i_header_lote-nro_cg
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF i_header_lote-nro_cg IS INITIAL.
        r_msg_error = | Não foi possivel gerar a numeração da Carga! Objeto ZSD_INS_CG Range: 01|.
        RETURN.
      ENDIF.

      IF sy-batch EQ abap_false.
        READ TABLE i_ordens_lote INTO DATA(lwa_solicitacao) INDEX 1.

        IF i_criacao_carga_safra_ctrl	EQ abap_false.
          r_msg_error = zcl_carga_saida_insumos=>check_permissao_carga_core(
                          i_bukrs     = lwa_solicitacao-vkorg
                          i_atividade = '01' "Montar Carga
                          i_spart     = lwa_solicitacao-spart ).
        ENDIF.
      ENDIF.

      CHECK r_msg_error IS INITIAL.

    ENDIF.

    e_nro_carga =  i_header_lote-nro_cg.

    LOOP AT i_clientes_lote ASSIGNING FIELD-SYMBOL(<fs_cliente_lote>).
      <fs_cliente_lote>-nro_lote =  i_header_lote-nro_lote.
    ENDLOOP.

    LOOP AT i_ordens_lote ASSIGNING FIELD-SYMBOL(<fs_ordem_lote>).
      <fs_ordem_lote>-nro_lote =  i_header_lote-nro_lote.
    ENDLOOP.

    LOOP AT i_notas_venda ASSIGNING FIELD-SYMBOL(<fs_nota_venda>).
      <fs_nota_venda>-nro_cg = i_header_lote-nro_cg.
    ENDLOOP.

    LOOP AT i_notas_transferencia ASSIGNING FIELD-SYMBOL(<fs_nota_transf>).
      <fs_nota_transf>-nro_cg = i_header_lote-nro_cg.
    ENDLOOP.

    LOOP AT i_lotes ASSIGNING FIELD-SYMBOL(<fs_lote>).
      <fs_lote>-nro_cg = i_header_lote-nro_cg.
    ENDLOOP.

    IF i_header_carga IS NOT INITIAL.
      i_header_carga-nro_cg = i_header_lote-nro_cg.
    ENDIF.

**------------------------------------------------------------------------------------------------------------*
**   Recuperar Registros antes da Alteração para executar Roolback se necessário
**------------------------------------------------------------------------------------------------------------*
    IF _registro_new EQ abap_false.

      "Cabeçalho Lote
      SELECT SINGLE *
        FROM zsdt0129 INTO @DATA(lwa_zsdt0129_old)
       WHERE nro_lote EQ @i_header_lote-nro_lote.

      "Cabeçalho Carga
      SELECT SINGLE *
        FROM zsdt0133 INTO @DATA(lwa_zsdt0133_old)
       WHERE nro_cg EQ @i_header_lote-nro_cg.

      "Clientes Lote
      SELECT *
        FROM zsdt0130 INTO TABLE @DATA(lit_zsdt0130_old)
       WHERE nro_lote EQ @i_header_lote-nro_lote.

      "Ordens/Solicitações do Lote
      SELECT *
        FROM zsdt0131 INTO TABLE @DATA(lit_zsdt0131_old)
       WHERE nro_lote EQ @i_header_lote-nro_lote.

      "Notas da Carga
      SELECT *
        FROM zsdt0410 INTO TABLE @DATA(lit_zsdt0410_old)
       WHERE nro_cg EQ @i_header_lote-nro_cg.

      "Lotes da Carga
      SELECT *
        FROM zsdt0134 INTO TABLE @DATA(lit_zsdt0134_old_tmp)
       WHERE nro_cg EQ @lwa_zsdt0133_old-nro_cg.

      LOOP AT lit_zsdt0134_old_tmp INTO DATA(lwa_zsdt0134_old_tmp).

        READ TABLE lit_zsdt0131_old WITH KEY nro_lote = i_header_lote-nro_lote
                                             vbeln    = lwa_zsdt0134_old_tmp-vbeln
                                             posnr    = lwa_zsdt0134_old_tmp-posnr TRANSPORTING NO FIELDS.
        CHECK sy-subrc EQ 0.

        APPEND INITIAL LINE TO lit_zsdt0134_old ASSIGNING FIELD-SYMBOL(<fs_zsdt0134_out>).
        MOVE-CORRESPONDING lwa_zsdt0134_old_tmp TO <fs_zsdt0134_out>.
      ENDLOOP.

      DELETE lit_zsdt0134_old WHERE NOT ( vbeln  EQ i_lote_ov_save-vbeln AND
                                          posnr  EQ i_lote_ov_save-posnr AND
                                          nr_rot EQ i_lote_ov_save-nr_rot ).
    ENDIF.

**------------------------------------------------------------------------------------------------------------*
**   Gravar Cabeçalho Carga
**------------------------------------------------------------------------------------------------------------*

    IF i_header_carga IS NOT INITIAL.

      IF _registro_new EQ abap_true.
        i_header_carga-status      = '1'.
        i_header_carga-usnam       = sy-uname.
        i_header_carga-data_atual  = sy-datum.
        i_header_carga-hora_atual  = sy-uzeit.

        IF i_header_carga-cod_transportadora IS NOT INITIAL.
          i_header_carga-dt_frete_contratado  = sy-datum.
        ENDIF.

      ELSE.

        SELECT SINGLE *
          FROM zsdt0133 INTO @DATA(lwa_zsdt0133_exists)
          WHERE nro_cg EQ @i_header_carga-nro_cg.

        IF sy-subrc EQ 0.

          DATA(_dt_hr_register_current) = i_header_carga-data_edit && i_header_carga-hora_edit.
          DATA(_dt_hr_register_exists)  = lwa_zsdt0133_exists-data_edit && lwa_zsdt0133_exists-hora_edit.

          IF _dt_hr_register_current NE _dt_hr_register_exists.
            r_msg_error = 'Registro da Carga foi atualizado! Recarregue as informações!'.
            RETURN.
          ENDIF.

          "Campos Preenchimento do Sistema
          i_header_carga-ds_url_file_carguero             =  lwa_zsdt0133_exists-ds_url_file_carguero.
          i_header_carga-id_lote_frete                    =  lwa_zsdt0133_exists-id_lote_frete.
          i_header_carga-carga_conferida                  =  lwa_zsdt0133_exists-carga_conferida.
          i_header_carga-dt_conferencia                   =  lwa_zsdt0133_exists-dt_conferencia.
          i_header_carga-hr_conferencia                   =  lwa_zsdt0133_exists-hr_conferencia.
          i_header_carga-us_conferencia                   =  lwa_zsdt0133_exists-us_conferencia.
          i_header_carga-doc_bordero_anexado              =  lwa_zsdt0133_exists-doc_bordero_anexado.
          i_header_carga-doc_fase_anexado                 =  lwa_zsdt0133_exists-doc_fase_anexado.
          i_header_carga-doc_certificado_anexado          =  lwa_zsdt0133_exists-doc_certificado_anexado.
          i_header_carga-doc_outros_anexado               =  lwa_zsdt0133_exists-doc_outros_anexado.
          i_header_carga-doc_nfe_anexado                  =  lwa_zsdt0133_exists-doc_nfe_anexado.
          i_header_carga-viagem_aprovada_carguero         =  lwa_zsdt0133_exists-viagem_aprovada_carguero.
          i_header_carga-integracao_pendente              =  lwa_zsdt0133_exists-integracao_pendente.
          i_header_carga-msg_integracao_pendente          =  lwa_zsdt0133_exists-msg_integracao_pendente.
          i_header_carga-int_transp_safra_ctrl            =  lwa_zsdt0133_exists-int_transp_safra_ctrl.
          i_header_carga-int_mot_safra_ctrl               =  lwa_zsdt0133_exists-int_mot_safra_ctrl.
          i_header_carga-int_dados_transp_safra_ctrl      =  lwa_zsdt0133_exists-int_dados_transp_safra_ctrl.
          i_header_carga-int_anexos_aut_emb_safra_ctrl    =  lwa_zsdt0133_exists-int_anexos_aut_emb_safra_ctrl.
          i_header_carga-int_anexos_nfe_safra_ctrl        =  lwa_zsdt0133_exists-int_anexos_nfe_safra_ctrl.
          i_header_carga-docs_env_carguero                =  lwa_zsdt0133_exists-docs_env_carguero.
          i_header_carga-dt_env_doc_cg                    =  lwa_zsdt0133_exists-dt_env_doc_cg.
          i_header_carga-hr_env_doc_cg                    =  lwa_zsdt0133_exists-hr_env_doc_cg.
          i_header_carga-us_env_doc_cg                    =  lwa_zsdt0133_exists-us_env_doc_cg.

          "Data Frete Contratado
          IF lwa_zsdt0133_exists-cod_transportadora IS INITIAL AND i_header_carga-cod_transportadora IS NOT INITIAL AND
             lwa_zsdt0133_exists-dt_frete_contratado IS INITIAL. "LES - Job Integração ZSDT0112 x Carguero US 190812 - JT

            lwa_zsdt0133_exists-dt_frete_contratado = sy-datum.
**<<<------"169665 - NMS - INI------>>>
            IF i_header_carga-dt_frete_contratado IS INITIAL.
              i_header_carga-dt_frete_contratado = sy-datum.
            ENDIF.
**<<<------"169665 - NMS - FIM------>>>
          ENDIF.

        ENDIF.

        i_header_carga-user_edit   = sy-uname.
        i_header_carga-data_edit   = sy-datum.
        i_header_carga-hora_edit   = sy-uzeit.

      ENDIF.

      i_header_carga-versao_processo = '02'.

      MODIFY zsdt0133 FROM i_header_carga.

    ENDIF.

**------------------------------------------------------------------------------------------------------------*
**   Gravar Cabeçalho do Lote
**------------------------------------------------------------------------------------------------------------*
    IF i_header_lote IS NOT INITIAL.

      IF _registro_new EQ abap_true.
        i_header_lote-status      = '2'.
      ENDIF.

      i_header_lote-usnam       = sy-uname.
      i_header_lote-data_atual  = sy-datum.
      i_header_lote-hora_atual  = sy-uzeit.

      MODIFY zsdt0129 FROM i_header_lote.
    ENDIF.

**------------------------------------------------------------------------------------------------------------*
**   Gravar Clientes do Lote
**------------------------------------------------------------------------------------------------------------*

    IF i_atualiza_dados_logistico EQ abap_false AND i_lote_ov_save IS INITIAL.

      IF i_clientes_lote[] IS NOT INITIAL.

        LOOP AT i_clientes_lote ASSIGNING <fs_cliente_lote>.

          IF _registro_new EQ abap_true.
            <fs_cliente_lote>-status      = '2'.
          ENDIF.

          <fs_cliente_lote>-usnam      = sy-uname.
          <fs_cliente_lote>-data_atual = sy-datum.
          <fs_cliente_lote>-hora_atual = sy-uzeit.
        ENDLOOP.

        MODIFY zsdt0130 FROM TABLE i_clientes_lote.
      ENDIF.

      "Identificar Clientes Removidos
      LOOP AT lit_zsdt0130_old INTO DATA(lwa_zsdt0130_old) WHERE status NE 'X'.
        READ TABLE i_clientes_lote WITH KEY nro_lote = lwa_zsdt0130_old-nro_lote
                                            nro_sol  = lwa_zsdt0130_old-nro_sol
                                            seq      = lwa_zsdt0130_old-seq
                                            kunnr    = lwa_zsdt0130_old-kunnr TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          lwa_zsdt0130_old-status    = 'X'.
          MODIFY zsdt0130 FROM lwa_zsdt0130_old.
        ENDIF.
      ENDLOOP.

    ENDIF.

**------------------------------------------------------------------------------------------------------------*
**   Gravar Notas da Carga
**------------------------------------------------------------------------------------------------------------*

    IF i_atualiza_dados_logistico EQ abap_false AND i_lote_ov_save IS INITIAL.

      "Notas Vendas
      IF i_notas_venda[] IS NOT INITIAL.

        LOOP AT i_notas_venda ASSIGNING <fs_nota_venda>.

          <fs_nota_venda>-processo = '2'.

          READ TABLE lit_zsdt0410_old INTO DATA(lwa_zsdt0410_old)
                                      WITH KEY chave_nfe = <fs_nota_venda>-chave_nfe
                                               processo  = '2'
                                               cancel    = abap_false.

          CASE sy-subrc.
            WHEN 0.
              <fs_nota_venda>-user_create = lwa_zsdt0410_old-user_create.
              <fs_nota_venda>-date_create = lwa_zsdt0410_old-date_create.
              <fs_nota_venda>-time_create = lwa_zsdt0410_old-time_create.
              <fs_nota_venda>-user_change = sy-uname.
              <fs_nota_venda>-date_change = sy-datum.
              <fs_nota_venda>-time_change = sy-uzeit.
            WHEN OTHERS.
              <fs_nota_venda>-user_create = sy-uname.
              <fs_nota_venda>-date_create = sy-datum.
              <fs_nota_venda>-time_create = sy-uzeit.
          ENDCASE.

        ENDLOOP.

        MODIFY zsdt0410 FROM TABLE i_notas_venda.

      ENDIF.

      "Notas Transferencia
      IF i_notas_transferencia[] IS NOT INITIAL.

        LOOP AT i_notas_transferencia ASSIGNING <fs_nota_transf>.

          <fs_nota_transf>-processo = '1'.

          READ TABLE lit_zsdt0410_old INTO lwa_zsdt0410_old
                                      WITH KEY chave_nfe = <fs_nota_transf>-chave_nfe
                                               processo  = '1'
                                               cancel    = abap_false.

          CASE sy-subrc.
            WHEN 0.
              <fs_nota_venda>-user_create = lwa_zsdt0410_old-user_create.
              <fs_nota_venda>-date_create = lwa_zsdt0410_old-date_create.
              <fs_nota_venda>-time_create = lwa_zsdt0410_old-time_create.
              <fs_nota_transf>-user_change = sy-uname.
              <fs_nota_transf>-date_change = sy-datum.
              <fs_nota_transf>-time_change = sy-uzeit.
            WHEN OTHERS.
              <fs_nota_transf>-user_create = sy-uname.
              <fs_nota_transf>-date_create = sy-datum.
              <fs_nota_transf>-time_create = sy-uzeit.
          ENDCASE.

        ENDLOOP.

        MODIFY zsdt0410 FROM TABLE i_notas_transferencia.

      ENDIF.

      "Identificar Notas Removidas
      LOOP AT lit_zsdt0410_old ASSIGNING FIELD-SYMBOL(<fs_nota_exists_carga>) WHERE cancel EQ abap_false.
        DATA(_achou_nota) = abap_false.

        READ TABLE i_notas_venda WITH KEY chave_nfe = <fs_nota_exists_carga>-chave_nfe TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          _achou_nota = abap_true.
        ENDIF.

        READ TABLE i_notas_transferencia WITH KEY chave_nfe = <fs_nota_exists_carga>-chave_nfe TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          _achou_nota = abap_true.
        ENDIF.

        IF _achou_nota EQ abap_false.
          APPEND INITIAL LINE TO lit_notas_removidas ASSIGNING FIELD-SYMBOL(<fs_nota_removida>).
          MOVE-CORRESPONDING <fs_nota_exists_carga> TO <fs_nota_removida>.

          <fs_nota_removida>-cancel      = abap_true.
          <fs_nota_removida>-user_cancel = sy-uname.
          <fs_nota_removida>-date_cancel = sy-datum.
          <fs_nota_removida>-time_cancel = sy-uzeit.
        ENDIF.
      ENDLOOP.

      IF lit_notas_removidas[] IS NOT INITIAL .
        MODIFY zsdt0410 FROM TABLE lit_notas_removidas.
      ENDIF.

    ENDIF.

**------------------------------------------------------------------------------------------------------------*
**   Gravar Solicitações do Carga
**------------------------------------------------------------------------------------------------------------*

    IF i_atualiza_dados_logistico EQ abap_false AND i_lote_ov_save IS INITIAL.

      IF i_ordens_lote[] IS NOT INITIAL.

        LOOP AT i_ordens_lote ASSIGNING <fs_ordem_lote>.

          IF _registro_new EQ abap_true.
            <fs_ordem_lote>-status      = '2'.
          ENDIF.

          <fs_ordem_lote>-usnam      = sy-uname.
          <fs_ordem_lote>-data_atual = sy-datum.
          <fs_ordem_lote>-hora_atual = sy-uzeit.

          SELECT SINGLE *
            FROM zsdt0082  INTO @DATA(lwa_zsdt0082)
           WHERE nro_sol EQ @<fs_ordem_lote>-nro_sol
             AND seq     EQ @<fs_ordem_lote>-seq
             AND vbeln   EQ @<fs_ordem_lote>-vbeln
             AND posnr   EQ @<fs_ordem_lote>-posnr.

          IF sy-subrc NE 0.
            ROLLBACK WORK.
            r_msg_error = |Solicitação { <fs_ordem_lote>-nro_sol } Seq. { <fs_ordem_lote>-seq } não encontrada!|.
            zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = e_nro_carga i_bloqueio = abap_false ).
            RETURN.
          ENDIF.

          MODIFY zsdt0131 FROM <fs_ordem_lote>. "*-US190744-16.09.2025-#190744-wpp


*         lwa_zsdt0082-status = '5'.         "*-US190744-16.09.2025-#190744-JT-inicio
*         MODIFY zsdt0082 FROM lwa_zsdt0082. "*-US190744-16.09.2025-#190744-JT-inicio

*-US190744-16.09.2025-#190744-JT-inicio
          zcl_carga_saida_insumos=>consolida_status_solicitacao( i_nro_sol = <fs_ordem_lote>-nro_sol
                                                                 i_seq     = <fs_ordem_lote>-seq
                                                                 i_vbeln   = <fs_ordem_lote>-vbeln
                                                                 i_posnr   = <fs_ordem_lote>-posnr ).
*-US190744-16.09.2025-#190744-JT-fim
        ENDLOOP.

        "MODIFY zsdt0131 FROM TABLE i_ordens_lote. "*-US190744-16.09.2025-#190744-wpp

      ENDIF.

      "Identificar Solicitações Removidos
      LOOP AT lit_zsdt0131_old INTO DATA(lwa_zsdt0131_old) WHERE status NE 'X'.
        READ TABLE i_ordens_lote WITH KEY nro_lote = lwa_zsdt0131_old-nro_lote
                                          nro_sol  = lwa_zsdt0131_old-nro_sol
                                          seq      = lwa_zsdt0131_old-seq
                                          kunnr    = lwa_zsdt0131_old-kunnr
                                          vbeln    = lwa_zsdt0131_old-vbeln
                                          posnr    = lwa_zsdt0131_old-posnr TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          lwa_zsdt0131_old-status    = 'X'.
          lwa_zsdt0131_old-user_canc = sy-uname.
          lwa_zsdt0131_old-dt_canc   = sy-datum.
          lwa_zsdt0131_old-hr_can    = sy-uzeit.
          MODIFY zsdt0131 FROM lwa_zsdt0131_old.

*-US190744-16.09.2025-#190744-JT-inicio
          zcl_carga_saida_insumos=>consolida_status_solicitacao( i_nro_sol = lwa_zsdt0131_old-nro_sol
                                                                 i_seq     = lwa_zsdt0131_old-seq
                                                                 i_vbeln   = lwa_zsdt0131_old-vbeln
                                                                 i_posnr   = lwa_zsdt0131_old-posnr ).
*-US190744-16.09.2025-#190744-JT-fim
        ENDIF.

      ENDLOOP.

    ENDIF.

**------------------------------------------------------------------------------------------------------------*
**   Gravar Lotes Carga
**------------------------------------------------------------------------------------------------------------*

    IF i_atualiza_dados_logistico EQ abap_false.

      LOOP AT i_lotes ASSIGNING <fs_lote>.
        <fs_lote>-data_atual  = sy-datum.
        <fs_lote>-hora_atual  = sy-uzeit.
        <fs_lote>-usnam       = sy-uname.
        MODIFY zsdt0134 FROM <fs_lote>.
      ENDLOOP.

      "Identificar Lotes Removidos
      LOOP AT lit_zsdt0134_old INTO DATA(lwa_zsdt0134_old) WHERE status NE 'X'.
        READ TABLE i_lotes WITH KEY nro_cg = lwa_zsdt0134_old-nro_cg
                                    vbeln  = lwa_zsdt0134_old-vbeln
                                    posnr  = lwa_zsdt0134_old-posnr
                                    nr_rot = lwa_zsdt0134_old-nr_rot
                                    charg  = lwa_zsdt0134_old-charg TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          lwa_zsdt0134_old-status    = 'X'.
          lwa_zsdt0134_old-user_canc = sy-uname.
          lwa_zsdt0134_old-dt_canc   = sy-datum.
          lwa_zsdt0134_old-hr_can    = sy-uzeit.
          MODIFY zsdt0134 FROM lwa_zsdt0134_old.
        ENDIF.
      ENDLOOP.

    ENDIF.

**------------------------------------------------------------------------------------------------------------*
**  Integração com Safra Control
**------------------------------------------------------------------------------------------------------------*
    DATA(_integracao_safra_ctrl_exec) = abap_false.

    IF i_criacao_carga_safra_ctrl EQ abap_false AND i_lote_ov_save IS INITIAL AND i_atualiza_dados_logistico EQ abap_false.

      "Recupera solicitações Atuais(pós alteração) da Carga
      SELECT *
        FROM zsdt0131 INTO TABLE @DATA(lit_zsdt0131_new)
       WHERE nro_lote EQ @i_header_lote-nro_lote.

      LOOP AT lit_zsdt0131_new INTO DATA(lwa_zsdt0131_new).

        SELECT SINGLE *
          FROM zsdt0082  INTO lwa_zsdt0082
         WHERE nro_sol EQ lwa_zsdt0131_new-nro_sol
           AND seq     EQ lwa_zsdt0131_new-seq
           AND vbeln   EQ lwa_zsdt0131_new-vbeln
           AND posnr   EQ lwa_zsdt0131_new-posnr.

        IF sy-subrc NE 0.
          r_msg_error = |Solicitação { lwa_zsdt0131_new-nro_sol } Seq. { lwa_zsdt0131_new-seq } não encontrada!|.
          RETURN.
        ENDIF.

        IF lwa_zsdt0082-carga_automatica EQ abap_true.
          APPEND lwa_zsdt0131_new TO lit_zsdt0131_safra_current.
        ENDIF.

      ENDLOOP.

      IF lit_zsdt0131_safra_current[] IS NOT INITIAL.

        "Validar Quantidades Safra Control
        zcl_carga_saida_insumos=>val_qtde_sol_safra_control(
          EXPORTING
            i_solicitacoes_current = CONV #( lit_zsdt0131_safra_current )
            i_solicitacoes_old     = CONV #( lit_zsdt0131_old )
            i_header_carga         = i_header_carga
          IMPORTING
            e_solicitacoes_safra   = DATA(lit_solicitacoes_safra)
          RECEIVING
            r_msg_error            = DATA(_msg_error_safra_control)
        ).

        IF _msg_error_safra_control IS INITIAL.
          "Atualizar Quantidades Safra Control
          _msg_error_safra_control =
             zcl_carga_saida_insumos=>atualiza_sol_safra_control(
              EXPORTING
                i_solicitacoes_current = CONV #( lit_zsdt0131_safra_current )
                i_header_carga         = i_header_carga
                i_solicitacoes_safra   = lit_solicitacoes_safra ).
        ENDIF.

        IF _msg_error_safra_control IS NOT INITIAL.
          zcl_carga_saida_insumos=>rollback_carga(
            i_nro_cg         = i_header_carga-nro_cg
            i_nro_lote       = i_header_lote-nro_lote
            i_zsdt0133_old   = lwa_zsdt0133_old
            i_zsdt0129_old   = lwa_zsdt0129_old
            i_zsdt0130_t_old = CONV #( lit_zsdt0130_old )
            i_zsdt0131_t_old = CONV #( lit_zsdt0131_old )
            i_zsdt0410_t_old = CONV #( lit_zsdt0410_old )
            i_zsdt0134_t_old = CONV #( lit_zsdt0134_old )
            i_lotes          = i_lotes
            i_registro_new   = _registro_new ).

          r_msg_error = |Manutenção de Solicitações no Safra Control não possivel: Motivo: { _msg_error_safra_control }|.
          RETURN.
        ENDIF.

        _integracao_safra_ctrl_exec = abap_true.
        COMMIT WORK.

      ENDIF.

    ENDIF.

**------------------------------------------------------------------------------------------------------------*
**   Integrar ao Carguero
**------------------------------------------------------------------------------------------------------------*
    IF i_atualiza_dados_logistico IS INITIAL AND i_lote_ov_save IS INITIAL.

      DATA(lva_msg_error_carguero) = zcl_carga_saida_insumos=>gerar_lote_embarcador_carguero( i_nro_cg = i_header_lote-nro_cg ).

      IF lva_msg_error_carguero IS NOT INITIAL.

        r_msg_error = |Erro ao criar/atualizar Lote Embarcador no Carguero! Motivo: { lva_msg_error_carguero }|.

        IF _integracao_safra_ctrl_exec EQ abap_false. "Se já Integrou com safra, não pode dar mais rollback
          zcl_carga_saida_insumos=>rollback_carga(
              i_nro_cg         = i_header_carga-nro_cg
              i_nro_lote       = i_header_lote-nro_lote
              i_zsdt0133_old   = lwa_zsdt0133_old
              i_zsdt0129_old   = lwa_zsdt0129_old
              i_zsdt0130_t_old = CONV #( lit_zsdt0130_old )
              i_zsdt0131_t_old = CONV #( lit_zsdt0131_old )
              i_zsdt0410_t_old = CONV #( lit_zsdt0410_old )
              i_zsdt0134_t_old = CONV #( lit_zsdt0134_old )
              i_lotes          = i_lotes
              i_registro_new   = _registro_new ).
        ENDIF.

        DATA(_msg_error_integracao) = 'Error ao atualizar Lote Embarcador no Carguero! Favor Editar e Salvar Solicitações da Carga novamente!'.
        UPDATE zsdt0133 SET integracao_pendente     = abap_true
                            msg_integracao_pendente = _msg_error_integracao
         WHERE nro_cg EQ i_header_carga-nro_cg.

        MESSAGE |Houve um erro integrar a Carga ao Carguero/Strada! Msg: { lva_msg_error_carguero }| TYPE 'S'.
        RETURN.
      ENDIF.

    ENDIF.

    IF r_msg_error IS NOT INITIAL.
      ROLLBACK WORK.
      MESSAGE |Houve um erro salvar a Carga! Msg: { r_msg_error }| TYPE 'S'.
      RETURN.
    ELSE.
      UPDATE zsdt0133 SET integracao_pendente     = abap_false
                          msg_integracao_pendente = space
       WHERE nro_cg EQ i_header_carga-nro_cg.

      COMMIT WORK.

      MESSAGE |Carga { i_header_lote-nro_cg } gravada com sucesso!| TYPE 'S'.
    ENDIF.


  ENDMETHOD.


  METHOD habilitar_edicao_chaves.

    CLEAR: r_msg_error.

    SELECT SINGLE *
      FROM zsdt0133 INTO @DATA(lwa_zsdt0133)
      WHERE nro_cg EQ @i_nro_carga.

    IF sy-subrc NE 0 OR i_nro_carga IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_zsdt0133-dt_autorizacao_embarque IS INITIAL.
      r_msg_error = 'A carga sem autorização de Embarque! Operação não permitida!'.
      RETURN.
    ENDIF.

    IF lwa_zsdt0133-carga_conferida IS NOT INITIAL.
      r_msg_error = 'A carga já foi conferida! Operação não permitida!'.
      RETURN.
    ENDIF.

    DATA(_embarque_luft) = zcl_carga_saida_insumos=>get_embarque_luft( EXPORTING i_nro_cg =  I_NRO_CARGA ).
    IF _embarque_luft eq abap_true.
      r_msg_error = 'Embarque LUFT! Operação não permitida!'.
      RETURN.
    ENDIF.

    r_msg_error = zcl_carga_saida_insumos=>check_permissao_carga(
        EXPORTING
           i_nro_carga = i_nro_carga
           i_atividade = '07' ).

    CHECK r_msg_error IS INITIAL.

    r_msg_error = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING
                                                                          i_nro_cg    = i_nro_carga
                                                                          i_bloqueio  = abap_true ).

  ENDMETHOD.


  METHOD habilitar_edicao_dados_logist.

    CLEAR: r_msg_error.

    SELECT SINGLE *
      FROM zsdt0133 INTO @DATA(lwa_zsdt0133)
     WHERE nro_cg EQ @i_nro_carga.

    IF sy-subrc NE 0 OR i_nro_carga IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_zsdt0133-integrar_carguero EQ abap_true.
      r_msg_error = |Carga com integração com Carguero! Operação não permitida|.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0410 INTO @DATA(lwa_zsdt0410)
     WHERE nro_cg EQ @lwa_zsdt0133-nro_cg
       AND cancel EQ @abap_false.

    IF sy-subrc EQ 0.
      r_msg_error = 'Carga já possui NF-e informada! Operaçao não permitida!'.
      RETURN.
    ENDIF.

    r_msg_error = zcl_carga_saida_insumos=>check_permissao_carga(
        EXPORTING
           i_nro_carga = i_nro_carga
           i_atividade = '04' ).

    CHECK r_msg_error IS INITIAL.

    r_msg_error = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga(
                    i_nro_cg   = i_nro_carga
                    i_bloqueio = abap_true ).

    CHECK r_msg_error IS INITIAL.

    r_msg_error = zcl_carga_saida_insumos=>check_reinicializacao_aut_emb(
      EXPORTING
        i_nro_carga = i_nro_carga
    ).

    IF r_msg_error IS NOT INITIAL.
      zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga(
                  i_nro_cg   = i_nro_carga
                  i_bloqueio = abap_false ).
    ENDIF.


  ENDMETHOD.


  METHOD habilitar_edicao_lotes.

    CLEAR: r_msg_error.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single =  i_nro_carga
      IMPORTING
        e_cargas          =  DATA(lit_carga)
        e_romaneios       =  DATA(lit_romaneios)
        e_lotes           =  DATA(lit_lotes)
        e_notas_venda     =  DATA(lit_notas)
    ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lit_romaneios[] is NOT INITIAL.
      r_msg_error = |Carga com romaneios gerados! Operação não permitida!|.
      RETURN.
    ENDIF.

    DATA(_preencher_lote_manual) = zcl_carga_saida_insumos=>get_preenchimento_lote_manual( i_nro_cg = i_nro_carga ).

    IF _preencher_lote_manual eq abap_false.
      r_msg_error = |Aba Lotes da Carga são preenchidos automaticamente na geração do romaneio! Operação não permitida!|.
      RETURN.
    ENDIF.

    DATA(_embarque_armazem) = zcl_carga_saida_insumos=>check_embarque_armazem( i_nro_cg = i_nro_carga  ).
    IF _embarque_armazem EQ  abap_false.
      IF lwa_carga-dt_autorizacao_embarque IS INITIAL.
        r_msg_error = 'Ponto de Coleta não é um Armazem/Filial! Necessário gerar uma autorização de embarque! Operação não permitida!'.
        RETURN.
      ENDIF.
**<<<------"169508 - NMS - INI------>>>
*      if lit_notas[] is NOT INITIAL.
      IF lit_notas[] IS INITIAL.
**<<<------"169508 - NMS - FIM------>>>
        r_msg_error = 'Ponto de Coleta não é um Armazem/Filial! Necessário informar Notas de Compra! Operação não permitida!'.
        RETURN.
      ENDIF.
    ENDIF.

    r_msg_error = zcl_carga_saida_insumos=>check_permissao_carga(
      EXPORTING
         i_nro_carga = i_nro_carga
         i_atividade = '11' ).

    CHECK r_msg_error IS INITIAL.

    r_msg_error = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING
                                                                          i_nro_cg    = i_nro_carga
                                                                          i_bloqueio  = abap_true ).

  ENDMETHOD.


  METHOD habilitar_edicao_solicitacoes.

    CLEAR: r_msg_error.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single =  i_nro_carga
      IMPORTING
        e_cargas          =  DATA(lit_carga)
        e_romaneios       =  DATA(lit_romaneios)
        e_lotes           =  DATA(lit_lotes)
    ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS NOT INITIAL.
      r_msg_error = 'Carga com romaneios gerados! Operação não permitida!'.
      RETURN.
    ENDIF.

    IF lit_lotes[] IS NOT INITIAL.
      r_msg_error = 'Carga com lotes informados! Operação não permitida!'.
      RETURN.
    ENDIF.

    IF lwa_carga-carga_conferida IS NOT INITIAL.
      r_msg_error = 'A carga já foi conferida! Operação não permitida!'.
      RETURN.
    ENDIF.

    r_msg_error = zcl_carga_saida_insumos=>check_permissao_carga(
        EXPORTING
           i_nro_carga = i_nro_carga
           i_atividade = '03' ).

    CHECK r_msg_error IS INITIAL.

    r_msg_error = zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga(
                    i_nro_cg   = i_nro_carga
                    i_bloqueio = abap_true ).

    CHECK r_msg_error IS INITIAL.

    SELECT SINGLE *
      FROM zsdt0410 INTO @DATA(lwa_zsdt0410)
     WHERE nro_cg EQ @i_nro_carga
       AND cancel EQ @abap_false.

    IF sy-subrc NE 0. "Não possui NF-e
      r_msg_error = zcl_carga_saida_insumos=>check_reinicializacao_aut_emb(
        EXPORTING
          i_nro_carga = i_nro_carga
      ).

      IF r_msg_error IS NOT INITIAL.
        zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga(
                 i_nro_cg   = i_nro_carga
                 i_bloqueio = abap_false ).
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD lista_status_carga.
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2LTT |02/07/2025 |Implementação Status de processamento &*
*&                                    |da Carga - INSUMOS. Chamado: 169508.  &*
*&---------------------------------------------------------------------------&*

* Declaração de Classes.
    DATA: lcl_table   TYPE REF TO cl_salv_table,
          lcl_display TYPE REF TO cl_salv_display_settings,
          lcl_columns TYPE REF TO cl_salv_columns_table,
          lcl_column  TYPE REF TO cl_salv_column_table.

    DATA:
      BEGIN OF le_txt_fld,
        tx_short  TYPE scrtext_s,
        tx_medium TYPE scrtext_m,
        tx_long   TYPE scrtext_l,
      END   OF le_txt_fld.

    CLEAR: r_status_carga[].

    SELECT domvalue_l valpos ddtext
      FROM dd07t
      INTO TABLE r_status_carga
    WHERE domname    EQ 'ZSDD_STS_CARGA'
      AND ddlanguage EQ 'P'.

    IF sy-subrc IS INITIAL.
      LOOP AT r_status_carga ASSIGNING FIELD-SYMBOL(<fs_legenda>).
        <fs_legenda>-id = COND icon_d( WHEN <fs_legenda>-status EQ '0' OR
                                            <fs_legenda>-status EQ '1'    THEN '@9K@'
                                       WHEN <fs_legenda>-status EQ '2'    THEN '@5D@'
                                       WHEN <fs_legenda>-status EQ '3'    THEN '@FD@'
                                       WHEN <fs_legenda>-status EQ '4'    THEN '@4A@'
                                       WHEN <fs_legenda>-status EQ '5'    THEN '@96@'
                                       WHEN <fs_legenda>-status EQ '6'    THEN '@0Q@'
                                       WHEN <fs_legenda>-status EQ '7'    THEN '@01@'
                                       WHEN <fs_legenda>-status EQ 'A'    THEN '@KB@'
                                       WHEN <fs_legenda>-status EQ 'B'    THEN '@9T@'
                                       WHEN <fs_legenda>-status EQ 'C'    THEN '@B4@'
                                       WHEN <fs_legenda>-status EQ 'D'    THEN '@VX@'
                                      ).

      ENDLOOP.

      SORT r_status_carga BY valpos.

      CHECK i_show_status EQ abap_true.

      DATA(vl_qtlns) = lines( r_status_carga ).
*... Create Instance
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lcl_table
        CHANGING
          t_table      = r_status_carga[].
*... Defifine as Popup.
      CALL METHOD lcl_table->set_screen_popup
        EXPORTING
          start_column = 15
          end_column   = 55
          start_line   = 2
          end_line     = CONV #( vl_qtlns ).
* ... Implement ZEBRA.
      lcl_display = lcl_table->get_display_settings( ).
      lcl_display->set_striped_pattern( abap_true ).
      lcl_display->set_striped_pattern( abap_true ).
      lcl_columns = lcl_table->get_columns( ).
      lcl_columns->set_optimize( abap_true ).
      lcl_display->set_list_header( 'Legenga da Carga' ).
*... Sequeência do valor do domínio.
      lcl_column ?= lcl_columns->get_column( 'VALPOS' ).
      lcl_column->set_visible( abap_false ).
*... ID do Ícone.
      le_txt_fld-tx_short = le_txt_fld-tx_medium = le_txt_fld-tx_long = 'ID Carga'.
      lcl_column ?= lcl_columns->get_column( 'ID' ).
      lcl_column->set_short_text( le_txt_fld-tx_short ).
      lcl_column->set_medium_text( le_txt_fld-tx_medium ).
      lcl_column->set_long_text( le_txt_fld-tx_long ).
*... Descrição do Ícone.
      le_txt_fld-tx_short = le_txt_fld-tx_medium = le_txt_fld-tx_long = 'Descrição'.
      lcl_column ?= lcl_columns->get_column( 'DESC' ).
      lcl_column->set_short_text( le_txt_fld-tx_short ).
      lcl_column->set_medium_text( le_txt_fld-tx_medium ).
      lcl_column->set_long_text( le_txt_fld-tx_long ).
*... Display table in ALV Grid
      lcl_table->display( ).

    ELSE.
      MESSAGE |Não há dados de legenda para serem exibidos| TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.

  ENDMETHOD.


  METHOD preparar_gravacao.

    DATA: lit_ov_lotes TYPE zsds381_t.

    DATA: lit_notas_carga TYPE zsdt0410_t.
    DATA: lra_nr_sol TYPE RANGE OF zsdt0131-nro_sol.
    DATA: lva_origem_estoque TYPE zsdt0082-origem_estoque.
    DATA: lva_lifnr_pc TYPE lfa1-lifnr.

    DATA: lva_qtde_bag TYPE zmmt0201-qtd_total_kg.

    DATA: lva_qtde_itens_kg  TYPE zmmt0201-qtd_total_kg.
    DATA: lva_qtde_vinc_lote TYPE zsdt0134-lfimg.
    DATA: lva_qtde_ov_rot    TYPE zsdt0134-lfimg.
    "DATA: lit_seq_entrega_ov_rot TYPE ZSDS381_t.

    CLEAR: r_msg_error, lit_notas_carga[], lit_ov_lotes[].

    DATA: lit_ordens_safra TYPE zsdt0131_t.

    "Check se é Embarque Armazem
    DATA(_embarque_armazem) = zcl_carga_saida_insumos=>check_embarque_armazem( i_nro_cg = i_header_carga-nro_cg ).

**------------------------------------------------------------------------------------------------------------*
**  Validações Cabeçalho Lote
**------------------------------------------------------------------------------------------------------------*

    IF i_header_lote-nro_lote IS NOT INITIAL AND i_header_lote-nro_cg IS INITIAL.
      r_msg_error = 'Nro de Lote gerado e numero da carga não informado! Operação não permitida!'.
      RETURN.
    ENDIF.

    IF i_header_carga-qtd_total_kg IS INITIAL.
      r_msg_error = 'Os campos: Qtd Total Kg são obrigatórios'.
      RETURN.
    ENDIF.

    IF i_header_carga-id_carga_safra_control IS NOT INITIAL.
      SELECT SINGLE *
        FROM zsdt0133 INTO @DATA(lwa_zsdt0133)
       WHERE id_carga_safra_control EQ @i_header_carga-id_carga_safra_control
         AND nro_cg                 NE @i_header_carga-nro_cg
         AND status                 NE 'X'.

      IF sy-subrc EQ 0.
        r_msg_error = |Já existe a carga nro: { lwa_zsdt0133-nro_cg } criada para o identificador Safra: { i_header_carga-id_carga_safra_control }|.
        RETURN.
      ENDIF.
    ENDIF.


    IF i_header_lote-qtd_total_kg IS INITIAL OR
*       i_header_lote-inco1 IS INITIAL OR "// US-169528 - WBARBOSA ZLES0136
       i_header_lote-dt_entrega IS INITIAL.

*      r_msg_error = 'Os campos: Qtd Total Kg,Tipo Frete, Data Entrega são obrigatórios'. "// US-169528 - WBARBOSA ZLES0136
      r_msg_error = 'Os campos: Qtd Total Kg, Data Entrega são obrigatórios'. "// US-169528 - WBARBOSA ZLES0136
      RETURN.

    ENDIF.

    IF i_header_carga-cod_transportadora IS NOT INITIAL.
      SELECT SINGLE *
        FROM lfa1 INTO @DATA(lwa_lfa1_transp)
       WHERE lifnr EQ @i_header_carga-cod_transportadora.

      IF sy-subrc NE 0.
        r_msg_error = |Transportadora com codigo { i_header_carga-cod_transportadora } não encontrada!|.
        RETURN.
      ENDIF.

      IF lwa_lfa1_transp-ktokk = 'ZFIC' AND  i_header_lote-inco1 EQ 'CPT'.
        r_msg_error = |Transportadora { i_header_carga-cod_transportadora } é intercompany! Tipo de Frete não pode ser CPT!|.
        RETURN.
      ENDIF.

      IF lwa_lfa1_transp-ktokk <> 'ZFIC' AND  i_header_lote-inco1 EQ 'CIF'.
        r_msg_error = |Transportadora { i_header_carga-cod_transportadora } é terceira! Tipo de Frete não pode ser CIF!|.
        RETURN.
      ENDIF.
    ENDIF.

    "Ficará trava via parametro
*    IF i_header_lote-placa_cav IS NOT INITIAL AND sy-sysid = 'PRD'.
*
*      SELECT SINGLE nrocg, placacav
*        FROM zcds_lista_cargas INTO @DATA(lwa_zmmt0201_open)
*        WHERE placacav         EQ @i_header_lote-placa_cav
*        AND nrocg              NE @i_header_lote-nro_cg
*        AND cancel             EQ @abap_false
*        AND status             LT '11'. "Status anterior a "CARGA CONFERIDA TOTAL"
*
*      IF sy-subrc EQ 0.
*        r_msg_error = |Existe uma Carga de Entrada não conferida para a Placa Cavalo: { lwa_zmmt0201_open-placacav }! Nro Carga: { lwa_zmmt0201_open-nrocg }!|.
*        RETURN.
*      ENDIF.
*
*      SELECT SINGLE nro_cg, placa_cav
*        FROM zsdt0129 AS a INTO @DATA(lwa_zsdt0133_open)
*       WHERE placa_cav  EQ @i_header_lote-placa_cav
*         AND a~nro_cg    NE @i_header_lote-nro_cg
*         AND EXISTS ( SELECT nro_cg
*                        FROM zsdt0133 AS b
*                       WHERE b~nro_cg          EQ a~nro_cg
*                         AND b~user_canc       EQ @abap_false
*                         AND b~carga_conferida EQ @abap_false
*                         AND b~versao_processo NE '00' ).
*      IF sy-subrc EQ 0.
*        r_msg_error = |Existe uma Carga de Saida não conferida para a Placa Cavalo: { lwa_zsdt0133_open-placa_cav }! Nro Carga: { lwa_zsdt0133_open-nro_cg }!|.
*        RETURN.
*      ENDIF.
*
*    ENDIF.

**------------------------------------------------------------------------------------------------------------*
**  Validações Solicitações Carga
**------------------------------------------------------------------------------------------------------------*

    IF i_header_lote-nro_lote IS NOT INITIAL.
      SELECT MAX( item_carga )
        FROM zsdt0131 INTO @DATA(lva_max_item_carga)
       WHERE nro_lote EQ @i_header_lote-nro_lote.
    ELSE.
      lva_max_item_carga = 0.
    ENDIF.

    IF i_ordens_lote[] IS NOT INITIAL.

      CLEAR: lva_qtde_itens_kg.

      LOOP AT i_ordens_lote ASSIGNING FIELD-SYMBOL(<fs_sol>).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_sol>-nro_sol ) TO lra_nr_sol.
      ENDLOOP.

      zcl_carga_saida_insumos=>busca_dados_montar_carga(
         EXPORTING
           i_nro_sol               = lra_nr_sol
           i_tipo_saldo            = 'T'
           i_nro_cg_no_check_saldo = i_header_lote-nro_cg
         IMPORTING
           e_tabela_monta_carga = DATA(lit_solicitacoes_saldo) ).

      DATA(lit_ordens_lote_aux) = i_ordens_lote[].


      CLEAR: lva_origem_estoque.
      LOOP AT i_ordens_lote ASSIGNING FIELD-SYMBOL(<fs_item>).

        IF <fs_item>-status = abap_true.
          CONTINUE.
        ENDIF.

        DATA(_count_item_duplicate) = 0.
        LOOP AT lit_ordens_lote_aux INTO DATA(lwa_ordem_lote) WHERE nro_sol       = <fs_item>-nro_sol
                                                                AND seq           = <fs_item>-seq
                                                                AND vbeln         = <fs_item>-vbeln
                                                                AND posnr         = <fs_item>-posnr.
          ADD 1 TO _count_item_duplicate.
        ENDLOOP.

        IF _count_item_duplicate > 1.
          r_msg_error = |Carga com Solicitações duplicadas! Nro Sol { <fs_item>-nro_sol } Item Sol: { <fs_item>-seq }| .
          RETURN.
        ENDIF.

        IF <fs_item>-qtd_vinc <= 0.
          r_msg_error = 'Quantidade Vinculada da Solicitação na Carga não pode ser 0'.
          RETURN.
        ENDIF.

        IF <fs_item>-qtd_emkg <= 0.
          r_msg_error = 'Quantidade Vinculada KG da Solicitação na Carga não pode ser 0'.
          RETURN.
        ENDIF.

        IF <fs_item>-item_carga IS INITIAL.
          ADD 1 TO lva_max_item_carga.
          <fs_item>-item_carga = lva_max_item_carga.
        ENDIF.


*        SELECT SINGLE matnr , brgew
*          FROM mara INTO @DATA(lwa_mara)
*          WHERE matnr = @<fs_item>-matnr.
*
*        IF sy-subrc NE 0 OR <fs_item>-matnr IS INITIAL OR lwa_mara-brgew IS INITIAL.
*          r_msg_error = |Peso do Material da Solicitação Nro { <fs_item>-nro_sol } Item Sol: { <fs_item>-seq } não encontrado| .
*          RETURN.
*        ENDIF.
*
*        IF <fs_item>-qtd_emkg < lwa_mara-brgew.
*          r_msg_error = |Nro { <fs_item>-nro_sol } Item Sol: { <fs_item>-seq } com quantidade vinculada inferior ao peso do material { lwa_mara-brgew }| .
*          RETURN.
*        ENDIF.

        IF <fs_item>-cod_loc_emb IS INITIAL.
          r_msg_error = 'Roteiro Ponto Coleta não informado a Carga'.
          RETURN.
        ENDIF.


        SELECT SINGLE *
          FROM zsdt0132 INTO @DATA(lwa_zsdt0132_pc)
          WHERE nr_rot EQ @<fs_item>-cod_loc_emb.

        IF sy-subrc NE 0 .
          r_msg_error = |Roteiro Ponto Coleta: { <fs_item>-cod_loc_emb } não encontrado| .
          RETURN.
        ENDIF.

        IF lwa_zsdt0132_pc-lifnr IS INITIAL.
          r_msg_error = |Roteiro: { <fs_item>-cod_loc_emb } não é de um fornecedor| .
          RETURN.
        ENDIF.

        lva_lifnr_pc = lwa_zsdt0132_pc-lifnr.

        READ TABLE lit_solicitacoes_saldo INTO DATA(lwa_sol_saldo) WITH KEY nro_sol       = <fs_item>-nro_sol
                                                                            seq           = <fs_item>-seq
                                                                            vbeln         = <fs_item>-vbeln
                                                                            posnr         = <fs_item>-posnr.

        IF sy-subrc IS NOT INITIAL.
          r_msg_error = | { 'Solicitação:' } { <fs_item>-nro_sol } { 'Item:' } { <fs_item>-seq } { 'já não possui mais saldo' } | .
          RETURN.
        ELSE.
          IF <fs_item>-qtd_vinc > lwa_sol_saldo-saldo.
            r_msg_error = | { 'Solicitação:' } { <fs_item>-nro_sol } { 'Item:' } { <fs_item>-seq } { 'possui saldo atual de' } { lwa_sol_saldo-saldo } | .
            RETURN.
          ENDIF.
        ENDIF.

        IF lwa_sol_saldo-origem_estoque IS INITIAL.
          r_msg_error = |Solicitação { <fs_item>-nro_sol } Seq. { <fs_item>-seq } sem origem de estoque definida!|.
          RETURN.
        ENDIF.

        IF lva_origem_estoque IS INITIAL.
          lva_origem_estoque = lwa_sol_saldo-origem_estoque.
        ELSEIF lva_origem_estoque NE lwa_sol_saldo-origem_estoque.
          r_msg_error = |Solicitações da Carga com Origens de estoque distintas!|.
          RETURN.
        ENDIF.

        SELECT SINGLE *
          FROM zsdt0082  INTO @DATA(lwa_zsdt0082)
         WHERE nro_sol EQ @<fs_item>-nro_sol
           AND seq     EQ @<fs_item>-seq
           AND vbeln   EQ @<fs_item>-vbeln
           AND posnr   EQ @<fs_item>-posnr.

        IF sy-subrc NE 0.
          r_msg_error = |Solicitação { <fs_item>-nro_sol } Seq. { <fs_item>-seq } não encontrada!|.
          RETURN.
        ENDIF.

        ADD <fs_item>-qtd_emkg TO lva_qtde_itens_kg.

        IF lwa_zsdt0082-carga_automatica = abap_true.
          APPEND <fs_item> TO lit_ordens_safra.
        ENDIF.

        READ TABLE i_clientes_lote INTO DATA(lwa_cliente_lote_check) WITH KEY nro_sol = lwa_sol_saldo-nro_sol
                                                                              seq     = lwa_sol_saldo-seq
                                                                              kunnr   = lwa_sol_saldo-kunnr.
        IF sy-subrc NE 0.
          r_msg_error = |Cliente Lote Solicitação{ lwa_sol_saldo-nro_sol } Seq. { lwa_sol_saldo-seq } não encontrada!|.
          RETURN.
        ENDIF.

        APPEND INITIAL LINE TO lit_ov_lotes ASSIGNING FIELD-SYMBOL(<fs_ov_lote>).
        <fs_ov_lote>-vbeln       = lwa_zsdt0082-vbeln.
        <fs_ov_lote>-posnr       = lwa_zsdt0082-posnr.
        <fs_ov_lote>-nr_rot      = lwa_zsdt0082-nr_rot.
        <fs_ov_lote>-seq_entrega = lwa_cliente_lote_check-seq_entrega.
        <fs_ov_lote>-qtd_vinc    = <fs_item>-qtd_vinc.

      ENDLOOP.



      SELECT SINGLE *
        FROM tvarvc INTO @DATA(lwa_limite_carga_saida)
        WHERE name EQ 'PESO_LIMITE_CARGA_INSUMOS'.

      IF sy-subrc EQ 0 AND lwa_limite_carga_saida-low IS NOT INITIAL.
        IF lva_qtde_itens_kg > lwa_limite_carga_saida-low.
          r_msg_error = |Peso total dos itens, é maior que peso limite de { lwa_limite_carga_saida-low }|.
          RETURN.
        ENDIF.
      ENDIF.

      DATA(_lit_ordens_aux) = i_ordens_lote[].
      SORT _lit_ordens_aux BY cod_loc_emb.
      DELETE ADJACENT DUPLICATES FROM _lit_ordens_aux COMPARING cod_loc_emb.

      IF lines( _lit_ordens_aux[] ) > 1.
        r_msg_error = |Carga mais mais de um Local de Embarque! Operação não permitida!|.
        RETURN.
      ENDIF.

      READ TABLE _lit_ordens_aux INTO DATA(lwa_ordem_aux) INDEX 1.

      DATA(_emb_armazem)  = zcl_carga_saida_insumos=>check_embarque_armazem( EXPORTING i_roteiro_pc = CONV #( lwa_ordem_aux-cod_loc_emb )  ).
      DATA(_emb_luft)     = zcl_carga_saida_insumos=>get_embarque_luft( EXPORTING i_roteiro_pc = CONV #( lwa_ordem_aux-cod_loc_emb ) ).

      CASE lva_origem_estoque.
        WHEN '1'. "Filial

          IF _emb_armazem EQ abap_false.
            r_msg_error = |Solicitação da Carga com Origem Estoque Fiilial, porém Local de Embarque não está configurado como Armazém! Operação não permitida!|.
            RETURN.
          ENDIF.

          TRY.
              zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( EXPORTING i_parceiro = lva_lifnr_pc
                                                                             )->ck_parceiro_local_negocio( IMPORTING e_j_1bbranch = DATA(lwa_branch) ).
            CATCH zcx_parceiros. " Erro Parceiros
              r_msg_error = |Código Fornecedor { lva_lifnr_pc }  deve ser uma filial|.
              RETURN.
          ENDTRY.

          LOOP AT i_ordens_lote ASSIGNING <fs_item>.
            IF <fs_item>-werks NE lwa_branch-branch.
              r_msg_error = |Nro Sol { <fs_item>-nro_sol } Item: { <fs_item>-seq } deve ter o centro de faturamento { lwa_branch-branch }. Centro Solicitação { <fs_item>-werks }| .
              RETURN.
            ENDIF.
          ENDLOOP.

        WHEN '2'. "Fornecedor
          IF _emb_armazem EQ abap_true.
            r_msg_error = |Solicitação da Carga com Origem Estoque Fornecedor, porém Local de Embarque está configurado como Armazém! Operação não permitida!|.
            RETURN.
          ENDIF.
        WHEN '3'. "Armazem


          IF _emb_armazem EQ abap_false.
            r_msg_error = |Solicitação da Carga com Origem Estoque Armazem, porém Local de Embarque não está configurado como Armazém! Operação não permitida!|.
            RETURN.
          ENDIF.

          IF _emb_luft EQ abap_true.
            r_msg_error = |Solicitação da Carga com Origem Estoque Armazem, porém Local de Embarque está configurado como LUFT! Operação não permitida!|.
            RETURN.
          ENDIF.

        WHEN OTHERS.
          r_msg_error = |Carga com origem de estoque:{ lva_origem_estoque } desconhecido!|.
          RETURN.
      ENDCASE.

    ENDIF.


*------------------------------------------------------------------------------------------------------------*
*  Validações Cliente Carga
*------------------------------------------------------------------------------------------------------------*

    LOOP AT i_clientes_lote INTO DATA(lwa_cliente_lote).

      IF lwa_cliente_lote-nro_sol IS INITIAL OR
         lwa_cliente_lote-seq IS INITIAL.
        r_msg_error = |Solicitação e Sequencia Sol. não foi informada! Clientes Lotes| .
        RETURN.
      ENDIF.

      IF lwa_cliente_lote-seq_ent_cg IS INITIAL OR
         lwa_cliente_lote-seq_entrega IS INITIAL.
        r_msg_error = |Sequencia Sol.: { lwa_cliente_lote-nro_sol } Seq: { lwa_cliente_lote-seq  } sem sequencia de entrega informada| .
        RETURN.
      ENDIF.

      IF lwa_cliente_lote-nr_rot IS INITIAL.
        r_msg_error = |Sequencia Sol.: { lwa_cliente_lote-nro_sol } Seq: { lwa_cliente_lote-seq  } sem Roteiro de entrega informado| .
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0132 INTO @DATA(lwa_zsdt0132)
        WHERE nr_rot EQ @lwa_cliente_lote-nr_rot.

      IF sy-subrc NE 0 .
        r_msg_error = |Roteiro entrega: { lwa_cliente_lote-nr_rot } não encontrado| .
        RETURN.
      ENDIF.

      IF lwa_zsdt0132-kunnr IS INITIAL.
        r_msg_error = |Roteiro: { lwa_cliente_lote-nr_rot } não é de um cliente| .
        RETURN.
      ENDIF.

      IF lwa_cliente_lote-kunnr IS INITIAL.
        r_msg_error = |Cliente nãofoi informado! Clientes Lotes| .
        RETURN.
      ENDIF.


    ENDLOOP.


*------------------------------------------------------------------------------------------------------------*
*  Validações Notas Carga
*------------------------------------------------------------------------------------------------------------*

    IF ( i_ordens_lote[] IS INITIAL ) AND
       ( i_notas_venda[] IS NOT INITIAL OR i_notas_transferencia[] IS NOT INITIAL ).
      r_msg_error = 'Carga não possui ordens/solicitações, portanto não pode ser informada notas na carga!'.
      RETURN.
    ENDIF.

    APPEND LINES OF i_notas_venda TO lit_notas_carga.
    APPEND LINES OF i_notas_transferencia TO lit_notas_carga.

    DATA(lit_notas_carga_aux) = lit_notas_carga[].

    LOOP AT lit_notas_carga ASSIGNING FIELD-SYMBOL(<fs_nota>).

      IF <fs_nota>-chave_nfe IS INITIAL.
        r_msg_error = |Chave NF-e não informada!|.
        RETURN.
      ENDIF.

      DATA(_count_duplicidade) = 0.
      LOOP AT lit_notas_carga_aux INTO DATA(lwa_nota_aux) WHERE chave_nfe = <fs_nota>-chave_nfe.
        ADD 1 TO _count_duplicidade.
      ENDLOOP.

      IF _count_duplicidade > 1.
        r_msg_error = |Chave NF-e { <fs_nota>-chave_nfe } duplicada!|.
        RETURN.
      ENDIF.

      IF strlen( <fs_nota>-chave_nfe ) NE 44 AND <fs_nota>-chave_nfe IS NOT INITIAL.
        r_msg_error = |Tamanho chave NF-e { <fs_nota>-chave_nfe } incorreto!|.
        RETURN.
      ENDIF.

      IF strlen( <fs_nota>-chave_cte ) NE 44 AND <fs_nota>-chave_cte IS NOT INITIAL.
        r_msg_error = |Tamanho chave CT-e { <fs_nota>-chave_cte } incorreto!|.
        RETURN.
      ENDIF.

    ENDLOOP.

**------------------------------------------------------------------------------------------------------------*
**  Validações Lotes Carga
**------------------------------------------------------------------------------------------------------------*

    DATA(i_lotes_aux) = i_lotes[].

    LOOP AT i_lotes ASSIGNING FIELD-SYMBOL(<fs_lote>).

      IF <fs_lote>-vbeln IS INITIAL.
        r_msg_error = |Aba Lote/ Ordem é um campo obrigatorio!|.
        RETURN.
      ENDIF.

      IF <fs_lote>-posnr IS INITIAL.
        r_msg_error = |Aba Lote/ Item Ordem é um campo obrigatorio!|.
        RETURN.
      ENDIF.

      IF <fs_lote>-charg   IS INITIAL.
        r_msg_error = |Aba Lote/ Campo Lote é um campo obrigatorio!|.
        RETURN.
      ENDIF.

      IF <fs_lote>-lfimg IS INITIAL.
        r_msg_error = |Aba Lote/ Campo Quantidade é um campo obrigatorio!|.
        RETURN.
      ENDIF.

      IF <fs_lote>-brgew IS INITIAL.
        r_msg_error = |Aba Lote/ Peso Bruto Material é um campo obrigatorio!|.
        RETURN.
      ENDIF.

      IF <fs_lote>-peso_liq_brt IS INITIAL.
        r_msg_error = |Aba Lote/ Peso Bruto Calculado é um campo obrigatorio!|.
        RETURN.
      ENDIF.

      READ TABLE i_ordens_lote INTO lwa_ordem_lote WITH KEY vbeln = <fs_lote>-vbeln
                                                            posnr = <fs_lote>-posnr.
      IF sy-subrc NE 0.
        r_msg_error = |Aba Lote/ Ordem { <fs_lote>-vbeln } e item { <fs_lote>-posnr } não pertence a carga!|.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0082  INTO @DATA(lwa_zsdt0082_check)
       WHERE nro_sol EQ @lwa_ordem_lote-nro_sol
         AND seq     EQ @lwa_ordem_lote-seq
         AND vbeln   EQ @lwa_ordem_lote-vbeln
         AND posnr   EQ @lwa_ordem_lote-posnr.

      IF sy-subrc NE 0.
        r_msg_error = |Solicitação { lwa_ordem_lote-nro_sol } Seq. { lwa_ordem_lote-seq } não encontrada!|.
        RETURN.
      ENDIF.
*
*      IF _embarque_armazem EQ abap_true.
*        CASE lwa_zsdt0082_check-flexibilidade.
*          WHEN '1'. "Permite Alteração Marca e Lote
*          WHEN '2'. "Manter Marca
*
*            zcl_charg=>get_valor_caracteristica(
*               EXPORTING
*                  iv_nome_attr = 'SEMENTE_MARCAS'
*                  iv_matnr     = CONV #( lwa_ordem_lote-matnr )
*                  iv_charg     = CONV #( <fs_lote>-charg )
*               IMPORTING
*                  e_atwrt      = DATA(lva_marca_semente)   ).
*
*            IF lwa_zsdt0082_check-marca NE lva_marca_semente.
*              r_msg_error = |Solicitação com Marca da Semente { lwa_zsdt0082_check-marca } e Carga com Marca { lva_marca_semente }|.
*              RETURN.
*            ENDIF.
*
*          WHEN '3'. "Manter Lote e Marca
*            IF lwa_zsdt0082_check-charg NE <fs_lote>-charg.
*              r_msg_error = |Solicitação { lwa_zsdt0082_check-nro_sol } com Lote { lwa_zsdt0082_check-charg } e Lote na Carga { <fs_lote>-charg }|.
*              RETURN.
*            ENDIF.
*        ENDCASE.
*      ENDIF.

      READ TABLE i_clientes_lote INTO lwa_cliente_lote WITH KEY nro_sol = lwa_ordem_lote-nro_sol
                                                                seq     = lwa_ordem_lote-seq.
      IF sy-subrc NE 0.
        r_msg_error = |Aba Lote/ Sol { lwa_ordem_lote-nro_sol } Seq { lwa_ordem_lote-seq } não encontrado!|.
        RETURN.
      ENDIF.

      <fs_lote>-nr_rot      = lwa_cliente_lote-nr_rot.

      IF <fs_lote>-nr_rot IS INITIAL.
        r_msg_error = |Aba Lote/ Roteiro é um campo obrigatorio!|.
        RETURN.
      ENDIF.

      "Validar Exigencia FASE
      DATA(_exige_fase) = abap_false.

      "Validar Cliente MT
      SELECT SINGLE *
        FROM kna1 INTO @DATA(lwa_kna1)
       WHERE kunnr EQ @lwa_cliente_lote-kunnr.

      IF sy-subrc EQ 0 AND lwa_kna1-regio EQ 'MT'.
        _exige_fase = abap_true.
      ENDIF.

      "Validar Ponto Embarque MT
      IF _exige_fase = abap_false.
        SELECT SINGLE *
          FROM zsdt0132 INTO @DATA(lwa_zsdt0132_check)
         WHERE nr_rot EQ @lwa_zsdt0082_check-nr_rot_pc.

        IF sy-subrc EQ 0 AND lwa_zsdt0132_check-lifnr IS NOT INITIAL.
          SELECT SINGLE *
            FROM lfa1 INTO @DATA(lwa_lfa1)
           WHERE lifnr EQ @lwa_zsdt0132_check-lifnr.

          IF sy-subrc EQ 0 AND lwa_lfa1-regio EQ 'MT'.
            _exige_fase = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.

      "Validar Centro Fornecedor MT
      IF _exige_fase = abap_false.

        DATA: lifnr_werks TYPE lfa1-lifnr.

        lifnr_werks = lwa_zsdt0082_check-werks.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lifnr_werks
          IMPORTING
            output = lifnr_werks.

        SELECT SINGLE *
          FROM lfa1 INTO @lwa_lfa1
         WHERE lifnr EQ @lifnr_werks.

        IF sy-subrc EQ 0 AND lwa_lfa1-regio EQ 'MT'.
          _exige_fase = abap_true.
        ENDIF.
      ENDIF.

      IF _exige_fase EQ abap_true.
        IF <fs_lote>-categoria IS INITIAL.
          r_msg_error = |Aba Lote/ Categoria é um campo obrigatorio!|.
          RETURN.
        ENDIF.

        IF <fs_lote>-nr_fase IS INITIAL.
          r_msg_error = |Aba Lote/ Campo Fase é um campo obrigatorio!|.
          RETURN.
        ENDIF.
      ENDIF.

      DATA(_count_charg_duplicate) = 0.
      LOOP AT i_lotes_aux INTO DATA(lwa_lote_aux) WHERE vbeln   = <fs_lote>-vbeln
                                                    AND posnr   = <fs_lote>-posnr
                                                    AND nr_rot  = <fs_lote>-nr_rot
                                                    AND charg   = <fs_lote>-charg
                                                    AND nr_fase = <fs_lote>-nr_fase.
        ADD 1 TO _count_charg_duplicate.
      ENDLOOP.

      IF _count_charg_duplicate > 1.
        r_msg_error = |Aba Lote/ Lote { <fs_lote>-charg } Fase: { <fs_lote>-nr_fase } duplicado para a OV: { <fs_lote>-vbeln } Item { <fs_lote>-posnr } Roterio: { <fs_lote>-nr_rot }!|.
        RETURN.
      ENDIF.

    ENDLOOP.

    DATA(_lit_ordens_carga_check) = i_ordens_lote[].

    LOOP AT _lit_ordens_carga_check INTO DATA(lwa_ordem_check).
      CLEAR: lva_qtde_vinc_lote, lva_qtde_ov_rot.

      READ TABLE i_clientes_lote INTO lwa_cliente_lote WITH KEY nro_sol = lwa_ordem_check-nro_sol
                                                                seq     = lwa_ordem_check-seq
                                                                kunnr   = lwa_ordem_check-kunnr.

      CHECK sy-subrc EQ 0.

      LOOP AT i_lotes INTO DATA(lwa_lote) WHERE vbeln  = lwa_ordem_check-vbeln
                                            AND posnr  = lwa_ordem_check-posnr
                                            AND nr_rot = lwa_cliente_lote-nr_rot.
        ADD lwa_lote-lfimg TO lva_qtde_vinc_lote.
      ENDLOOP.

      LOOP AT lit_ov_lotes INTO DATA(lwa_ov_lote) WHERE vbeln  EQ lwa_ordem_check-vbeln
                                                    AND posnr  EQ lwa_ordem_check-posnr
                                                    AND nr_rot EQ lwa_cliente_lote-nr_rot.
        ADD lwa_ov_lote-qtd_vinc TO lva_qtde_ov_rot.
      ENDLOOP.

      IF lva_qtde_vinc_lote GT lva_qtde_ov_rot.
        r_msg_error = |Aba Lote/ Total de Lotes vinculados para a Ordem: { lwa_ordem_check-vbeln } Item: { lwa_ordem_check-posnr } Roteiro: { lwa_cliente_lote-nr_rot }, foi excedido!|.
        r_msg_error = |{ r_msg_error } Qtde Limite: { lva_qtde_ov_rot } - Qtde Vinc. Lote { lva_qtde_vinc_lote } |.
        RETURN.
      ENDIF.

      "Checa sequencia entrega diferente para mesma OV Item Roteiro.
      DATA(_lit_ov_aux) = lit_ov_lotes[].
      DELETE _lit_ov_aux WHERE NOT (     vbeln  = lwa_ordem_check-vbeln
                                     AND posnr  = lwa_ordem_check-posnr
                                     AND nr_rot = lwa_cliente_lote-nr_rot ).

      SORT _lit_ov_aux BY vbeln posnr nr_rot seq_entrega.
      DELETE ADJACENT DUPLICATES FROM _lit_ov_aux COMPARING vbeln posnr nr_rot seq_entrega.
      IF lines( _lit_ov_aux[] ) > 1.
        r_msg_error = |Ordem: { lwa_ordem_check-vbeln } Item: { lwa_ordem_check-posnr } Roteiro: { lwa_cliente_lote-nr_rot } contem mais de uma sequencia de entrega!|.
        RETURN.
      ENDIF.

      "Checa mesma sequencia entrega com Roteiros diferentes.
      _lit_ov_aux = lit_ov_lotes[].
      DELETE _lit_ov_aux WHERE NOT ( seq_entrega = lwa_cliente_lote-seq_entrega ).

      SORT _lit_ov_aux BY seq_entrega nr_rot.
      DELETE ADJACENT DUPLICATES FROM _lit_ov_aux COMPARING seq_entrega nr_rot.
      IF lines( _lit_ov_aux[] ) > 1.
        r_msg_error = |Sequencia Entrega: { lwa_cliente_lote-seq_entrega } associada a mais de um roteiro de entrega!|.
        RETURN.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD validar_geracao_romaneios.

    CLEAR: r_msg_error.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single = i_nro_cg
      IMPORTING
        e_cargas          = DATA(lit_cargas)
        e_solicitacoes    = DATA(lit_solicitacoes)
        e_romaneios       = DATA(lit_romaneios)
        e_lotes           = DATA(lit_lotes)
    ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_cargas[] IS INITIAL.
      r_msg_error = 'Carga não encontrada!'.
      RETURN.
    ENDIF.

    IF lwa_carga-integracao_pendente eq abap_true.
      r_msg_error = lwa_carga-msg_integracao_pendente.
      RETURN.
    ENDIF.

    IF lwa_carga-carga_conferida IS INITIAL.
      r_msg_error = 'Carga não foi conferida!'.
      RETURN.
    ENDIF.

    IF lit_solicitacoes[] IS INITIAL.
      r_msg_error = 'Carga sem solicitações!'.
      RETURN.
    ENDIF.

    IF lit_lotes[] IS INITIAL.
      r_msg_error = 'Carga sem lotes informado! Operação não permitida!'.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS NOT INITIAL.
      r_msg_error = 'Carga com romaneios gerados! Operação não permitida!'.
      RETURN.
    ENDIF.

    SELECT *
      FROM vbpa INTO TABLE @DATA(lit_vbpa)
      FOR ALL ENTRIES IN @lit_solicitacoes
      WHERE vbeln EQ @lit_solicitacoes-vbeln
      AND ( parvw EQ 'PC' OR parvw EQ 'AG' ).

    LOOP AT lit_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao>).

      IF <fs_solicitacao>-tot_vinc_ov NE <fs_solicitacao>-qtde_lote_vinc_ov.
        r_msg_error = |Falta vinculação de Lotes de Produto para a Ordem: { <fs_solicitacao>-vbeln } Item: { <fs_solicitacao>-posnr } Roteiro: { <fs_solicitacao>-nr_rot }  | .
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM zi_sd_dados_compl_ov_info INTO @DATA(lwa_dados_comp_ov)
       WHERE vbeln EQ @<fs_solicitacao>-vbeln.

      IF sy-subrc NE 0 OR lwa_dados_comp_ov-nro_sol IS INITIAL.
        r_msg_error = |Simulador da OV { <fs_solicitacao>-vbeln } não encontrado!|.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0040 INTO @DATA(lwa_zsdt0040)
       WHERE doc_simulacao EQ @lwa_dados_comp_ov-nro_sol.

      IF sy-subrc NE 0 OR lwa_zsdt0040-safra IS INITIAL.
        r_msg_error = |Safra do Simulador { lwa_dados_comp_ov-nro_sol } não encontrado!|.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0136 INTO @DATA(lwa_zsdt0136)
       WHERE werks EQ @<fs_solicitacao>-werks
         AND safra EQ @lwa_zsdt0040-safra.

      IF sy-subrc IS NOT INITIAL.
        r_msg_error = |Falta range de números para romaneios no Centro { <fs_solicitacao>-werks } e Safra { lwa_zsdt0040-safra }|.
        RETURN.
      ENDIF.

      READ TABLE lit_vbpa INTO DATA(wa_vbpa) WITH KEY vbeln = <fs_solicitacao>-vbeln
                                                      parvw = 'PC'.
      IF sy-subrc NE 0.
        r_msg_error = |Ponto de Coleta da OV { <fs_solicitacao>-vbeln } não encontrado!|.
        RETURN.
      ENDIF.

      READ TABLE lit_vbpa INTO wa_vbpa WITH KEY vbeln = <fs_solicitacao>-vbeln
                                                parvw = 'AG'.
      IF sy-subrc NE 0.
        r_msg_error = |Cliente da OV { <fs_solicitacao>-vbeln } não encontrado!|.
        RETURN.
      ENDIF.


    ENDLOOP.

  ENDMETHOD.


  METHOD set_status_carga.

    DATA: lra_nr_carga TYPE zsdt_range_nro_cg.

    CHECK i_zsdt0133[] IS NOT INITIAL.

    LOOP AT i_zsdt0133 ASSIGNING FIELD-SYMBOL(<fs_zsdt0133_sel>).
      APPEND INITIAL LINE TO lra_nr_carga ASSIGNING FIELD-SYMBOL(<fs_lra_carga>).
      <fs_lra_carga>-sign   = 'I'.
      <fs_lra_carga>-option = 'EQ'.
      <fs_lra_carga>-low    = <fs_zsdt0133_sel>-nro_cg.
    ENDLOOP.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
         i_nr_carga          =  lra_nr_carga
         i_set_status_carga  =  abap_false
      IMPORTING
         e_cargas            =  DATA(lit_cargas)
         e_solicitacoes      =  DATA(lit_solicitacoes)
         e_notas_venda       =  DATA(lit_nf_venda)
         e_notas_transf      =  DATA(lit_notas_transf)
         e_ov_lotes          =  DATA(lit_ov_lotes)
         e_lotes             =  DATA(lit_lotes)
         e_romaneios         =  DATA(lit_romaneios) ).

    LOOP AT i_zsdt0133 ASSIGNING FIELD-SYMBOL(<fs_zsdt0133>).

      CHECK <fs_zsdt0133>-versao_processo IS NOT INITIAL.

      CHECK <fs_zsdt0133>-status NE 'X'. "Carga Cancelada...

      <fs_zsdt0133>-status = '1'. "Carga Criada

      IF <fs_zsdt0133>-integrar_carguero EQ abap_true OR <fs_zsdt0133>-carga_em_cotacao EQ abap_true.
        <fs_zsdt0133>-status = '3'. "Carga em Cotação
      ENDIF.

      IF <fs_zsdt0133>-cod_transportadora IS NOT INITIAL AND <fs_zsdt0133>-preco_frete IS NOT INITIAL.
        <fs_zsdt0133>-status = '4'. "Frete Contratado
      ENDIF.

      IF <fs_zsdt0133>-dt_autorizacao_embarque IS NOT INITIAL.
        <fs_zsdt0133>-status = '5'. "Embarque Autorizado
      ENDIF.

      IF <fs_zsdt0133>-doc_bordero_anexado IS NOT INITIAL.
        <fs_zsdt0133>-status = 'C'. "Carga com Bordero CD
      ENDIF.

      READ TABLE lit_notas_transf WITH  KEY nro_cg = <fs_zsdt0133>-nro_cg TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        <fs_zsdt0133>-status = 'A'. "Carga com NF Transf. Fornecedor
      ENDIF.

      READ TABLE lit_nf_venda WITH  KEY nro_cg = <fs_zsdt0133>-nro_cg TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        <fs_zsdt0133>-status = 'B'. "Carga com NF Fornecedor
      ENDIF.

      IF <fs_zsdt0133>-carga_conferida IS NOT INITIAL.
        <fs_zsdt0133>-status = 'D'. "Carga conferida Total
      ENDIF.

      DATA(_pendente_fat) = abap_false.
      READ TABLE lit_romaneios WITH  KEY nro_cg = <fs_zsdt0133>-nro_cg TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        <fs_zsdt0133>-status = '6'. "Romaneio Gerado

        LOOP AT lit_romaneios TRANSPORTING NO FIELDS WHERE nro_cg = <fs_zsdt0133>-nro_cg AND st_proc NE '99'.
          _pendente_fat = abap_true.
          EXIT.
        ENDLOOP.

        IF _pendente_fat EQ abap_false.
          <fs_zsdt0133>-status = '7'. "Romaneio Finalizado
        ENDIF.

      ENDIF.

    ENDLOOP.



  ENDMETHOD.


  METHOD desvincula_conferencia.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single   = i_nro_cg
      IMPORTING
        e_cargas            = DATA(lit_cargas)
    ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_cargas[] IS INITIAL.
      e_msg_erro = 'Carga não encontrada!'.
      RETURN.
    ENDIF.

    IF lwa_carga-carga_conferida IS NOT INITIAL.
      e_msg_erro = 'Carga já está conferida! Operação não permitida!'.
      RETURN.
    ENDIF.

    LOOP AT i_conferencia ASSIGNING FIELD-SYMBOL(<fs_conferencia>).

      UPDATE zsdt0420 SET cancel = abap_true WHERE id_conferencia       = <fs_conferencia>-id_conferencia
                                               AND nro_cg               = <fs_conferencia>-nro_cg
                                               AND nro_sol              = <fs_conferencia>-nro_sol
                                               AND seq                  = <fs_conferencia>-seq
                                               AND id_item_carregamento = <fs_conferencia>-id_item_carregamento
                                               AND lote_carregamento    = <fs_conferencia>-lote_carregamento
                                               AND chave_nfe            = <fs_conferencia>-chave_nfe
                                               AND prod_item            = <fs_conferencia>-prod_item.
    ENDLOOP.

    COMMIT WORK.


  ENDMETHOD.


  METHOD informa_qtd_conferencia.

    SELECT SINGLE *
      FROM zsdt0420 INTO @DATA(ls_0420)
     WHERE nro_cg               = @i_de_para-nro_cg
       AND id_item_carregamento = @i_de_para-id_item_carregamento
       AND lote_carregamento    = @i_de_para-lote_carregamento
       AND nro_sol              = @i_de_para-nro_sol
       AND seq                  = @i_de_para-seq
       AND chave_nfe            = @i_de_para-chave_nfe
       AND prod_item            = @i_de_para-prod_item
       AND cancel               = @space.

    IF sy-subrc NE 0.
      e_msg_erro = | Registro de conferencia não encontrado!|.
      RETURN.
    ENDIF.

*    IF i_de_para-quantidade < ls_0420-quantidade.
*      DATA(lv_menor) = abap_true.
*    ENDIF.

    "Agregar o Saldo já vinculado na conferencia da Carga
    IF c_bordero IS NOT INITIAL.
      ADD ls_0420-quantidade TO c_bordero-saldo_conf.
    ENDIF.

    IF c_item_carga IS NOT INITIAL.
      ADD ls_0420-quantidade TO c_item_carga-saldo_conf.
    ENDIF.

    IF c_nota IS NOT INITIAL.
      ADD ls_0420-quantidade TO c_nota-saldo_conf.
    ENDIF.

    IF c_item_carga IS INITIAL.
      e_msg_erro = |Item carga não informado para conferencia!|.
      RETURN.
    ENDIF.

    IF i_de_para-quantidade > c_item_carga-saldo_conf.
      e_msg_erro = |Nro Sol: { c_item_carga-nro_sol }  Seq. { c_item_carga-seq } possui saldo menor que a quantidade informada!|.
      RETURN.
    ENDIF.

    IF c_nota IS NOT INITIAL.
      IF c_nota-peso_conv IS INITIAL.
        e_msg_erro = |Item { c_nota-prod_item } da NFe { c_nota-numero_nfe } não possui Quantidade de conversão informada|.
        RETURN.
      ENDIF.

      IF i_de_para-quantidade > c_nota-saldo_conf.
        e_msg_erro = |Item { c_nota-prod_item } da nota { c_nota-numero_nfe } possui saldo menor que a quantidade informada|.
        RETURN.
      ENDIF.
    ENDIF.

    IF c_bordero IS NOT INITIAL.
      IF i_de_para-quantidade > c_bordero-saldo_conf.
        e_msg_erro = |Item { c_bordero-id_item } do borderô possui saldo menor que a quantidade informada |.
        RETURN.
      ENDIF.
    ENDIF.

    IF c_item_carga IS NOT INITIAL.
      SUBTRACT i_de_para-quantidade FROM c_item_carga-saldo_conf.
    ENDIF.

    IF c_bordero IS NOT INITIAL.
      SUBTRACT i_de_para-quantidade FROM c_bordero-saldo_conf.
    ENDIF.

    IF c_nota IS NOT INITIAL.
      SUBTRACT i_de_para-quantidade FROM c_nota-saldo_conf.
    ENDIF.

    UPDATE zsdt0420 SET quantidade   = i_de_para-quantidade
                        qtd_conv_nfe = c_nota-qtd_conv
     WHERE nro_cg               EQ i_nro_carga
       AND nro_sol              EQ c_item_carga-nro_sol
       AND seq                  EQ c_item_carga-seq
       AND id_item_carregamento EQ c_bordero-id_item
       AND lote_carregamento    EQ c_bordero-lote
       AND chave_nfe            EQ c_nota-chave_nfe
       AND prod_item            EQ c_nota-prod_item.

    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD vincular_conferencia.

    CLEAR: e_zsdt0420.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single   = i_nro_carga
      IMPORTING
        e_cargas            = DATA(lit_cargas)
        e_solicitacoes      = DATA(lit_solicitacoes)
    ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_cargas[] IS INITIAL.
      e_msg_erro = 'Carga não encontrada!'.
      RETURN.
    ENDIF.

    IF lwa_carga-carga_conferida IS NOT INITIAL.
      e_msg_erro = 'Carga já está conferida! Operação não permitida!'.
      RETURN.
    ENDIF.

    IF i_item_carga IS INITIAL.
      e_msg_erro = 'Os dados de solicitações são obrigatórios para de-para'.
      RETURN.
    ENDIF.

    READ TABLE lit_solicitacoes INTO DATA(lwa_solicitacao) WITH KEY  nro_sol = i_item_carga-nro_sol
                                                                     seq     = i_item_carga-seq.

    IF sy-subrc NE 0.
      e_msg_erro = 'Solicitação não encontrada!'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0420 INTO @DATA(lwa_zsdt0420)
     WHERE nro_cg               = @i_nro_carga
       AND id_item_carregamento = @i_bordero-id_item
       AND lote_carregamento    = @i_bordero-lote
       AND nro_sol              = @i_item_carga-nro_sol
       AND seq                  = @i_item_carga-seq
       AND chave_nfe            = @i_nota-chave_nfe
       AND prod_item            = @i_nota-prod_item
       AND cancel               = @space.

    IF sy-subrc IS INITIAL.
      e_msg_erro = 'Já existe conferência criada para esta mesma chave'.
      RETURN.
    ENDIF.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
    IF i_nota-chave_nfe IS NOT INITIAL AND i_nota-prod_item IS NOT INITIAL.
      SELECT *
        FROM zsdt0420 INTO TABLE @DATA(lit_zsdt0420_vinc_nfe)
       WHERE nro_cg               = @i_nro_carga
         AND chave_nfe            = @i_nota-chave_nfe
         AND prod_item            = @i_nota-prod_item
         AND cancel               = @space.

      LOOP AT lit_zsdt0420_vinc_nfe ASSIGNING FIELD-SYMBOL(<fs_ck_zsdt0420_vinc_nfe>).
        READ TABLE lit_solicitacoes INTO DATA(lwa_solicitacao_check) WITH KEY  nro_sol = <fs_ck_zsdt0420_vinc_nfe>-nro_sol
                                                                               seq     = <fs_ck_zsdt0420_vinc_nfe>-seq.

        IF sy-subrc NE 0.
          e_msg_erro = 'Solicitação não encontrada!'.
          RETURN.
        ENDIF.

        IF NOT ( lwa_solicitacao_check-ebeln = lwa_solicitacao-ebeln AND
                 lwa_solicitacao_check-ebelp = lwa_solicitacao-ebelp ).

          e_msg_erro = |Chave { i_nota-chave_nfe } Item { i_nota-prod_item } já vinculado com um Pedido/Item diferente! Operação não permitida|.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--


    DATA(_embarque_luft) = zcl_carga_saida_insumos=>get_embarque_luft( i_nro_cg = i_nro_carga ).

    IF _embarque_luft EQ abap_true.
      IF i_bordero IS INITIAL.
        e_msg_erro = 'Conferencia deve ser realizada com bordero!'.
        RETURN.
      ENDIF.

      IF i_bordero-lote IS INITIAL.
        e_msg_erro = 'Bordero sem Lote!'.
        RETURN.
      ENDIF.

      IF lwa_solicitacao-flexibilidade EQ '3'. "Manter Lote e Marca
        IF i_item_carga-charg IS NOT INITIAL AND
           i_item_carga-charg NE i_bordero-lote.
          e_msg_erro = |Solicitação com Lote { i_item_carga-charg } e Borderô com Lote { i_bordero-lote }|.
          RETURN.
        ENDIF.
      ENDIF.

      IF lwa_solicitacao-flexibilidade EQ '2'. "Manter Marca
        IF i_item_carga-marca IS NOT INITIAL.
          zcl_charg=>get_valor_caracteristica(
             EXPORTING
                iv_nome_attr = 'SEMENTE_MARCAS'
                iv_matnr     = CONV #( i_bordero-matnr )
                iv_charg     = CONV #( i_bordero-lote )
             IMPORTING
                e_atwrt      = DATA(lva_marca_semente)   ).

          IF i_item_carga-marca NE lva_marca_semente.
            e_msg_erro = |Solicitação com Marca da Semente { i_item_carga-marca } e Borderô com Marca { lva_marca_semente }|.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.
      IF i_nota IS INITIAL.
        "Embarque CD terceiro ou Fornecedor... Precisa da NF-e do Fornecedor ou NF-e Retorno Armazenagem
        e_msg_erro = 'Conferencia deve ser realizada com nota fiscal!'.
        RETURN.
      ENDIF.
    ENDIF.

    GET TIME STAMP FIELD DATA(lv_timestamp).

    lwa_zsdt0420-id_conferencia       = lv_timestamp.
    lwa_zsdt0420-nro_cg               = i_nro_carga.
    lwa_zsdt0420-nro_sol              = i_item_carga-nro_sol.
    lwa_zsdt0420-seq                  = i_item_carga-seq.
    lwa_zsdt0420-id_item_carregamento = i_bordero-id_item.
    lwa_zsdt0420-lote_carregamento    = i_bordero-lote.
    lwa_zsdt0420-nr_fase              = i_bordero-nr_fase.
    lwa_zsdt0420-chave_nfe            = i_nota-chave_nfe.
    lwa_zsdt0420-prod_item            = i_nota-prod_item.
    lwa_zsdt0420-peso_conv_nfe        = i_nota-peso_conv.
    lwa_zsdt0420-user_create          = sy-uname.
    lwa_zsdt0420-date_create          = sy-datum.
    lwa_zsdt0420-time_create          = sy-uzeit.

    MODIFY zsdt0420 FROM lwa_zsdt0420.
    IF sy-subrc IS INITIAL.
      e_zsdt0420 = lwa_zsdt0420.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD aceite_fiscal.

    DATA lo_aceite TYPE REF TO zcl_nfe_inbound.

    CHECK i_notas IS NOT INITIAL.

    DATA(lt_notas) = i_notas.
    SORT lt_notas BY chave_nfe.
    DELETE ADJACENT DUPLICATES FROM lt_notas COMPARING chave_nfe.

    LOOP AT lt_notas ASSIGNING FIELD-SYMBOL(<fs_nota>).

      TRY .
          CREATE OBJECT lo_aceite.

          CALL METHOD lo_aceite->zif_cadastro~set_registro
            EXPORTING
              i_id_registro = <fs_nota>-chave_nfe.

          lo_aceite->set_info_sap( ).

          LOOP AT i_notas ASSIGNING FIELD-SYMBOL(<fs_dados_aux>) WHERE chave_nfe = <fs_nota>-chave_nfe.

            CALL METHOD lo_aceite->set_item_material
              EXPORTING
                i_prod_item = <fs_dados_aux>-prod_item
                i_matnr     = <fs_dados_aux>-matnr
                i_ebeln     = <fs_dados_aux>-ebeln
                i_ebelp     = <fs_dados_aux>-ebelp
                i_menge     = <fs_dados_aux>-quantidade
                i_meins     = <fs_dados_aux>-meins
                i_lgort     = <fs_dados_aux>-lgort.

          ENDLOOP.

          lo_aceite->zif_cadastro~ck_alterou = abap_true.
          lo_aceite->set_aceitar_documento( ).
          lo_aceite->set_fiscal_taxas( ).
          lo_aceite->zif_cadastro~gravar_registro( ).

        CATCH zcx_cadastro INTO DATA(lo_cadastro).
          e_msg_erro = lo_cadastro->get_text( ).

        CATCH zcx_nfe_inbound_exception INTO DATA(lo_nfe_inbound).
          e_msg_erro = lo_nfe_inbound->get_text( ).

        CATCH zcx_pedido_compra_exception INTO DATA(lo_pedido_compra).
          e_msg_erro = lo_pedido_compra->get_text( ).
      ENDTRY.

      lo_aceite->free( ).

    ENDLOOP.

    "Marcar Carga como Conferida se todas as notas tiveram o aceite fiscal realizado
    zcl_carga_saida_insumos=>check_and_set_carga_conferida( i_nro_cg = i_nro_cg ).

  ENDMETHOD.


  METHOD cancelar_aceite.

    DATA lo_aceite TYPE REF TO zcl_nfe_inbound.

    TRY .
        CREATE OBJECT lo_aceite.

        CALL METHOD lo_aceite->zif_cadastro~set_registro
          EXPORTING
            i_id_registro = i_chave_nfe.

        lo_aceite->nfe_inbound_cancela_aceite( ).

        UPDATE zsdt0133 SET carga_conferida = abap_false
         WHERE nro_cg EQ i_nro_cg.

      CATCH zcx_nfe_inbound_exception INTO DATA(lo_nfe_inbound).
        e_msg_erro = lo_nfe_inbound->get_text( ).
      CATCH zcx_cadastro INTO DATA(lo_cadastro).
        e_msg_erro = lo_cadastro->get_text( ).
    ENDTRY.

    lo_aceite->free( ).

  ENDMETHOD.


  METHOD monta_dados_aceite_fiscal.

    CLEAR: e_dados_aceite_fiscal[].

    CHECK i_notas[] IS NOT INITIAL.

    DATA(lt_notas)      = i_notas.
    DATA(lt_notas_full) = i_notas.

    SELECT *
      FROM zib_nfe_dist_ter INTO TABLE @DATA(lt_dist_ter)
       FOR ALL ENTRIES IN @lt_notas
     WHERE chave_nfe = @lt_notas-chave_nfe.

    CHECK lt_dist_ter[] IS NOT INITIAL.

    SORT lt_dist_ter BY chave_nfe ck_fiscal.

    CASE i_cancelar_aceite.
      WHEN abap_true.

        LOOP AT lt_notas ASSIGNING FIELD-SYMBOL(<fs_notas>).

          READ TABLE lt_dist_ter TRANSPORTING NO FIELDS WITH KEY chave_nfe = <fs_notas>-chave_nfe
                                                             ck_fiscal = abap_true BINARY SEARCH.
          CHECK sy-subrc IS INITIAL.
          APPEND INITIAL LINE TO e_dados_aceite_fiscal ASSIGNING FIELD-SYMBOL(<fs_aceite_fiscal>).

          <fs_aceite_fiscal>-chave_nfe = <fs_notas>-chave_nfe.
          <fs_aceite_fiscal>-cod_nfe   = <fs_notas>-numero_nfe.
          <fs_aceite_fiscal>-acao      = 'Cancelar Aceite'.
          <fs_aceite_fiscal>-acao_bt   = 'CANCELAR'.

        ENDLOOP.

      WHEN abap_false.

        SORT lt_notas BY chave_nfe.
        DELETE ADJACENT DUPLICATES FROM lt_notas COMPARING chave_nfe.

        DATA(lt_de_para) = i_de_para.
        SORT lt_de_para BY chave_nfe.

        LOOP AT lt_notas ASSIGNING <fs_notas>.

          READ TABLE lt_dist_ter TRANSPORTING NO FIELDS WITH KEY chave_nfe = <fs_notas>-chave_nfe
                                                                 ck_fiscal = abap_true BINARY SEARCH.

          CHECK sy-subrc IS NOT INITIAL. "NF-e não pode estar marcada aceite fiscal

          READ TABLE lt_de_para TRANSPORTING NO FIELDS WITH KEY chave_nfe = <fs_notas>-chave_nfe BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            APPEND INITIAL LINE TO e_dados_aceite_fiscal ASSIGNING <fs_aceite_fiscal>.
            <fs_aceite_fiscal>-chave_nfe = <fs_notas>-chave_nfe.
            <fs_aceite_fiscal>-cod_nfe   = <fs_notas>-numero_nfe.
            <fs_aceite_fiscal>-acao      = 'De-Para Incompleto'.
            CONTINUE.
          ENDIF.

          LOOP AT lt_de_para ASSIGNING FIELD-SYMBOL(<fs_de_para>) WHERE chave_nfe = <fs_notas>-chave_nfe.

            APPEND INITIAL LINE TO e_dados_aceite_fiscal ASSIGNING <fs_aceite_fiscal>.
            <fs_aceite_fiscal>-chave_nfe = <fs_notas>-chave_nfe.
            <fs_aceite_fiscal>-cod_nfe   = <fs_notas>-numero_nfe.

            "Valida se todos os itens da nota estão com saldo 0
            DATA(_nota_com_saldo) = abap_false.
            LOOP AT lt_notas_full TRANSPORTING NO FIELDS WHERE chave_nfe = <fs_notas>-chave_nfe AND saldo_conf > 0.
              _nota_com_saldo = abap_true.
            ENDLOOP.

            IF _nota_com_saldo EQ abap_true.
              <fs_aceite_fiscal>-acao = 'De-Para Incompleto'.
              CONTINUE.
            ENDIF.

            "Valida se todos os itens do bordero estão com saldo 0 e se o borderô é do tipo 'LF'
            LOOP AT i_bordero TRANSPORTING NO FIELDS WHERE id_autorizacao_embarque = <fs_de_para>-nro_cg AND saldo_conf > 0.
              <fs_aceite_fiscal>-acao = 'De-Para Incompleto'.
              CONTINUE.
            ENDLOOP.

            IF <fs_de_para>-cod_prod_bordero IS NOT INITIAL AND
              ( <fs_de_para>-cod_prod_bordero <> <fs_de_para>-cod_prod_ov
                "vALIDAR DIVERGENCIA LOTE
                ).
              <fs_aceite_fiscal>-matnr = <fs_de_para>-cod_prod_ov.
              <fs_aceite_fiscal>-maktx = <fs_de_para>-desc_prod_ov.
              <fs_aceite_fiscal>-acao = 'Corrigir Borderô'.
              <fs_aceite_fiscal>-acao_bt = 'BORDERO'.
            ELSE.
              <fs_aceite_fiscal>-acao = 'Aceite Fiscal'.
              <fs_aceite_fiscal>-acao_bt = 'ACEITE'.
            ENDIF.

          ENDLOOP.

        ENDLOOP.

    ENDCASE.

    DELETE ADJACENT DUPLICATES FROM e_dados_aceite_fiscal COMPARING ALL FIELDS.

  ENDMETHOD.


  METHOD get_embarque_luft.

    DATA: lva_roteiro_pc TYPE z_nr_rot.

    CLEAR: r_embarque_luft, e_lfa1_embarque.

    IF i_nro_cg IS NOT INITIAL.
      SELECT SINGLE nro_cg, nro_lote
        FROM zsdt0129 INTO @DATA(lwa_zsdt0129)
       WHERE nro_cg EQ @i_nro_cg.

      CHECK sy-subrc EQ 0 AND lwa_zsdt0129-nro_lote IS NOT INITIAL.

      SELECT SINGLE nro_lote, cod_loc_emb
        FROM zsdt0131 INTO @DATA(lwa_zsdt0131)
       WHERE nro_lote EQ @lwa_zsdt0129-nro_lote.

      CHECK sy-subrc EQ 0 AND lwa_zsdt0131-cod_loc_emb IS NOT INITIAL.

      lva_roteiro_pc = lwa_zsdt0131-cod_loc_emb.

    ELSEif i_roteiro_pc is NOT INITIAL.
      lva_roteiro_pc = i_roteiro_pc.
    ENDIF.

    CHECK lva_roteiro_pc IS NOT INITIAL.

    SELECT SINGLE nr_rot, lifnr
      FROM zsdt0132 INTO @DATA(lwa_zsdt0132)
     WHERE nr_rot EQ @lva_roteiro_pc.

    CHECK sy-subrc EQ 0 AND lwa_zsdt0132-lifnr IS NOT INITIAL.

    SELECT SINGLE *
      FROM zsdt0419 INTO @DATA(lwa_zsdt0419)
     WHERE lifnr EQ @lwa_zsdt0132-lifnr.

    CHECK sy-subrc EQ 0 AND lwa_zsdt0132-lifnr IS NOT INITIAL.

    SELECT SINGLE *
      FROM lfa1 INTO e_lfa1_embarque
     WHERE lifnr EQ lwa_zsdt0132-lifnr.

    r_embarque_luft = abap_true.

  ENDMETHOD.


  METHOD anular_conferencia.

    DATA: lva_dt_null TYPE erdat.

    CHECK i_nro_cg IS NOT INITIAL.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single   = i_nro_cg
      IMPORTING
        e_cargas            = DATA(lit_cargas)
        e_romaneios         = DATA(lit_romaneios)
    ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_cargas[] IS INITIAL.
      r_msg_error = 'Carga não encontrada!'.
      RETURN.
    ENDIF.

    IF lwa_carga-carga_conferida is INITIAL.
      r_msg_error = 'Carga não está conferida! Operação não permitida!'.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS NOT INITIAL.
      r_msg_error = 'Carga com romaneios gerados! Operação não permitida!'.
      RETURN.
    ENDIF.

    UPDATE zsdt0133 SET  carga_conferida = abap_false
                         dt_conferencia  = lva_dt_null
                         hr_conferencia  = space
                         us_conferencia  = space
     WHERE nro_cg EQ i_nro_cg.

    "Remove Informações Conferencia Carga
    DELETE FROM zsdt0420 WHERE nro_cg EQ i_nro_cg.

    MESSAGE 'Anulação da conferencia da carga realizada com sucesso!' TYPE 'S'.

  ENDMETHOD.


  METHOD atualiza_sol_safra_control.

    DATA: lit_sol_post TYPE zsdt0131_t.
    DATA: lit_sol_del  TYPE zsdt0131_t.

    CLEAR: r_msg_error, lit_sol_post[], lit_sol_del[].

    LOOP AT i_solicitacoes_current INTO DATA(lwa_zsdt0131_current).

      DATA(_externalid_sol_item) = lwa_zsdt0131_current-nro_sol && lwa_zsdt0131_current-seq.

      READ TABLE i_solicitacoes_safra INTO DATA(ls_sol_safra) WITH KEY external_id = _externalid_sol_item.
      IF sy-subrc NE 0.
        r_msg_error = |Não foi possivel consultar Solicitação { lwa_zsdt0131_current-nro_sol } Seq: { lwa_zsdt0131_current-seq } no Safra Control|.
        RETURN.
      ENDIF.

      IF lwa_zsdt0131_current-status EQ 'X'. "Cancelada
        APPEND lwa_zsdt0131_current TO lit_sol_del.
      ELSE.
        APPEND lwa_zsdt0131_current TO lit_sol_post.
      ENDIF.

    ENDLOOP.

    "Enviar Inclusão/Alteração de Solicitações da Carga para Safra Control
    IF lit_sol_post[] IS NOT INITIAL.
      r_msg_error = zcl_carga_saida_insumos=>post_sol_carga_safra_control( EXPORTING i_header_carga = i_header_carga
                                                                                     i_zsdt0131_t   = lit_sol_post ).
    ENDIF.

    CHECK r_msg_error IS INITIAL.

    "Enviar Deleção de Solicitações da Carga para Safra Control
    IF lit_sol_del IS NOT INITIAL.
      r_msg_error = zcl_carga_saida_insumos=>del_sol_carga_safra_control( EXPORTING i_header_carga = i_header_carga
                                                                                    i_zsdt0131_t   = lit_sol_del ).
    ENDIF.

    CHECK r_msg_error IS INITIAL.

  ENDMETHOD.


  METHOD CHANGE_DELIVERY.

    DATA: LS_HEADER_DATA    TYPE BAPIOBDLVHDRCHG,
          LS_HEADER_CONTROL TYPE BAPIOBDLVHDRCTRLCHG.

    LS_HEADER_DATA =
    VALUE #(
              DELIV_NUMB = I_VBELN
              INCOTERMS1 = I_INCO1
              INCOTERMS2L = I_INCO1
           ).

    LS_HEADER_CONTROL =
    VALUE #(
              DELIV_NUMB = I_VBELN
              INCO1_FLG = ABAP_TRUE
              INCO2_L_FLG = ABAP_TRUE
           ).

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        HEADER_DATA    = LS_HEADER_DATA
        HEADER_CONTROL = LS_HEADER_CONTROL
        DELIVERY       = I_VBELN
      TABLES
        RETURN         = E_RETURN.

    CHECK NOT LINE_EXISTS( E_RETURN[ TYPE = 'E' ] ).

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = ABAP_TRUE.

  ENDMETHOD.


  METHOD check_and_set_carga_conferida.

    CLEAR: r_conferida_total.

    "Marcar Carga como Conferida se todas as notas tiveram o aceite fiscal realizado
    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single        = i_nro_cg
        i_dados_conferencia      = abap_true
      IMPORTING
        e_cargas                 = DATA(lit_cargas)
        e_notas_conferencia      = DATA(lit_notas_conf) ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    CHECK sy-subrc EQ 0 AND lwa_carga-nro_cg IS NOT INITIAL.

    CHECK lit_notas_conf[] IS NOT INITIAL.

    SELECT chave_nfe, ck_fiscal
      FROM zib_nfe_dist_ter INTO TABLE @DATA(lit_zib_ter)
       FOR ALL ENTRIES IN @lit_notas_conf
     WHERE chave_nfe EQ @lit_notas_conf-chave_nfe.

    CHECK lit_zib_ter[] IS NOT INITIAL.

    READ TABLE lit_zib_ter TRANSPORTING NO FIELDS WITH KEY ck_fiscal = abap_false.
    IF sy-subrc NE 0. "Todas as notas estao marcadas com aceite

      IF lwa_carga-carga_conferida EQ abap_false.

        r_conferida_total = abap_true.

        UPDATE zsdt0133 SET carga_conferida = abap_true
                            dt_conferencia  = sy-datum
                            hr_conferencia  = sy-uzeit
                            us_conferencia  = sy-uname
         WHERE nro_cg EQ i_nro_cg.

        MESSAGE 'Carga Conferida Totalmente com sucesso!' TYPE 'S'.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_carga_troca_nota.

    CLEAR: r_troca_nota.

    DATA(lva_origem_estoque) = zcl_carga_saida_insumos=>get_origem_estoque_carga( i_nro_cg = i_nro_cg ).

    CASE lva_origem_estoque.
      WHEN '1'. "Filial
        r_troca_nota = abap_false.
      WHEN '2'. "Fornecedor
        r_troca_nota = abap_true.
      WHEN '3'. "Armazem
        r_troca_nota = abap_true.
    ENDCASE.

  ENDMETHOD.


  METHOD check_conferencia_com_depara.

    CLEAR: r_conferencia_com_depara.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single =  i_nro_cg
      IMPORTING
        e_cargas          =  DATA(lit_carga) ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lva_embarque_integrado_luft) = abap_false.
    IF ( zcl_carga_saida_insumos=>get_embarque_luft( i_nro_cg = i_nro_cg ) = abap_true ) AND ( lwa_carga-nro_pedido_luft IS NOT INITIAL ).
      lva_embarque_integrado_luft = abap_true.
    ENDIF.

    DATA(lva_carga_troca_nota) = zcl_carga_saida_insumos=>check_carga_troca_nota( i_nro_cg = i_nro_cg ).

    IF lva_carga_troca_nota        EQ abap_true OR
       lva_embarque_integrado_luft EQ abap_true.
      r_conferencia_com_depara = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_entrada_nfe_pendente.

    CLEAR: r_msg_error.

    CHECK i_nro_cg IS NOT INITIAL.

    CHECK zcl_carga_saida_insumos=>check_embarque_armazem( i_nro_cg = i_nro_cg ) EQ abap_false. "Só Embarque Fornecedor

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single   = i_nro_cg
        i_dados_conferencia = abap_true
      IMPORTING
        e_cargas            = DATA(lit_cargas)
        e_notas_conferencia = DATA(lit_notas_conferencia)
        e_romaneios         = DATA(lit_romaneios)
    ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_cargas[] IS INITIAL.
      r_msg_error = 'Carga não encontrada!'.
      RETURN.
    ENDIF.

    IF lit_notas_conferencia[] IS INITIAL.
      r_msg_error = 'Carga sem notas para movimentação de entrada!'.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS INITIAL.
      r_msg_error = 'Carga sem romaneios para movimentação de entrada!'.
      RETURN.
    ENDIF.

    LOOP AT lit_notas_conferencia ASSIGNING FIELD-SYMBOL(<fs_nota_conf>) WHERE ck_fiscal EQ abap_false.
      r_msg_error = |NF-e { <fs_nota_conf>-chave_nfe } sem aceite fiscal!|.
      RETURN.
    ENDLOOP.

    LOOP AT lit_notas_conferencia ASSIGNING <fs_nota_conf>.

      SELECT SINGLE *
        FROM zib_nfe_dist_ter INTO @DATA(lwa_zib_nfe_dist_ter)
       WHERE chave_nfe EQ @<fs_nota_conf>-chave_nfe.

      IF sy-subrc NE 0.
        r_msg_error = |ÑF-e { <fs_nota_conf>-chave_nfe } não encontrada na ZMM0110!|.
        RETURN.
      ENDIF.

      IF NOT ( lwa_zib_nfe_dist_ter-mblnr IS NOT INITIAL AND lwa_zib_nfe_dist_ter-belnr IS NOT INITIAL ).
        r_msg_error = |ÑF-e { <fs_nota_conf>-chave_nfe } não possui Migo e Miro gerada na ZMM0110!|.
        RETURN.
      ENDIF.

    ENDLOOP.



  ENDMETHOD.


  METHOD CONFERIR_CARGA_COM_NF.

    DATA: lv_answer.

    CLEAR: r_msg_error.

    zcl_carga_saida_insumos=>busca_dados_carga(
     EXPORTING
       i_nr_carga_single   = i_nro_carga
       i_dados_conferencia = abap_true
     IMPORTING
       e_cargas            =  DATA(lit_carga)
       e_solicitacoes      =  DATA(lit_solicitacoes)
       e_romaneios         =  DATA(lit_romaneios)
       e_lotes             =  DATA(lit_lotes)
       e_notas_venda       =  DATA(lit_notas_venda)
       e_notas_conferencia =  DATA(lit_notas_conf)
       e_bordero           =  DATA(lit_bordero)
       e_dados_conferencia =  DATA(lit_dados_conferencia)
   ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_carga-carga_conferida IS NOT INITIAL.
      r_msg_error = |Carga já foi conferida!|.
      RETURN.
    ENDIF.

    IF lwa_carga-dt_autorizacao_embarque IS INITIAL.
      r_msg_error = |Carga não possui autorização de embarque gerada!|.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS NOT INITIAL.
      r_msg_error = 'Carga com romaneios gerados! Operação não permitida!'.
      RETURN.
    ENDIF.

    "Conferir Saldo nas conferencias
    LOOP AT lit_notas_conf ASSIGNING FIELD-SYMBOL(<fs_nf_conf>) WHERE saldo_conf > 0.
      r_msg_error = |Chave { <fs_nf_conf>-chave_nfe } com saldo para conferencia!|.
      RETURN.
    ENDLOOP..

    LOOP AT lit_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao>) WHERE saldo_conf > 0.
      r_msg_error = |Nro Sol. { <fs_solicitacao>-nro_sol } Seq.: { <fs_solicitacao>-seq } com saldo para conferencia!|.
      RETURN.
    ENDLOOP.

*    IF lit_lotes[] IS INITIAL.
*      r_msg_error = 'Carga sem lotes informados! Operação não permitida!'.
*      RETURN.
*    ENDIF.

    r_msg_error = zcl_carga_saida_insumos=>check_permissao_carga(
        EXPORTING
           i_nro_carga = i_nro_carga
           i_atividade = '09' ).

    CHECK r_msg_error IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Conferência'
        text_question         = 'Confirma a conferência dessa Carga?'
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_INCOMPLETE'
        display_cancel_button = space
      IMPORTING
        answer                = lv_answer.

    IF lv_answer <> '1'.
      RETURN.
    ENDIF.

    UPDATE zsdt0133 SET carga_conferida = abap_true
                        dt_conferencia  = sy-datum
                        hr_conferencia  = sy-uzeit
                        us_conferencia  = sy-uname
     WHERE nro_cg EQ i_nro_carga.

    MESSAGE 'Carga conferida com sucesso!' TYPE 'S'.

  ENDMETHOD.


  METHOD conferir_carga_sem_nf.

    DATA: lv_answer.

    CLEAR: r_msg_error.

    zcl_carga_saida_insumos=>busca_dados_carga(
     EXPORTING
       i_nr_carga_single   = i_nro_carga
       i_dados_conferencia = abap_true
     IMPORTING
       e_cargas            =  DATA(lit_carga)
       e_solicitacoes      =  DATA(lit_solicitacoes)
       e_romaneios         =  DATA(lit_romaneios)
       e_lotes             =  DATA(lit_lotes)
       e_notas_venda       =  DATA(lit_notas_venda)
       e_bordero           =  DATA(lit_bordero)
       e_dados_conferencia =  DATA(lit_dados_conferencia)
   ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_carga-carga_conferida IS NOT INITIAL.
      r_msg_error = |Carga já foi conferida!|.
      RETURN.
    ENDIF.

    IF lwa_carga-dt_autorizacao_embarque IS INITIAL.
      r_msg_error = |Carga não possui autorização de embarque gerada!|.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS NOT INITIAL.
      r_msg_error = 'Carga com romaneios gerados! Operação não permitida!'.
      RETURN.
    ENDIF.

    "Conferir Saldo nas conferencias
    LOOP AT lit_bordero ASSIGNING FIELD-SYMBOL(<fs_bordero>) WHERE saldo_conf > 0.
      r_msg_error = |Item { <fs_bordero>-id_item } do bordero com saldo para conferencia!|.
      RETURN.
    ENDLOOP..

    LOOP AT lit_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao>) WHERE saldo_conf > 0.
      r_msg_error = |Nro Sol. { <fs_solicitacao>-nro_sol } Seq.: { <fs_solicitacao>-seq } com saldo para conferencia!|.
      RETURN.
    ENDLOOP.

*    IF lit_lotes[] IS INITIAL.
*      r_msg_error = 'Carga sem lotes informados! Operação não permitida!'.
*      RETURN.
*    ENDIF.

    r_msg_error = zcl_carga_saida_insumos=>check_permissao_carga(
        EXPORTING
           i_nro_carga = i_nro_carga
           i_atividade = '09' ).

    CHECK r_msg_error IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Conferência'
        text_question         = 'Confirma a conferência dessa Carga?'
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_INCOMPLETE'
        display_cancel_button = space
      IMPORTING
        answer                = lv_answer.

    IF lv_answer <> '1'.
      RETURN.
    ENDIF.

    UPDATE zsdt0133 SET carga_conferida = abap_true
                        dt_conferencia  = sy-datum
                        hr_conferencia  = sy-uzeit
                        us_conferencia  = sy-uname
     WHERE nro_cg EQ i_nro_carga.

    MESSAGE 'Carga conferida com sucesso!' TYPE 'S'.

  ENDMETHOD.


  METHOD consolida_status_solicitacao.

    SELECT nro_lote, nro_sol, seq, vbeln, posnr
      INTO TABLE @DATA(t_zsdt0131)
      FROM zsdt0131
     WHERE nro_sol = @i_nro_sol
       AND seq     = @i_seq
       AND vbeln   = @i_vbeln
       AND posnr   = @i_posnr
       AND status <> @abap_true.

    IF sy-subrc <> 0.
      UPDATE zsdt0082 SET status  = '2'
                    WHERE nro_sol = i_nro_sol
                      AND seq     = i_seq
                      AND vbeln   = i_vbeln
                      AND posnr   = i_posnr.
      RETURN.
    ENDIF.

    SELECT nro_lote, nro_cg
      INTO TABLE @DATA(t_zsdt0129)
      FROM zsdt0129
       FOR ALL ENTRIES IN @t_zsdt0131
     WHERE nro_lote = @t_zsdt0131-nro_lote
       AND status  <> @abap_true.

    IF sy-subrc <> 0.
      FREE: t_zsdt0129.
    ENDIF.

    DESCRIBE TABLE t_zsdt0129 LINES DATA(l_lines).

    DATA(_status) = COND #( WHEN l_lines = 0 THEN '2' ELSE '5' ).

    UPDATE zsdt0082 SET status  = _status
                  WHERE nro_sol = i_nro_sol
                    AND seq     = i_seq
                    AND vbeln   = i_vbeln
                    AND posnr   = i_posnr.

  ENDMETHOD.


  METHOD criar_pedido_luft_v1.

    DATA: lwa_pedido_saida_luft TYPE zsde_integra_luft_pedido_saida.

    CLEAR: r_msg_error, lwa_pedido_saida_luft.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single = i_nro_cg
      IMPORTING
        e_cargas          = DATA(lit_carga)
        e_solicitacoes    = DATA(lit_solicitacoes)
    ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_carga-cod_transportadora IS INITIAL.
      r_msg_error = 'Carga sem transportadora definida!'.
      RETURN.
    ENDIF.

    LOOP AT lit_solicitacoes INTO DATA(lwa_solicitacao_check) WHERE item_carga IS INITIAL.
      r_msg_error = 'Carga com solicitações sem sequencial de item carga definido!'.
      RETURN.
    ENDLOOP.

    zcl_carga_saida_insumos=>get_embarque_luft(
      EXPORTING
        i_nro_cg        = i_nro_cg
      IMPORTING
        e_lfa1_embarque = DATA(lwa_lfa1_embarque)
      RECEIVING
        r_embarque_luft = DATA(_embarque_luft)
    ).

    CHECK _embarque_luft IS NOT INITIAL.

    "Gerar Sequencial Carregamento LUFT
    r_msg_error = zcl_carga_saida_insumos=>gerar_seq_carregamento_luft( EXPORTING i_nro_cg  = i_nro_cg ).

    CHECK r_msg_error IS INITIAL.

    "Recarregar informações Solicitações
    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single = i_nro_cg
      IMPORTING
        e_solicitacoes    = lit_solicitacoes
    ).

    LOOP AT lit_solicitacoes INTO lwa_solicitacao_check WHERE seq_carregamento_luft IS INITIAL.
      r_msg_error = 'Carga com solicitações sem sequencial Carregamento LUFT definido!'.
      RETURN.
    ENDLOOP.

    lwa_pedido_saida_luft-cnpj_depositante   = lwa_lfa1_embarque-stcd1.
    lwa_pedido_saida_luft-pedido-numero      = i_nro_pedido_luft.

    DATA(lva_spart) = zcl_carga_saida_insumos=>get_spart_carga( EXPORTING i_nro_carga = i_nro_cg ).

    CASE lva_spart.
      WHEN '02'. "Fertilizantes
        lwa_pedido_saida_luft-pedido-tipo_pedido = 'Fertilizantes'.
      WHEN '03'. "Defensivos
        lwa_pedido_saida_luft-pedido-tipo_pedido = 'Defensivos'.
      WHEN '04'. "Sementes
        lwa_pedido_saida_luft-pedido-tipo_pedido = 'Sementes'.
      WHEN OTHERS.
        r_msg_error = 'Setor de atividade desconhecido!'.
        RETURN.
    ENDCASE.


    lwa_pedido_saida_luft-pedido-transportadora-nome = lwa_carga-desc_transportadora.
    lwa_pedido_saida_luft-pedido-transportadora-cnpj = lwa_carga-cnpj_transportadora.
    lwa_pedido_saida_luft-pedido-valor_total         = 0.

    LOOP AT lit_solicitacoes INTO DATA(lwa_solicitacao).
      APPEND INITIAL LINE TO lwa_pedido_saida_luft-pedido-lista_itens_pedido ASSIGNING FIELD-SYMBOL(<fs_item_pedido>).
      <fs_item_pedido>-linha                              = lwa_solicitacao-item_carga.
      <fs_item_pedido>-sequencia_carregamento             = lwa_solicitacao-seq_carregamento_luft.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = lwa_solicitacao-matnr
        IMPORTING
          output = <fs_item_pedido>-codigo_produto.

      <fs_item_pedido>-descricao                          = lwa_solicitacao-maktx.
      <fs_item_pedido>-quantidade                         = lwa_solicitacao-qtd_vinc.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input  = lwa_solicitacao-meins
        IMPORTING
          output = <fs_item_pedido>-unidade_medida.

      <fs_item_pedido>-valor_total_item                   = 1.

      CASE lwa_solicitacao-flexibilidade.
        WHEN '1'. "Permite Alteração Marca e Lote
        WHEN '2'. "Manter Marca
          <fs_item_pedido>-marca            = lwa_solicitacao-marca.
        WHEN '3'. "Manter Lote e Marca
          <fs_item_pedido>-marca            = lwa_solicitacao-marca.
          <fs_item_pedido>-lote             = lwa_solicitacao-charg.
      ENDCASE.

      <fs_item_pedido>-destinatario-nome                  = lwa_solicitacao-name1_kunnr.
      <fs_item_pedido>-destinatario-cnpj                  = lwa_solicitacao-cnpj_lr.
      <fs_item_pedido>-destinatario-cpf                   = lwa_solicitacao-cpf_lr.
      <fs_item_pedido>-destinatario-ie                    = lwa_solicitacao-ie_lr.
      <fs_item_pedido>-destinatario-endereco-logradouro   = lwa_solicitacao-stras_lr.
      <fs_item_pedido>-destinatario-endereco-numero       = lwa_solicitacao-house_num1_lr.
      <fs_item_pedido>-destinatario-endereco-cep          = lwa_solicitacao-pstlz_lr.
      <fs_item_pedido>-destinatario-endereco-bairro       = lwa_solicitacao-ort02_lr.
      <fs_item_pedido>-destinatario-endereco-cidade       = lwa_solicitacao-ort01_lr.
      <fs_item_pedido>-destinatario-endereco-uf           = lwa_solicitacao-uf_lr.

      ADD <fs_item_pedido>-valor_total_item TO lwa_pedido_saida_luft-pedido-valor_total.
    ENDLOOP.

    TRY.
        zcl_int_ob_cria_ped_saida_luft=>zif_integracao_outbound~get_instance( )->execute_request(
          EXPORTING
            i_info_request    = lwa_pedido_saida_luft
          IMPORTING
            e_id_integracao   = DATA(lva_id_integracao)
            e_integracao      = DATA(lwa_integracao)
        ).
      CATCH zcx_integracao INTO DATA(ex_integracao).
        r_msg_error = ex_integracao->get_text( ).
        RETURN.
      CATCH zcx_error INTO DATA(ex_error).
        r_msg_error = ex_error->get_text( ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD del_lote_preenchimento_auto.

    CHECK i_nro_cg IS NOT INITIAL.

    "Excluir Lotes Gravados
    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single   = i_nro_cg
      IMPORTING
        e_lotes             = DATA(lit_lotes)
        e_romaneios         = DATA(lit_romaneios)
    ).

    CHECK lit_romaneios[] IS INITIAL.

    LOOP AT lit_lotes ASSIGNING FIELD-SYMBOL(<fs_lote>) WHERE preenchimento_auto EQ abap_true.
      DELETE FROM zsdt0134 WHERE vbeln    = <fs_lote>-vbeln
                             AND posnr    = <fs_lote>-posnr
                             AND charg    = <fs_lote>-charg
                             AND nro_cg   = <fs_lote>-nro_cg
                             AND nr_fase  = <fs_lote>-nr_fase
                             AND nr_rot   = <fs_lote>-nr_rot.
    ENDLOOP.

  ENDMETHOD.


  METHOD del_sol_carga_safra_control.

    DATA: lo_carga_safra_control TYPE REF TO zcl_int_ob_safra_crt_crg_itens,
          ls_carga_itens         TYPE zsds394.

    CLEAR: r_msg_error.

    "Montar Requisição
    CLEAR: ls_carga_itens.

    ls_carga_itens-externalid = i_header_carga-id_carga_safra_control.

    LOOP AT i_zsdt0131_t INTO DATA(lwa_solicitacao).
      APPEND INITIAL LINE TO ls_carga_itens-order_items ASSIGNING FIELD-SYMBOL(<fs_carga_item>).
      <fs_carga_item> = lwa_solicitacao-nro_sol && lwa_solicitacao-seq.
    ENDLOOP.

    TRY.
        CREATE OBJECT lo_carga_safra_control.

        CALL METHOD lo_carga_safra_control->set_metodo_http
          EXPORTING
            i_metodo = 'DELETE'.

        CALL METHOD lo_carga_safra_control->zif_integracao_outbound~execute_request
          EXPORTING
            i_info_request  = ls_carga_itens
          IMPORTING
            e_id_integracao = DATA(lv_id_integracao)
            e_integracao    = DATA(lwa_integracao).

      CATCH zcx_integracao INTO DATA(lo_integracao).
        r_msg_error = lo_integracao->get_text( ).
        r_msg_error = |Retorno Safra Control: { r_msg_error }|.
        RETURN.
      CATCH zcx_error INTO DATA(lo_error).
        r_msg_error = lo_error->get_text( ).
        r_msg_error = |Retorno Safra Control: { r_msg_error }|.
        RETURN.
    ENDTRY.


  ENDMETHOD.


  METHOD enviar_documentos_carguero.

    DATA: var_answer TYPE c.

    DATA: lwa_zlest0185         TYPE zlest0185,
          lwa_nfe_terceiro      TYPE zmme0252,
          lwa_nfe_embarcador    TYPE zless0007,
          lwa_response_consulta TYPE zlese0274.

    DATA obj_nfe TYPE REF TO zcl_nfe_inbound.

    DATA: t_doctos_faltantes  TYPE zsdt_doctos_faltantes.

    DATA: t_pdf_faturamento_carga TYPE zsdt_pdf_files.

    DATA: lit_saida    TYPE zde_les_saida_zsdt0001_t,
          lwa_zsdt0001 TYPE zde_les_zsdt0001,
          lwa_saida    TYPE zde_les_saida_zsdt0001.

    CHECK i_nro_cg IS NOT INITIAL.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single   = i_nro_cg
        i_dados_conferencia = abap_true
      IMPORTING
        e_cargas            = DATA(lit_cargas)
        e_notas_conferencia = DATA(lit_notas_conferencia)
        e_romaneios         = DATA(lit_romaneios)
    ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_cargas[] IS INITIAL.
      r_msg_error = 'Carga não encontrada!'.
      RETURN.
    ENDIF.

    IF lwa_carga-integrar_carguero IS INITIAL.
      r_msg_error = 'Carga não integrada ao Carguero!'.
      RETURN.
    ENDIF.

    IF lwa_carga-viagem_aprovada_carguero IS NOT INITIAL.
      r_msg_error = 'Documentação já enviada ao Carguero!'.
      RETURN.
    ENDIF.

    IF lwa_carga-viagem_id IS INITIAL.
      r_msg_error = 'Carga sem Viagem do Carguero!'.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS INITIAL.
      r_msg_error = 'Carga sem romaneios gerados!'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zlest0185 INTO lwa_zlest0185
     WHERE viagem_id = lwa_carga-viagem_id.

    IF sy-subrc NE 0.
      r_msg_error = |Viagem { lwa_carga-viagem_id } não encontrada!|.
      RETURN.
    ENDIF.

    CLEAR: t_pdf_faturamento_carga[].

    READ TABLE lit_romaneios ASSIGNING FIELD-SYMBOL(<fs_romaneio_check>) INDEX 1.
    IF NOT ( sy-subrc EQ 0 AND <fs_romaneio_check>-nro_nf_prod IS NOT INITIAL ).
      r_msg_error = |Faturamento da Carga não finalizado { lwa_carga-viagem_id }!|.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(lwa_doc_fiscal)
     WHERE docnum EQ @<fs_romaneio_check>-nro_nf_prod.

    IF sy-subrc NE 0.
      r_msg_error = |Faturamento da Carga não finalizado { lwa_carga-viagem_id }!|.
      RETURN.
    ENDIF.

    DATA(lva_chave_nfe_emb) = zcl_util_sd=>get_chave_docnum_fiscal( i_docnum = lwa_doc_fiscal-docnum ).
    IF NOT ( lva_chave_nfe_emb IS NOT INITIAL AND strlen( lva_chave_nfe_emb ) EQ 44 ).
      r_msg_error = |Chave Documento Fiscal { lwa_doc_fiscal-docnum } incompleta!|.
      RETURN.
    ENDIF.

    LOOP AT lit_romaneios ASSIGNING FIELD-SYMBOL(<fs_romaneio>).

      DATA(_docs_faltantes) = abap_false.
      CLEAR: t_doctos_faltantes[].

      "Obtem Documentos Faturamento Romaneio
      TRY.
          DATA(_lit_docs_romaneios) = zcl_faturamento=>zif_faturamento~get_instance(
                                         )->get_documentos_faturamento( EXPORTING i_ch_referencia  = <fs_romaneio>-ch_referencia ).

        CATCH zcx_faturamento.
        CATCH zcx_error.
      ENDTRY.

      "valida arquivos obrigatorios
      TRY.
          _docs_faltantes = zcl_faturamento=>zif_faturamento~get_instance(
                                 )->get_documentos_obrigatorios( EXPORTING i_ch_referencia    = <fs_romaneio>-ch_referencia
                                                                           t_pdf_files        = _lit_docs_romaneios
                                                                 IMPORTING t_doctos_faltantes = t_doctos_faltantes ).
        CATCH zcx_faturamento.
        CATCH zcx_error.
      ENDTRY.

      IF _docs_faltantes EQ abap_true.
        READ TABLE t_doctos_faltantes INTO DATA(lwa_doc_faltante) INDEX 1.
        IF t_doctos_faltantes[] IS NOT INITIAL AND lwa_doc_faltante-mensagem IS NOT INITIAL.
          r_msg_error = |Romaneio { <fs_romaneio>-nr_romaneio }! { lwa_doc_faltante-mensagem }|.
        ELSE.
          r_msg_error = |Romaneio { <fs_romaneio>-nr_romaneio } com documentos faltantes!|.
        ENDIF.

        RETURN.
      ENDIF.

      APPEND LINES OF _lit_docs_romaneios TO t_pdf_faturamento_carga[].
    ENDLOOP.

    DATA(lit_anexos_carga) = zcl_gos_service_utils=>get_anexos_objeto( EXPORTING
                                                                               i_class_name  =  'ZSDT0112'
                                                                               i_obj_key     =  CONV #( i_nro_cg ) ).

    LOOP AT lit_anexos_carga ASSIGNING FIELD-SYMBOL(<fs_anexo_carga>).
      TRANSLATE <fs_anexo_carga>-docuclass TO UPPER CASE.
      CHECK <fs_anexo_carga>-docuclass EQ 'PDF'.

      CHECK NOT ( <fs_anexo_carga>-descript CS 'Autorização Embarque' ) AND <fs_anexo_carga>-descript IS NOT INITIAL.

      DATA(lva_xstring_anexo) = zcl_gos_service_utils=>get_xstring_anexo( EXPORTING i_document_id = CONV #( <fs_anexo_carga>-loio_id ) ).

      APPEND INITIAL LINE TO t_pdf_faturamento_carga ASSIGNING FIELD-SYMBOL(<fs_pdf_file>).
      <fs_pdf_file>-data     = lva_xstring_anexo.
      <fs_pdf_file>-len      = xstrlen( <fs_pdf_file>-data ).
      <fs_pdf_file>-filename = <fs_anexo_carga>-descript.
      <fs_pdf_file>-tipo_doc = '99'.
    ENDLOOP.

    "Agrupa documentos
    TRY.
        DATA(merged_document) = zcl_faturamento=>zif_faturamento~get_instance(
                              )->get_merge_pdf( EXPORTING t_pdf_files = t_pdf_faturamento_carga
                              ).

      CATCH zcx_faturamento.
        r_msg_error = 'Não foi possivel agrupar os documentos!'.
        RETURN.
      CATCH zcx_error.
        r_msg_error = 'Não foi possivel agrupar os documentos!'.
        RETURN.
    ENDTRY.

    IF merged_document IS INITIAL.
      r_msg_error = 'Não foi possivel agrupar os documentos!'.
      RETURN.
    ENDIF.

    CLEAR: var_answer.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'ATENÇÃO!'
        text_question         = 'Documentos de faturamento da Carga, serão disponibilizados para a transportadora no sistema Carguero/Strada. Deseja visualizar documentos antes do envio?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF var_answer EQ '1'.
      CALL FUNCTION 'ZSMARTFORMS_PDF_FILE_PREVIEW'
        EXPORTING
          pdf_data = merged_document.
    ENDIF.

    CLEAR: var_answer.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'ATENÇÃO'
        text_question         = 'ESSA OPERAÇÃO É IRREVERSÍVEL. DESEJA CONTINUAR?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK var_answer EQ '1'.

*-----------------------------------------------------------------------------------------------------*
*  Obter Autorização no Storage do Carguero para Upload da Documentação do Carga
*-----------------------------------------------------------------------------------------------------*

    TRY.
        DATA(lva_url_upload_storage) =  zcl_int_ob_carguero_tn_apr_ter=>zif_int_ob_carguero_tn_apr_ter~get_url_documento( EXPORTING i_viagem_id  = lwa_zlest0185-viagem_id ).

        "Carega(autentica) a URL do Documento de Viagem.
        zcl_integracao_upload_auth=>zif_integracao_upload_auth~get_instance(
          )->set_empresa(       EXPORTING i_bukrs         = lwa_zlest0185-bukrs
          )->set_id_referencia( EXPORTING i_id_referencia = lwa_zlest0185-viagem_id
          )->set_blob_path(     EXPORTING i_blob_path     = CONV #( lva_url_upload_storage )
          )->get_json(          IMPORTING e_json          = DATA(vl_json_tn)
          )->set_ds_data(       EXPORTING i_json          = vl_json_tn
          )->set_ds_url(
          )->set_send_msg(      IMPORTING e_id_integracao = DATA(vl_id_integracao)
                                          e_url_upload    = DATA(vl_url_upload)
          ).
      CATCH zcx_integracao INTO DATA(ex_integracao_tn).
      CATCH zcx_error INTO DATA(ex_error_tn).
    ENDTRY.

    IF vl_url_upload IS INITIAL.
      r_msg_error = |Erro ao obter atorização Storage Carguero|.
      RETURN.
    ENDIF.

*-----------------------------------------------------------------------------------------------------*
*   Realizar Upload do Arquivo no Storage
*-----------------------------------------------------------------------------------------------------*

    TRY.
        zcl_integracao_upload_exec=>zif_integracao_upload_exec~get_instance(
          )->set_empresa(       EXPORTING i_bukrs         = lwa_zlest0185-bukrs
          )->set_id_referencia( EXPORTING i_id_referencia = lwa_zlest0185-viagem_id
          )->get_json(          EXPORTING i_file_bin      = merged_document
                                IMPORTING e_json_xstring  = DATA(vl_json_tn_xstring)
          )->set_ds_url(        EXPORTING i_url_upload    = vl_url_upload
          )->set_ds_data(       EXPORTING i_json_xstring  = vl_json_tn_xstring
          )->set_send_msg(      IMPORTING e_id_integracao = vl_id_integracao
          ).

      CATCH zcx_integracao INTO DATA(ex_integracao_tn_ret).
        r_msg_error = |Erro ao enviar Documentos Carga para Storage Carguero|.
        RETURN.
      CATCH zcx_error INTO DATA(ex_error_tn_ret).
        r_msg_error = |Erro ao enviar Documentos Carga para Storage Carguero|.
        RETURN.
    ENDTRY.

    DATA(lva_embarque_luft) = zcl_carga_saida_insumos=>get_embarque_luft( i_nro_cg = i_nro_cg ).

    IF lva_embarque_luft EQ abap_true.

      "--------------------------------------------------------------------------------------------------------------------*
      " Carregamento Normal
      "--------------------------------------------------------------------------------------------------------------------*

      lwa_nfe_embarcador-chave_acesso        = lva_chave_nfe_emb.
      lwa_nfe_embarcador-data_emissao        = lwa_doc_fiscal-docdat.
      lwa_nfe_embarcador-quantidade_faturada = lwa_doc_fiscal-brgew.
      lwa_nfe_embarcador-url_documento       = lva_url_upload_storage.

      TRY .
          zcl_integracao_viagem_carregar=>zif_integracao_viagem_carregar~get_instance(
            )->set_viagem_carregar(
            EXPORTING
              i_viagem_id              = CONV #( lwa_zlest0185-viagem_id )
              i_dt_carregamento        = CONV #( lwa_doc_fiscal-docdat )    " Data Carregamento
              i_peso_tara              = CONV #( 1 )
              i_peso_liquido           = CONV #( lwa_carga-qtd_total_kg )
              i_nfe_carregamento       = lwa_nfe_embarcador
            ).
        CATCH zcx_integracao INTO DATA(ex_integracao).
          r_msg_error = |Erro ao enviar os dados de carregamento para o Carguero|.
          RETURN.
        CATCH zcx_error INTO DATA(ex_error2).
          r_msg_error = |Erro ao enviar os dados de carregamento para o Carguero|.
          RETURN.
      ENDTRY.


    ELSE.

      "--------------------------------------------------------------------------------------------------------------------*
      " Carregamento Troca Nota
      "--------------------------------------------------------------------------------------------------------------------*

      "Consultar Viagem Carguero
      TRY.
          zcl_int_ob_get_viagem_carguero=>zif_integracao_outbound~get_instance(
                             )->execute_request( EXPORTING i_info_request = lwa_zlest0185
                                                 IMPORTING e_integracao   = DATA(lwa_zintegracao) ).

          CHECK lwa_zintegracao-ds_data_retorno IS NOT INITIAL.

          /ui2/cl_json=>deserialize( EXPORTING json = lwa_zintegracao-ds_data_retorno CHANGING data = lwa_response_consulta ).

          IF lwa_response_consulta IS INITIAL.
            r_msg_error = |Erro ao Consultar a Viagem no Carguero: Erro { r_msg_error }|.
            RETURN.
          ENDIF.

        CATCH zcx_integracao INTO DATA(lwa_zcx_integracao). " Classe de Erro de Integração
          r_msg_error = lwa_zcx_integracao->get_text( ).
          r_msg_error = |Erro ao Consultar a Viagem no Carguero: Erro { r_msg_error }|.
          RETURN.
        CATCH zcx_error INTO DATA(lwa_zcx_error). " Classe de Erro de Integração
          r_msg_error = lwa_zcx_error->get_text( ).
          r_msg_error = |Erro ao Consultar a Viagem no Carguero: Erro { r_msg_error }|.
          RETURN.
      ENDTRY.


*-----------------------------------------------------------------------------------------------------*
*     Aprova viagem no Carguero
*-----------------------------------------------------------------------------------------------------*

      READ TABLE lwa_response_consulta-documents-fiscal INTO DATA(lwa_document_fiscal) INDEX 1.
      IF sy-subrc NE 0 OR lwa_document_fiscal-access_key IS INITIAL.
        r_msg_error = |Nota Fiscal não informada na Viagem { lwa_carga-viagem_id } no sistema Carguero|.
        RETURN.
      ENDIF.

      IF lwa_document_fiscal-issue_date IS INITIAL.
        r_msg_error = |Data Emissão da Nota Fiscal do Fornecedor não informada na Viagem { lwa_carga-viagem_id } no sistema Carguero|.
        RETURN.
      ENDIF.

      IF strlen( lwa_document_fiscal-issue_date ) < 10 .
        r_msg_error = |Data Emissão da Nota Fiscal do Fornecedor inválida na Viagem { lwa_carga-viagem_id } no sistema Carguero|.
        RETURN.
      ENDIF.

      CLEAR: lwa_nfe_terceiro.
      lwa_nfe_terceiro-chave_nfe        = lva_chave_nfe_emb.
      lwa_nfe_terceiro-dt_emissao       = lwa_doc_fiscal-docdat.
      lwa_nfe_terceiro-numero           = lwa_doc_fiscal-nfenum.
      lwa_nfe_terceiro-serie            = lwa_doc_fiscal-series.
      lwa_nfe_terceiro-qtde_faturada    = lwa_doc_fiscal-brgew.

      TRY .
          zcl_int_ob_carguero_tn_apr_ter=>zif_int_ob_carguero_tn_apr_ter~get_instance(
            )->aprovar_viagem(
            EXPORTING
              i_viagem_id       = lwa_carga-viagem_id
              i_dt_carregamento = sy-datum
              i_peso_bruto      = CONV #( lwa_response_consulta-weight_ticket-gross_weight )
              i_peso_liquido    = CONV #( lwa_response_consulta-weight_ticket-net_weight )
              i_peso_tara       = CONV #( lwa_response_consulta-weight_ticket-tara_weight )
              i_nfe_terceiro    = lwa_nfe_terceiro
            ).

        CATCH zcx_integracao INTO DATA(ex_integracao_v2).
          r_msg_error = ex_integracao_v2->get_text( ).
          r_msg_error = |Não foi possível aprovar a Viagem no Carguero: Erro { r_msg_error }|.
          RETURN.
        CATCH zcx_error INTO DATA(ex_error_v2).
          r_msg_error = ex_error_v2->get_text( ).
          r_msg_error = |Não foi possível aprovar a Viagem no Carguero! Erro: { r_msg_error }|.
          RETURN.
      ENDTRY.

*-----------------------------------------------------------------------------------------------------*
*     Confirmar Envio Doc. Aprovação Storage
*-----------------------------------------------------------------------------------------------------*

      IF lwa_carga-inco1 = 'CIF'.

        TRY .
            zcl_integracao_upload_conf=>zif_integracao_upload_conf~get_instance(
              )->set_empresa(       EXPORTING i_bukrs         = lwa_zlest0185-bukrs
              )->set_id_referencia( EXPORTING i_id_referencia = lwa_zlest0185-viagem_id
              )->set_ds_url(        EXPORTING i_viagem_id     = lwa_zlest0185-viagem_id
              )->set_send_msg(      IMPORTING e_id_integracao = DATA(l_id_integracao)
              ).

          CATCH zcx_integracao INTO DATA(ex_integracao_tn_con).
            r_msg_error = ex_integracao_tn_con->get_text( ).
            r_msg_error = |Não foi possível confirmar o envio dos documentos na Viagem no Carguero! Erro: { r_msg_error }|.
            RETURN.
          CATCH zcx_error INTO DATA(ex_error_tn_con).
            r_msg_error = ex_error_tn_con->get_text( ).
            r_msg_error = |Não foi possível confirmar o envio dos documentos na Viagem no Carguero! Erro: { r_msg_error }|.
            RETURN.
        ENDTRY.

      ENDIF.


    ENDIF.

    UPDATE zsdt0133 SET docs_env_carguero        = abap_true
                        viagem_aprovada_carguero = abap_true
                        dt_env_doc_cg            = sy-datum
                        hr_env_doc_cg            = sy-uzeit
                        us_env_doc_cg            = sy-uname
     WHERE nro_cg = i_nro_cg.

    MESSAGE 'Documentação da Carga disponibilizada com sucesso no sistema Carguero/Strada!' TYPE 'S'.

  ENDMETHOD.


  METHOD executar_troca_nota.

    DATA obj_nfe TYPE REF TO zcl_nfe_inbound.

    DATA: lit_saida    TYPE zde_les_saida_zsdt0001_t,
          lwa_zsdt0001 TYPE zde_les_zsdt0001,
          lwa_saida    TYPE zde_les_saida_zsdt0001.

    CHECK i_nro_cg IS NOT INITIAL.

    CHECK zcl_carga_saida_insumos=>check_embarque_armazem( i_nro_cg = i_nro_cg ) EQ abap_false. "Só Embarque Fornecedor

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single   = i_nro_cg
        i_dados_conferencia = abap_true
      IMPORTING
        e_cargas            = DATA(lit_cargas)
        e_notas_conferencia = DATA(lit_notas_conferencia)
        e_romaneios         = DATA(lit_romaneios)
    ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_cargas[] IS INITIAL.
      r_msg_error = 'Carga não encontrada!'.
      RETURN.
    ENDIF.

    IF lit_notas_conferencia[] IS INITIAL.
      r_msg_error = 'Carga sem notas para movimentação de entrada!'.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS INITIAL.
      r_msg_error = 'Carga sem romaneios para movimentação de entrada!'.
      RETURN.
    ENDIF.

    LOOP AT lit_notas_conferencia ASSIGNING FIELD-SYMBOL(<fs_nota_conf>) WHERE ck_fiscal EQ abap_false.
      r_msg_error = |NF-e { <fs_nota_conf>-chave_nfe } sem aceite fiscal!|.
      RETURN.
    ENDLOOP.


    DATA(_error_entrada_migo_miro) = abap_false.
    DATA(_error_gerar_remessa)     = abap_false.

*-------------------------------------------------------------------------------------------------------*
*   Gerar Migo e Miro
*-------------------------------------------------------------------------------------------------------*
    LOOP AT lit_notas_conferencia ASSIGNING <fs_nota_conf>.

      TRY.
          FREE obj_nfe.
          CREATE OBJECT obj_nfe.

          CLEAR: sy-msgid.

          zcl_miro=>get_proximo_venc_fatura(
             IMPORTING
                e_data_vencimento = DATA(e_data_vencimento)
             EXCEPTIONS
               erro               = 1
               OTHERS             = 2 ).

          DATA(r_data_venc) = zcl_miro=>get_proximo_dia_util( EXPORTING i_data_base = e_data_vencimento
                                                                        i_signum    = '+' ).

          obj_nfe->zif_cadastro~set_registro( i_id_registro = <fs_nota_conf>-chave_nfe ).
          obj_nfe->set_dt_vencimento( i_dt_vencimento = r_data_venc ).
          obj_nfe->set_aceitar_fisico( ).
          obj_nfe->set_aceitar_faturar( ).
          "obj_nfe->ck_ignora_data_se_vencimento = abap_true.

          IF obj_nfe->zif_cadastro~gravar_registro( ) EQ abap_false.
            IF sy-msgid IS NOT INITIAL.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lva_msg_entrada).
              r_msg_error = |Erro entrada NF-e { <fs_nota_conf>-chave_nfe }! Mensagem: { lva_msg_entrada } |.
            ELSE.
              r_msg_error = |Não foi possivel gerar a Migo e Miro para chave NF-e { <fs_nota_conf>-chave_nfe } |.
            ENDIF.
          ENDIF.

        CATCH zcx_cadastro INTO DATA(lo_cadastro).
          r_msg_error = lo_cadastro->get_text( ).

        CATCH zcx_nfe_inbound_exception INTO DATA(lo_nfe_inbound).
          r_msg_error = lo_nfe_inbound->get_text( ).

        CATCH zcx_pedido_compra_exception INTO DATA(lo_pedido_compra).
          r_msg_error = lo_pedido_compra->get_text( ).
      ENDTRY.

      DATA(lc_cabecalho) = obj_nfe->get_cabecalho_nota( ).

      IF NOT ( lc_cabecalho-mblnr IS NOT INITIAL AND lc_cabecalho-belnr IS NOT INITIAL ).
        _error_entrada_migo_miro = abap_true.
      ENDIF.

      IF lc_cabecalho-mblnr IS NOT INITIAL AND lc_cabecalho-belnr IS INITIAL.
        TRY.
            obj_nfe->nfe_inbound_cancela_fisico( ).
          CATCH zcx_cadastro .
          CATCH zcx_nfe_inbound_exception.
          CATCH zcx_pedido_compra_exception.
        ENDTRY.
      ENDIF.

      obj_nfe->free( ).

      IF _error_entrada_migo_miro EQ abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

*-------------------------------------------------------------------------------------------------------*
*    Gerar Movimento Remessa dos Romaneios
*-------------------------------------------------------------------------------------------------------*
    IF _error_entrada_migo_miro EQ abap_false.

      LOOP AT lit_romaneios ASSIGNING FIELD-SYMBOL(<fs_romaneio>).

        CLEAR: lit_saida, lwa_zsdt0001, lwa_saida.

        CHECK <fs_romaneio>-doc_rem IS INITIAL.

        PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102 USING <fs_romaneio>-ch_referencia '05'. "Cockpit Sementes
        PERFORM f_saida                   IN PROGRAM zlesr0102.
        PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING lit_saida lwa_zsdt0001 lwa_saida   .

        DO 4 TIMES.

          DATA(l_index) = sy-index.

          TRY.
              PERFORM f_action_user_remessa IN PROGRAM zlesr0102 USING lwa_saida.
            CATCH zcx_error INTO DATA(ex_error).
              IF l_index = 4.
                DATA(l_mesg) = ex_error->msgv1 && ex_error->msgv2 && ex_error->msgv3 && ex_error->msgv4.
                r_msg_error = |Erro Gerar Remessa Romaneio { <fs_romaneio>-ch_referencia }! Mensagem: { l_mesg } |.
                _error_gerar_remessa = abap_true.
                EXIT.
              ELSE.
                WAIT UP TO 5 SECONDS.
              ENDIF.
          ENDTRY.

          PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102 USING <fs_romaneio>-ch_referencia '05'. "Cockpit Sementes
          PERFORM f_saida                   IN PROGRAM zlesr0102.
          PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING lit_saida lwa_zsdt0001 lwa_saida.
          PERFORM f_repare_docs_romaneio    IN PROGRAM zlesr0102 CHANGING lwa_saida.
          IF lwa_saida-remessa IS NOT INITIAL AND lwa_saida-remessa(1) <> '@'.
            <fs_romaneio>-doc_rem = lwa_saida-remessa.
            EXIT.
          ENDIF.

        ENDDO.

      ENDLOOP.

    ENDIF.

*-------------------------------------------------------------------------------------------------------*
*   Estornar Todos Movimentos caso algo der errado
*-------------------------------------------------------------------------------------------------------*
    IF _error_entrada_migo_miro EQ abap_true OR
       _error_gerar_remessa     EQ abap_true.

      "Estornar Remessas Romaneios
      LOOP AT lit_romaneios ASSIGNING <fs_romaneio>.

        CLEAR: lit_saida, lwa_zsdt0001, lwa_saida.

        CHECK <fs_romaneio>-doc_rem IS NOT INITIAL.

        DO 50 TIMES.
          SELECT SINGLE *
            FROM likp INTO @DATA(lwa_likp)
           WHERE vbeln EQ @<fs_romaneio>-doc_rem.

          IF sy-subrc EQ 0.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.

        PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102 USING <fs_romaneio>-ch_referencia '05'. "Cockpit Sementes
        PERFORM f_saida                   IN PROGRAM zlesr0102.
        PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING lit_saida lwa_zsdt0001 lwa_saida.

        DO 4 TIMES.
          l_index = sy-index.

          TRY.
              PERFORM f_estorno_nfe IN PROGRAM zlesr0102 USING lwa_saida.
            CATCH zcx_error INTO ex_error.
              IF l_index = 4.
                EXIT.
              ELSE.
                WAIT UP TO 5 SECONDS.
              ENDIF.
          ENDTRY.

          PERFORM f_selecao_fat_autom       IN PROGRAM zlesr0102 USING <fs_romaneio>-ch_referencia '05'. "Cockpit Sementes
          PERFORM f_saida                   IN PROGRAM zlesr0102.
          PERFORM f_recuperar_dados         IN PROGRAM zlesr0102 CHANGING lit_saida lwa_zsdt0001 lwa_saida.
          PERFORM f_repare_docs_romaneio    IN PROGRAM zlesr0102 CHANGING lwa_saida.
          IF lwa_saida-remessa IS INITIAL OR lwa_saida-remessa(1) = '@'.
            CLEAR: <fs_romaneio>-doc_rem.
            EXIT.
          ENDIF.
        ENDDO.

      ENDLOOP.

      "Estornar Miro e Migo
      LOOP AT lit_notas_conferencia ASSIGNING <fs_nota_conf>.

        TRY.
            FREE obj_nfe.
            CREATE OBJECT obj_nfe.
            obj_nfe->zif_cadastro~set_registro( i_id_registro = <fs_nota_conf>-chave_nfe ).
            lc_cabecalho = obj_nfe->get_cabecalho_nota( ).

            IF ( lc_cabecalho-belnr IS NOT INITIAL ).
              obj_nfe->nfe_inbound_cancela_fatura( ).
            ENDIF.

            IF ( lc_cabecalho-mblnr IS NOT INITIAL ).
              obj_nfe->nfe_inbound_cancela_fisico(  ).
            ENDIF.

          CATCH zcx_cadastro INTO lo_cadastro.
          CATCH zcx_nfe_inbound_exception INTO lo_nfe_inbound.
          CATCH zcx_pedido_compra_exception INTO lo_pedido_compra.
        ENDTRY.

        obj_nfe->free( ).

      ENDLOOP.

    ENDIF.


  ENDMETHOD.


  METHOD exec_faturamento_saida_auto.

    DATA: lwa_request       TYPE zlese0200,
          lwa_response      TYPE zlese0241,
          lva_msg_completa  TYPE zintegracao,
          lva_id_integracao	TYPE zde_id_integracao.

    DATA: lob_faturamento_automatico TYPE REF TO zcl_faturamento_automatico.

    CHECK i_nro_cg IS NOT INITIAL.

    r_msg_error = zcl_carga_saida_insumos=>check_entrada_nfe_pendente( i_nro_cg = i_nro_cg ).

    CHECK r_msg_error IS INITIAL.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single   = i_nro_cg
        i_dados_conferencia = abap_true
      IMPORTING
        e_cargas            = DATA(lit_cargas)
        e_notas_conferencia = DATA(lit_notas_conferencia)
        e_romaneios         = DATA(lit_romaneios)
    ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_cargas[] IS INITIAL.
      r_msg_error = 'Carga não encontrada!'.
      RETURN.
    ENDIF.

    IF lit_romaneios[] IS INITIAL.
      r_msg_error = 'Carga sem romaneios!'.
      RETURN.
    ENDIF.

    CREATE OBJECT lob_faturamento_automatico.

    lob_faturamento_automatico->at_faturamento_interno_sap = abap_true.
    lob_faturamento_automatico->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.

    lwa_request-inicia_faturamento = abap_true.
    lwa_request-preenchimento_info_automatica = abap_true.

    LOOP AT lit_romaneios ASSIGNING FIELD-SYMBOL(<fs_romaneio_fat>).
      APPEND INITIAL LINE TO lwa_request-romaneio ASSIGNING FIELD-SYMBOL(<fs_romaneio>).
      <fs_romaneio>-ch_referencia_romaneio = <fs_romaneio_fat>-ch_referencia.
    ENDLOOP.

    DATA(lwa_json) = /ui2/cl_json=>serialize( EXPORTING data = lwa_request ).

    TRY.
        lob_faturamento_automatico->set_executar_faturamento(
          EXPORTING
            i_msg_inbound          = lwa_json
            i_msg_completa         = lva_msg_completa
            i_id_integracao        = lva_id_integracao
           IMPORTING
             e_msg_outbound        = DATA(e_msg_outbound)
             e_sucesso             = DATA(e_sucesso)
             e_nm_code             = DATA(e_nm_code)
             e_msg_erro            = DATA(e_msg_erro)
        ).
      CATCH zcx_integracao. " Classe de Erro de Integração
      CATCH zcx_error.      " Classe de Erro Genérica
    ENDTRY.

    /ui2/cl_json=>deserialize( EXPORTING json = e_msg_outbound CHANGING data = lwa_response ).

    IF lwa_response-status-codigo EQ 'SUCESSO'.
      MESSAGE 'Faturamento iniciado com sucesso!' TYPE 'S'.
    ELSE.
      r_msg_error = lwa_response-status-mensagem.
    ENDIF.

  ENDMETHOD.


  METHOD gerar_seq_carregamento_luft.

    DATA: lva_seq_car_luft_null TYPE zsdt0131-seq_carregamento_luft.

    DATA: lva_seq_car_luft_cont TYPE zsdt0131-seq_carregamento_luft.

    CLEAR: r_msg_error.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single   = i_nro_cg
      IMPORTING
        e_cargas            = DATA(lit_cargas)
        e_notas_conferencia = DATA(lit_notas_conferencia)
        e_romaneios         = DATA(lit_romaneios)
        e_dados_conferencia = DATA(lit_dados_conferencia)
        e_solicitacoes      = DATA(lit_solicitacoes)
        e_lotes             = DATA(lit_lotes)
        e_bordero           = DATA(lit_bordero)
    ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_cargas[] IS INITIAL.
      r_msg_error = 'Carga não encontrada!'.
      RETURN.
    ENDIF.

    IF lwa_carga-carga_conferida IS NOT INITIAL.
      r_msg_error = 'Carga já foi conferida!'.
      RETURN.
    ENDIF.

    IF lit_solicitacoes[] IS INITIAL.
      r_msg_error = 'Carga sem solicitaçõe!'.
      RETURN.
    ENDIF.

    UPDATE zsdt0131 SET seq_carregamento_luft = lva_seq_car_luft_null
     WHERE nro_lote EQ lwa_carga-nro_lote.

    LOOP AT lit_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao>).
      CLEAR: <fs_solicitacao>-seq_carregamento_luft.
    ENDLOOP.

    "Definir Seq. Carregamento LUFT por -> OV / Item / Roteiro
    DATA(lit_solicitacao_seq_luft) = lit_solicitacoes.
    SORT lit_solicitacao_seq_luft BY seq_entrega vbeln nr_rot.
    DELETE ADJACENT DUPLICATES FROM lit_solicitacao_seq_luft COMPARING seq_entrega vbeln nr_rot.

    lva_seq_car_luft_cont = 1.
    LOOP AT lit_solicitacao_seq_luft ASSIGNING FIELD-SYMBOL(<fs_solic_seq_luft>).
      <fs_solic_seq_luft>-seq_carregamento_luft = lva_seq_car_luft_cont.
      ADD 1 TO lva_seq_car_luft_cont.
    ENDLOOP.

    LOOP AT lit_solicitacoes ASSIGNING <fs_solicitacao>.

      READ TABLE lit_solicitacao_seq_luft ASSIGNING <fs_solic_seq_luft> WITH KEY seq_entrega  = <fs_solicitacao>-seq_entrega
                                                                                 vbeln        = <fs_solicitacao>-vbeln
                                                                                 nr_rot       = <fs_solicitacao>-nr_rot.

      IF NOT ( sy-subrc EQ 0 AND <fs_solic_seq_luft>-seq_carregamento_luft IS NOT INITIAL ).
        r_msg_error = 'Não foi possivel gerar a sequencia de carregamento LUFT'.
        RETURN.
      ENDIF.

      <fs_solicitacao>-seq_carregamento_luft = <fs_solic_seq_luft>-seq_carregamento_luft.

      UPDATE zsdt0131 SET seq_carregamento_luft = <fs_solicitacao>-seq_carregamento_luft
       WHERE nro_lote = <fs_solicitacao>-nro_lote
         AND nro_sol  = <fs_solicitacao>-nro_sol
         AND seq      = <fs_solicitacao>-seq
         AND kunnr    = <fs_solicitacao>-kunnr
         AND vbeln    = <fs_solicitacao>-vbeln
         AND posnr    = <fs_solicitacao>-posnr.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_agente_frete.

    SELECT SINGLE viagem_id, nro_cg, integrar_carguero, cod_transportadora
      FROM zsdt0133
      INTO @DATA(lwa_zsdt0133)
      WHERE nro_cg EQ @i_nro_cg.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE nro_cg, inco1
      FROM zsdt0129 INTO @DATA(lwa_zsdt0129)
     WHERE nro_cg EQ @i_nro_cg.

    CHECK sy-subrc EQ 0.

    "IF lwa_zsdt0133-integrar_carguero = abap_false.
    e_inco1 = lwa_zsdt0129-inco1.
    e_tdlnr = lwa_zsdt0133-cod_transportadora.
    " EXIT.
    "ENDIF.

*    CHECK lwa_zsdt0133-viagem_id IS NOT INITIAL.
*
*    SELECT SINGLE agente_frete
*      FROM zlest0185 INTO e_tdlnr
*     WHERE viagem_id EQ lwa_zsdt0133-viagem_id.
*
*    IF e_tdlnr IS NOT INITIAL.
*      e_inco1 = 'CPT'.
*      RETURN.
*    ENDIF.

*    SELECT SINGLE regio
*      FROM t001w
*      INTO @DATA(lv_regio)
*    WHERE werks EQ @i_branch.
*
*    CHECK lv_regio IS NOT INITIAL.
*
*    SELECT SINGLE tdlnr
*      FROM zlest0207
*      INTO e_tdlnr
*      WHERE bukrs EQ i_bukrs
*        AND branch EQ i_branch
*        AND regio EQ lv_regio.
*
*    CHECK sy-subrc IS NOT INITIAL.
*
*    SELECT SINGLE tdlnr
*      FROM zlest0207
*      INTO e_tdlnr
*      WHERE bukrs EQ i_bukrs
*        AND regio EQ lv_regio.
*
*    e_inco1 = 'CIF'.

  ENDMETHOD.


  METHOD get_origem_estoque_carga.

    CLEAR: r_origem_estoque.

    SELECT SINGLE nro_cg, nro_lote
      FROM zsdt0129 INTO @DATA(lwa_zsdt0129)
     WHERE nro_cg EQ @i_nro_cg.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zsdt0131 INTO @DATA(lwa_zsdt0131)
     WHERE nro_lote EQ @lwa_zsdt0129-nro_lote
       AND status NE 'X'.

    CHECK sy-subrc EQ 0.

    "1  Filial
    "2  Fornecedor
    "3  Armazem
    SELECT SINGLE origem_estoque
      FROM zsdt0082 INTO r_origem_estoque
     WHERE nro_sol  EQ lwa_zsdt0131-nro_sol
       AND seq      EQ lwa_zsdt0131-seq
       AND vbeln    EQ lwa_zsdt0131-vbeln
       AND posnr    EQ lwa_zsdt0131-posnr.



  ENDMETHOD.


  METHOD get_preenchimento_lote_manual.

    CLEAR: r_preenche_manual.

    SELECT SINGLE *
      FROM zsdt0133 INTO @DATA(lwa_zsdt0133)
     WHERE nro_cg EQ @i_nro_cg.

    DATA(lva_origem_estoque) = zcl_carga_saida_insumos=>get_origem_estoque_carga( i_nro_cg = i_nro_cg ).

    DATA(lva_embarque_integrado_luft) = abap_false.
    IF ( zcl_carga_saida_insumos=>get_embarque_luft( i_nro_cg = i_nro_cg ) = abap_true ) AND ( lwa_zsdt0133-nro_pedido_luft IS NOT INITIAL ).
      lva_embarque_integrado_luft = abap_true.
    ENDIF.

    IF NOT ( lva_origem_estoque EQ '2' OR "Fornecedor
             lva_embarque_integrado_luft EQ abap_true ).
      r_preenche_manual = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_solicitacoes_safra_control.

    DATA: lo_order_itens           TYPE REF TO zcl_int_ob_safra_crt_ord_itens,
          ls_order_itens           TYPE zde_safra_control_ordem_itens,
          ls_retorno_consulta_item TYPE zsds392.

    CLEAR: r_solicitacoes[], e_msg_error.

    LOOP AT i_zsdt0131_t INTO DATA(lwa_zsdt0131_current).

      CLEAR: ls_retorno_consulta_item.

      TRY .
          FREE lo_order_itens.
          CREATE OBJECT lo_order_itens.

          CALL METHOD lo_order_itens->set_metodo_http
            EXPORTING
              i_metodo = 'GET'.

          ls_order_itens-externalid = lwa_zsdt0131_current-nro_sol && lwa_zsdt0131_current-seq.

          CALL METHOD lo_order_itens->zif_integracao_outbound~execute_request
            EXPORTING
              i_info_request  = ls_order_itens
            IMPORTING
              e_id_integracao = DATA(lv_id_integracao)
              e_integracao    = DATA(lwa_integracao).


          /ui2/cl_json=>deserialize( EXPORTING json        = lwa_integracao-ds_data_retorno
                                               pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                      CHANGING data = ls_retorno_consulta_item ).

          IF ls_retorno_consulta_item IS NOT INITIAL.
            APPEND ls_retorno_consulta_item TO r_solicitacoes.
          ENDIF.

        CATCH zcx_integracao INTO DATA(lo_integracao).
          e_msg_error = lo_integracao->get_text( ).
          RETURN.
        CATCH zcx_error INTO DATA(lo_error).
          e_msg_error = lo_error->get_text( ).
          RETURN.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD post_sol_carga_safra_control.

    DATA: lo_carga_safra_control TYPE REF TO zcl_int_ob_safra_crt_crg_itens,
          ls_carga_itens         TYPE zsds393.

    CLEAR: r_msg_error.

    "Montar Requisição
    CLEAR: ls_carga_itens.

    ls_carga_itens-externalid = i_header_carga-id_carga_safra_control.

    LOOP AT i_zsdt0131_t INTO DATA(lwa_solicitacao).
      APPEND INITIAL LINE TO ls_carga_itens-itens ASSIGNING FIELD-SYMBOL(<fs_carga_item>).
      <fs_carga_item>-quantity               = lwa_solicitacao-qtd_vinc.
      <fs_carga_item>-order_item-external_id = lwa_solicitacao-nro_sol && lwa_solicitacao-seq.
    ENDLOOP.

    TRY.
        CREATE OBJECT lo_carga_safra_control.

        CALL METHOD lo_carga_safra_control->set_metodo_http
          EXPORTING
            i_metodo = 'POST'.

        CALL METHOD lo_carga_safra_control->zif_integracao_outbound~execute_request
          EXPORTING
            i_info_request  = ls_carga_itens
          IMPORTING
            e_id_integracao = DATA(lv_id_integracao)
            e_integracao    = DATA(lwa_integracao).

      CATCH zcx_integracao INTO DATA(lo_integracao).
        r_msg_error = lo_integracao->get_text( ).
        r_msg_error = |Retorno Safra Control: { r_msg_error }|.
        RETURN.
      CATCH zcx_error INTO DATA(lo_error).
        r_msg_error = lo_error->get_text( ).
        r_msg_error = |Retorno Safra Control: { r_msg_error }|.
        RETURN.
    ENDTRY.


  ENDMETHOD.


  METHOD preencher_lotes_carga.

    TYPES: BEGIN OF ty_lotes_nfe_zmm0110,
             chave_nfe    TYPE zib_nfe_dist_itm-chave_nfe,
             prod_item    TYPE zib_nfe_dist_itm-prod_item,
             charg        TYPE zsdt0134-charg,
             nro_cg       TYPE zsdt0134-nro_cg,
             nr_fase      TYPE zsdt0134-nr_fase,
             categoria    TYPE zsdt0134-categoria,
             lfimg        TYPE zsdt0134-lfimg,
             brgew        TYPE zsdt0134-brgew,
             peso_liq_brt TYPE zsdt0134-peso_liq_brt.
    TYPES: END OF ty_lotes_nfe_zmm0110.

    DATA: lit_lotes_gravar TYPE zsdt0134_t.
    DATA: lit_lotes_gravar_group TYPE zsdt0134_t.
    DATA: lwa_ov_lote_save TYPE zsdt0134.
    DATA: lva_categoria    TYPE zsdt0134-categoria.
    DATA: lit_lotes_nfe_zmm0110 TYPE TABLE OF ty_lotes_nfe_zmm0110.


    DATA: lit_zib_nfe_dist_lot TYPE TABLE OF zib_nfe_dist_lot.
    DATA: lit_fases_lote TYPE TABLE OF zmmt0102.

    CLEAR: r_msg_error, lit_lotes_gravar[].

    DATA(_preencher_lote_manual) = zcl_carga_saida_insumos=>get_preenchimento_lote_manual( i_nro_cg = i_nro_cg ).
    DATA(_embarque_luft)         = zcl_carga_saida_insumos=>get_embarque_luft( i_nro_cg = i_nro_cg ).
    DATA(_embarque_armazem)      = zcl_carga_saida_insumos=>check_embarque_armazem( i_nro_cg = i_nro_cg ).

    CHECK _preencher_lote_manual EQ abap_false.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single   = i_nro_cg
        i_dados_conferencia = abap_true
      IMPORTING
        e_cargas            = DATA(lit_cargas)
        e_notas_conferencia = DATA(lit_notas_conferencia)
        e_romaneios         = DATA(lit_romaneios)
        e_dados_conferencia = DATA(lit_dados_conferencia)
        e_solicitacoes      = DATA(lit_solicitacoes)
        e_lotes             = DATA(lit_lotes)
        e_bordero           = DATA(lit_bordero)
    ).

    READ TABLE lit_cargas INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_cargas[] IS INITIAL.
      r_msg_error = 'Carga não encontrada!'.
      RETURN.
    ENDIF.

    CHECK lit_lotes[] IS INITIAL.

    IF lwa_carga-carga_conferida IS INITIAL.
      r_msg_error = 'Carga não foi conferida!'.
      RETURN.
    ENDIF.

    IF lit_dados_conferencia[] IS INITIAL.
      r_msg_error = 'Carga não foi conferida!'.
      RETURN.
    ENDIF.

*------------------------------------------------------------------------------------------------------------------------------*
*   Montar Lotes
*------------------------------------------------------------------------------------------------------------------------------*

    IF _embarque_luft EQ abap_true. "Embarque LUFT

      LOOP AT lit_dados_conferencia ASSIGNING FIELD-SYMBOL(<fs_conferencia>).

        CLEAR: lit_zib_nfe_dist_lot[].

        READ TABLE lit_solicitacoes INTO DATA(lwa_solicitacao) WITH KEY nro_sol   = <fs_conferencia>-nro_sol
                                                                        seq       = <fs_conferencia>-seq.
        IF sy-subrc NE 0.
          r_msg_error = 'Registro Solicitação não encontrado'.
          RETURN.
        ENDIF.

        READ TABLE lit_bordero INTO DATA(lwa_bordero) WITH KEY id_autorizacao_embarque   = <fs_conferencia>-nro_cg
                                                               id_item                   = <fs_conferencia>-id_item_carregamento
                                                               lote                      = <fs_conferencia>-lote_carregamento.
        IF sy-subrc NE 0.
          r_msg_error = 'Registro Bordero não encontrado'.
          RETURN.
        ENDIF.

        IF lwa_bordero-quantidade IS INITIAL.
          r_msg_error = 'Quantidade Lote Bordero não pode ser vazia!'.
          RETURN.
        ENDIF.

        DATA(lva_peso_bag_lote) = zcl_charg=>get_valor_caracteristica(
           EXPORTING
             iv_nome_attr = 'PESO_BAG'
             iv_matnr     = CONV #( lwa_bordero-matnr )
             iv_charg     = CONV #( lwa_bordero-lote )
         ).

        TRANSLATE lva_peso_bag_lote TO UPPER CASE.
        REPLACE ALL OCCURRENCES OF ','  IN lva_peso_bag_lote WITH '.'.
        REPLACE ALL OCCURRENCES OF 'KG' IN lva_peso_bag_lote WITH space.
        CONDENSE lva_peso_bag_lote NO-GAPS.

        DATA(lva_categoria_lote) = zcl_charg=>get_valor_caracteristica(
          EXPORTING
            iv_nome_attr = 'SEMENTE_CATEGORIA'
            iv_matnr     = CONV #( lwa_bordero-matnr )
            iv_charg     = CONV #( lwa_bordero-lote )
        ).

        CLEAR: lit_fases_lote[].

        IF lva_categoria_lote IS INITIAL.
          r_msg_error = |Lote { lwa_bordero-lote } do Bordero sem catacteristica SEMENTE CATEGORIA!|.
          RETURN.
        ENDIF.

        lva_categoria = lva_categoria_lote.
        REPLACE ALL OCCURRENCES OF '.' IN lva_categoria WITH space.
        CONDENSE lva_categoria NO-GAPS.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lva_categoria
          IMPORTING
            output = lva_categoria.

        IF lva_peso_bag_lote IS INITIAL.
          r_msg_error = |Lote { lwa_bordero-lote } do Bordero sem catacteristica PESO BAG!|.
          RETURN.
        ENDIF.

        APPEND INITIAL LINE TO lit_lotes_gravar ASSIGNING FIELD-SYMBOL(<fs_lote_gravar>).

        <fs_lote_gravar>-vbeln                = lwa_solicitacao-vbeln.
        <fs_lote_gravar>-posnr                = lwa_solicitacao-posnr.
        <fs_lote_gravar>-nr_rot               = lwa_solicitacao-nr_rot.
        <fs_lote_gravar>-categoria            = lva_categoria.
        <fs_lote_gravar>-nr_fase              = <fs_conferencia>-nr_fase.
        <fs_lote_gravar>-charg                = <fs_conferencia>-lote_carregamento.
        <fs_lote_gravar>-lfimg                = <fs_conferencia>-quantidade.
        <fs_lote_gravar>-brgew                = lva_peso_bag_lote.
        <fs_lote_gravar>-peso_liq_brt         = <fs_lote_gravar>-lfimg * <fs_lote_gravar>-brgew.
        <fs_lote_gravar>-preenchimento_auto   = abap_true.

      ENDLOOP.

    ELSEIF _embarque_armazem IS INITIAL. "Embarque Fornecedor

      CLEAR: lit_zib_nfe_dist_lot[], lit_lotes_nfe_zmm0110[].

      "--------------------------------------------------------------------------------------------------->>>>>
      " Recuperar Lotes Preenchidos na ZMM0110 para as NF-e/Itens da conferencia
      "--------------------------------------------------------------------------------------------------->>>>>
      DATA(lit_nfe_conferencia) = lit_dados_conferencia[].
      SORT lit_nfe_conferencia BY chave_nfe prod_item.
      DELETE ADJACENT DUPLICATES FROM lit_nfe_conferencia COMPARING chave_nfe prod_item.

      SELECT *
        FROM zib_nfe_dist_lot INTO TABLE lit_zib_nfe_dist_lot
         FOR ALL ENTRIES IN lit_nfe_conferencia
       WHERE chave_nfe EQ lit_nfe_conferencia-chave_nfe
         AND prod_item EQ lit_nfe_conferencia-prod_item.

      LOOP AT lit_nfe_conferencia ASSIGNING FIELD-SYMBOL(<fs_nfe_conferencia>).

        READ TABLE lit_solicitacoes INTO lwa_solicitacao WITH KEY nro_sol   = <fs_nfe_conferencia>-nro_sol
                                                                  seq       = <fs_nfe_conferencia>-seq.
        IF sy-subrc NE 0.
          r_msg_error = 'Registro Solicitação não encontrado'.
          RETURN.
        ENDIF.

        READ TABLE lit_zib_nfe_dist_lot WITH KEY chave_nfe = <fs_nfe_conferencia>-chave_nfe
                                                 prod_item = <fs_nfe_conferencia>-prod_item TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          r_msg_error = |Chave NF-e { <fs_nfe_conferencia>-chave_nfe } item { <fs_nfe_conferencia>-prod_item } sem lotes informados na ZMM0110!|.
          RETURN.
        ENDIF.

        LOOP AT lit_zib_nfe_dist_lot ASSIGNING FIELD-SYMBOL(<fs_lote_item_nfe>) WHERE chave_nfe = <fs_nfe_conferencia>-chave_nfe
                                                                                  AND prod_item = <fs_nfe_conferencia>-prod_item.

          CLEAR: lit_fases_lote[].

          CHECK <fs_lote_item_nfe>-menge IS NOT INITIAL.

          SELECT SINGLE atwrt
            FROM zib_nfe_dist_lca INTO @DATA(lva_categoria_lot)
           WHERE cd_lote_item EQ @<fs_lote_item_nfe>-cd_lote_item
             AND atnam        EQ 'SEMENTE_CATEGORIA'.

          IF sy-subrc NE 0 OR lva_categoria_lot IS INITIAL.
            r_msg_error = |Chave NF-e { <fs_nfe_conferencia>-chave_nfe } item { <fs_nfe_conferencia>-prod_item } Lote { <fs_lote_item_nfe>-charg } sem catacteristica SEMENTE CATEGORIA na ZMM0110!|.
            RETURN.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lva_categoria_lot
            IMPORTING
              output = lva_categoria.

          SELECT SINGLE atwrt
            FROM zib_nfe_dist_lca INTO @DATA(lva_peso_bag)
           WHERE cd_lote_item EQ @<fs_lote_item_nfe>-cd_lote_item
             AND atnam        EQ 'PESO_BAG'.

          IF sy-subrc NE 0 OR lva_peso_bag IS INITIAL.
            r_msg_error = |Chave NF-e { <fs_nfe_conferencia>-chave_nfe } item { <fs_nfe_conferencia>-prod_item } Lote { <fs_lote_item_nfe>-charg } sem catacteristica PESO BAG na ZMM0110!|.
            RETURN.
          ENDIF.

          TRANSLATE lva_peso_bag TO UPPER CASE.
          REPLACE ALL OCCURRENCES OF '.'  IN lva_peso_bag WITH ''.
          REPLACE ALL OCCURRENCES OF ','  IN lva_peso_bag WITH '.'.
          REPLACE ALL OCCURRENCES OF 'KG' IN lva_peso_bag WITH space.
          CONDENSE lva_peso_bag NO-GAPS.

          SELECT SINGLE *
            FROM zib_nfe_dist_ter  INTO @DATA(lwa_zib_nfe_dist_ter)
           WHERE chave_nfe = @<fs_lote_item_nfe>-chave_nfe.

          SELECT *
            FROM zmmt0102 INTO TABLE lit_fases_lote
           WHERE ebeln   = lwa_solicitacao-ebeln
             AND ebelp   = lwa_solicitacao-ebelp
             AND matnr   = lwa_solicitacao-matnr
             AND line_id = <fs_nfe_conferencia>-prod_item
             AND charg   = <fs_lote_item_nfe>-charg
             AND mblnr   = lwa_zib_nfe_dist_ter-mblnr
             AND mjahr   = lwa_zib_nfe_dist_ter-mjahr
             AND tcode   = 'ZMM0110'.

          IF lit_fases_lote[] IS NOT INITIAL.

            LOOP AT lit_fases_lote ASSIGNING FIELD-SYMBOL(<fs_fase_lote>).

              APPEND INITIAL LINE TO lit_lotes_nfe_zmm0110 ASSIGNING FIELD-SYMBOL(<fs_lote_nfe_zmm0110>).

              <fs_lote_nfe_zmm0110>-chave_nfe          = <fs_nfe_conferencia>-chave_nfe.
              <fs_lote_nfe_zmm0110>-prod_item          = <fs_nfe_conferencia>-prod_item.
              <fs_lote_nfe_zmm0110>-categoria          = lva_categoria.
              <fs_lote_nfe_zmm0110>-charg              = <fs_lote_item_nfe>-charg.
              <fs_lote_nfe_zmm0110>-nr_fase            = <fs_fase_lote>-nr_fase.
              <fs_lote_nfe_zmm0110>-lfimg              = <fs_fase_lote>-menge.
              <fs_lote_nfe_zmm0110>-brgew              = lva_peso_bag.
              <fs_lote_nfe_zmm0110>-peso_liq_brt       = <fs_lote_nfe_zmm0110>-lfimg * <fs_lote_nfe_zmm0110>-brgew.

            ENDLOOP.

          ELSE.

            APPEND INITIAL LINE TO lit_lotes_nfe_zmm0110 ASSIGNING <fs_lote_nfe_zmm0110>.

            <fs_lote_nfe_zmm0110>-chave_nfe            = <fs_nfe_conferencia>-chave_nfe.
            <fs_lote_nfe_zmm0110>-prod_item            = <fs_nfe_conferencia>-prod_item.
            <fs_lote_nfe_zmm0110>-categoria            = lva_categoria.
            <fs_lote_nfe_zmm0110>-charg                = <fs_lote_item_nfe>-charg.
            <fs_lote_nfe_zmm0110>-lfimg                = <fs_lote_item_nfe>-menge.
            <fs_lote_nfe_zmm0110>-brgew                = lva_peso_bag.
            <fs_lote_nfe_zmm0110>-peso_liq_brt         = <fs_lote_nfe_zmm0110>-lfimg * <fs_lote_nfe_zmm0110>-brgew.

          ENDIF.

        ENDLOOP.

      ENDLOOP.

      "--------------------------------------------------------------------------------------------------->>>>>
      " Popular estrutura de Lotes para Gravar na Carga
      "--------------------------------------------------------------------------------------------------->>>>>
      LOOP AT lit_dados_conferencia ASSIGNING <fs_conferencia>.

        CHECK <fs_conferencia>-quantidade > 0.

        READ TABLE lit_solicitacoes INTO lwa_solicitacao WITH KEY nro_sol   = <fs_conferencia>-nro_sol
                                                                  seq       = <fs_conferencia>-seq.
        IF sy-subrc NE 0.
          r_msg_error = 'Registro Solicitação não encontrado'.
          RETURN.
        ENDIF.

        LOOP AT lit_lotes_nfe_zmm0110 ASSIGNING <fs_lote_nfe_zmm0110> WHERE chave_nfe = <fs_conferencia>-chave_nfe
                                                                        AND prod_item = <fs_conferencia>-prod_item.

          CHECK <fs_conferencia>-quantidade > 0.
          CHECK <fs_lote_nfe_zmm0110>-lfimg > 0.

          APPEND INITIAL LINE TO lit_lotes_gravar ASSIGNING <fs_lote_gravar>.

          <fs_lote_gravar>-vbeln              = lwa_solicitacao-vbeln.
          <fs_lote_gravar>-posnr              = lwa_solicitacao-posnr.
          <fs_lote_gravar>-nr_rot             = lwa_solicitacao-nr_rot.
          <fs_lote_gravar>-categoria          = <fs_lote_nfe_zmm0110>-categoria.
          <fs_lote_gravar>-charg              = <fs_lote_nfe_zmm0110>-charg.
          <fs_lote_gravar>-nr_fase            = <fs_lote_nfe_zmm0110>-nr_fase.
          <fs_lote_gravar>-brgew              = <fs_lote_nfe_zmm0110>-brgew.
          <fs_lote_gravar>-preenchimento_auto = abap_true.


          IF <fs_lote_nfe_zmm0110>-lfimg >= <fs_conferencia>-quantidade.
            <fs_lote_gravar>-lfimg              = <fs_conferencia>-quantidade.
            <fs_lote_gravar>-peso_liq_brt       = <fs_lote_gravar>-lfimg * <fs_lote_gravar>-brgew.
          ELSE.
            <fs_lote_gravar>-lfimg              = <fs_lote_nfe_zmm0110>-lfimg.
            <fs_lote_gravar>-peso_liq_brt       = <fs_lote_gravar>-lfimg * <fs_lote_gravar>-brgew.
          ENDIF.

          SUBTRACT <fs_lote_gravar>-lfimg  FROM <fs_lote_nfe_zmm0110>-lfimg.
          SUBTRACT <fs_lote_gravar>-lfimg  FROM <fs_conferencia>-quantidade.

        ENDLOOP.

      ENDLOOP.


    ENDIF.

*------------------------------------------------------------------------------------------------------------------------------*
*   Gravar Lotes por OV Item Roteiro
*------------------------------------------------------------------------------------------------------------------------------*

    DATA(_lit_lotes_gravar_group_ov) = lit_lotes_gravar[].
    SORT _lit_lotes_gravar_group_ov BY vbeln posnr nr_rot.
    DELETE ADJACENT DUPLICATES FROM _lit_lotes_gravar_group_ov COMPARING vbeln posnr nr_rot.

    DATA(_lit_lotes_gravar_group_key) = lit_lotes_gravar[].
    SORT _lit_lotes_gravar_group_key BY vbeln posnr charg nro_cg nr_fase nr_rot.
    DELETE ADJACENT DUPLICATES FROM _lit_lotes_gravar_group_key COMPARING vbeln posnr charg nro_cg nr_fase nr_rot.


    LOOP AT _lit_lotes_gravar_group_ov ASSIGNING FIELD-SYMBOL(<fs_gravar_lote_ov>).

      CLEAR: lwa_ov_lote_save, lit_lotes_gravar_group[].

      lwa_ov_lote_save-vbeln  = <fs_gravar_lote_ov>-vbeln.
      lwa_ov_lote_save-posnr  = <fs_gravar_lote_ov>-posnr.
      lwa_ov_lote_save-nr_rot = <fs_gravar_lote_ov>-nr_rot.

      "Montar Lotes
      LOOP AT _lit_lotes_gravar_group_key ASSIGNING FIELD-SYMBOL(<fs_group_key>) WHERE vbeln  = <fs_gravar_lote_ov>-vbeln
                                                                                   AND posnr  = <fs_gravar_lote_ov>-posnr
                                                                                   AND nr_rot = <fs_gravar_lote_ov>-nr_rot.

        APPEND INITIAL LINE TO lit_lotes_gravar_group ASSIGNING FIELD-SYMBOL(<fs_lote_gravar_group>).

        MOVE-CORRESPONDING <fs_group_key> TO <fs_lote_gravar_group>.

        CLEAR: <fs_lote_gravar_group>-lfimg, <fs_lote_gravar_group>-peso_liq_brt.

        LOOP AT lit_lotes_gravar ASSIGNING <fs_lote_gravar> WHERE vbeln     = <fs_lote_gravar_group>-vbeln
                                                              AND posnr     = <fs_lote_gravar_group>-posnr
                                                              AND charg     = <fs_lote_gravar_group>-charg
                                                              AND nro_cg    = <fs_lote_gravar_group>-nro_cg
                                                              AND nr_fase   = <fs_lote_gravar_group>-nr_fase
                                                              AND nr_rot    = <fs_lote_gravar_group>-nr_rot.

          ADD <fs_lote_gravar>-lfimg        TO <fs_lote_gravar_group>-lfimg.
          ADD <fs_lote_gravar>-peso_liq_brt TO <fs_lote_gravar_group>-peso_liq_brt.
        ENDLOOP.

      ENDLOOP.

      zcl_carga_saida_insumos=>gravar_carga(
      EXPORTING
        i_header              = lwa_carga
        i_solicitacoes        = lit_solicitacoes
        i_lotes               = lit_lotes_gravar_group
        i_lote_ov_save        = lwa_ov_lote_save
      IMPORTING
        e_carga               = DATA(lva_nro_carga)
        e_msg_erro            = DATA(lva_msg_error)  ).

      IF lva_msg_error IS NOT INITIAL.
        r_msg_error = | Não foi possivel preencher os lotes automaticamente da OV: { lva_msg_error }|.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0133 INTO @DATA(lwa_zsdt0133_gravou)
       WHERE nro_cg EQ @i_nro_cg.

      IF sy-subrc EQ 0.
        lwa_carga-data_edit = lwa_zsdt0133_gravou-data_edit.
        lwa_carga-hora_edit = lwa_zsdt0133_gravou-hora_edit.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD rollback_carga.

    "Reversão da Alteração. (Não há como usar rollback, porque há execução de um commit dentro do requisição de APi dentro da chamada do metodo anterior )

    IF i_registro_new EQ abap_true.

      "Cabeçalho Lote
      DELETE FROM zsdt0129 WHERE nro_lote EQ i_nro_lote.

      "Cabeçalho Carga
      DELETE FROM zsdt0133 WHERE nro_cg EQ i_nro_cg.

      "Clientes Lote
      DELETE FROM zsdt0130 WHERE nro_lote EQ i_nro_lote.

      "Ordens/Solicitações do Lote
      DELETE FROM zsdt0131 WHERE nro_lote EQ i_nro_lote.

      COMMIT WORK.

    ELSE.

      "Cabeçalho Lote
      IF i_zsdt0129_old IS NOT INITIAL.
        MODIFY zsdt0129 FROM i_zsdt0129_old.
      ENDIF..

      "Cabeçalho Carga
      IF i_zsdt0133_old IS NOT INITIAL.
        MODIFY zsdt0133 FROM i_zsdt0133_old.
      ENDIF.

      "Clientes Lote
      IF i_zsdt0130_t_old[] IS NOT INITIAL.
        DELETE FROM zsdt0130 WHERE nro_lote EQ i_zsdt0129_old-nro_lote.
        MODIFY zsdt0130 FROM TABLE i_zsdt0130_t_old.
      ENDIF.

      "Ordens/Solicitações do Lote
      IF i_zsdt0131_t_old[] IS NOT INITIAL.
        DELETE FROM zsdt0131 WHERE nro_lote EQ i_zsdt0129_old-nro_lote.
        MODIFY zsdt0131 FROM TABLE i_zsdt0131_t_old.
      ENDIF.

      "Notas Carga
      IF i_zsdt0410_t_old[] IS NOT INITIAL.
        DELETE FROM zsdt0410 WHERE nro_cg EQ i_zsdt0129_old-nro_cg.
        MODIFY zsdt0410 FROM TABLE i_zsdt0410_t_old.
      ENDIF.

      "Lotes Carga
      IF i_zsdt0134_t_old[] IS NOT INITIAL.
        LOOP AT i_lotes ASSIGNING FIELD-SYMBOL(<fs_lote>).
          DELETE FROM zsdt0134 WHERE vbeln   = <fs_lote>-vbeln
                                 AND posnr   = <fs_lote>-posnr
                                 AND charg   = <fs_lote>-charg
                                 AND nro_cg  = <fs_lote>-nro_cg
                                 AND nr_fase = <fs_lote>-nr_fase
                                 AND nr_rot  = <fs_lote>-nr_rot.
        ENDLOOP.
        MODIFY zsdt0134 FROM TABLE i_zsdt0134_t_old.
      ENDIF.

      COMMIT WORK.

    ENDIF.

  ENDMETHOD.


  METHOD val_qtde_sol_safra_control.

    DATA: lva_saldo_item   TYPE zsdt0131-qtd_vinc,
          lva_qtd_item     TYPE zsdt0131-qtd_vinc,
          lva_qtd_in_carga TYPE zsdt0131-qtd_vinc.

    CLEAR: r_msg_error, e_solicitacoes_safra[].

    zcl_carga_saida_insumos=>get_solicitacoes_safra_control(
      EXPORTING
        i_zsdt0131_t   = i_solicitacoes_current
       IMPORTING
        e_msg_error    = DATA(_msg_error_get_sol)
      RECEIVING
        r_solicitacoes = DATA(lit_solicitacoes_safra)
    ).

    IF _msg_error_get_sol IS NOT INITIAL.
      r_msg_error = |Não foi possivel consultar as solicitações no Safra Control: Motivo: { _msg_error_get_sol }|.
      RETURN.
    ENDIF.

    e_solicitacoes_safra = lit_solicitacoes_safra[].

    LOOP AT i_solicitacoes_current INTO DATA(lwa_zsdt0131_current).

      DATA(_externalid) = lwa_zsdt0131_current-nro_sol && lwa_zsdt0131_current-seq.

      READ TABLE lit_solicitacoes_safra INTO DATA(ls_retorno_consulta_item) WITH KEY external_id = _externalid.
      IF sy-subrc NE 0.
        r_msg_error = |Não foi possivel consultar Solicitação { lwa_zsdt0131_current-nro_sol } Seq: { lwa_zsdt0131_current-seq } no Safra Control|.
        RETURN.
      ENDIF.

      IF lwa_zsdt0131_current-status = 'X'.
        CONTINUE. "Cancelado
      ENDIF.

      lva_qtd_item       = ls_retorno_consulta_item-quantity.
      lva_qtd_in_carga   = ls_retorno_consulta_item-quantity_in_cargo.

      "Se carga atual já consumiu o saldo da solicitação, retorna o saldo consumido pela carga para fazer calculo.
      READ TABLE i_solicitacoes_old INTO DATA(lwa_zsdt0131_old) WITH KEY nro_lote = lwa_zsdt0131_current-nro_lote
                                                                         nro_sol  = lwa_zsdt0131_current-nro_sol
                                                                         seq      = lwa_zsdt0131_current-seq
                                                                         kunnr    = lwa_zsdt0131_current-kunnr
                                                                         vbeln    = lwa_zsdt0131_current-vbeln.
      IF sy-subrc EQ 0.
        SUBTRACT lwa_zsdt0131_old-qtd_vinc FROM lva_qtd_in_carga.
      ENDIF.

      lva_saldo_item = lva_qtd_item - lva_qtd_in_carga.

      IF lwa_zsdt0131_current-qtd_vinc > lva_saldo_item.
        r_msg_error = |Saldo da Solicitação { lwa_zsdt0131_current-nro_sol } Seq: { lwa_zsdt0131_current-seq } no Safra Control : { lva_saldo_item }|.
        r_msg_error = |{ r_msg_error } inferior ao informado na Carga: { lwa_zsdt0131_current-qtd_vinc }|.
        RETURN.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD gravar_ajuste_frete.

    DATA: lv_preco_frete_max TYPE dmbtr.

    FREE: r_msg_error, lv_preco_frete_max.

    SELECT SINGLE *
      INTO @DATA(_tvarvc)
      FROM tvarvc
     WHERE name = 'ZSDT0112_AJUSTE_FRETE_MAX'.

    IF sy-subrc = 0.
      lv_preco_frete_max = zcl_util=>get_string_numeric( CONV #( _tvarvc-low ) ).
    ENDIF.

    IF   i_header-preco_frete IS INITIAL OR
       ( i_header-preco_frete  > lv_preco_frete_max AND lv_preco_frete_max IS NOT INITIAL ).
      r_msg_error = 'Preco Frete Ultrapassa Limite: ' && _tvarvc-low.
      RETURN.
    ENDIF.

    IF i_header-motivo_ajuste_frete IS INITIAL.
      r_msg_error = 'Informar Motivo Ajuste do Frete!'.
      RETURN.
    ENDIF.

*----------------------------
*-- gravar ajuste
*----------------------------
    UPDATE zsdt0133 SET preco_frete         = i_header-preco_frete
                        motivo_ajuste_frete = i_header-motivo_ajuste_frete
                  WHERE nro_cg              = i_header-nro_cg.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD habilitar_edicao_frete.

    CLEAR: r_msg_error.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single =  i_nro_carga
      IMPORTING
        e_cargas          =  DATA(lit_carga)
        e_romaneios       =  DATA(lit_romaneios)
        e_lotes           =  DATA(lit_lotes)
    ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    LOOP AT lit_romaneios INTO DATA(lwa_romaneios).
      IF lwa_romaneios-doc_rem IS NOT INITIAL.
        r_msg_error = 'Carga com Remessa Gerada! Operação não permitida!'.
        RETURN.
      ENDIF.
    ENDLOOP.

    r_msg_error = zcl_carga_saida_insumos=>check_permissao_carga(
        EXPORTING i_nro_carga = i_nro_carga
                  i_atividade = '15' ).

  ENDMETHOD.
ENDCLASS.
