class ZCL_CARGA_ENTRADA_INSUMOS definition
  public
  final
  create public .

public section.

  class-methods DEFINIR_ITENS_CARGA
    importing
      !I_NRO_SOLICITACAO type ZDE_NRO_SOL optional
      !I_NRO_CARGA type ZSDT0346-NRO_CARGA optional
      !I_TP_SALDO type ZDE_TP_SALDO optional
    exporting
      !I_ITENS_CARGA type ZMMTT0202 .
  class-methods BLOQUEIO_DESBLOQUEIO_CARGA
    importing
      !I_NRO_CG type ZMMT0201-NRO_CG
      !BLOQUEIO type FLAG
    exporting
      !MSG type STRING .
  class-methods ANEXAR_DOCUMENTOS_CARGA
    importing
      !I_NRO_CARGA type ZDE_NRO_CG
      !I_ARQUIVOS type ZMMT_ARQUIVOS
    exporting
      !E_MSG_ERRO type STRING .
  class-methods ENVIA_PATH_AUTORIZA_EMBARQUE
    importing
      !I_CARGA type ZNRO_CG
      !I_PDF_XSTRING type XSTRING optional
    exceptions
      ERRO_UPLOAD_NAO_AUTORIZADO
      ERRO_UPLOAD .
  class-methods GERAR_AUTORIZACAO_EMBARQUE
    importing
      !I_NRO_CG type ZNRO_CG
      !I_APROVA_VIAGEM_CARGUERO type CHAR01 optional
      !I_BACKGROUND type CHAR01 optional
      !I_ENVIA_AUTORIZACAO_CARGUERO type CHAR01 optional
    exporting
      !E_MSG_ERROR type STRING
    returning
      value(R_SUCESSO) type CHAR01 .
  class-methods DISPARA_EMAIL_FORNECEDOR
    importing
      !I_NRO_CG type ZNRO_CG
      !I_PDF_XSTRING type XSTRING .
  class-methods BUSCA_DADOS_MONTAR_CARGA
    importing
      !I_EMPRESA type FKK_RT_BUKRS optional
      !I_SEGMENTO type SPART optional
      !I_NRO_SOL type ZSDT_RANGE_NRO_SOL optional
      !I_DT_SOL type DATUM_RANGE_TAB optional
      !I_PEDIDO type EBELN_RANGE_TTY optional
    exporting
      !E_TABELA_MONTA_CARGA type ZSDT_MONTA_CARGA
      !E_MSG_ERRO type STRING
    returning
      value(SUCESSO) type CHAR1 .
  class-methods MONTAR_CARGA
    importing
      !I_EMPRESA type FKK_RT_BUKRS
      !I_SEGMENTO type SPART
      !I_NRO_SOL type ZSDT_RANGE_NRO_SOL
      !I_DT_SOL type DATUM_RANGE_TAB
      !I_PEDIDO type EBELN_RANGE_TTY
    exporting
      !E_TABELA_MONTA_CARGA type ZSDT_MONTA_CARGA
      !E_MSG_ERRO type STRING
    returning
      value(SUCESSO) type CHAR1 .
  class-methods BUSCA_DADOS_CARGA
    importing
      !I_CARGA type ZSDT_RANGE_NRO_CG optional
      !I_EMPRESA type FKK_RT_BUKRS optional
      !I_SEGMENTO type SPART optional
      !I_ID_VIAGEM type ZSDT_RANGE_ID_VIAGEM optional
      !I_DT_CARGA type DATUM_RANGE_TAB optional
      !I_STATUS type ZSDT_RANGE_STATUS optional
    exporting
      !E_CDS_CARGA type ANY
      !E_CARGAS type ZSDT_ALV_LISTA_CARGA
      !E_MSG_ERRO type STRING
      !E_NOTAS type ZMMT_0203
    returning
      value(SUCESSO) type CHAR1 .
  class-methods GRAVA_CARGA
    exporting
      !E_CARGA type ZNRO_CG
      !E_MSG_ERRO type STRING
    changing
      !I_DADOS type ZSDE_GRAVA_CARGA .
  class-methods ATUALIZA_DADOS_CARGA
    importing
      !I_NRO_CARGA type ZNRO_CG
      !I_HEADER type ZSDE_GRAVA_CARGA optional
      !I_ITENS type ZSDT_ITENS_CARGA optional
      !I_NOTAS_VENDA type ZSDT_NOTAS_VENDA optional
      !I_NOTAS_TRANSFERENCIA type ZSDT_NOTAS_TRANSF optional
    exporting
      !E_MSG_ERRO type STRING .
  class-methods CANCELA_CARGA
    importing
      !I_NRO_CARGA type ZNRO_CG
      !I_MOTIVO type STRING optional
    exporting
      !E_MSG_ERRO type STRING
    returning
      value(SUCESSO) type CHAR01 .
  class-methods ELIMINA_NOTAS_CARGA
    importing
      !I_NRO_CARGA type ZNRO_CG
      !I_TRANSFERENCIA type CHAR1
      !I_CHAVES type ZMMT_CHAVES_NOTA
    exporting
      !E_MSG_ERRO type STRING .
  class-methods CONFERIR_CARGA
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GET_SPART_CARGA
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_SPART) type SPART .
  class-methods GRAVAR_CARGA_CORE
    importing
      !I_DETERMINA_ITENS_AUTO type CHAR01 optional
    exporting
      !E_NRO_CARGA type ZSDT0346-NRO_CARGA
    changing
      !I_ZMMT0201 type ZMMT0201
      !I_ZMMT0202 type ZMMTT0202 optional
      !I_ZMMT0203 type ZMMTT0203 optional
      !I_ZSDT0391 type ZSDT0391 optional
      !I_ZSDT0346 type ZSDTT0346 optional
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods VALIDAR_DADOS
    importing
      !I_ZMMT0201 type ZMMT0201
      !I_ZMMT0202 type ZMMTT0202
      !I_ZMMT0203 type ZMMTT0203
      !I_ZSDT0391 type ZSDT0391
      !I_ZSDT0346 type ZSDTT0346
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods CHECK_REINICIALIZACAO_AUT_EMB
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
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
  class-methods HABILITAR_EDICAO_ITENS
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods HABILITAR_EDICAO_DADOS_LOGIST
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods HABILITAR_EDICAO_CHAVES
    importing
      !I_NRO_CARGA type ZNRO_CG
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods BUSCA_DADOS_CONFERENCIA
    importing
      !I_NRO_CARGA type ZNRO_CG
    exporting
      !E_BORDERO type ZMMT_DADOS_BORDERO
      !E_CARGA type ZMMT_DADOS_CARGA
      !E_NOTAS type ZMMT_DADOS_NOTAS_CARGA
      !E_DE_PARA_CONFERENCIA type ZMMT_DADOS_CONF_DE_PARA
      !E_MSG_ERRO type STRING .
  class-methods CONFERIR_CARGA_V2
    importing
      !I_NRO_CARGA type ZNRO_CG
    exporting
      !E_MSG_ERRO type STRING .
  class-methods VALIDACOES_CONFERENCIA
    importing
      !I_NRO_CARGA type ZNRO_CG
    exporting
      !E_MSG_ERRO type STRING .
  class-methods VINCULAR_CONFERENCIA
    importing
      !I_NRO_CARGA type ZNRO_CG
      !I_BORDERO type ZMME_ALV_DADOS_BORDERO optional
      !I_CARGA type ZMME_ALV_DADOS_CARGA
      !I_NOTAS type ZMME_ALV_DADOS_NOTAS_CARGA
    exporting
      !E_ZMMT0218 type ZMMT0218
      !E_MSG_ERRO type STRING .
  class-methods INFORMA_QTD_CONFERENCIA
    importing
      !I_DE_PARA type ZMME_ALV_DADOS_CONF_DE_PARA
      !I_NRO_CARGA type ZNRO_CG
    exporting
      !E_MSG_ERRO type STRING
    changing
      !C_BORDERO type ZMME_ALV_DADOS_BORDERO optional
      !C_CARGA type ZMME_ALV_DADOS_CARGA optional
      !C_NOTAS type ZMME_ALV_DADOS_NOTAS_CARGA optional .
  class-methods DESVINCULA_CONFERENCIA
    importing
      !I_CONFERENCIA type ZMMTT0218
    exporting
      !E_MSG_ERRO type STRING .
  class-methods MONTA_DADOS_ACEITE_FISCAL
    importing
      !I_NOTAS type ZMMT_DADOS_NOTAS_CARGA
      !I_CARGA type ZMMT_DADOS_CARGA
      !I_BORDERO type ZMMT_DADOS_BORDERO optional
      !I_DE_PARA type ZMMT_DADOS_CONF_DE_PARA
      !I_CANCELAR_ACEITE type CHAR1 optional
    exporting
      !E_DADOS_ACEITE_FISCAL type ZMMT_DADOS_ACEITE_FISCAL
      !E_MSG_ERRO type STRING .
  class-methods INCONSISTENCIA_BORDERO
    importing
      !I_DADOS type ZSDE_INTEGRA_LUFT_ACEITE_RECEB
    exporting
      !E_MSG_ERRO type STRING .
  class-methods ACEITE_FISCAL
    importing
      !I_DADOS type ZMMT_ACEITE_FISCAL
    exporting
      !E_MSG_ERRO type STRING .
  class-methods CANCELAR_ACEITE
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
    exporting
      !E_MSG_ERRO type STRING .
  class-methods ACEITE_BORDERO
    importing
      !I_DADOS type ZMME_ACEITE_FISCAL
    exporting
      !E_MSG_ERRO type STRING .
  PROTECTED SECTION.
private section.

  class-methods GERAR_AUTORIZACAO_EMBARQUE_SM
    importing
      !I_CARGA type ZNRO_CG
      !I_BINARY type CHAR1 optional
      !I_PREVIEW type CHAR1 optional
    exporting
      !E_PDF_XSTRING type XSTRING .
  class-methods GERAR_AUTORIZACAO_EMB_CORE
    importing
      !I_NRO_CG type ZNRO_CG
      !I_APROVA_VIAGEM_CARGUERO type CHAR01 optional
      !I_ENVIA_AUTORIZACAO_CARGUERO type CHAR01 optional
      !I_BACKGROUND type CHAR01 optional
    exporting
      !E_MSG_ERROR type STRING
    returning
      value(R_SUCESSO) type CHAR01 .
ENDCLASS.



CLASS ZCL_CARGA_ENTRADA_INSUMOS IMPLEMENTATION.


  METHOD gerar_autorizacao_embarque_sm.


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

    "Tabelas
    DATA:it_fat TYPE TABLE OF zembarquefat,
         it_pro TYPE TABLE OF zembarquepro,
         it_com TYPE TABLE OF zembarquecom.

    "Variáveis
    DATA: vl_formname TYPE tdsfname,
          vl_name     TYPE rs38l_fnam.


    CLEAR: e_pdf_xstring.

    " Seleciona o cabeçalho
    SELECT SINGLE *
      FROM zmmt0201
      WHERE nro_cg = @i_carga
        AND cancel EQ @space
      INTO @DATA(ls_201).

    CHECK sy-subrc EQ 0 AND i_carga IS NOT INITIAL.

    SELECT SINGLE *
      FROM zmmt0196 INTO @DATA(lwa_zmmt0196)
      WHERE nro_sol EQ @ls_201-nro_sol.

    CHECK sy-subrc EQ 0 AND ls_201-nro_sol IS NOT INITIAL.

    lva_lifnr_filial = lwa_zmmt0196-werks.

    CHECK lva_lifnr_filial IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lva_lifnr_filial
      IMPORTING
        output = lva_lifnr_filial.

    " Seleciona os itens
    SELECT *
      FROM zmmt0202
      WHERE nro_cg = @i_carga
        AND cancel EQ @space
      INTO TABLE @DATA(lt_202).

    " Selecionada dados fornecedores
    IF ls_201 IS NOT INITIAL.

      SELECT l~lifnr,
             l~name1,
             l~stcd1,
             l~stcd2,
             l~stcd3,
             l~telf1,
             l~stras,
             l~ort01,
             l~regio,
             l~adrnr,
             l~mcod3,
             a6~smtp_addr    AS email,
             a2~tel_number   AS contato
        FROM lfa1 AS l
        LEFT JOIN adr6 AS a6 ON a6~addrnumber = l~adrnr
        LEFT JOIN adr2 AS a2 ON a2~addrnumber = l~adrnr
        INTO TABLE @DATA(lt_lfa1_completo)
        WHERE l~lifnr IN (
              @ls_201-cod_transportadora,
              @ls_201-cod_motorista,
              @ls_201-local_entrega,
              @ls_201-ponto_coleta,
              @lva_lifnr_filial ).


    ENDIF.

    " Seleciona dados Roteiro
    SELECT a~*
      FROM zsdt0132 AS a
      INNER JOIN zmmt0196 AS b
       ON a~nr_rot EQ b~rota_pc
      INNER JOIN zmmt0202 AS c
       ON b~nro_sol EQ c~nro_sol AND
          b~seq     EQ c~seq
      WHERE a~lifnr  EQ @ls_201-ponto_coleta
        AND c~nro_cg EQ @ls_201-nro_cg
      INTO TABLE @DATA(lt_132).

    "Seleciona dados Material
    SELECT mara~matnr,
           mara~meins,
           makt~maktx
      FROM mara
      INNER JOIN makt
        ON makt~matnr = mara~matnr
      INTO TABLE @DATA(lt_material)
      FOR ALL ENTRIES IN @lt_202
      WHERE mara~matnr = @lt_202-matnr
        AND makt~spras = @sy-langu.

    "Seleciona itens documento de compras
    SELECT ekko~ebeln,
           ekko~ihrez
      FROM ekko
      INNER JOIN ekpo
        ON ekpo~ebeln = ekko~ebeln
      INTO TABLE @DATA(lt_pedidos)
      FOR ALL ENTRIES IN @lt_202
      WHERE ekpo~ebeln = @lt_202-ebeln
        AND ekpo~ebelp = @lt_202-ebelp.


    "Preenche tabelas do Smartforms

    wa_top-nro_cg     = ls_201-nro_cg.
    wa_top-spart      = abap_false.
    wa_top-desc_spart = 'SEMENTES'.
    wa_top-dt_emissao = ls_201-date_create.
    wa_top-icon       = ls_201-inco1.

    " Transportadora
    READ TABLE lt_lfa1_completo WITH KEY lifnr = ls_201-cod_transportadora INTO DATA(ls_lfa1_t).
    IF sy-subrc = 0.
      wa_tra-transportadora = ls_lfa1_t-name1.
      wa_tra-cnpj           = ls_lfa1_t-stcd1.
    ELSE.
      CLEAR: wa_tra-transportadora, wa_tra-cnpj.
    ENDIF.

    " Motorista
    wa_tra-nome_motorista = ls_201-nome_motorista.
    READ TABLE lt_lfa1_completo WITH KEY lifnr = ls_201-cod_motorista INTO DATA(ls_lfa1_m).
    IF sy-subrc = 0.
      wa_tra-nome_motorista = ls_lfa1_m-name1.
      wa_tra-cpf            = ls_lfa1_m-stcd2.
      wa_tra-contato        = ls_lfa1_m-telf1.
    ELSE.
      CLEAR: wa_tra-cpf, wa_tra-contato.
    ENDIF.

    " Placas
    wa_tra-placa_cavalo    = ls_201-placa_cav.
    wa_tra-placa_carreta1  = ls_201-placa_car1.
    wa_tra-placa_carreta2  = ls_201-placa_car2.
    wa_tra-placa_carreta3  = ls_201-placa_car3.

    "it_fat
    READ TABLE lt_lfa1_completo WITH KEY lifnr = lva_lifnr_filial INTO DATA(ls_lfa1_p).
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO it_fat ASSIGNING FIELD-SYMBOL(<fs_fat>).
      IF <fs_fat> IS ASSIGNED.
        <fs_fat>-cliente    = ls_lfa1_p-name1.
        <fs_fat>-endereco   = ls_lfa1_p-stras.
        <fs_fat>-cpf_cnpj   = ls_lfa1_p-stcd1.
        <fs_fat>-ins_est    = ls_lfa1_p-stcd3.
        <fs_fat>-cidade     = ls_lfa1_p-ort01.
        <fs_fat>-uf         = ls_lfa1_p-regio.
        <fs_fat>-email      = ls_lfa1_p-email.
        <fs_fat>-contato    = ls_lfa1_p-contato.
      ENDIF.
    ENDIF.

    "it_pro
    LOOP AT lt_202 ASSIGNING FIELD-SYMBOL(<fs_202>).
      APPEND INITIAL LINE TO it_pro ASSIGNING FIELD-SYMBOL(<fs_pro>).

      IF <fs_pro> IS ASSIGNED.
        " Dados diretos de ZMMT0202
        <fs_pro>-nro_cg     = <fs_202>-nro_cg.
        <fs_pro>-qtd_vinc   = <fs_202>-qtd_vinc_carga.
        <fs_pro>-seq_ent_cg = <fs_202>-item_carga.

        " Buscar dados do material
        READ TABLE lt_material WITH KEY matnr = <fs_202>-matnr INTO DATA(ls_mat).
        IF sy-subrc = 0.
          <fs_pro>-um     = ls_mat-meins.
          <fs_pro>-maktx  = ls_mat-maktx.
        ENDIF.

        " Buscar dados da rota
        READ TABLE lt_132 WITH KEY lifnr = ls_201-ponto_coleta INTO DATA(ls_rot).
        IF sy-subrc = 0.
          <fs_pro>-local  = ls_rot-rot_desc.
          <fs_pro>-nr_rot = ls_rot-nr_rot.
        ENDIF.

        " Buscar fornecedor (Ponto de Coleta)
        READ TABLE lt_lfa1_completo WITH KEY lifnr = ls_201-ponto_coleta INTO ls_lfa1_p.
        IF sy-subrc = 0.
          <fs_pro>-name1 = ls_lfa1_p-name1.
          <fs_pro>-mcod3 = ls_lfa1_p-mcod3.
        ENDIF.

*        " Buscar dados do pedido
        READ TABLE lt_pedidos WITH KEY ebeln = <fs_202>-ebeln INTO DATA(ls_ped).
        IF sy-subrc = 0.
          <fs_pro>-wrkst = ls_ped-ihrez.
          <fs_pro>-ebeln = ls_ped-ebeln.
        ENDIF.
      ENDIF.
    ENDLOOP.


    APPEND INITIAL LINE TO it_com ASSIGNING FIELD-SYMBOL(<fs_com>).

    "Buscar fornecedor
    READ TABLE lt_lfa1_completo WITH KEY lifnr = ls_201-ponto_coleta ASSIGNING FIELD-SYMBOL(<fs_lfa1_f>).
    IF sy-subrc = 0.
      <fs_com>-fornecedor = <fs_lfa1_f>-name1.
      <fs_com>-endereco   = <fs_lfa1_f>-stras.
    ENDIF.

    " Buscar roteiro com base no ponto de coleta
    READ TABLE lt_132 WITH KEY lifnr = ls_201-ponto_coleta INTO DATA(ls_roteiro).
    IF sy-subrc = 0.
      <fs_com>-local_embarque = ls_roteiro-rot_desc.
      <fs_com>-municipio      = ls_roteiro-city1.
      <fs_com>-uf             = ls_roteiro-uf.
    ENDIF.

    "Smartforms
    vl_formname = 'ZSDF0017'.
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
*    EXIT.
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
        WHERE viagem_id = @ls_201-viagem_id.

    IF sy-subrc <> 0.
      CLEAR lv_id_localizador.
    ENDIF.
    "FF #184481 - fim

    "Executa a função de chamada do Formulário para gerar o PDF da Autorização de Embarque
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

* Transfer Importing Xstring to binary solix table
        lt_solix = cl_bcs_convert=>xstring_to_solix( iv_xstring = <fs_arquivos>-xstring ).

* Fill Doc Data
        ls_doc_data-doc_size  = xstrlen( <fs_arquivos>-xstring ).
        ls_doc_data-obj_descr = <fs_arquivos>-descricao.
        ls_doc_data-obj_name  = <fs_arquivos>-descricao.
        MOVE <fs_arquivos>-tipo TO ld_doc_type.

        lt_header = VALUE #( ( |&SO_FILENAME={ <fs_arquivos>-descricao }| )
                             ( |&SO_FORMAT=BIN | ) ).

* Create Document
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

* To link the document attachment and business object
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


  METHOD bloqueio_desbloqueio_carga.

    DATA: lv_bloq TYPE c.

    SELECT *
      FROM zmmt0202
      INTO TABLE @DATA(lt_0202)
      WHERE nro_cg = @i_nro_cg.
    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE *
        FROM zmmt0201
        INTO @DATA(lw_0201)
        WHERE nro_cg = @i_nro_cg.
    ENDIF.

    IF lt_0202 IS NOT INITIAL.

      IF bloqueio IS NOT INITIAL.

        LOOP AT lt_0202 ASSIGNING FIELD-SYMBOL(<fs_0202>).
          CALL METHOD zcl_solicitacao_entrada_insumo=>bloqueia_solicitacao
            EXPORTING
              nro_solicitacao = <fs_0202>-nro_sol
            IMPORTING
              bloqueado       = lv_bloq
              mensagem_erro   = msg.
          IF msg IS NOT INITIAL.
            EXIT.
          ENDIF.

        ENDLOOP.

        IF msg IS INITIAL.
          CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave          = i_nro_cg
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
        ENDIF.

      ELSE.

        LOOP AT lt_0202 ASSIGNING <fs_0202>.
          CALL METHOD zcl_solicitacao_entrada_insumo=>desbloqueia_solicitacao
            EXPORTING
              nro_solicitacao = <fs_0202>-nro_sol
            IMPORTING
              desbloqueado    = DATA(lv_desbloqueado).

        ENDLOOP.

        CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
          EXPORTING
            chave = i_nro_cg.

      ENDIF.

    ELSE.

      IF bloqueio IS NOT INITIAL.

        CALL METHOD zcl_solicitacao_entrada_insumo=>bloqueia_solicitacao
          EXPORTING
            nro_solicitacao = lw_0201-nro_sol
          IMPORTING
            bloqueado       = lv_bloq
            mensagem_erro   = msg.
        IF msg IS NOT INITIAL.
          EXIT.
        ELSE.

          CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave          = i_nro_cg
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
        ENDIF.

      ELSE.

        CALL METHOD zcl_solicitacao_entrada_insumo=>desbloqueia_solicitacao
          EXPORTING
            nro_solicitacao = lw_0201-nro_sol
          IMPORTING
            desbloqueado    = lv_desbloqueado.

        CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
          EXPORTING
            chave = i_nro_cg.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD definir_itens_carga.

    DATA: lv_total_kg TYPE zmmt0201-qtd_total_kg,
          lv_item     TYPE sy-tabix.

    DATA: lv_kg_disponivel TYPE zmmt0201-qtd_total_kg.

    CLEAR: i_itens_carga[].

    zcl_solicitacao_entrada_insumo=>saldo_solicitacao(  EXPORTING
                                                           i_nro_solicitacao = i_nro_solicitacao
                                                           i_nro_cg          = i_nro_carga
                                                           i_itens           = abap_true
                                                           i_tp_saldo        = i_tp_saldo
                                                       IMPORTING
                                                           e_saldo = DATA(lt_saldo) ).

    SELECT SINGLE *
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
     WHERE nro_cg eq @i_nro_carga.

    CHECK sy-subrc IS INITIAL.

    lv_kg_disponivel = lwa_zmmt0201-qtd_total_kg.

    LOOP AT lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).
      IF lv_kg_disponivel <= 0.
        EXIT. " Já atingiu o limite
      ENDIF.

      DATA(lv_qtd_alocar) = <fs_saldo>-saldo_kg.

      IF lv_qtd_alocar <= lv_kg_disponivel.

        APPEND INITIAL LINE TO i_itens_carga ASSIGNING FIELD-SYMBOL(<fs_item>).

        ADD 1 TO lv_item.
        <fs_item>-nro_cg            = i_nro_carga.
        <fs_item>-item_carga        = lv_item.
        <fs_item>-nro_sol           = <fs_saldo>-nro_solic.
        <fs_item>-seq               = <fs_saldo>-seq.
        <fs_item>-matnr             = <fs_saldo>-matnr.
        <fs_item>-ebeln             = <fs_saldo>-ebeln.
        <fs_item>-ebelp             = <fs_saldo>-ebelp.
        <fs_item>-tp_saldo_vinc     = <fs_saldo>-tp_saldo.
        <fs_item>-qtd_vinc_carga    = <fs_saldo>-saldo.
        <fs_item>-qtd_vinc_carga_kg = lv_qtd_alocar.
        <fs_item>-user_create       = sy-uname.
        <fs_item>-date_create       = sy-datum.
        <fs_item>-time_create       = sy-uzeit.

        SUBTRACT lv_qtd_alocar FROM lv_kg_disponivel.

      ELSE.
        DATA(lv_saldo_pos) = floor( lv_kg_disponivel / <fs_saldo>-embalagem ).
        DATA(lv_qtd_final)  = lv_saldo_pos * <fs_saldo>-embalagem.

        IF lv_qtd_final > 0.

          APPEND INITIAL LINE TO i_itens_carga ASSIGNING <fs_item>.

          ADD 1 TO lv_item.
          <fs_item>-nro_cg            = i_nro_carga.
          <fs_item>-item_carga        = lv_item.
          <fs_item>-nro_sol           = <fs_saldo>-nro_solic.
          <fs_item>-seq               = <fs_saldo>-seq.
          <fs_item>-ebeln             = <fs_saldo>-ebeln.
          <fs_item>-ebelp             = <fs_saldo>-ebelp.
          <fs_item>-tp_saldo_vinc     = <fs_saldo>-tp_saldo.
          <fs_item>-matnr             = <fs_saldo>-matnr.
          <fs_item>-qtd_vinc_carga    = lv_saldo_pos.
          <fs_item>-qtd_vinc_carga_kg = lv_qtd_final.
          <fs_item>-user_create       = sy-uname.
          <fs_item>-date_create       = sy-datum.
          <fs_item>-time_create       = sy-uzeit.

          SUBTRACT lv_qtd_final FROM lv_kg_disponivel.
        ENDIF.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD envia_path_autoriza_embarque.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : ENVIA_PATH_AUTORIZA_EMBARQUE                                                         *
*& Chamado        : USER STORY 169536                                                                    *
*& Data           : 22/04/2025                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 22/04/2025|DEVK9A2IAI |NSEGATIN       | API Integração Para Envio da Autorização de embarque para uma *
*&                                       | viagem no Carguero/Strada.                                    *
*--------------------------------------------------------------------------------------------------------*

    SELECT SINGLE a~bukrs, b~viagem_id
      FROM zmmt0201 AS a
      INNER JOIN zlest0185 AS b
       ON a~viagem_id EQ b~viagem_id
      INTO @DATA(el_viagem)
    WHERE nro_cg EQ @i_carga.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE *
      FROM zlest0185 INTO @DATA(lwa_zlest0185)
     WHERE viagem_id EQ @el_viagem-viagem_id.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
     WHERE nro_cg EQ @i_carga.

    CHECK sy-subrc EQ 0.

    TRY .
* Gera a URL do Documento de Viagem.
        DATA(vl_url) = lwa_zlest0185-blob_path && '/AutorizacaoEmbarque_' && i_carga && '.pdf'.

* Carega(autentica) a URL do Documento de Viagem.
        zcl_integracao_upload_auth=>zif_integracao_upload_auth~get_instance(
          )->set_empresa(       EXPORTING i_bukrs         = el_viagem-bukrs
          )->set_id_referencia( EXPORTING i_id_referencia = el_viagem-viagem_id
          )->set_blob_path(     EXPORTING i_blob_path     = CONV #( vl_url )
          )->get_json(          IMPORTING e_json          = DATA(vl_json_tn)
          )->set_ds_data(       EXPORTING i_json          = vl_json_tn
          )->set_ds_url(
          )->set_send_msg(      IMPORTING e_id_integracao = DATA(vl_id_integracao)
                                          e_url_upload    = DATA(vl_url_upload)
          ).

      CATCH zcx_integracao INTO DATA(ex_integracao_tn).
        IF NOT ( ex_integracao_tn->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
                 ex_integracao_tn->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
          RAISE erro_upload_nao_autorizado.

        ENDIF.

      CATCH zcx_error INTO DATA(ex_error_tn).
        RAISE erro_upload_nao_autorizado.

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

* Efetua upload no carguero
    TRY .
        zcl_integracao_upload_exec=>zif_integracao_upload_exec~get_instance(
          )->set_empresa(       EXPORTING i_bukrs         = el_viagem-bukrs
          )->set_id_referencia( EXPORTING i_id_referencia = el_viagem-viagem_id
          )->get_json(          EXPORTING i_file_bin      = vl_pdf_bin
                                IMPORTING e_json_xstring  = DATA(vl_json_tn_xstring)
          )->set_ds_url(        EXPORTING i_url_upload    = vl_url_upload
          )->set_ds_data(       EXPORTING i_json_xstring  = vl_json_tn_xstring
          )->set_send_msg(      IMPORTING e_id_integracao = vl_id_integracao
          ).

      CATCH zcx_integracao INTO DATA(ex_integracao_tn_ret).
        IF NOT ( ex_integracao_tn_ret->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
                 ex_integracao_tn_ret->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
          RAISE erro_upload.

        ENDIF.

      CATCH zcx_error INTO DATA(ex_error_tn_ret).
        IF NOT ( ex_error_tn_ret->msgv1 CS '0201' AND
                 ex_error_tn_ret->msgv1 CS 'Created' ).
          RAISE erro_upload.

        ENDIF.

    ENDTRY.

    UPDATE zmmt0201 SET ds_url_file_carguero = vl_url
     WHERE nro_cg EQ i_carga.

    COMMIT WORK.


  ENDMETHOD.


  METHOD aceite_fiscal.
    DATA lo_aceite TYPE REF TO zcl_nfe_inbound.

    DATA: lt_dados_aceite TYPE zsde_integra_luft_aceite_receb.

    DATA(lt_dados) = i_dados.
    SORT lt_dados BY chave_nfe.
    DELETE ADJACENT DUPLICATES FROM lt_dados COMPARING chave_nfe.

    DATA(lt_dados_aux) = i_dados.
    SORT lt_dados_aux BY chave_nfe.

    LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).

      zcl_carga_entrada_insumos=>aceite_bordero(
        EXPORTING
          i_dados    = <fs_dados>
        IMPORTING
          e_msg_erro = DATA(lva_msg_aceite) ).

      IF lva_msg_aceite IS NOT INITIAL.
        e_msg_erro = |Erro ao Aceitar a Nota fiscal { <fs_dados>-chave_nfe } na LUFT! Erro: { lva_msg_aceite }|.
        RETURN.
      ENDIF.

      TRY .
          CREATE OBJECT lo_aceite.

          CALL METHOD lo_aceite->zif_cadastro~set_registro
            EXPORTING
              i_id_registro = <fs_dados>-chave_nfe.

          lo_aceite->set_info_sap( ).

          READ TABLE lt_dados_aux TRANSPORTING NO FIELDS
          WITH KEY chave_nfe = <fs_dados>-chave_nfe
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            LOOP AT lt_dados_aux ASSIGNING FIELD-SYMBOL(<fs_dados_aux>) FROM sy-tabix.
              IF <fs_dados>-chave_nfe <> <fs_dados_aux>-chave_nfe.
                CLEAR lo_aceite.
                EXIT.
              ENDIF.

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

          ENDIF.

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

  ENDMETHOD.


  METHOD atualiza_dados_carga.

    DATA: ls_0201 TYPE zmmt0201,
          lt_0202 TYPE TABLE OF zmmt0202,
          lt_0203 TYPE TABLE OF zmmt0203,
          ls_0391 TYPE zsdt0391.

*------------------------------------------------------------------------------------------------------------*
*   Processar Dados Cabeçalho Carga
*------------------------------------------------------------------------------------------------------------*
    CLEAR: ls_0201, ls_0391.

    IF i_nro_carga IS NOT INITIAL.
      SELECT SINGLE *
        FROM zmmt0201 INTO ls_0201
        WHERE nro_cg = i_nro_carga.

      SELECT SINGLE *
        FROM zsdt0391 INTO ls_0391
        WHERE nro_cg = i_nro_carga.
    ENDIF.

    IF i_header IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = i_header-transportadora
        IMPORTING
          output = ls_0201-cod_transportadora.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = i_header-motorista
        IMPORTING
          output = ls_0201-cod_motorista.

      IF ls_0201-cod_motorista IS NOT INITIAL.
        SELECT SINGLE name1 INTO ls_0201-nome_motorista
          FROM lfa1
          WHERE lifnr EQ ls_0201-cod_motorista.
      ENDIF.

      ls_0201-dt_prevista_embarque  = i_header-data_embarque.
      ls_0201-placa_cav             = i_header-placa_cavalo.
      ls_0201-placa_car1            = i_header-placa_carreta1.
      ls_0201-placa_car2            = i_header-placa_carreta2.
      ls_0201-placa_car3            = i_header-placa_dolly.
      ls_0201-qtd_total_kg          = i_header-qtd_prevista.
      ls_0201-inco1                 = i_header-tipo_frete.


      IF ls_0201 IS NOT INITIAL.
        ls_0201-date_change           = sy-datum.
        ls_0201-time_change           = sy-uzeit.
        ls_0201-user_change           = sy-uname.
      ELSE.
        ls_0201-date_create           = sy-datum.
        ls_0201-time_create           = sy-uzeit.
        ls_0201-user_create           = sy-uname.
      ENDIF.

      IF i_header-frete_por_t IS NOT INITIAL.
        DATA(lv_tp_frete) = '1'.
      ELSE.
        lv_tp_frete = '2'.
      ENDIF.

      "Dados Solicitação Frete
      ls_0391-modalidade_pag_frete  = lv_tp_frete.
      ls_0391-preco_total_frete     = i_header-valor_frete.

      IF ls_0391 IS NOT INITIAL.
        ls_0391-date_change           = sy-datum.
        ls_0391-time_change           = sy-uzeit.
        ls_0391-user_change           = sy-uname.
      ELSE.
        ls_0391-date_create           = sy-datum.
        ls_0391-time_create           = sy-uzeit.
        ls_0391-user_create           = sy-uname.
      ENDIF.

    ENDIF.

*------------------------------------------------------------------------------------------------------------*
*   Processar Itens da Carga
*------------------------------------------------------------------------------------------------------------*
    SELECT *
      FROM zmmt0202
      INTO TABLE @DATA(lt_0202_exists)
      WHERE nro_cg = @i_nro_carga.

    IF sy-subrc IS INITIAL.
      SORT lt_0202_exists BY nro_cg item_carga.
    ENDIF.

    LOOP AT i_itens ASSIGNING FIELD-SYMBOL(<fs_itens>).

      DATA(_item_exists) = abap_false.

      READ TABLE lt_0202_exists ASSIGNING FIELD-SYMBOL(<fs_0202_aux>) WITH KEY nro_cg     = i_nro_carga
                                                                               item_carga = <fs_itens>-item_carga BINARY SEARCH.

      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO lt_0202 ASSIGNING FIELD-SYMBOL(<fs_0202>).
        _item_exists = abap_true.
        MOVE-CORRESPONDING <fs_0202_aux> TO  <fs_0202>.
      ENDIF.

      IF _item_exists EQ abap_true.
        IF <fs_itens>-cancel IS NOT INITIAL.
          <fs_0202>-user_cancel = sy-uname.
          <fs_0202>-date_cancel = sy-datum.
          <fs_0202>-time_cancel = sy-uzeit.
          <fs_0202>-cancel      = abap_true.
        ELSE.
          <fs_0202>-user_change = sy-uname.
          <fs_0202>-date_change = sy-datum.
          <fs_0202>-time_change = sy-uzeit.
        ENDIF.
      ELSEIF <fs_itens>-cancel IS INITIAL.
        APPEND INITIAL LINE TO lt_0202 ASSIGNING <fs_0202>.
        <fs_0202>-user_create       = sy-uname.
        <fs_0202>-date_create       = sy-datum.
        <fs_0202>-time_create       = sy-uzeit.
      ELSE.
        CONTINUE.
      ENDIF.

      <fs_0202>-nro_cg            = i_nro_carga.
      <fs_0202>-item_carga        = <fs_itens>-item_carga.
      <fs_0202>-nro_sol           = <fs_itens>-nro_sol.
      <fs_0202>-seq               = <fs_itens>-item_solic.
      <fs_0202>-ebeln             = <fs_itens>-ebeln.
      <fs_0202>-ebelp             = <fs_itens>-ebelp.
      <fs_0202>-matnr             = <fs_itens>-matnr.
      <fs_0202>-qtd_vinc_carga    = <fs_itens>-qtd_carga.
      <fs_0202>-qtd_vinc_carga_kg = <fs_itens>-qtd_carga_kg.
      <fs_0202>-tp_saldo_vinc     = <fs_itens>-tp_saldo_vinc.

    ENDLOOP.


*------------------------------------------------------------------------------------------------------------*
*   Processar Notas Venda
*------------------------------------------------------------------------------------------------------------*

    IF i_notas_venda IS NOT INITIAL.
      SELECT *
        FROM zmmt0203
        INTO TABLE @DATA(lt_0203_aux)
        FOR ALL ENTRIES IN @i_notas_venda
        WHERE nro_cg = @i_nro_carga
          AND chave_nfe = @i_notas_venda-chave_nota
          AND processo  = '2'
          AND cancel    = @abap_false.
    ENDIF.

    SORT lt_0203_aux BY chave_nfe processo.

    LOOP AT i_notas_venda ASSIGNING FIELD-SYMBOL(<fs_chaves_nfe>).
      READ TABLE lt_0203_aux INTO DATA(lwa_0203_aux)
      WITH KEY chave_nfe = <fs_chaves_nfe>-chave_nota
               processo  = '2' BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_0203 ASSIGNING FIELD-SYMBOL(<fs_0203_save>).
        <fs_0203_save>-nro_cg      = i_nro_carga.
        <fs_0203_save>-chave_nfe   = <fs_chaves_nfe>-chave_nota.
        <fs_0203_save>-processo    = '2'.
        <fs_0203_save>-date_create = sy-datum.
        <fs_0203_save>-time_create = sy-uzeit.
        <fs_0203_save>-user_create = sy-uname.
      ELSE.
        APPEND INITIAL LINE TO lt_0203 ASSIGNING <fs_0203_save>.
        MOVE-CORRESPONDING lwa_0203_aux TO <fs_0203_save>.
      ENDIF.
    ENDLOOP.


*------------------------------------------------------------------------------------------------------------*
*   Processar Notas Transferencia
*------------------------------------------------------------------------------------------------------------*
    IF i_notas_transferencia IS NOT INITIAL.

      SELECT *
        FROM zmmt0203
        APPENDING TABLE lt_0203_aux
        FOR ALL ENTRIES IN i_notas_transferencia
        WHERE nro_cg = i_nro_carga
          AND chave_nfe = i_notas_transferencia-chave_nota
          AND processo  = '1'
          AND cancel    = abap_false.

    ENDIF.

    SORT lt_0203_aux BY chave_nfe processo.

    LOOP AT i_notas_transferencia ASSIGNING FIELD-SYMBOL(<fs_sub_notas>).

      READ TABLE lt_0203_aux ASSIGNING FIELD-SYMBOL(<fs_0203>)
      WITH KEY chave_nfe = <fs_sub_notas>-chave_nota
               processo  = '1' BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        APPEND INITIAL LINE TO lt_0203 ASSIGNING <fs_0203_save>.
        MOVE-CORRESPONDING <fs_0203> TO <fs_0203_save>.

        IF <fs_0203>-data_nfe  <> <fs_sub_notas>-data_nfe OR
           <fs_0203>-chave_cte <> <fs_sub_notas>-chave_cte.

          <fs_0203_save>-data_nfe    = <fs_sub_notas>-data_nfe .
          <fs_0203_save>-chave_cte   = <fs_sub_notas>-chave_cte .
          <fs_0203_save>-date_change = sy-datum.
          <fs_0203_save>-time_change = sy-uzeit.
          <fs_0203_save>-user_change = sy-uname.

        ENDIF.

      ELSE.

        APPEND INITIAL LINE TO lt_0203 ASSIGNING <fs_0203_save>.
        <fs_0203_save>-nro_cg      = i_nro_carga.
        <fs_0203_save>-chave_nfe   = <fs_sub_notas>-chave_nota.
        <fs_0203_save>-data_nfe    = <fs_sub_notas>-data_nfe .
        <fs_0203_save>-chave_cte   = <fs_sub_notas>-chave_cte .
        <fs_0203_save>-processo    = '1'.
        <fs_0203_save>-date_create = sy-datum.
        <fs_0203_save>-time_create = sy-uzeit.
        <fs_0203_save>-user_create = sy-uname.

      ENDIF.

    ENDLOOP.

*------------------------------------------------------------------------------------------------------------*
*   Gravar Dados Carga
*------------------------------------------------------------------------------------------------------------*

    zcl_carga_entrada_insumos=>gravar_carga_core(
      IMPORTING
        e_nro_carga = DATA(lva_nro_carga)
      CHANGING
        i_zmmt0201  = ls_0201
        i_zmmt0202  = lt_0202
        i_zmmt0203  = lt_0203
        i_zsdt0391  = ls_0391
      RECEIVING
        r_msg_error = e_msg_erro
    ).


  ENDMETHOD.


  METHOD busca_dados_carga.

    TYPES:
      BEGIN OF ty_nfe_itm,
        chave_nfe  TYPE zib_nfe_dist_itm-chave_nfe,
        docnum_nfe TYPE zib_nfe_dist_itm-docnum_nfe,
      END OF ty_nfe_itm,

      BEGIN OF ty_doc,
        docnum TYPE j_1bnfdoc-docnum,
        pstdat TYPE j_1bnfdoc-pstdat,
      END OF ty_doc,

      BEGIN OF ty_dist_ter,
        chave_nfe      TYPE zib_nfe_dist_ter-chave_nfe,
        dt_emissao     TYPE zib_nfe_dist_ter-dt_emissao,
        st_fiscal_data TYPE zib_nfe_dist_ter-st_fiscal_data,
      END OF ty_dist_ter.

    DATA: lr_matkl       TYPE RANGE OF matkl,
          t_nfe_itm      TYPE TABLE OF ty_nfe_itm,
          t_doc          TYPE TABLE OF ty_doc,
          t_0196         TYPE TABLE OF zmmt0196,
          t_0202         TYPE TABLE OF zmmt0202,
          t_ekko         TYPE TABLE OF ekko,
          t_ekpo         TYPE TABLE OF ekpo,
          t_0200         TYPE TABLE OF zmmt0200,
          t_lfa1         TYPE TABLE OF lfa1,
          t_0132         TYPE TABLE OF zsdt0132,
          t_0203         TYPE TABLE OF zmmt0203,
          t_lista_cargas TYPE TABLE OF zcds_lista_cargas,
          t_dist_ter     TYPE TABLE OF ty_dist_ter,
          t_status_carga TYPE TABLE OF tvarvc,
          lt_dominio     TYPE TABLE OF dd07v,
          lv_rc          TYPE sy-tabix.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZSDD_STATUS_CARGA'
        text           = abap_true
      IMPORTING
        rc             = lv_rc
      TABLES
        dd07v_tab      = lt_dominio
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    IF sy-subrc IS INITIAL.
      SORT lt_dominio BY domvalue_l.
    ENDIF.

    SELECT *
      FROM zmmt0200
      INTO TABLE t_0200
      WHERE spart = i_segmento.
    IF sy-subrc IS INITIAL.
      SORT t_0200 BY matkl.

      lr_matkl = VALUE #( FOR lw_0200 IN t_0200
                         ( sign = 'I'
                          option = 'EQ'
                          low    = lw_0200-matkl )
                          ).
    ENDIF.

    SELECT *
      FROM zcds_lista_cargas
      INTO TABLE @t_lista_cargas
      WHERE nrocg IN @i_carga
       AND  viagemid IN @i_id_viagem
        AND bukrs IN @i_empresa
        AND datecreate IN @i_dt_carga
        AND matkl IN @lr_matkl
        AND status  IN @i_status
        AND cancel = @space.
    IF sy-subrc IS INITIAL.

      DELETE ADJACENT DUPLICATES FROM t_lista_cargas COMPARING ALL FIELDS.

      e_cds_carga = t_lista_cargas.

      SORT t_lista_cargas BY nrocg.

      DATA(lt_lista_cargas) = t_lista_cargas.
      SORT lt_lista_cargas  BY nrocg.
      DELETE ADJACENT DUPLICATES FROM lt_lista_cargas COMPARING nrocg.

      SELECT *
        FROM zmmt0203
        INTO TABLE t_0203
        FOR ALL ENTRIES IN lt_lista_cargas
        WHERE nro_cg = lt_lista_cargas-nrocg
          AND cancel = space.
      IF sy-subrc IS INITIAL.
        SORT t_0203 BY nro_cg data_nfe ASCENDING.
        e_notas = t_0203.

        DATA(lt_0203) = t_0203.
        SORT lt_0203 BY chave_nfe.
        DELETE ADJACENT DUPLICATES FROM lt_0203 COMPARING chave_nfe.

        SELECT chave_nfe dt_emissao st_fiscal_data
          FROM zib_nfe_dist_ter
          INTO TABLE t_dist_ter
          FOR ALL ENTRIES IN lt_0203
          WHERE chave_nfe = lt_0203-chave_nfe.
        IF sy-subrc IS INITIAL.
          SORT t_dist_ter BY chave_nfe.
        ENDIF.

        SELECT chave_nfe docnum_nfe
          FROM zib_nfe_dist_itm
          INTO TABLE t_nfe_itm
          FOR ALL ENTRIES IN lt_0203
          WHERE chave_nfe = lt_0203-chave_nfe.
        IF sy-subrc IS INITIAL.
          SORT t_nfe_itm BY chave_nfe.

          DATA(lt_nfe_itm) = t_nfe_itm.
          SORT lt_nfe_itm BY docnum_nfe.
          DELETE ADJACENT DUPLICATES FROM lt_nfe_itm COMPARING docnum_nfe.

          SELECT docnum pstdat
            FROM j_1bnfdoc
            INTO TABLE t_doc
            FOR ALL ENTRIES IN lt_nfe_itm
            WHERE docnum = lt_nfe_itm-docnum_nfe.
          IF sy-subrc IS INITIAL.
            SORT t_doc BY docnum.
          ENDIF.

        ENDIF.

      ENDIF.

      lt_lista_cargas = t_lista_cargas.
      SORT lt_lista_cargas  BY nrosol.
      DELETE ADJACENT DUPLICATES FROM lt_lista_cargas COMPARING nrosol.

      SELECT *
        FROM zmmt0196
        INTO TABLE t_0196
        FOR ALL ENTRIES IN lt_lista_cargas
        WHERE nro_sol = lt_lista_cargas-nrosol.
      IF sy-subrc IS INITIAL.
        SORT t_0196 BY nro_sol.
      ENDIF.

    ELSE.

      e_msg_erro = 'Nenhum registro encontrado'.
      RETURN.

    ENDIF.


    DATA(lt_0203_aux) = t_0203.
    DELETE lt_0203_aux WHERE processo <> '1'.
    SORT lt_0203_aux BY nro_cg data_nfe ASCENDING.

    LOOP AT t_lista_cargas ASSIGNING FIELD-SYMBOL(<fs_lista_cargas>).
      APPEND INITIAL LINE TO e_cargas ASSIGNING FIELD-SYMBOL(<fs_alv_lista_cargas>).

      <fs_alv_lista_cargas>-nro_carga       = <fs_lista_cargas>-nrocg.
      <fs_alv_lista_cargas>-bukrs           = <fs_lista_cargas>-bukrs.
      <fs_alv_lista_cargas>-viagem_id       = <fs_lista_cargas>-viagemid.
      <fs_alv_lista_cargas>-cod_transp      = <fs_lista_cargas>-codtransportadora.
      <fs_alv_lista_cargas>-desc_transp     = <fs_lista_cargas>-desc_transp.
      <fs_alv_lista_cargas>-cod_motorista   = <fs_lista_cargas>-codmotorista.
      <fs_alv_lista_cargas>-nome_motorista  = <fs_lista_cargas>-nomemotorista.
      <fs_alv_lista_cargas>-placa_cavalo    = <fs_lista_cargas>-placacav.
      <fs_alv_lista_cargas>-placa_carreta1  = <fs_lista_cargas>-placacar1.
      <fs_alv_lista_cargas>-placa_carreta2  = <fs_lista_cargas>-placacar2.
      <fs_alv_lista_cargas>-placa_dolly     = <fs_lista_cargas>-placacar3.
      <fs_alv_lista_cargas>-incoterms       = <fs_lista_cargas>-inco1.
      <fs_alv_lista_cargas>-peso_carga      = <fs_lista_cargas>-qtdtotalkg.
      <fs_alv_lista_cargas>-ponto_coleta    = <fs_lista_cargas>-pontocoleta.
      <fs_alv_lista_cargas>-local_entrega   = <fs_lista_cargas>-localentrega.
      <fs_alv_lista_cargas>-valor_frete     = <fs_lista_cargas>-preco_frete.

      READ TABLE lt_dominio ASSIGNING FIELD-SYMBOL(<fs_dominio>)
      WITH KEY domvalue_l = <fs_lista_cargas>-status
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_alv_lista_cargas>-id_status_carga = <fs_dominio>-domvalue_l.
        <fs_alv_lista_cargas>-status          = <fs_dominio>-ddtext.
      ENDIF.

      <fs_alv_lista_cargas>-data_carga      = <fs_lista_cargas>-datecreate.
      <fs_alv_lista_cargas>-data_cotacao_frete = <fs_lista_cargas>-dt_cotacao_frete.
      <fs_alv_lista_cargas>-data_contrat_frete     = <fs_lista_cargas>-dt_contrat_frete.
      <fs_alv_lista_cargas>-data_autor_embarque = <fs_lista_cargas>-dtautorizacaoembarque.
      <fs_alv_lista_cargas>-transf_nf_fornec = <fs_lista_cargas>-transfnofornecedor.

      READ TABLE lt_0203_aux ASSIGNING FIELD-SYMBOL(<fs_0203_aux>)
      with key nro_cg = <fs_lista_cargas>-nrocg
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_alv_lista_cargas>-data_1_transf = <fs_0203_aux>-data_nfe.
      ENDIF.

      READ TABLE t_0203 ASSIGNING FIELD-SYMBOL(<fs_0203>)
      WITH KEY nro_cg = <fs_lista_cargas>-nrocg
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        READ TABLE t_dist_ter ASSIGNING FIELD-SYMBOL(<fs_dist_ter>)
        WITH KEY chave_nfe = <fs_0203>-chave_nfe
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <fs_alv_lista_cargas>-data_troca_nota = <fs_dist_ter>-dt_emissao.
          <fs_alv_lista_cargas>-data_conf_nota = <fs_dist_ter>-st_fiscal_data.
        ENDIF.

        READ TABLE t_nfe_itm ASSIGNING FIELD-SYMBOL(<fs_nfe_itm>)
        WITH KEY chave_nfe = <fs_0203>
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE t_doc ASSIGNING FIELD-SYMBOL(<fs_doc>)
          WITH KEY docnum = <fs_nfe_itm>-docnum_nfe
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_alv_lista_cargas>-data_entrada_nota = <fs_doc>-pstdat.
          ENDIF.

        ENDIF.

      ENDIF.

      <fs_alv_lista_cargas>-usuario_carga = <fs_lista_cargas>-usercreate.
      <fs_alv_lista_cargas>-hora_carga = <fs_lista_cargas>-timecreate.

    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM e_cargas COMPARING ALL FIELDS.

  ENDMETHOD.


  METHOD busca_dados_conferencia.

    TYPES:
      BEGIN OF ty_de_para.
        INCLUDE TYPE zmme_alv_dados_conf_de_para.
  TYPES END OF ty_de_para.


    DATA: lr_carga       TYPE RANGE OF zmmt0201-nro_cg,
          lt_dados_carga TYPE TABLE OF zcds_lista_cargas,
          lt_de_para_aux TYPE TABLE OF ty_de_para,
          ls_celltab     TYPE lvc_s_styl,
          lt_celltab     TYPE TABLE OF lvc_s_styl,
          lv_qtd_conf    TYPE menge_d.

    APPEND INITIAL LINE TO lr_carga ASSIGNING FIELD-SYMBOL(<fs_range_carga>).
    <fs_range_carga>-sign = 'I'.
    <fs_range_carga>-option = 'EQ'.
    <fs_range_carga>-low = i_nro_carga.

*** Busca dados da carga e de notas da carga
    zcl_carga_entrada_insumos=>busca_dados_carga(
    EXPORTING
      i_carga = lr_carga
    IMPORTING
      e_cds_carga = lt_dados_carga
      e_notas     = DATA(t_dados_notas)
      e_msg_erro = DATA(lv_msg_erro) ).

    DELETE lt_dados_carga WHERE itemcarga IS INITIAL.
    IF lt_dados_carga IS INITIAL.
      e_msg_erro = 'Não há dados para a carga'.
      RETURN.

    ELSE.

      SORT lt_dados_carga BY nrocg itemcarga.

      DELETE t_dados_notas WHERE processo <> '2'.

      IF t_dados_notas IS NOT INITIAL.

        DATA(lt_dados_notas) = t_dados_notas.
        SORT lt_dados_notas BY chave_nfe.
        DELETE ADJACENT DUPLICATES FROM lt_dados_notas COMPARING chave_nfe.

*** Busca dados complementares da nota
        SELECT *
          FROM zi_mm_dados_nfe_carga
          INTO TABLE @DATA(lt_nfe)
          FOR ALL ENTRIES IN @lt_dados_notas
          WHERE chavenfe = @lt_dados_notas-chave_nfe
            AND nrocg    = @i_nro_carga.
        IF sy-subrc IS INITIAL.
          SORT lt_nfe BY chavenfe proditem.
        ENDIF.

*** Busca dados borderô
        DATA(lt_nfe_aux) = lt_nfe.
        SORT lt_nfe_aux BY chavenfe.
        DELETE ADJACENT DUPLICATES FROM lt_nfe_aux COMPARING chavenfe.

        SELECT *
          FROM zmmt0205
          INTO TABLE @DATA(lt_0205)
          FOR ALL ENTRIES IN @lt_nfe_aux
          WHERE chave_nfe =  @lt_nfe_aux-chavenfe
            AND cancel    = @space.
        IF sy-subrc IS INITIAL.
          SELECT id_chegada_cd_luft,
                 id_item_chegada,
                 codigo_produto_amaggi,
                 produto,
                 lote,
                 embalagem,
                 quantidade
            FROM zmmt0206
            INTO TABLE @DATA(lt_bordero)
            FOR ALL ENTRIES IN @lt_0205
            WHERE id_chegada_cd_luft = @lt_0205-id_chegada_cd_luft.
          IF sy-subrc IS INITIAL.
            SORT lt_bordero BY id_chegada_cd_luft id_item_chegada.
          ENDIF.
        ENDIF.


      ENDIF.

*** Busca dados de conferência
      SELECT *
        FROM zmmt0218
        INTO TABLE @DATA(lt_0218)
        WHERE nro_cg = @i_nro_carga
          AND cancel = ''.
      IF sy-subrc IS INITIAL.
        SORT lt_0218 BY nro_cg.
      ENDIF.

    ENDIF.

    ls_celltab-fieldname = 'QUANTIDADE'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO lt_celltab INDEX 1.

*** Montagem alv de-para
    LOOP AT lt_0218 ASSIGNING FIELD-SYMBOL(<fs_0218>).

      APPEND INITIAL LINE TO e_de_para_conferencia ASSIGNING FIELD-SYMBOL(<fs_de_para>).

      READ TABLE lt_dados_carga ASSIGNING FIELD-SYMBOL(<fs_dados_carga>)
      WITH KEY nrocg     = <fs_0218>-nro_cg
               itemcarga = <fs_0218>-item_carga
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_de_para>-nro_cg           = <fs_0218>-nro_cg.
        <fs_de_para>-item_carga       = <fs_0218>-item_carga.
        <fs_de_para>-cod_prod_pedido  = <fs_dados_carga>-matnr.
        <fs_de_para>-desc_prod_pedido = <fs_dados_carga>-desc_material.
        <fs_de_para>-unidade_pedido   = <fs_dados_carga>-unidade.

*        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*          EXPORTING
*            input          = <fs_dados_carga>-unidade
*            language       = sy-langu
*          IMPORTING
*            output         = <fs_de_para>-unidade_pedido
*          EXCEPTIONS
*            unit_not_found = 1
*            OTHERS         = 2.

      ENDIF.

      READ TABLE lt_nfe ASSIGNING FIELD-SYMBOL(<fs_nfe>)
      WITH KEY chavenfe = <fs_0218>-chave_nfe
               proditem = <fs_0218>-prod_item
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_de_para>-chave_nfe     = <fs_nfe>-chavenfe.
        <fs_de_para>-item_nfe      = <fs_nfe>-proditem.
        <fs_de_para>-cod_prod_nfe  = <fs_nfe>-prodcodigo.
        <fs_de_para>-desc_prod_nfe = <fs_nfe>-proddescricao.
        <fs_de_para>-unidade_nfe   = <fs_nfe>-produndcomerci.
        <fs_de_para>-numero_nfe    = <fs_nfe>-numero.

        IF <fs_nfe>-ckfiscal IS NOT INITIAL.
          <fs_de_para>-celltab = lt_celltab.
        ENDIF.
      ENDIF.

      READ TABLE lt_bordero ASSIGNING FIELD-SYMBOL(<fs_bordero>)
      WITH KEY id_chegada_cd_luft = <fs_0218>-id_chegada_cd_luft
               id_item_chegada    = <fs_0218>-id_item_chegada
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_de_para>-id_chegada_cd_luft = <fs_0218>-id_chegada_cd_luft.
        <fs_de_para>-id_item_chegada    = <fs_0218>-id_item_chegada.
        <fs_de_para>-cod_prod_bordero   = <fs_bordero>-codigo_produto_amaggi.
        <fs_de_para>-desc_prod_bordero  = <fs_bordero>-produto.
        <fs_de_para>-lote_bordero       = <fs_bordero>-lote.
        <fs_de_para>-embalagem          = <fs_bordero>-embalagem.
      ENDIF.

      <fs_de_para>-quantidade             = <fs_0218>-quantidade.
      <fs_de_para>-cd_notificado_correcao = <fs_0218>-cd_notificado_correcao.

      APPEND INITIAL LINE TO lt_de_para_aux ASSIGNING FIELD-SYMBOL(<fs_de_para_aux>).
      MOVE-CORRESPONDING <fs_de_para> TO <fs_de_para_aux>.
      MOVE-CORRESPONDING <fs_0218> TO <fs_de_para_aux>.

    ENDLOOP.

*** Montagem alv borderô
    DATA(lt_de_para) = lt_de_para_aux.
    SORT lt_de_para BY id_chegada_cd_luft id_item_chegada.

    DATA(lt_nfe_aux2) = lt_nfe.
    SORT lt_nfe_aux2 BY chavenfe.

    SORT lt_0205 BY id_chegada_cd_luft.

    LOOP AT lt_bordero ASSIGNING FIELD-SYMBOL(<fs_dados_bordero>).
      APPEND INITIAL LINE TO e_bordero ASSIGNING FIELD-SYMBOL(<fs_alv_bordero>).

      <fs_alv_bordero>-id                    = <fs_dados_bordero>-id_chegada_cd_luft.
      <fs_alv_bordero>-id_item_chegada       = <fs_dados_bordero>-id_item_chegada.
      <fs_alv_bordero>-codigo_produto        = <fs_dados_bordero>-codigo_produto_amaggi.
      <fs_alv_bordero>-lote                  = <fs_dados_bordero>-lote.
      <fs_alv_bordero>-produto               = <fs_dados_bordero>-produto.
      <fs_alv_bordero>-embalagem             = <fs_dados_bordero>-embalagem.
      <fs_alv_bordero>-saldo                 = <fs_dados_bordero>-quantidade.
      <fs_alv_bordero>-quantidade            = <fs_dados_bordero>-quantidade.

      READ TABLE lt_0205 ASSIGNING FIELD-SYMBOL(<fs_0205>)
      WITH KEY id_chegada_cd_luft = <fs_dados_bordero>-id_chegada_cd_luft
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE lt_nfe_aux2 ASSIGNING FIELD-SYMBOL(<fs_nfe_aux2>)
        WITH KEY chavenfe = <fs_0205>-chave_nfe
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <fs_alv_bordero>-numero_nfe = <fs_nfe_aux2>-numero.
        ENDIF.
      ENDIF.

      READ TABLE lt_de_para TRANSPORTING NO FIELDS
      WITH KEY id_chegada_cd_luft  = <fs_dados_bordero>-id_chegada_cd_luft
               id_item_chegada     = <fs_dados_bordero>-id_item_chegada
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_de_para ASSIGNING <fs_de_para_aux> FROM sy-tabix.

          IF <fs_de_para_aux>-id_chegada_cd_luft <> <fs_dados_bordero>-id_chegada_cd_luft OR
             <fs_de_para_aux>-id_item_chegada    <> <fs_dados_bordero>-id_item_chegada.
            EXIT.
          ENDIF.

          <fs_alv_bordero>-saldo = <fs_alv_bordero>-saldo - <fs_de_para_aux>-quantidade.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

*** Montagem alv de cargas
    lt_de_para = lt_de_para_aux.
    SORT lt_de_para BY nro_cg item_carga.

    LOOP AT lt_dados_carga ASSIGNING <fs_dados_carga>.
      APPEND INITIAL LINE TO e_carga ASSIGNING FIELD-SYMBOL(<fs_carga>).

      <fs_carga>-item_carga     = <fs_dados_carga>-itemcarga.
      <fs_carga>-codigo_produto = <fs_dados_carga>-matnr.
      <fs_carga>-desc_produto   = <fs_dados_carga>-desc_material.
      <fs_carga>-pedido         = <fs_dados_carga>-ebeln.
      <fs_carga>-item_pedido    = <fs_dados_carga>-ebelp.
      <fs_carga>-qtd_vinc_carga = <fs_dados_carga>-qtdvinccarga.
      <fs_carga>-unidade        = <fs_dados_carga>-unidade.

*      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*        EXPORTING
*          input          = <fs_dados_carga>-unidade
*          language       = sy-langu
*        IMPORTING
*          output         = <fs_carga>-unidade
*        EXCEPTIONS
*          unit_not_found = 1
*          OTHERS         = 2.

      <fs_carga>-saldo          = <fs_carga>-qtd_vinc_carga.

      READ TABLE lt_de_para TRANSPORTING NO FIELDS
      WITH KEY nro_cg     = <fs_dados_carga>-nrocg
               item_carga = <fs_dados_carga>-itemcarga
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        LOOP AT lt_de_para ASSIGNING <fs_de_para_aux> FROM sy-tabix.
          IF <fs_de_para_aux>-nro_cg <> <fs_dados_carga>-nrocg OR
             <fs_de_para_aux>-item_carga <> <fs_dados_carga>-itemcarga.
            EXIT.
          ENDIF.

          <fs_carga>-saldo = <fs_carga>-saldo - <fs_de_para_aux>-quantidade.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

*** Montagem alv de notas
    DATA(lt_0218_aux) = lt_0218.
    SORT lt_0218_aux BY chave_nfe prod_item.

    LOOP AT lt_nfe ASSIGNING <fs_nfe>.
      CLEAR lv_qtd_conf.

      APPEND INITIAL LINE TO e_notas ASSIGNING FIELD-SYMBOL(<fs_notas>).

      <fs_notas>-chave_nfe      = <fs_nfe>-chavenfe.
      <fs_notas>-cod_controle   = <fs_nfe>-prodncm.
      <fs_notas>-codigo_produto = <fs_nfe>-prodcodigo.
      <fs_notas>-desc_produto   = <fs_nfe>-proddescricao.
      <fs_notas>-icms           = <fs_nfe>-icmsbase.
      <fs_notas>-item_nfe       = <fs_nfe>-proditem.
      <fs_notas>-montante       = <fs_nfe>-prodvlrtotalb.
      <fs_notas>-numero_nfe     = <fs_nfe>-numero.
      <fs_notas>-preco_liquido  = <fs_nfe>-prodvlrundcom.
      <fs_notas>-quantidade     = <fs_nfe>-prodqtdcomerci.

      READ TABLE lt_0218_aux TRANSPORTING NO FIELDS
      WITH KEY chave_nfe = <fs_nfe>-chavenfe
               prod_item = <fs_nfe>-proditem
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        LOOP AT lt_0218_aux ASSIGNING FIELD-SYMBOL(<fs_0218_aux>) FROM sy-tabix.
          IF <fs_0218_aux>-chave_nfe <> <fs_nfe>-chavenfe OR
             <fs_0218_aux>-prod_item  <> <fs_nfe>-proditem.
            EXIT.
          ENDIF.

          <fs_notas>-peso_conv = <fs_0218_aux>-peso_conv_nfe.
          lv_qtd_conf          = lv_qtd_conf + <fs_0218_aux>-quantidade.

        ENDLOOP.

        IF <fs_notas>-peso_conv > 0.
          <fs_notas>-qtd_conv = <fs_notas>-quantidade / <fs_notas>-peso_conv.
          <fs_notas>-saldo    = <fs_notas>-qtd_conv.
        ENDIF.

        IF <fs_notas>-saldo > 0.
          <fs_notas>-saldo = <fs_notas>-saldo - lv_qtd_conf.
        ELSE.
          <fs_notas>-saldo = <fs_notas>-qtd_conv.
        ENDIF.

      ENDIF.

      <fs_notas>-unidade        = <fs_nfe>-produndcomerci.

    ENDLOOP.

  ENDMETHOD.


  METHOD busca_dados_montar_carga.

    TYPES:
      BEGIN OF ty_sum_sol,
        nro_sol TYPE zmmt0196-nro_sol,
        qtd_sol TYPE zmmt0196-solicitacao_qte,
      END OF ty_sum_sol.

    DATA: lv_total   TYPE zmmt0202-qtd_vinc_carga,
          lt_sum_sol TYPE TABLE OF ty_sum_sol,
          lw_sum_sol TYPE ty_sum_sol.

    DATA: lr_matkl TYPE RANGE OF matkl,
          t_0196   TYPE TABLE OF zmmt0196,
          t_0202   TYPE TABLE OF zmmt0202,
          t_ekko   TYPE TABLE OF ekko,
          t_ekpo   TYPE TABLE OF ekpo,
          t_0200   TYPE TABLE OF zmmt0200,
          t_lfa1   TYPE TABLE OF lfa1,
          t_0132   TYPE TABLE OF zsdt0132.

    SELECT *
      FROM zmmt0200
      INTO TABLE t_0200
      WHERE spart = i_segmento.
    IF sy-subrc IS INITIAL.
      SORT t_0200 BY matkl.

      lr_matkl = VALUE #( FOR lw_0200 IN t_0200
                         ( sign = 'I'
                          option = 'EQ'
                          low    = lw_0200-matkl )
                          ).
    ENDIF.

    SELECT *
      FROM zmmt0196
      INTO TABLE t_0196
      WHERE nro_sol IN i_nro_sol
        AND date_create IN i_dt_sol
        AND ebeln IN i_pedido
        AND cancel = space.
    IF sy-subrc IS INITIAL.
      SORT t_0196 BY nro_sol.

      DATA(lt_0196) = t_0196.
      SORT lt_0196 BY ebeln.
      DELETE ADJACENT DUPLICATES FROM lt_0196 COMPARING ebeln.

      SELECT *
        FROM ekko
        INTO TABLE t_ekko
        FOR ALL ENTRIES IN lt_0196
        WHERE ebeln = lt_0196-ebeln
          AND bukrs IN i_empresa.
      IF sy-subrc IS INITIAL.
        SORT t_ekko BY ebeln.
      ENDIF.

      lt_0196 = t_0196.
      SORT lt_0196 BY ebeln ebelp.
      DELETE ADJACENT DUPLICATES FROM lt_0196 COMPARING ebeln ebelp.

      SELECT *
        FROM ekpo
        INTO TABLE t_ekpo
        FOR ALL ENTRIES IN lt_0196
        WHERE ebeln = lt_0196-ebeln
          AND ebelp = lt_0196-ebelp
          AND matkl IN lr_matkl.
      IF sy-subrc IS INITIAL.
        SORT t_ekpo BY ebeln ebelp.

        LOOP AT t_0196 ASSIGNING FIELD-SYMBOL(<fs_0196>).

          READ TABLE t_ekko ASSIGNING FIELD-SYMBOL(<fs_ekko>)
          WITH KEY ebeln = <fs_0196>-ebeln
          BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            DELETE t_0196 WHERE ebeln = <fs_0196>-ebeln.
            CONTINUE.
          ENDIF.

          READ TABLE t_ekpo TRANSPORTING NO FIELDS
          WITH KEY ebeln = <fs_0196>-ebeln
                   ebelp = <fs_0196>-ebelp
          BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            DELETE t_0196 WHERE ebeln = <fs_0196>-ebeln AND ebelp = <fs_0196>-ebelp.
          ENDIF.
        ENDLOOP.
      ELSE.

        FREE: t_0196.

      ENDIF.

      IF t_0196 IS INITIAL.
        e_msg_erro = | 'Nenhum registro encontrado' |.
        RETURN.
      ENDIF.

      lt_0196 = t_0196.
      SORT lt_0196 BY nro_sol seq.
      DELETE ADJACENT DUPLICATES FROM lt_0196 COMPARING nro_sol seq.

      SELECT *
        FROM zmmt0202 AS i
        INTO TABLE t_0202
        FOR ALL ENTRIES IN lt_0196
        WHERE nro_sol = lt_0196-nro_sol
          AND seq     = lt_0196-seq
          AND cancel  = space
         AND NOT EXISTS (

       SELECT nro_cg
        FROM zmmt0201  AS c
       WHERE c~nro_cg = i~nro_cg
         AND c~cancel = abap_true ).
      IF sy-subrc IS INITIAL.

        DELETE t_0202 WHERE tp_saldo_vinc NE 'M'. "Manual.

        SORT t_0202 BY nro_sol.
      ENDIF.

      lt_0196 = t_0196.
      SORT lt_0196 BY parceiro_pc.
      DELETE ADJACENT DUPLICATES FROM lt_0196 COMPARING parceiro_pc.

      SELECT *
        FROM lfa1
        INTO TABLE t_lfa1
        FOR ALL ENTRIES IN lt_0196
        WHERE lifnr = lt_0196-parceiro_pc.
      IF sy-subrc IS INITIAL.
        SORT t_lfa1 BY lifnr.
      ENDIF.

      lt_0196 = t_0196.
      SORT lt_0196 BY parceiro_le.
      DELETE ADJACENT DUPLICATES FROM lt_0196 COMPARING parceiro_le.

      SELECT *
        FROM lfa1
        APPENDING TABLE t_lfa1
        FOR ALL ENTRIES IN lt_0196
        WHERE lifnr = lt_0196-parceiro_le.
      IF sy-subrc IS INITIAL.
        SORT t_lfa1 BY lifnr.
      ENDIF.

      lt_0196 = t_0196.
      SORT lt_0196 BY rota_pc.
      DELETE ADJACENT DUPLICATES FROM lt_0196 COMPARING rota_pc.

      SELECT *
        FROM zsdt0132
        INTO TABLE t_0132
        FOR ALL ENTRIES IN lt_0196
        WHERE nr_rot = lt_0196-rota_pc.
      IF sy-subrc IS INITIAL.
        SORT t_0132 BY nr_rot.
      ENDIF.

      lt_0196 = t_0196.
      SORT lt_0196 BY rota_le.
      DELETE ADJACENT DUPLICATES FROM lt_0196 COMPARING rota_le.

      SELECT *
        FROM zsdt0132
        APPENDING TABLE t_0132
        FOR ALL ENTRIES IN lt_0196
        WHERE nr_rot = lt_0196-rota_le.
      IF sy-subrc IS INITIAL.
        SORT t_0132 BY nr_rot.
      ENDIF.

    ENDIF.

*** Monta tabela de saída
    LOOP AT t_0196 ASSIGNING <fs_0196>.
      lw_sum_sol-nro_sol = <fs_0196>-nro_sol.
      lw_sum_sol-qtd_sol = <fs_0196>-solicitacao_qte_manual.
      COLLECT lw_sum_sol INTO lt_sum_sol.
    ENDLOOP.

    lt_0196 = t_0196.
    SORT lt_0196 BY nro_sol.
    DELETE ADJACENT DUPLICATES FROM lt_0196 COMPARING nro_sol.

    LOOP AT lt_0196 ASSIGNING <fs_0196>.

      CLEAR: lv_total.

      READ TABLE t_0202 TRANSPORTING NO FIELDS
      WITH KEY nro_sol = <fs_0196>-nro_sol
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        LOOP AT t_0202 ASSIGNING FIELD-SYMBOL(<fs_0202>) FROM sy-tabix.
          IF <fs_0196>-nro_sol <> <fs_0202>-nro_sol.
            EXIT.
          ENDIF.

          lv_total = lv_total + <fs_0202>-qtd_vinc_carga.

        ENDLOOP.

      ENDIF.

      APPEND INITIAL LINE TO e_tabela_monta_carga ASSIGNING FIELD-SYMBOL(<fs_monta_carga>).
      <fs_monta_carga>-nro_solic  = <fs_0196>-nro_sol.

      READ TABLE t_ekko ASSIGNING <fs_ekko>
      WITH KEY ebeln = <fs_0196>-ebeln
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_monta_carga>-bukrs      = <fs_ekko>-bukrs.
      ENDIF.

      <fs_monta_carga>-data_solic = <fs_0196>-date_create.
      <fs_monta_carga>-data_receb = <fs_0196>-entrega_dt.
      <fs_monta_carga>-nivel_prior = <fs_0196>-prioridade_sol.
      <fs_monta_carga>-num_rota_pc = <fs_0196>-rota_pc.
      <fs_monta_carga>-num_rota_le = <fs_0196>-rota_le.

      READ TABLE lt_sum_sol ASSIGNING FIELD-SYMBOL(<fs_sum_sol>)
      WITH KEY nro_sol = <fs_0196>-nro_sol
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_monta_carga>-qtd_solic = <fs_sum_sol>-qtd_sol.
        <fs_monta_carga>-saldo_a_solic = <fs_monta_carga>-qtd_solic - lv_total.
      ENDIF.

      READ TABLE t_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
      WITH KEY lifnr = <fs_0196>-parceiro_pc
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_monta_carga>-ponto_coleta = <fs_lfa1>-name1.
        CONCATENATE <fs_lfa1>-stras ',' <fs_lfa1>-ort01 '-' INTO <fs_monta_carga>-endereco_pc SEPARATED BY space.
      ENDIF.

      READ TABLE t_0132 ASSIGNING FIELD-SYMBOL(<fs_0132>)
      WITH KEY nr_rot = <fs_0196>-rota_pc
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_monta_carga>-descricao_rota_pc = <fs_0132>-rot_desc.
        <fs_monta_carga>-obs_roteiro_pc    = <fs_0132>-rot_obs.
      ENDIF.

      READ TABLE t_lfa1 ASSIGNING <fs_lfa1>
      WITH KEY lifnr = <fs_0196>-parceiro_le
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_monta_carga>-local_entrega = <fs_lfa1>-name1.
        CONCATENATE <fs_lfa1>-stras ',' <fs_lfa1>-ort01 '-' INTO <fs_monta_carga>-endereco_le SEPARATED BY space.
      ENDIF.

      READ TABLE t_0132 ASSIGNING <fs_0132>
      WITH KEY nr_rot = <fs_0196>-rota_le
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_monta_carga>-descricao_rota_le = <fs_0132>-rot_desc.
        <fs_monta_carga>-obs_roteiro_le    = <fs_0132>-rot_obs.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD cancelar_aceite.
    DATA lo_aceite TYPE REF TO zcl_nfe_inbound.

    TRY .
        CREATE OBJECT lo_aceite.

        CALL METHOD lo_aceite->zif_cadastro~set_registro
          EXPORTING
            i_id_registro = i_chave_nfe.

        lo_aceite->nfe_inbound_cancela_aceite( ).

      CATCH zcx_nfe_inbound_exception INTO DATA(lo_nfe_inbound).
        e_msg_erro = lo_nfe_inbound->get_text( ).
      CATCH zcx_cadastro INTO DATA(lo_cadastro).
        e_msg_erro = lo_cadastro->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD cancela_carga.

    DATA: tl_texto TYPE catsxt_longtext_itab.

    DATA: lva_motivo TYPE string.

    DATA: lv_erro TYPE c.

    CLEAR: e_msg_erro.

    SELECT SINGLE *
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
      WHERE nro_cg = @i_nro_carga.

    IF sy-subrc NE 0 OR i_nro_carga IS INITIAL.
      e_msg_erro = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF sy-batch EQ abap_false.

      IF lwa_zmmt0201-viagem_id IS NOT INITIAL.
        e_msg_erro = zcl_carga_entrada_insumos=>check_permissao_carga(
            EXPORTING
               i_nro_carga = lwa_zmmt0201-nro_cg
               i_atividade = '10' ).
      ELSE.
        e_msg_erro = zcl_carga_entrada_insumos=>check_permissao_carga(
            EXPORTING
               i_nro_carga = lwa_zmmt0201-nro_cg
               i_atividade = '05' ).
      ENDIF.

      CHECK e_msg_erro IS INITIAL.

    ENDIF.

    SELECT SINGLE *
      FROM zmmt0203 INTO @DATA(lwa_zmt0203)
     WHERE nro_cg EQ @i_nro_carga
       AND cancel EQ @abap_false.

    IF sy-subrc EQ 0. "possui NF-e
      e_msg_erro = |Carga já possui NF-e informada! Operação não permitida!|.
      RETURN.
    ENDIF.

    IF sy-batch EQ abap_false.
      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title = 'Motivo do cancelamento'
        CHANGING
          ch_text  = tl_texto.

      LOOP AT tl_texto ASSIGNING FIELD-SYMBOL(<fs_texto>).
        lva_motivo = lva_motivo && <fs_texto>.
      ENDLOOP.
    ELSE.
      lva_motivo = 'Origem Cancelamento Sistema Carguero!'.
    ENDIF.

    IF lva_motivo IS INITIAL.
      e_msg_erro = 'Motivo cancelamento não informado!'.
      RETURN.
    ENDIF.

    zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga(
     EXPORTING
       i_nro_cg = i_nro_carga
       bloqueio = abap_true
       IMPORTING
         msg = DATA(lv_msg) ).
    IF lv_msg IS NOT INITIAL.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT *
      FROM zmmt0202
      INTO TABLE @DATA(lt_0202)
      WHERE nro_cg = @i_nro_carga.
    IF sy-subrc IS INITIAL.

      LOOP AT lt_0202 ASSIGNING FIELD-SYMBOL(<fs_0202>).

        <fs_0202>-cancel      = abap_true.
        <fs_0202>-date_cancel = sy-datum.
        <fs_0202>-user_cancel = sy-uname.
        <fs_0202>-time_cancel = sy-uzeit.

      ENDLOOP.

    ENDIF.

    IF lv_erro IS INITIAL.

      UPDATE zmmt0201 SET cancel      = abap_true
                          date_cancel = sy-datum
                          user_cancel = sy-uname
                          time_cancel = sy-uzeit
                          motivo_cancelamento = lva_motivo
       WHERE nro_cg = i_nro_carga.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ENDIF.

      IF lt_0202 IS NOT INITIAL.
        MODIFY zmmt0202 FROM TABLE lt_0202.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
        ENDIF.
      ENDIF.

    ENDIF.

    zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga(
    EXPORTING
      i_nro_cg = i_nro_carga
      bloqueio = abap_false
      IMPORTING
        msg = lv_msg ).


  ENDMETHOD.


  METHOD check_permissao_carga.

    DATA: lv_answer.

    DATA: lwa_zmmt0201 TYPE zmmt0201.

    CLEAR: r_msg_error, lwa_zmmt0201.

    SELECT SINGLE *
      FROM zmmt0201 INTO lwa_zmmt0201
     WHERE nro_cg EQ i_nro_carga.

    IF sy-subrc NE 0 OR i_nro_carga IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    DATA(_spart_carga) = zcl_carga_entrada_insumos=>get_spart_carga( i_nro_carga = i_nro_carga ).

    r_msg_error = zcl_carga_entrada_insumos=>check_permissao_carga_core(
      EXPORTING
        i_bukrs     = lwa_zmmt0201-bukrs
        i_atividade = i_atividade
        i_spart     = _spart_carga ).

  ENDMETHOD.


  METHOD CHECK_PERMISSAO_CARGA_CORE.

    DATA: lv_answer.

    CLEAR: r_msg_error.

    AUTHORITY-CHECK OBJECT 'ZSDT0112V3'
    ID 'ZSDATIVID'   FIELD i_atividade
    ID 'BUKRS'       FIELD i_bukrs
    ID 'SPART'       FIELD i_spart
    ID 'DIRECAO'     FIELD 'E'.

    IF sy-subrc IS NOT INITIAL.
      r_msg_error = |Sem autorização para essa ação!|.
      r_msg_error = |{ r_msg_error } Procure o departamento de Insumos Corporativo para solicitar o acesso|.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD check_reinicializacao_aut_emb.

    DATA: lv_answer.

    CLEAR: r_msg_error.

    SELECT SINGLE *
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
     WHERE nro_cg EQ @i_nro_carga.

    IF sy-subrc NE 0 OR i_nro_carga IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    CHECK lwa_zmmt0201-dt_autorizacao_embarque IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question = 'Autorização de embarque já existe, será necessário gerar nova autorização de embarque. Deseja continuar?'
        text_button_1 = 'Sim'
        icon_button_1 = 'ICON_CHECKED'
        text_button_2 = 'Não'
        icon_button_2 = 'ICON_INCOMPLETE'
      IMPORTING
        answer        = lv_answer.

    IF lv_answer NE '1'.
      r_msg_error = 'Operação abortada!'.
      RETURN.
    ENDIF.

    CLEAR lwa_zmmt0201-dt_autorizacao_embarque.

    zcl_carga_entrada_insumos=>gravar_carga_core(
      IMPORTING
        e_nro_carga = DATA(lva_nro_carga)
      CHANGING
        i_zmmt0201  = lwa_zmmt0201
      RECEIVING
        r_msg_error = DATA(e_msg_erro)
    ).

  ENDMETHOD.


  METHOD conferir_carga.

    DATA: lit_zmmt0202         TYPE TABLE OF zmmt0202.
    DATA: lva_peso_descarga_kg TYPE zmmt0202-qtd_vinc_carga_kg.

    DATA: lv_answer.

    CLEAR: r_msg_error.

    CHECK i_nro_carga IS NOT INITIAL.

    SELECT SINGLE *
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
     WHERE nro_cg EQ @i_nro_carga.

    CHECK sy-subrc EQ 0.

    IF lwa_zmmt0201-carga_conferida IS NOT INITIAL.
      r_msg_error = |Carga já foi conferida pelo usuário { lwa_zmmt0201-us_conferencia }!|.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zmmt0203 INTO @DATA(lwa_zmmt0203)
     WHERE nro_cg EQ @i_nro_carga
       AND cancel EQ @abap_false.

    IF sy-subrc NE 0.
      r_msg_error = |Carga não possui NF-e informada!|.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zmmt0202 INTO @DATA(lwa_zmmt0202)
     WHERE nro_cg EQ @i_nro_carga
       AND cancel EQ @abap_false.

    IF sy-subrc NE 0.
      r_msg_error = |Carga não possui itens informado!|.
      RETURN.
    ENDIF.

    r_msg_error = zcl_carga_entrada_insumos=>check_permissao_carga(
        EXPORTING
           i_nro_carga = lwa_zmmt0201-nro_cg
           i_atividade = '09' ).

    CHECK r_msg_error IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar      = 'ATENÇÃO'
        text_question = 'Essa operação é irreversivel! Confirma operação?'
        text_button_1 = 'Sim'
        icon_button_1 = 'ICON_CHECKED'
        text_button_2 = 'Não'
        icon_button_2 = 'ICON_INCOMPLETE'
      IMPORTING
        answer        = lv_answer.

    IF lv_answer <> '1'.
      RETURN.
    ENDIF.

    zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga( EXPORTING
                                                              i_nro_cg = i_nro_carga
                                                              bloqueio = abap_true
                                                           IMPORTING
                                                              msg = r_msg_error ).

    IF r_msg_error IS NOT INITIAL.
      zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga bloqueio = abap_false ).
      RETURN.
    ENDIF.


    IF lwa_zmmt0201-viagem_id IS NOT INITIAL.

      CLEAR: lit_zmmt0202[], lva_peso_descarga_kg.

      SELECT *
        FROM zmmt0202 INTO TABLE lit_zmmt0202
       WHERE nro_cg EQ i_nro_carga.


      DELETE lit_zmmt0202 WHERE tp_saldo_vinc NE 'C' OR "Carguero
                                cancel EQ abap_true.

      LOOP AT lit_zmmt0202 ASSIGNING FIELD-SYMBOL(<fs_zmt0202>).
        ADD <fs_zmt0202>-qtd_vinc_carga_kg TO lva_peso_descarga_kg.
      ENDLOOP.

      IF lva_peso_descarga_kg IS INITIAL.
        r_msg_error = 'Carga sem saldos definidos para o Carguero!'.
        RETURN.
      ENDIF.


      SELECT SINGLE *
        FROM tvarvc INTO @DATA(lwa_carga_insumos_descarga)
       WHERE name EQ 'CARGA_INSUMOS_NOT_DESCARGA'.

      IF sy-subrc NE 0.
        TRY .
            zcl_integracao_viagem_descarg=>zif_integracao_viagem_descarg~get_instance(
              )->set_viagem_descarregar(
              EXPORTING
                i_viagem_id             = lwa_zmmt0201-viagem_id
                i_dt_descarga           = sy-datum
                i_peso_liquido          = CONV #( lva_peso_descarga_kg )
            ).

          CATCH zcx_integracao INTO DATA(ex_integracao).
            zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga bloqueio = abap_false ).

            DATA(_msg_retorno) = ex_integracao->get_text( ).
            r_msg_error = |Não foi possivel confirmar a descarga no Carguero! Msg: { _msg_retorno } |.
            RETURN.

          CATCH zcx_error INTO DATA(ex_error).
            zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga bloqueio = abap_false ).

            _msg_retorno = ex_error->get_text( ).
            r_msg_error = |Não foi possivel confirmar a descarga no Carguero! Msg: { _msg_retorno } |.
            RETURN.
        ENDTRY.
      ENDIF.

    ENDIF.

    zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga bloqueio = abap_false ).

    UPDATE zmmt0201 SET carga_conferida = abap_true
                        dt_conferencia  = sy-datum
                        hr_conferencia  = sy-uzeit
                        us_conferencia  = sy-uname
     WHERE nro_cg EQ i_nro_carga.

    MESSAGE 'Carga conferida com sucesso!' TYPE 'S'.

  ENDMETHOD.


  METHOD conferir_carga_v2.

    DATA: lv_answer TYPE c.

    DATA: lva_peso_descarga_kg TYPE zmmt0202-qtd_vinc_carga_kg.

    zcl_carga_entrada_insumos=>validacoes_conferencia(
      EXPORTING
        i_nro_carga = i_nro_carga
      IMPORTING
        e_msg_erro = e_msg_erro ).

    CHECK e_msg_erro IS INITIAL.

    SELECT SINGLE nro_cg , viagem_id
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
     WHERE nro_cg EQ @i_nro_carga.

    IF lwa_zmmt0201-viagem_id IS NOT INITIAL.

      SELECT SINGLE viagem_id, nm_peso_liquido, ck_descarregado
        FROM zlest0185 INTO @DATA(lwa_zlest0185)
       WHERE viagem_id EQ @lwa_zmmt0201-viagem_id.

      IF sy-subrc NE 0.
        e_msg_erro = |Viagem { lwa_zmmt0201-viagem_id } não encontrada!|.
        RETURN.
      ENDIF.

      DATA(_descarrega_carguero)  = abap_true.
      SELECT SINGLE *
        FROM tvarvc INTO @DATA(lwa_carga_insumos_descarga)
       WHERE name EQ 'CARGA_INSUMOS_NOT_DESCARGA'.

      IF sy-subrc EQ 0.
        _descarrega_carguero = abap_false.
      ENDIF.

      IF lwa_zlest0185-ck_descarregado IS INITIAL AND _descarrega_carguero EQ abap_true.

        IF lwa_zlest0185-nm_peso_liquido IS INITIAL.
          e_msg_erro = |Viagem { lwa_zmmt0201-viagem_id } sem peso de carregamento!|.
          RETURN.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'ATENÇÃO'
            text_question         = 'Confirma Descarregamento da Carga no sistema Carguero?'
            text_button_1         = 'Sim'
            icon_button_1         = 'ICON_CHECKED'
            text_button_2         = 'Não'
            icon_button_2         = 'ICON_INCOMPLETE'
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_answer.

        IF lv_answer <> '1'.
          e_msg_erro = 'Operação abortada!'.
          RETURN.
        ENDIF.

        TRY .
            zcl_integracao_viagem_descarg=>zif_integracao_viagem_descarg~get_instance(
              )->set_viagem_descarregar(
              EXPORTING
                i_viagem_id             = lwa_zmmt0201-viagem_id
                i_dt_descarga           = sy-datum
                i_peso_liquido          = CONV #( lwa_zlest0185-nm_peso_liquido )
            ).

          CATCH zcx_integracao INTO DATA(ex_integracao).
            zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga bloqueio = abap_false ).

            DATA(_msg_retorno) = ex_integracao->get_text( ).
            e_msg_erro = |Não foi possivel confirmar a descarga no Carguero! Msg: { _msg_retorno } |.
            RETURN.

          CATCH zcx_error INTO DATA(ex_error).
            zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_carga bloqueio = abap_false ).

            _msg_retorno = ex_error->get_text( ).
            e_msg_erro = |Não foi possivel confirmar a descarga no Carguero! Msg: { _msg_retorno } |.
            RETURN.
        ENDTRY.

      ENDIF.

    ENDIF.

    CALL FUNCTION 'ZMMF_INICIAR_CONF_CARGA'
      EXPORTING
        i_nro_cg = i_nro_carga.


  ENDMETHOD.


  METHOD desvincula_conferencia.

    LOOP AT i_conferencia ASSIGNING FIELD-SYMBOL(<fs_conferencia>).
      UPDATE zmmt0218 SET cancel = abap_true WHERE id_chegada_cd_luft = <fs_conferencia>-id_chegada_cd_luft
                                               AND id_item_chegada    = <fs_conferencia>-id_item_chegada
                                               AND nro_cg             = <fs_conferencia>-nro_cg
                                               AND item_carga         = <fs_conferencia>-item_carga
                                               AND chave_nfe          = <fs_conferencia>-chave_nfe
                                               AND prod_item          = <fs_conferencia>-prod_item.
    ENDLOOP.

    COMMIT WORK.

  ENDMETHOD.


  METHOD dispara_email_fornecedor.

    DATA: lo_send_request TYPE REF TO cl_bcs.
    DATA: lo_document TYPE REF TO cl_document_bcs.
    DATA: lo_sender TYPE REF TO if_sender_bcs.
    DATA: lo_recipient   TYPE REF TO if_recipient_bcs,
          lt_solix       TYPE solix_tab,
          lv_sent_to_all TYPE os_boolean,
          lv_pdf_size    TYPE so_obj_len,
          lv_titulo      TYPE so_obj_des.

    DATA: it_text TYPE bcsy_text. "Internal table for email body

    DATA: lv_ponto_coleta TYPE lifnr.

    SELECT SINGLE PONTO_COLETA
      FROM zmmt0201 INTO lv_ponto_coleta
      WHERE nro_cg EQ i_nro_cg.

    CHECK sy-subrc eq 0.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_ponto_coleta
      IMPORTING
        output = lv_ponto_coleta.

    IF sy-sysid = 'PRD'.

      SELECT SINGLE b~smtp_addr
        FROM but020 AS a
        INNER JOIN adr6 AS b
        ON b~addrnumber = a~addrnumber
        INTO @DATA(lv_email)
        WHERE partner = @lv_ponto_coleta.

    ELSE.

      lv_email = 'suporte.sap@amaggi.com.br'.

    ENDIF.

    TRY.
        lo_send_request = cl_bcs=>create_persistent( ).

        lv_pdf_size = xstrlen( i_pdf_xstring ).

        lt_solix = cl_bcs_convert=>xstring_to_solix( iv_xstring = i_pdf_xstring ).

        CONCATENATE 'Autorização de embarque da carga' i_nro_cg INTO lv_titulo SEPARATED BY space.

        " Add the attachment (replace with your actual attachment logic)
        lo_document = cl_document_bcs=>create_document(
                  i_type    = 'PDF'
                  i_hex     = lt_solix
                  i_length  = lv_pdf_size
                  i_subject = lv_titulo ).                  "#EC NOTEXT


        lo_send_request->set_document( lo_document ).

        lo_recipient = cl_cam_address_bcs=>create_internet_address(
        i_address_string = lv_email ).

        lo_send_request->add_recipient( i_recipient = lo_recipient ).

        lv_sent_to_all = lo_send_request->send(
            i_with_error_screen = 'X' ).

        COMMIT WORK.

      CATCH cx_bcs INTO DATA(lo_bcs_exception).

        MESSAGE lo_bcs_exception->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD elimina_notas_carga.
    DATA: lr_chaves_del TYPE RANGE OF zmmt0203-chave_nfe.

    SELECT *
      FROM zcds_lista_cargas
      UP TO 1 ROWS
      INTO @DATA(ls_carga)
    WHERE nrocg = @i_nro_carga.
    ENDSELECT.

    lr_chaves_del = VALUE #( FOR ls_chaves_del IN i_chaves
                                 ( sign = 'I'
                                  option = 'EQ'
                                  low = ls_chaves_del-chave
                                  ) ).

    IF i_transferencia IS INITIAL.

      IF ls_carga-status >= 11.
        e_msg_erro = 'Não é possível elminiar, carga já conferida'.
        RETURN.
      ENDIF.

      SELECT *
        FROM zib_nfe_dist_itm
        INTO TABLE @DATA(lt_dist_itm)
        WHERE chave_nfe in @lr_chaves_del
          AND belnr_ft <> @space.
      IF  sy-subrc IS INITIAL.
        SORT lt_dist_itm BY chave_nfe.
      ENDIF.

      LOOP AT i_chaves ASSIGNING FIELD-SYMBOL(<fs_chaves>).

        READ TABLE lt_dist_itm TRANSPORTING NO FIELDS
        WITH KEY chave_nfe = <fs_chaves>-chave
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          e_msg_erro = 'Não é possível eliminar, NF já está com entrada realizada'.
          RETURN.
        ELSE.
          APPEND INITIAL LINE TO lr_chaves_del ASSIGNING FIELD-SYMBOL(<fs_delete>).
          <fs_delete>-sign = 'I'.
          <fs_delete>-option = 'EQ'.
          <fs_delete>-low = <fs_chaves>-chave.
        ENDIF.

      ENDLOOP.

    ENDIF.

    UPDATE zmmt0203 SET date_cancel = sy-datum
              time_cancel = sy-uzeit
              user_cancel = sy-uname
              cancel = abap_true
              WHERE chave_nfe IN lr_chaves_del.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD gerar_autorizacao_embarque.

    CLEAR: r_sucesso, e_msg_error.

    zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg = i_nro_cg
                                                                     bloqueio = abap_true
                                                           IMPORTING msg = DATA(lv_msg) ).
    IF lv_msg IS NOT INITIAL.
      e_msg_error = lv_msg.
      RETURN.
    ENDIF.

    zcl_carga_entrada_insumos=>gerar_autorizacao_emb_core(
      EXPORTING
        i_nro_cg                       = i_nro_cg
        i_aprova_viagem_carguero       = i_aprova_viagem_carguero
        i_envia_autorizacao_carguero   = i_envia_autorizacao_carguero
        i_background                   = i_background
      IMPORTING
        e_msg_error                    = e_msg_error
      RECEIVING
        r_sucesso                      = r_sucesso ).

    zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga(
      EXPORTING
        i_nro_cg = i_nro_cg
        bloqueio = abap_false
      IMPORTING
        msg = lv_msg ).

  ENDMETHOD.


  METHOD gerar_autorizacao_emb_core.

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

    CHECK i_nro_cg IS NOT INITIAL.

    SELECT SINGLE *
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
      WHERE nro_cg EQ @i_nro_cg
        AND cancel EQ @space.

    IF sy-subrc NE 0.
      e_msg_error = |Carga { i_nro_cg } não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_zmmt0201-dt_autorizacao_embarque IS NOT INITIAL.
      e_msg_error = |Autorização já emitida, acessar os anexos da carga|.
      RETURN.
    ENDIF.

    IF i_background EQ abap_false.
      e_msg_error = zcl_carga_entrada_insumos=>check_permissao_carga(
          EXPORTING
             i_nro_carga = lwa_zmmt0201-nro_cg
             i_atividade = '08' ).
    ENDIF.

    CHECK e_msg_error IS INITIAL.

    SELECT SUM( qtd_vinc_carga_kg )
      FROM zmmt0202
      INTO @DATA(lv_total_item)
      WHERE nro_cg = @i_nro_cg
        AND cancel EQ @space.
    IF lv_total_item = 0.
      e_msg_error = 'Não é possível autorizar embarque, carga ainda sem itens!'.
      RETURN.
    ENDIF.

    IF i_background EQ abap_false.

      IF lv_total_item < lwa_zmmt0201-qtd_total_kg.

        lv_question = | A carga ainda está incompleta, a quantidade em KG dos itens é Menor que a Quantidade prevista em KG da Carga! Deseja Continuar? |.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question = lv_question
            text_button_1 = 'Sim'
            icon_button_1 = 'ICON_CHECKED'
            text_button_2 = 'Não'
            icon_button_2 = 'ICON_INCOMPLETE'
          IMPORTING
            answer        = lv_answer.

        IF lv_answer <> '1'.
          RETURN.
        ENDIF.
      ENDIF.

    ENDIF.

*---------------------------------------------------------------------------------------------------------*
*   Pré Visualização Carga
*---------------------------------------------------------------------------------------------------------*
    IF i_background EQ abap_false.
      TRY .
          zcl_carga_entrada_insumos=>gerar_autorizacao_embarque_sm(
           EXPORTING
            i_carga   = lwa_zmmt0201-nro_cg
            i_preview = abap_true
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

    SELECT SINGLE transf_no_fornecedor
      FROM zmmt0196 INTO @DATA(lv_transf)
      WHERE nro_sol = @lwa_zmmt0201-nro_sol.

    IF sy-subrc IS INITIAL.
      IF lv_transf IS NOT INITIAL.
        UPDATE zmmt0201 SET transf_no_fornecedor = lv_transf
          WHERE nro_cg = lwa_zmmt0201-nro_cg.
      ENDIF.
    ENDIF.

*---------------------------------------------------------------------------------------------------------*
*   Gerar XString da Autorização Embarque
*---------------------------------------------------------------------------------------------------------*
    TRY.
        zcl_carga_entrada_insumos=>gerar_autorizacao_embarque_sm(
         EXPORTING
          i_carga   = lwa_zmmt0201-nro_cg
          i_binary  = abap_true
          i_preview = abap_false
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

*---------------------------------------------------------------------------------------------------------*
*   Anexar Documentos na Carga
*---------------------------------------------------------------------------------------------------------*

    CLEAR: lt_arquivos[].

    APPEND INITIAL LINE TO lt_arquivos ASSIGNING FIELD-SYMBOL(<fs_arquivos>).
    <fs_arquivos>-descricao = |Autorização Embarque { sy-datum } - { sy-uzeit }.pdf|.
    <fs_arquivos>-xstring   = lva_pdf_xstring.
    <fs_arquivos>-tipo      = 'PDF'.

    CALL METHOD zcl_carga_entrada_insumos=>anexar_documentos_carga(
      EXPORTING
        i_nro_carga = lwa_zmmt0201-nro_cg
        i_arquivos  = lt_arquivos
      IMPORTING
        e_msg_erro  = DATA(lv_msg_erro_anexo) ).

    UPDATE zmmt0201 SET dt_autorizacao_embarque = sy-datum
     WHERE nro_cg = lwa_zmmt0201-nro_cg.

*---------------------------------------------------------------------------------------------------------*
* Integração Carguero/Strada
*---------------------------------------------------------------------------------------------------------*
    IF lwa_zmmt0201-viagem_id IS NOT INITIAL.

      IF i_envia_autorizacao_carguero EQ abap_true AND
         lwa_zmmt0201-ds_url_file_carguero IS INITIAL. "Não foi enviado ainda... Só pode enviar uma vez..

        TRY.
            zcl_carga_entrada_insumos=>envia_path_autoriza_embarque(
              EXPORTING
                i_carga       = lwa_zmmt0201-nro_cg
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

    r_sucesso = abap_true.

    zcl_carga_entrada_insumos=>dispara_email_fornecedor(
      i_nro_cg      = lwa_zmmt0201-nro_cg
      i_pdf_xstring = lva_pdf_xstring ).

  ENDMETHOD.


  METHOD get_spart_carga.

    CLEAR: r_spart.

    SELECT SINGLE *
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
      WHERE nro_cg EQ @i_nro_carga.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zmmt0196 INTO @DATA(lwa_zmmt0196)
     WHERE nro_sol EQ @lwa_zmmt0201-nro_sol.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM ekpo INTO @DATA(lwa_ekpo)
     WHERE ebeln EQ @lwa_zmmt0196-ebeln.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zmmt0200 INTO @DATA(lwa_zmmt0200)
      WHERE matkl EQ @lwa_ekpo-matkl.

    CHECK sy-subrc EQ 0.

    r_spart = lwa_zmmt0200-spart.

  ENDMETHOD.


  METHOD gravar_carga_core.

    DATA: lwa_0201 TYPE zmmt0201,
          lwa_0204 TYPE zsdt0391,
          lwa_0346 TYPE zsdt0346.

    CLEAR: r_msg_error.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_zmmt0201-cod_motorista
      IMPORTING
        output = i_zmmt0201-cod_motorista.

    r_msg_error =  zcl_carga_entrada_insumos=>validar_dados(
                                                i_zmmt0201  = i_zmmt0201
                                                i_zmmt0202  = i_zmmt0202
                                                i_zmmt0203  = i_zmmt0203
                                                i_zsdt0391  = i_zsdt0391
                                                i_zsdt0346  = i_zsdt0346  ).

    CHECK r_msg_error IS INITIAL.

    "Se já foi criado carga para viagem id, não cria mais
    IF i_zmmt0201-viagem_id IS NOT INITIAL.
      SELECT SINGLE *
        FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
       WHERE viagem_id EQ @i_zmmt0201-viagem_id
         AND cancel    EQ @abap_false.

      CHECK ( sy-subrc NE 0 ) OR
            ( sy-subrc EQ 0 AND
              lwa_zmmt0201-nro_cg EQ i_zmmt0201-nro_cg AND
              i_zmmt0201-nro_cg IS NOT INITIAL ).
    ENDIF.

    IF i_zmmt0201-nro_cg IS INITIAL.

      DATA(_spart) = zcl_solicitacao_entrada_insumo=>get_spart( i_nro_solicitacao = i_zmmt0201-nro_sol ).

      IF sy-batch EQ abap_false.
        r_msg_error = zcl_carga_entrada_insumos=>check_permissao_carga_core(
                        i_bukrs     = i_zmmt0201-bukrs
                        i_atividade = '01' "Montar Carga
                        i_spart     = _spart ).
      ENDIF.

      CHECK r_msg_error IS INITIAL.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZSD_INS_CG'
        IMPORTING
          number                  = i_zmmt0201-nro_cg
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF sy-subrc NE 0 OR i_zmmt0201-nro_cg IS INITIAL.
        r_msg_error = |Não foi possivel determinar numeração para o objeto ZSD_INS_CG|.
        RETURN.
      ENDIF.

    ENDIF.

    e_nro_carga = i_zmmt0201-nro_cg.

    LOOP AT i_zmmt0202 ASSIGNING FIELD-SYMBOL(<fs_0202>).
      <fs_0202>-nro_cg =  i_zmmt0201-nro_cg.
    ENDLOOP.

    LOOP AT i_zmmt0203 ASSIGNING FIELD-SYMBOL(<fs_0203>).
      <fs_0203>-nro_cg =  i_zmmt0201-nro_cg.
    ENDLOOP.

    IF i_zsdt0391 IS NOT INITIAL.
      i_zsdt0391-nro_cg =  i_zmmt0201-nro_cg.
    ENDIF.

    SELECT MAX( seq_cotacao )
      FROM zsdt0346
      INTO @DATA(lv_seq_cotacao)
      WHERE nro_carga = @i_zmmt0201-nro_cg.
    IF sy-subrc IS INITIAL.
      ADD 1 TO lv_seq_cotacao.
    ENDIF.

    LOOP AT i_zsdt0346 ASSIGNING FIELD-SYMBOL(<fs_0346>).
      <fs_0346>-nro_carga =  i_zmmt0201-nro_cg.
      <fs_0346>-seq_cotacao = lv_seq_cotacao.
    ENDLOOP.

*------------------------------------------------------------------------------------------------------------*
*   Gravar Cabeçalho Carga
*------------------------------------------------------------------------------------------------------------*

    MODIFY zmmt0201 FROM i_zmmt0201.

*------------------------------------------------------------------------------------------------------------*
*   Gravar Itens Carga
*------------------------------------------------------------------------------------------------------------*

    IF i_zmmt0202[] IS INITIAL AND i_determina_itens_auto EQ abap_true. "Checa se determina automatico

      IF i_zmmt0201-viagem_id IS NOT INITIAL.
        DATA(_tp_saldo) = 'C'. "Carguero
      ELSE.
        _tp_saldo = 'G'. "Carga
      ENDIF.

      zcl_carga_entrada_insumos=>definir_itens_carga(
         EXPORTING
           i_nro_solicitacao = i_zmmt0201-nro_sol
           i_nro_carga       = i_zmmt0201-nro_cg
           i_tp_saldo        = _tp_saldo
         IMPORTING
           i_itens_carga     = i_zmmt0202
       ).

      IF i_zmmt0202[] IS INITIAL.
        r_msg_error = 'Não foi possível determinar automaticamente os itens da Carga de Entrada!'.
        RETURN.
      ENDIF.

      "Revalidar Informaçoes Carga
      r_msg_error =  zcl_carga_entrada_insumos=>validar_dados(
                                                  i_zmmt0201  = i_zmmt0201
                                                  i_zmmt0202  = i_zmmt0202
                                                  i_zmmt0203  = i_zmmt0203
                                                  i_zsdt0391  = i_zsdt0391
                                                  i_zsdt0346  = i_zsdt0346  ).

      CHECK r_msg_error IS INITIAL.

    ENDIF.

    IF i_zmmt0202[] IS NOT INITIAL.
      MODIFY zmmt0202 FROM TABLE i_zmmt0202.
    ENDIF.


*------------------------------------------------------------------------------------------------------------*
*   Gravar Notas Carga
*------------------------------------------------------------------------------------------------------------*

    IF i_zmmt0203[] IS NOT INITIAL.
      MODIFY zmmt0203 FROM TABLE i_zmmt0203.
    ENDIF.


*------------------------------------------------------------------------------------------------------------*
*   Gravar Frete Carga
*------------------------------------------------------------------------------------------------------------*
    IF i_zsdt0391 IS NOT INITIAL.
      MODIFY zsdt0391 FROM i_zsdt0391.
    ENDIF.

*------------------------------------------------------------------------------------------------------------*
*   Gravar Cotação Carga
*------------------------------------------------------------------------------------------------------------*
    IF i_zsdt0346[] IS NOT INITIAL.
      MODIFY zsdt0346 FROM TABLE i_zsdt0346.
    ENDIF.




  ENDMETHOD.


  METHOD grava_carga.

    DATA: lo_cargueiro TYPE REF TO zcl_integracao_lote_frete,
          lo_retorno   TYPE REF TO zif_integracao_lote_frete,

          lwa_0201     TYPE zmmt0201,
          lwa_0391     TYPE zsdt0391,
          lt_0346      TYPE TABLE OF zsdt0346,
          lt_0202      TYPE TABLE OF zmmt0202,

          lv_msg       TYPE string,
          lt_0203_save TYPE TABLE OF zmmt0203.

    CLEAR: e_msg_erro.

    CLEAR: lwa_0201, lwa_0391,lt_0346[],lt_0202[].

*------------------------------------------------------------------------------------------------------------*
*   Montar Cabeçalho Carga
*------------------------------------------------------------------------------------------------------------*

    lwa_0201-nro_sol = i_dados-nro_sol.

    SELECT transf_no_fornecedor,ebeln
      FROM zmmt0196
      INTO @DATA(ls_0196)
      UP TO 1 ROWS
      WHERE nro_sol = @i_dados-nro_sol.
    ENDSELECT.

    IF sy-subrc IS INITIAL.
      lwa_0201-transf_no_fornecedor = ls_0196-transf_no_fornecedor.

      SELECT SINGLE bukrs
        FROM ekko
        INTO lwa_0201-bukrs
        WHERE ebeln = ls_0196-ebeln.

    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_dados-transportadora
      IMPORTING
        output = lwa_0201-cod_transportadora.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_dados-motorista
      IMPORTING
        output = lwa_0201-cod_motorista.

    lwa_0201-cod_motorista = i_dados-motorista.
    lwa_0201-nome_motorista     = i_dados-desc_motorista.
    lwa_0201-placa_cav          = i_dados-placa_cavalo.
    lwa_0201-placa_car1         = i_dados-placa_carreta1.
    lwa_0201-placa_car2         = i_dados-placa_carreta2.
    lwa_0201-placa_car3         = i_dados-placa_dolly.
    lwa_0201-dt_prevista_embarque  = i_dados-data_embarque.
    lwa_0201-inco1                 = i_dados-tipo_frete.
    lwa_0201-qtd_total_kg          = i_dados-qtd_prevista.
    lwa_0201-ponto_coleta          = i_dados-ponto_coleta.
    lwa_0201-local_entrega         = i_dados-local_entrega.
    lwa_0201-user_create = sy-uname.
    lwa_0201-date_create = sy-datum.
    lwa_0201-time_create = sy-uzeit.

*------------------------------------------------------------------------------------------------------------*
*   Montar Frete Carga
*------------------------------------------------------------------------------------------------------------*

    IF i_dados-frete_por_t IS NOT INITIAL.
      lwa_0391-modalidade_pag_frete = '1'.
    ELSEIF i_dados-frete_por_v IS NOT INITIAL.
      lwa_0391-modalidade_pag_frete = '2'.
    ENDIF.

    lwa_0391-dt_cotacao_frete = sy-datum.
    lwa_0391-dt_contratacao_frete = sy-datum.
    lwa_0391-preco_total_frete = i_dados-valor_frete.
    lwa_0391-user_create = sy-uname.
    lwa_0391-date_create = sy-datum.
    lwa_0391-time_create = sy-uzeit.

*------------------------------------------------------------------------------------------------------------*
*   Montar Cotação Carga
*------------------------------------------------------------------------------------------------------------*

    APPEND INITIAL LINE TO lt_0346 ASSIGNING FIELD-SYMBOL(<fs_0346>).

    <fs_0346>-id_operacao = '00000001'.

    SELECT SINGLE tipo_transporte
      FROM zsdt0345
      INTO @DATA(lv_tipo_transp)
      WHERE id = @<fs_0346>-id_operacao.
    IF sy-subrc IS INITIAL.
      <fs_0346>-tipo_transporte = lv_tipo_transp.
    ENDIF.

    SELECT SINGLE lzone
      FROM lfa1
      INTO @DATA(lv_lzone)
      WHERE lifnr = @i_dados-ponto_coleta.
    IF sy-subrc IS INITIAL.
      <fs_0346>-ponto_coleta = lv_lzone.
    ENDIF.

    SELECT SINGLE lzone
    FROM lfa1
    INTO lv_lzone
    WHERE lifnr = i_dados-local_entrega.
    IF sy-subrc IS INITIAL.
      <fs_0346>-local_entrega = lv_lzone.
    ENDIF.

    SELECT route
      FROM trolz
      INTO @DATA(lv_route)
      UP TO 1 ROWS
      WHERE azone = @<fs_0346>-ponto_coleta
        AND lzone = @<fs_0346>-local_entrega.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      <fs_0346>-itinerario = lv_route.
    ENDIF.

    <fs_0346>-unidade_condicao = 'BRL'.
    <fs_0346>-valor = i_dados-valor_frete.

    IF i_dados-frete_por_t IS NOT INITIAL.
      <fs_0346>-unidade_medida = 'TO'.
    ENDIF.

    <fs_0346>-usuario = sy-uname.
    <fs_0346>-data = sy-datum.
    <fs_0346>-hora = sy-uzeit.

*------------------------------------------------------------------------------------------------------------*
*   Gravar Carga
*------------------------------------------------------------------------------------------------------------*

    zcl_carga_entrada_insumos=>gravar_carga_core(
      IMPORTING
        e_nro_carga = DATA(lv_nro_carga)
      CHANGING
        i_zmmt0201  = lwa_0201
        i_zsdt0391  = lwa_0391
        i_zsdt0346  = lt_0346
      RECEIVING
        r_msg_error = e_msg_erro
    ).

    IF lv_nro_carga IS NOT INITIAL.

      e_carga =  lv_nro_carga.

      zcl_solicitacao_entrada_insumo=>desbloqueia_solicitacao(
      EXPORTING
        nro_solicitacao = i_dados-nro_sol
        IMPORTING
          desbloqueado = DATA(lv_desbloq) ).

      CLEAR: i_dados-desc_motorista,
             i_dados-motorista,
             i_dados-transportadora,
             i_dados-desc_transp,
             i_dados-tipo_frete,
             i_dados-qtd_prevista,
             i_dados-valor_frete,
             i_dados-placa_carreta1,
             i_dados-placa_carreta2,
             i_dados-placa_cavalo,
             i_dados-placa_dolly.

    ENDIF.

  ENDMETHOD.


  METHOD HABILITAR_EDICAO_CHAVES.

    CLEAR: r_msg_error.

    SELECT SINGLE *
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
      WHERE nro_cg EQ @i_nro_carga.

    IF sy-subrc NE 0 OR i_nro_carga IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_zmmt0201-dt_autorizacao_embarque IS INITIAL.
      r_msg_error = 'A carga sem autorização de Embarque! Operação não permitida!'.
      RETURN.
    ENDIF.

    IF lwa_zmmt0201-carga_conferida IS NOT INITIAL.
      r_msg_error = 'A carga já foi conferida! Operação não permitida!'.
      RETURN.
    ENDIF.

    r_msg_error = zcl_carga_entrada_insumos=>check_permissao_carga(
        EXPORTING
           i_nro_carga = i_nro_carga
           i_atividade = '07' ).

    CHECK r_msg_error IS INITIAL.


  ENDMETHOD.


  METHOD habilitar_edicao_dados_logist.

    CLEAR: r_msg_error.

    SELECT SINGLE *
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
      WHERE nro_cg EQ @i_nro_carga.

    IF sy-subrc NE 0 OR i_nro_carga IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    SELECT SINGLE *
     FROM zmmt0203 INTO @DATA(lwa_zmmt0203)
     WHERE nro_cg EQ @lwa_zmmt0201-nro_cg
       AND cancel EQ @abap_false.

    IF sy-subrc EQ 0.
      r_msg_error = 'Carga já possui NF-e informada! Operaçao não permitida!'.
      RETURN.
    ENDIF.

    r_msg_error = zcl_carga_entrada_insumos=>check_permissao_carga(
        EXPORTING
           i_nro_carga = i_nro_carga
           i_atividade = '04' ).

    CHECK r_msg_error IS INITIAL.

    r_msg_error = zcl_carga_entrada_insumos=>check_reinicializacao_aut_emb(
      EXPORTING
        i_nro_carga = i_nro_carga
    ).

    CHECK r_msg_error IS INITIAL.



  ENDMETHOD.


  METHOD habilitar_edicao_itens.

    CLEAR: r_msg_error.

    SELECT SINGLE *
      FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
      WHERE nro_cg EQ @i_nro_carga.

    IF sy-subrc NE 0 OR i_nro_carga IS INITIAL.
      r_msg_error = |Carga não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_zmmt0201-carga_conferida IS NOT INITIAL.
      r_msg_error = 'A carga já foi conferida! Operação não permitida!'.
      RETURN.
    ENDIF.

    r_msg_error = zcl_carga_entrada_insumos=>check_permissao_carga(
        EXPORTING
           i_nro_carga = i_nro_carga
           i_atividade = '03' ).

    CHECK r_msg_error IS INITIAL.

    SELECT SINGLE *
      FROM zmmt0203 INTO @DATA(lwa_zmt0203)
     WHERE nro_cg EQ @i_nro_carga
       AND cancel EQ @abap_false.

    IF sy-subrc NE 0. "Não possui NF-e
      r_msg_error = zcl_carga_entrada_insumos=>check_reinicializacao_aut_emb(
        EXPORTING
          i_nro_carga = i_nro_carga
      ).

      CHECK r_msg_error IS INITIAL.
    ENDIF.

    CHECK r_msg_error IS INITIAL.

    UPDATE zmmt0218 SET cancel = abap_true
     WHERE nro_cg EQ i_nro_carga.



  ENDMETHOD.


  METHOD inconsistencia_bordero.
    DATA: lo_aceite_receb TYPE REF TO zcl_int_ob_aceite_receb_luft.

    CREATE OBJECT lo_aceite_receb.

    TRY .
        CALL METHOD lo_aceite_receb->zif_integracao_outbound~execute_request
          EXPORTING
            i_info_request  = i_dados
          IMPORTING
            e_id_integracao = DATA(lv_id_integracao).

        LOOP AT i_dados-pedido INTO DATA(lwa_pedido).
          UPDATE zmmt0218 SET cd_notificado_correcao = abap_true
           WHERE chave_nfe = lwa_pedido-chave_acesso_nota_fiscal.
        ENDLOOP.

      CATCH zcx_integracao INTO DATA(lo_integracao).
        e_msg_erro = lo_integracao->get_text( ).

      CATCH zcx_error INTO DATA(lo_error).
        e_msg_erro = lo_error->get_text( ).
    ENDTRY.


  ENDMETHOD.


  METHOD informa_qtd_conferencia.

    DATA: lva_qtd_conv_nfe TYPE zmmt0218-qtd_conv_nfe.

    SELECT SINGLE *
      FROM zmmt0218
      INTO @DATA(ls_0218)
      WHERE id_chegada_cd_luft = @i_De_para-id_chegada_cd_luft
        AND id_item_chegada    = @i_De_para-id_item_chegada
        AND nro_cg             = @i_De_para-nro_cg
        AND item_carga         = @i_De_para-item_carga
        AND chave_nfe          = @i_De_para-chave_nfe
        AND prod_item          = @i_De_para-item_nfe
        AND cancel             = @space.
    IF sy-subrc IS INITIAL.
      IF i_de_para-quantidade < ls_0218-quantidade.
        DATA(lv_menor) = abap_true.
      ENDIF.

      c_bordero-saldo = c_bordero-saldo + ls_0218-quantidade.
      c_carga-saldo   = c_carga-saldo + ls_0218-quantidade.
      c_notas-saldo   = c_notas-saldo + ls_0218-quantidade.

    ENDIF.

    IF c_notas-peso_conv IS INITIAL.
      e_msg_erro = | { 'Item' } { c_notas-item_nfe } { 'da NFe' } { c_notas-numero_nfe } { 'não possui qtd de conversão informada' } |.
      RETURN.
    ENDIF.

    IF c_bordero-saldo > 0 AND i_de_para-quantidade > c_bordero-saldo AND lv_menor IS INITIAL .
      e_msg_erro = | { 'Item' } { c_bordero-id_item_chegada } { 'do borderô' } { c_bordero-id } { 'possui saldo menor que a quantidade informada' } |.
      RETURN.
    ENDIF.

    IF c_carga-saldo > 0 AND i_de_para-quantidade > c_carga-saldo AND lv_menor IS INITIAL.
      e_msg_erro = | { 'Item' } { c_carga-item_carga } { 'da carga' } { 'possui saldo menor que a quantidade informada' } |.
      RETURN.
    ENDIF.

    IF c_notas-saldo > 0 AND i_de_para-quantidade > c_notas-saldo AND lv_menor IS INITIAL.
      e_msg_erro = | { 'Item' } { c_notas-item_nfe } { 'da nota' } { c_notas-numero_nfe } { 'possui saldo menor que a quantidade informada' } |.
      RETURN.
    ENDIF.

    c_carga-saldo   = c_carga-saldo   - i_de_para-quantidade.
    c_bordero-saldo = c_bordero-saldo - i_de_para-quantidade.
    c_notas-saldo   = c_notas-saldo   - i_de_para-quantidade.

    "lva_qtd_conv_nfe = i_de_para-quantidade * c_notas-peso_conv.

    UPDATE zmmt0218 SET quantidade   = i_de_para-quantidade
                        qtd_conv_nfe = c_notas-qtd_conv
    WHERE id_chegada_cd_luft = c_bordero-id
      AND id_item_chegada    = c_bordero-id_item_chegada
      AND nro_cg             = i_nro_carga
      AND item_carga         = c_carga-item_carga
      AND chave_nfe          = c_notas-chave_nfe
      AND prod_item          = c_notas-item_nfe.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.


  ENDMETHOD.


  METHOD montar_carga.

    zcl_carga_entrada_insumos=>busca_dados_montar_carga(
    EXPORTING
      i_empresa  = i_empresa
      i_segmento = i_segmento
      i_nro_sol  = i_nro_sol
      i_dt_sol   = i_dt_sol
      i_pedido   = i_pedido
    IMPORTING
      e_tabela_monta_carga = e_tabela_monta_carga
      e_msg_erro = e_msg_erro
    RECEIVING
      sucesso    = sucesso ).


  ENDMETHOD.


  METHOD monta_dados_aceite_fiscal.

    DATA(lt_notas) = i_notas.

    IF lt_notas IS NOT INITIAL.
      SELECT *
        FROM zib_nfe_dist_ter
        INTO TABLE @DATA(lt_dist_ter)
        FOR ALL ENTRIES IN @lt_notas
        WHERE chave_nfe = @lt_notas-chave_nfe.
      IF sy-subrc IS INITIAL.
        SORT lt_dist_ter BY chave_nfe ck_fiscal.
      ENDIF.
    ENDIF.


    IF i_cancelar_aceite IS INITIAL.

      lt_notas = i_notas.
      SORT lt_notas BY chave_nfe.
      DELETE ADJACENT DUPLICATES FROM lt_notas COMPARING chave_nfe.

      DATA(lt_de_para) = i_de_para.

      SORT lt_de_para BY chave_nfe.

      LOOP AT lt_notas ASSIGNING FIELD-SYMBOL(<fs_notas>).

        READ TABLE lt_dist_ter TRANSPORTING NO FIELDS
        WITH KEY chave_nfe = <fs_notas>-chave_nfe
                 ck_fiscal = abap_true
                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE lt_de_para TRANSPORTING NO FIELDS
        WITH KEY chave_nfe = <fs_notas>-chave_nfe
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          LOOP AT lt_de_para ASSIGNING FIELD-SYMBOL(<fs_de_para>) FROM sy-tabix.
            IF <fs_notas>-chave_nfe <> <fs_de_para>-chave_nfe.
              EXIT.
            ENDIF.

            APPEND INITIAL LINE TO e_dados_aceite_fiscal ASSIGNING FIELD-SYMBOL(<fs_aceite_fiscal>).
            <fs_aceite_fiscal>-chave_nfe = <fs_notas>-chave_nfe.
            <fs_aceite_fiscal>-cod_nfe   = <fs_notas>-numero_nfe.

*** Valida se todos os itens da nota estão com saldo 0
            DATA(lt_notas_aux) = i_notas.
            DELETE lt_notas_aux WHERE chave_nfe <> <fs_notas>-chave_nfe.
            DELETE lt_notas_aux WHERE saldo = 0.
            IF lt_notas_aux IS INITIAL.
*** Valida se todos os itens do bordero estão com saldo 0 e se o borderô é do tipo 'LF'
              DATA(lt_bordero_aux) = i_bordero.
              DELETE lt_bordero_aux WHERE id <> <fs_de_para>-id_chegada_cd_luft.
              DELETE lt_bordero_aux WHERE saldo = 0.
              IF lt_bordero_aux IS INITIAL.

                IF <fs_de_para>-cod_prod_bordero IS NOT INITIAL AND <fs_de_para>-cod_prod_bordero <> <fs_de_para>-cod_prod_pedido.
                  <fs_aceite_fiscal>-matnr           = <fs_de_para>-cod_prod_pedido.
                  <fs_aceite_fiscal>-maktx           = <fs_de_para>-desc_prod_pedido.
                  <fs_aceite_fiscal>-id_item_chegada = <fs_de_para>-id_item_chegada.
                  <fs_aceite_fiscal>-acao            = 'Corrigir Borderô'.
                  <fs_aceite_fiscal>-acao_bt         = 'BORDERO'.
                ELSE.
                  <fs_aceite_fiscal>-acao = 'Aceite Fiscal'.
                  <fs_aceite_fiscal>-acao_bt = 'ACEITE'.
                ENDIF.

              ELSE.
                <fs_aceite_fiscal>-acao = 'De-Para Incompleto'.
              ENDIF.
            ELSE.
              <fs_aceite_fiscal>-acao = 'De-Para Incompleto'.
            ENDIF.

          ENDLOOP.

        ELSE.
          APPEND INITIAL LINE TO e_dados_aceite_fiscal ASSIGNING <fs_aceite_fiscal>.
          <fs_aceite_fiscal>-chave_nfe = <fs_notas>-chave_nfe.
          <fs_aceite_fiscal>-cod_nfe   = <fs_notas>-numero_nfe.
          <fs_aceite_fiscal>-acao = 'De-Para Incompleto'.
        ENDIF.

      ENDLOOP.

      "Caso nota tenha correção, remover linhas de aceite.
      LOOP AT e_dados_aceite_fiscal ASSIGNING <fs_aceite_fiscal> WHERE acao_bt = 'BORDERO'.
        DELETE e_dados_aceite_fiscal WHERE chave_nfe = <fs_aceite_fiscal>-chave_nfe
                                       AND acao_bt = 'ACEITE'.
      ENDLOOP.


      LOOP AT e_dados_aceite_fiscal ASSIGNING <fs_aceite_fiscal>.
        READ TABLE i_de_para INTO DATA(lwa_depara_tmp) WITH KEY chave_nfe = <fs_aceite_fiscal>-chave_nfe .
        IF sy-subrc EQ 0 AND lwa_depara_tmp-cd_notificado_correcao EQ abap_true.
          <fs_aceite_fiscal>-acao = 'CD Notificado Correçao Material'.
          CLEAR: <fs_aceite_fiscal>-acao_bt.
        ENDIF.
      ENDLOOP.

    ELSE.

      LOOP AT lt_notas ASSIGNING <fs_notas>.
        READ TABLE lt_dist_ter TRANSPORTING NO FIELDS
        WITH KEY chave_nfe = <fs_notas>-chave_nfe
                 ck_fiscal = abap_true
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          APPEND INITIAL LINE TO e_dados_aceite_fiscal ASSIGNING <fs_aceite_fiscal>.

          <fs_aceite_fiscal>-chave_nfe = <fs_notas>-chave_nfe.
          <fs_aceite_fiscal>-cod_nfe   = <fs_notas>-numero_nfe.
          <fs_aceite_fiscal>-acao      = 'Cancelar Aceite Fiscal'.
          <fs_aceite_fiscal>-acao_bt   = 'CANCELAR'.

        ENDIF.

      ENDLOOP.

    ENDIF.

    DELETE ADJACENT DUPLICATES FROM e_dados_aceite_fiscal COMPARING ALL FIELDS.

  ENDMETHOD.


  METHOD validacoes_conferencia.

*** Valida permissão de acesso
    e_msg_erro = zcl_carga_entrada_insumos=>check_permissao_carga(
                  EXPORTING
                     i_nro_carga = i_nro_carga
                     i_atividade = '08' ).

    CHECK e_msg_erro IS INITIAL.

*** Valida se está com cancelamento parcial
    SELECT cancel_parcial
      FROM zmmt0201
      INTO @DATA(lv_cancel_parcial)
      UP TO 1 ROWS
      WHERE nro_cg = @i_nro_carga.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      IF lv_cancel_parcial IS NOT INITIAL.
        e_msg_erro = |{ TEXT-001 } { TEXT-002 } |.
        RETURN.
      ENDIF.
    ENDIF.

*** Valida se existe alguma nota
    SELECT *
      FROM zmmt0203
      INTO TABLE @DATA(lt_0203)
      WHERE nro_cg = @i_nro_carga.
    IF sy-subrc IS NOT INITIAL.
      e_msg_erro = TEXT-003.
      RETURN.

    ELSE.

*** Valida se há xml
      SELECT *
        FROM zib_nfe_dist_ter
        INTO TABLE @DATA(lt_dist_ter)
        FOR ALL ENTRIES IN @lt_0203
        WHERE chave_nfe = @lt_0203-chave_nfe.
      IF sy-subrc IS INITIAL  .
        SORT lt_dist_ter BY chave_nfe.
      ENDIF.

      LOOP AT lt_0203 ASSIGNING FIELD-SYMBOL(<fs_0203>).
        READ TABLE lt_dist_ter ASSIGNING FIELD-SYMBOL(<fs_dist_ter>)
        WITH KEY chave_nfe = <fs_0203>-chave_nfe
        BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          e_msg_erro = | { TEXT-004 } { <fs_0203>-chave_nfe } { TEXT-005 } |.
          RETURN.
        ENDIF.

      ENDLOOP.

    ENDIF.

*** Bloqueia carga
    zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga(
    EXPORTING
      i_nro_cg = i_nro_carga
      bloqueio = abap_true
      IMPORTING
        msg = e_msg_erro ).

  ENDMETHOD.


  METHOD validar_dados.

    DATA: lva_qtde_itens_kg TYPE zmmt0201-qtd_total_kg.

    CLEAR: r_msg_error.


*------------------------------------------------------------------------------------------------------------*
*  Validações Cabeçalho Carga
*------------------------------------------------------------------------------------------------------------*

    IF i_zmmt0201-bukrs IS INITIAL.
      r_msg_error = 'Empresa não informada na carga'.
      RETURN.
    ENDIF.

    IF i_zmmt0201-nro_sol IS INITIAL.
      r_msg_error = 'Solicitação não informada na carga'.
      RETURN.
    ENDIF.

    IF i_zmmt0201-qtd_total_kg IS INITIAL OR
       i_zmmt0201-inco1 IS INITIAL OR
       i_zmmt0201-cod_transportadora IS INITIAL OR
       i_zmmt0201-dt_prevista_embarque IS INITIAL OR
       i_zmmt0201-placa_cav IS INITIAL OR
       i_zmmt0201-placa_car1 IS INITIAL.

      r_msg_error = 'Os campos: Qtd Total,TipoFrete, Data Embarque,placa cavalo e Placa carreta são obrigatórios'.
      RETURN.

    ENDIF.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(lwa_lfa1_transp)
     WHERE lifnr EQ @i_zmmt0201-cod_transportadora.

    IF sy-subrc NE 0.
      r_msg_error = |Transportadora com codigo { i_zmmt0201-cod_transportadora } não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_lfa1_transp-ktokk = 'ZFIC' AND  i_zmmt0201-inco1 NE 'CIF'.
      r_msg_error = |Transportadora { i_zmmt0201-cod_transportadora } é intercompany! Tipo de Frete não pode ser { i_zmmt0201-inco1 }!|.
      RETURN.
    ENDIF.

    IF lwa_lfa1_transp-ktokk NE 'ZFIC' AND  i_zmmt0201-inco1 NE 'CPT'.
      r_msg_error = |Transportadora { i_zmmt0201-cod_transportadora } é terceira! Tipo de Frete não pode ser { i_zmmt0201-inco1 }!|.
      RETURN.
    ENDIF.

    "Ficará trava via parametro
*    SELECT SINGLE nrocg, placacav
*      FROM zcds_lista_cargas INTO @DATA(lwa_zmmt0201_open)
*     WHERE placacav         EQ @i_zmmt0201-placa_cav
*       AND nrocg            NE @i_zmmt0201-nro_cg
*       AND cancel           EQ @abap_false
*       AND status           LT '11'. "Status anterior a "CARGA CONFERIDA TOTAL"
*
*    IF sy-subrc EQ 0.
*      r_msg_error = |Existe uma Carga de Entrada não Conferida para a Placa Cavalo: { lwa_zmmt0201_open-placacav }! Nro Autorização Embarque: { lwa_zmmt0201_open-nrocg }!|.
*      RETURN.
*    ENDIF.

*------------------------------------------------------------------------------------------------------------*
*  Validações Itens Carga
*------------------------------------------------------------------------------------------------------------*

    CLEAR: lva_qtde_itens_kg.

    "Recuperar Saldo de Solicitações Vinculados na Carga
    zcl_solicitacao_entrada_insumo=>saldo_solicitacao(
      EXPORTING
        i_nro_cg          = i_zmmt0201-nro_cg
        i_nro_solicitacao = i_zmmt0201-nro_sol
        i_itens           = abap_true
        i_tp_saldo        = 'G'
      IMPORTING
        e_saldo = DATA(lt_saldo_solicitacao) ).

    SORT lt_saldo_solicitacao BY nro_solic seq tp_saldo.

    DATA(lit_zmmt0202_aux) = i_zmmt0202[].

    LOOP AT i_zmmt0202 ASSIGNING FIELD-SYMBOL(<fs_item>).
      IF <fs_item>-cancel = abap_true.
        CONTINUE.
      ENDIF.

      DATA(_count_item_duplicate) = 0.
      LOOP AT lit_zmmt0202_aux INTO DATA(lwa_zmmt0202_aux) WHERE nro_sol       = <fs_item>-nro_sol
                                                             AND seq           = <fs_item>-seq
                                                             AND tp_saldo_vinc = <fs_item>-tp_saldo_vinc
                                                             AND cancel        = abap_false.
        ADD 1 TO _count_item_duplicate.
      ENDLOOP.

      IF _count_item_duplicate > 1.
        r_msg_error = |Carga com Itens duplicados! Nro Sol { <fs_item>-nro_sol } Item Sol: { <fs_item>-seq } Tp. Saldo: { <fs_item>-tp_saldo_vinc } | .
        RETURN.
      ENDIF.

      IF <fs_item>-qtd_vinc_carga = 0.
        r_msg_error = 'Quantidade do item não pode ser 0'.
        RETURN.
      ENDIF.

      IF <fs_item>-tp_saldo_vinc IS INITIAL.
        r_msg_error = 'Tipo Saldo do Item não informado'.
        RETURN.
      ENDIF.

      IF <fs_item>-tp_saldo_vinc NE 'C' AND "Carguero
         <fs_item>-tp_saldo_vinc NE 'M'.     "Manual
        r_msg_error = 'Tipo Saldo do Item inválido!'.
        RETURN.
      ENDIF.

      READ TABLE lt_saldo_solicitacao ASSIGNING FIELD-SYMBOL(<fs_saldo_solicitacao>) WITH KEY nro_solic = <fs_item>-nro_sol
                                                                                              seq       = <fs_item>-seq
                                                                                              tp_saldo  = <fs_item>-tp_saldo_vinc BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        r_msg_error = | { 'Solicitação:' } { <fs_item>-nro_sol } { 'Item:' } { <fs_item>-seq } { 'já não possui mais saldo' } | .
        RETURN.
      ELSE.
        IF <fs_item>-qtd_vinc_carga > <fs_saldo_solicitacao>-saldo.
          r_msg_error = | { 'Solicitação:' } { <fs_item>-nro_sol } { 'Item:' } { <fs_item>-seq } { 'possui saldo atual de' } { <fs_saldo_solicitacao>-saldo } | .
          RETURN.
        ENDIF.
      ENDIF.

      ADD <fs_item>-qtd_vinc_carga_kg TO lva_qtde_itens_kg.

    ENDLOOP.

    IF lva_qtde_itens_kg > i_zmmt0201-qtd_total_kg AND i_zmmt0203 IS INITIAL.
      r_msg_error = 'Peso total dos itens, é maior que peso total da carga'.
      RETURN.
    ENDIF.

*------------------------------------------------------------------------------------------------------------*
*  Validações Notas Carga
*------------------------------------------------------------------------------------------------------------*
    DATA(i_zmmt0203_aux) = i_zmmt0203.

    IF i_zmmt0202[] IS INITIAL AND i_zmmt0203[] IS NOT INITIAL.
      r_msg_error = 'Carga não possui itens, portanto não pode ser informada notas na carga!'.
      RETURN.
    ENDIF.

    LOOP AT i_zmmt0203 ASSIGNING FIELD-SYMBOL(<fs_nota>).

      IF <fs_nota>-chave_nfe IS INITIAL.
        r_msg_error = |Chave NF-e não informada!|.
        RETURN.
      ENDIF.

      DATA(_count_duplicidade) = 0.
      LOOP AT i_zmmt0203_aux INTO DATA(lwa_zmmt0203_aux) WHERE chave_nfe = <fs_nota>-chave_nfe.
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

*------------------------------------------------------------------------------------------------------------*
*  Validações Dados Frete Carga
*------------------------------------------------------------------------------------------------------------*



*------------------------------------------------------------------------------------------------------------*
*  Validações Cotação Frete Carga
*------------------------------------------------------------------------------------------------------------*
    LOOP AT i_zsdt0346 ASSIGNING FIELD-SYMBOL(<fs_dados_cotacao>).

    ENDLOOP.


  ENDMETHOD.


  METHOD vincular_conferencia.

    SELECT SINGLE *
      FROM zmmt0218
      INTO @DATA(ls_0218)
      WHERE id_chegada_cd_luft = @i_bordero-id
        AND id_item_chegada    = @i_bordero-id_item_chegada
        AND nro_cg             = @i_nro_carga
        AND item_carga         = @i_carga-item_carga
        AND chave_nfe          = @i_notas-chave_nfe
        AND prod_item          = @i_notas-item_nfe
        AND cancel             = @space.
    IF sy-subrc IS INITIAL.
      e_msg_erro = 'Já existe conferência criada para esta mesma chave'.
      RETURN.
    ENDIF.

    IF i_carga IS INITIAL OR i_notas IS INITIAL.
      e_msg_erro = 'Os dados de carga e notas são obrigatórios'.
      RETURN.
    ENDIF.

    IF i_bordero-numero_nfe IS NOT INITIAL AND i_bordero-numero_nfe <> i_notas-numero_nfe.
      e_msg_erro = 'Não é possível vincular, NFe do borderô diferente da NFe da nota fiscal '.
      RETURN.
    ENDIF.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
    SELECT SINGLE *
      FROM zmmt0202 INTO @DATA(lwa_item_carga_vinc)
     WHERE nro_cg     EQ @i_nro_carga
       AND item_carga EQ @i_carga-item_carga.

    IF sy-subrc NE 0.
      e_msg_erro = 'Item carga não encontrado!'.
      RETURN.
    ENDIF.

    IF i_notas-chave_nfe IS NOT INITIAL AND i_notas-item_nfe IS NOT INITIAL.
      SELECT *
        FROM zmmt0218 INTO TABLE @DATA(lit_zmmt0218_vinc_nfe)
       WHERE nro_cg               = @i_nro_carga
         AND chave_nfe            = @i_notas-chave_nfe
         AND prod_item            = @i_notas-item_nfe
         AND cancel               = @space.

      LOOP AT lit_zmmt0218_vinc_nfe ASSIGNING FIELD-SYMBOL(<fs_ck_zmmt0218_vinc_nfe>).

        SELECT SINGLE *
          FROM zmmt0202 INTO @DATA(lwa_item_carga_check)
         WHERE nro_cg     EQ @<fs_ck_zmmt0218_vinc_nfe>-nro_cg
           AND item_carga EQ @<fs_ck_zmmt0218_vinc_nfe>-item_carga.

        IF sy-subrc NE 0.
          e_msg_erro = 'Item carga não encontrado!'.
          RETURN.
        ENDIF.

        IF NOT ( lwa_item_carga_check-ebeln = lwa_item_carga_vinc-ebeln AND
                 lwa_item_carga_check-ebelp = lwa_item_carga_vinc-ebelp ).
          e_msg_erro = |Chave { i_notas-chave_nfe } Item { i_notas-item_nfe } já vinculado com um Pedido/Item diferente! Operação não permitida|.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--

    GET TIME STAMP FIELD DATA(lv_timestamp).

    ls_0218-id_conferencia     = lv_timestamp.
    ls_0218-nro_cg             = i_nro_carga.
    ls_0218-item_carga         = i_carga-item_carga.
    ls_0218-id_chegada_cd_luft = i_bordero-id.
    ls_0218-id_item_chegada    = i_bordero-id_item_chegada.
    ls_0218-chave_nfe          = i_notas-chave_nfe.
    ls_0218-prod_item          = i_notas-item_nfe.
    ls_0218-peso_conv_nfe      = i_notas-peso_conv.
    ls_0218-user_create        = sy-uname.
    ls_0218-date_create        = sy-datum.
    ls_0218-time_create        = sy-uzeit.

    MODIFY zmmt0218 FROM ls_0218.
    IF sy-subrc IS INITIAL.
      e_zmmt0218 = ls_0218.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD aceite_bordero.

    DATA: lva_lifnr_filial TYPE lfa1-lifnr.

    DATA: lo_aceite_receb TYPE REF TO zcl_int_ob_aceite_receb_luft.

    DATA: lwa_dados_aceite  TYPE  zsde_integra_luft_aceite_receb.

    CLEAR: e_msg_erro.

    IF zcl_util_sd=>ck_integration_luft_active( i_direcao = 'E' ) EQ abap_false.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_aceite_receb.

    SELECT SINGLE *
      FROM zib_nfe_dist_ter INTO @DATA(lwa_zib_nfe_dist_ter)
     WHERE chave_nfe = @i_dados-chave_nfe.

    IF sy-subrc ne 0.
      e_msg_erro = |Chave NF-e { i_dados-chave_nfe } não encontrada na ZMM0110 |.
      RETURN.
    ENDIF.

    lva_lifnr_filial = lwa_zib_nfe_dist_ter-branch.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = lva_lifnr_filial
      IMPORTING
        OUTPUT        = lva_lifnr_filial.

    select SINGLE *
      from zsdt0419 INTO @DATA(lwa_zsdt0419)
     WHERE lifnr eq @lva_lifnr_filial.

    CHECK sy-subrc eq 0.

    select SINGLE *
      from lfa1 INTO @DATA(lwa_lfa1)
     WHERE lifnr eq @lva_lifnr_filial.

    lwa_dados_aceite-cnpj_depositante = lwa_lfa1-stcd1.
    APPEND INITIAL LINE TO lwa_dados_aceite-pedido ASSIGNING FIELD-SYMBOL(<fs_pedido>).
    <fs_pedido>-chave_acesso_nota_fiscal = I_DADOS-chave_nfe.

    TRY .
        CALL METHOD lo_aceite_receb->zif_integracao_outbound~execute_request
          EXPORTING
            i_info_request  = lwa_dados_aceite
          IMPORTING
            e_id_integracao = DATA(lv_id_integracao).
      CATCH zcx_integracao INTO DATA(lo_integracao).
        e_msg_erro = lo_integracao->get_text( ).

      CATCH zcx_error INTO DATA(lo_error).
        e_msg_erro = lo_error->get_text( ).
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
