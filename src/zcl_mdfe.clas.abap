CLASS zcl_mdfe DEFINITION
  PUBLIC
  CREATE PUBLIC .

*"* public components of class ZCL_MDFE
*"* do not include other source files here!!!
  PUBLIC SECTION.

    DATA:
      BEGIN OF ty_doc_mdfe ,
        docnum   TYPE j_1bdocnum,
        vl_chave TYPE c LENGTH 44,
        credat   TYPE j_1bnfdoc-credat,
        cretim   TYPE j_1bnfdoc-cretim,
        branch   TYPE j_1bnfdoc-branch,
      END OF ty_doc_mdfe .
    DATA:
      at_it_doc_mdfe LIKE TABLE OF ty_doc_mdfe .
    DATA:
      at_wa_doc_mdfe LIKE LINE OF at_it_doc_mdfe .
    DATA at_cnpj_emi TYPE char16 .
    DATA at_ie_emi TYPE char18 .
    DATA:
      at_modal(02) TYPE c .
    DATA at_ufini TYPE zde_uf_inicio .
    DATA at_cmunini TYPE zmunic_ibge .
    DATA at_nmunini TYPE zde_nm_mun_origem .
    DATA at_uffim TYPE zde_uf_final .
    DATA at_cmunfim TYPE zmunic_ibge .
    DATA at_nmunfim TYPE zde_nm_mun_final .
    DATA at_vcarga TYPE zxnfe_vcarga .
    DATA at_qcarga TYPE brgew .
    DATA at_motorista TYPE lifnr .
    DATA at_placa_car3 TYPE zplaca .
    DATA at_placa_car2 TYPE zplaca .
    DATA at_placa_car1 TYPE zplaca .
    DATA at_placa_cav TYPE zplaca .
    DATA at_cunid TYPE zde_uni_carga_mdfe .
    DATA:
      at_cunid_sap(02) TYPE c .
    DATA at_it_uf_perc TYPE zde_uf_percuro_t .
    DATA at_wa_uf_perc TYPE zde_uf_percuro .

    METHODS constructor
      IMPORTING
        !i_nmdfe  TYPE j_1bnfnum9 OPTIONAL
        !i_docnum TYPE j_1bdocnum OPTIONAL
          PREFERRED PARAMETER i_nmdfe .
    METHODS get_nmdf
      RETURNING
        VALUE(nmdf) TYPE j_1bnfnum9
      RAISING
        zcx_mdfe .
    METHODS get_serie
      RETURNING
        VALUE(serie) TYPE j_1bseries
      RAISING
        zcx_mdfe .
    METHODS set_nmdf
      IMPORTING
        !nmdf TYPE j_1bnfnum9
      RAISING
        zcx_mdfe .
    METHODS set_serie
      IMPORTING
        !serie TYPE j_1bseries OPTIONAL
      RAISING
        zcx_mdfe .
    METHODS get_data_emi
      RETURNING
        VALUE(e_data) TYPE j_1bdocdat .
    METHODS set_data_emi
      IMPORTING
        !data TYPE j_1bdocdat
      RAISING
        zcx_mdfe .
    METHODS gravar_mdfe
      IMPORTING
        !i_zsdt0237          TYPE zsdt0237 OPTIONAL
        !i_zsdt0241_t        TYPE zsdt0241_t OPTIONAL
        !i_faturamento_autom TYPE char01 OPTIONAL
        !i_ch_referencia     TYPE zch_ref OPTIONAL
      EXPORTING
        !e_docnum_sol_enc    TYPE j_1bdocnum
      RETURNING
        VALUE(e_docnum)      TYPE j_1bdocnum
      EXCEPTIONS
        erro_gravar .
    METHODS add_uf_perc
      IMPORTING
        !i_default TYPE char01 OPTIONAL
        !uf        TYPE char2 .
    METHODS set_validar_uf
      IMPORTING
        !i_uf         TYPE char2
      RETURNING
        VALUE(e_erro) TYPE char01 .
    METHODS add_documento
      IMPORTING
        !i_docnum TYPE j_1bdocnum .
    METHODS enviar_mdfe
      IMPORTING
        !i_sem_confirmacao      TYPE char01 DEFAULT ' '
        !i_aguardar             TYPE char01 DEFAULT ' '
        !i_ciclos               TYPE zde_qtd_ciclos DEFAULT 0
        !i_segundos             TYPE zde_qtd_segundos_ciclo DEFAULT 0
        !i_raise                TYPE char01 DEFAULT abap_false
        !i_faturamento_autom    TYPE char01 OPTIONAL
        !i_ch_referencia        TYPE zch_ref OPTIONAL
      RETURNING
        VALUE(e_docnum_sol_enc) TYPE j_1bdocnum
      RAISING
        zcx_averbacao_seguro
        zcx_doc_eletronico .
    METHODS cancelar_mdfe
      IMPORTING
        !i_sem_confirmacao      TYPE char01 DEFAULT ' '
        !i_justificativa_cancel TYPE string OPTIONAL .
    METHODS encerrar_mdfe
      IMPORTING
        !i_no_msg_confirm    TYPE char01 OPTIONAL
        !i_enc_aut           TYPE char01 OPTIONAL
        !i_doc_mdfe_sol      TYPE j_1bdocnum OPTIONAL
        !i_solicita_motivo   TYPE char01 OPTIONAL
        !i_faturamento_autom TYPE char01 OPTIONAL
        !i_ch_referencia     TYPE zch_ref OPTIONAL
      RETURNING
        VALUE(r_error)       TYPE char01 .
    METHODS set_docnum
      IMPORTING
        !i_docnum TYPE j_1bdocnum .
    METHODS get_docnum
      RETURNING
        VALUE(e_docnum) TYPE j_1bdocnum .
    METHODS get_hora_emi
      RETURNING
        VALUE(e_hora) TYPE j_1bcretim .
    METHODS set_hora_emi
      IMPORTING
        !hora TYPE j_1bcretim .
    METHODS set_just_canc
      IMPORTING
        !i_just TYPE string .
    CLASS-METHODS get_placas_mdfe_by_docnum
      IMPORTING
        !i_docnum       TYPE j_1bdocnum
      RETURNING
        VALUE(r_placas) TYPE zlest0002_t .
    METHODS get_just_canc
      RETURNING
        VALUE(e_just) TYPE char255 .
    METHODS estornar_mdfe .
    METHODS print_mdfe
      IMPORTING
        !i_imprimir TYPE char01 DEFAULT 'X'
      EXPORTING
        !e_url      TYPE string .
    METHODS set_dados_mdfe
      IMPORTING
        !i_docnum TYPE j_1bdocnum .
    METHODS set_mun_enc
      IMPORTING
        !i_mun_enc TYPE char10 .
    METHODS set_tp_doc_ref
      IMPORTING
        !i_tp_doc_ref TYPE zdoc_ref_mdfe .
    METHODS get_tp_doc_ref
      RETURNING
        VALUE(e_tp_doc_ref) TYPE zdoc_ref_mdfe .
    METHODS set_placa_cav
      IMPORTING
        !i_placa_cav TYPE zplaca .
    METHODS set_placa_car1
      IMPORTING
        !i_placa_car1 TYPE zplaca .
    METHODS set_placa_car2
      IMPORTING
        !i_placa_car2 TYPE zplaca .
    METHODS set_placa_car3
      IMPORTING
        !i_placa_car3 TYPE zplaca .
    METHODS get_placa_car3
      RETURNING
        VALUE(e_placa_car3) TYPE zplaca .
    METHODS get_placa_car2
      RETURNING
        VALUE(e_placa_car2) TYPE zplaca .
    METHODS get_placa_car1
      RETURNING
        VALUE(e_placa_car1) TYPE zplaca .
    METHODS get_placa_cav
      RETURNING
        VALUE(e_placa_cav) TYPE zplaca .
    METHODS set_motorista
      IMPORTING
        !i_motorista TYPE lifnr .
    METHODS get_motorista
      RETURNING
        VALUE(e_motorista) TYPE lifnr .
    METHODS set_cunid
      IMPORTING
        !i_cunid TYPE char2 .
    METHODS get_cunid
      RETURNING
        VALUE(e_cunid) TYPE char2 .
    METHODS set_vcarga
      IMPORTING
        !i_vcarga TYPE zxnfe_vcarga .
    METHODS set_qcarga
      IMPORTING
        !i_qcarga TYPE brgew .
    METHODS get_qcarga
      RETURNING
        VALUE(e_qcarga) TYPE brgew .
    METHODS get_ck_autorizado
      RETURNING
        VALUE(r_autorizado) TYPE char01 .
    CLASS-METHODS get_ufs_percurso
      IMPORTING
        !i_id_local_coleta   TYPE zde_id_local_coleta
        !i_id_local_descarga TYPE zde_id_local_descarga
      RETURNING
        VALUE(r_ufs)         TYPE zde_zsdt0104_t .
    METHODS set_parceiros
      IMPORTING
        !i_docnum TYPE j_1bdocnum .
    CLASS-METHODS get_parceiro
      IMPORTING
        !i_docnum            TYPE j_1bdocnum
      EXPORTING
        !e_id_local_coleta   TYPE zde_id_local_coleta
        !e_id_local_descarga TYPE zde_id_local_descarga .
    METHODS valida_cancelamento_troca_nota
      RETURNING
        VALUE(e_erro) TYPE char1 .
    CLASS-METHODS check_mesmo_conjunto_veicular
      IMPORTING
        !i_conjunto_veicular_1    TYPE zlest0002_t
        !i_conjunto_veicular_2    TYPE zlest0002_t
      RETURNING
        VALUE(r_conjuntos_iguais) TYPE char01 .
    CLASS-METHODS notifica_encerramento
      IMPORTING
        !i_docnum               TYPE j_1bdocnum
        !i_avaliar_encerramento TYPE char01 OPTIONAL
        !i_docnum_atual         TYPE j_1bdocnum OPTIONAL .
    CLASS-METHODS get_email_avaliacao_enc
      IMPORTING
        !i_bukrs       TYPE bukrs
      RETURNING
        VALUE(r_email) TYPE zemail .
    METHODS check_mesmos_docs_vinc
      IMPORTING
        !i_docnum                 TYPE zsdt0102-docnum
      RETURNING
        VALUE(r_mesmos_docs_vinc) TYPE char01 .
    METHODS check_contingencia_mdfe
      IMPORTING
        !i_branch      TYPE j_1bbranc_
      RETURNING
        VALUE(r_ativo) TYPE char01 .
    METHODS set_gravar_uf_perc
      IMPORTING
        !i_docnum TYPE j_1bdocnum
      EXCEPTIONS
        erro_gravar .
    METHODS check_conting_mdfe_reenvio
      IMPORTING
        !i_docnum      TYPE j_1bdocnum
      RETURNING
        VALUE(r_ativo) TYPE char01 .
  PROTECTED SECTION.
*"* protected components of class ZMDFE
*"* do not include other source files here!!!
  PRIVATE SECTION.

*"* private components of class ZCL_MDFE
*"* do not include other source files here!!!
    TYPES ty_nota_mdfe TYPE zob_nota_fiscal_sap .

    DATA:
      BEGIN OF wa_nota,
        nu_documento_sap TYPE j_1bdocnum,
        id_empresa       TYPE   bukrs,
        id_filial        TYPE j_1bbranc_,
        tb_direcao       TYPE  j_1bdirect,
        tp_authcod       TYPE c,
        nr_nfe           TYPE j_1bnfnum9,
        sr_nfe           TYPE j_1bseries,
        data_hora        TYPE zdata_hora,
        id_destinatario  TYPE j_1bparid,
        ds_email(100)    TYPE c,
        tx_xml           TYPE zxml,
        tx_xml2          TYPE zxml4000,
        tx_xml3          TYPE zxml4000,
      END OF wa_nota .
    DATA:
      it_nota LIKE STANDARD TABLE OF wa_nota .
    DATA at_nmdf TYPE j_1bnfnum9 .
    DATA at_serie TYPE j_1bseries .
    DATA at_dhemi TYPE oiu_ent_timestmp .
    DATA at_data_emi TYPE j_1bdocdat .
    DATA at_hora_emi TYPE j_1bcretim .
    DATA at_docnum TYPE j_1bdocnum .
    DATA at_just_canc TYPE char255 .
    DATA at_mun_enc TYPE numc10 .
    DATA at_tp_doc_ref TYPE zdoc_ref_mdfe .
    DATA at_autorizado TYPE char01 .
    DATA at_id_local_coleta TYPE zde_id_local_coleta .
    DATA at_id_local_descarga TYPE zde_id_local_descarga .
    DATA at_prodpred TYPE zde_mdfe_prodpred .

    METHODS verifica_existe
      IMPORTING
        !i_nmdf        TYPE j_1bnfnum9
      RETURNING
        VALUE(e_rnmdf) TYPE j_1bnfnum9 .
    METHODS monta_xml
      IMPORTING
        !tipo        TYPE char01
      RETURNING
        VALUE(e_xml) TYPE string .
    METHODS get_chave_mdfe
      RETURNING
        VALUE(e_chave) TYPE string .
*---> 20.06.2023 - Migração S4 - DG
    "      !I_MODAL type CHAR02 .
    METHODS set_modal
      IMPORTING
        !i_modal TYPE zchar02 .
*<--- 20.06.2023 - Migração S4 - DG
*---> 20.06.2023 - Migração S4 - DG
    "      value(E_MODAL) type CHAR02 .
    METHODS get_modal
      RETURNING
        VALUE(e_modal) TYPE zchar02 .
*<--- 20.06.2023 - Migração S4 - DG
    METHODS get_cnpj_emi
      IMPORTING
        !i_docnum         TYPE j_1bdocnum
      RETURNING
        VALUE(e_cnpj_ide) TYPE string .
    METHODS get_ie_emi
      IMPORTING
        !i_docnum       TYPE j_1bdocnum
      RETURNING
        VALUE(e_ie_ide) TYPE string .
    METHODS get_cod_um
      RETURNING
        VALUE(e_cod_um) TYPE string .
    METHODS get_mun_enc
      IMPORTING
        !i_docnum        TYPE j_1bdocnum
      RETURNING
        VALUE(e_mun_enc) TYPE string .
    METHODS get_modal_doc
      IMPORTING
        !i_docnum      TYPE j_1bdocnum
      RETURNING
        VALUE(e_modal) TYPE string .
    METHODS set_ie_emi
      IMPORTING
        !i_ie_emi TYPE char18 .
    METHODS set_cnpj_emi
      IMPORTING
        !i_cnpj_emi TYPE char16 .
    METHODS set_id_local_coleta
      IMPORTING
        !i_id_local_coleta TYPE zde_id_local_coleta .
    METHODS set_id_local_descarga
      IMPORTING
        !i_id_local_descarga TYPE zde_id_local_descarga .
    METHODS get_parceiros_documento
      IMPORTING
        !i_docnum              TYPE j_1bdocnum
      RETURNING
        VALUE(r_j_1bnfnad_tab) TYPE j_1bnfnad_tab .
    METHODS get_prodpred_doc_vinculados
      IMPORTING
        !i_zsdt0237                TYPE zsdt0237 OPTIONAL
      RETURNING
        VALUE(r_prod_produminante) TYPE zde_mdfe_prodpred
      RAISING
        zcx_mdfe .
    METHODS get_mdfes_encerramento .
    METHODS get_placas_mdfe
      RETURNING
        VALUE(r_placas) TYPE zlest0002_t .
    METHODS get_mdfe_nao_encerrados
      IMPORTING
        !i_placas      TYPE zlest0002_t
      RETURNING
        VALUE(r_mdfes) TYPE zsdt0102_t .
    METHODS check_encerramento_mdfes
      IMPORTING
        VALUE(i_sem_confirmacao) TYPE char01 OPTIONAL
        !i_faturamento_autom     TYPE char01 OPTIONAL
        !i_ch_referencia         TYPE zch_ref OPTIONAL
      EXPORTING
        !e_docnum_sol_enc        TYPE j_1bdocnum
      RETURNING
        VALUE(r_continue)        TYPE char01 .
    METHODS registra_email_notificacao_enc
      IMPORTING
        !i_docnum TYPE j_1bdocnum
        !i_email  TYPE ad_smtpadr .
    METHODS get_motivo_encerramento
      RETURNING
        VALUE(r_motivo) TYPE zsdt0102-motivo_enc .
ENDCLASS.



CLASS ZCL_MDFE IMPLEMENTATION.


  METHOD add_documento.

    DATA: wa_act_nota  TYPE j_1bnfe_active.
    DATA: wa_doc_nota  TYPE j_1bnfdoc.
    DATA: vl_chave TYPE c LENGTH 44.

    CLEAR: wa_act_nota, wa_doc_nota.

    SELECT SINGLE *
      INTO wa_act_nota
      FROM j_1bnfe_active
     WHERE docnum EQ i_docnum.

    SELECT SINGLE *
       INTO wa_doc_nota
       FROM j_1bnfdoc
      WHERE docnum EQ i_docnum.

    CONCATENATE wa_act_nota-regio
                wa_act_nota-nfyear
                wa_act_nota-nfmonth
                wa_act_nota-stcd1
                wa_act_nota-model
                wa_act_nota-serie
                wa_act_nota-nfnum9
                wa_act_nota-docnum9
                wa_act_nota-cdv INTO vl_chave.

    me->at_wa_doc_mdfe-vl_chave = vl_chave.
    me->at_wa_doc_mdfe-docnum   = i_docnum.

    me->at_wa_doc_mdfe-credat   = wa_doc_nota-credat.
    me->at_wa_doc_mdfe-cretim   = wa_doc_nota-cretim.
    me->at_wa_doc_mdfe-branch   = wa_doc_nota-branch.

    APPEND me->at_wa_doc_mdfe TO at_it_doc_mdfe.

    "CHECK AT_IT_UF_PERC IS INITIAL.

    SELECT SINGLE * INTO @DATA(wa_zlest0061)
      FROM zlest0061
     WHERE docnum EQ @i_docnum.

    CASE sy-subrc.
      WHEN 0. "Aquaviario

        SELECT * INTO TABLE @DATA(it_zlest0060)
          FROM zlest0060
         WHERE docnum EQ @i_docnum.

        CHECK sy-subrc IS INITIAL.

        me->at_cunid     = '01'.
        me->at_cunid_sap = 'KG'.

        LOOP AT it_zlest0060 INTO DATA(wa_zlest0060).
          IF NOT wa_zlest0060-peso_liq_ret IS INITIAL.      "<<RIM-SKM-IR121826-29.12.22
            ADD wa_zlest0060-peso_liq_ret TO me->at_qcarga.
          ELSE.                                             "<<RIM-SKM-IR121826-29.12.22
            ADD wa_zlest0060-peso_fiscal TO me->at_qcarga.  "<<RIM-SKM-IR121826-29.12.22
          ENDIF.                                            "<<RIM-SKM-IR121826-29.12.22
          ADD wa_zlest0060-netwr TO me->at_vcarga.
        ENDLOOP.

      WHEN OTHERS.

        SELECT SINGLE * INTO @DATA(wa_zcte_identifica)
          FROM zcte_identifica
         WHERE docnum EQ @i_docnum.

        CHECK sy-subrc IS INITIAL AND wa_zcte_identifica-tknum IS NOT INITIAL.

        SELECT * INTO TABLE @DATA(it_notas)
          FROM zcte_info_nota
         WHERE docnum EQ @i_docnum.

        me->at_ufini     = wa_zcte_identifica-ufini.
        me->at_cmunini   = wa_zcte_identifica-cmunini.
        me->at_nmunini   = wa_zcte_identifica-nmunini.
        me->at_uffim     = wa_zcte_identifica-uffim.
        me->at_cmunfim   = wa_zcte_identifica-cmunfim.
        me->at_nmunfim   = wa_zcte_identifica-nmunfim.
        "ME->AT_QCARGA    = 0.
        "ME->AT_VCARGA    = 0.
        "ME->AT_CUNID     = ''.
        "ME->AT_CUNID_SAP = ''.

        LOOP AT it_notas INTO DATA(wa_nota).

          ADD wa_nota-quantidade TO me->at_qcarga.
          ADD wa_nota-vl_nota_fiscal TO me->at_vcarga.
          me->at_cunid_sap = wa_nota-unidade.

          CASE me->at_cunid_sap.
            WHEN 'KG'.
              me->at_cunid = '01'.
            WHEN 'TO'.
              me->at_cunid = '02'.
            WHEN 'UN'.
              me->at_cunid = '03'.
            WHEN 'LT'.
              me->at_cunid = '04'.
          ENDCASE.

        ENDLOOP.

        SELECT SINGLE * INTO @DATA(wa_zcte_motorista)
          FROM zcte_motorista
         WHERE docnum EQ @i_docnum.

        me->at_motorista = wa_zcte_motorista-lifnr.

        SELECT * INTO TABLE @DATA(it_zcte_trans)
          FROM zcte_trans
         WHERE docnum EQ @i_docnum.

        LOOP AT it_zcte_trans INTO DATA(wa_zcte_trans) WHERE tp_veiculo EQ '0'.
          me->at_placa_cav = wa_zcte_trans-pc_veiculo.
        ENDLOOP.

*-IR 138188-17.04.2024-JT-inicio
        DATA(l_line) = 0.

        LOOP AT it_zcte_trans INTO wa_zcte_trans WHERE tp_veiculo EQ '1'.
*         CASE sy-tabix.
          l_line = l_line + 1.

          CASE l_line.
            WHEN 1.
              me->at_placa_car1 = wa_zcte_trans-pc_veiculo.
            WHEN 2.
              me->at_placa_car2 = wa_zcte_trans-pc_veiculo.
            WHEN 3.
              me->at_placa_car3 = wa_zcte_trans-pc_veiculo.
          ENDCASE.
        ENDLOOP.
*-IR 138188-17.04.2024-JT-fim

        SELECT SINGLE * INTO @DATA(wa_vttk)
          FROM vttk
         WHERE tknum EQ @wa_zcte_identifica-tknum.

        CHECK sy-subrc IS INITIAL AND wa_vttk-id_carga IS NOT INITIAL.

        SELECT SINGLE * INTO @DATA(wa_zsdt0001ft)
          FROM zsdt0001ft
         WHERE id_carga EQ @wa_vttk-id_carga.

        CHECK sy-subrc IS INITIAL AND wa_zsdt0001ft-id_carga_model IS NOT INITIAL.

        SELECT * INTO TABLE @DATA(it_zsdt0001ftufs)
          FROM zsdt0001ftufs
         WHERE id_carga EQ @wa_zsdt0001ft-id_carga_model.

        CHECK sy-subrc IS INITIAL.

        SORT it_zsdt0001ftufs BY nm_sequencia ASCENDING.

        LOOP AT it_zsdt0001ftufs INTO DATA(wa_zsdt0001ftufs).
          me->add_uf_perc( EXPORTING uf = wa_zsdt0001ftufs-bland(2) ).
        ENDLOOP.

    ENDCASE.


  ENDMETHOD.


  METHOD add_uf_perc.

    IF i_default EQ abap_true.
      IF me->at_id_local_coleta IS NOT INITIAL AND me->at_id_local_descarga IS NOT INITIAL.
        DATA(it_ufs) = me->get_ufs_percurso(
                       i_id_local_coleta   = me->at_id_local_coleta
                       i_id_local_descarga = me->at_id_local_descarga ).

        SORT it_ufs BY ordem_uf ASCENDING.

        LOOP AT it_ufs INTO DATA(wa_ufs).
          me->add_uf_perc( EXPORTING uf = wa_ufs-uf ).
        ENDLOOP.
      ENDIF.
    ELSE.
      me->at_wa_uf_perc-uf = uf.
      APPEND me->at_wa_uf_perc TO at_it_uf_perc.
    ENDIF.

  ENDMETHOD.


  METHOD cancelar_mdfe.

    "Internal Tables and Work Areas
    DATA: wa_zsdt0102  TYPE zsdt0102,
          wa_j_1bnfdoc TYPE j_1bnfdoc.

    "Variables
    DATA: xdhemi        TYPE c LENGTH 30,
          xdata         TYPE c LENGTH 30,
          xhora         TYPE c LENGTH 10,
          xml_ret       TYPE string,
          xml           TYPE zxml,
          var_answer    TYPE c,
          vl_length     TYPE i,
          vl_msg_exibir TYPE string,
          vl_erro       TYPE char1.

    REFRESH: it_nota.
    CLEAR: wa_nota, wa_j_1bnfdoc, wa_zsdt0102, xdhemi, xdata, xhora, xml_ret, xml, var_answer, vl_erro.

    IF i_sem_confirmacao EQ abap_false.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Confirma solicitação de cancelamento do MDF-e?'
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
        RETURN.
      ENDIF.
    ENDIF.

    SELECT SINGLE *
       FROM zsdt0102
       INTO wa_zsdt0102
      WHERE nmdfe  = me->at_nmdf
        AND docnum = me->at_docnum.

    IF sy-subrc IS INITIAL.

      IF NOT ( wa_zsdt0102-estornado IS INITIAL ).
        ROLLBACK WORK.
        MESSAGE w022.
        "MESSAGE 'MDF-e já estornado! Operação não permitida!' TYPE 'W'.
        RETURN.
      ENDIF.

      IF NOT ( wa_zsdt0102-encerrado IS INITIAL ).
        ROLLBACK WORK.
        MESSAGE w023.
        "MESSAGE 'MDF-e já encerrado! Operação não permitida!' TYPE 'W'.
        RETURN.
      ENDIF.

      IF NOT ( wa_zsdt0102-cancel IS INITIAL ).
        ROLLBACK WORK.
        MESSAGE w024.
        "MESSAGE 'MDF-e já cancelado! Operação não permitida!' TYPE 'W'.
        RETURN.
      ENDIF.

*----CS2021000508 - 07.06.2021 - JT - inicio
*-------------------------
*-- cancela TRoca nota, limpar docmntos no carguero
*-------------------------
      vl_erro = valida_cancelamento_troca_nota( ).

      IF vl_erro = abap_true.
        ROLLBACK WORK.
        MESSAGE w053.
        RETURN.
      ENDIF.
*----CS2021000508 - 07.06.2021 - JT - fim

      IF ( wa_zsdt0102-autorizado IS INITIAL ).
        ROLLBACK WORK.
        "MESSAGE 'MDF-e não autorizado! Operação não permitida!' TYPE 'W'.
        MESSAGE w025.
        RETURN.
      ENDIF.

    ELSE.
      ROLLBACK WORK.
      "MESSAGE 'Não foi possível encerrar o MDF-e' TYPE 'E'.
      MESSAGE w029.
      RETURN.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_zsdt0237)
      FROM zsdt0237
     WHERE docnum EQ @me->at_docnum.

    IF sy-subrc IS NOT INITIAL.

      CLEAR: at_wa_doc_mdfe.
      READ TABLE at_it_doc_mdfe INTO at_wa_doc_mdfe INDEX 1.

      SELECT SINGLE *
        INTO wa_j_1bnfdoc
        FROM j_1bnfdoc
       WHERE docnum = at_wa_doc_mdfe-docnum.

      IF sy-subrc <> 0.
        CLEAR: vl_msg_exibir.
        "CONCATENATE 'Não encontrado os dados do Documento:' AT_WA_DOC_MDFE-DOCNUM '.(Gravar Doc. MDF-e)!'
        "       INTO VL_MSG_EXIBIR SEPARATED BY SPACE.

        ROLLBACK WORK.
        "MESSAGE VL_MSG_EXIBIR TYPE 'W' .
        MESSAGE w011 WITH at_wa_doc_mdfe-docnum.
        RETURN.
      ENDIF.

    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_mdfe)
      FROM j_1bnfdoc
     WHERE docnum EQ @wa_zsdt0102-docnum
       AND model  EQ @zif_doc_eletronico=>at_st_model_mdfe.

    IF sy-subrc IS INITIAL.

      TRY .
          me->get_just_canc( RECEIVING e_just = wa_zsdt0102-just_canc ).

          DATA(lc_mdfe) = zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_mdfe-docnum
            ).

          lc_mdfe->set_registro( EXPORTING i_docnum = wa_mdfe-docnum
            ).

          lc_mdfe->set_cancelar( EXPORTING i_ds_motivo = CONV #( wa_zsdt0102-just_canc ) ).
          CLEAR: lc_mdfe.

          COMMIT WORK.
          MESSAGE s028.

        CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).

          lc_mdfe->set_liberar_registro( ).
          CLEAR: lc_mdfe.

          ex_doc_eletronico->published_erro( ).
      ENDTRY.

    ELSE.

      "Grava Justificativa Cancelamento.
      IF i_justificativa_cancel IS INITIAL.
        me->get_just_canc( RECEIVING e_just = wa_zsdt0102-just_canc ).
      ELSE.
        wa_zsdt0102-just_canc = i_justificativa_cancel.
        me->set_just_canc( i_just = i_justificativa_cancel ).
      ENDIF.

      vl_length = strlen( wa_zsdt0102-just_canc ).
      IF vl_length < 15 OR vl_length > 255.
        ROLLBACK WORK.
        MESSAGE e030.
      ENDIF.

      REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN wa_zsdt0102-just_canc WITH 'a' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN wa_zsdt0102-just_canc WITH 'e' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF        'í'     IN wa_zsdt0102-just_canc WITH 'i' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN wa_zsdt0102-just_canc WITH 'o' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN wa_zsdt0102-just_canc WITH 'u' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN wa_zsdt0102-just_canc WITH 'c' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF        '&'     IN wa_zsdt0102-just_canc WITH '&#38;'.
      REPLACE ALL OCCURRENCES OF        ''''    IN wa_zsdt0102-just_canc WITH '&#39;'.
      REPLACE ALL OCCURRENCES OF        'º'     IN wa_zsdt0102-just_canc WITH 'o' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF        'ª'     IN wa_zsdt0102-just_canc WITH 'a' IGNORING CASE.
      wa_zsdt0102-transmissao = '2'. "Cancelamento
      MODIFY zsdt0102 FROM wa_zsdt0102.

      IF sy-subrc IS NOT INITIAL.
        ROLLBACK WORK.
        MESSAGE e031.
        RETURN.
      ENDIF.

      me->monta_xml( EXPORTING tipo  = '2' RECEIVING e_xml = xml_ret ).
      IF ( xml_ret IS INITIAL ).
        ROLLBACK WORK.
        RETURN.
      ENDIF.

      xml = xml_ret.
      REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN xml WITH 'a' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN xml WITH 'e' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF        'í'     IN xml WITH 'i' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN xml WITH 'o' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN xml WITH 'u' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN xml WITH 'c' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF        '&'     IN xml WITH '&#38;'.
      REPLACE ALL OCCURRENCES OF        ''''    IN xml WITH '&#39;'.
      REPLACE ALL OCCURRENCES OF        'º'     IN xml WITH 'o' IGNORING CASE.
      CONCATENATE me->at_hora_emi(2) ':' me->at_hora_emi+2(2) ':' me->at_hora_emi+4(2) INTO xhora.
      CONCATENATE me->at_data_emi(4) '-' me->at_data_emi+4(2) '-' me->at_data_emi+6(2) INTO xdata.
      CONCATENATE xdata xhora INTO xdhemi SEPARATED BY space.

      wa_nota-tp_authcod       = '5'.
      wa_nota-nu_documento_sap = me->at_docnum..
      wa_nota-id_empresa       = wa_j_1bnfdoc-bukrs.
      wa_nota-id_filial        = wa_j_1bnfdoc-branch.
      wa_nota-tb_direcao       = wa_j_1bnfdoc-direct.
      wa_nota-nr_nfe           = me->at_nmdf.
      wa_nota-sr_nfe           = wa_j_1bnfdoc-series.
      wa_nota-data_hora        = xdhemi.
      wa_nota-id_destinatario  = wa_j_1bnfdoc-parid.
      wa_nota-tx_xml           = xml(4000).
      wa_nota-tx_xml2          = xml+04000(4000).
      wa_nota-tx_xml3          = xml+08000(4000).

      APPEND wa_nota TO it_nota.

      SELECT SINGLE *
        FROM setleaf INTO @DATA(wl_setleaf)
       WHERE setname EQ 'MAGGI_CTG_CTE_SAP'.

      IF ( sy-subrc = 0 ) AND ( wl_setleaf-valfrom IS NOT INITIAL ).
        DATA: wl_zob_cte_sap TYPE zob_cte_sap.
        MOVE-CORRESPONDING wa_nota TO wl_zob_cte_sap.
        CALL FUNCTION 'Z_GRAVAR_XML_NFE_CTE'
          EXPORTING
            i_xml01       = xml
            i_tipo        = '2' "CT-e
          CHANGING
            i_zob_cte_sap = wl_zob_cte_sap.
      ELSE.
        CALL FUNCTION 'Z_SD_OUTBOUND_CTE_XML' IN BACKGROUND TASK
          DESTINATION 'XI_XML_CTE'
          AS SEPARATE UNIT
          TABLES
            it_saida = it_nota.
      ENDIF.

      COMMIT WORK.
      MESSAGE s028.

    ENDIF.


  ENDMETHOD.


  METHOD check_encerramento_mdfes.

    DATA: lwa_doc_mdfe_encerrar TYPE j_1bnfdoc,
          it_zsdt0105_enc       TYPE TABLE OF zsdt0105,
          wa_zsdt0105_enc       TYPE zsdt0105,
          var_answer            TYPE c,
          vl_msg_exibir         TYPE string.

    DATA: zcl_mdfe_aux TYPE REF TO zcl_mdfe.

*-#133089-21.02.2024-JT-inicio
    DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
          lc_mesg                   TYPE string.
    IF i_faturamento_autom = abap_true.
      CREATE OBJECT lc_faturamento_automatico.
    ENDIF.
*-#133089-21.02.2024-JT-fim

    r_continue = abap_true.

    CLEAR: e_docnum_sol_enc.

    CHECK ( me->at_modal = '01' )  OR ( me->at_placa_cav IS NOT INITIAL ). "Modal Rodoviario

    DATA(lit_placas_mdfe_atual) = me->get_placas_mdfe( ).

    DATA(lit_zsdt0102_encerrar) = me->get_mdfe_nao_encerrados( i_placas =  lit_placas_mdfe_atual[] ).

    DELETE lit_zsdt0102_encerrar WHERE docnum EQ me->at_docnum. "SD - Ajuste encerramento MDF-e IR219403 - WPP

*-#133089-21.02.2024-JT-inicio
    CASE i_faturamento_autom.
      WHEN abap_off.
      WHEN abap_true.
        LOOP AT lit_zsdt0102_encerrar INTO DATA(w_zsdt0102_encerrar).
          lc_mesg = 'MDF-e a Encerrar: ' && w_zsdt0102_encerrar-docnum.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).
        ENDLOOP.
    ENDCASE.
*-#133089-21.02.2024-JT-fim

    LOOP AT lit_zsdt0102_encerrar INTO DATA(lwa_zsdt0102_encerrar).

      CLEAR: lwa_doc_mdfe_encerrar.

      SELECT SINGLE *
        FROM j_1bnfdoc INTO lwa_doc_mdfe_encerrar
       WHERE docnum EQ lwa_zsdt0102_encerrar-docnum.

      DATA(lit_placas_mdfe_encerrar) = me->get_placas_mdfe_by_docnum( i_docnum = CONV #( lwa_zsdt0102_encerrar-docnum ) ).

      DATA(_mesmo_conjunto_veicular) = me->check_mesmo_conjunto_veicular( EXPORTING i_conjunto_veicular_1  = lit_placas_mdfe_atual
                                                                                    i_conjunto_veicular_2  = lit_placas_mdfe_encerrar ).

      CLEAR: it_zsdt0105_enc[].

      SELECT *
        FROM zsdt0105 INTO TABLE it_zsdt0105_enc
       WHERE docnum_ref = lwa_zsdt0102_encerrar-docnum.

      LOOP AT lit_placas_mdfe_encerrar INTO DATA(wa_placa_mdfe_encerrar).

        READ TABLE lit_placas_mdfe_atual WITH KEY pc_veiculo = wa_placa_mdfe_encerrar-pc_veiculo TRANSPORTING NO FIELDS.

        CHECK sy-subrc EQ 0.

        CLEAR: wa_zsdt0105_enc.
        READ TABLE it_zsdt0105_enc INTO wa_zsdt0105_enc INDEX 1.

        CONCATENATE lwa_doc_mdfe_encerrar-docdat+6(2)
                    lwa_doc_mdfe_encerrar-docdat+4(2)
                    lwa_doc_mdfe_encerrar-docdat(4) INTO DATA(lwa_data_emissao_mdfe) SEPARATED BY '/'.

        DATA(lva_email_aviso) = me->get_email_avaliacao_enc( i_bukrs = CONV #( lwa_doc_mdfe_encerrar-bukrs ) ).


        IF ( sy-subrc EQ 0 ) AND it_zsdt0105_enc[] IS NOT INITIAL.
          CONCATENATE 'Existe um MDF-e não encerrado para a Placa:' wa_placa_mdfe_encerrar-pc_veiculo
                     '( MDF-e número:' lwa_zsdt0102_encerrar-nmdfe 'Documento:' lwa_zsdt0102_encerrar-docnum
                     ', CTe/NF-e Documento:' wa_zsdt0105_enc-docnum
                     ', Filial:' lwa_doc_mdfe_encerrar-branch 'Data Emissão:' lwa_data_emissao_mdfe
                        ')!'
                INTO vl_msg_exibir SEPARATED BY space.
        ELSE.
          CONCATENATE 'Existe um MDF-e não encerrado para a Placa:' wa_placa_mdfe_encerrar-pc_veiculo
                     '( MDF-e número:' lwa_zsdt0102_encerrar-nmdfe 'Documento:' lwa_zsdt0102_encerrar-docnum
                     ', Filial:' lwa_doc_mdfe_encerrar-branch 'Data Emissão:' lwa_data_emissao_mdfe
                        ')!'
                INTO vl_msg_exibir SEPARATED BY space.

        ENDIF.

        IF _mesmo_conjunto_veicular EQ abap_true.
          CONCATENATE vl_msg_exibir 'Confirma solicitação de encerramento do mesmo?'
                 INTO vl_msg_exibir SEPARATED BY space.
        ELSE.
          CONCATENATE vl_msg_exibir 'Enviar e-mail para o grupo' lva_email_aviso 'para avaliação.'
                 INTO vl_msg_exibir SEPARATED BY space.

          IF i_sem_confirmacao EQ abap_false.
            MESSAGE vl_msg_exibir TYPE 'I' DISPLAY LIKE 'W'.


            IF sy-batch EQ abap_false.
              DATA(_email_user) = zcl_user=>zif_user~get_mail_user( i_user = sy-uname ).
              me->registra_email_notificacao_enc( i_docnum =   CONV #( lwa_zsdt0102_encerrar-docnum )
                                                  i_email  =   CONV #( _email_user ) ).

              "Envia Email para area responsavel avaliar/encerrar o MDF-e
              me->notifica_encerramento( EXPORTING i_docnum               = lwa_zsdt0102_encerrar-docnum
                                                   i_avaliar_encerramento = abap_true
                                                   i_docnum_atual         = CONV #( me->at_docnum ) ).
            ENDIF.

          ENDIF.

          CLEAR r_continue.
          RETURN.
        ENDIF.

        CASE i_sem_confirmacao.
          WHEN abap_true.
            var_answer = '1'.
          WHEN abap_false.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Confirmação'
                text_question         = vl_msg_exibir
                text_button_1         = 'Sim'
                text_button_2         = 'Não'
                default_button        = '1'
                display_cancel_button = ''
              IMPORTING
                answer                = var_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.
        ENDCASE.

        IF var_answer = '1'.
          CREATE OBJECT zcl_mdfe_aux
            EXPORTING
              i_nmdfe  = lwa_zsdt0102_encerrar-nmdfe
              i_docnum = lwa_zsdt0102_encerrar-docnum.

          DATA(_error) = zcl_mdfe_aux->encerrar_mdfe( i_no_msg_confirm    = 'X'
                                                      i_doc_mdfe_sol      = me->at_docnum
                                                      i_faturamento_autom = i_faturamento_autom "*-#133089-21.02.2024-JT
                                                      i_ch_referencia     = i_ch_referencia ).  "*-#133089-21.02.2024-JT
          IF _error EQ abap_false.
            e_docnum_sol_enc = lwa_zsdt0102_encerrar-docnum.
            CLEAR: zcl_mdfe_aux.
            FREE: zcl_mdfe_aux.
*-#133089-21.02.2024-JT-inicio
            CASE i_faturamento_autom.
              WHEN abap_off.
                MESSAGE s036.
              WHEN abap_true.
                MESSAGE s036 INTO lc_mesg.
                lc_mesg = lc_mesg && ':' && lwa_zsdt0102_encerrar-docnum.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
          ENDIF.

        ENDIF.

        DATA(_permitir_continuar_processo) = abap_false.
        IF ( lwa_zsdt0102_encerrar-docnum IS NOT INITIAL ).

          READ TABLE at_it_doc_mdfe INTO DATA(wl_mdfe_atual) INDEX 1.

          IF ( sy-subrc EQ 0 ) .

            SELECT SINGLE *
              FROM j_1bnfdoc INTO @DATA(wl_doc_mdfe_atual)
             WHERE docnum EQ @wl_mdfe_atual-docnum.

            "Se for MDF-e de Empresa diferente, deixar continuar a gravação do MDF-e atual.
            IF ( sy-subrc EQ 0 ) AND
               ( wl_doc_mdfe_atual-bukrs NE lwa_doc_mdfe_encerrar-bukrs ) AND
               ( wl_doc_mdfe_atual-bukrs IS NOT INITIAL ) AND ( lwa_doc_mdfe_encerrar-bukrs IS NOT INITIAL ).
              IF lwa_zsdt0102_encerrar-code EQ '100'.
                _permitir_continuar_processo = abap_true.
                RETURN.
              ENDIF.

            ENDIF.

            "Se for MDF-e de UF de descarregamento  diferente, deixar continuar a gravação do MDF-e atual.
            me->set_dados_mdfe( EXPORTING i_docnum = wl_mdfe_atual-docnum ).
            IF ( sy-subrc EQ 0 ) AND
               ( lwa_zsdt0102_encerrar-uffim NE me->at_uffim ) AND
               ( lwa_zsdt0102_encerrar-uffim IS NOT INITIAL ) AND ( me->at_uffim IS NOT INITIAL ).
              IF lwa_zsdt0102_encerrar-code EQ '100'.
                _permitir_continuar_processo = abap_true.
                RETURN.
              ENDIF.

            ENDIF.

          ENDIF.
        ENDIF.

        IF _permitir_continuar_processo EQ abap_false.
          CLEAR: r_continue.
          RETURN.
        ENDIF.

      ENDLOOP. "LOOP AT lit_placas_mdfe_encerrar INTO DATA(wa_placa_mdfe_encerrar).

    ENDLOOP. "LOOP AT lit_zsdt0102_encerrar INTO DATA(lwa_zsdt0102_encerrar).

  ENDMETHOD.


  METHOD check_mesmos_docs_vinc.

    DATA: lit_zsdt0105 TYPE TABLE OF zsdt0105.

    CLEAR: lit_zsdt0105[], r_mesmos_docs_vinc.

    DATA(lva_doc_divergente_found) = abap_false.

    SELECT *
      FROM zsdt0105 INTO TABLE lit_zsdt0105
     WHERE docnum_ref EQ i_docnum.

    CHECK lines( me->at_it_doc_mdfe[] ) EQ lines( lit_zsdt0105[] ).

    LOOP AT lit_zsdt0105 INTO DATA(wa_zsdt0105).
      READ TABLE me->at_it_doc_mdfe INTO me->at_wa_doc_mdfe WITH KEY docnum = wa_zsdt0105-docnum.
      IF sy-subrc NE 0.
        lva_doc_divergente_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lva_doc_divergente_found EQ abap_false.
      r_mesmos_docs_vinc = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_mesmo_conjunto_veicular.

    DATA: lit_conjunto_01 TYPE zlest0002_t,
          lit_conjunto_02 TYPE zlest0002_t.


    r_conjuntos_iguais = abap_true.

    IF ( i_conjunto_veicular_1[] IS INITIAL ) AND ( i_conjunto_veicular_2[] IS INITIAL ).
      EXIT.
    ENDIF.

    DATA(lva_check_conjunto_01) = abap_true.
    DATA(lva_check_conjunto_02) = abap_true.

    lit_conjunto_01[] = i_conjunto_veicular_1[].
    lit_conjunto_02[] = i_conjunto_veicular_2[].

    DELETE: lit_conjunto_01 WHERE pc_veiculo IS INITIAL,
            lit_conjunto_02 WHERE pc_veiculo IS INITIAL.

    SORT: lit_conjunto_01 BY pc_veiculo,
          lit_conjunto_02 BY pc_veiculo.

    DELETE ADJACENT DUPLICATES FROM: lit_conjunto_01 COMPARING pc_veiculo,
                                     lit_conjunto_02 COMPARING pc_veiculo.


    "Se o MDF-e anterior tem uma quantidade de placas diferente do MDF-e que
    "esta sendo emitido no momento, o sistema vai considerar que é o mesmo
    "conjunto veicular se todas as placas contidas no MDF-e que tem menos
    "placas, existem no MDF-e que tem mais placas.
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarvc)
     WHERE name  EQ 'ZENC_MDFE_EXCECAO_01'
       AND low   EQ @abap_true.

    IF ( sy-subrc EQ 0 ) AND
       ( lines( lit_conjunto_01[] ) > 0 ) AND
       ( lines( lit_conjunto_02[] ) > 0 ) AND
       ( lines( lit_conjunto_01[] ) NE lines( lit_conjunto_02[] ) ).

      IF lines( lit_conjunto_01[] ) > lines( lit_conjunto_02[] ).
        lva_check_conjunto_01 = abap_false.
      ELSEIF lines( lit_conjunto_02[] ) > lines( lit_conjunto_01[] ).
        lva_check_conjunto_02 = abap_false.
      ENDIF.

    ENDIF.

    IF lva_check_conjunto_01 EQ abap_true.
      LOOP AT lit_conjunto_01 INTO DATA(lwa_conjunto_01).
        READ TABLE lit_conjunto_02 WITH KEY pc_veiculo = lwa_conjunto_01-pc_veiculo TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          r_conjuntos_iguais = abap_false.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lva_check_conjunto_02 EQ abap_true.
      LOOP AT lit_conjunto_02 INTO DATA(lwa_conjunto_02).
        READ TABLE lit_conjunto_01 WITH KEY pc_veiculo = lwa_conjunto_02-pc_veiculo TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          r_conjuntos_iguais = abap_false.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD constructor.

    DATA: vl_just_canc TYPE string.

    REFRESH me->at_it_doc_mdfe.
    REFRESH me->at_it_uf_perc.

    "  IF NOT ( I_NMDFE IS INITIAL ).

    "Dados MDF-e
    DATA: it_zsdt0102 TYPE TABLE OF zsdt0102,
          wa_zsdt0102 TYPE zsdt0102.

    "Dados UF Percurso.
    DATA: it_zsdt0104 TYPE TABLE OF zsdt0104,
          wa_zsdt0104 TYPE zsdt0104.

    "Dados Documentos Selecionados
    DATA: it_zsdt0105 TYPE TABLE OF zsdt0105,
          wa_zsdt0105 TYPE zsdt0105.

    "Dados Manuais Transporte (NF-e)
    DATA: it_zsdt0118 TYPE TABLE OF zsdt0118,
          wa_zsdt0118 TYPE zsdt0118.

    SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
      FROM j_1bnfdoc
     WHERE docnum EQ @i_docnum.

    SELECT *
      FROM zsdt0102
      INTO TABLE it_zsdt0102
     WHERE docnum = i_docnum
       "AND NMDFE  = I_NMDFE
      .

    SELECT *
      FROM zsdt0104
      INTO TABLE it_zsdt0104
     WHERE docnum = i_docnum.

    SELECT *
      FROM zsdt0105
      INTO TABLE it_zsdt0105
     WHERE docnum_ref = i_docnum.

    SELECT *
      FROM zsdt0118
      INTO TABLE it_zsdt0118
     WHERE docnum = i_docnum.

    "Atribui Dados MDF-e
    READ TABLE it_zsdt0102 INTO wa_zsdt0102 INDEX 1.

    me->set_nmdf(  wa_zsdt0102-nmdfe ).
    me->set_serie( wa_zsdt0102-serie ).
    me->set_modal( wa_zsdt0102-modal ).
    me->set_data_emi( wa_zsdt0102-data_emi ).
    me->set_hora_emi( wa_zsdt0102-hora_emi ).
    me->set_docnum( wa_zsdt0102-docnum ).
    me->set_cnpj_emi( wa_zsdt0102-cnpj_emi ).
    me->set_ie_emi( wa_zsdt0102-ie_emi ).
    me->set_cunid( wa_zsdt0102-cunid ).
    me->set_mun_enc( wa_zsdt0102-mun_enc ).
    me->set_tp_doc_ref( wa_zsdt0102-tp_doc_ref ).
    me->set_id_local_coleta( i_id_local_coleta = wa_zsdt0102-id_local_coleta ).
    me->set_id_local_descarga( i_id_local_descarga = wa_zsdt0102-id_local_descarga ).
    me->at_prodpred-matnr = wa_zsdt0102-matnr.
    me->at_prodpred-matkl = wa_zsdt0102-matkl.
    me->at_prodpred-tpcarga = wa_zsdt0102-tpcarga.
    me->at_prodpred-xprod = wa_zsdt0102-xprod.
    me->at_prodpred-locc_cep = wa_zsdt0102-locc_cep.
    me->at_prodpred-locd_cep = wa_zsdt0102-locd_cep.

    me->at_prodpred-locc_latitude = wa_zsdt0102-locc_latitude.
    me->at_prodpred-locd_latitude = wa_zsdt0102-locd_latitude.

    me->at_prodpred-locc_longitude = wa_zsdt0102-locc_longitude.
    me->at_prodpred-locd_longitude = wa_zsdt0102-locd_longitude.

    vl_just_canc = wa_zsdt0102-just_canc.
    me->set_just_canc( vl_just_canc ).

    "Carrega UFs
    LOOP AT it_zsdt0104 INTO wa_zsdt0104.
      me->add_uf_perc( wa_zsdt0104-uf ).
    ENDLOOP.

    "Carrega Docs Vinculados
    LOOP AT it_zsdt0105 INTO wa_zsdt0105.
      me->add_documento( wa_zsdt0105-docnum ).
    ENDLOOP.

    "Atribui Dados Manuais Transporte
    READ TABLE it_zsdt0118 INTO wa_zsdt0118 INDEX 1.
    me->set_placa_cav(  wa_zsdt0118-placa_cav  ).
    me->set_placa_car1( wa_zsdt0118-placa_car1 ).
    me->set_placa_car2( wa_zsdt0118-placa_car2 ).
    me->set_placa_car3( wa_zsdt0118-placa_car3 ).
    me->set_motorista(  wa_zsdt0118-motorista  ).
    me->set_cunid(      wa_zsdt0118-cunid      ).
    me->set_qcarga(     wa_zsdt0118-qcarga     ).

    me->at_autorizado = wa_zsdt0102-autorizado.

  ENDMETHOD.


  METHOD encerrar_mdfe.

    "Internal Tables and Work Areas
    DATA: wa_zsdt0102  TYPE zsdt0102,
          wa_j_1bnfdoc TYPE j_1bnfdoc.

    "Variables
    DATA: xdhemi        TYPE c LENGTH 30,
          xdata         TYPE c LENGTH 30,
          xhora         TYPE c LENGTH 10,
          vl_data_lim   TYPE d,
          xml_ret       TYPE string,
          xml           TYPE zxml,
          var_answer    TYPE c,
          vl_msg_exibir TYPE string.

*-#133089-21.02.2024-JT-inicio
    DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
          lc_mesg                   TYPE string.
    IF i_faturamento_autom = abap_true.
      CREATE OBJECT lc_faturamento_automatico.
    ENDIF.
*-#133089-21.02.2024-JT-fim

    "Faturamento Contingencia
*    DATA: lwa_romaneio TYPE zsdt0001.
*
*    SELECT SINGLE *
*      FROM TVARVC INTO @DATA(LWA_TVARVC_USER_CONT)
*     WHERE NAME = 'FAT_CONTINGENCIA_GOLIVE_US'
*       AND LOW = @SY-UNAME.
*
*    IF sy-subrc eq 0.
*      MESSAGE 'Encerramento MDF-e não permitido!' TYPE 'E'.
*      EXIT.
*    ENDIF.
*
*    LOOP AT at_it_doc_mdfe INTO at_wa_doc_mdfe.
*      CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0003'
*        EXPORTING
*          i_docnum      = at_wa_doc_mdfe-docnum
*        IMPORTING
*          e_zsdt0001    = lwa_romaneio.
*
*         if lwa_romaneio-fat_contingencia_ecc = abap_true.
*           MESSAGE 'Encerramento não permitido' TYPE 'E'.
*           RETURN.
*         endif.
*       ENDIF.
*    ENDLOOP.
    "Faturamento Contingencia - Fim

    REFRESH: it_nota.
    CLEAR: wa_nota, wa_j_1bnfdoc, wa_zsdt0102, xdhemi, xdata, xhora,
           xml_ret, xml, var_answer, r_error.

**--------------------------------------------------------------------------------*
**  **************************** ATENÇÃO ******************************************
**           UTILIZAR SOMENTE MESSAGE(S) COM TYPE 'S',
**           POIS O ESSE METODO É UTILIZADO POR UMA JOB.
**--------------------------------------------------------------------------------*

    IF i_no_msg_confirm IS INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Confirma solicitação de encerramento do MDF-e?'
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
        RETURN.
      ENDIF.
    ENDIF.

    "Tratativa para reenvio do envent
    SELECT SINGLE *
      FROM j_1bnfe_event INTO @DATA(lwa_event_exists)
     WHERE docnum    = @me->at_docnum
       AND int_event = 'EV_ENC'.

    IF sy-subrc EQ 0 AND lwa_event_exists-docsta NE '1'. "Evento criado e não retornado a autorização

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = |Evento encerramento em processo de autorização... Aguardando 1 minuto para reenvio...|.

      WAIT UP TO 60 SECONDS.

      SELECT SINGLE *
        FROM j_1bnfe_event INTO lwa_event_exists
       WHERE docnum    = me->at_docnum
         AND int_event = 'EV_ENC'.

      IF sy-subrc EQ 0 AND lwa_event_exists-docsta NE '1'.
        DELETE FROM j_1bnfe_event
         WHERE docnum    = me->at_docnum
          AND int_event = 'EV_ENC'.
      ENDIF.
    ENDIF.

    SELECT SINGLE *
      INTO wa_zsdt0102
      FROM zsdt0102
     WHERE docnum = me->at_docnum
       AND nmdfe  = me->at_nmdf.

    IF sy-subrc = 0.

      IF ( wa_zsdt0102-autorizado IS NOT INITIAL ) AND ( wa_zsdt0102-estornado IS NOT INITIAL ).
        UPDATE zsdt0102 SET estornado = ''
                      WHERE docnum = me->at_docnum
                        AND nmdfe  = me->at_nmdf.
        CLEAR: wa_zsdt0102-estornado.
        COMMIT WORK.
      ENDIF.

      "Check se MDF-e já foi encerrado
      IF ( wa_zsdt0102-encerrado IS INITIAL ).

        SELECT SINGLE *
          FROM j_1bnfe_event INTO @DATA(wl_event_enc)
         WHERE docnum     EQ @me->at_docnum
           AND int_event  EQ 'EV_ENC'
           AND authcod    NE @space.

        IF sy-subrc EQ 0.
          UPDATE zsdt0102 SET encerrado = abap_true
                        WHERE docnum = me->at_docnum.
          wa_zsdt0102-encerrado = abap_true.
          COMMIT WORK.
        ENDIF.
      ENDIF.

      IF NOT ( wa_zsdt0102-estornado IS INITIAL ).
        r_error = abap_true.
        ROLLBACK WORK.
        "MESSAGE 'MDF-e já estornado! Operação não permitida!' TYPE 'S'.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE s022.
            RETURN.
          WHEN abap_true.
            MESSAGE s022 INTO lc_mesg.
            lc_mesg = lc_mesg && ':' && wa_zsdt0102-docnum.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).
            RETURN.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

      IF NOT ( wa_zsdt0102-encerrado IS INITIAL ).
        r_error = abap_true.
        ROLLBACK WORK.
        "MESSAGE 'MDF-e já encerrado! Operação não permitida!' TYPE 'S'.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE s023.
            RETURN.
          WHEN abap_true.
            MESSAGE s023 INTO lc_mesg.
            lc_mesg = lc_mesg && ':' && wa_zsdt0102-docnum.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).
            RETURN.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

      IF NOT ( wa_zsdt0102-cancel IS INITIAL ).
        r_error = abap_true.
        ROLLBACK WORK.
        "MESSAGE 'MDF-e já cancelado! Operação não permitida!' TYPE 'S'.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE s024.
            RETURN.
          WHEN abap_true.
            MESSAGE s024 INTO lc_mesg.
            lc_mesg = lc_mesg && ':' && wa_zsdt0102-docnum.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).
            RETURN.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

      IF ( wa_zsdt0102-autorizado IS INITIAL ).
        r_error = abap_true.
        ROLLBACK WORK.
        "MESSAGE 'MDF-e não autorizado! Operação não permitida!' TYPE 'S'.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE s025.
            RETURN.
          WHEN abap_true.
            MESSAGE s025 INTO lc_mesg.
            lc_mesg = lc_mesg && ':' && wa_zsdt0102-docnum.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).
            RETURN.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

    ELSE.
      r_error = abap_true.
      ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
          MESSAGE s026.
          RETURN.
        WHEN abap_true.
          MESSAGE s026 INTO lc_mesg.
          lc_mesg = lc_mesg && ':' && wa_zsdt0102-docnum.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).
          RETURN.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_zsdt0237)
      FROM zsdt0237
     WHERE docnum EQ @me->at_docnum.

    IF sy-subrc IS NOT INITIAL.
      CLEAR: at_wa_doc_mdfe.
      READ TABLE at_it_doc_mdfe INTO at_wa_doc_mdfe INDEX 1.

      SELECT SINGLE *
        INTO wa_j_1bnfdoc
        FROM j_1bnfdoc
       WHERE docnum = at_wa_doc_mdfe-docnum.

      IF sy-subrc <> 0.
        r_error = abap_true.
        CLEAR: vl_msg_exibir.
        ROLLBACK WORK.
        MESSAGE s011 WITH at_wa_doc_mdfe-docnum.
        RETURN.
      ENDIF.
    ENDIF.

    IF ( sy-batch          EQ abap_false ) AND
       ( i_no_msg_confirm  IS INITIAL    ) AND
       ( i_solicita_motivo EQ abap_true  ).
      wa_zsdt0102-motivo_enc = me->get_motivo_encerramento( ).
      IF wa_zsdt0102-motivo_enc IS INITIAL.
        r_error = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    wa_zsdt0102-transmissao = '3'. "Encerramento
    wa_zsdt0102-usuario_enc = sy-uname.
    wa_zsdt0102-doc_mdfe_sol_enc = i_doc_mdfe_sol. "Documento MDF-e que Partiu a Solicitação de Encerramento Automático.

    IF ( i_enc_aut IS NOT INITIAL ).
      wa_zsdt0102-sol_enc_aut = 'X'.
    ENDIF.

    MODIFY zsdt0102 FROM wa_zsdt0102.

    IF ( sy-subrc <> 0 ).
      r_error = abap_true.
      ROLLBACK WORK.
      "MESSAGE 'Houve um erro ao gravar os dados de transmissão do MDF-e!' TYPE 'S'.
      MESSAGE s027.
      RETURN.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_mdfe)
      FROM j_1bnfdoc
     WHERE docnum EQ @wa_zsdt0102-docnum
       AND model  EQ @zif_doc_eletronico=>at_st_model_mdfe.

    IF ( sy-subrc IS INITIAL ).
      DATA(ck_grc) = abap_true.
    ELSE.
      ck_grc = abap_false.
    ENDIF.

    CASE ck_grc.
      WHEN abap_true.

        IF ( wa_zsdt0102-docnum IS NOT INITIAL ) AND ( wa_zsdt0102-nmdfe IS INITIAL ) AND ( wa_mdfe-nfenum IS NOT INITIAL ).
          UPDATE zsdt0102 SET nmdfe  = wa_mdfe-nfenum
                        WHERE docnum = wa_zsdt0102-docnum.
          COMMIT WORK.
        ENDIF.

        DATA: zet_bapiret2 TYPE zbapirettab.

        CALL FUNCTION 'Z_J_1B_MDFE_CLOSE'
          EXPORTING
            i_docnum              = wa_mdfe-docnum
          IMPORTING
            zet_bapiret2          = zet_bapiret2
          EXCEPTIONS
            rfc_error             = 1
            communication_failure = 2
            system_failure        = 3
            OTHERS                = 4.

        IF sy-subrc IS NOT INITIAL.

          r_error = abap_true.

          ROLLBACK WORK.

          IF NOT zet_bapiret2[] IS INITIAL.
            CALL FUNCTION 'J_1B_NFE_BAPIRET2_MAP_TO_LOG1'
              EXPORTING
                iv_docnum   = wa_mdfe-docnum
                it_bapiret2 = zet_bapiret2.
          ENDIF.

          IF sy-batch EQ abap_true.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
          ENDIF.

          RETURN.

        ENDIF.

      WHEN abap_false.

        me->monta_xml( EXPORTING tipo  = '3' " Encerramento
                       RECEIVING e_xml = xml_ret ).

        IF ( xml_ret IS INITIAL ).
          ROLLBACK WORK.
          RETURN.
        ENDIF.

        xml = xml_ret.

        REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN xml WITH 'a' IGNORING CASE.
        REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN xml WITH 'e' IGNORING CASE.
        REPLACE ALL OCCURRENCES OF        'í'     IN xml WITH 'i' IGNORING CASE.
        REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN xml WITH 'o' IGNORING CASE.
        REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN xml WITH 'u' IGNORING CASE.
        REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN xml WITH 'c' IGNORING CASE.
        REPLACE ALL OCCURRENCES OF        '&'     IN xml WITH '&#38;'.
        REPLACE ALL OCCURRENCES OF        ''''    IN xml WITH '&#39;'.
        REPLACE ALL OCCURRENCES OF        'º'     IN xml WITH 'o' IGNORING CASE.

        CONCATENATE sy-uzeit(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2) INTO xhora.
        CONCATENATE sy-datum(4) '-' sy-datum+4(2) '-' sy-datum+6(2) INTO xdata.
        CONCATENATE xdata xhora INTO xdhemi SEPARATED BY space.

        wa_nota-tp_authcod       = '6'.
        wa_nota-nu_documento_sap = me->at_docnum..
        wa_nota-id_empresa       = wa_j_1bnfdoc-bukrs.
        wa_nota-id_filial        = wa_j_1bnfdoc-branch.
        wa_nota-tb_direcao       = wa_j_1bnfdoc-direct.
        wa_nota-nr_nfe           = me->at_nmdf.
        wa_nota-sr_nfe           = wa_j_1bnfdoc-series.
        wa_nota-data_hora        = xdhemi.
        wa_nota-id_destinatario  = wa_j_1bnfdoc-parid.
        wa_nota-tx_xml           = xml(4000).
        wa_nota-tx_xml2          = xml+04000(4000).
        wa_nota-tx_xml3          = xml+08000(4000).

        APPEND wa_nota TO it_nota.

        SELECT SINGLE *
          FROM setleaf INTO @DATA(wl_setleaf)
         WHERE setname EQ 'MAGGI_CTG_CTE_SAP'.

        IF ( sy-subrc = 0 ) AND ( wl_setleaf-valfrom IS NOT INITIAL ).

          DATA: wl_zob_cte_sap TYPE zob_cte_sap.

          MOVE-CORRESPONDING wa_nota TO wl_zob_cte_sap.

          CALL FUNCTION 'Z_GRAVAR_XML_NFE_CTE'
            EXPORTING
              i_xml01       = xml
              i_tipo        = '2' "CT-e
            CHANGING
              i_zob_cte_sap = wl_zob_cte_sap.

        ELSE.

          CALL FUNCTION 'Z_SD_OUTBOUND_CTE_XML' IN BACKGROUND TASK
            DESTINATION 'XI_XML_CTE'
            AS SEPARATE UNIT
            TABLES
              it_saida = it_nota.

        ENDIF.

        COMMIT WORK.

        "MESSAGE 'Documento MDF-e processado com êxito!' TYPE 'S'.
        MESSAGE s028.
        "RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD enviar_mdfe.

    "Internal Tables and Work Areas
    DATA: wa_zsdt0102      TYPE zsdt0102.
    DATA: wa_j_1bnfdoc     TYPE j_1bnfdoc.

    "Variables
    DATA: xdhemi        TYPE c LENGTH 30,
          xdata         TYPE c LENGTH 30,
          xhora         TYPE c LENGTH 10,
          xml_ret       TYPE string,
          xml           TYPE zxml,
          vl_docnum     TYPE j_1bdocnum,
          var_answer    TYPE c,
          vl_msg_1      TYPE string,
          vl_msg_2      TYPE string,
          vl_msg_3      TYPE string,
          vl_msg_exibir TYPE string,
          vl_cnpj_emi   TYPE zcte_parceiros-emit_cnpj.

*-#133089-21.02.2024-JT-inicio
    DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
          lc_mesg                   TYPE string.
    IF i_faturamento_autom = abap_true.
      CREATE OBJECT lc_faturamento_automatico.
    ENDIF.
*-#133089-21.02.2024-JT-fim

    REFRESH: it_nota.
    CLEAR: wa_nota, wa_j_1bnfdoc, wa_zsdt0102, xdhemi, xdata, xhora,
           xml_ret, xml, vl_docnum, var_answer, vl_msg_exibir.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarvc_user_cont)
     WHERE name = 'FAT_CONTINGENCIA_GOLIVE_US'
       AND low = @sy-uname.

    IF sy-subrc EQ 0.
      MESSAGE 'Encerramento MDF-e não permitido!' TYPE 'E'.
      EXIT.
    ENDIF.

    "Encerramento Automático

    "Ajuste encerramento MDF-e IR237026 - WPP
    SELECT SINGLE *
      FROM zsdt0102 INTO @DATA(wa_zsdt0102_exists)
     WHERE docnum EQ @me->at_docnum.

    IF sy-subrc EQ 0 AND wa_zsdt0102_exists-autorizado IS INITIAL.
      DATA(lva_continue) = me->check_encerramento_mdfes( EXPORTING i_sem_confirmacao   = i_sem_confirmacao
                                                                   i_faturamento_autom = i_faturamento_autom "*-#133089-21.02.2024-JT
                                                                   i_ch_referencia     = i_ch_referencia     "*-#133089-21.02.2024-JT
                                                         IMPORTING e_docnum_sol_enc    = e_docnum_sol_enc ).

    ELSE.
      lva_continue = abap_true.
    ENDIF. "Ajuste encerramento MDF-e IR237026 - WPP

    CHECK lva_continue EQ abap_true.

    "------------------------------------------------------------------
    " Encerramento Automático - (Fim)
    "------------------------------------------------------------------
    IF i_sem_confirmacao NE abap_true.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Confirma envio do MDF-e?'
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
        RETURN.
      ENDIF.
    ENDIF.

    CLEAR: wa_zsdt0102.
    SELECT SINGLE *
      INTO wa_zsdt0102
      FROM zsdt0102
     WHERE docnum = me->at_docnum
       AND nmdfe  = me->at_nmdf.

    IF sy-subrc = 0.

      IF NOT ( wa_zsdt0102-estornado IS INITIAL ).
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE w022.
            "MESSAGE 'MDF-e já estornado! Operação não permitida!' TYPE 'W'.
            RETURN.
          WHEN abap_true.
            MESSAGE w022 INTO lc_mesg.
            lc_mesg = lc_mesg && ':' && wa_zsdt0102-docnum.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).
            RETURN.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

      IF NOT ( wa_zsdt0102-encerrado IS INITIAL ).
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE w023.
            "MESSAGE 'MDF-e já encerrado! Operação não permitida!' TYPE 'W'.
            RETURN.
          WHEN abap_true.
            MESSAGE w023 INTO lc_mesg.
            lc_mesg = lc_mesg && ':' && wa_zsdt0102-docnum.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).
            RETURN.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

      IF NOT ( wa_zsdt0102-cancel IS INITIAL ).
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE w024.
            "MESSAGE 'MDF-e já cancelado! Operação não permitida!' TYPE 'W'.
            RETURN.
          WHEN abap_true.
            MESSAGE w024 INTO lc_mesg.
            lc_mesg = lc_mesg && ':' && wa_zsdt0102-docnum.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).
            RETURN.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
      DATA: lwa_contingencia_mdfe        TYPE char01.

      SELECT SINGLE branch
        FROM j_1bnfe_active
        INTO @DATA(_branch)
       WHERE docnum = @me->at_docnum.

      lwa_contingencia_mdfe = me->check_contingencia_mdfe( _branch ).
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

      IF lwa_contingencia_mdfe NE abap_true. "*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
        IF NOT ( wa_zsdt0102-autorizado IS INITIAL ).
          IF ( wa_zsdt0102-authcode IS NOT INITIAL ).
            ROLLBACK WORK.
            MESSAGE w041.
            "MESSAGE 'MDF-e já autorizado! Operação não permitida!' TYPE 'W'.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF."*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    ELSE.
      ROLLBACK WORK.
      MESSAGE e037.
      "MESSAGE 'Não foi possível enviar o MDF-e' TYPE 'E'.
      "RETURN.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_zsdt0237)
      FROM zsdt0237
     WHERE docnum EQ @me->at_docnum.

    IF sy-subrc IS NOT INITIAL.

      SELECT * INTO TABLE @DATA(it_zsdt0241)
        FROM zsdt0241
       WHERE docnum EQ @me->at_docnum.

      CLEAR: at_wa_doc_mdfe.
      READ TABLE at_it_doc_mdfe INTO at_wa_doc_mdfe INDEX 1.

      SELECT SINGLE *
        INTO wa_j_1bnfdoc
        FROM j_1bnfdoc
       WHERE docnum = at_wa_doc_mdfe-docnum.

      IF sy-subrc <> 0 AND it_zsdt0241[] IS INITIAL.
        CLEAR: vl_msg_exibir.
        ROLLBACK WORK.
        MESSAGE w011 WITH at_wa_doc_mdfe-docnum.
        RETURN.
      ENDIF.

    ENDIF.

    LOOP AT me->at_it_doc_mdfe INTO me->at_wa_doc_mdfe.

      "Verficar se tem seguro
      TRY .
          MESSAGE s050(zmdfe) WITH me->at_wa_doc_mdfe-docnum.
          DATA(ck_emite_seguro) = zcl_averbacao_seguro=>ck_docnum_seguro( i_docnum = me->at_wa_doc_mdfe-docnum ).
        CATCH zcx_averbacao_seguro.
      ENDTRY.

      IF ck_emite_seguro EQ abap_true.
        IF i_raise EQ abap_false.

          TRY.
              MESSAGE s051(zmdfe) WITH me->at_wa_doc_mdfe-docnum.
              DATA(r_autorizado) = zcl_averbacao_seguro=>emitir_averbacao_cte( i_docnum = me->at_wa_doc_mdfe-docnum ).
            CATCH zcx_doc_eletronico INTO DATA(ex_doc_mdfe).
              ROLLBACK WORK.

              "FF #169614 - inicio
              CASE i_faturamento_autom.
                WHEN abap_off.
                  ex_doc_mdfe->published_erro( i_msgty = 'W' i_msgty_display = 'W' ).
                  RETURN.
                WHEN abap_on.

                  MESSAGE ID ex_doc_mdfe->msgid TYPE 'E'
                  NUMBER ex_doc_mdfe->msgno
                  WITH ex_doc_mdfe->msgv1 ex_doc_mdfe->msgv2 ex_doc_mdfe->msgv3 ex_doc_mdfe->msgv4
                  INTO lc_mesg.

                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).

                  RETURN.
              ENDCASE.
              "FF #169614 - fim

            CATCH zcx_averbacao_seguro INTO DATA(cx_averbacao_seguro).
              ROLLBACK WORK.

              "FF #169614 - inicio
              CASE i_faturamento_autom.
                WHEN abap_off.
                  cx_averbacao_seguro->published_erro( i_msgty = 'W' i_msgty_display = 'W' ).
                  RETURN.
                WHEN abap_on.

                  MESSAGE e021(zavseguro) WITH cx_averbacao_seguro->msgv1
                                               cx_averbacao_seguro->msgv2
                                               cx_averbacao_seguro->msgv3 INTO lc_mesg.

                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).

                  RETURN.
              ENDCASE.
              "FF #169614 - fim

            CATCH zcx_arquivo INTO DATA(cx_arquivo).
              ROLLBACK WORK.

              "FF #169614 - inicio
              CASE i_faturamento_autom.
                WHEN abap_off.
                  cx_arquivo->published_erro( i_msgty = 'W' i_msgty_display = 'W' ).
                  RETURN.
                WHEN abap_on.


                  MESSAGE ID cx_arquivo->msgid TYPE 'E'
                  NUMBER cx_arquivo->msgno
                  WITH cx_arquivo->msgv1 cx_arquivo->msgv2 cx_arquivo->msgv3 cx_arquivo->msgv4
                  INTO lc_mesg.

                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).

                  RETURN.
              ENDCASE.
              "FF #169614 - fim


            CATCH zcx_cadastro INTO DATA(cx_cadastro).
              ROLLBACK WORK.

              "FF #169614 - inicio
              CASE i_faturamento_autom.
                WHEN abap_off.
                  cx_cadastro->published_erro( i_msgty = 'W' i_msgty_display = 'W' ).
                  RETURN.
                WHEN abap_on.


                  MESSAGE ID cx_cadastro->msgid TYPE 'E'
                  NUMBER cx_cadastro->msgno
                  WITH cx_cadastro->msgv1 cx_cadastro->msgv2 cx_cadastro->msgv3 cx_cadastro->msgv4
                  INTO lc_mesg.

                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).

                  RETURN.
              ENDCASE.
              "FF #169614 - fim


          ENDTRY.

        ELSE.

          TRY.
              MESSAGE s051(zmdfe) WITH me->at_wa_doc_mdfe-docnum.
              r_autorizado = zcl_averbacao_seguro=>emitir_averbacao_cte( i_docnum = me->at_wa_doc_mdfe-docnum ).
              MESSAGE s052(zmdfe) WITH me->at_wa_doc_mdfe-docnum.

            CATCH zcx_doc_eletronico INTO ex_doc_mdfe.
              ROLLBACK WORK.


              "FF #169614 - inicio
              CASE i_faturamento_autom.
                WHEN abap_off.
                  "FF #169614 - fim


                  MESSAGE ID ex_doc_mdfe->msgid
                     TYPE 'S'
                     NUMBER ex_doc_mdfe->msgno
                     WITH ex_doc_mdfe->msgv1 ex_doc_mdfe->msgv2 ex_doc_mdfe->msgv3 ex_doc_mdfe->msgv4
                    DISPLAY LIKE 'E'.

                  RAISE EXCEPTION TYPE zcx_averbacao_seguro
                    EXPORTING
                      textid = VALUE #( msgid = ex_doc_mdfe->msgid
                                        msgno = ex_doc_mdfe->msgno
                                        attr1 = ex_doc_mdfe->msgv1
                                        attr2 = ex_doc_mdfe->msgv2
                                        attr3 = ex_doc_mdfe->msgv3
                                        attr4 = ex_doc_mdfe->msgv4 )
                      msgid  = ex_doc_mdfe->msgid
                      msgno  = ex_doc_mdfe->msgno
                      msgty  = 'E'
                      msgv1  = ex_doc_mdfe->msgv1
                      msgv2  = ex_doc_mdfe->msgv2
                      msgv3  = ex_doc_mdfe->msgv3
                      msgv4  = ex_doc_mdfe->msgv4.

                  "FF #169614 - inicio
                WHEN abap_on.

                  MESSAGE ID ex_doc_mdfe->msgid TYPE 'E'
                  NUMBER ex_doc_mdfe->msgno
                  WITH ex_doc_mdfe->msgv1 ex_doc_mdfe->msgv2 ex_doc_mdfe->msgv3 ex_doc_mdfe->msgv4
                  INTO lc_mesg.

                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).

                  RETURN.
              ENDCASE.
              "FF #169614 - fim


            CATCH zcx_averbacao_seguro INTO cx_averbacao_seguro.
              ROLLBACK WORK.


              "FF #169614 - inicio
              CASE i_faturamento_autom.
                WHEN abap_off.
                  "FF #169614 - fim

                  MESSAGE ID cx_averbacao_seguro->msgid
                     TYPE 'S'
                     NUMBER cx_averbacao_seguro->msgno
                     WITH cx_averbacao_seguro->msgv1 cx_averbacao_seguro->msgv2 cx_averbacao_seguro->msgv3 cx_averbacao_seguro->msgv4
                    DISPLAY LIKE 'E'.

                  RAISE EXCEPTION TYPE zcx_averbacao_seguro
                    EXPORTING
                      textid = VALUE #( msgid = cx_averbacao_seguro->msgid
                                        msgno = cx_averbacao_seguro->msgno
                                        attr1 = cx_averbacao_seguro->msgv1
                                        attr2 = cx_averbacao_seguro->msgv2
                                        attr3 = cx_averbacao_seguro->msgv3
                                        attr4 = cx_averbacao_seguro->msgv4 )
                      msgid  = cx_averbacao_seguro->msgid
                      msgno  = cx_averbacao_seguro->msgno
                      msgty  = 'E'
                      msgv1  = cx_averbacao_seguro->msgv1
                      msgv2  = cx_averbacao_seguro->msgv2
                      msgv3  = cx_averbacao_seguro->msgv3
                      msgv4  = cx_averbacao_seguro->msgv4.

                  "FF #169614 - inicio
                WHEN abap_on.

                  MESSAGE e021(zavseguro) WITH cx_averbacao_seguro->msgv1
                                               cx_averbacao_seguro->msgv2
                                               cx_averbacao_seguro->msgv3 INTO lc_mesg.

                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).

                  RETURN.
              ENDCASE.
              "FF #169614 - fim



            CATCH zcx_arquivo INTO cx_arquivo.
              ROLLBACK WORK.

              "FF #169614 - inicio
              CASE i_faturamento_autom.
                WHEN abap_off.
                  "FF #169614 - fim

                  MESSAGE ID cx_arquivo->msgid
                     TYPE 'S'
                     NUMBER cx_arquivo->msgno
                     WITH cx_arquivo->msgv1 cx_arquivo->msgv2 cx_arquivo->msgv3 cx_arquivo->msgv4
                    DISPLAY LIKE 'E'.

                  RAISE EXCEPTION TYPE zcx_averbacao_seguro
                    EXPORTING
                      textid = VALUE #( msgid = cx_arquivo->msgid
                                        msgno = cx_arquivo->msgno
                                        attr1 = cx_arquivo->msgv1
                                        attr2 = cx_arquivo->msgv2
                                        attr3 = cx_arquivo->msgv3
                                        attr4 = cx_arquivo->msgv4 )
                      msgid  = cx_arquivo->msgid
                      msgno  = cx_arquivo->msgno
                      msgty  = 'E'
                      msgv1  = cx_arquivo->msgv1
                      msgv2  = cx_arquivo->msgv2
                      msgv3  = cx_arquivo->msgv3
                      msgv4  = cx_arquivo->msgv4.


                  "FF #169614 - inicio
                WHEN abap_on.

                  MESSAGE ID cx_arquivo->msgid TYPE 'E'
                  NUMBER cx_arquivo->msgno
                  WITH cx_arquivo->msgv1 cx_arquivo->msgv2 cx_arquivo->msgv3 cx_arquivo->msgv4
                  INTO lc_mesg.

                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).

                  RETURN.
              ENDCASE.
              "FF #169614 - fim

            CATCH zcx_cadastro INTO cx_cadastro.
              ROLLBACK WORK.

              "FF #169614 - inicio
              CASE i_faturamento_autom.
                WHEN abap_off.
                  "FF #169614 - fim


                  MESSAGE ID cx_cadastro->msgid
                     TYPE 'S'
                     NUMBER cx_cadastro->msgno
                     WITH cx_cadastro->msgv1 cx_cadastro->msgv2 cx_cadastro->msgv3 cx_cadastro->msgv4
                    DISPLAY LIKE 'E'.

                  RAISE EXCEPTION TYPE zcx_averbacao_seguro
                    EXPORTING
                      textid = VALUE #( msgid = cx_cadastro->msgid
                                        msgno = cx_cadastro->msgno
                                        attr1 = cx_cadastro->msgv1
                                        attr2 = cx_cadastro->msgv2
                                        attr3 = cx_cadastro->msgv3
                                        attr4 = cx_cadastro->msgv4 )
                      msgid  = cx_cadastro->msgid
                      msgno  = cx_cadastro->msgno
                      msgty  = 'E'
                      msgv1  = cx_cadastro->msgv1
                      msgv2  = cx_cadastro->msgv2
                      msgv3  = cx_cadastro->msgv3
                      msgv4  = cx_cadastro->msgv4.


                  "FF #169614 - inicio
                WHEN abap_on.


                  MESSAGE ID cx_cadastro->msgid TYPE 'E'
                  NUMBER cx_cadastro->msgno
                  WITH cx_cadastro->msgv1 cx_cadastro->msgv2 cx_cadastro->msgv3 cx_cadastro->msgv4
                  INTO lc_mesg.

                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).

                  RETURN.
              ENDCASE.
              "FF #169614 - fim

          ENDTRY.

        ENDIF.
        CHECK r_autorizado EQ abap_true.

      ENDIF.

    ENDLOOP.

    wa_zsdt0102-transmissao = '1'. "Envio

    SELECT SINGLE * INTO @wa_j_1bnfdoc
      FROM j_1bnfdoc
     WHERE docnum EQ @wa_zsdt0102-docnum
       AND model  EQ @zif_doc_eletronico=>at_st_model_mdfe.

    IF ( sy-subrc IS INITIAL ).
      DATA(lc_grc) = abap_true.

      TRY .
          COMMIT WORK AND WAIT.

          zcl_mdfe_=>zif_doc_eletronico~get_instance( i_docnum = wa_j_1bnfdoc-docnum
            )->set_registro( EXPORTING i_docnum = wa_j_1bnfdoc-docnum i_sem_bloqueio = abap_true
            )->set_autorizar( i_aguardar = i_aguardar i_ciclos = i_ciclos  i_segundos = i_segundos
            )->set_registro( EXPORTING i_docnum = wa_j_1bnfdoc-docnum i_sem_bloqueio = abap_true
            )->get_registro( IMPORTING e_documento = wa_j_1bnfdoc e_info_doc_eletronico = DATA(e_info_doc_eletronico)
            ).

*        WA_ZSDT0102-NMDFE = E_INFO_DOC_ELETRONICO-NFNUM9.
*        WA_ZSDT0102-SERIE = E_INFO_DOC_ELETRONICO-SERIE.
*
*        UPDATE ZSDT0102
*           SET NMDFE = WA_ZSDT0102-NMDFE
*               SERIE = WA_ZSDT0102-SERIE
*         WHERE DOCNUM EQ WA_J_1BNFDOC-DOCNUM.
*
*        UPDATE ZSDT0105
*           SET NMDFE = WA_ZSDT0102-NMDFE
*         WHERE DOCNUM_REF EQ WA_J_1BNFDOC-DOCNUM.
*
*        UPDATE ZSDT0107
*           SET NMDFE = WA_ZSDT0102-NMDFE
*         WHERE DOCNUM EQ WA_J_1BNFDOC-DOCNUM.

          COMMIT WORK.

          "FF #169614 - inicio
          CASE i_faturamento_autom.
            WHEN abap_off.
              "FF #169614 - fim

              MESSAGE s028.

              "FF #169614 - inicio
            WHEN abap_on.

              MESSAGE s028 INTO lc_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'S' i_msg = lc_mesg i_status = 'MDFE' ).

          ENDCASE.
          "FF #169614 - fim
        CATCH zcx_doc_eletronico INTO ex_doc_mdfe.    " .
          ROLLBACK WORK.

          "FF #169614 - inicio
          CASE i_faturamento_autom.
            WHEN abap_off.
              "FF #169614 - fim

              CASE i_raise.
                WHEN abap_false.
                  ex_doc_mdfe->published_erro( ).

                WHEN abap_true.

                  MESSAGE ID ex_doc_mdfe->msgid
                     TYPE 'S'
                     NUMBER ex_doc_mdfe->msgno
                     WITH ex_doc_mdfe->msgv1 ex_doc_mdfe->msgv2 ex_doc_mdfe->msgv3 ex_doc_mdfe->msgv4
                    DISPLAY LIKE 'E'.

                  RAISE EXCEPTION TYPE zcx_doc_eletronico
                    EXPORTING
                      textid = VALUE #( msgid = ex_doc_mdfe->msgid
                                        msgno = ex_doc_mdfe->msgno
                                        attr1 = ex_doc_mdfe->msgv1
                                        attr2 = ex_doc_mdfe->msgv2
                                        attr3 = ex_doc_mdfe->msgv3
                                        attr4 = ex_doc_mdfe->msgv4 )
                      msgid  = ex_doc_mdfe->msgid
                      msgno  = ex_doc_mdfe->msgno
                      msgty  = 'E'
                      msgv1  = ex_doc_mdfe->msgv1
                      msgv2  = ex_doc_mdfe->msgv2
                      msgv3  = ex_doc_mdfe->msgv3
                      msgv4  = ex_doc_mdfe->msgv4.

              ENDCASE.

              "FF #169614 - inicio
            WHEN abap_on.

              MESSAGE ID ex_doc_mdfe->msgid TYPE 'E'
              NUMBER ex_doc_mdfe->msgno
              WITH ex_doc_mdfe->msgv1 ex_doc_mdfe->msgv2 ex_doc_mdfe->msgv3 ex_doc_mdfe->msgv4
              INTO lc_mesg.

              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).

              RETURN.
          ENDCASE.
          "FF #169614 - fim

      ENDTRY.

    ELSE.
      lc_grc = abap_false.
      CLEAR wa_zsdt0102-msg.
      MODIFY zsdt0102 FROM wa_zsdt0102.
    ENDIF.

    CHECK lc_grc EQ abap_false.

    IF ( sy-subrc <> 0 ).
      ROLLBACK WORK.

      "FF #169614 - inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
          "FF #169614 - fim

          MESSAGE e027.
          "MESSAGE 'Houve um erro ao gravar os dados de transmissão do MDF-e!' TYPE 'E'.
          "RETURN.

          "FF #169614 - inicio
        WHEN abap_on.

          MESSAGE s027 INTO lc_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = lc_mesg i_status = 'MDFE' ).

      ENDCASE.
      "FF #169614 - fim


    ENDIF.

    me->monta_xml( EXPORTING tipo  = '1' " Envio.
                   RECEIVING e_xml = xml_ret ).

    xml = xml_ret.

    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN xml WITH 'a' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN xml WITH 'e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'í'     IN xml WITH 'i' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN xml WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN xml WITH 'u' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN xml WITH 'c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        '&'     IN xml WITH '&#38;'.
    REPLACE ALL OCCURRENCES OF        ''''    IN xml WITH '&#39;'.
    REPLACE ALL OCCURRENCES OF        'º'     IN xml WITH 'o' IGNORING CASE.

    CONCATENATE me->at_hora_emi(2) ':' me->at_hora_emi+2(2) ':' me->at_hora_emi+4(2) INTO xhora.
    CONCATENATE me->at_data_emi(4) '-' me->at_data_emi+4(2) '-' me->at_data_emi+6(2) INTO xdata.
    CONCATENATE xdata xhora INTO xdhemi SEPARATED BY space.

    wa_nota-tp_authcod       = '4'.
    wa_nota-nu_documento_sap = me->at_docnum.
    wa_nota-id_empresa       = wa_j_1bnfdoc-bukrs.
    wa_nota-id_filial        = wa_j_1bnfdoc-branch.
    wa_nota-tb_direcao       = wa_j_1bnfdoc-direct.
    wa_nota-nr_nfe           = me->at_nmdf.
    wa_nota-sr_nfe           = wa_j_1bnfdoc-series.
    wa_nota-data_hora        = xdhemi.
    wa_nota-id_destinatario  = wa_j_1bnfdoc-parid.
    wa_nota-tx_xml           = xml(4000).
    wa_nota-tx_xml2          = xml+04000(4000).
    wa_nota-tx_xml3          = xml+08000(4000).

    APPEND wa_nota TO it_nota.

    SELECT SINGLE *
    FROM setleaf INTO @DATA(wl_setleaf)
    WHERE setname EQ 'MAGGI_CTG_CTE_SAP'.

    IF ( sy-subrc = 0 ) AND ( wl_setleaf-valfrom IS NOT INITIAL ).

      DATA: wl_zob_cte_sap TYPE zob_cte_sap.

      MOVE-CORRESPONDING wa_nota TO wl_zob_cte_sap.

      CALL FUNCTION 'Z_GRAVAR_XML_NFE_CTE'
        EXPORTING
          i_xml01       = xml
          i_tipo        = '2' "CT-e
        CHANGING
          i_zob_cte_sap = wl_zob_cte_sap.

    ELSE.

      CALL FUNCTION 'Z_SD_OUTBOUND_CTE_XML' IN BACKGROUND TASK
        DESTINATION 'XI_XML_CTE'
        AS SEPARATE UNIT
        TABLES
          it_saida = it_nota.

    ENDIF.

    COMMIT WORK.
    "FF #169614 - inicio
    CASE i_faturamento_autom.
      WHEN abap_off.
        "FF #169614 - fim
        MESSAGE s028.
        "MESSAGE 'Documento MDF-e processado com êxito!' TYPE 'S'.
        "RETURN.

        "FF #169614 - inicio
      WHEN abap_on.

        MESSAGE s028 INTO lc_mesg.
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'S' i_msg = lc_mesg i_status = 'MDFE' ).

    ENDCASE.
    "FF #169614 - fim
  ENDMETHOD.


  METHOD estornar_mdfe.

    "Internal Table and WorkAreas
    DATA: wa_zsdt0102 TYPE zsdt0102.

    "Variables
    DATA: var_answer TYPE c.

    CLEAR: var_answer, wa_zsdt0102.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja realmente estornar o MDF-e?'
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
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bnfe_active
      INTO @DATA(ls_active)
      WHERE docnum = @me->at_docnum.
    IF sy-subrc IS INITIAL.
      IF ls_active-scssta EQ '0' AND ls_active-action_requ IS INITIAL.
        MESSAGE e045.
        RETURN.
      ENDIF.
    ENDIF.

    SELECT SINGLE *
      INTO wa_zsdt0102
      FROM zsdt0102
     WHERE docnum = me->at_docnum
       "AND NMDFE  = ME->AT_NMDF
      .

    IF sy-subrc = 0.

      CASE wa_zsdt0102-transmissao.
        WHEN '1'.
          ROLLBACK WORK.
          MESSAGE w038.
          "MESSAGE 'MDF-e em processamento de envio! Operação não permitida!' TYPE 'W'.
          RETURN.
        WHEN '2'.
          ROLLBACK WORK.
          MESSAGE w039.
          "MESSAGE 'MDF-e em processamento de cancelamento! Operação não permitida!' TYPE 'W'.
          RETURN.
        WHEN '3'.
          ROLLBACK WORK.
          MESSAGE w040.
          "MESSAGE 'MDF-e em processamento de encerramento! Operação não permitida!' TYPE 'W'.
          RETURN.
      ENDCASE.

      IF NOT ( wa_zsdt0102-estornado IS INITIAL ).
        ROLLBACK WORK.
        "MESSAGE 'MDF-e já estornado! Operação não permitida!' TYPE 'W'.
        MESSAGE w022.
        RETURN.
      ENDIF.

      IF NOT ( wa_zsdt0102-encerrado IS INITIAL ).
        ROLLBACK WORK.
        "MESSAGE 'MDF-e já encerrado! Estorno não permitido!' TYPE 'W'.
        MESSAGE w023.
        RETURN.
      ENDIF.

      IF NOT ( wa_zsdt0102-cancel IS INITIAL ).
        ROLLBACK WORK.
        "MESSAGE 'MDF-e já cancelado! Estorno não permitido!' TYPE 'W'.
        MESSAGE w024.
        RETURN.
      ENDIF.

      IF NOT ( wa_zsdt0102-autorizado IS INITIAL ).
        ROLLBACK WORK.
        "MESSAGE 'MDF-e já autorizado! Estorno não permitido!' TYPE 'W'.
        MESSAGE w041.
        RETURN.
      ENDIF.

      "" Verificar se tem que estornar Documento Fiscal """"""""""""""""""""""""""""""""""""""""
      SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
        FROM j_1bnfdoc
       WHERE docnum EQ @wa_zsdt0102-docnum
         AND model  EQ @zif_doc_eletronico=>at_st_model_mdfe.

      IF sy-subrc IS INITIAL.

        DATA: fiscal TYPE REF TO zcl_fiscal.
        CREATE OBJECT fiscal.

        SELECT SINGLE * INTO @DATA(wa_active_mdfe)
          FROM j_1bnfe_active
         WHERE docnum EQ @wa_zsdt0102-docnum
           AND model  EQ @zif_doc_eletronico=>at_st_model_mdfe.

        IF ( sy-subrc                   EQ 0   ) AND
           ( wa_active_mdfe-action_requ EQ '8' ) AND
           ( wa_active_mdfe-docsta      NE '1' ).

          wa_active_mdfe-docsta = '3'.
          wa_active_mdfe-scssta = '0'.
          wa_active_mdfe-msstat = 'V'.

          wa_j_1bnfdoc-docstat  = '3'.

          CALL FUNCTION 'J_1B_NFE_UPDATE_ACTIVE'
            EXPORTING
              i_acttab     = wa_active_mdfe
              i_doc        = wa_j_1bnfdoc
              i_updmode    = 'U'
            EXCEPTIONS
              update_error = 1
              OTHERS       = 2.

          IF sy-subrc EQ 0.
            WAIT UP TO 3 SECONDS.
          ENDIF.
        ENDIF.

        fiscal->estornar(
          EXPORTING
            i_doc_number = wa_zsdt0102-docnum    " Nº documento
            i_ref_type   = space
            i_ref_key    = space
            i_bapi_wait  = abap_true
          IMPORTING
            e_retorno    = DATA(e_retorno)    " Tabela de retorno
          RECEIVING
            r_gerou      = DATA(r_gerou)    " Campo de texto do comprimento 1
        ).

        IF r_gerou EQ abap_false.
          READ TABLE e_retorno INTO DATA(wa_retorno) INDEX 1.
          IF sy-subrc IS INITIAL.
            ROLLBACK WORK.
            MESSAGE ID wa_retorno-id TYPE 'E'
             NUMBER wa_retorno-number
               WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.
            RETURN.
          ELSE.
            ROLLBACK WORK.
            MESSAGE e043.
            RETURN.
          ENDIF.
        ENDIF.

      ENDIF.

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      wa_zsdt0102-estornado = 'X'.
      MODIFY zsdt0102 FROM wa_zsdt0102.

      IF sy-subrc = 0.
        COMMIT WORK.
        "MESSAGE 'Estorno realizado com sucesso!' TYPE 'S'.
        MESSAGE s042.
        RETURN.
      ELSE.
        ROLLBACK WORK.
        "MESSAGE 'Não foi possivel realizar o estorno!' TYPE 'E'.
        MESSAGE e043.
        RETURN.
      ENDIF.

    ELSE.
      ROLLBACK WORK.
      MESSAGE e043.
      "MESSAGE 'Não foi possivel realizar o estorno!' TYPE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD get_chave_mdfe.

    "Internal Tables and WorkAreas
    DATA: wa_act_nota  TYPE j_1bnfe_active.

    "Variables
    DATA: vl_chave      TYPE c LENGTH 44,
          var_tam       TYPE i,
          vl_msg_exibir TYPE string,
          vl_msg1       TYPE string,
          vl_serie(3)   TYPE c.

    DATA: wa_mdfe TYPE zsdt0102.

    CLEAR: wa_mdfe, wa_act_nota, vl_chave.

    CLEAR: at_wa_doc_mdfe.
    READ TABLE at_it_doc_mdfe INTO at_wa_doc_mdfe INDEX 1.

    SELECT SINGLE *
      INTO wa_act_nota
      FROM j_1bnfe_active
     WHERE docnum EQ at_wa_doc_mdfe-docnum.

    IF sy-subrc <> 0.
      "MESSAGE 'Não foi possível obter a chave do MDF-e!' TYPE 'S'.
      MESSAGE s034.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      INTO wa_mdfe
      FROM zsdt0102
     WHERE nmdfe  = me->at_nmdf
       AND docnum = me->at_docnum.

    IF sy-subrc <> 0.
      "MESSAGE 'Não foi possível obter a chave do MDF-e!' TYPE 'S'.
      MESSAGE s034.
      RETURN.
    ENDIF.

    CLEAR vl_serie.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_mdfe-serie
      IMPORTING
        output = vl_serie.

    CONCATENATE wa_act_nota-regio
                wa_mdfe-data_emi+2(2)
                wa_mdfe-data_emi+4(2)
                wa_act_nota-stcd1
                zif_doc_eletronico=>at_st_model_mdfe
                vl_serie
                wa_mdfe-nmdfe
                wa_mdfe-docnum9
                wa_mdfe-cdv INTO vl_chave.


    var_tam = strlen( vl_chave ). "Captura o Tamanho da Chave do MDF-e.

    IF ( var_tam < 44 ). "Verifica se a chave é menor que 44 caracteres.

      CLEAR: vl_msg_exibir, vl_msg1 .

      vl_msg1 = var_tam.
      "CONCATENATE 'Chave:' VL_CHAVE 'menor que 44 caracteres. TAM: ' VL_MSG1
      "       INTO VL_MSG_EXIBIR SEPARATED BY SPACE.
      "MESSAGE VL_MSG_EXIBIR TYPE 'S'.
      MESSAGE s035 WITH vl_chave vl_msg1.
      CLEAR e_chave.

      RETURN.
    ELSE.
      e_chave = vl_chave.
    ENDIF.


  ENDMETHOD.


  METHOD get_ck_autorizado.
    r_autorizado = me->at_autorizado.
  ENDMETHOD.


  METHOD get_cnpj_emi.

    DATA: wa_doc_item       TYPE j_1bnflin,
          wa_fatura_servico TYPE vbrp,
          wa_ordem_venda    TYPE vbak,
          wa_doc_transp     TYPE vttk,
          wa_lfa1           TYPE lfa1,
          wa_header         TYPE j_1bnfdoc.

    DATA: var_lifnr TYPE lfa1-lifnr,
          vl_cnpj   TYPE string.

    CLEAR: wa_header, wa_doc_item, wa_fatura_servico, wa_ordem_venda,
           wa_doc_transp, wa_lfa1, var_lifnr, vl_cnpj.

    SELECT SINGLE *
      INTO wa_header
      FROM j_1bnfdoc
     WHERE docnum EQ i_docnum.

    "Documento Normal
    SELECT SINGLE *
      INTO wa_doc_item
      FROM j_1bnflin
     WHERE docnum EQ i_docnum.

    "Fatura do Serviço
    SELECT SINGLE *
      INTO wa_fatura_servico
      FROM vbrp
     WHERE vbeln = wa_doc_item-refkey(10)
       AND posnr = wa_doc_item-refitm.

    "Ordem de Venda
    SELECT SINGLE *
      INTO wa_ordem_venda
      FROM vbak
     WHERE vbeln = wa_fatura_servico-aubel.

    "Cabeçalho Transporte
    SELECT SINGLE *
      INTO wa_doc_transp
      FROM vttk
     WHERE tknum = wa_ordem_venda-tknum.

    IF ( sy-subrc NE 0 ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_header-branch
        IMPORTING
          output = var_lifnr.

      SELECT SINGLE *
        INTO wa_lfa1
        FROM lfa1
       WHERE lifnr = var_lifnr.

    ELSE.
      SELECT SINGLE *
        INTO wa_lfa1
        FROM lfa1
       WHERE lifnr = wa_doc_transp-tdlnr.
    ENDIF.

    vl_cnpj = wa_lfa1-stcd1.

    e_cnpj_ide = vl_cnpj.

  ENDMETHOD.


  METHOD get_cod_um.

    "Types
    TYPES: BEGIN OF y_unidade,
             meins TYPE j_1bnflin-meins,
           END OF y_unidade.

    "Internal Table and WorkAreas
    DATA: tl_unidade    TYPE TABLE OF y_unidade,
          st_unidade    TYPE y_unidade,
          ti_item_p     TYPE TABLE OF j_1bnflin,
          st_item_p     TYPE j_1bnflin,
          it_notas_info TYPE TABLE OF zcte_info_nota,
          wa_notas_info TYPE zcte_info_nota.

    "Variables
    DATA: vl_cunid TYPE string.

    CLEAR: me->at_wa_doc_mdfe.
    READ TABLE me->at_it_doc_mdfe INTO me->at_wa_doc_mdfe INDEX 1.

    CASE me->at_modal .

      WHEN '01'. " Modal Rodoviario

        REFRESH: it_notas_info.
        CLEAR: wa_notas_info.

        SELECT *
         FROM zcte_info_nota
         INTO TABLE it_notas_info
        WHERE docnum = me->at_wa_doc_mdfe-docnum.

        READ TABLE it_notas_info INTO wa_notas_info INDEX 1.

        IF ( sy-subrc NE 0 ) OR
           ( wa_notas_info-unidade IS INITIAL ).

          CALL FUNCTION 'Z_SD_INFO_CTE_AVULSO'
            EXPORTING
              p_cte_avulso   = me->at_wa_doc_mdfe-docnum
              p_chamar_tela  = ''
              p_gravar_dados = ''
            TABLES
              it_notas_info  = it_notas_info.

          READ TABLE it_notas_info INTO wa_notas_info INDEX 1.

        ENDIF.

        CASE wa_notas_info-unidade.
          WHEN 'KG'.
            vl_cunid = '01'.
          WHEN 'TO'.
            vl_cunid = '02'.
          WHEN 'UN'.
            vl_cunid = '03'.
          WHEN 'LT'.
            vl_cunid = '04'.
        ENDCASE.

      WHEN '03'. " Modal Aquaviario



    ENDCASE.

    e_cod_um  = vl_cunid.

  ENDMETHOD.


  METHOD get_cunid.

    e_cunid = me->at_cunid.

  ENDMETHOD.


  METHOD get_data_emi.
    e_data = me->at_data_emi.
  ENDMETHOD.


  METHOD get_docnum.
    e_docnum = me->at_docnum.
  ENDMETHOD.


  METHOD get_email_avaliacao_enc.

    CLEAR: r_email.

    IF i_bukrs IS NOT INITIAL.

      SELECT SINGLE *
        FROM zmail INTO @DATA(lwa_email_enc)
       WHERE bukrs      EQ @i_bukrs
*        AND bukrs_ate  EQ @i_bukrs   "#141955-30.07.2024-VRIENZO
         AND bukrs      NE @space
*        AND bukrs_ate  NE @space     "#141955-30.07.2024-VRIENZO
         AND tcode      EQ 'ZMDFE_ENCERRAR'.

      IF sy-subrc EQ 0 AND lwa_email_enc-email IS NOT INITIAL.
        r_email = lwa_email_enc-email.
        TRANSLATE r_email TO LOWER CASE.
        RETURN.
      ENDIF.

    ENDIF.

    SELECT SINGLE *
      FROM zmail INTO lwa_email_enc
     WHERE bukrs      EQ space
*      AND bukrs_ate  EQ space  "#141955-30.07.2024-VRIENZO
       AND tcode      EQ 'ZMDFE_ENCERRAR'.

    IF sy-subrc EQ 0 AND lwa_email_enc-email IS NOT INITIAL.
      r_email = lwa_email_enc-email.
      TRANSLATE r_email TO LOWER CASE.
      RETURN.
    ENDIF.

    r_email = 'logistica.ov@amaggi.com.br'. "Email Padrão


  ENDMETHOD.


  METHOD get_hora_emi.
    e_hora = me->at_hora_emi.
  ENDMETHOD.


  METHOD get_ie_emi.

    DATA: wa_doc_item       TYPE j_1bnflin,
          wa_fatura_servico TYPE vbrp,
          wa_ordem_venda    TYPE vbak,
          wa_doc_transp     TYPE vttk,
          wa_lfa1           TYPE lfa1,
          wa_header         TYPE j_1bnfdoc,
          wa_zlest0061      TYPE zlest0061.

    DATA: var_lifnr TYPE lfa1-lifnr,
          vl_ie     TYPE string.

    CLEAR: wa_header, wa_doc_item, wa_fatura_servico, wa_ordem_venda,
           wa_doc_transp, wa_lfa1, var_lifnr, vl_ie.

    SELECT SINGLE *
      INTO wa_header
      FROM j_1bnfdoc
     WHERE docnum EQ i_docnum.

    "Documento Normal
    SELECT SINGLE *
      INTO wa_doc_item
      FROM j_1bnflin
     WHERE docnum EQ i_docnum.

    "Fatura do Serviço
    SELECT SINGLE *
      INTO wa_fatura_servico
      FROM vbrp
     WHERE vbeln = wa_doc_item-refkey(10)
       AND posnr = wa_doc_item-refitm.

    "Ordem de Venda
    SELECT SINGLE *
      INTO wa_ordem_venda
      FROM vbak
     WHERE vbeln = wa_fatura_servico-aubel.

    "Cabeçalho Transporte
    SELECT SINGLE *
      INTO wa_doc_transp
      FROM vttk
     WHERE tknum = wa_ordem_venda-tknum.

    IF ( sy-subrc NE 0 ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_header-branch
        IMPORTING
          output = var_lifnr.

      SELECT SINGLE *
        INTO wa_lfa1
        FROM lfa1
       WHERE lifnr = var_lifnr.

    ELSE.
      SELECT SINGLE *
        INTO wa_lfa1
        FROM lfa1
       WHERE lifnr = wa_doc_transp-tdlnr.
    ENDIF.

    vl_ie = wa_lfa1-stcd3.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN  vl_ie WITH ''.

    e_ie_ide = vl_ie.

  ENDMETHOD.


  METHOD get_just_canc.
    e_just = me->at_just_canc.

  ENDMETHOD.


  METHOD get_mdfes_encerramento.
  ENDMETHOD.


  METHOD get_mdfe_nao_encerrados.

    DATA: lit_zsdt0102     TYPE TABLE OF zsdt0102.
    DATA: lit_zsdt0102_manual     TYPE TABLE OF zsdt0102.

    CLEAR: r_mdfes[], lit_zsdt0102[].

    CHECK i_placas[] IS NOT INITIAL.

    "Busca MDF-e's Autorizados e não encerrados com mesmas Placas do MDF-e atual
    SELECT m~docnum m~nmdfe m~tp_doc_ref m~uffim m~authcode m~code
      INTO CORRESPONDING FIELDS OF TABLE lit_zsdt0102
      FROM zsdt0102 AS m
      INNER JOIN zsdt0105       AS d ON m~docnum = d~docnum_ref
      INNER JOIN zcte_parceiros AS p ON d~docnum = p~docnum
      INNER JOIN zcte_trans     AS t ON d~docnum = t~docnum
      FOR ALL ENTRIES IN i_placas
     WHERE m~encerrado  NE 'X'
       AND m~autorizado EQ 'X'
       AND m~cancel     NE 'X'
       AND m~url_sefaz  NE ''
       AND t~pc_veiculo EQ i_placas-pc_veiculo.

**"// BUG-160774 WBARBOSA
*    "Busca MDF-e's Autorizados "Manifesto Documento Eletrônico - Avulso - Veículos"
*    SELECT m~docnum m~nmdfe m~tp_doc_ref m~uffim m~authcode m~code
*      INTO CORRESPONDING FIELDS OF TABLE lit_zsdt0102
*      FROM zsdt0102 AS m
*      INNER JOIN zsdt0243       AS d ON m~docnum = d~docnum
*      FOR ALL ENTRIES IN i_placas
*     WHERE m~encerrado  NE 'X'
*       AND m~autorizado EQ 'X'
*       AND m~cancel     NE 'X'
*       AND m~url_sefaz  NE ''
*       AND d~pc_veiculo EQ i_placas-pc_veiculo.
**"// BUG-160774 WBARBOSA

    "MDF-e x NF-e
    SELECT m~docnum m~nmdfe m~tp_doc_ref  m~uffim m~authcode m~code
      APPENDING CORRESPONDING FIELDS OF TABLE lit_zsdt0102
      FROM zsdt0102 AS m INNER JOIN zsdt0118 AS d ON m~docnum = d~docnum
      FOR ALL ENTRIES IN i_placas
     WHERE m~encerrado  NE 'X'
       AND m~autorizado EQ 'X'
       AND m~cancel     NE 'X'
       AND m~url_sefaz  NE ''
       AND ( ( d~placa_cav  EQ i_placas-pc_veiculo ) OR
             ( d~placa_car1 EQ i_placas-pc_veiculo ) OR
             ( d~placa_car2 EQ i_placas-pc_veiculo ) OR
             ( d~placa_car3 EQ i_placas-pc_veiculo ) ).




    SORT lit_zsdt0102 BY docnum.
    "*-CONTINGENCIA MDF-E -  - 06.05.2024 =================================

    "DELETE lit_zsdt0102 where AUTHCODE is INITIAL.
    "*-CONTINGENCIA MDF-E -  - 06.05.2024 =================================

    DELETE ADJACENT DUPLICATES FROM lit_zsdt0102 COMPARING docnum.
    " DELETE ADJACENT DUPLICATES FROM lit_zsdt0102_MANUAL COMPARING DOCNUM.

    MOVE-CORRESPONDING lit_zsdt0102[] TO lit_zsdt0102_manual[].
    LOOP AT lit_zsdt0102 INTO DATA(lwa_zsdt0102).

      DATA(lva_tabix) = sy-tabix.

      "Verifica se documentos vinculados ao MDF-e encontrado para Encerramento, são os mesmos vinculado ao MDF-e de Autorização.
      DATA(lva_mesmos_docs_vinc) = me->check_mesmos_docs_vinc( i_docnum = lwa_zsdt0102-docnum ).

      IF lva_mesmos_docs_vinc EQ abap_true.
        DELETE lit_zsdt0102 INDEX lva_tabix.
      ENDIF.

    ENDLOOP.

    IF lit_zsdt0102 IS INITIAL.
      MOVE-CORRESPONDING lit_zsdt0102_manual[] TO r_mdfes[].
    ELSE.
      MOVE-CORRESPONDING lit_zsdt0102[] TO r_mdfes[].
    ENDIF.

  ENDMETHOD.


  METHOD get_modal.

    e_modal = me->at_modal.

  ENDMETHOD.


  METHOD get_modal_doc.

    DATA: wa_doc_item       TYPE j_1bnflin,
          wa_fatura_servico TYPE vbrp,
          wa_ordem_venda    TYPE vbak,
          wa_doc_transp     TYPE vttk,
          wa_header         TYPE j_1bnfdoc,
          vl_modal          TYPE string,
          wa_zlest0061      TYPE zlest0061,
          wa_identifica     TYPE zcte_identifica.

    CLEAR: wa_header, wa_doc_item, wa_fatura_servico, wa_ordem_venda,
           wa_doc_transp,vl_modal.

    "Verificar se o docnum esta gravado na tabela do aquaviário.
    SELECT SINGLE *
      INTO wa_zlest0061
      FROM zlest0061
     WHERE docnum EQ i_docnum.

    IF sy-subrc = 0.
      e_modal = '03'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      INTO wa_identifica
      FROM zcte_identifica
     WHERE docnum = i_docnum.

    e_modal = wa_identifica-modal.

*  SELECT SINGLE *
*    INTO WA_HEADER
*    FROM J_1BNFDOC
*   WHERE DOCNUM EQ I_DOCNUM.
*
*  "Documento Normal
*  SELECT SINGLE *
*    INTO WA_DOC_ITEM
*    FROM J_1BNFLIN
*   WHERE DOCNUM EQ I_DOCNUM.
*
*  "Fatura do Serviço
*  SELECT SINGLE *
*    INTO WA_FATURA_SERVICO
*    FROM VBRP
*   WHERE VBELN = WA_DOC_ITEM-REFKEY(10)
*     AND POSNR = WA_DOC_ITEM-REFITM.
*
*  "Ordem de Venda
*  SELECT SINGLE *
*    INTO WA_ORDEM_VENDA
*    FROM VBAK
*   WHERE VBELN = WA_FATURA_SERVICO-AUBEL.
*
*  "Cabeçalho Transporte
*  SELECT SINGLE *
*    INTO WA_DOC_TRANSP
*    FROM VTTK
*   WHERE TKNUM = WA_ORDEM_VENDA-TKNUM.
*
*  VL_MODAL = WA_DOC_TRANSP-VSART.
*
*  E_MODAL = VL_MODAL.


  ENDMETHOD.


  METHOD get_motivo_encerramento.

    DATA: lit_texto_motivo TYPE catsxt_longtext_itab.
    DATA: lva_motivo_enc TYPE zsdt0102-motivo_enc.

    CLEAR: r_motivo, lit_texto_motivo[].

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title = 'Motivo Encerramento MDF-e'
      CHANGING
        ch_text  = lit_texto_motivo.
    LOOP AT lit_texto_motivo INTO DATA(texto).
      CONCATENATE r_motivo texto INTO r_motivo SEPARATED BY space.
    ENDLOOP.

    IF strlen( r_motivo  ) < 15.
      CLEAR: r_motivo.
      MESSAGE 'Motivo do encerramento deve ter no minimo 15 caracteres!' TYPE 'I'.
      RETURN.
    ENDIF.


  ENDMETHOD.


  METHOD get_motorista.
    e_motorista = me->at_motorista.
  ENDMETHOD.


  METHOD get_mun_enc.

    DATA: wa_identifica TYPE zcte_identifica,
          vl_mun_enc    TYPE string.

    CLEAR: vl_mun_enc,wa_identifica.

    IF me->at_modal = '01'. " Modal Rodoviario

      SELECT SINGLE *
        INTO wa_identifica
        FROM zcte_identifica
       WHERE docnum = i_docnum.

      vl_mun_enc = wa_identifica-cmunfim.

    ENDIF.

    e_mun_enc = vl_mun_enc.

  ENDMETHOD.


  METHOD get_nmdf.
************************************
*  Método de Acesso
*  Atributo: AT_NMDF
*  Retorno: NDMF
*  Descrição: Método para retornar o número do MDF-e, sendo que este vai ser único.
*  Developer: Victor Hugo Souza Nunes
*  26.11.2015 10:52:24
************************************
    nmdf = me->at_nmdf.
  ENDMETHOD.


  METHOD get_parceiro.

    SELECT SINGLE * INTO @DATA(wa_act_nota)
      FROM j_1bnfe_active
     WHERE docnum EQ @i_docnum.

    CHECK sy-subrc IS INITIAL.

    CASE wa_act_nota-model.
      WHEN zif_doc_eletronico=>at_st_model_nfe.

        SELECT * INTO TABLE @DATA(it_j_1bnfnad)
          FROM j_1bnfnad
         WHERE docnum EQ @i_docnum
           AND parvw  IN ('PC','LR').

        "Fornecedor (Ponto de Coleta)
        READ TABLE it_j_1bnfnad INTO DATA(wa_j_1bnfnad) WITH KEY parvw = 'PC'.
        IF sy-subrc IS INITIAL.
          e_id_local_coleta = wa_j_1bnfnad-parid.
        ENDIF.

        "Cliente (Ponto de Entrega)
        READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad WITH KEY parvw = 'LR'.
        IF sy-subrc IS INITIAL.
          e_id_local_descarga = wa_j_1bnfnad-parid.
        ENDIF.

      WHEN zif_doc_eletronico=>at_st_model_cte.

        SELECT * INTO TABLE @it_j_1bnfnad
          FROM j_1bnfnad
         WHERE docnum EQ @i_docnum
           AND parvw  IN ('T2','T3').

        "Fornecedor (Ponto de Coleta)
        READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad WITH KEY parvw = 'T2'.
        IF sy-subrc IS INITIAL.
          e_id_local_coleta = wa_j_1bnfnad-parid.
        ENDIF.

        "Cliente (Ponto de Entrega)
        READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad WITH KEY parvw = 'T3'.
        IF sy-subrc IS INITIAL.
          e_id_local_descarga = wa_j_1bnfnad-parid.
        ENDIF.

    ENDCASE.



  ENDMETHOD.


  METHOD get_parceiros_documento.

    DATA: it_vbpa          TYPE TABLE OF vbpa,
          wl_vbpa          TYPE vbpa,

          it_j_1bad        TYPE TABLE OF j_1bad,
          wl_j_1bad        TYPE j_1bad,

          it_j_1bnfnad     TYPE j_1bnfnad_tab,
          wl_j_1bnfnad     LIKE LINE OF it_j_1bnfnad,


          it_j_1bnfnad_doc TYPE TABLE OF j_1bnfnad,
          wl_j_1bnfnad_doc TYPE j_1bnfnad,


          it_zfiwrt0015    TYPE TABLE OF zfiwrt0015,
          wl_zfiwrt0015    TYPE zfiwrt0015,

          wl_j_1binnad     TYPE j_1binnad,

          ls_ttxd          TYPE ttxd,

          wl_lfa1_br       TYPE lfa1-lifnr.

    CLEAR: r_j_1bnfnad_tab[], it_j_1bnfnad[], it_vbpa[], it_j_1bad[], it_zfiwrt0015[], it_j_1bnfnad_doc[].

    ls_ttxd-leng1 = '03'.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(_wl_lin)
     WHERE docnum EQ @i_docnum.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(_wl_doc)
     WHERE docnum EQ @i_docnum.

    SELECT *
      FROM j_1bad INTO TABLE it_j_1bad.

    CASE _wl_lin-reftyp.
      WHEN 'BI'.

        SELECT SINGLE *
          FROM vbrp INTO @DATA(_wl_vbrp)
         WHERE vbeln EQ @_wl_lin-refkey(10).

        CHECK sy-subrc EQ 0.

        SELECT SINGLE *
          FROM likp INTO @DATA(_wl_likp)
         WHERE vbeln EQ @_wl_vbrp-vgbel.

        CHECK sy-subrc EQ 0.

        SELECT *
          FROM vbpa INTO TABLE it_vbpa
         WHERE vbeln EQ _wl_likp-vbeln.

        LOOP AT it_vbpa INTO wl_vbpa.

          CLEAR: wl_j_1bnfnad.

          READ TABLE it_j_1bad INTO wl_j_1bad WITH KEY parvw = wl_vbpa-parvw.

          CHECK sy-subrc EQ 0.

          wl_j_1bnfnad-docnum = i_docnum.
          wl_j_1bnfnad-parvw  = wl_j_1bad-parvw.
          wl_j_1bnfnad-partyp = wl_j_1bad-partyp.

          CASE wl_j_1bnfnad-partyp .
            WHEN 'V'.
              wl_j_1bnfnad-parid = wl_vbpa-lifnr.
            WHEN 'C'.
              wl_j_1bnfnad-parid = wl_vbpa-kunnr.
          ENDCASE.

          APPEND wl_j_1bnfnad TO it_j_1bnfnad.

        ENDLOOP.

      WHEN 'LI'.

        IF _wl_doc-entrad EQ abap_true.

          "Ponto de Coleta
          wl_j_1bnfnad-docnum = _wl_doc-docnum.
          wl_j_1bnfnad-parvw  = 'PC'.
          wl_j_1bnfnad-partyp = _wl_doc-partyp.
          wl_j_1bnfnad-parid  = _wl_doc-parid.
          APPEND wl_j_1bnfnad TO it_j_1bnfnad.

          "Local de Entrega
          wl_j_1bnfnad-docnum = _wl_doc-docnum.
          wl_j_1bnfnad-parvw  = 'LR'.
          wl_j_1bnfnad-partyp = 'C'.
          wl_j_1bnfnad-parid  = |{ _wl_lin-werks ALPHA = IN }|.
          APPEND wl_j_1bnfnad TO it_j_1bnfnad.

        ENDIF.


      WHEN 'ZW'.

        SELECT SINGLE *
          FROM zfiwrt0008 INTO @DATA(_wl_zfiwrt0008)
         WHERE seq_lcto EQ @_wl_lin-refkey(10).

        CHECK ( sy-subrc EQ 0 ).

        SELECT *
          FROM zfiwrt0015 INTO TABLE it_zfiwrt0015
         WHERE seq_lcto EQ _wl_zfiwrt0008-seq_lcto
           AND parvw    IN ('PC', 'LR').

        LOOP AT it_zfiwrt0015 INTO wl_zfiwrt0015.
          CLEAR: wl_j_1bnfnad.

          wl_j_1bnfnad-docnum = i_docnum.
          wl_j_1bnfnad-parvw  = wl_zfiwrt0015-parvw.

          CASE wl_j_1bnfnad-parvw.
            WHEN 'PC'.
              wl_j_1bnfnad-partyp = 'V'.
            WHEN 'LR'.
              wl_j_1bnfnad-partyp = 'C'.
            WHEN OTHERS.
              CONTINUE.
          ENDCASE.

          wl_j_1bnfnad-parid = wl_zfiwrt0015-parid.

          APPEND wl_j_1bnfnad TO it_j_1bnfnad.
        ENDLOOP.

      WHEN OTHERS.

        SELECT *
          FROM j_1bnfnad INTO TABLE it_j_1bnfnad_doc
         WHERE docnum EQ i_docnum.

        LOOP AT it_j_1bnfnad_doc INTO wl_j_1bnfnad_doc.
          CLEAR: wl_j_1bnfnad.

          CHECK wl_j_1bnfnad_doc-parvw EQ 'PC' OR wl_j_1bnfnad_doc-parvw EQ 'LR'.

          wl_j_1bnfnad-docnum = i_docnum.
          wl_j_1bnfnad-parvw  = wl_j_1bnfnad_doc-parvw.
          wl_j_1bnfnad-partyp = wl_j_1bnfnad_doc-partyp.
          wl_j_1bnfnad-parid  = wl_j_1bnfnad_doc-parid.
          APPEND wl_j_1bnfnad TO it_j_1bnfnad.

        ENDLOOP.

    ENDCASE.

    r_j_1bnfnad_tab = it_j_1bnfnad[].


  ENDMETHOD.


  METHOD get_placas_mdfe.

    DATA: lit_zcte_trans TYPE TABLE OF zcte_trans,
          lit_zsdt0243   TYPE TABLE OF zsdt0243.

    CLEAR: r_placas[], lit_zcte_trans[] , lit_zsdt0243[].

    IF me->at_it_doc_mdfe[] IS NOT INITIAL.
      SELECT *
        FROM zcte_trans INTO TABLE lit_zcte_trans
         FOR ALL ENTRIES IN me->at_it_doc_mdfe
       WHERE docnum = me->at_it_doc_mdfe-docnum.

      LOOP AT lit_zcte_trans INTO DATA(lwa_cte_trans).
        APPEND VALUE #( pc_veiculo = lwa_cte_trans-pc_veiculo ) TO r_placas.
      ENDLOOP.
    ENDIF.

    APPEND VALUE #( pc_veiculo = me->at_placa_cav  ) TO r_placas.
    APPEND VALUE #( pc_veiculo = me->at_placa_car1 ) TO r_placas.
    APPEND VALUE #( pc_veiculo = me->at_placa_car2 ) TO r_placas.
    APPEND VALUE #( pc_veiculo = me->at_placa_car3 ) TO r_placas.

    SELECT *
      FROM zsdt0243 INTO TABLE lit_zsdt0243
     WHERE docnum EQ me->at_docnum.

    LOOP AT lit_zsdt0243 INTO DATA(wa_zsdt0243).
      APPEND VALUE #( pc_veiculo = wa_zsdt0243-pc_veiculo ) TO r_placas.
    ENDLOOP.

    DELETE r_placas WHERE pc_veiculo IS INITIAL.
    SORT r_placas BY pc_veiculo.
    DELETE ADJACENT DUPLICATES FROM r_placas COMPARING pc_veiculo.

  ENDMETHOD.


  METHOD get_placas_mdfe_by_docnum.

    DATA: lit_zcte_trans TYPE TABLE OF zcte_trans,
          lit_zsdt0105   TYPE TABLE OF zsdt0105,
          lit_zsdt0243   TYPE TABLE OF zsdt0243.

    CLEAR: r_placas[], lit_zcte_trans[] , lit_zsdt0243[], lit_zsdt0105[].

    SELECT *
      FROM zsdt0105 INTO TABLE lit_zsdt0105
     WHERE docnum_ref EQ i_docnum.

    IF lit_zsdt0105[] IS NOT INITIAL.
      SELECT *
       FROM zcte_trans APPENDING TABLE lit_zcte_trans
        FOR ALL ENTRIES IN lit_zsdt0105
      WHERE docnum = lit_zsdt0105-docnum.
    ENDIF.

    LOOP AT lit_zcte_trans INTO DATA(lwa_cte_trans).
      APPEND VALUE #( pc_veiculo = lwa_cte_trans-pc_veiculo ) TO r_placas.
    ENDLOOP.

    SELECT SINGLE *
      FROM zsdt0118 INTO @DATA(lwa_zsdt0118)
     WHERE docnum = @i_docnum.

    IF sy-subrc = 0.
      APPEND VALUE #( pc_veiculo = lwa_zsdt0118-placa_cav  ) TO r_placas.
      APPEND VALUE #( pc_veiculo = lwa_zsdt0118-placa_car1 ) TO r_placas.
      APPEND VALUE #( pc_veiculo = lwa_zsdt0118-placa_car2 ) TO r_placas.
      APPEND VALUE #( pc_veiculo = lwa_zsdt0118-placa_car3 ) TO r_placas.
    ENDIF.

    SELECT *
      FROM zsdt0243 INTO TABLE lit_zsdt0243
     WHERE docnum EQ i_docnum.

    LOOP AT lit_zsdt0243 INTO DATA(wa_zsdt0243).
      APPEND VALUE #( pc_veiculo = wa_zsdt0243-pc_veiculo ) TO r_placas.
    ENDLOOP.

    DELETE r_placas WHERE pc_veiculo IS INITIAL.
    SORT r_placas BY pc_veiculo.
    DELETE ADJACENT DUPLICATES FROM r_placas COMPARING pc_veiculo.


  ENDMETHOD.


  METHOD get_placa_car1.
    e_placa_car1 = me->at_placa_car1.
  ENDMETHOD.


  METHOD get_placa_car2.
    e_placa_car2 = me->at_placa_car2.
  ENDMETHOD.


  METHOD get_placa_car3.
    e_placa_car3 = me->at_placa_car3.
  ENDMETHOD.


  METHOD get_placa_cav.
    e_placa_cav = me->at_placa_cav.
  ENDMETHOD.


  METHOD get_prodpred_doc_vinculados.

    TYPES BEGIN OF ty_volumes.
    TYPES: matnr TYPE matnr.
    TYPES: qtd   TYPE j_1bnetqty.
    TYPES END OF ty_volumes.

    DATA: it_volumes TYPE TABLE OF ty_volumes,
          wa_volumes TYPE ty_volumes.

    DATA: rgparvw      TYPE RANGE OF parvw.

    rgparvw = VALUE #( option = 'EQ' sign = 'I' ( low = 'PC' ) ( low = 'LR' ) ).

    CLEAR: r_prod_produminante, it_volumes, it_volumes[], wa_volumes.

    IF i_zsdt0237 IS NOT INITIAL.
      r_prod_produminante-matnr          = i_zsdt0237-matnr.
      r_prod_produminante-matkl          = i_zsdt0237-matkl.
      r_prod_produminante-tpcarga        = i_zsdt0237-tpcarga.
      r_prod_produminante-xprod          = i_zsdt0237-xprod.
      r_prod_produminante-locd_cep       = i_zsdt0237-locd_cep.
      r_prod_produminante-locd_latitude  = i_zsdt0237-locd_latitude.
      r_prod_produminante-locd_longitude = i_zsdt0237-locd_longitude.
      r_prod_produminante-locc_cep       = i_zsdt0237-locc_cep.
      r_prod_produminante-locc_latitude  = i_zsdt0237-locc_latitude.
      r_prod_produminante-locc_longitude = i_zsdt0237-locc_longitude.
    ELSE.

      LOOP AT at_it_doc_mdfe INTO DATA(wa_it_doc_mdfe).

        SELECT SINGLE * INTO @DATA(wa_j1bnfdoc)
          FROM j_1bnfdoc
         WHERE docnum EQ @wa_it_doc_mdfe-docnum.

        CASE wa_j1bnfdoc-model.
          WHEN zif_doc_eletronico=>at_st_model_nfe OR '01'.

            SELECT * APPENDING TABLE @DATA(it_j_1bnflin)
              FROM j_1bnflin
             WHERE docnum EQ @wa_j1bnfdoc-docnum.

            READ TABLE it_j_1bnflin INDEX 1 INTO DATA(wa_j_1bnflin).

            " Determinação de Latitude e Longitude e CEP """""""""""""""""""""""""""""""""""""""""""""""""
            CASE wa_j_1bnflin-reftyp.
              WHEN 'MD' OR 'BI'. "Documento de Mercadoria "Lançamento por Faturamento
                SELECT * INTO TABLE @DATA(it_j_1bnfnad) FROM j_1bnfnad WHERE docnum EQ @wa_j_1bnflin-docnum AND parvw IN @rgparvw.
              WHEN 'LI'. "Entrada de Fatura

                "Fornecedor da Mercadoria
                APPEND VALUE #( parvw = 'PC' partyp = 'V' parid = wa_j1bnfdoc-parid ) TO it_j_1bnfnad.
                "Filial de Entrada da Mercadoria
                APPEND VALUE #( parvw = 'LR' partyp = 'C' parid = zcl_string=>lpad( i_str = CONV #( wa_j1bnfdoc-branch ) i_qtd = 10 i_char = '0' )  ) TO it_j_1bnfnad.

              WHEN 'ZW'. "Lançamento Avulso

                SELECT SINGLE * INTO @DATA(wa_zfiwrt0008)
                  FROM zfiwrt0008
                 WHERE docnum EQ @wa_j1bnfdoc-docnum.

                IF sy-subrc IS INITIAL.
                  SELECT * INTO TABLE @DATA(it_zfiwrt0015) FROM zfiwrt0015 WHERE seq_lcto EQ @wa_zfiwrt0008-seq_lcto AND parvw IN @rgparvw.
                ENDIF.

                LOOP AT it_zfiwrt0015 INTO DATA(wa_zfiwrt0015).
                  CASE wa_zfiwrt0015-parvw.
                    WHEN 'LR'.
                      APPEND VALUE #( parvw = wa_zfiwrt0015-parvw partyp = 'C' parid = wa_zfiwrt0015-parid ) TO it_j_1bnfnad.
                    WHEN 'PC'.
                      APPEND VALUE #( parvw = wa_zfiwrt0015-parvw partyp = 'V' parid = wa_zfiwrt0015-parid ) TO it_j_1bnfnad.
                  ENDCASE.
                ENDLOOP.
            ENDCASE.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          WHEN zif_doc_eletronico=>at_st_model_cte.

            SELECT * APPENDING TABLE @DATA(it_zlest0061)
              FROM zlest0061
             WHERE docnum EQ @wa_j1bnfdoc-docnum.

            IF sy-subrc IS INITIAL.

              SELECT * INTO TABLE @it_j_1bnfnad
                FROM j_1bnfnad
               WHERE docnum EQ @wa_j1bnfdoc-docnum.

              LOOP AT it_j_1bnfnad INTO DATA(wa_j_1bnfnad).
                CASE wa_j_1bnfnad-parvw.
                  WHEN 'T2'.
                    APPEND VALUE #( parvw = 'PC' partyp = 'V' parid = wa_j_1bnfnad-parid ) TO it_j_1bnfnad.
                  WHEN 'T3'.
                    APPEND VALUE #( parvw = 'LR' partyp = 'C' parid = wa_j_1bnfnad-parid ) TO it_j_1bnfnad.
                ENDCASE.
              ENDLOOP.

            ELSE.

              SELECT * APPENDING TABLE @DATA(it_info_nota)
                FROM zcte_info_nota
               WHERE docnum EQ @wa_j1bnfdoc-docnum.

              SELECT SINGLE * INTO @DATA(wa_zcte_identifica)
                FROM zcte_identifica
               WHERE docnum EQ @wa_j1bnfdoc-docnum.

              IF sy-subrc IS INITIAL AND wa_zcte_identifica-tknum IS NOT INITIAL.

                SELECT * INTO TABLE @DATA(it_vtpa)
                  FROM vtpa
                 WHERE vbeln EQ @wa_zcte_identifica-tknum
                   AND parvw IN @rgparvw.

                LOOP AT it_vtpa INTO DATA(wa_vtpa).
                  CASE wa_vtpa-parvw.
                    WHEN 'LR'.
                      APPEND VALUE #( parvw = wa_vtpa-parvw partyp = 'C' parid = wa_vtpa-kunnr ) TO it_j_1bnfnad.
                    WHEN 'PC'.
                      APPEND VALUE #( parvw = wa_vtpa-parvw partyp = 'V' parid = wa_vtpa-lifnr ) TO it_j_1bnfnad.
                  ENDCASE.
                ENDLOOP.

              ENDIF.

            ENDIF.

          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_mdfe
              EXPORTING
                textid = VALUE #( msgid = zcx_mdfe=>zcx_modelo_not_prev-msgid
                                  msgno = zcx_mdfe=>zcx_modelo_not_prev-msgno
                                  attr1 = wa_j1bnfdoc-model )
                msgty  = 'E'
                msgid  = zcx_mdfe=>zcx_modelo_not_prev-msgid
                msgno  = zcx_mdfe=>zcx_modelo_not_prev-msgno
                msgv1  = CONV #( wa_j1bnfdoc-model ).
        ENDCASE.

      ENDLOOP.

      LOOP AT it_j_1bnflin INTO wa_j_1bnflin WHERE matkl IS NOT INITIAL.
        CLEAR: wa_volumes.
        wa_volumes-matnr = wa_j_1bnflin-matnr.
        wa_volumes-qtd   = 0.
        APPEND wa_volumes TO it_volumes.
      ENDLOOP.

      LOOP AT it_info_nota INTO DATA(wa_info_nota).
        wa_volumes-matnr = wa_info_nota-material.
        wa_volumes-qtd   = 0.
        APPEND wa_volumes TO it_volumes.
      ENDLOOP.

      LOOP AT it_zlest0061 INTO DATA(wa_zlest0061).
        wa_volumes-matnr = wa_zlest0061-cod_material.
        wa_volumes-qtd   = 0.
        APPEND wa_volumes TO it_volumes.
      ENDLOOP.

      SORT it_volumes BY matnr.
      DELETE ADJACENT DUPLICATES FROM it_volumes COMPARING matnr.

      LOOP AT it_volumes ASSIGNING FIELD-SYMBOL(<volume>).
        <volume>-qtd = 0.
        LOOP AT it_j_1bnflin INTO wa_j_1bnflin WHERE matnr EQ <volume>-matnr.
          ADD wa_j_1bnflin-menge TO <volume>-qtd.
        ENDLOOP.

        LOOP AT it_info_nota INTO wa_info_nota WHERE material EQ <volume>-matnr.
          ADD wa_info_nota-quantidade TO <volume>-qtd.
        ENDLOOP.

        LOOP AT it_zlest0061 INTO wa_zlest0061 WHERE cod_material EQ <volume>-matnr.
          ADD wa_zlest0061-peso_vinculado	TO <volume>-qtd.
        ENDLOOP.

      ENDLOOP.

      "Tipo de Carga ZLEST0193
      SORT it_volumes BY qtd DESCENDING.
      READ TABLE it_volumes INDEX 1 INTO wa_volumes.

      r_prod_produminante-matnr = wa_volumes-matnr.

      SELECT SINGLE * INTO @DATA(wa_mara)
        FROM mara
       WHERE matnr EQ @wa_volumes-matnr.

      r_prod_produminante-matkl = wa_mara-matkl.

      LOOP AT it_j_1bnfnad INTO wa_j_1bnfnad.

        CASE wa_j_1bnfnad-parvw.
          WHEN 'LR'.

            SELECT SINGLE lzone, adrnr, land1 INTO @DATA(wa_kna1)
              FROM kna1
             WHERE kunnr EQ @wa_j_1bnfnad-parid.

            SELECT SINGLE * INTO @DATA(wa_adrc)
              FROM adrc
             WHERE addrnumber EQ @wa_kna1-adrnr.

            SELECT SINGLE * INTO @DATA(wa_tzone)
              FROM tzone
             WHERE land1 EQ @wa_kna1-land1
               AND zone1 EQ @wa_kna1-lzone.

            DATA(lc_cep) = COND string( WHEN wa_tzone-zcep IS NOT INITIAL THEN wa_tzone-zcep ELSE wa_adrc-post_code1 ).

            r_prod_produminante-locd_cep = zcl_string=>replace( i_str = lc_cep i_char_old   = '-' i_char_new   = '' ).
            r_prod_produminante-locd_latitude  = wa_tzone-zlatitude.
            r_prod_produminante-locd_longitude = wa_tzone-zlongitude.

            CLEAR: wa_adrc, wa_tzone.

          WHEN 'PC'.

            CLEAR: wa_adrc, wa_tzone.

            SELECT SINGLE lzone, adrnr, land1 INTO @DATA(wa_lfa1)
              FROM lfa1
             WHERE lifnr EQ @wa_j_1bnfnad-parid.

            SELECT SINGLE * INTO @wa_adrc
              FROM adrc
             WHERE addrnumber EQ @wa_lfa1-adrnr.

            SELECT SINGLE * INTO @wa_tzone
              FROM tzone
             WHERE land1 EQ @wa_lfa1-land1
               AND zone1 EQ @wa_lfa1-lzone.

            lc_cep = COND string( WHEN wa_tzone-zcep IS NOT INITIAL THEN wa_tzone-zcep ELSE wa_adrc-post_code1 ).

            r_prod_produminante-locc_cep = zcl_string=>replace( i_str = lc_cep i_char_old   = '-' i_char_new   = '' ).
            r_prod_produminante-locc_latitude  = wa_tzone-zlatitude.
            r_prod_produminante-locc_longitude = wa_tzone-zlongitude.

        ENDCASE.

      ENDLOOP.

    ENDIF.

    IF r_prod_produminante-matnr IS INITIAL.
      r_prod_produminante-matnr = me->at_prodpred-matnr.
    ENDIF.

    IF r_prod_produminante-matkl IS INITIAL.
      r_prod_produminante-matkl = me->at_prodpred-matkl.
    ENDIF.

    IF r_prod_produminante-tpcarga IS INITIAL.
      r_prod_produminante-tpcarga = me->at_prodpred-tpcarga.
    ENDIF.

    IF r_prod_produminante-xprod IS INITIAL.
      r_prod_produminante-xprod = me->at_prodpred-xprod.
    ENDIF.

    IF r_prod_produminante-locd_cep IS INITIAL.
      r_prod_produminante-locd_cep = me->at_prodpred-locd_cep.
    ENDIF.

    IF r_prod_produminante-locd_latitude IS INITIAL.
      r_prod_produminante-locd_latitude = me->at_prodpred-locd_latitude.
    ENDIF.

    IF r_prod_produminante-locd_longitude IS INITIAL.
      r_prod_produminante-locd_longitude = me->at_prodpred-locd_longitude.
    ENDIF.

    IF r_prod_produminante-locc_cep IS INITIAL.
      r_prod_produminante-locc_cep = me->at_prodpred-locc_cep.
    ENDIF.

    IF r_prod_produminante-locc_latitude IS INITIAL.
      r_prod_produminante-locc_latitude = me->at_prodpred-locc_latitude.
    ENDIF.

    IF r_prod_produminante-locc_longitude IS INITIAL.
      r_prod_produminante-locc_longitude = me->at_prodpred-locc_longitude.
    ENDIF.

    IF r_prod_produminante-matnr IS INITIAL OR r_prod_produminante-matkl IS INITIAL.
      RAISE EXCEPTION TYPE zcx_mdfe
        EXPORTING
          textid = VALUE #( msgid = zcx_mdfe=>zcx_grupo_merc_not_info-msgid
                            msgno = zcx_mdfe=>zcx_grupo_merc_not_info-msgno )
          msgty  = 'E'
          msgno  = zcx_mdfe=>zcx_grupo_merc_not_info-msgno
          msgid  = zcx_mdfe=>zcx_grupo_merc_not_info-msgid.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_zlest0193)
      FROM zlest0193
     WHERE matkl EQ @r_prod_produminante-matkl.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_mdfe
        EXPORTING
          textid    = VALUE #( msgid = zcx_mdfe=>zcx_grupo_merc_not_para-msgid
                               msgno = zcx_mdfe=>zcx_grupo_merc_not_para-msgno
                               attr1 = r_prod_produminante-matkl )
          msgty     = 'E'
          msgid     = zcx_mdfe=>zcx_grupo_merc_not_para-msgid
          msgno     = zcx_mdfe=>zcx_grupo_merc_not_para-msgno
          msgv1     = CONV #( r_prod_produminante-matkl )
          transacao = 'ZLES0098'.
    ENDIF.

    r_prod_produminante-tpcarga = zcl_string=>lpad( i_str  = CONV #( wa_zlest0193-tipo_carga ) i_qtd  = 2 i_char = '0' ) .

    SELECT SINGLE maktx INTO @r_prod_produminante-xprod
      FROM makt
     WHERE matnr EQ @r_prod_produminante-matnr
       AND spras EQ @sy-langu.

    IF ( r_prod_produminante-locc_cep IS INITIAL ) AND
       ( r_prod_produminante-locc_latitude IS INITIAL OR r_prod_produminante-locc_longitude IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_mdfe
        EXPORTING
          textid    = VALUE #( msgid = zcx_mdfe=>zcx_local_carrega-msgid
                               msgno = zcx_mdfe=>zcx_local_carrega-msgno )
          msgty     = 'E'
          msgid     = zcx_mdfe=>zcx_local_carrega-msgid
          msgno     = zcx_mdfe=>zcx_local_carrega-msgno
          transacao = 'ZOVR1'.
    ENDIF.

    IF ( r_prod_produminante-locd_cep IS INITIAL ) AND
       ( r_prod_produminante-locd_latitude IS INITIAL OR r_prod_produminante-locd_longitude IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_mdfe
        EXPORTING
          textid    = VALUE #( msgid = zcx_mdfe=>zcx_local_descarrega-msgid
                               msgno = zcx_mdfe=>zcx_local_descarrega-msgno )
          msgty     = 'E'
          msgid     = zcx_mdfe=>zcx_local_descarrega-msgid
          msgno     = zcx_mdfe=>zcx_local_descarrega-msgno
          transacao = 'ZOVR1'.
    ENDIF.

  ENDMETHOD.


  METHOD get_qcarga.

    e_qcarga = me->at_qcarga.

  ENDMETHOD.


  METHOD get_serie.
************************************
*  Método de Acesso
*  Atributo: AT_SERIE
*  Retorno:  SERIE
*  Descrição: Método para retornar a serie da MDF-e.
*  Developer: Victor Hugo Souza Nunes
*  26.11.2015 11:00:41
************************************
    serie = me->at_serie.
  ENDMETHOD.


  METHOD get_tp_doc_ref.
    e_tp_doc_ref = me->at_tp_doc_ref.
  ENDMETHOD.


  METHOD get_ufs_percurso.

    CHECK i_id_local_coleta   IS NOT INITIAL AND
          i_id_local_descarga IS NOT INITIAL.

    SELECT SINGLE a~* INTO @DATA(wa_zsdt0102)
      FROM zsdt0102 AS a
     WHERE a~cancel            EQ @abap_false
       AND a~estornado         EQ @abap_false
       AND a~autorizado        EQ @abap_true
       AND a~id_local_coleta   EQ @i_id_local_coleta
       AND a~id_local_descarga EQ @i_id_local_descarga
       AND a~nmdfe             EQ ( SELECT MAX( b~nmdfe )
                                      FROM zsdt0102 AS b
                                     WHERE b~cancel            EQ @abap_false
                                       AND b~estornado         EQ @abap_false
                                       AND b~autorizado        EQ @abap_true
                                       AND b~id_local_coleta   EQ a~id_local_coleta
                                       AND b~id_local_descarga EQ a~id_local_descarga ).

    CHECK sy-subrc IS INITIAL.

    SELECT * INTO TABLE r_ufs
      FROM zsdt0104
     WHERE nmdfe EQ wa_zsdt0102-nmdfe
     ORDER BY ordem_uf.

  ENDMETHOD.


  METHOD gravar_mdfe.

    "Internal Tables and Work Areas. ---------------------------

*-#133089-21.02.2024-JT-inicio
    DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico.
    IF i_faturamento_autom = abap_true.
      CREATE OBJECT lc_faturamento_automatico.
    ENDIF.
*-#133089-21.02.2024-JT-fim

    "Dados MDF-e
    DATA: lit_zsdt0102_encerrar TYPE zsdt0102_t.

* UFs Percurso.
    DATA: it_zsdt0104 TYPE TABLE OF zsdt0104,
          wa_zsdt0104 TYPE zsdt0104.

* Documentos Vinculados
    DATA: it_zsdt0105 TYPE TABLE OF zsdt0105,
          wa_zsdt0105 TYPE zsdt0105.

* Dados Transporte Manual - MDF-e x NF-e
    DATA: it_zsdt0118 TYPE TABLE OF zsdt0118,
          wa_zsdt0118 TYPE zsdt0118.

    DATA: it_j_1baa             TYPE TABLE OF j_1baa,
          wa_j_1baa             TYPE j_1baa,
          it_j_1bb2             TYPE TABLE OF j_1bb2,
          wa_j_1bb2             TYPE j_1bb2,
          wa_j1bnfdoc           TYPE j_1bnfdoc,
          wa_act_nota           TYPE j_1bnfe_active,
          wa_zsdt0001           TYPE zsdt0001,
          wl_zlest0061          TYPE zlest0061,
          lwa_doc_mdfe_encerrar TYPE j_1bnfdoc,
          it_zsdt0105_enc       TYPE TABLE OF zsdt0105,
          wa_zsdt0105_enc       TYPE zsdt0105,
          t_romaneios           TYPE zsdt0001_t.

    "Variables. -------------------------------------------------
    DATA: vl_nmdfe                      TYPE j_1bnfdoc-docnum,
          var_answer                    TYPE c,
          vl_msg_exibir                 TYPE string,
          vl_msg1                       TYPE string,
          vl_msg2                       TYPE string,
          vl_ie_emi                     TYPE string,
          vl_cnpj_emi                   TYPE string,
          vl_ie_emi_aux                 TYPE string,
          vl_cnpj_emi_aux               TYPE string,
          vl_modal                      TYPE string,
          vl_modal_aux                  TYPE string,
          vl_branch_mdfe_completa_carga TYPE j_1bbranch-branch,
          vl_idx_doc                    TYPE sy-tabix,
          lv_nr_range                   TYPE nrnr,
          lv_object                     TYPE nrobj,
          lv_nr_mdfe                    TYPE j_1bnfnum9,
          lv_idx_uf                     TYPE i,
          vl_mdfe_completa_carga        TYPE c.

    CLEAR: e_docnum_sol_enc.

    "Faturamento Contingencia - Ini
    DATA: lwa_romaneio  TYPE zsdt0001.
    DATA: lwa_zlest0108 TYPE zlest0108.
    DATA: lwa_faturamento_ecc TYPE zde_compare_faturamento.

    DATA(_docnumento_emitido_ecc) = abap_false.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    DATA: lwa_contingencia_mdfe        TYPE char01.

    DATA(l_branch) = COND #( WHEN at_wa_doc_mdfe-branch IS INITIAL THEN i_zsdt0237-branch
                                                                   ELSE at_wa_doc_mdfe-branch ).
    lwa_contingencia_mdfe = me->check_contingencia_mdfe( l_branch ).
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarvc_user_cont)
     WHERE name = 'FAT_CONTINGENCIA_GOLIVE_US'
       AND low = @sy-uname.

    LOOP AT at_it_doc_mdfe INTO at_wa_doc_mdfe.
      CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0003'
        EXPORTING
          i_docnum    = at_wa_doc_mdfe-docnum
        IMPORTING
          e_zsdt0001  = lwa_romaneio
          e_zlest0108 = lwa_zlest0108.

      IF lwa_romaneio-fat_contingencia_ecc EQ abap_true.

        IF lwa_tvarvc_user_cont IS INITIAL.
          MESSAGE 'Usuario ao permitido! Documento Emitido em contingencia no ECC' TYPE 'E'.
          RETURN.
        ENDIF.
        _docnumento_emitido_ecc = abap_true.



        CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
          EXPORTING
            i_ch_referencia         = lwa_romaneio-ch_referencia
            i_get_dados_fat_ecc     = abap_true
          IMPORTING
            e_dados_faturamento_ecc = lwa_faturamento_ecc.

        IF lwa_faturamento_ecc-data_lcto_mdfe IS NOT INITIAL.
          me->at_data_emi = lwa_faturamento_ecc-data_lcto_mdfe.
        ENDIF.

      ENDIF.

      IF lwa_zlest0108-fat_contingencia_ecc EQ abap_true.
        IF lwa_tvarvc_user_cont IS INITIAL.
          MESSAGE 'Usuario ao permitido! Documento Emitido em contingencia no ECC' TYPE 'E'.
          RETURN.
        ENDIF.

        _docnumento_emitido_ecc = abap_true.

        CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
          EXPORTING
            i_vbeln                 = lwa_zlest0108-vbeln
            i_get_dados_fat_ecc     = abap_true
          IMPORTING
            e_dados_faturamento_ecc = lwa_faturamento_ecc.

        IF lwa_faturamento_ecc-data_lcto_mdfe IS NOT INITIAL.
          me->at_data_emi = lwa_faturamento_ecc-data_lcto_mdfe.
        ENDIF.

      ENDIF.

    ENDLOOP.
    "Faturamento Contigencia - Fim

    "Caso Tipo Doc Ref. MDF-e for 2 = NF-e.
    IF me->at_tp_doc_ref EQ '2'.

      IF ( me->at_placa_cav IS INITIAL ) AND ( me->at_qcarga IS INITIAL ).

        CLEAR: me->at_qcarga, me->at_vcarga.

        LOOP AT at_it_doc_mdfe INTO at_wa_doc_mdfe.
          TRY.
              zcl_faturamento=>zif_faturamento~get_instance( )->get_romaneio_from_fiscal(
                EXPORTING
                  i_docnum    =  at_wa_doc_mdfe-docnum
                IMPORTING
                  e_zsdt0001  = wa_zsdt0001 ).

              IF wa_zsdt0001 IS NOT INITIAL.
                me->at_placa_cav  = wa_zsdt0001-placa_cav.
                me->at_placa_car1 = wa_zsdt0001-placa_car1.
                me->at_placa_car2 = wa_zsdt0001-placa_car2.
                me->at_placa_car3 = wa_zsdt0001-placa_car3.
                me->at_motorista  = wa_zsdt0001-motorista.
                me->at_cunid      = '01'.
                me->at_cunid_sap  = 'KG'.

                CALL METHOD zcl_romaneio=>get_ck_faturar
                  EXPORTING
                    i_ch_referencia_sai = wa_zsdt0001-ch_referencia
                  IMPORTING
                    e_romaneios         = t_romaneios.

                READ TABLE t_romaneios INTO DATA(_wl_rom) WITH KEY ch_referencia = wa_zsdt0001-ch_referencia.
                IF ( sy-subrc EQ 0 ) AND ( _wl_rom-peso_subtotal IS NOT INITIAL ).
                  ADD _wl_rom-peso_subtotal TO me->at_qcarga.
                  ADD wa_zsdt0001-netwr     TO me->at_vcarga.

                ELSE. "" RMNI - 02/06/2022 - BOC - IR096917
                  ADD wa_zsdt0001-netwr TO me->at_vcarga.
                  ADD wa_zsdt0001-peso_subtotal TO me->at_qcarga.
                  "" RMNI - 02/06/2022 - EOC - IR096917
                ENDIF.
              ENDIF.

            CATCH zcx_faturamento INTO DATA(_cx_fat).
            CATCH zcx_error INTO DATA(_zcx_error).
          ENDTRY.
        ENDLOOP.

      ENDIF.

      IF me->at_placa_cav IS INITIAL.
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE s001.
            RETURN.
          WHEN abap_true.
            MESSAGE s001 INTO DATA(l_mesg).
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
            RETURN.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

*   IF ME->AT_PLACA_CAR1 IS INITIAL.
*     ROLLBACK WORK.
*     MESSAGE 'Placa Reboque 1 não informada' TYPE 'E'.
*     RETURN.
*   ENDIF.

    ENDIF.

    "Documentos Vinculados
    SORT at_it_doc_mdfe BY credat cretim.

    vl_idx_doc = 1.
    CLEAR: vl_ie_emi, vl_cnpj_emi, vl_modal, vl_mdfe_completa_carga, vl_branch_mdfe_completa_carga.
    LOOP AT at_it_doc_mdfe  INTO at_wa_doc_mdfe .

      "1ª Validação -> Documentos devem possuir os mesmos dados de Emissão
      "2ª Validação -> Documentos devem possuir a mesma Modalidade
      CLEAR: vl_ie_emi_aux, vl_cnpj_emi_aux, vl_modal_aux.

      "Seta Dados Gerais do MDF-e para o Documento
      me->set_dados_mdfe( EXPORTING i_docnum = at_wa_doc_mdfe-docnum ).

      vl_modal_aux    = me->at_modal.
      vl_ie_emi_aux   = me->at_ie_emi.
      vl_cnpj_emi_aux = me->at_cnpj_emi.

      IF ( vl_cnpj_emi_aux IS INITIAL ).

        CLEAR: vl_msg_exibir, vl_msg2.
        "VL_MSG2 = AT_WA_DOC_MDFE-DOCNUM.
        "CONCATENATE 'Não encontrado os dados(CNPJ) de Emitente do documento: ' VL_MSG2
        "       INTO VL_MSG_EXIBIR SEPARATED BY SPACE.

        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE e002 WITH at_wa_doc_mdfe-docnum.
          WHEN abap_true.
            MESSAGE e002 WITH at_wa_doc_mdfe-docnum INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        "MESSAGE VL_MSG_EXIBIR TYPE 'E'.
        "RETURN.

      ENDIF.

      IF ( vl_ie_emi_aux IS INITIAL ).

        CLEAR: vl_msg_exibir, vl_msg2.
        "VL_MSG2 = AT_WA_DOC_MDFE-DOCNUM.
        "CONCATENATE 'Não encontrado os dados(Inscrição Estadual) de Emitente do documento: ' VL_MSG2
        "       INTO VL_MSG_EXIBIR SEPARATED BY SPACE.

        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE e003 WITH at_wa_doc_mdfe-docnum.
          WHEN abap_true.
            MESSAGE e003 WITH at_wa_doc_mdfe-docnum INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        "MESSAGE VL_MSG_EXIBIR TYPE 'E'.
        "RETURN.

      ENDIF.

      IF ( vl_modal_aux IS INITIAL ).

        CLEAR: vl_msg_exibir, vl_msg2.
        "VL_MSG2 = AT_WA_DOC_MDFE-DOCNUM.
        "CONCATENATE 'Não encontrado a modalidade do documento: ' VL_MSG2
        "       INTO VL_MSG_EXIBIR SEPARATED BY SPACE.

        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE e004 WITH at_wa_doc_mdfe-docnum.
          WHEN abap_true.
            MESSAGE e004 WITH at_wa_doc_mdfe-docnum INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        "MESSAGE VL_MSG_EXIBIR TYPE 'E'.
        "RETURN.

      ENDIF.

      IF vl_idx_doc = 1.
        vl_ie_emi                     = vl_ie_emi_aux.
        vl_cnpj_emi                   = vl_cnpj_emi_aux.
        vl_modal                      = vl_modal_aux.
        vl_branch_mdfe_completa_carga = at_wa_doc_mdfe-branch.
      ELSE.
        "CS2021000786 Frota propria
        IF ( vl_cnpj_emi NE vl_cnpj_emi_aux        ) AND
           ( vl_cnpj_emi(8) EQ vl_cnpj_emi_aux(8)  ) AND
           ( vl_modal = '01'                       ) AND
           ( me->at_tp_doc_ref EQ '2'              ).
          vl_mdfe_completa_carga = abap_true.
        ELSE.
          vl_mdfe_completa_carga = abap_false.
        ENDIF.
        "CS2021000786 Frota propria

        IF vl_ie_emi NE vl_ie_emi_aux AND vl_mdfe_completa_carga NE abap_true.
          ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
          CASE i_faturamento_autom.
            WHEN abap_off.
              MESSAGE e005.
            WHEN abap_true.
              MESSAGE e005 INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
          "MESSAGE 'Documentos vinculados possuem Inscrições Estaduais(Emitente) distintas!' TYPE 'E'.
          "RETURN.
        ENDIF.

        IF vl_cnpj_emi NE vl_cnpj_emi_aux AND vl_mdfe_completa_carga NE abap_true.
          ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
          CASE i_faturamento_autom.
            WHEN abap_off.
              MESSAGE e006.
            WHEN abap_true.
              MESSAGE e006 INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
          "MESSAGE 'Documentos vinculados possuem CNPJ(Emitente) distintos!' TYPE 'E'.
          "RETURN.
        ENDIF.

        IF vl_modal NE vl_modal_aux.
          ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
          CASE i_faturamento_autom.
            WHEN abap_off.
              MESSAGE e007.
            WHEN abap_true.
              MESSAGE e007 INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
          "MESSAGE 'Documentos vinculados possuem modalidades distintas!' TYPE 'E'.
          "RETURN.
        ENDIF.

      ENDIF.

      "Verifica se Algum documento Vinculado, está em outro MDF-e Valido
      SELECT *
        INTO TABLE it_zsdt0105
        FROM zsdt0105
       WHERE docnum     EQ at_wa_doc_mdfe-docnum.

      LOOP AT it_zsdt0105 INTO wa_zsdt0105 WHERE docnum_ref IS NOT INITIAL.

        SELECT SINGLE *
          INTO @DATA(wa_zsdt0102)
          FROM zsdt0102
         WHERE docnum    EQ @wa_zsdt0105-docnum_ref
           AND estornado NE 'X'
           AND cancel    NE 'X'
           AND encerrado NE 'X'.

        IF sy-subrc = 0.
          "Verifica se documentos vinculados ao MDF-e encontrado, são os mesmos vinculado ao MDF-e de Autorização.
          DATA(lva_mesmos_docs_vinc) = me->check_mesmos_docs_vinc( i_docnum = wa_zsdt0102-docnum ).
          IF lva_mesmos_docs_vinc EQ abap_true.
            CLEAR: wa_zsdt0105, vl_msg1, vl_msg2.
            vl_msg1 = at_wa_doc_mdfe-docnum.
            vl_msg2 = wa_zsdt0102-docnum.
            ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
            CASE i_faturamento_autom.
              WHEN abap_off.
                MESSAGE w008 WITH at_wa_doc_mdfe-docnum wa_zsdt0102-docnum.
              WHEN abap_true.
                MESSAGE w008 WITH at_wa_doc_mdfe-docnum wa_zsdt0102-docnum INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
            RETURN.
          ENDIF.
        ENDIF.

      ENDLOOP.

      CLEAR: wa_act_nota.
      SELECT SINGLE *
        INTO wa_act_nota
        FROM j_1bnfe_active
       WHERE docnum EQ at_wa_doc_mdfe-docnum.

      CLEAR: vl_msg1, vl_msg_exibir.
      vl_msg1 = at_wa_doc_mdfe-docnum.

      IF ( sy-subrc NE 0 ) OR
         ( wa_act_nota-docsta NE 1 ) OR
         ( wa_act_nota-docnum9 IS INITIAL ).
        "CONCATENATE 'Documento:' VL_MSG1 ', não foi autorizado!' INTO VL_MSG_EXIBIR SEPARATED BY SPACE.
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE w009 WITH vl_msg1.
          WHEN abap_true.
            MESSAGE w009 WITH vl_msg1 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        "MESSAGE VL_MSG_EXIBIR TYPE 'W'.
        RETURN.
      ENDIF.

      IF ( wa_act_nota-cancel IS NOT INITIAL ) OR ( wa_act_nota-scssta EQ '2' ).
        "CONCATENATE 'Documento:' VL_MSG1 ', já foi cancelado!' INTO VL_MSG_EXIBIR SEPARATED BY SPACE.
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE w010 WITH vl_msg1.
          WHEN abap_true.
            MESSAGE w010 WITH vl_msg1 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        "MESSAGE VL_MSG_EXIBIR TYPE 'W' .
        RETURN.
      ENDIF.

*    IF WA_ACT_NOTA-SCSSTA EQ '1'.
*      ROLLBACK WORK.
*      "MESSAGE VL_MSG_EXIBIR TYPE 'W' .
*      MESSAGE W045 WITH VL_MSG1.
*      RETURN.
*    ENDIF.

      IF NOT ( wa_act_nota-docsta EQ '1' ) OR wa_act_nota-code NE '100'.
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE w046 WITH vl_msg1.
          WHEN abap_true.
            MESSAGE w046 WITH vl_msg1 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        RETURN.
      ENDIF.

      CLEAR : at_wa_doc_mdfe.
      ADD 1 TO vl_idx_doc.
    ENDLOOP.

    "Grava Dados MDF-e
    CLEAR: wa_zsdt0102.
    CLEAR: at_wa_doc_mdfe.

    IF i_zsdt0237 IS INITIAL.

      READ TABLE at_it_doc_mdfe INTO at_wa_doc_mdfe INDEX 1.

      SELECT SINGLE *
        INTO wa_j1bnfdoc
        FROM j_1bnfdoc
       WHERE docnum = at_wa_doc_mdfe-docnum.

      IF sy-subrc IS NOT INITIAL.
        CLEAR: vl_msg_exibir.
        "CONCATENATE 'Não encontrado os dados do Documento:' AT_WA_DOC_MDFE-DOCNUM '.(Gravar Doc. MDF-e)!'
        "       INTO VL_MSG_EXIBIR SEPARATED BY SPACE.

        ROLLBACK WORK.
        "MESSAGE VL_MSG_EXIBIR TYPE 'W' .
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE w011 WITH at_wa_doc_mdfe-docnum.
          WHEN abap_true.
            MESSAGE w011 WITH at_wa_doc_mdfe-docnum INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        RETURN.
      ENDIF.


      IF vl_mdfe_completa_carga EQ abap_true AND vl_branch_mdfe_completa_carga IS NOT INITIAL.
        wa_j1bnfdoc-branch = vl_branch_mdfe_completa_carga.
      ENDIF.

    ELSE.
      wa_j1bnfdoc-bukrs  = i_zsdt0237-bukrs.
      wa_j1bnfdoc-branch = i_zsdt0237-branch.
    ENDIF.

    SELECT SINGLE * INTO wa_j_1baa FROM j_1baa WHERE nftype = 'MD'.

    IF sy-subrc = 0.

      SELECT SINGLE *
        INTO wa_j_1bb2
        FROM j_1bb2
       WHERE form   = wa_j_1baa-form
         AND bukrs  = wa_j1bnfdoc-bukrs
         AND branch = wa_j1bnfdoc-branch.

      lv_nr_range = wa_j_1bb2-nfenrnr.
      lv_object   = wa_j_1bb2-subobj.

      IF ( wa_j_1bb2-series IS INITIAL ).
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE e012.
          WHEN abap_true.
            MESSAGE e012 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

      IF sy-subrc = 0.

*      SELECT SINGLE *
*        FROM SETLEAF INTO @DATA(WL_SETLEAF_MODEL_XI)
*       WHERE SETNAME EQ 'GRC_MDFE_CALL_XI_BRANCH'
*         AND VALFROM EQ @WA_J1BNFDOC-BRANCH.

        IF sy-subrc IS INITIAL.
          CLEAR: vl_nmdfe.
          "Gerar Documento Fiscal J_1bnfdoc """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          " Verificar SET para Emissão de MDF-e """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          DATA: i_cabecalho	   TYPE zde_fiscal_cabecalho,
                i_itens	       TYPE zde_fiscal_itens_t,
                w_item         TYPE zde_fiscal_itens,
                i_trailer_info TYPE bapi_j_1bnftrailer_tab,
                w_trailer_info TYPE bapi_j_1bnftrailer.

          CLEAR: wl_zlest0061.

          DATA: fiscal TYPE REF TO zcl_fiscal.
          CREATE OBJECT fiscal.

          SELECT SINGLE * INTO @DATA(wa_zlest0002)
            FROM zlest0002
           WHERE pc_veiculo EQ @me->at_placa_cav.

          SELECT SINGLE *
            FROM zlest0061 INTO wl_zlest0061
           WHERE docnum = wa_j1bnfdoc-docnum.

          i_cabecalho-nftype    = 'MD'.
          i_cabecalho-docdat    = me->at_data_emi.
          i_cabecalho-pstdat    = me->at_data_emi.
          "i_cabecalho-pstdat    = sy-datlo.
          i_cabecalho-manual    = abap_true.
          i_cabecalho-waerk     = 'BRL'.
          i_cabecalho-bukrs     = wa_j1bnfdoc-bukrs.
          i_cabecalho-branch    = wa_j1bnfdoc-branch.
          i_cabecalho-parvw     = 'SP'.

          CASE me->at_modal.
            WHEN '01'.
              i_cabecalho-parid   = wa_zlest0002-proprietario.
            WHEN '03'.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_j1bnfdoc-branch
                IMPORTING
                  output = i_cabecalho-parid.
          ENDCASE.

          i_cabecalho-anzpk     = me->at_qcarga.
          i_cabecalho-brgew     = me->at_qcarga.
          i_cabecalho-ntgew     = me->at_qcarga.
          i_cabecalho-gewei     = me->at_cunid_sap.
          i_cabecalho-nfe       = abap_true.
          i_cabecalho-modfrete  = '9'.
          i_cabecalho-ind_final = '0'.

          CASE me->at_modal.
            WHEN '01'.
              i_cabecalho-transp_mode = '1'.
              SELECT SINGLE * INTO @DATA(wa_identifica_doc)
                FROM zcte_identifica WHERE docnum EQ @me->at_wa_doc_mdfe-docnum.
              i_cabecalho-rntrc = wa_identifica_doc-rodo_rntrc.
            WHEN '03'.
              i_cabecalho-transp_mode = '3'.
          ENDCASE.

          CLEAR: w_item.

          IF i_zsdt0237 IS INITIAL.

            LOOP AT me->at_it_doc_mdfe INTO DATA(wa_it_doc_mdfe).

              SELECT SINGLE * INTO @DATA(wa_1bnfdoc)
                FROM j_1bnfdoc
               WHERE docnum EQ @wa_it_doc_mdfe-docnum.

              SELECT * INTO TABLE @DATA(it_1bnflin)
                FROM j_1bnflin
               WHERE docnum EQ @wa_it_doc_mdfe-docnum.

              LOOP AT it_1bnflin INTO DATA(wa_1bnflin).
                w_item-itmnum  = sy-tabix.
                w_item-bwkey   = wa_j1bnfdoc-branch.
                w_item-werks   = wa_j1bnfdoc-branch.
                w_item-cfop    = wa_1bnflin-cfop.
                w_item-nbm     = '153310'.
                w_item-itmtyp  = 'ZH'.
                w_item-meins   = 'UN'.

                ADD wa_1bnflin-netwr TO w_item-netwr.
                ADD wa_1bnflin-menge TO w_item-menge.

                w_item-taxlw1  = wa_1bnflin-taxlw1.
                w_item-taxlw2  = wa_1bnflin-taxlw2.
                w_item-taxlw4  = wa_1bnflin-taxlw4.
                w_item-taxlw5  = wa_1bnflin-taxlw5.
              ENDLOOP.

            ENDLOOP.
          ELSE.
            w_item-itmnum  = sy-tabix.
            w_item-bwkey   = wa_j1bnfdoc-branch.
            w_item-werks   = wa_j1bnfdoc-branch.
            "W_ITEM-CFOP    = WA_1BNFLIN-CFOP.
            w_item-nbm     = '153310'.
            w_item-itmtyp  = 'ZH'.
            w_item-meins   = 'UN'.

            DESCRIBE TABLE i_zsdt0241_t LINES DATA(qtd_documentos).
            w_item-netwr = qtd_documentos.
            w_item-menge = qtd_documentos.

            w_item-taxlw1  = 'IM5'.
            w_item-taxlw2  = 'I53'.
            w_item-taxlw4  = 'C08'.
            w_item-taxlw5  = 'P08'.
          ENDIF.

          IF w_item IS NOT INITIAL.
            APPEND w_item TO i_itens.
          ENDIF.

          CASE me->at_modal.
            WHEN '01'.

              DATA: lc_qtd_veiculo TYPE i.
              lc_qtd_veiculo = 1.

              IF me->at_placa_cav IS NOT INITIAL.
                CLEAR: w_trailer_info.
                w_trailer_info-counter = lc_qtd_veiculo.
                w_trailer_info-placa   = me->at_placa_cav.
                SELECT SINGLE * INTO @DATA(wa_zlest002)
                  FROM zlest0002
                 WHERE pc_veiculo EQ @me->at_placa_cav.
                w_trailer_info-veh_uf = wa_zlest002-cd_uf.
                SELECT SINGLE * INTO @DATA(wa_lfa1)
                  FROM lfa1
                 WHERE lifnr EQ @wa_zlest002-proprietario.
                w_trailer_info-rntc   = wa_lfa1-bahns.
                ADD 1 TO lc_qtd_veiculo.
                APPEND w_trailer_info TO i_trailer_info.
              ENDIF.

              IF me->at_placa_car1 IS NOT INITIAL.
                CLEAR: wa_zlest002, wa_lfa1, w_trailer_info.
                w_trailer_info-counter = lc_qtd_veiculo.
                w_trailer_info-placa   = me->at_placa_car1.
                SELECT SINGLE * INTO @wa_zlest002
                  FROM zlest0002
                 WHERE pc_veiculo EQ @me->at_placa_car1.
                w_trailer_info-veh_uf = wa_zlest002-cd_uf.
                SELECT SINGLE * INTO @wa_lfa1
                  FROM lfa1
                 WHERE lifnr EQ @wa_zlest002-proprietario.
                w_trailer_info-rntc   = wa_lfa1-bahns.
                ADD 1 TO lc_qtd_veiculo.
                APPEND w_trailer_info TO i_trailer_info.
              ENDIF.

              IF me->at_placa_car2 IS NOT INITIAL.
                CLEAR: wa_zlest002, wa_lfa1, w_trailer_info.
                w_trailer_info-counter = lc_qtd_veiculo.
                w_trailer_info-placa   = me->at_placa_car2.
                SELECT SINGLE * INTO @wa_zlest002
                  FROM zlest0002
                 WHERE pc_veiculo EQ @me->at_placa_car2.
                w_trailer_info-veh_uf = wa_zlest002-cd_uf.
                SELECT SINGLE * INTO @wa_lfa1
                  FROM lfa1
                 WHERE lifnr EQ @wa_zlest002-proprietario.
                w_trailer_info-rntc   = wa_lfa1-bahns.
                ADD 1 TO lc_qtd_veiculo.
                APPEND w_trailer_info TO i_trailer_info.
              ENDIF.

              IF me->at_placa_car3 IS NOT INITIAL.
                CLEAR: wa_zlest002, wa_lfa1, w_trailer_info.
                w_trailer_info-counter = lc_qtd_veiculo.
                w_trailer_info-placa   = me->at_placa_car3.
                SELECT SINGLE * INTO @wa_zlest002
                  FROM zlest0002
                 WHERE pc_veiculo EQ @me->at_placa_car3.
                w_trailer_info-veh_uf = wa_zlest002-cd_uf.
                SELECT SINGLE * INTO @wa_lfa1
                  FROM lfa1
                 WHERE lifnr EQ @wa_zlest002-proprietario.
                w_trailer_info-rntc   = wa_lfa1-bahns.
                ADD 1 TO lc_qtd_veiculo.
                APPEND w_trailer_info TO i_trailer_info.
              ENDIF.

            WHEN '03'.

          ENDCASE.

          fiscal->criar_mdfe(
            EXPORTING
              i_cabecalho        = i_cabecalho
              i_itens            = i_itens
              i_trailer_info     = i_trailer_info
              i_bapi_wait        = abap_true
            IMPORTING
              e_docnum           = e_docnum
              e_retorno          = DATA(e_retorno)
            RECEIVING
              r_gerou            = DATA(r_gerou)
            EXCEPTIONS
              data_fi_mm_nao     = 1
              nao_cte_forn       = 2
              documento_existe   = 3
              erro               = 4
              OTHERS             = 5 ).

          CLEAR: fiscal.

          IF sy-subrc IS NOT INITIAL.
*-#133089-21.02.2024-JT-inicio
            CASE i_faturamento_autom.
              WHEN abap_off.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              WHEN abap_true.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
            ROLLBACK WORK.
          ENDIF.

          IF r_gerou EQ abap_false.
            ROLLBACK WORK.
            READ TABLE e_retorno INTO DATA(wa_retorno) INDEX 1.
            IF sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
              CASE i_faturamento_autom.
                WHEN abap_off.
                  MESSAGE ID wa_retorno-id TYPE 'E' NUMBER wa_retorno-number WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.
                WHEN abap_true.
                  MESSAGE ID wa_retorno-id TYPE 'E' NUMBER wa_retorno-number WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4 INTO l_mesg..
                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
              ENDCASE.
*-#133089-21.02.2024-JT-fim
            ELSE.
*-#133089-21.02.2024-JT-inicio
              CASE i_faturamento_autom.
                WHEN abap_off.
                  MESSAGE e013.
                WHEN abap_true.
                  MESSAGE e013 INTO l_mesg.
                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
              ENDCASE.
*-#133089-21.02.2024-JT-fim
            ENDIF.
          ELSE.
            sy-subrc = 0.
          ENDIF.

          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        ELSE.

          CALL FUNCTION 'J_1B_NF_DOCUMENT_NUMB_GET_NEXT'
            EXPORTING
              i_nr_range                    = lv_nr_range
              i_subobject                   = lv_object
            IMPORTING
              doc_number                    = vl_nmdfe
            EXCEPTIONS
              interval_not_found            = 1
              number_range_not_internal     = 2
              object_not_found              = 3
              other_problems_with_numbering = 4
              OTHERS                        = 5.

          IF ( sy-subrc <> 0 ) OR ( vl_nmdfe IS INITIAL ).
            ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
            CASE i_faturamento_autom.
              WHEN abap_off.
                MESSAGE e013.
              WHEN abap_true.
                MESSAGE e013 INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
            "MESSAGE 'Não foi possivel determinar o número do MDF-e!' TYPE 'E'.
            "RETURN.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = vl_nmdfe
            IMPORTING
              output = lv_nr_mdfe.

        ENDIF.

      ELSE.
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE e014.
          WHEN abap_true.
            MESSAGE e014 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        "MESSAGE 'Não encontrado os dados para determinar o número do MDF-e!' TYPE 'E'.
        "RETURN.
      ENDIF.

    ELSE.
      ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
          MESSAGE e014.
        WHEN abap_true.
          MESSAGE e014 INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
      "MESSAGE 'Não encontrado os dados para determinar o número do MDF-e!' TYPE 'E'.
      "RETURN.
    ENDIF.

    IF e_docnum IS NOT INITIAL.

      DATA(_check_mdfe_gravado) = abap_false.
      DO 60 TIMES.
        SELECT SINGLE *
          FROM j_1bnfdoc INTO @DATA(wl_doc_mdfe_grv)
         WHERE docnum EQ @e_docnum
           AND model  EQ @zif_doc_eletronico=>at_st_model_mdfe.

        IF ( sy-subrc = 0 ).
          _check_mdfe_gravado = abap_true.
          EXIT.
        ELSE.
          WAIT UP TO 2 SECONDS.
        ENDIF.
      ENDDO.

      IF _check_mdfe_gravado EQ abap_false.
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE e016 RAISING erro_gravar.
          WHEN abap_true.
            MESSAGE e016 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
            RAISE erro_gravar.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        RETURN.
      ENDIF.

      wa_zsdt0102-docnum = e_docnum.
    ELSE.
      "Atribui Numero Documento para MDF-e
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZMDFE'
        IMPORTING
          number      = wa_zsdt0102-docnum.
    ENDIF.

    IF ( sy-subrc <> 0 ) OR ( wa_zsdt0102-docnum IS INITIAL ).
      ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
          MESSAGE e015.
        WHEN abap_true.
          MESSAGE e015 INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
      "MESSAGE 'Não foi possivel determinar o número de Documento do MDF-e!' TYPE 'E'.
      RETURN.
    ENDIF.

    wa_zsdt0102-nmdfe             = lv_nr_mdfe.
    wa_zsdt0102-serie             = wa_j_1bb2-series.
    wa_zsdt0102-dhemi             = me->at_dhemi.
    wa_zsdt0102-data_emi          = me->at_data_emi.
    wa_zsdt0102-hora_emi          = me->at_hora_emi.
    wa_zsdt0102-usuario           = sy-uname.
    wa_zsdt0102-modal             = me->at_modal.
    wa_zsdt0102-ie_emi            = me->at_ie_emi.
    wa_zsdt0102-cnpj_emi          = me->at_cnpj_emi.
    wa_zsdt0102-mun_enc           = me->at_mun_enc.
    wa_zsdt0102-cunid             = me->at_cunid.
    wa_zsdt0102-tp_doc_ref        = me->at_tp_doc_ref.
    wa_zsdt0102-id_local_coleta   = me->at_id_local_coleta.
    wa_zsdt0102-id_local_descarga = me->at_id_local_descarga.
    wa_zsdt0102-ufini             = me->at_ufini.
    wa_zsdt0102-cmunini           = me->at_cmunini.
    wa_zsdt0102-nmunini           = me->at_nmunini.
    wa_zsdt0102-uffim             = me->at_uffim.
    wa_zsdt0102-cmunfim           = me->at_cmunfim.
    wa_zsdt0102-nmunfim           = me->at_nmunfim.

    TRY .
        DATA(p_prod_preduminante) = me->get_prodpred_doc_vinculados( i_zsdt0237	= i_zsdt0237 ).
      CATCH zcx_mdfe INTO DATA(ex_mdfe).
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE ID ex_mdfe->msgid TYPE 'E' NUMBER ex_mdfe->msgno WITH ex_mdfe->msgv1 ex_mdfe->msgv2 ex_mdfe->msgv3 ex_mdfe->msgv4 RAISING erro_gravar.
          WHEN abap_true.
            MESSAGE ID ex_mdfe->msgid TYPE 'E' NUMBER ex_mdfe->msgno WITH ex_mdfe->msgv1 ex_mdfe->msgv2 ex_mdfe->msgv3 ex_mdfe->msgv4 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
            RAISE erro_gravar.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDTRY.

    wa_zsdt0102-matkl          = p_prod_preduminante-matkl.
    wa_zsdt0102-matnr          = p_prod_preduminante-matnr.
    wa_zsdt0102-tpcarga        = p_prod_preduminante-tpcarga.
    wa_zsdt0102-xprod          = p_prod_preduminante-xprod.

    wa_zsdt0102-locc_cep       = p_prod_preduminante-locc_cep.
    wa_zsdt0102-locd_cep       = p_prod_preduminante-locd_cep.

    wa_zsdt0102-locc_latitude  = p_prod_preduminante-locc_latitude.
    wa_zsdt0102-locd_latitude  = p_prod_preduminante-locd_latitude.

    wa_zsdt0102-locc_longitude = p_prod_preduminante-locc_longitude.
    wa_zsdt0102-locd_longitude = p_prod_preduminante-locd_longitude.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    IF lwa_contingencia_mdfe = abap_true.
      wa_zsdt0102-contingencia = abap_true.
      wa_zsdt0102-autorizado   = abap_true.
    ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

    INSERT zsdt0102 FROM wa_zsdt0102.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
          MESSAGE e016 RAISING erro_gravar.
        WHEN abap_true.
          MESSAGE e016 INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
          RAISE erro_gravar.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
      "MESSAGE 'Houve um erro ao gravar o MDF-e!' TYPE 'E' RAISING ERRO_GRAVAR.
      "RETURN.
    ENDIF.

    me->set_docnum( wa_zsdt0102-docnum ).

    TRY .
        me->set_nmdf( wa_zsdt0102-nmdfe ).
        me->set_serie( wa_zsdt0102-serie ).
      CATCH zcx_mdfe INTO ex_mdfe.
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE ID ex_mdfe->msgid TYPE 'E' NUMBER ex_mdfe->msgno WITH ex_mdfe->msgv1 ex_mdfe->msgv2 ex_mdfe->msgv3 ex_mdfe->msgv4 RAISING erro_gravar.
          WHEN abap_true.
            MESSAGE ID ex_mdfe->msgid TYPE 'E' NUMBER ex_mdfe->msgno WITH ex_mdfe->msgv1 ex_mdfe->msgv2 ex_mdfe->msgv3 ex_mdfe->msgv4 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
            RAISE erro_gravar.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDTRY.

    me->set_modal( wa_zsdt0102-modal ).

    "Grava UFs Percurso
    lv_idx_uf = 1.
    LOOP AT at_it_uf_perc INTO at_wa_uf_perc WHERE NOT ( uf IS INITIAL ).

      MOVE-CORRESPONDING at_wa_uf_perc TO wa_zsdt0104.
      wa_zsdt0104-nmdfe    = me->at_nmdf.
      wa_zsdt0104-docnum   = wa_zsdt0102-docnum.
      wa_zsdt0104-ordem_uf = lv_idx_uf.

      INSERT zsdt0104 FROM wa_zsdt0104.
      IF sy-subrc <> 0.
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE e017 RAISING erro_gravar.
          WHEN abap_true.
            MESSAGE e017 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
            RAISE erro_gravar.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        "MESSAGE 'Houve um erro ao gravar as UFs de percurso do MDF-e!' TYPE 'E' RAISING ERRO_GRAVAR.
        "RETURN.
      ENDIF.

      CLEAR wa_zsdt0104.
      ADD 1 TO lv_idx_uf.

    ENDLOOP.

    "Grava Documentos MDF-e
    LOOP AT at_it_doc_mdfe  INTO at_wa_doc_mdfe .

      MOVE-CORRESPONDING at_wa_doc_mdfe TO wa_zsdt0105.
      wa_zsdt0105-nmdfe      = me->at_nmdf.
      wa_zsdt0105-docnum_ref = wa_zsdt0102-docnum.

      INSERT zsdt0105 FROM wa_zsdt0105.

      IF sy-subrc <> 0.
        ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
        CASE i_faturamento_autom.
          WHEN abap_off.
            MESSAGE e018 RAISING erro_gravar.
          WHEN abap_true.
            MESSAGE e018 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
            RAISE erro_gravar.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        "MESSAGE 'Houve um erro ao gravar os documentos do MDF-e!' TYPE 'E' RAISING ERRO_GRAVAR .
        "RETURN.
      ENDIF.

      CLEAR wa_zsdt0105.

    ENDLOOP.

    "Grava Dados Transporte Manual.
    wa_zsdt0118-docnum      = wa_zsdt0102-docnum.
    wa_zsdt0118-placa_cav   = me->at_placa_cav.
    wa_zsdt0118-placa_car1  = me->at_placa_car1.
    wa_zsdt0118-placa_car2  = me->at_placa_car2.
    wa_zsdt0118-placa_car3  = me->at_placa_car3.
    wa_zsdt0118-motorista   = me->at_motorista.
    wa_zsdt0118-cunid       = me->at_cunid.
    wa_zsdt0118-qcarga      = me->at_qcarga.
    wa_zsdt0118-vcarga      = me->at_vcarga.
    INSERT zsdt0118 FROM wa_zsdt0118.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
          MESSAGE e019 RAISING erro_gravar.
        WHEN abap_true.
          MESSAGE e019 INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
          RAISE erro_gravar.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.

    e_docnum = wa_zsdt0102-docnum.

    IF e_docnum IS INITIAL.
      ROLLBACK WORK.
*-#133089-21.02.2024-JT-inicio
      CASE i_faturamento_autom.
        WHEN abap_off.
          MESSAGE e020 RAISING erro_gravar.
        WHEN abap_true.
          MESSAGE e020 INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'MDFE' ).
          RAISE erro_gravar.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
      "MESSAGE 'Houve um erro ao gravar o MDF-e(Doc. Num)!' TYPE 'E' RAISING ERRO_GRAVAR .
      "RETURN.
    ENDIF.

    COMMIT WORK.

*-#133089-21.02.2024-JT-inicio
    CASE i_faturamento_autom.
      WHEN abap_off.
        MESSAGE s021.
      WHEN abap_true.
        MESSAGE s021 INTO l_mesg.
        l_mesg = l_mesg && ':' && e_docnum.
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = i_ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'MDFE' ).
    ENDCASE.
*-#133089-21.02.2024-JT-fim

    IF _docnumento_emitido_ecc EQ abap_false.
*-#133089-21.02.2024-JT-inicio
      IF i_faturamento_autom = abap_false.
        me->check_encerramento_mdfes( IMPORTING  e_docnum_sol_enc  = e_docnum_sol_enc ).
      ELSE.
        me->check_encerramento_mdfes( EXPORTING  i_sem_confirmacao = abap_true
                            IMPORTING  e_docnum_sol_enc  = e_docnum_sol_enc ).
      ENDIF.
*-#133089-21.02.2024-JT-fim
    ENDIF.


  ENDMETHOD.


  METHOD monta_xml.

    "Internal Tables and WorkAreas
    DATA: wa_j_1bnfdoc  TYPE j_1bnfdoc,
          wa_j_1bbranch TYPE j_1bbranch,
          "Dados UF Percurso.
          it_zsdt0104   TYPE TABLE OF zsdt0104,
          wa_zsdt0104   TYPE zsdt0104.

    DATA: it_zsdt0228 TYPE TABLE OF zsdt0228.

    "Variables
    DATA: xdhemi       TYPE c LENGTH 30,
          xdtenc       TYPE c LENGTH 30,
          xdata        TYPE c LENGTH 30,
          xhora        TYPE c LENGTH 10,
          xvalor       TYPE string,
          vl_ie        TYPE string,
          vl_cnpj      TYPE string,
          vl_cunid     TYPE string,
          vl_mun_enc   TYPE string,
          vl_chave     TYPE c LENGTH 44,
          xml_mod_aqua TYPE string, "Variaveis XML Modal Àquaviario
          xml_mod_rodo TYPE string. "Variaveis XML Modal Rodoviario


    "Objects
    DATA: obj_zcl_util  TYPE REF TO zcl_util,
          zcl_mdfe_aqua TYPE REF TO zcl_mdfe_aqua, "Objeto Modal Aquaviario
          zcl_mdfe_rodo TYPE REF TO zcl_mdfe_rodo. "Objeto Modal Rodoviario


*  Tipo 1 = Enviar
*       2 = Cancelar
*       3 = Encerrar

    CLEAR: e_xml. "Limpar a variavel de retorno.
    CLEAR: vl_chave.

    CASE tipo.
      WHEN '2' OR '3'.
        me->get_chave_mdfe( RECEIVING e_chave = vl_chave ).
        IF ( vl_chave IS INITIAL ).
          ROLLBACK WORK.
          RETURN.
        ENDIF.
    ENDCASE.

    DEFINE conc_xml.
      CONCATENATE e_xml &1 INTO e_xml.
    END-OF-DEFINITION.

    CASE tipo.
      WHEN '1'. " Enviar

        FREE: zcl_mdfe_rodo, zcl_mdfe_aqua.

        CREATE OBJECT zcl_mdfe_rodo.
        CREATE OBJECT zcl_mdfe_aqua.

        CLEAR: me->at_wa_doc_mdfe.

        "------------------------------------------------------------------------
        "Recupera dados Gerais MDF-e.
        "------------------------------------------------------------------------
        READ TABLE me->at_it_doc_mdfe INTO me->at_wa_doc_mdfe INDEX 1.

        IF ( me->at_ie_emi IS INITIAL ) OR
           ( me->at_cnpj_emi IS INITIAL ) OR
           ( me->at_cunid IS INITIAL ).
          me->set_dados_mdfe( EXPORTING i_docnum = me->at_wa_doc_mdfe-docnum ).
        ENDIF.

        "Formata Data e Hora Emissão
        CONCATENATE me->at_hora_emi(2) ':' me->at_hora_emi+2(2) ':' me->at_hora_emi+4(2) INTO xhora.
        CONCATENATE me->at_data_emi(4) '-' me->at_data_emi+4(2) '-' me->at_data_emi+6(2) INTO xdata.
        CONCATENATE xdata xhora INTO xdhemi SEPARATED BY space.


        "CNPJ/CPF Autorizados Download
        CLEAR: it_zsdt0228[].

        SELECT SINGLE *
          FROM j_1bnfdoc INTO @DATA(wl_j_1bnfdoc_mdfe)
         WHERE docnum EQ @me->at_wa_doc_mdfe-docnum.

        IF ( sy-subrc EQ 0 ) AND ( me->at_wa_doc_mdfe-docnum IS NOT INITIAL ).
          SELECT *
            FROM zsdt0228 INTO TABLE it_zsdt0228
           WHERE bukrs EQ wl_j_1bnfdoc_mdfe-bukrs.
        ENDIF.

        conc_xml '<intgMDFe>'.

        "------------------------------------------------------------------------
        ""CNPJ/CPF Autorizados Download
        "------------------------------------------------------------------------
        LOOP AT it_zsdt0228 INTO DATA(wl_zsdt0228).
          conc_xml         '<autDownLoadXML>'.
          conc_xml             wl_zsdt0228-stcd1.
          conc_xml         '</autDownLoadXML>'.
        ENDLOOP.

        "------------------------------------------------------------------------
        "Identificação do Manifesto de Documentos Fiscais Eletrônicos - MDF-e
        "------------------------------------------------------------------------

        conc_xml     '<ide>'.

        conc_xml         '<nMDF> '.
        conc_xml            me->at_nmdf.
        conc_xml         '</nMDF>'.

        conc_xml         '<serie>'.
        conc_xml            me->at_serie.
        conc_xml         '</serie>'.

        conc_xml         '<dhEmi>'.
        conc_xml            xdhemi.
        conc_xml         '</dhEmi>'.

        "------------------------------------------------------------------------
        "Sigla das Unidades da Federação do percurso do veículo.
        "------------------------------------------------------------------------
        SELECT *
          FROM zsdt0104
          INTO TABLE it_zsdt0104
         WHERE nmdfe  = me->at_nmdf
           AND docnum = me->at_docnum.

        SORT it_zsdt0104 BY ordem_uf ASCENDING.
        LOOP AT it_zsdt0104 INTO wa_zsdt0104.
          IF NOT ( wa_zsdt0104-uf IS INITIAL ).
            conc_xml     '<uFPer>'.
            conc_xml        wa_zsdt0104-uf.
            conc_xml     '</uFPer>'.
          ENDIF.
          CLEAR: wa_zsdt0104.
        ENDLOOP.

        conc_xml         '<dhIniViagem>'.
        conc_xml            xdhemi.
        conc_xml         '</dhIniViagem>'.

        IF me->at_tp_doc_ref = '2'. "NF-e.

          conc_xml       '<tpEmit>2</tpEmit>'.

          "Tipo Transportador
          IF me->at_placa_cav IS NOT INITIAL.
            SELECT SINGLE *
              FROM zlest0002 INTO @DATA(_wl_zlest0002)
             WHERE pc_veiculo EQ @me->at_placa_cav.

            IF ( sy-subrc EQ 0 ) AND ( _wl_zlest0002-proprietario IS NOT INITIAL ).
              SELECT SINGLE *
                FROM lfa1 INTO @DATA(_wl_lfa1_prop)
               WHERE lifnr = @_wl_zlest0002-proprietario.

              IF _wl_lfa1_prop-stkzn IS INITIAL.
                conc_xml  '<tpTransp>2</tpTransp>'.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.

        conc_xml     '</ide>'.

        "------------------------------------------------------------------------
        "TAG de grupo de informações do emitente do MDF-e
        "------------------------------------------------------------------------
        conc_xml     '<emit>'.
        conc_xml         '<CNPJ>'.
        conc_xml            me->at_cnpj_emi.
        conc_xml         '</CNPJ>'.
        conc_xml         '<IE>'.
        conc_xml            me->at_ie_emi.
        conc_xml         '</IE>'.
        conc_xml     '</emit>'.

        "------------------------------------------------------------------------
        "Informações dos Documentos fiscais vinculados ao manifesto
        "------------------------------------------------------------------------
        conc_xml     '<infDoc>'.
        LOOP AT at_it_doc_mdfe INTO at_wa_doc_mdfe.
          IF me->at_tp_doc_ref = '2'.
            conc_xml       '<infNFe>'.
            conc_xml          at_wa_doc_mdfe-vl_chave.
            conc_xml       '</infNFe>'.
          ELSE.
            conc_xml       '<infCTe>'.
            conc_xml          at_wa_doc_mdfe-vl_chave.
            conc_xml       '</infCTe>'.
          ENDIF.
        ENDLOOP.
        conc_xml     '</infDoc>'.

        "------------------------------------------------------------------------
        "Totalizadores da carga transportada e seus documentos fiscais
        "------------------------------------------------------------------------
        conc_xml     '<tot>'.
        conc_xml         '<cUnid>'.
        conc_xml            me->at_cunid.
        conc_xml         '</cUnid>'.

        IF me->at_qcarga IS NOT INITIAL.
          xvalor = me->at_qcarga.
          conc_xml       '<qCarga>'.
          conc_xml          xvalor.
          conc_xml       '</qCarga>'.
        ENDIF.

        conc_xml     '</tot>'.

        "------------------------------------------------------------------------
        " Atribuição do XML especifíco do Modal.
        "------------------------------------------------------------------------
        IF me->at_modal = '01'. "Modal Rodoviario
          zcl_mdfe_rodo->monta_xml( EXPORTING i_obj = me
                            RECEIVING e_xml = xml_mod_rodo ).
          conc_xml  xml_mod_rodo.
        ENDIF.

        IF me->at_modal = '03'. "Modal Aquaviário
          zcl_mdfe_aqua->monta_xml( EXPORTING i_obj = me
                            RECEIVING e_xml = xml_mod_aqua ).
          conc_xml  xml_mod_aqua.
        ENDIF.

        LOOP AT at_it_doc_mdfe INTO at_wa_doc_mdfe.

          IF me->at_tp_doc_ref EQ '1'.

            SELECT *
              FROM zcte_seguro
              INTO TABLE @DATA(it_zcte_seguro)
             WHERE docnum EQ @at_wa_doc_mdfe-docnum.

            DATA: nm_seguro TYPE c LENGTH 30.

            IF sy-subrc IS INITIAL.
              READ TABLE it_zcte_seguro INDEX 1 INTO DATA(wa_zcte_seguro).
              SELECT SINGLE *
                FROM zlest0143
                INTO @DATA(wa_zlest0143)
               WHERE docnum EQ @me->at_wa_doc_mdfe-docnum.

              IF wa_zcte_seguro-resp_codigo IS NOT INITIAL.
                SELECT SINGLE * INTO @DATA(wa_segurado) FROM lfa1 WHERE lifnr EQ @wa_zcte_seguro-resp_codigo.
              ENDIF.

              conc_xml '<seg>'.
              conc_xml '<infResp>'.
              conc_xml '<respSeg>'.
              "CONC_XML WA_ZCTE_SEGURO-RESPSEG.
              conc_xml '1'.
              conc_xml '</respSeg>'.
              "CONC_XML '<CNPJ>'. CONC_XML '</CNPJ>'.
              "CONC_XML '<CPF>'. CONC_XML '</CPF>'.
              conc_xml '</infResp>'.

              conc_xml '<infSeg>'.
              conc_xml '<xSeg>'.
              MOVE wa_zcte_seguro-xseg TO nm_seguro.
              conc_xml nm_seguro.
              conc_xml '</xSeg>'.
              conc_xml '<CNPJ>'.
              conc_xml wa_segurado-stcd1.
              conc_xml '</CNPJ>'.
              conc_xml '</infSeg>'.

              conc_xml '<nApol>'.
              conc_xml wa_zcte_seguro-napol.
              conc_xml '</nApol>'.

              conc_xml '<nAver>'.
              conc_xml wa_zlest0143-nr_averbacao.
              conc_xml '</nAver>'.

              conc_xml '</seg>'.
            ENDIF.

          ENDIF.
        ENDLOOP.

        conc_xml '</intgMDFe>'.

      WHEN '2'. " Cancelar

        IF ( me->at_just_canc IS INITIAL ).
          ROLLBACK WORK.
          CLEAR e_xml.
          MESSAGE e032(zmdfe).
          "MESSAGE 'Houve um erro ao obter a Justificativa de cancelamento do MDF-e!' TYPE 'E'.
          "RETURN.
        ENDIF.

        conc_xml '<intgCancMDFe>'.

        conc_xml     '<chMDFe>'.
        conc_xml        vl_chave.
        conc_xml     '</chMDFe>'.

        conc_xml     '<xJust>'.
        conc_xml        me->at_just_canc.
        conc_xml     '</xJust>'.

        conc_xml '</intgCancMDFe>'.

      WHEN '3'. " Encerrar

        CLEAR me->at_wa_doc_mdfe.

        "------------------------------------------------------------------------
        "Recupera dados Gerais MDF-e.
        "------------------------------------------------------------------------
        READ TABLE me->at_it_doc_mdfe INTO me->at_wa_doc_mdfe INDEX 1.

        IF ( me->at_ie_emi IS INITIAL ) OR
           ( me->at_cnpj_emi IS INITIAL ) OR
           ( me->at_mun_enc IS INITIAL ).
          me->set_dados_mdfe( EXPORTING i_docnum = me->at_wa_doc_mdfe-docnum ).
        ENDIF.

        vl_mun_enc = me->at_mun_enc.

        CLEAR: xdtenc.
        CONCATENATE sy-datum(4) '-' sy-datum+4(2) '-' sy-datum+6(2) INTO xdtenc.

        IF ( vl_mun_enc IS INITIAL ).
          ROLLBACK WORK.
          CLEAR e_xml.
          MESSAGE s033(zmdfe).
          "MESSAGE 'Houve um erro ao obter o Município de Encerramento do MDF-e!' TYPE 'S'.
          RETURN.
        ENDIF.

        conc_xml '<intgEncMDFe>'.

        conc_xml     '<chMDFe>'.
        conc_xml        vl_chave.
        conc_xml     '</chMDFe>'.

        conc_xml     '<dtEnc>'.
        conc_xml        xdtenc.
        conc_xml     '</dtEnc>'.

        conc_xml     '<cMun>'.
        conc_xml        vl_mun_enc.
        conc_xml     '</cMun>'.

        conc_xml '</intgEncMDFe>'.

    ENDCASE.


  ENDMETHOD.


  METHOD notifica_encerramento.

    DATA: vl_titulo    TYPE string.
    DATA: lit_zsdt0105 TYPE TABLE OF zsdt0105.
    DATA: lit_zsdt0296 TYPE TABLE OF zsdt0296.
    DATA: lit_zmail    TYPE TABLE OF zmail.
    DATA: it_html      TYPE TABLE OF w3html INITIAL SIZE 0.


    DATA: objpack     TYPE TABLE OF sopcklsti1,
          lwa_objpack TYPE sopcklsti1.

    DATA: objhead     TYPE TABLE OF solisti1.
    DATA: objbin_ord  TYPE TABLE OF solisti1.
    DATA: objbin_log  TYPE TABLE OF solisti1.
    DATA: objbin_ann  TYPE solisti1.
    DATA: objbin      TYPE TABLE OF solisti1.
    DATA: wa_objbin   TYPE solisti1.
    DATA: content_hex TYPE STANDARD TABLE OF solix.
    DATA: objtxt      TYPE TABLE OF solisti1.
    DATA: reclist     TYPE TABLE OF somlreci1.
    DATA: lwa_reclist TYPE somlreci1.
    DATA: doc_chng    TYPE sodocchgi1.
    DATA: tab_lines   TYPE sy-tabix.

    DATA: lv_valor TYPE string.

    DATA: lwa_mdfe_atual     TYPE zsdt0102.
    DATA: lwa_mdfe_atual_doc TYPE j_1bnfdoc.

    DATA: lv_title_email TYPE string.

    DATA: vl_nmdfe    TYPE string,
          vl_docnum   TYPE string,
          vl_filial   TYPE string,
          vl_data_aut TYPE string,
          vl_msg_ret  TYPE string.

    DEFINE conc_html.

      lv_valor = &1.

      CALL FUNCTION 'ZHTML_ADD'
        EXPORTING
          i_texto = lv_valor
        TABLES
          it_html = it_html.
    END-OF-DEFINITION.

    CLEAR: lit_zsdt0105[], reclist[], lwa_mdfe_atual, lwa_mdfe_atual_doc.

    CHECK i_docnum IS NOT INITIAL.

    SELECT SINGLE *
      FROM zsdt0102 INTO @DATA(wa_zsdt0102)
     WHERE docnum    EQ @i_docnum.

    CHECK sy-subrc EQ 0.

    IF i_avaliar_encerramento EQ abap_false.
      CHECK wa_zsdt0102-encerrado = abap_true.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(wa_doc_mdfe)
     WHERE docnum    EQ @i_docnum.

    CHECK sy-subrc EQ 0.

    IF i_docnum_atual IS NOT INITIAL.
      SELECT SINGLE *
        FROM zsdt0102 INTO lwa_mdfe_atual
       WHERE docnum EQ i_docnum_atual.

      SELECT SINGLE *
        FROM j_1bnfdoc INTO lwa_mdfe_atual_doc
       WHERE docnum EQ i_docnum_atual.
    ENDIF.

    IF i_avaliar_encerramento EQ abap_true.
      lv_title_email = 'AVALIAÇÃO ENCERRAMENTO MDF-E'.
    ELSE.
      lv_title_email = 'ENCERRAMENTO MDF-E'.
    ENDIF.

    "Determinação Destinatarios

*--------------------------------------------------------------------------------------------------------------------*
*    Emails de Pessoas Interessantes no encerramento
*--------------------------------------------------------------------------------------------------------------------*
    SELECT *
      FROM zsdt0296 INTO TABLE lit_zsdt0296
     WHERE docnum EQ i_docnum.

    CHECK lit_zsdt0296[] IS NOT INITIAL.

    LOOP AT lit_zsdt0296 INTO DATA(lwa_zsdt0296) WHERE email IS NOT INITIAL.
      lwa_reclist-receiver = lwa_zsdt0296-email.
      lwa_reclist-rec_type = 'U'.

      IF i_avaliar_encerramento EQ abap_true.
        lwa_reclist-copy     = abap_true. "No email de avaliação do encerramento, os interessados ficam como copia
      ELSE.
        lwa_reclist-copy     = abap_false. "No email de notificação do encerramento, os interessados ficam destinatario
      ENDIF.

      APPEND lwa_reclist TO reclist.
    ENDLOOP.

*--------------------------------------------------------------------------------------------------------------------*
*   Emails de Pessoas que realizará a avaliação/encerramento do MDF-e
*--------------------------------------------------------------------------------------------------------------------*
    DATA(lva_email_aviso) = zcl_mdfe=>get_email_avaliacao_enc( i_bukrs = CONV #( wa_doc_mdfe-bukrs ) ).

    IF ( lva_email_aviso IS NOT INITIAL ).
      CLEAR: lwa_reclist.

      lwa_reclist-receiver = lva_email_aviso.
      lwa_reclist-rec_type = 'U'.
      lwa_reclist-copy     = abap_false.

      APPEND lwa_reclist TO reclist.
    ENDIF.

    CHECK reclist[] IS NOT INITIAL.

    IF i_avaliar_encerramento EQ abap_true.
      vl_titulo = 'AVISO! O MDF-E ABAIXO NECESSITA DE UMA AVALIAÇÃO PARA ENCERRAMENTO!'.
    ELSE.
      vl_titulo = 'AVISO! O MDF-E ABAIXO FOI ENCERRADO!'.
    ENDIF.

    "Monta Corpo Email
    conc_html '<html>'.
    conc_html '<head><title>'.
    conc_html    lv_title_email.
    conc_html '</title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
    conc_html '<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.
    conc_html '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>'.
    conc_html    vl_titulo.
    conc_html '</STRONG></FONT></DIV><BR>'.
    conc_html '<FONT face=Verdana color=#0000ff size=2>'.
    conc_html '<BR>'.

    conc_html '<table cellspacing="0" border="1" bordercolor="FFFFFF" width="100%">'.

    conc_html    '<tr bgcolor="B0C4DE">'.
    conc_html       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Num. MDF-e</b></font></p></td>'.
    conc_html       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Documento MDF-e </b></font></p></td>'.
    conc_html       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Filial</b></font></p></td>'.
    conc_html       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Data Emissão</b></font></p></td>'.
    conc_html       '<td width="20%"><p align="center"><font color="#000030" size=1><b>Placas</b></font></p></td>'.
    conc_html       '<td width="20%"><p align="center"><font color="#000030" size=1><b>CT-e/NF-e(s) Vinculados</b></font></p></td>'.

    IF lwa_mdfe_atual IS NOT INITIAL.
      conc_html     '<td width="20%"><p align="center"><font color="#000030" size=1><b>Novo Documento MDF-e</b></font></p></td>'.
      conc_html     '<td width="20%"><p align="center"><font color="#000030" size=1><b>Filial Novo MDF-e</b></font></p></td>'.
      conc_html     '<td width="20%"><p align="center"><font color="#000030" size=1><b>Placas Novo MDF-e</b></font></p></td>'.
    ENDIF.

    conc_html    '</tr>'.


    SELECT *
      FROM zsdt0105 INTO TABLE lit_zsdt0105
     WHERE docnum_ref = wa_zsdt0102-docnum.

    CONCATENATE wa_doc_mdfe-docdat+6(2) '/' wa_doc_mdfe-docdat+4(2) '/' wa_doc_mdfe-docdat(4) INTO DATA(vl_data).

    conc_html  '<tr bordercolor="black">'.

    conc_html     '<td width="20%"><p align="center"> <font color="000" size=1><b>'.
    conc_html        wa_zsdt0102-nmdfe.
    conc_html     '</b></font></p></td>'.

    conc_html     '<td width="20%"><p align="center"> <font color="000" size=1><b>'.
    conc_html        wa_zsdt0102-docnum.
    conc_html     '</b></font></p></td>'.

    conc_html     '<td width="20%"><p align="center"> <font color="000" size=1><b>'.
    conc_html        wa_doc_mdfe-branch.
    conc_html     '</b></font></p></td>'.

    conc_html     '<td width="20%"><p align="center"> <font color="000" size=1><b>'.
    conc_html        vl_data.
    conc_html     '</b></font></p></td>'.

    "Placas
    conc_html     '<td width="20%"><p align="center"> <font color="000" size=1><b>'.

    conc_html         '<table>'.

    DATA(lit_placas) = zcl_mdfe=>get_placas_mdfe_by_docnum( i_docnum = wa_zsdt0102-docnum ).

    LOOP AT lit_placas INTO DATA(lwa_veiculo).
      conc_html           '<tr bordercolor="black">'.
      conc_html              '<td width="100%"><p align="center"> <font color="000" size=1><b>'.
      conc_html                 lwa_veiculo-pc_veiculo.
      conc_html              '</b></font></p></td>'.
      conc_html           '</tr>'.
    ENDLOOP.

    conc_html         '</table>'.
    conc_html     '</b></font></p></td>'.

    "Documentos Vinculados
    conc_html     '<td width="20%"><p align="left"> <font color="000" size=1><b>'.

    conc_html         '<table>'.

    LOOP AT lit_zsdt0105 INTO DATA(wa_zsdt0105).
      conc_html           '<tr bordercolor="black">'.
      conc_html              '<td width="100%"><p align="left"> <font color="000" size=1><b>'.
      conc_html                 wa_zsdt0105-docnum.
      conc_html              '</b></font></p></td>'.
      conc_html           '</tr>'.
    ENDLOOP.

    conc_html         '</table>'.
    conc_html     '</b></font></p></td>'.


    IF lwa_mdfe_atual IS NOT INITIAL.
      conc_html   '<td width="20%"><p align="center"> <font color="000" size=1><b>'.
      conc_html      lwa_mdfe_atual-docnum.
      conc_html   '</b></font></p></td>'.

      conc_html   '<td width="20%"><p align="center"> <font color="000" size=1><b>'.
      conc_html      lwa_mdfe_atual_doc-branch.
      conc_html   '</b></font></p></td>'.

      "Placas
      conc_html   '<td width="20%"><p align="center"> <font color="000" size=1><b>'.

      conc_html         '<table>'.

      DATA(lit_placas_mdfe_atual) = zcl_mdfe=>get_placas_mdfe_by_docnum( i_docnum = lwa_mdfe_atual-docnum ).

      LOOP AT lit_placas_mdfe_atual INTO DATA(lwa_veiculo_atual).
        conc_html           '<tr bordercolor="black">'.
        conc_html              '<td width="100%"><p align="center"> <font color="000" size=1><b>'.
        conc_html                 lwa_veiculo_atual-pc_veiculo.
        conc_html              '</b></font></p></td>'.
        conc_html           '</tr>'.
      ENDLOOP.

      conc_html         '</table>'.
      conc_html     '</b></font></p></td>'.
    ENDIF.


    conc_html  '</tr>'.
    conc_html '</table>'.

    conc_html '<BR>'.
    conc_html '<BR>'.
    conc_html '<DIV align=left>'.

    conc_html '<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>E-mail gerado automáticamente pelo sistema</STRONG></FONT></DIV>'.

    conc_html '</DIV>'.
    conc_html '<BR>'.
    conc_html '</body>'.
    conc_html '</html>'.

    "Corpo
    doc_chng-obj_name  = lv_title_email.
    doc_chng-obj_descr = lv_title_email.
    doc_chng-no_change = 'X'.

    CLEAR lwa_objpack-transf_bin.
    lwa_objpack-head_start = 1.
    lwa_objpack-head_num = 0.
    lwa_objpack-body_start = 1.
    lwa_objpack-body_num = 99999.
    lwa_objpack-doc_type = 'HTM'.
    APPEND lwa_objpack TO objpack.

    "Enviar
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = objpack
        contents_txt               = it_html
        receivers                  = reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        operation_no_authorization = 4
        OTHERS                     = 99.

  ENDMETHOD.


  METHOD print_mdfe.

* Dados MDF-e
    DATA: wa_zsdt0102 TYPE zsdt0102.
    DATA: vl_ds_url TYPE agr_url.

    SELECT SINGLE *
      INTO wa_zsdt0102
      FROM zsdt0102
     WHERE docnum = me->at_docnum
       AND nmdfe  = me->at_nmdf.

    IF NOT ( wa_zsdt0102-url_sefaz IS INITIAL ).
      vl_ds_url = wa_zsdt0102-url_sefaz.
    ELSE.
      MESSAGE w044.
      "MESSAGE 'Impressão não pronta!' TYPE 'W'.
      RETURN.
    ENDIF.
    "{Begin of change - CS2023000094 - 30.03.2023}
    IF wa_zsdt0102-encerrado EQ abap_true.
      IF i_imprimir EQ abap_true.
        CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
          EXPORTING
            node_data = vl_ds_url. "Alterar para o formulario com marca d'agua
      ENDIF.
    ELSE.

      IF i_imprimir EQ abap_true.
        CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
          EXPORTING
            node_data = vl_ds_url.
      ENDIF.

    ENDIF.
    "{End of change - CS2023000094 - 30.03.2023}



    e_url = vl_ds_url.

  ENDMETHOD.


  METHOD registra_email_notificacao_enc.

    DATA: lwa_zsdt0296 TYPE zsdt0296.

    CLEAR: lwa_zsdt0296.

    CHECK ( i_docnum IS NOT INITIAL ) AND ( i_email IS NOT INITIAL ).

    lwa_zsdt0296-docnum = i_docnum.
    lwa_zsdt0296-email  = i_email.

    MODIFY zsdt0296 FROM lwa_zsdt0296.

  ENDMETHOD.


  METHOD set_cnpj_emi.

    me->at_cnpj_emi = i_cnpj_emi.

  ENDMETHOD.


  METHOD set_cunid.
    me->at_cunid = i_cunid.

    CASE me->at_cunid.
      WHEN '01'.
        me->at_cunid_sap = 'KG'.
      WHEN '02'.
        me->at_cunid_sap = 'TO'.
    ENDCASE.

  ENDMETHOD.


  METHOD set_dados_mdfe.

    DATA: wa_doc_item       TYPE j_1bnflin,
          wa_fatura_servico TYPE vbrp,
          wa_ordem_venda    TYPE vbak,
          wa_doc_transp     TYPE vttk,
          wa_lfa1           TYPE lfa1,
          wa_header         TYPE j_1bnfdoc,
          wa_partner        TYPE j_1bnfnad,
          wa_j_1binnad      TYPE j_1binnad,
          wa_zlest0061      TYPE zlest0061,
          wa_zlest0066      TYPE zlest0066,
          wa_parceiros      TYPE zcte_parceiros,
          wa_identifica     TYPE zcte_identifica,
          wa_notas_info     TYPE zcte_info_nota,
          ls_ttxd           TYPE ttxd,
          v_rom_final       TYPE zsdt0001.

    DATA: var_lifnr    TYPE lfa1-lifnr,
          vl_ie        TYPE string,
          vl_cnpj      TYPE string,
          vl_cunid     TYPE string,
          vl_cunid_sap TYPE string,
          vl_cunip     TYPE string.

    CLEAR: wa_header, wa_doc_item, wa_fatura_servico, wa_ordem_venda,
           wa_doc_transp, wa_lfa1, var_lifnr, vl_ie,
           vl_cunip.

    DATA: it_j_1bnfnad TYPE j_1bnfnad_tab,
          wl_j_1bnfnad LIKE LINE OF it_j_1bnfnad.

    DATA: it_j_1bad TYPE TABLE OF j_1bad,
          wl_j_1bad TYPE j_1bad.

    ls_ttxd-leng1 = '03'.

    IF me->at_tp_doc_ref = '2'.  "NF-e

      SELECT SINGLE *
        FROM j_1bnfdoc INTO wa_header
       WHERE docnum EQ i_docnum.

      CHECK sy-subrc = 0.

      SELECT SINGLE *
        FROM j_1bnflin INTO wa_doc_item
       WHERE docnum EQ i_docnum.

      CHECK sy-subrc = 0.

      SELECT *
    FROM j_1bad INTO TABLE it_j_1bad.

      vl_ie  = wa_header-ie_bupla.
      REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN  vl_ie WITH ''.

      me->at_ie_emi   = vl_ie.
      me->at_cnpj_emi = wa_header-cnpj_bupla.
      me->at_modal    = '01'. "Rodoviário.

      IF wa_doc_item-meins IS NOT INITIAL.
        vl_cunip =  wa_doc_item-meins.
      ELSE.
        vl_cunip =  wa_header-gewei.
      ENDIF.

      CASE vl_cunip.
        WHEN 'KG'.
          vl_cunid = '01'.
        WHEN 'TO'.
          vl_cunid = '02'.
        WHEN 'UN'.
          vl_cunid = '03'.
        WHEN 'LT'.
          vl_cunid = '04'.
      ENDCASE.

      vl_cunid_sap = vl_cunip.

      IF me->at_cunid IS INITIAL. "Quando não atribuido manualmente pelo Usuario
        me->at_cunid = vl_cunid.
      ENDIF.

      IF me->at_cunid_sap IS INITIAL.
        me->at_cunid_sap = vl_cunid_sap.
      ENDIF.

      it_j_1bnfnad[] = me->get_parceiros_documento( i_docnum = i_docnum ).

      "Municipio Ini |------------------------------------------------------------|
      CLEAR: wl_j_1bnfnad.

      READ TABLE it_j_1bnfnad INTO wl_j_1bnfnad
        WITH KEY parvw = 'PC'.

      IF sy-subrc NE 0.
        CLEAR: wl_j_1bnfnad.

        wl_j_1bnfnad-partyp = 'V'.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_header-branch
          IMPORTING
            output = wl_j_1bnfnad-parid.
      ENDIF.

      IF ( wl_j_1bnfnad-partyp IS NOT INITIAL ) AND ( wl_j_1bnfnad-parid IS NOT INITIAL ).
        CALL FUNCTION 'J_1B_NF_PARTNER_READ'
          EXPORTING
            partner_type           = wl_j_1bnfnad-partyp
            partner_id             = wl_j_1bnfnad-parid
            read_address           = 'X'
          IMPORTING
            parnad                 = wa_j_1binnad
          EXCEPTIONS
            partner_not_found      = 1
            partner_type_not_found = 2
            OTHERS                 = 3.
        IF sy-subrc = 0.
          me->at_ufini     = wa_j_1binnad-regio.
          me->at_cmunini   = wa_j_1binnad-txjcd+ls_ttxd-leng1.
          me->at_nmunini   = wa_j_1binnad-ort01.
        ENDIF.
      ENDIF.

      "Municipio Fim |-------------------------------------------------------------|

      CLEAR: wl_j_1bnfnad.

      READ TABLE it_j_1bnfnad INTO wl_j_1bnfnad
        WITH KEY parvw = 'LR'.

      "Busca Local de Entrega (Municipio Encerramento)
      IF sy-subrc NE 0.
        CLEAR: wl_j_1bnfnad.
        READ TABLE it_j_1bnfnad INTO wl_j_1bnfnad
          WITH KEY parvw  = 'WE'.
      ENDIF.

      IF sy-subrc NE 0.
        CLEAR: wl_j_1bnfnad.

        IF wa_header-partyp = 'C'. "Cliente
          wl_j_1bnfnad-partyp = wa_header-partyp.
          wl_j_1bnfnad-parid  = wa_header-parid.
        ENDIF.
      ENDIF.

      IF ( wl_j_1bnfnad-partyp IS NOT INITIAL ) AND ( wl_j_1bnfnad-parid IS NOT INITIAL ).
        CALL FUNCTION 'J_1B_NF_PARTNER_READ'
          EXPORTING
            partner_type           = wl_j_1bnfnad-partyp
            partner_id             = wl_j_1bnfnad-parid
            read_address           = 'X'
          IMPORTING
            parnad                 = wa_j_1binnad
          EXCEPTIONS
            partner_not_found      = 1
            partner_type_not_found = 2
            OTHERS                 = 3.
        IF sy-subrc = 0.
          me->at_uffim     = wa_j_1binnad-regio.
          me->at_cmunfim   = wa_j_1binnad-txjcd+ls_ttxd-leng1.
          me->at_nmunfim   = wa_j_1binnad-ort01.
          me->at_mun_enc   = wa_j_1binnad-txjcd+ls_ttxd-leng1.
        ENDIF.
      ENDIF.

      RETURN.
    ENDIF.

    "-------------------------------------------------------------------
    " Aquaviário
    "-------------------------------------------------------------------

    "Verificar se o docnum esta gravado na tabela do aquaviário.
    SELECT SINGLE *
      INTO wa_zlest0061
      FROM zlest0061
     WHERE docnum EQ i_docnum.

    IF sy-subrc = 0.

      CALL FUNCTION 'Z_LES_INFO_AQUAV'
        EXPORTING
          p_cte_avulso = i_docnum
        IMPORTING
          p_zlest0066  = wa_zlest0066.

      vl_ie   = wa_zlest0066-ie_emite.
      REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN  vl_ie WITH ''.
      me->at_ie_emi    = vl_ie.
      me->at_cnpj_emi  = wa_zlest0066-cnpj_emite.
      me->at_modal     = '03'.
      me->at_cunid     = wa_zlest0066-cunid.
      me->at_cunid_sap = wa_zlest0066-cunid_sap.

      "Ini Municio
      me->at_ufini     = wa_zlest0066-ufini.
      me->at_cmunini   = wa_zlest0066-cmunini.
      me->at_nmunini   = wa_zlest0066-xmunini.

      "Fim Municipio
      me->at_uffim     = wa_zlest0066-uffim.
      me->at_cmunfim   = wa_zlest0066-cmunfim.
      me->at_nmunfim   = wa_zlest0066-xmunfim.
      me->at_mun_enc   = wa_zlest0066-cmunfim.
      RETURN.
    ENDIF.

    "-------------------------------------------------------------------
    " Rodoviário
    "-------------------------------------------------------------------
    SELECT SINGLE *
      INTO wa_parceiros
      FROM zcte_parceiros
     WHERE docnum = i_docnum.

    SELECT SINGLE *
      INTO wa_identifica
      FROM zcte_identifica
     WHERE docnum = i_docnum.

    SELECT SINGLE *
      INTO wa_notas_info
      FROM zcte_info_nota
     WHERE docnum = i_docnum.

    CLEAR: vl_cunid.
    CASE wa_notas_info-unidade.
      WHEN 'KG'.
        vl_cunid = '01'.
      WHEN 'TO'.
        vl_cunid = '02'.
      WHEN 'UN'.
        vl_cunid = '03'.
      WHEN 'LT'.
        vl_cunid = '04'.
    ENDCASE.
    vl_cunid_sap = wa_notas_info-unidade.

    vl_ie = wa_parceiros-emit_ie.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN  vl_ie WITH ''.

    me->at_ie_emi    = vl_ie.
    me->at_cnpj_emi  = wa_parceiros-emit_cnpj.
    me->at_modal     = wa_identifica-modal.
    me->at_cunid     = vl_cunid.
    me->at_cunid_sap = vl_cunid_sap.
    me->at_mun_enc   = wa_identifica-cmunfim.

    "Verificar se Documento esta vinculado a uma carga com mais de um destinatario...
    IF wa_identifica-tknum IS NOT INITIAL.
      SELECT SINGLE *
FROM zsdt0001 INTO @DATA(_wl_0001)
WHERE doc_transp = @wa_identifica-tknum.

      IF sy-subrc EQ 0.
        CLEAR: v_rom_final.

        CALL METHOD zcl_romaneio=>get_ck_rom_final
          EXPORTING
            i_chv_rom_carga = _wl_0001-ch_referencia
          IMPORTING
            e_rom_final     = v_rom_final.

        IF v_rom_final-doc_transp IS NOT INITIAL.
          SELECT SINGLE *
FROM zcte_identifica INTO wa_identifica
WHERE tknum = _wl_0001-doc_transp.

          IF sy-subrc EQ 0.
            me->at_mun_enc = wa_identifica-cmunfim.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.



  ENDMETHOD.


  METHOD set_data_emi.


    me->at_data_emi = data.

  ENDMETHOD.


  METHOD set_docnum.

    me->at_docnum = i_docnum.

  ENDMETHOD.


  METHOD set_hora_emi.

    me->at_hora_emi = hora.

  ENDMETHOD.


  METHOD set_id_local_coleta.

    me->at_id_local_coleta = i_id_local_coleta.

  ENDMETHOD.


  METHOD set_id_local_descarga.

    me->at_id_local_descarga = i_id_local_descarga.

  ENDMETHOD.


  METHOD set_ie_emi.

    me->at_ie_emi = i_ie_emi.

  ENDMETHOD.


  METHOD set_just_canc.
    me->at_just_canc = i_just.
  ENDMETHOD.


  METHOD set_modal.

    me->at_modal = i_modal.

  ENDMETHOD.


  METHOD set_motorista.
    me->at_motorista = i_motorista.
  ENDMETHOD.


  METHOD set_mun_enc.

    me->at_mun_enc = i_mun_enc.

  ENDMETHOD.


  METHOD set_nmdf.
************************************
*  Método de Configuração
*  Atributo: AT_NMDF
*  Parâmetro: NMDF
*  Descrição: Método para configurar o número do MDF-e.
*  Developer: Victor Hugo Souza Nunes
*  26.11.2015 13:58:12
************************************
    "ME->VERIFICA_EXISTE( EXPORTING I_NMDF  = NMDF
    "                     RECEIVING E_RNMDF = ME->AT_NMDF
    "                     ).

    me->at_nmdf = nmdf.

  ENDMETHOD.


  METHOD set_parceiros.

    SELECT SINGLE * INTO @DATA(wa_act_nota)
      FROM j_1bnfe_active
     WHERE docnum EQ @i_docnum.

    CHECK sy-subrc IS INITIAL.

    CASE wa_act_nota-model.
      WHEN zif_doc_eletronico=>at_st_model_nfe.

        SELECT * INTO TABLE @DATA(it_j_1bnfnad)
          FROM j_1bnfnad
         WHERE docnum EQ @i_docnum
           AND parvw  IN ('PC','LR').

        "Fornecedor (Ponto de Coleta)
        READ TABLE it_j_1bnfnad INTO DATA(wa_j_1bnfnad) WITH KEY parvw = 'PC'.
        IF sy-subrc IS INITIAL.
          me->at_id_local_coleta = wa_j_1bnfnad-parid.
        ENDIF.

        "Cliente (Ponto de Entrega)
        READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad WITH KEY parvw = 'LR'.
        IF sy-subrc IS INITIAL.
          me->at_id_local_descarga = wa_j_1bnfnad-parid.
        ENDIF.

      WHEN zif_doc_eletronico=>at_st_model_cte.

        SELECT * INTO TABLE @it_j_1bnfnad
          FROM j_1bnfnad
         WHERE docnum EQ @i_docnum
           AND parvw  IN ('T2','T3').

        "Fornecedor (Ponto de Coleta)
        READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad WITH KEY parvw = 'T2'.
        IF sy-subrc IS INITIAL.
          me->at_id_local_coleta = wa_j_1bnfnad-parid.
        ENDIF.

        "Cliente (Ponto de Entrega)
        READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad WITH KEY parvw = 'T3'.
        IF sy-subrc IS INITIAL.
          me->at_id_local_descarga = wa_j_1bnfnad-parid.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD set_placa_car1.
    me->at_placa_car1 = i_placa_car1.
  ENDMETHOD.


  METHOD set_placa_car2.
    me->at_placa_car2 = i_placa_car2.
  ENDMETHOD.


  METHOD set_placa_car3.
    me->at_placa_car3 = i_placa_car3.
  ENDMETHOD.


  METHOD set_placa_cav.
    me->at_placa_cav = i_placa_cav.
  ENDMETHOD.


  METHOD set_qcarga.

    me->at_qcarga = i_qcarga.

  ENDMETHOD.


  METHOD set_serie.
************************************
*  Método de Configuração
*  Atributo: AT_SERIE
*  Parâmetro: SERIE
*  Descrição: Método para configurar a serie do MDF-e.
*  Developer: Victor Hugo Souza Nunes
*  26.11.2015 13:59:11
************************************
    me->at_serie = serie.
  ENDMETHOD.


  METHOD set_tp_doc_ref.
    me->at_tp_doc_ref = i_tp_doc_ref.
  ENDMETHOD.


  METHOD set_vcarga.
    me->at_vcarga = i_vcarga.
  ENDMETHOD.


  METHOD valida_cancelamento_troca_nota.

    DATA: l_troca_nota TYPE char1.

    e_erro = abap_false.

    FREE: at_wa_doc_mdfe.

*--------------------------------------
*-- obtem NF a cancelar
*--------------------------------------
    READ TABLE at_it_doc_mdfe INTO at_wa_doc_mdfe INDEX 1.

    CHECK sy-subrc = 0.

*--------------------------------------
*-- NF
*--------------------------------------
    SELECT SINGLE model
    INTO @DATA(l_model)
    FROM j_1bnfdoc
WHERE docnum = @at_wa_doc_mdfe-docnum.

    CHECK sy-subrc = 0.

    CASE l_model.

      WHEN '57'.
        SELECT tknum
 INTO @DATA(l_tknum)
 FROM zcte_ciot
   UP TO 1 ROWS
WHERE docnum = @at_wa_doc_mdfe-docnum.
        ENDSELECT.

        SELECT vbeln
 INTO @DATA(l_vbeln)
 FROM vttp
   UP TO 1 ROWS
WHERE tknum = @l_tknum.
        ENDSELECT.

        SELECT *
INTO @DATA(w_zsdt0001)
FROM zsdt0001
UP TO 1 ROWS
WHERE doc_rem = @l_vbeln.
        ENDSELECT.

      WHEN '55'.
        SELECT *
INTO @DATA(w_j_1bnflin)
FROM j_1bnflin
UP TO 1 ROWS
WHERE docnum = @at_wa_doc_mdfe-docnum.
        ENDSELECT.

        CASE w_j_1bnflin-reftyp.

          WHEN 'BI'.
            SELECT SINGLE vbelv
          INTO @DATA(l_remessa)
          FROM vbfa
WHERE vbeln   = @w_j_1bnflin-refkey(10)
AND vbtyp_n = 'M'
AND vbtyp_v = 'J'.

            IF sy-subrc IS INITIAL AND l_remessa IS NOT INITIAL.
              SELECT *
      INTO @w_zsdt0001
      FROM zsdt0001
        UP TO 1 ROWS
WHERE doc_rem EQ @l_remessa
AND doc_rem NE @space.
              ENDSELECT.
            ENDIF.

          WHEN 'MD'.
            SELECT xblnr_mkpf
              INTO l_remessa
              FROM mseg
                UP TO 1 ROWS
WHERE mblnr = w_j_1bnflin-refkey(10)
AND mjahr = w_j_1bnflin-refkey+10(4)
AND zeile = '0001'.
            ENDSELECT.

            IF sy-subrc IS INITIAL AND l_remessa IS NOT INITIAL.
              SELECT *
       INTO w_zsdt0001
       FROM zsdt0001
         UP TO 1 ROWS
WHERE doc_rem EQ l_remessa
AND doc_rem NE space.
              ENDSELECT.
            ENDIF.
        ENDCASE.
    ENDCASE.

    l_troca_nota =  zcl_faturamento=>zif_faturamento~get_romaneio_trocanota(
                     EXPORTING i_ch_referencia = w_zsdt0001-ch_referencia ).

    IF w_zsdt0001 IS NOT INITIAL.
      IF l_troca_nota                     = abap_true AND
         w_zsdt0001-docs_enviado_carguero = abap_true.
        e_erro = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD verifica_existe.

    DATA: lw_zsdt0102 TYPE zsdt0102.

    SELECT SINGLE * FROM zsdt0102 INTO lw_zsdt0102 WHERE nmdfe EQ i_nmdf.

    IF ( sy-subrc NE 0 ).
      e_rnmdf = i_nmdf.
    ENDIF.

  ENDMETHOD.


  METHOD check_contingencia_mdfe.

    r_ativo = abap_false.

    SELECT SINGLE *
     FROM zdrct0002
INTO @DATA(ls_0002).

    IF sy-subrc = 0 AND ls_0002-job_conting_mdfe = abap_true.
      r_ativo = abap_true.

      SELECT SINGLE *
       FROM zdrct0006
  INTO @DATA(ls_0006)
WHERE branch  = @i_branch
AND usuario = @sy-uname.

      IF sy-subrc = 0.
        r_ativo = abap_false.
      ELSE.
        SELECT SINGLE *
         FROM zdrct0006
         INTO ls_0006
WHERE branch  = i_branch
AND usuario = abap_off.

        IF sy-subrc = 0.
          r_ativo = abap_false.
        ELSE.
          SELECT SINGLE *
           FROM zdrct0006
           INTO ls_0006
 WHERE branch  = abap_off
  AND usuario = sy-uname.

          IF sy-subrc = 0.
            r_ativo = abap_false.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_conting_mdfe_reenvio.

    r_ativo = abap_false.

    SELECT SINGLE contingencia
       FROM zsdt0102
   INTO @DATA(wa_contingencia)
  WHERE docnum  = @i_docnum.

    IF wa_contingencia IS NOT INITIAL.

      r_ativo = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD set_gravar_uf_perc.

    DATA: it_zsdt0104 TYPE TABLE OF zsdt0104,
          wa_zsdt0104 TYPE zsdt0104,
          lv_idx_uf   TYPE i.

    CLEAR lv_idx_uf.

    SELECT SINGLE *
      FROM zsdt0102
 INTO @DATA(w_0102)
WHERE docnum = @i_docnum.

    CHECK sy-subrc = 0.

    DELETE FROM zsdt0104 WHERE docnum   = i_docnum.

    LOOP AT at_it_uf_perc            INTO at_wa_uf_perc WHERE uf IS NOT INITIAL.
      CLEAR wa_zsdt0104.
      lv_idx_uf                         = lv_idx_uf + 1.

      MOVE-CORRESPONDING at_wa_uf_perc TO wa_zsdt0104.
      wa_zsdt0104-nmdfe                 = w_0102-nmdfe.
      wa_zsdt0104-docnum                = w_0102-docnum.
      wa_zsdt0104-ordem_uf              = lv_idx_uf.
      MODIFY zsdt0104                FROM wa_zsdt0104.
    ENDLOOP.

    COMMIT WORK AND WAIT.


  ENDMETHOD.


  METHOD set_validar_uf.

    FREE: e_erro.

    SELECT SINGLE *
      FROM t005s
  INTO @DATA(t005s)
 WHERE land1 = 'BR'
 AND bland = @i_uf.

    IF sy-subrc <> 0.
      e_erro = abap_true.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
