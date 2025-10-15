class ZCL_COMERCIALIZACAO_ALGODAO definition
  public
  final
  create public .

public section.

  class-methods PROCESSAR_DISP_FARDOS
    importing
      !I_SAFRA type CHARG_D
      !I_FILIAL_ALGODOEIRA type WERKS_D
      !I_BLOCO type LGORT_D
      !I_MATNR type MATNR
      !I_FARDOS_DISPONIBILIZAR type ZPPS0010_T optional
      !I_LGORT_DESTINO type LGORT_D
      !I_CARREGAMENTO_AUTO type CHAR01 optional
      !I_ESTORNO type CHAR01 optional
    exporting
      !E_FARDOS_BLOCO_TRACE_COTTON type ZPPS0007_T
      !E_CHARG_PROC type CHARG_D
      !E_QTDE_DISP type LABST
      !E_FARDOS_DISPONIBILIZAR type ZPPS0007_T
      value(E_MSG_ERROR) type STRING
      !E_ERROR_LOCK type CHAR01 .
  class-methods DISP_FARDOS_COMERCIALIZAR
    importing
      !I_SAFRA type CHARG_D
      !I_FILIAL_ALGODOEIRA type WERKS_D
      !I_BLOCO type LGORT_D
      !I_MATNR type MATNR
      !I_FARDOS_DISPONIBILIZAR type ZPPS0010_T optional
      !I_LGORT_DESTINO type LGORT_D
      !I_CARREGAMENTO_AUTO type CHAR01 optional
    exporting
      !E_MSG_SUCESSO type STRING
      !E_MBLNR type MBLNR
      !E_MJAHR type MJAHR
      !E_MSG_ERROR type STRING
      !E_ERROR_LOCK type CHAR1 .
  class-methods ESTORNAR_DISP_FARDOS_COMERC
    importing
      !I_SAFRA type CHARG_D
      !I_FILIAL_ALGODOEIRA type WERKS_D
      !I_BLOCO type LGORT_D
      !I_MATNR type MATNR
      !I_FARDOS_ESTORNAR type ZPPS0010_T optional
      !I_CARREGAMENTO_AUTO type CHAR01 optional
    exporting
      !E_MSG_SUCESSO type STRING
      !E_MBLNR type MBLNR
      !E_MJAHR type MJAHR
      !E_MSG_ERROR type STRING
      !E_ERROR_LOCK type CHAR1 .
  class-methods NOTIFICAR_CARREGAMENTO_TRACE
    importing
      !I_ZMMT0008 type ZMMT0008 optional
      !I_ZMMT0008_DELETE type ZMMT0008_DELETE optional
      !I_CARREGAMENTO_AUTO type CHAR01 optional .
  class-methods CANCELAR_ESPELHO_ROMANEIO
    importing
      !I_WERKS type WERKS_D
      !I_LGORT type LGORT_D
      !I_SAFRA type CHARG_D
      !I_NR_ROMANEIO type ZNR_ROMANEIO
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods REMOVE_FARDOS_REPLICADOS
    importing
      !I_ZMMT0008_T type ZMMT0008_T
      !I_NR_ROMANEIO type ZNR_ROMANEIO
      !I_NR_SAFRA type ZNR_SAFRA
      !I_BRANCH type J_1BBRANC_
    returning
      value(R_UTILIZADO) type CHAR01 .
  class-methods IMPRIMIR_ESPELHO_ROMANEIO
    importing
      !I_WERKS type WERKS_D
      !I_LGORT type LGORT_D
      !I_SAFRA type CHARG_D
      !I_NR_ROMANEIO type ZNR_ROMANEIO
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GET_FARDOS_ESPELHO_ROMANEIO
    importing
      !I_WERKS type WERKS_D
      !I_LGORT type LGORT_D
      !I_SAFRA type CHARG_D
      !I_NR_ROMANEIO type ZNR_ROMANEIO
    returning
      value(R_ZMMT0008_T) type ZMMT0008_T .
  class-methods PROCESSAR_ESPELHO_ROMANEIO
    importing
      !I_WERKS type WERKS_D
      !I_LGORT type LGORT_D
      !I_SAFRA type CHARG_D
      !I_NR_ROMANEIO type ZNR_ROMANEIO
      !I_MOTORISTA type CHAR50
      !I_PLACA type ZPLACA
      !I_KUNNR type KUNNR
      !I_FARDOS_ESPELHO type ZSDS091_T optional
      !I_PROC_UNICO type CHAR1 optional
      !I_REPROCESSAR type CHAR1 optional
      !I_ENVIA_EMAIL type CHAR1 optional
      !I_INTEGRA_RONDOLINE type CHAR1 optional
      !I_CARREGAMENTO_AUTO type CHAR1 optional
    exporting
      value(E_MSG_ERROR) type STRING
      value(E_FARDOS_PROCESSAR) type ZSDS091_T .
  class-methods GERAR_ESPELHO_ROMANEIO
    importing
      !I_WERKS type WERKS_D
      !I_LGORT type LGORT_D
      !I_SAFRA type CHARG_D
      !I_NR_ROMANEIO type ZNR_ROMANEIO
      !I_MOTORISTA type CHAR50
      !I_PLACA type ZPLACA
      !I_KUNNR type KUNNR
      !I_FARDOS_ESPELHO type ZSDS091_T optional
      !I_PROC_UNICO type CHAR1 optional
      !I_REPROCESSAR type CHAR1 optional
      !I_ENVIA_EMAIL type CHAR1 optional
      !I_INTEGRA_RONDOLINE type CHAR1 optional
      !I_CARREGAMENTO_AUTO type CHAR1 optional
    exporting
      value(E_MSG_ERROR) type STRING
      value(E_MSG_SUCESSO) type STRING .
  class-methods SEND_EMAIL_ESPELHO_ROMANEIO
    importing
      !I_WERKS type WERKS_D
      !I_LGORT type LGORT_D
      !I_SAFRA type CHARG_D
      !I_NR_ROMANEIO type ZNR_ROMANEIO
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods CADASTRO_EMAIL_ESPELHO_ROM
    importing
      !I_LOCAL_ENTREGA type ZMMT0126-KUNNR .
  class-methods CK_AJUSTE_SOBRA_PERDA_ROMANEIO
    importing
      !I_ID_CARGA type ZID_CARGA
      !I_CH_REFERENCIA type ZCH_REF
    exporting
      !E_MSG_SUCESSO type STRING
      !E_MBLNR type MBLNR
      !E_MJAHR type MJAHR
      !E_MSG_ERROR type STRING .
  class-methods GERA_SOBRA_PERDA_ROMANEIO
    importing
      !I_ZSDT0001 type ZSDT0001
      !I_ZSDT0330 type ZSDT0330 optional
      !I_MATNR type MATNR
      !I_WERKS type WERKS_D
      !I_SAFRA type CHARG_D
      !I_QTDE_MOV type MENGE_D
      !I_TIPO_MOVIMENTO type ZDE_TIPO_MOV_ESTOQUE
    exporting
      !E_MSG_SUCESSO type STRING
      !E_MBLNR type MBLNR
      !E_MJAHR type MJAHR
      !E_MSG_ERROR type STRING .
  class-methods GET_MOV_SOBRA_PERDA_ROMANEIO
    importing
      !I_CH_REFERENCIA type ZCH_REF
    exporting
      !E_MBLNR type MBLNR
      !E_MJAHR type MJAHR .
  class-methods GET_LIST_SOBRA_PERDA_ROMANEIO
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(R_MOV_EST_T) type ZSDT0001_MOV_EST_T .
  class-methods GET_QTDE_SOBRA_PERDA_ROMANEIO
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(E_QTDE_PERDA_SOBRA) type MENGE_D .
  class-methods ESTORNAR_SOBRA_PERDA_ROMANEIO
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GET_FARDOS_ROMANEIO
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(R_ZSDT0330_T) type ZSDT0330_T .
  class-methods REINICIALIZA_MOV_SOBRA_PERDA
    importing
      !I_CHAVE_REFERENCIA_ROM type ZCH_REF
    returning
      value(R_ZSDT0344_REINICIALIZADO) type ZSDT0344 .
  class-methods CK_AJUSTE_SOBRA_PERDA_PENDENTE
    importing
      !I_CHAVE_REFERENCIA_ROM type ZCH_REF .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_COMERCIALIZACAO_ALGODAO IMPLEMENTATION.


  METHOD cancelar_espelho_romaneio.

    TYPES: ty_selection TYPE STANDARD TABLE OF rsparams.

    DATA: lit_zmmt0008      TYPE zmmt0008_t.
    DATA: lva_msg_aux       TYPE string.
    DATA: lva_msg_popup     TYPE string.
    DATA: lva_resposta      TYPE c.
    DATA: lva_count_reg     TYPE i.
    DATA: lva_status_del TYPE zmmt0008-status,
          lwa_selection  TYPE rsparams,
          lit_selection  TYPE ty_selection.

    CLEAR: r_msg_error.

    IF i_nr_romaneio IS INITIAL.
      r_msg_error = 'É obrigatorio o preenchimento do campo Número do Romaneio'.
      RETURN.
    ENDIF.

    IF i_werks IS INITIAL.
      r_msg_error = 'É obrigatorio o preenchimento do campo Centro'.
      RETURN.
    ENDIF.

    IF i_lgort IS INITIAL.
      r_msg_error = 'É obrigatorio o preenchimento do campo Depósito'.
      RETURN.
    ENDIF.

    IF i_safra IS INITIAL.
      r_msg_error = 'É obrigatorio o preenchimento do campo Safra'.
      RETURN.
    ENDIF.

    lit_zmmt0008 = get_fardos_espelho_romaneio( i_werks       = i_werks
                                                i_lgort       = i_lgort
                                                i_safra       = i_safra
                                                i_nr_romaneio = i_nr_romaneio ).

    IF lit_zmmt0008[] IS INITIAL .
      r_msg_error = 'Dados não encontrados.'.
      RETURN.
    ENDIF.

    LOOP AT lit_zmmt0008 INTO DATA(lwa_zmmt0008).

      CASE lwa_zmmt0008-status.
        WHEN '3'.

          r_msg_error = 'O Romaneio está em processo de estorno no sistema Rondonline, por favor aguardar!'.
          RETURN.

        WHEN '1' OR '5' OR '2'.

          CLEAR: lva_msg_popup, lva_count_reg.

          CASE lwa_zmmt0008-status.
            WHEN '1'.

              lva_msg_popup = | O Romaneio { lwa_zmmt0008-nr_romaneio } da filial { lwa_zmmt0008-werks } do Bloco  { lwa_zmmt0008-lgort } |.
              lva_msg_popup = |{ lva_msg_popup } está em processo de envio ao sistema Rondonline..... Deseja prosseguir com o Cancelamento? |.

            WHEN '5'.

              lva_msg_popup = | O Romaneio { lwa_zmmt0008-nr_romaneio } da filial { lwa_zmmt0008-werks } do Bloco  { lwa_zmmt0008-lgort } |.
              lva_msg_popup = |{ lva_msg_popup }  não foi integrado com o sistema Rondonline... Deseja prosseguir com o Cancelamento?|.

            WHEN '2'.
          ENDCASE.

          IF lva_msg_popup IS NOT INITIAL.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Cancelamento de Fardos de Venda'
                text_question         = lva_msg_popup
                text_button_1         = 'Ok'
                icon_button_1         = '@0V@'
                text_button_2         = 'Cancelar'
                icon_button_2         = '@0W@'
                default_button        = '1'
                display_cancel_button = ' '
              IMPORTING
                answer                = lva_resposta.

            IF lva_resposta NE '1'.
              r_msg_error = 'Ação cancelada'.
              RETURN.
            ENDIF.

          ENDIF.

          lva_count_reg = REDUCE i( INIT x = 0 FOR wa IN lit_zmmt0008 WHERE ( status = lwa_zmmt0008-status ) NEXT x = x + 1 ).
          IF lva_count_reg <= 0.
            r_msg_error = 'Nenhum registro encontrado para cancelamento!'.
            RETURN.
          ENDIF.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Cancelamento de Fardos de Venda'
              text_question         = |Serão cancelados { lva_count_reg } fardos de venda...|
              text_button_1         = 'Ok'
              icon_button_1         = '@0V@'
              text_button_2         = 'Cancelar'
              icon_button_2         = '@0W@'
              default_button        = '1'
              display_cancel_button = ' '
            IMPORTING
              answer                = lva_resposta.

          IF lva_resposta NE '1'.
            r_msg_error = 'Ação cancelada'.
            RETURN.
          ENDIF.

          "Check se é Operação Intercopany ... Caso Positivo, Remove Fardos Replicados
          DATA(_fardos_utilizados) = remove_fardos_replicados( i_zmmt0008_t  = lit_zmmt0008
                                                               i_nr_romaneio = CONV #( i_nr_romaneio )
                                                               i_nr_safra    = CONV #( i_safra )
                                                               i_branch      = CONV #( i_werks ) ).

          IF _fardos_utilizados = abap_true.
            r_msg_error = 'Ação cancelada'.
            RETURN.
          ENDIF.

          IF lwa_zmmt0008-status = '2'.

            LOOP AT lit_zmmt0008 ASSIGNING FIELD-SYMBOL(<fs_zmmt0008>) WHERE status = '2'.
              <fs_zmmt0008>-status           = '3'.
              <fs_zmmt0008>-dt_inicial_integ = sy-datum.
            ENDLOOP.

            lva_status_del = '3'.

          ELSE.

            LOOP AT lit_zmmt0008 ASSIGNING <fs_zmmt0008>.
              <fs_zmmt0008>-motorista   = space.
              <fs_zmmt0008>-nr_romaneio = space.
              <fs_zmmt0008>-vbeln       = space.
              <fs_zmmt0008>-vbeln_vf    = space.
              <fs_zmmt0008>-placa_cav   = space.
              <fs_zmmt0008>-nfnum       = space.
              <fs_zmmt0008>-kunnr       = space.
              <fs_zmmt0008>-integ_rondonline   = space.
              <fs_zmmt0008>-dt_inicial_integ = space.
            ENDLOOP.

            lva_status_del = lwa_zmmt0008-status.

          ENDIF.

          MODIFY zmmt0008 FROM TABLE lit_zmmt0008.
          MESSAGE |Foram cancelados { lva_count_reg } fardos de venda(s)!| TYPE 'S'.

          IF lwa_zmmt0008-status = '2'. "Estornar Integração Rondoline

            CLEAR: lit_selection[].

            CLEAR lwa_selection.
            lwa_selection-selname  = 'S_WERKS'.          "Option name
            lwa_selection-kind     = 'S'.                "S= select options P=Parameters
            lwa_selection-sign     = 'I'.                "Sign
            lwa_selection-option   = 'EQ'.               "Option
            lwa_selection-low      = lwa_zmmt0008-werks . "Value
            APPEND lwa_selection TO lit_selection.

            CLEAR lwa_selection.
            lwa_selection-selname  = 'S_LGORT'.
            lwa_selection-kind     = 'S'.
            lwa_selection-sign     = 'I'.
            lwa_selection-option   = 'EQ'.
            lwa_selection-low      = lwa_zmmt0008-lgort .
            APPEND lwa_selection TO lit_selection.

            CLEAR lwa_selection.
            lwa_selection-selname  = 'S_VBN_VF'.
            lwa_selection-kind     = 'S'.
            lwa_selection-sign     = 'I'.
            lwa_selection-option   = 'EQ'.
            lwa_selection-low      = lwa_zmmt0008-vbeln_vf .

            APPEND lwa_selection TO lit_selection.
            CLEAR lwa_selection.
            lwa_selection-selname  = 'S_PLCAV'.
            lwa_selection-kind     = 'S'.
            lwa_selection-sign     = 'I'.
            lwa_selection-option   = 'EQ'.
            lwa_selection-low      = lwa_zmmt0008-placa_cav .
            APPEND lwa_selection TO lit_selection.

            SELECT SINGLE *
              FROM tvarvc INTO @DATA(lwa_tvarv_integra_rondoline)
             WHERE name EQ 'ZMM0027_INTEGRA_RONDOLINE'.

            IF sy-subrc EQ 0.
              SUBMIT zmmr162 WITH SELECTION-TABLE lit_selection WITH s_acao EQ 'E' AND RETURN.
            ENDIF.

          ENDIF.

          DELETE lit_zmmt0008 WHERE status = lva_status_del.

      ENDCASE.

    ENDLOOP.


  ENDMETHOD.


  METHOD disp_fardos_comercializar.

    DATA: lva_material TYPE bapi2017_gm_head_ret-mat_doc,
          lva_year     TYPE bapi2017_gm_head_ret-doc_year,
          lva_code     TYPE bapi2017_gm_code.


    DATA: lwa_header   TYPE bapi2017_gm_head_01,
          lwa_item     TYPE bapi2017_gm_item_create,
          lit_item     TYPE TABLE OF bapi2017_gm_item_create,
          lit_return   TYPE TABLE OF bapiret2,
          lwa_return   TYPE bapiret2,
          lwa_zmmt0008 TYPE zmmt0008.

    DATA: lit_fardos_disponibilizar TYPE zpps0007_t,
          lva_msg_aux               TYPE string,
          lva_resposta              TYPE c.

    CLEAR: e_msg_error, e_msg_sucesso, e_mblnr, e_mjahr, lit_fardos_disponibilizar[], e_error_lock.

    IF i_fardos_disponibilizar[] IS INITIAL.
      e_msg_error = | Fardos para Disponibilizar para Comercialização não foram informados! |.
      RETURN.
    ENDIF.

    zcl_comercializacao_algodao=>processar_disp_fardos( EXPORTING i_safra                 = i_safra
                                                                  i_filial_algodoeira     = i_filial_algodoeira
                                                                  i_bloco                 = i_bloco
                                                                  i_matnr                 = i_matnr
                                                                  i_fardos_disponibilizar = i_fardos_disponibilizar
                                                                  i_lgort_destino         = i_lgort_destino
                                                                  i_carregamento_auto     = i_carregamento_auto
                                                        IMPORTING e_fardos_disponibilizar = lit_fardos_disponibilizar
                                                                  e_charg_proc            = DATA(lva_charg_proc)
                                                                  e_qtde_disp             = DATA(lva_qtde_transf)
                                                                  e_msg_error             = e_msg_error ).

    CHECK e_msg_error IS INITIAL.

    IF i_carregamento_auto EQ abap_false.

      DATA(_qtde_fardos_disp) = lines( i_fardos_disponibilizar ).

      lva_msg_aux = |Serão transferidos { _qtde_fardos_disp } fardos...|.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Transferencia de Fardos'
          text_question         = lva_msg_aux
          text_button_1         = 'Ok'
          icon_button_1         = '@0V@'
          text_button_2         = 'Cancelar'
          icon_button_2         = '@0W@'
          default_button        = '1'
          display_cancel_button = ' '
        IMPORTING
          answer                = lva_resposta.

      CHECK ( lva_resposta EQ 1 ).

    ENDIF.

*-------------------------------------------------------------------------------------------------------*
*   Iniciar Chamada BAPI
*-------------------------------------------------------------------------------------------------------*

    CLEAR: lva_material, lva_year, lit_item[], lit_return[], lwa_header.

*---------------------------------------------------------------------*
*  Preenchimento Dados Cabeçalho BAPI
*---------------------------------------------------------------------*
    lva_code              = '06'.
    lwa_header-pstng_date = sy-datum.
    lwa_header-doc_date   = sy-datum.


*---------------------------------------------------------------------*
* Preenchimento Dados Cabeçalho BAPI
*---------------------------------------------------------------------*

    CLEAR: lwa_item.
    lwa_item-move_type    = 'ZA1'.
    lwa_item-material     = i_matnr.
    lwa_item-plant        = i_filial_algodoeira.
    lwa_item-stge_loc     = i_bloco.
    lwa_item-batch        = lva_charg_proc.
    lwa_item-entry_qnt    = lva_qtde_transf.
    lwa_item-move_plant   = i_filial_algodoeira.
    lwa_item-move_stloc   = i_lgort_destino.
    lwa_item-move_mat     = i_matnr.
    lwa_item-move_batch   = i_safra.

    APPEND lwa_item TO lit_item.

    DO 5 TIMES.

      DATA(_index) = sy-index.

      CLEAR: lva_material, lva_year, lit_return[], e_error_lock.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = lwa_header
          goodsmvt_code    = lva_code
        IMPORTING
          materialdocument = lva_material
          matdocumentyear  = lva_year
        TABLES
          goodsmvt_item    = lit_item
          return           = lit_return.

      IF lva_material IS NOT INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        e_mblnr = lva_material.
        e_mjahr = lva_year.
        e_msg_sucesso = |Documento : { lva_material }  Ano: { lva_year } gerado com Sucesso!|.

        LOOP AT lit_fardos_disponibilizar INTO DATA(lwa_fardo_disp).
          CLEAR: lwa_zmmt0008.

          "Campos Chave
          lwa_zmmt0008-werks        = i_filial_algodoeira.
          lwa_zmmt0008-lgort        = i_bloco.
          lwa_zmmt0008-charg        = lwa_fardo_disp-nr_fardo_completo.
          lwa_zmmt0008-safra        = i_safra.

          "Demais Campos
          lwa_zmmt0008-cd_sai       = lwa_fardo_disp-cd_sai.
          lwa_zmmt0008-menge        = lwa_fardo_disp-peso_liquido.
          lwa_zmmt0008-matnr        = i_matnr.
          lwa_zmmt0008-lgortr       = i_lgort_destino.
          lwa_zmmt0008-tipo_fardo   = lwa_fardo_disp-tamanho_fardo.
          lwa_zmmt0008-mblnr        = lva_material.
          lwa_zmmt0008-mjahr        = lva_year.

          lwa_zmmt0008-dt_registro  = sy-datum.
          lwa_zmmt0008-hr_registro  = sy-uzeit.
          lwa_zmmt0008-us_registro  = sy-uname.

          MODIFY zmmt0008 FROM lwa_zmmt0008.

          COMMIT WORK.

          zcl_comercializacao_algodao=>notificar_carregamento_trace(
             i_zmmt0008              = lwa_zmmt0008
             i_carregamento_auto     = i_carregamento_auto
          ).

        ENDLOOP.

        RETURN.

      ELSE.

        READ TABLE  lit_return INTO lwa_return WITH KEY type = 'E'.
        IF sy-subrc EQ 0.

          CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
            EXPORTING
              id       = lwa_return-id
              number   = lwa_return-number
            IMPORTING
              is_block = e_error_lock.


          IF e_error_lock IS NOT INITIAL AND _index LE 5.
            WAIT UP TO 3 SECONDS.
          ELSE.
            e_msg_error = lwa_return-message.
            RETURN.
          ENDIF.

        ELSE.
          e_msg_error = 'Houve um erro desconhecido ao gerar transferencia!'.
          RETURN.
        ENDIF.

      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD estornar_disp_fardos_comerc.

    DATA: lva_material TYPE bapi2017_gm_head_ret-mat_doc,
          lva_year     TYPE bapi2017_gm_head_ret-doc_year,
          lva_code     TYPE bapi2017_gm_code.


    DATA: lwa_header           TYPE bapi2017_gm_head_01,
          lwa_item             TYPE bapi2017_gm_item_create,
          lit_item             TYPE TABLE OF bapi2017_gm_item_create,
          lit_return           TYPE TABLE OF bapiret2,
          lwa_return           TYPE bapiret2,
          lwa_zmmt0008         TYPE zmmt0008,
          lwa_zmmt0008_delete  TYPE zmmt0008_delete,
          lit_zmmt0008_estorno TYPE TABLE OF zmmt0008.


    DATA: lit_fardos_disponibilizar TYPE zpps0007_t,
          lva_msg_aux               TYPE string,
          lva_resposta              TYPE c,
          lva_qtde_estorno          TYPE mchb-clabs.

    CLEAR: e_msg_error, e_msg_sucesso, e_mblnr, e_mjahr, lit_fardos_disponibilizar[], e_error_lock, lit_zmmt0008_estorno[], lva_qtde_estorno.

    IF i_safra IS INITIAL.
      e_msg_error = | Safra não foi informada! |.
      RETURN.
    ENDIF.

    IF i_filial_algodoeira IS INITIAL.
      e_msg_error = | Filial Algodoeira não foi informada! |.
      RETURN.
    ENDIF.

    IF i_bloco IS INITIAL.
      e_msg_error = | Bloco não foi informado! |.
      RETURN.
    ENDIF.

    IF i_fardos_estornar[] IS INITIAL.
      e_msg_error = | Fardos para estornar não foram informados! |.
      RETURN.
    ENDIF.

    DATA(_charg_proc) = i_safra && '_' && i_filial_algodoeira.


    SELECT *
      FROM zmmt0008 INTO TABLE lit_zmmt0008_estorno
      FOR ALL ENTRIES IN i_fardos_estornar
     WHERE werks = i_filial_algodoeira
       AND lgort = i_bloco
       AND charg = i_fardos_estornar-nr_fardo_completo
       AND safra = i_safra.

    LOOP AT i_fardos_estornar INTO DATA(lwa_fardo_estornar).

      READ TABLE lit_zmmt0008_estorno INTO DATA(lwa_zmmt0008_estorno) WITH KEY charg = lwa_fardo_estornar-nr_fardo_completo.
      IF sy-subrc NE 0.
        e_msg_error = | Fardo { lwa_fardo_estornar-nr_fardo_completo } não foi disponibilizado para comercialização! |.
        RETURN.
      ENDIF.

      DATA(lva_deposito_destino) = lwa_zmmt0008_estorno-lgortr.

      IF i_carregamento_auto EQ abap_false. "Se não for pelo processo de carregamento automatico

        "Verificar se fardo esta vinculado ao processo de carregamento automatico
        SELECT SINGLE *
          FROM zsdt0330 INTO @DATA(lwa_zsdt0330)
         WHERE matnr     = @i_matnr
           AND werks     = @i_filial_algodoeira
           AND lgort     = @i_bloco
           AND acharg    = @lwa_fardo_estornar-nr_fardo_completo
           AND safra     = @i_safra
           AND cancelado = @abap_false.

        IF sy-subrc EQ 0.
          e_msg_error =  'Há fardos escolhidos que estão em Processamento Automático!'.
          RETURN.
        ENDIF.

      ENDIF.

      ADD lwa_zmmt0008_estorno-menge TO lva_qtde_estorno.
    ENDLOOP.

    IF lines( i_fardos_estornar  ) NE lines( lit_zmmt0008_estorno ).
      e_msg_error = | Quantidade Fardos para estornar diferente da Quantidade fardos disponibilizada para estoque!|.
      RETURN.
    ENDIF.

    DATA(lit_zmmt0008_estorno_aux) = lit_zmmt0008_estorno[].
    SORT lit_zmmt0008_estorno_aux BY lgortr.
    DELETE ADJACENT DUPLICATES FROM lit_zmmt0008_estorno_aux COMPARING lgortr.
    IF lines( lit_zmmt0008_estorno_aux[] ) > 1.
      "Se ocorrer essa trava, deve-se tratar os estorno individualmente, fazendo mais de uma movimentação de estorno
      e_msg_error = | Fardos foram transferidos para depositos diferentes! Operação Estorno não permitida|.
      RETURN.
    ENDIF.

    IF lva_deposito_destino IS INITIAL.
      e_msg_error = | Deposito destino não encontrado!|.
      RETURN.
    ENDIF.

    CHECK e_msg_error IS INITIAL.

    IF i_carregamento_auto EQ abap_false.

      DATA(_qtde_fardos_disp) = lines( i_fardos_estornar ).

      lva_msg_aux = |Será estornada a movimentação de { _qtde_fardos_disp } fardos...|.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Transferencia de Fardos'
          text_question         = lva_msg_aux
          text_button_1         = 'Ok'
          icon_button_1         = '@0V@'
          text_button_2         = 'Cancelar'
          icon_button_2         = '@0W@'
          default_button        = '1'
          display_cancel_button = ' '
        IMPORTING
          answer                = lva_resposta.

      CHECK ( lva_resposta EQ 1 ).

    ENDIF.

*-------------------------------------------------------------------------------------------------------*
*   Iniciar Chamada BAPI
*-------------------------------------------------------------------------------------------------------*

    CLEAR: lva_material, lva_year, lit_item[], lit_return[], lwa_header.

*---------------------------------------------------------------------*
*  Preenchimento Dados Cabeçalho BAPI
*---------------------------------------------------------------------*
    lva_code              = '06'.
    lwa_header-pstng_date = sy-datum.
    lwa_header-doc_date   = sy-datum.


*---------------------------------------------------------------------*
* Preenchimento Dados Cabeçalho BAPI
*---------------------------------------------------------------------*

    CLEAR: lwa_item.
    lwa_item-move_type    = 'ZA2'.

    lwa_item-material     = i_matnr.
    lwa_item-plant        = i_filial_algodoeira.
    lwa_item-stge_loc     = i_bloco.
    lwa_item-batch        = _charg_proc.

    lwa_item-move_mat     = i_matnr.
    lwa_item-move_plant   = i_filial_algodoeira.
    lwa_item-move_stloc   = lva_deposito_destino.
    lwa_item-move_batch   = i_safra.

    lwa_item-entry_qnt    = lva_qtde_estorno.

    APPEND lwa_item TO lit_item.

    DO 5 TIMES.

      DATA(_index) = sy-index.

      CLEAR: lva_material, lva_year, lit_return[], e_error_lock.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = lwa_header
          goodsmvt_code    = lva_code
        IMPORTING
          materialdocument = lva_material
          matdocumentyear  = lva_year
        TABLES
          goodsmvt_item    = lit_item
          return           = lit_return.

      IF lva_material IS NOT INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        e_mblnr = lva_material.
        e_mjahr = lva_year.
        e_msg_sucesso = |Documento : { lva_material }  Ano: { lva_year } gerado com Sucesso!|.

        LOOP AT lit_zmmt0008_estorno INTO DATA(lwa_estorno_zmmt0008).

          CLEAR: lwa_zmmt0008_delete.

          "Insere tabela historico
          MOVE-CORRESPONDING lwa_estorno_zmmt0008 TO lwa_zmmt0008_delete.
          MOVE sy-uname                 TO lwa_zmmt0008_delete-user_delete.
          MOVE sy-datum                 TO lwa_zmmt0008_delete-data_delete.
          MOVE sy-uzeit                 TO lwa_zmmt0008_delete-hora_delete.
          MOVE lva_material             TO lwa_zmmt0008_delete-mblnr_estorno.
          MOVE lva_year                 TO lwa_zmmt0008_delete-mjahr_estorno.

          MODIFY zmmt0008_delete      FROM lwa_zmmt0008_delete.

          "Elimina registro atual
          DELETE FROM zmmt0008 WHERE werks    = lwa_estorno_zmmt0008-werks
                                 AND lgort    = lwa_estorno_zmmt0008-lgort
                                 AND charg    = lwa_estorno_zmmt0008-charg
                                 AND safra    = lwa_estorno_zmmt0008-safra.

          COMMIT WORK AND WAIT.

          zcl_comercializacao_algodao=>notificar_carregamento_trace(
             i_zmmt0008_delete       = lwa_zmmt0008_delete
             i_carregamento_auto     = i_carregamento_auto
          ).

        ENDLOOP.

        RETURN.

      ELSE.

        READ TABLE  lit_return INTO lwa_return WITH KEY type = 'E'.
        IF sy-subrc EQ 0.

          CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
            EXPORTING
              id       = lwa_return-id
              number   = lwa_return-number
            IMPORTING
              is_block = e_error_lock.


          IF e_error_lock IS NOT INITIAL AND _index LE 5.
            WAIT UP TO 3 SECONDS.
          ELSE.
            e_msg_error = lwa_return-message.
            RETURN.
          ENDIF.

        ELSE.
          e_msg_error = 'Houve um erro desconhecido ao gerar transferencia!'.
          RETURN.
        ENDIF.

      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD get_fardos_espelho_romaneio.

    DATA: lva_nr_romaneio TYPE znr_romaneio,
          lva_safra       TYPE znr_safra,
          lva_lgort       TYPE lgort_d,
          lva_werks       TYPE werks_d.

    CLEAR: r_zmmt0008_t[].

    lva_werks       = i_werks.
    lva_lgort       = i_lgort.
    lva_safra       = i_safra.
    lva_nr_romaneio = i_nr_romaneio.

    SELECT a~*
      FROM zmmt0008 AS a INNER JOIN vbap AS b ON a~vbeln = b~vbeln INTO CORRESPONDING FIELDS OF TABLE @r_zmmt0008_t
     WHERE a~werks       EQ @lva_werks
       AND a~lgort       EQ @lva_lgort
       AND a~safra       EQ @lva_safra
       AND a~nr_romaneio EQ @lva_nr_romaneio
       AND b~charg       EQ @lva_safra.


  ENDMETHOD.


  METHOD imprimir_espelho_romaneio.

    TYPES: BEGIN OF ty_mara,
             matnr TYPE mara-matnr,
             normt TYPE mara-normt,
           END OF ty_mara,

           BEGIN OF ty_vbrp,
             vbeln  TYPE vbrp-vbeln,
             vgbel  TYPE vbrp-vgbel,
             matnr  TYPE vbrp-matnr,
             refkey TYPE j_1bnflin-refkey,
           END OF ty_vbrp,

           BEGIN OF ty_lin,
             docnum TYPE j_1bnflin-docnum,
             refkey TYPE j_1bnflin-refkey,
           END OF ty_lin.

    DATA: lit_mara TYPE TABLE OF ty_mara,
          lit_vbrp TYPE TABLE OF ty_vbrp.

    DATA: lwa_header         TYPE zmms005.
    DATA: lwa_zsdt0001      TYPE zsdt0001.
    DATA: lit_zmmt0008 TYPE zmmt0008_t,
          lwa_fardo    TYPE zmms004,
          lit_fardos   TYPE TABLE OF zmms004.


    DATA: lva_form    TYPE tdsfname,
          lva_name    TYPE rs38l_fnam,
          lva_safra   TYPE char4,
          sl_rodape   TYPE zmmr0001_rod,
          lit_fardos1 TYPE TABLE OF zmms004,
          lit_fardos2 TYPE TABLE OF zmms004,
          lit_fardos3 TYPE TABLE OF zmms004,
          lit_fardos4 TYPE TABLE OF zmms004,
          wl_cont     TYPE sy-tabix,
          wl_menge    TYPE zmmt0008-menge,
          lit_total   TYPE TABLE OF zmms006,
          lwa_total   TYPE zmms006,
          wl_tipo     TYPE mara-normt,
          wl_flag.
*

    CLEAR: r_msg_error.


    IF i_nr_romaneio IS INITIAL.
      r_msg_error = 'É obrigatorio o preenchimento do campo Número do Romaneio'.
      RETURN.
    ENDIF.

    IF i_werks IS INITIAL.
      r_msg_error = 'É obrigatorio o preenchimento do campo Centro'.
      RETURN.
    ENDIF.

    IF i_lgort IS INITIAL.
      r_msg_error = 'É obrigatorio o preenchimento do campo Depósito'.
      RETURN.
    ENDIF.

    IF i_safra IS INITIAL.
      r_msg_error = 'É obrigatorio o preenchimento do campo Safra'.
      RETURN.
    ENDIF.

*----------------------------------------------------------------------------------------------*
*   Seleção Dados
*----------------------------------------------------------------------------------------------*

    lit_zmmt0008 = get_fardos_espelho_romaneio( i_werks       = i_werks
                                                i_lgort       = i_lgort
                                                i_safra       = i_safra
                                                i_nr_romaneio = i_nr_romaneio ).

    IF lit_zmmt0008[] IS INITIAL .
      r_msg_error = 'Dados não encontrados.'.
      RETURN.
    ENDIF.

    READ TABLE lit_zmmt0008 INTO DATA(lwa_zmmt0008_header) INDEX 1.

    IF lwa_zmmt0008_header-status = '3'.
      r_msg_error = |O Romaneio está em processo de estorno no sistema Rondonline, impressão já não é mais possível!|.
      RETURN.
    ENDIF.

    lwa_header-veiculo   = lwa_zmmt0008_header-placa_cav.
    lwa_header-romaneio  = lwa_zmmt0008_header-nr_romaneio.
    lwa_header-motorista = lwa_zmmt0008_header-motorista.
    lwa_header-bloco     = lwa_zmmt0008_header-lgort.

    SELECT SINGLE name1
      FROM kna1 INTO lwa_header-cliente
     WHERE kunnr EQ lwa_zmmt0008_header-kunnr.

    SELECT SINGLE *
      FROM zsdt0001 INTO lwa_zsdt0001
      WHERE vbeln        EQ lwa_zmmt0008_header-vbeln
        AND tp_movimento EQ 'S'
        AND nr_romaneio  EQ lwa_zmmt0008_header-nr_romaneio
        AND branch       EQ lwa_zmmt0008_header-werks
        AND vbeln        EQ lwa_zmmt0008_header-vbeln.

    IF sy-subrc NE 0.
      r_msg_error = |O Romaneio não encontrado { lwa_zmmt0008_header-nr_romaneio }!|.
      RETURN.
    ENDIF.

    lwa_header-safra = lwa_zsdt0001-nr_safra.

    "Destinatario
    SELECT SINGLE name1
      FROM kna1 INTO lwa_header-destinatario
      WHERE kunnr EQ lwa_zsdt0001-id_cli_dest.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(lwa_j1bnflin)
       WHERE refkey EQ @lwa_zmmt0008_header-vbeln_vf.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE *
        FROM j_1bnfe_active INTO @DATA(lwa_j1bnfe_active)
         WHERE docnum EQ @lwa_j1bnflin-docnum.

      IF sy-subrc IS INITIAL.
        lwa_header-nfnum = lwa_j1bnfe_active-nfnum9. "Nota Fiscal

        SELECT SINGLE *
          FROM j_1bbranch INTO @DATA(lwa_j_1bbranch)
           WHERE bukrs  EQ @lwa_j1bnfe_active-bukrs
             AND branch EQ @lwa_j1bnfe_active-branch.

        "Remetente
        lwa_header-remetente = lwa_j_1bbranch-name.
        SELECT SINGLE butxt
          FROM t001 INTO lwa_header-empresa
           WHERE bukrs EQ lwa_j1bnfe_active-bukrs.

      ENDIF.
    ENDIF.


    SELECT vbeln vgbel matnr
      FROM vbrp INTO TABLE lit_vbrp
      FOR ALL ENTRIES IN lit_zmmt0008
       WHERE vbeln EQ lit_zmmt0008-vbeln_vf.

    IF lit_vbrp[] IS NOT INITIAL.
      SELECT matnr normt
        FROM mara INTO TABLE lit_mara
        FOR ALL ENTRIES IN lit_vbrp
       WHERE matnr EQ lit_vbrp-matnr.

      IF sy-subrc NE 0.
        r_msg_error = |Cadastro material não encontrado para o romaneio { lwa_zmmt0008_header-nr_romaneio }!|.
        RETURN.
      ENDIF.

      READ TABLE lit_mara INTO DATA(lwa_mara) INDEX 1.

    ENDIF.

    LOOP AT lit_zmmt0008 INTO DATA(lwa_zmmt0008).

      CLEAR: lwa_fardo.

      CONDENSE lwa_zmmt0008-charg NO-GAPS.

      IF lwa_zmmt0008-cd_sai IS INITIAL.
        IF lwa_zmmt0008-charg+02(1) = '/'. "Projeto Reestruturação Algodao 2024
          SELECT SINGLE cd_sai
            FROM zppt0002 INTO lwa_fardo-fardo
           WHERE acharg = lwa_zmmt0008-charg
             AND werks  = lwa_zmmt0008-werks.
        ENDIF.
      ELSE.
        lwa_fardo-fardo = lwa_zmmt0008-cd_sai. "Projeto Reestruturação Algodao 2024
      ENDIF.

      lwa_fardo-tipo  = lwa_mara-normt(5).
      lwa_fardo-peso  = lwa_zmmt0008-menge.

      APPEND lwa_fardo TO lit_fardos.
    ENDLOOP.

*----------------------------------------------------------------------------------------------*
*   Imprimir Smartform
*----------------------------------------------------------------------------------------------*

    lva_form = 'ZMMS002'.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = lva_form
      IMPORTING
        fm_name            = lva_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SORT: lit_fardos BY tipo.


    LOOP AT lit_fardos INTO lwa_fardo.
      MOVE:                    1 TO lwa_total-total,
                  lwa_fardo-tipo TO lwa_total-tipo,
                  lwa_fardo-peso TO lwa_total-peso.
      COLLECT lwa_total INTO lit_total.
    ENDLOOP.


    CALL FUNCTION lva_name
      EXPORTING
        header           = lwa_header
      TABLES
        tg_fardos        = lit_fardos
        tg_fardos1       = lit_fardos1
        tg_fardos2       = lit_fardos2
        tg_fardos3       = lit_fardos3
        tg_fardos4       = lit_fardos4
        tg_total         = lit_total
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO r_msg_error.
      RETURN.
    ENDIF.



  ENDMETHOD.


  METHOD notificar_carregamento_trace.

    DATA: lwa_zsdt0340 TYPE zsdt0340.

    CHECK ( i_zmmt0008 IS NOT INITIAL  ) OR ( i_zmmt0008_delete IS NOT INITIAL ).

    IF i_zmmt0008 IS NOT INITIAL.

      CLEAR: lwa_zsdt0340.

      GET TIME STAMP FIELD lwa_zsdt0340-timestamp.

      lwa_zsdt0340-werks                                = i_zmmt0008-werks.
      lwa_zsdt0340-lgort                                = i_zmmt0008-lgort.
      lwa_zsdt0340-charg                                = i_zmmt0008-charg.
      lwa_zsdt0340-safra                                = i_zmmt0008-safra.
      lwa_zsdt0340-cd_sai                               = i_zmmt0008-cd_sai.
      lwa_zsdt0340-disponibilizado_comerc               = abap_true.
      lwa_zsdt0340-carregamento_auto                    = i_carregamento_auto.
      lwa_zsdt0340-sincronizado                         = abap_false.
      lwa_zsdt0340-protocolo_recebimento                = space.
      lwa_zsdt0340-dt_registro                          = sy-datum.
      lwa_zsdt0340-hr_registro                          = sy-uzeit.
      lwa_zsdt0340-us_registro                          = sy-uname.

      MODIFY zsdt0340 FROM lwa_zsdt0340.
      COMMIT WORK.
    ENDIF.

    IF i_zmmt0008_delete IS NOT INITIAL.

      CLEAR: lwa_zsdt0340.

      GET TIME STAMP FIELD lwa_zsdt0340-timestamp.

      lwa_zsdt0340-werks                                = i_zmmt0008_delete-werks.
      lwa_zsdt0340-lgort                                = i_zmmt0008_delete-lgort.
      lwa_zsdt0340-charg                                = i_zmmt0008_delete-charg.
      lwa_zsdt0340-safra                                = i_zmmt0008_delete-safra.
      lwa_zsdt0340-cd_sai                               = i_zmmt0008_delete-cd_sai.
      lwa_zsdt0340-disponibilizado_comerc               = abap_false.
      lwa_zsdt0340-carregamento_auto                    = i_carregamento_auto.
      lwa_zsdt0340-sincronizado                         = abap_false.
      lwa_zsdt0340-protocolo_recebimento                = space.
      lwa_zsdt0340-dt_registro                          = sy-datum.
      lwa_zsdt0340-hr_registro                          = sy-uzeit.
      lwa_zsdt0340-us_registro                          = sy-uname.

      MODIFY zsdt0340 FROM lwa_zsdt0340.
      COMMIT WORK.
    ENDIF.



  ENDMETHOD.


  METHOD processar_disp_fardos.

    DATA: lva_enqmode TYPE enqmode.

    CLEAR: e_msg_error, e_fardos_bloco_trace_cotton, e_charg_proc, e_qtde_disp, e_fardos_disponibilizar[], e_error_lock.

    IF i_safra IS INITIAL.
      e_msg_error = |Safra não foi informada! |.
      RETURN.
    ENDIF.

    IF i_filial_algodoeira IS INITIAL.
      e_msg_error = |Filial Algodoeira não foi informada! |.
      RETURN.
    ENDIF.

    IF i_bloco IS INITIAL.
      e_msg_error = |Bloco não foi informado! |.
      RETURN.
    ENDIF.

    IF i_matnr IS INITIAL.
      e_msg_error = |Material não foi informado! |.
      RETURN.
    ENDIF.

    IF i_lgort_destino IS INITIAL AND i_estorno EQ abap_false.
      e_msg_error = |Deposito Destino não foi informado! |.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM mara INTO @DATA(lwa_mara_check)
     WHERE matnr EQ @i_matnr.

    IF sy-subrc NE 0.
      e_msg_error = |Cadastro Material: { i_matnr } não encontrado! |.
      RETURN.
    ENDIF.

    IF lwa_mara_check-mtart NE 'ZFER'.
      e_msg_error = |Material { i_matnr } não é do tipo ZFER! |.
      RETURN.
    ENDIF.

    IF lwa_mara_check-normt IS INITIAL.
      e_msg_error = |Denominação/Classificação Material { i_matnr } não informada no cadastro! |.
      RETURN.
    ENDIF.

    e_charg_proc = i_safra && '_' && i_filial_algodoeira.

    IF i_estorno EQ abap_false.
      SELECT SINGLE *
        FROM mchb INTO @DATA(lwa_mchb)
      WHERE  matnr EQ @i_matnr
        AND  werks EQ @i_filial_algodoeira
        AND  lgort EQ @i_bloco
        AND  charg EQ @e_charg_proc.

      IF sy-subrc NE 0 OR lwa_mchb-clabs LE 0.
        e_msg_error = |Material: { i_matnr } Centro: { i_filial_algodoeira } Deposito: { i_bloco } |.
        e_msg_error = |{ e_msg_error } Lote: { e_charg_proc } sem saldo para consumo!|.
        RETURN.
      ENDIF.
    ENDIF.

    zcl_trace_cotton_utils=>get_fardos_bloco_trace_cotton(
      EXPORTING
        i_safra                     = CONV #( i_safra )
        i_filial_algodoeira         = CONV #( i_filial_algodoeira )
        i_bloco                     = CONV #( i_bloco )
        i_matnr                     = i_matnr
        i_check_embarque_sap        = abap_true
        i_return_dados_acts         = abap_true
      IMPORTING
        e_msg_error                 = e_msg_error
        e_fardos_bloco_trace_cotton = e_fardos_bloco_trace_cotton
    ).

    CHECK e_msg_error IS INITIAL.

    DELETE e_fardos_bloco_trace_cotton WHERE status_takeup_lote_recente IS INITIAL. "Desconsiderar fardinhos sem takeup

*--------------------------------------------------------------------------------------------------------------------------*
*   Validar fardos solicitados para disponibilização
*--------------------------------------------------------------------------------------------------------------------------*
    IF sy-batch EQ abap_true.
      lva_enqmode  = 'X'.
    ELSE.
      lva_enqmode  = 'E'.
    ENDIF.

    LOOP AT i_fardos_disponibilizar INTO DATA(lwa_fardo_disp).

      CALL FUNCTION 'ENQUEUE_EZMMT0008_V2'
        EXPORTING
          mode_zmmt0008  = lva_enqmode
          werks          = i_filial_algodoeira
          lgort          = i_bloco
          charg          = lwa_fardo_disp-nr_fardo_completo
          safra          = i_safra
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        e_error_lock = abap_true.
        e_msg_error  = |Fardo { lwa_fardo_disp-nr_fardo_completo } está em processamento!|.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM zmmt0008 INTO @DATA(lwa_zmmt0008_exists)
       WHERE werks  eq @i_filial_algodoeira
         AND lgort  eq @i_bloco
         AND charg  eq @lwa_fardo_disp-nr_fardo_completo
         AND safra  eq @i_safra.

      IF sy-subrc eq 0.
        e_msg_error  = |Fardo { lwa_fardo_disp-nr_fardo_completo } já foi disponibilizado para comercialização!|.
        RETURN.
      ENDIF.

      READ TABLE e_fardos_bloco_trace_cotton INTO DATA(lwa_fardo_trace_cotton) WITH KEY nr_fardo_completo = lwa_fardo_disp-nr_fardo_completo.
      IF sy-subrc NE 0.
        e_msg_error = |Fardinho { lwa_fardo_disp-nr_fardo_completo } não consta no bloco: { i_bloco } ou esta sem Takeup no Trace Cotton! |.
        RETURN.
      ENDIF.

      IF lwa_fardo_trace_cotton-embarcado_sap EQ abap_true.
        e_msg_error = |Fardinho { lwa_fardo_disp-nr_fardo_completo } já foi embarcado no SAP! |.
        RETURN.
      ENDIF.

      IF lwa_fardo_trace_cotton-validacao_acts-icon = icon_led_red.
        e_msg_error =  'Existem lotes com status diferente de aprovado ou não possui ACTS, verificar!'.
        RETURN.
      ENDIF.

      IF i_carregamento_auto EQ abap_false. "Se não for pelo processo de carregamento automatico

        "Verificar se fardo esta vinculado ao processo de carregamento automatico
        SELECT SINGLE *
          FROM zsdt0330 INTO @DATA(lwa_zsdt0330)
         WHERE matnr     = @i_matnr
           AND werks     = @i_filial_algodoeira
           AND lgort     = @i_bloco
           AND acharg    = @lwa_fardo_trace_cotton-nr_fardo_completo
           AND safra     = @i_safra
           AND cancelado = @abap_false.

        IF sy-subrc EQ 0.
          e_msg_error =  'Há fardos escolhidos que estão em Processamento Automático!'.
          RETURN.
        ENDIF.

      ENDIF.

      ADD lwa_fardo_trace_cotton-peso_liquido TO e_qtde_disp.

      APPEND lwa_fardo_trace_cotton TO e_fardos_disponibilizar.

    ENDLOOP.

    IF i_fardos_disponibilizar[] IS NOT INITIAL AND e_qtde_disp IS INITIAL.
      e_msg_error = 'Quantidade para transferencia não foi identificada!'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD remove_fardos_replicados.

    DATA: l_mesg1 TYPE char200,
          l_mesg2 TYPE char200,
          l_mesg3 TYPE char200.

    CLEAR: r_utilizado.

    READ TABLE i_zmmt0008_t INTO DATA(lwa_zmmt0008_tmp) INDEX 1.
    CHECK sy-subrc EQ 0 AND lwa_zmmt0008_tmp-kunnr IS NOT INITIAL.

    CHECK i_zmmt0008_t[] IS NOT INITIAL.

    SELECT ch_referencia, id_cli_dest
      FROM zsdt0001
      INTO TABLE @DATA(lit_zsdt0001)
     WHERE nr_romaneio  = @i_nr_romaneio
       AND nr_safra     = @i_nr_safra
       AND branch       = @i_branch
       AND tp_movimento = 'S'.

    CHECK lit_zsdt0001[] IS NOT INITIAL.

    SELECT kunnr, ktokd
      FROM kna1 INTO TABLE @DATA(lit_kna1)
       FOR ALL ENTRIES IN @i_zmmt0008_t
     WHERE kunnr   = @i_zmmt0008_t-kunnr.

    READ TABLE lit_kna1 INTO DATA(lwa_kna1) WITH KEY ktokd = 'ZCIC'.
    CHECK sy-subrc = 0.

    READ TABLE lit_zsdt0001 INTO DATA(w_0001) INDEX 1.
    CHECK sy-subrc = 0.

    CHECK lwa_kna1-kunnr+6(4) NE i_branch.

    SELECT *
      FROM zmmt0008 INTO TABLE @DATA(lit_zmmt0008)
       FOR ALL ENTRIES IN @i_zmmt0008_t
     WHERE werks  = @lwa_kna1-kunnr+6(4)
       AND lgort  = @i_zmmt0008_t-lgort
       AND charg  = @i_zmmt0008_t-charg
       AND safra  = @i_zmmt0008_t-safra
       AND menge  = @i_zmmt0008_t-menge.

    DATA(lit_zmmt0008_faturado) = lit_zmmt0008[].

    DELETE lit_zmmt0008_faturado WHERE nr_romaneio IS INITIAL.

    IF lit_zmmt0008_faturado[] IS NOT INITIAL.

      r_utilizado = abap_true.

      READ TABLE lit_zmmt0008_faturado INTO DATA(lwa_zmmt0008) INDEX 1.

      l_mesg1 = |Os dados replicados do lote { lwa_zmmt0008-charg } para o centro { lwa_zmmt0008-werks }|.
      l_mesg2 = 'já foram utilidos nos romaneios, a serem exibidos.'.
      l_mesg3 = 'Solicite o Cancel.dos Romaneios para cancelar replicação de dados'.

      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
        EXPORTING
          titel        = 'Cancelamento Romaneio'
          textline1    = l_mesg1
          textline2    = l_mesg2
          textline3    = l_mesg3
          start_column = 25
          start_row    = 6.

      CALL FUNCTION 'ZSD_LOG_CANCEL_ROMANEIO'
        TABLES
          t_dados = lit_zmmt0008_faturado.

      RETURN.

    ENDIF.

    LOOP AT lit_zmmt0008 INTO lwa_zmmt0008.
      DELETE FROM zmmt0008 WHERE werks = lwa_zmmt0008-werks
                             AND lgort = lwa_zmmt0008-lgort
                             AND charg = lwa_zmmt0008-charg
                             AND safra = lwa_zmmt0008-safra.
    ENDLOOP.

    COMMIT WORK AND WAIT.



  ENDMETHOD.


  METHOD processar_espelho_romaneio.

    TYPES: BEGIN OF ty_j_1bnflin,
             docnum TYPE  j_1bnflin-docnum,
             refkey TYPE  j_1bnflin-refkey,
           END OF ty_j_1bnflin,

           BEGIN OF ty_vbrp,
             vbeln  TYPE vbrp-vbeln,
             vgbel  TYPE vbrp-vgbel,
             matnr  TYPE vbrp-matnr,
             refkey TYPE j_1bnflin-refkey,
           END OF ty_vbrp.

    DATA: lit_zsdt0001 TYPE TABLE OF zsdt0001.
    DATA: lit_vbrp TYPE TABLE OF ty_vbrp.
    DATA: lit_j_1bnflin TYPE TABLE OF j_1bnflin.
    DATA: lit_j_1bnfe_active TYPE TABLE OF j_1bnfe_active .
    DATA: lit_kna1 TYPE TABLE OF kna1.
    DATA: lit_zmmt0008 TYPE TABLE OF zmmt0008.
    DATA: lit_zmmt0008_aux TYPE TABLE OF zmmt0008.

    DATA: lwa_zsds091 TYPE zsds091.

    CLEAR: e_msg_error.


    DATA: lva_nr_romaneio TYPE znr_romaneio,
          lva_safra       TYPE znr_safra,
          lva_lgort       TYPE lgort_d,
          lva_werks       TYPE werks_d,
          lva_motorista   TYPE char50,
          lva_placa       TYPE zplaca,
          lva_kunnr       TYPE kunnr.

    DATA: lra_nr_romaneio TYPE RANGE OF zmmt0008-nr_romaneio.

    IF i_nr_romaneio IS INITIAL.
      e_msg_error = 'É obrigatorio o preenchimento do campo Número do Romaneio'.
      RETURN.
    ENDIF.

    IF i_werks IS INITIAL.
      e_msg_error = 'É obrigatorio o preenchimento do campo Centro'.
      RETURN.
    ENDIF.

    IF i_lgort IS INITIAL.
      e_msg_error = 'É obrigatorio o preenchimento do campo Depósito'.
      RETURN.
    ENDIF.

    IF i_safra IS INITIAL.
      e_msg_error = 'É obrigatorio o preenchimento do campo Safra'.
      RETURN.
    ENDIF.

*    IF i_motorista IS INITIAL.
*      e_msg_error = 'É obrigatorio o preenchimento do campo Motorista'.
*      RETURN.
*    ENDIF.

    IF i_placa IS INITIAL.
      e_msg_error = 'É obrigatorio o preenchimento do campo Placa do Veículo'.
      RETURN.
    ENDIF.

    IF i_kunnr IS INITIAL.
      e_msg_error = 'É obrigatorio o preenchimento do campo Cliente'.
      RETURN.
    ENDIF.

    lva_werks       = i_werks.
    lva_lgort       = i_lgort.
    lva_safra       = i_safra.
    lva_nr_romaneio = i_nr_romaneio.
    lva_motorista   = i_motorista.
    lva_placa       = i_placa.
    lva_kunnr       = i_kunnr.

*--------------------------------------------------------------------------------------------------------------------*
*   Seleção Inicial Dados
*--------------------------------------------------------------------------------------------------------------------*

    SELECT nr_romaneio vbeln branch doc_rem id_cli_dest
      FROM zsdt0001 INTO CORRESPONDING FIELDS OF TABLE lit_zsdt0001
       WHERE tp_movimento EQ 'S'
         AND nr_romaneio  EQ lva_nr_romaneio
         AND branch       EQ lva_werks
         AND nr_safra     EQ lva_safra.

    IF lit_zsdt0001[] IS INITIAL.
      e_msg_error = 'Romaneio informado não foi encontrado!'.
      RETURN.
    ENDIF.

    SELECT vbeln vgbel matnr
      FROM vbrp INTO CORRESPONDING FIELDS OF TABLE lit_vbrp
      FOR ALL ENTRIES IN lit_zsdt0001
       WHERE vgbel EQ lit_zsdt0001-doc_rem.

    IF lit_vbrp[] IS INITIAL.
      e_msg_error = 'Fatura Romaneio não foi encontrada!'.
      RETURN.
    ENDIF.

    LOOP AT lit_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp>).
      <fs_vbrp>-refkey = <fs_vbrp>-vbeln.
    ENDLOOP.

    SELECT docnum refkey
      FROM j_1bnflin INTO CORRESPONDING FIELDS OF TABLE lit_j_1bnflin
       FOR ALL ENTRIES IN lit_vbrp
       WHERE refkey EQ lit_vbrp-refkey.

    IF sy-subrc IS INITIAL.
      SELECT docnum nfnum9 bukrs branch
        FROM j_1bnfe_active INTO CORRESPONDING FIELDS OF TABLE lit_j_1bnfe_active
          FOR ALL ENTRIES IN lit_j_1bnflin
        WHERE docnum EQ lit_j_1bnflin-docnum
           AND nfnum9 <> ' '.
      IF lit_j_1bnfe_active[] IS INITIAL.
        e_msg_error = 'Para Gerar o lote de Venda a NF-e do Romaneio informado deve estar Determinada'.
        RETURN.
      ENDIF.
    ENDIF.

    CASE abap_true.
      WHEN i_envia_email OR i_integra_rondoline.

        SELECT *
          FROM zmmt0008 INTO CORRESPONDING FIELDS OF TABLE lit_zmmt0008
         WHERE werks 	   EQ lva_werks
           AND lgort       EQ lva_lgort
           AND safra       EQ lva_safra	
           AND nr_romaneio EQ lva_nr_romaneio.

      WHEN OTHERS.

        APPEND VALUE #( sign = 'I' option = 'EQ' low = 0 ) TO lra_nr_romaneio.

        IF i_carregamento_auto = abap_true.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = lva_nr_romaneio ) TO lra_nr_romaneio.
        ENDIF.

        SELECT *
          FROM zmmt0008 INTO CORRESPONDING FIELDS OF TABLE lit_zmmt0008
         WHERE werks       EQ lva_werks
           AND lgort       EQ lva_lgort
           AND safra       EQ lva_safra
           AND nr_romaneio IN lra_nr_romaneio.
    ENDCASE.

    IF i_fardos_espelho[] IS NOT INITIAL.
      LOOP AT lit_zmmt0008 ASSIGNING FIELD-SYMBOL(<fs_zmmt0008>).
        READ TABLE i_fardos_espelho WITH KEY werks = <fs_zmmt0008>-werks
                                             bloco = <fs_zmmt0008>-lgort
                                             safra = <fs_zmmt0008>-safra
                                             fardo = <fs_zmmt0008>-charg TRANSPORTING NO FIELDS.
        CHECK sy-subrc NE 0.

        CLEAR <fs_zmmt0008>-charg.

      ENDLOOP.

      DELETE lit_zmmt0008 WHERE charg IS INITIAL.
    ENDIF.

    IF lit_zmmt0008[] IS INITIAL.

      SELECT *
        FROM zmmt0008 INTO TABLE lit_zmmt0008_aux
        WHERE werks      EQ lva_werks
         AND lgort       EQ lva_lgort
         AND safra       EQ lva_safra
         AND nr_romaneio EQ lva_nr_romaneio
         AND placa_cav   EQ lva_placa
         AND kunnr       EQ lva_kunnr
         AND status      EQ '3'.

      DATA(lva_count) = lines( lit_zmmt0008_aux ).
      IF lva_count > 0.
        e_msg_error = 'Romaneio está em processo de cancelamento e estorno no sistema Rondonline, por favor aguardar!'.
        RETURN.
      ENDIF.
    ENDIF.

    IF lit_zmmt0008[] IS INITIAL.
      e_msg_error = 'Nenhum fardo disponivel para gerar espelho do romaneio!'.
      RETURN.
    ENDIF.

    SELECT kunnr name1
      FROM kna1 INTO CORRESPONDING FIELDS OF TABLE lit_kna1
       FOR ALL ENTRIES IN lit_zsdt0001
       WHERE kunnr EQ lit_zsdt0001-id_cli_dest.

    READ TABLE lit_kna1 INTO DATA(lwa_kna1) INDEX 1.


*--------------------------------------------------------------------------------------------------------------------*
*   Montagem Fardos para processar
*--------------------------------------------------------------------------------------------------------------------*

    LOOP AT lit_vbrp INTO DATA(lwa_vbrp).

      READ TABLE lit_j_1bnflin INTO DATA(lwa_j_1bnflin) WITH KEY refkey = lwa_vbrp-refkey.

      CHECK sy-subrc EQ 0.

      READ TABLE lit_j_1bnfe_active INTO DATA(lwa_j_1bnfe_active) WITH KEY docnum = lwa_j_1bnflin-docnum.

      CHECK sy-subrc EQ 0.

      READ TABLE lit_zsdt0001 INTO DATA(lwa_zsdt0001) WITH KEY doc_rem = lwa_vbrp-vgbel.
      CHECK sy-subrc EQ 0.

      LOOP AT lit_zmmt0008  INTO DATA(lwa_zmmt0008).

        APPEND INITIAL LINE TO e_fardos_processar ASSIGNING FIELD-SYMBOL(<fs_fardo_processar>).

        <fs_fardo_processar>-werks                =  lwa_zmmt0008-werks.
        <fs_fardo_processar>-bloco                =  lwa_zmmt0008-lgort.
        <fs_fardo_processar>-fardo                =  lwa_zmmt0008-charg.
        <fs_fardo_processar>-nr_romaneio          =  lwa_zsdt0001-nr_romaneio.
        <fs_fardo_processar>-nfnum                =  lwa_j_1bnfe_active-nfnum9.
        <fs_fardo_processar>-vbeln                =  lwa_zsdt0001-vbeln.
        <fs_fardo_processar>-vbeln_vf             =  lwa_vbrp-vbeln.
        <fs_fardo_processar>-matnr                =  lwa_vbrp-matnr.
        <fs_fardo_processar>-placa_cav            =  lva_placa.
        <fs_fardo_processar>-menge                =  lwa_zmmt0008-menge.
        <fs_fardo_processar>-werks_orig           =  lwa_zmmt0008-werks_orig.
        <fs_fardo_processar>-lgortr               =  lwa_zmmt0008-lgortr.
        <fs_fardo_processar>-safra                =  lwa_zmmt0008-safra.
        <fs_fardo_processar>-mblnr                =  lwa_zmmt0008-mblnr.
        <fs_fardo_processar>-mjahr                =  lwa_zmmt0008-mjahr.
        <fs_fardo_processar>-dados_contingencia   =  lwa_zmmt0008-dados_contingencia.
        <fs_fardo_processar>-tipo_fardo           =  lwa_zmmt0008-tipo_fardo.
        <fs_fardo_processar>-cd_sai               =  lwa_zmmt0008-cd_sai.
        <fs_fardo_processar>-kunnr_destino        =  lwa_kna1-kunnr.
        <fs_fardo_processar>-name1_destino        =  lwa_kna1-name1.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD gerar_espelho_romaneio.

    DATA: lva_nr_romaneio TYPE znr_romaneio,
          lva_safra       TYPE znr_safra,
          lva_lgort       TYPE lgort_d,
          lva_werks       TYPE werks_d,
          lva_motorista   TYPE char50,
          lva_placa       TYPE zplaca,
          lva_kunnr       TYPE kunnr.

    TYPES: ty_selection TYPE STANDARD TABLE OF rsparams.

    DATA: lwa_selection TYPE rsparams,
          lit_selection TYPE ty_selection.

    DATA: lva_resposta TYPE c.

    DATA: lit_zmmt0008_gravar  TYPE TABLE OF zmmt0008.

    CLEAR: e_msg_error, e_msg_sucesso.

    IF i_fardos_espelho[] IS INITIAL.
      e_msg_error = 'Fardos para gerar os espelho do romaneio não foram informados!'.
      RETURN.
    ENDIF.

    lva_werks       = i_werks.
    lva_lgort       = i_lgort.
    lva_safra       = i_safra.
    lva_nr_romaneio = i_nr_romaneio.
    lva_motorista   = i_motorista.
    lva_placa       = i_placa.
    lva_kunnr       = i_kunnr.

    processar_espelho_romaneio(
      EXPORTING
        i_werks             = CONV #( lva_werks )
        i_lgort             = CONV #( lva_lgort )
        i_safra             = CONV #( lva_safra )
        i_nr_romaneio       = CONV #( lva_nr_romaneio )
        i_motorista         = CONV #( lva_motorista )
        i_placa             = CONV #( lva_placa )
        i_kunnr             = CONV #( lva_kunnr )
        i_fardos_espelho    = CONV #( i_fardos_espelho )
        i_proc_unico        = CONV #( i_proc_unico )
        i_reprocessar       = CONV #( i_reprocessar )
        i_envia_email       = CONV #( i_envia_email )
        i_integra_rondoline = CONV #( i_integra_rondoline )
        i_carregamento_auto = CONV #( i_carregamento_auto )
      IMPORTING
        e_msg_error         = e_msg_error
        e_fardos_processar  = DATA(lit_fardos_processar)
    ).

    CHECK e_msg_error IS INITIAL.

    LOOP AT lit_fardos_processar ASSIGNING FIELD-SYMBOL(<fs_fardo_processar>).
      <fs_fardo_processar>-mark = abap_true.
    ENDLOOP.

    READ TABLE lit_fardos_processar INTO DATA(lwa_fardo_processar) WITH KEY mark  = abap_true
                                                                            nfnum = 0.
    IF sy-subrc IS INITIAL.
      e_msg_error = 'Não localizado número da Nota para o romaneio informado,' &&
                    'verifique se a mesma esta determinada!'.
      RETURN.
    ENDIF.

    SELECT SINGLE kunnr, ktokd
      FROM kna1 INTO @DATA(lwa_kna1)
     WHERE kunnr = @lva_kunnr.

    IF sy-subrc NE 0.
      e_msg_error = |Cadastro cliente { lva_kunnr } não encontrado!|.
      RETURN.
    ENDIF.

    DATA(_replica_fardos_intercompany) = abap_false.
    IF lwa_kna1-ktokd = 'ZCIC' AND lwa_kna1-kunnr+6(4) NE lva_werks.
      _replica_fardos_intercompany = abap_true.

      SELECT *
        FROM zmmt0008 INTO TABLE @DATA(lit_zmmt0008_replicar)
         FOR ALL ENTRIES IN @lit_fardos_processar
       WHERE werks = @lwa_kna1-kunnr+6(4)
         AND lgort = @lit_fardos_processar-bloco
         AND charg = @lit_fardos_processar-fardo
         AND safra = @lit_fardos_processar-safra.
    ENDIF.

    IF i_carregamento_auto EQ abap_false.
      LOOP AT lit_fardos_processar INTO lwa_fardo_processar WHERE mark EQ abap_true.
        SELECT SINGLE *
          FROM zsdt0330 INTO @DATA(lwa_zsdt0330)
         WHERE vbeln     = @lwa_fardo_processar-vbeln
           AND werks     = @lwa_fardo_processar-werks
           AND lgort     = @lwa_fardo_processar-bloco
           AND acharg    = @lwa_fardo_processar-fardo
           AND cancelado = @abap_false.

        IF sy-subrc = 0.
          e_msg_error = 'Este Fardo já está sendo Tratado pelo Processamento Automático!'.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    DATA(count_registros) = 0.

    CLEAR: lit_zmmt0008_gravar.

    LOOP AT lit_fardos_processar INTO lwa_fardo_processar WHERE mark EQ abap_true.

      ADD 1 TO count_registros.

      APPEND INITIAL LINE TO lit_zmmt0008_gravar ASSIGNING FIELD-SYMBOL(<fs_zmmt0008_gravar>).

      SELECT SINGLE *
        FROM zmmt0008 INTO @DATA(lwa_zmmt0008_exists)
       WHERE werks EQ @lwa_fardo_processar-werks
         AND lgort EQ @lwa_fardo_processar-bloco
         AND charg EQ @lwa_fardo_processar-fardo
         AND safra EQ @lwa_fardo_processar-safra.

      IF sy-subrc NE 0.
        e_msg_error = |Estoque do Fardo: { lwa_fardo_processar-fardo } / Bloco: { lwa_fardo_processar-bloco } Centro: { lwa_fardo_processar-werks }|.
        e_msg_error = |{ e_msg_error } Safra: { lwa_fardo_processar-safra }, não disponibilizado para comercialização|.
        RETURN.
      ENDIF.

      <fs_zmmt0008_gravar>-dt_registro                = lwa_zmmt0008_exists-dt_registro.
      <fs_zmmt0008_gravar>-hr_registro                = lwa_zmmt0008_exists-hr_registro.
      <fs_zmmt0008_gravar>-cd_sai                     = lwa_zmmt0008_exists-cd_sai.
      <fs_zmmt0008_gravar>-tipo_fardo                 = lwa_zmmt0008_exists-tipo_fardo.
      <fs_zmmt0008_gravar>-werks                      = lwa_zmmt0008_exists-werks.
      <fs_zmmt0008_gravar>-lgort                      = lwa_zmmt0008_exists-lgort.
      <fs_zmmt0008_gravar>-charg                      = lwa_zmmt0008_exists-charg.
      <fs_zmmt0008_gravar>-menge                      = lwa_zmmt0008_exists-menge.
      <fs_zmmt0008_gravar>-werks_orig                 = lwa_zmmt0008_exists-werks_orig.
      <fs_zmmt0008_gravar>-lgortr                     = lwa_zmmt0008_exists-lgortr.
      <fs_zmmt0008_gravar>-safra                      = lwa_zmmt0008_exists-safra.
      <fs_zmmt0008_gravar>-mblnr                      = lwa_zmmt0008_exists-mblnr.
      <fs_zmmt0008_gravar>-mjahr                      = lwa_zmmt0008_exists-mjahr.
      <fs_zmmt0008_gravar>-dados_contingencia         = lwa_zmmt0008_exists-dados_contingencia.

      "Dados do Espelho para Atualizar na ZMMT0008
      <fs_zmmt0008_gravar>-nr_romaneio                = lwa_fardo_processar-nr_romaneio.
      <fs_zmmt0008_gravar>-nfnum                      = lwa_fardo_processar-nfnum.
      <fs_zmmt0008_gravar>-vbeln                      = lwa_fardo_processar-vbeln.
      <fs_zmmt0008_gravar>-vbeln_vf                   = lwa_fardo_processar-vbeln_vf.
      <fs_zmmt0008_gravar>-placa_cav                  = lwa_fardo_processar-placa_cav.
      <fs_zmmt0008_gravar>-matnr                      = lwa_fardo_processar-matnr.
      <fs_zmmt0008_gravar>-motorista                  = lva_motorista.
      <fs_zmmt0008_gravar>-kunnr                      = lva_kunnr.
      <fs_zmmt0008_gravar>-dt_inicial_integ           = sy-datum.
      <fs_zmmt0008_gravar>-status                     = '1'.

      CLEAR: lit_selection[].

      CLEAR lwa_selection.
      lwa_selection-selname  = 'S_WERKS'.
      lwa_selection-kind     = 'S'.
      lwa_selection-sign     = 'I'.
      lwa_selection-option   = 'EQ'.
      lwa_selection-low      = <fs_zmmt0008_gravar>-werks.
      APPEND lwa_selection TO lit_selection.

      CLEAR lwa_selection.
      lwa_selection-selname  = 'S_LGORT'.
      lwa_selection-kind     = 'S'.
      lwa_selection-sign     = 'I'.
      lwa_selection-option   = 'EQ'.
      lwa_selection-low      = <fs_zmmt0008_gravar>-lgort .
      APPEND lwa_selection TO lit_selection.

      CLEAR lwa_selection.
      lwa_selection-selname  = 'S_VBN_VF'.
      lwa_selection-kind     = 'S'.
      lwa_selection-sign     = 'I'.
      lwa_selection-option   = 'EQ'.
      lwa_selection-low      = <fs_zmmt0008_gravar>-vbeln_vf .
      APPEND lwa_selection TO lit_selection.

      CLEAR lwa_selection.
      lwa_selection-selname  = 'S_PLCAV'.
      lwa_selection-kind     = 'S'.
      lwa_selection-sign     = 'I'.
      lwa_selection-option   = 'EQ'.
      lwa_selection-low      = <fs_zmmt0008_gravar>-placa_cav .
      APPEND lwa_selection TO lit_selection.


      IF _replica_fardos_intercompany = abap_true.
        READ TABLE lit_zmmt0008_replicar INTO DATA(lwa_zmmt0008_replicar) WITH KEY werks = lwa_kna1-kunnr+6(4)
                                                                                   lgort = lwa_fardo_processar-bloco
                                                                                   charg = lwa_fardo_processar-fardo
                                                                                   safra = lwa_fardo_processar-safra.
        IF ( sy-subrc <> 0 ) OR ( sy-subrc = 0 AND lwa_zmmt0008_replicar-nr_romaneio IS INITIAL ).
          APPEND INITIAL LINE TO lit_zmmt0008_gravar ASSIGNING FIELD-SYMBOL(<fs_zmmt0008_gravar_replica>).

          <fs_zmmt0008_gravar_replica>-dt_registro         =   lwa_zmmt0008_exists-dt_registro.
          <fs_zmmt0008_gravar_replica>-hr_registro         =   lwa_zmmt0008_exists-hr_registro.
          <fs_zmmt0008_gravar_replica>-werks               =   lwa_kna1-kunnr+6(4).
          <fs_zmmt0008_gravar_replica>-lgort               =   lwa_fardo_processar-bloco.
          <fs_zmmt0008_gravar_replica>-charg               =   lwa_fardo_processar-fardo.
          <fs_zmmt0008_gravar_replica>-safra               =   lwa_fardo_processar-safra.
          <fs_zmmt0008_gravar_replica>-werks_orig          =   lwa_fardo_processar-werks.
          <fs_zmmt0008_gravar_replica>-menge               =   lwa_fardo_processar-menge.
          <fs_zmmt0008_gravar_replica>-matnr               =   lwa_fardo_processar-matnr.
          <fs_zmmt0008_gravar_replica>-lgortr              =   lwa_fardo_processar-lgortr.
          <fs_zmmt0008_gravar_replica>-dados_contingencia  =   lwa_fardo_processar-dados_contingencia.
          <fs_zmmt0008_gravar_replica>-tipo_fardo          =   lwa_fardo_processar-tipo_fardo.
          <fs_zmmt0008_gravar_replica>-cd_sai              =   lwa_fardo_processar-cd_sai.

          CLEAR lwa_selection.
          lwa_selection-selname = 'S_WERKS'.
          lwa_selection-kind    = 'S'.
          lwa_selection-sign    = 'I'.
          lwa_selection-option  = 'EQ'.
          lwa_selection-low     = <fs_zmmt0008_gravar_replica>-werks.
          APPEND lwa_selection TO lit_selection.
        ENDIF.
      ENDIF.


    ENDLOOP.

    IF count_registros <= 0.
      e_msg_error = 'Nenhum fardo encontrado para processar!'.
      RETURN.
    ENDIF.

    IF i_carregamento_auto EQ abap_false.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Geração de fardos de venda'
          text_question         = |Serão gerados { count_registros } fardos de venda.|
          text_button_1         = 'Ok'
          icon_button_1         = '@0V@'
          text_button_2         = 'Cancelar'
          icon_button_2         = '@0W@'
          default_button        = '1'
          display_cancel_button = ' '
        IMPORTING
          answer                = lva_resposta.

      IF lva_resposta NE '1'.
        e_msg_error = 'Ação cancelada'.
        RETURN.
      ENDIF.
    ENDIF.

    IF i_proc_unico  = abap_false OR
       i_reprocessar = abap_true.

      MODIFY zmmt0008 FROM TABLE lit_zmmt0008_gravar.

    ENDIF.

    e_msg_sucesso = |Foram Gerados { count_registros } fardos de venda(s)!|.

*-----------------------------------------------------------------------------------------------------------*
*   Integração com Rondoline
*-----------------------------------------------------------------------------------------------------------*

    SORT lit_selection BY selname kind sign option low  .
    DELETE ADJACENT DUPLICATES FROM lit_selection COMPARING ALL FIELDS.


    IF ( i_proc_unico  = abap_false AND
         i_reprocessar = abap_false AND
         i_envia_email = abap_false AND
         i_integra_rondoline = abap_false ) OR

       ( i_proc_unico = abap_true  AND
         ( i_integra_rondoline = abap_true OR i_reprocessar = abap_true ) ).

      SELECT SINGLE *
        FROM tvarvc INTO @DATA(lwa_tvarv_integra_rondoline)
       WHERE name EQ 'ZMM0027_INTEGRA_RONDOLINE'.

      IF sy-subrc EQ 0.
        SUBMIT zmmr162 WITH SELECTION-TABLE lit_selection WITH s_acao EQ 'I' AND RETURN.
      ENDIF.

    ENDIF.


*-----------------------------------------------------------------------------------------------------------*
*   Disparo Email Espelho Romaneio
*-----------------------------------------------------------------------------------------------------------*
    IF ( i_proc_unico        = abap_false AND
         i_reprocessar       = abap_false AND
         i_envia_email       = abap_false AND
         i_integra_rondoline = abap_false ) OR

       ( i_proc_unico = abap_true  AND
         ( i_envia_email = abap_true OR i_reprocessar = abap_true ) ).

      send_email_espelho_romaneio(
        EXPORTING
          i_werks             = CONV #( lva_werks )
          i_lgort             = CONV #( lva_lgort )
          i_safra             = CONV #( lva_safra )
          i_nr_romaneio       = CONV #( lva_nr_romaneio ) ).
    ENDIF.


  ENDMETHOD.


  METHOD send_email_espelho_romaneio.

    TYPES: BEGIN OF ty_j_1bnflin,
             docnum TYPE  j_1bnflin-docnum,
             refkey TYPE  j_1bnflin-refkey,
           END OF ty_j_1bnflin,

           BEGIN OF ty_vbrp,
             vbeln  TYPE vbrp-vbeln,
             vgbel  TYPE vbrp-vgbel,
             matnr  TYPE vbrp-matnr,
             refkey TYPE j_1bnflin-refkey,
           END OF ty_vbrp.

    TYPES: BEGIN OF ty_excel,
             empresa    TYPE zmms005-empresa,
             safra      TYPE zmms005-safra,
             remetente  TYPE zmms005-remetente,
             terminal   TYPE zmms005-destinatario,
             cliente    TYPE zmms005-cliente,
             romaneio   TYPE zmms005-romaneio,
             nfnum      TYPE zmms005-nfnum,
             motorista  TYPE zmms005-motorista,
             veiculo    TYPE zmms005-veiculo,
             bloco      TYPE zmms005-bloco,
             qtd_fardos TYPE zmms006-total,
             peso_total TYPE zmms004-peso,
             instrucao  TYPE zsdt0066-instrucao,
             stcd1      TYPE j_1bbranch-stcd1,
           END OF ty_excel,

           BEGIN OF ty_fardos_exc,
             fardos  TYPE char21,
             peso    TYPE zmms004-peso,
             tipo    TYPE zmms004-tipo,
             tamanho TYPE zmmt0008-tipo_fardo,
           END OF ty_fardos_exc.

    TYPES: BEGIN OF ty_zsdt0045,
             zseq_inst     TYPE zsdt0045-zseq_inst,
             objek         TYPE zsdt0045-objek,
             objecttable   TYPE zsdt0045-objecttable,
             booking       TYPE zsdt0045-booking,
             instrucao     TYPE zsdt0045-instrucao,
             tamanho_fardo TYPE zsdt0045-tamanho_fardo,
           END OF ty_zsdt0045,

           BEGIN OF ty_mara,
             matnr TYPE mara-matnr,
             normt TYPE mara-normt,
           END OF ty_mara.

    CONSTANTS: gc_tab  TYPE c VALUE cl_bcs_convert=>gc_tab,
               gc_tab2 TYPE c VALUE cl_abap_char_utilities=>vertical_tab,
               gc_crlf TYPE c VALUE cl_bcs_convert=>gc_crlf.


    DATA: lit_excel      TYPE TABLE OF ty_excel,
          lwa_excel      TYPE ty_excel,
          lit_mara       TYPE TABLE OF ty_mara,
          lit_vbrp       TYPE TABLE OF ty_vbrp,
          lwa_zsdt0045   TYPE ty_zsdt0045,
          lit_fardos_exc TYPE TABLE OF ty_fardos_exc,
          lwa_fardos_exc TYPE ty_fardos_exc.


    CLEAR: r_msg_error.

    SELECT *
    FROM zmmt0008 INTO TABLE @DATA(it_0008)
    WHERE werks       EQ @i_werks
      AND lgort       EQ @i_lgort
      AND nr_romaneio EQ @i_nr_romaneio
      AND safra       EQ @i_safra.

    CHECK it_0008[] IS NOT INITIAL.


    READ TABLE it_0008 INTO DATA(wa_0008) INDEX 1.
    lwa_excel-veiculo   = wa_0008-placa_cav.
    lwa_excel-romaneio  = wa_0008-nr_romaneio.
    lwa_excel-motorista = wa_0008-motorista.
    lwa_excel-bloco     = wa_0008-lgort.


    SELECT SINGLE name1
      FROM kna1 INTO lwa_excel-cliente
     WHERE kunnr EQ wa_0008-kunnr.

    SELECT SINGLE *
     FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
      WHERE vbeln        EQ @wa_0008-vbeln
        AND tp_movimento EQ 'S'
        AND nr_romaneio  EQ @wa_0008-nr_romaneio
        AND branch       EQ @wa_0008-werks
        AND vbeln        EQ @wa_0008-vbeln.

    CHECK sy-subrc EQ 0.

    lwa_excel-safra = lwa_zsdt0001-nr_safra.

    SELECT SINGLE *
       FROM zsdt0066 INTO @DATA(wa_zsdt0066)
      WHERE vbeln EQ @lwa_zsdt0001-vbeln.

    "Envio de Romaneio de Saída (Algodoeira)
    CLEAR lwa_zsdt0045.

    IF sy-subrc IS INITIAL. "Busca dados baseado na formação de lote

      SELECT SINGLE zseq_inst objek objecttable booking instrucao
        FROM zsdt0045 INTO lwa_zsdt0045
        WHERE objek      EQ wa_zsdt0066-nro_sol_ov
          AND instrucao  EQ wa_zsdt0066-instrucao.

      SELECT *
        FROM zmmt0126 INTO TABLE @DATA(t_zmmt0126)
       WHERE kunnr EQ @wa_zsdt0066-lentrega.

      lwa_excel-instrucao = wa_zsdt0066-instrucao.

    ELSE. "Busca dados baseado na venda ZFEX

      SELECT SINGLE *
        FROM zsdt0053 INTO @DATA(wa_zsdt0053)
       WHERE vbeln EQ @lwa_zsdt0001-vbeln.

      SELECT SINGLE zseq_inst objek objecttable booking instrucao tamanho_fardo
        FROM zsdt0045 INTO lwa_zsdt0045
        WHERE objek      EQ wa_zsdt0053-nro_sol_ov
          AND instrucao  EQ wa_zsdt0053-instrucao.

      lwa_excel-instrucao = wa_zsdt0053-instrucao.

    ENDIF.

    "Destinatario
    SELECT SINGLE name1
      FROM kna1 INTO lwa_excel-terminal
       WHERE kunnr EQ lwa_zsdt0001-id_cli_dest.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(lwa_j_1bnflin)
       WHERE refkey EQ @wa_0008-vbeln_vf.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE *
        FROM j_1bnfe_active INTO @DATA(lwa_active)
         WHERE docnum EQ @lwa_j_1bnflin-docnum.

      IF sy-subrc IS INITIAL.
        "Nota Fiscal
        lwa_excel-nfnum = lwa_active-nfnum9.
        SELECT SINGLE *
          FROM j_1bbranch INTO @DATA(lwa_j_1bbranch)
           WHERE bukrs  EQ @lwa_active-bukrs
             AND branch EQ @lwa_active-branch.
        "Remetente
        lwa_excel-remetente = lwa_j_1bbranch-name.
        lwa_excel-stcd1     = lwa_j_1bbranch-stcd1.

        SELECT SINGLE butxt    FROM t001
          INTO lwa_excel-empresa
           WHERE bukrs EQ lwa_active-bukrs.
      ENDIF.
    ENDIF.


    SELECT vbeln vgbel matnr
      FROM vbrp INTO TABLE lit_vbrp
       WHERE vbeln EQ wa_0008-vbeln_vf.

    IF lit_vbrp[] IS NOT INITIAL.
      SELECT matnr normt
        FROM mara INTO TABLE lit_mara
        FOR ALL ENTRIES IN lit_vbrp
         WHERE matnr EQ lit_vbrp-matnr.

      READ TABLE lit_mara INTO DATA(lwa_mara) INDEX 1.
    ENDIF.

    LOOP AT it_0008 INTO wa_0008.

      CONDENSE wa_0008-charg NO-GAPS.

      lwa_fardos_exc-fardos  = |'{ wa_0008-cd_sai }|.
      lwa_fardos_exc-tipo    = lwa_mara-normt(5).
      lwa_fardos_exc-peso    = wa_0008-menge.
      lwa_fardos_exc-tamanho = wa_0008-tipo_fardo.

      IF lwa_fardos_exc-tamanho IS INITIAL.
        lwa_fardos_exc-tamanho = lwa_zsdt0045-tamanho_fardo.
      ENDIF.

      APPEND lwa_fardos_exc TO lit_fardos_exc.

    ENDLOOP.

    LOOP AT it_0008 INTO wa_0008.
      CONDENSE wa_0008-charg NO-GAPS.
      lwa_excel-qtd_fardos  = lwa_excel-qtd_fardos + 1.
      lwa_excel-peso_total  = lwa_excel-peso_total  + wa_0008-menge.
      CLEAR: wa_0008.
    ENDLOOP.

    APPEND lwa_excel  TO lit_excel.


    SELECT *
      FROM zmail INTO TABLE @DATA(lit_zmail)
      WHERE werks EQ @i_werks
      AND tcode   EQ 'ZMM0027'.

    LOOP AT t_zmmt0126 INTO DATA(w_zmmt0126).

      IF w_zmmt0126-email IS NOT INITIAL.

        APPEND VALUE #( email = w_zmmt0126-email
                        usuario = w_zmmt0126-usnam ) TO lit_zmail.

      ENDIF.

    ENDLOOP.

    SORT lit_zmail BY  email.
    DELETE ADJACENT DUPLICATES FROM lit_zmail COMPARING email.

*---------------------------------------------------------------------------------------------*
*   Monta Excel
*---------------------------------------------------------------------------------------------*

    DATA: lv_string   TYPE string,
          vqtd_fardos TYPE string,
          vpeso_total TYPE string,
          vpeso       TYPE string,
          vcnpj(18)   TYPE c.

    DATA: main_text      TYPE bcsy_text,
          gs_main_text   LIKE LINE OF main_text,
          binary_content TYPE solix_tab,
          size           TYPE so_obj_len,
          sent_to_all    TYPE os_boolean.

    LOOP AT lit_excel INTO lwa_excel.

      vqtd_fardos = lwa_excel-qtd_fardos.
      vpeso_total = lwa_excel-peso_total.

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = lwa_excel-stcd1
        IMPORTING
          output = vcnpj.

      CONCATENATE
      'Empresa:'          gc_tab    lwa_excel-empresa        gc_crlf
      'Safra:'            gc_tab    lwa_excel-safra          gc_crlf
      'Remetente:'        gc_tab    lwa_excel-remetente      gc_crlf
      'CNPJ Remetente'    gc_tab    vcnpj                   gc_crlf
      'Terminal:'         gc_tab    lwa_excel-terminal       gc_crlf
      'Cliente:'          gc_tab    lwa_excel-cliente        gc_crlf
      'Romaneio:'         gc_tab    lwa_excel-romaneio       gc_crlf
      'NFe.F.Lote:'       gc_tab    lwa_excel-nfnum          gc_crlf
      'Motorista:'        gc_tab    lwa_excel-motorista      gc_crlf
      'Veículo (Placa):'  gc_tab    lwa_excel-veiculo        gc_crlf
      'Lote (Bloco):'     gc_tab    lwa_excel-bloco          gc_crlf
      'Qtd Fardos:'       gc_tab    vqtd_fardos             gc_crlf
      'Peso Total:'       gc_tab    vpeso_total             gc_crlf
      'Instrução:'        gc_tab    lwa_excel-instrucao      gc_crlf
      INTO lv_string.

    ENDLOOP.

    CONCATENATE
    lv_string       gc_crlf
                    gc_crlf
    'Nr. Fardo'     gc_tab
    'Tipo'          gc_tab
    'Peso'          gc_tab
    'Tamanho'       gc_crlf
    INTO lv_string.

    LOOP AT lit_fardos_exc INTO lwa_fardos_exc.

      vpeso =   lwa_fardos_exc-peso.

      CONCATENATE
       lv_string
       lwa_fardos_exc-fardos   gc_tab
       lwa_fardos_exc-tipo     gc_tab
       vpeso                  gc_tab
       lwa_fardos_exc-tamanho  gc_crlf
      INTO lv_string.

    ENDLOOP.

    CLEAR: size, binary_content.

    TRY.
        cl_bcs_convert=>string_to_solix(
          EXPORTING
            iv_string   = lv_string
            iv_codepage = '4103'
            iv_add_bom  = 'X'
          IMPORTING
            et_solix    = binary_content
            ev_size     = size ).
      CATCH cx_bcs.
        MESSAGE e445(so).
    ENDTRY.

    IF lit_zmail[] IS INITIAL.
      r_msg_error = 'Romaneio gerado, mas não foram encontrados e-mail cadastrados!'.
    ENDIF.

*---------------------------------------------------------------------------------------------*
*   Dispara Email
*---------------------------------------------------------------------------------------------*

    DATA: send_request  TYPE REF TO cl_bcs,
          document      TYPE REF TO cl_document_bcs,
          recipient     TYPE REF TO if_recipient_bcs,
          bcs_exception TYPE REF TO cx_bcs.

    DATA: mailto  TYPE ad_smtpadr,
          subject TYPE so_obj_des.

    DATA: vuser TYPE sy-uname.

    DATA: l_romaneio TYPE c LENGTH 9.

    CLEAR:  main_text, subject, vuser, l_romaneio.

    l_romaneio =  lwa_excel-romaneio.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = l_romaneio
      IMPORTING
        output = l_romaneio.

    CONCATENATE 'Romaneio' l_romaneio 'SI' lwa_excel-instrucao INTO subject SEPARATED BY space.

    "Informações Adicionais - Ini
    MOVE '<BR>' TO gs_main_text-line.
    APPEND gs_main_text TO main_text.
    CLEAR gs_main_text.

    CONCATENATE 'Segue em anexo o Romaneio de Saída de Fardos.'(014) '<BR><BR>'
  INTO gs_main_text-line SEPARATED BY space.
    APPEND gs_main_text TO main_text.  CLEAR gs_main_text.

    MOVE '<TABLE BORDER=1>' TO gs_main_text-line.
    APPEND gs_main_text TO main_text.   CLEAR gs_main_text.

    "col1
    MOVE '<TR style="background-color: #96A5AA;"> <TH> Romaneio </TH> ' TO gs_main_text-line.
    APPEND gs_main_text TO main_text.   CLEAR gs_main_text.

    "col2
    MOVE '<TH> Instrução </TH> ' TO gs_main_text-line.
    APPEND gs_main_text TO main_text.   CLEAR gs_main_text.

    "col3
    MOVE '<TH> Booking </TH></TR>' TO gs_main_text-line.
    APPEND gs_main_text TO main_text.   CLEAR gs_main_text.


    "col1
    IF lwa_excel-romaneio IS NOT INITIAL.
      CONCATENATE '<TR> <TD>' lwa_excel-romaneio '</TD>' INTO gs_main_text-line.
      APPEND gs_main_text TO main_text.   CLEAR gs_main_text.
    ELSE.
      MOVE '<TR> <TD></TD>' TO gs_main_text-line.
      APPEND gs_main_text TO main_text.   CLEAR gs_main_text.
    ENDIF.

    "col2
    CONCATENATE '<TD>' lwa_excel-instrucao '</TD>' INTO gs_main_text-line.
    APPEND gs_main_text TO main_text.   CLEAR gs_main_text.

    "col3
    CONCATENATE '<TD>' lwa_zsdt0045-booking '</TD></TR>' INTO gs_main_text-line.
    APPEND gs_main_text TO main_text. CLEAR gs_main_text.


    MOVE '</TABLE>' TO gs_main_text-line.
    APPEND gs_main_text TO main_text.  CLEAR gs_main_text.

    MOVE '<BR>' TO gs_main_text-line.
    APPEND gs_main_text TO main_text.
    CLEAR gs_main_text.

    CONCATENATE '<BR>' 'Atenciosamente'(013) '<BR>'
    INTO gs_main_text-line.
    APPEND gs_main_text TO main_text. CLEAR gs_main_text.

    APPEND '  '     TO main_text.
    "Informações Adicionais - Fim


    TRY.
        send_request = cl_bcs=>create_persistent( ).

        document  = cl_document_bcs=>create_document(
           i_type    = 'HTM'
           i_text    = main_text
           i_subject = subject  ).

        document->add_attachment(
          i_attachment_type    = 'xls'
          i_attachment_subject = subject
          i_attachment_size    = size
          i_att_content_hex    = binary_content  ).


        send_request->set_document( document ).

        LOOP AT lit_zmail INTO DATA(wa_zmail).
          IF wa_zmail IS NOT INITIAL.
            CLEAR mailto.

            mailto = wa_zmail-email.

            recipient = cl_cam_address_bcs=>create_internet_address( mailto ).

            send_request->add_recipient( recipient ).
          ENDIF.
        ENDLOOP.

        vuser = sy-uname.
        sy-uname = 'JOBADM'.
        sent_to_all = send_request->send( i_with_error_screen = 'X' ).

        COMMIT WORK.
        sy-uname = vuser.

        IF sent_to_all IS INITIAL.
          r_msg_error = 'O destinatário não existe'.
        ENDIF.

      CATCH cx_bcs INTO bcs_exception.
        MESSAGE i865(so) WITH bcs_exception->error_type INTO r_msg_error.
    ENDTRY.


  ENDMETHOD.


  METHOD cadastro_email_espelho_rom.

    DATA: lra_local_entrega TYPE RANGE OF kna1-kunnr.

    DATA: t_set  TYPE TABLE OF rgsb4,
          wa_set TYPE rgsb4.
    DATA: v_check TYPE c VALUE ''.

    DATA: v_init     TYPE c.

    CONSTANTS: c_view TYPE char30 VALUE 'ZVMMT0126',
               c_u    TYPE char1 VALUE 'U',
               c_and  TYPE char3 VALUE 'AND'.

    DATA: gt_seltab   TYPE STANDARD TABLE OF vimsellist,
          g_fieldname TYPE vimsellist-viewfield,
          gt_exclude  TYPE TABLE OF vimexclfun,
          gwa_exclude TYPE vimexclfun.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        client        = sy-mandt
        setnr         = 'ZMM0027_USER'
        table         = 'ZMMT0005'
        class         = '0000'
        fieldname     = 'USNAM'
      TABLES
        set_values    = t_set
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.

    READ TABLE t_set INTO DATA(w_set) WITH KEY from = sy-uname.
    IF sy-subrc NE 0.
      MESSAGE s000(z_mm) WITH 'Usuário sem autorização! ' 'Contactar equipe Agro corporativo.'
     DISPLAY LIKE 'E'.
    ENDIF.

    FREE MEMORY ID 'SEL'.

    v_init = 4.

    EXPORT v_init FROM v_init TO MEMORY ID 'SEL'.

    IF i_local_entrega IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_local_entrega ) TO lra_local_entrega.
    ENDIF.

    g_fieldname = 'KUNNR'.
    CALL FUNCTION 'VIEW_RANGETAB_TO_SELLIST'
      EXPORTING
        fieldname          = g_fieldname
        append_conjunction = c_and
      TABLES
        sellist            = gt_seltab
        rangetab           = lra_local_entrega.

    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action      = c_u
        view_name   = c_view
      TABLES
        dba_sellist = gt_seltab.



  ENDMETHOD.


  METHOD gera_sobra_perda_romaneio.

    DATA: lwa_zsdt0001_mov_est TYPE zsdt0001_mov_est.
    DATA: lwa_zsdt0344         TYPE zsdt0344.


    DATA: lva_material TYPE bapi2017_gm_head_ret-mat_doc,
          lva_year     TYPE bapi2017_gm_head_ret-doc_year,
          lva_code     TYPE bapi2017_gm_code.


    DATA: lwa_header TYPE bapi2017_gm_head_01,
          lwa_item   TYPE bapi2017_gm_item_create,
          lit_item   TYPE TABLE OF bapi2017_gm_item_create,
          lit_return TYPE TABLE OF bapiret2,
          lwa_return TYPE bapiret2.


    DATA: lit_fardos_disponibilizar TYPE zpps0007_t,
          lva_msg_aux               TYPE string,
          lva_error_lock            TYPE c,
          lva_resposta              TYPE c.

    CLEAR: e_msg_error, e_msg_sucesso, e_mblnr , e_mjahr.

    IF i_zsdt0330 IS NOT INITIAL.

      IF i_zsdt0330-ch_referencia IS INITIAL.
        e_msg_error = |Romaneio para movimento de Ganho/Perda não informado!|.
        RETURN.
      ENDIF.

      CALL FUNCTION 'ENQUEUE_EZSDT0344'
        EXPORTING
          id_carga       = i_zsdt0330-id_carga
          seq_carga      = i_zsdt0330-seq
          vbeln          = i_zsdt0330-vbeln
          nr_romaneio    = i_zsdt0330-nr_romaneio
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CHECK sy-subrc EQ 0.

      SELECT SINGLE *
        FROM zsdt0344 INTO lwa_zsdt0344
       WHERE id_carga          = i_zsdt0330-id_carga
         AND seq_carga         = i_zsdt0330-seq
         AND vbeln             = i_zsdt0330-vbeln
         AND nr_romaneio       = i_zsdt0330-nr_romaneio
         AND ch_referencia_rom = i_zsdt0330-ch_referencia.

      IF sy-subrc NE 0.
        e_msg_error = |Registro processamento Ganho Perda não encontrado!|.
        RETURN.
      ENDIF.

      CHECK lwa_zsdt0344-mblnr IS INITIAL.

      "MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO e_msg_error.
      "RETURN.


    ENDIF.


    IF i_zsdt0001-ch_referencia IS INITIAL.
      e_msg_error = |Romaneio para movimento de Ganho/Perda não informado!|.
      RETURN.
    ENDIF.

    IF i_matnr IS INITIAL.
      e_msg_error = |Material para movimento de sobra/perda não informado!|.
      RETURN.
    ENDIF.

    IF i_werks IS INITIAL.
      e_msg_error = |Centro para movimento de sobra/perda não informado!|.
      RETURN.
    ENDIF.

    IF i_safra IS INITIAL.
      e_msg_error = |Safra para movimento de sobra/perda não informado!|.
      RETURN.
    ENDIF.

    IF i_qtde_mov IS INITIAL.
      e_msg_error = |Quantidade movimento de sobra/perda não informado!|.
      RETURN.
    ENDIF.

    IF i_tipo_movimento IS INITIAL.
      e_msg_error = |Tipo movimento de sobra/perda não informado!|.
      RETURN.
    ENDIF.


    IF i_qtde_mov > 0.
      lva_msg_aux = |Criando documento de sobra para o romaneio { CONV i( i_zsdt0001-nr_romaneio ) }|.
    ELSE.
      lva_msg_aux = |Criando documento de perda para o romaneio { CONV i( i_zsdt0001-nr_romaneio ) }|.
    ENDIF.


    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = lva_msg_aux.


*-------------------------------------------------------------------------------------------------------*
*   Iniciar Chamada BAPI
*-------------------------------------------------------------------------------------------------------*

    CLEAR: lva_material, lva_year, lit_item[], lit_return[], lwa_header.

*---------------------------------------------------------------------*
*  Preenchimento Dados Cabeçalho BAPI
*---------------------------------------------------------------------*
    lva_code              = '06'.
    lwa_header-pstng_date = sy-datum.
    lwa_header-doc_date   = sy-datum.


*---------------------------------------------------------------------*
* Preenchimento Dados Cabeçalho BAPI
*---------------------------------------------------------------------*

    CLEAR: lwa_item.

    IF i_qtde_mov > 0.
      lwa_item-move_type    = '521'.
    ELSE.
      lwa_item-move_type    = '522'.
    ENDIF.

    lwa_item-material     = i_matnr.
    lwa_item-plant        = i_werks.
    lwa_item-stge_loc     = 'ALGD'.
    lwa_item-batch        = i_safra.
    lwa_item-entry_qnt    = abs( i_qtde_mov ).


    APPEND lwa_item TO lit_item.

    DO 5 TIMES.

      DATA(_index) = sy-index.

      CLEAR: lva_material, lva_year, lit_return[], lva_error_lock.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = lwa_header
          goodsmvt_code    = lva_code
        IMPORTING
          materialdocument = lva_material
          matdocumentyear  = lva_year
        TABLES
          goodsmvt_item    = lit_item
          return           = lit_return.

      IF lva_material IS NOT INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        e_mblnr = lva_material.
        e_mjahr = lva_year.
        e_msg_sucesso = |Documento : { lva_material }  Ano: { lva_year } gerado com Sucesso!|.

        CLEAR: lwa_zsdt0001_mov_est.
        lwa_zsdt0001_mov_est-ch_referencia  = i_zsdt0001-ch_referencia.
        lwa_zsdt0001_mov_est-id_carga       = i_zsdt0330-id_carga.
        lwa_zsdt0001_mov_est-seq_carga      = i_zsdt0330-seq.
        lwa_zsdt0001_mov_est-mblnr          = e_mblnr.
        lwa_zsdt0001_mov_est-mjahr          = e_mjahr.
        lwa_zsdt0001_mov_est-tipo_movimento = i_tipo_movimento.
        lwa_zsdt0001_mov_est-quantidade     = i_qtde_mov.
        lwa_zsdt0001_mov_est-dt_registro    = sy-datum.
        lwa_zsdt0001_mov_est-hr_registro    = sy-uzeit.
        lwa_zsdt0001_mov_est-us_registro    = sy-uname.
        MODIFY zsdt0001_mov_est FROM lwa_zsdt0001_mov_est.

        IF lwa_zsdt0344 IS NOT INITIAL.
          lwa_zsdt0344-mblnr          = e_mblnr.
          lwa_zsdt0344-mjahr          = e_mjahr.
          lwa_zsdt0344-quantidade     = i_qtde_mov.
          lwa_zsdt0344-dt_registro    = sy-datum.
          lwa_zsdt0344-hr_registro    = sy-uzeit.
          lwa_zsdt0344-us_registro    = sy-uname.
          MODIFY zsdt0344 FROM lwa_zsdt0344.
        ENDIF.

        COMMIT WORK.


        RETURN.

      ELSE.

        READ TABLE  lit_return INTO lwa_return WITH KEY type = 'E'.
        IF sy-subrc EQ 0.

          CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
            EXPORTING
              id       = lwa_return-id
              number   = lwa_return-number
            IMPORTING
              is_block = lva_error_lock.


          IF lva_error_lock IS NOT INITIAL AND _index LE 5.
            WAIT UP TO 3 SECONDS.
          ELSE.
            e_msg_error = lwa_return-message.
            RETURN.
          ENDIF.

        ELSE.
          e_msg_error = 'Houve um erro desconhecido ao gerar o movimento de sobra/perda!'.
          RETURN.
        ENDIF.

      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD get_mov_sobra_perda_romaneio.

    CLEAR: e_mblnr, e_mjahr.

    CHECK i_ch_referencia IS NOT INITIAL.

    SELECT SINGLE *
      FROM zsdt0001_mov_est AS a INTO @DATA(lwa_mov_est)
     WHERE ch_referencia = @i_ch_referencia
       AND tipo_movimento IN ( '001' ,'002' ) "Manutenção e Ganho/Perda Peso Algodão
       AND EXISTS (  SELECT mblnr
                          FROM mkpf AS b
                         WHERE b~mblnr = a~mblnr
                           AND b~mblnr NE @space )
       AND NOT EXISTS (  SELECT mblnr
                           FROM mseg AS b
                          WHERE b~smbln = a~mblnr ).

    CHECK sy-subrc EQ 0.

    e_mblnr = lwa_mov_est-mblnr.
    e_mjahr = lwa_mov_est-mjahr.


  ENDMETHOD.


  METHOD get_list_sobra_perda_romaneio.

    CLEAR: r_mov_est_t[].

    CHECK i_ch_referencia IS NOT INITIAL.

    SELECT *
      FROM zsdt0001_mov_est AS a INTO TABLE r_mov_est_t
     WHERE ch_referencia = i_ch_referencia
       AND tipo_movimento IN ( '001' ,'002' ) "Manutenção e Ganho/Perda Peso Algodão
       AND EXISTS (  SELECT mblnr
                          FROM mkpf AS b
                         WHERE b~mblnr = a~mblnr
                           AND b~mblnr NE space )
       AND NOT EXISTS (  SELECT mblnr
                           FROM mseg AS b
                          WHERE b~smbln = a~mblnr ).

  ENDMETHOD.


  METHOD get_qtde_sobra_perda_romaneio.

    CLEAR: e_qtde_perda_sobra.

    CHECK i_ch_referencia IS NOT INITIAL.

    SELECT *
      FROM zsdt0001_mov_est AS a INTO TABLE @DATA(lit_mov_est)
     WHERE ch_referencia = @i_ch_referencia
       AND tipo_movimento IN ( '001', '002' ) "Ganho/Perda Peso Algodão
       AND EXISTS (  SELECT mblnr
                          FROM mkpf AS b
                         WHERE b~mblnr = a~mblnr
                           AND b~mblnr NE @space )
       AND NOT EXISTS (  SELECT mblnr
                           FROM mseg AS b
                          WHERE b~smbln = a~mblnr ).

    CHECK lit_mov_est[] IS NOT INITIAL.

    LOOP AT lit_mov_est ASSIGNING FIELD-SYMBOL(<fs_mov_est>).
      ADD <fs_mov_est>-quantidade TO e_qtde_perda_sobra.
    ENDLOOP.

  ENDMETHOD.


  METHOD estornar_sobra_perda_romaneio.

    DATA: lva_mat_doc               TYPE bapi2017_gm_head_02-mat_doc,
          lva_doc_year              TYPE bapi2017_gm_head_02-doc_year,
          lva_pstng_date            TYPE bapi2017_gm_head_02-pstng_date,
          lwa_invoicedocnumber_migo TYPE bapi2017_gm_head_ret,
          lit_return                TYPE TABLE OF bapiret2.

    CLEAR: r_msg_error.

    DATA(_lit_mov_estoque) = zcl_comercializacao_algodao=>get_list_sobra_perda_romaneio( i_ch_referencia = i_ch_referencia ).

    LOOP AT _lit_mov_estoque ASSIGNING FIELD-SYMBOL(<fs_mov_estoque>) WHERE mblnr IS NOT INITIAL.

      lva_mat_doc    = <fs_mov_estoque>-mblnr.
      lva_doc_year   = <fs_mov_estoque>-mjahr.
      lva_pstng_date = sy-datum.

      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument    = lva_mat_doc
          matdocumentyear     = lva_doc_year
          goodsmvt_pstng_date = lva_pstng_date
        IMPORTING
          goodsmvt_headret    = lwa_invoicedocnumber_migo
        TABLES
          return              = lit_return.

      IF lit_return[] IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        READ TABLE  lit_return INTO DATA(lwa_return) WITH KEY type = 'E'.
        IF sy-subrc EQ 0.
          MESSAGE ID lwa_return-id TYPE 'S'
            NUMBER lwa_return-number
              WITH lwa_return-message_v1
                   lwa_return-message_v2
                   lwa_return-message_v3
                   lwa_return-message_v4 INTO r_msg_error.
        ELSE.
          r_msg_error = 'Houve um erro desconhecido ao gerar o movimento de sobra/perda!'.
          RETURN.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_fardos_romaneio.

    CLEAR r_zsdt0330_t[].

    SELECT *
      FROM zsdt0330 INTO CORRESPONDING FIELDS OF TABLE r_zsdt0330_t
     WHERE ch_referencia  EQ i_ch_referencia
       AND status_fardo   EQ '3'
       AND status_estorno NE 'D'
       AND cancelado      EQ abap_false.


  ENDMETHOD.


  METHOD ck_ajuste_sobra_perda_romaneio.

    TYPES: BEGIN OF ty_zsdt0330,
             safra_ TYPE zmmt0008-safra.
             INCLUDE TYPE zsdt0330.
    TYPES: END OF ty_zsdt0330.

    DATA: lva_peso_fardos TYPE zmmt0008-menge.
    DATA: lva_qtde_mov TYPE zmmt0008-menge.
    DATA: lva_qtde_sobra_perda_new TYPE zmmt0008-menge.

    DATA: lit_fardos_romaneio TYPE TABLE OF ty_zsdt0330.

    CLEAR: e_msg_error, e_msg_sucesso, e_mblnr, e_mjahr.

    CHECK i_id_carga IS NOT INITIAL AND i_ch_referencia IS NOT INITIAL.

    SELECT SINGLE *
      FROM zsdt0330 AS a INTO @DATA(lwa_zsdt0330_processar)
     WHERE id_carga          EQ @i_id_carga
       AND ch_referencia     EQ @i_ch_referencia
       AND cancelado         EQ @abap_false

     "Não pode ter "adição" de fardos pendentes de geração
     AND NOT EXISTS (  SELECT id_carga
                         FROM zsdt0330 AS b
                        WHERE b~id_carga       EQ a~id_carga
                          AND b~seq            EQ a~seq
                          AND b~ch_referencia  EQ a~ch_referencia
                          AND b~status_estorno IN ( '', 'I' )
                          AND b~status_fardo   NE '3' )

     AND NOT EXISTS ( SELECT id_carga
                        FROM zsdt0344 AS b
                       WHERE b~id_carga          EQ a~id_carga
                         AND b~seq_carga         EQ a~seq
                         AND b~ch_referencia_rom EQ a~ch_referencia
                         AND b~mblnr             NE @space )

     AND seq  = ( SELECT MAX( seq )
                   FROM zsdt0330 AS b
                  WHERE b~id_carga    EQ a~id_carga ).

    IF sy-subrc NE 0.
      e_msg_error = |Registro carregamento ID: { i_id_carga } não possui encontrado!|.
      RETURN.
    ENDIF.

    IF lwa_zsdt0330_processar-ch_referencia IS INITIAL.
      e_msg_error = |Não encontrado romaneio para carregamento ID { i_id_carga } não encontrado!|.
      RETURN.
    ENDIF.

    DATA(_qtde_perda_sobra_gerado) =  zcl_comercializacao_algodao=>get_qtde_sobra_perda_romaneio( i_ch_referencia = lwa_zsdt0330_processar-ch_referencia ).

    SELECT SINGLE *
      FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
     WHERE ch_referencia = @lwa_zsdt0330_processar-ch_referencia.

    CHECK sy-subrc EQ 0.

    "Recuperar Fardos Antes da manutenção
    SELECT *
      FROM zsdt0330 AS a INTO CORRESPONDING FIELDS OF TABLE lit_fardos_romaneio
     WHERE id_carga          EQ lwa_zsdt0330_processar-id_carga
       AND seq               NE lwa_zsdt0330_processar-seq
       AND ch_referencia     EQ lwa_zsdt0330_processar-ch_referencia
       AND status_estorno    IN ( '', 'I' )
       AND cancelado         EQ abap_false
       AND status_fardo      EQ '3'.

    "Recuperar Fardos da manutenção
    SELECT *
      FROM zsdt0330 AS a INTO TABLE @DATA(lit_fardos_manut)
     WHERE id_carga          EQ @lwa_zsdt0330_processar-id_carga
       AND seq               EQ @lwa_zsdt0330_processar-seq
       AND ch_referencia     EQ @lwa_zsdt0330_processar-ch_referencia
       AND cancelado         EQ @abap_false.

    LOOP AT lit_fardos_manut ASSIGNING FIELD-SYMBOL(<fs_fardos_manut>).
      CASE <fs_fardos_manut>-status_estorno.
        WHEN 'I' OR space.
          APPEND INITIAL LINE TO lit_fardos_romaneio ASSIGNING FIELD-SYMBOL(<fs_fardo_romaneio>).
          MOVE-CORRESPONDING <fs_fardos_manut> TO <fs_fardo_romaneio>.
        WHEN 'D'.
          READ TABLE lit_fardos_romaneio ASSIGNING <fs_fardo_romaneio> WITH KEY id_carga  = <fs_fardos_manut>-id_carga
                                                                                  matnr     = <fs_fardos_manut>-matnr
                                                                                  werks     = <fs_fardos_manut>-werks
                                                                                  lgort     = <fs_fardos_manut>-lgort
                                                                                  acharg    = <fs_fardos_manut>-acharg
                                                                                  safra     = <fs_fardos_manut>-safra.
          IF sy-subrc NE 0.
            e_msg_error = |Fardo { <fs_fardos_manut>-acharg } não encontrado na carga para deleção!|.
            RETURN.
          ENDIF.

          <fs_fardo_romaneio>-cancelado = abap_true.
      ENDCASE.

    ENDLOOP.

    DELETE lit_fardos_romaneio WHERE cancelado = abap_true.

    LOOP AT lit_fardos_romaneio ASSIGNING <fs_fardo_romaneio>.
      <fs_fardo_romaneio>-safra_ = <fs_fardo_romaneio>-safra.
    ENDLOOP.

    IF lit_fardos_romaneio[] IS NOT INITIAL.
      SELECT *
       FROM zmmt0008 INTO TABLE @DATA(lit_zmmt0008)
       FOR ALL ENTRIES IN @lit_fardos_romaneio
      WHERE werks = @lit_fardos_romaneio-werks
        AND lgort = @lit_fardos_romaneio-lgort
        AND charg = @lit_fardos_romaneio-acharg
        AND safra = @lit_fardos_romaneio-safra_.
    ENDIF.

    CLEAR: lva_peso_fardos.
    LOOP AT lit_zmmt0008 ASSIGNING FIELD-SYMBOL(<fs_zmmt0008>).
      ADD <fs_zmmt0008>-menge TO lva_peso_fardos.
    ENDLOOP.

    READ TABLE lit_zmmt0008 INTO DATA(lwa_zmmt0008_mov) INDEX 1.
    lva_qtde_sobra_perda_new = lwa_zsdt0001-peso_liq - lva_peso_fardos.


    lva_qtde_mov = lva_qtde_sobra_perda_new - _qtde_perda_sobra_gerado.

    CHECK lva_qtde_mov NE 0.

    gera_sobra_perda_romaneio(
      EXPORTING
        i_zsdt0330       = lwa_zsdt0330_processar
        i_zsdt0001       = lwa_zsdt0001
        i_matnr          = CONV #( lwa_zsdt0330_processar-matnr )
        i_werks          = CONV #( lwa_zsdt0330_processar-werks )
        i_safra          = CONV #( lwa_zsdt0330_processar-safra )
        i_qtde_mov       = CONV #( lva_qtde_mov )
        i_tipo_movimento = '002' "Manutençaõ Ganho/Perda Peso Algodão
      IMPORTING
        e_msg_sucesso    = e_msg_sucesso
        e_mblnr          = e_mblnr
        e_mjahr          = e_mjahr
        e_msg_error      = e_msg_error ).


  ENDMETHOD.


  METHOD ck_ajuste_sobra_perda_pendente.

    CHECK i_chave_referencia_rom IS NOT INITIAL.

    DATA(lwa_zsdt0344_reiniciado) = zcl_comercializacao_algodao=>reinicializa_mov_sobra_perda( i_chave_referencia_rom = i_chave_referencia_rom ).

    CHECK lwa_zsdt0344_reiniciado IS NOT INITIAL.

    zcl_comercializacao_algodao=>ck_ajuste_sobra_perda_romaneio(
      EXPORTING
        i_id_carga      = lwa_zsdt0344_reiniciado-id_carga
        i_ch_referencia = lwa_zsdt0344_reiniciado-ch_referencia_rom
      IMPORTING
        e_msg_sucesso   = DATA(_msg_sucesso)
        e_mblnr         = DATA(_mblnr)
        e_mjahr         = DATA(_mjahr)
        e_msg_error     = DATA(_msg_error)  ).


  ENDMETHOD.


  METHOD reinicializa_mov_sobra_perda.

    CLEAR: r_zsdt0344_reinicializado.

    CHECK i_chave_referencia_rom IS NOT INITIAL.

    SELECT SINGLE * INTO @DATA(lwa_zsdt0344)
      FROM zsdt0344 AS a
     WHERE a~ch_referencia_rom  EQ @i_chave_referencia_rom
       AND a~seq_carga          EQ ( SELECT MAX( seq )
                                       FROM zsdt0330 AS b
                                     WHERE b~id_carga EQ a~id_carga ).


    CHECK sy-subrc EQ 0 AND lwa_zsdt0344-mblnr IS NOT INITIAL.

    SELECT SINGLE *
      FROM mkpf AS a INTO @DATA(lwa_mkpf)
     WHERE mblnr EQ @lwa_zsdt0344-mblnr
       AND mjahr EQ @lwa_zsdt0344-mjahr
       AND EXISTS (  SELECT mblnr
                       FROM mseg AS b
                      WHERE b~smbln = a~mblnr
                        AND b~sjahr = a~mjahr ).

    CHECK sy-subrc EQ 0. "Se Documento já foi estornado, reinicializa para gerar denovo movimento de sobra perda

    CLEAR: lwa_zsdt0344-mblnr,
           lwa_zsdt0344-mjahr,
           lwa_zsdt0344-quantidade.

    lwa_zsdt0344-dt_registro = sy-datum.
    lwa_zsdt0344-hr_registro = sy-uzeit.
    lwa_zsdt0344-us_registro = sy-uname.

    MODIFY zsdt0344 FROM lwa_zsdt0344.

    COMMIT WORK.

    r_zsdt0344_reinicializado = lwa_zsdt0344.


  ENDMETHOD.
ENDCLASS.
