class ZCL_EXC_APONT_MED definition
  public
  final
  create public .

public section.

  class-methods SELEC_DADOS_COMB .
  class-methods SELECT_PONT_MEDICAO
    importing
      value(I_EQUNR) type EQUNR optional
    returning
      value(T_DIMPT) type Z_IMPTT_T .
  class-methods PROCESSA_DOCUMENTO
    importing
      !T_DIMPT type Z_IMPTT_T
      !I_EQUNR type EQUNR
      !W_ZPME0058 type ZPME0058
      !P_SUB_CONT type CHAR01
    exporting
      !T_LOG type ZPME0061_T
      !P_ERR type CHAR01 .
  class-methods GET_EQUIPAMENTO
    importing
      !I_PLACA type LICENSE_NUM
    exporting
      !E_EQUNR type EQUNR .
  class-methods CHECK_ERRO
    importing
      value(T_REPORT) type ZPME0051_T optional
    exporting
      value(T_ERRO) type ZPME0050_T .
*---> 20.06.2023 - Migração S4 - DG
"      !I_ITEM type CHAR02
*<--- 20.06.2023 - Migração S4 - DG
  class-methods SET_DOC
    importing
      value(W_ZPME0058) type ZPME0058
      !I_MSGTY type SYST_MSGTY
      !I_MSGV1 type SYST_MSGV
      !I_ITEM type ZCHAR02
      value(I_DOC_MED) type IMRC_MDOCM optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EXC_APONT_MED IMPLEMENTATION.


  METHOD CHECK_ERRO.
*    FREE: T_ERRO.
*    DATA: PLCA TYPE CHAR7.
*    DATA: TEXT TYPE STRING.
*
*
*    LOOP AT T_REPORT ASSIGNING FIELD-SYMBOL(<_REPORT>).
*      ZCL_EXC_APONT_MED=>GET_EQUIPAMENTO(
*                EXPORTING
*                  I_PLACA = <_REPORT>-PLACA " Placa de veículo
*                IMPORTING
*                  E_EQUNR = DATA(EQUIPAMENTO) " Nº equipamento
*              ).
*
*      ZCL_EXC_APONT_MED=>SELECT_PONT_MEDICAO(
*          EXPORTING
*          I_EQUNR = EQUIPAMENTO
*          RECEIVING
*          T_DIMPT = DATA(T_DIMPT)
*      ).
*
*      DATA(IT_DIMPT) = T_DIMPT.
*      SORT IT_DIMPT BY ATNAM.
*      DELETE IT_DIMPT WHERE ATNAM NE 'COMBUSTIVEL'.
*      DELETE IT_DIMPT WHERE INACT EQ ABAP_TRUE.
*
*      IF IT_DIMPT IS INITIAL.
*        IF <_REPORT>-COD_MATERIAL EQ '000000000000000007'.
*          APPEND VALUE #( AVALIACAO     = 'Veiculo não possue pontos de medição de combustivel cadastrado, favor cadastrar.'
*                              FATURA        = <_REPORT>-FATURA
*                              CNPJ          = <_REPORT>-CNPJ
*                              DT_CUPOM_FISC = <_REPORT>-DT_CUPOM_FISC
*                              HR_CUPOM_FISC = <_REPORT>-HR_CUPOM_FISC
*                              PLACA         = <_REPORT>-PLACA
*                              COD_MATERIAL  = <_REPORT>-COD_MATERIAL
*                              QTDE          = <_REPORT>-QTDE
*                              ) TO T_ERRO.
**          <_REPORT>-ICON          = ICON_ANNOTATION.
*        ENDIF.
*      ENDIF.
*
*
*      "Verificando se material foi atribuido ao ponto de medição.
*      LOOP AT T_DIMPT ASSIGNING FIELD-SYMBOL(<W_DIMPT>).
*        CASE <W_DIMPT>-ATNAM.
*          WHEN 'COMBUSTIVEL'.
*            IF <W_DIMPT>-LOCAS IS INITIAL.
*              IF <_REPORT>-COD_MATERIAL EQ '000000000000000007'.
*                APPEND VALUE #( AVALIACAO = 'Veiculo não possue material 184924 atribuido ao ponto de medição de combustivel, favor abribuir.'
*                                FATURA        = <_REPORT>-FATURA
*                                CNPJ          = <_REPORT>-CNPJ
*                                DT_CUPOM_FISC = <_REPORT>-DT_CUPOM_FISC
*                                HR_CUPOM_FISC = <_REPORT>-HR_CUPOM_FISC
*                                PLACA         = <_REPORT>-PLACA
*                                COD_MATERIAL  = <_REPORT>-COD_MATERIAL
*                                QTDE          = <_REPORT>-QTDE
*                               ) TO T_ERRO.
*
**                <_REPORT>-ICON          = ICON_ANNOTATION.
*              ENDIF.
*            ENDIF.
*        ENDCASE.
*      ENDLOOP.
*
*
*      SORT T_DIMPT BY ATNAM.
*      DELETE T_DIMPT WHERE ATNAM NE 'ODOMETRO'.
*      DELETE T_DIMPT WHERE INACT EQ ABAP_TRUE.
*      IF T_DIMPT IS INITIAL.
*        IF <_REPORT>-COD_MATERIAL EQ '000000000000000007'.
*          APPEND VALUE #( AVALIACAO     = 'Veiculo não possue pontos de medição odometro cadastrado, favor cadastrar.'
*                                  FATURA        = <_REPORT>-FATURA
*                                  CNPJ          = <_REPORT>-CNPJ
*                                  DT_CUPOM_FISC = <_REPORT>-DT_CUPOM_FISC
*                                  HR_CUPOM_FISC = <_REPORT>-HR_CUPOM_FISC
*                                  PLACA         = <_REPORT>-PLACA
*                                  COD_MATERIAL  = <_REPORT>-COD_MATERIAL
*                                  QTDE          = <_REPORT>-QTDE
*                                  ) TO T_ERRO.
*
*          <_REPORT>-ICON          = ICON_ANNOTATION.
*        ENDIF.
*      ENDIF.
*
*      "Verificar se o valor odometro esta negativo ou igual anterior.
*      IF <_REPORT>-ODOMETRO < <_REPORT>-ODOMETRO_ANT.
*        IF <_REPORT>-COD_MATERIAL EQ '000000000000000007'.
*          APPEND VALUE #( AVALIACAO     = 'Medição não pode ser menor que a medição anterior.'
*                                  FATURA        = <_REPORT>-FATURA
*                                  CNPJ          = <_REPORT>-CNPJ
*                                  DT_CUPOM_FISC = <_REPORT>-DT_CUPOM_FISC
*                                  HR_CUPOM_FISC = <_REPORT>-HR_CUPOM_FISC
*                                  PLACA         = <_REPORT>-PLACA
*                                  COD_MATERIAL  = <_REPORT>-COD_MATERIAL
*                                  QTDE          = <_REPORT>-QTDE
*                                  ) TO T_ERRO.
*
*          <_REPORT>-ICON          = ICON_ANNOTATION.
*        ENDIF.
*      ENDIF.
*
*      "Verificar se o valor odometro esta negativo ou igual anterior.
*      IF <_REPORT>-ODOMETRO = <_REPORT>-ODOMETRO_ANT.
*        IF <_REPORT>-COD_MATERIAL EQ '000000000000000007'.
*          APPEND VALUE #( AVALIACAO     = 'Medição não pode ser igual a medição anterior, verificar odometro se esta quebrado e atualizar no sistema.'
*                                  FATURA        = <_REPORT>-FATURA
*                                  CNPJ          = <_REPORT>-CNPJ
*                                  DT_CUPOM_FISC = <_REPORT>-DT_CUPOM_FISC
*                                  HR_CUPOM_FISC = <_REPORT>-HR_CUPOM_FISC
*                                  PLACA         = <_REPORT>-PLACA
*                                  COD_MATERIAL  = <_REPORT>-COD_MATERIAL
*                                  QTDE          = <_REPORT>-QTDE
*                                  ) TO T_ERRO.
*
*          <_REPORT>-ICON          = ICON_ANNOTATION.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.


  METHOD GET_EQUIPAMENTO.
    DATA: GW_VEICULO TYPE EQUZ.
    FREE GW_VEICULO.

    SELECT SINGLE A~EQUNR
      FROM EQUZ AS A
      INNER JOIN EQUI AS B ON B~EQUNR EQ A~EQUNR
      INNER JOIN FLEET AS C ON C~OBJNR EQ B~OBJNR
        INTO E_EQUNR
          WHERE C~LICENSE_NUM EQ I_PLACA
            AND A~DATBI EQ '99991231'.
  ENDMETHOD.


  METHOD processa_documento.
    DATA:
      lv_reader          TYPE sy-uname,
      lv_value           TYPE rimr0-recdc,
      v_dt_ponto         TYPE sy-datum,
      v_hr_ponto         TYPE sy-uzeit,
      lv_docu            TYPE imrg-mdocm,
      lv_point           TYPE diimpt-point,
      lv_measurement_doc TYPE imrg-mdocm,
      ls_doc_complete    TYPE imrg,
      ls_notification    TYPE qmel-qmnum,
      lv_errsubrc        TYPE c LENGTH 50.


    FREE: t_log.
    CLEAR: p_err, v_dt_ponto, v_hr_ponto.

*    CHECK T_DIMPT IS NOT INITIAL.
    LOOP AT t_dimpt  ASSIGNING FIELD-SYMBOL(<w_dimpt>) WHERE indtr NE abap_true.
*
      CLEAR: lv_docu, lv_errsubrc, lv_measurement_doc.
      v_dt_ponto = w_zpme0058-dt_cupom_fisc.
      v_hr_ponto = w_zpme0058-hr_cupom_fisc.

      CASE <w_dimpt>-atnam.
        WHEN 'ODOMETRO' OR 'HORIMETRO'.

          DATA(v_odometro) = w_zpme0058-odometro.
          CONDENSE v_odometro NO-GAPS.
          REPLACE '.' IN v_odometro WITH ','.
*          REPLACE ALL OCCURRENCES OF ',' IN LV_VALUE WITH '.'.

          zcl_int_sappm_autotrac=>m_check_pont_med(
            EXPORTING
              i_date  = v_dt_ponto
              i_time  = v_hr_ponto
              i_point = <w_dimpt>-point   " Ponto de medição
            IMPORTING
              e_value =   DATA(pos_contador)  " Unidade de medida ao entrar documento
          ).


          IF v_odometro <= pos_contador.
            "Processa log.
            zcl_exc_apont_med=>set_doc(
              EXPORTING
                w_zpme0058 =  w_zpme0058   " Estrutra de dados para input de apontamento medição frota pr
*                i_doc_med  = ''
                i_msgty    =  'E'   " Campo do sistema: tipo de mensagem
*                  I_MSGV1    =  'Posição do contador ' && LV_DOCU && ' criado com sucesso'   " Campo do sistema ABAP: variável da mensagem
                i_msgv1    =  'Posição do contador ' && v_odometro && ' menor que o contador atual ->' && pos_contador" Campo do sistema ABAP: variável da mensagem
                i_item     = '40'
            ).
            p_err = abap_true.
            CLEAR: pos_contador, v_odometro.
            EXIT.
          ELSE.

            IF <w_dimpt>-indtr NE abap_true.
              "Criar documento medição odometro.
              CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
                EXPORTING
                  measurement_point    = <w_dimpt>-point
                  secondary_index      = ' '
                  reading_date         = v_dt_ponto
                  reading_time         = v_hr_ponto
                  short_text           = 'Abastecimento externo'
                  reader               = sy-uname
                  origin_indicator     = ' '
                  reading_after_action = ' '
                  recorded_value       = v_odometro
                  recorded_unit        = ' '
                  difference_reading   = ' '
                  code_version         = ' '
                  "USER_DATA            = ' '
                  check_custom_duprec  = ' '
                  with_dialog_screen   = ' '
                  prepare_update       = 'X'
                  commit_work          = 'X'
                  wait_after_commit    = 'X'
                IMPORTING
                  measurement_document = lv_docu
                EXCEPTIONS
                  no_authority         = 1
                  point_not_found      = 2
                  index_not_unique     = 3
                  type_not_found       = 4
                  point_locked         = 5
                  point_inactive       = 6
                  timestamp_in_future  = 7
                  timestamp_duprec     = 8
                  unit_unfit           = 9
                  value_not_fltp       = 10
                  value_overflow       = 11
                  value_unfit          = 12
                  value_missing        = 13
                  code_not_found       = 14
                  notif_type_not_found = 15
                  notif_prio_not_found = 16
                  notif_gener_problem  = 17
                  update_failed        = 18
                  invalid_time         = 19
                  invalid_date         = 20
                  OTHERS               = 21.

              IF sy-subrc NE 0.

                CASE sy-subrc.
                  WHEN 1.
                    lv_errsubrc = text-065.
                  WHEN 2.
                    lv_errsubrc = text-066.
                  WHEN 3.
                    lv_errsubrc = text-067.
                  WHEN 4.
                    lv_errsubrc = text-068.
                  WHEN 5.
                    lv_errsubrc = text-069.
                  WHEN 6.
                    lv_errsubrc = text-070.
                  WHEN 7.
                    lv_errsubrc = text-071.
                  WHEN 8.
                    lv_errsubrc = text-072.
                  WHEN 9.
                    lv_errsubrc = text-073.
                  WHEN 10.
                    lv_errsubrc = text-074.
                  WHEN 11.
                    lv_errsubrc = text-074.
                  WHEN 12.
                    lv_errsubrc = text-099.
                  WHEN 13.
                    lv_errsubrc = text-075.
                  WHEN 14.
                    lv_errsubrc = text-076.
                  WHEN 19.
                    lv_errsubrc = text-077.
                  WHEN 20.
                    lv_errsubrc = text-072.
                  WHEN OTHERS.
                    lv_errsubrc = text-098.
                ENDCASE.

                "Processa log.
                zcl_exc_apont_med=>set_doc(
                  EXPORTING
                    w_zpme0058 =  w_zpme0058   " Estrutra de dados para input de apontamento medição frota pr
*                    i_doc_med  = ' '
                    i_msgty    =  'E'   " Campo do sistema: tipo de mensagem
                    i_msgv1    =  lv_errsubrc  " Campo do sistema ABAP: variável da mensagem
                    i_item     = '10'
                ).


                p_err = abap_true.
                CLEAR: pos_contador, v_odometro.
                EXIT.
              ELSE.
                "Processa log.
*                w_zpme0058-mdocm =  lv_docu.
                zcl_exc_apont_med=>set_doc(
                  EXPORTING
                    w_zpme0058 =  w_zpme0058   " Estrutra de dados para input de apontamento medição frota pr
                    i_doc_med  = lv_docu
                    i_msgty    =  'S'   " Campo do sistema: tipo de mensagem
                    i_msgv1    =  'Documento ' && lv_docu && ' criado com sucesso '" Campo do sistema ABAP: variável da mensagem
                    i_item     = '10'
                ). "V_ODOMETRO >= POS_CONTADOR.

                APPEND VALUE #( mdocm = lv_docu ) TO t_log.
              ENDIF.
            ENDIF.
          ENDIF.
        WHEN 'COMBUSTIVEL'.

*          SELECT SINGLE MATNR
*          FROM ZPMT0034
*          INTO @DATA(COD_MATERIAL)
*            WHERE COD_MATERIAL EQ @W_ZPME0058-COD_MATERIAL.

*          IF COD_MATERIAL EQ <W_DIMPT>-LOCAS.
          lv_value = w_zpme0058-qtde.
          REPLACE ALL OCCURRENCES OF '.' IN lv_value WITH ','.
          CONDENSE lv_value NO-GAPS.

          " Grava Apontamento de Consumo
          CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
            EXPORTING
              measurement_point    = <w_dimpt>-point
              reading_date         = v_dt_ponto
              reading_time         = v_hr_ponto
              short_text           = 'Abastecimento externo'
              reader               = sy-uname
              origin_indicator     = 'A'
              recorded_value       = lv_value
              difference_reading   = abap_true
              prepare_update       = 'X'
              commit_work          = 'X'
              wait_after_commit    = 'X'
            IMPORTING
              measurement_document = lv_measurement_doc
            EXCEPTIONS
              no_authority         = 1
              point_not_found      = 2
              index_not_unique     = 3
              type_not_found       = 4
              point_locked         = 5
              point_inactive       = 6
              timestamp_in_future  = 7
              timestamp_duprec     = 8
              unit_unfit           = 9
              value_not_fltp       = 10
              value_overflow       = 11
              value_unfit          = 12
              value_missing        = 13
              code_not_found       = 14
              notif_type_not_found = 15
              notif_prio_not_found = 16
              notif_gener_problem  = 17
              update_failed        = 18
              invalid_time         = 19
              invalid_date         = 20
              OTHERS               = 21.

          IF sy-subrc NE 0.
            CASE sy-subrc.
              WHEN 1.
                lv_errsubrc = text-065.
              WHEN 2.
                lv_errsubrc = text-066.
              WHEN 3.
                lv_errsubrc = text-067.
              WHEN 4.
                lv_errsubrc = text-068.
              WHEN 5.
                lv_errsubrc = text-069.
              WHEN 6.
                lv_errsubrc = text-070.
              WHEN 7.
                lv_errsubrc = text-071.
              WHEN 8.
                lv_errsubrc = text-072.
              WHEN 9.
                lv_errsubrc = text-073.
              WHEN 10.
                lv_errsubrc = text-074.
              WHEN 11.
                lv_errsubrc = text-074.
              WHEN 12.
                lv_errsubrc = text-074.
              WHEN 13.
                lv_errsubrc = text-075.
              WHEN 14.
                lv_errsubrc = text-076.
              WHEN 19.
                lv_errsubrc = text-077.
              WHEN 20.
                lv_errsubrc = text-072.
              WHEN OTHERS.
                lv_errsubrc = text-098.
            ENDCASE.

            "Processa log.
            zcl_exc_apont_med=>set_doc(
              EXPORTING
                w_zpme0058 =  w_zpme0058   " Estrutra de dados para input de apontamento medição frota pr
*                i_doc_med  = ' '
                i_msgty    =  'E'   " Campo do sistema: tipo de mensagem
                i_msgv1    =  lv_errsubrc  " Campo do sistema ABAP: variável da mensagem
                i_item     = '20'
            ).

            p_err = abap_true.
            clear: pos_contador, v_odometro.
            EXIT.
          ELSE.
            "Processa log.
*            w_zpme0058-mdocm =  lv_measurement_doc.
            zcl_exc_apont_med=>set_doc(
              EXPORTING
                w_zpme0058 =  w_zpme0058   " Estrutra de dados para input de apontamento medição frota pr
                i_doc_med  =  lv_measurement_doc
                i_msgty    =  'S'   " Campo do sistema: tipo de mensagem
                i_msgv1    =  'Documento ' && lv_measurement_doc && ' criado com sucesso'   " Campo do sistema ABAP: variável da mensagem
                i_item     = '20'
            ).

            APPEND VALUE #( mdocm = lv_measurement_doc ) TO t_log.
          ENDIF.
*          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD SELECT_PONT_MEDICAO.

    DATA: IT_IMPTT TYPE TABLE OF DIIMPT,
          WA_DIMPT TYPE DIIMPT,
          VCONT    TYPE P DECIMALS 2,
          VTPPONTO TYPE DIIMPT-ATNAM.


    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
      EXPORTING
        I_EQUNR   = I_EQUNR
      TABLES
        ET_DIIMPT = T_DIMPT.

    SORT T_DIMPT BY ATNAM.
  ENDMETHOD.


  METHOD SELEC_DADOS_COMB.

    DATA: GT_ZPMT0024 TYPE TABLE OF ZPMT0024.
    DATA: GT_ZPMT0026 TYPE TABLE OF ZPMT0026.
    DATA: GT_VEICULO TYPE TABLE OF EQUZ.

**** Selecionar veiculos que realizaram abastecimento.
*    SELECT * FROM ZPMT0024 INTO TABLE GT_ZPMT0024 WHERE CHECK_APONT NE ABAP_TRUE.
*    CHECK GT_ZPMT0024 IS NOT INITIAL.
*    SORT GT_ZPMT0024 ASCENDING BY PLACA DT_CUPOM_FISC HR_CUPOM_FISC.
*
*****Seleciona os abastecimento dos veiculos selecionados.
*    SELECT * FROM ZPMT0026 INTO TABLE GT_ZPMT0026
*    FOR ALL ENTRIES IN GT_ZPMT0024 WHERE FATURA EQ GT_ZPMT0024-FATURA AND CNPJ EQ GT_ZPMT0024-CNPJ AND CUPOM_FISC EQ GT_ZPMT0024-CUPOM_FISC.
*
*    LOOP AT GT_ZPMT0024 ASSIGNING FIELD-SYMBOL(<W_ZPMT0024>).
*      LOOP AT GT_ZPMT0026 ASSIGNING FIELD-SYMBOL(<W_ZPMT0026>) WHERE FATURA EQ <W_ZPMT0024>-FATURA AND CNPJ EQ <W_ZPMT0024>-CNPJ AND CUPOM_FISC EQ <W_ZPMT0024>-CUPOM_FISC.

        "Seleciona equipamento.
*        ZCL_EXC_APONT_MED=>GET_EQUIPAMENTO( EXPORTING I_PLACA = <W_ZPMT0024>-PLACA  IMPORTING E_EQUNR = DATA(EQUNR) ).

        "Seleciona ponto de medição do equpamento.
*        IF EQUNR IS NOT INITIAL.
*          ZCL_EXC_APONT_MED=>SELECT_PONT_MEDICAO( EXPORTING I_EQUNR =   EQUNR RECEIVING T_DIMPT = DATA(T_DIMPT) ).

          "Processa documento.
*          ZCL_EXC_APONT_MED=>PROCESSA_DOCUMENTO( EXPORTING T_DIMPT =  T_DIMPT  I_EQUNR = EQUNR  W_ZPMT0058 =  ).
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.

  ENDMETHOD.


  METHOD SET_DOC.

    DATA: W_ZPMT0035 TYPE ZPMT0035.

    W_ZPMT0035 = VALUE #(   ID             = W_ZPME0058-ID
                            ITEM           = I_ITEM
                            ADDRESS        = W_ZPME0058-ADDRESS
                            CODE           = W_ZPME0058-CODE
                            EQUNR          = W_ZPME0058-EQUNR
                            MOTORISTA      = W_ZPME0058-MOTORISTA
                            DT_CUPOM_FISC  = W_ZPME0058-DT_CUPOM_FISC
                            HR_CUPOM_FISC  = W_ZPME0058-HR_CUPOM_FISC
                            PLACA          = W_ZPME0058-PLACA
                            ODOMETRO       = W_ZPME0058-ODOMETRO
                            CUPOM_FISC     = W_ZPME0058-CUPOM_FISC
                            COD_MATERIAL   = W_ZPME0058-COD_MATERIAL
                            QTDE           = W_ZPME0058-QTDE
                            VLR_TOTAL      = W_ZPME0058-VLR_TOTAL
                            FROTA          = W_ZPME0058-FROTA
                            GRU            = W_ZPME0058-GRU
                            USUARIO        = W_ZPME0058-USUARIO
                            MSGTY          = I_MSGTY
                            MSGV1          = I_MSGV1
                            LT_ARLA        = W_ZPME0058-LT_ARLA
                            MDOCM          = i_doc_med ).


    "Gravar as informações de log na tabela.
    MODIFY ZPMT0035 FROM W_ZPMT0035.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
  ENDMETHOD.
ENDCLASS.
