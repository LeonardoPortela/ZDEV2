class ZCL_INTEG_COMB definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_COMB .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEG_COMB IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_interface_comb.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = ''.

  ENDMETHOD.


  METHOD zif_integracao_comb~check_deposito.
    DATA: ls_t370fld_stn TYPE t370fld_stn.

    SELECT SINGLE storage
    FROM t370fld_stn
    INTO ls_t370fld_stn
    WHERE storage EQ me->zif_integracao_comb~at_deposito.

    IF ls_t370fld_stn IS INITIAL.
      me->zif_integracao_comb~at_return = VALUE bapiret2( type     = 'E' message  = |Deposito { me->zif_integracao_comb~at_deposito } não localizado!| number  = 1 ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_comb~check_material.
    DATA: lv_matkl TYPE mara-matkl.

    "Verifica cadastro do material.
    CLEAR: lv_matkl.
    SELECT SINGLE *
    FROM  mara
    INTO @DATA(WS_MARA)
    WHERE matnr = @me->zif_integracao_comb~at_material.
    lv_matkl = WS_MARA-matkl.

    IF  lv_matkl IS INITIAL.
      me->zif_integracao_comb~at_return = VALUE bapiret2( type     = 'E' message  = |Material { me->zif_integracao_comb~at_material } não cadastrado| number  = 1 ).
    ENDIF.

    "Verifica se o material é controlado por lote.
    "Caso seja verificar o saldo do material.
    "Verifica cadastro do material.
    IF WS_MARA-XCHPF EQ ABAP_TRUE.
    SELECT SINGLE CHARG
    FROM MCHB
    INTO me->zif_integracao_comb~AT_LOTE
    WHERE matnr = me->zif_integracao_comb~at_material
      AND WERKS = me->zif_integracao_comb~AT_CENTRO
      AND LGORT = me->zif_integracao_comb~AT_DEPOSITO
      AND CLABS > 0.
      IF SY-SUBRC NE 0.
        me->zif_integracao_comb~at_return = VALUE bapiret2( type     = 'E' message  = |Falta saldo estoque material { me->zif_integracao_comb~at_material }| number  = 1 ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_comb~check_periodo.

    CLEAR: me->zif_integracao_comb~at_return.

    DATA: ls_t001k TYPE  t001k,
          lv_data  TYPE sy-datum,
          lv_hora  TYPE sy-uzeit.

    clear: ls_t001k.


    CALL FUNCTION 'AIP01_PLANT_DETERMINE'
      EXPORTING
        i_werks  = me->zif_integracao_comb~at_centro
      IMPORTING
        es_t001k = ls_t001k
      EXCEPTIONS
        OTHERS   = 1.


    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        p_data_ent     = me->zif_integracao_comb~at_date
        p_bukrs        = ls_t001k-bukrs
        p_val_fi       = 'X'
        p_val_mm       = 'X'
      EXCEPTIONS
        data_fi_mm_nao = 1
        OTHERS         = 2.



    IF sy-subrc IS NOT INITIAL.
      me->zif_integracao_comb~at_return = VALUE bapiret2( type = 'E' message  = |A data informada esta em um período bloqueado para a empresa { ls_t001k-bukrs }.| number  = 1 ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_comb~check_veiculo.

    DATA: lv_equip         TYPE equi-equnr,
          wl_general_equi  TYPE bapi_itob,
          wl_specific_equi TYPE bapi_itob_eq_only,
          wl_return        TYPE bapiret2.


    CALL FUNCTION 'BAPI_EQUI_GETDETAIL' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        equipment         = me->zif_integracao_comb~at_epto
      IMPORTING
        data_general_exp  = me->zif_integracao_comb~at_dados_equipamento
        data_specific_exp = wl_specific_equi
        return            = wl_return.

    IF  me->zif_integracao_comb~at_dados_equipamento IS INITIAL.
      me->zif_integracao_comb~at_return = VALUE bapiret2( type     = wl_return-type message  = wl_return-message number  = 1 ).
    ENDIF.

    IF wl_specific_equi IS NOT INITIAL.
      me->zif_integracao_comb~at_eqtyp = wl_specific_equi-equicatgry.
    ENDIF.


  ENDMETHOD.


  METHOD zif_integracao_comb~estorna_doc_medicao.

    IF me->zif_integracao_comb~at_doc_med IS NOT INITIAL.
      CALL FUNCTION 'MEASUREM_DOCUM_CANCEL_ARRAY'
        EXPORTING
          messages_allowed       = abap_false
        TABLES
          cancel_requests        = me->zif_integracao_comb~at_doc_med
        EXCEPTIONS
          no_authority           = 1
          foreign_lock_occured   = 2
          system_failure_occured = 3
          recursiveness_found    = 4
          error_message          = 5
          OTHERS                 = 6.

      CHECK sy-subrc IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      LOOP AT me->zif_integracao_comb~at_doc_med ASSIGNING FIELD-SYMBOL(<w_doc>).
        me->zif_integracao_comb~at_return = VALUE bapiret2( type = 'S' message  = 'Documento medição estornado com sucesso!' number  = 0 ).
      ENDLOOP.

      free: me->zif_integracao_comb~at_doc_med.

    ENDIF.


  ENDMETHOD.


  METHOD zif_integracao_comb~get_instance.
    IF zif_integracao_comb~at_if_integracao_comb IS NOT BOUND.
      CREATE OBJECT zif_integracao_comb~at_if_integracao_comb
        TYPE zcl_integ_comb.
    ENDIF.
    r_if_integracao_comb = zif_integracao_comb~at_if_integracao_comb.
  ENDMETHOD.


  METHOD zif_integracao_comb~get_int_comb.
    r_if_integracao_comb = me.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_comb~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_comboio  ).




  ENDMETHOD.


  METHOD zif_integracao_comb~get_json.
    r_if_integracao_comb = me.



    me->zif_integracao_comb~monta_json( EXPORTING i_comboio = me->zif_integracao_comb~at_combust RECEIVING e_json = DATA(lc_json) ).
    e_json = lc_json.

    "CHECK me->zif_integracao_comb~at_json IS NOT INITIAL.
    "e_json = me->zif_integracao_comb~at_json.

  ENDMETHOD.


  METHOD zif_integracao_comb~monta_json.

    DATA: wc_json  TYPE string,
          w_json   TYPE string,
          zqtde    TYPE sy-index,
          zvirgula TYPE char1.
    CLEAR: wc_json.

    CLEAR: zqtde.
    DESCRIBE TABLE i_comboio LINES zqtde.

    LOOP AT i_comboio ASSIGNING FIELD-SYMBOL(<_abast>).
* Construindo JSON.

      IF zqtde = sy-tabix.
        zvirgula = ' '.
      ELSE.
        zvirgula = ','.
      ENDIF.

      wc_json = |{ wc_json }{ <_abast>-movi_codigo }{ zvirgula } |.
    ENDLOOP.

    e_json = |[ { wc_json } ]|.
  ENDMETHOD.


  METHOD zif_integracao_comb~proc_baixa_estoque.

    DATA: ls_header  TYPE bapi2017_gm_head_01,
          ls_code    TYPE bapi2017_gm_code,
          ls_item    TYPE bapi2017_gm_item_create,
          lt_item    TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          lt_return  TYPE STANDARD TABLE OF bapiret2,
          lw_return  TYPE bapiret2,
          ls_testrun TYPE STANDARD TABLE OF bapi2017_gm_gen,
          lw_testrun TYPE bapi2017_gm_gen,
          ls_return  TYPE bapiret2,
          lv_mater   TYPE bapi2017_gm_head_ret-mat_doc,
          lv_matkl   TYPE mara-matkl.



    CLEAR: ls_return, ls_header, ls_code, ls_item, lt_return, lv_mater, lv_matkl.
    FREE: lt_item, lt_return, ls_testrun.

    "Dados do header.
    ls_header = VALUE #( pstng_date = me->zif_integracao_comb~at_date
                         doc_date   = me->zif_integracao_comb~at_date
                         ref_doc_no = me->zif_integracao_comb~at_dados_equipamento-standorder ).


    "Dados do code.
    ls_code = VALUE #( gm_code  = '03' ).

*---> 04/07/2023 - Migração S4 - EO
    "Dados do item.
*    ls_item-material = me->zif_integracao_comb~at_material.
    DATA(v_len) = strlen( zif_integracao_comb~at_material ).

    ls_item = VALUE #( material      = COND #( WHEN v_len <= 18
                                               THEN zif_integracao_comb~at_material )
                       material_long = COND #( WHEN v_len > 18
                                               THEN zif_integracao_comb~at_material ) ).
*<--- 04/07/2023 - Migração S4 - EO

    SELECT SINGLE matkl
    FROM  mara
    INTO lv_matkl
    WHERE matnr = me->zif_integracao_comb~at_material.

    IF lv_matkl IS NOT INITIAL.
      SELECT SINGLE saknr
      FROM zmmt0039
      INTO ls_item-gl_account
      WHERE matkl = lv_matkl.
    ENDIF.


    ls_item-plant        = me->zif_integracao_comb~at_centro.
    ls_item-move_type    = '261'.
    ls_item-entry_qnt    = me->zif_integracao_comb~at_dados_abast-movi_volume.
    ls_item-orderid      = me->zif_integracao_comb~at_dados_equipamento-standorder.
    ls_item-stge_loc     = zif_integracao_comb~at_deposito.
    if zif_integracao_comb~at_lote is not INITIAL.
      ls_item-BATCH = zif_integracao_comb~at_lote.
    endif.


    IF me->zif_integracao_comb~at_dados_equipamento-standorder IS NOT INITIAL.
      SELECT SINGLE vornr
           FROM afvc AS a
          INNER JOIN afko AS b ON a~aufpl = b~aufpl
           INTO ls_item-activity
          WHERE b~aufnr = me->zif_integracao_comb~at_dados_equipamento-standorder.
    ENDIF.

    APPEND ls_item TO lt_item.
    CLEAR ls_item.

    lw_testrun-testrun = ' '.
    APPEND lw_testrun TO ls_testrun.

    "Execute BAPI.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = ls_header
        goodsmvt_code    = ls_code
      IMPORTING
        materialdocument = lv_mater  "Documento criado.
      TABLES
        goodsmvt_item    = lt_item
        return           = lt_return.

    CLEAR: ls_header, ls_code, lt_item[].


    IF lv_mater IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      ls_return = VALUE #( type = 'S' number = 0 message = |Documento material { lv_mater } criado com sucesso| ).
      me->zif_integracao_comb~set_log(
        EXPORTING
          i_dados  =  me->zif_integracao_comb~at_dados_abast   " Informações integradas SAP x UNIDATA
          i_return =  ls_return ).


      me->zif_integracao_comb~at_dados_abast-status_processamento = 'S'.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      READ TABLE lt_return INTO lw_return INDEX 1.
      "Processa log.

      ls_return = VALUE #( type = lw_return-type number = 1 message = lw_return-message ).
      me->zif_integracao_comb~set_log(
        EXPORTING
          i_dados  =  me->zif_integracao_comb~at_dados_abast   " Informações integradas SAP x UNIDATA
          i_return =  ls_return                                " Parâmetro de retorno
      ).

      "Atualiza status da tabela da ALV.
      me->zif_integracao_comb~at_dados_abast-status_processamento = 'E'.

********      Estorna documento de medição de horimetro/Odometro.
      me->zif_integracao_comb~estorna_doc_medicao( ).


    ENDIF.


  ENDMETHOD.


  METHOD zif_integracao_comb~proc_contador_odom_hom.


    DATA:
      lv_reader          TYPE sy-uname,
      lv_value           TYPE rimr0-recdc,
      v_dt_ponto         TYPE sy-datum,
      v_hr_ponto         TYPE sy-uzeit,
      v_hr_interv        TYPE p,
      lv_docu            TYPE imrg-mdocm,
      lv_point           TYPE diimpt-point,
      lv_measurement_doc TYPE imrg-mdocm,
      ls_doc_complete    TYPE imrg,
      ls_notification    TYPE qmel-qmnum,
      lv_errsubrc        TYPE c LENGTH 50,
      v_odometro         TYPE imrc_cntrc,
      pos_contador       TYPE imrc_cntrc,
      ls_return          TYPE bapiret2,
*--> IR117573 / cs1039215
      wa_odometro        type mseg-menge,
      wa_contador        type mseg-menge.
*<--- IR117573 / cs1039215

    FREE: me->zif_integracao_comb~at_doc_med.
    clear: v_odometro.

    IF me->zif_integracao_comb~at_ponto_medicao IS NOT INITIAL.

      SORT zif_integracao_comb~at_ponto_medicao BY atnam.

      LOOP AT me->zif_integracao_comb~at_ponto_medicao  ASSIGNING FIELD-SYMBOL(<w_dimpt>) WHERE indtr NE abap_true.
*
        CLEAR: lv_docu, lv_errsubrc, lv_measurement_doc, ls_return, pos_contador, v_odometro.
        v_dt_ponto = me->zif_integracao_comb~at_date.
        v_hr_ponto = me->zif_integracao_comb~at_hora.

        CASE <w_dimpt>-atnam.
          WHEN 'ODOMETRO' OR 'HORIMETRO'. "USER STORY 105446 / Anderson Oenning

            v_odometro = CONV #( me->zif_integracao_comb~at_dados_abast-movi_totalizador_veiculo ).
            CONDENSE v_odometro NO-GAPS.

            "Posição do contador atual.
            zcl_int_sappm_autotrac=>m_check_pont_med(
              EXPORTING
                i_date  = v_dt_ponto
                i_time  = v_hr_ponto
                i_point = <w_dimpt>-point   " Ponto de medição
              IMPORTING
                e_value =   pos_contador  " Unidade de medida ao entrar documento
                e_data  =   DATA(e_data)
            ).

            CONDENSE pos_contador NO-GAPS.
*--> IR117573 / cs1039215

            REPLACE ',' IN pos_contador WITH '.'.
            REPLACE ',' IN v_odometro   WITH '.'.

            wa_odometro = v_odometro.
            wa_contador = pos_contador.
*<-- IR117573 / cs1039215

            REPLACE '.' IN pos_contador WITH ','.
            REPLACE '.' IN v_odometro   WITH ','.
*--> IR117573 / cs1039215

*            IF v_odometro EQ pos_contador. "Se o contador for igual, verificar se o abastecimento aconteceu num intervalo de 30 minutos.
              "Processa log.
            if wa_odometro eq wa_contador.
*<-- IR117573 / cs1039215

              IF ( e_data-idate NE v_dt_ponto ).

                ls_return = VALUE #( type = 'E' number = 2 message = 'Odometro/Horimetro igual o anterior verificar se esta quebrado' ).
                me->zif_integracao_comb~set_log(
                  EXPORTING
                    i_dados  =  me->zif_integracao_comb~at_dados_abast   " Informações integradas SAP x UNIDATA
                    i_return =  ls_return                                " Parâmetro de retorno
                ).

                IF me->zif_integracao_comb~at_doc_med  IS NOT INITIAL.
                  "Estorna documento criado para odometro/horimetro.
                  me->zif_integracao_comb~estorna_doc_medicao( ).
                ENDIF.
                CONTINUE.

              ELSE. "Se for a mesma data, verificar o intervalo dentro se esta dentro de 30 minutos.
                CLEAR: v_hr_interv .
                v_hr_interv = ( v_hr_ponto - e_data-itime ).
                v_hr_interv = ( v_hr_interv / 60 ).

                IF v_hr_interv > 30. "Se for maior que 30 minutos, gravar o log de erro.

                  ls_return = VALUE #( type = 'E' number = 2 message = 'Odometro/Horimetro igual o anterior verificar se esta quebrado' ).
                  me->zif_integracao_comb~set_log(
                    EXPORTING
                      i_dados  =  me->zif_integracao_comb~at_dados_abast   " Informações integradas SAP x UNIDATA
                      i_return =  ls_return                                " Parâmetro de retorno
                  ).

                  IF me->zif_integracao_comb~at_doc_med  IS NOT INITIAL.
                    "Estorna documento criado para odometro/horimetro.
                    me->zif_integracao_comb~estorna_doc_medicao( ).
                  ENDIF.

                  "Atualiza status da tabela da ALV.
                  me->zif_integracao_comb~at_dados_abast-status_processamento = 'E'.
                  CONTINUE.

                ENDIF.
              ENDIF.
            ENDIF.

*--> IR117573 / cs1039215
          if ( wa_odometro LT wa_contador ).
*         IF ( v_odometro LT pos_contador ). "Se o contador for menor que o odometro/horimetro anterior gravar log de erro.
              "Processa log.
*<-- IR117573 / cs1039215
              ls_return = VALUE #( type = 'E' number = 2 message = 'Contador menor que contador atual' ).
              me->zif_integracao_comb~set_log(
                EXPORTING
                  i_dados  =  me->zif_integracao_comb~at_dados_abast   " Informações integradas SAP x UNIDATA
                  i_return =  ls_return                                " Parâmetro de retorno
              ).

              IF me->zif_integracao_comb~at_doc_med  IS NOT INITIAL.
                "Estorna documento criado para odometro/horimetro.
                me->zif_integracao_comb~estorna_doc_medicao( ).
              ENDIF.

              "Atualiza status da tabela da ALV.
              me->zif_integracao_comb~at_dados_abast-status_processamento = 'E'.

              CONTINUE.
            ENDIF.

*            IF <w_dimpt>-indtr NE abap_true.
              "Criar documento medição odometro.
              CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
                EXPORTING
                  measurement_point    = <w_dimpt>-point
                  secondary_index      = ' '
                  reading_date         = v_dt_ponto
                  reading_time         = v_hr_ponto
                  short_text           = 'Integração SAP x UNIDATA'
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
                    DATA(id_msg) = sy-subrc.
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
                IF id_msg IS NOT INITIAL.
                  ls_return = VALUE #( type = 'E' number = 2 message = lv_errsubrc ).
                ELSE.
                  ls_return = VALUE #( type = 'E' number = 3 message = lv_errsubrc ).
                ENDIF.

                me->zif_integracao_comb~set_log(
                  EXPORTING
                    i_dados  =  me->zif_integracao_comb~at_dados_abast   " Informações integradas SAP x UNIDATA
                    i_return =  ls_return                                " Parâmetro de retorno
                ).

                IF me->zif_integracao_comb~at_doc_med  IS NOT INITIAL.
                  "Estorna documento criado para odometro/horimetro.
                  me->zif_integracao_comb~estorna_doc_medicao( ).
                ENDIF.

                "Atualiza status da tabela da ALV.
                me->zif_integracao_comb~at_dados_abast-status_processamento = 'E'.

                CONTINUE.
              ELSE.
                ls_return = VALUE #( type = 'S' number = 0 message = |Documento { lv_docu } criado com sucesso| ).
                me->zif_integracao_comb~set_log(
                  EXPORTING
                    i_dados  =  me->zif_integracao_comb~at_dados_abast   " Informações integradas SAP x UNIDATA
                    i_return =  ls_return ).

                APPEND VALUE #( mdocm = lv_docu ) TO me->zif_integracao_comb~at_doc_med.
                CONTINUE.
              ENDIF.
*            ENDIF.

          WHEN 'COMBUSTIVEL' OR 'FLUIDO_ARLA_32'.

            lv_value = zif_integracao_comb~at_dados_abast-movi_volume.
            REPLACE ALL OCCURRENCES OF '.' IN lv_value WITH ','.
            CONDENSE lv_value NO-GAPS.

            " Grava Apontamento de Consumo
            CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
              EXPORTING
                measurement_point    = <w_dimpt>-point
                reading_date         = v_dt_ponto
                reading_time         = v_hr_ponto
                short_text           = 'Integração SAP x UNIDATA'
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
              ls_return = VALUE #( type = 'E' number = 2 message = lv_errsubrc ).
              me->zif_integracao_comb~set_log(
                EXPORTING
                  i_dados  =  me->zif_integracao_comb~at_dados_abast   " Informações integradas SAP x UNIDATA
                  i_return =  ls_return                                " Parâmetro de retorno
              ).

              IF me->zif_integracao_comb~at_doc_med  IS NOT INITIAL.
                "Estorna documento criado para odometro/horimetro.
                me->zif_integracao_comb~estorna_doc_medicao( ).
              ENDIF.

              "Atualiza status da tabela da ALV.
              me->zif_integracao_comb~at_dados_abast-status_processamento = 'E'.
              CONTINUE.
            ELSE.
              "Processa log.
              ls_return = VALUE #( type = 'S' number = 0 message = |Documento { lv_measurement_doc } criado com sucesso| ).
              me->zif_integracao_comb~set_log(
                EXPORTING
                  i_dados  =  me->zif_integracao_comb~at_dados_abast   " Informações integradas SAP x UNIDATA
                  i_return =  ls_return ).

              APPEND VALUE #( mdocm = lv_measurement_doc ) TO me->zif_integracao_comb~at_doc_med.

              CONTINUE.
            ENDIF.
        ENDCASE.
        CLEAR: id_msg.
      ENDLOOP.

    ELSE.

      ls_return = VALUE #( type = 'E' number = 0 message = 'Não existe ponto de medição cadastrado para equipamento' ).
      me->zif_integracao_comb~set_log(
        EXPORTING
          i_dados  =  me->zif_integracao_comb~at_dados_abast   " Informações integradas SAP x UNIDATA
          i_return =  ls_return                                " Parâmetro de retorno
      ).

      "Atualiza status da tabela da ALV.
      me->zif_integracao_comb~at_dados_abast-status_processamento = 'E'.

    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_comb~put_int_comb.

    r_if_integracao_comb = me.

    DATA: e_comboio TYPE TABLE OF zpmt0058.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_comb~get_json( IMPORTING e_json = data(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url_put(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_data  ).
  ENDMETHOD.


  METHOD zif_integracao_comb~regist_justif.


    DATA: w_zpmt0062 TYPE zpmt0062.


    w_zpmt0062 = VALUE #( id              = w_zpmt0058-movi_codigo
                          equnr           = |{ w_zpmt0058-veic_codigofrota ALPHA = IN }|
                          placa           = w_zpmt0058-veic_placa
                          frota           = |{ w_zpmt0058-veic_codigofrota ALPHA = IN }|
                          usuario         = sy-uname
                          odometro        = w_zpmt0058-movi_tot_veic_ant
                          odometro_atual  = w_zpmt0058-movi_totalizador_veiculo
                          msgv1           = i_line
                          datum           = sy-datum
                          uzeit           = sy-uzeit
                          ).


    MODIFY zpmt0062 FROM w_zpmt0062.
    COMMIT WORK.
  ENDMETHOD.


  METHOD zif_integracao_comb~set_dados_eqpto.

    DATA: zwerks    TYPE werks_d,
          zmatnr    TYPE matnr18,
          zequnr    TYPE equnr,
          zdate     TYPE sy-datum,
          ztime     TYPE sy-uzeit,
          zdeposito TYPE station_t.

    FREE: me->zif_integracao_comb~at_ponto_medicao, me->zif_integracao_comb~at_doc_med.
    CLEAR: me->zif_integracao_comb~at_dados_abast.
    CLEAR: me->zif_integracao_comb~at_material.
    CLEAR: me->zif_integracao_comb~at_deposito.
    CLEAR: me->zif_integracao_comb~at_centro.
    CLEAR: me->zif_integracao_comb~at_epto.
    CLEAR: me->zif_integracao_comb~at_date.
    CLEAR: me->zif_integracao_comb~at_hora.
    CLEAR: me->zif_integracao_comb~at_return.
    CLEAR: me->zif_integracao_comb~at_dados_abast-status_processamento.

    IF i_dados IS NOT INITIAL.

      zwerks      = i_dados-poco_centro.
      zmatnr      = |{ i_dados-prod_codigo_aux ALPHA = IN }|.

      IF i_dados-veic_frota_sap IS NOT INITIAL.
        zequnr      = |{ i_dados-veic_frota_sap ALPHA = IN }|. "|{ i_dados-veic_codigofrota ALPHA = IN }|.
      ELSE.
        zequnr      = |{ i_dados-veic_codigofrota ALPHA = IN }|. "|{ i_dados-veic_codigofrota ALPHA = IN }|.
      ENDIF.

      zdate       = i_dados-data_inicio.
      ztime       = i_dados-hora_inicio.
      zdeposito   = i_dados-poco_deposito.

      me->zif_integracao_comb~at_dados_abast = i_dados.
      me->zif_integracao_comb~at_material    = zmatnr.
      me->zif_integracao_comb~at_deposito    = zdeposito.
      me->zif_integracao_comb~at_centro      = zwerks.
      me->zif_integracao_comb~at_epto        = zequnr.
      me->zif_integracao_comb~at_date        = zdate.
      me->zif_integracao_comb~at_hora        = ztime.
    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_comb~set_ds_data.
    "Incluir Texto JSON para integração
    r_if_integracao_comb = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.
  ENDMETHOD.


  METHOD zif_integracao_comb~set_ds_url.
    r_if_integracao_comb = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ '7'
       AND servico EQ 'UN'. "@me->zif_integracao_comb~at_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'UN'
                            attr2 = me->zif_integracao_comb~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_integracao_comb~at_servico ).
    ENDIF.


    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~request_method'  value = 'GET' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type'     value = 'application/json; charset=UTF-8' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Token'            value = wa_webservice-senha ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato            =  'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token          = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url                = wa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             =  'GET'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   =  ''.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.


  ENDMETHOD.


  method ZIF_INTEGRACAO_COMB~SET_DS_URL_PUT.

     r_if_integracao_comb = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ '7'
       AND servico EQ 'UD'. "@me->zif_integracao_comb~at_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'UD'
                            attr2 = me->zif_integracao_comb~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_integracao_comb~at_servico ).
    ENDIF.


    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~request_method'  value = 'PUT' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type'     value = 'application/json; charset=UTF-8' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Token'            value = wa_webservice-senha ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato            =  'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token          = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url                = wa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             =  'PUT'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   =  ''.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
  endmethod.


  METHOD zif_integracao_comb~set_int_comb.
    r_if_integracao_comb = me.
  ENDMETHOD.


  METHOD zif_integracao_comb~set_log.

    DATA: lwa_zpmt0060 TYPE zpmt0060,
          lva_index    TYPE sy-tabix.


    lwa_zpmt0060-movi_codigo          =   i_dados-movi_codigo.
    lwa_zpmt0060-poco_codigo          =   i_dados-poco_codigo.
    lwa_zpmt0060-poco_descricao       =   i_dados-poco_descricao.
    lwa_zpmt0060-poco_deposito        =   i_dados-poco_deposito.
    lwa_zpmt0060-poco_codigo_destino  =   i_dados-poco_codigo_destino.
    lwa_zpmt0060-poco_centro          =   i_dados-poco_centro.
    lwa_zpmt0060-movi_datahora_ini    =   i_dados-data_inicio.
    lwa_zpmt0060-movi_datahora_fim    =   i_dados-data_fim.
    lwa_zpmt0060-veic_codigofrota     =   i_dados-veic_codigofrota.
    lwa_zpmt0060-usua_nome            =   sy-uname.
    lwa_zpmt0060-prod_codigo          =   i_dados-prod_codigo.
    lwa_zpmt0060-prod_display         =   i_dados-prod_display.
    lwa_zpmt0060-prod_tipo            =   i_dados-prod_tipo.
    lwa_zpmt0060-tipo_msg             =   i_return-type.
    lwa_zpmt0060-znumber              =   i_return-number.
    lwa_zpmt0060-message              =   i_return-message.

    MODIFY zpmt0060 FROM lwa_zpmt0060.
    COMMIT WORK.
    CLEAR:  lwa_zpmt0060.


    UPDATE zpmt0058 SET status_processamento = i_return-type
     WHERE movi_codigo = i_dados-movi_codigo.
    COMMIT WORK.


  ENDMETHOD.


  METHOD zif_integracao_comb~set_ponto_medicao.
    FREE: me->zif_integracao_comb~at_ponto_medicao.

    "Seleção pontos de medição veiculo.
    DATA: it_imptt  TYPE TABLE OF diimpt,
          ls_return TYPE bapiret2,
          wa_dimpt  TYPE diimpt,
          vcont     TYPE p DECIMALS 2,
          vtpponto  TYPE diimpt-atnam.

    FREE: me->zif_integracao_comb~at_ponto_medicao.


    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
      EXPORTING
        i_equnr   = me->zif_integracao_comb~at_epto
      TABLES
        et_diimpt = me->zif_integracao_comb~at_ponto_medicao.



*    IF me->zif_integracao_comb~at_ponto_medicao IS NOT INITIAL.
*      SORT me->zif_integracao_comb~at_ponto_medicao BY atnam.
*      IF me->zif_integracao_comb~at_material = '1010001127'. "Código ARLA.
*        DELETE me->zif_integracao_comb~at_ponto_medicao WHERE atnam NE 'FLUIDO_ARLA_32'.
*      ELSE.
*        DELETE me->zif_integracao_comb~at_ponto_medicao WHERE atnam EQ 'FLUIDO_ARLA_32'.
*      ENDIF.
*    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_comb~set_processa_consumo.

    DATA: ls_t001k       TYPE  t001k,
          ls_header      TYPE bapi2017_gm_head_01,
          ls_item        TYPE bapi2017_gm_item_create,
          lt_item        TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          lt_return      TYPE STANDARD TABLE OF bapiret2,
          lva_dt_ponto   TYPE sy-datum,
          lva_mater      TYPE bapi2017_gm_head_ret-mat_doc,
          lva_year       TYPE bapi2017_gm_head_ret-doc_year,
          ls_code        TYPE bapi2017_gm_code,
          lwa_zpmt0060   TYPE zpmt0060,
          lva_index      TYPE sy-tabix,
          lv_herst       TYPE zpmr0001-herst,
          lv_equnr       TYPE equi-equnr,
          lv_qtd_abastec TYPE dec_16_02_s.

    IF i_comboio IS NOT INITIAL.

      me->zif_integracao_comb~at_comboio = i_comboio.

      SORT me->zif_integracao_comb~at_comboio BY data_inicio hora_inicio veic_equip_sap prod_codigo.

      LOOP AT  me->zif_integracao_comb~at_comboio INTO DATA(lwa_comboio).
        lva_index = sy-tabix.
*** Inicio - Rubenilson - 14.01.24 - BUG163330

        lv_equnr = lwa_comboio-veic_codigofrota.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_equnr
          IMPORTING
            output = lv_equnr.

        SELECT SINGLE *
          FROM equi
          INTO @DATA(ls_equi)
          WHERE equnr = @lv_equnr.
        IF sy-subrc IS INITIAL.

          DATA(lo_pm_data_equipament) = NEW zcl_pm_data_equipament( ).

          lv_qtd_abastec = lwa_comboio-movi_volume.
          lo_pm_data_equipament->zif_pm_data_equipament~valida_capacidade_combustivel(

            EXPORTING
              i_cod_classe           = ls_equi-eqart
              i_fabricante           = ls_equi-herst
              i_modelo               = ls_equi-typbz
              i_qtd_abastec          = lv_qtd_abastec
            IMPORTING
              e_tq_comb              = DATA(lv_tq_comb)
              e_dentro_da_tolerancia = DATA(lv_dentro_tolerancia) ).

          IF lv_tq_comb IS INITIAL OR lv_tq_comb = 0.
            me->zif_integracao_comb~at_return = VALUE bapiret2( type = 'E' message  = |'Equipamento sem parâmetro na ZPM0017!'| number  = 1 ).

            me->zif_integracao_comb~set_log(
             EXPORTING
               i_dados  =  lwa_comboio " Informações integradas SAP x UNIDATA
               i_return = me->zif_integracao_comb~at_return    " Parâmetro de retorno
           ).

            CONTINUE.

          ELSE.

            IF lv_dentro_tolerancia IS INITIAL.

              me->zif_integracao_comb~at_return = VALUE bapiret2( type = 'E' message  = |'Quantidade de abastecimento maior que o cadastrado na ZPM0017'| number  = 1 ).

              me->zif_integracao_comb~set_log(
               EXPORTING
                 i_dados  =  lwa_comboio " Informações integradas SAP x UNIDATA
                 i_return = me->zif_integracao_comb~at_return    " Parâmetro de retorno
             ).

              CONTINUE.

            ENDIF.

          ENDIF.

        ENDIF.
*** Fim - Rubenilson - 14.01.24 - BUG163330

        me->zif_integracao_comb~set_dados_eqpto( i_dados = lwa_comboio ).
        me->zif_integracao_comb~check_periodo( ).
        me->zif_integracao_comb~check_veiculo( ).
        me->zif_integracao_comb~check_material( ).
        me->zif_integracao_comb~check_deposito( ).

        IF me->zif_integracao_comb~at_return IS NOT INITIAL.
          me->zif_integracao_comb~set_log(
            EXPORTING
              i_dados  =  lwa_comboio " Informações integradas SAP x UNIDATA
              i_return = me->zif_integracao_comb~at_return    " Parâmetro de retorno
          ).

          CONTINUE.
        ENDIF.

        IF me->zif_integracao_comb~at_eqtyp IS NOT INITIAL AND me->zif_integracao_comb~at_eqtyp NE 'M'.

          "Selecionar pontos de medição equipamento.
          me->zif_integracao_comb~set_ponto_medicao( ).

          "Processa documento posição do contador odometro / horimetro.
          me->zif_integracao_comb~proc_contador_odom_hom( ).

          "Processa baixa de estoque.
          IF me->zif_integracao_comb~at_doc_med IS NOT INITIAL.
            me->zif_integracao_comb~proc_baixa_estoque( ).
          ENDIF.

        ELSE.

          "Processa baixa de estoque.
          me->zif_integracao_comb~proc_baixa_estoque( ).

        ENDIF.


        lwa_comboio-status_processamento = me->zif_integracao_comb~at_dados_abast-status_processamento.
        MODIFY me->zif_integracao_comb~at_comboio FROM lwa_comboio INDEX lva_index.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD zif_integracao_comb~set_processa_transferencia.

    DATA: ls_t001k     TYPE  t001k,
          ls_header    TYPE bapi2017_gm_head_01,
          ls_item      TYPE bapi2017_gm_item_create,
          lt_item      TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          lt_return    TYPE STANDARD TABLE OF bapiret2,
          lva_dt_ponto TYPE sy-datum,
          lva_mater    TYPE bapi2017_gm_head_ret-mat_doc,
          lva_year     TYPE bapi2017_gm_head_ret-doc_year,
          ls_code      TYPE bapi2017_gm_code,
          lwa_zpmt0060 TYPE zpmt0060,
          lva_index    TYPE sy-tabix.

    free: me->zif_integracao_comb~at_comboio.
    IF i_comboio IS NOT INITIAL.

      me->zif_integracao_comb~at_comboio = i_comboio.

      LOOP AT  me->zif_integracao_comb~at_comboio INTO DATA(lwa_comboio).
        lva_index = sy-tabix.

        CALL FUNCTION 'AIP01_PLANT_DETERMINE'
          EXPORTING
            i_werks  = lwa_comboio-poco_centro
          IMPORTING
            es_t001k = ls_t001k
          EXCEPTIONS
            OTHERS   = 1.


        CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
          EXPORTING
            p_data_ent     = lwa_comboio-data_inicio
            p_bukrs        = ls_t001k-bukrs
            p_val_fi       = 'X'
            p_val_mm       = 'X'
          EXCEPTIONS
            data_fi_mm_nao = 1
            OTHERS         = 2.

        IF sy-subrc IS NOT INITIAL.

          lwa_zpmt0060-movi_codigo          =   lwa_comboio-movi_codigo.
          lwa_zpmt0060-poco_codigo          =   lwa_comboio-poco_codigo.
          lwa_zpmt0060-poco_descricao       =   lwa_comboio-poco_descricao.
          lwa_zpmt0060-poco_deposito        =   lwa_comboio-poco_deposito.
          lwa_zpmt0060-poco_codigo_destino  =   lwa_comboio-poco_codigo_destino.
          lwa_zpmt0060-poco_centro          =   lwa_comboio-poco_centro.
          lwa_zpmt0060-movi_datahora_ini    =   lwa_comboio-data_inicio.
          lwa_zpmt0060-movi_datahora_fim    =   lwa_comboio-data_fim.
          lwa_zpmt0060-veic_codigofrota     =   lwa_comboio-veic_codigofrota.
          lwa_zpmt0060-usua_nome            =   sy-uname.
          lwa_zpmt0060-prod_codigo          =   lwa_comboio-prod_codigo.
          lwa_zpmt0060-prod_display         =   lwa_comboio-prod_display.
          lwa_zpmt0060-prod_tipo            =   lwa_comboio-prod_tipo.
          lwa_zpmt0060-tipo_msg             = 'E'.
          CONCATENATE 'A data informada'  lwa_comboio-data_inicio  'esta em um período bloqueado para a empresa'
                        ls_t001k-bukrs INTO lwa_zpmt0060-message SEPARATED BY space .

          MODIFY zpmt0060 FROM lwa_zpmt0060.
          COMMIT WORK.
          CLEAR:  lwa_zpmt0060.

          UPDATE zpmt0058 SET status_processamento = 'E'
            WHERE movi_codigo = lwa_comboio-movi_codigo.


          CONTINUE.

        ELSE.

*---> 04/07/2023 - Migração S4 - EO
*          ls_item-material = |{ lwa_comboio-prod_codigo_aux  ALPHA = IN }|.
          ls_item-material = |{ lwa_comboio-prod_codigo_aux  ALPHA = IN WIDTH = 18 }|.
*---> 04/07/2023 - Migração S4 - EO

          ls_item-plant        = lwa_comboio-poco_centro.
          ls_item-move_plant   = lwa_comboio-poco_centro.
          ls_item-move_type    = '311'.
          ls_item-entry_qnt    = lwa_comboio-movi_volume.

          SELECT SINGLE storage
                FROM t370fld_stn
                  INTO ls_item-stge_loc
                WHERE storage EQ lwa_comboio-poco_deposito.

          IF ls_item-stge_loc IS INITIAL.

            lwa_zpmt0060-movi_codigo          =   lwa_comboio-movi_codigo.
            lwa_zpmt0060-poco_codigo          =   lwa_comboio-poco_codigo.
            lwa_zpmt0060-poco_descricao       =   lwa_comboio-poco_descricao.
            lwa_zpmt0060-poco_deposito        =   lwa_comboio-poco_deposito.
            lwa_zpmt0060-poco_codigo_destino  =   lwa_comboio-poco_codigo_destino.
            lwa_zpmt0060-poco_centro          =   lwa_comboio-poco_centro.
            lwa_zpmt0060-movi_datahora_ini    =   lwa_comboio-data_inicio.
            lwa_zpmt0060-movi_datahora_fim    =   lwa_comboio-data_fim.
            lwa_zpmt0060-veic_codigofrota     =   lwa_comboio-veic_codigofrota.
            lwa_zpmt0060-usua_nome            =   sy-uname.
            lwa_zpmt0060-prod_codigo          =   lwa_comboio-prod_codigo.
            lwa_zpmt0060-prod_display         =   lwa_comboio-prod_display.
            lwa_zpmt0060-prod_tipo            =   lwa_comboio-prod_tipo.
            lwa_zpmt0060-tipo_msg             =   'E'.
            lwa_zpmt0060-message              =   'O Tanque Origem e o Tanque Destino são obrigatórios para realizar a Transferência!'.

            MODIFY zpmt0060 FROM lwa_zpmt0060.
            COMMIT WORK.
            CLEAR:  lwa_zpmt0060.

            UPDATE zpmt0058 SET status_processamento = 'E'
              WHERE movi_codigo = lwa_comboio-movi_codigo.


            CONTINUE.


          ENDIF.

          SELECT SINGLE storage
                FROM t370fld_stn
                  INTO ls_item-move_stloc
                WHERE storage EQ lwa_comboio-poco_codigo_destino_aux.

          IF  ls_item-move_stloc IS INITIAL.

            lwa_zpmt0060-movi_codigo          =   lwa_comboio-movi_codigo.
            lwa_zpmt0060-poco_codigo          =   lwa_comboio-poco_codigo.
            lwa_zpmt0060-poco_descricao       =   lwa_comboio-poco_descricao.
            lwa_zpmt0060-poco_deposito        =   lwa_comboio-poco_deposito.
            lwa_zpmt0060-poco_codigo_destino  =   lwa_comboio-poco_codigo_destino.
            lwa_zpmt0060-poco_centro          =   lwa_comboio-poco_centro.
            lwa_zpmt0060-movi_datahora_ini    =   lwa_comboio-data_inicio.
            lwa_zpmt0060-movi_datahora_fim    =   lwa_comboio-data_fim.
            lwa_zpmt0060-veic_codigofrota     =   lwa_comboio-veic_codigofrota.
            lwa_zpmt0060-usua_nome            =   sy-uname.
            lwa_zpmt0060-prod_codigo          =   lwa_comboio-prod_codigo.
            lwa_zpmt0060-prod_display         =   lwa_comboio-prod_display.
            lwa_zpmt0060-prod_tipo            =   lwa_comboio-prod_tipo.
            lwa_zpmt0060-tipo_msg             =   'E'.
            lwa_zpmt0060-message              =   'O Tanque Origem e o Tanque Destino são obrigatórios para realizar a Transferência!'.

            MODIFY zpmt0060 FROM lwa_zpmt0060.
            COMMIT WORK.
            CLEAR:  lwa_zpmt0060.

            UPDATE zpmt0058 SET status_processamento = 'E'
              WHERE movi_codigo = lwa_comboio-movi_codigo.



            CONTINUE.
          ENDIF.

*          SELECT SINGLE vornr
*            FROM afvc AS a
*            INNER JOIN afko AS b ON a~aufpl = b~aufpl
*          INTO ls_item-activity
*            WHERE b~aufnr = lwa_comboio-movi_sig_ordem.
*          "ERRo EXIT

          APPEND ls_item TO lt_item.
          CLEAR ls_item.

          ls_code = '06'.
          lva_dt_ponto = lwa_comboio-data_inicio.
          ls_header-pstng_date = lva_dt_ponto.
          ls_header-doc_date   = lva_dt_ponto.

          "Executar BAPI de transferência.
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              goodsmvt_header  = ls_header
              goodsmvt_code    = ls_code
            IMPORTING
              materialdocument = lva_mater
              matdocumentyear  = lva_year
            TABLES
              goodsmvt_item    = lt_item
              return           = lt_return.

          CLEAR: ls_header, ls_code, lt_item[].

          IF lva_mater IS NOT INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            "lwa_comboio-doc_material = lva_mater.
            lwa_comboio-status_processamento = 'P'.

            lwa_zpmt0060-movi_codigo          =   lwa_comboio-movi_codigo.
            lwa_zpmt0060-poco_codigo          =   lwa_comboio-poco_codigo.
            lwa_zpmt0060-poco_descricao       =   lwa_comboio-poco_descricao.
            lwa_zpmt0060-poco_deposito        =   lwa_comboio-poco_deposito.
            lwa_zpmt0060-poco_codigo_destino  =   lwa_comboio-poco_codigo_destino.
            lwa_zpmt0060-poco_centro          =   lwa_comboio-poco_centro.
            lwa_zpmt0060-movi_datahora_ini    =   lwa_comboio-data_inicio.
            lwa_zpmt0060-movi_datahora_fim    =   lwa_comboio-data_fim.
            lwa_zpmt0060-veic_codigofrota     =   lwa_comboio-veic_codigofrota.
            lwa_zpmt0060-usua_nome            =   sy-uname.
            lwa_zpmt0060-prod_codigo          =   lwa_comboio-prod_codigo.
            lwa_zpmt0060-prod_display         =   lwa_comboio-prod_display.
            lwa_zpmt0060-prod_tipo            =   lwa_comboio-prod_tipo.
            lwa_zpmt0060-tipo_msg             =   'S'.

            CONCATENATE 'Documento' lva_mater 'Criado corretamente' INTO lwa_zpmt0060-message   SEPARATED BY space.

            MODIFY zpmt0060 FROM lwa_zpmt0060.
            COMMIT WORK.
            CLEAR:  lwa_zpmt0060.

            UPDATE zpmt0058 SET status_processamento = 'S'
              WHERE movi_codigo = lwa_comboio-movi_codigo.


          ELSE.
            lwa_comboio-status_processamento = 'E'.

            lwa_zpmt0060-movi_codigo          =   lwa_comboio-movi_codigo.
            lwa_zpmt0060-poco_codigo          =   lwa_comboio-poco_codigo.
            lwa_zpmt0060-poco_descricao       =   lwa_comboio-poco_descricao.
            lwa_zpmt0060-poco_deposito        =   lwa_comboio-poco_deposito.
            lwa_zpmt0060-poco_codigo_destino  =   lwa_comboio-poco_codigo_destino.
            lwa_zpmt0060-poco_centro          =   lwa_comboio-poco_centro.
            lwa_zpmt0060-movi_datahora_ini    =   lwa_comboio-data_inicio.
            lwa_zpmt0060-movi_datahora_fim    =   lwa_comboio-data_fim.
            lwa_zpmt0060-veic_codigofrota     =   lwa_comboio-veic_codigofrota.
            lwa_zpmt0060-usua_nome            =   sy-uname.
            lwa_zpmt0060-prod_codigo          =   lwa_comboio-prod_codigo.
            lwa_zpmt0060-prod_display         =   lwa_comboio-prod_display.
            lwa_zpmt0060-prod_tipo            =   lwa_comboio-prod_tipo.
            lwa_zpmt0060-tipo_msg             =   'E'.

            LOOP AT  lt_return INTO DATA(lw_return).
              CONCATENATE lw_return-number lw_return-id space space lw_return-message space INTO lwa_zpmt0060-message.
            ENDLOOP.

            MODIFY zpmt0060 FROM lwa_zpmt0060.
            COMMIT WORK.
            CLEAR:  lwa_zpmt0060.

            UPDATE zpmt0058 SET status_processamento = 'E'
              WHERE movi_codigo = lwa_comboio-movi_codigo.


            CLEAR: ls_header, ls_code, lt_item[].


          ENDIF.
        ENDIF.
        MODIFY me->zif_integracao_comb~at_comboio FROM lwa_comboio INDEX lva_index.
      ENDLOOP.
      DELETE me->zif_integracao_comb~at_comboio WHERE status_processamento = 'S'.
    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_comb~set_send_msg.
    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_comb = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.

*    DATA: lva_id_referencia TYPE zintegracao-id_referencia,
*          lva_id_integracao TYPE zintegracao-id_integracao.
*
*    lva_id_integracao =  c_integracao-id_integracao.
*    lva_id_referencia =  me->zif_integracao_tcot_contratos~at_id_referencia.
*
*    UPDATE zintegracao
*         SET id_referencia = lva_id_referencia
*           WHERE id_integracao        = lva_id_integracao
*           AND   id_interface         = '040'.
*    COMMIT WORK AND WAIT.


  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.
ENDCLASS.
