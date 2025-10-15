*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: CAMILA BRAND                                            &*
*& Data.....: 22/12/2021                                              &*
*& Descrição: Programa atualizar e salvar tabela   zpmt0058           &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT zpmr0075.


DATA: lwa_zpmt0058   TYPE zpmt0058,
      tl_zpmt0058    TYPE TABLE OF zpmt0058,
      rg_movi_codigo TYPE RANGE OF zpmt0058-movi_codigo.

DATA:
  lva_date      TYPE sy-datum,
  lva_days_i    TYPE i,
  lva_timestamp TYPE timestampl,
  lva_date_i    TYPE sy-datum,
  lva_time_i    TYPE sy-uzeit,
  lva_date_f    TYPE sy-datum,
  w_sts         TYPE timestamp,
  w_date1       TYPE d,
  w_time1       TYPE t,
  w_date2       TYPE d,
  w_time2       TYPE t.
CONSTANTS:
  lc_day_in_sec TYPE i VALUE 86400.


DATA: lv_timestamp    TYPE timestampl,
      lv_timsmsec     TYPE timestampl,
      lv_sec_i        TYPE i,
      vl_message(150) TYPE c,
      i_timestampl    TYPE timestampl,
      ev_time         TYPE syuzeit,
      ev_msec         TYPE num03.   " IV_TIMESTAMP stores milliseconds since January 1, 1970, 00:00:00 GMT

DATA: r_id_movi  TYPE RANGE OF zpmt0058-movi_codigo,
      t_zpmt0058 TYPE TABLE OF zpmt0058.

DATA: integ_comb TYPE REF TO zcl_integ_comb.
CREATE OBJECT integ_comb.
FREE: tl_zpmt0058.


START-OF-SELECTION.

  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF sy-batch EQ abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.


  vl_message = 'Aguarde, importando dados UNIDATA'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 99
      text       = vl_message.


  TRY .
      zcl_integ_comb=>zif_integracao_comb~get_instance(
        )->get_int_comb( IMPORTING e_comboio = DATA(e_comboio)   ).


***==================================================================================== Comentado para receber as informações com datas corridas da UNIDATA.
      "Check codigo ja importado e não salvar novamente.
*      IF e_comboio IS NOT INITIAL.
*        r_id_movi  =  VALUE #( FOR l IN  e_comboio   ( sign = 'I' option = 'EQ' low = l-movi_codigo ) ).
*        SELECT * FROM zpmt0058 INTO TABLE t_zpmt0058
*        WHERE movi_codigo IN r_id_movi.
*
*        IF t_zpmt0058 IS NOT INITIAL.
*          SORT e_comboio BY movi_codigo.
*          SORT t_zpmt0058 BY movi_codigo.
*          FREE: r_id_movi.
*          r_id_movi  =  VALUE #( FOR s IN  t_zpmt0058   ( sign = 'I' option = 'EQ' low = s-movi_codigo ) ).
*          DELETE e_comboio WHERE movi_codigo IN r_id_movi.
*        ENDIF.
*      ENDIF.

***=======================================================================================

      LOOP AT e_comboio ASSIGNING FIELD-SYMBOL(<lwa_comboio>).
        TRY .
            IF <lwa_comboio>-movi_volume > 0.
*              MOVE-CORRESPONDING lwa_comboio TO lwa_zpmt0058.

              lwa_zpmt0058-movi_codigo               =  <lwa_comboio>-movi_codigo               .
              lwa_zpmt0058-poco_codigo               =  <lwa_comboio>-poco_codigo               .
              lwa_zpmt0058-poco_descricao            =  <lwa_comboio>-poco_descricao            .
              lwa_zpmt0058-poco_codigo_aux           =  <lwa_comboio>-poco_codigo_aux           .
              lwa_zpmt0058-poco_deposito             =  <lwa_comboio>-poco_deposito             .
              lwa_zpmt0058-poco_centro               =  <lwa_comboio>-poco_centro               .
              lwa_zpmt0058-poco_cgc                  =  <lwa_comboio>-poco_cgc                  .
              lwa_zpmt0058-tafi_codigo               =  <lwa_comboio>-tafi_codigo               .
              lwa_zpmt0058-bico_numero               =  <lwa_comboio>-bico_numero               .
              lwa_zpmt0058-bico_codigo_aux           =  <lwa_comboio>-bico_codigo_aux           .
              lwa_zpmt0058-movi_datahora_inicio      =  <lwa_comboio>-movi_datahora_inicio      .
              lwa_zpmt0058-movi_datahora_fim         =  <lwa_comboio>-movi_datahora_fim         .
              lwa_zpmt0058-movi_duracao              =  <lwa_comboio>-movi_duracao              .
              lwa_zpmt0058-empr_codigo               =  <lwa_comboio>-empr_codigo               .
              lwa_zpmt0058-empr_descricao            =  <lwa_comboio>-empr_descricao            .
              lwa_zpmt0058-empr_desc_abrev           =  <lwa_comboio>-empr_desc_abrev           .
              lwa_zpmt0058-veic_codigofrota          =  <lwa_comboio>-veic_codigofrota          .
              lwa_zpmt0058-veic_placa                =  <lwa_comboio>-veic_placa                .
              lwa_zpmt0058-veic_tipo                 =  <lwa_comboio>-veic_tipo                 .
              lwa_zpmt0058-movi_encerrante_inicial   =  CONV #( <lwa_comboio>-movi_encerrante_inicial ).
              lwa_zpmt0058-movi_encerrante_final     =  CONV #( <lwa_comboio>-movi_encerrante_final ).
              lwa_zpmt0058-movi_volume               =  CONV #( <lwa_comboio>-movi_volume ).
              lwa_zpmt0058-movi_totalizador_veiculo  =  CONV #( <lwa_comboio>-movi_totalizador_veiculo ).
              lwa_zpmt0058-movi_tot_veic_ant         =  CONV #( <lwa_comboio>-movi_tot_veic_ant ).
              lwa_zpmt0058-movi_totalizador_rodado   =  CONV #( <lwa_comboio>-movi_totalizador_rodado   ).
              lwa_zpmt0058-veic_tipo_totalizador     =  CONV #( <lwa_comboio>-veic_tipo_totalizador     ).
              lwa_zpmt0058-move_consumo_medio        =  CONV #( <lwa_comboio>-move_consumo_medio        ).
              lwa_zpmt0058-movi_modo                 =  CONV #( <lwa_comboio>-movi_modo                 ).
              lwa_zpmt0058-movi_forma_id             =  CONV #( <lwa_comboio>-movi_forma_id             ).
              lwa_zpmt0058-usua_login                =  CONV #( <lwa_comboio>-usua_login                ).
              lwa_zpmt0058-usua_nome                 =  CONV #( <lwa_comboio>-usua_nome                 ).
              lwa_zpmt0058-prod_codigo               =  CONV #( <lwa_comboio>-prod_codigo               ).
              lwa_zpmt0058-prod_display              =  CONV #( <lwa_comboio>-prod_display              ).
              lwa_zpmt0058-prod_codigo_aux           =  CONV #( <lwa_comboio>-prod_codigo_aux           ).
              lwa_zpmt0058-prod_tipo                 =  CONV #( <lwa_comboio>-prod_tipo                 ).
              lwa_zpmt0058-cecu_codigo               =  CONV #( <lwa_comboio>-cecu_codigo               ).
              lwa_zpmt0058-cecu_descricao            =  CONV #( <lwa_comboio>-cecu_descricao            ).
              lwa_zpmt0058-movi_tipo_movimento       =  CONV #( <lwa_comboio>-movi_tipo_movimento       ).
              lwa_zpmt0058-movi_sig_ordem            =  CONV #( <lwa_comboio>-movi_sig_ordem            ).
              lwa_zpmt0058-movi_elemento_pep         =  CONV #( <lwa_comboio>-movi_elemento_pep         ).
              lwa_zpmt0058-movi_conta_razao          =  CONV #( <lwa_comboio>-movi_conta_razao          ).
              lwa_zpmt0058-cont_numero               =  CONV #( <lwa_comboio>-cont_numero               ).
              lwa_zpmt0058-movi_diagrama_rede        =  CONV #( <lwa_comboio>-movi_diagrama_rede        ).
              lwa_zpmt0058-movi_reserva              =  CONV #( <lwa_comboio>-movi_reserva              ).
              lwa_zpmt0058-moto_codigo               =  CONV #( <lwa_comboio>-moto_codigo               ).
              lwa_zpmt0058-moto_nome                 =  CONV #( <lwa_comboio>-moto_nome                 ).
              lwa_zpmt0058-vemo_codigo               =  CONV #( <lwa_comboio>-vemo_codigo               ).
              lwa_zpmt0058-vemo_descricao            =  CONV #( <lwa_comboio>-vemo_descricao            ).
              lwa_zpmt0058-vema_codigo               =  CONV #( <lwa_comboio>-vema_codigo               ).
              lwa_zpmt0058-vema_descricao            =  CONV #( <lwa_comboio>-vema_descricao            ).
              lwa_zpmt0058-gere_codigo               =  CONV #( <lwa_comboio>-gere_codigo               ).
              lwa_zpmt0058-gere_nome                 =  CONV #( <lwa_comboio>-gere_nome                 ).
              lwa_zpmt0058-veic_equip_sap            =  CONV #( <lwa_comboio>-veic_equip_sap            ).
              lwa_zpmt0058-veic_frota_sap            =  CONV #( <lwa_comboio>-veic_frota_sap            ).
              lwa_zpmt0058-veic_classe               =  CONV #( <lwa_comboio>-veic_classe               ).
              lwa_zpmt0058-movi_area                 =  CONV #( <lwa_comboio>-movi_area                 ).
              lwa_zpmt0058-movi_preco_unitario       =  CONV #( <lwa_comboio>-movi_preco_unitario       ).
              lwa_zpmt0058-movi_preco_total          =  CONV #( <lwa_comboio>-movi_preco_total          ).
              lwa_zpmt0058-empr_codigo_auxi          =  CONV #( <lwa_comboio>-empr_codigo_auxi          ).
              lwa_zpmt0058-movi_compartimento        =  CONV #( <lwa_comboio>-movi_compartimento        ).
              lwa_zpmt0058-comp_descricao            =  CONV #( <lwa_comboio>-comp_descricao            ).
              lwa_zpmt0058-movi_troca_lub            =  CONV #( <lwa_comboio>-movi_troca_lub            ).
              lwa_zpmt0058-movi_numero_os            =  CONV #( <lwa_comboio>-movi_numero_os            ).
              lwa_zpmt0058-cama_codigo               =  CONV #( <lwa_comboio>-cama_codigo               ).
              lwa_zpmt0058-cama_descricao            =  CONV #( <lwa_comboio>-cama_descricao            ).
              lwa_zpmt0058-movi_frasco               =  CONV #( <lwa_comboio>-movi_frasco               ).
              lwa_zpmt0058-gepo_latitude             =  CONV #( <lwa_comboio>-gepo_latitude             ).
              lwa_zpmt0058-gepo_longitude            =  CONV #( <lwa_comboio>-gepo_longitude            ).
              lwa_zpmt0058-gepo_altitude             =  CONV #( <lwa_comboio>-gepo_altitude             ).
              lwa_zpmt0058-gepo_dentro_cerca         =  CONV #( <lwa_comboio>-gepo_dentro_cerca         ).
              lwa_zpmt0058-movi_online               =  CONV #( <lwa_comboio>-movi_online               ).
              lwa_zpmt0058-oper_codigo               =  CONV #( <lwa_comboio>-oper_codigo               ).
              lwa_zpmt0058-oper_descricao            =  CONV #( <lwa_comboio>-oper_descricao            ).
              lwa_zpmt0058-movi_safra                =  CONV #( <lwa_comboio>-movi_safra                ).
              lwa_zpmt0058-safr_descricao            =  CONV #( <lwa_comboio>-safr_descricao            ).
              lwa_zpmt0058-poco_codigo_destino       =  CONV #( <lwa_comboio>-poco_codigo_destino       ).
              lwa_zpmt0058-poco_codigo_destino_aux   =  CONV #( <lwa_comboio>-poco_codigo_destino_aux   ).
              lwa_zpmt0058-status_processamento      =  CONV #( <lwa_comboio>-status_processamento      ).

            ELSE.
              CONTINUE.
            ENDIF.

            CLEAR: i_timestampl.
            IF lwa_zpmt0058-movi_datahora_inicio IS NOT INITIAL.
              i_timestampl = CONV #( lwa_zpmt0058-movi_datahora_inicio ).
              CALL FUNCTION 'Z_CONV_TIMESTAMP_MIL_TO_DATE'
                EXPORTING
                  i_date = i_timestampl
                IMPORTING
                  e_date = lwa_zpmt0058-data_inicio
                  e_time = lwa_zpmt0058-hora_inicio.
            ENDIF.


            IF lwa_zpmt0058-movi_datahora_fim IS NOT INITIAL.
              CLEAR: i_timestampl.
              i_timestampl = CONV #( lwa_zpmt0058-movi_datahora_fim ).
              CALL FUNCTION 'Z_CONV_TIMESTAMP_MIL_TO_DATE'
                EXPORTING
                  i_date = i_timestampl
                IMPORTING
                  e_date = lwa_zpmt0058-data_fim
                  e_time = lwa_zpmt0058-hora_fim.
            ENDIF.

            lwa_zpmt0058-dt_integracao = sy-datum.
            lwa_zpmt0058-hr_integracao = sy-uzeit.
            lwa_zpmt0058-usuario_integracao = sy-uname.
            <lwa_comboio>-integrado = abap_true.


            APPEND lwa_zpmt0058 TO tl_zpmt0058.
            CLEAR: lwa_zpmt0058.

          CATCH cx_sy_conversion_no_number INTO DATA(ws_conversion).
            CLEAR: lwa_zpmt0058.

        ENDTRY.

      ENDLOOP.

    CATCH zcx_integracao INTO DATA(ex_integra).
      ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    CATCH zcx_error INTO DATA(ex_error).    "  "
      ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

  IF tl_zpmt0058 IS NOT INITIAL.
    MODIFY zpmt0058 FROM TABLE tl_zpmt0058.
    COMMIT WORK.
  ENDIF.

  IF e_comboio IS NOT INITIAL.
    SORT e_comboio BY integrado.
    DELETE e_comboio WHERE integrado NE abap_true.
  ENDIF.

  "Retorno documento processados.
  IF e_comboio IS NOT INITIAL.

    TRY .
        FREE: integ_comb->zif_integracao_comb~at_combust.
        integ_comb->zif_integracao_comb~at_combust = e_comboio.
        zcl_integ_comb=>zif_integracao_comb~get_instance(
          )->put_int_comb( EXPORTING i_comboio = e_comboio ).

      CATCH zcx_integracao INTO ex_integra.
        ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO ex_error.    "  "
        ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDIF.
