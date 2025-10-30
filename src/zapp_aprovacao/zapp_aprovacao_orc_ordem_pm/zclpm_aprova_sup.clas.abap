CLASS zclpm_aprova_sup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS aprovar
      IMPORTING
        !iv_ordem TYPE aufnr .
    METHODS reprovar
      IMPORTING
        !iv_ordem  TYPE aufnr
        !iv_motivo TYPE string .
    METHODS solicita
      IMPORTING
        !is_param          TYPE zi_popup_suplementa
      RETURNING
        VALUE(rt_reported) TYPE bapiret2_t .
    METHODS muda_valor
      IMPORTING
        !is_param          TYPE zi_popup_mudavalor
        !iv_ordem          TYPE aufnr
      RETURNING
        VALUE(rt_reported) TYPE bapiret2_t .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gt_reported TYPE bapiret2_t .

    METHODS add_msg
      IMPORTING
        !iv_id         TYPE symsgid DEFAULT 'ZPMMSG'
        !iv_type       TYPE bapi_mtype
        !iv_number     TYPE symsgno
        !iv_message_v1 TYPE symsgv OPTIONAL
        !iv_message_v2 TYPE symsgv OPTIONAL
        !iv_message_v3 TYPE symsgv OPTIONAL
        !iv_message_v4 TYPE symsgv OPTIONAL .
    METHODS ordem_status
      IMPORTING
        !iv_ordem        TYPE aufnr
      RETURNING
        VALUE(rv_status) TYPE abap_bool .
ENDCLASS.



CLASS zclpm_aprova_sup IMPLEMENTATION.


  METHOD aprovar.

    DATA: lv_prox_aprov TYPE string,
          lv_id         TYPE belnr_d.

    DATA: lt_return TYPE bapiret2_t.

    CALL FUNCTION 'ZPM_APROVAR_SUPLEMENTACAO'
      EXPORTING
        ordem        = iv_ordem
        usuario      = sy-uname
      IMPORTING
        e_prox_aprov = lv_prox_aprov
        id_orcamento = lv_id
      TABLES
        errors       = lt_return.

  ENDMETHOD.


  METHOD reprovar.

    CALL FUNCTION 'ZPM_REPROVAR_SUPLEMENTACAO'
      EXPORTING
        ordem  = iv_ordem
        app    = abap_true
        motivo = iv_motivo.
*      IMPORTING
*        observacao = ls_ordem-observacao.

  ENDMETHOD.


  METHOD solicita.

    DATA user_data    TYPE alm_me_user_data.
    DATA order_header TYPE alm_me_order_header.
    DATA user_profile TYPE alm_me_c010prf.

    DATA lv_error      TYPE boolean.
    DATA lv_aufnr      TYPE aufnr.
    DATA lv_valor      TYPE zpmr0002-valor_de.
    DATA lv_prox_aprov TYPE zpm_ordem_orc-prox_aprov.

    CONSTANTS: lc_liberado       TYPE c VALUE 'L'.

    IF is_param IS NOT INITIAL.

      lv_aufnr = |{ is_param-ordem ALPHA = IN }|.

      SELECT SINGLE *
        FROM aufk
        INTO @DATA(ls_aufk)
       WHERE aufnr = @lv_aufnr.

      IF sy-subrc = 0.

        "//Tabela de permissões p/ solicitação de suplemento;
        SELECT SINGLE *
          FROM zpmr0007
          INTO @DATA(ls_zpmr0007)
         WHERE werks = @ls_aufk-werks
           AND usnam = @sy-uname.

        "//Verifica se possuí permissão p/ o centro da ordem;
        IF ( ls_zpmr0007-werks NE ls_aufk-werks ).
          add_msg(
            iv_number = '015'
            iv_type   = 'E' ).
        ELSE.


          "Verificar se existe orçamento inicial aprovado para ordem.
          zcl_ordem_man=>m_check_orc_inic_ord(
            EXPORTING
              i_aufnr  = lv_aufnr    " Nº ordem
            IMPORTING
              i_retorn = DATA(i_retorn)  " Campo de texto do comprimento 1
              i_belnr  = DATA(i_belnr)   " Valor total em moeda de transação
          ).

          IF i_belnr IS INITIAL.
            add_msg(
              iv_number = '016'
              iv_type   = 'E' ).

          ELSE.


            SELECT * FROM zpmt0064
              INTO TABLE @DATA(it_zpmt0064)
              WHERE centro EQ @ls_aufk-werks
              AND   tipo   EQ @ls_aufk-auart.

            IF NOT sy-subrc IS INITIAL.
              add_msg(
                iv_number = '017'
                iv_type   = 'E' ).

            ELSEIF ( ls_aufk-phas1 NE 'X' ).
              add_msg(
                iv_number = '018'
                iv_type   = 'E' ).

            ELSEIF ( is_param-descricao IS INITIAL ).
              add_msg(
                iv_number = '019'
                iv_type   = 'E' ).

            ELSE.

              SELECT SINGLE *
                FROM zpmr0006
                INTO @DATA(ls_zpmr0006)
               WHERE aufnr  EQ @lv_aufnr
                 AND status NE @lc_liberado.

              IF sy-subrc <> 0.

                CLEAR ls_zpmr0006.

                "//Obter texto do editor;
                ls_zpmr0006-observacao = is_param-descricao.

                CALL FUNCTION 'ALM_ME_ORDER_GETDETAIL'
                  EXPORTING
                    orderid       = lv_aufnr
                    resource      = 'X'
                    userdata      = user_data
                    order_profile = user_profile
                  IMPORTING
                    order_header  = order_header
                  EXCEPTIONS
                    read_error    = 1.

                ls_zpmr0006-status         = 'P'.                   "(P)- Pendente
                ls_zpmr0006-aufnr          = lv_aufnr.              "Ordem
                ls_zpmr0006-solicitante    = sy-uname.              "Requerente
                ls_zpmr0006-vlr_estimado   = is_param-valor.        "Custo global estimado da ordem
                ls_zpmr0006-dt_solicitacao = sy-datum.              "Data da solicitação
                ls_zpmr0006-werks          = ls_aufk-werks.         "Centro
                ls_zpmr0006-equipment      = order_header-equipment.
                ls_zpmr0006-equipment_desc = order_header-equipment_desc.
                ls_zpmr0006-short_text     = order_header-short_text.
                ls_zpmr0006-object         = order_header-object_no.
                ls_zpmr0006-currency       = order_header-currency.

                INSERT zpmr0006 FROM ls_zpmr0006.
                CLEAR ls_zpmr0006.

                lv_valor = order_header-estimated_costs + is_param-valor.

                SELECT *
                  FROM zpmr0002
                  INTO TABLE @DATA(lt_zpmt0002)
                  WHERE centro_desp = @order_header-plant
                    AND valor_de   <= @lv_valor
                    AND nivel       = '0000000001'.

                IF sy-subrc = 0.

                  SORT lt_zpmt0002 BY nivel.

                  LOOP AT lt_zpmt0002 ASSIGNING FIELD-SYMBOL(<fs_aprov>).

                    IF sy-tabix = 1.

                      IF <fs_aprov>-usua_subst IS NOT INITIAL AND <fs_aprov>-data_lim >= sy-datum.
                        CONCATENATE <fs_aprov>-usua_subst ',' INTO lv_prox_aprov.
                      ELSE.
                        CONCATENATE <fs_aprov>-aprovador ',' INTO lv_prox_aprov.
                      ENDIF.

                    ELSE.

                      IF <fs_aprov>-usua_subst IS NOT INITIAL AND <fs_aprov>-data_lim >= sy-datum.
                        CONCATENATE <fs_aprov>-usua_subst ',' lv_prox_aprov INTO lv_prox_aprov.
                      ELSE.
                        CONCATENATE <fs_aprov>-aprovador ',' lv_prox_aprov INTO lv_prox_aprov.
                      ENDIF.

                    ENDIF.

                  ENDLOOP.

                  add_msg(
                    iv_number = '020'
                    iv_type   = 'S' ).

                ENDIF.

              ELSE.

                add_msg(
                  iv_number = '021'
                  iv_type   = 'E' ).

              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

      ELSE.

        add_msg(
          iv_number = '014'
          iv_type   = 'E' ).

      ENDIF.

    ENDIF.

    rt_reported = gt_reported.

  ENDMETHOD.


  METHOD add_msg.

    APPEND INITIAL LINE TO gt_reported ASSIGNING FIELD-SYMBOL(<fs_rep>).

    <fs_rep>-id         = iv_id        .
    <fs_rep>-type       = iv_type      .
    <fs_rep>-number     = iv_number    .
    <fs_rep>-message_v1 = iv_message_v1.
    <fs_rep>-message_v2 = iv_message_v2.
    <fs_rep>-message_v3 = iv_message_v3.
    <fs_rep>-message_v4 = iv_message_v4.

  ENDMETHOD.


  METHOD muda_valor.

    CONSTANTS: lc_rejeitado TYPE char1 VALUE 'R',
               lc_pendente  TYPE char1 VALUE 'P'.

    IF ordem_status( iv_ordem ) NE lc_rejeitado.

      add_msg(
        iv_number = '022'
        iv_type   = 'E' ).

    ELSE.

      UPDATE zpmr0006
         SET vlr_estimado = is_param-valor
             observacao   = is_param-descricao
             status       = lc_pendente
       WHERE aufnr  EQ iv_ordem
         AND status EQ lc_rejeitado.

      add_msg(
        iv_number = '023'
        iv_type   = 'S' ).

    ENDIF.

    rt_reported = gt_reported.

  ENDMETHOD.


  METHOD ordem_status.

    SELECT
      aufnr, belnr, status
      FROM zpmr0006
      INTO @DATA(ls_ordem)
      UP TO 1 ROWS
      WHERE aufnr = @iv_ordem.
    ENDSELECT.

    rv_status = ls_ordem-status.

  ENDMETHOD.
ENDCLASS.
