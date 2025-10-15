FUNCTION-POOL zppgf001.                     "MESSAGE-ID ..

* INCLUDE LZPPGF001D...                      " Local class definition

TYPES   BEGIN OF ty_msg.
TYPES: type            TYPE bapi_mtype,
       cod             TYPE vbeln,
       lote_individual TYPE charg_d,   "FF - #134176
       message         TYPE bapi_msg.
TYPES END OF ty_msg.

TYPES   BEGIN OF ty_erro.
TYPES: lote    TYPE charg_d.
TYPES END OF ty_erro.

DATA: return            TYPE TABLE OF ty_msg,
      _return_batch     TYPE ty_msg,
      it_erros          TYPE TABLE OF ty_msg,
      l_return          TYPE TABLE OF bapiret2,
      t_return          TYPE TABLE OF bapireturn,
      _return           TYPE bapiret2,
      pedido            TYPE vbeln,
      data              TYPE sy-datum,
      time              TYPE sy-uzeit,
*      _SEQ     TYPE NUMC10,
      it_0011           TYPE TABLE OF zppt0011,
*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Início de Alteração
      it_0019_processar TYPE TABLE OF zppt0019,
      it_0019           TYPE TABLE OF zppt0019,
      it_0011_mod       TYPE TABLE OF zppt0011,
      it_0019_mod       TYPE TABLE OF zppt0019,
*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Fim de Alteração
      _order            TYPE char25,
      lv_qtd_invalido   TYPE c.

DATA: at_header TYPE bapi2017_gm_head_01,
      at_item   TYPE TABLE OF bapi2017_gm_item_create,
      _document TYPE bapi2017_gm_head_ret.

DATA: t_0020 TYPE TABLE OF zppt0020.
DATA: it_excecao TYPE TABLE OF ty_erro.
DATA: at_bloco TYPE zpmed005.

*data(obj_embalagem) = new zcl_pm_embalagens( ).
*data: obj_embalagem type ref to zcl_pm_embalagens.
*create object obj_embalagem.

CLASS zcl_emb_backup DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:

      get_vencimento
        IMPORTING
                  vl_matnr      TYPE matnr
                  werks         TYPE werks_d
                  lgort         TYPE lgort_d
                  vl_charg      TYPE zde_lote_forn
        RETURNING VALUE(return) TYPE vfdat,

      no_gaps
        CHANGING
                  input         TYPE string
        RETURNING VALUE(return) TYPE labst,

      processa_lotes
        IMPORTING
          acao  TYPE string
        CHANGING
          input TYPE zppt0015_t,

*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Início de Alteração
      atualizar_defensivo
        IMPORTING
          i_acao TYPE string,
      get_it_0019,
      fill_0019_mod
        IMPORTING
          i_acao   TYPE string
          i_w_0011 TYPE zppt0011,
*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Fim de Alteração

      get_pedido
        IMPORTING
                  matnr         TYPE matnr
                  werks         TYPE werks_d
                  lgort         TYPE lgort_d
                  charg         TYPE zde_lote_forn "FF  #142208
        RETURNING VALUE(return) TYPE vbeln,

      get_lote
        IMPORTING input         TYPE zde_lote_forn
                  matnr         TYPE matnr
                  werks         TYPE werks_d
                  lgort         TYPE lgort_d
        RETURNING VALUE(return) TYPE zde_lote_forn, "FF  #142208

      get_data
        RETURNING VALUE(return) TYPE sy-datum,

      get_time
        RETURNING VALUE(return) TYPE sy-uzeit,

      create_batch
        IMPORTING
                  material        TYPE mchb-matnr
                  centro          TYPE mchb-werks
                  deposito        TYPE mchb-lgort
                  lote            TYPE mchb-charg
                  dt_vencimento   TYPE zppt0011-vfdat
                  lote_fabricante TYPE zde_lote_forn
        EXPORTING error           TYPE char1,

      transfer_from_batch_to_batch
        IMPORTING
                  header    TYPE bapi2017_gm_head_01
                  items     TYPE bapi2017_gm_item_create_t
                  direction TYPE char1
        EXPORTING
                  document  TYPE bapi2017_gm_head_ret
        CHANGING  input     TYPE zppt0015,

      get_seq_zpp0011 RETURNING VALUE(seq) TYPE numc10,
      get_zppt0011
        IMPORTING input         TYPE charg_d
                  werks         TYPE werks_d OPTIONAL
        RETURNING VALUE(return) TYPE zppt0011,
      entrada IMPORTING acao  TYPE string
              CHANGING  input TYPE zppt0015,
      saida  IMPORTING acao  TYPE string
             CHANGING  input TYPE zppt0015,
      devolucao IMPORTING acao  TYPE string
                CHANGING  input TYPE zppt0015,
      transferencia IMPORTING acao  TYPE string
                    CHANGING  input TYPE zppt0015,

      "INICIO USER STORY 152607 / AOENNING
      transferencia_deposto IMPORTING acao  TYPE string
                            CHANGING  input TYPE zppt0015,

      check_lote    IMPORTING i_matnr  TYPE matnr
                              i_werks  TYPE werks_d
                              i_lgort  TYPE lgort_d
                              i_charg  TYPE charg_d
                    EXPORTING e_return TYPE char01,

      create_batch_lgort
        IMPORTING
                  material        TYPE mchb-matnr
                  centro          TYPE mchb-werks
                  deposito        TYPE mchb-lgort
                  lote            TYPE mchb-charg
                  dt_vencimento   TYPE zppt0011-vfdat
                  lote_fabricante TYPE zde_lote_forn
        EXPORTING error           TYPE char1,
      "FIM USER STORY 152607 / AOENNING


      recebimento IMPORTING acao  TYPE string
                  CHANGING  input TYPE zppt0015,
      e_transferencia CHANGING  input TYPE zppt0015,
      e_lote_fabricante CHANGING input TYPE zppt0015,
      validar_campos IMPORTING direction   TYPE char1
                     CHANGING  input       TYPE zppt0015
                     RETURNING VALUE(erro) TYPE char1,
      add_19 IMPORTING input TYPE zppet006,
      check_19 IMPORTING i_lote_fabricante TYPE zde_lote_forn
                         i_lote_individual TYPE charg_d
                         i_id_movimentacao TYPE zid_movimentacao
                         i_empresa         TYPE bukrs
                         i_centro          TYPE werks_d
                         i_acao            TYPE char50
               EXPORTING return            TYPE sy-subrc,
      update_19 IMPORTING i_lote_individual TYPE charg_d
                          i_empresa         TYPE bukrs
                          i_centro          TYPE werks_d
                          i_cendes          TYPE werks_d OPTIONAL
                          i_acao            TYPE char50,
      check_qtd IMPORTING embalagem TYPE zppet006,
      get_mchb IMPORTING input TYPE zppt0015 RETURNING VALUE(return) TYPE char1,
      set_bloco,
      get_bloco RETURNING VALUE(e_bloco) TYPE zpmed005.
ENDCLASS.

CLASS zcl_emb_backup IMPLEMENTATION.

  METHOD get_vencimento.
    SELECT SINGLE vfdat FROM mch1 INTO return  WHERE matnr EQ vl_matnr
                                                 AND charg EQ vl_charg.

    IF ( sy-subrc <> 0 ).
      SELECT SINGLE chargd FROM zppt0016 INTO @DATA(_chargd) WHERE matnr EQ @vl_matnr
                                                              AND werks EQ @werks
                                                              AND lgort EQ @lgort
                                                              AND zlicha EQ @vl_charg.
      IF ( _chargd IS NOT INITIAL ).
        SELECT SINGLE vfdat FROM mch1 INTO return  WHERE matnr EQ vl_matnr AND charg EQ _chargd.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD no_gaps.
    CONDENSE input NO-GAPS.
    return = input.
  ENDMETHOD.

  METHOD processa_lotes.

    DATA: sl_header   TYPE bapi2017_gm_head_01,
          tl_item     TYPE TABLE OF bapi2017_gm_item_create,
          vl_code     TYPE bapi2017_gm_code,
          vl_material TYPE bapi2017_gm_head_ret-mat_doc,
          vl_year     TYPE bapi2017_gm_head_ret-doc_year,
          tl_return   TYPE TABLE OF bapiret2.

    DATA: wa_0023 TYPE zppt0023.

    DATA: id_mov TYPE char30.

    DATA: reservation_header TYPE bapirkpfc,
          reserva            TYPE rsnum,
          t_return           TYPE TABLE OF bapireturn,
          reservation_items  TYPE TABLE OF bapiresbc.

    CLEAR sl_header.
    FREE: it_0011, t_0020.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<sl_trans>).

      IF line_exists( it_excecao[ lote = <sl_trans>-lote_individual ] ).
        <sl_trans>-msg = |A ação { acao } já foi executada para o Lote { <sl_trans>-lote_individual }!|.
        CONTINUE.
      ENDIF.

      DATA(_0011) = get_zppt0011( input = <sl_trans>-lote_individual ).

      <sl_trans>-lote_fabricante = |{ <sl_trans>-lote_fabricante CASE = UPPER }|.

      id_mov = <sl_trans>-id_movimentacao.

      CASE acao.
*       "// Entrada do Lote do Fabricante para ser fracionado em pequenos Lotes
        WHEN 'entrada'.
          CALL METHOD entrada( EXPORTING acao = acao CHANGING input = <sl_trans> ).

*       "// Saida dos Lotes fracionados para as Equipes
        WHEN 'saida'.
          CALL METHOD saida( EXPORTING acao = acao CHANGING input = <sl_trans> ).

*       "// Devolução da equipe dos Lotes fracionados não utilizado
        WHEN 'devolucao'.
          CALL METHOD devolucao( EXPORTING acao = acao CHANGING input = <sl_trans> ).

*       "// Transferencia dos Lotes Fracionados para outra filial
        WHEN 'transferencia' OR 'terceiros' OR 'fornecedor'.
          CALL METHOD transferencia( EXPORTING acao = acao CHANGING input = <sl_trans> ).

*       "// Confirmação do Recebimento da Transferencia dos Lotes Fracionados
        WHEN 'recebimento'.
          CALL METHOD recebimento( EXPORTING acao = acao CHANGING input = <sl_trans> ).

*       "// Editar a transferencia de um Lote Fracionado
        WHEN 'editar_transferencia'.
          CALL METHOD e_transferencia( CHANGING input = <sl_trans> ).

*       "// Editar um Lote fracionado para outro Lote fabricante
        WHEN 'editar_lote'.
*          CALL METHOD E_LOTE_FABRICANTE( <SL_TRANS> ).

          "INICIO USER STORY 152607 / AOENNING
        WHEN 'transferenciadeposito'.
          CALL METHOD transferencia_deposto( EXPORTING acao = acao CHANGING input = <sl_trans> ).
          "FIM USER STORY 152607 / AOENNING
      ENDCASE.

    ENDLOOP.

    CASE acao.

*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Início de Alteração
      WHEN 'entrada'.
        CALL METHOD atualizar_defensivo( acao ).

      WHEN 'devolucao'.
        CALL METHOD atualizar_defensivo( acao ).
*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Início de Alteração

      WHEN 'saida'.

        CHECK at_item IS NOT INITIAL.

        MODIFY zppt0020 FROM TABLE t_0020.
        COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024

        CALL METHOD transfer_from_batch_to_batch
          EXPORTING
            header    = at_header
            items     = at_item
            direction = 'S'
          IMPORTING
            document  = _document
          CHANGING
            input     = <sl_trans>.

        IF _document IS NOT INITIAL.

          LOOP AT at_item INTO DATA(wa_item).

            _0011 = get_zppt0011( input = wa_item-batch werks = wa_item-plant ).

            _0011-mblnr      = _document-mat_doc.
            _0011-mjahr      = _document-doc_year.
            _0011-umlgo      = wa_item-move_stloc.
            _0011-umcha      = wa_item-move_stloc.
            _0011-dlabs      = _0011-dlabs + wa_item-entry_qnt.
            _0011-nr_ordem   = _order.
            _0011-usnam      = sy-uname.
            _0011-data_atual = data.
            _0011-hora_atual = time.
            _0011-id_movimentacao = id_mov.

            MODIFY zppt0011 FROM _0011.
            COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024

            MOVE-CORRESPONDING _0011 TO wa_0023.
            wa_0023-dlabs = wa_item-entry_qnt.
            MODIFY zppt0023 FROM wa_0023.
            COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024

            CALL METHOD update_19
              EXPORTING
                i_lote_individual = _0011-charg
                i_empresa         = '0015'
                i_centro          = _0011-werks
                i_acao            = CONV #( acao ).

          ENDLOOP.

        ENDIF.

        DELETE zppt0020 FROM TABLE t_0020.
        COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024

    ENDCASE.

  ENDMETHOD.

*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Início de Alteração
  METHOD atualizar_defensivo.

    CLEAR: it_0011_mod,
           it_0019_mod.

*** Stefanini - IR206856 - 05/11/2024 - LAZAROSR - Início de Alteração
    IF it_0011 IS NOT INITIAL.
*** Stefanini - IR206856 - 05/11/2024 - LAZAROSR - Fim de Alteração

      SELECT matnr,
             werks,
             lgort,
             charg,
             mblnr,
             mjahr
        FROM mseg
        INTO TABLE @DATA(it_mseg)
        FOR ALL ENTRIES IN @it_0011
        WHERE matnr = @it_0011-matnr
          AND werks = @it_0011-werks
          AND lgort = @it_0011-lgort
          AND charg = @it_0011-charg
          AND mblnr = @it_0011-mblnr
          AND mjahr = @it_0011-mjahr.

      IF sy-subrc IS INITIAL.

        CALL METHOD get_it_0019( ).

        SORT it_mseg BY matnr werks lgort charg mblnr mjahr.

        LOOP AT it_0011 INTO DATA(wl_0011).

          READ TABLE it_mseg TRANSPORTING NO FIELDS
                             WITH KEY matnr = wl_0011-matnr
                                      werks = wl_0011-werks
                                      lgort = wl_0011-lgort
                                      charg = wl_0011-charg
                                      mblnr = wl_0011-mblnr
                                      mjahr = wl_0011-mjahr
                                              BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            APPEND wl_0011 TO it_0011_mod.
            CALL METHOD fill_0019_mod(
                i_acao   = i_acao
                i_w_0011 = wl_0011 ).

          ELSE.

            APPEND VALUE #(
                           type    = 'E'
                           lote_individual = wl_0011-charg
                           message = |'Não foi possível efetuar '| & |{ i_acao }| & |'. Favor, reenviar.'|
                          ) TO return.

          ENDIF.

        ENDLOOP.

        IF it_0011_mod IS NOT INITIAL.

          MODIFY zppt0011 FROM TABLE it_0011_mod.
          IF sy-subrc IS INITIAL.
            COMMIT WORK AND WAIT.
          ENDIF.

        ENDIF.

        IF it_0019_mod IS NOT INITIAL.

          MODIFY zppt0019 FROM TABLE it_0019_mod.
          IF sy-subrc IS INITIAL.
            COMMIT WORK AND WAIT.
          ENDIF.

        ENDIF.

      ENDIF.

*** Stefanini - IR206856 - 05/11/2024 - LAZAROSR - Início de Alteração
    ENDIF.
*** Stefanini - IR206856 - 05/11/2024 - LAZAROSR - Fim de Alteração

  ENDMETHOD.

  METHOD get_it_0019.

    CLEAR it_0019.

*** Stefanini - IR206856 - 05/11/2024 - LAZAROSR - Início de Alteração
    IF it_0019_processar IS NOT INITIAL.
*** Stefanini - IR206856 - 05/11/2024 - LAZAROSR - Fim de Alteração

      SELECT *
        FROM zppt0019
          INTO TABLE it_0019
          FOR ALL ENTRIES IN it_0019_processar
          WHERE lote_individual = it_0019_processar-lote_individual
            AND empresa         = it_0019_processar-empresa
            AND centro          = it_0019_processar-centro.
      IF sy-subrc IS INITIAL.

        SORT it_0019 BY lote_individual centro acao processado estorno.

      ENDIF.

*** Stefanini - IR206856 - 05/11/2024 - LAZAROSR - Início de Alteração
    ENDIF.
*** Stefanini - IR206856 - 05/11/2024 - LAZAROSR - Fim de Alteração

  ENDMETHOD.

  METHOD fill_0019_mod.

    DATA: lw_0019 TYPE zppt0019.

    CASE i_acao.
      WHEN 'devolucao' OR 'saida' OR 'transferencia' OR 'recebimento'.

        DATA(vstatus) = SWITCH zeacao( i_acao WHEN 'saida'         THEN 'devolucao'
                                              WHEN 'devolucao'     THEN 'saida'
                                              WHEN 'transferencia' THEN 'recebimento'
                                              WHEN 'recebimento'   THEN 'transferencia'
                                     ).

        DATA(werks_) = SWITCH werks_d( i_acao WHEN 'transferencia' THEN i_w_0011-r_werks
                                              WHEN 'recebimento'   THEN i_w_0011-werks
                                              ELSE i_w_0011-werks
                                     ).

        READ TABLE it_0019 INTO lw_0019
                            WITH KEY lote_individual = i_w_0011-charg
                                     centro          = werks_
                                     acao            = vstatus
                                     processado      = abap_true
                                     estorno         = abap_false.

        IF sy-subrc IS INITIAL.

          lw_0019-estorno = abap_true.
          APPEND lw_0019 TO it_0019_mod.

        ENDIF.

    ENDCASE.

    READ TABLE it_0019 INTO lw_0019
                        WITH KEY lote_individual = i_w_0011-charg
                                 centro          = i_w_0011-werks
                                 acao            = i_acao
                                 processado      = abap_false
                                 estorno         = abap_false.
    IF sy-subrc IS INITIAL.

      lw_0019-processado = abap_true.
      APPEND lw_0019 TO it_0019_mod.

    ENDIF.

  ENDMETHOD.
*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Início de Alteração

  METHOD get_pedido.

    SELECT SINGLE mblnr
      FROM mseg
      INTO @DATA(_mblnr)
      WHERE charg EQ @charg
      AND matnr EQ @matnr
      AND werks EQ @werks
      AND lgort EQ @lgort
      AND bwart EQ '101'.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE ebeln
        FROM ekbe
        INTO return
        WHERE belnr EQ _mblnr
        AND bewtp EQ 'E'
        AND bwart EQ '101'.

    ELSE.
      SELECT SINGLE umcha
        FROM mseg
        INTO @DATA(_umcha)
        WHERE charg EQ @charg
          AND matnr EQ @matnr
          AND werks EQ @werks
          AND lgort EQ @lgort
          AND bwart EQ '311' .

      IF ( _umcha IS NOT INITIAL ).
        SELECT SINGLE mblnr
          FROM mseg
          INTO _mblnr
          WHERE charg EQ _umcha
            AND matnr EQ matnr
            AND werks EQ werks
            AND lgort EQ lgort
            AND bwart EQ '101'.

        IF sy-subrc IS INITIAL.
          SELECT SINGLE ebeln
            FROM ekbe
            INTO return
            WHERE belnr EQ _mblnr
            AND bewtp EQ 'E'
            AND bwart EQ '101'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_lote.
    SELECT SINGLE * FROM zppt0016 INTO @DATA(wa_0016) WHERE zlicha EQ @input
                                                       AND matnr EQ @matnr
                                                       AND werks EQ @werks
                                                       AND lgort EQ @lgort.

    IF sy-subrc IS INITIAL.
      return = |{ wa_0016-chargd CASE = UPPER }|.
    ELSE.
      WRITE input TO return.
    ENDIF.

  ENDMETHOD.

  METHOD get_data.
    return = sy-datum.
  ENDMETHOD.

  METHOD get_time.
    return = sy-uzeit.
  ENDMETHOD.


  METHOD create_batch.

    DATA dt_venc   TYPE string.
    DATA new_batch TYPE TABLE OF mcha.

    DATA: lv_lote TYPE charg_d.                             "FF #14228
    lv_lote = lote_fabricante(10).

    CLEAR: _return_batch, error.

    SELECT SINGLE *
      FROM mch1
      INTO @DATA(wa_mch1)
      WHERE charg EQ @lv_lote                               "FF #142208
        AND matnr EQ @material. "Ajuste BUG SOLTO 145494 / IR187612 / AOENNING.

    IF sy-subrc IS NOT INITIAL.
      _return_batch = VALUE #(

                          type = 'E'
                          message = |Lote Fabricante { lote_fabricante } não encontrado!|
                          lote_individual = lote            "FF #142208

                          ).

      APPEND CORRESPONDING #( _return_batch ) TO return.

      error = abap_true.
      EXIT.
    ENDIF.

    TRY.
        cl_abap_datfm=>conv_date_int_to_ext( EXPORTING im_datint = wa_mch1-vfdat IMPORTING ex_datext = dt_venc ).
      CATCH cx_abap_datfm_format_unknown.
    ENDTRY.

    DATA(_header)
      = VALUE mcha(
                    matnr = wa_mch1-matnr
                    werks = centro
                    charg = lote
                    hsdat = wa_mch1-hsdat
                    vfdat = wa_mch1-vfdat
                  ).

    DATA(_characteristics)
      = VALUE clbatch_t(
                         ( atnam = 'ZDEFENSIVO_DT_VALIDADE'     atwtb = dt_venc )
                         ( atnam = 'ZDEFENSIVO_LOTE_FABRICANTE' atwtb = lote_fabricante )
                       ).

    CALL FUNCTION 'VB_CREATE_BATCH'
      EXPORTING
        ymcha                        = _header
        new_lgort                    = deposito
        kzcla                        = '2'
        class                        = 'ZDEFENSIVO'
        no_cfc_calls                 = 'X'
      IMPORTING
        ymcha                        = _header
      TABLES
        char_of_batch                = _characteristics
        new_batch                    = new_batch
        return                       = l_return
      EXCEPTIONS
        no_material                  = 1
        no_batch                     = 2
        no_plant                     = 3
        material_not_found           = 4
        plant_not_found              = 5
        stoloc_not_found             = 6
        lock_on_material             = 7
        lock_on_plant                = 8
        lock_on_batch                = 9
        lock_system_error            = 10
        no_authority                 = 11
        batch_exist                  = 12
        stoloc_exist                 = 13
        illegal_batch_number         = 14
        no_batch_handling            = 15
        no_valuation_area            = 16
        valuation_type_not_found     = 17
        no_valuation_found           = 18
        error_automatic_batch_number = 19
        cancelled                    = 20
        wrong_status                 = 21
        interval_not_found           = 22
        number_range_not_extern      = 23
        object_not_found             = 24
        error_check_batch_number     = 25
        no_external_number           = 26
        no_customer_number           = 27
        no_class                     = 28
        error_in_classification      = 29
        inconsistency_in_key         = 30
        region_of_origin_not_found   = 31
        country_of_origin_not_found  = 32
        OTHERS                       = 33.

    IF sy-subrc IS NOT INITIAL AND sy-subrc NE 12.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = _return.

      APPEND CORRESPONDING #( _return ) TO return.

      error = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD create_batch_lgort.

    DATA dt_venc   TYPE string.
    DATA new_batch TYPE TABLE OF mcha.

    CLEAR: _return_batch, error.

    "Chega data de vencimento.
    IF dt_vencimento IS INITIAL.
      _return_batch = VALUE #(

                          type = 'E'
                          message = |Informe a data de vencimento do lote!|
                          lote_individual = lote            "FF #142208

                          ).

      APPEND CORRESPONDING #( _return_batch ) TO return.

      error = abap_true.
      EXIT.
    ENDIF.

    TRY.
        cl_abap_datfm=>conv_date_int_to_ext( EXPORTING im_datint = dt_vencimento IMPORTING ex_datext = dt_venc ).
      CATCH cx_abap_datfm_format_unknown.
    ENDTRY.

    DATA(_header)
      = VALUE mcha(
                    matnr = material
                    werks = centro
                    charg = lote
*                      hsdat = ''
                    vfdat = dt_vencimento
                  ).

    DATA(_characteristics)
      = VALUE clbatch_t(
                         ( atnam = 'ZDEFENSIVO_DT_VALIDADE'     atwtb = dt_vencimento )
                         ( atnam = 'ZDEFENSIVO_LOTE_FABRICANTE' atwtb = lote_fabricante )
                       ).

    CALL FUNCTION 'VB_CREATE_BATCH'
      EXPORTING
        ymcha                        = _header
        new_lgort                    = deposito
        kzcla                        = '2'
        class                        = 'ZDEFENSIVO'
        no_cfc_calls                 = 'X'
      IMPORTING
        ymcha                        = _header
      TABLES
        char_of_batch                = _characteristics
        new_batch                    = new_batch
        return                       = l_return
      EXCEPTIONS
        no_material                  = 1
        no_batch                     = 2
        no_plant                     = 3
        material_not_found           = 4
        plant_not_found              = 5
        stoloc_not_found             = 6
        lock_on_material             = 7
        lock_on_plant                = 8
        lock_on_batch                = 9
        lock_system_error            = 10
        no_authority                 = 11
        batch_exist                  = 12
        stoloc_exist                 = 13
        illegal_batch_number         = 14
        no_batch_handling            = 15
        no_valuation_area            = 16
        valuation_type_not_found     = 17
        no_valuation_found           = 18
        error_automatic_batch_number = 19
        cancelled                    = 20
        wrong_status                 = 21
        interval_not_found           = 22
        number_range_not_extern      = 23
        object_not_found             = 24
        error_check_batch_number     = 25
        no_external_number           = 26
        no_customer_number           = 27
        no_class                     = 28
        error_in_classification      = 29
        inconsistency_in_key         = 30
        region_of_origin_not_found   = 31
        country_of_origin_not_found  = 32
        OTHERS                       = 33.

    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ELSE.
*      if sy-subrc is not initial and sy-subrc ne 12.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = _return.

      APPEND CORRESPONDING #( _return ) TO return.

      error = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD transfer_from_batch_to_batch.

    DATA: lo_exc_root TYPE REF TO cx_root,
          lw_header   TYPE bapi2017_gm_head_01,
          lt_items    TYPE bapi2017_gm_item_create_t,
          lw_document TYPE bapi2017_gm_head_ret,
          lt_return   TYPE TABLE OF bapiret2.

    FREE: l_return, _return.

    lw_header = header.
    lt_items  = items.
    lw_document = document.

    TRY.
        "Campos de Material tratados. Pseudo comentário adicionado.      " >> ---> S4 Migration - 07/07/2023 - RZ
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'   "#EC CI_USAGE_OK[2438131] " >> ---> S4 Migration - 07/07/2023 - RZ
          EXPORTING
            goodsmvt_header  = lw_header
            goodsmvt_code    = '06'
          IMPORTING
            materialdocument = lw_document-mat_doc
            matdocumentyear  = lw_document-doc_year
          TABLES
            goodsmvt_item    = lt_items
            return           = lt_return.

        IF ( lw_document-mat_doc IS NOT INITIAL ).

          document = lw_document.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          _return = VALUE #(
                              type = 'S'
                              message = SWITCH #( direction
                                                     WHEN 'E' THEN |Lote { lt_items[ 1 ]-move_batch } Criado com Sucesso!|
                                                     WHEN 'D' THEN |Devolução do Lote { lt_items[ 1 ]-move_batch } Realizada com Sucesso!|
                                                     WHEN 'T' THEN |Transferência do Lote { lt_items[ 1 ]-batch } Realizada com Sucesso!|
                                                     WHEN 'F' THEN |Lote { lt_items[ 1 ]-move_batch } Movido com Sucesso!|
                                                     WHEN 'S' THEN |Saida da Ordem { _order } Realizada com Sucesso!|
                                                     WHEN 'R' THEN |Recebimento do { lt_items[ 1 ]-move_batch } Realizada com Sucesso!|
                                                )
                            ).
        ELSE.

          IF direction  EQ 'S'.
            LOOP AT lt_return INTO DATA(ls).
              APPEND VALUE #(
                             type    = ls-type
                             lote_individual = input-lote_individual "FF - #134176
                             message = ls-message
                            ) TO return.
            ENDLOOP.
            _return-message = 'Ocorreu um Erro na Saida!'.
          ELSE.

            CALL FUNCTION 'BALW_BAPIRETURN_GET2'
              EXPORTING
                type   = sy-msgty
                cl     = sy-msgid
                number = sy-msgno
                par1   = sy-msgv1
                par2   = sy-msgv2
                par3   = sy-msgv3
                par4   = sy-msgv4
              IMPORTING
                return = _return.
          ENDIF.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        ENDIF.
      CATCH cx_root INTO lo_exc_root.
        _return = VALUE #( type = 'E' message = |Lote { lt_items[ 1 ]-move_batch }: { lo_exc_root->get_text( ) }| ).
    ENDTRY.

    input-msg = _return-message.
    APPEND VALUE #(
                    type    = COND #( WHEN _return-type NE 'S' THEN 'E' ELSE _return-type )
                    cod     = lw_document-mat_doc
                    lote_individual = input-lote_individual
*                    lote_individual = VALUE #( lt_items[ 1 ]-move_batch ) "FF - #134176
                    message = _return-message
                  ) TO return.

  ENDMETHOD.

  METHOD get_seq_zpp0011.
  ENDMETHOD.

  METHOD get_zppt0011.

    IF input IS NOT INITIAL AND werks IS INITIAL.
      SELECT SINGLE * FROM zppt0011 INTO return WHERE charg EQ input.
    ENDIF.

    IF input IS NOT INITIAL AND werks IS NOT INITIAL.
      SELECT SINGLE * FROM zppt0011 INTO return WHERE charg EQ input AND werks = werks.
    ENDIF.

  ENDMETHOD.

  METHOD entrada.

    CHECK validar_campos( EXPORTING direction = 'E' CHANGING input = input ) IS INITIAL.

*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Início de Alteração
    CLEAR _document.
*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Fim de Alteração

    DATA(lote_fb) = zcl_emb_backup=>get_lote( input = input-lote_fabricante
                                       matnr = input-material
                                       werks = input-centro
                                       lgort = input-deposito_saida
                                       ).

    CALL METHOD create_batch
      EXPORTING
        material        = input-material
        centro          = input-centro
        deposito        = input-deposito_saida
        lote            = input-lote_individual
        dt_vencimento   = input-data
        lote_fabricante = CONV #( lote_fb ) "// Lote Fabricante
      IMPORTING
        error           = DATA(_error).

    IF _error IS INITIAL.

      IF zcl_emb_backup=>get_mchb( input ) IS INITIAL.

        at_header = VALUE #(
                              pstng_date  = sy-datum
                              doc_date    = input-dt_registro
                              header_txt  = |{ acao } do { input-lote_individual }|
                            ).

        at_item = VALUE #( (
                              move_type  = input-tipo_movimento
                              plant      = input-centro
*                              material   = input-material " >> ---> S4 Migration - 07/07/2023 - RZ
                              entry_qnt  = input-quantidade

                              stge_loc   = input-deposito_saida
                              batch      = lote_fb "// Lote Fabricante

                              move_stloc = input-deposito_saida
                              move_batch = input-lote_individual
                         ) ).

* ---> S4 Migration - 07/07/2023 - RZ - Inicio
        READ TABLE at_item ASSIGNING FIELD-SYMBOL(<fs_item_aux>) INDEX 1.
        IF sy-subrc EQ 0.

          DATA(v_len) = strlen( input-material ).

          IF v_len > 18.
            <fs_item_aux>-material_long  =   input-material.
          ELSE.
            <fs_item_aux>-material       =   input-material.
          ENDIF.
        ENDIF.

* <--- S4 Migration - 07/07/2023 - RZ - Fim

        CALL METHOD transfer_from_batch_to_batch
          EXPORTING
            header    = at_header
            items     = at_item
            direction = 'E'
          IMPORTING
            document  = _document
          CHANGING
            input     = input.

        IF _document IS NOT INITIAL.

*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Início de Alteração
*          it_0011 = VALUE #( (
*                                ebeln = zcl_emb_backup=>get_pedido( EXPORTING
*                                                                      matnr = input-material
*                                                                      werks = input-centro
*                                                                      lgort = input-deposito_saida
*                                                                      charg = zcl_emb_backup=>get_lote( input = input-lote_fabricante
*                                                                                                 matnr = input-material
*                                                                                                 werks = input-centro
*                                                                                                 lgort = input-deposito_saida )
*                                                            )
*                                matnr = input-material
*                                werks = input-centro
*                                lgort = input-deposito_saida
*                                charg = input-lote_individual
*                                mblnr = _document-mat_doc
*                                mjahr = _document-doc_year
*                                clabs = input-quantidade
*                                vfdat = zcl_emb_backup=>get_vencimento( vl_matnr = input-material werks = input-centro lgort = input-deposito_saida vl_charg = input-lote_fabricante )
*                                lfabr = input-lote_fabricante
*                                usnam = sy-uname
*                                data_atual = data
*                                hora_atual = time
*                                id_movimentacao = input-id_movimentacao
*                            ) ).


*          MODIFY zppt0011 FROM TABLE it_0011.
*          COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024
*
*          CALL METHOD update_19
*            EXPORTING
*              i_lote_individual = input-lote_individual
*              i_empresa         = input-empresa
*              i_centro          = input-centro
*              i_acao            = CONV #( acao ).

          APPEND VALUE #(
                                ebeln = zcl_emb_backup=>get_pedido( EXPORTING
                                                                      matnr = input-material
                                                                      werks = input-centro
                                                                      lgort = input-deposito_saida
                                                                      charg = zcl_emb_backup=>get_lote( input = input-lote_fabricante
                                                                                                 matnr = input-material
                                                                                                 werks = input-centro
                                                                                                 lgort = input-deposito_saida )
                                                            )
                                matnr = input-material
                                werks = input-centro
                                lgort = input-deposito_saida
                                charg = input-lote_individual
                                mblnr = _document-mat_doc
                                mjahr = _document-doc_year
                                clabs = input-quantidade
                                vfdat = zcl_emb_backup=>get_vencimento( vl_matnr = input-material werks = input-centro lgort = input-deposito_saida vl_charg = input-lote_fabricante )
                                lfabr = input-lote_fabricante
                                usnam = sy-uname
                                data_atual = data
                                hora_atual = time
                                id_movimentacao = input-id_movimentacao
                            ) TO it_0011.

          APPEND VALUE #( lote_individual = input-lote_individual
                          empresa         = input-empresa
                          centro          = input-centro
                          acao            = acao ) TO it_0019_processar.
*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Fim de Alteração

        ENDIF.
      ELSE.
        APPEND VALUE #( type = 'E' message = 'Lote Individual já foi movimentado!' ) TO it_erros.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD saida.

    CHECK validar_campos( EXPORTING direction = 'S' CHANGING input = input  ) IS INITIAL.

    at_header = VALUE #(
                          pstng_date  = sy-datum
                          doc_date    = input-dt_registro
                          header_txt  = |{ acao }O.S{ input-nr_ordem }|
                       ).

* ---> S4 Migration - 07/07/2023 - RZ - Inicio
*    APPEND VALUE #(
*                     move_type  = input-tipo_movimento
*                     plant      = input-centro
*                     material   = input-material
*
*                     batch      = input-lote_individual
*                     entry_qnt  = input-quantidade
*                     stge_loc   = input-deposito_saida
*
*                     move_batch = input-deposito_receptor
*                     move_stloc = input-deposito_receptor
*
*                  ) TO at_item.

    APPEND INITIAL LINE TO at_item ASSIGNING FIELD-SYMBOL(<fs_item_aux>).
    <fs_item_aux> = VALUE #(
    move_type  = input-tipo_movimento
       plant      = input-centro
*       material   = input-material
       batch      = input-lote_individual
       entry_qnt  = input-quantidade
       stge_loc   = input-deposito_saida

       move_batch = input-deposito_receptor
       move_stloc = input-deposito_receptor

    ) .

    DATA(v_len) = strlen( input-material ).

    IF v_len > 18.
      <fs_item_aux>-material_long  =   input-material.
    ELSE.
      <fs_item_aux>-material       =   input-material.
    ENDIF.


* <--- S4 Migration - 07/07/2023 - RZ - Fim

    _order = input-nr_ordem.

    APPEND VALUE #(
                    matnr = input-material
                    werks = input-centro
                    lgort = input-deposito_saida
                    charg = input-lote_individual
                  ) TO t_0020.

  ENDMETHOD.

  METHOD devolucao.

    CHECK validar_campos( EXPORTING direction = 'D' CHANGING input = input  ) IS INITIAL.

*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Início de Alteração
    CLEAR _document.
*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Fim de Alteração

    at_header = VALUE #(
                          pstng_date  = sy-datum
                          doc_date    = input-dt_registro
                          header_txt  = |{ acao } do { input-lote_individual }|
                       ).

    at_item = VALUE #( (
                          move_type  = input-tipo_movimento
                          plant      = input-centro

                          material   = input-material
                          entry_qnt  = input-quantidade

                          stge_loc   = input-deposito_saida
                          batch      = input-deposito_saida

                          move_stloc = input-deposito_receptor
                          move_batch = input-lote_individual

                     ) ).
* ---> S4 Migration - 07/07/2023 - RZ - Inicio
    READ TABLE at_item ASSIGNING FIELD-SYMBOL(<fs_item_aux>) INDEX 1.
    IF sy-subrc EQ 0.
      DATA(v_len) = strlen( input-material ).

      IF v_len > 18.
        <fs_item_aux>-material_long  =   input-material.
      ELSE.
        <fs_item_aux>-material       =   input-material.
      ENDIF.
    ENDIF.

* <--- S4 Migration - 07/07/2023 - RZ - Fim

    CALL METHOD transfer_from_batch_to_batch
      EXPORTING
        header    = at_header
        items     = at_item
        direction = 'D'
      IMPORTING
        document  = _document
      CHANGING
        input     = input.

    IF _document IS NOT INITIAL.

      DATA(_0011) = get_zppt0011( input-lote_individual ).


      _0011-mblnr = _document-mat_doc.
      _0011-mjahr = _document-doc_year.
      _0011-dlabs = _0011-dlabs - input-quantidade.

      _0011-umlgo = ''.
      _0011-umcha = ''.

      _0011-usnam = sy-uname.
      _0011-data_atual = data.
      _0011-hora_atual = time.
      _0011-id_movimentacao = input-id_movimentacao.

*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Início de Alteração
      APPEND _0011 TO it_0011.

      APPEND VALUE #( lote_individual  = input-lote_individual
                      empresa          = input-empresa
                      centro           = input-centro
                      acao             = acao ) TO it_0019_processar.

*      MODIFY zppt0011 FROM _0011.
*      COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024
*
*      CALL METHOD update_19
*        EXPORTING
*          i_lote_individual = input-lote_individual
*          i_empresa         = input-empresa
*          i_centro          = input-centro
*          i_acao            = CONV #( acao ).
*** Stefanini - IR204386 - 21/10/2024 - LAZAROSR - Fim de Alteração

    ENDIF.

  ENDMETHOD.

  METHOD transferencia.

    CHECK validar_campos( EXPORTING direction = 'T' CHANGING input = input  ) IS INITIAL.

    at_header = VALUE #(
                          pstng_date  = sy-datum
                          doc_date    = input-dt_registro
                          header_txt  = |{ acao(5) }. do { input-lote_individual }|
                       ).

    at_item = VALUE #( (
                          move_type  = input-tipo_movimento
                          plant      = input-centro
                          material   = input-material
                          entry_qnt  = input-quantidade

                          stge_loc   = input-deposito_saida
                          batch      = input-lote_individual

                          move_stloc = input-deposito_saida
                          move_batch = SWITCH #( input-deposito_receptor WHEN 'FORN' THEN 'FORNECEDOR'
                                                                         WHEN 'TERC' THEN 'TERCEIROS'
                                                                         ELSE sy-datum(4) )
                     ) ).
* ---> S4 Migration - 07/07/2023 - RZ - Inicio
    READ TABLE at_item ASSIGNING FIELD-SYMBOL(<fs_item_aux>) INDEX 1.
    IF sy-subrc EQ 0.
      DATA(v_len) = strlen( input-material ).

      IF v_len > 18.
        <fs_item_aux>-material_long  =   input-material.
      ELSE.
        <fs_item_aux>-material       =   input-material.
      ENDIF.
    ENDIF.

* <--- S4 Migration - 07/07/2023 - RZ - Fim
    CLEAR _document.
    CALL METHOD transfer_from_batch_to_batch
      EXPORTING
        header    = at_header
        items     = at_item
        direction = 'T'
      IMPORTING
        document  = _document
      CHANGING
        input     = input.

    IF _document-mat_doc IS NOT INITIAL.

      DATA(_0011) = get_zppt0011( input-lote_individual ).

      _0011-mblnr      = _document-mat_doc.
      _0011-mjahr      = _document-doc_year.
      _0011-r_werks    = SWITCH #( input-deposito_receptor WHEN 'FORN' OR 'TERC' THEN ''
                                                           ELSE input-centro_receptor ).
      _0011-umlgo      = SWITCH #( input-deposito_receptor WHEN 'FORN' OR 'TERC' THEN ''
                                                           ELSE input-deposito_receptor ).
      _0011-umcha      = SWITCH #( input-deposito_receptor WHEN 'FORN' THEN 'FORNECEDOR'
                                                           WHEN 'TERC' THEN 'TERCEIROS'
                                                           ELSE sy-datum(4) ).

      _0011-usnam = sy-uname.
      _0011-data_atual = data.
      _0011-hora_atual = time.
      _0011-id_movimentacao = input-id_movimentacao.

      MODIFY zppt0011 FROM _0011.
      COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024

      CALL METHOD update_19
        EXPORTING
          i_lote_individual = input-lote_individual
          i_empresa         = input-empresa
          i_centro          = input-centro
          i_cendes          = input-centro_receptor
          i_acao            = CONV #( acao ).

    ENDIF.

  ENDMETHOD.



  "INICIO USER STORY 152607 / AOENNING
  METHOD transferencia_deposto.

    DATA: zcheck_lote TYPE char01.

    CALL METHOD check_lote
      EXPORTING
        i_matnr  = CONV #( input-material )
        i_werks  = CONV #( input-centro )
        i_lgort  = CONV #( input-deposito_receptor )
        i_charg  = CONV #( input-lote_individual )
      IMPORTING
        e_return = zcheck_lote.

    IF zcheck_lote IS INITIAL.
      CALL METHOD create_batch_lgort
        EXPORTING
          material        = input-material
          centro          = input-centro
          deposito        = input-deposito_receptor
          lote            = input-lote_individual
          dt_vencimento   = input-data
          lote_fabricante = CONV #( input-lote_fabricante ) "// Lote Fabricante
        IMPORTING
          error           = DATA(_error).
    ENDIF.

    CHECK _error IS INITIAL.

    at_header = VALUE #(
                          pstng_date  = sy-datum
                          doc_date    = input-dt_registro
                          header_txt  = |{ acao(5) }. deposito { input-lote_individual }|
                       ).

    at_item = VALUE #( (
                          move_type  = input-tipo_movimento
                          plant      = input-centro
                          material   = input-material
                          entry_qnt  = input-quantidade

                          stge_loc   = input-deposito_saida
                          batch      = input-lote_individual

                          move_stloc = input-deposito_receptor
*                          move_batch = switch #( input-deposito_receptor when 'FORN' then 'FORNECEDOR'
*                                                                         when 'TERC' then 'TERCEIROS'
*                                                                         else sy-datum(4) )
                     ) ).
* ---> S4 Migration - 07/07/2023 - RZ - Inicio
    READ TABLE at_item ASSIGNING FIELD-SYMBOL(<fs_item_aux>) INDEX 1.
    IF sy-subrc EQ 0.
      DATA(v_len) = strlen( input-material ).

      IF v_len > 18.
        <fs_item_aux>-material_long  =   input-material.
      ELSE.
        <fs_item_aux>-material       =   input-material.
      ENDIF.
    ENDIF.

* <--- S4 Migration - 07/07/2023 - RZ - Fim
    CLEAR _document.
    CALL METHOD transfer_from_batch_to_batch
      EXPORTING
        header    = at_header
        items     = at_item
        direction = 'T'
      IMPORTING
        document  = _document
      CHANGING
        input     = input.

    IF _document-mat_doc IS NOT INITIAL.

      DATA(_0011) = get_zppt0011( input-lote_individual ).

      _0011-mblnr      = _document-mat_doc.
      _0011-mjahr      = _document-doc_year.
      _0011-r_werks    = SWITCH #( input-deposito_receptor WHEN 'FORN' OR 'TERC' THEN ''
                                                           ELSE input-centro_receptor ).
      _0011-umlgo      = SWITCH #( input-deposito_receptor WHEN 'FORN' OR 'TERC' THEN ''
                                                           ELSE input-deposito_receptor ).
      _0011-umcha      = SWITCH #( input-deposito_receptor WHEN 'FORN' THEN 'FORNECEDOR'
                                                           WHEN 'TERC' THEN 'TERCEIROS'
                                                           ELSE sy-datum(4) ).

*      _0011-usnam = sy-uname.
*      _0011-data_atual = data.
*      _0011-hora_atual = time.
*      _0011-id_movimentacao = input-id_movimentacao.

      UPDATE zppt0011 SET lgort = input-deposito_receptor
                          umlgo = ''
                          usnam = sy-uname
                          data_atual = data
                          hora_atual = time
                          id_movimentacao = input-id_movimentacao
                          mblnr = _document-mat_doc
                          mjahr = _document-doc_year
          WHERE charg  EQ input-lote_individual
            AND matnr            EQ input-material
            AND werks            EQ input-centro
            AND lgort            EQ input-deposito_saida.

      UPDATE zppt0019 SET processado = abap_true
                          status     = 'F'
      WHERE lote_individual  EQ input-lote_individual
        AND empresa          EQ input-empresa
        AND centro           EQ input-centro
        AND acao             EQ 'transferenciadeposito'.
      COMMIT WORK AND WAIT.
    ENDIF.

    CLEAR: _error.
  ENDMETHOD.
  "FIM USER STORY 152607 / AOENNING

  "INICIO USER STORY 152607 / AOENNING
  METHOD check_lote.
    CHECK i_matnr IS NOT INITIAL AND i_werks IS NOT INITIAL AND i_charg IS NOT INITIAL.

    SELECT * FROM mchb INTO TABLE @DATA(ws_mchb)
      WHERE matnr EQ @i_matnr
        AND werks EQ @i_werks
        AND lgort EQ @i_lgort
        AND charg EQ @i_charg.
    IF sy-subrc EQ 0.
      e_return = abap_true.
    ENDIF.
  ENDMETHOD.
  "FIM USER STORY 152607 / AOENNING


  METHOD recebimento.

    CHECK validar_campos( EXPORTING direction = 'R' CHANGING input = input  ) IS INITIAL.

    SELECT SINGLE *
      FROM zppt0011
      INTO @DATA(_0011)
      WHERE charg EQ @input-lote_individual
      AND r_werks EQ @input-centro.

    IF _0011 IS INITIAL.
      _return = VALUE #(
                            type = 'E'
                            "lote_individual = input-lote_individual   "FF - #134176
                            message = |{ acao } do Lote { input-lote_individual } não Encontrado!|
                         ).
    ELSE.

      DATA(lote_fb) = zcl_emb_backup=>get_lote( matnr = _0011-matnr
                                         werks = _0011-werks
                                         lgort = _0011-lgort
                                         input = _0011-lfabr ).

      CALL METHOD create_batch
        EXPORTING
          material        = _0011-matnr
          centro          = _0011-r_werks
          deposito        = _0011-lgort
          lote            = _0011-charg
          dt_vencimento   = _0011-vfdat
          lote_fabricante = CONV #( lote_fb ) "// Lote Fabricante
        IMPORTING
          error           = DATA(_error).


      IF NOT _error IS INITIAL.

        _return = VALUE #(
                            type = 'E'
                            "lote_individual = itens-lote_individual   "FF - #134176
                            message = |{ acao } do Lote { input-lote_individual } não Encontrado!|
                         ).

      ELSE.

        at_header = VALUE #(
                             pstng_date  = sy-datum
                             doc_date    = input-dt_registro
                             header_txt  = |{ acao(5) }. do { input-lote_individual }|
                           ).

        at_item = VALUE #( (
                              move_type  = input-tipo_movimento
                              plant      = input-centro
*                              material   = _0011-matnr
*                              entry_qnt  = _0011-clabs
                              entry_qnt  = input-quantidade

                              stge_loc   = _0011-umlgo
                              batch      = _0011-umcha

*                              MOVE_PLANT = INPUT-CENTRO
                              move_stloc = _0011-lgort
                              move_batch = input-lote_individual
                         ) ).
* ---> S4 Migration - 07/07/2023 - RZ - Inicio
        READ TABLE at_item ASSIGNING FIELD-SYMBOL(<fs_item_aux>) INDEX 1.
        IF sy-subrc EQ 0.
          DATA(v_len) = strlen( _0011-matnr ).

          IF v_len > 18.
            <fs_item_aux>-material_long  =   _0011-matnr.
          ELSE.
            <fs_item_aux>-material       =   _0011-matnr.
          ENDIF.
        ENDIF.

* <--- S4 Migration - 07/07/2023 - RZ - Fim

        CALL METHOD transfer_from_batch_to_batch
          EXPORTING
            header    = at_header
            items     = at_item
            direction = 'R'
          IMPORTING
            document  = _document
          CHANGING
            input     = input.

        IF _document IS NOT INITIAL.

          _0011-r_werks = _0011-werks.

          _0011-werks = input-centro.

          _0011-lgort = _0011-umlgo.

          _0011-mblnr = _document-mat_doc.
          _0011-mjahr = _document-doc_year.

          _0011-umlgo = ''.
          _0011-umcha = ''.

          _0011-usnam = sy-uname.
          _0011-data_atual = data.
          _0011-hora_atual = time.
          _0011-id_movimentacao = input-id_movimentacao.

          MODIFY zppt0011 FROM _0011.
          COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024

          CALL METHOD update_19
            EXPORTING
              i_lote_individual = input-lote_individual
              i_empresa         = input-empresa
              i_centro          = input-centro
              i_cendes          = _0011-r_werks
              i_acao            = CONV #( acao ).

        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD e_transferencia.

*          DATA: IT_CHANGE TYPE TABLE OF BAPI2093_RES_ITEM_CHANGE.
*          DATA: IT_CHANGEX TYPE TABLE OF BAPI2093_RES_ITEM_CHANGEX.
*          DATA: IT_ITEM_NEW TYPE TABLE OF BAPI2093_RES_ITEM_NEW.
*
*          CLEAR WA_ZPP0011.
*
*          SELECT SINGLE *
*              FROM ZPPT0011
*              INTO WA_ZPP0011
*              WHERE CHARG EQ <SL_TRANS>-LOTE_EXCLUIDO.
*
*          IF WA_ZPP0011-RSNUM EQ '0000000000'.
*            _RETURN = VALUE #(
*                               TYPE = 'S'
*                               MESSAGE = |Reserva { <SL_TRANS>-LOTE_EXCLUIDO } não existe!|
*                             ).
*          ELSE.
*
*            SELECT *
*              FROM RESB
*              INTO TABLE @DATA(IT_RESB)
*              WHERE RSNUM EQ @WA_ZPP0011-RSNUM
*                AND KZEAR NE @ABAP_TRUE
*                AND XLOEK NE @ABAP_TRUE.
*
*            LOOP AT IT_RESB INTO DATA(WA_RESB).
*
*              REFRESH: IT_CHANGE, IT_CHANGEX, L_RETURN.
*
*              IT_ITEM_NEW = VALUE #( (
*                                        MATERIAL  = <SL_TRANS>-MATERIAL
*                                        PLANT     = <SL_TRANS>-CENTRO
*                                        STGE_LOC  = <SL_TRANS>-DEPOSITO_SAIDA
*                                        BATCH     = <SL_TRANS>-LOTE_INDIVIDUAL
*                                        ENTRY_QNT = <SL_TRANS>-QUANTIDADE
*                                        MOVEMENT  = ABAP_TRUE
*                                   ) ).
*
*              CALL FUNCTION 'BAPI_RESERVATION_CHANGE'
*                EXPORTING
*                  RESERVATION               = WA_RESB-RSNUM
*                TABLES
*                  RESERVATIONITEMS_CHANGED  = IT_CHANGE
*                  RESERVATIONITEMS_CHANGEDX = IT_CHANGEX
*                  RESERVATIONITEMS_NEW      = IT_ITEM_NEW
*                  RETURN                    = L_RETURN.
*
*              IF NOT LINE_EXISTS( L_RETURN[ TYPE = 'E' ] ).
*
*                UPDATE ZPPT0011
*                    SET RSNUM = WA_RESB-RSNUM
*                  WHERE CHARG EQ <SL_TRANS>-LOTE_INDIVIDUAL.
*
*
*                IT_CHANGE =  VALUE #( (
*                                      RES_ITEM = WA_RESB-RSPOS
*                                      DELETE_IND = ABAP_TRUE
*                                    ) ).
*
*                IT_CHANGEX =  VALUE #( (
*                                       RES_ITEM = WA_RESB-RSPOS
*                                       DELETE_IND = ABAP_TRUE
*                                     ) ).
*
*                CALL FUNCTION 'BAPI_RESERVATION_CHANGE'
*                  EXPORTING
*                    RESERVATION               = WA_RESB-RSNUM
*                  TABLES
*                    RESERVATIONITEMS_CHANGED  = IT_CHANGE
*                    RESERVATIONITEMS_CHANGEDX = IT_CHANGEX
*                    RETURN                    = L_RETURN.
*
*                IF NOT LINE_EXISTS( L_RETURN[ TYPE = 'E' ] ).
*
*                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*                    EXPORTING
*                      WAIT = ABAP_TRUE.
*
*                  IF <SL_TRANS>-LOTE_INDIVIDUAL NE <SL_TRANS>-LOTE_EXCLUIDO.
*                    UPDATE ZPPT0011
*                        SET RSNUM = '0000000000'
*                      WHERE CHARG EQ <SL_TRANS>-LOTE_EXCLUIDO.
*                  ENDIF.
*
*                  _RETURN = VALUE #(
*                                     TYPE = 'S'
*                                     MESSAGE = |Lote { <SL_TRANS>-LOTE_INDIVIDUAL } editado com sucesso!|
*                                   ).
*                ELSE.
*
*                  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
*                    EXPORTING
*                      TYPE   = SY-MSGTY
*                      CL     = SY-MSGID
*                      NUMBER = SY-MSGNO
*                      PAR1   = SY-MSGV1
*                      PAR2   = SY-MSGV2
*                      PAR3   = SY-MSGV3
*                      PAR4   = SY-MSGV4
*                    IMPORTING
*                      RETURN = _RETURN.
*                ENDIF.
*
*              ELSE.
*
*                CALL FUNCTION 'BALW_BAPIRETURN_GET2'
*                  EXPORTING
*                    TYPE   = SY-MSGTY
*                    CL     = SY-MSGID
*                    NUMBER = SY-MSGNO
*                    PAR1   = SY-MSGV1
*                    PAR2   = SY-MSGV2
*                    PAR3   = SY-MSGV3
*                    PAR4   = SY-MSGV4
*                  IMPORTING
*                    RETURN = _RETURN.
*              ENDIF.
*            ENDLOOP.
*          ENDIF.
*
*          APPEND CORRESPONDING #( _RETURN ) TO RETURN.

  ENDMETHOD.

  METHOD e_lote_fabricante.

    DATA(_0011) = get_zppt0011( input-lote_individual ).

    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument = _0011-mblnr
        matdocumentyear  = _0011-mjahr
      TABLES
        return           = l_return.

    IF NOT line_exists( l_return[ type = 'E' ] ).

      DELETE FROM zppt0011 WHERE charg EQ input-lote_individual.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      at_header = VALUE #(
                            pstng_date  = sy-datum
                            header_txt  = |Edição do { input-lote_individual }|
                         ).

      at_item = VALUE #( (
                            move_type  = '311'
                            plant      = input-centro
                            material   = input-material
                            entry_qnt  = input-quantidade

                            stge_loc   = input-deposito_saida
                            batch      = input-lote_fabricante

                            move_stloc = input-deposito_saida
                            move_batch = input-lote_individual
                       ) ).
* ---> S4 Migration - 07/07/2023 - RZ - Inicio
      READ TABLE at_item ASSIGNING FIELD-SYMBOL(<fs_item_aux>) INDEX 1.
      IF sy-subrc EQ 0.
        DATA(v_len) = strlen( input-material ).

        IF v_len > 18.
          <fs_item_aux>-material_long  =   input-material.
        ELSE.
          <fs_item_aux>-material       =   input-material.
        ENDIF.
      ENDIF.

* <--- S4 Migration - 07/07/2023 - RZ - Fim
      FREE: l_return, _return.

      CALL METHOD transfer_from_batch_to_batch
        EXPORTING
          header    = at_header
          items     = at_item
          direction = 'F'
        IMPORTING
          document  = _document
        CHANGING
          input     = input.

      IF ( _document IS NOT INITIAL ).
        it_0011 = VALUE #( (
                              ebeln = zcl_emb_backup=>get_pedido( EXPORTING
                                                                    matnr = input-material
                                                                    werks = input-centro
                                                                    lgort = input-deposito_saida
                                                                    charg = zcl_emb_backup=>get_lote( matnr = input-material
                                                                                               werks = input-centro
                                                                                               lgort = input-deposito_saida
                                                                                                input = input-lote_fabricante )
                                                                  )
                              matnr = input-material
                              werks = input-centro
                              lgort = input-deposito_saida
                              charg = input-lote_individual
                              mblnr = _document-mat_doc
                              mjahr = _document-doc_year
                              clabs = input-quantidade
                              vfdat = input-data
                              lfabr = input-lote_fabricante
                              usnam = sy-uname
                              data_atual = data
                              hora_atual = time
                              id_movimentacao = input-id_movimentacao
        ) ).

        MODIFY zppt0011 FROM TABLE it_0011.
        COMMIT WORK AND WAIT. "2000016202 - IR192540 - FT - STEFANINI - 03.09.2024

      ENDIF.

    ELSE.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = _return.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      APPEND CORRESPONDING #( _return ) TO return.

    ENDIF.

  ENDMETHOD.

  METHOD validar_campos.

    DATA(lines_begin) = lines( return ).

    FREE it_erros.

    CASE direction.
      WHEN 'E'.
        IF get_zppt0011( input-lote_individual ) IS NOT INITIAL.
          APPEND VALUE #( type = 'E'
                          lote_individual = input-lote_individual   "FF - #134176
                          message = |Lote { input-lote_individual } já foi realizado a Entrada!| ) TO it_erros.
        ENDIF.
      WHEN OTHERS.
        IF get_zppt0011( input-lote_individual ) IS INITIAL.
          APPEND VALUE #( type = 'E'
                          lote_individual = input-lote_individual   "FF - #134176
                          message = |Lote { input-lote_individual } não existe!| ) TO it_erros.
        ENDIF.
    ENDCASE.

    CASE direction.
      WHEN 'E'.
        IF input-centro IS INITIAL. APPEND VALUE #( type = 'E' message = 'Centro é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-material IS INITIAL. APPEND VALUE #( type = 'E' message = 'Material é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-quantidade IS INITIAL. APPEND VALUE #( type = 'E' message = 'Quantidade é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-deposito_saida IS INITIAL. APPEND VALUE #( type = 'E' message = 'Deposito Saida é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-lote_fabricante IS INITIAL. APPEND VALUE #( type = 'E' message = 'Lote Fabricante é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-lote_individual IS INITIAL. APPEND VALUE #( type = 'E' message = 'Lote Individual é Obrigatório!' ) TO it_erros. ENDIF.

      WHEN 'S'.
        IF input-centro IS INITIAL. APPEND VALUE #( type = 'E' message = 'Centro é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-material IS INITIAL. APPEND VALUE #( type = 'E' message = 'Material é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-lote_individual IS INITIAL. APPEND VALUE #( type = 'E' message = 'Lote Individual é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-quantidade IS INITIAL. APPEND VALUE #( type = 'E' message = 'Quantidade é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-deposito_saida IS INITIAL. APPEND VALUE #( type = 'E' message = 'Deposito Saida é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-deposito_receptor IS INITIAL. APPEND VALUE #( type = 'E' message = 'Deposito Receptor é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-nr_ordem IS INITIAL. APPEND VALUE #( type = 'E' message = 'O campo Ordem é Obrigatório!' ) TO it_erros. ENDIF.

        IF input-centro IS NOT INITIAL AND
           input-material IS NOT INITIAL AND
           input-deposito_receptor IS NOT INITIAL.

          SELECT COUNT(*)
             FROM mchb
             WHERE matnr EQ input-material
               AND werks EQ input-centro
               AND charg EQ input-deposito_receptor.

          IF sy-subrc IS NOT INITIAL.
            APPEND VALUE #(
                              type = 'E'
                              lote_individual = input-lote_individual   "FF - #134176
                              message = |Deposito { input-deposito_receptor } | &&
                                        |não expandido para o material { input-material } | &&
                                        |Centro { input-centro }!| ) TO it_erros.

          ENDIF.
        ENDIF.

      WHEN 'D'.
        IF input-centro IS INITIAL. APPEND VALUE #( type = 'E' message = 'Centro é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-material IS INITIAL. APPEND VALUE #( type = 'E' message = 'Material é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-quantidade IS INITIAL. APPEND VALUE #( type = 'E' message = 'Quantidade é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-deposito_saida IS INITIAL. APPEND VALUE #( type = 'E' message = 'Deposito Saida é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-deposito_receptor IS INITIAL. APPEND VALUE #( type = 'E' message = 'Deposito Receptor é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-lote_individual IS INITIAL. APPEND VALUE #( type = 'E' message = 'Lote Individual é Obrigatório!' ) TO it_erros. ENDIF.

        DATA(_0011) = get_zppt0011( input-lote_individual ).

        IF input-quantidade > _0011-dlabs.
          APPEND VALUE #( type = 'E'
                          lote_individual = input-lote_individual   "FF - #134176
                          message = 'Devolução superior com a permitida!' ) TO it_erros.
        ENDIF.
        IF _0011-umcha IS INITIAL.
          APPEND VALUE #( type = 'E'
                          lote_individual = input-lote_individual   "FF - #134176
                          message = |Lote { input-lote_individual } não encontrado para devolução!| ) TO it_erros.
        ENDIF.
        IF input-deposito_saida NE _0011-umcha.
          APPEND VALUE #( type = 'E'
                          lote_individual = input-lote_individual   "FF - #134176
                          message = |Equipe Divergente { input-deposito_saida } não é valido!| ) TO it_erros.
        ENDIF.

      WHEN 'T'.
        IF input-centro IS INITIAL. APPEND VALUE #( type = 'E' message = 'Centro é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-material IS INITIAL. APPEND VALUE #( type = 'E' message = 'Material é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-quantidade IS INITIAL. APPEND VALUE #( type = 'E' message = 'Quantidade é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-deposito_saida IS INITIAL. APPEND VALUE #( type = 'E' message = 'Deposito Saida é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-lote_individual IS INITIAL. APPEND VALUE #( type = 'E' message = 'Lote Individual é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-deposito_receptor IS INITIAL. APPEND VALUE #( type = 'E' message = 'Deposito Receptor é Obrigatório!' ) TO it_erros. ENDIF.

        IF input-centro IS NOT INITIAL AND
           input-material IS NOT INITIAL AND
           input-deposito_receptor IS NOT INITIAL AND
           input-centro_receptor IS NOT INITIAL.

          DATA(move_batch) = SWITCH #( input-deposito_receptor WHEN 'FORN' THEN 'FORNECEDOR'
                                                               WHEN 'TERC' THEN 'TERCEIROS'
                                                               ELSE sy-datum(4) ).

          SELECT COUNT(*)
            FROM mchb
            WHERE matnr EQ input-material
            AND werks EQ input-centro
            AND charg EQ move_batch.

          IF sy-subrc IS NOT INITIAL.
            APPEND VALUE #(
                              type = 'E'
                              lote_individual = input-lote_individual   "FF - #134176
                              message = |Lote { move_batch } | &&
                                        |não expandido para o material { input-material } | &&
                                        |Centro { input-centro }!| ) TO it_erros.
          ENDIF.

          IF move_batch EQ sy-datum(4).
            IF input-centro_receptor IS INITIAL. APPEND VALUE #( type = 'E' message = 'Centro Receptor é Obrigatório!' ) TO it_erros. ENDIF.

            SELECT COUNT(*)
              FROM mchb
              WHERE matnr EQ input-material
              AND werks EQ input-centro_receptor
              AND charg EQ move_batch.

            IF sy-subrc IS NOT INITIAL.
              APPEND VALUE #(
                                type = 'E'
                                lote_individual = input-lote_individual   "FF - #134176
                                message = |Lote { move_batch } | &&
                                          |não expandido para o material { input-material } | &&
                                          |Centro { input-centro_receptor }!| ) TO it_erros.
            ENDIF.

          ENDIF.

        ENDIF.

      WHEN 'R'.
        IF input-centro IS INITIAL. APPEND VALUE #( type = 'E' message = 'Centro é Obrigatório!' ) TO it_erros. ENDIF.
        IF input-lote_individual IS INITIAL. APPEND VALUE #( type = 'E' message = 'Lote Individual é Obrigatório!' ) TO it_erros. ENDIF.
    ENDCASE.

    TRY.
        input-msg = it_erros[ 1 ]-message.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    APPEND LINES OF it_erros TO return.

    DATA(lines_end) = lines( return ).

    IF lines_begin NE lines_end.
      erro = abap_true.
    ELSE.
      erro = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD check_19.
    SELECT COUNT(*)
    FROM zppt0019
    WHERE lote_individual   EQ i_lote_individual
    AND empresa           EQ i_empresa
    AND centro            EQ i_centro
    AND acao              EQ i_acao
    AND id_movimentacao   EQ i_id_movimentacao
    AND processado        EQ abap_true
    AND estorno           EQ abap_false.

    return = sy-subrc.
  ENDMETHOD.

  METHOD add_19.

    FREE it_excecao.
    DATA: wa_0019 TYPE zppt0019.
    DATA _matnr TYPE matnr.
    DATA _id_mov TYPE zid_movimentacao.

    DATA: zvg_acao TYPE char50.

    DATA(produtos) = input-produtos.
    SORT produtos BY lote_individual.
    DELETE ADJACENT DUPLICATES FROM produtos COMPARING lote_individual.

    LOOP AT produtos INTO DATA(itens).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = itens-codigo
        IMPORTING
          output       = _matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      CLEAR: _id_mov.
      _id_mov = |{ itens-id_movimentacao ALPHA = OUT }|.
      CONDENSE _id_mov NO-GAPS.


      CALL METHOD check_19
        EXPORTING
          i_lote_fabricante = itens-lote_fabricante
          i_lote_individual = itens-lote_individual
          i_id_movimentacao = _id_mov
          i_empresa         = input-empresa
          i_centro          = input-centro
          i_acao            = input-acao
        IMPORTING
          return            = DATA(i_subrc).

*      IF I_SUBRC EQ 0.
*        SELECT SINGLE *
*        FROM MCHB
*        INTO @DATA(W_MCHB)
*          WHERE MATNR EQ  @_MATNR
*            AND WERKS EQ @INPUT-CENTRO
*            AND CHARG EQ @ITENS-LOTE_INDIVIDUAL
*            AND CLABS NE @SPACE.
*        IF W_MCHB IS NOT INITIAL.
*          I_SUBRC = '4'.
*        ENDIF.
*      ENDIF.

      IF i_subrc IS INITIAL.
        APPEND VALUE #( type = 'E'
                        lote_individual = itens-lote_individual   "FF - #134176
                        message = |A ação { input-acao } já foi executada para o Lote { itens-lote_individual }!| )
                        TO return.

        APPEND VALUE #( lote = itens-lote_individual ) TO it_excecao.
        CONTINUE.
      ENDIF.

      IF input-acao EQ 'saida'.

        SELECT SINGLE *
        FROM zppt0011
          INTO @DATA(wa_0011)
        WHERE matnr EQ @_matnr
          AND werks EQ @input-centro
          AND lgort EQ @itens-deposito_saida
          AND charg EQ @itens-lote_individual.

        ADD itens-quantidade TO wa_0011-dlabs.

*        IF WA_0011-DLABS < WA_0011-CLABS.
*          CONTINUE.
*        ENDIF.

      ENDIF.

      "INICIO USER STORY 152607 / AOENNING
      IF input-acao = 'transferenciaDeposito'.
        zvg_acao = |{ input-acao CASE = LOWER }|.
      ELSE.
        zvg_acao = input-acao.
      ENDIF.
      "FIM USER STORY 152607 / AOENNING

      wa_0019 = VALUE #(
                          lote_individual     = |{ itens-lote_individual CASE = UPPER }|
                          id_movimentacao     = _id_mov
                          empresa             = input-empresa
                          centro              = input-centro
                          acao                = zvg_acao                   "USER STORY 152607 / AOENNING
                          bloco               = zcl_emb_backup=>get_bloco( )
                          codigo              = itens-codigo
                          descricao           = itens-descricao
                          data                = itens-data
                          lote_fabricante     = itens-lote_fabricante
                          quantidade          = itens-quantidade
                          deposito_saida      = itens-deposito_saida
                          centro_receptor     = itens-centro_receptor
                          lote_receptor       = itens-lote_receptor
                          deposito_receptor   = itens-deposito_receptor
                          unidade_medida      = itens-unidade_medida
                          lote_excluido       = itens-lote_excluido
                          nr_ordem            = input-nr_ordem
                          status              = 'A'
                          dt_registro         = sy-datum
                          hr_registro         = sy-uzeit
                       ).

      MODIFY zppt0019 FROM wa_0019.
      COMMIT WORK AND WAIT.

    ENDLOOP.

  ENDMETHOD.

  METHOD update_19.
    CASE i_acao.
      WHEN 'devolucao' OR 'saida' OR 'transferencia' OR 'recebimento' OR 'transferenciadeposito'.

        DATA(vstatus) = SWITCH zeacao( i_acao WHEN 'saida'         THEN 'devolucao'
                                              WHEN 'devolucao'     THEN 'saida'
                                              WHEN 'transferencia' THEN 'recebimento'
                                              WHEN 'recebimento'   THEN 'transferencia'
                                              WHEN 'transferenciadeposito' THEN 'transferencia'
                                     ).

        DATA(werks_) = SWITCH werks_d( i_acao WHEN 'transferencia' THEN i_cendes
                                              WHEN 'transferenciadeposito' THEN i_cendes
                                              WHEN 'recebimento'   THEN i_centro
                                              ELSE i_centro
                                     ).

        UPDATE zppt0019 SET estorno = abap_true
          WHERE lote_individual  EQ i_lote_individual
            AND empresa          EQ i_empresa
            AND centro           EQ werks_
            AND acao             EQ vstatus
            AND processado       EQ abap_true
            AND estorno          EQ abap_false.

    ENDCASE.

    UPDATE zppt0019 SET processado = abap_true
    WHERE lote_individual  EQ i_lote_individual
      AND empresa          EQ i_empresa
      AND centro           EQ i_centro
      AND acao             EQ i_acao
      AND processado       EQ abap_false
      AND estorno          EQ abap_false.

  ENDMETHOD.

  METHOD check_qtd.

    DATA: _matnr TYPE matnr,
          _werks TYPE werks_d,
          _lgort TYPE lgort_d,
          _charg TYPE charg_d.

    CLEAR lv_qtd_invalido.

    DATA(_material) = embalagem-produtos.
    SORT _material BY codigo.
    DELETE ADJACENT DUPLICATES FROM _material COMPARING codigo.

    LOOP AT _material INTO DATA(w_material).

*      DATA(saida) = embalagem-produtos[ 1 ].

      CASE embalagem-acao.
        WHEN 'devolucao'.

*          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*            EXPORTING
*              input        = w_material-codigo
*            IMPORTING
*              output       = _matnr
*            EXCEPTIONS
*              length_error = 1
*              OTHERS       = 2.
*
          _werks = embalagem-centro.
          _lgort = w_material-deposito_saida.
          _charg = w_material-lote_individual.

          SELECT SINGLE quantidade
            FROM zppt0019
            INTO @DATA(lv_quantidade)
            WHERE lote_individual EQ @_charg
              AND acao            EQ 'saida'.

          IF sy-subrc EQ 0.
            IF lv_quantidade < w_material-quantidade.
              APPEND VALUE #( type = 'E' message = |Não é permitido enviar volume diferente da embalagem| lote_individual = _charg ) TO return.
              lv_qtd_invalido = abap_true.
              EXIT.
            ENDIF.
          ENDIF.

        WHEN 'transferencia' OR 'terceiros'.

          SELECT SINGLE matnr, lgort, charg, clabs
            INTO @DATA(wa_tmchb)
            FROM mchb
            WHERE charg EQ @w_material-lote_individual
              AND lgort EQ @w_material-deposito_saida
              AND clabs NE @space.

          IF sy-subrc EQ 0.
            IF w_material-quantidade NE wa_tmchb-clabs.
              APPEND VALUE #( type = 'E' message = |Não é permitido enviar volume diferente da embalagem| lote_individual = wa_tmchb-charg ) TO return.
              lv_qtd_invalido = abap_true.
              EXIT.
            ENDIF.
          ENDIF.

        WHEN 'saida'.

          SELECT SINGLE matnr, lgort, charg, clabs
            INTO @DATA(ls_tmchb)
            FROM mchb
            WHERE charg EQ @w_material-lote_individual
              AND lgort EQ @w_material-deposito_saida
              AND clabs NE @space.

          IF sy-subrc EQ 0.
            IF w_material-quantidade <= ls_tmchb-clabs.

              "

            ELSE.
              APPEND VALUE #( type = 'E'
                              message = |Não é permitido enviar volume diferente da embalagem|
                              lote_individual = ls_tmchb-charg ) TO return.
              lv_qtd_invalido = abap_true.
              EXIT.
            ENDIF.
          ENDIF.


        WHEN 'recebimento'.
          SELECT SINGLE lote_individual, quantidade,
            MAX( dt_registro ) AS dt_registro,
            MAX( hr_registro ) AS hr_registro
              INTO @DATA(wa_zppt009)
              FROM zppt0019
              WHERE lote_individual EQ @w_material-lote_individual
                AND acao IN ( 'transferencia', 'devolucao' )
              GROUP BY lote_individual, quantidade.

          IF sy-subrc EQ 0.
            IF wa_zppt009-quantidade NE w_material-quantidade.
              APPEND VALUE #( type = 'E' message = |É esperado receber { wa_zppt009-quantidade }| lote_individual = wa_tmchb-charg ) TO return.
              lv_qtd_invalido = abap_true.
              EXIT.
            ENDIF.
          ENDIF.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

*      _werks = embalagem-centro.
*      _lgort = w_material-deposito_saida.
*      _charg = w_material-lote_individual.
*
*      DATA(total_emb) = REDUCE labst( INIT i TYPE labst FOR ls IN embalagem-produtos WHERE ( codigo EQ w_material-codigo ) NEXT i = i + ls-quantidade ).
*
*      SELECT SUM( clabs )
*        FROM mchb
*        INTO @DATA(total_mchb)
*        WHERE matnr EQ @_matnr
*          AND werks EQ @_werks
*          AND lgort EQ @_lgort
*          AND charg EQ @_charg.
*
*      IF total_mchb < total_emb.
*        APPEND VALUE #( type = 'E' message = |Quantidade { total_emb } Superior da Equipe { saida-deposito_saida }!| ) TO return.
*        lv_qtd_invalido = abap_true.
*        EXIT.
*      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_mchb.
    SELECT SUM( clabs )
      FROM mchb
      INTO @DATA(total_mchb)
      WHERE matnr EQ @input-material
        AND werks EQ @input-centro
        AND lgort EQ @input-deposito_saida
        AND charg EQ @input-lote_individual.
    IF total_mchb NE 0.
      return = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD set_bloco.
    CLEAR at_bloco.
    SELECT MAX( bloco )
      FROM zppt0015
      INTO at_bloco.
    ADD 1 TO at_bloco.
  ENDMETHOD.

  METHOD get_bloco.
    e_bloco = at_bloco.
  ENDMETHOD.

ENDCLASS.
