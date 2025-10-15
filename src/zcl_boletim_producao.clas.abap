CLASS zcl_boletim_producao DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_boletim_producao .

    CONSTANTS at_date_processamento TYPE sy-datum VALUE '' ##NO_TEXT.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_boletim_producao IMPLEMENTATION.


  METHOD zif_boletim_producao~aprovar_lancamento.

    DATA: var_answer TYPE c.

    IF i_id_boletim_producao IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0246 INTO @DATA(wl_0246)
     WHERE id_boletim EQ @i_id_boletim_producao.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Documento Boletim Id:' && i_id_boletim_producao )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Documento Boletim Id:' && i_id_boletim_producao ).
    ENDIF.

    IF wl_0246-aprovado EQ abap_true.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_aprovado-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_aprovado-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_aprovado-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_aprovado-msgid.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja realmente aprovar o Lançamento?'
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

    wl_0246-us_aprovacao = sy-uname.
    wl_0246-dt_aprovacao = sy-datum.
    wl_0246-hr_aprovacao = sy-uzeit.
    wl_0246-aprovado     = abap_true.

    MODIFY zsdt0246 FROM wl_0246.
    COMMIT WORK.

    MESSAGE s032.




  ENDMETHOD.


  METHOD zif_boletim_producao~bloquear_registros.

    IF i_id_boletim IS NOT INITIAL.

      CALL FUNCTION 'ENQUEUE_EZSDT0246'
        EXPORTING
          id_boletim     = i_id_boletim
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        zif_boletim_producao~show_erro_geral( ).
      ENDIF.

    ENDIF.

    IF i_docnum_zsdt0251 IS NOT INITIAL.

      CALL FUNCTION 'ENQUEUE_EZSDT0251'
        EXPORTING
          docnum         = i_docnum_zsdt0251
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        zif_boletim_producao~show_erro_geral( ).
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD zif_boletim_producao~bloqueio_desbloqueio_produtos.

    DATA(_bloqueio_mat_werks) = abap_false.
    DATA(_bloqueio_mat_charg) = abap_false.

    LOOP AT i_itens INTO DATA(wa_goodsmovements).
*---> 20/06/2023 - Migração S4 - JS
      DATA: lv_material TYPE mara-matnr.

      IF wa_goodsmovements-material IS NOT INITIAL.
        lv_material = wa_goodsmovements-material.
      ELSE.
        lv_material = wa_goodsmovements-material_long.
      ENDIF.
*<--- 20/06/2023 - Migração S4 - JS

      CASE i_acao.
        WHEN 'B'. "Bloquear

          "Bloqueio Material
          CALL FUNCTION 'ENQUEUE_EMMARCE'
            EXPORTING
              matnr          = lv_material
              werks          = wa_goodsmovements-plant
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF ( sy-subrc NE 0 ).

            _bloqueio_mat_werks = abap_true.

            DATA(_msgid) = sy-msgid.
            DATA(_msgno) = sy-msgno.
            DATA(_msgv1) = sy-msgv1.
            DATA(_msgv2) = sy-msgv2.
            DATA(_msgv3) = sy-msgv3.
            DATA(_msgv4) = sy-msgv4.

            "Bloqueio lote
          ELSEIF wa_goodsmovements-batch IS NOT INITIAL.

            CALL FUNCTION 'ENQUEUE_EMMCH1E'
              EXPORTING
                mode_mch1      = 'E'
                mandt          = sy-mandt
                matnr          = lv_material
                charg          = wa_goodsmovements-batch
                _scope         = '2'
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            IF sy-subrc <> 0.

              _bloqueio_mat_charg = abap_true.

              _msgid = sy-msgid.
              _msgno = sy-msgno.
              _msgv1 = sy-msgv1.
              _msgv2 = sy-msgv2.
              _msgv3 = sy-msgv3.
              _msgv4 = sy-msgv4.

            ENDIF.

          ENDIF.

          IF ( _bloqueio_mat_werks EQ abap_true ) OR ( _bloqueio_mat_charg EQ abap_true ) .

            LOOP AT i_itens INTO DATA(wa_goodsmovements_aux).
*---> 20/06/2023 - Migração S4 - JS
              DATA: lv_material_aux TYPE mara-matnr.

              IF wa_goodsmovements_aux-material IS NOT INITIAL.
                lv_material_aux = wa_goodsmovements_aux-material.
              ELSE.
                lv_material_aux = wa_goodsmovements_aux-material_long.
              ENDIF.
*<--- 20/06/2023 - Migração S4 - JS
              "Desbloqueio Material
              CALL FUNCTION 'DEQUEUE_EMMARCE'
                EXPORTING
                  matnr = lv_material_aux
                  werks = wa_goodsmovements_aux-plant.

              "Desbloqueio Lote
              IF wa_goodsmovements-batch IS NOT INITIAL.
                CALL FUNCTION 'DEQUEUE_EMMCH1E'
                  EXPORTING
                    mode_mch1 = 'E'
                    mandt     = sy-mandt
                    matnr     = lv_material_aux
                    charg     = wa_goodsmovements_aux-batch.
              ENDIF.
            ENDLOOP.


            IF _bloqueio_mat_werks EQ abap_true.

              RAISE EXCEPTION TYPE zcx_boletim_producao
                EXPORTING
                  textid = VALUE #( msgid = zcx_boletim_producao=>zcx_material_centro_bloqueado-msgid
                                    msgno = zcx_boletim_producao=>zcx_material_centro_bloqueado-msgno
                                    attr1 = CONV #( wa_goodsmovements-material )
                                    attr2 = CONV #( wa_goodsmovements-plant    )
                                    attr3 = CONV #( _msgv1                     )
                                   )
                  msgty  = 'E'
                  msgno  = zcx_boletim_producao=>zcx_material_centro_bloqueado-msgno
                  msgid  = zcx_boletim_producao=>zcx_material_centro_bloqueado-msgid
                  msgv1  = CONV #( wa_goodsmovements-material )
                  msgv2  = CONV #( wa_goodsmovements-plant )
                  msgv3  = CONV #( _msgv1 ).

            ELSEIF _bloqueio_mat_charg EQ abap_true.

              RAISE EXCEPTION TYPE zcx_boletim_producao
                EXPORTING
                  textid = VALUE #( msgid = zcx_boletim_producao=>zcx_material_lote_bloqueado-msgid
                                    msgno = zcx_boletim_producao=>zcx_material_lote_bloqueado-msgno
                                    attr1 = CONV #( wa_goodsmovements-material  )
                                    attr2 = CONV #( wa_goodsmovements-batch )
                                    attr3 = CONV #( _msgv1                      )
                                   )
                  msgty  = 'E'
                  msgno  = zcx_boletim_producao=>zcx_material_lote_bloqueado-msgno
                  msgid  = zcx_boletim_producao=>zcx_material_lote_bloqueado-msgid
                  msgv1  = CONV #( wa_goodsmovements-material )
                  msgv2  = CONV #( wa_goodsmovements-batch )
                  msgv3  = CONV #( _msgv1 ).

            ENDIF.

          ENDIF.

        WHEN 'D'. "Deabloquear


          "Desbloqueio Material
          CALL FUNCTION 'DEQUEUE_EMMARCE'
            EXPORTING
              matnr = lv_material
              werks = wa_goodsmovements-plant.

          "Desbloqueio Lote
          IF wa_goodsmovements-batch IS NOT INITIAL.
            CALL FUNCTION 'DEQUEUE_EMMCH1E'
              EXPORTING
                mode_mch1 = 'E'
                mandt     = sy-mandt
                matnr     = lv_material
                charg     = wa_goodsmovements-batch.
          ENDIF.

      ENDCASE.

    ENDLOOP.


  ENDMETHOD.


  METHOD zif_boletim_producao~check_aprovacao.

    IF i_id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0246 INTO @DATA(wl_0246)
     WHERE id_boletim EQ @i_id_boletim.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Documento Boletim Id:' && i_id_boletim )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Documento Boletim Id:' && i_id_boletim ).
    ENDIF.

    IF wl_0246-aprovado EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_not_aprovado-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_not_aprovado-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_not_aprovado-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_not_aprovado-msgid.
    ENDIF.

  ENDMETHOD.


  METHOD zif_boletim_producao~check_desaprovacao.

    IF i_id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0246 INTO @DATA(wl_0246)
     WHERE id_boletim EQ @i_id_boletim.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Documento Boletim Id:' && i_id_boletim )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Documento Boletim Id:' && i_id_boletim ).
    ENDIF.

    IF wl_0246-aprovado EQ abap_true.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Documento para validação' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Documento Boletim Id:' && i_id_boletim && 'esta aprovado' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_boletim_producao~check_documentos_gerados.

    DATA: it_0252 TYPE TABLE OF zsdt0252.

    DATA: it_zfiwrt0008 TYPE TABLE OF zfiwrt0008,
          it_mkpf       TYPE TABLE OF mkpf,
          it_mseg       TYPE TABLE OF mseg.

    DATA: v_key_docs_like TYPE zsdt0252-key_docs.

    CLEAR: it_0252[].

    IF i_id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    SELECT *
      FROM zsdt0252 INTO TABLE it_0252
     WHERE id_boletim EQ i_id_boletim.

    LOOP AT it_0252 INTO DATA(wl_0252) WHERE seqlcto_devol      IS NOT INITIAL OR
                                             seqlcto_ent_dev    IS NOT INITIAL OR
                                             seqlcto_ind        IS NOT INITIAL OR
                                             seqlcto_ent_ind    IS NOT INITIAL OR
                                             doc_prod_01        IS NOT INITIAL OR
                                             doc_prod_02        IS NOT INITIAL OR
                                             doc_prod_03        IS NOT INITIAL OR
                                             doc_prod_04        IS NOT INITIAL OR
                                             doc_prod_05        IS NOT INITIAL OR
                                             seqlcto_rfl_01     IS NOT INITIAL OR
                                             seqlcto_rfl_02     IS NOT INITIAL OR
                                             seqlcto_rfl_03     IS NOT INITIAL OR
                                             seqlcto_rco_01     IS NOT INITIAL OR
                                             seqlcto_ent_rco_01 IS NOT INITIAL OR
                                             seqlcto_rco_02     IS NOT INITIAL OR
                                             seqlcto_ent_rco_02 IS NOT INITIAL OR
                                             seqlcto_rco_03     IS NOT INITIAL OR
                                             seqlcto_ent_rco_03 IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_documentos_gerados-msgid
                            msgno = zcx_boletim_producao=>zcx_documentos_gerados-msgno
                            attr1 = CONV #( i_id_boletim )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_documentos_gerados-msgno
          msgid  = zcx_boletim_producao=>zcx_documentos_gerados-msgid
          msgv1  = CONV #( i_id_boletim ).

    ENDLOOP.

    v_key_docs_like = 'BP' && i_id_boletim && '%'.

*-------------------------------------------------------------------------------------------------------------*
*   Validar Lcto ZNFW por Chave do Boletim
*-------------------------------------------------------------------------------------------------------------*

    CLEAR: it_zfiwrt0008[].

    SELECT *
      FROM zfiwrt0008 INTO TABLE it_zfiwrt0008
     WHERE origin_key LIKE v_key_docs_like.

    DELETE it_zfiwrt0008 WHERE ( docs_estornados EQ abap_true ) OR ( loekz EQ abap_true ). "Eliminando documentos estornados/excluidos

    LOOP AT it_zfiwrt0008 INTO DATA(wl_0008).

      CHECK strlen( wl_0008-origin_key ) EQ 25.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_documentos_gerados-msgid
                            msgno = zcx_boletim_producao=>zcx_documentos_gerados-msgno
                            attr1 = CONV #( i_id_boletim )
                            attr2 = CONV #( 'Documento ZNFW:' && wl_0008-seq_lcto )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_documentos_gerados-msgno
          msgid  = zcx_boletim_producao=>zcx_documentos_gerados-msgid
          msgv1  = CONV #( i_id_boletim )
          msgv2  = CONV #( 'Documento ZNFW:' && wl_0008-seq_lcto ).


    ENDLOOP.

*-------------------------------------------------------------------------------------------------------------*
*   Validar Lcto Material por Chave do Boletim
*-------------------------------------------------------------------------------------------------------------*

    CLEAR: it_mkpf[], it_mseg[].

    SELECT *
      FROM mkpf INTO TABLE it_mkpf
     WHERE bktxt LIKE v_key_docs_like.

    IF it_mkpf[] IS NOT INITIAL.

      SELECT *
       FROM mseg INTO TABLE it_mseg
        FOR ALL ENTRIES IN it_mkpf
      WHERE mblnr EQ it_mkpf-mblnr.

    ENDIF.

    LOOP AT it_mkpf INTO DATA(wl_mkpf).

      CHECK strlen( wl_mkpf-bktxt ) EQ 25.

      READ TABLE it_mseg INTO DATA(wl_mseg) WITH KEY mblnr = wl_mkpf-mblnr.

      CHECK ( sy-subrc EQ 0 ) AND ( wl_mseg-smbln IS INITIAL ). "Não é documento de Estorno

      DATA(_lcto_valido) = me->zif_boletim_producao~check_doc_material_valido( i_mblnr  = wl_mkpf-mblnr ).

      IF _lcto_valido EQ abap_true.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_documentos_gerados-msgid
                              msgno = zcx_boletim_producao=>zcx_documentos_gerados-msgno
                              attr1 = CONV #( i_id_boletim )
                              attr2 = CONV #( 'Documento Material:' && wl_mkpf-mblnr )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_documentos_gerados-msgno
            msgid  = zcx_boletim_producao=>zcx_documentos_gerados-msgid
            msgv1  = CONV #( i_id_boletim )
            msgv2  = CONV #( 'Documento Material:' && wl_mkpf-mblnr ).
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD zif_boletim_producao~check_documentos_vinculados.

    IF i_id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO @DATA(wl_zsdt0252)
     WHERE id_boletim EQ @i_id_boletim.

    CHECK sy-subrc EQ 0.

    RAISE EXCEPTION TYPE zcx_boletim_producao
      EXPORTING
        textid = VALUE #( msgid = zcx_boletim_producao=>zcx_documentos_vinculados-msgid
                          msgno = zcx_boletim_producao=>zcx_documentos_vinculados-msgno
                          attr1 = CONV #( i_id_boletim )
                         )
        msgty  = 'E'
        msgno  = zcx_boletim_producao=>zcx_documentos_vinculados-msgno
        msgid  = zcx_boletim_producao=>zcx_documentos_vinculados-msgid
        msgv1  = CONV #( i_id_boletim ).


  ENDMETHOD.


  METHOD zif_boletim_producao~check_doc_material_valido.

    r_valido = abap_false.

    IF i_mblnr IS NOT INITIAL.

      SELECT SINGLE *
        FROM mkpf INTO @DATA(wl_mkpf)
       WHERE mblnr EQ @i_mblnr.

      IF sy-subrc EQ 0.

        SELECT SINGLE *
          FROM mseg INTO @DATA(wl_mseg_estorno)
         WHERE smbln EQ @i_mblnr.

        IF sy-subrc NE 0.
          r_valido = abap_true.
        ENDIF.

      ENDIF.

    ENDIF.

    IF ( r_valido EQ abap_true ) AND ( strlen( wl_mkpf-bktxt ) EQ 25 ) AND ( wl_mkpf-bktxt(2) EQ 'BP' ). "Documento Boletim Produção

      TRY.
          DATA(_valida_mat) = me->zif_boletim_producao~validar_materiais_doc_prod( i_mblnr = i_mblnr ).
        CATCH zcx_boletim_producao.
          CLEAR: _valida_mat.
      ENDTRY.

      IF _valida_mat EQ abap_false.
        r_valido = abap_false.
      ENDIF.
    ENDIF.

    IF ( r_valido EQ abap_false ) AND ( i_with_raise EQ abap_true ) AND ( i_ds_doc IS NOT INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                            attr1 = CONV #( i_ds_doc )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
          msgv1  = CONV #( i_ds_doc ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_boletim_producao~check_estoque_produtos.

    DATA: lv_saldo   TYPE mchb-clabs,
          lv_saldo_a TYPE mchb-clabs,
          lv_charg_a TYPE mchb-charg.

    LOOP AT i_itens INTO DATA(wa_goodsmovements).

      CHECK wa_goodsmovements-move_type EQ '261'.

      CLEAR: lv_saldo.

      IF wa_goodsmovements-batch IS NOT INITIAL.

        SELECT SINGLE clabs
          FROM mchb INTO lv_saldo
         WHERE matnr = wa_goodsmovements-material
           AND werks = wa_goodsmovements-plant
           AND lgort = wa_goodsmovements-stge_loc
           AND charg = wa_goodsmovements-batch.

        SELECT SINGLE *
          FROM zsdt_depara_cen INTO @DATA(lwa_depara_centro)
         WHERE centro_real       EQ @wa_goodsmovements-plant
           AND tp_centro_virtual EQ @zcl_pedido_compra=>st_tp_centro_a_fixar.

        IF ( sy-subrc EQ 0 ) AND ( lwa_depara_centro-centrov_1 IS NOT INITIAL ).
          CONCATENATE wa_goodsmovements-batch '_' wa_goodsmovements-plant INTO lv_charg_a.

          SELECT SINGLE clabs
            FROM mchb INTO lv_saldo_a
           WHERE matnr = wa_goodsmovements-material
             AND werks = lwa_depara_centro-centrov_1
             AND lgort = wa_goodsmovements-stge_loc
             AND charg = lv_charg_a.

          IF sy-subrc EQ 0.
            ADD lv_saldo_a TO lv_saldo.
          ENDIF.

        ENDIF.

      ELSE.

        SELECT SINGLE labst
          FROM mard INTO lv_saldo
         WHERE matnr = wa_goodsmovements-material
           AND werks = wa_goodsmovements-plant
           AND lgort = wa_goodsmovements-stge_loc.

      ENDIF.

      IF abs( wa_goodsmovements-entry_qnt ) > lv_saldo.

        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_estoque_not_found-msgid
                              msgno = zcx_boletim_producao=>zcx_estoque_not_found-msgno
                              attr1 = CONV #( wa_goodsmovements-material )
                              attr2 = CONV #( wa_goodsmovements-plant    )
                              attr3 = CONV #( wa_goodsmovements-stge_loc )
                              attr4 = CONV #( wa_goodsmovements-batch    )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_estoque_not_found-msgno
            msgid  = zcx_boletim_producao=>zcx_estoque_not_found-msgid
            msgv1  = CONV #( wa_goodsmovements-material )
            msgv2  = CONV #( wa_goodsmovements-plant )
            msgv3  = CONV #( wa_goodsmovements-stge_loc )
            msgv4  = CONV #( wa_goodsmovements-batch ).

      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD zif_boletim_producao~check_permissao_modificacao.

    DATA: v_seq_lcto_znfw TYPE zfiwrt0008-seq_lcto.

    DATA: v_doc_check           TYPE numc10,
          v_doc_subsequente     TYPE numc10,

          v_key_doc_check       TYPE zsdt0252-key_docs,
          v_ordem_doc_check_str TYPE numc2,

          v_key_doc_aux         TYPE zsdt0252-key_docs,

          v_ordem_doc_aux_int   TYPE i,
          v_ordem_doc_aux_str   TYPE numc2.


    CLEAR: v_key_doc_check.

    IF ( i_seq_lcto_znfw IS INITIAL ) AND ( i_mblnr IS INITIAL ) AND ( i_id_boletim IS INITIAL ) AND ( i_docnum IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Documento para validação' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Documento para validação' ).
    ENDIF.

    IF i_seq_lcto_znfw IS NOT INITIAL.
      v_seq_lcto_znfw = i_seq_lcto_znfw.
    ELSEIF i_docnum IS NOT INITIAL.

      SELECT SINGLE *
        FROM j_1bnflin INTO @DATA(wl_lin)
       WHERE docnum EQ @i_docnum.

      CHECK ( wl_lin-reftyp = 'ZW' ) AND ( wl_lin-refkey IS NOT INITIAL ).

      v_seq_lcto_znfw = wl_lin-refkey.

    ENDIF.

*---------------------------------------------------------------------------------------------------------------*
*   Validações para Documento ZNFW
*---------------------------------------------------------------------------------------------------------------*
    IF v_seq_lcto_znfw IS NOT INITIAL.

      SELECT SINGLE *
        FROM zfiwrt0008 INTO @DATA(wl_zfiwrt0008_check)
       WHERE seq_lcto EQ @v_seq_lcto_znfw.

      CHECK sy-subrc EQ 0.

      "Check se já foi feito devolução da remessa para industrialização
      IF ( wl_zfiwrt0008_check-docnum_saida IS NOT INITIAL ).

        SELECT SINGLE *
          FROM zsdt0251 INTO @DATA(wl_zsdt0251)
         WHERE docnum EQ @wl_zfiwrt0008_check-docnum_saida.

        IF ( sy-subrc EQ 0 ) AND ( wl_zsdt0251-qtde_utilizada > 0 ).
          SELECT SINGLE *
            FROM zsdt0249 INTO @DATA(wl_zsdt0249)
           WHERE docnum EQ @wl_zfiwrt0008_check-docnum_saida.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                                msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                                attr1 = CONV #( 'Boletim Produção Id:' && wl_zsdt0249-id_boletim )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
              msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
              msgv1  = CONV #( 'Boletim Produção Id:' && wl_zsdt0249-id_boletim ).
        ENDIF.

        RETURN.

      ENDIF.

      CHECK ( strlen( wl_zfiwrt0008_check-origin_key ) EQ 25   ).
      CHECK ( wl_zfiwrt0008_check-origin_key(2)        EQ 'BP' ). "Documento Boletim Produção

      v_key_doc_check = wl_zfiwrt0008_check-origin_key.

    ENDIF.

*---------------------------------------------------------------------------------------------------------------*
*   Validações para Documento Produção
*---------------------------------------------------------------------------------------------------------------*
    IF i_mblnr IS NOT INITIAL.

      SELECT SINGLE *
        FROM mkpf INTO @DATA(wl_mkpf)
       WHERE mblnr EQ @i_mblnr.

      CHECK ( sy-subrc EQ 0 ).

      CHECK ( strlen( wl_mkpf-bktxt ) EQ 25   ).
      CHECK ( wl_mkpf-bktxt(2)        EQ 'BP' ). "Documento Boletim Produção

      v_key_doc_check = wl_mkpf-bktxt.

    ENDIF.

    IF ( ( v_seq_lcto_znfw IS NOT INITIAL ) OR ( i_mblnr IS NOT INITIAL ) ) AND ( v_key_doc_check IS NOT INITIAL ) .

      CLEAR: v_doc_check.
      IF v_seq_lcto_znfw IS NOT INITIAL.
        v_doc_check = v_seq_lcto_znfw.
      ELSEIF ( i_mblnr IS NOT INITIAL ).
        v_doc_check = i_mblnr.
      ENDIF.

      v_ordem_doc_check_str = v_key_doc_check+23(02).

      v_ordem_doc_aux_int   = v_ordem_doc_check_str.

      WHILE v_ordem_doc_aux_int LT 18.

        ADD 1 TO v_ordem_doc_aux_int.

        v_ordem_doc_aux_str = v_ordem_doc_aux_int.

        v_key_doc_aux = v_key_doc_check(23) && v_ordem_doc_aux_str.

        CASE v_ordem_doc_check_str.
          WHEN zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rfl_01.
            CHECK ( v_ordem_doc_aux_str NE zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rfl_02 ) AND
                  ( v_ordem_doc_aux_str NE zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rfl_03 ).
          WHEN zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rfl_02.
            CHECK ( v_ordem_doc_aux_str NE zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rfl_03 ).
          WHEN zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rco_01 OR zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_ent_rco_01 .
            CHECK ( v_ordem_doc_aux_str NE zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rco_02     ) AND
                  ( v_ordem_doc_aux_str NE zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_ent_rco_02 ) AND
                  ( v_ordem_doc_aux_str NE zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rco_03     ) AND
                  ( v_ordem_doc_aux_str NE zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_ent_rco_03 ).
          WHEN zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rco_02 OR zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_ent_rco_02.
            CHECK ( v_ordem_doc_aux_str NE zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rco_03     ) AND
                  ( v_ordem_doc_aux_str NE zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_ent_rco_03 ).
        ENDCASE.

        CLEAR: v_doc_subsequente.

        me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( v_key_doc_aux )
                                                                           IMPORTING e_seq_lcto_znfw = DATA(e_seq_lcto_znfw)
                                                                                     e_mblnr         = DATA(e_mblnr)  ).
        IF ( e_seq_lcto_znfw IS NOT INITIAL ).
          v_doc_subsequente = e_seq_lcto_znfw.
        ELSEIF ( e_mblnr IS NOT INITIAL ).
          v_doc_subsequente = e_mblnr.
        ENDIF.

        IF ( v_doc_subsequente IS NOT INITIAL ).

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_documentos_subsequentes-msgid
                                msgno = zcx_boletim_producao=>zcx_documentos_subsequentes-msgno
                                attr1 = CONV #( v_doc_subsequente     )
                                attr2 = CONV #( v_doc_check           )
                                attr3 = CONV #( v_key_doc_check+2(10) )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_documentos_subsequentes-msgno
              msgid  = zcx_boletim_producao=>zcx_documentos_subsequentes-msgid
              msgv1  = CONV #( v_doc_subsequente )
              msgv2  = CONV #( v_doc_check )
              msgv3  = CONV #( v_key_doc_check+2(10) ).

        ENDIF.


      ENDWHILE.


    ENDIF.

*---------------------------------------------------------------------------------------------------------------*
*   Validação Boletim
*---------------------------------------------------------------------------------------------------------------*
    IF i_id_boletim IS NOT INITIAL.

      SELECT SINGLE *
        FROM zsdt0246 INTO @DATA(wl_0246)
       WHERE id_boletim EQ @i_id_boletim.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                              msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                              attr1 = CONV #( 'Documento Boletim Id:' && i_id_boletim )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
            msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
            msgv1  = CONV #( 'Documento Boletim Id:' && i_id_boletim ).
      ENDIF.

      "================================================================================="CS2020000694 Upgrade transação ZSDT0170 - ABAP AOENNING.
*      IF wl_0246-status EQ 'P'.
*        RAISE EXCEPTION TYPE zcx_boletim_producao
*          EXPORTING
*            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_aprovado-msgid
*                              msgno = zcx_boletim_producao=>zcx_bol_aprovado-msgno
*                             )
*            msgty  = 'E'
*            msgno  = zcx_boletim_producao=>zcx_bol_aprovado-msgno
*            msgid  = zcx_boletim_producao=>zcx_bol_aprovado-msgid.
*      ELSEIF sy-ucomm EQ 'EDIT_BOL' AND wl_0246-aprovado EQ abap_true.
*        RAISE EXCEPTION TYPE zcx_boletim_producao
*          EXPORTING
*            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_aprovado-msgid
*                              msgno = zcx_boletim_producao=>zcx_bol_aprovado-msgno
*                             )
*            msgty  = 'E'
*            msgno  = zcx_boletim_producao=>zcx_bol_aprovado-msgno
*            msgid  = zcx_boletim_producao=>zcx_bol_aprovado-msgid.
*      ENDIF.
      "================================================================================="CS2020000694 Upgrade transação ZSDT0170 - ABAP AOENNING.
      CASE wl_0246-com_nf.
        WHEN abap_true.
          me->zif_boletim_producao~check_documentos_vinculados( i_id_boletim = wl_0246-id_boletim ).
        WHEN abap_false.
          me->zif_boletim_producao~check_documentos_gerados( i_id_boletim = wl_0246-id_boletim ).
      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD zif_boletim_producao~check_status_boletim.

    CLEAR: r_status, e_change_status.

    SELECT SINGLE *
      FROM zsdt0246 INTO @DATA(wl_zsdt0246)
     WHERE id_boletim EQ @i_id_boletim_producao.

    CHECK ( sy-subrc EQ 0 ) AND ( i_id_boletim_producao IS NOT INITIAL ).

    CASE wl_zsdt0246-status.
      WHEN 'F'.

        TRY.
            wl_zsdt0246-status = me->zif_boletim_producao~get_status_boletim( EXPORTING i_id_boletim_producao = wl_zsdt0246-id_boletim ).
          CATCH zcx_boletim_producao.
        ENDTRY.

        IF wl_zsdt0246-status EQ 'A'.
          MESSAGE 'Boletim de Produção será reaberto!' TYPE 'I'.

          UPDATE zsdt0246 SET status = wl_zsdt0246-status
           WHERE id_boletim EQ wl_zsdt0246-id_boletim.

          MESSAGE 'Status do Boletim de Produção atualizado com sucesso!' TYPE 'S'.

          COMMIT WORK.

          e_change_status = abap_true.

        ENDIF.

      WHEN 'A' OR space.

        TRY.
            wl_zsdt0246-status = me->zif_boletim_producao~get_status_boletim( EXPORTING i_id_boletim_producao = wl_zsdt0246-id_boletim ).
          CATCH zcx_boletim_producao.
        ENDTRY.

        IF wl_zsdt0246-status EQ 'F'.
          MESSAGE 'Boletim de Produção será finalizado!' TYPE 'I'.

          UPDATE zsdt0246 SET status = wl_zsdt0246-status
           WHERE id_boletim EQ wl_zsdt0246-id_boletim.

          MESSAGE 'Status do Boletim de Produção atualizado com sucesso!' TYPE 'S'.

          COMMIT WORK.

          e_change_status = abap_true.

        ENDIF.

    ENDCASE.

    r_status = wl_zsdt0246-status.


  ENDMETHOD.


  METHOD zif_boletim_producao~deletar_registro.

    DATA: var_answer TYPE c.

    r_if_boletim_producao = me.

    IF i_id_boletim_producao IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    me->zif_boletim_producao~check_permissao_modificacao( i_id_boletim = i_id_boletim_producao ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja realmente excluir o Lançamento?'
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

    UPDATE zsdt0246 SET loekz = abap_true WHERE id_boletim EQ i_id_boletim_producao.
    IF sy-subrc EQ 0.
      MESSAGE s006.
    ENDIF.


  ENDMETHOD.


  METHOD zif_boletim_producao~desaprovar_lancamento.
    DATA: var_answer TYPE c.

    IF i_id_boletim_producao IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0246 INTO @DATA(wl_0246)
     WHERE id_boletim EQ @i_id_boletim_producao.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Documento Boletim Id:' && i_id_boletim_producao )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Documento Boletim Id:' && i_id_boletim_producao ).
    ENDIF.

    IF wl_0246-aprovado NE abap_true.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_aprovado-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_aprovado-msgno
                            attr1 = CONV #( 'Documento Boletim Id:' && i_id_boletim_producao && ' não esta aprovado' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_aprovado-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_aprovado-msgid.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja realmente desaprovar o Lançamento?'
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

    wl_0246-us_aprovacao = sy-uname.
    wl_0246-dt_aprovacao = sy-datum.
    wl_0246-hr_aprovacao = sy-uzeit.
    wl_0246-aprovado     = abap_false.

    MODIFY zsdt0246 FROM wl_0246.
    COMMIT WORK.

    MESSAGE s041.

  ENDMETHOD.


  METHOD zif_boletim_producao~desbloquear_registros.

    IF i_id_boletim IS NOT INITIAL.

      CALL FUNCTION 'DEQUEUE_EZSDT0246'
        EXPORTING
          id_boletim = i_id_boletim.

    ENDIF.

    IF i_docnum_zsdt0251 IS NOT INITIAL.

      CALL FUNCTION 'DEQUEUE_EZSDT0251'
        EXPORTING
          docnum = i_docnum_zsdt0251.

    ENDIF.

  ENDMETHOD.


  METHOD zif_boletim_producao~desvincular_nf.

    DATA: v_qtde_excedida   TYPE zsdt0251-qtde_saldo,
          v_total_vinculado TYPE zsdt0251-qtde_saldo,
          v_qtde_boletim    TYPE zsdt0251-qtde_saldo,
          v_qtde_vinc       TYPE zsdt0251-qtde_saldo.

    DATA: it_0252     TYPE TABLE OF zsdt0252,
          wl_0252     TYPE zsdt0252,
          wl_0249     TYPE zsdt0249,
          wl_0249_del TYPE zsdt0249,
          wl_0251     TYPE zsdt0251.

    r_if_boletim_producao = me.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    me->zif_boletim_producao~check_documentos_gerados( i_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim ).

    CLEAR: v_qtde_boletim.
    LOOP AT me->zif_boletim_producao~at_dados_producao INTO DATA(wl_dados_prod).
      ADD wl_dados_prod-qtde_consumo TO v_qtde_boletim.
    ENDLOOP.

    LOOP AT i_notas INTO DATA(wl_nota_vinc).

      IF wl_nota_vinc-docnum IS INITIAL.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                              msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                              attr1 = CONV #( 'Documento Fiscal' )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
            msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
            msgv1  = CONV #( 'Documento Fiscal' ).
      ENDIF.

*---------------------------------------------------------------------------------------------------------*
*   Verificar se NF esta vinculada no Boletim
*---------------------------------------------------------------------------------------------------------*
      SELECT SINGLE *
        FROM zsdt0249 INTO wl_0249
       WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim
         AND docnum     EQ wl_nota_vinc-docnum.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_nf_nao_vinculada_boletim-msgid
                              msgno = zcx_boletim_producao=>zcx_nf_nao_vinculada_boletim-msgno
                              attr1 = CONV #( wl_nota_vinc-docnum )
                              attr2 = CONV #( me->zif_boletim_producao~at_cabecalho-id_boletim )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_nf_nao_vinculada_boletim-msgno
            msgid  = zcx_boletim_producao=>zcx_nf_nao_vinculada_boletim-msgid
            msgv1  = CONV #( wl_nota_vinc-docnum )
            msgv2  = CONV #( me->zif_boletim_producao~at_cabecalho-id_boletim ).
      ENDIF.

      wl_0249_del = wl_0249.

*---------------------------------------------------------------------------------------------------------*
*   Get Tabela de Saldo Vinculado NF
*---------------------------------------------------------------------------------------------------------*
      SELECT SINGLE *
        FROM zsdt0251 INTO wl_0251
       WHERE docnum EQ wl_nota_vinc-docnum.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_control_saldo_nf_not_found-msgid
                              msgno = zcx_boletim_producao=>zcx_control_saldo_nf_not_found-msgno
                              attr1 = CONV #( wl_nota_vinc-docnum )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_control_saldo_nf_not_found-msgno
            msgid  = zcx_boletim_producao=>zcx_control_saldo_nf_not_found-msgid
            msgv1  = CONV #( wl_nota_vinc-docnum ).
      ENDIF.

      wl_0251-open_change = abap_true.
      MODIFY zsdt0251 FROM wl_0251.

*---------------------------------------------------------------------------------------------------------*
*     Bloqueio Registro
*---------------------------------------------------------------------------------------------------------*

      me->zif_boletim_producao~bloquear_registros( i_docnum_zsdt0251 = wl_nota_vinc-docnum ).

*---------------------------------------------------------------------------------------------------------*
*   Atualizar Tabelas Controle de Saldo
*---------------------------------------------------------------------------------------------------------*

      "Deletar Registro vinculação NF
      DELETE FROM zsdt0249 WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim
                             AND docnum     EQ wl_nota_vinc-docnum.

      CLEAR: v_total_vinculado.

      SELECT SUM( qtde_vinc )
        FROM zsdt0249 INTO v_total_vinculado
       WHERE docnum EQ wl_nota_vinc-docnum.

      wl_0251-qtde_utilizada = v_total_vinculado.
      wl_0251-qtde_saldo     = wl_0251-qtde_total - v_total_vinculado.
      MODIFY zsdt0251 FROM wl_0251.

*--------------------------------------------------------------------------------------------------------*
*   Check se depois da desvinculação, possui NF vinculada para a Filial/Lote
*--------------------------------------------------------------------------------------------------------*
      SELECT SINGLE *
        FROM zsdt0249 INTO wl_0249
       WHERE id_boletim EQ wl_0249_del-id_boletim
         AND branch     EQ wl_0249_del-branch
         AND charg      EQ wl_0249_del-charg
         AND id_agrp    EQ wl_0249_del-id_agrp.

      IF sy-subrc NE 0." Se não possuir, deletar tabela de documentos gerados...
        DELETE FROM zsdt0252
         WHERE id_boletim EQ wl_0249_del-id_boletim
           AND branch     EQ wl_0249_del-branch
           AND charg      EQ wl_0249_del-charg
           AND id_agrp    EQ wl_0249_del-id_agrp.
      ELSE.

        SELECT SINGLE *
          FROM zsdt0252 INTO wl_0252
         WHERE id_boletim EQ wl_0249_del-id_boletim
           AND branch     EQ wl_0249_del-branch
           AND charg      EQ wl_0249_del-charg
           AND id_agrp    EQ wl_0249_del-id_agrp.

        IF sy-subrc EQ 0.

          CLEAR: v_qtde_vinc.
          SELECT SUM( qtde_vinc )
            FROM zsdt0249 INTO v_qtde_vinc
           WHERE id_boletim EQ wl_0249_del-id_boletim
             AND branch     EQ wl_0249_del-branch
             AND charg      EQ wl_0249_del-charg
             AND id_agrp    EQ wl_0249_del-id_agrp.

          wl_0252-qtde_vinc        = v_qtde_vinc.

          MODIFY zsdt0252 FROM wl_0252.

        ENDIF.

      ENDIF.

      wl_0251-open_change = abap_false.
      MODIFY zsdt0251 FROM wl_0251.

      COMMIT WORK.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_boletim_producao~estorno_doc_producao.

    DATA: wl_confirmation_es TYPE bapi_rm_datkey-cancconfirmation,
          es_confirmation    TYPE bapi_rm_datkey-confirmation,
          wa_return          TYPE bapiret2.

    CLEAR: r_mblnr_estorno, wl_confirmation_es, es_confirmation, wa_return.

*    me->zif_boletim_producao~check_desaprovacao( i_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim  ).
    "Verificar se boletim esta aprovado.
    SELECT SINGLE *
          FROM zsdt0246 INTO @DATA(wl_0246)
         WHERE id_boletim EQ @me->zif_boletim_producao~at_cabecalho-id_boletim.

    IF wl_0246-aprovado EQ abap_true.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_aprovado-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_aprovado-msgno
                            attr1 = CONV #( 'Id. Boletim' && me->zif_boletim_producao~at_cabecalho-id_boletim )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_aprovado-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_aprovado-msgid
          msgv1  = CONV #( 'Documento Boletim Id:' && me->zif_boletim_producao~at_cabecalho-id_boletim ).
    ENDIF.

    CHECK i_mblnr IS NOT INITIAL.

    me->zif_boletim_producao~check_permissao_modificacao( EXPORTING i_mblnr = i_mblnr ).

    SELECT SINGLE *
      FROM blpp INTO @DATA(wl_blpp)
     WHERE belnr EQ @i_mblnr.

    IF sy-subrc NE 0.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Doc. Confirmação' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Doc. Confirmação' ).

    ENDIF.


    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Estornando Documento { i_mblnr } ...|.


    es_confirmation = wl_blpp-prtnr.

    CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
      EXPORTING
        confirmation     = es_confirmation
        postdate         = zif_boletim_producao~at_date
      IMPORTING
        cancconfirmation = wl_confirmation_es
        return           = wa_return.

    IF wa_return-type = 'E'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      zcl_boletim_producao=>zif_boletim_producao~gera_erro_geral(
        EXPORTING
          i_msgv1              =   CONV #( wa_return-message_v1 )
          i_msgv2              =   CONV #( wa_return-message_v2 )
          i_msgv3              =   CONV #( wa_return-message_v3 )
          i_msgv4              =   CONV #( wa_return-message_v4 )
          i_msgid              =   CONV #( wa_return-id )
          i_msgno              =   CONV #( wa_return-number )  ).

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      DO 100 TIMES.

        SELECT SINGLE *
          FROM blpp INTO @DATA(gs_blpp)
         WHERE prtnr = @wl_confirmation_es
           AND prtps = '0001'.

        IF sy-subrc = 0.

          COMMIT WORK AND WAIT .

          r_mblnr_estorno = gs_blpp-belnr.

          MESSAGE |Documento de Estorno: { gs_blpp-belnr } gerado com sucesso!| TYPE 'S'.

          EXIT.

        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.

      ENDDO.

    ENDIF.



  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_doc_producao_01.



  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_doc_producao_02.



  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_doc_producao_03.



  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_doc_producao_04.


  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_doc_producao_05.




  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_devolucao.

    DATA: wl_zfiwrt0008 TYPE zfiwrt0008,
          wl_zfiwrt0009 TYPE zfiwrt0009,
          wl_zsdt0252   TYPE zsdt0252,
          wl_zsdt0253   TYPE zsdt0253.

    DATA: v_netwr          TYPE zfiwrt0009-netwr.

    DATA: it_zsdt0249 TYPE TABLE OF zsdt0249.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_devolucao.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    me->zif_boletim_producao~check_aprovacao( i_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim  ).

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    zcl_boletim_producao=>zif_boletim_producao~get_saldos( EXPORTING i_id_boletim     = me->zif_boletim_producao~at_cabecalho-id_boletim
                                                           IMPORTING e_saldo_vincular = DATA(_saldo_vincular) ).

    IF _saldo_vincular NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_boletim_com_saldo-msgid
                            msgno = zcx_boletim_producao=>zcx_boletim_com_saldo-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_boletim_com_saldo-msgno
          msgid  = zcx_boletim_producao=>zcx_boletim_com_saldo-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_devol IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Operação Dev. ZNFW (ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Operação Dev. ZNFW (ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_boletim)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF ( sy-subrc NE 0 ) OR ( me->zif_boletim_producao~at_cabecalho-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-produto_rem_ind IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Produto Remessa Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Produto Remessa Industrialização' ).
    ENDIF.

    me->zif_boletim_producao~gerar_rateio_produtos( ).

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ i_zsdt0252-id_agrp.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_devol IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF Devolução' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF Devolução' ).
    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_devol = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF Devolução' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF Devolução' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Documentos Referenciados
    CLEAR: it_zsdt0249[].
    SELECT *
      FROM zsdt0249 INTO TABLE it_zsdt0249
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ i_zsdt0252-id_agrp.

    IF it_zsdt0249[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Documentos Referenciados' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Documentos Referenciados' ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF Devolução ( Filial: { wl_zsdt0252-branch } )|.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_devol.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_boletim-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_boletim-branch.
    wl_zfiwrt0008-parid           = i_zsdt0252-branch.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = sy-datum.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.

*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = me->zif_boletim_producao~at_cabecalho-produto_rem_ind.
    wl_zfiwrt0009-bwkey  = me->zif_boletim_producao~at_cabecalho-branch.

    LOOP AT it_zsdt0249 INTO DATA(wl_zsdt0249).

      SELECT SINGLE *
        FROM j_1bnflin INTO @DATA(wl_lin)
       WHERE docnum EQ @wl_zsdt0249-docnum.

      IF ( sy-subrc EQ 0 ) AND ( wl_lin-netwr > 0 ) AND ( wl_lin-netpr > 0 ).

        ADD wl_zsdt0249-qtde_vinc TO wl_zfiwrt0009-menge.

        CLEAR: v_netwr.
        v_netwr = ( wl_zsdt0249-qtde_vinc * wl_lin-netpr ).

        ADD v_netwr TO wl_zfiwrt0009-netwr.

      ELSE.

        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                              msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                              attr1 = CONV #( |Qtde/Valor documento: { wl_zsdt0249-docnum } | )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
            msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
            msgv1  = CONV #( |Qtde/Valor documento: { wl_zsdt0249-docnum } | ).

      ENDIF.

    ENDLOOP.

    wl_zfiwrt0009-netpr  =  wl_zfiwrt0009-netwr / wl_zfiwrt0009-menge.

    SELECT SINGLE *
      FROM mara INTO @DATA(wl_mara)
     WHERE matnr EQ @wl_zfiwrt0009-matnr.

    IF ( sy-subrc EQ 0 ).
      wl_zfiwrt0009-meins = wl_mara-meins.

      IF ( wl_mara-xchpf = abap_true ).
        wl_zfiwrt0009-charg = wl_lin-charg.
      ENDIF.
    ENDIF.

*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
    LOOP AT it_zsdt0249 INTO wl_zsdt0249.
      zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wl_zsdt0249-docnum ).
    ENDLOOP.

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Devolução' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Devolução' ).
    ENDIF.

    wl_zsdt0252-seqlcto_devol = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Devolução' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Devolução' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.

  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_ent_devolucao.

    DATA: wl_zfiwrt0008 TYPE zfiwrt0008,
          wl_zfiwrt0009 TYPE zfiwrt0009,
          wl_zsdt0252   TYPE zsdt0252,
          wl_zsdt0253   TYPE zsdt0253.

    DATA: wl_zfiwrt0008_dev TYPE zfiwrt0008,
          t_zfiwrt0009_dev  TYPE zfiwrt0009_t,
          wl_active_dev     TYPE j_1bnfe_active,
          wl_doc_dev        TYPE j_1bnfdoc.

    DATA: it_zsdt0249 TYPE TABLE OF zsdt0249.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_ent_devolucao.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_ent_devol IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Opr.Ent.Dev.ZNFW(ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Opr.Ent.Dev.ZNFW(ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_origem)
     WHERE branch EQ @i_zsdt0252-branch.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial Origem' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial Origem' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_boletim)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF ( sy-subrc NE 0 ) OR ( me->zif_boletim_producao~at_cabecalho-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-produto_rem_ind IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Produto Remessa Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Produto Remessa Industrialização' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ i_zsdt0252-id_agrp.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_ent_dev IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF Entrada Devolução' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF Entrada Devolução' ).

    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_ent_dev = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF Entrada Devolução' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF Entrada Devolução' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Validar Lançamento Devolução
    DATA(_valida_lcto_devol) = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto    = wl_zsdt0252-seqlcto_devol
                                                                            IMPORTING e_zfiwrt0008  = wl_zfiwrt0008_dev
                                                                                      e_zfiwrt0009  = t_zfiwrt0009_dev
                                                                                      e_active      = wl_active_dev
                                                                                      e_doc         = wl_doc_dev ).
    IF _valida_lcto_devol EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                            attr1 = CONV #( 'NF Devolução' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
          msgv1  = CONV #( 'NF Devolução' ).
    ENDIF.

    READ TABLE t_zfiwrt0009_dev INTO DATA(wl_zfiwrt0009_dev) INDEX 1.

    "Documentos Referenciados
    CLEAR: it_zsdt0249[].
    SELECT *
      FROM zsdt0249 INTO TABLE it_zsdt0249
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ i_zsdt0252-id_agrp.

    IF it_zsdt0249[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Documentos Referenciados' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Documentos Referenciados' ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF Ent. Devolução ( Filial: { wl_zsdt0252-branch } )|.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_ent_devol.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_origem-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_origem-branch.
    wl_zfiwrt0008-parid           = wl_branch_boletim-branch.
    wl_zfiwrt0008-nfenum          = wl_active_dev-nfnum9.
    wl_zfiwrt0008-series          = wl_active_dev-serie.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = wl_zfiwrt0008_dev-bldat.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.
    wl_zfiwrt0008-not_check_xml   = abap_true.
    wl_zfiwrt0008-access_key      = wl_active_dev-regio  && wl_active_dev-nfyear  && wl_active_dev-nfmonth &&
                                    wl_active_dev-stcd1  && wl_active_dev-model   && wl_active_dev-serie   &&
                                    wl_active_dev-nfnum9 && wl_active_dev-docnum9 && wl_active_dev-cdv.

*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = wl_zfiwrt0009_dev-matnr.
    wl_zfiwrt0009-bwkey  = wl_branch_origem-branch.
    wl_zfiwrt0009-menge  = wl_zfiwrt0009_dev-menge.
    wl_zfiwrt0009-netpr  = wl_zfiwrt0009_dev-netpr.
    wl_zfiwrt0009-netwr  = wl_zfiwrt0009_dev-netwr.
    wl_zfiwrt0009-meins  = wl_zfiwrt0009_dev-meins.
    wl_zfiwrt0009-charg  = wl_zfiwrt0009_dev-charg.

*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
    LOOP AT it_zsdt0249 INTO DATA(wl_zsdt0249).
      zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wl_zsdt0249-docnum ).
    ENDLOOP.

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Entrada Devolução' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Entrada Devolução' ).
    ENDIF.

    wl_zsdt0252-seqlcto_ent_dev = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Entrada Devolução' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Entrada Devolução' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.

  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_ent_industrializacao.

    DATA: wl_zfiwrt0008 TYPE zfiwrt0008,
          wl_zfiwrt0009 TYPE zfiwrt0009,
          wl_zsdt0252   TYPE zsdt0252,
          wl_zsdt0253   TYPE zsdt0253.

    DATA: wl_zfiwrt0008_ind TYPE zfiwrt0008,
          t_zfiwrt0009_ind  TYPE zfiwrt0009_t,
          t_zfiwrt0020_ind  TYPE zfiwrt0020_t,
          wl_active_ind     TYPE j_1bnfe_active,
          wl_doc_ind        TYPE j_1bnfdoc.

    DATA: it_zsdt0249 TYPE TABLE OF zsdt0249.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_ent_ind.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_ent_ind IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Opr.Ent.Ind.ZNFW(ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Opr.Ent.Ind.ZNFW(ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_origem)
     WHERE branch EQ @i_zsdt0252-branch.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial Origem' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial Origem' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_boletim)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF ( sy-subrc NE 0 ) OR ( me->zif_boletim_producao~at_cabecalho-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-produto_rem_ind IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Produto Remessa Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Produto Remessa Industrialização' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ i_zsdt0252-id_agrp.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_ent_ind IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF Entrada Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF Entrada Industrialização' ).

    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_ent_ind = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF Entrada Industrialização' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF Entrada Industrialização' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Validar Lançamento Industrialização
    DATA(_valida_lcto_devol) = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto    = wl_zsdt0252-seqlcto_ind
                                                                            IMPORTING e_zfiwrt0008  = wl_zfiwrt0008_ind
                                                                                      e_zfiwrt0009  = t_zfiwrt0009_ind
                                                                                      e_zfiwrt0020  = t_zfiwrt0020_ind
                                                                                      e_active      = wl_active_ind
                                                                                      e_doc         = wl_doc_ind ).
    IF _valida_lcto_devol EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                            attr1 = CONV #( 'NF Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
          msgv1  = CONV #( 'NF Industrialização' ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF Ent.Industrialização ( Filial: { wl_zsdt0252-branch } )|.

    READ TABLE t_zfiwrt0009_ind INTO DATA(wl_zfiwrt0009_ind) INDEX 1.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_ent_ind.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_origem-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_origem-branch.
    wl_zfiwrt0008-parid           = wl_branch_boletim-branch.
    wl_zfiwrt0008-nfenum          = wl_active_ind-nfnum9.
    wl_zfiwrt0008-series          = wl_active_ind-serie.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = wl_zfiwrt0008_ind-bldat.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.
    wl_zfiwrt0008-not_check_xml   = abap_true.
    wl_zfiwrt0008-access_key      = wl_active_ind-regio  && wl_active_ind-nfyear  && wl_active_ind-nfmonth &&
                                    wl_active_ind-stcd1  && wl_active_ind-model   && wl_active_ind-serie   &&
                                    wl_active_ind-nfnum9 && wl_active_ind-docnum9 && wl_active_ind-cdv.

*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = wl_zfiwrt0009_ind-matnr.
    wl_zfiwrt0009-bwkey  = wl_branch_origem-branch.
    wl_zfiwrt0009-menge  = wl_zfiwrt0009_ind-menge.
    wl_zfiwrt0009-netpr  = wl_zfiwrt0009_ind-netpr.
    wl_zfiwrt0009-netwr  = wl_zfiwrt0009_ind-netwr.
    wl_zfiwrt0009-meins  = wl_zfiwrt0009_ind-meins.
    wl_zfiwrt0009-charg  = wl_zfiwrt0009_ind-charg.

*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
    LOOP AT t_zfiwrt0020_ind INTO DATA(wl_0020_ind).
      zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wl_0020_ind-docnum ).
    ENDLOOP.

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Entrada Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Entrada Industrialização' ).
    ENDIF.

    wl_zsdt0252-seqlcto_ent_ind = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Entrada Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Entrada Industrialização' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.



  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_ent_rco_01.

    DATA: wl_zfiwrt0008 TYPE zfiwrt0008,
          wl_zfiwrt0009 TYPE zfiwrt0009,
          wl_zsdt0252   TYPE zsdt0252,
          wl_zsdt0253   TYPE zsdt0253.

    DATA: wl_zfiwrt0008_rco_01 TYPE zfiwrt0008,
          t_zfiwrt0009_rco_01  TYPE zfiwrt0009_t,
          t_zfiwrt0020_rco_01  TYPE zfiwrt0020_t,
          wl_active_rco_01     TYPE j_1bnfe_active,
          wl_doc_rco_01        TYPE j_1bnfdoc.

    DATA: it_zsdt0249 TYPE TABLE OF zsdt0249.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_ent_rco_01.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_ent_rco IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Opr.Ent.RCO.ZNFW(ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Opr.Ent.RCO.ZNFW(ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_boletim)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF ( sy-subrc NE 0 ) OR ( me->zif_boletim_producao~at_cabecalho-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_ent_rco_01 IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF Entrada RCO 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF Entrada RCO 01' ).

    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_ent_rco_01 = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF Entrada RCO 01' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF Entrada RCO 01' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Validar Lançamento RCO 01
    DATA(_valida_lcto_rco) = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto    = wl_zsdt0252-seqlcto_rco_01
                                                                          IMPORTING e_zfiwrt0008  = wl_zfiwrt0008_rco_01
                                                                                    e_zfiwrt0009  = t_zfiwrt0009_rco_01
                                                                                    e_zfiwrt0020  = t_zfiwrt0020_rco_01
                                                                                    e_active      = wl_active_rco_01
                                                                                    e_doc         = wl_doc_rco_01 ).
    IF _valida_lcto_rco EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                            attr1 = CONV #( 'NF RCO 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
          msgv1  = CONV #( 'NF RCO 01' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_destino_rco_01)
     WHERE branch EQ @wl_zfiwrt0008_rco_01-parid+6(4).

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( |Filial Destino: { wl_zfiwrt0008_rco_01-parid }| )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( |Filial Destino: { wl_zfiwrt0008_rco_01-parid }| ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF Ent. RCO 01 ( Filial: { wl_zsdt0252-branch } )|.

    READ TABLE t_zfiwrt0009_rco_01 INTO DATA(wl_zfiwrt0009_rco_01) INDEX 1.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_ent_rco.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_destino_rco_01-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_destino_rco_01-branch.
    wl_zfiwrt0008-parid           = wl_zfiwrt0008_rco_01-branch.
    wl_zfiwrt0008-nfenum          = wl_active_rco_01-nfnum9.
    wl_zfiwrt0008-series          = wl_active_rco_01-serie.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = wl_zfiwrt0008_rco_01-bldat.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.
    wl_zfiwrt0008-not_check_xml   = abap_true.
    wl_zfiwrt0008-access_key      = wl_active_rco_01-regio  && wl_active_rco_01-nfyear  && wl_active_rco_01-nfmonth &&
                                    wl_active_rco_01-stcd1  && wl_active_rco_01-model   && wl_active_rco_01-serie   &&
                                    wl_active_rco_01-nfnum9 && wl_active_rco_01-docnum9 && wl_active_rco_01-cdv.

*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = wl_zfiwrt0009_rco_01-matnr.
    wl_zfiwrt0009-bwkey  = wl_branch_destino_rco_01-branch.
    wl_zfiwrt0009-menge  = wl_zfiwrt0009_rco_01-menge.
    wl_zfiwrt0009-netpr  = wl_zfiwrt0009_rco_01-netpr.
    wl_zfiwrt0009-netwr  = wl_zfiwrt0009_rco_01-netwr.
    wl_zfiwrt0009-meins  = wl_zfiwrt0009_rco_01-meins.
    wl_zfiwrt0009-charg  = wl_zfiwrt0009_rco_01-charg.

*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
    LOOP AT t_zfiwrt0020_rco_01 INTO DATA(wl_0020_rco_01).
      zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wl_0020_rco_01-docnum ).
    ENDLOOP.

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Entrada RCO 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Entrada RCO 01' ).
    ENDIF.

    wl_zsdt0252-seqlcto_ent_rco_01 = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Entrada RCO 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Entrada RCO 01' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.



  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_ent_rco_02.

    DATA: wl_zfiwrt0008 TYPE zfiwrt0008,
          wl_zfiwrt0009 TYPE zfiwrt0009,
          wl_zsdt0252   TYPE zsdt0252,
          wl_zsdt0253   TYPE zsdt0253.

    DATA: wl_zfiwrt0008_rco_02 TYPE zfiwrt0008,
          t_zfiwrt0009_rco_02  TYPE zfiwrt0009_t,
          t_zfiwrt0020_rco_02  TYPE zfiwrt0020_t,
          wl_active_rco_02     TYPE j_1bnfe_active,
          wl_doc_rco_02        TYPE j_1bnfdoc.

    DATA: it_zsdt0249 TYPE TABLE OF zsdt0249.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_ent_rco_02.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_ent_rco IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Opr.Ent.RCO.ZNFW(ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Opr.Ent.RCO.ZNFW(ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_boletim)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF ( sy-subrc NE 0 ) OR ( me->zif_boletim_producao~at_cabecalho-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_ent_rco_02 IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF Entrada RCO 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF Entrada RCO 02' ).

    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_ent_rco_02 = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF Entrada RCO 02' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF Entrada RCO 02' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Validar Lançamento RCO 02
    DATA(_valida_lcto_rco) = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto    = wl_zsdt0252-seqlcto_rco_02
                                                                          IMPORTING e_zfiwrt0008  = wl_zfiwrt0008_rco_02
                                                                                    e_zfiwrt0009  = t_zfiwrt0009_rco_02
                                                                                    e_zfiwrt0020  = t_zfiwrt0020_rco_02
                                                                                    e_active      = wl_active_rco_02
                                                                                    e_doc         = wl_doc_rco_02 ).
    IF _valida_lcto_rco EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                            attr1 = CONV #( 'NF RCO 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
          msgv1  = CONV #( 'NF RCO 02' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_destino_rco_02)
     WHERE branch EQ @wl_zfiwrt0008_rco_02-parid+6(4).

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( |Filial Destino: { wl_zfiwrt0008_rco_02-parid }| )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( |Filial Destino: { wl_zfiwrt0008_rco_02-parid }| ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF Ent. RCO 02 ( Filial: { wl_zsdt0252-branch } )|.

    READ TABLE t_zfiwrt0009_rco_02 INTO DATA(wl_zfiwrt0009_rco_02) INDEX 1.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_ent_rco.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_destino_rco_02-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_destino_rco_02-branch.
    wl_zfiwrt0008-parid           = wl_zfiwrt0008_rco_02-branch.
    wl_zfiwrt0008-nfenum          = wl_active_rco_02-nfnum9.
    wl_zfiwrt0008-series          = wl_active_rco_02-serie.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = wl_zfiwrt0008_rco_02-bldat.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.
    wl_zfiwrt0008-not_check_xml   = abap_true.
    wl_zfiwrt0008-access_key      = wl_active_rco_02-regio  && wl_active_rco_02-nfyear  && wl_active_rco_02-nfmonth &&
                                    wl_active_rco_02-stcd1  && wl_active_rco_02-model   && wl_active_rco_02-serie   &&
                                    wl_active_rco_02-nfnum9 && wl_active_rco_02-docnum9 && wl_active_rco_02-cdv.

*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = wl_zfiwrt0009_rco_02-matnr.
    wl_zfiwrt0009-bwkey  = wl_branch_destino_rco_02-branch.
    wl_zfiwrt0009-menge  = wl_zfiwrt0009_rco_02-menge.
    wl_zfiwrt0009-netpr  = wl_zfiwrt0009_rco_02-netpr.
    wl_zfiwrt0009-netwr  = wl_zfiwrt0009_rco_02-netwr.
    wl_zfiwrt0009-meins  = wl_zfiwrt0009_rco_02-meins.
    wl_zfiwrt0009-charg  = wl_zfiwrt0009_rco_02-charg.

*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
    LOOP AT t_zfiwrt0020_rco_02 INTO DATA(wl_0020_rco_02).
      zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wl_0020_rco_02-docnum ).
    ENDLOOP.

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Entrada RCO 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Entrada RCO 02' ).
    ENDIF.

    wl_zsdt0252-seqlcto_ent_rco_02 = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Entrada RCO 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Entrada RCO 02' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.


  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_ent_rco_03.

    DATA: wl_zfiwrt0008 TYPE zfiwrt0008,
          wl_zfiwrt0009 TYPE zfiwrt0009,
          wl_zsdt0252   TYPE zsdt0252,
          wl_zsdt0253   TYPE zsdt0253.

    DATA: wl_zfiwrt0008_rco_03 TYPE zfiwrt0008,
          t_zfiwrt0009_rco_03  TYPE zfiwrt0009_t,
          t_zfiwrt0020_rco_03  TYPE zfiwrt0020_t,
          wl_active_rco_03     TYPE j_1bnfe_active,
          wl_doc_rco_03        TYPE j_1bnfdoc.

    DATA: it_zsdt0249 TYPE TABLE OF zsdt0249.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_ent_rco_03.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_ent_rco IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Opr.Ent.RCO.ZNFW(ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Opr.Ent.RCO.ZNFW(ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_boletim)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF ( sy-subrc NE 0 ) OR ( me->zif_boletim_producao~at_cabecalho-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_ent_rco_03 IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF Entrada RCO 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF Entrada RCO 03' ).

    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_ent_rco_03 = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF Entrada RCO 03' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF Entrada RCO 03' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Validar Lançamento RCO 03
    DATA(_valida_lcto_rco) = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto    = wl_zsdt0252-seqlcto_rco_03
                                                                          IMPORTING e_zfiwrt0008  = wl_zfiwrt0008_rco_03
                                                                                    e_zfiwrt0009  = t_zfiwrt0009_rco_03
                                                                                    e_zfiwrt0020  = t_zfiwrt0020_rco_03
                                                                                    e_active      = wl_active_rco_03
                                                                                    e_doc         = wl_doc_rco_03 ).
    IF _valida_lcto_rco EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                            attr1 = CONV #( 'NF RCO 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
          msgv1  = CONV #( 'NF RCO 03' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_destino_rco_03)
     WHERE branch EQ @wl_zfiwrt0008_rco_03-parid+6(4).

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( |Filial Destino: { wl_zfiwrt0008_rco_03-parid }| )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( |Filial Destino: { wl_zfiwrt0008_rco_03-parid }| ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF Ent. RCO 03 ( Filial: { wl_zsdt0252-branch } )|.

    READ TABLE t_zfiwrt0009_rco_03 INTO DATA(wl_zfiwrt0009_rco_03) INDEX 1.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_ent_rco.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_destino_rco_03-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_destino_rco_03-branch.
    wl_zfiwrt0008-parid           = wl_zfiwrt0008_rco_03-branch.
    wl_zfiwrt0008-nfenum          = wl_active_rco_03-nfnum9.
    wl_zfiwrt0008-series          = wl_active_rco_03-serie.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = wl_zfiwrt0008_rco_03-bldat.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.
    wl_zfiwrt0008-not_check_xml   = abap_true.
    wl_zfiwrt0008-access_key      = wl_active_rco_03-regio  && wl_active_rco_03-nfyear  && wl_active_rco_03-nfmonth &&
                                    wl_active_rco_03-stcd1  && wl_active_rco_03-model   && wl_active_rco_03-serie   &&
                                    wl_active_rco_03-nfnum9 && wl_active_rco_03-docnum9 && wl_active_rco_03-cdv.

*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = wl_zfiwrt0009_rco_03-matnr.
    wl_zfiwrt0009-bwkey  = wl_branch_destino_rco_03-branch.
    wl_zfiwrt0009-menge  = wl_zfiwrt0009_rco_03-menge.
    wl_zfiwrt0009-netpr  = wl_zfiwrt0009_rco_03-netpr.
    wl_zfiwrt0009-netwr  = wl_zfiwrt0009_rco_03-netwr.
    wl_zfiwrt0009-meins  = wl_zfiwrt0009_rco_03-meins.
    wl_zfiwrt0009-charg  = wl_zfiwrt0009_rco_03-charg.

*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
    LOOP AT t_zfiwrt0020_rco_03 INTO DATA(wl_0020_rco_03).
      zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wl_0020_rco_03-docnum ).
    ENDLOOP.

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Entrada RCO 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Entrada RCO 03' ).
    ENDIF.

    wl_zsdt0252-seqlcto_ent_rco_03 = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Entrada RCO 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Entrada RCO 03' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.




  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_industrializacao.

    DATA: wl_zfiwrt0008        TYPE zfiwrt0008,
          wl_zfiwrt0009        TYPE zfiwrt0009,
          wl_zsdt0252          TYPE zsdt0252,
          wl_zsdt0253          TYPE zsdt0253,
          wl_doc_ent_dev       TYPE j_1bnfdoc,
          t_zfiwrt0009_ent_dev TYPE zfiwrt0009_t.

    DATA: v_netwr      TYPE zfiwrt0009-netwr,
          v_menge_vinc TYPE zfiwrt0009-menge.

    DATA: it_zsdt0249 TYPE TABLE OF zsdt0249.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_ind.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    me->zif_boletim_producao~check_aprovacao( i_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim  ).

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    zcl_boletim_producao=>zif_boletim_producao~get_saldos( EXPORTING i_id_boletim     = me->zif_boletim_producao~at_cabecalho-id_boletim
                                                           IMPORTING e_saldo_vincular = DATA(_saldo_vincular) ).

    IF _saldo_vincular NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_boletim_com_saldo-msgid
                            msgno = zcx_boletim_producao=>zcx_boletim_com_saldo-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_boletim_com_saldo-msgno
          msgid  = zcx_boletim_producao=>zcx_boletim_com_saldo-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_ind IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Operação Ind. ZNFW (ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Operação Ind. ZNFW (ZSDT0169)' ).
    ENDIF.

    IF wl_zsdt0253-ind_cons_hx_sg <= 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Ind.Consumo Hexano(ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Ind.Consumo Hexano(ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_boletim)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF ( sy-subrc NE 0 ) OR ( me->zif_boletim_producao~at_cabecalho-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-produto_rem_ind IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Produto Remessa Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Produto Remessa Industrialização' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ i_zsdt0252-id_agrp.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    "Validar Entrada Devolução
    DATA(_valida_lcto_ent_dev) = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto   = wl_zsdt0252-seqlcto_ent_dev
                                                                              IMPORTING e_doc        = wl_doc_ent_dev
                                                                                        e_zfiwrt0009 = t_zfiwrt0009_ent_dev ).
    IF _valida_lcto_ent_dev EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                            attr1 = CONV #( 'NF Entrada Devolução' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
          msgv1  = CONV #( 'NF Entrada Devolução' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_ind IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF Industrialização' ).

    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_ind = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF Industrialização' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF Industrialização' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Documentos Referenciados
    CLEAR: it_zsdt0249[].
    SELECT *
      FROM zsdt0249 INTO TABLE it_zsdt0249
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ i_zsdt0252-id_agrp.

    IF it_zsdt0249[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Documentos Referenciados' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Documentos Referenciados' ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF Industrialização ( Filial: { wl_zsdt0252-branch } )|.

    READ TABLE t_zfiwrt0009_ent_dev INTO DATA(wl_zfiwrt0009_ent_dev) INDEX 1.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_ind.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_boletim-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_boletim-branch.
    wl_zfiwrt0008-parid           = i_zsdt0252-branch.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = sy-datum.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.

*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*


    CLEAR: wl_zfiwrt0009.

    DATA(_wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'HX' ). "Hexano

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = _wl_zsdt0250-matnr.
    wl_zfiwrt0009-bwkey  = me->zif_boletim_producao~at_cabecalho-branch.
    wl_zfiwrt0009-menge = wl_zfiwrt0009_ent_dev-menge * wl_zsdt0253-ind_cons_hx_sg.

    wl_zfiwrt0009-netpr  = zcl_preco=>zif_preco~get_preco_estoque( i_matnr   = CONV #( wl_zfiwrt0009-matnr )
                                                                   i_bwkey   = CONV #( wl_zfiwrt0009-bwkey )
                                                                   i_vprsv   = 'V'  "Preço médio móvel/preço interno periódico.
                                                                   i_waers   = 'BRL'
                                                                   i_last    = abap_true ).

    wl_zfiwrt0009-netwr  = wl_zfiwrt0009-menge * wl_zfiwrt0009-netpr.

    SELECT SINGLE *
      FROM mara INTO @DATA(wl_mara)
     WHERE matnr EQ @wl_zfiwrt0009-matnr.

    IF ( sy-subrc EQ 0 ).
      wl_zfiwrt0009-meins = wl_mara-meins.

      IF ( wl_mara-xchpf = abap_true ).
        wl_zfiwrt0009-charg = wl_zfiwrt0009_ent_dev-charg.
      ENDIF.
    ENDIF.

*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
    zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wl_doc_ent_dev-docnum ).

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Industrialização' ).
    ENDIF.

    wl_zsdt0252-seqlcto_ind = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF Industrialização' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.



  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_rco_01.

    DATA: wl_zfiwrt0008        TYPE zfiwrt0008,
          wl_zfiwrt0009        TYPE zfiwrt0009,
          wl_zsdt0252          TYPE zsdt0252,
          wl_zsdt0253          TYPE zsdt0253,
          wl_doc_rfl_01        TYPE j_1bnfdoc,
          wl_zfiwrt0008_rfl_01 TYPE zfiwrt0008,
          t_zfiwrt0009_rfl_01  TYPE zfiwrt0009_t.

    DATA: v_netwr      TYPE zfiwrt0009-netwr,
          v_menge_vinc TYPE zfiwrt0009-menge.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_rco_01.

*---------------------------------------------------------------------------------------------------------*
*   Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_rco IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Operação RCO. ZNFW (ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Operação RCO. ZNFW (ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_boletim)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF ( sy-subrc NE 0 ) OR ( me->zif_boletim_producao~at_cabecalho-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    "Validar RFL 01
    DATA(_valida_lcto_rfl_01) = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto    = wl_zsdt0252-seqlcto_rfl_01
                                                                             IMPORTING e_doc         = wl_doc_rfl_01
                                                                                       e_zfiwrt0008  = wl_zfiwrt0008_rfl_01
                                                                                       e_zfiwrt0009  = t_zfiwrt0009_rfl_01 ).
    IF _valida_lcto_rfl_01 EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                            attr1 = CONV #( 'NF RFL 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
          msgv1  = CONV #( 'NF RFL 01' ).
    ENDIF.


    SELECT SINGLE *
      FROM zfiwrt0015 INTO @DATA(wl_zfiwrt0015_z1)
     WHERE seq_lcto EQ @wl_zfiwrt0008_rfl_01-seq_lcto
       AND parvw    EQ 'Z1'.

    IF ( sy-subrc NE 0 ) OR ( wl_zfiwrt0015_z1-parid IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Parc.Z1 RFL 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Parc.Z1 RFL 01' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_rco_01 IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF RCO 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF RCO 01' ).

    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_rco_01 = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF RCO 01' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF RCO 01' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF RCO 01 ( Filial: { wl_zsdt0252-branch } )|.

    READ TABLE t_zfiwrt0009_rfl_01 INTO DATA(wl_zfiwrt0009_rfl_01) INDEX 1.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_rco.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_boletim-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_boletim-branch.
    wl_zfiwrt0008-parid           = wl_zfiwrt0015_z1-parid.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = sy-datum.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.

*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = wl_zfiwrt0009_rfl_01-matnr.
    wl_zfiwrt0009-bwkey  = wl_branch_boletim-branch.
    wl_zfiwrt0009-menge  = wl_zfiwrt0009_rfl_01-menge.
    wl_zfiwrt0009-netpr  = wl_zfiwrt0009_rfl_01-netpr.
    wl_zfiwrt0009-netwr  = wl_zfiwrt0009_rfl_01-netwr.
    wl_zfiwrt0009-meins  = wl_zfiwrt0009_rfl_01-meins.
    wl_zfiwrt0009-charg  = wl_zfiwrt0009_rfl_01-charg.


*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
    zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wl_doc_rfl_01-docnum ).

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RCO 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RCO 01' ).
    ENDIF.

    wl_zsdt0252-seqlcto_rco_01 = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RCO 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RCO 01' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.


  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_rco_02.

    DATA: wl_zfiwrt0008        TYPE zfiwrt0008,
          wl_zfiwrt0009        TYPE zfiwrt0009,
          wl_zsdt0252          TYPE zsdt0252,
          wl_zsdt0253          TYPE zsdt0253,
          wl_doc_rfl_02        TYPE j_1bnfdoc,
          wl_zfiwrt0008_rfl_02 TYPE zfiwrt0008,
          t_zfiwrt0009_rfl_02  TYPE zfiwrt0009_t.

    DATA: v_netwr      TYPE zfiwrt0009-netwr,
          v_menge_vinc TYPE zfiwrt0009-menge.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_rco_02.

*---------------------------------------------------------------------------------------------------------*
*   Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_rco IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Operação RCO. ZNFW (ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Operação RCO. ZNFW (ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_boletim)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF ( sy-subrc NE 0 ) OR ( me->zif_boletim_producao~at_cabecalho-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    "Validar RFL 02
    DATA(_valida_lcto_rfl_02) = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto    = wl_zsdt0252-seqlcto_rfl_02
                                                                             IMPORTING e_doc         = wl_doc_rfl_02
                                                                                       e_zfiwrt0008  = wl_zfiwrt0008_rfl_02
                                                                                       e_zfiwrt0009  = t_zfiwrt0009_rfl_02 ).
    IF _valida_lcto_rfl_02 EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                            attr1 = CONV #( 'NF RFL 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
          msgv1  = CONV #( 'NF RFL 02' ).
    ENDIF.


    SELECT SINGLE *
      FROM zfiwrt0015 INTO @DATA(wl_zfiwrt0015_z1)
     WHERE seq_lcto EQ @wl_zfiwrt0008_rfl_02-seq_lcto
       AND parvw    EQ 'Z1'.

    IF ( sy-subrc NE 0 ) OR ( wl_zfiwrt0015_z1-parid IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Parc.Z1 RFL 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Parc.Z1 RFL 02' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_rco_02 IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF RCO 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF RCO 02' ).

    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_rco_02 = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF RCO 02' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF RCO 02' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF RCO 02 ( Filial: { wl_zsdt0252-branch } )|.

    READ TABLE t_zfiwrt0009_rfl_02 INTO DATA(wl_zfiwrt0009_rfl_02) INDEX 1.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_rco.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_boletim-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_boletim-branch.
    wl_zfiwrt0008-parid           = wl_zfiwrt0015_z1-parid.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = sy-datum.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.

*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = wl_zfiwrt0009_rfl_02-matnr.
    wl_zfiwrt0009-bwkey  = wl_branch_boletim-branch.
    wl_zfiwrt0009-menge  = wl_zfiwrt0009_rfl_02-menge.
    wl_zfiwrt0009-netpr  = wl_zfiwrt0009_rfl_02-netpr.
    wl_zfiwrt0009-netwr  = wl_zfiwrt0009_rfl_02-netwr.
    wl_zfiwrt0009-meins  = wl_zfiwrt0009_rfl_02-meins.
    wl_zfiwrt0009-charg  = wl_zfiwrt0009_rfl_02-charg.


*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
    zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wl_doc_rfl_02-docnum ).

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RCO 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RCO 02' ).
    ENDIF.

    wl_zsdt0252-seqlcto_rco_02 = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RCO 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RCO 02' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.


  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_rco_03.

    DATA: wl_zfiwrt0008        TYPE zfiwrt0008,
          wl_zfiwrt0009        TYPE zfiwrt0009,
          wl_zsdt0252          TYPE zsdt0252,
          wl_zsdt0253          TYPE zsdt0253,
          wl_doc_rfl_03        TYPE j_1bnfdoc,
          wl_zfiwrt0008_rfl_03 TYPE zfiwrt0008,
          t_zfiwrt0009_rfl_03  TYPE zfiwrt0009_t.

    DATA: v_netwr      TYPE zfiwrt0009-netwr,
          v_menge_vinc TYPE zfiwrt0009-menge.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_rco_03.

*---------------------------------------------------------------------------------------------------------*
*   Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_rco IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Operação RCO. ZNFW (ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Operação RCO. ZNFW (ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_boletim)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF ( sy-subrc NE 0 ) OR ( me->zif_boletim_producao~at_cabecalho-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    "Validar RFL 03
    DATA(_valida_lcto_rfl_03) = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto    = wl_zsdt0252-seqlcto_rfl_03
                                                                             IMPORTING e_doc         = wl_doc_rfl_03
                                                                                       e_zfiwrt0008  = wl_zfiwrt0008_rfl_03
                                                                                       e_zfiwrt0009  = t_zfiwrt0009_rfl_03 ).
    IF _valida_lcto_rfl_03 EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_not_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_not_generate-msgno
                            attr1 = CONV #( 'NF RFL 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_not_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_not_generate-msgid
          msgv1  = CONV #( 'NF RFL 03' ).
    ENDIF.


    SELECT SINGLE *
      FROM zfiwrt0015 INTO @DATA(wl_zfiwrt0015_z1)
     WHERE seq_lcto EQ @wl_zfiwrt0008_rfl_03-seq_lcto
       AND parvw    EQ 'Z1'.

    IF ( sy-subrc NE 0 ) OR ( wl_zfiwrt0015_z1-parid IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Parc.Z1 RFL 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Parc.Z1 RFL 03' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_rco_03 IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF RCO 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF RCO 03' ).

    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_rco_03 = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF RCO 03' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF RCO 03' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF RCO 03 ( Filial: { wl_zsdt0252-branch } )|.

    READ TABLE t_zfiwrt0009_rfl_03 INTO DATA(wl_zfiwrt0009_rfl_03) INDEX 1.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_rco.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_boletim-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_boletim-branch.
    wl_zfiwrt0008-parid           = wl_zfiwrt0015_z1-parid.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = sy-datum.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.

*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = wl_zfiwrt0009_rfl_03-matnr.
    wl_zfiwrt0009-bwkey  = wl_branch_boletim-branch.
    wl_zfiwrt0009-menge  = wl_zfiwrt0009_rfl_03-menge.
    wl_zfiwrt0009-netpr  = wl_zfiwrt0009_rfl_03-netpr.
    wl_zfiwrt0009-netwr  = wl_zfiwrt0009_rfl_03-netwr.
    wl_zfiwrt0009-meins  = wl_zfiwrt0009_rfl_03-meins.
    wl_zfiwrt0009-charg  = wl_zfiwrt0009_rfl_03-charg.


*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
    zcl_nf_writer=>zif_nf_writer~get_instance( )->add_doc_ref( i_docnum =  wl_doc_rfl_03-docnum ).

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RCO 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RCO 03' ).
    ENDIF.

    wl_zsdt0252-seqlcto_rco_03 = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RCO 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RCO 03' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.


  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_rfl_01.

    DATA: wl_zfiwrt0008  TYPE zfiwrt0008,
          wl_zfiwrt0009  TYPE zfiwrt0009,
          wl_zfiwrt0015  TYPE zfiwrt0015,
          wl_zfiwrt0019  TYPE zfiwrt0019,
          wl_zsdt0252    TYPE zsdt0252,
          wl_zsdt0253    TYPE zsdt0253,
          wl_depara_depo TYPE zsdt_depara_depo.

    DATA: it_zsdt0249 TYPE TABLE OF zsdt0249.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_rfl_01.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_rfl IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Operação RFL ZNFW (ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Operação RFL ZNFW (ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_rfl)
     WHERE branch EQ @i_zsdt0252-branch.

    IF ( sy-subrc NE 0 ) OR ( i_zsdt0252-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    SELECT SINGLE *
      FROM adrc INTO @DATA(wl_branch_adrc)
     WHERE addrnumber EQ @wl_branch_rfl-adrnr.

    IF ( sy-subrc NE 0 ) OR ( i_zsdt0252-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Endereço Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Endereço Filial' ).
    ENDIF.


    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_rfl_01 IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF RFL 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF RFL 01' ).
    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_rfl_01 = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF RFL 01' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF RFL 01' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Validar Lançamento Doc. Produção.
    IF wl_zsdt0252-qtde_cp_doc_05 IS NOT INITIAL.
      me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_05
                                                                            i_with_raise = abap_true
                                                                            i_ds_doc     = 'Doc. Produção 05' ).
    ENDIF.

    IF wl_zsdt0252-qtde_fc_doc_04 IS NOT INITIAL.
      me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_04
                                                                            i_with_raise = abap_true
                                                                            i_ds_doc     = 'Doc. Produção 04' ).
    ENDIF.

    IF wl_zsdt0252-qtde_fh_doc_03 IS NOT INITIAL.
      me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_03
                                                                            i_with_raise = abap_true
                                                                            i_ds_doc     = 'Doc. Produção 03' ).
    ENDIF.

    me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_02
                                                                          i_with_raise = abap_true
                                                                          i_ds_doc     = 'Doc. Produção 02' ).


    me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_01
                                                                          i_with_raise = abap_true
                                                                          i_ds_doc     = 'Doc. Produção 01' ).

    "Documentos Referenciados
    CLEAR: it_zsdt0249[].
    SELECT *
      FROM zsdt0249 INTO TABLE it_zsdt0249
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ i_zsdt0252-id_agrp.

    IF it_zsdt0249[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Documentos Referenciados' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Documentos Referenciados' ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF RFL 01 ( Filial: { wl_zsdt0252-branch } )|.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_rfl.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_rfl-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_rfl-branch.
    wl_zfiwrt0008-parid           = i_zsdt0252-branch.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = sy-datum.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.

    CASE me->zif_boletim_producao~at_cabecalho-branch.
      WHEN '9121'.
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255 - INICIO
*        CALL FUNCTION 'Z_BUSCA_DEPARA'
*          EXPORTING
*            i_werks          = i_zsdt0252-branch
*            i_lifnr          = '0000001002'
*            i_opera          = 'RF'
*          IMPORTING
*            zsdt_depara_depo = wl_depara_depo.


        DATA(lva_participante) = zcl_eudr_utils=>ck_filial_participante_eudr( i_werks = CONV #( i_zsdt0252-branch ) ).

        IF lva_participante = 'S'.
          DATA(lva_eudr) = 'S'.
        ELSE.
          lva_eudr = abap_false.
        ENDIF.

        zcl_depara_centro_fixo_virtual=>get_dados_depara(
            EXPORTING
              i_werks       = i_zsdt0252-branch
              i_lifnr       = '0000001002'
              i_operacao    = 'RF'
              i_eudr        = lva_eudr
            IMPORTING
             e_single_depara          = wl_depara_depo  ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - FIM

        IF ( wl_depara_depo-werks_v IS INITIAL ) OR ( wl_depara_depo-lgort_t IS INITIAL ).
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                                msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                                attr1 = CONV #( 'Centro/Deposito Destino - Filial:' && i_zsdt0252-branch )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
              msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
              msgv1  = CONV #( 'Centro/Deposito Destino - Filial:' && i_zsdt0252-branch ).
        ENDIF.

        wl_zfiwrt0008-move_plant = wl_depara_depo-werks_v.

        IF me->zif_boletim_producao~at_cabecalho-categ_soja = 'RR'.
          wl_zfiwrt0008-move_stloc = wl_depara_depo-lgort.
        ELSE.
          wl_zfiwrt0008-move_stloc = wl_depara_depo-lgort_t.
        ENDIF.

        "FF #191283 - inicio
        IF me->zif_boletim_producao~at_cabecalho-categ_soja = 'RE' OR
           me->zif_boletim_producao~at_cabecalho-categ_soja = 'CE'.

          wl_zfiwrt0008-move_stloc = wl_depara_depo-lgort.

        ENDIF.
        "FF #191283 - fim

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_filial_invalida-msgid
                              msgno = zcx_boletim_producao=>zcx_filial_invalida-msgno
                              attr1 = CONV #( me->zif_boletim_producao~at_cabecalho-branch )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_filial_invalida-msgno
            msgid  = zcx_boletim_producao=>zcx_filial_invalida-msgid
            msgv1  = CONV #( me->zif_boletim_producao~at_cabecalho-branch ).
    ENDCASE.


*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    DATA(_wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'OD' ). "Oleo Degomado

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = _wl_zsdt0250-matnr.
    wl_zfiwrt0009-menge  =  i_zsdt0252-qtde_od_me. "I_ZSDT0252-QTDE_OD_DOC_02.
    wl_zfiwrt0009-netpr  = zcl_preco=>zif_preco~get_preco_pauta( i_regio = CONV #( wl_branch_adrc-region )
                                                                 i_matnr = CONV #( wl_zfiwrt0009-matnr )
                                                                 i_inco1 = CONV #( wl_zfiwrt0008-inco1 )
                                                                 i_last  = abap_true ).
    wl_zfiwrt0009-netwr  = wl_zfiwrt0009-netpr * wl_zfiwrt0009-menge.

    SELECT SINGLE *
      FROM mara INTO @DATA(wl_mara)
     WHERE matnr EQ @wl_zfiwrt0009-matnr.

    IF ( sy-subrc EQ 0 ).
      wl_zfiwrt0009-meins = wl_mara-meins.

      IF ( wl_mara-xchpf = abap_true ).
        wl_zfiwrt0009-charg = i_zsdt0252-charg.
      ENDIF.
    ENDIF.

    wl_zfiwrt0009-bwkey  = i_zsdt0252-werks_v.
    wl_zfiwrt0009-lgort  = 'PR01'.

*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
    "LOOP AT IT_ZSDT0249 INTO DATA(WL_ZSDT0249).
    "  ZCL_NF_WRITER=>ZIF_NF_WRITER~GET_INSTANCE( )->ADD_DOC_REF( I_DOCNUM =  WL_ZSDT0249-DOCNUM ).
    "ENDLOOP.

    CASE me->zif_boletim_producao~at_cabecalho-branch.
      WHEN '9121'.
        CLEAR: wl_zfiwrt0015.

        wl_zfiwrt0015-parid = '0000001002'.
        wl_zfiwrt0015-parvw = 'Z1'.
        zcl_nf_writer=>zif_nf_writer~get_instance( )->add_parceiro( i_parceiro = wl_zfiwrt0015 ).

        CLEAR: wl_zfiwrt0019.
        wl_zfiwrt0019-lifnr = '0000001002'.
        zcl_nf_writer=>zif_nf_writer~get_instance( )->set_dados_transp( i_dados_transp = wl_zfiwrt0019 ).
    ENDCASE.

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RFL 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RFL 01' ).
    ENDIF.

    wl_zsdt0252-seqlcto_rfl_01 = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RFL 01' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RFL 01' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.


  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_rfl_02.

    DATA: wl_zfiwrt0008  TYPE zfiwrt0008,
          wl_zfiwrt0009  TYPE zfiwrt0009,

          wl_zfiwrt0015  TYPE zfiwrt0015,
          wl_zfiwrt0019  TYPE zfiwrt0019,

          wl_zsdt0252    TYPE zsdt0252,
          wl_zsdt0253    TYPE zsdt0253,
          wl_depara_depo TYPE zsdt_depara_depo.

    DATA: it_zsdt0249 TYPE TABLE OF zsdt0249.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_rfl_02.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_rfl IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Operação RFL ZNFW (ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Operação RFL ZNFW (ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_rfl)
     WHERE branch EQ @i_zsdt0252-branch.

    IF ( sy-subrc NE 0 ) OR ( i_zsdt0252-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    SELECT SINGLE *
      FROM adrc INTO @DATA(wl_branch_adrc)
     WHERE addrnumber EQ @wl_branch_rfl-adrnr.

    IF ( sy-subrc NE 0 ) OR ( i_zsdt0252-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Endereço Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Endereço Filial' ).
    ENDIF.


    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_rfl_02 IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF RFL 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF RFL 02' ).
    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_rfl_02 = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF RFL 02' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF RFL 02' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Validar Lançamento Doc. Produção.
    IF wl_zsdt0252-qtde_cp_doc_05 IS NOT INITIAL.
      me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_05
                                                                            i_with_raise = abap_true
                                                                            i_ds_doc     = 'Doc. Produção 05' ).
    ENDIF.

    IF wl_zsdt0252-qtde_fc_doc_04 IS NOT INITIAL.
      me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_04
                                                                            i_with_raise = abap_true
                                                                            i_ds_doc     = 'Doc. Produção 04' ).
    ENDIF.

    "Farelo Hipro
    me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_03
                                                                          i_with_raise = abap_true
                                                                          i_ds_doc     = 'Doc. Produção 03' ).

    me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_02
                                                                          i_with_raise = abap_true
                                                                          i_ds_doc     = 'Doc. Produção 02' ).


    me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_01
                                                                          i_with_raise = abap_true
                                                                          i_ds_doc     = 'Doc. Produção 01' ).

    "Documentos Referenciados
    CLEAR: it_zsdt0249[].
    SELECT *
      FROM zsdt0249 INTO TABLE it_zsdt0249
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ i_zsdt0252-id_agrp.

    IF it_zsdt0249[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Documentos Referenciados' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Documentos Referenciados' ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF RFL 02 ( Filial: { wl_zsdt0252-branch } )|.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_rfl.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_rfl-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_rfl-branch.
    wl_zfiwrt0008-parid           = i_zsdt0252-branch.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = sy-datum.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.

    CASE me->zif_boletim_producao~at_cabecalho-branch.
      WHEN '9121'.
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255 - INICIO

*        CALL FUNCTION 'Z_BUSCA_DEPARA'
*          EXPORTING
*            I_WERKS          = I_ZSDT0252-BRANCH
*            I_LIFNR          = '0000001002'
*            I_OPERA          = 'RF'
*          IMPORTING
*            ZSDT_DEPARA_DEPO = WL_DEPARA_DEPO.

        DATA(lva_participante) = zcl_eudr_utils=>ck_filial_participante_eudr( i_werks = CONV #( i_zsdt0252-branch ) ).

        IF lva_participante = 'S'.
          DATA(lva_eudr) = 'S'.
        ELSE.
          lva_eudr = abap_false.
        ENDIF.

        zcl_depara_centro_fixo_virtual=>get_dados_depara(
            EXPORTING
              i_werks       = i_zsdt0252-branch
              i_lifnr       = '0000001002'
              i_operacao    = 'RF'
              i_eudr        = lva_eudr
            IMPORTING
             e_single_depara = wl_depara_depo  ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - FIM

        IF ( wl_depara_depo-werks_v IS INITIAL ) OR ( wl_depara_depo-lgort_t IS INITIAL ).
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                                msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                                attr1 = CONV #( 'Centro/Deposito Destino - Filial:' && i_zsdt0252-branch )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
              msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
              msgv1  = CONV #( 'Centro/Deposito Destino - Filial:' && i_zsdt0252-branch ).
        ENDIF.

        wl_zfiwrt0008-move_plant = wl_depara_depo-werks_v.

        IF me->zif_boletim_producao~at_cabecalho-categ_soja = 'RR'.
          wl_zfiwrt0008-move_stloc = wl_depara_depo-lgort.
        ELSE.
          wl_zfiwrt0008-move_stloc = wl_depara_depo-lgort_t.
        ENDIF.

        "FF #191283 - inicio
        IF me->zif_boletim_producao~at_cabecalho-categ_soja = 'RE' OR
           me->zif_boletim_producao~at_cabecalho-categ_soja = 'CE'.

          wl_zfiwrt0008-move_stloc = wl_depara_depo-lgort.

        ENDIF.
        "FF #191283 - fim

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_filial_invalida-msgid
                              msgno = zcx_boletim_producao=>zcx_filial_invalida-msgno
                              attr1 = CONV #( me->zif_boletim_producao~at_cabecalho-branch )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_filial_invalida-msgno
            msgid  = zcx_boletim_producao=>zcx_filial_invalida-msgid
            msgv1  = CONV #( me->zif_boletim_producao~at_cabecalho-branch ).
    ENDCASE.


*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    DATA(_wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'FH' ). "Farelo Hipro

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = _wl_zsdt0250-matnr.
    wl_zfiwrt0009-menge  = i_zsdt0252-qtde_fh_me. "I_ZSDT0252-QTDE_FH_DOC_03.
    wl_zfiwrt0009-netpr  = zcl_preco=>zif_preco~get_preco_pauta( i_regio = CONV #( wl_branch_adrc-region )
                                                                 i_matnr = CONV #( wl_zfiwrt0009-matnr )
                                                                 i_inco1 = CONV #( wl_zfiwrt0008-inco1 )
                                                                 i_last  = abap_true ).
    wl_zfiwrt0009-netwr  = wl_zfiwrt0009-netpr * wl_zfiwrt0009-menge.

    SELECT SINGLE *
      FROM mara INTO @DATA(wl_mara)
     WHERE matnr EQ @wl_zfiwrt0009-matnr.

    IF ( sy-subrc EQ 0 ).
      wl_zfiwrt0009-meins = wl_mara-meins.

      IF ( wl_mara-xchpf = abap_true ).
        wl_zfiwrt0009-charg = i_zsdt0252-charg.
      ENDIF.
    ENDIF.

    wl_zfiwrt0009-bwkey  = i_zsdt0252-werks_v.
    wl_zfiwrt0009-lgort  = 'PR01'.

*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
*    LOOP AT IT_ZSDT0249 INTO DATA(WL_ZSDT0249).
*      ZCL_NF_WRITER=>ZIF_NF_WRITER~GET_INSTANCE( )->ADD_DOC_REF( I_DOCNUM =  WL_ZSDT0249-DOCNUM ).
*    ENDLOOP.

    CASE me->zif_boletim_producao~at_cabecalho-branch.
      WHEN '9121'.
        CLEAR: wl_zfiwrt0015.

        wl_zfiwrt0015-parid = '0000001002'.
        wl_zfiwrt0015-parvw = 'Z1'.
        zcl_nf_writer=>zif_nf_writer~get_instance( )->add_parceiro( i_parceiro = wl_zfiwrt0015 ).

        CLEAR: wl_zfiwrt0019.
        wl_zfiwrt0019-lifnr = '0000001002'.
        zcl_nf_writer=>zif_nf_writer~get_instance( )->set_dados_transp( i_dados_transp = wl_zfiwrt0019 ).
    ENDCASE.

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RFL 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RFL 02' ).
    ENDIF.

    wl_zsdt0252-seqlcto_rfl_02 = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RFL 02' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RFL 02' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.

  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_nf_rfl_03.

    DATA: wl_zfiwrt0008  TYPE zfiwrt0008,
          wl_zfiwrt0009  TYPE zfiwrt0009,

          wl_zfiwrt0015  TYPE zfiwrt0015,
          wl_zfiwrt0019  TYPE zfiwrt0019,

          wl_zsdt0252    TYPE zsdt0252,
          wl_zsdt0253    TYPE zsdt0253,
          wl_depara_depo TYPE zsdt_depara_depo.

    DATA: it_zsdt0249 TYPE TABLE OF zsdt0249.

    r_if_boletim_producao = me.

    CLEAR: e_seq_lcto_znfw.

    CLEAR: wl_zsdt0253.
    SELECT SINGLE * FROM zsdt0253 INTO wl_zsdt0253 WHERE branch EQ me->zif_boletim_producao~at_cabecalho-branch.

    DATA(_key_lcto) = i_zsdt0252-key_docs && me->zif_boletim_producao~at_id_ordem_nf_rfl_03.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid
                            msgno = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgno
          msgid  = zcx_boletim_producao=>zcx_bol_sem_nota_fiscal-msgid.
    ENDIF.

    IF wl_zsdt0253-operacao_rfl IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Operação RFL ZNFW (ZSDT0169)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Operação RFL ZNFW (ZSDT0169)' ).
    ENDIF.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(wl_branch_rfl)
     WHERE branch EQ @i_zsdt0252-branch.

    IF ( sy-subrc NE 0 ) OR ( i_zsdt0252-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    SELECT SINGLE *
      FROM adrc INTO @DATA(wl_branch_adrc)
     WHERE addrnumber EQ @wl_branch_rfl-adrnr.

    IF ( sy-subrc NE 0 ) OR ( i_zsdt0252-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Endereço Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Endereço Filial' ).
    ENDIF.


    SELECT SINGLE *
      FROM zsdt0252 INTO wl_zsdt0252
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro de Documentos(ZSDT0252)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro de Documentos(ZSDT0252)' ).
    ENDIF.

    IF wl_zsdt0252-seqlcto_rfl_03 IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_document_generate-msgid
                            msgno = zcx_boletim_producao=>zcx_document_generate-msgno
                            attr1 = CONV #( 'NF RFL 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_document_generate-msgno
          msgid  = zcx_boletim_producao=>zcx_document_generate-msgid
          msgv1  = CONV #( 'NF RFL 03' ).
    ELSE.

      me->zif_boletim_producao~get_doc_boletim_valido( EXPORTING i_key_docs      = CONV #( _key_lcto )
                                                       IMPORTING e_seq_lcto_znfw = e_seq_lcto_znfw ).

      IF e_seq_lcto_znfw IS NOT INITIAL.

        wl_zsdt0252-seqlcto_rfl_03 = e_seq_lcto_znfw.
        MODIFY zsdt0252 FROM wl_zsdt0252.

        IF sy-subrc NE 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                                msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                                attr1 = CONV #( 'NF RFL 03' )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
              msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
              msgv1  = CONV #( 'NF RFL 03' ).
        ENDIF.

        MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.
        RETURN.

      ENDIF.

    ENDIF.

    "Validar Lançamento Doc. Produção.
    IF wl_zsdt0252-qtde_cp_doc_05 IS NOT INITIAL.
      me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_05
                                                                            i_with_raise = abap_true
                                                                            i_ds_doc     = 'Doc. Produção 05' ).
    ENDIF.

    "Farelo Comum
    me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_04
                                                                          i_with_raise = abap_true
                                                                          i_ds_doc     = 'Doc. Produção 04' ).

    IF wl_zsdt0252-qtde_fh_doc_03 IS NOT INITIAL.
      me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_03
                                                                            i_with_raise = abap_true
                                                                            i_ds_doc     = 'Doc. Produção 03' ).
    ENDIF.

    me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_02
                                                                          i_with_raise = abap_true
                                                                          i_ds_doc     = 'Doc. Produção 02' ).


    me->zif_boletim_producao~check_doc_material_valido( i_mblnr      = wl_zsdt0252-doc_prod_01
                                                                          i_with_raise = abap_true
                                                                          i_ds_doc     = 'Doc. Produção 01' ).

    "Documentos Referenciados
    CLEAR: it_zsdt0249[].
    SELECT *
      FROM zsdt0249 INTO TABLE it_zsdt0249
     WHERE id_boletim EQ i_zsdt0252-id_boletim
       AND branch     EQ i_zsdt0252-branch
       AND charg      EQ i_zsdt0252-charg
       AND id_agrp    EQ i_zsdt0252-id_agrp.

    IF it_zsdt0249[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Documentos Referenciados' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Documentos Referenciados' ).
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = |Gerando NF RFL 03 ( Filial: { wl_zsdt0252-branch } )|.

*--------------------------------------------------------------------------------------------------------------*
*   Determinar Cabeçalho
*--------------------------------------------------------------------------------------------------------------*
    CLEAR: wl_zfiwrt0008.

    wl_zfiwrt0008-operacao        = wl_zsdt0253-operacao_rfl.
    wl_zfiwrt0008-origin_key      = _key_lcto.
    wl_zfiwrt0008-bukrs           = wl_branch_rfl-bukrs.
    wl_zfiwrt0008-branch          = wl_branch_rfl-branch.
    wl_zfiwrt0008-parid           = i_zsdt0252-branch.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = wl_zfiwrt0008-parid IMPORTING output = wl_zfiwrt0008-parid.

    wl_zfiwrt0008-budat           = sy-datum.
    wl_zfiwrt0008-bldat           = sy-datum.
    wl_zfiwrt0008-inco1           = 'FOB'.
    wl_zfiwrt0008-inco2           = 'FOB'.
    wl_zfiwrt0008-tcode_org       = sy-tcode.

    CASE me->zif_boletim_producao~at_cabecalho-branch.
      WHEN '9121'.
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255 - INICIO
*        CALL FUNCTION 'Z_BUSCA_DEPARA'
*          EXPORTING
*            I_WERKS          = I_ZSDT0252-BRANCH
*            I_LIFNR          = '0000001002'
*            I_OPERA          = 'RF'
*          IMPORTING
*            ZSDT_DEPARA_DEPO = WL_DEPARA_DEPO.

        DATA(lva_participante) = zcl_eudr_utils=>ck_filial_participante_eudr( i_werks = CONV #( i_zsdt0252-branch ) ).

        IF lva_participante = 'S'.
          DATA(lva_eudr) = 'S'.
        ELSE.
          lva_eudr = abap_false.
        ENDIF.

        zcl_depara_centro_fixo_virtual=>get_dados_depara(
          EXPORTING
            i_werks       = i_zsdt0252-branch
            i_lifnr       = '0000001002'
            i_operacao    = 'RF'
            i_eudr        = lva_eudr
          IMPORTING
           e_single_depara          = wl_depara_depo  ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255 - FIM

        IF ( wl_depara_depo-werks_v IS INITIAL ) OR ( wl_depara_depo-lgort_t IS INITIAL ).
          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                                msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                                attr1 = CONV #( 'Centro/Deposito Destino - Filial:' && i_zsdt0252-branch )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
              msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
              msgv1  = CONV #( 'Centro/Deposito Destino - Filial:' && i_zsdt0252-branch ).
        ENDIF.

        wl_zfiwrt0008-move_plant = wl_depara_depo-werks_v.

        IF me->zif_boletim_producao~at_cabecalho-categ_soja = 'RR'.
          wl_zfiwrt0008-move_stloc = wl_depara_depo-lgort.
        ELSE.
          wl_zfiwrt0008-move_stloc = wl_depara_depo-lgort_t.
        ENDIF.

        "FF #191283 - inicio
        IF me->zif_boletim_producao~at_cabecalho-categ_soja = 'RE' OR
           me->zif_boletim_producao~at_cabecalho-categ_soja = 'CE'.

          wl_zfiwrt0008-move_stloc = wl_depara_depo-lgort.

        ENDIF.
        "FF #191283 - fim

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_filial_invalida-msgid
                              msgno = zcx_boletim_producao=>zcx_filial_invalida-msgno
                              attr1 = CONV #( me->zif_boletim_producao~at_cabecalho-branch )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_filial_invalida-msgno
            msgid  = zcx_boletim_producao=>zcx_filial_invalida-msgid
            msgv1  = CONV #( me->zif_boletim_producao~at_cabecalho-branch ).
    ENDCASE.


*--------------------------------------------------------------------------------------------------------*
*   Determinar Item
*--------------------------------------------------------------------------------------------------------*

    CLEAR: wl_zfiwrt0009.

    DATA(_wl_zsdt0250) = zcl_boletim_producao=>zif_boletim_producao~get_material_boletim( i_tp_produto =  'FC' ). "Farelo Comum

    wl_zfiwrt0009-itmnum = 10.
    wl_zfiwrt0009-matnr  = _wl_zsdt0250-matnr.
    wl_zfiwrt0009-menge  = i_zsdt0252-qtde_fc_me. "I_ZSDT0252-QTDE_FC_DOC_04.
    wl_zfiwrt0009-netpr  = zcl_preco=>zif_preco~get_preco_pauta( i_regio = CONV #( wl_branch_adrc-region )
                                                                 i_matnr = CONV #( wl_zfiwrt0009-matnr )
                                                                 i_inco1 = CONV #( wl_zfiwrt0008-inco1 )
                                                                 i_last  = abap_true ).
    wl_zfiwrt0009-netwr  = wl_zfiwrt0009-netpr * wl_zfiwrt0009-menge.

    SELECT SINGLE *
      FROM mara INTO @DATA(wl_mara)
     WHERE matnr EQ @wl_zfiwrt0009-matnr.

    IF ( sy-subrc EQ 0 ).
      wl_zfiwrt0009-meins = wl_mara-meins.

      IF ( wl_mara-xchpf = abap_true ).
        wl_zfiwrt0009-charg = i_zsdt0252-charg.
      ENDIF.
    ENDIF.

    wl_zfiwrt0009-bwkey  = i_zsdt0252-werks_v.
    wl_zfiwrt0009-lgort  = 'PR01'.

*--------------------------------------------------------------------------------------------------------*
*   Gravar Documento
*--------------------------------------------------------------------------------------------------------*

    zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                               )->set_cabecalho( i_cabecalho =  wl_zfiwrt0008
                                               )->add_item( i_item = wl_zfiwrt0009 ).
    "Documentos Referenciados.
*    LOOP AT IT_ZSDT0249 INTO DATA(WL_ZSDT0249).
*      ZCL_NF_WRITER=>ZIF_NF_WRITER~GET_INSTANCE( )->ADD_DOC_REF( I_DOCNUM =  WL_ZSDT0249-DOCNUM ).
*    ENDLOOP.

    CASE me->zif_boletim_producao~at_cabecalho-branch.
      WHEN '9121'.
        CLEAR: wl_zfiwrt0015.

        wl_zfiwrt0015-parid = '0000001002'.
        wl_zfiwrt0015-parvw = 'Z1'.
        zcl_nf_writer=>zif_nf_writer~get_instance( )->add_parceiro( i_parceiro = wl_zfiwrt0015 ).

        CLEAR: wl_zfiwrt0019.
        wl_zfiwrt0019-lifnr = '0000001002'.
        zcl_nf_writer=>zif_nf_writer~get_instance( )->set_dados_transp( i_dados_transp = wl_zfiwrt0019 ).
    ENDCASE.

    zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = DATA(_seq_lcto_znfw) ).

    IF _seq_lcto_znfw IS INITIAL.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RFL 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RFL 03' ).
    ENDIF.

    wl_zsdt0252-seqlcto_rfl_03 = _seq_lcto_znfw.
    MODIFY zsdt0252 FROM wl_zsdt0252.

    IF sy-subrc NE 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_generate_doc-msgid
                            msgno = zcx_boletim_producao=>zcx_error_generate_doc-msgno
                            attr1 = CONV #( 'NF RFL 03' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_generate_doc-msgno
          msgid  = zcx_boletim_producao=>zcx_error_generate_doc-msgid
          msgv1  = CONV #( 'NF RFL 03' ).
    ENDIF.

    e_seq_lcto_znfw = _seq_lcto_znfw.

    MESSAGE |Lançamento { e_seq_lcto_znfw } gerado com sucesso!| TYPE 'S'.

  ENDMETHOD.


  METHOD zif_boletim_producao~gerar_rateio_produtos.



  ENDMETHOD.


  METHOD zif_boletim_producao~gera_erro_geral.

    RAISE EXCEPTION TYPE zcx_boletim_producao
      EXPORTING
        textid = VALUE #( msgid = CONV #( i_msgid )
                          msgno = CONV #( i_msgno )
                          attr1 = CONV #( i_msgv1 )
                          attr2 = CONV #( i_msgv2 )
                          attr3 = CONV #( i_msgv3 )
                          attr4 = CONV #( i_msgv4 ) )
        msgid  = i_msgid
        msgno  = i_msgno
        msgty  = 'E'
        msgv1  = i_msgv1
        msgv2  = i_msgv2
        msgv3  = i_msgv3
        msgv4  = i_msgv4.


  ENDMETHOD.


  METHOD zif_boletim_producao~get_doc_boletim_valido.

    DATA: it_zfiwrt0008 TYPE TABLE OF zfiwrt0008,
          it_mkpf       TYPE TABLE OF mkpf.

    CLEAR: e_seq_lcto_znfw, e_mblnr.

    IF strlen( i_key_docs ) NE 25.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Key Documento(25 Char)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Key Documento(25 Char)' ).

    ENDIF.


    CASE i_key_docs+23(2).
      WHEN zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_devolucao      OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_ent_devolucao  OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_ind            OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_ent_ind        OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rfl_01         OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rfl_02         OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rfl_03         OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rco_01         OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rco_02         OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_rco_03         OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_ent_rco_01     OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_ent_rco_02     OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_nf_ent_rco_03.

        CLEAR: it_zfiwrt0008[].

        SELECT *
          FROM zfiwrt0008 INTO TABLE it_zfiwrt0008
         WHERE origin_key EQ i_key_docs.

        LOOP AT it_zfiwrt0008 INTO DATA(wl_zfiwrt0008) WHERE loekz           EQ abap_false
                                                         AND docs_estornados EQ abap_false.
          e_seq_lcto_znfw = wl_zfiwrt0008-seq_lcto.
          RETURN.
        ENDLOOP.

      WHEN zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_doc_prod_01       OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_doc_prod_02       OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_doc_prod_03       OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_doc_prod_04       OR
           zcl_boletim_producao=>zif_boletim_producao~at_id_ordem_doc_prod_05.

        CLEAR: it_mkpf[].

        SELECT *
          FROM mkpf AS c INTO TABLE it_mkpf
         WHERE bktxt EQ i_key_docs
           "Eliminando documentos de estorno
           AND NOT EXISTS ( SELECT i~mblnr
                              FROM mseg AS i
                             WHERE i~mblnr EQ c~mblnr
                               AND i~smbln NE space
                               AND i~smbln NE '0000000000' ).

        LOOP AT it_mkpf INTO DATA(wl_mkpf).

          DATA(_valido) = me->zif_boletim_producao~check_doc_material_valido( i_mblnr = CONV #( wl_mkpf-mblnr ) ).

          IF _valido EQ abap_true.
            e_mblnr = wl_mkpf-mblnr.
            RETURN.
          ENDIF.

        ENDLOOP.


      WHEN OTHERS.

        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                              msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                              attr1 = CONV #( 'Ordem Geração Doc.' )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
            msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
            msgv1  = CONV #( 'Ordem Geração Doc.' ).

    ENDCASE.


  ENDMETHOD.


  METHOD zif_boletim_producao~get_instance.


    IF zif_boletim_producao~at_if_boletim_producao IS NOT BOUND.
      CREATE OBJECT zif_boletim_producao~at_if_boletim_producao TYPE zcl_boletim_producao.
    ENDIF.

    r_if_boletim_producao = zif_boletim_producao~at_if_boletim_producao.

    CLEAR: zif_boletim_producao~at_date.
  ENDMETHOD.


  METHOD zif_boletim_producao~get_key_docs.

    CLEAR: r_key_docs.

    CASE me->zif_boletim_producao~at_cabecalho-tp_boletim.
      WHEN '02' OR '03'.
        r_key_docs   = 'BP' && i_zsdt0252-id_boletim && i_zsdt0252-branch && '0000' && i_zsdt0252-id_agrp.
      WHEN OTHERS.
        r_key_docs   = 'BP' && i_zsdt0252-id_boletim && i_zsdt0252-branch && i_zsdt0252-charg && i_zsdt0252-id_agrp.
    ENDCASE.



    IF strlen( r_key_docs ) NE 23.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Key Documento(23 Char)' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Key Documento(23 Char)' ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_boletim_producao~get_material_boletim.

    CLEAR: r_zsdt0250.

    SELECT SINGLE *
      FROM zsdt0250 INTO r_zsdt0250
     WHERE tp_produto_producao EQ i_tp_produto.

    IF ( sy-subrc NE 0 ) OR ( r_zsdt0250-matnr IS INITIAL ).

      DATA(_ds_tp_produto) = zcl_util=>get_desc_value_domain( i_domname = 'ZDM_TP_PRODUTO_PRODUCAO' i_domvalue = CONV #( i_tp_produto ) ).

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_produto_boletim_not_param-msgid
                            msgno = zcx_boletim_producao=>zcx_produto_boletim_not_param-msgno
                            attr1 = CONV #( _ds_tp_produto )
                            )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_produto_boletim_not_param-msgno
          msgid  = zcx_boletim_producao=>zcx_produto_boletim_not_param-msgid
          msgv1  = CONV #( _ds_tp_produto ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_boletim_producao~get_perc_rendimento.



  ENDMETHOD.


  METHOD zif_boletim_producao~get_saldos.

    CLEAR: e_saldo_vinculado, e_saldo_vincular.

    CHECK i_id_boletim IS NOT INITIAL.

    SELECT SUM( qtde_vinc )
      FROM zsdt0252 INTO e_saldo_vinculado
     WHERE id_boletim EQ i_id_boletim.


    SELECT SUM( qtde_consumo )
      FROM zsdt0247 INTO e_saldo_vincular
     WHERE id_boletim EQ i_id_boletim.

    SUBTRACT e_saldo_vinculado FROM e_saldo_vincular.

  ENDMETHOD.


  METHOD zif_boletim_producao~get_status_boletim.

    DATA: it_zsdt0252 TYPE TABLE OF zsdt0252.

    r_status    = 'A'.
    e_ds_status = 'Aberto'.

    IF i_id_boletim_producao IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0246 INTO @DATA(wl_0246)
     WHERE id_boletim EQ @i_id_boletim_producao.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Documento Boletim Id:' && i_id_boletim_producao )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Documento Boletim Id:' && i_id_boletim_producao ).
    ENDIF.

    CLEAR: it_zsdt0252[].

    SELECT *
      FROM zsdt0252 INTO TABLE it_zsdt0252
     WHERE id_boletim EQ i_id_boletim_producao.

    CHECK it_zsdt0252[] IS NOT INITIAL.

    DATA(_lctos_pendentes) = abap_false.

    LOOP AT it_zsdt0252 INTO DATA(wl_zsdt0252).

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF de Devolução - Saida e Entrada
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

      IF ( wl_0246-com_nf EQ abap_true ).

        DATA(_valida_lcto) = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_devol ).
        IF _valida_lcto EQ abap_false.
          _lctos_pendentes = abap_true.
          EXIT.
        ENDIF.

        _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_ent_dev ).
        IF _valida_lcto EQ abap_false.
          _lctos_pendentes = abap_true.
          EXIT.
        ENDIF.

      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF de Industrialização - Saida e Entrada
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

      IF ( wl_0246-com_nf EQ abap_true ).

        _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_ind ).
        IF _valida_lcto EQ abap_false.
          _lctos_pendentes = abap_true.
          EXIT.
        ENDIF.

        _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_ent_ind ).
        IF _valida_lcto EQ abap_false.
          _lctos_pendentes = abap_true.
          EXIT.
        ENDIF.

      ENDIF.
*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     Documentos de Produção
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

      CHECK wl_zsdt0252-id_agrp EQ 1.


      _valida_lcto =  me->zif_boletim_producao~check_doc_material_valido( i_mblnr = wl_zsdt0252-doc_prod_01 ).
      IF _valida_lcto EQ abap_false.
        _lctos_pendentes = abap_true.
        EXIT.
      ENDIF.

      _valida_lcto =  me->zif_boletim_producao~check_doc_material_valido( i_mblnr = wl_zsdt0252-doc_prod_02 ).
      IF _valida_lcto EQ abap_false.
        _lctos_pendentes = abap_true.
        EXIT.
      ENDIF.

      IF ( wl_zsdt0252-qtde_fh_doc_03 IS NOT INITIAL ).
        _valida_lcto =  me->zif_boletim_producao~check_doc_material_valido( i_mblnr = wl_zsdt0252-doc_prod_03 ).
        IF _valida_lcto EQ abap_false.
          _lctos_pendentes = abap_true.
          EXIT.
        ENDIF.
      ENDIF.

      IF ( wl_zsdt0252-qtde_fc_doc_04 IS NOT INITIAL ).
        _valida_lcto =  me->zif_boletim_producao~check_doc_material_valido( i_mblnr = wl_zsdt0252-doc_prod_04 ).
        IF _valida_lcto EQ abap_false.
          _lctos_pendentes = abap_true.
          EXIT.
        ENDIF.
      ENDIF.

      IF ( wl_zsdt0252-qtde_cp_doc_05 IS NOT INITIAL ).
        _valida_lcto =  me->zif_boletim_producao~check_doc_material_valido( i_mblnr = wl_zsdt0252-doc_prod_05 ).
        IF _valida_lcto EQ abap_false.
          _lctos_pendentes = abap_true.
          EXIT.
        ENDIF.
      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Remessa Formação de Lote - 01 , 02 , 03
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

      IF wl_0246-com_nf EQ abap_true.

        IF ( wl_zsdt0252-qtde_od_me IS NOT INITIAL ). "WL_0246-OD_DEST_MI EQ ABAP_FALSE.
          _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_rfl_01 ).
          IF _valida_lcto EQ abap_false.
            _lctos_pendentes = abap_true.
            EXIT.
          ENDIF.
        ENDIF.

        IF ( wl_zsdt0252-qtde_fh_me IS NOT INITIAL ). "WL_0246-FH_DEST_MI EQ ABAP_FALSE.
*          IF ( WL_ZSDT0252-QTDE_FH_DOC_03 IS NOT INITIAL ).
          _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_rfl_02 ).
          IF _valida_lcto EQ abap_false.
            _lctos_pendentes = abap_true.
            EXIT.
          ENDIF.
*          ENDIF.
        ENDIF.

        IF ( wl_zsdt0252-qtde_fc_me IS NOT INITIAL ). "WL_0246-FC_DEST_MI EQ ABAP_FALSE.
*          IF ( WL_ZSDT0252-QTDE_FC_DOC_04 IS NOT INITIAL ).
          _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_rfl_03 ).
          IF _valida_lcto EQ abap_false.
            _lctos_pendentes = abap_true.
            EXIT.
          ENDIF.
*          ENDIF.
        ENDIF.

      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Remessa Conta e Ordem 01 - Saida e Entrada
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

      IF ( wl_0246-com_nf EQ abap_true ).

        IF ( wl_zsdt0252-seqlcto_rfl_01 IS NOT INITIAL ).

          _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_rco_01 ).
          IF _valida_lcto EQ abap_false.
            _lctos_pendentes = abap_true.
            EXIT.
          ENDIF.

          _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_ent_rco_01 ).
          IF _valida_lcto EQ abap_false.
            _lctos_pendentes = abap_true.
            EXIT.
          ENDIF.

        ENDIF.

      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Remessa Conta e Ordem 02 - Saida e Entrada
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

      IF ( wl_0246-com_nf EQ abap_true ).

        IF ( wl_zsdt0252-seqlcto_rfl_02 IS NOT INITIAL ).

          _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_rco_02 ).
          IF _valida_lcto EQ abap_false.
            _lctos_pendentes = abap_true.
            EXIT.
          ENDIF.

          _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_ent_rco_02 ).
          IF _valida_lcto EQ abap_false.
            _lctos_pendentes = abap_true.
            EXIT.
          ENDIF.

        ENDIF.

      ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Remessa Conta e Ordem 03 - Saida e Entrada
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

      IF ( wl_0246-com_nf EQ abap_true ).

        IF ( wl_zsdt0252-seqlcto_rfl_03 IS NOT INITIAL ).

          _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_rco_03 ).
          IF _valida_lcto EQ abap_false.
            _lctos_pendentes = abap_true.
            EXIT.
          ENDIF.

          _valida_lcto = zcl_nf_writer=>zif_nf_writer~check_auth_doc( EXPORTING i_seq_lcto = wl_zsdt0252-seqlcto_ent_rco_03 ).
          IF _valida_lcto EQ abap_false.
            _lctos_pendentes = abap_true.
            EXIT.
          ENDIF.

        ENDIF.

      ENDIF.


    ENDLOOP.

    IF _lctos_pendentes EQ abap_false.
      r_status    = 'F'.
      e_ds_status = 'Finalizado'.
    ENDIF.


  ENDMETHOD.


  METHOD zif_boletim_producao~get_tp_produtos_producao.



  ENDMETHOD.


  METHOD zif_boletim_producao~get_tp_produtos_rendimento.




  ENDMETHOD.


  METHOD zif_boletim_producao~get_und_material.

    CLEAR: e_meins.
    SELECT SINGLE meins FROM mara INTO e_meins WHERE matnr EQ i_matnr.
  ENDMETHOD.


  METHOD zif_boletim_producao~get_versao.

    DATA: lva_count_version TYPE i,
          lva_msg_erro      TYPE c LENGTH 200.

    CLEAR: r_verid.

    SELECT *
      FROM mkal INTO TABLE @DATA(lit_mkal)
     WHERE matnr EQ @i_matnr
       AND werks EQ @i_werks
       AND mksp  EQ @space.

    lva_count_version = 0.

    LOOP AT lit_mkal INTO DATA(lwa_mkal).

      r_verid = lwa_mkal-verid.

      ADD 1 TO lva_count_version.

    ENDLOOP.

    IF r_verid IS INITIAL.

      CONCATENATE 'Nenhuma versão encontrada para o material:' i_matnr 'Centro:' i_werks INTO lva_msg_erro SEPARATED BY space.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_geral-msgid
                             msgno = zcx_boletim_producao=>zcx_erro_geral-msgno
                             attr1 = CONV #( lva_msg_erro+00(50) )
                             attr2 = CONV #( lva_msg_erro+50(50) )
                            )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_geral-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_geral-msgid
          msgv1  = CONV #( lva_msg_erro+00(50) )
          msgv2  = CONV #( lva_msg_erro+50(50) ).

    ELSEIF lva_count_version > 1.

      CONCATENATE 'Existe mais de uma versão valida para o material:' i_matnr 'Centro:' i_werks INTO lva_msg_erro SEPARATED BY space.

      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_geral-msgid
                             msgno = zcx_boletim_producao=>zcx_erro_geral-msgno
                             attr1 = CONV #( lva_msg_erro+00(50) )
                             attr2 = CONV #( lva_msg_erro+50(50) )
                            )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_geral-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_geral-msgid
          msgv1  = CONV #( lva_msg_erro+00(50) )
          msgv2  = CONV #( lva_msg_erro+50(50) ).


    ENDIF.

  ENDMETHOD.


  METHOD zif_boletim_producao~gravar_registro.

    DATA: wl_0252      TYPE zsdt0252,
          v_qte_sg     TYPE zsdt0247-qtde_consumo,
          v_perc_total TYPE zsdt0247-perc_total.

    r_if_boletim_producao = me.

    me->zif_boletim_producao~validar_registro( ).

    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZBOL_PROD'
        IMPORTING
          number                  = me->zif_boletim_producao~at_cabecalho-id_boletim
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF ( sy-subrc IS NOT INITIAL ) OR ( me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_obj_nro_bol_not_found-msgid
                              msgno = zcx_boletim_producao=>zcx_obj_nro_bol_not_found-msgno
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_obj_nro_bol_not_found-msgno
            msgid  = zcx_boletim_producao=>zcx_obj_nro_bol_not_found-msgid.
      ENDIF.

      me->zif_boletim_producao~at_cabecalho-dt_registro     = sy-datum.
      me->zif_boletim_producao~at_cabecalho-hr_registro     = sy-uzeit.
      me->zif_boletim_producao~at_cabecalho-us_registro     = sy-uname.

*      "Determinar Produtos com Destinação Mercado Interno -- Regra Temporario - Ini
*      IF ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-COM_NF EQ ABAP_TRUE.
*        SELECT SINGLE *
*          FROM SETLEAF INTO @DATA(LWA_GERAR_OD)
*         WHERE SETNAME EQ 'ZSDT0170_NOT_GERAR_PROD'
*           AND VALFROM EQ 'OD'.
*
*        IF SY-SUBRC EQ 0.
*          ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-OD_DEST_MI = ABAP_TRUE.
*        ENDIF.
*
*        SELECT SINGLE *
*          FROM SETLEAF INTO @DATA(LWA_GERAR_FH)
*         WHERE SETNAME EQ 'ZSDT0170_NOT_GERAR_PROD'
*           AND VALFROM EQ 'FH'.
*
*        IF SY-SUBRC EQ 0.
*          ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-FH_DEST_MI = ABAP_TRUE.
*        ENDIF.
*
*        SELECT SINGLE *
*          FROM SETLEAF INTO @DATA(LWA_GERAR_FC)
*         WHERE SETNAME EQ 'ZSDT0170_NOT_GERAR_PROD'
*           AND VALFROM EQ 'FC'.
*
*        IF SY-SUBRC EQ 0.
*          ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-FC_DEST_MI = ABAP_TRUE.
*        ENDIF.
*      ENDIF.
      "Determinar Produtos com Destinação Mercado Interno -- Regra Temporario - Fim

    ENDIF.

*----------------------------------------------------------------------------------------------------------*
*   Atribuir ID Boletim
*----------------------------------------------------------------------------------------------------------*
    LOOP AT me->zif_boletim_producao~at_dados_producao ASSIGNING FIELD-SYMBOL(<fs_dados_prod>).
      <fs_dados_prod>-id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim.
    ENDLOOP.

    LOOP AT me->zif_boletim_producao~at_dados_rendimento ASSIGNING FIELD-SYMBOL(<fs_dados_rend>).
      <fs_dados_rend>-id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim.
    ENDLOOP.

*----------------------------------------------------------------------------------------------------------*
*   Gravar Dados
*----------------------------------------------------------------------------------------------------------*

    "Calcular Percentual.
    CLEAR: v_qte_sg, v_perc_total.
    LOOP AT me->zif_boletim_producao~at_dados_producao ASSIGNING <fs_dados_prod>.
      ADD <fs_dados_prod>-qtde_consumo TO v_qte_sg.
    ENDLOOP.

    LOOP AT me->zif_boletim_producao~at_dados_producao ASSIGNING <fs_dados_prod>.
      <fs_dados_prod>-perc_total = ( <fs_dados_prod>-qtde_consumo / v_qte_sg ) * 100.
      ADD <fs_dados_prod>-perc_total TO v_perc_total.
    ENDLOOP.

    IF ( v_perc_total NE 100 ).
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_erro_rateio-msgid
                            msgno = zcx_boletim_producao=>zcx_erro_rateio-msgno
                            attr1 = CONV #( 'Qtde. Soja em Grão não rateada em 100%!' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_erro_rateio-msgno
          msgid  = zcx_boletim_producao=>zcx_erro_rateio-msgid
          msgv1  = CONV #( 'Qtde. Soja em Grão não rateada em 100%!' ).
    ENDIF.

    MODIFY zsdt0246 FROM me->zif_boletim_producao~at_cabecalho.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
                            msgno = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
                            attr1 = CONV #( 'ZSDT0246' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
          msgid  = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
          msgv1  = CONV #( 'ZSDT0246' ).
    ENDIF.

    MODIFY zsdt0247 FROM TABLE me->zif_boletim_producao~at_dados_producao.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
                            msgno = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
                            attr1 = CONV #( 'ZSDT0247' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
          msgid  = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
          msgv1  = CONV #( 'ZSDT0247' ).
    ENDIF.

    MODIFY zsdt0248 FROM TABLE me->zif_boletim_producao~at_dados_rendimento.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
                            msgno = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
                            attr1 = CONV #( 'ZSDT0248' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_error_gravar_dados-msgno
          msgid  = zcx_boletim_producao=>zcx_error_gravar_dados-msgid
          msgv1  = CONV #( 'ZSDT0248' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_false.

      CLEAR: wl_0252.

      DELETE FROM zsdt0252 WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim.

      wl_0252-id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim.
      wl_0252-branch     = me->zif_boletim_producao~at_cabecalho-branch.
      wl_0252-charg      = me->zif_boletim_producao~at_cabecalho-charg.
      wl_0252-werks_v    = me->zif_boletim_producao~at_cabecalho-branch.
      wl_0252-lgort_v    = me->zif_boletim_producao~at_cabecalho-lgort_prod.
      wl_0252-id_agrp    = 1.

      wl_0252-key_docs   = me->zif_boletim_producao~get_key_docs( i_zsdt0252 = wl_0252 ).

      LOOP AT me->zif_boletim_producao~at_dados_producao INTO DATA(wl_dados_prod).
        ADD wl_dados_prod-qtde_consumo TO wl_0252-qtde_vinc.
      ENDLOOP.

      MODIFY zsdt0252 FROM wl_0252.

    ENDIF.

    COMMIT WORK.

    MESSAGE s005 WITH me->zif_boletim_producao~at_cabecalho-id_boletim.

    e_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim.




  ENDMETHOD.


  METHOD zif_boletim_producao~novo_registro.

    r_if_boletim_producao = me.

    CLEAR: me->zif_boletim_producao~at_cabecalho,
           me->zif_boletim_producao~at_dados_producao[],
           me->zif_boletim_producao~at_dados_rendimento[].

  ENDMETHOD.


  METHOD zif_boletim_producao~set_dados_boletim.

    r_if_boletim_producao = me.

    me->zif_boletim_producao~at_cabecalho = i_dados_boletim.

  ENDMETHOD.


  METHOD zif_boletim_producao~set_dados_producao.

    r_if_boletim_producao = me.

    me->zif_boletim_producao~at_dados_producao = i_dados_producao.

  ENDMETHOD.


  METHOD zif_boletim_producao~set_dados_rendimento.

    r_if_boletim_producao = me.

    me->zif_boletim_producao~at_dados_rendimento = i_dados_rendimento.

  ENDMETHOD.


  METHOD zif_boletim_producao~set_date_processamento.
    IF i_date IS NOT INITIAL.
      me->zif_boletim_producao~at_date = i_date.
    ELSE.
      me->zif_boletim_producao~at_date = sy-datum.
    ENDIF.
  ENDMETHOD.


  METHOD zif_boletim_producao~set_registro.

    DATA: it_zsdt0247 TYPE zsdt0247_t,
          wa_zsdt0247 TYPE zsdt0247,

          it_zsdt0248 TYPE zsdt0248_t,
          wa_zsdt0248 TYPE zsdt0248.

    r_if_boletim_producao = me.

    CLEAR: it_zsdt0247[], it_zsdt0248[].

    me->zif_boletim_producao~novo_registro( ).

    IF i_id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

*---------------------------------------------------------------------*
* Carregar Dados Cabeçalho Boletim
*---------------------------------------------------------------------*

    SELECT SINGLE *
      FROM zsdt0246 INTO @DATA(wl_0246)
     WHERE id_boletim EQ @i_id_boletim.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_found-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_found-msgno
                            attr1 = CONV #( 'Registro Boletim Id:' && i_id_boletim )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_found-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_found-msgid
          msgv1  = CONV #( 'Registro Boletim Id:' && i_id_boletim ).
    ENDIF.

    MOVE-CORRESPONDING wl_0246 TO me->zif_boletim_producao~at_cabecalho.

*---------------------------------------------------------------------*
* Carregar Dados Produtos Produção
*---------------------------------------------------------------------*

    SELECT *
      FROM zsdt0247 INTO TABLE it_zsdt0247
     WHERE id_boletim EQ i_id_boletim.

    me->zif_boletim_producao~at_dados_producao[] = it_zsdt0247.

*---------------------------------------------------------------------*
* Carregar Dados Rendimento
*---------------------------------------------------------------------*

    SELECT *
      FROM zsdt0248 INTO TABLE it_zsdt0248
     WHERE id_boletim EQ i_id_boletim.

    me->zif_boletim_producao~at_dados_rendimento[] = it_zsdt0248.

    IF i_com_bloqueio IS NOT INITIAL.
      me->zif_boletim_producao~bloquear_registros( i_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_boletim_producao~set_tp_boletim.
    me->zif_boletim_producao~at_tp_boletim = i_tp_boletim.
  ENDMETHOD.


  METHOD zif_boletim_producao~show_erro_geral.

    RAISE EXCEPTION TYPE zcx_boletim_producao
      EXPORTING
        textid = VALUE #( msgid = sy-msgid
                          msgno = sy-msgno
                          attr1 = CONV #( sy-msgv1 )
                          attr2 = CONV #( sy-msgv2 )
                          attr3 = CONV #( sy-msgv3 )
                          attr4 = CONV #( sy-msgv4 ) )
        msgid  = sy-msgid
        msgno  = sy-msgno
        msgty  = 'E'
        msgv1  = sy-msgv1
        msgv2  = sy-msgv2
        msgv3  = sy-msgv3
        msgv4  = sy-msgv4.

  ENDMETHOD.


  METHOD zif_boletim_producao~validar_materiais_doc_prod.




  ENDMETHOD.


  METHOD zif_boletim_producao~validar_registro.

    DATA: it_produtos_prod TYPE zsdt0250_t,
          it_produtos_rend TYPE zsdt0250_t.

    DATA: v_tot_consumo_prod TYPE zsdt0247-qtde_consumo.

    DATA: v_msg_aux    TYPE c LENGTH 200,
          v_msg_aux_01 TYPE c LENGTH 200.

    r_if_boletim_producao = me.

    it_produtos_prod = me->zif_boletim_producao~get_tp_produtos_producao( ).
    it_produtos_rend = me->zif_boletim_producao~get_tp_produtos_rendimento( ).

*-----------------------------------------------------------------------------------------------------*
*   Dados Boletim Produção
*-----------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-dt_producao IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Data Produção' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Data Produção' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-dt_lancamento IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Data Lançamento' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Data Lançamento' ).
    ENDIF.

    IF  me->zif_boletim_producao~at_cabecalho-charg IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Lote' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Lote' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-branch IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Filial' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Filial' ).
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0253 INTO @DATA(wl_0253)
     WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( |Parâmetro Filial: { me->zif_boletim_producao~at_cabecalho-branch } ZSDT0169| )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( |Parâmetro Filial: { me->zif_boletim_producao~at_cabecalho-branch } ZSDT0169| ).
    ELSE.

      me->zif_boletim_producao~at_cabecalho-com_nf = wl_0253-emissao_nf.

      IF me->zif_boletim_producao~at_cabecalho-com_nf EQ abap_true.
        me->zif_boletim_producao~at_cabecalho-lgort_prod = 'PR01'.
      ELSE.
        me->zif_boletim_producao~at_cabecalho-lgort_prod = 'ARMZ'.
      ENDIF.

    ENDIF.

*-CS2021000386 - 28.04.2021 - JT - inicio
    IF wl_0253-emissao_nf = abap_true.
      IF me->zif_boletim_producao~at_cabecalho-categ_soja IS INITIAL.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                              msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                              attr1 = CONV #( 'Categoria Soja' )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
            msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
            msgv1  = CONV #( 'Categoria Soja' ).
      ENDIF.
    ELSE.
      me->zif_boletim_producao~at_cabecalho-categ_soja = 'RR'.
    ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim

    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS NOT INITIAL. "Alterando Boletim
      SELECT SINGLE *
        FROM zsdt0246 INTO @DATA(wl_zsdt0246_old)
       WHERE id_boletim EQ @me->zif_boletim_producao~at_cabecalho-id_boletim.

      IF ( sy-subrc EQ 0 ) AND ( wl_zsdt0246_old-branch NE me->zif_boletim_producao~at_cabecalho-branch ).
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_change_filial-msgid
                              msgno = zcx_boletim_producao=>zcx_change_filial-msgno
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_change_filial-msgno
            msgid  = zcx_boletim_producao=>zcx_change_filial-msgid.
      ENDIF.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
      ID 'WERKS' FIELD  me->zif_boletim_producao~at_cabecalho-branch
      ID 'ACTVT' FIELD '03'.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_not_acess_filial-msgid
                            msgno = zcx_boletim_producao=>zcx_not_acess_filial-msgno
                            attr1 = CONV #( me->zif_boletim_producao~at_cabecalho-branch  )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_not_acess_filial-msgno
          msgid  = zcx_boletim_producao=>zcx_not_acess_filial-msgid
          msgv1  = CONV #( me->zif_boletim_producao~at_cabecalho-branch ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-produto_rem_ind IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Produto Remessa Industrilização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Produto Remessa Industrilização' ).
    ENDIF.

    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS NOT INITIAL.
      me->zif_boletim_producao~check_permissao_modificacao( i_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim ).
    ENDIF.

*-----------------------------------------------------------------------------------------------------*
*   Dados Produção Soja
*-----------------------------------------------------------------------------------------------------*
    DATA(_dados_prod_inf) = abap_false.
    DATA(_gerar_fc)       = abap_false.
    DATA(_gerar_fh)       = abap_false.

    v_tot_consumo_prod = 0.

    LOOP AT it_produtos_prod INTO DATA(wl_produto_prod).

      v_msg_aux = zcl_util=>get_desc_value_domain(  i_domname  = 'ZDM_TP_PRODUTO_PRODUCAO'
                                                    i_domvalue = CONV #( wl_produto_prod-tp_produto_producao ) ).

      CONCATENATE 'da Produção de Soja( Consumo ' v_msg_aux ')' INTO v_msg_aux SEPARATED BY space.

      READ TABLE me->zif_boletim_producao~at_dados_producao INTO DATA(wl_dados_prod) WITH KEY tp_produto_producao = wl_produto_prod-tp_produto_producao.

      IF ( sy-subrc EQ 0  ) AND ( wl_dados_prod-qtde_consumo GT 0  ).

        _dados_prod_inf = abap_true.

        CASE wl_dados_prod-tp_produto_producao.
          WHEN 'FC'.
            _gerar_fc = abap_true.
          WHEN 'FH'.
            _gerar_fh = abap_true.
        ENDCASE.

        IF ( wl_dados_prod-tp_produto_producao IS INITIAL ) OR
           ( wl_dados_prod-unid_consumo        IS INITIAL ).

          RAISE EXCEPTION TYPE zcx_boletim_producao
            EXPORTING
              textid = VALUE #( msgid = zcx_boletim_producao=>zcx_dados_incompletos-msgid
                                msgno = zcx_boletim_producao=>zcx_dados_incompletos-msgno
                                attr1 = CONV #( v_msg_aux )
                               )
              msgty  = 'E'
              msgno  = zcx_boletim_producao=>zcx_dados_incompletos-msgno
              msgid  = zcx_boletim_producao=>zcx_dados_incompletos-msgid
              msgv1  = CONV #( v_msg_aux ).

        ENDIF.

        ADD wl_dados_prod-qtde_consumo TO v_tot_consumo_prod.

      ENDIF.

    ENDLOOP.

    IF ( _dados_prod_inf EQ abap_false ).
      v_msg_aux = 'Dados Produção de Soja!'.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_dados_incompletos-msgid
                            msgno = zcx_boletim_producao=>zcx_dados_incompletos-msgno
                            attr1 = CONV #( v_msg_aux )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_dados_incompletos-msgno
          msgid  = zcx_boletim_producao=>zcx_dados_incompletos-msgid
          msgv1  = CONV #( v_msg_aux ).
    ENDIF.

    IF  me->zif_boletim_producao~at_cabecalho-qtde_si IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Quantidade Soja Industrialização' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Quantidade Soja Industrialização' ).
    ENDIF.

*    IF ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-QTDE_SI GE V_TOT_CONSUMO_PROD.
*
*      WRITE: ME->ZIF_BOLETIM_PRODUCAO~AT_CABECALHO-QTDE_SI TO V_MSG_AUX,
*             V_TOT_CONSUMO_PROD                            TO V_MSG_AUX_01.
*
*      CONDENSE: V_MSG_AUX , V_MSG_AUX_01 NO-GAPS.
*
*      RAISE EXCEPTION TYPE ZCX_BOLETIM_PRODUCAO
*         EXPORTING
*           TEXTID = VALUE #( MSGID = ZCX_BOLETIM_PRODUCAO=>ZCX_QTDE_SI_INVALIDA-MSGID
*                             MSGNO = ZCX_BOLETIM_PRODUCAO=>ZCX_QTDE_SI_INVALIDA-MSGNO
*                             ATTR1 = CONV #( V_MSG_AUX    )
*                             ATTR2 = CONV #( V_MSG_AUX_01 )
*                            )
*           MSGTY  = 'E'
*           MSGNO  = ZCX_BOLETIM_PRODUCAO=>ZCX_QTDE_SI_INVALIDA-MSGNO
*           MSGID  = ZCX_BOLETIM_PRODUCAO=>ZCX_QTDE_SI_INVALIDA-MSGID
*           MSGV1 = CONV #( V_MSG_AUX    )
*           MSGV2 = CONV #( V_MSG_AUX_01 ).
*
*    ENDIF.

*-----------------------------------------------------------------------------------------------------*
*   Dados Rendimento
*-----------------------------------------------------------------------------------------------------*
    LOOP AT it_produtos_rend INTO DATA(wl_produto_rend).

      READ TABLE me->zif_boletim_producao~at_dados_rendimento INTO DATA(wl_dados_rend) WITH KEY tp_produto_producao = wl_produto_rend-tp_produto_producao.

      IF ( sy-subrc NE 0 ).

        v_msg_aux = zcl_util=>get_desc_value_domain(  i_domname  = 'ZDM_TP_PRODUTO_PRODUCAO'
                                                      i_domvalue = CONV #( wl_produto_rend-tp_produto_producao ) ).

        CONCATENATE 'do Rendimento( Produto ' v_msg_aux ')' INTO v_msg_aux SEPARATED BY space.

        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_dados_incompletos-msgid
                              msgno = zcx_boletim_producao=>zcx_dados_incompletos-msgno
                              attr1 = CONV #( v_msg_aux )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_dados_incompletos-msgno
            msgid  = zcx_boletim_producao=>zcx_dados_incompletos-msgid
            msgv1  = CONV #( v_msg_aux ).
      ENDIF.

    ENDLOOP.



    LOOP AT me->zif_boletim_producao~at_dados_rendimento INTO wl_dados_rend.

      DATA(_erro) = abap_false.

      CASE wl_dados_rend-tp_produto_producao.
        WHEN 'FC'. "Farelo Comum
          CHECK _gerar_fc EQ abap_true.
        WHEN 'FH'. "Farelo Hipro
          CHECK _gerar_fh EQ abap_true.
        WHEN OTHERS.
          CHECK ( wl_dados_rend-tp_produto_producao NE 'RS' ) AND  "Residuo de Soja.
                ( wl_dados_rend-tp_produto_producao NE 'CP' ) AND  "Casca Peletizada
                ( wl_dados_rend-tp_produto_producao NE 'CM' ).     "Casca Moida
      ENDCASE.

      IF ( wl_dados_rend-qtde                LE 0       ) OR
         ( wl_dados_rend-perc_rendimento     LE 0       ) OR
         ( wl_dados_rend-tp_produto_producao IS INITIAL ) OR
         ( wl_dados_rend-unid                IS INITIAL ).
        _erro = abap_true.
      ENDIF.

      v_msg_aux = zcl_util=>get_desc_value_domain(  i_domname  = 'ZDM_TP_PRODUTO_PRODUCAO'
                                                    i_domvalue = CONV #( wl_dados_rend-tp_produto_producao ) ).

      CONCATENATE 'do Rendimento -> Produto'  '-' v_msg_aux INTO v_msg_aux SEPARATED BY space.

      IF _erro = abap_true.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_dados_incompletos-msgid
                              msgno = zcx_boletim_producao=>zcx_dados_incompletos-msgno
                              attr1 = CONV #( v_msg_aux )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_dados_incompletos-msgno
            msgid  = zcx_boletim_producao=>zcx_dados_incompletos-msgid
            msgv1  = CONV #( v_msg_aux ).
      ENDIF.

*  Mercado Interno
      IF  wl_dados_rend-qtde < wl_dados_rend-qtde_mi.

        CLEAR  v_msg_aux.
        v_msg_aux = zcl_util=>get_desc_value_domain(  i_domname  = 'ZDM_TP_PRODUTO_PRODUCAO'
                                                           i_domvalue = CONV #( wl_dados_rend-tp_produto_producao ) ).


        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_dados_incompletos-msgid
                              msgno = '040'
                              attr1 = CONV #( v_msg_aux )
                             )
            msgty  = 'E'
            msgno  = '040'
            msgid  = zcx_boletim_producao=>zcx_dados_incompletos-msgid
            msgv1  = CONV #( v_msg_aux ).
      ENDIF.


    ENDLOOP.

  ENDMETHOD.


  METHOD zif_boletim_producao~vincular_nf.

    DATA: v_total_vinculado_nf  TYPE zsdt0251-qtde_saldo,
          v_total_vinculado_bol TYPE zsdt0251-qtde_saldo,
          v_qtde_boletim        TYPE zsdt0251-qtde_saldo,
          v_id_agrp             TYPE zsdt0249-id_agrp,
          v_count_nf_agrp       TYPE i,
          v_lim_vinc_nf         TYPE i,
          v_qtde_vinc           TYPE zsdt0249-qtde_vinc.

    DATA: v_str_vlr_01 TYPE c LENGTH 50,
          v_str_vlr_02 TYPE c LENGTH 50.


    DATA: it_ck_docs_vinculados TYPE TABLE OF zsdt0249.

    DATA: wl_0252   TYPE zsdt0252,
          wl_0249   TYPE zsdt0249,
          wl_0251   TYPE zsdt0251,
          l_lgort_v TYPE lgort_d.

    r_if_boletim_producao = me.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*
    IF me->zif_boletim_producao~at_cabecalho-id_boletim IS INITIAL.
      RAISE EXCEPTION TYPE zcx_boletim_producao
        EXPORTING
          textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                            msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                            attr1 = CONV #( 'Id. Boletim' )
                           )
          msgty  = 'E'
          msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
          msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
          msgv1  = CONV #( 'Id. Boletim' ).
    ENDIF.

    me->zif_boletim_producao~check_documentos_gerados( i_id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim ).

    LOOP AT i_notas INTO DATA(wl_nota_vinc).

      IF wl_nota_vinc-docnum IS INITIAL.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                              msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                              attr1 = CONV #( 'Documento Fiscal' )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
            msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
            msgv1  = CONV #( 'Documento Fiscal' ).
      ENDIF.

      IF wl_nota_vinc-qtde_vinc IS INITIAL.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_data_not_informed-msgid
                              msgno = zcx_boletim_producao=>zcx_data_not_informed-msgno
                              attr1 = CONV #( 'Quantidade vinculada' )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_data_not_informed-msgno
            msgid  = zcx_boletim_producao=>zcx_data_not_informed-msgid
            msgv1  = CONV #( 'Quantidade vinculada' ).
      ENDIF.

*---------------------------------------------------------------------------------------------------------*
*   Get Tabela de Saldo Vinculado NF
*---------------------------------------------------------------------------------------------------------*
      SELECT SINGLE *
        FROM zsdt0251 INTO wl_0251
       WHERE docnum EQ wl_nota_vinc-docnum.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_control_saldo_nf_not_found-msgid
                              msgno = zcx_boletim_producao=>zcx_control_saldo_nf_not_found-msgno
                              attr1 = CONV #( wl_nota_vinc-docnum )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_control_saldo_nf_not_found-msgno
            msgid  = zcx_boletim_producao=>zcx_control_saldo_nf_not_found-msgid
            msgv1  = CONV #( wl_nota_vinc-docnum ).
      ENDIF.

*-CS2021000386 - 28.04.2021 - JT - inicio
      l_lgort_v = wl_0251-lgort_v.
*-CS2021000386 - 28.04.2021 - JT - fim

      "Validar filial destino
      IF ( wl_0251-branch_destino NE me->zif_boletim_producao~at_cabecalho-branch ).
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_filial_nf_divergente_bol-msgid
                              msgno = zcx_boletim_producao=>zcx_filial_nf_divergente_bol-msgno
                              attr1 = CONV #( wl_nota_vinc-docnum  )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_filial_nf_divergente_bol-msgno
            msgid  = zcx_boletim_producao=>zcx_filial_nf_divergente_bol-msgid
            msgv1  = CONV #( wl_nota_vinc-docnum ).
      ENDIF.

      "Validar Lote
      IF ( wl_0251-charg NE me->zif_boletim_producao~at_cabecalho-charg ).
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_lote_nf_divergente_boletim-msgid
                              msgno = zcx_boletim_producao=>zcx_lote_nf_divergente_boletim-msgno
                              attr1 = CONV #( wl_nota_vinc-docnum  )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_lote_nf_divergente_boletim-msgno
            msgid  = zcx_boletim_producao=>zcx_lote_nf_divergente_boletim-msgid
            msgv1  = CONV #( wl_nota_vinc-docnum ).
      ENDIF.

*---------------------------------------------------------------------------------------------------------*
*   Validação de Centro Virtuais e Depositos já vinculados no Boletim
*---------------------------------------------------------------------------------------------------------
      CLEAR: it_ck_docs_vinculados[].

      SELECT *
        FROM zsdt0249 INTO TABLE it_ck_docs_vinculados
       WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim
         AND branch     EQ wl_0251-branch.

      LOOP AT it_ck_docs_vinculados INTO DATA(wl_doc_vinculado) WHERE ( werks_v NE wl_0251-werks_v ) OR ( lgort_v NE wl_0251-lgort_v ).
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_inconsistency_werks_lgort-msgid
                              msgno = zcx_boletim_producao=>zcx_inconsistency_werks_lgort-msgno
                              attr1 = CONV #( wl_0251-branch  )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_inconsistency_werks_lgort-msgno
            msgid  = zcx_boletim_producao=>zcx_inconsistency_werks_lgort-msgid
            msgv1  = CONV #( wl_0251-branch ).
      ENDLOOP.

*---------------------------------------------------------------------------------------------------------*
*   Verificar se NF já esta vinculada no Boletim
*---------------------------------------------------------------------------------------------------------*
      SELECT SINGLE *
        FROM zsdt0249 INTO wl_0249
       WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim
         AND docnum     EQ wl_nota_vinc-docnum.

      IF sy-subrc EQ 0.
        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_nf_vinculada_boletim-msgid
                              msgno = zcx_boletim_producao=>zcx_nf_vinculada_boletim-msgno
                              attr1 = CONV #( wl_nota_vinc-docnum )
                              attr2 = CONV #( me->zif_boletim_producao~at_cabecalho-id_boletim )
                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_nf_vinculada_boletim-msgno
            msgid  = zcx_boletim_producao=>zcx_nf_vinculada_boletim-msgid
            msgv1  = CONV #( wl_nota_vinc-docnum )
            msgv2  = CONV #( me->zif_boletim_producao~at_cabecalho-id_boletim ).
      ENDIF.

*---------------------------------------------------------------------------------------------------------*
*   Bloqueio Registro
*---------------------------------------------------------------------------------------------------------*
      me->zif_boletim_producao~bloquear_registros( i_docnum_zsdt0251 = wl_nota_vinc-docnum ).

*---------------------------------------------------------------------------------------------------------*
*   Check se vinculação vai exceder Saldo NF.
*---------------------------------------------------------------------------------------------------------*
      CLEAR: v_total_vinculado_nf.
      SELECT SUM( qtde_vinc )
        FROM zsdt0249 INTO v_total_vinculado_nf
       WHERE docnum EQ wl_nota_vinc-docnum.

      ADD wl_nota_vinc-qtde_vinc TO v_total_vinculado_nf.

      IF ( v_total_vinculado_nf > wl_0251-qtde_total ).

        WRITE wl_0251-qtde_total   TO v_str_vlr_01.
        WRITE v_total_vinculado_nf TO v_str_vlr_02.

        CONDENSE: v_str_vlr_01, v_str_vlr_02 NO-GAPS.

        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_nf_sem_saldo-msgid
                              msgno = zcx_boletim_producao=>zcx_nf_sem_saldo-msgno
                              attr1 = CONV #( wl_nota_vinc-docnum )
                              attr2 = CONV #( v_str_vlr_01 )
                              attr3 = CONV #( v_str_vlr_02 )

                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_nf_sem_saldo-msgno
            msgid  = zcx_boletim_producao=>zcx_nf_sem_saldo-msgid
            msgv1  = CONV #( wl_nota_vinc-docnum )
            msgv2  = CONV #( v_str_vlr_01 )
            msgv3  = CONV #( v_str_vlr_02 ).

      ENDIF.

*---------------------------------------------------------------------------------------------------------*
*    Valida Saldo Vinculado no Boletim de Produção
*---------------------------------------------------------------------------------------------------------*
      CLEAR: v_qtde_boletim, v_total_vinculado_bol.

      SELECT SUM( qtde_vinc )
        FROM zsdt0249 INTO v_total_vinculado_bol
       WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim.

      LOOP AT me->zif_boletim_producao~at_dados_producao INTO DATA(wl_dados_prod).
        ADD wl_dados_prod-qtde_consumo TO v_qtde_boletim.
      ENDLOOP.

      ADD wl_nota_vinc-qtde_vinc TO v_total_vinculado_bol.

      IF ( v_total_vinculado_bol > v_qtde_boletim ).

        WRITE v_qtde_boletim        TO v_str_vlr_01.
        WRITE v_total_vinculado_bol TO v_str_vlr_02.

        CONDENSE: v_str_vlr_01, v_str_vlr_02 NO-GAPS.

        RAISE EXCEPTION TYPE zcx_boletim_producao
          EXPORTING
            textid = VALUE #( msgid = zcx_boletim_producao=>zcx_boletim_sem_saldo-msgid
                              msgno = zcx_boletim_producao=>zcx_boletim_sem_saldo-msgno
                              attr1 = CONV #( v_str_vlr_01  )
                              attr2 = CONV #( v_str_vlr_02  )

                             )
            msgty  = 'E'
            msgno  = zcx_boletim_producao=>zcx_boletim_sem_saldo-msgno
            msgid  = zcx_boletim_producao=>zcx_boletim_sem_saldo-msgid
            msgv1  = CONV #( v_str_vlr_01 )
            msgv2  = CONV #( v_str_vlr_02 ).

      ENDIF.

*---------------------------------------------------------------------------------------------------------*
*   Get ID Agrupamento
*---------------------------------------------------------------------------------------------------------*
      DATA(_continue) = abap_true.
      v_id_agrp = 1.

      SELECT SINGLE * FROM zsdt0253 INTO @DATA(wl_0253) WHERE branch EQ @me->zif_boletim_producao~at_cabecalho-branch.

      IF ( sy-subrc EQ 0 ) AND ( wl_0253-qtde_nf_devol > 0 ).
        v_lim_vinc_nf = wl_0253-qtde_nf_devol.
      ELSE.
        v_lim_vinc_nf = 30.
      ENDIF.

*--------------------------------------------------------
*-CS2021000386 - 28.04.2021 - JT - inicio
*--------------------------------------------------------
* setar deposito de acordo com a categoria de soja
*--------------------------------------------------------
      IF wl_0253-emissao_nf = abap_true.
        IF me->zif_boletim_producao~at_cabecalho-categ_soja = 'CO'.
          l_lgort_v = 'PO58'.
          "FF #191283 - inicio
        ELSEIF me->zif_boletim_producao~at_cabecalho-categ_soja = 'RR'.
          l_lgort_v = 'PO17'.

        ELSEIF me->zif_boletim_producao~at_cabecalho-categ_soja = 'RE'.
          l_lgort_v = 'POD2'.

        ELSEIF me->zif_boletim_producao~at_cabecalho-categ_soja = 'CE'.
          l_lgort_v = 'POD3'.
        ENDIF.
        "FF #191283 -fim

      ENDIF.



*-CS2021000386 - 28.04.2021 - JT - fim
*--------------------------------------------------------

      WHILE _continue EQ abap_true.

        SELECT COUNT(*)
          FROM zsdt0249 INTO v_count_nf_agrp
         WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim
           AND branch     EQ wl_0251-branch
           AND charg      EQ wl_0251-charg
           AND id_agrp    EQ v_id_agrp.

        IF v_count_nf_agrp < v_lim_vinc_nf.
          EXIT.
        ELSE.
          ADD 1 TO v_id_agrp.
        ENDIF.

      ENDWHILE.

      wl_0251-open_change = abap_true.
      MODIFY zsdt0251 FROM wl_0251.

*---------------------------------------------------------------------------------------------------------*
*   Gravar Registro vinculação
*---------------------------------------------------------------------------------------------------------*
      CLEAR: wl_0249.

      wl_0249-id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim.
      wl_0249-docnum     = wl_nota_vinc-docnum.
      wl_0249-qtde_vinc  = wl_nota_vinc-qtde_vinc.
      wl_0249-branch     = wl_0251-branch.
      wl_0249-charg      = wl_0251-charg.
      wl_0249-werks_v    = wl_0251-werks_v.
      wl_0249-lgort_v    = l_lgort_v. "wl_0251-lgort_v.
      wl_0249-id_agrp    = v_id_agrp.
      MODIFY zsdt0249 FROM wl_0249.

*---------------------------------------------------------------------------------------------------------*
*   Atualizar Tabelas Controle de Saldo
*---------------------------------------------------------------------------------------------------------*
      wl_0251-qtde_utilizada = v_total_vinculado_nf.
      wl_0251-qtde_saldo     = wl_0251-qtde_total - wl_0251-qtde_utilizada.
      MODIFY zsdt0251 FROM wl_0251.

*---------------------------------------------------------------------------------------------------------*
*    Check se Filial/Lote já foi vinculado no Boletim
*---------------------------------------------------------------------------------------------------------*
      SELECT SINGLE *
        FROM zsdt0252 INTO wl_0252
       WHERE id_boletim EQ me->zif_boletim_producao~at_cabecalho-id_boletim
         AND branch     EQ wl_0251-branch
         AND charg      EQ wl_0251-charg
         AND id_agrp    EQ v_id_agrp.

      IF sy-subrc NE 0.
        CLEAR: wl_0252.
        wl_0252-id_boletim = me->zif_boletim_producao~at_cabecalho-id_boletim.
        wl_0252-branch     = wl_0251-branch.
        wl_0252-charg      = wl_0251-charg.
        wl_0252-id_agrp    = v_id_agrp.
        wl_0252-werks_v    = wl_0251-werks_v.
        wl_0252-lgort_v    = l_lgort_v. "wl_0251-lgort_v.
        wl_0252-key_docs   = me->zif_boletim_producao~get_key_docs( i_zsdt0252 = wl_0252 ).
      ENDIF.

      CLEAR: v_qtde_vinc.
      SELECT SUM( qtde_vinc )
        FROM zsdt0249 INTO v_qtde_vinc
       WHERE id_boletim EQ wl_0252-id_boletim
         AND branch     EQ wl_0252-branch
         AND charg      EQ wl_0252-charg
         AND id_agrp    EQ wl_0252-id_agrp.

      wl_0252-qtde_vinc = v_qtde_vinc.

      MODIFY zsdt0252 FROM wl_0252.

      wl_0251-open_change = abap_false.
      MODIFY zsdt0251 FROM wl_0251.

      COMMIT WORK.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
