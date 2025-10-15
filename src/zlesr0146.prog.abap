*&---------------------------------------------------------------------*
*& Report  ZLESR0146
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0146.

*&---------------------------------------------------------------------*
* TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_rom_saida_transf,
         vbeln         TYPE zsdt0001-vbeln,
         nro_nf_prod   TYPE zsdt0001-nro_nf_prod,
         nro_nf_frete  TYPE zsdt0001-nro_nf_frete,
         chave_nfe     TYPE zde_chave_nfe,
         transferencia TYPE c,
         id_ordem      TYPE zsdt0001-id_ordem,
       END OF ty_rom_saida_transf.

*&---------------------------------------------------------------------*
* VARIAVEIS
*&---------------------------------------------------------------------*
DATA: vl_data_lim   TYPE d,
      v_candat_null TYPE j_1bnfdoc-candat,
      v_viagem_id   TYPE zde_viagem_id,
      v_peso_liq    TYPE zlest0185-nm_peso_chegada.

TYPES: BEGIN OF ty_zsdt0001,
         ch_referencia TYPE zsdt0001-ch_referencia,
         id_ordem      TYPE zsdt0001-id_ordem,
         nro_nf_prod   TYPE zsdt0001-nro_nf_prod,
       END OF  ty_zsdt0001.

*&---------------------------------------------------------------------*
* TABELAS INTERNAS
*&---------------------------------------------------------------------*
DATA: it_rom_ent_transf     TYPE TABLE OF zsdt0001 WITH HEADER LINE,
      wa_rom_ent_transf     TYPE          zsdt0001,
      it_rom_ent_transf_aux TYPE TABLE OF zsdt0001 WITH HEADER LINE,
      it_rom_sai_transf     TYPE TABLE OF ty_rom_saida_transf WITH HEADER LINE,
      it_zsdt0105_l         TYPE TABLE OF zsdt0105 WITH HEADER LINE,
      it_zsdt0102_l         TYPE TABLE OF zsdt0102 WITH HEADER LINE,
      it_zsdt0001_s         TYPE STANDARD TABLE OF ty_zsdt0001 WITH UNIQUE SORTED KEY key COMPONENTS id_ordem ch_referencia,
      it_zsdt0001           TYPE STANDARD TABLE OF ty_zsdt0001.


*&---------------------------------------------------------------------*
* INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.

  TRY .
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd  = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.

*&---------------------------------------------------------------------*
* START
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lwa_zintegracao TYPE zintegracao.

  DATA(dt_30_dias) = sy-datum - 30.

  "Viagens não informado descarregamento
  SELECT viagem_id, id_ordem, nm_peso_chegada, dt_descarga, id_integracao_carregar INTO TABLE @DATA(it_zlest0185)
    FROM zlest0185
   WHERE ck_descarregado EQ @abap_false
     AND dt_carregado    GE @dt_30_dias.

  DELETE IT_ZLEST0185 WHERE ID_ORDEM               IS INITIAL.
  DELETE IT_ZLEST0185 WHERE id_integracao_carregar IS INITIAL.

  CHECK it_zlest0185[] IS NOT INITIAL.

  DATA(it_zlest0185_aux) = it_zlest0185[].

  "Descartar registros com retorno de carregamento invalido
  LOOP AT it_zlest0185_aux INTO DATA(lwa_zlest0185).

    SELECT SINGLE *
      FROM zintegracao INTO lwa_zintegracao
     WHERE id_integracao EQ lwa_zlest0185-id_integracao_carregar.

    CHECK ( sy-subrc eq 0 ) AND ( lwa_zintegracao-ds_data_retorno = 'null' ).

    delete it_zlest0185 WHERE viagem_id = lwa_zlest0185-viagem_id.
  ENDLOOP.

  "Pode vir carga de entrada ou de saída
  CLEAR: it_zsdt0001[].
  SELECT ch_referencia, id_ordem, nro_nf_prod INTO TABLE @it_zsdt0001
    FROM zsdt0001
     FOR ALL ENTRIES IN @it_zlest0185
   WHERE id_ordem    EQ @it_zlest0185-id_ordem.

  DELETE IT_ZSDT0001 WHERE NRO_NF_PROD IS INITIAL.

  CHECK it_zsdt0001[] IS NOT INITIAL.

  it_zsdt0001_s[] = it_zsdt0001[].

  "Selecionar Notas
  SELECT docnum, datachegada, datatransb, pesochegada, pesotransb
    INTO TABLE @DATA(it_zlest0039)
    FROM zlest0039
     FOR ALL ENTRIES IN @it_zsdt0001
   WHERE docnum EQ @it_zsdt0001-nro_nf_prod.

  SORT it_zlest0039 BY docnum.

  DELETE it_zlest0039 WHERE datachegada IS INITIAL AND datatransb IS INITIAL.

  LOOP AT it_zlest0185 INTO DATA(wa_zlest0185).

    CLEAR: wa_zlest0185-dt_descarga,
           wa_zlest0185-nm_peso_chegada.

    DATA(ck_enviar) = abap_true.

    LOOP AT it_zsdt0001_s INTO DATA(wa_zsdt0001)
      WHERE id_ordem EQ wa_zlest0185-id_ordem.

      IF wa_zsdt0001-nro_nf_prod IS INITIAL.
        ck_enviar = abap_false.
      ENDIF.

      READ TABLE it_zlest0039 INTO DATA(wa_zlest0039) WITH KEY docnum = wa_zsdt0001-nro_nf_prod BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF wa_zlest0039-datatransb IS NOT INITIAL.
          wa_zlest0185-dt_descarga = wa_zlest0039-datatransb.
          ADD wa_zlest0039-pesotransb TO wa_zlest0185-nm_peso_chegada.
        ELSE.
          wa_zlest0185-dt_descarga = wa_zlest0039-datachegada.
          ADD wa_zlest0039-pesochegada TO wa_zlest0185-nm_peso_chegada.
        ENDIF.
      ELSE.
        ck_enviar = abap_false.
      ENDIF.

    ENDLOOP.

    IF ck_enviar EQ abap_true AND wa_zlest0185-nm_peso_chegada IS NOT INITIAL.

      TRY .
          zcl_integracao_viagem_descarg=>zif_integracao_viagem_descarg~get_instance(
            )->set_viagem_descarregar(
            EXPORTING
              i_viagem_id             = wa_zlest0185-viagem_id       " Identificador de Viagem CARGUERO
              i_dt_descarga           = wa_zlest0185-dt_descarga     " Data Descarregamento
              i_peso_liquido          = wa_zlest0185-nm_peso_chegada " Peso Liquido Total
          ).

        CATCH zcx_integracao INTO DATA(ex_integracao).

          IF NOT ( ex_integracao->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
                   ex_integracao->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
            MESSAGE ID ex_integracao->msgid TYPE 'S' NUMBER ex_integracao->msgno WITH ex_integracao->msgv1 ex_integracao->msgv2 ex_integracao->msgv3 ex_integracao->msgv4 DISPLAY LIKE 'E'.
          ENDIF.

        CATCH zcx_error INTO DATA(ex_error).
          MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 DISPLAY LIKE 'E'.
      ENDTRY.

    ENDIF.


  ENDLOOP.

  PERFORM f_trata_pedido_transferencia.

*&---------------------------------------------------------------------*
* PEDIDOS TRANSFERENCIA
*&---------------------------------------------------------------------*
FORM f_trata_pedido_transferencia..

  vl_data_lim = sy-datum - 15.

  FREE : it_rom_sai_transf[],
         it_rom_ent_transf[],
         v_candat_null.

  SELECT vbeln, nro_nf_prod, nro_nf_frete, id_ordem
    FROM zsdt0001 INTO CORRESPONDING FIELDS OF TABLE @it_rom_sai_transf
   WHERE dt_movimento GE @vl_data_lim
     AND tp_movimento EQ 'S'.

  LOOP AT it_rom_sai_transf ASSIGNING FIELD-SYMBOL(<fs_rom_sai_transf>).

    <fs_rom_sai_transf>-nro_nf_prod = |{ <fs_rom_sai_transf>-nro_nf_prod ALPHA = IN }|.

    CHECK ( <fs_rom_sai_transf>-nro_nf_prod IS NOT INITIAL ) AND ( <fs_rom_sai_transf>-vbeln IS NOT INITIAL ).

    SELECT SINGLE *
      FROM ekko INTO @DATA(_wl_ekko)
     WHERE ebeln = @<fs_rom_sai_transf>-vbeln.

    CHECK ( sy-subrc EQ 0 ) AND ( _wl_ekko-bsart = 'ZUB' ).

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(_wl_doc)
     WHERE docnum EQ @<fs_rom_sai_transf>-nro_nf_prod
       AND candat EQ @v_candat_null
       AND cancel EQ @space.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM j_1bnfe_active INTO @DATA(_wl_active)
     WHERE docnum = @<fs_rom_sai_transf>-nro_nf_prod
       AND docsta = '1'.

    CHECK sy-subrc EQ 0.

    CONCATENATE _wl_active-regio
                _wl_active-nfyear
                _wl_active-nfmonth
                _wl_active-stcd1
                _wl_active-model
                _wl_active-serie
                _wl_active-nfnum9
                _wl_active-docnum9
                _wl_active-cdv INTO <fs_rom_sai_transf>-chave_nfe.

    IF strlen( <fs_rom_sai_transf>-chave_nfe ) EQ 44.
      <fs_rom_sai_transf>-transferencia = abap_true.
    ENDIF.

  ENDLOOP.

  DELETE it_rom_sai_transf WHERE ( transferencia EQ abap_false ) OR ( chave_nfe IS INITIAL ).

  CHECK it_rom_sai_transf[] IS NOT INITIAL.

*-------------------------------
*-romaneio de entrada
*-------------------------------
* SELECT *
*   FROM zsdt0001 INTO TABLE it_rom_ent_transf
*    FOR ALL ENTRIES IN it_rom_sai_transf
*  WHERE chave_nfe    = it_rom_sai_transf-chave_nfe
*    AND tp_movimento = 'E'.
*
* CHECK it_rom_ent_transf[] IS NOT INITIAL.

  LOOP AT it_rom_sai_transf.

    FREE: wa_rom_ent_transf.

*-------------------------------
*-romaneio de entrada
*-------------------------------
    SELECT *
      FROM zsdt0001 INTO wa_rom_ent_transf
        UP TO 1 ROWS
     WHERE chave_nfe    = it_rom_sai_transf-chave_nfe
       AND tp_movimento = 'E'.
    ENDSELECT.

    CHECK ( sy-subrc = 0 ) AND ( it_rom_sai_transf-id_ordem is NOT INITIAL ).

    "Viagens não informado descarregamento
    SELECT *
      INTO @DATA(wa_zlest0185)
      FROM zlest0185
        UP TO 1 ROWS
     WHERE id_ordem = @it_rom_sai_transf-id_ordem.
    ENDSELECT.

    CHECK ( sy-subrc = 0 ) AND ( wa_zlest0185-CK_DESCARREGADO is INITIAL ) AND ( WA_ZLEST0185-CK_CARREGADO is NOT INITIAL ).
*   CHECK wa_zlest0185-nm_peso_chegada IS NOT INITIAL.

    v_viagem_id = wa_zlest0185-viagem_id.
    v_peso_liq  = wa_rom_ent_transf-peso_subtotal.

    TRY .
        zcl_integracao_viagem_descarg=>zif_integracao_viagem_descarg~get_instance(
          )->set_viagem_descarregar(
          EXPORTING
            i_viagem_id             = v_viagem_id                      " Identificador de Viagem CARGUERO
            i_dt_descarga           = wa_rom_ent_transf-dt_movimento   " Data Descarregamento
            i_peso_liquido          = v_peso_liq                       " Peso Liquido Total
        ).

      CATCH zcx_integracao INTO DATA(ex_integracao).

        IF NOT ( ex_integracao->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
                 ex_integracao->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
          MESSAGE ID ex_integracao->msgid TYPE 'S' NUMBER ex_integracao->msgno WITH ex_integracao->msgv1 ex_integracao->msgv2 ex_integracao->msgv3 ex_integracao->msgv4 DISPLAY LIKE 'E'.
        ENDIF.

      CATCH zcx_error INTO DATA(ex_error).
        MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 DISPLAY LIKE 'E'.
    ENDTRY.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
