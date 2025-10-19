*----------------------------------------------------------------------*
*                   AMAGGI                                             *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZLESR0159                                               *
* Descrição  : Transbordo intermunicipal                               *
* Módulo     : LES                          Transação:                 *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Fernando W. Luvizotte                  Data: 08/07/2022 *
* Observações:                                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*
REPORT zlesr0159.

TYPES: BEGIN OF ty_zlest0108,
         id_ordem  TYPE zde_id_ordem,
         nur_ordem TYPE zde_nr_ordem,
         nr_safra  TYPE zde_nr_safra.
TYPES: END OF ty_zlest0108.

TYPES: BEGIN OF ty_zsd1,
         n_romaneio TYPE zch_ref,
         numero     TYPE j_1bnfnum9,
         parid      TYPE zsdt0001-parid,   "Nº Cliente/Fornecedor
         series     TYPE zsdt0001-series,  "Serie
         docdat     TYPE zsdt0001-docdat  , "Data de emissão.
         bukrs      TYPE zsdt0001-bukrs,       "Nº empresa
         branch     TYPE zsdt0001-branch,      "Filial
         nr_safra   TYPE zsdt0001-nr_safra.   "Safra.
TYPES: END OF ty_zsd1.



DATA: tg_zsdt0001_e TYPE TABLE OF zsdt0001,
      tg_zsdt0001_s TYPE TABLE OF zsdt0001,
      tg_zlest0110  TYPE TABLE OF zlest0110,
      tg_zlest0108  TYPE TABLE OF zlest0108,
      tg_zsdt0001od TYPE TABLE OF zsdt0001od,
      it_zlese0159  TYPE  TABLE OF zlese0159,
      tg_likp       TYPE TABLE OF likp,
      tg_vttk       TYPE TABLE OF vttk,
      tg_zlest0185  TYPE TABLE OF zlest0185,
      tg_zsd108     TYPE TABLE OF ty_zlest0108,
      tg_zsd1       TYPE TABLE OF ty_zsd1.

DATA: wg_zsdt0001     LIKE LINE OF tg_zsdt0001_e,
      wg_zlest0110    LIKE LINE OF tg_zlest0110,
      wg_zlest0108    LIKE LINE OF tg_zlest0108,
      wg_zsdt0001od   LIKE LINE OF tg_zsdt0001od,
      wg_likp         LIKE LINE OF tg_likp,
      wg_zsd1         LIKE LINE OF tg_zsd1,
      vr_qtd_hora_ref TYPE p,
      wg_vttk         TYPE vttk,
      wg_zlest0185    TYPE zlest0185,
      vr_qtd_hora     TYPE p,
      ws_zlese0159    TYPE  zlese0159,
      lg_dias         TYPE i.

DATA: r_idreferencia TYPE RANGE OF j_1bnfnum9.

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

  PERFORM zf_limpa_dados.

  PERFORM zf_tvarv.

  PERFORM zf_seleciona.

  PERFORM zf_proc_saida.



*&---------------------------------------------------------------------*
*&      Form  ZF_LIMPA_DADOS
*&---------------------------------------------------------------------*
FORM zf_limpa_dados .

  CLEAR: tg_zsdt0001_e[],
         tg_likp,
         tg_zlest0110.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA
*&---------------------------------------------------------------------*
FORM zf_seleciona .



  DATA: tb_0110    TYPE TABLE OF zlest0110,
        ws_return  TYPE REF TO data,
        r_id_ordem TYPE RANGE OF zde_id_ordem,
        r_nr_ordem TYPE RANGE OF zde_nr_ordem,
        lv_corte   TYPE datum.

  lv_corte = sy-datum - lg_dias.

  CHECK lv_corte IS NOT INITIAL.

  SELECT * INTO TABLE tg_zsdt0001_e
    FROM zsdt0001
    WHERE dt_atualizacao >= lv_corte
      AND  tp_movimento   = 'E'.

  CHECK tg_zsdt0001_e IS NOT INITIAL.

  FREE: tg_zsd1.  CLEAR: wg_zsdt0001.

  LOOP AT tg_zsdt0001_e INTO wg_zsdt0001.
    CLEAR wg_zsd1.
    MOVE-CORRESPONDING wg_zsdt0001 TO wg_zsd1.
    wg_zsd1-n_romaneio = |{ wg_zsdt0001-nr_romaneio ALPHA = IN }|.
    wg_zsd1-numero = CONV #( wg_zsdt0001-nfnum ) .
    APPEND wg_zsd1 TO tg_zsd1.

    wg_zsd1-n_romaneio = |{ wg_zsdt0001-nr_romaneio ALPHA = OUT }|.
    APPEND wg_zsd1 TO tg_zsd1.
  ENDLOOP.


  SELECT * INTO TABLE tb_0110
    FROM zlest0110
    FOR ALL ENTRIES IN tg_zsd1
    WHERE numero     EQ tg_zsd1-numero "Nº da NFe
      AND cliente    EQ tg_zsd1-parid  "Nº Cliente/Fornecedor
      AND serie      EQ tg_zsd1-series "Serie
      AND dtemissao  EQ tg_zsd1-docdat. "Data de emissão.

  CHECK tb_0110 IS NOT INITIAL.

  SELECT * INTO TABLE tg_likp
    FROM likp
    FOR ALL ENTRIES IN tb_0110
    WHERE vbeln = tb_0110-vbeln.

  CHECK tg_likp IS NOT INITIAL.

  LOOP AT  tg_likp INTO wg_likp.
    READ TABLE tb_0110 INTO wg_zlest0110
    WITH KEY vbeln = wg_likp-vbeln.
    IF sy-subrc EQ 0.
      APPEND wg_zlest0110 TO tg_zlest0110.
    ENDIF.
  ENDLOOP.

  "Verificar se a nota pertence a um frete Intermunicipal.
  SELECT  * INTO TABLE tg_zlest0108
  FROM zlest0108
    FOR ALL ENTRIES IN tg_zlest0110
    WHERE vbeln = tg_zlest0110-vbeln.

  CHECK tg_zlest0108 IS NOT INITIAL.
  tg_zsd108 = VALUE #( FOR l IN tg_zlest0108 ( nr_safra = l-safra_ordem_car id_ordem = l-id_ordem nur_ordem = l-nro_ordem_car ) ).

  SELECT  *
  FROM vttk
  INTO TABLE tg_vttk
  FOR ALL ENTRIES IN tg_zlest0108
  WHERE tknum EQ tg_zlest0108-doc_transp.

  IF tg_vttk IS NOT INITIAL.
    SELECT  *
    FROM zlest0185
    INTO TABLE tg_zlest0185
    FOR ALL ENTRIES IN tg_vttk
    WHERE viagem_id EQ tg_vttk-id_viagem.
  ENDIF.

  IF tg_zsd108 IS NOT INITIAL.
    SELECT * INTO TABLE tg_zsdt0001od
    FROM zsdt0001od
    FOR ALL ENTRIES IN tg_zsd108
   WHERE nr_ordem  EQ tg_zsd108-nur_ordem
     AND nr_safra  EQ tg_zsd108-nr_safra.

    SELECT *
      FROM zsdt0001od
        APPENDING TABLE tg_zsdt0001od
      FOR ALL ENTRIES IN tg_zsd108
     WHERE id_ordem EQ tg_zsd108-id_ordem.
  ENDIF.

  CHECK tg_zsdt0001od IS NOT INITIAL.

*---> 04/07/2023 - Migração S4 - WS
  SORT  tg_zsdt0001od BY nr_safra nr_ordem id_ordem.
*<--- 04/07/2023 - Migração S4 - WS
  DELETE ADJACENT DUPLICATES FROM tg_zsdt0001od COMPARING nr_safra nr_ordem id_ordem.

  "Verifica as ordem e retira da fila.
  LOOP AT tg_zsdt0001od INTO wg_zsdt0001od WHERE tp_status EQ 'FE'.
    READ TABLE tg_zlest0108 INTO wg_zlest0108
    WITH  KEY id_ordem = wg_zsdt0001od-id_ordem
       safra_ordem_car = wg_zsdt0001od-nr_safra.

    IF sy-subrc EQ 0. "Verifica documento transporte.
      READ TABLE tg_vttk INTO wg_vttk
      WITH  KEY tknum = wg_zlest0108-doc_transp.

      IF sy-subrc EQ 0.
        READ TABLE tg_zlest0185 INTO wg_zlest0185
        WITH  KEY viagem_id = wg_vttk-id_viagem.

        "Verifica viagem no Carguero.
        IF wg_zlest0185-ck_carregado EQ abap_true.
          DELETE TABLE tg_zlest0108 FROM wg_zlest0108.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: wg_zlest0185, wg_vttk, wg_zlest0108.
  ENDLOOP.

  CHECK tg_zlest0108 IS NOT INITIAL.

  SELECT * INTO TABLE tg_zsdt0001_s
    FROM zsdt0001
    FOR ALL ENTRIES IN tg_zsd1
    WHERE bukrs          EQ tg_zsd1-bukrs "Nº da NFe
      AND branch         EQ tg_zsd1-branch  "Nº Cliente/Fornecedor
      AND nr_safra       EQ tg_zsd1-nr_safra  "Serie
      AND id_referencia  EQ tg_zsd1-n_romaneio. "Data de emissão.


  "Prepara dados.
  LOOP AT tg_zlest0108 INTO DATA(ws_zles0108).
    READ TABLE tg_zlest0110 INTO DATA(ws_zlest0110) WITH KEY vbeln = ws_zles0108-vbeln.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE tg_zsdt0001_e INTO DATA(ws_zsdt0001_e)  WITH KEY  nfnum   = ws_zlest0110-numero     "Nº da NFe
                                                                 parid   = ws_zlest0110-cliente    "Nº Cliente/Fornecedor
                                                                 series  = ws_zlest0110-serie      "Serie
                                                                 docdat  = ws_zlest0110-dtemissao. "Data de emissão.

    IF sy-subrc EQ 0.
      vr_qtd_hora_ref = 2.
      "Verificar se a mais de duas horas de diferença.
      CLEAR: vr_qtd_hora.
      IF sy-datum EQ ws_zsdt0001_e-dt_atualizacao.
        vr_qtd_hora = ( sy-uzeit - ws_zsdt0001_e-hr_atualizacao ) / 60.
        vr_qtd_hora = vr_qtd_hora / 60.
        IF vr_qtd_hora <= vr_qtd_hora_ref.
          CONTINUE.
        ENDIF.
      ENDIF.



      READ TABLE tg_zsdt0001_s INTO DATA(ws_zsdt0001_s)  WITH KEY  bukrs          = ws_zsdt0001_e-bukrs          "empresa
                                                                   branch         = ws_zsdt0001_e-branch         "filial
                                                                   nr_safra       = ws_zsdt0001_e-nr_safra       "safra
                                                                   id_referencia  = ws_zsdt0001_e-id_referencia. "id referencia.


      IF sy-subrc NE 0.

        "Verificar se o romaneio vinculado  ao registro da ZLEST0108
        READ TABLE  tg_zsdt0001od INTO DATA(ws_zsdt0001od) WITH KEY nr_ordem  = ws_zles0108-nro_ordem_car
                                                                    nr_safra   = ws_zles0108-safra_ordem_car.

        IF sy-subrc EQ 0.
          APPEND VALUE #(
                           nr_safra  = ws_zsdt0001od-nr_safra
                           nr_ordem  = ws_zsdt0001od-nr_ordem
                           vbeln_vl  = ws_zles0108-vbeln
                           tknum     = ws_zles0108-doc_transp
                           ch_ref_rom_ent = ws_zsdt0001_e-ch_referencia
                           tp_status = 'FE' ) TO it_zlese0159.





        ELSE.

          READ TABLE  tg_zsdt0001od INTO ws_zsdt0001od WITH KEY id_ordem  = ws_zles0108-id_ordem.
          IF sy-subrc EQ 0.
            APPEND VALUE #(
                             id_ordem  = |{ ws_zsdt0001od-id_ordem ALPHA = OUT }|
                             vbeln_vl  = ws_zles0108-vbeln
                             tknum     = ws_zles0108-doc_transp
                             ch_ref_rom_ent = ws_zsdt0001_e-ch_referencia
                             tp_status = 'FE' ) TO it_zlese0159.



          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


    CLEAR: ws_zsdt0001od, ws_zsdt0001_s, ws_zsdt0001_e, ws_zlest0110.
  ENDLOOP.



ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_TVARV
*&---------------------------------------------------------------------*
FORM zf_tvarv .

  DATA: lv_low TYPE rvari_val_255.

  CONSTANTS: c_name TYPE rvari_vnam VALUE 'ZLESR0159_DIAS'.

  SELECT SINGLE low INTO lv_low
    FROM tvarvc
    WHERE name = c_name.
  IF sy-subrc NE 0.
    STOP.
  ENDIF.
  CONDENSE lv_low  NO-GAPS.
  MOVE lv_low TO lg_dias.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PROC_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_proc_saida .
  CHECK it_zlese0159 IS NOT INITIAL.
  DATA: e_data TYPE string.

  DATA: LVA_ST_OC TYPE ZSDT0001OD-tp_status.

  DATA: lit_likp TYPE TABLE OF likp.

  LOOP AT it_zlese0159 INTO ws_zlese0159.

    CLEAR: lva_st_oc.

    IF ws_zlese0159-id_ordem IS NOT INITIAL.
      READ TABLE  tg_zsdt0001od INTO DATA(ws_zsdt0001od) WITH KEY id_ordem  = ws_zlese0159-id_ordem.
      IF SY-SUBRC EQ 0.
        lva_st_oc = ws_zsdt0001od-tp_status.
      ENDIF.
    ELSEIF  ws_zlese0159-nr_ordem IS NOT INITIAL AND  ws_zlese0159-nr_safra IS NOT INITIAL.
      READ TABLE  tg_zsdt0001od INTO ws_zsdt0001od WITH KEY nr_ordem  = ws_zlese0159-nr_ordem
                                                            nr_safra  = ws_zlese0159-nr_safra.
      IF SY-SUBRC EQ 0.
        lva_st_oc = ws_zsdt0001od-tp_status.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

    IF lva_st_oc NE 'FE'. "Verifica se ordem ja esta fechada.
      TRY.
          "Executa a API.
          CLEAR: e_data.
          zcl_int_ordem_carrega_opus=>zif_int_ordem_carrega_opus~get_instance(
                )->set_dados_ordem_carregamento( i_data = ws_zlese0159
                )->post_ordem_car_opus( CHANGING e_data = e_data ).


        CATCH zcx_integracao INTO DATA(ex_integra).    "
          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

        CATCH zcx_error INTO DATA(ex_error).    "  "
          ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

      ENDTRY.
    ENDIF.

*----------------------------------------------------------------------------------*
*   Informar carregamento Carguero
*----------------------------------------------------------------------------------*
    CHECK zcl_faturamento=>zif_faturamento~get_romaneio_trocanota( EXPORTING i_vbeln = ws_zlese0159-vbeln_vl ) = abap_false.

    SELECT SINGLE *
      FROM zlest0108 INTO @DATA(lwa_zlest0108)
     WHERE vbeln EQ @ws_zlese0159-vbeln_vl.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zlest0109 INTO @DATA(lwa_zlest0109)
     WHERE vbeln EQ @ws_zlese0159-vbeln_vl.

    CHECK ( sy-subrc EQ 0 ) AND ( lwa_zlest0109-peso_bruto IS NOT INITIAL ) AND ( lwa_zlest0109-peso_tara IS NOT INITIAL ).

    CLEAR: lit_likp[].

    SELECT *
      FROM likp INTO TABLE lit_likp
     WHERE vbeln EQ ws_zlese0159-vbeln_vl.

    CHECK lit_likp[] IS NOT INITIAL.

    SELECT SINGLE *
      FROM vttk INTO @DATA(lwa_vttk)
     WHERE tknum = @ws_zlese0159-tknum.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zlest0185 INTO @DATA(lwa_zlest0185)
     WHERE id_ordem = @ws_zlese0159-id_ordem.

    CHECK sy-subrc EQ 0.

    "Início do Transporte
    TRY .
        zcl_integracao_viagem_carregar=>zif_integracao_viagem_carregar~get_instance(
          )->set_viagem_carregar(
          EXPORTING
            i_viagem_id              = CONV #( lwa_vttk-id_viagem )
            i_dt_carregamento        = CONV #( lwa_vttk-erdat )    " Data Carregamento
            i_peso_tara              = CONV #( lwa_zlest0109-peso_tara  )
            i_peso_liquido           = CONV #( lwa_zlest0109-qtde_aviso )
          ).
      CATCH zcx_integracao INTO DATA(ex_integracao).

*         IF NOT ( ex_integracao->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
*                  ex_integracao->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
*           MESSAGE ID ex_integracao->msgid TYPE 'E'
*           NUMBER ex_integracao->msgno WITH ex_integracao->msgv1 ex_integracao->msgv2 ex_integracao->msgv3 ex_integracao->msgv4 RAISING erro.
*         ENDIF.

      CATCH zcx_error INTO DATA(ex_error2).

*         MESSAGE ID ex_error->msgid TYPE 'E'
*         NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 RAISING erro.

    ENDTRY.

  ENDLOOP.

ENDFORM.
