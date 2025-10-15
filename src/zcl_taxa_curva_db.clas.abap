class ZCL_TAXA_CURVA_DB definition
  public
  create public .

*"* public components of class ZCL_TAXA_CURVA_DB
*"* do not include other source files here!!!
public section.

  interfaces ZIF_TAXA_CURVA_DB .

  data AT_IN_DT_ENT type ZIN_DT_ENTREGA .

  methods ENCERRAMENTO
    importing
      !I_NUMERO type ZSDED013
      !I_ESTORNO type NUM10 optional
      !I_TCODE type SYTCODE optional
      !I_STATUS type C optional
      !I_FIXACAO type POSNR optional
      !I_AUART type AUART optional
      !I_VBELN type VBELN optional
      !I_VBAP type VBAP_T optional .
  methods FREE .
  methods LIBERAR_OV
    importing
      !I_NUMERO type ZSDED013
      !I_ESTORNO type NUM10 optional
      !I_TCODE type SYTCODE optional
      !I_FIXACAO type POSNR optional .
  methods FRAME
    importing
      !I_NUMERO type ZSDED013
      !I_UCOMM type SYUCOMM optional
      !I_TCODE type SYTCODE optional
      !I_AUART type AUART optional
      !I_VBELN type VBELN optional .
  methods EDICAO
    importing
      !I_NUMERO type ZSDED013
      !I_FIXACAO type POSNR optional
      !I_UCOMM type SYUCOMM optional
      !I_VBELN type VBELN optional
      !I_TCODE type SYTCODE optional .
  methods FRETE
    importing
      !I_NUMERO type ZSDED013
      !I_FIXACAO type POSNR
      !I_STATUS type C optional
      !I_TCODE type SYTCODE optional
      !I_VBELN type VBELN optional
      !I_AUART type AUART optional
      !I_UCOMM type SYUCOMM optional .
  methods FRAME_EDICAO
    importing
      !I_NUMERO type ZSDED013
      !I_FIXACAO type POSNR
      !I_BEZEI type BEZEI30 optional
      !I_TCODE type SYTCODE optional
      !I_VBELN type VBELN optional
      !I_AUART type AUART optional
      !I_TIPO type CHAR3 optional
      !I_STATUS type CHAR1 optional .
  methods FRAME_PARCIAL
    importing
      !I_NUMERO type ZSDED013
      !I_FIXACAO type POSNR
      !I_TCODE type SYTCODE optional
      !I_UCOMM type SYUCOMM optional .
  methods STATUS_DEVOLUCAO
    importing
      !I_NUMERO type ZSDED013
      !I_ESTORNO type NUM10 optional
      !I_TCODE type SYTCODE optional
      !I_FIXACAO type POSNR optional
      !I_AUART type AUART optional .
  methods CALCULA_FRETE
    importing
      !I_NUMERO type ZSDED013
      !I_FIXACAO type POSNR optional
      !I_UCOMM type SYUCOMM optional
      !I_VBELN type VBELN optional
      !I_TCODE type SYTCODE optional .
  methods TOTAL_94
    importing
      !I_NUMERO type ZSDED013
      !I_FIXACAO type POSNR optional
    exporting
      value(E_DIFE) type DMBTR .
  methods VENDA_IN
    importing
      !I_NUMERO type ZSDED003 optional
      !I_TIPO type CHAR3 optional
      !I_ACAO type SY-UCOMM optional
      !T_ITENS type STANDARD TABLE optional
      !I_0090 type ZSDT0090 optional
      !I_DIR type BEZEI30 optional
      !I_TAXA_BOLETA type UKURSP optional .
  methods FRETE_IN
    importing
      !I_NUMERO type ZSDED003 optional
      !I_TIPO type CHAR3 optional
      !I_ACAO type SY-UCOMM optional
      !T_ITENS type STANDARD TABLE optional
      !I_0090 type ZSDT0090 optional
      !I_DIR type BEZEI30 optional
      !I_TAXA_BOLETA type UKURSP optional .
  methods ESTORNO_IN
    importing
      !I_NUMERO type ZSDED003 optional
      !I_TIPO type CHAR3 optional
      !I_VBELN type VBELN optional
      !I_DIR type BEZEI30 optional
      !I_SEQ type NUMC4 optional
      !I_0090 type ZSDT0090 optional
      !I_0090_MANUAL type FLAG optional .
  methods REVERSAO_FRETE_IN
    importing
      !I_NUMERO type ZSDED003 optional
      !I_TIPO type CHAR3 optional
      !I_VBELN type VBELN optional
      !I_DIR type BEZEI30 optional
      !I_SEQ type NUMC4 optional
      !I_0090 type ZSDT0090 optional .
  methods ESTORNO_AQV
    importing
      !VBELN type VBELN .
  methods FRETE_AQV
    importing
      !_VBRK type VBRK
      !_VBRP type VBRPVB
      !_AUART type AUART optional .
  class-methods DIA_UTIL
    importing
      !P_VENCIMENTO type ZSDT0054-VALDT
    exporting
      !E_SUBRC type SY-SUBRC .
  methods DIFERENCA
    importing
      !I_NUMERO type ZSDED013
      !I_FIXACAO type POSNR
      !I_TIPO type CHAR3
      !I_UCOMM type SYUCOMM optional
      !I_DIFERENCA type ZDMBTR
      !I_TCODE type SYTCODE optional .
  methods GET_PORCENTAGEM_FRETE
    exporting
      !PORC_FRETE type KURRF .
  methods DIFERENCA_IN
    importing
      !I_NUMERO type ZSDED013
      !I_FIXACAO type POSNR optional
      !I_TIPO type CHAR3
      !I_UCOMM type SYUCOMM optional
      !I_DIFERENCA type ZDMBTR
      !I_TCODE type SYTCODE optional
      !I_BEZEI type CHAR10 optional
      !I_WAERK type WAERK optional .
  methods FRETE_PEDIDO
    importing
      !_EKKO type EKKO
      !_EKPO type EKPO
      !_DIFERENCA type ZDMBTR
    raising
      ZCX_WEBSERVICE .
  methods GET_TOTAIS
    importing
      !I_NRO_SOL_OV type ZSDED013
    exporting
      !E_TOTAL_53 type DMBTR
      !E_TOTAL_54 type DMBTR
      !E_TOTAL_55 type ZSDED016
      !E_INDICE type ZINDICE
      !E_TOTAL_PESO_53 type DZMENG .
  methods ALTERA_DATA_ENTREGA
    importing
      !I_NUMERO type ZSDED003 optional
      !I_TIPO type CHAR3 optional
      !I_BEZEI type ZSDT0094-BEZEI optional
      !I_ACAO type SY-UCOMM optional
      !I_DT_ENTREGA type ZIN_DT_ENTREGA optional .
  methods CALC_CADENCIA_94
    importing
      !IT_0094 type ZSDC0094
      !IV_MATNR type MATNR
      !IV_DOC_SIMU type ZSDED013
    changing
      !CV_CADENCIA type DZMENG .
  PROTECTED SECTION.
*"* protected components of class ZCL_TAXA_CURVA_DB
*"* do not include other source files here!!!
private section.

*"* private components of class ZCL_TAXA_CURVA_DB
*"* do not include other source files here!!!
  data AT_TOTAL_53 type DMBTR .
  data AT_TOTAL_PESO_53 type DZMENG .
  data AT_TOTAL_54 type DMBTR .
  data AT_TOTAL_55 type ZSDED016 .
  data AT_TOTAL_94 type DMBTR .
  data AT_INDICE type ZINDICE .
  data AT_BIO_TAB type ZSDC0059 .
  data AT_MATKL type MATKL .

  methods TOTAL_53
    importing
      !I_NUMERO type ZSDED013
      !I_FIXACAO type POSNR optional .
  methods TOTAL_54
    importing
      !I_NUMERO type ZSDED013 .
  methods TOTAL_55
    importing
      !I_NUMERO type ZSDED013 .
  methods TOTAL_PESO_53
    importing
      !I_NUMERO type ZSDED013 .
  methods INDICE
    importing
      !I_TOTAL_53 type DMBTR
      !I_TOTAL_54 type DMBTR .
  methods CALC_DATA_VENC
    importing
      !I_DATA_INICIAL type D
      !I_DATA_FINAL type D
    returning
      value(E_DATA) type D .
  methods UPDATE_JOB
    importing
      !NRO_SOL_OV type ZSDED013
      !VL_VBELN type VBELN
      !TCODE type SYTCODE .
  methods TOTAL_94_FRE
    importing
      !I_NUMERO type ZSDED013
      !I_FIXACAO type POSNR
    exporting
      !E_DIFE type DMBTR .
  methods GET_VLR_BIO
    importing
      !IV_MATNR type MATNR
      !IV_DOC_SIMU type ZSDED013
      !IV_ACAO type CHAR1 optional
    changing
      !CV_CADENCIA type DZMENG .
ENDCLASS.



CLASS ZCL_TAXA_CURVA_DB IMPLEMENTATION.


  METHOD altera_data_entrega.

******************************************************************************
**** TABELA ZSDT0041 COM MATKL PARA AGRUPAR OS DADOS PELO GRUPO DE MERCADORIA
******************************************************************************
    TYPES BEGIN OF ty_0041.
            INCLUDE TYPE zsdt0041.
    TYPES matkl TYPE matkl.
    TYPES brgew TYPE brgew.
*---> S4 MIGRATION 10/07/2023 - MA
*    TYPES dtpgtcult TYPE bapi_jbd_dte_dzterm.
    TYPES dtpgtcult TYPE valdt.
*<--- S4 MIGRATION 10/07/2023 - MA
    TYPES kursk TYPE kursk.
    TYPES END OF ty_0041.

    TYPES BEGIN OF ty_0094.
            INCLUDE TYPE zsdt0094.
    TYPES doc_simulacao TYPE zsded003.
    TYPES matkl TYPE matkl.
    TYPES brgew TYPE brgew.
    TYPES spart TYPE spart.
    TYPES vlrtot TYPE zsded005.
    TYPES zmeng TYPE dzmeng.
    TYPES kursk TYPE kursk.
    TYPES END OF ty_0094.

    DATA: cont TYPE i.

***************************************************
**** OBJ DA TAXA CAMBIO E CURVA E OS METODOS DA 94
***************************************************
    DATA: obj_tx_curva TYPE REF TO zcl_webservice_tx_curva,
          obj_insere   TYPE REF TO zcl_taxa_curva_db,
          obj_0094     TYPE REF TO zcl_taxa_curva.

**************************************
****  TABELAS INTERNAS E WORK AREAS
**************************************
    DATA: it_0041        TYPE TABLE OF ty_0041,
          wa_0040        TYPE zsdt0040,
          wa_0041        TYPE zsdt0041,
          wa_0037        TYPE zsdt0037,
          var_total      TYPE dmbtr,
          lva_proporc    TYPE dmbtr,
          set_porc_frete TYPE p DECIMALS 5,
          tipo           TYPE char3,
          sequencia      TYPE posnr,
          vbeln          TYPE zsdt0090-vbeln,
          matnr          TYPE zsdt0090-matnr,
          vlr_frete      TYPE zsdt0037-vlr_frete,
          um_frete       TYPE zsdt0037-meins,
          vlr_frete_v    TYPE zsdt0037-vlr_frete,
          um_frete_v     TYPE zsdt0037-meins,
          it_zsdt0094    TYPE TABLE OF zsdt0094,
          wa_zsdt0094    TYPE zsdt0094.

    DATA: it_zsdt0094_input TYPE TABLE OF ty_0094,
          wa_zsdt0094_r     TYPE zsdt0094,
          wa_zsdt0094_h     TYPE zsdt0094.

    DATA: lva_dt_venc       TYPE zsdt0094-data_venc,
          lva_dt_venc_v     TYPE zsdt0094-data_venc,
          lva_dt_entrega    TYPE zsdt0094-data_venc,
          lva_vbeln         TYPE zsdt0094-vbeln,
          lva_posnr         TYPE zsdt0094-fixacao,
          lva_cadencia_qte  TYPE vbfa-rfmng,
          lva_cadencia      TYPE dzmeng,
          lva_total_proporc TYPE vbap-netwr,
          lva_taxa          TYPE zsdt0117-kursk,
          lva_s_subrc       TYPE sy-subrc,
          var_w             TYPE char1,
          s_subrc           TYPE sy-subrc.

    DATA: it_set TYPE TABLE OF rgsb4,
          wa_set TYPE rgsb4.

**********************
****** LIBERA OS OBJ
**********************
    FREE: obj_tx_curva, obj_0094, obj_insere.

**********************
****** CRIA OS OBJ
**********************
    CREATE OBJECT: obj_tx_curva, obj_0094, obj_insere.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class           = '0000'
        setnr           = 'MAGGI_FRI_HEDGE'
        no_descriptions = space
        no_rw_info      = space
      TABLES
        set_values      = it_set
      EXCEPTIONS
        set_not_found   = 1
        OTHERS          = 2.

    IF sy-subrc IS INITIAL.

      wa_set = it_set[ 1 ].

      CALL FUNCTION 'MOVE_CHAR_TO_NUM'
        EXPORTING
          chr             = wa_set-from
        IMPORTING
          num             = set_porc_frete
        EXCEPTIONS
          convt_no_number = 1
          convt_overflow  = 2
          OTHERS          = 3.

      set_porc_frete = set_porc_frete / 100.

    ENDIF.

    CASE sy-cprog.
      WHEN 'ZSDR0042'.

        CLEAR: wa_0040, wa_0041, wa_0037, it_zsdt0094,
               wa_zsdt0094_r, lva_dt_venc, lva_s_subrc, wa_zsdt0094_h.

        me->at_in_dt_ent = i_dt_entrega[].


*****************************
***** CABEÇARIO DA SIMULAÇÃO
*****************************

        LOOP AT me->at_in_dt_ent INTO DATA(wa_dt_entrega_venc).

          lva_dt_venc_v = wa_dt_entrega_venc-dt_entrega_v + 30.

          CLEAR: s_subrc.
          var_w = 0.
          WHILE var_w = 0.
            "Verifica se a data de vencimento é dia útil, caso não seja o sistema localiza a proxima data útil para ser o Vencimento.
            obj_insere->dia_util( EXPORTING p_vencimento  = lva_dt_venc_v
                                       IMPORTING e_subrc       = s_subrc
                                      ).
            IF s_subrc IS INITIAL.
              lva_dt_venc_v = lva_dt_venc_v + 1.
            ELSE.
              var_w = 1.
            ENDIF.
          ENDWHILE.

        ENDLOOP.

        SELECT SINGLE * FROM zsdt0040
          INTO wa_0040
          WHERE doc_simulacao EQ i_numero.

        SELECT SINGLE * FROM zsdt0117
          INTO @DATA(wl_0117)
          WHERE bukrs      EQ @wa_0040-vkorg
            AND desativado EQ @abap_false.

        SELECT *
         FROM zsdt0094
         INTO TABLE it_zsdt0094
         WHERE estorno = 0
           AND nro_sol_ov = i_numero
           AND tipo = 'FRI'
           AND data_venc = lva_dt_venc_v
           AND bezei = i_bezei.

        IF it_zsdt0094 IS NOT INITIAL.

          SORT: it_zsdt0094 STABLE BY data_registro ASCENDING
                                      hora_registro ASCENDING
                                      data_venc     ASCENDING.

          READ TABLE it_zsdt0094 INTO wa_zsdt0094 INDEX 1.

*****************************
***** CALCULO CADENCIA
*****************************

          CLEAR: lva_cadencia_qte.
          LOOP AT me->at_in_dt_ent INTO DATA(wa_dt_entrega_cad).
            IF wa_dt_entrega_cad-spart EQ '03'.
              lva_cadencia_qte = lva_cadencia_qte +  ( wa_dt_entrega_cad-sd_disp * wa_dt_entrega_cad-kbetr ) .
            ELSE.

              SELECT SINGLE * FROM mara
                INTO @DATA(wa_mara)
              WHERE matnr EQ @wa_dt_entrega_cad-matnr. "Do item

              obj_0094->set_matkl( i_matkl = wa_mara-matkl
                                   i_brgew = wa_mara-brgew ).

              obj_0094->set_zieme( wa_dt_entrega_cad-vrkme ).
              obj_0094->set_bezei( '' ).
              lva_cadencia = wa_dt_entrega_cad-sd_disp .
              obj_0094->set_cad_in_dentrega( i_cadencia = lva_cadencia ).

              lva_cadencia_qte = lva_cadencia_qte +  obj_0094->get_cadencia( ).

            ENDIF.

            lva_dt_entrega = wa_dt_entrega_cad-dt_entrega.
            lva_vbeln      = wa_dt_entrega_cad-vbeln.
            lva_posnr      = wa_dt_entrega_cad-posnr.

          ENDLOOP.

*****************************
***** CALCULO DO FRETE
*****************************

          LOOP AT me->at_in_dt_ent INTO DATA(wa_dt_entrega).

            SELECT SINGLE * FROM zsdt0041
              INTO wa_0041
               WHERE doc_simulacao EQ i_numero
                AND vbeln EQ wa_dt_entrega-vbeln
                AND matnr EQ wa_dt_entrega-matnr.

            IF ( wa_0041 IS NOT INITIAL ) AND ( wa_0041-inco1 NE 'FOB' ).

              vlr_frete = wa_0041-vlr_frete.
              um_frete  = wa_0041-zieme.

            ELSE.

              SELECT SINGLE *
                 FROM zsdt0037
                  INTO wa_0037
                   WHERE bukrs          EQ wa_0040-vkorg
                     AND matkl          EQ wa_dt_entrega-matklv "DT_ENTREGA
                     AND filial_origem  EQ wa_dt_entrega-werks "DT_ENTREGA-CENTRO
                     AND meins          EQ wa_dt_entrega-kmein "DT_ENTREGA-UM
                     AND filial_destino EQ wa_0040-vkbur
                     AND waers          EQ 'BRL'.

              vlr_frete = wa_0037-vlr_frete.
              um_frete  = wa_0037-meins.

            ENDIF.

            SELECT SINGLE * FROM mara
              INTO @DATA(wa_mara_cad)
            WHERE matnr EQ @wa_dt_entrega-matnr. "Do item

            obj_0094->set_matkl( i_matkl = wa_mara_cad-matkl
                                 i_brgew = wa_mara_cad-brgew ).

            obj_0094->set_frete_in( i_frete = vlr_frete
                                    i_zieme = um_frete ).


            IF wa_dt_entrega-spart EQ '03'.
              IF ( wa_0040-waerk = 'USD' ) AND ( wl_0117-kursk IS NOT INITIAL ).
                var_total =  ( ( ( lva_cadencia_qte ) * set_porc_frete )  ) * wl_0117-kursk .
              ELSE.
                var_total =   ( ( lva_cadencia_qte  ) * set_porc_frete ) .
              ENDIF.
            ELSE.
              var_total = obj_0094->get_total_proporcional( ).
            ENDIF.

            lva_proporc = lva_proporc + var_total.

          ENDLOOP.

          IF wa_dt_entrega-spart EQ '03'.
            lva_cadencia_qte = 0.
          ENDIF.

*---------------------------------------------------------------------*
*                   HEDGE VENCIDO
*---------------------------------------------------------------------*
          IF  sy-datum >  wa_zsdt0094-data_venc.

*** REVERSAO
            MOVE-CORRESPONDING wa_zsdt0094 TO wa_zsdt0094_r.

            lva_dt_venc =  sy-datum + 30.
            wa_zsdt0094_r-vbeln = lva_vbeln.
            wa_zsdt0094_r-fixacao = lva_posnr.

            var_w = 0.
            CLEAR: s_subrc.
            WHILE var_w = 0.
              "Verifica se a data de vencimento é dia útil, caso não seja o sistema localiza a proxima data útil para ser o Vencimento.
              obj_insere->dia_util( EXPORTING p_vencimento  = lva_dt_venc
                                         IMPORTING e_subrc       = s_subrc
                                        ).
              IF s_subrc IS INITIAL.
                lva_dt_venc = lva_dt_venc + 1.
              ELSE.
                var_w = 1.
              ENDIF.
            ENDWHILE.

            wa_zsdt0094_r-data_venc = lva_dt_venc.

            IF wa_zsdt0094-tipo_taxa EQ 'V'.
              wa_zsdt0094_r-tipo_taxa =  'C'.
              wa_zsdt0094_r-cadencia_qte  = lva_cadencia_qte .
              wa_zsdt0094_r-total_proporc = lva_proporc.
            ELSE.
              wa_zsdt0094_r-tipo_taxa =  'V'.
              wa_zsdt0094_r-cadencia_qte  = lva_cadencia_qte  * -1.
              wa_zsdt0094_r-total_proporc = lva_proporc * -1.
            ENDIF.

            wa_zsdt0094_r-data_lib = sy-datum.

            obj_tx_curva->buscar_taxa(
                         EXPORTING
                           i_data     = wa_zsdt0094_r-data_venc
                           i_data_lib = wa_zsdt0094_r-data_lib
                           i_tipo     = wa_zsdt0094_r-tipo_taxa
                           RECEIVING
                           e_cotacao = wa_zsdt0094_r-taxa_curva ).


            wa_zsdt0094_r-data_registro = sy-datum.
            wa_zsdt0094_r-hora_registro = sy-uzeit.

            APPEND  wa_zsdt0094_r TO it_zsdt0094_input.
            CLEAR: lva_taxa.

*** Novo Hedge
            CLEAR: lva_dt_venc.

            CLEAR: wa_zsdt0094_h.
            MOVE-CORRESPONDING wa_zsdt0094 TO wa_zsdt0094_h.

            lva_dt_venc  =  lva_dt_entrega + 30.
            "lva_dt_venc  =  lva_dt_venc + 30.
            wa_zsdt0094_h-vbeln = lva_vbeln.
            wa_zsdt0094_h-fixacao = lva_posnr.

            var_w = 0.
            CLEAR: s_subrc.
            WHILE var_w = 0.
              "Verifica se a data de vencimento é dia útil, caso não seja o sistema localiza a proxima data útil para ser o Vencimento.
              obj_insere->dia_util( EXPORTING p_vencimento  = lva_dt_venc
                                         IMPORTING e_subrc       = s_subrc
                                        ).
              IF s_subrc IS INITIAL.
                lva_dt_venc = lva_dt_venc + 1.
              ELSE.
                var_w = 1.
              ENDIF.
            ENDWHILE.

            wa_zsdt0094_h-data_lib  = sy-datum.
            wa_zsdt0094_h-data_venc = lva_dt_venc.

            IF wa_zsdt0094-tipo_taxa EQ 'C'.
              wa_zsdt0094_h-tipo_taxa =  'C'.
              wa_zsdt0094_h-cadencia_qte  = lva_cadencia_qte.
              wa_zsdt0094_h-total_proporc = lva_proporc.
            ELSE.
              wa_zsdt0094_h-tipo_taxa =  'V'.
              wa_zsdt0094_h-cadencia_qte  = lva_cadencia_qte * -1.
              wa_zsdt0094_h-total_proporc = lva_proporc * -1.
            ENDIF.

            obj_tx_curva->buscar_taxa(
                         EXPORTING
                           i_data     = wa_zsdt0094_h-data_venc
                           i_data_lib = wa_zsdt0094_h-data_lib
                           i_tipo     = wa_zsdt0094_h-tipo_taxa
                           RECEIVING
                           e_cotacao = wa_zsdt0094_h-taxa_curva ).

            wa_zsdt0094_h-taxa_cambio = wl_0117-kursk.

            wa_zsdt0094_h-data_registro = sy-datum.
            GET TIME FIELD wa_zsdt0094_h-hora_registro.

            APPEND wa_zsdt0094_h TO it_zsdt0094_input.
            CLEAR: lva_taxa.

          ELSE.

*---------------------------------------------------------------------*
*                   HEDGE NÃO VENCIDO
*---------------------------------------------------------------------*
*****************************
***** REVERSAO
*****************************
            CLEAR: wa_zsdt0094_r.
            MOVE-CORRESPONDING wa_zsdt0094 TO wa_zsdt0094_r.

            lva_taxa = wa_zsdt0094_r-taxa_cambio.
            wa_zsdt0094_r-vbeln = lva_vbeln.
            wa_zsdt0094_r-fixacao = lva_posnr.

            IF wa_zsdt0094-tipo_taxa EQ 'V'.
              wa_zsdt0094_r-tipo_taxa =  'C'.
              wa_zsdt0094_r-cadencia_qte  =  lva_cadencia_qte .
              wa_zsdt0094_r-total_proporc = lva_proporc.
            ELSE.
              wa_zsdt0094_r-tipo_taxa =  'V'.
              wa_zsdt0094_r-cadencia_qte  = lva_cadencia_qte * -1.
              wa_zsdt0094_r-total_proporc = lva_proporc * -1.
            ENDIF.

            wa_zsdt0094_r-data_lib = sy-datum.

            obj_tx_curva->buscar_taxa(
                         EXPORTING
                           i_data     = wa_zsdt0094_r-data_venc
                           i_data_lib = wa_zsdt0094_r-data_lib
                           i_tipo     = wa_zsdt0094_r-tipo_taxa
                           RECEIVING
                           e_cotacao = wa_zsdt0094_r-taxa_curva ).

            wa_zsdt0094_r-data_registro = sy-datum.
            wa_zsdt0094_r-hora_registro = sy-uzeit.

            APPEND wa_zsdt0094_r TO it_zsdt0094_input.
            CLEAR: lva_taxa.

*****************************
***** NOVO HEDGE
*****************************

            CLEAR: wa_zsdt0094_h.
            MOVE-CORRESPONDING wa_zsdt0094 TO wa_zsdt0094_h.


            lva_dt_venc  =  lva_dt_entrega + 30.
            wa_zsdt0094_h-vbeln = lva_vbeln.
            wa_zsdt0094_h-fixacao = lva_posnr.

            var_w = 0.
            CLEAR: s_subrc.
            WHILE var_w = 0.
              "Verifica se a data de vencimento é dia útil, caso não seja o sistema localiza a proxima data útil para ser o Vencimento.
              obj_insere->dia_util( EXPORTING p_vencimento  = lva_dt_venc
                                         IMPORTING e_subrc       = s_subrc
                                        ).
              IF s_subrc IS INITIAL.
                lva_dt_venc = lva_dt_venc + 1.
              ELSE.
                var_w = 1.
              ENDIF.
            ENDWHILE.


            wa_zsdt0094_h-data_lib  = sy-datum.
            wa_zsdt0094_h-data_venc = lva_dt_venc.

            IF wa_zsdt0094-tipo_taxa EQ 'C'.
              wa_zsdt0094_h-tipo_taxa =  'C'.

              wa_zsdt0094_h-cadencia_qte  = lva_cadencia_qte.
              wa_zsdt0094_h-total_proporc = lva_proporc.

            ELSE.
              wa_zsdt0094_h-tipo_taxa =  'V'.

              wa_zsdt0094_h-cadencia_qte  = lva_cadencia_qte * -1.
              wa_zsdt0094_h-total_proporc = lva_proporc * -1.

            ENDIF.

            obj_tx_curva->buscar_taxa(
                         EXPORTING
                           i_data     = wa_zsdt0094_h-data_venc
                           i_data_lib = wa_zsdt0094_h-data_lib
                           i_tipo     = wa_zsdt0094_h-tipo_taxa
                           RECEIVING
                           e_cotacao = wa_zsdt0094_h-taxa_curva ).

            wa_zsdt0094_h-data_registro = sy-datum.
            wa_zsdt0094_h-taxa_cambio = wl_0117-kursk.

            GET TIME FIELD wa_zsdt0094_h-hora_registro.

            APPEND wa_zsdt0094_h TO it_zsdt0094_input.

            CLEAR: lva_taxa.

          ENDIF.

        ENDIF.

        tipo = i_tipo.

        LOOP AT it_zsdt0094_input ASSIGNING FIELD-SYMBOL(<final>).

          obj_0094->set_numero( wa_dt_entrega-doc_simulacao ).
          obj_0094->set_posnr( <final>-fixacao ).
          obj_0094->set_data_venc( <final>-data_venc ).
          obj_0094->set_data_lib( <final>-data_lib  ).
          obj_0094->set_cadencia( <final>-cadencia_qte ).
          obj_0094->set_zieme( <final>-zieme ).
          obj_0094->set_total_proporcional( i_total =  <final>-total_proporc
                                            i_negativa = abap_false
                                           ).
          obj_0094->set_taxa_curva( <final>-taxa_curva ).
          obj_0094->set_taxa_cambio( <final>-taxa_cambio ).
          obj_0094->set_frete_cif( <final>-frete_cif ).
          obj_0094->set_frete_porto( <final>-frete_porto ).
          obj_0094->set_tipo( i_tipo ).
          obj_0094->set_bezei( i_bezei ).
          obj_0094->set_estorno( <final>-estorno ).
          obj_0094->tipo_taxa_in( '' ).
          obj_0094->set_tipo_taxa( <final>-tipo_taxa ).
          obj_0094->set_vbeln( <final>-vbeln ).
          obj_0094->set_incoterms( <final>-inco1 ).
          obj_0094->set_safra( <final>-safra ).
          obj_0094->set_data_lib( <final>-data_lib ).

          obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).

        ENDLOOP.
    ENDCASE.

  ENDMETHOD.


  METHOD CALCULA_FRETE.

    CONSTANTS: VAR_X TYPE C VALUE 'X'.

    DATA: GT_ZSDT0094 TYPE TABLE OF ZSDT0094,
          GW_ZSDT0094 TYPE ZSDT0094.

    TYPES: BEGIN OF TY_SALDO,
             NRO_SOL_OV TYPE ZSDT0055-NRO_SOL_OV,
             DATA       TYPE ZSDT0055-DATA_PROGR,
             VALOR      TYPE ZSDT0055-CADENCIA_QTE,
           END OF TY_SALDO.

    DATA: GT_SALDO TYPE TABLE OF TY_SALDO,
          GW_SALDO TYPE TY_SALDO.

    DATA: GT_ZSDT0053 TYPE TABLE OF ZSDT0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
          GT_ZSDT0055 TYPE TABLE OF ZSDT0055. "Tabela de Solicitação Ordem de Venda – Logistica

    DATA: GW_ZSDT0053 TYPE ZSDT0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
          GW_ZSDT0055 TYPE ZSDT0055. "Tabela de Solicitação Ordem de Venda – Logistica

    DATA: VAR_SOMA_55 TYPE ZSDT0055-CADENCIA_QTE,
          VAR_SOMA_53 TYPE ZSDT0053-ZMENG.
    DATA: SALDO_AUX TYPE ZSDT0055-CADENCIA_QTE.
    DATA: VAR_TIPO TYPE CHAR01.

    DATA: GT_ZSDT0055_SOMA TYPE TABLE OF ZSDT0055,
          GW_ZSDT0055_SOMA TYPE ZSDT0055.

    DATA: GT_ZSDT0053_SOMA TYPE TABLE OF ZSDT0053,
          GW_ZSDT0053_SOMA TYPE ZSDT0053.

    DATA: VAR_DIFERENCA TYPE ZSDT0055-CADENCIA_QTE.
    DATA: VAR_TABIX TYPE SY-TABIX.

    IF ( I_FIXACAO IS INITIAL ).
      SELECT * FROM ZSDT0055
        INTO TABLE GT_ZSDT0055_SOMA
      WHERE NRO_SOL_OV EQ I_NUMERO.
    ELSE.
      SELECT * FROM ZSDT0055
        INTO TABLE GT_ZSDT0055_SOMA
      WHERE NRO_SOL_OV EQ I_NUMERO
        AND FIXACAO    EQ I_FIXACAO.
    ENDIF.

    DELETE GT_ZSDT0055_SOMA WHERE STATUS EQ 'Y'.
    DELETE GT_ZSDT0055_SOMA WHERE STATUS EQ 'W'.
    DELETE GT_ZSDT0055_SOMA WHERE STATUS EQ 'C'.

    LOOP AT GT_ZSDT0055_SOMA INTO GW_ZSDT0055_SOMA.
      VAR_SOMA_55 = VAR_SOMA_55 + GW_ZSDT0055_SOMA-CADENCIA_QTE.
    ENDLOOP.

    IF ( I_FIXACAO IS INITIAL ).
      SELECT * FROM ZSDT0053
        INTO TABLE GT_ZSDT0053_SOMA
      WHERE NRO_SOL_OV EQ I_NUMERO.
    ELSE.
      SELECT * FROM ZSDT0053
        INTO TABLE GT_ZSDT0053_SOMA
      WHERE NRO_SOL_OV EQ I_NUMERO
        AND FIXACAO    EQ I_FIXACAO.
    ENDIF.

    DELETE GT_ZSDT0053_SOMA WHERE STATUS EQ 'Y'.
    DELETE GT_ZSDT0053_SOMA WHERE STATUS EQ 'W'.
    DELETE GT_ZSDT0053_SOMA WHERE STATUS EQ 'C'.

    LOOP AT  GT_ZSDT0053_SOMA INTO GW_ZSDT0053_SOMA.
      VAR_SOMA_53 = VAR_SOMA_53 + GW_ZSDT0053_SOMA-ZMENG.
    ENDLOOP.

    IF NOT ( VAR_SOMA_55 IS INITIAL ) AND NOT ( VAR_SOMA_53 IS INITIAL ).

      VAR_DIFERENCA = VAR_SOMA_55 - VAR_SOMA_53.

      IF ( I_FIXACAO IS INITIAL ).
        SELECT * FROM ZSDT0055
          INTO TABLE GT_ZSDT0055
        WHERE NRO_SOL_OV EQ I_NUMERO.
      ELSE.
        SELECT * FROM ZSDT0055
          INTO TABLE GT_ZSDT0055
        WHERE NRO_SOL_OV EQ I_NUMERO
          AND FIXACAO    EQ I_FIXACAO.
      ENDIF.


      DELETE GT_ZSDT0055 WHERE STATUS EQ 'Y'.
      DELETE GT_ZSDT0055 WHERE STATUS EQ 'W'.
      DELETE GT_ZSDT0055 WHERE STATUS EQ 'C'.

      SORT: GT_ZSDT0055 BY DATA_PROGR DESCENDING.

      IF NOT ( GT_ZSDT0055[] IS INITIAL ).

        READ TABLE GT_ZSDT0055 INTO GW_ZSDT0055 INDEX 1.
        VAR_TABIX = SY-TABIX.
        SALDO_AUX = GW_ZSDT0055-CADENCIA_QTE - VAR_DIFERENCA.

* dia 13.12.2015 foi incluida o >= (maior e igual) para o encerramento para valores Zerados no momento do calculo
*        IF SALDO_AUX > 0. e removida a forma anterios

        IF SALDO_AUX >= 0.
          GW_ZSDT0055-CADENCIA_QTE = SALDO_AUX.
          MODIFY GT_ZSDT0055 FROM GW_ZSDT0055 INDEX VAR_TABIX TRANSPORTING CADENCIA_QTE.
        ELSE.
          WHILE SALDO_AUX < 0.
            GW_ZSDT0055-CADENCIA_QTE = 0.
            MODIFY GT_ZSDT0055 FROM GW_ZSDT0055 INDEX VAR_TABIX TRANSPORTING CADENCIA_QTE.
            VAR_TABIX = VAR_TABIX + 1.

            READ TABLE GT_ZSDT0055 INTO GW_ZSDT0055 INDEX VAR_TABIX.
            SALDO_AUX = SALDO_AUX * -1.
            SALDO_AUX = GW_ZSDT0055-CADENCIA_QTE - SALDO_AUX.

            IF ( SALDO_AUX > 0 ).
              GW_ZSDT0055-CADENCIA_QTE = SALDO_AUX.
              MODIFY GT_ZSDT0055 FROM GW_ZSDT0055 INDEX VAR_TABIX TRANSPORTING CADENCIA_QTE.
            ELSE.
              GW_ZSDT0055-CADENCIA_QTE = 0.
              MODIFY GT_ZSDT0055 FROM GW_ZSDT0055 INDEX VAR_TABIX TRANSPORTING CADENCIA_QTE.
            ENDIF.
          ENDWHILE.
        ENDIF.

        LOOP AT GT_ZSDT0055 INTO GW_ZSDT0055.
          UPDATE ZSDT0055 SET CADENCIA_QTE = GW_ZSDT0055-CADENCIA_QTE
                          WHERE NRO_SOL_OV EQ GW_ZSDT0055-NRO_SOL_OV
                            AND DATA_PROGR EQ GW_ZSDT0055-DATA_PROGR
                            AND ID         EQ GW_ZSDT0055-ID
                            AND STATUS EQ ''.

          COMMIT WORK.
        ENDLOOP.
      ENDIF.
    ELSEIF NOT ( VAR_SOMA_55 IS INITIAL ) AND ( VAR_SOMA_53 IS INITIAL ).

*    VAR_DIFERENCA = VAR_SOMA_55 - VAR_SOMA_53.

      IF ( I_FIXACAO IS INITIAL ).
        SELECT * FROM ZSDT0055
          INTO TABLE GT_ZSDT0055
        WHERE NRO_SOL_OV EQ I_NUMERO.
      ELSE.
        SELECT * FROM ZSDT0055
          INTO TABLE GT_ZSDT0055
        WHERE NRO_SOL_OV EQ I_NUMERO
          AND FIXACAO    EQ I_FIXACAO.
      ENDIF.

      DELETE GT_ZSDT0055 WHERE STATUS EQ 'Y'.
      DELETE GT_ZSDT0055 WHERE STATUS EQ 'W'.
      DELETE GT_ZSDT0055 WHERE STATUS EQ 'C'.

      SORT: GT_ZSDT0055 BY DATA_PROGR DESCENDING.
      IF NOT ( GT_ZSDT0055[] IS INITIAL ).

        READ TABLE GT_ZSDT0055 INTO GW_ZSDT0055 INDEX 1.
        VAR_TABIX = SY-TABIX.

        WHILE VAR_TABIX <= LINES( GT_ZSDT0055[] ).
          GW_ZSDT0055-CADENCIA_QTE = 0.
          MODIFY GT_ZSDT0055 FROM GW_ZSDT0055 INDEX VAR_TABIX TRANSPORTING CADENCIA_QTE.
          VAR_TABIX = VAR_TABIX + 1.
        ENDWHILE.
      ENDIF.

      LOOP AT GT_ZSDT0055 INTO GW_ZSDT0055.
        UPDATE ZSDT0055 SET CADENCIA_QTE = GW_ZSDT0055-CADENCIA_QTE
                        WHERE NRO_SOL_OV EQ GW_ZSDT0055-NRO_SOL_OV
                          AND DATA_PROGR EQ GW_ZSDT0055-DATA_PROGR
                          AND ID         EQ GW_ZSDT0055-ID
                          AND STATUS EQ ''.

        COMMIT WORK.
      ENDLOOP.


    ENDIF.

  ENDMETHOD.


  METHOD calc_cadencia_94.

    CLEAR cv_cadencia.

    LOOP AT it_0094 ASSIGNING FIELD-SYMBOL(<fs_zsdt0094>).
      cv_cadencia = cv_cadencia + <fs_zsdt0094>-cadencia_qte.
    ENDLOOP.

    CALL METHOD get_vlr_bio
      EXPORTING
        iv_matnr    = iv_matnr
        iv_doc_simu = iv_doc_simu
        iv_acao     = 'R'  "reverte para o valor cheio, a quantidade que está com o conversor aplicado
      CHANGING
        cv_cadencia = cv_cadencia.


  ENDMETHOD.


  METHOD CALC_DATA_VENC.

    DATA: QTD_DIAS TYPE I.

    CALL FUNCTION 'HR_AUPBS_MONTH_DAY'
      EXPORTING
        BEG_DA     = I_DATA_INICIAL
        END_DA     = I_DATA_FINAL
      IMPORTING
        NO_CAL_DAY = QTD_DIAS.

    E_DATA = ( I_DATA_INICIAL + ( QTD_DIAS / 2 ) ).

  ENDMETHOD.


  METHOD DIA_UTIL.

    CHECK NOT P_VENCIMENTO IS INITIAL.

    DATA: VG_LAST_DAY    TYPE SY-DATUM,
          VG_FIRST_DAY   TYPE SY-DATUM,
          IT_SAB_DOM_FER TYPE TABLE OF ISCAL_DAY.

    CONCATENATE P_VENCIMENTO(6) '01' INTO VG_FIRST_DAY.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        I_DATE = VG_FIRST_DAY
      IMPORTING
        E_DATE = VG_LAST_DAY.

    FREE: IT_SAB_DOM_FER.

    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        FACTORY_CALENDAR           = 'ZF'
        DATE_FROM                  = P_VENCIMENTO
        DATE_TO                    = VG_LAST_DAY
      TABLES
        HOLIDAYS                   = IT_SAB_DOM_FER
      EXCEPTIONS
        FACTORY_CALENDAR_NOT_FOUND = 1
        HOLIDAY_CALENDAR_NOT_FOUND = 2
        DATE_HAS_INVALID_FORMAT    = 3
        DATE_INCONSISTENCY         = 4
        OTHERS                     = 5.

    READ TABLE IT_SAB_DOM_FER TRANSPORTING NO FIELDS WITH KEY DATE = P_VENCIMENTO.
    E_SUBRC = SY-SUBRC.

  ENDMETHOD.


  METHOD diferenca.

    DATA: var_diferenca TYPE zsdt0094-total_proporc.
    DATA: var_cotacao TYPE kurrf. "Valor da Cotação.
    DATA: gt_zsdt0094 TYPE TABLE OF zsdt0094,
          gw_zsdt0094 TYPE zsdt0094.
    DATA: gt_zsdt0059 TYPE TABLE OF zsdt0059,
          gw_zsdt0059 TYPE zsdt0059.
    DATA: var_taxa_cambio TYPE ukursp.

    DATA: var_tipo TYPE char01.

    DATA: gobj_zcl_taxa_curva          TYPE REF TO zcl_taxa_curva, "Objeto da taxa curva.
          gobj_zcl_webservice_tx_curva TYPE REF TO zcl_webservice_tx_curva.

    var_diferenca = i_diferenca.

    IF ( var_diferenca NE 0 ).

      IF i_numero IS NOT INITIAL.
        DATA(r_numemro) = VALUE zrsdsselopts( ( sign = 'I' option = 'EQ' low = i_numero ) ).
      ENDIF.

      IF i_fixacao IS NOT INITIAL.
        DATA(r_fixacao) = VALUE zrsdsselopts( ( sign = 'I' option = 'EQ' low = i_fixacao ) ).
      ENDIF.

      IF i_tipo IS NOT INITIAL.
        DATA(r_tipo) = VALUE zrsdsselopts( ( sign = 'I' option = 'EQ' low = i_tipo ) ).
      ENDIF.

      SELECT * FROM zsdt0094
        INTO TABLE gt_zsdt0094
      WHERE nro_sol_ov IN r_numemro
        AND fixacao    IN r_fixacao
        AND tipo       IN r_tipo.

      CHECK sy-subrc IS INITIAL.

      IF i_tipo EQ 'VDA' OR
         i_tipo EQ 'FRE'.
        SELECT * FROM zsdt0059
          INTO TABLE gt_zsdt0059
        WHERE nro_sol_ov EQ i_numero
          AND posnr      EQ i_fixacao
          AND field      EQ 'PRECO'
          AND bezei      IN ( 'TAXA CAMBIO FRAME', 'TAXA CAMBIO' ).
      ENDIF.

      SORT: gt_zsdt0094 BY data_venc DESCENDING.

      READ TABLE gt_zsdt0094 INTO gw_zsdt0094 INDEX 1.

      FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
      CREATE OBJECT gobj_zcl_taxa_curva.
      CREATE OBJECT gobj_zcl_webservice_tx_curva.

      gobj_zcl_taxa_curva->set_numero( gw_zsdt0094-nro_sol_ov ).

      IF ( gw_zsdt0094-data_venc < sy-datum ).
        gw_zsdt0094-data_venc = sy-datum + 30.
      ENDIF.

      gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0094-data_venc ).
      gobj_zcl_taxa_curva->set_data_lib( sy-datum ).
      gobj_zcl_taxa_curva->set_cadencia( i_cadencia = 0 ).
      gobj_zcl_taxa_curva->set_zieme( gw_zsdt0094-zieme ).
      gobj_zcl_taxa_curva->set_total_proporcional( i_total = var_diferenca  ).
      gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0094-frete_cif ).
      gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0094-frete_porto ).
      gobj_zcl_taxa_curva->set_tipo( gw_zsdt0094-tipo ).
      gobj_zcl_taxa_curva->set_posnr( gw_zsdt0094-fixacao ).
      gobj_zcl_taxa_curva->set_data_registro( sy-datum ).
      gobj_zcl_taxa_curva->set_hora_registro( sy-uzeit ).
      gobj_zcl_taxa_curva->set_bezei( |DIFE_{ i_tipo }_MI| ).
      gobj_zcl_taxa_curva->set_safra( gw_zsdt0094-safra ).

      IF i_tipo EQ 'VDA'.
        READ TABLE gt_zsdt0059 INTO gw_zsdt0059 INDEX 1.
        var_taxa_cambio = gw_zsdt0059-formula2.
      ELSE.
        var_taxa_cambio = gw_zsdt0094-taxa_cambio.
      ENDIF.

      gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
      gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
      var_tipo = gobj_zcl_taxa_curva->get_tipo_taxa( ).
      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = gw_zsdt0094-data_venc
                                                               i_data_lib = sy-datum
                                                               i_tipo     = var_tipo
                                                               ).
      gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
      me->zif_taxa_curva_db~inserir( obj_taxa = gobj_zcl_taxa_curva ).

      CLEAR: gw_zsdt0094, var_cotacao.
    ENDIF.

  ENDMETHOD.


  METHOD diferenca_in.

    DATA: var_diferenca   TYPE zsdt0094-total_proporc,
          var_cotacao     TYPE kurrf,
          gt_zsdt0094     TYPE TABLE OF zsdt0094,
          gw_zsdt0094     TYPE zsdt0094,
          var_taxa_cambio TYPE ukursp,
          var_tipo        TYPE char01.

    DATA: obj_tx_cuv   TYPE REF TO zcl_taxa_curva,
          obj_w_tx_cuv TYPE REF TO zcl_webservice_tx_curva.

    var_diferenca = i_diferenca.

    IF ( var_diferenca NE 0 ).

      IF i_numero IS NOT INITIAL.
        DATA(r_numemro) = VALUE zrsdsselopts( ( sign = 'I' option = 'EQ' low = i_numero ) ).
      ENDIF.

      IF i_fixacao IS NOT INITIAL.
        DATA(r_fixacao) = VALUE zrsdsselopts( ( sign = 'I' option = 'EQ' low = i_fixacao ) ).
      ENDIF.

      IF i_bezei IS NOT INITIAL.
        DATA(r_bezei) = VALUE zrsdsselopts( ( sign = 'I' option = 'EQ' low = i_bezei ) ).
      ENDIF.

      IF i_tipo IS NOT INITIAL.
        DATA(r_tipo) = VALUE zrsdsselopts( ( sign = 'I' option = 'EQ' low = i_tipo ) ).
      ENDIF.

      SELECT * FROM zsdt0094
        INTO TABLE gt_zsdt0094
      WHERE nro_sol_ov IN r_numemro
        AND fixacao    IN r_fixacao
        AND bezei      IN r_bezei
        AND tipo       IN r_tipo.

      CHECK sy-subrc IS INITIAL.

      SORT: gt_zsdt0094 BY data_venc DESCENDING.

      READ TABLE gt_zsdt0094 INTO gw_zsdt0094 INDEX 1.

      FREE: obj_tx_cuv, obj_w_tx_cuv.
      CREATE OBJECT obj_tx_cuv.
      CREATE OBJECT obj_w_tx_cuv.

      obj_tx_cuv->set_numero( gw_zsdt0094-nro_sol_ov ).

      IF ( gw_zsdt0094-data_venc < sy-datum ).
        gw_zsdt0094-data_venc = sy-datum + 30.
      ENDIF.

      obj_tx_cuv->set_data_venc( gw_zsdt0094-data_venc ).
      obj_tx_cuv->set_data_lib( sy-datum ).
      obj_tx_cuv->set_cadencia( i_cadencia = 0 ).
      obj_tx_cuv->set_zieme( gw_zsdt0094-zieme ).
      obj_tx_cuv->set_total_proporcional( i_total = var_diferenca  ).
      obj_tx_cuv->set_frete_cif( gw_zsdt0094-frete_cif ).
      obj_tx_cuv->set_frete_porto( gw_zsdt0094-frete_porto ).
      obj_tx_cuv->set_tipo( gw_zsdt0094-tipo ).
      obj_tx_cuv->set_posnr( gw_zsdt0094-fixacao ).
      obj_tx_cuv->set_data_registro( sy-datum ).
      obj_tx_cuv->set_hora_registro( sy-uzeit ).
      obj_tx_cuv->set_bezei( |{ gw_zsdt0094-bezei }D| ).
      obj_tx_cuv->set_safra( gw_zsdt0094-safra ).
      obj_tx_cuv->set_taxa_cambio( CONV #( gw_zsdt0094-taxa_cambio ) ).

****       11.04.2024 - 96174 - RAMON --> comentado para subir para prd, nao excluir!!
***      IF i_tipo = 'VDI' AND i_waerk = 'BRL'.
***        obj_tx_cuv->set_taxa_cambio_boleta( gw_zsdt0094-nro_sol_ov ).
***      ENDIF.
****       11.04.2024 - 96174 - RAMON --<

      obj_tx_cuv->tipo_taxa_in( 'DIF' ).

      var_tipo = obj_tx_cuv->get_tipo_taxa( ).

      CALL METHOD obj_w_tx_cuv->buscar_taxa
        EXPORTING
          i_data     = gw_zsdt0094-data_venc  " Data
          i_data_lib = sy-datum               " Data Liberação
          i_tipo     = var_tipo               " Campo de texto do comprimento 1
        RECEIVING
          e_cotacao  = var_cotacao.           " Taxa de câmbio para lançamentos FI

      obj_tx_cuv->set_taxa_curva( var_cotacao ).
      me->zif_taxa_curva_db~inserir( obj_taxa = obj_tx_cuv ).

      CLEAR: gw_zsdt0094, var_cotacao.
    ENDIF.

  ENDMETHOD.


  METHOD edicao.

    CONSTANTS: var_x TYPE c VALUE 'X'.

    DATA: gt_zsdt0094 TYPE TABLE OF zsdt0094,
          gw_zsdt0094 TYPE zsdt0094.

    TYPES: BEGIN OF ty_saldo,
             nro_sol_ov TYPE zsdt0055-nro_sol_ov,
             data       TYPE zsdt0055-data_progr,
             valor      TYPE zsdt0055-cadencia_qte,
           END OF ty_saldo.

    DATA: gt_saldo TYPE TABLE OF ty_saldo,
          gw_saldo TYPE ty_saldo.

    DATA: gt_zsdt0053 TYPE TABLE OF zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
          gt_zsdt0055 TYPE TABLE OF zsdt0055. "Tabela de Solicitação Ordem de Venda – Logistica

    DATA: gw_zsdt0053 TYPE zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
          gw_zsdt0055 TYPE zsdt0055. "Tabela de Solicitação Ordem de Venda – Logistica

    DATA: var_soma_55 TYPE zsdt0055-cadencia_qte,
          var_soma_53 TYPE zsdt0053-zmeng.
    DATA: saldo_aux TYPE zsdt0055-cadencia_qte.
    DATA: var_tipo TYPE char01.

    DATA: gt_zsdt0055_soma TYPE TABLE OF zsdt0055,
          gw_zsdt0055_soma TYPE zsdt0055.

    DATA: gt_zsdt0053_soma TYPE TABLE OF zsdt0053,
          gw_zsdt0053_soma TYPE zsdt0053.

    DATA: var_diferenca TYPE zsdt0055-cadencia_qte.

    DATA: var_tabix TYPE sy-tabix.

    DATA: var_estorno TYPE num10.

    DATA:
      gobj_zcl_taxa_curva          TYPE REF TO zcl_taxa_curva, "Objeto da taxa curva.
      gobj_zcl_webservice_tx_curva TYPE REF TO zcl_webservice_tx_curva.

    DATA: var_cotacao TYPE kurrf. "Valor da Cotação.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZEST_0094'
      IMPORTING
        number                  = var_estorno "Numeração para identificar o estorno.
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF ( i_fixacao IS INITIAL ).

      IF NOT ( i_vbeln IS INITIAL ).

        "Solicitação/Simulador de Ordem de Venda - HEDGE
        SELECT * FROM zsdt0094
          INTO TABLE gt_zsdt0094
        WHERE nro_sol_ov EQ i_numero
          AND programa   EQ 'SAPMV60A'.

      ELSE.

        "Solicitação/Simulador de Ordem de Venda - HEDGE
        SELECT * FROM zsdt0094
          INTO TABLE gt_zsdt0094
        WHERE nro_sol_ov EQ i_numero
          AND estorno    EQ 0
          AND tipo IN ('VDA', 'FRE').

        IF ( sy-subrc EQ 0 ).

          DELETE gt_zsdt0094 WHERE programa EQ 'SAPMV60A'.

*       Atualiza quantidade na Aba Logistica ZSDT0055
          me->calcula_frete(  i_numero   = i_numero
                              i_fixacao  = i_fixacao
                              i_ucomm    = i_ucomm
                              i_vbeln    = i_vbeln
                              i_tcode    = i_tcode ).
        ENDIF.
      ENDIF.

    ELSE.

      "Solicitação/Simulador de Ordem de Venda - HEDGE
      SELECT * FROM zsdt0094
        INTO TABLE gt_zsdt0094
      WHERE nro_sol_ov EQ i_numero
        AND fixacao    EQ i_fixacao
        AND estorno    EQ 0
        AND tipo       EQ 'FRE'.

      IF ( i_tcode NE 'VF01' ).
*     Atualiza a tabela zsdt0055 com o valor do encerramento.
        me->calcula_frete(  i_numero   = i_numero
                            i_fixacao  = i_fixacao
                            i_ucomm    = i_ucomm
                            i_vbeln    = i_vbeln
                            i_tcode    = i_tcode ).

      ENDIF.
    ENDIF.

    IF ( sy-subrc EQ 0 ) AND NOT ( var_estorno IS INITIAL ).

      IF i_tcode EQ 'VF11'.
        DELETE gt_zsdt0094 WHERE vbeln NE i_vbeln.
        DELETE gt_zsdt0094 WHERE estorno NE '0'.
      ELSE.
        DELETE gt_zsdt0094 WHERE programa EQ 'SAPMV60A' AND bezei EQ 'Y'.
        DELETE gt_zsdt0094 WHERE programa EQ 'SAPMV60A' AND bezei EQ 'W'.
      ENDIF.

      LOOP AT gt_zsdt0094 INTO gw_zsdt0094.

        FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
        CREATE OBJECT gobj_zcl_taxa_curva.
        CREATE OBJECT gobj_zcl_webservice_tx_curva.

        gobj_zcl_taxa_curva->set_numero( gw_zsdt0094-nro_sol_ov ).
        gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0094-inco1 ).
        gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0094-data_venc ).
        gobj_zcl_taxa_curva->set_data_lib( sy-datum ).
        gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0094-cadencia_qte
                                           i_negativa = var_x ).

        gobj_zcl_taxa_curva->set_zieme( gw_zsdt0094-zieme ).
        gobj_zcl_taxa_curva->set_total_proporcional( i_total    = gw_zsdt0094-total_proporc
                                                     i_negativa = var_x ).

        gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0094-frete_cif ).
        gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0094-frete_porto ).
        gobj_zcl_taxa_curva->set_bezei( gw_zsdt0094-bezei ).
        gobj_zcl_taxa_curva->set_tipo( gw_zsdt0094-tipo ).
        gobj_zcl_taxa_curva->set_estorno( var_estorno ).
        gobj_zcl_taxa_curva->set_posnr( gw_zsdt0094-fixacao ).
        gobj_zcl_taxa_curva->set_data_registro( gw_zsdt0094-data_registro ).
        gobj_zcl_taxa_curva->set_hora_registro( gw_zsdt0094-hora_registro ).
        gobj_zcl_taxa_curva->set_taxa_cambio( gw_zsdt0094-taxa_cambio ).
        gobj_zcl_taxa_curva->set_safra( gw_zsdt0094-safra ).
        gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode
                                                 i_tipo = 'EDI'
                                                 ).
        var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).

        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = gw_zsdt0094-data_venc
                                                                 i_data_lib = sy-datum
                                                                 i_tipo     = var_tipo
                                                                 ).
        gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
        me->zif_taxa_curva_db~atualizar( gobj_zcl_taxa_curva ).
        me->zif_taxa_curva_db~inserir( obj_taxa = gobj_zcl_taxa_curva ).

        CLEAR: gw_zsdt0094, var_cotacao.
      ENDLOOP.

      CASE i_ucomm.
        WHEN: 'LIBERAR'.
          me->liberar_ov( i_numero = i_numero
                          i_tcode  = i_tcode
                        ).
      ENDCASE.
    ENDIF.

*    ME->UPDATE_JOB(
*                    TCODE = I_TCODE
*                    NRO_SOL_OV = I_NUMERO
*                    VL_VBELN = I_VBELN
*                  ).

    CLEAR: var_soma_55, var_soma_53.
  ENDMETHOD.


  METHOD encerramento.


    CONSTANTS:
      c_frete_cif   TYPE c LENGTH 11 VALUE 'FRETE CIF',
      c_frete_porto TYPE c LENGTH 13 VALUE 'FRETE PORTO',
      c_fobs        TYPE c LENGTH 4  VALUE 'FOBS',
      c_venda       TYPE c LENGTH 3  VALUE 'VDA',
      c_frete       TYPE c LENGTH 3  VALUE 'FRE',
      c_taxa_cambio TYPE c LENGTH 11 VALUE 'TAXA CAMBIO',
      c_conv_bio    TYPE c LENGTH 13 VALUE 'CONVERSOR BIO'.

    DATA:
      gt_zsdt0051 TYPE TABLE OF zsdt0051, "Tabela de Solicitação Ordem de Venda - Cabeçalho
      gt_zsdt0052 TYPE TABLE OF zsdt0052, "Tabela de Solicitação Ordem de Venda – COND_PGTO
      gt_zsdt0053 TYPE TABLE OF zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
      gt_zsdt0054 TYPE TABLE OF zsdt0054, "Tabela de Solicitação Ordem de Venda – Pagamento Antecipado
      gt_zsdt0055 TYPE TABLE OF zsdt0055, "Tabela de Solicitação Ordem de Venda – Logistica
      gt_zsdt0059 TYPE TABLE OF zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gt_zsdt0069 TYPE TABLE OF zsdt0069, "Solicitação de Ordem de Venda – Histórico Aprovações
      gt_zsdt0162 TYPE TABLE OF zsdt0162, "Solicitação de Ordem de Venda – Histórico Aprovações
      gt_zsdt0073 TYPE TABLE OF zsdt0073, "Tabela condições de pagamento
      gt_t052     TYPE TABLE OF t052.     "Condições de pagamento

    DATA:
      gw_zsdt0051             TYPE zsdt0051, "Tabela de Solicitação Ordem de Venda - Cabeçalho
      gw_zsdt0052             TYPE zsdt0052, "Tabela de Solicitação Ordem de Venda – COND_PGTO
      gw_zsdt0053             TYPE zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
      gw_zsdt0054             TYPE zsdt0054, "Tabela de Solicitação Ordem de Venda – Pagamento Antecipado
      gw_zsdt0055             TYPE zsdt0055, "Tabela de Solicitação Ordem de Venda – Logistica
      gw_zsdt0069             TYPE zsdt0069, "Solicitação de Ordem de Venda – Histórico Aprovações
      gw_zsdt0059_porto       TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0059_cif         TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0059_fobs        TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0059_taxa_cambio TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0073             TYPE zsdt0073, "Tabela condições de pagamento
      gw_t052                 TYPE t052.     "Condições de pagamento

    DATA:
      gobj_zcl_taxa_curva          TYPE REF TO zcl_taxa_curva, "Objeto da taxa curva.
      gobj_zcl_webservice_tx_curva TYPE REF TO zcl_webservice_tx_curva.

    DATA:
      var_cotacao    TYPE kurrf, "Valor da Cotação.
*        VAR_MONTANTE    TYPE ZSDT0053-ZMENG, "Valor do Montante.
**** Incluido 13 5 para corrigir valores de aredondamento
      var_montante   TYPE p LENGTH 13 DECIMALS 5, "Valor do Montante.
      var_montante_2 TYPE zsdt0053-dmbtr. "Valor do Montante.


    DATA: var_data          TYPE datum,
          var_data_completa TYPE datum,
          var_mes           TYPE i,
          var_mes_aux       TYPE c LENGTH 2,
          var_ano           TYPE c LENGTH 4,
          var_taxa_cambio   TYPE ukursp. "Taxa do Cambio.

    DATA: var_data_calculada TYPE d,
          var_msg            TYPE string. "Variavel para mostrar a Mensagem texto da exception.

    DATA: var_tabix TYPE sy-tabix.
    DATA: cx_exception TYPE REF TO zcx_webservice. "Referencia para a Classe de Exception.

    DATA: var_tipo TYPE char01.
    DATA: data_venc TYPE datum.

    DATA: var_safra TYPE ajahr.

    "Tabela de Solicitação Ordem de Venda - Cabeçalho
    SELECT * FROM zsdt0051 INTO TABLE gt_zsdt0051 WHERE nro_sol_ov EQ i_numero.

    IF ( sy-subrc EQ 0 ).

      "Tabela de Solicitação Ordem de Venda – COND_PGTO
      SELECT * FROM zsdt0052
        INTO TABLE gt_zsdt0052
        FOR ALL ENTRIES IN gt_zsdt0051
       WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.

      "Tabela de Solicitação de ordem de venda - MATERIAIS
      CASE i_tcode.
        WHEN: 'ZSDT0062'.
          SELECT * FROM zsdt0053
            INTO TABLE gt_zsdt0053
            FOR ALL ENTRIES IN gt_zsdt0051
          WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.
          DELETE gt_zsdt0053 WHERE status EQ 'Y'.
          DELETE gt_zsdt0053 WHERE status EQ 'W'.
          DELETE gt_zsdt0053 WHERE status EQ 'C'.

        WHEN: 'VF01'. "VF01
          SELECT * FROM zsdt0053
            INTO TABLE gt_zsdt0053
            FOR ALL ENTRIES IN gt_zsdt0051
          WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
            AND vbeln EQ i_vbeln
            AND status EQ 'D'.

        WHEN: 'VF11'. "VF11
          SELECT * FROM zsdt0053
            INTO TABLE gt_zsdt0053
            FOR ALL ENTRIES IN gt_zsdt0051
          WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
            AND status EQ 'C'.
      ENDCASE.

      "Tabela de Solicitação Ordem de Venda – PRECO
      SELECT * FROM zsdt0059
        INTO TABLE gt_zsdt0059
        FOR ALL ENTRIES IN gt_zsdt0051
      WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
        AND bezei IN (c_frete_cif,c_frete_porto,c_fobs,c_taxa_cambio, c_conv_bio).

      "Condições de pagamento
      SELECT * FROM t052
        INTO TABLE gt_t052
        FOR ALL ENTRIES IN gt_zsdt0052
      WHERE zterm EQ gt_zsdt0052-zterm.

    ENDIF.

    "Tabela de Solicitação Ordem de Venda – Pagamento Antecipado
    SELECT * FROM zsdt0054
      INTO TABLE gt_zsdt0054
      FOR ALL ENTRIES IN gt_zsdt0051
    WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.

    CASE i_tcode.
      WHEN: 'ZSDT0062'.
        "Tabela de Solicitação Ordem de Venda – Logistica
        SELECT * FROM zsdt0055
          INTO TABLE gt_zsdt0055
          FOR ALL ENTRIES IN gt_zsdt0051
        WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.

        DELETE gt_zsdt0055 WHERE status EQ 'Y'.
        DELETE gt_zsdt0055 WHERE status EQ 'W'.
        DELETE gt_zsdt0055 WHERE status EQ 'C'.
        DELETE gt_zsdt0055 WHERE cadencia_qte EQ 0.

      WHEN: 'VF01'.

        "Tabela de Solicitação Ordem de Venda – Logistica
        SELECT * FROM zsdt0055
          INTO TABLE gt_zsdt0055
          FOR ALL ENTRIES IN gt_zsdt0051
        WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
          AND status EQ 'D'.

      WHEN: 'VF11'.

        "Tabela de Solicitação Ordem de Venda – Logistica
        SELECT * FROM zsdt0055
          INTO TABLE gt_zsdt0055
          FOR ALL ENTRIES IN gt_zsdt0051
        WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
          AND status     EQ 'C'.

    ENDCASE.

    "Solicitação de Ordem de Venda – Histórico Aprovações WorkFlow
    SELECT * FROM zsdt0162
      INTO TABLE gt_zsdt0162
      FOR ALL ENTRIES IN gt_zsdt0051
    WHERE vbeln EQ gt_zsdt0051-nro_sol_ov
      AND status EQ 'L'
      AND ck_recusa NE 'S'
      AND ultimo_nivel EQ 'X'.

    IF gt_zsdt0162 IS INITIAL.

*    "Solicitação de Ordem de Venda – Histórico Aprovações ZSDT0062
      SELECT * FROM zsdt0069
        INTO TABLE gt_zsdt0069
        FOR ALL ENTRIES IN gt_zsdt0051
      WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.

    ENDIF.

    LOOP AT gt_zsdt0162 INTO DATA(wa_zsdt0162).

      gw_zsdt0069-id_historico  = wa_zsdt0162-id_log.
      gw_zsdt0069-nro_sol_ov    = wa_zsdt0162-vbeln.
      gw_zsdt0069-status        = wa_zsdt0162-status.
      gw_zsdt0069-motivo        = wa_zsdt0162-motivo.
      gw_zsdt0069-usnam         = wa_zsdt0162-usuario.
      gw_zsdt0069-data_atual    = wa_zsdt0162-data_atual.
      gw_zsdt0069-hora_atual    = wa_zsdt0162-hora_atual.

      APPEND gw_zsdt0069 TO gt_zsdt0069.

    ENDLOOP.

    SORT: gt_zsdt0069 BY id_historico DESCENDING. "Ordenação.


    CASE i_tcode.
      WHEN: 'VF01'.
        "metodo para mudar o status quando é feito uma devolução.
        me->status_devolucao( i_numero  = i_numero
                              i_estorno = CONV #( i_vbeln )
                              i_auart   = i_auart ).
    ENDCASE.

    "Loop para iniciar o Processo.
    LOOP AT gt_zsdt0051 INTO gw_zsdt0051.

      IF gw_zsdt0051-waerk NE 'BRL'.
        CONTINUE.
      ENDIF.

      me->free( ). "Limpar os Atributos Totais.

      "Executa os metodos para somar os valores da ZSDT0053, ZSDT0054, ZSDT0055.
      me->total_53( gw_zsdt0051-nro_sol_ov ).
      me->total_peso_53( gw_zsdt0051-nro_sol_ov ).
      me->total_54( gw_zsdt0051-nro_sol_ov ).
      me->total_55( gw_zsdt0051-nro_sol_ov ).

      "Faz o Calculo para pegar o indice.
      IF NOT ( me->at_total_53 IS INITIAL ) AND NOT ( me->at_total_54 IS INITIAL ).
        me->indice( i_total_53 = me->at_total_53
                    i_total_54 = me->at_total_54
                    ).
      ENDIF.

      READ TABLE gt_zsdt0053 INTO gw_zsdt0053       WITH KEY nro_sol_ov  = gw_zsdt0051-nro_sol_ov.
      READ TABLE gt_zsdt0052 INTO gw_zsdt0052       WITH KEY nro_sol_ov  = gw_zsdt0051-nro_sol_ov.
      READ TABLE gt_zsdt0059 INTO gw_zsdt0059_cif   WITH KEY nro_sol_ov  = gw_zsdt0053-nro_sol_ov
                                                             bezei       = 'FRETE CIF'.
      READ TABLE gt_zsdt0059 INTO gw_zsdt0059_porto WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                             bezei      = 'FRETE PORTO'.
      READ TABLE gt_zsdt0059 INTO gw_zsdt0059_fobs  WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                             bezei      = 'FOBS'.
      READ TABLE gt_zsdt0059 INTO gw_zsdt0059_taxa_cambio WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                                   bezei      = 'TAXA CAMBIO'
                                                                   field      = 'PRECO'.
      READ TABLE gt_t052     INTO gw_t052           WITH KEY zterm      = gw_zsdt0052-zterm.
      READ TABLE gt_zsdt0073 INTO gw_zsdt0073 WITH KEY nro_sol_ov = gw_zsdt0051-nro_sol_ov.
      READ TABLE gt_t052     INTO gw_t052     WITH KEY zterm      = gw_zsdt0073-zterm.
      READ TABLE gt_zsdt0069 INTO gw_zsdt0069 INDEX 1.

      " 29.08.2024 - 150168 - RAMON -->
      SELECT SINGLE matkl FROM mara
        INTO @DATA(lv_matkl)
          WHERE matnr = @gw_zsdt0051-matnr.

      " quando for bio
      IF lv_matkl = '700400'.

        READ TABLE gt_zsdt0059 INTO DATA(ls_bio)
         WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                  bezei      = c_conv_bio.

        IF sy-subrc EQ 0.

          LOOP AT gt_zsdt0053 ASSIGNING FIELD-SYMBOL(<fs_0053>).
            <fs_0053>-zmeng = <fs_0053>-zmeng * ls_bio-formula2.
          ENDLOOP.

          LOOP AT gt_zsdt0055 ASSIGNING FIELD-SYMBOL(<fs_0055>).
            <fs_0055>-cadencia_qte = <fs_0055>-cadencia_qte * ls_bio-formula2.
          ENDLOOP.

          gw_zsdt0055-cadencia_qte = gw_zsdt0055-cadencia_qte * ls_bio-formula2.
          gw_zsdt0053-zmeng = gw_zsdt0053-zmeng * ls_bio-formula2.

          me->at_total_55 = me->at_total_55 * ls_bio-formula2.
          me->at_total_peso_53 = me->at_total_peso_53 * ls_bio-formula2.

        ENDIF.

      ENDIF.
      " 29.08.2024 - 150168 - RAMON --<

      CASE gw_zsdt0051-risco_sacado.
        WHEN: 'S'. "Quando existe o risco sacado (S)

          LOOP AT gt_zsdt0055 INTO gw_zsdt0055 WHERE nro_sol_ov EQ gw_zsdt0053-nro_sol_ov.

            "**********************************
            "* FRETE - INICIO
            "**********************************
            FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
            CREATE OBJECT gobj_zcl_taxa_curva.
            CREATE OBJECT gobj_zcl_webservice_tx_curva.

            gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
            gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
            data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
            gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
            gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
            gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
            gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
            gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
            gobj_zcl_taxa_curva->set_tipo( c_frete ).
            gobj_zcl_taxa_curva->set_estorno( i_estorno ).
            gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

            IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
              var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
            ELSE.
              IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.
              ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                " 29.08.2024 - 150168 - RAMON -->
              ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.
              ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                " 29.08.2024 - 150168 - RAMON --<



              ENDIF.
            ENDIF.

            " Conversão casas decimais
            var_montante_2 = var_montante .
            gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
            gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                     i_status = i_status ).

            var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
            gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                               i_tcode    = i_tcode
                                               ).

            IF ( sy-cprog NE 'ZCARGA').
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                       i_data_lib = sy-datum
                                                                       i_tipo     = var_tipo ).
            ELSE.
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                       i_data_lib = gw_zsdt0069-data_atual
                                                                       i_tipo     = var_tipo ).
            ENDIF.

            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
            var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*          GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
            gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                  i_numero = gw_zsdt0051-nro_sol_ov
                                                  i_data   = gw_zsdt0055-data_progr
                                                  i_tipo   = c_frete ).

            gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

            var_safra = gw_zsdt0053-charg.
            gobj_zcl_taxa_curva->set_safra( var_safra ).
            me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)
            "**********************************
            "* FRETE - FIM
            "**********************************

            FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
            CREATE OBJECT gobj_zcl_taxa_curva.
            CREATE OBJECT gobj_zcl_webservice_tx_curva.

*&----------------Inicio ajuste Bug Solto #149379 / AOENNING&*
            IF i_auart EQ 'ZCPV'.
              LOOP AT i_vbap ASSIGNING FIELD-SYMBOL(<wa_vbap>) WHERE vbeln EQ i_vbeln.
                var_montante = var_montante + <wa_vbap>-netwr + <wa_vbap>-mwsbp.
              ENDLOOP.
            ELSE.
*&----------------Fim ajuste Bug Solto #149379 / AOENNING&*
              IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
                var_montante = gw_zsdt0053-dmbtr * gw_zsdt0055-cadencia_qte.
              ELSE.

                IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                  var_montante = gw_zsdt0053-dmbtr / 1000.
                  var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                  var_montante = gw_zsdt0053-dmbtr  * 1000.
                  gw_zsdt0053-dmbtr = var_montante * gw_zsdt0055-cadencia_qte.

                  " 29.08.2024 - 150168 - RAMON -->
                ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                  var_montante = gw_zsdt0053-dmbtr / 1000.
                  var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                  var_montante = gw_zsdt0053-dmbtr  * 1000.
                  gw_zsdt0053-dmbtr = var_montante * gw_zsdt0055-cadencia_qte.
                  " 29.08.2024 - 150168 - RAMON --<
                ENDIF.

              ENDIF.
            ENDIF.

            gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
            gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
            data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-valdt_hedge ).
            gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
            gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

            IF NOT ( gw_zsdt0055-cadencia_qte IS INITIAL ).
              gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                                 i_tcode    = i_tcode
                                                 ).
            ELSE.
              gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                 i_tcode    = i_tcode ).
            ENDIF.

            " Conversão casas decimais
            var_montante_2 = var_montante .
            gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
            gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
            gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
            gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
            gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).

            gobj_zcl_taxa_curva->set_tipo( c_venda ).
            gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*          GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
            gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                     i_status = i_status ).

            var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
            gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

            IF ( sy-cprog NE 'ZCARGA').
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                       i_data_lib = sy-datum
                                                                       i_tipo     = var_tipo
                                                                       ).
            ELSE.
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                       i_data_lib = gw_zsdt0069-data_atual
                                                                       i_tipo     = var_tipo
                                                                       ).
            ENDIF.

            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
            var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.

*          GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
            gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                  i_numero = gw_zsdt0051-nro_sol_ov
                                                  i_data   = gw_zsdt0055-valdt_hedge
                                                  i_tipo   = c_venda ).

            gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

            var_safra = gw_zsdt0053-charg.
            gobj_zcl_taxa_curva->set_safra( var_safra ).
            me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

            CLEAR: var_cotacao, gw_zsdt0055, var_montante.

          ENDLOOP.

        WHEN: 'N'. "Quando não existe risco sacado (N)

          "Caso a ZSDT0053-VALDT (DATA_VENCIMENTO) OU ZSDT0052-VALDT ((DATA_VENCIMENTO) esteja preenchidas
          IF NOT ( gw_zsdt0053-valdt IS INITIAL ) OR NOT ( gw_zsdt0052-valdt IS INITIAL ). "Regra Número 01.

            LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov EQ gw_zsdt0051-nro_sol_ov.

              FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
              CREATE OBJECT gobj_zcl_taxa_curva.
              CREATE OBJECT gobj_zcl_webservice_tx_curva.

              gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
              gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

              IF NOT ( gw_zsdt0053-valdt IS INITIAL ).
                data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0053-valdt ).
              ELSE.
                data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0052-valdt ).
              ENDIF.

              gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
              gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                 i_tcode    = i_tcode
                                                 ).
              gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
              gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
              gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
              gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
              gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
              gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
              gobj_zcl_taxa_curva->set_tipo( c_venda ).
              gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*            GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
              gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                       i_status = i_status ).

              var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
              gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

              IF NOT ( gw_zsdt0053-valdt IS INITIAL ).

                IF ( sy-cprog NE 'ZCARGA').

                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0053-VALDT
                                                                           i_data_lib = sy-datum
                                                                           i_tipo     = var_tipo
                                                                           ).
                ELSE.
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0053-VALDT
                                                                           i_data_lib = gw_zsdt0069-data_atual
                                                                           i_tipo     = var_tipo
                                                                           ).
                ENDIF.

              ELSE.

                IF ( sy-cprog NE 'ZCARGA').
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0052-VALDT
                                                                           i_data_lib = sy-datum
                                                                           i_tipo     = var_tipo
                                                                           ).
                ELSE.
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0052-VALDT
                                                                           i_data_lib = gw_zsdt0069-data_atual
                                                                           i_tipo     = var_tipo
                                                                           ).
                ENDIF.
              ENDIF.

              gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
              var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*            GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
              IF NOT ( gw_zsdt0053-valdt IS INITIAL ).

                gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                      i_numero = gw_zsdt0051-nro_sol_ov
                                                      i_data   = gw_zsdt0053-valdt
                                                      i_tipo   = c_venda ).
              ELSE.

                gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                      i_numero = gw_zsdt0051-nro_sol_ov
                                                      i_data   = gw_zsdt0052-valdt
                                                      i_tipo   = c_venda ).
              ENDIF.

              gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

              var_safra = gw_zsdt0053-charg.
              gobj_zcl_taxa_curva->set_safra( var_safra ).
              me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094.

            ENDLOOP.

            "**********************************
            "*  FRETE - INICIO
            "*********************************
            READ TABLE gt_zsdt0055 INTO gw_zsdt0055 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
            IF ( sy-subrc EQ 0 ).

              LOOP AT gt_zsdt0055 INTO gw_zsdt0055 WHERE nro_sol_ov EQ gw_zsdt0051-nro_sol_ov.

                FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                CREATE OBJECT gobj_zcl_taxa_curva.
                CREATE OBJECT gobj_zcl_webservice_tx_curva.

                gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
                gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                gobj_zcl_taxa_curva->set_posnr( i_fixacao ).


                IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                  var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0055-cadencia_qte ).
                ELSE.
                  IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                    var_montante = var_montante *  gw_zsdt0055-cadencia_qte.
                  ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                    var_montante = var_montante *  gw_zsdt0055-cadencia_qte.

                    " 29.08.2024 - 150168 - RAMON -->
                  ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                    var_montante = var_montante *  gw_zsdt0055-cadencia_qte.
                  ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                    var_montante = var_montante *  gw_zsdt0055-cadencia_qte.
                    " 29.08.2024 - 150168 - RAMON --<

                  ENDIF.
                ENDIF.

                " Conversão casas decimais
                var_montante_2 = var_montante .
                gobj_zcl_taxa_curva->set_tipo( c_frete ).
                gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*              GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
                gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                         i_status = i_status ).

                var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                                   i_tcode    = i_tcode
                                                   ).
                gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).

                IF ( sy-cprog NE 'ZCARGA').
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                           i_data_lib = sy-datum
                                                                           i_tipo     = var_tipo ).
                ELSE.
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                           i_data_lib = gw_zsdt0069-data_atual
                                                                           i_tipo     = var_tipo ).
                ENDIF.

                gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*              GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                      i_numero = gw_zsdt0051-nro_sol_ov
                                                      i_data   = gw_zsdt0055-data_progr
                                                      i_tipo   = c_frete ).

                gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

                var_safra = gw_zsdt0053-charg.
                gobj_zcl_taxa_curva->set_safra( var_safra ).
                me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094.

              ENDLOOP.

            ELSE. "Caso não encontre na ZSDT0055

              LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov EQ gw_zsdt0051-nro_sol_ov .

                FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                CREATE OBJECT gobj_zcl_taxa_curva.
                CREATE OBJECT gobj_zcl_webservice_tx_curva.


                var_data_calculada = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                         i_data_final   = gw_zsdt0051-dtate_logist ).

                gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                  var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                ELSE.
                  IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                    var_montante = var_montante * gw_zsdt0053-zmeng.
                  ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                    var_montante = var_montante * gw_zsdt0053-zmeng.

                    " 29.08.2024 - 150168 - RAMON -->
                  ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                    var_montante = var_montante * gw_zsdt0053-zmeng.
                  ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                    var_montante = var_montante * gw_zsdt0053-zmeng.
                    " 29.08.2024 - 150168 - RAMON --<

                  ENDIF.
                ENDIF.

                " Conversão casas decimais
                var_montante_2 = var_montante .
                gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                gobj_zcl_taxa_curva->set_tipo( c_frete ).
                gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*              GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
                gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                         i_status = i_status ).

                var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).


                gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                   i_tcode    = i_tcode ).


*              GOBJ_ZCL_TAXA_CURVA->SET_CADENCIA( GW_ZSDT0053-ZMENG ).

                IF ( sy-cprog NE 'ZCARGA').
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                           i_data_lib = sy-datum
                                                                           i_tipo     = var_tipo ).

                ELSE.
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                           i_data_lib = gw_zsdt0069-data_atual
                                                                           i_tipo     = var_tipo ).

                ENDIF.

                gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*              GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                      i_numero = gw_zsdt0051-nro_sol_ov
                                                      i_data   = gw_zsdt0051-dtde_logist
                                                      i_tipo   = c_frete ).

                gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

                var_safra = gw_zsdt0053-charg.
                gobj_zcl_taxa_curva->set_safra( var_safra ).
                me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094.
                "**********************************
                "* FRETE - FIM
                "**********************************

              ENDLOOP.
            ENDIF.
            "ENDIF. "Fim da condição caso não tenha INDICE.

          ELSE. "Caso as datas de vencimento da ZSDT0053 OU ZSDT0052 não esteja preenchidas.

            IF ( gt_zsdt0055[] IS INITIAL ). "Caso não tenha indice/ZSDT0055.

              CASE gw_t052-zdart.
                WHEN: 'D'.

                  "READ TABLE GT_ZSDT0053 INTO GW_ZSDT0053 WITH KEY NRO_SOL_OV = GW_ZSDT0051-NRO_SOL_OV.

                  LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov = gw_zsdt0051-nro_sol_ov.

                    FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                    CREATE OBJECT gobj_zcl_taxa_curva.
                    CREATE OBJECT gobj_zcl_webservice_tx_curva.


                    gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                    gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                    var_data_calculada = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                             i_data_final   = gw_zsdt0051-dtate_logist ).

                    data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                    gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                    gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                       i_tcode    = i_tcode
                                                       ).
                    gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                    gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
                    gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                    gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                    gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                    gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                    gobj_zcl_taxa_curva->set_tipo( c_venda ).
                    gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*                  GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
                    gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                             i_status = i_status ).

                    var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                    gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                    IF ( sy-cprog NE 'ZCARGA').
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = sy-datum
                                                                               i_tipo     = var_tipo
                                                                               ).

                    ELSE.
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = gw_zsdt0069-data_atual
                                                                               i_tipo     = var_tipo
                                                                               ).
                    ENDIF.

                    gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                    var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                  GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                    gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                          i_numero = gw_zsdt0051-nro_sol_ov
                                                          i_data   = gw_zsdt0051-dtde_logist
                                                          i_tipo   = c_venda ).

                    gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

                    var_safra = gw_zsdt0053-charg.
                    gobj_zcl_taxa_curva->set_safra( var_safra ).
                    me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                    "**********************************
                    "* FRETE - INICIO
                    "**********************************
                    FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                    CREATE OBJECT gobj_zcl_taxa_curva.
                    CREATE OBJECT gobj_zcl_webservice_tx_curva.

                    gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                    gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                    var_data_calculada = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                             i_data_final   = gw_zsdt0051-dtate_logist ).

                    data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                    gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).

                    gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                    gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                    IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                    ELSE.
                      IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.
                      ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.

                        " 29.08.2024 - 150168 - RAMON -->
                      ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.
                      ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.
                        " 29.08.2024 - 150168 - RAMON --<

                      ENDIF.
                    ENDIF.

                    " Conversão casas decimais
                    var_montante_2 = var_montante .
                    gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                    gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                    gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                    gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                    gobj_zcl_taxa_curva->set_tipo( c_frete ).
                    gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*                  GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
                    gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                             i_status = i_status ).

                    var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                    gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                       i_tcode    = i_tcode
                                                       ).

                    IF ( sy-cprog NE 'ZCARGA' ).
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = sy-datum
                                                                               i_tipo     = var_tipo
                                                                               ).

                    ELSE.
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = gw_zsdt0069-data_atual
                                                                               i_tipo     = var_tipo
                                                                               ).

                    ENDIF.
                    gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                    var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                  GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                    gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                          i_numero = gw_zsdt0051-nro_sol_ov
                                                          i_data   = gw_zsdt0051-dtde_logist
                                                          i_tipo   = c_frete ).

                    gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

                    var_safra = gw_zsdt0053-charg.
                    gobj_zcl_taxa_curva->set_safra( var_safra ).
                    me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                    "**********************************
                    "* FRETE - FIM
                    "**********************************
                  ENDLOOP.


                WHEN: 'B'.

                  "READ TABLE GT_ZSDT0053 INTO GW_ZSDT0053 WITH KEY NRO_SOL_OV = GW_ZSDT0051-NRO_SOL_OV.

                  LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov = gw_zsdt0051-nro_sol_ov.

                    "**********************************
                    "* FRETE - INICIO
                    "**********************************
                    FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                    CREATE OBJECT gobj_zcl_taxa_curva.
                    CREATE OBJECT gobj_zcl_webservice_tx_curva.

                    gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                    gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                    var_data_calculada = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                             i_data_final   = gw_zsdt0051-dtate_logist ).

                    data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                    gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                    gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                    gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                    IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                    ELSE.
                      IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.
                      ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.

                        " 29.08.2024 - 150168 - RAMON -->
                      ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.
                      ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.
                        " 29.08.2024 - 150168 - RAMON --<

                      ENDIF.
                    ENDIF.

                    " Conversão casas decimais
                    var_montante_2 = var_montante .
                    gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                    gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                    gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                    gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                    gobj_zcl_taxa_curva->set_tipo( c_frete ).
                    gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*                  GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
                    gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                             i_status = i_status ).

                    var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                    gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                       i_tcode    = i_tcode
                                                       ).

                    IF ( sy-cprog NE 'ZCARGA' ).
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = sy-datum
                                                                               i_tipo     = var_tipo
                                                                               ).
                    ELSE.
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = gw_zsdt0069-data_atual
                                                                               i_tipo     = var_tipo
                                                                               ).
                    ENDIF.

                    gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                    var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                  GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                    gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                          i_numero = gw_zsdt0051-nro_sol_ov
                                                          i_data   = gw_zsdt0051-dtde_logist
                                                          i_tipo   = c_frete ).

                    gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

                    var_safra = gw_zsdt0053-charg.
                    gobj_zcl_taxa_curva->set_safra( var_safra ).
                    me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                    "**********************************
                    "* FRETE - FIM
                    "**********************************

                    IF NOT ( gw_t052-ztag1 IS INITIAL ).

                      FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                      CREATE OBJECT gobj_zcl_taxa_curva.
                      CREATE OBJECT gobj_zcl_webservice_tx_curva.

                      gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                      gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                      var_data_calculada = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                               i_data_final   = gw_zsdt0051-dtate_logist ).

                      var_data_calculada = var_data_calculada + gw_t052-ztag1.
                      data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                      gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                      gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                         i_tcode    = i_tcode ).
                      gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                      gobj_zcl_taxa_curva->set_total_proporcional( gw_zsdt0053-vlrtot ).
                      gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                      gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                      gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                      gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).

                      gobj_zcl_taxa_curva->set_tipo( c_venda ).
                      gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*                    GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
                      gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                               i_status = i_status ).
                      var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                      gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                      IF ( sy-cprog NE 'ZCARGA').
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                 i_data_lib = sy-datum
                                                                                 i_tipo     = var_tipo
                                                                                 ).
                      ELSE.
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                 i_data_lib = gw_zsdt0069-data_atual
                                                                                 i_tipo     = var_tipo
                                                                                 ).
                      ENDIF.


                      gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                      var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                    GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                      gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                            i_numero = gw_zsdt0051-nro_sol_ov
                                                            i_data   = gw_zsdt0051-dtde_logist
                                                            i_tipo   = c_venda ).

                      gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

                      var_safra = gw_zsdt0053-charg.
                      gobj_zcl_taxa_curva->set_safra( var_safra ).
                      me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no banco de dados (ZSDT0094).

                      CLEAR: var_data_calculada.
                    ELSE.

                      FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                      CREATE OBJECT gobj_zcl_taxa_curva.
                      CREATE OBJECT gobj_zcl_webservice_tx_curva.

                      gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                      gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                      var_data_calculada = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                               i_data_final   = gw_zsdt0051-dtate_logist ).

                      var_mes = ( ( var_mes + var_data_calculada+4(2) ) + gw_t052-zmona ).
                      IF ( var_mes > 12 ).
                        var_mes_aux =  gw_t052-zmona.
                        var_ano = var_data_calculada(4) + 1.
                      ELSE.
                        var_mes_aux = var_mes.
                        var_ano     = var_data_calculada(4).
                      ENDIF.

                      CONCATENATE var_ano var_mes_aux var_data+6(2) INTO var_data_completa.
                      var_data_completa = var_data_completa +  gw_t052-zfael.

                      data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_completa ).
                      gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                      gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                         i_tcode    = i_tcode
                                                         ).
                      gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                      gobj_zcl_taxa_curva->set_total_proporcional( gw_zsdt0053-vlrtot ).
                      gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                      gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                      gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                      gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).

                      gobj_zcl_taxa_curva->set_tipo( c_venda ).
                      gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*                    GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
                      gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                               i_status = i_status ).
                      var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                      gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                      IF ( sy-cprog NE 'ZCARGA').
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "VAR_DATA_COMPLETA
                                                                                 i_data_lib = sy-datum
                                                                                 i_tipo     = var_tipo
                                                                                 ).

                      ELSE.
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "VAR_DATA_COMPLETA
                                                                                 i_data_lib = gw_zsdt0069-data_atual
                                                                                 i_tipo     = var_tipo
                                                                                 ).

                      ENDIF.

                      gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                      var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                    GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                      gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                            i_numero = gw_zsdt0051-nro_sol_ov
                                                            i_data   = data_venc
                                                            i_tipo   = c_venda ).

                      gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

                      var_safra = gw_zsdt0053-charg.
                      gobj_zcl_taxa_curva->set_safra( var_safra ).
                      me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                    ENDIF.
                  ENDLOOP.
              ENDCASE.


            ELSE. "Caso tenha INDICE OU ZSDT0055

              IF NOT ( gt_zsdt0055[] IS INITIAL ).

                READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY nro_sol_ov = gw_zsdt0051-nro_sol_ov.

                LOOP AT gt_zsdt0055 INTO gw_zsdt0055 WHERE nro_sol_ov EQ gw_zsdt0053-nro_sol_ov.

                  "**********************************
                  "* FRETE - INICIO
                  "**********************************
                  FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                  CREATE OBJECT gobj_zcl_taxa_curva.
                  CREATE OBJECT gobj_zcl_webservice_tx_curva.

                  gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                  gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                  data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                  gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).

                  gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                  gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                  gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                  gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                  gobj_zcl_taxa_curva->set_tipo( c_frete ).
                  gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                  gobj_zcl_taxa_curva->set_posnr( i_fixacao ).


                  IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                  ELSE.
                    IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                    ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                      " 29.08.2024 - 150168 - RAMON -->
                    ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                    ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                      " 29.08.2024 - 150168 - RAMON --<
                    ENDIF.

                  ENDIF.

                  " Conversão casas decimais
                  var_montante_2 = var_montante .
                  gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
*                GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
                  gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                           i_status = i_status ).
                  var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                  gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                                     i_tcode    = i_tcode
                                                     ).


                  IF ( sy-cprog NE 'ZCARGA').
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                             i_data_lib = sy-datum
                                                                             i_tipo     = var_tipo ).
                  ELSE.
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                             i_data_lib = gw_zsdt0069-data_atual
                                                                             i_tipo     = var_tipo ).
                  ENDIF.

                  gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                  var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                  gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                        i_numero = gw_zsdt0051-nro_sol_ov
                                                        i_data   = gw_zsdt0055-data_progr
                                                        i_tipo   = c_frete ).

                  gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

                  var_safra = gw_zsdt0053-charg.
                  gobj_zcl_taxa_curva->set_safra( var_safra ).
                  me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)
                  "**********************************
                  "* FRETE - FIM
                  "**********************************

                  FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                  CREATE OBJECT gobj_zcl_taxa_curva.
                  CREATE OBJECT gobj_zcl_webservice_tx_curva.

                  CASE gw_t052-zdart.
                    WHEN: 'D'.
*&----------------Inicio ajuste Bug Solto #149379 / aoenning&*
                      IF i_auart EQ 'ZCPV'.
                        LOOP AT i_vbap ASSIGNING <wa_vbap> WHERE vbeln EQ i_vbeln.
                          var_montante = var_montante + <wa_vbap>-netwr + <wa_vbap>-mwsbp.
                        ENDLOOP.
                      ELSE.
*&----------------Fim ajuste Bug Solto #149379 / aoenning&*

                        IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
                          var_montante = gw_zsdt0053-dmbtr * gw_zsdt0055-cadencia_qte.
                        ELSE.

                          IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                            var_montante = gw_zsdt0053-dmbtr / 1000.
                            var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                          ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                            var_montante = gw_zsdt0053-dmbtr  * 1000.
                            var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                            " 29.08.2024 - 150168 - RAMON -->
                          ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                            var_montante = gw_zsdt0053-dmbtr / 1000.
                            var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                          ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                            var_montante = gw_zsdt0053-dmbtr  * 1000.
                            var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                            " 29.08.2024 - 150168 - RAMON --<

                          ENDIF.
                        ENDIF.
                      ENDIF.
                      gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                      gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                      data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                      gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                      gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                      IF NOT ( gw_zsdt0055-cadencia_qte IS INITIAL ).
                        gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                                           i_tcode    = i_tcode
                                                           ).
                      ELSE.
                        gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                           i_tcode    = i_tcode
                                                           ).
                      ENDIF.

                      " Conversão casas decimais
                      var_montante_2 = var_montante .

                      gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
                      gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                      gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                      gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                      gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                      gobj_zcl_taxa_curva->set_tipo( c_venda ).
                      gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*                    GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
                      gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                               i_status = i_status ).

                      var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                      gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                      IF ( sy-cprog NE 'ZCARGA').
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                 i_data_lib = sy-datum
                                                                                 i_tipo     = var_tipo
                                                                                 ).

                      ELSE.
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                 i_data_lib = gw_zsdt0069-data_atual
                                                                                 i_tipo     = var_tipo
                                                                                 ).
                      ENDIF.

                      gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                      var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                    GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                      gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                            i_numero = gw_zsdt0051-nro_sol_ov
                                                            i_data   = gw_zsdt0055-data_progr
                                                            i_tipo   = c_venda ).

                      gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

                      var_safra = gw_zsdt0053-charg.
                      gobj_zcl_taxa_curva->set_safra( var_safra ).
                      me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                    WHEN: 'B'.
                      IF NOT ( gw_t052-ztag1 IS INITIAL ).

                        gw_zsdt0055-data_progr = gw_zsdt0055-data_progr + gw_t052-ztag1.

*&----------------Inicio ajuste Bug Solto #149379 / AOENNING&*
                        IF i_auart EQ 'ZCPV'.
                          LOOP AT i_vbap ASSIGNING <wa_vbap> WHERE vbeln EQ i_vbeln.
                            var_montante = var_montante + <wa_vbap>-netwr + <wa_vbap>-mwsbp.
                          ENDLOOP.
                        ELSE.
*&----------------Fim ajuste Bug Solto #149379 / AOENNING&*
                          IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
                            var_montante = gw_zsdt0053-dmbtr * gw_zsdt0055-cadencia_qte.
                          ELSE.

                            IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                              var_montante = gw_zsdt0053-dmbtr / 1000.
                              var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                            ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                              var_montante = gw_zsdt0053-dmbtr  * 1000.
*                              gw_zsdt0053-dmbtr = var_montante * gw_zsdt0055-cadencia_qte. ajuste Bug Solto #149379 / AOENNING
                              var_montante = var_montante * gw_zsdt0055-cadencia_qte.       "ajuste Bug Solto #149379 / AOENNING

                              " 29.08.2024 - 150168 - RAMON -->
                            ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                              var_montante = gw_zsdt0053-dmbtr / 1000.
                              var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                            ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                              var_montante = gw_zsdt0053-dmbtr  * 1000.
                              var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                              " 29.08.2024 - 150168 - RAMON --<

                            ENDIF.

                          ENDIF.
                        ENDIF.
                        gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                        gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                        data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                        gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                        gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                        IF NOT ( gw_zsdt0055-cadencia_qte IS INITIAL ).
                          gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                                             i_tcode    = i_tcode
                                                             ).
                        ELSE.
                          gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                             i_tcode    = i_tcode
                                                             ).
                        ENDIF.

                        " Conversão casas decimais
                        var_montante_2 = var_montante .

                        gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
                        gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                        gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                        gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                        gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                        gobj_zcl_taxa_curva->set_tipo( c_venda ).
                        gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*                      GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
                        gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                                 i_status = i_status ).

                        var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                        gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                        IF ( sy-cprog NE 'ZCARGA').
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                   i_data_lib = sy-datum
                                                                                   i_tipo     = var_tipo
                                                                                   ).

                        ELSE.
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                   i_data_lib = gw_zsdt0069-data_atual
                                                                                   i_tipo     = var_tipo
                                                                                   ).
                        ENDIF.

                        gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                        var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                      GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                        gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                              i_numero = gw_zsdt0051-nro_sol_ov
                                                              i_data   = gw_zsdt0055-data_progr
                                                              i_tipo   = c_venda ).

                        gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

                        var_safra = gw_zsdt0053-charg.
                        gobj_zcl_taxa_curva->set_safra( var_safra ).
                        me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                      ELSE.

                        gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                        gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                        var_mes = ( ( var_mes + gw_zsdt0055-data_progr+4(2) ) + gw_t052-zmona ).
                        IF ( var_mes > 12 ).
                          var_mes_aux =  gw_t052-zmona.
                          var_ano = gw_zsdt0055-data_progr(4) + 1.
                        ELSE.
                          var_mes_aux = var_mes.
                          var_ano     = gw_zsdt0051-dtde_logist(4).
                        ENDIF.

                        CONCATENATE var_ano var_mes_aux var_data+6(2) INTO var_data_completa.
                        var_data_completa = var_data_completa +  gw_t052-zfael.
                        data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_completa ).
                        gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                        gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                           i_tcode    = i_tcode
                                                           ).
                        gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                        gobj_zcl_taxa_curva->set_total_proporcional( gw_zsdt0053-vlrtot ).
                        gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                        gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                        gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                        gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                        gobj_zcl_taxa_curva->set_tipo( c_venda ).
                        gobj_zcl_taxa_curva->set_estorno( i_estorno ).
*                      GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
                        gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                                 i_status = i_status ).

                        var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                        gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                        IF ( sy-cprog NE 'ZCARGA').
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "VAR_DATA_COMPLETA
                                                                                   i_data_lib = sy-datum
                                                                                   i_tipo     = var_tipo
                                                                                   ).
                        ELSE.
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "VAR_DATA_COMPLETA
                                                                                   i_data_lib = gw_zsdt0069-data_atual
                                                                                   i_tipo     = var_tipo
                                                                                   ).
                        ENDIF.

                        gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                        var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                      GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                        gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                              i_numero = gw_zsdt0051-nro_sol_ov
                                                              i_data   = data_venc
                                                              i_tipo   = c_venda ).

                        gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

                        var_safra = gw_zsdt0053-charg.
                        gobj_zcl_taxa_curva->set_safra( var_safra ).
                        me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                      ENDIF.
                  ENDCASE.
                  CLEAR: var_cotacao, gw_zsdt0055, var_montante.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD ESTORNO_AQV.

    DATA: VAR_ESTORNO TYPE NUM10.

    DATA(OBJ_TX_CURVA)    = NEW ZCL_TAXA_CURVA( ).
    DATA(OBJ_W_TX_CURVA)  = NEW ZCL_WEBSERVICE_TX_CURVA( ).

    CONSTANTS: VENDA  TYPE C VALUE 'V',
               COMPRA TYPE C VALUE 'C'.

    SELECT *
      FROM ZSDT0094
      INTO TABLE @DATA(_0094)
    WHERE NRO_SOL_OV EQ @VBELN
        AND ESTORNO  EQ 0.

    CHECK SY-SUBRC IS INITIAL.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZEST_0094'
      IMPORTING
        NUMBER                  = VAR_ESTORNO
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    LOOP AT _0094 INTO DATA(WA).

      OBJ_TX_CURVA->SET_NUMERO( WA-NRO_SOL_OV ).
      OBJ_TX_CURVA->SET_INCOTERMS( WA-INCO1 ).
      OBJ_TX_CURVA->SET_DATA_VENC( WA-DATA_VENC ).
      OBJ_TX_CURVA->SET_POSNR( WA-FIXACAO ).
      OBJ_TX_CURVA->SET_DATA_LIB( SY-DATUM ).
      OBJ_TX_CURVA->SET_DATA_REGISTRO( WA-DATA_REGISTRO ).
      OBJ_TX_CURVA->SET_HORA_REGISTRO( WA-HORA_REGISTRO ).

      OBJ_TX_CURVA->SET_CADENCIA(
                                  I_CADENCIA = WA-CADENCIA_QTE
                                  I_NEGATIVA = ABAP_TRUE
                                 ).

      OBJ_TX_CURVA->SET_ZIEME( WA-ZIEME ).
      OBJ_TX_CURVA->SET_TOTAL_PROPORCIONAL(
                                            I_TOTAL =  WA-TOTAL_PROPORC
                                            I_NEGATIVA = ABAP_TRUE
                                          ).

      OBJ_TX_CURVA->SET_FRETE_CIF( WA-FRETE_CIF ).
      OBJ_TX_CURVA->SET_FRETE_PORTO( WA-FRETE_PORTO ).
      OBJ_TX_CURVA->SET_TIPO( WA-TIPO ).
      OBJ_TX_CURVA->SET_BEZEI( WA-BEZEI ).
      OBJ_TX_CURVA->SET_TAXA_CAMBIO( WA-TAXA_CAMBIO ).
      OBJ_TX_CURVA->SET_ESTORNO( VAR_ESTORNO ).

      OBJ_TX_CURVA->SET_TIPO_TAXA( COND #( WHEN WA-TIPO_TAXA EQ VENDA THEN COMPRA ELSE VENDA ) ).

      OBJ_TX_CURVA->SET_TAXA_CURVA(
          OBJ_W_TX_CURVA->BUSCAR_TAXA(
                                       I_DATA     = OBJ_TX_CURVA->GET_DATA_VENC( )
                                       I_DATA_LIB = OBJ_TX_CURVA->GET_DATA_LIB( )
                                       I_TIPO     = OBJ_TX_CURVA->GET_TIPO_TAXA( )
                                     )
                                  ).

      OBJ_TX_CURVA->SET_VBELN( WA-VBELN ).
      OBJ_TX_CURVA->SET_SAFRA( WA-SAFRA ).
      OBJ_TX_CURVA->SET_INTERCOMPANY( WA-INTERCOMPANY ).

      ME->ZIF_TAXA_CURVA_DB~ATUALIZAR( OBJ_TX_CURVA ).
      ME->ZIF_TAXA_CURVA_DB~INSERIR_IN( OBJ_TX_CURVA ).

    ENDLOOP.

  ENDMETHOD.


  METHOD estorno_in.

    DATA: var_estorno      TYPE num10,
          it_zsdt0094      TYPE TABLE OF zsdt0094,
          v_dir            TYPE c LENGTH 4,
          seq              TYPE posnr,
          qtd              TYPE dzmeng,
          vl_cadencia      TYPE dzmeng,
          vl_brgew         TYPE vbap-brgew,
          vl_netwr         TYPE vbak-netwr,
          var_total        TYPE dmbtr,
          vl_posnr         TYPE posnr,
          wl_zsdt0090_prec TYPE zsdt0090,
          wl_vbap          TYPE vbap,
          wl_mara          TYPE mara.

    DATA: obj_tx_curva TYPE REF TO zcl_webservice_tx_curva,
          obj_insere   TYPE REF TO zcl_taxa_curva_db,
          obj_0094     TYPE REF TO zcl_taxa_curva.

    FREE: obj_tx_curva, obj_insere, obj_0094.
    CREATE OBJECT: obj_tx_curva, obj_insere, obj_0094.

    CASE sy-cprog.
      WHEN 'SAPMV60A'. "******* VA01
        v_dir = |%{ i_dir }|.

        SELECT * FROM zsdt0094
          INTO TABLE it_zsdt0094
          WHERE vbeln EQ i_vbeln
            AND programa EQ sy-cprog
            AND bezei LIKE v_dir
            AND estorno  EQ 0.

      WHEN 'ZSDR0042'. "******* ZSDT0087

        CASE i_dir.
          WHEN 'J'. "TAXA CAMBIO

*  Verifica quantos Itens o Vbelv existem ba 90 com Cambio
            FREE it_zsdt0094.
            CLEAR: vl_brgew, vl_netwr, vl_cadencia, wl_zsdt0090_prec, wl_vbap, wl_mara.
            SELECT SINGLE *
              FROM zsdt0090 INTO @DATA(wa_0090_aux01)
             WHERE vbelv EQ @i_vbeln
               AND categoria EQ 'C'
               AND estorno EQ @abap_false.

            IF sy-subrc IS INITIAL.

              seq = wa_0090_aux01-sequencia.

              SELECT *
                FROM zsdt0094 APPENDING TABLE it_zsdt0094
               WHERE nro_sol_ov EQ wa_0090_aux01-doc_simulacao
                 AND fixacao EQ seq
                 AND programa EQ sy-cprog
                 AND tipo     EQ 'VDI'
                 AND estorno  EQ 0.

              SELECT SUM( brgew )
                FROM vbap INTO vl_brgew
               WHERE vbeln EQ i_0090-vbelv.

              vl_cadencia = vl_brgew.

              SELECT SINGLE netwr
                FROM vbak INTO vl_netwr
               WHERE vbeln EQ i_0090-vbelv.

              SELECT SINGLE * FROM vbap
               INTO wl_vbap
               WHERE vbeln EQ i_0090-vbelv.

              SELECT SINGLE * FROM mara
                INTO wl_mara
                WHERE matnr EQ wl_vbap-matnr.

              CALL FUNCTION 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
                EXPORTING
                  i_doc_simulacao = i_0090-doc_simulacao
                  i_vbeln         = i_0090-vbelv
                CHANGING
                  c_zsdt0090_prec = wl_zsdt0090_prec.

            ENDIF.

          WHEN 'T'. "TAXA CAMBIO


*  Verifica quantos Itens o Vbelv existem ba 90 com Cambio
            FREE it_zsdt0094.

            SELECT *
              FROM zsdt0090
                INTO TABLE @DATA(gt_0090)
              WHERE vbelv EQ @i_vbeln
                AND categoria EQ 'C'
                AND estorno EQ @abap_false.

*            SELECT *
*              FROM zsdt0090
*                APPENDING TABLE gt_0090
*              WHERE vbeln   EQ i_vbeln
*                AND estorno EQ abap_false.

            IF gt_0090[] IS NOT INITIAL.

*            Add cada item encontrado na 94 para realizar a reversão
              LOOP AT gt_0090 INTO DATA(wa_0090).

                IF i_0090_manual IS INITIAL. " 16.02.2023 - RAMON - Ajuste para nao apagar automatico

*            Atualiza o Campo ESTORNO para 'X' com a situação Estornado.
                  UPDATE  zsdt0090 SET estorno       = abap_true
                                       usnam_e       = sy-uname
                                       data_atual_e  = sy-datum
                                       hora_atual_e  = sy-uzeit
                                       origem_est    = sy-cprog
                                       WHERE doc_simulacao EQ wa_0090-doc_simulacao
                                         AND vbelv     EQ wa_0090-vbelv
                                         AND categoria EQ 'C'
                                         AND sequencia EQ wa_0090-sequencia
                                         AND estorno   EQ abap_false.
                  IF sy-subrc IS INITIAL.
                    COMMIT WORK.
                  ENDIF.

                  seq = wa_0090-sequencia.

                  SELECT *
                    FROM zsdt0094
                      APPENDING TABLE it_zsdt0094
                    WHERE nro_sol_ov EQ wa_0090-doc_simulacao
                      AND fixacao EQ seq
                      AND programa EQ sy-cprog
                      AND tipo     EQ 'VDI'
                      AND estorno  EQ 0.

                ELSE.

                  seq = i_seq.

                  SELECT *
                    FROM zsdt0094
                      INTO TABLE it_zsdt0094
                    WHERE nro_sol_ov EQ wa_0090-doc_simulacao
                      AND fixacao EQ seq
                      AND programa EQ sy-cprog
                      AND tipo     EQ 'VDI'
                      AND estorno  EQ 0.

                ENDIF.

              ENDLOOP.
            ENDIF.

          WHEN 'V'.

            SELECT *
              FROM vbap
              INTO TABLE @DATA(it_vbap)
              WHERE vbeln EQ @i_vbeln.

            DATA(rt_vbap) = it_vbap[].
            FREE: rt_vbap.

            DATA(lt_vbap) = it_vbap[].
            SORT lt_vbap BY vbeln.
            DELETE ADJACENT DUPLICATES FROM lt_vbap COMPARING vbeln.

            SELECT * FROM mara
              INTO TABLE @DATA(it_mara)
              FOR ALL ENTRIES IN @it_vbap
                WHERE matnr EQ @it_vbap-matnr.

            LOOP AT it_vbap ASSIGNING FIELD-SYMBOL(<fvbap>).
              obj_0094->set_matkl( i_matkl = it_mara[ matnr = <fvbap>-matnr ]-matkl
                                   i_brgew = it_mara[ matnr = <fvbap>-matnr ]-brgew
                                 ).
              obj_0094->set_zieme( <fvbap>-zieme ).
              qtd = <fvbap>-kwmeng.
              obj_0094->set_cadencia_in( qtd ).
              <fvbap>-kwmeng = obj_0094->get_cadencia( ).
            ENDLOOP.


            rt_vbap = VALUE #( FOR <vbap> IN lt_vbap (
                        vbeln = <vbap>-vbeln
                        kwmeng = REDUCE #(
                                          INIT lv_sum = 0
                                          FOR wa_vbap IN it_vbap
                                            WHERE ( vbeln = <vbap>-vbeln )
                                              NEXT lv_sum = lv_sum + wa_vbap-kwmeng
                                          )
                       ) ).

            DATA(rw_vbap) = rt_vbap[ 1 ].

          WHEN OTHERS.

            CHECK NOT i_seq IS INITIAL.

            seq = i_seq.
            SELECT * FROM zsdt0094
              INTO TABLE it_zsdt0094
              WHERE fixacao    EQ seq
                AND nro_sol_ov EQ i_numero
                AND programa   EQ sy-cprog
                AND estorno    EQ 0.

        ENDCASE.

      WHEN 'ZSDR016'. " ******* ZSDT0044
        SELECT * FROM zsdt0094
          INTO TABLE it_zsdt0094
          WHERE nro_sol_ov EQ i_numero
            AND programa EQ sy-cprog
            AND estorno  EQ 0.
    ENDCASE.

    CHECK NOT it_zsdt0094  IS INITIAL.

    IF i_dir NE 'J'.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZEST_0094'
        IMPORTING
          number                  = var_estorno
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

    ENDIF.

    LOOP AT it_zsdt0094 INTO DATA(wa_zsdt0094).

      IF ( i_dir = 'J'                                        ) AND
         ( wl_zsdt0090_prec-matkl  IS NOT INITIAL             ) AND
         ( wl_zsdt0090_prec-matklv IS NOT INITIAL             ) AND
         ( wl_zsdt0090_prec-matkl  NE wl_zsdt0090_prec-matklv ).

        obj_0094->set_numero( wa_zsdt0094-nro_sol_ov ).
        obj_0094->set_incoterms( wl_zsdt0090_prec-inco1 ).
        obj_0094->set_data_venc( wa_zsdt0094-data_venc  ).

        obj_0094->set_matkl( i_matkl = wl_mara-matkl
                             i_brgew = wl_mara-brgew ).

        vl_posnr =  i_0090-sequencia.
        obj_0094->set_posnr( vl_posnr ).
        obj_0094->set_data_lib( sy-datum ).
        obj_0094->set_data_registro( sy-datum ).
        obj_0094->set_hora_registro( sy-uzeit ).

        IF i_0090-matklv EQ '658445'.
          obj_0094->set_cadencia_in( i_cadencia = 0 ).
        ELSE.
          obj_0094->set_cadencia_in( i_cadencia = vl_cadencia
                                     i_negativa = 'S' ).
        ENDIF.

        var_total = ( vl_netwr * wa_0090_aux01-kurrf ).
        obj_0094->set_total_proporcional( i_total    = var_total
                                          i_negativa = abap_true ).

        obj_0094->set_frete_cif( wa_zsdt0094-frete_cif ).
        obj_0094->set_frete_porto( wa_zsdt0094-frete_porto ).
        obj_0094->set_tipo( wa_zsdt0094-tipo ).
        obj_0094->set_taxa_cambio( wa_zsdt0094-taxa_cambio ).
        obj_0094->set_estorno( var_estorno ).
        obj_0094->set_tipo_taxa( wa_zsdt0094-tipo_taxa ).
        obj_0094->tipo_taxa_in( i_tipo ).

        IF wa_zsdt0094-taxa_curva IS NOT INITIAL.
          obj_0094->set_taxa_curva(
          obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                     i_data_lib = obj_0094->get_data_lib( )
                                     i_tipo     = obj_0094->get_tipo_taxa( )
                                    ) ).
        ENDIF.

        obj_0094->set_vbeln( i_0090-vbelv ).
        obj_0094->set_safra( wa_zsdt0094-safra ).
        obj_insere->zif_taxa_curva_db~atualizar( obj_0094 ).
        obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).

      ELSE.

        obj_0094->set_numero( wa_zsdt0094-nro_sol_ov ).
        obj_0094->set_incoterms( wa_zsdt0094-inco1 ).
        "obj_0094->set_data_venc( wa_zsdt0094-data_venc ). " 19.01.2024, foi comentado porque está pegando a data errada na hora de estornar o hedge na transação ZSDT0087 - 351775

        " 22.06.2023 - RAMON - Codigo para voltar a ultima prev de pag valida
        " só deve entrar aqui no estorno da trava de cambio feito pela ZSDT0087

*        obj_0094->set_data_venc( i_0090-data_prevpgto ).
*
*        " se a data de vencimento anterior for menor igual que data atual, passa ela para ser atualizada
*        IF i_0090-data_prevpgtov IS NOT INITIAL
*           AND i_0090-data_prevpgtov <= sy-datum.
*          obj_0094->set_data_venc( i_0090-data_prevpgtov ).
*        ENDIF.
*
*        IF obj_0094->get_data_venc( ) IS INITIAL.
*          obj_0094->set_data_venc( wa_zsdt0094-data_venc ).
*        ENDIF.

*** Stefanini - IR202925 - 11/10/2024 - LAZAROSR - Início de Alteração
        IF i_0090 IS NOT INITIAL.
*** Stefanini - IR202925 - 11/10/2024 - LAZAROSR - Fim de Alteração

          " verifica se o estorno é da mesma ov
          IF wa_zsdt0094-vbeln EQ i_0090-vbelv.

            "21.07.2023 -->
            IF i_0090-data_prevpgtov < sy-datum.
              obj_0094->set_data_venc( i_0090-data_prevpgto ).
            ELSE.
              "obj_0094->set_data_venc( i_0090-data_prevpgtov ).
              obj_0094->set_data_venc( wa_zsdt0094-data_venc ).
              "obj_0094->set_data_venc( i_0090-data_prevpgto ).
            ENDIF.
            "21.07.2023 --<
            "obj_0094->set_data_venc( wa_zsdt0094-data_venc ).


            " se for diferente, então pega a data da 0090
          ELSE.

            " a data de prev pg anterior é menor que hoje?
            IF i_0090-data_prevpgtov < sy-datum.
              " se sim, coloca a data pg atual
              obj_0094->set_data_venc( i_0090-data_prevpgto ).

              " se não tiver vencida, coloca prev pg anterior
            ELSE.
              obj_0094->set_data_venc( i_0090-data_prevpgtov ).
            ENDIF.


          ENDIF.

*** Stefanini - IR202925 - 11/10/2024 - LAZAROSR - Início de Alteração
        ELSE.

          obj_0094->set_data_venc( wa_zsdt0094-data_venc ).

        ENDIF.
*** Stefanini - IR202925 - 11/10/2024 - LAZAROSR - Fim de Alteração


        " ------<<

        IF i_dir = 'J'.
          vl_posnr =   i_0090-sequencia.
          obj_0094->set_posnr( vl_posnr ).
        ELSE.
          obj_0094->set_posnr( wa_zsdt0094-fixacao ).
        ENDIF.
        obj_0094->set_data_lib( sy-datum ).
        obj_0094->set_data_registro( wa_zsdt0094-data_registro ).
        obj_0094->set_hora_registro( wa_zsdt0094-hora_registro ).


        IF i_dir = 'J'.
          IF i_0090-matklv EQ '658445'.
            obj_0094->set_cadencia_in( i_cadencia = 0 ).
          ELSE.
            obj_0094->set_cadencia_in( i_cadencia = vl_cadencia
                                       i_estorno  = abap_true ).
          ENDIF.
          obj_0094->set_zieme( wa_zsdt0094-zieme ).

          var_total = ( vl_netwr * wa_0090_aux01-kurrf ).
          obj_0094->set_total_proporcional( i_total    = var_total
                                            i_negativa = abap_true ).

        ELSE.
          obj_0094->set_cadencia_in( i_cadencia = wa_zsdt0094-cadencia_qte
                                     i_estorno = abap_true
                                    ).


          obj_0094->set_zieme( wa_zsdt0094-zieme ).
          obj_0094->set_total_proporcional( i_total =  wa_zsdt0094-total_proporc
                                            i_negativa = abap_true
                                          ).
        ENDIF.

        obj_0094->set_frete_cif( wa_zsdt0094-frete_cif ).
        obj_0094->set_frete_porto( wa_zsdt0094-frete_porto ).
        obj_0094->set_tipo( wa_zsdt0094-tipo ).
        obj_0094->set_bezei( wa_zsdt0094-bezei ).
        obj_0094->set_taxa_cambio( wa_zsdt0094-taxa_cambio ).
        obj_0094->set_estorno( var_estorno ).
        obj_0094->set_tipo_taxa( wa_zsdt0094-tipo_taxa ).
        obj_0094->tipo_taxa_in( i_tipo ).

        IF wa_zsdt0094-taxa_curva IS NOT INITIAL.
          obj_0094->set_taxa_curva(
          obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                     i_data_lib = obj_0094->get_data_lib( )
                                     i_tipo     = obj_0094->get_tipo_taxa( )
                                    ) ).
        ENDIF.

        obj_0094->set_vbeln( wa_zsdt0094-vbeln ).
        obj_0094->set_safra( wa_zsdt0094-safra ).
        obj_insere->zif_taxa_curva_db~atualizar( obj_0094 ).
        obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).
      ENDIF.

    ENDLOOP.

    IF sy-cprog EQ 'ZSDR016'.
      UPDATE zsdt0040
                  SET job = abap_true
             WHERE doc_simulacao EQ i_numero.
    ENDIF.

  ENDMETHOD.


  METHOD frame.

    CONSTANTS: c_venda  TYPE c LENGTH 3 VALUE 'VDA'.

    TYPES: BEGIN OF ty_saldo_frame,
             nro_sol_ov TYPE zsdt0095-nro_sol_ov,
             fixacao    TYPE zsdt0095-fixacao,
             valor      TYPE zsdt0095-qtd_hedge,
             saldo      TYPE zsdt0095-qtd_hedge,
             data       TYPE zsdt0095-valdt_hedge,
             bezei      TYPE c LENGTH 3,
           END OF ty_saldo_frame.

    TYPES: BEGIN OF ty_zsdt0095,
             bezei       TYPE zsdt0095-bezei,
             fixacao     TYPE zsdt0095-fixacao,
             nro_sol_ov  TYPE zsdt0095-nro_sol_ov,
             valdt_hedge TYPE zsdt0095-valdt_hedge,
             qtd_hedge   TYPE zsdt0095-qtd_hedge,
           END OF ty_zsdt0095.

    TYPES: BEGIN OF ty_data,
             fixacao TYPE zsdt0095-fixacao,
             data    TYPE  zsdt0095-valdt_hedge,
           END  OF ty_data.

    TYPES: BEGIN OF ty_teste,
             valdt_hedge TYPE dats,
           END OF ty_teste.

    TYPES: BEGIN OF ty_soma,
             fixacao  TYPE sy-tabix,
             bezei    TYPE char3,
             soma     TYPE dzmeng,
             formula2 TYPE dzmeng,
           END OF ty_soma.

    DATA: soma     TYPE dzmeng,
          formula2 TYPE dzmeng.

    DATA: gt_zsdt0051          TYPE TABLE OF zsdt0051, "Tabela de Solicitação Ordem de Venda - Cabeçalho
          gt_zsdt0052          TYPE TABLE OF zsdt0052, "Tabela de Solicitação Ordem de Venda – COND_PGTO
          gt_zsdt0053          TYPE TABLE OF zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
          gt_zsdt0055          TYPE TABLE OF zsdt0055, "Tabela de Solicitação Ordem de Venda – Logistica
          gt_0055_aux          TYPE TABLE OF zsdt0055,
          gt_zsdt0059          TYPE TABLE OF zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
          gt_zsdt0059_cambio   TYPE TABLE OF zsdt0059, ""Tabela de Solicitação Ordem de Venda – PRECO
          gt_zsdt0059_conv_bio TYPE TABLE OF zsdt0059, " 22.08.2023
          gt_zsdt0073          TYPE TABLE OF zsdt0073, "Solicitação de Ordem de Venda – Fixação - condições esp.
          gt_t052              TYPE TABLE OF t052,     "Condições de pagamento
          gt_zsdt0094          TYPE TABLE OF zsdt0094,
          gt_zsdt0095          TYPE TABLE OF ty_zsdt0095,
          gt_zsdt0095_soma     TYPE TABLE OF ty_zsdt0095,
          gt_data              TYPE TABLE OF ty_data,
          gt_teste             TYPE TABLE OF zsdt0059,
          gt_teste_51          TYPE TABLE OF zsdt0059,
          gt_teste_v           TYPE TABLE OF ty_teste,
          gt_soma              TYPE TABLE OF ty_soma.


    DATA: gw_zsdt0051        TYPE zsdt0051, "Tabela de Solicitação Ordem de Venda - Cabeçalho
          gw_zsdt0052        TYPE zsdt0052, "Tabela de Solicitação Ordem de Venda – COND_PGTO
          gw_zsdt0053        TYPE zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
          gw_zsdt0055        TYPE zsdt0055, "Tabela de Solicitação Ordem de Venda – Logistica
          gw_0055_aux        TYPE zsdt0055,
          gw_zsdt0059        TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
          gw_zsdt0059_cambio TYPE zsdt0059, ""Tabela de Solicitação Ordem de Venda – PRECO
          gw_zsdt0073        TYPE zsdt0073, "Solicitação de Ordem de Venda – Fixação - condições esp.
          gw_t052            TYPE t052,     "Condições de pagamento
          gw_zsdt0094        TYPE zsdt0094,
          gw_zsdt0095        TYPE ty_zsdt0095,
          gw_zsdt0095_soma   TYPE ty_zsdt0095,
          gw_data            TYPE ty_data,
          gw_soma            TYPE ty_soma.

    DATA:
      gobj_zcl_taxa_curva          TYPE REF TO zcl_taxa_curva, "Objeto da taxa curva.
      gobj_zcl_webservice_tx_curva TYPE REF TO zcl_webservice_tx_curva.


    DATA: var_cadencia           TYPE dzmeng, "Valor da Cadencia
          var_cade_soma          TYPE dzmeng, "Soma Cadencia 94
          var_total_proporcional TYPE zsdt0053-dmbtr, "Valor Total Proporcional
          var_cotacao            TYPE kurrf,
          var_taxa_cambio        TYPE ukursp, "Valor da Cotação.
          var_tipo               TYPE char01.


    DATA: var_data           TYPE datum,
          var_data_completa  TYPE datum,
          var_mes            TYPE i,
          var_mes_aux        TYPE c LENGTH 2,
          var_ano            TYPE c LENGTH 4,
          var_msg            TYPE string, "Variavel para mostrar a Mensagem texto da exception.
          var_tabix          TYPE sy-tabix,
          var_verifica_frete TYPE zsdt0095-fixacao.

    DATA:    var_data_calculada TYPE d.
    DATA: data_venc TYPE datum.
    DATA: var_len   TYPE i.

    DATA: saldo_aux  TYPE zsdt0095-qtd_hedge.
    DATA: saldo_aux2 TYPE zsdt0095-qtd_hedge.
    DATA: aux_cadencia TYPE zsdt0095-qtd_hedge.
    DATA: up_cadencia TYPE zsdt0095-qtd_hedge.

    DATA: saldo_dife TYPE zsdt0095-qtd_hedge.

    DATA lv_formula2_bkp TYPE zsdt0095-qtd_hedge.

    DATA: gt_saldo TYPE TABLE OF ty_saldo_frame,
          gw_saldo TYPE ty_saldo_frame.

    DATA: gt_saldo_aux TYPE TABLE OF ty_saldo_frame,
          gw_saldo_aux TYPE ty_saldo_frame.

    DATA: gt_saldo_s TYPE TABLE OF ty_saldo_frame,
          gw_saldo_s TYPE ty_saldo_frame.


    DATA: gt_zsdt0056 TYPE TABLE OF zsdt0056,
          gw_zsdt0056 TYPE zsdt0056.

    DATA: r_bezei      TYPE RANGE OF zsdt0056-bezei,
          r_bezei_line LIKE LINE OF r_bezei.

    DATA var_safra TYPE ajahr.

    DATA(obj_auart) = NEW zcl_taxa_curva( ).

    DATA: r_auart TYPE RANGE OF auart.
    DATA: r_comp TYPE RANGE OF auart.
    DATA: r_devo_recu TYPE RANGE OF auart.

    r_comp = obj_auart->get_auart( 'ZHEDGECOMP' ). " Get SET de AUART de Complemento
    r_devo_recu = obj_auart->get_auart( 'ZHEDGEDEVO/RECU' ). " Get SET de AUART de Devolução/Recusa
    r_auart = obj_auart->get_auart( 'TODOS' ). " Get SET de AUART de Complemento/Devolução/Recusa

    FIELD-SYMBOLS: <fs_zsdt0055> TYPE zsdt0055.

    DATA: cx_exception TYPE REF TO zcx_webservice. "Referencia para a Classe de Exception.

    SELECT * FROM zsdt0056
      INTO TABLE gt_zsdt0056.
    IF ( sy-subrc EQ 0 ).

      LOOP AT gt_zsdt0056 INTO gw_zsdt0056.

        var_len  = strlen( gw_zsdt0056-bezei ).

        CASE var_len.
          WHEN: '2' OR '3'.
            IF ( gw_zsdt0056-bezei(1) EQ 'T' ).
              CLEAR: r_bezei_line.

              r_bezei_line-sign   =  'I'.
              r_bezei_line-option = 'EQ'.
              r_bezei_line-low    = gw_zsdt0056-bezei.
              r_bezei_line-high   = gw_zsdt0056-bezei.
              APPEND r_bezei_line TO r_bezei.

            ENDIF.
          WHEN OTHERS.
            CLEAR: gw_zsdt0056, var_len.
            CONTINUE.
        ENDCASE.
        CLEAR: gw_zsdt0056, var_len.
      ENDLOOP.
    ENDIF.

    "Tabela de Solicitação Ordem de Venda - Cabeçalho
    SELECT *
      FROM zsdt0051
      INTO TABLE gt_zsdt0051
    WHERE nro_sol_ov EQ i_numero.

    "Tabela de Solicitação Ordem de Venda – PRECO
    SELECT * FROM zsdt0059
       INTO TABLE gt_zsdt0059
       FOR ALL ENTRIES IN gt_zsdt0051
     WHERE nro_sol_ov  EQ gt_zsdt0051-nro_sol_ov
       AND bezei       IN r_bezei
       AND field       EQ 'QTDFIXADA'.


    SELECT * FROM zsdt0059
       INTO TABLE gt_zsdt0059_cambio
       FOR ALL ENTRIES IN gt_zsdt0051
     WHERE nro_sol_ov  EQ gt_zsdt0051-nro_sol_ov
       AND bezei       IN r_bezei
       AND field       EQ 'PRECO'.

    " 22.08.2023 - Ramon -->
    SELECT * FROM zsdt0059
       INTO TABLE gt_zsdt0059_conv_bio
       FOR ALL ENTRIES IN gt_zsdt0051
     WHERE nro_sol_ov  EQ gt_zsdt0051-nro_sol_ov
       AND bezei       EQ 'CONVERSOR BIO'.
    " 22.08.2023 - Ramon --<

    CHECK NOT  gt_zsdt0059[] IS INITIAL.

    DELETE gt_zsdt0059 WHERE valdt       IS INITIAL.
    DELETE gt_zsdt0059 WHERE valdt_hedge IS NOT INITIAL.

    DELETE gt_zsdt0059_cambio WHERE valdt       IS INITIAL.
    DELETE gt_zsdt0059_cambio WHERE valdt_hedge IS NOT INITIAL.

    " tabela para totalizar a Cadencia.
    IF i_auart IN r_auart.
      IF gt_zsdt0059 IS NOT INITIAL.
        SELECT * FROM zsdt0094
          INTO TABLE gt_zsdt0094
          FOR ALL ENTRIES IN gt_zsdt0059
          WHERE bezei EQ gt_zsdt0059-bezei
            AND fixacao EQ gt_zsdt0059-posnr
            AND nro_sol_ov EQ gt_zsdt0059-nro_sol_ov.
      ENDIF.
    ENDIF.

*    CASE I_AUART.
*      WHEN 'ZCPV' OR 'ZCOP'
*        OR 'ZROB' OR 'ZREB'
*        OR 'ZDMI' OR 'ZCFX'
*        OR 'ZROF'.
*
*        IF GT_ZSDT0059 IS NOT INITIAL.
*          SELECT * FROM ZSDT0094
*            INTO TABLE GT_ZSDT0094
*            FOR ALL ENTRIES IN GT_ZSDT0059
*            WHERE BEZEI EQ GT_ZSDT0059-BEZEI
*              AND FIXACAO EQ GT_ZSDT0059-POSNR
*              AND NRO_SOL_OV EQ GT_ZSDT0059-NRO_SOL_OV.
*        ENDIF.
*    ENDCASE.

    "Tabela de Solicitação Ordem de Venda – COND_PGTO
    SELECT * FROM zsdt0052
      INTO TABLE gt_zsdt0052
      FOR ALL ENTRIES IN gt_zsdt0051
     WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.

    "Tabela de Solicitação de ordem de venda - MATERIAIS
    SELECT * FROM zsdt0053
      INTO TABLE gt_zsdt0053
      FOR ALL ENTRIES IN gt_zsdt0059
    WHERE nro_sol_ov EQ gt_zsdt0059-nro_sol_ov
      AND posnr      EQ gt_zsdt0059-posnr1
      AND fixacao    EQ gt_zsdt0059-posnr.

    SELECT * FROM zsdt0055
      INTO TABLE gt_zsdt0055
      FOR ALL ENTRIES IN gt_zsdt0051
    WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.

    IF i_ucomm EQ 'MODRED'.

      SELECT * FROM zsdt0055
        INTO TABLE gt_zsdt0055
        FOR ALL ENTRIES IN gt_zsdt0059
        WHERE nro_sol_ov EQ gt_zsdt0059-nro_sol_ov
        AND fixacao    EQ gt_zsdt0059-posnr.

      DELETE gt_zsdt0055 WHERE status EQ 'Y'.
      DELETE gt_zsdt0055 WHERE status EQ 'W'.
      DELETE gt_zsdt0055 WHERE status EQ 'C'.

    ENDIF.

    CASE i_tcode.
      WHEN: 'VF01'.
        DELETE gt_zsdt0053 WHERE status EQ 'Y'.
        DELETE gt_zsdt0053 WHERE status EQ 'C'.
        DELETE gt_zsdt0055 WHERE status EQ 'Y'.
        DELETE gt_zsdt0055 WHERE status EQ 'W'.
        DELETE gt_zsdt0055 WHERE status EQ 'C'.
    ENDCASE.

    SELECT *
      FROM zsdt0073
      INTO TABLE gt_zsdt0073
      FOR ALL ENTRIES IN gt_zsdt0059
    WHERE nro_sol_ov EQ gt_zsdt0059-nro_sol_ov
      AND fixacao    EQ gt_zsdt0059-posnr.

    "Condições de pagamento
    SELECT * FROM t052
      INTO TABLE gt_t052
      FOR ALL ENTRIES IN gt_zsdt0073
    WHERE zterm EQ gt_zsdt0073-zterm.


    IF NOT ( gt_zsdt0055[] IS INITIAL ).
      LOOP AT gt_zsdt0055 ASSIGNING <fs_zsdt0055>.
        IF ( <fs_zsdt0055>-valdt_hedge IS INITIAL ).
          READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY fixacao = <fs_zsdt0055>-fixacao.
          IF ( sy-subrc EQ 0 ).
            <fs_zsdt0055>-valdt_hedge = gw_zsdt0053-valdt.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    UNASSIGN <fs_zsdt0055>.


    LOOP AT gt_zsdt0055 INTO gw_zsdt0055.

      gw_0055_aux-nro_sol_ov   = gw_zsdt0055-nro_sol_ov.
      gw_0055_aux-cadencia_qte = gw_zsdt0055-cadencia_qte.
      gw_0055_aux-valdt_hedge  = gw_zsdt0055-valdt_hedge.
      gw_0055_aux-fixacao      = gw_zsdt0055-fixacao.
      gw_0055_aux-status       = gw_zsdt0055-status.
      gw_0055_aux-vbeln        = gw_zsdt0055-vbeln.
      gw_0055_aux-zieme        = gw_zsdt0055-zieme.

      IF gw_zsdt0055-valdt_hedge > 0.
        COLLECT gw_0055_aux INTO gt_0055_aux.
      ELSE.
        gw_0055_aux-data_progr   = gw_zsdt0055-data_progr.
        gw_0055_aux-id   = gw_zsdt0055-id.

        APPEND gw_0055_aux TO gt_0055_aux.
      ENDIF.
    ENDLOOP.

    SORT gt_zsdt0059 BY posnr bezei ASCENDING.

    "Lançar uma negativa do frame.

    IF i_auart NOT IN r_auart.
      LOOP AT gt_zsdt0059 INTO gw_zsdt0059.

        me->frame_edicao( i_numero  = gw_zsdt0059-nro_sol_ov
                          i_fixacao = gw_zsdt0059-posnr
                          i_bezei   = gw_zsdt0059-bezei
                          i_tcode   = i_tcode ).
      ENDLOOP.
    ENDIF.
*
*    CASE I_AUART.
*      WHEN 'ZCPV' OR 'ZCOP'
*        OR 'ZROB' OR 'ZREB'
*        OR 'ZDMI' OR 'ZCFX'
*        OR 'ZROF'.
*      WHEN OTHERS.
*        LOOP AT GT_ZSDT0059 INTO GW_ZSDT0059.
*
*          ME->FRAME_EDICAO( I_NUMERO  = GW_ZSDT0059-NRO_SOL_OV
*                            I_FIXACAO = GW_ZSDT0059-POSNR
*                            I_BEZEI   = GW_ZSDT0059-BEZEI
*                            I_TCODE   = I_TCODE ).
*        ENDLOOP.
*    ENDCASE.

    "     Seleção para montagem do saldo.
    SELECT bezei fixacao nro_sol_ov valdt_hedge qtd_hedge
      FROM zsdt0095
      INTO TABLE gt_zsdt0095
      FOR ALL ENTRIES IN gt_zsdt0059
    WHERE nro_sol_ov  EQ gt_zsdt0059-nro_sol_ov
      AND fixacao     EQ gt_zsdt0059-posnr.
*        AND BEZEI       EQ GT_ZSDT0059-BEZEI.

*  BREAK ABAP.

    MOVE gt_0055_aux TO gt_zsdt0055.
*  MOVE GT_ZSDT0055 TO GT_0055_AUX.

    LOOP AT gt_zsdt0059 INTO gw_zsdt0059.

      CLEAR:  soma, formula2.
*    MOVE GT_ZSDT0055 TO GT_0055_AUX.
      formula2 = gw_zsdt0059-formula2.

      LOOP AT gt_0055_aux INTO gw_0055_aux WHERE fixacao EQ gw_zsdt0059-posnr.
        var_tabix = sy-tabix.

*       Comparação da soma com a quantidade utilizada fixada na zsdt0059,
*       utiliza quando faz uma fixação parcial.
        IF i_tcode NE 'VF01'.
          IF i_tcode NE 'ZSDT0066'.
            IF soma GE formula2.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_0055_aux-nro_sol_ov.

        READ TABLE gt_zsdt0073 INTO gw_zsdt0073 WITH KEY nro_sol_ov = gw_0055_aux-nro_sol_ov
                                                         fixacao    = gw_0055_aux-fixacao.
        READ TABLE gt_t052     INTO gw_t052     WITH KEY zterm      = gw_zsdt0073-zterm.

        READ TABLE gt_data INTO gw_data WITH KEY fixacao = gw_0055_aux-fixacao.
        IF ( sy-subrc NE 0 ).

          gw_data-fixacao = gw_0055_aux-fixacao.

          IF ( gw_0055_aux-valdt_hedge IS INITIAL ).
            CASE gw_t052-zdart.
              WHEN: 'B'.
                IF NOT ( gw_t052-ztag1 IS INITIAL ).
                  gw_data-data    = gw_0055_aux-data_progr + gw_t052-ztag1.
                ELSE.

                  var_mes = ( ( var_mes + gw_0055_aux-data_progr+4(2) ) + gw_t052-zmona ).
                  IF ( var_mes > 12 ).
                    var_mes_aux =  gw_t052-zmona.
                    var_ano = gw_0055_aux-data_progr(4) + 1.
                  ELSE.
                    var_mes_aux = var_mes.
                    var_ano     = gw_zsdt0051-dtde_logist(4).
                  ENDIF.

                  CONCATENATE var_ano var_mes_aux var_data+6(2) INTO var_data_completa.
                  var_data_completa = var_data_completa +  gw_t052-zfael.
                  gw_data-data      = var_data_completa.

                ENDIF.
              WHEN OTHERS.
                gw_data-data    = gw_0055_aux-data_progr.
            ENDCASE.
          ELSE.
            gw_data-data    = gw_0055_aux-valdt_hedge.
          ENDIF.

          APPEND gw_data TO gt_data.
        ELSE.

          IF ( gw_0055_aux-valdt_hedge IS INITIAL ).

            CASE gw_t052-zdart.
              WHEN: 'B'.
                IF NOT ( gw_t052-ztag1 IS INITIAL ).
                  gw_0055_aux-data_progr = gw_0055_aux-data_progr + gw_t052-ztag1.
                ELSE.

                  var_mes = ( ( var_mes + gw_0055_aux-data_progr+4(2) ) + gw_t052-zmona ).
                  IF ( var_mes > 12 ).
                    var_mes_aux =  gw_t052-zmona.
                    var_ano = gw_0055_aux-data_progr(4) + 1.
                  ELSE.
                    var_mes_aux = var_mes.
                    var_ano     = gw_zsdt0051-dtde_logist(4).
                  ENDIF.

                  CONCATENATE var_ano var_mes_aux var_data+6(2) INTO var_data_completa.
                  var_data_completa = var_data_completa +  gw_t052-zfael.
                  gw_0055_aux-data_progr = var_data_completa.

                ENDIF.
*              WHEN OTHERS.
*                GW_DATA-DATA    = GW_ZSDT0055-DATA_PROGR.
            ENDCASE.

*            APPEND GW_DATA TO GT_DATA.

            IF ( gw_0055_aux-data_progr > gw_data-data ).
              gw_data-data    = gw_0055_aux-data_progr.
*             MODIFY GT_DATA FROM GW_DATA INDEX VAR_TABIX TRANSPORTING DATA.
              MODIFY gt_data FROM gw_data TRANSPORTING data WHERE fixacao EQ gw_0055_aux-fixacao.
            ENDIF.
          ELSE.
            IF ( gw_0055_aux-valdt_hedge > gw_data-data ).
              gw_data-data    = gw_0055_aux-valdt_hedge.
*             MODIFY GT_DATA FROM GW_DATA INDEX VAR_TABIX TRANSPORTING DATA.
              MODIFY gt_data FROM gw_data TRANSPORTING data WHERE fixacao EQ gw_0055_aux-fixacao .
            ENDIF.
          ENDIF.
        ENDIF.


        gw_saldo-bezei      = gw_zsdt0059-bezei.
        READ TABLE gt_zsdt0059 TRANSPORTING NO FIELDS WITH KEY posnr = gw_0055_aux-fixacao.
        IF ( sy-subrc NE 0 ).
          CONTINUE.
        ENDIF.
        IF i_tcode EQ 'ZSDT0066'.
          CONTINUE.
        ENDIF.


        IF ( gw_0055_aux-valdt_hedge IS INITIAL ).

          LOOP AT gt_zsdt0095 INTO gw_zsdt0095 WHERE fixacao     = gw_0055_aux-fixacao
                                               AND nro_sol_ov  = gw_0055_aux-nro_sol_ov
                                               "AND VALDT_HEDGE = GW_ZSDT0055-DATA_PROGR.
                                               AND valdt_hedge = gw_data-data.
            gw_0055_aux-cadencia_qte = gw_0055_aux-cadencia_qte - gw_zsdt0095-qtd_hedge.
          ENDLOOP.

          IF i_auart NOT IN r_devo_recu.
            IF i_tcode NE 'ZSDT0066'.

              aux_cadencia = aux_cadencia + gw_0055_aux-cadencia_qte.
              up_cadencia = aux_cadencia.
              IF aux_cadencia > formula2.
                aux_cadencia = aux_cadencia - gw_0055_aux-cadencia_qte.
                gw_0055_aux-cadencia_qte = formula2 - aux_cadencia.
              ENDIF.

            ENDIF.
          ENDIF.

*          CASE I_AUART.
*            WHEN 'ZROB' OR 'ZREB' OR 'ZDMI' OR 'ZROF'.
*            WHEN OTHERS.
*              IF I_TCODE NE 'ZSDT0066'.
*
*                AUX_CADENCIA = AUX_CADENCIA + GW_0055_AUX-CADENCIA_QTE.
*                UP_CADENCIA = AUX_CADENCIA.
*                IF AUX_CADENCIA > FORMULA2.
*                  AUX_CADENCIA = AUX_CADENCIA - GW_0055_AUX-CADENCIA_QTE.
*                  GW_0055_AUX-CADENCIA_QTE = FORMULA2 - AUX_CADENCIA.
*                ENDIF.
*
*              ENDIF.
*          ENDCASE.

        ELSE.

          LOOP AT gt_zsdt0095 INTO gw_zsdt0095 WHERE fixacao     = gw_0055_aux-fixacao
                                                 AND nro_sol_ov  = gw_0055_aux-nro_sol_ov
                                                 "AND VALDT_HEDGE = GW_ZSDT0055-VALDT_HEDGE.
                                                 AND valdt_hedge = gw_data-data.
            gw_0055_aux-cadencia_qte = gw_0055_aux-cadencia_qte - gw_zsdt0095-qtd_hedge.
          ENDLOOP.

          IF i_auart NOT IN r_devo_recu.
            IF i_tcode NE 'ZSDT0066'.

              aux_cadencia = aux_cadencia + gw_0055_aux-cadencia_qte.
              up_cadencia = aux_cadencia.
              IF aux_cadencia > formula2.
                aux_cadencia = aux_cadencia - gw_0055_aux-cadencia_qte.
                gw_0055_aux-cadencia_qte = formula2 - aux_cadencia.
              ENDIF.

            ENDIF.
          ENDIF.

*          CASE I_AUART.
*            WHEN 'ZROB' OR 'ZREB' OR 'ZDMI' OR 'ZROF'.
*            WHEN OTHERS.
*              IF I_TCODE NE 'ZSDT0066'.
*
*                AUX_CADENCIA = AUX_CADENCIA + GW_0055_AUX-CADENCIA_QTE.
*                UP_CADENCIA = AUX_CADENCIA.
*                IF AUX_CADENCIA > FORMULA2.
*                  AUX_CADENCIA = AUX_CADENCIA - GW_0055_AUX-CADENCIA_QTE.
*                  GW_0055_AUX-CADENCIA_QTE = FORMULA2 - AUX_CADENCIA.
*                ENDIF.
*
*              ENDIF.
*          ENDCASE.

        ENDIF.
        IF ( gw_0055_aux-cadencia_qte EQ 0 ).
          MODIFY gt_0055_aux INDEX var_tabix FROM gw_0055_aux TRANSPORTING cadencia_qte.
          CONTINUE.
        ENDIF.

        gw_saldo-nro_sol_ov = gw_0055_aux-nro_sol_ov.
        gw_saldo-fixacao    = gw_0055_aux-fixacao.
        gw_saldo-valor      = gw_0055_aux-cadencia_qte.
        gw_saldo-saldo      = gw_0055_aux-cadencia_qte.

        IF ( gw_0055_aux-valdt_hedge IS INITIAL ).
          gw_saldo-data    = gw_data-data.
        ELSE.
          gw_saldo-data       = gw_0055_aux-valdt_hedge.
        ENDIF.

        IF up_cadencia > formula2.
          gw_0055_aux-cadencia_qte = up_cadencia  -  formula2.
          MODIFY gt_0055_aux INDEX var_tabix FROM gw_0055_aux TRANSPORTING cadencia_qte.
        ELSE.
          gw_0055_aux-cadencia_qte = 0.
          MODIFY gt_0055_aux INDEX var_tabix FROM gw_0055_aux TRANSPORTING cadencia_qte.
        ENDIF.

        APPEND gw_saldo TO gt_saldo.
        soma = soma + gw_saldo-saldo.

        CLEAR: gw_0055_aux, gw_saldo.

        CLEAR: gw_data, var_tabix.
      ENDLOOP.

      DELETE gt_0055_aux  WHERE cadencia_qte EQ 0.
      CLEAR: soma, aux_cadencia.

    ENDLOOP.

*  BREAK ABAP.

    LOOP AT gt_saldo INTO gw_saldo.

      gw_saldo_s-nro_sol_ov = gw_saldo-nro_sol_ov.
      gw_saldo_s-fixacao = gw_saldo-fixacao.
      gw_saldo_s-data = gw_saldo-data.
      gw_saldo_s-bezei = gw_saldo-bezei.
      gw_saldo_s-saldo = gw_saldo-saldo.
      gw_saldo_s-valor = gw_saldo-valor.

      COLLECT gw_saldo_s INTO gt_saldo_s.
    ENDLOOP.

    MOVE gt_saldo_s TO gt_saldo.

    CASE i_tcode.
      WHEN: 'VF01'.
        "metodo para mudar o status quando é feito uma devolução.
        me->status_devolucao( i_numero  = i_numero
                              i_estorno = CONV #( i_vbeln )
                              i_auart   = i_auart ).
    ENDCASE.

    DATA: lines_saldo TYPE sy-tabix.
    CLEAR: var_verifica_frete.
    MOVE gt_saldo TO gt_saldo_aux.

    LOOP AT gt_zsdt0059 INTO gw_zsdt0059.

      READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0059-nro_sol_ov.

      IF gw_zsdt0051-waerk NE 'BRL'.
        CONTINUE.
      ENDIF.

      READ TABLE gt_zsdt0052 INTO gw_zsdt0052 WITH KEY nro_sol_ov = gw_zsdt0051-nro_sol_ov.
      READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY nro_sol_ov = gw_zsdt0051-nro_sol_ov
                                                       posnr      = gw_zsdt0059-posnr1
                                                       fixacao    = gw_zsdt0059-posnr.

      READ TABLE gt_zsdt0055 INTO gw_zsdt0055 WITH KEY nro_sol_ov = gw_zsdt0051-nro_sol_ov
                                                       fixacao    = gw_zsdt0059-posnr.

      READ TABLE gt_zsdt0059_cambio INTO gw_zsdt0059_cambio WITH KEY posnr = gw_zsdt0059-posnr
                                                                     bezei = gw_zsdt0059-bezei.

      MOVE gt_saldo_aux TO gt_saldo.
      DELETE gt_saldo WHERE bezei NE gw_zsdt0059-bezei.
      DELETE gt_saldo WHERE fixacao NE gw_zsdt0059-posnr.


      IF NOT ( gw_zsdt0055-valdt_hedge IS INITIAL ).

        READ TABLE gt_saldo INTO gw_saldo WITH KEY fixacao = gw_zsdt0059-posnr.
        var_tabix = sy-tabix.

        lines_saldo = lines( gt_saldo ).

        WHILE lines_saldo >= var_tabix.

*            VAR_TABIX = VAR_TABIX + 1.

          IF ( gw_saldo-saldo > 0 ).

            IF var_tabix EQ 1.
              saldo_aux2 = gw_saldo-saldo - gw_zsdt0059-formula2.
            ENDIF.

            IF ( saldo_aux2 < 0 ).

              saldo_aux = saldo_aux2 * -1.
              gw_zsdt0059-formula2 = gw_saldo-saldo.
              gw_zsdt0055-valdt_hedge =  gw_saldo-data.

              "Inserir na ZSDT0095 E STARTAR A COTAÇÃO DO HEDGE
              IF i_auart NOT IN r_auart.
                me->zif_taxa_curva_db~inserir_frame( i_zsdt0059 = gw_zsdt0059
                                                     i_zsdt0055 = gw_zsdt0055
                                                     i_data     = gw_data-data
                                                     i_auart    = i_auart ).
              ENDIF.
*              CASE I_AUART.
*                WHEN 'ZCPV' OR 'ZCOP'
*                  OR 'ZROB' OR 'ZREB'
*                  OR 'ZDMI' OR 'ZCFX'
*                  OR 'ZROF'.
*                WHEN OTHERS.
*                  ME->ZIF_TAXA_CURVA_DB~INSERIR_FRAME( I_ZSDT0059 = GW_ZSDT0059
*                                                       I_ZSDT0055 = GW_ZSDT0055
*                                                       I_DATA     = GW_DATA-DATA
*                                                       I_AUART    = I_AUART ).
*              ENDCASE.

              gw_saldo-saldo = 0.
              MODIFY gt_saldo INDEX var_tabix FROM gw_saldo TRANSPORTING saldo.

              FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
              CREATE OBJECT gobj_zcl_taxa_curva.
              CREATE OBJECT gobj_zcl_webservice_tx_curva.

              gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
              gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
              data_venc = gobj_zcl_taxa_curva->set_data_venc(  gw_saldo-data ).

              IF i_auart IN r_auart.

                " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
*                LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
*                  var_cade_soma = var_cade_soma + gw_zsdt0094-cadencia_qte.
*                ENDLOOP.

                CALL METHOD calc_cadencia_94
                  EXPORTING
                    it_0094     = gt_zsdt0094
                    iv_matnr    = gw_zsdt0051-matnr
                    iv_doc_simu = gw_zsdt0059-nro_sol_ov
                  CHANGING
                    cv_cadencia = var_cade_soma.
                " 09.09.2023 - bug 150168 - quevedo
                var_cadencia = gw_zsdt0059-formula2 - var_cade_soma.
              ELSE.
                var_cadencia = gw_zsdt0059-formula2.
              ENDIF.




*              CASE I_AUART.
*                WHEN 'ZCPV' OR 'ZCOP'
*                  OR 'ZROB' OR 'ZREB'
*                  OR 'ZDMI' OR 'ZCFX'
*                  OR 'ZROF'.
*
*                  LOOP AT GT_ZSDT0094 INTO GW_ZSDT0094.
*                    VAR_CADE_SOMA = VAR_CADE_SOMA + GW_ZSDT0094-CADENCIA_QTE.
*                  ENDLOOP.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2 - VAR_CADE_SOMA.
*
*                WHEN OTHERS.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
*              ENDCASE.

              gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
              var_total_proporcional = ( ( gw_zsdt0053-vlrtot / gw_zsdt0053-zmeng ) * var_cadencia ).

              IF var_total_proporcional EQ 0.

                IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                  var_total_proporcional = gw_zsdt0053-dmbtr * var_cadencia.
                ELSE.
                  IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                  ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).

                    "  22.08.2023 ------>
                  ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).

                  ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                    "  22.08.2023 ------<

                  ENDIF.
                ENDIF.

              ENDIF.

              gobj_zcl_taxa_curva->set_total_proporcional( CONV #( var_total_proporcional ) ).
              gobj_zcl_taxa_curva->set_posnr( gw_zsdt0059-posnr ).
              gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059-bezei ).
              var_taxa_cambio = gw_zsdt0059_cambio-formula2.
              gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
              gobj_zcl_taxa_curva->set_tipo( c_venda ).

              " 31.08.2023 - Ramon -->
              CALL METHOD get_vlr_bio
                EXPORTING
                  iv_matnr    = gw_zsdt0051-matnr
                  iv_doc_simu = gw_zsdt0053-nro_sol_ov
                CHANGING
                  cv_cadencia = var_cadencia.
              " 31.08.2023 - Ramon -->

              gobj_zcl_taxa_curva->set_cadencia( i_cadencia = var_cadencia
                                                 i_tcode    = i_tcode
                                                 ).

              gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
              var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).


              IF ( gw_saldo-data < sy-datum ).
                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = sy-datum
                                                                         i_tipo = var_tipo
                                                                         ).
              ELSE.
                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = gw_saldo-data
                                                                         i_tipo = var_tipo
                                                                         ).
              ENDIF.

              IF ( gw_zsdt0059-valdt < sy-datum ).
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
              ELSE.
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
              ENDIF.

              gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).

              gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

              var_safra = gw_zsdt0053-charg.
              gobj_zcl_taxa_curva->set_safra( var_safra ).
              me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ).

              " 17.07.2024 - RAMON - 144484 -->
              " esse codigo foi inserido para nao deixar gerar hedge vazio,
              " foi feito aqui pq nenhum processo deve gerar hedge sem valor
              IF gobj_zcl_taxa_curva->get_total_proporcional( ) NE 0." ramon 17.07.2024
                me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva ).
              ENDIF.
              " 17.07.2024 - RAMON - 144484 --<

*              me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva ). " 17.07.2024 - RAMON - 144484 --<COMENTADO


              var_tabix = var_tabix + 1.
              READ TABLE gt_saldo INTO gw_saldo INDEX var_tabix.
              READ TABLE gt_zsdt0055 INTO gw_zsdt0055 WITH KEY nro_sol_ov = gw_zsdt0051-nro_sol_ov
                                                               fixacao    = gw_zsdt0059-posnr.

            ELSE.

              saldo_aux = gw_saldo-saldo.
              gw_saldo-saldo = saldo_aux2.

              MODIFY gt_saldo INDEX var_tabix FROM gw_saldo TRANSPORTING saldo.
              gw_zsdt0055-valdt_hedge =  gw_saldo-data.

              "Inserir na ZSDT0095 E STARTAR A COTAÇÃO DO HEDGE
              IF i_auart NOT IN r_auart.
                me->zif_taxa_curva_db~inserir_frame( i_zsdt0059 = gw_zsdt0059
                                                     i_zsdt0055 = gw_zsdt0055
                                                     i_data     = gw_data-data
                                                     i_auart    = i_auart ).
              ENDIF.

*              CASE I_AUART.
*                WHEN 'ZCPV' OR 'ZCOP'
*                  OR 'ZROB' OR 'ZREB'
*                  OR 'ZDMI' OR 'ZCFX'
*                  OR 'ZROF'.
*
*                WHEN OTHERS.
*                  ME->ZIF_TAXA_CURVA_DB~INSERIR_FRAME( I_ZSDT0059 = GW_ZSDT0059
*                                                       I_ZSDT0055 = GW_ZSDT0055
*                                                       I_DATA     = GW_DATA-DATA
*                                                       I_AUART    = I_AUART ).
*              ENDCASE.


              IF ( saldo_aux2 EQ 0 ).
                gw_zsdt0059-formula2 = saldo_aux.
              ENDIF.

              FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
              CREATE OBJECT gobj_zcl_taxa_curva.
              CREATE OBJECT gobj_zcl_webservice_tx_curva.

              gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
              gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
              gobj_zcl_taxa_curva->set_data_venc(  gw_saldo-data ).

              IF i_auart IN r_auart.

                " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
*                LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
*                  var_cade_soma = var_cade_soma + gw_zsdt0094-cadencia_qte.
*                ENDLOOP.

                CALL METHOD calc_cadencia_94
                  EXPORTING
                    it_0094     = gt_zsdt0094
                    iv_matnr    = gw_zsdt0051-matnr
                    iv_doc_simu = gw_zsdt0059-nro_sol_ov
                  CHANGING
                    cv_cadencia = var_cade_soma.
                " 09.09.2023 - bug 150168 - quevedo
                var_cadencia = gw_zsdt0059-formula2 - var_cade_soma.
              ELSE.
                var_cadencia = gw_zsdt0059-formula2.
              ENDIF.

*              CASE I_AUART.
*                WHEN 'ZCPV' OR 'ZCOP'
*                  OR 'ZROB' OR 'ZREB'
*                  OR 'ZDMI' OR 'ZCFX'
*                  OR 'ZROF'.
*
*                  LOOP AT GT_ZSDT0094 INTO GW_ZSDT0094.
*                    VAR_CADE_SOMA = VAR_CADE_SOMA + GW_ZSDT0094-CADENCIA_QTE.
*                  ENDLOOP.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2 - VAR_CADE_SOMA.
*
*                WHEN OTHERS.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
*              ENDCASE.

              gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
              var_total_proporcional = ( ( gw_zsdt0053-vlrtot / gw_zsdt0053-zmeng ) * var_cadencia ).

              IF var_total_proporcional EQ 0.

                IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                  var_total_proporcional = gw_zsdt0053-dmbtr * var_cadencia.
                ELSE.
                  IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                  ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).

                    "  22.08.2023 ------>
                  ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).

                  ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                    "  22.08.2023 ------<


                  ENDIF.
                ENDIF.

              ENDIF.

              gobj_zcl_taxa_curva->set_total_proporcional( var_total_proporcional ).
              gobj_zcl_taxa_curva->set_posnr( gw_zsdt0059-posnr ).
              gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059-bezei ).
              var_taxa_cambio = gw_zsdt0059_cambio-formula2.
              gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
              gobj_zcl_taxa_curva->set_tipo( c_venda ).

              " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
              CALL METHOD get_vlr_bio
                EXPORTING
                  iv_matnr    = gw_zsdt0051-matnr
                  iv_doc_simu = gw_zsdt0053-nro_sol_ov
                CHANGING
                  cv_cadencia = var_cadencia.
              " 09.09.2023 - bug 150168 - quevedo -->


              gobj_zcl_taxa_curva->set_cadencia( i_cadencia = var_cadencia
                                                 i_tcode    = i_tcode
                                                 ).

              gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
              var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).


              IF ( gw_saldo-data < sy-datum ).
                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = sy-datum
                                                                         i_tipo = var_tipo
                                                                         ).
              ELSE.
                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = gw_saldo-data
                                                                         i_tipo = var_tipo
                                                                         ).
              ENDIF.

              IF ( gw_zsdt0059-valdt < sy-datum ).
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
              ELSE.
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
              ENDIF.


              gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
              gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

              var_safra = gw_zsdt0053-charg.
              gobj_zcl_taxa_curva->set_safra( var_safra ).
              me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ).

              " 17.07.2024 - RAMON - 144484 -->
              " esse codigo foi inserido para nao deixar gerar hedge vazio,
              " foi feito aqui pq nenhum processo deve gerar hedge sem valor
              IF gobj_zcl_taxa_curva->get_total_proporcional( ) NE 0." ramon 17.07.2024
                me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva ).
              ENDIF.
              " 17.07.2024 - RAMON - 144484 --<

*              me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva )." 17.07.2024 - RAMON - 144484 --> COMENTADO

*                DELETE GT_SALDO WHERE SALDO < 1.
              var_tabix = var_tabix + 1.
*              CONTINUE.

            ENDIF.

          ELSE.

            FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
            CREATE OBJECT gobj_zcl_taxa_curva.
            CREATE OBJECT gobj_zcl_webservice_tx_curva.

            READ TABLE gt_data INTO gw_data WITH KEY fixacao = gw_zsdt0059-posnr.
            IF ( gw_data-data < sy-datum ).
              gw_data-data = gw_data-data + 30.
            ENDIF.

            "Inserir na ZSDT0095 E STARTAR A COTAÇÃO DO HEDGE
            IF i_auart NOT IN r_auart.
              me->zif_taxa_curva_db~inserir_frame( i_zsdt0059 = gw_zsdt0059
                                                   i_zsdt0055 = gw_zsdt0055
                                                   i_data     = gw_data-data
                                                   i_auart    = i_auart ).
            ENDIF.

*            CASE I_AUART.
*              WHEN 'ZCPV' OR 'ZCOP'
*                OR 'ZROB' OR 'ZREB'
*                OR 'ZDMI' OR 'ZCFX'
*                OR 'ZROF'.
*              WHEN OTHERS.
*                ME->ZIF_TAXA_CURVA_DB~INSERIR_FRAME( I_ZSDT0059 = GW_ZSDT0059
*                                                     I_ZSDT0055 = GW_ZSDT0055
*                                                     I_DATA     = GW_DATA-DATA
*                                                     I_AUART    = I_AUART ).
*            ENDCASE.

            gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
            gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
            data_venc = gobj_zcl_taxa_curva->set_data_venc(  gw_data-data ).

            IF i_auart IN r_auart.

              " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
*              LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
*                var_cade_soma = var_cade_soma + gw_zsdt0094-cadencia_qte.
*              ENDLOOP.
              CALL METHOD calc_cadencia_94
                EXPORTING
                  it_0094     = gt_zsdt0094
                  iv_matnr    = gw_zsdt0051-matnr
                  iv_doc_simu = gw_zsdt0059-nro_sol_ov
                CHANGING
                  cv_cadencia = var_cade_soma.
              " 09.09.2023 - bug 150168 - quevedo -->
              var_cadencia = gw_zsdt0059-formula2 - var_cade_soma.
            ELSE.
              var_cadencia = gw_zsdt0059-formula2.
            ENDIF.

*            CASE I_AUART.
*              WHEN 'ZCPV' OR 'ZCOP'
*                OR 'ZROB' OR 'ZREB'
*                OR 'ZDMI' OR 'ZCFX'
*                OR 'ZROF'.
*                LOOP AT GT_ZSDT0094 INTO GW_ZSDT0094.
*                  VAR_CADE_SOMA = VAR_CADE_SOMA + GW_ZSDT0094-CADENCIA_QTE.
*                ENDLOOP.
*                VAR_CADENCIA = GW_ZSDT0059-FORMULA2 - VAR_CADE_SOMA.
*              WHEN OTHERS.
*                VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
*            ENDCASE.

*            gobj_zcl_taxa_curva->set_cadencia( var_cadencia ). " 09.09.2023 - bug 150168 - quevedo --> mais abaixo está fazendo o calculo novamente
            gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
            var_total_proporcional = ( ( gw_zsdt0053-vlrtot / gw_zsdt0053-zmeng ) * var_cadencia ).

            IF var_total_proporcional EQ 0.

              IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                var_total_proporcional = gw_zsdt0053-dmbtr * var_cadencia.
              ELSE.
                IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).

                  "  22.08.2023 ------>
                ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                  "  22.08.2023 ------<
                ENDIF.
              ENDIF.
            ENDIF.

            gobj_zcl_taxa_curva->set_total_proporcional( var_total_proporcional ).
            gobj_zcl_taxa_curva->set_posnr( gw_zsdt0059-posnr ).
            gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059-bezei ).
            var_taxa_cambio = gw_zsdt0059_cambio-formula2.
            gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
            gobj_zcl_taxa_curva->set_tipo( c_venda ).

            " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
            CALL METHOD get_vlr_bio
              EXPORTING
                iv_matnr    = gw_zsdt0051-matnr
                iv_doc_simu = gw_zsdt0053-nro_sol_ov
              CHANGING
                cv_cadencia = var_cadencia.
            " 09.09.2023 - bug 150168 - quevedo -->

            gobj_zcl_taxa_curva->set_cadencia( i_cadencia = var_cadencia
                                               i_tcode    = i_tcode
                                               ).
            IF i_auart IN r_comp.
              var_tipo = 'C'.
              gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).
            ELSE.
              gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
              var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
            ENDIF.

*            CASE I_AUART.
*              WHEN 'ZCPV' OR 'ZCOP' OR 'ZCFX'.
*                VAR_TIPO = 'C'.
*                GOBJ_ZCL_TAXA_CURVA->SET_TIPO_TAXA( VAR_TIPO ).
*              WHEN OTHERS.
*                GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
*                VAR_TIPO =  GOBJ_ZCL_TAXA_CURVA->GET_TIPO_TAXA( ).
*            ENDCASE.

*            IF ( GW_SALDO-DATA < SY-DATUM ).
            IF ( data_venc < sy-datum ).
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = sy-datum
                                                                       i_tipo = var_tipo
                                                                       ).
            ELSE.
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = data_venc ). "GW_SALDO-DATA ).
            ENDIF.

            IF ( gw_zsdt0059-valdt < sy-datum ).
              gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
            ELSE.
              gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
            ENDIF.

            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
            gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

            var_safra = gw_zsdt0053-charg.
            gobj_zcl_taxa_curva->set_safra( var_safra ).
            me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ).

            " 17.07.2024 - RAMON - 144484 -->
            " esse codigo foi inserido para nao deixar gerar hedge vazio,
            " foi feito aqui pq nenhum processo deve gerar hedge sem valor
            IF gobj_zcl_taxa_curva->get_total_proporcional( ) NE 0." ramon 17.07.2024
              me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva ).
            ENDIF.
            " 17.07.2024 - RAMON - 144484 --<

*            me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva )." 17.07.2024 - RAMON - 144484 --> COMENTADO


*              DELETE GT_SALDO WHERE SALDO < 1.
            var_tabix = var_tabix + 1.
*              CONTINUE.

          ENDIF.
        ENDWHILE.

      ELSEIF NOT ( gw_zsdt0052-valdt IS INITIAL ) OR NOT ( gw_zsdt0053-valdt IS INITIAL  ).

        FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
        CREATE OBJECT gobj_zcl_taxa_curva.
        CREATE OBJECT gobj_zcl_webservice_tx_curva.

        gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
        gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).


        "Inserir na ZSDT0095 E STARTAR A COTAÇÃO DO HEDGE
        IF i_auart NOT IN r_auart.
          me->zif_taxa_curva_db~inserir_frame( i_zsdt0059 = gw_zsdt0059
                                               i_zsdt0055 = gw_zsdt0055
                                               i_data     = gw_data-data
                                               i_auart    = i_auart ).
        ENDIF.

*        CASE I_AUART.
*          WHEN 'ZCPV' OR 'ZCOP'
*            OR 'ZROB' OR 'ZREB'
*            OR 'ZDMI' OR 'ZCFX'
*            OR 'ZROF'.
*          WHEN OTHERS.
*            ME->ZIF_TAXA_CURVA_DB~INSERIR_FRAME( I_ZSDT0059 = GW_ZSDT0059
*                                                 I_ZSDT0055 = GW_ZSDT0055
*                                                 I_DATA     = GW_DATA-DATA
*                                                 I_AUART    = I_AUART ).
*
*        ENDCASE.

*          VAR_CADENCIA = GW_ZSDT0059-FORMULA2. "JA ESTAVA AQUI COMENTADO

        IF i_auart IN r_auart.
          " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
*          LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
*            var_cade_soma = var_cade_soma + gw_zsdt0094-cadencia_qte.
*          ENDLOOP.
          CALL METHOD calc_cadencia_94
            EXPORTING
              it_0094     = gt_zsdt0094
              iv_matnr    = gw_zsdt0051-matnr
              iv_doc_simu = gw_zsdt0059-nro_sol_ov
            CHANGING
              cv_cadencia = var_cade_soma.
          " 09.09.2023 - bug 150168 - quevedo
          var_cadencia = gw_zsdt0059-formula2 - var_cade_soma.
        ELSE.
          var_cadencia = gw_zsdt0059-formula2.
        ENDIF.

*        CASE I_AUART.
*          WHEN 'ZCPV' OR 'ZCOP'
*            OR 'ZROB' OR 'ZREB'
*            OR 'ZDMI' OR 'ZCFX'
*            OR 'ZROF'.
*            LOOP AT GT_ZSDT0094 INTO GW_ZSDT0094.
*              VAR_CADE_SOMA = VAR_CADE_SOMA + GW_ZSDT0094-CADENCIA_QTE.
*            ENDLOOP.
*            VAR_CADENCIA = GW_ZSDT0059-FORMULA2 - VAR_CADE_SOMA.
*          WHEN OTHERS.
*            VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
*        ENDCASE.

        "GOBJ_ZCL_TAXA_CURVA->SET_CADENCIA( VAR_CADENCIA ).

*        select single matkl from mara
*          into @DATA(lv_matkl)
*            where matnr = @gw_zsdt0051-matnr.
*
*        " quando for bio
*        IF lv_matkl = '700400'.
*
*          READ TABLE gt_zsdt0059_conv_bio INTO DATA(ls_bio)
*           WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
*
*          IF sy-subrc EQ 0.
*
*            LOOP AT gt_zsdt0053 ASSIGNING FIELD-SYMBOL(<fs_0053>).
*              var_cadencia = var_cadencia * ls_bio-formula2.
*            ENDLOOP.
*
*            LOOP AT gt_zsdt0055 ASSIGNING FIELD-SYMBOL(<fs_0055>).
*              <fs_0055>-cadencia_qte = <fs_0055>-cadencia_qte * ls_bio-formula2.
*            ENDLOOP.
*
*            gw_zsdt0055-cadencia_qte = gw_zsdt0055-cadencia_qte * ls_bio-formula2.
*            gw_zsdt0053-zmeng = gw_zsdt0053-zmeng * ls_bio-formula2.
*
*            "me->at_total_55 = me->at_total_55 * ls_bio-formula2.
*            "me->at_total_peso_53 = me->at_total_peso_53 * ls_bio-formula2.
*
*          ENDIF.
*
*        ENDIF.
        " 22.08.2023 - Ramon --<

        gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
        var_total_proporcional = ( ( gw_zsdt0053-vlrtot / gw_zsdt0053-zmeng ) * var_cadencia ).

        IF var_total_proporcional EQ 0.

          IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
            var_total_proporcional = gw_zsdt0053-dmbtr * var_cadencia.
          ELSE.
            IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
              var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
            ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
              var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
              "  22.08.2023 ------>
            ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
              var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
            ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
              var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
              "  22.08.2023 ------<
            ENDIF.
          ENDIF.

        ENDIF.

        gobj_zcl_taxa_curva->set_total_proporcional( var_total_proporcional ).
        gobj_zcl_taxa_curva->set_posnr( gw_zsdt0059-posnr ).
        gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059-bezei ).
        var_taxa_cambio = gw_zsdt0059_cambio-formula2.
        gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
        gobj_zcl_taxa_curva->set_tipo( c_venda ).

        " 31.08.2023 - Ramon -->
        CALL METHOD get_vlr_bio
          EXPORTING
            iv_matnr    = gw_zsdt0051-matnr
            iv_doc_simu = gw_zsdt0053-nro_sol_ov
          CHANGING
            cv_cadencia = var_cadencia.
        " 31.08.2023 - Ramon -->

        gobj_zcl_taxa_curva->set_cadencia( i_cadencia = var_cadencia
                                           i_tcode    = i_tcode ).

        gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
        var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).


        IF NOT ( gw_zsdt0052-valdt IS INITIAL ).
          data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0052-valdt ).
          TRY.

              IF ( gw_zsdt0059-valdt < sy-datum ).
                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc " GW_ZSDT0052-VALDT
                                                                         i_data_lib = sy-datum
                                                                         i_tipo     = var_tipo
                                                                         ).

              ELSE.
                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc " GW_ZSDT0052-VALDT
                                                                         i_data_lib = gw_zsdt0059-valdt
                                                                         i_tipo     = var_tipo
                                                                         ).
              ENDIF.

            CATCH zcx_webservice INTO cx_exception.
              var_msg  = cx_exception->get_text( ).
              MESSAGE e007(zwebservice) WITH var_msg.
          ENDTRY.

        ELSE.
          data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0053-valdt ).
          TRY.

              IF ( gw_zsdt0059-valdt  < sy-datum ).
                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc " GW_ZSDT0053-VALDT
                                                                         i_data_lib = sy-datum
                                                                         i_tipo     = var_tipo
                                                                         ).
              ELSE.

                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc " GW_ZSDT0053-VALDT
                                                                         i_data_lib = gw_zsdt0059-valdt
                                                                         i_tipo     = var_tipo
                                                                         ).

              ENDIF.
            CATCH zcx_webservice INTO cx_exception.
              var_msg  = cx_exception->get_text( ).
              MESSAGE e007(zwebservice) WITH var_msg.
          ENDTRY.

        ENDIF.

        IF ( gw_zsdt0059-valdt < sy-datum ).
          gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
        ELSE.
          gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
        ENDIF.


        gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
        gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

        var_safra = gw_zsdt0053-charg.
        gobj_zcl_taxa_curva->set_safra( var_safra ).
        me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ).

        " 17.07.2024 - RAMON - 144484 -->
        " esse codigo foi inserido para nao deixar gerar hedge vazio,
        " foi feito aqui pq nenhum processo deve gerar hedge sem valor
        IF gobj_zcl_taxa_curva->get_total_proporcional( ) NE 0." ramon 17.07.2024
          me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva ).
        ENDIF.
        " 17.07.2024 - RAMON - 144484 --<

      ELSEIF NOT ( gw_zsdt0055-data_progr IS INITIAL ).

        READ TABLE gt_saldo INTO gw_saldo WITH KEY fixacao = gw_zsdt0059-posnr.
        var_tabix = sy-tabix.
        lines_saldo = lines( gt_saldo ).
        WHILE lines_saldo >= var_tabix.

          IF ( gw_saldo-saldo > 0 ).

            IF var_tabix EQ 1.
              saldo_aux2 = gw_saldo-saldo - gw_zsdt0059-formula2.
            ENDIF.

            IF ( saldo_aux2 < 0 ).

              " 07.03.2024 - RAMON IR173219 -->
              lv_formula2_bkp = gw_zsdt0059-formula2.
              " 07.03.2024 - RAMON IR173219 --<

              saldo_aux = saldo_aux2 * -1.
              gw_zsdt0059-formula2 = gw_saldo-saldo.
              gw_zsdt0055-valdt_hedge =  gw_saldo-data.

              "Inserir na ZSDT0095 E STARTAR A COTAÇÃO DO HEDGE
              IF i_auart NOT IN r_auart .
                me->zif_taxa_curva_db~inserir_frame( i_zsdt0059 = gw_zsdt0059
                                                     i_zsdt0055 = gw_zsdt0055
                                                     i_data     = gw_data-data
                                                     i_auart    = i_auart ).
              ENDIF.

              " 07.03.2024 - RAMON IR173219 -->
              gw_zsdt0059-formula2 = lv_formula2_bkp.
              " fizemos esse desenvolvimento, pq o valor de formula2 estava sendo sobregravado,
              " ocasionando erro no hedge ordem de complemento
              " 07.03.2024 - RAMON IR173219 --<

*              CASE I_AUART.
*                WHEN 'ZCPV' OR 'ZCOP'
*                  OR 'ZROB' OR 'ZREB'
*                  OR 'ZDMI' OR 'ZCFX'
*                  OR 'ZROF'.
*                WHEN OTHERS.
*                  ME->ZIF_TAXA_CURVA_DB~INSERIR_FRAME( I_ZSDT0059 = GW_ZSDT0059
*                                                       I_ZSDT0055 = GW_ZSDT0055
*                                                       I_DATA     = GW_DATA-DATA
*                                                       I_AUART    = I_AUART ).
*
*              ENDCASE.

              gw_saldo-saldo = 0.
              MODIFY gt_saldo INDEX var_tabix FROM gw_saldo TRANSPORTING saldo.

              FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
              CREATE OBJECT gobj_zcl_taxa_curva.
              CREATE OBJECT gobj_zcl_webservice_tx_curva.

              gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
              gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
              gobj_zcl_taxa_curva->set_data_venc(  gw_saldo-data ).

*              VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
              IF i_auart IN r_auart.
                " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
*              LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
*                var_cade_soma = var_cade_soma + gw_zsdt0094-cadencia_qte.
*              ENDLOOP.

                CALL METHOD calc_cadencia_94
                  EXPORTING
                    it_0094     = gt_zsdt0094
                    iv_matnr    = gw_zsdt0051-matnr
                    iv_doc_simu = gw_zsdt0059-nro_sol_ov
                  CHANGING
                    cv_cadencia = var_cade_soma.
                " 09.09.2023 - bug 150168 - quevedo
                var_cadencia = gw_zsdt0059-formula2 - var_cade_soma.
              ELSE.
                var_cadencia = gw_zsdt0059-formula2.
              ENDIF.

*              CASE I_AUART.
*                WHEN 'ZCPV' OR 'ZCOP'
*                  OR 'ZROB' OR 'ZREB'
*                  OR 'ZDMI' OR 'ZCFX'
*                  OR 'ZROF'.
*                  LOOP AT GT_ZSDT0094 INTO GW_ZSDT0094.
*                    VAR_CADE_SOMA = VAR_CADE_SOMA + GW_ZSDT0094-CADENCIA_QTE.
*                  ENDLOOP.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2 - VAR_CADE_SOMA.
*                WHEN OTHERS.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
*              ENDCASE.

              "GOBJ_ZCL_TAXA_CURVA->SET_CADENCIA( VAR_CADENCIA ).
              gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
              var_total_proporcional = ( ( gw_zsdt0053-vlrtot / gw_zsdt0053-zmeng ) * var_cadencia ).

              IF var_total_proporcional EQ 0.

                IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                  var_total_proporcional = gw_zsdt0053-dmbtr * var_cadencia.
                ELSE.
                  IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                  ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                    "  22.08.2023 ------>
                  ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                  ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                    "  22.08.2023 ------<
                  ENDIF.
                ENDIF.
              ENDIF.

              gobj_zcl_taxa_curva->set_total_proporcional( var_total_proporcional ).
              gobj_zcl_taxa_curva->set_posnr( gw_zsdt0059-posnr ).
              gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059-bezei ).
              var_taxa_cambio = gw_zsdt0059_cambio-formula2.
              gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
              gobj_zcl_taxa_curva->set_tipo( c_venda ).

              " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
              CALL METHOD get_vlr_bio
                EXPORTING
                  iv_matnr    = gw_zsdt0051-matnr
                  iv_doc_simu = gw_zsdt0053-nro_sol_ov
                CHANGING
                  cv_cadencia = var_cadencia.
              " 09.09.2023 - bug 150168 - quevedo -->

              gobj_zcl_taxa_curva->set_cadencia( i_cadencia = var_cadencia
                                                 i_tcode    = i_tcode
                                                 ).

              gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
              var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).


              IF ( gw_saldo-data < sy-datum ).
                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = sy-datum
                                                                         i_tipo = var_tipo
                                                                         ).
              ELSE.
                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = gw_saldo-data
                                                                         i_tipo = var_tipo
                                                                         ).
              ENDIF.

              IF ( gw_zsdt0059-valdt < sy-datum ).
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
              ELSE.
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
              ENDIF.

              gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
              gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

              var_safra = gw_zsdt0053-charg.
              gobj_zcl_taxa_curva->set_safra( var_safra ).
              me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ).

              " 17.07.2024 - RAMON - 144484 -->
              " esse codigo foi inserido para nao deixar gerar hedge vazio,
              " foi feito aqui pq nenhum processo deve gerar hedge sem valor
              IF gobj_zcl_taxa_curva->get_total_proporcional( ) NE 0." ramon 17.07.2024
                me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva ).
              ENDIF.
              " 17.07.2024 - RAMON - 144484 --<

*              me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva )." 17.07.2024 - RAMON - 144484 --> COMENTADO

              var_tabix = var_tabix + 1.
              READ TABLE gt_saldo INTO gw_saldo INDEX var_tabix.

            ELSE.

              saldo_aux = gw_saldo-saldo.
              gw_saldo-saldo = saldo_aux2.

              MODIFY gt_saldo INDEX var_tabix FROM gw_saldo TRANSPORTING saldo.
              gw_zsdt0055-valdt_hedge =  gw_saldo-data.

              "Inserir na ZSDT0095 E STARTAR A COTAÇÃO DO HEDGE
              IF i_auart NOT IN r_auart.
                me->zif_taxa_curva_db~inserir_frame( i_zsdt0059 = gw_zsdt0059
                                                     i_zsdt0055 = gw_zsdt0055
                                                     i_data     = gw_data-data
                                                     i_auart    = i_auart ).

              ENDIF.

*              CASE I_AUART.
*                WHEN 'ZCPV' OR 'ZCOP'
*                  OR 'ZROB' OR 'ZREB'
*                  OR 'ZDMI' OR 'ZCFX'
*                  OR 'ZROF'.
*                WHEN OTHERS.
*                  ME->ZIF_TAXA_CURVA_DB~INSERIR_FRAME( I_ZSDT0059 = GW_ZSDT0059
*                                                       I_ZSDT0055 = GW_ZSDT0055
*                                                       I_DATA     = GW_DATA-DATA
*                                                       I_AUART    = I_AUART ).
*
*              ENDCASE.


              IF ( saldo_aux2 EQ 0 ).
                gw_zsdt0059-formula2 = saldo_aux.
              ENDIF.

              FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
              CREATE OBJECT gobj_zcl_taxa_curva.
              CREATE OBJECT gobj_zcl_webservice_tx_curva.

              gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
              gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
              gobj_zcl_taxa_curva->set_data_venc(  gw_saldo-data ).

*              VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
              IF i_auart IN r_auart.
                " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
*                LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
*                  var_cade_soma = var_cade_soma + gw_zsdt0094-cadencia_qte.
*                ENDLOOP.

                CALL METHOD calc_cadencia_94
                  EXPORTING
                    it_0094     = gt_zsdt0094
                    iv_matnr    = gw_zsdt0051-matnr
                    iv_doc_simu = gw_zsdt0059-nro_sol_ov
                  CHANGING
                    cv_cadencia = var_cade_soma.
                " 09.09.2023 - bug 150168 - quevedo
                var_cadencia = gw_zsdt0059-formula2 - var_cade_soma.
              ELSE.
                var_cadencia = gw_zsdt0059-formula2.
              ENDIF.

*              CASE I_AUART.
*                WHEN 'ZCPV' OR 'ZCOP'
*                  OR 'ZROB' OR 'ZREB'
*                  OR 'ZDMI' OR 'ZCFX'
*                  OR 'ZROF'.
*                  LOOP AT GT_ZSDT0094 INTO GW_ZSDT0094.
*                    VAR_CADE_SOMA = VAR_CADE_SOMA + GW_ZSDT0094-CADENCIA_QTE.
*                  ENDLOOP.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2 - VAR_CADE_SOMA.
*                WHEN OTHERS.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
*              ENDCASE.

              "              GOBJ_ZCL_TAXA_CURVA->SET_CADENCIA( VAR_CADENCIA ).
              gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
              var_total_proporcional = ( ( gw_zsdt0053-vlrtot / gw_zsdt0053-zmeng ) * var_cadencia ).

              IF var_total_proporcional EQ 0.

                IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                  var_total_proporcional = gw_zsdt0053-dmbtr * var_cadencia.
                ELSE.
                  IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                  ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                    "  22.08.2023 ------>
                  ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                  ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                    "  22.08.2023 ------<
                  ENDIF.
                ENDIF.
              ENDIF.

              gobj_zcl_taxa_curva->set_total_proporcional( var_total_proporcional ).
              gobj_zcl_taxa_curva->set_posnr( gw_zsdt0059-posnr ).
              gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059-bezei ).
              var_taxa_cambio = gw_zsdt0059_cambio-formula2.
              gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
              gobj_zcl_taxa_curva->set_tipo( c_venda ).

              " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
              CALL METHOD get_vlr_bio
                EXPORTING
                  iv_matnr    = gw_zsdt0051-matnr
                  iv_doc_simu = gw_zsdt0053-nro_sol_ov
                CHANGING
                  cv_cadencia = var_cadencia.
              " 09.09.2023 - bug 150168 - quevedo -->

              gobj_zcl_taxa_curva->set_cadencia( i_cadencia = var_cadencia
                                                 i_tcode    = i_tcode
                                                 ).

              gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
              var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).

*              VAR_TIPO = 'C'.
*              GOBJ_ZCL_TAXA_CURVA->SET_TIPO_TAXA( 'C' ).

              IF ( gw_saldo-data < sy-datum ).
                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = sy-datum
                                                                         i_tipo = var_tipo
                                                                         ).
              ELSE.
                var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = gw_saldo-data
                                                                         i_tipo = var_tipo
                                                                         ).
              ENDIF.

              IF ( gw_zsdt0059-valdt < sy-datum ).
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
              ELSE.
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
              ENDIF.


              gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
              gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

              var_safra = gw_zsdt0053-charg.
              gobj_zcl_taxa_curva->set_safra( var_safra ).
              me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ).

              " 17.07.2024 - RAMON - 144484 -->
              " esse codigo foi inserido para nao deixar gerar hedge vazio,
              " foi feito aqui pq nenhum processo deve gerar hedge sem valor
              IF gobj_zcl_taxa_curva->get_total_proporcional( ) NE 0." ramon 17.07.2024
                me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva ).
              ENDIF.
              " 17.07.2024 - RAMON - 144484 --<

*              me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva )." 17.07.2024 - RAMON - 144484 --> COMENTADO

*              IF ( I_UCOMM EQ 'MODRED' ).
*              IF ( SY-SUBRC IS INITIAL AND SY-UNAME EQ 'WBARBOSA').
*
*                ME->FRETE( I_NUMERO  = GW_ZSDT0059-NRO_SOL_OV
*                           I_FIXACAO = GW_ZSDT0059-POSNR
*                           I_TCODE   = I_TCODE ).
*
*                ME->FRETE( I_NUMERO = GW_ZSDT0059-NRO_SOL_OV
*                          I_FIXACAO = GW_ZSDT0059-POSNR
*                          I_STATUS  = 'X'
*                          I_TCODE   = I_TCODE
*                          ).
*
*              ENDIF.

*              DELETE GT_SALDO WHERE SALDO < 1.
              var_tabix = var_tabix + 1.
*              CONTINUE.

            ENDIF.

          ELSE.

            FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
            CREATE OBJECT gobj_zcl_taxa_curva.
            CREATE OBJECT gobj_zcl_webservice_tx_curva.

            READ TABLE gt_data INTO gw_data WITH KEY fixacao = gw_zsdt0059-posnr.
            IF ( gw_data-data < sy-datum ).
              gw_data-data = gw_data-data + 30.
            ENDIF.

            "Inserir na ZSDT0095 E STARTAR A COTAÇÃO DO HEDGE
            IF i_auart NOT IN r_auart.
              me->zif_taxa_curva_db~inserir_frame( i_zsdt0059 = gw_zsdt0059
                                                   i_zsdt0055 = gw_zsdt0055
                                                   i_data     = gw_data-data
                                                   i_auart    = i_auart ).
            ENDIF.

*            CASE I_AUART.
*              WHEN 'ZCPV' OR 'ZCOP'
*                OR 'ZROB' OR 'ZREB'
*                OR 'ZDMI' OR 'ZCFX'
*                OR 'ZROF'.
*              WHEN OTHERS.
*                ME->ZIF_TAXA_CURVA_DB~INSERIR_FRAME( I_ZSDT0059 = GW_ZSDT0059
*                                                     I_ZSDT0055 = GW_ZSDT0055
*                                                     I_DATA     = GW_DATA-DATA
*                                                     I_AUART    = I_AUART ).
*
*            ENDCASE.

            gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
            gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
            gobj_zcl_taxa_curva->set_data_venc(  gw_data-data ).

*            VAR_TIPO = 'C'.
*            GOBJ_ZCL_TAXA_CURVA->SET_TIPO_TAXA( 'C' ).

*            VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
            IF i_auart IN r_auart.
              " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
*               LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
*                var_cade_soma = var_cade_soma + gw_zsdt0094-cadencia_qte.
*              ENDLOOP.
              CALL METHOD calc_cadencia_94
                EXPORTING
                  it_0094     = gt_zsdt0094
                  iv_matnr    = gw_zsdt0051-matnr
                  iv_doc_simu = gw_zsdt0059-nro_sol_ov
                CHANGING
                  cv_cadencia = var_cade_soma.
              " 09.09.2023 - bug 150168 - quevedo
              var_cadencia = gw_zsdt0059-formula2 - var_cade_soma.
            ELSE.
              var_cadencia = gw_zsdt0059-formula2.
            ENDIF.

*            CASE I_AUART.
*              WHEN 'ZCPV' OR 'ZCOP'
*                OR 'ZROB' OR 'ZREB'
*                OR 'ZDMI' OR 'ZCFX'
*                OR 'ZROF'.
*                LOOP AT GT_ZSDT0094 INTO GW_ZSDT0094.
*                  VAR_CADE_SOMA = VAR_CADE_SOMA + GW_ZSDT0094-CADENCIA_QTE.
*                ENDLOOP.
*                VAR_CADENCIA = GW_ZSDT0059-FORMULA2 - VAR_CADE_SOMA.
*              WHEN OTHERS.
*                VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
*            ENDCASE.

            "GOBJ_ZCL_TAXA_CURVA->SET_CADENCIA( VAR_CADENCIA ).
            gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
            var_total_proporcional = ( ( gw_zsdt0053-vlrtot / gw_zsdt0053-zmeng ) * var_cadencia ).

            IF var_total_proporcional EQ 0.

              IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                var_total_proporcional = gw_zsdt0053-dmbtr * var_cadencia.
              ELSE.
                IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                  "  22.08.2023 ------>
                ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                  "  22.08.2023 ------<
                ENDIF.
              ENDIF.
            ENDIF.

            gobj_zcl_taxa_curva->set_total_proporcional( var_total_proporcional ).
            gobj_zcl_taxa_curva->set_posnr( gw_zsdt0059-posnr ).
            gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059-bezei ).
            var_taxa_cambio = gw_zsdt0059_cambio-formula2.
            gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
            gobj_zcl_taxa_curva->set_tipo( c_venda ).

            " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
            CALL METHOD get_vlr_bio
              EXPORTING
                iv_matnr    = gw_zsdt0051-matnr
                iv_doc_simu = gw_zsdt0053-nro_sol_ov
              CHANGING
                cv_cadencia = var_cadencia.
            " 09.09.2023 - bug 150168 - quevedo -->

            gobj_zcl_taxa_curva->set_cadencia( i_cadencia = var_cadencia
                                               i_tcode    = i_tcode
                                               ).

            gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
            var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).


            IF ( gw_saldo-data < sy-datum ).
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = sy-datum
                                                                       i_tipo = var_tipo
                                                                       ).
            ELSE.
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data = gw_saldo-data
                                                                       i_tipo = var_tipo ).
            ENDIF.

            IF ( gw_zsdt0059-valdt < sy-datum ).
              gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
            ELSE.
              gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
            ENDIF.

            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
            gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

            var_safra = gw_zsdt0053-charg.
            gobj_zcl_taxa_curva->set_safra( var_safra ).
            me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ).

            " 17.07.2024 - RAMON - 144484 -->
            " esse codigo foi inserido para nao deixar gerar hedge vazio,
            " foi feito aqui pq nenhum processo deve gerar hedge sem valor
            IF gobj_zcl_taxa_curva->get_total_proporcional( ) NE 0." ramon 17.07.2024
              me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva ).
            ENDIF.
            " 17.07.2024 - RAMON - 144484 --<

*            me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva )." 17.07.2024 - RAMON - 144484 --> COMENTADO


            var_tabix = var_tabix + 1.
          ENDIF.
        ENDWHILE.

      ELSE.


        READ TABLE gt_zsdt0073 INTO gw_zsdt0073 WITH KEY nro_sol_ov = gw_zsdt0059-nro_sol_ov
                                                         fixacao    = gw_zsdt0059-posnr.
        READ TABLE gt_t052     INTO gw_t052     WITH KEY zterm      = gw_zsdt0073-zterm.

        "Inserir na ZSDT0095 E STARTAR A COTAÇÃO DO HEDGE
        IF i_auart NOT IN r_auart.
          me->zif_taxa_curva_db~inserir_frame( i_zsdt0059 = gw_zsdt0059
                                               i_zsdt0055 = gw_zsdt0055
                                               i_data     = gw_data-data
                                               i_auart    = i_auart ).
        ENDIF.

*        CASE I_AUART.
*          WHEN 'ZCPV' OR 'ZCOP'
*            OR 'ZROB' OR 'ZREB'
*            OR 'ZDMI' OR 'ZCFX'
*            OR 'ZROF'.
*          WHEN OTHERS.
*            ME->ZIF_TAXA_CURVA_DB~INSERIR_FRAME( I_ZSDT0059 = GW_ZSDT0059
*                                                 I_ZSDT0055 = GW_ZSDT0055
*                                                 I_DATA     = GW_DATA-DATA
*                                                 I_AUART    = I_AUART ).
*
*        ENDCASE.

        CASE gw_t052-zdart.

          WHEN: 'D'.

            FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
            CREATE OBJECT gobj_zcl_taxa_curva.
            CREATE OBJECT gobj_zcl_webservice_tx_curva.

            gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
            gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

            var_data_calculada = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                     i_data_final   = gw_zsdt0051-dtate_logist ).

            data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).

            IF ( gw_zsdt0059-valdt < sy-datum ).
              gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
            ELSE.
              gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
            ENDIF.

*              VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
            IF i_auart IN r_auart.
              " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
*              LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
*                var_cade_soma = var_cade_soma + gw_zsdt0094-cadencia_qte.
*              ENDLOOP.
              CALL METHOD calc_cadencia_94
                EXPORTING
                  it_0094     = gt_zsdt0094
                  iv_matnr    = gw_zsdt0051-matnr
                  iv_doc_simu = gw_zsdt0059-nro_sol_ov
                CHANGING
                  cv_cadencia = var_cade_soma.
              " 09.09.2023 - bug 150168 - quevedo
              var_cadencia = gw_zsdt0059-formula2 - var_cade_soma.
            ELSE.
              var_cadencia = gw_zsdt0059-formula2.
            ENDIF.

*            CASE I_AUART.
*              WHEN 'ZCPV' OR 'ZCOP'
*                OR 'ZROB' OR 'ZREB'
*                OR 'ZDMI' OR 'ZCFX'
*                OR 'ZROF'.
*                LOOP AT GT_ZSDT0094 INTO GW_ZSDT0094.
*                  VAR_CADE_SOMA = VAR_CADE_SOMA + GW_ZSDT0094-CADENCIA_QTE.
*                ENDLOOP.
*                VAR_CADENCIA = GW_ZSDT0059-FORMULA2 - VAR_CADE_SOMA.
*              WHEN OTHERS.
*                VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
*            ENDCASE.

            "GOBJ_ZCL_TAXA_CURVA->SET_CADENCIA( VAR_CADENCIA ).
            gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
            var_total_proporcional = ( ( gw_zsdt0053-vlrtot / gw_zsdt0053-zmeng ) * var_cadencia ).

            IF var_total_proporcional EQ 0.

              IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                var_total_proporcional = gw_zsdt0053-dmbtr * var_cadencia.
              ELSE.
                IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).

                  "  22.08.2023 ------>
                ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).

                ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                  var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                  "  22.08.2023 ------<


                ENDIF.
              ENDIF.

            ENDIF.

            gobj_zcl_taxa_curva->set_total_proporcional( var_total_proporcional ).
            gobj_zcl_taxa_curva->set_posnr( gw_zsdt0059-posnr ).
            gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059-bezei ).
            var_taxa_cambio = gw_zsdt0059_cambio-formula2.
            gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
            gobj_zcl_taxa_curva->set_tipo( c_venda ).

            " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
            CALL METHOD get_vlr_bio
              EXPORTING
                iv_matnr    = gw_zsdt0051-matnr
                iv_doc_simu = gw_zsdt0053-nro_sol_ov
              CHANGING
                cv_cadencia = var_cadencia.
            " 09.09.2023 - bug 150168 - quevedo -->

            gobj_zcl_taxa_curva->set_cadencia( i_cadencia = var_cadencia
                                               i_tcode    = i_tcode ).
            gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
            var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).


            TRY.
                IF ( gw_zsdt0059-valdt  < sy-datum ).
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                           i_data_lib = sy-datum
                                                                           i_tipo     = var_tipo
                                                                           ).

                ELSE.
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                           i_data_lib = gw_zsdt0059-valdt
                                                                           i_tipo     = var_tipo
                                                                           ).
                ENDIF.

              CATCH zcx_webservice INTO cx_exception.
                var_msg  = cx_exception->get_text( ).
                MESSAGE e007(zwebservice) WITH var_msg.
            ENDTRY.


            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
            gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

            var_safra = gw_zsdt0053-charg.
            gobj_zcl_taxa_curva->set_safra( var_safra ).
            me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ).

            " 17.07.2024 - RAMON - 144484 -->
            " esse codigo foi inserido para nao deixar gerar hedge vazio,
            " foi feito aqui pq nenhum processo deve gerar hedge sem valor
            IF gobj_zcl_taxa_curva->get_total_proporcional( ) NE 0." ramon 17.07.2024
              me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva ).
            ENDIF.
            " 17.07.2024 - RAMON - 144484 --<

*            me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva )." 17.07.2024 - RAMON - 144484 --> COMENTADO


          WHEN: 'B'.

            IF NOT ( gw_t052-ztag1 IS INITIAL ).

              FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
              CREATE OBJECT gobj_zcl_taxa_curva.
              CREATE OBJECT gobj_zcl_webservice_tx_curva.

              gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
              gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

              var_data_calculada = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                       i_data_final   = gw_zsdt0051-dtate_logist ).

              gw_zsdt0051-dtde_logist = var_data_calculada + gw_t052-ztag1.

              data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0051-dtde_logist ).

              IF ( gw_zsdt0059-valdt < sy-datum ).
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).

              ELSE.
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
              ENDIF.

*                VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
              IF i_auart IN r_auart.
                " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
                CALL METHOD calc_cadencia_94
                  EXPORTING
                    it_0094     = gt_zsdt0094
                    iv_matnr    = gw_zsdt0051-matnr
                    iv_doc_simu = gw_zsdt0059-nro_sol_ov
                  CHANGING
                    cv_cadencia = var_cade_soma.
                " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
*                LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
*                  var_cade_soma = var_cade_soma + gw_zsdt0094-cadencia_qte.
*                ENDLOOP.
*                " 04.09.2023 - bug 150168 -

                var_cadencia = gw_zsdt0059-formula2 - var_cade_soma.
              ELSE.
                var_cadencia = gw_zsdt0059-formula2.
              ENDIF.

*              CASE I_AUART.
*                WHEN 'ZCPV' OR 'ZCOP'
*                  OR 'ZROB' OR 'ZREB'
*                  OR 'ZDMI' OR 'ZCFX'
*                  OR 'ZROF'.
*                  LOOP AT GT_ZSDT0094 INTO GW_ZSDT0094.
*                    VAR_CADE_SOMA = VAR_CADE_SOMA + GW_ZSDT0094-CADENCIA_QTE.
*                  ENDLOOP.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2 - VAR_CADE_SOMA.
*                WHEN OTHERS.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
*              ENDCASE.

              "GOBJ_ZCL_TAXA_CURVA->SET_CADENCIA( VAR_CADENCIA ).

              gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
              var_total_proporcional = ( ( gw_zsdt0053-vlrtot / gw_zsdt0053-zmeng ) * var_cadencia ).

              IF var_total_proporcional EQ 0.

                IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                  var_total_proporcional = gw_zsdt0053-dmbtr * var_cadencia.
                ELSE.
                  IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                  ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).

                    "  22.08.2023 ------>
                  ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).

                  ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                    "  22.08.2023 ------<


                  ENDIF.
                ENDIF.

              ENDIF.

*              gobj_zcl_taxa_curva->set_total_proporcional( var_total_proporcional ). " 09.09.2023 - bug 150168 - quevedo --> mesma chamada da próxima linha
              gobj_zcl_taxa_curva->set_total_proporcional( var_total_proporcional ).
              gobj_zcl_taxa_curva->set_posnr( gw_zsdt0059-posnr ).
              gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059-bezei ).
              var_taxa_cambio = gw_zsdt0059_cambio-formula2.
              gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
              gobj_zcl_taxa_curva->set_tipo( c_venda ).

              " 28.08.2023 - RAMON -->
              CALL METHOD get_vlr_bio
                EXPORTING
                  iv_matnr    = gw_zsdt0051-matnr
                  iv_doc_simu = gw_zsdt0053-nro_sol_ov
                CHANGING
                  cv_cadencia = var_cadencia.
              " 28.08.2023 - RAMON --<

              gobj_zcl_taxa_curva->set_cadencia( i_cadencia = var_cadencia
                                                 i_tcode    = i_tcode ).

              gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
              var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).


              TRY.

                  IF ( gw_zsdt0059-valdt  < sy-datum ).
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                             i_data_lib = sy-datum
                                                                             i_tipo     = var_tipo
                                                                             ).

                  ELSE.
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                             i_data_lib = gw_zsdt0059-valdt
                                                                             i_tipo     = var_tipo
                                                                             ).
                  ENDIF.
                CATCH zcx_webservice INTO cx_exception.
                  var_msg  = cx_exception->get_text( ).
                  MESSAGE e007(zwebservice) WITH var_msg.
              ENDTRY.

              gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
              gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

              var_safra = gw_zsdt0053-charg.
              gobj_zcl_taxa_curva->set_safra( var_safra ).
              me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ).

              " 17.07.2024 - RAMON - 144484 -->
              " esse codigo foi inserido para nao deixar gerar hedge vazio,
              " foi feito aqui pq nenhum processo deve gerar hedge sem valor
              IF gobj_zcl_taxa_curva->get_total_proporcional( ) NE 0." ramon 17.07.2024
                me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva ).
              ENDIF.
              " 17.07.2024 - RAMON - 144484 --<

*              me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva )." 17.07.2024 - RAMON - 144484 --> COMENTADO


            ELSEIF NOT ( gw_t052-zmona IS INITIAL ).

              FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
              CREATE OBJECT gobj_zcl_taxa_curva.
              CREATE OBJECT gobj_zcl_webservice_tx_curva.

              gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
              gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).


              var_mes = ( ( var_mes + gw_zsdt0051-dtde_logist+4(2) ) + gw_t052-zmona ).
              IF ( var_mes > 12 ).
                var_mes_aux =  gw_t052-zmona.
                var_ano = gw_zsdt0051-dtde_logist(4) + 1.
              ELSE.
                var_mes_aux = var_mes.
                var_ano     = gw_zsdt0051-dtde_logist(4).
              ENDIF.

              CONCATENATE var_ano var_mes_aux var_data+6(2) INTO var_data_completa.
              var_data_completa = var_data_completa +  gw_t052-zfael.

              data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_completa ).

              IF ( gw_zsdt0059-valdt  < sy-datum ).
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
              ELSE.
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0059-valdt ).
              ENDIF.

*                VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
              IF i_auart IN r_auart.

                " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
                CALL METHOD calc_cadencia_94
                  EXPORTING
                    it_0094     = gt_zsdt0094
                    iv_matnr    = gw_zsdt0051-matnr
                    iv_doc_simu = gw_zsdt0059-nro_sol_ov
                  CHANGING
                    cv_cadencia = var_cade_soma.
*                LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
*                  var_cade_soma = var_cade_soma + gw_zsdt0094-cadencia_qte.
*                ENDLOOP.
                " 09.09.2023 - bug 150168 - quevedo
                var_cadencia = gw_zsdt0059-formula2 - var_cade_soma.
              ELSE.
                var_cadencia = gw_zsdt0059-formula2.
              ENDIF.

*              CASE I_AUART.
*                WHEN 'ZCPV' OR 'ZCOP'
*                  OR 'ZROB' OR 'ZREB'
*                  OR 'ZDMI' OR 'ZCFX'
*                  OR 'ZROF'.
*                  LOOP AT GT_ZSDT0094 INTO GW_ZSDT0094.
*                    VAR_CADE_SOMA = VAR_CADE_SOMA + GW_ZSDT0094-CADENCIA_QTE.
*                  ENDLOOP.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2 - VAR_CADE_SOMA.
*                WHEN OTHERS.
*                  VAR_CADENCIA = GW_ZSDT0059-FORMULA2.
*              ENDCASE.

*              gobj_zcl_taxa_curva->set_cadencia( var_cadencia ). " 09.09.2023 - bug 150168 - quevedo - estava duplicado, já está fazendo mais embaixo
              gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
              var_total_proporcional = ( ( gw_zsdt0053-vlrtot / gw_zsdt0053-zmeng ) * var_cadencia ).

              IF var_total_proporcional EQ 0.

                IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                  var_total_proporcional = gw_zsdt0053-dmbtr * var_cadencia.
                ELSE.
                  IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).
                  ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).


                    "  22.08.2023 ------>
                  ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  / 1000 ).

                  ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                    var_total_proporcional = ( ( gw_zsdt0053-dmbtr * var_cadencia )  * 1000 ).
                    "  22.08.2023 ------<

                  ENDIF.
                ENDIF.

              ENDIF.

              gobj_zcl_taxa_curva->set_total_proporcional( var_total_proporcional ).
              gobj_zcl_taxa_curva->set_posnr( gw_zsdt0059-posnr ).
              gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059-bezei ).
              var_taxa_cambio = gw_zsdt0059_cambio-formula2.
              gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
              gobj_zcl_taxa_curva->set_tipo( c_venda ).

              " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
              CALL METHOD get_vlr_bio
                EXPORTING
                  iv_matnr    = gw_zsdt0051-matnr
                  iv_doc_simu = gw_zsdt0059-nro_sol_ov
                CHANGING
                  cv_cadencia = var_cadencia.
              " 09.09.2023 - bug 150168 - quevedo -->


              gobj_zcl_taxa_curva->set_cadencia( i_cadencia = var_cadencia
                                                 i_tcode    = i_tcode ).

              gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
              var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).

              TRY.
                  IF ( gw_zsdt0059-valdt < sy-datum ).

                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "VAR_DATA_COMPLETA
                                                                             i_data_lib = sy-datum
                                                                             i_tipo     = var_tipo
                                                                             ).

                  ELSE.
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "VAR_DATA_COMPLETA
                                                                             i_data_lib = gw_zsdt0059-valdt
                                                                             i_tipo     = var_tipo
                                                                             ).

                  ENDIF.
                CATCH zcx_webservice INTO cx_exception.
                  var_msg  = cx_exception->get_text( ).
                  MESSAGE e007(zwebservice) WITH var_msg.
              ENDTRY.

              gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
              gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

              var_safra = gw_zsdt0053-charg.
              gobj_zcl_taxa_curva->set_safra( var_safra ).
              me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ).

              " 17.07.2024 - RAMON - 144484 -->
              " esse codigo foi inserido para nao deixar gerar hedge vazio,
              " foi feito aqui pq nenhum processo deve gerar hedge sem valor
              IF gobj_zcl_taxa_curva->get_total_proporcional( ) NE 0." ramon 17.07.2024
                me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva ).
              ENDIF.
              " 17.07.2024 - RAMON - 144484 --<

*              me->zif_taxa_curva_db~atualizar_frame( gobj_zcl_taxa_curva )." 17.07.2024 - RAMON - 144484 --> COMENTADO

            ENDIF.
        ENDCASE.
      ENDIF.
      DELETE gt_saldo WHERE saldo < 1.
    ENDLOOP.


*Estorna e Lança o Frete da Edição da Quantidade da OV.
    IF gt_zsdt0059 IS NOT INITIAL AND i_ucomm EQ 'MODRED'.

      " 26.07.2024 - RAMON -- descomentado para testar - 144484 -->
      " se comentado nao gera o hedge da linha 0001 da posnr(fixação) - (vindo da zsdt0062)
      LOOP AT gt_zsdt0053 INTO gw_zsdt0053.
        " 26.07.2024 - RAMON -- descomentado para testar - 144484 --<

*     STATUS "W" PARA BAIXAR O FRETE E X PARA LANÇAR NOVO FRETE
        me->frete( i_numero  = gw_zsdt0053-nro_sol_ov
                   i_fixacao = gw_zsdt0053-fixacao
                   i_status  = 'W'
                   i_tcode   = i_tcode ).

        me->frete( i_numero  = gw_zsdt0053-nro_sol_ov
                   i_fixacao = gw_zsdt0053-fixacao
                   i_status  = 'X'
                   i_tcode   = i_tcode
                   i_ucomm   = i_ucomm ).

        " 26.07.2024 - RAMON -- descomentado para testar - 144484 ---->
      ENDLOOP.
      " 26.07.2024 - RAMON -- descomentado para testar - 144484 ----<
    ENDIF.

    me->update_job(
      tcode      = i_tcode
      nro_sol_ov = i_numero
      vl_vbeln   = i_vbeln
    ).

  ENDMETHOD.


  METHOD FRAME_EDICAO.

    DATA: GW_ZSDT0095 TYPE ZSDT0095.

    DATA: GT_ZSDT0094 TYPE TABLE OF ZSDT0094,
          GW_ZSDT0094 TYPE ZSDT0094.

    DATA: VAR_ESTORNO TYPE NUM10.

    DATA: VAR_COTACAO TYPE KURRF. "Valor da Cotação.
    DATA: VAR_TIPO    TYPE CHAR01.
    DATA: DATA_VENC TYPE DATUM.

    DATA:  GOBJ_ZCL_TAXA_CURVA          TYPE REF TO ZCL_TAXA_CURVA, "Objeto da taxa curva.
           GOBJ_ZCL_WEBSERVICE_TX_CURVA TYPE REF TO ZCL_WEBSERVICE_TX_CURVA.

    IF  ( I_BEZEI(4) EQ 'DIFE' ).

      SELECT * FROM ZSDT0094
        INTO TABLE GT_ZSDT0094
      WHERE NRO_SOL_OV = I_NUMERO
        AND FIXACAO    = I_FIXACAO
        AND BEZEI      = I_BEZEI
        AND ESTORNO    = 0.

    ELSE.

      SELECT SINGLE * FROM ZSDT0095 INTO GW_ZSDT0095 WHERE NRO_SOL_OV = I_NUMERO
                                                       AND FIXACAO    = I_FIXACAO
                                                       AND BEZEI      = I_BEZEI.
      IF ( SY-SUBRC EQ 0 ).

        DELETE FROM ZSDT0095
              WHERE NRO_SOL_OV = I_NUMERO
                AND FIXACAO    = I_FIXACAO
                AND BEZEI      = I_BEZEI.

        IF ( SY-DBCNT NE 0 ).

          COMMIT WORK.

          SELECT * FROM ZSDT0094
            INTO TABLE GT_ZSDT0094
          WHERE NRO_SOL_OV = I_NUMERO
            AND FIXACAO    = I_FIXACAO
            AND BEZEI      = I_BEZEI
            AND ESTORNO    = 0.
        ENDIF.

      ELSEIF ( SY-SUBRC NE 0 ) AND ( I_TCODE EQ 'VF11') .

        SELECT * FROM ZSDT0094
          INTO TABLE GT_ZSDT0094
        WHERE NRO_SOL_OV = I_NUMERO
          AND VBELN      = I_VBELN
          AND FIXACAO    = I_FIXACAO
          AND ESTORNO    = 0.

      ENDIF.
    ENDIF.

    IF NOT ( GT_ZSDT0094[] IS INITIAL ).

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR             = '01'
          OBJECT                  = 'ZEST_0094'
        IMPORTING
          NUMBER                  = VAR_ESTORNO "Numeração para identificar o estorno.
        EXCEPTIONS
          INTERVAL_NOT_FOUND      = 1
          NUMBER_RANGE_NOT_INTERN = 2
          OBJECT_NOT_FOUND        = 3
          QUANTITY_IS_0           = 4
          QUANTITY_IS_NOT_1       = 5
          INTERVAL_OVERFLOW       = 6
          BUFFER_OVERFLOW         = 7
          OTHERS                  = 8.

      LOOP AT GT_ZSDT0094 INTO GW_ZSDT0094.

        FREE: GOBJ_ZCL_TAXA_CURVA, GOBJ_ZCL_WEBSERVICE_TX_CURVA.
        CREATE OBJECT GOBJ_ZCL_TAXA_CURVA.
        CREATE OBJECT GOBJ_ZCL_WEBSERVICE_TX_CURVA.

        GOBJ_ZCL_TAXA_CURVA->SET_NUMERO( GW_ZSDT0094-NRO_SOL_OV ).
        GOBJ_ZCL_TAXA_CURVA->SET_INCOTERMS( GW_ZSDT0094-INCO1 ).
        GOBJ_ZCL_TAXA_CURVA->SET_DATA_VENC( GW_ZSDT0094-DATA_VENC ).
        GOBJ_ZCL_TAXA_CURVA->SET_POSNR( GW_ZSDT0094-FIXACAO ).
        GOBJ_ZCL_TAXA_CURVA->SET_DATA_LIB( SY-DATUM ).
        GOBJ_ZCL_TAXA_CURVA->SET_DATA_REGISTRO( GW_ZSDT0094-DATA_REGISTRO ).
        GOBJ_ZCL_TAXA_CURVA->SET_HORA_REGISTRO( GW_ZSDT0094-HORA_REGISTRO ).

        GOBJ_ZCL_TAXA_CURVA->SET_CADENCIA( I_CADENCIA = GW_ZSDT0094-CADENCIA_QTE
                                           I_NEGATIVA = 'X'
                                         ).
        GOBJ_ZCL_TAXA_CURVA->SET_ZIEME( GW_ZSDT0094-ZIEME ).
        GOBJ_ZCL_TAXA_CURVA->SET_TOTAL_PROPORCIONAL( I_TOTAL =  GW_ZSDT0094-TOTAL_PROPORC
                                                     I_NEGATIVA = 'X'
                                                     ).
        GOBJ_ZCL_TAXA_CURVA->SET_FRETE_CIF( GW_ZSDT0094-FRETE_CIF ).
        GOBJ_ZCL_TAXA_CURVA->SET_FRETE_PORTO( GW_ZSDT0094-FRETE_PORTO ).
        GOBJ_ZCL_TAXA_CURVA->SET_TIPO( GW_ZSDT0094-TIPO ).
        GOBJ_ZCL_TAXA_CURVA->SET_BEZEI( GW_ZSDT0094-BEZEI ).
        GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( GW_ZSDT0094-TAXA_CAMBIO ).
        GOBJ_ZCL_TAXA_CURVA->SET_ESTORNO( VAR_ESTORNO ).
        GOBJ_ZCL_TAXA_CURVA->SET_SAFRA( GW_ZSDT0094-SAFRA ).
        GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE
                                                 I_TIPO = I_TIPO
                                                 I_STATUS = I_STATUS ).

        VAR_TIPO =  GOBJ_ZCL_TAXA_CURVA->GET_TIPO_TAXA( ).

        VAR_COTACAO = GOBJ_ZCL_WEBSERVICE_TX_CURVA->BUSCAR_TAXA( I_DATA     = GW_ZSDT0094-DATA_VENC
                                                                 I_DATA_LIB = SY-DATUM
                                                                 I_TIPO     = VAR_TIPO
                                                                 ).
        GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CURVA( VAR_COTACAO ).
        ME->ZIF_TAXA_CURVA_DB~ATUALIZAR( GOBJ_ZCL_TAXA_CURVA ).
        ME->ZIF_TAXA_CURVA_DB~INSERIR( OBJ_TAXA = GOBJ_ZCL_TAXA_CURVA
                                       NEGATIVA = 'X').

* Estorna os Valores Lançados
        GOBJ_ZCL_TAXA_CURVA->ESTORNO_VF11( I_TCODE    = I_TCODE
                                           I_AUART    = I_AUART
                                           I_ZSDT0094 = GW_ZSDT0094
                                           I_VBELN    = I_VBELN ).

        CLEAR: GW_ZSDT0094, VAR_COTACAO, VAR_TIPO.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD FRAME_PARCIAL.

    DATA: VAR_DIFERENCA TYPE ZSDT0094-TOTAL_PROPORC.
    DATA: VAR_AUX_DIFER TYPE ZSDT0094-TOTAL_PROPORC.
    DATA: GT_ZSDT0094 TYPE TABLE OF ZSDT0094,
          GW_ZSDT0094 TYPE ZSDT0094.
    DATA: GT_ZSDT0059 TYPE TABLE OF ZSDT0059,
          GW_ZSDT0059 TYPE ZSDT0059.

    DATA: VAR_TAXA_CAMBIO TYPE UKURSP.

    DATA: VAR_COTACAO TYPE KURRF. "Valor da Cotação.
    DATA: VAR_TIPO TYPE CHAR01.

    DATA: GOBJ_ZCL_TAXA_CURVA          TYPE REF TO ZCL_TAXA_CURVA, "Objeto da taxa curva.
          GOBJ_ZCL_WEBSERVICE_TX_CURVA TYPE REF TO ZCL_WEBSERVICE_TX_CURVA.

    ME->TOTAL_53( I_NUMERO  = I_NUMERO
                  I_FIXACAO = I_FIXACAO ).

    ME->TOTAL_94( I_NUMERO  = I_NUMERO
                  I_FIXACAO = I_FIXACAO ).

    VAR_DIFERENCA = ME->AT_TOTAL_53 - ME->AT_TOTAL_94.

    IF ( VAR_DIFERENCA NE 0 ).

      ME->TOTAL_94( EXPORTING I_NUMERO  = I_NUMERO
                              I_FIXACAO = I_FIXACAO
                    IMPORTING E_DIFE    = VAR_AUX_DIFER ).

      ME->FRAME_EDICAO( I_NUMERO  = I_NUMERO
                        I_FIXACAO = I_FIXACAO
                        I_BEZEI   = 'DIFERENCA'
                        I_TCODE   = I_TCODE ).

      VAR_DIFERENCA = VAR_DIFERENCA + VAR_AUX_DIFER.

      SELECT * FROM ZSDT0094
        INTO TABLE GT_ZSDT0094
      WHERE NRO_SOL_OV EQ I_NUMERO
        AND FIXACAO    EQ I_FIXACAO
        AND TIPO       EQ 'VDA'.

      SELECT * FROM ZSDT0059
        INTO TABLE GT_ZSDT0059
      WHERE NRO_SOL_OV EQ I_NUMERO
        AND POSNR      EQ I_FIXACAO
        AND FIELD      EQ 'PRECO'
        AND BEZEI      EQ 'TAXA CAMBIO FRAME'.

      SORT: GT_ZSDT0094 BY DATA_VENC DESCENDING.

      READ TABLE GT_ZSDT0094 INTO GW_ZSDT0094 INDEX 1.

      FREE: GOBJ_ZCL_TAXA_CURVA, GOBJ_ZCL_WEBSERVICE_TX_CURVA.
      CREATE OBJECT GOBJ_ZCL_TAXA_CURVA.
      CREATE OBJECT GOBJ_ZCL_WEBSERVICE_TX_CURVA.

      GOBJ_ZCL_TAXA_CURVA->SET_NUMERO( GW_ZSDT0094-NRO_SOL_OV ).

      IF ( GW_ZSDT0094-DATA_VENC < SY-DATUM ).
        GW_ZSDT0094-DATA_VENC = SY-DATUM + 30.
      ENDIF.

      GOBJ_ZCL_TAXA_CURVA->SET_DATA_VENC( GW_ZSDT0094-DATA_VENC ).
      GOBJ_ZCL_TAXA_CURVA->SET_DATA_LIB( SY-DATUM ).
      GOBJ_ZCL_TAXA_CURVA->SET_CADENCIA( I_CADENCIA = 0 ).
      GOBJ_ZCL_TAXA_CURVA->SET_ZIEME( GW_ZSDT0094-ZIEME ).
      GOBJ_ZCL_TAXA_CURVA->SET_TOTAL_PROPORCIONAL( I_TOTAL = VAR_DIFERENCA  ).
      GOBJ_ZCL_TAXA_CURVA->SET_FRETE_CIF( GW_ZSDT0094-FRETE_CIF ).
      GOBJ_ZCL_TAXA_CURVA->SET_FRETE_PORTO( GW_ZSDT0094-FRETE_PORTO ).
      GOBJ_ZCL_TAXA_CURVA->SET_TIPO( GW_ZSDT0094-TIPO ).
      GOBJ_ZCL_TAXA_CURVA->SET_POSNR( GW_ZSDT0094-FIXACAO ).
      GOBJ_ZCL_TAXA_CURVA->SET_DATA_REGISTRO( SY-DATUM ).
      GOBJ_ZCL_TAXA_CURVA->SET_HORA_REGISTRO( SY-UZEIT ).
      GOBJ_ZCL_TAXA_CURVA->SET_BEZEI( 'DIFERENCA' ).
      GOBJ_ZCL_TAXA_CURVA->SET_SAFRA( GW_ZSDT0094-SAFRA ).

      READ TABLE GT_ZSDT0059 INTO GW_ZSDT0059 INDEX 1.

      VAR_TAXA_CAMBIO = GW_ZSDT0059-FORMULA2.
      GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
      GOBJ_ZCL_TAXA_CURVA->VERIFICA_TIPO_TAXA( I_TCODE = I_TCODE ).
      VAR_TIPO = GOBJ_ZCL_TAXA_CURVA->GET_TIPO_TAXA( ).
      VAR_COTACAO = GOBJ_ZCL_WEBSERVICE_TX_CURVA->BUSCAR_TAXA( I_DATA     = GW_ZSDT0094-DATA_VENC
                                                               I_DATA_LIB = SY-DATUM
                                                               I_TIPO     = VAR_TIPO
                                                               ).
      GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CURVA( VAR_COTACAO ).
      ME->ZIF_TAXA_CURVA_DB~INSERIR( OBJ_TAXA = GOBJ_ZCL_TAXA_CURVA ).

      CLEAR: GW_ZSDT0094, VAR_COTACAO.
    ENDIF.

  ENDMETHOD.


  METHOD FREE.
    FREE: AT_TOTAL_53,
          AT_TOTAL_54,
          AT_TOTAL_55,
          AT_TOTAL_PESO_53,
          AT_TOTAL_94.
  ENDMETHOD.


  METHOD frete.


    CONSTANTS:
      c_frete_cif    TYPE c LENGTH 11 VALUE 'FRETE CIF',
      c_frete_porto  TYPE c LENGTH 13 VALUE 'FRETE PORTO',
      c_fobs         TYPE c LENGTH 4  VALUE 'FOBS',
      c_cambio_frame TYPE c LENGTH 17 VALUE 'TAXA CAMBIO FRAME',
      c_frete        TYPE c LENGTH 3   VALUE 'FRE'.


    DATA:
      gt_zsdt0051 TYPE TABLE OF zsdt0051, "Tabela de Solicitação Ordem de Venda - Cabeçalho
      gt_zsdt0053 TYPE TABLE OF zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
      gt_zsdt0055 TYPE TABLE OF zsdt0055, "Tabela de Solicitação Ordem de Venda – Logistica
      gt_zsdt0059 TYPE TABLE OF zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gt_zsdt0094 TYPE TABLE OF zsdt0094.

    DATA:
      gw_zsdt0051       TYPE zsdt0051, "Tabela de Solicitação Ordem de Venda - Cabeçalho
      gw_zsdt0053       TYPE zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
      gw_zsdt0055       TYPE zsdt0055, "Tabela de Solicitação Ordem de Venda – Logistica
      gw_zsdt0059_porto TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0059_cif   TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0059_fobs  TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0059_frame TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0094       TYPE zsdt0094.


    DATA:
      gobj_zcl_taxa_curva          TYPE REF TO zcl_taxa_curva, "Objeto da taxa curva.
      gobj_zcl_webservice_tx_curva TYPE REF TO zcl_webservice_tx_curva.


    DATA:
      var_cotacao     TYPE kurrf, "Valor da Cotação.
*        VAR_MONTANTE    TYPE ZSDT0053-ZMENG, "Valor do Montante.
      var_montante    TYPE p LENGTH 13 DECIMALS 5 , "Valor do Montante.
      var_montante_2  TYPE zsdt0053-dmbtr, "Valor do Montante.
      var_taxa_cambio TYPE ukursp. "Valor da Cotação.

    DATA: data_venc TYPE datum.

    DATA: var_tipo           TYPE char01,
          var_data_calculada TYPE d,
          soma               TYPE dzmeng,
          var_safra          TYPE ajahr.

    DATA(obj_auart) = NEW zcl_taxa_curva( ).

    DATA: r_auart TYPE RANGE OF auart.
    DATA: r_comp TYPE RANGE OF auart.
    DATA: r_devo_recu TYPE RANGE OF auart.

    r_comp = obj_auart->get_auart( 'ZHEDGECOMP' ). " Get SET de AUART de Complemento
    r_devo_recu = obj_auart->get_auart( 'ZHEDGEDEVO/RECU' ). " Get SET de AUART de Devolução/Recusa
    r_auart = obj_auart->get_auart( 'TODOS' ). " Get SET de AUART de Complemento/Devolução/Recusa


    SELECT * FROM zsdt0051 INTO TABLE gt_zsdt0051 WHERE nro_sol_ov EQ i_numero.

    IF ( sy-subrc EQ 0 ).
      "Lançar a negativa.
      IF ( i_status IS INITIAL OR i_status EQ 'W').
        me->edicao( i_numero  = i_numero
                    i_fixacao = i_fixacao
                    i_tcode   = i_tcode ).

        IF ( i_status NE 'W').
          me->frame_edicao( i_numero  = i_numero
                            i_fixacao = i_fixacao
                            i_bezei   = 'DIFERENCA'
                            i_tcode   = i_tcode ).
        ENDIF.
      ELSE.

        SELECT * FROM zsdt0053
          INTO TABLE gt_zsdt0053
          FOR ALL ENTRIES IN gt_zsdt0051
        WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
          AND fixacao    EQ i_fixacao.

        IF i_auart IN r_devo_recu.
          DELETE gt_zsdt0053 WHERE status EQ 'Y' AND vbeln NE i_vbeln.
          DELETE gt_zsdt0053 WHERE status EQ 'W'.
        ELSEIF i_auart IN r_comp.
          DELETE gt_zsdt0053 WHERE status EQ 'Y'.
          DELETE gt_zsdt0053 WHERE status EQ 'W' AND vbeln NE i_vbeln.
        ENDIF.

*        CASE I_AUART.
*          WHEN 'ZREB' OR 'ZROB' OR 'ZDMI' OR 'ZROF'.
*            DELETE GT_ZSDT0053 WHERE STATUS EQ 'Y' AND VBELN NE I_VBELN.
*            DELETE GT_ZSDT0053 WHERE STATUS EQ 'W'.
*          WHEN OTHERS.
*            DELETE GT_ZSDT0053 WHERE STATUS EQ 'Y'.
*            DELETE GT_ZSDT0053 WHERE STATUS EQ 'W'.
*        ENDCASE.

        "Tabela de Solicitação Ordem de Venda – Logistica
        SELECT * FROM zsdt0055
          INTO TABLE gt_zsdt0055
          FOR ALL ENTRIES IN gt_zsdt0051
        WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
          AND fixacao    EQ i_fixacao.

*        IF ( sy-tcode NE 'VF01' ).
        IF ( I_tcode NE 'VF01' ). " para Atender os Complementos de Valor - Bug Solto 151247 - PQ
          DELETE gt_zsdt0055 WHERE status EQ 'Y'.
          DELETE gt_zsdt0055 WHERE status EQ 'W'.
        ENDIF.


*        IF ( sy-tcode EQ 'VF01' ).
        IF ( I_tcode EQ 'VF01' ). " para Atender os Complementos de Valor - Bug Solto 151247 - PQ
          DELETE gt_zsdt0055 WHERE status EQ 'Y' AND vbeln NE i_vbeln.
          DELETE gt_zsdt0055 WHERE status EQ 'W' AND vbeln NE i_vbeln.

          IF i_auart IN r_auart.
            DELETE gt_zsdt0055 WHERE status IS INITIAL.
          ENDIF.

*          CASE I_AUART.
*            WHEN 'ZCPV' OR 'ZCOP'
*              OR 'ZREB' OR 'ZROB'
*              OR 'ZDMI' OR 'ZCFX'
*              OR 'ZROF'.
*              DELETE GT_ZSDT0055 WHERE STATUS IS INITIAL.
*          ENDCASE.
        ENDIF.

        DELETE gt_zsdt0055 WHERE status EQ 'C'.
        DELETE gt_zsdt0053 WHERE status EQ 'C'.

        IF gt_zsdt0055[] IS INITIAL .
          IF i_auart IN r_comp AND r_comp IS NOT INITIAL.

            SELECT * FROM zsdt0053
              INTO TABLE gt_zsdt0053
              FOR ALL ENTRIES IN gt_zsdt0051
            WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
              AND fixacao    EQ i_fixacao
              AND status     EQ 'W'.

            SELECT * FROM zsdt0094
              INTO TABLE gt_zsdt0094
              FOR ALL ENTRIES IN gt_zsdt0051
            WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
              AND fixacao    EQ i_fixacao
              AND bezei      EQ 'W'.

            CLEAR soma.
            LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
              soma = soma + gw_zsdt0094-cadencia_qte.
            ENDLOOP.

            IF gw_zsdt0094-bezei EQ 'V'.
              soma = soma * 1.
            ENDIF.

          ENDIF.

          IF i_auart IN r_devo_recu AND r_devo_recu IS NOT INITIAL.

            SELECT * FROM zsdt0053
              INTO TABLE gt_zsdt0053
              FOR ALL ENTRIES IN gt_zsdt0051
            WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
              AND fixacao    EQ i_fixacao
              AND status     EQ 'Y'.

            SELECT * FROM zsdt0094
              INTO TABLE gt_zsdt0094
              FOR ALL ENTRIES IN gt_zsdt0051
            WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
              AND fixacao    EQ i_fixacao
              AND bezei      EQ 'Y'.

            CLEAR soma.
            LOOP AT gt_zsdt0094 INTO gw_zsdt0094.
              soma = soma + gw_zsdt0094-cadencia_qte.
            ENDLOOP.

            IF gw_zsdt0094-bezei EQ 'V'.
              soma = soma * 1.
            ENDIF.

          ENDIF.
        ENDIF.

        "Tabela de Solicitação Ordem de Venda – PRECO
        SELECT * FROM zsdt0059
          INTO TABLE gt_zsdt0059
          FOR ALL ENTRIES IN gt_zsdt0051
        WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
          AND bezei IN (c_frete_cif,c_frete_porto,c_fobs,c_cambio_frame)
          AND posnr EQ i_fixacao.

        " 22.08.2023 - Ramon -->
        SELECT * FROM zsdt0059
           INTO TABLE @DATA(gt_conv_bio)
           FOR ALL ENTRIES IN @gt_zsdt0051
         WHERE nro_sol_ov  EQ @gt_zsdt0051-nro_sol_ov
           AND bezei       EQ 'CONVERSOR BIO'.
        " 22.08.2023 - Ramon --<

        IF NOT ( gt_zsdt0055[] IS INITIAL ).

          LOOP AT gt_zsdt0055 INTO gw_zsdt0055.

            READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0055-nro_sol_ov.
            IF i_vbeln IS INITIAL.
              READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY nro_sol_ov = gw_zsdt0055-nro_sol_ov
                                                                fixacao   = gw_zsdt0055-fixacao.
            ELSE.
              READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY nro_sol_ov = gw_zsdt0055-nro_sol_ov
                                                                fixacao   = gw_zsdt0055-fixacao
                                                                vbeln     = i_vbeln.
            ENDIF.

            READ TABLE gt_zsdt0059 INTO gw_zsdt0059_cif   WITH KEY nro_sol_ov  = gw_zsdt0053-nro_sol_ov
                                                                   bezei       = c_frete_cif.

            READ TABLE gt_zsdt0059 INTO gw_zsdt0059_porto WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                                   bezei      = c_frete_porto.

            READ TABLE gt_zsdt0059 INTO gw_zsdt0059_fobs  WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                                   bezei      = c_fobs.

            READ TABLE gt_zsdt0059 INTO gw_zsdt0059_frame  WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                                    bezei      = c_cambio_frame.
            " 22.08.2023 - Ramon -->
*            READ TABLE gt_conv_bio INTO DATA(ls_conv_bio) WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.   " 09.09.2023 - bug 150168 - comentado pois iremos utilizar o metodo do conversor
            " 22.08.2023 - Ramon --<

            FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
            CREATE OBJECT gobj_zcl_taxa_curva.
            CREATE OBJECT gobj_zcl_webservice_tx_curva.

            gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
            gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
            data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
            gobj_zcl_taxa_curva->set_data_lib( sy-datum ).

            gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).

            " 09.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
            CALL METHOD get_vlr_bio
            EXPORTING
              iv_matnr    = gw_zsdt0051-matnr
              iv_doc_simu = gw_zsdt0055-nro_sol_ov
            CHANGING
              cv_cadencia = gw_zsdt0055-cadencia_qte.
            " 09.09.2023 - bug 150168 - quevedo -->

            IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
              var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0055-cadencia_qte ).
            ELSE.
              IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.
              ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                " 22.08.2023 - Ramon -->
              ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
*                gw_zsdt0055-cadencia_qte = gw_zsdt0055-cadencia_qte * ls_conv_bio-formula2. " 09.09.2023 - bug 150168 - quevedo --> já está sendo convertido acima pelo metodo do conversor
                var_montante = ( var_montante * gw_zsdt0055-cadencia_qte ).
              ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
*                gw_zsdt0055-cadencia_qte = gw_zsdt0055-cadencia_qte * ls_conv_bio-formula2. " 09.09.2023 - bug 150168 - quevedo --> já está sendo convertido acima pelo metodo do conversor
                var_montante = ( var_montante * gw_zsdt0055-cadencia_qte ).
                " 22.08.2023 - Ramon --<
              ENDIF.
            ENDIF.

            " Conversão casas decimais
            var_montante_2 = var_montante.

            gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
            gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
            gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
            gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
            gobj_zcl_taxa_curva->set_posnr( gw_zsdt0055-fixacao ).

            CASE gw_zsdt0055-status.
              WHEN 'Y'.    gobj_zcl_taxa_curva->set_bezei( 'Y' ).
              WHEN 'W'.    gobj_zcl_taxa_curva->set_bezei( 'W' ).
              WHEN OTHERS. gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059_frame-bezei ).
            ENDCASE.

*          GOBJ_ZCL_TAXA_CURVA->SET_BEZEI( GW_ZSDT0059_FRAME-BEZEI ).
            gobj_zcl_taxa_curva->set_tipo( c_frete ).

            CLEAR: gw_zsdt0059_frame.
            READ TABLE gt_zsdt0059 INTO gw_zsdt0059_frame  WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                                    bezei      = c_cambio_frame
                                                                    field      = 'PRECO'.
            var_taxa_cambio  = gw_zsdt0059_frame-formula2.
            gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
            gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode  = i_tcode
                                                     i_status = gw_zsdt0055-status ).
            var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
            gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0055-cadencia_qte
                                                i_tcode    = i_tcode ).

            var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                     i_data_lib = sy-datum
                                                                     i_tipo     = var_tipo
                                                                   ).

            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
            gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

            var_safra = gw_zsdt0053-charg.
            gobj_zcl_taxa_curva->set_safra( var_safra ).
            me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

            CLEAR: gw_zsdt0055, gw_zsdt0051, gw_zsdt0059_cif, gw_zsdt0059_fobs, gw_zsdt0059_porto, var_cotacao, var_montante, var_montante_2, var_taxa_cambio.

          ENDLOOP.

          "Método para disparar o frame parcial, caso tenha diferença na quantidade.
          me->frame_parcial( i_numero  = i_numero
                             i_fixacao = i_fixacao
                             i_tcode   = i_tcode
                             i_ucomm   = i_ucomm
                             ).

        ELSE.

          LOOP AT gt_zsdt0053 INTO gw_zsdt0053.

            READ TABLE gt_zsdt0051 INTO gw_zsdt0051 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
            READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY nro_sol_ov = gw_zsdt0055-nro_sol_ov
                                                              fixacao   = gw_zsdt0055-fixacao.

            READ TABLE gt_zsdt0059 INTO gw_zsdt0059_cif   WITH KEY nro_sol_ov  = gw_zsdt0053-nro_sol_ov
                                                                   bezei       = c_frete_cif.

            READ TABLE gt_zsdt0059 INTO gw_zsdt0059_porto WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                                   bezei      = c_frete_porto.

            READ TABLE gt_zsdt0059 INTO gw_zsdt0059_fobs  WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                                   bezei      = c_fobs.

            READ TABLE gt_zsdt0059 INTO gw_zsdt0059_frame  WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                                    bezei      = c_cambio_frame.

            " 22.08.2023 - Ramon -->
*            READ TABLE gt_conv_bio INTO ls_conv_bio WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov. " 09.09.2023 - bug 150168 - comentado pois iremos utilizar o metodo do conversor
            " 22.08.2023 - Ramon --<

            FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
            CREATE OBJECT gobj_zcl_taxa_curva.
            CREATE OBJECT gobj_zcl_webservice_tx_curva.

            gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
            gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
            var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                      i_data_final   = gw_zsdt0051-dtate_logist ).

            data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
            gobj_zcl_taxa_curva->set_data_lib( sy-datum ).

           " 04.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
            CALL METHOD get_vlr_bio
            EXPORTING
              iv_matnr    = gw_zsdt0051-matnr
              iv_doc_simu = gw_zsdt0053-nro_sol_ov
            CHANGING
              cv_cadencia = gw_zsdt0053-zmeng.

            IF i_auart IN r_auart AND r_auart IS NOT INITIAL.
              gw_zsdt0053-zmeng = gw_zsdt0053-zmeng - soma.
              gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                  i_tcode    = i_tcode ).
            ELSE.
              gobj_zcl_taxa_curva->set_cadencia(  gw_zsdt0053-zmeng ).
            ENDIF.
                " 04.09.2023 - bug 150168 -

            gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).

            IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
              var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
            ELSE.
              IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                var_montante = var_montante * gw_zsdt0053-zmeng.
              ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                var_montante = var_montante * gw_zsdt0053-zmeng.
                " 22.08.2023 - Ramon -->
              ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
*                gw_zsdt0053-zmeng = gw_zsdt0053-zmeng * ls_conv_bio-formula2. " 04.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
                var_montante = ( var_montante * gw_zsdt0053-zmeng ).
              ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
*                gw_zsdt0053-zmeng = gw_zsdt0053-zmeng * ls_conv_bio-formula2. " 04.09.2023 - bug 150168 - quevedo --> tratativa devoluçaão e complemento venda frame bio
                var_montante = ( var_montante * gw_zsdt0053-zmeng ).
                " 22.08.2023 - Ramon --<
              ENDIF.
            ENDIF.

            var_montante_2 = var_montante .
            gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
            gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
            gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
            gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
            gobj_zcl_taxa_curva->set_posnr( gw_zsdt0053-fixacao ).

*            IF ls_conv_bio IS INITIAL.
*              var_montante_2 = var_montante .
*              gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
*            ELSE.
*              " 29.08.2023 -->
*              DATA lv_frete TYPE bezei60.
*
*              lv_frete = gobj_zcl_taxa_curva->get_frete_porto( ).
*
*              var_montante_2 = ( gw_zsdt0053-zmeng * lv_frete ) / 1000.
*
*              gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
*              " 29.08.2023 --<
*            ENDIF.


            CASE gw_zsdt0053-status.
              WHEN 'Y'.    gobj_zcl_taxa_curva->set_bezei( 'Y' ).
              WHEN 'W'.    gobj_zcl_taxa_curva->set_bezei( 'W' ).
              WHEN OTHERS. gobj_zcl_taxa_curva->set_bezei( gw_zsdt0059_frame-bezei ).
            ENDCASE.

            CLEAR: gw_zsdt0059_frame.
            READ TABLE gt_zsdt0059 INTO gw_zsdt0059_frame  WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                                    bezei      = c_cambio_frame
                                                                    field      = 'PRECO'.

            gobj_zcl_taxa_curva->set_tipo( c_frete ).
            gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode
                                                     i_status = gw_zsdt0053-status ).
            var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).

"movido para antes de calcular o valor total                 " 04.09.2023 - bug 150168 -
*            IF i_auart IN r_auart AND r_auart IS NOT INITIAL.
*              gw_zsdt0053-zmeng = gw_zsdt0053-zmeng - soma.
*              gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
*                                                  i_tcode    = i_tcode ).
*            ELSE.
*              gobj_zcl_taxa_curva->set_cadencia(  gw_zsdt0053-zmeng ).
*            ENDIF.

*            CASE I_AUART.
*              WHEN 'ZCPV' OR 'ZCOP' OR 'ZCFX'.
*                GW_ZSDT0053-ZMENG = GW_ZSDT0053-ZMENG - SOMA.
*            ENDCASE.

*            gobj_zcl_taxa_curva->set_cadencia(  gw_zsdt0053-zmeng ).

            var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc " VAR_DATA_CALCULADA
                                                                     i_data_lib = sy-datum
                                                                     i_tipo     = var_tipo
                                                                     ).

            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
            var_taxa_cambio  = gw_zsdt0059_frame-formula2.
            gobj_zcl_taxa_curva->set_taxa_cambio( var_taxa_cambio ).
            gobj_zcl_taxa_curva->set_vbeln( i_vbeln ).

            var_safra = gw_zsdt0053-charg.
            gobj_zcl_taxa_curva->set_safra( var_safra ).
            me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)
            CLEAR: gw_zsdt0053, gw_zsdt0051, gw_zsdt0059_cif, gw_zsdt0059_fobs, gw_zsdt0059_porto, var_cotacao, var_montante, var_montante_2, var_taxa_cambio.
          ENDLOOP.

          "Método para disparar o frame parcial, caso tenha diferença na quantidade.
          me->frame_parcial( i_numero  = i_numero
                             i_fixacao = i_fixacao
                             i_tcode   = i_tcode
                             i_ucomm   = i_ucomm
                             ).

        ENDIF.
      ENDIF.
    ENDIF.

    me->update_job(
                    tcode      = i_tcode
                    nro_sol_ov = i_numero
                    vl_vbeln   = i_vbeln
                  ).

  ENDMETHOD.


  METHOD frete_aqv.

    CONSTANTS: venda        TYPE c VALUE 'V',
               compra       TYPE c VALUE 'C',
               kurst        TYPE c VALUE 'B',
               waerk        TYPE c LENGTH 3 VALUE 'USD',
               tcurr        TYPE c LENGTH 3 VALUE 'BRL',
               intercompany TYPE c LENGTH 4 VALUE 'ZCIC'.

    DATA: werks              TYPE werks_d,
          var_data           TYPE datum,
          zdata              TYPE datum,
          var_data_calculada TYPE d,
          var_mes            TYPE n LENGTH 2,
          var_mes_aux        TYPE c LENGTH 2,
          var_ano            TYPE c LENGTH 4,
          var_w              TYPE char1.

    DATA(obj_tx_curva)    = NEW zcl_taxa_curva( ).
    DATA(obj_tx_curva_db) = NEW zcl_taxa_curva_db( ).
    DATA(obj_w_tx_curva)  = NEW zcl_webservice_tx_curva( ).
    DATA(obj_util_sd)     = NEW zcl_util_sd( ).

    DATA(r_emp) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_EX_EMP' ).    "// Empresa que não Dispara ZCIC

    IF _vbrk-vkorg IN r_emp AND r_emp IS NOT INITIAL.
      DATA(r_exc) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_EX_ZCIC' ). "// Grupo de Contas de Cliente
      DATA(r_cli) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_EX_CLI' ).  "// Exceção de Cliente que não dispara o Hedge
      DATA(r_cl2) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_EX_CL2' ).  "// Exceção de Cliente que dispara a segunda perna do Hedge
    ELSE.
      r_exc = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_EXCECAO' ).       "// Grupo de Contas de Cliente
    ENDIF.

    SELECT COUNT(*)
      FROM zsdt0094
      WHERE nro_sol_ov EQ _vbrk-vbeln
        AND estorno    EQ abap_false
        AND tipo_taxa  EQ compra.

    CHECK sy-subrc IS NOT INITIAL.

*   "// Verifica se a Empresa esta na Exceção
    IF _vbrk-vkorg IN r_emp AND r_emp IS NOT INITIAL.
      IF r_cli IS NOT INITIAL.
*   "// Não dispara se a Cliente esta na Exceção
        CHECK NOT _vbrk-kunag IN r_cli.
      ENDIF.
    ENDIF.

*   "// Inicio
*   "// Verifica se o Cliente é Intercompany
*   "// se for ZCIC não será disparado nenhuma perna do Hedge.
    SELECT SINGLE *
      FROM kna1
      INTO @DATA(wa_kna1)
      WHERE kunnr EQ @_vbrk-kunag.

    SELECT SINGLE *
      FROM t052
      INTO @DATA(wa_t052)
      WHERE zterm EQ @_vbrk-zterm.

*    IF SY-SUBRC IS INITIAL.
*     CHECK WA_KNA1-KTOKD NE INTERCOMPANY.
*    ENDIF.
*   "// Fim

*   "// Não dispara se a Grupo de Contas de Cliente esta na Exceção
    IF r_exc IS NOT INITIAL.
      CHECK NOT wa_kna1-ktokd IN r_exc.
    ENDIF.

    obj_util_sd->set_data(  CONV #( sy-datum ) ).
    obj_util_sd->set_kurst( CONV #( kurst ) ).
    obj_util_sd->set_waerk( CONV #( waerk ) ).
    obj_util_sd->set_tcurr( CONV #( tcurr ) ).

*   "// Lançamento do Frete
    obj_tx_curva->set_numero( _vbrk-vbeln ).

    "//Verifica o tipo da condição de pagamento para fazer o cálculo da data de Vencimento
    IF _vbrk-valdt IS INITIAL.
      zdata = sy-datum.
    ELSE.
      zdata = _vbrk-valdt.
    ENDIF.

    IF wa_t052-zdart EQ 'B'.
      IF NOT ( wa_t052-ztag1 IS INITIAL ).
        var_data_calculada = zdata + wa_t052-ztag1.
      ELSE.
        var_mes = ( (  zdata+4(2) ) + wa_t052-zmona ).
        IF ( var_mes > 12 ).
          var_mes_aux =  wa_t052-zmona.
          var_ano = zdata(4) + 1.
        ELSE.
          var_mes_aux = var_mes.
          var_ano     = zdata(4).
        ENDIF.
        CONCATENATE var_ano var_mes_aux wa_t052-zfael INTO var_data_calculada.
      ENDIF.

      var_w = 0.

      WHILE var_w = 0.
        "Verifica se a data de vencimento é dia útil, caso não seja o sistema localiza a proxima data útil para ser o Vencimento.
        obj_tx_curva_db->dia_util( EXPORTING p_vencimento  = var_data_calculada
                                   IMPORTING e_subrc       = DATA(s_subrc)
                                  ).
        IF s_subrc IS INITIAL.
          var_data_calculada = var_data_calculada + 1.
        ELSE.
          var_w = 1.
        ENDIF.
      ENDWHILE.

      obj_tx_curva->set_data_venc( var_data_calculada ).

    ELSE.
      obj_tx_curva->set_data_venc( zdata ).
    ENDIF.

    obj_tx_curva->set_data_lib( _vbrk-fkdat ).

    obj_tx_curva->set_cadencia( CONV #( _vbrp-ntgew ) ).

    obj_tx_curva->set_zieme( _vbrp-gewei ).

    obj_tx_curva->set_total_proporcional( CONV #( ( _vbrk-netwr + _vbrk-mwsbk ) ) ).

    obj_tx_curva->set_tipo( i_tipo = obj_tx_curva->get_tipo_auart( vbrk = _vbrk auart = _auart ) ).

    obj_tx_curva->set_bezei( CONV #( _vbrk-vkorg ) ).
    obj_tx_curva->set_tipo_taxa( compra ).
    obj_tx_curva->set_vbeln( _vbrp-aubel ).
*    OBJ_TX_CURVA->SET_INCOTERMS( _VBRK-INCO1 ).

    obj_tx_curva->set_taxa_curva(
        obj_w_tx_curva->buscar_taxa(
                                     i_data     = obj_tx_curva->get_data_venc( )
                                     i_data_lib = obj_tx_curva->get_data_lib( )
                                     i_tipo     = obj_tx_curva->get_tipo_taxa( )
                                   )
                                ).

*   "// Taxa cambio é a ultima taxa cadastrada na TCURR
    obj_tx_curva->set_tx_cambio_aqv(
                                     i_taxa   = obj_util_sd->taxa_cambio( )
                                     i_vbeln  = obj_tx_curva->get_vbeln( )
                                     i_data   = obj_tx_curva->get_data_venc( )
                                     i_tipo   = obj_tx_curva->get_tipo_taxa( )
                                    ).

    IF _vbrk-vkorg IN r_emp AND r_emp IS NOT INITIAL.
      IF r_cl2 IS NOT INITIAL.
        IF _vbrk-kunag IN r_cl2.
          obj_tx_curva->set_intercompany( abap_true ).
        ENDIF.
      ENDIF.

      IF wa_kna1-ktokd EQ intercompany.
        obj_tx_curva->set_intercompany( abap_true ).
      ENDIF.

    ELSE.
      IF wa_kna1-ktokd EQ intercompany.
        obj_tx_curva->set_intercompany( abap_true ).
      ENDIF.
    ENDIF.

    obj_tx_curva_db->zif_taxa_curva_db~inserir_in( obj_tx_curva ).


    SELECT COUNT(*)
      FROM zsdt0094
      WHERE nro_sol_ov EQ _vbrk-vbeln
        AND estorno    EQ abap_false
        AND tipo_taxa  EQ venda.

    CHECK sy-subrc IS NOT INITIAL.

*     "##############################################// Disparo do Cliente quando For ZCIC "INTERCOMPANY".
*    SELECT SINGLE *
*      FROM KNA1
*      INTO @DATA(WA_KNA1)
*      WHERE KUNNR EQ @_VBRK-KUNAG.
*
*    CHECK SY-SUBRC IS INITIAL.

*   "// Verifica se o Cliente esta na Exceção de clientes que são intercompany para disparar o Hedge

    IF _vbrk-vkorg IN r_emp AND r_emp IS NOT INITIAL.
      IF wa_kna1-ktokd EQ intercompany.
        "// Verifica se o Cliente é Intercompany
        CHECK wa_kna1-ktokd EQ intercompany.
      ELSE.
        "// Verifica se é um cliente comum, mas para essa empresa(R_EMP) é tratado como Cliente Intercompany.
        CHECK r_cl2 IS NOT INITIAL.
        CHECK _vbrk-kunag IN r_cl2.
      ENDIF.
    ELSE.
*     "// Verifica se o Cliente é Intercompany
      CHECK wa_kna1-ktokd EQ intercompany.
    ENDIF.

    obj_tx_curva->set_cadencia( i_cadencia = CONV #( _vbrp-ntgew )
                                i_negativa = abap_true ).

    obj_tx_curva->set_total_proporcional( i_total = CONV #( _vbrk-netwr + _vbrk-mwsbk )
                                          i_negativa = abap_true ).

    "// Buscar qual é a empresa do cliente
    werks =  |{ _vbrk-kunag ALPHA = IN }|.

    SELECT SINGLE *
      FROM t001w
      INTO @DATA(wa_t001w)
      WHERE werks EQ @werks.

*    OBJ_TX_CURVA->SET_BEZEI( CONV #( _VBRK-KUNAG ) ).
    obj_tx_curva->set_bezei( CONV #( wa_t001w-vkorg ) ).


    obj_tx_curva->set_tipo_taxa( venda ).

    obj_tx_curva->set_taxa_curva(
        obj_w_tx_curva->buscar_taxa(
                                     i_data     = obj_tx_curva->get_data_venc( )
                                     i_data_lib = obj_tx_curva->get_data_lib( )
                                     i_tipo     = obj_tx_curva->get_tipo_taxa( )
                                   )
                                ).

*   "// Taxa cambio é o Primeiro disparo da taxa Curva
*    OBJ_TX_CURVA->SET_TX_CAMBIO_AQV( I_TAXA   = ''
*                                   I_VBELN  = OBJ_TX_CURVA->GET_VBELN( )
*                                   I_DATA   = OBJ_TX_CURVA->GET_DATA_VENC( )
*                                   I_TIPO   = OBJ_TX_CURVA->GET_TIPO_TAXA( ) ).

*    OBJ_TX_CURVA->SET_TX_CAMBIO_AQV(
*                                     I_TAXA   = OBJ_UTIL_SD->TAXA_CAMBIO( )
*                                     I_VBELN  = OBJ_TX_CURVA->GET_VBELN( )
*                                     I_DATA   = OBJ_TX_CURVA->GET_DATA_VENC( )
*                                     I_TIPO   = OBJ_TX_CURVA->GET_TIPO_TAXA( )
*                                    ).

    obj_tx_curva_db->zif_taxa_curva_db~inserir_in( obj_tx_curva ).


  ENDMETHOD.


  METHOD frete_in.

******************************************************************************
**** TABELA ZSDT0041 COM MATKL PARA AGRUPAR OS DADOS PELO GRUPO DE MERCADORIA
******************************************************************************
    TYPES BEGIN OF ty_0041.
    INCLUDE TYPE zsdt0041.
    TYPES matkl TYPE matkl.
    TYPES brgew TYPE brgew.
*---> S4 MIGRATION 10/07/2023 - MA
*    TYPES dtpgtcult TYPE bapi_jbd_dte_dzterm.
    TYPES dtpgtcult TYPE valdt.
*<--- S4 MIGRATION 10/07/2023 - MA

    TYPES kursk TYPE kursk.
    TYPES END OF ty_0041.

    DATA: cont TYPE i.

***************************************************
**** OBJ DA TAXA CAMBIO E CURVA E OS METODOS DA 94
***************************************************
    DATA: obj_tx_curva TYPE REF TO zcl_webservice_tx_curva,
          obj_insere   TYPE REF TO zcl_taxa_curva_db,
          obj_0094     TYPE REF TO zcl_taxa_curva.

**************************************
****  TABELAS INTERNAS E WORK AREAS
**************************************
    DATA: it_0041        TYPE TABLE OF ty_0041,
          wa_0040        TYPE zsdt0040,
          wa_0041        TYPE zsdt0041,
          wa_0037        TYPE zsdt0037,
          var_total      TYPE dmbtr,
          set_porc_frete TYPE p DECIMALS 5,
          tipo           TYPE char3,
          sequencia      TYPE posnr,
          vbeln          TYPE zsdt0090-vbeln,
          matnr          TYPE zsdt0090-matnr,
          vlr_frete      TYPE zsdt0037-vlr_frete,
          um_frete       TYPE zsdt0037-meins,
          vlr_frete_v    TYPE zsdt0037-vlr_frete,
          um_frete_v     TYPE zsdt0037-meins,
          lv_hedge       TYPE xfeld.

    DATA: it_set TYPE TABLE OF rgsb4,
          wa_set TYPE rgsb4.

**********************
****** LIBERA OS OBJ
**********************
    FREE:      obj_tx_curva, obj_0094, obj_insere.

**********************
****** CRIA OS OBJ
**********************
    CREATE OBJECT: obj_tx_curva, obj_0094, obj_insere.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class           = '0000'
        setnr           = 'MAGGI_FRI_HEDGE'
        no_descriptions = space
        no_rw_info      = space
      TABLES
        set_values      = it_set
      EXCEPTIONS
        set_not_found   = 1
        OTHERS          = 2.

    IF sy-subrc IS INITIAL.

      wa_set = it_set[ 1 ].

      CALL FUNCTION 'MOVE_CHAR_TO_NUM'
        EXPORTING
          chr             = wa_set-from
        IMPORTING
          num             = set_porc_frete
        EXCEPTIONS
          convt_no_number = 1
          convt_overflow  = 2
          OTHERS          = 3.

      set_porc_frete = set_porc_frete / 100.

    ENDIF.

    CASE sy-cprog.
      WHEN 'ZSDR016'.

*****************************
***** CABEÇARIO DA SIMULAÇÃO
*****************************
        SELECT SINGLE * FROM zsdt0040
          INTO wa_0040
          WHERE doc_simulacao EQ i_numero.

        SELECT SINGLE * FROM zsdt0117
          INTO @DATA(wl_0117)
          WHERE bukrs      EQ @wa_0040-vkorg
            AND desativado EQ @abap_false.

        wa_0040-dt_entrega_sem = wa_0040-dt_entrega_sem + 30.
        wa_0040-dt_entrega_def = wa_0040-dt_entrega_def + 30.
        wa_0040-dt_entrega_fet = wa_0040-dt_entrega_fet + 30.

**********************************************************************************
* MONTA A ESTRUTURA COM OS DADOS AGRUPADOS SOMADOS E DIVIDIDOS PELO INCO1 E MATKL
**********************************************************************************
        obj_0094->agrupa_dados( EXPORTING i_numero = i_numero
                                i_tipo   = i_tipo
                                t_itens  = t_itens
                                IMPORTING i_0041 = it_0041
                                          e_hedge = lv_hedge " INS IR246304
                               ).
        tipo = i_tipo.
        LOOP AT it_0041 ASSIGNING FIELD-SYMBOL(<final>).

          obj_0094->set_numero( <final>-doc_simulacao ).

          obj_0094->set_matkl( i_matkl = <final>-matkl
                               i_brgew = <final>-brgew
                             ).

          IF i_acao EQ 'DSC_ABS'.
            <final>-zmeng = 0.
            var_total = 0.
            tipo = 'DES'.
          ELSE.
            IF <final>-spart EQ '03'
** Inicio IR246304 - Ins Setor atividade para Iconterms CPT
            OR lv_hedge  = abap_true.
** Fom IR246304 - Ins Setor atividade para Iconterms CPT
              IF ( wa_0040-waerk = 'USD' ) AND ( wl_0117-kursk IS NOT INITIAL ).
                var_total = ( <final>-vlrtot * set_porc_frete ) * wl_0117-kursk.
              ELSE.
                var_total = <final>-vlrtot * set_porc_frete.
              ENDIF.

            ELSE.
              var_total = <final>-vlrtot.
            ENDIF.


          ENDIF.

          obj_0094->set_bezei( '' ).
          obj_0094->set_cadencia_in( i_cadencia =  <final>-zmeng
                                     i_negativa = 'S'
                                     i_0040 = wa_0040
                                   ).
          obj_0094->set_data_lib( sy-datum ).
          obj_0094->set_total_proporcional( i_total =  var_total
                                            i_negativa = abap_true
                                           ).
          obj_0094->tipo_taxa_in( tipo ).
          obj_0094->set_taxa_curva(
          obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                     i_data_lib = obj_0094->get_data_lib(  )
                                     i_tipo     = obj_0094->get_tipo_taxa( )
                                   ) ).
          obj_0094->set_taxa_cambio( <final>-kursk ).

          " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
          IF i_taxa_boleta IS NOT INITIAL.
            obj_0094->set_taxa_cambio( i_taxa_boleta ).
          ENDIF.
          " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

          obj_0094->set_tipo( i_tipo ).
          obj_0094->set_incoterms( <final>-inco1 ).
          obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).

        ENDLOOP.

**************************************************************
* DISPARO DA TRANSAÇÃO ZSDT0087
**************************************************************
      WHEN 'ZSDR0042'.

        CLEAR wa_0037.

        SELECT SINGLE * FROM zsdt0040
          INTO wa_0040
          WHERE doc_simulacao EQ i_0090-doc_simulacao.

        wa_0040-dt_entrega_sem = wa_0040-dt_entrega_sem + 30.
        wa_0040-dt_entrega_def = wa_0040-dt_entrega_def + 30.
        wa_0040-dt_entrega_fet = wa_0040-dt_entrega_fet + 30.

* 19.03.18
*Com a Inclusão da opção de troca de Incoterms na ZSDT0087, se faz necessário pegar o tipo de frete atual da OV.
*Logo não se faz necessário a chamada abaixo.
*        DATA(WA_0041) =
*        OBJ_0094->GET_FIRTS_FRETE( I_DOC_SIMULACAO = I_0090-DOC_SIMULACAO
*                                   I_VBELN = I_0090-VBELV
*                                   I_MATNR = I_0090-MATNRV
*                                 ).

*        CASE WA_0041-INCO1.
*          WHEN 'CIF' OR 'CPT'.
*            IF WA_0041-INCO1 EQ 'CPT'.
*              CHECK WA_0041-SPART EQ '03'.
*            ENDIF.
*          WHEN OTHERS.
*            EXIT.
*        ENDCASE.
* 19.03.18

* localiza o valor do frete para disparo com base na OV Velha.
        SELECT SINGLE * FROM zsdt0041
          INTO wa_0041
           WHERE doc_simulacao EQ i_0090-doc_simulacao
            AND vbeln EQ i_0090-vbelv
            AND matnr EQ i_0090-matnrv.

        IF ( wa_0041 IS NOT INITIAL ) AND ( wa_0041-inco1 NE 'FOB' ).
          vlr_frete = wa_0041-vlr_frete.
          um_frete  = wa_0041-zieme.

        ELSE.
          CASE i_acao.
            WHEN 'ALTERAR' OR 'ENCERRAR' OR 'TROCA'.
              SELECT SINGLE *
                 FROM zsdt0037
                  INTO wa_0037
                   WHERE bukrs          EQ wa_0040-vkorg
                     AND matkl          EQ i_0090-matklv
                     AND filial_origem  EQ i_0090-werksv
                     AND meins          EQ i_0090-kmeinv
                     AND filial_destino EQ  wa_0040-vkbur
                     AND waers          EQ 'BRL'.

              vlr_frete = wa_0037-vlr_frete.
              um_frete  = wa_0037-meins.

          ENDCASE.
        ENDIF.

        IF i_acao EQ 'REDIST'.
          vlr_frete_v = vlr_frete.
          um_frete_v  = um_frete.
        ENDIF.

*        CASE I_0090-INCO1V.
*          WHEN 'CIF' OR 'CPT' OR 'CFR'.
*            IF ( I_0090-INCO1V EQ 'CPT' ) OR ( I_0090-INCO1V EQ 'CFR' ).
*              CHECK I_0090-SPARTV EQ '03'.
*            ENDIF.
*          WHEN OTHERS.
*            EXIT.
*        ENDCASE.

        CASE i_0090-inco1v.
          WHEN 'CIF' OR 'CPT' OR 'CFR'.
            IF ( i_0090-inco1v EQ 'CIF' ) OR
**Inicio IR246304 - 29/07/2025
** IR246304 - Ins Setor atividade para Iconterms CPT
**             ( i_0090-inco1v EQ 'CPT' AND i_0090-spartv EQ '03') OR
               ( i_0090-inco1v EQ 'CPT' AND
                ( i_0090-spartv EQ '03' OR i_0090-spartv EQ '02'
                 OR i_0090-spartv EQ '04' ) ) OR
****IR246304 - Ins Setor atividade para Iconterms CPT
               ( i_0090-inco1v EQ 'CFR' AND i_0090-spartv EQ '03').
*              CHECK I_0090-SPARTV EQ '03'.
*            ENDIF.
*              IF I_0090-SPARTV EQ '03'.
              SELECT SINGLE * FROM zsdt0117
                INTO @DATA(wa_0117)
                WHERE bukrs EQ @wa_0040-vkorg
                AND desativado EQ @abap_false.

              SELECT SINGLE * FROM mara
                INTO @DATA(wa_mara)
                WHERE matnr EQ @i_0090-matnrv.

              IF i_acao NE 'REDIST'.
                MOVE i_0090-sequencia TO sequencia.

                obj_0094->set_numero( i_0090-doc_simulacao ).
                obj_0094->set_posnr( sequencia ).
                obj_0094->set_matkl( i_matkl = wa_mara-matkl
                                     i_brgew = wa_mara-brgew
                                    ).
                obj_0094->set_netpr( i_netpr = i_0090-netprv
                                     i_kmein = i_0090-kmeinv ).
                obj_0094->set_zieme( i_0090-ziemev ).
                obj_0094->set_bezei( '' ).


                CASE i_acao.
                  WHEN 'ALTERAR'.
                    obj_0094->set_cadencia_in( i_cadencia =  i_0090-zmengv ).
                    obj_0094->set_cadencia_in( i_cadencia =  obj_0094->get_cadencia( )
                                               i_negativa = 'S'
                                               i_0040 = wa_0040
                                              ).

                    IF wa_mara-matkl EQ '658445'.
                      obj_0094->set_zieme( i_0090-ziemev ).
                    ENDIF.

                    obj_0094->tipo_taxa_in( 'AQT' ).

*              OBJ_0094->SET_FRETE_IN( I_FRETE = WA_0041-VLR_FRETE
*                                      I_ZIEME = WA_0041-ZIEME
*                                     ).

                    obj_0094->set_frete_in( i_frete = vlr_frete
                                            i_zieme = um_frete
                                           ).

                    IF i_0090-spartv EQ '03'.

                      IF ( wa_0040-waerk = 'USD' ) AND ( wa_0117-kursk IS NOT INITIAL ).
                        var_total = ( ( ( i_0090-zmengv * i_0090-netprv ) * set_porc_frete ) * -1 ) * wa_0117-kursk .
                      ELSE.
                        var_total = ( ( i_0090-zmengv * i_0090-netprv ) * set_porc_frete ) * -1.
                      ENDIF.
                    ELSE.
                      var_total = obj_0094->get_total_proporcional( ).
                    ENDIF.

                    obj_0094->set_total_proporcional( var_total ).


                  WHEN 'ESTORNAR'.
                  WHEN 'TROCA'.
                    obj_0094->set_cadencia_in( i_cadencia =  i_0090-zmengv ).
                    obj_0094->set_cadencia_in( i_cadencia =  obj_0094->get_cadencia( )
                                               i_negativa = 'S'
                                               i_0040 = wa_0040
                                              ).

*              OBJ_0094->SET_FRETE_IN( I_FRETE = WA_0041-VLR_FRETE
*                                      I_ZIEME = WA_0041-ZIEME
*                                     ).

                    obj_0094->set_frete_in( i_frete = vlr_frete
                                            i_zieme = um_frete
                                           ).

                    IF i_0090-spartv EQ '03'.

                      IF ( wa_0040-waerk = 'USD' ) AND ( wa_0117-kursk IS NOT INITIAL ).
                        var_total = ( ( ( i_0090-zmengv * i_0090-netprv ) * set_porc_frete ) * -1 ) * wa_0117-kursk.
                      ELSE.
                        var_total = ( ( i_0090-zmengv * i_0090-netprv ) * set_porc_frete ) * -1.
                      ENDIF.

                      obj_0094->set_total_proporcional( var_total ).
                    ENDIF.

                    obj_0094->set_tipo_taxa( 'C' ).

                  WHEN 'ENCERRAR'.
                    obj_0094->set_cadencia_in( i_cadencia =  i_0090-zmengv ).
                    obj_0094->set_cadencia_in( i_cadencia =  obj_0094->get_cadencia( )
                                               i_negativa = 'S'
                                               i_0040 = wa_0040
                                              ).
                    IF wa_mara-matkl EQ '658445'.
                      obj_0094->set_zieme( i_0090-ziemev ).
                    ENDIF.

*              OBJ_0094->SET_FRETE_IN( I_FRETE = WA_0041-VLR_FRETE
*                                      I_ZIEME = WA_0041-ZIEME
*                                     ).

                    obj_0094->set_frete_in( i_frete = vlr_frete
                                            i_zieme = um_frete
                                           ).

                    IF i_0090-spartv EQ '03'.

                      IF ( wa_0040-waerk = 'USD' ) AND ( wa_0117-kursk IS NOT INITIAL ).
                        var_total = ( ( ( i_0090-zmengv * i_0090-netprv ) * set_porc_frete ) * -1 ) * wa_0117-kursk.
                      ELSE.
                        var_total = ( ( i_0090-zmengv * i_0090-netprv ) * set_porc_frete ) * -1.
                      ENDIF.

                      obj_0094->set_total_proporcional( var_total ).
                    ENDIF.

                    obj_0094->set_tipo_taxa( 'C' ).

                  WHEN 'PRICE'.
                  WHEN 'REDIST'.
                  WHEN 'MDF_VENC'.

                    IF wa_mara-matkl EQ '658445'.

                      obj_0094->set_cadencia_in( i_cadencia =  i_0090-zmengv ).
                      obj_0094->set_cadencia_in( i_cadencia =  obj_0094->get_cadencia( )
                                                 i_negativa = 'S'
                                                 i_0040 = wa_0040
                                                ).

                      var_total = ( i_0090-desc_absoluto * set_porc_frete ) * -1.
                      obj_0094->set_total_proporcional( var_total ).
                      obj_0094->tipo_taxa_in( 'DES' ).
                    ENDIF.

                ENDCASE.

                obj_0094->set_data_lib( sy-datum ).
                obj_0094->set_taxa_curva(
                obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                           i_data_lib = obj_0094->get_data_lib(  )
                                           i_tipo     = obj_0094->get_tipo_taxa( )
                                         ) ).

                IF wa_0040-taxa_frete IS NOT INITIAL.
                  obj_0094->set_taxa_cambio( wa_0040-taxa_frete ).
                ELSE.
                  obj_0094->set_taxa_cambio( wa_0117-kursk ).
                ENDIF.

                obj_0094->set_tipo( i_tipo ).
                obj_0094->set_vbeln( i_0090-vbelv ).
                obj_0094->set_incoterms( i_0090-inco1v ).
                obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).
              ENDIF.
*              ENDIF.
            ENDIF.
          WHEN OTHERS.
            CHECK i_acao EQ 'TROCA'.
        ENDCASE.

* Parte Nova do Disparo da zsdt0090

        CHECK NOT i_0090-vbeln IS INITIAL.
        FREE: wa_0041, wa_0037, wa_0117, wa_mara.
        CLEAR: vlr_frete, um_frete.

*        WA_0041 =
*        OBJ_0094->GET_FIRTS_FRETE( I_DOC_SIMULACAO = I_0090-DOC_SIMULACAO
*                                   I_VBELN = I_0090-VBELV
*                                   I_MATNR = I_0090-MATNRV
*                                 ).

* localiza o valor do frete para disparo com base na OV Novo.
        SELECT SINGLE * FROM zsdt0041
          INTO wa_0041
           WHERE doc_simulacao EQ i_0090-doc_simulacao
            AND vbeln EQ i_0090-vbeln
            AND matnr EQ i_0090-matnr.

        IF ( wa_0041 IS NOT INITIAL ) AND ( wa_0041-inco1 NE 'FOB' ).
          vlr_frete = wa_0041-vlr_frete.
          um_frete  = wa_0041-zieme.

        ELSE.
          CASE i_acao.

            WHEN 'REDIST' OR 'TROCA'.

              SELECT SINGLE *
                 FROM zsdt0037
                  INTO wa_0037
                   WHERE bukrs          EQ wa_0040-vkorg
                     AND matkl          EQ i_0090-matkl
                     AND filial_origem  EQ i_0090-werks
                     AND meins          EQ i_0090-kmein
                     AND filial_destino EQ  wa_0040-vkbur
                     AND waers          EQ 'BRL'.

              vlr_frete = wa_0037-vlr_frete.
              um_frete  = wa_0037-meins.

          ENDCASE.
        ENDIF.


        CASE i_0090-inco1.
**Inicio IR246304 - 29/07/2025
*          WHEN 'CIF' OR 'CPT' OR 'CFR'.
*            IF ( i_0090-inco1 EQ 'CPT' ) OR ( i_0090-inco1 EQ 'CFR' ).
          WHEN 'CIF' OR 'CFR'.
            IF i_0090-inco1 EQ 'CFR'.
              CHECK i_0090-spart EQ '03'.
            ENDIF.

          WHEN 'CPT'.
            CHECK i_0090-spart EQ '03'
            OR    i_0090-spart EQ '02'
            OR    i_0090-spart EQ '04'.
****Fim IR246304 - 29/07/2025
          WHEN OTHERS.
            EXIT.
        ENDCASE.


*********
*        CASE WA_0041-INCO1.
*          WHEN 'CIF' OR 'CPT'.
*            IF WA_0041-INCO1 EQ 'CPT'.
*              CHECK WA_0041-SPART EQ '03'.
*            ENDIF.
*          WHEN OTHERS.
*            EXIT.
*        ENDCASE.

        SELECT SINGLE * FROM zsdt0117
          INTO wa_0117
          WHERE bukrs EQ wa_0040-vkorg
          AND desativado EQ abap_false.

        SELECT SINGLE * FROM mara
          INTO wa_mara
          WHERE matnr EQ i_0090-matnr.

        MOVE i_0090-sequencia TO sequencia.

        obj_0094->set_numero( i_0090-doc_simulacao ).
        obj_0094->set_posnr( sequencia ).
        obj_0094->set_matkl( i_matkl = wa_mara-matkl
                             i_brgew = wa_mara-brgew
                            ).
        obj_0094->set_bezei( '' ).
        obj_0094->set_tipo( i_tipo ).

        CASE i_acao.
          WHEN 'REDIST'.

            CLEAR wa_0037.
*           add a unidade de medida da quantidade antiga
            obj_0094->set_zieme( i_0090-ziemev ).
*           add a quantidade antiga para realizar a Conversão de acordo com as regras de Unidade de Medida
            obj_0094->set_cadencia_in( abs( i_0090-zmengv ) ).
*           calcula o Total proporcional do Item Antigo
*            OBJ_0094->SET_FRETE_IN( I_FRETE = WA_0041-VLR_FRETE
*                                    I_ZIEME = WA_0041-ZIEME ).
            obj_0094->set_frete_in( i_frete = vlr_frete_v
                                    i_zieme = um_frete_v
                                    ).


            IF i_0090-spartv EQ '03'.

              IF ( wa_0040-waerk = 'USD' ) AND ( wa_0117-kursk IS NOT INITIAL ).
                var_total = ( abs( i_0090-zmengv ) * i_0090-netprv ) * wa_0117-kursk.
              ELSE.
                var_total = abs( i_0090-zmengv ) * i_0090-netprv.
              ENDIF.

            ELSE.
*           armazena o total proporcional do Item Antigo na Variavel VAR_TOTAL
              var_total = obj_0094->get_total_proporcional( ).
            ENDIF.
*||||||||||||||||||||||||||||||||||||||||||||||||||||||
*VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
            IF i_0090-werks NE i_0090-werksv.
*            AND I_0090-MATKLV NE '658445'.

*              DATA: WA_0037 TYPE ZSDT0037.

*              SELECT SINGLE *
*              FROM ZSDT0037
*              INTO WA_0037
*              WHERE MEINS EQ I_0090-KMEIN
*                AND BUKRS EQ WA_0040-VKORG
*                AND MATKL EQ WA_MARA-MATKL
*                AND FILIAL_ORIGEM EQ I_0090-WERKS
*                AND FILIAL_DESTINO EQ  WA_0040-VKBUR.

*           add a unidade de medida da quantidade NOVA
*              OBJ_0094->SET_ZIEME( I_0090-ZIEME ).
*           add a quantidade NOVA para realizar a Conversão de acordo com as regras de Unidade de Medida
*              OBJ_0094->SET_CADENCIA_IN( ABS( I_0090-ZMENG ) ).
*           calcula o Total Proporcional do Item NOVO
*              OBJ_0094->SET_FRETE_IN( I_FRETE = WA_0037-VLR_FRETE
*                                      I_ZIEME = WA_0037-MEINS ).

*            ELSE.

*           add a unidade de medida da quantidade NOVA
              obj_0094->set_zieme( i_0090-zieme ).
*           add a quantidade NOVA para realizar a Conversão de acordo com as regras de Unidade de Medida
              obj_0094->set_cadencia_in( abs( i_0090-zmeng ) ).
*           calcula o Total Proporcional do Item NOVO
*              OBJ_0094->SET_FRETE_IN( I_FRETE = WA_0041-VLR_FRETE
*                                      I_ZIEME = WA_0041-ZIEME ).
              obj_0094->set_frete_in( i_frete = vlr_frete
                                      i_zieme = um_frete
                                     ).

            ENDIF.

            IF i_0090-spart EQ '03'.

              IF ( wa_0040-waerk = 'USD' ) AND ( wa_0117-kursk IS NOT INITIAL ).
                var_total = ( ( var_total - ( abs( i_0090-zmeng ) * i_0090-netpr ) ) * set_porc_frete ) * wa_0117-kursk.
              ELSE.
                var_total = ( var_total - ( abs( i_0090-zmeng ) * i_0090-netpr ) ) * set_porc_frete.
              ENDIF.

            ELSE.
*           calcula a diferença do Valor Antigo com o NOVO
              var_total = var_total - obj_0094->get_total_proporcional( ).
            ENDIF.

            obj_0094->set_total_proporcional( var_total ).
            obj_0094->set_cadencia_in( i_cadencia =  0
                                       i_negativa = 'S'
                                       i_0040 = wa_0040
                                      ).
            obj_0094->tipo_taxa_in( 'RED' ).

          WHEN 'TROCA'.

*           add a unidade de medida da quantidade Nova
            obj_0094->set_zieme( i_0090-zieme ).
*           add a quantidade antiga para realizar a Conversão de acordo com as regras de Unidade de Medida

            obj_0094->set_cadencia_in( i_cadencia =  i_0090-zmeng ).

            obj_0094->set_cadencia_in( i_cadencia =  obj_0094->get_cadencia( )
                                       i_negativa = 'S'
                                       i_0040 = wa_0040
                                      ).

*           calcula o Total Proporcional do Item NOVO
*            OBJ_0094->SET_FRETE_IN( I_FRETE = WA_0041-VLR_FRETE
*                                    I_ZIEME = WA_0041-ZIEME ).
            obj_0094->set_frete_in( i_frete = vlr_frete
                                     i_zieme = um_frete
                                    ).

            IF i_0090-spart EQ '03'.

              IF ( wa_0040-waerk = 'USD' ) AND ( wa_0117-kursk IS NOT INITIAL ).
                var_total = ( ( ( i_0090-zmeng * i_0090-netpr ) * set_porc_frete ) ) * wa_0117-kursk.
              ELSE.
                var_total = ( ( i_0090-zmeng * i_0090-netpr ) * set_porc_frete ).
              ENDIF.

              obj_0094->set_total_proporcional( i_total = var_total
                                                i_negativa = abap_true ).
            ELSE.
              obj_0094->set_total_proporcional( i_total = obj_0094->get_total_proporcional( )
                                                i_negativa = abap_true ).
            ENDIF.

            var_total = obj_0094->get_total_proporcional( ).
            var_total = abs( var_total ) * -1.
            obj_0094->set_total_proporcional( var_total ).

            obj_0094->set_tipo_taxa( 'V' ).

        ENDCASE.

        obj_0094->set_data_lib( sy-datum ).
        obj_0094->set_taxa_curva(
        obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                   i_data_lib = obj_0094->get_data_lib(  )
                                   i_tipo     = obj_0094->get_tipo_taxa( )
                                 ) ).


        IF wa_0040-taxa_frete IS NOT INITIAL.
          obj_0094->set_taxa_cambio( wa_0040-taxa_frete ).
        ELSE.
          obj_0094->set_taxa_cambio( wa_0117-kursk ).
        ENDIF.

        obj_0094->set_vbeln( i_0090-vbeln ).
        obj_0094->set_incoterms( i_0090-inco1 ).
        obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).


      WHEN 'SAPMV60A'. " VF01

        CLEAR: wa_0041, wa_0037, vlr_frete, um_frete.

        SELECT SINGLE * FROM zsdt0040
           INTO wa_0040
           WHERE doc_simulacao EQ i_0090-doc_simulacao.

        wa_0040-dt_entrega_sem = wa_0040-dt_entrega_sem + 30.
        wa_0040-dt_entrega_def = wa_0040-dt_entrega_def + 30.
        wa_0040-dt_entrega_fet = wa_0040-dt_entrega_fet + 30.

        CLEAR wa_0041.
        wa_0041 =
        obj_0094->get_firts_frete( i_doc_simulacao = i_0090-doc_simulacao
                                   i_vbeln = i_0090-vbelv
                                   i_matnr = i_0090-matnrv
                                 ).

        CASE wa_0041-inco1.
          WHEN 'CIF' OR 'CPT'.
**Inicio IR246304 - 29/07/2025
** Incluir Setor atividade = '02' Quando Incoterm = 'CPT'
            IF wa_0041-inco1 EQ 'CPT'.
**              CHECK wa_0041-spart EQ '03'.
              CHECK wa_0041-spart EQ '03'
              OR    wa_0041-spart EQ '02'
              OR    wa_0041-spart EQ '04'.
****Fim IR246304 - 29/07/2025
            ENDIF.
          WHEN OTHERS.
            EXIT.
        ENDCASE.

        SELECT SINGLE * FROM vbak
          INTO @DATA(wa_vbak)
          WHERE vbeln EQ @i_0090-vbeln.
***
        SELECT SINGLE *
        FROM zsdt0037
          INTO wa_0037
           WHERE bukrs          EQ wa_0040-vkorg
             AND matkl          EQ i_0090-matklv
             AND filial_origem  EQ i_0090-werksv
             AND meins          EQ i_0090-kmeinv
             AND filial_destino EQ wa_0040-vkbur
             AND waers          EQ 'BRL'
             AND val_de         LE sy-datum
             AND val_ate        GE sy-datum.

        vlr_frete = wa_0037-vlr_frete.
        um_frete  = wa_0037-meins.
***
        SELECT SINGLE * FROM zsdt0117
          INTO wa_0117
          WHERE bukrs EQ wa_0040-vkorg
          AND desativado EQ abap_false.

        SELECT SINGLE * FROM mara
          INTO wa_mara
          WHERE matnr EQ i_0090-matnrv.

        MOVE i_0090-sequencia TO sequencia.

        obj_0094->set_numero( i_numero = i_0090-doc_simulacao
                              i_tipo   = 'IN' "Insumos
                             ).

        obj_0094->set_posnr( sequencia ).
        obj_0094->set_matkl( i_matkl = wa_mara-matkl
                             i_brgew = wa_mara-brgew
                            ).
        obj_0094->set_zieme( i_0090-zieme ).
        obj_0094->set_bezei( '' ).
        obj_0094->set_tipo( i_tipo ).

        IF NOT i_dir IS INITIAL.
          obj_0094->set_bezei( i_dir ).
        ENDIF.

        obj_0094->set_cadencia_in( i_0090-zmeng ).
        obj_0094->set_cadencia_in( i_cadencia =  obj_0094->get_cadencia( )
                                   i_negativa = 'S'
                                   i_0040 = wa_0040
                                  ).

        IF NOT i_dir IS INITIAL.
          obj_0094->set_frete_in( i_frete = vlr_frete
                                  i_zieme = um_frete ).
        ELSE.
          obj_0094->set_frete_in( i_frete = wa_0041-vlr_frete
                                  i_zieme = wa_0041-zieme ).
        ENDIF.

        IF i_0090-spart EQ '03'.

          IF ( wa_0040-waerk = 'USD' ) AND ( wa_0117-kursk IS NOT INITIAL ).
            var_total = ( ( ( i_0090-zmeng * i_0090-netpr ) * set_porc_frete ) * -1 ) * wa_0117-kursk.
          ELSE.
            var_total = ( ( i_0090-zmeng * i_0090-netpr ) * set_porc_frete ) * -1.
          ENDIF.

          obj_0094->set_total_proporcional( i_total = var_total ).
        ENDIF.


        CASE i_dir.
          WHEN 'Y'. obj_0094->set_tipo_taxa( 'C' ).
          WHEN 'W'. obj_0094->set_tipo_taxa( 'V' ).
        ENDCASE.

        obj_0094->set_data_lib( sy-datum ).
        obj_0094->set_taxa_curva(
        obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                   i_data_lib = obj_0094->get_data_lib(  )
                                   i_tipo     = obj_0094->get_tipo_taxa( )
                                 ) ).

        IF wa_0040-taxa_frete IS NOT INITIAL.
          obj_0094->set_taxa_cambio( wa_0040-taxa_frete ).
        ELSE.
          obj_0094->set_taxa_cambio( wa_0117-kursk ).
        ENDIF.

        obj_0094->set_vbeln( i_0090-vbeln ).
        obj_0094->set_incoterms( i_0090-inco1 ).
        obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).

    ENDCASE.

    IF sy-cprog EQ 'ZSDR016'.
      UPDATE zsdt0040
                  SET job = abap_true
             WHERE doc_simulacao EQ i_numero.
    ENDIF.

  ENDMETHOD.


  METHOD frete_pedido.

    CONSTANTS: venda  TYPE c VALUE 'V',
               compra TYPE c VALUE 'C'.

    DATA: var_data_calculada TYPE d,
          _zsdt0094          TYPE zsdt0094.

    DATA(obj_tx_curva)    = NEW zcl_taxa_curva( ).
    DATA(obj_tx_curva_db) = NEW zcl_taxa_curva_db( ).
    DATA(obj_w_tx_curva)  = NEW zcl_webservice_tx_curva( ).
*
    CLEAR: _zsdt0094.

    CHECK _ekko-zterm NE 'C003'. "FF #168911
    CHECK _ekko-zterm NE 'I006'.
    CHECK _ekko-zterm NE 'I007'.
    CHECK _ekko-waers EQ 'BRL'.

    SELECT SINGLE *
             FROM  zsdt0094
             INTO _zsdt0094
            WHERE nro_sol_ov = _ekko-ebeln.

    IF _diferenca > 0.
      obj_tx_curva->set_tipo_taxa( venda ).
    ELSE.
      obj_tx_curva->set_tipo_taxa( compra ).
    ENDIF.

    obj_tx_curva->set_numero( _ekko-ebeln ).

    IF _ekko-ihran < sy-datum.
      var_data_calculada = sy-datum + 30.
      obj_tx_curva->set_data_venc( var_data_calculada ).
    ELSE.
      obj_tx_curva->set_data_venc( _ekko-ihran ).
    ENDIF.

    obj_tx_curva->set_data_lib( sy-datum ).
    obj_tx_curva->set_cadencia( 0 ).
    obj_tx_curva->set_zieme( _ekpo-meins ).
    obj_tx_curva->set_total_proporcional( i_total = _diferenca ).

    obj_tx_curva->set_taxa_curva(
        obj_w_tx_curva->buscar_taxa(
                                     i_data     = obj_tx_curva->get_data_venc( ) "_zsdt0094-data_venc
                                     i_data_lib = obj_tx_curva->get_data_lib( )  "_zsdt0094-data_lib
                                     i_tipo     = obj_tx_curva->get_tipo_taxa( ) "_zsdt0094-tipo_taxa
                                   )
                                ).

    IF _zsdt0094-taxa_curva IS NOT INITIAL.
      obj_tx_curva->set_taxa_cambio( _zsdt0094-taxa_curva ).
    ELSE.
      obj_tx_curva->set_taxa_cambio( obj_tx_curva->get_taxa_curva( ) ).
    ENDIF.

    obj_tx_curva->set_tipo( 'PDI' ).
    obj_tx_curva->set_vbeln( _ekko-ebeln ).
    obj_tx_curva->set_incoterms( _ekko-inco1 ).
    obj_tx_curva_db->zif_taxa_curva_db~inserir_in( obj_tx_curva ).

  ENDMETHOD.


  METHOD get_porcentagem_frete.

    DATA: _set TYPE TABLE OF rgsb4.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class           = '0000'
        setnr           = 'MAGGI_FRI_HEDGE'
        no_descriptions = abap_false
        no_rw_info      = abap_false
      TABLES
        set_values      = _set
      EXCEPTIONS
        set_not_found   = 1
        OTHERS          = 2.

    CHECK sy-subrc IS INITIAL.

    TRY .

        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr             = _set[ 1 ]-from
          IMPORTING
            num             = porc_frete
          EXCEPTIONS
            convt_no_number = 1
            convt_overflow  = 2
            OTHERS          = 3.

        DIVIDE porc_frete BY 100.

      CATCH: cx_sy_itab_line_not_found.
    ENDTRY.


  ENDMETHOD.


  METHOD get_totais.

    me->free( ).

    me->total_53( i_nro_sol_ov ).
    e_total_53 = me->at_total_53.

    me->total_peso_53( i_nro_sol_ov ).
    e_total_peso_53 = me->at_total_peso_53.

    me->total_54( i_nro_sol_ov ).
    e_total_54 = me->at_total_54.

    me->total_55( i_nro_sol_ov ).
    e_total_55 = me->at_total_55.

*    CHECK e_total_53 IS NOT INITIAL.
*    CHECK e_total_54 IS NOT INITIAL.
*
*    me->indice( i_total_53 = e_total_53
*                i_total_54 = e_total_54
*               ).
*    e_indice = me->at_indice.

  ENDMETHOD.


  METHOD get_vlr_bio.

    IF at_bio_tab IS INITIAL.

      SELECT * FROM zsdt0059
       INTO TABLE at_bio_tab
        WHERE nro_sol_ov  EQ iv_doc_simu
          AND bezei       EQ 'CONVERSOR BIO'.

    ENDIF.

    IF at_matkl IS INITIAL.

      SELECT SINGLE matkl FROM mara
       INTO at_matkl
         WHERE matnr = iv_matnr.

    ENDIF.

    IF at_matkl = '700400' AND at_matkl IS NOT INITIAL.

      READ TABLE at_bio_tab ASSIGNING FIELD-SYMBOL(<fs_bio>)
        WITH KEY nro_sol_ov = iv_doc_simu.

      CHECK sy-subrc EQ 0.

*--------------Inicio BUG 150168 / PQ
     IF IV_ACAO = 'R'. "faz a reversão para a quantidade cheia, de uma quantidade que já está aplicado o conversor
       cv_cadencia = cv_cadencia / <fs_bio>-formula2.
     else.
      cv_cadencia = cv_cadencia * <fs_bio>-formula2.
     endif.
    ENDIF.
*--------------Fim BUG 150168 / PQ
  ENDMETHOD.


  METHOD INDICE.

    ME->AT_INDICE = ( ( I_TOTAL_53 - I_TOTAL_54 ) / I_TOTAL_53 ).

  ENDMETHOD.


  METHOD liberar_ov.

    CONSTANTS:
      c_frete_cif   TYPE c LENGTH 11 VALUE 'FRETE CIF',
      c_frete_porto TYPE c LENGTH 13 VALUE 'FRETE PORTO',
      c_fobs        TYPE c LENGTH 4  VALUE 'FOBS',
      c_venda       TYPE c LENGTH 3  VALUE 'VDA',
      c_frete       TYPE c LENGTH 3  VALUE 'FRE',
      c_taxa_cambio TYPE c LENGTH 11 VALUE 'TAXA CAMBIO',
      c_conv_bio    TYPE c LENGTH 13 VALUE 'CONVERSOR BIO'.

    DATA:
      gt_zsdt0051 TYPE TABLE OF zsdt0051, "Tabela de Solicitação Ordem de Venda - Cabeçalho
      gt_zsdt0052 TYPE TABLE OF zsdt0052, "Tabela de Solicitação Ordem de Venda – COND_PGTO
      gt_zsdt0053 TYPE TABLE OF zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
      gt_zsdt0054 TYPE TABLE OF zsdt0054, "Tabela de Solicitação Ordem de Venda – Pagamento Antecipado
      gt_zsdt0055 TYPE TABLE OF zsdt0055, "Tabela de Solicitação Ordem de Venda – Logistica
      gt_zsdt0059 TYPE TABLE OF zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gt_zsdt0069 TYPE TABLE OF zsdt0069, "Solicitação de Ordem de Venda – Histórico Aprovações
      gt_zsdt0162 TYPE TABLE OF zsdt0162, "Solicitação de Ordem de Venda – Histórico Aprovações
      gt_zsdt0073 TYPE TABLE OF zsdt0073, "Tabela condições de pagamento
      gt_t052     TYPE TABLE OF t052.     "Condições de pagamento

    DATA:
      gw_zsdt0051             TYPE zsdt0051, "Tabela de Solicitação Ordem de Venda - Cabeçalho
      gw_zsdt0052             TYPE zsdt0052, "Tabela de Solicitação Ordem de Venda – COND_PGTO
      gw_zsdt0053             TYPE zsdt0053, "Tabela de Solicitação de ordem de venda - MATERIAIS
      gw_zsdt0054             TYPE zsdt0054, "Tabela de Solicitação Ordem de Venda – Pagamento Antecipado
      gw_zsdt0055             TYPE zsdt0055, "Tabela de Solicitação Ordem de Venda – Logistica
      gw_zsdt0069             TYPE zsdt0069, "Solicitação de Ordem de Venda – Histórico Aprovações
      gw_zsdt0059_porto       TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0059_cif         TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0059_fobs        TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0059_taxa_cambio TYPE zsdt0059, "Tabela de Solicitação Ordem de Venda – PRECO
      gw_zsdt0073             TYPE zsdt0073, "Tabela condições de pagamento
      gw_t052                 TYPE t052.     "Condições de pagamento

    DATA:
      gobj_zcl_taxa_curva          TYPE REF TO zcl_taxa_curva, "Objeto da taxa curva.
      gobj_zcl_webservice_tx_curva TYPE REF TO zcl_webservice_tx_curva.

    DATA:
      var_cotacao    TYPE kurrf, "Valor da Cotação.
*        VAR_MONTANTE    TYPE ZSDT0053-ZMENG, "Valor do Montante.
      var_montante   TYPE p LENGTH 13 DECIMALS 5, "Valor do Montante.
      var_montante_2 TYPE zsdt0053-dmbtr. "Valor do Montante.


    DATA: var_data          TYPE datum,
          var_data_completa TYPE datum,
          var_mes           TYPE i,
          var_mes_aux       TYPE c LENGTH 2,
          var_ano           TYPE c LENGTH 4,
          var_taxa_cambio   TYPE ukursp. "Taxa do Cambio.

    DATA: var_data_calculada TYPE d,
          var_msg            TYPE string. "Variavel para mostrar a Mensagem texto da exception.

    DATA: var_tabix TYPE sy-tabix.
    DATA: cx_exception TYPE REF TO zcx_webservice. "Referencia para a Classe de Exception.

    DATA: var_tipo  TYPE char01.
    DATA: data_venc TYPE datum.
    DATA: var_safra TYPE ajahr.

    "Tabela de Solicitação Ordem de Venda - Cabeçalho
    SELECT * FROM zsdt0051 INTO TABLE gt_zsdt0051 WHERE nro_sol_ov EQ i_numero.

    IF ( sy-subrc EQ 0 ).

      "Tabela de Solicitação Ordem de Venda – COND_PGTO
      SELECT * FROM zsdt0052
        INTO TABLE gt_zsdt0052
        FOR ALL ENTRIES IN gt_zsdt0051
       WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.

      "Tabela de Solicitação de ordem de venda - MATERIAIS
      CASE i_tcode.
        WHEN: 'ZSDT0062'.
          SELECT * FROM zsdt0053
            INTO TABLE gt_zsdt0053
            FOR ALL ENTRIES IN gt_zsdt0051
          WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.
          DELETE gt_zsdt0053 WHERE status EQ 'Y'.
          DELETE gt_zsdt0053 WHERE status EQ 'W'.
          DELETE gt_zsdt0053 WHERE status EQ 'C'.
        WHEN: 'VF01'. "VF01

          SELECT * FROM zsdt0053
            INTO TABLE gt_zsdt0053
            FOR ALL ENTRIES IN gt_zsdt0051
          WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
            AND status EQ 'D'.


        WHEN: 'VF11'. "VF11
          SELECT * FROM zsdt0053
            INTO TABLE gt_zsdt0053
            FOR ALL ENTRIES IN gt_zsdt0051
          WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
            AND status EQ 'C'.
      ENDCASE.

      "Tabela de Solicitação Ordem de Venda – PRECO
      SELECT * FROM zsdt0059
        INTO TABLE gt_zsdt0059
        FOR ALL ENTRIES IN gt_zsdt0051
      WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
        AND bezei IN (c_frete_cif,c_frete_porto,c_fobs,c_taxa_cambio,c_conv_bio).

      "Condições de pagamento
      SELECT * FROM t052
        INTO TABLE gt_t052
        FOR ALL ENTRIES IN gt_zsdt0052
      WHERE zterm EQ gt_zsdt0052-zterm.

    ENDIF.

    "Tabela de Solicitação Ordem de Venda – Pagamento Antecipado
    SELECT * FROM zsdt0054
      INTO TABLE gt_zsdt0054
      FOR ALL ENTRIES IN gt_zsdt0051
    WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.

    CASE i_tcode.
      WHEN: 'ZSDT0062'.
        "Tabela de Solicitação Ordem de Venda – Logistica
        SELECT * FROM zsdt0055
          INTO TABLE gt_zsdt0055
          FOR ALL ENTRIES IN gt_zsdt0051
        WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.

        DELETE gt_zsdt0055 WHERE status EQ 'Y'.
        DELETE gt_zsdt0055 WHERE status EQ 'W'.
        DELETE gt_zsdt0055 WHERE status EQ 'C'.

      WHEN: 'VF01'.


        "Tabela de Solicitação Ordem de Venda – Logistica
        SELECT * FROM zsdt0055
          INTO TABLE gt_zsdt0055
          FOR ALL ENTRIES IN gt_zsdt0051
        WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
          AND status EQ 'D'.

      WHEN: 'VF11'.

        "Tabela de Solicitação Ordem de Venda – Logistica
        SELECT * FROM zsdt0055
          INTO TABLE gt_zsdt0055
          FOR ALL ENTRIES IN gt_zsdt0051
        WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov
          AND status     EQ 'C'.

    ENDCASE.



    "Solicitação de Ordem de Venda – Histórico Aprovações WorkFlow
    SELECT * FROM zsdt0162
      INTO TABLE gt_zsdt0162
      FOR ALL ENTRIES IN gt_zsdt0051
    WHERE vbeln EQ gt_zsdt0051-nro_sol_ov
      AND status EQ 'L'
      AND ck_recusa NE 'S'
      AND ultimo_nivel EQ 'X'.

    IF gt_zsdt0162 IS INITIAL.

*    "Solicitação de Ordem de Venda – Histórico Aprovações ZSDT0062
      SELECT * FROM zsdt0069
        INTO TABLE gt_zsdt0069
        FOR ALL ENTRIES IN gt_zsdt0051
      WHERE nro_sol_ov EQ gt_zsdt0051-nro_sol_ov.

    ENDIF.

    LOOP AT gt_zsdt0162 INTO DATA(wa_zsdt0162).

      gw_zsdt0069-id_historico  = wa_zsdt0162-id_log.
      gw_zsdt0069-nro_sol_ov    = wa_zsdt0162-vbeln.
      gw_zsdt0069-status        = wa_zsdt0162-status.
      gw_zsdt0069-motivo        = wa_zsdt0162-motivo.
      gw_zsdt0069-usnam         = wa_zsdt0162-usuario.
      gw_zsdt0069-data_atual    = wa_zsdt0162-data_atual.
      gw_zsdt0069-hora_atual    = wa_zsdt0162-hora_atual.

      APPEND gw_zsdt0069 TO gt_zsdt0069.

    ENDLOOP.

    SORT: gt_zsdt0069 BY id_historico DESCENDING. "Ordenação.

    "metodo para mudar o status quando é feito uma devolução.
    CASE i_tcode.
      WHEN: 'VF01'.
        me->status_devolucao( i_numero = i_numero ).
    ENDCASE.


    "Loop para iniciar o Processo.
    LOOP AT gt_zsdt0051 INTO gw_zsdt0051.

      me->free( ). "Limpar os Atributos Totais.

      "Executa os metodos para somar os valores da ZSDT0053, ZSDT0054, ZSDT0055.
      me->total_53( gw_zsdt0051-nro_sol_ov ).
      me->total_peso_53( gw_zsdt0051-nro_sol_ov ).
      me->total_54( gw_zsdt0051-nro_sol_ov ).
      me->total_55( gw_zsdt0051-nro_sol_ov ).

      "Faz o Calculo para pegar o indice.
      IF NOT ( me->at_total_53 IS INITIAL ) AND NOT ( me->at_total_54 IS INITIAL ).
        me->indice( i_total_53 = me->at_total_53
                    i_total_54 = me->at_total_54
                   ).
      ENDIF.

      READ TABLE gt_zsdt0053 INTO gw_zsdt0053       WITH KEY nro_sol_ov  = gw_zsdt0051-nro_sol_ov.
      READ TABLE gt_zsdt0052 INTO gw_zsdt0052       WITH KEY nro_sol_ov  = gw_zsdt0051-nro_sol_ov.
      READ TABLE gt_zsdt0059 INTO gw_zsdt0059_cif   WITH KEY nro_sol_ov  = gw_zsdt0053-nro_sol_ov
                                                             bezei       = 'FRETE CIF'.
      READ TABLE gt_zsdt0059 INTO gw_zsdt0059_porto WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                             bezei      = 'FRETE PORTO'.
      READ TABLE gt_zsdt0059 INTO gw_zsdt0059_fobs  WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                             bezei      = 'FOBS'.
      READ TABLE gt_zsdt0059 INTO gw_zsdt0059_taxa_cambio WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                                                                   bezei      = 'TAXA CAMBIO'
                                                                   field      = 'PRECO'.
      READ TABLE gt_t052     INTO gw_t052           WITH KEY zterm      = gw_zsdt0052-zterm.
      READ TABLE gt_zsdt0073 INTO gw_zsdt0073 WITH KEY nro_sol_ov = gw_zsdt0051-nro_sol_ov.
      READ TABLE gt_t052     INTO gw_t052     WITH KEY zterm      = gw_zsdt0073-zterm.
      READ TABLE gt_zsdt0069 INTO gw_zsdt0069 INDEX 1.

      " 18.07.2023 - Ramon -->
      SELECT SINGLE matkl FROM mara
        INTO @DATA(lv_matkl)
          WHERE matnr = @gw_zsdt0051-matnr.

      " quando for bio
      IF lv_matkl = '700400'.

        READ TABLE gt_zsdt0059 INTO DATA(ls_bio)
         WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov
                  bezei      = c_conv_bio.

        IF sy-subrc EQ 0.

          LOOP AT gt_zsdt0053 ASSIGNING FIELD-SYMBOL(<fs_0053>).
            <fs_0053>-zmeng = <fs_0053>-zmeng * ls_bio-formula2.
          ENDLOOP.

          LOOP AT gt_zsdt0055 ASSIGNING FIELD-SYMBOL(<fs_0055>).
            <fs_0055>-cadencia_qte = <fs_0055>-cadencia_qte * ls_bio-formula2.
          ENDLOOP.

          gw_zsdt0055-cadencia_qte = gw_zsdt0055-cadencia_qte * ls_bio-formula2.
          gw_zsdt0053-zmeng = gw_zsdt0053-zmeng * ls_bio-formula2.

          me->at_total_55 = me->at_total_55 * ls_bio-formula2.
          me->at_total_peso_53 = me->at_total_peso_53 * ls_bio-formula2.

        ENDIF.

      ENDIF.
      " 18.07.2023 - Ramon --<


      CASE gw_zsdt0051-risco_sacado.
        WHEN: 'S'. "Quando existe o risco sacado (S)

          LOOP AT gt_zsdt0055 INTO gw_zsdt0055 WHERE nro_sol_ov EQ gw_zsdt0053-nro_sol_ov.

            "**********************************
            "* FRETE - INICIO
            "**********************************
            FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
            CREATE OBJECT gobj_zcl_taxa_curva.
            CREATE OBJECT gobj_zcl_webservice_tx_curva.

            gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
            gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
            data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
            gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
            gobj_zcl_taxa_curva->set_zieme(  gw_zsdt0053-zieme ).
            gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
            gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
            gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
            gobj_zcl_taxa_curva->set_tipo( c_frete ).
            gobj_zcl_taxa_curva->set_estorno( i_estorno ).
            gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

            IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
              var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
            ELSE.
              IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.
              ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                " 18.07.2023 ------>
              ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.

              ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                " 18.07.2023 ------<

              ENDIF.
            ENDIF.

            " Conversão casas decimais
            var_montante_2 = CONV #( var_montante ).
            gobj_zcl_taxa_curva->set_total_proporcional( CONV #( var_montante_2 ) ).
            gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
            var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
            gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                               i_tcode    = i_tcode
                                              ).

            IF ( sy-cprog NE 'ZCARGA').
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                       i_data_lib = sy-datum
                                                                       i_tipo     = var_tipo ).
            ELSE.
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                       i_data_lib = gw_zsdt0069-data_atual
                                                                       i_tipo     = var_tipo ).
            ENDIF.

            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
            var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.

            gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa   = var_taxa_cambio
                                                  i_numero = gw_zsdt0051-nro_sol_ov
                                                  i_data   = gw_zsdt0055-data_progr
                                                  i_tipo   = c_frete ).

            var_safra = gw_zsdt0053-charg.
            gobj_zcl_taxa_curva->set_safra( var_safra ).
            me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)
            "**********************************
            "* FRETE - FIM
            "**********************************

            FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
            CREATE OBJECT gobj_zcl_taxa_curva.
            CREATE OBJECT gobj_zcl_webservice_tx_curva.


            IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
              var_montante = gw_zsdt0053-dmbtr * gw_zsdt0055-cadencia_qte.
            ELSE.

              IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                var_montante = gw_zsdt0053-dmbtr / 1000.
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.
              ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                var_montante = gw_zsdt0053-dmbtr  * 1000.
                gw_zsdt0053-dmbtr = var_montante * gw_zsdt0055-cadencia_qte.


                " 18.07.2023 ------>
              ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.

              ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                " 18.07.2023 ------<





              ENDIF.

            ENDIF.

            gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
            gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
            data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-valdt_hedge ).
            gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
            gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

            IF NOT ( gw_zsdt0055-cadencia_qte IS INITIAL ).
              gobj_zcl_taxa_curva->set_cadencia( i_cadencia =  gw_zsdt0055-cadencia_qte
                                                 i_tcode    = i_tcode
                                                 ).
            ELSE.
              gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                  i_tcode    = i_tcode
                                               ).
            ENDIF.

            " Conversão casas decimais

       clear: var_montante_2, var_montante. "SMC
            var_montante = GW_ZSDT0053-vlrtot."SMC
            var_montante_2 = var_montante.
            gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
            gobj_zcl_taxa_curva->set_total_proporcional( CONV #( var_montante_2 ) ).
            gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
            gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
            gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).

            gobj_zcl_taxa_curva->set_tipo( c_venda ).
            gobj_zcl_taxa_curva->set_estorno( i_estorno ).
            gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
            var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
            gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

            IF ( sy-cprog NE 'ZCARGA').
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                       i_data_lib = sy-datum
                                                                       i_tipo     = var_tipo
                                                                       ).
            ELSE.
              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                       i_data_lib = gw_zsdt0069-data_atual
                                                                       i_tipo     = var_tipo
                                                                       ).
            ENDIF.

            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
            var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*         GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
            gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                  i_numero = gw_zsdt0051-nro_sol_ov
                                                  i_data = gw_zsdt0055-valdt_hedge
                                                  i_tipo = c_venda ).
            var_safra = gw_zsdt0053-charg.
            gobj_zcl_taxa_curva->set_safra( var_safra ).
            me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)
            CLEAR: var_cotacao, gw_zsdt0055, var_montante.
          ENDLOOP.

        WHEN: 'N'. "Quando não existe risco sacado (N)

          "Caso a ZSDT0053-VALDT (DATA_VENCIMENTO) OU ZSDT0052-VALDT ((DATA_VENCIMENTO) esteja preenchidas
          IF NOT ( gw_zsdt0053-valdt IS INITIAL ) OR NOT ( gw_zsdt0052-valdt IS INITIAL ). "Regra Número 01.

            IF NOT ( gt_zsdt0054[] IS INITIAL ). "Caso tenha o indice fazer o processo pela a ZSDT0054

              LOOP AT gt_zsdt0054 INTO gw_zsdt0054 WHERE nro_sol_ov EQ gw_zsdt0051-nro_sol_ov.

                FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                CREATE OBJECT gobj_zcl_taxa_curva.
                CREATE OBJECT gobj_zcl_webservice_tx_curva.

                gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0054-valdt ).
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                gobj_zcl_taxa_curva->set_cadencia( 1 ).
                gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0054-dmbtr ) ).
                gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                gobj_zcl_taxa_curva->set_tipo( c_venda ).
                gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                IF ( sy-cprog EQ 'ZCARGA').
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0054-VALDT
                                                                           i_data_lib = gw_zsdt0069-data_atual
                                                                           i_tipo     = var_tipo
                                                                           ).
                ELSE.
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0054-VALDT
                                                                           i_data_lib = sy-datum
                                                                           i_tipo     = var_tipo ).
                ENDIF.
                gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*              GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                      i_numero = gw_zsdt0051-nro_sol_ov
                                                      i_data = gw_zsdt0054-valdt
                                                      i_tipo = c_venda ).

                var_safra = gw_zsdt0053-charg.
                gobj_zcl_taxa_curva->set_safra( var_safra ).
                me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094
              ENDLOOP.

              IF ( me->at_total_54 < me->at_total_53 ).

                LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov EQ gw_zsdt0051-nro_sol_ov.

                  CLEAR: gw_zsdt0054.
                  READ TABLE gt_zsdt0054 INTO gw_zsdt0054 WITH KEY posnr      = gw_zsdt0053-posnr
                                                                   nro_sol_ov = gw_zsdt0053-nro_sol_ov.

                  IF ( gw_zsdt0053-vlrtot EQ gw_zsdt0054-dmbtr ).
                    CONTINUE.
                  ELSE.
                    gw_zsdt0053-vlrtot = gw_zsdt0053-vlrtot - gw_zsdt0054-dmbtr.
                  ENDIF.

                  FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                  CREATE OBJECT gobj_zcl_taxa_curva.
                  CREATE OBJECT gobj_zcl_webservice_tx_curva.

                  gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                  gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                  data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0053-valdt ).
                  gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                  gobj_zcl_taxa_curva->set_cadencia( gw_zsdt0053-zmeng ).
                  gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                  gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                  gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
                  gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                  gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                  gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                  gobj_zcl_taxa_curva->set_tipo( c_venda ).
                  gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                  gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                  var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                  gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                  IF ( sy-cprog EQ 'ZCARGA').
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0053-VALDT
                                                                             i_data_lib = gw_zsdt0069-data_atual
                                                                             i_tipo     = var_tipo ).
                  ELSE.
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0053-VALDT
                                                                             i_data_lib = sy-datum
                                                                             i_tipo     = var_tipo
                                                                             ).
                  ENDIF.

                  gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                  var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                  gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                        i_numero = gw_zsdt0051-nro_sol_ov
                                                        i_data = gw_zsdt0053-valdt
                                                        i_tipo = c_venda ).

                  var_safra = gw_zsdt0053-charg.
                  gobj_zcl_taxa_curva->set_safra( var_safra ).
                  me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094
                ENDLOOP.
              ENDIF.

              "**********************************
              "*  FRETE - INICIO
              "*********************************
              READ TABLE gt_zsdt0055 INTO gw_zsdt0055 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
              IF ( sy-subrc EQ 0 ).

                LOOP AT gt_zsdt0055 INTO gw_zsdt0055 WHERE nro_sol_ov EQ gw_zsdt0051-nro_sol_ov.

                  FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                  CREATE OBJECT gobj_zcl_taxa_curva.
                  CREATE OBJECT gobj_zcl_webservice_tx_curva.

                  gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                  gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                  data_venc = gobj_zcl_taxa_curva->set_data_venc(   gw_zsdt0055-data_progr ).
                  gobj_zcl_taxa_curva->set_data_lib(    gw_zsdt0069-data_atual ).
                  gobj_zcl_taxa_curva->set_zieme(       gw_zsdt0053-zieme ).
                  gobj_zcl_taxa_curva->set_frete_cif(   gw_zsdt0059_cif-formula2 ).
                  gobj_zcl_taxa_curva->set_fobs(        gw_zsdt0059_fobs-formula2 ).
                  gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                  gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                  IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                  ELSE.
                    IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                    ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                      " 18.07.2023 ------>
                    ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                    ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                      " 18.07.2023 ------<



                    ENDIF.
                  ENDIF.

                  gobj_zcl_taxa_curva->set_tipo( c_frete ).
                  gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                  gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                  var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).

                  " Conversão casas decimais
                  var_montante_2 = var_montante .
                  gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                  gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                            i_tcode    = i_tcode
                                           ).
                  IF ( sy-cprog NE 'ZCARGA').
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                             i_data_lib = sy-datum
                                                                             i_tipo     = var_tipo ).
                  ELSE.
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                             i_data_lib = gw_zsdt0069-data_atual
                                                                             i_tipo     = var_tipo ).
                  ENDIF.

                  gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                  var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                  gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                        i_numero = gw_zsdt0051-nro_sol_ov
                                                        i_data = gw_zsdt0055-data_progr
                                                        i_tipo = c_frete ).

                  var_safra = gw_zsdt0053-charg.
                  gobj_zcl_taxa_curva->set_safra( var_safra ).
                  me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094.

                  CLEAR: var_tipo.
                ENDLOOP.

              ELSE. "Caso não encontre na ZSDT0055

                FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                CREATE OBJECT gobj_zcl_taxa_curva.
                CREATE OBJECT gobj_zcl_webservice_tx_curva.

                gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).


                var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                          i_data_final   = gw_zsdt0051-dtate_logist ).

                gobj_zcl_taxa_curva->set_tipo( c_frete ).
                gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                  var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * me->at_total_peso_53 ).
                ELSE.
                  IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                    var_montante = var_montante * me->at_total_peso_53.
                  ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                    var_montante = var_montante * me->at_total_peso_53.

                    " 18.07.2023 ------>
                  ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                    var_montante = var_montante * me->at_total_peso_53.

                  ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                    var_montante = var_montante * me->at_total_peso_53.

                    " 18.07.2023 ------<

                  ENDIF.
                ENDIF.

                " Conversão casas decimais
                var_montante_2 = var_montante .
                gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = me->at_total_peso_53
                                                    i_tcode    =  i_tcode
                                                  ).
                gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).

                IF ( sy-cprog NE 'ZCARGA').
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                           i_data_lib = sy-datum
                                                                           i_tipo     = var_tipo ).
                ELSE.
                  var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                           i_data_lib = gw_zsdt0069-data_atual
                                                                           i_tipo     = var_tipo ).
                ENDIF.

                gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*              GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                      i_numero = gw_zsdt0051-nro_sol_ov
                                                      i_data = data_venc
                                                      i_tipo = c_frete ).

                var_safra = gw_zsdt0053-charg.
                gobj_zcl_taxa_curva->set_safra( var_safra ).
                me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094.

              ENDIF.
              "**********************************
              "* FRETE - FIM
              "**********************************


            ELSE. "Entrar nessa condição caso não tenha INDICE.

              LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov EQ gw_zsdt0051-nro_sol_ov.

                FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                CREATE OBJECT gobj_zcl_taxa_curva.
                CREATE OBJECT gobj_zcl_webservice_tx_curva.

                gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                IF NOT ( gw_zsdt0053-valdt IS INITIAL ).
                  data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0053-valdt ).
                ELSE.
                  data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0052-valdt ).
                ENDIF.

                gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                    i_tcode    = i_tcode
                                                  ).
                gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
                gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                gobj_zcl_taxa_curva->set_tipo( c_venda ).
                gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).


                IF NOT ( gw_zsdt0053-valdt IS INITIAL ).

                  IF ( sy-cprog NE 'ZCARGA').

                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0053-VALDT
                                                                             i_data_lib = sy-datum
                                                                             i_tipo     = var_tipo
                                                                             ).
                  ELSE.
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0053-VALDT
                                                                             i_data_lib = gw_zsdt0069-data_atual
                                                                             i_tipo     = var_tipo
                                                                             ).
                  ENDIF.

                ELSE.

                  IF ( sy-cprog NE 'ZCARGA').
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0052-VALDT
                                                                             i_data_lib = sy-datum
                                                                             i_tipo     = var_tipo
                                                                             ).
                  ELSE.
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0052-VALDT
                                                                             i_data_lib = gw_zsdt0069-data_atual
                                                                             i_tipo     = var_tipo
                                                                             ).
                  ENDIF.
                ENDIF.

                gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*             GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).

                IF NOT ( gw_zsdt0053-valdt IS INITIAL ).

                  gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                        i_numero = gw_zsdt0051-nro_sol_ov
                                                        i_data = gw_zsdt0053-valdt
                                                        i_tipo = c_venda ).
                ELSE.

                  gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                        i_numero = gw_zsdt0051-nro_sol_ov
                                                        i_data = gw_zsdt0052-valdt
                                                        i_tipo = c_venda ).
                ENDIF.

                var_safra = gw_zsdt0053-charg.
                gobj_zcl_taxa_curva->set_safra( var_safra ).
                me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094.

              ENDLOOP.

              "**********************************
              "*  FRETE - INICIO
              "*********************************
              READ TABLE gt_zsdt0055 INTO gw_zsdt0055 WITH KEY nro_sol_ov = gw_zsdt0053-nro_sol_ov.
              IF ( sy-subrc EQ 0 ).

                LOOP AT gt_zsdt0055 INTO gw_zsdt0055 WHERE nro_sol_ov EQ gw_zsdt0051-nro_sol_ov.

                  FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                  CREATE OBJECT gobj_zcl_taxa_curva.
                  CREATE OBJECT gobj_zcl_webservice_tx_curva.

                  gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                  gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                  data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                  gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                  gobj_zcl_taxa_curva->set_zieme(  gw_zsdt0055-zieme ).
                  gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                  gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                  gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                  gobj_zcl_taxa_curva->set_posnr( i_fixacao ).


                  IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0055-cadencia_qte ).
                  ELSE.
                    IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                      var_montante = var_montante *  gw_zsdt0055-cadencia_qte.
                    ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                      var_montante = var_montante *  gw_zsdt0055-cadencia_qte.

                      " 18.07.2023 ------>
                    ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                    ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                      " 18.07.2023 ------<


                    ENDIF.
                  ENDIF.

                  " Conversão casas decimais
                  var_montante_2 = var_montante .
                  gobj_zcl_taxa_curva->set_tipo( c_frete ).
                  gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                  gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                  var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                  gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                       i_tcode    = i_tcode
                                      ).
                  gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).

                  IF ( sy-cprog NE 'ZCARGA').
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                             i_data_lib = sy-datum
                                                                             i_tipo     = var_tipo ).
                  ELSE.
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                             i_data_lib = gw_zsdt0069-data_atual
                                                                             i_tipo     = var_tipo ).
                  ENDIF.

                  gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                  var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                  gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                        i_numero = gw_zsdt0051-nro_sol_ov
                                                        i_data = gw_zsdt0055-data_progr
                                                        i_tipo = c_frete ).

                  var_safra = gw_zsdt0053-charg.
                  gobj_zcl_taxa_curva->set_safra( var_safra ).
                  me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094.

                ENDLOOP.

              ELSE. "Caso não encontre na ZSDT0055

                LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov EQ gw_zsdt0051-nro_sol_ov .

                  FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                  CREATE OBJECT gobj_zcl_taxa_curva.
                  CREATE OBJECT gobj_zcl_webservice_tx_curva.


                  var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                            i_data_final   = gw_zsdt0051-dtate_logist ).

                  gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                  gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                  data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                  gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                  gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                  gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                  IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                  ELSE.
                    IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                      var_montante = var_montante * gw_zsdt0053-zmeng.
                    ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                      var_montante = var_montante * gw_zsdt0053-zmeng.

                      " 18.07.2023 ------>
                    ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                      var_montante = var_montante * gw_zsdt0053-zmeng.

                    ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                      var_montante = var_montante * gw_zsdt0053-zmeng.

                      " 18.07.2023 ------<

                    ENDIF.
                  ENDIF.

                  " Conversão casas decimais
                  var_montante_2 = var_montante .
                  gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                  gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                  gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                  gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                  gobj_zcl_taxa_curva->set_tipo( c_frete ).
                  gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                  gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                  var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                  gobj_zcl_taxa_curva->set_cadencia( gw_zsdt0053-zmeng ).

                  IF ( sy-cprog NE 'ZCARGA').
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                             i_data_lib = sy-datum
                                                                             i_tipo     = var_tipo ).

                  ELSE.
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data    =  data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                             i_data_lib = gw_zsdt0069-data_atual
                                                                             i_tipo     = var_tipo ).

                  ENDIF.

                  gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                  var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                  gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                        i_numero = gw_zsdt0051-nro_sol_ov
                                                        i_data = data_venc
                                                        i_tipo = c_frete ).

                  var_safra = gw_zsdt0053-charg.
                  gobj_zcl_taxa_curva->set_safra( var_safra ).
                  me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094.
                  "**********************************
                  "* FRETE - FIM
                  "**********************************

                ENDLOOP.
              ENDIF.
            ENDIF. "Fim da condição caso não tenha INDICE.

          ELSE. "Caso as datas de vencimento da ZSDT0053 OU ZSDT0052 não esteja preenchidas.

            IF ( gt_zsdt0054[] IS INITIAL ) AND ( gt_zsdt0055[] IS INITIAL ). "Caso não tenha indice/ZSDT0055.

              CASE gw_t052-zdart.
                WHEN: 'D'.

                  "READ TABLE GT_ZSDT0053 INTO GW_ZSDT0053 WITH KEY NRO_SOL_OV = GW_ZSDT0051-NRO_SOL_OV.

                  LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov = gw_zsdt0051-nro_sol_ov.

                    FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                    CREATE OBJECT gobj_zcl_taxa_curva.
                    CREATE OBJECT gobj_zcl_webservice_tx_curva.


                    gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                    gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                    var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                              i_data_final   = gw_zsdt0051-dtate_logist ).

                    data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                    gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                    gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                        i_tcode    = i_tcode
                                                      ).
                    gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                    gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
                    gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                    gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                    gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                    gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                    gobj_zcl_taxa_curva->set_tipo( c_venda ).
                    gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                    gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                    var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                    gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                    IF ( sy-cprog NE 'ZCARGA').
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = sy-datum
                                                                               i_tipo     = var_tipo
                                                                               ).

                    ELSE.
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = gw_zsdt0069-data_atual
                                                                               i_tipo     = var_tipo
                                                                               ).
                    ENDIF.

                    gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                    var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                  GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                    gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                          i_numero = gw_zsdt0051-nro_sol_ov
                                                          i_data = data_venc
                                                          i_tipo = c_venda ).

                    var_safra = gw_zsdt0053-charg.
                    gobj_zcl_taxa_curva->set_safra( var_safra ).
                    me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                    "**********************************
                    "* FRETE - INICIO
                    "**********************************
                    FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                    CREATE OBJECT gobj_zcl_taxa_curva.
                    CREATE OBJECT gobj_zcl_webservice_tx_curva.

                    gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                    gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                    var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                              i_data_final   = gw_zsdt0051-dtate_logist ).

                    data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                    gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).

                    gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                    gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                    IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                    ELSE.
                      IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.
                      ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.

                        " 18.07.2023 ------>
                      ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.

                      ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.

                        " 18.07.2023 ------<

                      ENDIF.
                    ENDIF.

                    " Conversão casas decimais
                    var_montante_2 = var_montante .
                    gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                    gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                    gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                    gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                    gobj_zcl_taxa_curva->set_tipo( c_frete ).
                    gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                    gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                    var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                    gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                        i_tcode    = i_tcode
                                                     ).

                    IF ( sy-cprog NE 'ZCARGA' ).
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = sy-datum
                                                                               i_tipo     = var_tipo
                                                                               ).

                    ELSE.
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = gw_zsdt0069-data_atual
                                                                               i_tipo     = var_tipo
                                                                               ).

                    ENDIF.
                    gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                    var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                  GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                    gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                          i_numero = gw_zsdt0051-nro_sol_ov
                                                          i_data = data_venc
                                                          i_tipo = c_frete ).

                    var_safra = gw_zsdt0053-charg.
                    gobj_zcl_taxa_curva->set_safra( var_safra ).
                    me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                    "**********************************
                    "* FRETE - FIM
                    "**********************************
                  ENDLOOP.


                WHEN: 'B'.

                  "READ TABLE GT_ZSDT0053 INTO GW_ZSDT0053 WITH KEY NRO_SOL_OV = GW_ZSDT0051-NRO_SOL_OV.

                  LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov = gw_zsdt0051-nro_sol_ov.

                    "**********************************
                    "* FRETE - INICIO
                    "**********************************
                    FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                    CREATE OBJECT gobj_zcl_taxa_curva.
                    CREATE OBJECT gobj_zcl_webservice_tx_curva.

                    gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                    gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                    var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                              i_data_final   = gw_zsdt0051-dtate_logist ).

                    data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                    gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                    gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                    gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                    IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                    ELSE.
                      IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.
                      ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.

                        " 18.07.2023 ------>
                      ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.

                      ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.

                        " 18.07.2023 ------<

                      ENDIF.
                    ENDIF.

                    " Conversão casas decimais
                    var_montante_2 = var_montante .
                    gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                    gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                    gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                    gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                    gobj_zcl_taxa_curva->set_tipo( c_frete ).
                    gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                    gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                    var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                    gobj_zcl_taxa_curva->set_cadencia( i_cadencia =  gw_zsdt0053-zmeng
                                                       i_tcode    = i_tcode
                                                     ).

                    IF ( sy-cprog NE 'ZCARGA' ).
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = sy-datum
                                                                               i_tipo     = var_tipo
                                                                               ).
                    ELSE.
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = gw_zsdt0069-data_atual
                                                                               i_tipo     = var_tipo
                                                                               ).
                    ENDIF.

                    gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                    var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                  GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                    gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                          i_numero = gw_zsdt0051-nro_sol_ov
                                                          i_data = data_venc
                                                          i_tipo = c_frete ).

                    var_safra = gw_zsdt0053-charg.
                    gobj_zcl_taxa_curva->set_safra( var_safra ).
                    me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                    "**********************************
                    "* FRETE - FIM
                    "**********************************

                    IF NOT ( gw_t052-ztag1 IS INITIAL ).

                      FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                      CREATE OBJECT gobj_zcl_taxa_curva.
                      CREATE OBJECT gobj_zcl_webservice_tx_curva.

                      gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                      gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                      var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                                i_data_final   = gw_zsdt0051-dtate_logist ).

                      var_data_calculada = var_data_calculada + gw_t052-ztag1.
                      data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                      gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                      gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                          i_tcode    = i_tcode
                                                       ).
                      gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                      gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
                      gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                      gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                      gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                      gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).

                      gobj_zcl_taxa_curva->set_tipo( c_venda ).
                      gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                      gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                      var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                      gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                      IF ( sy-cprog NE 'ZCARGA').
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                 i_data_lib = sy-datum
                                                                                 i_tipo     = var_tipo
                                                                                 ).
                      ELSE.
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                 i_data_lib = gw_zsdt0069-data_atual
                                                                                 i_tipo     = var_tipo
                                                                                 ).
                      ENDIF.


                      gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                      var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                    GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                      gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                            i_numero = gw_zsdt0051-nro_sol_ov
                                                            i_data = data_venc
                                                            i_tipo = c_venda ).

                      var_safra = gw_zsdt0053-charg.
                      gobj_zcl_taxa_curva->set_safra( var_safra ).
                      me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no banco de dados (ZSDT0094).

                      CLEAR: var_data_calculada.
                    ELSE.

                      FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                      CREATE OBJECT gobj_zcl_taxa_curva.
                      CREATE OBJECT gobj_zcl_webservice_tx_curva.

                      gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                      gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                      var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                                i_data_final   = gw_zsdt0051-dtate_logist ).

                      var_mes = ( ( var_mes + var_data_calculada+4(2) ) + gw_t052-zmona ).
                      IF ( var_mes > 12 ).
                        var_mes_aux =  gw_t052-zmona.
                        var_ano = var_data_calculada(4) + 1.
                      ELSE.
                        var_mes_aux = var_mes.
                        var_ano     = var_data_calculada(4).
                      ENDIF.

                      CONCATENATE var_ano var_mes_aux var_data+6(2) INTO var_data_completa.
                      var_data_completa = var_data_completa +  gw_t052-zfael.

                      data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_completa ).
                      gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                      gobj_zcl_taxa_curva->set_cadencia( i_cadencia =  gw_zsdt0053-zmeng
                                                         i_tcode =  i_tcode
                                                        ).
                      gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                      gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
                      gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                      gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                      gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                      gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).

                      gobj_zcl_taxa_curva->set_tipo( c_venda ).
                      gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                      gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                      var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                      gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                      IF ( sy-cprog NE 'ZCARGA').
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "VAR_DATA_COMPLETA
                                                                                 i_data_lib = sy-datum
                                                                                 i_tipo     = var_tipo
                                                                                 ).

                      ELSE.
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "VAR_DATA_COMPLETA
                                                                                 i_data_lib = gw_zsdt0069-data_atual
                                                                                 i_tipo     = var_tipo
                                                                                 ).

                      ENDIF.

                      gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                      var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                    GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                      gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                            i_numero = gw_zsdt0051-nro_sol_ov
                                                            i_data = data_venc
                                                            i_tipo = c_venda ).

                      var_safra = gw_zsdt0053-charg.
                      gobj_zcl_taxa_curva->set_safra( var_safra ).
                      me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                    ENDIF.
                  ENDLOOP.
              ENDCASE.


            ELSE. "Caso tenha INDICE OU ZSDT0055

              IF ( gt_zsdt0054[] IS INITIAL ) AND NOT ( gt_zsdt0055[] IS INITIAL ).

                READ TABLE gt_zsdt0053 INTO gw_zsdt0053 WITH KEY nro_sol_ov = gw_zsdt0051-nro_sol_ov.

                LOOP AT gt_zsdt0055 INTO gw_zsdt0055 WHERE nro_sol_ov EQ gw_zsdt0053-nro_sol_ov.

                  "**********************************
                  "* FRETE - INICIO
                  "**********************************
                  FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                  CREATE OBJECT gobj_zcl_taxa_curva.
                  CREATE OBJECT gobj_zcl_webservice_tx_curva.

                  gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                  gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                  data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                  gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).

                  gobj_zcl_taxa_curva->set_zieme(  gw_zsdt0053-zieme ).
                  gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                  gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                  gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                  gobj_zcl_taxa_curva->set_tipo( c_frete ).
                  gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                  gobj_zcl_taxa_curva->set_posnr( i_fixacao ).


                  IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
                    var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                  ELSE.
                    IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                    ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                      " 18.07.2023 ------>
                    ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                    ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                      var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                      " 18.07.2023 ------<


                    ENDIF.
                  ENDIF.

                  " Conversão casas decimais
                  var_montante_2 = var_montante .
                  gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                  gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                  var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                  gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                            i_tcode    = i_tcode
                                           ).


                  IF ( sy-cprog NE 'ZCARGA').
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                             i_data_lib = sy-datum
                                                                             i_tipo     = var_tipo ).
                  ELSE.
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                             i_data_lib = gw_zsdt0069-data_atual
                                                                             i_tipo     = var_tipo ).
                  ENDIF.

                  gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                  var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                  gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                        i_numero = gw_zsdt0051-nro_sol_ov
                                                        i_data = gw_zsdt0055-data_progr
                                                        i_tipo = c_frete ).

                  var_safra = gw_zsdt0053-charg.
                  gobj_zcl_taxa_curva->set_safra( var_safra ).
                  me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)
                  "**********************************
                  "* FRETE - FIM
                  "**********************************

                  FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                  CREATE OBJECT gobj_zcl_taxa_curva.
                  CREATE OBJECT gobj_zcl_webservice_tx_curva.

                  CASE gw_t052-zdart.
                    WHEN: 'D'.

                      IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
                        var_montante = gw_zsdt0053-dmbtr * gw_zsdt0055-cadencia_qte.
                      ELSE.

                        IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                          var_montante = gw_zsdt0053-dmbtr / 1000.
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                        ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                          var_montante = gw_zsdt0053-dmbtr  * 1000.
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                          " 18.07.2023 ------>
                        ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                        ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                          " 18.07.2023 ------<

                        ENDIF.
                      ENDIF.

                      gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                      gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                      data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                      gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                      gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                      IF NOT ( gw_zsdt0055-cadencia_qte IS INITIAL ).
                        gobj_zcl_taxa_curva->set_cadencia( i_cadencia =  gw_zsdt0055-cadencia_qte
                                                           i_tcode    = i_tcode
                                                          ).
                      ELSE.
                        gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                            i_tcode    = i_tcode
                                                          ).
                      ENDIF.

                      " Conversão casas decimais
                      var_montante_2 = var_montante .

                      gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
                      gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                      gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                      gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                      gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                      gobj_zcl_taxa_curva->set_tipo( c_venda ).
                      gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                      gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                      var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                      gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                      IF ( sy-cprog NE 'ZCARGA').
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                 i_data_lib = sy-datum
                                                                                 i_tipo     = var_tipo
                                                                                 ).

                      ELSE.
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                 i_data_lib = gw_zsdt0069-data_atual
                                                                                 i_tipo     = var_tipo
                                                                                 ).
                      ENDIF.

                      gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                      var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                    GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                      gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                            i_numero = gw_zsdt0051-nro_sol_ov
                                                            i_data = gw_zsdt0055-data_progr
                                                            i_tipo = c_venda ).

                      var_safra = gw_zsdt0053-charg.
                      gobj_zcl_taxa_curva->set_safra( var_safra ).
                      me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                    WHEN: 'B'.
                      IF NOT ( gw_t052-ztag1 IS INITIAL ).

                        gw_zsdt0055-data_progr = gw_zsdt0055-data_progr + gw_t052-ztag1.
                        IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
                          var_montante = gw_zsdt0053-dmbtr * gw_zsdt0055-cadencia_qte.
                        ELSE.

                          IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                            var_montante = gw_zsdt0053-dmbtr / 1000.
                            var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                          ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                            var_montante = gw_zsdt0053-dmbtr  * 1000.
                            gw_zsdt0053-dmbtr = var_montante * gw_zsdt0055-cadencia_qte.

                            " 18.07.2023 ------>
                          ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                            var_montante = gw_zsdt0053-dmbtr  / 1000 .
                            var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                          ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                            var_montante = gw_zsdt0053-dmbtr  * 1000 .
                            var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                            " 18.07.2023 ------<

                          ENDIF.

                        ENDIF.

                        gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                        gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                        data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                        gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                        gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                        IF NOT ( gw_zsdt0055-cadencia_qte IS INITIAL ).
                          gobj_zcl_taxa_curva->set_cadencia( i_cadencia =  gw_zsdt0055-cadencia_qte
                                                             i_tcode    = i_tcode
                                                            ).
                        ELSE.
                          gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                              i_tcode    = i_tcode
                                                            ).
                        ENDIF.

                        " Conversão casas decimais
                        var_montante_2 = var_montante .

                        gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
                        gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                        gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                        gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                        gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                        gobj_zcl_taxa_curva->set_tipo( c_venda ).
                        gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                        gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                        var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                        gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                        IF ( sy-cprog NE 'ZCARGA').
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                   i_data_lib = sy-datum
                                                                                   i_tipo     = var_tipo
                                                                                   ).

                        ELSE.
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                   i_data_lib = gw_zsdt0069-data_atual
                                                                                   i_tipo     = var_tipo
                                                                                   ).
                        ENDIF.

                        gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                        var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                      GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                        gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                              i_numero = gw_zsdt0051-nro_sol_ov
                                                              i_data = gw_zsdt0055-data_progr
                                                              i_tipo = c_venda ).

                        var_safra = gw_zsdt0053-charg.
                        gobj_zcl_taxa_curva->set_safra( var_safra ).
                        me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                      ELSE.

                        gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                        gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                        var_mes = ( ( var_mes + gw_zsdt0055-data_progr+4(2) ) + gw_t052-zmona ).
                        IF ( var_mes > 12 ).
                          var_mes_aux =  gw_t052-zmona.
                          var_ano = gw_zsdt0055-data_progr(4) + 1.
                        ELSE.
                          var_mes_aux = var_mes.
                          var_ano     = gw_zsdt0051-dtde_logist(4).
                        ENDIF.

                        CONCATENATE var_ano var_mes_aux var_data+6(2) INTO var_data_completa.
                        var_data_completa = var_data_completa +  gw_t052-zfael.
                        data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_completa ).
                        gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                        gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                            i_tcode    = i_tcode
                                                         ).
                        gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                        gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
                        gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                        gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                        gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                        gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                        gobj_zcl_taxa_curva->set_tipo( c_venda ).
                        gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                        gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                        var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                        gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                        IF ( sy-cprog NE 'ZCARGA').
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "VAR_DATA_COMPLETA
                                                                                   i_data_lib = sy-datum
                                                                                   i_tipo     = var_tipo
                                                                                   ).
                        ELSE.
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "VAR_DATA_COMPLETA
                                                                                   i_data_lib = gw_zsdt0069-data_atual
                                                                                   i_tipo     = var_tipo
                                                                                   ).
                        ENDIF.

                        gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                        var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                      GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                        gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                              i_numero = gw_zsdt0051-nro_sol_ov
                                                              i_data = var_data_completa
                                                              i_tipo = c_venda ).

                        var_safra = gw_zsdt0053-charg.
                        gobj_zcl_taxa_curva->set_safra( var_safra ).
                        me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                      ENDIF.
                  ENDCASE.
                  CLEAR: var_cotacao, gw_zsdt0055, var_montante.
                ENDLOOP.


              ELSEIF NOT ( gt_zsdt0054[] IS INITIAL ) AND ( gt_zsdt0055[] IS INITIAL ). "Existe indice


                LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov = gw_zsdt0051-nro_sol_ov.
                  "                VAR_TABIX = SY-TABIX.

                  LOOP AT gt_zsdt0054 INTO gw_zsdt0054 WHERE nro_sol_ov EQ gw_zsdt0053-nro_sol_ov
                                                          AND posnr     EQ gw_zsdt0053-posnr.

                    FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                    CREATE OBJECT gobj_zcl_taxa_curva.
                    CREATE OBJECT gobj_zcl_webservice_tx_curva.

                    gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                    gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                    data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0054-valdt ).
                    gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                    gobj_zcl_taxa_curva->set_cadencia( 1 ).
                    gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                    gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0054-dmbtr ) ).
                    gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                    gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                    gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                    gobj_zcl_taxa_curva->set_tipo( c_venda ).
                    gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                    gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                    gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                    var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                    gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                    IF ( sy-cprog NE 'ZCARGA').
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0054-VALDT
                                                                               i_data_lib = sy-datum
                                                                               i_tipo     = var_tipo
                                                                               ).
                    ELSE.
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0054-VALDT
                                                                               i_data_lib = gw_zsdt0069-data_atual
                                                                               i_tipo     = var_tipo
                                                                               ).
                    ENDIF.

                    gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                    var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                  GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                    gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                          i_numero = gw_zsdt0051-nro_sol_ov
                                                          i_data = gw_zsdt0054-valdt
                                                          i_tipo = c_venda ).

                    var_safra = gw_zsdt0053-charg.
                    gobj_zcl_taxa_curva->set_safra( var_safra ).
                    me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094

                  ENDLOOP.
*                DELETE GT_ZSDT0053 INDEX VAR_TABIX.
*                CLEAR: VAR_TABIX.
                ENDLOOP.

                IF ( me->at_total_54 < me->at_total_53 ).

                  CASE gw_t052-zdart.

                    WHEN: 'D'.

                      LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov = gw_zsdt0051-nro_sol_ov.

                        FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                        CREATE OBJECT gobj_zcl_taxa_curva.
                        CREATE OBJECT gobj_zcl_webservice_tx_curva.

                        gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                        gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                        var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                                  i_data_final   = gw_zsdt0051-dtate_logist ).

                        data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                        gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                        gobj_zcl_taxa_curva->set_cadencia( i_cadencia =  gw_zsdt0053-zmeng
                                                           i_tcode    = i_tcode
                                                          ).
                        gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                        gw_zsdt0053-vlrtot = gw_zsdt0053-vlrtot * me->at_indice.
                        gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
                        gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                        gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                        gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                        gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                        gobj_zcl_taxa_curva->set_tipo( c_venda ).
                        gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                        gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                        var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                        gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                        IF ( sy-cprog NE 'ZCARGA').
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                   i_data_lib = sy-datum
                                                                                   i_tipo     = var_tipo
                                                                                   ).

                        ELSE.
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                   i_data_lib = gw_zsdt0069-data_atual
                                                                                   i_tipo     = var_tipo
                                                                                   ).
                        ENDIF.

                        gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                        var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                      GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                        gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                              i_numero = gw_zsdt0051-nro_sol_ov
                                                              i_data = data_venc
                                                              i_tipo = c_venda ).

                        var_safra = gw_zsdt0053-charg.
                        gobj_zcl_taxa_curva->set_safra( var_safra ).
                        me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                        "**********************************
                        "* FRETE - INICIO
                        "**********************************
                        FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                        CREATE OBJECT gobj_zcl_taxa_curva.
                        CREATE OBJECT gobj_zcl_webservice_tx_curva.

                        gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                        gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                        var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                                  i_data_final   = gw_zsdt0051-dtate_logist ).

                        data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                        gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).

                        gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).

                        IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                        ELSE.
                          IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                            var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                            var_montante = var_montante * gw_zsdt0053-zmeng.
                          ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                            var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                            var_montante = var_montante * gw_zsdt0053-zmeng.


                            " 18.07.2023 ------>
                          ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                            var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                            var_montante = var_montante * gw_zsdt0053-zmeng.

                          ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                            var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                            var_montante = var_montante * gw_zsdt0053-zmeng.

                            " 18.07.2023 ------<



                          ENDIF.
                        ENDIF.

                        " Conversão casas decimais
                        var_montante_2 = var_montante .
                        gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                        gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                        gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                        gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                        gobj_zcl_taxa_curva->set_tipo( c_frete ).
                        gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                        gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                        var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                        gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                            i_tcode    = i_tcode
                                                          ).
                        gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                        IF ( sy-cprog NE 'ZCARGA').
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                   i_data_lib = sy-datum
                                                                                   i_tipo     = var_tipo ).
                        ELSE.
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                   i_data_lib = gw_zsdt0069-data_atual
                                                                                   i_tipo     = var_tipo ).
                        ENDIF.

                        gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                        var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                      GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                        gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                              i_numero = gw_zsdt0051-nro_sol_ov
                                                              i_data = data_venc
                                                              i_tipo = c_frete ).

                        var_safra = gw_zsdt0053-charg.
                        gobj_zcl_taxa_curva->set_safra( var_safra ).
                        me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                        "**********************************
                        "* FRETE - FIM
                        "**********************************
                      ENDLOOP.


                    WHEN: 'B'.

                      LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov = gw_zsdt0051-nro_sol_ov.

                        "**********************************
                        "* FRETE - INICIO
                        "**********************************
                        FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                        CREATE OBJECT gobj_zcl_taxa_curva.
                        CREATE OBJECT gobj_zcl_webservice_tx_curva.

                        gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                        gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                        var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                                  i_data_final   = gw_zsdt0051-dtate_logist ).

                        data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                        gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                        gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                        gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                        IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                        ELSE.
                          IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                            var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                            var_montante = var_montante * gw_zsdt0053-zmeng.
                          ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                            var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                            var_montante = var_montante * gw_zsdt0053-zmeng.

                            " 18.07.2023 ------>
                          ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                            var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                            var_montante = var_montante * gw_zsdt0053-zmeng.

                          ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                            var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                            var_montante = var_montante * gw_zsdt0053-zmeng.

                            " 18.07.2023 ------<


                          ENDIF.
                        ENDIF.

                        " Conversão casas decimais
                        var_montante_2 = var_montante .

                        gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                        gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                        gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                        gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                        gobj_zcl_taxa_curva->set_tipo( c_frete ).
                        gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                        gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                        var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                        gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0053-zmeng
                                                           i_tcode    = i_tcode
                                                         ).

                        IF ( sy-cprog NE 'ZCARGA').
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                   i_data_lib = sy-datum
                                                                                   i_tipo     = var_tipo
                                                                                   ).
                        ELSE.
                          var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                   i_data_lib = gw_zsdt0069-data_atual
                                                                                   i_tipo     = var_tipo
                                                                                   ).
                        ENDIF.

                        gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                        var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                      GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                        gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                              i_numero = gw_zsdt0051-nro_sol_ov
                                                              i_data = data_venc
                                                              i_tipo = c_frete ).

                        var_safra = gw_zsdt0053-charg.
                        gobj_zcl_taxa_curva->set_safra( var_safra ).
                        me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)
                        "**********************************
                        "* FRETE - FIM
                        "**********************************

                        IF NOT ( gw_t052-ztag1 IS INITIAL ).

                          FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                          CREATE OBJECT gobj_zcl_taxa_curva.
                          CREATE OBJECT gobj_zcl_webservice_tx_curva.

                          gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                          gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                          gw_zsdt0051-dtde_logist = gw_zsdt0051-dtde_logist + gw_t052-ztag1.
                          data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0051-dtde_logist ).
                          gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                          gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                              i_tcode    = i_tcode ).
                          gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                          gw_zsdt0053-vlrtot = gw_zsdt0053-vlrtot * me->at_indice.
                          gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
                          gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                          gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                          gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                          gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                          gobj_zcl_taxa_curva->set_tipo( c_venda ).
                          gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                          gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                          var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                          gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                          IF ( sy-cprog NE 'ZCARGA').
                            var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                     i_data_lib = sy-datum
                                                                                     i_tipo     = var_tipo
                                                                                     ).

                          ELSE.
                            var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                                     i_data_lib = gw_zsdt0069-data_atual
                                                                                     i_tipo     = var_tipo
                                                                                     ).

                          ENDIF.

                          gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                          var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                        GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                          gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                                i_numero = gw_zsdt0051-nro_sol_ov
                                                                i_data = gw_zsdt0051-dtde_logist
                                                                i_tipo = c_venda ).

                          var_safra = gw_zsdt0053-charg.
                          gobj_zcl_taxa_curva->set_safra( var_safra ).
                          me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no banco de dados (ZSDT0094).

                        ELSE.

                          FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                          CREATE OBJECT gobj_zcl_taxa_curva.
                          CREATE OBJECT gobj_zcl_webservice_tx_curva.

                          gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                          gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                          var_mes = ( ( var_mes + gw_zsdt0051-dtde_logist+4(2) ) + gw_t052-zmona ).
                          IF ( var_mes > 12 ).
                            var_mes_aux =  gw_t052-zmona.
                            var_ano = gw_zsdt0051-dtde_logist(4) + 1.
                          ELSE.
                            var_mes_aux = var_mes.
                            var_ano     = gw_zsdt0051-dtde_logist(4).
                          ENDIF.

                          CONCATENATE var_ano var_mes_aux var_data+6(2) INTO var_data_completa.
                          var_data_completa = var_data_completa +  gw_t052-zfael.

                          data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_completa ).
                          gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                          gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0053-zmeng
                                                              i_tcode    = i_tcode
                                                            ).
                          gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                          gw_zsdt0053-vlrtot = gw_zsdt0053-vlrtot * me->at_indice.
                          gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
                          gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                          gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                          gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                          gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                          gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).

                          gobj_zcl_taxa_curva->set_tipo( c_venda ).
                          gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                          gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                          var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                          gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                          IF ( sy-cprog NE 'ZCARGA').
                            var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "VAR_DATA_COMPLETA
                                                                                     i_data_lib = sy-datum
                                                                                     i_tipo     = var_tipo
                                                                                     ).

                          ELSE.
                            var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "VAR_DATA_COMPLETA
                                                                                     i_data_lib = gw_zsdt0069-data_atual
                                                                                     i_tipo     = var_tipo
                                                                                     ).
                          ENDIF.

                          gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                          var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                        GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                          gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                                i_numero = gw_zsdt0051-nro_sol_ov
                                                                i_data = var_data_completa
                                                                i_tipo = c_venda ).

                          var_safra = gw_zsdt0053-charg.
                          gobj_zcl_taxa_curva->set_safra( var_safra ).
                          me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                        ENDIF.
                      ENDLOOP.
                  ENDCASE.
                ELSE. " Lançamento do frete quando total da ZSDT0054 é igual ao total da ZSDT0053

                  "**********************************
                  "* FRETE - INICIO
                  "**********************************
                  LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov = gw_zsdt0053-nro_sol_ov.

                    FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                    CREATE OBJECT gobj_zcl_taxa_curva.
                    CREATE OBJECT gobj_zcl_webservice_tx_curva.

                    gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                    gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                    var_data_calculada  = me->calc_data_venc( i_data_inicial = gw_zsdt0051-dtde_logist
                                                              i_data_final   = gw_zsdt0051-dtate_logist ).
                    data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_calculada ).
                    gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).

                    gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                    gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                    IF ( gw_zsdt0053-zieme EQ  gw_zsdt0053-pmein ).
                      var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                    ELSE.
                      IF ( gw_zsdt0053-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.
                      ELSEIF ( gw_zsdt0053-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.


                        " 18.07.2023 ------>
                      ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.

                      ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                        var_montante = var_montante * gw_zsdt0053-zmeng.

                        " 18.07.2023 ------<

                      ENDIF.
                    ENDIF.

                    " Conversão casas decimais
                    var_montante_2 = var_montante .

                    gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                    gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                    gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                    gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                    gobj_zcl_taxa_curva->set_tipo( c_frete ).
                    gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                    gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                    var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                    gobj_zcl_taxa_curva->set_cadencia( i_cadencia =  gw_zsdt0053-zmeng
                                                       i_tcode    = i_tcode
                                                     ).

                    IF ( sy-cprog NE 'ZCARGA').
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = sy-datum
                                                                               i_tipo     = var_tipo ).
                    ELSE.
                      var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0051-DTDE_LOGIST
                                                                               i_data_lib = gw_zsdt0069-data_atual
                                                                               i_tipo     = var_tipo ).
                    ENDIF.

                    gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                    var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                  GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                    gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                          i_numero = gw_zsdt0051-nro_sol_ov
                                                          i_data = data_venc
                                                          i_tipo = c_frete ).

                    var_safra = gw_zsdt0053-charg.
                    gobj_zcl_taxa_curva->set_safra( var_safra ).
                    me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)
                    "**********************************
                    "* FRETE - FIM
                    "**********************************
                  ENDLOOP.

                ENDIF.

              ELSEIF NOT ( gt_zsdt0054[] IS INITIAL ) AND NOT ( gt_zsdt0055[] IS INITIAL ).

                LOOP AT gt_zsdt0054 INTO gw_zsdt0054 WHERE nro_sol_ov EQ gw_zsdt0051-nro_sol_ov.

                  FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                  CREATE OBJECT gobj_zcl_taxa_curva.
                  CREATE OBJECT gobj_zcl_webservice_tx_curva.

                  gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                  gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                  data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0054-valdt ).
                  gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                  gobj_zcl_taxa_curva->set_cadencia( 1 ).
                  gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                  gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0054-dmbtr ) ).
                  gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                  gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                  gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                  gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                  gobj_zcl_taxa_curva->set_tipo( c_venda ).
                  gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                  gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                  var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                  gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                  IF ( sy-cprog NE 'ZCARGA').
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0054-VALDT
                                                                             i_data_lib = sy-datum
                                                                             i_tipo     = var_tipo
                                                                             ).
                  ELSE.
                    var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0054-VALDT
                                                                             i_data_lib = gw_zsdt0069-data_atual
                                                                             i_tipo     = var_tipo
                                                                             ).
                  ENDIF.

                  gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                  var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                  gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                        i_numero = gw_zsdt0051-nro_sol_ov
                                                        i_data = gw_zsdt0054-valdt
                                                        i_tipo = c_venda ).

                  var_safra = gw_zsdt0053-charg.
                  gobj_zcl_taxa_curva->set_safra( var_safra ).
                  me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir na tabela ZSDT0094

                ENDLOOP.


                IF ( me->at_total_54 < me->at_total_53 ).

                  LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov = gw_zsdt0051-nro_sol_ov.
                    var_tabix = sy-tabix.
                    CLEAR: var_montante.
                    LOOP AT gt_zsdt0055 INTO gw_zsdt0055 WHERE nro_sol_ov EQ gw_zsdt0051-nro_sol_ov.

                      FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                      CREATE OBJECT gobj_zcl_taxa_curva.
                      CREATE OBJECT gobj_zcl_webservice_tx_curva.

                      "**********************************
                      "* FRETE - INICIO
                      "**********************************
                      FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                      CREATE OBJECT gobj_zcl_taxa_curva.
                      CREATE OBJECT gobj_zcl_webservice_tx_curva.

                      gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                      gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                      data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                      gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                      gobj_zcl_taxa_curva->set_zieme(  gw_zsdt0053-zieme ).
                      gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                      IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                      ELSE.
                        IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                        ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                          " 18.07.2023 ------>
                        ELSEIF ( gw_zsdt0053-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                        ELSEIF ( gw_zsdt0053-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                          " 18.07.2023 ------<



                        ENDIF.
                      ENDIF.

                      " Conversão casas decimais
                      var_montante_2 = var_montante .
                      gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                      gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                      gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                      gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                      gobj_zcl_taxa_curva->set_tipo( c_frete ).
                      gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                      gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                      var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                      gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                                  i_tcode    = i_tcode
                                                 ).

                      IF ( sy-cprog NE 'ZCARGA').
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                 i_data_lib = sy-datum
                                                                                 i_tipo     = var_tipo ).
                      ELSE.
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                 i_data_lib = gw_zsdt0069-data_atual
                                                                                 i_tipo     = var_tipo ).
                      ENDIF.

                      gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                      var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                    GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                      gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                            i_numero = gw_zsdt0051-nro_sol_ov
                                                            i_data = gw_zsdt0055-data_progr
                                                            i_tipo = c_venda ).

                      var_safra = gw_zsdt0053-charg.
                      gobj_zcl_taxa_curva->set_safra( var_safra ).
                      me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)
                      "**********************************
                      "* FRETE - FIM
                      "**********************************

                      CASE gw_t052-zdart.
                        WHEN: 'D'.

                          IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
                            var_montante = gw_zsdt0053-dmbtr * gw_zsdt0055-cadencia_qte.
                          ELSE.

                            IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                              var_montante = gw_zsdt0053-dmbtr / 1000.
                              var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                            ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                              var_montante = gw_zsdt0053-dmbtr  * 1000.
                              var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                              " 18.07.2023 ------>
                            ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                              var_montante = gw_zsdt0053-dmbtr  / 1000.
                              var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                            ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                              var_montante = gw_zsdt0053-dmbtr  * 1000.
                              var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                              " 18.07.2023 ------<



                            ENDIF.
                          ENDIF.

                          gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                          gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                          data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                          gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                          gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                          IF NOT ( gw_zsdt0055-cadencia_qte IS INITIAL ).
                            gobj_zcl_taxa_curva->set_cadencia( i_cadencia =  gw_zsdt0055-cadencia_qte
                                                               i_tcode    = i_tcode
                                                              ).
                          ELSE.
                            gobj_zcl_taxa_curva->set_cadencia( i_cadencia =  gw_zsdt0053-zmeng
                                                               i_tcode   = i_tcode
                                                             ).
                          ENDIF.

                          " Conversão casas decimais
                          var_montante_2 = var_montante .
                          gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
                          var_montante = var_montante * me->at_indice.
                          gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                          gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                          gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                          gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                          gobj_zcl_taxa_curva->set_tipo( c_venda ).
                          gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                          gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                          var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                          gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).


                          IF ( sy-cprog NE 'ZCARGA' ).
                            var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                     i_data_lib = sy-datum
                                                                                     i_tipo     = var_tipo
                                                                                     ).

                          ELSE.
                            var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                     i_data_lib = gw_zsdt0069-data_atual
                                                                                     i_tipo     = var_tipo
                                                                                     ).

                          ENDIF.

                          gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                          var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                        GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                          gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                                i_numero = gw_zsdt0051-nro_sol_ov
                                                                i_data = gw_zsdt0055-data_progr
                                                                i_tipo = c_venda ).

                          var_safra = gw_zsdt0053-charg.
                          gobj_zcl_taxa_curva->set_safra( var_safra ).
                          me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                        WHEN: 'B'.

                          IF NOT ( gw_t052-ztag1 IS INITIAL ).

                            gw_zsdt0055-data_progr = gw_zsdt0055-data_progr + gw_t052-ztag1.
                            IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
                              var_montante = gw_zsdt0053-dmbtr * gw_zsdt0055-cadencia_qte.
                            ELSE.

                              IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                                var_montante = gw_zsdt0053-dmbtr / 1000.
                                var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                              ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                                var_montante = gw_zsdt0053-dmbtr  * 1000.
                                gw_zsdt0053-dmbtr = var_montante * gw_zsdt0055-cadencia_qte.

                                " 18.07.2023 ------>
                              ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                                var_montante = gw_zsdt0053-dmbtr  / 1000.
                                var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                              ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                                var_montante = gw_zsdt0053-dmbtr  * 1000.
                                var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                                " 18.07.2023 ------<


                              ENDIF.

                            ENDIF.

                            gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                            gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                            data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                            gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                            gobj_zcl_taxa_curva->set_posnr( i_fixacao ).

                            IF NOT ( gw_zsdt0055-cadencia_qte IS INITIAL ).
                              gobj_zcl_taxa_curva->set_cadencia(  i_cadencia = gw_zsdt0055-cadencia_qte
                                                                  i_tcode    = i_tcode
                                                               ).
                            ELSE.
                              gobj_zcl_taxa_curva->set_cadencia( i_cadencia =  gw_zsdt0053-zmeng
                                                                 i_tcode    = i_tcode
                                                                ).
                            ENDIF.

                            " Conversão casas decimais
                            var_montante_2 = var_montante .

                            gobj_zcl_taxa_curva->set_zieme( gw_zsdt0055-zieme ).
                            var_montante = var_montante * me->at_indice.
                            gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                            gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                            gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                            gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).

                            gobj_zcl_taxa_curva->set_tipo( c_venda ).
                            gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                            gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                            var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                            gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).

                            IF ( sy-cprog NE 'ZCARGA').
                              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                       i_data_lib = sy-datum
                                                                                       i_tipo     = var_tipo
                                                                                       ).

                            ELSE.
                              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     = data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                       i_data_lib = gw_zsdt0069-data_atual
                                                                                       i_tipo     = var_tipo
                                                                                       ).

                            ENDIF.

                            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                            var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                          GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                            gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                                  i_numero = gw_zsdt0051-nro_sol_ov
                                                                  i_data = gw_zsdt0055-data_progr
                                                                  i_tipo = c_venda ).

                            var_safra = gw_zsdt0053-charg.
                            gobj_zcl_taxa_curva->set_safra( var_safra ).
                            me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                          ELSE.

                            gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                            gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).

                            var_mes = ( ( var_mes + gw_zsdt0055-data_progr+4(2) ) + gw_t052-zmona ).
                            IF ( var_mes > 12 ).
                              var_mes_aux =  gw_t052-zmona.
                              var_ano = gw_zsdt0055-data_progr(4) + 1.
                            ELSE.
                              var_mes_aux = var_mes.
                              var_ano     = gw_zsdt0051-dtde_logist(4).
                            ENDIF.

                            CONCATENATE var_ano var_mes_aux var_data+6(2) INTO var_data_completa.
                            var_data_completa = var_data_completa +  gw_t052-zfael.
                            data_venc = gobj_zcl_taxa_curva->set_data_venc( var_data_completa ).
                            gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                            gobj_zcl_taxa_curva->set_cadencia( i_cadencia =  gw_zsdt0053-zmeng
                                                               i_tcode    = i_tcode
                                                              ).
                            gobj_zcl_taxa_curva->set_zieme( gw_zsdt0053-zieme ).
                            gw_zsdt0053-vlrtot = gw_zsdt0053-vlrtot * me->at_indice.
                            gobj_zcl_taxa_curva->set_total_proporcional( CONV #( gw_zsdt0053-vlrtot ) ).
                            gobj_zcl_taxa_curva->set_posnr( i_fixacao ).
                            gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                            gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                            gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                            gobj_zcl_taxa_curva->set_tipo( c_venda ).
                            gobj_zcl_taxa_curva->set_estorno( i_estorno ).

                            gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                            var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                            gobj_zcl_taxa_curva->set_tipo_taxa( var_tipo ).


                            IF ( sy-cprog NE 'ZCARGA').
                              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "VAR_DATA_COMPLETA
                                                                                       i_data_lib = sy-datum
                                                                                       i_tipo     = var_tipo
                                                                                       ).
                            ELSE.
                              var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "VAR_DATA_COMPLETA
                                                                                       i_data_lib = gw_zsdt0069-data_atual
                                                                                       i_tipo     = var_tipo
                                                                                       ).
                            ENDIF.

                            gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                            var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                          GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                            gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                                  i_numero = gw_zsdt0051-nro_sol_ov
                                                                  i_data = var_data_completa
                                                                  i_tipo = c_venda ).

                            var_safra = gw_zsdt0053-charg.
                            gobj_zcl_taxa_curva->set_safra( var_safra ).
                            me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)

                          ENDIF.
                      ENDCASE.

                      CLEAR: var_cotacao, gw_zsdt0055.
                    ENDLOOP.
                    DELETE gt_zsdt0053 INDEX var_tabix.
                    CLEAR: var_tabix.
                  ENDLOOP.
                ELSE.

                  "**********************************
                  "* FRETE - INICIO
                  "**********************************
                  LOOP AT gt_zsdt0053 INTO gw_zsdt0053 WHERE nro_sol_ov = gw_zsdt0051-nro_sol_ov.
                    var_tabix = sy-tabix.
                    LOOP AT gt_zsdt0055 INTO gw_zsdt0055 WHERE nro_sol_ov EQ gw_zsdt0053-nro_sol_ov.

                      FREE: gobj_zcl_taxa_curva, gobj_zcl_webservice_tx_curva.
                      CREATE OBJECT gobj_zcl_taxa_curva.
                      CREATE OBJECT gobj_zcl_webservice_tx_curva.

                      gobj_zcl_taxa_curva->set_numero( gw_zsdt0051-nro_sol_ov ).
                      gobj_zcl_taxa_curva->set_incoterms( gw_zsdt0051-inco1 ).
                      data_venc = gobj_zcl_taxa_curva->set_data_venc( gw_zsdt0055-data_progr ).
                      gobj_zcl_taxa_curva->set_data_lib( gw_zsdt0069-data_atual ).
                      gobj_zcl_taxa_curva->set_zieme(  gw_zsdt0053-zieme ).
                      gobj_zcl_taxa_curva->set_posnr( i_fixacao ).


                      IF ( gw_zsdt0055-zieme EQ  gw_zsdt0053-pmein ).
                        var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 ) * gw_zsdt0053-zmeng ).
                      ELSE.
                        IF ( gw_zsdt0055-zieme EQ 'KG' ) AND ( gw_zsdt0053-pmein EQ 'TO' ).
                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.
                        ELSEIF ( gw_zsdt0055-zieme EQ 'TO' ) AND ( gw_zsdt0053-pmein EQ 'KG' ).
                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                          " 18.07.2023 ------>
                        ELSEIF ( gw_zsdt0055-zieme EQ 'L' ) AND ( gw_zsdt0053-pmein EQ 'M3' ).
                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  / 1000 ).
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                        ELSEIF ( gw_zsdt0055-zieme EQ 'M3' ) AND ( gw_zsdt0053-pmein EQ 'L' ).

                          var_montante = ( ( gw_zsdt0059_porto-formula2 - gw_zsdt0059_cif-formula2 - gw_zsdt0059_fobs-formula2 )  * 1000 ).
                          var_montante = var_montante * gw_zsdt0055-cadencia_qte.

                          " 18.07.2023 ------<


                        ENDIF.
                      ENDIF.

                      " Conversão casas decimais
                      var_montante_2 = var_montante .

                      gobj_zcl_taxa_curva->set_total_proporcional( var_montante_2 ).
                      gobj_zcl_taxa_curva->set_frete_cif( gw_zsdt0059_cif-formula2 ).
                      gobj_zcl_taxa_curva->set_fobs( gw_zsdt0059_fobs-formula2 ).
                      gobj_zcl_taxa_curva->set_frete_porto( gw_zsdt0059_porto-formula2 ).
                      gobj_zcl_taxa_curva->set_tipo( c_frete ).
                      gobj_zcl_taxa_curva->set_estorno( i_estorno ).
                      gobj_zcl_taxa_curva->verifica_tipo_taxa( i_tcode = i_tcode ).
                      var_tipo =  gobj_zcl_taxa_curva->get_tipo_taxa( ).
                      gobj_zcl_taxa_curva->set_cadencia( i_cadencia = gw_zsdt0055-cadencia_qte
                                                 i_tcode    = i_tcode
                                                ).

                      IF ( sy-cprog NE 'ZCARGA').
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                 i_data_lib = sy-datum
                                                                                 i_tipo     = var_tipo ).
                      ELSE.
                        var_cotacao = gobj_zcl_webservice_tx_curva->buscar_taxa( i_data     =  data_venc "GW_ZSDT0055-DATA_PROGR
                                                                                 i_data_lib = gw_zsdt0069-data_atual
                                                                                 i_tipo     = var_tipo
                                                                                 ).
                      ENDIF.

                      gobj_zcl_taxa_curva->set_taxa_curva( var_cotacao ).
                      var_taxa_cambio = gw_zsdt0059_taxa_cambio-formula2.
*                    GOBJ_ZCL_TAXA_CURVA->SET_TAXA_CAMBIO( VAR_TAXA_CAMBIO ).
                      gobj_zcl_taxa_curva->set_taxa_cambio( i_taxa =   var_taxa_cambio
                                                            i_numero = gw_zsdt0051-nro_sol_ov
                                                            i_data = gw_zsdt0055-data_progr
                                                            i_tipo = c_frete ).

                      var_safra = gw_zsdt0053-charg.
                      gobj_zcl_taxa_curva->set_safra( var_safra ).
                      me->zif_taxa_curva_db~inserir( gobj_zcl_taxa_curva ). "Inserir no Banco de Dados (ZSDT0094)
                    ENDLOOP.

                    "**********************************
                    "* FRETE - FIM
                    "**********************************
                    DELETE gt_zsdt0053 INDEX var_tabix.
                    CLEAR: var_tabix.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD reversao_frete_in.

***************************************************
**** OBJ DA TAXA CAMBIO E CURVA E OS METODOS DA 94
***************************************************
    DATA: obj_tx_curva TYPE REF TO zcl_webservice_tx_curva,
          obj_insere   TYPE REF TO zcl_taxa_curva_db,
          obj_0094     TYPE REF TO zcl_taxa_curva.

    DATA: it_set TYPE TABLE OF rgsb4,
          wa_set TYPE rgsb4.
    DATA var_total TYPE dmbtr.

    DATA: sequencia      TYPE posnr.
    DATA: set_porc_frete TYPE p DECIMALS 5.

*   FOB -> CPT, FOB -> CFR
    IF (  i_0090-inco1v EQ 'FOB' ).
**Inicio IR246304 - 29/07/2025
*      IF ( i_0090-inco1 EQ 'CPT' AND i_0090-spartv NE '03' ) OR ( i_0090-inco1 EQ 'CFR' ).
      IF ( i_0090-inco1 EQ 'CPT' AND i_0090-spartv NE '03' AND i_0090-spartv NE '02'
         AND i_0090-spartv NE '04' )
        OR ( i_0090-inco1 EQ 'CFR' ).
****Fim IR246304 - 29/07/2025
        EXIT.
      ENDIF.
    ENDIF.

*   CPT -> FOB, CPT -> CFR
    IF (  i_0090-inco1v EQ 'CPT' ).
**Inicio IR246304 - 29/07/2025
*      IF ( i_0090-inco1 EQ 'FOB' OR i_0090-inco1 EQ 'CFR' ) AND i_0090-spartv NE '03' .
      IF ( i_0090-inco1 EQ 'FOB' OR i_0090-inco1 EQ 'CFR' )
        AND ( i_0090-spartv NE '03' AND i_0090-spartv NE '02'
        AND   i_0090-spartv NE '04'  ).
****Fim IR246304 - 29/07/2025
        EXIT.
      ENDIF.
    ENDIF.

*   CFR -> FOB, CFR -> CPT
    IF (  i_0090-inco1v EQ 'CFR' ).
**Inicio IR246304 - 29/07/2025
**  Ins Setor atividade para Iconterms CPT
*    IF ( i_0090-inco1 EQ 'FOB' OR ( i_0090-inco1 EQ 'CPT' AND i_0090-spartv NE '03' ) ).
      IF ( i_0090-inco1 EQ 'FOB'
        OR ( i_0090-inco1 EQ 'CPT' AND i_0090-spartv NE '03'
        AND i_0090-spartv NE '02'
        AND i_0090-spartv NE '04'  ) ).
****Fim IR246304 - 29/07/2025
        EXIT.
      ENDIF.
    ENDIF.

    CREATE OBJECT: obj_tx_curva, obj_0094, obj_insere.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class           = '0000'
        setnr           = 'MAGGI_FRI_HEDGE'
        no_descriptions = space
        no_rw_info      = space
      TABLES
        set_values      = it_set
      EXCEPTIONS
        set_not_found   = 1
        OTHERS          = 2.

    IF sy-subrc IS INITIAL.

      wa_set = it_set[ 1 ].

      CALL FUNCTION 'MOVE_CHAR_TO_NUM'
        EXPORTING
          chr             = wa_set-from
        IMPORTING
          num             = set_porc_frete
        EXCEPTIONS
          convt_no_number = 1
          convt_overflow  = 2
          OTHERS          = 3.

      set_porc_frete = set_porc_frete / 100.

    ENDIF.

    SELECT SINGLE *
      FROM zsdt0040
      INTO @DATA(wa_0040)
      WHERE doc_simulacao EQ @i_0090-doc_simulacao.

    IF i_0090-inco1 NE 'CIF'.
      DATA(venc) = SWITCH #( i_0090-spartv WHEN '03' THEN wa_0040-dt_entrega_def
                                           WHEN '02' THEN wa_0040-dt_entrega_fet
                                           WHEN '04' THEN wa_0040-dt_entrega_sem ).
      obj_0094->set_data_venc( venc ).
    ENDIF.

    SELECT SINGLE * FROM zsdt0117
      INTO @DATA(wa_0117)
      WHERE bukrs EQ @wa_0040-vkorg
      AND desativado EQ @abap_false.

    SELECT SINGLE * FROM mara
      INTO @DATA(wa_mara)
      WHERE matnr EQ @i_0090-matnrv.

    SELECT SINGLE *
    FROM zsdt0037
    INTO @DATA(wa_0037)
    WHERE val_de  LE @sy-datum
      AND val_ate GE @sy-datum
      AND bukrs   EQ @wa_0040-vkorg
      AND matkl   EQ @wa_mara-matkl
      AND filial_origem EQ @i_0090-werksv
      AND meins   EQ @i_0090-kmeinv
      AND filial_destino EQ @wa_0040-vkbur
      AND waers   EQ 'BRL'.


    MOVE i_0090-sequencia TO sequencia.

    obj_0094->set_numero( i_0090-doc_simulacao ).
    obj_0094->set_posnr( sequencia ).
    obj_0094->set_matkl( i_matkl = wa_mara-matkl
                         i_brgew = wa_mara-brgew
                        ).
    obj_0094->set_netpr( i_netpr = i_0090-netprv
                         i_kmein = i_0090-kmeinv ).
    obj_0094->set_zieme( i_0090-ziemev ).
    obj_0094->set_bezei( '' ).


    obj_0094->set_cadencia_in( i_cadencia =  COND #( WHEN i_dir EQ 'F' THEN i_0090-zmengv ELSE i_0090-zmeng ) ).
    obj_0094->set_cadencia_in( i_cadencia =  obj_0094->get_cadencia( )
                               i_negativa = COND #( WHEN i_0090-inco1 NE 'CIF' THEN 'N' ELSE 'S' )
                               i_0040 = wa_0040
                              ).
    IF wa_mara-matkl EQ '658445'.
      obj_0094->set_zieme( i_0090-ziemev ).
    ENDIF.

    IF i_0090-spartv EQ '03'.

      DATA(v_um) = COND #( WHEN i_0090-inco1 NE 'CIF' THEN 1 ELSE -1 ).

      IF ( wa_0040-waerk = 'USD' ) AND ( wa_0117-kursk IS NOT INITIAL ).
        var_total = ( ( ( i_0090-zmengv * i_0090-netprv ) * set_porc_frete ) * v_um ) * wa_0117-kursk.
      ELSE.
        var_total = ( ( i_0090-zmengv * i_0090-netprv ) * set_porc_frete ) * v_um.
      ENDIF.

      obj_0094->set_total_proporcional( var_total ).
      obj_0094->set_cadencia_in( i_cadencia =  0 ).

    ENDIF.

    obj_0094->set_tipo_taxa( COND #( WHEN i_0090-inco1 NE 'CIF' THEN 'C' ELSE 'V' ) ).

    obj_0094->set_data_lib( sy-datum ).
    obj_0094->set_taxa_curva(
    obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                               i_data_lib = obj_0094->get_data_lib(  )
                               i_tipo     = obj_0094->get_tipo_taxa( )
                             ) ).

    IF wa_0040-taxa_frete IS INITIAL.
      obj_0094->set_taxa_cambio( wa_0117-kursk ).
    ELSE.
      obj_0094->set_taxa_cambio( wa_0040-taxa_frete ).
    ENDIF.

*    IF WA_0040-WAERK EQ 'USD'.
*      WA_0037-VLR_FRETE = WA_0037-VLR_FRETE / OBJ_0094->GET_TAXA_CAMBIO( ).
*    ENDIF.

    IF i_0090-spartv NE '03'.
      obj_0094->set_frete_in( i_frete = wa_0037-vlr_frete
                              i_zieme = wa_0037-meins
                             ).
    ENDIF.

    obj_0094->set_tipo( 'FRI' ).
    obj_0094->set_vbeln( i_0090-vbelv ).
    obj_0094->set_incoterms( i_0090-inco1 ).
    obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).


  ENDMETHOD.


  METHOD STATUS_DEVOLUCAO.
    DATA: LT_ZSDT0055 TYPE TABLE OF ZSDT0055,
          LW_ZSDT0055 TYPE ZSDT0055.

    DATA: LT_ZSDT0053 TYPE TABLE OF ZSDT0053,
          LW_ZSDT0053 TYPE ZSDT0053.

    SELECT * FROM ZSDT0055 INTO TABLE LT_ZSDT0055
      WHERE NRO_SOL_OV EQ I_NUMERO
        AND STATUS     EQ 'D'.

    IF ( SY-SUBRC EQ 0 ).
      LOOP AT LT_ZSDT0055 INTO LW_ZSDT0055.
        IF I_AUART NE 'ZCPV'.
          UPDATE ZSDT0055 SET STATUS = 'Y'
            WHERE NRO_SOL_OV EQ LW_ZSDT0055-NRO_SOL_OV
              AND DATA_PROGR EQ LW_ZSDT0055-DATA_PROGR
              AND ID         EQ LW_ZSDT0055-ID.
        ELSE.
          UPDATE ZSDT0055 SET STATUS = 'W'
          WHERE NRO_SOL_OV EQ LW_ZSDT0055-NRO_SOL_OV
            AND DATA_PROGR EQ LW_ZSDT0055-DATA_PROGR
            AND ID         EQ LW_ZSDT0055-ID.
        ENDIF.

        COMMIT WORK.

        WAIT UP TO 2 SECONDS.

        CLEAR: LW_ZSDT0055.

      ENDLOOP.
    ENDIF.


    SELECT * FROM ZSDT0053
      INTO TABLE LT_ZSDT0053
    WHERE NRO_SOL_OV EQ I_NUMERO
      AND VBELN EQ I_ESTORNO
      AND STATUS EQ 'D'.

    IF ( SY-SUBRC EQ 0 ).
      LOOP AT LT_ZSDT0053 INTO LW_ZSDT0053.

        IF I_AUART NE 'ZCPV'.
          UPDATE ZSDT0053 SET STATUS = 'Y'
            WHERE NRO_SOL_OV EQ LW_ZSDT0053-NRO_SOL_OV
             AND POSNR       EQ LW_ZSDT0053-POSNR.
        ELSE.
          UPDATE ZSDT0053 SET STATUS = 'W'
          WHERE NRO_SOL_OV EQ LW_ZSDT0053-NRO_SOL_OV
           AND POSNR       EQ LW_ZSDT0053-POSNR.
        ENDIF.

        COMMIT WORK.
        WAIT UP TO 2 SECONDS.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD TOTAL_53.

    DATA: LT_ZSDT0053 TYPE TABLE OF ZSDT0053,
          LW_ZSDT0053 TYPE ZSDT0053.

    IF ( I_FIXACAO IS INITIAL ).
      SELECT * FROM ZSDT0053 INTO TABLE  LT_ZSDT0053 WHERE NRO_SOL_OV EQ I_NUMERO
                                                       AND STATUS     NE 'C'.
    ELSE.
      SELECT * FROM ZSDT0053 INTO TABLE  LT_ZSDT0053 WHERE NRO_SOL_OV EQ I_NUMERO
                                                       AND FIXACAO    EQ I_FIXACAO
                                                       AND STATUS     NE 'C'.
    ENDIF.

    LOOP AT LT_ZSDT0053 INTO LW_ZSDT0053.
      ME->AT_TOTAL_53 = ME->AT_TOTAL_53 + LW_ZSDT0053-VLRTOT.
    ENDLOOP.

  ENDMETHOD.


  METHOD TOTAL_54.

    DATA: LT_ZSDT0054 TYPE TABLE OF ZSDT0054,
          LW_ZSDT0054 TYPE ZSDT0054.

    SELECT * FROM ZSDT0054 INTO TABLE LT_ZSDT0054 WHERE NRO_SOL_OV EQ I_NUMERO.

    LOOP AT LT_ZSDT0054 INTO LW_ZSDT0054.
      ME->AT_TOTAL_54 = ME->AT_TOTAL_54 + LW_ZSDT0054-DMBTR.
    ENDLOOP.


  ENDMETHOD.


  METHOD TOTAL_55.

    DATA: LT_ZSDT0055 TYPE TABLE OF ZSDT0055,
          LW_ZSDT0055 TYPE ZSDT0055.

    SELECT * FROM ZSDT0055 INTO TABLE LT_ZSDT0055 WHERE NRO_SOL_OV EQ I_NUMERO.

    LOOP AT LT_ZSDT0055 INTO LW_ZSDT0055.
      ME->AT_TOTAL_55 = ME->AT_TOTAL_55 + LW_ZSDT0055-CADENCIA_QTE.
    ENDLOOP.

  ENDMETHOD.


  METHOD TOTAL_94.

    DATA: LT_ZSDT0094 TYPE TABLE OF ZSDT0094,
          LW_ZSDT0094 TYPE ZSDT0094.

    CLEAR E_DIFE.

    IF ( I_FIXACAO IS INITIAL ).
      SELECT * FROM ZSDT0094 INTO TABLE LT_ZSDT0094 WHERE NRO_SOL_OV EQ I_NUMERO
                                                      AND BEZEI      NE 'DIFERENCA'.

    ELSE.
      SELECT * FROM ZSDT0094 INTO TABLE LT_ZSDT0094 WHERE NRO_SOL_OV EQ I_NUMERO
                                                      AND FIXACAO    EQ I_FIXACAO
                                                      AND TIPO       EQ 'VDA'.

    ENDIF.

    LOOP AT LT_ZSDT0094 INTO LW_ZSDT0094.
      ME->AT_TOTAL_94 = ME->AT_TOTAL_94 + LW_ZSDT0094-TOTAL_PROPORC.

      IF LW_ZSDT0094-BEZEI EQ 'DIFERENCA'.
        CASE LW_ZSDT0094-ESTORNO.
          WHEN 0.
            MOVE LW_ZSDT0094-TOTAL_PROPORC TO E_DIFE.
        ENDCASE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD TOTAL_94_FRE.


    DATA: LT_ZSDT0094 TYPE TABLE OF ZSDT0094,
          LW_ZSDT0094 TYPE ZSDT0094.

    CLEAR E_DIFE.

    IF ( I_FIXACAO IS INITIAL ).
      SELECT * FROM ZSDT0094 INTO TABLE LT_ZSDT0094 WHERE NRO_SOL_OV EQ I_NUMERO
                                                      AND BEZEI      NE 'DIFERENCAFRE'.

    ELSE.
      SELECT * FROM ZSDT0094 INTO TABLE LT_ZSDT0094 WHERE NRO_SOL_OV EQ I_NUMERO
                                                      AND FIXACAO    EQ I_FIXACAO
                                                      AND TIPO       EQ 'FRE'.

    ENDIF.

    LOOP AT LT_ZSDT0094 INTO LW_ZSDT0094.
      ME->AT_TOTAL_94 = ME->AT_TOTAL_94 + LW_ZSDT0094-TOTAL_PROPORC.

      IF LW_ZSDT0094-BEZEI EQ 'DIFERENCAFRE'.
        CASE LW_ZSDT0094-ESTORNO.
          WHEN 0.
            MOVE LW_ZSDT0094-TOTAL_PROPORC TO E_DIFE.
        ENDCASE.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD TOTAL_PESO_53.

    DATA: LT_ZSDT0053 TYPE TABLE OF ZSDT0053,
          LW_ZSDT0053 TYPE ZSDT0053.

    SELECT * FROM ZSDT0053 INTO TABLE  LT_ZSDT0053 WHERE NRO_SOL_OV EQ I_NUMERO.

    LOOP AT LT_ZSDT0053 INTO LW_ZSDT0053.
      ME->AT_TOTAL_PESO_53 = ME->AT_TOTAL_PESO_53 + LW_ZSDT0053-ZMENG.
    ENDLOOP.

  ENDMETHOD.


  METHOD update_job.

    CHECK tcode EQ 'ZSDT0062' OR tcode EQ 'VF01'.

    UPDATE zsdt0051
         SET job = abap_true
         WHERE nro_sol_ov EQ nro_sol_ov.

    CHECK vl_vbeln IS NOT INITIAL.

    UPDATE zsdt0053
        SET job = abap_true
        WHERE nro_sol_ov EQ nro_sol_ov
          AND status EQ 'E'
          AND job EQ 'E'.

    UPDATE zsdt0053
          SET job = abap_true
          WHERE nro_sol_ov EQ nro_sol_ov
            AND vbeln EQ vl_vbeln
            AND job IN ( 'Y', 'W' ).

  ENDMETHOD.


  METHOD venda_in.

******************************************************************************
**** TABELA ZSDT0041 COM MATKL PARA AGRUPAR OS DADOS PELO GRUPO DE MERCADORIA
******************************************************************************
    TYPES BEGIN OF ty_0041.
    INCLUDE TYPE zsdt0041.
    TYPES matkl TYPE matkl.
    TYPES brgew TYPE brgew.
*---> S4 MIGRATION 10/07/2023 - MA
*    TYPES dtpgtcult TYPE bapi_jbd_dte_dzterm.
    TYPES dtpgtcult TYPE valdt.
*<--- S4 MIGRATION 10/07/2023 - MA
    TYPES kursk TYPE kursk.
    TYPES END OF ty_0041.

***************************************************
**** OBJ DA TAXA CAMBIO E CURVA E OS METODOS DA 94
***************************************************
    DATA: obj_tx_curva TYPE REF TO zcl_webservice_tx_curva,
          obj_insere   TYPE REF TO zcl_taxa_curva_db,
          obj_0094     TYPE REF TO zcl_taxa_curva.

******************************************
********  TABELAS INTERNAS E WORK AREAS
******************************************
    DATA: it_0041       TYPE TABLE OF ty_0041,
          tipo          TYPE char3,
          var_total     TYPE dmbtr,
          sequencia     TYPE posnr,
          taxa          TYPE kursf,
          taxa_0090     TYPE kursf,
          vl_qtde_equal TYPE c,
          brgew         TYPE brgew,
          v_liq         TYPE netwr,
          v_imp         TYPE mwsbp.

    FIELD-SYMBOLS <final> TYPE ty_0041.
**********************
****** LIBERA OS OBJ
**********************
    FREE: obj_tx_curva, obj_0094, obj_insere.

**********************
****** CRIA OS OBJ
**********************
    CREATE OBJECT: obj_tx_curva, obj_0094, obj_insere.

    CLEAR taxa.

    CASE sy-cprog.
      WHEN 'ZSDR016'.

**********************************************************************************
* MONTA A ESTRUTURA COM OS DADOS AGRUPADOS SOMADOS E DIVIDIDOS PELO INCO1 E MATKL
**********************************************************************************
        obj_0094->agrupa_dados( EXPORTING i_numero = i_numero
                                          i_tipo   = i_tipo
                                          t_itens  = t_itens
                                IMPORTING i_0041   = it_0041
                                         ).

        SELECT SINGLE * FROM zsdt0040
          INTO @DATA(wa_0040)
          WHERE doc_simulacao EQ @i_numero.

*    MONTA A SAIDA PARA INCLUIR NO DISPARO
        tipo = i_tipo.
        LOOP AT it_0041 ASSIGNING <final>.

          obj_0094->set_numero( <final>-doc_simulacao ).

          CASE wa_0040-tpsim.
            WHEN 'VV' OR 'AD'.
              obj_0094->set_data_venc( wa_0040-dtvencov ).
            WHEN OTHERS.
              obj_0094->set_data_venc( <final>-dtpgtcult ).
          ENDCASE.

          obj_0094->set_data_lib( sy-datum ).

          IF i_acao EQ 'DSC_ABS'.
            <final>-zmeng = 0.
            var_total = <final>-desc_absoluto * -1.
            tipo = 'DES'.
          ELSE.
            var_total = <final>-vlrtot.
          ENDIF.

          obj_0094->set_total_proporcional( var_total ).
          obj_0094->tipo_taxa_in( tipo ).
          obj_0094->set_taxa_curva(
          obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                     i_data_lib = obj_0094->get_data_lib(  )
                                     i_tipo     = obj_0094->get_tipo_taxa( )
                                   ) ).

          obj_0094->set_tipo( i_tipo ).
          obj_0094->set_incoterms( <final>-inco1 ).
          obj_0094->set_matkl( i_matkl = <final>-matkl
                               i_brgew = <final>-brgew
                               ).
          obj_0094->set_bezei( '' ).
          obj_0094->set_cadencia_in( i_cadencia = <final>-zmeng
                                     i_negativa = 'N'
                                     ).

          obj_0094->set_taxa_in( ).
          IF wa_0040-tpsim EQ 'VF'.
            obj_0094->set_taxa_cambio( wa_0040-kursf ).
          ELSE.
            IF obj_0094->get_taxa_in( ) IS INITIAL.
              obj_0094->set_taxa_cambio( obj_0094->get_taxa_curva( ) ).
            ELSE.
              obj_0094->set_taxa_cambio( obj_0094->get_taxa_in( ) ).
            ENDIF.
          ENDIF.

          " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
          IF i_taxa_boleta IS NOT INITIAL.
            obj_0094->set_taxa_cambio( i_taxa_boleta ).
          ENDIF.
          " 03.01.2023 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<

          obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).

        ENDLOOP.

**************************************************************
* DISPARO DA TRANSAÇÃO ZSDT0087
**************************************************************
      WHEN 'ZSDR0042'.

        SELECT SINGLE * FROM zsdt0040
          INTO wa_0040
        WHERE doc_simulacao EQ i_0090-doc_simulacao.

        SELECT SINGLE * FROM vbak
          INTO @DATA(wa_vbak)
          WHERE vbeln EQ @i_0090-vbelv.

        SELECT SINGLE * FROM vbkd
          INTO @DATA(wa_vbkd)
          WHERE vbeln EQ @wa_vbak-vbeln.

        "Ajuste Desconto 19.05.2017
*--->S4 MIGRATION 10/07/2023 - MA
        IF wa_vbak-knumv IS NOT INITIAL.
*          SELECT *
*            FROM konv INTO TABLE @DATA(tl_konv)
*           WHERE knumv EQ @wa_vbak-knumv.
*
*          SELECT SINGLE *
*            FROM konv INTO @DATA(wa_konv)
*           WHERE knumv EQ @wa_vbak-knumv.
          SELECT *
            FROM v_konv INTO TABLE @DATA(tl_konv)
           WHERE knumv EQ @wa_vbak-knumv.

          SELECT SINGLE *
            FROM v_konv INTO @DATA(wa_konv)
           WHERE knumv EQ @wa_vbak-knumv.
*<---S4 MIGRATION 10/07/2023 - MA
        ENDIF.

        CLEAR: taxa_0090.
        IF wa_0040-waerk = 'USD'.
          CALL FUNCTION 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
            EXPORTING
              i_doc_simulacao = i_0090-doc_simulacao
              i_vbeln         = i_0090-vbelv
            CHANGING
              c_taxa          = taxa_0090.
        ENDIF.

        CALL FUNCTION 'ZSDMF001_COMPARE_UNIT_MAT'
          EXPORTING
            i_matnr_01 = i_0090-matnr
            i_menge_01 = i_0090-zmeng
            i_matnr_02 = i_0090-matnrv
            i_menge_02 = i_0090-zmengv
          IMPORTING
            e_equal    = vl_qtde_equal.


        IF i_acao EQ 'PRICE' OR i_acao EQ 'PRICE_NEW' .

          SELECT SINGLE * FROM vbap
            INTO @DATA(wa_vbap)
            WHERE vbeln EQ @i_0090-vbelv.

          SELECT SINGLE * FROM mara
            INTO @DATA(wa_mara)
            WHERE matnr EQ @wa_vbap-matnr.

          SELECT SUM( brgew ) FROM vbap
            INTO brgew
            WHERE vbeln EQ i_0090-vbelv.

          SELECT SUM( netwr ) FROM vbap
            INTO v_liq
            WHERE vbeln EQ i_0090-vbelv.

          SELECT SUM( mwsbp ) FROM vbap
            INTO v_imp
            WHERE vbeln EQ i_0090-vbelv.

        ELSE.

          SELECT SINGLE * FROM vbap
            INTO wa_vbap
            WHERE vbeln EQ i_0090-vbelv
              AND matnr EQ i_0090-matnrv.

          SELECT SINGLE * FROM mara
            INTO wa_mara
            WHERE matnr EQ i_0090-matnrv.

        ENDIF.

        IF wa_0040-tpsim EQ 'VF'.
          taxa = wa_0040-kursf.
        ENDIF.

        IF i_acao NE 'REDIST'.

          MOVE i_0090-sequencia TO sequencia.

          obj_0094->set_numero( i_0090-doc_simulacao ).
          obj_0094->set_posnr( sequencia ).
          obj_0094->set_data_venc( wa_vbkd-valdt ).
          obj_0094->set_data_lib( sy-datum ).

          obj_0094->set_matkl( i_matkl = wa_mara-matkl
                               i_brgew = wa_mara-brgew
                               ).
          obj_0094->set_zieme( i_0090-ziemev ).
          obj_0094->set_netpr( i_netpr = i_0090-netprv
                               i_kmein = i_0090-kmeinv ).

          obj_0094->set_bezei( '' ).
          obj_0094->set_cadencia_in( i_cadencia = i_0090-zmengv
                                     i_tipo     = i_tipo ).
          obj_0094->set_tipo( i_tipo ).

          CASE i_acao.
            WHEN 'ALTERAR'.

              "Voltar Desconto para o item da O.V
              LOOP AT tl_konv INTO wa_konv WHERE knumv EQ wa_vbak-knumv
                                             AND kposn EQ wa_vbap-posnr
                                             AND kschl EQ 'RB00'
                                             AND kbetr NE 0.
                IF wa_konv-kbetr < 0.
                  wa_vbap-netwr = wa_vbap-netwr + abs( wa_konv-kbetr ).
                ELSE.
                  wa_vbap-netwr = wa_vbap-netwr - abs( wa_konv-kbetr ).
                ENDIF.
              ENDLOOP.

              IF wa_0040-waerk = 'USD'.
                var_total = ( ( wa_vbap-netwr + wa_vbap-mwsbp ) / wa_vbap-kwmeng ) * i_0090-zmengv * taxa_0090.
                taxa      = taxa_0090.
              ELSE.
                var_total = ( ( wa_vbap-netwr + wa_vbap-mwsbp ) / wa_vbap-kwmeng ) * i_0090-zmengv.
              ENDIF.

              obj_0094->set_total_proporcional( var_total ).

              IF i_0090-matklv EQ '658445'.
                obj_0094->set_cadencia_in( i_cadencia = 0 ).
              ENDIF.

              obj_0094->tipo_taxa_in( 'OTH' ).
              obj_0094->set_taxa_curva(
              obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                         i_data_lib = obj_0094->get_data_lib(  )
                                         i_tipo     = obj_0094->get_tipo_taxa( )
                                       ) ).

              obj_0094->set_taxa_cambio( i_taxa   = taxa
                                         i_numero = obj_0094->get_numero( )
                                         i_data   = obj_0094->get_data_venc( )
                                         i_tipo   = 'VDI' ).

*              OBJ_0094->SET_TAXA_CAMBIO( OBJ_0094->GET_TAXA_CURVA( ) ).

            WHEN 'ESTORNAR'.
*              METODO SEPARADO
            WHEN 'TROCA'.

              obj_0094->set_zieme( i_0090-ziemev ).

              IF wa_0040-waerk = 'USD'.
                obj_0094->calc_tot( i_taxa = taxa_0090 ).
                taxa      = taxa_0090.
              ELSE.
*                OBJ_0094->CALC_TOT( ).
                obj_0094->set_total_proporcional( i_total = CONV #( i_0090-netwr )
                                                  i_negativa = COND #( WHEN i_0090-zmengv < 0
                                                                      THEN abap_true
                                                                      ELSE abap_false ) ).

              ENDIF.

*              VAR_TOTAL = ( I_0090-ZMENGV * OBJ_0094->GET_NETPR( ) ).
*              VAR_TOTAL = ( OBJ_0094->GET_CADENCIA( ) * OBJ_0094->GET_NETPR( ) ).
*              OBJ_0094->SET_TOTAL_PROPORCIONAL( VAR_TOTAL ).
              obj_0094->set_tipo_taxa( 'V' ).
              obj_0094->set_taxa_curva(
              obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                         i_data_lib = obj_0094->get_data_lib(  )
                                         i_tipo     = obj_0094->get_tipo_taxa( )
                                       ) ).

              obj_0094->set_taxa_cambio( i_taxa   = taxa
                                         i_numero = obj_0094->get_numero( )
                                         i_data   = obj_0094->get_data_venc( )
                                         i_tipo   = 'VDI' ).

              "Definição para zerar valores quando -> Troca e Cadencia Anterior diferente da Nova
              IF ( 'M_U'  CS i_dir   ) AND
                 ( i_0090-matklv EQ i_0090-matkl ) AND
                 ( vl_qtde_equal EQ abap_false   ).
                "( ABS( I_0090-ZMENGV ) NE ABS( I_0090-ZMENG ) ).


                obj_0094->set_taxa_curva( 0 ).

                "OBJ_0094->SET_TAXA_CAMBIO( I_TAXA  = 0
                "                           I_ZERAR = 'X' ).

                obj_0094->set_total_proporcional( 0 ).
                obj_0094->set_tipo_taxa( '' ).
              ENDIF.

*              OBJ_0094->SET_TAXA_CAMBIO( OBJ_0094->GET_TAXA_CURVA( ) ).

            WHEN 'ENCERRAR'.

              obj_0094->set_zieme( i_0090-ziemev ).

              IF wa_0040-waerk = 'USD'.
                obj_0094->calc_tot( i_taxa = taxa_0090 ).
                taxa      = taxa_0090.
              ELSE.
                obj_0094->calc_tot( ).
              ENDIF.

              obj_0094->set_tipo_taxa( 'V' ).
              obj_0094->set_taxa_curva(
              obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                         i_data_lib = obj_0094->get_data_lib(  )
                                         i_tipo     = obj_0094->get_tipo_taxa( )
                                       ) ).

              obj_0094->set_taxa_cambio( i_taxa   = taxa
                                         i_numero = obj_0094->get_numero( )
                                         i_data   = obj_0094->get_data_venc( )
                                         i_tipo   = 'VDI' ).
*              OBJ_0094->SET_TAXA_CAMBIO( OBJ_0094->GET_TAXA_CURVA( ) ).

            WHEN 'PRICE'.

*              OBJ_0094->SET_MATKL( I_MATKL = I_0090-MATKLV ).

              IF i_0090-matklv EQ '658445'.
                obj_0094->set_cadencia_in( i_cadencia = 0 ).
              ELSE.
                obj_0094->set_cadencia_in( i_cadencia = brgew
                                           i_tipo     = i_tipo ).
              ENDIF.

              obj_0094->set_data_venc( i_0090-valdt ).
*              VAR_TOTAL = ( WA_VBAK-NETWR * I_0090-KURRF ).
              var_total = ( ( v_liq + v_imp ) * i_0090-kurrf ).
              obj_0094->set_total_proporcional( var_total ).
              obj_0094->set_tipo_taxa( 'C' ).
              obj_0094->set_taxa_curva(
              obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                         i_data_lib = obj_0094->get_data_lib(  )
                                         i_tipo     = obj_0094->get_tipo_taxa( )
                                       ) ).
              obj_0094->set_taxa_cambio( i_0090-kurrf ).

              " 15.02.2023 - RAMON - Ajuste para lançamento por trava de cambio -->
            WHEN 'PRICE_NEW'.


*              OBJ_0094->SET_MATKL( I_MATKL = I_0090-MATKLV ).

              IF i_0090-matklv EQ '658445'.
                obj_0094->set_cadencia_in( i_cadencia = 0 ).
              ELSE.
                obj_0094->set_cadencia_in( i_cadencia = brgew
                                           i_tipo     = i_tipo ).
              ENDIF.

              obj_0094->set_data_venc( i_0090-data_prevpgto ).
              var_total = i_0090-prev_pgto_brl.
*              var_total = ( ( v_liq + v_imp ) * i_0090-kurrf ).
              obj_0094->set_total_proporcional( var_total ).
              obj_0094->set_tipo_taxa( 'C' ).

              obj_0094->set_taxa_curva(
              obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                          i_data_lib = obj_0094->get_data_lib(  )
                          i_tipo     = obj_0094->get_tipo_taxa( )
                         ) ).

              obj_0094->set_taxa_cambio( i_0090-kurrf ).

              " 15.02.2023 - RAMON - Ajuste para lançamento por trava de cambio --<

              " 16.05.2023 - 98623 - RBL -->
            WHEN 'VENDA'.

              IF i_0090-matklv EQ '658445'.
                obj_0094->set_cadencia_in( i_cadencia = 0 ).
              ELSE.
                obj_0094->set_cadencia_in( i_cadencia = brgew
                                           i_tipo     = i_tipo ).
              ENDIF.

              obj_0094->set_zieme( i_0090-ziemev ).
              obj_0094->set_cadencia_in( abs( i_0090-zmengv ) ).

              " 18.07.2023 - RAMON -->
              " foi alterado pq estava pegando o valdt da vbkd,
              " sendo que foi informado na 0090
              "IF obj_0094->get_data_venc( ) IS INITIAL.
                "obj_0094->set_data_venc( i_0090-data_prevpgto ).
              "ENDIF.

              IF i_0090-data_prevpgto is not INITIAL.
                obj_0094->set_data_venc( i_0090-data_prevpgto ).
              ENDIF.
              " 18.07.2023 - RAMON --<

              var_total = i_0090-prev_pgto_brl.
*              var_total = ( ( v_liq + v_imp ) * i_0090-kurrf ).
              obj_0094->set_total_proporcional( i_negativa = abap_true i_total = var_total ).
              obj_0094->set_tipo_taxa( 'V' ).

              obj_0094->set_taxa_curva(
              obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                         i_data_lib = obj_0094->get_data_lib(  )
                                         i_tipo     = obj_0094->get_tipo_taxa( )
                                       ) ).

              obj_0094->set_taxa_cambio( i_0090-kurrf ).



              " 16.05.2023 - 98623 - RBL --<

            WHEN 'REDIST'.
*              REDISTRIBUIÇÃO SOMENTE NA LINHA NOVA
            WHEN 'MDF_VENC'.

              IF wa_0040-waerk = 'USD'.
                var_total = i_0090-desc_absoluto * taxa_0090.
                taxa      = taxa_0090.
              ELSE.
                var_total = i_0090-desc_absoluto.
              ENDIF.

              obj_0094->set_total_proporcional( var_total ).
              obj_0094->tipo_taxa_in( 'DES' ).
              obj_0094->set_taxa_curva(
              obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                         i_data_lib = obj_0094->get_data_lib(  )
                                         i_tipo     = obj_0094->get_tipo_taxa( )
                                       ) ).

              obj_0094->set_taxa_cambio( i_taxa   = taxa
                                         i_numero = obj_0094->get_numero( )
                                         i_data   = obj_0094->get_data_venc( )
                                         i_tipo   = 'VDI' ).

          ENDCASE.

          obj_0094->set_vbeln( i_0090-vbelv ).
          obj_0094->set_incoterms( i_0090-inco1v ).
          obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).

        ENDIF.

        CHECK NOT i_0090-vbeln IS INITIAL.

        SELECT SINGLE * FROM vbap
          INTO @DATA(wa_vbapn)
          WHERE vbeln EQ @i_0090-vbeln.

        SELECT SINGLE * FROM mara
          INTO @DATA(wa_maran)
          WHERE matnr EQ @i_0090-matnr.

        SELECT SINGLE * FROM vbak
          INTO @DATA(wa_vbakn)
          WHERE vbeln EQ @i_0090-vbeln.

        "Ajuste Desconto 19.05.2017
        IF wa_vbakn-knumv IS NOT INITIAL.
*---> S4 MIGRATION 10/07/2023 - MA
*          SELECT *
*            FROM konv INTO TABLE @DATA(tl_konvn)
*           WHERE knumv EQ @wa_vbakn-knumv.
*
*          SELECT SINGLE *
*            FROM konv INTO @DATA(wa_konvn)
*           WHERE knumv EQ @wa_vbakn-knumv.
          SELECT *
            FROM v_konv INTO TABLE @DATA(tl_konvn)
           WHERE knumv EQ @wa_vbakn-knumv.

          SELECT SINGLE *
            FROM v_konv INTO @DATA(wa_konvn)
           WHERE knumv EQ @wa_vbakn-knumv.
*<--- S4 MIGRATION 10/07/2023 - MA
        ENDIF.


        MOVE i_0090-sequencia TO sequencia.

        obj_0094->set_numero( i_0090-doc_simulacao ).
        obj_0094->set_posnr( sequencia ).
*        OBJ_0094->SET_DATA_VENC( WA_0040-DTPGTCULT ).
        obj_0094->set_data_venc( wa_vbkd-valdt ).
        obj_0094->set_data_lib( sy-datum ).
        obj_0094->set_matkl( i_matkl = wa_maran-matkl
                             i_brgew = wa_maran-brgew
                             ).
        obj_0094->set_zieme( i_0090-zieme ).
        obj_0094->set_bezei( '' ).
        obj_0094->set_tipo( i_tipo ).

        CASE i_acao.
          WHEN 'REDIST'. " REDISTRIBUIÇÃO DE QUANTIDADE

*           add a unidade de medida da quantidade NOVA
            obj_0094->set_zieme( i_0090-zieme ).
*           add a quantidade NOVA para realizar a Conversão de acordo com as regras de Unidade de Medida
            obj_0094->set_cadencia_in( abs( i_0090-zmeng ) ).
*           add o preco e a unidade de medidada do preço NOVA para concverter no CALC_TOT
            obj_0094->set_netpr( i_netpr = i_0090-netpr
                                 i_kmein = i_0090-kmein ).
*           calcula o valor Proporcional do Item NOVA

            IF wa_0040-waerk = 'USD'.
              obj_0094->calc_tot( i_taxa = taxa_0090 ).
              taxa      = taxa_0090.
            ELSE.
              obj_0094->calc_tot( ).
            ENDIF.

*           Recebe o Valor Proporcional do Item NOVO
            var_total = obj_0094->get_total_proporcional( ).


*           add a unidade de medida da quantidade antiga
            obj_0094->set_zieme( i_0090-ziemev ).
*           add a quantidade antiga para realizar a Conversão de acordo com as regras de Unidade de Medida
            obj_0094->set_cadencia_in( abs( i_0090-zmengv ) ).
*           add o preco e a unidade de medidada do preço velho para concverter no CALC_TOT
            obj_0094->set_netpr( i_netpr = i_0090-netprv
                                 i_kmein = i_0090-kmeinv ).
*           calcula o valor Proporcional do Item Antigo

            IF wa_0040-waerk = 'USD'.
              obj_0094->calc_tot( i_taxa = taxa_0090 ).
              taxa      = taxa_0090.
            ELSE.
              obj_0094->calc_tot( ).
            ENDIF.

*           Realiza o Calculo do Valor NOVO Subtraindo com o Valor Antigo
            var_total = var_total - obj_0094->get_total_proporcional( ).

            obj_0094->set_cadencia_in( 0 ).


*            VAR_TOTAL = ( I_0090-ZMENGV * I_0090-NETPRV ) - ( I_0090-ZMENG * I_0090-NETPR ).



            obj_0094->set_total_proporcional( var_total ).
            obj_0094->tipo_taxa_in( 'RED' ).

          WHEN OTHERS. " Lançamento de Venda Novo QUanto Açâo Troca

            IF i_0090-matkl EQ '658445'.
              obj_0094->set_cadencia_in( i_cadencia = 0 ).
            ELSE.
              obj_0094->set_cadencia_in( i_0090-zmeng ).
            ENDIF.

            IF ( 'M_U'  CS i_dir   ) AND
               ( i_acao EQ 'TROCA' ) AND
               ( i_0090-matklv EQ i_0090-matkl ) AND
               ( vl_qtde_equal EQ abap_false   ).
              "( ABS( I_0090-ZMENGV ) NE ABS( I_0090-ZMENG ) ).

*              VAR_TOTAL = OBJ_0094->GET_TOTAL_PROPORCIONAL( ).
*              VAR_TOTAL = VAR_TOTAL * -1.
*              OBJ_0094->SET_TOTAL_PROPORCIONAL( VAR_TOTAL ).

              "Definição para zerar valores quando -> Troca e Cadencia Anterior diferente da Nova
              obj_0094->set_total_proporcional( 0 ).
              obj_0094->set_tipo_taxa( '' ).

            ELSE.

              "Voltar Desconto para o item da O.V
              LOOP AT tl_konvn INTO wa_konvn WHERE knumv EQ wa_vbakn-knumv
                                               AND kposn EQ wa_vbapn-posnr
                                               AND kschl EQ 'RB00'
                                               AND kbetr NE 0.

*               "// get do coeficiente o Iten da OV
                DATA(coeficiente_diferenca) = zcl_solicitacao_ov=>get_imposto(
                  _direcao = 'O'
                  _vbeln   = wa_vbapn-vbeln
                  _posnr   = wa_vbapn-posnr
                ).
*               "// add 1 quando o valor estiver ZERADO
                coeficiente_diferenca = COND #( WHEN coeficiente_diferenca IS INITIAL THEN 1 ELSE coeficiente_diferenca ).
                DIVIDE wa_vbapn-netwr BY coeficiente_diferenca.
*                ADD WA_KONVN-KBETR TO WA_VBAPN-NETWR.
*                IF WA_KONVN-KBETR < 0.
*                  WA_VBAPN-NETWR = WA_VBAPN-NETWR + ABS( WA_KONVN-KBETR ).
*                ELSE.
*                  WA_VBAPN-NETWR = WA_VBAPN-NETWR - ABS( WA_KONVN-KBETR ).
*                ENDIF.
              ENDLOOP.

              IF wa_0040-waerk = 'USD'.
                var_total = ( wa_vbapn-netwr / wa_vbapn-kwmeng ) * i_0090-zmeng * taxa_0090.
                taxa      = taxa_0090.
              ELSE.
                var_total = ( wa_vbapn-netwr / wa_vbapn-kwmeng ) * i_0090-zmeng.
              ENDIF.

              IF i_acao EQ 'TROCA'.
                obj_0094->set_total_proporcional( CONV #( i_0090-netwr ) ).
              ELSE.
                obj_0094->set_total_proporcional( var_total ).
              ENDIF.

              obj_0094->tipo_taxa_in( 'OTH' ).
            ENDIF.

        ENDCASE.

        IF ( 'M_U'  CS i_dir   ) AND
           ( i_acao EQ 'TROCA' ) AND
           ( i_0090-matklv EQ i_0090-matkl ) AND
           ( vl_qtde_equal EQ abap_false   ).
          "( ABS( I_0090-ZMENGV ) NE ABS( I_0090-ZMENG ) ).

          obj_0094->set_taxa_curva( 0 ).

          "OBJ_0094->SET_TAXA_CAMBIO( I_TAXA  = 0
          "                           I_ZERAR = 'X' ).


        ELSE.
          obj_0094->set_taxa_curva(
          obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                     i_data_lib = obj_0094->get_data_lib(  )
                                     i_tipo     = obj_0094->get_tipo_taxa( )
                                   ) ).

          obj_0094->set_taxa_cambio( i_taxa   = taxa
                                     i_numero = obj_0094->get_numero( )
                                     i_data   = obj_0094->get_data_venc( )
                                     i_tipo   = 'VDI' ).
        ENDIF.

*        OBJ_0094->SET_TAXA_CAMBIO( OBJ_0094->GET_TAXA_CURVA( ) ).
        obj_0094->set_vbeln( i_0090-vbeln ).
        obj_0094->set_incoterms( i_0090-inco1 ).
        obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).

      WHEN 'SAPMV60A'. " VF01

        SELECT SINGLE * FROM zsdt0040
          INTO wa_0040
          WHERE doc_simulacao EQ i_0090-doc_simulacao.

        SELECT SINGLE * FROM vbak
          INTO wa_vbak
          WHERE vbeln EQ i_0090-vbeln.

        SELECT SINGLE * FROM vbap
          INTO wa_vbap
          WHERE vbeln EQ i_0090-vbeln.

        SELECT SINGLE * FROM mara
          INTO wa_mara
          WHERE matnr EQ i_0090-matnr.

        CLEAR: taxa_0090.
        IF wa_0040-waerk = 'USD'.
          CALL FUNCTION 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
            EXPORTING
              i_doc_simulacao = i_0090-doc_simulacao
              i_vbeln         = i_0090-vbelv
            CHANGING
              c_taxa          = taxa_0090.
        ENDIF.

        MOVE i_0090-sequencia TO sequencia.

        obj_0094->set_numero( i_numero = i_0090-doc_simulacao
                              i_tipo   = 'IN' "Insumos
                              ).
        obj_0094->set_posnr( sequencia ).

        CASE wa_0040-tpsim.
          WHEN 'VV' OR 'AD'.
            obj_0094->set_data_venc( wa_0040-dtvencov ).
          WHEN OTHERS.
            obj_0094->set_data_venc( wa_0040-dtpgtcult ).
        ENDCASE.

*        OBJ_0094->SET_DATA_VENC( WA_0040-DTPGTCULT ).
        obj_0094->set_data_lib( sy-datum ).
        obj_0094->set_matkl( i_matkl = wa_mara-matkl
                             i_brgew = wa_mara-brgew
                             ).
        obj_0094->set_zieme( i_0090-zieme ).
        obj_0094->set_netpr( i_netpr = i_0090-netpr
                             i_kmein = i_0090-kmein ).

        obj_0094->set_bezei( '' ).
        obj_0094->set_tipo( i_tipo ).

        IF NOT i_dir IS INITIAL.
          obj_0094->set_bezei( i_dir ).
        ENDIF.

        obj_0094->set_cadencia_in( i_0090-zmeng ).

        CASE i_dir.
          WHEN 'Y'. obj_0094->set_tipo_taxa( 'V' ).
          WHEN 'W'. obj_0094->set_tipo_taxa( 'C' ).
        ENDCASE.

        IF wa_0040-waerk = 'USD'.
          obj_0094->calc_tot( i_taxa = taxa_0090 ).
          taxa      = taxa_0090.
        ELSE.
          obj_0094->calc_tot( ).
        ENDIF.

*        VAR_TOTAL = ( I_0090-ZMENGV * OBJ_0094->GET_NETPR( ) ).
*        OBJ_0094->SET_TOTAL_PROPORCIONAL( VAR_TOTAL ).

        obj_0094->set_taxa_curva(
        obj_tx_curva->buscar_taxa( i_data     = obj_0094->get_data_venc( )
                                   i_data_lib = obj_0094->get_data_lib(  )
                                   i_tipo     = obj_0094->get_tipo_taxa( )
                                 ) ).

        obj_0094->set_taxa_cambio( i_taxa   = taxa
                                   i_numero = obj_0094->get_numero( )
                                   i_data   = obj_0094->get_data_venc( )
                                   i_tipo   = 'VDI' ).

        obj_0094->set_vbeln( i_0090-vbeln ).
        obj_0094->set_incoterms( i_0090-inco1 ).
        obj_insere->zif_taxa_curva_db~inserir( obj_taxa = obj_0094 ).

    ENDCASE.

    IF sy-cprog EQ 'ZSDR016'.
      UPDATE zsdt0040
                  SET job = abap_true
             WHERE doc_simulacao EQ i_numero.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_TAXA_CURVA_DB~ATUALIZAR.

    DATA: LW_ZSDT0094 TYPE ZSDT0094.

    LW_ZSDT0094-NRO_SOL_OV    = OBJ_TAXA->GET_NUMERO( ).
    LW_ZSDT0094-DATA_REGISTRO = OBJ_TAXA->GET_DATA_REGISTRO( ).
    LW_ZSDT0094-HORA_REGISTRO = OBJ_TAXA->GET_HORA_REGISTRO( ).
    LW_ZSDT0094-ESTORNO       = OBJ_TAXA->GET_ESTORNO( ).

    UPDATE ZSDT0094 SET ESTORNO = LW_ZSDT0094-ESTORNO
     WHERE NRO_SOL_OV    EQ LW_ZSDT0094-NRO_SOL_OV
       AND DATA_REGISTRO EQ LW_ZSDT0094-DATA_REGISTRO
       AND HORA_REGISTRO EQ LW_ZSDT0094-HORA_REGISTRO.

    COMMIT WORK.

    WAIT UP TO 2 SECONDS.

  ENDMETHOD.


  METHOD ZIF_TAXA_CURVA_DB~ATUALIZAR_FRAME.


    DATA: LW_ZSDT0059 TYPE ZSDT0059.

    LW_ZSDT0059-NRO_SOL_OV = OBJ_TAXA->GET_NUMERO( ).
    LW_ZSDT0059-POSNR      = OBJ_TAXA->GET_POSNR( ).
    LW_ZSDT0059-BEZEI      = OBJ_TAXA->GET_BEZEI( ).

    "Atualizar a tabela ZSDT0059
    UPDATE ZSDT0059
       SET VALDT_HEDGE = SY-DATUM
     WHERE NRO_SOL_OV EQ LW_ZSDT0059-NRO_SOL_OV
       AND POSNR      EQ LW_ZSDT0059-POSNR
       AND BEZEI      EQ LW_ZSDT0059-BEZEI.

    COMMIT WORK.
    WAIT UP TO 2 SECONDS.

  ENDMETHOD.


  METHOD zif_taxa_curva_db~inserir.

    DATA: gw_zsdt0094   TYPE zsdt0094.

    CHECK obj_taxa->check_auart(
                                 tipo   = obj_taxa->get_tipo( )
                                 numero =  obj_taxa->get_numero( )
                               ) IS NOT INITIAL.

    gw_zsdt0094-mandt           = sy-mandt.
    gw_zsdt0094-data_registro   = sy-datum.
    gw_zsdt0094-hora_registro   = sy-uzeit.
    gw_zsdt0094-programa        = sy-cprog.
    gw_zsdt0094-nro_sol_ov      = obj_taxa->get_numero( ).
    gw_zsdt0094-fixacao         = obj_taxa->get_posnr( ).
    gw_zsdt0094-data_venc       = obj_taxa->get_data_venc( ).
    gw_zsdt0094-data_lib        = obj_taxa->get_data_lib( ).
    gw_zsdt0094-cadencia_qte    = obj_taxa->get_cadencia( ).
    gw_zsdt0094-zieme           = obj_taxa->get_zieme( ).
    gw_zsdt0094-total_proporc   = obj_taxa->get_total_proporcional( ).
    gw_zsdt0094-taxa_curva      = obj_taxa->get_taxa_curva( ).
    gw_zsdt0094-taxa_cambio     = obj_taxa->get_taxa_cambio( ).
    gw_zsdt0094-frete_cif       = obj_taxa->get_frete_cif( ).
    gw_zsdt0094-frete_porto     = obj_taxa->get_frete_porto( ).
    gw_zsdt0094-tipo            = obj_taxa->get_tipo( ).
    gw_zsdt0094-bezei           = obj_taxa->get_bezei( ).
    gw_zsdt0094-estorno         = obj_taxa->get_estorno( ).
    gw_zsdt0094-tipo_taxa       = obj_taxa->get_tipo_taxa( ).
    gw_zsdt0094-vbeln           = obj_taxa->get_vbeln( ).
    gw_zsdt0094-inco1           = obj_taxa->get_incoterms( ).
    gw_zsdt0094-safra           = obj_taxa->get_safra( ).

    " 17.07.2024 - RAMON - 144484 -->
    " esse codigo foi inserido para nao deixar gerar hedge vazio,
    " foi feito aqui pq nenhum processo deve gerar hedge sem valor
    IF gw_zsdt0094-total_proporc IS INITIAL.
      EXIT.
    ENDIF.

    " 26.03.2025 - RAMON - 170634 -->
    CHECK  zcl_hedge_limite=>get_instance( )->consulta_limite( gw_zsdt0094 ) = abap_true.
    " 26.03.2025 - RAMON - 170634 --<

    " 17.07.2024 - RAMON - 144484 --<
    INSERT INTO zsdt0094 VALUES gw_zsdt0094.

    COMMIT WORK.
    WAIT UP TO 2 SECONDS.

  ENDMETHOD.


  METHOD ZIF_TAXA_CURVA_DB~INSERIR_FRAME.


    DATA: LW_ZSDT0095 TYPE ZSDT0095. "Sol. OV. – Controle de Cadencia - HEDGE
    DATA: VAR_SEQ     TYPE ZSDT0095-SEQ.

    DATA: IT_0094 TYPE TABLE OF ZSDT0094,
          WA_0094 TYPE ZSDT0094,
          TOTAL   TYPE BEZEI60.

    SELECT * FROM ZSDT0094 INTO TABLE IT_0094 WHERE NRO_SOL_OV EQ I_ZSDT0059-NRO_SOL_OV
                                         AND  FIXACAO EQ I_ZSDT0059-POSNR
                                         AND BEZEI EQ I_ZSDT0059-BEZEI.


    IF ( I_ZSDT0059-FORMULA2 > 0 ).

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR             = '01'
          OBJECT                  = 'ZSEQ_0095'
        IMPORTING
          NUMBER                  = VAR_SEQ
        EXCEPTIONS
          INTERVAL_NOT_FOUND      = 1
          NUMBER_RANGE_NOT_INTERN = 2
          OBJECT_NOT_FOUND        = 3
          QUANTITY_IS_0           = 4
          QUANTITY_IS_NOT_1       = 5
          INTERVAL_OVERFLOW       = 6
          BUFFER_OVERFLOW         = 7
          OTHERS                  = 8.

      LW_ZSDT0095-SEQ         = VAR_SEQ.
      LW_ZSDT0095-BEZEI       = I_ZSDT0059-BEZEI.
      LW_ZSDT0095-NRO_SOL_OV  = I_ZSDT0059-NRO_SOL_OV.
      LW_ZSDT0095-FIXACAO     = I_ZSDT0059-POSNR.

      IF  I_AUART EQ 'ZCPV'.
        LW_ZSDT0095-VALDT_HEDGE = I_DATA.
        LOOP AT IT_0094 INTO WA_0094.
          TOTAL = TOTAL + WA_0094-CADENCIA_QTE.
        ENDLOOP.
        LW_ZSDT0095-QTD_HEDGE   = I_ZSDT0059-FORMULA2 - TOTAL.
      ELSE.
        LW_ZSDT0095-VALDT_HEDGE = I_ZSDT0055-VALDT_HEDGE.
        LW_ZSDT0095-QTD_HEDGE   = I_ZSDT0059-FORMULA2.
      ENDIF.


      INSERT INTO ZSDT0095 VALUES LW_ZSDT0095.
      COMMIT WORK.

      WAIT UP TO 2 SECONDS.
      CLEAR: LW_ZSDT0095, VAR_SEQ.
    ENDIF.

  ENDMETHOD.


  METHOD zif_taxa_curva_db~inserir_in.

    DATA: wa_zsdt0094 TYPE zsdt0094.

    wa_zsdt0094-mandt           = sy-mandt.
    wa_zsdt0094-data_registro   = sy-datum.
    wa_zsdt0094-hora_registro   = sy-uzeit.
    wa_zsdt0094-programa        = sy-cprog.
    wa_zsdt0094-nro_sol_ov      = obj_taxa->get_numero( ).
    wa_zsdt0094-fixacao         = obj_taxa->get_posnr( ).
    wa_zsdt0094-data_venc       = obj_taxa->get_data_venc( ).
    wa_zsdt0094-data_lib        = obj_taxa->get_data_lib( ).
    wa_zsdt0094-cadencia_qte    = obj_taxa->get_cadencia( ).
    wa_zsdt0094-zieme           = obj_taxa->get_zieme( ).
    wa_zsdt0094-total_proporc   = obj_taxa->get_total_proporcional( ).
    wa_zsdt0094-taxa_curva      = obj_taxa->get_taxa_curva( ).
    wa_zsdt0094-taxa_cambio     = obj_taxa->get_taxa_cambio( ).
    wa_zsdt0094-frete_cif       = obj_taxa->get_frete_cif( ).
    wa_zsdt0094-frete_porto     = obj_taxa->get_frete_porto( ).
    wa_zsdt0094-tipo            = obj_taxa->get_tipo( ).
    wa_zsdt0094-bezei           = obj_taxa->get_bezei( ).
    wa_zsdt0094-estorno         = obj_taxa->get_estorno( ).
    wa_zsdt0094-tipo_taxa       = obj_taxa->get_tipo_taxa( ).
    wa_zsdt0094-vbeln           = obj_taxa->get_vbeln( ).
    wa_zsdt0094-inco1           = obj_taxa->get_incoterms( ).
    wa_zsdt0094-intercompany    = obj_taxa->get_intercompany( ).

    " 17.07.2024 - RAMON - 144484 -->
    " esse codigo foi inserido para nao deixar gerar hedge vazio,
    " foi feito aqui pq nenhum processo deve gerar hedge sem valor
    IF wa_zsdt0094-total_proporc IS INITIAL.
      EXIT.
    ENDIF.

    " 17.07.2024 - RAMON - 144484 --<

    " 26.03.2025 - RAMON - 170634 -->
    CHECK  zcl_hedge_limite=>get_instance( )->consulta_limite( wa_zsdt0094 ) = abap_true.
    " 26.03.2025 - RAMON - 170634 --<


    INSERT INTO zsdt0094 VALUES wa_zsdt0094.

    COMMIT WORK.
    WAIT UP TO 2 SECONDS.

  ENDMETHOD.
ENDCLASS.
