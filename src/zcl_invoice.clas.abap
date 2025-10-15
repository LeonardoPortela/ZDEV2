class ZCL_INVOICE definition
  public
  final
  create public .

public section.

  class-data AT_RG_BUKRS type ZDE_BUKRS_T .
  class-data AT_RG_KUNNR type ZDE_KUNNR_T .
  class-data AT_RG_SAFRA type ZDE_NR_SAFRA_T .
  class-data AT_RG_CONTR type ZDE_NR_CONTR_T .
  class-data AT_RG_LOTE type ZDE_CHARG_T .
  class-data AT_RG_DTTKUP type ZDE_DATE_T .
  class-data AT_RG_DTPROV type ZDE_DATE_T .
  class-data AT_RG_INSTR type ZDE_NR_INSTRUCAO_T .
  class-data AT_RG_PROV type ZDE_NR_PROVISIONAL_T .
  class-data AT_RG_DTINV type ZDE_DATE_T .
  class-data AT_ID_INVOICE type ZDE_ID_INVOICE .
  class-data AT_RG_STATUS type ZDE_STATUS_INVOICE_T .
  class-data AT_RG_INVOICE type ZDE_ID_INVOICE_T .

  class-methods GET_DADOS_PROVISIONAL
    importing
      !I_PARAM type RS_T_SELECT
    returning
      value(IT_RESULT) type ZSDE0089_T .
  class-methods GET_DADOS_INVOICE
    importing
      !I_PARAM type RS_T_SELECT
    returning
      value(IT_RESULT) type ZSDE0098_T .
  class-methods GET_DADOS_NOTA_INVOICE
    importing
      !I_PARAM type RS_T_SELECT
    returning
      value(IT_RESULT) type ZSDE0091_T .
  class-methods SAVE_MEMOR_DATA_PROVISIONAL
    importing
      value(I_DATA) type ZSDE0089_T optional
    returning
      value(E_RETURN) type CHAR01 .
  class-methods GET_DADOS_INSTRUCAO
    returning
      value(IT_RESULT) type ZSDE0092_T .
  class-methods SET_PARAMETRO_FILTER
    importing
      !I_PARAM type RS_T_SELECT .
  class-methods GET_DADOS_LOTE_INSTRUCAO
    importing
      !I_INSTRUCAO type ZSDED030
    returning
      value(IT_RESULT) type ZSDE0093_T .
  class-methods GERAR_SEQ_INVOICE
    importing
      !I_SAFRA type GJAHR
    returning
      value(E_NR_INVOICE) type ZDE_ID_INVOICE .
  class-methods SAVE_DADOS_INVOICE
    importing
      !I_HEADER type ZSDE0094
      !I_ITENS type ZSDE0093_T
      !I_LOTE_AVARIADO type ZSDE0099_T
    exporting
      !E_ID_INVOICE type ZDE_ID_INVOICE
    returning
      value(R_RESULT) type CHAR01 .
  class-methods GET_INVOICE_GERADAS
    importing
      !I_ID_INVOICE type ZDE_ID_INVOICE
    exporting
      !E_ITENS type ZSDE0093_T
      !E_HEADER type ZSDE0094
      !E_LOTE_AVARIADO type ZSDE0099_T .
  class-methods GET_SALDO_LOTE_CLIENTE
    importing
      !I_KUNNR type KUNNR
      !I_SAFRA type GJAHR
    returning
      value(IT_RESULT) type ZSDE0099_T .
  class-methods GET_ABATIMENTO_PI
    returning
      value(IT_RESULT) type ZSDE0101_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INVOICE IMPLEMENTATION.


  method gerar_seq_invoice.

    data: lv_number  type nrnr,              " Número sequencial retornado
*          lv_subobject type nrsobject value ' ',  " Subobjeto (se não usar, deixa vazio)
          lv_object  type inri-object value 'ZID_INVOIC', " Objeto de número
          lv_year    type gjahr,         " Ano (Safra)
          lv_retcode type sy-subrc.

    if i_safra is initial.
      message e024(sd) with 'Preencha a safra.'.
      exit.
    endif.

    call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr             = '1'          " Faixa de número que você criou (normalmente 01)
        object                  = lv_object
        quantity                = '1'           " Quantidade de números a pegar
        toyear                  = i_safra       " Se o objeto for por ano, incluir
      importing
        number                  = e_nr_invoice
      exceptions
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0_or_less   = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        others                  = 7.

    if sy-subrc = 0.
      at_id_invoice = e_nr_invoice.
    else.
      message e024(sd) with 'Erro ao obter número sequencial.'.
    endif.
  endmethod.


  method get_dados_invoice.


    select *
    from zsdt0399
    into corresponding fields of table it_result
    where date_create in at_rg_dtinv
      and id_invoice  in at_rg_invoice
      and status      in at_rg_status
      and bukrs      in at_rg_bukrs
      and safra      in AT_RG_SAFRA. "ZSDT0352 - Ajustes Visão Invoice #190384 - BG
    if sy-subrc eq 0.
      select *
      from zsdt0400
      into table @data(it_zsdt0400)
      for all entries in @it_result
        where id_invoice eq @it_result-id_invoice.


      select *
       from zsdt0404
       into table @data(it_zsdt0404)
       for all entries in @it_result
         where id_invoice eq @it_result-id_invoice.

      select * from kna1 into table @data(it_kna1) for all entries in @it_zsdt0400
      where kunnr eq @it_zsdt0400-kunnr.
    endif.

    loop at it_result assigning field-symbol(<wa_result>).
      if <wa_result>-status eq '1'.
        <wa_result>-status_proc = icon_defect.
      else.
        <wa_result>-status_proc = icon_complete.
      endif.

      loop at it_zsdt0400 into data(wa_zsdt0400) where id_invoice eq <wa_result>-id_invoice.
        if wa_zsdt0400-total_uss > 0.
          add wa_zsdt0400-total_uss to <wa_result>-total_uss.
        endif.
        if wa_zsdt0400-valor_antec > 0.
          add wa_zsdt0400-valor_antec to <wa_result>-valor_antec.
        endif.
        if wa_zsdt0400-valor_cad > 0.
          add wa_zsdt0400-valor_cad to <wa_result>-valor_cad.
        endif.

        read table it_kna1 into data(wa_kna1) with key kunnr = wa_zsdt0400-kunnr.
        if sy-subrc eq 0.
          <wa_result>-name1 = wa_kna1-name1.
        endif.
      endloop.

      loop at it_zsdt0404 into data(wa_zsdt0404) where id_invoice eq <wa_result>-id_invoice.
        if wa_zsdt0404-valor_cad > 0.
          add wa_zsdt0404-valor_cad to <wa_result>-util_pi.
        endif.
      endloop.
    endloop.
*  endloop.
  endmethod.


  METHOD GET_DADOS_LOTE_INSTRUCAO.

    TYPES BEGIN OF TY_TRACE.
    TYPES: ID_TRACE   TYPE NUMC15,
           LOTE       TYPE CHARG_D,
           KUNNR      TYPE KUNNR,
*       data_takeup TYPE dats,
           SAFRA      TYPE GJAHR,
           WERKS      TYPE WERKS_D,
           ALGODOEIRA TYPE ZSDT0166-ALGODOEIRA,
           DATA       TYPE ZSDT0166-DATA,
           HORA       TYPE ZSDT0166-HORA.
    TYPES END OF TY_TRACE.

    TYPES BEGIN OF TY_ITEM.
    TYPES: NRO_SOL_OV TYPE ZSDT0053-NRO_SOL_OV,
           POSNR      TYPE ZSDT0053-POSNR,
           ZMENG      TYPE ZSDT0053-ZMENG,
           BRGEW      TYPE ZSDT0053-BRGEW,
           INSTRUCAO  TYPE ZSDT0053-INSTRUCAO,
           VOLUME     TYPE ZSDT0053-VOLUM,
           LGORT      TYPE ZSDT0213-LGORT,
           VOLUM      TYPE ZSDT0213-VOLUM,
           PESO_TOTAL TYPE ZSDT0053-BRGEW.
    TYPES  END OF TY_ITEM.

    TYPES BEGIN OF TY_KNA1.
    TYPES: KUNNR TYPE KUNNR,
           WERKS TYPE WERKS_D,
           NAME1 TYPE NAME1_GP,
           NAME4 TYPE NAME1_GP.
    TYPES END OF TY_KNA1.


    TYPES BEGIN OF TY_0166.
    INCLUDE TYPE ZSDT0166.
    TYPES CHECK TYPE CHAR1.
    TYPES END OF TY_0166.

    DATA: IT_ZSDT0166               TYPE TABLE OF TY_0166,
          IT_DADOS_ITEM             TYPE TABLE OF TY_ITEM,
          IT_TRACE                  TYPE TABLE OF TY_TRACE,
          IT_KNA1_AUX               TYPE TABLE OF TY_KNA1,
          IT_KNA1                   TYPE TABLE OF TY_KNA1,
          R_STATUS                  TYPE RANGE OF Z_STATUS_TRACE,
          ZVAR_PRICE_USS            TYPE P DECIMALS 4 VALUE '2.2046',
          ZVAR_PESO                 TYPE ZSDT0053-BRGEW,
          ZVAR_PESO_TOTAL           TYPE ZSDT0053-BRGEW,
          ZVAR_QUANT_PESO_TOTAL     TYPE P,
          ZVAR_QUANT_PESO_LIQ       TYPE P,
          ZVAR_QUANT_PESO_TOTAL_BKP TYPE ZSDT0053-BRGEW,
          ZVAR_QUANT_PESO_LIQ_BKP   TYPE ZSDT0053-BRGEW.

    R_STATUS = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = 'A' ) ).

    CHECK I_INSTRUCAO IS NOT INITIAL.

    "Seleciona lotes vinculado a instrução.
    SELECT
    B~NRO_SOL_OV,
    B~KUNNR,
    D~NAME1,
    A~SAFRA,
    A~INSTRUCAO,
    A~CONTRATO,
    A~CHARG,
    A~WERKS,
    A~MATNR,
*    f~tipo,
    E~NAME AS DESC_PONT_COLE,
    A~DMBTR AS PRICE_LBS,
    F~CONTRATO_CLIENTE
*    c~tipo
*    f~volum
    FROM ZSDT0045 AS A
    INNER JOIN ZSDT0051 AS B ON B~NRO_SOL_OV EQ A~OBJEK
*    inner join zsdt0166 as c on c~contrato eq a~contrato and c~lote eq a~charg and c~status eq 'A'
*      inner join zsdt0213 as f on f~nro_sol_ov eq a~objek and f~lgort eq a~charg
      LEFT JOIN KNA1 AS D ON D~KUNNR EQ B~KUNNR
      LEFT JOIN J_1BBRANCH AS E ON E~BRANCH EQ A~WERKS
      LEFT JOIN ZSDT0143 AS F ON F~CONTRATO EQ A~CONTRATO AND F~EMPRESA EQ E~BUKRS
      INTO CORRESPONDING FIELDS OF TABLE @IT_RESULT
      WHERE A~INSTRUCAO EQ @I_INSTRUCAO.

    IF SY-SUBRC EQ 0.

      "busca se tem invoice para a instrução selecionada -- INICIO
      SELECT * FROM ZSDT0400 INTO TABLE @DATA(IT_ZSDT0400)  FOR ALL ENTRIES IN @IT_RESULT
        WHERE  NRO_SOL_OV EQ @IT_RESULT-NRO_SOL_OV
        AND CHARG EQ @IT_RESULT-CHARG
        AND CONTRATO EQ @IT_RESULT-CONTRATO.
      "busca se tem invoice para a instrução selecionada -- FIM

      SORT IT_RESULT BY KUNNR SAFRA INSTRUCAO CONTRATO CHARG WERKS TIPO.
      DELETE ADJACENT DUPLICATES FROM IT_RESULT COMPARING KUNNR SAFRA INSTRUCAO CONTRATO CHARG WERKS TIPO.

      "Dados da filial.
      IT_KNA1_AUX = VALUE #( FOR I IN IT_RESULT ( KUNNR = |{ I-WERKS ALPHA = IN }| WERKS = I-WERKS ) ).
      IF IT_KNA1_AUX IS NOT INITIAL.
        SELECT KUNNR NAME4 FROM KNA1
        INTO CORRESPONDING FIELDS OF TABLE IT_KNA1
        FOR ALL ENTRIES IN IT_KNA1_AUX
        WHERE KUNNR EQ IT_KNA1_AUX-KUNNR.
      ENDIF.

      SELECT * FROM ZSDT0166
        INTO TABLE IT_ZSDT0166
        FOR ALL ENTRIES IN IT_RESULT
        WHERE CONTRATO EQ IT_RESULT-CONTRATO
          AND LOTE     EQ IT_RESULT-CHARG.
*          and nr_provisional ne space.
*          and safra    eq @it_result-safra.

      SORT IT_ZSDT0166 BY ID DATA_ATUAL DESCENDING HORA_ATUAL DESCENDING.
      DELETE ADJACENT DUPLICATES FROM IT_ZSDT0166 COMPARING ID.

      LOOP AT IT_ZSDT0166 ASSIGNING FIELD-SYMBOL(<F_0066>).

        TRY .
            DATA(_TRACE) = IT_TRACE[
                                     LOTE        = <F_0066>-LOTE
                                     KUNNR       = <F_0066>-KUNNR
*                                   data_takeup = <f_0066>-data_takeup
                                     SAFRA       = <F_0066>-SAFRA
                                     WERKS       = <F_0066>-WERKS
                                     ALGODOEIRA  = <F_0066>-ALGODOEIRA
                                    ].
          CATCH CX_SY_ITAB_LINE_NOT_FOUND.
            CLEAR _TRACE.
        ENDTRY.

        IF _TRACE IS INITIAL.
          APPEND VALUE #(
                          ID_TRACE    = <F_0066>-ID
                          LOTE        = <F_0066>-LOTE
                          KUNNR       = <F_0066>-KUNNR
*                        data_takeup = <f_0066>-data_takeup
                          SAFRA       = <F_0066>-SAFRA
                          WERKS       = <F_0066>-WERKS
                          ALGODOEIRA  = <F_0066>-ALGODOEIRA
                          DATA        = <F_0066>-DATA
                          HORA        = <F_0066>-HORA
                        ) TO IT_TRACE.
        ELSE.
          DATA(_DATA_HORA_REG_ATUAL)    = <F_0066>-DATA && <F_0066>-HORA.
          DATA(_DATA_HORA_REG_ANTERIOR) = _TRACE-DATA && _TRACE-HORA.

          IF _DATA_HORA_REG_ANTERIOR < _DATA_HORA_REG_ATUAL.
            DELETE IT_TRACE WHERE ID_TRACE = _TRACE-ID_TRACE.
            APPEND VALUE #(
                            ID_TRACE    = <F_0066>-ID
                            LOTE        = <F_0066>-LOTE
                            KUNNR       = <F_0066>-KUNNR
*                          data_takeup = <f_0066>-data_takeup
                            SAFRA       = <F_0066>-SAFRA
                            WERKS       = <F_0066>-WERKS
                            ALGODOEIRA  = <F_0066>-ALGODOEIRA
                          ) TO IT_TRACE.
          ENDIF.
        ENDIF.
      ENDLOOP.

      LOOP AT IT_ZSDT0166 ASSIGNING <F_0066>.
        IF NOT LINE_EXISTS( IT_TRACE[ ID_TRACE = <F_0066>-ID ] ).
          <F_0066>-CHECK = ABAP_TRUE.
        ENDIF.
        IF <F_0066>-STATUS EQ 'N' OR ( NOT <F_0066>-STATUS IN R_STATUS ).
          <F_0066>-CHECK = ABAP_TRUE.
        ENDIF.
      ENDLOOP.

      SORT IT_ZSDT0166 BY CHECK.
      DATA(IT_ZSDT0166_AUX) = IT_ZSDT0166.
      DELETE IT_ZSDT0166_AUX WHERE CHECK EQ ABAP_TRUE.

      SORT IT_ZSDT0166_AUX BY NR_PROVISIONAL.
      DELETE IT_ZSDT0166 WHERE NR_PROVISIONAL EQ SPACE.

      SELECT A~NRO_SOL_OV, A~POSNR, A~LGORT, A~VOLUM, B~INSTRUCAO FROM ZSDT0213 AS A
      INNER JOIN ZSDT0053 AS B
      ON B~NRO_SOL_OV EQ A~NRO_SOL_OV
      AND B~POSNR EQ A~POSNR
      INTO TABLE @DATA(IT_ZSDT0213)
        FOR ALL ENTRIES IN @IT_RESULT
      WHERE A~NRO_SOL_OV EQ @IT_RESULT-NRO_SOL_OV
        AND A~LGORT EQ @IT_RESULT-CHARG+0(4).

      SELECT A~NRO_SOL_OV, A~POSNR, A~ZMENG, A~BRGEW, A~INSTRUCAO, A~VOLUM AS VOLUME, B~LGORT, B~VOLUM
      FROM ZSDT0053 AS A
      INNER JOIN ZSDT0213 AS B
      ON B~NRO_SOL_OV EQ A~NRO_SOL_OV
      AND B~POSNR EQ A~POSNR
      INTO TABLE @IT_DADOS_ITEM
      FOR ALL ENTRIES IN @IT_RESULT
        WHERE A~NRO_SOL_OV EQ @IT_RESULT-NRO_SOL_OV
          AND A~INSTRUCAO EQ @IT_RESULT-INSTRUCAO.


      "Totalizar os volumes por lotes.
      LOOP AT IT_RESULT ASSIGNING FIELD-SYMBOL(<WS_RESULT>).
        CLEAR: ZVAR_QUANT_PESO_TOTAL, ZVAR_QUANT_PESO_LIQ, ZVAR_QUANT_PESO_TOTAL_BKP, ZVAR_QUANT_PESO_LIQ_BKP, ZVAR_PESO, ZVAR_PESO_TOTAL.
        READ TABLE IT_KNA1_AUX INTO DATA(WA_KNA1_AUX) WITH KEY WERKS = <WS_RESULT>-WERKS.
        IF SY-SUBRC EQ 0.
          READ TABLE IT_KNA1 INTO DATA(WA_KNA1) WITH KEY KUNNR = WA_KNA1_AUX-KUNNR.
          IF SY-SUBRC EQ 0.
            <WS_RESULT>-DESC_PONT_COLE = WA_KNA1-NAME4.
          ENDIF.
        ENDIF.
 "Verifica se ja foi usado o volume em outra invoice -- INICIO
        READ TABLE IT_ZSDT0400 INTO DATA(WA_ZSDT0400) WITH KEY NRO_SOL_OV = <WS_RESULT>-NRO_SOL_OV
                                                               CHARG      = <WS_RESULT>-CHARG
                                                               CONTRATO   = <WS_RESULT>-CONTRATO.
        IF  SY-SUBRC IS NOT INITIAL.

          LOOP AT IT_ZSDT0213 INTO DATA(WS_ZSDT0213) WHERE NRO_SOL_OV EQ <WS_RESULT>-NRO_SOL_OV AND LGORT EQ <WS_RESULT>-CHARG AND INSTRUCAO EQ <WS_RESULT>-INSTRUCAO.
            ADD WS_ZSDT0213-VOLUM TO <WS_RESULT>-VOLUM.
            <WS_RESULT>-POSNR = WS_ZSDT0213-POSNR.
          ENDLOOP.
        ENDIF.
        "Verifica se ja foi usado o volume em outra invoice -- FIM

        IF <WS_RESULT>-PRICE_LBS IS NOT INITIAL.
          <WS_RESULT>-PRICE_USS = ( <WS_RESULT>-PRICE_LBS * ZVAR_PRICE_USS ).
        ENDIF.


        LOOP AT IT_DADOS_ITEM ASSIGNING FIELD-SYMBOL(<WS_DADOS_ITEM>) WHERE NRO_SOL_OV EQ <WS_RESULT>-NRO_SOL_OV
                                                                          AND INSTRUCAO EQ <WS_RESULT>-INSTRUCAO
                                                                          AND LGORT     EQ <WS_RESULT>-CHARG.

          IF <WS_DADOS_ITEM>-VOLUME IS NOT INITIAL.
            ZVAR_PESO = ( <WS_DADOS_ITEM>-BRGEW / <WS_DADOS_ITEM>-VOLUME ).
            ZVAR_PESO_TOTAL = ( ZVAR_PESO * <WS_DADOS_ITEM>-VOLUM ).

            ADD ZVAR_PESO_TOTAL TO  <WS_RESULT>-GROSS_WEIGHT.
            ADD ZVAR_PESO_TOTAL TO  ZVAR_QUANT_PESO_TOTAL_BKP.

            CLEAR: ZVAR_PESO, ZVAR_PESO_TOTAL.
            ZVAR_PESO = ( <WS_DADOS_ITEM>-ZMENG / <WS_DADOS_ITEM>-VOLUME ).
            ZVAR_PESO_TOTAL = ( ZVAR_PESO * <WS_DADOS_ITEM>-VOLUM ).

            ADD ZVAR_PESO_TOTAL TO  <WS_RESULT>-NET_WEGHT.
            ADD ZVAR_PESO_TOTAL TO  ZVAR_QUANT_PESO_LIQ_BKP.
          ENDIF.
        ENDLOOP.

        ZVAR_QUANT_PESO_TOTAL = <WS_RESULT>-GROSS_WEIGHT.
        ZVAR_QUANT_PESO_LIQ   = <WS_RESULT>-NET_WEGHT.

        <WS_RESULT>-GROSS_WEIGHT = ZVAR_QUANT_PESO_TOTAL.
        <WS_RESULT>-NET_WEGHT    = ZVAR_QUANT_PESO_LIQ.


        IF <WS_RESULT>-GROSS_WEIGHT > 0 AND <WS_RESULT>-NET_WEGHT > 0.
          <WS_RESULT>-TARE = ( <WS_RESULT>-GROSS_WEIGHT - <WS_RESULT>-NET_WEGHT ).
        ENDIF.

        IF <WS_RESULT>-NET_WEGHT > 0.
          <WS_RESULT>-TOTAL_USS = ( <WS_RESULT>-NET_WEGHT * <WS_RESULT>-PRICE_USS ).
        ENDIF.

        READ TABLE IT_ZSDT0166 INTO DATA(WS_ZSDT0166) WITH KEY CONTRATO = <WS_RESULT>-CONTRATO
                                                                  LOTE  = <WS_RESULT>-CHARG
                                                                  SAFRA = <WS_RESULT>-SAFRA
                                                                  WERKS = <WS_RESULT>-WERKS.
        IF SY-SUBRC EQ 0.

          IF WS_ZSDT0166-NR_PROVISIONAL IS INITIAL.
            READ TABLE IT_ZSDT0166 INTO DATA(WS_ZSDT0166_AUX) WITH KEY CONTRATO = WS_ZSDT0166-CONTRATO
                                                                     SAFRA  = WS_ZSDT0166-SAFRA
                                                                       LOTE = WS_ZSDT0166-LOTE
                                                                      WERKS = WS_ZSDT0166-WERKS.
            IF SY-SUBRC EQ 0.
              WS_ZSDT0166-NR_PROVISIONAL = WS_ZSDT0166_AUX-NR_PROVISIONAL.
            ENDIF.
          ENDIF.




          <WS_RESULT>-VOLUM_CONTRATO = WS_ZSDT0166-QTD_FARDOS.
          <WS_RESULT>-TIPO           = WS_ZSDT0166-TIPO.
          <WS_RESULT>-NR_PROVISIONAL = WS_ZSDT0166-NR_PROVISIONAL.
          <WS_RESULT>-MATNR          = WS_ZSDT0166-MATNR.
          <WS_RESULT>-VALOR_CAD      = ( <WS_RESULT>-TOTAL_USS - <WS_RESULT>-VALOR_ANTEC ).
          IF WS_ZSDT0166-NR_PROVISIONAL IS NOT INITIAL.
            <WS_RESULT>-VALOR_ANTEC    = ( WS_ZSDT0166-VALOR_ANTEC / <WS_RESULT>-VOLUM_CONTRATO ) * <WS_RESULT>-VOLUM.
          ENDIF.
        ENDIF.




        "Peso pelo volume entregue.
        ZVAR_PESO = <WS_RESULT>-VOLUM_CONTRATO.


*        zvar_quant_peso_total = <ws_result>-gross_weight.
*        zvar_quant_peso_liq   = <ws_result>-net_weght.

*        <ws_result>-gross_weight = zvar_quant_peso_total.
*        <ws_result>-net_weght    = zvar_quant_peso_liq.
        CLEAR: WS_ZSDT0166_AUX, WS_ZSDT0166.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  method GET_DADOS_NOTA_INVOICE.
  endmethod.


  method get_dados_provisional.

    types begin of ty_trace.
    types: id_trace   type numc15,
           lote       type charg_d,
           kunnr      type kunnr,
*       data_takeup TYPE dats,
           safra      type gjahr,
           werks      type werks_d,
           algodoeira type zsdt0166-algodoeira,
           data       type zsdt0166-data,
           hora       type zsdt0166-hora.
    types end of ty_trace.

    types begin of ty_item.
    types: nro_sol_ov type zsdt0053-nro_sol_ov,
           posnr      type zsdt0053-posnr,
           zmeng      type zsdt0053-zmeng,
           brgew      type zsdt0053-brgew,
           instrucao  type zsdt0053-instrucao,
           volume     type zsdt0053-volum,
           lgort      type zsdt0213-lgort,
           volum      type zsdt0213-volum,
           peso_total type zsdt0053-brgew.
    types  end of ty_item.

    types begin of ty_kna1.
    types: kunnr type kunnr,
           werks type werks_d,
           name1 type name1_gp,
           name4 type name1_gp.
    types end of ty_kna1.


    types begin of ty_0166.
    include type zsdt0166.
    types id_invoice type zsdt0400-id_invoice.
    types check type char1.
    types end of ty_0166.

    data: it_zsdt0166 type table of ty_0166,
          r_status    type range of z_status_trace,
          rg_bukrs    type range of bukrs,
          it_trace    type table of ty_trace,
          rg_kunnr    type range of kunnr,
          rg_safra    type range of gjahr,
          rg_CONTR    type range of bstkd,
          rg_PROV     type range of zde_provisinal,
          rg_LOTE     type range of CHARG_d,
          rg_DTTKUP   type range of sy-datum,
          rg_DTPROV   type range of sy-datum.

    data: wa_provisional    type zsde0089,
          vg_nr_provisional type zsdt0166-nr_provisional.


    free: it_result.

    select *
    from zsdt0166
    into table it_zsdt0166
    where empresa in at_rg_bukrs "rg_bukrs "Ajustes ZSDT0352 #189510 - BG
    and safra in AT_RG_SAFRA "rg_safra "Ajustes ZSDT0352 #189510 - BG
    and contrato in at_rg_contr
    and kunnr    in at_rg_kunnr
    and   nr_provisional in at_rg_prov
    and nr_provisional ne space.

    sort it_zsdt0166 by id data_atual descending hora_atual descending.
    delete adjacent duplicates from it_zsdt0166 comparing id.

    loop at it_zsdt0166 assigning field-symbol(<f_0066>).

      try .
          data(_trace) = it_trace[
                                   lote        = <f_0066>-lote
                                   kunnr       = <f_0066>-kunnr
*                                   data_takeup = <f_0066>-data_takeup
                                   safra       = <f_0066>-safra
                                   werks       = <f_0066>-werks
                                   algodoeira  = <f_0066>-algodoeira
                                  ].
        catch cx_sy_itab_line_not_found.
          clear _trace.
      endtry.

      if _trace is initial.
        append value #(
                        id_trace    = <f_0066>-id
                        lote        = <f_0066>-lote
                        kunnr       = <f_0066>-kunnr
*                        data_takeup = <f_0066>-data_takeup
                        safra       = <f_0066>-safra
                        werks       = <f_0066>-werks
                        algodoeira  = <f_0066>-algodoeira
                        data        = <f_0066>-data
                        hora        = <f_0066>-hora
                      ) to it_trace.
      else.
        data(_data_hora_reg_atual)    = <f_0066>-data && <f_0066>-hora.
        data(_data_hora_reg_anterior) = _trace-data && _trace-hora.

        if _data_hora_reg_anterior < _data_hora_reg_atual.
          delete it_trace where id_trace = _trace-id_trace.
          append value #(
                          id_trace    = <f_0066>-id
                          lote        = <f_0066>-lote
                          kunnr       = <f_0066>-kunnr
*                          data_takeup = <f_0066>-data_takeup
                          safra       = <f_0066>-safra
                          werks       = <f_0066>-werks
                          algodoeira  = <f_0066>-algodoeira
                        ) to it_trace.
        endif.
      endif.
    endloop.

    loop at it_zsdt0166 assigning <f_0066>.
      if not line_exists( it_trace[ id_trace = <f_0066>-id ] ).
        <f_0066>-check = abap_true.
      endif.
      if <f_0066>-status eq 'N' or ( not <f_0066>-status in r_status ).
        <f_0066>-check = abap_true.
      endif.
    endloop.

    sort it_zsdt0166 by check.
    delete it_zsdt0166 where check eq abap_true.

    if it_zsdt0166 is not initial.
      select *
      from kna1
      into table @data(it_kna1)
        for all entries in @it_zsdt0166
      where kunnr eq @it_zsdt0166-kunnr.

      if  sy-subrc = 0.
        select  *
          from adrc
          into table @data(it_ADRC)
          for all entries in @it_kna1
         where addrnumber eq @it_kna1-adrnr
           and date_to    >= @sy-datum.
      endif.

      "Seleção dados do contrato/solicição/dados adiantamento.
      select a~nro_sol_ov, a~vkorg, a~bstkd as contrato, b~adiant, b~dmbtr as valor, b~nr_provis_inv, c~belnr, c~dmbtr, c~wrbtr, c~augdt "d~bldat "Ajustes ZSDT0352 #189510 - BG
      from zsdt0051 as a
      left join zsdt0054 as b on b~nro_sol_ov eq a~nro_sol_ov
      inner join bseg as c on c~belnr eq b~adiant and c~bukrs eq a~vkorg
      inner join bkpf as d on d~belnr eq c~belnr and d~gjahr eq c~gjahr
      into table @data(it_dados_adiant)
        for all entries in @it_zsdt0166
      where a~bstkd eq  @it_zsdt0166-contrato
        and d~stblg eq @space.
      sort it_dados_adiant by augdt. "AUGDT "Ajustes ZSDT0352 #189510 - BG

      "Dados salvo rel.provisional.
      select nr_provisional, contrato, kunnr, bukrs, dt_ret_corretora, observacao
      from zsdt0409
      into table @data(it_zsdt0409)
        for all entries in @it_zsdt0166
        where nr_provisional eq @it_zsdt0166-nr_provisional
          and contrato eq @it_zsdt0166-contrato
          and bukrs    eq @it_zsdt0166-empresa.
    endif.


    if it_zsdt0166[] is not initial.
      data(it_zsdt0166_aux) = it_zsdt0166.
      sort it_zsdt0166 by contrato nr_provisional.
      delete adjacent duplicates from it_zsdt0166 comparing contrato nr_provisional.

      sort it_dados_adiant by vkorg contrato nr_provis_inv.

      loop at it_zsdt0166 into data(wa_zsdt0166).
        move-corresponding wa_zsdt0166 to wa_provisional.
        wa_provisional-dt_takeup =  wa_zsdt0166-data_takeup.
        wa_provisional-mes_takeup = |{ wa_zsdt0166-data_takeup+4(2) }|.
        wa_provisional-bukrs =  wa_zsdt0166-empresa.
        wa_provisional-nr_provisional =  wa_zsdt0166-nr_provisional.


        read table it_kna1 into data(w_kna1) with key kunnr = wa_zsdt0166-kunnr.
        if sy-subrc is initial.
          read table it_ADRC into data(w_ADRC) with key addrnumber = w_kna1-adrnr.
          if sy-subrc is initial.
            concatenate  w_adrc-name1 w_adrc-name2 into wa_provisional-desc_cliente separated by space.
          endif.
        endif.
        loop at it_zsdt0166_aux assigning field-symbol(<ws_zsdt0166>) where contrato eq wa_zsdt0166-contrato and nr_provisional eq wa_zsdt0166-nr_provisional.
          if <ws_zsdt0166>-peso_lote > 0.
            add <ws_zsdt0166>-peso_lote to wa_provisional-qtd_ton.
          endif.

          if <ws_zsdt0166>-valor_total > 0.
            add <ws_zsdt0166>-valor_total to wa_provisional-val_total.
          endif.

          if <ws_zsdt0166>-valor_antec > 0.
            add <ws_zsdt0166>-valor_antec to wa_provisional-val_adiant.
          endif.
        endloop.

        if wa_provisional-qtd_ton is not initial.
          "Converter em tonelada.
          wa_provisional-qtd_ton = ( wa_provisional-qtd_ton / 1000 ).
        endif.

        if wa_provisional-val_adiant is not initial.
          wa_provisional-per_adiant = ( wa_provisional-val_adiant / wa_provisional-val_total ) * 100.
        endif.

        if wa_provisional-val_adiant is not initial.
          wa_provisional-previsao_cad = ( wa_provisional-val_total - wa_provisional-val_adiant ).
        endif.

        wa_provisional-dt_emissao = <ws_zsdt0166>-DATA_ATUAL.  "Ajustes ZSDT0352 #189510 - BG
        wa_provisional-qdt_dia_takeup_e = ( wa_zsdt0166-data - wa_zsdt0166-data_takeup ).

        loop at it_dados_adiant into data(wa_dados_adiant) where contrato = wa_zsdt0166-contrato
                                                             and    vkorg = wa_zsdt0166-empresa.

          clear: vg_nr_provisional.
          vg_nr_provisional = wa_dados_adiant-nr_provis_inv+0(4).

          if vg_nr_provisional eq wa_zsdt0166-nr_provisional.

            if wa_provisional-doc_cont_pi is initial.
              wa_provisional-doc_cont_pi = wa_dados_adiant-belnr.
            else.
              wa_provisional-doc_cont_pi = |{ wa_provisional-doc_cont_pi }/{ wa_dados_adiant-belnr }|.
            endif.

            if wa_provisional-nro_sol_ov is initial and wa_dados_adiant-nro_sol_ov ne wa_provisional-nro_sol_ov.
              wa_provisional-nro_sol_ov = wa_dados_adiant-nro_sol_ov.
            else.
              wa_provisional-nro_sol_ov = |{ wa_provisional-nro_sol_ov }/{ wa_dados_adiant-nro_sol_ov }|.
            endif.

            if wa_dados_adiant-wrbtr is not initial AND wa_dados_adiant-AUGDT IS NOT INITIAL. "Ajustes ZSDT0352 #189510 - BG
              add wa_dados_adiant-wrbtr to wa_provisional-recebifo_usd.
            endif.

            wa_provisional-a_receber_usd = ( wa_provisional-val_adiant - wa_provisional-recebifo_usd ).

            wa_provisional-dt_pgto_pi = wa_dados_adiant-AUGDT. "wa_dados_adiant-AUGDT "Ajustes ZSDT0352 #189510 - BG
            wa_provisional-ano = |{ wa_dados_adiant-AUGDT+0(4) }|. ""wa_dados_adiant-AUGDT "Ajustes ZSDT0352 #189510 - BG
          endif.
        endloop.

        if wa_provisional-dt_ret_corretora is not initial.
          wa_provisional-dia_emissao_ret = ( wa_provisional-dt_ret_corretora - wa_provisional-dt_emissao ).
          wa_provisional-dia_ret_pgto = ( wa_provisional-dt_pgto_pi - wa_provisional-dt_ret_corretora ).
        endif.

        wa_provisional-qtd_tons_rec = ( ( wa_provisional-per_adiant / 100 ) * wa_provisional-qtd_ton ).

        wa_provisional-qtd_receber =  ( ( wa_provisional-per_adiant / 100 ) * wa_provisional-qtd_ton ).

        read table it_zsdt0409 into data(wa_zsdt0409) with key nr_provisional = wa_zsdt0166-nr_provisional
                                                               contrato       = wa_zsdt0166-contrato
                                                               bukrs          = wa_zsdt0166-empresa.
        if sy-subrc eq 0.
          wa_provisional-dt_ret_corretora = wa_zsdt0409-dt_ret_corretora.
          wa_provisional-observacao       = wa_zsdt0409-observacao.
*          wa_provisional-dt_emissao       = wa_zsdt0409-dt_emissao.

          if wa_provisional-dt_ret_corretora is not initial.
            wa_provisional-dia_emissao_ret = ( wa_provisional-dt_ret_corretora - wa_provisional-dt_emissao ).
            wa_provisional-dia_ret_pgto = ( wa_provisional-dt_pgto_pi - wa_provisional-dt_ret_corretora ).
          endif.
        endif.
        append wa_provisional to it_result.
        clear: wa_provisional, wa_zsdt0409, wa_dados_adiant.
      endloop.
    endif.

  endmethod.


  method get_saldo_lote_cliente.

    types begin of ty_trace.
    types: id_trace   type numc15,
           lote       type charg_d,
           kunnr      type kunnr,
*       data_takeup TYPE dats,
           safra      type gjahr,
           werks      type werks_d,
           algodoeira type zsdt0166-algodoeira,
           data       type zsdt0166-data,
           hora       type zsdt0166-hora.
    types end of ty_trace.

    types begin of ty_item.
    types: nro_sol_ov type zsdt0053-nro_sol_ov,
           posnr      type zsdt0053-posnr,
           zmeng      type zsdt0053-zmeng,
           brgew      type zsdt0053-brgew,
           instrucao  type zsdt0053-instrucao,
           volume     type zsdt0053-volum,
           lgort      type zsdt0213-lgort,
           volum      type zsdt0213-volum,
           peso_total type zsdt0053-brgew.
    types  end of ty_item.

    types begin of ty_kna1.
    types: kunnr type kunnr,
           werks type werks_d,
           name1 type name1_gp,
           name4 type name1_gp.
    types end of ty_kna1.


    types begin of ty_0166.
    include type zsdt0166.
    types check type char1.
    types end of ty_0166.

    data: it_zsdt0166               type table of ty_0166,
          it_dados_item             type table of ty_item,
          it_trace                  type table of ty_trace,
          it_kna1_aux               type table of ty_kna1,
          it_kna1                   type table of ty_kna1,
          r_status                  type range of z_status_trace,
          zvar_price_uss            type p decimals 4 value '2.2046',
          zvar_peso                 type zsdt0053-brgew,
          zvar_peso_total           type zsdt0053-brgew,
          zvar_quant_peso_total     type p,
          zvar_quant_peso_liq       type p,
          zvar_quant_peso_total_bkp type zsdt0053-brgew,
          zvar_quant_peso_liq_bkp   type zsdt0053-brgew.

    r_status = value #( ( sign = 'I' option = 'EQ' low = 'A' ) ).

    check i_kunnr is not initial and i_SAFRA is not initial.

    if at_rg_lote is not initial.
      select * from zsdt0166
        into table @it_zsdt0166
        where kunnr eq @i_kunnr
*        and lote not in @at_rg_lote
        and nr_provisional ne @space.
    else.
      select * from zsdt0166
      into table @it_zsdt0166
      where kunnr eq @i_kunnr
      and nr_provisional ne @space.
    endif.

    sort it_zsdt0166 by id data_atual descending hora_atual descending.
    delete adjacent duplicates from it_zsdt0166 comparing id.

    loop at it_zsdt0166 assigning field-symbol(<f_0066>).

      try .
          data(_trace) = it_trace[
                                   lote        = <f_0066>-lote
                                   kunnr       = <f_0066>-kunnr
*                                   data_takeup = <f_0066>-data_takeup
                                   safra       = <f_0066>-safra
                                   werks       = <f_0066>-werks
                                   algodoeira  = <f_0066>-algodoeira
                                  ].
        catch cx_sy_itab_line_not_found.
          clear _trace.
      endtry.

      if _trace is initial.
        append value #(
                        id_trace    = <f_0066>-id
                        lote        = <f_0066>-lote
                        kunnr       = <f_0066>-kunnr
*                        data_takeup = <f_0066>-data_takeup
                        safra       = <f_0066>-safra
                        werks       = <f_0066>-werks
                        algodoeira  = <f_0066>-algodoeira
                        data        = <f_0066>-data
                        hora        = <f_0066>-hora
                      ) to it_trace.
      else.
        data(_data_hora_reg_atual)    = <f_0066>-data && <f_0066>-hora.
        data(_data_hora_reg_anterior) = _trace-data && _trace-hora.

        if _data_hora_reg_anterior < _data_hora_reg_atual.
          delete it_trace where id_trace = _trace-id_trace.
          append value #(
                          id_trace    = <f_0066>-id
                          lote        = <f_0066>-lote
                          kunnr       = <f_0066>-kunnr
*                          data_takeup = <f_0066>-data_takeup
                          safra       = <f_0066>-safra
                          werks       = <f_0066>-werks
                          algodoeira  = <f_0066>-algodoeira
                        ) to it_trace.
        endif.
      endif.
    endloop.

    loop at it_zsdt0166 assigning <f_0066>.
      if not line_exists( it_trace[ id_trace = <f_0066>-id ] ).
        <f_0066>-check = abap_true.
      endif.
      if <f_0066>-status eq 'N' or ( not <f_0066>-status in r_status ).
        <f_0066>-check = abap_true.
      endif.
    endloop.

    sort it_zsdt0166 by check.
    delete it_zsdt0166 where check eq abap_true.
    sort it_zsdt0166 by lote.
*    delete it_zsdt0166 where ( lote not in at_rg_lote and nr_provisional not in at_rg_prov ).

    if it_zsdt0166 is not initial.
      it_result = value #( for i in it_zsdt0166 (
      contrato       = i-contrato
      nr_provisional = i-nr_provisional
               charg = i-lote
         valor_antec = i-valor_antec
*         valor_cad   = ( i-valor_total    - i-valor_antec )
         observacao  = |{ i-nr_provisional }/{ i-lote }|
      ) ).
    endif.
  endmethod.


  method save_dados_invoice.
    data: it_zsdt0399 type table of zsdt0399, "Tabela principal cabeçalho invoice.
          it_zsdt0400 type table of zsdt0400,
          it_zsdt0404 type table of zsdt0404. "Tabela dados itens instrução/contrato/provisional/lote.

    free: it_zsdt0399, it_zsdt0400.
    "Gerar numero sequencia.
    if i_header-id_invoice is initial.
      data(nr_invoice) = gerar_seq_invoice( i_header-safra ).
    else.
      nr_invoice = i_header-id_invoice.
    endif.

    check nr_invoice is not initial.

    "Dados cabeçalho.
    it_zsdt0399 = value #( ( id_invoice = nr_invoice
                                safra = i_header-safra
                                bukrs = i_header-bukrs
                             shippers = i_header-shippers
                            instrucao = i_header-instrucao
                       instrucao_orig = i_header-instrucao_orig
                              loading = i_header-loading
                          destination = i_header-destination
                               vessel = i_header-vessel
                 description_of_goods = i_header-description_of_goods
                              bl_date = i_header-bl_date
                            nr_conhec = i_header-nr_conhec
                             data_receb = i_header-data_receb
                             valor_receb = i_header-valor_receb
                                kunnr = i_header-kunnr
                               status = i_header-status
                          desc_status = i_header-desc_status
                           numero_due = i_header-numero_due
                           numero_ruc = i_header-numero_ruc
                          user_create = sy-uname
                          date_create = sy-datum
                          time_create = sy-uzeit
                           ) ).


    "Dados do itens.
    it_zsdt0400 = value #( for i in i_itens (
                           id_invoice  = nr_invoice
                                charg  = i-charg
                             contrato  = i-contrato
                           nro_sol_ov  = i-nro_sol_ov
                                posnr  = i-posnr
                                volum  = i-volum
                                kunnr  = i-kunnr
                                name1  = i-name1
                                safra  = i-safra
                             instrucao = i-instrucao
                     contrato_cliente  = i-contrato_cliente
                                werks  = i-werks
                       desc_pont_cole  = i-desc_pont_cole
                                 tipo  = i-tipo
                                matnr  = i-matnr
                            net_weght  = i-net_weght
                         gross_weight  = i-gross_weight
                                tare   = i-tare
                            price_lbs  = i-price_lbs
                            price_uss  = i-price_uss
                            total_uss  = i-total_uss
*                    belnr_provisional  = i-
*                    gjahr_provisional  = i-
                       nr_provisional  = i-nr_provisional
                          valor_antec  = i-valor_antec
                            valor_cad  = i-valor_cad
    ) ).


    "Dados do itens.
    it_zsdt0404 = value #( for t in i_lote_avariado (
                                    id_invoice  = nr_invoice
                                    contrato    = t-contrato
                                contrato_ref    = t-contrato_ref
                          nr_provisional_ref    = t-nr_provisional_ref
                                    charg       = t-charg
                                nr_provisional  = t-nr_provisional
                                valor_antec     = t-valor_antec
                                valor_cad       = t-valor_cad
                                observacao      = t-observacao
      ) ).





    if it_zsdt0399 is not initial.
      modify zsdt0399 from table it_zsdt0399.
    endif.

    if it_zsdt0400 is not initial.
      modify zsdt0400 from table it_zsdt0400.
    endif.

    if it_zsdt0404 is not initial.
      modify zsdt0404 from table it_zsdt0404.
    endif.

    if sy-subrc eq 0.
      commit work.
      r_result =  abap_true.
      e_id_invoice = nr_invoice.
    endif.
  endmethod.


  method SAVE_MEMOR_DATA_PROVISIONAL.
    data: it_ZSDT0409 type table of zsdt0409.
    clear: e_return.

    check i_data is not initial.

    move-corresponding i_data to it_ZSDT0409.
*    sort it_ZSDT0409 by dt_ret_corretora.
*    delete it_ZSDT0409 where dt_ret_corretora eq space.

    modify zsdt0409 from table it_ZSDT0409.
    commit work.
    if sy-subrc eq 0.
      e_return = abap_true.
    endif.
  endmethod.


  method set_parametro_filter.

    check i_param is not initial.

    loop at i_param into data(wa_param).
      case wa_param-fieldnm.
        when 'BUKRS'.
          append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to at_rg_bukrs.
        when 'KUNNR'.
          append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to at_rg_kunnr.
        when 'SAFRA'.
          append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to at_rg_safra.
        when 'CONTR'.
          append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to at_rg_contr.
        when 'PROV'.
          append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to at_rg_prov.
        when 'LOTE'.
          append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to at_rg_lote.
        when 'DTTKUP'.
          append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to at_rg_dttkup.
        when 'DTPROV'.
          append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to at_rg_dtprov.
        when 'DTINV'.
          append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to at_rg_dtinv.
        when 'STATUS'.
          append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to at_rg_status.
        when 'ID_INVOICE'.
          append value #( sign = wa_param-sign option = wa_param-option low = wa_param-low high = wa_param-high ) to at_rg_invoice.
      endcase.
    endloop.
  endmethod.


  method GET_DADOS_INSTRUCAO.
    free: it_result.
    select a~bukrs, a~instrucao, a~safra, b~kunnr, c~name1
    from zsdt0045 as a
    inner join zsdt0051 as b on b~nro_sol_ov eq  a~objek
    left join kna1 as c on c~kunnr eq b~kunnr
    into table @it_result
    where a~bukrs in @at_rg_bukrs
      and a~instrucao in @at_rg_instr
      and a~contrato in @at_rg_contr
      and a~safra in @at_rg_safra
      and b~kunnr in @at_rg_kunnr.

    sort it_result by bukrs instrucao safra kunnr.
    delete adjacent duplicates from it_result comparing bukrs instrucao safra kunnr.

  endmethod.


  method get_invoice_geradas.

    check i_id_invoice is not initial.

    select single * from zsdt0399
    into corresponding fields of e_header
    where id_invoice eq i_id_invoice.

    select *
    from zsdt0400
    into corresponding fields of table e_itens
    where id_invoice eq i_id_invoice.

    select *
    from zsdt0404
    into corresponding fields of table e_lote_avariado
    where id_invoice eq i_id_invoice.
    sort e_lote_avariado by nr_provisional charg.

    select single a~numero_ruc, c~numero_due, b~nr_conhec, b~dt_data, d~ds_nome_transpor
    from zsdt0053 as a
    left join znom_conhec as b on b~id_nomeacao_tran eq a~id_nomeacao_tran
    left join zsdt0170 as c on c~numero_ruc eq a~numero_ruc
    left join znom_transporte as d on d~id_nomeacao_tran eq a~id_nomeacao_tran
    into ( @e_header-numero_ruc, @e_header-numero_due, @e_header-nr_conhec, @e_header-bl_date, @e_header-vessel )
      where a~instrucao eq @e_header-instrucao.

  endmethod.


  METHOD GET_ABATIMENTO_PI.

    TYPES BEGIN OF TY_TRACE.
    TYPES: ID_TRACE   TYPE NUMC15,
           LOTE       TYPE CHARG_D,
           KUNNR      TYPE KUNNR,
*       data_takeup TYPE dats,
           SAFRA      TYPE GJAHR,
           WERKS      TYPE WERKS_D,
           ALGODOEIRA TYPE ZSDT0166-ALGODOEIRA,
           DATA       TYPE ZSDT0166-DATA,
           HORA       TYPE ZSDT0166-HORA.
    TYPES END OF TY_TRACE.

    TYPES BEGIN OF TY_ITEM.
    TYPES: NRO_SOL_OV TYPE ZSDT0053-NRO_SOL_OV,
           POSNR      TYPE ZSDT0053-POSNR,
           ZMENG      TYPE ZSDT0053-ZMENG,
           BRGEW      TYPE ZSDT0053-BRGEW,
           INSTRUCAO  TYPE ZSDT0053-INSTRUCAO,
           VOLUME     TYPE ZSDT0053-VOLUM,
           LGORT      TYPE ZSDT0213-LGORT,
           VOLUM      TYPE ZSDT0213-VOLUM,
           PESO_TOTAL TYPE ZSDT0053-BRGEW.
    TYPES  END OF TY_ITEM.

    TYPES BEGIN OF TY_KNA1.
    TYPES: KUNNR TYPE KUNNR,
           WERKS TYPE WERKS_D,
           NAME1 TYPE NAME1_GP,
           NAME4 TYPE NAME1_GP.
    TYPES END OF TY_KNA1.


    TYPES BEGIN OF TY_0166.
    INCLUDE TYPE ZSDT0166.
    TYPES ID_INVOICE TYPE ZSDT0400-ID_INVOICE.
*    types data_receb type zsdt0399-data_receb.
*    types valor_receb type zsdt0399-valor_receb.
    TYPES NAME1 TYPE NAME1_GP.
    TYPES CHECK TYPE CHAR1.
    TYPES END OF TY_0166.

    DATA: IT_ZSDT0166               TYPE TABLE OF TY_0166,
          IT_DADOS_ITEM             TYPE TABLE OF TY_ITEM,
          IT_TRACE                  TYPE TABLE OF TY_TRACE,
          IT_KNA1_AUX               TYPE TABLE OF TY_KNA1,
          IT_KNA1                   TYPE TABLE OF TY_KNA1,
          WA_RESULT                 TYPE ZSDE0101,
          R_STATUS                  TYPE RANGE OF Z_STATUS_TRACE,
          ZVAR_PRICE_USS            TYPE P DECIMALS 4 VALUE '2.2046',
          ZVAR_PESO                 TYPE ZSDT0053-BRGEW,
          ZVAR_PESO_TOTAL           TYPE ZSDT0053-BRGEW,
          ZVAR_QUANT_PESO_TOTAL     TYPE P,
          ZVAR_QUANT_PESO_LIQ       TYPE P,
          ZVAR_QUANT_PESO_TOTAL_BKP TYPE ZSDT0053-BRGEW,
          ZVAR_QUANT_PESO_LIQ_BKP   TYPE ZSDT0053-BRGEW,
          ZVAR_INVOICE_NOT_EXIT     TYPE CHAR01,
          ZVAR_TOTAL_ADIANT         TYPE ZSDT0053-BRGEW,
          ZVAR_TABIX                TYPE SY-TABIX,
          ZVAR_FORM_NO_ZERO         TYPE CHAR20,
          ZVAR_DIF                  TYPE ZSDT0053-BRGEW,
          V_ANTECIPACAO_ANT         TYPE ZSDT0053-BRGEW, "BG 06/10/2025
          v_PI_ANT                  TYPE ZSDT0166-NR_PROVISIONAL. "BG 06/10/2025

    FREE: IT_RESULT.

    R_STATUS = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = 'A' ) ).

*    if at_rg_invoice[] is not initial.
*    select a~*, b~id_invoice, c~name1 from zsdt0166 as a
*    inner join zsdt0400 as b on b~nr_provisional eq a~nr_provisional and b~contrato eq a~contrato
**   left join zsdt0399 as d on d~id_invoice eq b~id_invoice
*    inner join kna1 as c on c~kunnr eq a~kunnr
*    into corresponding fields of table @it_zsdt0166
*    where a~nr_provisional in @at_rg_prov
*    and   a~contrato in @at_rg_contr
*    and   a~safra in @at_rg_safra
*    and   a~empresa in @at_rg_bukrs
*    and   a~kunnr   in @at_rg_kunnr
*    and a~nr_provisional ne @space
*    and b~id_invoice in @at_rg_invoice.

*    else.

    SELECT A~*, B~ID_INVOICE, C~NAME1 FROM ZSDT0166 AS A
    LEFT JOIN ZSDT0400 AS B ON B~NR_PROVISIONAL EQ A~NR_PROVISIONAL AND B~CONTRATO EQ A~CONTRATO
*   left join zsdt0399 as d on d~id_invoice eq b~id_invoice
    LEFT JOIN KNA1 AS C ON C~KUNNR EQ A~KUNNR
    INTO CORRESPONDING FIELDS OF TABLE @IT_ZSDT0166
    WHERE A~NR_PROVISIONAL IN @AT_RG_PROV
    AND   A~CONTRATO IN @AT_RG_CONTR
    AND   A~SAFRA IN @AT_RG_SAFRA
    AND   A~EMPRESA IN @AT_RG_BUKRS
    AND   A~KUNNR   IN @AT_RG_KUNNR
    AND A~NR_PROVISIONAL NE @SPACE
    AND B~ID_INVOICE IN @AT_RG_INVOICE.
*    endif.

    SORT IT_ZSDT0166 BY ID DATA_ATUAL DESCENDING HORA_ATUAL DESCENDING.
    DELETE ADJACENT DUPLICATES FROM IT_ZSDT0166 COMPARING ID.

    LOOP AT IT_ZSDT0166 ASSIGNING FIELD-SYMBOL(<F_0066>).

      TRY .
          DATA(_TRACE) = IT_TRACE[
                                   LOTE        = <F_0066>-LOTE
                                   KUNNR       = <F_0066>-KUNNR
*                                   data_takeup = <f_0066>-data_takeup
                                   SAFRA       = <F_0066>-SAFRA
                                   WERKS       = <F_0066>-WERKS
                                   ALGODOEIRA  = <F_0066>-ALGODOEIRA
                                  ].
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          CLEAR _TRACE.
      ENDTRY.

      IF _TRACE IS INITIAL.
        APPEND VALUE #(
                        ID_TRACE    = <F_0066>-ID
                        LOTE        = <F_0066>-LOTE
                        KUNNR       = <F_0066>-KUNNR
*                        data_takeup = <f_0066>-data_takeup
                        SAFRA       = <F_0066>-SAFRA
                        WERKS       = <F_0066>-WERKS
                        ALGODOEIRA  = <F_0066>-ALGODOEIRA
                        DATA        = <F_0066>-DATA
                        HORA        = <F_0066>-HORA
                      ) TO IT_TRACE.
      ELSE.
        DATA(_DATA_HORA_REG_ATUAL)    = <F_0066>-DATA && <F_0066>-HORA.
        DATA(_DATA_HORA_REG_ANTERIOR) = _TRACE-DATA && _TRACE-HORA.

        IF _DATA_HORA_REG_ANTERIOR < _DATA_HORA_REG_ATUAL.
          DELETE IT_TRACE WHERE ID_TRACE = _TRACE-ID_TRACE.
          APPEND VALUE #(
                          ID_TRACE    = <F_0066>-ID
                          LOTE        = <F_0066>-LOTE
                          KUNNR       = <F_0066>-KUNNR
*                          data_takeup = <f_0066>-data_takeup
                          SAFRA       = <F_0066>-SAFRA
                          WERKS       = <F_0066>-WERKS
                          ALGODOEIRA  = <F_0066>-ALGODOEIRA
                        ) TO IT_TRACE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT IT_ZSDT0166 ASSIGNING <F_0066>.
      IF NOT LINE_EXISTS( IT_TRACE[ ID_TRACE = <F_0066>-ID ] ).
        <F_0066>-CHECK = ABAP_TRUE.
      ENDIF.
      IF <F_0066>-STATUS EQ 'N' OR ( NOT <F_0066>-STATUS IN R_STATUS ).
        <F_0066>-CHECK = ABAP_TRUE.
      ENDIF.
    ENDLOOP.

    SORT IT_ZSDT0166 BY CHECK.
    DELETE IT_ZSDT0166 WHERE CHECK EQ ABAP_TRUE.

    SELECT
    A~ID_INVOICE,
    A~CHARG,
    A~CONTRATO,
    A~NRO_SOL_OV,
    A~POSNR,
    A~NR_PROVISIONAL,
    A~KUNNR,
    A~SAFRA,
    A~INSTRUCAO,
    A~CONTRATO_CLIENTE,
    A~WERKS,
    A~DESC_PONT_COLE,
    A~TIPO,
    A~VOLUM,
    A~MATNR,
    A~TARE,
    A~NET_WEGHT,
    A~GROSS_WEIGHT,
    A~PRICE_LBS,
    A~PRICE_USS,
    A~TOTAL_USS,
    A~BELNR_PROVISIONAL,
    A~GJAHR_PROVISIONAL,
    A~VALOR_ANTEC,
    A~VALOR_CAD,
    B~NAME1,
    C~DATA_RECEB,
    C~VALOR_RECEB
    FROM ZSDT0400 AS A
    LEFT JOIN KNA1 AS B ON B~KUNNR EQ A~KUNNR
    LEFT JOIN ZSDT0399 AS C ON C~ID_INVOICE EQ A~ID_INVOICE
    INTO TABLE @DATA(IT_ITENS)
    FOR ALL ENTRIES IN @IT_ZSDT0166
    WHERE A~CONTRATO EQ @IT_ZSDT0166-CONTRATO
      AND A~NR_PROVISIONAL EQ @IT_ZSDT0166-NR_PROVISIONAL
      AND A~ID_INVOICE EQ @IT_ZSDT0166-ID_INVOICE.

    SELECT *
    FROM ZSDT0404
    INTO TABLE @DATA(IT_LOTE_AVARIADO)
    FOR ALL ENTRIES IN @IT_ITENS
    WHERE ID_INVOICE EQ @IT_ITENS-ID_INVOICE.

    DATA(IT_ZSDT0166_AUX) = IT_ZSDT0166.
    SORT IT_ZSDT0166 BY CONTRATO NR_PROVISIONAL ID_INVOICE.
    DELETE ADJACENT DUPLICATES FROM IT_ZSDT0166 COMPARING ID_INVOICE CONTRATO NR_PROVISIONAL .

    SORT IT_ITENS BY CONTRATO NR_PROVISIONAL ID_INVOICE.
    DATA(IT_ITENS_AUX) = IT_ITENS.
    DELETE ADJACENT DUPLICATES FROM IT_ITENS COMPARING  ID_INVOICE CONTRATO NR_PROVISIONAL.

    CLEAR: ZVAR_INVOICE_NOT_EXIT.

    LOOP AT IT_ZSDT0166 ASSIGNING FIELD-SYMBOL(<WA_ZSDT0166>).

      LOOP AT IT_ZSDT0166_AUX ASSIGNING FIELD-SYMBOL(<WS_ZSDT0166>) WHERE CONTRATO EQ <WA_ZSDT0166>-CONTRATO AND NR_PROVISIONAL EQ <WA_ZSDT0166>-NR_PROVISIONAL.

        IF <WS_ZSDT0166>-VALOR_TOTAL > 0.
          ADD <WS_ZSDT0166>-VALOR_TOTAL TO WA_RESULT-TOTAL_PI.
        ENDIF.

        IF <WS_ZSDT0166>-VALOR_ANTEC > 0.
          ADD <WS_ZSDT0166>-VALOR_ANTEC TO WA_RESULT-VALOR_ANTEC_PI.
        ENDIF.

        IF <WS_ZSDT0166>-LOTE > 0.
          ADD <WS_ZSDT0166>-LOTE TO WA_RESULT-VOLUM.
        ENDIF.
      ENDLOOP.

      WA_RESULT-CONTRATO        = <WA_ZSDT0166>-CONTRATO.
      WA_RESULT-NR_PROVISIONAL  = <WA_ZSDT0166>-NR_PROVISIONAL.
      WA_RESULT-VALOR_CAD_PI    = ( WA_RESULT-TOTAL_PI - WA_RESULT-VALOR_ANTEC_PI ).
      WA_RESULT-KUNNR           = <WA_ZSDT0166>-KUNNR.
      WA_RESULT-NAME1           = <WA_ZSDT0166>-NAME1.
      ZVAR_INVOICE_NOT_EXIT        = ABAP_TRUE.


      CLEAR: ZVAR_TABIX, ZVAR_TOTAL_ADIANT.
      LOOP AT IT_ITENS INTO DATA(WA_ITENS) WHERE  CONTRATO = <WA_ZSDT0166>-CONTRATO
                                            AND  NR_PROVISIONAL = <WA_ZSDT0166>-NR_PROVISIONAL
                                            AND ID_INVOICE      = <WA_ZSDT0166>-ID_INVOICE.
        ADD 1 TO ZVAR_TABIX.


        LOOP AT IT_ITENS_AUX ASSIGNING FIELD-SYMBOL(<WA_ITENS_AUX>) WHERE CONTRATO       = WA_ITENS-CONTRATO
                                                                      AND NR_PROVISIONAL = WA_ITENS-NR_PROVISIONAL
                                                                      AND ID_INVOICE     = WA_ITENS-ID_INVOICE.

          ADD <WA_ITENS_AUX>-TOTAL_USS TO WA_RESULT-TOTAL_INV.
          ADD <WA_ITENS_AUX>-VALOR_ANTEC TO WA_RESULT-VALOR_ANTEC_INV.
          ADD <WA_ITENS_AUX>-VALOR_CAD TO WA_RESULT-VALOR_CAD_INV.
          ADD <WA_ITENS_AUX>-VOLUM TO WA_RESULT-VOLUM_INV.
        ENDLOOP.

        ADD WA_RESULT-VALOR_ANTEC_INV TO ZVAR_TOTAL_ADIANT.

        IF ZVAR_TABIX > 1.
          WA_RESULT-VALOR_ANTEC_PI = WA_RESULT-DIFERENCA_CAD.
        ELSE.
*          wa_result-valor_antec_pi = wa_result-diferenca_cad.
        ENDIF.


        IF V_ANTECIPACAO_ANT IS INITIAL AND V_PI_ANT IS INITIAL.
          WA_RESULT-DIFERENCA_CAD = ( WA_RESULT-VALOR_ANTEC_PI - WA_RESULT-VALOR_ANTEC_INV ).
          V_ANTECIPACAO_ANT = WA_RESULT-DIFERENCA_CAD.
          V_PI_ANT = WA_RESULT-NR_PROVISIONAL.
        ELSE.
          IF V_PI_ANT NE WA_RESULT-NR_PROVISIONAL.
            WA_RESULT-DIFERENCA_CAD = ( WA_RESULT-VALOR_ANTEC_PI - WA_RESULT-VALOR_ANTEC_INV ).
            V_ANTECIPACAO_ANT = WA_RESULT-DIFERENCA_CAD.
            V_PI_ANT = WA_RESULT-NR_PROVISIONAL.
          ELSE.
            WA_RESULT-DIFERENCA_CAD = ( V_ANTECIPACAO_ANT - WA_RESULT-VALOR_ANTEC_INV ).
            V_ANTECIPACAO_ANT = WA_RESULT-DIFERENCA_CAD.
          ENDIF.
          " WA_RESULT-DIFERENCA_CAD = ( V_ANTECIPACAO_ANT - WA_RESULT-VALOR_ANTEC_INV ).
          " V_ANTECIPACAO_ANT = WA_RESULT-DIFERENCA_CAD.
        ENDIF.


        WA_RESULT-INSTRUCAO = WA_ITENS-INSTRUCAO.
        WA_RESULT-NAME1 = WA_ITENS-NAME1.
        WA_RESULT-ID_INVOICE = WA_ITENS-ID_INVOICE.
        CLEAR: WA_RESULT-UTIL_PI, ZVAR_FORM_NO_ZERO.
        LOOP AT IT_LOTE_AVARIADO INTO DATA(WA_LOTE_AV) WHERE ID_INVOICE = WA_ITENS-ID_INVOICE AND CONTRATO_REF EQ WA_ITENS-CONTRATO AND NR_PROVISIONAL_REF EQ WA_ITENS-NR_PROVISIONAL.
          ADD WA_LOTE_AV-VALOR_CAD TO WA_RESULT-UTIL_PI.

          WA_RESULT-CONTRATO_REF =  WA_LOTE_AV-CONTRATO_REF.

          ZVAR_FORM_NO_ZERO = WA_LOTE_AV-NR_PROVISIONAL.
          ZVAR_FORM_NO_ZERO = |{ ZVAR_FORM_NO_ZERO ALPHA = OUT }|.

          IF WA_RESULT-NR_PROVISIONAL_REF IS INITIAL.
*            wa_lote_av-nr_provisional = |{ wa_lote_av-nr_provisional alpha = out }|.
            WA_RESULT-NR_PROVISIONAL_REF =  ZVAR_FORM_NO_ZERO.
          ELSE.
            WA_RESULT-NR_PROVISIONAL_REF = |{ WA_RESULT-NR_PROVISIONAL_REF }/{ ZVAR_FORM_NO_ZERO }|.
          ENDIF.
        ENDLOOP.

        WA_RESULT-VALOR_CAD_FINAL = WA_RESULT-VALOR_CAD_INV - WA_RESULT-UTIL_PI.
        WA_RESULT-DATA_RECEB      = WA_ITENS-DATA_RECEB.
        WA_RESULT-VALOR_RECEB     = WA_ITENS-VALOR_RECEB.

        APPEND WA_RESULT TO IT_RESULT.
        CLEAR: WA_ITENS, WA_LOTE_AV,
        WA_RESULT-TOTAL_INV,
        WA_RESULT-VALOR_ANTEC_INV,
        WA_RESULT-VALOR_CAD_INV,
        WA_RESULT-INSTRUCAO,
        WA_RESULT-NAME1,
        WA_RESULT-ID_INVOICE,
        WA_RESULT-VALOR_CAD_FINAL,
        ZVAR_INVOICE_NOT_EXIT,
        WA_RESULT-VOLUM_INV.
      ENDLOOP.

      IF ZVAR_INVOICE_NOT_EXIT = ABAP_TRUE.
        IF V_ANTECIPACAO_ANT IS INITIAL AND V_PI_ANT IS INITIAL.
          WA_RESULT-DIFERENCA_CAD = ( WA_RESULT-VALOR_ANTEC_PI - WA_RESULT-VALOR_ANTEC_INV ).
          V_ANTECIPACAO_ANT = WA_RESULT-DIFERENCA_CAD.
          V_PI_ANT = WA_RESULT-NR_PROVISIONAL.
        ELSE.
          IF V_PI_ANT NE WA_RESULT-NR_PROVISIONAL.
            WA_RESULT-DIFERENCA_CAD = ( WA_RESULT-VALOR_ANTEC_PI - WA_RESULT-VALOR_ANTEC_INV ).
            V_ANTECIPACAO_ANT = WA_RESULT-DIFERENCA_CAD.
            V_PI_ANT = WA_RESULT-NR_PROVISIONAL.
          ELSE.
            WA_RESULT-DIFERENCA_CAD = ( V_ANTECIPACAO_ANT - WA_RESULT-VALOR_ANTEC_INV ).
            V_ANTECIPACAO_ANT = WA_RESULT-DIFERENCA_CAD.
          ENDIF.
          " WA_RESULT-DIFERENCA_CAD = ( V_ANTECIPACAO_ANT - WA_RESULT-VALOR_ANTEC_INV ).
          " V_ANTECIPACAO_ANT = WA_RESULT-DIFERENCA_CAD.
        ENDIF.
*        IF V_ANTECIPACAO_ANT IS INITIAL AND V_PI_ANT IS INITIAL.
*          WA_RESULT-DIFERENCA_CAD = ( WA_RESULT-VALOR_ANTEC_PI - WA_RESULT-VALOR_ANTEC_INV ).
*          V_ANTECIPACAO_ANT = WA_RESULT-DIFERENCA_CAD.
*          V_PI_ANT = WA_RESULT-NR_PROVISIONAL.
*        ELSE.
*          IF V_PI_ANT NE WA_RESULT-NR_PROVISIONAL.
*            V_PI_ANT = WA_RESULT-NR_PROVISIONAL.
*            WA_RESULT-DIFERENCA_CAD = ( WA_RESULT-VALOR_ANTEC_PI - WA_RESULT-VALOR_ANTEC_INV ).
*          ELSE.
*            WA_RESULT-DIFERENCA_CAD = ( V_ANTECIPACAO_ANT - WA_RESULT-VALOR_ANTEC_INV ).
*            V_ANTECIPACAO_ANT = WA_RESULT-DIFERENCA_CAD.
*          ENDIF.
*
*          WA_RESULT-DIFERENCA_CAD = ( V_ANTECIPACAO_ANT - WA_RESULT-VALOR_ANTEC_INV ).
*          V_ANTECIPACAO_ANT = WA_RESULT-DIFERENCA_CAD.
*        ENDIF.
        APPEND WA_RESULT TO IT_RESULT.
      ENDIF.
      CLEAR: WA_ITENS, WA_LOTE_AV, WA_RESULT, ZVAR_INVOICE_NOT_EXIT.
    ENDLOOP.

    "Ajusta o valor recebido.
    SORT IT_RESULT BY ID_INVOICE.
    DATA(T_RESULT) = IT_RESULT.
*    delete adjacent duplicates from it_result comparing id_invoice contrato.
    LOOP AT T_RESULT ASSIGNING FIELD-SYMBOL(<LS_RESULT>) WHERE ID_INVOICE <> SPACE.
      CLEAR: ZVAR_DIF.

      LOOP AT IT_RESULT ASSIGNING FIELD-SYMBOL(<WS_RESULT>) WHERE ID_INVOICE EQ <LS_RESULT>-ID_INVOICE AND VALOR_RECEB > 0. "BG 191628
        <WS_RESULT>-VALOR_RECEB = ( <WS_RESULT>-VALOR_CAD_INV / <LS_RESULT>-VALOR_RECEB ) * <LS_RESULT>-VALOR_RECEB.
        <WS_RESULT>-VALOR_RECEB = <WS_RESULT>-VALOR_RECEB - <WS_RESULT>-UTIL_PI.
        ADD <WS_RESULT>-VALOR_RECEB TO ZVAR_DIF.
      ENDLOOP.
      IF ZVAR_DIF <> <LS_RESULT>-VALOR_RECEB.
        ZVAR_DIF = <LS_RESULT>-VALOR_RECEB - ZVAR_DIF.
*        zvar_dif = abs( zvar_dif ).
        <WS_RESULT>-VALOR_RECEB = <WS_RESULT>-VALOR_RECEB + ZVAR_DIF.
      ENDIF.


    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
