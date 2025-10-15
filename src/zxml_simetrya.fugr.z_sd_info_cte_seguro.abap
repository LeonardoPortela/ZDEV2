FUNCTION z_sd_info_cte_seguro.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_CTE_AVULSO) TYPE  J_1BDOCNUM
*"     REFERENCE(P_CTE_GERA) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_CTE_VERIFICA_SEGURO) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      IT_CTE_SEGURO STRUCTURE  ZCTE_SEGURO OPTIONAL
*"  EXCEPTIONS
*"      ERRO
*"----------------------------------------------------------------------

  RANGES: rinicio  FOR zlest0115-dt_inicio,
          rfinal   FOR zlest0115-dt_final.

  DATA: wa_zcte_seguro    TYPE zcte_seguro,
        wa_cte_item       TYPE j_1bnflin,
        wa_fatura_servico TYPE vbrp,
        wa_ordem_venda    TYPE vbak,
        it_custo_frete    TYPE TABLE OF vfkp,
        it_condicoes      TYPE TABLE OF konv,
        it_parceiros      TYPE TABLE OF vtpa,
        wa_parceiros      TYPE vtpa,
        wa_info_part      TYPE lfa1,
        wa_condicoes      TYPE konv,
        vg_kinak          TYPE kinak,
        wa_vttp           TYPE vttp,
        wa_lips           TYPE lips,
        wa_ttds           TYPE ttds,
        wa_zlest0052      TYPE zlest0052,
        it_zlest0116      TYPE TABLE OF zlest0116 WITH HEADER LINE,
        it_zlest0115      TYPE TABLE OF zlest0115 WITH HEADER LINE,
        wa_vttk           TYPE vttk,
        wa_lfa1           TYPE lfa1.

  IF p_cte_gera IS INITIAL AND p_cte_verifica_seguro EQ abap_false.

    SELECT * INTO TABLE it_cte_seguro
      FROM zcte_seguro
     WHERE docnum EQ p_cte_avulso.

  ELSE.

    "" Documento Normal
    SELECT SINGLE * INTO wa_cte_item
      FROM j_1bnflin
     WHERE docnum EQ p_cte_avulso.

    CHECK sy-subrc IS INITIAL.

    "Fatura do Serviço
    SELECT SINGLE * INTO wa_fatura_servico
      FROM vbrp
     WHERE vbeln = wa_cte_item-refkey(10)
       AND posnr = wa_cte_item-refitm.

    CHECK sy-subrc IS INITIAL.

    "Ordem de Venda
    SELECT SINGLE * INTO wa_ordem_venda
      FROM vbak
     WHERE vbeln = wa_fatura_servico-aubel.

    CHECK sy-subrc IS INITIAL.


    SELECT SINGLE * INTO wa_vttp
      FROM vttp
    WHERE tknum EQ wa_ordem_venda-tknum.


    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE * INTO wa_lips
      FROM lips
    WHERE vbeln EQ wa_vttp-vbeln.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE * INTO wa_ttds
      FROM ttds
    WHERE tplst EQ wa_lips-werks.

*-BUG 149457-11.10.2024-JT-#149457-inicio
    IF sy-subrc <> 0.
      MESSAGE e033(zavseguro) WITH wa_lips-werks RAISING erro.
    ENDIF.
*-BUG 149457-11.10.2024-JT-#149457-fim

    "SELECT SINGLE * INTO WA_TTDS
    " FROM TTDS
    "WHERE TPLST EQ WA_FATURA_SERVICO-WERKS.

    CLEAR: wa_lfa1.

    SELECT SINGLE * INTO wa_zlest0052
      FROM zlest0052
    WHERE bukrs    EQ wa_ttds-bukrs
      AND matkl    EQ wa_lips-matkl
      AND status   EQ 'X'
      AND bloqueio EQ 'A'.

    IF sy-subrc IS NOT INITIAL.
      SELECT * INTO TABLE it_zlest0116
        FROM zlest0116
       WHERE cd_empresa EQ wa_ttds-bukrs
         AND cd_grupo   EQ wa_lips-matkl.

      IF sy-subrc IS INITIAL.
        CLEAR: rinicio.
        rinicio-sign   = 'I'.
        rinicio-option = 'LE'.
        rinicio-low    = sy-datum.
        rinicio-high   = sy-datum.
        APPEND rinicio.

        CLEAR: rfinal.
        rfinal-sign   = 'I'.
        rfinal-option = 'GE'.
        rfinal-low    = sy-datum.
        rfinal-high   = sy-datum.
        APPEND rfinal.

        SELECT * INTO TABLE it_zlest0115
          FROM zlest0115
           FOR ALL ENTRIES IN it_zlest0116
         WHERE cd_apolice    EQ it_zlest0116-cd_apolice
           AND dt_inicio     IN rinicio
           AND dt_final      IN rfinal
           AND ck_excluido   EQ space.

        IF sy-subrc IS INITIAL.
          READ TABLE it_zlest0115 INDEX 1.
          wa_lfa1-lifnr = it_zlest0115-cd_fornecedor.
          IF it_zlest0115-nr_apolice IS NOT INITIAL.
            wa_zcte_seguro-napol = it_zlest0115-nr_apolice.
          ELSE.
            wa_zcte_seguro-napol = it_zlest0115-nr_proposta.
          ENDIF.
        ELSEIF p_cte_verifica_seguro EQ abap_true.
          MESSAGE e028(zavseguro) WITH sy-datum RAISING erro.
        ENDIF.
      ELSEIF p_cte_verifica_seguro EQ abap_true.
        MESSAGE e027(zavseguro) WITH wa_lips-matkl wa_ttds-bukrs RAISING erro.
      ENDIF.
    ELSE.
      wa_lfa1-lifnr        = wa_zlest0052-lifnr.
      wa_zcte_seguro-napol = wa_zlest0052-nr_apolice.
    ENDIF.

    CHECK p_cte_verifica_seguro EQ abap_false.

    IF wa_lfa1-lifnr IS NOT INITIAL.
      SELECT SINGLE * INTO wa_lfa1
        FROM lfa1
       WHERE lifnr EQ wa_lfa1-lifnr.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZCTESEG'
      IMPORTING
        number                  = wa_zcte_seguro-cd_seguro
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    wa_zcte_seguro-resp_codigo = wa_lfa1-lifnr.
    wa_zcte_seguro-xseg        = wa_lfa1-name1.
    wa_zcte_seguro-docnum      = p_cte_avulso.

    SELECT SINGLE * INTO wa_vttk
      FROM vttk
    WHERE tknum EQ wa_ordem_venda-tknum
      AND vbtyp EQ '8'.

    CASE wa_vttk-shtyp.
      WHEN: 'Z020' OR 'Z021' OR 'Z009'.
        wa_zcte_seguro-respseg = '3'.
      WHEN OTHERS.
        wa_zcte_seguro-respseg = '0'.
    ENDCASE.

    APPEND wa_zcte_seguro TO it_cte_seguro.
*
*
*
*
*    SELECT * FROM vfkp
*      INTO TABLE it_custo_frete
*     WHERE refty EQ c_8
*       AND rebel EQ wa_ordem_venda-tknum.
*
*    CHECK sy-subrc IS INITIAL.
*
*    SELECT *
*      FROM konv
*      INTO CORRESPONDING FIELDS OF TABLE it_condicoes
*      FOR ALL ENTRIES IN it_custo_frete
*     WHERE knumv EQ it_custo_frete-knumv
*       AND kschl EQ c_zseg
*       AND kinak EQ vg_kinak
*       AND kwert GT 0.
*
*    SELECT *
*      FROM vtpa
*      INTO CORRESPONDING FIELDS OF TABLE it_parceiros
*     WHERE vbeln EQ wa_ordem_venda-tknum.
*
*    LOOP AT it_condicoes INTO wa_condicoes.
*
*      CALL FUNCTION 'NUMBER_GET_NEXT'
*        EXPORTING
*          nr_range_nr             = '01'
*          object                  = 'ZCTESEG'
*        IMPORTING
*          number                  = wa_zcte_seguro-cd_seguro
*        EXCEPTIONS
*          interval_not_found      = 1
*          number_range_not_intern = 2
*          object_not_found        = 3
*          quantity_is_0           = 4
*          quantity_is_not_1       = 5
*          interval_overflow       = 6
*          buffer_overflow         = 7
*          OTHERS                  = 8.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*      wa_zcte_seguro-docnum  = p_cte_avulso.
*      wa_zcte_seguro-respseg = c_4.
*
*      READ TABLE it_parceiros INTO wa_parceiros WITH KEY parvw = c_sg.
*      IF ( sy-subrc EQ 0 ) AND ( wa_parceiros-lifnr IS NOT INITIAL ).
*        CALL FUNCTION 'Z_PARCEIRO_INFO'
*          EXPORTING
*            p_parceiro   = wa_parceiros-lifnr
*            p_partype    = 'V'
*          CHANGING
*            wa_info_part = wa_info_part.
*        wa_zcte_seguro-resp_codigo = wa_info_part-lifnr.
*        wa_zcte_seguro-xseg        = wa_info_part-name1.
*      ENDIF.
*
*      APPEND wa_zcte_seguro TO it_cte_seguro.
*
*    ENDLOOP.

  ENDIF.


ENDFUNCTION.
