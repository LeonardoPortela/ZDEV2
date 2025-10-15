FUNCTION zsd_portal_produtor.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(AT_TCON) TYPE  ZRSDSSELOPTS
*"     REFERENCE(AT_CONT) TYPE  ZRSDSSELOPTS
*"     REFERENCE(AT_ORGV) TYPE  ZRSDSSELOPTS
*"     REFERENCE(AT_CDIS) TYPE  ZRSDSSELOPTS
*"     REFERENCE(AT_SATI) TYPE  ZRSDSSELOPTS
*"     REFERENCE(AT_FATU) TYPE  ZRSDSSELOPTS
*"     REFERENCE(AT_CLIE) TYPE  ZRSDSSELOPTS
*"     REFERENCE(AT_DATE) TYPE  ZRSDSSELOPTS
*"     REFERENCE(TIPO) TYPE  CHAR1
*"     VALUE(AT_INSCESTADUAL) TYPE  ZRSDSSELOPTS OPTIONAL
*"  TABLES
*"      AT_HEADER TYPE  ZSDE006_T OPTIONAL
*"      AT_ITEM TYPE  ZSDE002_T OPTIONAL
*"----------------------------------------------------------------------

  DATA: lv_call_prog   TYPE sy-repid VALUE 'ZSDR0223',
        it_zsdt0257    TYPE TABLE OF zsdt0257,
        it_solicitacao TYPE TABLE OF zsdt0257,
        lt_values      TYPE TABLE OF rgsb4,

        it_header      TYPE STANDARD TABLE OF zsde006,
        wa_header      TYPE zsde006,


        it_item        TYPE STANDARD TABLE OF zsde002,
        wa_item        TYPE zsde002,
        vlr_total      TYPE zde_vlr_qtdfat.

  DATA: r_auart TYPE RANGE OF auart.

  DATA:  r_kunnr TYPE RANGE OF kunnr.

*  "// Inicio BUG 160640 10-12-2024 - WBARBOSA
  EXPORT lv_call_prog TO DATABASE indx(zp) ID 'GETAPPPRODUTOR'.
*  EXPORT lv_call_prog TO DATABASE indx(zp) ID 'ZSDT0257'.
*  "// Fim BUG 160640 10-12-2024 - WBARBOSA

  SUBMIT zsdr0018
      WITH mm EQ abap_false
      WITH sd EQ abap_true
      WITH p_prod   EQ abap_true
      WITH p_tpcont IN at_tcon
      WITH p_cont   IN at_cont
      WITH p_orgven IN at_orgv
      WITH p_cdist  IN at_cdis
      WITH p_sativ  IN at_sati
      WITH p_clien  IN at_clie
      WITH p_datent IN at_date
      WITH p_fatuv  IN at_fatu
      EXPORTING LIST TO MEMORY AND RETURN.

*  "// Inicio BUG 160640 10-12-2024 - WBARBOSA
  IMPORT it_zsdt0257  FROM DATABASE indx(zk) ID 'GETAPPPRODUTOR'.
  DELETE FROM DATABASE indx(zk) ID 'GETAPPPRODUTOR'.
*  IMPORT it_zsdt0257  FROM DATABASE indx(zk) ID 'ZSDT0257'.
*  DELETE FROM DATABASE indx(zk) ID 'ZSDT0257'.
*  "// Fim BUG 160640 10-12-2024 - WBARBOSA

  it_solicitacao = it_zsdt0257.

  SORT it_solicitacao BY doc_simulacao vbeln.
  DELETE ADJACENT DUPLICATES FROM it_solicitacao COMPARING doc_simulacao.

  IF it_zsdt0257 IS NOT INITIAL.

    SELECT *
      FROM tspat
      INTO TABLE @DATA(it_tspat)
      FOR ALL ENTRIES IN @it_zsdt0257
      WHERE spart EQ @it_zsdt0257-spart
      AND spras EQ @sy-langu.

    SELECT *
      FROM zsdt0040
      INTO TABLE @DATA(it_zsdt0040)
      FOR ALL ENTRIES IN @it_zsdt0257
      WHERE doc_simulacao EQ @it_zsdt0257-doc_simulacao.

    SELECT *
      FROM kna1
      INTO TABLE @DATA(it_kna1)
      FOR ALL ENTRIES IN @it_zsdt0257
      WHERE kunnr EQ @it_zsdt0257-kunnr
        AND stcd3 IN @at_inscestadual.

  ENDIF.

  "Filtrar pela escrição estadual.
  IF at_inscestadual IS NOT INITIAL AND it_kna1 IS NOT INITIAL.
    r_kunnr = VALUE #( FOR i IN it_kna1 ( sign = 'I' option = 'EQ' low = i-kunnr ) ).
    DELETE it_solicitacao WHERE kunnr NOT IN r_kunnr.

    "FF - 31.01.2024, ajuste filtro - inicio
  ELSEIF at_inscestadual IS NOT INITIAL AND sy-subrc <> 0.
    CLEAR it_solicitacao[].

    "FF - 31.01.2024, ajuste filtro - fim
  ENDIF.

*** US #77981 - AOENNING
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr         = 'ZHEDGEDEVO/RECU'
      class         = '0000'
    TABLES
      set_values    = lt_values
    EXCEPTIONS
      set_not_found = 1.

  IF sy-subrc IS INITIAL.
    SORT lt_values BY from.
    FREE: r_auart.
    r_auart = VALUE #( FOR t IN lt_values ( sign = 'I' option = 'EQ' low = t-from ) ).
    IF r_auart IS NOT INITIAL AND it_zsdt0257 IS NOT INITIAL.
      SORT it_zsdt0257 BY auart.
      DELETE it_zsdt0257 WHERE auart IN r_auart.
    ENDIF.
  ENDIF.
*** US #77981 - AOENNING



  CASE tipo.
    WHEN '1'.

      LOOP AT it_solicitacao INTO DATA(wa_solicitacao).
        READ TABLE it_tspat INTO DATA(wa_tspat) WITH KEY spart = wa_solicitacao-spart.
        READ TABLE it_zsdt0040 INTO DATA(wa_zsdt0040) WITH KEY doc_simulacao = wa_solicitacao-doc_simulacao.
        READ TABLE it_kna1 INTO DATA(wa_kna1) WITH KEY kunnr = wa_solicitacao-kunnr.

        wa_header =
        VALUE #(
                  contrato    = wa_solicitacao-doc_simulacao
                  data_pedido = wa_solicitacao-data_atual
                  nome        = |{ wa_solicitacao-name1 CASE = UPPER }|
                  safra       = wa_solicitacao-safra
*                  insumos     = |{ wa_tspat-vtext CASE = UPPER }|
                  cultura     = wa_solicitacao-cultura
                  moeda       = wa_solicitacao-waers
                  vencimento  = wa_solicitacao-valdt
                  modalidade  = |{ wa_solicitacao-vtext+5 CASE = UPPER }|
                  antecipado      = wa_zsdt0040-prec_ant_cult
                  trocatotalsac   = wa_zsdt0040-trototsc
                  scha            = wa_zsdt0040-scha
                  cpf_cnpj        = COND #( WHEN wa_kna1-stcd1 IS INITIAL THEN wa_kna1-stcd2 ELSE wa_kna1-stcd1 )
                  clientesap      = wa_zsdt0040-kunnr
                  cod_modalidade  = wa_zsdt0040-tpsim
                  antec           = wa_zsdt0040-antec
                  vlr_adto        = wa_zsdt0040-vlr_adto
                  juros_ano       = wa_zsdt0040-juros_ano
                  inscestadual    = wa_kna1-stcd3
               ).

        vlr_total = 0.
        LOOP AT it_zsdt0257 INTO DATA(wa_zsdt0257) WHERE doc_simulacao EQ wa_solicitacao-doc_simulacao.
          ADD wa_zsdt0257-vlr_totbrt TO vlr_total.
        ENDLOOP.

        wa_header-valor_total = vlr_total.

        APPEND wa_header TO at_header.
        APPEND wa_item TO at_item.
        CLEAR: wa_header, wa_solicitacao, wa_tspat, wa_zsdt0257, vlr_total.

      ENDLOOP.

    WHEN '2'.

      LOOP AT it_zsdt0257 INTO wa_zsdt0257.

        wa_item =
        VALUE #(
                  filial         = wa_zsdt0257-bezei
                  contrato       = wa_zsdt0257-doc_simulacao
                  ordem          = wa_zsdt0257-vbeln
                  item           = wa_zsdt0257-posnr
                  descricao      = |{ wa_zsdt0257-arktx CASE = UPPER }|
                  marca          = wa_zsdt0257-wrkst
                  volume         = wa_zsdt0257-qtd
                  unidade        = |{ wa_zsdt0257-unid CASE = UPPER }|
                  faturado       = wa_zsdt0257-qtdefaturado
                  saldo          = wa_zsdt0257-saldo
                  moeda          = wa_zsdt0257-waers
                  valor_unitario = wa_zsdt0257-kbetr2
                  valor_total    = CONV #( wa_zsdt0257-vlr_totbrt )
                  vencimento     = wa_zsdt0257-valdt
                  modalidade     = |{ wa_zsdt0257-vtext+5 CASE = UPPER }|
                  safra          = wa_zsdt0257-safra
                  cod_filial     = wa_zsdt0257-werks
               ).

        APPEND wa_item TO at_item.
        CLEAR: wa_item, wa_zsdt0257.

      ENDLOOP.

    WHEN '3'.
    WHEN OTHERS.
  ENDCASE.
ENDFUNCTION.
