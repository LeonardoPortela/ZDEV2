DATA: _lifnr             TYPE lifnr,
      _kg                TYPE numc10,
      _total_kg_contrato TYPE numc10,
      quantidade_extenso TYPE string,
      word               LIKE spell,
      conversao          TYPE p DECIMALS 2.

CLEAR: wa_total-bales, wa_total-ton, wa_total-quantidade.

_lifnr = |{ header-werks ALPHA = IN }|.

SELECT SINGLE *
  FROM zsdt0143
  INTO @DATA(wa_143x)
  WHERE contrato EQ @header-contrato
  AND safra EQ @header-safra
*** Stefanini - IR195586 - 12/12/2024 - LAZAROSR - Início de Alteração
  AND empresa   EQ @header-empresa
  AND cliente   EQ @header-kunnr
  AND cancelado EQ @abap_false.
*** Stefanini - IR195586 - 12/12/2024 - LAZAROSR - Fim de Alteração


SELECT quatidade
FROM zsdt0143
INTO TABLE ti_143
WHERE contrato EQ header-contrato
  AND safra EQ header-safra
*** Stefanini - IR195586 - 12/12/2024 - LAZAROSR - Início de Alteração
  AND empresa   EQ header-empresa
  AND cliente   EQ header-kunnr
  AND cancelado EQ abap_false.
*** Stefanini - IR195586 - 12/12/2024 - LAZAROSR - Fim de Alteração

LOOP AT itens INTO DATA(wa).
  ADD wa-qtd_fardos  TO wa_total-bales.
  ADD wa-peso_lote  TO _kg.
ENDLOOP.
wa_total-ton = _kg / 1000.

LOOP AT ti_143 INTO DATA(wa_).
  ADD wa_-quantidade  TO _total_kg_contrato.
ENDLOOP.

wa_total-quantidade = _total_kg_contrato / 1000.

SHIFT wa_total-bales LEFT DELETING LEADING '0'.
SHIFT wa_total-ton LEFT DELETING LEADING '0'.
"SHIFT HEADER-NR_WAREHOUSE LEFT DELETING LEADING '0'.

SELECT SINGLE *
  FROM kna1
  INTO wa_kna1
  WHERE kunnr EQ header-kunnr.

SELECT SINGLE *
  FROM adrc
  INTO wa_adrc
  WHERE addrnumber EQ wa_kna1-adrnr
    AND date_to    >= sy-datum.

w_nome_client = wa_adrc-name1 && wa_adrc-name2.

*** Stefanini - IR195586 - 12/12/2024 - LAZAROSR - Início de Alteração
DATA:
  lo_sd_header_to TYPE REF TO zcl_sd_header_to.

CREATE OBJECT lo_sd_header_to
  EXPORTING
    i_w_adrc = wa_adrc
    i_v_separador_dom_cidade_pais = zcl_sd_header_to=>c_virgula
    i_v_pais_extenso = abap_true.

w_header_to = lo_sd_header_to->get_w_header_to( ).
*** Stefanini - IR195586 - 12/12/2024 - LAZAROSR - Fim de Alteração

SELECT SINGLE *
  FROM lfa1
  INTO wa_lfa1
  WHERE lifnr = _lifnr.

CONCATENATE wa_lfa1-stcd1(2) '.' wa_lfa1-stcd1+2(3)
'.' wa_lfa1-stcd1+5(3) '/' wa_lfa1-stcd1+8(4) '.' wa_lfa1-stcd1+12(2) INTO cnpj_formatado.
CONCATENATE wa_lfa1-stcd3(2) '.' wa_lfa1-stcd3+2(3) '.' wa_lfa1-stcd3+5(3) '-' wa_lfa1-stcd3+8(1) INTO ie.

CASE _lifnr.
  WHEN '0000001507'.
    endereco_pt = 'Tucunaré, Rod. MT 235 KM 133, no município de Sapezal-MT'.
    endereco_en = 'Tucunaré at Rod. MT 235 KM 133, at city in Sapezal-MT'.
  WHEN '0000001520'.
    endereco_pt = 'Água Quente Estrada SZ-01 KM 34 – margem esquerda, no município de Sapezal-MT'.
    endereco_en = 'Água Quente Road SZ-01 KM 34 – right side , at city in Sapezal-MT'.
  WHEN '0000001521'.
    endereco_pt = 'Itamarati Rod. BR 364 entroncamento com MT 358, Munícipio Campo Novo do Parecis-MT'.
    endereco_en = 'Itamarati at Rod. BR 364 junction with MT 358, City at in Campo Novo do Parecis-MT'.
  WHEN OTHERS.
    endereco_pt = ''.
    endereco_en = ''.
ENDCASE.


*------------- DATA INGLES-----------

SELECT SINGLE ltx
  FROM t247
  INTO mes_en
  WHERE mnr EQ wa_143x-dt_venda+4(2) AND spras = 'EN'.

dia_en = wa_143x-dt_venda+6(2).

ano_venda = wa_143x-dt_venda(4).

CASE dia_en.
  WHEN '11' OR '12' OR '13'.
    dia_en = |{ dia_en }th|.
  WHEN OTHERS.
    DATA(vlr) = dia_en MOD 10.
    CASE vlr.
      WHEN 1. dia_en = |{ dia_en }st|.
      WHEN 2. dia_en = |{ dia_en }nd|.
      WHEN 3. dia_en = |{ dia_en }rd|.
      WHEN OTHERS. dia_en = |{ dia_en }th|.
    ENDCASE.
ENDCASE.

*------------- DATA PORTUGUES-------

SELECT SINGLE ltx
  FROM t247
  INTO mes_venda_pt
  WHERE mnr EQ wa_143x-dt_venda+4(2) AND spras = 'PT'.

dia_venda_pt = wa_143x-dt_venda+6(2).


*------------- DATA ATUAL-----------

SELECT SINGLE ltx
  FROM t247
  INTO mes
  WHERE mnr EQ sy-datum+4(2) AND spras = 'PT'.

dia = sy-datum+6(2).
ano = sy-datum(4).




*--------------------------- QUANTIDADE POR EXTENSO
conversao = wa_total-quantidade.
CALL FUNCTION 'SPELL_AMOUNT'
  EXPORTING
    amount    = conversao
    currency  = 'INR'
    filler    = ' '
    language  = 'P'
  IMPORTING
    in_words  = word
  EXCEPTIONS
    not_found = 1
    too_large = 2
    OTHERS    = 3.
IF word-decword NE 'ZERO'.
  CONCATENATE word-word 'E' word-decword INTO wa_total-qtd_extenso SEPARATED BY space.
ELSE.
  wa_total-qtd_extenso = word-word.
ENDIF.

CALL FUNCTION 'SPELL_AMOUNT'
  EXPORTING
    amount    = conversao
    currency  = 'INR'
    filler    = ' '
    language  = 'E'
  IMPORTING
    in_words  = word
  EXCEPTIONS
    not_found = 1
    too_large = 2
    OTHERS    = 3.
IF word-decword NE 'ZERO'.
  CONCATENATE word-word 'AND' word-decword INTO wa_total-qtd_extenso_en SEPARATED BY space.
ELSE.
  wa_total-qtd_extenso_en = word-word.
ENDIF.

"---------------- ARREDONDAR PORCENTAGEM -------------

CALL FUNCTION 'ROUND'
  EXPORTING
    decimals = '0'
    input    = wa_143-pctgem_ant
  IMPORTING
    output   = porcentagem.

CALL FUNCTION 'ROUND'
  EXPORTING
    decimals = '0'
    input    = wa_total-quantidade
  IMPORTING
    output   = quantidade_s_decimal.
