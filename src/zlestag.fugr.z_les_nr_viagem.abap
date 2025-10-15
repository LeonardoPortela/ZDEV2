FUNCTION Z_LES_NR_VIAGEM.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_BUKRS) TYPE  BUKRS
*"     REFERENCE(P_WERKS) TYPE  WERKS_D
*"     REFERENCE(P_ANO_VIAGEM) TYPE  AJAHR
*"  EXPORTING
*"     REFERENCE(P_NR_VIAGEM) TYPE  NUM4
*"----------------------------------------------------------------------

  " Tabela do Frete Aquaviário - Sequencia de Viagens
  TABLES: ZLEST0058, ZLEST0068.


  DATA: GT_ZLEST0058 TYPE TABLE OF ZLEST0058 WITH HEADER LINE, "Tabela Interna da ZLEST0058 - Frete Aquaviário - Sequencia de Viagens
        GW_ZLEST0058 TYPE ZLEST0058. "Work Area da ZLEST0058 - Frete Aquaviário - Sequencia de Viagens

  DATA: INSR_ZLEST0058 TYPE ZLEST0058, "Work Area configurada para receber os valores de inserção na tabela ZLEST0058
        INSR_ZLEST0068 TYPE ZLEST0068. "Work Area configurada para receber os valores de inserção na tabela ZLEST0068

  DATA: NR_VIAGEM TYPE NUM10. "Variável para calcular o novo número da viagem

  REFRESH: GT_ZLEST0058[]. "Limpar tabela interna referente a seleção da ZLEST0058
  CLEAR: GW_ZLEST0058." Limpar Work area referente ao read table da seleção da ZLEST0058

  "Seleciona na tabela de Sequencia de Viagem se já existe alguma para empresa/centro/ano
  "passados no parâmetro.
  SELECT *
    FROM ZLEST0058
    INTO TABLE GT_ZLEST0058
  WHERE BUKRS      EQ P_BUKRS
    AND WERKS      EQ P_WERKS
    AND ANO_VIAGEM EQ P_ANO_VIAGEM.

  "Se encontrar algum registro na seleção acima vai pegar o valor encontrado
  "e somar mais 1, atualizar a tabela sequencial ZLEST0058 referente a empresa/centro/ano da viagem.
  "e retornar no parâmetro P_NR_VIAGEM o número da viagem atual.
  IF NOT ( GT_ZLEST0058[] IS INITIAL ).

    CLEAR: NR_VIAGEM, GW_ZLEST0058, INSR_ZLEST0068 . "Limpar variavel NR_VIAGEM e WORK AREA.

    READ TABLE GT_ZLEST0058 INTO GW_ZLEST0058 INDEX 1. "Ler o registro encontrado na seleção da ZLEST0058

    "Calculo simples para somar o número da viagem encontrada + 1
    NR_VIAGEM = GW_ZLEST0058-NR_VIAGEM + 1.

    "Atualizar na tabela ZLEST0058 o número da viagem calculado acima referente a Empresa/Centro/Ano da Viagem.
    UPDATE ZLEST0058 SET NR_VIAGEM = NR_VIAGEM
     WHERE BUKRS      EQ P_BUKRS
       AND WERKS      EQ P_WERKS
       AND ANO_VIAGEM EQ P_ANO_VIAGEM.

    P_NR_VIAGEM = NR_VIAGEM. "Retornar o novo número da viagem

    INSR_ZLEST0068-NR_VIAGEM  = P_NR_VIAGEM.
    INSR_ZLEST0068-BUKRS      = P_BUKRS.
    INSR_ZLEST0068-WERKS      = P_WERKS.
    INSR_ZLEST0068-ANO_VIAGEM = P_ANO_VIAGEM.

    INSERT INTO ZLEST0068 VALUES INSR_ZLEST0068.

    IF ( SY-SUBRC EQ 0 ).
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.


  ELSE.
    "Caso não encontre o registro na seleção acima o número da viagem sempre vai ser inicial 1
    "Vai preencher todos os campos Empresa/Centro/Ano da Viagem e NR_VIAGEM
    "Para Inserir na tabela ZLEST0058
    "Retornar para o parâmetro P_NR_VIAGEM


    INSR_ZLEST0058-BUKRS       = P_BUKRS. "Empresa
    INSR_ZLEST0058-WERKS       = P_WERKS. "Centro
    INSR_ZLEST0058-ANO_VIAGEM  = P_ANO_VIAGEM. "Ano Viagem
    INSR_ZLEST0058-NR_VIAGEM   = 1. "Número da Viagem Inicial 1.

    INSERT ZLEST0058 FROM INSR_ZLEST0058. "Inserir os Valores configurados

    P_NR_VIAGEM = INSR_ZLEST0058-NR_VIAGEM. "Retorna o número da Viagem inicial 1

    INSR_ZLEST0068-NR_VIAGEM  = P_NR_VIAGEM.
    INSR_ZLEST0068-BUKRS      = P_BUKRS.
    INSR_ZLEST0068-WERKS      = P_WERKS.
    INSR_ZLEST0068-ANO_VIAGEM = P_ANO_VIAGEM.

    INSERT INTO ZLEST0068 VALUES INSR_ZLEST0068.

    IF ( SY-SUBRC EQ 0 ).
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDIF.

ENDFUNCTION.
