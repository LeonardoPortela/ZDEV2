FUNCTION ZLES_VALOR_FRETE_ORDEM_CAR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(I_ID_ORDEM) TYPE  ZDE_ID_ORDEM OPTIONAL
*"     REFERENCE(I_PLACA_CAV) TYPE  ZPLACA
*"     REFERENCE(I_SHTYP) TYPE  SHTYP OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_VLR_FRETE_NEG) TYPE  ZVALOR_FRETE
*"     REFERENCE(E_ID_ORDEM) TYPE  ZDE_ID_ORDEM
*"     REFERENCE(E_ZLEST0155) TYPE  ZLEST0155
*"----------------------------------------------------------------------

  DATA: WA_ZLEST0155 TYPE ZLEST0155.

  CLEAR: E_VLR_FRETE_NEG, E_ID_ORDEM, E_ZLEST0155.

  "Procurar Preço diferenciado
  IF I_ID_ORDEM IS NOT INITIAL AND I_PLACA_CAV IS NOT INITIAL.

    SELECT SINGLE *
      FROM ZLEST0155 AS A INTO WA_ZLEST0155
     WHERE A~ID_ORDEM         = I_ID_ORDEM
       AND A~DS_PLACA_TRATOR  = I_PLACA_CAV
       AND A~VLR_FRETE_NEG    > 0.
*       AND EXISTS ( SELECT *
*                      FROM ZSDT0001OD AS B
*                     WHERE B~ID_ORDEM    = A~ID_ORDEM
*                       AND B~DT_EMISSAO  <= SY-DATLO
*                       AND B~DT_VALIDADE >= SY-DATLO ).

  ELSEIF I_VBELN IS NOT INITIAL AND I_PLACA_CAV IS NOT INITIAL.

    SELECT SINGLE *
      FROM ZLEST0155 AS A INTO WA_ZLEST0155
     WHERE A~VBELN            = I_VBELN
       AND A~DS_PLACA_TRATOR  = I_PLACA_CAV
       AND A~VLR_FRETE_NEG    > 0
       AND EXISTS ( SELECT *
                      FROM ZSDT0001OD AS B
                     WHERE B~ID_ORDEM    = A~ID_ORDEM
                       AND B~DT_EMISSAO  <= SY-DATLO
                       AND B~DT_VALIDADE >= SY-DATLO ).

  ELSE.
    SY-SUBRC = 1.
  ENDIF.

  "Achou Liberação de Preço diferênciado
  IF SY-SUBRC IS INITIAL AND I_SHTYP NE 'Z021'.

    SELECT SINGLE * INTO @DATA(WA_ZSDT0001)
      FROM ZSDT0001
     WHERE TP_MOVIMENTO EQ 'S'
       AND ID_ORDEM     EQ @WA_ZLEST0155-ID_ORDEM.

    IF SY-SUBRC IS INITIAL.

      "Só prosseguir se não tiver gerado custo
      CHECK WA_ZSDT0001-FKNUM IS INITIAL.

    ELSEIF WA_ZLEST0155-CH_REFERENCIA IS NOT INITIAL. "Caso tenha faturamento feito

      SELECT SINGLE *
        FROM ZSDT0001 INTO @WA_ZSDT0001
       WHERE CH_REFERENCIA EQ @WA_ZLEST0155-CH_REFERENCIA.

      "Só prosseguir se não tiver gerado custo
      CHECK ( SY-SUBRC IS INITIAL ) AND ( WA_ZSDT0001-FKNUM IS INITIAL ).

    ENDIF.

    E_VLR_FRETE_NEG = WA_ZLEST0155-VLR_FRETE_NEG. "Atribuir Valor de Frete Negociado
    E_ID_ORDEM      = WA_ZLEST0155-ID_ORDEM.
    E_ZLEST0155     = WA_ZLEST0155.

  ELSEIF I_ID_ORDEM IS NOT INITIAL.

    IF I_SHTYP IS INITIAL.
      "Procurar VT para verificar tipo do frete Entrada/Saída
      SELECT SINGLE * INTO @DATA(WA_VTTK)
        FROM VTTK
       WHERE ID_ORDEM EQ @I_ID_ORDEM.

      CHECK SY-SUBRC IS INITIAL.
    ELSE.
      WA_VTTK-SHTYP = I_SHTYP.
    ENDIF.

    "Não achou preço diferênciado, então caso tenha uma viagem de frete via Carguero, deve ser respeitado o valor aprovado na viagem
    SELECT SINGLE * INTO @DATA(WA_ZLEST0185)
      FROM ZLEST0185
     WHERE ID_ORDEM      EQ @I_ID_ORDEM
       AND CK_AUTORIZADA EQ @ABAP_TRUE.

    CHECK 1 EQ 2.

    CHECK SY-SUBRC IS INITIAL.

    CASE WA_VTTK-SHTYP.
      WHEN 'Z021'. "Frete Entrada

        " Valor do Frete de Entrada
        E_VLR_FRETE_NEG = WA_ZLEST0185-NM_VR_FRETE_MOTORISTA_ENTRADA. "Atribuir Valor de Frete Negociado para valor de frete de entrada
        E_ID_ORDEM      = WA_ZLEST0185-ID_ORDEM.

      WHEN OTHERS.

        SELECT SINGLE * INTO @DATA(WA_ZLEST0181)
          FROM ZLEST0181
         WHERE ID_LOTE_FRETE EQ @WA_ZLEST0185-ID_LOTE_FRETE.

        CASE WA_ZLEST0181-KVGR5. "Emite Frete de Entrada?
          WHEN '002'.
            "Valor do Frete 2ª Perna
            E_VLR_FRETE_NEG = WA_ZLEST0185-NM_VR_FRETE_MOTORISTA.
          WHEN OTHERS.
            "Valor do Frete, quando não tem intermunicipal
            E_VLR_FRETE_NEG = WA_ZLEST0185-NM_VR_FRETE_EMPRESA. "Atribuir Valor de Frete Empresa para valor de frete de saída
        ENDCASE.
        E_ID_ORDEM = WA_ZLEST0185-ID_ORDEM.

    ENDCASE.

  ENDIF.

ENDFUNCTION.
