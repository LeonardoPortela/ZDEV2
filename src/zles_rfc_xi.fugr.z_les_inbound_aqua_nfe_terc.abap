FUNCTION Z_LES_INBOUND_AQUA_NFE_TERC.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_ZLEST0065 STRUCTURE  ZLEST0065
*"----------------------------------------------------------------------

  "Descrição: RFC Responsável por receber informações da tabela do XI referente a NF de Terceiros do modulo LES-Aquaviário.
  "Tabela no Oracle: XML_TERCEIROS
  TABLES: ZLEST0065.
  DATA: WL_ZLEST0065 TYPE ZLEST0065,
        TL_INPUT_ZLEST0065 TYPE TABLE OF ZLEST0065 WITH HEADER LINE.

  "Verifica qual é o Status da NFE
  " 1 - Autorizado
  " 2 - Cancelado

  LOOP AT IT_ZLEST0065.

    CASE IT_ZLEST0065-STATUS_NF.

      WHEN: '1'.
        IF NOT ( IT_ZLEST0065-CH_REFERENCIA IS INITIAL ).
          REFRESH: TL_INPUT_ZLEST0065.
          MOVE-CORRESPONDING IT_ZLEST0065 TO TL_INPUT_ZLEST0065.
          INSERT INTO ZLEST0065 VALUES TL_INPUT_ZLEST0065.
        ENDIF.
      WHEN: '2'.
        "Atualiza o Status caso tenha nota cancelada.
        SELECT SINGLE * FROM ZLEST0065 WHERE CH_REFERENCIA EQ IT_ZLEST0065-CH_REFERENCIA
                                         AND NFENUM        EQ IT_ZLEST0065-NFENUM.

        IF ( SY-SUBRC EQ 0 ).
          UPDATE ZLEST0065 SET STATUS_NF = IT_ZLEST0065-STATUS_NF WHERE CH_REFERENCIA EQ IT_ZLEST0065-CH_REFERENCIA
                                                                    AND NFENUM        EQ IT_ZLEST0065-NFENUM.
        ENDIF.
    ENDCASE.

  ENDLOOP.
ENDFUNCTION.
