FUNCTION ZLESF0141_RES_OUTROS_VALORES.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_DATA_POSICAO) TYPE  ZDT_POSICAO OPTIONAL
*"  EXPORTING
*"     REFERENCE(MSG_RETORNO) TYPE  CHAR10
*"----------------------------------------------------------------------
  TYPES: TY_RG_CHVID    TYPE RANGE OF ZPFE_LOTE_ITEM-CHVID.

  DATA: V_SOMA_OUTROS     TYPE ZDE_SALDO_OUTROS,
        IT_0141_LOTE_SAVE TYPE TABLE OF ZLEST0141_LOTE,
        IT_0141_ITEM_SAVE TYPE TABLE OF ZLEST0141_L_ITEM, "lote_item
        IT_SET_CHVID      TYPE TABLE OF RGSB4,
        WA_ZPFE_LOTE      TYPE ZPFE_LOTE,
        V_REG_GRAVADO     TYPE ABAP_BOOL VALUE ABAP_FALSE,
        V_TPLOTE          TYPE ZPFE_LOTE-TPLOTE,
        V_TP_PLANO_ADM    TYPE ZPFE_LOTE-TP_PLANO_ADMINISTRADORA,
        V_DATA_0141       TYPE ZLEST0141-DATA.

  IF ( V_DATA_POSICAO IS INITIAL ).
    V_DATA_POSICAO = SY-DATUM.
  ENDIF.

  V_TPLOTE = 'S'.
  V_TP_PLANO_ADM = '02'.

  SELECT *
      FROM ZPFE_LOTE
      INTO TABLE @DATA(IT_ZPFE_LOTE)
      WHERE DT_LEITURA              <= @V_DATA_POSICAO
        AND TPLOTE                  =  @V_TPLOTE   " Saldo
        AND TP_PLANO_ADMINISTRADORA =  @V_TP_PLANO_ADM  " Pré-pago
        AND DT_PAG_ZLES0145         = '00000000'.

  SORT IT_ZPFE_LOTE[] BY NM_LOTE NR_LOTE_ADM ASCENDING.
  "DELETE IT_ZPFE_LOTE[] WHERE DT_PAG_ZLES0145 IS NOT INITIAL.

  IF ( IT_ZPFE_LOTE[] IS NOT INITIAL ).

    IT_0141_LOTE_SAVE[] = CORRESPONDING #( IT_ZPFE_LOTE[] ).
    MODIFY ZLEST0141_LOTE FROM TABLE IT_0141_LOTE_SAVE[].

    "buscar histórico de chaves no set
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        CLASS         = '0000'
        SETNR         = 'MAGGI_ZLES0145_HIST'
      TABLES
        SET_VALUES    = IT_SET_CHVID
      EXCEPTIONS
        SET_NOT_FOUND = 1
        OTHERS        = 2.
    IF ( SY-SUBRC = 0 ).

      DATA(RG_CHVID) = VALUE TY_RG_CHVID(
          FOR W_SET IN IT_SET_CHVID[] (
          SIGN   = 'I'
          OPTION = 'EQ'
          LOW    = W_SET-FROM
          HIGH   = W_SET-TO ) ).
      SORT RG_CHVID[] BY LOW ASCENDING.

    ENDIF.

    SELECT  ZLOTE_IT~NM_LOTE,
            ZLOTE_IT~NM_LOTE_ITEM,
            ZLOTE_IT~NR_LOTE_ADM,
            ZLOTE_IT~DOCNUM,
            ZLOTE_IT~CHVID,
            ZLOTE_IT~VL_PAGO_LOTE,
            ZLOTE_IT~BELNR,
            ZLOTE_IT~GJAHR,
            ZLOTE_IT~DT_BELNR
    FROM ZPFE_LOTE_ITEM AS ZLOTE_IT
    INTO TABLE @DATA(IT_ZPFE_LOTE_ITEM)
    FOR ALL ENTRIES IN @IT_ZPFE_LOTE
    WHERE
        ZLOTE_IT~NM_LOTE =  @IT_ZPFE_LOTE-NM_LOTE AND
        ZLOTE_IT~CHVID   IN @RG_CHVID[]. " maggi_zles0145_hist
    SORT IT_ZPFE_LOTE_ITEM[] BY NM_LOTE NM_LOTE_ITEM ASCENDING.

    IF ( IT_ZPFE_LOTE_ITEM IS NOT INITIAL ).

      "-> Enquanto dia útil não for válido,já tiver sido utilizado na ZLES0145, buscar próximo
      WHILE ( V_REG_GRAVADO = ABAP_FALSE ).

        ZCL_MIRO=>GET_PROXIMO_DIA_UTIL(
            EXPORTING
              I_DATA_BASE        = V_DATA_POSICAO
              I_SIGNUM           = '+'    " Nenhuma modificação durante a ausência
              I_CK_DATA_ZLES0145 = 'X'     " Considerar datas informadas ZLES0145
            RECEIVING
              R_DATA             = DATA(V_DIA_UTIL)    " Data
            EXCEPTIONS
              ERRO               = 1
              OTHERS             = 2    ).
        IF ( SY-SUBRC = 0 ).

          "-> Valida se nessa data já existe pagamento na ZLES0145
          DATA(LVA_CTB_ZLEST0145) = ABAP_FALSE.
          LOOP AT IT_ZPFE_LOTE INTO DATA(LWA_PFE_LOTE_TMP).
            SELECT SINGLE *
              FROM ZLEST0141 INTO @DATA(_WL_0141_CTB)
             WHERE BUKRS EQ @LWA_PFE_LOTE_TMP-BUKRS
               AND DATA  EQ @V_DIA_UTIL.

            IF ( SY-SUBRC EQ 0 ) AND ( _WL_0141_CTB-LOTE IS NOT INITIAL OR _WL_0141_CTB-SEM_SLD_PGTO IS NOT INITIAL ).
              LVA_CTB_ZLEST0145 = ABAP_TRUE.
            ENDIF.
          ENDLOOP.

          IF ( LVA_CTB_ZLEST0145 EQ ABAP_FALSE ).

            IT_0141_ITEM_SAVE[] = CORRESPONDING #( IT_ZPFE_LOTE_ITEM[] ).

            WA_ZPFE_LOTE-DT_PAG_ZLES0145 = V_DIA_UTIL.
            MODIFY IT_ZPFE_LOTE[] FROM WA_ZPFE_LOTE TRANSPORTING DT_PAG_ZLES0145 WHERE DT_PAG_ZLES0145 IS INITIAL.

            LOOP AT IT_0141_ITEM_SAVE[] ASSIGNING FIELD-SYMBOL(<W_0141_ITEM_SAVE>).
              <W_0141_ITEM_SAVE>-DT_UTIL = V_DIA_UTIL.
            ENDLOOP.

            MODIFY ZLEST0141_L_ITEM FROM TABLE IT_0141_ITEM_SAVE[].
            MODIFY ZPFE_LOTE FROM TABLE IT_ZPFE_LOTE[].

            COMMIT WORK.

            V_REG_GRAVADO = ABAP_TRUE.

          ELSE. "-> Se data já tiver sido utilizada na ZLES0145, pegar próximo dia.

            ADD 1 TO V_DATA_POSICAO.

          ENDIF.

        ENDIF.

      ENDWHILE.

    ENDIF.

  ENDIF.

  "Atualiza valores de saldo_outros na ZLEST0141:
  SELECT  Z_ITEM~NM_LOTE,
          Z_ITEM~NM_LOTE_ITEM,
          Z_ITEM~NR_LOTE_ADM,
          Z_ITEM~DT_UTIL,                                 "<<-SKM-11.04.23-IR133461
          Z_ITEM~DOCNUM,
          Z_ITEM~CHVID,
          Z_ITEM~VL_PAGO_LOTE,
          Z_ITEM~BELNR,
          Z_ITEM~GJAHR,
          Z_ITEM~DT_BELNR,
          Z_LOTE~BUKRS,
          Z_LOTE~BRANCH
      FROM ZLEST0141_L_ITEM AS Z_ITEM
      LEFT JOIN ZLEST0141_LOTE AS Z_LOTE
        ON Z_LOTE~NM_LOTE = Z_ITEM~NM_LOTE AND
           Z_LOTE~NR_LOTE_ADM = Z_ITEM~NR_LOTE_ADM
      INTO TABLE @DATA(IT0141_LOTE_ITEM)
      WHERE Z_ITEM~DT_UTIL = @V_DATA_POSICAO.
*  SORT IT0141_LOTE_ITEM[] BY BUKRS DT_BELNR ASCENDING.   ">>-SKM-11.04.23-IR133461
  SORT IT0141_LOTE_ITEM[] BY BUKRS DT_UTIL ASCENDING.     "<<-SKM-11.04.23-IR133461

  IF ( IT0141_LOTE_ITEM[] IS NOT INITIAL ).

    LOOP AT IT0141_LOTE_ITEM[] INTO DATA(W_LOTE_ITEM) GROUP BY (
        BUKRS = W_LOTE_ITEM-BUKRS
*        DT_BELNR = W_LOTE_ITEM-DT_BELNR )                ">>-SKM-11.04.23-IR133461
         DT_UTIL = W_LOTE_ITEM-DT_UTIL )                  "<<-SKM-11.04.23-IR133461
        ASCENDING REFERENCE INTO DATA(GROUP_LOTES).

      LOOP AT GROUP GROUP_LOTES ASSIGNING FIELD-SYMBOL(<W_LOTE_ITEM>).
        V_SOMA_OUTROS = V_SOMA_OUTROS + <W_LOTE_ITEM>-VL_PAGO_LOTE.
      ENDLOOP.

      IF ( V_SOMA_OUTROS IS NOT INITIAL ).

        V_DATA_0141 = COND #( WHEN V_DIA_UTIL IS NOT INITIAL THEN V_DIA_UTIL ELSE V_DATA_POSICAO ).

        UPDATE ZLEST0141
           SET SALDO_OUTROS = V_SOMA_OUTROS
         WHERE BUKRS = GROUP_LOTES->BUKRS AND
               DATA  = V_DATA_0141.

        COMMIT WORK.

        IF ( SY-SUBRC = 0 ).
          MSG_RETORNO = 'SUCESSO'.
        ENDIF.

        CLEAR: V_SOMA_OUTROS.

      ENDIF.

    ENDLOOP.

  ENDIF.




ENDFUNCTION.
