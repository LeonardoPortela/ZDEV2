FUNCTION ZFI_PROC_FLX_REAL.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DATA_INI) TYPE  DZFBDT
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_REF_SALDO) TYPE  CHAR01
*"     REFERENCE(I_CALC_SEM_CLASS) TYPE  CHAR01 OPTIONAL
*"  EXCEPTIONS
*"      M_ERROR
*"----------------------------------------------------------------------

  DATA: BEGIN OF TG_0080_RESUMO OCCURS 0,
          BUKRS        TYPE ZFIT0080-BUKRS,
          COD_FLX      TYPE ZFIT0080-COD_FLX,
          AUGDT        TYPE ZFIT0080-AUGDT,
          DMBTR        TYPE ZFIT0080-DMBTR,
          DMBE2        TYPE ZFIT0080-DMBE2,
        END OF TG_0080_RESUMO,

        BEGIN OF TG_0123_RESUMO OCCURS 0,
          BUKRS        TYPE ZFIT0124-BUKRS,
          AUGDT        TYPE ZFIT0124-AUGDT,
        END OF TG_0123_RESUMO.

  DATA: TG_0080     LIKE ZFIT0080 OCCURS 0 WITH HEADER LINE,
        TG_0080_AUX LIKE ZFIT0080 OCCURS 0 WITH HEADER LINE,
        TG_0077     LIKE ZFIT0077 OCCURS 0 WITH HEADER LINE,
        TG_0077_AUX LIKE ZFIT0077 OCCURS 0 WITH HEADER LINE,
        TG_0123_DET LIKE ZFIT0123 OCCURS 0 WITH HEADER LINE,
        TG_0124     LIKE ZFIT0124 OCCURS 0 WITH HEADER LINE,
        TG_0124_AUX LIKE ZFIT0124 OCCURS 0 WITH HEADER LINE,
        TG_0080_SLD LIKE ZFIT0080 OCCURS 0 WITH HEADER LINE.

  DATA: VL_MAX_AUGDT    TYPE ZFIT0124-AUGDT,
        VL_SLD_CONTAS   TYPE ZFIT0080-SLD_CONTAS,
        VL_SALDO_INI_R  TYPE ZFIT0124-SDO_INICIAL_R,
        VL_SALDO_INI_US TYPE ZFIT0124-SDO_INICIAL_US,
        VL_SALDO_DIA_R  TYPE ZFIT0124-DMBTR,
        VL_SALDO_DIA_US TYPE ZFIT0124-DMBE2,
        VL_SALDO_FIM_R  TYPE ZFIT0124-SDO_FINAL_R,
        VL_SALDO_FIM_US TYPE ZFIT0124-SDO_FINAL_US,
        VL_MIN_DT_CLONE TYPE SYDATUM,
        VL_IDX_RESUMO   TYPE I.

  IF I_REF_SALDO IS NOT INITIAL.
    "Limpa Tabela de Resumo Detalhe
    DELETE FROM ZFIT0123 WHERE BUKRS          =  I_BUKRS
                           AND AUGDT          >= I_DATA_INI.

    "Limpa Tabela de Resumo
    DELETE FROM ZFIT0124 WHERE BUKRS          =  I_BUKRS
                           AND AUGDT          >= I_DATA_INI.

  ENDIF.


  "Buscar Registros p/ Atualização do Detalhe do Resumo (ZFIT0080)
  SELECT BUKRS COD_FLX AUGDT SUM( DMBTR ) SUM( DMBE2 )
    FROM ZFIT0080 INTO TABLE TG_0080_RESUMO
   WHERE BUKRS            EQ I_BUKRS
     AND AUGDT            >= I_DATA_INI
     AND SLD_CONTAS       EQ ''
   GROUP BY BUKRS COD_FLX AUGDT.

  IF I_CALC_SEM_CLASS IS INITIAL. "Não calcular documentos sem classificação
    DELETE TG_0080_RESUMO WHERE COD_FLX IS INITIAL.
  ENDIF.

  IF ( TG_0080_RESUMO[] IS NOT INITIAL ).

    SELECT *
      FROM ZFIT0077 INTO TABLE TG_0077
      FOR ALL ENTRIES IN TG_0080_RESUMO
     WHERE COD_FLX  = TG_0080_RESUMO-COD_FLX.

  ENDIF.

  SORT TG_0077 BY COD_FLX.
  DELETE ADJACENT DUPLICATES FROM TG_0077 COMPARING COD_FLX.

  "Grava dados detalhados para compor Tabela de Resumo - ( ZFIT0080 )
  LOOP AT TG_0080_RESUMO.

    CLEAR: TG_0123_DET, TG_0077.

    READ TABLE TG_0077 WITH KEY COD_FLX = TG_0080_RESUMO-COD_FLX.

    TG_0123_DET-BUKRS           = TG_0080_RESUMO-BUKRS.
    TG_0123_DET-COD_FLX         = TG_0080_RESUMO-COD_FLX.
    TG_0123_DET-AUGDT           =	TG_0080_RESUMO-AUGDT.
    TG_0123_DET-DMBTR           = TG_0080_RESUMO-DMBTR.
    TG_0123_DET-DMBE2           = TG_0080_RESUMO-DMBE2.
    TG_0123_DET-DT_ATUAL        = SY-DATUM.
    TG_0123_DET-HR_ATUAL        = SY-UZEIT.

    IF ( TG_0080_RESUMO-DMBTR NE 0 ) AND ( TG_0080_RESUMO-DMBE2 NE 0 ).
      TG_0123_DET-KURSF         = ( ABS( TG_0080_RESUMO-DMBTR ) / ABS( TG_0080_RESUMO-DMBE2 ) ).
    ENDIF.

    MODIFY ZFIT0123 FROM TG_0123_DET.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gerar o Resumo' TYPE 'S' RAISING M_ERROR.
      RETURN.
    ENDIF.

  ENDLOOP.

*---------------------------------------------------------------------------------------*
*  Verifica se tem movimento na data atual para a empresa. Caso não tenha,
*  gerar apenas uma linha nos detalhes do resumo com valor zerado,
*  para atribuição do Saldo inicial do Dia.
*---------------------------------------------------------------------------------------*
*  SELECT SINGLE *
*    INTO TG_0123_DET
*    FROM ZFIT0111
*   WHERE BUKRS           = I_BUKRS
*     AND DT_VCTO         = SY-DATUM
*     AND DT_BASE_VERSAO  = I_DT_BASE_VERSAO
*     AND VERSAO          = I_VERSAO.
*
*  IF SY-SUBRC NE 0.
*
*    CLEAR: TG_0111_DET, TG_0109.
*
*    "Busca Cod. Fluxo para Saldo Inicial.
*    SELECT SINGLE *
*      INTO TG_0109
*      FROM ZFIT0109
*     WHERE TP_PREV = 'S'.
*
*    IF SY-SUBRC EQ 0.
*
*      TG_0111_DET-BUKRS           = I_BUKRS.
*      TG_0111_DET-CODIGO          = TG_0109-CODIGO.
*      TG_0111_DET-CLAS_FLX        =  TG_0109-CLAS_FLX.
*      TG_0111_DET-TP_PREV         =  TG_0109-TP_PREV.
*      TG_0111_DET-SEQ             = TG_0109-SEQ.
*      TG_0111_DET-DT_VCTO         =  SY-DATUM.
*      TG_0111_DET-DMBTR           = 0.
*      TG_0111_DET-DMBE2           = 0.
*      TG_0111_DET-DT_ATUAL        = SY-DATUM.
*      TG_0111_DET-HR_ATUAL        = SY-UZEIT.
*      TG_0111_DET-KURSF           = 0.
*      TG_0111_DET-DT_BASE_VERSAO  = I_DT_BASE_VERSAO.
*      TG_0111_DET-HORA_VERSAO     = I_HORA_VERSAO.
*      TG_0111_DET-VERSAO          = I_VERSAO.
*
*      MODIFY ZFIT0111 FROM TG_0111_DET.
*
*      IF SY-SUBRC NE 0.
*        ROLLBACK WORK.
*        MESSAGE 'Houve um erro ao gerar o Resumo' TYPE 'S' RAISING M_ERROR.
*        RETURN.
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.

  REFRESH: TG_0123_RESUMO, TG_0123_DET.

  "Buscar Registros para gerar Resumo
  SELECT BUKRS AUGDT
    FROM ZFIT0123 INTO TABLE TG_0123_RESUMO
   WHERE BUKRS  = I_BUKRS
     AND AUGDT >= I_DATA_INI
   GROUP BY BUKRS AUGDT.

  SELECT *
    FROM ZFIT0123 INTO TABLE TG_0123_DET
   WHERE BUKRS EQ I_BUKRS
     AND AUGDT >= I_DATA_INI.

  "Ordenar por data de Compensacao
  SORT: TG_0123_RESUMO BY AUGDT,
        TG_0123_DET    BY AUGDT.

  VL_IDX_RESUMO = 1.

  LOOP AT TG_0123_RESUMO.

    CLEAR: TG_0124, TG_0124[], TG_0124_AUX, TG_0124_AUX[], VL_MAX_AUGDT,
           VL_SALDO_INI_R, VL_SALDO_INI_US,
           VL_SALDO_DIA_R, VL_SALDO_DIA_US,
           VL_SALDO_FIM_R, VL_SALDO_FIM_US.

    CLEAR: VL_MAX_AUGDT, TG_0124_AUX, TG_0080_SLD, VL_SLD_CONTAS.

    VL_SALDO_INI_R  = 0.
    VL_SALDO_INI_US = 0.

    "Verifica se tem Lançamento de Saldo de Contas efetuado anterior a data de compensação.
    SELECT MAX( AUGDT )
      INTO VL_MAX_AUGDT
      FROM ZFIT0080
     WHERE BUKRS   = TG_0123_RESUMO-BUKRS
       AND AUGDT   < TG_0123_RESUMO-AUGDT.
    IF ( SY-SUBRC = 0 ) AND ( VL_MAX_AUGDT IS NOT INITIAL ).

      SELECT SINGLE *
        FROM ZFIT0080 INTO TG_0080_SLD
       WHERE BUKRS      = TG_0123_RESUMO-BUKRS
         AND AUGDT      = VL_MAX_AUGDT
         AND SLD_CONTAS = 'X'.

      IF SY-SUBRC = 0.
        VL_SLD_CONTAS   = 'X'.
        VL_SALDO_INI_R  = TG_0080_SLD-DMBTR.
        VL_SALDO_INI_US = TG_0080_SLD-DMBE2.
      ENDIF.

    ENDIF.
    CLEAR: VL_MAX_AUGDT.
    "Fim

    IF VL_SLD_CONTAS IS INITIAL. "Se não encontrou uma lançamento de Saldo de Contas anterior ao movimento.

      "Verifica se tem saldo anterior a data de Compensacao.
      SELECT MAX( AUGDT )
        INTO VL_MAX_AUGDT
        FROM ZFIT0124
       WHERE BUKRS   = TG_0123_RESUMO-BUKRS
         AND AUGDT   < TG_0123_RESUMO-AUGDT.

      IF ( SY-SUBRC = 0 ) AND ( VL_MAX_AUGDT IS NOT INITIAL ).

        SELECT SINGLE *
          FROM ZFIT0124 INTO TG_0124_AUX
         WHERE BUKRS  = TG_0123_RESUMO-BUKRS
           AND AUGDT  = VL_MAX_AUGDT.

        IF SY-SUBRC = 0.
          VL_SALDO_INI_R  = TG_0124_AUX-SDO_FINAL_R.
          VL_SALDO_INI_US = TG_0124_AUX-SDO_FINAL_US.
        ENDIF.

      ENDIF.

    ENDIF.

    TG_0124-SDO_INICIAL_R   = VL_SALDO_INI_R.
    TG_0124-SDO_INICIAL_US  = VL_SALDO_INI_US.
    TG_0124-BUKRS           = TG_0123_RESUMO-BUKRS.
    TG_0124-AUGDT           =	TG_0123_RESUMO-AUGDT.

    "Buscar Saldo do dia da Compensacao.
    LOOP AT TG_0123_DET WHERE BUKRS = TG_0123_RESUMO-BUKRS
                          AND AUGDT = TG_0123_RESUMO-AUGDT.


      VL_SALDO_DIA_R  = VL_SALDO_DIA_R   + ( TG_0123_DET-DMBTR ).
      VL_SALDO_DIA_US = VL_SALDO_DIA_US  + ( TG_0123_DET-DMBE2 ).

    ENDLOOP.

    TG_0124-DMBTR           = VL_SALDO_DIA_R.
    TG_0124-DMBE2           = VL_SALDO_DIA_US.
    TG_0124-DT_ATUAL        = SY-DATUM.
    TG_0124-HR_ATUAL        = SY-UZEIT.
    TG_0124-SDO_FINAL_R     = ( VL_SALDO_INI_R  + VL_SALDO_DIA_R ) .
    TG_0124-SDO_FINAL_US    = ( VL_SALDO_INI_US + VL_SALDO_DIA_US ).

    IF ( VL_SALDO_DIA_R NE 0 ) AND ( VL_SALDO_DIA_US NE 0 ).
      TG_0124-KURSF         = ( ABS( VL_SALDO_DIA_R ) / ABS( VL_SALDO_DIA_US ) ).
    ENDIF.

    MODIFY ZFIT0124 FROM TG_0124.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gerar o Resumo' TYPE 'S' RAISING M_ERROR.
      RETURN.
    ENDIF.

  ENDLOOP.





ENDFUNCTION.
